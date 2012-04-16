  SUBROUTINE wrf_histout ( fid , grid , config_flags, switch , &
                           dryrun, ierr )
    USE module_io
    USE module_wrf_error
    USE module_io_wrf
    USE module_domain
    USE module_state_description
    USE module_configure
    USE module_scalar_tables
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
    INTEGER, INTENT(IN) :: fid, switch
    INTEGER, INTENT(INOUT) :: ierr
    LOGICAL, INTENT(IN) :: dryrun

    
    INTEGER ids , ide , jds , jde , kds , kde , &
            ims , ime , jms , jme , kms , kme , &
            ips , ipe , jps , jpe , kps , kpe

    INTEGER       itrace,idim1,idim2,idim3,idim4,idim5,idim6,idim7
    INTEGER , DIMENSION(3) :: domain_start , domain_end
    INTEGER , DIMENSION(3) :: memory_start , memory_end
    INTEGER , DIMENSION(3) :: patch_start , patch_end
    INTEGER i,j
    INTEGER julyr, julday, idt, iswater , map_proj
    REAL    gmt, cen_lat, cen_lon, bdyfrq , truelat1 , truelat2, &
            mp_physics, ra_lw_physics, ra_sw_physics, sf_sfclay_physics, &
            sf_surface_physics, bl_pbl_physics, cu_physics
    REAL    khdif, kvdif
    INTEGER rc

    CHARACTER*256 message
    CHARACTER*80  char_junk
    INTEGER    ibuf(1)
    REAL       rbuf(1)
    CHARACTER*40            :: next_datestr

    CALL get_ijk_from_grid (  grid ,                        &
                              ids, ide, jds, jde, kds, kde,    &
                              ims, ime, jms, jme, kms, kme,    &
                              ips, ipe, jps, jpe, kps, kpe    )


    
    








IF ( in_use_for_config(grid%id,'x_2') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'TOYVAR'               , &  
                       grid%x_2               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XZY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'bottom_top'               , &  
                       'south_north'               , &  
                       '-'               , &  
                       '-'               , &  
'inc/wrf_histout.inc ext_write_field TOYVAR memorder XZY' , & 
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'lu_index') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'LU_INDEX'               , &  
                       grid%lu_index               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'LAND USE CATEGORY'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field LU_INDEX memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'hbm2') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'HBM2'               , &  
                       grid%hbm2               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Height boundary mask; =0 outer 2 rows on H points'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field HBM2 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'sm') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SM'               , &  
                       grid%sm               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Sea mask; =1 for sea, =0 for land'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field SM memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'sice') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SICE'               , &  
                       grid%sice               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Sea ice mask; =1 for sea ice, =0 for no sea ice'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field SICE memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'pd') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'PD'               , &  
                       grid%pd               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Mass at I,J in the sigma domain'               , &  
                       'Pa'               , &  
'inc/wrf_histout.inc ext_write_field PD memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'fis') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'FIS'               , &  
                       grid%fis               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Surface geopotential'               , &  
                       'm2 s-2'               , &  
'inc/wrf_histout.inc ext_write_field FIS memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'res') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'RES'               , &  
                       grid%res               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Reciprocal of surface sigma'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field RES memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'t') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'T'               , &  
                       grid%t               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XYZ'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       'bottom_top'               , &  
                       'Sensible temperature'               , &  
                       'K'               , &  
'inc/wrf_histout.inc ext_write_field T memorder XYZ' , & 
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'q') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'Q'               , &  
                       grid%q               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XYZ'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       'bottom_top'               , &  
                       'Specific humidity'               , &  
                       'kg kg-1'               , &  
'inc/wrf_histout.inc ext_write_field Q memorder XYZ' , & 
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'u') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'U'               , &  
                       grid%u               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XYZ'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       'bottom_top'               , &  
                       'U component of wind'               , &  
                       'm s-1'               , &  
'inc/wrf_histout.inc ext_write_field U memorder XYZ' , & 
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'v') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'V'               , &  
                       grid%v               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XYZ'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       'bottom_top'               , &  
                       'V component of wind'               , &  
                       'm s-1'               , &  
'inc/wrf_histout.inc ext_write_field V memorder XYZ' , & 
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'dx_nmm') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'DX_NMM'               , &  
                       grid%dx_nmm               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'East-west distance H-to-V points'               , &  
                       'm'               , &  
'inc/wrf_histout.inc ext_write_field DX_NMM memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'deta1') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'DETA1'               , &  
                       grid%deta1               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'Z'               , &  
                       ''               , &  
                       'bottom_top'               , &  
                       ''               , &  
                       ''               , &  
                       'Delta sigma in pressure domain'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field DETA1 memorder Z' , & 
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'aeta1') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'AETA1'               , &  
                       grid%aeta1               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'Z'               , &  
                       ''               , &  
                       'bottom_top'               , &  
                       ''               , &  
                       ''               , &  
                       'Midlayer sigma value in pressure domain'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field AETA1 memorder Z' , & 
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'eta1') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ETA1'               , &  
                       grid%eta1               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'Z'               , &  
                       'Z'               , &  
                       'bottom_top_stag'               , &  
                       ''               , &  
                       ''               , &  
                       'Interface sigma value in pressure domain'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field ETA1 memorder Z' , & 
kds , kde , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( kde, kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'deta2') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'DETA2'               , &  
                       grid%deta2               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'Z'               , &  
                       ''               , &  
                       'bottom_top'               , &  
                       ''               , &  
                       ''               , &  
                       'Delta sigma in sigma domain'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field DETA2 memorder Z' , & 
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'aeta2') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'AETA2'               , &  
                       grid%aeta2               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'Z'               , &  
                       ''               , &  
                       'bottom_top'               , &  
                       ''               , &  
                       ''               , &  
                       'Midlayer sigma value in sigma domain'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field AETA2 memorder Z' , & 
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'eta2') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ETA2'               , &  
                       grid%eta2               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'Z'               , &  
                       'Z'               , &  
                       'bottom_top_stag'               , &  
                       ''               , &  
                       ''               , &  
                       'Interface sigma value in sigma domain'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field ETA2 memorder Z' , & 
kds , kde , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( kde, kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'dy_nmm') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'DY_NMM'               , &  
                       grid%dy_nmm               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       '0'               , &  
                       ''               , &  
                       ''               , &  
                       ''               , &  
                       ''               , &  
                       'North-south distance H-to-V points'               , &  
                       'm'               , &  
'inc/wrf_histout.inc ext_write_field DY_NMM memorder 0' , & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'dlmd') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'DLMD'               , &  
                       grid%dlmd               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       '0'               , &  
                       ''               , &  
                       ''               , &  
                       ''               , &  
                       ''               , &  
                       'East-west angular distance H-to-V points'               , &  
                       'degrees'               , &  
'inc/wrf_histout.inc ext_write_field DLMD memorder 0' , & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'dphd') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'DPHD'               , &  
                       grid%dphd               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       '0'               , &  
                       ''               , &  
                       ''               , &  
                       ''               , &  
                       ''               , &  
                       'North-south angular distance H-to-V points'               , &  
                       'degrees'               , &  
'inc/wrf_histout.inc ext_write_field DPHD memorder 0' , & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'pdtop') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'PDTOP'               , &  
                       grid%pdtop               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       '0'               , &  
                       ''               , &  
                       ''               , &  
                       ''               , &  
                       ''               , &  
                       'Mass at I,J in pressure domain'               , &  
                       'Pa'               , &  
'inc/wrf_histout.inc ext_write_field PDTOP memorder 0' , & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'pt') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'PT'               , &  
                       grid%pt               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       '0'               , &  
                       ''               , &  
                       ''               , &  
                       ''               , &  
                       ''               , &  
                       'Pressure at top of domain'               , &  
                       'Pa'               , &  
'inc/wrf_histout.inc ext_write_field PT memorder 0' , & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'pblh') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'PBLH'               , &  
                       grid%pblh               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'PBL Height'               , &  
                       'm'               , &  
'inc/wrf_histout.inc ext_write_field PBLH memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'mixht') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'MIXHT'               , &  
                       grid%mixht               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'MXL HEIGHT'               , &  
                       'm'               , &  
'inc/wrf_histout.inc ext_write_field MIXHT memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'ustar') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'USTAR'               , &  
                       grid%ustar               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Friction velocity'               , &  
                       'm s-1'               , &  
'inc/wrf_histout.inc ext_write_field USTAR memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'z0') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'Z0'               , &  
                       grid%z0               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Roughness height'               , &  
                       'm'               , &  
'inc/wrf_histout.inc ext_write_field Z0 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'ths') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'THS'               , &  
                       grid%ths               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Surface potential temperature'               , &  
                       'K'               , &  
'inc/wrf_histout.inc ext_write_field THS memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'qsh') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'QS'               , &  
                       grid%qsh               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Surface specific humidity'               , &  
                       'kg kg-1'               , &  
'inc/wrf_histout.inc ext_write_field QS memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'twbs') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'TWBS'               , &  
                       grid%twbs               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Instantaneous sensible heat flux'               , &  
                       'W m-2'               , &  
'inc/wrf_histout.inc ext_write_field TWBS memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'qwbs') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'QWBS'               , &  
                       grid%qwbs               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Instantaneous latent heat flux'               , &  
                       'W m-2'               , &  
'inc/wrf_histout.inc ext_write_field QWBS memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'prec') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'PREC'               , &  
                       grid%prec               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Precipitation in physics timestep'               , &  
                       'm'               , &  
'inc/wrf_histout.inc ext_write_field PREC memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'aprec') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'APREC'               , &  
                       grid%aprec               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       '-'               , &  
                       '-'               , &  
'inc/wrf_histout.inc ext_write_field APREC memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'acprec') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ACPREC'               , &  
                       grid%acprec               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Accumulated total precipitation'               , &  
                       'm'               , &  
'inc/wrf_histout.inc ext_write_field ACPREC memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'cuprec') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'CUPREC'               , &  
                       grid%cuprec               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Accumulated convective precipitation'               , &  
                       'm'               , &  
'inc/wrf_histout.inc ext_write_field CUPREC memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'sno') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SNO'               , &  
                       grid%sno               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Liquid water snow amount'               , &  
                       'm'               , &  
'inc/wrf_histout.inc ext_write_field SNO memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'si') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SI'               , &  
                       grid%si               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Snow depth'               , &  
                       'm'               , &  
'inc/wrf_histout.inc ext_write_field SI memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'cldefi') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'CLDEFI'               , &  
                       grid%cldefi               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Convective cloud efficiency'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field CLDEFI memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'th10') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'TH10'               , &  
                       grid%th10               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       '10-m potential temperature from MYJ'               , &  
                       'K'               , &  
'inc/wrf_histout.inc ext_write_field TH10 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'q10') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'Q10'               , &  
                       grid%q10               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       '10-m specific humidity from MYJ'               , &  
                       'kg kg-1'               , &  
'inc/wrf_histout.inc ext_write_field Q10 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'pshltr') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'PSHLTR'               , &  
                       grid%pshltr               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       '2-m pressure from MYJ'               , &  
                       'Pa'               , &  
'inc/wrf_histout.inc ext_write_field PSHLTR memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'tshltr') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'TSHLTR'               , &  
                       grid%tshltr               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       '2-m potential temperature from MYJ'               , &  
                       'K'               , &  
'inc/wrf_histout.inc ext_write_field TSHLTR memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'qshltr') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'QSHLTR'               , &  
                       grid%qshltr               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       '2-m specific humidity from MYJ'               , &  
                       'kg kg-1'               , &  
'inc/wrf_histout.inc ext_write_field QSHLTR memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'q2') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'Q2'               , &  
                       grid%q2               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XYZ'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       'bottom_top'               , &  
                       '2 * Turbulence kinetic energy'               , &  
                       'm2 s-2'               , &  
'inc/wrf_histout.inc ext_write_field Q2 memorder XYZ' , & 
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'akhs_out') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'AKHS_OUT'               , &  
                       grid%akhs_out               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Output sfc exch coeff for heat'               , &  
                       'm2 s-1'               , &  
'inc/wrf_histout.inc ext_write_field AKHS_OUT memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'akms_out') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'AKMS_OUT'               , &  
                       grid%akms_out               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Output sfc exch coeff for momentum'               , &  
                       'm2 s-1'               , &  
'inc/wrf_histout.inc ext_write_field AKMS_OUT memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'albase') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ALBASE'               , &  
                       grid%albase               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Base albedo'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field ALBASE memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'albedo') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ALBEDO'               , &  
                       grid%albedo               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Dynamic albedo'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field ALBEDO memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'cnvbot') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'CNVBOT'               , &  
                       grid%cnvbot               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Lowest convec cloud bottom lyr between outputs'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field CNVBOT memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'cnvtop') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'CNVTOP'               , &  
                       grid%cnvtop               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Highest convec cloud top lyr between outputs'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field CNVTOP memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'czen') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'CZEN'               , &  
                       grid%czen               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Cosine of solar zenith angle'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field CZEN memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'czmean') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'CZMEAN'               , &  
                       grid%czmean               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Mean CZEN between SW radiation calls'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field CZMEAN memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'epsr') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'EPSR'               , &  
                       grid%epsr               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Radiative emissivity'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field EPSR memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'glat') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'GLAT'               , &  
                       grid%glat               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Geographic latitude, radians'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field GLAT memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'glon') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'GLON'               , &  
                       grid%glon               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Geographic longitude, radians'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field GLON memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'nmm_tsk') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'TSK'               , &  
                       grid%nmm_tsk               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Skin temperature'               , &  
                       'K'               , &  
'inc/wrf_histout.inc ext_write_field TSK memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'mxsnal') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'MXSNAL'               , &  
                       grid%mxsnal               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Maximum deep snow albedo'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field MXSNAL memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'radot') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'RADOT'               , &  
                       grid%radot               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Radiative emission from surface'               , &  
                       'W m-2'               , &  
'inc/wrf_histout.inc ext_write_field RADOT memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'sigt4') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SIGT4'               , &  
                       grid%sigt4               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Stefan-Boltzmann * T**4'               , &  
                       'W m-2'               , &  
'inc/wrf_histout.inc ext_write_field SIGT4 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'tg') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'TGROUND'               , &  
                       grid%tg               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Deep ground soil temperature'               , &  
                       'K'               , &  
'inc/wrf_histout.inc ext_write_field TGROUND memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'cwm') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'CWM'               , &  
                       grid%cwm               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XYZ'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       'bottom_top'               , &  
                       'Total condensate'               , &  
                       'kg kg-1'               , &  
'inc/wrf_histout.inc ext_write_field CWM memorder XYZ' , & 
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'f_ice') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'F_ICE'               , &  
                       grid%f_ice               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XZY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'bottom_top'               , &  
                       'south_north'               , &  
                       'Frozen fraction of CWM'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field F_ICE memorder XZY' , & 
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'f_rain') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'F_RAIN'               , &  
                       grid%f_rain               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XZY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'bottom_top'               , &  
                       'south_north'               , &  
                       'Rain fraction of liquid part of CWM'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field F_RAIN memorder XZY' , & 
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'f_rimef') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'F_RIMEF'               , &  
                       grid%f_rimef               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XZY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'bottom_top'               , &  
                       'south_north'               , &  
                       'Rime factor'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field F_RIMEF memorder XZY' , & 
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'sr') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SR'               , &  
                       grid%sr               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Timestep mass ratio of snow:precip'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field SR memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'cfrach') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'CFRACH'               , &  
                       grid%cfrach               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'High cloud fraction'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field CFRACH memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'cfracl') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'CFRACL'               , &  
                       grid%cfracl               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Low cloud fraction'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field CFRACL memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'cfracm') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'CFRACM'               , &  
                       grid%cfracm               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Middle cloud fraction'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field CFRACM memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'islope') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ISLOPE'               , &  
                       grid%islope               , &  
                       WRF_integer             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       '-'               , &  
                       '-'               , &  
'inc/wrf_histout.inc ext_write_field ISLOPE memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'dzsoil') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'DZSOIL'               , &  
                       grid%dzsoil               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'Z'               , &  
                       ''               , &  
                       'bottom_top'               , &  
                       ''               , &  
                       ''               , &  
                       'Thickness of soil layers'               , &  
                       'm'               , &  
'inc/wrf_histout.inc ext_write_field DZSOIL memorder Z' , & 
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'sldpth') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SLDPTH'               , &  
                       grid%sldpth               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'Z'               , &  
                       ''               , &  
                       'bottom_top'               , &  
                       ''               , &  
                       ''               , &  
                       'Depths of centers of soil layers'               , &  
                       'm'               , &  
'inc/wrf_histout.inc ext_write_field SLDPTH memorder Z' , & 
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'cmc') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'CMC'               , &  
                       grid%cmc               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Canopy moisture'               , &  
                       'm'               , &  
'inc/wrf_histout.inc ext_write_field CMC memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'grnflx') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'GRNFLX'               , &  
                       grid%grnflx               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Deep soil heat flux'               , &  
                       'W m-2'               , &  
'inc/wrf_histout.inc ext_write_field GRNFLX memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'pctsno') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'PCTSNO'               , &  
                       grid%pctsno               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       '-'               , &  
                       '-'               , &  
'inc/wrf_histout.inc ext_write_field PCTSNO memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'soiltb') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SOILTB'               , &  
                       grid%soiltb               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Deep ground soil temperature'               , &  
                       'K'               , &  
'inc/wrf_histout.inc ext_write_field SOILTB memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'vegfrc') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'VEGFRC'               , &  
                       grid%vegfrc               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Vegetation fraction'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field VEGFRC memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'sh2o') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SH2O'               , &  
                       grid%sh2o               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XZY'               , &  
                       'Z'               , &  
                       'west_east'               , &  
                       'soil_layers_stag'               , &  
                       'south_north'               , &  
                       'Unfrozen soil moisture volume fraction'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field SH2O memorder XZY' , & 
ids , (ide-1) , 1 , config_flags%num_soil_layers , jds , (jde-1) ,  & 
ims , ime , 1 , config_flags%num_soil_layers , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , 1 , config_flags%num_soil_layers , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'smc') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SMC'               , &  
                       grid%smc               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XZY'               , &  
                       'Z'               , &  
                       'west_east'               , &  
                       'soil_layers_stag'               , &  
                       'south_north'               , &  
                       'Soil moisture volume fraction'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field SMC memorder XZY' , & 
ids , (ide-1) , 1 , config_flags%num_soil_layers , jds , (jde-1) ,  & 
ims , ime , 1 , config_flags%num_soil_layers , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , 1 , config_flags%num_soil_layers , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'stc') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'STC'               , &  
                       grid%stc               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XZY'               , &  
                       'Z'               , &  
                       'west_east'               , &  
                       'soil_layers_stag'               , &  
                       'south_north'               , &  
                       'Soil temperature'               , &  
                       'K'               , &  
'inc/wrf_histout.inc ext_write_field STC memorder XZY' , & 
ids , (ide-1) , 1 , config_flags%num_soil_layers , jds , (jde-1) ,  & 
ims , ime , 1 , config_flags%num_soil_layers , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , 1 , config_flags%num_soil_layers , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'pint') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'PINT'               , &  
                       grid%pint               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XYZ'               , &  
                       'Z'               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       'bottom_top_stag'               , &  
                       'Model layer interface pressure'               , &  
                       'Pa'               , &  
'inc/wrf_histout.inc ext_write_field PINT memorder XYZ' , & 
ids , (ide-1) , jds , (jde-1) , kds , kde ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( kde, kpe ) ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'w') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'W'               , &  
                       grid%w               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XYZ'               , &  
                       'Z'               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       'bottom_top_stag'               , &  
                       'Vertical velocity'               , &  
                       'm s-1'               , &  
'inc/wrf_histout.inc ext_write_field W memorder XYZ' , & 
ids , (ide-1) , jds , (jde-1) , kds , kde ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( kde, kpe ) ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'acfrcv') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ACFRCV'               , &  
                       grid%acfrcv               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Accum convective cloud fraction'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field ACFRCV memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'acfrst') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ACFRST'               , &  
                       grid%acfrst               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Accum stratiform cloud fraction'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field ACFRST memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'ssroff') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SSROFF'               , &  
                       grid%ssroff               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Surface runoff'               , &  
                       'mm'               , &  
'inc/wrf_histout.inc ext_write_field SSROFF memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'bgroff') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'BGROFF'               , &  
                       grid%bgroff               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Subsurface runoff'               , &  
                       'mm'               , &  
'inc/wrf_histout.inc ext_write_field BGROFF memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'rlwin') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'RLWIN'               , &  
                       grid%rlwin               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Downward longwave at surface'               , &  
                       'W m-2'               , &  
'inc/wrf_histout.inc ext_write_field RLWIN memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'rlwtoa') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'RLWTOA'               , &  
                       grid%rlwtoa               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Outgoing LW flux at top of atmos'               , &  
                       'W m-2'               , &  
'inc/wrf_histout.inc ext_write_field RLWTOA memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'alwin') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ALWIN'               , &  
                       grid%alwin               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Accum LW down at surface'               , &  
                       'W m-2'               , &  
'inc/wrf_histout.inc ext_write_field ALWIN memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'alwout') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ALWOUT'               , &  
                       grid%alwout               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Accum RADOT (see above)'               , &  
                       'W m-2'               , &  
'inc/wrf_histout.inc ext_write_field ALWOUT memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'alwtoa') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ALWTOA'               , &  
                       grid%alwtoa               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Accum RLWTOA'               , &  
                       'W m-2'               , &  
'inc/wrf_histout.inc ext_write_field ALWTOA memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'rswin') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'RSWIN'               , &  
                       grid%rswin               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Downward shortwave at surface'               , &  
                       'W m-2'               , &  
'inc/wrf_histout.inc ext_write_field RSWIN memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'rswinc') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'RSWINC'               , &  
                       grid%rswinc               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Clear-sky equivalent of RSWIN'               , &  
                       'W m-2'               , &  
'inc/wrf_histout.inc ext_write_field RSWINC memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'rswout') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'RSWOUT'               , &  
                       grid%rswout               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Upward shortwave at surface'               , &  
                       'W m-2'               , &  
'inc/wrf_histout.inc ext_write_field RSWOUT memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'aswin') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ASWIN'               , &  
                       grid%aswin               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Accum SW down at surface'               , &  
                       'W m-2'               , &  
'inc/wrf_histout.inc ext_write_field ASWIN memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'aswout') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ASWOUT'               , &  
                       grid%aswout               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Accum RSWOUT'               , &  
                       'W m-2'               , &  
'inc/wrf_histout.inc ext_write_field ASWOUT memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'aswtoa') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ASWTOA'               , &  
                       grid%aswtoa               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Accum RSWTOA'               , &  
                       'W m-2'               , &  
'inc/wrf_histout.inc ext_write_field ASWTOA memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'sfcshx') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SFCSHX'               , &  
                       grid%sfcshx               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Accum sfc sensible heat flux'               , &  
                       'W m-2'               , &  
'inc/wrf_histout.inc ext_write_field SFCSHX memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'sfclhx') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SFCLHX'               , &  
                       grid%sfclhx               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Accum sfc latent heat flux'               , &  
                       'W m-2'               , &  
'inc/wrf_histout.inc ext_write_field SFCLHX memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'subshx') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SUBSHX'               , &  
                       grid%subshx               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Accum deep soil heat flux'               , &  
                       'W m-2'               , &  
'inc/wrf_histout.inc ext_write_field SUBSHX memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'snopcx') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SNOPCX'               , &  
                       grid%snopcx               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Snow phase change heat flux'               , &  
                       'W m-2'               , &  
'inc/wrf_histout.inc ext_write_field SNOPCX memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'sfcuvx') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SFCUVX'               , &  
                       grid%sfcuvx               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       '-'               , &  
                       '-'               , &  
'inc/wrf_histout.inc ext_write_field SFCUVX memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'potevp') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'POTEVP'               , &  
                       grid%potevp               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Accum potential evaporation'               , &  
                       'm'               , &  
'inc/wrf_histout.inc ext_write_field POTEVP memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'potflx') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'POTFLX'               , &  
                       grid%potflx               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Energy equivalent of POTEVP'               , &  
                       'W m-2'               , &  
'inc/wrf_histout.inc ext_write_field POTFLX memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'tlmin') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'TLMIN'               , &  
                       grid%tlmin               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       '-'               , &  
                       '-'               , &  
'inc/wrf_histout.inc ext_write_field TLMIN memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'tlmax') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'TLMAX'               , &  
                       grid%tlmax               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       '-'               , &  
                       '-'               , &  
'inc/wrf_histout.inc ext_write_field TLMAX memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'t02_min') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'T02_MIN'               , &  
                       grid%t02_min               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Hourly Min Shelter Temperature'               , &  
                       'K'               , &  
'inc/wrf_histout.inc ext_write_field T02_MIN memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'t02_max') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'T02_MAX'               , &  
                       grid%t02_max               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Hourly Max Shelter Temperature'               , &  
                       'K'               , &  
'inc/wrf_histout.inc ext_write_field T02_MAX memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'rh02_min') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'RH02_MIN'               , &  
                       grid%rh02_min               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Hourly Min Relative Humidity'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field RH02_MIN memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'rh02_max') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'RH02_MAX'               , &  
                       grid%rh02_max               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Hourly Max Relative Humidity'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field RH02_MAX memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'rlwtt') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'RLWTT'               , &  
                       grid%rlwtt               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XYZ'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       'bottom_top'               , &  
                       'Longwave temperature tendency'               , &  
                       'K s-1'               , &  
'inc/wrf_histout.inc ext_write_field RLWTT memorder XYZ' , & 
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'rswtt') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'RSWTT'               , &  
                       grid%rswtt               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XYZ'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       'bottom_top'               , &  
                       'Shortwave temperature tendency'               , &  
                       'K s-1'               , &  
'inc/wrf_histout.inc ext_write_field RSWTT memorder XYZ' , & 
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'train') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'TRAIN'               , &  
                       grid%train               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XYZ'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       'bottom_top'               , &  
                       'Accum stratiform temp tendency'               , &  
                       'K s-1'               , &  
'inc/wrf_histout.inc ext_write_field TRAIN memorder XYZ' , & 
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'ncfrcv') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'NCFRCV'               , &  
                       grid%ncfrcv               , &  
                       WRF_integer             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       '  times convec cloud >0 between rad calls'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field NCFRCV memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'ncfrst') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'NCFRST'               , &  
                       grid%ncfrst               , &  
                       WRF_integer             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       '  times stratiform cloud >0 between rad calls'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field NCFRST memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'nphs0') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'NPHS0'               , &  
                       grid%nphs0               , &  
                       WRF_integer             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       '0'               , &  
                       ''               , &  
                       ''               , &  
                       ''               , &  
                       ''               , &  
                       '-'               , &  
                       '-'               , &  
'inc/wrf_histout.inc ext_write_field NPHS0 memorder 0' , & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'nprec') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'NPREC'               , &  
                       grid%nprec               , &  
                       WRF_integer             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       '0'               , &  
                       ''               , &  
                       ''               , &  
                       ''               , &  
                       ''               , &  
                       '  timesteps between resetting precip bucket'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field NPREC memorder 0' , & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'nclod') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'NCLOD'               , &  
                       grid%nclod               , &  
                       WRF_integer             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       '0'               , &  
                       ''               , &  
                       ''               , &  
                       ''               , &  
                       ''               , &  
                       '  timesteps between resetting cloud frac accum'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field NCLOD memorder 0' , & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'nheat') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'NHEAT'               , &  
                       grid%nheat               , &  
                       WRF_integer             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       '0'               , &  
                       ''               , &  
                       ''               , &  
                       ''               , &  
                       ''               , &  
                       '  timesteps between resetting latent heat accum'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field NHEAT memorder 0' , & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'nrdlw') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'NRDLW'               , &  
                       grid%nrdlw               , &  
                       WRF_integer             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       '0'               , &  
                       ''               , &  
                       ''               , &  
                       ''               , &  
                       ''               , &  
                       '  timesteps between resetting longwave accums'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field NRDLW memorder 0' , & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'nrdsw') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'NRDSW'               , &  
                       grid%nrdsw               , &  
                       WRF_integer             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       '0'               , &  
                       ''               , &  
                       ''               , &  
                       ''               , &  
                       ''               , &  
                       '  timesteps between resetting shortwave accums'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field NRDSW memorder 0' , & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'nsrfc') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'NSRFC'               , &  
                       grid%nsrfc               , &  
                       WRF_integer             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       '0'               , &  
                       ''               , &  
                       ''               , &  
                       ''               , &  
                       ''               , &  
                       '  timesteps between resetting sfcflux accums'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field NSRFC memorder 0' , & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'avrain') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'AVRAIN'               , &  
                       grid%avrain               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       '0'               , &  
                       ''               , &  
                       ''               , &  
                       ''               , &  
                       ''               , &  
                       '  of times gridscale precip called in NHEAT steps'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field AVRAIN memorder 0' , & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'avcnvc') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'AVCNVC'               , &  
                       grid%avcnvc               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       '0'               , &  
                       ''               , &  
                       ''               , &  
                       ''               , &  
                       ''               , &  
                       '  of times convective precip called in NHEAT steps'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field AVCNVC memorder 0' , & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'acutim') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ACUTIM'               , &  
                       grid%acutim               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       '0'               , &  
                       ''               , &  
                       ''               , &  
                       ''               , &  
                       ''               , &  
                       '-'               , &  
                       '-'               , &  
'inc/wrf_histout.inc ext_write_field ACUTIM memorder 0' , & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'ardlw') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ARDLW'               , &  
                       grid%ardlw               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       '0'               , &  
                       ''               , &  
                       ''               , &  
                       ''               , &  
                       ''               , &  
                       '  of times LW fluxes summed before resetting'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field ARDLW memorder 0' , & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'ardsw') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ARDSW'               , &  
                       grid%ardsw               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       '0'               , &  
                       ''               , &  
                       ''               , &  
                       ''               , &  
                       ''               , &  
                       '  of times SW fluxes summed before resetting'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field ARDSW memorder 0' , & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'asrfc') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ASRFC'               , &  
                       grid%asrfc               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       '0'               , &  
                       ''               , &  
                       ''               , &  
                       ''               , &  
                       ''               , &  
                       '  of times sfc fluxes summed before resetting'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field ASRFC memorder 0' , & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'aphtim') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'APHTIM'               , &  
                       grid%aphtim               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       '0'               , &  
                       ''               , &  
                       ''               , &  
                       ''               , &  
                       ''               , &  
                       '-'               , &  
                       '-'               , &  
'inc/wrf_histout.inc ext_write_field APHTIM memorder 0' , & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'max10mw') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'MAX10MW'               , &  
                       grid%max10mw               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Maximum 10 m wind speed (m/s) in previous hour'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field MAX10MW memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'max10u') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'MAX10U'               , &  
                       grid%max10u               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Maximum 10 m wind U comp (m/s) previous hour'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field MAX10U memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'max10v') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'MAX10V'               , &  
                       grid%max10v               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Maximum 10 m wind V comp (m/s) previous hour'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field MAX10V memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'maxupdr') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'MAXUPDR'               , &  
                       grid%maxupdr               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Maximum updraft (m/s) in previous hour'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field MAXUPDR memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'maxdndr') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'MAXDNDR'               , &  
                       grid%maxdndr               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Maximum downdraft (m/s) in previous hour'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field MAXDNDR memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'maxhlcy') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'MAXHLCY'               , &  
                       grid%maxhlcy               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Maximum updraft helicity (m^2/s^2)'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field MAXHLCY memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'maxdbz') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'MAXDBZ'               , &  
                       grid%maxdbz               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'Maximum ~1000m m simulated radar reflectivity'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field MAXDBZ memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'landmask') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'LANDMASK'               , &  
                       grid%landmask               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'description'               , &  
                       'units'               , &  
'inc/wrf_histout.inc ext_write_field LANDMASK memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
DO itrace = PARAM_FIRST_SCALAR , num_moist
  IF (BTEST(moist_stream_table(grid%id, itrace ) , switch )) THEN
    CALL wrf_ext_write_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(moist_dname_table( grid%id, itrace )), & 
          grid%moist(ims,jms,kms,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          dryrun             , &  
          'XYZ'               , &  
          ''                , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       'bottom_top'               , &  
          moist_desc_table( grid%id, itrace  ), & 
          moist_units_table( grid%id, itrace  ), & 
'inc/wrf_histout.inc ext_write_field '//TRIM(moist_dname_table( grid%id, itrace ))//' memorder XYZ' , & 
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_dfi_moist
  IF (BTEST(dfi_moist_stream_table(grid%id, itrace ) , switch )) THEN
    CALL wrf_ext_write_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(dfi_moist_dname_table( grid%id, itrace )), & 
          grid%dfi_moist(ims,jms,kms,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          dryrun             , &  
          'XYZ'               , &  
          ''                , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       'bottom_top'               , &  
          dfi_moist_desc_table( grid%id, itrace  ), & 
          dfi_moist_units_table( grid%id, itrace  ), & 
'inc/wrf_histout.inc ext_write_field '//TRIM(dfi_moist_dname_table( grid%id, itrace ))//' memorder XYZ' , & 
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_scalar
  IF (BTEST(scalar_stream_table(grid%id, itrace ) , switch )) THEN
    CALL wrf_ext_write_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(scalar_dname_table( grid%id, itrace )), & 
          grid%scalar(ims,jms,kms,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          dryrun             , &  
          'XYZ'               , &  
          ''                , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       'bottom_top'               , &  
          scalar_desc_table( grid%id, itrace  ), & 
          scalar_units_table( grid%id, itrace  ), & 
'inc/wrf_histout.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder XYZ' , & 
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_dfi_scalar
  IF (BTEST(dfi_scalar_stream_table(grid%id, itrace ) , switch )) THEN
    CALL wrf_ext_write_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(dfi_scalar_dname_table( grid%id, itrace )), & 
          grid%dfi_scalar(ims,kms,jms,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          dryrun             , &  
          'XZY'               , &  
          ''                , &  
                       'west_east'               , &  
                       'bottom_top'               , &  
                       'south_north'               , &  
          dfi_scalar_desc_table( grid%id, itrace  ), & 
          dfi_scalar_units_table( grid%id, itrace  ), & 
'inc/wrf_histout.inc ext_write_field '//TRIM(dfi_scalar_dname_table( grid%id, itrace ))//' memorder XZY' , & 
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_chem
  IF (BTEST(chem_stream_table(grid%id, itrace ) , switch )) THEN
    CALL wrf_ext_write_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(chem_dname_table( grid%id, itrace )), & 
          grid%chem(ims,kms,jms,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          dryrun             , &  
          'XZY'               , &  
          ''                , &  
                       'west_east'               , &  
                       'bottom_top'               , &  
                       'south_north'               , &  
          chem_desc_table( grid%id, itrace  ), & 
          chem_units_table( grid%id, itrace  ), & 
'inc/wrf_histout.inc ext_write_field '//TRIM(chem_dname_table( grid%id, itrace ))//' memorder XZY' , & 
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                         ierr )
  ENDIF
ENDDO
IF ( in_use_for_config(grid%id,'psfc') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'PSFC'               , &  
                       grid%psfc               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'SFC PRESSURE'               , &  
                       '-'               , &  
'inc/wrf_histout.inc ext_write_field PSFC memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'th2') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'TH2'               , &  
                       grid%th2               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'POT TEMP at 2 M'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field TH2 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'u10') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'U10'               , &  
                       grid%u10               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'U at 10 M'               , &  
                       ' '               , &  
'inc/wrf_histout.inc ext_write_field U10 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'v10') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'V10'               , &  
                       grid%v10               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'V at 10 M'               , &  
                       ' '               , &  
'inc/wrf_histout.inc ext_write_field V10 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'smstav') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SMSTAV'               , &  
                       grid%smstav               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'MOISTURE VARIBILITY'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field SMSTAV memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'smstot') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SMSTOT'               , &  
                       grid%smstot               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'TOTAL SOIL MOISTURE'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field SMSTOT memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'sfcrunoff') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SFROFF'               , &  
                       grid%sfcrunoff               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'SURFACE RUNOFF'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field SFROFF memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'udrunoff') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'UDROFF'               , &  
                       grid%udrunoff               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'UNDERGROUND RUNOFF'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field UDROFF memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'ivgtyp') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'IVGTYP'               , &  
                       grid%ivgtyp               , &  
                       WRF_integer             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'VEGETATION TYPE'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field IVGTYP memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'isltyp') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ISLTYP'               , &  
                       grid%isltyp               , &  
                       WRF_integer             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'SOIL TYPE'               , &  
                       ' '               , &  
'inc/wrf_histout.inc ext_write_field ISLTYP memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'vegfra') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'VEGFRA'               , &  
                       grid%vegfra               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'VEGETATION FRACTION'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field VEGFRA memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'sfcevp') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SFCEVP'               , &  
                       grid%sfcevp               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'SURFACE EVAPORATION'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field SFCEVP memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'grdflx') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'GRDFLX'               , &  
                       grid%grdflx               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'GROUND HEAT FLUX'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field GRDFLX memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'sfcexc') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SFCEXC '               , &  
                       grid%sfcexc               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'SURFACE EXCHANGE COEFFICIENT'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field SFCEXC  memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'acsnow') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ACSNOW'               , &  
                       grid%acsnow               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'ACCUMULATED SNOW'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field ACSNOW memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'acsnom') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ACSNOM'               , &  
                       grid%acsnom               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'ACCUMULATED MELTED SNOW'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field ACSNOM memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'snow') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SNOW'               , &  
                       grid%snow               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'SNOW WATER EQUIVALENT'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field SNOW memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'canwat') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'CANWAT'               , &  
                       grid%canwat               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'CANOPY WATER'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field CANWAT memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'sst') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SST'               , &  
                       grid%sst               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'SEA SURFACE TEMPERATURE'               , &  
                       'K'               , &  
'inc/wrf_histout.inc ext_write_field SST memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'weasd') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'WEASD'               , &  
                       grid%weasd               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'WATER EQUIVALENT OF ACCUMULATED SNOW'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field WEASD memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'thz0') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'THZ0'               , &  
                       grid%thz0               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'POT. TEMPERATURE AT TOP OF VISC. SUBLYR'               , &  
                       'K'               , &  
'inc/wrf_histout.inc ext_write_field THZ0 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'qz0') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'QZ0'               , &  
                       grid%qz0               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'SPECIFIC HUMIDITY AT TOP OF VISC. SUBLYR'               , &  
                       'kg kg-1'               , &  
'inc/wrf_histout.inc ext_write_field QZ0 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'uz0') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'UZ0'               , &  
                       grid%uz0               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'U WIND COMPONENT AT TOP OF VISC. SUBLYR'               , &  
                       'm s-1'               , &  
'inc/wrf_histout.inc ext_write_field UZ0 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'vz0') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'VZ0'               , &  
                       grid%vz0               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'V WIND COMPONENT AT TOP OF VISC. SUBLYR'               , &  
                       'm s-1'               , &  
'inc/wrf_histout.inc ext_write_field VZ0 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'qsfc') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'QSFC'               , &  
                       grid%qsfc               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'SPECIFIC HUMIDITY AT LOWER BOUNDARY'               , &  
                       'kg kg-1'               , &  
'inc/wrf_histout.inc ext_write_field QSFC memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'htop') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'HTOP'               , &  
                       grid%htop               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'TOP OF CONVECTION LEVEL'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field HTOP memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'hbot') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'HBOT'               , &  
                       grid%hbot               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'BOT OF CONVECTION LEVEL'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field HBOT memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'snowh') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SNOWH'               , &  
                       grid%snowh               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       'XY'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'south_north'               , &  
                       ''               , &  
                       'PHYSICAL SNOW DEPTH'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field SNOWH memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'itimestep') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ITIMESTEP'               , &  
                       grid%itimestep               , &  
                       WRF_integer             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       '0'               , &  
                       ''               , &  
                       ''               , &  
                       ''               , &  
                       ''               , &  
                       ''               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field ITIMESTEP memorder 0' , & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'xtime') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'XTIME'               , &  
                       grid%xtime               , &  
                       WRF_FLOAT          , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask       , &  
                       dryrun             , &  
                       '0'               , &  
                       ''               , &  
                       ''               , &  
                       ''               , &  
                       ''               , &  
                       'minutes since simulation start'               , &  
                       ''               , &  
'inc/wrf_histout.inc ext_write_field XTIME memorder 0' , & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF


    RETURN
    END
