  SUBROUTINE wrf_auxinput1out ( fid , grid , config_flags, switch , &
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


    
    








IF ( in_use_for_config(grid%id,'p_gc') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'PRES'               , &  
                       grid%p_gc               , &  
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
                       'num_metgrid_levels_stag'               , &  
                       'pressure'               , &  
                       'Pa'               , &  
'inc/wrf_auxinput1out.inc ext_write_field PRES memorder XYZ' , & 
ids , (ide-1) , jds , (jde-1) , 1 , config_flags%num_metgrid_levels ,  & 
ims , ime , jms , jme , 1 , config_flags%num_metgrid_levels ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , config_flags%num_metgrid_levels ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'vegcat') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'VEGCAT'               , &  
                       grid%vegcat               , &  
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
                       'VEGETATION CAT DOMINANT TYPE'               , &  
                       ''               , &  
'inc/wrf_auxinput1out.inc ext_write_field VEGCAT memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'soilcat') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SOILCAT'               , &  
                       grid%soilcat               , &  
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
                       'SOIL CAT DOMINANT TYPE'               , &  
                       ''               , &  
'inc/wrf_auxinput1out.inc ext_write_field SOILCAT memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'input_soil_cat') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SOIL_CAT'               , &  
                       grid%input_soil_cat               , &  
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
                       'SOIL CAT DOMINANT TYPE'               , &  
                       ''               , &  
'inc/wrf_auxinput1out.inc ext_write_field SOIL_CAT memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'tsk_gc') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SKINTEMP'               , &  
                       grid%tsk_gc               , &  
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
                       'skin temperature'               , &  
                       'K'               , &  
'inc/wrf_auxinput1out.inc ext_write_field SKINTEMP memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'xice_gc') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SEAICE'               , &  
                       grid%xice_gc               , &  
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
                       'SEA ICE'               , &  
                       ''               , &  
'inc/wrf_auxinput1out.inc ext_write_field SEAICE memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'ght_gc') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'GHT'               , &  
                       grid%ght_gc               , &  
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
                       'num_metgrid_levels_stag'               , &  
                       'geopotential height'               , &  
                       'm'               , &  
'inc/wrf_auxinput1out.inc ext_write_field GHT memorder XYZ' , & 
ids , (ide-1) , jds , (jde-1) , 1 , config_flags%num_metgrid_levels ,  & 
ims , ime , jms , jme , 1 , config_flags%num_metgrid_levels ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , config_flags%num_metgrid_levels ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'rh_gc') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'RH'               , &  
                       grid%rh_gc               , &  
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
                       'num_metgrid_levels_stag'               , &  
                       'relative humidity'               , &  
                       '%'               , &  
'inc/wrf_auxinput1out.inc ext_write_field RH memorder XYZ' , & 
ids , (ide-1) , jds , (jde-1) , 1 , config_flags%num_metgrid_levels ,  & 
ims , ime , jms , jme , 1 , config_flags%num_metgrid_levels ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , config_flags%num_metgrid_levels ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'v_gc') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'VV'               , &  
                       grid%v_gc               , &  
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
                       'num_metgrid_levels_stag'               , &  
                       'y-wind component'               , &  
                       'm s-1'               , &  
'inc/wrf_auxinput1out.inc ext_write_field VV memorder XYZ' , & 
ids , (ide-1) , jds , (jde-1) , 1 , config_flags%num_metgrid_levels ,  & 
ims , ime , jms , jme , 1 , config_flags%num_metgrid_levels ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , config_flags%num_metgrid_levels ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'u_gc') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'UU'               , &  
                       grid%u_gc               , &  
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
                       'num_metgrid_levels_stag'               , &  
                       'x-wind component'               , &  
                       'm s-1'               , &  
'inc/wrf_auxinput1out.inc ext_write_field UU memorder XYZ' , & 
ids , (ide-1) , jds , (jde-1) , 1 , config_flags%num_metgrid_levels ,  & 
ims , ime , jms , jme , 1 , config_flags%num_metgrid_levels ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , config_flags%num_metgrid_levels ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'t_gc') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'TT'               , &  
                       grid%t_gc               , &  
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
                       'num_metgrid_levels_stag'               , &  
                       'temperature'               , &  
                       'K'               , &  
'inc/wrf_auxinput1out.inc ext_write_field TT memorder XYZ' , & 
ids , (ide-1) , jds , (jde-1) , 1 , config_flags%num_metgrid_levels ,  & 
ims , ime , jms , jme , 1 , config_flags%num_metgrid_levels ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , config_flags%num_metgrid_levels ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'snoalb') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SNOALB'               , &  
                       grid%snoalb               , &  
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
                       'ANNUAL MAX SNOW ALBEDO IN FRACTION'               , &  
                       ''               , &  
'inc/wrf_auxinput1out.inc ext_write_field SNOALB memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'greenfrac_gc') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'GREENFRAC'               , &  
                       grid%greenfrac_gc               , &  
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
                       'months_per_year_stag'               , &  
                       'monthly greenness fraction'               , &  
                       '0 - 1 fraction'               , &  
'inc/wrf_auxinput1out.inc ext_write_field GREENFRAC memorder XYZ' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 12 ,  & 
ims , ime , jms , jme , 1 , 12 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 12 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'albedo12m_gc') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ALBEDO12M'               , &  
                       grid%albedo12m_gc               , &  
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
                       'months_per_year_stag'               , &  
                       'background albedo'               , &  
                       '0 - 1 fraction'               , &  
'inc/wrf_auxinput1out.inc ext_write_field ALBEDO12M memorder XYZ' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 12 ,  & 
ims , ime , jms , jme , 1 , 12 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 12 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'soilcbot_gc') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SOILCBOT'               , &  
                       grid%soilcbot_gc               , &  
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
                       'soil_cat_stag'               , &  
                       'description'               , &  
                       'units'               , &  
'inc/wrf_auxinput1out.inc ext_write_field SOILCBOT memorder XYZ' , & 
ids , (ide-1) , jds , (jde-1) , 1 , config_flags%num_soil_cat ,  & 
ims , ime , jms , jme , 1 , config_flags%num_soil_cat ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , config_flags%num_soil_cat ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'soilctop_gc') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SOILCTOP'               , &  
                       grid%soilctop_gc               , &  
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
                       'soil_cat_stag'               , &  
                       'description'               , &  
                       'units'               , &  
'inc/wrf_auxinput1out.inc ext_write_field SOILCTOP memorder XYZ' , & 
ids , (ide-1) , jds , (jde-1) , 1 , config_flags%num_soil_cat ,  & 
ims , ime , jms , jme , 1 , config_flags%num_soil_cat ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , config_flags%num_soil_cat ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'tmn_gc') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SOILTEMP'               , &  
                       grid%tmn_gc               , &  
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
                       'annual mean deep soil temperature'               , &  
                       'K'               , &  
'inc/wrf_auxinput1out.inc ext_write_field SOILTEMP memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'htv_gc') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'HGT_V'               , &  
                       grid%htv_gc               , &  
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
                       'wind point topography elevation'               , &  
                       'm'               , &  
'inc/wrf_auxinput1out.inc ext_write_field HGT_V memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'ht_gc') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'HGT_M'               , &  
                       grid%ht_gc               , &  
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
                       'mass point topography elevation'               , &  
                       'm'               , &  
'inc/wrf_auxinput1out.inc ext_write_field HGT_M memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'landusef_gc') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'LANDUSEF'               , &  
                       grid%landusef_gc               , &  
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
                       'land_cat_stag'               , &  
                       'description'               , &  
                       'units'               , &  
'inc/wrf_auxinput1out.inc ext_write_field LANDUSEF memorder XYZ' , & 
ids , (ide-1) , jds , (jde-1) , 1 , config_flags%num_land_cat ,  & 
ims , ime , jms , jme , 1 , config_flags%num_land_cat ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , config_flags%num_land_cat ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'vlon_gc') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'XLONG_V'               , &  
                       grid%vlon_gc               , &  
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
                       'longitude, positive east'               , &  
                       'degrees'               , &  
'inc/wrf_auxinput1out.inc ext_write_field XLONG_V memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'vlat_gc') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'XLAT_V'               , &  
                       grid%vlat_gc               , &  
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
                       'latitude, positive north'               , &  
                       'degrees'               , &  
'inc/wrf_auxinput1out.inc ext_write_field XLAT_V memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'hlon_gc') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'XLONG_M'               , &  
                       grid%hlon_gc               , &  
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
                       'longitude, positive east'               , &  
                       'degrees'               , &  
'inc/wrf_auxinput1out.inc ext_write_field XLONG_M memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'hlat_gc') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'XLAT_M'               , &  
                       grid%hlat_gc               , &  
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
                       'latitude, positive north'               , &  
                       'degrees'               , &  
'inc/wrf_auxinput1out.inc ext_write_field XLAT_M memorder XY' , & 
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
'inc/wrf_auxinput1out.inc ext_write_field SM memorder XY' , & 
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
'inc/wrf_auxinput1out.inc ext_write_field PD memorder XY' , & 
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
'inc/wrf_auxinput1out.inc ext_write_field FIS memorder XY' , & 
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
'inc/wrf_auxinput1out.inc ext_write_field T memorder XYZ' , & 
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
'inc/wrf_auxinput1out.inc ext_write_field Q memorder XYZ' , & 
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
'inc/wrf_auxinput1out.inc ext_write_field U memorder XYZ' , & 
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
'inc/wrf_auxinput1out.inc ext_write_field V memorder XYZ' , & 
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'deta') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'DETA'               , &  
                       grid%deta               , &  
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
'inc/wrf_auxinput1out.inc ext_write_field DETA memorder Z' , & 
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'aeta') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'AETA'               , &  
                       grid%aeta               , &  
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
                       '-'               , &  
                       '-'               , &  
'inc/wrf_auxinput1out.inc ext_write_field AETA memorder Z' , & 
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'etax') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ETAX'               , &  
                       grid%etax               , &  
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
                       '-'               , &  
                       '-'               , &  
'inc/wrf_auxinput1out.inc ext_write_field ETAX memorder Z' , & 
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'dfl') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'DFL'               , &  
                       grid%dfl               , &  
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
                       'Standard atmosphere geopotential'               , &  
                       'm2 s-2'               , &  
'inc/wrf_auxinput1out.inc ext_write_field DFL memorder Z' , & 
kds , kde , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( kde, kpe ) , 1 , 1 , 1 , 1 ,  & 
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
'inc/wrf_auxinput1out.inc ext_write_field DETA1 memorder Z' , & 
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
'inc/wrf_auxinput1out.inc ext_write_field AETA1 memorder Z' , & 
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
'inc/wrf_auxinput1out.inc ext_write_field ETA1 memorder Z' , & 
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
'inc/wrf_auxinput1out.inc ext_write_field DETA2 memorder Z' , & 
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
'inc/wrf_auxinput1out.inc ext_write_field AETA2 memorder Z' , & 
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
'inc/wrf_auxinput1out.inc ext_write_field ETA2 memorder Z' , & 
kds , kde , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( kde, kpe ) , 1 , 1 , 1 , 1 ,  & 
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
'inc/wrf_auxinput1out.inc ext_write_field PDTOP memorder 0' , & 
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
'inc/wrf_auxinput1out.inc ext_write_field PT memorder 0' , & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
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
'inc/wrf_auxinput1out.inc ext_write_field Z0 memorder XY' , & 
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
'inc/wrf_auxinput1out.inc ext_write_field ALBASE memorder XY' , & 
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
'inc/wrf_auxinput1out.inc ext_write_field GLAT memorder XY' , & 
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
'inc/wrf_auxinput1out.inc ext_write_field GLON memorder XY' , & 
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
'inc/wrf_auxinput1out.inc ext_write_field TSK memorder XY' , & 
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
'inc/wrf_auxinput1out.inc ext_write_field MXSNAL memorder XY' , & 
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
'inc/wrf_auxinput1out.inc ext_write_field TGROUND memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'dfrlg') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'DFRLG'               , &  
                       grid%dfrlg               , &  
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
                       'Std atmosphere height of model layer interfaces'               , &  
                       'm'               , &  
'inc/wrf_auxinput1out.inc ext_write_field DFRLG memorder Z' , & 
kds , kde , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( kde, kpe ) , 1 , 1 , 1 , 1 ,  & 
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
'inc/wrf_auxinput1out.inc ext_write_field ISLOPE memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'rtdpth') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'RTDPTH'               , &  
                       grid%rtdpth               , &  
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
                       '-'               , &  
                       '-'               , &  
'inc/wrf_auxinput1out.inc ext_write_field RTDPTH memorder Z' , & 
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
'inc/wrf_auxinput1out.inc ext_write_field SLDPTH memorder Z' , & 
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
'inc/wrf_auxinput1out.inc ext_write_field CMC memorder XY' , & 
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
'inc/wrf_auxinput1out.inc ext_write_field SOILTB memorder XY' , & 
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
'inc/wrf_auxinput1out.inc ext_write_field VEGFRC memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'hstdv') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'HSTDV'               , &  
                       grid%hstdv               , &  
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
                       'Standard deviation of height'               , &  
                       'm'               , &  
'inc/wrf_auxinput1out.inc ext_write_field HSTDV memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'hcnvx') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'HCNVX'               , &  
                       grid%hcnvx               , &  
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
                       'Normalized 4th moment of orographic convexity'               , &  
                       ''               , &  
'inc/wrf_auxinput1out.inc ext_write_field HCNVX memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'hasyw') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'HASYW'               , &  
                       grid%hasyw               , &  
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
                       'Orographic asymmetry in W-E plane'               , &  
                       ''               , &  
'inc/wrf_auxinput1out.inc ext_write_field HASYW memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'hasys') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'HASYS'               , &  
                       grid%hasys               , &  
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
                       'Orographic asymmetry in S-N plane'               , &  
                       ''               , &  
'inc/wrf_auxinput1out.inc ext_write_field HASYS memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'hasysw') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'HASYSW'               , &  
                       grid%hasysw               , &  
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
                       'Orographic asymmetry in SW-NE plane'               , &  
                       ''               , &  
'inc/wrf_auxinput1out.inc ext_write_field HASYSW memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'hasynw') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'HASYNW'               , &  
                       grid%hasynw               , &  
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
                       'Orographic asymmetry in NW-SE plane'               , &  
                       ''               , &  
'inc/wrf_auxinput1out.inc ext_write_field HASYNW memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'hlenw') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'HLENW'               , &  
                       grid%hlenw               , &  
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
                       'Orographic length scale in W-E plane'               , &  
                       ''               , &  
'inc/wrf_auxinput1out.inc ext_write_field HLENW memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'hlens') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'HLENS'               , &  
                       grid%hlens               , &  
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
                       'Orographic length scale in S-N plane'               , &  
                       ''               , &  
'inc/wrf_auxinput1out.inc ext_write_field HLENS memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'hlensw') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'HLENSW'               , &  
                       grid%hlensw               , &  
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
                       'Orographic length scale in SW-NE plane'               , &  
                       ''               , &  
'inc/wrf_auxinput1out.inc ext_write_field HLENSW memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'hlennw') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'HLENNW'               , &  
                       grid%hlennw               , &  
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
                       'Orographic length scale in NW-SE plane'               , &  
                       ''               , &  
'inc/wrf_auxinput1out.inc ext_write_field HLENNW memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'hangl') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'HANGL'               , &  
                       grid%hangl               , &  
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
                       'Angle of the mountain range w/r/t east'               , &  
                       'deg'               , &  
'inc/wrf_auxinput1out.inc ext_write_field HANGL memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'hanis') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'HANIS'               , &  
                       grid%hanis               , &  
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
                       'Anisotropy/aspect ratio of orography'               , &  
                       ''               , &  
'inc/wrf_auxinput1out.inc ext_write_field HANIS memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'hslop') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'HSLOP'               , &  
                       grid%hslop               , &  
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
                       'Slope of orography'               , &  
                       ''               , &  
'inc/wrf_auxinput1out.inc ext_write_field HSLOP memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'hzmax') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'HZMAX'               , &  
                       grid%hzmax               , &  
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
                       'Maximum height above mean orography'               , &  
                       'm'               , &  
'inc/wrf_auxinput1out.inc ext_write_field HZMAX memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'sm000007') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SM000007'               , &  
                       grid%sm000007               , &  
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
                       'LAYER SOIL MOISTURE'               , &  
                       'm3 m-3'               , &  
'inc/wrf_auxinput1out.inc ext_write_field SM000007 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'sm007028') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SM007028'               , &  
                       grid%sm007028               , &  
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
                       'LAYER SOIL MOISTURE'               , &  
                       'm3 m-3'               , &  
'inc/wrf_auxinput1out.inc ext_write_field SM007028 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'sm028100') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SM028100'               , &  
                       grid%sm028100               , &  
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
                       'LAYER SOIL MOISTURE'               , &  
                       'm3 m-3'               , &  
'inc/wrf_auxinput1out.inc ext_write_field SM028100 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'sm100255') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SM100255'               , &  
                       grid%sm100255               , &  
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
                       'LAYER SOIL MOISTURE'               , &  
                       'm3 m-3'               , &  
'inc/wrf_auxinput1out.inc ext_write_field SM100255 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'st000007') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ST000007'               , &  
                       grid%st000007               , &  
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
                       'LAYER SOIL TEMPERATURE'               , &  
                       'K'               , &  
'inc/wrf_auxinput1out.inc ext_write_field ST000007 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'st007028') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ST007028'               , &  
                       grid%st007028               , &  
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
                       'LAYER SOIL TEMPERATURE'               , &  
                       'K'               , &  
'inc/wrf_auxinput1out.inc ext_write_field ST007028 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'st028100') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ST028100'               , &  
                       grid%st028100               , &  
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
                       'LAYER SOIL TEMPERATURE'               , &  
                       'K'               , &  
'inc/wrf_auxinput1out.inc ext_write_field ST028100 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'st100255') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ST100255'               , &  
                       grid%st100255               , &  
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
                       'LAYER SOIL TEMPERATURE'               , &  
                       'K'               , &  
'inc/wrf_auxinput1out.inc ext_write_field ST100255 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'sm000010') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SM000010'               , &  
                       grid%sm000010               , &  
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
'inc/wrf_auxinput1out.inc ext_write_field SM000010 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'sm010040') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SM010040 '               , &  
                       grid%sm010040               , &  
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
'inc/wrf_auxinput1out.inc ext_write_field SM010040  memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'sm040100') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SM040100 '               , &  
                       grid%sm040100               , &  
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
'inc/wrf_auxinput1out.inc ext_write_field SM040100  memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'sm100200') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SM100200 '               , &  
                       grid%sm100200               , &  
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
'inc/wrf_auxinput1out.inc ext_write_field SM100200  memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'sm010200') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SM010200'               , &  
                       grid%sm010200               , &  
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
'inc/wrf_auxinput1out.inc ext_write_field SM010200 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'soilm000') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SOILM000'               , &  
                       grid%soilm000               , &  
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
'inc/wrf_auxinput1out.inc ext_write_field SOILM000 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'soilm005') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SOILM005'               , &  
                       grid%soilm005               , &  
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
'inc/wrf_auxinput1out.inc ext_write_field SOILM005 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'soilm020') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SOILM020'               , &  
                       grid%soilm020               , &  
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
'inc/wrf_auxinput1out.inc ext_write_field SOILM020 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'soilm040') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SOILM040'               , &  
                       grid%soilm040               , &  
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
'inc/wrf_auxinput1out.inc ext_write_field SOILM040 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'soilm160') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SOILM160'               , &  
                       grid%soilm160               , &  
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
'inc/wrf_auxinput1out.inc ext_write_field SOILM160 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'soilm300') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SOILM300'               , &  
                       grid%soilm300               , &  
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
'inc/wrf_auxinput1out.inc ext_write_field SOILM300 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'sw000010') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SW000010'               , &  
                       grid%sw000010               , &  
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
'inc/wrf_auxinput1out.inc ext_write_field SW000010 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'sw010040') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SW010040'               , &  
                       grid%sw010040               , &  
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
'inc/wrf_auxinput1out.inc ext_write_field SW010040 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'sw040100') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SW040100'               , &  
                       grid%sw040100               , &  
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
'inc/wrf_auxinput1out.inc ext_write_field SW040100 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'sw100200') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SW100200'               , &  
                       grid%sw100200               , &  
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
'inc/wrf_auxinput1out.inc ext_write_field SW100200 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'sw010200') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SW010200'               , &  
                       grid%sw010200               , &  
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
'inc/wrf_auxinput1out.inc ext_write_field SW010200 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'soilw000') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SOILW000'               , &  
                       grid%soilw000               , &  
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
'inc/wrf_auxinput1out.inc ext_write_field SOILW000 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'soilw005') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SOILW005'               , &  
                       grid%soilw005               , &  
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
'inc/wrf_auxinput1out.inc ext_write_field SOILW005 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'soilw020') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SOILW020'               , &  
                       grid%soilw020               , &  
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
'inc/wrf_auxinput1out.inc ext_write_field SOILW020 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'soilw040') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SOILW040'               , &  
                       grid%soilw040               , &  
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
'inc/wrf_auxinput1out.inc ext_write_field SOILW040 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'soilw160') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SOILW160'               , &  
                       grid%soilw160               , &  
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
'inc/wrf_auxinput1out.inc ext_write_field SOILW160 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'soilw300') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SOILW300'               , &  
                       grid%soilw300               , &  
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
'inc/wrf_auxinput1out.inc ext_write_field SOILW300 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'st000010') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ST000010'               , &  
                       grid%st000010               , &  
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
'inc/wrf_auxinput1out.inc ext_write_field ST000010 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'st010040') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ST010040'               , &  
                       grid%st010040               , &  
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
'inc/wrf_auxinput1out.inc ext_write_field ST010040 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'st040100') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ST040100'               , &  
                       grid%st040100               , &  
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
'inc/wrf_auxinput1out.inc ext_write_field ST040100 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'st100200') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ST100200'               , &  
                       grid%st100200               , &  
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
'inc/wrf_auxinput1out.inc ext_write_field ST100200 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'st010200') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ST010200'               , &  
                       grid%st010200               , &  
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
'inc/wrf_auxinput1out.inc ext_write_field ST010200 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'soilt000') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SOILT000'               , &  
                       grid%soilt000               , &  
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
'inc/wrf_auxinput1out.inc ext_write_field SOILT000 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'soilt005') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SOILT005'               , &  
                       grid%soilt005               , &  
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
'inc/wrf_auxinput1out.inc ext_write_field SOILT005 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'soilt020') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SOILT020'               , &  
                       grid%soilt020               , &  
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
'inc/wrf_auxinput1out.inc ext_write_field SOILT020 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'soilt040') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SOILT040'               , &  
                       grid%soilt040               , &  
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
'inc/wrf_auxinput1out.inc ext_write_field SOILT040 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'soilt160') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SOILT160'               , &  
                       grid%soilt160               , &  
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
'inc/wrf_auxinput1out.inc ext_write_field SOILT160 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'soilt300') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SOILT300'               , &  
                       grid%soilt300               , &  
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
'inc/wrf_auxinput1out.inc ext_write_field SOILT300 memorder XY' , & 
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
'inc/wrf_auxinput1out.inc ext_write_field LANDMASK memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'topostdv') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'TOPOSTDV'               , &  
                       grid%topostdv               , &  
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
'inc/wrf_auxinput1out.inc ext_write_field TOPOSTDV memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'toposlpx') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'TOPOSLPX'               , &  
                       grid%toposlpx               , &  
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
'inc/wrf_auxinput1out.inc ext_write_field TOPOSLPX memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'toposlpy') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'TOPOSLPY'               , &  
                       grid%toposlpy               , &  
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
'inc/wrf_auxinput1out.inc ext_write_field TOPOSLPY memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'greenmax') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'GREENMAX'               , &  
                       grid%greenmax               , &  
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
'inc/wrf_auxinput1out.inc ext_write_field GREENMAX memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'greenmin') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'GREENMIN'               , &  
                       grid%greenmin               , &  
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
'inc/wrf_auxinput1out.inc ext_write_field GREENMIN memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'albedomx') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ALBEDOMX'               , &  
                       grid%albedomx               , &  
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
'inc/wrf_auxinput1out.inc ext_write_field ALBEDOMX memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'slopecat') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SLOPECAT'               , &  
                       grid%slopecat               , &  
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
'inc/wrf_auxinput1out.inc ext_write_field SLOPECAT memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'toposoil') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SOILHGT'               , &  
                       grid%toposoil               , &  
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
'inc/wrf_auxinput1out.inc ext_write_field SOILHGT memorder XY' , & 
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
'inc/wrf_auxinput1out.inc ext_write_field '//TRIM(moist_dname_table( grid%id, itrace ))//' memorder XYZ' , & 
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
'inc/wrf_auxinput1out.inc ext_write_field '//TRIM(dfi_moist_dname_table( grid%id, itrace ))//' memorder XYZ' , & 
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
'inc/wrf_auxinput1out.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder XYZ' , & 
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
'inc/wrf_auxinput1out.inc ext_write_field '//TRIM(dfi_scalar_dname_table( grid%id, itrace ))//' memorder XZY' , & 
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
'inc/wrf_auxinput1out.inc ext_write_field '//TRIM(chem_dname_table( grid%id, itrace ))//' memorder XZY' , & 
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
'inc/wrf_auxinput1out.inc ext_write_field PSFC memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'xice') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'XICE'               , &  
                       grid%xice               , &  
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
                       'SEA ICE'               , &  
                       ''               , &  
'inc/wrf_auxinput1out.inc ext_write_field XICE memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'lai') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'LAI'               , &  
                       grid%lai               , &  
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
                       'Leaf area index'               , &  
                       'area/area'               , &  
'inc/wrf_auxinput1out.inc ext_write_field LAI memorder XY' , & 
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
'inc/wrf_auxinput1out.inc ext_write_field VEGFRA memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'albbck') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ALBBCK'               , &  
                       grid%albbck               , &  
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
                       'BACKGROUND ALBEDO'               , &  
                       'NA'               , &  
'inc/wrf_auxinput1out.inc ext_write_field ALBBCK memorder XY' , & 
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
'inc/wrf_auxinput1out.inc ext_write_field SNOW memorder XY' , & 
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
'inc/wrf_auxinput1out.inc ext_write_field CANWAT memorder XY' , & 
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
'inc/wrf_auxinput1out.inc ext_write_field SST memorder XY' , & 
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
'inc/wrf_auxinput1out.inc ext_write_field WEASD memorder XY' , & 
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
'inc/wrf_auxinput1out.inc ext_write_field SNOWH memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'rhosn') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'RHOSN'               , &  
                       grid%rhosn               , &  
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
                       ' SNOW DENSITY'               , &  
                       'kg m-3'               , &  
'inc/wrf_auxinput1out.inc ext_write_field RHOSN memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF


    RETURN
    END
