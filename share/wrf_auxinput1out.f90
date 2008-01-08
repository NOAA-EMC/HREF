  SUBROUTINE wrf_auxinput1out ( fid , grid , config_flags, switch , &
                                dryrun, ierr )
    USE module_io
    USE module_wrf_error
    USE module_io_wrf
    USE module_domain
    USE module_state_description
    USE module_configure
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
! This bit is for backwards compatibility with old variants of these flags 
! that are still being used in io_grib1 and io_phdf5.  It should be removed!  
      integer, parameter  :: WRF_FILE_OPENED_AND_COMMITTED        = 102
  
!WRF Error and Warning messages (1-999)
!All i/o package-specific status codes you may want to add must be handled by your package (see below)
! WRF handles these and netCDF messages only
  integer, parameter  :: WRF_NO_ERR                  =  0       !no error
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       !file not found, or incomplete
  integer, parameter  :: WRF_WARN_MD_NF              = -2       !metadata not found
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       !timestamp not found
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       !no more timestamps
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       !variable not found
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       !no more variables for the current time
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       !too many open files
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       !data type mismatch
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       !attempt to write readonly file
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      !attempt to read writeonly file
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      !attempt to access unopened file
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      !attempt to do 2 trainings for 1 variable
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      !attempt to read past EOF
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      !bad data handle
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      !write length not equal to training length
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      !more dimensions requested than training
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      !attempt to read more data than exists
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      !input dimension inconsistent
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      !input MemoryOrder not recognized
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      !a dimension name with 2 different lengths
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      !string longer than provided storage
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      !function not supportable
  integer, parameter  :: WRF_WARN_NOOP               = -23      !package implements this routine as NOOP

!Fatal errors 
  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 !allocation error
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 !dealloc error
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 !bad file status


!Package specific errors (1000+)        
!Netcdf status codes
!WRF will accept status codes of 1000+, but it is up to the package to handle
! and return the status to the user.

  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    !this code not in either spec?
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    !this code not in either spec?
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

! For HDF5 only
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

    ! Local data
    INTEGER ids , ide , jds , jde , kds , kde , &
            ims , ime , jms , jme , kms , kme , &
            ips , ipe , jps , jpe , kps , kpe

    INTEGER       itrace
    INTEGER , DIMENSION(3) :: domain_start , domain_end
    INTEGER , DIMENSION(3) :: memory_start , memory_end
    INTEGER , DIMENSION(3) :: patch_start , patch_end
    INTEGER i,j
    INTEGER julyr, julday, idt, iswater , map_proj
    REAL    gmt, cen_lat, cen_lon, bdyfrq , truelat1 , truelat2
    INTEGER dyn_opt, diff_opt, km_opt, damp_opt,  &
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

    call nl_get_dyn_opt ( 1 , dyn_opt                       )

    ! note that the string current_date comes in through use association
    ! of module_io_wrf

! generated by the registry
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/wrf_auxinput1out.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PRES'               , &  ! Data Name 
                       grid%nmm_p_gc               , &  ! Field 
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
                       'num_metgrid_levels_stag'               , &  ! Dimname 3 
                       'pressure'               , &  ! Desc  
                       'Pa'               , &  ! Units 
'inc/wrf_auxinput1out.inc ext_write_field PRES memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , config_flags%num_metgrid_levels ,  & 
ims , ime , jms , jme , 1 , config_flags%num_metgrid_levels ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , config_flags%num_metgrid_levels ,  & 
                       ierr )
END IF
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'VEGCAT'               , &  ! Data Name 
                       grid%vegcat               , &  ! Field 
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
                       'VEGETATION CAT DOMINANT TYPE'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_auxinput1out.inc ext_write_field VEGCAT memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SOILCAT'               , &  ! Data Name 
                       grid%soilcat               , &  ! Field 
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
                       'SOIL CAT DOMINANT TYPE'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_auxinput1out.inc ext_write_field SOILCAT memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SOIL_CAT'               , &  ! Data Name 
                       grid%input_soil_cat               , &  ! Field 
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
                       'SOIL CAT DOMINANT TYPE'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_auxinput1out.inc ext_write_field SOIL_CAT memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SKINTEMP'               , &  ! Data Name 
                       grid%nmm_tsk_gc               , &  ! Field 
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
                       'skin temperature'               , &  ! Desc  
                       'K'               , &  ! Units 
'inc/wrf_auxinput1out.inc ext_write_field SKINTEMP memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
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
'inc/wrf_auxinput1out.inc ext_write_field SEAICE memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'GHT'               , &  ! Data Name 
                       grid%nmm_ght_gc               , &  ! Field 
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
                       'num_metgrid_levels_stag'               , &  ! Dimname 3 
                       'geopotential height'               , &  ! Desc  
                       'm'               , &  ! Units 
'inc/wrf_auxinput1out.inc ext_write_field GHT memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , config_flags%num_metgrid_levels ,  & 
ims , ime , jms , jme , 1 , config_flags%num_metgrid_levels ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , config_flags%num_metgrid_levels ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RH'               , &  ! Data Name 
                       grid%nmm_rh_gc               , &  ! Field 
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
                       'num_metgrid_levels_stag'               , &  ! Dimname 3 
                       'relative humidity'               , &  ! Desc  
                       '%'               , &  ! Units 
'inc/wrf_auxinput1out.inc ext_write_field RH memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , config_flags%num_metgrid_levels ,  & 
ims , ime , jms , jme , 1 , config_flags%num_metgrid_levels ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , config_flags%num_metgrid_levels ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'VV'               , &  ! Data Name 
                       grid%nmm_v_gc               , &  ! Field 
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
                       'num_metgrid_levels_stag'               , &  ! Dimname 3 
                       'y-wind component'               , &  ! Desc  
                       'm s-1'               , &  ! Units 
'inc/wrf_auxinput1out.inc ext_write_field VV memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , config_flags%num_metgrid_levels ,  & 
ims , ime , jms , jme , 1 , config_flags%num_metgrid_levels ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , config_flags%num_metgrid_levels ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'UU'               , &  ! Data Name 
                       grid%nmm_u_gc               , &  ! Field 
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
                       'num_metgrid_levels_stag'               , &  ! Dimname 3 
                       'x-wind component'               , &  ! Desc  
                       'm s-1'               , &  ! Units 
'inc/wrf_auxinput1out.inc ext_write_field UU memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , config_flags%num_metgrid_levels ,  & 
ims , ime , jms , jme , 1 , config_flags%num_metgrid_levels ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , config_flags%num_metgrid_levels ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'TT'               , &  ! Data Name 
                       grid%nmm_t_gc               , &  ! Field 
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
                       'num_metgrid_levels_stag'               , &  ! Dimname 3 
                       'temperature'               , &  ! Desc  
                       'K'               , &  ! Units 
'inc/wrf_auxinput1out.inc ext_write_field TT memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , config_flags%num_metgrid_levels ,  & 
ims , ime , jms , jme , 1 , config_flags%num_metgrid_levels ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , config_flags%num_metgrid_levels ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RWMR'               , &  ! Data Name 
                       grid%nmm_rwmr_gc               , &  ! Field 
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
                       'num_metgrid_levels_stag'               , &  ! Dimname 3 
                       'Rain water mixing ratio'               , &  ! Desc  
                       'kg/kg'               , &  ! Units 
'inc/wrf_auxinput1out.inc ext_write_field RWMR memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , config_flags%num_metgrid_levels ,  & 
ims , ime , jms , jme , 1 , config_flags%num_metgrid_levels ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , config_flags%num_metgrid_levels ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SNMR'               , &  ! Data Name 
                       grid%nmm_snmr_gc               , &  ! Field 
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
                       'num_metgrid_levels_stag'               , &  ! Dimname 3 
                       'Snow mixing ratio'               , &  ! Desc  
                       'kg/kg'               , &  ! Units 
'inc/wrf_auxinput1out.inc ext_write_field SNMR memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , config_flags%num_metgrid_levels ,  & 
ims , ime , jms , jme , 1 , config_flags%num_metgrid_levels ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , config_flags%num_metgrid_levels ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CLWMR'               , &  ! Data Name 
                       grid%nmm_clwmr_gc               , &  ! Field 
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
                       'num_metgrid_levels_stag'               , &  ! Dimname 3 
                       'Snow mixing ratio'               , &  ! Desc  
                       'kg/kg'               , &  ! Units 
'inc/wrf_auxinput1out.inc ext_write_field CLWMR memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , config_flags%num_metgrid_levels ,  & 
ims , ime , jms , jme , 1 , config_flags%num_metgrid_levels ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , config_flags%num_metgrid_levels ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CICE'               , &  ! Data Name 
                       grid%nmm_cice_gc               , &  ! Field 
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
                       'num_metgrid_levels_stag'               , &  ! Dimname 3 
                       'Cloud ice concentration'               , &  ! Desc  
                       'kg/kg'               , &  ! Units 
'inc/wrf_auxinput1out.inc ext_write_field CICE memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , config_flags%num_metgrid_levels ,  & 
ims , ime , jms , jme , 1 , config_flags%num_metgrid_levels ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , config_flags%num_metgrid_levels ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RIMEF'               , &  ! Data Name 
                       grid%nmm_rimef_gc               , &  ! Field 
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
                       'num_metgrid_levels_stag'               , &  ! Dimname 3 
                       'Rime factor'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_auxinput1out.inc ext_write_field RIMEF memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , config_flags%num_metgrid_levels ,  & 
ims , ime , jms , jme , 1 , config_flags%num_metgrid_levels ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , config_flags%num_metgrid_levels ,  & 
                       ierr )
END IF
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
'inc/wrf_auxinput1out.inc ext_write_field SNOALB memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'GREENFRAC'               , &  ! Data Name 
                       grid%nmm_greenfrac_gc               , &  ! Field 
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
                       'months_per_year_stag'               , &  ! Dimname 3 
                       'monthly greenness fraction'               , &  ! Desc  
                       '0 - 1 fraction'               , &  ! Units 
'inc/wrf_auxinput1out.inc ext_write_field GREENFRAC memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 12 ,  & 
ims , ime , jms , jme , 1 , 12 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 12 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ALBEDO12M'               , &  ! Data Name 
                       grid%nmm_albedo12m_gc               , &  ! Field 
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
                       'months_per_year_stag'               , &  ! Dimname 3 
                       'background albedo'               , &  ! Desc  
                       '0 - 1 fraction'               , &  ! Units 
'inc/wrf_auxinput1out.inc ext_write_field ALBEDO12M memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 12 ,  & 
ims , ime , jms , jme , 1 , 12 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 12 ,  & 
                       ierr )
END IF
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SOILCBOT'               , &  ! Data Name 
                       grid%soilcbot_gc               , &  ! Field 
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
                       'soil_cat_stag'               , &  ! Dimname 3 
                       'description'               , &  ! Desc  
                       'units'               , &  ! Units 
'inc/wrf_auxinput1out.inc ext_write_field SOILCBOT memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , config_flags%num_soil_cat ,  & 
ims , ime , jms , jme , 1 , config_flags%num_soil_cat ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , config_flags%num_soil_cat ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SOILCTOP'               , &  ! Data Name 
                       grid%soilctop_gc               , &  ! Field 
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
                       'soil_cat_stag'               , &  ! Dimname 3 
                       'description'               , &  ! Desc  
                       'units'               , &  ! Units 
'inc/wrf_auxinput1out.inc ext_write_field SOILCTOP memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , config_flags%num_soil_cat ,  & 
ims , ime , jms , jme , 1 , config_flags%num_soil_cat ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , config_flags%num_soil_cat ,  & 
                       ierr )
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SOILTEMP'               , &  ! Data Name 
                       grid%nmm_tmn_gc               , &  ! Field 
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
                       'annual mean deep soil temperature'               , &  ! Desc  
                       'K'               , &  ! Units 
'inc/wrf_auxinput1out.inc ext_write_field SOILTEMP memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'HGT_V'               , &  ! Data Name 
                       grid%nmm_htv_gc               , &  ! Field 
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
                       'wind point topography elevation'               , &  ! Desc  
                       'm'               , &  ! Units 
'inc/wrf_auxinput1out.inc ext_write_field HGT_V memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'HGT_M'               , &  ! Data Name 
                       grid%nmm_ht_gc               , &  ! Field 
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
                       'mass point topography elevation'               , &  ! Desc  
                       'm'               , &  ! Units 
'inc/wrf_auxinput1out.inc ext_write_field HGT_M memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LANDUSEF'               , &  ! Data Name 
                       grid%landusef_gc               , &  ! Field 
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
                       'land_cat_stag'               , &  ! Dimname 3 
                       'description'               , &  ! Desc  
                       'units'               , &  ! Units 
'inc/wrf_auxinput1out.inc ext_write_field LANDUSEF memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , config_flags%num_land_cat ,  & 
ims , ime , jms , jme , 1 , config_flags%num_land_cat ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , config_flags%num_land_cat ,  & 
                       ierr )
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'XLONG_V'               , &  ! Data Name 
                       grid%nmm_vlon_gc               , &  ! Field 
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
                       'longitude, positive east'               , &  ! Desc  
                       'degrees'               , &  ! Units 
'inc/wrf_auxinput1out.inc ext_write_field XLONG_V memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'XLAT_V'               , &  ! Data Name 
                       grid%nmm_vlat_gc               , &  ! Field 
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
                       'latitude, positive north'               , &  ! Desc  
                       'degrees'               , &  ! Units 
'inc/wrf_auxinput1out.inc ext_write_field XLAT_V memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'XLONG_M'               , &  ! Data Name 
                       grid%nmm_hlon_gc               , &  ! Field 
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
                       'longitude, positive east'               , &  ! Desc  
                       'degrees'               , &  ! Units 
'inc/wrf_auxinput1out.inc ext_write_field XLONG_M memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'XLAT_M'               , &  ! Data Name 
                       grid%nmm_hlat_gc               , &  ! Field 
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
                       'latitude, positive north'               , &  ! Desc  
                       'degrees'               , &  ! Units 
'inc/wrf_auxinput1out.inc ext_write_field XLAT_M memorder XY' , & ! Debug message
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
'inc/wrf_auxinput1out.inc ext_write_field SM memorder XY' , & ! Debug message
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
'inc/wrf_auxinput1out.inc ext_write_field PD memorder XY' , & ! Debug message
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
'inc/wrf_auxinput1out.inc ext_write_field FIS memorder XY' , & ! Debug message
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
'inc/wrf_auxinput1out.inc ext_write_field T memorder XYZ' , & ! Debug message
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
'inc/wrf_auxinput1out.inc ext_write_field Q memorder XYZ' , & ! Debug message
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
'inc/wrf_auxinput1out.inc ext_write_field U memorder XYZ' , & ! Debug message
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
'inc/wrf_auxinput1out.inc ext_write_field V memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
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
'inc/wrf_auxinput1out.inc ext_write_field DETA memorder Z' , & ! Debug message
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
'inc/wrf_auxinput1out.inc ext_write_field AETA memorder Z' , & ! Debug message
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
'inc/wrf_auxinput1out.inc ext_write_field ETAX memorder Z' , & ! Debug message
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
'inc/wrf_auxinput1out.inc ext_write_field DFL memorder Z' , & ! Debug message
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
'inc/wrf_auxinput1out.inc ext_write_field DETA1 memorder Z' , & ! Debug message
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
'inc/wrf_auxinput1out.inc ext_write_field AETA1 memorder Z' , & ! Debug message
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
'inc/wrf_auxinput1out.inc ext_write_field ETA1 memorder Z' , & ! Debug message
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
'inc/wrf_auxinput1out.inc ext_write_field DETA2 memorder Z' , & ! Debug message
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
'inc/wrf_auxinput1out.inc ext_write_field AETA2 memorder Z' , & ! Debug message
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
'inc/wrf_auxinput1out.inc ext_write_field ETA2 memorder Z' , & ! Debug message
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
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
'inc/wrf_auxinput1out.inc ext_write_field PDTOP memorder 0' , & ! Debug message
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
'inc/wrf_auxinput1out.inc ext_write_field PT memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
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
'inc/wrf_auxinput1out.inc ext_write_field Z0 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
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
'inc/wrf_auxinput1out.inc ext_write_field ALBASE memorder XY' , & ! Debug message
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
'inc/wrf_auxinput1out.inc ext_write_field GLAT memorder XY' , & ! Debug message
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
'inc/wrf_auxinput1out.inc ext_write_field GLON memorder XY' , & ! Debug message
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
'inc/wrf_auxinput1out.inc ext_write_field TSK memorder XY' , & ! Debug message
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
'inc/wrf_auxinput1out.inc ext_write_field MXSNAL memorder XY' , & ! Debug message
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
'inc/wrf_auxinput1out.inc ext_write_field TGROUND memorder XY' , & ! Debug message
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
'inc/wrf_auxinput1out.inc ext_write_field DFRLG memorder Z' , & ! Debug message
kds , kde , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( kde, kpe ) , 1 , 1 , 1 , 1 ,  & 
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
'inc/wrf_auxinput1out.inc ext_write_field CWM memorder XYZ' , & ! Debug message
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
'inc/wrf_auxinput1out.inc ext_write_field F_ICE memorder XZY' , & ! Debug message
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
'inc/wrf_auxinput1out.inc ext_write_field F_RAIN memorder XZY' , & ! Debug message
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
'inc/wrf_auxinput1out.inc ext_write_field F_RIMEF memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
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
'inc/wrf_auxinput1out.inc ext_write_field ISLOPE memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
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
'inc/wrf_auxinput1out.inc ext_write_field RTDPTH memorder Z' , & ! Debug message
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
'inc/wrf_auxinput1out.inc ext_write_field SLDPTH memorder Z' , & ! Debug message
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
'inc/wrf_auxinput1out.inc ext_write_field CMC memorder XY' , & ! Debug message
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
'inc/wrf_auxinput1out.inc ext_write_field SOILTB memorder XY' , & ! Debug message
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
'inc/wrf_auxinput1out.inc ext_write_field VEGFRC memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SM000007'               , &  ! Data Name 
                       grid%sm000007               , &  ! Field 
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
                       'LAYER SOIL MOISTURE'               , &  ! Desc  
                       'm3 m-3'               , &  ! Units 
'inc/wrf_auxinput1out.inc ext_write_field SM000007 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SM007028'               , &  ! Data Name 
                       grid%sm007028               , &  ! Field 
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
                       'LAYER SOIL MOISTURE'               , &  ! Desc  
                       'm3 m-3'               , &  ! Units 
'inc/wrf_auxinput1out.inc ext_write_field SM007028 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SM028100'               , &  ! Data Name 
                       grid%sm028100               , &  ! Field 
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
                       'LAYER SOIL MOISTURE'               , &  ! Desc  
                       'm3 m-3'               , &  ! Units 
'inc/wrf_auxinput1out.inc ext_write_field SM028100 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SM100255'               , &  ! Data Name 
                       grid%sm100255               , &  ! Field 
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
                       'LAYER SOIL MOISTURE'               , &  ! Desc  
                       'm3 m-3'               , &  ! Units 
'inc/wrf_auxinput1out.inc ext_write_field SM100255 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ST000007'               , &  ! Data Name 
                       grid%st000007               , &  ! Field 
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
                       'LAYER SOIL TEMPERATURE'               , &  ! Desc  
                       'K'               , &  ! Units 
'inc/wrf_auxinput1out.inc ext_write_field ST000007 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ST007028'               , &  ! Data Name 
                       grid%st007028               , &  ! Field 
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
                       'LAYER SOIL TEMPERATURE'               , &  ! Desc  
                       'K'               , &  ! Units 
'inc/wrf_auxinput1out.inc ext_write_field ST007028 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ST028100'               , &  ! Data Name 
                       grid%st028100               , &  ! Field 
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
                       'LAYER SOIL TEMPERATURE'               , &  ! Desc  
                       'K'               , &  ! Units 
'inc/wrf_auxinput1out.inc ext_write_field ST028100 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ST100255'               , &  ! Data Name 
                       grid%st100255               , &  ! Field 
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
                       'LAYER SOIL TEMPERATURE'               , &  ! Desc  
                       'K'               , &  ! Units 
'inc/wrf_auxinput1out.inc ext_write_field ST100255 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SM000010'               , &  ! Data Name 
                       grid%sm000010               , &  ! Field 
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
'inc/wrf_auxinput1out.inc ext_write_field SM000010 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SM010040 '               , &  ! Data Name 
                       grid%sm010040               , &  ! Field 
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
'inc/wrf_auxinput1out.inc ext_write_field SM010040  memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SM040100 '               , &  ! Data Name 
                       grid%sm040100               , &  ! Field 
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
'inc/wrf_auxinput1out.inc ext_write_field SM040100  memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SM100200 '               , &  ! Data Name 
                       grid%sm100200               , &  ! Field 
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
'inc/wrf_auxinput1out.inc ext_write_field SM100200  memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SM010200'               , &  ! Data Name 
                       grid%sm010200               , &  ! Field 
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
'inc/wrf_auxinput1out.inc ext_write_field SM010200 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SOILM000'               , &  ! Data Name 
                       grid%soilm000               , &  ! Field 
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
'inc/wrf_auxinput1out.inc ext_write_field SOILM000 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SOILM005'               , &  ! Data Name 
                       grid%soilm005               , &  ! Field 
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
'inc/wrf_auxinput1out.inc ext_write_field SOILM005 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SOILM020'               , &  ! Data Name 
                       grid%soilm020               , &  ! Field 
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
'inc/wrf_auxinput1out.inc ext_write_field SOILM020 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SOILM040'               , &  ! Data Name 
                       grid%soilm040               , &  ! Field 
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
'inc/wrf_auxinput1out.inc ext_write_field SOILM040 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SOILM160'               , &  ! Data Name 
                       grid%soilm160               , &  ! Field 
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
'inc/wrf_auxinput1out.inc ext_write_field SOILM160 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SOILM300'               , &  ! Data Name 
                       grid%soilm300               , &  ! Field 
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
'inc/wrf_auxinput1out.inc ext_write_field SOILM300 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SW000010'               , &  ! Data Name 
                       grid%sw000010               , &  ! Field 
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
'inc/wrf_auxinput1out.inc ext_write_field SW000010 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SW010040'               , &  ! Data Name 
                       grid%sw010040               , &  ! Field 
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
'inc/wrf_auxinput1out.inc ext_write_field SW010040 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SW040100'               , &  ! Data Name 
                       grid%sw040100               , &  ! Field 
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
'inc/wrf_auxinput1out.inc ext_write_field SW040100 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SW100200'               , &  ! Data Name 
                       grid%sw100200               , &  ! Field 
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
'inc/wrf_auxinput1out.inc ext_write_field SW100200 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SW010200'               , &  ! Data Name 
                       grid%sw010200               , &  ! Field 
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
'inc/wrf_auxinput1out.inc ext_write_field SW010200 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SOILW000'               , &  ! Data Name 
                       grid%soilw000               , &  ! Field 
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
'inc/wrf_auxinput1out.inc ext_write_field SOILW000 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SOILW005'               , &  ! Data Name 
                       grid%soilw005               , &  ! Field 
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
'inc/wrf_auxinput1out.inc ext_write_field SOILW005 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SOILW020'               , &  ! Data Name 
                       grid%soilw020               , &  ! Field 
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
'inc/wrf_auxinput1out.inc ext_write_field SOILW020 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SOILW040'               , &  ! Data Name 
                       grid%soilw040               , &  ! Field 
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
'inc/wrf_auxinput1out.inc ext_write_field SOILW040 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SOILW160'               , &  ! Data Name 
                       grid%soilw160               , &  ! Field 
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
'inc/wrf_auxinput1out.inc ext_write_field SOILW160 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SOILW300'               , &  ! Data Name 
                       grid%soilw300               , &  ! Field 
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
'inc/wrf_auxinput1out.inc ext_write_field SOILW300 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ST000010'               , &  ! Data Name 
                       grid%st000010               , &  ! Field 
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
'inc/wrf_auxinput1out.inc ext_write_field ST000010 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ST010040'               , &  ! Data Name 
                       grid%st010040               , &  ! Field 
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
'inc/wrf_auxinput1out.inc ext_write_field ST010040 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ST040100'               , &  ! Data Name 
                       grid%st040100               , &  ! Field 
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
'inc/wrf_auxinput1out.inc ext_write_field ST040100 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ST100200'               , &  ! Data Name 
                       grid%st100200               , &  ! Field 
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
'inc/wrf_auxinput1out.inc ext_write_field ST100200 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ST010200'               , &  ! Data Name 
                       grid%st010200               , &  ! Field 
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
'inc/wrf_auxinput1out.inc ext_write_field ST010200 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SOILT000'               , &  ! Data Name 
                       grid%soilt000               , &  ! Field 
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
'inc/wrf_auxinput1out.inc ext_write_field SOILT000 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SOILT005'               , &  ! Data Name 
                       grid%soilt005               , &  ! Field 
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
'inc/wrf_auxinput1out.inc ext_write_field SOILT005 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SOILT020'               , &  ! Data Name 
                       grid%soilt020               , &  ! Field 
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
'inc/wrf_auxinput1out.inc ext_write_field SOILT020 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SOILT040'               , &  ! Data Name 
                       grid%soilt040               , &  ! Field 
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
'inc/wrf_auxinput1out.inc ext_write_field SOILT040 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SOILT160'               , &  ! Data Name 
                       grid%soilt160               , &  ! Field 
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
'inc/wrf_auxinput1out.inc ext_write_field SOILT160 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SOILT300'               , &  ! Data Name 
                       grid%soilt300               , &  ! Field 
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
'inc/wrf_auxinput1out.inc ext_write_field SOILT300 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
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
'inc/wrf_auxinput1out.inc ext_write_field LANDMASK memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'TOPOSTDV'               , &  ! Data Name 
                       grid%topostdv               , &  ! Field 
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
'inc/wrf_auxinput1out.inc ext_write_field TOPOSTDV memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'TOPOSLPX'               , &  ! Data Name 
                       grid%toposlpx               , &  ! Field 
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
'inc/wrf_auxinput1out.inc ext_write_field TOPOSLPX memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'TOPOSLPY'               , &  ! Data Name 
                       grid%toposlpy               , &  ! Field 
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
'inc/wrf_auxinput1out.inc ext_write_field TOPOSLPY memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'GREENMAX'               , &  ! Data Name 
                       grid%greenmax               , &  ! Field 
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
'inc/wrf_auxinput1out.inc ext_write_field GREENMAX memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'GREENMIN'               , &  ! Data Name 
                       grid%greenmin               , &  ! Field 
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
'inc/wrf_auxinput1out.inc ext_write_field GREENMIN memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ALBEDOMX'               , &  ! Data Name 
                       grid%albedomx               , &  ! Field 
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
'inc/wrf_auxinput1out.inc ext_write_field ALBEDOMX memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SLOPECAT'               , &  ! Data Name 
                       grid%slopecat               , &  ! Field 
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
'inc/wrf_auxinput1out.inc ext_write_field SLOPECAT memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SOILHGT'               , &  ! Data Name 
                       grid%toposoil               , &  ! Field 
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
'inc/wrf_auxinput1out.inc ext_write_field SOILHGT memorder XY' , & ! Debug message
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
'inc/wrf_auxinput1out.inc ext_write_field '//TRIM(moist_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
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
'inc/wrf_auxinput1out.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
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
'inc/wrf_auxinput1out.inc ext_write_field '//TRIM(chem_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                         ierr )
  ENDIF
ENDDO
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
'inc/wrf_auxinput1out.inc ext_write_field PSFC memorder XY' , & ! Debug message
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
'inc/wrf_auxinput1out.inc ext_write_field XICE memorder XY' , & ! Debug message
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
'inc/wrf_auxinput1out.inc ext_write_field VEGFRA memorder XY' , & ! Debug message
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
'inc/wrf_auxinput1out.inc ext_write_field ALBBCK memorder XY' , & ! Debug message
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
'inc/wrf_auxinput1out.inc ext_write_field SNOW memorder XY' , & ! Debug message
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
'inc/wrf_auxinput1out.inc ext_write_field CANWAT memorder XY' , & ! Debug message
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
'inc/wrf_auxinput1out.inc ext_write_field SST memorder XY' , & ! Debug message
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
'inc/wrf_auxinput1out.inc ext_write_field WEASD memorder XY' , & ! Debug message
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
'inc/wrf_auxinput1out.inc ext_write_field SNOWH memorder XY' , & ! Debug message
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
'inc/wrf_auxinput1out.inc ext_write_field RHOSN memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
!ENDOFREGISTRYGENERATEDINCLUDE

    RETURN
    END
