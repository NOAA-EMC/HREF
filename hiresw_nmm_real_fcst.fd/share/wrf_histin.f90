SUBROUTINE wrf_histin ( fid , grid , config_flags , switch , ierr )
    USE module_domain
    USE module_state_description
    USE module_configure
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
    INTEGER, INTENT(IN) :: fid
    INTEGER, INTENT(IN) :: switch
    INTEGER, INTENT(INOUT) :: ierr

    ! Local data
    INTEGER ids , ide , jds , jde , kds , kde , &
            ims , ime , jms , jme , kms , kme , &
            ips , ipe , jps , jpe , kps , kpe

    INTEGER       itrace
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
    INTEGER itmp, dyn_opt
    INTEGER :: ide_compare , jde_compare , kde_compare
    ierr = 0

    CALL get_ijk_from_grid (  grid ,                        &
                              ids, ide, jds, jde, kds, kde,    &
                              ims, ime, jms, jme, kms, kme,    &
                              ips, ipe, jps, jpe, kps, kpe    )

    CALL nl_get_dyn_opt ( 1 , dyn_opt )

!STARTOFREGISTRYGENERATEDINCLUDE 'inc/wrf_histin.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
IF ( grid%dyn_opt .EQ. dyn_exp ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'TOYVAR'               , &  ! Data Name 
                       grid%exp_x_2               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field TOYVAR memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
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
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'HBM2'               , &  ! Data Name 
                       grid%nmm_hbm2               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field HBM2 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'HBM3'               , &  ! Data Name 
                       grid%nmm_hbm3               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field HBM3 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'VBM2'               , &  ! Data Name 
                       grid%nmm_vbm2               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field VBM2 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'VBM3'               , &  ! Data Name 
                       grid%nmm_vbm3               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field VBM3 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SM'               , &  ! Data Name 
                       grid%nmm_sm               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field SM memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SICE'               , &  ! Data Name 
                       grid%nmm_sice               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field SICE memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PD'               , &  ! Data Name 
                       grid%nmm_pd               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field PD memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'FIS'               , &  ! Data Name 
                       grid%nmm_fis               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field FIS memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RES'               , &  ! Data Name 
                       grid%nmm_res               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field RES memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T'               , &  ! Data Name 
                       grid%nmm_t               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XYZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field T memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Q'               , &  ! Data Name 
                       grid%nmm_q               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XYZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field Q memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'U'               , &  ! Data Name 
                       grid%nmm_u               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XYZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field U memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'V'               , &  ! Data Name 
                       grid%nmm_v               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XYZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field V memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'DX_NMM'               , &  ! Data Name 
                       grid%nmm_dx_nmm               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field DX_NMM memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ETA1'               , &  ! Data Name 
                       grid%nmm_eta1               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'Z'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field ETA1 memorder Z' , & ! Debug message
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ETA2'               , &  ! Data Name 
                       grid%nmm_eta2               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'Z'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field ETA2 memorder Z' , & ! Debug message
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PDTOP'               , &  ! Data Name 
                       grid%nmm_pdtop               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field PDTOP memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PT'               , &  ! Data Name 
                       grid%nmm_pt               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field PT memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PBLH'               , &  ! Data Name 
                       grid%nmm_pblh               , &  ! Field 
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
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'USTAR'               , &  ! Data Name 
                       grid%nmm_ustar               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field USTAR memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Z0'               , &  ! Data Name 
                       grid%nmm_z0               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field Z0 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'THS'               , &  ! Data Name 
                       grid%nmm_ths               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field THS memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'QS'               , &  ! Data Name 
                       grid%nmm_qsh               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field QS memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'TWBS'               , &  ! Data Name 
                       grid%nmm_twbs               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field TWBS memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'QWBS'               , &  ! Data Name 
                       grid%nmm_qwbs               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field QWBS memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PREC'               , &  ! Data Name 
                       grid%nmm_prec               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field PREC memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'APREC'               , &  ! Data Name 
                       grid%nmm_aprec               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field APREC memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ACPREC'               , &  ! Data Name 
                       grid%nmm_acprec               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field ACPREC memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CUPREC'               , &  ! Data Name 
                       grid%nmm_cuprec               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field CUPREC memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LSPA'               , &  ! Data Name 
                       grid%nmm_lspa               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field LSPA memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SNO'               , &  ! Data Name 
                       grid%nmm_sno               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field SNO memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SI'               , &  ! Data Name 
                       grid%nmm_si               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field SI memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CLDEFI'               , &  ! Data Name 
                       grid%nmm_cldefi               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field CLDEFI memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'TH10'               , &  ! Data Name 
                       grid%nmm_th10               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field TH10 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Q10'               , &  ! Data Name 
                       grid%nmm_q10               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field Q10 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PSHLTR'               , &  ! Data Name 
                       grid%nmm_pshltr               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field PSHLTR memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'TSHLTR'               , &  ! Data Name 
                       grid%nmm_tshltr               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field TSHLTR memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'QSHLTR'               , &  ! Data Name 
                       grid%nmm_qshltr               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field QSHLTR memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Q2'               , &  ! Data Name 
                       grid%nmm_q2               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XYZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field Q2 memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'AKHS_OUT'               , &  ! Data Name 
                       grid%nmm_akhs_out               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field AKHS_OUT memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'AKMS_OUT'               , &  ! Data Name 
                       grid%nmm_akms_out               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field AKMS_OUT memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ALBASE'               , &  ! Data Name 
                       grid%nmm_albase               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field ALBASE memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ALBEDO'               , &  ! Data Name 
                       grid%nmm_albedo               , &  ! Field 
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
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CNVBOT'               , &  ! Data Name 
                       grid%nmm_cnvbot               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field CNVBOT memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CNVTOP'               , &  ! Data Name 
                       grid%nmm_cnvtop               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field CNVTOP memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CZEN'               , &  ! Data Name 
                       grid%nmm_czen               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field CZEN memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CZMEAN'               , &  ! Data Name 
                       grid%nmm_czmean               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field CZMEAN memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'GLAT'               , &  ! Data Name 
                       grid%nmm_glat               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field GLAT memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'GLON'               , &  ! Data Name 
                       grid%nmm_glon               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field GLON memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MXSNAL'               , &  ! Data Name 
                       grid%nmm_mxsnal               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field MXSNAL memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RADOT'               , &  ! Data Name 
                       grid%nmm_radot               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field RADOT memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SIGT4'               , &  ! Data Name 
                       grid%nmm_sigt4               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field SIGT4 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'TGROUND'               , &  ! Data Name 
                       grid%nmm_tg               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field TGROUND memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CWM'               , &  ! Data Name 
                       grid%nmm_cwm               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XYZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field CWM memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'F_ICE'               , &  ! Data Name 
                       grid%nmm_f_ice               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field F_ICE memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'F_RAIN'               , &  ! Data Name 
                       grid%nmm_f_rain               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field F_RAIN memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'F_RIMEF'               , &  ! Data Name 
                       grid%nmm_f_rimef               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field F_RIMEF memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SR'               , &  ! Data Name 
                       grid%nmm_sr               , &  ! Field 
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
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CFRACH'               , &  ! Data Name 
                       grid%nmm_cfrach               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field CFRACH memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CFRACL'               , &  ! Data Name 
                       grid%nmm_cfracl               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field CFRACL memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CFRACM'               , &  ! Data Name 
                       grid%nmm_cfracm               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field CFRACM memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ISLOPE'               , &  ! Data Name 
                       grid%nmm_islope               , &  ! Field 
                       WRF_integer             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field ISLOPE memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SLDPTH'               , &  ! Data Name 
                       grid%nmm_sldpth               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'Z'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field SLDPTH memorder Z' , & ! Debug message
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CMC'               , &  ! Data Name 
                       grid%nmm_cmc               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field CMC memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'GRNFLX'               , &  ! Data Name 
                       grid%nmm_grnflx               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field GRNFLX memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PCTSNO'               , &  ! Data Name 
                       grid%nmm_pctsno               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field PCTSNO memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SOILTB'               , &  ! Data Name 
                       grid%nmm_soiltb               , &  ! Field 
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
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'VEGFRC'               , &  ! Data Name 
                       grid%nmm_vegfrc               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field VEGFRC memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SH2O'               , &  ! Data Name 
                       grid%nmm_sh2o               , &  ! Field 
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
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SMC'               , &  ! Data Name 
                       grid%nmm_smc               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field SMC memorder XZY' , & ! Debug message
ids , (ide-1) , 1 , config_flags%num_soil_layers , jds , (jde-1) ,  & 
ims , ime , 1 , config_flags%num_soil_layers , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , 1 , config_flags%num_soil_layers , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'STC'               , &  ! Data Name 
                       grid%nmm_stc               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field STC memorder XZY' , & ! Debug message
ids , (ide-1) , 1 , config_flags%num_soil_layers , jds , (jde-1) ,  & 
ims , ime , 1 , config_flags%num_soil_layers , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , 1 , config_flags%num_soil_layers , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PINT'               , &  ! Data Name 
                       grid%nmm_pint               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XYZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field PINT memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , kds , kde ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( kde, kpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'W'               , &  ! Data Name 
                       grid%nmm_w               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XYZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field W memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , kds , kde ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( kde, kpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ACFRCV'               , &  ! Data Name 
                       grid%nmm_acfrcv               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field ACFRCV memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ACFRST'               , &  ! Data Name 
                       grid%nmm_acfrst               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field ACFRST memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SSROFF'               , &  ! Data Name 
                       grid%nmm_ssroff               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field SSROFF memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'BGROFF'               , &  ! Data Name 
                       grid%nmm_bgroff               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field BGROFF memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RLWIN'               , &  ! Data Name 
                       grid%nmm_rlwin               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field RLWIN memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RLWTOA'               , &  ! Data Name 
                       grid%nmm_rlwtoa               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field RLWTOA memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ALWIN'               , &  ! Data Name 
                       grid%nmm_alwin               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field ALWIN memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ALWOUT'               , &  ! Data Name 
                       grid%nmm_alwout               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field ALWOUT memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ALWTOA'               , &  ! Data Name 
                       grid%nmm_alwtoa               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field ALWTOA memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RSWIN'               , &  ! Data Name 
                       grid%nmm_rswin               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field RSWIN memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RSWINC'               , &  ! Data Name 
                       grid%nmm_rswinc               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field RSWINC memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RSWOUT'               , &  ! Data Name 
                       grid%nmm_rswout               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field RSWOUT memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ASWIN'               , &  ! Data Name 
                       grid%nmm_aswin               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field ASWIN memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ASWOUT'               , &  ! Data Name 
                       grid%nmm_aswout               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field ASWOUT memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ASWTOA'               , &  ! Data Name 
                       grid%nmm_aswtoa               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field ASWTOA memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SFCSHX'               , &  ! Data Name 
                       grid%nmm_sfcshx               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field SFCSHX memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SFCLHX'               , &  ! Data Name 
                       grid%nmm_sfclhx               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field SFCLHX memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SUBSHX'               , &  ! Data Name 
                       grid%nmm_subshx               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field SUBSHX memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SNOPCX'               , &  ! Data Name 
                       grid%nmm_snopcx               , &  ! Field 
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
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SFCUVX'               , &  ! Data Name 
                       grid%nmm_sfcuvx               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field SFCUVX memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'POTEVP'               , &  ! Data Name 
                       grid%nmm_potevp               , &  ! Field 
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
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'POTFLX'               , &  ! Data Name 
                       grid%nmm_potflx               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field POTFLX memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'TLMIN'               , &  ! Data Name 
                       grid%nmm_tlmin               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field TLMIN memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'TLMAX'               , &  ! Data Name 
                       grid%nmm_tlmax               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field TLMAX memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'TCUCN'               , &  ! Data Name 
                       grid%nmm_tcucn               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XYZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field TCUCN memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'TRAIN'               , &  ! Data Name 
                       grid%nmm_train               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XYZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field TRAIN memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'NCFRCV'               , &  ! Data Name 
                       grid%nmm_ncfrcv               , &  ! Field 
                       WRF_integer             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field NCFRCV memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'NCFRST'               , &  ! Data Name 
                       grid%nmm_ncfrst               , &  ! Field 
                       WRF_integer             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field NCFRST memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'NPHS0'               , &  ! Data Name 
                       grid%nmm_nphs0               , &  ! Field 
                       WRF_integer             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field NPHS0 memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'NPREC'               , &  ! Data Name 
                       grid%nmm_nprec               , &  ! Field 
                       WRF_integer             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field NPREC memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'NCLOD'               , &  ! Data Name 
                       grid%nmm_nclod               , &  ! Field 
                       WRF_integer             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field NCLOD memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'NHEAT'               , &  ! Data Name 
                       grid%nmm_nheat               , &  ! Field 
                       WRF_integer             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field NHEAT memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'NRDLW'               , &  ! Data Name 
                       grid%nmm_nrdlw               , &  ! Field 
                       WRF_integer             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field NRDLW memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'NRDSW'               , &  ! Data Name 
                       grid%nmm_nrdsw               , &  ! Field 
                       WRF_integer             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field NRDSW memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'NSRFC'               , &  ! Data Name 
                       grid%nmm_nsrfc               , &  ! Field 
                       WRF_integer             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field NSRFC memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'AVRAIN'               , &  ! Data Name 
                       grid%nmm_avrain               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field AVRAIN memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'AVCNVC'               , &  ! Data Name 
                       grid%nmm_avcnvc               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field AVCNVC memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ACUTIM'               , &  ! Data Name 
                       grid%nmm_acutim               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field ACUTIM memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ARDLW'               , &  ! Data Name 
                       grid%nmm_ardlw               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field ARDLW memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ARDSW'               , &  ! Data Name 
                       grid%nmm_ardsw               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field ARDSW memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ASRFC'               , &  ! Data Name 
                       grid%nmm_asrfc               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field ASRFC memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'APHTIM'               , &  ! Data Name 
                       grid%nmm_aphtim               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field APHTIM memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
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
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
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
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
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
                       'SMSTAV'               , &  ! Data Name 
                       grid%smstav               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field SMSTAV memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SMSTOT'               , &  ! Data Name 
                       grid%smstot               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field SMSTOT memorder XY' , & ! Debug message
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
                       'SFCEVP'               , &  ! Data Name 
                       grid%sfcevp               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field SFCEVP memorder XY' , & ! Debug message
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
                       'SFCEXC '               , &  ! Data Name 
                       grid%sfcexc               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field SFCEXC  memorder XY' , & ! Debug message
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
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'WEASD'               , &  ! Data Name 
                       grid%weasd               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field WEASD memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'THZ0'               , &  ! Data Name 
                       grid%thz0               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field THZ0 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'QZ0'               , &  ! Data Name 
                       grid%qz0               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field QZ0 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'UZ0'               , &  ! Data Name 
                       grid%uz0               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field UZ0 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'VZ0'               , &  ! Data Name 
                       grid%vz0               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field VZ0 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'QSFC'               , &  ! Data Name 
                       grid%qsfc               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field QSFC memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'HTOP'               , &  ! Data Name 
                       grid%htop               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field HTOP memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'HBOT'               , &  ! Data Name 
                       grid%hbot               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field HBOT memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'HTOPD'               , &  ! Data Name 
                       grid%htopd               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field HTOPD memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'HBOTD'               , &  ! Data Name 
                       grid%hbotd               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field HBOTD memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'HTOPS'               , &  ! Data Name 
                       grid%htops               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field HTOPS memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'HBOTS'               , &  ! Data Name 
                       grid%hbots               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field HBOTS memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CUPPT'               , &  ! Data Name 
                       grid%cuppt               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field CUPPT memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CPRATE'               , &  ! Data Name 
                       grid%cprate               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field CPRATE memorder XY' , & ! Debug message
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
                       'SMFR3D'               , &  ! Data Name 
                       grid%smfr3d               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field SMFR3D memorder XZY' , & ! Debug message
ids , (ide-1) , 1 , config_flags%num_soil_layers , jds , (jde-1) ,  & 
ims , ime , 1 , config_flags%num_soil_layers , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , 1 , config_flags%num_soil_layers , jps , MIN( (jde-1), jpe ) ,  & 
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
!ENDOFREGISTRYGENERATEDINCLUDE

    RETURN
    END
