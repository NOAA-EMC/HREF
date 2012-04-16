


  SUBROUTINE input_wrf ( fid , grid , config_flags , switch , ierr )
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
    CHARACTER*256 errmess, currtimestr
    CHARACTER*40            :: this_datestr, next_datestr
    CHARACTER*9   NAMESTR
    INTEGER       IBDY, NAMELEN
    LOGICAL wrf_dm_on_monitor
    EXTERNAL wrf_dm_on_monitor
    Type(WRFU_Time) time, currtime
    CHARACTER*19  new_date
    CHARACTER*24  base_date
    CHARACTER*80  fname
    LOGICAL dryrun
    INTEGER idt
    INTEGER itmp
    INTEGER filestate, ierr3
    INTEGER :: ide_compare , jde_compare , kde_compare
    CHARACTER (len=19) simulation_start_date
    INTEGER simulation_start_year   , &
            simulation_start_month  , &
            simulation_start_day    , &
            simulation_start_hour   , &
            simulation_start_minute , &
            simulation_start_second
    LOGICAL reset_simulation_start
    REAL dx_compare , dy_compare , dum
    INTEGER :: num_land_cat_compare
    CHARACTER (LEN=256) :: MMINLU










    WRITE(wrf_err_message,*)'input_wrf: begin, fid = ',fid
    CALL wrf_debug( 300 , wrf_err_message )

    ierr = 0

    CALL get_ijk_from_grid (  grid ,                        &
                              ids, ide, jds, jde, kds, kde,    &
                              ims, ime, jms, jme, kms, kme,    &
                              ips, ipe, jps, jpe, kps, kpe    )


    CALL wrf_inquire_filename ( fid , fname , filestate , ierr )
    IF ( ierr /= 0 ) THEN
      WRITE(wrf_err_message,*)'module_io_wrf: input_wrf: wrf_inquire_filename Status = ',ierr
      CALL wrf_error_fatal3("",233,&
wrf_err_message )
    ENDIF

    WRITE(wrf_err_message,*)'input_wrf: fid,filestate = ',fid,filestate
    CALL wrf_debug( 300 , wrf_err_message )

    dryrun        = ( filestate .EQ. WRF_FILE_OPENED_NOT_COMMITTED )

    WRITE(wrf_err_message,*)'input_wrf: dryrun = ',dryrun
    CALL wrf_debug( 300 , wrf_err_message )

    check_if_dryrun : IF ( .NOT. dryrun ) THEN


    IF ( ( switch .EQ.     model_input_only  ) .OR. &
         ( switch .EQ.          restart_only ) ) THEN
      CALL wrf_get_dom_ti_char ( fid , 'SIMULATION_START_DATE' , simulation_start_date , ierr )
      CALL nl_get_reset_simulation_start ( 1, reset_simulation_start )
      IF ( ( ierr .EQ. 0 ) .AND. ( .NOT. reset_simulation_start ) ) THEN
        
        READ ( simulation_start_date , fmt = '(I4,1x,I2,1x,I2,1x,I2,1x,I2,1x,I2)' ) &
               simulation_start_year,   simulation_start_month,                     &
               simulation_start_day,    simulation_start_hour,                      &
               simulation_start_minute, simulation_start_second
        CALL nl_set_simulation_start_year   ( 1 , simulation_start_year   )
        CALL nl_set_simulation_start_month  ( 1 , simulation_start_month  )
        CALL nl_set_simulation_start_day    ( 1 , simulation_start_day    )
        CALL nl_set_simulation_start_hour   ( 1 , simulation_start_hour   )
        CALL nl_set_simulation_start_minute ( 1 , simulation_start_minute )
        CALL nl_set_simulation_start_second ( 1 , simulation_start_second )
        IF ( switch .EQ. model_input_only  ) THEN
          WRITE(wrf_err_message,*)fid,' input_wrf, model_input_only:  SIMULATION_START_DATE = ', &
                                  simulation_start_date(1:19)
          CALL wrf_debug ( 300 , TRIM(wrf_err_message ) )
        ELSE IF ( switch .EQ. restart_only  ) THEN
          WRITE(wrf_err_message,*)fid,' input_wrf, restart_only:  SIMULATION_START_DATE = ', &
                                  simulation_start_date(1:19)
          CALL wrf_debug ( 300 , TRIM(wrf_err_message ) )
        ENDIF
      ELSE
        CALL nl_get_start_year   ( 1 , simulation_start_year   )
        CALL nl_get_start_month  ( 1 , simulation_start_month  )
        CALL nl_get_start_day    ( 1 , simulation_start_day    )
        CALL nl_get_start_hour   ( 1 , simulation_start_hour   )
        CALL nl_get_start_minute ( 1 , simulation_start_minute )
        CALL nl_get_start_second ( 1 , simulation_start_second )
        CALL nl_set_simulation_start_year   ( 1 , simulation_start_year   )
        CALL nl_set_simulation_start_month  ( 1 , simulation_start_month  )
        CALL nl_set_simulation_start_day    ( 1 , simulation_start_day    )
        CALL nl_set_simulation_start_hour   ( 1 , simulation_start_hour   )
        CALL nl_set_simulation_start_minute ( 1 , simulation_start_minute )
        CALL nl_set_simulation_start_second ( 1 , simulation_start_second )
        IF ( reset_simulation_start ) THEN
          CALL wrf_message('input_wrf: forcing SIMULATION_START_DATE = head_grid start time')
          CALL wrf_message('           due to namelist variable reset_simulation_start')
        ELSE
          CALL wrf_message('input_wrf: SIMULATION_START_DATE not available in input')
          CALL wrf_message('will use head_grid start time from namelist')
        ENDIF
      ENDIF
      
      
      
      CALL domain_clock_get( grid, minutesSinceSimulationStart=grid%xtime )
      
      WRITE(wrf_err_message,*) 'input_wrf:  set xtime to ',grid%xtime
      CALL wrf_debug ( 100, TRIM(wrf_err_message) )
    ENDIF

    
    

    IF ( ( switch .EQ.     model_input_only  ) .OR. &
         ( switch .EQ. aux_model_input1_only ) ) THEN
       ierr = 0
       CALL wrf_get_dom_ti_integer ( fid , 'WEST-EAST_GRID_DIMENSION' ,    ide_compare , 1 , icnt , ierr3 )
       ierr = max( ierr, ierr3 )
       CALL wrf_get_dom_ti_integer ( fid , 'SOUTH-NORTH_GRID_DIMENSION' ,  jde_compare , 1 , icnt , ierr3 )
       ierr = max( ierr, ierr3 )
       CALL wrf_get_dom_ti_integer ( fid , 'BOTTOM-TOP_GRID_DIMENSION' ,   kde_compare , 1 , icnt , ierr3 )
       ierr = max( ierr, ierr3 )

       IF ( ierr3 .NE. 0 ) CALL wrf_debug( 'wrf_get_dom_ti_integer getting dimension information from dataset' )

    END IF

    

    
    

    IF ( switch .NE. boundary_only ) THEN
       CALL wrf_get_dom_ti_real ( fid , 'CEN_LAT' ,  config_flags%cen_lat , 1 , icnt , ierr )
       WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for CEN_LAT returns ',config_flags%cen_lat
       CALL wrf_debug ( 300 , wrf_err_message )
       CALL nl_set_cen_lat ( grid%id , config_flags%cen_lat )

       CALL wrf_get_dom_ti_real ( fid , 'CEN_LON' ,  config_flags%cen_lon , 1 , icnt , ierr )
       WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for CEN_LON returns ',config_flags%cen_lon
       CALL wrf_debug ( 300 , wrf_err_message )
       CALL nl_set_cen_lon ( grid%id , config_flags%cen_lon )
    ELSE
       CALL wrf_get_dom_ti_real ( fid , 'CEN_LAT' ,  dum , 1 , icnt , ierr )
       WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for CEN_LAT returns ',dum
       CALL wrf_debug ( 300 , wrf_err_message )

       CALL wrf_get_dom_ti_real ( fid , 'CEN_LON' ,  dum , 1 , icnt , ierr )
       WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for CEN_LON returns ',dum
       CALL wrf_debug ( 300 , wrf_err_message )
    END IF

    CALL wrf_get_dom_ti_real ( fid , 'TRUELAT1' ,  config_flags%truelat1 , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for TRUELAT1 returns ',config_flags%truelat1
    CALL wrf_debug ( 300 , wrf_err_message )
    CALL nl_set_truelat1 ( grid%id , config_flags%truelat1 )

    CALL wrf_get_dom_ti_real ( fid , 'TRUELAT2' ,  config_flags%truelat2 , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for TRUELAT2 returns ',config_flags%truelat2
    CALL wrf_debug ( 300 , wrf_err_message )
    CALL nl_set_truelat2 ( grid%id , config_flags%truelat2 )

    CALL wrf_get_dom_ti_real ( fid , 'MOAD_CEN_LAT' ,  config_flags%moad_cen_lat , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for MOAD_CEN_LAT returns ',config_flags%moad_cen_lat
    CALL wrf_debug ( 300 , wrf_err_message )
    CALL nl_set_moad_cen_lat ( grid%id , config_flags%moad_cen_lat )

    CALL wrf_get_dom_ti_real ( fid , 'STAND_LON' ,  config_flags%stand_lon , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for STAND_LON returns ',config_flags%stand_lon
    CALL wrf_debug ( 300 , wrf_err_message )
    CALL nl_set_stand_lon ( grid%id , config_flags%stand_lon )


    IF ( switch .NE. boundary_only ) THEN
      CALL wrf_get_dom_ti_real ( fid , 'GMT' ,  config_flags%gmt , 1 , icnt , ierr )
      WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for GMT returns ',config_flags%gmt
      CALL wrf_debug ( 300 , wrf_err_message )
      CALL nl_set_gmt ( grid%id , config_flags%gmt )

      CALL wrf_get_dom_ti_integer ( fid , 'JULYR' ,  config_flags%julyr , 1 , icnt , ierr )
      WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for JULYR returns ',config_flags%julyr
      CALL wrf_debug ( 300 , wrf_err_message )
      CALL nl_set_julyr ( grid%id , config_flags%julyr )

      CALL wrf_get_dom_ti_integer ( fid , 'JULDAY' ,  config_flags%julday , 1 , icnt , ierr )
      WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for JULDAY returns ',config_flags%julday
      CALL wrf_debug ( 300 , wrf_err_message )
      CALL nl_set_julday ( grid%id , config_flags%julday )
    ENDIF

    CALL wrf_get_dom_ti_integer ( fid , 'MAP_PROJ' ,  config_flags%map_proj , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for MAP_PROJ returns ',config_flags%map_proj
    CALL wrf_debug ( 300 , wrf_err_message )
    CALL nl_set_map_proj ( grid%id , config_flags%map_proj )
    grid%map_proj = config_flags%map_proj

    CALL wrf_get_dom_ti_char ( fid , 'MMINLU', mminlu , ierr )
    IF ( ierr .NE. 0 ) THEN
	write(0,*) 'hardwire MMINLU to USGS: '
        mminlu(1:4)="USGS"




    ELSE IF ( ( ( mminlu(1:1) .GE. "A" ) .AND. ( mminlu(1:1) .LE. "Z" ) ) .OR. &
              ( ( mminlu(1:1) .GE. "a" ) .AND. ( mminlu(1:1) .LE. "z" ) ) .OR. &
              ( ( mminlu(1:1) .GE. "0" ) .AND. ( mminlu(1:1) .LE. "9" ) ) ) THEN
       
    ELSE IF ( mminlu(1:1) .EQ. " " ) THEN
       mminlu = " "
    ELSE
       mminlu = " "
    END IF
    call wrf_debug( 1 , "mminlu = '" // TRIM(mminlu) // "'")
    if (index(mminlu, char(0)) > 0) mminlu(index(mminlu, char(0)):) = " "
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_char for MMINLU returns ' // TRIM(mminlu)
    CALL wrf_debug ( 300 , wrf_err_message )
    CALL nl_set_mminlu ( grid%id, mminlu )

    
    
    

    call wrf_get_dom_ti_integer(fid, "NUM_LAND_CAT", num_land_cat_compare, 1, icnt, ierr)
    if ( (ierr .NE. 0) .OR. ( num_land_cat_compare .LT. 1 ) .OR. ( num_land_cat_compare .GT. 1000 ) ) then
       call wrf_debug( 1 , "Must be old WPS data, assuming 24 levels for NUM_LAND_CAT")
       num_land_cat_compare = 24

    endif
    if ( config_flags%num_land_cat /= num_land_cat_compare ) then
       call wrf_message("----------------- ERROR -------------------")
       WRITE(wrf_err_message,'("namelist    : NUM_LAND_CAT = ",I10)') config_flags%num_land_cat
       call wrf_message(wrf_err_message)
       WRITE(wrf_err_message,'("input files : NUM_LAND_CAT = ",I10, " (from geogrid selections).")') num_land_cat_compare
       call wrf_message(wrf_err_message)
       call wrf_error_fatal3("",428,&
"Mismatch between namelist and wrf input files for dimension NUM_LAND_CAT")
    endif

    CALL wrf_get_dom_ti_integer ( fid , 'ISWATER' ,  config_flags%iswater , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for ISWATER returns ',config_flags%iswater
    CALL wrf_debug ( 300 , wrf_err_message )
    IF ( ierr .NE. 0 ) THEN
         IF (mminlu == 'UMD') THEN
              config_flags%iswater = 14
         ELSE
              config_flags%iswater = 16
         ENDIF
    ENDIF
    CALL nl_set_iswater ( grid%id , config_flags%iswater )

    CALL wrf_get_dom_ti_integer ( fid , 'ISLAKE' ,  config_flags%islake , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for ISLAKE returns ',config_flags%islake
    CALL wrf_debug ( 300 , wrf_err_message )
    IF ( ierr .NE. 0 ) THEN
         config_flags%islake = -1
    ENDIF
    CALL nl_set_islake ( grid%id , config_flags%islake )

    CALL wrf_get_dom_ti_integer ( fid , 'ISICE' ,  config_flags%isice , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for ISICE returns ',config_flags%isice
    CALL wrf_debug ( 300 , wrf_err_message )
    IF ( ierr .NE.  0 ) THEN
         IF (mminlu == 'UMD') THEN
              config_flags%isice = 14
         ELSE
              config_flags%isice = 24
         ENDIF
    ENDIF
    CALL nl_set_isice ( grid%id , config_flags%isice )

    CALL wrf_get_dom_ti_integer ( fid , 'ISURBAN' ,  config_flags%isurban , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for ISURBAN returns ',config_flags%isurban
    CALL wrf_debug ( 300 , wrf_err_message )
    IF ( ierr .NE. 0 ) THEN
         IF (mminlu == 'UMD') THEN
              config_flags%isurban = 13
         ELSE
              config_flags%isurban = 1
         ENDIF
    ENDIF
    CALL nl_set_isurban ( grid%id , config_flags%isurban )

    CALL wrf_get_dom_ti_integer ( fid , 'ISOILWATER' ,  config_flags%isoilwater , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for ISOILWATER returns ',config_flags%isoilwater
    CALL wrf_debug ( 300 , wrf_err_message )
    IF ( ierr .NE. 0 ) THEN
         config_flags%isoilwater = 14
    ENDIF
    CALL nl_set_isoilwater ( grid%id , config_flags%isoilwater )




    IF      ( ( switch .EQ. aux_model_input1_only  ) .AND. &
              ( config_flags%auxinput1_inname(1:8) .EQ. 'wrf_real' ) ) THEN

       CALL wrf_get_dom_ti_integer ( fid , 'BOTTOM-TOP_GRID_DIMSNSION' ,   kde_compare , 1 , icnt , ierr3 )

       

       IF ( ( ide-1 .NE. ide_compare    ) .OR. &
            ( kde   .NE. kde_compare    ) .OR. &
            ( jde-1 .NE. jde_compare    ) .AND. ierr3 .EQ. 0 ) THEN
          WRITE(wrf_err_message,*)'input_wrf.F: SIZE MISMATCH:  namelist ide-1,jde-1,kde=',ide-1,jde-1,kde,&
                                  '; input data ide,jde,kde=',ide_compare , jde_compare , kde_compare
          CALL wrf_debug( 100, wrf_err_message )
       ENDIF

       ELSEIF ( switch .EQ. aux_model_input1_only ) THEN          
         IF ( ( ide-1                           .NE. ide_compare ) .OR. &
            ( config_flags%num_metgrid_levels .NE. kde_compare ) .OR. &
            ( jde-1                             .NE. jde_compare ) .AND. ierr3 .EQ. 0 ) THEN
                WRITE(wrf_err_message,*)'input_wrf.F: SIZE MISMATCH:  ',&
                 'namelist ide-1,jde-1,num_metgrid_levels=',ide-1,jde-1,config_flags%num_metgrid_levels,&
                 '; input data ide,jde,num_metgrid_levels=',ide_compare , jde_compare , kde_compare
                IF (ide-1 .eq. ide_compare .AND. jde-1 .EQ. jde_compare) THEN
                  CALL wrf_message(wrf_err_message)
                  CALL wrf_error_fatal3("",511,&
"appears that the vertical dimension is wrong - quitting" )
                ELSE
                  CALL wrf_message(wrf_err_message)
                  CALL wrf_error_fatal3("",515,&
"appears that I or J dimensions are wrong - quitting" )
                ENDIF
         ENDIF
       ENDIF


    ENDIF check_if_dryrun










    3003 continue

	write(0,*) 'calling wrf_get_next_time'
	write(0,*) 'fid: ', fid
	write(0,*) 'current_date(1:19) into wrf_get_next_time: ', current_date(1:19)

    CALL wrf_get_next_time(fid, current_date , ierr)
	write(0,*) 'past get_next_time with current_date: ', current_date(1:19)

    WRITE(wrf_err_message,*)fid,' input_wrf: wrf_get_next_time current_date: ',current_date(1:19),' Status = ',ierr
    CALL wrf_debug ( 300 , TRIM(wrf_err_message ) )
    IF ( ierr .NE. 0 .AND. ierr .NE. WRF_WARN_NOTSUPPORTED .AND. ierr .NE. WRF_WARN_DRYRUN_READ ) THEN
      CALL wrf_message ( TRIM(wrf_err_message ) )
      IF ( switch .EQ. boundary_only ) THEN
        WRITE(wrf_err_message,*) ' ... May have run out of valid boundary conditions in file ',TRIM(fname)
        CALL wrf_error_fatal3("",548,&
TRIM(wrf_err_message) )
      ELSE

        WRITE(wrf_err_message,*) '... Could not find matching time in input file ',TRIM(fname)
        CALL wrf_error_fatal3("",553,&
TRIM(wrf_err_message) )

      ENDIF
    ELSE IF ( ierr .NE. WRF_WARN_NOTSUPPORTED .AND. ierr .NE. WRF_WARN_DRYRUN_READ) THEN




      SELECT CASE ( switch )
        CASE ( model_input_only, aux_model_input1_only, aux_model_input2_only,       &
               aux_model_input3_only, aux_model_input4_only, aux_model_input5_only,  &
               aux_model_input9_only, aux_model_input10_only )
	write(0,*) 'call wrf_atotime from here'
            CALL wrf_atotime( current_date(1:19), time )
            CALL domain_clock_get( grid, current_time=currtime, &
                                         current_timestr=currtimestr )


            CALL domain_clockprint(150, grid, &
                   'DEBUG input_wrf():  get CurrTime from clock,')
            IF ( time .NE. currtime ) THEN
                WRITE( wrf_err_message , * )'Time in file: ',trim( current_date(1:19) )
                CALL wrf_message ( trim(wrf_err_message) )
                WRITE( wrf_err_message , * )'Time on domain: ',trim( currtimestr )
                CALL wrf_message ( trim(wrf_err_message) )
                CALL wrf_message( "**WARNING** Time in input file not equal to time on domain **WARNING**" )
                WRITE(wrf_err_message,*) "**WARNING** Trying next time in file ",TRIM(fname)," ..."
                CALL wrf_message( TRIM(wrf_err_message) )
                GOTO 3003
            ENDIF
        CASE DEFAULT
      END SELECT
    ENDIF






    IF ( switch .EQ. boundary_only ) THEN
	write(0,*) 'boundary_only'
	write(0,*) 'fid: ', fid
        CALL wrf_get_dom_td_char ( fid , 'THISBDYTIME' ,  current_date(1:19), this_datestr , ierr )
	write(0,*) 'ierr from wrf_get_dom_td_char: ', ierr
	write(0,*) 'THISBDYTIME...: ', this_datestr(1:19)
        CALL wrf_atotime( this_datestr(1:19), grid%this_bdy_time )
        CALL wrf_get_dom_td_char ( fid , 'NEXTBDYTIME' ,  current_date(1:19), next_datestr , ierr )
	write(0,*) 'NEXTBDYTIME...: ', next_datestr(1:19)
        CALL wrf_atotime( next_datestr(1:19), grid%next_bdy_time )
    ENDIF

    IF      ( switch .EQ. model_input_only ) THEN
      CALL wrf_inputin( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. history_only ) THEN
      CALL wrf_histin( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. aux_model_input1_only ) THEN
      CALL wrf_auxinput1in( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. aux_model_input2_only ) THEN
      CALL wrf_auxinput2in( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. aux_model_input3_only ) THEN
      CALL wrf_auxinput3in( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. aux_model_input4_only ) THEN
      CALL wrf_auxinput4in( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. aux_model_input5_only ) THEN
      CALL wrf_auxinput5in( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. aux_model_input6_only ) THEN
      CALL wrf_auxinput6in( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. aux_model_input7_only ) THEN
      CALL wrf_auxinput7in( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. aux_model_input8_only ) THEN
      CALL wrf_auxinput8in( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. aux_model_input9_only ) THEN
      CALL wrf_auxinput9in( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. aux_model_input10_only ) THEN
      CALL wrf_auxinput10in( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. aux_model_input11_only ) THEN
      CALL wrf_auxinput11in( fid , grid , config_flags , switch , ierr )


    ELSE IF ( switch .EQ. aux_hist1_only ) THEN
      CALL wrf_auxhist1in( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. aux_hist2_only ) THEN
      CALL wrf_auxhist2in( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. aux_hist3_only ) THEN
      CALL wrf_auxhist3in( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. aux_hist4_only ) THEN
      CALL wrf_auxhist4in( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. aux_hist5_only ) THEN
      CALL wrf_auxhist5in( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. aux_hist6_only ) THEN
      CALL wrf_auxhist6in( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. aux_hist7_only ) THEN
      CALL wrf_auxhist7in( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. aux_hist8_only ) THEN
      CALL wrf_auxhist8in( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. aux_hist9_only ) THEN
      CALL wrf_auxhist9in( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. aux_hist10_only ) THEN
      CALL wrf_auxhist10in( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. aux_hist11_only ) THEN
      CALL wrf_auxhist11in( fid , grid , config_flags , switch , ierr )

    ELSE IF ( switch .EQ. restart_only ) THEN
      CALL wrf_restartin( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. boundary_only ) THEN
	write(0,*) 'call wrf_bdyin'
      CALL wrf_bdyin( fid , grid , config_flags , switch , ierr )
	write(0,*) 'return wrf_bdyin'
    ENDIF

    CALL wrf_tsin( grid , ierr )

    WRITE(wrf_err_message,*)'input_wrf: end, fid = ',fid
    CALL wrf_debug( 300 , wrf_err_message )

    RETURN
  END SUBROUTINE input_wrf
