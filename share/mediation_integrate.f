
!WRF:MEDIATION_LAYER:IO
!

SUBROUTINE med_calc_model_time ( grid , config_flags )
  ! Driver layer
   USE module_domain
  ! Model layer
   USE module_configure
   USE module_date_time

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

  ! Local data
   REAL                                       :: time 

! this is now handled by with calls to ESMF time manager
!   time = head_grid%dt * head_grid%total_time_steps
!   CALL calc_current_date (grid%id, time)


END SUBROUTINE med_calc_model_time

SUBROUTINE med_before_solve_io ( grid , config_flags )
  ! Driver layer
   USE module_domain
  ! Model layer
   USE module_configure
   USE esmf_mod

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
  ! Local
   INTEGER                                    :: rc

   CALL med_latbound_in ( grid , config_flags )

   IF( ESMF_AlarmIsRinging( grid%alarms( HISTORY_ALARM ), rc=rc ) ) THEN
     CALL med_history_out ( grid , config_flags )
     CALL wrf_debug(1, "med_before_solve_io: med_filter_out disabled")
     CALL ESMF_AlarmTurnOff( grid%alarms( HISTORY_ALARM ), rc=rc )
   ENDIF

   IF( ESMF_AlarmIsRinging( grid%alarms( AUXHIST1_ALARM ), rc=rc ) ) THEN
     CALL med_auxhist1_out ( grid , config_flags )
     CALL ESMF_AlarmTurnOff( grid%alarms( AUXHIST1_ALARM ), rc=rc )
   ENDIF
   IF( ESMF_AlarmIsRinging( grid%alarms( AUXHIST2_ALARM ), rc=rc ) ) THEN
     CALL med_auxhist2_out ( grid , config_flags )
     CALL ESMF_AlarmTurnOff( grid%alarms( AUXHIST2_ALARM ), rc=rc )
   ENDIF
   IF( ESMF_AlarmIsRinging( grid%alarms( AUXHIST3_ALARM ), rc=rc ) ) THEN
     CALL med_auxhist3_out ( grid , config_flags )
     CALL ESMF_AlarmTurnOff( grid%alarms( AUXHIST3_ALARM ), rc=rc )
   ENDIF
   IF( ESMF_AlarmIsRinging( grid%alarms( AUXHIST4_ALARM ), rc=rc ) ) THEN
     CALL med_auxhist4_out ( grid , config_flags )
     CALL ESMF_AlarmTurnOff( grid%alarms( AUXHIST4_ALARM ), rc=rc )
   ENDIF
   IF( ESMF_AlarmIsRinging( grid%alarms( AUXHIST5_ALARM ), rc=rc ) ) THEN
     CALL med_auxhist5_out ( grid , config_flags )
     CALL ESMF_AlarmTurnOff( grid%alarms( AUXHIST5_ALARM ), rc=rc )
   ENDIF

! - RESTART OUTPUT
   IF( ESMF_AlarmIsRinging( grid%alarms( RESTART_ALARM ), rc=rc ) ) THEN
	write(0,*) 'wanted to output restart file, but wouldnt let it'
!	write(0,*) calling med_restart_out with DFL : , grid%nmm_dfl
!     CALL med_restart_out ( grid , config_flags )
     CALL ESMF_AlarmTurnOff( grid%alarms( RESTART_ALARM ), rc=rc )
   ENDIF

   RETURN
END SUBROUTINE med_before_solve_io

SUBROUTINE med_after_solve_io ( grid , config_flags )
  ! Driver layer
   USE module_domain
   USE module_timing
  ! Model layer
   USE module_configure

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

   RETURN
END SUBROUTINE med_after_solve_io

SUBROUTINE med_nest_initial ( parent , nest , config_flags )
  ! Driver layer
   USE module_domain
   USE module_timing
  ! Model layer
   USE module_configure

   IMPLICIT NONE

  ! Arguments
   TYPE(domain) , POINTER                     :: parent, nest
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

   RETURN
END SUBROUTINE med_nest_initial


SUBROUTINE med_nest_force ( parent , nest , config_flags )
  ! Driver layer
   USE module_domain
   USE module_timing
  ! Model layer
   USE module_configure

   IMPLICIT NONE

  ! Arguments
   TYPE(domain) , POINTER                     :: parent, nest
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
  ! Local
   INTEGER                                    :: idum1 , idum2 , fid

   INTERFACE
     SUBROUTINE med_force_domain ( parent , nest )
        USE module_domain
        TYPE(domain) , POINTER                 :: parent , nest
     END SUBROUTINE med_force_domain
   END INTERFACE

! force nest with interpolated data from the parent
    CALL med_force_domain( parent, nest )

! might also have calls here to do input from a file into the nest

   RETURN
END SUBROUTINE med_nest_force

SUBROUTINE med_nest_feedback ( parent , nest , config_flags )
  ! Driver layer
   USE module_domain
   USE module_timing
  ! Model layer
   USE module_configure

   IMPLICIT NONE

  ! Arguments
   TYPE(domain) , POINTER                     :: parent, nest
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
  ! Local
   INTEGER                                    :: idum1 , idum2 , fid

   INTERFACE
     SUBROUTINE med_feedback_domain ( parent , nest )
        USE module_domain
        TYPE(domain) , POINTER                 :: parent , nest
     END SUBROUTINE med_feedback_domain
   END INTERFACE

! feedback nest with interpolated data from the parent
    CALL med_feedback_domain( parent, nest )

   RETURN
END SUBROUTINE med_nest_feedback

SUBROUTINE med_last_solve_io ( grid , config_flags )
  ! Driver layer
   USE module_domain
  ! Model layer
   USE module_configure

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
  ! Local
   INTEGER                                    :: rc

   IF( ESMF_AlarmIsRinging( grid%alarms( HISTORY_ALARM ), rc=rc ) ) THEN
     CALL med_history_out ( grid , config_flags )
     CALL wrf_debug(1, "med_before_solve_io: med_filter_out disabled")
   ENDIF

   IF( ESMF_AlarmIsRinging( grid%alarms( AUXHIST1_ALARM ), rc=rc ) ) THEN
     CALL med_auxhist1_out ( grid , config_flags )
   ENDIF
   IF( ESMF_AlarmIsRinging( grid%alarms( AUXHIST2_ALARM ), rc=rc ) ) THEN
     CALL med_auxhist2_out ( grid , config_flags )
   ENDIF
   IF( ESMF_AlarmIsRinging( grid%alarms( AUXHIST3_ALARM ), rc=rc ) ) THEN
     CALL med_auxhist3_out ( grid , config_flags )
   ENDIF
   IF( ESMF_AlarmIsRinging( grid%alarms( AUXHIST4_ALARM ), rc=rc ) ) THEN
     CALL med_auxhist4_out ( grid , config_flags )
   ENDIF
   IF( ESMF_AlarmIsRinging( grid%alarms( AUXHIST5_ALARM ), rc=rc ) ) THEN
     CALL med_auxhist5_out ( grid , config_flags )
   ENDIF

! - RESTART OUTPUT
   IF( ESMF_AlarmIsRinging( grid%alarms( RESTART_ALARM ), rc=rc ) ) THEN
	write(0,*) '2nd try for med_restart...no way'
!     CALL med_restart_out ( grid , config_flags )
   ENDIF

   RETURN
END SUBROUTINE med_last_solve_io

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE med_restart_out ( grid , config_flags )
  ! Driver layer
   USE module_domain
   USE module_io_domain
  ! Model layer
   USE module_configure
   USE module_bc_time_utilities
   USE ESMF_Mod

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

  ! Local
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   CHARACTER*80                           :: rstname , outname
   INTEGER                                :: fid , rid
   CHARACTER (LEN=256)                    :: message
   INTEGER                                :: ierr
   INTEGER                                :: myproc
   TYPE(ESMF_Time)                        :: CurrTime
   CHARACTER*80                           :: timestr

   IF ( wrf_dm_on_monitor() ) THEN
     CALL start_timing
   END IF

   CALL construct_filename1 ( rstname , 'wrfrst' , grid%id , 2 )
   CALL ESMF_ClockGetCurrTime( grid%domain_clock, CurrTime=CurrTime, rc=ierr )
   CALL ESMF_TimeGetString( CurrTime, timestr, rc=ierr )
   rstname = TRIM(rstname) // "_" // TRIM(timestr)
   WRITE( message , '("med_restart_out: opening ",A," for writing")' ) TRIM ( rstname )
   CALL wrf_debug( 0 , message )
   grid%write_metadata = .false.
   CALL open_w_dataset ( rid, TRIM(rstname), grid , &
                         config_flags , output_restart , "DATASET=RESTART", ierr )

   IF ( ierr .NE. 0 ) THEN
     CALL WRF_message( message )
   ENDIF
   grid%write_metadata = .true.
   CALL output_restart ( rid, grid , config_flags , ierr )
   IF ( wrf_dm_on_monitor() ) THEN
     WRITE ( message , FMT = '("Writing restart for domain ",I8)' ) grid%id
     CALL end_timing ( TRIM(message) )
   END IF
   CALL close_dataset ( rid , config_flags , "DATASET=RESTART" )
   RETURN
END SUBROUTINE med_restart_out


SUBROUTINE med_history_out ( grid , config_flags )
  ! Driver layer
   USE module_domain
   USE module_io_domain
  ! Model layer
   USE module_configure
   USE module_bc_time_utilities
   USE ESMF_Mod

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

  ! Local
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   CHARACTER*80                           :: rstname , outname
   INTEGER                                :: fid , rid
   CHARACTER (LEN=256)                    :: message
   INTEGER                                :: ierr
   INTEGER                                :: myproc
   TYPE(ESMF_Time)                        :: CurrTime
   CHARACTER*80                           :: timestr
   REAL*8                                 :: timef
   REAL                                   :: btim


   IF ( wrf_dm_on_monitor() ) THEN
     CALL start_timing
   END IF

	btim=timef()

   IF ( grid%oid .eq. 0 ) THEN
     CALL construct_filename1 ( outname , 'wrfout' , grid%id , 2 )
     CALL ESMF_ClockGetCurrTime( grid%domain_clock, CurrTime=CurrTime, rc=ierr )
     CALL ESMF_TimeGetString( CurrTime, timestr, rc=ierr )
     outname = TRIM(outname) // "_" // TRIM(timestr)

     WRITE ( message , '("med_history_out 1: opening ",A," for writing. ",I3)') TRIM ( outname ), ierr
     CALL wrf_debug( 0, message )

     grid%write_metadata = .false.
     CALL open_w_dataset ( grid%oid, TRIM(outname), grid ,  &
                           config_flags , output_history , "DATASET=HISTORY", ierr )
     IF ( ierr .NE. 0 ) THEN
       CALL wrf_message( message )
     ENDIF
     grid%write_metadata = .true.
   ELSE
     grid%write_metadata = .false.
   END IF

   CALL output_history ( grid%oid, grid , config_flags , ierr )

   grid%nframes = grid%nframes + 1
   IF ( grid%nframes >= config_flags%frames_per_outfile ) THEN
     CALL close_dataset ( grid%oid , config_flags , "DATASET=HISTORY" )
     grid%nframes = 0
     grid%oid = 0
   ENDIF
   IF ( wrf_dm_on_monitor() ) THEN
     WRITE ( message , FMT = '("Writing output for domain ",I8)' ) grid%id
     CALL end_timing ( TRIM(message) )
   END IF
	write(0,*) 'time in med_history_out: ', (timef()-btim)*1.e-3
   RETURN
END SUBROUTINE med_history_out

SUBROUTINE med_auxhist1_out ( grid , config_flags )
   USE module_domain
   USE module_configure
   IMPLICIT NONE
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   CALL med_auxhist_out( grid , 1 , config_flags )
   RETURN
END SUBROUTINE med_auxhist1_out

SUBROUTINE med_auxhist2_out ( grid , config_flags )
   USE module_domain
   USE module_configure
   IMPLICIT NONE
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   CALL med_auxhist_out( grid , 2 , config_flags )
   RETURN
END SUBROUTINE med_auxhist2_out

SUBROUTINE med_auxhist3_out ( grid , config_flags )
   USE module_domain
   USE module_configure
   IMPLICIT NONE
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   CALL med_auxhist_out( grid , 3 , config_flags )
   RETURN
END SUBROUTINE med_auxhist3_out

SUBROUTINE med_auxhist4_out ( grid , config_flags )
   USE module_domain
   USE module_configure
   IMPLICIT NONE
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   CALL med_auxhist_out( grid , 4 , config_flags )
   RETURN
END SUBROUTINE med_auxhist4_out

SUBROUTINE med_auxhist5_out ( grid , config_flags )
   USE module_domain
   USE module_configure
   IMPLICIT NONE
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   CALL med_auxhist_out( grid , 5 , config_flags )
   RETURN
END SUBROUTINE med_auxhist5_out

SUBROUTINE med_auxhist_out ( grid , stream, config_flags )
  ! Driver layer
   USE module_domain
   USE module_io_domain
  ! Model layer
   USE module_configure
   USE module_bc_time_utilities
   USE ESMF_Mod

   IMPLICIT NONE
  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   INTEGER , INTENT(IN)                       :: stream
  ! Local
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   CHARACTER*80                           :: rstname , outname, auxname, n1, n2
   INTEGER                                :: fid , rid
   CHARACTER (LEN=256)                    :: message
   INTEGER                                :: ierr
   INTEGER                                :: myproc
   TYPE(ESMF_Time)                        :: CurrTime
   CHARACTER*80                           :: timestr

   IF ( stream .LT. 1 .OR. stream .GT. 5 ) THEN
     WRITE(message,*)'med_auxhist_out: invalid history stream ',stream
     CALL wrf_error_fatal( message )
   ENDIF
   IF ( grid%auxhist1_oid .eq. 0 ) THEN
     WRITE(n1,'("auxhist",I1)')stream
     WRITE(n2,'("DATASET=AUXHIST",I1)')stream
     CALL construct_filename1 ( auxname , n1 , grid%id , 2 )
     CALL ESMF_ClockGetCurrTime( grid%domain_clock, CurrTime=CurrTime, rc=ierr )
     CALL ESMF_TimeGetString( CurrTime, timestr, rc=ierr )
     auxname = TRIM(auxname) // "_" // TRIM(timestr)
     WRITE ( message , '("med_auxhist_out : opening ",A," for writing. ",I3)') TRIM ( auxname ), ierr
     CALL wrf_debug( 0, message )
     grid%write_metadata = .false.
     CALL open_w_dataset ( grid%auxhist1_oid, TRIM(auxname), grid ,  &
                           config_flags , output_aux_hist1 , n2, ierr )
     IF ( ierr .NE. 0 ) THEN
       CALL wrf_message( message )
     ENDIF
     grid%write_metadata = .true.
   ELSE
     grid%write_metadata = .false.
   END IF
   SELECT CASE( stream )
     CASE ( 1 )
       CALL output_aux_hist1 ( grid%auxhist1_oid, grid , config_flags , ierr )
     CASE ( 2 )
       CALL output_aux_hist2 ( grid%auxhist2_oid, grid , config_flags , ierr )
     CASE ( 3 )
       CALL output_aux_hist2 ( grid%auxhist3_oid, grid , config_flags , ierr )
     CASE ( 4 )
       CALL output_aux_hist2 ( grid%auxhist4_oid, grid , config_flags , ierr )
     CASE ( 5 )
       CALL output_aux_hist2 ( grid%auxhist5_oid, grid , config_flags , ierr )
   END SELECT
   RETURN
END SUBROUTINE med_auxhist_out

SUBROUTINE med_filter_out ( grid , config_flags )
  ! Driver layer
   USE module_domain
   USE module_io_domain
  ! Model layer
   USE module_configure
   USE module_bc_time_utilities

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

  ! Local
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   CHARACTER*80                           :: rstname , outname
   INTEGER                                :: fid , rid
   CHARACTER (LEN=256)                    :: message
   INTEGER                                :: ierr
   INTEGER                                :: myproc

   CALL wrf_error_fatal( "CALL TO OBSOLETE med_filter_out" )
   RETURN
END SUBROUTINE med_filter_out

SUBROUTINE med_latbound_in ( grid , config_flags )
  ! Driver layer
   USE module_domain
   USE module_io_domain
  ! Model layer
   USE module_configure
   USE module_bc_time_utilities
   USE esmf_mod

   IMPLICIT NONE

  
!WRF Error and Warning messages (1-999)
!All i/o package-specific status codes you may want to add must be handled by your package (see below)
! WRF handles these and netCDF messages only
  integer, parameter  :: WRF_NO_ERR                  =  0       !no error
  integer, parameter  :: WRF_WARN_FILE_NF            = -1	!file not found, or incomplete
  integer, parameter  :: WRF_WARN_MD_NF              = -2	!metadata not found
  integer, parameter  :: WRF_WARN_TIME_NF            = -3	!timestamp not found
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4	!no more timestamps
  integer, parameter  :: WRF_WARN_VAR_NF             = -5	!variable not found
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6	!no more variables for the current time
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7	!too many open files
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8	!data type mismatch
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9	!attempt to write readonly file
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10	!attempt to read writeonly file
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11	!attempt to access unopened file
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12	!attempt to do 2 trainings for 1 variable
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13	!attempt to read past EOF
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14	!bad data handle
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15	!write length not equal to training length
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16	!more dimensions requested than training
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17	!attempt to read more data than exists
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18	!input dimension inconsistent
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19	!input MemoryOrder not recognized
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20	!a dimension name with 2 different lengths
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21	!string longer than provided storage
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22	!function not supportable
  integer, parameter  :: WRF_WARN_NOOP	             = -23   	!package implements this routine as NOOP

!Fatal errors 
  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100	!allocation error
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101	!dealloc error
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102	!bad file status


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

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

  ! Local data
   LOGICAL, EXTERNAL                      :: wrf_dm_on_monitor
   LOGICAL                                :: lbc_opened
   INTEGER                                :: idum1 , idum2 , ierr , open_status , fid, rc
   REAL                                   :: bfrq
   CHARACTER (LEN=256)                    :: message
   CHARACTER (LEN=80)                     :: bdyname
   Type (ESMF_Time )                      :: time, btime
   Type (ESMF_Time )                      :: current_time

integer xid 

  integer, parameter  :: WRF_FILE_NOT_OPENED                  = 100
  integer, parameter  :: WRF_FILE_OPENED_NOT_COMMITTED        = 101
  integer, parameter  :: WRF_FILE_OPENED_AND_COMMITTED        = 102
  integer, parameter  :: WRF_FILE_OPENED_FOR_READ             = 103
  integer, parameter  :: WRF_REAL                             = 104
  integer, parameter  :: WRF_DOUBLE               = 105
  integer, parameter  :: WRF_INTEGER                          = 106
  integer, parameter  :: WRF_LOGICAL                          = 107
  integer, parameter  :: WRF_COMPLEX                          = 108
  integer, parameter  :: WRF_DOUBLE_COMPLEX                   = 109
  integer, parameter  :: WRF_FILE_OPENED_FOR_UPDATE           = 110

   IF ( grid%id .EQ. 1 .AND. config_flags%specified ) THEN

     IF ( ( lbc_read_time( grid%current_time ) ) .AND. &
          ( grid%current_time + grid%step_time .GE. grid%stop_time ) ) THEN
       CALL wrf_debug( 100 , 'med_latbound_in: Skipping attempt to read lateral boundary file during last time step ' )

     ELSE IF ( ESMF_AlarmIsRinging( grid%alarms( BOUNDARY_ALARM ), rc=rc ) ) THEN
       CALL ESMF_AlarmTurnOff( grid%alarms( BOUNDARY_ALARM ), rc=rc )
       IF ( wrf_dm_on_monitor() ) CALL start_timing

       CALL construct_filename1 ( bdyname , 'wrfbdy' , grid%id , 2 )
       CALL wrf_inquire_opened(head_grid%lbc_fid , TRIM(bdyname) , open_status , ierr ) 
write(0,*) 'here I am...open_status: ', open_status
write(0,*) 'WRF_FILE_OPENED_FOR_READ: ', WRF_FILE_OPENED_FOR_READ
	write(0,*) 'head_grid%lbc_fid: ', head_grid%lbc_fid
       IF ( open_status .EQ. WRF_FILE_OPENED_FOR_READ ) THEN
         lbc_opened = .TRUE.
       ELSE
         lbc_opened = .FALSE.
       ENDIF
	write(0,*) 'lbc_opened: ', lbc_opened
       CALL wrf_dm_bcast_bytes ( lbc_opened , 4 )
       IF ( .NOT. lbc_opened ) THEN
         CALL construct_filename1 ( bdyname , 'wrfbdy' , grid%id , 2 )
	write(0,*) 'call open_r_dataset'
         CALL open_r_dataset ( head_grid%lbc_fid, TRIM(bdyname) , grid , config_flags , "DATASET=BOUNDARY", ierr )
	write(0,*) 'return with head_grid%lbc_fid: ',  head_grid%lbc_fid
	write(0,*) 'grid%lbc_fid: ', grid%lbc_fid

          IF ( ierr .NE. 0 ) THEN
            WRITE( message, * ) 'med_latbound_in: error opening ',TRIM(bdyname), ' for reading. IERR = ',ierr
            CALL WRF_ERROR_FATAL( message )
          ENDIF
       ENDIF
       CALL wrf_debug( 100 , 'med_latbound_in: calling input_boundary ' )
	write(0,*) 'calling input_boundary from mediation_integrate(1)'
!	write(0,*) skipping input_boundary from mediation_integrate(1)
       CALL input_boundary ( grid%lbc_fid, grid , config_flags , ierr )
	write(0,*) 'return input_boundary(1)'

       CALL ESMF_ClockGetCurrTime( grid%domain_clock, current_time, rc=rc)
!
      write(0,*)' current_time%YR=',current_time%YR
      write(0,*)' current_time%MM=',current_time%MM
      write(0,*)' current_time%DD=',current_time%DD
      write(0,*)' current_time%basetime%S=',current_time%basetime%S
      write(0,*)' current_time%basetime%Sn=',current_time%basetime%Sn
      write(0,*)' current_time%basetime%Sd=',current_time%basetime%Sd
      write(0,*)' grid%next_bdy_time%YR=',grid%next_bdy_time%YR
      write(0,*)' grid%next_bdy_time%MM=',grid%next_bdy_time%MM
      write(0,*)' grid%next_bdy_time%DD=',grid%next_bdy_time%DD
      write(0,*)' grid%next_bdy_time%basetime%S=',grid%next_bdy_time%basetime%S
      write(0,*)' grid%next_bdy_time%basetime%Sn=',grid%next_bdy_time%basetime%Sn
      write(0,*)' grid%next_bdy_time%basetime%Sd=',grid%next_bdy_time%basetime%Sd
!
       DO WHILE (current_time .GE. grid%next_bdy_time )         ! next_bdy_time is set by input_boundary from bdy file
         CALL wrf_debug( 100 , 'med_latbound_in: calling input_boundary ' )
	write(0,*) 'calling input_boundary from mediation_integrate(2)'
!	write(0,*) skipping input_boundary from mediation_integrate(2)
!	write(0,*) current_time: , current_time
!	write(0,*) grid%next_bdy_time: , grid%next_bdy_time
         CALL input_boundary ( grid%lbc_fid, grid , config_flags , ierr )
	write(0,*) 'return input_boundary(2)'
       ENDDO
       CALL ESMF_AlarmSet( grid%alarms( BOUNDARY_ALARM ), RingTime=grid%next_bdy_time, rc=rc )

       IF ( ierr .NE. 0 .and. ierr .NE. WRF_WARN_NETCDF ) THEN
         WRITE( message, * ) 'med_latbound_in: error reading ',TRIM(bdyname), ' IERR = ',ierr
         CALL WRF_ERROR_FATAL( message )
       ENDIF
       IF ( grid%current_time .EQ. grid%this_bdy_time ) grid%dtbc = 0.
  
       IF ( wrf_dm_on_monitor() ) THEN
         WRITE ( message , FMT = '("processing lateral boundary for domain ",I8)' ) grid%id
         CALL end_timing ( TRIM(message) )
       ENDIF

     ENDIF
   ENDIF
   RETURN
END SUBROUTINE med_latbound_in

SUBROUTINE med_setup_step ( grid , config_flags )
  ! Driver layer
   USE module_domain
  ! Model layer
   USE module_configure

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
  ! Local
   INTEGER                                    :: idum1 , idum2

   CALL set_scalar_indices_from_config ( grid%id , idum1 , idum2 )

   RETURN

END SUBROUTINE med_setup_step

