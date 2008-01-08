!
!WRF:MEDIATION_LAYER:IO
!

SUBROUTINE med_calc_model_time ( grid , config_flags )
  ! Driver layer
   USE module_domain
   USE module_configure
  ! Model layer
   USE module_date_time

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

  ! Local data
   REAL                                       :: time 

! this is now handled by with calls to time manager
!   time = head_grid%dt * head_grid%total_time_steps
!   CALL calc_current_date (grid%id, time)


END SUBROUTINE med_calc_model_time

SUBROUTINE med_before_solve_io ( grid , config_flags )
  ! Driver layer
   USE module_domain
   USE module_configure
  ! Model layer
   USE module_utility

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
  ! Local
   INTEGER                                    :: rc

 ! Note that when grid%return_after_training_io == .TRUE. this routine 
 ! will return after the training phase for all auxiliary I/O streams.  
 ! Nothing else will be done.  This ugly hack is only needed for ESMF 
 ! coupling.  grid%return_after_training_io == .FALSE. in all other cases.  
 IF ( .NOT. grid%return_after_training_io ) THEN
   IF( WRFU_AlarmIsRinging( grid%alarms( HISTORY_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 0, config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( HISTORY_ALARM ), rc=rc )
   ENDIF

   IF( WRFU_AlarmIsRinging( grid%alarms( INPUTOUT_ALARM ), rc=rc ) ) THEN
     CALL med_filter_out  ( grid , config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( INPUTOUT_ALARM ), rc=rc )
   ENDIF
 ENDIF

! - AUX HISTORY OUTPUT 
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST1_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 1, config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXHIST1_ALARM ), rc=rc )
   ENDIF
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST2_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 2, config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXHIST2_ALARM ), rc=rc )
   ENDIF
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST3_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 3,  config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXHIST3_ALARM ), rc=rc )
   ENDIF
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST4_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 4, config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXHIST4_ALARM ), rc=rc )
   ENDIF
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST5_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 5, config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXHIST5_ALARM ), rc=rc )
   ENDIF
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST6_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 6, config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXHIST6_ALARM ), rc=rc )
   ENDIF
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST7_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 7, config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXHIST7_ALARM ), rc=rc )
   ENDIF
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST8_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 8, config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXHIST8_ALARM ), rc=rc )
   ENDIF
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST9_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 9, config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXHIST9_ALARM ), rc=rc )
   ENDIF
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST10_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 10, config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXHIST10_ALARM ), rc=rc )
   ENDIF
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST11_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 12, config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXHIST11_ALARM ), rc=rc )
   ENDIF

! - AUX INPUT INPUT
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXINPUT1_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput1_in ( grid , config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXINPUT1_ALARM ), rc=rc )
   ENDIF
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXINPUT2_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput2_in ( grid , config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXINPUT2_ALARM ), rc=rc )
   ENDIF
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXINPUT3_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput3_in ( grid , config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXINPUT3_ALARM ), rc=rc )
   ENDIF
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXINPUT4_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput4_in ( grid , config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXINPUT4_ALARM ), rc=rc )
   ENDIF

! this needs to be looked at again so we can get rid of the special
! handling of AUXINPUT5 but for now...

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! add for wrf_chem emiss input
! - Get chemistry data
  IF( config_flags%chem_opt > 0 ) THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ELSE
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXINPUT5_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput5_in ( grid , config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXINPUT5_ALARM ), rc=rc )
   ENDIF
  ENDIF

   IF( WRFU_AlarmIsRinging( grid%alarms( AUXINPUT6_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput6_in ( grid , config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXINPUT6_ALARM ), rc=rc )
   ENDIF
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXINPUT7_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput7_in ( grid , config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXINPUT7_ALARM ), rc=rc )
   ENDIF
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXINPUT8_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput8_in ( grid , config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXINPUT8_ALARM ), rc=rc )
   ENDIF
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXINPUT9_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput9_in ( grid , config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXINPUT9_ALARM ), rc=rc )
   ENDIF
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXINPUT10_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput10_in ( grid , config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXINPUT10_ALARM ), rc=rc )
   ENDIF
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXINPUT11_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput11_in ( grid , config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXINPUT11_ALARM ), rc=rc )
   ENDIF

 IF ( .NOT. grid%return_after_training_io ) THEN
! - RESTART OUTPUT
   IF( WRFU_AlarmIsRinging( grid%alarms( RESTART_ALARM ), rc=rc ) ) THEN
     IF ( grid%id .EQ. 1 ) THEN
       ! Only the parent initiates the restart writing. Otherwise, different
       ! domains may be written out at different times and with different 
       ! time stamps in the file names.
       CALL med_restart_out ( grid , config_flags )
     ENDIF
     CALL WRFU_AlarmRingerOff( grid%alarms( RESTART_ALARM ), rc=rc )
   ENDIF

! - Look for boundary data after writing out history and restart files
   CALL med_latbound_in ( grid , config_flags )
 ELSE
   CALL wrf_debug ( 1 , 'DEBUG:  med_before_solve_io():  returned after training aux I/O' )
 ENDIF

   RETURN
END SUBROUTINE med_before_solve_io

SUBROUTINE med_after_solve_io ( grid , config_flags )
  ! Driver layer
   USE module_domain
   USE module_timing
   USE module_configure
  ! Model layer

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

   RETURN
END SUBROUTINE med_after_solve_io

SUBROUTINE med_pre_nest_initial ( parent , newid , config_flags )
  ! Driver layer
   USE module_domain
   USE module_timing
   USE module_io_domain
   USE module_configure
  ! Model layer

   IMPLICIT NONE

  ! Arguments
   TYPE(domain) , POINTER                      :: parent
   INTEGER, INTENT(IN)                         :: newid
   TYPE (grid_config_rec_type) , INTENT(INOUT) :: config_flags
   TYPE (grid_config_rec_type)                 :: nest_config_flags

  ! Local
   INTEGER                :: itmp, fid, ierr, icnt
   CHARACTER*256          :: rstname, message, timestr

   TYPE(WRFU_Time)        :: strt_time, cur_time


END SUBROUTINE med_pre_nest_initial


SUBROUTINE med_nest_initial ( parent , nest , config_flags )
  ! Driver layer
   USE module_domain
   USE module_timing
   USE module_io_domain
   USE module_configure
   USE module_utility
  ! Model layer

   IMPLICIT NONE

  ! Arguments
   TYPE(domain) , POINTER                     :: parent, nest
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   TYPE (grid_config_rec_type)                :: nest_config_flags


  RETURN
END SUBROUTINE med_nest_initial

SUBROUTINE init_domain_constants ( parent , nest )
   USE module_domain
   IMPLICIT NONE
   TYPE(domain) :: parent , nest
END SUBROUTINE init_domain_constants


SUBROUTINE med_nest_force ( parent , nest )
  ! Driver layer
   USE module_domain
   USE module_timing
   USE module_configure
  ! Model layer
  ! External
   USE module_utility

   IMPLICIT NONE

  ! Arguments
   TYPE(domain) , POINTER                     :: parent, nest
  ! Local
   INTEGER                                    :: idum1 , idum2 , fid, rc


   INTERFACE
     SUBROUTINE med_force_domain ( parent , nest )
        USE module_domain
        TYPE(domain) , POINTER                 :: parent , nest
     END SUBROUTINE med_force_domain
     SUBROUTINE med_interp_domain ( parent , nest )
        USE module_domain
        TYPE(domain) , POINTER                 :: parent , nest
     END SUBROUTINE med_interp_domain
   END INTERFACE


   IF ( .NOT. WRFU_ClockIsStopTime(nest%domain_clock ,rc=rc) ) THEN
! initialize nest with interpolated data from the parent
     nest%imask_nostag = 1
     nest%imask_xstag = 1
     nest%imask_ystag = 1
     nest%imask_xystag = 1
     CALL med_force_domain( parent, nest )
   ENDIF

! might also have calls here to do input from a file into the nest

   RETURN
END SUBROUTINE med_nest_force

SUBROUTINE med_nest_feedback ( parent , nest , config_flags )
  ! Driver layer
   USE module_domain
   USE module_timing
   USE module_configure
  ! Model layer
  ! External
   USE module_utility
   IMPLICIT NONE


  ! Arguments
   TYPE(domain) , POINTER                     :: parent, nest
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
  ! Local
   INTEGER                                    :: idum1 , idum2 , fid, rc
   INTEGER                         :: ids , ide , jds , jde , kds , kde , &
                                      ims , ime , jms , jme , kms , kme , &
                                      ips , ipe , jps , jpe , kps , kpe
   INTEGER i,j

   INTERFACE
     SUBROUTINE med_feedback_domain ( parent , nest )
        USE module_domain
        TYPE(domain) , POINTER                 :: parent , nest
     END SUBROUTINE med_feedback_domain
   END INTERFACE

! feedback nest to the parent
    IF ( .NOT. WRFU_ClockIsStopTime(nest%domain_clock ,rc=rc) .AND. &
         config_flags%feedback .NE. 0 ) THEN
      CALL med_feedback_domain( parent, nest )
    END IF

   RETURN
END SUBROUTINE med_nest_feedback

SUBROUTINE med_last_solve_io ( grid , config_flags )
  ! Driver layer
   USE module_domain
   USE module_configure
  ! Model layer

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
  ! Local
   INTEGER                                    :: rc

   IF( WRFU_AlarmIsRinging( grid%alarms( HISTORY_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 0 , config_flags )
   ENDIF

   IF( WRFU_AlarmIsRinging( grid%alarms( INPUTOUT_ALARM ), rc=rc ) ) THEN
     CALL med_filter_out  ( grid , config_flags )
   ENDIF

   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST1_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 1 , config_flags )
   ENDIF
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST2_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 2 , config_flags )
   ENDIF
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST3_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 3 , config_flags )
   ENDIF
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST4_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 4 , config_flags )
   ENDIF
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST5_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 5 , config_flags )
   ENDIF
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST6_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 6 , config_flags )
   ENDIF
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST7_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 7 , config_flags )
   ENDIF
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST8_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 8 , config_flags )
   ENDIF
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST9_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 9 , config_flags )
   ENDIF
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST10_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 10 , config_flags )
   ENDIF
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST11_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 11 , config_flags )
   ENDIF

! - RESTART OUTPUT
   IF( WRFU_AlarmIsRinging( grid%alarms( RESTART_ALARM ), rc=rc ) ) THEN
     IF ( grid%id .EQ. 1 ) THEN
       CALL med_restart_out ( grid , config_flags )
     ENDIF
   ENDIF

   RETURN
END SUBROUTINE med_last_solve_io

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

RECURSIVE SUBROUTINE med_restart_out ( grid , config_flags )
  ! Driver layer
   USE module_domain
   USE module_io_domain
   USE module_timing
   USE module_configure
  ! Model layer
   USE module_bc_time_utilities
   USE module_utility

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

  ! Local
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   CHARACTER*80                           :: rstname , outname
   INTEGER                                :: fid , rid, kid
   CHARACTER (LEN=256)                    :: message
   INTEGER                                :: ierr
   INTEGER                                :: myproc
   CHARACTER*80                           :: timestr
   TYPE (grid_config_rec_type)            :: kid_config_flags

   IF ( wrf_dm_on_monitor() ) THEN
     CALL start_timing
   END IF

   ! write out this domains restart file first

   CALL domain_clock_get( grid, current_timestr=timestr )
   CALL construct_filename2a ( rstname , config_flags%rst_outname , grid%id , 2 , timestr )

   WRITE( message , '("med_restart_out: opening ",A," for writing")' ) TRIM ( rstname )
   CALL wrf_debug( 1 , message )
   CALL open_w_dataset ( rid, TRIM(rstname), grid , &
                         config_flags , output_restart , "DATASET=RESTART", ierr )

   IF ( ierr .NE. 0 ) THEN
     CALL WRF_message( message )
   ENDIF
   CALL output_restart ( rid, grid , config_flags , ierr )
   IF ( wrf_dm_on_monitor() ) THEN
     WRITE ( message , FMT = '("Writing restart for domain ",I8)' ) grid%id
     CALL end_timing ( TRIM(message) )
   END IF
   CALL close_dataset ( rid , config_flags , "DATASET=RESTART" )

   ! call recursively for children, (if any)
   DO kid = 1, max_nests
      IF ( ASSOCIATED( grid%nests(kid)%ptr ) ) THEN
        CALL model_to_grid_config_rec ( grid%nests(kid)%ptr%id , model_config_rec , kid_config_flags )
        CALL med_restart_out ( grid%nests(kid)%ptr , kid_config_flags ) 
      ENDIF
   ENDDO

   RETURN
END SUBROUTINE med_restart_out

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE med_hist_out ( grid , stream, config_flags )
  ! Driver layer
   USE module_domain
   USE module_timing
   USE module_io_domain
   USE module_configure
   USE module_bc_time_utilities
   USE module_utility

   IMPLICIT NONE
  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   INTEGER , INTENT(IN)                       :: stream
  ! Local
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   CHARACTER*80                           :: fname, n1, n2
   INTEGER                                :: fid , rid
   CHARACTER (LEN=256)                    :: message
   INTEGER                                :: ierr
   INTEGER                                :: myproc
   CHARACTER*80                           :: timestr
   TYPE(WRFU_Time)                        :: ST,CT
   INTEGER                                :: n
   LOGICAL                                :: adjust

   IF ( wrf_dm_on_monitor() ) THEN
     CALL start_timing
   END IF

   IF ( stream .LT. 0 .OR. stream .GT. 11 ) THEN
     WRITE(message,*)'med_hist_out: invalid history stream ',stream
     CALL wrf_error_fatal3 ( "mediation_integrate.b" , 1144 ,  message )
   ENDIF
   CALL nl_get_adjust_output_times( grid%id, adjust )
   CALL domain_clock_get( grid, current_time=CT, start_time=ST, current_timestr=timestr )

   SELECT CASE( stream )
     CASE ( 0 )
       IF ( adjust ) CALL adjust_io_timestr( grid%io_intervals( HISTORY_ALARM ), CT, ST, timestr )
       CALL construct_filename2a ( fname , config_flags%history_outname , grid%id , 2 , timestr )
     CASE ( 1 )
       IF ( adjust ) CALL adjust_io_timestr( grid%io_intervals( AUXHIST1_ALARM ), CT, ST, timestr )
       CALL construct_filename2a ( fname , config_flags%auxhist1_outname , grid%id , 2 , timestr )
     CASE ( 2 )
       IF ( adjust ) CALL adjust_io_timestr( grid%io_intervals( AUXHIST2_ALARM ), CT, ST, timestr )
       CALL construct_filename2a ( fname , config_flags%auxhist2_outname , grid%id , 2 , timestr )
     CASE ( 3 )
       IF ( adjust ) CALL adjust_io_timestr( grid%io_intervals( AUXHIST3_ALARM ), CT, ST, timestr )
       CALL construct_filename2a ( fname , config_flags%auxhist3_outname , grid%id , 2 , timestr )
     CASE ( 4 )
       IF ( adjust ) CALL adjust_io_timestr( grid%io_intervals( AUXHIST4_ALARM ), CT, ST, timestr )
       CALL construct_filename2a ( fname , config_flags%auxhist4_outname , grid%id , 2 , timestr )
     CASE ( 5 )
       IF ( adjust ) CALL adjust_io_timestr( grid%io_intervals( AUXHIST5_ALARM ), CT, ST, timestr )
       CALL construct_filename2a ( fname , config_flags%auxhist5_outname , grid%id , 2 , timestr )
     CASE ( 6 )
       IF ( adjust ) CALL adjust_io_timestr( grid%io_intervals( AUXHIST6_ALARM ), CT, ST, timestr )
       CALL construct_filename2a ( fname , config_flags%auxhist6_outname , grid%id , 2 , timestr )
     CASE ( 7 )
       IF ( adjust ) CALL adjust_io_timestr( grid%io_intervals( AUXHIST7_ALARM ), CT, ST, timestr )
       CALL construct_filename2a ( fname , config_flags%auxhist7_outname , grid%id , 2 , timestr )
     CASE ( 8 )
       IF ( adjust ) CALL adjust_io_timestr( grid%io_intervals( AUXHIST8_ALARM ), CT, ST, timestr )
       CALL construct_filename2a ( fname , config_flags%auxhist8_outname , grid%id , 2 , timestr )
     CASE ( 9 )
       IF ( adjust ) CALL adjust_io_timestr( grid%io_intervals( AUXHIST9_ALARM ), CT, ST, timestr )
       CALL construct_filename2a ( fname , config_flags%auxhist9_outname , grid%id , 2 , timestr )
     CASE ( 10 )
       IF ( adjust ) CALL adjust_io_timestr( grid%io_intervals( AUXHIST10_ALARM ), CT, ST, timestr )
       CALL construct_filename2a ( fname , config_flags%auxhist10_outname , grid%id , 2 , timestr )
     CASE ( 11 )
       IF ( adjust ) CALL adjust_io_timestr( grid%io_intervals( AUXHIST11_ALARM ), CT, ST, timestr )
       CALL construct_filename2a ( fname , config_flags%auxhist11_outname , grid%id , 2 , timestr )
   END SELECT

   IF (      ( stream .eq. 0 .and. grid%oid          .eq. 0 )    &
        .or. ( stream .eq. 1 .and. grid%auxhist1_oid .eq. 0 )    &
        .or. ( stream .eq. 2 .and. grid%auxhist2_oid .eq. 0 )    &
        .or. ( stream .eq. 3 .and. grid%auxhist3_oid .eq. 0 )    &
        .or. ( stream .eq. 4 .and. grid%auxhist4_oid .eq. 0 )    &
        .or. ( stream .eq. 5 .and. grid%auxhist5_oid .eq. 0 )    &
        .or. ( stream .eq. 6 .and. grid%auxhist6_oid .eq. 0 )    &
        .or. ( stream .eq. 7 .and. grid%auxhist7_oid .eq. 0 )    &
        .or. ( stream .eq. 8 .and. grid%auxhist8_oid .eq. 0 )    &
        .or. ( stream .eq. 9 .and. grid%auxhist9_oid .eq. 0 )    &
        .or. ( stream .eq. 10 .and. grid%auxhist10_oid .eq. 0 )    &
        .or. ( stream .eq. 11 .and. grid%auxhist11_oid .eq. 0 )    &
      ) THEN

     IF ( stream .EQ. 10 ) THEN
       WRITE(n2,'("DATASET=AUXHIST10")')
     ELSE IF ( stream .EQ. 11 ) THEN
       WRITE(n2,'("DATASET=AUXHIST11")')
     ELSE
       WRITE(n2,'("DATASET=AUXHIST",I1)')stream  ! may be overwritten, below, if stream is 0
     ENDIF
     WRITE ( message , '("med_hist_out : opening ",A," for writing. ")') TRIM ( fname )
     CALL wrf_debug( 1, message )
     SELECT CASE( stream )
       CASE ( 0 )
         CALL open_w_dataset ( grid%oid, TRIM(fname), grid ,  &
                               config_flags , output_history , 'DATASET=HISTORY' , ierr )
       CASE ( 1 )
         CALL open_w_dataset ( grid%auxhist1_oid, TRIM(fname), grid ,  &
                               config_flags , output_aux_hist1 , n2, ierr )
       CASE ( 2 )
         CALL open_w_dataset ( grid%auxhist2_oid, TRIM(fname), grid ,  &
                               config_flags , output_aux_hist2 , n2, ierr )
       CASE ( 3 )
         CALL open_w_dataset ( grid%auxhist3_oid, TRIM(fname), grid ,  &
                               config_flags , output_aux_hist3 , n2, ierr )
       CASE ( 4 )
         CALL open_w_dataset ( grid%auxhist4_oid, TRIM(fname), grid ,  &
                               config_flags , output_aux_hist4 , n2, ierr )
       CASE ( 5 )
         CALL open_w_dataset ( grid%auxhist5_oid, TRIM(fname), grid ,  &
                               config_flags , output_aux_hist5 , n2, ierr )
       CASE ( 6 )
         CALL open_w_dataset ( grid%auxhist6_oid, TRIM(fname), grid ,  &
                               config_flags , output_aux_hist6 , n2, ierr )
       CASE ( 7 )
         CALL open_w_dataset ( grid%auxhist7_oid, TRIM(fname), grid ,  &
                               config_flags , output_aux_hist7 , n2, ierr )
       CASE ( 8 )
         CALL open_w_dataset ( grid%auxhist8_oid, TRIM(fname), grid ,  &
                               config_flags , output_aux_hist8 , n2, ierr )
       CASE ( 9 )
         CALL open_w_dataset ( grid%auxhist9_oid, TRIM(fname), grid ,  &
                               config_flags , output_aux_hist9 , n2, ierr )
       CASE ( 10 )
         CALL open_w_dataset ( grid%auxhist10_oid, TRIM(fname), grid ,  &
                               config_flags , output_aux_hist10 , n2, ierr )
       CASE ( 11 )
         CALL open_w_dataset ( grid%auxhist11_oid, TRIM(fname), grid ,  &
                               config_flags , output_aux_hist11 , n2, ierr )
     END SELECT
     IF ( ierr .NE. 0 ) THEN
       WRITE ( message , '("med_hist_out : error opening ",A," for writing. ",I3)') TRIM ( fname ), ierr
       CALL wrf_message( message )
     ENDIF
   END IF

   ! early return after training
   IF ( .NOT. grid%return_after_training_io ) THEN
     SELECT CASE( stream )
       CASE ( 0 )
         CALL output_history ( grid%oid, grid , config_flags , ierr )
       CASE ( 1 )
         CALL output_aux_hist1 ( grid%auxhist1_oid, grid , config_flags , ierr )
       CASE ( 2 )
         CALL output_aux_hist2 ( grid%auxhist2_oid, grid , config_flags , ierr )
       CASE ( 3 )
         CALL output_aux_hist3 ( grid%auxhist3_oid, grid , config_flags , ierr )
       CASE ( 4 )
         CALL output_aux_hist4 ( grid%auxhist4_oid, grid , config_flags , ierr )
       CASE ( 5 )
         CALL output_aux_hist5 ( grid%auxhist5_oid, grid , config_flags , ierr )
       CASE ( 6 )
         CALL output_aux_hist6 ( grid%auxhist6_oid, grid , config_flags , ierr )
       CASE ( 7 )
         CALL output_aux_hist7 ( grid%auxhist7_oid, grid , config_flags , ierr )
       CASE ( 8 )
         CALL output_aux_hist8 ( grid%auxhist8_oid, grid , config_flags , ierr )
       CASE ( 9 )
         CALL output_aux_hist9 ( grid%auxhist9_oid, grid , config_flags , ierr )
       CASE ( 10 )
         CALL output_aux_hist10 ( grid%auxhist10_oid, grid , config_flags , ierr )
       CASE ( 11 )
         CALL output_aux_hist11 ( grid%auxhist11_oid, grid , config_flags , ierr )
     END SELECT

     grid%nframes(stream) = grid%nframes(stream) + 1

     SELECT CASE( stream )
       CASE ( 0 )
         IF ( grid%nframes(stream) >= config_flags%frames_per_outfile ) THEN
           CALL close_dataset ( grid%oid , config_flags , "DATASET=HISTORY" ) 
           grid%oid = 0
           grid%nframes(stream) = 0
         ENDIF
       CASE ( 1 )
         IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist1 ) THEN
           CALL close_dataset ( grid%auxhist1_oid , config_flags , n2 ) 
           grid%auxhist1_oid = 0
           grid%nframes(stream) = 0
         ENDIF
       CASE ( 2 )
         IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist2 ) THEN
           CALL close_dataset ( grid%auxhist2_oid , config_flags , n2 ) 
           grid%auxhist2_oid = 0
           grid%nframes(stream) = 0
         ENDIF
       CASE ( 3 )
         IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist3 ) THEN
           CALL close_dataset ( grid%auxhist3_oid , config_flags , n2 ) 
           grid%auxhist3_oid = 0
           grid%nframes(stream) = 0
         ENDIF
       CASE ( 4 )
         IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist4 ) THEN
           CALL close_dataset ( grid%auxhist4_oid , config_flags , n2 ) 
           grid%auxhist4_oid = 0
           grid%nframes(stream) = 0
         ENDIF
       CASE ( 5 )
         IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist5 ) THEN
           CALL close_dataset ( grid%auxhist5_oid , config_flags , n2 ) 
           grid%auxhist5_oid = 0
           grid%nframes(stream) = 0
         ENDIF
       CASE ( 6 )
         IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist6 ) THEN
           CALL close_dataset ( grid%auxhist6_oid , config_flags , n2 ) 
           grid%auxhist6_oid = 0
           grid%nframes(stream) = 0
         ENDIF
       CASE ( 7 )
         IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist7 ) THEN
           CALL close_dataset ( grid%auxhist7_oid , config_flags , n2 ) 
           grid%auxhist7_oid = 0
           grid%nframes(stream) = 0
         ENDIF
       CASE ( 8 )
         IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist8 ) THEN
           CALL close_dataset ( grid%auxhist8_oid , config_flags , n2 ) 
           grid%auxhist8_oid = 0
           grid%nframes(stream) = 0
         ENDIF
       CASE ( 9 )
         IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist9 ) THEN
           CALL close_dataset ( grid%auxhist9_oid , config_flags , n2 ) 
           grid%auxhist9_oid = 0
           grid%nframes(stream) = 0
         ENDIF
       CASE ( 10 )
         IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist10 ) THEN
           CALL close_dataset ( grid%auxhist10_oid , config_flags , n2 ) 
           grid%auxhist10_oid = 0
           grid%nframes(stream) = 0
         ENDIF
       CASE ( 11 )
         IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist11 ) THEN
           CALL close_dataset ( grid%auxhist11_oid , config_flags , n2 ) 
           grid%auxhist11_oid = 0
           grid%nframes(stream) = 0
         ENDIF
     END SELECT
     IF ( wrf_dm_on_monitor() ) THEN
       WRITE ( message , FMT = '("Writing ",A30," for domain ",I8)' )TRIM(fname),grid%id
       CALL end_timing ( TRIM(message) )
     END IF
   ELSE
     CALL wrf_debug( 1, 'DEBUG:  med_hist_out() returned after training' )
   ENDIF

   RETURN
END SUBROUTINE med_hist_out

SUBROUTINE med_auxinput1_in ( grid , config_flags )
   USE module_domain
   USE module_configure
   IMPLICIT NONE
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   CALL med_auxinput_in( grid , 1 , config_flags )
   RETURN
END SUBROUTINE med_auxinput1_in

SUBROUTINE med_auxinput2_in ( grid , config_flags )
   USE module_domain
   USE module_configure
   IMPLICIT NONE
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   CALL med_auxinput_in( grid , 2 , config_flags )
   RETURN
END SUBROUTINE med_auxinput2_in

SUBROUTINE med_auxinput3_in ( grid , config_flags )
   USE module_domain
   USE module_configure
   IMPLICIT NONE
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   CALL med_auxinput_in( grid , 3 , config_flags )
   RETURN
END SUBROUTINE med_auxinput3_in

SUBROUTINE med_auxinput4_in ( grid , config_flags )
   USE module_domain
   USE module_configure
   IMPLICIT NONE
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   CALL med_auxinput_in( grid , 4 , config_flags )
   RETURN
END SUBROUTINE med_auxinput4_in

SUBROUTINE med_auxinput5_in ( grid , config_flags )
   USE module_domain
   USE module_configure
   IMPLICIT NONE
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   CALL med_auxinput_in( grid , 5 , config_flags )
   RETURN
END SUBROUTINE med_auxinput5_in

SUBROUTINE med_auxinput6_in ( grid , config_flags )
   USE module_domain
   USE module_configure
   IMPLICIT NONE
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   CALL med_auxinput_in( grid , 6 , config_flags )
   RETURN
END SUBROUTINE med_auxinput6_in

SUBROUTINE med_auxinput7_in ( grid , config_flags )
   USE module_domain
   USE module_configure
   IMPLICIT NONE
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   CALL med_auxinput_in( grid , 7 , config_flags )
   RETURN
END SUBROUTINE med_auxinput7_in

SUBROUTINE med_auxinput8_in ( grid , config_flags )
   USE module_domain
   USE module_configure
   IMPLICIT NONE
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   CALL med_auxinput_in( grid , 8 , config_flags )
   RETURN
END SUBROUTINE med_auxinput8_in

SUBROUTINE med_auxinput9_in ( grid , config_flags )
   USE module_domain
   USE module_configure
   IMPLICIT NONE
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   CALL med_auxinput_in( grid , 9 , config_flags )
   RETURN
END SUBROUTINE med_auxinput9_in

SUBROUTINE med_auxinput10_in ( grid , config_flags )
   USE module_domain
   USE module_configure
   IMPLICIT NONE
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   CALL med_auxinput_in( grid , 10 , config_flags )
   RETURN
END SUBROUTINE med_auxinput10_in

SUBROUTINE med_auxinput11_in ( grid , config_flags )
   USE module_domain
   USE module_configure
   IMPLICIT NONE
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   CALL med_auxinput_in( grid , 11 , config_flags )
   RETURN
END SUBROUTINE med_auxinput11_in

SUBROUTINE med_fddaobs_in ( grid , config_flags )
   USE module_domain
   USE module_configure
   IMPLICIT NONE
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   CALL wrf_fddaobs_in( grid, config_flags )
   RETURN
END SUBROUTINE med_fddaobs_in

SUBROUTINE med_auxinput_in ( grid , stream, config_flags )
  ! Driver layer
   USE module_domain
   USE module_io_domain
  ! Model layer
   USE module_configure
   USE module_bc_time_utilities
   USE module_utility

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
   CHARACTER*80                           :: timestr
   TYPE(WRFU_Time)                        :: ST,CT
   INTEGER                                :: n
   LOGICAL                                :: adjust

   CALL nl_get_adjust_input_times( grid%id, adjust )

   IF ( stream .LT. 1 .OR. stream .GT. 11 ) THEN
     WRITE(message,*)'med_auxinput_in: invalid input stream ',stream
     CALL wrf_error_fatal3 ( "mediation_integrate.b" , 1521 ,  message )
   ENDIF
   CALL domain_clock_get( grid, current_time=CT, start_time=ST, current_timestr=timestr )
   SELECT CASE( stream )
     CASE ( 1 )
       IF ( adjust ) CALL adjust_io_timestr( grid%io_intervals( AUXINPUT1_ALARM ), CT, ST, timestr )
       CALL construct_filename2a ( auxname , config_flags%auxinput1_inname, grid%id , 2 , timestr )
     CASE ( 2 )
       IF ( adjust ) CALL adjust_io_timestr( grid%io_intervals( AUXINPUT2_ALARM ), CT, ST, timestr )
       CALL construct_filename2a ( auxname , config_flags%auxinput2_inname , grid%id , 2 , timestr )
     CASE ( 3 )
       IF ( adjust ) CALL adjust_io_timestr( grid%io_intervals( AUXINPUT3_ALARM ), CT, ST, timestr )
       CALL construct_filename2a ( auxname , config_flags%auxinput3_inname , grid%id , 2 , timestr )
     CASE ( 4 )
       IF ( adjust ) CALL adjust_io_timestr( grid%io_intervals( AUXINPUT4_ALARM ), CT, ST, timestr )
       CALL construct_filename2a ( auxname , config_flags%auxinput4_inname , grid%id , 2 , timestr )
     CASE ( 5 )
       IF ( adjust ) CALL adjust_io_timestr( grid%io_intervals( AUXINPUT5_ALARM ), CT, ST, timestr )
       CALL construct_filename2a ( auxname , config_flags%auxinput5_inname , grid%id , 2 , timestr )
     CASE ( 6 )
       IF ( adjust ) CALL adjust_io_timestr( grid%io_intervals( AUXINPUT6_ALARM ), CT, ST, timestr )
       CALL construct_filename2a ( auxname , config_flags%auxinput6_inname , grid%id , 2 , timestr )
     CASE ( 7 )
       IF ( adjust ) CALL adjust_io_timestr( grid%io_intervals( AUXINPUT7_ALARM ), CT, ST, timestr )
       CALL construct_filename2a ( auxname , config_flags%auxinput7_inname , grid%id , 2 , timestr )
     CASE ( 8 )
       IF ( adjust ) CALL adjust_io_timestr( grid%io_intervals( AUXINPUT8_ALARM ), CT, ST, timestr )
       CALL construct_filename2a ( auxname , config_flags%auxinput8_inname , grid%id , 2 , timestr )
     CASE ( 9 )
       IF ( adjust ) CALL adjust_io_timestr( grid%io_intervals( AUXINPUT9_ALARM ), CT, ST, timestr )
       CALL construct_filename2a ( auxname , config_flags%auxinput9_inname , grid%id , 2 , timestr )
     CASE ( 10 )
       IF ( adjust ) CALL adjust_io_timestr( grid%io_intervals( AUXINPUT10_ALARM ), CT, ST, timestr )
       CALL construct_filename2a ( auxname , config_flags%gfdda_inname , grid%id , 2 , timestr )
     CASE ( 11 )
       IF ( adjust ) CALL adjust_io_timestr( grid%io_intervals( AUXINPUT11_ALARM ), CT, ST, timestr )
       CALL construct_filename2a ( auxname , config_flags%auxinput11_inname , grid%id , 2 , timestr )
   END SELECT
   IF (      ( stream .eq. 1 .and. grid%auxinput1_oid .eq. 0 )    &
        .or. ( stream .eq. 2 .and. grid%auxinput2_oid .eq. 0 )    &
        .or. ( stream .eq. 3 .and. grid%auxinput3_oid .eq. 0 )    &
        .or. ( stream .eq. 4 .and. grid%auxinput4_oid .eq. 0 )    &
        .or. ( stream .eq. 5 .and. grid%auxinput5_oid .eq. 0 )    &
        .or. ( stream .eq. 6 .and. grid%auxinput6_oid .eq. 0 )    &
        .or. ( stream .eq. 7 .and. grid%auxinput7_oid .eq. 0 )    &
        .or. ( stream .eq. 8 .and. grid%auxinput8_oid .eq. 0 )    &
        .or. ( stream .eq. 9 .and. grid%auxinput9_oid .eq. 0 )    &
        .or. ( stream .eq. 10 .and. grid%auxinput10_oid .eq. 0 )    &
        .or. ( stream .eq. 11 .and. grid%auxinput11_oid .eq. 0 )    &
      ) THEN

       IF      ( stream .EQ. 10 ) THEN
         WRITE(n2,'("DATASET=AUXINPUT10")')
       ELSE IF ( stream .EQ. 11 ) THEN
         WRITE(n2,'("DATASET=AUXINPUT11")')
       ELSE
         WRITE(n2,'("DATASET=AUXINPUT",I1)')stream
       ENDIF
       WRITE ( message , '("med_auxinput_in : opening ",A," for reading. ",I3)') TRIM ( auxname ), ierr
       CALL wrf_debug( 1, message )

!<DESCRIPTION>
!
!Open_u_dataset is called rather than open_r_dataset to allow interfaces
!that can do blending or masking to update an existing field. (MCEL IO does this).
!No effect for other interfaces; open_u_dataset is equivalent to open_r_dataset 
!in those cases.
!
!</DESCRIPTION>

     SELECT CASE( stream )
       CASE ( 1 )
         CALL open_u_dataset ( grid%auxinput1_oid, TRIM(auxname), grid ,  &
                               config_flags , input_aux_model_input1 , n2, ierr )
       CASE ( 2 )
         CALL open_u_dataset ( grid%auxinput2_oid, TRIM(auxname), grid ,  &
                               config_flags , input_aux_model_input2 , n2, ierr )
       CASE ( 3 )
         CALL open_u_dataset ( grid%auxinput3_oid, TRIM(auxname), grid ,  &
                               config_flags , input_aux_model_input3 , n2, ierr )
       CASE ( 4 )
         CALL open_u_dataset ( grid%auxinput4_oid, TRIM(auxname), grid ,  &
                               config_flags , input_aux_model_input4 , n2, ierr )
       CASE ( 5 )
         CALL open_u_dataset ( grid%auxinput5_oid, TRIM(auxname), grid ,  &
                               config_flags , input_aux_model_input5 , n2, ierr )
       CASE ( 6 )
         CALL open_u_dataset ( grid%auxinput6_oid, TRIM(auxname), grid ,  &
                               config_flags , input_aux_model_input6 , n2, ierr )
       CASE ( 7 )
         CALL open_u_dataset ( grid%auxinput7_oid, TRIM(auxname), grid ,  &
                               config_flags , input_aux_model_input7 , n2, ierr )
       CASE ( 8 )
         CALL open_u_dataset ( grid%auxinput8_oid, TRIM(auxname), grid ,  &
                               config_flags , input_aux_model_input8 , n2, ierr )
       CASE ( 9 )
         CALL open_u_dataset ( grid%auxinput9_oid, TRIM(auxname), grid ,  &
                               config_flags , input_aux_model_input9 , n2, ierr )
       CASE ( 10 )
         CALL open_u_dataset ( grid%auxinput10_oid, TRIM(auxname), grid ,  &
                               config_flags , input_aux_model_input10 , n2, ierr )
       CASE ( 11 )
         CALL open_u_dataset ( grid%auxinput11_oid, TRIM(auxname), grid ,  &
                               config_flags , input_aux_model_input11 , n2, ierr )
     END SELECT
     IF ( ierr .NE. 0 ) THEN
       CALL wrf_message( message )
     ENDIF
   END IF
   ! early return after training
   IF ( .NOT. grid%return_after_training_io ) THEN
     SELECT CASE( stream )
       CASE ( 1 )
         CALL input_aux_model_input1 ( grid%auxinput1_oid, grid , config_flags , ierr )
       CASE ( 2 )
         CALL input_aux_model_input2 ( grid%auxinput2_oid, grid , config_flags , ierr )
       CASE ( 3 )
         CALL input_aux_model_input3 ( grid%auxinput3_oid, grid , config_flags , ierr )
       CASE ( 4 )
         CALL input_aux_model_input4 ( grid%auxinput4_oid, grid , config_flags , ierr )
       CASE ( 5 )
         CALL input_aux_model_input5 ( grid%auxinput5_oid, grid , config_flags , ierr )
       CASE ( 6 )
         CALL input_aux_model_input6 ( grid%auxinput6_oid, grid , config_flags , ierr )
       CASE ( 7 )
         CALL input_aux_model_input7 ( grid%auxinput7_oid, grid , config_flags , ierr )
       CASE ( 8 )
         CALL input_aux_model_input8 ( grid%auxinput8_oid, grid , config_flags , ierr )
       CASE ( 9 )
         CALL input_aux_model_input9 ( grid%auxinput9_oid, grid , config_flags , ierr )
       CASE ( 10 )
         CALL input_aux_model_input10 ( grid%auxinput10_oid, grid , config_flags , ierr )
       CASE ( 11 )
         CALL input_aux_model_input11 ( grid%auxinput11_oid, grid , config_flags , ierr )
     END SELECT
   ELSE
     CALL wrf_debug( 1, 'DEBUG:  med_auxinput_in() returned after training' )
   ENDIF
   RETURN
END SUBROUTINE med_auxinput_in

SUBROUTINE med_filter_out ( grid , config_flags )
  ! Driver layer
   USE module_domain
   USE module_io_domain
   USE module_timing
   USE module_configure
  ! Model layer
   USE module_bc_time_utilities

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   CHARACTER*80                           :: rstname , outname
   INTEGER                                :: fid , rid
   CHARACTER (LEN=256)                    :: message
   INTEGER                                :: ierr
   INTEGER                                :: myproc
   CHARACTER*80                           :: timestr

   IF ( config_flags%write_input ) THEN

   IF ( wrf_dm_on_monitor() ) THEN
     CALL start_timing
   END IF

     CALL domain_clock_get( grid, current_timestr=timestr )
     CALL construct_filename2a ( outname , config_flags%input_outname , grid%id , 2 , timestr )

     WRITE ( message , '("med_filter_out 1: opening ",A," for writing. ",I3)') TRIM ( outname ), ierr
     CALL wrf_debug( 1, message )

     CALL open_w_dataset ( fid, TRIM(outname), grid ,  &
                           config_flags , output_model_input , "DATASET=INPUT", ierr )
     IF ( ierr .NE. 0 ) THEN
       CALL wrf_error_fatal3 ( "mediation_integrate.b" , 1700 ,  message )
     ENDIF

     IF ( ierr .NE. 0 ) THEN
       CALL wrf_error_fatal3 ( "mediation_integrate.b" , 1704 ,  message )
     ENDIF

   CALL output_model_input ( fid, grid , config_flags , ierr )
   CALL close_dataset ( fid , config_flags , "DATASET=INPUT" )

   IF ( wrf_dm_on_monitor() ) THEN
     WRITE ( message , FMT = '("Writing filter output for domain ",I8)' ) grid%id
     CALL end_timing ( TRIM(message) )
   END IF
   ENDIF

   RETURN
END SUBROUTINE med_filter_out

SUBROUTINE med_latbound_in ( grid , config_flags )
  ! Driver layer
   USE module_domain
   USE module_io_domain
   USE module_timing
   USE module_configure
  ! Model layer
   USE module_bc_time_utilities
   USE module_utility

   IMPLICIT NONE

  
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
   Type (WRFU_Time )                      :: startTime, stopTime, currentTime
   Type (WRFU_TimeInterval )              :: stepTime
integer myproc,i,j,k

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

   CALL wrf_debug ( 200 , 'in med_latbound_in' )

   IF ( grid%id .EQ. 1 .AND. config_flags%specified .AND. config_flags%io_form_boundary .GT. 0 ) THEN

     CALL domain_clock_get( grid, current_time=currentTime, &
                                  start_time=startTime,     &
                                  stop_time=stopTime,       &
                                  time_step=stepTime )

     IF ( ( lbc_read_time( currentTime ) ) .AND. &
          ( currentTime + stepTime .GE. stopTime ) .AND. &
          ( currentTime .NE. startTime ) ) THEN
       CALL wrf_debug( 100 , 'med_latbound_in: Skipping attempt to read lateral boundary file during last time step ' )

     ELSE IF ( WRFU_AlarmIsRinging( grid%alarms( BOUNDARY_ALARM ), rc=rc ) ) THEN
       CALL wrf_debug ( 100 , 'in med_latbound_in preparing to read' )
       CALL WRFU_AlarmRingerOff( grid%alarms( BOUNDARY_ALARM ), rc=rc )
       IF ( wrf_dm_on_monitor() ) CALL start_timing

! typically a <date> wouldnt be part of the bdy_inname, so just pass a dummy
       CALL construct_filename2a ( bdyname , config_flags%bdy_inname , grid%id , 2 , 'dummydate' )

       CALL wrf_inquire_opened(head_grid%lbc_fid , TRIM(bdyname) , open_status , ierr ) 
       IF ( open_status .EQ. WRF_FILE_OPENED_FOR_READ ) THEN
         lbc_opened = .TRUE.
       ELSE
         lbc_opened = .FALSE.
       ENDIF
       CALL wrf_dm_bcast_bytes ( lbc_opened , 4 )
       IF ( .NOT. lbc_opened ) THEN
         CALL construct_filename1 ( bdyname , 'wrfbdy' , grid%id , 2 )
         CALL open_r_dataset ( head_grid%lbc_fid, TRIM(bdyname) , grid , config_flags , "DATASET=BOUNDARY", ierr )
          IF ( ierr .NE. 0 ) THEN
            WRITE( message, * ) 'med_latbound_in: error opening ',TRIM(bdyname), ' for reading. IERR = ',ierr
            CALL wrf_error_fatal3 ( "mediation_integrate.b" , 1784 ,  message )
          ENDIF
       ELSE
         CALL wrf_debug( 100 , bdyname // 'already opened' )
       ENDIF
       CALL wrf_debug( 100 , 'med_latbound_in: calling input_boundary ' )
       CALL input_boundary ( grid%lbc_fid, grid , config_flags , ierr )

       CALL domain_clock_get( grid, current_time=currentTime )
       DO WHILE (currentTime .GE. grid%next_bdy_time )         ! next_bdy_time is set by input_boundary from bdy file
         CALL wrf_debug( 100 , 'med_latbound_in: calling input_boundary ' )
         CALL input_boundary ( grid%lbc_fid, grid , config_flags , ierr )
       ENDDO
       CALL WRFU_AlarmSet( grid%alarms( BOUNDARY_ALARM ), RingTime=grid%next_bdy_time, rc=rc )

       IF ( ierr .NE. 0 .and. ierr .NE. WRF_WARN_NETCDF ) THEN
         WRITE( message, * ) 'med_latbound_in: error reading ',TRIM(bdyname), ' IERR = ',ierr
         CALL wrf_error_fatal3 ( "mediation_integrate.b" , 1801 ,  message )
       ENDIF
       IF ( currentTime .EQ. grid%this_bdy_time ) grid%dtbc = 0.
  
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
   USE module_configure
  ! Model layer

   IMPLICIT NONE
!<DESCRIPTION>
!
!The driver layer routine integrate() calls this mediation layer routine
!prior to initiating a time step on the domain specified by the argument
!grid.  This provides the model-layer contributor an opportunity to make
!any pre-time-step initializations that pertain to a particular model
!domain.  In WRF, this routine is used to call
!set_scalar_indices_from_config for the specified domain.
!
!</DESCRIPTION>

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
  ! Local
   INTEGER                                    :: idum1 , idum2

   CALL set_scalar_indices_from_config ( grid%id , idum1 , idum2 )

   RETURN

END SUBROUTINE med_setup_step

SUBROUTINE med_endup_step ( grid , config_flags )
  ! Driver layer
   USE module_domain
   USE module_configure
  ! Model layer

   IMPLICIT NONE
!<DESCRIPTION>
!
!The driver layer routine integrate() calls this mediation layer routine
!prior to initiating a time step on the domain specified by the argument
!grid.  This provides the model-layer contributor an opportunity to make
!any pre-time-step initializations that pertain to a particular model
!domain.  In WRF, this routine is used to call
!set_scalar_indices_from_config for the specified domain.
!
!</DESCRIPTION>

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(OUT)   :: config_flags
  ! Local
   INTEGER                                    :: idum1 , idum2

   IF ( grid%id .EQ. 1 ) THEN
     ! turn off the restart flag after the first mother-domain step is finished
     model_config_rec%restart = .FALSE.
     config_flags%restart = .FALSE.
     CALL nl_set_restart(1, .FALSE.)

   ENDIF

   RETURN

END SUBROUTINE med_endup_step

 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
