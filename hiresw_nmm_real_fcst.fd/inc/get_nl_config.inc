!STARTOFREGISTRYGENERATEDINCLUDE 'inc/get_nl_config.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
SUBROUTINE nl_get_run_days ( id_id , run_days )
  integer , INTENT(OUT) :: run_days
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_run_days: run_days applies to all domains. First arg ignored.')
  ENDIF
  run_days = model_config_rec%run_days
  RETURN
END SUBROUTINE nl_get_run_days
SUBROUTINE nl_get_run_hours ( id_id , run_hours )
  integer , INTENT(OUT) :: run_hours
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_run_hours: run_hours applies to all domains. First arg ignored.')
  ENDIF
  run_hours = model_config_rec%run_hours
  RETURN
END SUBROUTINE nl_get_run_hours
SUBROUTINE nl_get_run_minutes ( id_id , run_minutes )
  integer , INTENT(OUT) :: run_minutes
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_run_minutes: run_minutes applies to all domains. First arg ignored.')
  ENDIF
  run_minutes = model_config_rec%run_minutes
  RETURN
END SUBROUTINE nl_get_run_minutes
SUBROUTINE nl_get_run_seconds ( id_id , run_seconds )
  integer , INTENT(OUT) :: run_seconds
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_run_seconds: run_seconds applies to all domains. First arg ignored.')
  ENDIF
  run_seconds = model_config_rec%run_seconds
  RETURN
END SUBROUTINE nl_get_run_seconds
SUBROUTINE nl_get_start_year ( id_id , start_year )
  integer , INTENT(OUT) :: start_year
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_start_year: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  start_year = model_config_rec%start_year(id_id)
  RETURN
END SUBROUTINE nl_get_start_year
SUBROUTINE nl_get_start_month ( id_id , start_month )
  integer , INTENT(OUT) :: start_month
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_start_month: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  start_month = model_config_rec%start_month(id_id)
  RETURN
END SUBROUTINE nl_get_start_month
SUBROUTINE nl_get_start_day ( id_id , start_day )
  integer , INTENT(OUT) :: start_day
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_start_day: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  start_day = model_config_rec%start_day(id_id)
  RETURN
END SUBROUTINE nl_get_start_day
SUBROUTINE nl_get_start_hour ( id_id , start_hour )
  integer , INTENT(OUT) :: start_hour
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_start_hour: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  start_hour = model_config_rec%start_hour(id_id)
  RETURN
END SUBROUTINE nl_get_start_hour
SUBROUTINE nl_get_start_minute ( id_id , start_minute )
  integer , INTENT(OUT) :: start_minute
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_start_minute: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  start_minute = model_config_rec%start_minute(id_id)
  RETURN
END SUBROUTINE nl_get_start_minute
SUBROUTINE nl_get_start_second ( id_id , start_second )
  integer , INTENT(OUT) :: start_second
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_start_second: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  start_second = model_config_rec%start_second(id_id)
  RETURN
END SUBROUTINE nl_get_start_second
SUBROUTINE nl_get_end_year ( id_id , end_year )
  integer , INTENT(OUT) :: end_year
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_end_year: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  end_year = model_config_rec%end_year(id_id)
  RETURN
END SUBROUTINE nl_get_end_year
SUBROUTINE nl_get_end_month ( id_id , end_month )
  integer , INTENT(OUT) :: end_month
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_end_month: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  end_month = model_config_rec%end_month(id_id)
  RETURN
END SUBROUTINE nl_get_end_month
SUBROUTINE nl_get_end_day ( id_id , end_day )
  integer , INTENT(OUT) :: end_day
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_end_day: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  end_day = model_config_rec%end_day(id_id)
  RETURN
END SUBROUTINE nl_get_end_day
SUBROUTINE nl_get_end_hour ( id_id , end_hour )
  integer , INTENT(OUT) :: end_hour
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_end_hour: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  end_hour = model_config_rec%end_hour(id_id)
  RETURN
END SUBROUTINE nl_get_end_hour
SUBROUTINE nl_get_end_minute ( id_id , end_minute )
  integer , INTENT(OUT) :: end_minute
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_end_minute: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  end_minute = model_config_rec%end_minute(id_id)
  RETURN
END SUBROUTINE nl_get_end_minute
SUBROUTINE nl_get_end_second ( id_id , end_second )
  integer , INTENT(OUT) :: end_second
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_end_second: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  end_second = model_config_rec%end_second(id_id)
  RETURN
END SUBROUTINE nl_get_end_second
SUBROUTINE nl_get_interval_seconds ( id_id , interval_seconds )
  integer , INTENT(OUT) :: interval_seconds
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_interval_seconds: interval_seconds applies to all domains. First arg ignored.')
  ENDIF
  interval_seconds = model_config_rec%interval_seconds
  RETURN
END SUBROUTINE nl_get_interval_seconds
SUBROUTINE nl_get_input_from_file ( id_id , input_from_file )
  logical , INTENT(OUT) :: input_from_file
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_input_from_file: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  input_from_file = model_config_rec%input_from_file(id_id)
  RETURN
END SUBROUTINE nl_get_input_from_file
SUBROUTINE nl_get_fine_input_stream ( id_id , fine_input_stream )
  integer , INTENT(OUT) :: fine_input_stream
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_fine_input_stream: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  fine_input_stream = model_config_rec%fine_input_stream(id_id)
  RETURN
END SUBROUTINE nl_get_fine_input_stream
SUBROUTINE nl_get_history_interval ( id_id , history_interval )
  integer , INTENT(OUT) :: history_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_history_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  history_interval = model_config_rec%history_interval(id_id)
  RETURN
END SUBROUTINE nl_get_history_interval
SUBROUTINE nl_get_frames_per_outfile ( id_id , frames_per_outfile )
  integer , INTENT(OUT) :: frames_per_outfile
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_frames_per_outfile: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  frames_per_outfile = model_config_rec%frames_per_outfile(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_outfile
SUBROUTINE nl_get_frames_per_auxhist1 ( id_id , frames_per_auxhist1 )
  integer , INTENT(OUT) :: frames_per_auxhist1
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_frames_per_auxhist1: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  frames_per_auxhist1 = model_config_rec%frames_per_auxhist1(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist1
SUBROUTINE nl_get_frames_per_auxhist2 ( id_id , frames_per_auxhist2 )
  integer , INTENT(OUT) :: frames_per_auxhist2
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_frames_per_auxhist2: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  frames_per_auxhist2 = model_config_rec%frames_per_auxhist2(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist2
SUBROUTINE nl_get_frames_per_auxhist3 ( id_id , frames_per_auxhist3 )
  integer , INTENT(OUT) :: frames_per_auxhist3
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_frames_per_auxhist3: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  frames_per_auxhist3 = model_config_rec%frames_per_auxhist3(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist3
SUBROUTINE nl_get_frames_per_auxhist4 ( id_id , frames_per_auxhist4 )
  integer , INTENT(OUT) :: frames_per_auxhist4
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_frames_per_auxhist4: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  frames_per_auxhist4 = model_config_rec%frames_per_auxhist4(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist4
SUBROUTINE nl_get_frames_per_auxhist5 ( id_id , frames_per_auxhist5 )
  integer , INTENT(OUT) :: frames_per_auxhist5
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_frames_per_auxhist5: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  frames_per_auxhist5 = model_config_rec%frames_per_auxhist5(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist5
SUBROUTINE nl_get_frames_per_auxhist6 ( id_id , frames_per_auxhist6 )
  integer , INTENT(OUT) :: frames_per_auxhist6
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_frames_per_auxhist6: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  frames_per_auxhist6 = model_config_rec%frames_per_auxhist6(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist6
SUBROUTINE nl_get_frames_per_auxhist7 ( id_id , frames_per_auxhist7 )
  integer , INTENT(OUT) :: frames_per_auxhist7
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_frames_per_auxhist7: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  frames_per_auxhist7 = model_config_rec%frames_per_auxhist7(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist7
SUBROUTINE nl_get_frames_per_auxhist8 ( id_id , frames_per_auxhist8 )
  integer , INTENT(OUT) :: frames_per_auxhist8
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_frames_per_auxhist8: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  frames_per_auxhist8 = model_config_rec%frames_per_auxhist8(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist8
SUBROUTINE nl_get_frames_per_auxhist9 ( id_id , frames_per_auxhist9 )
  integer , INTENT(OUT) :: frames_per_auxhist9
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_frames_per_auxhist9: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  frames_per_auxhist9 = model_config_rec%frames_per_auxhist9(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist9
SUBROUTINE nl_get_frames_per_auxhist10 ( id_id , frames_per_auxhist10 )
  integer , INTENT(OUT) :: frames_per_auxhist10
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_frames_per_auxhist10: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  frames_per_auxhist10 = model_config_rec%frames_per_auxhist10(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist10
SUBROUTINE nl_get_frames_per_auxhist11 ( id_id , frames_per_auxhist11 )
  integer , INTENT(OUT) :: frames_per_auxhist11
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_frames_per_auxhist11: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  frames_per_auxhist11 = model_config_rec%frames_per_auxhist11(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist11
SUBROUTINE nl_get_restart ( id_id , restart )
  logical , INTENT(OUT) :: restart
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_restart: restart applies to all domains. First arg ignored.')
  ENDIF
  restart = model_config_rec%restart
  RETURN
END SUBROUTINE nl_get_restart
SUBROUTINE nl_get_restart_interval ( id_id , restart_interval )
  integer , INTENT(OUT) :: restart_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_restart_interval: restart_interval applies to all domains. First arg ignored.')
  ENDIF
  restart_interval = model_config_rec%restart_interval
  RETURN
END SUBROUTINE nl_get_restart_interval
SUBROUTINE nl_get_io_form_input ( id_id , io_form_input )
  integer , INTENT(OUT) :: io_form_input
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_io_form_input: io_form_input applies to all domains. First arg ignored.')
  ENDIF
  io_form_input = model_config_rec%io_form_input
  RETURN
END SUBROUTINE nl_get_io_form_input
SUBROUTINE nl_get_io_form_history ( id_id , io_form_history )
  integer , INTENT(OUT) :: io_form_history
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_io_form_history: io_form_history applies to all domains. First arg ignored.')
  ENDIF
  io_form_history = model_config_rec%io_form_history
  RETURN
END SUBROUTINE nl_get_io_form_history
SUBROUTINE nl_get_io_form_restart ( id_id , io_form_restart )
  integer , INTENT(OUT) :: io_form_restart
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_io_form_restart: io_form_restart applies to all domains. First arg ignored.')
  ENDIF
  io_form_restart = model_config_rec%io_form_restart
  RETURN
END SUBROUTINE nl_get_io_form_restart
SUBROUTINE nl_get_io_form_boundary ( id_id , io_form_boundary )
  integer , INTENT(OUT) :: io_form_boundary
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_io_form_boundary: io_form_boundary applies to all domains. First arg ignored.')
  ENDIF
  io_form_boundary = model_config_rec%io_form_boundary
  RETURN
END SUBROUTINE nl_get_io_form_boundary
SUBROUTINE nl_get_debug_level ( id_id , debug_level )
  integer , INTENT(OUT) :: debug_level
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_debug_level: debug_level applies to all domains. First arg ignored.')
  ENDIF
  debug_level = model_config_rec%debug_level
  RETURN
END SUBROUTINE nl_get_debug_level
SUBROUTINE nl_get_self_test_domain ( id_id , self_test_domain )
  logical , INTENT(OUT) :: self_test_domain
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_self_test_domain: self_test_domain applies to all domains. First arg ignored.')
  ENDIF
  self_test_domain = model_config_rec%self_test_domain
  RETURN
END SUBROUTINE nl_get_self_test_domain
SUBROUTINE nl_get_history_outname ( id_id , history_outname )
  character*256 , INTENT(OUT) :: history_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_history_outname: history_outname applies to all domains. First arg ignored.')
  ENDIF
  history_outname = trim(model_config_rec%history_outname)
  RETURN
END SUBROUTINE nl_get_history_outname
SUBROUTINE nl_get_auxhist1_outname ( id_id , auxhist1_outname )
  character*256 , INTENT(OUT) :: auxhist1_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_auxhist1_outname: auxhist1_outname applies to all domains. First arg ignored.')
  ENDIF
  auxhist1_outname = trim(model_config_rec%auxhist1_outname)
  RETURN
END SUBROUTINE nl_get_auxhist1_outname
SUBROUTINE nl_get_auxhist2_outname ( id_id , auxhist2_outname )
  character*256 , INTENT(OUT) :: auxhist2_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_auxhist2_outname: auxhist2_outname applies to all domains. First arg ignored.')
  ENDIF
  auxhist2_outname = trim(model_config_rec%auxhist2_outname)
  RETURN
END SUBROUTINE nl_get_auxhist2_outname
SUBROUTINE nl_get_auxhist3_outname ( id_id , auxhist3_outname )
  character*256 , INTENT(OUT) :: auxhist3_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_auxhist3_outname: auxhist3_outname applies to all domains. First arg ignored.')
  ENDIF
  auxhist3_outname = trim(model_config_rec%auxhist3_outname)
  RETURN
END SUBROUTINE nl_get_auxhist3_outname
SUBROUTINE nl_get_auxhist4_outname ( id_id , auxhist4_outname )
  character*256 , INTENT(OUT) :: auxhist4_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_auxhist4_outname: auxhist4_outname applies to all domains. First arg ignored.')
  ENDIF
  auxhist4_outname = trim(model_config_rec%auxhist4_outname)
  RETURN
END SUBROUTINE nl_get_auxhist4_outname
SUBROUTINE nl_get_auxhist5_outname ( id_id , auxhist5_outname )
  character*256 , INTENT(OUT) :: auxhist5_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_auxhist5_outname: auxhist5_outname applies to all domains. First arg ignored.')
  ENDIF
  auxhist5_outname = trim(model_config_rec%auxhist5_outname)
  RETURN
END SUBROUTINE nl_get_auxhist5_outname
SUBROUTINE nl_get_auxhist6_outname ( id_id , auxhist6_outname )
  character*256 , INTENT(OUT) :: auxhist6_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_auxhist6_outname: auxhist6_outname applies to all domains. First arg ignored.')
  ENDIF
  auxhist6_outname = trim(model_config_rec%auxhist6_outname)
  RETURN
END SUBROUTINE nl_get_auxhist6_outname
SUBROUTINE nl_get_auxhist7_outname ( id_id , auxhist7_outname )
  character*256 , INTENT(OUT) :: auxhist7_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_auxhist7_outname: auxhist7_outname applies to all domains. First arg ignored.')
  ENDIF
  auxhist7_outname = trim(model_config_rec%auxhist7_outname)
  RETURN
END SUBROUTINE nl_get_auxhist7_outname
SUBROUTINE nl_get_auxhist8_outname ( id_id , auxhist8_outname )
  character*256 , INTENT(OUT) :: auxhist8_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_auxhist8_outname: auxhist8_outname applies to all domains. First arg ignored.')
  ENDIF
  auxhist8_outname = trim(model_config_rec%auxhist8_outname)
  RETURN
END SUBROUTINE nl_get_auxhist8_outname
SUBROUTINE nl_get_auxhist9_outname ( id_id , auxhist9_outname )
  character*256 , INTENT(OUT) :: auxhist9_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_auxhist9_outname: auxhist9_outname applies to all domains. First arg ignored.')
  ENDIF
  auxhist9_outname = trim(model_config_rec%auxhist9_outname)
  RETURN
END SUBROUTINE nl_get_auxhist9_outname
SUBROUTINE nl_get_auxhist10_outname ( id_id , auxhist10_outname )
  character*256 , INTENT(OUT) :: auxhist10_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_auxhist10_outname: auxhist10_outname applies to all domains. First arg ignored.')
  ENDIF
  auxhist10_outname = trim(model_config_rec%auxhist10_outname)
  RETURN
END SUBROUTINE nl_get_auxhist10_outname
SUBROUTINE nl_get_auxhist11_outname ( id_id , auxhist11_outname )
  character*256 , INTENT(OUT) :: auxhist11_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_auxhist11_outname: auxhist11_outname applies to all domains. First arg ignored.')
  ENDIF
  auxhist11_outname = trim(model_config_rec%auxhist11_outname)
  RETURN
END SUBROUTINE nl_get_auxhist11_outname
SUBROUTINE nl_get_history_inname ( id_id , history_inname )
  character*256 , INTENT(OUT) :: history_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_history_inname: history_inname applies to all domains. First arg ignored.')
  ENDIF
  history_inname = trim(model_config_rec%history_inname)
  RETURN
END SUBROUTINE nl_get_history_inname
SUBROUTINE nl_get_auxhist1_inname ( id_id , auxhist1_inname )
  character*256 , INTENT(OUT) :: auxhist1_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_auxhist1_inname: auxhist1_inname applies to all domains. First arg ignored.')
  ENDIF
  auxhist1_inname = trim(model_config_rec%auxhist1_inname)
  RETURN
END SUBROUTINE nl_get_auxhist1_inname
SUBROUTINE nl_get_auxhist2_inname ( id_id , auxhist2_inname )
  character*256 , INTENT(OUT) :: auxhist2_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_auxhist2_inname: auxhist2_inname applies to all domains. First arg ignored.')
  ENDIF
  auxhist2_inname = trim(model_config_rec%auxhist2_inname)
  RETURN
END SUBROUTINE nl_get_auxhist2_inname
SUBROUTINE nl_get_auxhist3_inname ( id_id , auxhist3_inname )
  character*256 , INTENT(OUT) :: auxhist3_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_auxhist3_inname: auxhist3_inname applies to all domains. First arg ignored.')
  ENDIF
  auxhist3_inname = trim(model_config_rec%auxhist3_inname)
  RETURN
END SUBROUTINE nl_get_auxhist3_inname
SUBROUTINE nl_get_auxhist4_inname ( id_id , auxhist4_inname )
  character*256 , INTENT(OUT) :: auxhist4_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_auxhist4_inname: auxhist4_inname applies to all domains. First arg ignored.')
  ENDIF
  auxhist4_inname = trim(model_config_rec%auxhist4_inname)
  RETURN
END SUBROUTINE nl_get_auxhist4_inname
SUBROUTINE nl_get_auxhist5_inname ( id_id , auxhist5_inname )
  character*256 , INTENT(OUT) :: auxhist5_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_auxhist5_inname: auxhist5_inname applies to all domains. First arg ignored.')
  ENDIF
  auxhist5_inname = trim(model_config_rec%auxhist5_inname)
  RETURN
END SUBROUTINE nl_get_auxhist5_inname
SUBROUTINE nl_get_auxhist6_inname ( id_id , auxhist6_inname )
  character*256 , INTENT(OUT) :: auxhist6_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_auxhist6_inname: auxhist6_inname applies to all domains. First arg ignored.')
  ENDIF
  auxhist6_inname = trim(model_config_rec%auxhist6_inname)
  RETURN
END SUBROUTINE nl_get_auxhist6_inname
SUBROUTINE nl_get_auxhist7_inname ( id_id , auxhist7_inname )
  character*256 , INTENT(OUT) :: auxhist7_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_auxhist7_inname: auxhist7_inname applies to all domains. First arg ignored.')
  ENDIF
  auxhist7_inname = trim(model_config_rec%auxhist7_inname)
  RETURN
END SUBROUTINE nl_get_auxhist7_inname
SUBROUTINE nl_get_auxhist8_inname ( id_id , auxhist8_inname )
  character*256 , INTENT(OUT) :: auxhist8_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_auxhist8_inname: auxhist8_inname applies to all domains. First arg ignored.')
  ENDIF
  auxhist8_inname = trim(model_config_rec%auxhist8_inname)
  RETURN
END SUBROUTINE nl_get_auxhist8_inname
SUBROUTINE nl_get_auxhist9_inname ( id_id , auxhist9_inname )
  character*256 , INTENT(OUT) :: auxhist9_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_auxhist9_inname: auxhist9_inname applies to all domains. First arg ignored.')
  ENDIF
  auxhist9_inname = trim(model_config_rec%auxhist9_inname)
  RETURN
END SUBROUTINE nl_get_auxhist9_inname
SUBROUTINE nl_get_auxhist10_inname ( id_id , auxhist10_inname )
  character*256 , INTENT(OUT) :: auxhist10_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_auxhist10_inname: auxhist10_inname applies to all domains. First arg ignored.')
  ENDIF
  auxhist10_inname = trim(model_config_rec%auxhist10_inname)
  RETURN
END SUBROUTINE nl_get_auxhist10_inname
SUBROUTINE nl_get_auxhist11_inname ( id_id , auxhist11_inname )
  character*256 , INTENT(OUT) :: auxhist11_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_auxhist11_inname: auxhist11_inname applies to all domains. First arg ignored.')
  ENDIF
  auxhist11_inname = trim(model_config_rec%auxhist11_inname)
  RETURN
END SUBROUTINE nl_get_auxhist11_inname
SUBROUTINE nl_get_auxinput1_outname ( id_id , auxinput1_outname )
  character*256 , INTENT(OUT) :: auxinput1_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_auxinput1_outname: auxinput1_outname applies to all domains. First arg ignored.')
  ENDIF
  auxinput1_outname = trim(model_config_rec%auxinput1_outname)
  RETURN
END SUBROUTINE nl_get_auxinput1_outname
SUBROUTINE nl_get_auxinput2_outname ( id_id , auxinput2_outname )
  character*256 , INTENT(OUT) :: auxinput2_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_auxinput2_outname: auxinput2_outname applies to all domains. First arg ignored.')
  ENDIF
  auxinput2_outname = trim(model_config_rec%auxinput2_outname)
  RETURN
END SUBROUTINE nl_get_auxinput2_outname
SUBROUTINE nl_get_auxinput3_outname ( id_id , auxinput3_outname )
  character*256 , INTENT(OUT) :: auxinput3_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_auxinput3_outname: auxinput3_outname applies to all domains. First arg ignored.')
  ENDIF
  auxinput3_outname = trim(model_config_rec%auxinput3_outname)
  RETURN
END SUBROUTINE nl_get_auxinput3_outname
SUBROUTINE nl_get_auxinput4_outname ( id_id , auxinput4_outname )
  character*256 , INTENT(OUT) :: auxinput4_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_auxinput4_outname: auxinput4_outname applies to all domains. First arg ignored.')
  ENDIF
  auxinput4_outname = trim(model_config_rec%auxinput4_outname)
  RETURN
END SUBROUTINE nl_get_auxinput4_outname
SUBROUTINE nl_get_auxinput5_outname ( id_id , auxinput5_outname )
  character*256 , INTENT(OUT) :: auxinput5_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_auxinput5_outname: auxinput5_outname applies to all domains. First arg ignored.')
  ENDIF
  auxinput5_outname = trim(model_config_rec%auxinput5_outname)
  RETURN
END SUBROUTINE nl_get_auxinput5_outname
SUBROUTINE nl_get_auxinput6_outname ( id_id , auxinput6_outname )
  character*256 , INTENT(OUT) :: auxinput6_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_auxinput6_outname: auxinput6_outname applies to all domains. First arg ignored.')
  ENDIF
  auxinput6_outname = trim(model_config_rec%auxinput6_outname)
  RETURN
END SUBROUTINE nl_get_auxinput6_outname
SUBROUTINE nl_get_auxinput7_outname ( id_id , auxinput7_outname )
  character*256 , INTENT(OUT) :: auxinput7_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_auxinput7_outname: auxinput7_outname applies to all domains. First arg ignored.')
  ENDIF
  auxinput7_outname = trim(model_config_rec%auxinput7_outname)
  RETURN
END SUBROUTINE nl_get_auxinput7_outname
SUBROUTINE nl_get_auxinput8_outname ( id_id , auxinput8_outname )
  character*256 , INTENT(OUT) :: auxinput8_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_auxinput8_outname: auxinput8_outname applies to all domains. First arg ignored.')
  ENDIF
  auxinput8_outname = trim(model_config_rec%auxinput8_outname)
  RETURN
END SUBROUTINE nl_get_auxinput8_outname
SUBROUTINE nl_get_auxinput9_outname ( id_id , auxinput9_outname )
  character*256 , INTENT(OUT) :: auxinput9_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_auxinput9_outname: auxinput9_outname applies to all domains. First arg ignored.')
  ENDIF
  auxinput9_outname = trim(model_config_rec%auxinput9_outname)
  RETURN
END SUBROUTINE nl_get_auxinput9_outname
SUBROUTINE nl_get_auxinput10_outname ( id_id , auxinput10_outname )
  character*256 , INTENT(OUT) :: auxinput10_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_auxinput10_outname: auxinput10_outname applies to all domains. First arg ignored.')
  ENDIF
  auxinput10_outname = trim(model_config_rec%auxinput10_outname)
  RETURN
END SUBROUTINE nl_get_auxinput10_outname
SUBROUTINE nl_get_auxinput11_outname ( id_id , auxinput11_outname )
  character*256 , INTENT(OUT) :: auxinput11_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_auxinput11_outname: auxinput11_outname applies to all domains. First arg ignored.')
  ENDIF
  auxinput11_outname = trim(model_config_rec%auxinput11_outname)
  RETURN
END SUBROUTINE nl_get_auxinput11_outname
SUBROUTINE nl_get_auxinput1_inname ( id_id , auxinput1_inname )
  character*256 , INTENT(OUT) :: auxinput1_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_auxinput1_inname: auxinput1_inname applies to all domains. First arg ignored.')
  ENDIF
  auxinput1_inname = trim(model_config_rec%auxinput1_inname)
  RETURN
END SUBROUTINE nl_get_auxinput1_inname
SUBROUTINE nl_get_auxinput2_inname ( id_id , auxinput2_inname )
  character*256 , INTENT(OUT) :: auxinput2_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_auxinput2_inname: auxinput2_inname applies to all domains. First arg ignored.')
  ENDIF
  auxinput2_inname = trim(model_config_rec%auxinput2_inname)
  RETURN
END SUBROUTINE nl_get_auxinput2_inname
SUBROUTINE nl_get_auxinput3_inname ( id_id , auxinput3_inname )
  character*256 , INTENT(OUT) :: auxinput3_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_auxinput3_inname: auxinput3_inname applies to all domains. First arg ignored.')
  ENDIF
  auxinput3_inname = trim(model_config_rec%auxinput3_inname)
  RETURN
END SUBROUTINE nl_get_auxinput3_inname
SUBROUTINE nl_get_auxinput4_inname ( id_id , auxinput4_inname )
  character*256 , INTENT(OUT) :: auxinput4_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_auxinput4_inname: auxinput4_inname applies to all domains. First arg ignored.')
  ENDIF
  auxinput4_inname = trim(model_config_rec%auxinput4_inname)
  RETURN
END SUBROUTINE nl_get_auxinput4_inname
SUBROUTINE nl_get_auxinput5_inname ( id_id , auxinput5_inname )
  character*256 , INTENT(OUT) :: auxinput5_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_auxinput5_inname: auxinput5_inname applies to all domains. First arg ignored.')
  ENDIF
  auxinput5_inname = trim(model_config_rec%auxinput5_inname)
  RETURN
END SUBROUTINE nl_get_auxinput5_inname
SUBROUTINE nl_get_auxinput6_inname ( id_id , auxinput6_inname )
  character*256 , INTENT(OUT) :: auxinput6_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_auxinput6_inname: auxinput6_inname applies to all domains. First arg ignored.')
  ENDIF
  auxinput6_inname = trim(model_config_rec%auxinput6_inname)
  RETURN
END SUBROUTINE nl_get_auxinput6_inname
SUBROUTINE nl_get_auxinput7_inname ( id_id , auxinput7_inname )
  character*256 , INTENT(OUT) :: auxinput7_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_auxinput7_inname: auxinput7_inname applies to all domains. First arg ignored.')
  ENDIF
  auxinput7_inname = trim(model_config_rec%auxinput7_inname)
  RETURN
END SUBROUTINE nl_get_auxinput7_inname
SUBROUTINE nl_get_auxinput8_inname ( id_id , auxinput8_inname )
  character*256 , INTENT(OUT) :: auxinput8_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_auxinput8_inname: auxinput8_inname applies to all domains. First arg ignored.')
  ENDIF
  auxinput8_inname = trim(model_config_rec%auxinput8_inname)
  RETURN
END SUBROUTINE nl_get_auxinput8_inname
SUBROUTINE nl_get_auxinput9_inname ( id_id , auxinput9_inname )
  character*256 , INTENT(OUT) :: auxinput9_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_auxinput9_inname: auxinput9_inname applies to all domains. First arg ignored.')
  ENDIF
  auxinput9_inname = trim(model_config_rec%auxinput9_inname)
  RETURN
END SUBROUTINE nl_get_auxinput9_inname
SUBROUTINE nl_get_gfdda_inname ( id_id , gfdda_inname )
  character*256 , INTENT(OUT) :: gfdda_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_gfdda_inname: gfdda_inname applies to all domains. First arg ignored.')
  ENDIF
  gfdda_inname = trim(model_config_rec%gfdda_inname)
  RETURN
END SUBROUTINE nl_get_gfdda_inname
SUBROUTINE nl_get_auxinput11_inname ( id_id , auxinput11_inname )
  character*256 , INTENT(OUT) :: auxinput11_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_auxinput11_inname: auxinput11_inname applies to all domains. First arg ignored.')
  ENDIF
  auxinput11_inname = trim(model_config_rec%auxinput11_inname)
  RETURN
END SUBROUTINE nl_get_auxinput11_inname
SUBROUTINE nl_get_history_interval_mo ( id_id , history_interval_mo )
  integer , INTENT(OUT) :: history_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_history_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  history_interval_mo = model_config_rec%history_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_history_interval_mo
SUBROUTINE nl_get_history_interval_d ( id_id , history_interval_d )
  integer , INTENT(OUT) :: history_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_history_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  history_interval_d = model_config_rec%history_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_history_interval_d
SUBROUTINE nl_get_history_interval_h ( id_id , history_interval_h )
  integer , INTENT(OUT) :: history_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_history_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  history_interval_h = model_config_rec%history_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_history_interval_h
SUBROUTINE nl_get_history_interval_m ( id_id , history_interval_m )
  integer , INTENT(OUT) :: history_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_history_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  history_interval_m = model_config_rec%history_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_history_interval_m
SUBROUTINE nl_get_history_interval_s ( id_id , history_interval_s )
  integer , INTENT(OUT) :: history_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_history_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  history_interval_s = model_config_rec%history_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_history_interval_s
SUBROUTINE nl_get_inputout_interval_mo ( id_id , inputout_interval_mo )
  integer , INTENT(OUT) :: inputout_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_inputout_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  inputout_interval_mo = model_config_rec%inputout_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_interval_mo
SUBROUTINE nl_get_inputout_interval_d ( id_id , inputout_interval_d )
  integer , INTENT(OUT) :: inputout_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_inputout_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  inputout_interval_d = model_config_rec%inputout_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_interval_d
SUBROUTINE nl_get_inputout_interval_h ( id_id , inputout_interval_h )
  integer , INTENT(OUT) :: inputout_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_inputout_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  inputout_interval_h = model_config_rec%inputout_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_interval_h
SUBROUTINE nl_get_inputout_interval_m ( id_id , inputout_interval_m )
  integer , INTENT(OUT) :: inputout_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_inputout_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  inputout_interval_m = model_config_rec%inputout_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_interval_m
SUBROUTINE nl_get_inputout_interval_s ( id_id , inputout_interval_s )
  integer , INTENT(OUT) :: inputout_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_inputout_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  inputout_interval_s = model_config_rec%inputout_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_interval_s
SUBROUTINE nl_get_inputout_interval ( id_id , inputout_interval )
  integer , INTENT(OUT) :: inputout_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_inputout_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  inputout_interval = model_config_rec%inputout_interval(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_interval
SUBROUTINE nl_get_auxhist1_interval_mo ( id_id , auxhist1_interval_mo )
  integer , INTENT(OUT) :: auxhist1_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist1_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist1_interval_mo = model_config_rec%auxhist1_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_interval_mo
SUBROUTINE nl_get_auxhist1_interval_d ( id_id , auxhist1_interval_d )
  integer , INTENT(OUT) :: auxhist1_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist1_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist1_interval_d = model_config_rec%auxhist1_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_interval_d
SUBROUTINE nl_get_auxhist1_interval_h ( id_id , auxhist1_interval_h )
  integer , INTENT(OUT) :: auxhist1_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist1_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist1_interval_h = model_config_rec%auxhist1_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_interval_h
SUBROUTINE nl_get_auxhist1_interval_m ( id_id , auxhist1_interval_m )
  integer , INTENT(OUT) :: auxhist1_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist1_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist1_interval_m = model_config_rec%auxhist1_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_interval_m
SUBROUTINE nl_get_auxhist1_interval_s ( id_id , auxhist1_interval_s )
  integer , INTENT(OUT) :: auxhist1_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist1_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist1_interval_s = model_config_rec%auxhist1_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_interval_s
SUBROUTINE nl_get_auxhist1_interval ( id_id , auxhist1_interval )
  integer , INTENT(OUT) :: auxhist1_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist1_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist1_interval = model_config_rec%auxhist1_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_interval
SUBROUTINE nl_get_auxhist2_interval_mo ( id_id , auxhist2_interval_mo )
  integer , INTENT(OUT) :: auxhist2_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist2_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist2_interval_mo = model_config_rec%auxhist2_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_interval_mo
SUBROUTINE nl_get_auxhist2_interval_d ( id_id , auxhist2_interval_d )
  integer , INTENT(OUT) :: auxhist2_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist2_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist2_interval_d = model_config_rec%auxhist2_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_interval_d
SUBROUTINE nl_get_auxhist2_interval_h ( id_id , auxhist2_interval_h )
  integer , INTENT(OUT) :: auxhist2_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist2_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist2_interval_h = model_config_rec%auxhist2_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_interval_h
SUBROUTINE nl_get_auxhist2_interval_m ( id_id , auxhist2_interval_m )
  integer , INTENT(OUT) :: auxhist2_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist2_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist2_interval_m = model_config_rec%auxhist2_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_interval_m
SUBROUTINE nl_get_auxhist2_interval_s ( id_id , auxhist2_interval_s )
  integer , INTENT(OUT) :: auxhist2_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist2_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist2_interval_s = model_config_rec%auxhist2_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_interval_s
SUBROUTINE nl_get_auxhist2_interval ( id_id , auxhist2_interval )
  integer , INTENT(OUT) :: auxhist2_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist2_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist2_interval = model_config_rec%auxhist2_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_interval
SUBROUTINE nl_get_auxhist3_interval_mo ( id_id , auxhist3_interval_mo )
  integer , INTENT(OUT) :: auxhist3_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist3_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist3_interval_mo = model_config_rec%auxhist3_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_interval_mo
SUBROUTINE nl_get_auxhist3_interval_d ( id_id , auxhist3_interval_d )
  integer , INTENT(OUT) :: auxhist3_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist3_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist3_interval_d = model_config_rec%auxhist3_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_interval_d
SUBROUTINE nl_get_auxhist3_interval_h ( id_id , auxhist3_interval_h )
  integer , INTENT(OUT) :: auxhist3_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist3_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist3_interval_h = model_config_rec%auxhist3_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_interval_h
SUBROUTINE nl_get_auxhist3_interval_m ( id_id , auxhist3_interval_m )
  integer , INTENT(OUT) :: auxhist3_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist3_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist3_interval_m = model_config_rec%auxhist3_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_interval_m
SUBROUTINE nl_get_auxhist3_interval_s ( id_id , auxhist3_interval_s )
  integer , INTENT(OUT) :: auxhist3_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist3_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist3_interval_s = model_config_rec%auxhist3_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_interval_s
SUBROUTINE nl_get_auxhist3_interval ( id_id , auxhist3_interval )
  integer , INTENT(OUT) :: auxhist3_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist3_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist3_interval = model_config_rec%auxhist3_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_interval
SUBROUTINE nl_get_auxhist4_interval_mo ( id_id , auxhist4_interval_mo )
  integer , INTENT(OUT) :: auxhist4_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist4_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist4_interval_mo = model_config_rec%auxhist4_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_interval_mo
SUBROUTINE nl_get_auxhist4_interval_d ( id_id , auxhist4_interval_d )
  integer , INTENT(OUT) :: auxhist4_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist4_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist4_interval_d = model_config_rec%auxhist4_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_interval_d
SUBROUTINE nl_get_auxhist4_interval_h ( id_id , auxhist4_interval_h )
  integer , INTENT(OUT) :: auxhist4_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist4_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist4_interval_h = model_config_rec%auxhist4_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_interval_h
SUBROUTINE nl_get_auxhist4_interval_m ( id_id , auxhist4_interval_m )
  integer , INTENT(OUT) :: auxhist4_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist4_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist4_interval_m = model_config_rec%auxhist4_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_interval_m
SUBROUTINE nl_get_auxhist4_interval_s ( id_id , auxhist4_interval_s )
  integer , INTENT(OUT) :: auxhist4_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist4_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist4_interval_s = model_config_rec%auxhist4_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_interval_s
SUBROUTINE nl_get_auxhist4_interval ( id_id , auxhist4_interval )
  integer , INTENT(OUT) :: auxhist4_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist4_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist4_interval = model_config_rec%auxhist4_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_interval
SUBROUTINE nl_get_auxhist5_interval_mo ( id_id , auxhist5_interval_mo )
  integer , INTENT(OUT) :: auxhist5_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist5_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist5_interval_mo = model_config_rec%auxhist5_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_interval_mo
SUBROUTINE nl_get_auxhist5_interval_d ( id_id , auxhist5_interval_d )
  integer , INTENT(OUT) :: auxhist5_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist5_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist5_interval_d = model_config_rec%auxhist5_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_interval_d
SUBROUTINE nl_get_auxhist5_interval_h ( id_id , auxhist5_interval_h )
  integer , INTENT(OUT) :: auxhist5_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist5_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist5_interval_h = model_config_rec%auxhist5_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_interval_h
SUBROUTINE nl_get_auxhist5_interval_m ( id_id , auxhist5_interval_m )
  integer , INTENT(OUT) :: auxhist5_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist5_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist5_interval_m = model_config_rec%auxhist5_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_interval_m
SUBROUTINE nl_get_auxhist5_interval_s ( id_id , auxhist5_interval_s )
  integer , INTENT(OUT) :: auxhist5_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist5_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist5_interval_s = model_config_rec%auxhist5_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_interval_s
SUBROUTINE nl_get_auxhist5_interval ( id_id , auxhist5_interval )
  integer , INTENT(OUT) :: auxhist5_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist5_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist5_interval = model_config_rec%auxhist5_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_interval
SUBROUTINE nl_get_auxhist6_interval_mo ( id_id , auxhist6_interval_mo )
  integer , INTENT(OUT) :: auxhist6_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist6_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist6_interval_mo = model_config_rec%auxhist6_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_interval_mo
SUBROUTINE nl_get_auxhist6_interval_d ( id_id , auxhist6_interval_d )
  integer , INTENT(OUT) :: auxhist6_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist6_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist6_interval_d = model_config_rec%auxhist6_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_interval_d
SUBROUTINE nl_get_auxhist6_interval_h ( id_id , auxhist6_interval_h )
  integer , INTENT(OUT) :: auxhist6_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist6_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist6_interval_h = model_config_rec%auxhist6_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_interval_h
SUBROUTINE nl_get_auxhist6_interval_m ( id_id , auxhist6_interval_m )
  integer , INTENT(OUT) :: auxhist6_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist6_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist6_interval_m = model_config_rec%auxhist6_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_interval_m
SUBROUTINE nl_get_auxhist6_interval_s ( id_id , auxhist6_interval_s )
  integer , INTENT(OUT) :: auxhist6_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist6_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist6_interval_s = model_config_rec%auxhist6_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_interval_s
SUBROUTINE nl_get_auxhist6_interval ( id_id , auxhist6_interval )
  integer , INTENT(OUT) :: auxhist6_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist6_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist6_interval = model_config_rec%auxhist6_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_interval
SUBROUTINE nl_get_auxhist7_interval_mo ( id_id , auxhist7_interval_mo )
  integer , INTENT(OUT) :: auxhist7_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist7_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist7_interval_mo = model_config_rec%auxhist7_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_interval_mo
SUBROUTINE nl_get_auxhist7_interval_d ( id_id , auxhist7_interval_d )
  integer , INTENT(OUT) :: auxhist7_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist7_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist7_interval_d = model_config_rec%auxhist7_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_interval_d
SUBROUTINE nl_get_auxhist7_interval_h ( id_id , auxhist7_interval_h )
  integer , INTENT(OUT) :: auxhist7_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist7_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist7_interval_h = model_config_rec%auxhist7_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_interval_h
SUBROUTINE nl_get_auxhist7_interval_m ( id_id , auxhist7_interval_m )
  integer , INTENT(OUT) :: auxhist7_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist7_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist7_interval_m = model_config_rec%auxhist7_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_interval_m
SUBROUTINE nl_get_auxhist7_interval_s ( id_id , auxhist7_interval_s )
  integer , INTENT(OUT) :: auxhist7_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist7_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist7_interval_s = model_config_rec%auxhist7_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_interval_s
SUBROUTINE nl_get_auxhist7_interval ( id_id , auxhist7_interval )
  integer , INTENT(OUT) :: auxhist7_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist7_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist7_interval = model_config_rec%auxhist7_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_interval
SUBROUTINE nl_get_auxhist8_interval_mo ( id_id , auxhist8_interval_mo )
  integer , INTENT(OUT) :: auxhist8_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist8_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist8_interval_mo = model_config_rec%auxhist8_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_interval_mo
SUBROUTINE nl_get_auxhist8_interval_d ( id_id , auxhist8_interval_d )
  integer , INTENT(OUT) :: auxhist8_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist8_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist8_interval_d = model_config_rec%auxhist8_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_interval_d
SUBROUTINE nl_get_auxhist8_interval_h ( id_id , auxhist8_interval_h )
  integer , INTENT(OUT) :: auxhist8_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist8_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist8_interval_h = model_config_rec%auxhist8_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_interval_h
SUBROUTINE nl_get_auxhist8_interval_m ( id_id , auxhist8_interval_m )
  integer , INTENT(OUT) :: auxhist8_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist8_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist8_interval_m = model_config_rec%auxhist8_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_interval_m
SUBROUTINE nl_get_auxhist8_interval_s ( id_id , auxhist8_interval_s )
  integer , INTENT(OUT) :: auxhist8_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist8_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist8_interval_s = model_config_rec%auxhist8_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_interval_s
SUBROUTINE nl_get_auxhist8_interval ( id_id , auxhist8_interval )
  integer , INTENT(OUT) :: auxhist8_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist8_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist8_interval = model_config_rec%auxhist8_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_interval
SUBROUTINE nl_get_auxhist9_interval_mo ( id_id , auxhist9_interval_mo )
  integer , INTENT(OUT) :: auxhist9_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist9_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist9_interval_mo = model_config_rec%auxhist9_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_interval_mo
SUBROUTINE nl_get_auxhist9_interval_d ( id_id , auxhist9_interval_d )
  integer , INTENT(OUT) :: auxhist9_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist9_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist9_interval_d = model_config_rec%auxhist9_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_interval_d
SUBROUTINE nl_get_auxhist9_interval_h ( id_id , auxhist9_interval_h )
  integer , INTENT(OUT) :: auxhist9_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist9_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist9_interval_h = model_config_rec%auxhist9_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_interval_h
SUBROUTINE nl_get_auxhist9_interval_m ( id_id , auxhist9_interval_m )
  integer , INTENT(OUT) :: auxhist9_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist9_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist9_interval_m = model_config_rec%auxhist9_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_interval_m
SUBROUTINE nl_get_auxhist9_interval_s ( id_id , auxhist9_interval_s )
  integer , INTENT(OUT) :: auxhist9_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist9_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist9_interval_s = model_config_rec%auxhist9_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_interval_s
SUBROUTINE nl_get_auxhist9_interval ( id_id , auxhist9_interval )
  integer , INTENT(OUT) :: auxhist9_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist9_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist9_interval = model_config_rec%auxhist9_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_interval
SUBROUTINE nl_get_auxhist10_interval_mo ( id_id , auxhist10_interval_mo )
  integer , INTENT(OUT) :: auxhist10_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist10_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist10_interval_mo = model_config_rec%auxhist10_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_interval_mo
SUBROUTINE nl_get_auxhist10_interval_d ( id_id , auxhist10_interval_d )
  integer , INTENT(OUT) :: auxhist10_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist10_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist10_interval_d = model_config_rec%auxhist10_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_interval_d
SUBROUTINE nl_get_auxhist10_interval_h ( id_id , auxhist10_interval_h )
  integer , INTENT(OUT) :: auxhist10_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist10_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist10_interval_h = model_config_rec%auxhist10_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_interval_h
SUBROUTINE nl_get_auxhist10_interval_m ( id_id , auxhist10_interval_m )
  integer , INTENT(OUT) :: auxhist10_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist10_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist10_interval_m = model_config_rec%auxhist10_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_interval_m
SUBROUTINE nl_get_auxhist10_interval_s ( id_id , auxhist10_interval_s )
  integer , INTENT(OUT) :: auxhist10_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist10_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist10_interval_s = model_config_rec%auxhist10_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_interval_s
SUBROUTINE nl_get_auxhist10_interval ( id_id , auxhist10_interval )
  integer , INTENT(OUT) :: auxhist10_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist10_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist10_interval = model_config_rec%auxhist10_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_interval
SUBROUTINE nl_get_auxhist11_interval_mo ( id_id , auxhist11_interval_mo )
  integer , INTENT(OUT) :: auxhist11_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist11_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist11_interval_mo = model_config_rec%auxhist11_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_interval_mo
SUBROUTINE nl_get_auxhist11_interval_d ( id_id , auxhist11_interval_d )
  integer , INTENT(OUT) :: auxhist11_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist11_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist11_interval_d = model_config_rec%auxhist11_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_interval_d
SUBROUTINE nl_get_auxhist11_interval_h ( id_id , auxhist11_interval_h )
  integer , INTENT(OUT) :: auxhist11_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist11_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist11_interval_h = model_config_rec%auxhist11_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_interval_h
SUBROUTINE nl_get_auxhist11_interval_m ( id_id , auxhist11_interval_m )
  integer , INTENT(OUT) :: auxhist11_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist11_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist11_interval_m = model_config_rec%auxhist11_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_interval_m
SUBROUTINE nl_get_auxhist11_interval_s ( id_id , auxhist11_interval_s )
  integer , INTENT(OUT) :: auxhist11_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist11_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist11_interval_s = model_config_rec%auxhist11_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_interval_s
SUBROUTINE nl_get_auxhist11_interval ( id_id , auxhist11_interval )
  integer , INTENT(OUT) :: auxhist11_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist11_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist11_interval = model_config_rec%auxhist11_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_interval
SUBROUTINE nl_get_auxinput1_interval_mo ( id_id , auxinput1_interval_mo )
  integer , INTENT(OUT) :: auxinput1_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput1_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput1_interval_mo = model_config_rec%auxinput1_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_interval_mo
SUBROUTINE nl_get_auxinput1_interval_d ( id_id , auxinput1_interval_d )
  integer , INTENT(OUT) :: auxinput1_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput1_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput1_interval_d = model_config_rec%auxinput1_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_interval_d
SUBROUTINE nl_get_auxinput1_interval_h ( id_id , auxinput1_interval_h )
  integer , INTENT(OUT) :: auxinput1_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput1_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput1_interval_h = model_config_rec%auxinput1_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_interval_h
SUBROUTINE nl_get_auxinput1_interval_m ( id_id , auxinput1_interval_m )
  integer , INTENT(OUT) :: auxinput1_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput1_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput1_interval_m = model_config_rec%auxinput1_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_interval_m
SUBROUTINE nl_get_auxinput1_interval_s ( id_id , auxinput1_interval_s )
  integer , INTENT(OUT) :: auxinput1_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput1_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput1_interval_s = model_config_rec%auxinput1_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_interval_s
SUBROUTINE nl_get_auxinput1_interval ( id_id , auxinput1_interval )
  integer , INTENT(OUT) :: auxinput1_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput1_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput1_interval = model_config_rec%auxinput1_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_interval
SUBROUTINE nl_get_auxinput2_interval_mo ( id_id , auxinput2_interval_mo )
  integer , INTENT(OUT) :: auxinput2_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput2_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput2_interval_mo = model_config_rec%auxinput2_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_interval_mo
SUBROUTINE nl_get_auxinput2_interval_d ( id_id , auxinput2_interval_d )
  integer , INTENT(OUT) :: auxinput2_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput2_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput2_interval_d = model_config_rec%auxinput2_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_interval_d
SUBROUTINE nl_get_auxinput2_interval_h ( id_id , auxinput2_interval_h )
  integer , INTENT(OUT) :: auxinput2_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput2_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput2_interval_h = model_config_rec%auxinput2_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_interval_h
SUBROUTINE nl_get_auxinput2_interval_m ( id_id , auxinput2_interval_m )
  integer , INTENT(OUT) :: auxinput2_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput2_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput2_interval_m = model_config_rec%auxinput2_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_interval_m
SUBROUTINE nl_get_auxinput2_interval_s ( id_id , auxinput2_interval_s )
  integer , INTENT(OUT) :: auxinput2_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput2_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput2_interval_s = model_config_rec%auxinput2_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_interval_s
SUBROUTINE nl_get_auxinput2_interval ( id_id , auxinput2_interval )
  integer , INTENT(OUT) :: auxinput2_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput2_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput2_interval = model_config_rec%auxinput2_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_interval
SUBROUTINE nl_get_auxinput3_interval_mo ( id_id , auxinput3_interval_mo )
  integer , INTENT(OUT) :: auxinput3_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput3_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput3_interval_mo = model_config_rec%auxinput3_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_interval_mo
SUBROUTINE nl_get_auxinput3_interval_d ( id_id , auxinput3_interval_d )
  integer , INTENT(OUT) :: auxinput3_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput3_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput3_interval_d = model_config_rec%auxinput3_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_interval_d
SUBROUTINE nl_get_auxinput3_interval_h ( id_id , auxinput3_interval_h )
  integer , INTENT(OUT) :: auxinput3_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput3_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput3_interval_h = model_config_rec%auxinput3_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_interval_h
SUBROUTINE nl_get_auxinput3_interval_m ( id_id , auxinput3_interval_m )
  integer , INTENT(OUT) :: auxinput3_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput3_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput3_interval_m = model_config_rec%auxinput3_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_interval_m
SUBROUTINE nl_get_auxinput3_interval_s ( id_id , auxinput3_interval_s )
  integer , INTENT(OUT) :: auxinput3_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput3_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput3_interval_s = model_config_rec%auxinput3_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_interval_s
SUBROUTINE nl_get_auxinput3_interval ( id_id , auxinput3_interval )
  integer , INTENT(OUT) :: auxinput3_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput3_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput3_interval = model_config_rec%auxinput3_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_interval
SUBROUTINE nl_get_auxinput4_interval_mo ( id_id , auxinput4_interval_mo )
  integer , INTENT(OUT) :: auxinput4_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput4_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput4_interval_mo = model_config_rec%auxinput4_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_interval_mo
SUBROUTINE nl_get_auxinput4_interval_d ( id_id , auxinput4_interval_d )
  integer , INTENT(OUT) :: auxinput4_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput4_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput4_interval_d = model_config_rec%auxinput4_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_interval_d
SUBROUTINE nl_get_auxinput4_interval_h ( id_id , auxinput4_interval_h )
  integer , INTENT(OUT) :: auxinput4_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput4_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput4_interval_h = model_config_rec%auxinput4_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_interval_h
SUBROUTINE nl_get_auxinput4_interval_m ( id_id , auxinput4_interval_m )
  integer , INTENT(OUT) :: auxinput4_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput4_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput4_interval_m = model_config_rec%auxinput4_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_interval_m
SUBROUTINE nl_get_auxinput4_interval_s ( id_id , auxinput4_interval_s )
  integer , INTENT(OUT) :: auxinput4_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput4_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput4_interval_s = model_config_rec%auxinput4_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_interval_s
SUBROUTINE nl_get_auxinput4_interval ( id_id , auxinput4_interval )
  integer , INTENT(OUT) :: auxinput4_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput4_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput4_interval = model_config_rec%auxinput4_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_interval
SUBROUTINE nl_get_auxinput5_interval_mo ( id_id , auxinput5_interval_mo )
  integer , INTENT(OUT) :: auxinput5_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput5_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput5_interval_mo = model_config_rec%auxinput5_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_interval_mo
SUBROUTINE nl_get_auxinput5_interval_d ( id_id , auxinput5_interval_d )
  integer , INTENT(OUT) :: auxinput5_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput5_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput5_interval_d = model_config_rec%auxinput5_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_interval_d
SUBROUTINE nl_get_auxinput5_interval_h ( id_id , auxinput5_interval_h )
  integer , INTENT(OUT) :: auxinput5_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput5_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput5_interval_h = model_config_rec%auxinput5_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_interval_h
SUBROUTINE nl_get_auxinput5_interval_m ( id_id , auxinput5_interval_m )
  integer , INTENT(OUT) :: auxinput5_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput5_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput5_interval_m = model_config_rec%auxinput5_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_interval_m
SUBROUTINE nl_get_auxinput5_interval_s ( id_id , auxinput5_interval_s )
  integer , INTENT(OUT) :: auxinput5_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput5_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput5_interval_s = model_config_rec%auxinput5_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_interval_s
SUBROUTINE nl_get_auxinput5_interval ( id_id , auxinput5_interval )
  integer , INTENT(OUT) :: auxinput5_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput5_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput5_interval = model_config_rec%auxinput5_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_interval
SUBROUTINE nl_get_auxinput6_interval_mo ( id_id , auxinput6_interval_mo )
  integer , INTENT(OUT) :: auxinput6_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput6_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput6_interval_mo = model_config_rec%auxinput6_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_interval_mo
SUBROUTINE nl_get_auxinput6_interval_d ( id_id , auxinput6_interval_d )
  integer , INTENT(OUT) :: auxinput6_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput6_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput6_interval_d = model_config_rec%auxinput6_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_interval_d
SUBROUTINE nl_get_auxinput6_interval_h ( id_id , auxinput6_interval_h )
  integer , INTENT(OUT) :: auxinput6_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput6_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput6_interval_h = model_config_rec%auxinput6_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_interval_h
SUBROUTINE nl_get_auxinput6_interval_m ( id_id , auxinput6_interval_m )
  integer , INTENT(OUT) :: auxinput6_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput6_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput6_interval_m = model_config_rec%auxinput6_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_interval_m
SUBROUTINE nl_get_auxinput6_interval_s ( id_id , auxinput6_interval_s )
  integer , INTENT(OUT) :: auxinput6_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput6_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput6_interval_s = model_config_rec%auxinput6_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_interval_s
SUBROUTINE nl_get_auxinput6_interval ( id_id , auxinput6_interval )
  integer , INTENT(OUT) :: auxinput6_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput6_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput6_interval = model_config_rec%auxinput6_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_interval
SUBROUTINE nl_get_auxinput7_interval_mo ( id_id , auxinput7_interval_mo )
  integer , INTENT(OUT) :: auxinput7_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput7_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput7_interval_mo = model_config_rec%auxinput7_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_interval_mo
SUBROUTINE nl_get_auxinput7_interval_d ( id_id , auxinput7_interval_d )
  integer , INTENT(OUT) :: auxinput7_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput7_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput7_interval_d = model_config_rec%auxinput7_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_interval_d
SUBROUTINE nl_get_auxinput7_interval_h ( id_id , auxinput7_interval_h )
  integer , INTENT(OUT) :: auxinput7_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput7_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput7_interval_h = model_config_rec%auxinput7_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_interval_h
SUBROUTINE nl_get_auxinput7_interval_m ( id_id , auxinput7_interval_m )
  integer , INTENT(OUT) :: auxinput7_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput7_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput7_interval_m = model_config_rec%auxinput7_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_interval_m
SUBROUTINE nl_get_auxinput7_interval_s ( id_id , auxinput7_interval_s )
  integer , INTENT(OUT) :: auxinput7_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput7_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput7_interval_s = model_config_rec%auxinput7_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_interval_s
SUBROUTINE nl_get_auxinput7_interval ( id_id , auxinput7_interval )
  integer , INTENT(OUT) :: auxinput7_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput7_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput7_interval = model_config_rec%auxinput7_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_interval
SUBROUTINE nl_get_auxinput8_interval_mo ( id_id , auxinput8_interval_mo )
  integer , INTENT(OUT) :: auxinput8_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput8_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput8_interval_mo = model_config_rec%auxinput8_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_interval_mo
SUBROUTINE nl_get_auxinput8_interval_d ( id_id , auxinput8_interval_d )
  integer , INTENT(OUT) :: auxinput8_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput8_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput8_interval_d = model_config_rec%auxinput8_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_interval_d
SUBROUTINE nl_get_auxinput8_interval_h ( id_id , auxinput8_interval_h )
  integer , INTENT(OUT) :: auxinput8_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput8_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput8_interval_h = model_config_rec%auxinput8_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_interval_h
SUBROUTINE nl_get_auxinput8_interval_m ( id_id , auxinput8_interval_m )
  integer , INTENT(OUT) :: auxinput8_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput8_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput8_interval_m = model_config_rec%auxinput8_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_interval_m
SUBROUTINE nl_get_auxinput8_interval_s ( id_id , auxinput8_interval_s )
  integer , INTENT(OUT) :: auxinput8_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput8_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput8_interval_s = model_config_rec%auxinput8_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_interval_s
SUBROUTINE nl_get_auxinput8_interval ( id_id , auxinput8_interval )
  integer , INTENT(OUT) :: auxinput8_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput8_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput8_interval = model_config_rec%auxinput8_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_interval
SUBROUTINE nl_get_auxinput9_interval_mo ( id_id , auxinput9_interval_mo )
  integer , INTENT(OUT) :: auxinput9_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput9_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput9_interval_mo = model_config_rec%auxinput9_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput9_interval_mo
SUBROUTINE nl_get_auxinput9_interval_d ( id_id , auxinput9_interval_d )
  integer , INTENT(OUT) :: auxinput9_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput9_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput9_interval_d = model_config_rec%auxinput9_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput9_interval_d
SUBROUTINE nl_get_auxinput9_interval_h ( id_id , auxinput9_interval_h )
  integer , INTENT(OUT) :: auxinput9_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput9_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput9_interval_h = model_config_rec%auxinput9_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput9_interval_h
SUBROUTINE nl_get_auxinput9_interval_m ( id_id , auxinput9_interval_m )
  integer , INTENT(OUT) :: auxinput9_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput9_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput9_interval_m = model_config_rec%auxinput9_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput9_interval_m
SUBROUTINE nl_get_auxinput9_interval_s ( id_id , auxinput9_interval_s )
  integer , INTENT(OUT) :: auxinput9_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput9_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput9_interval_s = model_config_rec%auxinput9_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput9_interval_s
SUBROUTINE nl_get_auxinput9_interval ( id_id , auxinput9_interval )
  integer , INTENT(OUT) :: auxinput9_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput9_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput9_interval = model_config_rec%auxinput9_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput9_interval
SUBROUTINE nl_get_gfdda_interval_mo ( id_id , gfdda_interval_mo )
  integer , INTENT(OUT) :: gfdda_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_gfdda_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  gfdda_interval_mo = model_config_rec%gfdda_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_gfdda_interval_mo
SUBROUTINE nl_get_gfdda_interval_d ( id_id , gfdda_interval_d )
  integer , INTENT(OUT) :: gfdda_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_gfdda_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  gfdda_interval_d = model_config_rec%gfdda_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_gfdda_interval_d
SUBROUTINE nl_get_gfdda_interval_h ( id_id , gfdda_interval_h )
  integer , INTENT(OUT) :: gfdda_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_gfdda_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  gfdda_interval_h = model_config_rec%gfdda_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_gfdda_interval_h
SUBROUTINE nl_get_gfdda_interval_m ( id_id , gfdda_interval_m )
  integer , INTENT(OUT) :: gfdda_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_gfdda_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  gfdda_interval_m = model_config_rec%gfdda_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_gfdda_interval_m
SUBROUTINE nl_get_gfdda_interval_s ( id_id , gfdda_interval_s )
  integer , INTENT(OUT) :: gfdda_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_gfdda_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  gfdda_interval_s = model_config_rec%gfdda_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_gfdda_interval_s
SUBROUTINE nl_get_gfdda_interval ( id_id , gfdda_interval )
  integer , INTENT(OUT) :: gfdda_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_gfdda_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  gfdda_interval = model_config_rec%gfdda_interval(id_id)
  RETURN
END SUBROUTINE nl_get_gfdda_interval
SUBROUTINE nl_get_auxinput11_interval_mo ( id_id , auxinput11_interval_mo )
  integer , INTENT(OUT) :: auxinput11_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput11_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput11_interval_mo = model_config_rec%auxinput11_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_interval_mo
SUBROUTINE nl_get_auxinput11_interval_d ( id_id , auxinput11_interval_d )
  integer , INTENT(OUT) :: auxinput11_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput11_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput11_interval_d = model_config_rec%auxinput11_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_interval_d
SUBROUTINE nl_get_auxinput11_interval_h ( id_id , auxinput11_interval_h )
  integer , INTENT(OUT) :: auxinput11_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput11_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput11_interval_h = model_config_rec%auxinput11_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_interval_h
SUBROUTINE nl_get_auxinput11_interval_m ( id_id , auxinput11_interval_m )
  integer , INTENT(OUT) :: auxinput11_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput11_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput11_interval_m = model_config_rec%auxinput11_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_interval_m
SUBROUTINE nl_get_auxinput11_interval_s ( id_id , auxinput11_interval_s )
  integer , INTENT(OUT) :: auxinput11_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput11_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput11_interval_s = model_config_rec%auxinput11_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_interval_s
SUBROUTINE nl_get_auxinput11_interval ( id_id , auxinput11_interval )
  integer , INTENT(OUT) :: auxinput11_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput11_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput11_interval = model_config_rec%auxinput11_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_interval
SUBROUTINE nl_get_restart_interval_mo ( id_id , restart_interval_mo )
  integer , INTENT(OUT) :: restart_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_restart_interval_mo: restart_interval_mo applies to all domains. First arg ignored.')
  ENDIF
  restart_interval_mo = model_config_rec%restart_interval_mo
  RETURN
END SUBROUTINE nl_get_restart_interval_mo
SUBROUTINE nl_get_restart_interval_d ( id_id , restart_interval_d )
  integer , INTENT(OUT) :: restart_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_restart_interval_d: restart_interval_d applies to all domains. First arg ignored.')
  ENDIF
  restart_interval_d = model_config_rec%restart_interval_d
  RETURN
END SUBROUTINE nl_get_restart_interval_d
SUBROUTINE nl_get_restart_interval_h ( id_id , restart_interval_h )
  integer , INTENT(OUT) :: restart_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_restart_interval_h: restart_interval_h applies to all domains. First arg ignored.')
  ENDIF
  restart_interval_h = model_config_rec%restart_interval_h
  RETURN
END SUBROUTINE nl_get_restart_interval_h
SUBROUTINE nl_get_restart_interval_m ( id_id , restart_interval_m )
  integer , INTENT(OUT) :: restart_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_restart_interval_m: restart_interval_m applies to all domains. First arg ignored.')
  ENDIF
  restart_interval_m = model_config_rec%restart_interval_m
  RETURN
END SUBROUTINE nl_get_restart_interval_m
SUBROUTINE nl_get_restart_interval_s ( id_id , restart_interval_s )
  integer , INTENT(OUT) :: restart_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_restart_interval_s: restart_interval_s applies to all domains. First arg ignored.')
  ENDIF
  restart_interval_s = model_config_rec%restart_interval_s
  RETURN
END SUBROUTINE nl_get_restart_interval_s
SUBROUTINE nl_get_history_begin_y ( id_id , history_begin_y )
  integer , INTENT(OUT) :: history_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_history_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  history_begin_y = model_config_rec%history_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_history_begin_y
SUBROUTINE nl_get_history_begin_mo ( id_id , history_begin_mo )
  integer , INTENT(OUT) :: history_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_history_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  history_begin_mo = model_config_rec%history_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_history_begin_mo
SUBROUTINE nl_get_history_begin_d ( id_id , history_begin_d )
  integer , INTENT(OUT) :: history_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_history_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  history_begin_d = model_config_rec%history_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_history_begin_d
SUBROUTINE nl_get_history_begin_h ( id_id , history_begin_h )
  integer , INTENT(OUT) :: history_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_history_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  history_begin_h = model_config_rec%history_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_history_begin_h
SUBROUTINE nl_get_history_begin_m ( id_id , history_begin_m )
  integer , INTENT(OUT) :: history_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_history_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  history_begin_m = model_config_rec%history_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_history_begin_m
SUBROUTINE nl_get_history_begin_s ( id_id , history_begin_s )
  integer , INTENT(OUT) :: history_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_history_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  history_begin_s = model_config_rec%history_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_history_begin_s
SUBROUTINE nl_get_inputout_begin_y ( id_id , inputout_begin_y )
  integer , INTENT(OUT) :: inputout_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_inputout_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  inputout_begin_y = model_config_rec%inputout_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_begin_y
SUBROUTINE nl_get_inputout_begin_mo ( id_id , inputout_begin_mo )
  integer , INTENT(OUT) :: inputout_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_inputout_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  inputout_begin_mo = model_config_rec%inputout_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_begin_mo
SUBROUTINE nl_get_inputout_begin_d ( id_id , inputout_begin_d )
  integer , INTENT(OUT) :: inputout_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_inputout_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  inputout_begin_d = model_config_rec%inputout_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_begin_d
SUBROUTINE nl_get_inputout_begin_h ( id_id , inputout_begin_h )
  integer , INTENT(OUT) :: inputout_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_inputout_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  inputout_begin_h = model_config_rec%inputout_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_begin_h
SUBROUTINE nl_get_inputout_begin_m ( id_id , inputout_begin_m )
  integer , INTENT(OUT) :: inputout_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_inputout_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  inputout_begin_m = model_config_rec%inputout_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_begin_m
SUBROUTINE nl_get_inputout_begin_s ( id_id , inputout_begin_s )
  integer , INTENT(OUT) :: inputout_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_inputout_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  inputout_begin_s = model_config_rec%inputout_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_begin_s
SUBROUTINE nl_get_auxhist1_begin_y ( id_id , auxhist1_begin_y )
  integer , INTENT(OUT) :: auxhist1_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist1_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist1_begin_y = model_config_rec%auxhist1_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_begin_y
SUBROUTINE nl_get_auxhist1_begin_mo ( id_id , auxhist1_begin_mo )
  integer , INTENT(OUT) :: auxhist1_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist1_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist1_begin_mo = model_config_rec%auxhist1_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_begin_mo
SUBROUTINE nl_get_auxhist1_begin_d ( id_id , auxhist1_begin_d )
  integer , INTENT(OUT) :: auxhist1_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist1_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist1_begin_d = model_config_rec%auxhist1_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_begin_d
SUBROUTINE nl_get_auxhist1_begin_h ( id_id , auxhist1_begin_h )
  integer , INTENT(OUT) :: auxhist1_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist1_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist1_begin_h = model_config_rec%auxhist1_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_begin_h
SUBROUTINE nl_get_auxhist1_begin_m ( id_id , auxhist1_begin_m )
  integer , INTENT(OUT) :: auxhist1_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist1_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist1_begin_m = model_config_rec%auxhist1_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_begin_m
SUBROUTINE nl_get_auxhist1_begin_s ( id_id , auxhist1_begin_s )
  integer , INTENT(OUT) :: auxhist1_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist1_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist1_begin_s = model_config_rec%auxhist1_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_begin_s
SUBROUTINE nl_get_auxhist2_begin_y ( id_id , auxhist2_begin_y )
  integer , INTENT(OUT) :: auxhist2_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist2_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist2_begin_y = model_config_rec%auxhist2_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_begin_y
SUBROUTINE nl_get_auxhist2_begin_mo ( id_id , auxhist2_begin_mo )
  integer , INTENT(OUT) :: auxhist2_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist2_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist2_begin_mo = model_config_rec%auxhist2_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_begin_mo
SUBROUTINE nl_get_auxhist2_begin_d ( id_id , auxhist2_begin_d )
  integer , INTENT(OUT) :: auxhist2_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist2_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist2_begin_d = model_config_rec%auxhist2_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_begin_d
SUBROUTINE nl_get_auxhist2_begin_h ( id_id , auxhist2_begin_h )
  integer , INTENT(OUT) :: auxhist2_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist2_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist2_begin_h = model_config_rec%auxhist2_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_begin_h
SUBROUTINE nl_get_auxhist2_begin_m ( id_id , auxhist2_begin_m )
  integer , INTENT(OUT) :: auxhist2_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist2_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist2_begin_m = model_config_rec%auxhist2_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_begin_m
SUBROUTINE nl_get_auxhist2_begin_s ( id_id , auxhist2_begin_s )
  integer , INTENT(OUT) :: auxhist2_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist2_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist2_begin_s = model_config_rec%auxhist2_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_begin_s
SUBROUTINE nl_get_auxhist3_begin_y ( id_id , auxhist3_begin_y )
  integer , INTENT(OUT) :: auxhist3_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist3_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist3_begin_y = model_config_rec%auxhist3_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_begin_y
SUBROUTINE nl_get_auxhist3_begin_mo ( id_id , auxhist3_begin_mo )
  integer , INTENT(OUT) :: auxhist3_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist3_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist3_begin_mo = model_config_rec%auxhist3_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_begin_mo
SUBROUTINE nl_get_auxhist3_begin_d ( id_id , auxhist3_begin_d )
  integer , INTENT(OUT) :: auxhist3_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist3_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist3_begin_d = model_config_rec%auxhist3_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_begin_d
SUBROUTINE nl_get_auxhist3_begin_h ( id_id , auxhist3_begin_h )
  integer , INTENT(OUT) :: auxhist3_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist3_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist3_begin_h = model_config_rec%auxhist3_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_begin_h
SUBROUTINE nl_get_auxhist3_begin_m ( id_id , auxhist3_begin_m )
  integer , INTENT(OUT) :: auxhist3_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist3_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist3_begin_m = model_config_rec%auxhist3_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_begin_m
SUBROUTINE nl_get_auxhist3_begin_s ( id_id , auxhist3_begin_s )
  integer , INTENT(OUT) :: auxhist3_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist3_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist3_begin_s = model_config_rec%auxhist3_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_begin_s
SUBROUTINE nl_get_auxhist4_begin_y ( id_id , auxhist4_begin_y )
  integer , INTENT(OUT) :: auxhist4_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist4_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist4_begin_y = model_config_rec%auxhist4_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_begin_y
SUBROUTINE nl_get_auxhist4_begin_mo ( id_id , auxhist4_begin_mo )
  integer , INTENT(OUT) :: auxhist4_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist4_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist4_begin_mo = model_config_rec%auxhist4_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_begin_mo
SUBROUTINE nl_get_auxhist4_begin_d ( id_id , auxhist4_begin_d )
  integer , INTENT(OUT) :: auxhist4_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist4_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist4_begin_d = model_config_rec%auxhist4_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_begin_d
SUBROUTINE nl_get_auxhist4_begin_h ( id_id , auxhist4_begin_h )
  integer , INTENT(OUT) :: auxhist4_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist4_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist4_begin_h = model_config_rec%auxhist4_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_begin_h
SUBROUTINE nl_get_auxhist4_begin_m ( id_id , auxhist4_begin_m )
  integer , INTENT(OUT) :: auxhist4_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist4_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist4_begin_m = model_config_rec%auxhist4_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_begin_m
SUBROUTINE nl_get_auxhist4_begin_s ( id_id , auxhist4_begin_s )
  integer , INTENT(OUT) :: auxhist4_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist4_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist4_begin_s = model_config_rec%auxhist4_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_begin_s
SUBROUTINE nl_get_auxhist5_begin_y ( id_id , auxhist5_begin_y )
  integer , INTENT(OUT) :: auxhist5_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist5_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist5_begin_y = model_config_rec%auxhist5_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_begin_y
SUBROUTINE nl_get_auxhist5_begin_mo ( id_id , auxhist5_begin_mo )
  integer , INTENT(OUT) :: auxhist5_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist5_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist5_begin_mo = model_config_rec%auxhist5_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_begin_mo
SUBROUTINE nl_get_auxhist5_begin_d ( id_id , auxhist5_begin_d )
  integer , INTENT(OUT) :: auxhist5_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist5_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist5_begin_d = model_config_rec%auxhist5_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_begin_d
SUBROUTINE nl_get_auxhist5_begin_h ( id_id , auxhist5_begin_h )
  integer , INTENT(OUT) :: auxhist5_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist5_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist5_begin_h = model_config_rec%auxhist5_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_begin_h
SUBROUTINE nl_get_auxhist5_begin_m ( id_id , auxhist5_begin_m )
  integer , INTENT(OUT) :: auxhist5_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist5_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist5_begin_m = model_config_rec%auxhist5_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_begin_m
SUBROUTINE nl_get_auxhist5_begin_s ( id_id , auxhist5_begin_s )
  integer , INTENT(OUT) :: auxhist5_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist5_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist5_begin_s = model_config_rec%auxhist5_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_begin_s
SUBROUTINE nl_get_auxhist6_begin_y ( id_id , auxhist6_begin_y )
  integer , INTENT(OUT) :: auxhist6_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist6_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist6_begin_y = model_config_rec%auxhist6_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_begin_y
SUBROUTINE nl_get_auxhist6_begin_mo ( id_id , auxhist6_begin_mo )
  integer , INTENT(OUT) :: auxhist6_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist6_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist6_begin_mo = model_config_rec%auxhist6_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_begin_mo
SUBROUTINE nl_get_auxhist6_begin_d ( id_id , auxhist6_begin_d )
  integer , INTENT(OUT) :: auxhist6_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist6_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist6_begin_d = model_config_rec%auxhist6_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_begin_d
SUBROUTINE nl_get_auxhist6_begin_h ( id_id , auxhist6_begin_h )
  integer , INTENT(OUT) :: auxhist6_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist6_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist6_begin_h = model_config_rec%auxhist6_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_begin_h
SUBROUTINE nl_get_auxhist6_begin_m ( id_id , auxhist6_begin_m )
  integer , INTENT(OUT) :: auxhist6_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist6_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist6_begin_m = model_config_rec%auxhist6_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_begin_m
SUBROUTINE nl_get_auxhist6_begin_s ( id_id , auxhist6_begin_s )
  integer , INTENT(OUT) :: auxhist6_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist6_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist6_begin_s = model_config_rec%auxhist6_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_begin_s
SUBROUTINE nl_get_auxhist7_begin_y ( id_id , auxhist7_begin_y )
  integer , INTENT(OUT) :: auxhist7_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist7_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist7_begin_y = model_config_rec%auxhist7_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_begin_y
SUBROUTINE nl_get_auxhist7_begin_mo ( id_id , auxhist7_begin_mo )
  integer , INTENT(OUT) :: auxhist7_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist7_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist7_begin_mo = model_config_rec%auxhist7_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_begin_mo
SUBROUTINE nl_get_auxhist7_begin_d ( id_id , auxhist7_begin_d )
  integer , INTENT(OUT) :: auxhist7_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist7_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist7_begin_d = model_config_rec%auxhist7_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_begin_d
SUBROUTINE nl_get_auxhist7_begin_h ( id_id , auxhist7_begin_h )
  integer , INTENT(OUT) :: auxhist7_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist7_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist7_begin_h = model_config_rec%auxhist7_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_begin_h
SUBROUTINE nl_get_auxhist7_begin_m ( id_id , auxhist7_begin_m )
  integer , INTENT(OUT) :: auxhist7_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist7_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist7_begin_m = model_config_rec%auxhist7_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_begin_m
SUBROUTINE nl_get_auxhist7_begin_s ( id_id , auxhist7_begin_s )
  integer , INTENT(OUT) :: auxhist7_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist7_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist7_begin_s = model_config_rec%auxhist7_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_begin_s
SUBROUTINE nl_get_auxhist8_begin_y ( id_id , auxhist8_begin_y )
  integer , INTENT(OUT) :: auxhist8_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist8_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist8_begin_y = model_config_rec%auxhist8_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_begin_y
SUBROUTINE nl_get_auxhist8_begin_mo ( id_id , auxhist8_begin_mo )
  integer , INTENT(OUT) :: auxhist8_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist8_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist8_begin_mo = model_config_rec%auxhist8_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_begin_mo
SUBROUTINE nl_get_auxhist8_begin_d ( id_id , auxhist8_begin_d )
  integer , INTENT(OUT) :: auxhist8_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist8_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist8_begin_d = model_config_rec%auxhist8_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_begin_d
SUBROUTINE nl_get_auxhist8_begin_h ( id_id , auxhist8_begin_h )
  integer , INTENT(OUT) :: auxhist8_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist8_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist8_begin_h = model_config_rec%auxhist8_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_begin_h
SUBROUTINE nl_get_auxhist8_begin_m ( id_id , auxhist8_begin_m )
  integer , INTENT(OUT) :: auxhist8_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist8_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist8_begin_m = model_config_rec%auxhist8_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_begin_m
SUBROUTINE nl_get_auxhist8_begin_s ( id_id , auxhist8_begin_s )
  integer , INTENT(OUT) :: auxhist8_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist8_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist8_begin_s = model_config_rec%auxhist8_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_begin_s
SUBROUTINE nl_get_auxhist9_begin_y ( id_id , auxhist9_begin_y )
  integer , INTENT(OUT) :: auxhist9_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist9_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist9_begin_y = model_config_rec%auxhist9_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_begin_y
SUBROUTINE nl_get_auxhist9_begin_mo ( id_id , auxhist9_begin_mo )
  integer , INTENT(OUT) :: auxhist9_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist9_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist9_begin_mo = model_config_rec%auxhist9_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_begin_mo
SUBROUTINE nl_get_auxhist9_begin_d ( id_id , auxhist9_begin_d )
  integer , INTENT(OUT) :: auxhist9_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist9_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist9_begin_d = model_config_rec%auxhist9_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_begin_d
SUBROUTINE nl_get_auxhist9_begin_h ( id_id , auxhist9_begin_h )
  integer , INTENT(OUT) :: auxhist9_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist9_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist9_begin_h = model_config_rec%auxhist9_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_begin_h
SUBROUTINE nl_get_auxhist9_begin_m ( id_id , auxhist9_begin_m )
  integer , INTENT(OUT) :: auxhist9_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist9_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist9_begin_m = model_config_rec%auxhist9_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_begin_m
SUBROUTINE nl_get_auxhist9_begin_s ( id_id , auxhist9_begin_s )
  integer , INTENT(OUT) :: auxhist9_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist9_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist9_begin_s = model_config_rec%auxhist9_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_begin_s
SUBROUTINE nl_get_auxhist10_begin_y ( id_id , auxhist10_begin_y )
  integer , INTENT(OUT) :: auxhist10_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist10_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist10_begin_y = model_config_rec%auxhist10_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_begin_y
SUBROUTINE nl_get_auxhist10_begin_mo ( id_id , auxhist10_begin_mo )
  integer , INTENT(OUT) :: auxhist10_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist10_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist10_begin_mo = model_config_rec%auxhist10_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_begin_mo
SUBROUTINE nl_get_auxhist10_begin_d ( id_id , auxhist10_begin_d )
  integer , INTENT(OUT) :: auxhist10_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist10_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist10_begin_d = model_config_rec%auxhist10_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_begin_d
SUBROUTINE nl_get_auxhist10_begin_h ( id_id , auxhist10_begin_h )
  integer , INTENT(OUT) :: auxhist10_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist10_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist10_begin_h = model_config_rec%auxhist10_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_begin_h
SUBROUTINE nl_get_auxhist10_begin_m ( id_id , auxhist10_begin_m )
  integer , INTENT(OUT) :: auxhist10_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist10_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist10_begin_m = model_config_rec%auxhist10_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_begin_m
SUBROUTINE nl_get_auxhist10_begin_s ( id_id , auxhist10_begin_s )
  integer , INTENT(OUT) :: auxhist10_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist10_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist10_begin_s = model_config_rec%auxhist10_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_begin_s
SUBROUTINE nl_get_auxhist11_begin_y ( id_id , auxhist11_begin_y )
  integer , INTENT(OUT) :: auxhist11_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist11_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist11_begin_y = model_config_rec%auxhist11_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_begin_y
SUBROUTINE nl_get_auxhist11_begin_mo ( id_id , auxhist11_begin_mo )
  integer , INTENT(OUT) :: auxhist11_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist11_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist11_begin_mo = model_config_rec%auxhist11_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_begin_mo
SUBROUTINE nl_get_auxhist11_begin_d ( id_id , auxhist11_begin_d )
  integer , INTENT(OUT) :: auxhist11_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist11_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist11_begin_d = model_config_rec%auxhist11_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_begin_d
SUBROUTINE nl_get_auxhist11_begin_h ( id_id , auxhist11_begin_h )
  integer , INTENT(OUT) :: auxhist11_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist11_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist11_begin_h = model_config_rec%auxhist11_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_begin_h
SUBROUTINE nl_get_auxhist11_begin_m ( id_id , auxhist11_begin_m )
  integer , INTENT(OUT) :: auxhist11_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist11_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist11_begin_m = model_config_rec%auxhist11_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_begin_m
SUBROUTINE nl_get_auxhist11_begin_s ( id_id , auxhist11_begin_s )
  integer , INTENT(OUT) :: auxhist11_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist11_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist11_begin_s = model_config_rec%auxhist11_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_begin_s
SUBROUTINE nl_get_auxinput1_begin_y ( id_id , auxinput1_begin_y )
  integer , INTENT(OUT) :: auxinput1_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput1_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput1_begin_y = model_config_rec%auxinput1_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_begin_y
SUBROUTINE nl_get_auxinput1_begin_mo ( id_id , auxinput1_begin_mo )
  integer , INTENT(OUT) :: auxinput1_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput1_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput1_begin_mo = model_config_rec%auxinput1_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_begin_mo
SUBROUTINE nl_get_auxinput1_begin_d ( id_id , auxinput1_begin_d )
  integer , INTENT(OUT) :: auxinput1_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput1_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput1_begin_d = model_config_rec%auxinput1_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_begin_d
SUBROUTINE nl_get_auxinput1_begin_h ( id_id , auxinput1_begin_h )
  integer , INTENT(OUT) :: auxinput1_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput1_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput1_begin_h = model_config_rec%auxinput1_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_begin_h
SUBROUTINE nl_get_auxinput1_begin_m ( id_id , auxinput1_begin_m )
  integer , INTENT(OUT) :: auxinput1_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput1_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput1_begin_m = model_config_rec%auxinput1_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_begin_m
SUBROUTINE nl_get_auxinput1_begin_s ( id_id , auxinput1_begin_s )
  integer , INTENT(OUT) :: auxinput1_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput1_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput1_begin_s = model_config_rec%auxinput1_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_begin_s
SUBROUTINE nl_get_auxinput2_begin_y ( id_id , auxinput2_begin_y )
  integer , INTENT(OUT) :: auxinput2_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput2_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput2_begin_y = model_config_rec%auxinput2_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_begin_y
SUBROUTINE nl_get_auxinput2_begin_mo ( id_id , auxinput2_begin_mo )
  integer , INTENT(OUT) :: auxinput2_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput2_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput2_begin_mo = model_config_rec%auxinput2_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_begin_mo
SUBROUTINE nl_get_auxinput2_begin_d ( id_id , auxinput2_begin_d )
  integer , INTENT(OUT) :: auxinput2_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput2_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput2_begin_d = model_config_rec%auxinput2_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_begin_d
SUBROUTINE nl_get_auxinput2_begin_h ( id_id , auxinput2_begin_h )
  integer , INTENT(OUT) :: auxinput2_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput2_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput2_begin_h = model_config_rec%auxinput2_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_begin_h
SUBROUTINE nl_get_auxinput2_begin_m ( id_id , auxinput2_begin_m )
  integer , INTENT(OUT) :: auxinput2_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput2_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput2_begin_m = model_config_rec%auxinput2_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_begin_m
SUBROUTINE nl_get_auxinput2_begin_s ( id_id , auxinput2_begin_s )
  integer , INTENT(OUT) :: auxinput2_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput2_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput2_begin_s = model_config_rec%auxinput2_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_begin_s
SUBROUTINE nl_get_auxinput3_begin_y ( id_id , auxinput3_begin_y )
  integer , INTENT(OUT) :: auxinput3_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput3_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput3_begin_y = model_config_rec%auxinput3_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_begin_y
SUBROUTINE nl_get_auxinput3_begin_mo ( id_id , auxinput3_begin_mo )
  integer , INTENT(OUT) :: auxinput3_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput3_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput3_begin_mo = model_config_rec%auxinput3_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_begin_mo
SUBROUTINE nl_get_auxinput3_begin_d ( id_id , auxinput3_begin_d )
  integer , INTENT(OUT) :: auxinput3_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput3_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput3_begin_d = model_config_rec%auxinput3_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_begin_d
SUBROUTINE nl_get_auxinput3_begin_h ( id_id , auxinput3_begin_h )
  integer , INTENT(OUT) :: auxinput3_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput3_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput3_begin_h = model_config_rec%auxinput3_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_begin_h
SUBROUTINE nl_get_auxinput3_begin_m ( id_id , auxinput3_begin_m )
  integer , INTENT(OUT) :: auxinput3_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput3_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput3_begin_m = model_config_rec%auxinput3_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_begin_m
SUBROUTINE nl_get_auxinput3_begin_s ( id_id , auxinput3_begin_s )
  integer , INTENT(OUT) :: auxinput3_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput3_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput3_begin_s = model_config_rec%auxinput3_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_begin_s
SUBROUTINE nl_get_auxinput4_begin_y ( id_id , auxinput4_begin_y )
  integer , INTENT(OUT) :: auxinput4_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput4_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput4_begin_y = model_config_rec%auxinput4_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_begin_y
SUBROUTINE nl_get_auxinput4_begin_mo ( id_id , auxinput4_begin_mo )
  integer , INTENT(OUT) :: auxinput4_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput4_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput4_begin_mo = model_config_rec%auxinput4_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_begin_mo
SUBROUTINE nl_get_auxinput4_begin_d ( id_id , auxinput4_begin_d )
  integer , INTENT(OUT) :: auxinput4_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput4_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput4_begin_d = model_config_rec%auxinput4_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_begin_d
SUBROUTINE nl_get_auxinput4_begin_h ( id_id , auxinput4_begin_h )
  integer , INTENT(OUT) :: auxinput4_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput4_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput4_begin_h = model_config_rec%auxinput4_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_begin_h
SUBROUTINE nl_get_auxinput4_begin_m ( id_id , auxinput4_begin_m )
  integer , INTENT(OUT) :: auxinput4_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput4_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput4_begin_m = model_config_rec%auxinput4_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_begin_m
SUBROUTINE nl_get_auxinput4_begin_s ( id_id , auxinput4_begin_s )
  integer , INTENT(OUT) :: auxinput4_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput4_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput4_begin_s = model_config_rec%auxinput4_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_begin_s
SUBROUTINE nl_get_auxinput5_begin_y ( id_id , auxinput5_begin_y )
  integer , INTENT(OUT) :: auxinput5_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput5_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput5_begin_y = model_config_rec%auxinput5_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_begin_y
SUBROUTINE nl_get_auxinput5_begin_mo ( id_id , auxinput5_begin_mo )
  integer , INTENT(OUT) :: auxinput5_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput5_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput5_begin_mo = model_config_rec%auxinput5_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_begin_mo
SUBROUTINE nl_get_auxinput5_begin_d ( id_id , auxinput5_begin_d )
  integer , INTENT(OUT) :: auxinput5_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput5_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput5_begin_d = model_config_rec%auxinput5_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_begin_d
SUBROUTINE nl_get_auxinput5_begin_h ( id_id , auxinput5_begin_h )
  integer , INTENT(OUT) :: auxinput5_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput5_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput5_begin_h = model_config_rec%auxinput5_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_begin_h
SUBROUTINE nl_get_auxinput5_begin_m ( id_id , auxinput5_begin_m )
  integer , INTENT(OUT) :: auxinput5_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput5_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput5_begin_m = model_config_rec%auxinput5_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_begin_m
SUBROUTINE nl_get_auxinput5_begin_s ( id_id , auxinput5_begin_s )
  integer , INTENT(OUT) :: auxinput5_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput5_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput5_begin_s = model_config_rec%auxinput5_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_begin_s
SUBROUTINE nl_get_auxinput6_begin_y ( id_id , auxinput6_begin_y )
  integer , INTENT(OUT) :: auxinput6_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput6_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput6_begin_y = model_config_rec%auxinput6_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_begin_y
SUBROUTINE nl_get_auxinput6_begin_mo ( id_id , auxinput6_begin_mo )
  integer , INTENT(OUT) :: auxinput6_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput6_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput6_begin_mo = model_config_rec%auxinput6_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_begin_mo
SUBROUTINE nl_get_auxinput6_begin_d ( id_id , auxinput6_begin_d )
  integer , INTENT(OUT) :: auxinput6_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput6_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput6_begin_d = model_config_rec%auxinput6_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_begin_d
SUBROUTINE nl_get_auxinput6_begin_h ( id_id , auxinput6_begin_h )
  integer , INTENT(OUT) :: auxinput6_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput6_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput6_begin_h = model_config_rec%auxinput6_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_begin_h
SUBROUTINE nl_get_auxinput6_begin_m ( id_id , auxinput6_begin_m )
  integer , INTENT(OUT) :: auxinput6_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput6_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput6_begin_m = model_config_rec%auxinput6_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_begin_m
SUBROUTINE nl_get_auxinput6_begin_s ( id_id , auxinput6_begin_s )
  integer , INTENT(OUT) :: auxinput6_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput6_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput6_begin_s = model_config_rec%auxinput6_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_begin_s
SUBROUTINE nl_get_auxinput7_begin_y ( id_id , auxinput7_begin_y )
  integer , INTENT(OUT) :: auxinput7_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput7_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput7_begin_y = model_config_rec%auxinput7_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_begin_y
SUBROUTINE nl_get_auxinput7_begin_mo ( id_id , auxinput7_begin_mo )
  integer , INTENT(OUT) :: auxinput7_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput7_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput7_begin_mo = model_config_rec%auxinput7_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_begin_mo
SUBROUTINE nl_get_auxinput7_begin_d ( id_id , auxinput7_begin_d )
  integer , INTENT(OUT) :: auxinput7_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput7_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput7_begin_d = model_config_rec%auxinput7_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_begin_d
SUBROUTINE nl_get_auxinput7_begin_h ( id_id , auxinput7_begin_h )
  integer , INTENT(OUT) :: auxinput7_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput7_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput7_begin_h = model_config_rec%auxinput7_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_begin_h
SUBROUTINE nl_get_auxinput7_begin_m ( id_id , auxinput7_begin_m )
  integer , INTENT(OUT) :: auxinput7_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput7_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput7_begin_m = model_config_rec%auxinput7_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_begin_m
SUBROUTINE nl_get_auxinput7_begin_s ( id_id , auxinput7_begin_s )
  integer , INTENT(OUT) :: auxinput7_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput7_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput7_begin_s = model_config_rec%auxinput7_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_begin_s
SUBROUTINE nl_get_auxinput8_begin_y ( id_id , auxinput8_begin_y )
  integer , INTENT(OUT) :: auxinput8_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput8_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput8_begin_y = model_config_rec%auxinput8_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_begin_y
SUBROUTINE nl_get_auxinput8_begin_mo ( id_id , auxinput8_begin_mo )
  integer , INTENT(OUT) :: auxinput8_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput8_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput8_begin_mo = model_config_rec%auxinput8_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_begin_mo
SUBROUTINE nl_get_auxinput8_begin_d ( id_id , auxinput8_begin_d )
  integer , INTENT(OUT) :: auxinput8_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput8_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput8_begin_d = model_config_rec%auxinput8_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_begin_d
SUBROUTINE nl_get_auxinput8_begin_h ( id_id , auxinput8_begin_h )
  integer , INTENT(OUT) :: auxinput8_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput8_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput8_begin_h = model_config_rec%auxinput8_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_begin_h
SUBROUTINE nl_get_auxinput8_begin_m ( id_id , auxinput8_begin_m )
  integer , INTENT(OUT) :: auxinput8_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput8_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput8_begin_m = model_config_rec%auxinput8_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_begin_m
SUBROUTINE nl_get_auxinput8_begin_s ( id_id , auxinput8_begin_s )
  integer , INTENT(OUT) :: auxinput8_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput8_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput8_begin_s = model_config_rec%auxinput8_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_begin_s
SUBROUTINE nl_get_auxinput9_begin_y ( id_id , auxinput9_begin_y )
  integer , INTENT(OUT) :: auxinput9_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput9_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput9_begin_y = model_config_rec%auxinput9_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput9_begin_y
SUBROUTINE nl_get_auxinput9_begin_mo ( id_id , auxinput9_begin_mo )
  integer , INTENT(OUT) :: auxinput9_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput9_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput9_begin_mo = model_config_rec%auxinput9_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput9_begin_mo
SUBROUTINE nl_get_auxinput9_begin_d ( id_id , auxinput9_begin_d )
  integer , INTENT(OUT) :: auxinput9_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput9_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput9_begin_d = model_config_rec%auxinput9_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput9_begin_d
SUBROUTINE nl_get_auxinput9_begin_h ( id_id , auxinput9_begin_h )
  integer , INTENT(OUT) :: auxinput9_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput9_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput9_begin_h = model_config_rec%auxinput9_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput9_begin_h
SUBROUTINE nl_get_auxinput9_begin_m ( id_id , auxinput9_begin_m )
  integer , INTENT(OUT) :: auxinput9_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput9_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput9_begin_m = model_config_rec%auxinput9_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput9_begin_m
SUBROUTINE nl_get_auxinput9_begin_s ( id_id , auxinput9_begin_s )
  integer , INTENT(OUT) :: auxinput9_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput9_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput9_begin_s = model_config_rec%auxinput9_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput9_begin_s
SUBROUTINE nl_get_gfdda_begin_y ( id_id , gfdda_begin_y )
  integer , INTENT(OUT) :: gfdda_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_gfdda_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  gfdda_begin_y = model_config_rec%gfdda_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_gfdda_begin_y
SUBROUTINE nl_get_gfdda_begin_mo ( id_id , gfdda_begin_mo )
  integer , INTENT(OUT) :: gfdda_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_gfdda_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  gfdda_begin_mo = model_config_rec%gfdda_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_gfdda_begin_mo
SUBROUTINE nl_get_gfdda_begin_d ( id_id , gfdda_begin_d )
  integer , INTENT(OUT) :: gfdda_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_gfdda_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  gfdda_begin_d = model_config_rec%gfdda_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_gfdda_begin_d
SUBROUTINE nl_get_gfdda_begin_h ( id_id , gfdda_begin_h )
  integer , INTENT(OUT) :: gfdda_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_gfdda_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  gfdda_begin_h = model_config_rec%gfdda_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_gfdda_begin_h
SUBROUTINE nl_get_gfdda_begin_m ( id_id , gfdda_begin_m )
  integer , INTENT(OUT) :: gfdda_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_gfdda_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  gfdda_begin_m = model_config_rec%gfdda_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_gfdda_begin_m
SUBROUTINE nl_get_gfdda_begin_s ( id_id , gfdda_begin_s )
  integer , INTENT(OUT) :: gfdda_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_gfdda_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  gfdda_begin_s = model_config_rec%gfdda_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_gfdda_begin_s
SUBROUTINE nl_get_auxinput11_begin_y ( id_id , auxinput11_begin_y )
  integer , INTENT(OUT) :: auxinput11_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput11_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput11_begin_y = model_config_rec%auxinput11_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_begin_y
SUBROUTINE nl_get_auxinput11_begin_mo ( id_id , auxinput11_begin_mo )
  integer , INTENT(OUT) :: auxinput11_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput11_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput11_begin_mo = model_config_rec%auxinput11_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_begin_mo
SUBROUTINE nl_get_auxinput11_begin_d ( id_id , auxinput11_begin_d )
  integer , INTENT(OUT) :: auxinput11_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput11_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput11_begin_d = model_config_rec%auxinput11_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_begin_d
SUBROUTINE nl_get_auxinput11_begin_h ( id_id , auxinput11_begin_h )
  integer , INTENT(OUT) :: auxinput11_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput11_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput11_begin_h = model_config_rec%auxinput11_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_begin_h
SUBROUTINE nl_get_auxinput11_begin_m ( id_id , auxinput11_begin_m )
  integer , INTENT(OUT) :: auxinput11_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput11_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput11_begin_m = model_config_rec%auxinput11_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_begin_m
SUBROUTINE nl_get_auxinput11_begin_s ( id_id , auxinput11_begin_s )
  integer , INTENT(OUT) :: auxinput11_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput11_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput11_begin_s = model_config_rec%auxinput11_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_begin_s
SUBROUTINE nl_get_restart_begin_y ( id_id , restart_begin_y )
  integer , INTENT(OUT) :: restart_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_restart_begin_y: restart_begin_y applies to all domains. First arg ignored.')
  ENDIF
  restart_begin_y = model_config_rec%restart_begin_y
  RETURN
END SUBROUTINE nl_get_restart_begin_y
SUBROUTINE nl_get_restart_begin_mo ( id_id , restart_begin_mo )
  integer , INTENT(OUT) :: restart_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_restart_begin_mo: restart_begin_mo applies to all domains. First arg ignored.')
  ENDIF
  restart_begin_mo = model_config_rec%restart_begin_mo
  RETURN
END SUBROUTINE nl_get_restart_begin_mo
SUBROUTINE nl_get_restart_begin_d ( id_id , restart_begin_d )
  integer , INTENT(OUT) :: restart_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_restart_begin_d: restart_begin_d applies to all domains. First arg ignored.')
  ENDIF
  restart_begin_d = model_config_rec%restart_begin_d
  RETURN
END SUBROUTINE nl_get_restart_begin_d
SUBROUTINE nl_get_restart_begin_h ( id_id , restart_begin_h )
  integer , INTENT(OUT) :: restart_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_restart_begin_h: restart_begin_h applies to all domains. First arg ignored.')
  ENDIF
  restart_begin_h = model_config_rec%restart_begin_h
  RETURN
END SUBROUTINE nl_get_restart_begin_h
SUBROUTINE nl_get_restart_begin_m ( id_id , restart_begin_m )
  integer , INTENT(OUT) :: restart_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_restart_begin_m: restart_begin_m applies to all domains. First arg ignored.')
  ENDIF
  restart_begin_m = model_config_rec%restart_begin_m
  RETURN
END SUBROUTINE nl_get_restart_begin_m
SUBROUTINE nl_get_restart_begin_s ( id_id , restart_begin_s )
  integer , INTENT(OUT) :: restart_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_restart_begin_s: restart_begin_s applies to all domains. First arg ignored.')
  ENDIF
  restart_begin_s = model_config_rec%restart_begin_s
  RETURN
END SUBROUTINE nl_get_restart_begin_s
SUBROUTINE nl_get_history_end_y ( id_id , history_end_y )
  integer , INTENT(OUT) :: history_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_history_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  history_end_y = model_config_rec%history_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_history_end_y
SUBROUTINE nl_get_history_end_mo ( id_id , history_end_mo )
  integer , INTENT(OUT) :: history_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_history_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  history_end_mo = model_config_rec%history_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_history_end_mo
SUBROUTINE nl_get_history_end_d ( id_id , history_end_d )
  integer , INTENT(OUT) :: history_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_history_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  history_end_d = model_config_rec%history_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_history_end_d
SUBROUTINE nl_get_history_end_h ( id_id , history_end_h )
  integer , INTENT(OUT) :: history_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_history_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  history_end_h = model_config_rec%history_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_history_end_h
SUBROUTINE nl_get_history_end_m ( id_id , history_end_m )
  integer , INTENT(OUT) :: history_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_history_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  history_end_m = model_config_rec%history_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_history_end_m
SUBROUTINE nl_get_history_end_s ( id_id , history_end_s )
  integer , INTENT(OUT) :: history_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_history_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  history_end_s = model_config_rec%history_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_history_end_s
SUBROUTINE nl_get_inputout_end_y ( id_id , inputout_end_y )
  integer , INTENT(OUT) :: inputout_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_inputout_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  inputout_end_y = model_config_rec%inputout_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_end_y
SUBROUTINE nl_get_inputout_end_mo ( id_id , inputout_end_mo )
  integer , INTENT(OUT) :: inputout_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_inputout_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  inputout_end_mo = model_config_rec%inputout_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_end_mo
SUBROUTINE nl_get_inputout_end_d ( id_id , inputout_end_d )
  integer , INTENT(OUT) :: inputout_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_inputout_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  inputout_end_d = model_config_rec%inputout_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_end_d
SUBROUTINE nl_get_inputout_end_h ( id_id , inputout_end_h )
  integer , INTENT(OUT) :: inputout_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_inputout_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  inputout_end_h = model_config_rec%inputout_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_end_h
SUBROUTINE nl_get_inputout_end_m ( id_id , inputout_end_m )
  integer , INTENT(OUT) :: inputout_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_inputout_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  inputout_end_m = model_config_rec%inputout_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_end_m
SUBROUTINE nl_get_inputout_end_s ( id_id , inputout_end_s )
  integer , INTENT(OUT) :: inputout_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_inputout_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  inputout_end_s = model_config_rec%inputout_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_end_s
SUBROUTINE nl_get_auxhist1_end_y ( id_id , auxhist1_end_y )
  integer , INTENT(OUT) :: auxhist1_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist1_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist1_end_y = model_config_rec%auxhist1_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_end_y
SUBROUTINE nl_get_auxhist1_end_mo ( id_id , auxhist1_end_mo )
  integer , INTENT(OUT) :: auxhist1_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist1_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist1_end_mo = model_config_rec%auxhist1_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_end_mo
SUBROUTINE nl_get_auxhist1_end_d ( id_id , auxhist1_end_d )
  integer , INTENT(OUT) :: auxhist1_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist1_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist1_end_d = model_config_rec%auxhist1_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_end_d
SUBROUTINE nl_get_auxhist1_end_h ( id_id , auxhist1_end_h )
  integer , INTENT(OUT) :: auxhist1_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist1_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist1_end_h = model_config_rec%auxhist1_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_end_h
SUBROUTINE nl_get_auxhist1_end_m ( id_id , auxhist1_end_m )
  integer , INTENT(OUT) :: auxhist1_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist1_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist1_end_m = model_config_rec%auxhist1_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_end_m
SUBROUTINE nl_get_auxhist1_end_s ( id_id , auxhist1_end_s )
  integer , INTENT(OUT) :: auxhist1_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist1_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist1_end_s = model_config_rec%auxhist1_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_end_s
SUBROUTINE nl_get_auxhist2_end_y ( id_id , auxhist2_end_y )
  integer , INTENT(OUT) :: auxhist2_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist2_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist2_end_y = model_config_rec%auxhist2_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_end_y
SUBROUTINE nl_get_auxhist2_end_mo ( id_id , auxhist2_end_mo )
  integer , INTENT(OUT) :: auxhist2_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist2_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist2_end_mo = model_config_rec%auxhist2_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_end_mo
SUBROUTINE nl_get_auxhist2_end_d ( id_id , auxhist2_end_d )
  integer , INTENT(OUT) :: auxhist2_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist2_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist2_end_d = model_config_rec%auxhist2_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_end_d
SUBROUTINE nl_get_auxhist2_end_h ( id_id , auxhist2_end_h )
  integer , INTENT(OUT) :: auxhist2_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist2_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist2_end_h = model_config_rec%auxhist2_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_end_h
SUBROUTINE nl_get_auxhist2_end_m ( id_id , auxhist2_end_m )
  integer , INTENT(OUT) :: auxhist2_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist2_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist2_end_m = model_config_rec%auxhist2_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_end_m
SUBROUTINE nl_get_auxhist2_end_s ( id_id , auxhist2_end_s )
  integer , INTENT(OUT) :: auxhist2_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist2_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist2_end_s = model_config_rec%auxhist2_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_end_s
SUBROUTINE nl_get_auxhist3_end_y ( id_id , auxhist3_end_y )
  integer , INTENT(OUT) :: auxhist3_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist3_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist3_end_y = model_config_rec%auxhist3_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_end_y
SUBROUTINE nl_get_auxhist3_end_mo ( id_id , auxhist3_end_mo )
  integer , INTENT(OUT) :: auxhist3_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist3_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist3_end_mo = model_config_rec%auxhist3_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_end_mo
SUBROUTINE nl_get_auxhist3_end_d ( id_id , auxhist3_end_d )
  integer , INTENT(OUT) :: auxhist3_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist3_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist3_end_d = model_config_rec%auxhist3_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_end_d
SUBROUTINE nl_get_auxhist3_end_h ( id_id , auxhist3_end_h )
  integer , INTENT(OUT) :: auxhist3_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist3_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist3_end_h = model_config_rec%auxhist3_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_end_h
SUBROUTINE nl_get_auxhist3_end_m ( id_id , auxhist3_end_m )
  integer , INTENT(OUT) :: auxhist3_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist3_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist3_end_m = model_config_rec%auxhist3_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_end_m
SUBROUTINE nl_get_auxhist3_end_s ( id_id , auxhist3_end_s )
  integer , INTENT(OUT) :: auxhist3_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist3_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist3_end_s = model_config_rec%auxhist3_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_end_s
SUBROUTINE nl_get_auxhist4_end_y ( id_id , auxhist4_end_y )
  integer , INTENT(OUT) :: auxhist4_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist4_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist4_end_y = model_config_rec%auxhist4_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_end_y
SUBROUTINE nl_get_auxhist4_end_mo ( id_id , auxhist4_end_mo )
  integer , INTENT(OUT) :: auxhist4_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist4_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist4_end_mo = model_config_rec%auxhist4_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_end_mo
SUBROUTINE nl_get_auxhist4_end_d ( id_id , auxhist4_end_d )
  integer , INTENT(OUT) :: auxhist4_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist4_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist4_end_d = model_config_rec%auxhist4_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_end_d
SUBROUTINE nl_get_auxhist4_end_h ( id_id , auxhist4_end_h )
  integer , INTENT(OUT) :: auxhist4_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist4_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist4_end_h = model_config_rec%auxhist4_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_end_h
SUBROUTINE nl_get_auxhist4_end_m ( id_id , auxhist4_end_m )
  integer , INTENT(OUT) :: auxhist4_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist4_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist4_end_m = model_config_rec%auxhist4_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_end_m
SUBROUTINE nl_get_auxhist4_end_s ( id_id , auxhist4_end_s )
  integer , INTENT(OUT) :: auxhist4_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist4_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist4_end_s = model_config_rec%auxhist4_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_end_s
SUBROUTINE nl_get_auxhist5_end_y ( id_id , auxhist5_end_y )
  integer , INTENT(OUT) :: auxhist5_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist5_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist5_end_y = model_config_rec%auxhist5_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_end_y
SUBROUTINE nl_get_auxhist5_end_mo ( id_id , auxhist5_end_mo )
  integer , INTENT(OUT) :: auxhist5_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist5_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist5_end_mo = model_config_rec%auxhist5_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_end_mo
SUBROUTINE nl_get_auxhist5_end_d ( id_id , auxhist5_end_d )
  integer , INTENT(OUT) :: auxhist5_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist5_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist5_end_d = model_config_rec%auxhist5_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_end_d
SUBROUTINE nl_get_auxhist5_end_h ( id_id , auxhist5_end_h )
  integer , INTENT(OUT) :: auxhist5_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist5_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist5_end_h = model_config_rec%auxhist5_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_end_h
SUBROUTINE nl_get_auxhist5_end_m ( id_id , auxhist5_end_m )
  integer , INTENT(OUT) :: auxhist5_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist5_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist5_end_m = model_config_rec%auxhist5_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_end_m
SUBROUTINE nl_get_auxhist5_end_s ( id_id , auxhist5_end_s )
  integer , INTENT(OUT) :: auxhist5_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist5_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist5_end_s = model_config_rec%auxhist5_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_end_s
SUBROUTINE nl_get_auxhist6_end_y ( id_id , auxhist6_end_y )
  integer , INTENT(OUT) :: auxhist6_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist6_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist6_end_y = model_config_rec%auxhist6_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_end_y
SUBROUTINE nl_get_auxhist6_end_mo ( id_id , auxhist6_end_mo )
  integer , INTENT(OUT) :: auxhist6_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist6_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist6_end_mo = model_config_rec%auxhist6_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_end_mo
SUBROUTINE nl_get_auxhist6_end_d ( id_id , auxhist6_end_d )
  integer , INTENT(OUT) :: auxhist6_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist6_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist6_end_d = model_config_rec%auxhist6_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_end_d
SUBROUTINE nl_get_auxhist6_end_h ( id_id , auxhist6_end_h )
  integer , INTENT(OUT) :: auxhist6_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist6_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist6_end_h = model_config_rec%auxhist6_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_end_h
SUBROUTINE nl_get_auxhist6_end_m ( id_id , auxhist6_end_m )
  integer , INTENT(OUT) :: auxhist6_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist6_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist6_end_m = model_config_rec%auxhist6_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_end_m
SUBROUTINE nl_get_auxhist6_end_s ( id_id , auxhist6_end_s )
  integer , INTENT(OUT) :: auxhist6_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist6_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist6_end_s = model_config_rec%auxhist6_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_end_s
SUBROUTINE nl_get_auxhist7_end_y ( id_id , auxhist7_end_y )
  integer , INTENT(OUT) :: auxhist7_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist7_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist7_end_y = model_config_rec%auxhist7_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_end_y
SUBROUTINE nl_get_auxhist7_end_mo ( id_id , auxhist7_end_mo )
  integer , INTENT(OUT) :: auxhist7_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist7_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist7_end_mo = model_config_rec%auxhist7_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_end_mo
SUBROUTINE nl_get_auxhist7_end_d ( id_id , auxhist7_end_d )
  integer , INTENT(OUT) :: auxhist7_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist7_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist7_end_d = model_config_rec%auxhist7_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_end_d
SUBROUTINE nl_get_auxhist7_end_h ( id_id , auxhist7_end_h )
  integer , INTENT(OUT) :: auxhist7_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist7_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist7_end_h = model_config_rec%auxhist7_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_end_h
SUBROUTINE nl_get_auxhist7_end_m ( id_id , auxhist7_end_m )
  integer , INTENT(OUT) :: auxhist7_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist7_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist7_end_m = model_config_rec%auxhist7_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_end_m
SUBROUTINE nl_get_auxhist7_end_s ( id_id , auxhist7_end_s )
  integer , INTENT(OUT) :: auxhist7_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist7_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist7_end_s = model_config_rec%auxhist7_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_end_s
SUBROUTINE nl_get_auxhist8_end_y ( id_id , auxhist8_end_y )
  integer , INTENT(OUT) :: auxhist8_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist8_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist8_end_y = model_config_rec%auxhist8_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_end_y
SUBROUTINE nl_get_auxhist8_end_mo ( id_id , auxhist8_end_mo )
  integer , INTENT(OUT) :: auxhist8_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist8_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist8_end_mo = model_config_rec%auxhist8_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_end_mo
SUBROUTINE nl_get_auxhist8_end_d ( id_id , auxhist8_end_d )
  integer , INTENT(OUT) :: auxhist8_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist8_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist8_end_d = model_config_rec%auxhist8_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_end_d
SUBROUTINE nl_get_auxhist8_end_h ( id_id , auxhist8_end_h )
  integer , INTENT(OUT) :: auxhist8_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist8_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist8_end_h = model_config_rec%auxhist8_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_end_h
SUBROUTINE nl_get_auxhist8_end_m ( id_id , auxhist8_end_m )
  integer , INTENT(OUT) :: auxhist8_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist8_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist8_end_m = model_config_rec%auxhist8_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_end_m
SUBROUTINE nl_get_auxhist8_end_s ( id_id , auxhist8_end_s )
  integer , INTENT(OUT) :: auxhist8_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist8_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist8_end_s = model_config_rec%auxhist8_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_end_s
SUBROUTINE nl_get_auxhist9_end_y ( id_id , auxhist9_end_y )
  integer , INTENT(OUT) :: auxhist9_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist9_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist9_end_y = model_config_rec%auxhist9_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_end_y
SUBROUTINE nl_get_auxhist9_end_mo ( id_id , auxhist9_end_mo )
  integer , INTENT(OUT) :: auxhist9_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist9_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist9_end_mo = model_config_rec%auxhist9_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_end_mo
SUBROUTINE nl_get_auxhist9_end_d ( id_id , auxhist9_end_d )
  integer , INTENT(OUT) :: auxhist9_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist9_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist9_end_d = model_config_rec%auxhist9_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_end_d
SUBROUTINE nl_get_auxhist9_end_h ( id_id , auxhist9_end_h )
  integer , INTENT(OUT) :: auxhist9_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist9_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist9_end_h = model_config_rec%auxhist9_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_end_h
SUBROUTINE nl_get_auxhist9_end_m ( id_id , auxhist9_end_m )
  integer , INTENT(OUT) :: auxhist9_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist9_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist9_end_m = model_config_rec%auxhist9_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_end_m
SUBROUTINE nl_get_auxhist9_end_s ( id_id , auxhist9_end_s )
  integer , INTENT(OUT) :: auxhist9_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist9_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist9_end_s = model_config_rec%auxhist9_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_end_s
SUBROUTINE nl_get_auxhist10_end_y ( id_id , auxhist10_end_y )
  integer , INTENT(OUT) :: auxhist10_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist10_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist10_end_y = model_config_rec%auxhist10_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_end_y
SUBROUTINE nl_get_auxhist10_end_mo ( id_id , auxhist10_end_mo )
  integer , INTENT(OUT) :: auxhist10_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist10_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist10_end_mo = model_config_rec%auxhist10_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_end_mo
SUBROUTINE nl_get_auxhist10_end_d ( id_id , auxhist10_end_d )
  integer , INTENT(OUT) :: auxhist10_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist10_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist10_end_d = model_config_rec%auxhist10_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_end_d
SUBROUTINE nl_get_auxhist10_end_h ( id_id , auxhist10_end_h )
  integer , INTENT(OUT) :: auxhist10_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist10_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist10_end_h = model_config_rec%auxhist10_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_end_h
SUBROUTINE nl_get_auxhist10_end_m ( id_id , auxhist10_end_m )
  integer , INTENT(OUT) :: auxhist10_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist10_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist10_end_m = model_config_rec%auxhist10_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_end_m
SUBROUTINE nl_get_auxhist10_end_s ( id_id , auxhist10_end_s )
  integer , INTENT(OUT) :: auxhist10_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist10_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist10_end_s = model_config_rec%auxhist10_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_end_s
SUBROUTINE nl_get_auxhist11_end_y ( id_id , auxhist11_end_y )
  integer , INTENT(OUT) :: auxhist11_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist11_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist11_end_y = model_config_rec%auxhist11_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_end_y
SUBROUTINE nl_get_auxhist11_end_mo ( id_id , auxhist11_end_mo )
  integer , INTENT(OUT) :: auxhist11_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist11_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist11_end_mo = model_config_rec%auxhist11_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_end_mo
SUBROUTINE nl_get_auxhist11_end_d ( id_id , auxhist11_end_d )
  integer , INTENT(OUT) :: auxhist11_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist11_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist11_end_d = model_config_rec%auxhist11_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_end_d
SUBROUTINE nl_get_auxhist11_end_h ( id_id , auxhist11_end_h )
  integer , INTENT(OUT) :: auxhist11_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist11_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist11_end_h = model_config_rec%auxhist11_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_end_h
SUBROUTINE nl_get_auxhist11_end_m ( id_id , auxhist11_end_m )
  integer , INTENT(OUT) :: auxhist11_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist11_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist11_end_m = model_config_rec%auxhist11_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_end_m
SUBROUTINE nl_get_auxhist11_end_s ( id_id , auxhist11_end_s )
  integer , INTENT(OUT) :: auxhist11_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxhist11_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxhist11_end_s = model_config_rec%auxhist11_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_end_s
SUBROUTINE nl_get_auxinput1_end_y ( id_id , auxinput1_end_y )
  integer , INTENT(OUT) :: auxinput1_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput1_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput1_end_y = model_config_rec%auxinput1_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_end_y
SUBROUTINE nl_get_auxinput1_end_mo ( id_id , auxinput1_end_mo )
  integer , INTENT(OUT) :: auxinput1_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput1_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput1_end_mo = model_config_rec%auxinput1_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_end_mo
SUBROUTINE nl_get_auxinput1_end_d ( id_id , auxinput1_end_d )
  integer , INTENT(OUT) :: auxinput1_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput1_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput1_end_d = model_config_rec%auxinput1_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_end_d
SUBROUTINE nl_get_auxinput1_end_h ( id_id , auxinput1_end_h )
  integer , INTENT(OUT) :: auxinput1_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput1_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput1_end_h = model_config_rec%auxinput1_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_end_h
SUBROUTINE nl_get_auxinput1_end_m ( id_id , auxinput1_end_m )
  integer , INTENT(OUT) :: auxinput1_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput1_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput1_end_m = model_config_rec%auxinput1_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_end_m
SUBROUTINE nl_get_auxinput1_end_s ( id_id , auxinput1_end_s )
  integer , INTENT(OUT) :: auxinput1_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput1_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput1_end_s = model_config_rec%auxinput1_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_end_s
SUBROUTINE nl_get_auxinput2_end_y ( id_id , auxinput2_end_y )
  integer , INTENT(OUT) :: auxinput2_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput2_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput2_end_y = model_config_rec%auxinput2_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_end_y
SUBROUTINE nl_get_auxinput2_end_mo ( id_id , auxinput2_end_mo )
  integer , INTENT(OUT) :: auxinput2_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput2_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput2_end_mo = model_config_rec%auxinput2_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_end_mo
SUBROUTINE nl_get_auxinput2_end_d ( id_id , auxinput2_end_d )
  integer , INTENT(OUT) :: auxinput2_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput2_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput2_end_d = model_config_rec%auxinput2_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_end_d
SUBROUTINE nl_get_auxinput2_end_h ( id_id , auxinput2_end_h )
  integer , INTENT(OUT) :: auxinput2_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput2_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput2_end_h = model_config_rec%auxinput2_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_end_h
SUBROUTINE nl_get_auxinput2_end_m ( id_id , auxinput2_end_m )
  integer , INTENT(OUT) :: auxinput2_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput2_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput2_end_m = model_config_rec%auxinput2_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_end_m
SUBROUTINE nl_get_auxinput2_end_s ( id_id , auxinput2_end_s )
  integer , INTENT(OUT) :: auxinput2_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput2_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput2_end_s = model_config_rec%auxinput2_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_end_s
SUBROUTINE nl_get_auxinput3_end_y ( id_id , auxinput3_end_y )
  integer , INTENT(OUT) :: auxinput3_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput3_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput3_end_y = model_config_rec%auxinput3_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_end_y
SUBROUTINE nl_get_auxinput3_end_mo ( id_id , auxinput3_end_mo )
  integer , INTENT(OUT) :: auxinput3_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput3_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput3_end_mo = model_config_rec%auxinput3_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_end_mo
SUBROUTINE nl_get_auxinput3_end_d ( id_id , auxinput3_end_d )
  integer , INTENT(OUT) :: auxinput3_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput3_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput3_end_d = model_config_rec%auxinput3_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_end_d
SUBROUTINE nl_get_auxinput3_end_h ( id_id , auxinput3_end_h )
  integer , INTENT(OUT) :: auxinput3_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput3_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput3_end_h = model_config_rec%auxinput3_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_end_h
SUBROUTINE nl_get_auxinput3_end_m ( id_id , auxinput3_end_m )
  integer , INTENT(OUT) :: auxinput3_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput3_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput3_end_m = model_config_rec%auxinput3_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_end_m
SUBROUTINE nl_get_auxinput3_end_s ( id_id , auxinput3_end_s )
  integer , INTENT(OUT) :: auxinput3_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput3_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput3_end_s = model_config_rec%auxinput3_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_end_s
SUBROUTINE nl_get_auxinput4_end_y ( id_id , auxinput4_end_y )
  integer , INTENT(OUT) :: auxinput4_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput4_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput4_end_y = model_config_rec%auxinput4_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_end_y
SUBROUTINE nl_get_auxinput4_end_mo ( id_id , auxinput4_end_mo )
  integer , INTENT(OUT) :: auxinput4_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput4_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput4_end_mo = model_config_rec%auxinput4_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_end_mo
SUBROUTINE nl_get_auxinput4_end_d ( id_id , auxinput4_end_d )
  integer , INTENT(OUT) :: auxinput4_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput4_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput4_end_d = model_config_rec%auxinput4_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_end_d
SUBROUTINE nl_get_auxinput4_end_h ( id_id , auxinput4_end_h )
  integer , INTENT(OUT) :: auxinput4_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput4_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput4_end_h = model_config_rec%auxinput4_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_end_h
SUBROUTINE nl_get_auxinput4_end_m ( id_id , auxinput4_end_m )
  integer , INTENT(OUT) :: auxinput4_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput4_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput4_end_m = model_config_rec%auxinput4_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_end_m
SUBROUTINE nl_get_auxinput4_end_s ( id_id , auxinput4_end_s )
  integer , INTENT(OUT) :: auxinput4_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput4_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput4_end_s = model_config_rec%auxinput4_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_end_s
SUBROUTINE nl_get_auxinput5_end_y ( id_id , auxinput5_end_y )
  integer , INTENT(OUT) :: auxinput5_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput5_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput5_end_y = model_config_rec%auxinput5_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_end_y
SUBROUTINE nl_get_auxinput5_end_mo ( id_id , auxinput5_end_mo )
  integer , INTENT(OUT) :: auxinput5_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput5_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput5_end_mo = model_config_rec%auxinput5_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_end_mo
SUBROUTINE nl_get_auxinput5_end_d ( id_id , auxinput5_end_d )
  integer , INTENT(OUT) :: auxinput5_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput5_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput5_end_d = model_config_rec%auxinput5_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_end_d
SUBROUTINE nl_get_auxinput5_end_h ( id_id , auxinput5_end_h )
  integer , INTENT(OUT) :: auxinput5_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput5_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput5_end_h = model_config_rec%auxinput5_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_end_h
SUBROUTINE nl_get_auxinput5_end_m ( id_id , auxinput5_end_m )
  integer , INTENT(OUT) :: auxinput5_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput5_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput5_end_m = model_config_rec%auxinput5_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_end_m
SUBROUTINE nl_get_auxinput5_end_s ( id_id , auxinput5_end_s )
  integer , INTENT(OUT) :: auxinput5_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput5_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput5_end_s = model_config_rec%auxinput5_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_end_s
SUBROUTINE nl_get_auxinput6_end_y ( id_id , auxinput6_end_y )
  integer , INTENT(OUT) :: auxinput6_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput6_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput6_end_y = model_config_rec%auxinput6_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_end_y
SUBROUTINE nl_get_auxinput6_end_mo ( id_id , auxinput6_end_mo )
  integer , INTENT(OUT) :: auxinput6_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput6_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput6_end_mo = model_config_rec%auxinput6_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_end_mo
SUBROUTINE nl_get_auxinput6_end_d ( id_id , auxinput6_end_d )
  integer , INTENT(OUT) :: auxinput6_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput6_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput6_end_d = model_config_rec%auxinput6_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_end_d
SUBROUTINE nl_get_auxinput6_end_h ( id_id , auxinput6_end_h )
  integer , INTENT(OUT) :: auxinput6_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput6_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput6_end_h = model_config_rec%auxinput6_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_end_h
SUBROUTINE nl_get_auxinput6_end_m ( id_id , auxinput6_end_m )
  integer , INTENT(OUT) :: auxinput6_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput6_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput6_end_m = model_config_rec%auxinput6_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_end_m
SUBROUTINE nl_get_auxinput6_end_s ( id_id , auxinput6_end_s )
  integer , INTENT(OUT) :: auxinput6_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput6_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput6_end_s = model_config_rec%auxinput6_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_end_s
SUBROUTINE nl_get_auxinput7_end_y ( id_id , auxinput7_end_y )
  integer , INTENT(OUT) :: auxinput7_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput7_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput7_end_y = model_config_rec%auxinput7_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_end_y
SUBROUTINE nl_get_auxinput7_end_mo ( id_id , auxinput7_end_mo )
  integer , INTENT(OUT) :: auxinput7_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput7_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput7_end_mo = model_config_rec%auxinput7_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_end_mo
SUBROUTINE nl_get_auxinput7_end_d ( id_id , auxinput7_end_d )
  integer , INTENT(OUT) :: auxinput7_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput7_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput7_end_d = model_config_rec%auxinput7_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_end_d
SUBROUTINE nl_get_auxinput7_end_h ( id_id , auxinput7_end_h )
  integer , INTENT(OUT) :: auxinput7_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput7_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput7_end_h = model_config_rec%auxinput7_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_end_h
SUBROUTINE nl_get_auxinput7_end_m ( id_id , auxinput7_end_m )
  integer , INTENT(OUT) :: auxinput7_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput7_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput7_end_m = model_config_rec%auxinput7_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_end_m
SUBROUTINE nl_get_auxinput7_end_s ( id_id , auxinput7_end_s )
  integer , INTENT(OUT) :: auxinput7_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput7_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput7_end_s = model_config_rec%auxinput7_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_end_s
SUBROUTINE nl_get_auxinput8_end_y ( id_id , auxinput8_end_y )
  integer , INTENT(OUT) :: auxinput8_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput8_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput8_end_y = model_config_rec%auxinput8_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_end_y
SUBROUTINE nl_get_auxinput8_end_mo ( id_id , auxinput8_end_mo )
  integer , INTENT(OUT) :: auxinput8_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput8_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput8_end_mo = model_config_rec%auxinput8_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_end_mo
SUBROUTINE nl_get_auxinput8_end_d ( id_id , auxinput8_end_d )
  integer , INTENT(OUT) :: auxinput8_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput8_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput8_end_d = model_config_rec%auxinput8_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_end_d
SUBROUTINE nl_get_auxinput8_end_h ( id_id , auxinput8_end_h )
  integer , INTENT(OUT) :: auxinput8_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput8_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput8_end_h = model_config_rec%auxinput8_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_end_h
SUBROUTINE nl_get_auxinput8_end_m ( id_id , auxinput8_end_m )
  integer , INTENT(OUT) :: auxinput8_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput8_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput8_end_m = model_config_rec%auxinput8_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_end_m
SUBROUTINE nl_get_auxinput8_end_s ( id_id , auxinput8_end_s )
  integer , INTENT(OUT) :: auxinput8_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput8_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput8_end_s = model_config_rec%auxinput8_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_end_s
SUBROUTINE nl_get_auxinput9_end_y ( id_id , auxinput9_end_y )
  integer , INTENT(OUT) :: auxinput9_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput9_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput9_end_y = model_config_rec%auxinput9_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput9_end_y
SUBROUTINE nl_get_auxinput9_end_mo ( id_id , auxinput9_end_mo )
  integer , INTENT(OUT) :: auxinput9_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput9_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput9_end_mo = model_config_rec%auxinput9_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput9_end_mo
SUBROUTINE nl_get_auxinput9_end_d ( id_id , auxinput9_end_d )
  integer , INTENT(OUT) :: auxinput9_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput9_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput9_end_d = model_config_rec%auxinput9_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput9_end_d
SUBROUTINE nl_get_auxinput9_end_h ( id_id , auxinput9_end_h )
  integer , INTENT(OUT) :: auxinput9_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput9_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput9_end_h = model_config_rec%auxinput9_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput9_end_h
SUBROUTINE nl_get_auxinput9_end_m ( id_id , auxinput9_end_m )
  integer , INTENT(OUT) :: auxinput9_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput9_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput9_end_m = model_config_rec%auxinput9_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput9_end_m
SUBROUTINE nl_get_auxinput9_end_s ( id_id , auxinput9_end_s )
  integer , INTENT(OUT) :: auxinput9_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput9_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput9_end_s = model_config_rec%auxinput9_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput9_end_s
SUBROUTINE nl_get_gfdda_end_y ( id_id , gfdda_end_y )
  integer , INTENT(OUT) :: gfdda_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_gfdda_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  gfdda_end_y = model_config_rec%gfdda_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_gfdda_end_y
SUBROUTINE nl_get_gfdda_end_mo ( id_id , gfdda_end_mo )
  integer , INTENT(OUT) :: gfdda_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_gfdda_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  gfdda_end_mo = model_config_rec%gfdda_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_gfdda_end_mo
SUBROUTINE nl_get_gfdda_end_d ( id_id , gfdda_end_d )
  integer , INTENT(OUT) :: gfdda_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_gfdda_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  gfdda_end_d = model_config_rec%gfdda_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_gfdda_end_d
SUBROUTINE nl_get_gfdda_end_h ( id_id , gfdda_end_h )
  integer , INTENT(OUT) :: gfdda_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_gfdda_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  gfdda_end_h = model_config_rec%gfdda_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_gfdda_end_h
SUBROUTINE nl_get_gfdda_end_m ( id_id , gfdda_end_m )
  integer , INTENT(OUT) :: gfdda_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_gfdda_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  gfdda_end_m = model_config_rec%gfdda_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_gfdda_end_m
SUBROUTINE nl_get_gfdda_end_s ( id_id , gfdda_end_s )
  integer , INTENT(OUT) :: gfdda_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_gfdda_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  gfdda_end_s = model_config_rec%gfdda_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_gfdda_end_s
SUBROUTINE nl_get_auxinput11_end_y ( id_id , auxinput11_end_y )
  integer , INTENT(OUT) :: auxinput11_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput11_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput11_end_y = model_config_rec%auxinput11_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_end_y
SUBROUTINE nl_get_auxinput11_end_mo ( id_id , auxinput11_end_mo )
  integer , INTENT(OUT) :: auxinput11_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput11_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput11_end_mo = model_config_rec%auxinput11_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_end_mo
SUBROUTINE nl_get_auxinput11_end_d ( id_id , auxinput11_end_d )
  integer , INTENT(OUT) :: auxinput11_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput11_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput11_end_d = model_config_rec%auxinput11_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_end_d
SUBROUTINE nl_get_auxinput11_end_h ( id_id , auxinput11_end_h )
  integer , INTENT(OUT) :: auxinput11_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput11_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput11_end_h = model_config_rec%auxinput11_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_end_h
SUBROUTINE nl_get_auxinput11_end_m ( id_id , auxinput11_end_m )
  integer , INTENT(OUT) :: auxinput11_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput11_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput11_end_m = model_config_rec%auxinput11_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_end_m
SUBROUTINE nl_get_auxinput11_end_s ( id_id , auxinput11_end_s )
  integer , INTENT(OUT) :: auxinput11_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_auxinput11_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  auxinput11_end_s = model_config_rec%auxinput11_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_end_s
SUBROUTINE nl_get_io_form_auxinput1 ( id_id , io_form_auxinput1 )
  integer , INTENT(OUT) :: io_form_auxinput1
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_io_form_auxinput1: io_form_auxinput1 applies to all domains. First arg ignored.')
  ENDIF
  io_form_auxinput1 = model_config_rec%io_form_auxinput1
  RETURN
END SUBROUTINE nl_get_io_form_auxinput1
SUBROUTINE nl_get_io_form_auxinput2 ( id_id , io_form_auxinput2 )
  integer , INTENT(OUT) :: io_form_auxinput2
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_io_form_auxinput2: io_form_auxinput2 applies to all domains. First arg ignored.')
  ENDIF
  io_form_auxinput2 = model_config_rec%io_form_auxinput2
  RETURN
END SUBROUTINE nl_get_io_form_auxinput2
SUBROUTINE nl_get_io_form_auxinput3 ( id_id , io_form_auxinput3 )
  integer , INTENT(OUT) :: io_form_auxinput3
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_io_form_auxinput3: io_form_auxinput3 applies to all domains. First arg ignored.')
  ENDIF
  io_form_auxinput3 = model_config_rec%io_form_auxinput3
  RETURN
END SUBROUTINE nl_get_io_form_auxinput3
SUBROUTINE nl_get_io_form_auxinput4 ( id_id , io_form_auxinput4 )
  integer , INTENT(OUT) :: io_form_auxinput4
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_io_form_auxinput4: io_form_auxinput4 applies to all domains. First arg ignored.')
  ENDIF
  io_form_auxinput4 = model_config_rec%io_form_auxinput4
  RETURN
END SUBROUTINE nl_get_io_form_auxinput4
SUBROUTINE nl_get_io_form_auxinput5 ( id_id , io_form_auxinput5 )
  integer , INTENT(OUT) :: io_form_auxinput5
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_io_form_auxinput5: io_form_auxinput5 applies to all domains. First arg ignored.')
  ENDIF
  io_form_auxinput5 = model_config_rec%io_form_auxinput5
  RETURN
END SUBROUTINE nl_get_io_form_auxinput5
SUBROUTINE nl_get_io_form_auxinput6 ( id_id , io_form_auxinput6 )
  integer , INTENT(OUT) :: io_form_auxinput6
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_io_form_auxinput6: io_form_auxinput6 applies to all domains. First arg ignored.')
  ENDIF
  io_form_auxinput6 = model_config_rec%io_form_auxinput6
  RETURN
END SUBROUTINE nl_get_io_form_auxinput6
SUBROUTINE nl_get_io_form_auxinput7 ( id_id , io_form_auxinput7 )
  integer , INTENT(OUT) :: io_form_auxinput7
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_io_form_auxinput7: io_form_auxinput7 applies to all domains. First arg ignored.')
  ENDIF
  io_form_auxinput7 = model_config_rec%io_form_auxinput7
  RETURN
END SUBROUTINE nl_get_io_form_auxinput7
SUBROUTINE nl_get_io_form_auxinput8 ( id_id , io_form_auxinput8 )
  integer , INTENT(OUT) :: io_form_auxinput8
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_io_form_auxinput8: io_form_auxinput8 applies to all domains. First arg ignored.')
  ENDIF
  io_form_auxinput8 = model_config_rec%io_form_auxinput8
  RETURN
END SUBROUTINE nl_get_io_form_auxinput8
SUBROUTINE nl_get_io_form_auxinput9 ( id_id , io_form_auxinput9 )
  integer , INTENT(OUT) :: io_form_auxinput9
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_io_form_auxinput9: io_form_auxinput9 applies to all domains. First arg ignored.')
  ENDIF
  io_form_auxinput9 = model_config_rec%io_form_auxinput9
  RETURN
END SUBROUTINE nl_get_io_form_auxinput9
SUBROUTINE nl_get_io_form_gfdda ( id_id , io_form_gfdda )
  integer , INTENT(OUT) :: io_form_gfdda
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_io_form_gfdda: io_form_gfdda applies to all domains. First arg ignored.')
  ENDIF
  io_form_gfdda = model_config_rec%io_form_gfdda
  RETURN
END SUBROUTINE nl_get_io_form_gfdda
SUBROUTINE nl_get_io_form_auxinput11 ( id_id , io_form_auxinput11 )
  integer , INTENT(OUT) :: io_form_auxinput11
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_io_form_auxinput11: io_form_auxinput11 applies to all domains. First arg ignored.')
  ENDIF
  io_form_auxinput11 = model_config_rec%io_form_auxinput11
  RETURN
END SUBROUTINE nl_get_io_form_auxinput11
SUBROUTINE nl_get_io_form_auxhist1 ( id_id , io_form_auxhist1 )
  integer , INTENT(OUT) :: io_form_auxhist1
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_io_form_auxhist1: io_form_auxhist1 applies to all domains. First arg ignored.')
  ENDIF
  io_form_auxhist1 = model_config_rec%io_form_auxhist1
  RETURN
END SUBROUTINE nl_get_io_form_auxhist1
SUBROUTINE nl_get_io_form_auxhist2 ( id_id , io_form_auxhist2 )
  integer , INTENT(OUT) :: io_form_auxhist2
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_io_form_auxhist2: io_form_auxhist2 applies to all domains. First arg ignored.')
  ENDIF
  io_form_auxhist2 = model_config_rec%io_form_auxhist2
  RETURN
END SUBROUTINE nl_get_io_form_auxhist2
SUBROUTINE nl_get_io_form_auxhist3 ( id_id , io_form_auxhist3 )
  integer , INTENT(OUT) :: io_form_auxhist3
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_io_form_auxhist3: io_form_auxhist3 applies to all domains. First arg ignored.')
  ENDIF
  io_form_auxhist3 = model_config_rec%io_form_auxhist3
  RETURN
END SUBROUTINE nl_get_io_form_auxhist3
SUBROUTINE nl_get_io_form_auxhist4 ( id_id , io_form_auxhist4 )
  integer , INTENT(OUT) :: io_form_auxhist4
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_io_form_auxhist4: io_form_auxhist4 applies to all domains. First arg ignored.')
  ENDIF
  io_form_auxhist4 = model_config_rec%io_form_auxhist4
  RETURN
END SUBROUTINE nl_get_io_form_auxhist4
SUBROUTINE nl_get_io_form_auxhist5 ( id_id , io_form_auxhist5 )
  integer , INTENT(OUT) :: io_form_auxhist5
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_io_form_auxhist5: io_form_auxhist5 applies to all domains. First arg ignored.')
  ENDIF
  io_form_auxhist5 = model_config_rec%io_form_auxhist5
  RETURN
END SUBROUTINE nl_get_io_form_auxhist5
SUBROUTINE nl_get_io_form_auxhist6 ( id_id , io_form_auxhist6 )
  integer , INTENT(OUT) :: io_form_auxhist6
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_io_form_auxhist6: io_form_auxhist6 applies to all domains. First arg ignored.')
  ENDIF
  io_form_auxhist6 = model_config_rec%io_form_auxhist6
  RETURN
END SUBROUTINE nl_get_io_form_auxhist6
SUBROUTINE nl_get_io_form_auxhist7 ( id_id , io_form_auxhist7 )
  integer , INTENT(OUT) :: io_form_auxhist7
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_io_form_auxhist7: io_form_auxhist7 applies to all domains. First arg ignored.')
  ENDIF
  io_form_auxhist7 = model_config_rec%io_form_auxhist7
  RETURN
END SUBROUTINE nl_get_io_form_auxhist7
SUBROUTINE nl_get_io_form_auxhist8 ( id_id , io_form_auxhist8 )
  integer , INTENT(OUT) :: io_form_auxhist8
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_io_form_auxhist8: io_form_auxhist8 applies to all domains. First arg ignored.')
  ENDIF
  io_form_auxhist8 = model_config_rec%io_form_auxhist8
  RETURN
END SUBROUTINE nl_get_io_form_auxhist8
SUBROUTINE nl_get_io_form_auxhist9 ( id_id , io_form_auxhist9 )
  integer , INTENT(OUT) :: io_form_auxhist9
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_io_form_auxhist9: io_form_auxhist9 applies to all domains. First arg ignored.')
  ENDIF
  io_form_auxhist9 = model_config_rec%io_form_auxhist9
  RETURN
END SUBROUTINE nl_get_io_form_auxhist9
SUBROUTINE nl_get_io_form_auxhist10 ( id_id , io_form_auxhist10 )
  integer , INTENT(OUT) :: io_form_auxhist10
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_io_form_auxhist10: io_form_auxhist10 applies to all domains. First arg ignored.')
  ENDIF
  io_form_auxhist10 = model_config_rec%io_form_auxhist10
  RETURN
END SUBROUTINE nl_get_io_form_auxhist10
SUBROUTINE nl_get_io_form_auxhist11 ( id_id , io_form_auxhist11 )
  integer , INTENT(OUT) :: io_form_auxhist11
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_io_form_auxhist11: io_form_auxhist11 applies to all domains. First arg ignored.')
  ENDIF
  io_form_auxhist11 = model_config_rec%io_form_auxhist11
  RETURN
END SUBROUTINE nl_get_io_form_auxhist11
SUBROUTINE nl_get_julyr ( id_id , julyr )
  integer , INTENT(OUT) :: julyr
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_julyr: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  julyr = model_config_rec%julyr(id_id)
  RETURN
END SUBROUTINE nl_get_julyr
SUBROUTINE nl_get_julday ( id_id , julday )
  integer , INTENT(OUT) :: julday
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_julday: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  julday = model_config_rec%julday(id_id)
  RETURN
END SUBROUTINE nl_get_julday
SUBROUTINE nl_get_gmt ( id_id , gmt )
  real , INTENT(OUT) :: gmt
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_gmt: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  gmt = model_config_rec%gmt(id_id)
  RETURN
END SUBROUTINE nl_get_gmt
SUBROUTINE nl_get_input_inname ( id_id , input_inname )
  character*256 , INTENT(OUT) :: input_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_input_inname: input_inname applies to all domains. First arg ignored.')
  ENDIF
  input_inname = trim(model_config_rec%input_inname)
  RETURN
END SUBROUTINE nl_get_input_inname
SUBROUTINE nl_get_input_outname ( id_id , input_outname )
  character*256 , INTENT(OUT) :: input_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_input_outname: input_outname applies to all domains. First arg ignored.')
  ENDIF
  input_outname = trim(model_config_rec%input_outname)
  RETURN
END SUBROUTINE nl_get_input_outname
SUBROUTINE nl_get_bdy_inname ( id_id , bdy_inname )
  character*256 , INTENT(OUT) :: bdy_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_bdy_inname: bdy_inname applies to all domains. First arg ignored.')
  ENDIF
  bdy_inname = trim(model_config_rec%bdy_inname)
  RETURN
END SUBROUTINE nl_get_bdy_inname
SUBROUTINE nl_get_bdy_outname ( id_id , bdy_outname )
  character*256 , INTENT(OUT) :: bdy_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_bdy_outname: bdy_outname applies to all domains. First arg ignored.')
  ENDIF
  bdy_outname = trim(model_config_rec%bdy_outname)
  RETURN
END SUBROUTINE nl_get_bdy_outname
SUBROUTINE nl_get_rst_inname ( id_id , rst_inname )
  character*256 , INTENT(OUT) :: rst_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_rst_inname: rst_inname applies to all domains. First arg ignored.')
  ENDIF
  rst_inname = trim(model_config_rec%rst_inname)
  RETURN
END SUBROUTINE nl_get_rst_inname
SUBROUTINE nl_get_rst_outname ( id_id , rst_outname )
  character*256 , INTENT(OUT) :: rst_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_rst_outname: rst_outname applies to all domains. First arg ignored.')
  ENDIF
  rst_outname = trim(model_config_rec%rst_outname)
  RETURN
END SUBROUTINE nl_get_rst_outname
SUBROUTINE nl_get_write_input ( id_id , write_input )
  logical , INTENT(OUT) :: write_input
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_write_input: write_input applies to all domains. First arg ignored.')
  ENDIF
  write_input = model_config_rec%write_input
  RETURN
END SUBROUTINE nl_get_write_input
SUBROUTINE nl_get_write_restart_at_0h ( id_id , write_restart_at_0h )
  logical , INTENT(OUT) :: write_restart_at_0h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_write_restart_at_0h: write_restart_at_0h applies to all domains. First arg ignored.')
  ENDIF
  write_restart_at_0h = model_config_rec%write_restart_at_0h
  RETURN
END SUBROUTINE nl_get_write_restart_at_0h
SUBROUTINE nl_get_adjust_output_times ( id_id , adjust_output_times )
  logical , INTENT(OUT) :: adjust_output_times
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_adjust_output_times: adjust_output_times applies to all domains. First arg ignored.')
  ENDIF
  adjust_output_times = model_config_rec%adjust_output_times
  RETURN
END SUBROUTINE nl_get_adjust_output_times
SUBROUTINE nl_get_adjust_input_times ( id_id , adjust_input_times )
  logical , INTENT(OUT) :: adjust_input_times
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_adjust_input_times: adjust_input_times applies to all domains. First arg ignored.')
  ENDIF
  adjust_input_times = model_config_rec%adjust_input_times
  RETURN
END SUBROUTINE nl_get_adjust_input_times
SUBROUTINE nl_get_tstart ( id_id , tstart )
  real , INTENT(OUT) :: tstart
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_tstart: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  tstart = model_config_rec%tstart(id_id)
  RETURN
END SUBROUTINE nl_get_tstart
SUBROUTINE nl_get_nocolons ( id_id , nocolons )
  logical , INTENT(OUT) :: nocolons
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_nocolons: nocolons applies to all domains. First arg ignored.')
  ENDIF
  nocolons = model_config_rec%nocolons
  RETURN
END SUBROUTINE nl_get_nocolons
SUBROUTINE nl_get_time_step ( id_id , time_step )
  integer , INTENT(OUT) :: time_step
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_time_step: time_step applies to all domains. First arg ignored.')
  ENDIF
  time_step = model_config_rec%time_step
  RETURN
END SUBROUTINE nl_get_time_step
SUBROUTINE nl_get_time_step_fract_num ( id_id , time_step_fract_num )
  integer , INTENT(OUT) :: time_step_fract_num
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_time_step_fract_num: time_step_fract_num applies to all domains. First arg ignored.')
  ENDIF
  time_step_fract_num = model_config_rec%time_step_fract_num
  RETURN
END SUBROUTINE nl_get_time_step_fract_num
SUBROUTINE nl_get_time_step_fract_den ( id_id , time_step_fract_den )
  integer , INTENT(OUT) :: time_step_fract_den
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_time_step_fract_den: time_step_fract_den applies to all domains. First arg ignored.')
  ENDIF
  time_step_fract_den = model_config_rec%time_step_fract_den
  RETURN
END SUBROUTINE nl_get_time_step_fract_den
SUBROUTINE nl_get_max_dom ( id_id , max_dom )
  integer , INTENT(OUT) :: max_dom
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_max_dom: max_dom applies to all domains. First arg ignored.')
  ENDIF
  max_dom = model_config_rec%max_dom
  RETURN
END SUBROUTINE nl_get_max_dom
SUBROUTINE nl_get_s_we ( id_id , s_we )
  integer , INTENT(OUT) :: s_we
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_s_we: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  s_we = model_config_rec%s_we(id_id)
  RETURN
END SUBROUTINE nl_get_s_we
SUBROUTINE nl_get_e_we ( id_id , e_we )
  integer , INTENT(OUT) :: e_we
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_e_we: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  e_we = model_config_rec%e_we(id_id)
  RETURN
END SUBROUTINE nl_get_e_we
SUBROUTINE nl_get_s_sn ( id_id , s_sn )
  integer , INTENT(OUT) :: s_sn
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_s_sn: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  s_sn = model_config_rec%s_sn(id_id)
  RETURN
END SUBROUTINE nl_get_s_sn
SUBROUTINE nl_get_e_sn ( id_id , e_sn )
  integer , INTENT(OUT) :: e_sn
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_e_sn: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  e_sn = model_config_rec%e_sn(id_id)
  RETURN
END SUBROUTINE nl_get_e_sn
SUBROUTINE nl_get_s_vert ( id_id , s_vert )
  integer , INTENT(OUT) :: s_vert
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_s_vert: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  s_vert = model_config_rec%s_vert(id_id)
  RETURN
END SUBROUTINE nl_get_s_vert
SUBROUTINE nl_get_e_vert ( id_id , e_vert )
  integer , INTENT(OUT) :: e_vert
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_e_vert: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  e_vert = model_config_rec%e_vert(id_id)
  RETURN
END SUBROUTINE nl_get_e_vert
SUBROUTINE nl_get_dx ( id_id , dx )
  real , INTENT(OUT) :: dx
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_dx: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  dx = model_config_rec%dx(id_id)
  RETURN
END SUBROUTINE nl_get_dx
SUBROUTINE nl_get_dy ( id_id , dy )
  real , INTENT(OUT) :: dy
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_dy: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  dy = model_config_rec%dy(id_id)
  RETURN
END SUBROUTINE nl_get_dy
SUBROUTINE nl_get_grid_id ( id_id , grid_id )
  integer , INTENT(OUT) :: grid_id
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_grid_id: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  grid_id = model_config_rec%grid_id(id_id)
  RETURN
END SUBROUTINE nl_get_grid_id
SUBROUTINE nl_get_parent_id ( id_id , parent_id )
  integer , INTENT(OUT) :: parent_id
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_parent_id: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  parent_id = model_config_rec%parent_id(id_id)
  RETURN
END SUBROUTINE nl_get_parent_id
SUBROUTINE nl_get_i_parent_start ( id_id , i_parent_start )
  integer , INTENT(OUT) :: i_parent_start
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_i_parent_start: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  i_parent_start = model_config_rec%i_parent_start(id_id)
  RETURN
END SUBROUTINE nl_get_i_parent_start
SUBROUTINE nl_get_j_parent_start ( id_id , j_parent_start )
  integer , INTENT(OUT) :: j_parent_start
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_j_parent_start: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  j_parent_start = model_config_rec%j_parent_start(id_id)
  RETURN
END SUBROUTINE nl_get_j_parent_start
SUBROUTINE nl_get_parent_grid_ratio ( id_id , parent_grid_ratio )
  integer , INTENT(OUT) :: parent_grid_ratio
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_parent_grid_ratio: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  parent_grid_ratio = model_config_rec%parent_grid_ratio(id_id)
  RETURN
END SUBROUTINE nl_get_parent_grid_ratio
SUBROUTINE nl_get_parent_time_step_ratio ( id_id , parent_time_step_ratio )
  integer , INTENT(OUT) :: parent_time_step_ratio
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_parent_time_step_ratio: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  parent_time_step_ratio = model_config_rec%parent_time_step_ratio(id_id)
  RETURN
END SUBROUTINE nl_get_parent_time_step_ratio
SUBROUTINE nl_get_feedback ( id_id , feedback )
  integer , INTENT(OUT) :: feedback
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_feedback: feedback applies to all domains. First arg ignored.')
  ENDIF
  feedback = model_config_rec%feedback
  RETURN
END SUBROUTINE nl_get_feedback
SUBROUTINE nl_get_smooth_option ( id_id , smooth_option )
  integer , INTENT(OUT) :: smooth_option
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_smooth_option: smooth_option applies to all domains. First arg ignored.')
  ENDIF
  smooth_option = model_config_rec%smooth_option
  RETURN
END SUBROUTINE nl_get_smooth_option
SUBROUTINE nl_get_ztop ( id_id , ztop )
  real , INTENT(OUT) :: ztop
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_ztop: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  ztop = model_config_rec%ztop(id_id)
  RETURN
END SUBROUTINE nl_get_ztop
SUBROUTINE nl_get_moad_grid_ratio ( id_id , moad_grid_ratio )
  integer , INTENT(OUT) :: moad_grid_ratio
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_moad_grid_ratio: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  moad_grid_ratio = model_config_rec%moad_grid_ratio(id_id)
  RETURN
END SUBROUTINE nl_get_moad_grid_ratio
SUBROUTINE nl_get_moad_time_step_ratio ( id_id , moad_time_step_ratio )
  integer , INTENT(OUT) :: moad_time_step_ratio
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_moad_time_step_ratio: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  moad_time_step_ratio = model_config_rec%moad_time_step_ratio(id_id)
  RETURN
END SUBROUTINE nl_get_moad_time_step_ratio
SUBROUTINE nl_get_shw ( id_id , shw )
  integer , INTENT(OUT) :: shw
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_shw: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  shw = model_config_rec%shw(id_id)
  RETURN
END SUBROUTINE nl_get_shw
SUBROUTINE nl_get_tile_sz_x ( id_id , tile_sz_x )
  integer , INTENT(OUT) :: tile_sz_x
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_tile_sz_x: tile_sz_x applies to all domains. First arg ignored.')
  ENDIF
  tile_sz_x = model_config_rec%tile_sz_x
  RETURN
END SUBROUTINE nl_get_tile_sz_x
SUBROUTINE nl_get_tile_sz_y ( id_id , tile_sz_y )
  integer , INTENT(OUT) :: tile_sz_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_tile_sz_y: tile_sz_y applies to all domains. First arg ignored.')
  ENDIF
  tile_sz_y = model_config_rec%tile_sz_y
  RETURN
END SUBROUTINE nl_get_tile_sz_y
SUBROUTINE nl_get_numtiles ( id_id , numtiles )
  integer , INTENT(OUT) :: numtiles
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_numtiles: numtiles applies to all domains. First arg ignored.')
  ENDIF
  numtiles = model_config_rec%numtiles
  RETURN
END SUBROUTINE nl_get_numtiles
SUBROUTINE nl_get_nproc_x ( id_id , nproc_x )
  integer , INTENT(OUT) :: nproc_x
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_nproc_x: nproc_x applies to all domains. First arg ignored.')
  ENDIF
  nproc_x = model_config_rec%nproc_x
  RETURN
END SUBROUTINE nl_get_nproc_x
SUBROUTINE nl_get_nproc_y ( id_id , nproc_y )
  integer , INTENT(OUT) :: nproc_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_nproc_y: nproc_y applies to all domains. First arg ignored.')
  ENDIF
  nproc_y = model_config_rec%nproc_y
  RETURN
END SUBROUTINE nl_get_nproc_y
SUBROUTINE nl_get_irand ( id_id , irand )
  integer , INTENT(OUT) :: irand
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_irand: irand applies to all domains. First arg ignored.')
  ENDIF
  irand = model_config_rec%irand
  RETURN
END SUBROUTINE nl_get_irand
SUBROUTINE nl_get_dt ( id_id , dt )
  real , INTENT(OUT) :: dt
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_dt: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  dt = model_config_rec%dt(id_id)
  RETURN
END SUBROUTINE nl_get_dt
SUBROUTINE nl_get_num_moves ( id_id , num_moves )
  integer , INTENT(OUT) :: num_moves
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_num_moves: num_moves applies to all domains. First arg ignored.')
  ENDIF
  num_moves = model_config_rec%num_moves
  RETURN
END SUBROUTINE nl_get_num_moves
SUBROUTINE nl_get_move_id ( id_id , move_id )
  integer , INTENT(OUT) :: move_id
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%num_moves ) THEN
    WRITE(emess,*)'nl_get_move_id: Out of range move number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  move_id = model_config_rec%move_id(id_id)
  RETURN
END SUBROUTINE nl_get_move_id
SUBROUTINE nl_get_move_interval ( id_id , move_interval )
  integer , INTENT(OUT) :: move_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%num_moves ) THEN
    WRITE(emess,*)'nl_get_move_interval: Out of range move number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  move_interval = model_config_rec%move_interval(id_id)
  RETURN
END SUBROUTINE nl_get_move_interval
SUBROUTINE nl_get_move_cd_x ( id_id , move_cd_x )
  integer , INTENT(OUT) :: move_cd_x
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%num_moves ) THEN
    WRITE(emess,*)'nl_get_move_cd_x: Out of range move number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  move_cd_x = model_config_rec%move_cd_x(id_id)
  RETURN
END SUBROUTINE nl_get_move_cd_x
SUBROUTINE nl_get_move_cd_y ( id_id , move_cd_y )
  integer , INTENT(OUT) :: move_cd_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%num_moves ) THEN
    WRITE(emess,*)'nl_get_move_cd_y: Out of range move number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  move_cd_y = model_config_rec%move_cd_y(id_id)
  RETURN
END SUBROUTINE nl_get_move_cd_y
SUBROUTINE nl_get_swap_x ( id_id , swap_x )
  logical , INTENT(OUT) :: swap_x
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_swap_x: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  swap_x = model_config_rec%swap_x(id_id)
  RETURN
END SUBROUTINE nl_get_swap_x
SUBROUTINE nl_get_swap_y ( id_id , swap_y )
  logical , INTENT(OUT) :: swap_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_swap_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  swap_y = model_config_rec%swap_y(id_id)
  RETURN
END SUBROUTINE nl_get_swap_y
SUBROUTINE nl_get_cycle_x ( id_id , cycle_x )
  logical , INTENT(OUT) :: cycle_x
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_cycle_x: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  cycle_x = model_config_rec%cycle_x(id_id)
  RETURN
END SUBROUTINE nl_get_cycle_x
SUBROUTINE nl_get_cycle_y ( id_id , cycle_y )
  logical , INTENT(OUT) :: cycle_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_cycle_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  cycle_y = model_config_rec%cycle_y(id_id)
  RETURN
END SUBROUTINE nl_get_cycle_y
SUBROUTINE nl_get_reorder_mesh ( id_id , reorder_mesh )
  logical , INTENT(OUT) :: reorder_mesh
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_reorder_mesh: reorder_mesh applies to all domains. First arg ignored.')
  ENDIF
  reorder_mesh = model_config_rec%reorder_mesh
  RETURN
END SUBROUTINE nl_get_reorder_mesh
SUBROUTINE nl_get_perturb_input ( id_id , perturb_input )
  logical , INTENT(OUT) :: perturb_input
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_perturb_input: perturb_input applies to all domains. First arg ignored.')
  ENDIF
  perturb_input = model_config_rec%perturb_input
  RETURN
END SUBROUTINE nl_get_perturb_input
SUBROUTINE nl_get_eta_levels ( id_id , eta_levels )
  real , INTENT(OUT) :: eta_levels
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%e_vert(1) ) THEN
    WRITE(emess,*)'nl_get_eta_levels: Out of range eta_level number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  eta_levels = model_config_rec%eta_levels(id_id)
  RETURN
END SUBROUTINE nl_get_eta_levels
SUBROUTINE nl_get_ptsgm ( id_id , ptsgm )
  real , INTENT(OUT) :: ptsgm
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_ptsgm: ptsgm applies to all domains. First arg ignored.')
  ENDIF
  ptsgm = model_config_rec%ptsgm
  RETURN
END SUBROUTINE nl_get_ptsgm
SUBROUTINE nl_get_num_metgrid_levels ( id_id , num_metgrid_levels )
  integer , INTENT(OUT) :: num_metgrid_levels
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_num_metgrid_levels: num_metgrid_levels applies to all domains. First arg ignored.')
  ENDIF
  num_metgrid_levels = model_config_rec%num_metgrid_levels
  RETURN
END SUBROUTINE nl_get_num_metgrid_levels
SUBROUTINE nl_get_p_top_requested ( id_id , p_top_requested )
  real , INTENT(OUT) :: p_top_requested
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_p_top_requested: p_top_requested applies to all domains. First arg ignored.')
  ENDIF
  p_top_requested = model_config_rec%p_top_requested
  RETURN
END SUBROUTINE nl_get_p_top_requested
SUBROUTINE nl_get_mp_physics ( id_id , mp_physics )
  integer , INTENT(OUT) :: mp_physics
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_mp_physics: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  mp_physics = model_config_rec%mp_physics(id_id)
  RETURN
END SUBROUTINE nl_get_mp_physics
SUBROUTINE nl_get_ra_lw_physics ( id_id , ra_lw_physics )
  integer , INTENT(OUT) :: ra_lw_physics
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_ra_lw_physics: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  ra_lw_physics = model_config_rec%ra_lw_physics(id_id)
  RETURN
END SUBROUTINE nl_get_ra_lw_physics
SUBROUTINE nl_get_ra_sw_physics ( id_id , ra_sw_physics )
  integer , INTENT(OUT) :: ra_sw_physics
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_ra_sw_physics: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  ra_sw_physics = model_config_rec%ra_sw_physics(id_id)
  RETURN
END SUBROUTINE nl_get_ra_sw_physics
SUBROUTINE nl_get_radt ( id_id , radt )
  real , INTENT(OUT) :: radt
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_radt: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  radt = model_config_rec%radt(id_id)
  RETURN
END SUBROUTINE nl_get_radt
SUBROUTINE nl_get_sf_sfclay_physics ( id_id , sf_sfclay_physics )
  integer , INTENT(OUT) :: sf_sfclay_physics
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_sf_sfclay_physics: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  sf_sfclay_physics = model_config_rec%sf_sfclay_physics(id_id)
  RETURN
END SUBROUTINE nl_get_sf_sfclay_physics
SUBROUTINE nl_get_sf_surface_physics ( id_id , sf_surface_physics )
  integer , INTENT(OUT) :: sf_surface_physics
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_sf_surface_physics: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  sf_surface_physics = model_config_rec%sf_surface_physics(id_id)
  RETURN
END SUBROUTINE nl_get_sf_surface_physics
SUBROUTINE nl_get_bl_pbl_physics ( id_id , bl_pbl_physics )
  integer , INTENT(OUT) :: bl_pbl_physics
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_bl_pbl_physics: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  bl_pbl_physics = model_config_rec%bl_pbl_physics(id_id)
  RETURN
END SUBROUTINE nl_get_bl_pbl_physics
SUBROUTINE nl_get_bldt ( id_id , bldt )
  real , INTENT(OUT) :: bldt
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_bldt: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  bldt = model_config_rec%bldt(id_id)
  RETURN
END SUBROUTINE nl_get_bldt
SUBROUTINE nl_get_cu_physics ( id_id , cu_physics )
  integer , INTENT(OUT) :: cu_physics
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_cu_physics: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  cu_physics = model_config_rec%cu_physics(id_id)
  RETURN
END SUBROUTINE nl_get_cu_physics
SUBROUTINE nl_get_cudt ( id_id , cudt )
  real , INTENT(OUT) :: cudt
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_cudt: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  cudt = model_config_rec%cudt(id_id)
  RETURN
END SUBROUTINE nl_get_cudt
SUBROUTINE nl_get_gsmdt ( id_id , gsmdt )
  real , INTENT(OUT) :: gsmdt
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_gsmdt: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  gsmdt = model_config_rec%gsmdt(id_id)
  RETURN
END SUBROUTINE nl_get_gsmdt
SUBROUTINE nl_get_isfflx ( id_id , isfflx )
  integer , INTENT(OUT) :: isfflx
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_isfflx: isfflx applies to all domains. First arg ignored.')
  ENDIF
  isfflx = model_config_rec%isfflx
  RETURN
END SUBROUTINE nl_get_isfflx
SUBROUTINE nl_get_ifsnow ( id_id , ifsnow )
  integer , INTENT(OUT) :: ifsnow
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_ifsnow: ifsnow applies to all domains. First arg ignored.')
  ENDIF
  ifsnow = model_config_rec%ifsnow
  RETURN
END SUBROUTINE nl_get_ifsnow
SUBROUTINE nl_get_icloud ( id_id , icloud )
  integer , INTENT(OUT) :: icloud
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_icloud: icloud applies to all domains. First arg ignored.')
  ENDIF
  icloud = model_config_rec%icloud
  RETURN
END SUBROUTINE nl_get_icloud
SUBROUTINE nl_get_swrad_scat ( id_id , swrad_scat )
  real , INTENT(OUT) :: swrad_scat
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_swrad_scat: swrad_scat applies to all domains. First arg ignored.')
  ENDIF
  swrad_scat = model_config_rec%swrad_scat
  RETURN
END SUBROUTINE nl_get_swrad_scat
SUBROUTINE nl_get_surface_input_source ( id_id , surface_input_source )
  integer , INTENT(OUT) :: surface_input_source
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_surface_input_source: surface_input_source applies to all domains. First arg ignored.')
  ENDIF
  surface_input_source = model_config_rec%surface_input_source
  RETURN
END SUBROUTINE nl_get_surface_input_source
SUBROUTINE nl_get_num_soil_layers ( id_id , num_soil_layers )
  integer , INTENT(OUT) :: num_soil_layers
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_num_soil_layers: num_soil_layers applies to all domains. First arg ignored.')
  ENDIF
  num_soil_layers = model_config_rec%num_soil_layers
  RETURN
END SUBROUTINE nl_get_num_soil_layers
SUBROUTINE nl_get_maxiens ( id_id , maxiens )
  integer , INTENT(OUT) :: maxiens
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_maxiens: maxiens applies to all domains. First arg ignored.')
  ENDIF
  maxiens = model_config_rec%maxiens
  RETURN
END SUBROUTINE nl_get_maxiens
SUBROUTINE nl_get_maxens ( id_id , maxens )
  integer , INTENT(OUT) :: maxens
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_maxens: maxens applies to all domains. First arg ignored.')
  ENDIF
  maxens = model_config_rec%maxens
  RETURN
END SUBROUTINE nl_get_maxens
SUBROUTINE nl_get_maxens2 ( id_id , maxens2 )
  integer , INTENT(OUT) :: maxens2
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_maxens2: maxens2 applies to all domains. First arg ignored.')
  ENDIF
  maxens2 = model_config_rec%maxens2
  RETURN
END SUBROUTINE nl_get_maxens2
SUBROUTINE nl_get_maxens3 ( id_id , maxens3 )
  integer , INTENT(OUT) :: maxens3
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_maxens3: maxens3 applies to all domains. First arg ignored.')
  ENDIF
  maxens3 = model_config_rec%maxens3
  RETURN
END SUBROUTINE nl_get_maxens3
SUBROUTINE nl_get_ensdim ( id_id , ensdim )
  integer , INTENT(OUT) :: ensdim
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_ensdim: ensdim applies to all domains. First arg ignored.')
  ENDIF
  ensdim = model_config_rec%ensdim
  RETURN
END SUBROUTINE nl_get_ensdim
SUBROUTINE nl_get_chem_opt ( id_id , chem_opt )
  integer , INTENT(OUT) :: chem_opt
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_chem_opt: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  chem_opt = model_config_rec%chem_opt(id_id)
  RETURN
END SUBROUTINE nl_get_chem_opt
SUBROUTINE nl_get_num_land_cat ( id_id , num_land_cat )
  integer , INTENT(OUT) :: num_land_cat
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_num_land_cat: num_land_cat applies to all domains. First arg ignored.')
  ENDIF
  num_land_cat = model_config_rec%num_land_cat
  RETURN
END SUBROUTINE nl_get_num_land_cat
SUBROUTINE nl_get_num_soil_cat ( id_id , num_soil_cat )
  integer , INTENT(OUT) :: num_soil_cat
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_num_soil_cat: num_soil_cat applies to all domains. First arg ignored.')
  ENDIF
  num_soil_cat = model_config_rec%num_soil_cat
  RETURN
END SUBROUTINE nl_get_num_soil_cat
SUBROUTINE nl_get_mp_zero_out ( id_id , mp_zero_out )
  integer , INTENT(OUT) :: mp_zero_out
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_mp_zero_out: mp_zero_out applies to all domains. First arg ignored.')
  ENDIF
  mp_zero_out = model_config_rec%mp_zero_out
  RETURN
END SUBROUTINE nl_get_mp_zero_out
SUBROUTINE nl_get_mp_zero_out_thresh ( id_id , mp_zero_out_thresh )
  real , INTENT(OUT) :: mp_zero_out_thresh
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_mp_zero_out_thresh: mp_zero_out_thresh applies to all domains. First arg ignored.')
  ENDIF
  mp_zero_out_thresh = model_config_rec%mp_zero_out_thresh
  RETURN
END SUBROUTINE nl_get_mp_zero_out_thresh
SUBROUTINE nl_get_seaice_threshold ( id_id , seaice_threshold )
  real , INTENT(OUT) :: seaice_threshold
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_seaice_threshold: seaice_threshold applies to all domains. First arg ignored.')
  ENDIF
  seaice_threshold = model_config_rec%seaice_threshold
  RETURN
END SUBROUTINE nl_get_seaice_threshold
SUBROUTINE nl_get_sst_update ( id_id , sst_update )
  integer , INTENT(OUT) :: sst_update
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_sst_update: sst_update applies to all domains. First arg ignored.')
  ENDIF
  sst_update = model_config_rec%sst_update
  RETURN
END SUBROUTINE nl_get_sst_update
SUBROUTINE nl_get_ucmcall ( id_id , ucmcall )
  integer , INTENT(OUT) :: ucmcall
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_ucmcall: ucmcall applies to all domains. First arg ignored.')
  ENDIF
  ucmcall = model_config_rec%ucmcall
  RETURN
END SUBROUTINE nl_get_ucmcall
SUBROUTINE nl_get_idtad ( id_id , idtad )
  integer , INTENT(OUT) :: idtad
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_idtad: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  idtad = model_config_rec%idtad(id_id)
  RETURN
END SUBROUTINE nl_get_idtad
SUBROUTINE nl_get_nsoil ( id_id , nsoil )
  integer , INTENT(OUT) :: nsoil
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_nsoil: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  nsoil = model_config_rec%nsoil(id_id)
  RETURN
END SUBROUTINE nl_get_nsoil
SUBROUTINE nl_get_nphs ( id_id , nphs )
  integer , INTENT(OUT) :: nphs
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_nphs: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  nphs = model_config_rec%nphs(id_id)
  RETURN
END SUBROUTINE nl_get_nphs
SUBROUTINE nl_get_ncnvc ( id_id , ncnvc )
  integer , INTENT(OUT) :: ncnvc
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_ncnvc: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  ncnvc = model_config_rec%ncnvc(id_id)
  RETURN
END SUBROUTINE nl_get_ncnvc
SUBROUTINE nl_get_nrads ( id_id , nrads )
  integer , INTENT(OUT) :: nrads
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_nrads: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  nrads = model_config_rec%nrads(id_id)
  RETURN
END SUBROUTINE nl_get_nrads
SUBROUTINE nl_get_nradl ( id_id , nradl )
  integer , INTENT(OUT) :: nradl
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_nradl: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  nradl = model_config_rec%nradl(id_id)
  RETURN
END SUBROUTINE nl_get_nradl
SUBROUTINE nl_get_tprec ( id_id , tprec )
  real , INTENT(OUT) :: tprec
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_tprec: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  tprec = model_config_rec%tprec(id_id)
  RETURN
END SUBROUTINE nl_get_tprec
SUBROUTINE nl_get_theat ( id_id , theat )
  real , INTENT(OUT) :: theat
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_theat: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  theat = model_config_rec%theat(id_id)
  RETURN
END SUBROUTINE nl_get_theat
SUBROUTINE nl_get_tclod ( id_id , tclod )
  real , INTENT(OUT) :: tclod
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_tclod: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  tclod = model_config_rec%tclod(id_id)
  RETURN
END SUBROUTINE nl_get_tclod
SUBROUTINE nl_get_trdsw ( id_id , trdsw )
  real , INTENT(OUT) :: trdsw
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_trdsw: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  trdsw = model_config_rec%trdsw(id_id)
  RETURN
END SUBROUTINE nl_get_trdsw
SUBROUTINE nl_get_trdlw ( id_id , trdlw )
  real , INTENT(OUT) :: trdlw
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_trdlw: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  trdlw = model_config_rec%trdlw(id_id)
  RETURN
END SUBROUTINE nl_get_trdlw
SUBROUTINE nl_get_tsrfc ( id_id , tsrfc )
  real , INTENT(OUT) :: tsrfc
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_tsrfc: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  tsrfc = model_config_rec%tsrfc(id_id)
  RETURN
END SUBROUTINE nl_get_tsrfc
SUBROUTINE nl_get_pcpflg ( id_id , pcpflg )
  logical , INTENT(OUT) :: pcpflg
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_pcpflg: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  pcpflg = model_config_rec%pcpflg(id_id)
  RETURN
END SUBROUTINE nl_get_pcpflg
SUBROUTINE nl_get_sigma ( id_id , sigma )
  integer , INTENT(OUT) :: sigma
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_sigma: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  sigma = model_config_rec%sigma(id_id)
  RETURN
END SUBROUTINE nl_get_sigma
SUBROUTINE nl_get_co2tf ( id_id , co2tf )
  integer , INTENT(OUT) :: co2tf
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_co2tf: co2tf applies to all domains. First arg ignored.')
  ENDIF
  co2tf = model_config_rec%co2tf
  RETURN
END SUBROUTINE nl_get_co2tf
SUBROUTINE nl_get_ra_call_offset ( id_id , ra_call_offset )
  integer , INTENT(OUT) :: ra_call_offset
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_ra_call_offset: ra_call_offset applies to all domains. First arg ignored.')
  ENDIF
  ra_call_offset = model_config_rec%ra_call_offset
  RETURN
END SUBROUTINE nl_get_ra_call_offset
SUBROUTINE nl_get_cam_abs_freq_s ( id_id , cam_abs_freq_s )
  real , INTENT(OUT) :: cam_abs_freq_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_cam_abs_freq_s: cam_abs_freq_s applies to all domains. First arg ignored.')
  ENDIF
  cam_abs_freq_s = model_config_rec%cam_abs_freq_s
  RETURN
END SUBROUTINE nl_get_cam_abs_freq_s
SUBROUTINE nl_get_levsiz ( id_id , levsiz )
  integer , INTENT(OUT) :: levsiz
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_levsiz: levsiz applies to all domains. First arg ignored.')
  ENDIF
  levsiz = model_config_rec%levsiz
  RETURN
END SUBROUTINE nl_get_levsiz
SUBROUTINE nl_get_paerlev ( id_id , paerlev )
  integer , INTENT(OUT) :: paerlev
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_paerlev: paerlev applies to all domains. First arg ignored.')
  ENDIF
  paerlev = model_config_rec%paerlev
  RETURN
END SUBROUTINE nl_get_paerlev
SUBROUTINE nl_get_cam_abs_dim1 ( id_id , cam_abs_dim1 )
  integer , INTENT(OUT) :: cam_abs_dim1
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_cam_abs_dim1: cam_abs_dim1 applies to all domains. First arg ignored.')
  ENDIF
  cam_abs_dim1 = model_config_rec%cam_abs_dim1
  RETURN
END SUBROUTINE nl_get_cam_abs_dim1
SUBROUTINE nl_get_cam_abs_dim2 ( id_id , cam_abs_dim2 )
  integer , INTENT(OUT) :: cam_abs_dim2
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_cam_abs_dim2: cam_abs_dim2 applies to all domains. First arg ignored.')
  ENDIF
  cam_abs_dim2 = model_config_rec%cam_abs_dim2
  RETURN
END SUBROUTINE nl_get_cam_abs_dim2
SUBROUTINE nl_get_cu_rad_feedback ( id_id , cu_rad_feedback )
  logical , INTENT(OUT) :: cu_rad_feedback
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_cu_rad_feedback: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  cu_rad_feedback = model_config_rec%cu_rad_feedback(id_id)
  RETURN
END SUBROUTINE nl_get_cu_rad_feedback
SUBROUTINE nl_get_dyn_opt ( id_id , dyn_opt )
  integer , INTENT(OUT) :: dyn_opt
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_dyn_opt: dyn_opt applies to all domains. First arg ignored.')
  ENDIF
  dyn_opt = model_config_rec%dyn_opt
  RETURN
END SUBROUTINE nl_get_dyn_opt
SUBROUTINE nl_get_rk_ord ( id_id , rk_ord )
  integer , INTENT(OUT) :: rk_ord
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_rk_ord: rk_ord applies to all domains. First arg ignored.')
  ENDIF
  rk_ord = model_config_rec%rk_ord
  RETURN
END SUBROUTINE nl_get_rk_ord
SUBROUTINE nl_get_w_damping ( id_id , w_damping )
  integer , INTENT(OUT) :: w_damping
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_w_damping: w_damping applies to all domains. First arg ignored.')
  ENDIF
  w_damping = model_config_rec%w_damping
  RETURN
END SUBROUTINE nl_get_w_damping
SUBROUTINE nl_get_diff_opt ( id_id , diff_opt )
  integer , INTENT(OUT) :: diff_opt
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_diff_opt: diff_opt applies to all domains. First arg ignored.')
  ENDIF
  diff_opt = model_config_rec%diff_opt
  RETURN
END SUBROUTINE nl_get_diff_opt
SUBROUTINE nl_get_km_opt ( id_id , km_opt )
  integer , INTENT(OUT) :: km_opt
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_km_opt: km_opt applies to all domains. First arg ignored.')
  ENDIF
  km_opt = model_config_rec%km_opt
  RETURN
END SUBROUTINE nl_get_km_opt
SUBROUTINE nl_get_damp_opt ( id_id , damp_opt )
  integer , INTENT(OUT) :: damp_opt
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_damp_opt: damp_opt applies to all domains. First arg ignored.')
  ENDIF
  damp_opt = model_config_rec%damp_opt
  RETURN
END SUBROUTINE nl_get_damp_opt
SUBROUTINE nl_get_zdamp ( id_id , zdamp )
  real , INTENT(OUT) :: zdamp
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_zdamp: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  zdamp = model_config_rec%zdamp(id_id)
  RETURN
END SUBROUTINE nl_get_zdamp
SUBROUTINE nl_get_base_pres ( id_id , base_pres )
  real , INTENT(OUT) :: base_pres
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_base_pres: base_pres applies to all domains. First arg ignored.')
  ENDIF
  base_pres = model_config_rec%base_pres
  RETURN
END SUBROUTINE nl_get_base_pres
SUBROUTINE nl_get_base_temp ( id_id , base_temp )
  real , INTENT(OUT) :: base_temp
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_base_temp: base_temp applies to all domains. First arg ignored.')
  ENDIF
  base_temp = model_config_rec%base_temp
  RETURN
END SUBROUTINE nl_get_base_temp
SUBROUTINE nl_get_base_lapse ( id_id , base_lapse )
  real , INTENT(OUT) :: base_lapse
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_base_lapse: base_lapse applies to all domains. First arg ignored.')
  ENDIF
  base_lapse = model_config_rec%base_lapse
  RETURN
END SUBROUTINE nl_get_base_lapse
SUBROUTINE nl_get_dampcoef ( id_id , dampcoef )
  real , INTENT(OUT) :: dampcoef
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_dampcoef: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  dampcoef = model_config_rec%dampcoef(id_id)
  RETURN
END SUBROUTINE nl_get_dampcoef
SUBROUTINE nl_get_khdif ( id_id , khdif )
  real , INTENT(OUT) :: khdif
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_khdif: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  khdif = model_config_rec%khdif(id_id)
  RETURN
END SUBROUTINE nl_get_khdif
SUBROUTINE nl_get_kvdif ( id_id , kvdif )
  real , INTENT(OUT) :: kvdif
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_kvdif: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  kvdif = model_config_rec%kvdif(id_id)
  RETURN
END SUBROUTINE nl_get_kvdif
SUBROUTINE nl_get_smdiv ( id_id , smdiv )
  real , INTENT(OUT) :: smdiv
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_smdiv: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  smdiv = model_config_rec%smdiv(id_id)
  RETURN
END SUBROUTINE nl_get_smdiv
SUBROUTINE nl_get_emdiv ( id_id , emdiv )
  real , INTENT(OUT) :: emdiv
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_emdiv: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  emdiv = model_config_rec%emdiv(id_id)
  RETURN
END SUBROUTINE nl_get_emdiv
SUBROUTINE nl_get_epssm ( id_id , epssm )
  real , INTENT(OUT) :: epssm
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_epssm: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  epssm = model_config_rec%epssm(id_id)
  RETURN
END SUBROUTINE nl_get_epssm
SUBROUTINE nl_get_non_hydrostatic ( id_id , non_hydrostatic )
  logical , INTENT(OUT) :: non_hydrostatic
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_non_hydrostatic: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  non_hydrostatic = model_config_rec%non_hydrostatic(id_id)
  RETURN
END SUBROUTINE nl_get_non_hydrostatic
SUBROUTINE nl_get_time_step_sound ( id_id , time_step_sound )
  integer , INTENT(OUT) :: time_step_sound
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_time_step_sound: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  time_step_sound = model_config_rec%time_step_sound(id_id)
  RETURN
END SUBROUTINE nl_get_time_step_sound
SUBROUTINE nl_get_h_mom_adv_order ( id_id , h_mom_adv_order )
  integer , INTENT(OUT) :: h_mom_adv_order
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_h_mom_adv_order: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  h_mom_adv_order = model_config_rec%h_mom_adv_order(id_id)
  RETURN
END SUBROUTINE nl_get_h_mom_adv_order
SUBROUTINE nl_get_v_mom_adv_order ( id_id , v_mom_adv_order )
  integer , INTENT(OUT) :: v_mom_adv_order
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_v_mom_adv_order: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  v_mom_adv_order = model_config_rec%v_mom_adv_order(id_id)
  RETURN
END SUBROUTINE nl_get_v_mom_adv_order
SUBROUTINE nl_get_h_sca_adv_order ( id_id , h_sca_adv_order )
  integer , INTENT(OUT) :: h_sca_adv_order
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_h_sca_adv_order: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  h_sca_adv_order = model_config_rec%h_sca_adv_order(id_id)
  RETURN
END SUBROUTINE nl_get_h_sca_adv_order
SUBROUTINE nl_get_v_sca_adv_order ( id_id , v_sca_adv_order )
  integer , INTENT(OUT) :: v_sca_adv_order
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_v_sca_adv_order: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  v_sca_adv_order = model_config_rec%v_sca_adv_order(id_id)
  RETURN
END SUBROUTINE nl_get_v_sca_adv_order
SUBROUTINE nl_get_top_radiation ( id_id , top_radiation )
  logical , INTENT(OUT) :: top_radiation
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_top_radiation: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  top_radiation = model_config_rec%top_radiation(id_id)
  RETURN
END SUBROUTINE nl_get_top_radiation
SUBROUTINE nl_get_mix_cr_len ( id_id , mix_cr_len )
  real , INTENT(OUT) :: mix_cr_len
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_mix_cr_len: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  mix_cr_len = model_config_rec%mix_cr_len(id_id)
  RETURN
END SUBROUTINE nl_get_mix_cr_len
SUBROUTINE nl_get_tke_upper_bound ( id_id , tke_upper_bound )
  real , INTENT(OUT) :: tke_upper_bound
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_tke_upper_bound: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  tke_upper_bound = model_config_rec%tke_upper_bound(id_id)
  RETURN
END SUBROUTINE nl_get_tke_upper_bound
SUBROUTINE nl_get_kh_tke_upper_bound ( id_id , kh_tke_upper_bound )
  real , INTENT(OUT) :: kh_tke_upper_bound
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_kh_tke_upper_bound: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  kh_tke_upper_bound = model_config_rec%kh_tke_upper_bound(id_id)
  RETURN
END SUBROUTINE nl_get_kh_tke_upper_bound
SUBROUTINE nl_get_kv_tke_upper_bound ( id_id , kv_tke_upper_bound )
  real , INTENT(OUT) :: kv_tke_upper_bound
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_kv_tke_upper_bound: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  kv_tke_upper_bound = model_config_rec%kv_tke_upper_bound(id_id)
  RETURN
END SUBROUTINE nl_get_kv_tke_upper_bound
SUBROUTINE nl_get_tke_drag_coefficient ( id_id , tke_drag_coefficient )
  real , INTENT(OUT) :: tke_drag_coefficient
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_tke_drag_coefficient: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  tke_drag_coefficient = model_config_rec%tke_drag_coefficient(id_id)
  RETURN
END SUBROUTINE nl_get_tke_drag_coefficient
SUBROUTINE nl_get_tke_heat_flux ( id_id , tke_heat_flux )
  real , INTENT(OUT) :: tke_heat_flux
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_tke_heat_flux: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  tke_heat_flux = model_config_rec%tke_heat_flux(id_id)
  RETURN
END SUBROUTINE nl_get_tke_heat_flux
SUBROUTINE nl_get_pert_coriolis ( id_id , pert_coriolis )
  logical , INTENT(OUT) :: pert_coriolis
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_pert_coriolis: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  pert_coriolis = model_config_rec%pert_coriolis(id_id)
  RETURN
END SUBROUTINE nl_get_pert_coriolis
SUBROUTINE nl_get_spec_bdy_width ( id_id , spec_bdy_width )
  integer , INTENT(OUT) :: spec_bdy_width
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_spec_bdy_width: spec_bdy_width applies to all domains. First arg ignored.')
  ENDIF
  spec_bdy_width = model_config_rec%spec_bdy_width
  RETURN
END SUBROUTINE nl_get_spec_bdy_width
SUBROUTINE nl_get_spec_zone ( id_id , spec_zone )
  integer , INTENT(OUT) :: spec_zone
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_spec_zone: spec_zone applies to all domains. First arg ignored.')
  ENDIF
  spec_zone = model_config_rec%spec_zone
  RETURN
END SUBROUTINE nl_get_spec_zone
SUBROUTINE nl_get_relax_zone ( id_id , relax_zone )
  integer , INTENT(OUT) :: relax_zone
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_relax_zone: relax_zone applies to all domains. First arg ignored.')
  ENDIF
  relax_zone = model_config_rec%relax_zone
  RETURN
END SUBROUTINE nl_get_relax_zone
SUBROUTINE nl_get_specified ( id_id , specified )
  logical , INTENT(OUT) :: specified
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_specified: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  specified = model_config_rec%specified(id_id)
  RETURN
END SUBROUTINE nl_get_specified
SUBROUTINE nl_get_periodic_x ( id_id , periodic_x )
  logical , INTENT(OUT) :: periodic_x
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_periodic_x: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  periodic_x = model_config_rec%periodic_x(id_id)
  RETURN
END SUBROUTINE nl_get_periodic_x
SUBROUTINE nl_get_symmetric_xs ( id_id , symmetric_xs )
  logical , INTENT(OUT) :: symmetric_xs
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_symmetric_xs: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  symmetric_xs = model_config_rec%symmetric_xs(id_id)
  RETURN
END SUBROUTINE nl_get_symmetric_xs
SUBROUTINE nl_get_symmetric_xe ( id_id , symmetric_xe )
  logical , INTENT(OUT) :: symmetric_xe
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_symmetric_xe: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  symmetric_xe = model_config_rec%symmetric_xe(id_id)
  RETURN
END SUBROUTINE nl_get_symmetric_xe
SUBROUTINE nl_get_open_xs ( id_id , open_xs )
  logical , INTENT(OUT) :: open_xs
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_open_xs: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  open_xs = model_config_rec%open_xs(id_id)
  RETURN
END SUBROUTINE nl_get_open_xs
SUBROUTINE nl_get_open_xe ( id_id , open_xe )
  logical , INTENT(OUT) :: open_xe
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_open_xe: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  open_xe = model_config_rec%open_xe(id_id)
  RETURN
END SUBROUTINE nl_get_open_xe
SUBROUTINE nl_get_periodic_y ( id_id , periodic_y )
  logical , INTENT(OUT) :: periodic_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_periodic_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  periodic_y = model_config_rec%periodic_y(id_id)
  RETURN
END SUBROUTINE nl_get_periodic_y
SUBROUTINE nl_get_symmetric_ys ( id_id , symmetric_ys )
  logical , INTENT(OUT) :: symmetric_ys
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_symmetric_ys: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  symmetric_ys = model_config_rec%symmetric_ys(id_id)
  RETURN
END SUBROUTINE nl_get_symmetric_ys
SUBROUTINE nl_get_symmetric_ye ( id_id , symmetric_ye )
  logical , INTENT(OUT) :: symmetric_ye
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_symmetric_ye: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  symmetric_ye = model_config_rec%symmetric_ye(id_id)
  RETURN
END SUBROUTINE nl_get_symmetric_ye
SUBROUTINE nl_get_open_ys ( id_id , open_ys )
  logical , INTENT(OUT) :: open_ys
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_open_ys: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  open_ys = model_config_rec%open_ys(id_id)
  RETURN
END SUBROUTINE nl_get_open_ys
SUBROUTINE nl_get_open_ye ( id_id , open_ye )
  logical , INTENT(OUT) :: open_ye
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_open_ye: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  open_ye = model_config_rec%open_ye(id_id)
  RETURN
END SUBROUTINE nl_get_open_ye
SUBROUTINE nl_get_nested ( id_id , nested )
  logical , INTENT(OUT) :: nested
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_nested: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  nested = model_config_rec%nested(id_id)
  RETURN
END SUBROUTINE nl_get_nested
SUBROUTINE nl_get_real_data_init_type ( id_id , real_data_init_type )
  integer , INTENT(OUT) :: real_data_init_type
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_real_data_init_type: real_data_init_type applies to all domains. First arg ignored.')
  ENDIF
  real_data_init_type = model_config_rec%real_data_init_type
  RETURN
END SUBROUTINE nl_get_real_data_init_type
SUBROUTINE nl_get_background_proc_id ( id_id , background_proc_id )
  integer , INTENT(OUT) :: background_proc_id
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_background_proc_id: background_proc_id applies to all domains. First arg ignored.')
  ENDIF
  background_proc_id = model_config_rec%background_proc_id
  RETURN
END SUBROUTINE nl_get_background_proc_id
SUBROUTINE nl_get_forecast_proc_id ( id_id , forecast_proc_id )
  integer , INTENT(OUT) :: forecast_proc_id
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_forecast_proc_id: forecast_proc_id applies to all domains. First arg ignored.')
  ENDIF
  forecast_proc_id = model_config_rec%forecast_proc_id
  RETURN
END SUBROUTINE nl_get_forecast_proc_id
SUBROUTINE nl_get_production_status ( id_id , production_status )
  integer , INTENT(OUT) :: production_status
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_production_status: production_status applies to all domains. First arg ignored.')
  ENDIF
  production_status = model_config_rec%production_status
  RETURN
END SUBROUTINE nl_get_production_status
SUBROUTINE nl_get_compression ( id_id , compression )
  integer , INTENT(OUT) :: compression
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_compression: compression applies to all domains. First arg ignored.')
  ENDIF
  compression = model_config_rec%compression
  RETURN
END SUBROUTINE nl_get_compression
SUBROUTINE nl_get_cen_lat ( id_id , cen_lat )
  real , INTENT(OUT) :: cen_lat
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_cen_lat: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  cen_lat = model_config_rec%cen_lat(id_id)
  RETURN
END SUBROUTINE nl_get_cen_lat
SUBROUTINE nl_get_cen_lon ( id_id , cen_lon )
  real , INTENT(OUT) :: cen_lon
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_cen_lon: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  cen_lon = model_config_rec%cen_lon(id_id)
  RETURN
END SUBROUTINE nl_get_cen_lon
SUBROUTINE nl_get_truelat1 ( id_id , truelat1 )
  real , INTENT(OUT) :: truelat1
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_truelat1: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  truelat1 = model_config_rec%truelat1(id_id)
  RETURN
END SUBROUTINE nl_get_truelat1
SUBROUTINE nl_get_truelat2 ( id_id , truelat2 )
  real , INTENT(OUT) :: truelat2
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_truelat2: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  truelat2 = model_config_rec%truelat2(id_id)
  RETURN
END SUBROUTINE nl_get_truelat2
SUBROUTINE nl_get_moad_cen_lat ( id_id , moad_cen_lat )
  real , INTENT(OUT) :: moad_cen_lat
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_moad_cen_lat: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  moad_cen_lat = model_config_rec%moad_cen_lat(id_id)
  RETURN
END SUBROUTINE nl_get_moad_cen_lat
SUBROUTINE nl_get_stand_lon ( id_id , stand_lon )
  real , INTENT(OUT) :: stand_lon
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_stand_lon: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  stand_lon = model_config_rec%stand_lon(id_id)
  RETURN
END SUBROUTINE nl_get_stand_lon
SUBROUTINE nl_get_bdyfrq ( id_id , bdyfrq )
  real , INTENT(OUT) :: bdyfrq
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_bdyfrq: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  bdyfrq = model_config_rec%bdyfrq(id_id)
  RETURN
END SUBROUTINE nl_get_bdyfrq
SUBROUTINE nl_get_iswater ( id_id , iswater )
  integer , INTENT(OUT) :: iswater
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_iswater: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  iswater = model_config_rec%iswater(id_id)
  RETURN
END SUBROUTINE nl_get_iswater
SUBROUTINE nl_get_isice ( id_id , isice )
  integer , INTENT(OUT) :: isice
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_isice: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  isice = model_config_rec%isice(id_id)
  RETURN
END SUBROUTINE nl_get_isice
SUBROUTINE nl_get_isurban ( id_id , isurban )
  integer , INTENT(OUT) :: isurban
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_isurban: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  isurban = model_config_rec%isurban(id_id)
  RETURN
END SUBROUTINE nl_get_isurban
SUBROUTINE nl_get_isoilwater ( id_id , isoilwater )
  integer , INTENT(OUT) :: isoilwater
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_isoilwater: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  isoilwater = model_config_rec%isoilwater(id_id)
  RETURN
END SUBROUTINE nl_get_isoilwater
SUBROUTINE nl_get_map_proj ( id_id , map_proj )
  integer , INTENT(OUT) :: map_proj
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_get_map_proj: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  map_proj = model_config_rec%map_proj(id_id)
  RETURN
END SUBROUTINE nl_get_map_proj
SUBROUTINE nl_get_simulation_start_year ( id_id , simulation_start_year )
  integer , INTENT(OUT) :: simulation_start_year
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_simulation_start_year: simulation_start_year applies to all domains. First arg ignored.')
  ENDIF
  simulation_start_year = model_config_rec%simulation_start_year
  RETURN
END SUBROUTINE nl_get_simulation_start_year
SUBROUTINE nl_get_simulation_start_month ( id_id , simulation_start_month )
  integer , INTENT(OUT) :: simulation_start_month
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_simulation_start_month: simulation_start_month applies to all domains. First arg ignored.')
  ENDIF
  simulation_start_month = model_config_rec%simulation_start_month
  RETURN
END SUBROUTINE nl_get_simulation_start_month
SUBROUTINE nl_get_simulation_start_day ( id_id , simulation_start_day )
  integer , INTENT(OUT) :: simulation_start_day
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_simulation_start_day: simulation_start_day applies to all domains. First arg ignored.')
  ENDIF
  simulation_start_day = model_config_rec%simulation_start_day
  RETURN
END SUBROUTINE nl_get_simulation_start_day
SUBROUTINE nl_get_simulation_start_hour ( id_id , simulation_start_hour )
  integer , INTENT(OUT) :: simulation_start_hour
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_simulation_start_hour: simulation_start_hour applies to all domains. First arg ignored.')
  ENDIF
  simulation_start_hour = model_config_rec%simulation_start_hour
  RETURN
END SUBROUTINE nl_get_simulation_start_hour
SUBROUTINE nl_get_simulation_start_minute ( id_id , simulation_start_minute )
  integer , INTENT(OUT) :: simulation_start_minute
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_simulation_start_minute: simulation_start_minute applies to all domains. First arg ignored.')
  ENDIF
  simulation_start_minute = model_config_rec%simulation_start_minute
  RETURN
END SUBROUTINE nl_get_simulation_start_minute
SUBROUTINE nl_get_simulation_start_second ( id_id , simulation_start_second )
  integer , INTENT(OUT) :: simulation_start_second
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_get_simulation_start_second: simulation_start_second applies to all domains. First arg ignored.')
  ENDIF
  simulation_start_second = model_config_rec%simulation_start_second
  RETURN
END SUBROUTINE nl_get_simulation_start_second
SUBROUTINE nl_set_run_days ( id_id , run_days )
  integer , INTENT(IN) :: run_days
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_run_days: run_days applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%run_days = run_days 
  RETURN
END SUBROUTINE nl_set_run_days
SUBROUTINE nl_set_run_hours ( id_id , run_hours )
  integer , INTENT(IN) :: run_hours
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_run_hours: run_hours applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%run_hours = run_hours 
  RETURN
END SUBROUTINE nl_set_run_hours
SUBROUTINE nl_set_run_minutes ( id_id , run_minutes )
  integer , INTENT(IN) :: run_minutes
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_run_minutes: run_minutes applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%run_minutes = run_minutes 
  RETURN
END SUBROUTINE nl_set_run_minutes
SUBROUTINE nl_set_run_seconds ( id_id , run_seconds )
  integer , INTENT(IN) :: run_seconds
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_run_seconds: run_seconds applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%run_seconds = run_seconds 
  RETURN
END SUBROUTINE nl_set_run_seconds
SUBROUTINE nl_set_start_year ( id_id , start_year )
  integer , INTENT(IN) :: start_year
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_start_year: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%start_year(id_id) = start_year
  RETURN
END SUBROUTINE nl_set_start_year
SUBROUTINE nl_set_start_month ( id_id , start_month )
  integer , INTENT(IN) :: start_month
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_start_month: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%start_month(id_id) = start_month
  RETURN
END SUBROUTINE nl_set_start_month
SUBROUTINE nl_set_start_day ( id_id , start_day )
  integer , INTENT(IN) :: start_day
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_start_day: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%start_day(id_id) = start_day
  RETURN
END SUBROUTINE nl_set_start_day
SUBROUTINE nl_set_start_hour ( id_id , start_hour )
  integer , INTENT(IN) :: start_hour
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_start_hour: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%start_hour(id_id) = start_hour
  RETURN
END SUBROUTINE nl_set_start_hour
SUBROUTINE nl_set_start_minute ( id_id , start_minute )
  integer , INTENT(IN) :: start_minute
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_start_minute: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%start_minute(id_id) = start_minute
  RETURN
END SUBROUTINE nl_set_start_minute
SUBROUTINE nl_set_start_second ( id_id , start_second )
  integer , INTENT(IN) :: start_second
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_start_second: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%start_second(id_id) = start_second
  RETURN
END SUBROUTINE nl_set_start_second
SUBROUTINE nl_set_end_year ( id_id , end_year )
  integer , INTENT(IN) :: end_year
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_end_year: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%end_year(id_id) = end_year
  RETURN
END SUBROUTINE nl_set_end_year
SUBROUTINE nl_set_end_month ( id_id , end_month )
  integer , INTENT(IN) :: end_month
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_end_month: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%end_month(id_id) = end_month
  RETURN
END SUBROUTINE nl_set_end_month
SUBROUTINE nl_set_end_day ( id_id , end_day )
  integer , INTENT(IN) :: end_day
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_end_day: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%end_day(id_id) = end_day
  RETURN
END SUBROUTINE nl_set_end_day
SUBROUTINE nl_set_end_hour ( id_id , end_hour )
  integer , INTENT(IN) :: end_hour
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_end_hour: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%end_hour(id_id) = end_hour
  RETURN
END SUBROUTINE nl_set_end_hour
SUBROUTINE nl_set_end_minute ( id_id , end_minute )
  integer , INTENT(IN) :: end_minute
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_end_minute: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%end_minute(id_id) = end_minute
  RETURN
END SUBROUTINE nl_set_end_minute
SUBROUTINE nl_set_end_second ( id_id , end_second )
  integer , INTENT(IN) :: end_second
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_end_second: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%end_second(id_id) = end_second
  RETURN
END SUBROUTINE nl_set_end_second
SUBROUTINE nl_set_interval_seconds ( id_id , interval_seconds )
  integer , INTENT(IN) :: interval_seconds
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_interval_seconds: interval_seconds applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%interval_seconds = interval_seconds 
  RETURN
END SUBROUTINE nl_set_interval_seconds
SUBROUTINE nl_set_input_from_file ( id_id , input_from_file )
  logical , INTENT(IN) :: input_from_file
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_input_from_file: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%input_from_file(id_id) = input_from_file
  RETURN
END SUBROUTINE nl_set_input_from_file
SUBROUTINE nl_set_fine_input_stream ( id_id , fine_input_stream )
  integer , INTENT(IN) :: fine_input_stream
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_fine_input_stream: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%fine_input_stream(id_id) = fine_input_stream
  RETURN
END SUBROUTINE nl_set_fine_input_stream
SUBROUTINE nl_set_history_interval ( id_id , history_interval )
  integer , INTENT(IN) :: history_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_history_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%history_interval(id_id) = history_interval
  RETURN
END SUBROUTINE nl_set_history_interval
SUBROUTINE nl_set_frames_per_outfile ( id_id , frames_per_outfile )
  integer , INTENT(IN) :: frames_per_outfile
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_frames_per_outfile: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%frames_per_outfile(id_id) = frames_per_outfile
  RETURN
END SUBROUTINE nl_set_frames_per_outfile
SUBROUTINE nl_set_frames_per_auxhist1 ( id_id , frames_per_auxhist1 )
  integer , INTENT(IN) :: frames_per_auxhist1
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_frames_per_auxhist1: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%frames_per_auxhist1(id_id) = frames_per_auxhist1
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist1
SUBROUTINE nl_set_frames_per_auxhist2 ( id_id , frames_per_auxhist2 )
  integer , INTENT(IN) :: frames_per_auxhist2
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_frames_per_auxhist2: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%frames_per_auxhist2(id_id) = frames_per_auxhist2
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist2
SUBROUTINE nl_set_frames_per_auxhist3 ( id_id , frames_per_auxhist3 )
  integer , INTENT(IN) :: frames_per_auxhist3
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_frames_per_auxhist3: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%frames_per_auxhist3(id_id) = frames_per_auxhist3
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist3
SUBROUTINE nl_set_frames_per_auxhist4 ( id_id , frames_per_auxhist4 )
  integer , INTENT(IN) :: frames_per_auxhist4
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_frames_per_auxhist4: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%frames_per_auxhist4(id_id) = frames_per_auxhist4
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist4
SUBROUTINE nl_set_frames_per_auxhist5 ( id_id , frames_per_auxhist5 )
  integer , INTENT(IN) :: frames_per_auxhist5
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_frames_per_auxhist5: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%frames_per_auxhist5(id_id) = frames_per_auxhist5
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist5
SUBROUTINE nl_set_frames_per_auxhist6 ( id_id , frames_per_auxhist6 )
  integer , INTENT(IN) :: frames_per_auxhist6
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_frames_per_auxhist6: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%frames_per_auxhist6(id_id) = frames_per_auxhist6
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist6
SUBROUTINE nl_set_frames_per_auxhist7 ( id_id , frames_per_auxhist7 )
  integer , INTENT(IN) :: frames_per_auxhist7
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_frames_per_auxhist7: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%frames_per_auxhist7(id_id) = frames_per_auxhist7
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist7
SUBROUTINE nl_set_frames_per_auxhist8 ( id_id , frames_per_auxhist8 )
  integer , INTENT(IN) :: frames_per_auxhist8
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_frames_per_auxhist8: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%frames_per_auxhist8(id_id) = frames_per_auxhist8
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist8
SUBROUTINE nl_set_frames_per_auxhist9 ( id_id , frames_per_auxhist9 )
  integer , INTENT(IN) :: frames_per_auxhist9
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_frames_per_auxhist9: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%frames_per_auxhist9(id_id) = frames_per_auxhist9
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist9
SUBROUTINE nl_set_frames_per_auxhist10 ( id_id , frames_per_auxhist10 )
  integer , INTENT(IN) :: frames_per_auxhist10
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_frames_per_auxhist10: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%frames_per_auxhist10(id_id) = frames_per_auxhist10
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist10
SUBROUTINE nl_set_frames_per_auxhist11 ( id_id , frames_per_auxhist11 )
  integer , INTENT(IN) :: frames_per_auxhist11
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_frames_per_auxhist11: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%frames_per_auxhist11(id_id) = frames_per_auxhist11
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist11
SUBROUTINE nl_set_restart ( id_id , restart )
  logical , INTENT(IN) :: restart
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_restart: restart applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%restart = restart 
  RETURN
END SUBROUTINE nl_set_restart
SUBROUTINE nl_set_restart_interval ( id_id , restart_interval )
  integer , INTENT(IN) :: restart_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_restart_interval: restart_interval applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%restart_interval = restart_interval 
  RETURN
END SUBROUTINE nl_set_restart_interval
SUBROUTINE nl_set_io_form_input ( id_id , io_form_input )
  integer , INTENT(IN) :: io_form_input
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_io_form_input: io_form_input applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%io_form_input = io_form_input 
  RETURN
END SUBROUTINE nl_set_io_form_input
SUBROUTINE nl_set_io_form_history ( id_id , io_form_history )
  integer , INTENT(IN) :: io_form_history
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_io_form_history: io_form_history applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%io_form_history = io_form_history 
  RETURN
END SUBROUTINE nl_set_io_form_history
SUBROUTINE nl_set_io_form_restart ( id_id , io_form_restart )
  integer , INTENT(IN) :: io_form_restart
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_io_form_restart: io_form_restart applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%io_form_restart = io_form_restart 
  RETURN
END SUBROUTINE nl_set_io_form_restart
SUBROUTINE nl_set_io_form_boundary ( id_id , io_form_boundary )
  integer , INTENT(IN) :: io_form_boundary
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_io_form_boundary: io_form_boundary applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%io_form_boundary = io_form_boundary 
  RETURN
END SUBROUTINE nl_set_io_form_boundary
SUBROUTINE nl_set_debug_level ( id_id , debug_level )
  integer , INTENT(IN) :: debug_level
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_debug_level: debug_level applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%debug_level = debug_level 
  RETURN
END SUBROUTINE nl_set_debug_level
SUBROUTINE nl_set_self_test_domain ( id_id , self_test_domain )
  logical , INTENT(IN) :: self_test_domain
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_self_test_domain: self_test_domain applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%self_test_domain = self_test_domain 
  RETURN
END SUBROUTINE nl_set_self_test_domain
SUBROUTINE nl_set_history_outname ( id_id , history_outname )
  character*256 , INTENT(IN) :: history_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_history_outname: history_outname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%history_outname = trim(history_outname) 
  RETURN
END SUBROUTINE nl_set_history_outname
SUBROUTINE nl_set_auxhist1_outname ( id_id , auxhist1_outname )
  character*256 , INTENT(IN) :: auxhist1_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_auxhist1_outname: auxhist1_outname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxhist1_outname = trim(auxhist1_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist1_outname
SUBROUTINE nl_set_auxhist2_outname ( id_id , auxhist2_outname )
  character*256 , INTENT(IN) :: auxhist2_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_auxhist2_outname: auxhist2_outname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxhist2_outname = trim(auxhist2_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist2_outname
SUBROUTINE nl_set_auxhist3_outname ( id_id , auxhist3_outname )
  character*256 , INTENT(IN) :: auxhist3_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_auxhist3_outname: auxhist3_outname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxhist3_outname = trim(auxhist3_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist3_outname
SUBROUTINE nl_set_auxhist4_outname ( id_id , auxhist4_outname )
  character*256 , INTENT(IN) :: auxhist4_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_auxhist4_outname: auxhist4_outname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxhist4_outname = trim(auxhist4_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist4_outname
SUBROUTINE nl_set_auxhist5_outname ( id_id , auxhist5_outname )
  character*256 , INTENT(IN) :: auxhist5_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_auxhist5_outname: auxhist5_outname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxhist5_outname = trim(auxhist5_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist5_outname
SUBROUTINE nl_set_auxhist6_outname ( id_id , auxhist6_outname )
  character*256 , INTENT(IN) :: auxhist6_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_auxhist6_outname: auxhist6_outname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxhist6_outname = trim(auxhist6_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist6_outname
SUBROUTINE nl_set_auxhist7_outname ( id_id , auxhist7_outname )
  character*256 , INTENT(IN) :: auxhist7_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_auxhist7_outname: auxhist7_outname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxhist7_outname = trim(auxhist7_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist7_outname
SUBROUTINE nl_set_auxhist8_outname ( id_id , auxhist8_outname )
  character*256 , INTENT(IN) :: auxhist8_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_auxhist8_outname: auxhist8_outname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxhist8_outname = trim(auxhist8_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist8_outname
SUBROUTINE nl_set_auxhist9_outname ( id_id , auxhist9_outname )
  character*256 , INTENT(IN) :: auxhist9_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_auxhist9_outname: auxhist9_outname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxhist9_outname = trim(auxhist9_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist9_outname
SUBROUTINE nl_set_auxhist10_outname ( id_id , auxhist10_outname )
  character*256 , INTENT(IN) :: auxhist10_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_auxhist10_outname: auxhist10_outname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxhist10_outname = trim(auxhist10_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist10_outname
SUBROUTINE nl_set_auxhist11_outname ( id_id , auxhist11_outname )
  character*256 , INTENT(IN) :: auxhist11_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_auxhist11_outname: auxhist11_outname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxhist11_outname = trim(auxhist11_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist11_outname
SUBROUTINE nl_set_history_inname ( id_id , history_inname )
  character*256 , INTENT(IN) :: history_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_history_inname: history_inname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%history_inname = trim(history_inname) 
  RETURN
END SUBROUTINE nl_set_history_inname
SUBROUTINE nl_set_auxhist1_inname ( id_id , auxhist1_inname )
  character*256 , INTENT(IN) :: auxhist1_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_auxhist1_inname: auxhist1_inname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxhist1_inname = trim(auxhist1_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist1_inname
SUBROUTINE nl_set_auxhist2_inname ( id_id , auxhist2_inname )
  character*256 , INTENT(IN) :: auxhist2_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_auxhist2_inname: auxhist2_inname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxhist2_inname = trim(auxhist2_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist2_inname
SUBROUTINE nl_set_auxhist3_inname ( id_id , auxhist3_inname )
  character*256 , INTENT(IN) :: auxhist3_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_auxhist3_inname: auxhist3_inname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxhist3_inname = trim(auxhist3_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist3_inname
SUBROUTINE nl_set_auxhist4_inname ( id_id , auxhist4_inname )
  character*256 , INTENT(IN) :: auxhist4_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_auxhist4_inname: auxhist4_inname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxhist4_inname = trim(auxhist4_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist4_inname
SUBROUTINE nl_set_auxhist5_inname ( id_id , auxhist5_inname )
  character*256 , INTENT(IN) :: auxhist5_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_auxhist5_inname: auxhist5_inname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxhist5_inname = trim(auxhist5_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist5_inname
SUBROUTINE nl_set_auxhist6_inname ( id_id , auxhist6_inname )
  character*256 , INTENT(IN) :: auxhist6_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_auxhist6_inname: auxhist6_inname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxhist6_inname = trim(auxhist6_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist6_inname
SUBROUTINE nl_set_auxhist7_inname ( id_id , auxhist7_inname )
  character*256 , INTENT(IN) :: auxhist7_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_auxhist7_inname: auxhist7_inname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxhist7_inname = trim(auxhist7_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist7_inname
SUBROUTINE nl_set_auxhist8_inname ( id_id , auxhist8_inname )
  character*256 , INTENT(IN) :: auxhist8_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_auxhist8_inname: auxhist8_inname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxhist8_inname = trim(auxhist8_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist8_inname
SUBROUTINE nl_set_auxhist9_inname ( id_id , auxhist9_inname )
  character*256 , INTENT(IN) :: auxhist9_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_auxhist9_inname: auxhist9_inname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxhist9_inname = trim(auxhist9_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist9_inname
SUBROUTINE nl_set_auxhist10_inname ( id_id , auxhist10_inname )
  character*256 , INTENT(IN) :: auxhist10_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_auxhist10_inname: auxhist10_inname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxhist10_inname = trim(auxhist10_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist10_inname
SUBROUTINE nl_set_auxhist11_inname ( id_id , auxhist11_inname )
  character*256 , INTENT(IN) :: auxhist11_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_auxhist11_inname: auxhist11_inname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxhist11_inname = trim(auxhist11_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist11_inname
SUBROUTINE nl_set_auxinput1_outname ( id_id , auxinput1_outname )
  character*256 , INTENT(IN) :: auxinput1_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_auxinput1_outname: auxinput1_outname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxinput1_outname = trim(auxinput1_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput1_outname
SUBROUTINE nl_set_auxinput2_outname ( id_id , auxinput2_outname )
  character*256 , INTENT(IN) :: auxinput2_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_auxinput2_outname: auxinput2_outname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxinput2_outname = trim(auxinput2_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput2_outname
SUBROUTINE nl_set_auxinput3_outname ( id_id , auxinput3_outname )
  character*256 , INTENT(IN) :: auxinput3_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_auxinput3_outname: auxinput3_outname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxinput3_outname = trim(auxinput3_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput3_outname
SUBROUTINE nl_set_auxinput4_outname ( id_id , auxinput4_outname )
  character*256 , INTENT(IN) :: auxinput4_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_auxinput4_outname: auxinput4_outname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxinput4_outname = trim(auxinput4_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput4_outname
SUBROUTINE nl_set_auxinput5_outname ( id_id , auxinput5_outname )
  character*256 , INTENT(IN) :: auxinput5_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_auxinput5_outname: auxinput5_outname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxinput5_outname = trim(auxinput5_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput5_outname
SUBROUTINE nl_set_auxinput6_outname ( id_id , auxinput6_outname )
  character*256 , INTENT(IN) :: auxinput6_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_auxinput6_outname: auxinput6_outname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxinput6_outname = trim(auxinput6_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput6_outname
SUBROUTINE nl_set_auxinput7_outname ( id_id , auxinput7_outname )
  character*256 , INTENT(IN) :: auxinput7_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_auxinput7_outname: auxinput7_outname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxinput7_outname = trim(auxinput7_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput7_outname
SUBROUTINE nl_set_auxinput8_outname ( id_id , auxinput8_outname )
  character*256 , INTENT(IN) :: auxinput8_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_auxinput8_outname: auxinput8_outname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxinput8_outname = trim(auxinput8_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput8_outname
SUBROUTINE nl_set_auxinput9_outname ( id_id , auxinput9_outname )
  character*256 , INTENT(IN) :: auxinput9_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_auxinput9_outname: auxinput9_outname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxinput9_outname = trim(auxinput9_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput9_outname
SUBROUTINE nl_set_auxinput10_outname ( id_id , auxinput10_outname )
  character*256 , INTENT(IN) :: auxinput10_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_auxinput10_outname: auxinput10_outname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxinput10_outname = trim(auxinput10_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput10_outname
SUBROUTINE nl_set_auxinput11_outname ( id_id , auxinput11_outname )
  character*256 , INTENT(IN) :: auxinput11_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_auxinput11_outname: auxinput11_outname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxinput11_outname = trim(auxinput11_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput11_outname
SUBROUTINE nl_set_auxinput1_inname ( id_id , auxinput1_inname )
  character*256 , INTENT(IN) :: auxinput1_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_auxinput1_inname: auxinput1_inname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxinput1_inname = trim(auxinput1_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput1_inname
SUBROUTINE nl_set_auxinput2_inname ( id_id , auxinput2_inname )
  character*256 , INTENT(IN) :: auxinput2_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_auxinput2_inname: auxinput2_inname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxinput2_inname = trim(auxinput2_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput2_inname
SUBROUTINE nl_set_auxinput3_inname ( id_id , auxinput3_inname )
  character*256 , INTENT(IN) :: auxinput3_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_auxinput3_inname: auxinput3_inname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxinput3_inname = trim(auxinput3_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput3_inname
SUBROUTINE nl_set_auxinput4_inname ( id_id , auxinput4_inname )
  character*256 , INTENT(IN) :: auxinput4_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_auxinput4_inname: auxinput4_inname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxinput4_inname = trim(auxinput4_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput4_inname
SUBROUTINE nl_set_auxinput5_inname ( id_id , auxinput5_inname )
  character*256 , INTENT(IN) :: auxinput5_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_auxinput5_inname: auxinput5_inname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxinput5_inname = trim(auxinput5_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput5_inname
SUBROUTINE nl_set_auxinput6_inname ( id_id , auxinput6_inname )
  character*256 , INTENT(IN) :: auxinput6_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_auxinput6_inname: auxinput6_inname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxinput6_inname = trim(auxinput6_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput6_inname
SUBROUTINE nl_set_auxinput7_inname ( id_id , auxinput7_inname )
  character*256 , INTENT(IN) :: auxinput7_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_auxinput7_inname: auxinput7_inname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxinput7_inname = trim(auxinput7_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput7_inname
SUBROUTINE nl_set_auxinput8_inname ( id_id , auxinput8_inname )
  character*256 , INTENT(IN) :: auxinput8_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_auxinput8_inname: auxinput8_inname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxinput8_inname = trim(auxinput8_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput8_inname
SUBROUTINE nl_set_auxinput9_inname ( id_id , auxinput9_inname )
  character*256 , INTENT(IN) :: auxinput9_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_auxinput9_inname: auxinput9_inname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxinput9_inname = trim(auxinput9_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput9_inname
SUBROUTINE nl_set_gfdda_inname ( id_id , gfdda_inname )
  character*256 , INTENT(IN) :: gfdda_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_gfdda_inname: gfdda_inname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%gfdda_inname = trim(gfdda_inname) 
  RETURN
END SUBROUTINE nl_set_gfdda_inname
SUBROUTINE nl_set_auxinput11_inname ( id_id , auxinput11_inname )
  character*256 , INTENT(IN) :: auxinput11_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_auxinput11_inname: auxinput11_inname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%auxinput11_inname = trim(auxinput11_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput11_inname
SUBROUTINE nl_set_history_interval_mo ( id_id , history_interval_mo )
  integer , INTENT(IN) :: history_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_history_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%history_interval_mo(id_id) = history_interval_mo
  RETURN
END SUBROUTINE nl_set_history_interval_mo
SUBROUTINE nl_set_history_interval_d ( id_id , history_interval_d )
  integer , INTENT(IN) :: history_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_history_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%history_interval_d(id_id) = history_interval_d
  RETURN
END SUBROUTINE nl_set_history_interval_d
SUBROUTINE nl_set_history_interval_h ( id_id , history_interval_h )
  integer , INTENT(IN) :: history_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_history_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%history_interval_h(id_id) = history_interval_h
  RETURN
END SUBROUTINE nl_set_history_interval_h
SUBROUTINE nl_set_history_interval_m ( id_id , history_interval_m )
  integer , INTENT(IN) :: history_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_history_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%history_interval_m(id_id) = history_interval_m
  RETURN
END SUBROUTINE nl_set_history_interval_m
SUBROUTINE nl_set_history_interval_s ( id_id , history_interval_s )
  integer , INTENT(IN) :: history_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_history_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%history_interval_s(id_id) = history_interval_s
  RETURN
END SUBROUTINE nl_set_history_interval_s
SUBROUTINE nl_set_inputout_interval_mo ( id_id , inputout_interval_mo )
  integer , INTENT(IN) :: inputout_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_inputout_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%inputout_interval_mo(id_id) = inputout_interval_mo
  RETURN
END SUBROUTINE nl_set_inputout_interval_mo
SUBROUTINE nl_set_inputout_interval_d ( id_id , inputout_interval_d )
  integer , INTENT(IN) :: inputout_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_inputout_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%inputout_interval_d(id_id) = inputout_interval_d
  RETURN
END SUBROUTINE nl_set_inputout_interval_d
SUBROUTINE nl_set_inputout_interval_h ( id_id , inputout_interval_h )
  integer , INTENT(IN) :: inputout_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_inputout_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%inputout_interval_h(id_id) = inputout_interval_h
  RETURN
END SUBROUTINE nl_set_inputout_interval_h
SUBROUTINE nl_set_inputout_interval_m ( id_id , inputout_interval_m )
  integer , INTENT(IN) :: inputout_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_inputout_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%inputout_interval_m(id_id) = inputout_interval_m
  RETURN
END SUBROUTINE nl_set_inputout_interval_m
SUBROUTINE nl_set_inputout_interval_s ( id_id , inputout_interval_s )
  integer , INTENT(IN) :: inputout_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_inputout_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%inputout_interval_s(id_id) = inputout_interval_s
  RETURN
END SUBROUTINE nl_set_inputout_interval_s
SUBROUTINE nl_set_inputout_interval ( id_id , inputout_interval )
  integer , INTENT(IN) :: inputout_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_inputout_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%inputout_interval(id_id) = inputout_interval
  RETURN
END SUBROUTINE nl_set_inputout_interval
SUBROUTINE nl_set_auxhist1_interval_mo ( id_id , auxhist1_interval_mo )
  integer , INTENT(IN) :: auxhist1_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist1_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist1_interval_mo(id_id) = auxhist1_interval_mo
  RETURN
END SUBROUTINE nl_set_auxhist1_interval_mo
SUBROUTINE nl_set_auxhist1_interval_d ( id_id , auxhist1_interval_d )
  integer , INTENT(IN) :: auxhist1_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist1_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist1_interval_d(id_id) = auxhist1_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist1_interval_d
SUBROUTINE nl_set_auxhist1_interval_h ( id_id , auxhist1_interval_h )
  integer , INTENT(IN) :: auxhist1_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist1_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist1_interval_h(id_id) = auxhist1_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist1_interval_h
SUBROUTINE nl_set_auxhist1_interval_m ( id_id , auxhist1_interval_m )
  integer , INTENT(IN) :: auxhist1_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist1_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist1_interval_m(id_id) = auxhist1_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist1_interval_m
SUBROUTINE nl_set_auxhist1_interval_s ( id_id , auxhist1_interval_s )
  integer , INTENT(IN) :: auxhist1_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist1_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist1_interval_s(id_id) = auxhist1_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist1_interval_s
SUBROUTINE nl_set_auxhist1_interval ( id_id , auxhist1_interval )
  integer , INTENT(IN) :: auxhist1_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist1_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist1_interval(id_id) = auxhist1_interval
  RETURN
END SUBROUTINE nl_set_auxhist1_interval
SUBROUTINE nl_set_auxhist2_interval_mo ( id_id , auxhist2_interval_mo )
  integer , INTENT(IN) :: auxhist2_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist2_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist2_interval_mo(id_id) = auxhist2_interval_mo
  RETURN
END SUBROUTINE nl_set_auxhist2_interval_mo
SUBROUTINE nl_set_auxhist2_interval_d ( id_id , auxhist2_interval_d )
  integer , INTENT(IN) :: auxhist2_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist2_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist2_interval_d(id_id) = auxhist2_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist2_interval_d
SUBROUTINE nl_set_auxhist2_interval_h ( id_id , auxhist2_interval_h )
  integer , INTENT(IN) :: auxhist2_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist2_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist2_interval_h(id_id) = auxhist2_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist2_interval_h
SUBROUTINE nl_set_auxhist2_interval_m ( id_id , auxhist2_interval_m )
  integer , INTENT(IN) :: auxhist2_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist2_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist2_interval_m(id_id) = auxhist2_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist2_interval_m
SUBROUTINE nl_set_auxhist2_interval_s ( id_id , auxhist2_interval_s )
  integer , INTENT(IN) :: auxhist2_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist2_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist2_interval_s(id_id) = auxhist2_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist2_interval_s
SUBROUTINE nl_set_auxhist2_interval ( id_id , auxhist2_interval )
  integer , INTENT(IN) :: auxhist2_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist2_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist2_interval(id_id) = auxhist2_interval
  RETURN
END SUBROUTINE nl_set_auxhist2_interval
SUBROUTINE nl_set_auxhist3_interval_mo ( id_id , auxhist3_interval_mo )
  integer , INTENT(IN) :: auxhist3_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist3_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist3_interval_mo(id_id) = auxhist3_interval_mo
  RETURN
END SUBROUTINE nl_set_auxhist3_interval_mo
SUBROUTINE nl_set_auxhist3_interval_d ( id_id , auxhist3_interval_d )
  integer , INTENT(IN) :: auxhist3_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist3_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist3_interval_d(id_id) = auxhist3_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist3_interval_d
SUBROUTINE nl_set_auxhist3_interval_h ( id_id , auxhist3_interval_h )
  integer , INTENT(IN) :: auxhist3_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist3_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist3_interval_h(id_id) = auxhist3_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist3_interval_h
SUBROUTINE nl_set_auxhist3_interval_m ( id_id , auxhist3_interval_m )
  integer , INTENT(IN) :: auxhist3_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist3_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist3_interval_m(id_id) = auxhist3_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist3_interval_m
SUBROUTINE nl_set_auxhist3_interval_s ( id_id , auxhist3_interval_s )
  integer , INTENT(IN) :: auxhist3_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist3_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist3_interval_s(id_id) = auxhist3_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist3_interval_s
SUBROUTINE nl_set_auxhist3_interval ( id_id , auxhist3_interval )
  integer , INTENT(IN) :: auxhist3_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist3_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist3_interval(id_id) = auxhist3_interval
  RETURN
END SUBROUTINE nl_set_auxhist3_interval
SUBROUTINE nl_set_auxhist4_interval_mo ( id_id , auxhist4_interval_mo )
  integer , INTENT(IN) :: auxhist4_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist4_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist4_interval_mo(id_id) = auxhist4_interval_mo
  RETURN
END SUBROUTINE nl_set_auxhist4_interval_mo
SUBROUTINE nl_set_auxhist4_interval_d ( id_id , auxhist4_interval_d )
  integer , INTENT(IN) :: auxhist4_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist4_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist4_interval_d(id_id) = auxhist4_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist4_interval_d
SUBROUTINE nl_set_auxhist4_interval_h ( id_id , auxhist4_interval_h )
  integer , INTENT(IN) :: auxhist4_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist4_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist4_interval_h(id_id) = auxhist4_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist4_interval_h
SUBROUTINE nl_set_auxhist4_interval_m ( id_id , auxhist4_interval_m )
  integer , INTENT(IN) :: auxhist4_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist4_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist4_interval_m(id_id) = auxhist4_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist4_interval_m
SUBROUTINE nl_set_auxhist4_interval_s ( id_id , auxhist4_interval_s )
  integer , INTENT(IN) :: auxhist4_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist4_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist4_interval_s(id_id) = auxhist4_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist4_interval_s
SUBROUTINE nl_set_auxhist4_interval ( id_id , auxhist4_interval )
  integer , INTENT(IN) :: auxhist4_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist4_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist4_interval(id_id) = auxhist4_interval
  RETURN
END SUBROUTINE nl_set_auxhist4_interval
SUBROUTINE nl_set_auxhist5_interval_mo ( id_id , auxhist5_interval_mo )
  integer , INTENT(IN) :: auxhist5_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist5_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist5_interval_mo(id_id) = auxhist5_interval_mo
  RETURN
END SUBROUTINE nl_set_auxhist5_interval_mo
SUBROUTINE nl_set_auxhist5_interval_d ( id_id , auxhist5_interval_d )
  integer , INTENT(IN) :: auxhist5_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist5_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist5_interval_d(id_id) = auxhist5_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist5_interval_d
SUBROUTINE nl_set_auxhist5_interval_h ( id_id , auxhist5_interval_h )
  integer , INTENT(IN) :: auxhist5_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist5_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist5_interval_h(id_id) = auxhist5_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist5_interval_h
SUBROUTINE nl_set_auxhist5_interval_m ( id_id , auxhist5_interval_m )
  integer , INTENT(IN) :: auxhist5_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist5_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist5_interval_m(id_id) = auxhist5_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist5_interval_m
SUBROUTINE nl_set_auxhist5_interval_s ( id_id , auxhist5_interval_s )
  integer , INTENT(IN) :: auxhist5_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist5_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist5_interval_s(id_id) = auxhist5_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist5_interval_s
SUBROUTINE nl_set_auxhist5_interval ( id_id , auxhist5_interval )
  integer , INTENT(IN) :: auxhist5_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist5_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist5_interval(id_id) = auxhist5_interval
  RETURN
END SUBROUTINE nl_set_auxhist5_interval
SUBROUTINE nl_set_auxhist6_interval_mo ( id_id , auxhist6_interval_mo )
  integer , INTENT(IN) :: auxhist6_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist6_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist6_interval_mo(id_id) = auxhist6_interval_mo
  RETURN
END SUBROUTINE nl_set_auxhist6_interval_mo
SUBROUTINE nl_set_auxhist6_interval_d ( id_id , auxhist6_interval_d )
  integer , INTENT(IN) :: auxhist6_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist6_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist6_interval_d(id_id) = auxhist6_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist6_interval_d
SUBROUTINE nl_set_auxhist6_interval_h ( id_id , auxhist6_interval_h )
  integer , INTENT(IN) :: auxhist6_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist6_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist6_interval_h(id_id) = auxhist6_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist6_interval_h
SUBROUTINE nl_set_auxhist6_interval_m ( id_id , auxhist6_interval_m )
  integer , INTENT(IN) :: auxhist6_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist6_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist6_interval_m(id_id) = auxhist6_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist6_interval_m
SUBROUTINE nl_set_auxhist6_interval_s ( id_id , auxhist6_interval_s )
  integer , INTENT(IN) :: auxhist6_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist6_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist6_interval_s(id_id) = auxhist6_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist6_interval_s
SUBROUTINE nl_set_auxhist6_interval ( id_id , auxhist6_interval )
  integer , INTENT(IN) :: auxhist6_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist6_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist6_interval(id_id) = auxhist6_interval
  RETURN
END SUBROUTINE nl_set_auxhist6_interval
SUBROUTINE nl_set_auxhist7_interval_mo ( id_id , auxhist7_interval_mo )
  integer , INTENT(IN) :: auxhist7_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist7_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist7_interval_mo(id_id) = auxhist7_interval_mo
  RETURN
END SUBROUTINE nl_set_auxhist7_interval_mo
SUBROUTINE nl_set_auxhist7_interval_d ( id_id , auxhist7_interval_d )
  integer , INTENT(IN) :: auxhist7_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist7_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist7_interval_d(id_id) = auxhist7_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist7_interval_d
SUBROUTINE nl_set_auxhist7_interval_h ( id_id , auxhist7_interval_h )
  integer , INTENT(IN) :: auxhist7_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist7_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist7_interval_h(id_id) = auxhist7_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist7_interval_h
SUBROUTINE nl_set_auxhist7_interval_m ( id_id , auxhist7_interval_m )
  integer , INTENT(IN) :: auxhist7_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist7_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist7_interval_m(id_id) = auxhist7_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist7_interval_m
SUBROUTINE nl_set_auxhist7_interval_s ( id_id , auxhist7_interval_s )
  integer , INTENT(IN) :: auxhist7_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist7_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist7_interval_s(id_id) = auxhist7_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist7_interval_s
SUBROUTINE nl_set_auxhist7_interval ( id_id , auxhist7_interval )
  integer , INTENT(IN) :: auxhist7_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist7_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist7_interval(id_id) = auxhist7_interval
  RETURN
END SUBROUTINE nl_set_auxhist7_interval
SUBROUTINE nl_set_auxhist8_interval_mo ( id_id , auxhist8_interval_mo )
  integer , INTENT(IN) :: auxhist8_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist8_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist8_interval_mo(id_id) = auxhist8_interval_mo
  RETURN
END SUBROUTINE nl_set_auxhist8_interval_mo
SUBROUTINE nl_set_auxhist8_interval_d ( id_id , auxhist8_interval_d )
  integer , INTENT(IN) :: auxhist8_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist8_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist8_interval_d(id_id) = auxhist8_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist8_interval_d
SUBROUTINE nl_set_auxhist8_interval_h ( id_id , auxhist8_interval_h )
  integer , INTENT(IN) :: auxhist8_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist8_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist8_interval_h(id_id) = auxhist8_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist8_interval_h
SUBROUTINE nl_set_auxhist8_interval_m ( id_id , auxhist8_interval_m )
  integer , INTENT(IN) :: auxhist8_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist8_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist8_interval_m(id_id) = auxhist8_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist8_interval_m
SUBROUTINE nl_set_auxhist8_interval_s ( id_id , auxhist8_interval_s )
  integer , INTENT(IN) :: auxhist8_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist8_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist8_interval_s(id_id) = auxhist8_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist8_interval_s
SUBROUTINE nl_set_auxhist8_interval ( id_id , auxhist8_interval )
  integer , INTENT(IN) :: auxhist8_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist8_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist8_interval(id_id) = auxhist8_interval
  RETURN
END SUBROUTINE nl_set_auxhist8_interval
SUBROUTINE nl_set_auxhist9_interval_mo ( id_id , auxhist9_interval_mo )
  integer , INTENT(IN) :: auxhist9_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist9_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist9_interval_mo(id_id) = auxhist9_interval_mo
  RETURN
END SUBROUTINE nl_set_auxhist9_interval_mo
SUBROUTINE nl_set_auxhist9_interval_d ( id_id , auxhist9_interval_d )
  integer , INTENT(IN) :: auxhist9_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist9_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist9_interval_d(id_id) = auxhist9_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist9_interval_d
SUBROUTINE nl_set_auxhist9_interval_h ( id_id , auxhist9_interval_h )
  integer , INTENT(IN) :: auxhist9_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist9_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist9_interval_h(id_id) = auxhist9_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist9_interval_h
SUBROUTINE nl_set_auxhist9_interval_m ( id_id , auxhist9_interval_m )
  integer , INTENT(IN) :: auxhist9_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist9_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist9_interval_m(id_id) = auxhist9_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist9_interval_m
SUBROUTINE nl_set_auxhist9_interval_s ( id_id , auxhist9_interval_s )
  integer , INTENT(IN) :: auxhist9_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist9_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist9_interval_s(id_id) = auxhist9_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist9_interval_s
SUBROUTINE nl_set_auxhist9_interval ( id_id , auxhist9_interval )
  integer , INTENT(IN) :: auxhist9_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist9_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist9_interval(id_id) = auxhist9_interval
  RETURN
END SUBROUTINE nl_set_auxhist9_interval
SUBROUTINE nl_set_auxhist10_interval_mo ( id_id , auxhist10_interval_mo )
  integer , INTENT(IN) :: auxhist10_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist10_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist10_interval_mo(id_id) = auxhist10_interval_mo
  RETURN
END SUBROUTINE nl_set_auxhist10_interval_mo
SUBROUTINE nl_set_auxhist10_interval_d ( id_id , auxhist10_interval_d )
  integer , INTENT(IN) :: auxhist10_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist10_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist10_interval_d(id_id) = auxhist10_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist10_interval_d
SUBROUTINE nl_set_auxhist10_interval_h ( id_id , auxhist10_interval_h )
  integer , INTENT(IN) :: auxhist10_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist10_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist10_interval_h(id_id) = auxhist10_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist10_interval_h
SUBROUTINE nl_set_auxhist10_interval_m ( id_id , auxhist10_interval_m )
  integer , INTENT(IN) :: auxhist10_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist10_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist10_interval_m(id_id) = auxhist10_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist10_interval_m
SUBROUTINE nl_set_auxhist10_interval_s ( id_id , auxhist10_interval_s )
  integer , INTENT(IN) :: auxhist10_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist10_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist10_interval_s(id_id) = auxhist10_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist10_interval_s
SUBROUTINE nl_set_auxhist10_interval ( id_id , auxhist10_interval )
  integer , INTENT(IN) :: auxhist10_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist10_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist10_interval(id_id) = auxhist10_interval
  RETURN
END SUBROUTINE nl_set_auxhist10_interval
SUBROUTINE nl_set_auxhist11_interval_mo ( id_id , auxhist11_interval_mo )
  integer , INTENT(IN) :: auxhist11_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist11_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist11_interval_mo(id_id) = auxhist11_interval_mo
  RETURN
END SUBROUTINE nl_set_auxhist11_interval_mo
SUBROUTINE nl_set_auxhist11_interval_d ( id_id , auxhist11_interval_d )
  integer , INTENT(IN) :: auxhist11_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist11_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist11_interval_d(id_id) = auxhist11_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist11_interval_d
SUBROUTINE nl_set_auxhist11_interval_h ( id_id , auxhist11_interval_h )
  integer , INTENT(IN) :: auxhist11_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist11_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist11_interval_h(id_id) = auxhist11_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist11_interval_h
SUBROUTINE nl_set_auxhist11_interval_m ( id_id , auxhist11_interval_m )
  integer , INTENT(IN) :: auxhist11_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist11_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist11_interval_m(id_id) = auxhist11_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist11_interval_m
SUBROUTINE nl_set_auxhist11_interval_s ( id_id , auxhist11_interval_s )
  integer , INTENT(IN) :: auxhist11_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist11_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist11_interval_s(id_id) = auxhist11_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist11_interval_s
SUBROUTINE nl_set_auxhist11_interval ( id_id , auxhist11_interval )
  integer , INTENT(IN) :: auxhist11_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist11_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist11_interval(id_id) = auxhist11_interval
  RETURN
END SUBROUTINE nl_set_auxhist11_interval
SUBROUTINE nl_set_auxinput1_interval_mo ( id_id , auxinput1_interval_mo )
  integer , INTENT(IN) :: auxinput1_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput1_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput1_interval_mo(id_id) = auxinput1_interval_mo
  RETURN
END SUBROUTINE nl_set_auxinput1_interval_mo
SUBROUTINE nl_set_auxinput1_interval_d ( id_id , auxinput1_interval_d )
  integer , INTENT(IN) :: auxinput1_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput1_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput1_interval_d(id_id) = auxinput1_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput1_interval_d
SUBROUTINE nl_set_auxinput1_interval_h ( id_id , auxinput1_interval_h )
  integer , INTENT(IN) :: auxinput1_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput1_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput1_interval_h(id_id) = auxinput1_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput1_interval_h
SUBROUTINE nl_set_auxinput1_interval_m ( id_id , auxinput1_interval_m )
  integer , INTENT(IN) :: auxinput1_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput1_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput1_interval_m(id_id) = auxinput1_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput1_interval_m
SUBROUTINE nl_set_auxinput1_interval_s ( id_id , auxinput1_interval_s )
  integer , INTENT(IN) :: auxinput1_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput1_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput1_interval_s(id_id) = auxinput1_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput1_interval_s
SUBROUTINE nl_set_auxinput1_interval ( id_id , auxinput1_interval )
  integer , INTENT(IN) :: auxinput1_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput1_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput1_interval(id_id) = auxinput1_interval
  RETURN
END SUBROUTINE nl_set_auxinput1_interval
SUBROUTINE nl_set_auxinput2_interval_mo ( id_id , auxinput2_interval_mo )
  integer , INTENT(IN) :: auxinput2_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput2_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput2_interval_mo(id_id) = auxinput2_interval_mo
  RETURN
END SUBROUTINE nl_set_auxinput2_interval_mo
SUBROUTINE nl_set_auxinput2_interval_d ( id_id , auxinput2_interval_d )
  integer , INTENT(IN) :: auxinput2_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput2_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput2_interval_d(id_id) = auxinput2_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput2_interval_d
SUBROUTINE nl_set_auxinput2_interval_h ( id_id , auxinput2_interval_h )
  integer , INTENT(IN) :: auxinput2_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput2_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput2_interval_h(id_id) = auxinput2_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput2_interval_h
SUBROUTINE nl_set_auxinput2_interval_m ( id_id , auxinput2_interval_m )
  integer , INTENT(IN) :: auxinput2_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput2_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput2_interval_m(id_id) = auxinput2_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput2_interval_m
SUBROUTINE nl_set_auxinput2_interval_s ( id_id , auxinput2_interval_s )
  integer , INTENT(IN) :: auxinput2_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput2_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput2_interval_s(id_id) = auxinput2_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput2_interval_s
SUBROUTINE nl_set_auxinput2_interval ( id_id , auxinput2_interval )
  integer , INTENT(IN) :: auxinput2_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput2_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput2_interval(id_id) = auxinput2_interval
  RETURN
END SUBROUTINE nl_set_auxinput2_interval
SUBROUTINE nl_set_auxinput3_interval_mo ( id_id , auxinput3_interval_mo )
  integer , INTENT(IN) :: auxinput3_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput3_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput3_interval_mo(id_id) = auxinput3_interval_mo
  RETURN
END SUBROUTINE nl_set_auxinput3_interval_mo
SUBROUTINE nl_set_auxinput3_interval_d ( id_id , auxinput3_interval_d )
  integer , INTENT(IN) :: auxinput3_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput3_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput3_interval_d(id_id) = auxinput3_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput3_interval_d
SUBROUTINE nl_set_auxinput3_interval_h ( id_id , auxinput3_interval_h )
  integer , INTENT(IN) :: auxinput3_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput3_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput3_interval_h(id_id) = auxinput3_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput3_interval_h
SUBROUTINE nl_set_auxinput3_interval_m ( id_id , auxinput3_interval_m )
  integer , INTENT(IN) :: auxinput3_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput3_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput3_interval_m(id_id) = auxinput3_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput3_interval_m
SUBROUTINE nl_set_auxinput3_interval_s ( id_id , auxinput3_interval_s )
  integer , INTENT(IN) :: auxinput3_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput3_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput3_interval_s(id_id) = auxinput3_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput3_interval_s
SUBROUTINE nl_set_auxinput3_interval ( id_id , auxinput3_interval )
  integer , INTENT(IN) :: auxinput3_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput3_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput3_interval(id_id) = auxinput3_interval
  RETURN
END SUBROUTINE nl_set_auxinput3_interval
SUBROUTINE nl_set_auxinput4_interval_mo ( id_id , auxinput4_interval_mo )
  integer , INTENT(IN) :: auxinput4_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput4_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput4_interval_mo(id_id) = auxinput4_interval_mo
  RETURN
END SUBROUTINE nl_set_auxinput4_interval_mo
SUBROUTINE nl_set_auxinput4_interval_d ( id_id , auxinput4_interval_d )
  integer , INTENT(IN) :: auxinput4_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput4_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput4_interval_d(id_id) = auxinput4_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput4_interval_d
SUBROUTINE nl_set_auxinput4_interval_h ( id_id , auxinput4_interval_h )
  integer , INTENT(IN) :: auxinput4_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput4_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput4_interval_h(id_id) = auxinput4_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput4_interval_h
SUBROUTINE nl_set_auxinput4_interval_m ( id_id , auxinput4_interval_m )
  integer , INTENT(IN) :: auxinput4_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput4_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput4_interval_m(id_id) = auxinput4_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput4_interval_m
SUBROUTINE nl_set_auxinput4_interval_s ( id_id , auxinput4_interval_s )
  integer , INTENT(IN) :: auxinput4_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput4_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput4_interval_s(id_id) = auxinput4_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput4_interval_s
SUBROUTINE nl_set_auxinput4_interval ( id_id , auxinput4_interval )
  integer , INTENT(IN) :: auxinput4_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput4_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput4_interval(id_id) = auxinput4_interval
  RETURN
END SUBROUTINE nl_set_auxinput4_interval
SUBROUTINE nl_set_auxinput5_interval_mo ( id_id , auxinput5_interval_mo )
  integer , INTENT(IN) :: auxinput5_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput5_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput5_interval_mo(id_id) = auxinput5_interval_mo
  RETURN
END SUBROUTINE nl_set_auxinput5_interval_mo
SUBROUTINE nl_set_auxinput5_interval_d ( id_id , auxinput5_interval_d )
  integer , INTENT(IN) :: auxinput5_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput5_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput5_interval_d(id_id) = auxinput5_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput5_interval_d
SUBROUTINE nl_set_auxinput5_interval_h ( id_id , auxinput5_interval_h )
  integer , INTENT(IN) :: auxinput5_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput5_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput5_interval_h(id_id) = auxinput5_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput5_interval_h
SUBROUTINE nl_set_auxinput5_interval_m ( id_id , auxinput5_interval_m )
  integer , INTENT(IN) :: auxinput5_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput5_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput5_interval_m(id_id) = auxinput5_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput5_interval_m
SUBROUTINE nl_set_auxinput5_interval_s ( id_id , auxinput5_interval_s )
  integer , INTENT(IN) :: auxinput5_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput5_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput5_interval_s(id_id) = auxinput5_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput5_interval_s
SUBROUTINE nl_set_auxinput5_interval ( id_id , auxinput5_interval )
  integer , INTENT(IN) :: auxinput5_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput5_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput5_interval(id_id) = auxinput5_interval
  RETURN
END SUBROUTINE nl_set_auxinput5_interval
SUBROUTINE nl_set_auxinput6_interval_mo ( id_id , auxinput6_interval_mo )
  integer , INTENT(IN) :: auxinput6_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput6_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput6_interval_mo(id_id) = auxinput6_interval_mo
  RETURN
END SUBROUTINE nl_set_auxinput6_interval_mo
SUBROUTINE nl_set_auxinput6_interval_d ( id_id , auxinput6_interval_d )
  integer , INTENT(IN) :: auxinput6_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput6_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput6_interval_d(id_id) = auxinput6_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput6_interval_d
SUBROUTINE nl_set_auxinput6_interval_h ( id_id , auxinput6_interval_h )
  integer , INTENT(IN) :: auxinput6_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput6_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput6_interval_h(id_id) = auxinput6_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput6_interval_h
SUBROUTINE nl_set_auxinput6_interval_m ( id_id , auxinput6_interval_m )
  integer , INTENT(IN) :: auxinput6_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput6_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput6_interval_m(id_id) = auxinput6_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput6_interval_m
SUBROUTINE nl_set_auxinput6_interval_s ( id_id , auxinput6_interval_s )
  integer , INTENT(IN) :: auxinput6_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput6_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput6_interval_s(id_id) = auxinput6_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput6_interval_s
SUBROUTINE nl_set_auxinput6_interval ( id_id , auxinput6_interval )
  integer , INTENT(IN) :: auxinput6_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput6_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput6_interval(id_id) = auxinput6_interval
  RETURN
END SUBROUTINE nl_set_auxinput6_interval
SUBROUTINE nl_set_auxinput7_interval_mo ( id_id , auxinput7_interval_mo )
  integer , INTENT(IN) :: auxinput7_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput7_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput7_interval_mo(id_id) = auxinput7_interval_mo
  RETURN
END SUBROUTINE nl_set_auxinput7_interval_mo
SUBROUTINE nl_set_auxinput7_interval_d ( id_id , auxinput7_interval_d )
  integer , INTENT(IN) :: auxinput7_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput7_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput7_interval_d(id_id) = auxinput7_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput7_interval_d
SUBROUTINE nl_set_auxinput7_interval_h ( id_id , auxinput7_interval_h )
  integer , INTENT(IN) :: auxinput7_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput7_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput7_interval_h(id_id) = auxinput7_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput7_interval_h
SUBROUTINE nl_set_auxinput7_interval_m ( id_id , auxinput7_interval_m )
  integer , INTENT(IN) :: auxinput7_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput7_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput7_interval_m(id_id) = auxinput7_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput7_interval_m
SUBROUTINE nl_set_auxinput7_interval_s ( id_id , auxinput7_interval_s )
  integer , INTENT(IN) :: auxinput7_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput7_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput7_interval_s(id_id) = auxinput7_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput7_interval_s
SUBROUTINE nl_set_auxinput7_interval ( id_id , auxinput7_interval )
  integer , INTENT(IN) :: auxinput7_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput7_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput7_interval(id_id) = auxinput7_interval
  RETURN
END SUBROUTINE nl_set_auxinput7_interval
SUBROUTINE nl_set_auxinput8_interval_mo ( id_id , auxinput8_interval_mo )
  integer , INTENT(IN) :: auxinput8_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput8_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput8_interval_mo(id_id) = auxinput8_interval_mo
  RETURN
END SUBROUTINE nl_set_auxinput8_interval_mo
SUBROUTINE nl_set_auxinput8_interval_d ( id_id , auxinput8_interval_d )
  integer , INTENT(IN) :: auxinput8_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput8_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput8_interval_d(id_id) = auxinput8_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput8_interval_d
SUBROUTINE nl_set_auxinput8_interval_h ( id_id , auxinput8_interval_h )
  integer , INTENT(IN) :: auxinput8_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput8_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput8_interval_h(id_id) = auxinput8_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput8_interval_h
SUBROUTINE nl_set_auxinput8_interval_m ( id_id , auxinput8_interval_m )
  integer , INTENT(IN) :: auxinput8_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput8_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput8_interval_m(id_id) = auxinput8_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput8_interval_m
SUBROUTINE nl_set_auxinput8_interval_s ( id_id , auxinput8_interval_s )
  integer , INTENT(IN) :: auxinput8_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput8_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput8_interval_s(id_id) = auxinput8_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput8_interval_s
SUBROUTINE nl_set_auxinput8_interval ( id_id , auxinput8_interval )
  integer , INTENT(IN) :: auxinput8_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput8_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput8_interval(id_id) = auxinput8_interval
  RETURN
END SUBROUTINE nl_set_auxinput8_interval
SUBROUTINE nl_set_auxinput9_interval_mo ( id_id , auxinput9_interval_mo )
  integer , INTENT(IN) :: auxinput9_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput9_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput9_interval_mo(id_id) = auxinput9_interval_mo
  RETURN
END SUBROUTINE nl_set_auxinput9_interval_mo
SUBROUTINE nl_set_auxinput9_interval_d ( id_id , auxinput9_interval_d )
  integer , INTENT(IN) :: auxinput9_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput9_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput9_interval_d(id_id) = auxinput9_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput9_interval_d
SUBROUTINE nl_set_auxinput9_interval_h ( id_id , auxinput9_interval_h )
  integer , INTENT(IN) :: auxinput9_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput9_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput9_interval_h(id_id) = auxinput9_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput9_interval_h
SUBROUTINE nl_set_auxinput9_interval_m ( id_id , auxinput9_interval_m )
  integer , INTENT(IN) :: auxinput9_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput9_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput9_interval_m(id_id) = auxinput9_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput9_interval_m
SUBROUTINE nl_set_auxinput9_interval_s ( id_id , auxinput9_interval_s )
  integer , INTENT(IN) :: auxinput9_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput9_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput9_interval_s(id_id) = auxinput9_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput9_interval_s
SUBROUTINE nl_set_auxinput9_interval ( id_id , auxinput9_interval )
  integer , INTENT(IN) :: auxinput9_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput9_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput9_interval(id_id) = auxinput9_interval
  RETURN
END SUBROUTINE nl_set_auxinput9_interval
SUBROUTINE nl_set_gfdda_interval_mo ( id_id , gfdda_interval_mo )
  integer , INTENT(IN) :: gfdda_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_gfdda_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%gfdda_interval_mo(id_id) = gfdda_interval_mo
  RETURN
END SUBROUTINE nl_set_gfdda_interval_mo
SUBROUTINE nl_set_gfdda_interval_d ( id_id , gfdda_interval_d )
  integer , INTENT(IN) :: gfdda_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_gfdda_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%gfdda_interval_d(id_id) = gfdda_interval_d
  RETURN
END SUBROUTINE nl_set_gfdda_interval_d
SUBROUTINE nl_set_gfdda_interval_h ( id_id , gfdda_interval_h )
  integer , INTENT(IN) :: gfdda_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_gfdda_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%gfdda_interval_h(id_id) = gfdda_interval_h
  RETURN
END SUBROUTINE nl_set_gfdda_interval_h
SUBROUTINE nl_set_gfdda_interval_m ( id_id , gfdda_interval_m )
  integer , INTENT(IN) :: gfdda_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_gfdda_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%gfdda_interval_m(id_id) = gfdda_interval_m
  RETURN
END SUBROUTINE nl_set_gfdda_interval_m
SUBROUTINE nl_set_gfdda_interval_s ( id_id , gfdda_interval_s )
  integer , INTENT(IN) :: gfdda_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_gfdda_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%gfdda_interval_s(id_id) = gfdda_interval_s
  RETURN
END SUBROUTINE nl_set_gfdda_interval_s
SUBROUTINE nl_set_gfdda_interval ( id_id , gfdda_interval )
  integer , INTENT(IN) :: gfdda_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_gfdda_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%gfdda_interval(id_id) = gfdda_interval
  RETURN
END SUBROUTINE nl_set_gfdda_interval
SUBROUTINE nl_set_auxinput11_interval_mo ( id_id , auxinput11_interval_mo )
  integer , INTENT(IN) :: auxinput11_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput11_interval_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput11_interval_mo(id_id) = auxinput11_interval_mo
  RETURN
END SUBROUTINE nl_set_auxinput11_interval_mo
SUBROUTINE nl_set_auxinput11_interval_d ( id_id , auxinput11_interval_d )
  integer , INTENT(IN) :: auxinput11_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput11_interval_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput11_interval_d(id_id) = auxinput11_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput11_interval_d
SUBROUTINE nl_set_auxinput11_interval_h ( id_id , auxinput11_interval_h )
  integer , INTENT(IN) :: auxinput11_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput11_interval_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput11_interval_h(id_id) = auxinput11_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput11_interval_h
SUBROUTINE nl_set_auxinput11_interval_m ( id_id , auxinput11_interval_m )
  integer , INTENT(IN) :: auxinput11_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput11_interval_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput11_interval_m(id_id) = auxinput11_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput11_interval_m
SUBROUTINE nl_set_auxinput11_interval_s ( id_id , auxinput11_interval_s )
  integer , INTENT(IN) :: auxinput11_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput11_interval_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput11_interval_s(id_id) = auxinput11_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput11_interval_s
SUBROUTINE nl_set_auxinput11_interval ( id_id , auxinput11_interval )
  integer , INTENT(IN) :: auxinput11_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput11_interval: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput11_interval(id_id) = auxinput11_interval
  RETURN
END SUBROUTINE nl_set_auxinput11_interval
SUBROUTINE nl_set_restart_interval_mo ( id_id , restart_interval_mo )
  integer , INTENT(IN) :: restart_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_restart_interval_mo: restart_interval_mo applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%restart_interval_mo = restart_interval_mo 
  RETURN
END SUBROUTINE nl_set_restart_interval_mo
SUBROUTINE nl_set_restart_interval_d ( id_id , restart_interval_d )
  integer , INTENT(IN) :: restart_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_restart_interval_d: restart_interval_d applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%restart_interval_d = restart_interval_d 
  RETURN
END SUBROUTINE nl_set_restart_interval_d
SUBROUTINE nl_set_restart_interval_h ( id_id , restart_interval_h )
  integer , INTENT(IN) :: restart_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_restart_interval_h: restart_interval_h applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%restart_interval_h = restart_interval_h 
  RETURN
END SUBROUTINE nl_set_restart_interval_h
SUBROUTINE nl_set_restart_interval_m ( id_id , restart_interval_m )
  integer , INTENT(IN) :: restart_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_restart_interval_m: restart_interval_m applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%restart_interval_m = restart_interval_m 
  RETURN
END SUBROUTINE nl_set_restart_interval_m
SUBROUTINE nl_set_restart_interval_s ( id_id , restart_interval_s )
  integer , INTENT(IN) :: restart_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_restart_interval_s: restart_interval_s applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%restart_interval_s = restart_interval_s 
  RETURN
END SUBROUTINE nl_set_restart_interval_s
SUBROUTINE nl_set_history_begin_y ( id_id , history_begin_y )
  integer , INTENT(IN) :: history_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_history_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%history_begin_y(id_id) = history_begin_y
  RETURN
END SUBROUTINE nl_set_history_begin_y
SUBROUTINE nl_set_history_begin_mo ( id_id , history_begin_mo )
  integer , INTENT(IN) :: history_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_history_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%history_begin_mo(id_id) = history_begin_mo
  RETURN
END SUBROUTINE nl_set_history_begin_mo
SUBROUTINE nl_set_history_begin_d ( id_id , history_begin_d )
  integer , INTENT(IN) :: history_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_history_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%history_begin_d(id_id) = history_begin_d
  RETURN
END SUBROUTINE nl_set_history_begin_d
SUBROUTINE nl_set_history_begin_h ( id_id , history_begin_h )
  integer , INTENT(IN) :: history_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_history_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%history_begin_h(id_id) = history_begin_h
  RETURN
END SUBROUTINE nl_set_history_begin_h
SUBROUTINE nl_set_history_begin_m ( id_id , history_begin_m )
  integer , INTENT(IN) :: history_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_history_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%history_begin_m(id_id) = history_begin_m
  RETURN
END SUBROUTINE nl_set_history_begin_m
SUBROUTINE nl_set_history_begin_s ( id_id , history_begin_s )
  integer , INTENT(IN) :: history_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_history_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%history_begin_s(id_id) = history_begin_s
  RETURN
END SUBROUTINE nl_set_history_begin_s
SUBROUTINE nl_set_inputout_begin_y ( id_id , inputout_begin_y )
  integer , INTENT(IN) :: inputout_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_inputout_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%inputout_begin_y(id_id) = inputout_begin_y
  RETURN
END SUBROUTINE nl_set_inputout_begin_y
SUBROUTINE nl_set_inputout_begin_mo ( id_id , inputout_begin_mo )
  integer , INTENT(IN) :: inputout_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_inputout_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%inputout_begin_mo(id_id) = inputout_begin_mo
  RETURN
END SUBROUTINE nl_set_inputout_begin_mo
SUBROUTINE nl_set_inputout_begin_d ( id_id , inputout_begin_d )
  integer , INTENT(IN) :: inputout_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_inputout_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%inputout_begin_d(id_id) = inputout_begin_d
  RETURN
END SUBROUTINE nl_set_inputout_begin_d
SUBROUTINE nl_set_inputout_begin_h ( id_id , inputout_begin_h )
  integer , INTENT(IN) :: inputout_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_inputout_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%inputout_begin_h(id_id) = inputout_begin_h
  RETURN
END SUBROUTINE nl_set_inputout_begin_h
SUBROUTINE nl_set_inputout_begin_m ( id_id , inputout_begin_m )
  integer , INTENT(IN) :: inputout_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_inputout_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%inputout_begin_m(id_id) = inputout_begin_m
  RETURN
END SUBROUTINE nl_set_inputout_begin_m
SUBROUTINE nl_set_inputout_begin_s ( id_id , inputout_begin_s )
  integer , INTENT(IN) :: inputout_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_inputout_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%inputout_begin_s(id_id) = inputout_begin_s
  RETURN
END SUBROUTINE nl_set_inputout_begin_s
SUBROUTINE nl_set_auxhist1_begin_y ( id_id , auxhist1_begin_y )
  integer , INTENT(IN) :: auxhist1_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist1_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist1_begin_y(id_id) = auxhist1_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist1_begin_y
SUBROUTINE nl_set_auxhist1_begin_mo ( id_id , auxhist1_begin_mo )
  integer , INTENT(IN) :: auxhist1_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist1_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist1_begin_mo(id_id) = auxhist1_begin_mo
  RETURN
END SUBROUTINE nl_set_auxhist1_begin_mo
SUBROUTINE nl_set_auxhist1_begin_d ( id_id , auxhist1_begin_d )
  integer , INTENT(IN) :: auxhist1_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist1_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist1_begin_d(id_id) = auxhist1_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist1_begin_d
SUBROUTINE nl_set_auxhist1_begin_h ( id_id , auxhist1_begin_h )
  integer , INTENT(IN) :: auxhist1_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist1_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist1_begin_h(id_id) = auxhist1_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist1_begin_h
SUBROUTINE nl_set_auxhist1_begin_m ( id_id , auxhist1_begin_m )
  integer , INTENT(IN) :: auxhist1_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist1_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist1_begin_m(id_id) = auxhist1_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist1_begin_m
SUBROUTINE nl_set_auxhist1_begin_s ( id_id , auxhist1_begin_s )
  integer , INTENT(IN) :: auxhist1_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist1_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist1_begin_s(id_id) = auxhist1_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist1_begin_s
SUBROUTINE nl_set_auxhist2_begin_y ( id_id , auxhist2_begin_y )
  integer , INTENT(IN) :: auxhist2_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist2_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist2_begin_y(id_id) = auxhist2_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist2_begin_y
SUBROUTINE nl_set_auxhist2_begin_mo ( id_id , auxhist2_begin_mo )
  integer , INTENT(IN) :: auxhist2_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist2_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist2_begin_mo(id_id) = auxhist2_begin_mo
  RETURN
END SUBROUTINE nl_set_auxhist2_begin_mo
SUBROUTINE nl_set_auxhist2_begin_d ( id_id , auxhist2_begin_d )
  integer , INTENT(IN) :: auxhist2_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist2_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist2_begin_d(id_id) = auxhist2_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist2_begin_d
SUBROUTINE nl_set_auxhist2_begin_h ( id_id , auxhist2_begin_h )
  integer , INTENT(IN) :: auxhist2_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist2_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist2_begin_h(id_id) = auxhist2_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist2_begin_h
SUBROUTINE nl_set_auxhist2_begin_m ( id_id , auxhist2_begin_m )
  integer , INTENT(IN) :: auxhist2_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist2_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist2_begin_m(id_id) = auxhist2_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist2_begin_m
SUBROUTINE nl_set_auxhist2_begin_s ( id_id , auxhist2_begin_s )
  integer , INTENT(IN) :: auxhist2_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist2_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist2_begin_s(id_id) = auxhist2_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist2_begin_s
SUBROUTINE nl_set_auxhist3_begin_y ( id_id , auxhist3_begin_y )
  integer , INTENT(IN) :: auxhist3_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist3_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist3_begin_y(id_id) = auxhist3_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist3_begin_y
SUBROUTINE nl_set_auxhist3_begin_mo ( id_id , auxhist3_begin_mo )
  integer , INTENT(IN) :: auxhist3_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist3_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist3_begin_mo(id_id) = auxhist3_begin_mo
  RETURN
END SUBROUTINE nl_set_auxhist3_begin_mo
SUBROUTINE nl_set_auxhist3_begin_d ( id_id , auxhist3_begin_d )
  integer , INTENT(IN) :: auxhist3_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist3_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist3_begin_d(id_id) = auxhist3_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist3_begin_d
SUBROUTINE nl_set_auxhist3_begin_h ( id_id , auxhist3_begin_h )
  integer , INTENT(IN) :: auxhist3_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist3_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist3_begin_h(id_id) = auxhist3_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist3_begin_h
SUBROUTINE nl_set_auxhist3_begin_m ( id_id , auxhist3_begin_m )
  integer , INTENT(IN) :: auxhist3_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist3_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist3_begin_m(id_id) = auxhist3_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist3_begin_m
SUBROUTINE nl_set_auxhist3_begin_s ( id_id , auxhist3_begin_s )
  integer , INTENT(IN) :: auxhist3_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist3_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist3_begin_s(id_id) = auxhist3_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist3_begin_s
SUBROUTINE nl_set_auxhist4_begin_y ( id_id , auxhist4_begin_y )
  integer , INTENT(IN) :: auxhist4_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist4_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist4_begin_y(id_id) = auxhist4_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist4_begin_y
SUBROUTINE nl_set_auxhist4_begin_mo ( id_id , auxhist4_begin_mo )
  integer , INTENT(IN) :: auxhist4_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist4_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist4_begin_mo(id_id) = auxhist4_begin_mo
  RETURN
END SUBROUTINE nl_set_auxhist4_begin_mo
SUBROUTINE nl_set_auxhist4_begin_d ( id_id , auxhist4_begin_d )
  integer , INTENT(IN) :: auxhist4_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist4_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist4_begin_d(id_id) = auxhist4_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist4_begin_d
SUBROUTINE nl_set_auxhist4_begin_h ( id_id , auxhist4_begin_h )
  integer , INTENT(IN) :: auxhist4_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist4_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist4_begin_h(id_id) = auxhist4_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist4_begin_h
SUBROUTINE nl_set_auxhist4_begin_m ( id_id , auxhist4_begin_m )
  integer , INTENT(IN) :: auxhist4_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist4_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist4_begin_m(id_id) = auxhist4_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist4_begin_m
SUBROUTINE nl_set_auxhist4_begin_s ( id_id , auxhist4_begin_s )
  integer , INTENT(IN) :: auxhist4_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist4_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist4_begin_s(id_id) = auxhist4_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist4_begin_s
SUBROUTINE nl_set_auxhist5_begin_y ( id_id , auxhist5_begin_y )
  integer , INTENT(IN) :: auxhist5_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist5_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist5_begin_y(id_id) = auxhist5_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist5_begin_y
SUBROUTINE nl_set_auxhist5_begin_mo ( id_id , auxhist5_begin_mo )
  integer , INTENT(IN) :: auxhist5_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist5_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist5_begin_mo(id_id) = auxhist5_begin_mo
  RETURN
END SUBROUTINE nl_set_auxhist5_begin_mo
SUBROUTINE nl_set_auxhist5_begin_d ( id_id , auxhist5_begin_d )
  integer , INTENT(IN) :: auxhist5_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist5_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist5_begin_d(id_id) = auxhist5_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist5_begin_d
SUBROUTINE nl_set_auxhist5_begin_h ( id_id , auxhist5_begin_h )
  integer , INTENT(IN) :: auxhist5_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist5_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist5_begin_h(id_id) = auxhist5_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist5_begin_h
SUBROUTINE nl_set_auxhist5_begin_m ( id_id , auxhist5_begin_m )
  integer , INTENT(IN) :: auxhist5_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist5_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist5_begin_m(id_id) = auxhist5_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist5_begin_m
SUBROUTINE nl_set_auxhist5_begin_s ( id_id , auxhist5_begin_s )
  integer , INTENT(IN) :: auxhist5_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist5_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist5_begin_s(id_id) = auxhist5_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist5_begin_s
SUBROUTINE nl_set_auxhist6_begin_y ( id_id , auxhist6_begin_y )
  integer , INTENT(IN) :: auxhist6_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist6_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist6_begin_y(id_id) = auxhist6_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist6_begin_y
SUBROUTINE nl_set_auxhist6_begin_mo ( id_id , auxhist6_begin_mo )
  integer , INTENT(IN) :: auxhist6_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist6_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist6_begin_mo(id_id) = auxhist6_begin_mo
  RETURN
END SUBROUTINE nl_set_auxhist6_begin_mo
SUBROUTINE nl_set_auxhist6_begin_d ( id_id , auxhist6_begin_d )
  integer , INTENT(IN) :: auxhist6_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist6_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist6_begin_d(id_id) = auxhist6_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist6_begin_d
SUBROUTINE nl_set_auxhist6_begin_h ( id_id , auxhist6_begin_h )
  integer , INTENT(IN) :: auxhist6_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist6_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist6_begin_h(id_id) = auxhist6_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist6_begin_h
SUBROUTINE nl_set_auxhist6_begin_m ( id_id , auxhist6_begin_m )
  integer , INTENT(IN) :: auxhist6_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist6_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist6_begin_m(id_id) = auxhist6_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist6_begin_m
SUBROUTINE nl_set_auxhist6_begin_s ( id_id , auxhist6_begin_s )
  integer , INTENT(IN) :: auxhist6_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist6_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist6_begin_s(id_id) = auxhist6_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist6_begin_s
SUBROUTINE nl_set_auxhist7_begin_y ( id_id , auxhist7_begin_y )
  integer , INTENT(IN) :: auxhist7_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist7_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist7_begin_y(id_id) = auxhist7_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist7_begin_y
SUBROUTINE nl_set_auxhist7_begin_mo ( id_id , auxhist7_begin_mo )
  integer , INTENT(IN) :: auxhist7_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist7_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist7_begin_mo(id_id) = auxhist7_begin_mo
  RETURN
END SUBROUTINE nl_set_auxhist7_begin_mo
SUBROUTINE nl_set_auxhist7_begin_d ( id_id , auxhist7_begin_d )
  integer , INTENT(IN) :: auxhist7_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist7_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist7_begin_d(id_id) = auxhist7_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist7_begin_d
SUBROUTINE nl_set_auxhist7_begin_h ( id_id , auxhist7_begin_h )
  integer , INTENT(IN) :: auxhist7_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist7_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist7_begin_h(id_id) = auxhist7_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist7_begin_h
SUBROUTINE nl_set_auxhist7_begin_m ( id_id , auxhist7_begin_m )
  integer , INTENT(IN) :: auxhist7_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist7_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist7_begin_m(id_id) = auxhist7_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist7_begin_m
SUBROUTINE nl_set_auxhist7_begin_s ( id_id , auxhist7_begin_s )
  integer , INTENT(IN) :: auxhist7_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist7_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist7_begin_s(id_id) = auxhist7_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist7_begin_s
SUBROUTINE nl_set_auxhist8_begin_y ( id_id , auxhist8_begin_y )
  integer , INTENT(IN) :: auxhist8_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist8_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist8_begin_y(id_id) = auxhist8_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist8_begin_y
SUBROUTINE nl_set_auxhist8_begin_mo ( id_id , auxhist8_begin_mo )
  integer , INTENT(IN) :: auxhist8_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist8_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist8_begin_mo(id_id) = auxhist8_begin_mo
  RETURN
END SUBROUTINE nl_set_auxhist8_begin_mo
SUBROUTINE nl_set_auxhist8_begin_d ( id_id , auxhist8_begin_d )
  integer , INTENT(IN) :: auxhist8_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist8_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist8_begin_d(id_id) = auxhist8_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist8_begin_d
SUBROUTINE nl_set_auxhist8_begin_h ( id_id , auxhist8_begin_h )
  integer , INTENT(IN) :: auxhist8_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist8_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist8_begin_h(id_id) = auxhist8_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist8_begin_h
SUBROUTINE nl_set_auxhist8_begin_m ( id_id , auxhist8_begin_m )
  integer , INTENT(IN) :: auxhist8_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist8_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist8_begin_m(id_id) = auxhist8_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist8_begin_m
SUBROUTINE nl_set_auxhist8_begin_s ( id_id , auxhist8_begin_s )
  integer , INTENT(IN) :: auxhist8_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist8_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist8_begin_s(id_id) = auxhist8_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist8_begin_s
SUBROUTINE nl_set_auxhist9_begin_y ( id_id , auxhist9_begin_y )
  integer , INTENT(IN) :: auxhist9_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist9_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist9_begin_y(id_id) = auxhist9_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist9_begin_y
SUBROUTINE nl_set_auxhist9_begin_mo ( id_id , auxhist9_begin_mo )
  integer , INTENT(IN) :: auxhist9_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist9_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist9_begin_mo(id_id) = auxhist9_begin_mo
  RETURN
END SUBROUTINE nl_set_auxhist9_begin_mo
SUBROUTINE nl_set_auxhist9_begin_d ( id_id , auxhist9_begin_d )
  integer , INTENT(IN) :: auxhist9_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist9_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist9_begin_d(id_id) = auxhist9_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist9_begin_d
SUBROUTINE nl_set_auxhist9_begin_h ( id_id , auxhist9_begin_h )
  integer , INTENT(IN) :: auxhist9_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist9_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist9_begin_h(id_id) = auxhist9_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist9_begin_h
SUBROUTINE nl_set_auxhist9_begin_m ( id_id , auxhist9_begin_m )
  integer , INTENT(IN) :: auxhist9_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist9_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist9_begin_m(id_id) = auxhist9_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist9_begin_m
SUBROUTINE nl_set_auxhist9_begin_s ( id_id , auxhist9_begin_s )
  integer , INTENT(IN) :: auxhist9_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist9_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist9_begin_s(id_id) = auxhist9_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist9_begin_s
SUBROUTINE nl_set_auxhist10_begin_y ( id_id , auxhist10_begin_y )
  integer , INTENT(IN) :: auxhist10_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist10_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist10_begin_y(id_id) = auxhist10_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist10_begin_y
SUBROUTINE nl_set_auxhist10_begin_mo ( id_id , auxhist10_begin_mo )
  integer , INTENT(IN) :: auxhist10_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist10_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist10_begin_mo(id_id) = auxhist10_begin_mo
  RETURN
END SUBROUTINE nl_set_auxhist10_begin_mo
SUBROUTINE nl_set_auxhist10_begin_d ( id_id , auxhist10_begin_d )
  integer , INTENT(IN) :: auxhist10_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist10_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist10_begin_d(id_id) = auxhist10_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist10_begin_d
SUBROUTINE nl_set_auxhist10_begin_h ( id_id , auxhist10_begin_h )
  integer , INTENT(IN) :: auxhist10_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist10_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist10_begin_h(id_id) = auxhist10_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist10_begin_h
SUBROUTINE nl_set_auxhist10_begin_m ( id_id , auxhist10_begin_m )
  integer , INTENT(IN) :: auxhist10_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist10_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist10_begin_m(id_id) = auxhist10_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist10_begin_m
SUBROUTINE nl_set_auxhist10_begin_s ( id_id , auxhist10_begin_s )
  integer , INTENT(IN) :: auxhist10_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist10_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist10_begin_s(id_id) = auxhist10_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist10_begin_s
SUBROUTINE nl_set_auxhist11_begin_y ( id_id , auxhist11_begin_y )
  integer , INTENT(IN) :: auxhist11_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist11_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist11_begin_y(id_id) = auxhist11_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist11_begin_y
SUBROUTINE nl_set_auxhist11_begin_mo ( id_id , auxhist11_begin_mo )
  integer , INTENT(IN) :: auxhist11_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist11_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist11_begin_mo(id_id) = auxhist11_begin_mo
  RETURN
END SUBROUTINE nl_set_auxhist11_begin_mo
SUBROUTINE nl_set_auxhist11_begin_d ( id_id , auxhist11_begin_d )
  integer , INTENT(IN) :: auxhist11_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist11_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist11_begin_d(id_id) = auxhist11_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist11_begin_d
SUBROUTINE nl_set_auxhist11_begin_h ( id_id , auxhist11_begin_h )
  integer , INTENT(IN) :: auxhist11_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist11_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist11_begin_h(id_id) = auxhist11_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist11_begin_h
SUBROUTINE nl_set_auxhist11_begin_m ( id_id , auxhist11_begin_m )
  integer , INTENT(IN) :: auxhist11_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist11_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist11_begin_m(id_id) = auxhist11_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist11_begin_m
SUBROUTINE nl_set_auxhist11_begin_s ( id_id , auxhist11_begin_s )
  integer , INTENT(IN) :: auxhist11_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist11_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist11_begin_s(id_id) = auxhist11_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist11_begin_s
SUBROUTINE nl_set_auxinput1_begin_y ( id_id , auxinput1_begin_y )
  integer , INTENT(IN) :: auxinput1_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput1_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput1_begin_y(id_id) = auxinput1_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput1_begin_y
SUBROUTINE nl_set_auxinput1_begin_mo ( id_id , auxinput1_begin_mo )
  integer , INTENT(IN) :: auxinput1_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput1_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput1_begin_mo(id_id) = auxinput1_begin_mo
  RETURN
END SUBROUTINE nl_set_auxinput1_begin_mo
SUBROUTINE nl_set_auxinput1_begin_d ( id_id , auxinput1_begin_d )
  integer , INTENT(IN) :: auxinput1_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput1_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput1_begin_d(id_id) = auxinput1_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput1_begin_d
SUBROUTINE nl_set_auxinput1_begin_h ( id_id , auxinput1_begin_h )
  integer , INTENT(IN) :: auxinput1_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput1_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput1_begin_h(id_id) = auxinput1_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput1_begin_h
SUBROUTINE nl_set_auxinput1_begin_m ( id_id , auxinput1_begin_m )
  integer , INTENT(IN) :: auxinput1_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput1_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput1_begin_m(id_id) = auxinput1_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput1_begin_m
SUBROUTINE nl_set_auxinput1_begin_s ( id_id , auxinput1_begin_s )
  integer , INTENT(IN) :: auxinput1_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput1_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput1_begin_s(id_id) = auxinput1_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput1_begin_s
SUBROUTINE nl_set_auxinput2_begin_y ( id_id , auxinput2_begin_y )
  integer , INTENT(IN) :: auxinput2_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput2_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput2_begin_y(id_id) = auxinput2_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput2_begin_y
SUBROUTINE nl_set_auxinput2_begin_mo ( id_id , auxinput2_begin_mo )
  integer , INTENT(IN) :: auxinput2_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput2_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput2_begin_mo(id_id) = auxinput2_begin_mo
  RETURN
END SUBROUTINE nl_set_auxinput2_begin_mo
SUBROUTINE nl_set_auxinput2_begin_d ( id_id , auxinput2_begin_d )
  integer , INTENT(IN) :: auxinput2_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput2_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput2_begin_d(id_id) = auxinput2_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput2_begin_d
SUBROUTINE nl_set_auxinput2_begin_h ( id_id , auxinput2_begin_h )
  integer , INTENT(IN) :: auxinput2_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput2_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput2_begin_h(id_id) = auxinput2_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput2_begin_h
SUBROUTINE nl_set_auxinput2_begin_m ( id_id , auxinput2_begin_m )
  integer , INTENT(IN) :: auxinput2_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput2_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput2_begin_m(id_id) = auxinput2_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput2_begin_m
SUBROUTINE nl_set_auxinput2_begin_s ( id_id , auxinput2_begin_s )
  integer , INTENT(IN) :: auxinput2_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput2_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput2_begin_s(id_id) = auxinput2_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput2_begin_s
SUBROUTINE nl_set_auxinput3_begin_y ( id_id , auxinput3_begin_y )
  integer , INTENT(IN) :: auxinput3_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput3_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput3_begin_y(id_id) = auxinput3_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput3_begin_y
SUBROUTINE nl_set_auxinput3_begin_mo ( id_id , auxinput3_begin_mo )
  integer , INTENT(IN) :: auxinput3_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput3_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput3_begin_mo(id_id) = auxinput3_begin_mo
  RETURN
END SUBROUTINE nl_set_auxinput3_begin_mo
SUBROUTINE nl_set_auxinput3_begin_d ( id_id , auxinput3_begin_d )
  integer , INTENT(IN) :: auxinput3_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput3_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput3_begin_d(id_id) = auxinput3_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput3_begin_d
SUBROUTINE nl_set_auxinput3_begin_h ( id_id , auxinput3_begin_h )
  integer , INTENT(IN) :: auxinput3_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput3_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput3_begin_h(id_id) = auxinput3_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput3_begin_h
SUBROUTINE nl_set_auxinput3_begin_m ( id_id , auxinput3_begin_m )
  integer , INTENT(IN) :: auxinput3_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput3_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput3_begin_m(id_id) = auxinput3_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput3_begin_m
SUBROUTINE nl_set_auxinput3_begin_s ( id_id , auxinput3_begin_s )
  integer , INTENT(IN) :: auxinput3_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput3_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput3_begin_s(id_id) = auxinput3_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput3_begin_s
SUBROUTINE nl_set_auxinput4_begin_y ( id_id , auxinput4_begin_y )
  integer , INTENT(IN) :: auxinput4_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput4_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput4_begin_y(id_id) = auxinput4_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput4_begin_y
SUBROUTINE nl_set_auxinput4_begin_mo ( id_id , auxinput4_begin_mo )
  integer , INTENT(IN) :: auxinput4_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput4_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput4_begin_mo(id_id) = auxinput4_begin_mo
  RETURN
END SUBROUTINE nl_set_auxinput4_begin_mo
SUBROUTINE nl_set_auxinput4_begin_d ( id_id , auxinput4_begin_d )
  integer , INTENT(IN) :: auxinput4_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput4_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput4_begin_d(id_id) = auxinput4_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput4_begin_d
SUBROUTINE nl_set_auxinput4_begin_h ( id_id , auxinput4_begin_h )
  integer , INTENT(IN) :: auxinput4_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput4_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput4_begin_h(id_id) = auxinput4_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput4_begin_h
SUBROUTINE nl_set_auxinput4_begin_m ( id_id , auxinput4_begin_m )
  integer , INTENT(IN) :: auxinput4_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput4_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput4_begin_m(id_id) = auxinput4_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput4_begin_m
SUBROUTINE nl_set_auxinput4_begin_s ( id_id , auxinput4_begin_s )
  integer , INTENT(IN) :: auxinput4_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput4_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput4_begin_s(id_id) = auxinput4_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput4_begin_s
SUBROUTINE nl_set_auxinput5_begin_y ( id_id , auxinput5_begin_y )
  integer , INTENT(IN) :: auxinput5_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput5_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput5_begin_y(id_id) = auxinput5_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput5_begin_y
SUBROUTINE nl_set_auxinput5_begin_mo ( id_id , auxinput5_begin_mo )
  integer , INTENT(IN) :: auxinput5_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput5_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput5_begin_mo(id_id) = auxinput5_begin_mo
  RETURN
END SUBROUTINE nl_set_auxinput5_begin_mo
SUBROUTINE nl_set_auxinput5_begin_d ( id_id , auxinput5_begin_d )
  integer , INTENT(IN) :: auxinput5_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput5_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput5_begin_d(id_id) = auxinput5_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput5_begin_d
SUBROUTINE nl_set_auxinput5_begin_h ( id_id , auxinput5_begin_h )
  integer , INTENT(IN) :: auxinput5_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput5_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput5_begin_h(id_id) = auxinput5_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput5_begin_h
SUBROUTINE nl_set_auxinput5_begin_m ( id_id , auxinput5_begin_m )
  integer , INTENT(IN) :: auxinput5_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput5_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput5_begin_m(id_id) = auxinput5_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput5_begin_m
SUBROUTINE nl_set_auxinput5_begin_s ( id_id , auxinput5_begin_s )
  integer , INTENT(IN) :: auxinput5_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput5_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput5_begin_s(id_id) = auxinput5_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput5_begin_s
SUBROUTINE nl_set_auxinput6_begin_y ( id_id , auxinput6_begin_y )
  integer , INTENT(IN) :: auxinput6_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput6_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput6_begin_y(id_id) = auxinput6_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput6_begin_y
SUBROUTINE nl_set_auxinput6_begin_mo ( id_id , auxinput6_begin_mo )
  integer , INTENT(IN) :: auxinput6_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput6_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput6_begin_mo(id_id) = auxinput6_begin_mo
  RETURN
END SUBROUTINE nl_set_auxinput6_begin_mo
SUBROUTINE nl_set_auxinput6_begin_d ( id_id , auxinput6_begin_d )
  integer , INTENT(IN) :: auxinput6_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput6_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput6_begin_d(id_id) = auxinput6_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput6_begin_d
SUBROUTINE nl_set_auxinput6_begin_h ( id_id , auxinput6_begin_h )
  integer , INTENT(IN) :: auxinput6_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput6_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput6_begin_h(id_id) = auxinput6_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput6_begin_h
SUBROUTINE nl_set_auxinput6_begin_m ( id_id , auxinput6_begin_m )
  integer , INTENT(IN) :: auxinput6_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput6_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput6_begin_m(id_id) = auxinput6_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput6_begin_m
SUBROUTINE nl_set_auxinput6_begin_s ( id_id , auxinput6_begin_s )
  integer , INTENT(IN) :: auxinput6_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput6_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput6_begin_s(id_id) = auxinput6_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput6_begin_s
SUBROUTINE nl_set_auxinput7_begin_y ( id_id , auxinput7_begin_y )
  integer , INTENT(IN) :: auxinput7_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput7_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput7_begin_y(id_id) = auxinput7_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput7_begin_y
SUBROUTINE nl_set_auxinput7_begin_mo ( id_id , auxinput7_begin_mo )
  integer , INTENT(IN) :: auxinput7_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput7_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput7_begin_mo(id_id) = auxinput7_begin_mo
  RETURN
END SUBROUTINE nl_set_auxinput7_begin_mo
SUBROUTINE nl_set_auxinput7_begin_d ( id_id , auxinput7_begin_d )
  integer , INTENT(IN) :: auxinput7_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput7_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput7_begin_d(id_id) = auxinput7_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput7_begin_d
SUBROUTINE nl_set_auxinput7_begin_h ( id_id , auxinput7_begin_h )
  integer , INTENT(IN) :: auxinput7_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput7_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput7_begin_h(id_id) = auxinput7_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput7_begin_h
SUBROUTINE nl_set_auxinput7_begin_m ( id_id , auxinput7_begin_m )
  integer , INTENT(IN) :: auxinput7_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput7_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput7_begin_m(id_id) = auxinput7_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput7_begin_m
SUBROUTINE nl_set_auxinput7_begin_s ( id_id , auxinput7_begin_s )
  integer , INTENT(IN) :: auxinput7_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput7_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput7_begin_s(id_id) = auxinput7_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput7_begin_s
SUBROUTINE nl_set_auxinput8_begin_y ( id_id , auxinput8_begin_y )
  integer , INTENT(IN) :: auxinput8_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput8_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput8_begin_y(id_id) = auxinput8_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput8_begin_y
SUBROUTINE nl_set_auxinput8_begin_mo ( id_id , auxinput8_begin_mo )
  integer , INTENT(IN) :: auxinput8_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput8_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput8_begin_mo(id_id) = auxinput8_begin_mo
  RETURN
END SUBROUTINE nl_set_auxinput8_begin_mo
SUBROUTINE nl_set_auxinput8_begin_d ( id_id , auxinput8_begin_d )
  integer , INTENT(IN) :: auxinput8_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput8_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput8_begin_d(id_id) = auxinput8_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput8_begin_d
SUBROUTINE nl_set_auxinput8_begin_h ( id_id , auxinput8_begin_h )
  integer , INTENT(IN) :: auxinput8_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput8_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput8_begin_h(id_id) = auxinput8_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput8_begin_h
SUBROUTINE nl_set_auxinput8_begin_m ( id_id , auxinput8_begin_m )
  integer , INTENT(IN) :: auxinput8_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput8_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput8_begin_m(id_id) = auxinput8_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput8_begin_m
SUBROUTINE nl_set_auxinput8_begin_s ( id_id , auxinput8_begin_s )
  integer , INTENT(IN) :: auxinput8_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput8_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput8_begin_s(id_id) = auxinput8_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput8_begin_s
SUBROUTINE nl_set_auxinput9_begin_y ( id_id , auxinput9_begin_y )
  integer , INTENT(IN) :: auxinput9_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput9_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput9_begin_y(id_id) = auxinput9_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput9_begin_y
SUBROUTINE nl_set_auxinput9_begin_mo ( id_id , auxinput9_begin_mo )
  integer , INTENT(IN) :: auxinput9_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput9_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput9_begin_mo(id_id) = auxinput9_begin_mo
  RETURN
END SUBROUTINE nl_set_auxinput9_begin_mo
SUBROUTINE nl_set_auxinput9_begin_d ( id_id , auxinput9_begin_d )
  integer , INTENT(IN) :: auxinput9_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput9_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput9_begin_d(id_id) = auxinput9_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput9_begin_d
SUBROUTINE nl_set_auxinput9_begin_h ( id_id , auxinput9_begin_h )
  integer , INTENT(IN) :: auxinput9_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput9_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput9_begin_h(id_id) = auxinput9_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput9_begin_h
SUBROUTINE nl_set_auxinput9_begin_m ( id_id , auxinput9_begin_m )
  integer , INTENT(IN) :: auxinput9_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput9_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput9_begin_m(id_id) = auxinput9_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput9_begin_m
SUBROUTINE nl_set_auxinput9_begin_s ( id_id , auxinput9_begin_s )
  integer , INTENT(IN) :: auxinput9_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput9_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput9_begin_s(id_id) = auxinput9_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput9_begin_s
SUBROUTINE nl_set_gfdda_begin_y ( id_id , gfdda_begin_y )
  integer , INTENT(IN) :: gfdda_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_gfdda_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%gfdda_begin_y(id_id) = gfdda_begin_y
  RETURN
END SUBROUTINE nl_set_gfdda_begin_y
SUBROUTINE nl_set_gfdda_begin_mo ( id_id , gfdda_begin_mo )
  integer , INTENT(IN) :: gfdda_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_gfdda_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%gfdda_begin_mo(id_id) = gfdda_begin_mo
  RETURN
END SUBROUTINE nl_set_gfdda_begin_mo
SUBROUTINE nl_set_gfdda_begin_d ( id_id , gfdda_begin_d )
  integer , INTENT(IN) :: gfdda_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_gfdda_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%gfdda_begin_d(id_id) = gfdda_begin_d
  RETURN
END SUBROUTINE nl_set_gfdda_begin_d
SUBROUTINE nl_set_gfdda_begin_h ( id_id , gfdda_begin_h )
  integer , INTENT(IN) :: gfdda_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_gfdda_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%gfdda_begin_h(id_id) = gfdda_begin_h
  RETURN
END SUBROUTINE nl_set_gfdda_begin_h
SUBROUTINE nl_set_gfdda_begin_m ( id_id , gfdda_begin_m )
  integer , INTENT(IN) :: gfdda_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_gfdda_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%gfdda_begin_m(id_id) = gfdda_begin_m
  RETURN
END SUBROUTINE nl_set_gfdda_begin_m
SUBROUTINE nl_set_gfdda_begin_s ( id_id , gfdda_begin_s )
  integer , INTENT(IN) :: gfdda_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_gfdda_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%gfdda_begin_s(id_id) = gfdda_begin_s
  RETURN
END SUBROUTINE nl_set_gfdda_begin_s
SUBROUTINE nl_set_auxinput11_begin_y ( id_id , auxinput11_begin_y )
  integer , INTENT(IN) :: auxinput11_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput11_begin_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput11_begin_y(id_id) = auxinput11_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput11_begin_y
SUBROUTINE nl_set_auxinput11_begin_mo ( id_id , auxinput11_begin_mo )
  integer , INTENT(IN) :: auxinput11_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput11_begin_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput11_begin_mo(id_id) = auxinput11_begin_mo
  RETURN
END SUBROUTINE nl_set_auxinput11_begin_mo
SUBROUTINE nl_set_auxinput11_begin_d ( id_id , auxinput11_begin_d )
  integer , INTENT(IN) :: auxinput11_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput11_begin_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput11_begin_d(id_id) = auxinput11_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput11_begin_d
SUBROUTINE nl_set_auxinput11_begin_h ( id_id , auxinput11_begin_h )
  integer , INTENT(IN) :: auxinput11_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput11_begin_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput11_begin_h(id_id) = auxinput11_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput11_begin_h
SUBROUTINE nl_set_auxinput11_begin_m ( id_id , auxinput11_begin_m )
  integer , INTENT(IN) :: auxinput11_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput11_begin_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput11_begin_m(id_id) = auxinput11_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput11_begin_m
SUBROUTINE nl_set_auxinput11_begin_s ( id_id , auxinput11_begin_s )
  integer , INTENT(IN) :: auxinput11_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput11_begin_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput11_begin_s(id_id) = auxinput11_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput11_begin_s
SUBROUTINE nl_set_restart_begin_y ( id_id , restart_begin_y )
  integer , INTENT(IN) :: restart_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_restart_begin_y: restart_begin_y applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%restart_begin_y = restart_begin_y 
  RETURN
END SUBROUTINE nl_set_restart_begin_y
SUBROUTINE nl_set_restart_begin_mo ( id_id , restart_begin_mo )
  integer , INTENT(IN) :: restart_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_restart_begin_mo: restart_begin_mo applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%restart_begin_mo = restart_begin_mo 
  RETURN
END SUBROUTINE nl_set_restart_begin_mo
SUBROUTINE nl_set_restart_begin_d ( id_id , restart_begin_d )
  integer , INTENT(IN) :: restart_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_restart_begin_d: restart_begin_d applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%restart_begin_d = restart_begin_d 
  RETURN
END SUBROUTINE nl_set_restart_begin_d
SUBROUTINE nl_set_restart_begin_h ( id_id , restart_begin_h )
  integer , INTENT(IN) :: restart_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_restart_begin_h: restart_begin_h applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%restart_begin_h = restart_begin_h 
  RETURN
END SUBROUTINE nl_set_restart_begin_h
SUBROUTINE nl_set_restart_begin_m ( id_id , restart_begin_m )
  integer , INTENT(IN) :: restart_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_restart_begin_m: restart_begin_m applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%restart_begin_m = restart_begin_m 
  RETURN
END SUBROUTINE nl_set_restart_begin_m
SUBROUTINE nl_set_restart_begin_s ( id_id , restart_begin_s )
  integer , INTENT(IN) :: restart_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_restart_begin_s: restart_begin_s applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%restart_begin_s = restart_begin_s 
  RETURN
END SUBROUTINE nl_set_restart_begin_s
SUBROUTINE nl_set_history_end_y ( id_id , history_end_y )
  integer , INTENT(IN) :: history_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_history_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%history_end_y(id_id) = history_end_y
  RETURN
END SUBROUTINE nl_set_history_end_y
SUBROUTINE nl_set_history_end_mo ( id_id , history_end_mo )
  integer , INTENT(IN) :: history_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_history_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%history_end_mo(id_id) = history_end_mo
  RETURN
END SUBROUTINE nl_set_history_end_mo
SUBROUTINE nl_set_history_end_d ( id_id , history_end_d )
  integer , INTENT(IN) :: history_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_history_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%history_end_d(id_id) = history_end_d
  RETURN
END SUBROUTINE nl_set_history_end_d
SUBROUTINE nl_set_history_end_h ( id_id , history_end_h )
  integer , INTENT(IN) :: history_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_history_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%history_end_h(id_id) = history_end_h
  RETURN
END SUBROUTINE nl_set_history_end_h
SUBROUTINE nl_set_history_end_m ( id_id , history_end_m )
  integer , INTENT(IN) :: history_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_history_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%history_end_m(id_id) = history_end_m
  RETURN
END SUBROUTINE nl_set_history_end_m
SUBROUTINE nl_set_history_end_s ( id_id , history_end_s )
  integer , INTENT(IN) :: history_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_history_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%history_end_s(id_id) = history_end_s
  RETURN
END SUBROUTINE nl_set_history_end_s
SUBROUTINE nl_set_inputout_end_y ( id_id , inputout_end_y )
  integer , INTENT(IN) :: inputout_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_inputout_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%inputout_end_y(id_id) = inputout_end_y
  RETURN
END SUBROUTINE nl_set_inputout_end_y
SUBROUTINE nl_set_inputout_end_mo ( id_id , inputout_end_mo )
  integer , INTENT(IN) :: inputout_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_inputout_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%inputout_end_mo(id_id) = inputout_end_mo
  RETURN
END SUBROUTINE nl_set_inputout_end_mo
SUBROUTINE nl_set_inputout_end_d ( id_id , inputout_end_d )
  integer , INTENT(IN) :: inputout_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_inputout_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%inputout_end_d(id_id) = inputout_end_d
  RETURN
END SUBROUTINE nl_set_inputout_end_d
SUBROUTINE nl_set_inputout_end_h ( id_id , inputout_end_h )
  integer , INTENT(IN) :: inputout_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_inputout_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%inputout_end_h(id_id) = inputout_end_h
  RETURN
END SUBROUTINE nl_set_inputout_end_h
SUBROUTINE nl_set_inputout_end_m ( id_id , inputout_end_m )
  integer , INTENT(IN) :: inputout_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_inputout_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%inputout_end_m(id_id) = inputout_end_m
  RETURN
END SUBROUTINE nl_set_inputout_end_m
SUBROUTINE nl_set_inputout_end_s ( id_id , inputout_end_s )
  integer , INTENT(IN) :: inputout_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_inputout_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%inputout_end_s(id_id) = inputout_end_s
  RETURN
END SUBROUTINE nl_set_inputout_end_s
SUBROUTINE nl_set_auxhist1_end_y ( id_id , auxhist1_end_y )
  integer , INTENT(IN) :: auxhist1_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist1_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist1_end_y(id_id) = auxhist1_end_y
  RETURN
END SUBROUTINE nl_set_auxhist1_end_y
SUBROUTINE nl_set_auxhist1_end_mo ( id_id , auxhist1_end_mo )
  integer , INTENT(IN) :: auxhist1_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist1_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist1_end_mo(id_id) = auxhist1_end_mo
  RETURN
END SUBROUTINE nl_set_auxhist1_end_mo
SUBROUTINE nl_set_auxhist1_end_d ( id_id , auxhist1_end_d )
  integer , INTENT(IN) :: auxhist1_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist1_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist1_end_d(id_id) = auxhist1_end_d
  RETURN
END SUBROUTINE nl_set_auxhist1_end_d
SUBROUTINE nl_set_auxhist1_end_h ( id_id , auxhist1_end_h )
  integer , INTENT(IN) :: auxhist1_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist1_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist1_end_h(id_id) = auxhist1_end_h
  RETURN
END SUBROUTINE nl_set_auxhist1_end_h
SUBROUTINE nl_set_auxhist1_end_m ( id_id , auxhist1_end_m )
  integer , INTENT(IN) :: auxhist1_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist1_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist1_end_m(id_id) = auxhist1_end_m
  RETURN
END SUBROUTINE nl_set_auxhist1_end_m
SUBROUTINE nl_set_auxhist1_end_s ( id_id , auxhist1_end_s )
  integer , INTENT(IN) :: auxhist1_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist1_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist1_end_s(id_id) = auxhist1_end_s
  RETURN
END SUBROUTINE nl_set_auxhist1_end_s
SUBROUTINE nl_set_auxhist2_end_y ( id_id , auxhist2_end_y )
  integer , INTENT(IN) :: auxhist2_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist2_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist2_end_y(id_id) = auxhist2_end_y
  RETURN
END SUBROUTINE nl_set_auxhist2_end_y
SUBROUTINE nl_set_auxhist2_end_mo ( id_id , auxhist2_end_mo )
  integer , INTENT(IN) :: auxhist2_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist2_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist2_end_mo(id_id) = auxhist2_end_mo
  RETURN
END SUBROUTINE nl_set_auxhist2_end_mo
SUBROUTINE nl_set_auxhist2_end_d ( id_id , auxhist2_end_d )
  integer , INTENT(IN) :: auxhist2_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist2_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist2_end_d(id_id) = auxhist2_end_d
  RETURN
END SUBROUTINE nl_set_auxhist2_end_d
SUBROUTINE nl_set_auxhist2_end_h ( id_id , auxhist2_end_h )
  integer , INTENT(IN) :: auxhist2_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist2_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist2_end_h(id_id) = auxhist2_end_h
  RETURN
END SUBROUTINE nl_set_auxhist2_end_h
SUBROUTINE nl_set_auxhist2_end_m ( id_id , auxhist2_end_m )
  integer , INTENT(IN) :: auxhist2_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist2_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist2_end_m(id_id) = auxhist2_end_m
  RETURN
END SUBROUTINE nl_set_auxhist2_end_m
SUBROUTINE nl_set_auxhist2_end_s ( id_id , auxhist2_end_s )
  integer , INTENT(IN) :: auxhist2_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist2_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist2_end_s(id_id) = auxhist2_end_s
  RETURN
END SUBROUTINE nl_set_auxhist2_end_s
SUBROUTINE nl_set_auxhist3_end_y ( id_id , auxhist3_end_y )
  integer , INTENT(IN) :: auxhist3_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist3_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist3_end_y(id_id) = auxhist3_end_y
  RETURN
END SUBROUTINE nl_set_auxhist3_end_y
SUBROUTINE nl_set_auxhist3_end_mo ( id_id , auxhist3_end_mo )
  integer , INTENT(IN) :: auxhist3_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist3_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist3_end_mo(id_id) = auxhist3_end_mo
  RETURN
END SUBROUTINE nl_set_auxhist3_end_mo
SUBROUTINE nl_set_auxhist3_end_d ( id_id , auxhist3_end_d )
  integer , INTENT(IN) :: auxhist3_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist3_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist3_end_d(id_id) = auxhist3_end_d
  RETURN
END SUBROUTINE nl_set_auxhist3_end_d
SUBROUTINE nl_set_auxhist3_end_h ( id_id , auxhist3_end_h )
  integer , INTENT(IN) :: auxhist3_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist3_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist3_end_h(id_id) = auxhist3_end_h
  RETURN
END SUBROUTINE nl_set_auxhist3_end_h
SUBROUTINE nl_set_auxhist3_end_m ( id_id , auxhist3_end_m )
  integer , INTENT(IN) :: auxhist3_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist3_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist3_end_m(id_id) = auxhist3_end_m
  RETURN
END SUBROUTINE nl_set_auxhist3_end_m
SUBROUTINE nl_set_auxhist3_end_s ( id_id , auxhist3_end_s )
  integer , INTENT(IN) :: auxhist3_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist3_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist3_end_s(id_id) = auxhist3_end_s
  RETURN
END SUBROUTINE nl_set_auxhist3_end_s
SUBROUTINE nl_set_auxhist4_end_y ( id_id , auxhist4_end_y )
  integer , INTENT(IN) :: auxhist4_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist4_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist4_end_y(id_id) = auxhist4_end_y
  RETURN
END SUBROUTINE nl_set_auxhist4_end_y
SUBROUTINE nl_set_auxhist4_end_mo ( id_id , auxhist4_end_mo )
  integer , INTENT(IN) :: auxhist4_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist4_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist4_end_mo(id_id) = auxhist4_end_mo
  RETURN
END SUBROUTINE nl_set_auxhist4_end_mo
SUBROUTINE nl_set_auxhist4_end_d ( id_id , auxhist4_end_d )
  integer , INTENT(IN) :: auxhist4_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist4_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist4_end_d(id_id) = auxhist4_end_d
  RETURN
END SUBROUTINE nl_set_auxhist4_end_d
SUBROUTINE nl_set_auxhist4_end_h ( id_id , auxhist4_end_h )
  integer , INTENT(IN) :: auxhist4_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist4_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist4_end_h(id_id) = auxhist4_end_h
  RETURN
END SUBROUTINE nl_set_auxhist4_end_h
SUBROUTINE nl_set_auxhist4_end_m ( id_id , auxhist4_end_m )
  integer , INTENT(IN) :: auxhist4_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist4_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist4_end_m(id_id) = auxhist4_end_m
  RETURN
END SUBROUTINE nl_set_auxhist4_end_m
SUBROUTINE nl_set_auxhist4_end_s ( id_id , auxhist4_end_s )
  integer , INTENT(IN) :: auxhist4_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist4_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist4_end_s(id_id) = auxhist4_end_s
  RETURN
END SUBROUTINE nl_set_auxhist4_end_s
SUBROUTINE nl_set_auxhist5_end_y ( id_id , auxhist5_end_y )
  integer , INTENT(IN) :: auxhist5_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist5_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist5_end_y(id_id) = auxhist5_end_y
  RETURN
END SUBROUTINE nl_set_auxhist5_end_y
SUBROUTINE nl_set_auxhist5_end_mo ( id_id , auxhist5_end_mo )
  integer , INTENT(IN) :: auxhist5_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist5_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist5_end_mo(id_id) = auxhist5_end_mo
  RETURN
END SUBROUTINE nl_set_auxhist5_end_mo
SUBROUTINE nl_set_auxhist5_end_d ( id_id , auxhist5_end_d )
  integer , INTENT(IN) :: auxhist5_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist5_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist5_end_d(id_id) = auxhist5_end_d
  RETURN
END SUBROUTINE nl_set_auxhist5_end_d
SUBROUTINE nl_set_auxhist5_end_h ( id_id , auxhist5_end_h )
  integer , INTENT(IN) :: auxhist5_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist5_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist5_end_h(id_id) = auxhist5_end_h
  RETURN
END SUBROUTINE nl_set_auxhist5_end_h
SUBROUTINE nl_set_auxhist5_end_m ( id_id , auxhist5_end_m )
  integer , INTENT(IN) :: auxhist5_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist5_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist5_end_m(id_id) = auxhist5_end_m
  RETURN
END SUBROUTINE nl_set_auxhist5_end_m
SUBROUTINE nl_set_auxhist5_end_s ( id_id , auxhist5_end_s )
  integer , INTENT(IN) :: auxhist5_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist5_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist5_end_s(id_id) = auxhist5_end_s
  RETURN
END SUBROUTINE nl_set_auxhist5_end_s
SUBROUTINE nl_set_auxhist6_end_y ( id_id , auxhist6_end_y )
  integer , INTENT(IN) :: auxhist6_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist6_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist6_end_y(id_id) = auxhist6_end_y
  RETURN
END SUBROUTINE nl_set_auxhist6_end_y
SUBROUTINE nl_set_auxhist6_end_mo ( id_id , auxhist6_end_mo )
  integer , INTENT(IN) :: auxhist6_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist6_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist6_end_mo(id_id) = auxhist6_end_mo
  RETURN
END SUBROUTINE nl_set_auxhist6_end_mo
SUBROUTINE nl_set_auxhist6_end_d ( id_id , auxhist6_end_d )
  integer , INTENT(IN) :: auxhist6_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist6_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist6_end_d(id_id) = auxhist6_end_d
  RETURN
END SUBROUTINE nl_set_auxhist6_end_d
SUBROUTINE nl_set_auxhist6_end_h ( id_id , auxhist6_end_h )
  integer , INTENT(IN) :: auxhist6_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist6_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist6_end_h(id_id) = auxhist6_end_h
  RETURN
END SUBROUTINE nl_set_auxhist6_end_h
SUBROUTINE nl_set_auxhist6_end_m ( id_id , auxhist6_end_m )
  integer , INTENT(IN) :: auxhist6_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist6_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist6_end_m(id_id) = auxhist6_end_m
  RETURN
END SUBROUTINE nl_set_auxhist6_end_m
SUBROUTINE nl_set_auxhist6_end_s ( id_id , auxhist6_end_s )
  integer , INTENT(IN) :: auxhist6_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist6_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist6_end_s(id_id) = auxhist6_end_s
  RETURN
END SUBROUTINE nl_set_auxhist6_end_s
SUBROUTINE nl_set_auxhist7_end_y ( id_id , auxhist7_end_y )
  integer , INTENT(IN) :: auxhist7_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist7_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist7_end_y(id_id) = auxhist7_end_y
  RETURN
END SUBROUTINE nl_set_auxhist7_end_y
SUBROUTINE nl_set_auxhist7_end_mo ( id_id , auxhist7_end_mo )
  integer , INTENT(IN) :: auxhist7_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist7_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist7_end_mo(id_id) = auxhist7_end_mo
  RETURN
END SUBROUTINE nl_set_auxhist7_end_mo
SUBROUTINE nl_set_auxhist7_end_d ( id_id , auxhist7_end_d )
  integer , INTENT(IN) :: auxhist7_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist7_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist7_end_d(id_id) = auxhist7_end_d
  RETURN
END SUBROUTINE nl_set_auxhist7_end_d
SUBROUTINE nl_set_auxhist7_end_h ( id_id , auxhist7_end_h )
  integer , INTENT(IN) :: auxhist7_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist7_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist7_end_h(id_id) = auxhist7_end_h
  RETURN
END SUBROUTINE nl_set_auxhist7_end_h
SUBROUTINE nl_set_auxhist7_end_m ( id_id , auxhist7_end_m )
  integer , INTENT(IN) :: auxhist7_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist7_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist7_end_m(id_id) = auxhist7_end_m
  RETURN
END SUBROUTINE nl_set_auxhist7_end_m
SUBROUTINE nl_set_auxhist7_end_s ( id_id , auxhist7_end_s )
  integer , INTENT(IN) :: auxhist7_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist7_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist7_end_s(id_id) = auxhist7_end_s
  RETURN
END SUBROUTINE nl_set_auxhist7_end_s
SUBROUTINE nl_set_auxhist8_end_y ( id_id , auxhist8_end_y )
  integer , INTENT(IN) :: auxhist8_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist8_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist8_end_y(id_id) = auxhist8_end_y
  RETURN
END SUBROUTINE nl_set_auxhist8_end_y
SUBROUTINE nl_set_auxhist8_end_mo ( id_id , auxhist8_end_mo )
  integer , INTENT(IN) :: auxhist8_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist8_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist8_end_mo(id_id) = auxhist8_end_mo
  RETURN
END SUBROUTINE nl_set_auxhist8_end_mo
SUBROUTINE nl_set_auxhist8_end_d ( id_id , auxhist8_end_d )
  integer , INTENT(IN) :: auxhist8_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist8_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist8_end_d(id_id) = auxhist8_end_d
  RETURN
END SUBROUTINE nl_set_auxhist8_end_d
SUBROUTINE nl_set_auxhist8_end_h ( id_id , auxhist8_end_h )
  integer , INTENT(IN) :: auxhist8_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist8_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist8_end_h(id_id) = auxhist8_end_h
  RETURN
END SUBROUTINE nl_set_auxhist8_end_h
SUBROUTINE nl_set_auxhist8_end_m ( id_id , auxhist8_end_m )
  integer , INTENT(IN) :: auxhist8_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist8_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist8_end_m(id_id) = auxhist8_end_m
  RETURN
END SUBROUTINE nl_set_auxhist8_end_m
SUBROUTINE nl_set_auxhist8_end_s ( id_id , auxhist8_end_s )
  integer , INTENT(IN) :: auxhist8_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist8_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist8_end_s(id_id) = auxhist8_end_s
  RETURN
END SUBROUTINE nl_set_auxhist8_end_s
SUBROUTINE nl_set_auxhist9_end_y ( id_id , auxhist9_end_y )
  integer , INTENT(IN) :: auxhist9_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist9_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist9_end_y(id_id) = auxhist9_end_y
  RETURN
END SUBROUTINE nl_set_auxhist9_end_y
SUBROUTINE nl_set_auxhist9_end_mo ( id_id , auxhist9_end_mo )
  integer , INTENT(IN) :: auxhist9_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist9_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist9_end_mo(id_id) = auxhist9_end_mo
  RETURN
END SUBROUTINE nl_set_auxhist9_end_mo
SUBROUTINE nl_set_auxhist9_end_d ( id_id , auxhist9_end_d )
  integer , INTENT(IN) :: auxhist9_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist9_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist9_end_d(id_id) = auxhist9_end_d
  RETURN
END SUBROUTINE nl_set_auxhist9_end_d
SUBROUTINE nl_set_auxhist9_end_h ( id_id , auxhist9_end_h )
  integer , INTENT(IN) :: auxhist9_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist9_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist9_end_h(id_id) = auxhist9_end_h
  RETURN
END SUBROUTINE nl_set_auxhist9_end_h
SUBROUTINE nl_set_auxhist9_end_m ( id_id , auxhist9_end_m )
  integer , INTENT(IN) :: auxhist9_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist9_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist9_end_m(id_id) = auxhist9_end_m
  RETURN
END SUBROUTINE nl_set_auxhist9_end_m
SUBROUTINE nl_set_auxhist9_end_s ( id_id , auxhist9_end_s )
  integer , INTENT(IN) :: auxhist9_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist9_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist9_end_s(id_id) = auxhist9_end_s
  RETURN
END SUBROUTINE nl_set_auxhist9_end_s
SUBROUTINE nl_set_auxhist10_end_y ( id_id , auxhist10_end_y )
  integer , INTENT(IN) :: auxhist10_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist10_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist10_end_y(id_id) = auxhist10_end_y
  RETURN
END SUBROUTINE nl_set_auxhist10_end_y
SUBROUTINE nl_set_auxhist10_end_mo ( id_id , auxhist10_end_mo )
  integer , INTENT(IN) :: auxhist10_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist10_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist10_end_mo(id_id) = auxhist10_end_mo
  RETURN
END SUBROUTINE nl_set_auxhist10_end_mo
SUBROUTINE nl_set_auxhist10_end_d ( id_id , auxhist10_end_d )
  integer , INTENT(IN) :: auxhist10_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist10_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist10_end_d(id_id) = auxhist10_end_d
  RETURN
END SUBROUTINE nl_set_auxhist10_end_d
SUBROUTINE nl_set_auxhist10_end_h ( id_id , auxhist10_end_h )
  integer , INTENT(IN) :: auxhist10_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist10_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist10_end_h(id_id) = auxhist10_end_h
  RETURN
END SUBROUTINE nl_set_auxhist10_end_h
SUBROUTINE nl_set_auxhist10_end_m ( id_id , auxhist10_end_m )
  integer , INTENT(IN) :: auxhist10_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist10_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist10_end_m(id_id) = auxhist10_end_m
  RETURN
END SUBROUTINE nl_set_auxhist10_end_m
SUBROUTINE nl_set_auxhist10_end_s ( id_id , auxhist10_end_s )
  integer , INTENT(IN) :: auxhist10_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist10_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist10_end_s(id_id) = auxhist10_end_s
  RETURN
END SUBROUTINE nl_set_auxhist10_end_s
SUBROUTINE nl_set_auxhist11_end_y ( id_id , auxhist11_end_y )
  integer , INTENT(IN) :: auxhist11_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist11_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist11_end_y(id_id) = auxhist11_end_y
  RETURN
END SUBROUTINE nl_set_auxhist11_end_y
SUBROUTINE nl_set_auxhist11_end_mo ( id_id , auxhist11_end_mo )
  integer , INTENT(IN) :: auxhist11_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist11_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist11_end_mo(id_id) = auxhist11_end_mo
  RETURN
END SUBROUTINE nl_set_auxhist11_end_mo
SUBROUTINE nl_set_auxhist11_end_d ( id_id , auxhist11_end_d )
  integer , INTENT(IN) :: auxhist11_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist11_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist11_end_d(id_id) = auxhist11_end_d
  RETURN
END SUBROUTINE nl_set_auxhist11_end_d
SUBROUTINE nl_set_auxhist11_end_h ( id_id , auxhist11_end_h )
  integer , INTENT(IN) :: auxhist11_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist11_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist11_end_h(id_id) = auxhist11_end_h
  RETURN
END SUBROUTINE nl_set_auxhist11_end_h
SUBROUTINE nl_set_auxhist11_end_m ( id_id , auxhist11_end_m )
  integer , INTENT(IN) :: auxhist11_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist11_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist11_end_m(id_id) = auxhist11_end_m
  RETURN
END SUBROUTINE nl_set_auxhist11_end_m
SUBROUTINE nl_set_auxhist11_end_s ( id_id , auxhist11_end_s )
  integer , INTENT(IN) :: auxhist11_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxhist11_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxhist11_end_s(id_id) = auxhist11_end_s
  RETURN
END SUBROUTINE nl_set_auxhist11_end_s
SUBROUTINE nl_set_auxinput1_end_y ( id_id , auxinput1_end_y )
  integer , INTENT(IN) :: auxinput1_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput1_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput1_end_y(id_id) = auxinput1_end_y
  RETURN
END SUBROUTINE nl_set_auxinput1_end_y
SUBROUTINE nl_set_auxinput1_end_mo ( id_id , auxinput1_end_mo )
  integer , INTENT(IN) :: auxinput1_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput1_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput1_end_mo(id_id) = auxinput1_end_mo
  RETURN
END SUBROUTINE nl_set_auxinput1_end_mo
SUBROUTINE nl_set_auxinput1_end_d ( id_id , auxinput1_end_d )
  integer , INTENT(IN) :: auxinput1_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput1_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput1_end_d(id_id) = auxinput1_end_d
  RETURN
END SUBROUTINE nl_set_auxinput1_end_d
SUBROUTINE nl_set_auxinput1_end_h ( id_id , auxinput1_end_h )
  integer , INTENT(IN) :: auxinput1_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput1_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput1_end_h(id_id) = auxinput1_end_h
  RETURN
END SUBROUTINE nl_set_auxinput1_end_h
SUBROUTINE nl_set_auxinput1_end_m ( id_id , auxinput1_end_m )
  integer , INTENT(IN) :: auxinput1_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput1_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput1_end_m(id_id) = auxinput1_end_m
  RETURN
END SUBROUTINE nl_set_auxinput1_end_m
SUBROUTINE nl_set_auxinput1_end_s ( id_id , auxinput1_end_s )
  integer , INTENT(IN) :: auxinput1_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput1_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput1_end_s(id_id) = auxinput1_end_s
  RETURN
END SUBROUTINE nl_set_auxinput1_end_s
SUBROUTINE nl_set_auxinput2_end_y ( id_id , auxinput2_end_y )
  integer , INTENT(IN) :: auxinput2_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput2_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput2_end_y(id_id) = auxinput2_end_y
  RETURN
END SUBROUTINE nl_set_auxinput2_end_y
SUBROUTINE nl_set_auxinput2_end_mo ( id_id , auxinput2_end_mo )
  integer , INTENT(IN) :: auxinput2_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput2_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput2_end_mo(id_id) = auxinput2_end_mo
  RETURN
END SUBROUTINE nl_set_auxinput2_end_mo
SUBROUTINE nl_set_auxinput2_end_d ( id_id , auxinput2_end_d )
  integer , INTENT(IN) :: auxinput2_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput2_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput2_end_d(id_id) = auxinput2_end_d
  RETURN
END SUBROUTINE nl_set_auxinput2_end_d
SUBROUTINE nl_set_auxinput2_end_h ( id_id , auxinput2_end_h )
  integer , INTENT(IN) :: auxinput2_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput2_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput2_end_h(id_id) = auxinput2_end_h
  RETURN
END SUBROUTINE nl_set_auxinput2_end_h
SUBROUTINE nl_set_auxinput2_end_m ( id_id , auxinput2_end_m )
  integer , INTENT(IN) :: auxinput2_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput2_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput2_end_m(id_id) = auxinput2_end_m
  RETURN
END SUBROUTINE nl_set_auxinput2_end_m
SUBROUTINE nl_set_auxinput2_end_s ( id_id , auxinput2_end_s )
  integer , INTENT(IN) :: auxinput2_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput2_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput2_end_s(id_id) = auxinput2_end_s
  RETURN
END SUBROUTINE nl_set_auxinput2_end_s
SUBROUTINE nl_set_auxinput3_end_y ( id_id , auxinput3_end_y )
  integer , INTENT(IN) :: auxinput3_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput3_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput3_end_y(id_id) = auxinput3_end_y
  RETURN
END SUBROUTINE nl_set_auxinput3_end_y
SUBROUTINE nl_set_auxinput3_end_mo ( id_id , auxinput3_end_mo )
  integer , INTENT(IN) :: auxinput3_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput3_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput3_end_mo(id_id) = auxinput3_end_mo
  RETURN
END SUBROUTINE nl_set_auxinput3_end_mo
SUBROUTINE nl_set_auxinput3_end_d ( id_id , auxinput3_end_d )
  integer , INTENT(IN) :: auxinput3_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput3_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput3_end_d(id_id) = auxinput3_end_d
  RETURN
END SUBROUTINE nl_set_auxinput3_end_d
SUBROUTINE nl_set_auxinput3_end_h ( id_id , auxinput3_end_h )
  integer , INTENT(IN) :: auxinput3_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput3_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput3_end_h(id_id) = auxinput3_end_h
  RETURN
END SUBROUTINE nl_set_auxinput3_end_h
SUBROUTINE nl_set_auxinput3_end_m ( id_id , auxinput3_end_m )
  integer , INTENT(IN) :: auxinput3_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput3_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput3_end_m(id_id) = auxinput3_end_m
  RETURN
END SUBROUTINE nl_set_auxinput3_end_m
SUBROUTINE nl_set_auxinput3_end_s ( id_id , auxinput3_end_s )
  integer , INTENT(IN) :: auxinput3_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput3_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput3_end_s(id_id) = auxinput3_end_s
  RETURN
END SUBROUTINE nl_set_auxinput3_end_s
SUBROUTINE nl_set_auxinput4_end_y ( id_id , auxinput4_end_y )
  integer , INTENT(IN) :: auxinput4_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput4_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput4_end_y(id_id) = auxinput4_end_y
  RETURN
END SUBROUTINE nl_set_auxinput4_end_y
SUBROUTINE nl_set_auxinput4_end_mo ( id_id , auxinput4_end_mo )
  integer , INTENT(IN) :: auxinput4_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput4_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput4_end_mo(id_id) = auxinput4_end_mo
  RETURN
END SUBROUTINE nl_set_auxinput4_end_mo
SUBROUTINE nl_set_auxinput4_end_d ( id_id , auxinput4_end_d )
  integer , INTENT(IN) :: auxinput4_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput4_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput4_end_d(id_id) = auxinput4_end_d
  RETURN
END SUBROUTINE nl_set_auxinput4_end_d
SUBROUTINE nl_set_auxinput4_end_h ( id_id , auxinput4_end_h )
  integer , INTENT(IN) :: auxinput4_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput4_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput4_end_h(id_id) = auxinput4_end_h
  RETURN
END SUBROUTINE nl_set_auxinput4_end_h
SUBROUTINE nl_set_auxinput4_end_m ( id_id , auxinput4_end_m )
  integer , INTENT(IN) :: auxinput4_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput4_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput4_end_m(id_id) = auxinput4_end_m
  RETURN
END SUBROUTINE nl_set_auxinput4_end_m
SUBROUTINE nl_set_auxinput4_end_s ( id_id , auxinput4_end_s )
  integer , INTENT(IN) :: auxinput4_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput4_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput4_end_s(id_id) = auxinput4_end_s
  RETURN
END SUBROUTINE nl_set_auxinput4_end_s
SUBROUTINE nl_set_auxinput5_end_y ( id_id , auxinput5_end_y )
  integer , INTENT(IN) :: auxinput5_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput5_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput5_end_y(id_id) = auxinput5_end_y
  RETURN
END SUBROUTINE nl_set_auxinput5_end_y
SUBROUTINE nl_set_auxinput5_end_mo ( id_id , auxinput5_end_mo )
  integer , INTENT(IN) :: auxinput5_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput5_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput5_end_mo(id_id) = auxinput5_end_mo
  RETURN
END SUBROUTINE nl_set_auxinput5_end_mo
SUBROUTINE nl_set_auxinput5_end_d ( id_id , auxinput5_end_d )
  integer , INTENT(IN) :: auxinput5_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput5_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput5_end_d(id_id) = auxinput5_end_d
  RETURN
END SUBROUTINE nl_set_auxinput5_end_d
SUBROUTINE nl_set_auxinput5_end_h ( id_id , auxinput5_end_h )
  integer , INTENT(IN) :: auxinput5_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput5_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput5_end_h(id_id) = auxinput5_end_h
  RETURN
END SUBROUTINE nl_set_auxinput5_end_h
SUBROUTINE nl_set_auxinput5_end_m ( id_id , auxinput5_end_m )
  integer , INTENT(IN) :: auxinput5_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput5_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput5_end_m(id_id) = auxinput5_end_m
  RETURN
END SUBROUTINE nl_set_auxinput5_end_m
SUBROUTINE nl_set_auxinput5_end_s ( id_id , auxinput5_end_s )
  integer , INTENT(IN) :: auxinput5_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput5_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput5_end_s(id_id) = auxinput5_end_s
  RETURN
END SUBROUTINE nl_set_auxinput5_end_s
SUBROUTINE nl_set_auxinput6_end_y ( id_id , auxinput6_end_y )
  integer , INTENT(IN) :: auxinput6_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput6_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput6_end_y(id_id) = auxinput6_end_y
  RETURN
END SUBROUTINE nl_set_auxinput6_end_y
SUBROUTINE nl_set_auxinput6_end_mo ( id_id , auxinput6_end_mo )
  integer , INTENT(IN) :: auxinput6_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput6_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput6_end_mo(id_id) = auxinput6_end_mo
  RETURN
END SUBROUTINE nl_set_auxinput6_end_mo
SUBROUTINE nl_set_auxinput6_end_d ( id_id , auxinput6_end_d )
  integer , INTENT(IN) :: auxinput6_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput6_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput6_end_d(id_id) = auxinput6_end_d
  RETURN
END SUBROUTINE nl_set_auxinput6_end_d
SUBROUTINE nl_set_auxinput6_end_h ( id_id , auxinput6_end_h )
  integer , INTENT(IN) :: auxinput6_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput6_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput6_end_h(id_id) = auxinput6_end_h
  RETURN
END SUBROUTINE nl_set_auxinput6_end_h
SUBROUTINE nl_set_auxinput6_end_m ( id_id , auxinput6_end_m )
  integer , INTENT(IN) :: auxinput6_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput6_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput6_end_m(id_id) = auxinput6_end_m
  RETURN
END SUBROUTINE nl_set_auxinput6_end_m
SUBROUTINE nl_set_auxinput6_end_s ( id_id , auxinput6_end_s )
  integer , INTENT(IN) :: auxinput6_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput6_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput6_end_s(id_id) = auxinput6_end_s
  RETURN
END SUBROUTINE nl_set_auxinput6_end_s
SUBROUTINE nl_set_auxinput7_end_y ( id_id , auxinput7_end_y )
  integer , INTENT(IN) :: auxinput7_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput7_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput7_end_y(id_id) = auxinput7_end_y
  RETURN
END SUBROUTINE nl_set_auxinput7_end_y
SUBROUTINE nl_set_auxinput7_end_mo ( id_id , auxinput7_end_mo )
  integer , INTENT(IN) :: auxinput7_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput7_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput7_end_mo(id_id) = auxinput7_end_mo
  RETURN
END SUBROUTINE nl_set_auxinput7_end_mo
SUBROUTINE nl_set_auxinput7_end_d ( id_id , auxinput7_end_d )
  integer , INTENT(IN) :: auxinput7_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput7_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput7_end_d(id_id) = auxinput7_end_d
  RETURN
END SUBROUTINE nl_set_auxinput7_end_d
SUBROUTINE nl_set_auxinput7_end_h ( id_id , auxinput7_end_h )
  integer , INTENT(IN) :: auxinput7_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput7_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput7_end_h(id_id) = auxinput7_end_h
  RETURN
END SUBROUTINE nl_set_auxinput7_end_h
SUBROUTINE nl_set_auxinput7_end_m ( id_id , auxinput7_end_m )
  integer , INTENT(IN) :: auxinput7_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput7_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput7_end_m(id_id) = auxinput7_end_m
  RETURN
END SUBROUTINE nl_set_auxinput7_end_m
SUBROUTINE nl_set_auxinput7_end_s ( id_id , auxinput7_end_s )
  integer , INTENT(IN) :: auxinput7_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput7_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput7_end_s(id_id) = auxinput7_end_s
  RETURN
END SUBROUTINE nl_set_auxinput7_end_s
SUBROUTINE nl_set_auxinput8_end_y ( id_id , auxinput8_end_y )
  integer , INTENT(IN) :: auxinput8_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput8_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput8_end_y(id_id) = auxinput8_end_y
  RETURN
END SUBROUTINE nl_set_auxinput8_end_y
SUBROUTINE nl_set_auxinput8_end_mo ( id_id , auxinput8_end_mo )
  integer , INTENT(IN) :: auxinput8_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput8_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput8_end_mo(id_id) = auxinput8_end_mo
  RETURN
END SUBROUTINE nl_set_auxinput8_end_mo
SUBROUTINE nl_set_auxinput8_end_d ( id_id , auxinput8_end_d )
  integer , INTENT(IN) :: auxinput8_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput8_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput8_end_d(id_id) = auxinput8_end_d
  RETURN
END SUBROUTINE nl_set_auxinput8_end_d
SUBROUTINE nl_set_auxinput8_end_h ( id_id , auxinput8_end_h )
  integer , INTENT(IN) :: auxinput8_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput8_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput8_end_h(id_id) = auxinput8_end_h
  RETURN
END SUBROUTINE nl_set_auxinput8_end_h
SUBROUTINE nl_set_auxinput8_end_m ( id_id , auxinput8_end_m )
  integer , INTENT(IN) :: auxinput8_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput8_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput8_end_m(id_id) = auxinput8_end_m
  RETURN
END SUBROUTINE nl_set_auxinput8_end_m
SUBROUTINE nl_set_auxinput8_end_s ( id_id , auxinput8_end_s )
  integer , INTENT(IN) :: auxinput8_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput8_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput8_end_s(id_id) = auxinput8_end_s
  RETURN
END SUBROUTINE nl_set_auxinput8_end_s
SUBROUTINE nl_set_auxinput9_end_y ( id_id , auxinput9_end_y )
  integer , INTENT(IN) :: auxinput9_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput9_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput9_end_y(id_id) = auxinput9_end_y
  RETURN
END SUBROUTINE nl_set_auxinput9_end_y
SUBROUTINE nl_set_auxinput9_end_mo ( id_id , auxinput9_end_mo )
  integer , INTENT(IN) :: auxinput9_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput9_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput9_end_mo(id_id) = auxinput9_end_mo
  RETURN
END SUBROUTINE nl_set_auxinput9_end_mo
SUBROUTINE nl_set_auxinput9_end_d ( id_id , auxinput9_end_d )
  integer , INTENT(IN) :: auxinput9_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput9_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput9_end_d(id_id) = auxinput9_end_d
  RETURN
END SUBROUTINE nl_set_auxinput9_end_d
SUBROUTINE nl_set_auxinput9_end_h ( id_id , auxinput9_end_h )
  integer , INTENT(IN) :: auxinput9_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput9_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput9_end_h(id_id) = auxinput9_end_h
  RETURN
END SUBROUTINE nl_set_auxinput9_end_h
SUBROUTINE nl_set_auxinput9_end_m ( id_id , auxinput9_end_m )
  integer , INTENT(IN) :: auxinput9_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput9_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput9_end_m(id_id) = auxinput9_end_m
  RETURN
END SUBROUTINE nl_set_auxinput9_end_m
SUBROUTINE nl_set_auxinput9_end_s ( id_id , auxinput9_end_s )
  integer , INTENT(IN) :: auxinput9_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput9_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput9_end_s(id_id) = auxinput9_end_s
  RETURN
END SUBROUTINE nl_set_auxinput9_end_s
SUBROUTINE nl_set_gfdda_end_y ( id_id , gfdda_end_y )
  integer , INTENT(IN) :: gfdda_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_gfdda_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%gfdda_end_y(id_id) = gfdda_end_y
  RETURN
END SUBROUTINE nl_set_gfdda_end_y
SUBROUTINE nl_set_gfdda_end_mo ( id_id , gfdda_end_mo )
  integer , INTENT(IN) :: gfdda_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_gfdda_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%gfdda_end_mo(id_id) = gfdda_end_mo
  RETURN
END SUBROUTINE nl_set_gfdda_end_mo
SUBROUTINE nl_set_gfdda_end_d ( id_id , gfdda_end_d )
  integer , INTENT(IN) :: gfdda_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_gfdda_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%gfdda_end_d(id_id) = gfdda_end_d
  RETURN
END SUBROUTINE nl_set_gfdda_end_d
SUBROUTINE nl_set_gfdda_end_h ( id_id , gfdda_end_h )
  integer , INTENT(IN) :: gfdda_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_gfdda_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%gfdda_end_h(id_id) = gfdda_end_h
  RETURN
END SUBROUTINE nl_set_gfdda_end_h
SUBROUTINE nl_set_gfdda_end_m ( id_id , gfdda_end_m )
  integer , INTENT(IN) :: gfdda_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_gfdda_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%gfdda_end_m(id_id) = gfdda_end_m
  RETURN
END SUBROUTINE nl_set_gfdda_end_m
SUBROUTINE nl_set_gfdda_end_s ( id_id , gfdda_end_s )
  integer , INTENT(IN) :: gfdda_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_gfdda_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%gfdda_end_s(id_id) = gfdda_end_s
  RETURN
END SUBROUTINE nl_set_gfdda_end_s
SUBROUTINE nl_set_auxinput11_end_y ( id_id , auxinput11_end_y )
  integer , INTENT(IN) :: auxinput11_end_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput11_end_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput11_end_y(id_id) = auxinput11_end_y
  RETURN
END SUBROUTINE nl_set_auxinput11_end_y
SUBROUTINE nl_set_auxinput11_end_mo ( id_id , auxinput11_end_mo )
  integer , INTENT(IN) :: auxinput11_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput11_end_mo: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput11_end_mo(id_id) = auxinput11_end_mo
  RETURN
END SUBROUTINE nl_set_auxinput11_end_mo
SUBROUTINE nl_set_auxinput11_end_d ( id_id , auxinput11_end_d )
  integer , INTENT(IN) :: auxinput11_end_d
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput11_end_d: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput11_end_d(id_id) = auxinput11_end_d
  RETURN
END SUBROUTINE nl_set_auxinput11_end_d
SUBROUTINE nl_set_auxinput11_end_h ( id_id , auxinput11_end_h )
  integer , INTENT(IN) :: auxinput11_end_h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput11_end_h: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput11_end_h(id_id) = auxinput11_end_h
  RETURN
END SUBROUTINE nl_set_auxinput11_end_h
SUBROUTINE nl_set_auxinput11_end_m ( id_id , auxinput11_end_m )
  integer , INTENT(IN) :: auxinput11_end_m
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput11_end_m: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput11_end_m(id_id) = auxinput11_end_m
  RETURN
END SUBROUTINE nl_set_auxinput11_end_m
SUBROUTINE nl_set_auxinput11_end_s ( id_id , auxinput11_end_s )
  integer , INTENT(IN) :: auxinput11_end_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_auxinput11_end_s: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%auxinput11_end_s(id_id) = auxinput11_end_s
  RETURN
END SUBROUTINE nl_set_auxinput11_end_s
SUBROUTINE nl_set_io_form_auxinput1 ( id_id , io_form_auxinput1 )
  integer , INTENT(IN) :: io_form_auxinput1
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_io_form_auxinput1: io_form_auxinput1 applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%io_form_auxinput1 = io_form_auxinput1 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput1
SUBROUTINE nl_set_io_form_auxinput2 ( id_id , io_form_auxinput2 )
  integer , INTENT(IN) :: io_form_auxinput2
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_io_form_auxinput2: io_form_auxinput2 applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%io_form_auxinput2 = io_form_auxinput2 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput2
SUBROUTINE nl_set_io_form_auxinput3 ( id_id , io_form_auxinput3 )
  integer , INTENT(IN) :: io_form_auxinput3
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_io_form_auxinput3: io_form_auxinput3 applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%io_form_auxinput3 = io_form_auxinput3 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput3
SUBROUTINE nl_set_io_form_auxinput4 ( id_id , io_form_auxinput4 )
  integer , INTENT(IN) :: io_form_auxinput4
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_io_form_auxinput4: io_form_auxinput4 applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%io_form_auxinput4 = io_form_auxinput4 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput4
SUBROUTINE nl_set_io_form_auxinput5 ( id_id , io_form_auxinput5 )
  integer , INTENT(IN) :: io_form_auxinput5
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_io_form_auxinput5: io_form_auxinput5 applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%io_form_auxinput5 = io_form_auxinput5 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput5
SUBROUTINE nl_set_io_form_auxinput6 ( id_id , io_form_auxinput6 )
  integer , INTENT(IN) :: io_form_auxinput6
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_io_form_auxinput6: io_form_auxinput6 applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%io_form_auxinput6 = io_form_auxinput6 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput6
SUBROUTINE nl_set_io_form_auxinput7 ( id_id , io_form_auxinput7 )
  integer , INTENT(IN) :: io_form_auxinput7
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_io_form_auxinput7: io_form_auxinput7 applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%io_form_auxinput7 = io_form_auxinput7 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput7
SUBROUTINE nl_set_io_form_auxinput8 ( id_id , io_form_auxinput8 )
  integer , INTENT(IN) :: io_form_auxinput8
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_io_form_auxinput8: io_form_auxinput8 applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%io_form_auxinput8 = io_form_auxinput8 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput8
SUBROUTINE nl_set_io_form_auxinput9 ( id_id , io_form_auxinput9 )
  integer , INTENT(IN) :: io_form_auxinput9
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_io_form_auxinput9: io_form_auxinput9 applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%io_form_auxinput9 = io_form_auxinput9 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput9
SUBROUTINE nl_set_io_form_gfdda ( id_id , io_form_gfdda )
  integer , INTENT(IN) :: io_form_gfdda
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_io_form_gfdda: io_form_gfdda applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%io_form_gfdda = io_form_gfdda 
  RETURN
END SUBROUTINE nl_set_io_form_gfdda
SUBROUTINE nl_set_io_form_auxinput11 ( id_id , io_form_auxinput11 )
  integer , INTENT(IN) :: io_form_auxinput11
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_io_form_auxinput11: io_form_auxinput11 applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%io_form_auxinput11 = io_form_auxinput11 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput11
SUBROUTINE nl_set_io_form_auxhist1 ( id_id , io_form_auxhist1 )
  integer , INTENT(IN) :: io_form_auxhist1
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_io_form_auxhist1: io_form_auxhist1 applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%io_form_auxhist1 = io_form_auxhist1 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist1
SUBROUTINE nl_set_io_form_auxhist2 ( id_id , io_form_auxhist2 )
  integer , INTENT(IN) :: io_form_auxhist2
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_io_form_auxhist2: io_form_auxhist2 applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%io_form_auxhist2 = io_form_auxhist2 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist2
SUBROUTINE nl_set_io_form_auxhist3 ( id_id , io_form_auxhist3 )
  integer , INTENT(IN) :: io_form_auxhist3
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_io_form_auxhist3: io_form_auxhist3 applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%io_form_auxhist3 = io_form_auxhist3 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist3
SUBROUTINE nl_set_io_form_auxhist4 ( id_id , io_form_auxhist4 )
  integer , INTENT(IN) :: io_form_auxhist4
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_io_form_auxhist4: io_form_auxhist4 applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%io_form_auxhist4 = io_form_auxhist4 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist4
SUBROUTINE nl_set_io_form_auxhist5 ( id_id , io_form_auxhist5 )
  integer , INTENT(IN) :: io_form_auxhist5
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_io_form_auxhist5: io_form_auxhist5 applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%io_form_auxhist5 = io_form_auxhist5 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist5
SUBROUTINE nl_set_io_form_auxhist6 ( id_id , io_form_auxhist6 )
  integer , INTENT(IN) :: io_form_auxhist6
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_io_form_auxhist6: io_form_auxhist6 applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%io_form_auxhist6 = io_form_auxhist6 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist6
SUBROUTINE nl_set_io_form_auxhist7 ( id_id , io_form_auxhist7 )
  integer , INTENT(IN) :: io_form_auxhist7
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_io_form_auxhist7: io_form_auxhist7 applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%io_form_auxhist7 = io_form_auxhist7 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist7
SUBROUTINE nl_set_io_form_auxhist8 ( id_id , io_form_auxhist8 )
  integer , INTENT(IN) :: io_form_auxhist8
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_io_form_auxhist8: io_form_auxhist8 applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%io_form_auxhist8 = io_form_auxhist8 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist8
SUBROUTINE nl_set_io_form_auxhist9 ( id_id , io_form_auxhist9 )
  integer , INTENT(IN) :: io_form_auxhist9
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_io_form_auxhist9: io_form_auxhist9 applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%io_form_auxhist9 = io_form_auxhist9 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist9
SUBROUTINE nl_set_io_form_auxhist10 ( id_id , io_form_auxhist10 )
  integer , INTENT(IN) :: io_form_auxhist10
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_io_form_auxhist10: io_form_auxhist10 applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%io_form_auxhist10 = io_form_auxhist10 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist10
SUBROUTINE nl_set_io_form_auxhist11 ( id_id , io_form_auxhist11 )
  integer , INTENT(IN) :: io_form_auxhist11
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_io_form_auxhist11: io_form_auxhist11 applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%io_form_auxhist11 = io_form_auxhist11 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist11
SUBROUTINE nl_set_julyr ( id_id , julyr )
  integer , INTENT(IN) :: julyr
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_julyr: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%julyr(id_id) = julyr
  RETURN
END SUBROUTINE nl_set_julyr
SUBROUTINE nl_set_julday ( id_id , julday )
  integer , INTENT(IN) :: julday
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_julday: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%julday(id_id) = julday
  RETURN
END SUBROUTINE nl_set_julday
SUBROUTINE nl_set_gmt ( id_id , gmt )
  real , INTENT(IN) :: gmt
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_gmt: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%gmt(id_id) = gmt
  RETURN
END SUBROUTINE nl_set_gmt
SUBROUTINE nl_set_input_inname ( id_id , input_inname )
  character*256 , INTENT(IN) :: input_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_input_inname: input_inname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%input_inname = trim(input_inname) 
  RETURN
END SUBROUTINE nl_set_input_inname
SUBROUTINE nl_set_input_outname ( id_id , input_outname )
  character*256 , INTENT(IN) :: input_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_input_outname: input_outname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%input_outname = trim(input_outname) 
  RETURN
END SUBROUTINE nl_set_input_outname
SUBROUTINE nl_set_bdy_inname ( id_id , bdy_inname )
  character*256 , INTENT(IN) :: bdy_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_bdy_inname: bdy_inname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%bdy_inname = trim(bdy_inname) 
  RETURN
END SUBROUTINE nl_set_bdy_inname
SUBROUTINE nl_set_bdy_outname ( id_id , bdy_outname )
  character*256 , INTENT(IN) :: bdy_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_bdy_outname: bdy_outname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%bdy_outname = trim(bdy_outname) 
  RETURN
END SUBROUTINE nl_set_bdy_outname
SUBROUTINE nl_set_rst_inname ( id_id , rst_inname )
  character*256 , INTENT(IN) :: rst_inname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_rst_inname: rst_inname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%rst_inname = trim(rst_inname) 
  RETURN
END SUBROUTINE nl_set_rst_inname
SUBROUTINE nl_set_rst_outname ( id_id , rst_outname )
  character*256 , INTENT(IN) :: rst_outname
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_rst_outname: rst_outname applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%rst_outname = trim(rst_outname) 
  RETURN
END SUBROUTINE nl_set_rst_outname
SUBROUTINE nl_set_write_input ( id_id , write_input )
  logical , INTENT(IN) :: write_input
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_write_input: write_input applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%write_input = write_input 
  RETURN
END SUBROUTINE nl_set_write_input
SUBROUTINE nl_set_write_restart_at_0h ( id_id , write_restart_at_0h )
  logical , INTENT(IN) :: write_restart_at_0h
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_write_restart_at_0h: write_restart_at_0h applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%write_restart_at_0h = write_restart_at_0h 
  RETURN
END SUBROUTINE nl_set_write_restart_at_0h
SUBROUTINE nl_set_adjust_output_times ( id_id , adjust_output_times )
  logical , INTENT(IN) :: adjust_output_times
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_adjust_output_times: adjust_output_times applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%adjust_output_times = adjust_output_times 
  RETURN
END SUBROUTINE nl_set_adjust_output_times
SUBROUTINE nl_set_adjust_input_times ( id_id , adjust_input_times )
  logical , INTENT(IN) :: adjust_input_times
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_adjust_input_times: adjust_input_times applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%adjust_input_times = adjust_input_times 
  RETURN
END SUBROUTINE nl_set_adjust_input_times
SUBROUTINE nl_set_tstart ( id_id , tstart )
  real , INTENT(IN) :: tstart
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_tstart: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%tstart(id_id) = tstart
  RETURN
END SUBROUTINE nl_set_tstart
SUBROUTINE nl_set_nocolons ( id_id , nocolons )
  logical , INTENT(IN) :: nocolons
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_nocolons: nocolons applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%nocolons = nocolons 
  RETURN
END SUBROUTINE nl_set_nocolons
SUBROUTINE nl_set_time_step ( id_id , time_step )
  integer , INTENT(IN) :: time_step
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_time_step: time_step applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%time_step = time_step 
  RETURN
END SUBROUTINE nl_set_time_step
SUBROUTINE nl_set_time_step_fract_num ( id_id , time_step_fract_num )
  integer , INTENT(IN) :: time_step_fract_num
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_time_step_fract_num: time_step_fract_num applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%time_step_fract_num = time_step_fract_num 
  RETURN
END SUBROUTINE nl_set_time_step_fract_num
SUBROUTINE nl_set_time_step_fract_den ( id_id , time_step_fract_den )
  integer , INTENT(IN) :: time_step_fract_den
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_time_step_fract_den: time_step_fract_den applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%time_step_fract_den = time_step_fract_den 
  RETURN
END SUBROUTINE nl_set_time_step_fract_den
SUBROUTINE nl_set_max_dom ( id_id , max_dom )
  integer , INTENT(IN) :: max_dom
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_max_dom: max_dom applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%max_dom = max_dom 
  RETURN
END SUBROUTINE nl_set_max_dom
SUBROUTINE nl_set_s_we ( id_id , s_we )
  integer , INTENT(IN) :: s_we
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_s_we: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%s_we(id_id) = s_we
  RETURN
END SUBROUTINE nl_set_s_we
SUBROUTINE nl_set_e_we ( id_id , e_we )
  integer , INTENT(IN) :: e_we
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_e_we: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%e_we(id_id) = e_we
  RETURN
END SUBROUTINE nl_set_e_we
SUBROUTINE nl_set_s_sn ( id_id , s_sn )
  integer , INTENT(IN) :: s_sn
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_s_sn: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%s_sn(id_id) = s_sn
  RETURN
END SUBROUTINE nl_set_s_sn
SUBROUTINE nl_set_e_sn ( id_id , e_sn )
  integer , INTENT(IN) :: e_sn
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_e_sn: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%e_sn(id_id) = e_sn
  RETURN
END SUBROUTINE nl_set_e_sn
SUBROUTINE nl_set_s_vert ( id_id , s_vert )
  integer , INTENT(IN) :: s_vert
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_s_vert: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%s_vert(id_id) = s_vert
  RETURN
END SUBROUTINE nl_set_s_vert
SUBROUTINE nl_set_e_vert ( id_id , e_vert )
  integer , INTENT(IN) :: e_vert
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_e_vert: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%e_vert(id_id) = e_vert
  RETURN
END SUBROUTINE nl_set_e_vert
SUBROUTINE nl_set_dx ( id_id , dx )
  real , INTENT(IN) :: dx
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_dx: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%dx(id_id) = dx
  RETURN
END SUBROUTINE nl_set_dx
SUBROUTINE nl_set_dy ( id_id , dy )
  real , INTENT(IN) :: dy
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_dy: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%dy(id_id) = dy
  RETURN
END SUBROUTINE nl_set_dy
SUBROUTINE nl_set_grid_id ( id_id , grid_id )
  integer , INTENT(IN) :: grid_id
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_grid_id: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%grid_id(id_id) = grid_id
  RETURN
END SUBROUTINE nl_set_grid_id
SUBROUTINE nl_set_parent_id ( id_id , parent_id )
  integer , INTENT(IN) :: parent_id
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_parent_id: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%parent_id(id_id) = parent_id
  RETURN
END SUBROUTINE nl_set_parent_id
SUBROUTINE nl_set_i_parent_start ( id_id , i_parent_start )
  integer , INTENT(IN) :: i_parent_start
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_i_parent_start: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%i_parent_start(id_id) = i_parent_start
  RETURN
END SUBROUTINE nl_set_i_parent_start
SUBROUTINE nl_set_j_parent_start ( id_id , j_parent_start )
  integer , INTENT(IN) :: j_parent_start
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_j_parent_start: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%j_parent_start(id_id) = j_parent_start
  RETURN
END SUBROUTINE nl_set_j_parent_start
SUBROUTINE nl_set_parent_grid_ratio ( id_id , parent_grid_ratio )
  integer , INTENT(IN) :: parent_grid_ratio
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_parent_grid_ratio: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%parent_grid_ratio(id_id) = parent_grid_ratio
  RETURN
END SUBROUTINE nl_set_parent_grid_ratio
SUBROUTINE nl_set_parent_time_step_ratio ( id_id , parent_time_step_ratio )
  integer , INTENT(IN) :: parent_time_step_ratio
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_parent_time_step_ratio: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%parent_time_step_ratio(id_id) = parent_time_step_ratio
  RETURN
END SUBROUTINE nl_set_parent_time_step_ratio
SUBROUTINE nl_set_feedback ( id_id , feedback )
  integer , INTENT(IN) :: feedback
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_feedback: feedback applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%feedback = feedback 
  RETURN
END SUBROUTINE nl_set_feedback
SUBROUTINE nl_set_smooth_option ( id_id , smooth_option )
  integer , INTENT(IN) :: smooth_option
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_smooth_option: smooth_option applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%smooth_option = smooth_option 
  RETURN
END SUBROUTINE nl_set_smooth_option
SUBROUTINE nl_set_ztop ( id_id , ztop )
  real , INTENT(IN) :: ztop
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_ztop: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%ztop(id_id) = ztop
  RETURN
END SUBROUTINE nl_set_ztop
SUBROUTINE nl_set_moad_grid_ratio ( id_id , moad_grid_ratio )
  integer , INTENT(IN) :: moad_grid_ratio
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_moad_grid_ratio: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%moad_grid_ratio(id_id) = moad_grid_ratio
  RETURN
END SUBROUTINE nl_set_moad_grid_ratio
SUBROUTINE nl_set_moad_time_step_ratio ( id_id , moad_time_step_ratio )
  integer , INTENT(IN) :: moad_time_step_ratio
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_moad_time_step_ratio: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%moad_time_step_ratio(id_id) = moad_time_step_ratio
  RETURN
END SUBROUTINE nl_set_moad_time_step_ratio
SUBROUTINE nl_set_shw ( id_id , shw )
  integer , INTENT(IN) :: shw
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_shw: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%shw(id_id) = shw
  RETURN
END SUBROUTINE nl_set_shw
SUBROUTINE nl_set_tile_sz_x ( id_id , tile_sz_x )
  integer , INTENT(IN) :: tile_sz_x
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_tile_sz_x: tile_sz_x applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%tile_sz_x = tile_sz_x 
  RETURN
END SUBROUTINE nl_set_tile_sz_x
SUBROUTINE nl_set_tile_sz_y ( id_id , tile_sz_y )
  integer , INTENT(IN) :: tile_sz_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_tile_sz_y: tile_sz_y applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%tile_sz_y = tile_sz_y 
  RETURN
END SUBROUTINE nl_set_tile_sz_y
SUBROUTINE nl_set_numtiles ( id_id , numtiles )
  integer , INTENT(IN) :: numtiles
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_numtiles: numtiles applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%numtiles = numtiles 
  RETURN
END SUBROUTINE nl_set_numtiles
SUBROUTINE nl_set_nproc_x ( id_id , nproc_x )
  integer , INTENT(IN) :: nproc_x
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_nproc_x: nproc_x applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%nproc_x = nproc_x 
  RETURN
END SUBROUTINE nl_set_nproc_x
SUBROUTINE nl_set_nproc_y ( id_id , nproc_y )
  integer , INTENT(IN) :: nproc_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_nproc_y: nproc_y applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%nproc_y = nproc_y 
  RETURN
END SUBROUTINE nl_set_nproc_y
SUBROUTINE nl_set_irand ( id_id , irand )
  integer , INTENT(IN) :: irand
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_irand: irand applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%irand = irand 
  RETURN
END SUBROUTINE nl_set_irand
SUBROUTINE nl_set_dt ( id_id , dt )
  real , INTENT(IN) :: dt
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_dt: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%dt(id_id) = dt
  RETURN
END SUBROUTINE nl_set_dt
SUBROUTINE nl_set_num_moves ( id_id , num_moves )
  integer , INTENT(IN) :: num_moves
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_num_moves: num_moves applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%num_moves = num_moves 
  RETURN
END SUBROUTINE nl_set_num_moves
SUBROUTINE nl_set_move_id ( id_id , move_id )
  integer , INTENT(IN) :: move_id
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%num_moves ) THEN
    WRITE(emess,*)'nl_set_move_id: Out of range move number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%move_id(id_id) = move_id
  RETURN
END SUBROUTINE nl_set_move_id
SUBROUTINE nl_set_move_interval ( id_id , move_interval )
  integer , INTENT(IN) :: move_interval
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%num_moves ) THEN
    WRITE(emess,*)'nl_set_move_interval: Out of range move number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%move_interval(id_id) = move_interval
  RETURN
END SUBROUTINE nl_set_move_interval
SUBROUTINE nl_set_move_cd_x ( id_id , move_cd_x )
  integer , INTENT(IN) :: move_cd_x
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%num_moves ) THEN
    WRITE(emess,*)'nl_set_move_cd_x: Out of range move number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%move_cd_x(id_id) = move_cd_x
  RETURN
END SUBROUTINE nl_set_move_cd_x
SUBROUTINE nl_set_move_cd_y ( id_id , move_cd_y )
  integer , INTENT(IN) :: move_cd_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%num_moves ) THEN
    WRITE(emess,*)'nl_set_move_cd_y: Out of range move number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%move_cd_y(id_id) = move_cd_y
  RETURN
END SUBROUTINE nl_set_move_cd_y
SUBROUTINE nl_set_swap_x ( id_id , swap_x )
  logical , INTENT(IN) :: swap_x
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_swap_x: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%swap_x(id_id) = swap_x
  RETURN
END SUBROUTINE nl_set_swap_x
SUBROUTINE nl_set_swap_y ( id_id , swap_y )
  logical , INTENT(IN) :: swap_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_swap_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%swap_y(id_id) = swap_y
  RETURN
END SUBROUTINE nl_set_swap_y
SUBROUTINE nl_set_cycle_x ( id_id , cycle_x )
  logical , INTENT(IN) :: cycle_x
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_cycle_x: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%cycle_x(id_id) = cycle_x
  RETURN
END SUBROUTINE nl_set_cycle_x
SUBROUTINE nl_set_cycle_y ( id_id , cycle_y )
  logical , INTENT(IN) :: cycle_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_cycle_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%cycle_y(id_id) = cycle_y
  RETURN
END SUBROUTINE nl_set_cycle_y
SUBROUTINE nl_set_reorder_mesh ( id_id , reorder_mesh )
  logical , INTENT(IN) :: reorder_mesh
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_reorder_mesh: reorder_mesh applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%reorder_mesh = reorder_mesh 
  RETURN
END SUBROUTINE nl_set_reorder_mesh
SUBROUTINE nl_set_perturb_input ( id_id , perturb_input )
  logical , INTENT(IN) :: perturb_input
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_perturb_input: perturb_input applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%perturb_input = perturb_input 
  RETURN
END SUBROUTINE nl_set_perturb_input
SUBROUTINE nl_set_eta_levels ( id_id , eta_levels )
  real , INTENT(IN) :: eta_levels
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%e_vert(1) ) THEN
    WRITE(emess,*)'nl_set_eta_levels: Out of range eta_level number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%eta_levels(id_id) = eta_levels
  RETURN
END SUBROUTINE nl_set_eta_levels
SUBROUTINE nl_set_ptsgm ( id_id , ptsgm )
  real , INTENT(IN) :: ptsgm
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_ptsgm: ptsgm applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%ptsgm = ptsgm 
  RETURN
END SUBROUTINE nl_set_ptsgm
SUBROUTINE nl_set_num_metgrid_levels ( id_id , num_metgrid_levels )
  integer , INTENT(IN) :: num_metgrid_levels
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_num_metgrid_levels: num_metgrid_levels applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%num_metgrid_levels = num_metgrid_levels 
  RETURN
END SUBROUTINE nl_set_num_metgrid_levels
SUBROUTINE nl_set_p_top_requested ( id_id , p_top_requested )
  real , INTENT(IN) :: p_top_requested
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_p_top_requested: p_top_requested applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%p_top_requested = p_top_requested 
  RETURN
END SUBROUTINE nl_set_p_top_requested
SUBROUTINE nl_set_mp_physics ( id_id , mp_physics )
  integer , INTENT(IN) :: mp_physics
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_mp_physics: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%mp_physics(id_id) = mp_physics
  RETURN
END SUBROUTINE nl_set_mp_physics
SUBROUTINE nl_set_ra_lw_physics ( id_id , ra_lw_physics )
  integer , INTENT(IN) :: ra_lw_physics
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_ra_lw_physics: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%ra_lw_physics(id_id) = ra_lw_physics
  RETURN
END SUBROUTINE nl_set_ra_lw_physics
SUBROUTINE nl_set_ra_sw_physics ( id_id , ra_sw_physics )
  integer , INTENT(IN) :: ra_sw_physics
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_ra_sw_physics: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%ra_sw_physics(id_id) = ra_sw_physics
  RETURN
END SUBROUTINE nl_set_ra_sw_physics
SUBROUTINE nl_set_radt ( id_id , radt )
  real , INTENT(IN) :: radt
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_radt: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%radt(id_id) = radt
  RETURN
END SUBROUTINE nl_set_radt
SUBROUTINE nl_set_sf_sfclay_physics ( id_id , sf_sfclay_physics )
  integer , INTENT(IN) :: sf_sfclay_physics
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_sf_sfclay_physics: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%sf_sfclay_physics(id_id) = sf_sfclay_physics
  RETURN
END SUBROUTINE nl_set_sf_sfclay_physics
SUBROUTINE nl_set_sf_surface_physics ( id_id , sf_surface_physics )
  integer , INTENT(IN) :: sf_surface_physics
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_sf_surface_physics: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%sf_surface_physics(id_id) = sf_surface_physics
  RETURN
END SUBROUTINE nl_set_sf_surface_physics
SUBROUTINE nl_set_bl_pbl_physics ( id_id , bl_pbl_physics )
  integer , INTENT(IN) :: bl_pbl_physics
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_bl_pbl_physics: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%bl_pbl_physics(id_id) = bl_pbl_physics
  RETURN
END SUBROUTINE nl_set_bl_pbl_physics
SUBROUTINE nl_set_bldt ( id_id , bldt )
  real , INTENT(IN) :: bldt
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_bldt: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%bldt(id_id) = bldt
  RETURN
END SUBROUTINE nl_set_bldt
SUBROUTINE nl_set_cu_physics ( id_id , cu_physics )
  integer , INTENT(IN) :: cu_physics
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_cu_physics: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%cu_physics(id_id) = cu_physics
  RETURN
END SUBROUTINE nl_set_cu_physics
SUBROUTINE nl_set_cudt ( id_id , cudt )
  real , INTENT(IN) :: cudt
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_cudt: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%cudt(id_id) = cudt
  RETURN
END SUBROUTINE nl_set_cudt
SUBROUTINE nl_set_gsmdt ( id_id , gsmdt )
  real , INTENT(IN) :: gsmdt
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_gsmdt: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%gsmdt(id_id) = gsmdt
  RETURN
END SUBROUTINE nl_set_gsmdt
SUBROUTINE nl_set_isfflx ( id_id , isfflx )
  integer , INTENT(IN) :: isfflx
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_isfflx: isfflx applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%isfflx = isfflx 
  RETURN
END SUBROUTINE nl_set_isfflx
SUBROUTINE nl_set_ifsnow ( id_id , ifsnow )
  integer , INTENT(IN) :: ifsnow
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_ifsnow: ifsnow applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%ifsnow = ifsnow 
  RETURN
END SUBROUTINE nl_set_ifsnow
SUBROUTINE nl_set_icloud ( id_id , icloud )
  integer , INTENT(IN) :: icloud
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_icloud: icloud applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%icloud = icloud 
  RETURN
END SUBROUTINE nl_set_icloud
SUBROUTINE nl_set_swrad_scat ( id_id , swrad_scat )
  real , INTENT(IN) :: swrad_scat
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_swrad_scat: swrad_scat applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%swrad_scat = swrad_scat 
  RETURN
END SUBROUTINE nl_set_swrad_scat
SUBROUTINE nl_set_surface_input_source ( id_id , surface_input_source )
  integer , INTENT(IN) :: surface_input_source
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_surface_input_source: surface_input_source applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%surface_input_source = surface_input_source 
  RETURN
END SUBROUTINE nl_set_surface_input_source
SUBROUTINE nl_set_num_soil_layers ( id_id , num_soil_layers )
  integer , INTENT(IN) :: num_soil_layers
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_num_soil_layers: num_soil_layers applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%num_soil_layers = num_soil_layers 
  RETURN
END SUBROUTINE nl_set_num_soil_layers
SUBROUTINE nl_set_maxiens ( id_id , maxiens )
  integer , INTENT(IN) :: maxiens
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_maxiens: maxiens applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%maxiens = maxiens 
  RETURN
END SUBROUTINE nl_set_maxiens
SUBROUTINE nl_set_maxens ( id_id , maxens )
  integer , INTENT(IN) :: maxens
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_maxens: maxens applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%maxens = maxens 
  RETURN
END SUBROUTINE nl_set_maxens
SUBROUTINE nl_set_maxens2 ( id_id , maxens2 )
  integer , INTENT(IN) :: maxens2
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_maxens2: maxens2 applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%maxens2 = maxens2 
  RETURN
END SUBROUTINE nl_set_maxens2
SUBROUTINE nl_set_maxens3 ( id_id , maxens3 )
  integer , INTENT(IN) :: maxens3
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_maxens3: maxens3 applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%maxens3 = maxens3 
  RETURN
END SUBROUTINE nl_set_maxens3
SUBROUTINE nl_set_ensdim ( id_id , ensdim )
  integer , INTENT(IN) :: ensdim
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_ensdim: ensdim applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%ensdim = ensdim 
  RETURN
END SUBROUTINE nl_set_ensdim
SUBROUTINE nl_set_chem_opt ( id_id , chem_opt )
  integer , INTENT(IN) :: chem_opt
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_chem_opt: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%chem_opt(id_id) = chem_opt
  RETURN
END SUBROUTINE nl_set_chem_opt
SUBROUTINE nl_set_num_land_cat ( id_id , num_land_cat )
  integer , INTENT(IN) :: num_land_cat
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_num_land_cat: num_land_cat applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%num_land_cat = num_land_cat 
  RETURN
END SUBROUTINE nl_set_num_land_cat
SUBROUTINE nl_set_num_soil_cat ( id_id , num_soil_cat )
  integer , INTENT(IN) :: num_soil_cat
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_num_soil_cat: num_soil_cat applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%num_soil_cat = num_soil_cat 
  RETURN
END SUBROUTINE nl_set_num_soil_cat
SUBROUTINE nl_set_mp_zero_out ( id_id , mp_zero_out )
  integer , INTENT(IN) :: mp_zero_out
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_mp_zero_out: mp_zero_out applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%mp_zero_out = mp_zero_out 
  RETURN
END SUBROUTINE nl_set_mp_zero_out
SUBROUTINE nl_set_mp_zero_out_thresh ( id_id , mp_zero_out_thresh )
  real , INTENT(IN) :: mp_zero_out_thresh
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_mp_zero_out_thresh: mp_zero_out_thresh applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%mp_zero_out_thresh = mp_zero_out_thresh 
  RETURN
END SUBROUTINE nl_set_mp_zero_out_thresh
SUBROUTINE nl_set_seaice_threshold ( id_id , seaice_threshold )
  real , INTENT(IN) :: seaice_threshold
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_seaice_threshold: seaice_threshold applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%seaice_threshold = seaice_threshold 
  RETURN
END SUBROUTINE nl_set_seaice_threshold
SUBROUTINE nl_set_sst_update ( id_id , sst_update )
  integer , INTENT(IN) :: sst_update
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_sst_update: sst_update applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%sst_update = sst_update 
  RETURN
END SUBROUTINE nl_set_sst_update
SUBROUTINE nl_set_ucmcall ( id_id , ucmcall )
  integer , INTENT(IN) :: ucmcall
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_ucmcall: ucmcall applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%ucmcall = ucmcall 
  RETURN
END SUBROUTINE nl_set_ucmcall
SUBROUTINE nl_set_idtad ( id_id , idtad )
  integer , INTENT(IN) :: idtad
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_idtad: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%idtad(id_id) = idtad
  RETURN
END SUBROUTINE nl_set_idtad
SUBROUTINE nl_set_nsoil ( id_id , nsoil )
  integer , INTENT(IN) :: nsoil
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_nsoil: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%nsoil(id_id) = nsoil
  RETURN
END SUBROUTINE nl_set_nsoil
SUBROUTINE nl_set_nphs ( id_id , nphs )
  integer , INTENT(IN) :: nphs
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_nphs: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%nphs(id_id) = nphs
  RETURN
END SUBROUTINE nl_set_nphs
SUBROUTINE nl_set_ncnvc ( id_id , ncnvc )
  integer , INTENT(IN) :: ncnvc
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_ncnvc: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%ncnvc(id_id) = ncnvc
  RETURN
END SUBROUTINE nl_set_ncnvc
SUBROUTINE nl_set_nrads ( id_id , nrads )
  integer , INTENT(IN) :: nrads
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_nrads: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%nrads(id_id) = nrads
  RETURN
END SUBROUTINE nl_set_nrads
SUBROUTINE nl_set_nradl ( id_id , nradl )
  integer , INTENT(IN) :: nradl
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_nradl: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%nradl(id_id) = nradl
  RETURN
END SUBROUTINE nl_set_nradl
SUBROUTINE nl_set_tprec ( id_id , tprec )
  real , INTENT(IN) :: tprec
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_tprec: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%tprec(id_id) = tprec
  RETURN
END SUBROUTINE nl_set_tprec
SUBROUTINE nl_set_theat ( id_id , theat )
  real , INTENT(IN) :: theat
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_theat: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%theat(id_id) = theat
  RETURN
END SUBROUTINE nl_set_theat
SUBROUTINE nl_set_tclod ( id_id , tclod )
  real , INTENT(IN) :: tclod
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_tclod: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%tclod(id_id) = tclod
  RETURN
END SUBROUTINE nl_set_tclod
SUBROUTINE nl_set_trdsw ( id_id , trdsw )
  real , INTENT(IN) :: trdsw
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_trdsw: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%trdsw(id_id) = trdsw
  RETURN
END SUBROUTINE nl_set_trdsw
SUBROUTINE nl_set_trdlw ( id_id , trdlw )
  real , INTENT(IN) :: trdlw
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_trdlw: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%trdlw(id_id) = trdlw
  RETURN
END SUBROUTINE nl_set_trdlw
SUBROUTINE nl_set_tsrfc ( id_id , tsrfc )
  real , INTENT(IN) :: tsrfc
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_tsrfc: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%tsrfc(id_id) = tsrfc
  RETURN
END SUBROUTINE nl_set_tsrfc
SUBROUTINE nl_set_pcpflg ( id_id , pcpflg )
  logical , INTENT(IN) :: pcpflg
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_pcpflg: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%pcpflg(id_id) = pcpflg
  RETURN
END SUBROUTINE nl_set_pcpflg
SUBROUTINE nl_set_sigma ( id_id , sigma )
  integer , INTENT(IN) :: sigma
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_sigma: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%sigma(id_id) = sigma
  RETURN
END SUBROUTINE nl_set_sigma
SUBROUTINE nl_set_co2tf ( id_id , co2tf )
  integer , INTENT(IN) :: co2tf
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_co2tf: co2tf applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%co2tf = co2tf 
  RETURN
END SUBROUTINE nl_set_co2tf
SUBROUTINE nl_set_ra_call_offset ( id_id , ra_call_offset )
  integer , INTENT(IN) :: ra_call_offset
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_ra_call_offset: ra_call_offset applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%ra_call_offset = ra_call_offset 
  RETURN
END SUBROUTINE nl_set_ra_call_offset
SUBROUTINE nl_set_cam_abs_freq_s ( id_id , cam_abs_freq_s )
  real , INTENT(IN) :: cam_abs_freq_s
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_cam_abs_freq_s: cam_abs_freq_s applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%cam_abs_freq_s = cam_abs_freq_s 
  RETURN
END SUBROUTINE nl_set_cam_abs_freq_s
SUBROUTINE nl_set_levsiz ( id_id , levsiz )
  integer , INTENT(IN) :: levsiz
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_levsiz: levsiz applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%levsiz = levsiz 
  RETURN
END SUBROUTINE nl_set_levsiz
SUBROUTINE nl_set_paerlev ( id_id , paerlev )
  integer , INTENT(IN) :: paerlev
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_paerlev: paerlev applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%paerlev = paerlev 
  RETURN
END SUBROUTINE nl_set_paerlev
SUBROUTINE nl_set_cam_abs_dim1 ( id_id , cam_abs_dim1 )
  integer , INTENT(IN) :: cam_abs_dim1
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_cam_abs_dim1: cam_abs_dim1 applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%cam_abs_dim1 = cam_abs_dim1 
  RETURN
END SUBROUTINE nl_set_cam_abs_dim1
SUBROUTINE nl_set_cam_abs_dim2 ( id_id , cam_abs_dim2 )
  integer , INTENT(IN) :: cam_abs_dim2
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_cam_abs_dim2: cam_abs_dim2 applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%cam_abs_dim2 = cam_abs_dim2 
  RETURN
END SUBROUTINE nl_set_cam_abs_dim2
SUBROUTINE nl_set_cu_rad_feedback ( id_id , cu_rad_feedback )
  logical , INTENT(IN) :: cu_rad_feedback
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_cu_rad_feedback: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%cu_rad_feedback(id_id) = cu_rad_feedback
  RETURN
END SUBROUTINE nl_set_cu_rad_feedback
SUBROUTINE nl_set_dyn_opt ( id_id , dyn_opt )
  integer , INTENT(IN) :: dyn_opt
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_dyn_opt: dyn_opt applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%dyn_opt = dyn_opt 
  RETURN
END SUBROUTINE nl_set_dyn_opt
SUBROUTINE nl_set_rk_ord ( id_id , rk_ord )
  integer , INTENT(IN) :: rk_ord
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_rk_ord: rk_ord applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%rk_ord = rk_ord 
  RETURN
END SUBROUTINE nl_set_rk_ord
SUBROUTINE nl_set_w_damping ( id_id , w_damping )
  integer , INTENT(IN) :: w_damping
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_w_damping: w_damping applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%w_damping = w_damping 
  RETURN
END SUBROUTINE nl_set_w_damping
SUBROUTINE nl_set_diff_opt ( id_id , diff_opt )
  integer , INTENT(IN) :: diff_opt
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_diff_opt: diff_opt applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%diff_opt = diff_opt 
  RETURN
END SUBROUTINE nl_set_diff_opt
SUBROUTINE nl_set_km_opt ( id_id , km_opt )
  integer , INTENT(IN) :: km_opt
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_km_opt: km_opt applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%km_opt = km_opt 
  RETURN
END SUBROUTINE nl_set_km_opt
SUBROUTINE nl_set_damp_opt ( id_id , damp_opt )
  integer , INTENT(IN) :: damp_opt
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_damp_opt: damp_opt applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%damp_opt = damp_opt 
  RETURN
END SUBROUTINE nl_set_damp_opt
SUBROUTINE nl_set_zdamp ( id_id , zdamp )
  real , INTENT(IN) :: zdamp
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_zdamp: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%zdamp(id_id) = zdamp
  RETURN
END SUBROUTINE nl_set_zdamp
SUBROUTINE nl_set_base_pres ( id_id , base_pres )
  real , INTENT(IN) :: base_pres
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_base_pres: base_pres applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%base_pres = base_pres 
  RETURN
END SUBROUTINE nl_set_base_pres
SUBROUTINE nl_set_base_temp ( id_id , base_temp )
  real , INTENT(IN) :: base_temp
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_base_temp: base_temp applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%base_temp = base_temp 
  RETURN
END SUBROUTINE nl_set_base_temp
SUBROUTINE nl_set_base_lapse ( id_id , base_lapse )
  real , INTENT(IN) :: base_lapse
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_base_lapse: base_lapse applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%base_lapse = base_lapse 
  RETURN
END SUBROUTINE nl_set_base_lapse
SUBROUTINE nl_set_dampcoef ( id_id , dampcoef )
  real , INTENT(IN) :: dampcoef
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_dampcoef: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%dampcoef(id_id) = dampcoef
  RETURN
END SUBROUTINE nl_set_dampcoef
SUBROUTINE nl_set_khdif ( id_id , khdif )
  real , INTENT(IN) :: khdif
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_khdif: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%khdif(id_id) = khdif
  RETURN
END SUBROUTINE nl_set_khdif
SUBROUTINE nl_set_kvdif ( id_id , kvdif )
  real , INTENT(IN) :: kvdif
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_kvdif: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%kvdif(id_id) = kvdif
  RETURN
END SUBROUTINE nl_set_kvdif
SUBROUTINE nl_set_smdiv ( id_id , smdiv )
  real , INTENT(IN) :: smdiv
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_smdiv: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%smdiv(id_id) = smdiv
  RETURN
END SUBROUTINE nl_set_smdiv
SUBROUTINE nl_set_emdiv ( id_id , emdiv )
  real , INTENT(IN) :: emdiv
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_emdiv: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%emdiv(id_id) = emdiv
  RETURN
END SUBROUTINE nl_set_emdiv
SUBROUTINE nl_set_epssm ( id_id , epssm )
  real , INTENT(IN) :: epssm
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_epssm: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%epssm(id_id) = epssm
  RETURN
END SUBROUTINE nl_set_epssm
SUBROUTINE nl_set_non_hydrostatic ( id_id , non_hydrostatic )
  logical , INTENT(IN) :: non_hydrostatic
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_non_hydrostatic: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%non_hydrostatic(id_id) = non_hydrostatic
  RETURN
END SUBROUTINE nl_set_non_hydrostatic
SUBROUTINE nl_set_time_step_sound ( id_id , time_step_sound )
  integer , INTENT(IN) :: time_step_sound
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_time_step_sound: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%time_step_sound(id_id) = time_step_sound
  RETURN
END SUBROUTINE nl_set_time_step_sound
SUBROUTINE nl_set_h_mom_adv_order ( id_id , h_mom_adv_order )
  integer , INTENT(IN) :: h_mom_adv_order
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_h_mom_adv_order: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%h_mom_adv_order(id_id) = h_mom_adv_order
  RETURN
END SUBROUTINE nl_set_h_mom_adv_order
SUBROUTINE nl_set_v_mom_adv_order ( id_id , v_mom_adv_order )
  integer , INTENT(IN) :: v_mom_adv_order
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_v_mom_adv_order: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%v_mom_adv_order(id_id) = v_mom_adv_order
  RETURN
END SUBROUTINE nl_set_v_mom_adv_order
SUBROUTINE nl_set_h_sca_adv_order ( id_id , h_sca_adv_order )
  integer , INTENT(IN) :: h_sca_adv_order
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_h_sca_adv_order: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%h_sca_adv_order(id_id) = h_sca_adv_order
  RETURN
END SUBROUTINE nl_set_h_sca_adv_order
SUBROUTINE nl_set_v_sca_adv_order ( id_id , v_sca_adv_order )
  integer , INTENT(IN) :: v_sca_adv_order
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_v_sca_adv_order: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%v_sca_adv_order(id_id) = v_sca_adv_order
  RETURN
END SUBROUTINE nl_set_v_sca_adv_order
SUBROUTINE nl_set_top_radiation ( id_id , top_radiation )
  logical , INTENT(IN) :: top_radiation
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_top_radiation: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%top_radiation(id_id) = top_radiation
  RETURN
END SUBROUTINE nl_set_top_radiation
SUBROUTINE nl_set_mix_cr_len ( id_id , mix_cr_len )
  real , INTENT(IN) :: mix_cr_len
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_mix_cr_len: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%mix_cr_len(id_id) = mix_cr_len
  RETURN
END SUBROUTINE nl_set_mix_cr_len
SUBROUTINE nl_set_tke_upper_bound ( id_id , tke_upper_bound )
  real , INTENT(IN) :: tke_upper_bound
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_tke_upper_bound: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%tke_upper_bound(id_id) = tke_upper_bound
  RETURN
END SUBROUTINE nl_set_tke_upper_bound
SUBROUTINE nl_set_kh_tke_upper_bound ( id_id , kh_tke_upper_bound )
  real , INTENT(IN) :: kh_tke_upper_bound
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_kh_tke_upper_bound: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%kh_tke_upper_bound(id_id) = kh_tke_upper_bound
  RETURN
END SUBROUTINE nl_set_kh_tke_upper_bound
SUBROUTINE nl_set_kv_tke_upper_bound ( id_id , kv_tke_upper_bound )
  real , INTENT(IN) :: kv_tke_upper_bound
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_kv_tke_upper_bound: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%kv_tke_upper_bound(id_id) = kv_tke_upper_bound
  RETURN
END SUBROUTINE nl_set_kv_tke_upper_bound
SUBROUTINE nl_set_tke_drag_coefficient ( id_id , tke_drag_coefficient )
  real , INTENT(IN) :: tke_drag_coefficient
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_tke_drag_coefficient: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%tke_drag_coefficient(id_id) = tke_drag_coefficient
  RETURN
END SUBROUTINE nl_set_tke_drag_coefficient
SUBROUTINE nl_set_tke_heat_flux ( id_id , tke_heat_flux )
  real , INTENT(IN) :: tke_heat_flux
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_tke_heat_flux: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%tke_heat_flux(id_id) = tke_heat_flux
  RETURN
END SUBROUTINE nl_set_tke_heat_flux
SUBROUTINE nl_set_pert_coriolis ( id_id , pert_coriolis )
  logical , INTENT(IN) :: pert_coriolis
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_pert_coriolis: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%pert_coriolis(id_id) = pert_coriolis
  RETURN
END SUBROUTINE nl_set_pert_coriolis
SUBROUTINE nl_set_spec_bdy_width ( id_id , spec_bdy_width )
  integer , INTENT(IN) :: spec_bdy_width
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_spec_bdy_width: spec_bdy_width applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%spec_bdy_width = spec_bdy_width 
  RETURN
END SUBROUTINE nl_set_spec_bdy_width
SUBROUTINE nl_set_spec_zone ( id_id , spec_zone )
  integer , INTENT(IN) :: spec_zone
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_spec_zone: spec_zone applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%spec_zone = spec_zone 
  RETURN
END SUBROUTINE nl_set_spec_zone
SUBROUTINE nl_set_relax_zone ( id_id , relax_zone )
  integer , INTENT(IN) :: relax_zone
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_relax_zone: relax_zone applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%relax_zone = relax_zone 
  RETURN
END SUBROUTINE nl_set_relax_zone
SUBROUTINE nl_set_specified ( id_id , specified )
  logical , INTENT(IN) :: specified
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_specified: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%specified(id_id) = specified
  RETURN
END SUBROUTINE nl_set_specified
SUBROUTINE nl_set_periodic_x ( id_id , periodic_x )
  logical , INTENT(IN) :: periodic_x
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_periodic_x: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%periodic_x(id_id) = periodic_x
  RETURN
END SUBROUTINE nl_set_periodic_x
SUBROUTINE nl_set_symmetric_xs ( id_id , symmetric_xs )
  logical , INTENT(IN) :: symmetric_xs
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_symmetric_xs: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%symmetric_xs(id_id) = symmetric_xs
  RETURN
END SUBROUTINE nl_set_symmetric_xs
SUBROUTINE nl_set_symmetric_xe ( id_id , symmetric_xe )
  logical , INTENT(IN) :: symmetric_xe
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_symmetric_xe: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%symmetric_xe(id_id) = symmetric_xe
  RETURN
END SUBROUTINE nl_set_symmetric_xe
SUBROUTINE nl_set_open_xs ( id_id , open_xs )
  logical , INTENT(IN) :: open_xs
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_open_xs: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%open_xs(id_id) = open_xs
  RETURN
END SUBROUTINE nl_set_open_xs
SUBROUTINE nl_set_open_xe ( id_id , open_xe )
  logical , INTENT(IN) :: open_xe
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_open_xe: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%open_xe(id_id) = open_xe
  RETURN
END SUBROUTINE nl_set_open_xe
SUBROUTINE nl_set_periodic_y ( id_id , periodic_y )
  logical , INTENT(IN) :: periodic_y
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_periodic_y: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%periodic_y(id_id) = periodic_y
  RETURN
END SUBROUTINE nl_set_periodic_y
SUBROUTINE nl_set_symmetric_ys ( id_id , symmetric_ys )
  logical , INTENT(IN) :: symmetric_ys
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_symmetric_ys: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%symmetric_ys(id_id) = symmetric_ys
  RETURN
END SUBROUTINE nl_set_symmetric_ys
SUBROUTINE nl_set_symmetric_ye ( id_id , symmetric_ye )
  logical , INTENT(IN) :: symmetric_ye
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_symmetric_ye: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%symmetric_ye(id_id) = symmetric_ye
  RETURN
END SUBROUTINE nl_set_symmetric_ye
SUBROUTINE nl_set_open_ys ( id_id , open_ys )
  logical , INTENT(IN) :: open_ys
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_open_ys: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%open_ys(id_id) = open_ys
  RETURN
END SUBROUTINE nl_set_open_ys
SUBROUTINE nl_set_open_ye ( id_id , open_ye )
  logical , INTENT(IN) :: open_ye
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_open_ye: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%open_ye(id_id) = open_ye
  RETURN
END SUBROUTINE nl_set_open_ye
SUBROUTINE nl_set_nested ( id_id , nested )
  logical , INTENT(IN) :: nested
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_nested: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%nested(id_id) = nested
  RETURN
END SUBROUTINE nl_set_nested
SUBROUTINE nl_set_real_data_init_type ( id_id , real_data_init_type )
  integer , INTENT(IN) :: real_data_init_type
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_real_data_init_type: real_data_init_type applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%real_data_init_type = real_data_init_type 
  RETURN
END SUBROUTINE nl_set_real_data_init_type
SUBROUTINE nl_set_background_proc_id ( id_id , background_proc_id )
  integer , INTENT(IN) :: background_proc_id
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_background_proc_id: background_proc_id applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%background_proc_id = background_proc_id 
  RETURN
END SUBROUTINE nl_set_background_proc_id
SUBROUTINE nl_set_forecast_proc_id ( id_id , forecast_proc_id )
  integer , INTENT(IN) :: forecast_proc_id
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_forecast_proc_id: forecast_proc_id applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%forecast_proc_id = forecast_proc_id 
  RETURN
END SUBROUTINE nl_set_forecast_proc_id
SUBROUTINE nl_set_production_status ( id_id , production_status )
  integer , INTENT(IN) :: production_status
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_production_status: production_status applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%production_status = production_status 
  RETURN
END SUBROUTINE nl_set_production_status
SUBROUTINE nl_set_compression ( id_id , compression )
  integer , INTENT(IN) :: compression
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_compression: compression applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%compression = compression 
  RETURN
END SUBROUTINE nl_set_compression
SUBROUTINE nl_set_cen_lat ( id_id , cen_lat )
  real , INTENT(IN) :: cen_lat
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_cen_lat: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%cen_lat(id_id) = cen_lat
  RETURN
END SUBROUTINE nl_set_cen_lat
SUBROUTINE nl_set_cen_lon ( id_id , cen_lon )
  real , INTENT(IN) :: cen_lon
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_cen_lon: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%cen_lon(id_id) = cen_lon
  RETURN
END SUBROUTINE nl_set_cen_lon
SUBROUTINE nl_set_truelat1 ( id_id , truelat1 )
  real , INTENT(IN) :: truelat1
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_truelat1: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%truelat1(id_id) = truelat1
  RETURN
END SUBROUTINE nl_set_truelat1
SUBROUTINE nl_set_truelat2 ( id_id , truelat2 )
  real , INTENT(IN) :: truelat2
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_truelat2: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%truelat2(id_id) = truelat2
  RETURN
END SUBROUTINE nl_set_truelat2
SUBROUTINE nl_set_moad_cen_lat ( id_id , moad_cen_lat )
  real , INTENT(IN) :: moad_cen_lat
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_moad_cen_lat: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%moad_cen_lat(id_id) = moad_cen_lat
  RETURN
END SUBROUTINE nl_set_moad_cen_lat
SUBROUTINE nl_set_stand_lon ( id_id , stand_lon )
  real , INTENT(IN) :: stand_lon
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_stand_lon: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%stand_lon(id_id) = stand_lon
  RETURN
END SUBROUTINE nl_set_stand_lon
SUBROUTINE nl_set_bdyfrq ( id_id , bdyfrq )
  real , INTENT(IN) :: bdyfrq
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_bdyfrq: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%bdyfrq(id_id) = bdyfrq
  RETURN
END SUBROUTINE nl_set_bdyfrq
SUBROUTINE nl_set_iswater ( id_id , iswater )
  integer , INTENT(IN) :: iswater
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_iswater: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%iswater(id_id) = iswater
  RETURN
END SUBROUTINE nl_set_iswater
SUBROUTINE nl_set_isice ( id_id , isice )
  integer , INTENT(IN) :: isice
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_isice: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%isice(id_id) = isice
  RETURN
END SUBROUTINE nl_set_isice
SUBROUTINE nl_set_isurban ( id_id , isurban )
  integer , INTENT(IN) :: isurban
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_isurban: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%isurban(id_id) = isurban
  RETURN
END SUBROUTINE nl_set_isurban
SUBROUTINE nl_set_isoilwater ( id_id , isoilwater )
  integer , INTENT(IN) :: isoilwater
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_isoilwater: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%isoilwater(id_id) = isoilwater
  RETURN
END SUBROUTINE nl_set_isoilwater
SUBROUTINE nl_set_map_proj ( id_id , map_proj )
  integer , INTENT(IN) :: map_proj
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%max_dom ) THEN
    WRITE(emess,*)'nl_set_map_proj: Out of range domain number: ',id_id
    CALL wrf_error_fatal(emess)
  ENDIF
  model_config_rec%map_proj(id_id) = map_proj
  RETURN
END SUBROUTINE nl_set_map_proj
SUBROUTINE nl_set_simulation_start_year ( id_id , simulation_start_year )
  integer , INTENT(IN) :: simulation_start_year
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_simulation_start_year: simulation_start_year applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%simulation_start_year = simulation_start_year 
  RETURN
END SUBROUTINE nl_set_simulation_start_year
SUBROUTINE nl_set_simulation_start_month ( id_id , simulation_start_month )
  integer , INTENT(IN) :: simulation_start_month
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_simulation_start_month: simulation_start_month applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%simulation_start_month = simulation_start_month 
  RETURN
END SUBROUTINE nl_set_simulation_start_month
SUBROUTINE nl_set_simulation_start_day ( id_id , simulation_start_day )
  integer , INTENT(IN) :: simulation_start_day
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_simulation_start_day: simulation_start_day applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%simulation_start_day = simulation_start_day 
  RETURN
END SUBROUTINE nl_set_simulation_start_day
SUBROUTINE nl_set_simulation_start_hour ( id_id , simulation_start_hour )
  integer , INTENT(IN) :: simulation_start_hour
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_simulation_start_hour: simulation_start_hour applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%simulation_start_hour = simulation_start_hour 
  RETURN
END SUBROUTINE nl_set_simulation_start_hour
SUBROUTINE nl_set_simulation_start_minute ( id_id , simulation_start_minute )
  integer , INTENT(IN) :: simulation_start_minute
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_simulation_start_minute: simulation_start_minute applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%simulation_start_minute = simulation_start_minute 
  RETURN
END SUBROUTINE nl_set_simulation_start_minute
SUBROUTINE nl_set_simulation_start_second ( id_id , simulation_start_second )
  integer , INTENT(IN) :: simulation_start_second
  INTEGER id_id
  CHARACTER*80 emess
  IF ( id_id .NE. 1 ) THEN
    call wrf_debug(1,&
'WARNING in nl_set_simulation_start_second: simulation_start_second applies to all domains. First arg ignored.')
  ENDIF
  model_config_rec%simulation_start_second = simulation_start_second 
  RETURN
END SUBROUTINE nl_set_simulation_start_second
!ENDOFREGISTRYGENERATEDINCLUDE
