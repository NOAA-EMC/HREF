





SUBROUTINE not_a_real_sub_a
END SUBROUTINE not_a_real_sub_a







SUBROUTINE nl_get_run_days ( id_id , run_days )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: run_days
  INTEGER id_id
  CHARACTER*80 emess
  run_days = model_config_rec%run_days
  RETURN
END SUBROUTINE nl_get_run_days
SUBROUTINE nl_get_run_hours ( id_id , run_hours )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: run_hours
  INTEGER id_id
  CHARACTER*80 emess
  run_hours = model_config_rec%run_hours
  RETURN
END SUBROUTINE nl_get_run_hours
SUBROUTINE nl_get_run_minutes ( id_id , run_minutes )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: run_minutes
  INTEGER id_id
  CHARACTER*80 emess
  run_minutes = model_config_rec%run_minutes
  RETURN
END SUBROUTINE nl_get_run_minutes
SUBROUTINE nl_get_run_seconds ( id_id , run_seconds )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: run_seconds
  INTEGER id_id
  CHARACTER*80 emess
  run_seconds = model_config_rec%run_seconds
  RETURN
END SUBROUTINE nl_get_run_seconds
SUBROUTINE nl_get_start_year ( id_id , start_year )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: start_year
  INTEGER id_id
  CHARACTER*80 emess
  start_year = model_config_rec%start_year(id_id)
  RETURN
END SUBROUTINE nl_get_start_year
SUBROUTINE nl_get_start_month ( id_id , start_month )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: start_month
  INTEGER id_id
  CHARACTER*80 emess
  start_month = model_config_rec%start_month(id_id)
  RETURN
END SUBROUTINE nl_get_start_month
SUBROUTINE nl_get_start_day ( id_id , start_day )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: start_day
  INTEGER id_id
  CHARACTER*80 emess
  start_day = model_config_rec%start_day(id_id)
  RETURN
END SUBROUTINE nl_get_start_day
SUBROUTINE nl_get_start_hour ( id_id , start_hour )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: start_hour
  INTEGER id_id
  CHARACTER*80 emess
  start_hour = model_config_rec%start_hour(id_id)
  RETURN
END SUBROUTINE nl_get_start_hour
SUBROUTINE nl_get_start_minute ( id_id , start_minute )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: start_minute
  INTEGER id_id
  CHARACTER*80 emess
  start_minute = model_config_rec%start_minute(id_id)
  RETURN
END SUBROUTINE nl_get_start_minute
SUBROUTINE nl_get_start_second ( id_id , start_second )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: start_second
  INTEGER id_id
  CHARACTER*80 emess
  start_second = model_config_rec%start_second(id_id)
  RETURN
END SUBROUTINE nl_get_start_second
SUBROUTINE nl_get_end_year ( id_id , end_year )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: end_year
  INTEGER id_id
  CHARACTER*80 emess
  end_year = model_config_rec%end_year(id_id)
  RETURN
END SUBROUTINE nl_get_end_year
SUBROUTINE nl_get_end_month ( id_id , end_month )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: end_month
  INTEGER id_id
  CHARACTER*80 emess
  end_month = model_config_rec%end_month(id_id)
  RETURN
END SUBROUTINE nl_get_end_month
SUBROUTINE nl_get_end_day ( id_id , end_day )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: end_day
  INTEGER id_id
  CHARACTER*80 emess
  end_day = model_config_rec%end_day(id_id)
  RETURN
END SUBROUTINE nl_get_end_day
SUBROUTINE nl_get_end_hour ( id_id , end_hour )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: end_hour
  INTEGER id_id
  CHARACTER*80 emess
  end_hour = model_config_rec%end_hour(id_id)
  RETURN
END SUBROUTINE nl_get_end_hour
SUBROUTINE nl_get_end_minute ( id_id , end_minute )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: end_minute
  INTEGER id_id
  CHARACTER*80 emess
  end_minute = model_config_rec%end_minute(id_id)
  RETURN
END SUBROUTINE nl_get_end_minute
SUBROUTINE nl_get_end_second ( id_id , end_second )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: end_second
  INTEGER id_id
  CHARACTER*80 emess
  end_second = model_config_rec%end_second(id_id)
  RETURN
END SUBROUTINE nl_get_end_second
SUBROUTINE nl_get_interval_seconds ( id_id , interval_seconds )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: interval_seconds
  INTEGER id_id
  CHARACTER*80 emess
  interval_seconds = model_config_rec%interval_seconds
  RETURN
END SUBROUTINE nl_get_interval_seconds
SUBROUTINE nl_get_input_from_file ( id_id , input_from_file )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: input_from_file
  INTEGER id_id
  CHARACTER*80 emess
  input_from_file = model_config_rec%input_from_file(id_id)
  RETURN
END SUBROUTINE nl_get_input_from_file
SUBROUTINE nl_get_fine_input_stream ( id_id , fine_input_stream )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: fine_input_stream
  INTEGER id_id
  CHARACTER*80 emess
  fine_input_stream = model_config_rec%fine_input_stream(id_id)
  RETURN
END SUBROUTINE nl_get_fine_input_stream
SUBROUTINE nl_get_history_interval ( id_id , history_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: history_interval
  INTEGER id_id
  CHARACTER*80 emess
  history_interval = model_config_rec%history_interval(id_id)
  RETURN
END SUBROUTINE nl_get_history_interval
SUBROUTINE nl_get_frames_per_outfile ( id_id , frames_per_outfile )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_outfile
  INTEGER id_id
  CHARACTER*80 emess
  frames_per_outfile = model_config_rec%frames_per_outfile(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_outfile
SUBROUTINE nl_get_frames_per_auxhist1 ( id_id , frames_per_auxhist1 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxhist1
  INTEGER id_id
  CHARACTER*80 emess
  frames_per_auxhist1 = model_config_rec%frames_per_auxhist1(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist1
SUBROUTINE nl_get_frames_per_auxhist2 ( id_id , frames_per_auxhist2 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxhist2
  INTEGER id_id
  CHARACTER*80 emess
  frames_per_auxhist2 = model_config_rec%frames_per_auxhist2(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist2
SUBROUTINE nl_get_frames_per_auxhist3 ( id_id , frames_per_auxhist3 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxhist3
  INTEGER id_id
  CHARACTER*80 emess
  frames_per_auxhist3 = model_config_rec%frames_per_auxhist3(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist3
SUBROUTINE nl_get_frames_per_auxhist4 ( id_id , frames_per_auxhist4 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxhist4
  INTEGER id_id
  CHARACTER*80 emess
  frames_per_auxhist4 = model_config_rec%frames_per_auxhist4(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist4
SUBROUTINE nl_get_frames_per_auxhist5 ( id_id , frames_per_auxhist5 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxhist5
  INTEGER id_id
  CHARACTER*80 emess
  frames_per_auxhist5 = model_config_rec%frames_per_auxhist5(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist5
SUBROUTINE nl_get_frames_per_auxhist6 ( id_id , frames_per_auxhist6 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxhist6
  INTEGER id_id
  CHARACTER*80 emess
  frames_per_auxhist6 = model_config_rec%frames_per_auxhist6(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist6
SUBROUTINE nl_get_frames_per_auxhist7 ( id_id , frames_per_auxhist7 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxhist7
  INTEGER id_id
  CHARACTER*80 emess
  frames_per_auxhist7 = model_config_rec%frames_per_auxhist7(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist7
SUBROUTINE nl_get_frames_per_auxhist8 ( id_id , frames_per_auxhist8 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxhist8
  INTEGER id_id
  CHARACTER*80 emess
  frames_per_auxhist8 = model_config_rec%frames_per_auxhist8(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist8
SUBROUTINE nl_get_frames_per_auxhist9 ( id_id , frames_per_auxhist9 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxhist9
  INTEGER id_id
  CHARACTER*80 emess
  frames_per_auxhist9 = model_config_rec%frames_per_auxhist9(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist9
SUBROUTINE nl_get_frames_per_auxhist10 ( id_id , frames_per_auxhist10 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxhist10
  INTEGER id_id
  CHARACTER*80 emess
  frames_per_auxhist10 = model_config_rec%frames_per_auxhist10(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist10
SUBROUTINE nl_get_frames_per_auxhist11 ( id_id , frames_per_auxhist11 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxhist11
  INTEGER id_id
  CHARACTER*80 emess
  frames_per_auxhist11 = model_config_rec%frames_per_auxhist11(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist11
SUBROUTINE nl_get_restart ( id_id , restart )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: restart
  INTEGER id_id
  CHARACTER*80 emess
  restart = model_config_rec%restart
  RETURN
END SUBROUTINE nl_get_restart
SUBROUTINE nl_get_restart_interval ( id_id , restart_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: restart_interval
  INTEGER id_id
  CHARACTER*80 emess
  restart_interval = model_config_rec%restart_interval
  RETURN
END SUBROUTINE nl_get_restart_interval
SUBROUTINE nl_get_io_form_input ( id_id , io_form_input )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_input
  INTEGER id_id
  CHARACTER*80 emess
  io_form_input = model_config_rec%io_form_input
  RETURN
END SUBROUTINE nl_get_io_form_input
SUBROUTINE nl_get_io_form_history ( id_id , io_form_history )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_history
  INTEGER id_id
  CHARACTER*80 emess
  io_form_history = model_config_rec%io_form_history
  RETURN
END SUBROUTINE nl_get_io_form_history
SUBROUTINE nl_get_io_form_restart ( id_id , io_form_restart )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_restart
  INTEGER id_id
  CHARACTER*80 emess
  io_form_restart = model_config_rec%io_form_restart
  RETURN
END SUBROUTINE nl_get_io_form_restart
SUBROUTINE nl_get_io_form_boundary ( id_id , io_form_boundary )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_boundary
  INTEGER id_id
  CHARACTER*80 emess
  io_form_boundary = model_config_rec%io_form_boundary
  RETURN
END SUBROUTINE nl_get_io_form_boundary
SUBROUTINE nl_get_debug_level ( id_id , debug_level )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: debug_level
  INTEGER id_id
  CHARACTER*80 emess
  debug_level = model_config_rec%debug_level
  RETURN
END SUBROUTINE nl_get_debug_level
SUBROUTINE nl_get_self_test_domain ( id_id , self_test_domain )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: self_test_domain
  INTEGER id_id
  CHARACTER*80 emess
  self_test_domain = model_config_rec%self_test_domain
  RETURN
END SUBROUTINE nl_get_self_test_domain
SUBROUTINE nl_get_history_outname ( id_id , history_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: history_outname
  INTEGER id_id
  CHARACTER*80 emess
  history_outname = trim(model_config_rec%history_outname)
  RETURN
END SUBROUTINE nl_get_history_outname
SUBROUTINE nl_get_auxhist1_outname ( id_id , auxhist1_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist1_outname
  INTEGER id_id
  CHARACTER*80 emess
  auxhist1_outname = trim(model_config_rec%auxhist1_outname)
  RETURN
END SUBROUTINE nl_get_auxhist1_outname
SUBROUTINE nl_get_auxhist2_outname ( id_id , auxhist2_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist2_outname
  INTEGER id_id
  CHARACTER*80 emess
  auxhist2_outname = trim(model_config_rec%auxhist2_outname)
  RETURN
END SUBROUTINE nl_get_auxhist2_outname
SUBROUTINE nl_get_auxhist3_outname ( id_id , auxhist3_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist3_outname
  INTEGER id_id
  CHARACTER*80 emess
  auxhist3_outname = trim(model_config_rec%auxhist3_outname)
  RETURN
END SUBROUTINE nl_get_auxhist3_outname
SUBROUTINE nl_get_auxhist4_outname ( id_id , auxhist4_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist4_outname
  INTEGER id_id
  CHARACTER*80 emess
  auxhist4_outname = trim(model_config_rec%auxhist4_outname)
  RETURN
END SUBROUTINE nl_get_auxhist4_outname
SUBROUTINE nl_get_auxhist5_outname ( id_id , auxhist5_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist5_outname
  INTEGER id_id
  CHARACTER*80 emess
  auxhist5_outname = trim(model_config_rec%auxhist5_outname)
  RETURN
END SUBROUTINE nl_get_auxhist5_outname
SUBROUTINE nl_get_auxhist6_outname ( id_id , auxhist6_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist6_outname
  INTEGER id_id
  CHARACTER*80 emess
  auxhist6_outname = trim(model_config_rec%auxhist6_outname)
  RETURN
END SUBROUTINE nl_get_auxhist6_outname
SUBROUTINE nl_get_auxhist7_outname ( id_id , auxhist7_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist7_outname
  INTEGER id_id
  CHARACTER*80 emess
  auxhist7_outname = trim(model_config_rec%auxhist7_outname)
  RETURN
END SUBROUTINE nl_get_auxhist7_outname
SUBROUTINE nl_get_auxhist8_outname ( id_id , auxhist8_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist8_outname
  INTEGER id_id
  CHARACTER*80 emess
  auxhist8_outname = trim(model_config_rec%auxhist8_outname)
  RETURN
END SUBROUTINE nl_get_auxhist8_outname
SUBROUTINE nl_get_auxhist9_outname ( id_id , auxhist9_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist9_outname
  INTEGER id_id
  CHARACTER*80 emess
  auxhist9_outname = trim(model_config_rec%auxhist9_outname)
  RETURN
END SUBROUTINE nl_get_auxhist9_outname
SUBROUTINE nl_get_auxhist10_outname ( id_id , auxhist10_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist10_outname
  INTEGER id_id
  CHARACTER*80 emess
  auxhist10_outname = trim(model_config_rec%auxhist10_outname)
  RETURN
END SUBROUTINE nl_get_auxhist10_outname
SUBROUTINE nl_get_auxhist11_outname ( id_id , auxhist11_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist11_outname
  INTEGER id_id
  CHARACTER*80 emess
  auxhist11_outname = trim(model_config_rec%auxhist11_outname)
  RETURN
END SUBROUTINE nl_get_auxhist11_outname
SUBROUTINE nl_get_history_inname ( id_id , history_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: history_inname
  INTEGER id_id
  CHARACTER*80 emess
  history_inname = trim(model_config_rec%history_inname)
  RETURN
END SUBROUTINE nl_get_history_inname
SUBROUTINE nl_get_auxhist1_inname ( id_id , auxhist1_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist1_inname
  INTEGER id_id
  CHARACTER*80 emess
  auxhist1_inname = trim(model_config_rec%auxhist1_inname)
  RETURN
END SUBROUTINE nl_get_auxhist1_inname
SUBROUTINE nl_get_auxhist2_inname ( id_id , auxhist2_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist2_inname
  INTEGER id_id
  CHARACTER*80 emess
  auxhist2_inname = trim(model_config_rec%auxhist2_inname)
  RETURN
END SUBROUTINE nl_get_auxhist2_inname
SUBROUTINE nl_get_auxhist3_inname ( id_id , auxhist3_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist3_inname
  INTEGER id_id
  CHARACTER*80 emess
  auxhist3_inname = trim(model_config_rec%auxhist3_inname)
  RETURN
END SUBROUTINE nl_get_auxhist3_inname
SUBROUTINE nl_get_auxhist4_inname ( id_id , auxhist4_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist4_inname
  INTEGER id_id
  CHARACTER*80 emess
  auxhist4_inname = trim(model_config_rec%auxhist4_inname)
  RETURN
END SUBROUTINE nl_get_auxhist4_inname
SUBROUTINE nl_get_auxhist5_inname ( id_id , auxhist5_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist5_inname
  INTEGER id_id
  CHARACTER*80 emess
  auxhist5_inname = trim(model_config_rec%auxhist5_inname)
  RETURN
END SUBROUTINE nl_get_auxhist5_inname
SUBROUTINE nl_get_auxhist6_inname ( id_id , auxhist6_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist6_inname
  INTEGER id_id
  CHARACTER*80 emess
  auxhist6_inname = trim(model_config_rec%auxhist6_inname)
  RETURN
END SUBROUTINE nl_get_auxhist6_inname
SUBROUTINE nl_get_auxhist7_inname ( id_id , auxhist7_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist7_inname
  INTEGER id_id
  CHARACTER*80 emess
  auxhist7_inname = trim(model_config_rec%auxhist7_inname)
  RETURN
END SUBROUTINE nl_get_auxhist7_inname
SUBROUTINE nl_get_auxhist8_inname ( id_id , auxhist8_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist8_inname
  INTEGER id_id
  CHARACTER*80 emess
  auxhist8_inname = trim(model_config_rec%auxhist8_inname)
  RETURN
END SUBROUTINE nl_get_auxhist8_inname
SUBROUTINE nl_get_auxhist9_inname ( id_id , auxhist9_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist9_inname
  INTEGER id_id
  CHARACTER*80 emess
  auxhist9_inname = trim(model_config_rec%auxhist9_inname)
  RETURN
END SUBROUTINE nl_get_auxhist9_inname
SUBROUTINE nl_get_auxhist10_inname ( id_id , auxhist10_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist10_inname
  INTEGER id_id
  CHARACTER*80 emess
  auxhist10_inname = trim(model_config_rec%auxhist10_inname)
  RETURN
END SUBROUTINE nl_get_auxhist10_inname
SUBROUTINE nl_get_auxhist11_inname ( id_id , auxhist11_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist11_inname
  INTEGER id_id
  CHARACTER*80 emess
  auxhist11_inname = trim(model_config_rec%auxhist11_inname)
  RETURN
END SUBROUTINE nl_get_auxhist11_inname
SUBROUTINE nl_get_auxinput1_outname ( id_id , auxinput1_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput1_outname
  INTEGER id_id
  CHARACTER*80 emess
  auxinput1_outname = trim(model_config_rec%auxinput1_outname)
  RETURN
END SUBROUTINE nl_get_auxinput1_outname
SUBROUTINE nl_get_auxinput2_outname ( id_id , auxinput2_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput2_outname
  INTEGER id_id
  CHARACTER*80 emess
  auxinput2_outname = trim(model_config_rec%auxinput2_outname)
  RETURN
END SUBROUTINE nl_get_auxinput2_outname
SUBROUTINE nl_get_auxinput3_outname ( id_id , auxinput3_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput3_outname
  INTEGER id_id
  CHARACTER*80 emess
  auxinput3_outname = trim(model_config_rec%auxinput3_outname)
  RETURN
END SUBROUTINE nl_get_auxinput3_outname
SUBROUTINE nl_get_auxinput4_outname ( id_id , auxinput4_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput4_outname
  INTEGER id_id
  CHARACTER*80 emess
  auxinput4_outname = trim(model_config_rec%auxinput4_outname)
  RETURN
END SUBROUTINE nl_get_auxinput4_outname
SUBROUTINE nl_get_auxinput5_outname ( id_id , auxinput5_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput5_outname
  INTEGER id_id
  CHARACTER*80 emess
  auxinput5_outname = trim(model_config_rec%auxinput5_outname)
  RETURN
END SUBROUTINE nl_get_auxinput5_outname
SUBROUTINE nl_get_auxinput6_outname ( id_id , auxinput6_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput6_outname
  INTEGER id_id
  CHARACTER*80 emess
  auxinput6_outname = trim(model_config_rec%auxinput6_outname)
  RETURN
END SUBROUTINE nl_get_auxinput6_outname
SUBROUTINE nl_get_auxinput7_outname ( id_id , auxinput7_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput7_outname
  INTEGER id_id
  CHARACTER*80 emess
  auxinput7_outname = trim(model_config_rec%auxinput7_outname)
  RETURN
END SUBROUTINE nl_get_auxinput7_outname
SUBROUTINE nl_get_auxinput8_outname ( id_id , auxinput8_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput8_outname
  INTEGER id_id
  CHARACTER*80 emess
  auxinput8_outname = trim(model_config_rec%auxinput8_outname)
  RETURN
END SUBROUTINE nl_get_auxinput8_outname
SUBROUTINE nl_get_auxinput9_outname ( id_id , auxinput9_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput9_outname
  INTEGER id_id
  CHARACTER*80 emess
  auxinput9_outname = trim(model_config_rec%auxinput9_outname)
  RETURN
END SUBROUTINE nl_get_auxinput9_outname
SUBROUTINE nl_get_auxinput10_outname ( id_id , auxinput10_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput10_outname
  INTEGER id_id
  CHARACTER*80 emess
  auxinput10_outname = trim(model_config_rec%auxinput10_outname)
  RETURN
END SUBROUTINE nl_get_auxinput10_outname
SUBROUTINE nl_get_auxinput11_outname ( id_id , auxinput11_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput11_outname
  INTEGER id_id
  CHARACTER*80 emess
  auxinput11_outname = trim(model_config_rec%auxinput11_outname)
  RETURN
END SUBROUTINE nl_get_auxinput11_outname
SUBROUTINE nl_get_auxinput1_inname ( id_id , auxinput1_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput1_inname
  INTEGER id_id
  CHARACTER*80 emess
  auxinput1_inname = trim(model_config_rec%auxinput1_inname)
  RETURN
END SUBROUTINE nl_get_auxinput1_inname
SUBROUTINE nl_get_auxinput2_inname ( id_id , auxinput2_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput2_inname
  INTEGER id_id
  CHARACTER*80 emess
  auxinput2_inname = trim(model_config_rec%auxinput2_inname)
  RETURN
END SUBROUTINE nl_get_auxinput2_inname
SUBROUTINE nl_get_auxinput3_inname ( id_id , auxinput3_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput3_inname
  INTEGER id_id
  CHARACTER*80 emess
  auxinput3_inname = trim(model_config_rec%auxinput3_inname)
  RETURN
END SUBROUTINE nl_get_auxinput3_inname
SUBROUTINE nl_get_auxinput4_inname ( id_id , auxinput4_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput4_inname
  INTEGER id_id
  CHARACTER*80 emess
  auxinput4_inname = trim(model_config_rec%auxinput4_inname)
  RETURN
END SUBROUTINE nl_get_auxinput4_inname
SUBROUTINE nl_get_auxinput5_inname ( id_id , auxinput5_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput5_inname
  INTEGER id_id
  CHARACTER*80 emess
  auxinput5_inname = trim(model_config_rec%auxinput5_inname)
  RETURN
END SUBROUTINE nl_get_auxinput5_inname
SUBROUTINE nl_get_auxinput6_inname ( id_id , auxinput6_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput6_inname
  INTEGER id_id
  CHARACTER*80 emess
  auxinput6_inname = trim(model_config_rec%auxinput6_inname)
  RETURN
END SUBROUTINE nl_get_auxinput6_inname
SUBROUTINE nl_get_auxinput7_inname ( id_id , auxinput7_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput7_inname
  INTEGER id_id
  CHARACTER*80 emess
  auxinput7_inname = trim(model_config_rec%auxinput7_inname)
  RETURN
END SUBROUTINE nl_get_auxinput7_inname
SUBROUTINE nl_get_auxinput8_inname ( id_id , auxinput8_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput8_inname
  INTEGER id_id
  CHARACTER*80 emess
  auxinput8_inname = trim(model_config_rec%auxinput8_inname)
  RETURN
END SUBROUTINE nl_get_auxinput8_inname
SUBROUTINE nl_get_sgfdda_inname ( id_id , sgfdda_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: sgfdda_inname
  INTEGER id_id
  CHARACTER*80 emess
  sgfdda_inname = trim(model_config_rec%sgfdda_inname)
  RETURN
END SUBROUTINE nl_get_sgfdda_inname
SUBROUTINE nl_get_gfdda_inname ( id_id , gfdda_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: gfdda_inname
  INTEGER id_id
  CHARACTER*80 emess
  gfdda_inname = trim(model_config_rec%gfdda_inname)
  RETURN
END SUBROUTINE nl_get_gfdda_inname
SUBROUTINE nl_get_auxinput11_inname ( id_id , auxinput11_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput11_inname
  INTEGER id_id
  CHARACTER*80 emess
  auxinput11_inname = trim(model_config_rec%auxinput11_inname)
  RETURN
END SUBROUTINE nl_get_auxinput11_inname
SUBROUTINE nl_get_history_interval_mo ( id_id , history_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: history_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  history_interval_mo = model_config_rec%history_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_history_interval_mo
SUBROUTINE nl_get_history_interval_d ( id_id , history_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: history_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  history_interval_d = model_config_rec%history_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_history_interval_d
SUBROUTINE nl_get_history_interval_h ( id_id , history_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: history_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  history_interval_h = model_config_rec%history_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_history_interval_h
SUBROUTINE nl_get_history_interval_m ( id_id , history_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: history_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  history_interval_m = model_config_rec%history_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_history_interval_m
SUBROUTINE nl_get_history_interval_s ( id_id , history_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: history_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  history_interval_s = model_config_rec%history_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_history_interval_s
SUBROUTINE nl_get_inputout_interval_mo ( id_id , inputout_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: inputout_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  inputout_interval_mo = model_config_rec%inputout_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_interval_mo
SUBROUTINE nl_get_inputout_interval_d ( id_id , inputout_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: inputout_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  inputout_interval_d = model_config_rec%inputout_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_interval_d
SUBROUTINE nl_get_inputout_interval_h ( id_id , inputout_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: inputout_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  inputout_interval_h = model_config_rec%inputout_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_interval_h
SUBROUTINE nl_get_inputout_interval_m ( id_id , inputout_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: inputout_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  inputout_interval_m = model_config_rec%inputout_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_interval_m
SUBROUTINE nl_get_inputout_interval_s ( id_id , inputout_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: inputout_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  inputout_interval_s = model_config_rec%inputout_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_interval_s
SUBROUTINE nl_get_inputout_interval ( id_id , inputout_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: inputout_interval
  INTEGER id_id
  CHARACTER*80 emess
  inputout_interval = model_config_rec%inputout_interval(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_interval
SUBROUTINE nl_get_auxhist1_interval_mo ( id_id , auxhist1_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist1_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxhist1_interval_mo = model_config_rec%auxhist1_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_interval_mo
SUBROUTINE nl_get_auxhist1_interval_d ( id_id , auxhist1_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist1_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  auxhist1_interval_d = model_config_rec%auxhist1_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_interval_d
SUBROUTINE nl_get_auxhist1_interval_h ( id_id , auxhist1_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist1_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  auxhist1_interval_h = model_config_rec%auxhist1_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_interval_h
SUBROUTINE nl_get_auxhist1_interval_m ( id_id , auxhist1_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist1_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  auxhist1_interval_m = model_config_rec%auxhist1_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_interval_m
SUBROUTINE nl_get_auxhist1_interval_s ( id_id , auxhist1_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist1_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  auxhist1_interval_s = model_config_rec%auxhist1_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_interval_s
SUBROUTINE nl_get_auxhist1_interval ( id_id , auxhist1_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist1_interval
  INTEGER id_id
  CHARACTER*80 emess
  auxhist1_interval = model_config_rec%auxhist1_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_interval
SUBROUTINE nl_get_auxhist2_interval_mo ( id_id , auxhist2_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist2_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxhist2_interval_mo = model_config_rec%auxhist2_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_interval_mo
SUBROUTINE nl_get_auxhist2_interval_d ( id_id , auxhist2_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist2_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  auxhist2_interval_d = model_config_rec%auxhist2_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_interval_d
SUBROUTINE nl_get_auxhist2_interval_h ( id_id , auxhist2_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist2_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  auxhist2_interval_h = model_config_rec%auxhist2_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_interval_h
SUBROUTINE nl_get_auxhist2_interval_m ( id_id , auxhist2_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist2_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  auxhist2_interval_m = model_config_rec%auxhist2_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_interval_m
SUBROUTINE nl_get_auxhist2_interval_s ( id_id , auxhist2_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist2_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  auxhist2_interval_s = model_config_rec%auxhist2_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_interval_s
SUBROUTINE nl_get_auxhist2_interval ( id_id , auxhist2_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist2_interval
  INTEGER id_id
  CHARACTER*80 emess
  auxhist2_interval = model_config_rec%auxhist2_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_interval
SUBROUTINE nl_get_auxhist3_interval_mo ( id_id , auxhist3_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist3_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxhist3_interval_mo = model_config_rec%auxhist3_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_interval_mo
SUBROUTINE nl_get_auxhist3_interval_d ( id_id , auxhist3_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist3_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  auxhist3_interval_d = model_config_rec%auxhist3_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_interval_d
SUBROUTINE nl_get_auxhist3_interval_h ( id_id , auxhist3_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist3_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  auxhist3_interval_h = model_config_rec%auxhist3_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_interval_h
SUBROUTINE nl_get_auxhist3_interval_m ( id_id , auxhist3_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist3_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  auxhist3_interval_m = model_config_rec%auxhist3_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_interval_m
SUBROUTINE nl_get_auxhist3_interval_s ( id_id , auxhist3_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist3_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  auxhist3_interval_s = model_config_rec%auxhist3_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_interval_s
SUBROUTINE nl_get_auxhist3_interval ( id_id , auxhist3_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist3_interval
  INTEGER id_id
  CHARACTER*80 emess
  auxhist3_interval = model_config_rec%auxhist3_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_interval
SUBROUTINE nl_get_auxhist4_interval_mo ( id_id , auxhist4_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist4_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxhist4_interval_mo = model_config_rec%auxhist4_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_interval_mo
SUBROUTINE nl_get_auxhist4_interval_d ( id_id , auxhist4_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist4_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  auxhist4_interval_d = model_config_rec%auxhist4_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_interval_d
SUBROUTINE nl_get_auxhist4_interval_h ( id_id , auxhist4_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist4_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  auxhist4_interval_h = model_config_rec%auxhist4_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_interval_h
SUBROUTINE nl_get_auxhist4_interval_m ( id_id , auxhist4_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist4_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  auxhist4_interval_m = model_config_rec%auxhist4_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_interval_m
SUBROUTINE nl_get_auxhist4_interval_s ( id_id , auxhist4_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist4_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  auxhist4_interval_s = model_config_rec%auxhist4_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_interval_s
SUBROUTINE nl_get_auxhist4_interval ( id_id , auxhist4_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist4_interval
  INTEGER id_id
  CHARACTER*80 emess
  auxhist4_interval = model_config_rec%auxhist4_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_interval
SUBROUTINE nl_get_auxhist5_interval_mo ( id_id , auxhist5_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist5_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxhist5_interval_mo = model_config_rec%auxhist5_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_interval_mo
SUBROUTINE nl_get_auxhist5_interval_d ( id_id , auxhist5_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist5_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  auxhist5_interval_d = model_config_rec%auxhist5_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_interval_d
SUBROUTINE nl_get_auxhist5_interval_h ( id_id , auxhist5_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist5_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  auxhist5_interval_h = model_config_rec%auxhist5_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_interval_h
SUBROUTINE nl_get_auxhist5_interval_m ( id_id , auxhist5_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist5_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  auxhist5_interval_m = model_config_rec%auxhist5_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_interval_m
SUBROUTINE nl_get_auxhist5_interval_s ( id_id , auxhist5_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist5_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  auxhist5_interval_s = model_config_rec%auxhist5_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_interval_s
SUBROUTINE nl_get_auxhist5_interval ( id_id , auxhist5_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist5_interval
  INTEGER id_id
  CHARACTER*80 emess
  auxhist5_interval = model_config_rec%auxhist5_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_interval
SUBROUTINE nl_get_auxhist6_interval_mo ( id_id , auxhist6_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist6_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxhist6_interval_mo = model_config_rec%auxhist6_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_interval_mo
SUBROUTINE nl_get_auxhist6_interval_d ( id_id , auxhist6_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist6_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  auxhist6_interval_d = model_config_rec%auxhist6_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_interval_d
SUBROUTINE nl_get_auxhist6_interval_h ( id_id , auxhist6_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist6_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  auxhist6_interval_h = model_config_rec%auxhist6_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_interval_h
SUBROUTINE nl_get_auxhist6_interval_m ( id_id , auxhist6_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist6_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  auxhist6_interval_m = model_config_rec%auxhist6_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_interval_m
SUBROUTINE nl_get_auxhist6_interval_s ( id_id , auxhist6_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist6_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  auxhist6_interval_s = model_config_rec%auxhist6_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_interval_s
SUBROUTINE nl_get_auxhist6_interval ( id_id , auxhist6_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist6_interval
  INTEGER id_id
  CHARACTER*80 emess
  auxhist6_interval = model_config_rec%auxhist6_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_interval
SUBROUTINE nl_get_auxhist7_interval_mo ( id_id , auxhist7_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist7_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxhist7_interval_mo = model_config_rec%auxhist7_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_interval_mo
SUBROUTINE nl_get_auxhist7_interval_d ( id_id , auxhist7_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist7_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  auxhist7_interval_d = model_config_rec%auxhist7_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_interval_d
SUBROUTINE nl_get_auxhist7_interval_h ( id_id , auxhist7_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist7_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  auxhist7_interval_h = model_config_rec%auxhist7_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_interval_h
SUBROUTINE nl_get_auxhist7_interval_m ( id_id , auxhist7_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist7_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  auxhist7_interval_m = model_config_rec%auxhist7_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_interval_m
SUBROUTINE nl_get_auxhist7_interval_s ( id_id , auxhist7_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist7_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  auxhist7_interval_s = model_config_rec%auxhist7_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_interval_s
SUBROUTINE nl_get_auxhist7_interval ( id_id , auxhist7_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist7_interval
  INTEGER id_id
  CHARACTER*80 emess
  auxhist7_interval = model_config_rec%auxhist7_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_interval
SUBROUTINE nl_get_auxhist8_interval_mo ( id_id , auxhist8_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist8_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxhist8_interval_mo = model_config_rec%auxhist8_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_interval_mo
SUBROUTINE nl_get_auxhist8_interval_d ( id_id , auxhist8_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist8_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  auxhist8_interval_d = model_config_rec%auxhist8_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_interval_d
SUBROUTINE nl_get_auxhist8_interval_h ( id_id , auxhist8_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist8_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  auxhist8_interval_h = model_config_rec%auxhist8_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_interval_h
SUBROUTINE nl_get_auxhist8_interval_m ( id_id , auxhist8_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist8_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  auxhist8_interval_m = model_config_rec%auxhist8_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_interval_m
SUBROUTINE nl_get_auxhist8_interval_s ( id_id , auxhist8_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist8_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  auxhist8_interval_s = model_config_rec%auxhist8_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_interval_s
SUBROUTINE nl_get_auxhist8_interval ( id_id , auxhist8_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist8_interval
  INTEGER id_id
  CHARACTER*80 emess
  auxhist8_interval = model_config_rec%auxhist8_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_interval
SUBROUTINE nl_get_auxhist9_interval_mo ( id_id , auxhist9_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist9_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxhist9_interval_mo = model_config_rec%auxhist9_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_interval_mo
SUBROUTINE nl_get_auxhist9_interval_d ( id_id , auxhist9_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist9_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  auxhist9_interval_d = model_config_rec%auxhist9_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_interval_d
SUBROUTINE nl_get_auxhist9_interval_h ( id_id , auxhist9_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist9_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  auxhist9_interval_h = model_config_rec%auxhist9_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_interval_h
SUBROUTINE nl_get_auxhist9_interval_m ( id_id , auxhist9_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist9_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  auxhist9_interval_m = model_config_rec%auxhist9_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_interval_m
SUBROUTINE nl_get_auxhist9_interval_s ( id_id , auxhist9_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist9_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  auxhist9_interval_s = model_config_rec%auxhist9_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_interval_s
SUBROUTINE nl_get_auxhist9_interval ( id_id , auxhist9_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist9_interval
  INTEGER id_id
  CHARACTER*80 emess
  auxhist9_interval = model_config_rec%auxhist9_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_interval
SUBROUTINE nl_get_auxhist10_interval_mo ( id_id , auxhist10_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist10_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxhist10_interval_mo = model_config_rec%auxhist10_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_interval_mo
SUBROUTINE nl_get_auxhist10_interval_d ( id_id , auxhist10_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist10_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  auxhist10_interval_d = model_config_rec%auxhist10_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_interval_d
SUBROUTINE nl_get_auxhist10_interval_h ( id_id , auxhist10_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist10_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  auxhist10_interval_h = model_config_rec%auxhist10_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_interval_h
SUBROUTINE nl_get_auxhist10_interval_m ( id_id , auxhist10_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist10_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  auxhist10_interval_m = model_config_rec%auxhist10_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_interval_m
SUBROUTINE nl_get_auxhist10_interval_s ( id_id , auxhist10_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist10_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  auxhist10_interval_s = model_config_rec%auxhist10_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_interval_s
SUBROUTINE nl_get_auxhist10_interval ( id_id , auxhist10_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist10_interval
  INTEGER id_id
  CHARACTER*80 emess
  auxhist10_interval = model_config_rec%auxhist10_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_interval
SUBROUTINE nl_get_auxhist11_interval_mo ( id_id , auxhist11_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist11_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxhist11_interval_mo = model_config_rec%auxhist11_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_interval_mo
SUBROUTINE nl_get_auxhist11_interval_d ( id_id , auxhist11_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist11_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  auxhist11_interval_d = model_config_rec%auxhist11_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_interval_d
SUBROUTINE nl_get_auxhist11_interval_h ( id_id , auxhist11_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist11_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  auxhist11_interval_h = model_config_rec%auxhist11_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_interval_h
SUBROUTINE nl_get_auxhist11_interval_m ( id_id , auxhist11_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist11_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  auxhist11_interval_m = model_config_rec%auxhist11_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_interval_m
SUBROUTINE nl_get_auxhist11_interval_s ( id_id , auxhist11_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist11_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  auxhist11_interval_s = model_config_rec%auxhist11_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_interval_s
SUBROUTINE nl_get_auxhist11_interval ( id_id , auxhist11_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist11_interval
  INTEGER id_id
  CHARACTER*80 emess
  auxhist11_interval = model_config_rec%auxhist11_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_interval
SUBROUTINE nl_get_auxinput1_interval_mo ( id_id , auxinput1_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput1_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxinput1_interval_mo = model_config_rec%auxinput1_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_interval_mo
SUBROUTINE nl_get_auxinput1_interval_d ( id_id , auxinput1_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput1_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  auxinput1_interval_d = model_config_rec%auxinput1_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_interval_d
SUBROUTINE nl_get_auxinput1_interval_h ( id_id , auxinput1_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput1_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  auxinput1_interval_h = model_config_rec%auxinput1_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_interval_h
SUBROUTINE nl_get_auxinput1_interval_m ( id_id , auxinput1_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput1_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  auxinput1_interval_m = model_config_rec%auxinput1_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_interval_m
SUBROUTINE nl_get_auxinput1_interval_s ( id_id , auxinput1_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput1_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  auxinput1_interval_s = model_config_rec%auxinput1_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_interval_s
SUBROUTINE nl_get_auxinput1_interval ( id_id , auxinput1_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput1_interval
  INTEGER id_id
  CHARACTER*80 emess
  auxinput1_interval = model_config_rec%auxinput1_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_interval
SUBROUTINE nl_get_auxinput2_interval_mo ( id_id , auxinput2_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput2_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxinput2_interval_mo = model_config_rec%auxinput2_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_interval_mo
SUBROUTINE nl_get_auxinput2_interval_d ( id_id , auxinput2_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput2_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  auxinput2_interval_d = model_config_rec%auxinput2_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_interval_d
SUBROUTINE nl_get_auxinput2_interval_h ( id_id , auxinput2_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput2_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  auxinput2_interval_h = model_config_rec%auxinput2_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_interval_h
SUBROUTINE nl_get_auxinput2_interval_m ( id_id , auxinput2_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput2_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  auxinput2_interval_m = model_config_rec%auxinput2_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_interval_m
SUBROUTINE nl_get_auxinput2_interval_s ( id_id , auxinput2_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput2_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  auxinput2_interval_s = model_config_rec%auxinput2_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_interval_s
SUBROUTINE nl_get_auxinput2_interval ( id_id , auxinput2_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput2_interval
  INTEGER id_id
  CHARACTER*80 emess
  auxinput2_interval = model_config_rec%auxinput2_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_interval
SUBROUTINE nl_get_auxinput3_interval_mo ( id_id , auxinput3_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput3_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxinput3_interval_mo = model_config_rec%auxinput3_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_interval_mo
SUBROUTINE nl_get_auxinput3_interval_d ( id_id , auxinput3_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput3_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  auxinput3_interval_d = model_config_rec%auxinput3_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_interval_d
SUBROUTINE nl_get_auxinput3_interval_h ( id_id , auxinput3_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput3_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  auxinput3_interval_h = model_config_rec%auxinput3_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_interval_h
SUBROUTINE nl_get_auxinput3_interval_m ( id_id , auxinput3_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput3_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  auxinput3_interval_m = model_config_rec%auxinput3_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_interval_m
SUBROUTINE nl_get_auxinput3_interval_s ( id_id , auxinput3_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput3_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  auxinput3_interval_s = model_config_rec%auxinput3_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_interval_s
SUBROUTINE nl_get_auxinput3_interval ( id_id , auxinput3_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput3_interval
  INTEGER id_id
  CHARACTER*80 emess
  auxinput3_interval = model_config_rec%auxinput3_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_interval
SUBROUTINE nl_get_auxinput4_interval_mo ( id_id , auxinput4_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput4_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxinput4_interval_mo = model_config_rec%auxinput4_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_interval_mo
SUBROUTINE nl_get_auxinput4_interval_d ( id_id , auxinput4_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput4_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  auxinput4_interval_d = model_config_rec%auxinput4_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_interval_d
SUBROUTINE nl_get_auxinput4_interval_h ( id_id , auxinput4_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput4_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  auxinput4_interval_h = model_config_rec%auxinput4_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_interval_h
SUBROUTINE nl_get_auxinput4_interval_m ( id_id , auxinput4_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput4_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  auxinput4_interval_m = model_config_rec%auxinput4_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_interval_m
SUBROUTINE nl_get_auxinput4_interval_s ( id_id , auxinput4_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput4_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  auxinput4_interval_s = model_config_rec%auxinput4_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_interval_s
SUBROUTINE nl_get_auxinput4_interval ( id_id , auxinput4_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput4_interval
  INTEGER id_id
  CHARACTER*80 emess
  auxinput4_interval = model_config_rec%auxinput4_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_interval
SUBROUTINE nl_get_auxinput5_interval_mo ( id_id , auxinput5_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput5_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxinput5_interval_mo = model_config_rec%auxinput5_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_interval_mo
SUBROUTINE nl_get_auxinput5_interval_d ( id_id , auxinput5_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput5_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  auxinput5_interval_d = model_config_rec%auxinput5_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_interval_d
SUBROUTINE nl_get_auxinput5_interval_h ( id_id , auxinput5_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput5_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  auxinput5_interval_h = model_config_rec%auxinput5_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_interval_h
SUBROUTINE nl_get_auxinput5_interval_m ( id_id , auxinput5_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput5_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  auxinput5_interval_m = model_config_rec%auxinput5_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_interval_m
SUBROUTINE nl_get_auxinput5_interval_s ( id_id , auxinput5_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput5_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  auxinput5_interval_s = model_config_rec%auxinput5_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_interval_s
SUBROUTINE nl_get_auxinput5_interval ( id_id , auxinput5_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput5_interval
  INTEGER id_id
  CHARACTER*80 emess
  auxinput5_interval = model_config_rec%auxinput5_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_interval
SUBROUTINE nl_get_auxinput6_interval_mo ( id_id , auxinput6_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput6_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxinput6_interval_mo = model_config_rec%auxinput6_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_interval_mo
SUBROUTINE nl_get_auxinput6_interval_d ( id_id , auxinput6_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput6_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  auxinput6_interval_d = model_config_rec%auxinput6_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_interval_d
SUBROUTINE nl_get_auxinput6_interval_h ( id_id , auxinput6_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput6_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  auxinput6_interval_h = model_config_rec%auxinput6_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_interval_h
SUBROUTINE nl_get_auxinput6_interval_m ( id_id , auxinput6_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput6_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  auxinput6_interval_m = model_config_rec%auxinput6_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_interval_m
SUBROUTINE nl_get_auxinput6_interval_s ( id_id , auxinput6_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput6_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  auxinput6_interval_s = model_config_rec%auxinput6_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_interval_s
SUBROUTINE nl_get_auxinput6_interval ( id_id , auxinput6_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput6_interval
  INTEGER id_id
  CHARACTER*80 emess
  auxinput6_interval = model_config_rec%auxinput6_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_interval
SUBROUTINE nl_get_auxinput7_interval_mo ( id_id , auxinput7_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput7_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxinput7_interval_mo = model_config_rec%auxinput7_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_interval_mo
SUBROUTINE nl_get_auxinput7_interval_d ( id_id , auxinput7_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput7_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  auxinput7_interval_d = model_config_rec%auxinput7_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_interval_d
SUBROUTINE nl_get_auxinput7_interval_h ( id_id , auxinput7_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput7_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  auxinput7_interval_h = model_config_rec%auxinput7_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_interval_h
SUBROUTINE nl_get_auxinput7_interval_m ( id_id , auxinput7_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput7_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  auxinput7_interval_m = model_config_rec%auxinput7_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_interval_m
SUBROUTINE nl_get_auxinput7_interval_s ( id_id , auxinput7_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput7_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  auxinput7_interval_s = model_config_rec%auxinput7_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_interval_s
SUBROUTINE nl_get_auxinput7_interval ( id_id , auxinput7_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput7_interval
  INTEGER id_id
  CHARACTER*80 emess
  auxinput7_interval = model_config_rec%auxinput7_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_interval
SUBROUTINE nl_get_auxinput8_interval_mo ( id_id , auxinput8_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput8_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxinput8_interval_mo = model_config_rec%auxinput8_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_interval_mo
SUBROUTINE nl_get_auxinput8_interval_d ( id_id , auxinput8_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput8_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  auxinput8_interval_d = model_config_rec%auxinput8_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_interval_d
SUBROUTINE nl_get_auxinput8_interval_h ( id_id , auxinput8_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput8_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  auxinput8_interval_h = model_config_rec%auxinput8_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_interval_h
SUBROUTINE nl_get_auxinput8_interval_m ( id_id , auxinput8_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput8_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  auxinput8_interval_m = model_config_rec%auxinput8_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_interval_m
SUBROUTINE nl_get_auxinput8_interval_s ( id_id , auxinput8_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput8_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  auxinput8_interval_s = model_config_rec%auxinput8_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_interval_s
SUBROUTINE nl_get_auxinput8_interval ( id_id , auxinput8_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput8_interval
  INTEGER id_id
  CHARACTER*80 emess
  auxinput8_interval = model_config_rec%auxinput8_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_interval
SUBROUTINE nl_get_sgfdda_interval_mo ( id_id , sgfdda_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: sgfdda_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  sgfdda_interval_mo = model_config_rec%sgfdda_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_sgfdda_interval_mo
SUBROUTINE nl_get_sgfdda_interval_d ( id_id , sgfdda_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: sgfdda_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  sgfdda_interval_d = model_config_rec%sgfdda_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_sgfdda_interval_d
SUBROUTINE nl_get_sgfdda_interval_h ( id_id , sgfdda_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: sgfdda_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  sgfdda_interval_h = model_config_rec%sgfdda_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_sgfdda_interval_h
SUBROUTINE nl_get_sgfdda_interval_m ( id_id , sgfdda_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: sgfdda_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  sgfdda_interval_m = model_config_rec%sgfdda_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_sgfdda_interval_m
SUBROUTINE nl_get_sgfdda_interval_s ( id_id , sgfdda_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: sgfdda_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  sgfdda_interval_s = model_config_rec%sgfdda_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_sgfdda_interval_s
SUBROUTINE nl_get_sgfdda_interval ( id_id , sgfdda_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: sgfdda_interval
  INTEGER id_id
  CHARACTER*80 emess
  sgfdda_interval = model_config_rec%sgfdda_interval(id_id)
  RETURN
END SUBROUTINE nl_get_sgfdda_interval
SUBROUTINE nl_get_gfdda_interval_mo ( id_id , gfdda_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: gfdda_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  gfdda_interval_mo = model_config_rec%gfdda_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_gfdda_interval_mo
SUBROUTINE nl_get_gfdda_interval_d ( id_id , gfdda_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: gfdda_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  gfdda_interval_d = model_config_rec%gfdda_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_gfdda_interval_d
SUBROUTINE nl_get_gfdda_interval_h ( id_id , gfdda_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: gfdda_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  gfdda_interval_h = model_config_rec%gfdda_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_gfdda_interval_h
SUBROUTINE nl_get_gfdda_interval_m ( id_id , gfdda_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: gfdda_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  gfdda_interval_m = model_config_rec%gfdda_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_gfdda_interval_m
SUBROUTINE nl_get_gfdda_interval_s ( id_id , gfdda_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: gfdda_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  gfdda_interval_s = model_config_rec%gfdda_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_gfdda_interval_s
SUBROUTINE nl_get_gfdda_interval ( id_id , gfdda_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: gfdda_interval
  INTEGER id_id
  CHARACTER*80 emess
  gfdda_interval = model_config_rec%gfdda_interval(id_id)
  RETURN
END SUBROUTINE nl_get_gfdda_interval
SUBROUTINE nl_get_auxinput11_interval_mo ( id_id , auxinput11_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput11_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxinput11_interval_mo = model_config_rec%auxinput11_interval_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_interval_mo
SUBROUTINE nl_get_auxinput11_interval_d ( id_id , auxinput11_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput11_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  auxinput11_interval_d = model_config_rec%auxinput11_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_interval_d
SUBROUTINE nl_get_auxinput11_interval_h ( id_id , auxinput11_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput11_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  auxinput11_interval_h = model_config_rec%auxinput11_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_interval_h
SUBROUTINE nl_get_auxinput11_interval_m ( id_id , auxinput11_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput11_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  auxinput11_interval_m = model_config_rec%auxinput11_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_interval_m
SUBROUTINE nl_get_auxinput11_interval_s ( id_id , auxinput11_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput11_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  auxinput11_interval_s = model_config_rec%auxinput11_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_interval_s
SUBROUTINE nl_get_auxinput11_interval ( id_id , auxinput11_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput11_interval
  INTEGER id_id
  CHARACTER*80 emess
  auxinput11_interval = model_config_rec%auxinput11_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_interval
SUBROUTINE nl_get_restart_interval_mo ( id_id , restart_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: restart_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  restart_interval_mo = model_config_rec%restart_interval_mo
  RETURN
END SUBROUTINE nl_get_restart_interval_mo
SUBROUTINE nl_get_restart_interval_d ( id_id , restart_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: restart_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  restart_interval_d = model_config_rec%restart_interval_d
  RETURN
END SUBROUTINE nl_get_restart_interval_d
SUBROUTINE nl_get_restart_interval_h ( id_id , restart_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: restart_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  restart_interval_h = model_config_rec%restart_interval_h
  RETURN
END SUBROUTINE nl_get_restart_interval_h
SUBROUTINE nl_get_restart_interval_m ( id_id , restart_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: restart_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  restart_interval_m = model_config_rec%restart_interval_m
  RETURN
END SUBROUTINE nl_get_restart_interval_m
SUBROUTINE nl_get_restart_interval_s ( id_id , restart_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: restart_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  restart_interval_s = model_config_rec%restart_interval_s
  RETURN
END SUBROUTINE nl_get_restart_interval_s
SUBROUTINE nl_get_history_begin_y ( id_id , history_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: history_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  history_begin_y = model_config_rec%history_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_history_begin_y
SUBROUTINE nl_get_history_begin_mo ( id_id , history_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: history_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  history_begin_mo = model_config_rec%history_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_history_begin_mo
SUBROUTINE nl_get_history_begin_d ( id_id , history_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: history_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  history_begin_d = model_config_rec%history_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_history_begin_d
SUBROUTINE nl_get_history_begin_h ( id_id , history_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: history_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  history_begin_h = model_config_rec%history_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_history_begin_h
SUBROUTINE nl_get_history_begin_m ( id_id , history_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: history_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  history_begin_m = model_config_rec%history_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_history_begin_m
SUBROUTINE nl_get_history_begin_s ( id_id , history_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: history_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  history_begin_s = model_config_rec%history_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_history_begin_s
SUBROUTINE nl_get_inputout_begin_y ( id_id , inputout_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: inputout_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  inputout_begin_y = model_config_rec%inputout_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_begin_y
SUBROUTINE nl_get_inputout_begin_mo ( id_id , inputout_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: inputout_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  inputout_begin_mo = model_config_rec%inputout_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_begin_mo
SUBROUTINE nl_get_inputout_begin_d ( id_id , inputout_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: inputout_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  inputout_begin_d = model_config_rec%inputout_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_begin_d
SUBROUTINE nl_get_inputout_begin_h ( id_id , inputout_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: inputout_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  inputout_begin_h = model_config_rec%inputout_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_begin_h
SUBROUTINE nl_get_inputout_begin_m ( id_id , inputout_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: inputout_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  inputout_begin_m = model_config_rec%inputout_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_begin_m
SUBROUTINE nl_get_inputout_begin_s ( id_id , inputout_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: inputout_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  inputout_begin_s = model_config_rec%inputout_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_begin_s
SUBROUTINE nl_get_auxhist1_begin_y ( id_id , auxhist1_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist1_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  auxhist1_begin_y = model_config_rec%auxhist1_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_begin_y
SUBROUTINE nl_get_auxhist1_begin_mo ( id_id , auxhist1_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist1_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxhist1_begin_mo = model_config_rec%auxhist1_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_begin_mo
SUBROUTINE nl_get_auxhist1_begin_d ( id_id , auxhist1_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist1_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  auxhist1_begin_d = model_config_rec%auxhist1_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_begin_d
SUBROUTINE nl_get_auxhist1_begin_h ( id_id , auxhist1_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist1_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  auxhist1_begin_h = model_config_rec%auxhist1_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_begin_h
SUBROUTINE nl_get_auxhist1_begin_m ( id_id , auxhist1_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist1_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  auxhist1_begin_m = model_config_rec%auxhist1_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_begin_m
SUBROUTINE nl_get_auxhist1_begin_s ( id_id , auxhist1_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist1_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  auxhist1_begin_s = model_config_rec%auxhist1_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_begin_s
SUBROUTINE nl_get_auxhist2_begin_y ( id_id , auxhist2_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist2_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  auxhist2_begin_y = model_config_rec%auxhist2_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_begin_y
SUBROUTINE nl_get_auxhist2_begin_mo ( id_id , auxhist2_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist2_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxhist2_begin_mo = model_config_rec%auxhist2_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_begin_mo
SUBROUTINE nl_get_auxhist2_begin_d ( id_id , auxhist2_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist2_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  auxhist2_begin_d = model_config_rec%auxhist2_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_begin_d
SUBROUTINE nl_get_auxhist2_begin_h ( id_id , auxhist2_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist2_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  auxhist2_begin_h = model_config_rec%auxhist2_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_begin_h
SUBROUTINE nl_get_auxhist2_begin_m ( id_id , auxhist2_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist2_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  auxhist2_begin_m = model_config_rec%auxhist2_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_begin_m
SUBROUTINE nl_get_auxhist2_begin_s ( id_id , auxhist2_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist2_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  auxhist2_begin_s = model_config_rec%auxhist2_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_begin_s
SUBROUTINE nl_get_auxhist3_begin_y ( id_id , auxhist3_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist3_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  auxhist3_begin_y = model_config_rec%auxhist3_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_begin_y
SUBROUTINE nl_get_auxhist3_begin_mo ( id_id , auxhist3_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist3_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxhist3_begin_mo = model_config_rec%auxhist3_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_begin_mo
SUBROUTINE nl_get_auxhist3_begin_d ( id_id , auxhist3_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist3_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  auxhist3_begin_d = model_config_rec%auxhist3_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_begin_d
SUBROUTINE nl_get_auxhist3_begin_h ( id_id , auxhist3_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist3_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  auxhist3_begin_h = model_config_rec%auxhist3_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_begin_h
SUBROUTINE nl_get_auxhist3_begin_m ( id_id , auxhist3_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist3_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  auxhist3_begin_m = model_config_rec%auxhist3_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_begin_m
SUBROUTINE nl_get_auxhist3_begin_s ( id_id , auxhist3_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist3_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  auxhist3_begin_s = model_config_rec%auxhist3_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_begin_s
SUBROUTINE nl_get_auxhist4_begin_y ( id_id , auxhist4_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist4_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  auxhist4_begin_y = model_config_rec%auxhist4_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_begin_y
SUBROUTINE nl_get_auxhist4_begin_mo ( id_id , auxhist4_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist4_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxhist4_begin_mo = model_config_rec%auxhist4_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_begin_mo
SUBROUTINE nl_get_auxhist4_begin_d ( id_id , auxhist4_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist4_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  auxhist4_begin_d = model_config_rec%auxhist4_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_begin_d
SUBROUTINE nl_get_auxhist4_begin_h ( id_id , auxhist4_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist4_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  auxhist4_begin_h = model_config_rec%auxhist4_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_begin_h
SUBROUTINE nl_get_auxhist4_begin_m ( id_id , auxhist4_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist4_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  auxhist4_begin_m = model_config_rec%auxhist4_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_begin_m
SUBROUTINE nl_get_auxhist4_begin_s ( id_id , auxhist4_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist4_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  auxhist4_begin_s = model_config_rec%auxhist4_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_begin_s
SUBROUTINE nl_get_auxhist5_begin_y ( id_id , auxhist5_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist5_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  auxhist5_begin_y = model_config_rec%auxhist5_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_begin_y
SUBROUTINE nl_get_auxhist5_begin_mo ( id_id , auxhist5_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist5_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxhist5_begin_mo = model_config_rec%auxhist5_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_begin_mo
SUBROUTINE nl_get_auxhist5_begin_d ( id_id , auxhist5_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist5_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  auxhist5_begin_d = model_config_rec%auxhist5_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_begin_d
SUBROUTINE nl_get_auxhist5_begin_h ( id_id , auxhist5_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist5_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  auxhist5_begin_h = model_config_rec%auxhist5_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_begin_h
SUBROUTINE nl_get_auxhist5_begin_m ( id_id , auxhist5_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist5_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  auxhist5_begin_m = model_config_rec%auxhist5_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_begin_m
SUBROUTINE nl_get_auxhist5_begin_s ( id_id , auxhist5_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist5_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  auxhist5_begin_s = model_config_rec%auxhist5_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_begin_s
SUBROUTINE nl_get_auxhist6_begin_y ( id_id , auxhist6_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist6_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  auxhist6_begin_y = model_config_rec%auxhist6_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_begin_y
SUBROUTINE nl_get_auxhist6_begin_mo ( id_id , auxhist6_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist6_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxhist6_begin_mo = model_config_rec%auxhist6_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_begin_mo
SUBROUTINE nl_get_auxhist6_begin_d ( id_id , auxhist6_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist6_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  auxhist6_begin_d = model_config_rec%auxhist6_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_begin_d
SUBROUTINE nl_get_auxhist6_begin_h ( id_id , auxhist6_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist6_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  auxhist6_begin_h = model_config_rec%auxhist6_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_begin_h
SUBROUTINE nl_get_auxhist6_begin_m ( id_id , auxhist6_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist6_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  auxhist6_begin_m = model_config_rec%auxhist6_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_begin_m
SUBROUTINE nl_get_auxhist6_begin_s ( id_id , auxhist6_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist6_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  auxhist6_begin_s = model_config_rec%auxhist6_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_begin_s
SUBROUTINE nl_get_auxhist7_begin_y ( id_id , auxhist7_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist7_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  auxhist7_begin_y = model_config_rec%auxhist7_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_begin_y
SUBROUTINE nl_get_auxhist7_begin_mo ( id_id , auxhist7_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist7_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxhist7_begin_mo = model_config_rec%auxhist7_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_begin_mo
SUBROUTINE nl_get_auxhist7_begin_d ( id_id , auxhist7_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist7_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  auxhist7_begin_d = model_config_rec%auxhist7_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_begin_d
SUBROUTINE nl_get_auxhist7_begin_h ( id_id , auxhist7_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist7_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  auxhist7_begin_h = model_config_rec%auxhist7_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_begin_h
SUBROUTINE nl_get_auxhist7_begin_m ( id_id , auxhist7_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist7_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  auxhist7_begin_m = model_config_rec%auxhist7_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_begin_m
SUBROUTINE nl_get_auxhist7_begin_s ( id_id , auxhist7_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist7_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  auxhist7_begin_s = model_config_rec%auxhist7_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_begin_s
SUBROUTINE nl_get_auxhist8_begin_y ( id_id , auxhist8_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist8_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  auxhist8_begin_y = model_config_rec%auxhist8_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_begin_y
SUBROUTINE nl_get_auxhist8_begin_mo ( id_id , auxhist8_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist8_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxhist8_begin_mo = model_config_rec%auxhist8_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_begin_mo
SUBROUTINE nl_get_auxhist8_begin_d ( id_id , auxhist8_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist8_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  auxhist8_begin_d = model_config_rec%auxhist8_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_begin_d
SUBROUTINE nl_get_auxhist8_begin_h ( id_id , auxhist8_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist8_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  auxhist8_begin_h = model_config_rec%auxhist8_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_begin_h
SUBROUTINE nl_get_auxhist8_begin_m ( id_id , auxhist8_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist8_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  auxhist8_begin_m = model_config_rec%auxhist8_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_begin_m
SUBROUTINE nl_get_auxhist8_begin_s ( id_id , auxhist8_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist8_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  auxhist8_begin_s = model_config_rec%auxhist8_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_begin_s
SUBROUTINE nl_get_auxhist9_begin_y ( id_id , auxhist9_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist9_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  auxhist9_begin_y = model_config_rec%auxhist9_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_begin_y
SUBROUTINE nl_get_auxhist9_begin_mo ( id_id , auxhist9_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist9_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxhist9_begin_mo = model_config_rec%auxhist9_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_begin_mo
SUBROUTINE nl_get_auxhist9_begin_d ( id_id , auxhist9_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist9_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  auxhist9_begin_d = model_config_rec%auxhist9_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_begin_d
SUBROUTINE nl_get_auxhist9_begin_h ( id_id , auxhist9_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist9_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  auxhist9_begin_h = model_config_rec%auxhist9_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_begin_h
SUBROUTINE nl_get_auxhist9_begin_m ( id_id , auxhist9_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist9_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  auxhist9_begin_m = model_config_rec%auxhist9_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_begin_m
SUBROUTINE nl_get_auxhist9_begin_s ( id_id , auxhist9_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist9_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  auxhist9_begin_s = model_config_rec%auxhist9_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_begin_s
SUBROUTINE nl_get_auxhist10_begin_y ( id_id , auxhist10_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist10_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  auxhist10_begin_y = model_config_rec%auxhist10_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_begin_y
SUBROUTINE nl_get_auxhist10_begin_mo ( id_id , auxhist10_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist10_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxhist10_begin_mo = model_config_rec%auxhist10_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_begin_mo
SUBROUTINE nl_get_auxhist10_begin_d ( id_id , auxhist10_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist10_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  auxhist10_begin_d = model_config_rec%auxhist10_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_begin_d
SUBROUTINE nl_get_auxhist10_begin_h ( id_id , auxhist10_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist10_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  auxhist10_begin_h = model_config_rec%auxhist10_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_begin_h
SUBROUTINE nl_get_auxhist10_begin_m ( id_id , auxhist10_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist10_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  auxhist10_begin_m = model_config_rec%auxhist10_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_begin_m
SUBROUTINE nl_get_auxhist10_begin_s ( id_id , auxhist10_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist10_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  auxhist10_begin_s = model_config_rec%auxhist10_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_begin_s
SUBROUTINE nl_get_auxhist11_begin_y ( id_id , auxhist11_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist11_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  auxhist11_begin_y = model_config_rec%auxhist11_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_begin_y
SUBROUTINE nl_get_auxhist11_begin_mo ( id_id , auxhist11_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist11_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxhist11_begin_mo = model_config_rec%auxhist11_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_begin_mo
SUBROUTINE nl_get_auxhist11_begin_d ( id_id , auxhist11_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist11_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  auxhist11_begin_d = model_config_rec%auxhist11_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_begin_d
SUBROUTINE nl_get_auxhist11_begin_h ( id_id , auxhist11_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist11_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  auxhist11_begin_h = model_config_rec%auxhist11_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_begin_h
SUBROUTINE nl_get_auxhist11_begin_m ( id_id , auxhist11_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist11_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  auxhist11_begin_m = model_config_rec%auxhist11_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_begin_m
SUBROUTINE nl_get_auxhist11_begin_s ( id_id , auxhist11_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist11_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  auxhist11_begin_s = model_config_rec%auxhist11_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_begin_s
SUBROUTINE nl_get_auxinput1_begin_y ( id_id , auxinput1_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput1_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  auxinput1_begin_y = model_config_rec%auxinput1_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_begin_y
SUBROUTINE nl_get_auxinput1_begin_mo ( id_id , auxinput1_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput1_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxinput1_begin_mo = model_config_rec%auxinput1_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_begin_mo
SUBROUTINE nl_get_auxinput1_begin_d ( id_id , auxinput1_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput1_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  auxinput1_begin_d = model_config_rec%auxinput1_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_begin_d
SUBROUTINE nl_get_auxinput1_begin_h ( id_id , auxinput1_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput1_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  auxinput1_begin_h = model_config_rec%auxinput1_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_begin_h
SUBROUTINE nl_get_auxinput1_begin_m ( id_id , auxinput1_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput1_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  auxinput1_begin_m = model_config_rec%auxinput1_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_begin_m
SUBROUTINE nl_get_auxinput1_begin_s ( id_id , auxinput1_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput1_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  auxinput1_begin_s = model_config_rec%auxinput1_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_begin_s
SUBROUTINE nl_get_auxinput2_begin_y ( id_id , auxinput2_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput2_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  auxinput2_begin_y = model_config_rec%auxinput2_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_begin_y
SUBROUTINE nl_get_auxinput2_begin_mo ( id_id , auxinput2_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput2_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxinput2_begin_mo = model_config_rec%auxinput2_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_begin_mo
SUBROUTINE nl_get_auxinput2_begin_d ( id_id , auxinput2_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput2_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  auxinput2_begin_d = model_config_rec%auxinput2_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_begin_d
SUBROUTINE nl_get_auxinput2_begin_h ( id_id , auxinput2_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput2_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  auxinput2_begin_h = model_config_rec%auxinput2_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_begin_h
SUBROUTINE nl_get_auxinput2_begin_m ( id_id , auxinput2_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput2_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  auxinput2_begin_m = model_config_rec%auxinput2_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_begin_m
SUBROUTINE nl_get_auxinput2_begin_s ( id_id , auxinput2_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput2_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  auxinput2_begin_s = model_config_rec%auxinput2_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_begin_s
SUBROUTINE nl_get_auxinput3_begin_y ( id_id , auxinput3_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput3_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  auxinput3_begin_y = model_config_rec%auxinput3_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_begin_y
SUBROUTINE nl_get_auxinput3_begin_mo ( id_id , auxinput3_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput3_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxinput3_begin_mo = model_config_rec%auxinput3_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_begin_mo
SUBROUTINE nl_get_auxinput3_begin_d ( id_id , auxinput3_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput3_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  auxinput3_begin_d = model_config_rec%auxinput3_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_begin_d
SUBROUTINE nl_get_auxinput3_begin_h ( id_id , auxinput3_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput3_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  auxinput3_begin_h = model_config_rec%auxinput3_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_begin_h
SUBROUTINE nl_get_auxinput3_begin_m ( id_id , auxinput3_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput3_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  auxinput3_begin_m = model_config_rec%auxinput3_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_begin_m
SUBROUTINE nl_get_auxinput3_begin_s ( id_id , auxinput3_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput3_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  auxinput3_begin_s = model_config_rec%auxinput3_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_begin_s
SUBROUTINE nl_get_auxinput4_begin_y ( id_id , auxinput4_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput4_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  auxinput4_begin_y = model_config_rec%auxinput4_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_begin_y
SUBROUTINE nl_get_auxinput4_begin_mo ( id_id , auxinput4_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput4_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxinput4_begin_mo = model_config_rec%auxinput4_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_begin_mo
SUBROUTINE nl_get_auxinput4_begin_d ( id_id , auxinput4_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput4_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  auxinput4_begin_d = model_config_rec%auxinput4_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_begin_d
SUBROUTINE nl_get_auxinput4_begin_h ( id_id , auxinput4_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput4_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  auxinput4_begin_h = model_config_rec%auxinput4_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_begin_h
SUBROUTINE nl_get_auxinput4_begin_m ( id_id , auxinput4_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput4_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  auxinput4_begin_m = model_config_rec%auxinput4_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_begin_m
SUBROUTINE nl_get_auxinput4_begin_s ( id_id , auxinput4_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput4_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  auxinput4_begin_s = model_config_rec%auxinput4_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_begin_s
SUBROUTINE nl_get_auxinput5_begin_y ( id_id , auxinput5_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput5_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  auxinput5_begin_y = model_config_rec%auxinput5_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_begin_y
SUBROUTINE nl_get_auxinput5_begin_mo ( id_id , auxinput5_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput5_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxinput5_begin_mo = model_config_rec%auxinput5_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_begin_mo
SUBROUTINE nl_get_auxinput5_begin_d ( id_id , auxinput5_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput5_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  auxinput5_begin_d = model_config_rec%auxinput5_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_begin_d
SUBROUTINE nl_get_auxinput5_begin_h ( id_id , auxinput5_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput5_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  auxinput5_begin_h = model_config_rec%auxinput5_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_begin_h
SUBROUTINE nl_get_auxinput5_begin_m ( id_id , auxinput5_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput5_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  auxinput5_begin_m = model_config_rec%auxinput5_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_begin_m
SUBROUTINE nl_get_auxinput5_begin_s ( id_id , auxinput5_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput5_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  auxinput5_begin_s = model_config_rec%auxinput5_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_begin_s
SUBROUTINE nl_get_auxinput6_begin_y ( id_id , auxinput6_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput6_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  auxinput6_begin_y = model_config_rec%auxinput6_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_begin_y
SUBROUTINE nl_get_auxinput6_begin_mo ( id_id , auxinput6_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput6_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxinput6_begin_mo = model_config_rec%auxinput6_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_begin_mo
SUBROUTINE nl_get_auxinput6_begin_d ( id_id , auxinput6_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput6_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  auxinput6_begin_d = model_config_rec%auxinput6_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_begin_d
SUBROUTINE nl_get_auxinput6_begin_h ( id_id , auxinput6_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput6_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  auxinput6_begin_h = model_config_rec%auxinput6_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_begin_h
SUBROUTINE nl_get_auxinput6_begin_m ( id_id , auxinput6_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput6_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  auxinput6_begin_m = model_config_rec%auxinput6_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_begin_m
SUBROUTINE nl_get_auxinput6_begin_s ( id_id , auxinput6_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput6_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  auxinput6_begin_s = model_config_rec%auxinput6_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_begin_s
SUBROUTINE nl_get_auxinput7_begin_y ( id_id , auxinput7_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput7_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  auxinput7_begin_y = model_config_rec%auxinput7_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_begin_y
SUBROUTINE nl_get_auxinput7_begin_mo ( id_id , auxinput7_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput7_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxinput7_begin_mo = model_config_rec%auxinput7_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_begin_mo
SUBROUTINE nl_get_auxinput7_begin_d ( id_id , auxinput7_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput7_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  auxinput7_begin_d = model_config_rec%auxinput7_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_begin_d
SUBROUTINE nl_get_auxinput7_begin_h ( id_id , auxinput7_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput7_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  auxinput7_begin_h = model_config_rec%auxinput7_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_begin_h
SUBROUTINE nl_get_auxinput7_begin_m ( id_id , auxinput7_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput7_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  auxinput7_begin_m = model_config_rec%auxinput7_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_begin_m
SUBROUTINE nl_get_auxinput7_begin_s ( id_id , auxinput7_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput7_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  auxinput7_begin_s = model_config_rec%auxinput7_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_begin_s
SUBROUTINE nl_get_auxinput8_begin_y ( id_id , auxinput8_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput8_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  auxinput8_begin_y = model_config_rec%auxinput8_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_begin_y
SUBROUTINE nl_get_auxinput8_begin_mo ( id_id , auxinput8_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput8_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxinput8_begin_mo = model_config_rec%auxinput8_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_begin_mo
SUBROUTINE nl_get_auxinput8_begin_d ( id_id , auxinput8_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput8_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  auxinput8_begin_d = model_config_rec%auxinput8_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_begin_d
SUBROUTINE nl_get_auxinput8_begin_h ( id_id , auxinput8_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput8_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  auxinput8_begin_h = model_config_rec%auxinput8_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_begin_h
SUBROUTINE nl_get_auxinput8_begin_m ( id_id , auxinput8_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput8_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  auxinput8_begin_m = model_config_rec%auxinput8_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_begin_m
SUBROUTINE nl_get_auxinput8_begin_s ( id_id , auxinput8_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput8_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  auxinput8_begin_s = model_config_rec%auxinput8_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_begin_s
SUBROUTINE nl_get_sgfdda_begin_y ( id_id , sgfdda_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: sgfdda_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  sgfdda_begin_y = model_config_rec%sgfdda_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_sgfdda_begin_y
SUBROUTINE nl_get_sgfdda_begin_mo ( id_id , sgfdda_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: sgfdda_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  sgfdda_begin_mo = model_config_rec%sgfdda_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_sgfdda_begin_mo
SUBROUTINE nl_get_sgfdda_begin_d ( id_id , sgfdda_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: sgfdda_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  sgfdda_begin_d = model_config_rec%sgfdda_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_sgfdda_begin_d
SUBROUTINE nl_get_sgfdda_begin_h ( id_id , sgfdda_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: sgfdda_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  sgfdda_begin_h = model_config_rec%sgfdda_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_sgfdda_begin_h
SUBROUTINE nl_get_sgfdda_begin_m ( id_id , sgfdda_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: sgfdda_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  sgfdda_begin_m = model_config_rec%sgfdda_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_sgfdda_begin_m
SUBROUTINE nl_get_sgfdda_begin_s ( id_id , sgfdda_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: sgfdda_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  sgfdda_begin_s = model_config_rec%sgfdda_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_sgfdda_begin_s
SUBROUTINE nl_get_gfdda_begin_y ( id_id , gfdda_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: gfdda_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  gfdda_begin_y = model_config_rec%gfdda_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_gfdda_begin_y
SUBROUTINE nl_get_gfdda_begin_mo ( id_id , gfdda_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: gfdda_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  gfdda_begin_mo = model_config_rec%gfdda_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_gfdda_begin_mo
SUBROUTINE nl_get_gfdda_begin_d ( id_id , gfdda_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: gfdda_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  gfdda_begin_d = model_config_rec%gfdda_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_gfdda_begin_d
SUBROUTINE nl_get_gfdda_begin_h ( id_id , gfdda_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: gfdda_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  gfdda_begin_h = model_config_rec%gfdda_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_gfdda_begin_h
SUBROUTINE nl_get_gfdda_begin_m ( id_id , gfdda_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: gfdda_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  gfdda_begin_m = model_config_rec%gfdda_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_gfdda_begin_m
SUBROUTINE nl_get_gfdda_begin_s ( id_id , gfdda_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: gfdda_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  gfdda_begin_s = model_config_rec%gfdda_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_gfdda_begin_s
SUBROUTINE nl_get_auxinput11_begin_y ( id_id , auxinput11_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput11_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  auxinput11_begin_y = model_config_rec%auxinput11_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_begin_y
SUBROUTINE nl_get_auxinput11_begin_mo ( id_id , auxinput11_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput11_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  auxinput11_begin_mo = model_config_rec%auxinput11_begin_mo(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_begin_mo
SUBROUTINE nl_get_auxinput11_begin_d ( id_id , auxinput11_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput11_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  auxinput11_begin_d = model_config_rec%auxinput11_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_begin_d
SUBROUTINE nl_get_auxinput11_begin_h ( id_id , auxinput11_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput11_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  auxinput11_begin_h = model_config_rec%auxinput11_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_begin_h
SUBROUTINE nl_get_auxinput11_begin_m ( id_id , auxinput11_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput11_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  auxinput11_begin_m = model_config_rec%auxinput11_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_begin_m
SUBROUTINE nl_get_auxinput11_begin_s ( id_id , auxinput11_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput11_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  auxinput11_begin_s = model_config_rec%auxinput11_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_begin_s
SUBROUTINE nl_get_restart_begin_y ( id_id , restart_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: restart_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  restart_begin_y = model_config_rec%restart_begin_y
  RETURN
END SUBROUTINE nl_get_restart_begin_y
SUBROUTINE nl_get_restart_begin_mo ( id_id , restart_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: restart_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  restart_begin_mo = model_config_rec%restart_begin_mo
  RETURN
END SUBROUTINE nl_get_restart_begin_mo
SUBROUTINE nl_get_restart_begin_d ( id_id , restart_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: restart_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  restart_begin_d = model_config_rec%restart_begin_d
  RETURN
END SUBROUTINE nl_get_restart_begin_d
SUBROUTINE nl_get_restart_begin_h ( id_id , restart_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: restart_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  restart_begin_h = model_config_rec%restart_begin_h
  RETURN
END SUBROUTINE nl_get_restart_begin_h
SUBROUTINE nl_get_restart_begin_m ( id_id , restart_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: restart_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  restart_begin_m = model_config_rec%restart_begin_m
  RETURN
END SUBROUTINE nl_get_restart_begin_m
SUBROUTINE nl_get_restart_begin_s ( id_id , restart_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: restart_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  restart_begin_s = model_config_rec%restart_begin_s
  RETURN
END SUBROUTINE nl_get_restart_begin_s
SUBROUTINE nl_get_history_end_y ( id_id , history_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: history_end_y
  INTEGER id_id
  CHARACTER*80 emess
  history_end_y = model_config_rec%history_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_history_end_y
SUBROUTINE nl_get_history_end_mo ( id_id , history_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: history_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  history_end_mo = model_config_rec%history_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_history_end_mo
SUBROUTINE nl_get_history_end_d ( id_id , history_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: history_end_d
  INTEGER id_id
  CHARACTER*80 emess
  history_end_d = model_config_rec%history_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_history_end_d
SUBROUTINE nl_get_history_end_h ( id_id , history_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: history_end_h
  INTEGER id_id
  CHARACTER*80 emess
  history_end_h = model_config_rec%history_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_history_end_h
SUBROUTINE nl_get_history_end_m ( id_id , history_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: history_end_m
  INTEGER id_id
  CHARACTER*80 emess
  history_end_m = model_config_rec%history_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_history_end_m
SUBROUTINE nl_get_history_end_s ( id_id , history_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: history_end_s
  INTEGER id_id
  CHARACTER*80 emess
  history_end_s = model_config_rec%history_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_history_end_s
SUBROUTINE nl_get_inputout_end_y ( id_id , inputout_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: inputout_end_y
  INTEGER id_id
  CHARACTER*80 emess
  inputout_end_y = model_config_rec%inputout_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_end_y
SUBROUTINE nl_get_inputout_end_mo ( id_id , inputout_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: inputout_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  inputout_end_mo = model_config_rec%inputout_end_mo(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_end_mo
SUBROUTINE nl_get_inputout_end_d ( id_id , inputout_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: inputout_end_d
  INTEGER id_id
  CHARACTER*80 emess
  inputout_end_d = model_config_rec%inputout_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_end_d




