!STARTOFREGISTRYGENERATEDINCLUDE 'inc/set_0_nl_config.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
SUBROUTINE nl_set_run_days ( id_id , run_days )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: run_days
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%run_days = run_days 
  RETURN
END SUBROUTINE nl_set_run_days
SUBROUTINE nl_set_run_hours ( id_id , run_hours )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: run_hours
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%run_hours = run_hours 
  RETURN
END SUBROUTINE nl_set_run_hours
SUBROUTINE nl_set_run_minutes ( id_id , run_minutes )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: run_minutes
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%run_minutes = run_minutes 
  RETURN
END SUBROUTINE nl_set_run_minutes
SUBROUTINE nl_set_run_seconds ( id_id , run_seconds )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: run_seconds
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%run_seconds = run_seconds 
  RETURN
END SUBROUTINE nl_set_run_seconds
SUBROUTINE nl_set_start_year ( id_id , start_year )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: start_year
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%start_year(id_id) = start_year
  RETURN
END SUBROUTINE nl_set_start_year
SUBROUTINE nl_set_start_month ( id_id , start_month )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: start_month
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%start_month(id_id) = start_month
  RETURN
END SUBROUTINE nl_set_start_month
SUBROUTINE nl_set_start_day ( id_id , start_day )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: start_day
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%start_day(id_id) = start_day
  RETURN
END SUBROUTINE nl_set_start_day
SUBROUTINE nl_set_start_hour ( id_id , start_hour )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: start_hour
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%start_hour(id_id) = start_hour
  RETURN
END SUBROUTINE nl_set_start_hour
SUBROUTINE nl_set_start_minute ( id_id , start_minute )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: start_minute
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%start_minute(id_id) = start_minute
  RETURN
END SUBROUTINE nl_set_start_minute
SUBROUTINE nl_set_start_second ( id_id , start_second )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: start_second
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%start_second(id_id) = start_second
  RETURN
END SUBROUTINE nl_set_start_second
SUBROUTINE nl_set_end_year ( id_id , end_year )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: end_year
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%end_year(id_id) = end_year
  RETURN
END SUBROUTINE nl_set_end_year
SUBROUTINE nl_set_end_month ( id_id , end_month )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: end_month
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%end_month(id_id) = end_month
  RETURN
END SUBROUTINE nl_set_end_month
SUBROUTINE nl_set_end_day ( id_id , end_day )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: end_day
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%end_day(id_id) = end_day
  RETURN
END SUBROUTINE nl_set_end_day
SUBROUTINE nl_set_end_hour ( id_id , end_hour )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: end_hour
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%end_hour(id_id) = end_hour
  RETURN
END SUBROUTINE nl_set_end_hour
SUBROUTINE nl_set_end_minute ( id_id , end_minute )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: end_minute
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%end_minute(id_id) = end_minute
  RETURN
END SUBROUTINE nl_set_end_minute
SUBROUTINE nl_set_end_second ( id_id , end_second )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: end_second
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%end_second(id_id) = end_second
  RETURN
END SUBROUTINE nl_set_end_second
SUBROUTINE nl_set_interval_seconds ( id_id , interval_seconds )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: interval_seconds
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%interval_seconds = interval_seconds 
  RETURN
END SUBROUTINE nl_set_interval_seconds
SUBROUTINE nl_set_input_from_file ( id_id , input_from_file )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: input_from_file
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%input_from_file(id_id) = input_from_file
  RETURN
END SUBROUTINE nl_set_input_from_file
SUBROUTINE nl_set_fine_input_stream ( id_id , fine_input_stream )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: fine_input_stream
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%fine_input_stream(id_id) = fine_input_stream
  RETURN
END SUBROUTINE nl_set_fine_input_stream
SUBROUTINE nl_set_history_interval ( id_id , history_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_interval
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%history_interval(id_id) = history_interval
  RETURN
END SUBROUTINE nl_set_history_interval
SUBROUTINE nl_set_frames_per_outfile ( id_id , frames_per_outfile )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_outfile
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%frames_per_outfile(id_id) = frames_per_outfile
  RETURN
END SUBROUTINE nl_set_frames_per_outfile
SUBROUTINE nl_set_frames_per_auxhist1 ( id_id , frames_per_auxhist1 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxhist1
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%frames_per_auxhist1(id_id) = frames_per_auxhist1
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist1
SUBROUTINE nl_set_frames_per_auxhist2 ( id_id , frames_per_auxhist2 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxhist2
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%frames_per_auxhist2(id_id) = frames_per_auxhist2
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist2
SUBROUTINE nl_set_frames_per_auxhist3 ( id_id , frames_per_auxhist3 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxhist3
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%frames_per_auxhist3(id_id) = frames_per_auxhist3
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist3
SUBROUTINE nl_set_frames_per_auxhist4 ( id_id , frames_per_auxhist4 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxhist4
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%frames_per_auxhist4(id_id) = frames_per_auxhist4
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist4
SUBROUTINE nl_set_frames_per_auxhist5 ( id_id , frames_per_auxhist5 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxhist5
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%frames_per_auxhist5(id_id) = frames_per_auxhist5
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist5
SUBROUTINE nl_set_frames_per_auxhist6 ( id_id , frames_per_auxhist6 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxhist6
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%frames_per_auxhist6(id_id) = frames_per_auxhist6
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist6
SUBROUTINE nl_set_frames_per_auxhist7 ( id_id , frames_per_auxhist7 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxhist7
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%frames_per_auxhist7(id_id) = frames_per_auxhist7
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist7
SUBROUTINE nl_set_frames_per_auxhist8 ( id_id , frames_per_auxhist8 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxhist8
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%frames_per_auxhist8(id_id) = frames_per_auxhist8
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist8
SUBROUTINE nl_set_frames_per_auxhist9 ( id_id , frames_per_auxhist9 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxhist9
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%frames_per_auxhist9(id_id) = frames_per_auxhist9
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist9
SUBROUTINE nl_set_frames_per_auxhist10 ( id_id , frames_per_auxhist10 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxhist10
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%frames_per_auxhist10(id_id) = frames_per_auxhist10
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist10
SUBROUTINE nl_set_frames_per_auxhist11 ( id_id , frames_per_auxhist11 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxhist11
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%frames_per_auxhist11(id_id) = frames_per_auxhist11
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist11
SUBROUTINE nl_set_restart ( id_id , restart )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: restart
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%restart = restart 
  RETURN
END SUBROUTINE nl_set_restart
SUBROUTINE nl_set_restart_interval ( id_id , restart_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: restart_interval
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%restart_interval = restart_interval 
  RETURN
END SUBROUTINE nl_set_restart_interval
SUBROUTINE nl_set_io_form_input ( id_id , io_form_input )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_input
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%io_form_input = io_form_input 
  RETURN
END SUBROUTINE nl_set_io_form_input
SUBROUTINE nl_set_io_form_history ( id_id , io_form_history )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_history
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%io_form_history = io_form_history 
  RETURN
END SUBROUTINE nl_set_io_form_history
SUBROUTINE nl_set_io_form_restart ( id_id , io_form_restart )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_restart
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%io_form_restart = io_form_restart 
  RETURN
END SUBROUTINE nl_set_io_form_restart
SUBROUTINE nl_set_io_form_boundary ( id_id , io_form_boundary )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_boundary
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%io_form_boundary = io_form_boundary 
  RETURN
END SUBROUTINE nl_set_io_form_boundary
SUBROUTINE nl_set_debug_level ( id_id , debug_level )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: debug_level
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%debug_level = debug_level 
  RETURN
END SUBROUTINE nl_set_debug_level
SUBROUTINE nl_set_self_test_domain ( id_id , self_test_domain )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: self_test_domain
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%self_test_domain = self_test_domain 
  RETURN
END SUBROUTINE nl_set_self_test_domain
SUBROUTINE nl_set_history_outname ( id_id , history_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: history_outname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%history_outname = trim(history_outname) 
  RETURN
END SUBROUTINE nl_set_history_outname
SUBROUTINE nl_set_auxhist1_outname ( id_id , auxhist1_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist1_outname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist1_outname = trim(auxhist1_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist1_outname
SUBROUTINE nl_set_auxhist2_outname ( id_id , auxhist2_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist2_outname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist2_outname = trim(auxhist2_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist2_outname
SUBROUTINE nl_set_auxhist3_outname ( id_id , auxhist3_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist3_outname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist3_outname = trim(auxhist3_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist3_outname
SUBROUTINE nl_set_auxhist4_outname ( id_id , auxhist4_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist4_outname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist4_outname = trim(auxhist4_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist4_outname
SUBROUTINE nl_set_auxhist5_outname ( id_id , auxhist5_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist5_outname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist5_outname = trim(auxhist5_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist5_outname
SUBROUTINE nl_set_auxhist6_outname ( id_id , auxhist6_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist6_outname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist6_outname = trim(auxhist6_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist6_outname
SUBROUTINE nl_set_auxhist7_outname ( id_id , auxhist7_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist7_outname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist7_outname = trim(auxhist7_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist7_outname
SUBROUTINE nl_set_auxhist8_outname ( id_id , auxhist8_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist8_outname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist8_outname = trim(auxhist8_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist8_outname
SUBROUTINE nl_set_auxhist9_outname ( id_id , auxhist9_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist9_outname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist9_outname = trim(auxhist9_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist9_outname
SUBROUTINE nl_set_auxhist10_outname ( id_id , auxhist10_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist10_outname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist10_outname = trim(auxhist10_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist10_outname
SUBROUTINE nl_set_auxhist11_outname ( id_id , auxhist11_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist11_outname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist11_outname = trim(auxhist11_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist11_outname
SUBROUTINE nl_set_history_inname ( id_id , history_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: history_inname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%history_inname = trim(history_inname) 
  RETURN
END SUBROUTINE nl_set_history_inname
SUBROUTINE nl_set_auxhist1_inname ( id_id , auxhist1_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist1_inname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist1_inname = trim(auxhist1_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist1_inname
SUBROUTINE nl_set_auxhist2_inname ( id_id , auxhist2_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist2_inname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist2_inname = trim(auxhist2_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist2_inname
SUBROUTINE nl_set_auxhist3_inname ( id_id , auxhist3_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist3_inname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist3_inname = trim(auxhist3_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist3_inname
SUBROUTINE nl_set_auxhist4_inname ( id_id , auxhist4_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist4_inname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist4_inname = trim(auxhist4_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist4_inname
SUBROUTINE nl_set_auxhist5_inname ( id_id , auxhist5_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist5_inname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist5_inname = trim(auxhist5_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist5_inname
SUBROUTINE nl_set_auxhist6_inname ( id_id , auxhist6_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist6_inname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist6_inname = trim(auxhist6_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist6_inname
SUBROUTINE nl_set_auxhist7_inname ( id_id , auxhist7_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist7_inname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist7_inname = trim(auxhist7_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist7_inname
SUBROUTINE nl_set_auxhist8_inname ( id_id , auxhist8_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist8_inname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist8_inname = trim(auxhist8_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist8_inname
SUBROUTINE nl_set_auxhist9_inname ( id_id , auxhist9_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist9_inname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist9_inname = trim(auxhist9_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist9_inname
SUBROUTINE nl_set_auxhist10_inname ( id_id , auxhist10_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist10_inname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist10_inname = trim(auxhist10_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist10_inname
SUBROUTINE nl_set_auxhist11_inname ( id_id , auxhist11_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist11_inname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist11_inname = trim(auxhist11_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist11_inname
SUBROUTINE nl_set_auxinput1_outname ( id_id , auxinput1_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput1_outname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput1_outname = trim(auxinput1_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput1_outname
SUBROUTINE nl_set_auxinput2_outname ( id_id , auxinput2_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput2_outname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput2_outname = trim(auxinput2_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput2_outname
SUBROUTINE nl_set_auxinput3_outname ( id_id , auxinput3_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput3_outname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput3_outname = trim(auxinput3_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput3_outname
SUBROUTINE nl_set_auxinput4_outname ( id_id , auxinput4_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput4_outname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput4_outname = trim(auxinput4_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput4_outname
SUBROUTINE nl_set_auxinput5_outname ( id_id , auxinput5_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput5_outname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput5_outname = trim(auxinput5_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput5_outname
SUBROUTINE nl_set_auxinput6_outname ( id_id , auxinput6_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput6_outname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput6_outname = trim(auxinput6_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput6_outname
SUBROUTINE nl_set_auxinput7_outname ( id_id , auxinput7_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput7_outname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput7_outname = trim(auxinput7_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput7_outname
SUBROUTINE nl_set_auxinput8_outname ( id_id , auxinput8_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput8_outname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput8_outname = trim(auxinput8_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput8_outname
SUBROUTINE nl_set_auxinput9_outname ( id_id , auxinput9_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput9_outname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput9_outname = trim(auxinput9_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput9_outname
SUBROUTINE nl_set_auxinput10_outname ( id_id , auxinput10_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput10_outname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput10_outname = trim(auxinput10_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput10_outname
SUBROUTINE nl_set_auxinput11_outname ( id_id , auxinput11_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput11_outname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput11_outname = trim(auxinput11_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput11_outname
SUBROUTINE nl_set_auxinput1_inname ( id_id , auxinput1_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput1_inname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput1_inname = trim(auxinput1_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput1_inname
SUBROUTINE nl_set_auxinput2_inname ( id_id , auxinput2_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput2_inname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput2_inname = trim(auxinput2_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput2_inname
SUBROUTINE nl_set_auxinput3_inname ( id_id , auxinput3_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput3_inname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput3_inname = trim(auxinput3_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput3_inname
SUBROUTINE nl_set_auxinput4_inname ( id_id , auxinput4_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput4_inname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput4_inname = trim(auxinput4_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput4_inname
SUBROUTINE nl_set_auxinput5_inname ( id_id , auxinput5_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput5_inname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput5_inname = trim(auxinput5_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput5_inname
SUBROUTINE nl_set_auxinput6_inname ( id_id , auxinput6_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput6_inname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput6_inname = trim(auxinput6_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput6_inname
SUBROUTINE nl_set_auxinput7_inname ( id_id , auxinput7_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput7_inname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput7_inname = trim(auxinput7_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput7_inname
SUBROUTINE nl_set_auxinput8_inname ( id_id , auxinput8_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput8_inname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput8_inname = trim(auxinput8_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput8_inname
SUBROUTINE nl_set_sgfdda_inname ( id_id , sgfdda_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: sgfdda_inname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%sgfdda_inname = trim(sgfdda_inname) 
  RETURN
END SUBROUTINE nl_set_sgfdda_inname
SUBROUTINE nl_set_gfdda_inname ( id_id , gfdda_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: gfdda_inname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%gfdda_inname = trim(gfdda_inname) 
  RETURN
END SUBROUTINE nl_set_gfdda_inname
SUBROUTINE nl_set_auxinput11_inname ( id_id , auxinput11_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput11_inname
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput11_inname = trim(auxinput11_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput11_inname
SUBROUTINE nl_set_history_interval_mo ( id_id , history_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%history_interval_mo(id_id) = history_interval_mo
  RETURN
END SUBROUTINE nl_set_history_interval_mo
SUBROUTINE nl_set_history_interval_d ( id_id , history_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%history_interval_d(id_id) = history_interval_d
  RETURN
END SUBROUTINE nl_set_history_interval_d
SUBROUTINE nl_set_history_interval_h ( id_id , history_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%history_interval_h(id_id) = history_interval_h
  RETURN
END SUBROUTINE nl_set_history_interval_h
SUBROUTINE nl_set_history_interval_m ( id_id , history_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%history_interval_m(id_id) = history_interval_m
  RETURN
END SUBROUTINE nl_set_history_interval_m
SUBROUTINE nl_set_history_interval_s ( id_id , history_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%history_interval_s(id_id) = history_interval_s
  RETURN
END SUBROUTINE nl_set_history_interval_s
SUBROUTINE nl_set_inputout_interval_mo ( id_id , inputout_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%inputout_interval_mo(id_id) = inputout_interval_mo
  RETURN
END SUBROUTINE nl_set_inputout_interval_mo
SUBROUTINE nl_set_inputout_interval_d ( id_id , inputout_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%inputout_interval_d(id_id) = inputout_interval_d
  RETURN
END SUBROUTINE nl_set_inputout_interval_d
SUBROUTINE nl_set_inputout_interval_h ( id_id , inputout_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%inputout_interval_h(id_id) = inputout_interval_h
  RETURN
END SUBROUTINE nl_set_inputout_interval_h
SUBROUTINE nl_set_inputout_interval_m ( id_id , inputout_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%inputout_interval_m(id_id) = inputout_interval_m
  RETURN
END SUBROUTINE nl_set_inputout_interval_m
SUBROUTINE nl_set_inputout_interval_s ( id_id , inputout_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%inputout_interval_s(id_id) = inputout_interval_s
  RETURN
END SUBROUTINE nl_set_inputout_interval_s
SUBROUTINE nl_set_inputout_interval ( id_id , inputout_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_interval
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%inputout_interval(id_id) = inputout_interval
  RETURN
END SUBROUTINE nl_set_inputout_interval
SUBROUTINE nl_set_auxhist1_interval_mo ( id_id , auxhist1_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist1_interval_mo(id_id) = auxhist1_interval_mo
  RETURN
END SUBROUTINE nl_set_auxhist1_interval_mo
SUBROUTINE nl_set_auxhist1_interval_d ( id_id , auxhist1_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist1_interval_d(id_id) = auxhist1_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist1_interval_d
SUBROUTINE nl_set_auxhist1_interval_h ( id_id , auxhist1_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist1_interval_h(id_id) = auxhist1_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist1_interval_h
SUBROUTINE nl_set_auxhist1_interval_m ( id_id , auxhist1_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist1_interval_m(id_id) = auxhist1_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist1_interval_m
SUBROUTINE nl_set_auxhist1_interval_s ( id_id , auxhist1_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist1_interval_s(id_id) = auxhist1_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist1_interval_s
SUBROUTINE nl_set_auxhist1_interval ( id_id , auxhist1_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_interval
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist1_interval(id_id) = auxhist1_interval
  RETURN
END SUBROUTINE nl_set_auxhist1_interval
SUBROUTINE nl_set_auxhist2_interval_mo ( id_id , auxhist2_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist2_interval_mo(id_id) = auxhist2_interval_mo
  RETURN
END SUBROUTINE nl_set_auxhist2_interval_mo
SUBROUTINE nl_set_auxhist2_interval_d ( id_id , auxhist2_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist2_interval_d(id_id) = auxhist2_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist2_interval_d
SUBROUTINE nl_set_auxhist2_interval_h ( id_id , auxhist2_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist2_interval_h(id_id) = auxhist2_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist2_interval_h
SUBROUTINE nl_set_auxhist2_interval_m ( id_id , auxhist2_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist2_interval_m(id_id) = auxhist2_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist2_interval_m
SUBROUTINE nl_set_auxhist2_interval_s ( id_id , auxhist2_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist2_interval_s(id_id) = auxhist2_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist2_interval_s
SUBROUTINE nl_set_auxhist2_interval ( id_id , auxhist2_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_interval
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist2_interval(id_id) = auxhist2_interval
  RETURN
END SUBROUTINE nl_set_auxhist2_interval
SUBROUTINE nl_set_auxhist3_interval_mo ( id_id , auxhist3_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist3_interval_mo(id_id) = auxhist3_interval_mo
  RETURN
END SUBROUTINE nl_set_auxhist3_interval_mo
SUBROUTINE nl_set_auxhist3_interval_d ( id_id , auxhist3_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist3_interval_d(id_id) = auxhist3_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist3_interval_d
SUBROUTINE nl_set_auxhist3_interval_h ( id_id , auxhist3_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist3_interval_h(id_id) = auxhist3_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist3_interval_h
SUBROUTINE nl_set_auxhist3_interval_m ( id_id , auxhist3_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist3_interval_m(id_id) = auxhist3_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist3_interval_m
SUBROUTINE nl_set_auxhist3_interval_s ( id_id , auxhist3_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist3_interval_s(id_id) = auxhist3_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist3_interval_s
SUBROUTINE nl_set_auxhist3_interval ( id_id , auxhist3_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_interval
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist3_interval(id_id) = auxhist3_interval
  RETURN
END SUBROUTINE nl_set_auxhist3_interval
SUBROUTINE nl_set_auxhist4_interval_mo ( id_id , auxhist4_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist4_interval_mo(id_id) = auxhist4_interval_mo
  RETURN
END SUBROUTINE nl_set_auxhist4_interval_mo
SUBROUTINE nl_set_auxhist4_interval_d ( id_id , auxhist4_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist4_interval_d(id_id) = auxhist4_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist4_interval_d
SUBROUTINE nl_set_auxhist4_interval_h ( id_id , auxhist4_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist4_interval_h(id_id) = auxhist4_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist4_interval_h
SUBROUTINE nl_set_auxhist4_interval_m ( id_id , auxhist4_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist4_interval_m(id_id) = auxhist4_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist4_interval_m
SUBROUTINE nl_set_auxhist4_interval_s ( id_id , auxhist4_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist4_interval_s(id_id) = auxhist4_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist4_interval_s
SUBROUTINE nl_set_auxhist4_interval ( id_id , auxhist4_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_interval
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist4_interval(id_id) = auxhist4_interval
  RETURN
END SUBROUTINE nl_set_auxhist4_interval
SUBROUTINE nl_set_auxhist5_interval_mo ( id_id , auxhist5_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist5_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist5_interval_mo(id_id) = auxhist5_interval_mo
  RETURN
END SUBROUTINE nl_set_auxhist5_interval_mo
SUBROUTINE nl_set_auxhist5_interval_d ( id_id , auxhist5_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist5_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist5_interval_d(id_id) = auxhist5_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist5_interval_d
SUBROUTINE nl_set_auxhist5_interval_h ( id_id , auxhist5_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist5_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist5_interval_h(id_id) = auxhist5_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist5_interval_h
SUBROUTINE nl_set_auxhist5_interval_m ( id_id , auxhist5_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist5_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist5_interval_m(id_id) = auxhist5_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist5_interval_m
SUBROUTINE nl_set_auxhist5_interval_s ( id_id , auxhist5_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist5_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist5_interval_s(id_id) = auxhist5_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist5_interval_s
SUBROUTINE nl_set_auxhist5_interval ( id_id , auxhist5_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist5_interval
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist5_interval(id_id) = auxhist5_interval
  RETURN
END SUBROUTINE nl_set_auxhist5_interval
SUBROUTINE nl_set_auxhist6_interval_mo ( id_id , auxhist6_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist6_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist6_interval_mo(id_id) = auxhist6_interval_mo
  RETURN
END SUBROUTINE nl_set_auxhist6_interval_mo
SUBROUTINE nl_set_auxhist6_interval_d ( id_id , auxhist6_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist6_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist6_interval_d(id_id) = auxhist6_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist6_interval_d
SUBROUTINE nl_set_auxhist6_interval_h ( id_id , auxhist6_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist6_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist6_interval_h(id_id) = auxhist6_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist6_interval_h
SUBROUTINE nl_set_auxhist6_interval_m ( id_id , auxhist6_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist6_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist6_interval_m(id_id) = auxhist6_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist6_interval_m
SUBROUTINE nl_set_auxhist6_interval_s ( id_id , auxhist6_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist6_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist6_interval_s(id_id) = auxhist6_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist6_interval_s
SUBROUTINE nl_set_auxhist6_interval ( id_id , auxhist6_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist6_interval
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist6_interval(id_id) = auxhist6_interval
  RETURN
END SUBROUTINE nl_set_auxhist6_interval
SUBROUTINE nl_set_auxhist7_interval_mo ( id_id , auxhist7_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist7_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist7_interval_mo(id_id) = auxhist7_interval_mo
  RETURN
END SUBROUTINE nl_set_auxhist7_interval_mo
SUBROUTINE nl_set_auxhist7_interval_d ( id_id , auxhist7_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist7_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist7_interval_d(id_id) = auxhist7_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist7_interval_d
SUBROUTINE nl_set_auxhist7_interval_h ( id_id , auxhist7_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist7_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist7_interval_h(id_id) = auxhist7_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist7_interval_h
SUBROUTINE nl_set_auxhist7_interval_m ( id_id , auxhist7_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist7_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist7_interval_m(id_id) = auxhist7_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist7_interval_m
SUBROUTINE nl_set_auxhist7_interval_s ( id_id , auxhist7_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist7_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist7_interval_s(id_id) = auxhist7_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist7_interval_s
SUBROUTINE nl_set_auxhist7_interval ( id_id , auxhist7_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist7_interval
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist7_interval(id_id) = auxhist7_interval
  RETURN
END SUBROUTINE nl_set_auxhist7_interval
SUBROUTINE nl_set_auxhist8_interval_mo ( id_id , auxhist8_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist8_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist8_interval_mo(id_id) = auxhist8_interval_mo
  RETURN
END SUBROUTINE nl_set_auxhist8_interval_mo
SUBROUTINE nl_set_auxhist8_interval_d ( id_id , auxhist8_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist8_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist8_interval_d(id_id) = auxhist8_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist8_interval_d
SUBROUTINE nl_set_auxhist8_interval_h ( id_id , auxhist8_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist8_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist8_interval_h(id_id) = auxhist8_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist8_interval_h
SUBROUTINE nl_set_auxhist8_interval_m ( id_id , auxhist8_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist8_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist8_interval_m(id_id) = auxhist8_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist8_interval_m
SUBROUTINE nl_set_auxhist8_interval_s ( id_id , auxhist8_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist8_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist8_interval_s(id_id) = auxhist8_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist8_interval_s
SUBROUTINE nl_set_auxhist8_interval ( id_id , auxhist8_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist8_interval
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist8_interval(id_id) = auxhist8_interval
  RETURN
END SUBROUTINE nl_set_auxhist8_interval
SUBROUTINE nl_set_auxhist9_interval_mo ( id_id , auxhist9_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist9_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist9_interval_mo(id_id) = auxhist9_interval_mo
  RETURN
END SUBROUTINE nl_set_auxhist9_interval_mo
SUBROUTINE nl_set_auxhist9_interval_d ( id_id , auxhist9_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist9_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist9_interval_d(id_id) = auxhist9_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist9_interval_d
SUBROUTINE nl_set_auxhist9_interval_h ( id_id , auxhist9_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist9_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist9_interval_h(id_id) = auxhist9_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist9_interval_h
SUBROUTINE nl_set_auxhist9_interval_m ( id_id , auxhist9_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist9_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist9_interval_m(id_id) = auxhist9_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist9_interval_m
SUBROUTINE nl_set_auxhist9_interval_s ( id_id , auxhist9_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist9_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist9_interval_s(id_id) = auxhist9_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist9_interval_s
SUBROUTINE nl_set_auxhist9_interval ( id_id , auxhist9_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist9_interval
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist9_interval(id_id) = auxhist9_interval
  RETURN
END SUBROUTINE nl_set_auxhist9_interval
SUBROUTINE nl_set_auxhist10_interval_mo ( id_id , auxhist10_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist10_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist10_interval_mo(id_id) = auxhist10_interval_mo
  RETURN
END SUBROUTINE nl_set_auxhist10_interval_mo
SUBROUTINE nl_set_auxhist10_interval_d ( id_id , auxhist10_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist10_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist10_interval_d(id_id) = auxhist10_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist10_interval_d
SUBROUTINE nl_set_auxhist10_interval_h ( id_id , auxhist10_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist10_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist10_interval_h(id_id) = auxhist10_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist10_interval_h
SUBROUTINE nl_set_auxhist10_interval_m ( id_id , auxhist10_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist10_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist10_interval_m(id_id) = auxhist10_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist10_interval_m
SUBROUTINE nl_set_auxhist10_interval_s ( id_id , auxhist10_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist10_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist10_interval_s(id_id) = auxhist10_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist10_interval_s
SUBROUTINE nl_set_auxhist10_interval ( id_id , auxhist10_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist10_interval
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist10_interval(id_id) = auxhist10_interval
  RETURN
END SUBROUTINE nl_set_auxhist10_interval
SUBROUTINE nl_set_auxhist11_interval_mo ( id_id , auxhist11_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist11_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist11_interval_mo(id_id) = auxhist11_interval_mo
  RETURN
END SUBROUTINE nl_set_auxhist11_interval_mo
SUBROUTINE nl_set_auxhist11_interval_d ( id_id , auxhist11_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist11_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist11_interval_d(id_id) = auxhist11_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist11_interval_d
SUBROUTINE nl_set_auxhist11_interval_h ( id_id , auxhist11_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist11_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist11_interval_h(id_id) = auxhist11_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist11_interval_h
SUBROUTINE nl_set_auxhist11_interval_m ( id_id , auxhist11_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist11_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist11_interval_m(id_id) = auxhist11_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist11_interval_m
SUBROUTINE nl_set_auxhist11_interval_s ( id_id , auxhist11_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist11_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist11_interval_s(id_id) = auxhist11_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist11_interval_s
SUBROUTINE nl_set_auxhist11_interval ( id_id , auxhist11_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist11_interval
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist11_interval(id_id) = auxhist11_interval
  RETURN
END SUBROUTINE nl_set_auxhist11_interval
SUBROUTINE nl_set_auxinput1_interval_mo ( id_id , auxinput1_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput1_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput1_interval_mo(id_id) = auxinput1_interval_mo
  RETURN
END SUBROUTINE nl_set_auxinput1_interval_mo
SUBROUTINE nl_set_auxinput1_interval_d ( id_id , auxinput1_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput1_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput1_interval_d(id_id) = auxinput1_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput1_interval_d
SUBROUTINE nl_set_auxinput1_interval_h ( id_id , auxinput1_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput1_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput1_interval_h(id_id) = auxinput1_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput1_interval_h
SUBROUTINE nl_set_auxinput1_interval_m ( id_id , auxinput1_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput1_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput1_interval_m(id_id) = auxinput1_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput1_interval_m
SUBROUTINE nl_set_auxinput1_interval_s ( id_id , auxinput1_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput1_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput1_interval_s(id_id) = auxinput1_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput1_interval_s
SUBROUTINE nl_set_auxinput1_interval ( id_id , auxinput1_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput1_interval
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput1_interval(id_id) = auxinput1_interval
  RETURN
END SUBROUTINE nl_set_auxinput1_interval
SUBROUTINE nl_set_auxinput2_interval_mo ( id_id , auxinput2_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput2_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput2_interval_mo(id_id) = auxinput2_interval_mo
  RETURN
END SUBROUTINE nl_set_auxinput2_interval_mo
SUBROUTINE nl_set_auxinput2_interval_d ( id_id , auxinput2_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput2_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput2_interval_d(id_id) = auxinput2_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput2_interval_d
SUBROUTINE nl_set_auxinput2_interval_h ( id_id , auxinput2_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput2_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput2_interval_h(id_id) = auxinput2_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput2_interval_h
SUBROUTINE nl_set_auxinput2_interval_m ( id_id , auxinput2_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput2_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput2_interval_m(id_id) = auxinput2_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput2_interval_m
SUBROUTINE nl_set_auxinput2_interval_s ( id_id , auxinput2_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput2_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput2_interval_s(id_id) = auxinput2_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput2_interval_s
SUBROUTINE nl_set_auxinput2_interval ( id_id , auxinput2_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput2_interval
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput2_interval(id_id) = auxinput2_interval
  RETURN
END SUBROUTINE nl_set_auxinput2_interval
SUBROUTINE nl_set_auxinput3_interval_mo ( id_id , auxinput3_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput3_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput3_interval_mo(id_id) = auxinput3_interval_mo
  RETURN
END SUBROUTINE nl_set_auxinput3_interval_mo
SUBROUTINE nl_set_auxinput3_interval_d ( id_id , auxinput3_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput3_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput3_interval_d(id_id) = auxinput3_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput3_interval_d
SUBROUTINE nl_set_auxinput3_interval_h ( id_id , auxinput3_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput3_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput3_interval_h(id_id) = auxinput3_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput3_interval_h
SUBROUTINE nl_set_auxinput3_interval_m ( id_id , auxinput3_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput3_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput3_interval_m(id_id) = auxinput3_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput3_interval_m
SUBROUTINE nl_set_auxinput3_interval_s ( id_id , auxinput3_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput3_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput3_interval_s(id_id) = auxinput3_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput3_interval_s
SUBROUTINE nl_set_auxinput3_interval ( id_id , auxinput3_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput3_interval
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput3_interval(id_id) = auxinput3_interval
  RETURN
END SUBROUTINE nl_set_auxinput3_interval
SUBROUTINE nl_set_auxinput4_interval_mo ( id_id , auxinput4_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput4_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput4_interval_mo(id_id) = auxinput4_interval_mo
  RETURN
END SUBROUTINE nl_set_auxinput4_interval_mo
SUBROUTINE nl_set_auxinput4_interval_d ( id_id , auxinput4_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput4_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput4_interval_d(id_id) = auxinput4_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput4_interval_d
SUBROUTINE nl_set_auxinput4_interval_h ( id_id , auxinput4_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput4_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput4_interval_h(id_id) = auxinput4_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput4_interval_h
SUBROUTINE nl_set_auxinput4_interval_m ( id_id , auxinput4_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput4_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput4_interval_m(id_id) = auxinput4_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput4_interval_m
SUBROUTINE nl_set_auxinput4_interval_s ( id_id , auxinput4_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput4_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput4_interval_s(id_id) = auxinput4_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput4_interval_s
SUBROUTINE nl_set_auxinput4_interval ( id_id , auxinput4_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput4_interval
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput4_interval(id_id) = auxinput4_interval
  RETURN
END SUBROUTINE nl_set_auxinput4_interval
SUBROUTINE nl_set_auxinput5_interval_mo ( id_id , auxinput5_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput5_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput5_interval_mo(id_id) = auxinput5_interval_mo
  RETURN
END SUBROUTINE nl_set_auxinput5_interval_mo
SUBROUTINE nl_set_auxinput5_interval_d ( id_id , auxinput5_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput5_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput5_interval_d(id_id) = auxinput5_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput5_interval_d
SUBROUTINE nl_set_auxinput5_interval_h ( id_id , auxinput5_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput5_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput5_interval_h(id_id) = auxinput5_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput5_interval_h
SUBROUTINE nl_set_auxinput5_interval_m ( id_id , auxinput5_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput5_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput5_interval_m(id_id) = auxinput5_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput5_interval_m
SUBROUTINE nl_set_auxinput5_interval_s ( id_id , auxinput5_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput5_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput5_interval_s(id_id) = auxinput5_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput5_interval_s
SUBROUTINE nl_set_auxinput5_interval ( id_id , auxinput5_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput5_interval
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput5_interval(id_id) = auxinput5_interval
  RETURN
END SUBROUTINE nl_set_auxinput5_interval
SUBROUTINE nl_set_auxinput6_interval_mo ( id_id , auxinput6_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput6_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput6_interval_mo(id_id) = auxinput6_interval_mo
  RETURN
END SUBROUTINE nl_set_auxinput6_interval_mo
SUBROUTINE nl_set_auxinput6_interval_d ( id_id , auxinput6_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput6_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput6_interval_d(id_id) = auxinput6_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput6_interval_d
SUBROUTINE nl_set_auxinput6_interval_h ( id_id , auxinput6_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput6_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput6_interval_h(id_id) = auxinput6_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput6_interval_h
SUBROUTINE nl_set_auxinput6_interval_m ( id_id , auxinput6_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput6_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput6_interval_m(id_id) = auxinput6_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput6_interval_m
SUBROUTINE nl_set_auxinput6_interval_s ( id_id , auxinput6_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput6_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput6_interval_s(id_id) = auxinput6_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput6_interval_s
SUBROUTINE nl_set_auxinput6_interval ( id_id , auxinput6_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput6_interval
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput6_interval(id_id) = auxinput6_interval
  RETURN
END SUBROUTINE nl_set_auxinput6_interval
SUBROUTINE nl_set_auxinput7_interval_mo ( id_id , auxinput7_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput7_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput7_interval_mo(id_id) = auxinput7_interval_mo
  RETURN
END SUBROUTINE nl_set_auxinput7_interval_mo
SUBROUTINE nl_set_auxinput7_interval_d ( id_id , auxinput7_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput7_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput7_interval_d(id_id) = auxinput7_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput7_interval_d
SUBROUTINE nl_set_auxinput7_interval_h ( id_id , auxinput7_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput7_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput7_interval_h(id_id) = auxinput7_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput7_interval_h
SUBROUTINE nl_set_auxinput7_interval_m ( id_id , auxinput7_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput7_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput7_interval_m(id_id) = auxinput7_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput7_interval_m
SUBROUTINE nl_set_auxinput7_interval_s ( id_id , auxinput7_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput7_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput7_interval_s(id_id) = auxinput7_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput7_interval_s
SUBROUTINE nl_set_auxinput7_interval ( id_id , auxinput7_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput7_interval
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput7_interval(id_id) = auxinput7_interval
  RETURN
END SUBROUTINE nl_set_auxinput7_interval
SUBROUTINE nl_set_auxinput8_interval_mo ( id_id , auxinput8_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput8_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput8_interval_mo(id_id) = auxinput8_interval_mo
  RETURN
END SUBROUTINE nl_set_auxinput8_interval_mo
SUBROUTINE nl_set_auxinput8_interval_d ( id_id , auxinput8_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput8_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput8_interval_d(id_id) = auxinput8_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput8_interval_d
SUBROUTINE nl_set_auxinput8_interval_h ( id_id , auxinput8_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput8_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput8_interval_h(id_id) = auxinput8_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput8_interval_h
SUBROUTINE nl_set_auxinput8_interval_m ( id_id , auxinput8_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput8_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput8_interval_m(id_id) = auxinput8_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput8_interval_m
SUBROUTINE nl_set_auxinput8_interval_s ( id_id , auxinput8_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput8_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput8_interval_s(id_id) = auxinput8_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput8_interval_s
SUBROUTINE nl_set_auxinput8_interval ( id_id , auxinput8_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput8_interval
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput8_interval(id_id) = auxinput8_interval
  RETURN
END SUBROUTINE nl_set_auxinput8_interval
SUBROUTINE nl_set_sgfdda_interval_mo ( id_id , sgfdda_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sgfdda_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%sgfdda_interval_mo(id_id) = sgfdda_interval_mo
  RETURN
END SUBROUTINE nl_set_sgfdda_interval_mo
SUBROUTINE nl_set_sgfdda_interval_d ( id_id , sgfdda_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sgfdda_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%sgfdda_interval_d(id_id) = sgfdda_interval_d
  RETURN
END SUBROUTINE nl_set_sgfdda_interval_d
SUBROUTINE nl_set_sgfdda_interval_h ( id_id , sgfdda_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sgfdda_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%sgfdda_interval_h(id_id) = sgfdda_interval_h
  RETURN
END SUBROUTINE nl_set_sgfdda_interval_h
SUBROUTINE nl_set_sgfdda_interval_m ( id_id , sgfdda_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sgfdda_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%sgfdda_interval_m(id_id) = sgfdda_interval_m
  RETURN
END SUBROUTINE nl_set_sgfdda_interval_m
SUBROUTINE nl_set_sgfdda_interval_s ( id_id , sgfdda_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sgfdda_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%sgfdda_interval_s(id_id) = sgfdda_interval_s
  RETURN
END SUBROUTINE nl_set_sgfdda_interval_s
SUBROUTINE nl_set_sgfdda_interval ( id_id , sgfdda_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sgfdda_interval
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%sgfdda_interval(id_id) = sgfdda_interval
  RETURN
END SUBROUTINE nl_set_sgfdda_interval
SUBROUTINE nl_set_gfdda_interval_mo ( id_id , gfdda_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: gfdda_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%gfdda_interval_mo(id_id) = gfdda_interval_mo
  RETURN
END SUBROUTINE nl_set_gfdda_interval_mo
SUBROUTINE nl_set_gfdda_interval_d ( id_id , gfdda_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: gfdda_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%gfdda_interval_d(id_id) = gfdda_interval_d
  RETURN
END SUBROUTINE nl_set_gfdda_interval_d
SUBROUTINE nl_set_gfdda_interval_h ( id_id , gfdda_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: gfdda_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%gfdda_interval_h(id_id) = gfdda_interval_h
  RETURN
END SUBROUTINE nl_set_gfdda_interval_h
SUBROUTINE nl_set_gfdda_interval_m ( id_id , gfdda_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: gfdda_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%gfdda_interval_m(id_id) = gfdda_interval_m
  RETURN
END SUBROUTINE nl_set_gfdda_interval_m
SUBROUTINE nl_set_gfdda_interval_s ( id_id , gfdda_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: gfdda_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%gfdda_interval_s(id_id) = gfdda_interval_s
  RETURN
END SUBROUTINE nl_set_gfdda_interval_s
SUBROUTINE nl_set_gfdda_interval ( id_id , gfdda_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: gfdda_interval
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%gfdda_interval(id_id) = gfdda_interval
  RETURN
END SUBROUTINE nl_set_gfdda_interval
SUBROUTINE nl_set_auxinput11_interval_mo ( id_id , auxinput11_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput11_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput11_interval_mo(id_id) = auxinput11_interval_mo
  RETURN
END SUBROUTINE nl_set_auxinput11_interval_mo
SUBROUTINE nl_set_auxinput11_interval_d ( id_id , auxinput11_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput11_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput11_interval_d(id_id) = auxinput11_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput11_interval_d
SUBROUTINE nl_set_auxinput11_interval_h ( id_id , auxinput11_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput11_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput11_interval_h(id_id) = auxinput11_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput11_interval_h
SUBROUTINE nl_set_auxinput11_interval_m ( id_id , auxinput11_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput11_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput11_interval_m(id_id) = auxinput11_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput11_interval_m
SUBROUTINE nl_set_auxinput11_interval_s ( id_id , auxinput11_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput11_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput11_interval_s(id_id) = auxinput11_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput11_interval_s
SUBROUTINE nl_set_auxinput11_interval ( id_id , auxinput11_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput11_interval
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput11_interval(id_id) = auxinput11_interval
  RETURN
END SUBROUTINE nl_set_auxinput11_interval
SUBROUTINE nl_set_restart_interval_mo ( id_id , restart_interval_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: restart_interval_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%restart_interval_mo = restart_interval_mo 
  RETURN
END SUBROUTINE nl_set_restart_interval_mo
SUBROUTINE nl_set_restart_interval_d ( id_id , restart_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: restart_interval_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%restart_interval_d = restart_interval_d 
  RETURN
END SUBROUTINE nl_set_restart_interval_d
SUBROUTINE nl_set_restart_interval_h ( id_id , restart_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: restart_interval_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%restart_interval_h = restart_interval_h 
  RETURN
END SUBROUTINE nl_set_restart_interval_h
SUBROUTINE nl_set_restart_interval_m ( id_id , restart_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: restart_interval_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%restart_interval_m = restart_interval_m 
  RETURN
END SUBROUTINE nl_set_restart_interval_m
SUBROUTINE nl_set_restart_interval_s ( id_id , restart_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: restart_interval_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%restart_interval_s = restart_interval_s 
  RETURN
END SUBROUTINE nl_set_restart_interval_s
SUBROUTINE nl_set_history_begin_y ( id_id , history_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%history_begin_y(id_id) = history_begin_y
  RETURN
END SUBROUTINE nl_set_history_begin_y
SUBROUTINE nl_set_history_begin_mo ( id_id , history_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%history_begin_mo(id_id) = history_begin_mo
  RETURN
END SUBROUTINE nl_set_history_begin_mo
SUBROUTINE nl_set_history_begin_d ( id_id , history_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%history_begin_d(id_id) = history_begin_d
  RETURN
END SUBROUTINE nl_set_history_begin_d
SUBROUTINE nl_set_history_begin_h ( id_id , history_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%history_begin_h(id_id) = history_begin_h
  RETURN
END SUBROUTINE nl_set_history_begin_h
SUBROUTINE nl_set_history_begin_m ( id_id , history_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%history_begin_m(id_id) = history_begin_m
  RETURN
END SUBROUTINE nl_set_history_begin_m
SUBROUTINE nl_set_history_begin_s ( id_id , history_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%history_begin_s(id_id) = history_begin_s
  RETURN
END SUBROUTINE nl_set_history_begin_s
SUBROUTINE nl_set_inputout_begin_y ( id_id , inputout_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%inputout_begin_y(id_id) = inputout_begin_y
  RETURN
END SUBROUTINE nl_set_inputout_begin_y
SUBROUTINE nl_set_inputout_begin_mo ( id_id , inputout_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%inputout_begin_mo(id_id) = inputout_begin_mo
  RETURN
END SUBROUTINE nl_set_inputout_begin_mo
SUBROUTINE nl_set_inputout_begin_d ( id_id , inputout_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%inputout_begin_d(id_id) = inputout_begin_d
  RETURN
END SUBROUTINE nl_set_inputout_begin_d
SUBROUTINE nl_set_inputout_begin_h ( id_id , inputout_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%inputout_begin_h(id_id) = inputout_begin_h
  RETURN
END SUBROUTINE nl_set_inputout_begin_h
SUBROUTINE nl_set_inputout_begin_m ( id_id , inputout_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%inputout_begin_m(id_id) = inputout_begin_m
  RETURN
END SUBROUTINE nl_set_inputout_begin_m
SUBROUTINE nl_set_inputout_begin_s ( id_id , inputout_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%inputout_begin_s(id_id) = inputout_begin_s
  RETURN
END SUBROUTINE nl_set_inputout_begin_s
SUBROUTINE nl_set_auxhist1_begin_y ( id_id , auxhist1_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist1_begin_y(id_id) = auxhist1_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist1_begin_y
SUBROUTINE nl_set_auxhist1_begin_mo ( id_id , auxhist1_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist1_begin_mo(id_id) = auxhist1_begin_mo
  RETURN
END SUBROUTINE nl_set_auxhist1_begin_mo
SUBROUTINE nl_set_auxhist1_begin_d ( id_id , auxhist1_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist1_begin_d(id_id) = auxhist1_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist1_begin_d
SUBROUTINE nl_set_auxhist1_begin_h ( id_id , auxhist1_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist1_begin_h(id_id) = auxhist1_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist1_begin_h
SUBROUTINE nl_set_auxhist1_begin_m ( id_id , auxhist1_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist1_begin_m(id_id) = auxhist1_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist1_begin_m
SUBROUTINE nl_set_auxhist1_begin_s ( id_id , auxhist1_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist1_begin_s(id_id) = auxhist1_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist1_begin_s
SUBROUTINE nl_set_auxhist2_begin_y ( id_id , auxhist2_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist2_begin_y(id_id) = auxhist2_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist2_begin_y
SUBROUTINE nl_set_auxhist2_begin_mo ( id_id , auxhist2_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist2_begin_mo(id_id) = auxhist2_begin_mo
  RETURN
END SUBROUTINE nl_set_auxhist2_begin_mo
SUBROUTINE nl_set_auxhist2_begin_d ( id_id , auxhist2_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist2_begin_d(id_id) = auxhist2_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist2_begin_d
SUBROUTINE nl_set_auxhist2_begin_h ( id_id , auxhist2_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist2_begin_h(id_id) = auxhist2_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist2_begin_h
SUBROUTINE nl_set_auxhist2_begin_m ( id_id , auxhist2_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist2_begin_m(id_id) = auxhist2_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist2_begin_m
SUBROUTINE nl_set_auxhist2_begin_s ( id_id , auxhist2_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist2_begin_s(id_id) = auxhist2_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist2_begin_s
SUBROUTINE nl_set_auxhist3_begin_y ( id_id , auxhist3_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist3_begin_y(id_id) = auxhist3_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist3_begin_y
SUBROUTINE nl_set_auxhist3_begin_mo ( id_id , auxhist3_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist3_begin_mo(id_id) = auxhist3_begin_mo
  RETURN
END SUBROUTINE nl_set_auxhist3_begin_mo
SUBROUTINE nl_set_auxhist3_begin_d ( id_id , auxhist3_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist3_begin_d(id_id) = auxhist3_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist3_begin_d
SUBROUTINE nl_set_auxhist3_begin_h ( id_id , auxhist3_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist3_begin_h(id_id) = auxhist3_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist3_begin_h
SUBROUTINE nl_set_auxhist3_begin_m ( id_id , auxhist3_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist3_begin_m(id_id) = auxhist3_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist3_begin_m
SUBROUTINE nl_set_auxhist3_begin_s ( id_id , auxhist3_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist3_begin_s(id_id) = auxhist3_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist3_begin_s
SUBROUTINE nl_set_auxhist4_begin_y ( id_id , auxhist4_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist4_begin_y(id_id) = auxhist4_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist4_begin_y
SUBROUTINE nl_set_auxhist4_begin_mo ( id_id , auxhist4_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist4_begin_mo(id_id) = auxhist4_begin_mo
  RETURN
END SUBROUTINE nl_set_auxhist4_begin_mo
SUBROUTINE nl_set_auxhist4_begin_d ( id_id , auxhist4_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist4_begin_d(id_id) = auxhist4_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist4_begin_d
SUBROUTINE nl_set_auxhist4_begin_h ( id_id , auxhist4_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist4_begin_h(id_id) = auxhist4_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist4_begin_h
SUBROUTINE nl_set_auxhist4_begin_m ( id_id , auxhist4_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist4_begin_m(id_id) = auxhist4_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist4_begin_m
SUBROUTINE nl_set_auxhist4_begin_s ( id_id , auxhist4_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist4_begin_s(id_id) = auxhist4_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist4_begin_s
SUBROUTINE nl_set_auxhist5_begin_y ( id_id , auxhist5_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist5_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist5_begin_y(id_id) = auxhist5_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist5_begin_y
SUBROUTINE nl_set_auxhist5_begin_mo ( id_id , auxhist5_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist5_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist5_begin_mo(id_id) = auxhist5_begin_mo
  RETURN
END SUBROUTINE nl_set_auxhist5_begin_mo
SUBROUTINE nl_set_auxhist5_begin_d ( id_id , auxhist5_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist5_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist5_begin_d(id_id) = auxhist5_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist5_begin_d
SUBROUTINE nl_set_auxhist5_begin_h ( id_id , auxhist5_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist5_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist5_begin_h(id_id) = auxhist5_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist5_begin_h
SUBROUTINE nl_set_auxhist5_begin_m ( id_id , auxhist5_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist5_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist5_begin_m(id_id) = auxhist5_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist5_begin_m
SUBROUTINE nl_set_auxhist5_begin_s ( id_id , auxhist5_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist5_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist5_begin_s(id_id) = auxhist5_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist5_begin_s
SUBROUTINE nl_set_auxhist6_begin_y ( id_id , auxhist6_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist6_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist6_begin_y(id_id) = auxhist6_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist6_begin_y
SUBROUTINE nl_set_auxhist6_begin_mo ( id_id , auxhist6_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist6_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist6_begin_mo(id_id) = auxhist6_begin_mo
  RETURN
END SUBROUTINE nl_set_auxhist6_begin_mo
SUBROUTINE nl_set_auxhist6_begin_d ( id_id , auxhist6_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist6_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist6_begin_d(id_id) = auxhist6_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist6_begin_d
SUBROUTINE nl_set_auxhist6_begin_h ( id_id , auxhist6_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist6_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist6_begin_h(id_id) = auxhist6_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist6_begin_h
SUBROUTINE nl_set_auxhist6_begin_m ( id_id , auxhist6_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist6_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist6_begin_m(id_id) = auxhist6_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist6_begin_m
SUBROUTINE nl_set_auxhist6_begin_s ( id_id , auxhist6_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist6_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist6_begin_s(id_id) = auxhist6_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist6_begin_s
SUBROUTINE nl_set_auxhist7_begin_y ( id_id , auxhist7_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist7_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist7_begin_y(id_id) = auxhist7_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist7_begin_y
SUBROUTINE nl_set_auxhist7_begin_mo ( id_id , auxhist7_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist7_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist7_begin_mo(id_id) = auxhist7_begin_mo
  RETURN
END SUBROUTINE nl_set_auxhist7_begin_mo
SUBROUTINE nl_set_auxhist7_begin_d ( id_id , auxhist7_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist7_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist7_begin_d(id_id) = auxhist7_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist7_begin_d
SUBROUTINE nl_set_auxhist7_begin_h ( id_id , auxhist7_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist7_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist7_begin_h(id_id) = auxhist7_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist7_begin_h
SUBROUTINE nl_set_auxhist7_begin_m ( id_id , auxhist7_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist7_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist7_begin_m(id_id) = auxhist7_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist7_begin_m
SUBROUTINE nl_set_auxhist7_begin_s ( id_id , auxhist7_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist7_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist7_begin_s(id_id) = auxhist7_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist7_begin_s
SUBROUTINE nl_set_auxhist8_begin_y ( id_id , auxhist8_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist8_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist8_begin_y(id_id) = auxhist8_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist8_begin_y
SUBROUTINE nl_set_auxhist8_begin_mo ( id_id , auxhist8_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist8_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist8_begin_mo(id_id) = auxhist8_begin_mo
  RETURN
END SUBROUTINE nl_set_auxhist8_begin_mo
SUBROUTINE nl_set_auxhist8_begin_d ( id_id , auxhist8_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist8_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist8_begin_d(id_id) = auxhist8_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist8_begin_d
SUBROUTINE nl_set_auxhist8_begin_h ( id_id , auxhist8_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist8_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist8_begin_h(id_id) = auxhist8_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist8_begin_h
SUBROUTINE nl_set_auxhist8_begin_m ( id_id , auxhist8_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist8_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist8_begin_m(id_id) = auxhist8_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist8_begin_m
SUBROUTINE nl_set_auxhist8_begin_s ( id_id , auxhist8_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist8_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist8_begin_s(id_id) = auxhist8_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist8_begin_s
SUBROUTINE nl_set_auxhist9_begin_y ( id_id , auxhist9_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist9_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist9_begin_y(id_id) = auxhist9_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist9_begin_y
SUBROUTINE nl_set_auxhist9_begin_mo ( id_id , auxhist9_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist9_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist9_begin_mo(id_id) = auxhist9_begin_mo
  RETURN
END SUBROUTINE nl_set_auxhist9_begin_mo
SUBROUTINE nl_set_auxhist9_begin_d ( id_id , auxhist9_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist9_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist9_begin_d(id_id) = auxhist9_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist9_begin_d
SUBROUTINE nl_set_auxhist9_begin_h ( id_id , auxhist9_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist9_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist9_begin_h(id_id) = auxhist9_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist9_begin_h
SUBROUTINE nl_set_auxhist9_begin_m ( id_id , auxhist9_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist9_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist9_begin_m(id_id) = auxhist9_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist9_begin_m
SUBROUTINE nl_set_auxhist9_begin_s ( id_id , auxhist9_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist9_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist9_begin_s(id_id) = auxhist9_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist9_begin_s
SUBROUTINE nl_set_auxhist10_begin_y ( id_id , auxhist10_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist10_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist10_begin_y(id_id) = auxhist10_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist10_begin_y
SUBROUTINE nl_set_auxhist10_begin_mo ( id_id , auxhist10_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist10_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist10_begin_mo(id_id) = auxhist10_begin_mo
  RETURN
END SUBROUTINE nl_set_auxhist10_begin_mo
SUBROUTINE nl_set_auxhist10_begin_d ( id_id , auxhist10_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist10_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist10_begin_d(id_id) = auxhist10_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist10_begin_d
SUBROUTINE nl_set_auxhist10_begin_h ( id_id , auxhist10_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist10_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist10_begin_h(id_id) = auxhist10_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist10_begin_h
SUBROUTINE nl_set_auxhist10_begin_m ( id_id , auxhist10_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist10_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist10_begin_m(id_id) = auxhist10_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist10_begin_m
SUBROUTINE nl_set_auxhist10_begin_s ( id_id , auxhist10_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist10_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist10_begin_s(id_id) = auxhist10_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist10_begin_s
SUBROUTINE nl_set_auxhist11_begin_y ( id_id , auxhist11_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist11_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist11_begin_y(id_id) = auxhist11_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist11_begin_y
SUBROUTINE nl_set_auxhist11_begin_mo ( id_id , auxhist11_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist11_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist11_begin_mo(id_id) = auxhist11_begin_mo
  RETURN
END SUBROUTINE nl_set_auxhist11_begin_mo
SUBROUTINE nl_set_auxhist11_begin_d ( id_id , auxhist11_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist11_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist11_begin_d(id_id) = auxhist11_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist11_begin_d
SUBROUTINE nl_set_auxhist11_begin_h ( id_id , auxhist11_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist11_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist11_begin_h(id_id) = auxhist11_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist11_begin_h
SUBROUTINE nl_set_auxhist11_begin_m ( id_id , auxhist11_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist11_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist11_begin_m(id_id) = auxhist11_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist11_begin_m
SUBROUTINE nl_set_auxhist11_begin_s ( id_id , auxhist11_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist11_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxhist11_begin_s(id_id) = auxhist11_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist11_begin_s
SUBROUTINE nl_set_auxinput1_begin_y ( id_id , auxinput1_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput1_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput1_begin_y(id_id) = auxinput1_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput1_begin_y
SUBROUTINE nl_set_auxinput1_begin_mo ( id_id , auxinput1_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput1_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput1_begin_mo(id_id) = auxinput1_begin_mo
  RETURN
END SUBROUTINE nl_set_auxinput1_begin_mo
SUBROUTINE nl_set_auxinput1_begin_d ( id_id , auxinput1_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput1_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput1_begin_d(id_id) = auxinput1_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput1_begin_d
SUBROUTINE nl_set_auxinput1_begin_h ( id_id , auxinput1_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput1_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput1_begin_h(id_id) = auxinput1_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput1_begin_h
SUBROUTINE nl_set_auxinput1_begin_m ( id_id , auxinput1_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput1_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput1_begin_m(id_id) = auxinput1_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput1_begin_m
SUBROUTINE nl_set_auxinput1_begin_s ( id_id , auxinput1_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput1_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput1_begin_s(id_id) = auxinput1_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput1_begin_s
SUBROUTINE nl_set_auxinput2_begin_y ( id_id , auxinput2_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput2_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput2_begin_y(id_id) = auxinput2_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput2_begin_y
SUBROUTINE nl_set_auxinput2_begin_mo ( id_id , auxinput2_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput2_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput2_begin_mo(id_id) = auxinput2_begin_mo
  RETURN
END SUBROUTINE nl_set_auxinput2_begin_mo
SUBROUTINE nl_set_auxinput2_begin_d ( id_id , auxinput2_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput2_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput2_begin_d(id_id) = auxinput2_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput2_begin_d
SUBROUTINE nl_set_auxinput2_begin_h ( id_id , auxinput2_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput2_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput2_begin_h(id_id) = auxinput2_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput2_begin_h
SUBROUTINE nl_set_auxinput2_begin_m ( id_id , auxinput2_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput2_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput2_begin_m(id_id) = auxinput2_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput2_begin_m
SUBROUTINE nl_set_auxinput2_begin_s ( id_id , auxinput2_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput2_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput2_begin_s(id_id) = auxinput2_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput2_begin_s
SUBROUTINE nl_set_auxinput3_begin_y ( id_id , auxinput3_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput3_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput3_begin_y(id_id) = auxinput3_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput3_begin_y
SUBROUTINE nl_set_auxinput3_begin_mo ( id_id , auxinput3_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput3_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput3_begin_mo(id_id) = auxinput3_begin_mo
  RETURN
END SUBROUTINE nl_set_auxinput3_begin_mo
SUBROUTINE nl_set_auxinput3_begin_d ( id_id , auxinput3_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput3_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput3_begin_d(id_id) = auxinput3_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput3_begin_d
SUBROUTINE nl_set_auxinput3_begin_h ( id_id , auxinput3_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput3_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput3_begin_h(id_id) = auxinput3_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput3_begin_h
SUBROUTINE nl_set_auxinput3_begin_m ( id_id , auxinput3_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput3_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput3_begin_m(id_id) = auxinput3_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput3_begin_m
SUBROUTINE nl_set_auxinput3_begin_s ( id_id , auxinput3_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput3_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput3_begin_s(id_id) = auxinput3_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput3_begin_s
SUBROUTINE nl_set_auxinput4_begin_y ( id_id , auxinput4_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput4_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput4_begin_y(id_id) = auxinput4_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput4_begin_y
SUBROUTINE nl_set_auxinput4_begin_mo ( id_id , auxinput4_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput4_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput4_begin_mo(id_id) = auxinput4_begin_mo
  RETURN
END SUBROUTINE nl_set_auxinput4_begin_mo
SUBROUTINE nl_set_auxinput4_begin_d ( id_id , auxinput4_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput4_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput4_begin_d(id_id) = auxinput4_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput4_begin_d
SUBROUTINE nl_set_auxinput4_begin_h ( id_id , auxinput4_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput4_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput4_begin_h(id_id) = auxinput4_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput4_begin_h
SUBROUTINE nl_set_auxinput4_begin_m ( id_id , auxinput4_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput4_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput4_begin_m(id_id) = auxinput4_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput4_begin_m
SUBROUTINE nl_set_auxinput4_begin_s ( id_id , auxinput4_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput4_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput4_begin_s(id_id) = auxinput4_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput4_begin_s
SUBROUTINE nl_set_auxinput5_begin_y ( id_id , auxinput5_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput5_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput5_begin_y(id_id) = auxinput5_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput5_begin_y
SUBROUTINE nl_set_auxinput5_begin_mo ( id_id , auxinput5_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput5_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput5_begin_mo(id_id) = auxinput5_begin_mo
  RETURN
END SUBROUTINE nl_set_auxinput5_begin_mo
SUBROUTINE nl_set_auxinput5_begin_d ( id_id , auxinput5_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput5_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput5_begin_d(id_id) = auxinput5_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput5_begin_d
SUBROUTINE nl_set_auxinput5_begin_h ( id_id , auxinput5_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput5_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput5_begin_h(id_id) = auxinput5_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput5_begin_h
SUBROUTINE nl_set_auxinput5_begin_m ( id_id , auxinput5_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput5_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput5_begin_m(id_id) = auxinput5_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput5_begin_m
SUBROUTINE nl_set_auxinput5_begin_s ( id_id , auxinput5_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput5_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput5_begin_s(id_id) = auxinput5_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput5_begin_s
SUBROUTINE nl_set_auxinput6_begin_y ( id_id , auxinput6_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput6_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput6_begin_y(id_id) = auxinput6_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput6_begin_y
SUBROUTINE nl_set_auxinput6_begin_mo ( id_id , auxinput6_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput6_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput6_begin_mo(id_id) = auxinput6_begin_mo
  RETURN
END SUBROUTINE nl_set_auxinput6_begin_mo
SUBROUTINE nl_set_auxinput6_begin_d ( id_id , auxinput6_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput6_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput6_begin_d(id_id) = auxinput6_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput6_begin_d
SUBROUTINE nl_set_auxinput6_begin_h ( id_id , auxinput6_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput6_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput6_begin_h(id_id) = auxinput6_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput6_begin_h
SUBROUTINE nl_set_auxinput6_begin_m ( id_id , auxinput6_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput6_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput6_begin_m(id_id) = auxinput6_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput6_begin_m
SUBROUTINE nl_set_auxinput6_begin_s ( id_id , auxinput6_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput6_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput6_begin_s(id_id) = auxinput6_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput6_begin_s
SUBROUTINE nl_set_auxinput7_begin_y ( id_id , auxinput7_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput7_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput7_begin_y(id_id) = auxinput7_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput7_begin_y
SUBROUTINE nl_set_auxinput7_begin_mo ( id_id , auxinput7_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput7_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput7_begin_mo(id_id) = auxinput7_begin_mo
  RETURN
END SUBROUTINE nl_set_auxinput7_begin_mo
SUBROUTINE nl_set_auxinput7_begin_d ( id_id , auxinput7_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput7_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput7_begin_d(id_id) = auxinput7_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput7_begin_d
SUBROUTINE nl_set_auxinput7_begin_h ( id_id , auxinput7_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput7_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput7_begin_h(id_id) = auxinput7_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput7_begin_h
SUBROUTINE nl_set_auxinput7_begin_m ( id_id , auxinput7_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput7_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput7_begin_m(id_id) = auxinput7_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput7_begin_m
SUBROUTINE nl_set_auxinput7_begin_s ( id_id , auxinput7_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput7_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput7_begin_s(id_id) = auxinput7_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput7_begin_s
SUBROUTINE nl_set_auxinput8_begin_y ( id_id , auxinput8_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput8_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput8_begin_y(id_id) = auxinput8_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput8_begin_y
SUBROUTINE nl_set_auxinput8_begin_mo ( id_id , auxinput8_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput8_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput8_begin_mo(id_id) = auxinput8_begin_mo
  RETURN
END SUBROUTINE nl_set_auxinput8_begin_mo
SUBROUTINE nl_set_auxinput8_begin_d ( id_id , auxinput8_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput8_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput8_begin_d(id_id) = auxinput8_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput8_begin_d
SUBROUTINE nl_set_auxinput8_begin_h ( id_id , auxinput8_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput8_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput8_begin_h(id_id) = auxinput8_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput8_begin_h
SUBROUTINE nl_set_auxinput8_begin_m ( id_id , auxinput8_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput8_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput8_begin_m(id_id) = auxinput8_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput8_begin_m
SUBROUTINE nl_set_auxinput8_begin_s ( id_id , auxinput8_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput8_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput8_begin_s(id_id) = auxinput8_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput8_begin_s
SUBROUTINE nl_set_sgfdda_begin_y ( id_id , sgfdda_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sgfdda_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%sgfdda_begin_y(id_id) = sgfdda_begin_y
  RETURN
END SUBROUTINE nl_set_sgfdda_begin_y
SUBROUTINE nl_set_sgfdda_begin_mo ( id_id , sgfdda_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sgfdda_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%sgfdda_begin_mo(id_id) = sgfdda_begin_mo
  RETURN
END SUBROUTINE nl_set_sgfdda_begin_mo
SUBROUTINE nl_set_sgfdda_begin_d ( id_id , sgfdda_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sgfdda_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%sgfdda_begin_d(id_id) = sgfdda_begin_d
  RETURN
END SUBROUTINE nl_set_sgfdda_begin_d
SUBROUTINE nl_set_sgfdda_begin_h ( id_id , sgfdda_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sgfdda_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%sgfdda_begin_h(id_id) = sgfdda_begin_h
  RETURN
END SUBROUTINE nl_set_sgfdda_begin_h
SUBROUTINE nl_set_sgfdda_begin_m ( id_id , sgfdda_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sgfdda_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%sgfdda_begin_m(id_id) = sgfdda_begin_m
  RETURN
END SUBROUTINE nl_set_sgfdda_begin_m
SUBROUTINE nl_set_sgfdda_begin_s ( id_id , sgfdda_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sgfdda_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%sgfdda_begin_s(id_id) = sgfdda_begin_s
  RETURN
END SUBROUTINE nl_set_sgfdda_begin_s
SUBROUTINE nl_set_gfdda_begin_y ( id_id , gfdda_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: gfdda_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%gfdda_begin_y(id_id) = gfdda_begin_y
  RETURN
END SUBROUTINE nl_set_gfdda_begin_y
SUBROUTINE nl_set_gfdda_begin_mo ( id_id , gfdda_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: gfdda_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%gfdda_begin_mo(id_id) = gfdda_begin_mo
  RETURN
END SUBROUTINE nl_set_gfdda_begin_mo
SUBROUTINE nl_set_gfdda_begin_d ( id_id , gfdda_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: gfdda_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%gfdda_begin_d(id_id) = gfdda_begin_d
  RETURN
END SUBROUTINE nl_set_gfdda_begin_d
SUBROUTINE nl_set_gfdda_begin_h ( id_id , gfdda_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: gfdda_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%gfdda_begin_h(id_id) = gfdda_begin_h
  RETURN
END SUBROUTINE nl_set_gfdda_begin_h
SUBROUTINE nl_set_gfdda_begin_m ( id_id , gfdda_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: gfdda_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%gfdda_begin_m(id_id) = gfdda_begin_m
  RETURN
END SUBROUTINE nl_set_gfdda_begin_m
SUBROUTINE nl_set_gfdda_begin_s ( id_id , gfdda_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: gfdda_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%gfdda_begin_s(id_id) = gfdda_begin_s
  RETURN
END SUBROUTINE nl_set_gfdda_begin_s
SUBROUTINE nl_set_auxinput11_begin_y ( id_id , auxinput11_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput11_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput11_begin_y(id_id) = auxinput11_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput11_begin_y
SUBROUTINE nl_set_auxinput11_begin_mo ( id_id , auxinput11_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput11_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput11_begin_mo(id_id) = auxinput11_begin_mo
  RETURN
END SUBROUTINE nl_set_auxinput11_begin_mo
SUBROUTINE nl_set_auxinput11_begin_d ( id_id , auxinput11_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput11_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput11_begin_d(id_id) = auxinput11_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput11_begin_d
SUBROUTINE nl_set_auxinput11_begin_h ( id_id , auxinput11_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput11_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput11_begin_h(id_id) = auxinput11_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput11_begin_h
SUBROUTINE nl_set_auxinput11_begin_m ( id_id , auxinput11_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput11_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput11_begin_m(id_id) = auxinput11_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput11_begin_m
SUBROUTINE nl_set_auxinput11_begin_s ( id_id , auxinput11_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput11_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%auxinput11_begin_s(id_id) = auxinput11_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput11_begin_s
SUBROUTINE nl_set_restart_begin_y ( id_id , restart_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: restart_begin_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%restart_begin_y = restart_begin_y 
  RETURN
END SUBROUTINE nl_set_restart_begin_y
SUBROUTINE nl_set_restart_begin_mo ( id_id , restart_begin_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: restart_begin_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%restart_begin_mo = restart_begin_mo 
  RETURN
END SUBROUTINE nl_set_restart_begin_mo
SUBROUTINE nl_set_restart_begin_d ( id_id , restart_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: restart_begin_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%restart_begin_d = restart_begin_d 
  RETURN
END SUBROUTINE nl_set_restart_begin_d
SUBROUTINE nl_set_restart_begin_h ( id_id , restart_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: restart_begin_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%restart_begin_h = restart_begin_h 
  RETURN
END SUBROUTINE nl_set_restart_begin_h
SUBROUTINE nl_set_restart_begin_m ( id_id , restart_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: restart_begin_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%restart_begin_m = restart_begin_m 
  RETURN
END SUBROUTINE nl_set_restart_begin_m
SUBROUTINE nl_set_restart_begin_s ( id_id , restart_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: restart_begin_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%restart_begin_s = restart_begin_s 
  RETURN
END SUBROUTINE nl_set_restart_begin_s
SUBROUTINE nl_set_history_end_y ( id_id , history_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_end_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%history_end_y(id_id) = history_end_y
  RETURN
END SUBROUTINE nl_set_history_end_y
SUBROUTINE nl_set_history_end_mo ( id_id , history_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%history_end_mo(id_id) = history_end_mo
  RETURN
END SUBROUTINE nl_set_history_end_mo
SUBROUTINE nl_set_history_end_d ( id_id , history_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_end_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%history_end_d(id_id) = history_end_d
  RETURN
END SUBROUTINE nl_set_history_end_d
SUBROUTINE nl_set_history_end_h ( id_id , history_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_end_h
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%history_end_h(id_id) = history_end_h
  RETURN
END SUBROUTINE nl_set_history_end_h
SUBROUTINE nl_set_history_end_m ( id_id , history_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_end_m
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%history_end_m(id_id) = history_end_m
  RETURN
END SUBROUTINE nl_set_history_end_m
SUBROUTINE nl_set_history_end_s ( id_id , history_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_end_s
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%history_end_s(id_id) = history_end_s
  RETURN
END SUBROUTINE nl_set_history_end_s
SUBROUTINE nl_set_inputout_end_y ( id_id , inputout_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_end_y
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%inputout_end_y(id_id) = inputout_end_y
  RETURN
END SUBROUTINE nl_set_inputout_end_y
SUBROUTINE nl_set_inputout_end_mo ( id_id , inputout_end_mo )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_end_mo
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%inputout_end_mo(id_id) = inputout_end_mo
  RETURN
END SUBROUTINE nl_set_inputout_end_mo
SUBROUTINE nl_set_inputout_end_d ( id_id , inputout_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_end_d
  INTEGER id_id
  CHARACTER*80 emess
  model_config_rec%inputout_end_d(id_id) = inputout_end_d
  RETURN
END SUBROUTINE nl_set_inputout_end_d
!ENDOFREGISTRYGENERATEDINCLUDE
