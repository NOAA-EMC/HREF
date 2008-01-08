!STARTOFREGISTRYGENERATEDINCLUDE 'inc/namelist_defines.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
integer    :: first_item_in_struct
integer :: run_days
integer :: run_hours
integer :: run_minutes
integer :: run_seconds
integer , DIMENSION(max_domains) :: start_year
integer , DIMENSION(max_domains) :: start_month
integer , DIMENSION(max_domains) :: start_day
integer , DIMENSION(max_domains) :: start_hour
integer , DIMENSION(max_domains) :: start_minute
integer , DIMENSION(max_domains) :: start_second
integer , DIMENSION(max_domains) :: end_year
integer , DIMENSION(max_domains) :: end_month
integer , DIMENSION(max_domains) :: end_day
integer , DIMENSION(max_domains) :: end_hour
integer , DIMENSION(max_domains) :: end_minute
integer , DIMENSION(max_domains) :: end_second
integer :: interval_seconds
logical , DIMENSION(max_domains) :: input_from_file
integer , DIMENSION(max_domains) :: fine_input_stream
logical , DIMENSION(max_domains) :: input_from_hires
character*256 :: rsmas_data_path
integer , DIMENSION(max_domains) :: history_interval
integer , DIMENSION(max_domains) :: frames_per_outfile
integer , DIMENSION(max_domains) :: frames_per_auxhist1
integer , DIMENSION(max_domains) :: frames_per_auxhist2
integer , DIMENSION(max_domains) :: frames_per_auxhist3
integer , DIMENSION(max_domains) :: frames_per_auxhist4
integer , DIMENSION(max_domains) :: frames_per_auxhist5
integer , DIMENSION(max_domains) :: frames_per_auxhist6
integer , DIMENSION(max_domains) :: frames_per_auxhist7
integer , DIMENSION(max_domains) :: frames_per_auxhist8
integer , DIMENSION(max_domains) :: frames_per_auxhist9
integer , DIMENSION(max_domains) :: frames_per_auxhist10
integer , DIMENSION(max_domains) :: frames_per_auxhist11
logical :: restart
integer :: restart_interval
integer :: io_form_input
integer :: io_form_history
integer :: io_form_restart
integer :: io_form_boundary
integer :: debug_level
logical :: self_test_domain
character*256 :: history_outname
character*256 :: auxhist1_outname
character*256 :: auxhist2_outname
character*256 :: auxhist3_outname
character*256 :: auxhist4_outname
character*256 :: auxhist5_outname
character*256 :: auxhist6_outname
character*256 :: auxhist7_outname
character*256 :: auxhist8_outname
character*256 :: auxhist9_outname
character*256 :: auxhist10_outname
character*256 :: auxhist11_outname
character*256 :: history_inname
character*256 :: auxhist1_inname
character*256 :: auxhist2_inname
character*256 :: auxhist3_inname
character*256 :: auxhist4_inname
character*256 :: auxhist5_inname
character*256 :: auxhist6_inname
character*256 :: auxhist7_inname
character*256 :: auxhist8_inname
character*256 :: auxhist9_inname
character*256 :: auxhist10_inname
character*256 :: auxhist11_inname
character*256 :: auxinput1_outname
character*256 :: auxinput2_outname
character*256 :: auxinput3_outname
character*256 :: auxinput4_outname
character*256 :: auxinput5_outname
character*256 :: auxinput6_outname
character*256 :: auxinput7_outname
character*256 :: auxinput8_outname
character*256 :: auxinput9_outname
character*256 :: auxinput10_outname
character*256 :: auxinput11_outname
character*256 :: auxinput2_inname
character*256 :: auxinput3_inname
character*256 :: auxinput4_inname
character*256 :: auxinput5_inname
character*256 :: auxinput6_inname
character*256 :: auxinput7_inname
character*256 :: auxinput8_inname
character*256 :: auxinput9_inname
character*256 :: gfdda_inname
character*256 :: auxinput11_inname
integer , DIMENSION(max_domains) :: history_interval_mo
integer , DIMENSION(max_domains) :: history_interval_d
integer , DIMENSION(max_domains) :: history_interval_h
integer , DIMENSION(max_domains) :: history_interval_m
integer , DIMENSION(max_domains) :: history_interval_s
integer , DIMENSION(max_domains) :: inputout_interval_mo
integer , DIMENSION(max_domains) :: inputout_interval_d
integer , DIMENSION(max_domains) :: inputout_interval_h
integer , DIMENSION(max_domains) :: inputout_interval_m
integer , DIMENSION(max_domains) :: inputout_interval_s
integer , DIMENSION(max_domains) :: inputout_interval
integer , DIMENSION(max_domains) :: auxhist1_interval_mo
integer , DIMENSION(max_domains) :: auxhist1_interval_d
integer , DIMENSION(max_domains) :: auxhist1_interval_h
integer , DIMENSION(max_domains) :: auxhist1_interval_m
integer , DIMENSION(max_domains) :: auxhist1_interval_s
integer , DIMENSION(max_domains) :: auxhist1_interval
integer , DIMENSION(max_domains) :: auxhist2_interval_mo
integer , DIMENSION(max_domains) :: auxhist2_interval_d
integer , DIMENSION(max_domains) :: auxhist2_interval_h
integer , DIMENSION(max_domains) :: auxhist2_interval_m
integer , DIMENSION(max_domains) :: auxhist2_interval_s
integer , DIMENSION(max_domains) :: auxhist2_interval
integer , DIMENSION(max_domains) :: auxhist3_interval_mo
integer , DIMENSION(max_domains) :: auxhist3_interval_d
integer , DIMENSION(max_domains) :: auxhist3_interval_h
integer , DIMENSION(max_domains) :: auxhist3_interval_m
integer , DIMENSION(max_domains) :: auxhist3_interval_s
integer , DIMENSION(max_domains) :: auxhist3_interval
integer , DIMENSION(max_domains) :: auxhist4_interval_mo
integer , DIMENSION(max_domains) :: auxhist4_interval_d
integer , DIMENSION(max_domains) :: auxhist4_interval_h
integer , DIMENSION(max_domains) :: auxhist4_interval_m
integer , DIMENSION(max_domains) :: auxhist4_interval_s
integer , DIMENSION(max_domains) :: auxhist4_interval
integer , DIMENSION(max_domains) :: auxhist5_interval_mo
integer , DIMENSION(max_domains) :: auxhist5_interval_d
integer , DIMENSION(max_domains) :: auxhist5_interval_h
integer , DIMENSION(max_domains) :: auxhist5_interval_m
integer , DIMENSION(max_domains) :: auxhist5_interval_s
integer , DIMENSION(max_domains) :: auxhist5_interval
integer , DIMENSION(max_domains) :: auxhist6_interval_mo
integer , DIMENSION(max_domains) :: auxhist6_interval_d
integer , DIMENSION(max_domains) :: auxhist6_interval_h
integer , DIMENSION(max_domains) :: auxhist6_interval_m
integer , DIMENSION(max_domains) :: auxhist6_interval_s
integer , DIMENSION(max_domains) :: auxhist6_interval
integer , DIMENSION(max_domains) :: auxhist7_interval_mo
integer , DIMENSION(max_domains) :: auxhist7_interval_d
integer , DIMENSION(max_domains) :: auxhist7_interval_h
integer , DIMENSION(max_domains) :: auxhist7_interval_m
integer , DIMENSION(max_domains) :: auxhist7_interval_s
integer , DIMENSION(max_domains) :: auxhist7_interval
integer , DIMENSION(max_domains) :: auxhist8_interval_mo
integer , DIMENSION(max_domains) :: auxhist8_interval_d
integer , DIMENSION(max_domains) :: auxhist8_interval_h
integer , DIMENSION(max_domains) :: auxhist8_interval_m
integer , DIMENSION(max_domains) :: auxhist8_interval_s
integer , DIMENSION(max_domains) :: auxhist8_interval
integer , DIMENSION(max_domains) :: auxhist9_interval_mo
integer , DIMENSION(max_domains) :: auxhist9_interval_d
integer , DIMENSION(max_domains) :: auxhist9_interval_h
integer , DIMENSION(max_domains) :: auxhist9_interval_m
integer , DIMENSION(max_domains) :: auxhist9_interval_s
integer , DIMENSION(max_domains) :: auxhist9_interval
integer , DIMENSION(max_domains) :: auxhist10_interval_mo
integer , DIMENSION(max_domains) :: auxhist10_interval_d
integer , DIMENSION(max_domains) :: auxhist10_interval_h
integer , DIMENSION(max_domains) :: auxhist10_interval_m
integer , DIMENSION(max_domains) :: auxhist10_interval_s
integer , DIMENSION(max_domains) :: auxhist10_interval
integer , DIMENSION(max_domains) :: auxhist11_interval_mo
integer , DIMENSION(max_domains) :: auxhist11_interval_d
integer , DIMENSION(max_domains) :: auxhist11_interval_h
integer , DIMENSION(max_domains) :: auxhist11_interval_m
integer , DIMENSION(max_domains) :: auxhist11_interval_s
integer , DIMENSION(max_domains) :: auxhist11_interval
integer , DIMENSION(max_domains) :: auxinput1_interval_mo
integer , DIMENSION(max_domains) :: auxinput1_interval_d
integer , DIMENSION(max_domains) :: auxinput1_interval_h
integer , DIMENSION(max_domains) :: auxinput1_interval_m
integer , DIMENSION(max_domains) :: auxinput1_interval_s
integer , DIMENSION(max_domains) :: auxinput1_interval
integer , DIMENSION(max_domains) :: auxinput2_interval_mo
integer , DIMENSION(max_domains) :: auxinput2_interval_d
integer , DIMENSION(max_domains) :: auxinput2_interval_h
integer , DIMENSION(max_domains) :: auxinput2_interval_m
integer , DIMENSION(max_domains) :: auxinput2_interval_s
integer , DIMENSION(max_domains) :: auxinput2_interval
integer , DIMENSION(max_domains) :: auxinput3_interval_mo
integer , DIMENSION(max_domains) :: auxinput3_interval_d
integer , DIMENSION(max_domains) :: auxinput3_interval_h
integer , DIMENSION(max_domains) :: auxinput3_interval_m
integer , DIMENSION(max_domains) :: auxinput3_interval_s
integer , DIMENSION(max_domains) :: auxinput3_interval
integer , DIMENSION(max_domains) :: auxinput4_interval_mo
integer , DIMENSION(max_domains) :: auxinput4_interval_d
integer , DIMENSION(max_domains) :: auxinput4_interval_h
integer , DIMENSION(max_domains) :: auxinput4_interval_m
integer , DIMENSION(max_domains) :: auxinput4_interval_s
integer , DIMENSION(max_domains) :: auxinput4_interval
integer , DIMENSION(max_domains) :: auxinput5_interval_mo
integer , DIMENSION(max_domains) :: auxinput5_interval_d
integer , DIMENSION(max_domains) :: auxinput5_interval_h
integer , DIMENSION(max_domains) :: auxinput5_interval_m
integer , DIMENSION(max_domains) :: auxinput5_interval_s
integer , DIMENSION(max_domains) :: auxinput5_interval
integer , DIMENSION(max_domains) :: auxinput6_interval_mo
integer , DIMENSION(max_domains) :: auxinput6_interval_d
integer , DIMENSION(max_domains) :: auxinput6_interval_h
integer , DIMENSION(max_domains) :: auxinput6_interval_m
integer , DIMENSION(max_domains) :: auxinput6_interval_s
integer , DIMENSION(max_domains) :: auxinput6_interval
integer , DIMENSION(max_domains) :: auxinput7_interval_mo
integer , DIMENSION(max_domains) :: auxinput7_interval_d
integer , DIMENSION(max_domains) :: auxinput7_interval_h
integer , DIMENSION(max_domains) :: auxinput7_interval_m
integer , DIMENSION(max_domains) :: auxinput7_interval_s
integer , DIMENSION(max_domains) :: auxinput7_interval
integer , DIMENSION(max_domains) :: auxinput8_interval_mo
integer , DIMENSION(max_domains) :: auxinput8_interval_d
integer , DIMENSION(max_domains) :: auxinput8_interval_h
integer , DIMENSION(max_domains) :: auxinput8_interval_m
integer , DIMENSION(max_domains) :: auxinput8_interval_s
integer , DIMENSION(max_domains) :: auxinput8_interval
integer , DIMENSION(max_domains) :: auxinput9_interval_mo
integer , DIMENSION(max_domains) :: auxinput9_interval_d
integer , DIMENSION(max_domains) :: auxinput9_interval_h
integer , DIMENSION(max_domains) :: auxinput9_interval_m
integer , DIMENSION(max_domains) :: auxinput9_interval_s
integer , DIMENSION(max_domains) :: auxinput9_interval
integer , DIMENSION(max_domains) :: gfdda_interval_mo
integer , DIMENSION(max_domains) :: gfdda_interval_d
integer , DIMENSION(max_domains) :: gfdda_interval_h
integer , DIMENSION(max_domains) :: gfdda_interval_m
integer , DIMENSION(max_domains) :: gfdda_interval_s
integer , DIMENSION(max_domains) :: gfdda_interval
integer , DIMENSION(max_domains) :: auxinput11_interval_mo
integer , DIMENSION(max_domains) :: auxinput11_interval_d
integer , DIMENSION(max_domains) :: auxinput11_interval_h
integer , DIMENSION(max_domains) :: auxinput11_interval_m
integer , DIMENSION(max_domains) :: auxinput11_interval_s
integer , DIMENSION(max_domains) :: auxinput11_interval
integer :: restart_interval_mo
integer :: restart_interval_d
integer :: restart_interval_h
integer :: restart_interval_m
integer :: restart_interval_s
integer , DIMENSION(max_domains) :: history_begin_y
integer , DIMENSION(max_domains) :: history_begin_mo
integer , DIMENSION(max_domains) :: history_begin_d
integer , DIMENSION(max_domains) :: history_begin_h
integer , DIMENSION(max_domains) :: history_begin_m
integer , DIMENSION(max_domains) :: history_begin_s
integer , DIMENSION(max_domains) :: inputout_begin_y
integer , DIMENSION(max_domains) :: inputout_begin_mo
integer , DIMENSION(max_domains) :: inputout_begin_d
integer , DIMENSION(max_domains) :: inputout_begin_h
integer , DIMENSION(max_domains) :: inputout_begin_m
integer , DIMENSION(max_domains) :: inputout_begin_s
integer , DIMENSION(max_domains) :: auxhist1_begin_y
integer , DIMENSION(max_domains) :: auxhist1_begin_mo
integer , DIMENSION(max_domains) :: auxhist1_begin_d
integer , DIMENSION(max_domains) :: auxhist1_begin_h
integer , DIMENSION(max_domains) :: auxhist1_begin_m
integer , DIMENSION(max_domains) :: auxhist1_begin_s
integer , DIMENSION(max_domains) :: auxhist2_begin_y
integer , DIMENSION(max_domains) :: auxhist2_begin_mo
integer , DIMENSION(max_domains) :: auxhist2_begin_d
integer , DIMENSION(max_domains) :: auxhist2_begin_h
integer , DIMENSION(max_domains) :: auxhist2_begin_m
integer , DIMENSION(max_domains) :: auxhist2_begin_s
integer , DIMENSION(max_domains) :: auxhist3_begin_y
integer , DIMENSION(max_domains) :: auxhist3_begin_mo
integer , DIMENSION(max_domains) :: auxhist3_begin_d
integer , DIMENSION(max_domains) :: auxhist3_begin_h
integer , DIMENSION(max_domains) :: auxhist3_begin_m
integer , DIMENSION(max_domains) :: auxhist3_begin_s
integer , DIMENSION(max_domains) :: auxhist4_begin_y
integer , DIMENSION(max_domains) :: auxhist4_begin_mo
integer , DIMENSION(max_domains) :: auxhist4_begin_d
integer , DIMENSION(max_domains) :: auxhist4_begin_h
integer , DIMENSION(max_domains) :: auxhist4_begin_m
integer , DIMENSION(max_domains) :: auxhist4_begin_s
integer , DIMENSION(max_domains) :: auxhist5_begin_y
integer , DIMENSION(max_domains) :: auxhist5_begin_mo
integer , DIMENSION(max_domains) :: auxhist5_begin_d
integer , DIMENSION(max_domains) :: auxhist5_begin_h
integer , DIMENSION(max_domains) :: auxhist5_begin_m
integer , DIMENSION(max_domains) :: auxhist5_begin_s
integer , DIMENSION(max_domains) :: auxhist6_begin_y
integer , DIMENSION(max_domains) :: auxhist6_begin_mo
integer , DIMENSION(max_domains) :: auxhist6_begin_d
integer , DIMENSION(max_domains) :: auxhist6_begin_h
integer , DIMENSION(max_domains) :: auxhist6_begin_m
integer , DIMENSION(max_domains) :: auxhist6_begin_s
integer , DIMENSION(max_domains) :: auxhist7_begin_y
integer , DIMENSION(max_domains) :: auxhist7_begin_mo
integer , DIMENSION(max_domains) :: auxhist7_begin_d
integer , DIMENSION(max_domains) :: auxhist7_begin_h
integer , DIMENSION(max_domains) :: auxhist7_begin_m
integer , DIMENSION(max_domains) :: auxhist7_begin_s
integer , DIMENSION(max_domains) :: auxhist8_begin_y
integer , DIMENSION(max_domains) :: auxhist8_begin_mo
integer , DIMENSION(max_domains) :: auxhist8_begin_d
integer , DIMENSION(max_domains) :: auxhist8_begin_h
integer , DIMENSION(max_domains) :: auxhist8_begin_m
integer , DIMENSION(max_domains) :: auxhist8_begin_s
integer , DIMENSION(max_domains) :: auxhist9_begin_y
integer , DIMENSION(max_domains) :: auxhist9_begin_mo
integer , DIMENSION(max_domains) :: auxhist9_begin_d
integer , DIMENSION(max_domains) :: auxhist9_begin_h
integer , DIMENSION(max_domains) :: auxhist9_begin_m
integer , DIMENSION(max_domains) :: auxhist9_begin_s
integer , DIMENSION(max_domains) :: auxhist10_begin_y
integer , DIMENSION(max_domains) :: auxhist10_begin_mo
integer , DIMENSION(max_domains) :: auxhist10_begin_d
integer , DIMENSION(max_domains) :: auxhist10_begin_h
integer , DIMENSION(max_domains) :: auxhist10_begin_m
integer , DIMENSION(max_domains) :: auxhist10_begin_s
integer , DIMENSION(max_domains) :: auxhist11_begin_y
integer , DIMENSION(max_domains) :: auxhist11_begin_mo
integer , DIMENSION(max_domains) :: auxhist11_begin_d
integer , DIMENSION(max_domains) :: auxhist11_begin_h
integer , DIMENSION(max_domains) :: auxhist11_begin_m
integer , DIMENSION(max_domains) :: auxhist11_begin_s
integer , DIMENSION(max_domains) :: auxinput1_begin_y
integer , DIMENSION(max_domains) :: auxinput1_begin_mo
integer , DIMENSION(max_domains) :: auxinput1_begin_d
integer , DIMENSION(max_domains) :: auxinput1_begin_h
integer , DIMENSION(max_domains) :: auxinput1_begin_m
integer , DIMENSION(max_domains) :: auxinput1_begin_s
integer , DIMENSION(max_domains) :: auxinput2_begin_y
integer , DIMENSION(max_domains) :: auxinput2_begin_mo
integer , DIMENSION(max_domains) :: auxinput2_begin_d
integer , DIMENSION(max_domains) :: auxinput2_begin_h
integer , DIMENSION(max_domains) :: auxinput2_begin_m
integer , DIMENSION(max_domains) :: auxinput2_begin_s
integer , DIMENSION(max_domains) :: auxinput3_begin_y
integer , DIMENSION(max_domains) :: auxinput3_begin_mo
integer , DIMENSION(max_domains) :: auxinput3_begin_d
integer , DIMENSION(max_domains) :: auxinput3_begin_h
integer , DIMENSION(max_domains) :: auxinput3_begin_m
integer , DIMENSION(max_domains) :: auxinput3_begin_s
integer , DIMENSION(max_domains) :: auxinput4_begin_y
integer , DIMENSION(max_domains) :: auxinput4_begin_mo
integer , DIMENSION(max_domains) :: auxinput4_begin_d
integer , DIMENSION(max_domains) :: auxinput4_begin_h
integer , DIMENSION(max_domains) :: auxinput4_begin_m
integer , DIMENSION(max_domains) :: auxinput4_begin_s
integer , DIMENSION(max_domains) :: auxinput5_begin_y
integer , DIMENSION(max_domains) :: auxinput5_begin_mo
integer , DIMENSION(max_domains) :: auxinput5_begin_d
integer , DIMENSION(max_domains) :: auxinput5_begin_h
integer , DIMENSION(max_domains) :: auxinput5_begin_m
integer , DIMENSION(max_domains) :: auxinput5_begin_s
integer , DIMENSION(max_domains) :: auxinput6_begin_y
integer , DIMENSION(max_domains) :: auxinput6_begin_mo
integer , DIMENSION(max_domains) :: auxinput6_begin_d
integer , DIMENSION(max_domains) :: auxinput6_begin_h
integer , DIMENSION(max_domains) :: auxinput6_begin_m
integer , DIMENSION(max_domains) :: auxinput6_begin_s
integer , DIMENSION(max_domains) :: auxinput7_begin_y
integer , DIMENSION(max_domains) :: auxinput7_begin_mo
integer , DIMENSION(max_domains) :: auxinput7_begin_d
integer , DIMENSION(max_domains) :: auxinput7_begin_h
integer , DIMENSION(max_domains) :: auxinput7_begin_m
integer , DIMENSION(max_domains) :: auxinput7_begin_s
integer , DIMENSION(max_domains) :: auxinput8_begin_y
integer , DIMENSION(max_domains) :: auxinput8_begin_mo
integer , DIMENSION(max_domains) :: auxinput8_begin_d
integer , DIMENSION(max_domains) :: auxinput8_begin_h
integer , DIMENSION(max_domains) :: auxinput8_begin_m
integer , DIMENSION(max_domains) :: auxinput8_begin_s
integer , DIMENSION(max_domains) :: auxinput9_begin_y
integer , DIMENSION(max_domains) :: auxinput9_begin_mo
integer , DIMENSION(max_domains) :: auxinput9_begin_d
integer , DIMENSION(max_domains) :: auxinput9_begin_h
integer , DIMENSION(max_domains) :: auxinput9_begin_m
integer , DIMENSION(max_domains) :: auxinput9_begin_s
integer , DIMENSION(max_domains) :: gfdda_begin_y
integer , DIMENSION(max_domains) :: gfdda_begin_mo
integer , DIMENSION(max_domains) :: gfdda_begin_d
integer , DIMENSION(max_domains) :: gfdda_begin_h
integer , DIMENSION(max_domains) :: gfdda_begin_m
integer , DIMENSION(max_domains) :: gfdda_begin_s
integer , DIMENSION(max_domains) :: auxinput11_begin_y
integer , DIMENSION(max_domains) :: auxinput11_begin_mo
integer , DIMENSION(max_domains) :: auxinput11_begin_d
integer , DIMENSION(max_domains) :: auxinput11_begin_h
integer , DIMENSION(max_domains) :: auxinput11_begin_m
integer , DIMENSION(max_domains) :: auxinput11_begin_s
integer :: restart_begin_y
integer :: restart_begin_mo
integer :: restart_begin_d
integer :: restart_begin_h
integer :: restart_begin_m
integer :: restart_begin_s
integer , DIMENSION(max_domains) :: history_end_y
integer , DIMENSION(max_domains) :: history_end_mo
integer , DIMENSION(max_domains) :: history_end_d
integer , DIMENSION(max_domains) :: history_end_h
integer , DIMENSION(max_domains) :: history_end_m
integer , DIMENSION(max_domains) :: history_end_s
integer , DIMENSION(max_domains) :: inputout_end_y
integer , DIMENSION(max_domains) :: inputout_end_mo
integer , DIMENSION(max_domains) :: inputout_end_d
integer , DIMENSION(max_domains) :: inputout_end_h
integer , DIMENSION(max_domains) :: inputout_end_m
integer , DIMENSION(max_domains) :: inputout_end_s
integer , DIMENSION(max_domains) :: auxhist1_end_y
integer , DIMENSION(max_domains) :: auxhist1_end_mo
integer , DIMENSION(max_domains) :: auxhist1_end_d
integer , DIMENSION(max_domains) :: auxhist1_end_h
integer , DIMENSION(max_domains) :: auxhist1_end_m
integer , DIMENSION(max_domains) :: auxhist1_end_s
integer , DIMENSION(max_domains) :: auxhist2_end_y
integer , DIMENSION(max_domains) :: auxhist2_end_mo
integer , DIMENSION(max_domains) :: auxhist2_end_d
integer , DIMENSION(max_domains) :: auxhist2_end_h
integer , DIMENSION(max_domains) :: auxhist2_end_m
integer , DIMENSION(max_domains) :: auxhist2_end_s
integer , DIMENSION(max_domains) :: auxhist3_end_y
integer , DIMENSION(max_domains) :: auxhist3_end_mo
integer , DIMENSION(max_domains) :: auxhist3_end_d
integer , DIMENSION(max_domains) :: auxhist3_end_h
integer , DIMENSION(max_domains) :: auxhist3_end_m
integer , DIMENSION(max_domains) :: auxhist3_end_s
integer , DIMENSION(max_domains) :: auxhist4_end_y
integer , DIMENSION(max_domains) :: auxhist4_end_mo
integer , DIMENSION(max_domains) :: auxhist4_end_d
integer , DIMENSION(max_domains) :: auxhist4_end_h
integer , DIMENSION(max_domains) :: auxhist4_end_m
integer , DIMENSION(max_domains) :: auxhist4_end_s
integer , DIMENSION(max_domains) :: auxhist5_end_y
integer , DIMENSION(max_domains) :: auxhist5_end_mo
integer , DIMENSION(max_domains) :: auxhist5_end_d
integer , DIMENSION(max_domains) :: auxhist5_end_h
integer , DIMENSION(max_domains) :: auxhist5_end_m
integer , DIMENSION(max_domains) :: auxhist5_end_s
integer , DIMENSION(max_domains) :: auxhist6_end_y
integer , DIMENSION(max_domains) :: auxhist6_end_mo
integer , DIMENSION(max_domains) :: auxhist6_end_d
integer , DIMENSION(max_domains) :: auxhist6_end_h
integer , DIMENSION(max_domains) :: auxhist6_end_m
integer , DIMENSION(max_domains) :: auxhist6_end_s
integer , DIMENSION(max_domains) :: auxhist7_end_y
integer , DIMENSION(max_domains) :: auxhist7_end_mo
integer , DIMENSION(max_domains) :: auxhist7_end_d
integer , DIMENSION(max_domains) :: auxhist7_end_h
integer , DIMENSION(max_domains) :: auxhist7_end_m
integer , DIMENSION(max_domains) :: auxhist7_end_s
integer , DIMENSION(max_domains) :: auxhist8_end_y
integer , DIMENSION(max_domains) :: auxhist8_end_mo
integer , DIMENSION(max_domains) :: auxhist8_end_d
integer , DIMENSION(max_domains) :: auxhist8_end_h
integer , DIMENSION(max_domains) :: auxhist8_end_m
integer , DIMENSION(max_domains) :: auxhist8_end_s
integer , DIMENSION(max_domains) :: auxhist9_end_y
integer , DIMENSION(max_domains) :: auxhist9_end_mo
integer , DIMENSION(max_domains) :: auxhist9_end_d
integer , DIMENSION(max_domains) :: auxhist9_end_h
integer , DIMENSION(max_domains) :: auxhist9_end_m
integer , DIMENSION(max_domains) :: auxhist9_end_s
integer , DIMENSION(max_domains) :: auxhist10_end_y
integer , DIMENSION(max_domains) :: auxhist10_end_mo
integer , DIMENSION(max_domains) :: auxhist10_end_d
integer , DIMENSION(max_domains) :: auxhist10_end_h
integer , DIMENSION(max_domains) :: auxhist10_end_m
integer , DIMENSION(max_domains) :: auxhist10_end_s
integer , DIMENSION(max_domains) :: auxhist11_end_y
integer , DIMENSION(max_domains) :: auxhist11_end_mo
integer , DIMENSION(max_domains) :: auxhist11_end_d
integer , DIMENSION(max_domains) :: auxhist11_end_h
integer , DIMENSION(max_domains) :: auxhist11_end_m
integer , DIMENSION(max_domains) :: auxhist11_end_s
integer , DIMENSION(max_domains) :: auxinput1_end_y
integer , DIMENSION(max_domains) :: auxinput1_end_mo
integer , DIMENSION(max_domains) :: auxinput1_end_d
integer , DIMENSION(max_domains) :: auxinput1_end_h
integer , DIMENSION(max_domains) :: auxinput1_end_m
integer , DIMENSION(max_domains) :: auxinput1_end_s
integer , DIMENSION(max_domains) :: auxinput2_end_y
integer , DIMENSION(max_domains) :: auxinput2_end_mo
integer , DIMENSION(max_domains) :: auxinput2_end_d
integer , DIMENSION(max_domains) :: auxinput2_end_h
integer , DIMENSION(max_domains) :: auxinput2_end_m
integer , DIMENSION(max_domains) :: auxinput2_end_s
integer , DIMENSION(max_domains) :: auxinput3_end_y
integer , DIMENSION(max_domains) :: auxinput3_end_mo
integer , DIMENSION(max_domains) :: auxinput3_end_d
integer , DIMENSION(max_domains) :: auxinput3_end_h
integer , DIMENSION(max_domains) :: auxinput3_end_m
integer , DIMENSION(max_domains) :: auxinput3_end_s
integer , DIMENSION(max_domains) :: auxinput4_end_y
integer , DIMENSION(max_domains) :: auxinput4_end_mo
integer , DIMENSION(max_domains) :: auxinput4_end_d
integer , DIMENSION(max_domains) :: auxinput4_end_h
integer , DIMENSION(max_domains) :: auxinput4_end_m
integer , DIMENSION(max_domains) :: auxinput4_end_s
integer , DIMENSION(max_domains) :: auxinput5_end_y
integer , DIMENSION(max_domains) :: auxinput5_end_mo
integer , DIMENSION(max_domains) :: auxinput5_end_d
integer , DIMENSION(max_domains) :: auxinput5_end_h
integer , DIMENSION(max_domains) :: auxinput5_end_m
integer , DIMENSION(max_domains) :: auxinput5_end_s
integer , DIMENSION(max_domains) :: auxinput6_end_y
integer , DIMENSION(max_domains) :: auxinput6_end_mo
integer , DIMENSION(max_domains) :: auxinput6_end_d
integer , DIMENSION(max_domains) :: auxinput6_end_h
integer , DIMENSION(max_domains) :: auxinput6_end_m
integer , DIMENSION(max_domains) :: auxinput6_end_s
integer , DIMENSION(max_domains) :: auxinput7_end_y
integer , DIMENSION(max_domains) :: auxinput7_end_mo
integer , DIMENSION(max_domains) :: auxinput7_end_d
integer , DIMENSION(max_domains) :: auxinput7_end_h
integer , DIMENSION(max_domains) :: auxinput7_end_m
integer , DIMENSION(max_domains) :: auxinput7_end_s
integer , DIMENSION(max_domains) :: auxinput8_end_y
integer , DIMENSION(max_domains) :: auxinput8_end_mo
integer , DIMENSION(max_domains) :: auxinput8_end_d
integer , DIMENSION(max_domains) :: auxinput8_end_h
integer , DIMENSION(max_domains) :: auxinput8_end_m
integer , DIMENSION(max_domains) :: auxinput8_end_s
integer , DIMENSION(max_domains) :: auxinput9_end_y
integer , DIMENSION(max_domains) :: auxinput9_end_mo
integer , DIMENSION(max_domains) :: auxinput9_end_d
integer , DIMENSION(max_domains) :: auxinput9_end_h
integer , DIMENSION(max_domains) :: auxinput9_end_m
integer , DIMENSION(max_domains) :: auxinput9_end_s
integer , DIMENSION(max_domains) :: gfdda_end_y
integer , DIMENSION(max_domains) :: gfdda_end_mo
integer , DIMENSION(max_domains) :: gfdda_end_d
integer , DIMENSION(max_domains) :: gfdda_end_h
integer , DIMENSION(max_domains) :: gfdda_end_m
integer , DIMENSION(max_domains) :: gfdda_end_s
integer , DIMENSION(max_domains) :: auxinput11_end_y
integer , DIMENSION(max_domains) :: auxinput11_end_mo
integer , DIMENSION(max_domains) :: auxinput11_end_d
integer , DIMENSION(max_domains) :: auxinput11_end_h
integer , DIMENSION(max_domains) :: auxinput11_end_m
integer , DIMENSION(max_domains) :: auxinput11_end_s
integer :: io_form_auxinput1
integer :: io_form_auxinput2
integer :: io_form_auxinput3
integer :: io_form_auxinput4
integer :: io_form_auxinput5
integer :: io_form_auxinput6
integer :: io_form_auxinput7
integer :: io_form_auxinput8
integer :: io_form_auxinput9
integer :: io_form_gfdda
integer :: io_form_auxinput11
integer :: io_form_auxhist1
integer :: io_form_auxhist2
integer :: io_form_auxhist3
integer :: io_form_auxhist4
integer :: io_form_auxhist5
integer :: io_form_auxhist6
integer :: io_form_auxhist7
integer :: io_form_auxhist8
integer :: io_form_auxhist9
integer :: io_form_auxhist10
integer :: io_form_auxhist11
character*256 :: auxinput1_inname
integer , DIMENSION(max_domains) :: julyr
integer , DIMENSION(max_domains) :: julday
real , DIMENSION(max_domains) :: gmt
character*256 :: input_inname
character*256 :: input_outname
character*256 :: bdy_inname
character*256 :: bdy_outname
character*256 :: rst_inname
character*256 :: rst_outname
logical :: write_input
logical :: write_restart_at_0h
logical :: adjust_output_times
logical :: adjust_input_times
integer :: diag_print
logical :: nocolons
integer :: time_step
integer :: time_step_fract_num
integer :: time_step_fract_den
integer :: max_dom
integer , DIMENSION(max_domains) :: s_we
integer , DIMENSION(max_domains) :: e_we
integer , DIMENSION(max_domains) :: s_sn
integer , DIMENSION(max_domains) :: e_sn
integer , DIMENSION(max_domains) :: s_vert
integer , DIMENSION(max_domains) :: e_vert
integer :: num_metgrid_levels
real :: p_top_requested
integer :: interp_type
logical :: lowest_lev_from_sfc
integer :: lagrange_order
integer :: force_sfc_in_vinterp
real :: zap_close_levels
logical :: sfcp_to_sfcp
logical :: adjust_heights
real , DIMENSION(max_domains) :: dx
real , DIMENSION(max_domains) :: dy
integer , DIMENSION(max_domains) :: grid_id
integer , DIMENSION(max_domains) :: parent_id
integer , DIMENSION(max_domains) :: i_parent_start
integer , DIMENSION(max_domains) :: j_parent_start
integer , DIMENSION(max_domains) :: parent_grid_ratio
integer , DIMENSION(max_domains) :: parent_time_step_ratio
integer :: feedback
integer :: smooth_option
integer :: blend_width
real , DIMENSION(max_domains) :: ztop
integer , DIMENSION(max_domains) :: moad_grid_ratio
integer , DIMENSION(max_domains) :: moad_time_step_ratio
integer , DIMENSION(max_domains) :: shw
integer :: tile_sz_x
integer :: tile_sz_y
integer :: numtiles
integer :: nproc_x
integer :: nproc_y
integer :: irand
real , DIMENSION(max_domains) :: dt
integer :: num_moves
integer , DIMENSION(max_domains) :: vortex_interval
integer , DIMENSION(max_domains) :: max_vortex_speed
integer , DIMENSION(max_domains) :: corral_dist
integer , DIMENSION(max_moves) :: move_id
integer , DIMENSION(max_moves) :: move_interval
integer , DIMENSION(max_moves) :: move_cd_x
integer , DIMENSION(max_moves) :: move_cd_y
logical , DIMENSION(max_domains) :: swap_x
logical , DIMENSION(max_domains) :: swap_y
logical , DIMENSION(max_domains) :: cycle_x
logical , DIMENSION(max_domains) :: cycle_y
logical :: reorder_mesh
logical :: perturb_input
real , DIMENSION(max_eta) :: eta_levels
real :: max_dz
integer , DIMENSION(max_domains) :: mp_physics
integer , DIMENSION(max_domains) :: ra_lw_physics
integer , DIMENSION(max_domains) :: ra_sw_physics
real , DIMENSION(max_domains) :: radt
integer , DIMENSION(max_domains) :: sf_sfclay_physics
integer , DIMENSION(max_domains) :: sf_surface_physics
integer , DIMENSION(max_domains) :: bl_pbl_physics
real , DIMENSION(max_domains) :: bldt
integer , DIMENSION(max_domains) :: cu_physics
real , DIMENSION(max_domains) :: cudt
real , DIMENSION(max_domains) :: gsmdt
integer :: isfflx
integer :: ifsnow
integer :: icloud
real :: swrad_scat
integer :: surface_input_source
integer :: num_soil_layers
integer :: num_months
integer :: maxiens
integer :: maxens
integer :: maxens2
integer :: maxens3
integer :: ensdim
integer , DIMENSION(max_domains) :: chem_opt
integer :: num_land_cat
integer :: num_soil_cat
integer :: mp_zero_out
real :: mp_zero_out_thresh
real :: seaice_threshold
integer :: sst_update
integer :: ucmcall
integer :: co2tf
integer :: ra_call_offset
real :: cam_abs_freq_s
integer :: levsiz
integer :: paerlev
integer :: cam_abs_dim1
integer :: cam_abs_dim2
logical , DIMENSION(max_domains) :: cu_rad_feedback
real , DIMENSION(max_domains) :: fgdt
integer , DIMENSION(max_domains) :: grid_fdda
integer , DIMENSION(max_domains) :: if_no_pbl_nudging_uv
integer , DIMENSION(max_domains) :: if_no_pbl_nudging_t
integer , DIMENSION(max_domains) :: if_no_pbl_nudging_q
integer , DIMENSION(max_domains) :: if_zfac_uv
integer , DIMENSION(max_domains) :: k_zfac_uv
integer , DIMENSION(max_domains) :: if_zfac_t
integer , DIMENSION(max_domains) :: k_zfac_t
integer , DIMENSION(max_domains) :: if_zfac_q
integer , DIMENSION(max_domains) :: k_zfac_q
real , DIMENSION(max_domains) :: guv
real , DIMENSION(max_domains) :: gt
real , DIMENSION(max_domains) :: gq
real :: dtramp_min
integer :: if_ramping
integer , DIMENSION(max_domains) :: obs_nudge_opt
integer :: max_obs
integer :: nobs_ndg_vars
integer :: nobs_err_flds
real , DIMENSION(max_domains) :: fdda_start
real , DIMENSION(max_domains) :: fdda_end
integer , DIMENSION(max_domains) :: obs_nudge_wind
real , DIMENSION(max_domains) :: obs_coef_wind
integer , DIMENSION(max_domains) :: obs_nudge_temp
real , DIMENSION(max_domains) :: obs_coef_temp
integer , DIMENSION(max_domains) :: obs_nudge_mois
real , DIMENSION(max_domains) :: obs_coef_mois
integer , DIMENSION(max_domains) :: obs_nudge_pstr
real , DIMENSION(max_domains) :: obs_coef_pstr
real , DIMENSION(max_domains) :: obs_rinxy
real :: obs_rinsig
real :: obs_twindo
integer :: obs_npfi
integer :: obs_ionf
integer :: obs_idynin
real :: obs_dtramp
logical :: obs_ipf_in4dob
logical :: obs_ipf_errob
logical :: obs_ipf_nudob
integer :: dyn_opt
integer :: rk_ord
integer :: w_damping
integer :: diff_opt
integer :: km_opt
integer :: damp_opt
real , DIMENSION(max_domains) :: zdamp
real , DIMENSION(max_domains) :: dampcoef
real , DIMENSION(max_domains) :: khdif
real , DIMENSION(max_domains) :: kvdif
real , DIMENSION(max_domains) :: diff_6th_factor
integer , DIMENSION(max_domains) :: diff_6th_opt
real , DIMENSION(max_domains) :: smdiv
real , DIMENSION(max_domains) :: emdiv
real , DIMENSION(max_domains) :: epssm
logical , DIMENSION(max_domains) :: non_hydrostatic
integer , DIMENSION(max_domains) :: time_step_sound
integer , DIMENSION(max_domains) :: h_mom_adv_order
integer , DIMENSION(max_domains) :: v_mom_adv_order
integer , DIMENSION(max_domains) :: h_sca_adv_order
integer , DIMENSION(max_domains) :: v_sca_adv_order
logical , DIMENSION(max_domains) :: pd_moist
logical , DIMENSION(max_domains) :: pd_chem
logical , DIMENSION(max_domains) :: pd_scalar
logical , DIMENSION(max_domains) :: pd_tke
logical , DIMENSION(max_domains) :: top_radiation
real , DIMENSION(max_domains) :: mix_cr_len
real , DIMENSION(max_domains) :: tke_upper_bound
real , DIMENSION(max_domains) :: kh_tke_upper_bound
real , DIMENSION(max_domains) :: kv_tke_upper_bound
real , DIMENSION(max_domains) :: tke_drag_coefficient
real , DIMENSION(max_domains) :: tke_heat_flux
logical , DIMENSION(max_domains) :: pert_coriolis
logical , DIMENSION(max_domains) :: mix_full_fields
real :: base_pres
real :: base_temp
real :: base_lapse
integer :: spec_bdy_width
integer :: spec_zone
integer :: relax_zone
logical , DIMENSION(max_domains) :: specified
logical , DIMENSION(max_domains) :: periodic_x
logical , DIMENSION(max_domains) :: symmetric_xs
logical , DIMENSION(max_domains) :: symmetric_xe
logical , DIMENSION(max_domains) :: open_xs
logical , DIMENSION(max_domains) :: open_xe
logical , DIMENSION(max_domains) :: periodic_y
logical , DIMENSION(max_domains) :: symmetric_ys
logical , DIMENSION(max_domains) :: symmetric_ye
logical , DIMENSION(max_domains) :: open_ys
logical , DIMENSION(max_domains) :: open_ye
logical , DIMENSION(max_domains) :: nested
integer :: real_data_init_type
integer :: background_proc_id
integer :: forecast_proc_id
integer :: production_status
integer :: compression
real , DIMENSION(max_domains) :: cen_lat
real , DIMENSION(max_domains) :: cen_lon
real , DIMENSION(max_domains) :: truelat1
real , DIMENSION(max_domains) :: truelat2
real , DIMENSION(max_domains) :: moad_cen_lat
real , DIMENSION(max_domains) :: stand_lon
real , DIMENSION(max_domains) :: bdyfrq
integer , DIMENSION(max_domains) :: iswater
integer , DIMENSION(max_domains) :: isice
integer , DIMENSION(max_domains) :: isurban
integer , DIMENSION(max_domains) :: isoilwater
integer , DIMENSION(max_domains) :: map_proj
integer :: simulation_start_year
integer :: simulation_start_month
integer :: simulation_start_day
integer :: simulation_start_hour
integer :: simulation_start_minute
integer :: simulation_start_second
integer    :: last_item_in_struct
!ENDOFREGISTRYGENERATEDINCLUDE
