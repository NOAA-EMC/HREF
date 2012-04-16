


MODULE module_scalar_tables
  USE module_driver_constants
  USE module_state_description






  INTEGER :: moist_index_table( param_num_moist, max_domains )
  INTEGER :: moist_num_table( max_domains )
  INTEGER :: moist_stream_table( max_domains, param_num_moist )
  CHARACTER*256 :: moist_dname_table( max_domains, param_num_moist )
  CHARACTER*256 :: moist_desc_table( max_domains, param_num_moist )
  CHARACTER*256 :: moist_units_table( max_domains, param_num_moist )
  INTEGER :: dfi_moist_index_table( param_num_dfi_moist, max_domains )
  INTEGER :: dfi_moist_num_table( max_domains )
  INTEGER :: dfi_moist_stream_table( max_domains, param_num_dfi_moist )
  CHARACTER*256 :: dfi_moist_dname_table( max_domains, param_num_dfi_moist )
  CHARACTER*256 :: dfi_moist_desc_table( max_domains, param_num_dfi_moist )
  CHARACTER*256 :: dfi_moist_units_table( max_domains, param_num_dfi_moist )
  INTEGER :: scalar_index_table( param_num_scalar, max_domains )
  INTEGER :: scalar_num_table( max_domains )
  INTEGER :: scalar_stream_table( max_domains, param_num_scalar )
  CHARACTER*256 :: scalar_dname_table( max_domains, param_num_scalar )
  CHARACTER*256 :: scalar_desc_table( max_domains, param_num_scalar )
  CHARACTER*256 :: scalar_units_table( max_domains, param_num_scalar )
  INTEGER :: dfi_scalar_index_table( param_num_dfi_scalar, max_domains )
  INTEGER :: dfi_scalar_num_table( max_domains )
  INTEGER :: dfi_scalar_stream_table( max_domains, param_num_dfi_scalar )
  CHARACTER*256 :: dfi_scalar_dname_table( max_domains, param_num_dfi_scalar )
  CHARACTER*256 :: dfi_scalar_desc_table( max_domains, param_num_dfi_scalar )
  CHARACTER*256 :: dfi_scalar_units_table( max_domains, param_num_dfi_scalar )
  INTEGER :: chem_index_table( param_num_chem, max_domains )
  INTEGER :: chem_num_table( max_domains )
  INTEGER :: chem_stream_table( max_domains, param_num_chem )
  CHARACTER*256 :: chem_dname_table( max_domains, param_num_chem )
  CHARACTER*256 :: chem_desc_table( max_domains, param_num_chem )
  CHARACTER*256 :: chem_units_table( max_domains, param_num_chem )

CONTAINS
  SUBROUTINE init_module_scalar_tables
     INTEGER i , j
     DO j = 1, max_domains






  moist_num_table( j ) = 1
  dfi_moist_num_table( j ) = 1
  scalar_num_table( j ) = 1
  dfi_scalar_num_table( j ) = 1
  chem_num_table( j ) = 1

     END DO
  END SUBROUTINE init_module_scalar_tables
END MODULE module_scalar_tables

MODULE module_configure

   USE module_driver_constants
   USE module_state_description
   USE module_wrf_error

   TYPE model_config_rec_type
      SEQUENCE











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
character*256 :: auxinput1_inname
character*256 :: auxinput2_inname
character*256 :: auxinput3_inname
character*256 :: auxinput4_inname
character*256 :: auxinput5_inname
character*256 :: auxinput6_inname
character*256 :: auxinput7_inname
character*256 :: auxinput8_inname
character*256 :: sgfdda_inname
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
integer , DIMENSION(max_domains) :: sgfdda_interval_mo
integer , DIMENSION(max_domains) :: sgfdda_interval_d
integer , DIMENSION(max_domains) :: sgfdda_interval_h
integer , DIMENSION(max_domains) :: sgfdda_interval_m
integer , DIMENSION(max_domains) :: sgfdda_interval_s
integer , DIMENSION(max_domains) :: sgfdda_interval
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
integer , DIMENSION(max_domains) :: sgfdda_begin_y
integer , DIMENSION(max_domains) :: sgfdda_begin_mo
integer , DIMENSION(max_domains) :: sgfdda_begin_d
integer , DIMENSION(max_domains) :: sgfdda_begin_h
integer , DIMENSION(max_domains) :: sgfdda_begin_m
integer , DIMENSION(max_domains) :: sgfdda_begin_s
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
integer , DIMENSION(max_domains) :: sgfdda_end_y
integer , DIMENSION(max_domains) :: sgfdda_end_mo
integer , DIMENSION(max_domains) :: sgfdda_end_d
integer , DIMENSION(max_domains) :: sgfdda_end_h
integer , DIMENSION(max_domains) :: sgfdda_end_m
integer , DIMENSION(max_domains) :: sgfdda_end_s
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
integer :: io_form_sgfdda
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
integer :: simulation_start_year
integer :: simulation_start_month
integer :: simulation_start_day
integer :: simulation_start_hour
integer :: simulation_start_minute
integer :: simulation_start_second
logical :: reset_simulation_start
integer , DIMENSION(max_domains) :: sr_x
integer , DIMENSION(max_domains) :: sr_y
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
real , DIMENSION(max_domains) :: tstart
logical :: nocolons
logical :: cycling
integer :: dfi_opt
integer :: dfi_nfilter
logical :: dfi_write_filtered_input
logical :: dfi_write_dfi_history
integer :: dfi_cutoff_seconds
integer :: dfi_time_dim
integer :: dfi_fwdstop_year
integer :: dfi_fwdstop_month
integer :: dfi_fwdstop_day
integer :: dfi_fwdstop_hour
integer :: dfi_fwdstop_minute
integer :: dfi_fwdstop_second
integer :: dfi_bckstop_year
integer :: dfi_bckstop_month
integer :: dfi_bckstop_day
integer :: dfi_bckstop_hour
integer :: dfi_bckstop_minute
integer :: dfi_bckstop_second
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
real , DIMENSION(max_domains) :: dx
real , DIMENSION(max_domains) :: dy
integer , DIMENSION(max_domains) :: grid_id
logical , DIMENSION(max_domains) :: grid_allowed
integer , DIMENSION(max_domains) :: parent_id
integer , DIMENSION(max_domains) :: i_parent_start
integer , DIMENSION(max_domains) :: j_parent_start
integer , DIMENSION(max_domains) :: parent_grid_ratio
integer , DIMENSION(max_domains) :: parent_time_step_ratio
integer :: feedback
integer :: smooth_option
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
integer :: ts_buf_size
integer :: max_ts_locs
integer :: num_moves
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
real :: ptsgm
integer :: num_metgrid_levels
real :: p_top_requested
integer , DIMENSION(max_domains) :: mp_physics
integer , DIMENSION(max_domains) :: ra_lw_physics
integer , DIMENSION(max_domains) :: ra_sw_physics
real , DIMENSION(max_domains) :: radt
integer , DIMENSION(max_domains) :: sf_sfclay_physics
integer , DIMENSION(max_domains) :: sf_surface_physics
integer , DIMENSION(max_domains) :: bl_pbl_physics
integer , DIMENSION(max_domains) :: sf_urban_physics
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
integer :: num_urban_layers
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
integer :: fractional_seaice
integer :: sst_update
logical :: usemonalb
logical :: rdmaxalb
logical :: rdlai2d
integer , DIMENSION(max_domains) :: gwd_opt
integer , DIMENSION(max_domains) :: idtad
integer , DIMENSION(max_domains) :: nsoil
integer , DIMENSION(max_domains) :: nphs
integer , DIMENSION(max_domains) :: ncnvc
integer , DIMENSION(max_domains) :: nrads
integer , DIMENSION(max_domains) :: nradl
real , DIMENSION(max_domains) :: tprec
real , DIMENSION(max_domains) :: theat
real , DIMENSION(max_domains) :: tclod
real , DIMENSION(max_domains) :: trdsw
real , DIMENSION(max_domains) :: trdlw
real , DIMENSION(max_domains) :: tsrfc
logical , DIMENSION(max_domains) :: pcpflg
integer , DIMENSION(max_domains) :: sigma
real , DIMENSION(max_domains) :: sfenth
integer :: co2tf
integer :: ra_call_offset
real :: cam_abs_freq_s
integer :: levsiz
integer :: paerlev
integer :: cam_abs_dim1
integer :: cam_abs_dim2
logical , DIMENSION(max_domains) :: cu_rad_feedback
integer :: dyn_opt
integer :: rk_ord
integer :: w_damping
integer :: diff_opt
integer :: km_opt
integer :: damp_opt
real , DIMENSION(max_domains) :: zdamp
real :: base_pres
real :: base_temp
real :: base_lapse
real :: iso_temp
real , DIMENSION(max_domains) :: dampcoef
real , DIMENSION(max_domains) :: khdif
real , DIMENSION(max_domains) :: kvdif
real , DIMENSION(max_domains) :: c_s
real , DIMENSION(max_domains) :: c_k
real , DIMENSION(max_domains) :: smdiv
real , DIMENSION(max_domains) :: emdiv
real , DIMENSION(max_domains) :: epssm
logical , DIMENSION(max_domains) :: non_hydrostatic
integer , DIMENSION(max_domains) :: time_step_sound
integer , DIMENSION(max_domains) :: h_mom_adv_order
integer , DIMENSION(max_domains) :: v_mom_adv_order
integer , DIMENSION(max_domains) :: h_sca_adv_order
integer , DIMENSION(max_domains) :: v_sca_adv_order
logical , DIMENSION(max_domains) :: top_radiation
real , DIMENSION(max_domains) :: tke_upper_bound
real , DIMENSION(max_domains) :: tke_drag_coefficient
real , DIMENSION(max_domains) :: tke_heat_flux
logical , DIMENSION(max_domains) :: pert_coriolis
logical :: euler_adv
integer :: idtadt
integer :: idtadc
logical :: boundary_flux
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
logical , DIMENSION(max_domains) :: polar
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
integer :: flag_metgrid
integer :: flag_snow
integer :: flag_psfc
integer :: flag_sm000010
integer :: flag_sm010040
integer :: flag_sm040100
integer :: flag_sm100200
integer :: flag_st000010
integer :: flag_st010040
integer :: flag_st040100
integer :: flag_st100200
integer :: flag_slp
integer :: flag_soilhgt
integer :: flag_mf_xy
real , DIMENSION(max_domains) :: bdyfrq
character*256 , DIMENSION(max_domains) :: mminlu
integer , DIMENSION(max_domains) :: iswater
integer , DIMENSION(max_domains) :: islake
integer , DIMENSION(max_domains) :: isice
integer , DIMENSION(max_domains) :: isurban
integer , DIMENSION(max_domains) :: isoilwater
integer , DIMENSION(max_domains) :: map_proj
integer :: dfi_stage
integer , DIMENSION(max_domains) :: mp_physics_dfi
integer    :: last_item_in_struct

   END TYPE model_config_rec_type

   TYPE grid_config_rec_type






integer    :: first_item_in_struct
integer :: run_days
integer :: run_hours
integer :: run_minutes
integer :: run_seconds
integer :: start_year
integer :: start_month
integer :: start_day
integer :: start_hour
integer :: start_minute
integer :: start_second
integer :: end_year
integer :: end_month
integer :: end_day
integer :: end_hour
integer :: end_minute
integer :: end_second
integer :: interval_seconds
logical :: input_from_file
integer :: fine_input_stream
integer :: history_interval
integer :: frames_per_outfile
integer :: frames_per_auxhist1
integer :: frames_per_auxhist2
integer :: frames_per_auxhist3
integer :: frames_per_auxhist4
integer :: frames_per_auxhist5
integer :: frames_per_auxhist6
integer :: frames_per_auxhist7
integer :: frames_per_auxhist8
integer :: frames_per_auxhist9
integer :: frames_per_auxhist10
integer :: frames_per_auxhist11
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
character*256 :: auxinput1_inname
character*256 :: auxinput2_inname
character*256 :: auxinput3_inname
character*256 :: auxinput4_inname
character*256 :: auxinput5_inname
character*256 :: auxinput6_inname
character*256 :: auxinput7_inname
character*256 :: auxinput8_inname
character*256 :: sgfdda_inname
character*256 :: gfdda_inname
character*256 :: auxinput11_inname
integer :: history_interval_mo
integer :: history_interval_d
integer :: history_interval_h
integer :: history_interval_m
integer :: history_interval_s
integer :: inputout_interval_mo
integer :: inputout_interval_d
integer :: inputout_interval_h
integer :: inputout_interval_m
integer :: inputout_interval_s
integer :: inputout_interval
integer :: auxhist1_interval_mo
integer :: auxhist1_interval_d
integer :: auxhist1_interval_h
integer :: auxhist1_interval_m
integer :: auxhist1_interval_s
integer :: auxhist1_interval
integer :: auxhist2_interval_mo
integer :: auxhist2_interval_d
integer :: auxhist2_interval_h
integer :: auxhist2_interval_m
integer :: auxhist2_interval_s
integer :: auxhist2_interval
integer :: auxhist3_interval_mo
integer :: auxhist3_interval_d
integer :: auxhist3_interval_h
integer :: auxhist3_interval_m
integer :: auxhist3_interval_s
integer :: auxhist3_interval
integer :: auxhist4_interval_mo
integer :: auxhist4_interval_d
integer :: auxhist4_interval_h
integer :: auxhist4_interval_m
integer :: auxhist4_interval_s
integer :: auxhist4_interval
integer :: auxhist5_interval_mo
integer :: auxhist5_interval_d
integer :: auxhist5_interval_h
integer :: auxhist5_interval_m
integer :: auxhist5_interval_s
integer :: auxhist5_interval
integer :: auxhist6_interval_mo
integer :: auxhist6_interval_d
integer :: auxhist6_interval_h
integer :: auxhist6_interval_m
integer :: auxhist6_interval_s
integer :: auxhist6_interval
integer :: auxhist7_interval_mo
integer :: auxhist7_interval_d
integer :: auxhist7_interval_h
integer :: auxhist7_interval_m
integer :: auxhist7_interval_s
integer :: auxhist7_interval
integer :: auxhist8_interval_mo
integer :: auxhist8_interval_d
integer :: auxhist8_interval_h
integer :: auxhist8_interval_m
integer :: auxhist8_interval_s
integer :: auxhist8_interval
integer :: auxhist9_interval_mo
integer :: auxhist9_interval_d
integer :: auxhist9_interval_h
integer :: auxhist9_interval_m
integer :: auxhist9_interval_s
integer :: auxhist9_interval
integer :: auxhist10_interval_mo
integer :: auxhist10_interval_d
integer :: auxhist10_interval_h
integer :: auxhist10_interval_m
integer :: auxhist10_interval_s
integer :: auxhist10_interval
integer :: auxhist11_interval_mo
integer :: auxhist11_interval_d
integer :: auxhist11_interval_h
integer :: auxhist11_interval_m
integer :: auxhist11_interval_s
integer :: auxhist11_interval
integer :: auxinput1_interval_mo
integer :: auxinput1_interval_d
integer :: auxinput1_interval_h
integer :: auxinput1_interval_m
integer :: auxinput1_interval_s
integer :: auxinput1_interval
integer :: auxinput2_interval_mo
integer :: auxinput2_interval_d
integer :: auxinput2_interval_h
integer :: auxinput2_interval_m
integer :: auxinput2_interval_s
integer :: auxinput2_interval
integer :: auxinput3_interval_mo
integer :: auxinput3_interval_d
integer :: auxinput3_interval_h
integer :: auxinput3_interval_m
integer :: auxinput3_interval_s
integer :: auxinput3_interval
integer :: auxinput4_interval_mo
integer :: auxinput4_interval_d
integer :: auxinput4_interval_h
integer :: auxinput4_interval_m
integer :: auxinput4_interval_s
integer :: auxinput4_interval
integer :: auxinput5_interval_mo
integer :: auxinput5_interval_d
integer :: auxinput5_interval_h
integer :: auxinput5_interval_m
integer :: auxinput5_interval_s
integer :: auxinput5_interval
integer :: auxinput6_interval_mo
integer :: auxinput6_interval_d
integer :: auxinput6_interval_h
integer :: auxinput6_interval_m
integer :: auxinput6_interval_s
integer :: auxinput6_interval
integer :: auxinput7_interval_mo
integer :: auxinput7_interval_d
integer :: auxinput7_interval_h
integer :: auxinput7_interval_m
integer :: auxinput7_interval_s
integer :: auxinput7_interval
integer :: auxinput8_interval_mo
integer :: auxinput8_interval_d
integer :: auxinput8_interval_h
integer :: auxinput8_interval_m
integer :: auxinput8_interval_s
integer :: auxinput8_interval
integer :: sgfdda_interval_mo
integer :: sgfdda_interval_d
integer :: sgfdda_interval_h
integer :: sgfdda_interval_m
integer :: sgfdda_interval_s
integer :: sgfdda_interval
integer :: gfdda_interval_mo
integer :: gfdda_interval_d
integer :: gfdda_interval_h
integer :: gfdda_interval_m
integer :: gfdda_interval_s
integer :: gfdda_interval
integer :: auxinput11_interval_mo
integer :: auxinput11_interval_d
integer :: auxinput11_interval_h
integer :: auxinput11_interval_m
integer :: auxinput11_interval_s
integer :: auxinput11_interval
integer :: restart_interval_mo
integer :: restart_interval_d
integer :: restart_interval_h
integer :: restart_interval_m
integer :: restart_interval_s
integer :: history_begin_y
integer :: history_begin_mo
integer :: history_begin_d
integer :: history_begin_h
integer :: history_begin_m
integer :: history_begin_s
integer :: inputout_begin_y
integer :: inputout_begin_mo
integer :: inputout_begin_d
integer :: inputout_begin_h
integer :: inputout_begin_m
integer :: inputout_begin_s
integer :: auxhist1_begin_y
integer :: auxhist1_begin_mo
integer :: auxhist1_begin_d
integer :: auxhist1_begin_h
integer :: auxhist1_begin_m
integer :: auxhist1_begin_s
integer :: auxhist2_begin_y
integer :: auxhist2_begin_mo
integer :: auxhist2_begin_d
integer :: auxhist2_begin_h
integer :: auxhist2_begin_m
integer :: auxhist2_begin_s
integer :: auxhist3_begin_y
integer :: auxhist3_begin_mo
integer :: auxhist3_begin_d
integer :: auxhist3_begin_h
integer :: auxhist3_begin_m
integer :: auxhist3_begin_s
integer :: auxhist4_begin_y
integer :: auxhist4_begin_mo
integer :: auxhist4_begin_d
integer :: auxhist4_begin_h
integer :: auxhist4_begin_m
integer :: auxhist4_begin_s
integer :: auxhist5_begin_y
integer :: auxhist5_begin_mo
integer :: auxhist5_begin_d
integer :: auxhist5_begin_h
integer :: auxhist5_begin_m
integer :: auxhist5_begin_s
integer :: auxhist6_begin_y
integer :: auxhist6_begin_mo
integer :: auxhist6_begin_d
integer :: auxhist6_begin_h
integer :: auxhist6_begin_m
integer :: auxhist6_begin_s
integer :: auxhist7_begin_y
integer :: auxhist7_begin_mo
integer :: auxhist7_begin_d
integer :: auxhist7_begin_h
integer :: auxhist7_begin_m
integer :: auxhist7_begin_s
integer :: auxhist8_begin_y
integer :: auxhist8_begin_mo
integer :: auxhist8_begin_d
integer :: auxhist8_begin_h
integer :: auxhist8_begin_m
integer :: auxhist8_begin_s
integer :: auxhist9_begin_y
integer :: auxhist9_begin_mo
integer :: auxhist9_begin_d
integer :: auxhist9_begin_h
integer :: auxhist9_begin_m
integer :: auxhist9_begin_s
integer :: auxhist10_begin_y
integer :: auxhist10_begin_mo
integer :: auxhist10_begin_d
integer :: auxhist10_begin_h
integer :: auxhist10_begin_m
integer :: auxhist10_begin_s
integer :: auxhist11_begin_y
integer :: auxhist11_begin_mo
integer :: auxhist11_begin_d
integer :: auxhist11_begin_h
integer :: auxhist11_begin_m
integer :: auxhist11_begin_s
integer :: auxinput1_begin_y
integer :: auxinput1_begin_mo
integer :: auxinput1_begin_d
integer :: auxinput1_begin_h
integer :: auxinput1_begin_m
integer :: auxinput1_begin_s
integer :: auxinput2_begin_y
integer :: auxinput2_begin_mo
integer :: auxinput2_begin_d
integer :: auxinput2_begin_h
integer :: auxinput2_begin_m
integer :: auxinput2_begin_s
integer :: auxinput3_begin_y
integer :: auxinput3_begin_mo
integer :: auxinput3_begin_d
integer :: auxinput3_begin_h
integer :: auxinput3_begin_m
integer :: auxinput3_begin_s
integer :: auxinput4_begin_y
integer :: auxinput4_begin_mo
integer :: auxinput4_begin_d
integer :: auxinput4_begin_h
integer :: auxinput4_begin_m
integer :: auxinput4_begin_s
integer :: auxinput5_begin_y
integer :: auxinput5_begin_mo
integer :: auxinput5_begin_d
integer :: auxinput5_begin_h
integer :: auxinput5_begin_m
integer :: auxinput5_begin_s
integer :: auxinput6_begin_y
integer :: auxinput6_begin_mo
integer :: auxinput6_begin_d
integer :: auxinput6_begin_h
integer :: auxinput6_begin_m
integer :: auxinput6_begin_s
integer :: auxinput7_begin_y
integer :: auxinput7_begin_mo
integer :: auxinput7_begin_d
integer :: auxinput7_begin_h
integer :: auxinput7_begin_m
integer :: auxinput7_begin_s
integer :: auxinput8_begin_y
integer :: auxinput8_begin_mo
integer :: auxinput8_begin_d
integer :: auxinput8_begin_h
integer :: auxinput8_begin_m
integer :: auxinput8_begin_s
integer :: sgfdda_begin_y
integer :: sgfdda_begin_mo
integer :: sgfdda_begin_d
integer :: sgfdda_begin_h
integer :: sgfdda_begin_m
integer :: sgfdda_begin_s
integer :: gfdda_begin_y
integer :: gfdda_begin_mo
integer :: gfdda_begin_d
integer :: gfdda_begin_h
integer :: gfdda_begin_m
integer :: gfdda_begin_s
integer :: auxinput11_begin_y
integer :: auxinput11_begin_mo
integer :: auxinput11_begin_d
integer :: auxinput11_begin_h
integer :: auxinput11_begin_m
integer :: auxinput11_begin_s
integer :: restart_begin_y
integer :: restart_begin_mo
integer :: restart_begin_d
integer :: restart_begin_h
integer :: restart_begin_m
integer :: restart_begin_s
integer :: history_end_y
integer :: history_end_mo
integer :: history_end_d
integer :: history_end_h
integer :: history_end_m
integer :: history_end_s
integer :: inputout_end_y
integer :: inputout_end_mo
integer :: inputout_end_d
integer :: inputout_end_h
integer :: inputout_end_m
integer :: inputout_end_s
integer :: auxhist1_end_y
integer :: auxhist1_end_mo
integer :: auxhist1_end_d
integer :: auxhist1_end_h
integer :: auxhist1_end_m
integer :: auxhist1_end_s
integer :: auxhist2_end_y
integer :: auxhist2_end_mo
integer :: auxhist2_end_d
integer :: auxhist2_end_h
integer :: auxhist2_end_m
integer :: auxhist2_end_s
integer :: auxhist3_end_y
integer :: auxhist3_end_mo
integer :: auxhist3_end_d
integer :: auxhist3_end_h
integer :: auxhist3_end_m
integer :: auxhist3_end_s
integer :: auxhist4_end_y
integer :: auxhist4_end_mo
integer :: auxhist4_end_d
integer :: auxhist4_end_h
integer :: auxhist4_end_m
integer :: auxhist4_end_s
integer :: auxhist5_end_y
integer :: auxhist5_end_mo
integer :: auxhist5_end_d
integer :: auxhist5_end_h
integer :: auxhist5_end_m
integer :: auxhist5_end_s
integer :: auxhist6_end_y
integer :: auxhist6_end_mo
integer :: auxhist6_end_d
integer :: auxhist6_end_h
integer :: auxhist6_end_m
integer :: auxhist6_end_s
integer :: auxhist7_end_y
integer :: auxhist7_end_mo
integer :: auxhist7_end_d
integer :: auxhist7_end_h
integer :: auxhist7_end_m
integer :: auxhist7_end_s
integer :: auxhist8_end_y
integer :: auxhist8_end_mo
integer :: auxhist8_end_d
integer :: auxhist8_end_h
integer :: auxhist8_end_m
integer :: auxhist8_end_s
integer :: auxhist9_end_y
integer :: auxhist9_end_mo
integer :: auxhist9_end_d
integer :: auxhist9_end_h
integer :: auxhist9_end_m
integer :: auxhist9_end_s
integer :: auxhist10_end_y
integer :: auxhist10_end_mo
integer :: auxhist10_end_d
integer :: auxhist10_end_h
integer :: auxhist10_end_m
integer :: auxhist10_end_s
integer :: auxhist11_end_y
integer :: auxhist11_end_mo
integer :: auxhist11_end_d
integer :: auxhist11_end_h
integer :: auxhist11_end_m
integer :: auxhist11_end_s
integer :: auxinput1_end_y
integer :: auxinput1_end_mo
integer :: auxinput1_end_d
integer :: auxinput1_end_h
integer :: auxinput1_end_m
integer :: auxinput1_end_s
integer :: auxinput2_end_y
integer :: auxinput2_end_mo
integer :: auxinput2_end_d
integer :: auxinput2_end_h
integer :: auxinput2_end_m
integer :: auxinput2_end_s
integer :: auxinput3_end_y
integer :: auxinput3_end_mo
integer :: auxinput3_end_d
integer :: auxinput3_end_h
integer :: auxinput3_end_m
integer :: auxinput3_end_s
integer :: auxinput4_end_y
integer :: auxinput4_end_mo
integer :: auxinput4_end_d
integer :: auxinput4_end_h
integer :: auxinput4_end_m
integer :: auxinput4_end_s
integer :: auxinput5_end_y
integer :: auxinput5_end_mo
integer :: auxinput5_end_d
integer :: auxinput5_end_h
integer :: auxinput5_end_m
integer :: auxinput5_end_s
integer :: auxinput6_end_y
integer :: auxinput6_end_mo
integer :: auxinput6_end_d
integer :: auxinput6_end_h
integer :: auxinput6_end_m
integer :: auxinput6_end_s
integer :: auxinput7_end_y
integer :: auxinput7_end_mo
integer :: auxinput7_end_d
integer :: auxinput7_end_h
integer :: auxinput7_end_m
integer :: auxinput7_end_s
integer :: auxinput8_end_y
integer :: auxinput8_end_mo
integer :: auxinput8_end_d
integer :: auxinput8_end_h
integer :: auxinput8_end_m
integer :: auxinput8_end_s
integer :: sgfdda_end_y
integer :: sgfdda_end_mo
integer :: sgfdda_end_d
integer :: sgfdda_end_h
integer :: sgfdda_end_m
integer :: sgfdda_end_s
integer :: gfdda_end_y
integer :: gfdda_end_mo
integer :: gfdda_end_d
integer :: gfdda_end_h
integer :: gfdda_end_m
integer :: gfdda_end_s
integer :: auxinput11_end_y
integer :: auxinput11_end_mo
integer :: auxinput11_end_d
integer :: auxinput11_end_h
integer :: auxinput11_end_m
integer :: auxinput11_end_s
integer :: io_form_auxinput1
integer :: io_form_auxinput2
integer :: io_form_auxinput3
integer :: io_form_auxinput4
integer :: io_form_auxinput5
integer :: io_form_auxinput6
integer :: io_form_auxinput7
integer :: io_form_auxinput8
integer :: io_form_sgfdda
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
integer :: simulation_start_year
integer :: simulation_start_month
integer :: simulation_start_day
integer :: simulation_start_hour
integer :: simulation_start_minute
integer :: simulation_start_second
logical :: reset_simulation_start
integer :: sr_x
integer :: sr_y
integer :: julyr
integer :: julday
real :: gmt
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
real :: tstart
logical :: nocolons
logical :: cycling
integer :: dfi_opt
integer :: dfi_nfilter
logical :: dfi_write_filtered_input
logical :: dfi_write_dfi_history
integer :: dfi_cutoff_seconds
integer :: dfi_time_dim
integer :: dfi_fwdstop_year
integer :: dfi_fwdstop_month
integer :: dfi_fwdstop_day
integer :: dfi_fwdstop_hour
integer :: dfi_fwdstop_minute
integer :: dfi_fwdstop_second
integer :: dfi_bckstop_year
integer :: dfi_bckstop_month
integer :: dfi_bckstop_day
integer :: dfi_bckstop_hour
integer :: dfi_bckstop_minute
integer :: dfi_bckstop_second
integer :: time_step
integer :: time_step_fract_num
integer :: time_step_fract_den
integer :: max_dom
integer :: s_we
integer :: e_we
integer :: s_sn
integer :: e_sn
integer :: s_vert
integer :: e_vert
real :: dx
real :: dy
integer :: grid_id
logical :: grid_allowed
integer :: parent_id
integer :: i_parent_start
integer :: j_parent_start
integer :: parent_grid_ratio
integer :: parent_time_step_ratio
integer :: feedback
integer :: smooth_option
real :: ztop
integer :: moad_grid_ratio
integer :: moad_time_step_ratio
integer :: shw
integer :: tile_sz_x
integer :: tile_sz_y
integer :: numtiles
integer :: nproc_x
integer :: nproc_y
integer :: irand
real :: dt
integer :: ts_buf_size
integer :: max_ts_locs
integer :: num_moves
integer :: move_id
integer :: move_interval
integer :: move_cd_x
integer :: move_cd_y
logical :: swap_x
logical :: swap_y
logical :: cycle_x
logical :: cycle_y
logical :: reorder_mesh
logical :: perturb_input
real :: eta_levels
real :: ptsgm
integer :: num_metgrid_levels
real :: p_top_requested
integer :: mp_physics
integer :: ra_lw_physics
integer :: ra_sw_physics
real :: radt
integer :: sf_sfclay_physics
integer :: sf_surface_physics
integer :: bl_pbl_physics
integer :: sf_urban_physics
real :: bldt
integer :: cu_physics
real :: cudt
real :: gsmdt
integer :: isfflx
integer :: ifsnow
integer :: icloud
real :: swrad_scat
integer :: surface_input_source
integer :: num_soil_layers
integer :: num_urban_layers
integer :: maxiens
integer :: maxens
integer :: maxens2
integer :: maxens3
integer :: ensdim
integer :: chem_opt
integer :: num_land_cat
integer :: num_soil_cat
integer :: mp_zero_out
real :: mp_zero_out_thresh
real :: seaice_threshold
integer :: fractional_seaice
integer :: sst_update
logical :: usemonalb
logical :: rdmaxalb
logical :: rdlai2d
integer :: gwd_opt
integer :: idtad
integer :: nsoil
integer :: nphs
integer :: ncnvc
integer :: nrads
integer :: nradl
real :: tprec
real :: theat
real :: tclod
real :: trdsw
real :: trdlw
real :: tsrfc
logical :: pcpflg
integer :: sigma
real :: sfenth
integer :: co2tf
integer :: ra_call_offset
real :: cam_abs_freq_s
integer :: levsiz
integer :: paerlev
integer :: cam_abs_dim1
integer :: cam_abs_dim2
logical :: cu_rad_feedback
integer :: dyn_opt
integer :: rk_ord
integer :: w_damping
integer :: diff_opt
integer :: km_opt
integer :: damp_opt
real :: zdamp
real :: base_pres
real :: base_temp
real :: base_lapse
real :: iso_temp
real :: dampcoef
real :: khdif
real :: kvdif
real :: c_s
real :: c_k
real :: smdiv
real :: emdiv
real :: epssm
logical :: non_hydrostatic
integer :: time_step_sound
integer :: h_mom_adv_order
integer :: v_mom_adv_order
integer :: h_sca_adv_order
integer :: v_sca_adv_order
logical :: top_radiation
real :: tke_upper_bound
real :: tke_drag_coefficient
real :: tke_heat_flux
logical :: pert_coriolis
logical :: euler_adv
integer :: idtadt
integer :: idtadc
logical :: boundary_flux
integer :: spec_bdy_width
integer :: spec_zone
integer :: relax_zone
logical :: specified
logical :: periodic_x
logical :: symmetric_xs
logical :: symmetric_xe
logical :: open_xs
logical :: open_xe
logical :: periodic_y
logical :: symmetric_ys
logical :: symmetric_ye
logical :: open_ys
logical :: open_ye
logical :: polar
logical :: nested
integer :: real_data_init_type
integer :: background_proc_id
integer :: forecast_proc_id
integer :: production_status
integer :: compression
real :: cen_lat
real :: cen_lon
real :: truelat1
real :: truelat2
real :: moad_cen_lat
real :: stand_lon
integer :: flag_metgrid
integer :: flag_snow
integer :: flag_psfc
integer :: flag_sm000010
integer :: flag_sm010040
integer :: flag_sm040100
integer :: flag_sm100200
integer :: flag_st000010
integer :: flag_st010040
integer :: flag_st040100
integer :: flag_st100200
integer :: flag_slp
integer :: flag_soilhgt
integer :: flag_mf_xy
real :: bdyfrq
character*256 :: mminlu
integer :: iswater
integer :: islake
integer :: isice
integer :: isurban
integer :: isoilwater
integer :: map_proj
integer :: dfi_stage
integer :: mp_physics_dfi
integer    :: last_item_in_struct

   END TYPE grid_config_rec_type

   TYPE(model_config_rec_type) :: model_config_rec







CONTAINS




   SUBROUTINE initial_config







































      IMPLICIT NONE

      INTEGER              :: io_status
      INTEGER              :: i

      LOGICAL              :: nml_read_error

      CHARACTER (LEN=1024) :: nml_name

      INTEGER, PARAMETER :: nml_write_unit= 9
      INTEGER, PARAMETER :: nml_read_unit = 10









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
character*256 :: auxinput1_inname
character*256 :: auxinput2_inname
character*256 :: auxinput3_inname
character*256 :: auxinput4_inname
character*256 :: auxinput5_inname
character*256 :: auxinput6_inname
character*256 :: auxinput7_inname
character*256 :: auxinput8_inname
character*256 :: sgfdda_inname
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
integer , DIMENSION(max_domains) :: sgfdda_interval_mo
integer , DIMENSION(max_domains) :: sgfdda_interval_d
integer , DIMENSION(max_domains) :: sgfdda_interval_h
integer , DIMENSION(max_domains) :: sgfdda_interval_m
integer , DIMENSION(max_domains) :: sgfdda_interval_s
integer , DIMENSION(max_domains) :: sgfdda_interval
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
integer , DIMENSION(max_domains) :: sgfdda_begin_y
integer , DIMENSION(max_domains) :: sgfdda_begin_mo
integer , DIMENSION(max_domains) :: sgfdda_begin_d
integer , DIMENSION(max_domains) :: sgfdda_begin_h
integer , DIMENSION(max_domains) :: sgfdda_begin_m
integer , DIMENSION(max_domains) :: sgfdda_begin_s
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
integer , DIMENSION(max_domains) :: sgfdda_end_y
integer , DIMENSION(max_domains) :: sgfdda_end_mo
integer , DIMENSION(max_domains) :: sgfdda_end_d
integer , DIMENSION(max_domains) :: sgfdda_end_h
integer , DIMENSION(max_domains) :: sgfdda_end_m
integer , DIMENSION(max_domains) :: sgfdda_end_s
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
integer :: io_form_sgfdda
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
integer :: simulation_start_year
integer :: simulation_start_month
integer :: simulation_start_day
integer :: simulation_start_hour
integer :: simulation_start_minute
integer :: simulation_start_second
logical :: reset_simulation_start
integer , DIMENSION(max_domains) :: sr_x
integer , DIMENSION(max_domains) :: sr_y
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
real , DIMENSION(max_domains) :: tstart
logical :: nocolons
logical :: cycling
integer :: dfi_opt
integer :: dfi_nfilter
logical :: dfi_write_filtered_input
logical :: dfi_write_dfi_history
integer :: dfi_cutoff_seconds
integer :: dfi_time_dim
integer :: dfi_fwdstop_year
integer :: dfi_fwdstop_month
integer :: dfi_fwdstop_day
integer :: dfi_fwdstop_hour
integer :: dfi_fwdstop_minute
integer :: dfi_fwdstop_second
integer :: dfi_bckstop_year
integer :: dfi_bckstop_month
integer :: dfi_bckstop_day
integer :: dfi_bckstop_hour
integer :: dfi_bckstop_minute
integer :: dfi_bckstop_second
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
real , DIMENSION(max_domains) :: dx
real , DIMENSION(max_domains) :: dy
integer , DIMENSION(max_domains) :: grid_id
logical , DIMENSION(max_domains) :: grid_allowed
integer , DIMENSION(max_domains) :: parent_id
integer , DIMENSION(max_domains) :: i_parent_start
integer , DIMENSION(max_domains) :: j_parent_start
integer , DIMENSION(max_domains) :: parent_grid_ratio
integer , DIMENSION(max_domains) :: parent_time_step_ratio
integer :: feedback
integer :: smooth_option
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
integer :: ts_buf_size
integer :: max_ts_locs
integer :: num_moves
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
real :: ptsgm
integer :: num_metgrid_levels
real :: p_top_requested
integer , DIMENSION(max_domains) :: mp_physics
integer , DIMENSION(max_domains) :: ra_lw_physics
integer , DIMENSION(max_domains) :: ra_sw_physics
real , DIMENSION(max_domains) :: radt
integer , DIMENSION(max_domains) :: sf_sfclay_physics
integer , DIMENSION(max_domains) :: sf_surface_physics
integer , DIMENSION(max_domains) :: bl_pbl_physics
integer , DIMENSION(max_domains) :: sf_urban_physics
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
integer :: num_urban_layers
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
integer :: fractional_seaice
integer :: sst_update
logical :: usemonalb
logical :: rdmaxalb
logical :: rdlai2d
integer , DIMENSION(max_domains) :: gwd_opt
integer , DIMENSION(max_domains) :: idtad
integer , DIMENSION(max_domains) :: nsoil
integer , DIMENSION(max_domains) :: nphs
integer , DIMENSION(max_domains) :: ncnvc
integer , DIMENSION(max_domains) :: nrads
integer , DIMENSION(max_domains) :: nradl
real , DIMENSION(max_domains) :: tprec
real , DIMENSION(max_domains) :: theat
real , DIMENSION(max_domains) :: tclod
real , DIMENSION(max_domains) :: trdsw
real , DIMENSION(max_domains) :: trdlw
real , DIMENSION(max_domains) :: tsrfc
logical , DIMENSION(max_domains) :: pcpflg
integer , DIMENSION(max_domains) :: sigma
real , DIMENSION(max_domains) :: sfenth
integer :: co2tf
integer :: ra_call_offset
real :: cam_abs_freq_s
integer :: levsiz
integer :: paerlev
integer :: cam_abs_dim1
integer :: cam_abs_dim2
logical , DIMENSION(max_domains) :: cu_rad_feedback
integer :: dyn_opt
integer :: rk_ord
integer :: w_damping
integer :: diff_opt
integer :: km_opt
integer :: damp_opt
real , DIMENSION(max_domains) :: zdamp
real :: base_pres
real :: base_temp
real :: base_lapse
real :: iso_temp
real , DIMENSION(max_domains) :: dampcoef
real , DIMENSION(max_domains) :: khdif
real , DIMENSION(max_domains) :: kvdif
real , DIMENSION(max_domains) :: c_s
real , DIMENSION(max_domains) :: c_k
real , DIMENSION(max_domains) :: smdiv
real , DIMENSION(max_domains) :: emdiv
real , DIMENSION(max_domains) :: epssm
logical , DIMENSION(max_domains) :: non_hydrostatic
integer , DIMENSION(max_domains) :: time_step_sound
integer , DIMENSION(max_domains) :: h_mom_adv_order
integer , DIMENSION(max_domains) :: v_mom_adv_order
integer , DIMENSION(max_domains) :: h_sca_adv_order
integer , DIMENSION(max_domains) :: v_sca_adv_order
logical , DIMENSION(max_domains) :: top_radiation
real , DIMENSION(max_domains) :: tke_upper_bound
real , DIMENSION(max_domains) :: tke_drag_coefficient
real , DIMENSION(max_domains) :: tke_heat_flux
logical , DIMENSION(max_domains) :: pert_coriolis
logical :: euler_adv
integer :: idtadt
integer :: idtadc
logical :: boundary_flux
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
logical , DIMENSION(max_domains) :: polar
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
integer :: flag_metgrid
integer :: flag_snow
integer :: flag_psfc
integer :: flag_sm000010
integer :: flag_sm010040
integer :: flag_sm040100
integer :: flag_sm100200
integer :: flag_st000010
integer :: flag_st010040
integer :: flag_st040100
integer :: flag_st100200
integer :: flag_slp
integer :: flag_soilhgt
integer :: flag_mf_xy
real , DIMENSION(max_domains) :: bdyfrq
character*256 , DIMENSION(max_domains) :: mminlu
integer , DIMENSION(max_domains) :: iswater
integer , DIMENSION(max_domains) :: islake
integer , DIMENSION(max_domains) :: isice
integer , DIMENSION(max_domains) :: isurban
integer , DIMENSION(max_domains) :: isoilwater
integer , DIMENSION(max_domains) :: map_proj
integer :: dfi_stage
integer , DIMENSION(max_domains) :: mp_physics_dfi
integer    :: last_item_in_struct









NAMELIST /time_control/ run_days
NAMELIST /time_control/ run_hours
NAMELIST /time_control/ run_minutes
NAMELIST /time_control/ run_seconds
NAMELIST /time_control/ start_year
NAMELIST /time_control/ start_month
NAMELIST /time_control/ start_day
NAMELIST /time_control/ start_hour
NAMELIST /time_control/ start_minute
NAMELIST /time_control/ start_second
NAMELIST /time_control/ end_year
NAMELIST /time_control/ end_month
NAMELIST /time_control/ end_day
NAMELIST /time_control/ end_hour
NAMELIST /time_control/ end_minute
NAMELIST /time_control/ end_second
NAMELIST /time_control/ interval_seconds
NAMELIST /time_control/ input_from_file
NAMELIST /time_control/ fine_input_stream
NAMELIST /time_control/ history_interval
NAMELIST /time_control/ frames_per_outfile
NAMELIST /time_control/ frames_per_auxhist1
NAMELIST /time_control/ frames_per_auxhist2
NAMELIST /time_control/ frames_per_auxhist3
NAMELIST /time_control/ frames_per_auxhist4
NAMELIST /time_control/ frames_per_auxhist5
NAMELIST /time_control/ frames_per_auxhist6
NAMELIST /time_control/ frames_per_auxhist7
NAMELIST /time_control/ frames_per_auxhist8
NAMELIST /time_control/ frames_per_auxhist9
NAMELIST /time_control/ frames_per_auxhist10
NAMELIST /time_control/ frames_per_auxhist11
NAMELIST /time_control/ restart
NAMELIST /time_control/ restart_interval
NAMELIST /time_control/ io_form_input
NAMELIST /time_control/ io_form_history
NAMELIST /time_control/ io_form_restart
NAMELIST /time_control/ io_form_boundary
NAMELIST /time_control/ debug_level
NAMELIST /time_control/ self_test_domain
NAMELIST /time_control/ history_outname
NAMELIST /time_control/ auxhist1_outname
NAMELIST /time_control/ auxhist2_outname
NAMELIST /time_control/ auxhist3_outname
NAMELIST /time_control/ auxhist4_outname
NAMELIST /time_control/ auxhist5_outname
NAMELIST /time_control/ auxhist6_outname
NAMELIST /time_control/ auxhist7_outname
NAMELIST /time_control/ auxhist8_outname
NAMELIST /time_control/ auxhist9_outname
NAMELIST /time_control/ auxhist10_outname
NAMELIST /time_control/ auxhist11_outname
NAMELIST /time_control/ history_inname
NAMELIST /time_control/ auxhist1_inname
NAMELIST /time_control/ auxhist2_inname
NAMELIST /time_control/ auxhist3_inname
NAMELIST /time_control/ auxhist4_inname
NAMELIST /time_control/ auxhist5_inname
NAMELIST /time_control/ auxhist6_inname
NAMELIST /time_control/ auxhist7_inname
NAMELIST /time_control/ auxhist8_inname
NAMELIST /time_control/ auxhist9_inname
NAMELIST /time_control/ auxhist10_inname
NAMELIST /time_control/ auxhist11_inname
NAMELIST /time_control/ auxinput1_outname
NAMELIST /time_control/ auxinput2_outname
NAMELIST /time_control/ auxinput3_outname
NAMELIST /time_control/ auxinput4_outname
NAMELIST /time_control/ auxinput5_outname
NAMELIST /time_control/ auxinput6_outname
NAMELIST /time_control/ auxinput7_outname
NAMELIST /time_control/ auxinput8_outname
NAMELIST /time_control/ auxinput9_outname
NAMELIST /time_control/ auxinput10_outname
NAMELIST /time_control/ auxinput11_outname
NAMELIST /time_control/ auxinput1_inname
NAMELIST /time_control/ auxinput2_inname
NAMELIST /time_control/ auxinput3_inname
NAMELIST /time_control/ auxinput4_inname
NAMELIST /time_control/ auxinput5_inname
NAMELIST /time_control/ auxinput6_inname
NAMELIST /time_control/ auxinput7_inname
NAMELIST /time_control/ auxinput8_inname
NAMELIST /fdda/ sgfdda_inname
NAMELIST /fdda/ gfdda_inname
NAMELIST /time_control/ auxinput11_inname
NAMELIST /time_control/ history_interval_mo
NAMELIST /time_control/ history_interval_d
NAMELIST /time_control/ history_interval_h
NAMELIST /time_control/ history_interval_m
NAMELIST /time_control/ history_interval_s
NAMELIST /time_control/ inputout_interval_mo
NAMELIST /time_control/ inputout_interval_d
NAMELIST /time_control/ inputout_interval_h
NAMELIST /time_control/ inputout_interval_m
NAMELIST /time_control/ inputout_interval_s
NAMELIST /time_control/ inputout_interval
NAMELIST /time_control/ auxhist1_interval_mo
NAMELIST /time_control/ auxhist1_interval_d
NAMELIST /time_control/ auxhist1_interval_h
NAMELIST /time_control/ auxhist1_interval_m
NAMELIST /time_control/ auxhist1_interval_s
NAMELIST /time_control/ auxhist1_interval
NAMELIST /time_control/ auxhist2_interval_mo
NAMELIST /time_control/ auxhist2_interval_d
NAMELIST /time_control/ auxhist2_interval_h
NAMELIST /time_control/ auxhist2_interval_m
NAMELIST /time_control/ auxhist2_interval_s
NAMELIST /time_control/ auxhist2_interval
NAMELIST /time_control/ auxhist3_interval_mo
NAMELIST /time_control/ auxhist3_interval_d
NAMELIST /time_control/ auxhist3_interval_h
NAMELIST /time_control/ auxhist3_interval_m
NAMELIST /time_control/ auxhist3_interval_s
NAMELIST /time_control/ auxhist3_interval
NAMELIST /time_control/ auxhist4_interval_mo
NAMELIST /time_control/ auxhist4_interval_d
NAMELIST /time_control/ auxhist4_interval_h
NAMELIST /time_control/ auxhist4_interval_m
NAMELIST /time_control/ auxhist4_interval_s
NAMELIST /time_control/ auxhist4_interval
NAMELIST /time_control/ auxhist5_interval_mo
NAMELIST /time_control/ auxhist5_interval_d
NAMELIST /time_control/ auxhist5_interval_h
NAMELIST /time_control/ auxhist5_interval_m
NAMELIST /time_control/ auxhist5_interval_s
NAMELIST /time_control/ auxhist5_interval
NAMELIST /time_control/ auxhist6_interval_mo
NAMELIST /time_control/ auxhist6_interval_d
NAMELIST /time_control/ auxhist6_interval_h
NAMELIST /time_control/ auxhist6_interval_m
NAMELIST /time_control/ auxhist6_interval_s
NAMELIST /time_control/ auxhist6_interval
NAMELIST /time_control/ auxhist7_interval_mo
NAMELIST /time_control/ auxhist7_interval_d
NAMELIST /time_control/ auxhist7_interval_h
NAMELIST /time_control/ auxhist7_interval_m
NAMELIST /time_control/ auxhist7_interval_s
NAMELIST /time_control/ auxhist7_interval
NAMELIST /time_control/ auxhist8_interval_mo
NAMELIST /time_control/ auxhist8_interval_d
NAMELIST /time_control/ auxhist8_interval_h
NAMELIST /time_control/ auxhist8_interval_m
NAMELIST /time_control/ auxhist8_interval_s
NAMELIST /time_control/ auxhist8_interval
NAMELIST /time_control/ auxhist9_interval_mo
NAMELIST /time_control/ auxhist9_interval_d
NAMELIST /time_control/ auxhist9_interval_h
NAMELIST /time_control/ auxhist9_interval_m
NAMELIST /time_control/ auxhist9_interval_s
NAMELIST /time_control/ auxhist9_interval
NAMELIST /time_control/ auxhist10_interval_mo
NAMELIST /time_control/ auxhist10_interval_d
NAMELIST /time_control/ auxhist10_interval_h
NAMELIST /time_control/ auxhist10_interval_m
NAMELIST /time_control/ auxhist10_interval_s
NAMELIST /time_control/ auxhist10_interval
NAMELIST /time_control/ auxhist11_interval_mo
NAMELIST /time_control/ auxhist11_interval_d
NAMELIST /time_control/ auxhist11_interval_h
NAMELIST /time_control/ auxhist11_interval_m
NAMELIST /time_control/ auxhist11_interval_s
NAMELIST /time_control/ auxhist11_interval
NAMELIST /time_control/ auxinput1_interval_mo
NAMELIST /time_control/ auxinput1_interval_d
NAMELIST /time_control/ auxinput1_interval_h
NAMELIST /time_control/ auxinput1_interval_m
NAMELIST /time_control/ auxinput1_interval_s
NAMELIST /time_control/ auxinput1_interval
NAMELIST /time_control/ auxinput2_interval_mo
NAMELIST /time_control/ auxinput2_interval_d
NAMELIST /time_control/ auxinput2_interval_h
NAMELIST /time_control/ auxinput2_interval_m
NAMELIST /time_control/ auxinput2_interval_s
NAMELIST /time_control/ auxinput2_interval
NAMELIST /time_control/ auxinput3_interval_mo
NAMELIST /time_control/ auxinput3_interval_d
NAMELIST /time_control/ auxinput3_interval_h
NAMELIST /time_control/ auxinput3_interval_m
NAMELIST /time_control/ auxinput3_interval_s
NAMELIST /time_control/ auxinput3_interval
NAMELIST /time_control/ auxinput4_interval_mo
NAMELIST /time_control/ auxinput4_interval_d
NAMELIST /time_control/ auxinput4_interval_h
NAMELIST /time_control/ auxinput4_interval_m
NAMELIST /time_control/ auxinput4_interval_s
NAMELIST /time_control/ auxinput4_interval
NAMELIST /time_control/ auxinput5_interval_mo
NAMELIST /time_control/ auxinput5_interval_d
NAMELIST /time_control/ auxinput5_interval_h
NAMELIST /time_control/ auxinput5_interval_m
NAMELIST /time_control/ auxinput5_interval_s
NAMELIST /time_control/ auxinput5_interval
NAMELIST /time_control/ auxinput6_interval_mo
NAMELIST /time_control/ auxinput6_interval_d
NAMELIST /time_control/ auxinput6_interval_h
NAMELIST /time_control/ auxinput6_interval_m
NAMELIST /time_control/ auxinput6_interval_s
NAMELIST /time_control/ auxinput6_interval
NAMELIST /time_control/ auxinput7_interval_mo
NAMELIST /time_control/ auxinput7_interval_d
NAMELIST /time_control/ auxinput7_interval_h
NAMELIST /time_control/ auxinput7_interval_m
NAMELIST /time_control/ auxinput7_interval_s
NAMELIST /time_control/ auxinput7_interval
NAMELIST /time_control/ auxinput8_interval_mo
NAMELIST /time_control/ auxinput8_interval_d
NAMELIST /time_control/ auxinput8_interval_h
NAMELIST /time_control/ auxinput8_interval_m
NAMELIST /time_control/ auxinput8_interval_s
NAMELIST /time_control/ auxinput8_interval
NAMELIST /fdda/ sgfdda_interval_mo
NAMELIST /fdda/ sgfdda_interval_d
NAMELIST /fdda/ sgfdda_interval_h
NAMELIST /fdda/ sgfdda_interval_m
NAMELIST /fdda/ sgfdda_interval_s
NAMELIST /fdda/ sgfdda_interval
NAMELIST /fdda/ gfdda_interval_mo
NAMELIST /fdda/ gfdda_interval_d
NAMELIST /fdda/ gfdda_interval_h
NAMELIST /fdda/ gfdda_interval_m
NAMELIST /fdda/ gfdda_interval_s
NAMELIST /fdda/ gfdda_interval
NAMELIST /time_control/ auxinput11_interval_mo
NAMELIST /time_control/ auxinput11_interval_d
NAMELIST /time_control/ auxinput11_interval_h
NAMELIST /time_control/ auxinput11_interval_m
NAMELIST /time_control/ auxinput11_interval_s
NAMELIST /time_control/ auxinput11_interval
NAMELIST /time_control/ restart_interval_mo
NAMELIST /time_control/ restart_interval_d
NAMELIST /time_control/ restart_interval_h
NAMELIST /time_control/ restart_interval_m
NAMELIST /time_control/ restart_interval_s
NAMELIST /time_control/ history_begin_y
NAMELIST /time_control/ history_begin_mo
NAMELIST /time_control/ history_begin_d
NAMELIST /time_control/ history_begin_h
NAMELIST /time_control/ history_begin_m
NAMELIST /time_control/ history_begin_s
NAMELIST /time_control/ inputout_begin_y
NAMELIST /time_control/ inputout_begin_mo
NAMELIST /time_control/ inputout_begin_d
NAMELIST /time_control/ inputout_begin_h
NAMELIST /time_control/ inputout_begin_m
NAMELIST /time_control/ inputout_begin_s
NAMELIST /time_control/ auxhist1_begin_y
NAMELIST /time_control/ auxhist1_begin_mo
NAMELIST /time_control/ auxhist1_begin_d
NAMELIST /time_control/ auxhist1_begin_h
NAMELIST /time_control/ auxhist1_begin_m
NAMELIST /time_control/ auxhist1_begin_s
NAMELIST /time_control/ auxhist2_begin_y
NAMELIST /time_control/ auxhist2_begin_mo
NAMELIST /time_control/ auxhist2_begin_d
NAMELIST /time_control/ auxhist2_begin_h
NAMELIST /time_control/ auxhist2_begin_m
NAMELIST /time_control/ auxhist2_begin_s
NAMELIST /time_control/ auxhist3_begin_y
NAMELIST /time_control/ auxhist3_begin_mo
NAMELIST /time_control/ auxhist3_begin_d
NAMELIST /time_control/ auxhist3_begin_h
NAMELIST /time_control/ auxhist3_begin_m
NAMELIST /time_control/ auxhist3_begin_s
NAMELIST /time_control/ auxhist4_begin_y
NAMELIST /time_control/ auxhist4_begin_mo
NAMELIST /time_control/ auxhist4_begin_d
NAMELIST /time_control/ auxhist4_begin_h
NAMELIST /time_control/ auxhist4_begin_m
NAMELIST /time_control/ auxhist4_begin_s
NAMELIST /time_control/ auxhist5_begin_y
NAMELIST /time_control/ auxhist5_begin_mo
NAMELIST /time_control/ auxhist5_begin_d
NAMELIST /time_control/ auxhist5_begin_h
NAMELIST /time_control/ auxhist5_begin_m
NAMELIST /time_control/ auxhist5_begin_s
NAMELIST /time_control/ auxhist6_begin_y
NAMELIST /time_control/ auxhist6_begin_mo
NAMELIST /time_control/ auxhist6_begin_d
NAMELIST /time_control/ auxhist6_begin_h
NAMELIST /time_control/ auxhist6_begin_m
NAMELIST /time_control/ auxhist6_begin_s
NAMELIST /time_control/ auxhist7_begin_y
NAMELIST /time_control/ auxhist7_begin_mo
NAMELIST /time_control/ auxhist7_begin_d
NAMELIST /time_control/ auxhist7_begin_h
NAMELIST /time_control/ auxhist7_begin_m
NAMELIST /time_control/ auxhist7_begin_s
NAMELIST /time_control/ auxhist8_begin_y
NAMELIST /time_control/ auxhist8_begin_mo
NAMELIST /time_control/ auxhist8_begin_d
NAMELIST /time_control/ auxhist8_begin_h
NAMELIST /time_control/ auxhist8_begin_m
NAMELIST /time_control/ auxhist8_begin_s
NAMELIST /time_control/ auxhist9_begin_y
NAMELIST /time_control/ auxhist9_begin_mo
NAMELIST /time_control/ auxhist9_begin_d
NAMELIST /time_control/ auxhist9_begin_h
NAMELIST /time_control/ auxhist9_begin_m
NAMELIST /time_control/ auxhist9_begin_s
NAMELIST /time_control/ auxhist10_begin_y
NAMELIST /time_control/ auxhist10_begin_mo
NAMELIST /time_control/ auxhist10_begin_d
NAMELIST /time_control/ auxhist10_begin_h
NAMELIST /time_control/ auxhist10_begin_m
NAMELIST /time_control/ auxhist10_begin_s
NAMELIST /time_control/ auxhist11_begin_y
NAMELIST /time_control/ auxhist11_begin_mo
NAMELIST /time_control/ auxhist11_begin_d
NAMELIST /time_control/ auxhist11_begin_h
NAMELIST /time_control/ auxhist11_begin_m
NAMELIST /time_control/ auxhist11_begin_s
NAMELIST /time_control/ auxinput1_begin_y
NAMELIST /time_control/ auxinput1_begin_mo
NAMELIST /time_control/ auxinput1_begin_d
NAMELIST /time_control/ auxinput1_begin_h
NAMELIST /time_control/ auxinput1_begin_m
NAMELIST /time_control/ auxinput1_begin_s
NAMELIST /time_control/ auxinput2_begin_y
NAMELIST /time_control/ auxinput2_begin_mo
NAMELIST /time_control/ auxinput2_begin_d
NAMELIST /time_control/ auxinput2_begin_h
NAMELIST /time_control/ auxinput2_begin_m
NAMELIST /time_control/ auxinput2_begin_s
NAMELIST /time_control/ auxinput3_begin_y
NAMELIST /time_control/ auxinput3_begin_mo
NAMELIST /time_control/ auxinput3_begin_d
NAMELIST /time_control/ auxinput3_begin_h
NAMELIST /time_control/ auxinput3_begin_m
NAMELIST /time_control/ auxinput3_begin_s
NAMELIST /time_control/ auxinput4_begin_y
NAMELIST /time_control/ auxinput4_begin_mo
NAMELIST /time_control/ auxinput4_begin_d
NAMELIST /time_control/ auxinput4_begin_h
NAMELIST /time_control/ auxinput4_begin_m
NAMELIST /time_control/ auxinput4_begin_s
NAMELIST /time_control/ auxinput5_begin_y
NAMELIST /time_control/ auxinput5_begin_mo
NAMELIST /time_control/ auxinput5_begin_d
NAMELIST /time_control/ auxinput5_begin_h
NAMELIST /time_control/ auxinput5_begin_m
NAMELIST /time_control/ auxinput5_begin_s
NAMELIST /time_control/ auxinput6_begin_y
NAMELIST /time_control/ auxinput6_begin_mo
NAMELIST /time_control/ auxinput6_begin_d
NAMELIST /time_control/ auxinput6_begin_h
NAMELIST /time_control/ auxinput6_begin_m
NAMELIST /time_control/ auxinput6_begin_s
NAMELIST /time_control/ auxinput7_begin_y
NAMELIST /time_control/ auxinput7_begin_mo
NAMELIST /time_control/ auxinput7_begin_d
NAMELIST /time_control/ auxinput7_begin_h
NAMELIST /time_control/ auxinput7_begin_m
NAMELIST /time_control/ auxinput7_begin_s
NAMELIST /time_control/ auxinput8_begin_y
NAMELIST /time_control/ auxinput8_begin_mo
NAMELIST /time_control/ auxinput8_begin_d
NAMELIST /time_control/ auxinput8_begin_h
NAMELIST /time_control/ auxinput8_begin_m
NAMELIST /time_control/ auxinput8_begin_s
NAMELIST /fdda/ sgfdda_begin_y
NAMELIST /fdda/ sgfdda_begin_mo
NAMELIST /fdda/ sgfdda_begin_d
NAMELIST /fdda/ sgfdda_begin_h
NAMELIST /fdda/ sgfdda_begin_m
NAMELIST /fdda/ sgfdda_begin_s
NAMELIST /fdda/ gfdda_begin_y
NAMELIST /fdda/ gfdda_begin_mo
NAMELIST /fdda/ gfdda_begin_d
NAMELIST /fdda/ gfdda_begin_h
NAMELIST /fdda/ gfdda_begin_m
NAMELIST /fdda/ gfdda_begin_s
NAMELIST /time_control/ auxinput11_begin_y
NAMELIST /time_control/ auxinput11_begin_mo
NAMELIST /time_control/ auxinput11_begin_d
NAMELIST /time_control/ auxinput11_begin_h
NAMELIST /time_control/ auxinput11_begin_m
NAMELIST /time_control/ auxinput11_begin_s
NAMELIST /time_control/ restart_begin_y
NAMELIST /time_control/ restart_begin_mo
NAMELIST /time_control/ restart_begin_d
NAMELIST /time_control/ restart_begin_h
NAMELIST /time_control/ restart_begin_m
NAMELIST /time_control/ restart_begin_s
NAMELIST /time_control/ history_end_y
NAMELIST /time_control/ history_end_mo
NAMELIST /time_control/ history_end_d
NAMELIST /time_control/ history_end_h
NAMELIST /time_control/ history_end_m
NAMELIST /time_control/ history_end_s
NAMELIST /time_control/ inputout_end_y
NAMELIST /time_control/ inputout_end_mo
NAMELIST /time_control/ inputout_end_d
NAMELIST /time_control/ inputout_end_h
NAMELIST /time_control/ inputout_end_m
NAMELIST /time_control/ inputout_end_s
NAMELIST /time_control/ auxhist1_end_y
NAMELIST /time_control/ auxhist1_end_mo
NAMELIST /time_control/ auxhist1_end_d
NAMELIST /time_control/ auxhist1_end_h
NAMELIST /time_control/ auxhist1_end_m
NAMELIST /time_control/ auxhist1_end_s
NAMELIST /time_control/ auxhist2_end_y
NAMELIST /time_control/ auxhist2_end_mo
NAMELIST /time_control/ auxhist2_end_d
NAMELIST /time_control/ auxhist2_end_h
NAMELIST /time_control/ auxhist2_end_m
NAMELIST /time_control/ auxhist2_end_s
NAMELIST /time_control/ auxhist3_end_y
NAMELIST /time_control/ auxhist3_end_mo
NAMELIST /time_control/ auxhist3_end_d
NAMELIST /time_control/ auxhist3_end_h
NAMELIST /time_control/ auxhist3_end_m
NAMELIST /time_control/ auxhist3_end_s
NAMELIST /time_control/ auxhist4_end_y
NAMELIST /time_control/ auxhist4_end_mo
NAMELIST /time_control/ auxhist4_end_d
NAMELIST /time_control/ auxhist4_end_h
NAMELIST /time_control/ auxhist4_end_m
NAMELIST /time_control/ auxhist4_end_s
NAMELIST /time_control/ auxhist5_end_y
NAMELIST /time_control/ auxhist5_end_mo
NAMELIST /time_control/ auxhist5_end_d
NAMELIST /time_control/ auxhist5_end_h
NAMELIST /time_control/ auxhist5_end_m
NAMELIST /time_control/ auxhist5_end_s
NAMELIST /time_control/ auxhist6_end_y
NAMELIST /time_control/ auxhist6_end_mo
NAMELIST /time_control/ auxhist6_end_d
NAMELIST /time_control/ auxhist6_end_h
NAMELIST /time_control/ auxhist6_end_m
NAMELIST /time_control/ auxhist6_end_s
NAMELIST /time_control/ auxhist7_end_y
NAMELIST /time_control/ auxhist7_end_mo
NAMELIST /time_control/ auxhist7_end_d
NAMELIST /time_control/ auxhist7_end_h
NAMELIST /time_control/ auxhist7_end_m
NAMELIST /time_control/ auxhist7_end_s
NAMELIST /time_control/ auxhist8_end_y
NAMELIST /time_control/ auxhist8_end_mo
NAMELIST /time_control/ auxhist8_end_d
NAMELIST /time_control/ auxhist8_end_h
NAMELIST /time_control/ auxhist8_end_m
NAMELIST /time_control/ auxhist8_end_s
NAMELIST /time_control/ auxhist9_end_y
NAMELIST /time_control/ auxhist9_end_mo
NAMELIST /time_control/ auxhist9_end_d
NAMELIST /time_control/ auxhist9_end_h
NAMELIST /time_control/ auxhist9_end_m
NAMELIST /time_control/ auxhist9_end_s
NAMELIST /time_control/ auxhist10_end_y
NAMELIST /time_control/ auxhist10_end_mo
NAMELIST /time_control/ auxhist10_end_d
NAMELIST /time_control/ auxhist10_end_h
NAMELIST /time_control/ auxhist10_end_m
NAMELIST /time_control/ auxhist10_end_s
NAMELIST /time_control/ auxhist11_end_y
NAMELIST /time_control/ auxhist11_end_mo
NAMELIST /time_control/ auxhist11_end_d
NAMELIST /time_control/ auxhist11_end_h
NAMELIST /time_control/ auxhist11_end_m
NAMELIST /time_control/ auxhist11_end_s
NAMELIST /time_control/ auxinput1_end_y
NAMELIST /time_control/ auxinput1_end_mo
NAMELIST /time_control/ auxinput1_end_d
NAMELIST /time_control/ auxinput1_end_h
NAMELIST /time_control/ auxinput1_end_m
NAMELIST /time_control/ auxinput1_end_s
NAMELIST /time_control/ auxinput2_end_y
NAMELIST /time_control/ auxinput2_end_mo
NAMELIST /time_control/ auxinput2_end_d
NAMELIST /time_control/ auxinput2_end_h
NAMELIST /time_control/ auxinput2_end_m
NAMELIST /time_control/ auxinput2_end_s
NAMELIST /time_control/ auxinput3_end_y
NAMELIST /time_control/ auxinput3_end_mo
NAMELIST /time_control/ auxinput3_end_d
NAMELIST /time_control/ auxinput3_end_h
NAMELIST /time_control/ auxinput3_end_m
NAMELIST /time_control/ auxinput3_end_s
NAMELIST /time_control/ auxinput4_end_y
NAMELIST /time_control/ auxinput4_end_mo
NAMELIST /time_control/ auxinput4_end_d
NAMELIST /time_control/ auxinput4_end_h
NAMELIST /time_control/ auxinput4_end_m
NAMELIST /time_control/ auxinput4_end_s
NAMELIST /time_control/ auxinput5_end_y
NAMELIST /time_control/ auxinput5_end_mo
NAMELIST /time_control/ auxinput5_end_d
NAMELIST /time_control/ auxinput5_end_h
NAMELIST /time_control/ auxinput5_end_m
NAMELIST /time_control/ auxinput5_end_s
NAMELIST /time_control/ auxinput6_end_y
NAMELIST /time_control/ auxinput6_end_mo
NAMELIST /time_control/ auxinput6_end_d
NAMELIST /time_control/ auxinput6_end_h
NAMELIST /time_control/ auxinput6_end_m
NAMELIST /time_control/ auxinput6_end_s
NAMELIST /time_control/ auxinput7_end_y
NAMELIST /time_control/ auxinput7_end_mo
NAMELIST /time_control/ auxinput7_end_d
NAMELIST /time_control/ auxinput7_end_h
NAMELIST /time_control/ auxinput7_end_m
NAMELIST /time_control/ auxinput7_end_s
NAMELIST /time_control/ auxinput8_end_y
NAMELIST /time_control/ auxinput8_end_mo
NAMELIST /time_control/ auxinput8_end_d
NAMELIST /time_control/ auxinput8_end_h
NAMELIST /time_control/ auxinput8_end_m
NAMELIST /time_control/ auxinput8_end_s
NAMELIST /fdda/ sgfdda_end_y
NAMELIST /fdda/ sgfdda_end_mo
NAMELIST /fdda/ sgfdda_end_d
NAMELIST /fdda/ sgfdda_end_h
NAMELIST /fdda/ sgfdda_end_m
NAMELIST /fdda/ sgfdda_end_s
NAMELIST /fdda/ gfdda_end_y
NAMELIST /fdda/ gfdda_end_mo
NAMELIST /fdda/ gfdda_end_d
NAMELIST /fdda/ gfdda_end_h
NAMELIST /fdda/ gfdda_end_m
NAMELIST /fdda/ gfdda_end_s
NAMELIST /time_control/ auxinput11_end_y
NAMELIST /time_control/ auxinput11_end_mo
NAMELIST /time_control/ auxinput11_end_d
NAMELIST /time_control/ auxinput11_end_h
NAMELIST /time_control/ auxinput11_end_m
NAMELIST /time_control/ auxinput11_end_s
NAMELIST /time_control/ io_form_auxinput1
NAMELIST /time_control/ io_form_auxinput2
NAMELIST /time_control/ io_form_auxinput3
NAMELIST /time_control/ io_form_auxinput4
NAMELIST /time_control/ io_form_auxinput5
NAMELIST /time_control/ io_form_auxinput6
NAMELIST /time_control/ io_form_auxinput7
NAMELIST /time_control/ io_form_auxinput8
NAMELIST /fdda/ io_form_sgfdda
NAMELIST /fdda/ io_form_gfdda
NAMELIST /time_control/ io_form_auxinput11
NAMELIST /time_control/ io_form_auxhist1
NAMELIST /time_control/ io_form_auxhist2
NAMELIST /time_control/ io_form_auxhist3
NAMELIST /time_control/ io_form_auxhist4
NAMELIST /time_control/ io_form_auxhist5
NAMELIST /time_control/ io_form_auxhist6
NAMELIST /time_control/ io_form_auxhist7
NAMELIST /time_control/ io_form_auxhist8
NAMELIST /time_control/ io_form_auxhist9
NAMELIST /time_control/ io_form_auxhist10
NAMELIST /time_control/ io_form_auxhist11
NAMELIST /time_control/ reset_simulation_start
NAMELIST /domains/ sr_x
NAMELIST /domains/ sr_y
NAMELIST /time_control/ julyr
NAMELIST /time_control/ julday
NAMELIST /time_control/ gmt
NAMELIST /time_control/ input_inname
NAMELIST /time_control/ input_outname
NAMELIST /time_control/ bdy_inname
NAMELIST /time_control/ bdy_outname
NAMELIST /time_control/ rst_inname
NAMELIST /time_control/ rst_outname
NAMELIST /time_control/ write_input
NAMELIST /time_control/ write_restart_at_0h
NAMELIST /time_control/ adjust_output_times
NAMELIST /time_control/ adjust_input_times
NAMELIST /time_control/ tstart
NAMELIST /time_control/ nocolons
NAMELIST /time_control/ cycling
NAMELIST /dfi_control/ dfi_opt
NAMELIST /dfi_control/ dfi_nfilter
NAMELIST /dfi_control/ dfi_write_filtered_input
NAMELIST /dfi_control/ dfi_write_dfi_history
NAMELIST /dfi_control/ dfi_cutoff_seconds
NAMELIST /dfi_control/ dfi_time_dim
NAMELIST /dfi_control/ dfi_fwdstop_year
NAMELIST /dfi_control/ dfi_fwdstop_month
NAMELIST /dfi_control/ dfi_fwdstop_day
NAMELIST /dfi_control/ dfi_fwdstop_hour
NAMELIST /dfi_control/ dfi_fwdstop_minute
NAMELIST /dfi_control/ dfi_fwdstop_second
NAMELIST /dfi_control/ dfi_bckstop_year
NAMELIST /dfi_control/ dfi_bckstop_month
NAMELIST /dfi_control/ dfi_bckstop_day
NAMELIST /dfi_control/ dfi_bckstop_hour
NAMELIST /dfi_control/ dfi_bckstop_minute
NAMELIST /dfi_control/ dfi_bckstop_second
NAMELIST /domains/ time_step
NAMELIST /domains/ time_step_fract_num
NAMELIST /domains/ time_step_fract_den
NAMELIST /domains/ max_dom
NAMELIST /domains/ s_we
NAMELIST /domains/ e_we
NAMELIST /domains/ s_sn
NAMELIST /domains/ e_sn
NAMELIST /domains/ s_vert
NAMELIST /domains/ e_vert
NAMELIST /domains/ dx
NAMELIST /domains/ dy
NAMELIST /domains/ grid_id
NAMELIST /domains/ grid_allowed
NAMELIST /domains/ parent_id
NAMELIST /domains/ i_parent_start
NAMELIST /domains/ j_parent_start
NAMELIST /domains/ parent_grid_ratio
NAMELIST /domains/ parent_time_step_ratio
NAMELIST /domains/ feedback
NAMELIST /domains/ smooth_option
NAMELIST /domains/ ztop
NAMELIST /domains/ moad_grid_ratio
NAMELIST /domains/ moad_time_step_ratio
NAMELIST /domains/ shw
NAMELIST /domains/ tile_sz_x
NAMELIST /domains/ tile_sz_y
NAMELIST /domains/ numtiles
NAMELIST /domains/ nproc_x
NAMELIST /domains/ nproc_y
NAMELIST /domains/ irand
NAMELIST /domains/ ts_buf_size
NAMELIST /domains/ max_ts_locs
NAMELIST /domains/ num_moves
NAMELIST /domains/ move_id
NAMELIST /domains/ move_interval
NAMELIST /domains/ move_cd_x
NAMELIST /domains/ move_cd_y
NAMELIST /domains/ swap_x
NAMELIST /domains/ swap_y
NAMELIST /domains/ cycle_x
NAMELIST /domains/ cycle_y
NAMELIST /domains/ reorder_mesh
NAMELIST /domains/ perturb_input
NAMELIST /domains/ eta_levels
NAMELIST /domains/ ptsgm
NAMELIST /domains/ num_metgrid_levels
NAMELIST /domains/ p_top_requested
NAMELIST /physics/ mp_physics
NAMELIST /physics/ ra_lw_physics
NAMELIST /physics/ ra_sw_physics
NAMELIST /physics/ radt
NAMELIST /physics/ sf_sfclay_physics
NAMELIST /physics/ sf_surface_physics
NAMELIST /physics/ bl_pbl_physics
NAMELIST /physics/ sf_urban_physics
NAMELIST /physics/ bldt
NAMELIST /physics/ cu_physics
NAMELIST /physics/ cudt
NAMELIST /physics/ gsmdt
NAMELIST /physics/ isfflx
NAMELIST /physics/ ifsnow
NAMELIST /physics/ icloud
NAMELIST /physics/ swrad_scat
NAMELIST /physics/ surface_input_source
NAMELIST /physics/ num_soil_layers
NAMELIST /physics/ num_urban_layers
NAMELIST /physics/ maxiens
NAMELIST /physics/ maxens
NAMELIST /physics/ maxens2
NAMELIST /physics/ maxens3
NAMELIST /physics/ ensdim
NAMELIST /physics/ chem_opt
NAMELIST /physics/ num_land_cat
NAMELIST /physics/ num_soil_cat
NAMELIST /physics/ mp_zero_out
NAMELIST /physics/ mp_zero_out_thresh
NAMELIST /physics/ seaice_threshold
NAMELIST /physics/ fractional_seaice
NAMELIST /physics/ sst_update
NAMELIST /physics/ usemonalb
NAMELIST /physics/ rdmaxalb
NAMELIST /physics/ rdlai2d
NAMELIST /physics/ gwd_opt
NAMELIST /physics/ idtad
NAMELIST /physics/ nsoil
NAMELIST /physics/ nphs
NAMELIST /physics/ ncnvc
NAMELIST /physics/ nrads
NAMELIST /physics/ nradl
NAMELIST /physics/ tprec
NAMELIST /physics/ theat
NAMELIST /physics/ tclod
NAMELIST /physics/ trdsw
NAMELIST /physics/ trdlw
NAMELIST /physics/ tsrfc
NAMELIST /physics/ pcpflg
NAMELIST /physics/ sigma
NAMELIST /physics/ sfenth
NAMELIST /physics/ co2tf
NAMELIST /physics/ ra_call_offset
NAMELIST /physics/ cam_abs_freq_s
NAMELIST /physics/ levsiz
NAMELIST /physics/ paerlev
NAMELIST /physics/ cam_abs_dim1
NAMELIST /physics/ cam_abs_dim2
NAMELIST /physics/ cu_rad_feedback
NAMELIST /dynamics/ dyn_opt
NAMELIST /dynamics/ rk_ord
NAMELIST /dynamics/ w_damping
NAMELIST /dynamics/ diff_opt
NAMELIST /dynamics/ km_opt
NAMELIST /dynamics/ damp_opt
NAMELIST /dynamics/ zdamp
NAMELIST /dynamics/ base_pres
NAMELIST /dynamics/ base_temp
NAMELIST /dynamics/ base_lapse
NAMELIST /dynamics/ iso_temp
NAMELIST /dynamics/ dampcoef
NAMELIST /dynamics/ khdif
NAMELIST /dynamics/ kvdif
NAMELIST /dynamics/ c_s
NAMELIST /dynamics/ c_k
NAMELIST /dynamics/ smdiv
NAMELIST /dynamics/ emdiv
NAMELIST /dynamics/ epssm
NAMELIST /dynamics/ non_hydrostatic
NAMELIST /dynamics/ time_step_sound
NAMELIST /dynamics/ h_mom_adv_order
NAMELIST /dynamics/ v_mom_adv_order
NAMELIST /dynamics/ h_sca_adv_order
NAMELIST /dynamics/ v_sca_adv_order
NAMELIST /dynamics/ top_radiation
NAMELIST /dynamics/ tke_upper_bound
NAMELIST /dynamics/ tke_drag_coefficient
NAMELIST /dynamics/ tke_heat_flux
NAMELIST /dynamics/ pert_coriolis
NAMELIST /dynamics/ euler_adv
NAMELIST /dynamics/ idtadt
NAMELIST /dynamics/ idtadc
NAMELIST /dynamics/ boundary_flux
NAMELIST /bdy_control/ spec_bdy_width
NAMELIST /bdy_control/ spec_zone
NAMELIST /bdy_control/ relax_zone
NAMELIST /bdy_control/ specified
NAMELIST /bdy_control/ periodic_x
NAMELIST /bdy_control/ symmetric_xs
NAMELIST /bdy_control/ symmetric_xe
NAMELIST /bdy_control/ open_xs
NAMELIST /bdy_control/ open_xe
NAMELIST /bdy_control/ periodic_y
NAMELIST /bdy_control/ symmetric_ys
NAMELIST /bdy_control/ symmetric_ye
NAMELIST /bdy_control/ open_ys
NAMELIST /bdy_control/ open_ye
NAMELIST /bdy_control/ polar
NAMELIST /bdy_control/ nested
NAMELIST /bdy_control/ real_data_init_type
NAMELIST /grib2/ background_proc_id
NAMELIST /grib2/ forecast_proc_id
NAMELIST /grib2/ production_status
NAMELIST /grib2/ compression


      OPEN ( UNIT   = nml_read_unit    ,      &
             FILE   = "namelist.input" ,      &
             FORM   = "FORMATTED"      ,      &
             STATUS = "OLD"            ,      &
             IOSTAT = io_status         )

      IF ( io_status .NE. 0 ) THEN
        CALL wrf_error_fatal3("",3300,&
'ERROR OPENING namelist.input' )
      ENDIF

      OPEN ( UNIT   = nml_write_unit    ,      &
             FILE   = "namelist.output" ,      &
             FORM   = "FORMATTED"      ,      &
             STATUS = "REPLACE"        ,      &
             IOSTAT = io_status         )

      IF ( io_status .NE. 0 ) THEN
        CALL wrf_error_fatal3("",3311,&
'ERROR OPENING namelist.output' )
      ENDIF








run_days = 0
run_hours = 0
run_minutes = 0
run_seconds = 0
start_year = 1993
start_month = 03
start_day = 13
start_hour = 12
start_minute = 00
start_second = 00
end_year = 1993
end_month = 03
end_day = 14
end_hour = 12
end_minute = 00
end_second = 00
interval_seconds = 43200
input_from_file = .false.
fine_input_stream = 0
history_interval = 0
frames_per_outfile = 10
frames_per_auxhist1 = 10
frames_per_auxhist2 = 10
frames_per_auxhist3 = 10
frames_per_auxhist4 = 10
frames_per_auxhist5 = 10
frames_per_auxhist6 = 10
frames_per_auxhist7 = 10
frames_per_auxhist8 = 10
frames_per_auxhist9 = 10
frames_per_auxhist10 = 10
frames_per_auxhist11 = 10
restart = .false.
restart_interval = 0
io_form_input = 2
io_form_history = 2
io_form_restart = 2
io_form_boundary = 2
debug_level = 0
self_test_domain = .false.
history_outname = "wrfout_d<domain>_<date>"
auxhist1_outname = "auxhist1_d<domain>_<date>"
auxhist2_outname = "auxhist2_d<domain>_<date>"
auxhist3_outname = "auxhist3_d<domain>_<date>"
auxhist4_outname = "auxhist4_d<domain>_<date>"
auxhist5_outname = "auxhist5_d<domain>_<date>"
auxhist6_outname = "auxhist6_d<domain>_<date>"
auxhist7_outname = "auxhist7_d<domain>_<date>"
auxhist8_outname = "auxhist8_d<domain>_<date>"
auxhist9_outname = "auxhist9_d<domain>_<date>"
auxhist10_outname = "auxhist10_d<domain>_<date>"
auxhist11_outname = "auxhist11_d<domain>_<date>"
history_inname = "wrfhist_d<domain>_<date>"
auxhist1_inname = "auxhist1_d<domain>_<date>"
auxhist2_inname = "auxhist2_d<domain>_<date>"
auxhist3_inname = "auxhist3_d<domain>_<date>"
auxhist4_inname = "auxhist4_d<domain>_<date>"
auxhist5_inname = "auxhist5_d<domain>_<date>"
auxhist6_inname = "auxhist6_d<domain>_<date>"
auxhist7_inname = "auxhist7_d<domain>_<date>"
auxhist8_inname = "auxhist8_d<domain>_<date>"
auxhist9_inname = "auxhist9_d<domain>_<date>"
auxhist10_inname = "auxhist10_d<domain>_<date>"
auxhist11_inname = "auxhist11_d<domain>_<date>"
auxinput1_outname = "auxinput1_d<domain>_<date>"
auxinput2_outname = "auxinput2_d<domain>_<date>"
auxinput3_outname = "auxinput3_d<domain>_<date>"
auxinput4_outname = "auxinput4_d<domain>_<date>"
auxinput5_outname = "auxinput5_d<domain>_<date>"
auxinput6_outname = "auxinput6_d<domain>_<date>"
auxinput7_outname = "auxinput7_d<domain>_<date>"
auxinput8_outname = "auxinput8_d<domain>_<date>"
auxinput9_outname = "auxinput9_d<domain>_<date>"
auxinput10_outname = "auxinput10_d<domain>_<date>"
auxinput11_outname = "auxinput11_d<domain>_<date>"
auxinput1_inname = "met_nmm.d<domain>.<date>"
auxinput2_inname = "auxinput2_d<domain>"
auxinput3_inname = "auxinput3_d<domain>"
auxinput4_inname = "auxinput4_d<domain>"
auxinput5_inname = "auxinput5_d<domain>"
auxinput6_inname = "auxinput6_d<domain>"
auxinput7_inname = "auxinput7_d<domain>"
auxinput8_inname = "auxinput8_d<domain>"
sgfdda_inname = "sgfdda_d<domain>"
gfdda_inname = "gfdda_d<domain>"
auxinput11_inname = "auxinput11_d<domain>"
history_interval_mo = 0
history_interval_d = 0
history_interval_h = 0
history_interval_m = 0
history_interval_s = 0
inputout_interval_mo = 0
inputout_interval_d = 0
inputout_interval_h = 0
inputout_interval_m = 0
inputout_interval_s = 0
inputout_interval = 0
auxhist1_interval_mo = 0
auxhist1_interval_d = 0
auxhist1_interval_h = 0
auxhist1_interval_m = 0
auxhist1_interval_s = 0
auxhist1_interval = 0
auxhist2_interval_mo = 0
auxhist2_interval_d = 0
auxhist2_interval_h = 0
auxhist2_interval_m = 0
auxhist2_interval_s = 0
auxhist2_interval = 0
auxhist3_interval_mo = 0
auxhist3_interval_d = 0
auxhist3_interval_h = 0
auxhist3_interval_m = 0
auxhist3_interval_s = 0
auxhist3_interval = 0
auxhist4_interval_mo = 0
auxhist4_interval_d = 0
auxhist4_interval_h = 0
auxhist4_interval_m = 0
auxhist4_interval_s = 0
auxhist4_interval = 0
auxhist5_interval_mo = 0
auxhist5_interval_d = 0
auxhist5_interval_h = 0
auxhist5_interval_m = 0
auxhist5_interval_s = 0
auxhist5_interval = 0
auxhist6_interval_mo = 0
auxhist6_interval_d = 0
auxhist6_interval_h = 0
auxhist6_interval_m = 0
auxhist6_interval_s = 0
auxhist6_interval = 0
auxhist7_interval_mo = 0
auxhist7_interval_d = 0
auxhist7_interval_h = 0
auxhist7_interval_m = 0
auxhist7_interval_s = 0
auxhist7_interval = 0
auxhist8_interval_mo = 0
auxhist8_interval_d = 0
auxhist8_interval_h = 0
auxhist8_interval_m = 0
auxhist8_interval_s = 0
auxhist8_interval = 0
auxhist9_interval_mo = 0
auxhist9_interval_d = 0
auxhist9_interval_h = 0
auxhist9_interval_m = 0
auxhist9_interval_s = 0
auxhist9_interval = 0
auxhist10_interval_mo = 0
auxhist10_interval_d = 0
auxhist10_interval_h = 0
auxhist10_interval_m = 0
auxhist10_interval_s = 0
auxhist10_interval = 0
auxhist11_interval_mo = 0
auxhist11_interval_d = 0
auxhist11_interval_h = 0
auxhist11_interval_m = 0
auxhist11_interval_s = 0
auxhist11_interval = 0
auxinput1_interval_mo = 0
auxinput1_interval_d = 0
auxinput1_interval_h = 0
auxinput1_interval_m = 0
auxinput1_interval_s = 0
auxinput1_interval = 0
auxinput2_interval_mo = 0
auxinput2_interval_d = 0
auxinput2_interval_h = 0
auxinput2_interval_m = 0
auxinput2_interval_s = 0
auxinput2_interval = 0
auxinput3_interval_mo = 0
auxinput3_interval_d = 0
auxinput3_interval_h = 0
auxinput3_interval_m = 0
auxinput3_interval_s = 0
auxinput3_interval = 0
auxinput4_interval_mo = 0
auxinput4_interval_d = 0
auxinput4_interval_h = 0
auxinput4_interval_m = 0
auxinput4_interval_s = 0
auxinput4_interval = 0
auxinput5_interval_mo = 0
auxinput5_interval_d = 0
auxinput5_interval_h = 0
auxinput5_interval_m = 0
auxinput5_interval_s = 0
auxinput5_interval = 0
auxinput6_interval_mo = 0
auxinput6_interval_d = 0
auxinput6_interval_h = 0
auxinput6_interval_m = 0
auxinput6_interval_s = 0
auxinput6_interval = 0
auxinput7_interval_mo = 0
auxinput7_interval_d = 0
auxinput7_interval_h = 0
auxinput7_interval_m = 0
auxinput7_interval_s = 0
auxinput7_interval = 0
auxinput8_interval_mo = 0
auxinput8_interval_d = 0
auxinput8_interval_h = 0
auxinput8_interval_m = 0
auxinput8_interval_s = 0
auxinput8_interval = 0
sgfdda_interval_mo = 0
sgfdda_interval_d = 0
sgfdda_interval_h = 0
sgfdda_interval_m = 0
sgfdda_interval_s = 0
sgfdda_interval = 0
gfdda_interval_mo = 0
gfdda_interval_d = 0
gfdda_interval_h = 0
gfdda_interval_m = 0
gfdda_interval_s = 0
gfdda_interval = 0
auxinput11_interval_mo = 0
auxinput11_interval_d = 0
auxinput11_interval_h = 0
auxinput11_interval_m = 0
auxinput11_interval_s = 0
auxinput11_interval = 0
restart_interval_mo = 0
restart_interval_d = 0
restart_interval_h = 0
restart_interval_m = 0
restart_interval_s = 0
history_begin_y = 0
history_begin_mo = 0
history_begin_d = 0
history_begin_h = 0
history_begin_m = 0
history_begin_s = 0
inputout_begin_y = 0
inputout_begin_mo = 0
inputout_begin_d = 0
inputout_begin_h = 0
inputout_begin_m = 0
inputout_begin_s = 0
auxhist1_begin_y = 0
auxhist1_begin_mo = 0
auxhist1_begin_d = 0
auxhist1_begin_h = 0
auxhist1_begin_m = 0
auxhist1_begin_s = 0
auxhist2_begin_y = 0
auxhist2_begin_mo = 0
auxhist2_begin_d = 0
auxhist2_begin_h = 0
auxhist2_begin_m = 0
auxhist2_begin_s = 0
auxhist3_begin_y = 0
auxhist3_begin_mo = 0
auxhist3_begin_d = 0
auxhist3_begin_h = 0
auxhist3_begin_m = 0
auxhist3_begin_s = 0
auxhist4_begin_y = 0
auxhist4_begin_mo = 0
auxhist4_begin_d = 0
auxhist4_begin_h = 0
auxhist4_begin_m = 0
auxhist4_begin_s = 0
auxhist5_begin_y = 0
auxhist5_begin_mo = 0
auxhist5_begin_d = 0
auxhist5_begin_h = 0
auxhist5_begin_m = 0
auxhist5_begin_s = 0
auxhist6_begin_y = 0
auxhist6_begin_mo = 0
auxhist6_begin_d = 0
auxhist6_begin_h = 0
auxhist6_begin_m = 0
auxhist6_begin_s = 0
auxhist7_begin_y = 0
auxhist7_begin_mo = 0
auxhist7_begin_d = 0
auxhist7_begin_h = 0
auxhist7_begin_m = 0
auxhist7_begin_s = 0
auxhist8_begin_y = 0
auxhist8_begin_mo = 0
auxhist8_begin_d = 0
auxhist8_begin_h = 0
auxhist8_begin_m = 0
auxhist8_begin_s = 0
auxhist9_begin_y = 0
auxhist9_begin_mo = 0
auxhist9_begin_d = 0
auxhist9_begin_h = 0
auxhist9_begin_m = 0
auxhist9_begin_s = 0
auxhist10_begin_y = 0
auxhist10_begin_mo = 0
auxhist10_begin_d = 0
auxhist10_begin_h = 0
auxhist10_begin_m = 0
auxhist10_begin_s = 0
auxhist11_begin_y = 0
auxhist11_begin_mo = 0
auxhist11_begin_d = 0
auxhist11_begin_h = 0
auxhist11_begin_m = 0
auxhist11_begin_s = 0
auxinput1_begin_y = 0
auxinput1_begin_mo = 0
auxinput1_begin_d = 0
auxinput1_begin_h = 0
auxinput1_begin_m = 0
auxinput1_begin_s = 0
auxinput2_begin_y = 0
auxinput2_begin_mo = 0
auxinput2_begin_d = 0
auxinput2_begin_h = 0
auxinput2_begin_m = 0
auxinput2_begin_s = 0
auxinput3_begin_y = 0
auxinput3_begin_mo = 0
auxinput3_begin_d = 0
auxinput3_begin_h = 0
auxinput3_begin_m = 0
auxinput3_begin_s = 0
auxinput4_begin_y = 0
auxinput4_begin_mo = 0
auxinput4_begin_d = 0
auxinput4_begin_h = 0
auxinput4_begin_m = 0
auxinput4_begin_s = 0
auxinput5_begin_y = 0
auxinput5_begin_mo = 0
auxinput5_begin_d = 0
auxinput5_begin_h = 0
auxinput5_begin_m = 0
auxinput5_begin_s = 0
auxinput6_begin_y = 0
auxinput6_begin_mo = 0
auxinput6_begin_d = 0
auxinput6_begin_h = 0
auxinput6_begin_m = 0
auxinput6_begin_s = 0
auxinput7_begin_y = 0
auxinput7_begin_mo = 0
auxinput7_begin_d = 0
auxinput7_begin_h = 0
auxinput7_begin_m = 0
auxinput7_begin_s = 0
auxinput8_begin_y = 0
auxinput8_begin_mo = 0
auxinput8_begin_d = 0
auxinput8_begin_h = 0
auxinput8_begin_m = 0
auxinput8_begin_s = 0
sgfdda_begin_y = 0
sgfdda_begin_mo = 0
sgfdda_begin_d = 0
sgfdda_begin_h = 0
sgfdda_begin_m = 0
sgfdda_begin_s = 0
gfdda_begin_y = 0
gfdda_begin_mo = 0
gfdda_begin_d = 0
gfdda_begin_h = 0
gfdda_begin_m = 0
gfdda_begin_s = 0
auxinput11_begin_y = 0
auxinput11_begin_mo = 0
auxinput11_begin_d = 0
auxinput11_begin_h = 0
auxinput11_begin_m = 0
auxinput11_begin_s = 0
restart_begin_y = 0
restart_begin_mo = 0
restart_begin_d = 0
restart_begin_h = 0
restart_begin_m = 0
restart_begin_s = 0
history_end_y = 0
history_end_mo = 0
history_end_d = 0
history_end_h = 0
history_end_m = 0
history_end_s = 0
inputout_end_y = 0
inputout_end_mo = 0
inputout_end_d = 0
inputout_end_h = 0
inputout_end_m = 0
inputout_end_s = 0
auxhist1_end_y = 0
auxhist1_end_mo = 0
auxhist1_end_d = 0
auxhist1_end_h = 0
auxhist1_end_m = 0
auxhist1_end_s = 0
auxhist2_end_y = 0
auxhist2_end_mo = 0
auxhist2_end_d = 0
auxhist2_end_h = 0
auxhist2_end_m = 0
auxhist2_end_s = 0
auxhist3_end_y = 0
auxhist3_end_mo = 0
auxhist3_end_d = 0
auxhist3_end_h = 0
auxhist3_end_m = 0
auxhist3_end_s = 0
auxhist4_end_y = 0
auxhist4_end_mo = 0
auxhist4_end_d = 0
auxhist4_end_h = 0
auxhist4_end_m = 0
auxhist4_end_s = 0
auxhist5_end_y = 0
auxhist5_end_mo = 0
auxhist5_end_d = 0
auxhist5_end_h = 0
auxhist5_end_m = 0
auxhist5_end_s = 0
auxhist6_end_y = 0
auxhist6_end_mo = 0
auxhist6_end_d = 0
auxhist6_end_h = 0
auxhist6_end_m = 0
auxhist6_end_s = 0
auxhist7_end_y = 0
auxhist7_end_mo = 0
auxhist7_end_d = 0
auxhist7_end_h = 0
auxhist7_end_m = 0
auxhist7_end_s = 0
auxhist8_end_y = 0
auxhist8_end_mo = 0
auxhist8_end_d = 0
auxhist8_end_h = 0
auxhist8_end_m = 0
auxhist8_end_s = 0
auxhist9_end_y = 0
auxhist9_end_mo = 0
auxhist9_end_d = 0
auxhist9_end_h = 0
auxhist9_end_m = 0
auxhist9_end_s = 0
auxhist10_end_y = 0
auxhist10_end_mo = 0
auxhist10_end_d = 0
auxhist10_end_h = 0
auxhist10_end_m = 0
auxhist10_end_s = 0
auxhist11_end_y = 0
auxhist11_end_mo = 0
auxhist11_end_d = 0
auxhist11_end_h = 0
auxhist11_end_m = 0
auxhist11_end_s = 0
auxinput1_end_y = 0
auxinput1_end_mo = 0
auxinput1_end_d = 0
auxinput1_end_h = 0
auxinput1_end_m = 0
auxinput1_end_s = 0
auxinput2_end_y = 0
auxinput2_end_mo = 0
auxinput2_end_d = 0
auxinput2_end_h = 0
auxinput2_end_m = 0
auxinput2_end_s = 0
auxinput3_end_y = 0
auxinput3_end_mo = 0
auxinput3_end_d = 0
auxinput3_end_h = 0
auxinput3_end_m = 0
auxinput3_end_s = 0
auxinput4_end_y = 0
auxinput4_end_mo = 0
auxinput4_end_d = 0
auxinput4_end_h = 0
auxinput4_end_m = 0
auxinput4_end_s = 0
auxinput5_end_y = 0
auxinput5_end_mo = 0
auxinput5_end_d = 0
auxinput5_end_h = 0
auxinput5_end_m = 0
auxinput5_end_s = 0
auxinput6_end_y = 0
auxinput6_end_mo = 0
auxinput6_end_d = 0
auxinput6_end_h = 0
auxinput6_end_m = 0
auxinput6_end_s = 0
auxinput7_end_y = 0
auxinput7_end_mo = 0
auxinput7_end_d = 0
auxinput7_end_h = 0
auxinput7_end_m = 0
auxinput7_end_s = 0
auxinput8_end_y = 0
auxinput8_end_mo = 0
auxinput8_end_d = 0
auxinput8_end_h = 0
auxinput8_end_m = 0
auxinput8_end_s = 0
sgfdda_end_y = 0
sgfdda_end_mo = 0
sgfdda_end_d = 0
sgfdda_end_h = 0
sgfdda_end_m = 0
sgfdda_end_s = 0
gfdda_end_y = 0
gfdda_end_mo = 0
gfdda_end_d = 0
gfdda_end_h = 0
gfdda_end_m = 0
gfdda_end_s = 0
auxinput11_end_y = 0
auxinput11_end_mo = 0
auxinput11_end_d = 0
auxinput11_end_h = 0
auxinput11_end_m = 0
auxinput11_end_s = 0
io_form_auxinput1 = 2
io_form_auxinput2 = 2
io_form_auxinput3 = 2
io_form_auxinput4 = 2
io_form_auxinput5 = 2
io_form_auxinput6 = 2
io_form_auxinput7 = 2
io_form_auxinput8 = 2
io_form_sgfdda = 2
io_form_gfdda = 2
io_form_auxinput11 = 2
io_form_auxhist1 = 2
io_form_auxhist2 = 2
io_form_auxhist3 = 2
io_form_auxhist4 = 2
io_form_auxhist5 = 2
io_form_auxhist6 = 2
io_form_auxhist7 = 2
io_form_auxhist8 = 2
io_form_auxhist9 = 2
io_form_auxhist10 = 2
io_form_auxhist11 = 2
simulation_start_year = 0
simulation_start_month = 0
simulation_start_day = 0
simulation_start_hour = 0
simulation_start_minute = 0
simulation_start_second = 0
reset_simulation_start = .false.
sr_x = 0
sr_y = 0
julyr = 0
julday = 1
gmt = 0.
input_inname = "wrfinput_d<domain>"
input_outname = "wrfinput_d<domain>"
bdy_inname = "wrfbdy_d<domain>"
bdy_outname = "wrfbdy_d<domain>"
rst_inname = "wrfrst_d<domain>_<date>"
rst_outname = "wrfrst_d<domain>_<date>"
write_input = .false.
write_restart_at_0h = .false.
adjust_output_times = .false.
adjust_input_times = .false.
tstart = 0.
nocolons = .false.
cycling = .false.
dfi_opt = 0
dfi_nfilter = 7
dfi_write_filtered_input = .true.
dfi_write_dfi_history = .false.
dfi_cutoff_seconds = 3600
dfi_time_dim = 1000
dfi_fwdstop_year = 2004
dfi_fwdstop_month = 03
dfi_fwdstop_day = 13
dfi_fwdstop_hour = 12
dfi_fwdstop_minute = 00
dfi_fwdstop_second = 00
dfi_bckstop_year = 2004
dfi_bckstop_month = 03
dfi_bckstop_day = 14
dfi_bckstop_hour = 12
dfi_bckstop_minute = 00
dfi_bckstop_second = 00
time_step_fract_num = 0
time_step_fract_den = 1
max_dom = 1
s_we = 1
e_we = 32
s_sn = 1
e_sn = 32
s_vert = 1
e_vert = 31
dx = 200
dy = 200
grid_id = 1
grid_allowed = .true.
parent_id = 0
i_parent_start = 1
j_parent_start = 1
parent_grid_ratio = 1
parent_time_step_ratio = 1
feedback = 1
smooth_option = 2
ztop = 15000.
moad_grid_ratio = 1
moad_time_step_ratio = 1
shw = 2
tile_sz_x = 0
tile_sz_y = 0
numtiles = 1
nproc_x = -1
nproc_y = -1
irand = 0
dt = 2.
ts_buf_size = 200
max_ts_locs = 5
num_moves = 0
move_id = 0
move_interval = 999999999
move_cd_x = 0
move_cd_y = 0
swap_x = .false.
swap_y = .false.
cycle_x = .false.
cycle_y = .false.
reorder_mesh = .false.
perturb_input = .false.
eta_levels = -1.
ptsgm = 42000.
num_metgrid_levels = 43
p_top_requested = 5000
mp_physics = 0
ra_lw_physics = 0
ra_sw_physics = 0
radt = 0
sf_sfclay_physics = 0
sf_surface_physics = 0
bl_pbl_physics = 0
sf_urban_physics = 0
bldt = 0
cu_physics = 0
cudt = 0
gsmdt = 0
isfflx = 1
ifsnow = 0
icloud = 1
swrad_scat = 1
surface_input_source = 1
num_soil_layers = 5
num_urban_layers = 400
maxiens = 1
maxens = 3
maxens2 = 3
maxens3 = 16
ensdim = 144
chem_opt = 0
num_land_cat = 24
num_soil_cat = 16
mp_zero_out = 0
mp_zero_out_thresh = 1.e-8
seaice_threshold = 271
fractional_seaice = 0
sst_update = 0
usemonalb = .true.
rdmaxalb = .true.
rdlai2d = .false.
gwd_opt = 0
idtad = 2
nsoil = 4
nphs = 10
ncnvc = 10
nrads = 200
nradl = 200
tprec = 3.
theat = 6.
tclod = 6.
trdsw = 6.
trdlw = 6.
tsrfc = 6.
pcpflg = .false.
sigma = 1
sfenth = 1.0
co2tf = 0
ra_call_offset = -1
cam_abs_freq_s = 21600.
levsiz = 1
paerlev = 1
cam_abs_dim1 = 1
cam_abs_dim2 = 1
cu_rad_feedback = .false.
rk_ord = 3
w_damping = 0
diff_opt = 1
km_opt = 1
damp_opt = 1
zdamp = 5000.
base_pres = 100000.
base_temp = 290.
base_lapse = 50.
iso_temp = 0.
dampcoef = 0.2
khdif = 0
kvdif = 0
c_s = 0.25
c_k = 0.15
smdiv = 0.
emdiv = 0.
epssm = .1
non_hydrostatic = .true.
time_step_sound = 10
h_mom_adv_order = 3
v_mom_adv_order = 3
h_sca_adv_order = 3
v_sca_adv_order = 3
top_radiation = .false.
tke_upper_bound = 1000.
tke_drag_coefficient = 0.
tke_heat_flux = 0.
pert_coriolis = .false.
euler_adv = .false.
idtadt = 1
idtadc = 1
boundary_flux = .false.
spec_bdy_width = 5
spec_zone = 1
relax_zone = 4
specified = .false.
periodic_x = .false.
symmetric_xs = .false.
symmetric_xe = .false.
open_xs = .false.
open_xe = .false.
periodic_y = .false.
symmetric_ys = .false.
symmetric_ye = .false.
open_ys = .false.
open_ye = .false.
polar = .false.
nested = .false.
real_data_init_type = 1
background_proc_id = 255
forecast_proc_id = 255
production_status = 255
compression = 40
cen_lat = 0
cen_lon = 0
truelat1 = 0
truelat2 = 0
moad_cen_lat = 0
stand_lon = 0
flag_metgrid = 0
flag_snow = 0
flag_psfc = 0
flag_sm000010 = 0
flag_sm010040 = 0
flag_sm040100 = 0
flag_sm100200 = 0
flag_st000010 = 0
flag_st010040 = 0
flag_st040100 = 0
flag_st100200 = 0
flag_slp = 0
flag_soilhgt = 0
flag_mf_xy = 0
bdyfrq = 0
mminlu = " "
iswater = 0
islake = 0
isice = 0
isurban = 0
isoilwater = 0
map_proj = 0
dfi_stage = 3
mp_physics_dfi = -1











 nml_read_error = .FALSE.
 NML_LOOP : DO i=1,8
    REWIND ( UNIT = nml_read_unit )
    SELECT CASE ( i )
       CASE ( 1 ) 
          nml_name = "time_control"
          READ   ( UNIT = nml_read_unit , NML = time_control , ERR=9201, END=9202 )
          WRITE ( UNIT = nml_write_unit, NML = time_control )
          CYCLE NML_LOOP
       CASE ( 2 ) 
          nml_name = "fdda"
          READ   ( UNIT = nml_read_unit , NML = fdda , ERR=9201, END=9202 )
          WRITE ( UNIT = nml_write_unit, NML = fdda )
          CYCLE NML_LOOP
       CASE ( 3 ) 
          nml_name = "domains"
          READ   ( UNIT = nml_read_unit , NML = domains , ERR=9201, END=9202 )
          WRITE ( UNIT = nml_write_unit, NML = domains )
          CYCLE NML_LOOP
       CASE ( 4 ) 
          nml_name = "dfi_control"
          READ   ( UNIT = nml_read_unit , NML = dfi_control , ERR=9201, END=9202 )
          WRITE ( UNIT = nml_write_unit, NML = dfi_control )
          CYCLE NML_LOOP
       CASE ( 5 ) 
          nml_name = "physics"
          READ   ( UNIT = nml_read_unit , NML = physics , ERR=9201, END=9202 )
          WRITE ( UNIT = nml_write_unit, NML = physics )
          CYCLE NML_LOOP
       CASE ( 6 ) 
          nml_name = "dynamics"
          READ   ( UNIT = nml_read_unit , NML = dynamics , ERR=9201, END=9202 )
          WRITE ( UNIT = nml_write_unit, NML = dynamics )
          CYCLE NML_LOOP
       CASE ( 7 ) 
          nml_name = "bdy_control"
          READ   ( UNIT = nml_read_unit , NML = bdy_control , ERR=9201, END=9202 )
          WRITE ( UNIT = nml_write_unit, NML = bdy_control )
          CYCLE NML_LOOP
       CASE ( 8 ) 
          nml_name = "grib2"
          READ   ( UNIT = nml_read_unit , NML = grib2 , ERR=9201, END=9202 )
          WRITE ( UNIT = nml_write_unit, NML = grib2 )
          CYCLE NML_LOOP
    END SELECT
9201 CALL wrf_message("Error while reading namelist "//TRIM(nml_name))
    nml_read_error = .TRUE.
    IF ( TRIM(nml_name) .EQ. "dynamics") THEN
        CALL wrf_alt_nml_dynamics(nml_read_unit, TRIM(nml_name))
    ENDIF
    IF ( TRIM(nml_name) .EQ. "physics") THEN
        CALL wrf_alt_nml_physics(nml_read_unit, TRIM(nml_name))
    ENDIF
    IF ( TRIM(nml_name) .EQ. "fdda") THEN
        CALL wrf_alt_nml_fdda(nml_read_unit, TRIM(nml_name))
    ENDIF
    IF ( TRIM(nml_name) .EQ. "wrfvar1") THEN
        CALL wrfvar_alt_nml_wrfvar1(nml_read_unit, TRIM(nml_name))
    ENDIF
    IF ( TRIM(nml_name) .EQ. "wrfvar2") THEN
        CALL wrfvar_alt_nml_wrfvar2(nml_read_unit, TRIM(nml_name))
    ENDIF
    IF ( TRIM(nml_name) .EQ. "wrfvar4") THEN
        CALL wrfvar_alt_nml_wrfvar4(nml_read_unit, TRIM(nml_name))
    ENDIF
    IF ( TRIM(nml_name) .EQ. "wrfvar14") THEN
        CALL wrfvar_alt_nml_wrfvar14(nml_read_unit, TRIM(nml_name))
    ENDIF
    CYCLE NML_LOOP
9202 CALL wrf_message("Namelist "//TRIM(nml_name)//" not found in namelist.input."// & 
                      " Using registry defaults for variables in "//TRIM(nml_name))
 END DO NML_LOOP
 
 IF ( nml_read_error ) CALL wrf_error_fatal3("",4190,&
"Errors while reading one or more namelists from namelist.input.")










      DO i = 1, max_dom
         mp_physics(i) = mp_physics(max_dom)
      ENDDO










 model_config_rec % run_days                   =  run_days 
 model_config_rec % run_hours                  =  run_hours 
 model_config_rec % run_minutes                =  run_minutes 
 model_config_rec % run_seconds                =  run_seconds 
 model_config_rec % start_year                 =  start_year 
 model_config_rec % start_month                =  start_month 
 model_config_rec % start_day                  =  start_day 
 model_config_rec % start_hour                 =  start_hour 
 model_config_rec % start_minute               =  start_minute 
 model_config_rec % start_second               =  start_second 
 model_config_rec % end_year                   =  end_year 
 model_config_rec % end_month                  =  end_month 
 model_config_rec % end_day                    =  end_day 
 model_config_rec % end_hour                   =  end_hour 
 model_config_rec % end_minute                 =  end_minute 
 model_config_rec % end_second                 =  end_second 
 model_config_rec % interval_seconds           =  interval_seconds 
 model_config_rec % input_from_file            =  input_from_file 
 model_config_rec % fine_input_stream          =  fine_input_stream 
 model_config_rec % history_interval           =  history_interval 
 model_config_rec % frames_per_outfile         =  frames_per_outfile 
 model_config_rec % frames_per_auxhist1        =  frames_per_auxhist1 
 model_config_rec % frames_per_auxhist2        =  frames_per_auxhist2 
 model_config_rec % frames_per_auxhist3        =  frames_per_auxhist3 
 model_config_rec % frames_per_auxhist4        =  frames_per_auxhist4 
 model_config_rec % frames_per_auxhist5        =  frames_per_auxhist5 
 model_config_rec % frames_per_auxhist6        =  frames_per_auxhist6 
 model_config_rec % frames_per_auxhist7        =  frames_per_auxhist7 
 model_config_rec % frames_per_auxhist8        =  frames_per_auxhist8 
 model_config_rec % frames_per_auxhist9        =  frames_per_auxhist9 
 model_config_rec % frames_per_auxhist10       =  frames_per_auxhist10 
 model_config_rec % frames_per_auxhist11       =  frames_per_auxhist11 
 model_config_rec % restart                    =  restart 
 model_config_rec % restart_interval           =  restart_interval 
 model_config_rec % io_form_input              =  io_form_input 
 model_config_rec % io_form_history            =  io_form_history 
 model_config_rec % io_form_restart            =  io_form_restart 
 model_config_rec % io_form_boundary           =  io_form_boundary 
 model_config_rec % debug_level                =  debug_level 
 model_config_rec % self_test_domain           =  self_test_domain 
 model_config_rec % history_outname            =  history_outname 
 model_config_rec % auxhist1_outname           =  auxhist1_outname 
 model_config_rec % auxhist2_outname           =  auxhist2_outname 
 model_config_rec % auxhist3_outname           =  auxhist3_outname 
 model_config_rec % auxhist4_outname           =  auxhist4_outname 
 model_config_rec % auxhist5_outname           =  auxhist5_outname 
 model_config_rec % auxhist6_outname           =  auxhist6_outname 
 model_config_rec % auxhist7_outname           =  auxhist7_outname 
 model_config_rec % auxhist8_outname           =  auxhist8_outname 
 model_config_rec % auxhist9_outname           =  auxhist9_outname 
 model_config_rec % auxhist10_outname          =  auxhist10_outname 
 model_config_rec % auxhist11_outname          =  auxhist11_outname 
 model_config_rec % history_inname             =  history_inname 
 model_config_rec % auxhist1_inname            =  auxhist1_inname 
 model_config_rec % auxhist2_inname            =  auxhist2_inname 
 model_config_rec % auxhist3_inname            =  auxhist3_inname 
 model_config_rec % auxhist4_inname            =  auxhist4_inname 
 model_config_rec % auxhist5_inname            =  auxhist5_inname 
 model_config_rec % auxhist6_inname            =  auxhist6_inname 
 model_config_rec % auxhist7_inname            =  auxhist7_inname 
 model_config_rec % auxhist8_inname            =  auxhist8_inname 
 model_config_rec % auxhist9_inname            =  auxhist9_inname 
 model_config_rec % auxhist10_inname           =  auxhist10_inname 
 model_config_rec % auxhist11_inname           =  auxhist11_inname 
 model_config_rec % auxinput1_outname          =  auxinput1_outname 
 model_config_rec % auxinput2_outname          =  auxinput2_outname 
 model_config_rec % auxinput3_outname          =  auxinput3_outname 
 model_config_rec % auxinput4_outname          =  auxinput4_outname 
 model_config_rec % auxinput5_outname          =  auxinput5_outname 
 model_config_rec % auxinput6_outname          =  auxinput6_outname 
 model_config_rec % auxinput7_outname          =  auxinput7_outname 
 model_config_rec % auxinput8_outname          =  auxinput8_outname 
 model_config_rec % auxinput9_outname          =  auxinput9_outname 
 model_config_rec % auxinput10_outname         =  auxinput10_outname 
 model_config_rec % auxinput11_outname         =  auxinput11_outname 
 model_config_rec % auxinput1_inname           =  auxinput1_inname 
 model_config_rec % auxinput2_inname           =  auxinput2_inname 
 model_config_rec % auxinput3_inname           =  auxinput3_inname 
 model_config_rec % auxinput4_inname           =  auxinput4_inname 
 model_config_rec % auxinput5_inname           =  auxinput5_inname 
 model_config_rec % auxinput6_inname           =  auxinput6_inname 
 model_config_rec % auxinput7_inname           =  auxinput7_inname 
 model_config_rec % auxinput8_inname           =  auxinput8_inname 
 model_config_rec % sgfdda_inname              =  sgfdda_inname 
 model_config_rec % gfdda_inname               =  gfdda_inname 
 model_config_rec % auxinput11_inname          =  auxinput11_inname 
 model_config_rec % history_interval_mo        =  history_interval_mo 
 model_config_rec % history_interval_d         =  history_interval_d 
 model_config_rec % history_interval_h         =  history_interval_h 
 model_config_rec % history_interval_m         =  history_interval_m 
 model_config_rec % history_interval_s         =  history_interval_s 
 model_config_rec % inputout_interval_mo       =  inputout_interval_mo 
 model_config_rec % inputout_interval_d        =  inputout_interval_d 
 model_config_rec % inputout_interval_h        =  inputout_interval_h 
 model_config_rec % inputout_interval_m        =  inputout_interval_m 
 model_config_rec % inputout_interval_s        =  inputout_interval_s 
 model_config_rec % inputout_interval          =  inputout_interval 
 model_config_rec % auxhist1_interval_mo       =  auxhist1_interval_mo 
 model_config_rec % auxhist1_interval_d        =  auxhist1_interval_d 
 model_config_rec % auxhist1_interval_h        =  auxhist1_interval_h 
 model_config_rec % auxhist1_interval_m        =  auxhist1_interval_m 
 model_config_rec % auxhist1_interval_s        =  auxhist1_interval_s 
 model_config_rec % auxhist1_interval          =  auxhist1_interval 
 model_config_rec % auxhist2_interval_mo       =  auxhist2_interval_mo 
 model_config_rec % auxhist2_interval_d        =  auxhist2_interval_d 
 model_config_rec % auxhist2_interval_h        =  auxhist2_interval_h 
 model_config_rec % auxhist2_interval_m        =  auxhist2_interval_m 
 model_config_rec % auxhist2_interval_s        =  auxhist2_interval_s 
 model_config_rec % auxhist2_interval          =  auxhist2_interval 
 model_config_rec % auxhist3_interval_mo       =  auxhist3_interval_mo 
 model_config_rec % auxhist3_interval_d        =  auxhist3_interval_d 
 model_config_rec % auxhist3_interval_h        =  auxhist3_interval_h 
 model_config_rec % auxhist3_interval_m        =  auxhist3_interval_m 
 model_config_rec % auxhist3_interval_s        =  auxhist3_interval_s 
 model_config_rec % auxhist3_interval          =  auxhist3_interval 
 model_config_rec % auxhist4_interval_mo       =  auxhist4_interval_mo 
 model_config_rec % auxhist4_interval_d        =  auxhist4_interval_d 
 model_config_rec % auxhist4_interval_h        =  auxhist4_interval_h 
 model_config_rec % auxhist4_interval_m        =  auxhist4_interval_m 
 model_config_rec % auxhist4_interval_s        =  auxhist4_interval_s 
 model_config_rec % auxhist4_interval          =  auxhist4_interval 
 model_config_rec % auxhist5_interval_mo       =  auxhist5_interval_mo 
 model_config_rec % auxhist5_interval_d        =  auxhist5_interval_d 
 model_config_rec % auxhist5_interval_h        =  auxhist5_interval_h 
 model_config_rec % auxhist5_interval_m        =  auxhist5_interval_m 
 model_config_rec % auxhist5_interval_s        =  auxhist5_interval_s 
 model_config_rec % auxhist5_interval          =  auxhist5_interval 
 model_config_rec % auxhist6_interval_mo       =  auxhist6_interval_mo 
 model_config_rec % auxhist6_interval_d        =  auxhist6_interval_d 
 model_config_rec % auxhist6_interval_h        =  auxhist6_interval_h 
 model_config_rec % auxhist6_interval_m        =  auxhist6_interval_m 
 model_config_rec % auxhist6_interval_s        =  auxhist6_interval_s 
 model_config_rec % auxhist6_interval          =  auxhist6_interval 
 model_config_rec % auxhist7_interval_mo       =  auxhist7_interval_mo 
 model_config_rec % auxhist7_interval_d        =  auxhist7_interval_d 
 model_config_rec % auxhist7_interval_h        =  auxhist7_interval_h 
 model_config_rec % auxhist7_interval_m        =  auxhist7_interval_m 
 model_config_rec % auxhist7_interval_s        =  auxhist7_interval_s 
 model_config_rec % auxhist7_interval          =  auxhist7_interval 
 model_config_rec % auxhist8_interval_mo       =  auxhist8_interval_mo 
 model_config_rec % auxhist8_interval_d        =  auxhist8_interval_d 
 model_config_rec % auxhist8_interval_h        =  auxhist8_interval_h 
 model_config_rec % auxhist8_interval_m        =  auxhist8_interval_m 
 model_config_rec % auxhist8_interval_s        =  auxhist8_interval_s 
 model_config_rec % auxhist8_interval          =  auxhist8_interval 
 model_config_rec % auxhist9_interval_mo       =  auxhist9_interval_mo 
 model_config_rec % auxhist9_interval_d        =  auxhist9_interval_d 
 model_config_rec % auxhist9_interval_h        =  auxhist9_interval_h 
 model_config_rec % auxhist9_interval_m        =  auxhist9_interval_m 
 model_config_rec % auxhist9_interval_s        =  auxhist9_interval_s 
 model_config_rec % auxhist9_interval          =  auxhist9_interval 
 model_config_rec % auxhist10_interval_mo      =  auxhist10_interval_mo 
 model_config_rec % auxhist10_interval_d       =  auxhist10_interval_d 
 model_config_rec % auxhist10_interval_h       =  auxhist10_interval_h 
 model_config_rec % auxhist10_interval_m       =  auxhist10_interval_m 
 model_config_rec % auxhist10_interval_s       =  auxhist10_interval_s 
 model_config_rec % auxhist10_interval         =  auxhist10_interval 
 model_config_rec % auxhist11_interval_mo      =  auxhist11_interval_mo 
 model_config_rec % auxhist11_interval_d       =  auxhist11_interval_d 
 model_config_rec % auxhist11_interval_h       =  auxhist11_interval_h 
 model_config_rec % auxhist11_interval_m       =  auxhist11_interval_m 
 model_config_rec % auxhist11_interval_s       =  auxhist11_interval_s 
 model_config_rec % auxhist11_interval         =  auxhist11_interval 
 model_config_rec % auxinput1_interval_mo      =  auxinput1_interval_mo 
 model_config_rec % auxinput1_interval_d       =  auxinput1_interval_d 
 model_config_rec % auxinput1_interval_h       =  auxinput1_interval_h 
 model_config_rec % auxinput1_interval_m       =  auxinput1_interval_m 
 model_config_rec % auxinput1_interval_s       =  auxinput1_interval_s 
 model_config_rec % auxinput1_interval         =  auxinput1_interval 
 model_config_rec % auxinput2_interval_mo      =  auxinput2_interval_mo 
 model_config_rec % auxinput2_interval_d       =  auxinput2_interval_d 
 model_config_rec % auxinput2_interval_h       =  auxinput2_interval_h 
 model_config_rec % auxinput2_interval_m       =  auxinput2_interval_m 
 model_config_rec % auxinput2_interval_s       =  auxinput2_interval_s 
 model_config_rec % auxinput2_interval         =  auxinput2_interval 
 model_config_rec % auxinput3_interval_mo      =  auxinput3_interval_mo 
 model_config_rec % auxinput3_interval_d       =  auxinput3_interval_d 
 model_config_rec % auxinput3_interval_h       =  auxinput3_interval_h 
 model_config_rec % auxinput3_interval_m       =  auxinput3_interval_m 
 model_config_rec % auxinput3_interval_s       =  auxinput3_interval_s 
 model_config_rec % auxinput3_interval         =  auxinput3_interval 
 model_config_rec % auxinput4_interval_mo      =  auxinput4_interval_mo 
 model_config_rec % auxinput4_interval_d       =  auxinput4_interval_d 
 model_config_rec % auxinput4_interval_h       =  auxinput4_interval_h 
 model_config_rec % auxinput4_interval_m       =  auxinput4_interval_m 
 model_config_rec % auxinput4_interval_s       =  auxinput4_interval_s 
 model_config_rec % auxinput4_interval         =  auxinput4_interval 
 model_config_rec % auxinput5_interval_mo      =  auxinput5_interval_mo 
 model_config_rec % auxinput5_interval_d       =  auxinput5_interval_d 
 model_config_rec % auxinput5_interval_h       =  auxinput5_interval_h 
 model_config_rec % auxinput5_interval_m       =  auxinput5_interval_m 
 model_config_rec % auxinput5_interval_s       =  auxinput5_interval_s 
 model_config_rec % auxinput5_interval         =  auxinput5_interval 
 model_config_rec % auxinput6_interval_mo      =  auxinput6_interval_mo 
 model_config_rec % auxinput6_interval_d       =  auxinput6_interval_d 
 model_config_rec % auxinput6_interval_h       =  auxinput6_interval_h 
 model_config_rec % auxinput6_interval_m       =  auxinput6_interval_m 
 model_config_rec % auxinput6_interval_s       =  auxinput6_interval_s 
 model_config_rec % auxinput6_interval         =  auxinput6_interval 
 model_config_rec % auxinput7_interval_mo      =  auxinput7_interval_mo 
 model_config_rec % auxinput7_interval_d       =  auxinput7_interval_d 
 model_config_rec % auxinput7_interval_h       =  auxinput7_interval_h 
 model_config_rec % auxinput7_interval_m       =  auxinput7_interval_m 
 model_config_rec % auxinput7_interval_s       =  auxinput7_interval_s 
 model_config_rec % auxinput7_interval         =  auxinput7_interval 
 model_config_rec % auxinput8_interval_mo      =  auxinput8_interval_mo 
 model_config_rec % auxinput8_interval_d       =  auxinput8_interval_d 
 model_config_rec % auxinput8_interval_h       =  auxinput8_interval_h 
 model_config_rec % auxinput8_interval_m       =  auxinput8_interval_m 
 model_config_rec % auxinput8_interval_s       =  auxinput8_interval_s 
 model_config_rec % auxinput8_interval         =  auxinput8_interval 
 model_config_rec % sgfdda_interval_mo         =  sgfdda_interval_mo 
 model_config_rec % sgfdda_interval_d          =  sgfdda_interval_d 
 model_config_rec % sgfdda_interval_h          =  sgfdda_interval_h 
 model_config_rec % sgfdda_interval_m          =  sgfdda_interval_m 
 model_config_rec % sgfdda_interval_s          =  sgfdda_interval_s 
 model_config_rec % sgfdda_interval            =  sgfdda_interval 
 model_config_rec % gfdda_interval_mo          =  gfdda_interval_mo 
 model_config_rec % gfdda_interval_d           =  gfdda_interval_d 
 model_config_rec % gfdda_interval_h           =  gfdda_interval_h 
 model_config_rec % gfdda_interval_m           =  gfdda_interval_m 
 model_config_rec % gfdda_interval_s           =  gfdda_interval_s 
 model_config_rec % gfdda_interval             =  gfdda_interval 
 model_config_rec % auxinput11_interval_mo     =  auxinput11_interval_mo 
 model_config_rec % auxinput11_interval_d      =  auxinput11_interval_d 
 model_config_rec % auxinput11_interval_h      =  auxinput11_interval_h 
 model_config_rec % auxinput11_interval_m      =  auxinput11_interval_m 
 model_config_rec % auxinput11_interval_s      =  auxinput11_interval_s 
 model_config_rec % auxinput11_interval        =  auxinput11_interval 
 model_config_rec % restart_interval_mo        =  restart_interval_mo 
 model_config_rec % restart_interval_d         =  restart_interval_d 
 model_config_rec % restart_interval_h         =  restart_interval_h 
 model_config_rec % restart_interval_m         =  restart_interval_m 
 model_config_rec % restart_interval_s         =  restart_interval_s 
 model_config_rec % history_begin_y            =  history_begin_y 
 model_config_rec % history_begin_mo           =  history_begin_mo 
 model_config_rec % history_begin_d            =  history_begin_d 
 model_config_rec % history_begin_h            =  history_begin_h 
 model_config_rec % history_begin_m            =  history_begin_m 
 model_config_rec % history_begin_s            =  history_begin_s 
 model_config_rec % inputout_begin_y           =  inputout_begin_y 
 model_config_rec % inputout_begin_mo          =  inputout_begin_mo 
 model_config_rec % inputout_begin_d           =  inputout_begin_d 
 model_config_rec % inputout_begin_h           =  inputout_begin_h 
 model_config_rec % inputout_begin_m           =  inputout_begin_m 
 model_config_rec % inputout_begin_s           =  inputout_begin_s 
 model_config_rec % auxhist1_begin_y           =  auxhist1_begin_y 
 model_config_rec % auxhist1_begin_mo          =  auxhist1_begin_mo 
 model_config_rec % auxhist1_begin_d           =  auxhist1_begin_d 
 model_config_rec % auxhist1_begin_h           =  auxhist1_begin_h 
 model_config_rec % auxhist1_begin_m           =  auxhist1_begin_m 
 model_config_rec % auxhist1_begin_s           =  auxhist1_begin_s 
 model_config_rec % auxhist2_begin_y           =  auxhist2_begin_y 
 model_config_rec % auxhist2_begin_mo          =  auxhist2_begin_mo 
 model_config_rec % auxhist2_begin_d           =  auxhist2_begin_d 
 model_config_rec % auxhist2_begin_h           =  auxhist2_begin_h 
 model_config_rec % auxhist2_begin_m           =  auxhist2_begin_m 
 model_config_rec % auxhist2_begin_s           =  auxhist2_begin_s 
 model_config_rec % auxhist3_begin_y           =  auxhist3_begin_y 
 model_config_rec % auxhist3_begin_mo          =  auxhist3_begin_mo 
 model_config_rec % auxhist3_begin_d           =  auxhist3_begin_d 
 model_config_rec % auxhist3_begin_h           =  auxhist3_begin_h 
 model_config_rec % auxhist3_begin_m           =  auxhist3_begin_m 
 model_config_rec % auxhist3_begin_s           =  auxhist3_begin_s 
 model_config_rec % auxhist4_begin_y           =  auxhist4_begin_y 
 model_config_rec % auxhist4_begin_mo          =  auxhist4_begin_mo 
 model_config_rec % auxhist4_begin_d           =  auxhist4_begin_d 
 model_config_rec % auxhist4_begin_h           =  auxhist4_begin_h 
 model_config_rec % auxhist4_begin_m           =  auxhist4_begin_m 
 model_config_rec % auxhist4_begin_s           =  auxhist4_begin_s 
 model_config_rec % auxhist5_begin_y           =  auxhist5_begin_y 
 model_config_rec % auxhist5_begin_mo          =  auxhist5_begin_mo 
 model_config_rec % auxhist5_begin_d           =  auxhist5_begin_d 
 model_config_rec % auxhist5_begin_h           =  auxhist5_begin_h 
 model_config_rec % auxhist5_begin_m           =  auxhist5_begin_m 
 model_config_rec % auxhist5_begin_s           =  auxhist5_begin_s 
 model_config_rec % auxhist6_begin_y           =  auxhist6_begin_y 
 model_config_rec % auxhist6_begin_mo          =  auxhist6_begin_mo 
 model_config_rec % auxhist6_begin_d           =  auxhist6_begin_d 
 model_config_rec % auxhist6_begin_h           =  auxhist6_begin_h 
 model_config_rec % auxhist6_begin_m           =  auxhist6_begin_m 
 model_config_rec % auxhist6_begin_s           =  auxhist6_begin_s 
 model_config_rec % auxhist7_begin_y           =  auxhist7_begin_y 
 model_config_rec % auxhist7_begin_mo          =  auxhist7_begin_mo 
 model_config_rec % auxhist7_begin_d           =  auxhist7_begin_d 
 model_config_rec % auxhist7_begin_h           =  auxhist7_begin_h 
 model_config_rec % auxhist7_begin_m           =  auxhist7_begin_m 
 model_config_rec % auxhist7_begin_s           =  auxhist7_begin_s 
 model_config_rec % auxhist8_begin_y           =  auxhist8_begin_y 
 model_config_rec % auxhist8_begin_mo          =  auxhist8_begin_mo 
 model_config_rec % auxhist8_begin_d           =  auxhist8_begin_d 
 model_config_rec % auxhist8_begin_h           =  auxhist8_begin_h 
 model_config_rec % auxhist8_begin_m           =  auxhist8_begin_m 
 model_config_rec % auxhist8_begin_s           =  auxhist8_begin_s 
 model_config_rec % auxhist9_begin_y           =  auxhist9_begin_y 
 model_config_rec % auxhist9_begin_mo          =  auxhist9_begin_mo 
 model_config_rec % auxhist9_begin_d           =  auxhist9_begin_d 
 model_config_rec % auxhist9_begin_h           =  auxhist9_begin_h 
 model_config_rec % auxhist9_begin_m           =  auxhist9_begin_m 
 model_config_rec % auxhist9_begin_s           =  auxhist9_begin_s 
 model_config_rec % auxhist10_begin_y          =  auxhist10_begin_y 
 model_config_rec % auxhist10_begin_mo         =  auxhist10_begin_mo 
 model_config_rec % auxhist10_begin_d          =  auxhist10_begin_d 
 model_config_rec % auxhist10_begin_h          =  auxhist10_begin_h 
 model_config_rec % auxhist10_begin_m          =  auxhist10_begin_m 
 model_config_rec % auxhist10_begin_s          =  auxhist10_begin_s 
 model_config_rec % auxhist11_begin_y          =  auxhist11_begin_y 
 model_config_rec % auxhist11_begin_mo         =  auxhist11_begin_mo 
 model_config_rec % auxhist11_begin_d          =  auxhist11_begin_d 
 model_config_rec % auxhist11_begin_h          =  auxhist11_begin_h 
 model_config_rec % auxhist11_begin_m          =  auxhist11_begin_m 
 model_config_rec % auxhist11_begin_s          =  auxhist11_begin_s 
 model_config_rec % auxinput1_begin_y          =  auxinput1_begin_y 
 model_config_rec % auxinput1_begin_mo         =  auxinput1_begin_mo 
 model_config_rec % auxinput1_begin_d          =  auxinput1_begin_d 
 model_config_rec % auxinput1_begin_h          =  auxinput1_begin_h 
 model_config_rec % auxinput1_begin_m          =  auxinput1_begin_m 
 model_config_rec % auxinput1_begin_s          =  auxinput1_begin_s 
 model_config_rec % auxinput2_begin_y          =  auxinput2_begin_y 
 model_config_rec % auxinput2_begin_mo         =  auxinput2_begin_mo 
 model_config_rec % auxinput2_begin_d          =  auxinput2_begin_d 
 model_config_rec % auxinput2_begin_h          =  auxinput2_begin_h 
 model_config_rec % auxinput2_begin_m          =  auxinput2_begin_m 
 model_config_rec % auxinput2_begin_s          =  auxinput2_begin_s 
 model_config_rec % auxinput3_begin_y          =  auxinput3_begin_y 
 model_config_rec % auxinput3_begin_mo         =  auxinput3_begin_mo 
 model_config_rec % auxinput3_begin_d          =  auxinput3_begin_d 
 model_config_rec % auxinput3_begin_h          =  auxinput3_begin_h 
 model_config_rec % auxinput3_begin_m          =  auxinput3_begin_m 
 model_config_rec % auxinput3_begin_s          =  auxinput3_begin_s 
 model_config_rec % auxinput4_begin_y          =  auxinput4_begin_y 
 model_config_rec % auxinput4_begin_mo         =  auxinput4_begin_mo 
 model_config_rec % auxinput4_begin_d          =  auxinput4_begin_d 
 model_config_rec % auxinput4_begin_h          =  auxinput4_begin_h 
 model_config_rec % auxinput4_begin_m          =  auxinput4_begin_m 
 model_config_rec % auxinput4_begin_s          =  auxinput4_begin_s 
 model_config_rec % auxinput5_begin_y          =  auxinput5_begin_y 
 model_config_rec % auxinput5_begin_mo         =  auxinput5_begin_mo 
 model_config_rec % auxinput5_begin_d          =  auxinput5_begin_d 
 model_config_rec % auxinput5_begin_h          =  auxinput5_begin_h 
 model_config_rec % auxinput5_begin_m          =  auxinput5_begin_m 
 model_config_rec % auxinput5_begin_s          =  auxinput5_begin_s 
 model_config_rec % auxinput6_begin_y          =  auxinput6_begin_y 
 model_config_rec % auxinput6_begin_mo         =  auxinput6_begin_mo 
 model_config_rec % auxinput6_begin_d          =  auxinput6_begin_d 
 model_config_rec % auxinput6_begin_h          =  auxinput6_begin_h 
 model_config_rec % auxinput6_begin_m          =  auxinput6_begin_m 
 model_config_rec % auxinput6_begin_s          =  auxinput6_begin_s 
 model_config_rec % auxinput7_begin_y          =  auxinput7_begin_y 
 model_config_rec % auxinput7_begin_mo         =  auxinput7_begin_mo 
 model_config_rec % auxinput7_begin_d          =  auxinput7_begin_d 
 model_config_rec % auxinput7_begin_h          =  auxinput7_begin_h 
 model_config_rec % auxinput7_begin_m          =  auxinput7_begin_m 
 model_config_rec % auxinput7_begin_s          =  auxinput7_begin_s 
 model_config_rec % auxinput8_begin_y          =  auxinput8_begin_y 
 model_config_rec % auxinput8_begin_mo         =  auxinput8_begin_mo 
 model_config_rec % auxinput8_begin_d          =  auxinput8_begin_d 
 model_config_rec % auxinput8_begin_h          =  auxinput8_begin_h 
 model_config_rec % auxinput8_begin_m          =  auxinput8_begin_m 
 model_config_rec % auxinput8_begin_s          =  auxinput8_begin_s 
 model_config_rec % sgfdda_begin_y             =  sgfdda_begin_y 
 model_config_rec % sgfdda_begin_mo            =  sgfdda_begin_mo 
 model_config_rec % sgfdda_begin_d             =  sgfdda_begin_d 
 model_config_rec % sgfdda_begin_h             =  sgfdda_begin_h 
 model_config_rec % sgfdda_begin_m             =  sgfdda_begin_m 
 model_config_rec % sgfdda_begin_s             =  sgfdda_begin_s 
 model_config_rec % gfdda_begin_y              =  gfdda_begin_y 
 model_config_rec % gfdda_begin_mo             =  gfdda_begin_mo 
 model_config_rec % gfdda_begin_d              =  gfdda_begin_d 
 model_config_rec % gfdda_begin_h              =  gfdda_begin_h 
 model_config_rec % gfdda_begin_m              =  gfdda_begin_m 
 model_config_rec % gfdda_begin_s              =  gfdda_begin_s 
 model_config_rec % auxinput11_begin_y         =  auxinput11_begin_y 
 model_config_rec % auxinput11_begin_mo        =  auxinput11_begin_mo 
 model_config_rec % auxinput11_begin_d         =  auxinput11_begin_d 
 model_config_rec % auxinput11_begin_h         =  auxinput11_begin_h 
 model_config_rec % auxinput11_begin_m         =  auxinput11_begin_m 
 model_config_rec % auxinput11_begin_s         =  auxinput11_begin_s 
 model_config_rec % restart_begin_y            =  restart_begin_y 
 model_config_rec % restart_begin_mo           =  restart_begin_mo 
 model_config_rec % restart_begin_d            =  restart_begin_d 
 model_config_rec % restart_begin_h            =  restart_begin_h 
 model_config_rec % restart_begin_m            =  restart_begin_m 
 model_config_rec % restart_begin_s            =  restart_begin_s 
 model_config_rec % history_end_y              =  history_end_y 
 model_config_rec % history_end_mo             =  history_end_mo 
 model_config_rec % history_end_d              =  history_end_d 
 model_config_rec % history_end_h              =  history_end_h 
 model_config_rec % history_end_m              =  history_end_m 
 model_config_rec % history_end_s              =  history_end_s 
 model_config_rec % inputout_end_y             =  inputout_end_y 
 model_config_rec % inputout_end_mo            =  inputout_end_mo 
 model_config_rec % inputout_end_d             =  inputout_end_d 
 model_config_rec % inputout_end_h             =  inputout_end_h 
 model_config_rec % inputout_end_m             =  inputout_end_m 
 model_config_rec % inputout_end_s             =  inputout_end_s 
 model_config_rec % auxhist1_end_y             =  auxhist1_end_y 
 model_config_rec % auxhist1_end_mo            =  auxhist1_end_mo 
 model_config_rec % auxhist1_end_d             =  auxhist1_end_d 
 model_config_rec % auxhist1_end_h             =  auxhist1_end_h 
 model_config_rec % auxhist1_end_m             =  auxhist1_end_m 
 model_config_rec % auxhist1_end_s             =  auxhist1_end_s 
 model_config_rec % auxhist2_end_y             =  auxhist2_end_y 
 model_config_rec % auxhist2_end_mo            =  auxhist2_end_mo 
 model_config_rec % auxhist2_end_d             =  auxhist2_end_d 
 model_config_rec % auxhist2_end_h             =  auxhist2_end_h 
 model_config_rec % auxhist2_end_m             =  auxhist2_end_m 
 model_config_rec % auxhist2_end_s             =  auxhist2_end_s 
 model_config_rec % auxhist3_end_y             =  auxhist3_end_y 
 model_config_rec % auxhist3_end_mo            =  auxhist3_end_mo 
 model_config_rec % auxhist3_end_d             =  auxhist3_end_d 
 model_config_rec % auxhist3_end_h             =  auxhist3_end_h 
 model_config_rec % auxhist3_end_m             =  auxhist3_end_m 
 model_config_rec % auxhist3_end_s             =  auxhist3_end_s 
 model_config_rec % auxhist4_end_y             =  auxhist4_end_y 
 model_config_rec % auxhist4_end_mo            =  auxhist4_end_mo 
 model_config_rec % auxhist4_end_d             =  auxhist4_end_d 
 model_config_rec % auxhist4_end_h             =  auxhist4_end_h 
 model_config_rec % auxhist4_end_m             =  auxhist4_end_m 
 model_config_rec % auxhist4_end_s             =  auxhist4_end_s 
 model_config_rec % auxhist5_end_y             =  auxhist5_end_y 
 model_config_rec % auxhist5_end_mo            =  auxhist5_end_mo 
 model_config_rec % auxhist5_end_d             =  auxhist5_end_d 
 model_config_rec % auxhist5_end_h             =  auxhist5_end_h 
 model_config_rec % auxhist5_end_m             =  auxhist5_end_m 
 model_config_rec % auxhist5_end_s             =  auxhist5_end_s 
 model_config_rec % auxhist6_end_y             =  auxhist6_end_y 
 model_config_rec % auxhist6_end_mo            =  auxhist6_end_mo 
 model_config_rec % auxhist6_end_d             =  auxhist6_end_d 
 model_config_rec % auxhist6_end_h             =  auxhist6_end_h 
 model_config_rec % auxhist6_end_m             =  auxhist6_end_m 
 model_config_rec % auxhist6_end_s             =  auxhist6_end_s 
 model_config_rec % auxhist7_end_y             =  auxhist7_end_y 
 model_config_rec % auxhist7_end_mo            =  auxhist7_end_mo 
 model_config_rec % auxhist7_end_d             =  auxhist7_end_d 
 model_config_rec % auxhist7_end_h             =  auxhist7_end_h 
 model_config_rec % auxhist7_end_m             =  auxhist7_end_m 
 model_config_rec % auxhist7_end_s             =  auxhist7_end_s 
 model_config_rec % auxhist8_end_y             =  auxhist8_end_y 
 model_config_rec % auxhist8_end_mo            =  auxhist8_end_mo 
 model_config_rec % auxhist8_end_d             =  auxhist8_end_d 
 model_config_rec % auxhist8_end_h             =  auxhist8_end_h 
 model_config_rec % auxhist8_end_m             =  auxhist8_end_m 
 model_config_rec % auxhist8_end_s             =  auxhist8_end_s 
 model_config_rec % auxhist9_end_y             =  auxhist9_end_y 
 model_config_rec % auxhist9_end_mo            =  auxhist9_end_mo 
 model_config_rec % auxhist9_end_d             =  auxhist9_end_d 
 model_config_rec % auxhist9_end_h             =  auxhist9_end_h 
 model_config_rec % auxhist9_end_m             =  auxhist9_end_m 
 model_config_rec % auxhist9_end_s             =  auxhist9_end_s 
 model_config_rec % auxhist10_end_y            =  auxhist10_end_y 
 model_config_rec % auxhist10_end_mo           =  auxhist10_end_mo 
 model_config_rec % auxhist10_end_d            =  auxhist10_end_d 
 model_config_rec % auxhist10_end_h            =  auxhist10_end_h 
 model_config_rec % auxhist10_end_m            =  auxhist10_end_m 
 model_config_rec % auxhist10_end_s            =  auxhist10_end_s 
 model_config_rec % auxhist11_end_y            =  auxhist11_end_y 
 model_config_rec % auxhist11_end_mo           =  auxhist11_end_mo 
 model_config_rec % auxhist11_end_d            =  auxhist11_end_d 
 model_config_rec % auxhist11_end_h            =  auxhist11_end_h 
 model_config_rec % auxhist11_end_m            =  auxhist11_end_m 
 model_config_rec % auxhist11_end_s            =  auxhist11_end_s 
 model_config_rec % auxinput1_end_y            =  auxinput1_end_y 
 model_config_rec % auxinput1_end_mo           =  auxinput1_end_mo 
 model_config_rec % auxinput1_end_d            =  auxinput1_end_d 
 model_config_rec % auxinput1_end_h            =  auxinput1_end_h 
 model_config_rec % auxinput1_end_m            =  auxinput1_end_m 
 model_config_rec % auxinput1_end_s            =  auxinput1_end_s 
 model_config_rec % auxinput2_end_y            =  auxinput2_end_y 
 model_config_rec % auxinput2_end_mo           =  auxinput2_end_mo 
 model_config_rec % auxinput2_end_d            =  auxinput2_end_d 
 model_config_rec % auxinput2_end_h            =  auxinput2_end_h 
 model_config_rec % auxinput2_end_m            =  auxinput2_end_m 
 model_config_rec % auxinput2_end_s            =  auxinput2_end_s 
 model_config_rec % auxinput3_end_y            =  auxinput3_end_y 
 model_config_rec % auxinput3_end_mo           =  auxinput3_end_mo 
 model_config_rec % auxinput3_end_d            =  auxinput3_end_d 
 model_config_rec % auxinput3_end_h            =  auxinput3_end_h 
 model_config_rec % auxinput3_end_m            =  auxinput3_end_m 
 model_config_rec % auxinput3_end_s            =  auxinput3_end_s 
 model_config_rec % auxinput4_end_y            =  auxinput4_end_y 
 model_config_rec % auxinput4_end_mo           =  auxinput4_end_mo 
 model_config_rec % auxinput4_end_d            =  auxinput4_end_d 
 model_config_rec % auxinput4_end_h            =  auxinput4_end_h 
 model_config_rec % auxinput4_end_m            =  auxinput4_end_m 
 model_config_rec % auxinput4_end_s            =  auxinput4_end_s 
 model_config_rec % auxinput5_end_y            =  auxinput5_end_y 
 model_config_rec % auxinput5_end_mo           =  auxinput5_end_mo 
 model_config_rec % auxinput5_end_d            =  auxinput5_end_d 
 model_config_rec % auxinput5_end_h            =  auxinput5_end_h 
 model_config_rec % auxinput5_end_m            =  auxinput5_end_m 
 model_config_rec % auxinput5_end_s            =  auxinput5_end_s 
 model_config_rec % auxinput6_end_y            =  auxinput6_end_y 
 model_config_rec % auxinput6_end_mo           =  auxinput6_end_mo 
 model_config_rec % auxinput6_end_d            =  auxinput6_end_d 
 model_config_rec % auxinput6_end_h            =  auxinput6_end_h 
 model_config_rec % auxinput6_end_m            =  auxinput6_end_m 
 model_config_rec % auxinput6_end_s            =  auxinput6_end_s 
 model_config_rec % auxinput7_end_y            =  auxinput7_end_y 
 model_config_rec % auxinput7_end_mo           =  auxinput7_end_mo 
 model_config_rec % auxinput7_end_d            =  auxinput7_end_d 
 model_config_rec % auxinput7_end_h            =  auxinput7_end_h 
 model_config_rec % auxinput7_end_m            =  auxinput7_end_m 
 model_config_rec % auxinput7_end_s            =  auxinput7_end_s 
 model_config_rec % auxinput8_end_y            =  auxinput8_end_y 
 model_config_rec % auxinput8_end_mo           =  auxinput8_end_mo 
 model_config_rec % auxinput8_end_d            =  auxinput8_end_d 
 model_config_rec % auxinput8_end_h            =  auxinput8_end_h 
 model_config_rec % auxinput8_end_m            =  auxinput8_end_m 
 model_config_rec % auxinput8_end_s            =  auxinput8_end_s 
 model_config_rec % sgfdda_end_y               =  sgfdda_end_y 
 model_config_rec % sgfdda_end_mo              =  sgfdda_end_mo 
 model_config_rec % sgfdda_end_d               =  sgfdda_end_d 
 model_config_rec % sgfdda_end_h               =  sgfdda_end_h 
 model_config_rec % sgfdda_end_m               =  sgfdda_end_m 
 model_config_rec % sgfdda_end_s               =  sgfdda_end_s 
 model_config_rec % gfdda_end_y                =  gfdda_end_y 
 model_config_rec % gfdda_end_mo               =  gfdda_end_mo 
 model_config_rec % gfdda_end_d                =  gfdda_end_d 
 model_config_rec % gfdda_end_h                =  gfdda_end_h 
 model_config_rec % gfdda_end_m                =  gfdda_end_m 
 model_config_rec % gfdda_end_s                =  gfdda_end_s 
 model_config_rec % auxinput11_end_y           =  auxinput11_end_y 
 model_config_rec % auxinput11_end_mo          =  auxinput11_end_mo 
 model_config_rec % auxinput11_end_d           =  auxinput11_end_d 
 model_config_rec % auxinput11_end_h           =  auxinput11_end_h 
 model_config_rec % auxinput11_end_m           =  auxinput11_end_m 
 model_config_rec % auxinput11_end_s           =  auxinput11_end_s 
 model_config_rec % io_form_auxinput1          =  io_form_auxinput1 
 model_config_rec % io_form_auxinput2          =  io_form_auxinput2 
 model_config_rec % io_form_auxinput3          =  io_form_auxinput3 
 model_config_rec % io_form_auxinput4          =  io_form_auxinput4 
 model_config_rec % io_form_auxinput5          =  io_form_auxinput5 
 model_config_rec % io_form_auxinput6          =  io_form_auxinput6 
 model_config_rec % io_form_auxinput7          =  io_form_auxinput7 
 model_config_rec % io_form_auxinput8          =  io_form_auxinput8 
 model_config_rec % io_form_sgfdda             =  io_form_sgfdda 
 model_config_rec % io_form_gfdda              =  io_form_gfdda 
 model_config_rec % io_form_auxinput11         =  io_form_auxinput11 
 model_config_rec % io_form_auxhist1           =  io_form_auxhist1 
 model_config_rec % io_form_auxhist2           =  io_form_auxhist2 
 model_config_rec % io_form_auxhist3           =  io_form_auxhist3 
 model_config_rec % io_form_auxhist4           =  io_form_auxhist4 
 model_config_rec % io_form_auxhist5           =  io_form_auxhist5 
 model_config_rec % io_form_auxhist6           =  io_form_auxhist6 
 model_config_rec % io_form_auxhist7           =  io_form_auxhist7 
 model_config_rec % io_form_auxhist8           =  io_form_auxhist8 
 model_config_rec % io_form_auxhist9           =  io_form_auxhist9 
 model_config_rec % io_form_auxhist10          =  io_form_auxhist10 
 model_config_rec % io_form_auxhist11          =  io_form_auxhist11 
 model_config_rec % simulation_start_year      =  simulation_start_year 
 model_config_rec % simulation_start_month     =  simulation_start_month 
 model_config_rec % simulation_start_day       =  simulation_start_day 
 model_config_rec % simulation_start_hour      =  simulation_start_hour 
 model_config_rec % simulation_start_minute    =  simulation_start_minute 
 model_config_rec % simulation_start_second    =  simulation_start_second 
 model_config_rec % reset_simulation_start     =  reset_simulation_start 
 model_config_rec % sr_x                       =  sr_x 
 model_config_rec % sr_y                       =  sr_y 
 model_config_rec % julyr                      =  julyr 
 model_config_rec % julday                     =  julday 
 model_config_rec % gmt                        =  gmt 
 model_config_rec % input_inname               =  input_inname 
 model_config_rec % input_outname              =  input_outname 
 model_config_rec % bdy_inname                 =  bdy_inname 
 model_config_rec % bdy_outname                =  bdy_outname 
 model_config_rec % rst_inname                 =  rst_inname 
 model_config_rec % rst_outname                =  rst_outname 
 model_config_rec % write_input                =  write_input 
 model_config_rec % write_restart_at_0h        =  write_restart_at_0h 
 model_config_rec % adjust_output_times        =  adjust_output_times 
 model_config_rec % adjust_input_times         =  adjust_input_times 
 model_config_rec % tstart                     =  tstart 
 model_config_rec % nocolons                   =  nocolons 
 model_config_rec % cycling                    =  cycling 
 model_config_rec % dfi_opt                    =  dfi_opt 
 model_config_rec % dfi_nfilter                =  dfi_nfilter 
 model_config_rec % dfi_write_filtered_input   =  dfi_write_filtered_input 
 model_config_rec % dfi_write_dfi_history      =  dfi_write_dfi_history 
 model_config_rec % dfi_cutoff_seconds         =  dfi_cutoff_seconds 
 model_config_rec % dfi_time_dim               =  dfi_time_dim 
 model_config_rec % dfi_fwdstop_year           =  dfi_fwdstop_year 
 model_config_rec % dfi_fwdstop_month          =  dfi_fwdstop_month 
 model_config_rec % dfi_fwdstop_day            =  dfi_fwdstop_day 
 model_config_rec % dfi_fwdstop_hour           =  dfi_fwdstop_hour 
 model_config_rec % dfi_fwdstop_minute         =  dfi_fwdstop_minute 
 model_config_rec % dfi_fwdstop_second         =  dfi_fwdstop_second 
 model_config_rec % dfi_bckstop_year           =  dfi_bckstop_year 
 model_config_rec % dfi_bckstop_month          =  dfi_bckstop_month 
 model_config_rec % dfi_bckstop_day            =  dfi_bckstop_day 
 model_config_rec % dfi_bckstop_hour           =  dfi_bckstop_hour 
 model_config_rec % dfi_bckstop_minute         =  dfi_bckstop_minute 
 model_config_rec % dfi_bckstop_second         =  dfi_bckstop_second 
 model_config_rec % time_step                  =  time_step 
 model_config_rec % time_step_fract_num        =  time_step_fract_num 
 model_config_rec % time_step_fract_den        =  time_step_fract_den 
 model_config_rec % max_dom                    =  max_dom 
 model_config_rec % s_we                       =  s_we 
 model_config_rec % e_we                       =  e_we 
 model_config_rec % s_sn                       =  s_sn 
 model_config_rec % e_sn                       =  e_sn 
 model_config_rec % s_vert                     =  s_vert 
 model_config_rec % e_vert                     =  e_vert 
 model_config_rec % dx                         =  dx 
 model_config_rec % dy                         =  dy 
 model_config_rec % grid_id                    =  grid_id 
 model_config_rec % grid_allowed               =  grid_allowed 
 model_config_rec % parent_id                  =  parent_id 
 model_config_rec % i_parent_start             =  i_parent_start 
 model_config_rec % j_parent_start             =  j_parent_start 
 model_config_rec % parent_grid_ratio          =  parent_grid_ratio 
 model_config_rec % parent_time_step_ratio     =  parent_time_step_ratio 
 model_config_rec % feedback                   =  feedback 
 model_config_rec % smooth_option              =  smooth_option 
 model_config_rec % ztop                       =  ztop 
 model_config_rec % moad_grid_ratio            =  moad_grid_ratio 
 model_config_rec % moad_time_step_ratio       =  moad_time_step_ratio 
 model_config_rec % shw                        =  shw 
 model_config_rec % tile_sz_x                  =  tile_sz_x 
 model_config_rec % tile_sz_y                  =  tile_sz_y 
 model_config_rec % numtiles                   =  numtiles 
 model_config_rec % nproc_x                    =  nproc_x 
 model_config_rec % nproc_y                    =  nproc_y 
 model_config_rec % irand                      =  irand 
 model_config_rec % dt                         =  dt 
 model_config_rec % ts_buf_size                =  ts_buf_size 
 model_config_rec % max_ts_locs                =  max_ts_locs 
 model_config_rec % num_moves                  =  num_moves 
 model_config_rec % move_id                    =  move_id 
 model_config_rec % move_interval              =  move_interval 
 model_config_rec % move_cd_x                  =  move_cd_x 
 model_config_rec % move_cd_y                  =  move_cd_y 
 model_config_rec % swap_x                     =  swap_x 
 model_config_rec % swap_y                     =  swap_y 
 model_config_rec % cycle_x                    =  cycle_x 
 model_config_rec % cycle_y                    =  cycle_y 
 model_config_rec % reorder_mesh               =  reorder_mesh 
 model_config_rec % perturb_input              =  perturb_input 
 model_config_rec % eta_levels                 =  eta_levels 
 model_config_rec % ptsgm                      =  ptsgm 
 model_config_rec % num_metgrid_levels         =  num_metgrid_levels 
 model_config_rec % p_top_requested            =  p_top_requested 
 model_config_rec % mp_physics                 =  mp_physics 
 model_config_rec % ra_lw_physics              =  ra_lw_physics 
 model_config_rec % ra_sw_physics              =  ra_sw_physics 
 model_config_rec % radt                       =  radt 
 model_config_rec % sf_sfclay_physics          =  sf_sfclay_physics 
 model_config_rec % sf_surface_physics         =  sf_surface_physics 
 model_config_rec % bl_pbl_physics             =  bl_pbl_physics 
 model_config_rec % sf_urban_physics           =  sf_urban_physics 
 model_config_rec % bldt                       =  bldt 
 model_config_rec % cu_physics                 =  cu_physics 
 model_config_rec % cudt                       =  cudt 
 model_config_rec % gsmdt                      =  gsmdt 
 model_config_rec % isfflx                     =  isfflx 
 model_config_rec % ifsnow                     =  ifsnow 
 model_config_rec % icloud                     =  icloud 
 model_config_rec % swrad_scat                 =  swrad_scat 
 model_config_rec % surface_input_source       =  surface_input_source 
 model_config_rec % num_soil_layers            =  num_soil_layers 
 model_config_rec % num_urban_layers           =  num_urban_layers 
 model_config_rec % maxiens                    =  maxiens 
 model_config_rec % maxens                     =  maxens 
 model_config_rec % maxens2                    =  maxens2 
 model_config_rec % maxens3                    =  maxens3 
 model_config_rec % ensdim                     =  ensdim 
 model_config_rec % chem_opt                   =  chem_opt 
 model_config_rec % num_land_cat               =  num_land_cat 
 model_config_rec % num_soil_cat               =  num_soil_cat 
 model_config_rec % mp_zero_out                =  mp_zero_out 
 model_config_rec % mp_zero_out_thresh         =  mp_zero_out_thresh 
 model_config_rec % seaice_threshold           =  seaice_threshold 
 model_config_rec % fractional_seaice          =  fractional_seaice 
 model_config_rec % sst_update                 =  sst_update 
 model_config_rec % usemonalb                  =  usemonalb 
 model_config_rec % rdmaxalb                   =  rdmaxalb 
 model_config_rec % rdlai2d                    =  rdlai2d 
 model_config_rec % gwd_opt                    =  gwd_opt 
 model_config_rec % idtad                      =  idtad 
 model_config_rec % nsoil                      =  nsoil 
 model_config_rec % nphs                       =  nphs 
 model_config_rec % ncnvc                      =  ncnvc 
 model_config_rec % nrads                      =  nrads 
 model_config_rec % nradl                      =  nradl 
 model_config_rec % tprec                      =  tprec 
 model_config_rec % theat                      =  theat 
 model_config_rec % tclod                      =  tclod 
 model_config_rec % trdsw                      =  trdsw 
 model_config_rec % trdlw                      =  trdlw 
 model_config_rec % tsrfc                      =  tsrfc 
 model_config_rec % pcpflg                     =  pcpflg 
 model_config_rec % sigma                      =  sigma 
 model_config_rec % sfenth                     =  sfenth 
 model_config_rec % co2tf                      =  co2tf 
 model_config_rec % ra_call_offset             =  ra_call_offset 
 model_config_rec % cam_abs_freq_s             =  cam_abs_freq_s 
 model_config_rec % levsiz                     =  levsiz 
 model_config_rec % paerlev                    =  paerlev 
 model_config_rec % cam_abs_dim1               =  cam_abs_dim1 
 model_config_rec % cam_abs_dim2               =  cam_abs_dim2 
 model_config_rec % cu_rad_feedback            =  cu_rad_feedback 
 model_config_rec % dyn_opt                    =  dyn_opt 
 model_config_rec % rk_ord                     =  rk_ord 
 model_config_rec % w_damping                  =  w_damping 
 model_config_rec % diff_opt                   =  diff_opt 
 model_config_rec % km_opt                     =  km_opt 
 model_config_rec % damp_opt                   =  damp_opt 
 model_config_rec % zdamp                      =  zdamp 
 model_config_rec % base_pres                  =  base_pres 
 model_config_rec % base_temp                  =  base_temp 
 model_config_rec % base_lapse                 =  base_lapse 
 model_config_rec % iso_temp                   =  iso_temp 
 model_config_rec % dampcoef                   =  dampcoef 
 model_config_rec % khdif                      =  khdif 
 model_config_rec % kvdif                      =  kvdif 
 model_config_rec % c_s                        =  c_s 
 model_config_rec % c_k                        =  c_k 
 model_config_rec % smdiv                      =  smdiv 
 model_config_rec % emdiv                      =  emdiv 
 model_config_rec % epssm                      =  epssm 
 model_config_rec % non_hydrostatic            =  non_hydrostatic 
 model_config_rec % time_step_sound            =  time_step_sound 
 model_config_rec % h_mom_adv_order            =  h_mom_adv_order 
 model_config_rec % v_mom_adv_order            =  v_mom_adv_order 
 model_config_rec % h_sca_adv_order            =  h_sca_adv_order 
 model_config_rec % v_sca_adv_order            =  v_sca_adv_order 
 model_config_rec % top_radiation              =  top_radiation 
 model_config_rec % tke_upper_bound            =  tke_upper_bound 
 model_config_rec % tke_drag_coefficient       =  tke_drag_coefficient 
 model_config_rec % tke_heat_flux              =  tke_heat_flux 
 model_config_rec % pert_coriolis              =  pert_coriolis 
 model_config_rec % euler_adv                  =  euler_adv 
 model_config_rec % idtadt                     =  idtadt 
 model_config_rec % idtadc                     =  idtadc 
 model_config_rec % boundary_flux              =  boundary_flux 
 model_config_rec % spec_bdy_width             =  spec_bdy_width 
 model_config_rec % spec_zone                  =  spec_zone 
 model_config_rec % relax_zone                 =  relax_zone 
 model_config_rec % specified                  =  specified 
 model_config_rec % periodic_x                 =  periodic_x 
 model_config_rec % symmetric_xs               =  symmetric_xs 
 model_config_rec % symmetric_xe               =  symmetric_xe 
 model_config_rec % open_xs                    =  open_xs 
 model_config_rec % open_xe                    =  open_xe 
 model_config_rec % periodic_y                 =  periodic_y 
 model_config_rec % symmetric_ys               =  symmetric_ys 
 model_config_rec % symmetric_ye               =  symmetric_ye 
 model_config_rec % open_ys                    =  open_ys 
 model_config_rec % open_ye                    =  open_ye 
 model_config_rec % polar                      =  polar 
 model_config_rec % nested                     =  nested 
 model_config_rec % real_data_init_type        =  real_data_init_type 
 model_config_rec % background_proc_id         =  background_proc_id 
 model_config_rec % forecast_proc_id           =  forecast_proc_id 
 model_config_rec % production_status          =  production_status 
 model_config_rec % compression                =  compression 
 model_config_rec % cen_lat                    =  cen_lat 
 model_config_rec % cen_lon                    =  cen_lon 
 model_config_rec % truelat1                   =  truelat1 
 model_config_rec % truelat2                   =  truelat2 
 model_config_rec % moad_cen_lat               =  moad_cen_lat 
 model_config_rec % stand_lon                  =  stand_lon 
 model_config_rec % flag_metgrid               =  flag_metgrid 
 model_config_rec % flag_snow                  =  flag_snow 
 model_config_rec % flag_psfc                  =  flag_psfc 
 model_config_rec % flag_sm000010              =  flag_sm000010 
 model_config_rec % flag_sm010040              =  flag_sm010040 
 model_config_rec % flag_sm040100              =  flag_sm040100 
 model_config_rec % flag_sm100200              =  flag_sm100200 
 model_config_rec % flag_st000010              =  flag_st000010 
 model_config_rec % flag_st010040              =  flag_st010040 
 model_config_rec % flag_st040100              =  flag_st040100 
 model_config_rec % flag_st100200              =  flag_st100200 
 model_config_rec % flag_slp                   =  flag_slp 
 model_config_rec % flag_soilhgt               =  flag_soilhgt 
 model_config_rec % flag_mf_xy                 =  flag_mf_xy 
 model_config_rec % bdyfrq                     =  bdyfrq 
 model_config_rec % mminlu                     =  mminlu 
 model_config_rec % iswater                    =  iswater 
 model_config_rec % islake                     =  islake 
 model_config_rec % isice                      =  isice 
 model_config_rec % isurban                    =  isurban 
 model_config_rec % isoilwater                 =  isoilwater 
 model_config_rec % map_proj                   =  map_proj 
 model_config_rec % dfi_stage                  =  dfi_stage 
 model_config_rec % mp_physics_dfi             =  mp_physics_dfi 



      CLOSE ( UNIT = nml_read_unit , IOSTAT = io_status )

      IF ( io_status .NE. 0 ) THEN
        CALL wrf_error_fatal3("",5007,&
'ERROR CLOSING namelist.input' )
      ENDIF

      CLOSE ( UNIT = nml_write_unit , IOSTAT = io_status )

      IF ( io_status .NE. 0 ) THEN
        CALL wrf_error_fatal3("",5014,&
'ERROR CLOSING namelist.output' )
      ENDIF

      RETURN

   END SUBROUTINE initial_config

   SUBROUTINE get_config_as_buffer( buffer, buflen, ncopied )

      INTEGER,   INTENT(INOUT) ::  buffer(*)
      INTEGER,   INTENT(IN)    ::  buflen
      INTEGER,   INTENT(OUT)   ::  ncopied

      INTEGER :: nbytes
      CALL wrf_num_bytes_between ( model_config_rec%last_item_in_struct ,   &
                                   model_config_rec%first_item_in_struct ,  &
                                   nbytes )


      IF ( nbytes .gt. buflen ) THEN
        CALL wrf_error_fatal3("",5035,&
        "get_config_rec_as_buffer: buffer size too small for config_rec" )
      ENDIF
      CALL wrf_mem_copy( model_config_rec, buffer, nbytes )
      ncopied = nbytes
      RETURN
   END SUBROUTINE get_config_as_buffer

   SUBROUTINE set_config_as_buffer( buffer, buflen )

      INTEGER,   INTENT(INOUT) ::  buffer(*)
      INTEGER,   INTENT(IN)    ::  buflen

      INTEGER :: nbytes
      CALL wrf_num_bytes_between ( model_config_rec%last_item_in_struct ,  &
                                   model_config_rec%first_item_in_struct , &
                                   nbytes )


      IF ( nbytes .gt. buflen ) THEN
        CALL wrf_error_fatal3("",5055,&
        "set_config_rec_as_buffer: buffer length too small to fill model config record" )
      ENDIF
      CALL wrf_mem_copy( buffer, model_config_rec, nbytes )
      RETURN
   END SUBROUTINE set_config_as_buffer

   SUBROUTINE model_to_grid_config_rec ( id_id , model_config_rec , grid_config_rec )
      INTEGER , INTENT(IN)                         ::  id_id
      TYPE ( model_config_rec_type ) , INTENT(IN)  ::  model_config_rec
      TYPE ( grid_config_rec_type  ) , INTENT(OUT) ::  grid_config_rec


































 grid_config_rec % run_days                   = model_config_rec % run_days 
 grid_config_rec % run_hours                  = model_config_rec % run_hours 
 grid_config_rec % run_minutes                = model_config_rec % run_minutes 
 grid_config_rec % run_seconds                = model_config_rec % run_seconds 
 grid_config_rec % start_year                 = model_config_rec % start_year (id_id)
 grid_config_rec % start_month                = model_config_rec % start_month (id_id)
 grid_config_rec % start_day                  = model_config_rec % start_day (id_id)
 grid_config_rec % start_hour                 = model_config_rec % start_hour (id_id)
 grid_config_rec % start_minute               = model_config_rec % start_minute (id_id)
 grid_config_rec % start_second               = model_config_rec % start_second (id_id)
 grid_config_rec % end_year                   = model_config_rec % end_year (id_id)
 grid_config_rec % end_month                  = model_config_rec % end_month (id_id)
 grid_config_rec % end_day                    = model_config_rec % end_day (id_id)
 grid_config_rec % end_hour                   = model_config_rec % end_hour (id_id)
 grid_config_rec % end_minute                 = model_config_rec % end_minute (id_id)
 grid_config_rec % end_second                 = model_config_rec % end_second (id_id)
 grid_config_rec % interval_seconds           = model_config_rec % interval_seconds 
 grid_config_rec % input_from_file            = model_config_rec % input_from_file (id_id)
 grid_config_rec % fine_input_stream          = model_config_rec % fine_input_stream (id_id)
 grid_config_rec % history_interval           = model_config_rec % history_interval (id_id)
 grid_config_rec % frames_per_outfile         = model_config_rec % frames_per_outfile (id_id)
 grid_config_rec % frames_per_auxhist1        = model_config_rec % frames_per_auxhist1 (id_id)
 grid_config_rec % frames_per_auxhist2        = model_config_rec % frames_per_auxhist2 (id_id)
 grid_config_rec % frames_per_auxhist3        = model_config_rec % frames_per_auxhist3 (id_id)
 grid_config_rec % frames_per_auxhist4        = model_config_rec % frames_per_auxhist4 (id_id)
 grid_config_rec % frames_per_auxhist5        = model_config_rec % frames_per_auxhist5 (id_id)
 grid_config_rec % frames_per_auxhist6        = model_config_rec % frames_per_auxhist6 (id_id)
 grid_config_rec % frames_per_auxhist7        = model_config_rec % frames_per_auxhist7 (id_id)
 grid_config_rec % frames_per_auxhist8        = model_config_rec % frames_per_auxhist8 (id_id)
 grid_config_rec % frames_per_auxhist9        = model_config_rec % frames_per_auxhist9 (id_id)
 grid_config_rec % frames_per_auxhist10       = model_config_rec % frames_per_auxhist10 (id_id)
 grid_config_rec % frames_per_auxhist11       = model_config_rec % frames_per_auxhist11 (id_id)
 grid_config_rec % restart                    = model_config_rec % restart 
 grid_config_rec % restart_interval           = model_config_rec % restart_interval 
 grid_config_rec % io_form_input              = model_config_rec % io_form_input 
 grid_config_rec % io_form_history            = model_config_rec % io_form_history 
 grid_config_rec % io_form_restart            = model_config_rec % io_form_restart 
 grid_config_rec % io_form_boundary           = model_config_rec % io_form_boundary 
 grid_config_rec % debug_level                = model_config_rec % debug_level 
 grid_config_rec % self_test_domain           = model_config_rec % self_test_domain 
 grid_config_rec % history_outname            = model_config_rec % history_outname 
 grid_config_rec % auxhist1_outname           = model_config_rec % auxhist1_outname 
 grid_config_rec % auxhist2_outname           = model_config_rec % auxhist2_outname 
 grid_config_rec % auxhist3_outname           = model_config_rec % auxhist3_outname 
 grid_config_rec % auxhist4_outname           = model_config_rec % auxhist4_outname 
 grid_config_rec % auxhist5_outname           = model_config_rec % auxhist5_outname 
 grid_config_rec % auxhist6_outname           = model_config_rec % auxhist6_outname 
 grid_config_rec % auxhist7_outname           = model_config_rec % auxhist7_outname 
 grid_config_rec % auxhist8_outname           = model_config_rec % auxhist8_outname 
 grid_config_rec % auxhist9_outname           = model_config_rec % auxhist9_outname 
 grid_config_rec % auxhist10_outname          = model_config_rec % auxhist10_outname 
 grid_config_rec % auxhist11_outname          = model_config_rec % auxhist11_outname 
 grid_config_rec % history_inname             = model_config_rec % history_inname 
 grid_config_rec % auxhist1_inname            = model_config_rec % auxhist1_inname 
 grid_config_rec % auxhist2_inname            = model_config_rec % auxhist2_inname 
 grid_config_rec % auxhist3_inname            = model_config_rec % auxhist3_inname 
 grid_config_rec % auxhist4_inname            = model_config_rec % auxhist4_inname 
 grid_config_rec % auxhist5_inname            = model_config_rec % auxhist5_inname 
 grid_config_rec % auxhist6_inname            = model_config_rec % auxhist6_inname 
 grid_config_rec % auxhist7_inname            = model_config_rec % auxhist7_inname 
 grid_config_rec % auxhist8_inname            = model_config_rec % auxhist8_inname 
 grid_config_rec % auxhist9_inname            = model_config_rec % auxhist9_inname 
 grid_config_rec % auxhist10_inname           = model_config_rec % auxhist10_inname 
 grid_config_rec % auxhist11_inname           = model_config_rec % auxhist11_inname 
 grid_config_rec % auxinput1_outname          = model_config_rec % auxinput1_outname 
 grid_config_rec % auxinput2_outname          = model_config_rec % auxinput2_outname 
 grid_config_rec % auxinput3_outname          = model_config_rec % auxinput3_outname 
 grid_config_rec % auxinput4_outname          = model_config_rec % auxinput4_outname 
 grid_config_rec % auxinput5_outname          = model_config_rec % auxinput5_outname 
 grid_config_rec % auxinput6_outname          = model_config_rec % auxinput6_outname 
 grid_config_rec % auxinput7_outname          = model_config_rec % auxinput7_outname 
 grid_config_rec % auxinput8_outname          = model_config_rec % auxinput8_outname 
 grid_config_rec % auxinput9_outname          = model_config_rec % auxinput9_outname 
 grid_config_rec % auxinput10_outname         = model_config_rec % auxinput10_outname 
 grid_config_rec % auxinput11_outname         = model_config_rec % auxinput11_outname 
 grid_config_rec % auxinput1_inname           = model_config_rec % auxinput1_inname 
 grid_config_rec % auxinput2_inname           = model_config_rec % auxinput2_inname 
 grid_config_rec % auxinput3_inname           = model_config_rec % auxinput3_inname 
 grid_config_rec % auxinput4_inname           = model_config_rec % auxinput4_inname 
 grid_config_rec % auxinput5_inname           = model_config_rec % auxinput5_inname 
 grid_config_rec % auxinput6_inname           = model_config_rec % auxinput6_inname 
 grid_config_rec % auxinput7_inname           = model_config_rec % auxinput7_inname 
 grid_config_rec % auxinput8_inname           = model_config_rec % auxinput8_inname 
 grid_config_rec % sgfdda_inname              = model_config_rec % sgfdda_inname 
 grid_config_rec % gfdda_inname               = model_config_rec % gfdda_inname 
 grid_config_rec % auxinput11_inname          = model_config_rec % auxinput11_inname 
 grid_config_rec % history_interval_mo        = model_config_rec % history_interval_mo (id_id)
 grid_config_rec % history_interval_d         = model_config_rec % history_interval_d (id_id)
 grid_config_rec % history_interval_h         = model_config_rec % history_interval_h (id_id)
 grid_config_rec % history_interval_m         = model_config_rec % history_interval_m (id_id)
 grid_config_rec % history_interval_s         = model_config_rec % history_interval_s (id_id)
 grid_config_rec % inputout_interval_mo       = model_config_rec % inputout_interval_mo (id_id)
 grid_config_rec % inputout_interval_d        = model_config_rec % inputout_interval_d (id_id)
 grid_config_rec % inputout_interval_h        = model_config_rec % inputout_interval_h (id_id)
 grid_config_rec % inputout_interval_m        = model_config_rec % inputout_interval_m (id_id)
 grid_config_rec % inputout_interval_s        = model_config_rec % inputout_interval_s (id_id)
 grid_config_rec % inputout_interval          = model_config_rec % inputout_interval (id_id)
 grid_config_rec % auxhist1_interval_mo       = model_config_rec % auxhist1_interval_mo (id_id)
 grid_config_rec % auxhist1_interval_d        = model_config_rec % auxhist1_interval_d (id_id)
 grid_config_rec % auxhist1_interval_h        = model_config_rec % auxhist1_interval_h (id_id)
 grid_config_rec % auxhist1_interval_m        = model_config_rec % auxhist1_interval_m (id_id)
 grid_config_rec % auxhist1_interval_s        = model_config_rec % auxhist1_interval_s (id_id)
 grid_config_rec % auxhist1_interval          = model_config_rec % auxhist1_interval (id_id)
 grid_config_rec % auxhist2_interval_mo       = model_config_rec % auxhist2_interval_mo (id_id)
 grid_config_rec % auxhist2_interval_d        = model_config_rec % auxhist2_interval_d (id_id)
 grid_config_rec % auxhist2_interval_h        = model_config_rec % auxhist2_interval_h (id_id)
 grid_config_rec % auxhist2_interval_m        = model_config_rec % auxhist2_interval_m (id_id)
 grid_config_rec % auxhist2_interval_s        = model_config_rec % auxhist2_interval_s (id_id)
 grid_config_rec % auxhist2_interval          = model_config_rec % auxhist2_interval (id_id)
 grid_config_rec % auxhist3_interval_mo       = model_config_rec % auxhist3_interval_mo (id_id)
 grid_config_rec % auxhist3_interval_d        = model_config_rec % auxhist3_interval_d (id_id)
 grid_config_rec % auxhist3_interval_h        = model_config_rec % auxhist3_interval_h (id_id)
 grid_config_rec % auxhist3_interval_m        = model_config_rec % auxhist3_interval_m (id_id)
 grid_config_rec % auxhist3_interval_s        = model_config_rec % auxhist3_interval_s (id_id)
 grid_config_rec % auxhist3_interval          = model_config_rec % auxhist3_interval (id_id)
 grid_config_rec % auxhist4_interval_mo       = model_config_rec % auxhist4_interval_mo (id_id)
 grid_config_rec % auxhist4_interval_d        = model_config_rec % auxhist4_interval_d (id_id)
 grid_config_rec % auxhist4_interval_h        = model_config_rec % auxhist4_interval_h (id_id)
 grid_config_rec % auxhist4_interval_m        = model_config_rec % auxhist4_interval_m (id_id)
 grid_config_rec % auxhist4_interval_s        = model_config_rec % auxhist4_interval_s (id_id)
 grid_config_rec % auxhist4_interval          = model_config_rec % auxhist4_interval (id_id)
 grid_config_rec % auxhist5_interval_mo       = model_config_rec % auxhist5_interval_mo (id_id)
 grid_config_rec % auxhist5_interval_d        = model_config_rec % auxhist5_interval_d (id_id)
 grid_config_rec % auxhist5_interval_h        = model_config_rec % auxhist5_interval_h (id_id)
 grid_config_rec % auxhist5_interval_m        = model_config_rec % auxhist5_interval_m (id_id)
 grid_config_rec % auxhist5_interval_s        = model_config_rec % auxhist5_interval_s (id_id)
 grid_config_rec % auxhist5_interval          = model_config_rec % auxhist5_interval (id_id)
 grid_config_rec % auxhist6_interval_mo       = model_config_rec % auxhist6_interval_mo (id_id)
 grid_config_rec % auxhist6_interval_d        = model_config_rec % auxhist6_interval_d (id_id)
 grid_config_rec % auxhist6_interval_h        = model_config_rec % auxhist6_interval_h (id_id)
 grid_config_rec % auxhist6_interval_m        = model_config_rec % auxhist6_interval_m (id_id)
 grid_config_rec % auxhist6_interval_s        = model_config_rec % auxhist6_interval_s (id_id)
 grid_config_rec % auxhist6_interval          = model_config_rec % auxhist6_interval (id_id)
 grid_config_rec % auxhist7_interval_mo       = model_config_rec % auxhist7_interval_mo (id_id)
 grid_config_rec % auxhist7_interval_d        = model_config_rec % auxhist7_interval_d (id_id)
 grid_config_rec % auxhist7_interval_h        = model_config_rec % auxhist7_interval_h (id_id)
 grid_config_rec % auxhist7_interval_m        = model_config_rec % auxhist7_interval_m (id_id)
 grid_config_rec % auxhist7_interval_s        = model_config_rec % auxhist7_interval_s (id_id)
 grid_config_rec % auxhist7_interval          = model_config_rec % auxhist7_interval (id_id)
 grid_config_rec % auxhist8_interval_mo       = model_config_rec % auxhist8_interval_mo (id_id)
 grid_config_rec % auxhist8_interval_d        = model_config_rec % auxhist8_interval_d (id_id)
 grid_config_rec % auxhist8_interval_h        = model_config_rec % auxhist8_interval_h (id_id)
 grid_config_rec % auxhist8_interval_m        = model_config_rec % auxhist8_interval_m (id_id)
 grid_config_rec % auxhist8_interval_s        = model_config_rec % auxhist8_interval_s (id_id)
 grid_config_rec % auxhist8_interval          = model_config_rec % auxhist8_interval (id_id)
 grid_config_rec % auxhist9_interval_mo       = model_config_rec % auxhist9_interval_mo (id_id)
 grid_config_rec % auxhist9_interval_d        = model_config_rec % auxhist9_interval_d (id_id)
 grid_config_rec % auxhist9_interval_h        = model_config_rec % auxhist9_interval_h (id_id)
 grid_config_rec % auxhist9_interval_m        = model_config_rec % auxhist9_interval_m (id_id)
 grid_config_rec % auxhist9_interval_s        = model_config_rec % auxhist9_interval_s (id_id)
 grid_config_rec % auxhist9_interval          = model_config_rec % auxhist9_interval (id_id)
 grid_config_rec % auxhist10_interval_mo      = model_config_rec % auxhist10_interval_mo (id_id)
 grid_config_rec % auxhist10_interval_d       = model_config_rec % auxhist10_interval_d (id_id)
 grid_config_rec % auxhist10_interval_h       = model_config_rec % auxhist10_interval_h (id_id)
 grid_config_rec % auxhist10_interval_m       = model_config_rec % auxhist10_interval_m (id_id)
 grid_config_rec % auxhist10_interval_s       = model_config_rec % auxhist10_interval_s (id_id)
 grid_config_rec % auxhist10_interval         = model_config_rec % auxhist10_interval (id_id)
 grid_config_rec % auxhist11_interval_mo      = model_config_rec % auxhist11_interval_mo (id_id)
 grid_config_rec % auxhist11_interval_d       = model_config_rec % auxhist11_interval_d (id_id)
 grid_config_rec % auxhist11_interval_h       = model_config_rec % auxhist11_interval_h (id_id)
 grid_config_rec % auxhist11_interval_m       = model_config_rec % auxhist11_interval_m (id_id)
 grid_config_rec % auxhist11_interval_s       = model_config_rec % auxhist11_interval_s (id_id)
 grid_config_rec % auxhist11_interval         = model_config_rec % auxhist11_interval (id_id)
 grid_config_rec % auxinput1_interval_mo      = model_config_rec % auxinput1_interval_mo (id_id)
 grid_config_rec % auxinput1_interval_d       = model_config_rec % auxinput1_interval_d (id_id)
 grid_config_rec % auxinput1_interval_h       = model_config_rec % auxinput1_interval_h (id_id)
 grid_config_rec % auxinput1_interval_m       = model_config_rec % auxinput1_interval_m (id_id)
 grid_config_rec % auxinput1_interval_s       = model_config_rec % auxinput1_interval_s (id_id)
 grid_config_rec % auxinput1_interval         = model_config_rec % auxinput1_interval (id_id)
 grid_config_rec % auxinput2_interval_mo      = model_config_rec % auxinput2_interval_mo (id_id)
 grid_config_rec % auxinput2_interval_d       = model_config_rec % auxinput2_interval_d (id_id)
 grid_config_rec % auxinput2_interval_h       = model_config_rec % auxinput2_interval_h (id_id)
 grid_config_rec % auxinput2_interval_m       = model_config_rec % auxinput2_interval_m (id_id)
 grid_config_rec % auxinput2_interval_s       = model_config_rec % auxinput2_interval_s (id_id)
 grid_config_rec % auxinput2_interval         = model_config_rec % auxinput2_interval (id_id)
 grid_config_rec % auxinput3_interval_mo      = model_config_rec % auxinput3_interval_mo (id_id)
 grid_config_rec % auxinput3_interval_d       = model_config_rec % auxinput3_interval_d (id_id)
 grid_config_rec % auxinput3_interval_h       = model_config_rec % auxinput3_interval_h (id_id)
 grid_config_rec % auxinput3_interval_m       = model_config_rec % auxinput3_interval_m (id_id)
 grid_config_rec % auxinput3_interval_s       = model_config_rec % auxinput3_interval_s (id_id)
 grid_config_rec % auxinput3_interval         = model_config_rec % auxinput3_interval (id_id)
 grid_config_rec % auxinput4_interval_mo      = model_config_rec % auxinput4_interval_mo (id_id)
 grid_config_rec % auxinput4_interval_d       = model_config_rec % auxinput4_interval_d (id_id)
 grid_config_rec % auxinput4_interval_h       = model_config_rec % auxinput4_interval_h (id_id)
 grid_config_rec % auxinput4_interval_m       = model_config_rec % auxinput4_interval_m (id_id)
 grid_config_rec % auxinput4_interval_s       = model_config_rec % auxinput4_interval_s (id_id)
 grid_config_rec % auxinput4_interval         = model_config_rec % auxinput4_interval (id_id)
 grid_config_rec % auxinput5_interval_mo      = model_config_rec % auxinput5_interval_mo (id_id)
 grid_config_rec % auxinput5_interval_d       = model_config_rec % auxinput5_interval_d (id_id)
 grid_config_rec % auxinput5_interval_h       = model_config_rec % auxinput5_interval_h (id_id)
 grid_config_rec % auxinput5_interval_m       = model_config_rec % auxinput5_interval_m (id_id)
 grid_config_rec % auxinput5_interval_s       = model_config_rec % auxinput5_interval_s (id_id)
 grid_config_rec % auxinput5_interval         = model_config_rec % auxinput5_interval (id_id)
 grid_config_rec % auxinput6_interval_mo      = model_config_rec % auxinput6_interval_mo (id_id)
 grid_config_rec % auxinput6_interval_d       = model_config_rec % auxinput6_interval_d (id_id)
 grid_config_rec % auxinput6_interval_h       = model_config_rec % auxinput6_interval_h (id_id)
 grid_config_rec % auxinput6_interval_m       = model_config_rec % auxinput6_interval_m (id_id)
 grid_config_rec % auxinput6_interval_s       = model_config_rec % auxinput6_interval_s (id_id)
 grid_config_rec % auxinput6_interval         = model_config_rec % auxinput6_interval (id_id)
 grid_config_rec % auxinput7_interval_mo      = model_config_rec % auxinput7_interval_mo (id_id)
 grid_config_rec % auxinput7_interval_d       = model_config_rec % auxinput7_interval_d (id_id)
 grid_config_rec % auxinput7_interval_h       = model_config_rec % auxinput7_interval_h (id_id)
 grid_config_rec % auxinput7_interval_m       = model_config_rec % auxinput7_interval_m (id_id)
 grid_config_rec % auxinput7_interval_s       = model_config_rec % auxinput7_interval_s (id_id)
 grid_config_rec % auxinput7_interval         = model_config_rec % auxinput7_interval (id_id)
 grid_config_rec % auxinput8_interval_mo      = model_config_rec % auxinput8_interval_mo (id_id)
 grid_config_rec % auxinput8_interval_d       = model_config_rec % auxinput8_interval_d (id_id)
 grid_config_rec % auxinput8_interval_h       = model_config_rec % auxinput8_interval_h (id_id)
 grid_config_rec % auxinput8_interval_m       = model_config_rec % auxinput8_interval_m (id_id)
 grid_config_rec % auxinput8_interval_s       = model_config_rec % auxinput8_interval_s (id_id)
 grid_config_rec % auxinput8_interval         = model_config_rec % auxinput8_interval (id_id)
 grid_config_rec % sgfdda_interval_mo         = model_config_rec % sgfdda_interval_mo (id_id)
 grid_config_rec % sgfdda_interval_d          = model_config_rec % sgfdda_interval_d (id_id)
 grid_config_rec % sgfdda_interval_h          = model_config_rec % sgfdda_interval_h (id_id)
 grid_config_rec % sgfdda_interval_m          = model_config_rec % sgfdda_interval_m (id_id)
 grid_config_rec % sgfdda_interval_s          = model_config_rec % sgfdda_interval_s (id_id)
 grid_config_rec % sgfdda_interval            = model_config_rec % sgfdda_interval (id_id)
 grid_config_rec % gfdda_interval_mo          = model_config_rec % gfdda_interval_mo (id_id)
 grid_config_rec % gfdda_interval_d           = model_config_rec % gfdda_interval_d (id_id)
 grid_config_rec % gfdda_interval_h           = model_config_rec % gfdda_interval_h (id_id)
 grid_config_rec % gfdda_interval_m           = model_config_rec % gfdda_interval_m (id_id)
 grid_config_rec % gfdda_interval_s           = model_config_rec % gfdda_interval_s (id_id)
 grid_config_rec % gfdda_interval             = model_config_rec % gfdda_interval (id_id)
 grid_config_rec % auxinput11_interval_mo     = model_config_rec % auxinput11_interval_mo (id_id)
 grid_config_rec % auxinput11_interval_d      = model_config_rec % auxinput11_interval_d (id_id)
 grid_config_rec % auxinput11_interval_h      = model_config_rec % auxinput11_interval_h (id_id)
 grid_config_rec % auxinput11_interval_m      = model_config_rec % auxinput11_interval_m (id_id)
 grid_config_rec % auxinput11_interval_s      = model_config_rec % auxinput11_interval_s (id_id)
 grid_config_rec % auxinput11_interval        = model_config_rec % auxinput11_interval (id_id)
 grid_config_rec % restart_interval_mo        = model_config_rec % restart_interval_mo 
 grid_config_rec % restart_interval_d         = model_config_rec % restart_interval_d 
 grid_config_rec % restart_interval_h         = model_config_rec % restart_interval_h 
 grid_config_rec % restart_interval_m         = model_config_rec % restart_interval_m 
 grid_config_rec % restart_interval_s         = model_config_rec % restart_interval_s 
 grid_config_rec % history_begin_y            = model_config_rec % history_begin_y (id_id)
 grid_config_rec % history_begin_mo           = model_config_rec % history_begin_mo (id_id)
 grid_config_rec % history_begin_d            = model_config_rec % history_begin_d (id_id)
 grid_config_rec % history_begin_h            = model_config_rec % history_begin_h (id_id)
 grid_config_rec % history_begin_m            = model_config_rec % history_begin_m (id_id)
 grid_config_rec % history_begin_s            = model_config_rec % history_begin_s (id_id)
 grid_config_rec % inputout_begin_y           = model_config_rec % inputout_begin_y (id_id)
 grid_config_rec % inputout_begin_mo          = model_config_rec % inputout_begin_mo (id_id)
 grid_config_rec % inputout_begin_d           = model_config_rec % inputout_begin_d (id_id)
 grid_config_rec % inputout_begin_h           = model_config_rec % inputout_begin_h (id_id)
 grid_config_rec % inputout_begin_m           = model_config_rec % inputout_begin_m (id_id)
 grid_config_rec % inputout_begin_s           = model_config_rec % inputout_begin_s (id_id)
 grid_config_rec % auxhist1_begin_y           = model_config_rec % auxhist1_begin_y (id_id)
 grid_config_rec % auxhist1_begin_mo          = model_config_rec % auxhist1_begin_mo (id_id)
 grid_config_rec % auxhist1_begin_d           = model_config_rec % auxhist1_begin_d (id_id)
 grid_config_rec % auxhist1_begin_h           = model_config_rec % auxhist1_begin_h (id_id)
 grid_config_rec % auxhist1_begin_m           = model_config_rec % auxhist1_begin_m (id_id)
 grid_config_rec % auxhist1_begin_s           = model_config_rec % auxhist1_begin_s (id_id)
 grid_config_rec % auxhist2_begin_y           = model_config_rec % auxhist2_begin_y (id_id)
 grid_config_rec % auxhist2_begin_mo          = model_config_rec % auxhist2_begin_mo (id_id)
 grid_config_rec % auxhist2_begin_d           = model_config_rec % auxhist2_begin_d (id_id)
 grid_config_rec % auxhist2_begin_h           = model_config_rec % auxhist2_begin_h (id_id)
 grid_config_rec % auxhist2_begin_m           = model_config_rec % auxhist2_begin_m (id_id)
 grid_config_rec % auxhist2_begin_s           = model_config_rec % auxhist2_begin_s (id_id)
 grid_config_rec % auxhist3_begin_y           = model_config_rec % auxhist3_begin_y (id_id)
 grid_config_rec % auxhist3_begin_mo          = model_config_rec % auxhist3_begin_mo (id_id)
 grid_config_rec % auxhist3_begin_d           = model_config_rec % auxhist3_begin_d (id_id)
 grid_config_rec % auxhist3_begin_h           = model_config_rec % auxhist3_begin_h (id_id)
 grid_config_rec % auxhist3_begin_m           = model_config_rec % auxhist3_begin_m (id_id)
 grid_config_rec % auxhist3_begin_s           = model_config_rec % auxhist3_begin_s (id_id)
 grid_config_rec % auxhist4_begin_y           = model_config_rec % auxhist4_begin_y (id_id)
 grid_config_rec % auxhist4_begin_mo          = model_config_rec % auxhist4_begin_mo (id_id)
 grid_config_rec % auxhist4_begin_d           = model_config_rec % auxhist4_begin_d (id_id)
 grid_config_rec % auxhist4_begin_h           = model_config_rec % auxhist4_begin_h (id_id)
 grid_config_rec % auxhist4_begin_m           = model_config_rec % auxhist4_begin_m (id_id)
 grid_config_rec % auxhist4_begin_s           = model_config_rec % auxhist4_begin_s (id_id)
 grid_config_rec % auxhist5_begin_y           = model_config_rec % auxhist5_begin_y (id_id)
 grid_config_rec % auxhist5_begin_mo          = model_config_rec % auxhist5_begin_mo (id_id)
 grid_config_rec % auxhist5_begin_d           = model_config_rec % auxhist5_begin_d (id_id)
 grid_config_rec % auxhist5_begin_h           = model_config_rec % auxhist5_begin_h (id_id)
 grid_config_rec % auxhist5_begin_m           = model_config_rec % auxhist5_begin_m (id_id)
 grid_config_rec % auxhist5_begin_s           = model_config_rec % auxhist5_begin_s (id_id)
 grid_config_rec % auxhist6_begin_y           = model_config_rec % auxhist6_begin_y (id_id)
 grid_config_rec % auxhist6_begin_mo          = model_config_rec % auxhist6_begin_mo (id_id)
 grid_config_rec % auxhist6_begin_d           = model_config_rec % auxhist6_begin_d (id_id)
 grid_config_rec % auxhist6_begin_h           = model_config_rec % auxhist6_begin_h (id_id)
 grid_config_rec % auxhist6_begin_m           = model_config_rec % auxhist6_begin_m (id_id)
 grid_config_rec % auxhist6_begin_s           = model_config_rec % auxhist6_begin_s (id_id)
 grid_config_rec % auxhist7_begin_y           = model_config_rec % auxhist7_begin_y (id_id)
 grid_config_rec % auxhist7_begin_mo          = model_config_rec % auxhist7_begin_mo (id_id)
 grid_config_rec % auxhist7_begin_d           = model_config_rec % auxhist7_begin_d (id_id)
 grid_config_rec % auxhist7_begin_h           = model_config_rec % auxhist7_begin_h (id_id)
 grid_config_rec % auxhist7_begin_m           = model_config_rec % auxhist7_begin_m (id_id)
 grid_config_rec % auxhist7_begin_s           = model_config_rec % auxhist7_begin_s (id_id)
 grid_config_rec % auxhist8_begin_y           = model_config_rec % auxhist8_begin_y (id_id)
 grid_config_rec % auxhist8_begin_mo          = model_config_rec % auxhist8_begin_mo (id_id)
 grid_config_rec % auxhist8_begin_d           = model_config_rec % auxhist8_begin_d (id_id)
 grid_config_rec % auxhist8_begin_h           = model_config_rec % auxhist8_begin_h (id_id)
 grid_config_rec % auxhist8_begin_m           = model_config_rec % auxhist8_begin_m (id_id)
 grid_config_rec % auxhist8_begin_s           = model_config_rec % auxhist8_begin_s (id_id)
 grid_config_rec % auxhist9_begin_y           = model_config_rec % auxhist9_begin_y (id_id)
 grid_config_rec % auxhist9_begin_mo          = model_config_rec % auxhist9_begin_mo (id_id)
 grid_config_rec % auxhist9_begin_d           = model_config_rec % auxhist9_begin_d (id_id)
 grid_config_rec % auxhist9_begin_h           = model_config_rec % auxhist9_begin_h (id_id)
 grid_config_rec % auxhist9_begin_m           = model_config_rec % auxhist9_begin_m (id_id)
 grid_config_rec % auxhist9_begin_s           = model_config_rec % auxhist9_begin_s (id_id)
 grid_config_rec % auxhist10_begin_y          = model_config_rec % auxhist10_begin_y (id_id)
 grid_config_rec % auxhist10_begin_mo         = model_config_rec % auxhist10_begin_mo (id_id)
 grid_config_rec % auxhist10_begin_d          = model_config_rec % auxhist10_begin_d (id_id)
 grid_config_rec % auxhist10_begin_h          = model_config_rec % auxhist10_begin_h (id_id)
 grid_config_rec % auxhist10_begin_m          = model_config_rec % auxhist10_begin_m (id_id)
 grid_config_rec % auxhist10_begin_s          = model_config_rec % auxhist10_begin_s (id_id)
 grid_config_rec % auxhist11_begin_y          = model_config_rec % auxhist11_begin_y (id_id)
 grid_config_rec % auxhist11_begin_mo         = model_config_rec % auxhist11_begin_mo (id_id)
 grid_config_rec % auxhist11_begin_d          = model_config_rec % auxhist11_begin_d (id_id)
 grid_config_rec % auxhist11_begin_h          = model_config_rec % auxhist11_begin_h (id_id)
 grid_config_rec % auxhist11_begin_m          = model_config_rec % auxhist11_begin_m (id_id)
 grid_config_rec % auxhist11_begin_s          = model_config_rec % auxhist11_begin_s (id_id)
 grid_config_rec % auxinput1_begin_y          = model_config_rec % auxinput1_begin_y (id_id)
 grid_config_rec % auxinput1_begin_mo         = model_config_rec % auxinput1_begin_mo (id_id)
 grid_config_rec % auxinput1_begin_d          = model_config_rec % auxinput1_begin_d (id_id)
 grid_config_rec % auxinput1_begin_h          = model_config_rec % auxinput1_begin_h (id_id)
 grid_config_rec % auxinput1_begin_m          = model_config_rec % auxinput1_begin_m (id_id)
 grid_config_rec % auxinput1_begin_s          = model_config_rec % auxinput1_begin_s (id_id)
 grid_config_rec % auxinput2_begin_y          = model_config_rec % auxinput2_begin_y (id_id)
 grid_config_rec % auxinput2_begin_mo         = model_config_rec % auxinput2_begin_mo (id_id)
 grid_config_rec % auxinput2_begin_d          = model_config_rec % auxinput2_begin_d (id_id)
 grid_config_rec % auxinput2_begin_h          = model_config_rec % auxinput2_begin_h (id_id)
 grid_config_rec % auxinput2_begin_m          = model_config_rec % auxinput2_begin_m (id_id)
 grid_config_rec % auxinput2_begin_s          = model_config_rec % auxinput2_begin_s (id_id)
 grid_config_rec % auxinput3_begin_y          = model_config_rec % auxinput3_begin_y (id_id)
 grid_config_rec % auxinput3_begin_mo         = model_config_rec % auxinput3_begin_mo (id_id)
 grid_config_rec % auxinput3_begin_d          = model_config_rec % auxinput3_begin_d (id_id)
 grid_config_rec % auxinput3_begin_h          = model_config_rec % auxinput3_begin_h (id_id)
 grid_config_rec % auxinput3_begin_m          = model_config_rec % auxinput3_begin_m (id_id)
 grid_config_rec % auxinput3_begin_s          = model_config_rec % auxinput3_begin_s (id_id)
 grid_config_rec % auxinput4_begin_y          = model_config_rec % auxinput4_begin_y (id_id)
 grid_config_rec % auxinput4_begin_mo         = model_config_rec % auxinput4_begin_mo (id_id)
 grid_config_rec % auxinput4_begin_d          = model_config_rec % auxinput4_begin_d (id_id)
 grid_config_rec % auxinput4_begin_h          = model_config_rec % auxinput4_begin_h (id_id)
 grid_config_rec % auxinput4_begin_m          = model_config_rec % auxinput4_begin_m (id_id)
 grid_config_rec % auxinput4_begin_s          = model_config_rec % auxinput4_begin_s (id_id)
 grid_config_rec % auxinput5_begin_y          = model_config_rec % auxinput5_begin_y (id_id)
 grid_config_rec % auxinput5_begin_mo         = model_config_rec % auxinput5_begin_mo (id_id)
 grid_config_rec % auxinput5_begin_d          = model_config_rec % auxinput5_begin_d (id_id)
 grid_config_rec % auxinput5_begin_h          = model_config_rec % auxinput5_begin_h (id_id)
 grid_config_rec % auxinput5_begin_m          = model_config_rec % auxinput5_begin_m (id_id)
 grid_config_rec % auxinput5_begin_s          = model_config_rec % auxinput5_begin_s (id_id)
 grid_config_rec % auxinput6_begin_y          = model_config_rec % auxinput6_begin_y (id_id)
 grid_config_rec % auxinput6_begin_mo         = model_config_rec % auxinput6_begin_mo (id_id)
 grid_config_rec % auxinput6_begin_d          = model_config_rec % auxinput6_begin_d (id_id)
 grid_config_rec % auxinput6_begin_h          = model_config_rec % auxinput6_begin_h (id_id)
 grid_config_rec % auxinput6_begin_m          = model_config_rec % auxinput6_begin_m (id_id)
 grid_config_rec % auxinput6_begin_s          = model_config_rec % auxinput6_begin_s (id_id)
 grid_config_rec % auxinput7_begin_y          = model_config_rec % auxinput7_begin_y (id_id)
 grid_config_rec % auxinput7_begin_mo         = model_config_rec % auxinput7_begin_mo (id_id)
 grid_config_rec % auxinput7_begin_d          = model_config_rec % auxinput7_begin_d (id_id)
 grid_config_rec % auxinput7_begin_h          = model_config_rec % auxinput7_begin_h (id_id)
 grid_config_rec % auxinput7_begin_m          = model_config_rec % auxinput7_begin_m (id_id)
 grid_config_rec % auxinput7_begin_s          = model_config_rec % auxinput7_begin_s (id_id)
 grid_config_rec % auxinput8_begin_y          = model_config_rec % auxinput8_begin_y (id_id)
 grid_config_rec % auxinput8_begin_mo         = model_config_rec % auxinput8_begin_mo (id_id)
 grid_config_rec % auxinput8_begin_d          = model_config_rec % auxinput8_begin_d (id_id)
 grid_config_rec % auxinput8_begin_h          = model_config_rec % auxinput8_begin_h (id_id)
 grid_config_rec % auxinput8_begin_m          = model_config_rec % auxinput8_begin_m (id_id)
 grid_config_rec % auxinput8_begin_s          = model_config_rec % auxinput8_begin_s (id_id)
 grid_config_rec % sgfdda_begin_y             = model_config_rec % sgfdda_begin_y (id_id)
 grid_config_rec % sgfdda_begin_mo            = model_config_rec % sgfdda_begin_mo (id_id)
 grid_config_rec % sgfdda_begin_d             = model_config_rec % sgfdda_begin_d (id_id)
 grid_config_rec % sgfdda_begin_h             = model_config_rec % sgfdda_begin_h (id_id)
 grid_config_rec % sgfdda_begin_m             = model_config_rec % sgfdda_begin_m (id_id)
 grid_config_rec % sgfdda_begin_s             = model_config_rec % sgfdda_begin_s (id_id)
 grid_config_rec % gfdda_begin_y              = model_config_rec % gfdda_begin_y (id_id)
 grid_config_rec % gfdda_begin_mo             = model_config_rec % gfdda_begin_mo (id_id)
 grid_config_rec % gfdda_begin_d              = model_config_rec % gfdda_begin_d (id_id)
 grid_config_rec % gfdda_begin_h              = model_config_rec % gfdda_begin_h (id_id)
 grid_config_rec % gfdda_begin_m              = model_config_rec % gfdda_begin_m (id_id)
 grid_config_rec % gfdda_begin_s              = model_config_rec % gfdda_begin_s (id_id)
 grid_config_rec % auxinput11_begin_y         = model_config_rec % auxinput11_begin_y (id_id)
 grid_config_rec % auxinput11_begin_mo        = model_config_rec % auxinput11_begin_mo (id_id)
 grid_config_rec % auxinput11_begin_d         = model_config_rec % auxinput11_begin_d (id_id)
 grid_config_rec % auxinput11_begin_h         = model_config_rec % auxinput11_begin_h (id_id)
 grid_config_rec % auxinput11_begin_m         = model_config_rec % auxinput11_begin_m (id_id)
 grid_config_rec % auxinput11_begin_s         = model_config_rec % auxinput11_begin_s (id_id)
 grid_config_rec % restart_begin_y            = model_config_rec % restart_begin_y 
 grid_config_rec % restart_begin_mo           = model_config_rec % restart_begin_mo 
 grid_config_rec % restart_begin_d            = model_config_rec % restart_begin_d 
 grid_config_rec % restart_begin_h            = model_config_rec % restart_begin_h 
 grid_config_rec % restart_begin_m            = model_config_rec % restart_begin_m 
 grid_config_rec % restart_begin_s            = model_config_rec % restart_begin_s 
 grid_config_rec % history_end_y              = model_config_rec % history_end_y (id_id)
 grid_config_rec % history_end_mo             = model_config_rec % history_end_mo (id_id)
 grid_config_rec % history_end_d              = model_config_rec % history_end_d (id_id)
 grid_config_rec % history_end_h              = model_config_rec % history_end_h (id_id)
 grid_config_rec % history_end_m              = model_config_rec % history_end_m (id_id)
 grid_config_rec % history_end_s              = model_config_rec % history_end_s (id_id)
 grid_config_rec % inputout_end_y             = model_config_rec % inputout_end_y (id_id)
 grid_config_rec % inputout_end_mo            = model_config_rec % inputout_end_mo (id_id)
 grid_config_rec % inputout_end_d             = model_config_rec % inputout_end_d (id_id)
 grid_config_rec % inputout_end_h             = model_config_rec % inputout_end_h (id_id)
 grid_config_rec % inputout_end_m             = model_config_rec % inputout_end_m (id_id)
 grid_config_rec % inputout_end_s             = model_config_rec % inputout_end_s (id_id)
 grid_config_rec % auxhist1_end_y             = model_config_rec % auxhist1_end_y (id_id)
 grid_config_rec % auxhist1_end_mo            = model_config_rec % auxhist1_end_mo (id_id)
 grid_config_rec % auxhist1_end_d             = model_config_rec % auxhist1_end_d (id_id)
 grid_config_rec % auxhist1_end_h             = model_config_rec % auxhist1_end_h (id_id)
 grid_config_rec % auxhist1_end_m             = model_config_rec % auxhist1_end_m (id_id)
 grid_config_rec % auxhist1_end_s             = model_config_rec % auxhist1_end_s (id_id)
 grid_config_rec % auxhist2_end_y             = model_config_rec % auxhist2_end_y (id_id)
 grid_config_rec % auxhist2_end_mo            = model_config_rec % auxhist2_end_mo (id_id)
 grid_config_rec % auxhist2_end_d             = model_config_rec % auxhist2_end_d (id_id)
 grid_config_rec % auxhist2_end_h             = model_config_rec % auxhist2_end_h (id_id)
 grid_config_rec % auxhist2_end_m             = model_config_rec % auxhist2_end_m (id_id)
 grid_config_rec % auxhist2_end_s             = model_config_rec % auxhist2_end_s (id_id)
 grid_config_rec % auxhist3_end_y             = model_config_rec % auxhist3_end_y (id_id)
 grid_config_rec % auxhist3_end_mo            = model_config_rec % auxhist3_end_mo (id_id)
 grid_config_rec % auxhist3_end_d             = model_config_rec % auxhist3_end_d (id_id)
 grid_config_rec % auxhist3_end_h             = model_config_rec % auxhist3_end_h (id_id)
 grid_config_rec % auxhist3_end_m             = model_config_rec % auxhist3_end_m (id_id)
 grid_config_rec % auxhist3_end_s             = model_config_rec % auxhist3_end_s (id_id)
 grid_config_rec % auxhist4_end_y             = model_config_rec % auxhist4_end_y (id_id)
 grid_config_rec % auxhist4_end_mo            = model_config_rec % auxhist4_end_mo (id_id)
 grid_config_rec % auxhist4_end_d             = model_config_rec % auxhist4_end_d (id_id)
 grid_config_rec % auxhist4_end_h             = model_config_rec % auxhist4_end_h (id_id)
 grid_config_rec % auxhist4_end_m             = model_config_rec % auxhist4_end_m (id_id)
 grid_config_rec % auxhist4_end_s             = model_config_rec % auxhist4_end_s (id_id)
 grid_config_rec % auxhist5_end_y             = model_config_rec % auxhist5_end_y (id_id)
 grid_config_rec % auxhist5_end_mo            = model_config_rec % auxhist5_end_mo (id_id)
 grid_config_rec % auxhist5_end_d             = model_config_rec % auxhist5_end_d (id_id)
 grid_config_rec % auxhist5_end_h             = model_config_rec % auxhist5_end_h (id_id)
 grid_config_rec % auxhist5_end_m             = model_config_rec % auxhist5_end_m (id_id)
 grid_config_rec % auxhist5_end_s             = model_config_rec % auxhist5_end_s (id_id)
 grid_config_rec % auxhist6_end_y             = model_config_rec % auxhist6_end_y (id_id)
 grid_config_rec % auxhist6_end_mo            = model_config_rec % auxhist6_end_mo (id_id)
 grid_config_rec % auxhist6_end_d             = model_config_rec % auxhist6_end_d (id_id)
 grid_config_rec % auxhist6_end_h             = model_config_rec % auxhist6_end_h (id_id)
 grid_config_rec % auxhist6_end_m             = model_config_rec % auxhist6_end_m (id_id)
 grid_config_rec % auxhist6_end_s             = model_config_rec % auxhist6_end_s (id_id)
 grid_config_rec % auxhist7_end_y             = model_config_rec % auxhist7_end_y (id_id)
 grid_config_rec % auxhist7_end_mo            = model_config_rec % auxhist7_end_mo (id_id)
 grid_config_rec % auxhist7_end_d             = model_config_rec % auxhist7_end_d (id_id)
 grid_config_rec % auxhist7_end_h             = model_config_rec % auxhist7_end_h (id_id)
 grid_config_rec % auxhist7_end_m             = model_config_rec % auxhist7_end_m (id_id)
 grid_config_rec % auxhist7_end_s             = model_config_rec % auxhist7_end_s (id_id)
 grid_config_rec % auxhist8_end_y             = model_config_rec % auxhist8_end_y (id_id)
 grid_config_rec % auxhist8_end_mo            = model_config_rec % auxhist8_end_mo (id_id)
 grid_config_rec % auxhist8_end_d             = model_config_rec % auxhist8_end_d (id_id)
 grid_config_rec % auxhist8_end_h             = model_config_rec % auxhist8_end_h (id_id)
 grid_config_rec % auxhist8_end_m             = model_config_rec % auxhist8_end_m (id_id)
 grid_config_rec % auxhist8_end_s             = model_config_rec % auxhist8_end_s (id_id)
 grid_config_rec % auxhist9_end_y             = model_config_rec % auxhist9_end_y (id_id)
 grid_config_rec % auxhist9_end_mo            = model_config_rec % auxhist9_end_mo (id_id)
 grid_config_rec % auxhist9_end_d             = model_config_rec % auxhist9_end_d (id_id)
 grid_config_rec % auxhist9_end_h             = model_config_rec % auxhist9_end_h (id_id)
 grid_config_rec % auxhist9_end_m             = model_config_rec % auxhist9_end_m (id_id)
 grid_config_rec % auxhist9_end_s             = model_config_rec % auxhist9_end_s (id_id)
 grid_config_rec % auxhist10_end_y            = model_config_rec % auxhist10_end_y (id_id)
 grid_config_rec % auxhist10_end_mo           = model_config_rec % auxhist10_end_mo (id_id)
 grid_config_rec % auxhist10_end_d            = model_config_rec % auxhist10_end_d (id_id)
 grid_config_rec % auxhist10_end_h            = model_config_rec % auxhist10_end_h (id_id)
 grid_config_rec % auxhist10_end_m            = model_config_rec % auxhist10_end_m (id_id)
 grid_config_rec % auxhist10_end_s            = model_config_rec % auxhist10_end_s (id_id)
 grid_config_rec % auxhist11_end_y            = model_config_rec % auxhist11_end_y (id_id)
 grid_config_rec % auxhist11_end_mo           = model_config_rec % auxhist11_end_mo (id_id)
 grid_config_rec % auxhist11_end_d            = model_config_rec % auxhist11_end_d (id_id)
 grid_config_rec % auxhist11_end_h            = model_config_rec % auxhist11_end_h (id_id)
 grid_config_rec % auxhist11_end_m            = model_config_rec % auxhist11_end_m (id_id)
 grid_config_rec % auxhist11_end_s            = model_config_rec % auxhist11_end_s (id_id)
 grid_config_rec % auxinput1_end_y            = model_config_rec % auxinput1_end_y (id_id)
 grid_config_rec % auxinput1_end_mo           = model_config_rec % auxinput1_end_mo (id_id)
 grid_config_rec % auxinput1_end_d            = model_config_rec % auxinput1_end_d (id_id)
 grid_config_rec % auxinput1_end_h            = model_config_rec % auxinput1_end_h (id_id)
 grid_config_rec % auxinput1_end_m            = model_config_rec % auxinput1_end_m (id_id)
 grid_config_rec % auxinput1_end_s            = model_config_rec % auxinput1_end_s (id_id)
 grid_config_rec % auxinput2_end_y            = model_config_rec % auxinput2_end_y (id_id)
 grid_config_rec % auxinput2_end_mo           = model_config_rec % auxinput2_end_mo (id_id)
 grid_config_rec % auxinput2_end_d            = model_config_rec % auxinput2_end_d (id_id)
 grid_config_rec % auxinput2_end_h            = model_config_rec % auxinput2_end_h (id_id)
 grid_config_rec % auxinput2_end_m            = model_config_rec % auxinput2_end_m (id_id)
 grid_config_rec % auxinput2_end_s            = model_config_rec % auxinput2_end_s (id_id)
 grid_config_rec % auxinput3_end_y            = model_config_rec % auxinput3_end_y (id_id)
 grid_config_rec % auxinput3_end_mo           = model_config_rec % auxinput3_end_mo (id_id)
 grid_config_rec % auxinput3_end_d            = model_config_rec % auxinput3_end_d (id_id)
 grid_config_rec % auxinput3_end_h            = model_config_rec % auxinput3_end_h (id_id)
 grid_config_rec % auxinput3_end_m            = model_config_rec % auxinput3_end_m (id_id)
 grid_config_rec % auxinput3_end_s            = model_config_rec % auxinput3_end_s (id_id)
 grid_config_rec % auxinput4_end_y            = model_config_rec % auxinput4_end_y (id_id)
 grid_config_rec % auxinput4_end_mo           = model_config_rec % auxinput4_end_mo (id_id)
 grid_config_rec % auxinput4_end_d            = model_config_rec % auxinput4_end_d (id_id)
 grid_config_rec % auxinput4_end_h            = model_config_rec % auxinput4_end_h (id_id)
 grid_config_rec % auxinput4_end_m            = model_config_rec % auxinput4_end_m (id_id)
 grid_config_rec % auxinput4_end_s            = model_config_rec % auxinput4_end_s (id_id)
 grid_config_rec % auxinput5_end_y            = model_config_rec % auxinput5_end_y (id_id)
 grid_config_rec % auxinput5_end_mo           = model_config_rec % auxinput5_end_mo (id_id)
 grid_config_rec % auxinput5_end_d            = model_config_rec % auxinput5_end_d (id_id)
 grid_config_rec % auxinput5_end_h            = model_config_rec % auxinput5_end_h (id_id)
 grid_config_rec % auxinput5_end_m            = model_config_rec % auxinput5_end_m (id_id)
 grid_config_rec % auxinput5_end_s            = model_config_rec % auxinput5_end_s (id_id)
 grid_config_rec % auxinput6_end_y            = model_config_rec % auxinput6_end_y (id_id)
 grid_config_rec % auxinput6_end_mo           = model_config_rec % auxinput6_end_mo (id_id)
 grid_config_rec % auxinput6_end_d            = model_config_rec % auxinput6_end_d (id_id)
 grid_config_rec % auxinput6_end_h            = model_config_rec % auxinput6_end_h (id_id)
 grid_config_rec % auxinput6_end_m            = model_config_rec % auxinput6_end_m (id_id)
 grid_config_rec % auxinput6_end_s            = model_config_rec % auxinput6_end_s (id_id)
 grid_config_rec % auxinput7_end_y            = model_config_rec % auxinput7_end_y (id_id)
 grid_config_rec % auxinput7_end_mo           = model_config_rec % auxinput7_end_mo (id_id)
 grid_config_rec % auxinput7_end_d            = model_config_rec % auxinput7_end_d (id_id)
 grid_config_rec % auxinput7_end_h            = model_config_rec % auxinput7_end_h (id_id)
 grid_config_rec % auxinput7_end_m            = model_config_rec % auxinput7_end_m (id_id)
 grid_config_rec % auxinput7_end_s            = model_config_rec % auxinput7_end_s (id_id)
 grid_config_rec % auxinput8_end_y            = model_config_rec % auxinput8_end_y (id_id)
 grid_config_rec % auxinput8_end_mo           = model_config_rec % auxinput8_end_mo (id_id)
 grid_config_rec % auxinput8_end_d            = model_config_rec % auxinput8_end_d (id_id)
 grid_config_rec % auxinput8_end_h            = model_config_rec % auxinput8_end_h (id_id)
 grid_config_rec % auxinput8_end_m            = model_config_rec % auxinput8_end_m (id_id)
 grid_config_rec % auxinput8_end_s            = model_config_rec % auxinput8_end_s (id_id)
 grid_config_rec % sgfdda_end_y               = model_config_rec % sgfdda_end_y (id_id)
 grid_config_rec % sgfdda_end_mo              = model_config_rec % sgfdda_end_mo (id_id)
 grid_config_rec % sgfdda_end_d               = model_config_rec % sgfdda_end_d (id_id)
 grid_config_rec % sgfdda_end_h               = model_config_rec % sgfdda_end_h (id_id)
 grid_config_rec % sgfdda_end_m               = model_config_rec % sgfdda_end_m (id_id)
 grid_config_rec % sgfdda_end_s               = model_config_rec % sgfdda_end_s (id_id)
 grid_config_rec % gfdda_end_y                = model_config_rec % gfdda_end_y (id_id)
 grid_config_rec % gfdda_end_mo               = model_config_rec % gfdda_end_mo (id_id)
 grid_config_rec % gfdda_end_d                = model_config_rec % gfdda_end_d (id_id)
 grid_config_rec % gfdda_end_h                = model_config_rec % gfdda_end_h (id_id)
 grid_config_rec % gfdda_end_m                = model_config_rec % gfdda_end_m (id_id)
 grid_config_rec % gfdda_end_s                = model_config_rec % gfdda_end_s (id_id)
 grid_config_rec % auxinput11_end_y           = model_config_rec % auxinput11_end_y (id_id)
 grid_config_rec % auxinput11_end_mo          = model_config_rec % auxinput11_end_mo (id_id)
 grid_config_rec % auxinput11_end_d           = model_config_rec % auxinput11_end_d (id_id)
 grid_config_rec % auxinput11_end_h           = model_config_rec % auxinput11_end_h (id_id)
 grid_config_rec % auxinput11_end_m           = model_config_rec % auxinput11_end_m (id_id)
 grid_config_rec % auxinput11_end_s           = model_config_rec % auxinput11_end_s (id_id)
 grid_config_rec % io_form_auxinput1          = model_config_rec % io_form_auxinput1 
 grid_config_rec % io_form_auxinput2          = model_config_rec % io_form_auxinput2 
 grid_config_rec % io_form_auxinput3          = model_config_rec % io_form_auxinput3 
 grid_config_rec % io_form_auxinput4          = model_config_rec % io_form_auxinput4 
 grid_config_rec % io_form_auxinput5          = model_config_rec % io_form_auxinput5 
 grid_config_rec % io_form_auxinput6          = model_config_rec % io_form_auxinput6 
 grid_config_rec % io_form_auxinput7          = model_config_rec % io_form_auxinput7 
 grid_config_rec % io_form_auxinput8          = model_config_rec % io_form_auxinput8 
 grid_config_rec % io_form_sgfdda             = model_config_rec % io_form_sgfdda 
 grid_config_rec % io_form_gfdda              = model_config_rec % io_form_gfdda 
 grid_config_rec % io_form_auxinput11         = model_config_rec % io_form_auxinput11 
 grid_config_rec % io_form_auxhist1           = model_config_rec % io_form_auxhist1 
 grid_config_rec % io_form_auxhist2           = model_config_rec % io_form_auxhist2 
 grid_config_rec % io_form_auxhist3           = model_config_rec % io_form_auxhist3 
 grid_config_rec % io_form_auxhist4           = model_config_rec % io_form_auxhist4 
 grid_config_rec % io_form_auxhist5           = model_config_rec % io_form_auxhist5 
 grid_config_rec % io_form_auxhist6           = model_config_rec % io_form_auxhist6 
 grid_config_rec % io_form_auxhist7           = model_config_rec % io_form_auxhist7 
 grid_config_rec % io_form_auxhist8           = model_config_rec % io_form_auxhist8 
 grid_config_rec % io_form_auxhist9           = model_config_rec % io_form_auxhist9 
 grid_config_rec % io_form_auxhist10          = model_config_rec % io_form_auxhist10 
 grid_config_rec % io_form_auxhist11          = model_config_rec % io_form_auxhist11 
 grid_config_rec % simulation_start_year      = model_config_rec % simulation_start_year 
 grid_config_rec % simulation_start_month     = model_config_rec % simulation_start_month 
 grid_config_rec % simulation_start_day       = model_config_rec % simulation_start_day 
 grid_config_rec % simulation_start_hour      = model_config_rec % simulation_start_hour 
 grid_config_rec % simulation_start_minute    = model_config_rec % simulation_start_minute 
 grid_config_rec % simulation_start_second    = model_config_rec % simulation_start_second 
 grid_config_rec % reset_simulation_start     = model_config_rec % reset_simulation_start 
 grid_config_rec % sr_x                       = model_config_rec % sr_x (id_id)
 grid_config_rec % sr_y                       = model_config_rec % sr_y (id_id)
 grid_config_rec % julyr                      = model_config_rec % julyr (id_id)
 grid_config_rec % julday                     = model_config_rec % julday (id_id)
 grid_config_rec % gmt                        = model_config_rec % gmt (id_id)
 grid_config_rec % input_inname               = model_config_rec % input_inname 
 grid_config_rec % input_outname              = model_config_rec % input_outname 
 grid_config_rec % bdy_inname                 = model_config_rec % bdy_inname 
 grid_config_rec % bdy_outname                = model_config_rec % bdy_outname 
 grid_config_rec % rst_inname                 = model_config_rec % rst_inname 
 grid_config_rec % rst_outname                = model_config_rec % rst_outname 
 grid_config_rec % write_input                = model_config_rec % write_input 
 grid_config_rec % write_restart_at_0h        = model_config_rec % write_restart_at_0h 
 grid_config_rec % adjust_output_times        = model_config_rec % adjust_output_times 
 grid_config_rec % adjust_input_times         = model_config_rec % adjust_input_times 
 grid_config_rec % tstart                     = model_config_rec % tstart (id_id)
 grid_config_rec % nocolons                   = model_config_rec % nocolons 
 grid_config_rec % cycling                    = model_config_rec % cycling 
 grid_config_rec % dfi_opt                    = model_config_rec % dfi_opt 
 grid_config_rec % dfi_nfilter                = model_config_rec % dfi_nfilter 
 grid_config_rec % dfi_write_filtered_input   = model_config_rec % dfi_write_filtered_input 
 grid_config_rec % dfi_write_dfi_history      = model_config_rec % dfi_write_dfi_history 
 grid_config_rec % dfi_cutoff_seconds         = model_config_rec % dfi_cutoff_seconds 
 grid_config_rec % dfi_time_dim               = model_config_rec % dfi_time_dim 
 grid_config_rec % dfi_fwdstop_year           = model_config_rec % dfi_fwdstop_year 
 grid_config_rec % dfi_fwdstop_month          = model_config_rec % dfi_fwdstop_month 
 grid_config_rec % dfi_fwdstop_day            = model_config_rec % dfi_fwdstop_day 
 grid_config_rec % dfi_fwdstop_hour           = model_config_rec % dfi_fwdstop_hour 
 grid_config_rec % dfi_fwdstop_minute         = model_config_rec % dfi_fwdstop_minute 
 grid_config_rec % dfi_fwdstop_second         = model_config_rec % dfi_fwdstop_second 
 grid_config_rec % dfi_bckstop_year           = model_config_rec % dfi_bckstop_year 
 grid_config_rec % dfi_bckstop_month          = model_config_rec % dfi_bckstop_month 
 grid_config_rec % dfi_bckstop_day            = model_config_rec % dfi_bckstop_day 
 grid_config_rec % dfi_bckstop_hour           = model_config_rec % dfi_bckstop_hour 
 grid_config_rec % dfi_bckstop_minute         = model_config_rec % dfi_bckstop_minute 
 grid_config_rec % dfi_bckstop_second         = model_config_rec % dfi_bckstop_second 
 grid_config_rec % time_step                  = model_config_rec % time_step 
 grid_config_rec % time_step_fract_num        = model_config_rec % time_step_fract_num 
 grid_config_rec % time_step_fract_den        = model_config_rec % time_step_fract_den 
 grid_config_rec % max_dom                    = model_config_rec % max_dom 
 grid_config_rec % s_we                       = model_config_rec % s_we (id_id)
 grid_config_rec % e_we                       = model_config_rec % e_we (id_id)
 grid_config_rec % s_sn                       = model_config_rec % s_sn (id_id)
 grid_config_rec % e_sn                       = model_config_rec % e_sn (id_id)
 grid_config_rec % s_vert                     = model_config_rec % s_vert (id_id)
 grid_config_rec % e_vert                     = model_config_rec % e_vert (id_id)
 grid_config_rec % dx                         = model_config_rec % dx (id_id)
 grid_config_rec % dy                         = model_config_rec % dy (id_id)
 grid_config_rec % grid_id                    = model_config_rec % grid_id (id_id)
 grid_config_rec % grid_allowed               = model_config_rec % grid_allowed (id_id)
 grid_config_rec % parent_id                  = model_config_rec % parent_id (id_id)
 grid_config_rec % i_parent_start             = model_config_rec % i_parent_start (id_id)
 grid_config_rec % j_parent_start             = model_config_rec % j_parent_start (id_id)
 grid_config_rec % parent_grid_ratio          = model_config_rec % parent_grid_ratio (id_id)
 grid_config_rec % parent_time_step_ratio     = model_config_rec % parent_time_step_ratio (id_id)
 grid_config_rec % feedback                   = model_config_rec % feedback 
 grid_config_rec % smooth_option              = model_config_rec % smooth_option 
 grid_config_rec % ztop                       = model_config_rec % ztop (id_id)
 grid_config_rec % moad_grid_ratio            = model_config_rec % moad_grid_ratio (id_id)
 grid_config_rec % moad_time_step_ratio       = model_config_rec % moad_time_step_ratio (id_id)
 grid_config_rec % shw                        = model_config_rec % shw (id_id)
 grid_config_rec % tile_sz_x                  = model_config_rec % tile_sz_x 
 grid_config_rec % tile_sz_y                  = model_config_rec % tile_sz_y 
 grid_config_rec % numtiles                   = model_config_rec % numtiles 
 grid_config_rec % nproc_x                    = model_config_rec % nproc_x 
 grid_config_rec % nproc_y                    = model_config_rec % nproc_y 
 grid_config_rec % irand                      = model_config_rec % irand 
 grid_config_rec % dt                         = model_config_rec % dt (id_id)
 grid_config_rec % ts_buf_size                = model_config_rec % ts_buf_size 
 grid_config_rec % max_ts_locs                = model_config_rec % max_ts_locs 
 grid_config_rec % num_moves                  = model_config_rec % num_moves 
 grid_config_rec % move_id                    = model_config_rec % move_id (id_id)
 grid_config_rec % move_interval              = model_config_rec % move_interval (id_id)
 grid_config_rec % move_cd_x                  = model_config_rec % move_cd_x (id_id)
 grid_config_rec % move_cd_y                  = model_config_rec % move_cd_y (id_id)
 grid_config_rec % swap_x                     = model_config_rec % swap_x (id_id)
 grid_config_rec % swap_y                     = model_config_rec % swap_y (id_id)
 grid_config_rec % cycle_x                    = model_config_rec % cycle_x (id_id)
 grid_config_rec % cycle_y                    = model_config_rec % cycle_y (id_id)
 grid_config_rec % reorder_mesh               = model_config_rec % reorder_mesh 
 grid_config_rec % perturb_input              = model_config_rec % perturb_input 
 grid_config_rec % eta_levels                 = model_config_rec % eta_levels (id_id)
 grid_config_rec % ptsgm                      = model_config_rec % ptsgm 
 grid_config_rec % num_metgrid_levels         = model_config_rec % num_metgrid_levels 
 grid_config_rec % p_top_requested            = model_config_rec % p_top_requested 
 grid_config_rec % mp_physics                 = model_config_rec % mp_physics (id_id)
 grid_config_rec % ra_lw_physics              = model_config_rec % ra_lw_physics (id_id)
 grid_config_rec % ra_sw_physics              = model_config_rec % ra_sw_physics (id_id)
 grid_config_rec % radt                       = model_config_rec % radt (id_id)
 grid_config_rec % sf_sfclay_physics          = model_config_rec % sf_sfclay_physics (id_id)
 grid_config_rec % sf_surface_physics         = model_config_rec % sf_surface_physics (id_id)
 grid_config_rec % bl_pbl_physics             = model_config_rec % bl_pbl_physics (id_id)
 grid_config_rec % sf_urban_physics           = model_config_rec % sf_urban_physics (id_id)
 grid_config_rec % bldt                       = model_config_rec % bldt (id_id)
 grid_config_rec % cu_physics                 = model_config_rec % cu_physics (id_id)
 grid_config_rec % cudt                       = model_config_rec % cudt (id_id)
 grid_config_rec % gsmdt                      = model_config_rec % gsmdt (id_id)
 grid_config_rec % isfflx                     = model_config_rec % isfflx 
 grid_config_rec % ifsnow                     = model_config_rec % ifsnow 
 grid_config_rec % icloud                     = model_config_rec % icloud 
 grid_config_rec % swrad_scat                 = model_config_rec % swrad_scat 
 grid_config_rec % surface_input_source       = model_config_rec % surface_input_source 
 grid_config_rec % num_soil_layers            = model_config_rec % num_soil_layers 
 grid_config_rec % num_urban_layers           = model_config_rec % num_urban_layers 
 grid_config_rec % maxiens                    = model_config_rec % maxiens 
 grid_config_rec % maxens                     = model_config_rec % maxens 
 grid_config_rec % maxens2                    = model_config_rec % maxens2 
 grid_config_rec % maxens3                    = model_config_rec % maxens3 
 grid_config_rec % ensdim                     = model_config_rec % ensdim 
 grid_config_rec % chem_opt                   = model_config_rec % chem_opt (id_id)
 grid_config_rec % num_land_cat               = model_config_rec % num_land_cat 
 grid_config_rec % num_soil_cat               = model_config_rec % num_soil_cat 
 grid_config_rec % mp_zero_out                = model_config_rec % mp_zero_out 
 grid_config_rec % mp_zero_out_thresh         = model_config_rec % mp_zero_out_thresh 
 grid_config_rec % seaice_threshold           = model_config_rec % seaice_threshold 
 grid_config_rec % fractional_seaice          = model_config_rec % fractional_seaice 
 grid_config_rec % sst_update                 = model_config_rec % sst_update 
 grid_config_rec % usemonalb                  = model_config_rec % usemonalb 
 grid_config_rec % rdmaxalb                   = model_config_rec % rdmaxalb 
 grid_config_rec % rdlai2d                    = model_config_rec % rdlai2d 
 grid_config_rec % gwd_opt                    = model_config_rec % gwd_opt (id_id)
 grid_config_rec % idtad                      = model_config_rec % idtad (id_id)
 grid_config_rec % nsoil                      = model_config_rec % nsoil (id_id)
 grid_config_rec % nphs                       = model_config_rec % nphs (id_id)
 grid_config_rec % ncnvc                      = model_config_rec % ncnvc (id_id)
 grid_config_rec % nrads                      = model_config_rec % nrads (id_id)
 grid_config_rec % nradl                      = model_config_rec % nradl (id_id)
 grid_config_rec % tprec                      = model_config_rec % tprec (id_id)
 grid_config_rec % theat                      = model_config_rec % theat (id_id)
 grid_config_rec % tclod                      = model_config_rec % tclod (id_id)
 grid_config_rec % trdsw                      = model_config_rec % trdsw (id_id)
 grid_config_rec % trdlw                      = model_config_rec % trdlw (id_id)
 grid_config_rec % tsrfc                      = model_config_rec % tsrfc (id_id)
 grid_config_rec % pcpflg                     = model_config_rec % pcpflg (id_id)
 grid_config_rec % sigma                      = model_config_rec % sigma (id_id)
 grid_config_rec % sfenth                     = model_config_rec % sfenth (id_id)
 grid_config_rec % co2tf                      = model_config_rec % co2tf 
 grid_config_rec % ra_call_offset             = model_config_rec % ra_call_offset 
 grid_config_rec % cam_abs_freq_s             = model_config_rec % cam_abs_freq_s 
 grid_config_rec % levsiz                     = model_config_rec % levsiz 
 grid_config_rec % paerlev                    = model_config_rec % paerlev 
 grid_config_rec % cam_abs_dim1               = model_config_rec % cam_abs_dim1 
 grid_config_rec % cam_abs_dim2               = model_config_rec % cam_abs_dim2 
 grid_config_rec % cu_rad_feedback            = model_config_rec % cu_rad_feedback (id_id)
 grid_config_rec % dyn_opt                    = model_config_rec % dyn_opt 
 grid_config_rec % rk_ord                     = model_config_rec % rk_ord 
 grid_config_rec % w_damping                  = model_config_rec % w_damping 
 grid_config_rec % diff_opt                   = model_config_rec % diff_opt 
 grid_config_rec % km_opt                     = model_config_rec % km_opt 
 grid_config_rec % damp_opt                   = model_config_rec % damp_opt 
 grid_config_rec % zdamp                      = model_config_rec % zdamp (id_id)
 grid_config_rec % base_pres                  = model_config_rec % base_pres 
 grid_config_rec % base_temp                  = model_config_rec % base_temp 
 grid_config_rec % base_lapse                 = model_config_rec % base_lapse 
 grid_config_rec % iso_temp                   = model_config_rec % iso_temp 
 grid_config_rec % dampcoef                   = model_config_rec % dampcoef (id_id)
 grid_config_rec % khdif                      = model_config_rec % khdif (id_id)
 grid_config_rec % kvdif                      = model_config_rec % kvdif (id_id)
 grid_config_rec % c_s                        = model_config_rec % c_s (id_id)
 grid_config_rec % c_k                        = model_config_rec % c_k (id_id)
 grid_config_rec % smdiv                      = model_config_rec % smdiv (id_id)
 grid_config_rec % emdiv                      = model_config_rec % emdiv (id_id)
 grid_config_rec % epssm                      = model_config_rec % epssm (id_id)
 grid_config_rec % non_hydrostatic            = model_config_rec % non_hydrostatic (id_id)
 grid_config_rec % time_step_sound            = model_config_rec % time_step_sound (id_id)
 grid_config_rec % h_mom_adv_order            = model_config_rec % h_mom_adv_order (id_id)
 grid_config_rec % v_mom_adv_order            = model_config_rec % v_mom_adv_order (id_id)
 grid_config_rec % h_sca_adv_order            = model_config_rec % h_sca_adv_order (id_id)
 grid_config_rec % v_sca_adv_order            = model_config_rec % v_sca_adv_order (id_id)
 grid_config_rec % top_radiation              = model_config_rec % top_radiation (id_id)
 grid_config_rec % tke_upper_bound            = model_config_rec % tke_upper_bound (id_id)
 grid_config_rec % tke_drag_coefficient       = model_config_rec % tke_drag_coefficient (id_id)
 grid_config_rec % tke_heat_flux              = model_config_rec % tke_heat_flux (id_id)
 grid_config_rec % pert_coriolis              = model_config_rec % pert_coriolis (id_id)
 grid_config_rec % euler_adv                  = model_config_rec % euler_adv 
 grid_config_rec % idtadt                     = model_config_rec % idtadt 
 grid_config_rec % idtadc                     = model_config_rec % idtadc 
 grid_config_rec % boundary_flux              = model_config_rec % boundary_flux 
 grid_config_rec % spec_bdy_width             = model_config_rec % spec_bdy_width 
 grid_config_rec % spec_zone                  = model_config_rec % spec_zone 
 grid_config_rec % relax_zone                 = model_config_rec % relax_zone 
 grid_config_rec % specified                  = model_config_rec % specified (id_id)
 grid_config_rec % periodic_x                 = model_config_rec % periodic_x (id_id)
 grid_config_rec % symmetric_xs               = model_config_rec % symmetric_xs (id_id)
 grid_config_rec % symmetric_xe               = model_config_rec % symmetric_xe (id_id)
 grid_config_rec % open_xs                    = model_config_rec % open_xs (id_id)
 grid_config_rec % open_xe                    = model_config_rec % open_xe (id_id)
 grid_config_rec % periodic_y                 = model_config_rec % periodic_y (id_id)
 grid_config_rec % symmetric_ys               = model_config_rec % symmetric_ys (id_id)
 grid_config_rec % symmetric_ye               = model_config_rec % symmetric_ye (id_id)
 grid_config_rec % open_ys                    = model_config_rec % open_ys (id_id)
 grid_config_rec % open_ye                    = model_config_rec % open_ye (id_id)
 grid_config_rec % polar                      = model_config_rec % polar (id_id)
 grid_config_rec % nested                     = model_config_rec % nested (id_id)
 grid_config_rec % real_data_init_type        = model_config_rec % real_data_init_type 
 grid_config_rec % background_proc_id         = model_config_rec % background_proc_id 
 grid_config_rec % forecast_proc_id           = model_config_rec % forecast_proc_id 
 grid_config_rec % production_status          = model_config_rec % production_status 
 grid_config_rec % compression                = model_config_rec % compression 
 grid_config_rec % cen_lat                    = model_config_rec % cen_lat (id_id)
 grid_config_rec % cen_lon                    = model_config_rec % cen_lon (id_id)
 grid_config_rec % truelat1                   = model_config_rec % truelat1 (id_id)
 grid_config_rec % truelat2                   = model_config_rec % truelat2 (id_id)
 grid_config_rec % moad_cen_lat               = model_config_rec % moad_cen_lat (id_id)
 grid_config_rec % stand_lon                  = model_config_rec % stand_lon (id_id)
 grid_config_rec % flag_metgrid               = model_config_rec % flag_metgrid 
 grid_config_rec % flag_snow                  = model_config_rec % flag_snow 
 grid_config_rec % flag_psfc                  = model_config_rec % flag_psfc 
 grid_config_rec % flag_sm000010              = model_config_rec % flag_sm000010 
 grid_config_rec % flag_sm010040              = model_config_rec % flag_sm010040 
 grid_config_rec % flag_sm040100              = model_config_rec % flag_sm040100 
 grid_config_rec % flag_sm100200              = model_config_rec % flag_sm100200 
 grid_config_rec % flag_st000010              = model_config_rec % flag_st000010 
 grid_config_rec % flag_st010040              = model_config_rec % flag_st010040 
 grid_config_rec % flag_st040100              = model_config_rec % flag_st040100 
 grid_config_rec % flag_st100200              = model_config_rec % flag_st100200 
 grid_config_rec % flag_slp                   = model_config_rec % flag_slp 
 grid_config_rec % flag_soilhgt               = model_config_rec % flag_soilhgt 
 grid_config_rec % flag_mf_xy                 = model_config_rec % flag_mf_xy 
 grid_config_rec % bdyfrq                     = model_config_rec % bdyfrq (id_id)
 grid_config_rec % mminlu                     = model_config_rec % mminlu (id_id)
 grid_config_rec % iswater                    = model_config_rec % iswater (id_id)
 grid_config_rec % islake                     = model_config_rec % islake (id_id)
 grid_config_rec % isice                      = model_config_rec % isice (id_id)
 grid_config_rec % isurban                    = model_config_rec % isurban (id_id)
 grid_config_rec % isoilwater                 = model_config_rec % isoilwater (id_id)
 grid_config_rec % map_proj                   = model_config_rec % map_proj (id_id)
 grid_config_rec % dfi_stage                  = model_config_rec % dfi_stage 
 grid_config_rec % mp_physics_dfi             = model_config_rec % mp_physics_dfi (id_id)

   END SUBROUTINE model_to_grid_config_rec


   FUNCTION in_use_for_config ( id, vname ) RESULT ( in_use )
     INTEGER, INTENT(IN) :: id
     CHARACTER*(*), INTENT(IN) :: vname
     LOGICAL in_use
     INTEGER uses

     uses = 0
     in_use = .TRUE.







IF(TRIM(vname).EQ.'dfi_pd')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_pint')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_dwdt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_t')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_q')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_u')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_v')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_q2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_cwm')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_rrw')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_stc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_smc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_sh2o')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_snowh')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_canwat')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_nmm_tsk')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_snowc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_pd')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_pint')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_dwdt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_t')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_q')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_u')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_v')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_q2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_cwm')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_rrw')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_stc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_smc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_sh2o')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_snowh')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_canwat')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_nmm_tsk')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_snowc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_pd')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_pint')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_dwdt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_t')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_q')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_u')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_v')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_q2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_cwm')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_rrw')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_stc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_smc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_sh2o')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_snowh')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_canwat')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_nmm_tsk')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_snowc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF


     RETURN
   END FUNCTION





   SUBROUTINE init_module_configure
     USE module_scalar_tables
     IMPLICIT NONE
     CALL init_module_scalar_tables
   END SUBROUTINE init_module_configure

   SUBROUTINE wrf_alt_nml_dynamics (nml_read_unit, nml_name)







     IMPLICIT NONE
     INTEGER, INTENT(IN)       :: nml_read_unit
     CHARACTER*(*), INTENT(IN) :: nml_name
     INTEGER                   :: nml_error







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
character*256 :: auxinput1_inname
character*256 :: auxinput2_inname
character*256 :: auxinput3_inname
character*256 :: auxinput4_inname
character*256 :: auxinput5_inname
character*256 :: auxinput6_inname
character*256 :: auxinput7_inname
character*256 :: auxinput8_inname
character*256 :: sgfdda_inname
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
integer , DIMENSION(max_domains) :: sgfdda_interval_mo
integer , DIMENSION(max_domains) :: sgfdda_interval_d
integer , DIMENSION(max_domains) :: sgfdda_interval_h
integer , DIMENSION(max_domains) :: sgfdda_interval_m
integer , DIMENSION(max_domains) :: sgfdda_interval_s
integer , DIMENSION(max_domains) :: sgfdda_interval
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
integer , DIMENSION(max_domains) :: sgfdda_begin_y
integer , DIMENSION(max_domains) :: sgfdda_begin_mo
integer , DIMENSION(max_domains) :: sgfdda_begin_d
integer , DIMENSION(max_domains) :: sgfdda_begin_h
integer , DIMENSION(max_domains) :: sgfdda_begin_m
integer , DIMENSION(max_domains) :: sgfdda_begin_s
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
integer , DIMENSION(max_domains) :: sgfdda_end_y
integer , DIMENSION(max_domains) :: sgfdda_end_mo
integer , DIMENSION(max_domains) :: sgfdda_end_d
integer , DIMENSION(max_domains) :: sgfdda_end_h
integer , DIMENSION(max_domains) :: sgfdda_end_m
integer , DIMENSION(max_domains) :: sgfdda_end_s
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
integer :: io_form_sgfdda
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
integer :: simulation_start_year
integer :: simulation_start_month
integer :: simulation_start_day
integer :: simulation_start_hour
integer :: simulation_start_minute
integer :: simulation_start_second
logical :: reset_simulation_start
integer , DIMENSION(max_domains) :: sr_x
integer , DIMENSION(max_domains) :: sr_y
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
real , DIMENSION(max_domains) :: tstart
logical :: nocolons
logical :: cycling
integer :: dfi_opt
integer :: dfi_nfilter
logical :: dfi_write_filtered_input
logical :: dfi_write_dfi_history
integer :: dfi_cutoff_seconds
integer :: dfi_time_dim
integer :: dfi_fwdstop_year
integer :: dfi_fwdstop_month
integer :: dfi_fwdstop_day
integer :: dfi_fwdstop_hour
integer :: dfi_fwdstop_minute
integer :: dfi_fwdstop_second
integer :: dfi_bckstop_year
integer :: dfi_bckstop_month
integer :: dfi_bckstop_day
integer :: dfi_bckstop_hour
integer :: dfi_bckstop_minute
integer :: dfi_bckstop_second
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
real , DIMENSION(max_domains) :: dx
real , DIMENSION(max_domains) :: dy
integer , DIMENSION(max_domains) :: grid_id
logical , DIMENSION(max_domains) :: grid_allowed
integer , DIMENSION(max_domains) :: parent_id
integer , DIMENSION(max_domains) :: i_parent_start
integer , DIMENSION(max_domains) :: j_parent_start
integer , DIMENSION(max_domains) :: parent_grid_ratio
integer , DIMENSION(max_domains) :: parent_time_step_ratio
integer :: feedback
integer :: smooth_option
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
integer :: ts_buf_size
integer :: max_ts_locs
integer :: num_moves
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
real :: ptsgm
integer :: num_metgrid_levels
real :: p_top_requested
integer , DIMENSION(max_domains) :: mp_physics
integer , DIMENSION(max_domains) :: ra_lw_physics
integer , DIMENSION(max_domains) :: ra_sw_physics
real , DIMENSION(max_domains) :: radt
integer , DIMENSION(max_domains) :: sf_sfclay_physics
integer , DIMENSION(max_domains) :: sf_surface_physics
integer , DIMENSION(max_domains) :: bl_pbl_physics
integer , DIMENSION(max_domains) :: sf_urban_physics
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
integer :: num_urban_layers
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
integer :: fractional_seaice
integer :: sst_update
logical :: usemonalb
logical :: rdmaxalb
logical :: rdlai2d
integer , DIMENSION(max_domains) :: gwd_opt
integer , DIMENSION(max_domains) :: idtad
integer , DIMENSION(max_domains) :: nsoil
integer , DIMENSION(max_domains) :: nphs
integer , DIMENSION(max_domains) :: ncnvc
integer , DIMENSION(max_domains) :: nrads
integer , DIMENSION(max_domains) :: nradl
real , DIMENSION(max_domains) :: tprec
real , DIMENSION(max_domains) :: theat
real , DIMENSION(max_domains) :: tclod
real , DIMENSION(max_domains) :: trdsw
real , DIMENSION(max_domains) :: trdlw
real , DIMENSION(max_domains) :: tsrfc
logical , DIMENSION(max_domains) :: pcpflg
integer , DIMENSION(max_domains) :: sigma
real , DIMENSION(max_domains) :: sfenth
integer :: co2tf
integer :: ra_call_offset
real :: cam_abs_freq_s
integer :: levsiz
integer :: paerlev
integer :: cam_abs_dim1
integer :: cam_abs_dim2
logical , DIMENSION(max_domains) :: cu_rad_feedback
integer :: dyn_opt
integer :: rk_ord
integer :: w_damping
integer :: diff_opt
integer :: km_opt
integer :: damp_opt
real , DIMENSION(max_domains) :: zdamp
real :: base_pres
real :: base_temp
real :: base_lapse
real :: iso_temp
real , DIMENSION(max_domains) :: dampcoef
real , DIMENSION(max_domains) :: khdif
real , DIMENSION(max_domains) :: kvdif
real , DIMENSION(max_domains) :: c_s
real , DIMENSION(max_domains) :: c_k
real , DIMENSION(max_domains) :: smdiv
real , DIMENSION(max_domains) :: emdiv
real , DIMENSION(max_domains) :: epssm
logical , DIMENSION(max_domains) :: non_hydrostatic
integer , DIMENSION(max_domains) :: time_step_sound
integer , DIMENSION(max_domains) :: h_mom_adv_order
integer , DIMENSION(max_domains) :: v_mom_adv_order
integer , DIMENSION(max_domains) :: h_sca_adv_order
integer , DIMENSION(max_domains) :: v_sca_adv_order
logical , DIMENSION(max_domains) :: top_radiation
real , DIMENSION(max_domains) :: tke_upper_bound
real , DIMENSION(max_domains) :: tke_drag_coefficient
real , DIMENSION(max_domains) :: tke_heat_flux
logical , DIMENSION(max_domains) :: pert_coriolis
logical :: euler_adv
integer :: idtadt
integer :: idtadc
logical :: boundary_flux
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
logical , DIMENSION(max_domains) :: polar
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
integer :: flag_metgrid
integer :: flag_snow
integer :: flag_psfc
integer :: flag_sm000010
integer :: flag_sm010040
integer :: flag_sm040100
integer :: flag_sm100200
integer :: flag_st000010
integer :: flag_st010040
integer :: flag_st040100
integer :: flag_st100200
integer :: flag_slp
integer :: flag_soilhgt
integer :: flag_mf_xy
real , DIMENSION(max_domains) :: bdyfrq
character*256 , DIMENSION(max_domains) :: mminlu
integer , DIMENSION(max_domains) :: iswater
integer , DIMENSION(max_domains) :: islake
integer , DIMENSION(max_domains) :: isice
integer , DIMENSION(max_domains) :: isurban
integer , DIMENSION(max_domains) :: isoilwater
integer , DIMENSION(max_domains) :: map_proj
integer :: dfi_stage
integer , DIMENSION(max_domains) :: mp_physics_dfi
integer    :: last_item_in_struct







NAMELIST /time_control/ run_days
NAMELIST /time_control/ run_hours
NAMELIST /time_control/ run_minutes
NAMELIST /time_control/ run_seconds
NAMELIST /time_control/ start_year
NAMELIST /time_control/ start_month
NAMELIST /time_control/ start_day
NAMELIST /time_control/ start_hour
NAMELIST /time_control/ start_minute
NAMELIST /time_control/ start_second
NAMELIST /time_control/ end_year
NAMELIST /time_control/ end_month
NAMELIST /time_control/ end_day
NAMELIST /time_control/ end_hour
NAMELIST /time_control/ end_minute
NAMELIST /time_control/ end_second
NAMELIST /time_control/ interval_seconds
NAMELIST /time_control/ input_from_file
NAMELIST /time_control/ fine_input_stream
NAMELIST /time_control/ history_interval
NAMELIST /time_control/ frames_per_outfile
NAMELIST /time_control/ frames_per_auxhist1
NAMELIST /time_control/ frames_per_auxhist2
NAMELIST /time_control/ frames_per_auxhist3
NAMELIST /time_control/ frames_per_auxhist4
NAMELIST /time_control/ frames_per_auxhist5
NAMELIST /time_control/ frames_per_auxhist6
NAMELIST /time_control/ frames_per_auxhist7
NAMELIST /time_control/ frames_per_auxhist8
NAMELIST /time_control/ frames_per_auxhist9
NAMELIST /time_control/ frames_per_auxhist10
NAMELIST /time_control/ frames_per_auxhist11
NAMELIST /time_control/ restart
NAMELIST /time_control/ restart_interval
NAMELIST /time_control/ io_form_input
NAMELIST /time_control/ io_form_history
NAMELIST /time_control/ io_form_restart
NAMELIST /time_control/ io_form_boundary
NAMELIST /time_control/ debug_level
NAMELIST /time_control/ self_test_domain
NAMELIST /time_control/ history_outname
NAMELIST /time_control/ auxhist1_outname
NAMELIST /time_control/ auxhist2_outname
NAMELIST /time_control/ auxhist3_outname
NAMELIST /time_control/ auxhist4_outname
NAMELIST /time_control/ auxhist5_outname
NAMELIST /time_control/ auxhist6_outname
NAMELIST /time_control/ auxhist7_outname
NAMELIST /time_control/ auxhist8_outname
NAMELIST /time_control/ auxhist9_outname
NAMELIST /time_control/ auxhist10_outname
NAMELIST /time_control/ auxhist11_outname
NAMELIST /time_control/ history_inname
NAMELIST /time_control/ auxhist1_inname
NAMELIST /time_control/ auxhist2_inname
NAMELIST /time_control/ auxhist3_inname
NAMELIST /time_control/ auxhist4_inname
NAMELIST /time_control/ auxhist5_inname
NAMELIST /time_control/ auxhist6_inname
NAMELIST /time_control/ auxhist7_inname
NAMELIST /time_control/ auxhist8_inname
NAMELIST /time_control/ auxhist9_inname
NAMELIST /time_control/ auxhist10_inname
NAMELIST /time_control/ auxhist11_inname
NAMELIST /time_control/ auxinput1_outname
NAMELIST /time_control/ auxinput2_outname
NAMELIST /time_control/ auxinput3_outname
NAMELIST /time_control/ auxinput4_outname
NAMELIST /time_control/ auxinput5_outname
NAMELIST /time_control/ auxinput6_outname
NAMELIST /time_control/ auxinput7_outname
NAMELIST /time_control/ auxinput8_outname
NAMELIST /time_control/ auxinput9_outname
NAMELIST /time_control/ auxinput10_outname
NAMELIST /time_control/ auxinput11_outname
NAMELIST /time_control/ auxinput1_inname
NAMELIST /time_control/ auxinput2_inname
NAMELIST /time_control/ auxinput3_inname
NAMELIST /time_control/ auxinput4_inname
NAMELIST /time_control/ auxinput5_inname
NAMELIST /time_control/ auxinput6_inname
NAMELIST /time_control/ auxinput7_inname
NAMELIST /time_control/ auxinput8_inname
NAMELIST /fdda/ sgfdda_inname
NAMELIST /fdda/ gfdda_inname
NAMELIST /time_control/ auxinput11_inname
NAMELIST /time_control/ history_interval_mo
NAMELIST /time_control/ history_interval_d
NAMELIST /time_control/ history_interval_h
NAMELIST /time_control/ history_interval_m
NAMELIST /time_control/ history_interval_s
NAMELIST /time_control/ inputout_interval_mo
NAMELIST /time_control/ inputout_interval_d
NAMELIST /time_control/ inputout_interval_h
NAMELIST /time_control/ inputout_interval_m
NAMELIST /time_control/ inputout_interval_s
NAMELIST /time_control/ inputout_interval
NAMELIST /time_control/ auxhist1_interval_mo
NAMELIST /time_control/ auxhist1_interval_d
NAMELIST /time_control/ auxhist1_interval_h
NAMELIST /time_control/ auxhist1_interval_m
NAMELIST /time_control/ auxhist1_interval_s
NAMELIST /time_control/ auxhist1_interval
NAMELIST /time_control/ auxhist2_interval_mo
NAMELIST /time_control/ auxhist2_interval_d
NAMELIST /time_control/ auxhist2_interval_h
NAMELIST /time_control/ auxhist2_interval_m
NAMELIST /time_control/ auxhist2_interval_s
NAMELIST /time_control/ auxhist2_interval
NAMELIST /time_control/ auxhist3_interval_mo
NAMELIST /time_control/ auxhist3_interval_d
NAMELIST /time_control/ auxhist3_interval_h
NAMELIST /time_control/ auxhist3_interval_m
NAMELIST /time_control/ auxhist3_interval_s
NAMELIST /time_control/ auxhist3_interval
NAMELIST /time_control/ auxhist4_interval_mo
NAMELIST /time_control/ auxhist4_interval_d
NAMELIST /time_control/ auxhist4_interval_h
NAMELIST /time_control/ auxhist4_interval_m
NAMELIST /time_control/ auxhist4_interval_s
NAMELIST /time_control/ auxhist4_interval
NAMELIST /time_control/ auxhist5_interval_mo
NAMELIST /time_control/ auxhist5_interval_d
NAMELIST /time_control/ auxhist5_interval_h
NAMELIST /time_control/ auxhist5_interval_m
NAMELIST /time_control/ auxhist5_interval_s
NAMELIST /time_control/ auxhist5_interval
NAMELIST /time_control/ auxhist6_interval_mo
NAMELIST /time_control/ auxhist6_interval_d
NAMELIST /time_control/ auxhist6_interval_h
NAMELIST /time_control/ auxhist6_interval_m
NAMELIST /time_control/ auxhist6_interval_s
NAMELIST /time_control/ auxhist6_interval
NAMELIST /time_control/ auxhist7_interval_mo
NAMELIST /time_control/ auxhist7_interval_d
NAMELIST /time_control/ auxhist7_interval_h
NAMELIST /time_control/ auxhist7_interval_m
NAMELIST /time_control/ auxhist7_interval_s
NAMELIST /time_control/ auxhist7_interval
NAMELIST /time_control/ auxhist8_interval_mo
NAMELIST /time_control/ auxhist8_interval_d
NAMELIST /time_control/ auxhist8_interval_h
NAMELIST /time_control/ auxhist8_interval_m
NAMELIST /time_control/ auxhist8_interval_s
NAMELIST /time_control/ auxhist8_interval
NAMELIST /time_control/ auxhist9_interval_mo
NAMELIST /time_control/ auxhist9_interval_d
NAMELIST /time_control/ auxhist9_interval_h
NAMELIST /time_control/ auxhist9_interval_m
NAMELIST /time_control/ auxhist9_interval_s
NAMELIST /time_control/ auxhist9_interval
NAMELIST /time_control/ auxhist10_interval_mo
NAMELIST /time_control/ auxhist10_interval_d
NAMELIST /time_control/ auxhist10_interval_h
NAMELIST /time_control/ auxhist10_interval_m
NAMELIST /time_control/ auxhist10_interval_s
NAMELIST /time_control/ auxhist10_interval
NAMELIST /time_control/ auxhist11_interval_mo
NAMELIST /time_control/ auxhist11_interval_d
NAMELIST /time_control/ auxhist11_interval_h
NAMELIST /time_control/ auxhist11_interval_m
NAMELIST /time_control/ auxhist11_interval_s
NAMELIST /time_control/ auxhist11_interval
NAMELIST /time_control/ auxinput1_interval_mo
NAMELIST /time_control/ auxinput1_interval_d
NAMELIST /time_control/ auxinput1_interval_h
NAMELIST /time_control/ auxinput1_interval_m
NAMELIST /time_control/ auxinput1_interval_s
NAMELIST /time_control/ auxinput1_interval
NAMELIST /time_control/ auxinput2_interval_mo
NAMELIST /time_control/ auxinput2_interval_d
NAMELIST /time_control/ auxinput2_interval_h
NAMELIST /time_control/ auxinput2_interval_m
NAMELIST /time_control/ auxinput2_interval_s
NAMELIST /time_control/ auxinput2_interval
NAMELIST /time_control/ auxinput3_interval_mo
NAMELIST /time_control/ auxinput3_interval_d
NAMELIST /time_control/ auxinput3_interval_h
NAMELIST /time_control/ auxinput3_interval_m
NAMELIST /time_control/ auxinput3_interval_s
NAMELIST /time_control/ auxinput3_interval
NAMELIST /time_control/ auxinput4_interval_mo
NAMELIST /time_control/ auxinput4_interval_d
NAMELIST /time_control/ auxinput4_interval_h
NAMELIST /time_control/ auxinput4_interval_m
NAMELIST /time_control/ auxinput4_interval_s
NAMELIST /time_control/ auxinput4_interval
NAMELIST /time_control/ auxinput5_interval_mo
NAMELIST /time_control/ auxinput5_interval_d
NAMELIST /time_control/ auxinput5_interval_h
NAMELIST /time_control/ auxinput5_interval_m
NAMELIST /time_control/ auxinput5_interval_s
NAMELIST /time_control/ auxinput5_interval
NAMELIST /time_control/ auxinput6_interval_mo
NAMELIST /time_control/ auxinput6_interval_d
NAMELIST /time_control/ auxinput6_interval_h
NAMELIST /time_control/ auxinput6_interval_m
NAMELIST /time_control/ auxinput6_interval_s
NAMELIST /time_control/ auxinput6_interval
NAMELIST /time_control/ auxinput7_interval_mo
NAMELIST /time_control/ auxinput7_interval_d
NAMELIST /time_control/ auxinput7_interval_h
NAMELIST /time_control/ auxinput7_interval_m
NAMELIST /time_control/ auxinput7_interval_s
NAMELIST /time_control/ auxinput7_interval
NAMELIST /time_control/ auxinput8_interval_mo
NAMELIST /time_control/ auxinput8_interval_d
NAMELIST /time_control/ auxinput8_interval_h
NAMELIST /time_control/ auxinput8_interval_m
NAMELIST /time_control/ auxinput8_interval_s
NAMELIST /time_control/ auxinput8_interval
NAMELIST /fdda/ sgfdda_interval_mo
NAMELIST /fdda/ sgfdda_interval_d
NAMELIST /fdda/ sgfdda_interval_h
NAMELIST /fdda/ sgfdda_interval_m
NAMELIST /fdda/ sgfdda_interval_s
NAMELIST /fdda/ sgfdda_interval
NAMELIST /fdda/ gfdda_interval_mo
NAMELIST /fdda/ gfdda_interval_d
NAMELIST /fdda/ gfdda_interval_h
NAMELIST /fdda/ gfdda_interval_m
NAMELIST /fdda/ gfdda_interval_s
NAMELIST /fdda/ gfdda_interval
NAMELIST /time_control/ auxinput11_interval_mo
NAMELIST /time_control/ auxinput11_interval_d
NAMELIST /time_control/ auxinput11_interval_h
NAMELIST /time_control/ auxinput11_interval_m
NAMELIST /time_control/ auxinput11_interval_s
NAMELIST /time_control/ auxinput11_interval
NAMELIST /time_control/ restart_interval_mo
NAMELIST /time_control/ restart_interval_d
NAMELIST /time_control/ restart_interval_h
NAMELIST /time_control/ restart_interval_m
NAMELIST /time_control/ restart_interval_s
NAMELIST /time_control/ history_begin_y
NAMELIST /time_control/ history_begin_mo
NAMELIST /time_control/ history_begin_d
NAMELIST /time_control/ history_begin_h
NAMELIST /time_control/ history_begin_m
NAMELIST /time_control/ history_begin_s
NAMELIST /time_control/ inputout_begin_y
NAMELIST /time_control/ inputout_begin_mo
NAMELIST /time_control/ inputout_begin_d
NAMELIST /time_control/ inputout_begin_h
NAMELIST /time_control/ inputout_begin_m
NAMELIST /time_control/ inputout_begin_s
NAMELIST /time_control/ auxhist1_begin_y
NAMELIST /time_control/ auxhist1_begin_mo
NAMELIST /time_control/ auxhist1_begin_d
NAMELIST /time_control/ auxhist1_begin_h
NAMELIST /time_control/ auxhist1_begin_m
NAMELIST /time_control/ auxhist1_begin_s
NAMELIST /time_control/ auxhist2_begin_y
NAMELIST /time_control/ auxhist2_begin_mo
NAMELIST /time_control/ auxhist2_begin_d
NAMELIST /time_control/ auxhist2_begin_h
NAMELIST /time_control/ auxhist2_begin_m
NAMELIST /time_control/ auxhist2_begin_s
NAMELIST /time_control/ auxhist3_begin_y
NAMELIST /time_control/ auxhist3_begin_mo
NAMELIST /time_control/ auxhist3_begin_d
NAMELIST /time_control/ auxhist3_begin_h
NAMELIST /time_control/ auxhist3_begin_m
NAMELIST /time_control/ auxhist3_begin_s
NAMELIST /time_control/ auxhist4_begin_y
NAMELIST /time_control/ auxhist4_begin_mo
NAMELIST /time_control/ auxhist4_begin_d
NAMELIST /time_control/ auxhist4_begin_h
NAMELIST /time_control/ auxhist4_begin_m
NAMELIST /time_control/ auxhist4_begin_s
NAMELIST /time_control/ auxhist5_begin_y
NAMELIST /time_control/ auxhist5_begin_mo
NAMELIST /time_control/ auxhist5_begin_d
NAMELIST /time_control/ auxhist5_begin_h
NAMELIST /time_control/ auxhist5_begin_m
NAMELIST /time_control/ auxhist5_begin_s
NAMELIST /time_control/ auxhist6_begin_y
NAMELIST /time_control/ auxhist6_begin_mo
NAMELIST /time_control/ auxhist6_begin_d
NAMELIST /time_control/ auxhist6_begin_h
NAMELIST /time_control/ auxhist6_begin_m
NAMELIST /time_control/ auxhist6_begin_s
NAMELIST /time_control/ auxhist7_begin_y
NAMELIST /time_control/ auxhist7_begin_mo
NAMELIST /time_control/ auxhist7_begin_d
NAMELIST /time_control/ auxhist7_begin_h
NAMELIST /time_control/ auxhist7_begin_m
NAMELIST /time_control/ auxhist7_begin_s
NAMELIST /time_control/ auxhist8_begin_y
NAMELIST /time_control/ auxhist8_begin_mo
NAMELIST /time_control/ auxhist8_begin_d
NAMELIST /time_control/ auxhist8_begin_h
NAMELIST /time_control/ auxhist8_begin_m
NAMELIST /time_control/ auxhist8_begin_s
NAMELIST /time_control/ auxhist9_begin_y
NAMELIST /time_control/ auxhist9_begin_mo
NAMELIST /time_control/ auxhist9_begin_d
NAMELIST /time_control/ auxhist9_begin_h
NAMELIST /time_control/ auxhist9_begin_m
NAMELIST /time_control/ auxhist9_begin_s
NAMELIST /time_control/ auxhist10_begin_y
NAMELIST /time_control/ auxhist10_begin_mo
NAMELIST /time_control/ auxhist10_begin_d
NAMELIST /time_control/ auxhist10_begin_h
NAMELIST /time_control/ auxhist10_begin_m
NAMELIST /time_control/ auxhist10_begin_s
NAMELIST /time_control/ auxhist11_begin_y
NAMELIST /time_control/ auxhist11_begin_mo
NAMELIST /time_control/ auxhist11_begin_d
NAMELIST /time_control/ auxhist11_begin_h
NAMELIST /time_control/ auxhist11_begin_m
NAMELIST /time_control/ auxhist11_begin_s
NAMELIST /time_control/ auxinput1_begin_y
NAMELIST /time_control/ auxinput1_begin_mo
NAMELIST /time_control/ auxinput1_begin_d
NAMELIST /time_control/ auxinput1_begin_h
NAMELIST /time_control/ auxinput1_begin_m
NAMELIST /time_control/ auxinput1_begin_s
NAMELIST /time_control/ auxinput2_begin_y
NAMELIST /time_control/ auxinput2_begin_mo
NAMELIST /time_control/ auxinput2_begin_d
NAMELIST /time_control/ auxinput2_begin_h
NAMELIST /time_control/ auxinput2_begin_m
NAMELIST /time_control/ auxinput2_begin_s
NAMELIST /time_control/ auxinput3_begin_y
NAMELIST /time_control/ auxinput3_begin_mo
NAMELIST /time_control/ auxinput3_begin_d
NAMELIST /time_control/ auxinput3_begin_h
NAMELIST /time_control/ auxinput3_begin_m
NAMELIST /time_control/ auxinput3_begin_s
NAMELIST /time_control/ auxinput4_begin_y
NAMELIST /time_control/ auxinput4_begin_mo
NAMELIST /time_control/ auxinput4_begin_d
NAMELIST /time_control/ auxinput4_begin_h
NAMELIST /time_control/ auxinput4_begin_m
NAMELIST /time_control/ auxinput4_begin_s
NAMELIST /time_control/ auxinput5_begin_y
NAMELIST /time_control/ auxinput5_begin_mo
NAMELIST /time_control/ auxinput5_begin_d
NAMELIST /time_control/ auxinput5_begin_h
NAMELIST /time_control/ auxinput5_begin_m
NAMELIST /time_control/ auxinput5_begin_s
NAMELIST /time_control/ auxinput6_begin_y
NAMELIST /time_control/ auxinput6_begin_mo
NAMELIST /time_control/ auxinput6_begin_d
NAMELIST /time_control/ auxinput6_begin_h
NAMELIST /time_control/ auxinput6_begin_m
NAMELIST /time_control/ auxinput6_begin_s
NAMELIST /time_control/ auxinput7_begin_y
NAMELIST /time_control/ auxinput7_begin_mo
NAMELIST /time_control/ auxinput7_begin_d
NAMELIST /time_control/ auxinput7_begin_h
NAMELIST /time_control/ auxinput7_begin_m
NAMELIST /time_control/ auxinput7_begin_s
NAMELIST /time_control/ auxinput8_begin_y
NAMELIST /time_control/ auxinput8_begin_mo
NAMELIST /time_control/ auxinput8_begin_d
NAMELIST /time_control/ auxinput8_begin_h
NAMELIST /time_control/ auxinput8_begin_m
NAMELIST /time_control/ auxinput8_begin_s
NAMELIST /fdda/ sgfdda_begin_y
NAMELIST /fdda/ sgfdda_begin_mo
NAMELIST /fdda/ sgfdda_begin_d
NAMELIST /fdda/ sgfdda_begin_h
NAMELIST /fdda/ sgfdda_begin_m
NAMELIST /fdda/ sgfdda_begin_s
NAMELIST /fdda/ gfdda_begin_y
NAMELIST /fdda/ gfdda_begin_mo
NAMELIST /fdda/ gfdda_begin_d
NAMELIST /fdda/ gfdda_begin_h
NAMELIST /fdda/ gfdda_begin_m
NAMELIST /fdda/ gfdda_begin_s
NAMELIST /time_control/ auxinput11_begin_y
NAMELIST /time_control/ auxinput11_begin_mo
NAMELIST /time_control/ auxinput11_begin_d
NAMELIST /time_control/ auxinput11_begin_h
NAMELIST /time_control/ auxinput11_begin_m
NAMELIST /time_control/ auxinput11_begin_s
NAMELIST /time_control/ restart_begin_y
NAMELIST /time_control/ restart_begin_mo
NAMELIST /time_control/ restart_begin_d
NAMELIST /time_control/ restart_begin_h
NAMELIST /time_control/ restart_begin_m
NAMELIST /time_control/ restart_begin_s
NAMELIST /time_control/ history_end_y
NAMELIST /time_control/ history_end_mo
NAMELIST /time_control/ history_end_d
NAMELIST /time_control/ history_end_h
NAMELIST /time_control/ history_end_m
NAMELIST /time_control/ history_end_s
NAMELIST /time_control/ inputout_end_y
NAMELIST /time_control/ inputout_end_mo
NAMELIST /time_control/ inputout_end_d
NAMELIST /time_control/ inputout_end_h
NAMELIST /time_control/ inputout_end_m
NAMELIST /time_control/ inputout_end_s
NAMELIST /time_control/ auxhist1_end_y
NAMELIST /time_control/ auxhist1_end_mo
NAMELIST /time_control/ auxhist1_end_d
NAMELIST /time_control/ auxhist1_end_h
NAMELIST /time_control/ auxhist1_end_m
NAMELIST /time_control/ auxhist1_end_s
NAMELIST /time_control/ auxhist2_end_y
NAMELIST /time_control/ auxhist2_end_mo
NAMELIST /time_control/ auxhist2_end_d
NAMELIST /time_control/ auxhist2_end_h
NAMELIST /time_control/ auxhist2_end_m
NAMELIST /time_control/ auxhist2_end_s
NAMELIST /time_control/ auxhist3_end_y
NAMELIST /time_control/ auxhist3_end_mo
NAMELIST /time_control/ auxhist3_end_d
NAMELIST /time_control/ auxhist3_end_h
NAMELIST /time_control/ auxhist3_end_m
NAMELIST /time_control/ auxhist3_end_s
NAMELIST /time_control/ auxhist4_end_y
NAMELIST /time_control/ auxhist4_end_mo
NAMELIST /time_control/ auxhist4_end_d
NAMELIST /time_control/ auxhist4_end_h
NAMELIST /time_control/ auxhist4_end_m
NAMELIST /time_control/ auxhist4_end_s
NAMELIST /time_control/ auxhist5_end_y
NAMELIST /time_control/ auxhist5_end_mo
NAMELIST /time_control/ auxhist5_end_d
NAMELIST /time_control/ auxhist5_end_h
NAMELIST /time_control/ auxhist5_end_m
NAMELIST /time_control/ auxhist5_end_s
NAMELIST /time_control/ auxhist6_end_y
NAMELIST /time_control/ auxhist6_end_mo
NAMELIST /time_control/ auxhist6_end_d
NAMELIST /time_control/ auxhist6_end_h
NAMELIST /time_control/ auxhist6_end_m
NAMELIST /time_control/ auxhist6_end_s
NAMELIST /time_control/ auxhist7_end_y
NAMELIST /time_control/ auxhist7_end_mo
NAMELIST /time_control/ auxhist7_end_d
NAMELIST /time_control/ auxhist7_end_h
NAMELIST /time_control/ auxhist7_end_m
NAMELIST /time_control/ auxhist7_end_s
NAMELIST /time_control/ auxhist8_end_y
NAMELIST /time_control/ auxhist8_end_mo
NAMELIST /time_control/ auxhist8_end_d
NAMELIST /time_control/ auxhist8_end_h
NAMELIST /time_control/ auxhist8_end_m
NAMELIST /time_control/ auxhist8_end_s
NAMELIST /time_control/ auxhist9_end_y
NAMELIST /time_control/ auxhist9_end_mo
NAMELIST /time_control/ auxhist9_end_d
NAMELIST /time_control/ auxhist9_end_h
NAMELIST /time_control/ auxhist9_end_m
NAMELIST /time_control/ auxhist9_end_s
NAMELIST /time_control/ auxhist10_end_y
NAMELIST /time_control/ auxhist10_end_mo
NAMELIST /time_control/ auxhist10_end_d
NAMELIST /time_control/ auxhist10_end_h
NAMELIST /time_control/ auxhist10_end_m
NAMELIST /time_control/ auxhist10_end_s
NAMELIST /time_control/ auxhist11_end_y
NAMELIST /time_control/ auxhist11_end_mo
NAMELIST /time_control/ auxhist11_end_d
NAMELIST /time_control/ auxhist11_end_h
NAMELIST /time_control/ auxhist11_end_m
NAMELIST /time_control/ auxhist11_end_s
NAMELIST /time_control/ auxinput1_end_y
NAMELIST /time_control/ auxinput1_end_mo
NAMELIST /time_control/ auxinput1_end_d
NAMELIST /time_control/ auxinput1_end_h
NAMELIST /time_control/ auxinput1_end_m
NAMELIST /time_control/ auxinput1_end_s
NAMELIST /time_control/ auxinput2_end_y
NAMELIST /time_control/ auxinput2_end_mo
NAMELIST /time_control/ auxinput2_end_d
NAMELIST /time_control/ auxinput2_end_h
NAMELIST /time_control/ auxinput2_end_m
NAMELIST /time_control/ auxinput2_end_s
NAMELIST /time_control/ auxinput3_end_y
NAMELIST /time_control/ auxinput3_end_mo
NAMELIST /time_control/ auxinput3_end_d
NAMELIST /time_control/ auxinput3_end_h
NAMELIST /time_control/ auxinput3_end_m
NAMELIST /time_control/ auxinput3_end_s
NAMELIST /time_control/ auxinput4_end_y
NAMELIST /time_control/ auxinput4_end_mo
NAMELIST /time_control/ auxinput4_end_d
NAMELIST /time_control/ auxinput4_end_h
NAMELIST /time_control/ auxinput4_end_m
NAMELIST /time_control/ auxinput4_end_s
NAMELIST /time_control/ auxinput5_end_y
NAMELIST /time_control/ auxinput5_end_mo
NAMELIST /time_control/ auxinput5_end_d
NAMELIST /time_control/ auxinput5_end_h
NAMELIST /time_control/ auxinput5_end_m
NAMELIST /time_control/ auxinput5_end_s
NAMELIST /time_control/ auxinput6_end_y
NAMELIST /time_control/ auxinput6_end_mo
NAMELIST /time_control/ auxinput6_end_d
NAMELIST /time_control/ auxinput6_end_h
NAMELIST /time_control/ auxinput6_end_m
NAMELIST /time_control/ auxinput6_end_s
NAMELIST /time_control/ auxinput7_end_y
NAMELIST /time_control/ auxinput7_end_mo
NAMELIST /time_control/ auxinput7_end_d
NAMELIST /time_control/ auxinput7_end_h
NAMELIST /time_control/ auxinput7_end_m
NAMELIST /time_control/ auxinput7_end_s
NAMELIST /time_control/ auxinput8_end_y
NAMELIST /time_control/ auxinput8_end_mo
NAMELIST /time_control/ auxinput8_end_d
NAMELIST /time_control/ auxinput8_end_h
NAMELIST /time_control/ auxinput8_end_m
NAMELIST /time_control/ auxinput8_end_s
NAMELIST /fdda/ sgfdda_end_y
NAMELIST /fdda/ sgfdda_end_mo
NAMELIST /fdda/ sgfdda_end_d
NAMELIST /fdda/ sgfdda_end_h
NAMELIST /fdda/ sgfdda_end_m
NAMELIST /fdda/ sgfdda_end_s
NAMELIST /fdda/ gfdda_end_y
NAMELIST /fdda/ gfdda_end_mo
NAMELIST /fdda/ gfdda_end_d
NAMELIST /fdda/ gfdda_end_h
NAMELIST /fdda/ gfdda_end_m
NAMELIST /fdda/ gfdda_end_s
NAMELIST /time_control/ auxinput11_end_y
NAMELIST /time_control/ auxinput11_end_mo
NAMELIST /time_control/ auxinput11_end_d
NAMELIST /time_control/ auxinput11_end_h
NAMELIST /time_control/ auxinput11_end_m
NAMELIST /time_control/ auxinput11_end_s
NAMELIST /time_control/ io_form_auxinput1
NAMELIST /time_control/ io_form_auxinput2
NAMELIST /time_control/ io_form_auxinput3
NAMELIST /time_control/ io_form_auxinput4
NAMELIST /time_control/ io_form_auxinput5
NAMELIST /time_control/ io_form_auxinput6
NAMELIST /time_control/ io_form_auxinput7
NAMELIST /time_control/ io_form_auxinput8
NAMELIST /fdda/ io_form_sgfdda
NAMELIST /fdda/ io_form_gfdda
NAMELIST /time_control/ io_form_auxinput11
NAMELIST /time_control/ io_form_auxhist1
NAMELIST /time_control/ io_form_auxhist2
NAMELIST /time_control/ io_form_auxhist3
NAMELIST /time_control/ io_form_auxhist4
NAMELIST /time_control/ io_form_auxhist5
NAMELIST /time_control/ io_form_auxhist6
NAMELIST /time_control/ io_form_auxhist7
NAMELIST /time_control/ io_form_auxhist8
NAMELIST /time_control/ io_form_auxhist9
NAMELIST /time_control/ io_form_auxhist10
NAMELIST /time_control/ io_form_auxhist11
NAMELIST /time_control/ reset_simulation_start
NAMELIST /domains/ sr_x
NAMELIST /domains/ sr_y
NAMELIST /time_control/ julyr
NAMELIST /time_control/ julday
NAMELIST /time_control/ gmt
NAMELIST /time_control/ input_inname
NAMELIST /time_control/ input_outname
NAMELIST /time_control/ bdy_inname
NAMELIST /time_control/ bdy_outname
NAMELIST /time_control/ rst_inname
NAMELIST /time_control/ rst_outname
NAMELIST /time_control/ write_input
NAMELIST /time_control/ write_restart_at_0h
NAMELIST /time_control/ adjust_output_times
NAMELIST /time_control/ adjust_input_times
NAMELIST /time_control/ tstart
NAMELIST /time_control/ nocolons
NAMELIST /time_control/ cycling
NAMELIST /dfi_control/ dfi_opt
NAMELIST /dfi_control/ dfi_nfilter
NAMELIST /dfi_control/ dfi_write_filtered_input
NAMELIST /dfi_control/ dfi_write_dfi_history
NAMELIST /dfi_control/ dfi_cutoff_seconds
NAMELIST /dfi_control/ dfi_time_dim
NAMELIST /dfi_control/ dfi_fwdstop_year
NAMELIST /dfi_control/ dfi_fwdstop_month
NAMELIST /dfi_control/ dfi_fwdstop_day
NAMELIST /dfi_control/ dfi_fwdstop_hour
NAMELIST /dfi_control/ dfi_fwdstop_minute
NAMELIST /dfi_control/ dfi_fwdstop_second
NAMELIST /dfi_control/ dfi_bckstop_year
NAMELIST /dfi_control/ dfi_bckstop_month
NAMELIST /dfi_control/ dfi_bckstop_day
NAMELIST /dfi_control/ dfi_bckstop_hour
NAMELIST /dfi_control/ dfi_bckstop_minute
NAMELIST /dfi_control/ dfi_bckstop_second
NAMELIST /domains/ time_step
NAMELIST /domains/ time_step_fract_num
NAMELIST /domains/ time_step_fract_den
NAMELIST /domains/ max_dom
NAMELIST /domains/ s_we
NAMELIST /domains/ e_we
NAMELIST /domains/ s_sn
NAMELIST /domains/ e_sn
NAMELIST /domains/ s_vert
NAMELIST /domains/ e_vert
NAMELIST /domains/ dx
NAMELIST /domains/ dy
NAMELIST /domains/ grid_id
NAMELIST /domains/ grid_allowed
NAMELIST /domains/ parent_id
NAMELIST /domains/ i_parent_start
NAMELIST /domains/ j_parent_start
NAMELIST /domains/ parent_grid_ratio
NAMELIST /domains/ parent_time_step_ratio
NAMELIST /domains/ feedback
NAMELIST /domains/ smooth_option
NAMELIST /domains/ ztop
NAMELIST /domains/ moad_grid_ratio
NAMELIST /domains/ moad_time_step_ratio
NAMELIST /domains/ shw
NAMELIST /domains/ tile_sz_x
NAMELIST /domains/ tile_sz_y
NAMELIST /domains/ numtiles
NAMELIST /domains/ nproc_x
NAMELIST /domains/ nproc_y
NAMELIST /domains/ irand
NAMELIST /domains/ ts_buf_size
NAMELIST /domains/ max_ts_locs
NAMELIST /domains/ num_moves
NAMELIST /domains/ move_id
NAMELIST /domains/ move_interval
NAMELIST /domains/ move_cd_x
NAMELIST /domains/ move_cd_y
NAMELIST /domains/ swap_x
NAMELIST /domains/ swap_y
NAMELIST /domains/ cycle_x
NAMELIST /domains/ cycle_y
NAMELIST /domains/ reorder_mesh
NAMELIST /domains/ perturb_input
NAMELIST /domains/ eta_levels
NAMELIST /domains/ ptsgm
NAMELIST /domains/ num_metgrid_levels
NAMELIST /domains/ p_top_requested
NAMELIST /physics/ mp_physics
NAMELIST /physics/ ra_lw_physics
NAMELIST /physics/ ra_sw_physics
NAMELIST /physics/ radt
NAMELIST /physics/ sf_sfclay_physics
NAMELIST /physics/ sf_surface_physics
NAMELIST /physics/ bl_pbl_physics
NAMELIST /physics/ sf_urban_physics
NAMELIST /physics/ bldt
NAMELIST /physics/ cu_physics
NAMELIST /physics/ cudt
NAMELIST /physics/ gsmdt
NAMELIST /physics/ isfflx
NAMELIST /physics/ ifsnow
NAMELIST /physics/ icloud
NAMELIST /physics/ swrad_scat
NAMELIST /physics/ surface_input_source
NAMELIST /physics/ num_soil_layers
NAMELIST /physics/ num_urban_layers
NAMELIST /physics/ maxiens
NAMELIST /physics/ maxens
NAMELIST /physics/ maxens2
NAMELIST /physics/ maxens3
NAMELIST /physics/ ensdim
NAMELIST /physics/ chem_opt
NAMELIST /physics/ num_land_cat
NAMELIST /physics/ num_soil_cat
NAMELIST /physics/ mp_zero_out
NAMELIST /physics/ mp_zero_out_thresh
NAMELIST /physics/ seaice_threshold
NAMELIST /physics/ fractional_seaice
NAMELIST /physics/ sst_update
NAMELIST /physics/ usemonalb
NAMELIST /physics/ rdmaxalb
NAMELIST /physics/ rdlai2d
NAMELIST /physics/ gwd_opt
NAMELIST /physics/ idtad
NAMELIST /physics/ nsoil
NAMELIST /physics/ nphs
NAMELIST /physics/ ncnvc
NAMELIST /physics/ nrads
NAMELIST /physics/ nradl
NAMELIST /physics/ tprec
NAMELIST /physics/ theat
NAMELIST /physics/ tclod
NAMELIST /physics/ trdsw
NAMELIST /physics/ trdlw
NAMELIST /physics/ tsrfc
NAMELIST /physics/ pcpflg
NAMELIST /physics/ sigma
NAMELIST /physics/ sfenth
NAMELIST /physics/ co2tf
NAMELIST /physics/ ra_call_offset
NAMELIST /physics/ cam_abs_freq_s
NAMELIST /physics/ levsiz
NAMELIST /physics/ paerlev
NAMELIST /physics/ cam_abs_dim1
NAMELIST /physics/ cam_abs_dim2
NAMELIST /physics/ cu_rad_feedback
NAMELIST /dynamics/ dyn_opt
NAMELIST /dynamics/ rk_ord
NAMELIST /dynamics/ w_damping
NAMELIST /dynamics/ diff_opt
NAMELIST /dynamics/ km_opt
NAMELIST /dynamics/ damp_opt
NAMELIST /dynamics/ zdamp
NAMELIST /dynamics/ base_pres
NAMELIST /dynamics/ base_temp
NAMELIST /dynamics/ base_lapse
NAMELIST /dynamics/ iso_temp
NAMELIST /dynamics/ dampcoef
NAMELIST /dynamics/ khdif
NAMELIST /dynamics/ kvdif
NAMELIST /dynamics/ c_s
NAMELIST /dynamics/ c_k
NAMELIST /dynamics/ smdiv
NAMELIST /dynamics/ emdiv
NAMELIST /dynamics/ epssm
NAMELIST /dynamics/ non_hydrostatic
NAMELIST /dynamics/ time_step_sound
NAMELIST /dynamics/ h_mom_adv_order
NAMELIST /dynamics/ v_mom_adv_order
NAMELIST /dynamics/ h_sca_adv_order
NAMELIST /dynamics/ v_sca_adv_order
NAMELIST /dynamics/ top_radiation
NAMELIST /dynamics/ tke_upper_bound
NAMELIST /dynamics/ tke_drag_coefficient
NAMELIST /dynamics/ tke_heat_flux
NAMELIST /dynamics/ pert_coriolis
NAMELIST /dynamics/ euler_adv
NAMELIST /dynamics/ idtadt
NAMELIST /dynamics/ idtadc
NAMELIST /dynamics/ boundary_flux
NAMELIST /bdy_control/ spec_bdy_width
NAMELIST /bdy_control/ spec_zone
NAMELIST /bdy_control/ relax_zone
NAMELIST /bdy_control/ specified
NAMELIST /bdy_control/ periodic_x
NAMELIST /bdy_control/ symmetric_xs
NAMELIST /bdy_control/ symmetric_xe
NAMELIST /bdy_control/ open_xs
NAMELIST /bdy_control/ open_xe
NAMELIST /bdy_control/ periodic_y
NAMELIST /bdy_control/ symmetric_ys
NAMELIST /bdy_control/ symmetric_ye
NAMELIST /bdy_control/ open_ys
NAMELIST /bdy_control/ open_ye
NAMELIST /bdy_control/ polar
NAMELIST /bdy_control/ nested
NAMELIST /bdy_control/ real_data_init_type
NAMELIST /grib2/ background_proc_id
NAMELIST /grib2/ forecast_proc_id
NAMELIST /grib2/ production_status
NAMELIST /grib2/ compression



     logical , DIMENSION(max_domains) :: pd_moist
     logical , DIMENSION(max_domains) :: pd_chem
     logical , DIMENSION(max_domains) :: pd_tke
     logical , DIMENSION(max_domains) :: pd_scalar
     NAMELIST /dynamics/ pd_moist
     NAMELIST /dynamics/ pd_chem
     NAMELIST /dynamics/ pd_tke
     NAMELIST /dynamics/ pd_scalar





     REWIND ( UNIT = nml_read_unit )
     READ   ( UNIT = nml_read_unit , NML = dynamics , iostat=nml_error )

     IF ( nml_error .EQ. 0 ) then    
        CALL wrf_debug(0, "Are pd_moist, pd_chem, pd_tke, or pd_scalar still in your "// &
                              TRIM(nml_name)//" namelist?")
        CALL wrf_error_fatal3("",7937,&
"Replace them with moist_adv_opt, chem_adv_opt, tke_adv_opt"// &
                             " and scalar_adv_opt, respectively.")
     ELSE     
        return
     ENDIF

   END SUBROUTINE wrf_alt_nml_dynamics

   SUBROUTINE wrf_alt_nml_physics (nml_read_unit, nml_name)







     IMPLICIT NONE
     INTEGER, INTENT(IN)       :: nml_read_unit
     CHARACTER*(*), INTENT(IN) :: nml_name
     INTEGER                   :: nml_error







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
character*256 :: auxinput1_inname
character*256 :: auxinput2_inname
character*256 :: auxinput3_inname
character*256 :: auxinput4_inname
character*256 :: auxinput5_inname
character*256 :: auxinput6_inname
character*256 :: auxinput7_inname
character*256 :: auxinput8_inname
character*256 :: sgfdda_inname
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
integer , DIMENSION(max_domains) :: sgfdda_interval_mo
integer , DIMENSION(max_domains) :: sgfdda_interval_d
integer , DIMENSION(max_domains) :: sgfdda_interval_h
integer , DIMENSION(max_domains) :: sgfdda_interval_m
integer , DIMENSION(max_domains) :: sgfdda_interval_s
integer , DIMENSION(max_domains) :: sgfdda_interval
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
integer , DIMENSION(max_domains) :: sgfdda_begin_y
integer , DIMENSION(max_domains) :: sgfdda_begin_mo
integer , DIMENSION(max_domains) :: sgfdda_begin_d
integer , DIMENSION(max_domains) :: sgfdda_begin_h
integer , DIMENSION(max_domains) :: sgfdda_begin_m
integer , DIMENSION(max_domains) :: sgfdda_begin_s
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
integer , DIMENSION(max_domains) :: sgfdda_end_y
integer , DIMENSION(max_domains) :: sgfdda_end_mo
integer , DIMENSION(max_domains) :: sgfdda_end_d
integer , DIMENSION(max_domains) :: sgfdda_end_h
integer , DIMENSION(max_domains) :: sgfdda_end_m
integer , DIMENSION(max_domains) :: sgfdda_end_s
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
integer :: io_form_sgfdda
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
integer :: simulation_start_year
integer :: simulation_start_month
integer :: simulation_start_day
integer :: simulation_start_hour
integer :: simulation_start_minute
integer :: simulation_start_second
logical :: reset_simulation_start
integer , DIMENSION(max_domains) :: sr_x
integer , DIMENSION(max_domains) :: sr_y
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
real , DIMENSION(max_domains) :: tstart
logical :: nocolons
logical :: cycling
integer :: dfi_opt
integer :: dfi_nfilter
logical :: dfi_write_filtered_input
logical :: dfi_write_dfi_history
integer :: dfi_cutoff_seconds
integer :: dfi_time_dim
integer :: dfi_fwdstop_year
integer :: dfi_fwdstop_month
integer :: dfi_fwdstop_day
integer :: dfi_fwdstop_hour
integer :: dfi_fwdstop_minute
integer :: dfi_fwdstop_second
integer :: dfi_bckstop_year
integer :: dfi_bckstop_month
integer :: dfi_bckstop_day
integer :: dfi_bckstop_hour
integer :: dfi_bckstop_minute
integer :: dfi_bckstop_second
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
real , DIMENSION(max_domains) :: dx
real , DIMENSION(max_domains) :: dy
integer , DIMENSION(max_domains) :: grid_id
logical , DIMENSION(max_domains) :: grid_allowed
integer , DIMENSION(max_domains) :: parent_id
integer , DIMENSION(max_domains) :: i_parent_start
integer , DIMENSION(max_domains) :: j_parent_start
integer , DIMENSION(max_domains) :: parent_grid_ratio
integer , DIMENSION(max_domains) :: parent_time_step_ratio
integer :: feedback
integer :: smooth_option
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
integer :: ts_buf_size
integer :: max_ts_locs
integer :: num_moves
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
real :: ptsgm
integer :: num_metgrid_levels
real :: p_top_requested
integer , DIMENSION(max_domains) :: mp_physics
integer , DIMENSION(max_domains) :: ra_lw_physics
integer , DIMENSION(max_domains) :: ra_sw_physics
real , DIMENSION(max_domains) :: radt
integer , DIMENSION(max_domains) :: sf_sfclay_physics
integer , DIMENSION(max_domains) :: sf_surface_physics
integer , DIMENSION(max_domains) :: bl_pbl_physics
integer , DIMENSION(max_domains) :: sf_urban_physics
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
integer :: num_urban_layers
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
integer :: fractional_seaice
integer :: sst_update
logical :: usemonalb
logical :: rdmaxalb
logical :: rdlai2d
integer , DIMENSION(max_domains) :: gwd_opt
integer , DIMENSION(max_domains) :: idtad
integer , DIMENSION(max_domains) :: nsoil
integer , DIMENSION(max_domains) :: nphs
integer , DIMENSION(max_domains) :: ncnvc
integer , DIMENSION(max_domains) :: nrads
integer , DIMENSION(max_domains) :: nradl
real , DIMENSION(max_domains) :: tprec
real , DIMENSION(max_domains) :: theat
real , DIMENSION(max_domains) :: tclod
real , DIMENSION(max_domains) :: trdsw
real , DIMENSION(max_domains) :: trdlw
real , DIMENSION(max_domains) :: tsrfc
logical , DIMENSION(max_domains) :: pcpflg
integer , DIMENSION(max_domains) :: sigma
real , DIMENSION(max_domains) :: sfenth
integer :: co2tf
integer :: ra_call_offset
real :: cam_abs_freq_s
integer :: levsiz
integer :: paerlev
integer :: cam_abs_dim1
integer :: cam_abs_dim2
logical , DIMENSION(max_domains) :: cu_rad_feedback
integer :: dyn_opt
integer :: rk_ord
integer :: w_damping
integer :: diff_opt
integer :: km_opt
integer :: damp_opt
real , DIMENSION(max_domains) :: zdamp
real :: base_pres
real :: base_temp
real :: base_lapse
real :: iso_temp
real , DIMENSION(max_domains) :: dampcoef
real , DIMENSION(max_domains) :: khdif
real , DIMENSION(max_domains) :: kvdif
real , DIMENSION(max_domains) :: c_s
real , DIMENSION(max_domains) :: c_k
real , DIMENSION(max_domains) :: smdiv
real , DIMENSION(max_domains) :: emdiv
real , DIMENSION(max_domains) :: epssm
logical , DIMENSION(max_domains) :: non_hydrostatic
integer , DIMENSION(max_domains) :: time_step_sound
integer , DIMENSION(max_domains) :: h_mom_adv_order
integer , DIMENSION(max_domains) :: v_mom_adv_order
integer , DIMENSION(max_domains) :: h_sca_adv_order
integer , DIMENSION(max_domains) :: v_sca_adv_order
logical , DIMENSION(max_domains) :: top_radiation
real , DIMENSION(max_domains) :: tke_upper_bound
real , DIMENSION(max_domains) :: tke_drag_coefficient
real , DIMENSION(max_domains) :: tke_heat_flux
logical , DIMENSION(max_domains) :: pert_coriolis
logical :: euler_adv
integer :: idtadt
integer :: idtadc
logical :: boundary_flux
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
logical , DIMENSION(max_domains) :: polar
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
integer :: flag_metgrid
integer :: flag_snow
integer :: flag_psfc
integer :: flag_sm000010
integer :: flag_sm010040
integer :: flag_sm040100
integer :: flag_sm100200
integer :: flag_st000010
integer :: flag_st010040
integer :: flag_st040100
integer :: flag_st100200
integer :: flag_slp
integer :: flag_soilhgt
integer :: flag_mf_xy
real , DIMENSION(max_domains) :: bdyfrq
character*256 , DIMENSION(max_domains) :: mminlu
integer , DIMENSION(max_domains) :: iswater
integer , DIMENSION(max_domains) :: islake
integer , DIMENSION(max_domains) :: isice
integer , DIMENSION(max_domains) :: isurban
integer , DIMENSION(max_domains) :: isoilwater
integer , DIMENSION(max_domains) :: map_proj
integer :: dfi_stage
integer , DIMENSION(max_domains) :: mp_physics_dfi
integer    :: last_item_in_struct







NAMELIST /time_control/ run_days
NAMELIST /time_control/ run_hours
NAMELIST /time_control/ run_minutes
NAMELIST /time_control/ run_seconds
NAMELIST /time_control/ start_year
NAMELIST /time_control/ start_month
NAMELIST /time_control/ start_day
NAMELIST /time_control/ start_hour
NAMELIST /time_control/ start_minute
NAMELIST /time_control/ start_second
NAMELIST /time_control/ end_year
NAMELIST /time_control/ end_month
NAMELIST /time_control/ end_day
NAMELIST /time_control/ end_hour
NAMELIST /time_control/ end_minute
NAMELIST /time_control/ end_second
NAMELIST /time_control/ interval_seconds
NAMELIST /time_control/ input_from_file
NAMELIST /time_control/ fine_input_stream
NAMELIST /time_control/ history_interval
NAMELIST /time_control/ frames_per_outfile
NAMELIST /time_control/ frames_per_auxhist1
NAMELIST /time_control/ frames_per_auxhist2
NAMELIST /time_control/ frames_per_auxhist3
NAMELIST /time_control/ frames_per_auxhist4
NAMELIST /time_control/ frames_per_auxhist5
NAMELIST /time_control/ frames_per_auxhist6
NAMELIST /time_control/ frames_per_auxhist7
NAMELIST /time_control/ frames_per_auxhist8
NAMELIST /time_control/ frames_per_auxhist9
NAMELIST /time_control/ frames_per_auxhist10
NAMELIST /time_control/ frames_per_auxhist11
NAMELIST /time_control/ restart
NAMELIST /time_control/ restart_interval
NAMELIST /time_control/ io_form_input
NAMELIST /time_control/ io_form_history
NAMELIST /time_control/ io_form_restart
NAMELIST /time_control/ io_form_boundary
NAMELIST /time_control/ debug_level
NAMELIST /time_control/ self_test_domain
NAMELIST /time_control/ history_outname
NAMELIST /time_control/ auxhist1_outname
NAMELIST /time_control/ auxhist2_outname
NAMELIST /time_control/ auxhist3_outname
NAMELIST /time_control/ auxhist4_outname
NAMELIST /time_control/ auxhist5_outname
NAMELIST /time_control/ auxhist6_outname
NAMELIST /time_control/ auxhist7_outname
NAMELIST /time_control/ auxhist8_outname
NAMELIST /time_control/ auxhist9_outname
NAMELIST /time_control/ auxhist10_outname
NAMELIST /time_control/ auxhist11_outname
NAMELIST /time_control/ history_inname
NAMELIST /time_control/ auxhist1_inname
NAMELIST /time_control/ auxhist2_inname
NAMELIST /time_control/ auxhist3_inname
NAMELIST /time_control/ auxhist4_inname
NAMELIST /time_control/ auxhist5_inname
NAMELIST /time_control/ auxhist6_inname
NAMELIST /time_control/ auxhist7_inname
NAMELIST /time_control/ auxhist8_inname
NAMELIST /time_control/ auxhist9_inname
NAMELIST /time_control/ auxhist10_inname
NAMELIST /time_control/ auxhist11_inname
NAMELIST /time_control/ auxinput1_outname
NAMELIST /time_control/ auxinput2_outname
NAMELIST /time_control/ auxinput3_outname
NAMELIST /time_control/ auxinput4_outname
NAMELIST /time_control/ auxinput5_outname
NAMELIST /time_control/ auxinput6_outname
NAMELIST /time_control/ auxinput7_outname
NAMELIST /time_control/ auxinput8_outname
NAMELIST /time_control/ auxinput9_outname
NAMELIST /time_control/ auxinput10_outname
NAMELIST /time_control/ auxinput11_outname
NAMELIST /time_control/ auxinput1_inname
NAMELIST /time_control/ auxinput2_inname
NAMELIST /time_control/ auxinput3_inname
NAMELIST /time_control/ auxinput4_inname
NAMELIST /time_control/ auxinput5_inname
NAMELIST /time_control/ auxinput6_inname
NAMELIST /time_control/ auxinput7_inname
NAMELIST /time_control/ auxinput8_inname
NAMELIST /fdda/ sgfdda_inname
NAMELIST /fdda/ gfdda_inname
NAMELIST /time_control/ auxinput11_inname
NAMELIST /time_control/ history_interval_mo
NAMELIST /time_control/ history_interval_d
NAMELIST /time_control/ history_interval_h
NAMELIST /time_control/ history_interval_m
NAMELIST /time_control/ history_interval_s
NAMELIST /time_control/ inputout_interval_mo
NAMELIST /time_control/ inputout_interval_d
NAMELIST /time_control/ inputout_interval_h
NAMELIST /time_control/ inputout_interval_m
NAMELIST /time_control/ inputout_interval_s
NAMELIST /time_control/ inputout_interval
NAMELIST /time_control/ auxhist1_interval_mo
NAMELIST /time_control/ auxhist1_interval_d
NAMELIST /time_control/ auxhist1_interval_h
NAMELIST /time_control/ auxhist1_interval_m
NAMELIST /time_control/ auxhist1_interval_s
NAMELIST /time_control/ auxhist1_interval
NAMELIST /time_control/ auxhist2_interval_mo
NAMELIST /time_control/ auxhist2_interval_d
NAMELIST /time_control/ auxhist2_interval_h
NAMELIST /time_control/ auxhist2_interval_m
NAMELIST /time_control/ auxhist2_interval_s
NAMELIST /time_control/ auxhist2_interval
NAMELIST /time_control/ auxhist3_interval_mo
NAMELIST /time_control/ auxhist3_interval_d
NAMELIST /time_control/ auxhist3_interval_h
NAMELIST /time_control/ auxhist3_interval_m
NAMELIST /time_control/ auxhist3_interval_s
NAMELIST /time_control/ auxhist3_interval
NAMELIST /time_control/ auxhist4_interval_mo
NAMELIST /time_control/ auxhist4_interval_d
NAMELIST /time_control/ auxhist4_interval_h
NAMELIST /time_control/ auxhist4_interval_m
NAMELIST /time_control/ auxhist4_interval_s
NAMELIST /time_control/ auxhist4_interval
NAMELIST /time_control/ auxhist5_interval_mo
NAMELIST /time_control/ auxhist5_interval_d
NAMELIST /time_control/ auxhist5_interval_h
NAMELIST /time_control/ auxhist5_interval_m
NAMELIST /time_control/ auxhist5_interval_s
NAMELIST /time_control/ auxhist5_interval
NAMELIST /time_control/ auxhist6_interval_mo
NAMELIST /time_control/ auxhist6_interval_d
NAMELIST /time_control/ auxhist6_interval_h
NAMELIST /time_control/ auxhist6_interval_m
NAMELIST /time_control/ auxhist6_interval_s
NAMELIST /time_control/ auxhist6_interval
NAMELIST /time_control/ auxhist7_interval_mo
NAMELIST /time_control/ auxhist7_interval_d
NAMELIST /time_control/ auxhist7_interval_h
NAMELIST /time_control/ auxhist7_interval_m
NAMELIST /time_control/ auxhist7_interval_s
NAMELIST /time_control/ auxhist7_interval
NAMELIST /time_control/ auxhist8_interval_mo
NAMELIST /time_control/ auxhist8_interval_d
NAMELIST /time_control/ auxhist8_interval_h
NAMELIST /time_control/ auxhist8_interval_m
NAMELIST /time_control/ auxhist8_interval_s
NAMELIST /time_control/ auxhist8_interval
NAMELIST /time_control/ auxhist9_interval_mo
NAMELIST /time_control/ auxhist9_interval_d
NAMELIST /time_control/ auxhist9_interval_h
NAMELIST /time_control/ auxhist9_interval_m
NAMELIST /time_control/ auxhist9_interval_s
NAMELIST /time_control/ auxhist9_interval
NAMELIST /time_control/ auxhist10_interval_mo
NAMELIST /time_control/ auxhist10_interval_d
NAMELIST /time_control/ auxhist10_interval_h
NAMELIST /time_control/ auxhist10_interval_m
NAMELIST /time_control/ auxhist10_interval_s
NAMELIST /time_control/ auxhist10_interval
NAMELIST /time_control/ auxhist11_interval_mo
NAMELIST /time_control/ auxhist11_interval_d
NAMELIST /time_control/ auxhist11_interval_h
NAMELIST /time_control/ auxhist11_interval_m
NAMELIST /time_control/ auxhist11_interval_s
NAMELIST /time_control/ auxhist11_interval
NAMELIST /time_control/ auxinput1_interval_mo
NAMELIST /time_control/ auxinput1_interval_d
NAMELIST /time_control/ auxinput1_interval_h
NAMELIST /time_control/ auxinput1_interval_m
NAMELIST /time_control/ auxinput1_interval_s
NAMELIST /time_control/ auxinput1_interval
NAMELIST /time_control/ auxinput2_interval_mo
NAMELIST /time_control/ auxinput2_interval_d
NAMELIST /time_control/ auxinput2_interval_h
NAMELIST /time_control/ auxinput2_interval_m
NAMELIST /time_control/ auxinput2_interval_s
NAMELIST /time_control/ auxinput2_interval
NAMELIST /time_control/ auxinput3_interval_mo
NAMELIST /time_control/ auxinput3_interval_d
NAMELIST /time_control/ auxinput3_interval_h
NAMELIST /time_control/ auxinput3_interval_m
NAMELIST /time_control/ auxinput3_interval_s
NAMELIST /time_control/ auxinput3_interval
NAMELIST /time_control/ auxinput4_interval_mo
NAMELIST /time_control/ auxinput4_interval_d
NAMELIST /time_control/ auxinput4_interval_h
NAMELIST /time_control/ auxinput4_interval_m
NAMELIST /time_control/ auxinput4_interval_s
NAMELIST /time_control/ auxinput4_interval
NAMELIST /time_control/ auxinput5_interval_mo
NAMELIST /time_control/ auxinput5_interval_d
NAMELIST /time_control/ auxinput5_interval_h
NAMELIST /time_control/ auxinput5_interval_m
NAMELIST /time_control/ auxinput5_interval_s
NAMELIST /time_control/ auxinput5_interval
NAMELIST /time_control/ auxinput6_interval_mo
NAMELIST /time_control/ auxinput6_interval_d
NAMELIST /time_control/ auxinput6_interval_h
NAMELIST /time_control/ auxinput6_interval_m
NAMELIST /time_control/ auxinput6_interval_s
NAMELIST /time_control/ auxinput6_interval
NAMELIST /time_control/ auxinput7_interval_mo
NAMELIST /time_control/ auxinput7_interval_d
NAMELIST /time_control/ auxinput7_interval_h
NAMELIST /time_control/ auxinput7_interval_m
NAMELIST /time_control/ auxinput7_interval_s
NAMELIST /time_control/ auxinput7_interval
NAMELIST /time_control/ auxinput8_interval_mo
NAMELIST /time_control/ auxinput8_interval_d
NAMELIST /time_control/ auxinput8_interval_h
NAMELIST /time_control/ auxinput8_interval_m
NAMELIST /time_control/ auxinput8_interval_s
NAMELIST /time_control/ auxinput8_interval
NAMELIST /fdda/ sgfdda_interval_mo
NAMELIST /fdda/ sgfdda_interval_d
NAMELIST /fdda/ sgfdda_interval_h
NAMELIST /fdda/ sgfdda_interval_m
NAMELIST /fdda/ sgfdda_interval_s
NAMELIST /fdda/ sgfdda_interval
NAMELIST /fdda/ gfdda_interval_mo
NAMELIST /fdda/ gfdda_interval_d
NAMELIST /fdda/ gfdda_interval_h
NAMELIST /fdda/ gfdda_interval_m
NAMELIST /fdda/ gfdda_interval_s
NAMELIST /fdda/ gfdda_interval
NAMELIST /time_control/ auxinput11_interval_mo
NAMELIST /time_control/ auxinput11_interval_d
NAMELIST /time_control/ auxinput11_interval_h
NAMELIST /time_control/ auxinput11_interval_m
NAMELIST /time_control/ auxinput11_interval_s
NAMELIST /time_control/ auxinput11_interval
NAMELIST /time_control/ restart_interval_mo
NAMELIST /time_control/ restart_interval_d
NAMELIST /time_control/ restart_interval_h
NAMELIST /time_control/ restart_interval_m
NAMELIST /time_control/ restart_interval_s
NAMELIST /time_control/ history_begin_y
NAMELIST /time_control/ history_begin_mo
NAMELIST /time_control/ history_begin_d
NAMELIST /time_control/ history_begin_h
NAMELIST /time_control/ history_begin_m
NAMELIST /time_control/ history_begin_s
NAMELIST /time_control/ inputout_begin_y
NAMELIST /time_control/ inputout_begin_mo
NAMELIST /time_control/ inputout_begin_d
NAMELIST /time_control/ inputout_begin_h
NAMELIST /time_control/ inputout_begin_m
NAMELIST /time_control/ inputout_begin_s
NAMELIST /time_control/ auxhist1_begin_y
NAMELIST /time_control/ auxhist1_begin_mo
NAMELIST /time_control/ auxhist1_begin_d
NAMELIST /time_control/ auxhist1_begin_h
NAMELIST /time_control/ auxhist1_begin_m
NAMELIST /time_control/ auxhist1_begin_s
NAMELIST /time_control/ auxhist2_begin_y
NAMELIST /time_control/ auxhist2_begin_mo
NAMELIST /time_control/ auxhist2_begin_d
NAMELIST /time_control/ auxhist2_begin_h
NAMELIST /time_control/ auxhist2_begin_m
NAMELIST /time_control/ auxhist2_begin_s
NAMELIST /time_control/ auxhist3_begin_y
NAMELIST /time_control/ auxhist3_begin_mo
NAMELIST /time_control/ auxhist3_begin_d
NAMELIST /time_control/ auxhist3_begin_h
NAMELIST /time_control/ auxhist3_begin_m
NAMELIST /time_control/ auxhist3_begin_s
NAMELIST /time_control/ auxhist4_begin_y
NAMELIST /time_control/ auxhist4_begin_mo
NAMELIST /time_control/ auxhist4_begin_d
NAMELIST /time_control/ auxhist4_begin_h
NAMELIST /time_control/ auxhist4_begin_m
NAMELIST /time_control/ auxhist4_begin_s
NAMELIST /time_control/ auxhist5_begin_y
NAMELIST /time_control/ auxhist5_begin_mo
NAMELIST /time_control/ auxhist5_begin_d
NAMELIST /time_control/ auxhist5_begin_h
NAMELIST /time_control/ auxhist5_begin_m
NAMELIST /time_control/ auxhist5_begin_s
NAMELIST /time_control/ auxhist6_begin_y
NAMELIST /time_control/ auxhist6_begin_mo
NAMELIST /time_control/ auxhist6_begin_d
NAMELIST /time_control/ auxhist6_begin_h
NAMELIST /time_control/ auxhist6_begin_m
NAMELIST /time_control/ auxhist6_begin_s
NAMELIST /time_control/ auxhist7_begin_y
NAMELIST /time_control/ auxhist7_begin_mo
NAMELIST /time_control/ auxhist7_begin_d
NAMELIST /time_control/ auxhist7_begin_h
NAMELIST /time_control/ auxhist7_begin_m
NAMELIST /time_control/ auxhist7_begin_s
NAMELIST /time_control/ auxhist8_begin_y
NAMELIST /time_control/ auxhist8_begin_mo
NAMELIST /time_control/ auxhist8_begin_d
NAMELIST /time_control/ auxhist8_begin_h
NAMELIST /time_control/ auxhist8_begin_m
NAMELIST /time_control/ auxhist8_begin_s
NAMELIST /time_control/ auxhist9_begin_y
NAMELIST /time_control/ auxhist9_begin_mo
NAMELIST /time_control/ auxhist9_begin_d
NAMELIST /time_control/ auxhist9_begin_h
NAMELIST /time_control/ auxhist9_begin_m
NAMELIST /time_control/ auxhist9_begin_s
NAMELIST /time_control/ auxhist10_begin_y
NAMELIST /time_control/ auxhist10_begin_mo
NAMELIST /time_control/ auxhist10_begin_d
NAMELIST /time_control/ auxhist10_begin_h
NAMELIST /time_control/ auxhist10_begin_m
NAMELIST /time_control/ auxhist10_begin_s
NAMELIST /time_control/ auxhist11_begin_y
NAMELIST /time_control/ auxhist11_begin_mo
NAMELIST /time_control/ auxhist11_begin_d
NAMELIST /time_control/ auxhist11_begin_h
NAMELIST /time_control/ auxhist11_begin_m
NAMELIST /time_control/ auxhist11_begin_s
NAMELIST /time_control/ auxinput1_begin_y
NAMELIST /time_control/ auxinput1_begin_mo
NAMELIST /time_control/ auxinput1_begin_d
NAMELIST /time_control/ auxinput1_begin_h
NAMELIST /time_control/ auxinput1_begin_m
NAMELIST /time_control/ auxinput1_begin_s
NAMELIST /time_control/ auxinput2_begin_y
NAMELIST /time_control/ auxinput2_begin_mo
NAMELIST /time_control/ auxinput2_begin_d
NAMELIST /time_control/ auxinput2_begin_h
NAMELIST /time_control/ auxinput2_begin_m
NAMELIST /time_control/ auxinput2_begin_s
NAMELIST /time_control/ auxinput3_begin_y
NAMELIST /time_control/ auxinput3_begin_mo
NAMELIST /time_control/ auxinput3_begin_d
NAMELIST /time_control/ auxinput3_begin_h
NAMELIST /time_control/ auxinput3_begin_m
NAMELIST /time_control/ auxinput3_begin_s
NAMELIST /time_control/ auxinput4_begin_y
NAMELIST /time_control/ auxinput4_begin_mo
NAMELIST /time_control/ auxinput4_begin_d
NAMELIST /time_control/ auxinput4_begin_h
NAMELIST /time_control/ auxinput4_begin_m
NAMELIST /time_control/ auxinput4_begin_s
NAMELIST /time_control/ auxinput5_begin_y
NAMELIST /time_control/ auxinput5_begin_mo
NAMELIST /time_control/ auxinput5_begin_d
NAMELIST /time_control/ auxinput5_begin_h
NAMELIST /time_control/ auxinput5_begin_m
NAMELIST /time_control/ auxinput5_begin_s
NAMELIST /time_control/ auxinput6_begin_y
NAMELIST /time_control/ auxinput6_begin_mo
NAMELIST /time_control/ auxinput6_begin_d
NAMELIST /time_control/ auxinput6_begin_h
NAMELIST /time_control/ auxinput6_begin_m
NAMELIST /time_control/ auxinput6_begin_s
NAMELIST /time_control/ auxinput7_begin_y
NAMELIST /time_control/ auxinput7_begin_mo
NAMELIST /time_control/ auxinput7_begin_d
NAMELIST /time_control/ auxinput7_begin_h
NAMELIST /time_control/ auxinput7_begin_m
NAMELIST /time_control/ auxinput7_begin_s
NAMELIST /time_control/ auxinput8_begin_y
NAMELIST /time_control/ auxinput8_begin_mo
NAMELIST /time_control/ auxinput8_begin_d
NAMELIST /time_control/ auxinput8_begin_h
NAMELIST /time_control/ auxinput8_begin_m
NAMELIST /time_control/ auxinput8_begin_s
NAMELIST /fdda/ sgfdda_begin_y
NAMELIST /fdda/ sgfdda_begin_mo
NAMELIST /fdda/ sgfdda_begin_d
NAMELIST /fdda/ sgfdda_begin_h
NAMELIST /fdda/ sgfdda_begin_m
NAMELIST /fdda/ sgfdda_begin_s
NAMELIST /fdda/ gfdda_begin_y
NAMELIST /fdda/ gfdda_begin_mo
NAMELIST /fdda/ gfdda_begin_d
NAMELIST /fdda/ gfdda_begin_h
NAMELIST /fdda/ gfdda_begin_m
NAMELIST /fdda/ gfdda_begin_s
NAMELIST /time_control/ auxinput11_begin_y
NAMELIST /time_control/ auxinput11_begin_mo
NAMELIST /time_control/ auxinput11_begin_d
NAMELIST /time_control/ auxinput11_begin_h
NAMELIST /time_control/ auxinput11_begin_m
NAMELIST /time_control/ auxinput11_begin_s
NAMELIST /time_control/ restart_begin_y
NAMELIST /time_control/ restart_begin_mo
NAMELIST /time_control/ restart_begin_d
NAMELIST /time_control/ restart_begin_h
NAMELIST /time_control/ restart_begin_m
NAMELIST /time_control/ restart_begin_s
NAMELIST /time_control/ history_end_y
NAMELIST /time_control/ history_end_mo
NAMELIST /time_control/ history_end_d
NAMELIST /time_control/ history_end_h
NAMELIST /time_control/ history_end_m
NAMELIST /time_control/ history_end_s
NAMELIST /time_control/ inputout_end_y
NAMELIST /time_control/ inputout_end_mo
NAMELIST /time_control/ inputout_end_d
NAMELIST /time_control/ inputout_end_h
NAMELIST /time_control/ inputout_end_m
NAMELIST /time_control/ inputout_end_s
NAMELIST /time_control/ auxhist1_end_y
NAMELIST /time_control/ auxhist1_end_mo
NAMELIST /time_control/ auxhist1_end_d
NAMELIST /time_control/ auxhist1_end_h
NAMELIST /time_control/ auxhist1_end_m
NAMELIST /time_control/ auxhist1_end_s
NAMELIST /time_control/ auxhist2_end_y
NAMELIST /time_control/ auxhist2_end_mo
NAMELIST /time_control/ auxhist2_end_d
NAMELIST /time_control/ auxhist2_end_h
NAMELIST /time_control/ auxhist2_end_m
NAMELIST /time_control/ auxhist2_end_s
NAMELIST /time_control/ auxhist3_end_y
NAMELIST /time_control/ auxhist3_end_mo
NAMELIST /time_control/ auxhist3_end_d
NAMELIST /time_control/ auxhist3_end_h
NAMELIST /time_control/ auxhist3_end_m
NAMELIST /time_control/ auxhist3_end_s
NAMELIST /time_control/ auxhist4_end_y
NAMELIST /time_control/ auxhist4_end_mo
NAMELIST /time_control/ auxhist4_end_d
NAMELIST /time_control/ auxhist4_end_h
NAMELIST /time_control/ auxhist4_end_m
NAMELIST /time_control/ auxhist4_end_s
NAMELIST /time_control/ auxhist5_end_y
NAMELIST /time_control/ auxhist5_end_mo
NAMELIST /time_control/ auxhist5_end_d
NAMELIST /time_control/ auxhist5_end_h
NAMELIST /time_control/ auxhist5_end_m
NAMELIST /time_control/ auxhist5_end_s
NAMELIST /time_control/ auxhist6_end_y
NAMELIST /time_control/ auxhist6_end_mo
NAMELIST /time_control/ auxhist6_end_d
NAMELIST /time_control/ auxhist6_end_h
NAMELIST /time_control/ auxhist6_end_m
NAMELIST /time_control/ auxhist6_end_s
NAMELIST /time_control/ auxhist7_end_y
NAMELIST /time_control/ auxhist7_end_mo
NAMELIST /time_control/ auxhist7_end_d
NAMELIST /time_control/ auxhist7_end_h
NAMELIST /time_control/ auxhist7_end_m
NAMELIST /time_control/ auxhist7_end_s
NAMELIST /time_control/ auxhist8_end_y
NAMELIST /time_control/ auxhist8_end_mo
NAMELIST /time_control/ auxhist8_end_d
NAMELIST /time_control/ auxhist8_end_h
NAMELIST /time_control/ auxhist8_end_m
NAMELIST /time_control/ auxhist8_end_s
NAMELIST /time_control/ auxhist9_end_y
NAMELIST /time_control/ auxhist9_end_mo
NAMELIST /time_control/ auxhist9_end_d
NAMELIST /time_control/ auxhist9_end_h
NAMELIST /time_control/ auxhist9_end_m
NAMELIST /time_control/ auxhist9_end_s
NAMELIST /time_control/ auxhist10_end_y
NAMELIST /time_control/ auxhist10_end_mo
NAMELIST /time_control/ auxhist10_end_d
NAMELIST /time_control/ auxhist10_end_h
NAMELIST /time_control/ auxhist10_end_m
NAMELIST /time_control/ auxhist10_end_s
NAMELIST /time_control/ auxhist11_end_y
NAMELIST /time_control/ auxhist11_end_mo
NAMELIST /time_control/ auxhist11_end_d
NAMELIST /time_control/ auxhist11_end_h
NAMELIST /time_control/ auxhist11_end_m
NAMELIST /time_control/ auxhist11_end_s
NAMELIST /time_control/ auxinput1_end_y
NAMELIST /time_control/ auxinput1_end_mo
NAMELIST /time_control/ auxinput1_end_d
NAMELIST /time_control/ auxinput1_end_h
NAMELIST /time_control/ auxinput1_end_m
NAMELIST /time_control/ auxinput1_end_s
NAMELIST /time_control/ auxinput2_end_y
NAMELIST /time_control/ auxinput2_end_mo
NAMELIST /time_control/ auxinput2_end_d
NAMELIST /time_control/ auxinput2_end_h
NAMELIST /time_control/ auxinput2_end_m
NAMELIST /time_control/ auxinput2_end_s
NAMELIST /time_control/ auxinput3_end_y
NAMELIST /time_control/ auxinput3_end_mo
NAMELIST /time_control/ auxinput3_end_d
NAMELIST /time_control/ auxinput3_end_h
NAMELIST /time_control/ auxinput3_end_m
NAMELIST /time_control/ auxinput3_end_s
NAMELIST /time_control/ auxinput4_end_y
NAMELIST /time_control/ auxinput4_end_mo
NAMELIST /time_control/ auxinput4_end_d
NAMELIST /time_control/ auxinput4_end_h
NAMELIST /time_control/ auxinput4_end_m
NAMELIST /time_control/ auxinput4_end_s
NAMELIST /time_control/ auxinput5_end_y
NAMELIST /time_control/ auxinput5_end_mo
NAMELIST /time_control/ auxinput5_end_d
NAMELIST /time_control/ auxinput5_end_h
NAMELIST /time_control/ auxinput5_end_m
NAMELIST /time_control/ auxinput5_end_s
NAMELIST /time_control/ auxinput6_end_y
NAMELIST /time_control/ auxinput6_end_mo
NAMELIST /time_control/ auxinput6_end_d
NAMELIST /time_control/ auxinput6_end_h
NAMELIST /time_control/ auxinput6_end_m
NAMELIST /time_control/ auxinput6_end_s
NAMELIST /time_control/ auxinput7_end_y
NAMELIST /time_control/ auxinput7_end_mo
NAMELIST /time_control/ auxinput7_end_d
NAMELIST /time_control/ auxinput7_end_h
NAMELIST /time_control/ auxinput7_end_m
NAMELIST /time_control/ auxinput7_end_s
NAMELIST /time_control/ auxinput8_end_y
NAMELIST /time_control/ auxinput8_end_mo
NAMELIST /time_control/ auxinput8_end_d
NAMELIST /time_control/ auxinput8_end_h
NAMELIST /time_control/ auxinput8_end_m
NAMELIST /time_control/ auxinput8_end_s
NAMELIST /fdda/ sgfdda_end_y
NAMELIST /fdda/ sgfdda_end_mo
NAMELIST /fdda/ sgfdda_end_d
NAMELIST /fdda/ sgfdda_end_h
NAMELIST /fdda/ sgfdda_end_m
NAMELIST /fdda/ sgfdda_end_s
NAMELIST /fdda/ gfdda_end_y
NAMELIST /fdda/ gfdda_end_mo
NAMELIST /fdda/ gfdda_end_d
NAMELIST /fdda/ gfdda_end_h
NAMELIST /fdda/ gfdda_end_m
NAMELIST /fdda/ gfdda_end_s
NAMELIST /time_control/ auxinput11_end_y
NAMELIST /time_control/ auxinput11_end_mo
NAMELIST /time_control/ auxinput11_end_d
NAMELIST /time_control/ auxinput11_end_h
NAMELIST /time_control/ auxinput11_end_m
NAMELIST /time_control/ auxinput11_end_s
NAMELIST /time_control/ io_form_auxinput1
NAMELIST /time_control/ io_form_auxinput2
NAMELIST /time_control/ io_form_auxinput3
NAMELIST /time_control/ io_form_auxinput4
NAMELIST /time_control/ io_form_auxinput5
NAMELIST /time_control/ io_form_auxinput6
NAMELIST /time_control/ io_form_auxinput7
NAMELIST /time_control/ io_form_auxinput8
NAMELIST /fdda/ io_form_sgfdda
NAMELIST /fdda/ io_form_gfdda
NAMELIST /time_control/ io_form_auxinput11
NAMELIST /time_control/ io_form_auxhist1
NAMELIST /time_control/ io_form_auxhist2
NAMELIST /time_control/ io_form_auxhist3
NAMELIST /time_control/ io_form_auxhist4
NAMELIST /time_control/ io_form_auxhist5
NAMELIST /time_control/ io_form_auxhist6
NAMELIST /time_control/ io_form_auxhist7
NAMELIST /time_control/ io_form_auxhist8
NAMELIST /time_control/ io_form_auxhist9
NAMELIST /time_control/ io_form_auxhist10
NAMELIST /time_control/ io_form_auxhist11
NAMELIST /time_control/ reset_simulation_start
NAMELIST /domains/ sr_x
NAMELIST /domains/ sr_y
NAMELIST /time_control/ julyr
NAMELIST /time_control/ julday
NAMELIST /time_control/ gmt
NAMELIST /time_control/ input_inname
NAMELIST /time_control/ input_outname
NAMELIST /time_control/ bdy_inname
NAMELIST /time_control/ bdy_outname
NAMELIST /time_control/ rst_inname
NAMELIST /time_control/ rst_outname
NAMELIST /time_control/ write_input
NAMELIST /time_control/ write_restart_at_0h
NAMELIST /time_control/ adjust_output_times
NAMELIST /time_control/ adjust_input_times
NAMELIST /time_control/ tstart
NAMELIST /time_control/ nocolons
NAMELIST /time_control/ cycling
NAMELIST /dfi_control/ dfi_opt
NAMELIST /dfi_control/ dfi_nfilter
NAMELIST /dfi_control/ dfi_write_filtered_input
NAMELIST /dfi_control/ dfi_write_dfi_history
NAMELIST /dfi_control/ dfi_cutoff_seconds
NAMELIST /dfi_control/ dfi_time_dim
NAMELIST /dfi_control/ dfi_fwdstop_year
NAMELIST /dfi_control/ dfi_fwdstop_month
NAMELIST /dfi_control/ dfi_fwdstop_day
NAMELIST /dfi_control/ dfi_fwdstop_hour
NAMELIST /dfi_control/ dfi_fwdstop_minute
NAMELIST /dfi_control/ dfi_fwdstop_second
NAMELIST /dfi_control/ dfi_bckstop_year
NAMELIST /dfi_control/ dfi_bckstop_month
NAMELIST /dfi_control/ dfi_bckstop_day
NAMELIST /dfi_control/ dfi_bckstop_hour
NAMELIST /dfi_control/ dfi_bckstop_minute
NAMELIST /dfi_control/ dfi_bckstop_second
NAMELIST /domains/ time_step
NAMELIST /domains/ time_step_fract_num
NAMELIST /domains/ time_step_fract_den
NAMELIST /domains/ max_dom
NAMELIST /domains/ s_we
NAMELIST /domains/ e_we
NAMELIST /domains/ s_sn
NAMELIST /domains/ e_sn
NAMELIST /domains/ s_vert
NAMELIST /domains/ e_vert
NAMELIST /domains/ dx
NAMELIST /domains/ dy
NAMELIST /domains/ grid_id
NAMELIST /domains/ grid_allowed
NAMELIST /domains/ parent_id
NAMELIST /domains/ i_parent_start
NAMELIST /domains/ j_parent_start
NAMELIST /domains/ parent_grid_ratio
NAMELIST /domains/ parent_time_step_ratio
NAMELIST /domains/ feedback
NAMELIST /domains/ smooth_option
NAMELIST /domains/ ztop
NAMELIST /domains/ moad_grid_ratio
NAMELIST /domains/ moad_time_step_ratio
NAMELIST /domains/ shw
NAMELIST /domains/ tile_sz_x
NAMELIST /domains/ tile_sz_y
NAMELIST /domains/ numtiles
NAMELIST /domains/ nproc_x
NAMELIST /domains/ nproc_y
NAMELIST /domains/ irand
NAMELIST /domains/ ts_buf_size
NAMELIST /domains/ max_ts_locs
NAMELIST /domains/ num_moves
NAMELIST /domains/ move_id
NAMELIST /domains/ move_interval
NAMELIST /domains/ move_cd_x
NAMELIST /domains/ move_cd_y
NAMELIST /domains/ swap_x
NAMELIST /domains/ swap_y
NAMELIST /domains/ cycle_x
NAMELIST /domains/ cycle_y
NAMELIST /domains/ reorder_mesh
NAMELIST /domains/ perturb_input
NAMELIST /domains/ eta_levels
NAMELIST /domains/ ptsgm
NAMELIST /domains/ num_metgrid_levels
NAMELIST /domains/ p_top_requested
NAMELIST /physics/ mp_physics
NAMELIST /physics/ ra_lw_physics
NAMELIST /physics/ ra_sw_physics
NAMELIST /physics/ radt
NAMELIST /physics/ sf_sfclay_physics
NAMELIST /physics/ sf_surface_physics
NAMELIST /physics/ bl_pbl_physics
NAMELIST /physics/ sf_urban_physics
NAMELIST /physics/ bldt
NAMELIST /physics/ cu_physics
NAMELIST /physics/ cudt
NAMELIST /physics/ gsmdt
NAMELIST /physics/ isfflx
NAMELIST /physics/ ifsnow
NAMELIST /physics/ icloud
NAMELIST /physics/ swrad_scat
NAMELIST /physics/ surface_input_source
NAMELIST /physics/ num_soil_layers
NAMELIST /physics/ num_urban_layers
NAMELIST /physics/ maxiens
NAMELIST /physics/ maxens
NAMELIST /physics/ maxens2
NAMELIST /physics/ maxens3
NAMELIST /physics/ ensdim
NAMELIST /physics/ chem_opt
NAMELIST /physics/ num_land_cat
NAMELIST /physics/ num_soil_cat
NAMELIST /physics/ mp_zero_out
NAMELIST /physics/ mp_zero_out_thresh
NAMELIST /physics/ seaice_threshold
NAMELIST /physics/ fractional_seaice
NAMELIST /physics/ sst_update
NAMELIST /physics/ usemonalb
NAMELIST /physics/ rdmaxalb
NAMELIST /physics/ rdlai2d
NAMELIST /physics/ gwd_opt
NAMELIST /physics/ idtad
NAMELIST /physics/ nsoil
NAMELIST /physics/ nphs
NAMELIST /physics/ ncnvc
NAMELIST /physics/ nrads
NAMELIST /physics/ nradl
NAMELIST /physics/ tprec
NAMELIST /physics/ theat
NAMELIST /physics/ tclod
NAMELIST /physics/ trdsw
NAMELIST /physics/ trdlw
NAMELIST /physics/ tsrfc
NAMELIST /physics/ pcpflg
NAMELIST /physics/ sigma
NAMELIST /physics/ sfenth
NAMELIST /physics/ co2tf
NAMELIST /physics/ ra_call_offset
NAMELIST /physics/ cam_abs_freq_s
NAMELIST /physics/ levsiz
NAMELIST /physics/ paerlev
NAMELIST /physics/ cam_abs_dim1
NAMELIST /physics/ cam_abs_dim2
NAMELIST /physics/ cu_rad_feedback
NAMELIST /dynamics/ dyn_opt
NAMELIST /dynamics/ rk_ord
NAMELIST /dynamics/ w_damping
NAMELIST /dynamics/ diff_opt
NAMELIST /dynamics/ km_opt
NAMELIST /dynamics/ damp_opt
NAMELIST /dynamics/ zdamp
NAMELIST /dynamics/ base_pres
NAMELIST /dynamics/ base_temp
NAMELIST /dynamics/ base_lapse
NAMELIST /dynamics/ iso_temp
NAMELIST /dynamics/ dampcoef
NAMELIST /dynamics/ khdif
NAMELIST /dynamics/ kvdif
NAMELIST /dynamics/ c_s
NAMELIST /dynamics/ c_k
NAMELIST /dynamics/ smdiv
NAMELIST /dynamics/ emdiv
NAMELIST /dynamics/ epssm
NAMELIST /dynamics/ non_hydrostatic
NAMELIST /dynamics/ time_step_sound
NAMELIST /dynamics/ h_mom_adv_order
NAMELIST /dynamics/ v_mom_adv_order
NAMELIST /dynamics/ h_sca_adv_order
NAMELIST /dynamics/ v_sca_adv_order
NAMELIST /dynamics/ top_radiation
NAMELIST /dynamics/ tke_upper_bound
NAMELIST /dynamics/ tke_drag_coefficient
NAMELIST /dynamics/ tke_heat_flux
NAMELIST /dynamics/ pert_coriolis
NAMELIST /dynamics/ euler_adv
NAMELIST /dynamics/ idtadt
NAMELIST /dynamics/ idtadc
NAMELIST /dynamics/ boundary_flux
NAMELIST /bdy_control/ spec_bdy_width
NAMELIST /bdy_control/ spec_zone
NAMELIST /bdy_control/ relax_zone
NAMELIST /bdy_control/ specified
NAMELIST /bdy_control/ periodic_x
NAMELIST /bdy_control/ symmetric_xs
NAMELIST /bdy_control/ symmetric_xe
NAMELIST /bdy_control/ open_xs
NAMELIST /bdy_control/ open_xe
NAMELIST /bdy_control/ periodic_y
NAMELIST /bdy_control/ symmetric_ys
NAMELIST /bdy_control/ symmetric_ye
NAMELIST /bdy_control/ open_ys
NAMELIST /bdy_control/ open_ye
NAMELIST /bdy_control/ polar
NAMELIST /bdy_control/ nested
NAMELIST /bdy_control/ real_data_init_type
NAMELIST /grib2/ background_proc_id
NAMELIST /grib2/ forecast_proc_id
NAMELIST /grib2/ production_status
NAMELIST /grib2/ compression



     integer , DIMENSION(max_domains) :: ucmcall
     NAMELIST /physics/ ucmcall





     REWIND ( UNIT = nml_read_unit )
     READ   ( UNIT = nml_read_unit , NML = physics , iostat=nml_error )

     IF ( nml_error .EQ. 0 ) then    
        CALL wrf_debug(0,"Is ucmcall still in your "// TRIM(nml_name)//" namelist?")
        CALL wrf_error_fatal3("",9524,&
"Replace it with sf_urban_physics")
     ELSE     
        return
     ENDIF

   END SUBROUTINE wrf_alt_nml_physics

   SUBROUTINE wrf_alt_nml_fdda (nml_read_unit, nml_name)







     IMPLICIT NONE
     INTEGER, INTENT(IN)       :: nml_read_unit
     CHARACTER*(*), INTENT(IN) :: nml_name
     INTEGER                   :: nml_error







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
character*256 :: auxinput1_inname
character*256 :: auxinput2_inname
character*256 :: auxinput3_inname
character*256 :: auxinput4_inname
character*256 :: auxinput5_inname
character*256 :: auxinput6_inname
character*256 :: auxinput7_inname
character*256 :: auxinput8_inname
character*256 :: sgfdda_inname
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
integer , DIMENSION(max_domains) :: sgfdda_interval_mo
integer , DIMENSION(max_domains) :: sgfdda_interval_d
integer , DIMENSION(max_domains) :: sgfdda_interval_h
integer , DIMENSION(max_domains) :: sgfdda_interval_m
integer , DIMENSION(max_domains) :: sgfdda_interval_s
integer , DIMENSION(max_domains) :: sgfdda_interval
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
integer , DIMENSION(max_domains) :: sgfdda_begin_y
integer , DIMENSION(max_domains) :: sgfdda_begin_mo
integer , DIMENSION(max_domains) :: sgfdda_begin_d
integer , DIMENSION(max_domains) :: sgfdda_begin_h
integer , DIMENSION(max_domains) :: sgfdda_begin_m
integer , DIMENSION(max_domains) :: sgfdda_begin_s
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
integer , DIMENSION(max_domains) :: sgfdda_end_y
integer , DIMENSION(max_domains) :: sgfdda_end_mo
integer , DIMENSION(max_domains) :: sgfdda_end_d
integer , DIMENSION(max_domains) :: sgfdda_end_h
integer , DIMENSION(max_domains) :: sgfdda_end_m
integer , DIMENSION(max_domains) :: sgfdda_end_s
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
integer :: io_form_sgfdda
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
integer :: simulation_start_year
integer :: simulation_start_month
integer :: simulation_start_day
integer :: simulation_start_hour
integer :: simulation_start_minute
integer :: simulation_start_second
logical :: reset_simulation_start
integer , DIMENSION(max_domains) :: sr_x
integer , DIMENSION(max_domains) :: sr_y
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
real , DIMENSION(max_domains) :: tstart
logical :: nocolons
logical :: cycling
integer :: dfi_opt
integer :: dfi_nfilter
logical :: dfi_write_filtered_input
logical :: dfi_write_dfi_history
integer :: dfi_cutoff_seconds
integer :: dfi_time_dim
integer :: dfi_fwdstop_year
integer :: dfi_fwdstop_month
integer :: dfi_fwdstop_day
integer :: dfi_fwdstop_hour
integer :: dfi_fwdstop_minute
integer :: dfi_fwdstop_second
integer :: dfi_bckstop_year
integer :: dfi_bckstop_month
integer :: dfi_bckstop_day
integer :: dfi_bckstop_hour
integer :: dfi_bckstop_minute
integer :: dfi_bckstop_second
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
real , DIMENSION(max_domains) :: dx
real , DIMENSION(max_domains) :: dy
integer , DIMENSION(max_domains) :: grid_id
logical , DIMENSION(max_domains) :: grid_allowed
integer , DIMENSION(max_domains) :: parent_id
integer , DIMENSION(max_domains) :: i_parent_start
integer , DIMENSION(max_domains) :: j_parent_start
integer , DIMENSION(max_domains) :: parent_grid_ratio
integer , DIMENSION(max_domains) :: parent_time_step_ratio
integer :: feedback
integer :: smooth_option
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
integer :: ts_buf_size
integer :: max_ts_locs
integer :: num_moves
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
real :: ptsgm
integer :: num_metgrid_levels
real :: p_top_requested
integer , DIMENSION(max_domains) :: mp_physics
integer , DIMENSION(max_domains) :: ra_lw_physics
integer , DIMENSION(max_domains) :: ra_sw_physics
real , DIMENSION(max_domains) :: radt
integer , DIMENSION(max_domains) :: sf_sfclay_physics
integer , DIMENSION(max_domains) :: sf_surface_physics
integer , DIMENSION(max_domains) :: bl_pbl_physics
integer , DIMENSION(max_domains) :: sf_urban_physics
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
integer :: num_urban_layers
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
integer :: fractional_seaice
integer :: sst_update
logical :: usemonalb
logical :: rdmaxalb
logical :: rdlai2d
integer , DIMENSION(max_domains) :: gwd_opt
integer , DIMENSION(max_domains) :: idtad
integer , DIMENSION(max_domains) :: nsoil
integer , DIMENSION(max_domains) :: nphs
integer , DIMENSION(max_domains) :: ncnvc
integer , DIMENSION(max_domains) :: nrads
integer , DIMENSION(max_domains) :: nradl
real , DIMENSION(max_domains) :: tprec
real , DIMENSION(max_domains) :: theat
real , DIMENSION(max_domains) :: tclod
real , DIMENSION(max_domains) :: trdsw
real , DIMENSION(max_domains) :: trdlw
real , DIMENSION(max_domains) :: tsrfc
logical , DIMENSION(max_domains) :: pcpflg
integer , DIMENSION(max_domains) :: sigma
real , DIMENSION(max_domains) :: sfenth
integer :: co2tf
integer :: ra_call_offset
real :: cam_abs_freq_s
integer :: levsiz
integer :: paerlev
integer :: cam_abs_dim1
integer :: cam_abs_dim2
logical , DIMENSION(max_domains) :: cu_rad_feedback
integer :: dyn_opt
integer :: rk_ord
integer :: w_damping
integer :: diff_opt
integer :: km_opt
integer :: damp_opt
real , DIMENSION(max_domains) :: zdamp
real :: base_pres
real :: base_temp
real :: base_lapse
real :: iso_temp
real , DIMENSION(max_domains) :: dampcoef
real , DIMENSION(max_domains) :: khdif
real , DIMENSION(max_domains) :: kvdif
real , DIMENSION(max_domains) :: c_s
real , DIMENSION(max_domains) :: c_k
real , DIMENSION(max_domains) :: smdiv
real , DIMENSION(max_domains) :: emdiv
real , DIMENSION(max_domains) :: epssm
logical , DIMENSION(max_domains) :: non_hydrostatic
integer , DIMENSION(max_domains) :: time_step_sound
integer , DIMENSION(max_domains) :: h_mom_adv_order
integer , DIMENSION(max_domains) :: v_mom_adv_order
integer , DIMENSION(max_domains) :: h_sca_adv_order
integer , DIMENSION(max_domains) :: v_sca_adv_order
logical , DIMENSION(max_domains) :: top_radiation
real , DIMENSION(max_domains) :: tke_upper_bound
real , DIMENSION(max_domains) :: tke_drag_coefficient
real , DIMENSION(max_domains) :: tke_heat_flux
logical , DIMENSION(max_domains) :: pert_coriolis
logical :: euler_adv
integer :: idtadt
integer :: idtadc
logical :: boundary_flux
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
logical , DIMENSION(max_domains) :: polar
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
integer :: flag_metgrid
integer :: flag_snow
integer :: flag_psfc
integer :: flag_sm000010
integer :: flag_sm010040
integer :: flag_sm040100
integer :: flag_sm100200
integer :: flag_st000010
integer :: flag_st010040
integer :: flag_st040100
integer :: flag_st100200
integer :: flag_slp
integer :: flag_soilhgt
integer :: flag_mf_xy
real , DIMENSION(max_domains) :: bdyfrq
character*256 , DIMENSION(max_domains) :: mminlu
integer , DIMENSION(max_domains) :: iswater
integer , DIMENSION(max_domains) :: islake
integer , DIMENSION(max_domains) :: isice
integer , DIMENSION(max_domains) :: isurban
integer , DIMENSION(max_domains) :: isoilwater
integer , DIMENSION(max_domains) :: map_proj
integer :: dfi_stage
integer , DIMENSION(max_domains) :: mp_physics_dfi
integer    :: last_item_in_struct







NAMELIST /time_control/ run_days
NAMELIST /time_control/ run_hours
NAMELIST /time_control/ run_minutes
NAMELIST /time_control/ run_seconds
NAMELIST /time_control/ start_year
NAMELIST /time_control/ start_month
NAMELIST /time_control/ start_day
NAMELIST /time_control/ start_hour
NAMELIST /time_control/ start_minute
NAMELIST /time_control/ start_second
NAMELIST /time_control/ end_year
NAMELIST /time_control/ end_month
NAMELIST /time_control/ end_day
NAMELIST /time_control/ end_hour
NAMELIST /time_control/ end_minute
NAMELIST /time_control/ end_second
NAMELIST /time_control/ interval_seconds
NAMELIST /time_control/ input_from_file
NAMELIST /time_control/ fine_input_stream
NAMELIST /time_control/ history_interval
NAMELIST /time_control/ frames_per_outfile
NAMELIST /time_control/ frames_per_auxhist1
NAMELIST /time_control/ frames_per_auxhist2
NAMELIST /time_control/ frames_per_auxhist3
NAMELIST /time_control/ frames_per_auxhist4
NAMELIST /time_control/ frames_per_auxhist5
NAMELIST /time_control/ frames_per_auxhist6
NAMELIST /time_control/ frames_per_auxhist7
NAMELIST /time_control/ frames_per_auxhist8
NAMELIST /time_control/ frames_per_auxhist9
NAMELIST /time_control/ frames_per_auxhist10
NAMELIST /time_control/ frames_per_auxhist11
NAMELIST /time_control/ restart
NAMELIST /time_control/ restart_interval
NAMELIST /time_control/ io_form_input
NAMELIST /time_control/ io_form_history
NAMELIST /time_control/ io_form_restart
NAMELIST /time_control/ io_form_boundary
NAMELIST /time_control/ debug_level
NAMELIST /time_control/ self_test_domain
NAMELIST /time_control/ history_outname
NAMELIST /time_control/ auxhist1_outname
NAMELIST /time_control/ auxhist2_outname
NAMELIST /time_control/ auxhist3_outname
NAMELIST /time_control/ auxhist4_outname
NAMELIST /time_control/ auxhist5_outname
NAMELIST /time_control/ auxhist6_outname
NAMELIST /time_control/ auxhist7_outname
NAMELIST /time_control/ auxhist8_outname
NAMELIST /time_control/ auxhist9_outname
NAMELIST /time_control/ auxhist10_outname
NAMELIST /time_control/ auxhist11_outname
NAMELIST /time_control/ history_inname
NAMELIST /time_control/ auxhist1_inname
NAMELIST /time_control/ auxhist2_inname
NAMELIST /time_control/ auxhist3_inname
NAMELIST /time_control/ auxhist4_inname
NAMELIST /time_control/ auxhist5_inname
NAMELIST /time_control/ auxhist6_inname
NAMELIST /time_control/ auxhist7_inname
NAMELIST /time_control/ auxhist8_inname
NAMELIST /time_control/ auxhist9_inname
NAMELIST /time_control/ auxhist10_inname
NAMELIST /time_control/ auxhist11_inname
NAMELIST /time_control/ auxinput1_outname
NAMELIST /time_control/ auxinput2_outname
NAMELIST /time_control/ auxinput3_outname
NAMELIST /time_control/ auxinput4_outname
NAMELIST /time_control/ auxinput5_outname
NAMELIST /time_control/ auxinput6_outname
NAMELIST /time_control/ auxinput7_outname
NAMELIST /time_control/ auxinput8_outname
NAMELIST /time_control/ auxinput9_outname
NAMELIST /time_control/ auxinput10_outname
NAMELIST /time_control/ auxinput11_outname
NAMELIST /time_control/ auxinput1_inname
NAMELIST /time_control/ auxinput2_inname
NAMELIST /time_control/ auxinput3_inname
NAMELIST /time_control/ auxinput4_inname
NAMELIST /time_control/ auxinput5_inname
NAMELIST /time_control/ auxinput6_inname
NAMELIST /time_control/ auxinput7_inname
NAMELIST /time_control/ auxinput8_inname
NAMELIST /fdda/ sgfdda_inname
NAMELIST /fdda/ gfdda_inname
NAMELIST /time_control/ auxinput11_inname
NAMELIST /time_control/ history_interval_mo
NAMELIST /time_control/ history_interval_d
NAMELIST /time_control/ history_interval_h
NAMELIST /time_control/ history_interval_m
NAMELIST /time_control/ history_interval_s
NAMELIST /time_control/ inputout_interval_mo
NAMELIST /time_control/ inputout_interval_d
NAMELIST /time_control/ inputout_interval_h
NAMELIST /time_control/ inputout_interval_m
NAMELIST /time_control/ inputout_interval_s
NAMELIST /time_control/ inputout_interval
NAMELIST /time_control/ auxhist1_interval_mo
NAMELIST /time_control/ auxhist1_interval_d
NAMELIST /time_control/ auxhist1_interval_h
NAMELIST /time_control/ auxhist1_interval_m
NAMELIST /time_control/ auxhist1_interval_s
NAMELIST /time_control/ auxhist1_interval
NAMELIST /time_control/ auxhist2_interval_mo
NAMELIST /time_control/ auxhist2_interval_d
NAMELIST /time_control/ auxhist2_interval_h
NAMELIST /time_control/ auxhist2_interval_m
NAMELIST /time_control/ auxhist2_interval_s
NAMELIST /time_control/ auxhist2_interval
NAMELIST /time_control/ auxhist3_interval_mo
NAMELIST /time_control/ auxhist3_interval_d
NAMELIST /time_control/ auxhist3_interval_h
NAMELIST /time_control/ auxhist3_interval_m
NAMELIST /time_control/ auxhist3_interval_s
NAMELIST /time_control/ auxhist3_interval
NAMELIST /time_control/ auxhist4_interval_mo
NAMELIST /time_control/ auxhist4_interval_d
NAMELIST /time_control/ auxhist4_interval_h
NAMELIST /time_control/ auxhist4_interval_m
NAMELIST /time_control/ auxhist4_interval_s
NAMELIST /time_control/ auxhist4_interval
NAMELIST /time_control/ auxhist5_interval_mo
NAMELIST /time_control/ auxhist5_interval_d
NAMELIST /time_control/ auxhist5_interval_h
NAMELIST /time_control/ auxhist5_interval_m
NAMELIST /time_control/ auxhist5_interval_s
NAMELIST /time_control/ auxhist5_interval
NAMELIST /time_control/ auxhist6_interval_mo
NAMELIST /time_control/ auxhist6_interval_d
NAMELIST /time_control/ auxhist6_interval_h
NAMELIST /time_control/ auxhist6_interval_m
NAMELIST /time_control/ auxhist6_interval_s
NAMELIST /time_control/ auxhist6_interval
NAMELIST /time_control/ auxhist7_interval_mo
NAMELIST /time_control/ auxhist7_interval_d
NAMELIST /time_control/ auxhist7_interval_h
NAMELIST /time_control/ auxhist7_interval_m
NAMELIST /time_control/ auxhist7_interval_s
NAMELIST /time_control/ auxhist7_interval
NAMELIST /time_control/ auxhist8_interval_mo
NAMELIST /time_control/ auxhist8_interval_d
NAMELIST /time_control/ auxhist8_interval_h
NAMELIST /time_control/ auxhist8_interval_m
NAMELIST /time_control/ auxhist8_interval_s
NAMELIST /time_control/ auxhist8_interval
NAMELIST /time_control/ auxhist9_interval_mo
NAMELIST /time_control/ auxhist9_interval_d
NAMELIST /time_control/ auxhist9_interval_h
NAMELIST /time_control/ auxhist9_interval_m
NAMELIST /time_control/ auxhist9_interval_s
NAMELIST /time_control/ auxhist9_interval
NAMELIST /time_control/ auxhist10_interval_mo
NAMELIST /time_control/ auxhist10_interval_d
NAMELIST /time_control/ auxhist10_interval_h
NAMELIST /time_control/ auxhist10_interval_m
NAMELIST /time_control/ auxhist10_interval_s
NAMELIST /time_control/ auxhist10_interval
NAMELIST /time_control/ auxhist11_interval_mo
NAMELIST /time_control/ auxhist11_interval_d
NAMELIST /time_control/ auxhist11_interval_h
NAMELIST /time_control/ auxhist11_interval_m
NAMELIST /time_control/ auxhist11_interval_s
NAMELIST /time_control/ auxhist11_interval
NAMELIST /time_control/ auxinput1_interval_mo
NAMELIST /time_control/ auxinput1_interval_d
NAMELIST /time_control/ auxinput1_interval_h
NAMELIST /time_control/ auxinput1_interval_m
NAMELIST /time_control/ auxinput1_interval_s
NAMELIST /time_control/ auxinput1_interval
NAMELIST /time_control/ auxinput2_interval_mo
NAMELIST /time_control/ auxinput2_interval_d
NAMELIST /time_control/ auxinput2_interval_h
NAMELIST /time_control/ auxinput2_interval_m
NAMELIST /time_control/ auxinput2_interval_s
NAMELIST /time_control/ auxinput2_interval
NAMELIST /time_control/ auxinput3_interval_mo
NAMELIST /time_control/ auxinput3_interval_d
NAMELIST /time_control/ auxinput3_interval_h
NAMELIST /time_control/ auxinput3_interval_m
NAMELIST /time_control/ auxinput3_interval_s
NAMELIST /time_control/ auxinput3_interval
NAMELIST /time_control/ auxinput4_interval_mo
NAMELIST /time_control/ auxinput4_interval_d
NAMELIST /time_control/ auxinput4_interval_h
NAMELIST /time_control/ auxinput4_interval_m
NAMELIST /time_control/ auxinput4_interval_s
NAMELIST /time_control/ auxinput4_interval
NAMELIST /time_control/ auxinput5_interval_mo
NAMELIST /time_control/ auxinput5_interval_d
NAMELIST /time_control/ auxinput5_interval_h
NAMELIST /time_control/ auxinput5_interval_m
NAMELIST /time_control/ auxinput5_interval_s
NAMELIST /time_control/ auxinput5_interval
NAMELIST /time_control/ auxinput6_interval_mo
NAMELIST /time_control/ auxinput6_interval_d
NAMELIST /time_control/ auxinput6_interval_h
NAMELIST /time_control/ auxinput6_interval_m
NAMELIST /time_control/ auxinput6_interval_s
NAMELIST /time_control/ auxinput6_interval
NAMELIST /time_control/ auxinput7_interval_mo
NAMELIST /time_control/ auxinput7_interval_d
NAMELIST /time_control/ auxinput7_interval_h
NAMELIST /time_control/ auxinput7_interval_m
NAMELIST /time_control/ auxinput7_interval_s
NAMELIST /time_control/ auxinput7_interval
NAMELIST /time_control/ auxinput8_interval_mo
NAMELIST /time_control/ auxinput8_interval_d
NAMELIST /time_control/ auxinput8_interval_h
NAMELIST /time_control/ auxinput8_interval_m
NAMELIST /time_control/ auxinput8_interval_s
NAMELIST /time_control/ auxinput8_interval
NAMELIST /fdda/ sgfdda_interval_mo
NAMELIST /fdda/ sgfdda_interval_d
NAMELIST /fdda/ sgfdda_interval_h
NAMELIST /fdda/ sgfdda_interval_m
NAMELIST /fdda/ sgfdda_interval_s
NAMELIST /fdda/ sgfdda_interval
NAMELIST /fdda/ gfdda_interval_mo
NAMELIST /fdda/ gfdda_interval_d
NAMELIST /fdda/ gfdda_interval_h
NAMELIST /fdda/ gfdda_interval_m
NAMELIST /fdda/ gfdda_interval_s
NAMELIST /fdda/ gfdda_interval
NAMELIST /time_control/ auxinput11_interval_mo
NAMELIST /time_control/ auxinput11_interval_d
NAMELIST /time_control/ auxinput11_interval_h
NAMELIST /time_control/ auxinput11_interval_m
NAMELIST /time_control/ auxinput11_interval_s
NAMELIST /time_control/ auxinput11_interval
NAMELIST /time_control/ restart_interval_mo
NAMELIST /time_control/ restart_interval_d
NAMELIST /time_control/ restart_interval_h
NAMELIST /time_control/ restart_interval_m
NAMELIST /time_control/ restart_interval_s
NAMELIST /time_control/ history_begin_y
NAMELIST /time_control/ history_begin_mo
NAMELIST /time_control/ history_begin_d
NAMELIST /time_control/ history_begin_h
NAMELIST /time_control/ history_begin_m
NAMELIST /time_control/ history_begin_s
NAMELIST /time_control/ inputout_begin_y
NAMELIST /time_control/ inputout_begin_mo
NAMELIST /time_control/ inputout_begin_d
NAMELIST /time_control/ inputout_begin_h
NAMELIST /time_control/ inputout_begin_m
NAMELIST /time_control/ inputout_begin_s
NAMELIST /time_control/ auxhist1_begin_y
NAMELIST /time_control/ auxhist1_begin_mo
NAMELIST /time_control/ auxhist1_begin_d
NAMELIST /time_control/ auxhist1_begin_h
NAMELIST /time_control/ auxhist1_begin_m
NAMELIST /time_control/ auxhist1_begin_s
NAMELIST /time_control/ auxhist2_begin_y
NAMELIST /time_control/ auxhist2_begin_mo
NAMELIST /time_control/ auxhist2_begin_d
NAMELIST /time_control/ auxhist2_begin_h
NAMELIST /time_control/ auxhist2_begin_m
NAMELIST /time_control/ auxhist2_begin_s
NAMELIST /time_control/ auxhist3_begin_y
NAMELIST /time_control/ auxhist3_begin_mo
NAMELIST /time_control/ auxhist3_begin_d
NAMELIST /time_control/ auxhist3_begin_h
NAMELIST /time_control/ auxhist3_begin_m
NAMELIST /time_control/ auxhist3_begin_s
NAMELIST /time_control/ auxhist4_begin_y
NAMELIST /time_control/ auxhist4_begin_mo
NAMELIST /time_control/ auxhist4_begin_d
NAMELIST /time_control/ auxhist4_begin_h
NAMELIST /time_control/ auxhist4_begin_m
NAMELIST /time_control/ auxhist4_begin_s
NAMELIST /time_control/ auxhist5_begin_y
NAMELIST /time_control/ auxhist5_begin_mo
NAMELIST /time_control/ auxhist5_begin_d
NAMELIST /time_control/ auxhist5_begin_h
NAMELIST /time_control/ auxhist5_begin_m
NAMELIST /time_control/ auxhist5_begin_s
NAMELIST /time_control/ auxhist6_begin_y
NAMELIST /time_control/ auxhist6_begin_mo
NAMELIST /time_control/ auxhist6_begin_d
NAMELIST /time_control/ auxhist6_begin_h
NAMELIST /time_control/ auxhist6_begin_m
NAMELIST /time_control/ auxhist6_begin_s
NAMELIST /time_control/ auxhist7_begin_y
NAMELIST /time_control/ auxhist7_begin_mo
NAMELIST /time_control/ auxhist7_begin_d
NAMELIST /time_control/ auxhist7_begin_h
NAMELIST /time_control/ auxhist7_begin_m
NAMELIST /time_control/ auxhist7_begin_s
NAMELIST /time_control/ auxhist8_begin_y
NAMELIST /time_control/ auxhist8_begin_mo
NAMELIST /time_control/ auxhist8_begin_d
NAMELIST /time_control/ auxhist8_begin_h
NAMELIST /time_control/ auxhist8_begin_m
NAMELIST /time_control/ auxhist8_begin_s
NAMELIST /time_control/ auxhist9_begin_y
NAMELIST /time_control/ auxhist9_begin_mo
NAMELIST /time_control/ auxhist9_begin_d
NAMELIST /time_control/ auxhist9_begin_h
NAMELIST /time_control/ auxhist9_begin_m
NAMELIST /time_control/ auxhist9_begin_s
NAMELIST /time_control/ auxhist10_begin_y
NAMELIST /time_control/ auxhist10_begin_mo
NAMELIST /time_control/ auxhist10_begin_d
NAMELIST /time_control/ auxhist10_begin_h
NAMELIST /time_control/ auxhist10_begin_m
NAMELIST /time_control/ auxhist10_begin_s
NAMELIST /time_control/ auxhist11_begin_y
NAMELIST /time_control/ auxhist11_begin_mo
NAMELIST /time_control/ auxhist11_begin_d
NAMELIST /time_control/ auxhist11_begin_h
NAMELIST /time_control/ auxhist11_begin_m
NAMELIST /time_control/ auxhist11_begin_s
NAMELIST /time_control/ auxinput1_begin_y
NAMELIST /time_control/ auxinput1_begin_mo
NAMELIST /time_control/ auxinput1_begin_d
NAMELIST /time_control/ auxinput1_begin_h
NAMELIST /time_control/ auxinput1_begin_m
NAMELIST /time_control/ auxinput1_begin_s
NAMELIST /time_control/ auxinput2_begin_y
NAMELIST /time_control/ auxinput2_begin_mo
NAMELIST /time_control/ auxinput2_begin_d
NAMELIST /time_control/ auxinput2_begin_h
NAMELIST /time_control/ auxinput2_begin_m
NAMELIST /time_control/ auxinput2_begin_s
NAMELIST /time_control/ auxinput3_begin_y
NAMELIST /time_control/ auxinput3_begin_mo
NAMELIST /time_control/ auxinput3_begin_d
NAMELIST /time_control/ auxinput3_begin_h
NAMELIST /time_control/ auxinput3_begin_m
NAMELIST /time_control/ auxinput3_begin_s
NAMELIST /time_control/ auxinput4_begin_y
NAMELIST /time_control/ auxinput4_begin_mo
NAMELIST /time_control/ auxinput4_begin_d
NAMELIST /time_control/ auxinput4_begin_h
NAMELIST /time_control/ auxinput4_begin_m
NAMELIST /time_control/ auxinput4_begin_s
NAMELIST /time_control/ auxinput5_begin_y
NAMELIST /time_control/ auxinput5_begin_mo
NAMELIST /time_control/ auxinput5_begin_d
NAMELIST /time_control/ auxinput5_begin_h
NAMELIST /time_control/ auxinput5_begin_m
NAMELIST /time_control/ auxinput5_begin_s
NAMELIST /time_control/ auxinput6_begin_y
NAMELIST /time_control/ auxinput6_begin_mo
NAMELIST /time_control/ auxinput6_begin_d
NAMELIST /time_control/ auxinput6_begin_h
NAMELIST /time_control/ auxinput6_begin_m
NAMELIST /time_control/ auxinput6_begin_s
NAMELIST /time_control/ auxinput7_begin_y
NAMELIST /time_control/ auxinput7_begin_mo
NAMELIST /time_control/ auxinput7_begin_d
NAMELIST /time_control/ auxinput7_begin_h
NAMELIST /time_control/ auxinput7_begin_m
NAMELIST /time_control/ auxinput7_begin_s
NAMELIST /time_control/ auxinput8_begin_y
NAMELIST /time_control/ auxinput8_begin_mo
NAMELIST /time_control/ auxinput8_begin_d
NAMELIST /time_control/ auxinput8_begin_h
NAMELIST /time_control/ auxinput8_begin_m
NAMELIST /time_control/ auxinput8_begin_s
NAMELIST /fdda/ sgfdda_begin_y
NAMELIST /fdda/ sgfdda_begin_mo
NAMELIST /fdda/ sgfdda_begin_d
NAMELIST /fdda/ sgfdda_begin_h
NAMELIST /fdda/ sgfdda_begin_m
NAMELIST /fdda/ sgfdda_begin_s
NAMELIST /fdda/ gfdda_begin_y
NAMELIST /fdda/ gfdda_begin_mo
NAMELIST /fdda/ gfdda_begin_d
NAMELIST /fdda/ gfdda_begin_h
NAMELIST /fdda/ gfdda_begin_m
NAMELIST /fdda/ gfdda_begin_s
NAMELIST /time_control/ auxinput11_begin_y
NAMELIST /time_control/ auxinput11_begin_mo
NAMELIST /time_control/ auxinput11_begin_d
NAMELIST /time_control/ auxinput11_begin_h
NAMELIST /time_control/ auxinput11_begin_m
NAMELIST /time_control/ auxinput11_begin_s
NAMELIST /time_control/ restart_begin_y
NAMELIST /time_control/ restart_begin_mo
NAMELIST /time_control/ restart_begin_d
NAMELIST /time_control/ restart_begin_h
NAMELIST /time_control/ restart_begin_m
NAMELIST /time_control/ restart_begin_s
NAMELIST /time_control/ history_end_y
NAMELIST /time_control/ history_end_mo
NAMELIST /time_control/ history_end_d
NAMELIST /time_control/ history_end_h
NAMELIST /time_control/ history_end_m
NAMELIST /time_control/ history_end_s
NAMELIST /time_control/ inputout_end_y
NAMELIST /time_control/ inputout_end_mo
NAMELIST /time_control/ inputout_end_d
NAMELIST /time_control/ inputout_end_h
NAMELIST /time_control/ inputout_end_m
NAMELIST /time_control/ inputout_end_s
NAMELIST /time_control/ auxhist1_end_y
NAMELIST /time_control/ auxhist1_end_mo
NAMELIST /time_control/ auxhist1_end_d
NAMELIST /time_control/ auxhist1_end_h
NAMELIST /time_control/ auxhist1_end_m
NAMELIST /time_control/ auxhist1_end_s
NAMELIST /time_control/ auxhist2_end_y
NAMELIST /time_control/ auxhist2_end_mo
NAMELIST /time_control/ auxhist2_end_d
NAMELIST /time_control/ auxhist2_end_h
NAMELIST /time_control/ auxhist2_end_m
NAMELIST /time_control/ auxhist2_end_s
NAMELIST /time_control/ auxhist3_end_y
NAMELIST /time_control/ auxhist3_end_mo
NAMELIST /time_control/ auxhist3_end_d
NAMELIST /time_control/ auxhist3_end_h
NAMELIST /time_control/ auxhist3_end_m
NAMELIST /time_control/ auxhist3_end_s
NAMELIST /time_control/ auxhist4_end_y
NAMELIST /time_control/ auxhist4_end_mo
NAMELIST /time_control/ auxhist4_end_d
NAMELIST /time_control/ auxhist4_end_h
NAMELIST /time_control/ auxhist4_end_m
NAMELIST /time_control/ auxhist4_end_s
NAMELIST /time_control/ auxhist5_end_y
NAMELIST /time_control/ auxhist5_end_mo
NAMELIST /time_control/ auxhist5_end_d
NAMELIST /time_control/ auxhist5_end_h
NAMELIST /time_control/ auxhist5_end_m
NAMELIST /time_control/ auxhist5_end_s
NAMELIST /time_control/ auxhist6_end_y
NAMELIST /time_control/ auxhist6_end_mo
NAMELIST /time_control/ auxhist6_end_d
NAMELIST /time_control/ auxhist6_end_h
NAMELIST /time_control/ auxhist6_end_m
NAMELIST /time_control/ auxhist6_end_s
NAMELIST /time_control/ auxhist7_end_y
NAMELIST /time_control/ auxhist7_end_mo
NAMELIST /time_control/ auxhist7_end_d
NAMELIST /time_control/ auxhist7_end_h
NAMELIST /time_control/ auxhist7_end_m
NAMELIST /time_control/ auxhist7_end_s
NAMELIST /time_control/ auxhist8_end_y
NAMELIST /time_control/ auxhist8_end_mo
NAMELIST /time_control/ auxhist8_end_d
NAMELIST /time_control/ auxhist8_end_h
NAMELIST /time_control/ auxhist8_end_m
NAMELIST /time_control/ auxhist8_end_s
NAMELIST /time_control/ auxhist9_end_y
NAMELIST /time_control/ auxhist9_end_mo
NAMELIST /time_control/ auxhist9_end_d
NAMELIST /time_control/ auxhist9_end_h
NAMELIST /time_control/ auxhist9_end_m
NAMELIST /time_control/ auxhist9_end_s
NAMELIST /time_control/ auxhist10_end_y
NAMELIST /time_control/ auxhist10_end_mo
NAMELIST /time_control/ auxhist10_end_d
NAMELIST /time_control/ auxhist10_end_h
NAMELIST /time_control/ auxhist10_end_m
NAMELIST /time_control/ auxhist10_end_s
NAMELIST /time_control/ auxhist11_end_y
NAMELIST /time_control/ auxhist11_end_mo
NAMELIST /time_control/ auxhist11_end_d
NAMELIST /time_control/ auxhist11_end_h
NAMELIST /time_control/ auxhist11_end_m
NAMELIST /time_control/ auxhist11_end_s
NAMELIST /time_control/ auxinput1_end_y
NAMELIST /time_control/ auxinput1_end_mo
NAMELIST /time_control/ auxinput1_end_d
NAMELIST /time_control/ auxinput1_end_h
NAMELIST /time_control/ auxinput1_end_m
NAMELIST /time_control/ auxinput1_end_s
NAMELIST /time_control/ auxinput2_end_y
NAMELIST /time_control/ auxinput2_end_mo
NAMELIST /time_control/ auxinput2_end_d
NAMELIST /time_control/ auxinput2_end_h
NAMELIST /time_control/ auxinput2_end_m
NAMELIST /time_control/ auxinput2_end_s
NAMELIST /time_control/ auxinput3_end_y
NAMELIST /time_control/ auxinput3_end_mo
NAMELIST /time_control/ auxinput3_end_d
NAMELIST /time_control/ auxinput3_end_h
NAMELIST /time_control/ auxinput3_end_m
NAMELIST /time_control/ auxinput3_end_s
NAMELIST /time_control/ auxinput4_end_y
NAMELIST /time_control/ auxinput4_end_mo
NAMELIST /time_control/ auxinput4_end_d
NAMELIST /time_control/ auxinput4_end_h
NAMELIST /time_control/ auxinput4_end_m
NAMELIST /time_control/ auxinput4_end_s
NAMELIST /time_control/ auxinput5_end_y
NAMELIST /time_control/ auxinput5_end_mo
NAMELIST /time_control/ auxinput5_end_d
NAMELIST /time_control/ auxinput5_end_h
NAMELIST /time_control/ auxinput5_end_m
NAMELIST /time_control/ auxinput5_end_s
NAMELIST /time_control/ auxinput6_end_y
NAMELIST /time_control/ auxinput6_end_mo
NAMELIST /time_control/ auxinput6_end_d
NAMELIST /time_control/ auxinput6_end_h
NAMELIST /time_control/ auxinput6_end_m
NAMELIST /time_control/ auxinput6_end_s
NAMELIST /time_control/ auxinput7_end_y
NAMELIST /time_control/ auxinput7_end_mo
NAMELIST /time_control/ auxinput7_end_d
NAMELIST /time_control/ auxinput7_end_h
NAMELIST /time_control/ auxinput7_end_m
NAMELIST /time_control/ auxinput7_end_s
NAMELIST /time_control/ auxinput8_end_y
NAMELIST /time_control/ auxinput8_end_mo
NAMELIST /time_control/ auxinput8_end_d
NAMELIST /time_control/ auxinput8_end_h
NAMELIST /time_control/ auxinput8_end_m
NAMELIST /time_control/ auxinput8_end_s
NAMELIST /fdda/ sgfdda_end_y
NAMELIST /fdda/ sgfdda_end_mo
NAMELIST /fdda/ sgfdda_end_d
NAMELIST /fdda/ sgfdda_end_h
NAMELIST /fdda/ sgfdda_end_m
NAMELIST /fdda/ sgfdda_end_s
NAMELIST /fdda/ gfdda_end_y
NAMELIST /fdda/ gfdda_end_mo
NAMELIST /fdda/ gfdda_end_d
NAMELIST /fdda/ gfdda_end_h
NAMELIST /fdda/ gfdda_end_m
NAMELIST /fdda/ gfdda_end_s
NAMELIST /time_control/ auxinput11_end_y
NAMELIST /time_control/ auxinput11_end_mo
NAMELIST /time_control/ auxinput11_end_d
NAMELIST /time_control/ auxinput11_end_h
NAMELIST /time_control/ auxinput11_end_m
NAMELIST /time_control/ auxinput11_end_s
NAMELIST /time_control/ io_form_auxinput1
NAMELIST /time_control/ io_form_auxinput2
NAMELIST /time_control/ io_form_auxinput3
NAMELIST /time_control/ io_form_auxinput4
NAMELIST /time_control/ io_form_auxinput5
NAMELIST /time_control/ io_form_auxinput6
NAMELIST /time_control/ io_form_auxinput7
NAMELIST /time_control/ io_form_auxinput8
NAMELIST /fdda/ io_form_sgfdda
NAMELIST /fdda/ io_form_gfdda
NAMELIST /time_control/ io_form_auxinput11
NAMELIST /time_control/ io_form_auxhist1
NAMELIST /time_control/ io_form_auxhist2
NAMELIST /time_control/ io_form_auxhist3
NAMELIST /time_control/ io_form_auxhist4
NAMELIST /time_control/ io_form_auxhist5
NAMELIST /time_control/ io_form_auxhist6
NAMELIST /time_control/ io_form_auxhist7
NAMELIST /time_control/ io_form_auxhist8
NAMELIST /time_control/ io_form_auxhist9
NAMELIST /time_control/ io_form_auxhist10
NAMELIST /time_control/ io_form_auxhist11
NAMELIST /time_control/ reset_simulation_start
NAMELIST /domains/ sr_x
NAMELIST /domains/ sr_y
NAMELIST /time_control/ julyr
NAMELIST /time_control/ julday
NAMELIST /time_control/ gmt
NAMELIST /time_control/ input_inname
NAMELIST /time_control/ input_outname
NAMELIST /time_control/ bdy_inname
NAMELIST /time_control/ bdy_outname
NAMELIST /time_control/ rst_inname
NAMELIST /time_control/ rst_outname
NAMELIST /time_control/ write_input
NAMELIST /time_control/ write_restart_at_0h
NAMELIST /time_control/ adjust_output_times
NAMELIST /time_control/ adjust_input_times
NAMELIST /time_control/ tstart
NAMELIST /time_control/ nocolons
NAMELIST /time_control/ cycling
NAMELIST /dfi_control/ dfi_opt
NAMELIST /dfi_control/ dfi_nfilter
NAMELIST /dfi_control/ dfi_write_filtered_input
NAMELIST /dfi_control/ dfi_write_dfi_history
NAMELIST /dfi_control/ dfi_cutoff_seconds
NAMELIST /dfi_control/ dfi_time_dim
NAMELIST /dfi_control/ dfi_fwdstop_year
NAMELIST /dfi_control/ dfi_fwdstop_month
NAMELIST /dfi_control/ dfi_fwdstop_day
NAMELIST /dfi_control/ dfi_fwdstop_hour
NAMELIST /dfi_control/ dfi_fwdstop_minute
NAMELIST /dfi_control/ dfi_fwdstop_second
NAMELIST /dfi_control/ dfi_bckstop_year
NAMELIST /dfi_control/ dfi_bckstop_month
NAMELIST /dfi_control/ dfi_bckstop_day
NAMELIST /dfi_control/ dfi_bckstop_hour
NAMELIST /dfi_control/ dfi_bckstop_minute
NAMELIST /dfi_control/ dfi_bckstop_second
NAMELIST /domains/ time_step
NAMELIST /domains/ time_step_fract_num
NAMELIST /domains/ time_step_fract_den
NAMELIST /domains/ max_dom
NAMELIST /domains/ s_we
NAMELIST /domains/ e_we
NAMELIST /domains/ s_sn
NAMELIST /domains/ e_sn
NAMELIST /domains/ s_vert
NAMELIST /domains/ e_vert
NAMELIST /domains/ dx
NAMELIST /domains/ dy
NAMELIST /domains/ grid_id
NAMELIST /domains/ grid_allowed
NAMELIST /domains/ parent_id
NAMELIST /domains/ i_parent_start
NAMELIST /domains/ j_parent_start
NAMELIST /domains/ parent_grid_ratio
NAMELIST /domains/ parent_time_step_ratio
NAMELIST /domains/ feedback
NAMELIST /domains/ smooth_option
NAMELIST /domains/ ztop
NAMELIST /domains/ moad_grid_ratio
NAMELIST /domains/ moad_time_step_ratio
NAMELIST /domains/ shw
NAMELIST /domains/ tile_sz_x
NAMELIST /domains/ tile_sz_y
NAMELIST /domains/ numtiles
NAMELIST /domains/ nproc_x
NAMELIST /domains/ nproc_y
NAMELIST /domains/ irand
NAMELIST /domains/ ts_buf_size
NAMELIST /domains/ max_ts_locs
NAMELIST /domains/ num_moves
NAMELIST /domains/ move_id
NAMELIST /domains/ move_interval
NAMELIST /domains/ move_cd_x
NAMELIST /domains/ move_cd_y
NAMELIST /domains/ swap_x
NAMELIST /domains/ swap_y
NAMELIST /domains/ cycle_x
NAMELIST /domains/ cycle_y
NAMELIST /domains/ reorder_mesh
NAMELIST /domains/ perturb_input
NAMELIST /domains/ eta_levels
NAMELIST /domains/ ptsgm
NAMELIST /domains/ num_metgrid_levels
NAMELIST /domains/ p_top_requested
NAMELIST /physics/ mp_physics
NAMELIST /physics/ ra_lw_physics
NAMELIST /physics/ ra_sw_physics
NAMELIST /physics/ radt
NAMELIST /physics/ sf_sfclay_physics
NAMELIST /physics/ sf_surface_physics
NAMELIST /physics/ bl_pbl_physics
NAMELIST /physics/ sf_urban_physics
NAMELIST /physics/ bldt
NAMELIST /physics/ cu_physics
NAMELIST /physics/ cudt
NAMELIST /physics/ gsmdt
NAMELIST /physics/ isfflx
NAMELIST /physics/ ifsnow
NAMELIST /physics/ icloud
NAMELIST /physics/ swrad_scat
NAMELIST /physics/ surface_input_source
NAMELIST /physics/ num_soil_layers
NAMELIST /physics/ num_urban_layers
NAMELIST /physics/ maxiens
NAMELIST /physics/ maxens
NAMELIST /physics/ maxens2
NAMELIST /physics/ maxens3
NAMELIST /physics/ ensdim
NAMELIST /physics/ chem_opt
NAMELIST /physics/ num_land_cat
NAMELIST /physics/ num_soil_cat
NAMELIST /physics/ mp_zero_out
NAMELIST /physics/ mp_zero_out_thresh
NAMELIST /physics/ seaice_threshold
NAMELIST /physics/ fractional_seaice
NAMELIST /physics/ sst_update
NAMELIST /physics/ usemonalb
NAMELIST /physics/ rdmaxalb
NAMELIST /physics/ rdlai2d
NAMELIST /physics/ gwd_opt
NAMELIST /physics/ idtad
NAMELIST /physics/ nsoil
NAMELIST /physics/ nphs
NAMELIST /physics/ ncnvc
NAMELIST /physics/ nrads
NAMELIST /physics/ nradl
NAMELIST /physics/ tprec
NAMELIST /physics/ theat
NAMELIST /physics/ tclod
NAMELIST /physics/ trdsw
NAMELIST /physics/ trdlw
NAMELIST /physics/ tsrfc
NAMELIST /physics/ pcpflg
NAMELIST /physics/ sigma
NAMELIST /physics/ sfenth
NAMELIST /physics/ co2tf
NAMELIST /physics/ ra_call_offset
NAMELIST /physics/ cam_abs_freq_s
NAMELIST /physics/ levsiz
NAMELIST /physics/ paerlev
NAMELIST /physics/ cam_abs_dim1
NAMELIST /physics/ cam_abs_dim2
NAMELIST /physics/ cu_rad_feedback
NAMELIST /dynamics/ dyn_opt
NAMELIST /dynamics/ rk_ord
NAMELIST /dynamics/ w_damping
NAMELIST /dynamics/ diff_opt
NAMELIST /dynamics/ km_opt
NAMELIST /dynamics/ damp_opt
NAMELIST /dynamics/ zdamp
NAMELIST /dynamics/ base_pres
NAMELIST /dynamics/ base_temp
NAMELIST /dynamics/ base_lapse
NAMELIST /dynamics/ iso_temp
NAMELIST /dynamics/ dampcoef
NAMELIST /dynamics/ khdif
NAMELIST /dynamics/ kvdif
NAMELIST /dynamics/ c_s
NAMELIST /dynamics/ c_k
NAMELIST /dynamics/ smdiv
NAMELIST /dynamics/ emdiv
NAMELIST /dynamics/ epssm
NAMELIST /dynamics/ non_hydrostatic
NAMELIST /dynamics/ time_step_sound
NAMELIST /dynamics/ h_mom_adv_order
NAMELIST /dynamics/ v_mom_adv_order
NAMELIST /dynamics/ h_sca_adv_order
NAMELIST /dynamics/ v_sca_adv_order
NAMELIST /dynamics/ top_radiation
NAMELIST /dynamics/ tke_upper_bound
NAMELIST /dynamics/ tke_drag_coefficient
NAMELIST /dynamics/ tke_heat_flux
NAMELIST /dynamics/ pert_coriolis
NAMELIST /dynamics/ euler_adv
NAMELIST /dynamics/ idtadt
NAMELIST /dynamics/ idtadc
NAMELIST /dynamics/ boundary_flux
NAMELIST /bdy_control/ spec_bdy_width
NAMELIST /bdy_control/ spec_zone
NAMELIST /bdy_control/ relax_zone
NAMELIST /bdy_control/ specified
NAMELIST /bdy_control/ periodic_x
NAMELIST /bdy_control/ symmetric_xs
NAMELIST /bdy_control/ symmetric_xe
NAMELIST /bdy_control/ open_xs
NAMELIST /bdy_control/ open_xe
NAMELIST /bdy_control/ periodic_y
NAMELIST /bdy_control/ symmetric_ys
NAMELIST /bdy_control/ symmetric_ye
NAMELIST /bdy_control/ open_ys
NAMELIST /bdy_control/ open_ye
NAMELIST /bdy_control/ polar
NAMELIST /bdy_control/ nested
NAMELIST /bdy_control/ real_data_init_type
NAMELIST /grib2/ background_proc_id
NAMELIST /grib2/ forecast_proc_id
NAMELIST /grib2/ production_status
NAMELIST /grib2/ compression



     integer , DIMENSION(max_domains) :: obs_nobs_prt
     NAMELIST /fdda/ obs_nobs_prt





     REWIND ( UNIT = nml_read_unit )
     READ   ( UNIT = nml_read_unit , NML = fdda , iostat=nml_error )

     IF ( nml_error .EQ. 0 ) then    
        CALL wrf_debug(0,"Is obs_nobs_prt still in your "// TRIM(nml_name)//" namelist?")
        CALL wrf_error_fatal3("",11110,&
"Replace it with obs_prt_max")
     ELSE     
        return
     ENDIF

   END SUBROUTINE wrf_alt_nml_fdda

   SUBROUTINE wrfvar_alt_nml_wrfvar1 (nml_read_unit, nml_name)







     IMPLICIT NONE
     INTEGER, INTENT(IN)       :: nml_read_unit
     CHARACTER*(*), INTENT(IN) :: nml_name
     INTEGER                   :: nml_error







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
character*256 :: auxinput1_inname
character*256 :: auxinput2_inname
character*256 :: auxinput3_inname
character*256 :: auxinput4_inname
character*256 :: auxinput5_inname
character*256 :: auxinput6_inname
character*256 :: auxinput7_inname
character*256 :: auxinput8_inname
character*256 :: sgfdda_inname
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
integer , DIMENSION(max_domains) :: sgfdda_interval_mo
integer , DIMENSION(max_domains) :: sgfdda_interval_d
integer , DIMENSION(max_domains) :: sgfdda_interval_h
integer , DIMENSION(max_domains) :: sgfdda_interval_m
integer , DIMENSION(max_domains) :: sgfdda_interval_s
integer , DIMENSION(max_domains) :: sgfdda_interval
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
integer , DIMENSION(max_domains) :: sgfdda_begin_y
integer , DIMENSION(max_domains) :: sgfdda_begin_mo
integer , DIMENSION(max_domains) :: sgfdda_begin_d
integer , DIMENSION(max_domains) :: sgfdda_begin_h
integer , DIMENSION(max_domains) :: sgfdda_begin_m
integer , DIMENSION(max_domains) :: sgfdda_begin_s
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
integer , DIMENSION(max_domains) :: sgfdda_end_y
integer , DIMENSION(max_domains) :: sgfdda_end_mo
integer , DIMENSION(max_domains) :: sgfdda_end_d
integer , DIMENSION(max_domains) :: sgfdda_end_h
integer , DIMENSION(max_domains) :: sgfdda_end_m
integer , DIMENSION(max_domains) :: sgfdda_end_s
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
integer :: io_form_sgfdda
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
integer :: simulation_start_year
integer :: simulation_start_month
integer :: simulation_start_day
integer :: simulation_start_hour
integer :: simulation_start_minute
integer :: simulation_start_second
logical :: reset_simulation_start
integer , DIMENSION(max_domains) :: sr_x
integer , DIMENSION(max_domains) :: sr_y
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
real , DIMENSION(max_domains) :: tstart
logical :: nocolons
logical :: cycling
integer :: dfi_opt
integer :: dfi_nfilter
logical :: dfi_write_filtered_input
logical :: dfi_write_dfi_history
integer :: dfi_cutoff_seconds
integer :: dfi_time_dim
integer :: dfi_fwdstop_year
integer :: dfi_fwdstop_month
integer :: dfi_fwdstop_day
integer :: dfi_fwdstop_hour
integer :: dfi_fwdstop_minute
integer :: dfi_fwdstop_second
integer :: dfi_bckstop_year
integer :: dfi_bckstop_month
integer :: dfi_bckstop_day
integer :: dfi_bckstop_hour
integer :: dfi_bckstop_minute
integer :: dfi_bckstop_second
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
real , DIMENSION(max_domains) :: dx
real , DIMENSION(max_domains) :: dy
integer , DIMENSION(max_domains) :: grid_id
logical , DIMENSION(max_domains) :: grid_allowed
integer , DIMENSION(max_domains) :: parent_id
integer , DIMENSION(max_domains) :: i_parent_start
integer , DIMENSION(max_domains) :: j_parent_start
integer , DIMENSION(max_domains) :: parent_grid_ratio
integer , DIMENSION(max_domains) :: parent_time_step_ratio
integer :: feedback
integer :: smooth_option
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
integer :: ts_buf_size
integer :: max_ts_locs
integer :: num_moves
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
real :: ptsgm
integer :: num_metgrid_levels
real :: p_top_requested
integer , DIMENSION(max_domains) :: mp_physics
integer , DIMENSION(max_domains) :: ra_lw_physics
integer , DIMENSION(max_domains) :: ra_sw_physics
real , DIMENSION(max_domains) :: radt
integer , DIMENSION(max_domains) :: sf_sfclay_physics
integer , DIMENSION(max_domains) :: sf_surface_physics
integer , DIMENSION(max_domains) :: bl_pbl_physics
integer , DIMENSION(max_domains) :: sf_urban_physics
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
integer :: num_urban_layers
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
integer :: fractional_seaice
integer :: sst_update
logical :: usemonalb
logical :: rdmaxalb
logical :: rdlai2d
integer , DIMENSION(max_domains) :: gwd_opt
integer , DIMENSION(max_domains) :: idtad
integer , DIMENSION(max_domains) :: nsoil
integer , DIMENSION(max_domains) :: nphs
integer , DIMENSION(max_domains) :: ncnvc
integer , DIMENSION(max_domains) :: nrads
integer , DIMENSION(max_domains) :: nradl
real , DIMENSION(max_domains) :: tprec
real , DIMENSION(max_domains) :: theat
real , DIMENSION(max_domains) :: tclod
real , DIMENSION(max_domains) :: trdsw
real , DIMENSION(max_domains) :: trdlw
real , DIMENSION(max_domains) :: tsrfc
logical , DIMENSION(max_domains) :: pcpflg
integer , DIMENSION(max_domains) :: sigma
real , DIMENSION(max_domains) :: sfenth
integer :: co2tf
integer :: ra_call_offset
real :: cam_abs_freq_s
integer :: levsiz
integer :: paerlev
integer :: cam_abs_dim1
integer :: cam_abs_dim2
logical , DIMENSION(max_domains) :: cu_rad_feedback
integer :: dyn_opt
integer :: rk_ord
integer :: w_damping
integer :: diff_opt
integer :: km_opt
integer :: damp_opt
real , DIMENSION(max_domains) :: zdamp
real :: base_pres
real :: base_temp
real :: base_lapse
real :: iso_temp
real , DIMENSION(max_domains) :: dampcoef
real , DIMENSION(max_domains) :: khdif
real , DIMENSION(max_domains) :: kvdif
real , DIMENSION(max_domains) :: c_s
real , DIMENSION(max_domains) :: c_k
real , DIMENSION(max_domains) :: smdiv
real , DIMENSION(max_domains) :: emdiv
real , DIMENSION(max_domains) :: epssm
logical , DIMENSION(max_domains) :: non_hydrostatic
integer , DIMENSION(max_domains) :: time_step_sound
integer , DIMENSION(max_domains) :: h_mom_adv_order
integer , DIMENSION(max_domains) :: v_mom_adv_order
integer , DIMENSION(max_domains) :: h_sca_adv_order
integer , DIMENSION(max_domains) :: v_sca_adv_order
logical , DIMENSION(max_domains) :: top_radiation
real , DIMENSION(max_domains) :: tke_upper_bound
real , DIMENSION(max_domains) :: tke_drag_coefficient
real , DIMENSION(max_domains) :: tke_heat_flux
logical , DIMENSION(max_domains) :: pert_coriolis
logical :: euler_adv
integer :: idtadt
integer :: idtadc
logical :: boundary_flux
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
logical , DIMENSION(max_domains) :: polar
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
integer :: flag_metgrid
integer :: flag_snow
integer :: flag_psfc
integer :: flag_sm000010
integer :: flag_sm010040
integer :: flag_sm040100
integer :: flag_sm100200
integer :: flag_st000010
integer :: flag_st010040
integer :: flag_st040100
integer :: flag_st100200
integer :: flag_slp
integer :: flag_soilhgt
integer :: flag_mf_xy
real , DIMENSION(max_domains) :: bdyfrq
character*256 , DIMENSION(max_domains) :: mminlu
integer , DIMENSION(max_domains) :: iswater
integer , DIMENSION(max_domains) :: islake
integer , DIMENSION(max_domains) :: isice
integer , DIMENSION(max_domains) :: isurban
integer , DIMENSION(max_domains) :: isoilwater
integer , DIMENSION(max_domains) :: map_proj
integer :: dfi_stage
integer , DIMENSION(max_domains) :: mp_physics_dfi
integer    :: last_item_in_struct







NAMELIST /time_control/ run_days
NAMELIST /time_control/ run_hours
NAMELIST /time_control/ run_minutes
NAMELIST /time_control/ run_seconds
NAMELIST /time_control/ start_year
NAMELIST /time_control/ start_month
NAMELIST /time_control/ start_day
NAMELIST /time_control/ start_hour
NAMELIST /time_control/ start_minute
NAMELIST /time_control/ start_second
NAMELIST /time_control/ end_year
NAMELIST /time_control/ end_month
NAMELIST /time_control/ end_day
NAMELIST /time_control/ end_hour
NAMELIST /time_control/ end_minute
NAMELIST /time_control/ end_second
NAMELIST /time_control/ interval_seconds
NAMELIST /time_control/ input_from_file
NAMELIST /time_control/ fine_input_stream
NAMELIST /time_control/ history_interval
NAMELIST /time_control/ frames_per_outfile
NAMELIST /time_control/ frames_per_auxhist1
NAMELIST /time_control/ frames_per_auxhist2
NAMELIST /time_control/ frames_per_auxhist3
NAMELIST /time_control/ frames_per_auxhist4
NAMELIST /time_control/ frames_per_auxhist5
NAMELIST /time_control/ frames_per_auxhist6
NAMELIST /time_control/ frames_per_auxhist7
NAMELIST /time_control/ frames_per_auxhist8
NAMELIST /time_control/ frames_per_auxhist9
NAMELIST /time_control/ frames_per_auxhist10
NAMELIST /time_control/ frames_per_auxhist11
NAMELIST /time_control/ restart
NAMELIST /time_control/ restart_interval
NAMELIST /time_control/ io_form_input
NAMELIST /time_control/ io_form_history
NAMELIST /time_control/ io_form_restart
NAMELIST /time_control/ io_form_boundary
NAMELIST /time_control/ debug_level
NAMELIST /time_control/ self_test_domain
NAMELIST /time_control/ history_outname
NAMELIST /time_control/ auxhist1_outname
NAMELIST /time_control/ auxhist2_outname
NAMELIST /time_control/ auxhist3_outname
NAMELIST /time_control/ auxhist4_outname
NAMELIST /time_control/ auxhist5_outname
NAMELIST /time_control/ auxhist6_outname
NAMELIST /time_control/ auxhist7_outname
NAMELIST /time_control/ auxhist8_outname
NAMELIST /time_control/ auxhist9_outname
NAMELIST /time_control/ auxhist10_outname
NAMELIST /time_control/ auxhist11_outname
NAMELIST /time_control/ history_inname
NAMELIST /time_control/ auxhist1_inname
NAMELIST /time_control/ auxhist2_inname
NAMELIST /time_control/ auxhist3_inname
NAMELIST /time_control/ auxhist4_inname
NAMELIST /time_control/ auxhist5_inname
NAMELIST /time_control/ auxhist6_inname
NAMELIST /time_control/ auxhist7_inname
NAMELIST /time_control/ auxhist8_inname
NAMELIST /time_control/ auxhist9_inname
NAMELIST /time_control/ auxhist10_inname
NAMELIST /time_control/ auxhist11_inname
NAMELIST /time_control/ auxinput1_outname
NAMELIST /time_control/ auxinput2_outname
NAMELIST /time_control/ auxinput3_outname
NAMELIST /time_control/ auxinput4_outname
NAMELIST /time_control/ auxinput5_outname
NAMELIST /time_control/ auxinput6_outname
NAMELIST /time_control/ auxinput7_outname
NAMELIST /time_control/ auxinput8_outname
NAMELIST /time_control/ auxinput9_outname
NAMELIST /time_control/ auxinput10_outname
NAMELIST /time_control/ auxinput11_outname
NAMELIST /time_control/ auxinput1_inname
NAMELIST /time_control/ auxinput2_inname
NAMELIST /time_control/ auxinput3_inname
NAMELIST /time_control/ auxinput4_inname
NAMELIST /time_control/ auxinput5_inname
NAMELIST /time_control/ auxinput6_inname
NAMELIST /time_control/ auxinput7_inname
NAMELIST /time_control/ auxinput8_inname
NAMELIST /fdda/ sgfdda_inname
NAMELIST /fdda/ gfdda_inname
NAMELIST /time_control/ auxinput11_inname
NAMELIST /time_control/ history_interval_mo
NAMELIST /time_control/ history_interval_d
NAMELIST /time_control/ history_interval_h
NAMELIST /time_control/ history_interval_m
NAMELIST /time_control/ history_interval_s
NAMELIST /time_control/ inputout_interval_mo
NAMELIST /time_control/ inputout_interval_d
NAMELIST /time_control/ inputout_interval_h
NAMELIST /time_control/ inputout_interval_m
NAMELIST /time_control/ inputout_interval_s
NAMELIST /time_control/ inputout_interval
NAMELIST /time_control/ auxhist1_interval_mo
NAMELIST /time_control/ auxhist1_interval_d
NAMELIST /time_control/ auxhist1_interval_h
NAMELIST /time_control/ auxhist1_interval_m
NAMELIST /time_control/ auxhist1_interval_s
NAMELIST /time_control/ auxhist1_interval
NAMELIST /time_control/ auxhist2_interval_mo
NAMELIST /time_control/ auxhist2_interval_d
NAMELIST /time_control/ auxhist2_interval_h
NAMELIST /time_control/ auxhist2_interval_m
NAMELIST /time_control/ auxhist2_interval_s
NAMELIST /time_control/ auxhist2_interval
NAMELIST /time_control/ auxhist3_interval_mo
NAMELIST /time_control/ auxhist3_interval_d
NAMELIST /time_control/ auxhist3_interval_h
NAMELIST /time_control/ auxhist3_interval_m
NAMELIST /time_control/ auxhist3_interval_s
NAMELIST /time_control/ auxhist3_interval
NAMELIST /time_control/ auxhist4_interval_mo
NAMELIST /time_control/ auxhist4_interval_d
NAMELIST /time_control/ auxhist4_interval_h
NAMELIST /time_control/ auxhist4_interval_m
NAMELIST /time_control/ auxhist4_interval_s
NAMELIST /time_control/ auxhist4_interval
NAMELIST /time_control/ auxhist5_interval_mo
NAMELIST /time_control/ auxhist5_interval_d
NAMELIST /time_control/ auxhist5_interval_h
NAMELIST /time_control/ auxhist5_interval_m
NAMELIST /time_control/ auxhist5_interval_s
NAMELIST /time_control/ auxhist5_interval
NAMELIST /time_control/ auxhist6_interval_mo
NAMELIST /time_control/ auxhist6_interval_d
NAMELIST /time_control/ auxhist6_interval_h
NAMELIST /time_control/ auxhist6_interval_m
NAMELIST /time_control/ auxhist6_interval_s
NAMELIST /time_control/ auxhist6_interval
NAMELIST /time_control/ auxhist7_interval_mo
NAMELIST /time_control/ auxhist7_interval_d
NAMELIST /time_control/ auxhist7_interval_h
NAMELIST /time_control/ auxhist7_interval_m
NAMELIST /time_control/ auxhist7_interval_s
NAMELIST /time_control/ auxhist7_interval
NAMELIST /time_control/ auxhist8_interval_mo
NAMELIST /time_control/ auxhist8_interval_d
NAMELIST /time_control/ auxhist8_interval_h
NAMELIST /time_control/ auxhist8_interval_m
NAMELIST /time_control/ auxhist8_interval_s
NAMELIST /time_control/ auxhist8_interval
NAMELIST /time_control/ auxhist9_interval_mo
NAMELIST /time_control/ auxhist9_interval_d
NAMELIST /time_control/ auxhist9_interval_h
NAMELIST /time_control/ auxhist9_interval_m
NAMELIST /time_control/ auxhist9_interval_s
NAMELIST /time_control/ auxhist9_interval
NAMELIST /time_control/ auxhist10_interval_mo
NAMELIST /time_control/ auxhist10_interval_d
NAMELIST /time_control/ auxhist10_interval_h
NAMELIST /time_control/ auxhist10_interval_m
NAMELIST /time_control/ auxhist10_interval_s
NAMELIST /time_control/ auxhist10_interval
NAMELIST /time_control/ auxhist11_interval_mo
NAMELIST /time_control/ auxhist11_interval_d
NAMELIST /time_control/ auxhist11_interval_h
NAMELIST /time_control/ auxhist11_interval_m
NAMELIST /time_control/ auxhist11_interval_s
NAMELIST /time_control/ auxhist11_interval
NAMELIST /time_control/ auxinput1_interval_mo
NAMELIST /time_control/ auxinput1_interval_d
NAMELIST /time_control/ auxinput1_interval_h
NAMELIST /time_control/ auxinput1_interval_m
NAMELIST /time_control/ auxinput1_interval_s
NAMELIST /time_control/ auxinput1_interval
NAMELIST /time_control/ auxinput2_interval_mo
NAMELIST /time_control/ auxinput2_interval_d
NAMELIST /time_control/ auxinput2_interval_h
NAMELIST /time_control/ auxinput2_interval_m
NAMELIST /time_control/ auxinput2_interval_s
NAMELIST /time_control/ auxinput2_interval
NAMELIST /time_control/ auxinput3_interval_mo
NAMELIST /time_control/ auxinput3_interval_d
NAMELIST /time_control/ auxinput3_interval_h
NAMELIST /time_control/ auxinput3_interval_m
NAMELIST /time_control/ auxinput3_interval_s
NAMELIST /time_control/ auxinput3_interval
NAMELIST /time_control/ auxinput4_interval_mo
NAMELIST /time_control/ auxinput4_interval_d
NAMELIST /time_control/ auxinput4_interval_h
NAMELIST /time_control/ auxinput4_interval_m
NAMELIST /time_control/ auxinput4_interval_s
NAMELIST /time_control/ auxinput4_interval
NAMELIST /time_control/ auxinput5_interval_mo
NAMELIST /time_control/ auxinput5_interval_d
NAMELIST /time_control/ auxinput5_interval_h
NAMELIST /time_control/ auxinput5_interval_m
NAMELIST /time_control/ auxinput5_interval_s
NAMELIST /time_control/ auxinput5_interval
NAMELIST /time_control/ auxinput6_interval_mo
NAMELIST /time_control/ auxinput6_interval_d
NAMELIST /time_control/ auxinput6_interval_h
NAMELIST /time_control/ auxinput6_interval_m
NAMELIST /time_control/ auxinput6_interval_s
NAMELIST /time_control/ auxinput6_interval
NAMELIST /time_control/ auxinput7_interval_mo
NAMELIST /time_control/ auxinput7_interval_d
NAMELIST /time_control/ auxinput7_interval_h
NAMELIST /time_control/ auxinput7_interval_m
NAMELIST /time_control/ auxinput7_interval_s
NAMELIST /time_control/ auxinput7_interval
NAMELIST /time_control/ auxinput8_interval_mo
NAMELIST /time_control/ auxinput8_interval_d
NAMELIST /time_control/ auxinput8_interval_h
NAMELIST /time_control/ auxinput8_interval_m
NAMELIST /time_control/ auxinput8_interval_s
NAMELIST /time_control/ auxinput8_interval
NAMELIST /fdda/ sgfdda_interval_mo
NAMELIST /fdda/ sgfdda_interval_d
NAMELIST /fdda/ sgfdda_interval_h
NAMELIST /fdda/ sgfdda_interval_m
NAMELIST /fdda/ sgfdda_interval_s
NAMELIST /fdda/ sgfdda_interval
NAMELIST /fdda/ gfdda_interval_mo
NAMELIST /fdda/ gfdda_interval_d
NAMELIST /fdda/ gfdda_interval_h
NAMELIST /fdda/ gfdda_interval_m
NAMELIST /fdda/ gfdda_interval_s
NAMELIST /fdda/ gfdda_interval
NAMELIST /time_control/ auxinput11_interval_mo
NAMELIST /time_control/ auxinput11_interval_d
NAMELIST /time_control/ auxinput11_interval_h
NAMELIST /time_control/ auxinput11_interval_m
NAMELIST /time_control/ auxinput11_interval_s
NAMELIST /time_control/ auxinput11_interval
NAMELIST /time_control/ restart_interval_mo
NAMELIST /time_control/ restart_interval_d
NAMELIST /time_control/ restart_interval_h
NAMELIST /time_control/ restart_interval_m
NAMELIST /time_control/ restart_interval_s
NAMELIST /time_control/ history_begin_y
NAMELIST /time_control/ history_begin_mo
NAMELIST /time_control/ history_begin_d
NAMELIST /time_control/ history_begin_h
NAMELIST /time_control/ history_begin_m
NAMELIST /time_control/ history_begin_s
NAMELIST /time_control/ inputout_begin_y
NAMELIST /time_control/ inputout_begin_mo
NAMELIST /time_control/ inputout_begin_d
NAMELIST /time_control/ inputout_begin_h
NAMELIST /time_control/ inputout_begin_m
NAMELIST /time_control/ inputout_begin_s
NAMELIST /time_control/ auxhist1_begin_y
NAMELIST /time_control/ auxhist1_begin_mo
NAMELIST /time_control/ auxhist1_begin_d
NAMELIST /time_control/ auxhist1_begin_h
NAMELIST /time_control/ auxhist1_begin_m
NAMELIST /time_control/ auxhist1_begin_s
NAMELIST /time_control/ auxhist2_begin_y
NAMELIST /time_control/ auxhist2_begin_mo
NAMELIST /time_control/ auxhist2_begin_d
NAMELIST /time_control/ auxhist2_begin_h
NAMELIST /time_control/ auxhist2_begin_m
NAMELIST /time_control/ auxhist2_begin_s
NAMELIST /time_control/ auxhist3_begin_y
NAMELIST /time_control/ auxhist3_begin_mo
NAMELIST /time_control/ auxhist3_begin_d
NAMELIST /time_control/ auxhist3_begin_h
NAMELIST /time_control/ auxhist3_begin_m
NAMELIST /time_control/ auxhist3_begin_s
NAMELIST /time_control/ auxhist4_begin_y
NAMELIST /time_control/ auxhist4_begin_mo
NAMELIST /time_control/ auxhist4_begin_d
NAMELIST /time_control/ auxhist4_begin_h
NAMELIST /time_control/ auxhist4_begin_m
NAMELIST /time_control/ auxhist4_begin_s
NAMELIST /time_control/ auxhist5_begin_y
NAMELIST /time_control/ auxhist5_begin_mo
NAMELIST /time_control/ auxhist5_begin_d
NAMELIST /time_control/ auxhist5_begin_h
NAMELIST /time_control/ auxhist5_begin_m
NAMELIST /time_control/ auxhist5_begin_s
NAMELIST /time_control/ auxhist6_begin_y
NAMELIST /time_control/ auxhist6_begin_mo
NAMELIST /time_control/ auxhist6_begin_d
NAMELIST /time_control/ auxhist6_begin_h
NAMELIST /time_control/ auxhist6_begin_m
NAMELIST /time_control/ auxhist6_begin_s
NAMELIST /time_control/ auxhist7_begin_y
NAMELIST /time_control/ auxhist7_begin_mo
NAMELIST /time_control/ auxhist7_begin_d
NAMELIST /time_control/ auxhist7_begin_h
NAMELIST /time_control/ auxhist7_begin_m
NAMELIST /time_control/ auxhist7_begin_s
NAMELIST /time_control/ auxhist8_begin_y
NAMELIST /time_control/ auxhist8_begin_mo
NAMELIST /time_control/ auxhist8_begin_d
NAMELIST /time_control/ auxhist8_begin_h
NAMELIST /time_control/ auxhist8_begin_m
NAMELIST /time_control/ auxhist8_begin_s
NAMELIST /time_control/ auxhist9_begin_y
NAMELIST /time_control/ auxhist9_begin_mo
NAMELIST /time_control/ auxhist9_begin_d
NAMELIST /time_control/ auxhist9_begin_h
NAMELIST /time_control/ auxhist9_begin_m
NAMELIST /time_control/ auxhist9_begin_s
NAMELIST /time_control/ auxhist10_begin_y
NAMELIST /time_control/ auxhist10_begin_mo
NAMELIST /time_control/ auxhist10_begin_d
NAMELIST /time_control/ auxhist10_begin_h
NAMELIST /time_control/ auxhist10_begin_m
NAMELIST /time_control/ auxhist10_begin_s
NAMELIST /time_control/ auxhist11_begin_y
NAMELIST /time_control/ auxhist11_begin_mo
NAMELIST /time_control/ auxhist11_begin_d
NAMELIST /time_control/ auxhist11_begin_h
NAMELIST /time_control/ auxhist11_begin_m
NAMELIST /time_control/ auxhist11_begin_s
NAMELIST /time_control/ auxinput1_begin_y
NAMELIST /time_control/ auxinput1_begin_mo
NAMELIST /time_control/ auxinput1_begin_d
NAMELIST /time_control/ auxinput1_begin_h
NAMELIST /time_control/ auxinput1_begin_m
NAMELIST /time_control/ auxinput1_begin_s
NAMELIST /time_control/ auxinput2_begin_y
NAMELIST /time_control/ auxinput2_begin_mo
NAMELIST /time_control/ auxinput2_begin_d
NAMELIST /time_control/ auxinput2_begin_h
NAMELIST /time_control/ auxinput2_begin_m
NAMELIST /time_control/ auxinput2_begin_s
NAMELIST /time_control/ auxinput3_begin_y
NAMELIST /time_control/ auxinput3_begin_mo
NAMELIST /time_control/ auxinput3_begin_d
NAMELIST /time_control/ auxinput3_begin_h
NAMELIST /time_control/ auxinput3_begin_m
NAMELIST /time_control/ auxinput3_begin_s
NAMELIST /time_control/ auxinput4_begin_y
NAMELIST /time_control/ auxinput4_begin_mo
NAMELIST /time_control/ auxinput4_begin_d
NAMELIST /time_control/ auxinput4_begin_h
NAMELIST /time_control/ auxinput4_begin_m
NAMELIST /time_control/ auxinput4_begin_s
NAMELIST /time_control/ auxinput5_begin_y
NAMELIST /time_control/ auxinput5_begin_mo
NAMELIST /time_control/ auxinput5_begin_d
NAMELIST /time_control/ auxinput5_begin_h
NAMELIST /time_control/ auxinput5_begin_m
NAMELIST /time_control/ auxinput5_begin_s
NAMELIST /time_control/ auxinput6_begin_y
NAMELIST /time_control/ auxinput6_begin_mo
NAMELIST /time_control/ auxinput6_begin_d
NAMELIST /time_control/ auxinput6_begin_h
NAMELIST /time_control/ auxinput6_begin_m
NAMELIST /time_control/ auxinput6_begin_s
NAMELIST /time_control/ auxinput7_begin_y
NAMELIST /time_control/ auxinput7_begin_mo
NAMELIST /time_control/ auxinput7_begin_d
NAMELIST /time_control/ auxinput7_begin_h
NAMELIST /time_control/ auxinput7_begin_m
NAMELIST /time_control/ auxinput7_begin_s
NAMELIST /time_control/ auxinput8_begin_y
NAMELIST /time_control/ auxinput8_begin_mo
NAMELIST /time_control/ auxinput8_begin_d
NAMELIST /time_control/ auxinput8_begin_h
NAMELIST /time_control/ auxinput8_begin_m
NAMELIST /time_control/ auxinput8_begin_s
NAMELIST /fdda/ sgfdda_begin_y
NAMELIST /fdda/ sgfdda_begin_mo
NAMELIST /fdda/ sgfdda_begin_d
NAMELIST /fdda/ sgfdda_begin_h
NAMELIST /fdda/ sgfdda_begin_m
NAMELIST /fdda/ sgfdda_begin_s
NAMELIST /fdda/ gfdda_begin_y
NAMELIST /fdda/ gfdda_begin_mo
NAMELIST /fdda/ gfdda_begin_d
NAMELIST /fdda/ gfdda_begin_h
NAMELIST /fdda/ gfdda_begin_m
NAMELIST /fdda/ gfdda_begin_s
NAMELIST /time_control/ auxinput11_begin_y
NAMELIST /time_control/ auxinput11_begin_mo
NAMELIST /time_control/ auxinput11_begin_d
NAMELIST /time_control/ auxinput11_begin_h
NAMELIST /time_control/ auxinput11_begin_m
NAMELIST /time_control/ auxinput11_begin_s
NAMELIST /time_control/ restart_begin_y
NAMELIST /time_control/ restart_begin_mo
NAMELIST /time_control/ restart_begin_d
NAMELIST /time_control/ restart_begin_h
NAMELIST /time_control/ restart_begin_m
NAMELIST /time_control/ restart_begin_s
NAMELIST /time_control/ history_end_y
NAMELIST /time_control/ history_end_mo
NAMELIST /time_control/ history_end_d
NAMELIST /time_control/ history_end_h
NAMELIST /time_control/ history_end_m
NAMELIST /time_control/ history_end_s
NAMELIST /time_control/ inputout_end_y
NAMELIST /time_control/ inputout_end_mo
NAMELIST /time_control/ inputout_end_d
NAMELIST /time_control/ inputout_end_h
NAMELIST /time_control/ inputout_end_m
NAMELIST /time_control/ inputout_end_s
NAMELIST /time_control/ auxhist1_end_y
NAMELIST /time_control/ auxhist1_end_mo
NAMELIST /time_control/ auxhist1_end_d
NAMELIST /time_control/ auxhist1_end_h
NAMELIST /time_control/ auxhist1_end_m
NAMELIST /time_control/ auxhist1_end_s
NAMELIST /time_control/ auxhist2_end_y
NAMELIST /time_control/ auxhist2_end_mo
NAMELIST /time_control/ auxhist2_end_d
NAMELIST /time_control/ auxhist2_end_h
NAMELIST /time_control/ auxhist2_end_m
NAMELIST /time_control/ auxhist2_end_s
NAMELIST /time_control/ auxhist3_end_y
NAMELIST /time_control/ auxhist3_end_mo
NAMELIST /time_control/ auxhist3_end_d
NAMELIST /time_control/ auxhist3_end_h
NAMELIST /time_control/ auxhist3_end_m
NAMELIST /time_control/ auxhist3_end_s
NAMELIST /time_control/ auxhist4_end_y
NAMELIST /time_control/ auxhist4_end_mo
NAMELIST /time_control/ auxhist4_end_d
NAMELIST /time_control/ auxhist4_end_h
NAMELIST /time_control/ auxhist4_end_m
NAMELIST /time_control/ auxhist4_end_s
NAMELIST /time_control/ auxhist5_end_y
NAMELIST /time_control/ auxhist5_end_mo
NAMELIST /time_control/ auxhist5_end_d
NAMELIST /time_control/ auxhist5_end_h
NAMELIST /time_control/ auxhist5_end_m
NAMELIST /time_control/ auxhist5_end_s
NAMELIST /time_control/ auxhist6_end_y
NAMELIST /time_control/ auxhist6_end_mo
NAMELIST /time_control/ auxhist6_end_d
NAMELIST /time_control/ auxhist6_end_h
NAMELIST /time_control/ auxhist6_end_m
NAMELIST /time_control/ auxhist6_end_s
NAMELIST /time_control/ auxhist7_end_y
NAMELIST /time_control/ auxhist7_end_mo
NAMELIST /time_control/ auxhist7_end_d
NAMELIST /time_control/ auxhist7_end_h
NAMELIST /time_control/ auxhist7_end_m
NAMELIST /time_control/ auxhist7_end_s
NAMELIST /time_control/ auxhist8_end_y
NAMELIST /time_control/ auxhist8_end_mo
NAMELIST /time_control/ auxhist8_end_d
NAMELIST /time_control/ auxhist8_end_h
NAMELIST /time_control/ auxhist8_end_m
NAMELIST /time_control/ auxhist8_end_s
NAMELIST /time_control/ auxhist9_end_y
NAMELIST /time_control/ auxhist9_end_mo
NAMELIST /time_control/ auxhist9_end_d
NAMELIST /time_control/ auxhist9_end_h
NAMELIST /time_control/ auxhist9_end_m
NAMELIST /time_control/ auxhist9_end_s
NAMELIST /time_control/ auxhist10_end_y
NAMELIST /time_control/ auxhist10_end_mo
NAMELIST /time_control/ auxhist10_end_d
NAMELIST /time_control/ auxhist10_end_h
NAMELIST /time_control/ auxhist10_end_m
NAMELIST /time_control/ auxhist10_end_s
NAMELIST /time_control/ auxhist11_end_y
NAMELIST /time_control/ auxhist11_end_mo
NAMELIST /time_control/ auxhist11_end_d
NAMELIST /time_control/ auxhist11_end_h
NAMELIST /time_control/ auxhist11_end_m
NAMELIST /time_control/ auxhist11_end_s
NAMELIST /time_control/ auxinput1_end_y
NAMELIST /time_control/ auxinput1_end_mo
NAMELIST /time_control/ auxinput1_end_d
NAMELIST /time_control/ auxinput1_end_h
NAMELIST /time_control/ auxinput1_end_m
NAMELIST /time_control/ auxinput1_end_s
NAMELIST /time_control/ auxinput2_end_y
NAMELIST /time_control/ auxinput2_end_mo
NAMELIST /time_control/ auxinput2_end_d
NAMELIST /time_control/ auxinput2_end_h
NAMELIST /time_control/ auxinput2_end_m
NAMELIST /time_control/ auxinput2_end_s
NAMELIST /time_control/ auxinput3_end_y
NAMELIST /time_control/ auxinput3_end_mo
NAMELIST /time_control/ auxinput3_end_d
NAMELIST /time_control/ auxinput3_end_h
NAMELIST /time_control/ auxinput3_end_m
NAMELIST /time_control/ auxinput3_end_s
NAMELIST /time_control/ auxinput4_end_y
NAMELIST /time_control/ auxinput4_end_mo
NAMELIST /time_control/ auxinput4_end_d
NAMELIST /time_control/ auxinput4_end_h
NAMELIST /time_control/ auxinput4_end_m
NAMELIST /time_control/ auxinput4_end_s
NAMELIST /time_control/ auxinput5_end_y
NAMELIST /time_control/ auxinput5_end_mo
NAMELIST /time_control/ auxinput5_end_d
NAMELIST /time_control/ auxinput5_end_h
NAMELIST /time_control/ auxinput5_end_m
NAMELIST /time_control/ auxinput5_end_s
NAMELIST /time_control/ auxinput6_end_y
NAMELIST /time_control/ auxinput6_end_mo
NAMELIST /time_control/ auxinput6_end_d
NAMELIST /time_control/ auxinput6_end_h
NAMELIST /time_control/ auxinput6_end_m
NAMELIST /time_control/ auxinput6_end_s
NAMELIST /time_control/ auxinput7_end_y
NAMELIST /time_control/ auxinput7_end_mo
NAMELIST /time_control/ auxinput7_end_d
NAMELIST /time_control/ auxinput7_end_h
NAMELIST /time_control/ auxinput7_end_m
NAMELIST /time_control/ auxinput7_end_s
NAMELIST /time_control/ auxinput8_end_y
NAMELIST /time_control/ auxinput8_end_mo
NAMELIST /time_control/ auxinput8_end_d
NAMELIST /time_control/ auxinput8_end_h
NAMELIST /time_control/ auxinput8_end_m
NAMELIST /time_control/ auxinput8_end_s
NAMELIST /fdda/ sgfdda_end_y
NAMELIST /fdda/ sgfdda_end_mo
NAMELIST /fdda/ sgfdda_end_d
NAMELIST /fdda/ sgfdda_end_h
NAMELIST /fdda/ sgfdda_end_m
NAMELIST /fdda/ sgfdda_end_s
NAMELIST /fdda/ gfdda_end_y
NAMELIST /fdda/ gfdda_end_mo
NAMELIST /fdda/ gfdda_end_d
NAMELIST /fdda/ gfdda_end_h
NAMELIST /fdda/ gfdda_end_m
NAMELIST /fdda/ gfdda_end_s
NAMELIST /time_control/ auxinput11_end_y
NAMELIST /time_control/ auxinput11_end_mo
NAMELIST /time_control/ auxinput11_end_d
NAMELIST /time_control/ auxinput11_end_h
NAMELIST /time_control/ auxinput11_end_m
NAMELIST /time_control/ auxinput11_end_s
NAMELIST /time_control/ io_form_auxinput1
NAMELIST /time_control/ io_form_auxinput2
NAMELIST /time_control/ io_form_auxinput3
NAMELIST /time_control/ io_form_auxinput4
NAMELIST /time_control/ io_form_auxinput5
NAMELIST /time_control/ io_form_auxinput6
NAMELIST /time_control/ io_form_auxinput7
NAMELIST /time_control/ io_form_auxinput8
NAMELIST /fdda/ io_form_sgfdda
NAMELIST /fdda/ io_form_gfdda
NAMELIST /time_control/ io_form_auxinput11
NAMELIST /time_control/ io_form_auxhist1
NAMELIST /time_control/ io_form_auxhist2
NAMELIST /time_control/ io_form_auxhist3
NAMELIST /time_control/ io_form_auxhist4
NAMELIST /time_control/ io_form_auxhist5
NAMELIST /time_control/ io_form_auxhist6
NAMELIST /time_control/ io_form_auxhist7
NAMELIST /time_control/ io_form_auxhist8
NAMELIST /time_control/ io_form_auxhist9
NAMELIST /time_control/ io_form_auxhist10
NAMELIST /time_control/ io_form_auxhist11
NAMELIST /time_control/ reset_simulation_start
NAMELIST /domains/ sr_x
NAMELIST /domains/ sr_y
NAMELIST /time_control/ julyr
NAMELIST /time_control/ julday
NAMELIST /time_control/ gmt
NAMELIST /time_control/ input_inname
NAMELIST /time_control/ input_outname
NAMELIST /time_control/ bdy_inname
NAMELIST /time_control/ bdy_outname
NAMELIST /time_control/ rst_inname
NAMELIST /time_control/ rst_outname
NAMELIST /time_control/ write_input
NAMELIST /time_control/ write_restart_at_0h
NAMELIST /time_control/ adjust_output_times
NAMELIST /time_control/ adjust_input_times
NAMELIST /time_control/ tstart
NAMELIST /time_control/ nocolons
NAMELIST /time_control/ cycling
NAMELIST /dfi_control/ dfi_opt
NAMELIST /dfi_control/ dfi_nfilter
NAMELIST /dfi_control/ dfi_write_filtered_input
NAMELIST /dfi_control/ dfi_write_dfi_history
NAMELIST /dfi_control/ dfi_cutoff_seconds
NAMELIST /dfi_control/ dfi_time_dim
NAMELIST /dfi_control/ dfi_fwdstop_year
NAMELIST /dfi_control/ dfi_fwdstop_month
NAMELIST /dfi_control/ dfi_fwdstop_day
NAMELIST /dfi_control/ dfi_fwdstop_hour
NAMELIST /dfi_control/ dfi_fwdstop_minute
NAMELIST /dfi_control/ dfi_fwdstop_second
NAMELIST /dfi_control/ dfi_bckstop_year
NAMELIST /dfi_control/ dfi_bckstop_month
NAMELIST /dfi_control/ dfi_bckstop_day
NAMELIST /dfi_control/ dfi_bckstop_hour
NAMELIST /dfi_control/ dfi_bckstop_minute
NAMELIST /dfi_control/ dfi_bckstop_second
NAMELIST /domains/ time_step
NAMELIST /domains/ time_step_fract_num
NAMELIST /domains/ time_step_fract_den
NAMELIST /domains/ max_dom
NAMELIST /domains/ s_we
NAMELIST /domains/ e_we
NAMELIST /domains/ s_sn
NAMELIST /domains/ e_sn
NAMELIST /domains/ s_vert
NAMELIST /domains/ e_vert
NAMELIST /domains/ dx
NAMELIST /domains/ dy
NAMELIST /domains/ grid_id
NAMELIST /domains/ grid_allowed
NAMELIST /domains/ parent_id
NAMELIST /domains/ i_parent_start
NAMELIST /domains/ j_parent_start
NAMELIST /domains/ parent_grid_ratio
NAMELIST /domains/ parent_time_step_ratio
NAMELIST /domains/ feedback
NAMELIST /domains/ smooth_option
NAMELIST /domains/ ztop
NAMELIST /domains/ moad_grid_ratio
NAMELIST /domains/ moad_time_step_ratio
NAMELIST /domains/ shw
NAMELIST /domains/ tile_sz_x
NAMELIST /domains/ tile_sz_y
NAMELIST /domains/ numtiles
NAMELIST /domains/ nproc_x
NAMELIST /domains/ nproc_y
NAMELIST /domains/ irand
NAMELIST /domains/ ts_buf_size
NAMELIST /domains/ max_ts_locs
NAMELIST /domains/ num_moves
NAMELIST /domains/ move_id
NAMELIST /domains/ move_interval
NAMELIST /domains/ move_cd_x
NAMELIST /domains/ move_cd_y
NAMELIST /domains/ swap_x
NAMELIST /domains/ swap_y
NAMELIST /domains/ cycle_x
NAMELIST /domains/ cycle_y
NAMELIST /domains/ reorder_mesh
NAMELIST /domains/ perturb_input
NAMELIST /domains/ eta_levels
NAMELIST /domains/ ptsgm
NAMELIST /domains/ num_metgrid_levels
NAMELIST /domains/ p_top_requested
NAMELIST /physics/ mp_physics
NAMELIST /physics/ ra_lw_physics
NAMELIST /physics/ ra_sw_physics
NAMELIST /physics/ radt
NAMELIST /physics/ sf_sfclay_physics
NAMELIST /physics/ sf_surface_physics
NAMELIST /physics/ bl_pbl_physics
NAMELIST /physics/ sf_urban_physics
NAMELIST /physics/ bldt
NAMELIST /physics/ cu_physics
NAMELIST /physics/ cudt
NAMELIST /physics/ gsmdt
NAMELIST /physics/ isfflx
NAMELIST /physics/ ifsnow
NAMELIST /physics/ icloud
NAMELIST /physics/ swrad_scat
NAMELIST /physics/ surface_input_source
NAMELIST /physics/ num_soil_layers
NAMELIST /physics/ num_urban_layers
NAMELIST /physics/ maxiens
NAMELIST /physics/ maxens
NAMELIST /physics/ maxens2
NAMELIST /physics/ maxens3
NAMELIST /physics/ ensdim
NAMELIST /physics/ chem_opt
NAMELIST /physics/ num_land_cat
NAMELIST /physics/ num_soil_cat
NAMELIST /physics/ mp_zero_out
NAMELIST /physics/ mp_zero_out_thresh
NAMELIST /physics/ seaice_threshold
NAMELIST /physics/ fractional_seaice
NAMELIST /physics/ sst_update
NAMELIST /physics/ usemonalb
NAMELIST /physics/ rdmaxalb
NAMELIST /physics/ rdlai2d
NAMELIST /physics/ gwd_opt
NAMELIST /physics/ idtad
NAMELIST /physics/ nsoil
NAMELIST /physics/ nphs
NAMELIST /physics/ ncnvc
NAMELIST /physics/ nrads
NAMELIST /physics/ nradl
NAMELIST /physics/ tprec
NAMELIST /physics/ theat
NAMELIST /physics/ tclod
NAMELIST /physics/ trdsw
NAMELIST /physics/ trdlw
NAMELIST /physics/ tsrfc
NAMELIST /physics/ pcpflg
NAMELIST /physics/ sigma
NAMELIST /physics/ sfenth
NAMELIST /physics/ co2tf
NAMELIST /physics/ ra_call_offset
NAMELIST /physics/ cam_abs_freq_s
NAMELIST /physics/ levsiz
NAMELIST /physics/ paerlev
NAMELIST /physics/ cam_abs_dim1
NAMELIST /physics/ cam_abs_dim2
NAMELIST /physics/ cu_rad_feedback
NAMELIST /dynamics/ dyn_opt
NAMELIST /dynamics/ rk_ord
NAMELIST /dynamics/ w_damping
NAMELIST /dynamics/ diff_opt
NAMELIST /dynamics/ km_opt
NAMELIST /dynamics/ damp_opt
NAMELIST /dynamics/ zdamp
NAMELIST /dynamics/ base_pres
NAMELIST /dynamics/ base_temp
NAMELIST /dynamics/ base_lapse
NAMELIST /dynamics/ iso_temp
NAMELIST /dynamics/ dampcoef
NAMELIST /dynamics/ khdif
NAMELIST /dynamics/ kvdif
NAMELIST /dynamics/ c_s
NAMELIST /dynamics/ c_k
NAMELIST /dynamics/ smdiv
NAMELIST /dynamics/ emdiv
NAMELIST /dynamics/ epssm
NAMELIST /dynamics/ non_hydrostatic
NAMELIST /dynamics/ time_step_sound
NAMELIST /dynamics/ h_mom_adv_order
NAMELIST /dynamics/ v_mom_adv_order
NAMELIST /dynamics/ h_sca_adv_order
NAMELIST /dynamics/ v_sca_adv_order
NAMELIST /dynamics/ top_radiation
NAMELIST /dynamics/ tke_upper_bound
NAMELIST /dynamics/ tke_drag_coefficient
NAMELIST /dynamics/ tke_heat_flux
NAMELIST /dynamics/ pert_coriolis
NAMELIST /dynamics/ euler_adv
NAMELIST /dynamics/ idtadt
NAMELIST /dynamics/ idtadc
NAMELIST /dynamics/ boundary_flux
NAMELIST /bdy_control/ spec_bdy_width
NAMELIST /bdy_control/ spec_zone
NAMELIST /bdy_control/ relax_zone
NAMELIST /bdy_control/ specified
NAMELIST /bdy_control/ periodic_x
NAMELIST /bdy_control/ symmetric_xs
NAMELIST /bdy_control/ symmetric_xe
NAMELIST /bdy_control/ open_xs
NAMELIST /bdy_control/ open_xe
NAMELIST /bdy_control/ periodic_y
NAMELIST /bdy_control/ symmetric_ys
NAMELIST /bdy_control/ symmetric_ye
NAMELIST /bdy_control/ open_ys
NAMELIST /bdy_control/ open_ye
NAMELIST /bdy_control/ polar
NAMELIST /bdy_control/ nested
NAMELIST /bdy_control/ real_data_init_type
NAMELIST /grib2/ background_proc_id
NAMELIST /grib2/ forecast_proc_id
NAMELIST /grib2/ production_status
NAMELIST /grib2/ compression



     LOGICAL :: global
     LOGICAL :: print_detail_airep
     LOGICAL :: print_detail_timing
     NAMELIST /wrfvar1/ global
     NAMELIST /wrfvar1/ print_detail_airep
     NAMELIST /wrfvar1/ print_detail_timing





     REWIND ( UNIT = nml_read_unit )
     READ   ( UNIT = nml_read_unit , NML = wrfvar1 , iostat=nml_error )

     IF ( nml_error .EQ. 0 ) then    
        CALL wrf_debug(0,"Are global, print_detail_airep, print_detail_timing still in your "// &
                         TRIM(nml_name)//" namelist?")
        CALL wrf_error_fatal3("",12701,&
"Remove global, print_detail_airep, print_detail_timing "// &
                             "from wrfvar1 namelist as they are obsolete.")
     ELSE     
        return
     ENDIF

   END SUBROUTINE wrfvar_alt_nml_wrfvar1

   SUBROUTINE wrfvar_alt_nml_wrfvar2 (nml_read_unit, nml_name)







     IMPLICIT NONE
     INTEGER, INTENT(IN)       :: nml_read_unit
     CHARACTER*(*), INTENT(IN) :: nml_name
     INTEGER                   :: nml_error







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
character*256 :: auxinput1_inname
character*256 :: auxinput2_inname
character*256 :: auxinput3_inname
character*256 :: auxinput4_inname
character*256 :: auxinput5_inname
character*256 :: auxinput6_inname
character*256 :: auxinput7_inname
character*256 :: auxinput8_inname
character*256 :: sgfdda_inname
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
integer , DIMENSION(max_domains) :: sgfdda_interval_mo
integer , DIMENSION(max_domains) :: sgfdda_interval_d
integer , DIMENSION(max_domains) :: sgfdda_interval_h
integer , DIMENSION(max_domains) :: sgfdda_interval_m
integer , DIMENSION(max_domains) :: sgfdda_interval_s
integer , DIMENSION(max_domains) :: sgfdda_interval
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
integer , DIMENSION(max_domains) :: sgfdda_begin_y
integer , DIMENSION(max_domains) :: sgfdda_begin_mo
integer , DIMENSION(max_domains) :: sgfdda_begin_d
integer , DIMENSION(max_domains) :: sgfdda_begin_h
integer , DIMENSION(max_domains) :: sgfdda_begin_m
integer , DIMENSION(max_domains) :: sgfdda_begin_s
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
integer , DIMENSION(max_domains) :: sgfdda_end_y
integer , DIMENSION(max_domains) :: sgfdda_end_mo
integer , DIMENSION(max_domains) :: sgfdda_end_d
integer , DIMENSION(max_domains) :: sgfdda_end_h
integer , DIMENSION(max_domains) :: sgfdda_end_m
integer , DIMENSION(max_domains) :: sgfdda_end_s
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
integer :: io_form_sgfdda
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
integer :: simulation_start_year
integer :: simulation_start_month
integer :: simulation_start_day
integer :: simulation_start_hour
integer :: simulation_start_minute
integer :: simulation_start_second
logical :: reset_simulation_start
integer , DIMENSION(max_domains) :: sr_x
integer , DIMENSION(max_domains) :: sr_y
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
real , DIMENSION(max_domains) :: tstart
logical :: nocolons
logical :: cycling
integer :: dfi_opt
integer :: dfi_nfilter
logical :: dfi_write_filtered_input
logical :: dfi_write_dfi_history
integer :: dfi_cutoff_seconds
integer :: dfi_time_dim
integer :: dfi_fwdstop_year
integer :: dfi_fwdstop_month
integer :: dfi_fwdstop_day
integer :: dfi_fwdstop_hour
integer :: dfi_fwdstop_minute
integer :: dfi_fwdstop_second
integer :: dfi_bckstop_year
integer :: dfi_bckstop_month
integer :: dfi_bckstop_day
integer :: dfi_bckstop_hour
integer :: dfi_bckstop_minute
integer :: dfi_bckstop_second
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
real , DIMENSION(max_domains) :: dx
real , DIMENSION(max_domains) :: dy
integer , DIMENSION(max_domains) :: grid_id
logical , DIMENSION(max_domains) :: grid_allowed
integer , DIMENSION(max_domains) :: parent_id
integer , DIMENSION(max_domains) :: i_parent_start
integer , DIMENSION(max_domains) :: j_parent_start
integer , DIMENSION(max_domains) :: parent_grid_ratio
integer , DIMENSION(max_domains) :: parent_time_step_ratio
integer :: feedback
integer :: smooth_option
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
integer :: ts_buf_size
integer :: max_ts_locs
integer :: num_moves
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
real :: ptsgm
integer :: num_metgrid_levels
real :: p_top_requested
integer , DIMENSION(max_domains) :: mp_physics
integer , DIMENSION(max_domains) :: ra_lw_physics
integer , DIMENSION(max_domains) :: ra_sw_physics
real , DIMENSION(max_domains) :: radt
integer , DIMENSION(max_domains) :: sf_sfclay_physics
integer , DIMENSION(max_domains) :: sf_surface_physics
integer , DIMENSION(max_domains) :: bl_pbl_physics
integer , DIMENSION(max_domains) :: sf_urban_physics
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
integer :: num_urban_layers
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
integer :: fractional_seaice
integer :: sst_update
logical :: usemonalb
logical :: rdmaxalb
logical :: rdlai2d
integer , DIMENSION(max_domains) :: gwd_opt
integer , DIMENSION(max_domains) :: idtad
integer , DIMENSION(max_domains) :: nsoil
integer , DIMENSION(max_domains) :: nphs
integer , DIMENSION(max_domains) :: ncnvc
integer , DIMENSION(max_domains) :: nrads
integer , DIMENSION(max_domains) :: nradl
real , DIMENSION(max_domains) :: tprec
real , DIMENSION(max_domains) :: theat
real , DIMENSION(max_domains) :: tclod
real , DIMENSION(max_domains) :: trdsw
real , DIMENSION(max_domains) :: trdlw
real , DIMENSION(max_domains) :: tsrfc
logical , DIMENSION(max_domains) :: pcpflg
integer , DIMENSION(max_domains) :: sigma
real , DIMENSION(max_domains) :: sfenth
integer :: co2tf
integer :: ra_call_offset
real :: cam_abs_freq_s
integer :: levsiz
integer :: paerlev
integer :: cam_abs_dim1
integer :: cam_abs_dim2
logical , DIMENSION(max_domains) :: cu_rad_feedback
integer :: dyn_opt
integer :: rk_ord
integer :: w_damping
integer :: diff_opt
integer :: km_opt
integer :: damp_opt
real , DIMENSION(max_domains) :: zdamp
real :: base_pres
real :: base_temp
real :: base_lapse
real :: iso_temp
real , DIMENSION(max_domains) :: dampcoef
real , DIMENSION(max_domains) :: khdif
real , DIMENSION(max_domains) :: kvdif
real , DIMENSION(max_domains) :: c_s
real , DIMENSION(max_domains) :: c_k
real , DIMENSION(max_domains) :: smdiv
real , DIMENSION(max_domains) :: emdiv
real , DIMENSION(max_domains) :: epssm
logical , DIMENSION(max_domains) :: non_hydrostatic
integer , DIMENSION(max_domains) :: time_step_sound
integer , DIMENSION(max_domains) :: h_mom_adv_order
integer , DIMENSION(max_domains) :: v_mom_adv_order
integer , DIMENSION(max_domains) :: h_sca_adv_order
integer , DIMENSION(max_domains) :: v_sca_adv_order
logical , DIMENSION(max_domains) :: top_radiation
real , DIMENSION(max_domains) :: tke_upper_bound
real , DIMENSION(max_domains) :: tke_drag_coefficient
real , DIMENSION(max_domains) :: tke_heat_flux
logical , DIMENSION(max_domains) :: pert_coriolis
logical :: euler_adv
integer :: idtadt
integer :: idtadc
logical :: boundary_flux
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
logical , DIMENSION(max_domains) :: polar
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
integer :: flag_metgrid
integer :: flag_snow
integer :: flag_psfc
integer :: flag_sm000010
integer :: flag_sm010040
integer :: flag_sm040100
integer :: flag_sm100200
integer :: flag_st000010
integer :: flag_st010040
integer :: flag_st040100
integer :: flag_st100200
integer :: flag_slp
integer :: flag_soilhgt
integer :: flag_mf_xy
real , DIMENSION(max_domains) :: bdyfrq
character*256 , DIMENSION(max_domains) :: mminlu
integer , DIMENSION(max_domains) :: iswater
integer , DIMENSION(max_domains) :: islake
integer , DIMENSION(max_domains) :: isice
integer , DIMENSION(max_domains) :: isurban
integer , DIMENSION(max_domains) :: isoilwater
integer , DIMENSION(max_domains) :: map_proj
integer :: dfi_stage
integer , DIMENSION(max_domains) :: mp_physics_dfi
integer    :: last_item_in_struct







NAMELIST /time_control/ run_days
NAMELIST /time_control/ run_hours
NAMELIST /time_control/ run_minutes
NAMELIST /time_control/ run_seconds
NAMELIST /time_control/ start_year
NAMELIST /time_control/ start_month
NAMELIST /time_control/ start_day
NAMELIST /time_control/ start_hour
NAMELIST /time_control/ start_minute
NAMELIST /time_control/ start_second
NAMELIST /time_control/ end_year
NAMELIST /time_control/ end_month
NAMELIST /time_control/ end_day
NAMELIST /time_control/ end_hour
NAMELIST /time_control/ end_minute
NAMELIST /time_control/ end_second
NAMELIST /time_control/ interval_seconds
NAMELIST /time_control/ input_from_file
NAMELIST /time_control/ fine_input_stream
NAMELIST /time_control/ history_interval
NAMELIST /time_control/ frames_per_outfile
NAMELIST /time_control/ frames_per_auxhist1
NAMELIST /time_control/ frames_per_auxhist2
NAMELIST /time_control/ frames_per_auxhist3
NAMELIST /time_control/ frames_per_auxhist4
NAMELIST /time_control/ frames_per_auxhist5
NAMELIST /time_control/ frames_per_auxhist6
NAMELIST /time_control/ frames_per_auxhist7
NAMELIST /time_control/ frames_per_auxhist8
NAMELIST /time_control/ frames_per_auxhist9
NAMELIST /time_control/ frames_per_auxhist10
NAMELIST /time_control/ frames_per_auxhist11
NAMELIST /time_control/ restart
NAMELIST /time_control/ restart_interval
NAMELIST /time_control/ io_form_input
NAMELIST /time_control/ io_form_history
NAMELIST /time_control/ io_form_restart
NAMELIST /time_control/ io_form_boundary
NAMELIST /time_control/ debug_level
NAMELIST /time_control/ self_test_domain
NAMELIST /time_control/ history_outname
NAMELIST /time_control/ auxhist1_outname
NAMELIST /time_control/ auxhist2_outname
NAMELIST /time_control/ auxhist3_outname
NAMELIST /time_control/ auxhist4_outname
NAMELIST /time_control/ auxhist5_outname
NAMELIST /time_control/ auxhist6_outname
NAMELIST /time_control/ auxhist7_outname
NAMELIST /time_control/ auxhist8_outname
NAMELIST /time_control/ auxhist9_outname
NAMELIST /time_control/ auxhist10_outname
NAMELIST /time_control/ auxhist11_outname
NAMELIST /time_control/ history_inname
NAMELIST /time_control/ auxhist1_inname
NAMELIST /time_control/ auxhist2_inname
NAMELIST /time_control/ auxhist3_inname
NAMELIST /time_control/ auxhist4_inname
NAMELIST /time_control/ auxhist5_inname
NAMELIST /time_control/ auxhist6_inname
NAMELIST /time_control/ auxhist7_inname
NAMELIST /time_control/ auxhist8_inname
NAMELIST /time_control/ auxhist9_inname
NAMELIST /time_control/ auxhist10_inname
NAMELIST /time_control/ auxhist11_inname
NAMELIST /time_control/ auxinput1_outname
NAMELIST /time_control/ auxinput2_outname
NAMELIST /time_control/ auxinput3_outname
NAMELIST /time_control/ auxinput4_outname
NAMELIST /time_control/ auxinput5_outname
NAMELIST /time_control/ auxinput6_outname
NAMELIST /time_control/ auxinput7_outname
NAMELIST /time_control/ auxinput8_outname
NAMELIST /time_control/ auxinput9_outname
NAMELIST /time_control/ auxinput10_outname
NAMELIST /time_control/ auxinput11_outname
NAMELIST /time_control/ auxinput1_inname
NAMELIST /time_control/ auxinput2_inname
NAMELIST /time_control/ auxinput3_inname
NAMELIST /time_control/ auxinput4_inname
NAMELIST /time_control/ auxinput5_inname
NAMELIST /time_control/ auxinput6_inname
NAMELIST /time_control/ auxinput7_inname
NAMELIST /time_control/ auxinput8_inname
NAMELIST /fdda/ sgfdda_inname
NAMELIST /fdda/ gfdda_inname
NAMELIST /time_control/ auxinput11_inname
NAMELIST /time_control/ history_interval_mo
NAMELIST /time_control/ history_interval_d
NAMELIST /time_control/ history_interval_h
NAMELIST /time_control/ history_interval_m
NAMELIST /time_control/ history_interval_s
NAMELIST /time_control/ inputout_interval_mo
NAMELIST /time_control/ inputout_interval_d
NAMELIST /time_control/ inputout_interval_h
NAMELIST /time_control/ inputout_interval_m
NAMELIST /time_control/ inputout_interval_s
NAMELIST /time_control/ inputout_interval
NAMELIST /time_control/ auxhist1_interval_mo
NAMELIST /time_control/ auxhist1_interval_d
NAMELIST /time_control/ auxhist1_interval_h
NAMELIST /time_control/ auxhist1_interval_m
NAMELIST /time_control/ auxhist1_interval_s
NAMELIST /time_control/ auxhist1_interval
NAMELIST /time_control/ auxhist2_interval_mo
NAMELIST /time_control/ auxhist2_interval_d
NAMELIST /time_control/ auxhist2_interval_h
NAMELIST /time_control/ auxhist2_interval_m
NAMELIST /time_control/ auxhist2_interval_s
NAMELIST /time_control/ auxhist2_interval
NAMELIST /time_control/ auxhist3_interval_mo
NAMELIST /time_control/ auxhist3_interval_d
NAMELIST /time_control/ auxhist3_interval_h
NAMELIST /time_control/ auxhist3_interval_m
NAMELIST /time_control/ auxhist3_interval_s
NAMELIST /time_control/ auxhist3_interval
NAMELIST /time_control/ auxhist4_interval_mo
NAMELIST /time_control/ auxhist4_interval_d
NAMELIST /time_control/ auxhist4_interval_h
NAMELIST /time_control/ auxhist4_interval_m
NAMELIST /time_control/ auxhist4_interval_s
NAMELIST /time_control/ auxhist4_interval
NAMELIST /time_control/ auxhist5_interval_mo
NAMELIST /time_control/ auxhist5_interval_d
NAMELIST /time_control/ auxhist5_interval_h
NAMELIST /time_control/ auxhist5_interval_m
NAMELIST /time_control/ auxhist5_interval_s
NAMELIST /time_control/ auxhist5_interval
NAMELIST /time_control/ auxhist6_interval_mo
NAMELIST /time_control/ auxhist6_interval_d
NAMELIST /time_control/ auxhist6_interval_h
NAMELIST /time_control/ auxhist6_interval_m
NAMELIST /time_control/ auxhist6_interval_s
NAMELIST /time_control/ auxhist6_interval
NAMELIST /time_control/ auxhist7_interval_mo
NAMELIST /time_control/ auxhist7_interval_d
NAMELIST /time_control/ auxhist7_interval_h
NAMELIST /time_control/ auxhist7_interval_m
NAMELIST /time_control/ auxhist7_interval_s
NAMELIST /time_control/ auxhist7_interval
NAMELIST /time_control/ auxhist8_interval_mo
NAMELIST /time_control/ auxhist8_interval_d
NAMELIST /time_control/ auxhist8_interval_h
NAMELIST /time_control/ auxhist8_interval_m
NAMELIST /time_control/ auxhist8_interval_s
NAMELIST /time_control/ auxhist8_interval
NAMELIST /time_control/ auxhist9_interval_mo
NAMELIST /time_control/ auxhist9_interval_d
NAMELIST /time_control/ auxhist9_interval_h
NAMELIST /time_control/ auxhist9_interval_m
NAMELIST /time_control/ auxhist9_interval_s
NAMELIST /time_control/ auxhist9_interval
NAMELIST /time_control/ auxhist10_interval_mo
NAMELIST /time_control/ auxhist10_interval_d
NAMELIST /time_control/ auxhist10_interval_h
NAMELIST /time_control/ auxhist10_interval_m
NAMELIST /time_control/ auxhist10_interval_s
NAMELIST /time_control/ auxhist10_interval
NAMELIST /time_control/ auxhist11_interval_mo
NAMELIST /time_control/ auxhist11_interval_d
NAMELIST /time_control/ auxhist11_interval_h
NAMELIST /time_control/ auxhist11_interval_m
NAMELIST /time_control/ auxhist11_interval_s
NAMELIST /time_control/ auxhist11_interval
NAMELIST /time_control/ auxinput1_interval_mo
NAMELIST /time_control/ auxinput1_interval_d
NAMELIST /time_control/ auxinput1_interval_h
NAMELIST /time_control/ auxinput1_interval_m
NAMELIST /time_control/ auxinput1_interval_s
NAMELIST /time_control/ auxinput1_interval
NAMELIST /time_control/ auxinput2_interval_mo
NAMELIST /time_control/ auxinput2_interval_d
NAMELIST /time_control/ auxinput2_interval_h
NAMELIST /time_control/ auxinput2_interval_m
NAMELIST /time_control/ auxinput2_interval_s
NAMELIST /time_control/ auxinput2_interval
NAMELIST /time_control/ auxinput3_interval_mo
NAMELIST /time_control/ auxinput3_interval_d
NAMELIST /time_control/ auxinput3_interval_h
NAMELIST /time_control/ auxinput3_interval_m
NAMELIST /time_control/ auxinput3_interval_s
NAMELIST /time_control/ auxinput3_interval
NAMELIST /time_control/ auxinput4_interval_mo
NAMELIST /time_control/ auxinput4_interval_d
NAMELIST /time_control/ auxinput4_interval_h
NAMELIST /time_control/ auxinput4_interval_m
NAMELIST /time_control/ auxinput4_interval_s
NAMELIST /time_control/ auxinput4_interval
NAMELIST /time_control/ auxinput5_interval_mo
NAMELIST /time_control/ auxinput5_interval_d
NAMELIST /time_control/ auxinput5_interval_h
NAMELIST /time_control/ auxinput5_interval_m
NAMELIST /time_control/ auxinput5_interval_s
NAMELIST /time_control/ auxinput5_interval
NAMELIST /time_control/ auxinput6_interval_mo
NAMELIST /time_control/ auxinput6_interval_d
NAMELIST /time_control/ auxinput6_interval_h
NAMELIST /time_control/ auxinput6_interval_m
NAMELIST /time_control/ auxinput6_interval_s
NAMELIST /time_control/ auxinput6_interval
NAMELIST /time_control/ auxinput7_interval_mo
NAMELIST /time_control/ auxinput7_interval_d
NAMELIST /time_control/ auxinput7_interval_h
NAMELIST /time_control/ auxinput7_interval_m
NAMELIST /time_control/ auxinput7_interval_s
NAMELIST /time_control/ auxinput7_interval
NAMELIST /time_control/ auxinput8_interval_mo
NAMELIST /time_control/ auxinput8_interval_d
NAMELIST /time_control/ auxinput8_interval_h
NAMELIST /time_control/ auxinput8_interval_m
NAMELIST /time_control/ auxinput8_interval_s
NAMELIST /time_control/ auxinput8_interval
NAMELIST /fdda/ sgfdda_interval_mo
NAMELIST /fdda/ sgfdda_interval_d
NAMELIST /fdda/ sgfdda_interval_h
NAMELIST /fdda/ sgfdda_interval_m
NAMELIST /fdda/ sgfdda_interval_s
NAMELIST /fdda/ sgfdda_interval
NAMELIST /fdda/ gfdda_interval_mo
NAMELIST /fdda/ gfdda_interval_d
NAMELIST /fdda/ gfdda_interval_h
NAMELIST /fdda/ gfdda_interval_m
NAMELIST /fdda/ gfdda_interval_s
NAMELIST /fdda/ gfdda_interval
NAMELIST /time_control/ auxinput11_interval_mo
NAMELIST /time_control/ auxinput11_interval_d
NAMELIST /time_control/ auxinput11_interval_h
NAMELIST /time_control/ auxinput11_interval_m
NAMELIST /time_control/ auxinput11_interval_s
NAMELIST /time_control/ auxinput11_interval
NAMELIST /time_control/ restart_interval_mo
NAMELIST /time_control/ restart_interval_d
NAMELIST /time_control/ restart_interval_h
NAMELIST /time_control/ restart_interval_m
NAMELIST /time_control/ restart_interval_s
NAMELIST /time_control/ history_begin_y
NAMELIST /time_control/ history_begin_mo
NAMELIST /time_control/ history_begin_d
NAMELIST /time_control/ history_begin_h
NAMELIST /time_control/ history_begin_m
NAMELIST /time_control/ history_begin_s
NAMELIST /time_control/ inputout_begin_y
NAMELIST /time_control/ inputout_begin_mo
NAMELIST /time_control/ inputout_begin_d
NAMELIST /time_control/ inputout_begin_h
NAMELIST /time_control/ inputout_begin_m
NAMELIST /time_control/ inputout_begin_s
NAMELIST /time_control/ auxhist1_begin_y
NAMELIST /time_control/ auxhist1_begin_mo
NAMELIST /time_control/ auxhist1_begin_d
NAMELIST /time_control/ auxhist1_begin_h
NAMELIST /time_control/ auxhist1_begin_m
NAMELIST /time_control/ auxhist1_begin_s
NAMELIST /time_control/ auxhist2_begin_y
NAMELIST /time_control/ auxhist2_begin_mo
NAMELIST /time_control/ auxhist2_begin_d
NAMELIST /time_control/ auxhist2_begin_h
NAMELIST /time_control/ auxhist2_begin_m
NAMELIST /time_control/ auxhist2_begin_s
NAMELIST /time_control/ auxhist3_begin_y
NAMELIST /time_control/ auxhist3_begin_mo
NAMELIST /time_control/ auxhist3_begin_d
NAMELIST /time_control/ auxhist3_begin_h
NAMELIST /time_control/ auxhist3_begin_m
NAMELIST /time_control/ auxhist3_begin_s
NAMELIST /time_control/ auxhist4_begin_y
NAMELIST /time_control/ auxhist4_begin_mo
NAMELIST /time_control/ auxhist4_begin_d
NAMELIST /time_control/ auxhist4_begin_h
NAMELIST /time_control/ auxhist4_begin_m
NAMELIST /time_control/ auxhist4_begin_s
NAMELIST /time_control/ auxhist5_begin_y
NAMELIST /time_control/ auxhist5_begin_mo
NAMELIST /time_control/ auxhist5_begin_d
NAMELIST /time_control/ auxhist5_begin_h
NAMELIST /time_control/ auxhist5_begin_m
NAMELIST /time_control/ auxhist5_begin_s
NAMELIST /time_control/ auxhist6_begin_y
NAMELIST /time_control/ auxhist6_begin_mo
NAMELIST /time_control/ auxhist6_begin_d
NAMELIST /time_control/ auxhist6_begin_h
NAMELIST /time_control/ auxhist6_begin_m
NAMELIST /time_control/ auxhist6_begin_s
NAMELIST /time_control/ auxhist7_begin_y
NAMELIST /time_control/ auxhist7_begin_mo
NAMELIST /time_control/ auxhist7_begin_d
NAMELIST /time_control/ auxhist7_begin_h
NAMELIST /time_control/ auxhist7_begin_m
NAMELIST /time_control/ auxhist7_begin_s
NAMELIST /time_control/ auxhist8_begin_y
NAMELIST /time_control/ auxhist8_begin_mo
NAMELIST /time_control/ auxhist8_begin_d
NAMELIST /time_control/ auxhist8_begin_h
NAMELIST /time_control/ auxhist8_begin_m
NAMELIST /time_control/ auxhist8_begin_s
NAMELIST /time_control/ auxhist9_begin_y
NAMELIST /time_control/ auxhist9_begin_mo
NAMELIST /time_control/ auxhist9_begin_d
NAMELIST /time_control/ auxhist9_begin_h
NAMELIST /time_control/ auxhist9_begin_m
NAMELIST /time_control/ auxhist9_begin_s
NAMELIST /time_control/ auxhist10_begin_y
NAMELIST /time_control/ auxhist10_begin_mo
NAMELIST /time_control/ auxhist10_begin_d
NAMELIST /time_control/ auxhist10_begin_h
NAMELIST /time_control/ auxhist10_begin_m
NAMELIST /time_control/ auxhist10_begin_s
NAMELIST /time_control/ auxhist11_begin_y
NAMELIST /time_control/ auxhist11_begin_mo
NAMELIST /time_control/ auxhist11_begin_d
NAMELIST /time_control/ auxhist11_begin_h
NAMELIST /time_control/ auxhist11_begin_m
NAMELIST /time_control/ auxhist11_begin_s
NAMELIST /time_control/ auxinput1_begin_y
NAMELIST /time_control/ auxinput1_begin_mo
NAMELIST /time_control/ auxinput1_begin_d
NAMELIST /time_control/ auxinput1_begin_h
NAMELIST /time_control/ auxinput1_begin_m
NAMELIST /time_control/ auxinput1_begin_s
NAMELIST /time_control/ auxinput2_begin_y
NAMELIST /time_control/ auxinput2_begin_mo
NAMELIST /time_control/ auxinput2_begin_d
NAMELIST /time_control/ auxinput2_begin_h
NAMELIST /time_control/ auxinput2_begin_m
NAMELIST /time_control/ auxinput2_begin_s
NAMELIST /time_control/ auxinput3_begin_y
NAMELIST /time_control/ auxinput3_begin_mo
NAMELIST /time_control/ auxinput3_begin_d
NAMELIST /time_control/ auxinput3_begin_h
NAMELIST /time_control/ auxinput3_begin_m
NAMELIST /time_control/ auxinput3_begin_s
NAMELIST /time_control/ auxinput4_begin_y
NAMELIST /time_control/ auxinput4_begin_mo
NAMELIST /time_control/ auxinput4_begin_d
NAMELIST /time_control/ auxinput4_begin_h
NAMELIST /time_control/ auxinput4_begin_m
NAMELIST /time_control/ auxinput4_begin_s
NAMELIST /time_control/ auxinput5_begin_y
NAMELIST /time_control/ auxinput5_begin_mo
NAMELIST /time_control/ auxinput5_begin_d
NAMELIST /time_control/ auxinput5_begin_h
NAMELIST /time_control/ auxinput5_begin_m
NAMELIST /time_control/ auxinput5_begin_s
NAMELIST /time_control/ auxinput6_begin_y
NAMELIST /time_control/ auxinput6_begin_mo
NAMELIST /time_control/ auxinput6_begin_d
NAMELIST /time_control/ auxinput6_begin_h
NAMELIST /time_control/ auxinput6_begin_m
NAMELIST /time_control/ auxinput6_begin_s
NAMELIST /time_control/ auxinput7_begin_y
NAMELIST /time_control/ auxinput7_begin_mo
NAMELIST /time_control/ auxinput7_begin_d
NAMELIST /time_control/ auxinput7_begin_h
NAMELIST /time_control/ auxinput7_begin_m
NAMELIST /time_control/ auxinput7_begin_s
NAMELIST /time_control/ auxinput8_begin_y
NAMELIST /time_control/ auxinput8_begin_mo
NAMELIST /time_control/ auxinput8_begin_d
NAMELIST /time_control/ auxinput8_begin_h
NAMELIST /time_control/ auxinput8_begin_m
NAMELIST /time_control/ auxinput8_begin_s
NAMELIST /fdda/ sgfdda_begin_y
NAMELIST /fdda/ sgfdda_begin_mo
NAMELIST /fdda/ sgfdda_begin_d
NAMELIST /fdda/ sgfdda_begin_h
NAMELIST /fdda/ sgfdda_begin_m
NAMELIST /fdda/ sgfdda_begin_s
NAMELIST /fdda/ gfdda_begin_y
NAMELIST /fdda/ gfdda_begin_mo
NAMELIST /fdda/ gfdda_begin_d
NAMELIST /fdda/ gfdda_begin_h
NAMELIST /fdda/ gfdda_begin_m
NAMELIST /fdda/ gfdda_begin_s
NAMELIST /time_control/ auxinput11_begin_y
NAMELIST /time_control/ auxinput11_begin_mo
NAMELIST /time_control/ auxinput11_begin_d
NAMELIST /time_control/ auxinput11_begin_h
NAMELIST /time_control/ auxinput11_begin_m
NAMELIST /time_control/ auxinput11_begin_s
NAMELIST /time_control/ restart_begin_y
NAMELIST /time_control/ restart_begin_mo
NAMELIST /time_control/ restart_begin_d
NAMELIST /time_control/ restart_begin_h
NAMELIST /time_control/ restart_begin_m
NAMELIST /time_control/ restart_begin_s
NAMELIST /time_control/ history_end_y
NAMELIST /time_control/ history_end_mo
NAMELIST /time_control/ history_end_d
NAMELIST /time_control/ history_end_h
NAMELIST /time_control/ history_end_m
NAMELIST /time_control/ history_end_s
NAMELIST /time_control/ inputout_end_y
NAMELIST /time_control/ inputout_end_mo
NAMELIST /time_control/ inputout_end_d
NAMELIST /time_control/ inputout_end_h
NAMELIST /time_control/ inputout_end_m
NAMELIST /time_control/ inputout_end_s
NAMELIST /time_control/ auxhist1_end_y
NAMELIST /time_control/ auxhist1_end_mo
NAMELIST /time_control/ auxhist1_end_d
NAMELIST /time_control/ auxhist1_end_h
NAMELIST /time_control/ auxhist1_end_m
NAMELIST /time_control/ auxhist1_end_s
NAMELIST /time_control/ auxhist2_end_y
NAMELIST /time_control/ auxhist2_end_mo
NAMELIST /time_control/ auxhist2_end_d
NAMELIST /time_control/ auxhist2_end_h
NAMELIST /time_control/ auxhist2_end_m
NAMELIST /time_control/ auxhist2_end_s
NAMELIST /time_control/ auxhist3_end_y
NAMELIST /time_control/ auxhist3_end_mo
NAMELIST /time_control/ auxhist3_end_d
NAMELIST /time_control/ auxhist3_end_h
NAMELIST /time_control/ auxhist3_end_m
NAMELIST /time_control/ auxhist3_end_s
NAMELIST /time_control/ auxhist4_end_y
NAMELIST /time_control/ auxhist4_end_mo
NAMELIST /time_control/ auxhist4_end_d
NAMELIST /time_control/ auxhist4_end_h
NAMELIST /time_control/ auxhist4_end_m
NAMELIST /time_control/ auxhist4_end_s
NAMELIST /time_control/ auxhist5_end_y
NAMELIST /time_control/ auxhist5_end_mo
NAMELIST /time_control/ auxhist5_end_d
NAMELIST /time_control/ auxhist5_end_h
NAMELIST /time_control/ auxhist5_end_m
NAMELIST /time_control/ auxhist5_end_s
NAMELIST /time_control/ auxhist6_end_y
NAMELIST /time_control/ auxhist6_end_mo
NAMELIST /time_control/ auxhist6_end_d
NAMELIST /time_control/ auxhist6_end_h
NAMELIST /time_control/ auxhist6_end_m
NAMELIST /time_control/ auxhist6_end_s
NAMELIST /time_control/ auxhist7_end_y
NAMELIST /time_control/ auxhist7_end_mo
NAMELIST /time_control/ auxhist7_end_d
NAMELIST /time_control/ auxhist7_end_h
NAMELIST /time_control/ auxhist7_end_m
NAMELIST /time_control/ auxhist7_end_s
NAMELIST /time_control/ auxhist8_end_y
NAMELIST /time_control/ auxhist8_end_mo
NAMELIST /time_control/ auxhist8_end_d
NAMELIST /time_control/ auxhist8_end_h
NAMELIST /time_control/ auxhist8_end_m
NAMELIST /time_control/ auxhist8_end_s
NAMELIST /time_control/ auxhist9_end_y
NAMELIST /time_control/ auxhist9_end_mo
NAMELIST /time_control/ auxhist9_end_d
NAMELIST /time_control/ auxhist9_end_h
NAMELIST /time_control/ auxhist9_end_m
NAMELIST /time_control/ auxhist9_end_s
NAMELIST /time_control/ auxhist10_end_y
NAMELIST /time_control/ auxhist10_end_mo
NAMELIST /time_control/ auxhist10_end_d
NAMELIST /time_control/ auxhist10_end_h
NAMELIST /time_control/ auxhist10_end_m
NAMELIST /time_control/ auxhist10_end_s
NAMELIST /time_control/ auxhist11_end_y
NAMELIST /time_control/ auxhist11_end_mo
NAMELIST /time_control/ auxhist11_end_d
NAMELIST /time_control/ auxhist11_end_h
NAMELIST /time_control/ auxhist11_end_m
NAMELIST /time_control/ auxhist11_end_s
NAMELIST /time_control/ auxinput1_end_y
NAMELIST /time_control/ auxinput1_end_mo
NAMELIST /time_control/ auxinput1_end_d
NAMELIST /time_control/ auxinput1_end_h
NAMELIST /time_control/ auxinput1_end_m
NAMELIST /time_control/ auxinput1_end_s
NAMELIST /time_control/ auxinput2_end_y
NAMELIST /time_control/ auxinput2_end_mo
NAMELIST /time_control/ auxinput2_end_d
NAMELIST /time_control/ auxinput2_end_h
NAMELIST /time_control/ auxinput2_end_m
NAMELIST /time_control/ auxinput2_end_s
NAMELIST /time_control/ auxinput3_end_y
NAMELIST /time_control/ auxinput3_end_mo
NAMELIST /time_control/ auxinput3_end_d
NAMELIST /time_control/ auxinput3_end_h
NAMELIST /time_control/ auxinput3_end_m
NAMELIST /time_control/ auxinput3_end_s
NAMELIST /time_control/ auxinput4_end_y
NAMELIST /time_control/ auxinput4_end_mo
NAMELIST /time_control/ auxinput4_end_d
NAMELIST /time_control/ auxinput4_end_h
NAMELIST /time_control/ auxinput4_end_m
NAMELIST /time_control/ auxinput4_end_s
NAMELIST /time_control/ auxinput5_end_y
NAMELIST /time_control/ auxinput5_end_mo
NAMELIST /time_control/ auxinput5_end_d
NAMELIST /time_control/ auxinput5_end_h
NAMELIST /time_control/ auxinput5_end_m
NAMELIST /time_control/ auxinput5_end_s
NAMELIST /time_control/ auxinput6_end_y
NAMELIST /time_control/ auxinput6_end_mo
NAMELIST /time_control/ auxinput6_end_d
NAMELIST /time_control/ auxinput6_end_h
NAMELIST /time_control/ auxinput6_end_m
NAMELIST /time_control/ auxinput6_end_s
NAMELIST /time_control/ auxinput7_end_y
NAMELIST /time_control/ auxinput7_end_mo
NAMELIST /time_control/ auxinput7_end_d
NAMELIST /time_control/ auxinput7_end_h
NAMELIST /time_control/ auxinput7_end_m
NAMELIST /time_control/ auxinput7_end_s
NAMELIST /time_control/ auxinput8_end_y
NAMELIST /time_control/ auxinput8_end_mo
NAMELIST /time_control/ auxinput8_end_d
NAMELIST /time_control/ auxinput8_end_h
NAMELIST /time_control/ auxinput8_end_m
NAMELIST /time_control/ auxinput8_end_s
NAMELIST /fdda/ sgfdda_end_y
NAMELIST /fdda/ sgfdda_end_mo
NAMELIST /fdda/ sgfdda_end_d
NAMELIST /fdda/ sgfdda_end_h
NAMELIST /fdda/ sgfdda_end_m
NAMELIST /fdda/ sgfdda_end_s
NAMELIST /fdda/ gfdda_end_y
NAMELIST /fdda/ gfdda_end_mo
NAMELIST /fdda/ gfdda_end_d
NAMELIST /fdda/ gfdda_end_h
NAMELIST /fdda/ gfdda_end_m
NAMELIST /fdda/ gfdda_end_s
NAMELIST /time_control/ auxinput11_end_y
NAMELIST /time_control/ auxinput11_end_mo
NAMELIST /time_control/ auxinput11_end_d
NAMELIST /time_control/ auxinput11_end_h
NAMELIST /time_control/ auxinput11_end_m
NAMELIST /time_control/ auxinput11_end_s
NAMELIST /time_control/ io_form_auxinput1
NAMELIST /time_control/ io_form_auxinput2
NAMELIST /time_control/ io_form_auxinput3
NAMELIST /time_control/ io_form_auxinput4
NAMELIST /time_control/ io_form_auxinput5
NAMELIST /time_control/ io_form_auxinput6
NAMELIST /time_control/ io_form_auxinput7
NAMELIST /time_control/ io_form_auxinput8
NAMELIST /fdda/ io_form_sgfdda
NAMELIST /fdda/ io_form_gfdda
NAMELIST /time_control/ io_form_auxinput11
NAMELIST /time_control/ io_form_auxhist1
NAMELIST /time_control/ io_form_auxhist2
NAMELIST /time_control/ io_form_auxhist3
NAMELIST /time_control/ io_form_auxhist4
NAMELIST /time_control/ io_form_auxhist5
NAMELIST /time_control/ io_form_auxhist6
NAMELIST /time_control/ io_form_auxhist7
NAMELIST /time_control/ io_form_auxhist8
NAMELIST /time_control/ io_form_auxhist9
NAMELIST /time_control/ io_form_auxhist10
NAMELIST /time_control/ io_form_auxhist11
NAMELIST /time_control/ reset_simulation_start
NAMELIST /domains/ sr_x
NAMELIST /domains/ sr_y
NAMELIST /time_control/ julyr
NAMELIST /time_control/ julday
NAMELIST /time_control/ gmt
NAMELIST /time_control/ input_inname
NAMELIST /time_control/ input_outname
NAMELIST /time_control/ bdy_inname
NAMELIST /time_control/ bdy_outname
NAMELIST /time_control/ rst_inname
NAMELIST /time_control/ rst_outname
NAMELIST /time_control/ write_input
NAMELIST /time_control/ write_restart_at_0h
NAMELIST /time_control/ adjust_output_times
NAMELIST /time_control/ adjust_input_times
NAMELIST /time_control/ tstart
NAMELIST /time_control/ nocolons
NAMELIST /time_control/ cycling
NAMELIST /dfi_control/ dfi_opt
NAMELIST /dfi_control/ dfi_nfilter
NAMELIST /dfi_control/ dfi_write_filtered_input
NAMELIST /dfi_control/ dfi_write_dfi_history
NAMELIST /dfi_control/ dfi_cutoff_seconds
NAMELIST /dfi_control/ dfi_time_dim
NAMELIST /dfi_control/ dfi_fwdstop_year
NAMELIST /dfi_control/ dfi_fwdstop_month
NAMELIST /dfi_control/ dfi_fwdstop_day
NAMELIST /dfi_control/ dfi_fwdstop_hour
NAMELIST /dfi_control/ dfi_fwdstop_minute
NAMELIST /dfi_control/ dfi_fwdstop_second
NAMELIST /dfi_control/ dfi_bckstop_year
NAMELIST /dfi_control/ dfi_bckstop_month
NAMELIST /dfi_control/ dfi_bckstop_day
NAMELIST /dfi_control/ dfi_bckstop_hour
NAMELIST /dfi_control/ dfi_bckstop_minute
NAMELIST /dfi_control/ dfi_bckstop_second
NAMELIST /domains/ time_step
NAMELIST /domains/ time_step_fract_num
NAMELIST /domains/ time_step_fract_den
NAMELIST /domains/ max_dom
NAMELIST /domains/ s_we
NAMELIST /domains/ e_we
NAMELIST /domains/ s_sn
NAMELIST /domains/ e_sn
NAMELIST /domains/ s_vert
NAMELIST /domains/ e_vert
NAMELIST /domains/ dx
NAMELIST /domains/ dy
NAMELIST /domains/ grid_id
NAMELIST /domains/ grid_allowed
NAMELIST /domains/ parent_id
NAMELIST /domains/ i_parent_start
NAMELIST /domains/ j_parent_start
NAMELIST /domains/ parent_grid_ratio
NAMELIST /domains/ parent_time_step_ratio
NAMELIST /domains/ feedback
NAMELIST /domains/ smooth_option
NAMELIST /domains/ ztop
NAMELIST /domains/ moad_grid_ratio
NAMELIST /domains/ moad_time_step_ratio
NAMELIST /domains/ shw
NAMELIST /domains/ tile_sz_x
NAMELIST /domains/ tile_sz_y
NAMELIST /domains/ numtiles
NAMELIST /domains/ nproc_x
NAMELIST /domains/ nproc_y
NAMELIST /domains/ irand
NAMELIST /domains/ ts_buf_size
NAMELIST /domains/ max_ts_locs
NAMELIST /domains/ num_moves
NAMELIST /domains/ move_id
NAMELIST /domains/ move_interval
NAMELIST /domains/ move_cd_x
NAMELIST /domains/ move_cd_y
NAMELIST /domains/ swap_x
NAMELIST /domains/ swap_y
NAMELIST /domains/ cycle_x
NAMELIST /domains/ cycle_y
NAMELIST /domains/ reorder_mesh
NAMELIST /domains/ perturb_input
NAMELIST /domains/ eta_levels
NAMELIST /domains/ ptsgm
NAMELIST /domains/ num_metgrid_levels
NAMELIST /domains/ p_top_requested
NAMELIST /physics/ mp_physics
NAMELIST /physics/ ra_lw_physics
NAMELIST /physics/ ra_sw_physics
NAMELIST /physics/ radt
NAMELIST /physics/ sf_sfclay_physics
NAMELIST /physics/ sf_surface_physics
NAMELIST /physics/ bl_pbl_physics
NAMELIST /physics/ sf_urban_physics
NAMELIST /physics/ bldt
NAMELIST /physics/ cu_physics
NAMELIST /physics/ cudt
NAMELIST /physics/ gsmdt
NAMELIST /physics/ isfflx
NAMELIST /physics/ ifsnow
NAMELIST /physics/ icloud
NAMELIST /physics/ swrad_scat
NAMELIST /physics/ surface_input_source
NAMELIST /physics/ num_soil_layers
NAMELIST /physics/ num_urban_layers
NAMELIST /physics/ maxiens
NAMELIST /physics/ maxens
NAMELIST /physics/ maxens2
NAMELIST /physics/ maxens3
NAMELIST /physics/ ensdim
NAMELIST /physics/ chem_opt
NAMELIST /physics/ num_land_cat
NAMELIST /physics/ num_soil_cat
NAMELIST /physics/ mp_zero_out
NAMELIST /physics/ mp_zero_out_thresh
NAMELIST /physics/ seaice_threshold
NAMELIST /physics/ fractional_seaice
NAMELIST /physics/ sst_update
NAMELIST /physics/ usemonalb
NAMELIST /physics/ rdmaxalb
NAMELIST /physics/ rdlai2d
NAMELIST /physics/ gwd_opt
NAMELIST /physics/ idtad
NAMELIST /physics/ nsoil
NAMELIST /physics/ nphs
NAMELIST /physics/ ncnvc
NAMELIST /physics/ nrads
NAMELIST /physics/ nradl
NAMELIST /physics/ tprec
NAMELIST /physics/ theat
NAMELIST /physics/ tclod
NAMELIST /physics/ trdsw
NAMELIST /physics/ trdlw
NAMELIST /physics/ tsrfc
NAMELIST /physics/ pcpflg
NAMELIST /physics/ sigma
NAMELIST /physics/ sfenth
NAMELIST /physics/ co2tf
NAMELIST /physics/ ra_call_offset
NAMELIST /physics/ cam_abs_freq_s
NAMELIST /physics/ levsiz
NAMELIST /physics/ paerlev
NAMELIST /physics/ cam_abs_dim1
NAMELIST /physics/ cam_abs_dim2
NAMELIST /physics/ cu_rad_feedback
NAMELIST /dynamics/ dyn_opt
NAMELIST /dynamics/ rk_ord
NAMELIST /dynamics/ w_damping
NAMELIST /dynamics/ diff_opt
NAMELIST /dynamics/ km_opt
NAMELIST /dynamics/ damp_opt
NAMELIST /dynamics/ zdamp
NAMELIST /dynamics/ base_pres
NAMELIST /dynamics/ base_temp
NAMELIST /dynamics/ base_lapse
NAMELIST /dynamics/ iso_temp
NAMELIST /dynamics/ dampcoef
NAMELIST /dynamics/ khdif
NAMELIST /dynamics/ kvdif
NAMELIST /dynamics/ c_s
NAMELIST /dynamics/ c_k
NAMELIST /dynamics/ smdiv
NAMELIST /dynamics/ emdiv
NAMELIST /dynamics/ epssm
NAMELIST /dynamics/ non_hydrostatic
NAMELIST /dynamics/ time_step_sound
NAMELIST /dynamics/ h_mom_adv_order
NAMELIST /dynamics/ v_mom_adv_order
NAMELIST /dynamics/ h_sca_adv_order
NAMELIST /dynamics/ v_sca_adv_order
NAMELIST /dynamics/ top_radiation
NAMELIST /dynamics/ tke_upper_bound
NAMELIST /dynamics/ tke_drag_coefficient
NAMELIST /dynamics/ tke_heat_flux
NAMELIST /dynamics/ pert_coriolis
NAMELIST /dynamics/ euler_adv
NAMELIST /dynamics/ idtadt
NAMELIST /dynamics/ idtadc
NAMELIST /dynamics/ boundary_flux
NAMELIST /bdy_control/ spec_bdy_width
NAMELIST /bdy_control/ spec_zone
NAMELIST /bdy_control/ relax_zone
NAMELIST /bdy_control/ specified
NAMELIST /bdy_control/ periodic_x
NAMELIST /bdy_control/ symmetric_xs
NAMELIST /bdy_control/ symmetric_xe
NAMELIST /bdy_control/ open_xs
NAMELIST /bdy_control/ open_xe
NAMELIST /bdy_control/ periodic_y
NAMELIST /bdy_control/ symmetric_ys
NAMELIST /bdy_control/ symmetric_ye
NAMELIST /bdy_control/ open_ys
NAMELIST /bdy_control/ open_ye
NAMELIST /bdy_control/ polar
NAMELIST /bdy_control/ nested
NAMELIST /bdy_control/ real_data_init_type
NAMELIST /grib2/ background_proc_id
NAMELIST /grib2/ forecast_proc_id
NAMELIST /grib2/ production_status
NAMELIST /grib2/ compression



     LOGICAL :: write_qcw
     LOGICAL :: write_qrn
     LOGICAL :: write_qci
     LOGICAL :: write_qsn
     LOGICAL :: write_qgr
     LOGICAL :: write_filtered_obs
     NAMELIST /wrfvar2/ write_qcw
     NAMELIST /wrfvar2/ write_qrn
     NAMELIST /wrfvar2/ write_qci
     NAMELIST /wrfvar2/ write_qsn
     NAMELIST /wrfvar2/ write_qgr
     NAMELIST /wrfvar2/ write_filtered_obs





     REWIND ( UNIT = nml_read_unit )
     READ   ( UNIT = nml_read_unit , NML = wrfvar2 , iostat=nml_error )

     IF ( nml_error .EQ. 0 ) then    
        CALL wrf_debug(0,"Are write_qcw, write_qrn, write_qci, write_qsn, write_qgr, "// &
                         "write_filtered_obs still in your "// &
                         TRIM(nml_name)//" namelist?")
        CALL wrf_error_fatal3("",14300,&
"Remove write_qcw, write_qrn, write_qci, write_qsn, write_qgr, "// &
                             "write_filtered_obs as they are obsolete.")
     ELSE     
        return
     ENDIF

   END SUBROUTINE wrfvar_alt_nml_wrfvar2

   SUBROUTINE wrfvar_alt_nml_wrfvar4 (nml_read_unit, nml_name)







     IMPLICIT NONE
     INTEGER, INTENT(IN)       :: nml_read_unit
     CHARACTER*(*), INTENT(IN) :: nml_name
     INTEGER                   :: nml_error







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
character*256 :: auxinput1_inname
character*256 :: auxinput2_inname
character*256 :: auxinput3_inname
character*256 :: auxinput4_inname
character*256 :: auxinput5_inname
character*256 :: auxinput6_inname
character*256 :: auxinput7_inname
character*256 :: auxinput8_inname
character*256 :: sgfdda_inname
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
integer , DIMENSION(max_domains) :: sgfdda_interval_mo
integer , DIMENSION(max_domains) :: sgfdda_interval_d
integer , DIMENSION(max_domains) :: sgfdda_interval_h
integer , DIMENSION(max_domains) :: sgfdda_interval_m
integer , DIMENSION(max_domains) :: sgfdda_interval_s
integer , DIMENSION(max_domains) :: sgfdda_interval
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
integer , DIMENSION(max_domains) :: sgfdda_begin_y
integer , DIMENSION(max_domains) :: sgfdda_begin_mo
integer , DIMENSION(max_domains) :: sgfdda_begin_d
integer , DIMENSION(max_domains) :: sgfdda_begin_h
integer , DIMENSION(max_domains) :: sgfdda_begin_m
integer , DIMENSION(max_domains) :: sgfdda_begin_s
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
integer , DIMENSION(max_domains) :: sgfdda_end_y
integer , DIMENSION(max_domains) :: sgfdda_end_mo
integer , DIMENSION(max_domains) :: sgfdda_end_d
integer , DIMENSION(max_domains) :: sgfdda_end_h
integer , DIMENSION(max_domains) :: sgfdda_end_m
integer , DIMENSION(max_domains) :: sgfdda_end_s
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
integer :: io_form_sgfdda
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
integer :: simulation_start_year
integer :: simulation_start_month
integer :: simulation_start_day
integer :: simulation_start_hour
integer :: simulation_start_minute
integer :: simulation_start_second
logical :: reset_simulation_start
integer , DIMENSION(max_domains) :: sr_x
integer , DIMENSION(max_domains) :: sr_y
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
real , DIMENSION(max_domains) :: tstart
logical :: nocolons
logical :: cycling
integer :: dfi_opt
integer :: dfi_nfilter
logical :: dfi_write_filtered_input
logical :: dfi_write_dfi_history
integer :: dfi_cutoff_seconds
integer :: dfi_time_dim
integer :: dfi_fwdstop_year
integer :: dfi_fwdstop_month
integer :: dfi_fwdstop_day
integer :: dfi_fwdstop_hour
integer :: dfi_fwdstop_minute
integer :: dfi_fwdstop_second
integer :: dfi_bckstop_year
integer :: dfi_bckstop_month
integer :: dfi_bckstop_day
integer :: dfi_bckstop_hour
integer :: dfi_bckstop_minute
integer :: dfi_bckstop_second
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
real , DIMENSION(max_domains) :: dx
real , DIMENSION(max_domains) :: dy
integer , DIMENSION(max_domains) :: grid_id
logical , DIMENSION(max_domains) :: grid_allowed
integer , DIMENSION(max_domains) :: parent_id
integer , DIMENSION(max_domains) :: i_parent_start
integer , DIMENSION(max_domains) :: j_parent_start
integer , DIMENSION(max_domains) :: parent_grid_ratio
integer , DIMENSION(max_domains) :: parent_time_step_ratio
integer :: feedback
integer :: smooth_option
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
integer :: ts_buf_size
integer :: max_ts_locs
integer :: num_moves
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
real :: ptsgm
integer :: num_metgrid_levels
real :: p_top_requested
integer , DIMENSION(max_domains) :: mp_physics
integer , DIMENSION(max_domains) :: ra_lw_physics
integer , DIMENSION(max_domains) :: ra_sw_physics
real , DIMENSION(max_domains) :: radt
integer , DIMENSION(max_domains) :: sf_sfclay_physics
integer , DIMENSION(max_domains) :: sf_surface_physics
integer , DIMENSION(max_domains) :: bl_pbl_physics
integer , DIMENSION(max_domains) :: sf_urban_physics
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
integer :: num_urban_layers
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
integer :: fractional_seaice
integer :: sst_update
logical :: usemonalb
logical :: rdmaxalb
logical :: rdlai2d
integer , DIMENSION(max_domains) :: gwd_opt
integer , DIMENSION(max_domains) :: idtad
integer , DIMENSION(max_domains) :: nsoil
integer , DIMENSION(max_domains) :: nphs
integer , DIMENSION(max_domains) :: ncnvc
integer , DIMENSION(max_domains) :: nrads
integer , DIMENSION(max_domains) :: nradl
real , DIMENSION(max_domains) :: tprec
real , DIMENSION(max_domains) :: theat
real , DIMENSION(max_domains) :: tclod
real , DIMENSION(max_domains) :: trdsw
real , DIMENSION(max_domains) :: trdlw
real , DIMENSION(max_domains) :: tsrfc
logical , DIMENSION(max_domains) :: pcpflg
integer , DIMENSION(max_domains) :: sigma
real , DIMENSION(max_domains) :: sfenth
integer :: co2tf
integer :: ra_call_offset
real :: cam_abs_freq_s
integer :: levsiz
integer :: paerlev
integer :: cam_abs_dim1
integer :: cam_abs_dim2
logical , DIMENSION(max_domains) :: cu_rad_feedback
integer :: dyn_opt
integer :: rk_ord
integer :: w_damping
integer :: diff_opt
integer :: km_opt
integer :: damp_opt
real , DIMENSION(max_domains) :: zdamp
real :: base_pres
real :: base_temp
real :: base_lapse
real :: iso_temp
real , DIMENSION(max_domains) :: dampcoef
real , DIMENSION(max_domains) :: khdif
real , DIMENSION(max_domains) :: kvdif
real , DIMENSION(max_domains) :: c_s
real , DIMENSION(max_domains) :: c_k
real , DIMENSION(max_domains) :: smdiv
real , DIMENSION(max_domains) :: emdiv
real , DIMENSION(max_domains) :: epssm
logical , DIMENSION(max_domains) :: non_hydrostatic
integer , DIMENSION(max_domains) :: time_step_sound
integer , DIMENSION(max_domains) :: h_mom_adv_order
integer , DIMENSION(max_domains) :: v_mom_adv_order
integer , DIMENSION(max_domains) :: h_sca_adv_order
integer , DIMENSION(max_domains) :: v_sca_adv_order
logical , DIMENSION(max_domains) :: top_radiation
real , DIMENSION(max_domains) :: tke_upper_bound
real , DIMENSION(max_domains) :: tke_drag_coefficient
real , DIMENSION(max_domains) :: tke_heat_flux
logical , DIMENSION(max_domains) :: pert_coriolis
logical :: euler_adv
integer :: idtadt
integer :: idtadc
logical :: boundary_flux
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
logical , DIMENSION(max_domains) :: polar
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
integer :: flag_metgrid
integer :: flag_snow
integer :: flag_psfc
integer :: flag_sm000010
integer :: flag_sm010040
integer :: flag_sm040100
integer :: flag_sm100200
integer :: flag_st000010
integer :: flag_st010040
integer :: flag_st040100
integer :: flag_st100200
integer :: flag_slp
integer :: flag_soilhgt
integer :: flag_mf_xy
real , DIMENSION(max_domains) :: bdyfrq
character*256 , DIMENSION(max_domains) :: mminlu
integer , DIMENSION(max_domains) :: iswater
integer , DIMENSION(max_domains) :: islake
integer , DIMENSION(max_domains) :: isice
integer , DIMENSION(max_domains) :: isurban
integer , DIMENSION(max_domains) :: isoilwater
integer , DIMENSION(max_domains) :: map_proj
integer :: dfi_stage
integer , DIMENSION(max_domains) :: mp_physics_dfi
integer    :: last_item_in_struct







NAMELIST /time_control/ run_days
NAMELIST /time_control/ run_hours
NAMELIST /time_control/ run_minutes
NAMELIST /time_control/ run_seconds
NAMELIST /time_control/ start_year
NAMELIST /time_control/ start_month
NAMELIST /time_control/ start_day
NAMELIST /time_control/ start_hour
NAMELIST /time_control/ start_minute
NAMELIST /time_control/ start_second
NAMELIST /time_control/ end_year
NAMELIST /time_control/ end_month
NAMELIST /time_control/ end_day
NAMELIST /time_control/ end_hour
NAMELIST /time_control/ end_minute
NAMELIST /time_control/ end_second
NAMELIST /time_control/ interval_seconds
NAMELIST /time_control/ input_from_file
NAMELIST /time_control/ fine_input_stream
NAMELIST /time_control/ history_interval
NAMELIST /time_control/ frames_per_outfile
NAMELIST /time_control/ frames_per_auxhist1
NAMELIST /time_control/ frames_per_auxhist2
NAMELIST /time_control/ frames_per_auxhist3
NAMELIST /time_control/ frames_per_auxhist4
NAMELIST /time_control/ frames_per_auxhist5
NAMELIST /time_control/ frames_per_auxhist6
NAMELIST /time_control/ frames_per_auxhist7
NAMELIST /time_control/ frames_per_auxhist8
NAMELIST /time_control/ frames_per_auxhist9
NAMELIST /time_control/ frames_per_auxhist10
NAMELIST /time_control/ frames_per_auxhist11
NAMELIST /time_control/ restart
NAMELIST /time_control/ restart_interval
NAMELIST /time_control/ io_form_input
NAMELIST /time_control/ io_form_history
NAMELIST /time_control/ io_form_restart
NAMELIST /time_control/ io_form_boundary
NAMELIST /time_control/ debug_level
NAMELIST /time_control/ self_test_domain
NAMELIST /time_control/ history_outname
NAMELIST /time_control/ auxhist1_outname
NAMELIST /time_control/ auxhist2_outname
NAMELIST /time_control/ auxhist3_outname
NAMELIST /time_control/ auxhist4_outname
NAMELIST /time_control/ auxhist5_outname
NAMELIST /time_control/ auxhist6_outname
NAMELIST /time_control/ auxhist7_outname
NAMELIST /time_control/ auxhist8_outname
NAMELIST /time_control/ auxhist9_outname
NAMELIST /time_control/ auxhist10_outname
NAMELIST /time_control/ auxhist11_outname
NAMELIST /time_control/ history_inname
NAMELIST /time_control/ auxhist1_inname
NAMELIST /time_control/ auxhist2_inname
NAMELIST /time_control/ auxhist3_inname
NAMELIST /time_control/ auxhist4_inname
NAMELIST /time_control/ auxhist5_inname
NAMELIST /time_control/ auxhist6_inname
NAMELIST /time_control/ auxhist7_inname
NAMELIST /time_control/ auxhist8_inname
NAMELIST /time_control/ auxhist9_inname
NAMELIST /time_control/ auxhist10_inname
NAMELIST /time_control/ auxhist11_inname
NAMELIST /time_control/ auxinput1_outname
NAMELIST /time_control/ auxinput2_outname
NAMELIST /time_control/ auxinput3_outname
NAMELIST /time_control/ auxinput4_outname
NAMELIST /time_control/ auxinput5_outname
NAMELIST /time_control/ auxinput6_outname
NAMELIST /time_control/ auxinput7_outname
NAMELIST /time_control/ auxinput8_outname
NAMELIST /time_control/ auxinput9_outname
NAMELIST /time_control/ auxinput10_outname
NAMELIST /time_control/ auxinput11_outname
NAMELIST /time_control/ auxinput1_inname
NAMELIST /time_control/ auxinput2_inname
NAMELIST /time_control/ auxinput3_inname
NAMELIST /time_control/ auxinput4_inname
NAMELIST /time_control/ auxinput5_inname
NAMELIST /time_control/ auxinput6_inname
NAMELIST /time_control/ auxinput7_inname
NAMELIST /time_control/ auxinput8_inname
NAMELIST /fdda/ sgfdda_inname
NAMELIST /fdda/ gfdda_inname
NAMELIST /time_control/ auxinput11_inname
NAMELIST /time_control/ history_interval_mo
NAMELIST /time_control/ history_interval_d
NAMELIST /time_control/ history_interval_h
NAMELIST /time_control/ history_interval_m
NAMELIST /time_control/ history_interval_s
NAMELIST /time_control/ inputout_interval_mo
NAMELIST /time_control/ inputout_interval_d
NAMELIST /time_control/ inputout_interval_h
NAMELIST /time_control/ inputout_interval_m
NAMELIST /time_control/ inputout_interval_s
NAMELIST /time_control/ inputout_interval
NAMELIST /time_control/ auxhist1_interval_mo
NAMELIST /time_control/ auxhist1_interval_d
NAMELIST /time_control/ auxhist1_interval_h
NAMELIST /time_control/ auxhist1_interval_m
NAMELIST /time_control/ auxhist1_interval_s
NAMELIST /time_control/ auxhist1_interval
NAMELIST /time_control/ auxhist2_interval_mo
NAMELIST /time_control/ auxhist2_interval_d
NAMELIST /time_control/ auxhist2_interval_h
NAMELIST /time_control/ auxhist2_interval_m
NAMELIST /time_control/ auxhist2_interval_s
NAMELIST /time_control/ auxhist2_interval
NAMELIST /time_control/ auxhist3_interval_mo
NAMELIST /time_control/ auxhist3_interval_d
NAMELIST /time_control/ auxhist3_interval_h
NAMELIST /time_control/ auxhist3_interval_m
NAMELIST /time_control/ auxhist3_interval_s
NAMELIST /time_control/ auxhist3_interval
NAMELIST /time_control/ auxhist4_interval_mo
NAMELIST /time_control/ auxhist4_interval_d
NAMELIST /time_control/ auxhist4_interval_h
NAMELIST /time_control/ auxhist4_interval_m
NAMELIST /time_control/ auxhist4_interval_s
NAMELIST /time_control/ auxhist4_interval
NAMELIST /time_control/ auxhist5_interval_mo
NAMELIST /time_control/ auxhist5_interval_d
NAMELIST /time_control/ auxhist5_interval_h
NAMELIST /time_control/ auxhist5_interval_m
NAMELIST /time_control/ auxhist5_interval_s
NAMELIST /time_control/ auxhist5_interval
NAMELIST /time_control/ auxhist6_interval_mo
NAMELIST /time_control/ auxhist6_interval_d
NAMELIST /time_control/ auxhist6_interval_h
NAMELIST /time_control/ auxhist6_interval_m
NAMELIST /time_control/ auxhist6_interval_s
NAMELIST /time_control/ auxhist6_interval
NAMELIST /time_control/ auxhist7_interval_mo
NAMELIST /time_control/ auxhist7_interval_d
NAMELIST /time_control/ auxhist7_interval_h
NAMELIST /time_control/ auxhist7_interval_m
NAMELIST /time_control/ auxhist7_interval_s
NAMELIST /time_control/ auxhist7_interval
NAMELIST /time_control/ auxhist8_interval_mo
NAMELIST /time_control/ auxhist8_interval_d
NAMELIST /time_control/ auxhist8_interval_h
NAMELIST /time_control/ auxhist8_interval_m
NAMELIST /time_control/ auxhist8_interval_s
NAMELIST /time_control/ auxhist8_interval
NAMELIST /time_control/ auxhist9_interval_mo
NAMELIST /time_control/ auxhist9_interval_d
NAMELIST /time_control/ auxhist9_interval_h
NAMELIST /time_control/ auxhist9_interval_m
NAMELIST /time_control/ auxhist9_interval_s
NAMELIST /time_control/ auxhist9_interval
NAMELIST /time_control/ auxhist10_interval_mo
NAMELIST /time_control/ auxhist10_interval_d
NAMELIST /time_control/ auxhist10_interval_h
NAMELIST /time_control/ auxhist10_interval_m
NAMELIST /time_control/ auxhist10_interval_s
NAMELIST /time_control/ auxhist10_interval
NAMELIST /time_control/ auxhist11_interval_mo
NAMELIST /time_control/ auxhist11_interval_d
NAMELIST /time_control/ auxhist11_interval_h
NAMELIST /time_control/ auxhist11_interval_m
NAMELIST /time_control/ auxhist11_interval_s
NAMELIST /time_control/ auxhist11_interval
NAMELIST /time_control/ auxinput1_interval_mo
NAMELIST /time_control/ auxinput1_interval_d
NAMELIST /time_control/ auxinput1_interval_h
NAMELIST /time_control/ auxinput1_interval_m
NAMELIST /time_control/ auxinput1_interval_s
NAMELIST /time_control/ auxinput1_interval
NAMELIST /time_control/ auxinput2_interval_mo
NAMELIST /time_control/ auxinput2_interval_d
NAMELIST /time_control/ auxinput2_interval_h
NAMELIST /time_control/ auxinput2_interval_m
NAMELIST /time_control/ auxinput2_interval_s
NAMELIST /time_control/ auxinput2_interval
NAMELIST /time_control/ auxinput3_interval_mo
NAMELIST /time_control/ auxinput3_interval_d
NAMELIST /time_control/ auxinput3_interval_h
NAMELIST /time_control/ auxinput3_interval_m
NAMELIST /time_control/ auxinput3_interval_s
NAMELIST /time_control/ auxinput3_interval
NAMELIST /time_control/ auxinput4_interval_mo
NAMELIST /time_control/ auxinput4_interval_d
NAMELIST /time_control/ auxinput4_interval_h
NAMELIST /time_control/ auxinput4_interval_m
NAMELIST /time_control/ auxinput4_interval_s
NAMELIST /time_control/ auxinput4_interval
NAMELIST /time_control/ auxinput5_interval_mo
NAMELIST /time_control/ auxinput5_interval_d
NAMELIST /time_control/ auxinput5_interval_h
NAMELIST /time_control/ auxinput5_interval_m
NAMELIST /time_control/ auxinput5_interval_s
NAMELIST /time_control/ auxinput5_interval
NAMELIST /time_control/ auxinput6_interval_mo
NAMELIST /time_control/ auxinput6_interval_d
NAMELIST /time_control/ auxinput6_interval_h
NAMELIST /time_control/ auxinput6_interval_m
NAMELIST /time_control/ auxinput6_interval_s
NAMELIST /time_control/ auxinput6_interval
NAMELIST /time_control/ auxinput7_interval_mo
NAMELIST /time_control/ auxinput7_interval_d
NAMELIST /time_control/ auxinput7_interval_h
NAMELIST /time_control/ auxinput7_interval_m
NAMELIST /time_control/ auxinput7_interval_s
NAMELIST /time_control/ auxinput7_interval
NAMELIST /time_control/ auxinput8_interval_mo
NAMELIST /time_control/ auxinput8_interval_d
NAMELIST /time_control/ auxinput8_interval_h
NAMELIST /time_control/ auxinput8_interval_m
NAMELIST /time_control/ auxinput8_interval_s
NAMELIST /time_control/ auxinput8_interval
NAMELIST /fdda/ sgfdda_interval_mo
NAMELIST /fdda/ sgfdda_interval_d
NAMELIST /fdda/ sgfdda_interval_h
NAMELIST /fdda/ sgfdda_interval_m
NAMELIST /fdda/ sgfdda_interval_s
NAMELIST /fdda/ sgfdda_interval
NAMELIST /fdda/ gfdda_interval_mo
NAMELIST /fdda/ gfdda_interval_d
NAMELIST /fdda/ gfdda_interval_h
NAMELIST /fdda/ gfdda_interval_m
NAMELIST /fdda/ gfdda_interval_s
NAMELIST /fdda/ gfdda_interval
NAMELIST /time_control/ auxinput11_interval_mo
NAMELIST /time_control/ auxinput11_interval_d
NAMELIST /time_control/ auxinput11_interval_h
NAMELIST /time_control/ auxinput11_interval_m
NAMELIST /time_control/ auxinput11_interval_s
NAMELIST /time_control/ auxinput11_interval
NAMELIST /time_control/ restart_interval_mo
NAMELIST /time_control/ restart_interval_d
NAMELIST /time_control/ restart_interval_h
NAMELIST /time_control/ restart_interval_m
NAMELIST /time_control/ restart_interval_s
NAMELIST /time_control/ history_begin_y
NAMELIST /time_control/ history_begin_mo
NAMELIST /time_control/ history_begin_d
NAMELIST /time_control/ history_begin_h
NAMELIST /time_control/ history_begin_m
NAMELIST /time_control/ history_begin_s
NAMELIST /time_control/ inputout_begin_y
NAMELIST /time_control/ inputout_begin_mo
NAMELIST /time_control/ inputout_begin_d
NAMELIST /time_control/ inputout_begin_h
NAMELIST /time_control/ inputout_begin_m
NAMELIST /time_control/ inputout_begin_s
NAMELIST /time_control/ auxhist1_begin_y
NAMELIST /time_control/ auxhist1_begin_mo
NAMELIST /time_control/ auxhist1_begin_d
NAMELIST /time_control/ auxhist1_begin_h
NAMELIST /time_control/ auxhist1_begin_m
NAMELIST /time_control/ auxhist1_begin_s
NAMELIST /time_control/ auxhist2_begin_y
NAMELIST /time_control/ auxhist2_begin_mo
NAMELIST /time_control/ auxhist2_begin_d
NAMELIST /time_control/ auxhist2_begin_h
NAMELIST /time_control/ auxhist2_begin_m
NAMELIST /time_control/ auxhist2_begin_s
NAMELIST /time_control/ auxhist3_begin_y
NAMELIST /time_control/ auxhist3_begin_mo
NAMELIST /time_control/ auxhist3_begin_d
NAMELIST /time_control/ auxhist3_begin_h
NAMELIST /time_control/ auxhist3_begin_m
NAMELIST /time_control/ auxhist3_begin_s
NAMELIST /time_control/ auxhist4_begin_y
NAMELIST /time_control/ auxhist4_begin_mo
NAMELIST /time_control/ auxhist4_begin_d
NAMELIST /time_control/ auxhist4_begin_h
NAMELIST /time_control/ auxhist4_begin_m
NAMELIST /time_control/ auxhist4_begin_s
NAMELIST /time_control/ auxhist5_begin_y
NAMELIST /time_control/ auxhist5_begin_mo
NAMELIST /time_control/ auxhist5_begin_d
NAMELIST /time_control/ auxhist5_begin_h
NAMELIST /time_control/ auxhist5_begin_m
NAMELIST /time_control/ auxhist5_begin_s
NAMELIST /time_control/ auxhist6_begin_y
NAMELIST /time_control/ auxhist6_begin_mo
NAMELIST /time_control/ auxhist6_begin_d
NAMELIST /time_control/ auxhist6_begin_h
NAMELIST /time_control/ auxhist6_begin_m
NAMELIST /time_control/ auxhist6_begin_s
NAMELIST /time_control/ auxhist7_begin_y
NAMELIST /time_control/ auxhist7_begin_mo
NAMELIST /time_control/ auxhist7_begin_d
NAMELIST /time_control/ auxhist7_begin_h
NAMELIST /time_control/ auxhist7_begin_m
NAMELIST /time_control/ auxhist7_begin_s
NAMELIST /time_control/ auxhist8_begin_y
NAMELIST /time_control/ auxhist8_begin_mo
NAMELIST /time_control/ auxhist8_begin_d
NAMELIST /time_control/ auxhist8_begin_h
NAMELIST /time_control/ auxhist8_begin_m
NAMELIST /time_control/ auxhist8_begin_s
NAMELIST /time_control/ auxhist9_begin_y
NAMELIST /time_control/ auxhist9_begin_mo
NAMELIST /time_control/ auxhist9_begin_d
NAMELIST /time_control/ auxhist9_begin_h
NAMELIST /time_control/ auxhist9_begin_m
NAMELIST /time_control/ auxhist9_begin_s
NAMELIST /time_control/ auxhist10_begin_y
NAMELIST /time_control/ auxhist10_begin_mo
NAMELIST /time_control/ auxhist10_begin_d
NAMELIST /time_control/ auxhist10_begin_h
NAMELIST /time_control/ auxhist10_begin_m
NAMELIST /time_control/ auxhist10_begin_s
NAMELIST /time_control/ auxhist11_begin_y
NAMELIST /time_control/ auxhist11_begin_mo
NAMELIST /time_control/ auxhist11_begin_d
NAMELIST /time_control/ auxhist11_begin_h
NAMELIST /time_control/ auxhist11_begin_m
NAMELIST /time_control/ auxhist11_begin_s
NAMELIST /time_control/ auxinput1_begin_y
NAMELIST /time_control/ auxinput1_begin_mo
NAMELIST /time_control/ auxinput1_begin_d
NAMELIST /time_control/ auxinput1_begin_h
NAMELIST /time_control/ auxinput1_begin_m
NAMELIST /time_control/ auxinput1_begin_s
NAMELIST /time_control/ auxinput2_begin_y
NAMELIST /time_control/ auxinput2_begin_mo
NAMELIST /time_control/ auxinput2_begin_d
NAMELIST /time_control/ auxinput2_begin_h
NAMELIST /time_control/ auxinput2_begin_m
NAMELIST /time_control/ auxinput2_begin_s
NAMELIST /time_control/ auxinput3_begin_y
NAMELIST /time_control/ auxinput3_begin_mo
NAMELIST /time_control/ auxinput3_begin_d
NAMELIST /time_control/ auxinput3_begin_h
NAMELIST /time_control/ auxinput3_begin_m
NAMELIST /time_control/ auxinput3_begin_s
NAMELIST /time_control/ auxinput4_begin_y
NAMELIST /time_control/ auxinput4_begin_mo
NAMELIST /time_control/ auxinput4_begin_d
NAMELIST /time_control/ auxinput4_begin_h
NAMELIST /time_control/ auxinput4_begin_m
NAMELIST /time_control/ auxinput4_begin_s
NAMELIST /time_control/ auxinput5_begin_y
NAMELIST /time_control/ auxinput5_begin_mo
NAMELIST /time_control/ auxinput5_begin_d
NAMELIST /time_control/ auxinput5_begin_h
NAMELIST /time_control/ auxinput5_begin_m
NAMELIST /time_control/ auxinput5_begin_s
NAMELIST /time_control/ auxinput6_begin_y
NAMELIST /time_control/ auxinput6_begin_mo
NAMELIST /time_control/ auxinput6_begin_d
NAMELIST /time_control/ auxinput6_begin_h
NAMELIST /time_control/ auxinput6_begin_m
NAMELIST /time_control/ auxinput6_begin_s
NAMELIST /time_control/ auxinput7_begin_y
NAMELIST /time_control/ auxinput7_begin_mo
NAMELIST /time_control/ auxinput7_begin_d
NAMELIST /time_control/ auxinput7_begin_h
NAMELIST /time_control/ auxinput7_begin_m
NAMELIST /time_control/ auxinput7_begin_s
NAMELIST /time_control/ auxinput8_begin_y
NAMELIST /time_control/ auxinput8_begin_mo
NAMELIST /time_control/ auxinput8_begin_d
NAMELIST /time_control/ auxinput8_begin_h
NAMELIST /time_control/ auxinput8_begin_m
NAMELIST /time_control/ auxinput8_begin_s
NAMELIST /fdda/ sgfdda_begin_y
NAMELIST /fdda/ sgfdda_begin_mo
NAMELIST /fdda/ sgfdda_begin_d
NAMELIST /fdda/ sgfdda_begin_h
NAMELIST /fdda/ sgfdda_begin_m
NAMELIST /fdda/ sgfdda_begin_s
NAMELIST /fdda/ gfdda_begin_y
NAMELIST /fdda/ gfdda_begin_mo
NAMELIST /fdda/ gfdda_begin_d
NAMELIST /fdda/ gfdda_begin_h
NAMELIST /fdda/ gfdda_begin_m
NAMELIST /fdda/ gfdda_begin_s
NAMELIST /time_control/ auxinput11_begin_y
NAMELIST /time_control/ auxinput11_begin_mo
NAMELIST /time_control/ auxinput11_begin_d
NAMELIST /time_control/ auxinput11_begin_h
NAMELIST /time_control/ auxinput11_begin_m
NAMELIST /time_control/ auxinput11_begin_s
NAMELIST /time_control/ restart_begin_y
NAMELIST /time_control/ restart_begin_mo
NAMELIST /time_control/ restart_begin_d
NAMELIST /time_control/ restart_begin_h
NAMELIST /time_control/ restart_begin_m
NAMELIST /time_control/ restart_begin_s
NAMELIST /time_control/ history_end_y
NAMELIST /time_control/ history_end_mo
NAMELIST /time_control/ history_end_d
NAMELIST /time_control/ history_end_h
NAMELIST /time_control/ history_end_m
NAMELIST /time_control/ history_end_s
NAMELIST /time_control/ inputout_end_y
NAMELIST /time_control/ inputout_end_mo
NAMELIST /time_control/ inputout_end_d
NAMELIST /time_control/ inputout_end_h
NAMELIST /time_control/ inputout_end_m
NAMELIST /time_control/ inputout_end_s
NAMELIST /time_control/ auxhist1_end_y
NAMELIST /time_control/ auxhist1_end_mo
NAMELIST /time_control/ auxhist1_end_d
NAMELIST /time_control/ auxhist1_end_h
NAMELIST /time_control/ auxhist1_end_m
NAMELIST /time_control/ auxhist1_end_s
NAMELIST /time_control/ auxhist2_end_y
NAMELIST /time_control/ auxhist2_end_mo
NAMELIST /time_control/ auxhist2_end_d
NAMELIST /time_control/ auxhist2_end_h
NAMELIST /time_control/ auxhist2_end_m
NAMELIST /time_control/ auxhist2_end_s
NAMELIST /time_control/ auxhist3_end_y
NAMELIST /time_control/ auxhist3_end_mo
NAMELIST /time_control/ auxhist3_end_d
NAMELIST /time_control/ auxhist3_end_h
NAMELIST /time_control/ auxhist3_end_m
NAMELIST /time_control/ auxhist3_end_s
NAMELIST /time_control/ auxhist4_end_y
NAMELIST /time_control/ auxhist4_end_mo
NAMELIST /time_control/ auxhist4_end_d
NAMELIST /time_control/ auxhist4_end_h
NAMELIST /time_control/ auxhist4_end_m
NAMELIST /time_control/ auxhist4_end_s
NAMELIST /time_control/ auxhist5_end_y
NAMELIST /time_control/ auxhist5_end_mo
NAMELIST /time_control/ auxhist5_end_d
NAMELIST /time_control/ auxhist5_end_h
NAMELIST /time_control/ auxhist5_end_m
NAMELIST /time_control/ auxhist5_end_s
NAMELIST /time_control/ auxhist6_end_y
NAMELIST /time_control/ auxhist6_end_mo
NAMELIST /time_control/ auxhist6_end_d
NAMELIST /time_control/ auxhist6_end_h
NAMELIST /time_control/ auxhist6_end_m
NAMELIST /time_control/ auxhist6_end_s
NAMELIST /time_control/ auxhist7_end_y
NAMELIST /time_control/ auxhist7_end_mo
NAMELIST /time_control/ auxhist7_end_d
NAMELIST /time_control/ auxhist7_end_h
NAMELIST /time_control/ auxhist7_end_m
NAMELIST /time_control/ auxhist7_end_s
NAMELIST /time_control/ auxhist8_end_y
NAMELIST /time_control/ auxhist8_end_mo
NAMELIST /time_control/ auxhist8_end_d
NAMELIST /time_control/ auxhist8_end_h
NAMELIST /time_control/ auxhist8_end_m
NAMELIST /time_control/ auxhist8_end_s
NAMELIST /time_control/ auxhist9_end_y
NAMELIST /time_control/ auxhist9_end_mo
NAMELIST /time_control/ auxhist9_end_d
NAMELIST /time_control/ auxhist9_end_h
NAMELIST /time_control/ auxhist9_end_m
NAMELIST /time_control/ auxhist9_end_s
NAMELIST /time_control/ auxhist10_end_y
NAMELIST /time_control/ auxhist10_end_mo
NAMELIST /time_control/ auxhist10_end_d
NAMELIST /time_control/ auxhist10_end_h
NAMELIST /time_control/ auxhist10_end_m
NAMELIST /time_control/ auxhist10_end_s
NAMELIST /time_control/ auxhist11_end_y
NAMELIST /time_control/ auxhist11_end_mo
NAMELIST /time_control/ auxhist11_end_d
NAMELIST /time_control/ auxhist11_end_h
NAMELIST /time_control/ auxhist11_end_m
NAMELIST /time_control/ auxhist11_end_s
NAMELIST /time_control/ auxinput1_end_y
NAMELIST /time_control/ auxinput1_end_mo
NAMELIST /time_control/ auxinput1_end_d
NAMELIST /time_control/ auxinput1_end_h
NAMELIST /time_control/ auxinput1_end_m
NAMELIST /time_control/ auxinput1_end_s
NAMELIST /time_control/ auxinput2_end_y
NAMELIST /time_control/ auxinput2_end_mo
NAMELIST /time_control/ auxinput2_end_d
NAMELIST /time_control/ auxinput2_end_h
NAMELIST /time_control/ auxinput2_end_m
NAMELIST /time_control/ auxinput2_end_s
NAMELIST /time_control/ auxinput3_end_y
NAMELIST /time_control/ auxinput3_end_mo
NAMELIST /time_control/ auxinput3_end_d
NAMELIST /time_control/ auxinput3_end_h
NAMELIST /time_control/ auxinput3_end_m
NAMELIST /time_control/ auxinput3_end_s
NAMELIST /time_control/ auxinput4_end_y
NAMELIST /time_control/ auxinput4_end_mo
NAMELIST /time_control/ auxinput4_end_d
NAMELIST /time_control/ auxinput4_end_h
NAMELIST /time_control/ auxinput4_end_m
NAMELIST /time_control/ auxinput4_end_s
NAMELIST /time_control/ auxinput5_end_y
NAMELIST /time_control/ auxinput5_end_mo
NAMELIST /time_control/ auxinput5_end_d
NAMELIST /time_control/ auxinput5_end_h
NAMELIST /time_control/ auxinput5_end_m
NAMELIST /time_control/ auxinput5_end_s
NAMELIST /time_control/ auxinput6_end_y
NAMELIST /time_control/ auxinput6_end_mo
NAMELIST /time_control/ auxinput6_end_d
NAMELIST /time_control/ auxinput6_end_h
NAMELIST /time_control/ auxinput6_end_m
NAMELIST /time_control/ auxinput6_end_s
NAMELIST /time_control/ auxinput7_end_y
NAMELIST /time_control/ auxinput7_end_mo
NAMELIST /time_control/ auxinput7_end_d
NAMELIST /time_control/ auxinput7_end_h
NAMELIST /time_control/ auxinput7_end_m
NAMELIST /time_control/ auxinput7_end_s
NAMELIST /time_control/ auxinput8_end_y
NAMELIST /time_control/ auxinput8_end_mo
NAMELIST /time_control/ auxinput8_end_d
NAMELIST /time_control/ auxinput8_end_h
NAMELIST /time_control/ auxinput8_end_m
NAMELIST /time_control/ auxinput8_end_s
NAMELIST /fdda/ sgfdda_end_y
NAMELIST /fdda/ sgfdda_end_mo
NAMELIST /fdda/ sgfdda_end_d
NAMELIST /fdda/ sgfdda_end_h
NAMELIST /fdda/ sgfdda_end_m
NAMELIST /fdda/ sgfdda_end_s
NAMELIST /fdda/ gfdda_end_y
NAMELIST /fdda/ gfdda_end_mo
NAMELIST /fdda/ gfdda_end_d
NAMELIST /fdda/ gfdda_end_h
NAMELIST /fdda/ gfdda_end_m
NAMELIST /fdda/ gfdda_end_s
NAMELIST /time_control/ auxinput11_end_y
NAMELIST /time_control/ auxinput11_end_mo
NAMELIST /time_control/ auxinput11_end_d
NAMELIST /time_control/ auxinput11_end_h
NAMELIST /time_control/ auxinput11_end_m
NAMELIST /time_control/ auxinput11_end_s
NAMELIST /time_control/ io_form_auxinput1
NAMELIST /time_control/ io_form_auxinput2
NAMELIST /time_control/ io_form_auxinput3
NAMELIST /time_control/ io_form_auxinput4
NAMELIST /time_control/ io_form_auxinput5
NAMELIST /time_control/ io_form_auxinput6
NAMELIST /time_control/ io_form_auxinput7
NAMELIST /time_control/ io_form_auxinput8
NAMELIST /fdda/ io_form_sgfdda
NAMELIST /fdda/ io_form_gfdda
NAMELIST /time_control/ io_form_auxinput11
NAMELIST /time_control/ io_form_auxhist1
NAMELIST /time_control/ io_form_auxhist2
NAMELIST /time_control/ io_form_auxhist3
NAMELIST /time_control/ io_form_auxhist4
NAMELIST /time_control/ io_form_auxhist5
NAMELIST /time_control/ io_form_auxhist6
NAMELIST /time_control/ io_form_auxhist7
NAMELIST /time_control/ io_form_auxhist8
NAMELIST /time_control/ io_form_auxhist9
NAMELIST /time_control/ io_form_auxhist10
NAMELIST /time_control/ io_form_auxhist11
NAMELIST /time_control/ reset_simulation_start
NAMELIST /domains/ sr_x
NAMELIST /domains/ sr_y
NAMELIST /time_control/ julyr
NAMELIST /time_control/ julday
NAMELIST /time_control/ gmt
NAMELIST /time_control/ input_inname
NAMELIST /time_control/ input_outname
NAMELIST /time_control/ bdy_inname
NAMELIST /time_control/ bdy_outname
NAMELIST /time_control/ rst_inname
NAMELIST /time_control/ rst_outname
NAMELIST /time_control/ write_input
NAMELIST /time_control/ write_restart_at_0h
NAMELIST /time_control/ adjust_output_times
NAMELIST /time_control/ adjust_input_times
NAMELIST /time_control/ tstart
NAMELIST /time_control/ nocolons
NAMELIST /time_control/ cycling
NAMELIST /dfi_control/ dfi_opt
NAMELIST /dfi_control/ dfi_nfilter
NAMELIST /dfi_control/ dfi_write_filtered_input
NAMELIST /dfi_control/ dfi_write_dfi_history
NAMELIST /dfi_control/ dfi_cutoff_seconds
NAMELIST /dfi_control/ dfi_time_dim
NAMELIST /dfi_control/ dfi_fwdstop_year
NAMELIST /dfi_control/ dfi_fwdstop_month
NAMELIST /dfi_control/ dfi_fwdstop_day
NAMELIST /dfi_control/ dfi_fwdstop_hour
NAMELIST /dfi_control/ dfi_fwdstop_minute
NAMELIST /dfi_control/ dfi_fwdstop_second
NAMELIST /dfi_control/ dfi_bckstop_year
NAMELIST /dfi_control/ dfi_bckstop_month
NAMELIST /dfi_control/ dfi_bckstop_day
NAMELIST /dfi_control/ dfi_bckstop_hour
NAMELIST /dfi_control/ dfi_bckstop_minute
NAMELIST /dfi_control/ dfi_bckstop_second
NAMELIST /domains/ time_step
NAMELIST /domains/ time_step_fract_num
NAMELIST /domains/ time_step_fract_den
NAMELIST /domains/ max_dom
NAMELIST /domains/ s_we
NAMELIST /domains/ e_we
NAMELIST /domains/ s_sn
NAMELIST /domains/ e_sn
NAMELIST /domains/ s_vert
NAMELIST /domains/ e_vert
NAMELIST /domains/ dx
NAMELIST /domains/ dy
NAMELIST /domains/ grid_id
NAMELIST /domains/ grid_allowed
NAMELIST /domains/ parent_id
NAMELIST /domains/ i_parent_start
NAMELIST /domains/ j_parent_start
NAMELIST /domains/ parent_grid_ratio
NAMELIST /domains/ parent_time_step_ratio
NAMELIST /domains/ feedback
NAMELIST /domains/ smooth_option
NAMELIST /domains/ ztop
NAMELIST /domains/ moad_grid_ratio
NAMELIST /domains/ moad_time_step_ratio
NAMELIST /domains/ shw
NAMELIST /domains/ tile_sz_x
NAMELIST /domains/ tile_sz_y
NAMELIST /domains/ numtiles
NAMELIST /domains/ nproc_x
NAMELIST /domains/ nproc_y
NAMELIST /domains/ irand
NAMELIST /domains/ ts_buf_size
NAMELIST /domains/ max_ts_locs
NAMELIST /domains/ num_moves
NAMELIST /domains/ move_id
NAMELIST /domains/ move_interval
NAMELIST /domains/ move_cd_x
NAMELIST /domains/ move_cd_y
NAMELIST /domains/ swap_x
NAMELIST /domains/ swap_y
NAMELIST /domains/ cycle_x
NAMELIST /domains/ cycle_y
NAMELIST /domains/ reorder_mesh
NAMELIST /domains/ perturb_input
NAMELIST /domains/ eta_levels
NAMELIST /domains/ ptsgm
NAMELIST /domains/ num_metgrid_levels
NAMELIST /domains/ p_top_requested
NAMELIST /physics/ mp_physics
NAMELIST /physics/ ra_lw_physics
NAMELIST /physics/ ra_sw_physics
NAMELIST /physics/ radt
NAMELIST /physics/ sf_sfclay_physics
NAMELIST /physics/ sf_surface_physics
NAMELIST /physics/ bl_pbl_physics
NAMELIST /physics/ sf_urban_physics
NAMELIST /physics/ bldt
NAMELIST /physics/ cu_physics
NAMELIST /physics/ cudt
NAMELIST /physics/ gsmdt
NAMELIST /physics/ isfflx
NAMELIST /physics/ ifsnow
NAMELIST /physics/ icloud
NAMELIST /physics/ swrad_scat
NAMELIST /physics/ surface_input_source
NAMELIST /physics/ num_soil_layers
NAMELIST /physics/ num_urban_layers
NAMELIST /physics/ maxiens
NAMELIST /physics/ maxens
NAMELIST /physics/ maxens2
NAMELIST /physics/ maxens3
NAMELIST /physics/ ensdim
NAMELIST /physics/ chem_opt
NAMELIST /physics/ num_land_cat
NAMELIST /physics/ num_soil_cat
NAMELIST /physics/ mp_zero_out
NAMELIST /physics/ mp_zero_out_thresh
NAMELIST /physics/ seaice_threshold
NAMELIST /physics/ fractional_seaice
NAMELIST /physics/ sst_update
NAMELIST /physics/ usemonalb
NAMELIST /physics/ rdmaxalb
NAMELIST /physics/ rdlai2d
NAMELIST /physics/ gwd_opt
NAMELIST /physics/ idtad
NAMELIST /physics/ nsoil
NAMELIST /physics/ nphs
NAMELIST /physics/ ncnvc
NAMELIST /physics/ nrads
NAMELIST /physics/ nradl
NAMELIST /physics/ tprec
NAMELIST /physics/ theat
NAMELIST /physics/ tclod
NAMELIST /physics/ trdsw
NAMELIST /physics/ trdlw
NAMELIST /physics/ tsrfc
NAMELIST /physics/ pcpflg
NAMELIST /physics/ sigma
NAMELIST /physics/ sfenth
NAMELIST /physics/ co2tf
NAMELIST /physics/ ra_call_offset
NAMELIST /physics/ cam_abs_freq_s
NAMELIST /physics/ levsiz
NAMELIST /physics/ paerlev
NAMELIST /physics/ cam_abs_dim1
NAMELIST /physics/ cam_abs_dim2
NAMELIST /physics/ cu_rad_feedback
NAMELIST /dynamics/ dyn_opt
NAMELIST /dynamics/ rk_ord
NAMELIST /dynamics/ w_damping
NAMELIST /dynamics/ diff_opt
NAMELIST /dynamics/ km_opt
NAMELIST /dynamics/ damp_opt
NAMELIST /dynamics/ zdamp
NAMELIST /dynamics/ base_pres
NAMELIST /dynamics/ base_temp
NAMELIST /dynamics/ base_lapse
NAMELIST /dynamics/ iso_temp
NAMELIST /dynamics/ dampcoef
NAMELIST /dynamics/ khdif
NAMELIST /dynamics/ kvdif
NAMELIST /dynamics/ c_s
NAMELIST /dynamics/ c_k
NAMELIST /dynamics/ smdiv
NAMELIST /dynamics/ emdiv
NAMELIST /dynamics/ epssm
NAMELIST /dynamics/ non_hydrostatic
NAMELIST /dynamics/ time_step_sound
NAMELIST /dynamics/ h_mom_adv_order
NAMELIST /dynamics/ v_mom_adv_order
NAMELIST /dynamics/ h_sca_adv_order
NAMELIST /dynamics/ v_sca_adv_order
NAMELIST /dynamics/ top_radiation
NAMELIST /dynamics/ tke_upper_bound
NAMELIST /dynamics/ tke_drag_coefficient
NAMELIST /dynamics/ tke_heat_flux
NAMELIST /dynamics/ pert_coriolis
NAMELIST /dynamics/ euler_adv
NAMELIST /dynamics/ idtadt
NAMELIST /dynamics/ idtadc
NAMELIST /dynamics/ boundary_flux
NAMELIST /bdy_control/ spec_bdy_width
NAMELIST /bdy_control/ spec_zone
NAMELIST /bdy_control/ relax_zone
NAMELIST /bdy_control/ specified
NAMELIST /bdy_control/ periodic_x
NAMELIST /bdy_control/ symmetric_xs
NAMELIST /bdy_control/ symmetric_xe
NAMELIST /bdy_control/ open_xs
NAMELIST /bdy_control/ open_xe
NAMELIST /bdy_control/ periodic_y
NAMELIST /bdy_control/ symmetric_ys
NAMELIST /bdy_control/ symmetric_ye
NAMELIST /bdy_control/ open_ys
NAMELIST /bdy_control/ open_ye
NAMELIST /bdy_control/ polar
NAMELIST /bdy_control/ nested
NAMELIST /bdy_control/ real_data_init_type
NAMELIST /grib2/ background_proc_id
NAMELIST /grib2/ forecast_proc_id
NAMELIST /grib2/ production_status
NAMELIST /grib2/ compression



     LOGICAL :: use_eos_radobs
     NAMELIST /wrfvar4/ use_eos_radobs





     REWIND ( UNIT = nml_read_unit )
     READ   ( UNIT = nml_read_unit , NML = wrfvar4 , iostat=nml_error )

     IF ( nml_error .EQ. 0 ) then    
        CALL wrf_debug(0,"Is use_eos_radobs still in your "// &
                         TRIM(nml_name)//" namelist?")
        CALL wrf_error_fatal3("",15888,&
"Remove use_eos_radobs as it is obsolete.")
     ELSE     
        return
     ENDIF

   END SUBROUTINE wrfvar_alt_nml_wrfvar4

   SUBROUTINE wrfvar_alt_nml_wrfvar14 (nml_read_unit, nml_name)







     IMPLICIT NONE
     INTEGER, INTENT(IN)       :: nml_read_unit
     CHARACTER*(*), INTENT(IN) :: nml_name
     INTEGER                   :: nml_error







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
character*256 :: auxinput1_inname
character*256 :: auxinput2_inname
character*256 :: auxinput3_inname
character*256 :: auxinput4_inname
character*256 :: auxinput5_inname
character*256 :: auxinput6_inname
character*256 :: auxinput7_inname
character*256 :: auxinput8_inname
character*256 :: sgfdda_inname
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
integer , DIMENSION(max_domains) :: sgfdda_interval_mo
integer , DIMENSION(max_domains) :: sgfdda_interval_d
integer , DIMENSION(max_domains) :: sgfdda_interval_h
integer , DIMENSION(max_domains) :: sgfdda_interval_m
integer , DIMENSION(max_domains) :: sgfdda_interval_s
integer , DIMENSION(max_domains) :: sgfdda_interval
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
integer , DIMENSION(max_domains) :: sgfdda_begin_y
integer , DIMENSION(max_domains) :: sgfdda_begin_mo
integer , DIMENSION(max_domains) :: sgfdda_begin_d
integer , DIMENSION(max_domains) :: sgfdda_begin_h
integer , DIMENSION(max_domains) :: sgfdda_begin_m
integer , DIMENSION(max_domains) :: sgfdda_begin_s
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
integer , DIMENSION(max_domains) :: sgfdda_end_y
integer , DIMENSION(max_domains) :: sgfdda_end_mo
integer , DIMENSION(max_domains) :: sgfdda_end_d
integer , DIMENSION(max_domains) :: sgfdda_end_h
integer , DIMENSION(max_domains) :: sgfdda_end_m
integer , DIMENSION(max_domains) :: sgfdda_end_s
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
integer :: io_form_sgfdda
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
integer :: simulation_start_year
integer :: simulation_start_month
integer :: simulation_start_day
integer :: simulation_start_hour
integer :: simulation_start_minute
integer :: simulation_start_second
logical :: reset_simulation_start
integer , DIMENSION(max_domains) :: sr_x
integer , DIMENSION(max_domains) :: sr_y
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
real , DIMENSION(max_domains) :: tstart
logical :: nocolons
logical :: cycling
integer :: dfi_opt
integer :: dfi_nfilter
logical :: dfi_write_filtered_input
logical :: dfi_write_dfi_history
integer :: dfi_cutoff_seconds
integer :: dfi_time_dim
integer :: dfi_fwdstop_year
integer :: dfi_fwdstop_month
integer :: dfi_fwdstop_day
integer :: dfi_fwdstop_hour
integer :: dfi_fwdstop_minute
integer :: dfi_fwdstop_second
integer :: dfi_bckstop_year
integer :: dfi_bckstop_month
integer :: dfi_bckstop_day
integer :: dfi_bckstop_hour
integer :: dfi_bckstop_minute
integer :: dfi_bckstop_second
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
real , DIMENSION(max_domains) :: dx
real , DIMENSION(max_domains) :: dy
integer , DIMENSION(max_domains) :: grid_id
logical , DIMENSION(max_domains) :: grid_allowed
integer , DIMENSION(max_domains) :: parent_id
integer , DIMENSION(max_domains) :: i_parent_start
integer , DIMENSION(max_domains) :: j_parent_start
integer , DIMENSION(max_domains) :: parent_grid_ratio
integer , DIMENSION(max_domains) :: parent_time_step_ratio
integer :: feedback
integer :: smooth_option
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
integer :: ts_buf_size
integer :: max_ts_locs
integer :: num_moves
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
real :: ptsgm
integer :: num_metgrid_levels
real :: p_top_requested
integer , DIMENSION(max_domains) :: mp_physics
integer , DIMENSION(max_domains) :: ra_lw_physics
integer , DIMENSION(max_domains) :: ra_sw_physics
real , DIMENSION(max_domains) :: radt
integer , DIMENSION(max_domains) :: sf_sfclay_physics
integer , DIMENSION(max_domains) :: sf_surface_physics
integer , DIMENSION(max_domains) :: bl_pbl_physics
integer , DIMENSION(max_domains) :: sf_urban_physics
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
integer :: num_urban_layers
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
integer :: fractional_seaice
integer :: sst_update
logical :: usemonalb
logical :: rdmaxalb
logical :: rdlai2d
integer , DIMENSION(max_domains) :: gwd_opt
integer , DIMENSION(max_domains) :: idtad
integer , DIMENSION(max_domains) :: nsoil
integer , DIMENSION(max_domains) :: nphs
integer , DIMENSION(max_domains) :: ncnvc
integer , DIMENSION(max_domains) :: nrads
integer , DIMENSION(max_domains) :: nradl
real , DIMENSION(max_domains) :: tprec
real , DIMENSION(max_domains) :: theat
real , DIMENSION(max_domains) :: tclod
real , DIMENSION(max_domains) :: trdsw
real , DIMENSION(max_domains) :: trdlw
real , DIMENSION(max_domains) :: tsrfc
logical , DIMENSION(max_domains) :: pcpflg
integer , DIMENSION(max_domains) :: sigma
real , DIMENSION(max_domains) :: sfenth
integer :: co2tf
integer :: ra_call_offset
real :: cam_abs_freq_s
integer :: levsiz
integer :: paerlev
integer :: cam_abs_dim1
integer :: cam_abs_dim2
logical , DIMENSION(max_domains) :: cu_rad_feedback
integer :: dyn_opt
integer :: rk_ord
integer :: w_damping
integer :: diff_opt
integer :: km_opt
integer :: damp_opt
real , DIMENSION(max_domains) :: zdamp
real :: base_pres
real :: base_temp
real :: base_lapse
real :: iso_temp
real , DIMENSION(max_domains) :: dampcoef
real , DIMENSION(max_domains) :: khdif
real , DIMENSION(max_domains) :: kvdif
real , DIMENSION(max_domains) :: c_s
real , DIMENSION(max_domains) :: c_k
real , DIMENSION(max_domains) :: smdiv
real , DIMENSION(max_domains) :: emdiv
real , DIMENSION(max_domains) :: epssm
logical , DIMENSION(max_domains) :: non_hydrostatic
integer , DIMENSION(max_domains) :: time_step_sound
integer , DIMENSION(max_domains) :: h_mom_adv_order
integer , DIMENSION(max_domains) :: v_mom_adv_order
integer , DIMENSION(max_domains) :: h_sca_adv_order
integer , DIMENSION(max_domains) :: v_sca_adv_order
logical , DIMENSION(max_domains) :: top_radiation
real , DIMENSION(max_domains) :: tke_upper_bound
real , DIMENSION(max_domains) :: tke_drag_coefficient
real , DIMENSION(max_domains) :: tke_heat_flux
logical , DIMENSION(max_domains) :: pert_coriolis
logical :: euler_adv
integer :: idtadt
integer :: idtadc
logical :: boundary_flux
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
logical , DIMENSION(max_domains) :: polar
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
integer :: flag_metgrid
integer :: flag_snow
integer :: flag_psfc
integer :: flag_sm000010
integer :: flag_sm010040
integer :: flag_sm040100
integer :: flag_sm100200
integer :: flag_st000010
integer :: flag_st010040
integer :: flag_st040100
integer :: flag_st100200
integer :: flag_slp
integer :: flag_soilhgt
integer :: flag_mf_xy
real , DIMENSION(max_domains) :: bdyfrq
character*256 , DIMENSION(max_domains) :: mminlu
integer , DIMENSION(max_domains) :: iswater
integer , DIMENSION(max_domains) :: islake
integer , DIMENSION(max_domains) :: isice
integer , DIMENSION(max_domains) :: isurban
integer , DIMENSION(max_domains) :: isoilwater
integer , DIMENSION(max_domains) :: map_proj
integer :: dfi_stage
integer , DIMENSION(max_domains) :: mp_physics_dfi
integer    :: last_item_in_struct







NAMELIST /time_control/ run_days
NAMELIST /time_control/ run_hours
NAMELIST /time_control/ run_minutes
NAMELIST /time_control/ run_seconds
NAMELIST /time_control/ start_year
NAMELIST /time_control/ start_month
NAMELIST /time_control/ start_day
NAMELIST /time_control/ start_hour
NAMELIST /time_control/ start_minute
NAMELIST /time_control/ start_second
NAMELIST /time_control/ end_year
NAMELIST /time_control/ end_month
NAMELIST /time_control/ end_day
NAMELIST /time_control/ end_hour
NAMELIST /time_control/ end_minute
NAMELIST /time_control/ end_second
NAMELIST /time_control/ interval_seconds
NAMELIST /time_control/ input_from_file
NAMELIST /time_control/ fine_input_stream
NAMELIST /time_control/ history_interval
NAMELIST /time_control/ frames_per_outfile
NAMELIST /time_control/ frames_per_auxhist1
NAMELIST /time_control/ frames_per_auxhist2
NAMELIST /time_control/ frames_per_auxhist3
NAMELIST /time_control/ frames_per_auxhist4
NAMELIST /time_control/ frames_per_auxhist5
NAMELIST /time_control/ frames_per_auxhist6
NAMELIST /time_control/ frames_per_auxhist7
NAMELIST /time_control/ frames_per_auxhist8
NAMELIST /time_control/ frames_per_auxhist9
NAMELIST /time_control/ frames_per_auxhist10
NAMELIST /time_control/ frames_per_auxhist11
NAMELIST /time_control/ restart
NAMELIST /time_control/ restart_interval
NAMELIST /time_control/ io_form_input
NAMELIST /time_control/ io_form_history
NAMELIST /time_control/ io_form_restart
NAMELIST /time_control/ io_form_boundary
NAMELIST /time_control/ debug_level
NAMELIST /time_control/ self_test_domain
NAMELIST /time_control/ history_outname
NAMELIST /time_control/ auxhist1_outname
NAMELIST /time_control/ auxhist2_outname
NAMELIST /time_control/ auxhist3_outname
NAMELIST /time_control/ auxhist4_outname
NAMELIST /time_control/ auxhist5_outname
NAMELIST /time_control/ auxhist6_outname
NAMELIST /time_control/ auxhist7_outname
NAMELIST /time_control/ auxhist8_outname
NAMELIST /time_control/ auxhist9_outname
NAMELIST /time_control/ auxhist10_outname
NAMELIST /time_control/ auxhist11_outname
NAMELIST /time_control/ history_inname
NAMELIST /time_control/ auxhist1_inname
NAMELIST /time_control/ auxhist2_inname
NAMELIST /time_control/ auxhist3_inname
NAMELIST /time_control/ auxhist4_inname
NAMELIST /time_control/ auxhist5_inname
NAMELIST /time_control/ auxhist6_inname
NAMELIST /time_control/ auxhist7_inname
NAMELIST /time_control/ auxhist8_inname
NAMELIST /time_control/ auxhist9_inname
NAMELIST /time_control/ auxhist10_inname
NAMELIST /time_control/ auxhist11_inname
NAMELIST /time_control/ auxinput1_outname
NAMELIST /time_control/ auxinput2_outname
NAMELIST /time_control/ auxinput3_outname
NAMELIST /time_control/ auxinput4_outname
NAMELIST /time_control/ auxinput5_outname
NAMELIST /time_control/ auxinput6_outname
NAMELIST /time_control/ auxinput7_outname
NAMELIST /time_control/ auxinput8_outname
NAMELIST /time_control/ auxinput9_outname
NAMELIST /time_control/ auxinput10_outname
NAMELIST /time_control/ auxinput11_outname
NAMELIST /time_control/ auxinput1_inname
NAMELIST /time_control/ auxinput2_inname
NAMELIST /time_control/ auxinput3_inname
NAMELIST /time_control/ auxinput4_inname
NAMELIST /time_control/ auxinput5_inname
NAMELIST /time_control/ auxinput6_inname
NAMELIST /time_control/ auxinput7_inname
NAMELIST /time_control/ auxinput8_inname
NAMELIST /fdda/ sgfdda_inname
NAMELIST /fdda/ gfdda_inname
NAMELIST /time_control/ auxinput11_inname
NAMELIST /time_control/ history_interval_mo
NAMELIST /time_control/ history_interval_d
NAMELIST /time_control/ history_interval_h
NAMELIST /time_control/ history_interval_m
NAMELIST /time_control/ history_interval_s
NAMELIST /time_control/ inputout_interval_mo
NAMELIST /time_control/ inputout_interval_d
NAMELIST /time_control/ inputout_interval_h
NAMELIST /time_control/ inputout_interval_m
NAMELIST /time_control/ inputout_interval_s
NAMELIST /time_control/ inputout_interval
NAMELIST /time_control/ auxhist1_interval_mo
NAMELIST /time_control/ auxhist1_interval_d
NAMELIST /time_control/ auxhist1_interval_h
NAMELIST /time_control/ auxhist1_interval_m
NAMELIST /time_control/ auxhist1_interval_s
NAMELIST /time_control/ auxhist1_interval
NAMELIST /time_control/ auxhist2_interval_mo
NAMELIST /time_control/ auxhist2_interval_d
NAMELIST /time_control/ auxhist2_interval_h
NAMELIST /time_control/ auxhist2_interval_m
NAMELIST /time_control/ auxhist2_interval_s
NAMELIST /time_control/ auxhist2_interval
NAMELIST /time_control/ auxhist3_interval_mo
NAMELIST /time_control/ auxhist3_interval_d
NAMELIST /time_control/ auxhist3_interval_h
NAMELIST /time_control/ auxhist3_interval_m
NAMELIST /time_control/ auxhist3_interval_s
NAMELIST /time_control/ auxhist3_interval
NAMELIST /time_control/ auxhist4_interval_mo
NAMELIST /time_control/ auxhist4_interval_d
NAMELIST /time_control/ auxhist4_interval_h
NAMELIST /time_control/ auxhist4_interval_m
NAMELIST /time_control/ auxhist4_interval_s
NAMELIST /time_control/ auxhist4_interval
NAMELIST /time_control/ auxhist5_interval_mo
NAMELIST /time_control/ auxhist5_interval_d
NAMELIST /time_control/ auxhist5_interval_h
NAMELIST /time_control/ auxhist5_interval_m
NAMELIST /time_control/ auxhist5_interval_s
NAMELIST /time_control/ auxhist5_interval
NAMELIST /time_control/ auxhist6_interval_mo
NAMELIST /time_control/ auxhist6_interval_d
NAMELIST /time_control/ auxhist6_interval_h
NAMELIST /time_control/ auxhist6_interval_m
NAMELIST /time_control/ auxhist6_interval_s
NAMELIST /time_control/ auxhist6_interval
NAMELIST /time_control/ auxhist7_interval_mo
NAMELIST /time_control/ auxhist7_interval_d
NAMELIST /time_control/ auxhist7_interval_h
NAMELIST /time_control/ auxhist7_interval_m
NAMELIST /time_control/ auxhist7_interval_s
NAMELIST /time_control/ auxhist7_interval
NAMELIST /time_control/ auxhist8_interval_mo
NAMELIST /time_control/ auxhist8_interval_d
NAMELIST /time_control/ auxhist8_interval_h
NAMELIST /time_control/ auxhist8_interval_m
NAMELIST /time_control/ auxhist8_interval_s
NAMELIST /time_control/ auxhist8_interval
NAMELIST /time_control/ auxhist9_interval_mo
NAMELIST /time_control/ auxhist9_interval_d
NAMELIST /time_control/ auxhist9_interval_h
NAMELIST /time_control/ auxhist9_interval_m
NAMELIST /time_control/ auxhist9_interval_s
NAMELIST /time_control/ auxhist9_interval
NAMELIST /time_control/ auxhist10_interval_mo
NAMELIST /time_control/ auxhist10_interval_d
NAMELIST /time_control/ auxhist10_interval_h
NAMELIST /time_control/ auxhist10_interval_m
NAMELIST /time_control/ auxhist10_interval_s
NAMELIST /time_control/ auxhist10_interval
NAMELIST /time_control/ auxhist11_interval_mo
NAMELIST /time_control/ auxhist11_interval_d
NAMELIST /time_control/ auxhist11_interval_h
NAMELIST /time_control/ auxhist11_interval_m
NAMELIST /time_control/ auxhist11_interval_s
NAMELIST /time_control/ auxhist11_interval
NAMELIST /time_control/ auxinput1_interval_mo
NAMELIST /time_control/ auxinput1_interval_d
NAMELIST /time_control/ auxinput1_interval_h
NAMELIST /time_control/ auxinput1_interval_m
NAMELIST /time_control/ auxinput1_interval_s
NAMELIST /time_control/ auxinput1_interval
NAMELIST /time_control/ auxinput2_interval_mo
NAMELIST /time_control/ auxinput2_interval_d
NAMELIST /time_control/ auxinput2_interval_h
NAMELIST /time_control/ auxinput2_interval_m
NAMELIST /time_control/ auxinput2_interval_s
NAMELIST /time_control/ auxinput2_interval
NAMELIST /time_control/ auxinput3_interval_mo
NAMELIST /time_control/ auxinput3_interval_d
NAMELIST /time_control/ auxinput3_interval_h
NAMELIST /time_control/ auxinput3_interval_m
NAMELIST /time_control/ auxinput3_interval_s
NAMELIST /time_control/ auxinput3_interval
NAMELIST /time_control/ auxinput4_interval_mo
NAMELIST /time_control/ auxinput4_interval_d
NAMELIST /time_control/ auxinput4_interval_h
NAMELIST /time_control/ auxinput4_interval_m
NAMELIST /time_control/ auxinput4_interval_s
NAMELIST /time_control/ auxinput4_interval
NAMELIST /time_control/ auxinput5_interval_mo
NAMELIST /time_control/ auxinput5_interval_d
NAMELIST /time_control/ auxinput5_interval_h
NAMELIST /time_control/ auxinput5_interval_m
NAMELIST /time_control/ auxinput5_interval_s
NAMELIST /time_control/ auxinput5_interval
NAMELIST /time_control/ auxinput6_interval_mo
NAMELIST /time_control/ auxinput6_interval_d
NAMELIST /time_control/ auxinput6_interval_h
NAMELIST /time_control/ auxinput6_interval_m
NAMELIST /time_control/ auxinput6_interval_s
NAMELIST /time_control/ auxinput6_interval
NAMELIST /time_control/ auxinput7_interval_mo
NAMELIST /time_control/ auxinput7_interval_d
NAMELIST /time_control/ auxinput7_interval_h
NAMELIST /time_control/ auxinput7_interval_m
NAMELIST /time_control/ auxinput7_interval_s
NAMELIST /time_control/ auxinput7_interval
NAMELIST /time_control/ auxinput8_interval_mo
NAMELIST /time_control/ auxinput8_interval_d
NAMELIST /time_control/ auxinput8_interval_h
NAMELIST /time_control/ auxinput8_interval_m
NAMELIST /time_control/ auxinput8_interval_s
NAMELIST /time_control/ auxinput8_interval
NAMELIST /fdda/ sgfdda_interval_mo
NAMELIST /fdda/ sgfdda_interval_d
NAMELIST /fdda/ sgfdda_interval_h
NAMELIST /fdda/ sgfdda_interval_m
NAMELIST /fdda/ sgfdda_interval_s
NAMELIST /fdda/ sgfdda_interval
NAMELIST /fdda/ gfdda_interval_mo
NAMELIST /fdda/ gfdda_interval_d
NAMELIST /fdda/ gfdda_interval_h
NAMELIST /fdda/ gfdda_interval_m
NAMELIST /fdda/ gfdda_interval_s
NAMELIST /fdda/ gfdda_interval
NAMELIST /time_control/ auxinput11_interval_mo
NAMELIST /time_control/ auxinput11_interval_d
NAMELIST /time_control/ auxinput11_interval_h
NAMELIST /time_control/ auxinput11_interval_m
NAMELIST /time_control/ auxinput11_interval_s
NAMELIST /time_control/ auxinput11_interval
NAMELIST /time_control/ restart_interval_mo
NAMELIST /time_control/ restart_interval_d
NAMELIST /time_control/ restart_interval_h
NAMELIST /time_control/ restart_interval_m
NAMELIST /time_control/ restart_interval_s
NAMELIST /time_control/ history_begin_y
NAMELIST /time_control/ history_begin_mo
NAMELIST /time_control/ history_begin_d
NAMELIST /time_control/ history_begin_h
NAMELIST /time_control/ history_begin_m
NAMELIST /time_control/ history_begin_s
NAMELIST /time_control/ inputout_begin_y
NAMELIST /time_control/ inputout_begin_mo
NAMELIST /time_control/ inputout_begin_d
NAMELIST /time_control/ inputout_begin_h
NAMELIST /time_control/ inputout_begin_m
NAMELIST /time_control/ inputout_begin_s
NAMELIST /time_control/ auxhist1_begin_y
NAMELIST /time_control/ auxhist1_begin_mo
NAMELIST /time_control/ auxhist1_begin_d
NAMELIST /time_control/ auxhist1_begin_h
NAMELIST /time_control/ auxhist1_begin_m
NAMELIST /time_control/ auxhist1_begin_s
NAMELIST /time_control/ auxhist2_begin_y
NAMELIST /time_control/ auxhist2_begin_mo
NAMELIST /time_control/ auxhist2_begin_d
NAMELIST /time_control/ auxhist2_begin_h
NAMELIST /time_control/ auxhist2_begin_m
NAMELIST /time_control/ auxhist2_begin_s
NAMELIST /time_control/ auxhist3_begin_y
NAMELIST /time_control/ auxhist3_begin_mo
NAMELIST /time_control/ auxhist3_begin_d
NAMELIST /time_control/ auxhist3_begin_h
NAMELIST /time_control/ auxhist3_begin_m
NAMELIST /time_control/ auxhist3_begin_s
NAMELIST /time_control/ auxhist4_begin_y
NAMELIST /time_control/ auxhist4_begin_mo
NAMELIST /time_control/ auxhist4_begin_d
NAMELIST /time_control/ auxhist4_begin_h
NAMELIST /time_control/ auxhist4_begin_m
NAMELIST /time_control/ auxhist4_begin_s
NAMELIST /time_control/ auxhist5_begin_y
NAMELIST /time_control/ auxhist5_begin_mo
NAMELIST /time_control/ auxhist5_begin_d
NAMELIST /time_control/ auxhist5_begin_h
NAMELIST /time_control/ auxhist5_begin_m
NAMELIST /time_control/ auxhist5_begin_s
NAMELIST /time_control/ auxhist6_begin_y
NAMELIST /time_control/ auxhist6_begin_mo
NAMELIST /time_control/ auxhist6_begin_d
NAMELIST /time_control/ auxhist6_begin_h
NAMELIST /time_control/ auxhist6_begin_m
NAMELIST /time_control/ auxhist6_begin_s
NAMELIST /time_control/ auxhist7_begin_y
NAMELIST /time_control/ auxhist7_begin_mo
NAMELIST /time_control/ auxhist7_begin_d
NAMELIST /time_control/ auxhist7_begin_h
NAMELIST /time_control/ auxhist7_begin_m
NAMELIST /time_control/ auxhist7_begin_s
NAMELIST /time_control/ auxhist8_begin_y
NAMELIST /time_control/ auxhist8_begin_mo
NAMELIST /time_control/ auxhist8_begin_d
NAMELIST /time_control/ auxhist8_begin_h
NAMELIST /time_control/ auxhist8_begin_m
NAMELIST /time_control/ auxhist8_begin_s
NAMELIST /time_control/ auxhist9_begin_y
NAMELIST /time_control/ auxhist9_begin_mo
NAMELIST /time_control/ auxhist9_begin_d
NAMELIST /time_control/ auxhist9_begin_h
NAMELIST /time_control/ auxhist9_begin_m
NAMELIST /time_control/ auxhist9_begin_s
NAMELIST /time_control/ auxhist10_begin_y
NAMELIST /time_control/ auxhist10_begin_mo
NAMELIST /time_control/ auxhist10_begin_d
NAMELIST /time_control/ auxhist10_begin_h
NAMELIST /time_control/ auxhist10_begin_m
NAMELIST /time_control/ auxhist10_begin_s
NAMELIST /time_control/ auxhist11_begin_y
NAMELIST /time_control/ auxhist11_begin_mo
NAMELIST /time_control/ auxhist11_begin_d
NAMELIST /time_control/ auxhist11_begin_h
NAMELIST /time_control/ auxhist11_begin_m
NAMELIST /time_control/ auxhist11_begin_s
NAMELIST /time_control/ auxinput1_begin_y
NAMELIST /time_control/ auxinput1_begin_mo
NAMELIST /time_control/ auxinput1_begin_d
NAMELIST /time_control/ auxinput1_begin_h
NAMELIST /time_control/ auxinput1_begin_m
NAMELIST /time_control/ auxinput1_begin_s
NAMELIST /time_control/ auxinput2_begin_y
NAMELIST /time_control/ auxinput2_begin_mo
NAMELIST /time_control/ auxinput2_begin_d
NAMELIST /time_control/ auxinput2_begin_h
NAMELIST /time_control/ auxinput2_begin_m
NAMELIST /time_control/ auxinput2_begin_s
NAMELIST /time_control/ auxinput3_begin_y
NAMELIST /time_control/ auxinput3_begin_mo
NAMELIST /time_control/ auxinput3_begin_d
NAMELIST /time_control/ auxinput3_begin_h
NAMELIST /time_control/ auxinput3_begin_m
NAMELIST /time_control/ auxinput3_begin_s
NAMELIST /time_control/ auxinput4_begin_y
NAMELIST /time_control/ auxinput4_begin_mo
NAMELIST /time_control/ auxinput4_begin_d
NAMELIST /time_control/ auxinput4_begin_h
NAMELIST /time_control/ auxinput4_begin_m
NAMELIST /time_control/ auxinput4_begin_s
NAMELIST /time_control/ auxinput5_begin_y
NAMELIST /time_control/ auxinput5_begin_mo
NAMELIST /time_control/ auxinput5_begin_d
NAMELIST /time_control/ auxinput5_begin_h
NAMELIST /time_control/ auxinput5_begin_m
NAMELIST /time_control/ auxinput5_begin_s
NAMELIST /time_control/ auxinput6_begin_y
NAMELIST /time_control/ auxinput6_begin_mo
NAMELIST /time_control/ auxinput6_begin_d
NAMELIST /time_control/ auxinput6_begin_h
NAMELIST /time_control/ auxinput6_begin_m
NAMELIST /time_control/ auxinput6_begin_s
NAMELIST /time_control/ auxinput7_begin_y
NAMELIST /time_control/ auxinput7_begin_mo
NAMELIST /time_control/ auxinput7_begin_d
NAMELIST /time_control/ auxinput7_begin_h
NAMELIST /time_control/ auxinput7_begin_m
NAMELIST /time_control/ auxinput7_begin_s
NAMELIST /time_control/ auxinput8_begin_y
NAMELIST /time_control/ auxinput8_begin_mo
NAMELIST /time_control/ auxinput8_begin_d
NAMELIST /time_control/ auxinput8_begin_h
NAMELIST /time_control/ auxinput8_begin_m
NAMELIST /time_control/ auxinput8_begin_s
NAMELIST /fdda/ sgfdda_begin_y
NAMELIST /fdda/ sgfdda_begin_mo
NAMELIST /fdda/ sgfdda_begin_d
NAMELIST /fdda/ sgfdda_begin_h
NAMELIST /fdda/ sgfdda_begin_m
NAMELIST /fdda/ sgfdda_begin_s
NAMELIST /fdda/ gfdda_begin_y
NAMELIST /fdda/ gfdda_begin_mo
NAMELIST /fdda/ gfdda_begin_d
NAMELIST /fdda/ gfdda_begin_h
NAMELIST /fdda/ gfdda_begin_m
NAMELIST /fdda/ gfdda_begin_s
NAMELIST /time_control/ auxinput11_begin_y
NAMELIST /time_control/ auxinput11_begin_mo
NAMELIST /time_control/ auxinput11_begin_d
NAMELIST /time_control/ auxinput11_begin_h
NAMELIST /time_control/ auxinput11_begin_m
NAMELIST /time_control/ auxinput11_begin_s
NAMELIST /time_control/ restart_begin_y
NAMELIST /time_control/ restart_begin_mo
NAMELIST /time_control/ restart_begin_d
NAMELIST /time_control/ restart_begin_h
NAMELIST /time_control/ restart_begin_m
NAMELIST /time_control/ restart_begin_s
NAMELIST /time_control/ history_end_y
NAMELIST /time_control/ history_end_mo
NAMELIST /time_control/ history_end_d
NAMELIST /time_control/ history_end_h
NAMELIST /time_control/ history_end_m
NAMELIST /time_control/ history_end_s
NAMELIST /time_control/ inputout_end_y
NAMELIST /time_control/ inputout_end_mo
NAMELIST /time_control/ inputout_end_d
NAMELIST /time_control/ inputout_end_h
NAMELIST /time_control/ inputout_end_m
NAMELIST /time_control/ inputout_end_s
NAMELIST /time_control/ auxhist1_end_y
NAMELIST /time_control/ auxhist1_end_mo
NAMELIST /time_control/ auxhist1_end_d
NAMELIST /time_control/ auxhist1_end_h
NAMELIST /time_control/ auxhist1_end_m
NAMELIST /time_control/ auxhist1_end_s
NAMELIST /time_control/ auxhist2_end_y
NAMELIST /time_control/ auxhist2_end_mo
NAMELIST /time_control/ auxhist2_end_d
NAMELIST /time_control/ auxhist2_end_h
NAMELIST /time_control/ auxhist2_end_m
NAMELIST /time_control/ auxhist2_end_s
NAMELIST /time_control/ auxhist3_end_y
NAMELIST /time_control/ auxhist3_end_mo
NAMELIST /time_control/ auxhist3_end_d
NAMELIST /time_control/ auxhist3_end_h
NAMELIST /time_control/ auxhist3_end_m
NAMELIST /time_control/ auxhist3_end_s
NAMELIST /time_control/ auxhist4_end_y
NAMELIST /time_control/ auxhist4_end_mo
NAMELIST /time_control/ auxhist4_end_d
NAMELIST /time_control/ auxhist4_end_h
NAMELIST /time_control/ auxhist4_end_m
NAMELIST /time_control/ auxhist4_end_s
NAMELIST /time_control/ auxhist5_end_y
NAMELIST /time_control/ auxhist5_end_mo
NAMELIST /time_control/ auxhist5_end_d
NAMELIST /time_control/ auxhist5_end_h
NAMELIST /time_control/ auxhist5_end_m
NAMELIST /time_control/ auxhist5_end_s
NAMELIST /time_control/ auxhist6_end_y
NAMELIST /time_control/ auxhist6_end_mo
NAMELIST /time_control/ auxhist6_end_d
NAMELIST /time_control/ auxhist6_end_h
NAMELIST /time_control/ auxhist6_end_m
NAMELIST /time_control/ auxhist6_end_s
NAMELIST /time_control/ auxhist7_end_y
NAMELIST /time_control/ auxhist7_end_mo
NAMELIST /time_control/ auxhist7_end_d
NAMELIST /time_control/ auxhist7_end_h
NAMELIST /time_control/ auxhist7_end_m
NAMELIST /time_control/ auxhist7_end_s
NAMELIST /time_control/ auxhist8_end_y
NAMELIST /time_control/ auxhist8_end_mo
NAMELIST /time_control/ auxhist8_end_d
NAMELIST /time_control/ auxhist8_end_h
NAMELIST /time_control/ auxhist8_end_m
NAMELIST /time_control/ auxhist8_end_s
NAMELIST /time_control/ auxhist9_end_y
NAMELIST /time_control/ auxhist9_end_mo
NAMELIST /time_control/ auxhist9_end_d
NAMELIST /time_control/ auxhist9_end_h
NAMELIST /time_control/ auxhist9_end_m
NAMELIST /time_control/ auxhist9_end_s
NAMELIST /time_control/ auxhist10_end_y
NAMELIST /time_control/ auxhist10_end_mo
NAMELIST /time_control/ auxhist10_end_d
NAMELIST /time_control/ auxhist10_end_h
NAMELIST /time_control/ auxhist10_end_m
NAMELIST /time_control/ auxhist10_end_s
NAMELIST /time_control/ auxhist11_end_y
NAMELIST /time_control/ auxhist11_end_mo
NAMELIST /time_control/ auxhist11_end_d
NAMELIST /time_control/ auxhist11_end_h
NAMELIST /time_control/ auxhist11_end_m
NAMELIST /time_control/ auxhist11_end_s
NAMELIST /time_control/ auxinput1_end_y
NAMELIST /time_control/ auxinput1_end_mo
NAMELIST /time_control/ auxinput1_end_d
NAMELIST /time_control/ auxinput1_end_h
NAMELIST /time_control/ auxinput1_end_m
NAMELIST /time_control/ auxinput1_end_s
NAMELIST /time_control/ auxinput2_end_y
NAMELIST /time_control/ auxinput2_end_mo
NAMELIST /time_control/ auxinput2_end_d
NAMELIST /time_control/ auxinput2_end_h
NAMELIST /time_control/ auxinput2_end_m
NAMELIST /time_control/ auxinput2_end_s
NAMELIST /time_control/ auxinput3_end_y
NAMELIST /time_control/ auxinput3_end_mo
NAMELIST /time_control/ auxinput3_end_d
NAMELIST /time_control/ auxinput3_end_h
NAMELIST /time_control/ auxinput3_end_m
NAMELIST /time_control/ auxinput3_end_s
NAMELIST /time_control/ auxinput4_end_y
NAMELIST /time_control/ auxinput4_end_mo
NAMELIST /time_control/ auxinput4_end_d
NAMELIST /time_control/ auxinput4_end_h
NAMELIST /time_control/ auxinput4_end_m
NAMELIST /time_control/ auxinput4_end_s
NAMELIST /time_control/ auxinput5_end_y
NAMELIST /time_control/ auxinput5_end_mo
NAMELIST /time_control/ auxinput5_end_d
NAMELIST /time_control/ auxinput5_end_h
NAMELIST /time_control/ auxinput5_end_m
NAMELIST /time_control/ auxinput5_end_s
NAMELIST /time_control/ auxinput6_end_y
NAMELIST /time_control/ auxinput6_end_mo
NAMELIST /time_control/ auxinput6_end_d
NAMELIST /time_control/ auxinput6_end_h
NAMELIST /time_control/ auxinput6_end_m
NAMELIST /time_control/ auxinput6_end_s
NAMELIST /time_control/ auxinput7_end_y
NAMELIST /time_control/ auxinput7_end_mo
NAMELIST /time_control/ auxinput7_end_d
NAMELIST /time_control/ auxinput7_end_h
NAMELIST /time_control/ auxinput7_end_m
NAMELIST /time_control/ auxinput7_end_s
NAMELIST /time_control/ auxinput8_end_y
NAMELIST /time_control/ auxinput8_end_mo
NAMELIST /time_control/ auxinput8_end_d
NAMELIST /time_control/ auxinput8_end_h
NAMELIST /time_control/ auxinput8_end_m
NAMELIST /time_control/ auxinput8_end_s
NAMELIST /fdda/ sgfdda_end_y
NAMELIST /fdda/ sgfdda_end_mo
NAMELIST /fdda/ sgfdda_end_d
NAMELIST /fdda/ sgfdda_end_h
NAMELIST /fdda/ sgfdda_end_m
NAMELIST /fdda/ sgfdda_end_s
NAMELIST /fdda/ gfdda_end_y
NAMELIST /fdda/ gfdda_end_mo
NAMELIST /fdda/ gfdda_end_d
NAMELIST /fdda/ gfdda_end_h
NAMELIST /fdda/ gfdda_end_m
NAMELIST /fdda/ gfdda_end_s
NAMELIST /time_control/ auxinput11_end_y
NAMELIST /time_control/ auxinput11_end_mo
NAMELIST /time_control/ auxinput11_end_d
NAMELIST /time_control/ auxinput11_end_h
NAMELIST /time_control/ auxinput11_end_m
NAMELIST /time_control/ auxinput11_end_s
NAMELIST /time_control/ io_form_auxinput1
NAMELIST /time_control/ io_form_auxinput2
NAMELIST /time_control/ io_form_auxinput3
NAMELIST /time_control/ io_form_auxinput4
NAMELIST /time_control/ io_form_auxinput5
NAMELIST /time_control/ io_form_auxinput6
NAMELIST /time_control/ io_form_auxinput7
NAMELIST /time_control/ io_form_auxinput8
NAMELIST /fdda/ io_form_sgfdda
NAMELIST /fdda/ io_form_gfdda
NAMELIST /time_control/ io_form_auxinput11
NAMELIST /time_control/ io_form_auxhist1
NAMELIST /time_control/ io_form_auxhist2
NAMELIST /time_control/ io_form_auxhist3
NAMELIST /time_control/ io_form_auxhist4
NAMELIST /time_control/ io_form_auxhist5
NAMELIST /time_control/ io_form_auxhist6
NAMELIST /time_control/ io_form_auxhist7
NAMELIST /time_control/ io_form_auxhist8
NAMELIST /time_control/ io_form_auxhist9
NAMELIST /time_control/ io_form_auxhist10
NAMELIST /time_control/ io_form_auxhist11
NAMELIST /time_control/ reset_simulation_start
NAMELIST /domains/ sr_x
NAMELIST /domains/ sr_y
NAMELIST /time_control/ julyr
NAMELIST /time_control/ julday
NAMELIST /time_control/ gmt
NAMELIST /time_control/ input_inname
NAMELIST /time_control/ input_outname
NAMELIST /time_control/ bdy_inname
NAMELIST /time_control/ bdy_outname
NAMELIST /time_control/ rst_inname
NAMELIST /time_control/ rst_outname
NAMELIST /time_control/ write_input
NAMELIST /time_control/ write_restart_at_0h
NAMELIST /time_control/ adjust_output_times
NAMELIST /time_control/ adjust_input_times
NAMELIST /time_control/ tstart
NAMELIST /time_control/ nocolons
NAMELIST /time_control/ cycling
NAMELIST /dfi_control/ dfi_opt
NAMELIST /dfi_control/ dfi_nfilter
NAMELIST /dfi_control/ dfi_write_filtered_input
NAMELIST /dfi_control/ dfi_write_dfi_history
NAMELIST /dfi_control/ dfi_cutoff_seconds
NAMELIST /dfi_control/ dfi_time_dim
NAMELIST /dfi_control/ dfi_fwdstop_year
NAMELIST /dfi_control/ dfi_fwdstop_month
NAMELIST /dfi_control/ dfi_fwdstop_day
NAMELIST /dfi_control/ dfi_fwdstop_hour
NAMELIST /dfi_control/ dfi_fwdstop_minute
NAMELIST /dfi_control/ dfi_fwdstop_second
NAMELIST /dfi_control/ dfi_bckstop_year
NAMELIST /dfi_control/ dfi_bckstop_month
NAMELIST /dfi_control/ dfi_bckstop_day
NAMELIST /dfi_control/ dfi_bckstop_hour
NAMELIST /dfi_control/ dfi_bckstop_minute
NAMELIST /dfi_control/ dfi_bckstop_second
NAMELIST /domains/ time_step
NAMELIST /domains/ time_step_fract_num
NAMELIST /domains/ time_step_fract_den
NAMELIST /domains/ max_dom
NAMELIST /domains/ s_we
NAMELIST /domains/ e_we
NAMELIST /domains/ s_sn
NAMELIST /domains/ e_sn
NAMELIST /domains/ s_vert
NAMELIST /domains/ e_vert
NAMELIST /domains/ dx
NAMELIST /domains/ dy
NAMELIST /domains/ grid_id
NAMELIST /domains/ grid_allowed
NAMELIST /domains/ parent_id
NAMELIST /domains/ i_parent_start
NAMELIST /domains/ j_parent_start
NAMELIST /domains/ parent_grid_ratio
NAMELIST /domains/ parent_time_step_ratio
NAMELIST /domains/ feedback
NAMELIST /domains/ smooth_option
NAMELIST /domains/ ztop
NAMELIST /domains/ moad_grid_ratio
NAMELIST /domains/ moad_time_step_ratio
NAMELIST /domains/ shw
NAMELIST /domains/ tile_sz_x
NAMELIST /domains/ tile_sz_y
NAMELIST /domains/ numtiles
NAMELIST /domains/ nproc_x
NAMELIST /domains/ nproc_y
NAMELIST /domains/ irand
NAMELIST /domains/ ts_buf_size
NAMELIST /domains/ max_ts_locs
NAMELIST /domains/ num_moves
NAMELIST /domains/ move_id
NAMELIST /domains/ move_interval
NAMELIST /domains/ move_cd_x
NAMELIST /domains/ move_cd_y
NAMELIST /domains/ swap_x
NAMELIST /domains/ swap_y
NAMELIST /domains/ cycle_x
NAMELIST /domains/ cycle_y
NAMELIST /domains/ reorder_mesh
NAMELIST /domains/ perturb_input
NAMELIST /domains/ eta_levels
NAMELIST /domains/ ptsgm
NAMELIST /domains/ num_metgrid_levels
NAMELIST /domains/ p_top_requested
NAMELIST /physics/ mp_physics
NAMELIST /physics/ ra_lw_physics
NAMELIST /physics/ ra_sw_physics
NAMELIST /physics/ radt
NAMELIST /physics/ sf_sfclay_physics
NAMELIST /physics/ sf_surface_physics
NAMELIST /physics/ bl_pbl_physics
NAMELIST /physics/ sf_urban_physics
NAMELIST /physics/ bldt
NAMELIST /physics/ cu_physics
NAMELIST /physics/ cudt
NAMELIST /physics/ gsmdt
NAMELIST /physics/ isfflx
NAMELIST /physics/ ifsnow
NAMELIST /physics/ icloud
NAMELIST /physics/ swrad_scat
NAMELIST /physics/ surface_input_source
NAMELIST /physics/ num_soil_layers
NAMELIST /physics/ num_urban_layers
NAMELIST /physics/ maxiens
NAMELIST /physics/ maxens
NAMELIST /physics/ maxens2
NAMELIST /physics/ maxens3
NAMELIST /physics/ ensdim
NAMELIST /physics/ chem_opt
NAMELIST /physics/ num_land_cat
NAMELIST /physics/ num_soil_cat
NAMELIST /physics/ mp_zero_out
NAMELIST /physics/ mp_zero_out_thresh
NAMELIST /physics/ seaice_threshold
NAMELIST /physics/ fractional_seaice
NAMELIST /physics/ sst_update
NAMELIST /physics/ usemonalb
NAMELIST /physics/ rdmaxalb
NAMELIST /physics/ rdlai2d
NAMELIST /physics/ gwd_opt
NAMELIST /physics/ idtad
NAMELIST /physics/ nsoil
NAMELIST /physics/ nphs
NAMELIST /physics/ ncnvc
NAMELIST /physics/ nrads
NAMELIST /physics/ nradl
NAMELIST /physics/ tprec
NAMELIST /physics/ theat
NAMELIST /physics/ tclod
NAMELIST /physics/ trdsw
NAMELIST /physics/ trdlw
NAMELIST /physics/ tsrfc
NAMELIST /physics/ pcpflg
NAMELIST /physics/ sigma
NAMELIST /physics/ sfenth
NAMELIST /physics/ co2tf
NAMELIST /physics/ ra_call_offset
NAMELIST /physics/ cam_abs_freq_s
NAMELIST /physics/ levsiz
NAMELIST /physics/ paerlev
NAMELIST /physics/ cam_abs_dim1
NAMELIST /physics/ cam_abs_dim2
NAMELIST /physics/ cu_rad_feedback
NAMELIST /dynamics/ dyn_opt
NAMELIST /dynamics/ rk_ord
NAMELIST /dynamics/ w_damping
NAMELIST /dynamics/ diff_opt
NAMELIST /dynamics/ km_opt
NAMELIST /dynamics/ damp_opt
NAMELIST /dynamics/ zdamp
NAMELIST /dynamics/ base_pres
NAMELIST /dynamics/ base_temp
NAMELIST /dynamics/ base_lapse
NAMELIST /dynamics/ iso_temp
NAMELIST /dynamics/ dampcoef
NAMELIST /dynamics/ khdif
NAMELIST /dynamics/ kvdif
NAMELIST /dynamics/ c_s
NAMELIST /dynamics/ c_k
NAMELIST /dynamics/ smdiv
NAMELIST /dynamics/ emdiv
NAMELIST /dynamics/ epssm
NAMELIST /dynamics/ non_hydrostatic
NAMELIST /dynamics/ time_step_sound
NAMELIST /dynamics/ h_mom_adv_order
NAMELIST /dynamics/ v_mom_adv_order
NAMELIST /dynamics/ h_sca_adv_order
NAMELIST /dynamics/ v_sca_adv_order
NAMELIST /dynamics/ top_radiation
NAMELIST /dynamics/ tke_upper_bound
NAMELIST /dynamics/ tke_drag_coefficient
NAMELIST /dynamics/ tke_heat_flux
NAMELIST /dynamics/ pert_coriolis
NAMELIST /dynamics/ euler_adv
NAMELIST /dynamics/ idtadt
NAMELIST /dynamics/ idtadc
NAMELIST /dynamics/ boundary_flux
NAMELIST /bdy_control/ spec_bdy_width
NAMELIST /bdy_control/ spec_zone
NAMELIST /bdy_control/ relax_zone
NAMELIST /bdy_control/ specified
NAMELIST /bdy_control/ periodic_x
NAMELIST /bdy_control/ symmetric_xs
NAMELIST /bdy_control/ symmetric_xe
NAMELIST /bdy_control/ open_xs
NAMELIST /bdy_control/ open_xe
NAMELIST /bdy_control/ periodic_y
NAMELIST /bdy_control/ symmetric_ys
NAMELIST /bdy_control/ symmetric_ye
NAMELIST /bdy_control/ open_ys
NAMELIST /bdy_control/ open_ye
NAMELIST /bdy_control/ polar
NAMELIST /bdy_control/ nested
NAMELIST /bdy_control/ real_data_init_type
NAMELIST /grib2/ background_proc_id
NAMELIST /grib2/ forecast_proc_id
NAMELIST /grib2/ production_status
NAMELIST /grib2/ compression



     LOGICAL             :: use_crtm_kmatrix_fast
     CHARACTER (LEN=256) :: spccoeff_file
     CHARACTER (LEN=256) :: taucoeff_file
     CHARACTER (LEN=256) :: aerosolcoeff_file
     CHARACTER (LEN=256) :: cloudcoeff_file
     CHARACTER (LEN=256) :: emiscoeff_file
     NAMELIST /wrfvar14/ use_crtm_kmatrix_fast
     NAMELIST /wrfvar14/ spccoeff_file
     NAMELIST /wrfvar14/ taucoeff_file
     NAMELIST /wrfvar14/ aerosolcoeff_file
     NAMELIST /wrfvar14/ cloudcoeff_file
     NAMELIST /wrfvar14/ emiscoeff_file





     REWIND ( UNIT = nml_read_unit )
     READ   ( UNIT = nml_read_unit , NML = wrfvar14 , iostat=nml_error )

     IF ( nml_error .EQ. 0 ) then    
        CALL wrf_debug(0,"Are use_crtm_kmatrix_fast, spccoeff_file, taucoeff_file, "// &
                         "aerosolcoeff_file, cloudcoeff_file, emiscoeff_file still in your "// &
                         TRIM(nml_name)//" namelist?")
        CALL wrf_error_fatal3("",17486,&
"Remove them as they are obsolete.")
     ELSE     
        return
     ENDIF

   END SUBROUTINE wrfvar_alt_nml_wrfvar14

END MODULE module_configure


SUBROUTINE set_scalar_indices_from_config ( idomain , dummy2, dummy1 )
  USE module_driver_constants
  USE module_state_description
  USE module_wrf_error
  USE module_configure, ONLY : model_config_rec
  USE module_scalar_tables
  IMPLICIT NONE
  INTEGER , INTENT(IN)  :: idomain
  INTEGER               :: dummy1
  INTEGER               :: dummy2
































  P_qv = 1 ; F_qv = .FALSE. 
  P_qc = 1 ; F_qc = .FALSE. 
  P_qr = 1 ; F_qr = .FALSE. 
  P_qi = 1 ; F_qi = .FALSE. 
  P_qs = 1 ; F_qs = .FALSE. 
  P_qg = 1 ; F_qg = .FALSE. 
  P_dfi_qv = 1 ; F_dfi_qv = .FALSE. 
  P_dfi_qc = 1 ; F_dfi_qc = .FALSE. 
  P_dfi_qr = 1 ; F_dfi_qr = .FALSE. 
  P_dfi_qi = 1 ; F_dfi_qi = .FALSE. 
  P_dfi_qs = 1 ; F_dfi_qs = .FALSE. 
  P_dfi_qg = 1 ; F_dfi_qg = .FALSE. 
  P_qni = 1 ; F_qni = .FALSE. 
  P_qns = 1 ; F_qns = .FALSE. 
  P_qnr = 1 ; F_qnr = .FALSE. 
  P_qng = 1 ; F_qng = .FALSE. 
  P_qnn = 1 ; F_qnn = .FALSE. 
  P_qnc = 1 ; F_qnc = .FALSE. 
  P_dfi_qndrop = 1 ; F_dfi_qndrop = .FALSE. 
  P_dfi_qni = 1 ; F_dfi_qni = .FALSE. 
  P_dfi_qt = 1 ; F_dfi_qt = .FALSE. 
  P_dfi_qns = 1 ; F_dfi_qns = .FALSE. 
  P_dfi_qnr = 1 ; F_dfi_qnr = .FALSE. 
  P_dfi_qng = 1 ; F_dfi_qng = .FALSE. 
  P_dfi_qnn = 1 ; F_dfi_qnn = .FALSE. 
  P_dfi_qnc = 1 ; F_dfi_qnc = .FALSE. 
  IF (model_config_rec%mp_physics(idomain)==0)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_stream_table( idomain, P_qv ) = 33554432
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   F_qv = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==1)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_stream_table( idomain, P_qv ) = 33554432
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_stream_table( idomain, P_qc ) = 33554432
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_stream_table( idomain, P_qr ) = 33554432
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   F_qr = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==2)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_stream_table( idomain, P_qv ) = 33554432
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_stream_table( idomain, P_qc ) = 33554432
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_stream_table( idomain, P_qr ) = 33554432
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_stream_table( idomain, P_qi ) = 33554432
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_stream_table( idomain, P_qs ) = 33554432
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   F_qs = .TRUE.
   IF ( moist_index_table( PARAM_qg , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qg = moist_num_table(idomain)
     moist_index_table( PARAM_qg , idomain ) = P_qg
   ELSE
     P_qg = moist_index_table( PARAM_qg , idomain )
   END IF
   moist_stream_table( idomain, P_qg ) = 33554432
   moist_dname_table( idomain, P_qg ) = 'QGRAUP'
   moist_desc_table( idomain, P_qg ) = 'Graupel mixing ratio'
   moist_units_table( idomain, P_qg ) = 'kg kg-1'
   F_qg = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==3)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_stream_table( idomain, P_qv ) = 33554432
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_stream_table( idomain, P_qc ) = 33554432
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_stream_table( idomain, P_qr ) = 33554432
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   F_qr = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==4)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_stream_table( idomain, P_qv ) = 33554432
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_stream_table( idomain, P_qc ) = 33554432
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_stream_table( idomain, P_qr ) = 33554432
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_stream_table( idomain, P_qi ) = 33554432
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_stream_table( idomain, P_qs ) = 33554432
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   F_qs = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==5)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_stream_table( idomain, P_qv ) = 33554432
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_stream_table( idomain, P_qc ) = 33554432
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_stream_table( idomain, P_qr ) = 33554432
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_stream_table( idomain, P_qs ) = 33554432
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   F_qs = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==6)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_stream_table( idomain, P_qv ) = 33554432
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_stream_table( idomain, P_qc ) = 33554432
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_stream_table( idomain, P_qr ) = 33554432
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_stream_table( idomain, P_qi ) = 33554432
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_stream_table( idomain, P_qs ) = 33554432
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   F_qs = .TRUE.
   IF ( moist_index_table( PARAM_qg , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qg = moist_num_table(idomain)
     moist_index_table( PARAM_qg , idomain ) = P_qg
   ELSE
     P_qg = moist_index_table( PARAM_qg , idomain )
   END IF
   moist_stream_table( idomain, P_qg ) = 33554432
   moist_dname_table( idomain, P_qg ) = 'QGRAUP'
   moist_desc_table( idomain, P_qg ) = 'Graupel mixing ratio'
   moist_units_table( idomain, P_qg ) = 'kg kg-1'
   F_qg = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==7)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_stream_table( idomain, P_qv ) = 33554432
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_stream_table( idomain, P_qc ) = 33554432
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_stream_table( idomain, P_qr ) = 33554432
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_stream_table( idomain, P_qi ) = 33554432
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_stream_table( idomain, P_qs ) = 33554432
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   F_qs = .TRUE.
   IF ( moist_index_table( PARAM_qg , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qg = moist_num_table(idomain)
     moist_index_table( PARAM_qg , idomain ) = P_qg
   ELSE
     P_qg = moist_index_table( PARAM_qg , idomain )
   END IF
   moist_stream_table( idomain, P_qg ) = 33554432
   moist_dname_table( idomain, P_qg ) = 'QGRAUP'
   moist_desc_table( idomain, P_qg ) = 'Graupel mixing ratio'
   moist_units_table( idomain, P_qg ) = 'kg kg-1'
   F_qg = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==8)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_stream_table( idomain, P_qv ) = 33554432
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_stream_table( idomain, P_qc ) = 33554432
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_stream_table( idomain, P_qr ) = 33554432
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_stream_table( idomain, P_qi ) = 33554432
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_stream_table( idomain, P_qs ) = 33554432
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   F_qs = .TRUE.
   IF ( moist_index_table( PARAM_qg , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qg = moist_num_table(idomain)
     moist_index_table( PARAM_qg , idomain ) = P_qg
   ELSE
     P_qg = moist_index_table( PARAM_qg , idomain )
   END IF
   moist_stream_table( idomain, P_qg ) = 33554432
   moist_dname_table( idomain, P_qg ) = 'QGRAUP'
   moist_desc_table( idomain, P_qg ) = 'Graupel mixing ratio'
   moist_units_table( idomain, P_qg ) = 'kg kg-1'
   F_qg = .TRUE.
   IF ( scalar_index_table( PARAM_qni , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qni = scalar_num_table(idomain)
     scalar_index_table( PARAM_qni , idomain ) = P_qni
   ELSE
     P_qni = scalar_index_table( PARAM_qni , idomain )
   END IF
   scalar_stream_table( idomain, P_qni ) = 2113953792
   scalar_dname_table( idomain, P_qni ) = 'QNI'
   scalar_desc_table( idomain, P_qni ) = 'Ice Number concentration'
   scalar_units_table( idomain, P_qni ) = '  kg(-1)'
   F_qni = .TRUE.
   IF ( scalar_index_table( PARAM_qnr , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnr = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnr , idomain ) = P_qnr
   ELSE
     P_qnr = scalar_index_table( PARAM_qnr , idomain )
   END IF
   scalar_stream_table( idomain, P_qnr ) = 2113953792
   scalar_dname_table( idomain, P_qnr ) = 'QNR'
   scalar_desc_table( idomain, P_qnr ) = 'Rain Number concentration'
   scalar_units_table( idomain, P_qnr ) = '  kg(-1)'
   F_qnr = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==10)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_stream_table( idomain, P_qv ) = 33554432
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_stream_table( idomain, P_qc ) = 33554432
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_stream_table( idomain, P_qr ) = 33554432
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_stream_table( idomain, P_qi ) = 33554432
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_stream_table( idomain, P_qs ) = 33554432
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   F_qs = .TRUE.
   IF ( moist_index_table( PARAM_qg , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qg = moist_num_table(idomain)
     moist_index_table( PARAM_qg , idomain ) = P_qg
   ELSE
     P_qg = moist_index_table( PARAM_qg , idomain )
   END IF
   moist_stream_table( idomain, P_qg ) = 33554432
   moist_dname_table( idomain, P_qg ) = 'QGRAUP'
   moist_desc_table( idomain, P_qg ) = 'Graupel mixing ratio'
   moist_units_table( idomain, P_qg ) = 'kg kg-1'
   F_qg = .TRUE.
   IF ( scalar_index_table( PARAM_qni , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qni = scalar_num_table(idomain)
     scalar_index_table( PARAM_qni , idomain ) = P_qni
   ELSE
     P_qni = scalar_index_table( PARAM_qni , idomain )
   END IF
   scalar_stream_table( idomain, P_qni ) = 2113953792
   scalar_dname_table( idomain, P_qni ) = 'QNI'
   scalar_desc_table( idomain, P_qni ) = 'Ice Number concentration'
   scalar_units_table( idomain, P_qni ) = '  kg(-1)'
   F_qni = .TRUE.
   IF ( scalar_index_table( PARAM_qns , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qns = scalar_num_table(idomain)
     scalar_index_table( PARAM_qns , idomain ) = P_qns
   ELSE
     P_qns = scalar_index_table( PARAM_qns , idomain )
   END IF
   scalar_stream_table( idomain, P_qns ) = 2113953792
   scalar_dname_table( idomain, P_qns ) = 'QNS'
   scalar_desc_table( idomain, P_qns ) = 'Snow Number concentration'
   scalar_units_table( idomain, P_qns ) = '  kg(-1)'
   F_qns = .TRUE.
   IF ( scalar_index_table( PARAM_qnr , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnr = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnr , idomain ) = P_qnr
   ELSE
     P_qnr = scalar_index_table( PARAM_qnr , idomain )
   END IF
   scalar_stream_table( idomain, P_qnr ) = 2113953792
   scalar_dname_table( idomain, P_qnr ) = 'QNR'
   scalar_desc_table( idomain, P_qnr ) = 'Rain Number concentration'
   scalar_units_table( idomain, P_qnr ) = '  kg(-1)'
   F_qnr = .TRUE.
   IF ( scalar_index_table( PARAM_qng , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qng = scalar_num_table(idomain)
     scalar_index_table( PARAM_qng , idomain ) = P_qng
   ELSE
     P_qng = scalar_index_table( PARAM_qng , idomain )
   END IF
   scalar_stream_table( idomain, P_qng ) = 2113953792
   scalar_dname_table( idomain, P_qng ) = 'QNG'
   scalar_desc_table( idomain, P_qng ) = 'Graupel Number concentration'
   scalar_units_table( idomain, P_qng ) = '  kg(-1)'
   F_qng = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==14)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_stream_table( idomain, P_qv ) = 33554432
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_stream_table( idomain, P_qc ) = 33554432
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_stream_table( idomain, P_qr ) = 33554432
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_stream_table( idomain, P_qi ) = 33554432
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_stream_table( idomain, P_qs ) = 33554432
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   F_qs = .TRUE.
   IF ( scalar_index_table( PARAM_qnn , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnn = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnn , idomain ) = P_qnn
   ELSE
     P_qnn = scalar_index_table( PARAM_qnn , idomain )
   END IF
   scalar_stream_table( idomain, P_qnn ) = 2113953792
   scalar_dname_table( idomain, P_qnn ) = 'QNCCN'
   scalar_desc_table( idomain, P_qnn ) = 'CCN Number concentration'
   scalar_units_table( idomain, P_qnn ) = '  kg(-1)'
   F_qnn = .TRUE.
   IF ( scalar_index_table( PARAM_qnc , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnc = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnc , idomain ) = P_qnc
   ELSE
     P_qnc = scalar_index_table( PARAM_qnc , idomain )
   END IF
   scalar_stream_table( idomain, P_qnc ) = 2113953792
   scalar_dname_table( idomain, P_qnc ) = 'QNCLOUD'
   scalar_desc_table( idomain, P_qnc ) = 'cloud water Number concentration'
   scalar_units_table( idomain, P_qnc ) = '  kg(-1)'
   F_qnc = .TRUE.
   IF ( scalar_index_table( PARAM_qnr , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnr = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnr , idomain ) = P_qnr
   ELSE
     P_qnr = scalar_index_table( PARAM_qnr , idomain )
   END IF
   scalar_stream_table( idomain, P_qnr ) = 2113953792
   scalar_dname_table( idomain, P_qnr ) = 'QNR'
   scalar_desc_table( idomain, P_qnr ) = 'Rain Number concentration'
   scalar_units_table( idomain, P_qnr ) = '  kg(-1)'
   F_qnr = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==16)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_stream_table( idomain, P_qv ) = 33554432
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_stream_table( idomain, P_qc ) = 33554432
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_stream_table( idomain, P_qr ) = 33554432
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_stream_table( idomain, P_qi ) = 33554432
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_stream_table( idomain, P_qs ) = 33554432
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   F_qs = .TRUE.
   IF ( moist_index_table( PARAM_qg , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qg = moist_num_table(idomain)
     moist_index_table( PARAM_qg , idomain ) = P_qg
   ELSE
     P_qg = moist_index_table( PARAM_qg , idomain )
   END IF
   moist_stream_table( idomain, P_qg ) = 33554432
   moist_dname_table( idomain, P_qg ) = 'QGRAUP'
   moist_desc_table( idomain, P_qg ) = 'Graupel mixing ratio'
   moist_units_table( idomain, P_qg ) = 'kg kg-1'
   F_qg = .TRUE.
   IF ( scalar_index_table( PARAM_qnn , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnn = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnn , idomain ) = P_qnn
   ELSE
     P_qnn = scalar_index_table( PARAM_qnn , idomain )
   END IF
   scalar_stream_table( idomain, P_qnn ) = 2113953792
   scalar_dname_table( idomain, P_qnn ) = 'QNCCN'
   scalar_desc_table( idomain, P_qnn ) = 'CCN Number concentration'
   scalar_units_table( idomain, P_qnn ) = '  kg(-1)'
   F_qnn = .TRUE.
   IF ( scalar_index_table( PARAM_qnc , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnc = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnc , idomain ) = P_qnc
   ELSE
     P_qnc = scalar_index_table( PARAM_qnc , idomain )
   END IF
   scalar_stream_table( idomain, P_qnc ) = 2113953792
   scalar_dname_table( idomain, P_qnc ) = 'QNCLOUD'
   scalar_desc_table( idomain, P_qnc ) = 'cloud water Number concentration'
   scalar_units_table( idomain, P_qnc ) = '  kg(-1)'
   F_qnc = .TRUE.
   IF ( scalar_index_table( PARAM_qnr , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnr = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnr , idomain ) = P_qnr
   ELSE
     P_qnr = scalar_index_table( PARAM_qnr , idomain )
   END IF
   scalar_stream_table( idomain, P_qnr ) = 2113953792
   scalar_dname_table( idomain, P_qnr ) = 'QNR'
   scalar_desc_table( idomain, P_qnr ) = 'Rain Number concentration'
   scalar_units_table( idomain, P_qnr ) = '  kg(-1)'
   F_qnr = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==98)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_stream_table( idomain, P_qv ) = 33554432
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_stream_table( idomain, P_qc ) = 33554432
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_stream_table( idomain, P_qr ) = 33554432
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_stream_table( idomain, P_qi ) = 33554432
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_stream_table( idomain, P_qs ) = 33554432
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   F_qs = .TRUE.
   IF ( moist_index_table( PARAM_qg , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qg = moist_num_table(idomain)
     moist_index_table( PARAM_qg , idomain ) = P_qg
   ELSE
     P_qg = moist_index_table( PARAM_qg , idomain )
   END IF
   moist_stream_table( idomain, P_qg ) = 33554432
   moist_dname_table( idomain, P_qg ) = 'QGRAUP'
   moist_desc_table( idomain, P_qg ) = 'Graupel mixing ratio'
   moist_units_table( idomain, P_qg ) = 'kg kg-1'
   F_qg = .TRUE.
   IF ( scalar_index_table( PARAM_qni , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qni = scalar_num_table(idomain)
     scalar_index_table( PARAM_qni , idomain ) = P_qni
   ELSE
     P_qni = scalar_index_table( PARAM_qni , idomain )
   END IF
   scalar_stream_table( idomain, P_qni ) = 2113953792
   scalar_dname_table( idomain, P_qni ) = 'QNI'
   scalar_desc_table( idomain, P_qni ) = 'Ice Number concentration'
   scalar_units_table( idomain, P_qni ) = '  kg(-1)'
   F_qni = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==-1)THEN
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==0)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qv ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   F_dfi_qv = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==1)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qv ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qc ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qr ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   F_dfi_qr = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==2)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qv ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qc ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qr ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qi , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qi = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qi , idomain ) = P_dfi_qi
   ELSE
     P_dfi_qi = dfi_moist_index_table( PARAM_dfi_qi , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qi ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qi ) = 'QICE'
   dfi_moist_desc_table( idomain, P_dfi_qi ) = 'Ice mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qi ) = 'kg kg-1'
   F_dfi_qi = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qs ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   F_dfi_qs = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qg , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qg = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qg , idomain ) = P_dfi_qg
   ELSE
     P_dfi_qg = dfi_moist_index_table( PARAM_dfi_qg , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qg ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qg ) = 'QGRAUP'
   dfi_moist_desc_table( idomain, P_dfi_qg ) = 'Graupel mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qg ) = 'kg kg-1'
   F_dfi_qg = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==3)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qv ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qc ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qr ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   F_dfi_qr = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==4)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qv ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qc ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qr ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qi , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qi = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qi , idomain ) = P_dfi_qi
   ELSE
     P_dfi_qi = dfi_moist_index_table( PARAM_dfi_qi , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qi ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qi ) = 'QICE'
   dfi_moist_desc_table( idomain, P_dfi_qi ) = 'Ice mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qi ) = 'kg kg-1'
   F_dfi_qi = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qs ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   F_dfi_qs = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==5)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qv ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qc ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qr ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qi , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qi = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qi , idomain ) = P_dfi_qi
   ELSE
     P_dfi_qi = dfi_moist_index_table( PARAM_dfi_qi , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qi ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qi ) = 'QICE'
   dfi_moist_desc_table( idomain, P_dfi_qi ) = 'Ice mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qi ) = 'kg kg-1'
   F_dfi_qi = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qs ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   F_dfi_qs = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qg , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qg = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qg , idomain ) = P_dfi_qg
   ELSE
     P_dfi_qg = dfi_moist_index_table( PARAM_dfi_qg , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qg ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qg ) = 'QGRAUP'
   dfi_moist_desc_table( idomain, P_dfi_qg ) = 'Graupel mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qg ) = 'kg kg-1'
   F_dfi_qg = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==6)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qv ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qc ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qr ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qi , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qi = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qi , idomain ) = P_dfi_qi
   ELSE
     P_dfi_qi = dfi_moist_index_table( PARAM_dfi_qi , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qi ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qi ) = 'QICE'
   dfi_moist_desc_table( idomain, P_dfi_qi ) = 'Ice mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qi ) = 'kg kg-1'
   F_dfi_qi = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qs ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   F_dfi_qs = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qg , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qg = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qg , idomain ) = P_dfi_qg
   ELSE
     P_dfi_qg = dfi_moist_index_table( PARAM_dfi_qg , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qg ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qg ) = 'QGRAUP'
   dfi_moist_desc_table( idomain, P_dfi_qg ) = 'Graupel mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qg ) = 'kg kg-1'
   F_dfi_qg = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==7)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qv ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qc ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qr ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qi , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qi = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qi , idomain ) = P_dfi_qi
   ELSE
     P_dfi_qi = dfi_moist_index_table( PARAM_dfi_qi , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qi ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qi ) = 'QICE'
   dfi_moist_desc_table( idomain, P_dfi_qi ) = 'Ice mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qi ) = 'kg kg-1'
   F_dfi_qi = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qs ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   F_dfi_qs = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qg , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qg = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qg , idomain ) = P_dfi_qg
   ELSE
     P_dfi_qg = dfi_moist_index_table( PARAM_dfi_qg , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qg ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qg ) = 'QGRAUP'
   dfi_moist_desc_table( idomain, P_dfi_qg ) = 'Graupel mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qg ) = 'kg kg-1'
   F_dfi_qg = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==8)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qv ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qc ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qr ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qi , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qi = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qi , idomain ) = P_dfi_qi
   ELSE
     P_dfi_qi = dfi_moist_index_table( PARAM_dfi_qi , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qi ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qi ) = 'QICE'
   dfi_moist_desc_table( idomain, P_dfi_qi ) = 'Ice mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qi ) = 'kg kg-1'
   F_dfi_qi = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qs ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   F_dfi_qs = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qg , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qg = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qg , idomain ) = P_dfi_qg
   ELSE
     P_dfi_qg = dfi_moist_index_table( PARAM_dfi_qg , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qg ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qg ) = 'QGRAUP'
   dfi_moist_desc_table( idomain, P_dfi_qg ) = 'Graupel mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qg ) = 'kg kg-1'
   F_dfi_qg = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qni , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qni = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qni , idomain ) = P_dfi_qni
   ELSE
     P_dfi_qni = dfi_scalar_index_table( PARAM_dfi_qni , idomain )
   END IF
   dfi_scalar_stream_table( idomain, P_dfi_qni ) = 2113929216
   dfi_scalar_dname_table( idomain, P_dfi_qni ) = 'DFI_QNICE'
   dfi_scalar_desc_table( idomain, P_dfi_qni ) = 'Ice Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qni ) = '  kg-1'
   F_dfi_qni = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnr , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnr = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnr , idomain ) = P_dfi_qnr
   ELSE
     P_dfi_qnr = dfi_scalar_index_table( PARAM_dfi_qnr , idomain )
   END IF
   dfi_scalar_stream_table( idomain, P_dfi_qnr ) = 2113929216
   dfi_scalar_dname_table( idomain, P_dfi_qnr ) = 'DFI_QNRAIN'
   dfi_scalar_desc_table( idomain, P_dfi_qnr ) = 'Rain Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qnr ) = '  kg(-1)'
   F_dfi_qnr = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==10)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qv ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qc ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qr ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qi , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qi = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qi , idomain ) = P_dfi_qi
   ELSE
     P_dfi_qi = dfi_moist_index_table( PARAM_dfi_qi , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qi ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qi ) = 'QICE'
   dfi_moist_desc_table( idomain, P_dfi_qi ) = 'Ice mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qi ) = 'kg kg-1'
   F_dfi_qi = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qs ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   F_dfi_qs = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qg , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qg = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qg , idomain ) = P_dfi_qg
   ELSE
     P_dfi_qg = dfi_moist_index_table( PARAM_dfi_qg , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qg ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qg ) = 'QGRAUP'
   dfi_moist_desc_table( idomain, P_dfi_qg ) = 'Graupel mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qg ) = 'kg kg-1'
   F_dfi_qg = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qni , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qni = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qni , idomain ) = P_dfi_qni
   ELSE
     P_dfi_qni = dfi_scalar_index_table( PARAM_dfi_qni , idomain )
   END IF
   dfi_scalar_stream_table( idomain, P_dfi_qni ) = 2113929216
   dfi_scalar_dname_table( idomain, P_dfi_qni ) = 'DFI_QNICE'
   dfi_scalar_desc_table( idomain, P_dfi_qni ) = 'Ice Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qni ) = '  kg-1'
   F_dfi_qni = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qns , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qns = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qns , idomain ) = P_dfi_qns
   ELSE
     P_dfi_qns = dfi_scalar_index_table( PARAM_dfi_qns , idomain )
   END IF
   dfi_scalar_stream_table( idomain, P_dfi_qns ) = 2113929216
   dfi_scalar_dname_table( idomain, P_dfi_qns ) = 'DFI_QNSNOW'
   dfi_scalar_desc_table( idomain, P_dfi_qns ) = 'Snow Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qns ) = '  kg(-1)'
   F_dfi_qns = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnr , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnr = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnr , idomain ) = P_dfi_qnr
   ELSE
     P_dfi_qnr = dfi_scalar_index_table( PARAM_dfi_qnr , idomain )
   END IF
   dfi_scalar_stream_table( idomain, P_dfi_qnr ) = 2113929216
   dfi_scalar_dname_table( idomain, P_dfi_qnr ) = 'DFI_QNRAIN'
   dfi_scalar_desc_table( idomain, P_dfi_qnr ) = 'Rain Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qnr ) = '  kg(-1)'
   F_dfi_qnr = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qng , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qng = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qng , idomain ) = P_dfi_qng
   ELSE
     P_dfi_qng = dfi_scalar_index_table( PARAM_dfi_qng , idomain )
   END IF
   dfi_scalar_stream_table( idomain, P_dfi_qng ) = 2113929216
   dfi_scalar_dname_table( idomain, P_dfi_qng ) = 'DFI_QNGRAUPEL'
   dfi_scalar_desc_table( idomain, P_dfi_qng ) = 'Graupel Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qng ) = '  kg(-1)'
   F_dfi_qng = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==14)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qv ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qc ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qr ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qi , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qi = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qi , idomain ) = P_dfi_qi
   ELSE
     P_dfi_qi = dfi_moist_index_table( PARAM_dfi_qi , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qi ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qi ) = 'QICE'
   dfi_moist_desc_table( idomain, P_dfi_qi ) = 'Ice mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qi ) = 'kg kg-1'
   F_dfi_qi = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qs ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   F_dfi_qs = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnn , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnn = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnn , idomain ) = P_dfi_qnn
   ELSE
     P_dfi_qnn = dfi_scalar_index_table( PARAM_dfi_qnn , idomain )
   END IF
   dfi_scalar_stream_table( idomain, P_dfi_qnn ) = 2113929216
   dfi_scalar_dname_table( idomain, P_dfi_qnn ) = 'DFI_QNCC'
   dfi_scalar_desc_table( idomain, P_dfi_qnn ) = 'CNN Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qnn ) = '  kg(-1)'
   F_dfi_qnn = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnc , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnc = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnc , idomain ) = P_dfi_qnc
   ELSE
     P_dfi_qnc = dfi_scalar_index_table( PARAM_dfi_qnc , idomain )
   END IF
   dfi_scalar_stream_table( idomain, P_dfi_qnc ) = 2113929216
   dfi_scalar_dname_table( idomain, P_dfi_qnc ) = 'DFI_QNCLOUD'
   dfi_scalar_desc_table( idomain, P_dfi_qnc ) = 'Cloud Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qnc ) = '  kg(-1)'
   F_dfi_qnc = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnr , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnr = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnr , idomain ) = P_dfi_qnr
   ELSE
     P_dfi_qnr = dfi_scalar_index_table( PARAM_dfi_qnr , idomain )
   END IF
   dfi_scalar_stream_table( idomain, P_dfi_qnr ) = 2113929216
   dfi_scalar_dname_table( idomain, P_dfi_qnr ) = 'DFI_QNRAIN'
   dfi_scalar_desc_table( idomain, P_dfi_qnr ) = 'Rain Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qnr ) = '  kg(-1)'
   F_dfi_qnr = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==16)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qv ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qc ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qr ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qi , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qi = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qi , idomain ) = P_dfi_qi
   ELSE
     P_dfi_qi = dfi_moist_index_table( PARAM_dfi_qi , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qi ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qi ) = 'QICE'
   dfi_moist_desc_table( idomain, P_dfi_qi ) = 'Ice mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qi ) = 'kg kg-1'
   F_dfi_qi = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qs ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   F_dfi_qs = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qg , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qg = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qg , idomain ) = P_dfi_qg
   ELSE
     P_dfi_qg = dfi_moist_index_table( PARAM_dfi_qg , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qg ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qg ) = 'QGRAUP'
   dfi_moist_desc_table( idomain, P_dfi_qg ) = 'Graupel mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qg ) = 'kg kg-1'
   F_dfi_qg = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnn , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnn = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnn , idomain ) = P_dfi_qnn
   ELSE
     P_dfi_qnn = dfi_scalar_index_table( PARAM_dfi_qnn , idomain )
   END IF
   dfi_scalar_stream_table( idomain, P_dfi_qnn ) = 2113929216
   dfi_scalar_dname_table( idomain, P_dfi_qnn ) = 'DFI_QNCC'
   dfi_scalar_desc_table( idomain, P_dfi_qnn ) = 'CNN Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qnn ) = '  kg(-1)'
   F_dfi_qnn = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnc , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnc = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnc , idomain ) = P_dfi_qnc
   ELSE
     P_dfi_qnc = dfi_scalar_index_table( PARAM_dfi_qnc , idomain )
   END IF
   dfi_scalar_stream_table( idomain, P_dfi_qnc ) = 2113929216
   dfi_scalar_dname_table( idomain, P_dfi_qnc ) = 'DFI_QNCLOUD'
   dfi_scalar_desc_table( idomain, P_dfi_qnc ) = 'Cloud Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qnc ) = '  kg(-1)'
   F_dfi_qnc = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnr , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnr = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnr , idomain ) = P_dfi_qnr
   ELSE
     P_dfi_qnr = dfi_scalar_index_table( PARAM_dfi_qnr , idomain )
   END IF
   dfi_scalar_stream_table( idomain, P_dfi_qnr ) = 2113929216
   dfi_scalar_dname_table( idomain, P_dfi_qnr ) = 'DFI_QNRAIN'
   dfi_scalar_desc_table( idomain, P_dfi_qnr ) = 'Rain Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qnr ) = '  kg(-1)'
   F_dfi_qnr = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==98)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qv ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qc ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qr ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qi , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qi = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qi , idomain ) = P_dfi_qi
   ELSE
     P_dfi_qi = dfi_moist_index_table( PARAM_dfi_qi , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qi ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qi ) = 'QICE'
   dfi_moist_desc_table( idomain, P_dfi_qi ) = 'Ice mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qi ) = 'kg kg-1'
   F_dfi_qi = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qs ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   F_dfi_qs = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qg , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qg = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qg , idomain ) = P_dfi_qg
   ELSE
     P_dfi_qg = dfi_moist_index_table( PARAM_dfi_qg , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qg ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qg ) = 'QGRAUP'
   dfi_moist_desc_table( idomain, P_dfi_qg ) = 'Graupel mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qg ) = 'kg kg-1'
   F_dfi_qg = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qni , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qni = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qni , idomain ) = P_dfi_qni
   ELSE
     P_dfi_qni = dfi_scalar_index_table( PARAM_dfi_qni , idomain )
   END IF
   dfi_scalar_stream_table( idomain, P_dfi_qni ) = 2113929216
   dfi_scalar_dname_table( idomain, P_dfi_qni ) = 'DFI_QNICE'
   dfi_scalar_desc_table( idomain, P_dfi_qni ) = 'Ice Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qni ) = '  kg-1'
   F_dfi_qni = .TRUE.
  END IF
  IF (model_config_rec%ra_lw_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%ra_lw_physics(idomain)==3)THEN
  END IF
  IF (model_config_rec%ra_lw_physics(idomain)==4)THEN
  END IF
  IF (model_config_rec%ra_lw_physics(idomain)==99)THEN
  END IF
  IF (model_config_rec%ra_lw_physics(idomain)==31)THEN
  END IF
  IF (model_config_rec%ra_sw_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%ra_sw_physics(idomain)==2)THEN
  END IF
  IF (model_config_rec%ra_sw_physics(idomain)==3)THEN
  END IF
  IF (model_config_rec%ra_sw_physics(idomain)==4)THEN
  END IF
  IF (model_config_rec%ra_sw_physics(idomain)==99)THEN
  END IF
  IF (model_config_rec%sf_sfclay_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%sf_sfclay_physics(idomain)==2)THEN
  END IF
  IF (model_config_rec%sf_sfclay_physics(idomain)==3)THEN
  END IF
  IF (model_config_rec%sf_sfclay_physics(idomain)==88)THEN
  END IF
  IF (model_config_rec%sf_sfclay_physics(idomain)==4)THEN
  END IF
  IF (model_config_rec%sf_sfclay_physics(idomain)==7)THEN
  END IF
  IF (model_config_rec%sf_surface_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%sf_surface_physics(idomain)==2)THEN
  END IF
  IF (model_config_rec%sf_surface_physics(idomain)==3)THEN
  END IF
  IF (model_config_rec%sf_surface_physics(idomain)==88)THEN
  END IF
  IF (model_config_rec%sf_surface_physics(idomain)==7)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==2)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==3)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==4)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==7)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==8)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==99)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==2)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==3)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==4)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==5)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==99)THEN
  END IF
  IF (model_config_rec%io_form_restart==1)THEN
  END IF
  IF (model_config_rec%io_form_restart==2)THEN
  END IF
  IF (model_config_rec%io_form_restart==3)THEN
  END IF
  IF (model_config_rec%io_form_restart==4)THEN
  END IF
  IF (model_config_rec%io_form_restart==5)THEN
  END IF
  IF (model_config_rec%io_form_restart==6)THEN
  END IF
  IF (model_config_rec%io_form_restart==7)THEN
  END IF
  IF (model_config_rec%io_form_restart==8)THEN
  END IF
  IF (model_config_rec%io_form_restart==9)THEN
  END IF
  IF (model_config_rec%io_form_restart==10)THEN
  END IF
  IF (model_config_rec%io_form_restart==11)THEN
  END IF
  IF (model_config_rec%dfi_opt==0)THEN
  END IF
  IF (model_config_rec%dfi_opt==1)THEN
  END IF
  IF (model_config_rec%dfi_opt==2)THEN
  END IF
  IF (model_config_rec%dfi_opt==3)THEN
  END IF
  IF (model_config_rec%dfi_stage==0)THEN
  END IF
  IF (model_config_rec%dfi_stage==1)THEN
  END IF
  IF (model_config_rec%dfi_stage==2)THEN
  END IF
  IF (model_config_rec%dfi_stage==3)THEN
  END IF







  num_moist = moist_num_table( idomain )
  num_dfi_moist = dfi_moist_num_table( idomain )
  num_scalar = scalar_num_table( idomain )
  num_dfi_scalar = dfi_scalar_num_table( idomain )
  num_chem = chem_num_table( idomain )

  RETURN
END SUBROUTINE set_scalar_indices_from_config

