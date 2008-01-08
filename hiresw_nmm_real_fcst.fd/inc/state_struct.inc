!STARTOFREGISTRYGENERATEDINCLUDE 'inc/state_struct.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
integer                                  :: nmm_ntsd
integer                                  :: nmm_nstart_hour
real                                     :: nmm_dy_nmm
real                                     :: nmm_cpgfv
real                                     :: nmm_en
real                                     :: nmm_ent
real                                     :: nmm_f4d
real                                     :: nmm_f4q
real                                     :: nmm_ef4t
logical                                  :: nmm_upstrm
real                                     :: nmm_dlmd
real                                     :: nmm_dphd
real                                     :: nmm_pdtop
real                                     :: nmm_pt
logical                                  :: nmm_micro_start
logical                                  :: nmm_hydro
integer                                  :: nmm_nphs0
integer                                  :: nmm_nprec
integer                                  :: nmm_nclod
integer                                  :: nmm_nheat
integer                                  :: nmm_nrdlw
integer                                  :: nmm_nrdsw
integer                                  :: nmm_nsrfc
real                                     :: nmm_avrain
real                                     :: nmm_avcnvc
real                                     :: nmm_aratim
real                                     :: nmm_acutim
real                                     :: nmm_ardlw
real                                     :: nmm_ardsw
real                                     :: nmm_asrfc
real                                     :: nmm_aphtim
integer                                  :: imicrogram
real                                     :: dtbc
integer                                  :: landuse_isice
integer                                  :: landuse_lucats
integer                                  :: landuse_luseas
integer                                  :: landuse_isn
integer                                  :: number_at_same_level
integer                                  :: itimestep
real                                     :: xtime
real                                     :: julian
integer                                  :: lbc_fid
logical                                  :: tiled
logical                                  :: patched
integer                                  :: run_days
integer                                  :: run_hours
integer                                  :: run_minutes
integer                                  :: run_seconds
integer                                  :: start_year
integer                                  :: start_month
integer                                  :: start_day
integer                                  :: start_hour
integer                                  :: start_minute
integer                                  :: start_second
integer                                  :: end_year
integer                                  :: end_month
integer                                  :: end_day
integer                                  :: end_hour
integer                                  :: end_minute
integer                                  :: end_second
integer                                  :: interval_seconds
logical                                  :: input_from_file
integer                                  :: fine_input_stream
integer                                  :: oid
integer                                  :: auxhist1_oid
integer                                  :: auxhist2_oid
integer                                  :: auxhist3_oid
integer                                  :: auxhist4_oid
integer                                  :: auxhist5_oid
integer                                  :: auxhist6_oid
integer                                  :: auxhist7_oid
integer                                  :: auxhist8_oid
integer                                  :: auxhist9_oid
integer                                  :: auxhist10_oid
integer                                  :: auxhist11_oid
integer                                  :: auxinput1_oid
integer                                  :: auxinput2_oid
integer                                  :: auxinput3_oid
integer                                  :: auxinput4_oid
integer                                  :: auxinput5_oid
integer                                  :: auxinput6_oid
integer                                  :: auxinput7_oid
integer                                  :: auxinput8_oid
integer                                  :: auxinput9_oid
integer                                  :: auxinput10_oid
integer                                  :: auxinput11_oid
integer                                  :: history_interval
integer                                  :: frames_per_outfile
integer                                  :: frames_per_auxhist1
integer                                  :: frames_per_auxhist2
integer                                  :: frames_per_auxhist3
integer                                  :: frames_per_auxhist4
integer                                  :: frames_per_auxhist5
integer                                  :: frames_per_auxhist6
integer                                  :: frames_per_auxhist7
integer                                  :: frames_per_auxhist8
integer                                  :: frames_per_auxhist9
integer                                  :: frames_per_auxhist10
integer                                  :: frames_per_auxhist11
logical                                  :: restart
integer                                  :: restart_interval
integer                                  :: io_form_input
integer                                  :: io_form_history
integer                                  :: io_form_restart
integer                                  :: io_form_boundary
integer                                  :: debug_level
logical                                  :: self_test_domain
character*256                               :: history_outname
character*256                               :: auxhist1_outname
character*256                               :: auxhist2_outname
character*256                               :: auxhist3_outname
character*256                               :: auxhist4_outname
character*256                               :: auxhist5_outname
character*256                               :: auxhist6_outname
character*256                               :: auxhist7_outname
character*256                               :: auxhist8_outname
character*256                               :: auxhist9_outname
character*256                               :: auxhist10_outname
character*256                               :: auxhist11_outname
character*256                               :: history_inname
character*256                               :: auxhist1_inname
character*256                               :: auxhist2_inname
character*256                               :: auxhist3_inname
character*256                               :: auxhist4_inname
character*256                               :: auxhist5_inname
character*256                               :: auxhist6_inname
character*256                               :: auxhist7_inname
character*256                               :: auxhist8_inname
character*256                               :: auxhist9_inname
character*256                               :: auxhist10_inname
character*256                               :: auxhist11_inname
character*256                               :: auxinput1_outname
character*256                               :: auxinput2_outname
character*256                               :: auxinput3_outname
character*256                               :: auxinput4_outname
character*256                               :: auxinput5_outname
character*256                               :: auxinput6_outname
character*256                               :: auxinput7_outname
character*256                               :: auxinput8_outname
character*256                               :: auxinput9_outname
character*256                               :: auxinput10_outname
character*256                               :: auxinput11_outname
character*256                               :: auxinput1_inname
character*256                               :: auxinput2_inname
character*256                               :: auxinput3_inname
character*256                               :: auxinput4_inname
character*256                               :: auxinput5_inname
character*256                               :: auxinput6_inname
character*256                               :: auxinput7_inname
character*256                               :: auxinput8_inname
character*256                               :: auxinput9_inname
character*256                               :: gfdda_inname
character*256                               :: auxinput11_inname
integer                                  :: history_interval_mo
integer                                  :: history_interval_d
integer                                  :: history_interval_h
integer                                  :: history_interval_m
integer                                  :: history_interval_s
integer                                  :: inputout_interval_mo
integer                                  :: inputout_interval_d
integer                                  :: inputout_interval_h
integer                                  :: inputout_interval_m
integer                                  :: inputout_interval_s
integer                                  :: inputout_interval
integer                                  :: auxhist1_interval_mo
integer                                  :: auxhist1_interval_d
integer                                  :: auxhist1_interval_h
integer                                  :: auxhist1_interval_m
integer                                  :: auxhist1_interval_s
integer                                  :: auxhist1_interval
integer                                  :: auxhist2_interval_mo
integer                                  :: auxhist2_interval_d
integer                                  :: auxhist2_interval_h
integer                                  :: auxhist2_interval_m
integer                                  :: auxhist2_interval_s
integer                                  :: auxhist2_interval
integer                                  :: auxhist3_interval_mo
integer                                  :: auxhist3_interval_d
integer                                  :: auxhist3_interval_h
integer                                  :: auxhist3_interval_m
integer                                  :: auxhist3_interval_s
integer                                  :: auxhist3_interval
integer                                  :: auxhist4_interval_mo
integer                                  :: auxhist4_interval_d
integer                                  :: auxhist4_interval_h
integer                                  :: auxhist4_interval_m
integer                                  :: auxhist4_interval_s
integer                                  :: auxhist4_interval
integer                                  :: auxhist5_interval_mo
integer                                  :: auxhist5_interval_d
integer                                  :: auxhist5_interval_h
integer                                  :: auxhist5_interval_m
integer                                  :: auxhist5_interval_s
integer                                  :: auxhist5_interval
integer                                  :: auxhist6_interval_mo
integer                                  :: auxhist6_interval_d
integer                                  :: auxhist6_interval_h
integer                                  :: auxhist6_interval_m
integer                                  :: auxhist6_interval_s
integer                                  :: auxhist6_interval
integer                                  :: auxhist7_interval_mo
integer                                  :: auxhist7_interval_d
integer                                  :: auxhist7_interval_h
integer                                  :: auxhist7_interval_m
integer                                  :: auxhist7_interval_s
integer                                  :: auxhist7_interval
integer                                  :: auxhist8_interval_mo
integer                                  :: auxhist8_interval_d
integer                                  :: auxhist8_interval_h
integer                                  :: auxhist8_interval_m
integer                                  :: auxhist8_interval_s
integer                                  :: auxhist8_interval
integer                                  :: auxhist9_interval_mo
integer                                  :: auxhist9_interval_d
integer                                  :: auxhist9_interval_h
integer                                  :: auxhist9_interval_m
integer                                  :: auxhist9_interval_s
integer                                  :: auxhist9_interval
integer                                  :: auxhist10_interval_mo
integer                                  :: auxhist10_interval_d
integer                                  :: auxhist10_interval_h
integer                                  :: auxhist10_interval_m
integer                                  :: auxhist10_interval_s
integer                                  :: auxhist10_interval
integer                                  :: auxhist11_interval_mo
integer                                  :: auxhist11_interval_d
integer                                  :: auxhist11_interval_h
integer                                  :: auxhist11_interval_m
integer                                  :: auxhist11_interval_s
integer                                  :: auxhist11_interval
integer                                  :: auxinput1_interval_mo
integer                                  :: auxinput1_interval_d
integer                                  :: auxinput1_interval_h
integer                                  :: auxinput1_interval_m
integer                                  :: auxinput1_interval_s
integer                                  :: auxinput1_interval
integer                                  :: auxinput2_interval_mo
integer                                  :: auxinput2_interval_d
integer                                  :: auxinput2_interval_h
integer                                  :: auxinput2_interval_m
integer                                  :: auxinput2_interval_s
integer                                  :: auxinput2_interval
integer                                  :: auxinput3_interval_mo
integer                                  :: auxinput3_interval_d
integer                                  :: auxinput3_interval_h
integer                                  :: auxinput3_interval_m
integer                                  :: auxinput3_interval_s
integer                                  :: auxinput3_interval
integer                                  :: auxinput4_interval_mo
integer                                  :: auxinput4_interval_d
integer                                  :: auxinput4_interval_h
integer                                  :: auxinput4_interval_m
integer                                  :: auxinput4_interval_s
integer                                  :: auxinput4_interval
integer                                  :: auxinput5_interval_mo
integer                                  :: auxinput5_interval_d
integer                                  :: auxinput5_interval_h
integer                                  :: auxinput5_interval_m
integer                                  :: auxinput5_interval_s
integer                                  :: auxinput5_interval
integer                                  :: auxinput6_interval_mo
integer                                  :: auxinput6_interval_d
integer                                  :: auxinput6_interval_h
integer                                  :: auxinput6_interval_m
integer                                  :: auxinput6_interval_s
integer                                  :: auxinput6_interval
integer                                  :: auxinput7_interval_mo
integer                                  :: auxinput7_interval_d
integer                                  :: auxinput7_interval_h
integer                                  :: auxinput7_interval_m
integer                                  :: auxinput7_interval_s
integer                                  :: auxinput7_interval
integer                                  :: auxinput8_interval_mo
integer                                  :: auxinput8_interval_d
integer                                  :: auxinput8_interval_h
integer                                  :: auxinput8_interval_m
integer                                  :: auxinput8_interval_s
integer                                  :: auxinput8_interval
integer                                  :: auxinput9_interval_mo
integer                                  :: auxinput9_interval_d
integer                                  :: auxinput9_interval_h
integer                                  :: auxinput9_interval_m
integer                                  :: auxinput9_interval_s
integer                                  :: auxinput9_interval
integer                                  :: gfdda_interval_mo
integer                                  :: gfdda_interval_d
integer                                  :: gfdda_interval_h
integer                                  :: gfdda_interval_m
integer                                  :: gfdda_interval_s
integer                                  :: gfdda_interval
integer                                  :: auxinput11_interval_mo
integer                                  :: auxinput11_interval_d
integer                                  :: auxinput11_interval_h
integer                                  :: auxinput11_interval_m
integer                                  :: auxinput11_interval_s
integer                                  :: auxinput11_interval
integer                                  :: restart_interval_mo
integer                                  :: restart_interval_d
integer                                  :: restart_interval_h
integer                                  :: restart_interval_m
integer                                  :: restart_interval_s
integer                                  :: history_begin_y
integer                                  :: history_begin_mo
integer                                  :: history_begin_d
integer                                  :: history_begin_h
integer                                  :: history_begin_m
integer                                  :: history_begin_s
integer                                  :: inputout_begin_y
integer                                  :: inputout_begin_mo
integer                                  :: inputout_begin_d
integer                                  :: inputout_begin_h
integer                                  :: inputout_begin_m
integer                                  :: inputout_begin_s
integer                                  :: auxhist1_begin_y
integer                                  :: auxhist1_begin_mo
integer                                  :: auxhist1_begin_d
integer                                  :: auxhist1_begin_h
integer                                  :: auxhist1_begin_m
integer                                  :: auxhist1_begin_s
integer                                  :: auxhist2_begin_y
integer                                  :: auxhist2_begin_mo
integer                                  :: auxhist2_begin_d
integer                                  :: auxhist2_begin_h
integer                                  :: auxhist2_begin_m
integer                                  :: auxhist2_begin_s
integer                                  :: auxhist3_begin_y
integer                                  :: auxhist3_begin_mo
integer                                  :: auxhist3_begin_d
integer                                  :: auxhist3_begin_h
integer                                  :: auxhist3_begin_m
integer                                  :: auxhist3_begin_s
integer                                  :: auxhist4_begin_y
integer                                  :: auxhist4_begin_mo
integer                                  :: auxhist4_begin_d
integer                                  :: auxhist4_begin_h
integer                                  :: auxhist4_begin_m
integer                                  :: auxhist4_begin_s
integer                                  :: auxhist5_begin_y
integer                                  :: auxhist5_begin_mo
integer                                  :: auxhist5_begin_d
integer                                  :: auxhist5_begin_h
integer                                  :: auxhist5_begin_m
integer                                  :: auxhist5_begin_s
integer                                  :: auxhist6_begin_y
integer                                  :: auxhist6_begin_mo
integer                                  :: auxhist6_begin_d
integer                                  :: auxhist6_begin_h
integer                                  :: auxhist6_begin_m
integer                                  :: auxhist6_begin_s
integer                                  :: auxhist7_begin_y
integer                                  :: auxhist7_begin_mo
integer                                  :: auxhist7_begin_d
integer                                  :: auxhist7_begin_h
integer                                  :: auxhist7_begin_m
integer                                  :: auxhist7_begin_s
integer                                  :: auxhist8_begin_y
integer                                  :: auxhist8_begin_mo
integer                                  :: auxhist8_begin_d
integer                                  :: auxhist8_begin_h
integer                                  :: auxhist8_begin_m
integer                                  :: auxhist8_begin_s
integer                                  :: auxhist9_begin_y
integer                                  :: auxhist9_begin_mo
integer                                  :: auxhist9_begin_d
integer                                  :: auxhist9_begin_h
integer                                  :: auxhist9_begin_m
integer                                  :: auxhist9_begin_s
integer                                  :: auxhist10_begin_y
integer                                  :: auxhist10_begin_mo
integer                                  :: auxhist10_begin_d
integer                                  :: auxhist10_begin_h
integer                                  :: auxhist10_begin_m
integer                                  :: auxhist10_begin_s
integer                                  :: auxhist11_begin_y
integer                                  :: auxhist11_begin_mo
integer                                  :: auxhist11_begin_d
integer                                  :: auxhist11_begin_h
integer                                  :: auxhist11_begin_m
integer                                  :: auxhist11_begin_s
integer                                  :: auxinput1_begin_y
integer                                  :: auxinput1_begin_mo
integer                                  :: auxinput1_begin_d
integer                                  :: auxinput1_begin_h
integer                                  :: auxinput1_begin_m
integer                                  :: auxinput1_begin_s
integer                                  :: auxinput2_begin_y
integer                                  :: auxinput2_begin_mo
integer                                  :: auxinput2_begin_d
integer                                  :: auxinput2_begin_h
integer                                  :: auxinput2_begin_m
integer                                  :: auxinput2_begin_s
integer                                  :: auxinput3_begin_y
integer                                  :: auxinput3_begin_mo
integer                                  :: auxinput3_begin_d
integer                                  :: auxinput3_begin_h
integer                                  :: auxinput3_begin_m
integer                                  :: auxinput3_begin_s
integer                                  :: auxinput4_begin_y
integer                                  :: auxinput4_begin_mo
integer                                  :: auxinput4_begin_d
integer                                  :: auxinput4_begin_h
integer                                  :: auxinput4_begin_m
integer                                  :: auxinput4_begin_s
integer                                  :: auxinput5_begin_y
integer                                  :: auxinput5_begin_mo
integer                                  :: auxinput5_begin_d
integer                                  :: auxinput5_begin_h
integer                                  :: auxinput5_begin_m
integer                                  :: auxinput5_begin_s
integer                                  :: auxinput6_begin_y
integer                                  :: auxinput6_begin_mo
integer                                  :: auxinput6_begin_d
integer                                  :: auxinput6_begin_h
integer                                  :: auxinput6_begin_m
integer                                  :: auxinput6_begin_s
integer                                  :: auxinput7_begin_y
integer                                  :: auxinput7_begin_mo
integer                                  :: auxinput7_begin_d
integer                                  :: auxinput7_begin_h
integer                                  :: auxinput7_begin_m
integer                                  :: auxinput7_begin_s
integer                                  :: auxinput8_begin_y
integer                                  :: auxinput8_begin_mo
integer                                  :: auxinput8_begin_d
integer                                  :: auxinput8_begin_h
integer                                  :: auxinput8_begin_m
integer                                  :: auxinput8_begin_s
integer                                  :: auxinput9_begin_y
integer                                  :: auxinput9_begin_mo
integer                                  :: auxinput9_begin_d
integer                                  :: auxinput9_begin_h
integer                                  :: auxinput9_begin_m
integer                                  :: auxinput9_begin_s
integer                                  :: gfdda_begin_y
integer                                  :: gfdda_begin_mo
integer                                  :: gfdda_begin_d
integer                                  :: gfdda_begin_h
integer                                  :: gfdda_begin_m
integer                                  :: gfdda_begin_s
integer                                  :: auxinput11_begin_y
integer                                  :: auxinput11_begin_mo
integer                                  :: auxinput11_begin_d
integer                                  :: auxinput11_begin_h
integer                                  :: auxinput11_begin_m
integer                                  :: auxinput11_begin_s
integer                                  :: restart_begin_y
integer                                  :: restart_begin_mo
integer                                  :: restart_begin_d
integer                                  :: restart_begin_h
integer                                  :: restart_begin_m
integer                                  :: restart_begin_s
integer                                  :: history_end_y
integer                                  :: history_end_mo
integer                                  :: history_end_d
integer                                  :: history_end_h
integer                                  :: history_end_m
integer                                  :: history_end_s
integer                                  :: inputout_end_y
integer                                  :: inputout_end_mo
integer                                  :: inputout_end_d
integer                                  :: inputout_end_h
integer                                  :: inputout_end_m
integer                                  :: inputout_end_s
integer                                  :: auxhist1_end_y
integer                                  :: auxhist1_end_mo
integer                                  :: auxhist1_end_d
integer                                  :: auxhist1_end_h
integer                                  :: auxhist1_end_m
integer                                  :: auxhist1_end_s
integer                                  :: auxhist2_end_y
integer                                  :: auxhist2_end_mo
integer                                  :: auxhist2_end_d
integer                                  :: auxhist2_end_h
integer                                  :: auxhist2_end_m
integer                                  :: auxhist2_end_s
integer                                  :: auxhist3_end_y
integer                                  :: auxhist3_end_mo
integer                                  :: auxhist3_end_d
integer                                  :: auxhist3_end_h
integer                                  :: auxhist3_end_m
integer                                  :: auxhist3_end_s
integer                                  :: auxhist4_end_y
integer                                  :: auxhist4_end_mo
integer                                  :: auxhist4_end_d
integer                                  :: auxhist4_end_h
integer                                  :: auxhist4_end_m
integer                                  :: auxhist4_end_s
integer                                  :: auxhist5_end_y
integer                                  :: auxhist5_end_mo
integer                                  :: auxhist5_end_d
integer                                  :: auxhist5_end_h
integer                                  :: auxhist5_end_m
integer                                  :: auxhist5_end_s
integer                                  :: auxhist6_end_y
integer                                  :: auxhist6_end_mo
integer                                  :: auxhist6_end_d
integer                                  :: auxhist6_end_h
integer                                  :: auxhist6_end_m
integer                                  :: auxhist6_end_s
integer                                  :: auxhist7_end_y
integer                                  :: auxhist7_end_mo
integer                                  :: auxhist7_end_d
integer                                  :: auxhist7_end_h
integer                                  :: auxhist7_end_m
integer                                  :: auxhist7_end_s
integer                                  :: auxhist8_end_y
integer                                  :: auxhist8_end_mo
integer                                  :: auxhist8_end_d
integer                                  :: auxhist8_end_h
integer                                  :: auxhist8_end_m
integer                                  :: auxhist8_end_s
integer                                  :: auxhist9_end_y
integer                                  :: auxhist9_end_mo
integer                                  :: auxhist9_end_d
integer                                  :: auxhist9_end_h
integer                                  :: auxhist9_end_m
integer                                  :: auxhist9_end_s
integer                                  :: auxhist10_end_y
integer                                  :: auxhist10_end_mo
integer                                  :: auxhist10_end_d
integer                                  :: auxhist10_end_h
integer                                  :: auxhist10_end_m
integer                                  :: auxhist10_end_s
integer                                  :: auxhist11_end_y
integer                                  :: auxhist11_end_mo
integer                                  :: auxhist11_end_d
integer                                  :: auxhist11_end_h
integer                                  :: auxhist11_end_m
integer                                  :: auxhist11_end_s
integer                                  :: auxinput1_end_y
integer                                  :: auxinput1_end_mo
integer                                  :: auxinput1_end_d
integer                                  :: auxinput1_end_h
integer                                  :: auxinput1_end_m
integer                                  :: auxinput1_end_s
integer                                  :: auxinput2_end_y
integer                                  :: auxinput2_end_mo
integer                                  :: auxinput2_end_d
integer                                  :: auxinput2_end_h
integer                                  :: auxinput2_end_m
integer                                  :: auxinput2_end_s
integer                                  :: auxinput3_end_y
integer                                  :: auxinput3_end_mo
integer                                  :: auxinput3_end_d
integer                                  :: auxinput3_end_h
integer                                  :: auxinput3_end_m
integer                                  :: auxinput3_end_s
integer                                  :: auxinput4_end_y
integer                                  :: auxinput4_end_mo
integer                                  :: auxinput4_end_d
integer                                  :: auxinput4_end_h
integer                                  :: auxinput4_end_m
integer                                  :: auxinput4_end_s
integer                                  :: auxinput5_end_y
integer                                  :: auxinput5_end_mo
integer                                  :: auxinput5_end_d
integer                                  :: auxinput5_end_h
integer                                  :: auxinput5_end_m
integer                                  :: auxinput5_end_s
integer                                  :: auxinput6_end_y
integer                                  :: auxinput6_end_mo
integer                                  :: auxinput6_end_d
integer                                  :: auxinput6_end_h
integer                                  :: auxinput6_end_m
integer                                  :: auxinput6_end_s
integer                                  :: auxinput7_end_y
integer                                  :: auxinput7_end_mo
integer                                  :: auxinput7_end_d
integer                                  :: auxinput7_end_h
integer                                  :: auxinput7_end_m
integer                                  :: auxinput7_end_s
integer                                  :: auxinput8_end_y
integer                                  :: auxinput8_end_mo
integer                                  :: auxinput8_end_d
integer                                  :: auxinput8_end_h
integer                                  :: auxinput8_end_m
integer                                  :: auxinput8_end_s
integer                                  :: auxinput9_end_y
integer                                  :: auxinput9_end_mo
integer                                  :: auxinput9_end_d
integer                                  :: auxinput9_end_h
integer                                  :: auxinput9_end_m
integer                                  :: auxinput9_end_s
integer                                  :: gfdda_end_y
integer                                  :: gfdda_end_mo
integer                                  :: gfdda_end_d
integer                                  :: gfdda_end_h
integer                                  :: gfdda_end_m
integer                                  :: gfdda_end_s
integer                                  :: auxinput11_end_y
integer                                  :: auxinput11_end_mo
integer                                  :: auxinput11_end_d
integer                                  :: auxinput11_end_h
integer                                  :: auxinput11_end_m
integer                                  :: auxinput11_end_s
integer                                  :: io_form_auxinput1
integer                                  :: io_form_auxinput2
integer                                  :: io_form_auxinput3
integer                                  :: io_form_auxinput4
integer                                  :: io_form_auxinput5
integer                                  :: io_form_auxinput6
integer                                  :: io_form_auxinput7
integer                                  :: io_form_auxinput8
integer                                  :: io_form_auxinput9
integer                                  :: io_form_gfdda
integer                                  :: io_form_auxinput11
integer                                  :: io_form_auxhist1
integer                                  :: io_form_auxhist2
integer                                  :: io_form_auxhist3
integer                                  :: io_form_auxhist4
integer                                  :: io_form_auxhist5
integer                                  :: io_form_auxhist6
integer                                  :: io_form_auxhist7
integer                                  :: io_form_auxhist8
integer                                  :: io_form_auxhist9
integer                                  :: io_form_auxhist10
integer                                  :: io_form_auxhist11
integer                                  :: julyr
integer                                  :: julday
real                                     :: gmt
character*256                               :: input_inname
character*256                               :: input_outname
character*256                               :: bdy_inname
character*256                               :: bdy_outname
character*256                               :: rst_inname
character*256                               :: rst_outname
logical                                  :: write_input
logical                                  :: write_restart_at_0h
logical                                  :: adjust_output_times
logical                                  :: adjust_input_times
real                                     :: tstart
logical                                  :: nocolons
integer                                  :: time_step
integer                                  :: time_step_fract_num
integer                                  :: time_step_fract_den
integer                                  :: max_dom
integer                                  :: s_we
integer                                  :: e_we
integer                                  :: s_sn
integer                                  :: e_sn
integer                                  :: s_vert
integer                                  :: e_vert
real                                     :: dx
real                                     :: dy
integer                                  :: grid_id
integer                                  :: parent_id
integer                                  :: i_parent_start
integer                                  :: j_parent_start
integer                                  :: parent_grid_ratio
integer                                  :: parent_time_step_ratio
integer                                  :: feedback
integer                                  :: smooth_option
real                                     :: ztop
integer                                  :: moad_grid_ratio
integer                                  :: moad_time_step_ratio
integer                                  :: shw
integer                                  :: tile_sz_x
integer                                  :: tile_sz_y
integer                                  :: numtiles
integer                                  :: nproc_x
integer                                  :: nproc_y
integer                                  :: irand
real                                     :: dt
integer                                  :: num_moves
integer                                  :: move_id
integer                                  :: move_interval
integer                                  :: move_cd_x
integer                                  :: move_cd_y
logical                                  :: swap_x
logical                                  :: swap_y
logical                                  :: cycle_x
logical                                  :: cycle_y
logical                                  :: reorder_mesh
logical                                  :: perturb_input
real                                     :: eta_levels
real                                     :: ptsgm
integer                                  :: num_metgrid_levels
real                                     :: p_top_requested
integer                                  :: mp_physics
integer                                  :: ra_lw_physics
integer                                  :: ra_sw_physics
real                                     :: radt
integer                                  :: sf_sfclay_physics
integer                                  :: sf_surface_physics
integer                                  :: bl_pbl_physics
real                                     :: bldt
integer                                  :: cu_physics
real                                     :: cudt
real                                     :: gsmdt
integer                                  :: isfflx
integer                                  :: ifsnow
integer                                  :: icloud
real                                     :: swrad_scat
integer                                  :: surface_input_source
integer                                  :: num_soil_layers
integer                                  :: maxiens
integer                                  :: maxens
integer                                  :: maxens2
integer                                  :: maxens3
integer                                  :: ensdim
integer                                  :: chem_opt
integer                                  :: num_land_cat
integer                                  :: num_soil_cat
integer                                  :: mp_zero_out
real                                     :: mp_zero_out_thresh
real                                     :: seaice_threshold
integer                                  :: sst_update
integer                                  :: ucmcall
integer                                  :: idtad
integer                                  :: nsoil
integer                                  :: nphs
integer                                  :: ncnvc
integer                                  :: nrads
integer                                  :: nradl
real                                     :: tprec
real                                     :: theat
real                                     :: tclod
real                                     :: trdsw
real                                     :: trdlw
real                                     :: tsrfc
logical                                  :: pcpflg
integer                                  :: sigma
integer                                  :: co2tf
integer                                  :: ra_call_offset
real                                     :: cam_abs_freq_s
integer                                  :: levsiz
integer                                  :: paerlev
integer                                  :: cam_abs_dim1
integer                                  :: cam_abs_dim2
logical                                  :: cu_rad_feedback
integer                                  :: dyn_opt
integer                                  :: rk_ord
integer                                  :: w_damping
integer                                  :: diff_opt
integer                                  :: km_opt
integer                                  :: damp_opt
real                                     :: zdamp
real                                     :: base_pres
real                                     :: base_temp
real                                     :: base_lapse
real                                     :: dampcoef
real                                     :: khdif
real                                     :: kvdif
real                                     :: smdiv
real                                     :: emdiv
real                                     :: epssm
logical                                  :: non_hydrostatic
integer                                  :: time_step_sound
integer                                  :: h_mom_adv_order
integer                                  :: v_mom_adv_order
integer                                  :: h_sca_adv_order
integer                                  :: v_sca_adv_order
logical                                  :: top_radiation
real                                     :: mix_cr_len
real                                     :: tke_upper_bound
real                                     :: kh_tke_upper_bound
real                                     :: kv_tke_upper_bound
real                                     :: tke_drag_coefficient
real                                     :: tke_heat_flux
logical                                  :: pert_coriolis
integer                                  :: spec_bdy_width
integer                                  :: spec_zone
integer                                  :: relax_zone
logical                                  :: specified
logical                                  :: periodic_x
logical                                  :: symmetric_xs
logical                                  :: symmetric_xe
logical                                  :: open_xs
logical                                  :: open_xe
logical                                  :: periodic_y
logical                                  :: symmetric_ys
logical                                  :: symmetric_ye
logical                                  :: open_ys
logical                                  :: open_ye
logical                                  :: nested
integer                                  :: real_data_init_type
integer                                  :: background_proc_id
integer                                  :: forecast_proc_id
integer                                  :: production_status
integer                                  :: compression
real                                     :: cen_lat
real                                     :: cen_lon
real                                     :: truelat1
real                                     :: truelat2
real                                     :: moad_cen_lat
real                                     :: stand_lon
real                                     :: bdyfrq
integer                                  :: iswater
integer                                  :: isice
integer                                  :: isurban
integer                                  :: isoilwater
integer                                  :: map_proj
integer                                  :: simulation_start_year
integer                                  :: simulation_start_month
integer                                  :: simulation_start_day
integer                                  :: simulation_start_hour
integer                                  :: simulation_start_minute
integer                                  :: simulation_start_second
integer                                  :: nodyn_nodyn_dummy
real      ,DIMENSION(:,:,:)   ,POINTER   :: exp_x_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: exp_x_2
real      ,DIMENSION(:,:)     ,POINTER   :: lu_index
real      ,DIMENSION(:,:)     ,POINTER   :: lu_mask
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_p_gc
real      ,DIMENSION(:,:)     ,POINTER   :: vegcat
real      ,DIMENSION(:,:)     ,POINTER   :: soilcat
real      ,DIMENSION(:,:)     ,POINTER   :: input_soil_cat
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_tsk_gc
real      ,DIMENSION(:,:)     ,POINTER   :: xice_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_ght_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_rh_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_v_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_u_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_t_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_rwmr_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_snmr_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_clwmr_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_cice_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_rimef_gc
real      ,DIMENSION(:,:)     ,POINTER   :: snoalb
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_greenfrac_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_albedo12m_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: soilcbot_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: soilctop_gc
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_tmn_gc
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_htv_gc
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_ht_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: landusef_gc
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_vlon_gc
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_vlat_gc
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_hlon_gc
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_hlat_gc
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_hbm2
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_hbm3
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_vbm2
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_vbm3
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_sm
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_sice
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_pd
real      ,DIMENSION(:,:,:,:) ,POINTER   :: nmm_pd_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: nmm_pd_bt
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_fis
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_res
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_t
real      ,DIMENSION(:,:,:,:) ,POINTER   :: nmm_t_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: nmm_t_bt
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_q
real      ,DIMENSION(:,:,:,:) ,POINTER   :: nmm_q_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: nmm_q_bt
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_u
real      ,DIMENSION(:,:,:,:) ,POINTER   :: nmm_u_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: nmm_u_bt
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_v
real      ,DIMENSION(:,:,:,:) ,POINTER   :: nmm_v_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: nmm_v_bt
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_told
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_uold
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_vold
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_dx_nmm
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_wpdar
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_cpgfu
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_curv
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_fcp
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_fdiv
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_f
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_fad
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_ddmpu
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_ddmpv
real      ,DIMENSION(:)       ,POINTER   :: nmm_deta
real      ,DIMENSION(:)       ,POINTER   :: nmm_rdeta
real      ,DIMENSION(:)       ,POINTER   :: nmm_aeta
real      ,DIMENSION(:)       ,POINTER   :: nmm_f4q2
real      ,DIMENSION(:)       ,POINTER   :: nmm_etax
real      ,DIMENSION(:)       ,POINTER   :: nmm_dfl
real      ,DIMENSION(:)       ,POINTER   :: nmm_deta1
real      ,DIMENSION(:)       ,POINTER   :: nmm_aeta1
real      ,DIMENSION(:)       ,POINTER   :: nmm_eta1
real      ,DIMENSION(:)       ,POINTER   :: nmm_deta2
real      ,DIMENSION(:)       ,POINTER   :: nmm_aeta2
real      ,DIMENSION(:)       ,POINTER   :: nmm_eta2
real      ,DIMENSION(:)       ,POINTER   :: nmm_em
real      ,DIMENSION(:)       ,POINTER   :: nmm_emt
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_adt
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_adu
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_adv
real      ,DIMENSION(:)       ,POINTER   :: nmm_em_loc
real      ,DIMENSION(:)       ,POINTER   :: nmm_emt_loc
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_pdsl
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_pdslo
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_psdt
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_div
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_few
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_fne
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_fns
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_fse
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_omgalf
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_petdt
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_rtop
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_pblh
integer   ,DIMENSION(:,:)     ,POINTER   :: nmm_lpbl
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_ustar
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_z0
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_z0base
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_ths
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_mavail
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_qsh
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_twbs
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_qwbs
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_prec
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_aprec
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_acprec
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_cuprec
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_lspa
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_ddata
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_accliq
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_sno
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_si
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_cldefi
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_deep
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_rf
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_th10
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_q10
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_pshltr
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_tshltr
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_qshltr
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_q2
real      ,DIMENSION(:,:,:,:) ,POINTER   :: nmm_q2_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: nmm_q2_bt
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_t_adj
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_t_old
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_zero_3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_w0avg
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_akhs_out
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_akms_out
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_albase
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_albedo
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_cnvbot
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_cnvtop
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_czen
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_czmean
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_epsr
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_gffc
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_glat
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_glon
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_nmm_tsk
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_hdac
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_hdacv
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_mxsnal
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_radin
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_radot
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_sigt4
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_tg
real      ,DIMENSION(:)       ,POINTER   :: nmm_dfrlg
integer   ,DIMENSION(:,:)     ,POINTER   :: nmm_lvl
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_cwm
real      ,DIMENSION(:,:,:,:) ,POINTER   :: nmm_cwm_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: nmm_cwm_bt
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_f_ice
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_f_rain
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_f_rimef
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_cldfra
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_sr
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_cfrach
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_cfracl
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_cfracm
integer   ,DIMENSION(:,:)     ,POINTER   :: nmm_islope
real      ,DIMENSION(:)       ,POINTER   :: nmm_dzsoil
real      ,DIMENSION(:)       ,POINTER   :: nmm_rtdpth
real      ,DIMENSION(:)       ,POINTER   :: nmm_sldpth
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_cmc
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_grnflx
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_pctsno
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_soiltb
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_vegfrc
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_shdmin
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_shdmax
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_sh2o
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_smc
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_stc
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_dwdtmn
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_dwdtmx
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_dwdt
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_pdwdt
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_pint
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_w
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_z
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_acfrcv
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_acfrst
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_ssroff
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_bgroff
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_rlwin
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_rlwout
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_rlwtoa
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_alwin
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_alwout
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_alwtoa
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_rswin
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_rswinc
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_rswout
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_rswtoa
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_aswin
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_aswout
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_aswtoa
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_sfcshx
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_sfclhx
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_subshx
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_snopcx
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_sfcuvx
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_potevp
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_potflx
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_tlmin
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_tlmax
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_rlwtt
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_rswtt
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_tcucn
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_train
integer   ,DIMENSION(:,:)     ,POINTER   :: nmm_ncfrcv
integer   ,DIMENSION(:,:)     ,POINTER   :: nmm_ncfrst
integer   ,DIMENSION(:)       ,POINTER   :: nmm_ihe
integer   ,DIMENSION(:)       ,POINTER   :: nmm_ihw
integer   ,DIMENSION(:)       ,POINTER   :: nmm_ive
integer   ,DIMENSION(:)       ,POINTER   :: nmm_ivw
integer   ,DIMENSION(:)       ,POINTER   :: nmm_irad
integer   ,DIMENSION(:)       ,POINTER   :: nmm_iheg
integer   ,DIMENSION(:)       ,POINTER   :: nmm_ihwg
integer   ,DIMENSION(:)       ,POINTER   :: nmm_iveg
integer   ,DIMENSION(:)       ,POINTER   :: nmm_ivwg
integer   ,DIMENSION(:)       ,POINTER   :: nmm_iradg
integer   ,DIMENSION(:)       ,POINTER   :: nmm_n_iup_h
integer   ,DIMENSION(:)       ,POINTER   :: nmm_n_iup_v
integer   ,DIMENSION(:)       ,POINTER   :: nmm_n_iup_adh
integer   ,DIMENSION(:)       ,POINTER   :: nmm_n_iup_adv
integer   ,DIMENSION(:,:)     ,POINTER   :: nmm_iup_h
integer   ,DIMENSION(:,:)     ,POINTER   :: nmm_iup_v
integer   ,DIMENSION(:,:)     ,POINTER   :: nmm_iup_adh
integer   ,DIMENSION(:,:)     ,POINTER   :: nmm_iup_adv
integer   ,DIMENSION(:,:)     ,POINTER   :: imask_nostag
integer   ,DIMENSION(:,:)     ,POINTER   :: imask_xstag
integer   ,DIMENSION(:,:)     ,POINTER   :: imask_ystag
integer   ,DIMENSION(:,:)     ,POINTER   :: imask_xystag
real      ,DIMENSION(:,:)     ,POINTER   :: sm000007
real      ,DIMENSION(:,:)     ,POINTER   :: sm007028
real      ,DIMENSION(:,:)     ,POINTER   :: sm028100
real      ,DIMENSION(:,:)     ,POINTER   :: sm100255
real      ,DIMENSION(:,:)     ,POINTER   :: st000007
real      ,DIMENSION(:,:)     ,POINTER   :: st007028
real      ,DIMENSION(:,:)     ,POINTER   :: st028100
real      ,DIMENSION(:,:)     ,POINTER   :: st100255
real      ,DIMENSION(:,:)     ,POINTER   :: sm000010
real      ,DIMENSION(:,:)     ,POINTER   :: sm010040
real      ,DIMENSION(:,:)     ,POINTER   :: sm040100
real      ,DIMENSION(:,:)     ,POINTER   :: sm100200
real      ,DIMENSION(:,:)     ,POINTER   :: sm010200
real      ,DIMENSION(:,:)     ,POINTER   :: soilm000
real      ,DIMENSION(:,:)     ,POINTER   :: soilm005
real      ,DIMENSION(:,:)     ,POINTER   :: soilm020
real      ,DIMENSION(:,:)     ,POINTER   :: soilm040
real      ,DIMENSION(:,:)     ,POINTER   :: soilm160
real      ,DIMENSION(:,:)     ,POINTER   :: soilm300
real      ,DIMENSION(:,:)     ,POINTER   :: sw000010
real      ,DIMENSION(:,:)     ,POINTER   :: sw010040
real      ,DIMENSION(:,:)     ,POINTER   :: sw040100
real      ,DIMENSION(:,:)     ,POINTER   :: sw100200
real      ,DIMENSION(:,:)     ,POINTER   :: sw010200
real      ,DIMENSION(:,:)     ,POINTER   :: soilw000
real      ,DIMENSION(:,:)     ,POINTER   :: soilw005
real      ,DIMENSION(:,:)     ,POINTER   :: soilw020
real      ,DIMENSION(:,:)     ,POINTER   :: soilw040
real      ,DIMENSION(:,:)     ,POINTER   :: soilw160
real      ,DIMENSION(:,:)     ,POINTER   :: soilw300
real      ,DIMENSION(:,:)     ,POINTER   :: st000010
real      ,DIMENSION(:,:)     ,POINTER   :: st010040
real      ,DIMENSION(:,:)     ,POINTER   :: st040100
real      ,DIMENSION(:,:)     ,POINTER   :: st100200
real      ,DIMENSION(:,:)     ,POINTER   :: st010200
real      ,DIMENSION(:,:)     ,POINTER   :: soilt000
real      ,DIMENSION(:,:)     ,POINTER   :: soilt005
real      ,DIMENSION(:,:)     ,POINTER   :: soilt020
real      ,DIMENSION(:,:)     ,POINTER   :: soilt040
real      ,DIMENSION(:,:)     ,POINTER   :: soilt160
real      ,DIMENSION(:,:)     ,POINTER   :: soilt300
real      ,DIMENSION(:,:)     ,POINTER   :: landmask
real      ,DIMENSION(:,:)     ,POINTER   :: topostdv
real      ,DIMENSION(:,:)     ,POINTER   :: toposlpx
real      ,DIMENSION(:,:)     ,POINTER   :: toposlpy
real      ,DIMENSION(:,:)     ,POINTER   :: greenmax
real      ,DIMENSION(:,:)     ,POINTER   :: greenmin
real      ,DIMENSION(:,:)     ,POINTER   :: albedomx
real      ,DIMENSION(:,:)     ,POINTER   :: slopecat
real      ,DIMENSION(:,:)     ,POINTER   :: toposoil
real      ,DIMENSION(:,:,:)   ,POINTER   :: landusef
real      ,DIMENSION(:,:,:)   ,POINTER   :: soilctop
real      ,DIMENSION(:,:,:)   ,POINTER   :: soilcbot
real      ,DIMENSION(:,:,:,:) ,POINTER   :: moist
real      ,DIMENSION(:,:,:,:) ,POINTER   :: scalar
real      ,DIMENSION(:,:,:,:,:),POINTER   :: scalar_b
real      ,DIMENSION(:,:,:,:,:),POINTER   :: scalar_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: chem
real      ,DIMENSION(:,:,:)   ,POINTER   :: smois
real      ,DIMENSION(:,:,:)   ,POINTER   :: tslb
real      ,DIMENSION(:,:)     ,POINTER   :: gsw
real      ,DIMENSION(:,:)     ,POINTER   :: xlat
real      ,DIMENSION(:,:)     ,POINTER   :: xlong
real      ,DIMENSION(:,:)     ,POINTER   :: xland
real      ,DIMENSION(:,:)     ,POINTER   :: raincv
real      ,DIMENSION(:,:)     ,POINTER   :: psfc
real      ,DIMENSION(:,:)     ,POINTER   :: th2
real      ,DIMENSION(:,:)     ,POINTER   :: t2
real      ,DIMENSION(:,:)     ,POINTER   :: u10
real      ,DIMENSION(:,:)     ,POINTER   :: v10
real      ,DIMENSION(:,:)     ,POINTER   :: xice
real      ,DIMENSION(:,:)     ,POINTER   :: smstav
real      ,DIMENSION(:,:)     ,POINTER   :: smstot
real      ,DIMENSION(:,:)     ,POINTER   :: sfcrunoff
real      ,DIMENSION(:,:)     ,POINTER   :: udrunoff
integer   ,DIMENSION(:,:)     ,POINTER   :: ivgtyp
integer   ,DIMENSION(:,:)     ,POINTER   :: isltyp
real      ,DIMENSION(:,:)     ,POINTER   :: vegfra
real      ,DIMENSION(:,:)     ,POINTER   :: sfcevp
real      ,DIMENSION(:,:)     ,POINTER   :: grdflx
real      ,DIMENSION(:,:)     ,POINTER   :: albbck
real      ,DIMENSION(:,:)     ,POINTER   :: sfcexc
real      ,DIMENSION(:,:)     ,POINTER   :: acsnow
real      ,DIMENSION(:,:)     ,POINTER   :: acsnom
real      ,DIMENSION(:,:)     ,POINTER   :: rmol
real      ,DIMENSION(:,:)     ,POINTER   :: snow
real      ,DIMENSION(:,:)     ,POINTER   :: canwat
real      ,DIMENSION(:,:)     ,POINTER   :: sst
real      ,DIMENSION(:,:)     ,POINTER   :: weasd
real      ,DIMENSION(:,:)     ,POINTER   :: znt
real      ,DIMENSION(:,:)     ,POINTER   :: mol
real      ,DIMENSION(:,:,:)   ,POINTER   :: tke_myj
real      ,DIMENSION(:,:,:)   ,POINTER   :: el_myj
real      ,DIMENSION(:,:,:)   ,POINTER   :: exch_h
real      ,DIMENSION(:,:)     ,POINTER   :: thz0
real      ,DIMENSION(:,:)     ,POINTER   :: qz0
real      ,DIMENSION(:,:)     ,POINTER   :: uz0
real      ,DIMENSION(:,:)     ,POINTER   :: vz0
real      ,DIMENSION(:,:)     ,POINTER   :: flhc
real      ,DIMENSION(:,:)     ,POINTER   :: flqc
real      ,DIMENSION(:,:)     ,POINTER   :: qsg
real      ,DIMENSION(:,:)     ,POINTER   :: qvg
real      ,DIMENSION(:,:)     ,POINTER   :: qcg
real      ,DIMENSION(:,:)     ,POINTER   :: soilt1
real      ,DIMENSION(:,:)     ,POINTER   :: tsnav
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_psfc_out
real      ,DIMENSION(:,:)     ,POINTER   :: uz0h
real      ,DIMENSION(:,:)     ,POINTER   :: vz0h
real      ,DIMENSION(:,:,:)   ,POINTER   :: dudt
real      ,DIMENSION(:,:,:)   ,POINTER   :: dvdt
real      ,DIMENSION(:,:)     ,POINTER   :: qsfc
real      ,DIMENSION(:,:)     ,POINTER   :: akhs
real      ,DIMENSION(:,:)     ,POINTER   :: akms
real      ,DIMENSION(:,:)     ,POINTER   :: htop
real      ,DIMENSION(:,:)     ,POINTER   :: hbot
real      ,DIMENSION(:,:)     ,POINTER   :: htopr
real      ,DIMENSION(:,:)     ,POINTER   :: hbotr
real      ,DIMENSION(:,:)     ,POINTER   :: htopd
real      ,DIMENSION(:,:)     ,POINTER   :: hbotd
real      ,DIMENSION(:,:)     ,POINTER   :: htops
real      ,DIMENSION(:,:)     ,POINTER   :: hbots
real      ,DIMENSION(:,:)     ,POINTER   :: cuppt
real      ,DIMENSION(:,:)     ,POINTER   :: cprate
real      ,DIMENSION(:,:,:)   ,POINTER   :: f_ice_phy
real      ,DIMENSION(:,:,:)   ,POINTER   :: f_rain_phy
real      ,DIMENSION(:,:,:)   ,POINTER   :: f_rimef_phy
real      ,DIMENSION(:,:)     ,POINTER   :: mass_flux
real      ,DIMENSION(:,:)     ,POINTER   :: apr_gr
real      ,DIMENSION(:,:)     ,POINTER   :: apr_w
real      ,DIMENSION(:,:)     ,POINTER   :: apr_mc
real      ,DIMENSION(:,:)     ,POINTER   :: apr_st
real      ,DIMENSION(:,:)     ,POINTER   :: apr_as
real      ,DIMENSION(:,:)     ,POINTER   :: apr_capma
real      ,DIMENSION(:,:)     ,POINTER   :: apr_capme
real      ,DIMENSION(:,:)     ,POINTER   :: apr_capmi
real      ,DIMENSION(:,:,:)   ,POINTER   :: xf_ens
real      ,DIMENSION(:,:,:)   ,POINTER   :: pr_ens
real      ,DIMENSION(:,:,:)   ,POINTER   :: rthften
real      ,DIMENSION(:,:,:)   ,POINTER   :: rqvften
real      ,DIMENSION(:,:)     ,POINTER   :: snowh
real      ,DIMENSION(:,:)     ,POINTER   :: rhosn
real      ,DIMENSION(:,:,:)   ,POINTER   :: smfr3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: keepfr3dflag
real      ,DIMENSION(:)       ,POINTER   :: mp_restart_state
real      ,DIMENSION(:)       ,POINTER   :: tbpvs_state
real      ,DIMENSION(:)       ,POINTER   :: tbpvs0_state
real      ,DIMENSION(:)       ,POINTER   :: lu_state
!ENDOFREGISTRYGENERATEDINCLUDE
