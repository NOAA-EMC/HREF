MODULE module_domain_type

   USE module_driver_constants
   USE module_utility

   CHARACTER (LEN=80) program_name

   
   
   
   
   

   TYPE domain_ptr
      TYPE(domain), POINTER :: ptr
   END TYPE domain_ptr

   INTEGER, PARAMETER :: HISTORY_ALARM=1, AUXHIST1_ALARM=2, AUXHIST2_ALARM=3,     &
                         AUXHIST3_ALARM=4, AUXHIST4_ALARM=5, AUXHIST5_ALARM=6,    &
                         AUXHIST6_ALARM=7, AUXHIST7_ALARM=8, AUXHIST8_ALARM=9,    &
                         AUXHIST9_ALARM=10, AUXHIST10_ALARM=11, AUXHIST11_ALARM=12,    &
                         AUXINPUT1_ALARM=13, AUXINPUT2_ALARM=14, AUXINPUT3_ALARM=15, &
                         AUXINPUT4_ALARM=16, AUXINPUT5_ALARM=17,                  &
                         AUXINPUT6_ALARM=18, AUXINPUT7_ALARM=19, AUXINPUT8_ALARM=20, &
                         AUXINPUT9_ALARM=21, AUXINPUT10_ALARM=22, AUXINPUT11_ALARM=23, &
                         RESTART_ALARM=24, BOUNDARY_ALARM=25, INPUTOUT_ALARM=26,  &  
                         ALARM_SUBTIME=27,                                        &
                         COMPUTE_VORTEX_CENTER_ALARM=28,                          &
                         MAX_WRF_ALARMS=28  
                                            
                                            









   TYPE domain








logical                                  :: moved
integer                                  :: ntsd
integer                                  :: nstart_hour
real                                     :: hcoeff_tot
real                                     :: dy_nmm
real                                     :: cpgfv
real                                     :: en
real                                     :: ent
real                                     :: f4d
real                                     :: f4q
real                                     :: ef4t
logical                                  :: upstrm
real                                     :: dlmd
real                                     :: dphd
real                                     :: pdtop
real                                     :: pt
logical                                  :: micro_start
logical                                  :: hydro
integer                                  :: nphs0
integer                                  :: nprec
integer                                  :: nclod
integer                                  :: nheat
integer                                  :: nrdlw
integer                                  :: nrdsw
integer                                  :: nsrfc
real                                     :: avrain
real                                     :: avcnvc
real                                     :: aratim
real                                     :: acutim
real                                     :: ardlw
real                                     :: ardsw
real                                     :: asrfc
real                                     :: aphtim
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
character*256                               :: sgfdda_inname
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
integer                                  :: sgfdda_interval_mo
integer                                  :: sgfdda_interval_d
integer                                  :: sgfdda_interval_h
integer                                  :: sgfdda_interval_m
integer                                  :: sgfdda_interval_s
integer                                  :: sgfdda_interval
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
integer                                  :: sgfdda_begin_y
integer                                  :: sgfdda_begin_mo
integer                                  :: sgfdda_begin_d
integer                                  :: sgfdda_begin_h
integer                                  :: sgfdda_begin_m
integer                                  :: sgfdda_begin_s
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
integer                                  :: sgfdda_end_y
integer                                  :: sgfdda_end_mo
integer                                  :: sgfdda_end_d
integer                                  :: sgfdda_end_h
integer                                  :: sgfdda_end_m
integer                                  :: sgfdda_end_s
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
integer                                  :: io_form_sgfdda
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
integer                                  :: simulation_start_year
integer                                  :: simulation_start_month
integer                                  :: simulation_start_day
integer                                  :: simulation_start_hour
integer                                  :: simulation_start_minute
integer                                  :: simulation_start_second
logical                                  :: reset_simulation_start
integer                                  :: sr_x
integer                                  :: sr_y
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
logical                                  :: cycling
integer                                  :: dfi_opt
integer                                  :: dfi_nfilter
logical                                  :: dfi_write_filtered_input
logical                                  :: dfi_write_dfi_history
integer                                  :: dfi_cutoff_seconds
integer                                  :: dfi_time_dim
integer                                  :: dfi_fwdstop_year
integer                                  :: dfi_fwdstop_month
integer                                  :: dfi_fwdstop_day
integer                                  :: dfi_fwdstop_hour
integer                                  :: dfi_fwdstop_minute
integer                                  :: dfi_fwdstop_second
integer                                  :: dfi_bckstop_year
integer                                  :: dfi_bckstop_month
integer                                  :: dfi_bckstop_day
integer                                  :: dfi_bckstop_hour
integer                                  :: dfi_bckstop_minute
integer                                  :: dfi_bckstop_second
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
logical                                  :: grid_allowed
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
integer                                  :: ts_buf_size
integer                                  :: max_ts_locs
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
integer                                  :: sf_urban_physics
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
integer                                  :: num_urban_layers
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
integer                                  :: fractional_seaice
integer                                  :: sst_update
logical                                  :: usemonalb
logical                                  :: rdmaxalb
logical                                  :: rdlai2d
integer                                  :: gwd_opt
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
real                                     :: sfenth
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
real                                     :: iso_temp
real                                     :: dampcoef
real                                     :: khdif
real                                     :: kvdif
real                                     :: c_s
real                                     :: c_k
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
real                                     :: tke_upper_bound
real                                     :: tke_drag_coefficient
real                                     :: tke_heat_flux
logical                                  :: pert_coriolis
logical                                  :: euler_adv
integer                                  :: idtadt
integer                                  :: idtadc
logical                                  :: boundary_flux
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
logical                                  :: polar
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
integer                                  :: flag_metgrid
integer                                  :: flag_snow
integer                                  :: flag_psfc
integer                                  :: flag_sm000010
integer                                  :: flag_sm010040
integer                                  :: flag_sm040100
integer                                  :: flag_sm100200
integer                                  :: flag_st000010
integer                                  :: flag_st010040
integer                                  :: flag_st040100
integer                                  :: flag_st100200
integer                                  :: flag_slp
integer                                  :: flag_soilhgt
integer                                  :: flag_mf_xy
real                                     :: bdyfrq
character*256                               :: mminlu
integer                                  :: iswater
integer                                  :: islake
integer                                  :: isice
integer                                  :: isurban
integer                                  :: isoilwater
integer                                  :: map_proj
integer                                  :: dfi_stage
integer                                  :: mp_physics_dfi
integer                                  :: nodyn_dummy
real      ,DIMENSION(:,:,:)   ,POINTER   :: x_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: x_2
real      ,DIMENSION(:,:)     ,POINTER   :: lu_index
real      ,DIMENSION(:,:)     ,POINTER   :: lu_mask
real      ,DIMENSION(:,:,:)   ,POINTER   :: p_gc
real      ,DIMENSION(:,:)     ,POINTER   :: vegcat
real      ,DIMENSION(:,:)     ,POINTER   :: soilcat
real      ,DIMENSION(:,:)     ,POINTER   :: input_soil_cat
real      ,DIMENSION(:,:)     ,POINTER   :: tsk_gc
real      ,DIMENSION(:,:)     ,POINTER   :: xice_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: ght_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: rh_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: v_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: u_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_gc
real      ,DIMENSION(:,:)     ,POINTER   :: snoalb
real      ,DIMENSION(:,:,:)   ,POINTER   :: greenfrac_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: albedo12m_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: soilcbot_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: soilctop_gc
real      ,DIMENSION(:,:)     ,POINTER   :: tmn_gc
real      ,DIMENSION(:,:)     ,POINTER   :: htv_gc
real      ,DIMENSION(:,:)     ,POINTER   :: ht_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: landusef_gc
real      ,DIMENSION(:,:)     ,POINTER   :: vlon_gc
real      ,DIMENSION(:,:)     ,POINTER   :: vlat_gc
real      ,DIMENSION(:,:)     ,POINTER   :: hlon_gc
real      ,DIMENSION(:,:)     ,POINTER   :: hlat_gc
real      ,DIMENSION(:,:)     ,POINTER   :: hbm2
real      ,DIMENSION(:,:)     ,POINTER   :: hbm3
real      ,DIMENSION(:,:)     ,POINTER   :: vbm2
real      ,DIMENSION(:,:)     ,POINTER   :: vbm3
real      ,DIMENSION(:,:)     ,POINTER   :: sm
real      ,DIMENSION(:,:)     ,POINTER   :: sice
real      ,DIMENSION(:,:)     ,POINTER   :: pd
real      ,DIMENSION(:,:,:)   ,POINTER   :: pd_bxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: pd_bxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: pd_bys
real      ,DIMENSION(:,:,:)   ,POINTER   :: pd_bye
real      ,DIMENSION(:,:,:)   ,POINTER   :: pd_btxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: pd_btxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: pd_btys
real      ,DIMENSION(:,:,:)   ,POINTER   :: pd_btye
real      ,DIMENSION(:,:)     ,POINTER   :: fis
real      ,DIMENSION(:,:)     ,POINTER   :: res
real      ,DIMENSION(:,:,:)   ,POINTER   :: t
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_bxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_bxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_bys
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_bye
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_btxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_btxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_btys
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_btye
real      ,DIMENSION(:,:,:)   ,POINTER   :: q
real      ,DIMENSION(:,:,:)   ,POINTER   :: q_bxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: q_bxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: q_bys
real      ,DIMENSION(:,:,:)   ,POINTER   :: q_bye
real      ,DIMENSION(:,:,:)   ,POINTER   :: q_btxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: q_btxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: q_btys
real      ,DIMENSION(:,:,:)   ,POINTER   :: q_btye
real      ,DIMENSION(:,:,:)   ,POINTER   :: u
real      ,DIMENSION(:,:,:)   ,POINTER   :: u_bxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: u_bxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: u_bys
real      ,DIMENSION(:,:,:)   ,POINTER   :: u_bye
real      ,DIMENSION(:,:,:)   ,POINTER   :: u_btxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: u_btxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: u_btys
real      ,DIMENSION(:,:,:)   ,POINTER   :: u_btye
real      ,DIMENSION(:,:,:)   ,POINTER   :: v
real      ,DIMENSION(:,:,:)   ,POINTER   :: v_bxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: v_bxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: v_bys
real      ,DIMENSION(:,:,:)   ,POINTER   :: v_bye
real      ,DIMENSION(:,:,:)   ,POINTER   :: v_btxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: v_btxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: v_btys
real      ,DIMENSION(:,:,:)   ,POINTER   :: v_btye
real      ,DIMENSION(:,:,:)   ,POINTER   :: told
real      ,DIMENSION(:,:,:)   ,POINTER   :: uold
real      ,DIMENSION(:,:,:)   ,POINTER   :: vold
real      ,DIMENSION(:)       ,POINTER   :: hcoeff
real      ,DIMENSION(:,:)     ,POINTER   :: dfi_pd
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_pint
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_dwdt
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_t
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_q
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_u
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_v
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_q2
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_cwm
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_rrw
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_stc
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_smc
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_sh2o
real      ,DIMENSION(:,:)     ,POINTER   :: dfi_snow
real      ,DIMENSION(:,:)     ,POINTER   :: dfi_snowh
real      ,DIMENSION(:,:)     ,POINTER   :: dfi_canwat
real      ,DIMENSION(:,:)     ,POINTER   :: dfi_nmm_tsk
real      ,DIMENSION(:,:)     ,POINTER   :: dfi_snowc
real      ,DIMENSION(:,:)     ,POINTER   :: dx_nmm
real      ,DIMENSION(:,:)     ,POINTER   :: wpdar
real      ,DIMENSION(:,:)     ,POINTER   :: cpgfu
real      ,DIMENSION(:,:)     ,POINTER   :: curv
real      ,DIMENSION(:,:)     ,POINTER   :: fcp
real      ,DIMENSION(:,:)     ,POINTER   :: fdiv
real      ,DIMENSION(:,:)     ,POINTER   :: f
real      ,DIMENSION(:,:)     ,POINTER   :: fad
real      ,DIMENSION(:,:)     ,POINTER   :: ddmpu
real      ,DIMENSION(:,:)     ,POINTER   :: ddmpv
real      ,DIMENSION(:)       ,POINTER   :: deta
real      ,DIMENSION(:)       ,POINTER   :: rdeta
real      ,DIMENSION(:)       ,POINTER   :: aeta
real      ,DIMENSION(:)       ,POINTER   :: f4q2
real      ,DIMENSION(:)       ,POINTER   :: etax
real      ,DIMENSION(:)       ,POINTER   :: dfl
real      ,DIMENSION(:)       ,POINTER   :: deta1
real      ,DIMENSION(:)       ,POINTER   :: aeta1
real      ,DIMENSION(:)       ,POINTER   :: eta1
real      ,DIMENSION(:)       ,POINTER   :: deta2
real      ,DIMENSION(:)       ,POINTER   :: aeta2
real      ,DIMENSION(:)       ,POINTER   :: eta2
real      ,DIMENSION(:)       ,POINTER   :: em
real      ,DIMENSION(:)       ,POINTER   :: emt
real      ,DIMENSION(:,:)     ,POINTER   :: adt
real      ,DIMENSION(:,:)     ,POINTER   :: adu
real      ,DIMENSION(:,:)     ,POINTER   :: adv
real      ,DIMENSION(:)       ,POINTER   :: em_loc
real      ,DIMENSION(:)       ,POINTER   :: emt_loc
real      ,DIMENSION(:,:)     ,POINTER   :: pdsl
real      ,DIMENSION(:,:)     ,POINTER   :: pdslo
real      ,DIMENSION(:,:)     ,POINTER   :: psdt
real      ,DIMENSION(:,:,:)   ,POINTER   :: div
real      ,DIMENSION(:,:,:)   ,POINTER   :: few
real      ,DIMENSION(:,:,:)   ,POINTER   :: fne
real      ,DIMENSION(:,:,:)   ,POINTER   :: fns
real      ,DIMENSION(:,:,:)   ,POINTER   :: fse
real      ,DIMENSION(:,:,:)   ,POINTER   :: omgalf
real      ,DIMENSION(:,:,:)   ,POINTER   :: petdt
real      ,DIMENSION(:,:,:)   ,POINTER   :: rtop
real      ,DIMENSION(:,:)     ,POINTER   :: pblh
integer   ,DIMENSION(:,:)     ,POINTER   :: lpbl
real      ,DIMENSION(:,:)     ,POINTER   :: mixht
real      ,DIMENSION(:,:)     ,POINTER   :: ustar
real      ,DIMENSION(:,:)     ,POINTER   :: z0
real      ,DIMENSION(:,:)     ,POINTER   :: z0base
real      ,DIMENSION(:,:)     ,POINTER   :: ths
real      ,DIMENSION(:,:)     ,POINTER   :: mavail
real      ,DIMENSION(:,:)     ,POINTER   :: qsh
real      ,DIMENSION(:,:)     ,POINTER   :: twbs
real      ,DIMENSION(:,:)     ,POINTER   :: qwbs
real      ,DIMENSION(:,:)     ,POINTER   :: taux
real      ,DIMENSION(:,:)     ,POINTER   :: tauy
real      ,DIMENSION(:,:)     ,POINTER   :: prec
real      ,DIMENSION(:,:)     ,POINTER   :: aprec
real      ,DIMENSION(:,:)     ,POINTER   :: acprec
real      ,DIMENSION(:,:)     ,POINTER   :: cuprec
real      ,DIMENSION(:,:)     ,POINTER   :: lspa
real      ,DIMENSION(:,:)     ,POINTER   :: ddata
real      ,DIMENSION(:,:)     ,POINTER   :: accliq
real      ,DIMENSION(:,:)     ,POINTER   :: sno
real      ,DIMENSION(:,:)     ,POINTER   :: si
real      ,DIMENSION(:,:)     ,POINTER   :: cldefi
real      ,DIMENSION(:,:)     ,POINTER   :: deep
real      ,DIMENSION(:,:)     ,POINTER   :: rf
real      ,DIMENSION(:,:)     ,POINTER   :: th10
real      ,DIMENSION(:,:)     ,POINTER   :: q10
real      ,DIMENSION(:,:)     ,POINTER   :: pshltr
real      ,DIMENSION(:,:)     ,POINTER   :: tshltr
real      ,DIMENSION(:,:)     ,POINTER   :: qshltr
real      ,DIMENSION(:,:,:)   ,POINTER   :: q2
real      ,DIMENSION(:,:,:)   ,POINTER   :: q2_bxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: q2_bxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: q2_bys
real      ,DIMENSION(:,:,:)   ,POINTER   :: q2_bye
real      ,DIMENSION(:,:,:)   ,POINTER   :: q2_btxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: q2_btxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: q2_btys
real      ,DIMENSION(:,:,:)   ,POINTER   :: q2_btye
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_adj
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_old
real      ,DIMENSION(:,:,:)   ,POINTER   :: zero_3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: w0avg
real      ,DIMENSION(:,:)     ,POINTER   :: akhs_out
real      ,DIMENSION(:,:)     ,POINTER   :: akms_out
real      ,DIMENSION(:,:)     ,POINTER   :: albase
real      ,DIMENSION(:,:)     ,POINTER   :: albedo
real      ,DIMENSION(:,:)     ,POINTER   :: cnvbot
real      ,DIMENSION(:,:)     ,POINTER   :: cnvtop
real      ,DIMENSION(:,:)     ,POINTER   :: czen
real      ,DIMENSION(:,:)     ,POINTER   :: czmean
real      ,DIMENSION(:,:)     ,POINTER   :: embck
real      ,DIMENSION(:,:)     ,POINTER   :: epsr
real      ,DIMENSION(:,:)     ,POINTER   :: gffc
real      ,DIMENSION(:,:)     ,POINTER   :: glat
real      ,DIMENSION(:,:)     ,POINTER   :: glon
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_tsk
real      ,DIMENSION(:,:)     ,POINTER   :: hdac
real      ,DIMENSION(:,:)     ,POINTER   :: hdacv
real      ,DIMENSION(:,:)     ,POINTER   :: mxsnal
real      ,DIMENSION(:,:)     ,POINTER   :: radin
real      ,DIMENSION(:,:)     ,POINTER   :: radot
real      ,DIMENSION(:,:)     ,POINTER   :: sigt4
real      ,DIMENSION(:,:)     ,POINTER   :: tg
real      ,DIMENSION(:)       ,POINTER   :: dfrlg
integer   ,DIMENSION(:,:)     ,POINTER   :: lvl
real      ,DIMENSION(:,:,:)   ,POINTER   :: cwm
real      ,DIMENSION(:,:,:)   ,POINTER   :: cwm_bxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: cwm_bxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: cwm_bys
real      ,DIMENSION(:,:,:)   ,POINTER   :: cwm_bye
real      ,DIMENSION(:,:,:)   ,POINTER   :: cwm_btxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: cwm_btxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: cwm_btys
real      ,DIMENSION(:,:,:)   ,POINTER   :: cwm_btye
real      ,DIMENSION(:,:,:)   ,POINTER   :: rrw
real      ,DIMENSION(:,:,:)   ,POINTER   :: rrw_bxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: rrw_bxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: rrw_bys
real      ,DIMENSION(:,:,:)   ,POINTER   :: rrw_bye
real      ,DIMENSION(:,:,:)   ,POINTER   :: rrw_btxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: rrw_btxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: rrw_btys
real      ,DIMENSION(:,:,:)   ,POINTER   :: rrw_btye
real      ,DIMENSION(:,:,:)   ,POINTER   :: f_ice
real      ,DIMENSION(:,:,:)   ,POINTER   :: f_rain
real      ,DIMENSION(:,:,:)   ,POINTER   :: f_rimef
real      ,DIMENSION(:,:,:)   ,POINTER   :: cldfra
real      ,DIMENSION(:,:)     ,POINTER   :: sr
real      ,DIMENSION(:,:)     ,POINTER   :: cfrach
real      ,DIMENSION(:,:)     ,POINTER   :: cfracl
real      ,DIMENSION(:,:)     ,POINTER   :: cfracm
integer   ,DIMENSION(:,:)     ,POINTER   :: islope
real      ,DIMENSION(:)       ,POINTER   :: dzsoil
real      ,DIMENSION(:)       ,POINTER   :: rtdpth
real      ,DIMENSION(:)       ,POINTER   :: sldpth
real      ,DIMENSION(:,:)     ,POINTER   :: cmc
real      ,DIMENSION(:,:)     ,POINTER   :: grnflx
real      ,DIMENSION(:,:)     ,POINTER   :: pctsno
real      ,DIMENSION(:,:)     ,POINTER   :: soiltb
real      ,DIMENSION(:,:)     ,POINTER   :: vegfrc
real      ,DIMENSION(:,:)     ,POINTER   :: shdmin
real      ,DIMENSION(:,:)     ,POINTER   :: shdmax
real      ,DIMENSION(:,:,:)   ,POINTER   :: sh2o
real      ,DIMENSION(:,:,:)   ,POINTER   :: smc
real      ,DIMENSION(:,:,:)   ,POINTER   :: stc
real      ,DIMENSION(:,:)     ,POINTER   :: hstdv
real      ,DIMENSION(:,:)     ,POINTER   :: hcnvx
real      ,DIMENSION(:,:)     ,POINTER   :: hasyw
real      ,DIMENSION(:,:)     ,POINTER   :: hasys
real      ,DIMENSION(:,:)     ,POINTER   :: hasysw
real      ,DIMENSION(:,:)     ,POINTER   :: hasynw
real      ,DIMENSION(:,:)     ,POINTER   :: hlenw
real      ,DIMENSION(:,:)     ,POINTER   :: hlens
real      ,DIMENSION(:,:)     ,POINTER   :: hlensw
real      ,DIMENSION(:,:)     ,POINTER   :: hlennw
real      ,DIMENSION(:,:)     ,POINTER   :: hangl
real      ,DIMENSION(:,:)     ,POINTER   :: hanis
real      ,DIMENSION(:,:)     ,POINTER   :: hslop
real      ,DIMENSION(:,:)     ,POINTER   :: hzmax
real      ,DIMENSION(:,:)     ,POINTER   :: crot
real      ,DIMENSION(:,:)     ,POINTER   :: srot
real      ,DIMENSION(:,:)     ,POINTER   :: ugwdsfc
real      ,DIMENSION(:,:)     ,POINTER   :: vgwdsfc
real      ,DIMENSION(:,:)     ,POINTER   :: dwdtmn
real      ,DIMENSION(:,:)     ,POINTER   :: dwdtmx
real      ,DIMENSION(:,:,:)   ,POINTER   :: dwdt
real      ,DIMENSION(:,:,:)   ,POINTER   :: pdwdt
real      ,DIMENSION(:,:,:)   ,POINTER   :: pint
real      ,DIMENSION(:,:,:)   ,POINTER   :: w
real      ,DIMENSION(:,:,:)   ,POINTER   :: z
real      ,DIMENSION(:,:)     ,POINTER   :: acfrcv
real      ,DIMENSION(:,:)     ,POINTER   :: acfrst
real      ,DIMENSION(:,:)     ,POINTER   :: ssroff
real      ,DIMENSION(:,:)     ,POINTER   :: bgroff
real      ,DIMENSION(:,:)     ,POINTER   :: rlwin
real      ,DIMENSION(:,:)     ,POINTER   :: rlwout
real      ,DIMENSION(:,:)     ,POINTER   :: rlwtoa
real      ,DIMENSION(:,:)     ,POINTER   :: alwin
real      ,DIMENSION(:,:)     ,POINTER   :: alwout
real      ,DIMENSION(:,:)     ,POINTER   :: alwtoa
real      ,DIMENSION(:,:)     ,POINTER   :: rswin
real      ,DIMENSION(:,:)     ,POINTER   :: rswinc
real      ,DIMENSION(:,:)     ,POINTER   :: rswout
real      ,DIMENSION(:,:)     ,POINTER   :: rswtoa
real      ,DIMENSION(:,:)     ,POINTER   :: aswin
real      ,DIMENSION(:,:)     ,POINTER   :: aswout
real      ,DIMENSION(:,:)     ,POINTER   :: aswtoa
real      ,DIMENSION(:,:)     ,POINTER   :: sfcshx
real      ,DIMENSION(:,:)     ,POINTER   :: sfclhx
real      ,DIMENSION(:,:)     ,POINTER   :: subshx
real      ,DIMENSION(:,:)     ,POINTER   :: snopcx
real      ,DIMENSION(:,:)     ,POINTER   :: sfcuvx
real      ,DIMENSION(:,:)     ,POINTER   :: potevp
real      ,DIMENSION(:,:)     ,POINTER   :: potflx
real      ,DIMENSION(:,:)     ,POINTER   :: tlmin
real      ,DIMENSION(:,:)     ,POINTER   :: tlmax
real      ,DIMENSION(:,:)     ,POINTER   :: t02_min
real      ,DIMENSION(:,:)     ,POINTER   :: t02_max
real      ,DIMENSION(:,:)     ,POINTER   :: rh02_min
real      ,DIMENSION(:,:)     ,POINTER   :: rh02_max
real      ,DIMENSION(:,:,:)   ,POINTER   :: rlwtt
real      ,DIMENSION(:,:,:)   ,POINTER   :: rswtt
real      ,DIMENSION(:,:,:)   ,POINTER   :: tcucn
real      ,DIMENSION(:,:,:)   ,POINTER   :: train
integer   ,DIMENSION(:,:)     ,POINTER   :: ncfrcv
integer   ,DIMENSION(:,:)     ,POINTER   :: ncfrst
real      ,DIMENSION(:,:)     ,POINTER   :: max10mw
real      ,DIMENSION(:,:)     ,POINTER   :: max10u
real      ,DIMENSION(:,:)     ,POINTER   :: max10v
real      ,DIMENSION(:,:)     ,POINTER   :: maxupdr
real      ,DIMENSION(:,:)     ,POINTER   :: maxdndr
real      ,DIMENSION(:,:)     ,POINTER   :: maxhlcy
real      ,DIMENSION(:,:)     ,POINTER   :: maxdbz
integer   ,DIMENSION(:)       ,POINTER   :: ihe
integer   ,DIMENSION(:)       ,POINTER   :: ihw
integer   ,DIMENSION(:)       ,POINTER   :: ive
integer   ,DIMENSION(:)       ,POINTER   :: ivw
integer   ,DIMENSION(:)       ,POINTER   :: irad
integer   ,DIMENSION(:)       ,POINTER   :: iheg
integer   ,DIMENSION(:)       ,POINTER   :: ihwg
integer   ,DIMENSION(:)       ,POINTER   :: iveg
integer   ,DIMENSION(:)       ,POINTER   :: ivwg
integer   ,DIMENSION(:)       ,POINTER   :: iradg
integer   ,DIMENSION(:)       ,POINTER   :: n_iup_h
integer   ,DIMENSION(:)       ,POINTER   :: n_iup_v
integer   ,DIMENSION(:)       ,POINTER   :: n_iup_adh
integer   ,DIMENSION(:)       ,POINTER   :: n_iup_adv
integer   ,DIMENSION(:,:)     ,POINTER   :: iup_h
integer   ,DIMENSION(:,:)     ,POINTER   :: iup_v
integer   ,DIMENSION(:,:)     ,POINTER   :: iup_adh
integer   ,DIMENSION(:,:)     ,POINTER   :: iup_adv
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
real      ,DIMENSION(:,:)     ,POINTER   :: ts_hour
real      ,DIMENSION(:,:)     ,POINTER   :: ts_u
real      ,DIMENSION(:,:)     ,POINTER   :: ts_v
real      ,DIMENSION(:,:)     ,POINTER   :: ts_q
real      ,DIMENSION(:,:)     ,POINTER   :: ts_t
real      ,DIMENSION(:,:)     ,POINTER   :: ts_psfc
real      ,DIMENSION(:,:)     ,POINTER   :: ts_tsk
real      ,DIMENSION(:,:)     ,POINTER   :: ts_tslb
real      ,DIMENSION(:,:)     ,POINTER   :: ts_clw
real      ,DIMENSION(:,:,:,:) ,POINTER   :: moist
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_moist
real      ,DIMENSION(:,:,:,:) ,POINTER   :: scalar
real      ,DIMENSION(:,:,:,:) ,POINTER   :: scalar_bxs
real      ,DIMENSION(:,:,:,:) ,POINTER   :: scalar_bxe
real      ,DIMENSION(:,:,:,:) ,POINTER   :: scalar_bys
real      ,DIMENSION(:,:,:,:) ,POINTER   :: scalar_bye
real      ,DIMENSION(:,:,:,:) ,POINTER   :: scalar_btxs
real      ,DIMENSION(:,:,:,:) ,POINTER   :: scalar_btxe
real      ,DIMENSION(:,:,:,:) ,POINTER   :: scalar_btys
real      ,DIMENSION(:,:,:,:) ,POINTER   :: scalar_btye
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_scalar
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_scalar_bxs
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_scalar_bxe
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_scalar_bys
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_scalar_bye
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_scalar_btxs
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_scalar_btxe
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_scalar_btys
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_scalar_btye
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
real      ,DIMENSION(:,:)     ,POINTER   :: lai
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
real      ,DIMENSION(:,:)     ,POINTER   :: snotime
real      ,DIMENSION(:,:)     ,POINTER   :: acsnow
real      ,DIMENSION(:,:)     ,POINTER   :: acsnom
real      ,DIMENSION(:,:)     ,POINTER   :: rmol
real      ,DIMENSION(:,:)     ,POINTER   :: snow
real      ,DIMENSION(:,:)     ,POINTER   :: canwat
real      ,DIMENSION(:,:)     ,POINTER   :: sst
real      ,DIMENSION(:,:)     ,POINTER   :: weasd
real      ,DIMENSION(:,:)     ,POINTER   :: znt
real      ,DIMENSION(:,:)     ,POINTER   :: mol
real      ,DIMENSION(:,:)     ,POINTER   :: noahres
real      ,DIMENSION(:,:,:)   ,POINTER   :: tke_myj
real      ,DIMENSION(:,:,:)   ,POINTER   :: el_myj
real      ,DIMENSION(:,:,:)   ,POINTER   :: exch_h
real      ,DIMENSION(:,:,:)   ,POINTER   :: exch_m
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
real      ,DIMENSION(:,:)     ,POINTER   :: psfc_out
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


      INTEGER                                             :: comms( max_comms ), shift_x, shift_y

      INTEGER                                             :: id
      INTEGER                                             :: domdesc
      INTEGER                                             :: communicator
      INTEGER                                             :: iocommunicator
      INTEGER,POINTER                                     :: mapping(:,:)
      INTEGER,POINTER                                     :: i_start(:),i_end(:)
      INTEGER,POINTER                                     :: j_start(:),j_end(:)
      INTEGER                                             :: max_tiles
      INTEGER                                             :: num_tiles        
      INTEGER                                             :: num_tiles_x      
      INTEGER                                             :: num_tiles_y      
      INTEGER                                             :: num_tiles_spec   
                                                                              

      TYPE(domain_ptr) , DIMENSION( : ) , POINTER         :: parents                            
      TYPE(domain_ptr) , DIMENSION( : ) , POINTER         :: nests                            
      TYPE(domain) , POINTER                              :: sibling 
      TYPE(domain) , POINTER                              :: intermediate_grid
      LOGICAL                                             :: is_intermediate
      INTEGER :: nids, nide, njds, njde  
      INTEGER                                             :: num_parents, num_nests, num_siblings
      INTEGER      , DIMENSION( max_parents )             :: child_of_parent
      INTEGER      , DIMENSION( max_nests )               :: active

      INTEGER      , DIMENSION(0:5)                       :: nframes          
                                                                              

      TYPE(domain) , POINTER                              :: next
      TYPE(domain) , POINTER                              :: same_level

      LOGICAL      , DIMENSION ( 4 )                      :: bdy_mask         

      LOGICAL                                             :: first_force

      

      INTEGER    :: sd31,   ed31,   sd32,   ed32,   sd33,   ed33,         &
                    sd21,   ed21,   sd22,   ed22,                         &
                    sd11,   ed11

      INTEGER    :: sp31,   ep31,   sp32,   ep32,   sp33,   ep33,         &
                    sp21,   ep21,   sp22,   ep22,                         &
                    sp11,   ep11,                                         &
                    sm31,   em31,   sm32,   em32,   sm33,   em33,         &
                    sm21,   em21,   sm22,   em22,                         &
                    sm11,   em11,                                         &
                    sp31x,  ep31x,  sp32x,  ep32x,  sp33x,  ep33x,        &
                    sp21x,  ep21x,  sp22x,  ep22x,                        &
                    sm31x,  em31x,  sm32x,  em32x,  sm33x,  em33x,        &
                    sm21x,  em21x,  sm22x,  em22x,                        &
                    sp31y,  ep31y,  sp32y,  ep32y,  sp33y,  ep33y,        &
                    sp21y,  ep21y,  sp22y,  ep22y,                        &
                    sm31y,  em31y,  sm32y,  em32y,  sm33y,  em33y,        &
                    sm21y,  em21y,  sm22y,  em22y
      Type(WRFU_Clock), POINTER                           :: domain_clock
      Type(WRFU_Time)                                     :: start_subtime, stop_subtime
      Type(WRFU_Time)                                     :: this_bdy_time, next_bdy_time
      Type(WRFU_Time)                                     :: this_emi_time, next_emi_time
      Type(WRFU_TimeInterval), DIMENSION(MAX_WRF_ALARMS)  :: io_intervals
      Type(WRFU_Alarm), POINTER :: alarms(:)




      LOGICAL :: domain_clock_created
      LOGICAL, POINTER :: alarms_created(:)

      
      LOGICAL :: time_set




      REAL :: max_cfl_val
      REAL :: last_max_vert_cfl
      REAL :: max_vert_cfl
      REAL :: max_horiz_cfl
      Type(WRFU_TimeInterval) :: last_dtInterval

      
      INTEGER :: ntsloc, ntsloc_domain
      INTEGER :: next_ts_time
      INTEGER, POINTER, DIMENSION(:) :: itsloc, jtsloc, id_tsloc
      REAL, POINTER, DIMENSION(:) :: lattsloc, lontsloc
      CHARACTER (LEN=5), POINTER, DIMENSION(:) :: nametsloc
      CHARACTER (LEN=25), POINTER, DIMENSION(:) :: desctsloc
      CHARACTER (LEN=256), POINTER, DIMENSION(:) :: ts_filename
      LOGICAL :: have_calculated_tslocs

   END TYPE domain
END MODULE module_domain_type
