!WRF:DRIVER_LAYER:DOMAIN_OBJECT
!
!  Following are the routines contained within this MODULE:

!  alloc_and_configure_domain        1. Allocate the space for a single domain (constants
!                                       and null terminate pointers).
!                                    2. Connect the domains as a linked list.
!                                    3. Store all of the domain constants.
!                                    4. CALL alloc_space_field.

!  alloc_space_field                 1. Allocate space for the gridded data required for
!                                       each domain.

!  dealloc_space_domain              1. Reconnect linked list nodes since the current
!                                       node is removed.
!                                    2. CALL dealloc_space_field.
!                                    3. Deallocate single domain.

!  dealloc_space_field               1. Deallocate each of the fields for a particular
!                                       domain.

!  first_loc_integer                 1. Find the first incidence of a particular
!                                       domain identifier from an array of domain
!                                       identifiers.

MODULE module_domain

   USE module_driver_constants
   USE module_machine
   USE module_state_description
   USE module_configure
   USE module_wrf_error
   USE module_utility

   CHARACTER (LEN=80) program_name

   !  An entire domain.  This contains multiple meteorological fields by having
   !  arrays (such as "data_3d") of pointers for each field.  Also inside each
   !  domain is a link to a couple of other domains, one is just the 
   !  "next" domain that is to be stored, the other is the next domain which 
   !  happens to also be on the "same_level".

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
                         RESTART_ALARM=24, BOUNDARY_ALARM=25, INPUTOUT_ALARM=26,  &  ! for outputing input (e.g. for 3dvar)
                         ALARM_SUBTIME=27,                                        &
                         MAX_WRF_ALARMS=27  ! WARNING:  MAX_WRF_ALARMS must be 
                                            ! large enough to include all of 
                                            ! the alarms declared above.  

!STARTOFREGISTRYGENERATEDINCLUDE 'inc/state_subtypes.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
!ENDOFREGISTRYGENERATEDINCLUDE

   TYPE domain

! SEE THE INCLUDE FILE FOR DEFINITIONS OF STATE FIELDS WITHIN THE DOMAIN DATA STRUCTURE
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

      INTEGER                                             :: comms( max_comms ), shift_x, shift_y

      INTEGER                                             :: id
      INTEGER                                             :: domdesc
      INTEGER                                             :: communicator
      INTEGER                                             :: iocommunicator
      INTEGER,POINTER                                     :: mapping(:,:)
      INTEGER,POINTER                                     :: i_start(:),i_end(:)
      INTEGER,POINTER                                     :: j_start(:),j_end(:)
      INTEGER                                             :: max_tiles
      INTEGER                                             :: num_tiles        ! taken out of namelist 20000908
      INTEGER                                             :: num_tiles_x      ! taken out of namelist 20000908
      INTEGER                                             :: num_tiles_y      ! taken out of namelist 20000908
      INTEGER                                             :: num_tiles_spec   ! place to store number of tiles computed from 
                                                                              ! externally specified params

      TYPE(domain_ptr) , DIMENSION( : ) , POINTER         :: parents                            
      TYPE(domain_ptr) , DIMENSION( : ) , POINTER         :: nests                            
      TYPE(domain) , POINTER                              :: sibling ! overlapped domains at same lev
      TYPE(domain) , POINTER                              :: intermediate_grid
      INTEGER                                             :: num_parents, num_nests, num_siblings
      INTEGER      , DIMENSION( max_parents )             :: child_of_parent
      INTEGER      , DIMENSION( max_nests )               :: active

      INTEGER      , DIMENSION(0:5)                       :: nframes          ! frames per outfile for history 
                                                                              ! streams (0 is main history)                  

      TYPE(domain) , POINTER                              :: next
      TYPE(domain) , POINTER                              :: same_level

      LOGICAL      , DIMENSION ( 4 )                      :: bdy_mask         ! which boundaries are on processor

      LOGICAL                                             :: first_force

      ! domain dimensions

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
! This awful hackery accounts for the fact that ESMF2.2.0 objects cannot tell 
! us if they have ever been created or not.  So, we have to keep track of this 
! ourselves to avoid destroying an object that has never been created!  Rip 
! this out once ESMF has useful introspection for creation...  
      LOGICAL :: domain_clock_created
      LOGICAL, POINTER :: alarms_created(:)

      ! Have clocks and times been initialized yet?
      LOGICAL :: time_set
      ! This flag controls first-time-step behavior for ESMF runs 
      ! which require components to return to the top-level driver 
      ! after initializing import and export states.  In WRF, this 
      ! initialization is done in the "training phase" of 
      ! med_before_solve_io().  
      LOGICAL                                             :: return_after_training_io

   END TYPE domain

   !  Now that a "domain" TYPE exists, we can use it to store a few pointers
   !  to this type.  These are primarily for use in traversing the linked list.
   !  The "head_grid" is always the pointer to the first domain that is
   !  allocated.  This is available and is not to be changed.  The others are
   !  just temporary pointers.

   TYPE(domain) , POINTER :: head_grid , new_grid , next_grid , old_grid

   !  To facilitate an easy integration of each of the domains that are on the
   !  same level, we have an array for the head pointer for each level.  This
   !  removed the need to search through the linked list at each time step to
   !  find which domains are to be active.

   TYPE domain_levels
      TYPE(domain) , POINTER                              :: first_domain
   END TYPE domain_levels

   TYPE(domain_levels) , DIMENSION(max_levels)            :: head_for_each_level

   ! Use this to support debugging features, giving easy access to clock, etc.  
   TYPE(domain), POINTER :: current_grid
   LOGICAL, SAVE :: current_grid_set = .FALSE.

   ! internal routines
   PRIVATE domain_time_test_print
   PRIVATE test_adjust_io_timestr

   INTERFACE get_ijk_from_grid
     MODULE PROCEDURE get_ijk_from_grid1, get_ijk_from_grid2
   END INTERFACE


CONTAINS

   SUBROUTINE adjust_domain_dims_for_move( grid , dx, dy )
    IMPLICIT NONE

    TYPE( domain ), POINTER   :: grid
    INTEGER, INTENT(IN) ::  dx, dy

    data_ordering : SELECT CASE ( model_data_order )
       CASE  ( DATA_ORDER_XYZ )
            grid%sm31  = grid%sm31 + dx
            grid%em31  = grid%em31 + dx
            grid%sm32  = grid%sm32 + dy
            grid%em32  = grid%em32 + dy
            grid%sp31  = grid%sp31 + dx
            grid%ep31  = grid%ep31 + dx
            grid%sp32  = grid%sp32 + dy
            grid%ep32  = grid%ep32 + dy
            grid%sd31  = grid%sd31 + dx
            grid%ed31  = grid%ed31 + dx
            grid%sd32  = grid%sd32 + dy
            grid%ed32  = grid%ed32 + dy

       CASE  ( DATA_ORDER_YXZ )
            grid%sm31  = grid%sm31 + dy
            grid%em31  = grid%em31 + dy
            grid%sm32  = grid%sm32 + dx
            grid%em32  = grid%em32 + dx
            grid%sp31  = grid%sp31 + dy
            grid%ep31  = grid%ep31 + dy
            grid%sp32  = grid%sp32 + dx
            grid%ep32  = grid%ep32 + dx
            grid%sd31  = grid%sd31 + dy
            grid%ed31  = grid%ed31 + dy
            grid%sd32  = grid%sd32 + dx
            grid%ed32  = grid%ed32 + dx

       CASE  ( DATA_ORDER_ZXY )
            grid%sm32  = grid%sm32 + dx
            grid%em32  = grid%em32 + dx
            grid%sm33  = grid%sm33 + dy
            grid%em33  = grid%em33 + dy
            grid%sp32  = grid%sp32 + dx
            grid%ep32  = grid%ep32 + dx
            grid%sp33  = grid%sp33 + dy
            grid%ep33  = grid%ep33 + dy
            grid%sd32  = grid%sd32 + dx
            grid%ed32  = grid%ed32 + dx
            grid%sd33  = grid%sd33 + dy
            grid%ed33  = grid%ed33 + dy

       CASE  ( DATA_ORDER_ZYX )
            grid%sm32  = grid%sm32 + dy
            grid%em32  = grid%em32 + dy
            grid%sm33  = grid%sm33 + dx
            grid%em33  = grid%em33 + dx
            grid%sp32  = grid%sp32 + dy
            grid%ep32  = grid%ep32 + dy
            grid%sp33  = grid%sp33 + dx
            grid%ep33  = grid%ep33 + dx
            grid%sd32  = grid%sd32 + dy
            grid%ed32  = grid%ed32 + dy
            grid%sd33  = grid%sd33 + dx
            grid%ed33  = grid%ed33 + dx

       CASE  ( DATA_ORDER_XZY )
            grid%sm31  = grid%sm31 + dx
            grid%em31  = grid%em31 + dx
            grid%sm33  = grid%sm33 + dy
            grid%em33  = grid%em33 + dy
            grid%sp31  = grid%sp31 + dx
            grid%ep31  = grid%ep31 + dx
            grid%sp33  = grid%sp33 + dy
            grid%ep33  = grid%ep33 + dy
            grid%sd31  = grid%sd31 + dx
            grid%ed31  = grid%ed31 + dx
            grid%sd33  = grid%sd33 + dy
            grid%ed33  = grid%ed33 + dy

       CASE  ( DATA_ORDER_YZX )
            grid%sm31  = grid%sm31 + dy
            grid%em31  = grid%em31 + dy
            grid%sm33  = grid%sm33 + dx
            grid%em33  = grid%em33 + dx
            grid%sp31  = grid%sp31 + dy
            grid%ep31  = grid%ep31 + dy
            grid%sp33  = grid%sp33 + dx
            grid%ep33  = grid%ep33 + dx
            grid%sd31  = grid%sd31 + dy
            grid%ed31  = grid%ed31 + dy
            grid%sd33  = grid%sd33 + dx
            grid%ed33  = grid%ed33 + dx

    END SELECT data_ordering


    RETURN
   END SUBROUTINE adjust_domain_dims_for_move

   SUBROUTINE get_ijk_from_grid1 (  grid ,                   &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe,    &
                           imsx, imex, jmsx, jmex, kmsx, kmex,    &
                           ipsx, ipex, jpsx, jpex, kpsx, kpex,    &
                           imsy, imey, jmsy, jmey, kmsy, kmey,    &
                           ipsy, ipey, jpsy, jpey, kpsy, kpey )
    IMPLICIT NONE
    TYPE( domain ), INTENT (IN)  :: grid
    INTEGER, INTENT(OUT) ::                                 &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe,    &
                           imsx, imex, jmsx, jmex, kmsx, kmex,    &
                           ipsx, ipex, jpsx, jpex, kpsx, kpex,    &
                           imsy, imey, jmsy, jmey, kmsy, kmey,    &
                           ipsy, ipey, jpsy, jpey, kpsy, kpey

     CALL get_ijk_from_grid2 (  grid ,                   &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe )
     data_ordering : SELECT CASE ( model_data_order )
       CASE  ( DATA_ORDER_XYZ )
           imsx = grid%sm31x ; imex = grid%em31x ; jmsx = grid%sm32x ; jmex = grid%em32x ; kmsx = grid%sm33x ; kmex = grid%em33x ;
           ipsx = grid%sp31x ; ipex = grid%ep31x ; jpsx = grid%sp32x ; jpex = grid%ep32x ; kpsx = grid%sp33x ; kpex = grid%ep33x ;
           imsy = grid%sm31y ; imey = grid%em31y ; jmsy = grid%sm32y ; jmey = grid%em32y ; kmsy = grid%sm33y ; kmey = grid%em33y ;
           ipsy = grid%sp31y ; ipey = grid%ep31y ; jpsy = grid%sp32y ; jpey = grid%ep32y ; kpsy = grid%sp33y ; kpey = grid%ep33y ;
       CASE  ( DATA_ORDER_YXZ )
           imsx = grid%sm32x ; imex = grid%em32x ; jmsx = grid%sm31x ; jmex = grid%em31x ; kmsx = grid%sm33x ; kmex = grid%em33x ;
           ipsx = grid%sp32x ; ipex = grid%ep32x ; jpsx = grid%sp31x ; jpex = grid%ep31x ; kpsx = grid%sp33x ; kpex = grid%ep33x ;
           imsy = grid%sm32y ; imey = grid%em32y ; jmsy = grid%sm31y ; jmey = grid%em31y ; kmsy = grid%sm33y ; kmey = grid%em33y ;
           ipsy = grid%sp32y ; ipey = grid%ep32y ; jpsy = grid%sp31y ; jpey = grid%ep31y ; kpsy = grid%sp33y ; kpey = grid%ep33y ;
       CASE  ( DATA_ORDER_ZXY )
           imsx = grid%sm32x ; imex = grid%em32x ; jmsx = grid%sm33x ; jmex = grid%em33x ; kmsx = grid%sm31x ; kmex = grid%em31x ;
           ipsx = grid%sp32x ; ipex = grid%ep32x ; jpsx = grid%sp33x ; jpex = grid%ep33x ; kpsx = grid%sp31x ; kpex = grid%ep31x ;
           imsy = grid%sm32y ; imey = grid%em32y ; jmsy = grid%sm33y ; jmey = grid%em33y ; kmsy = grid%sm31y ; kmey = grid%em31y ;
           ipsy = grid%sp32y ; ipey = grid%ep32y ; jpsy = grid%sp33y ; jpey = grid%ep33y ; kpsy = grid%sp31y ; kpey = grid%ep31y ;
       CASE  ( DATA_ORDER_ZYX )
           imsx = grid%sm33x ; imex = grid%em33x ; jmsx = grid%sm32x ; jmex = grid%em32x ; kmsx = grid%sm31x ; kmex = grid%em31x ;
           ipsx = grid%sp33x ; ipex = grid%ep33x ; jpsx = grid%sp32x ; jpex = grid%ep32x ; kpsx = grid%sp31x ; kpex = grid%ep31x ;
           imsy = grid%sm33y ; imey = grid%em33y ; jmsy = grid%sm32y ; jmey = grid%em32y ; kmsy = grid%sm31y ; kmey = grid%em31y ;
           ipsy = grid%sp33y ; ipey = grid%ep33y ; jpsy = grid%sp32y ; jpey = grid%ep32y ; kpsy = grid%sp31y ; kpey = grid%ep31y ;
       CASE  ( DATA_ORDER_XZY )
           imsx = grid%sm31x ; imex = grid%em31x ; jmsx = grid%sm33x ; jmex = grid%em33x ; kmsx = grid%sm32x ; kmex = grid%em32x ;
           ipsx = grid%sp31x ; ipex = grid%ep31x ; jpsx = grid%sp33x ; jpex = grid%ep33x ; kpsx = grid%sp32x ; kpex = grid%ep32x ;
           imsy = grid%sm31y ; imey = grid%em31y ; jmsy = grid%sm33y ; jmey = grid%em33y ; kmsy = grid%sm32y ; kmey = grid%em32y ;
           ipsy = grid%sp31y ; ipey = grid%ep31y ; jpsy = grid%sp33y ; jpey = grid%ep33y ; kpsy = grid%sp32y ; kpey = grid%ep32y ;
       CASE  ( DATA_ORDER_YZX )
           imsx = grid%sm33x ; imex = grid%em33x ; jmsx = grid%sm31x ; jmex = grid%em31x ; kmsx = grid%sm32x ; kmex = grid%em32x ;
           ipsx = grid%sp33x ; ipex = grid%ep33x ; jpsx = grid%sp31x ; jpex = grid%ep31x ; kpsx = grid%sp32x ; kpex = grid%ep32x ;
           imsy = grid%sm33y ; imey = grid%em33y ; jmsy = grid%sm31y ; jmey = grid%em31y ; kmsy = grid%sm32y ; kmey = grid%em32y ;
           ipsy = grid%sp33y ; ipey = grid%ep33y ; jpsy = grid%sp31y ; jpey = grid%ep31y ; kpsy = grid%sp32y ; kpey = grid%ep32y ;
     END SELECT data_ordering
   END SUBROUTINE get_ijk_from_grid1

   SUBROUTINE get_ijk_from_grid2 (  grid ,                   &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe )

    IMPLICIT NONE

    TYPE( domain ), INTENT (IN)  :: grid
    INTEGER, INTENT(OUT) ::                                 &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe

    data_ordering : SELECT CASE ( model_data_order )
       CASE  ( DATA_ORDER_XYZ )
           ids = grid%sd31 ; ide = grid%ed31 ; jds = grid%sd32 ; jde = grid%ed32 ; kds = grid%sd33 ; kde = grid%ed33 ;
           ims = grid%sm31 ; ime = grid%em31 ; jms = grid%sm32 ; jme = grid%em32 ; kms = grid%sm33 ; kme = grid%em33 ;
           ips = grid%sp31 ; ipe = grid%ep31 ; jps = grid%sp32 ; jpe = grid%ep32 ; kps = grid%sp33 ; kpe = grid%ep33 ; 
       CASE  ( DATA_ORDER_YXZ )
           ids = grid%sd32  ; ide = grid%ed32  ; jds = grid%sd31  ; jde = grid%ed31  ; kds = grid%sd33  ; kde = grid%ed33  ; 
           ims = grid%sm32  ; ime = grid%em32  ; jms = grid%sm31  ; jme = grid%em31  ; kms = grid%sm33  ; kme = grid%em33  ; 
           ips = grid%sp32  ; ipe = grid%ep32  ; jps = grid%sp31  ; jpe = grid%ep31  ; kps = grid%sp33  ; kpe = grid%ep33  ; 
       CASE  ( DATA_ORDER_ZXY )
           ids = grid%sd32  ; ide = grid%ed32  ; jds = grid%sd33  ; jde = grid%ed33  ; kds = grid%sd31  ; kde = grid%ed31  ; 
           ims = grid%sm32  ; ime = grid%em32  ; jms = grid%sm33  ; jme = grid%em33  ; kms = grid%sm31  ; kme = grid%em31  ; 
           ips = grid%sp32  ; ipe = grid%ep32  ; jps = grid%sp33  ; jpe = grid%ep33  ; kps = grid%sp31  ; kpe = grid%ep31  ; 
       CASE  ( DATA_ORDER_ZYX )
           ids = grid%sd33  ; ide = grid%ed33  ; jds = grid%sd32  ; jde = grid%ed32  ; kds = grid%sd31  ; kde = grid%ed31  ; 
           ims = grid%sm33  ; ime = grid%em33  ; jms = grid%sm32  ; jme = grid%em32  ; kms = grid%sm31  ; kme = grid%em31  ; 
           ips = grid%sp33  ; ipe = grid%ep33  ; jps = grid%sp32  ; jpe = grid%ep32  ; kps = grid%sp31  ; kpe = grid%ep31  ; 
       CASE  ( DATA_ORDER_XZY )
           ids = grid%sd31  ; ide = grid%ed31  ; jds = grid%sd33  ; jde = grid%ed33  ; kds = grid%sd32  ; kde = grid%ed32  ; 
           ims = grid%sm31  ; ime = grid%em31  ; jms = grid%sm33  ; jme = grid%em33  ; kms = grid%sm32  ; kme = grid%em32  ; 
           ips = grid%sp31  ; ipe = grid%ep31  ; jps = grid%sp33  ; jpe = grid%ep33  ; kps = grid%sp32  ; kpe = grid%ep32  ; 
       CASE  ( DATA_ORDER_YZX )
           ids = grid%sd33  ; ide = grid%ed33  ; jds = grid%sd31  ; jde = grid%ed31  ; kds = grid%sd32  ; kde = grid%ed32  ; 
           ims = grid%sm33  ; ime = grid%em33  ; jms = grid%sm31  ; jme = grid%em31  ; kms = grid%sm32  ; kme = grid%em32  ; 
           ips = grid%sp33  ; ipe = grid%ep33  ; jps = grid%sp31  ; jpe = grid%ep31  ; kps = grid%sp32  ; kpe = grid%ep32  ; 
    END SELECT data_ordering
   END SUBROUTINE get_ijk_from_grid2

! Default version ; Otherwise module containing interface to DM library will provide

   SUBROUTINE wrf_patch_domain( id , domdesc , parent, parent_id , parent_domdesc , &
                            sd1 , ed1 , sp1 , ep1 , sm1 , em1 , &
                            sd2 , ed2 , sp2 , ep2 , sm2 , em2 , &
                            sd3 , ed3 , sp3 , ep3 , sm3 , em3 , &
                                        sp1x , ep1x , sm1x , em1x , &
                                        sp2x , ep2x , sm2x , em2x , &
                                        sp3x , ep3x , sm3x , em3x , &
                                        sp1y , ep1y , sm1y , em1y , &
                                        sp2y , ep2y , sm2y , em2y , &
                                        sp3y , ep3y , sm3y , em3y , &
                            bdx , bdy , bdy_mask )
!<DESCRIPTION>
! Wrf_patch_domain is called as part of the process of initiating a new
! domain.  Based on the global domain dimension information that is
! passed in it computes the patch and memory dimensions on this
! distributed-memory process for parallel compilation when 1 is
! defined in configure.wrf.  In this case, it relies on an external
! communications package-contributed routine, wrf_dm_patch_domain. For
! non-parallel compiles, it returns the patch and memory dimensions based
! on the entire domain. In either case, the memory dimensions will be
! larger than the patch dimensions, since they allow for distributed
! memory halo regions (1 only) and for boundary regions around
! the domain (used for idealized cases only).  The width of the boundary
! regions to be accommodated is passed in as bdx and bdy.
! 
! The bdy_mask argument is a four-dimensional logical array, each element
! of which is set to true for any boundaries that this processs patch
! contains (all four are true in the non-1 case) and false
! otherwise. The indices into the bdy_mask are defined in
! frame/module_state_description.F. P_XSB corresponds boundary that
! exists at the beginning of the X-dimension; ie. the western boundary;
! P_XEB to the boundary that corresponds to the end of the X-dimension
! (east). Likewise for Y (south and north respectively).
! 
! The correspondence of the first, second, and third dimension of each
! set (domain, memory, and patch) with the coordinate axes of the model
! domain is based on the setting of the variable model_data_order, which
! comes into this routine through USE association of
! module_driver_constants in the enclosing module of this routine,
! module_domain.  Model_data_order is defined by the Registry, based on
! the dimspec entries which associate dimension specifiers (e.g. k) in
! the Registry with a coordinate axis and specify which dimension of the
! arrays they represent. For WRF, the sd1 , ed1 , sp1 , ep1 , sm1 , and
! em1 correspond to the starts and ends of the global, patch, and memory
! dimensions in X; those with 2 specify Z (vertical); and those with 3
! specify Y.  Note that the WRF convention is to overdimension to allow
! for staggered fields so that sd<em>n</em>:ed<em>n</em> are the starts
! and ends of the staggered domains in X.  The non-staggered grid runs
! sd<em>n</em>:ed<em>n</em>-1. The extra row or column on the north or
! east boundaries is not used for non-staggered fields.
! 
! The domdesc and parent_domdesc arguments are for external communication
! packages (e.g. RSL) that establish and return to WRF integer handles
! for referring to operations on domains.  These descriptors are not set
! or used otherwise and they are opaque, which means they are never
! accessed or modified in WRF; they are only only passed between calls to
! the external package.
!</DESCRIPTION>

   USE module_machine
   IMPLICIT NONE
   LOGICAL, DIMENSION(4), INTENT(OUT)  :: bdy_mask
   INTEGER, INTENT(IN)   :: sd1 , ed1 , sd2 , ed2 , sd3 , ed3 , bdx , bdy
   INTEGER, INTENT(OUT)  :: sp1  , ep1  , sp2  , ep2  , sp3  , ep3  , &  ! z-xpose (std)
                            sm1  , em1  , sm2  , em2  , sm3  , em3
   INTEGER, INTENT(OUT)  :: sp1x , ep1x , sp2x , ep2x , sp3x , ep3x , &  ! x-xpose
                            sm1x , em1x , sm2x , em2x , sm3x , em3x
   INTEGER, INTENT(OUT)  :: sp1y , ep1y , sp2y , ep2y , sp3y , ep3y , &  ! y-xpose
                            sm1y , em1y , sm2y , em2y , sm3y , em3y
   INTEGER, INTENT(IN)   :: id , parent_id , parent_domdesc
   INTEGER, INTENT(INOUT)  :: domdesc
   TYPE(domain), POINTER :: parent

!local data

   INTEGER spec_bdy_width

   CALL nl_get_spec_bdy_width( 1, spec_bdy_width )

! This is supplied by the package specific version of module_dm, which
! is supplied by the external package and copied into the src directory
! when the code is compiled. The cp command will be found in the externals
! target of the configure.wrf file for this architecture.  Eg: for RSL
! routine is defined in external/RSL/module_dm.F .
! Note, it would be very nice to be able to pass parent to this routine;
! however, there doesnt seem to be a way to do that in F90. That is because
! to pass a pointer to a domain structure, this call requires an interface
! definition for wrf_dm_patch_domain (otherwise it will try to convert the
! pointer to something). In order to provide an interface definition, we
! would need to either USE module_dm or use an interface block. In either
! case it generates a circular USE reference, since module_dm uses
! module_domain.  JM 20020416

   CALL wrf_dm_patch_domain( id , domdesc , parent_id , parent_domdesc , &
                             sd1 , ed1 , sp1 , ep1 , sm1 , em1 , &
                             sd2 , ed2 , sp2 , ep2 , sm2 , em2 , &
                             sd3 , ed3 , sp3 , ep3 , sm3 , em3 , &
                                         sp1x , ep1x , sm1x , em1x , &
                                         sp2x , ep2x , sm2x , em2x , &
                                         sp3x , ep3x , sm3x , em3x , &
                                         sp1y , ep1y , sm1y , em1y , &
                                         sp2y , ep2y , sm2y , em2y , &
                                         sp3y , ep3y , sm3y , em3y , &
                             bdx , bdy )

   SELECT CASE ( model_data_order )
      CASE ( DATA_ORDER_XYZ )
   bdy_mask( P_XSB ) = ( sp1 <= sd1 .AND. sd1 <= ep1 .AND. sp1 <= sd1+spec_bdy_width-1 .AND. sd1+spec_bdy_width-1 <= ep1 )
   bdy_mask( P_YSB ) = ( sp2 <= sd2 .AND. sd2 <= ep2 .AND. sp2 <= sd2+spec_bdy_width-1 .AND. sd2+spec_bdy_width-1 <= ep2 )
   bdy_mask( P_XEB ) = ( sp1 <= ed1 .AND. ed1 <= ep1 .AND. sp1 <= ed1-spec_bdy_width-1 .AND. ed1-spec_bdy_width-1 <= ep1 )
   bdy_mask( P_YEB ) = ( sp2 <= ed2 .AND. ed2 <= ep2 .AND. sp2 <= ed2-spec_bdy_width-1 .AND. ed2-spec_bdy_width-1 <= ep2 )
      CASE ( DATA_ORDER_YXZ )
   bdy_mask( P_XSB ) = ( sp2 <= sd2 .AND. sd2 <= ep2 .AND. sp2 <= sd2+spec_bdy_width-1 .AND. sd2+spec_bdy_width-1 <= ep2 )
   bdy_mask( P_YSB ) = ( sp1 <= sd1 .AND. sd1 <= ep1 .AND. sp1 <= sd1+spec_bdy_width-1 .AND. sd1+spec_bdy_width-1 <= ep1 )
   bdy_mask( P_XEB ) = ( sp2 <= ed2 .AND. ed2 <= ep2 .AND. sp2 <= ed2-spec_bdy_width-1 .AND. ed2-spec_bdy_width-1 <= ep2 )
   bdy_mask( P_YEB ) = ( sp1 <= ed1 .AND. ed1 <= ep1 .AND. sp1 <= ed1-spec_bdy_width-1 .AND. ed1-spec_bdy_width-1 <= ep1 )
      CASE ( DATA_ORDER_ZXY )
   bdy_mask( P_XSB ) = ( sp2 <= sd2 .AND. sd2 <= ep2 .AND. sp2 <= sd2+spec_bdy_width-1 .AND. sd2+spec_bdy_width-1 <= ep2 )
   bdy_mask( P_YSB ) = ( sp3 <= sd3 .AND. sd3 <= ep3 .AND. sp3 <= sd3+spec_bdy_width-1 .AND. sd3+spec_bdy_width-1 <= ep3 )
   bdy_mask( P_XEB ) = ( sp2 <= ed2 .AND. ed2 <= ep2 .AND. sp2 <= ed2-spec_bdy_width-1 .AND. ed2-spec_bdy_width-1 <= ep2 )
   bdy_mask( P_YEB ) = ( sp3 <= ed3 .AND. ed3 <= ep3 .AND. sp3 <= ed3-spec_bdy_width-1 .AND. ed3-spec_bdy_width-1 <= ep3 )
      CASE ( DATA_ORDER_ZYX )
   bdy_mask( P_XSB ) = ( sp3 <= sd3 .AND. sd3 <= ep3 .AND. sp3 <= sd3+spec_bdy_width-1 .AND. sd3+spec_bdy_width-1 <= ep3 )
   bdy_mask( P_YSB ) = ( sp2 <= sd2 .AND. sd2 <= ep2 .AND. sp2 <= sd2+spec_bdy_width-1 .AND. sd2+spec_bdy_width-1 <= ep2 )
   bdy_mask( P_XEB ) = ( sp3 <= ed3 .AND. ed3 <= ep3 .AND. sp3 <= ed3-spec_bdy_width-1 .AND. ed3-spec_bdy_width-1 <= ep3 )
   bdy_mask( P_YEB ) = ( sp2 <= ed2 .AND. ed2 <= ep2 .AND. sp2 <= ed2-spec_bdy_width-1 .AND. ed2-spec_bdy_width-1 <= ep2 )
      CASE ( DATA_ORDER_XZY )
   bdy_mask( P_XSB ) = ( sp1 <= sd1 .AND. sd1 <= ep1 .AND. sp1 <= sd1+spec_bdy_width-1 .AND. sd1+spec_bdy_width-1 <= ep1 )
   bdy_mask( P_YSB ) = ( sp3 <= sd3 .AND. sd3 <= ep3 .AND. sp3 <= sd3+spec_bdy_width-1 .AND. sd3+spec_bdy_width-1 <= ep3 )
   bdy_mask( P_XEB ) = ( sp1 <= ed1 .AND. ed1 <= ep1 .AND. sp1 <= ed1-spec_bdy_width-1 .AND. ed1-spec_bdy_width-1 <= ep1 )
   bdy_mask( P_YEB ) = ( sp3 <= ed3 .AND. ed3 <= ep3 .AND. sp3 <= ed3-spec_bdy_width-1 .AND. ed3-spec_bdy_width-1 <= ep3 )
      CASE ( DATA_ORDER_YZX )
   bdy_mask( P_XSB ) = ( sp3 <= sd3 .AND. sd3 <= ep3 .AND. sp3 <= sd3+spec_bdy_width-1 .AND. sd3+spec_bdy_width-1 <= ep3 )
   bdy_mask( P_YSB ) = ( sp1 <= sd1 .AND. sd1 <= ep1 .AND. sp1 <= sd1+spec_bdy_width-1 .AND. sd1+spec_bdy_width-1 <= ep1 )
   bdy_mask( P_XEB ) = ( sp3 <= ed3 .AND. ed3 <= ep3 .AND. sp3 <= ed3-spec_bdy_width-1 .AND. ed3-spec_bdy_width-1 <= ep3 )
   bdy_mask( P_YEB ) = ( sp1 <= ed1 .AND. ed1 <= ep1 .AND. sp1 <= ed1-spec_bdy_width-1 .AND. ed1-spec_bdy_width-1 <= ep1 )
   END SELECT


   RETURN
   END SUBROUTINE wrf_patch_domain
!
   SUBROUTINE alloc_and_configure_domain ( domain_id , grid , parent, kid )

!<DESCRIPTION>
! This subroutine is used to allocate a domain data structure of
! TYPE(DOMAIN) pointed to by the argument <em>grid</em>, link it into the
! nested domain hierarchy, and set its configuration information from
! the appropriate settings in the WRF namelist file. Specifically, if the
! domain being allocated and configured is nest, the <em>parent</em>
! argument will point to the already existing domain data structure for
! the parent domain and the <em>kid</em> argument will be set to an
! integer indicating which child of the parent this grid will be (child
! indices start at 1).  If this is the top-level domain, the parent and
! kid arguments are ignored.  <b>WRF domains may have multiple children
! but only ever have one parent.</b>
!
! The <em>domain_id</em> argument is the
! integer handle by which this new domain will be referred; it comes from
! the grid_id setting in the namelist, and these grid ids correspond to
! the ordering of settings in the namelist, starting with 1 for the
! top-level domain. The id of 1 always corresponds to the top-level
! domain.  and these grid ids correspond to the ordering of settings in
! the namelist, starting with 1 for the top-level domain.
! 
! Model_data_order is provide by USE association of
! module_driver_constants and is set from dimspec entries in the
! Registry.
! 
! The allocation of the TYPE(DOMAIN) itself occurs in this routine.
! However, the numerous multi-dimensional arrays that make up the members
! of the domain are allocated in the call to alloc_space_field, after
! wrf_patch_domain has been called to determine the dimensions in memory
! that should be allocated.  It bears noting here that arrays and code
! that indexes these arrays are always global, regardless of how the
! model is decomposed over patches. Thus, when arrays are allocated on a
! given process, the start and end of an array dimension are the global
! indices of the start and end of that processs subdomain.
! 
! Configuration information for the domain (that is, information from the
! namelist) is added by the call to <a href=med_add_config_info_to_grid.html>med_add_config_info_to_grid</a>, defined
! in share/mediation_wrfmain.F. 
!</DESCRIPTION>

      
      IMPLICIT NONE

      !  Input data.

      INTEGER , INTENT(IN)                           :: domain_id
      TYPE( domain ) , POINTER                       :: grid
      TYPE( domain ) , POINTER                       :: parent
      INTEGER , INTENT(IN)                           :: kid    ! which kid of parent am I?

      !  Local data.
      INTEGER                     :: sd1 , ed1 , sp1 , ep1 , sm1 , em1
      INTEGER                     :: sd2 , ed2 , sp2 , ep2 , sm2 , em2
      INTEGER                     :: sd3 , ed3 , sp3 , ep3 , sm3 , em3

      INTEGER                     :: sd1x , ed1x , sp1x , ep1x , sm1x , em1x
      INTEGER                     :: sd2x , ed2x , sp2x , ep2x , sm2x , em2x
      INTEGER                     :: sd3x , ed3x , sp3x , ep3x , sm3x , em3x

      INTEGER                     :: sd1y , ed1y , sp1y , ep1y , sm1y , em1y
      INTEGER                     :: sd2y , ed2y , sp2y , ep2y , sm2y , em2y
      INTEGER                     :: sd3y , ed3y , sp3y , ep3y , sm3y , em3y

      TYPE(domain) , POINTER      :: new_grid
      INTEGER                     :: i
      INTEGER                     :: parent_id , parent_domdesc , new_domdesc
      INTEGER                     :: bdyzone_x , bdyzone_y
      INTEGER                     :: nx, ny


! This next step uses information that is listed in the registry as namelist_derived
! to properly size the domain and the patches; this in turn is stored in the new_grid
! data structure


      data_ordering : SELECT CASE ( model_data_order )
        CASE  ( DATA_ORDER_XYZ )

          CALL nl_get_s_we( domain_id , sd1 )
          CALL nl_get_e_we( domain_id , ed1 )
          CALL nl_get_s_sn( domain_id , sd2 )
          CALL nl_get_e_sn( domain_id , ed2 )
          CALL nl_get_s_vert( domain_id , sd3 )
          CALL nl_get_e_vert( domain_id , ed3 )
          nx = ed1-sd1+1
          ny = ed2-sd2+1

        CASE  ( DATA_ORDER_YXZ )

          CALL nl_get_s_sn( domain_id , sd1 )
          CALL nl_get_e_sn( domain_id , ed1 )
          CALL nl_get_s_we( domain_id , sd2 )
          CALL nl_get_e_we( domain_id , ed2 )
          CALL nl_get_s_vert( domain_id , sd3 )
          CALL nl_get_e_vert( domain_id , ed3 )
          nx = ed2-sd2+1
          ny = ed1-sd1+1

        CASE  ( DATA_ORDER_ZXY )

          CALL nl_get_s_vert( domain_id , sd1 )
          CALL nl_get_e_vert( domain_id , ed1 )
          CALL nl_get_s_we( domain_id , sd2 )
          CALL nl_get_e_we( domain_id , ed2 )
          CALL nl_get_s_sn( domain_id , sd3 )
          CALL nl_get_e_sn( domain_id , ed3 )
          nx = ed2-sd2+1
          ny = ed3-sd3+1

        CASE  ( DATA_ORDER_ZYX )

          CALL nl_get_s_vert( domain_id , sd1 )
          CALL nl_get_e_vert( domain_id , ed1 )
          CALL nl_get_s_sn( domain_id , sd2 )
          CALL nl_get_e_sn( domain_id , ed2 )
          CALL nl_get_s_we( domain_id , sd3 )
          CALL nl_get_e_we( domain_id , ed3 )
          nx = ed3-sd3+1
          ny = ed2-sd2+1

        CASE  ( DATA_ORDER_XZY )

          CALL nl_get_s_we( domain_id , sd1 )
          CALL nl_get_e_we( domain_id , ed1 )
          CALL nl_get_s_vert( domain_id , sd2 )
          CALL nl_get_e_vert( domain_id , ed2 )
          CALL nl_get_s_sn( domain_id , sd3 )
          CALL nl_get_e_sn( domain_id , ed3 )
          nx = ed1-sd1+1
          ny = ed3-sd3+1

        CASE  ( DATA_ORDER_YZX )

          CALL nl_get_s_sn( domain_id , sd1 )
          CALL nl_get_e_sn( domain_id , ed1 )
          CALL nl_get_s_vert( domain_id , sd2 )
          CALL nl_get_e_vert( domain_id , ed2 )
          CALL nl_get_s_we( domain_id , sd3 )
          CALL nl_get_e_we( domain_id , ed3 )
          nx = ed3-sd3+1
          ny = ed1-sd1+1

      END SELECT data_ordering



      IF ( num_time_levels > 3 ) THEN
        WRITE ( wrf_err_message , * ) 'alloc_and_configure_domain: ', &
          'Incorrect value for num_time_levels ', num_time_levels
        CALL wrf_error_fatal3 ( "module_domain.b" , 742 ,  TRIM ( wrf_err_message ) )
      ENDIF

      IF (ASSOCIATED(parent)) THEN
        parent_id = parent%id
        parent_domdesc = parent%domdesc
      ELSE
        parent_id = -1
        parent_domdesc = -1
      ENDIF

! provided by application, WRF defines in share/module_bc.F
      CALL get_bdyzone_x( bdyzone_x )
      CALL get_bdyzone_y( bdyzone_y )

      ALLOCATE ( new_grid )
      ALLOCATE ( new_grid%parents( max_parents ) )
      ALLOCATE ( new_grid%nests( max_nests ) )
      NULLIFY( new_grid%sibling )
      DO i = 1, max_nests
         NULLIFY( new_grid%nests(i)%ptr )
      ENDDO
      NULLIFY  (new_grid%next)
      NULLIFY  (new_grid%same_level)
      NULLIFY  (new_grid%i_start)
      NULLIFY  (new_grid%j_start)
      NULLIFY  (new_grid%i_end)
      NULLIFY  (new_grid%j_end)
      ALLOCATE( new_grid%domain_clock )
      new_grid%domain_clock_created = .FALSE.
      ALLOCATE( new_grid%alarms( MAX_WRF_ALARMS ) )    ! initialize in setup_timekeeping
      ALLOCATE( new_grid%alarms_created( MAX_WRF_ALARMS ) )
      DO i = 1, MAX_WRF_ALARMS
        new_grid%alarms_created( i ) = .FALSE.
      ENDDO
      new_grid%time_set = .FALSE.
      new_grid%return_after_training_io = .FALSE.

      ! set up the pointers that represent the nest hierarchy
      ! set this up *prior* to calling the patching or allocation
      ! routines so that implementations of these routines can
      ! traverse the nest hierarchy (through the root head_grid)
      ! if they need to 

 
      IF ( domain_id .NE. 1 ) THEN
         new_grid%parents(1)%ptr => parent
         new_grid%num_parents = 1
         parent%nests(kid)%ptr => new_grid
         new_grid%child_of_parent(1) = kid    ! note assumption that nest can have only 1 parent
         parent%num_nests = parent%num_nests + 1
      END IF
      new_grid%id = domain_id                 ! this needs to be assigned prior to calling wrf_patch_domain

      CALL wrf_patch_domain( domain_id  , new_domdesc , parent, parent_id, parent_domdesc , &

                             sd1 , ed1 , sp1 , ep1 , sm1 , em1 , &     ! z-xpose dims
                             sd2 , ed2 , sp2 , ep2 , sm2 , em2 , &     ! (standard)
                             sd3 , ed3 , sp3 , ep3 , sm3 , em3 , &

                                     sp1x , ep1x , sm1x , em1x , &     ! x-xpose dims
                                     sp2x , ep2x , sm2x , em2x , &
                                     sp3x , ep3x , sm3x , em3x , &

                                     sp1y , ep1y , sm1y , em1y , &     ! y-xpose dims
                                     sp2y , ep2y , sm2y , em2y , &
                                     sp3y , ep3y , sm3y , em3y , &

                         bdyzone_x  , bdyzone_y , new_grid%bdy_mask &
      ) 


      new_grid%domdesc = new_domdesc
      new_grid%num_nests = 0
      new_grid%num_siblings = 0
      new_grid%num_parents = 0
      new_grid%max_tiles   = 0
      new_grid%num_tiles_spec   = 0
      new_grid%nframes   = 0         ! initialize the number of frames per file (array assignment)

      CALL alloc_space_field ( new_grid, domain_id , 3 , 3 , .FALSE. ,      &
                               sd1, ed1, sd2, ed2, sd3, ed3,       &
                               sm1,  em1,  sm2,  em2,  sm3,  em3,  &
                               sm1x, em1x, sm2x, em2x, sm3x, em3x, &   ! x-xpose
                               sm1y, em1y, sm2y, em2y, sm3y, em3y  &   ! y-xpose
      )

      new_grid%sd31                            = sd1 
      new_grid%ed31                            = ed1
      new_grid%sp31                            = sp1 
      new_grid%ep31                            = ep1 
      new_grid%sm31                            = sm1 
      new_grid%em31                            = em1
      new_grid%sd32                            = sd2 
      new_grid%ed32                            = ed2
      new_grid%sp32                            = sp2 
      new_grid%ep32                            = ep2 
      new_grid%sm32                            = sm2 
      new_grid%em32                            = em2
      new_grid%sd33                            = sd3 
      new_grid%ed33                            = ed3
      new_grid%sp33                            = sp3 
      new_grid%ep33                            = ep3 
      new_grid%sm33                            = sm3 
      new_grid%em33                            = em3

      new_grid%sp31x                           = sp1x
      new_grid%ep31x                           = ep1x
      new_grid%sm31x                           = sm1x
      new_grid%em31x                           = em1x
      new_grid%sp32x                           = sp2x
      new_grid%ep32x                           = ep2x
      new_grid%sm32x                           = sm2x
      new_grid%em32x                           = em2x
      new_grid%sp33x                           = sp3x
      new_grid%ep33x                           = ep3x
      new_grid%sm33x                           = sm3x
      new_grid%em33x                           = em3x

      new_grid%sp31y                           = sp1y
      new_grid%ep31y                           = ep1y
      new_grid%sm31y                           = sm1y
      new_grid%em31y                           = em1y
      new_grid%sp32y                           = sp2y
      new_grid%ep32y                           = ep2y
      new_grid%sm32y                           = sm2y
      new_grid%em32y                           = em2y
      new_grid%sp33y                           = sp3y
      new_grid%ep33y                           = ep3y
      new_grid%sm33y                           = sm3y
      new_grid%em33y                           = em3y

      SELECT CASE ( model_data_order )
         CASE  ( DATA_ORDER_XYZ )
            new_grid%sd21 = sd1 ; new_grid%sd22 = sd2 ;
            new_grid%ed21 = ed1 ; new_grid%ed22 = ed2 ;
            new_grid%sp21 = sp1 ; new_grid%sp22 = sp2 ;
            new_grid%ep21 = ep1 ; new_grid%ep22 = ep2 ;
            new_grid%sm21 = sm1 ; new_grid%sm22 = sm2 ;
            new_grid%em21 = em1 ; new_grid%em22 = em2 ;
            new_grid%sd11 = sd1
            new_grid%ed11 = ed1
            new_grid%sp11 = sp1
            new_grid%ep11 = ep1
            new_grid%sm11 = sm1
            new_grid%em11 = em1
         CASE  ( DATA_ORDER_YXZ )
            new_grid%sd21 = sd1 ; new_grid%sd22 = sd2 ;
            new_grid%ed21 = ed1 ; new_grid%ed22 = ed2 ;
            new_grid%sp21 = sp1 ; new_grid%sp22 = sp2 ;
            new_grid%ep21 = ep1 ; new_grid%ep22 = ep2 ;
            new_grid%sm21 = sm1 ; new_grid%sm22 = sm2 ;
            new_grid%em21 = em1 ; new_grid%em22 = em2 ;
            new_grid%sd11 = sd1
            new_grid%ed11 = ed1
            new_grid%sp11 = sp1
            new_grid%ep11 = ep1
            new_grid%sm11 = sm1
            new_grid%em11 = em1
         CASE  ( DATA_ORDER_ZXY )
            new_grid%sd21 = sd2 ; new_grid%sd22 = sd3 ;
            new_grid%ed21 = ed2 ; new_grid%ed22 = ed3 ;
            new_grid%sp21 = sp2 ; new_grid%sp22 = sp3 ;
            new_grid%ep21 = ep2 ; new_grid%ep22 = ep3 ;
            new_grid%sm21 = sm2 ; new_grid%sm22 = sm3 ;
            new_grid%em21 = em2 ; new_grid%em22 = em3 ;
            new_grid%sd11 = sd2
            new_grid%ed11 = ed2
            new_grid%sp11 = sp2
            new_grid%ep11 = ep2
            new_grid%sm11 = sm2
            new_grid%em11 = em2
         CASE  ( DATA_ORDER_ZYX )
            new_grid%sd21 = sd2 ; new_grid%sd22 = sd3 ;
            new_grid%ed21 = ed2 ; new_grid%ed22 = ed3 ;
            new_grid%sp21 = sp2 ; new_grid%sp22 = sp3 ;
            new_grid%ep21 = ep2 ; new_grid%ep22 = ep3 ;
            new_grid%sm21 = sm2 ; new_grid%sm22 = sm3 ;
            new_grid%em21 = em2 ; new_grid%em22 = em3 ;
            new_grid%sd11 = sd2
            new_grid%ed11 = ed2
            new_grid%sp11 = sp2
            new_grid%ep11 = ep2
            new_grid%sm11 = sm2
            new_grid%em11 = em2
         CASE  ( DATA_ORDER_XZY )
            new_grid%sd21 = sd1 ; new_grid%sd22 = sd3 ;
            new_grid%ed21 = ed1 ; new_grid%ed22 = ed3 ;
            new_grid%sp21 = sp1 ; new_grid%sp22 = sp3 ;
            new_grid%ep21 = ep1 ; new_grid%ep22 = ep3 ;
            new_grid%sm21 = sm1 ; new_grid%sm22 = sm3 ;
            new_grid%em21 = em1 ; new_grid%em22 = em3 ;
            new_grid%sd11 = sd1
            new_grid%ed11 = ed1
            new_grid%sp11 = sp1
            new_grid%ep11 = ep1
            new_grid%sm11 = sm1
            new_grid%em11 = em1
         CASE  ( DATA_ORDER_YZX )
            new_grid%sd21 = sd1 ; new_grid%sd22 = sd3 ;
            new_grid%ed21 = ed1 ; new_grid%ed22 = ed3 ;
            new_grid%sp21 = sp1 ; new_grid%sp22 = sp3 ;
            new_grid%ep21 = ep1 ; new_grid%ep22 = ep3 ;
            new_grid%sm21 = sm1 ; new_grid%sm22 = sm3 ;
            new_grid%em21 = em1 ; new_grid%em22 = em3 ;
            new_grid%sd11 = sd1
            new_grid%ed11 = ed1
            new_grid%sp11 = sp1
            new_grid%ep11 = ep1
            new_grid%sm11 = sm1
            new_grid%em11 = em1
      END SELECT

      CALL med_add_config_info_to_grid ( new_grid )           ! this is a mediation layer routine

! Some miscellaneous state that is in the Registry but not namelist data

      new_grid%tiled                           = .false.
      new_grid%patched                         = .false.
      NULLIFY(new_grid%mapping)

! This next set of includes causes all but the namelist_derived variables to be
! properly assigned to the new_grid record

      grid => new_grid

      CALL wrf_get_dm_communicator ( grid%communicator )
      CALL wrf_dm_define_comms( grid )

   END SUBROUTINE alloc_and_configure_domain

!

!  This routine ALLOCATEs the required space for the meteorological fields
!  for a specific domain.  The fields are simply ALLOCATEd as an -1.  They
!  are referenced as wind, temperature, moisture, etc. in routines that are
!  below this top-level of data allocation and management (in the solve routine
!  and below).

   SUBROUTINE alloc_space_field ( grid,   id, setinitval_in ,  tl_in , inter_domain_in ,   &
                                  sd31, ed31, sd32, ed32, sd33, ed33, &
                                  sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                  sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                  sm31y, em31y, sm32y, em32y, sm33y, em33y )

      
      USE module_configure
      IMPLICIT NONE
 

      !  Input data.

      TYPE(domain)               , POINTER          :: grid
      INTEGER , INTENT(IN)            :: id
      INTEGER , INTENT(IN)            :: setinitval_in   ! 3 = everything, 1 = arrays only, 0 = none
      INTEGER , INTENT(IN)            :: sd31, ed31, sd32, ed32, sd33, ed33
      INTEGER , INTENT(IN)            :: sm31, em31, sm32, em32, sm33, em33
      INTEGER , INTENT(IN)            :: sm31x, em31x, sm32x, em32x, sm33x, em33x
      INTEGER , INTENT(IN)            :: sm31y, em31y, sm32y, em32y, sm33y, em33y

      ! this argument is a bitmask. First bit is time level 1, second is time level 2, and so on.
      ! e.g. to set both 1st and second time level, use 3
      !      to set only 1st                        use 1
      !      to set only 2st                        use 2
      INTEGER , INTENT(IN)            :: tl_in
  
      ! true if the allocation is for an intermediate domain (for nesting); only certain fields allocated
      ! false otherwise (all allocated, modulo tl above)
      LOGICAL , INTENT(IN)            :: inter_domain_in

      !  Local data.
      INTEGER dyn_opt, idum1, idum2, spec_bdy_width
      INTEGER num_bytes_allocated
      REAL    initial_data_value
      CHARACTER (LEN=256) message
      INTEGER tl
      LOGICAL inter_domain
      INTEGER setinitval

      !declare ierr variable for error checking ALLOCATE calls
      INTEGER ierr

      INTEGER                              :: loop

      tl = tl_in
      inter_domain = inter_domain_in

      CALL get_initial_data_value ( initial_data_value )

      setinitval = setinitval_in

      CALL nl_get_dyn_opt( 1, dyn_opt )
      CALL nl_get_spec_bdy_width( 1, spec_bdy_width )

      CALL set_scalar_indices_from_config( id , idum1 , idum2 )

      num_bytes_allocated = 0 


      IF ( dyn_opt == DYN_NODYN ) THEN

        IF ( grid%id .EQ. 1 ) &
          CALL wrf_message ( 'DYNAMICS OPTION: dynamics disabled ' )

        WRITE(message,*)                                        &
          "To run the the NODYN option, recompile ",            &
          "-DALLOW_NODYN in ARCHFLAGS settings of configure.wrf"
        CALL wrf_error_fatal3 ( "module_domain.b" , 1074 ,  message )

      ELSE IF ( dyn_opt == DYN_NMM ) THEN
        IF ( grid%id .EQ. 1 ) &
          CALL wrf_message ( 'DYNAMICS OPTION: nmm dyncore' )
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/nmm_allocs.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
ALLOCATE(grid%lu_index(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%lu_index(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%lu_index=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%lu_mask(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%lu_mask(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%lu_mask=initial_data_value
ELSE
ALLOCATE(grid%lu_mask(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%lu_mask(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_p_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_p_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_p_gc=initial_data_value
ELSE
ALLOCATE(grid%nmm_p_gc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_p_gc(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%vegcat(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%vegcat(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%vegcat=initial_data_value
ELSE
ALLOCATE(grid%vegcat(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%vegcat(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilcat(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilcat(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilcat=initial_data_value
ELSE
ALLOCATE(grid%soilcat(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilcat(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%input_soil_cat(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%input_soil_cat(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%input_soil_cat=initial_data_value
ELSE
ALLOCATE(grid%input_soil_cat(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%input_soil_cat(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_tsk_gc(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_tsk_gc(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_tsk_gc=initial_data_value
ELSE
ALLOCATE(grid%nmm_tsk_gc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_tsk_gc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%xice_gc(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xice_gc(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xice_gc=initial_data_value
ELSE
ALLOCATE(grid%xice_gc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xice_gc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_ght_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ght_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_ght_gc=initial_data_value
ELSE
ALLOCATE(grid%nmm_ght_gc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ght_gc(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_rh_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rh_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_rh_gc=initial_data_value
ELSE
ALLOCATE(grid%nmm_rh_gc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rh_gc(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_v_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_v_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_v_gc=initial_data_value
ELSE
ALLOCATE(grid%nmm_v_gc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_v_gc(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_u_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_u_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_u_gc=initial_data_value
ELSE
ALLOCATE(grid%nmm_u_gc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_u_gc(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_t_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_t_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_t_gc=initial_data_value
ELSE
ALLOCATE(grid%nmm_t_gc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_t_gc(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_rwmr_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rwmr_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_rwmr_gc=initial_data_value
ELSE
ALLOCATE(grid%nmm_rwmr_gc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rwmr_gc(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_snmr_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_snmr_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_snmr_gc=initial_data_value
ELSE
ALLOCATE(grid%nmm_snmr_gc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_snmr_gc(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_clwmr_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_clwmr_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_clwmr_gc=initial_data_value
ELSE
ALLOCATE(grid%nmm_clwmr_gc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_clwmr_gc(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_cice_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cice_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_cice_gc=initial_data_value
ELSE
ALLOCATE(grid%nmm_cice_gc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cice_gc(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_rimef_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rimef_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_rimef_gc=initial_data_value
ELSE
ALLOCATE(grid%nmm_rimef_gc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rimef_gc(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%snoalb(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%snoalb(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%snoalb=initial_data_value
ELSE
ALLOCATE(grid%snoalb(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%snoalb(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_greenfrac_gc(sm31:em31,sm32:em32,1:12),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_greenfrac_gc(sm31:em31,sm32:em32,1:12). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_greenfrac_gc=initial_data_value
ELSE
ALLOCATE(grid%nmm_greenfrac_gc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_greenfrac_gc(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_albedo12m_gc(sm31:em31,sm32:em32,1:12),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_albedo12m_gc(sm31:em31,sm32:em32,1:12). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_albedo12m_gc=initial_data_value
ELSE
ALLOCATE(grid%nmm_albedo12m_gc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_albedo12m_gc(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilcbot_gc(sm31:em31,sm32:em32,1:model_config_rec%num_soil_cat),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilcbot_gc(sm31:em31,sm32:em32,1:model_config_rec%num_soil_cat). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilcbot_gc=initial_data_value
ELSE
ALLOCATE(grid%soilcbot_gc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilcbot_gc(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilctop_gc(sm31:em31,sm32:em32,1:model_config_rec%num_soil_cat),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilctop_gc(sm31:em31,sm32:em32,1:model_config_rec%num_soil_cat). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilctop_gc=initial_data_value
ELSE
ALLOCATE(grid%soilctop_gc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilctop_gc(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_tmn_gc(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_tmn_gc(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_tmn_gc=initial_data_value
ELSE
ALLOCATE(grid%nmm_tmn_gc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_tmn_gc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_htv_gc(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_htv_gc(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_htv_gc=initial_data_value
ELSE
ALLOCATE(grid%nmm_htv_gc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_htv_gc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_ht_gc(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ht_gc(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_ht_gc=initial_data_value
ELSE
ALLOCATE(grid%nmm_ht_gc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ht_gc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%landusef_gc(sm31:em31,sm32:em32,1:model_config_rec%num_land_cat),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%landusef_gc(sm31:em31,sm32:em32,1:model_config_rec%num_land_cat). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%landusef_gc=initial_data_value
ELSE
ALLOCATE(grid%landusef_gc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%landusef_gc(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_vlon_gc(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_vlon_gc(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_vlon_gc=initial_data_value
ELSE
ALLOCATE(grid%nmm_vlon_gc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_vlon_gc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_vlat_gc(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_vlat_gc(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_vlat_gc=initial_data_value
ELSE
ALLOCATE(grid%nmm_vlat_gc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_vlat_gc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_hlon_gc(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_hlon_gc(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_hlon_gc=initial_data_value
ELSE
ALLOCATE(grid%nmm_hlon_gc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_hlon_gc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_hlat_gc(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_hlat_gc(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_hlat_gc=initial_data_value
ELSE
ALLOCATE(grid%nmm_hlat_gc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_hlat_gc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_hbm2(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_hbm2(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_hbm2=initial_data_value
ELSE
ALLOCATE(grid%nmm_hbm2(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_hbm2(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_hbm3(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_hbm3(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_hbm3=initial_data_value
ELSE
ALLOCATE(grid%nmm_hbm3(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_hbm3(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_vbm2(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_vbm2(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_vbm2=initial_data_value
ELSE
ALLOCATE(grid%nmm_vbm2(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_vbm2(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_vbm3(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_vbm3(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_vbm3=initial_data_value
ELSE
ALLOCATE(grid%nmm_vbm3(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_vbm3(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_sm(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_sm(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_sm=initial_data_value
ELSE
ALLOCATE(grid%nmm_sm(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_sm(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_sice(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_sice(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_sice=initial_data_value
ELSE
ALLOCATE(grid%nmm_sice(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_sice(1,1).  ')
 endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%nmm_ntsd=0
IF ( setinitval .EQ. 3 ) grid%nmm_nstart_hour=0
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_pd(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_pd(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_pd=initial_data_value
ELSE
ALLOCATE(grid%nmm_pd(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_pd(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_pd_b(max(ed31,ed32),1,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_pd_b(max(ed31,ed32),1,spec_bdy_width,4). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_pd_b=initial_data_value
ELSE
ALLOCATE(grid%nmm_pd_b(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_pd_b(1,1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_pd_bt(max(ed31,ed32),1,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_pd_bt(max(ed31,ed32),1,spec_bdy_width,4). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_pd_bt=initial_data_value
ELSE
ALLOCATE(grid%nmm_pd_bt(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_pd_bt(1,1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_fis(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_fis(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_fis=initial_data_value
ELSE
ALLOCATE(grid%nmm_fis(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_fis(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_res(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_res(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_res=initial_data_value
ELSE
ALLOCATE(grid%nmm_res(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_res(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_t(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_t(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_t=initial_data_value
ELSE
ALLOCATE(grid%nmm_t(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_t(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_t_b(max(ed31,ed32),sd33:ed33,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_t_b(max(ed31,ed32),sd33:ed33,spec_bdy_width,4). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_t_b=initial_data_value
ELSE
ALLOCATE(grid%nmm_t_b(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_t_b(1,1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_t_bt(max(ed31,ed32),sd33:ed33,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_t_bt(max(ed31,ed32),sd33:ed33,spec_bdy_width,4). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_t_bt=initial_data_value
ELSE
ALLOCATE(grid%nmm_t_bt(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_t_bt(1,1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_q(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_q(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_q=initial_data_value
ELSE
ALLOCATE(grid%nmm_q(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_q(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_q_b(max(ed31,ed32),sd33:ed33,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_q_b(max(ed31,ed32),sd33:ed33,spec_bdy_width,4). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_q_b=initial_data_value
ELSE
ALLOCATE(grid%nmm_q_b(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_q_b(1,1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_q_bt(max(ed31,ed32),sd33:ed33,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_q_bt(max(ed31,ed32),sd33:ed33,spec_bdy_width,4). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_q_bt=initial_data_value
ELSE
ALLOCATE(grid%nmm_q_bt(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_q_bt(1,1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_u(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_u(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_u=initial_data_value
ELSE
ALLOCATE(grid%nmm_u(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_u(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_u_b(max(ed31,ed32),sd33:ed33,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_u_b(max(ed31,ed32),sd33:ed33,spec_bdy_width,4). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_u_b=initial_data_value
ELSE
ALLOCATE(grid%nmm_u_b(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_u_b(1,1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_u_bt(max(ed31,ed32),sd33:ed33,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_u_bt(max(ed31,ed32),sd33:ed33,spec_bdy_width,4). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_u_bt=initial_data_value
ELSE
ALLOCATE(grid%nmm_u_bt(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_u_bt(1,1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_v(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_v(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_v=initial_data_value
ELSE
ALLOCATE(grid%nmm_v(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_v(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_v_b(max(ed31,ed32),sd33:ed33,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_v_b(max(ed31,ed32),sd33:ed33,spec_bdy_width,4). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_v_b=initial_data_value
ELSE
ALLOCATE(grid%nmm_v_b(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_v_b(1,1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_v_bt(max(ed31,ed32),sd33:ed33,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_v_bt(max(ed31,ed32),sd33:ed33,spec_bdy_width,4). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_v_bt=initial_data_value
ELSE
ALLOCATE(grid%nmm_v_bt(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_v_bt(1,1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_told(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_told(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_told=initial_data_value
ELSE
ALLOCATE(grid%nmm_told(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_told(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_uold(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_uold(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_uold=initial_data_value
ELSE
ALLOCATE(grid%nmm_uold(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_uold(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_vold(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_vold(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_vold=initial_data_value
ELSE
ALLOCATE(grid%nmm_vold(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_vold(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_dx_nmm(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_dx_nmm(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_dx_nmm=initial_data_value
ELSE
ALLOCATE(grid%nmm_dx_nmm(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_dx_nmm(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_wpdar(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_wpdar(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_wpdar=initial_data_value
ELSE
ALLOCATE(grid%nmm_wpdar(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_wpdar(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_cpgfu(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cpgfu(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_cpgfu=initial_data_value
ELSE
ALLOCATE(grid%nmm_cpgfu(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cpgfu(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_curv(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_curv(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_curv=initial_data_value
ELSE
ALLOCATE(grid%nmm_curv(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_curv(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_fcp(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_fcp(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_fcp=initial_data_value
ELSE
ALLOCATE(grid%nmm_fcp(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_fcp(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_fdiv(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_fdiv(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_fdiv=initial_data_value
ELSE
ALLOCATE(grid%nmm_fdiv(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_fdiv(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_f(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_f(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_f=initial_data_value
ELSE
ALLOCATE(grid%nmm_f(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_f(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_fad(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_fad(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_fad=initial_data_value
ELSE
ALLOCATE(grid%nmm_fad(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_fad(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_ddmpu(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ddmpu(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_ddmpu=initial_data_value
ELSE
ALLOCATE(grid%nmm_ddmpu(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ddmpu(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_ddmpv(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ddmpv(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_ddmpv=initial_data_value
ELSE
ALLOCATE(grid%nmm_ddmpv(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ddmpv(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_deta(sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_deta(sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_deta=initial_data_value
ELSE
ALLOCATE(grid%nmm_deta(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_deta(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_rdeta(sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rdeta(sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_rdeta=initial_data_value
ELSE
ALLOCATE(grid%nmm_rdeta(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rdeta(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_aeta(sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_aeta(sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_aeta=initial_data_value
ELSE
ALLOCATE(grid%nmm_aeta(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_aeta(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_f4q2(sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_f4q2(sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_f4q2=initial_data_value
ELSE
ALLOCATE(grid%nmm_f4q2(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_f4q2(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_etax(sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_etax(sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_etax=initial_data_value
ELSE
ALLOCATE(grid%nmm_etax(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_etax(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_dfl(sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_dfl(sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_dfl=initial_data_value
ELSE
ALLOCATE(grid%nmm_dfl(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_dfl(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_deta1(sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_deta1(sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_deta1=initial_data_value
ELSE
ALLOCATE(grid%nmm_deta1(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_deta1(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_aeta1(sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_aeta1(sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_aeta1=initial_data_value
ELSE
ALLOCATE(grid%nmm_aeta1(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_aeta1(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_eta1(sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_eta1(sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_eta1=initial_data_value
ELSE
ALLOCATE(grid%nmm_eta1(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_eta1(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_deta2(sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_deta2(sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_deta2=initial_data_value
ELSE
ALLOCATE(grid%nmm_deta2(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_deta2(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_aeta2(sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_aeta2(sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_aeta2=initial_data_value
ELSE
ALLOCATE(grid%nmm_aeta2(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_aeta2(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_eta2(sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_eta2(sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_eta2=initial_data_value
ELSE
ALLOCATE(grid%nmm_eta2(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_eta2(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_em(1:2600),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_em(1:2600). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_em=initial_data_value
ELSE
ALLOCATE(grid%nmm_em(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_em(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_emt(1:2600),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_emt(1:2600). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_emt=initial_data_value
ELSE
ALLOCATE(grid%nmm_emt(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_emt(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_adt(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_adt(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_adt=initial_data_value
ELSE
ALLOCATE(grid%nmm_adt(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_adt(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_adu(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_adu(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_adu=initial_data_value
ELSE
ALLOCATE(grid%nmm_adu(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_adu(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_adv(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_adv(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_adv=initial_data_value
ELSE
ALLOCATE(grid%nmm_adv(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_adv(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_em_loc(1:2600),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_em_loc(1:2600). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_em_loc=initial_data_value
ELSE
ALLOCATE(grid%nmm_em_loc(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_em_loc(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_emt_loc(1:2600),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_emt_loc(1:2600). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_emt_loc=initial_data_value
ELSE
ALLOCATE(grid%nmm_emt_loc(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_emt_loc(1).  ')
 endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%nmm_dy_nmm=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nmm_cpgfv=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nmm_en=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nmm_ent=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nmm_f4d=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nmm_f4q=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nmm_ef4t=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nmm_upstrm=.FALSE.
IF ( setinitval .EQ. 3 ) grid%nmm_dlmd=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nmm_dphd=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nmm_pdtop=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nmm_pt=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_pdsl(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_pdsl(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_pdsl=initial_data_value
ELSE
ALLOCATE(grid%nmm_pdsl(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_pdsl(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_pdslo(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_pdslo(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_pdslo=initial_data_value
ELSE
ALLOCATE(grid%nmm_pdslo(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_pdslo(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_psdt(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_psdt(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_psdt=initial_data_value
ELSE
ALLOCATE(grid%nmm_psdt(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_psdt(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_div(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_div(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_div=initial_data_value
ELSE
ALLOCATE(grid%nmm_div(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_div(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_few(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_few(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_few=initial_data_value
ELSE
ALLOCATE(grid%nmm_few(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_few(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_fne(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_fne(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_fne=initial_data_value
ELSE
ALLOCATE(grid%nmm_fne(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_fne(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_fns(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_fns(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_fns=initial_data_value
ELSE
ALLOCATE(grid%nmm_fns(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_fns(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_fse(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_fse(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_fse=initial_data_value
ELSE
ALLOCATE(grid%nmm_fse(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_fse(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_omgalf(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_omgalf(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_omgalf=initial_data_value
ELSE
ALLOCATE(grid%nmm_omgalf(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_omgalf(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_petdt(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_petdt(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_petdt=initial_data_value
ELSE
ALLOCATE(grid%nmm_petdt(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_petdt(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_rtop(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rtop(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_rtop=initial_data_value
ELSE
ALLOCATE(grid%nmm_rtop(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rtop(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_pblh(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_pblh(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_pblh=initial_data_value
ELSE
ALLOCATE(grid%nmm_pblh(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_pblh(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_lpbl(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_lpbl(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_lpbl=0
ELSE
ALLOCATE(grid%nmm_lpbl(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_lpbl(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_ustar(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ustar(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_ustar=initial_data_value
ELSE
ALLOCATE(grid%nmm_ustar(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ustar(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_z0(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_z0(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_z0=initial_data_value
ELSE
ALLOCATE(grid%nmm_z0(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_z0(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_z0base(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_z0base(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_z0base=initial_data_value
ELSE
ALLOCATE(grid%nmm_z0base(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_z0base(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_ths(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ths(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_ths=initial_data_value
ELSE
ALLOCATE(grid%nmm_ths(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ths(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_mavail(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_mavail(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_mavail=initial_data_value
ELSE
ALLOCATE(grid%nmm_mavail(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_mavail(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_qsh(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_qsh(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_qsh=initial_data_value
ELSE
ALLOCATE(grid%nmm_qsh(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_qsh(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_twbs(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_twbs(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_twbs=initial_data_value
ELSE
ALLOCATE(grid%nmm_twbs(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_twbs(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_qwbs(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_qwbs(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_qwbs=initial_data_value
ELSE
ALLOCATE(grid%nmm_qwbs(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_qwbs(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_prec(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_prec(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_prec=initial_data_value
ELSE
ALLOCATE(grid%nmm_prec(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_prec(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_aprec(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_aprec(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_aprec=initial_data_value
ELSE
ALLOCATE(grid%nmm_aprec(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_aprec(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_acprec(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_acprec(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_acprec=initial_data_value
ELSE
ALLOCATE(grid%nmm_acprec(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_acprec(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_cuprec(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cuprec(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_cuprec=initial_data_value
ELSE
ALLOCATE(grid%nmm_cuprec(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cuprec(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_lspa(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_lspa(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_lspa=initial_data_value
ELSE
ALLOCATE(grid%nmm_lspa(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_lspa(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_ddata(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ddata(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_ddata=initial_data_value
ELSE
ALLOCATE(grid%nmm_ddata(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ddata(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_accliq(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_accliq(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_accliq=initial_data_value
ELSE
ALLOCATE(grid%nmm_accliq(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_accliq(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_sno(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_sno(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_sno=initial_data_value
ELSE
ALLOCATE(grid%nmm_sno(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_sno(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_si(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_si(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_si=initial_data_value
ELSE
ALLOCATE(grid%nmm_si(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_si(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_cldefi(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cldefi(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_cldefi=initial_data_value
ELSE
ALLOCATE(grid%nmm_cldefi(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cldefi(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_deep(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_deep(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_deep=initial_data_value
ELSE
ALLOCATE(grid%nmm_deep(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_deep(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_rf(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rf(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_rf=initial_data_value
ELSE
ALLOCATE(grid%nmm_rf(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rf(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_th10(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_th10(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_th10=initial_data_value
ELSE
ALLOCATE(grid%nmm_th10(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_th10(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_q10(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_q10(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_q10=initial_data_value
ELSE
ALLOCATE(grid%nmm_q10(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_q10(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_pshltr(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_pshltr(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_pshltr=initial_data_value
ELSE
ALLOCATE(grid%nmm_pshltr(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_pshltr(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_tshltr(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_tshltr(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_tshltr=initial_data_value
ELSE
ALLOCATE(grid%nmm_tshltr(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_tshltr(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_qshltr(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_qshltr(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_qshltr=initial_data_value
ELSE
ALLOCATE(grid%nmm_qshltr(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_qshltr(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_q2(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_q2(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_q2=initial_data_value
ELSE
ALLOCATE(grid%nmm_q2(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_q2(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_q2_b(max(ed31,ed32),sd33:ed33,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_q2_b(max(ed31,ed32),sd33:ed33,spec_bdy_width,4). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_q2_b=initial_data_value
ELSE
ALLOCATE(grid%nmm_q2_b(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_q2_b(1,1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_q2_bt(max(ed31,ed32),sd33:ed33,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_q2_bt(max(ed31,ed32),sd33:ed33,spec_bdy_width,4). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_q2_bt=initial_data_value
ELSE
ALLOCATE(grid%nmm_q2_bt(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_q2_bt(1,1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_t_adj(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_t_adj(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_t_adj=initial_data_value
ELSE
ALLOCATE(grid%nmm_t_adj(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_t_adj(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_t_old(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_t_old(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_t_old=initial_data_value
ELSE
ALLOCATE(grid%nmm_t_old(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_t_old(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_zero_3d(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_zero_3d(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_zero_3d=initial_data_value
ELSE
ALLOCATE(grid%nmm_zero_3d(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_zero_3d(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_w0avg(sm31:em31,sm33:em33,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_w0avg(sm31:em31,sm33:em33,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_w0avg=initial_data_value
ELSE
ALLOCATE(grid%nmm_w0avg(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_w0avg(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_akhs_out(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_akhs_out(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_akhs_out=initial_data_value
ELSE
ALLOCATE(grid%nmm_akhs_out(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_akhs_out(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_akms_out(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_akms_out(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_akms_out=initial_data_value
ELSE
ALLOCATE(grid%nmm_akms_out(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_akms_out(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_albase(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_albase(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_albase=initial_data_value
ELSE
ALLOCATE(grid%nmm_albase(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_albase(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_albedo(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_albedo(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_albedo=initial_data_value
ELSE
ALLOCATE(grid%nmm_albedo(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_albedo(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_cnvbot(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cnvbot(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_cnvbot=initial_data_value
ELSE
ALLOCATE(grid%nmm_cnvbot(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cnvbot(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_cnvtop(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cnvtop(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_cnvtop=initial_data_value
ELSE
ALLOCATE(grid%nmm_cnvtop(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cnvtop(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_czen(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_czen(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_czen=initial_data_value
ELSE
ALLOCATE(grid%nmm_czen(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_czen(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_czmean(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_czmean(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_czmean=initial_data_value
ELSE
ALLOCATE(grid%nmm_czmean(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_czmean(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_epsr(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_epsr(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_epsr=initial_data_value
ELSE
ALLOCATE(grid%nmm_epsr(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_epsr(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_gffc(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_gffc(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_gffc=initial_data_value
ELSE
ALLOCATE(grid%nmm_gffc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_gffc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_glat(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_glat(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_glat=initial_data_value
ELSE
ALLOCATE(grid%nmm_glat(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_glat(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_glon(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_glon(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_glon=initial_data_value
ELSE
ALLOCATE(grid%nmm_glon(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_glon(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_nmm_tsk(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_nmm_tsk(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_nmm_tsk=initial_data_value
ELSE
ALLOCATE(grid%nmm_nmm_tsk(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_nmm_tsk(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_hdac(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_hdac(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_hdac=initial_data_value
ELSE
ALLOCATE(grid%nmm_hdac(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_hdac(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_hdacv(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_hdacv(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_hdacv=initial_data_value
ELSE
ALLOCATE(grid%nmm_hdacv(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_hdacv(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_mxsnal(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_mxsnal(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_mxsnal=initial_data_value
ELSE
ALLOCATE(grid%nmm_mxsnal(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_mxsnal(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_radin(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_radin(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_radin=initial_data_value
ELSE
ALLOCATE(grid%nmm_radin(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_radin(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_radot(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_radot(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_radot=initial_data_value
ELSE
ALLOCATE(grid%nmm_radot(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_radot(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_sigt4(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_sigt4(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_sigt4=initial_data_value
ELSE
ALLOCATE(grid%nmm_sigt4(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_sigt4(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_tg(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_tg(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_tg=initial_data_value
ELSE
ALLOCATE(grid%nmm_tg(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_tg(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_dfrlg(sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_dfrlg(sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_dfrlg=initial_data_value
ELSE
ALLOCATE(grid%nmm_dfrlg(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_dfrlg(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_lvl(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_lvl(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_lvl=0
ELSE
ALLOCATE(grid%nmm_lvl(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_lvl(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_cwm(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cwm(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_cwm=initial_data_value
ELSE
ALLOCATE(grid%nmm_cwm(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cwm(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_cwm_b(max(ed31,ed32),sd33:ed33,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cwm_b(max(ed31,ed32),sd33:ed33,spec_bdy_width,4). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_cwm_b=initial_data_value
ELSE
ALLOCATE(grid%nmm_cwm_b(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cwm_b(1,1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_cwm_bt(max(ed31,ed32),sd33:ed33,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cwm_bt(max(ed31,ed32),sd33:ed33,spec_bdy_width,4). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_cwm_bt=initial_data_value
ELSE
ALLOCATE(grid%nmm_cwm_bt(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cwm_bt(1,1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_f_ice(sm31:em31,sm33:em33,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_f_ice(sm31:em31,sm33:em33,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_f_ice=initial_data_value
ELSE
ALLOCATE(grid%nmm_f_ice(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_f_ice(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_f_rain(sm31:em31,sm33:em33,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_f_rain(sm31:em31,sm33:em33,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_f_rain=initial_data_value
ELSE
ALLOCATE(grid%nmm_f_rain(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_f_rain(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_f_rimef(sm31:em31,sm33:em33,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_f_rimef(sm31:em31,sm33:em33,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_f_rimef=initial_data_value
ELSE
ALLOCATE(grid%nmm_f_rimef(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_f_rimef(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_cldfra(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cldfra(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_cldfra=initial_data_value
ELSE
ALLOCATE(grid%nmm_cldfra(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cldfra(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_sr(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_sr(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_sr=initial_data_value
ELSE
ALLOCATE(grid%nmm_sr(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_sr(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_cfrach(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cfrach(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_cfrach=initial_data_value
ELSE
ALLOCATE(grid%nmm_cfrach(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cfrach(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_cfracl(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cfracl(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_cfracl=initial_data_value
ELSE
ALLOCATE(grid%nmm_cfracl(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cfracl(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_cfracm(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cfracm(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_cfracm=initial_data_value
ELSE
ALLOCATE(grid%nmm_cfracm(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cfracm(1,1).  ')
 endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%nmm_micro_start=.FALSE.
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_islope(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_islope(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_islope=0
ELSE
ALLOCATE(grid%nmm_islope(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_islope(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_dzsoil(sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_dzsoil(sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_dzsoil=initial_data_value
ELSE
ALLOCATE(grid%nmm_dzsoil(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_dzsoil(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_rtdpth(sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rtdpth(sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_rtdpth=initial_data_value
ELSE
ALLOCATE(grid%nmm_rtdpth(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rtdpth(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_sldpth(sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_sldpth(sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_sldpth=initial_data_value
ELSE
ALLOCATE(grid%nmm_sldpth(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_sldpth(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_cmc(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cmc(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_cmc=initial_data_value
ELSE
ALLOCATE(grid%nmm_cmc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cmc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_grnflx(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_grnflx(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_grnflx=initial_data_value
ELSE
ALLOCATE(grid%nmm_grnflx(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_grnflx(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_pctsno(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_pctsno(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_pctsno=initial_data_value
ELSE
ALLOCATE(grid%nmm_pctsno(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_pctsno(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_soiltb(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_soiltb(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_soiltb=initial_data_value
ELSE
ALLOCATE(grid%nmm_soiltb(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_soiltb(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_vegfrc(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_vegfrc(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_vegfrc=initial_data_value
ELSE
ALLOCATE(grid%nmm_vegfrc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_vegfrc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_shdmin(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_shdmin(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_shdmin=initial_data_value
ELSE
ALLOCATE(grid%nmm_shdmin(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_shdmin(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_shdmax(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_shdmax(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_shdmax=initial_data_value
ELSE
ALLOCATE(grid%nmm_shdmax(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_shdmax(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_sh2o(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_sh2o(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_sh2o=initial_data_value
ELSE
ALLOCATE(grid%nmm_sh2o(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_sh2o(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_smc(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_smc(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_smc=initial_data_value
ELSE
ALLOCATE(grid%nmm_smc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_smc(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_stc(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_stc(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_stc=initial_data_value
ELSE
ALLOCATE(grid%nmm_stc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_stc(1,1,1).  ')
 endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%nmm_hydro=.FALSE.
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_dwdtmn(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_dwdtmn(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_dwdtmn=initial_data_value
ELSE
ALLOCATE(grid%nmm_dwdtmn(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_dwdtmn(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_dwdtmx(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_dwdtmx(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_dwdtmx=initial_data_value
ELSE
ALLOCATE(grid%nmm_dwdtmx(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_dwdtmx(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_dwdt(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_dwdt(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_dwdt=initial_data_value
ELSE
ALLOCATE(grid%nmm_dwdt(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_dwdt(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_pdwdt(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_pdwdt(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_pdwdt=initial_data_value
ELSE
ALLOCATE(grid%nmm_pdwdt(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_pdwdt(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_pint(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_pint(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_pint=initial_data_value
ELSE
ALLOCATE(grid%nmm_pint(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_pint(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_w(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_w(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_w=initial_data_value
ELSE
ALLOCATE(grid%nmm_w(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_w(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_z(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_z(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_z=initial_data_value
ELSE
ALLOCATE(grid%nmm_z(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_z(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_acfrcv(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_acfrcv(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_acfrcv=initial_data_value
ELSE
ALLOCATE(grid%nmm_acfrcv(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_acfrcv(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_acfrst(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_acfrst(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_acfrst=initial_data_value
ELSE
ALLOCATE(grid%nmm_acfrst(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_acfrst(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_ssroff(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ssroff(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_ssroff=initial_data_value
ELSE
ALLOCATE(grid%nmm_ssroff(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ssroff(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_bgroff(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_bgroff(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_bgroff=initial_data_value
ELSE
ALLOCATE(grid%nmm_bgroff(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_bgroff(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_rlwin(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rlwin(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_rlwin=initial_data_value
ELSE
ALLOCATE(grid%nmm_rlwin(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rlwin(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_rlwout(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rlwout(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_rlwout=initial_data_value
ELSE
ALLOCATE(grid%nmm_rlwout(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rlwout(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_rlwtoa(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rlwtoa(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_rlwtoa=initial_data_value
ELSE
ALLOCATE(grid%nmm_rlwtoa(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rlwtoa(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_alwin(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_alwin(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_alwin=initial_data_value
ELSE
ALLOCATE(grid%nmm_alwin(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_alwin(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_alwout(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_alwout(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_alwout=initial_data_value
ELSE
ALLOCATE(grid%nmm_alwout(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_alwout(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_alwtoa(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_alwtoa(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_alwtoa=initial_data_value
ELSE
ALLOCATE(grid%nmm_alwtoa(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_alwtoa(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_rswin(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rswin(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_rswin=initial_data_value
ELSE
ALLOCATE(grid%nmm_rswin(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rswin(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_rswinc(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rswinc(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_rswinc=initial_data_value
ELSE
ALLOCATE(grid%nmm_rswinc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rswinc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_rswout(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rswout(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_rswout=initial_data_value
ELSE
ALLOCATE(grid%nmm_rswout(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rswout(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_rswtoa(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rswtoa(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_rswtoa=initial_data_value
ELSE
ALLOCATE(grid%nmm_rswtoa(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rswtoa(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_aswin(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_aswin(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_aswin=initial_data_value
ELSE
ALLOCATE(grid%nmm_aswin(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_aswin(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_aswout(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_aswout(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_aswout=initial_data_value
ELSE
ALLOCATE(grid%nmm_aswout(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_aswout(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_aswtoa(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_aswtoa(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_aswtoa=initial_data_value
ELSE
ALLOCATE(grid%nmm_aswtoa(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_aswtoa(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_sfcshx(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_sfcshx(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_sfcshx=initial_data_value
ELSE
ALLOCATE(grid%nmm_sfcshx(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_sfcshx(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_sfclhx(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_sfclhx(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_sfclhx=initial_data_value
ELSE
ALLOCATE(grid%nmm_sfclhx(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_sfclhx(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_subshx(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_subshx(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_subshx=initial_data_value
ELSE
ALLOCATE(grid%nmm_subshx(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_subshx(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_snopcx(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_snopcx(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_snopcx=initial_data_value
ELSE
ALLOCATE(grid%nmm_snopcx(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_snopcx(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_sfcuvx(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_sfcuvx(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_sfcuvx=initial_data_value
ELSE
ALLOCATE(grid%nmm_sfcuvx(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_sfcuvx(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_potevp(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_potevp(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_potevp=initial_data_value
ELSE
ALLOCATE(grid%nmm_potevp(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_potevp(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_potflx(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_potflx(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_potflx=initial_data_value
ELSE
ALLOCATE(grid%nmm_potflx(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_potflx(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_tlmin(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_tlmin(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_tlmin=initial_data_value
ELSE
ALLOCATE(grid%nmm_tlmin(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_tlmin(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_tlmax(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_tlmax(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_tlmax=initial_data_value
ELSE
ALLOCATE(grid%nmm_tlmax(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_tlmax(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_rlwtt(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rlwtt(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_rlwtt=initial_data_value
ELSE
ALLOCATE(grid%nmm_rlwtt(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rlwtt(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_rswtt(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rswtt(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_rswtt=initial_data_value
ELSE
ALLOCATE(grid%nmm_rswtt(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rswtt(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_tcucn(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_tcucn(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_tcucn=initial_data_value
ELSE
ALLOCATE(grid%nmm_tcucn(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_tcucn(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_train(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_train(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_train=initial_data_value
ELSE
ALLOCATE(grid%nmm_train(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_train(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_ncfrcv(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ncfrcv(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_ncfrcv=0
ELSE
ALLOCATE(grid%nmm_ncfrcv(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ncfrcv(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_ncfrst(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ncfrst(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_ncfrst=0
ELSE
ALLOCATE(grid%nmm_ncfrst(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ncfrst(1,1).  ')
 endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%nmm_nphs0=0
IF ( setinitval .EQ. 3 ) grid%nmm_nprec=0
IF ( setinitval .EQ. 3 ) grid%nmm_nclod=0
IF ( setinitval .EQ. 3 ) grid%nmm_nheat=0
IF ( setinitval .EQ. 3 ) grid%nmm_nrdlw=0
IF ( setinitval .EQ. 3 ) grid%nmm_nrdsw=0
IF ( setinitval .EQ. 3 ) grid%nmm_nsrfc=0
IF ( setinitval .EQ. 3 ) grid%nmm_avrain=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nmm_avcnvc=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nmm_aratim=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nmm_acutim=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nmm_ardlw=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nmm_ardsw=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nmm_asrfc=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nmm_aphtim=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_ihe(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ihe(sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_ihe=0
ELSE
ALLOCATE(grid%nmm_ihe(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ihe(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_ihw(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ihw(sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_ihw=0
ELSE
ALLOCATE(grid%nmm_ihw(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ihw(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_ive(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ive(sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_ive=0
ELSE
ALLOCATE(grid%nmm_ive(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ive(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_ivw(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ivw(sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_ivw=0
ELSE
ALLOCATE(grid%nmm_ivw(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ivw(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_irad(sm31:em31),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_irad(sm31:em31). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_irad=0
ELSE
ALLOCATE(grid%nmm_irad(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_irad(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_iheg(1:2600),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_iheg(1:2600). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_iheg=0
ELSE
ALLOCATE(grid%nmm_iheg(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_iheg(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_ihwg(1:2600),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ihwg(1:2600). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_ihwg=0
ELSE
ALLOCATE(grid%nmm_ihwg(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ihwg(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_iveg(1:2600),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_iveg(1:2600). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_iveg=0
ELSE
ALLOCATE(grid%nmm_iveg(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_iveg(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_ivwg(1:2600),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ivwg(1:2600). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_ivwg=0
ELSE
ALLOCATE(grid%nmm_ivwg(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ivwg(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_iradg(1:2000),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_iradg(1:2000). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_iradg=0
ELSE
ALLOCATE(grid%nmm_iradg(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_iradg(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_n_iup_h(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_n_iup_h(sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_n_iup_h=0
ELSE
ALLOCATE(grid%nmm_n_iup_h(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_n_iup_h(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_n_iup_v(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_n_iup_v(sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_n_iup_v=0
ELSE
ALLOCATE(grid%nmm_n_iup_v(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_n_iup_v(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_n_iup_adh(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_n_iup_adh(sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_n_iup_adh=0
ELSE
ALLOCATE(grid%nmm_n_iup_adh(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_n_iup_adh(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_n_iup_adv(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_n_iup_adv(sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_n_iup_adv=0
ELSE
ALLOCATE(grid%nmm_n_iup_adv(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_n_iup_adv(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_iup_h(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_iup_h(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_iup_h=0
ELSE
ALLOCATE(grid%nmm_iup_h(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_iup_h(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_iup_v(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_iup_v(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_iup_v=0
ELSE
ALLOCATE(grid%nmm_iup_v(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_iup_v(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_iup_adh(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_iup_adh(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_iup_adh=0
ELSE
ALLOCATE(grid%nmm_iup_adh(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_iup_adh(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_iup_adv(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_iup_adv(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_iup_adv=0
ELSE
ALLOCATE(grid%nmm_iup_adv(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_iup_adv(1,1).  ')
 endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%imicrogram=0
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%imask_nostag(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%imask_nostag(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%imask_nostag=0
ELSE
ALLOCATE(grid%imask_nostag(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%imask_nostag(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%imask_xstag(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%imask_xstag(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%imask_xstag=0
ELSE
ALLOCATE(grid%imask_xstag(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%imask_xstag(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%imask_ystag(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%imask_ystag(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%imask_ystag=0
ELSE
ALLOCATE(grid%imask_ystag(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%imask_ystag(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%imask_xystag(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%imask_xystag(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%imask_xystag=0
ELSE
ALLOCATE(grid%imask_xystag(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%imask_xystag(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%sm000007(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm000007(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sm000007=initial_data_value
ELSE
ALLOCATE(grid%sm000007(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm000007(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%sm007028(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm007028(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sm007028=initial_data_value
ELSE
ALLOCATE(grid%sm007028(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm007028(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%sm028100(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm028100(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sm028100=initial_data_value
ELSE
ALLOCATE(grid%sm028100(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm028100(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%sm100255(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm100255(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sm100255=initial_data_value
ELSE
ALLOCATE(grid%sm100255(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm100255(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%st000007(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st000007(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%st000007=initial_data_value
ELSE
ALLOCATE(grid%st000007(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st000007(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%st007028(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st007028(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%st007028=initial_data_value
ELSE
ALLOCATE(grid%st007028(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st007028(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%st028100(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st028100(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%st028100=initial_data_value
ELSE
ALLOCATE(grid%st028100(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st028100(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%st100255(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st100255(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%st100255=initial_data_value
ELSE
ALLOCATE(grid%st100255(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st100255(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%sm000010(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm000010(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sm000010=initial_data_value
ELSE
ALLOCATE(grid%sm000010(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm000010(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%sm010040(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm010040(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sm010040=initial_data_value
ELSE
ALLOCATE(grid%sm010040(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm010040(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%sm040100(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm040100(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sm040100=initial_data_value
ELSE
ALLOCATE(grid%sm040100(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm040100(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%sm100200(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm100200(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sm100200=initial_data_value
ELSE
ALLOCATE(grid%sm100200(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm100200(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%sm010200(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm010200(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sm010200=initial_data_value
ELSE
ALLOCATE(grid%sm010200(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm010200(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilm000(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilm000(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilm000=initial_data_value
ELSE
ALLOCATE(grid%soilm000(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilm000(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilm005(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilm005(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilm005=initial_data_value
ELSE
ALLOCATE(grid%soilm005(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilm005(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilm020(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilm020(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilm020=initial_data_value
ELSE
ALLOCATE(grid%soilm020(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilm020(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilm040(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilm040(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilm040=initial_data_value
ELSE
ALLOCATE(grid%soilm040(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilm040(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilm160(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilm160(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilm160=initial_data_value
ELSE
ALLOCATE(grid%soilm160(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilm160(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilm300(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilm300(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilm300=initial_data_value
ELSE
ALLOCATE(grid%soilm300(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilm300(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%sw000010(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sw000010(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sw000010=initial_data_value
ELSE
ALLOCATE(grid%sw000010(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sw000010(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%sw010040(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sw010040(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sw010040=initial_data_value
ELSE
ALLOCATE(grid%sw010040(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sw010040(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%sw040100(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sw040100(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sw040100=initial_data_value
ELSE
ALLOCATE(grid%sw040100(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sw040100(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%sw100200(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sw100200(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sw100200=initial_data_value
ELSE
ALLOCATE(grid%sw100200(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sw100200(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%sw010200(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sw010200(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sw010200=initial_data_value
ELSE
ALLOCATE(grid%sw010200(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sw010200(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilw000(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilw000(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilw000=initial_data_value
ELSE
ALLOCATE(grid%soilw000(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilw000(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilw005(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilw005(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilw005=initial_data_value
ELSE
ALLOCATE(grid%soilw005(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilw005(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilw020(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilw020(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilw020=initial_data_value
ELSE
ALLOCATE(grid%soilw020(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilw020(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilw040(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilw040(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilw040=initial_data_value
ELSE
ALLOCATE(grid%soilw040(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilw040(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilw160(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilw160(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilw160=initial_data_value
ELSE
ALLOCATE(grid%soilw160(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilw160(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilw300(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilw300(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilw300=initial_data_value
ELSE
ALLOCATE(grid%soilw300(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilw300(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%st000010(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st000010(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%st000010=initial_data_value
ELSE
ALLOCATE(grid%st000010(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st000010(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%st010040(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st010040(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%st010040=initial_data_value
ELSE
ALLOCATE(grid%st010040(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st010040(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%st040100(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st040100(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%st040100=initial_data_value
ELSE
ALLOCATE(grid%st040100(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st040100(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%st100200(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st100200(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%st100200=initial_data_value
ELSE
ALLOCATE(grid%st100200(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st100200(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%st010200(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st010200(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%st010200=initial_data_value
ELSE
ALLOCATE(grid%st010200(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st010200(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilt000(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilt000(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilt000=initial_data_value
ELSE
ALLOCATE(grid%soilt000(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilt000(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilt005(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilt005(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilt005=initial_data_value
ELSE
ALLOCATE(grid%soilt005(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilt005(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilt020(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilt020(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilt020=initial_data_value
ELSE
ALLOCATE(grid%soilt020(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilt020(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilt040(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilt040(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilt040=initial_data_value
ELSE
ALLOCATE(grid%soilt040(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilt040(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilt160(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilt160(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilt160=initial_data_value
ELSE
ALLOCATE(grid%soilt160(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilt160(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilt300(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilt300(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilt300=initial_data_value
ELSE
ALLOCATE(grid%soilt300(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilt300(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%landmask(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%landmask(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%landmask=initial_data_value
ELSE
ALLOCATE(grid%landmask(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%landmask(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%topostdv(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%topostdv(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%topostdv=initial_data_value
ELSE
ALLOCATE(grid%topostdv(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%topostdv(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%toposlpx(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%toposlpx(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%toposlpx=initial_data_value
ELSE
ALLOCATE(grid%toposlpx(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%toposlpx(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%toposlpy(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%toposlpy(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%toposlpy=initial_data_value
ELSE
ALLOCATE(grid%toposlpy(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%toposlpy(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%greenmax(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%greenmax(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%greenmax=initial_data_value
ELSE
ALLOCATE(grid%greenmax(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%greenmax(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%greenmin(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%greenmin(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%greenmin=initial_data_value
ELSE
ALLOCATE(grid%greenmin(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%greenmin(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%albedomx(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%albedomx(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%albedomx=initial_data_value
ELSE
ALLOCATE(grid%albedomx(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%albedomx(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%slopecat(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%slopecat(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%slopecat=initial_data_value
ELSE
ALLOCATE(grid%slopecat(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%slopecat(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%toposoil(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%toposoil(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%toposoil=initial_data_value
ELSE
ALLOCATE(grid%toposoil(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%toposoil(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%landusef(sm31:em31,1:model_config_rec%num_land_cat,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%landusef(sm31:em31,1:model_config_rec%num_land_cat,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%landusef=initial_data_value
ELSE
ALLOCATE(grid%landusef(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%landusef(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilctop(sm31:em31,1:model_config_rec%num_soil_cat,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilctop(sm31:em31,1:model_config_rec%num_soil_cat,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilctop=initial_data_value
ELSE
ALLOCATE(grid%soilctop(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilctop(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilcbot(sm31:em31,1:model_config_rec%num_soil_cat,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilcbot(sm31:em31,1:model_config_rec%num_soil_cat,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilcbot=initial_data_value
ELSE
ALLOCATE(grid%soilcbot(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilcbot(1,1,1).  ')
 endif
ENDIF
ALLOCATE(grid%moist(sm31:em31,sm32:em32,sm33:em33,num_moist),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%moist(sm31:em31,sm32:em32,sm33:em33,num_moist). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%moist=initial_data_value
ALLOCATE(grid%scalar(sm31:em31,sm32:em32,sm33:em33,num_scalar),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%scalar(sm31:em31,sm32:em32,sm33:em33,num_scalar). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%scalar=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%scalar_b(max(ed31,ed32),sd33:ed33,spec_bdy_width,4,num_scalar),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%scalar_b(max(ed31,ed32),sd33:ed33,spec_bdy_width,4,num_scalar). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%scalar_b=initial_data_value
ELSE
ALLOCATE(grid%scalar_b(1,1,1,1,num_scalar),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%scalar_b(1,1,1,1,num_scalar).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%scalar_bt(max(ed31,ed32),sd33:ed33,spec_bdy_width,4,num_scalar),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%scalar_bt(max(ed31,ed32),sd33:ed33,spec_bdy_width,4,num_scalar). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%scalar_bt=initial_data_value
ELSE
ALLOCATE(grid%scalar_bt(1,1,1,1,num_scalar),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%scalar_bt(1,1,1,1,num_scalar).  ')
 endif
ENDIF
ALLOCATE(grid%chem(sm31:em31,sm33:em33,sm32:em32,num_chem),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%chem(sm31:em31,sm33:em33,sm32:em32,num_chem). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%chem=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%smois(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%smois(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%smois=initial_data_value
ELSE
ALLOCATE(grid%smois(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%smois(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%tslb(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tslb(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tslb=initial_data_value
ELSE
ALLOCATE(grid%tslb(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tslb(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%gsw(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%gsw(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%gsw=initial_data_value
ELSE
ALLOCATE(grid%gsw(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%gsw(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%xlat(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xlat(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xlat=initial_data_value
ELSE
ALLOCATE(grid%xlat(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xlat(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%xlong(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xlong(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xlong=initial_data_value
ELSE
ALLOCATE(grid%xlong(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xlong(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%xland(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xland(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xland=initial_data_value
ELSE
ALLOCATE(grid%xland(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xland(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%raincv(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%raincv(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%raincv=initial_data_value
ELSE
ALLOCATE(grid%raincv(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%raincv(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%psfc(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%psfc(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%psfc=initial_data_value
ELSE
ALLOCATE(grid%psfc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%psfc(1,1).  ')
 endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%dtbc=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%th2(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%th2(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%th2=initial_data_value
ELSE
ALLOCATE(grid%th2(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%th2(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%t2(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%t2(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%t2=initial_data_value
ELSE
ALLOCATE(grid%t2(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%t2(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%u10(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%u10(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%u10=initial_data_value
ELSE
ALLOCATE(grid%u10(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%u10(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%v10(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%v10(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%v10=initial_data_value
ELSE
ALLOCATE(grid%v10(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%v10(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%xice(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xice(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xice=initial_data_value
ELSE
ALLOCATE(grid%xice(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xice(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%smstav(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%smstav(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%smstav=initial_data_value
ELSE
ALLOCATE(grid%smstav(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%smstav(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%smstot(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%smstot(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%smstot=initial_data_value
ELSE
ALLOCATE(grid%smstot(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%smstot(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%sfcrunoff(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sfcrunoff(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sfcrunoff=initial_data_value
ELSE
ALLOCATE(grid%sfcrunoff(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sfcrunoff(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%udrunoff(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%udrunoff(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%udrunoff=initial_data_value
ELSE
ALLOCATE(grid%udrunoff(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%udrunoff(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%ivgtyp(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ivgtyp(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ivgtyp=0
ELSE
ALLOCATE(grid%ivgtyp(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ivgtyp(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%isltyp(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%isltyp(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%isltyp=0
ELSE
ALLOCATE(grid%isltyp(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%isltyp(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%vegfra(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%vegfra(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%vegfra=initial_data_value
ELSE
ALLOCATE(grid%vegfra(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%vegfra(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%sfcevp(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sfcevp(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sfcevp=initial_data_value
ELSE
ALLOCATE(grid%sfcevp(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sfcevp(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%grdflx(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%grdflx(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%grdflx=initial_data_value
ELSE
ALLOCATE(grid%grdflx(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%grdflx(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%albbck(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%albbck(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%albbck=initial_data_value
ELSE
ALLOCATE(grid%albbck(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%albbck(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%sfcexc(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sfcexc(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sfcexc=initial_data_value
ELSE
ALLOCATE(grid%sfcexc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sfcexc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%acsnow(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%acsnow(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%acsnow=initial_data_value
ELSE
ALLOCATE(grid%acsnow(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%acsnow(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%acsnom(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%acsnom(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%acsnom=initial_data_value
ELSE
ALLOCATE(grid%acsnom(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%acsnom(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rmol(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rmol(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rmol=initial_data_value
ELSE
ALLOCATE(grid%rmol(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rmol(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%snow(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%snow(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%snow=initial_data_value
ELSE
ALLOCATE(grid%snow(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%snow(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%canwat(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%canwat(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%canwat=initial_data_value
ELSE
ALLOCATE(grid%canwat(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%canwat(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%sst(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sst(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sst=initial_data_value
ELSE
ALLOCATE(grid%sst(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sst(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%weasd(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%weasd(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%weasd=initial_data_value
ELSE
ALLOCATE(grid%weasd(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%weasd(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%znt(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%znt(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%znt=initial_data_value
ELSE
ALLOCATE(grid%znt(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%znt(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%mol(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%mol(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%mol=initial_data_value
ELSE
ALLOCATE(grid%mol(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%mol(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%tke_myj(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tke_myj(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tke_myj=initial_data_value
ELSE
ALLOCATE(grid%tke_myj(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tke_myj(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%el_myj(sm31:em31,sm33:em33,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%el_myj(sm31:em31,sm33:em33,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%el_myj=initial_data_value
ELSE
ALLOCATE(grid%el_myj(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%el_myj(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%exch_h(sm31:em31,sm33:em33,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%exch_h(sm31:em31,sm33:em33,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%exch_h=initial_data_value
ELSE
ALLOCATE(grid%exch_h(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%exch_h(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%thz0(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%thz0(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%thz0=initial_data_value
ELSE
ALLOCATE(grid%thz0(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%thz0(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%qz0(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%qz0(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%qz0=initial_data_value
ELSE
ALLOCATE(grid%qz0(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%qz0(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%uz0(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%uz0(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%uz0=initial_data_value
ELSE
ALLOCATE(grid%uz0(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%uz0(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%vz0(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%vz0(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%vz0=initial_data_value
ELSE
ALLOCATE(grid%vz0(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%vz0(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%flhc(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%flhc(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%flhc=initial_data_value
ELSE
ALLOCATE(grid%flhc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%flhc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%flqc(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%flqc(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%flqc=initial_data_value
ELSE
ALLOCATE(grid%flqc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%flqc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%qsg(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%qsg(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%qsg=initial_data_value
ELSE
ALLOCATE(grid%qsg(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%qsg(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%qvg(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%qvg(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%qvg=initial_data_value
ELSE
ALLOCATE(grid%qvg(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%qvg(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%qcg(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%qcg(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%qcg=initial_data_value
ELSE
ALLOCATE(grid%qcg(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%qcg(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilt1(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilt1(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilt1=initial_data_value
ELSE
ALLOCATE(grid%soilt1(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilt1(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%tsnav(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tsnav(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tsnav=initial_data_value
ELSE
ALLOCATE(grid%tsnav(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tsnav(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_psfc_out(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_psfc_out(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_psfc_out=initial_data_value
ELSE
ALLOCATE(grid%nmm_psfc_out(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_psfc_out(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%uz0h(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%uz0h(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%uz0h=initial_data_value
ELSE
ALLOCATE(grid%uz0h(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%uz0h(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%vz0h(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%vz0h(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%vz0h=initial_data_value
ELSE
ALLOCATE(grid%vz0h(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%vz0h(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%dudt(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%dudt(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dudt=initial_data_value
ELSE
ALLOCATE(grid%dudt(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%dudt(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%dvdt(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%dvdt(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dvdt=initial_data_value
ELSE
ALLOCATE(grid%dvdt(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%dvdt(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%qsfc(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%qsfc(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%qsfc=initial_data_value
ELSE
ALLOCATE(grid%qsfc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%qsfc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%akhs(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%akhs(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%akhs=initial_data_value
ELSE
ALLOCATE(grid%akhs(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%akhs(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%akms(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%akms(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%akms=initial_data_value
ELSE
ALLOCATE(grid%akms(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%akms(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%htop(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%htop(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%htop=initial_data_value
ELSE
ALLOCATE(grid%htop(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%htop(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%hbot(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%hbot(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%hbot=initial_data_value
ELSE
ALLOCATE(grid%hbot(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%hbot(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%htopr(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%htopr(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%htopr=initial_data_value
ELSE
ALLOCATE(grid%htopr(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%htopr(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%hbotr(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%hbotr(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%hbotr=initial_data_value
ELSE
ALLOCATE(grid%hbotr(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%hbotr(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%htopd(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%htopd(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%htopd=initial_data_value
ELSE
ALLOCATE(grid%htopd(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%htopd(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%hbotd(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%hbotd(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%hbotd=initial_data_value
ELSE
ALLOCATE(grid%hbotd(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%hbotd(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%htops(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%htops(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%htops=initial_data_value
ELSE
ALLOCATE(grid%htops(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%htops(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%hbots(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%hbots(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%hbots=initial_data_value
ELSE
ALLOCATE(grid%hbots(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%hbots(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%cuppt(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%cuppt(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cuppt=initial_data_value
ELSE
ALLOCATE(grid%cuppt(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%cuppt(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%cprate(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%cprate(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cprate=initial_data_value
ELSE
ALLOCATE(grid%cprate(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%cprate(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%f_ice_phy(sm31:em31,sm33:em33,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%f_ice_phy(sm31:em31,sm33:em33,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%f_ice_phy=initial_data_value
ELSE
ALLOCATE(grid%f_ice_phy(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%f_ice_phy(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%f_rain_phy(sm31:em31,sm33:em33,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%f_rain_phy(sm31:em31,sm33:em33,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%f_rain_phy=initial_data_value
ELSE
ALLOCATE(grid%f_rain_phy(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%f_rain_phy(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%f_rimef_phy(sm31:em31,sm33:em33,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%f_rimef_phy(sm31:em31,sm33:em33,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%f_rimef_phy=initial_data_value
ELSE
ALLOCATE(grid%f_rimef_phy(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%f_rimef_phy(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%mass_flux(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%mass_flux(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%mass_flux=initial_data_value
ELSE
ALLOCATE(grid%mass_flux(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%mass_flux(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%apr_gr(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_gr(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%apr_gr=initial_data_value
ELSE
ALLOCATE(grid%apr_gr(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_gr(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%apr_w(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_w(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%apr_w=initial_data_value
ELSE
ALLOCATE(grid%apr_w(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_w(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%apr_mc(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_mc(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%apr_mc=initial_data_value
ELSE
ALLOCATE(grid%apr_mc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_mc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%apr_st(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_st(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%apr_st=initial_data_value
ELSE
ALLOCATE(grid%apr_st(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_st(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%apr_as(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_as(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%apr_as=initial_data_value
ELSE
ALLOCATE(grid%apr_as(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_as(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%apr_capma(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_capma(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%apr_capma=initial_data_value
ELSE
ALLOCATE(grid%apr_capma(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_capma(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%apr_capme(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_capme(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%apr_capme=initial_data_value
ELSE
ALLOCATE(grid%apr_capme(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_capme(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%apr_capmi(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_capmi(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%apr_capmi=initial_data_value
ELSE
ALLOCATE(grid%apr_capmi(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_capmi(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%xf_ens(sm31:em31,sm32:em32,1:model_config_rec%ensdim),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xf_ens(sm31:em31,sm32:em32,1:model_config_rec%ensdim). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xf_ens=initial_data_value
ELSE
ALLOCATE(grid%xf_ens(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xf_ens(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%pr_ens(sm31:em31,sm32:em32,1:model_config_rec%ensdim),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%pr_ens(sm31:em31,sm32:em32,1:model_config_rec%ensdim). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%pr_ens=initial_data_value
ELSE
ALLOCATE(grid%pr_ens(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%pr_ens(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rthften(sm31:em31,sm33:em33,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rthften(sm31:em31,sm33:em33,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rthften=initial_data_value
ELSE
ALLOCATE(grid%rthften(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rthften(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rqvften(sm31:em31,sm33:em33,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rqvften(sm31:em31,sm33:em33,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqvften=initial_data_value
ELSE
ALLOCATE(grid%rqvften(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rqvften(1,1,1).  ')
 endif
ENDIF
ALLOCATE(grid%snowh(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%snowh(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%snowh=initial_data_value
ALLOCATE(grid%rhosn(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rhosn(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rhosn=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%smfr3d(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%smfr3d(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%smfr3d=initial_data_value
ELSE
ALLOCATE(grid%smfr3d(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%smfr3d(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%keepfr3dflag(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%keepfr3dflag(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%keepfr3dflag=initial_data_value
ELSE
ALLOCATE(grid%keepfr3dflag(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%keepfr3dflag(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%mp_restart_state(1:7501),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%mp_restart_state(1:7501). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%mp_restart_state=initial_data_value
ELSE
ALLOCATE(grid%mp_restart_state(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%mp_restart_state(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%tbpvs_state(1:7501),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tbpvs_state(1:7501). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tbpvs_state=initial_data_value
ELSE
ALLOCATE(grid%tbpvs_state(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tbpvs_state(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%tbpvs0_state(1:7501),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tbpvs0_state(1:7501). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tbpvs0_state=initial_data_value
ELSE
ALLOCATE(grid%tbpvs0_state(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tbpvs0_state(1).  ')
 endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%landuse_isice=0
IF ( setinitval .EQ. 3 ) grid%landuse_lucats=0
IF ( setinitval .EQ. 3 ) grid%landuse_luseas=0
IF ( setinitval .EQ. 3 ) grid%landuse_isn=0
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%lu_state(1:7501),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%lu_state(1:7501). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%lu_state=initial_data_value
ELSE
ALLOCATE(grid%lu_state(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%lu_state(1).  ')
 endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%number_at_same_level=0
IF ( setinitval .EQ. 3 ) grid%itimestep=0
IF ( setinitval .EQ. 3 ) grid%xtime=initial_data_value
IF ( setinitval .EQ. 3 ) grid%julian=initial_data_value
IF ( setinitval .EQ. 3 ) grid%lbc_fid=0
IF ( setinitval .EQ. 3 ) grid%tiled=.FALSE.
IF ( setinitval .EQ. 3 ) grid%patched=.FALSE.
IF ( setinitval .EQ. 3 ) grid%run_days=0
IF ( setinitval .EQ. 3 ) grid%run_hours=0
IF ( setinitval .EQ. 3 ) grid%run_minutes=0
IF ( setinitval .EQ. 3 ) grid%run_seconds=0
IF ( setinitval .EQ. 3 ) grid%start_year=0
IF ( setinitval .EQ. 3 ) grid%start_month=0
IF ( setinitval .EQ. 3 ) grid%start_day=0
IF ( setinitval .EQ. 3 ) grid%start_hour=0
IF ( setinitval .EQ. 3 ) grid%start_minute=0
IF ( setinitval .EQ. 3 ) grid%start_second=0
IF ( setinitval .EQ. 3 ) grid%end_year=0
IF ( setinitval .EQ. 3 ) grid%end_month=0
IF ( setinitval .EQ. 3 ) grid%end_day=0
IF ( setinitval .EQ. 3 ) grid%end_hour=0
IF ( setinitval .EQ. 3 ) grid%end_minute=0
IF ( setinitval .EQ. 3 ) grid%end_second=0
IF ( setinitval .EQ. 3 ) grid%interval_seconds=0
IF ( setinitval .EQ. 3 ) grid%input_from_file=.FALSE.
IF ( setinitval .EQ. 3 ) grid%fine_input_stream=0
IF ( setinitval .EQ. 3 ) grid%oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput9_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput10_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_oid=0
IF ( setinitval .EQ. 3 ) grid%history_interval=0
IF ( setinitval .EQ. 3 ) grid%frames_per_outfile=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist1=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist2=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist3=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist4=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist5=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist6=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist7=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist8=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist9=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist10=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist11=0
IF ( setinitval .EQ. 3 ) grid%restart=.FALSE.
IF ( setinitval .EQ. 3 ) grid%restart_interval=0
IF ( setinitval .EQ. 3 ) grid%io_form_input=0
IF ( setinitval .EQ. 3 ) grid%io_form_history=0
IF ( setinitval .EQ. 3 ) grid%io_form_restart=0
IF ( setinitval .EQ. 3 ) grid%io_form_boundary=0
IF ( setinitval .EQ. 3 ) grid%debug_level=0
IF ( setinitval .EQ. 3 ) grid%self_test_domain=.FALSE.
IF ( setinitval .EQ. 3 ) grid%history_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%history_interval_d=0
IF ( setinitval .EQ. 3 ) grid%history_interval_h=0
IF ( setinitval .EQ. 3 ) grid%history_interval_m=0
IF ( setinitval .EQ. 3 ) grid%history_interval_s=0
IF ( setinitval .EQ. 3 ) grid%inputout_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%inputout_interval_d=0
IF ( setinitval .EQ. 3 ) grid%inputout_interval_h=0
IF ( setinitval .EQ. 3 ) grid%inputout_interval_m=0
IF ( setinitval .EQ. 3 ) grid%inputout_interval_s=0
IF ( setinitval .EQ. 3 ) grid%inputout_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_interval=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_interval=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_interval=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_interval=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_interval=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_interval=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_interval=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_interval=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_interval=0
IF ( setinitval .EQ. 3 ) grid%auxinput9_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput9_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput9_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput9_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput9_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput9_interval=0
IF ( setinitval .EQ. 3 ) grid%gfdda_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%gfdda_interval_d=0
IF ( setinitval .EQ. 3 ) grid%gfdda_interval_h=0
IF ( setinitval .EQ. 3 ) grid%gfdda_interval_m=0
IF ( setinitval .EQ. 3 ) grid%gfdda_interval_s=0
IF ( setinitval .EQ. 3 ) grid%gfdda_interval=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_interval=0
IF ( setinitval .EQ. 3 ) grid%restart_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%restart_interval_d=0
IF ( setinitval .EQ. 3 ) grid%restart_interval_h=0
IF ( setinitval .EQ. 3 ) grid%restart_interval_m=0
IF ( setinitval .EQ. 3 ) grid%restart_interval_s=0
IF ( setinitval .EQ. 3 ) grid%history_begin_y=0
IF ( setinitval .EQ. 3 ) grid%history_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%history_begin_d=0
IF ( setinitval .EQ. 3 ) grid%history_begin_h=0
IF ( setinitval .EQ. 3 ) grid%history_begin_m=0
IF ( setinitval .EQ. 3 ) grid%history_begin_s=0
IF ( setinitval .EQ. 3 ) grid%inputout_begin_y=0
IF ( setinitval .EQ. 3 ) grid%inputout_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%inputout_begin_d=0
IF ( setinitval .EQ. 3 ) grid%inputout_begin_h=0
IF ( setinitval .EQ. 3 ) grid%inputout_begin_m=0
IF ( setinitval .EQ. 3 ) grid%inputout_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput9_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput9_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput9_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput9_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput9_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput9_begin_s=0
IF ( setinitval .EQ. 3 ) grid%gfdda_begin_y=0
IF ( setinitval .EQ. 3 ) grid%gfdda_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%gfdda_begin_d=0
IF ( setinitval .EQ. 3 ) grid%gfdda_begin_h=0
IF ( setinitval .EQ. 3 ) grid%gfdda_begin_m=0
IF ( setinitval .EQ. 3 ) grid%gfdda_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_begin_s=0
IF ( setinitval .EQ. 3 ) grid%restart_begin_y=0
IF ( setinitval .EQ. 3 ) grid%restart_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%restart_begin_d=0
IF ( setinitval .EQ. 3 ) grid%restart_begin_h=0
IF ( setinitval .EQ. 3 ) grid%restart_begin_m=0
IF ( setinitval .EQ. 3 ) grid%restart_begin_s=0
IF ( setinitval .EQ. 3 ) grid%history_end_y=0
IF ( setinitval .EQ. 3 ) grid%history_end_mo=0
IF ( setinitval .EQ. 3 ) grid%history_end_d=0
IF ( setinitval .EQ. 3 ) grid%history_end_h=0
IF ( setinitval .EQ. 3 ) grid%history_end_m=0
IF ( setinitval .EQ. 3 ) grid%history_end_s=0
IF ( setinitval .EQ. 3 ) grid%inputout_end_y=0
IF ( setinitval .EQ. 3 ) grid%inputout_end_mo=0
IF ( setinitval .EQ. 3 ) grid%inputout_end_d=0
IF ( setinitval .EQ. 3 ) grid%inputout_end_h=0
IF ( setinitval .EQ. 3 ) grid%inputout_end_m=0
IF ( setinitval .EQ. 3 ) grid%inputout_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput9_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput9_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput9_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput9_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput9_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput9_end_s=0
IF ( setinitval .EQ. 3 ) grid%gfdda_end_y=0
IF ( setinitval .EQ. 3 ) grid%gfdda_end_mo=0
IF ( setinitval .EQ. 3 ) grid%gfdda_end_d=0
IF ( setinitval .EQ. 3 ) grid%gfdda_end_h=0
IF ( setinitval .EQ. 3 ) grid%gfdda_end_m=0
IF ( setinitval .EQ. 3 ) grid%gfdda_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_end_s=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxinput1=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxinput2=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxinput3=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxinput4=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxinput5=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxinput6=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxinput7=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxinput8=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxinput9=0
IF ( setinitval .EQ. 3 ) grid%io_form_gfdda=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxinput11=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist1=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist2=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist3=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist4=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist5=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist6=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist7=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist8=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist9=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist10=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist11=0
IF ( setinitval .EQ. 3 ) grid%julyr=0
IF ( setinitval .EQ. 3 ) grid%julday=0
IF ( setinitval .EQ. 3 ) grid%gmt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%write_input=.FALSE.
IF ( setinitval .EQ. 3 ) grid%write_restart_at_0h=.FALSE.
IF ( setinitval .EQ. 3 ) grid%adjust_output_times=.FALSE.
IF ( setinitval .EQ. 3 ) grid%adjust_input_times=.FALSE.
IF ( setinitval .EQ. 3 ) grid%tstart=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nocolons=.FALSE.
IF ( setinitval .EQ. 3 ) grid%time_step=0
IF ( setinitval .EQ. 3 ) grid%time_step_fract_num=0
IF ( setinitval .EQ. 3 ) grid%time_step_fract_den=0
IF ( setinitval .EQ. 3 ) grid%max_dom=0
IF ( setinitval .EQ. 3 ) grid%s_we=0
IF ( setinitval .EQ. 3 ) grid%e_we=0
IF ( setinitval .EQ. 3 ) grid%s_sn=0
IF ( setinitval .EQ. 3 ) grid%e_sn=0
IF ( setinitval .EQ. 3 ) grid%s_vert=0
IF ( setinitval .EQ. 3 ) grid%e_vert=0
IF ( setinitval .EQ. 3 ) grid%dx=initial_data_value
IF ( setinitval .EQ. 3 ) grid%dy=initial_data_value
IF ( setinitval .EQ. 3 ) grid%grid_id=0
IF ( setinitval .EQ. 3 ) grid%parent_id=0
IF ( setinitval .EQ. 3 ) grid%i_parent_start=0
IF ( setinitval .EQ. 3 ) grid%j_parent_start=0
IF ( setinitval .EQ. 3 ) grid%parent_grid_ratio=0
IF ( setinitval .EQ. 3 ) grid%parent_time_step_ratio=0
IF ( setinitval .EQ. 3 ) grid%feedback=0
IF ( setinitval .EQ. 3 ) grid%smooth_option=0
IF ( setinitval .EQ. 3 ) grid%ztop=initial_data_value
IF ( setinitval .EQ. 3 ) grid%moad_grid_ratio=0
IF ( setinitval .EQ. 3 ) grid%moad_time_step_ratio=0
IF ( setinitval .EQ. 3 ) grid%shw=0
IF ( setinitval .EQ. 3 ) grid%tile_sz_x=0
IF ( setinitval .EQ. 3 ) grid%tile_sz_y=0
IF ( setinitval .EQ. 3 ) grid%numtiles=0
IF ( setinitval .EQ. 3 ) grid%nproc_x=0
IF ( setinitval .EQ. 3 ) grid%nproc_y=0
IF ( setinitval .EQ. 3 ) grid%irand=0
IF ( setinitval .EQ. 3 ) grid%dt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%num_moves=0
IF ( setinitval .EQ. 3 ) grid%move_id=0
IF ( setinitval .EQ. 3 ) grid%move_interval=0
IF ( setinitval .EQ. 3 ) grid%move_cd_x=0
IF ( setinitval .EQ. 3 ) grid%move_cd_y=0
IF ( setinitval .EQ. 3 ) grid%swap_x=.FALSE.
IF ( setinitval .EQ. 3 ) grid%swap_y=.FALSE.
IF ( setinitval .EQ. 3 ) grid%cycle_x=.FALSE.
IF ( setinitval .EQ. 3 ) grid%cycle_y=.FALSE.
IF ( setinitval .EQ. 3 ) grid%reorder_mesh=.FALSE.
IF ( setinitval .EQ. 3 ) grid%perturb_input=.FALSE.
IF ( setinitval .EQ. 3 ) grid%eta_levels=initial_data_value
IF ( setinitval .EQ. 3 ) grid%ptsgm=initial_data_value
IF ( setinitval .EQ. 3 ) grid%num_metgrid_levels=0
IF ( setinitval .EQ. 3 ) grid%p_top_requested=initial_data_value
IF ( setinitval .EQ. 3 ) grid%mp_physics=0
IF ( setinitval .EQ. 3 ) grid%ra_lw_physics=0
IF ( setinitval .EQ. 3 ) grid%ra_sw_physics=0
IF ( setinitval .EQ. 3 ) grid%radt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%sf_sfclay_physics=0
IF ( setinitval .EQ. 3 ) grid%sf_surface_physics=0
IF ( setinitval .EQ. 3 ) grid%bl_pbl_physics=0
IF ( setinitval .EQ. 3 ) grid%bldt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%cu_physics=0
IF ( setinitval .EQ. 3 ) grid%cudt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%gsmdt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%isfflx=0
IF ( setinitval .EQ. 3 ) grid%ifsnow=0
IF ( setinitval .EQ. 3 ) grid%icloud=0
IF ( setinitval .EQ. 3 ) grid%swrad_scat=initial_data_value
IF ( setinitval .EQ. 3 ) grid%surface_input_source=0
IF ( setinitval .EQ. 3 ) grid%num_soil_layers=0
IF ( setinitval .EQ. 3 ) grid%maxiens=0
IF ( setinitval .EQ. 3 ) grid%maxens=0
IF ( setinitval .EQ. 3 ) grid%maxens2=0
IF ( setinitval .EQ. 3 ) grid%maxens3=0
IF ( setinitval .EQ. 3 ) grid%ensdim=0
IF ( setinitval .EQ. 3 ) grid%chem_opt=0
IF ( setinitval .EQ. 3 ) grid%num_land_cat=0
IF ( setinitval .EQ. 3 ) grid%num_soil_cat=0
IF ( setinitval .EQ. 3 ) grid%mp_zero_out=0
IF ( setinitval .EQ. 3 ) grid%mp_zero_out_thresh=initial_data_value
IF ( setinitval .EQ. 3 ) grid%seaice_threshold=initial_data_value
IF ( setinitval .EQ. 3 ) grid%sst_update=0
IF ( setinitval .EQ. 3 ) grid%ucmcall=0
IF ( setinitval .EQ. 3 ) grid%idtad=0
IF ( setinitval .EQ. 3 ) grid%nsoil=0
IF ( setinitval .EQ. 3 ) grid%nphs=0
IF ( setinitval .EQ. 3 ) grid%ncnvc=0
IF ( setinitval .EQ. 3 ) grid%nrads=0
IF ( setinitval .EQ. 3 ) grid%nradl=0
IF ( setinitval .EQ. 3 ) grid%tprec=initial_data_value
IF ( setinitval .EQ. 3 ) grid%theat=initial_data_value
IF ( setinitval .EQ. 3 ) grid%tclod=initial_data_value
IF ( setinitval .EQ. 3 ) grid%trdsw=initial_data_value
IF ( setinitval .EQ. 3 ) grid%trdlw=initial_data_value
IF ( setinitval .EQ. 3 ) grid%tsrfc=initial_data_value
IF ( setinitval .EQ. 3 ) grid%pcpflg=.FALSE.
IF ( setinitval .EQ. 3 ) grid%sigma=0
IF ( setinitval .EQ. 3 ) grid%co2tf=0
IF ( setinitval .EQ. 3 ) grid%ra_call_offset=0
IF ( setinitval .EQ. 3 ) grid%cam_abs_freq_s=initial_data_value
IF ( setinitval .EQ. 3 ) grid%levsiz=0
IF ( setinitval .EQ. 3 ) grid%paerlev=0
IF ( setinitval .EQ. 3 ) grid%cam_abs_dim1=0
IF ( setinitval .EQ. 3 ) grid%cam_abs_dim2=0
IF ( setinitval .EQ. 3 ) grid%cu_rad_feedback=.FALSE.
IF ( setinitval .EQ. 3 ) grid%dyn_opt=0
IF ( setinitval .EQ. 3 ) grid%rk_ord=0
IF ( setinitval .EQ. 3 ) grid%w_damping=0
IF ( setinitval .EQ. 3 ) grid%diff_opt=0
IF ( setinitval .EQ. 3 ) grid%km_opt=0
IF ( setinitval .EQ. 3 ) grid%damp_opt=0
IF ( setinitval .EQ. 3 ) grid%zdamp=initial_data_value
IF ( setinitval .EQ. 3 ) grid%base_pres=initial_data_value
IF ( setinitval .EQ. 3 ) grid%base_temp=initial_data_value
IF ( setinitval .EQ. 3 ) grid%base_lapse=initial_data_value
IF ( setinitval .EQ. 3 ) grid%dampcoef=initial_data_value
IF ( setinitval .EQ. 3 ) grid%khdif=initial_data_value
IF ( setinitval .EQ. 3 ) grid%kvdif=initial_data_value
IF ( setinitval .EQ. 3 ) grid%smdiv=initial_data_value
IF ( setinitval .EQ. 3 ) grid%emdiv=initial_data_value
IF ( setinitval .EQ. 3 ) grid%epssm=initial_data_value
IF ( setinitval .EQ. 3 ) grid%non_hydrostatic=.FALSE.
IF ( setinitval .EQ. 3 ) grid%time_step_sound=0
IF ( setinitval .EQ. 3 ) grid%h_mom_adv_order=0
IF ( setinitval .EQ. 3 ) grid%v_mom_adv_order=0
IF ( setinitval .EQ. 3 ) grid%h_sca_adv_order=0
IF ( setinitval .EQ. 3 ) grid%v_sca_adv_order=0
IF ( setinitval .EQ. 3 ) grid%top_radiation=.FALSE.
IF ( setinitval .EQ. 3 ) grid%mix_cr_len=initial_data_value
IF ( setinitval .EQ. 3 ) grid%tke_upper_bound=initial_data_value
IF ( setinitval .EQ. 3 ) grid%kh_tke_upper_bound=initial_data_value
IF ( setinitval .EQ. 3 ) grid%kv_tke_upper_bound=initial_data_value
IF ( setinitval .EQ. 3 ) grid%tke_drag_coefficient=initial_data_value
IF ( setinitval .EQ. 3 ) grid%tke_heat_flux=initial_data_value
IF ( setinitval .EQ. 3 ) grid%pert_coriolis=.FALSE.
IF ( setinitval .EQ. 3 ) grid%spec_bdy_width=0
IF ( setinitval .EQ. 3 ) grid%spec_zone=0
IF ( setinitval .EQ. 3 ) grid%relax_zone=0
IF ( setinitval .EQ. 3 ) grid%specified=.FALSE.
IF ( setinitval .EQ. 3 ) grid%periodic_x=.FALSE.
IF ( setinitval .EQ. 3 ) grid%symmetric_xs=.FALSE.
IF ( setinitval .EQ. 3 ) grid%symmetric_xe=.FALSE.
IF ( setinitval .EQ. 3 ) grid%open_xs=.FALSE.
IF ( setinitval .EQ. 3 ) grid%open_xe=.FALSE.
IF ( setinitval .EQ. 3 ) grid%periodic_y=.FALSE.
IF ( setinitval .EQ. 3 ) grid%symmetric_ys=.FALSE.
IF ( setinitval .EQ. 3 ) grid%symmetric_ye=.FALSE.
IF ( setinitval .EQ. 3 ) grid%open_ys=.FALSE.
IF ( setinitval .EQ. 3 ) grid%open_ye=.FALSE.
IF ( setinitval .EQ. 3 ) grid%nested=.FALSE.
IF ( setinitval .EQ. 3 ) grid%real_data_init_type=0
IF ( setinitval .EQ. 3 ) grid%background_proc_id=0
IF ( setinitval .EQ. 3 ) grid%forecast_proc_id=0
IF ( setinitval .EQ. 3 ) grid%production_status=0
IF ( setinitval .EQ. 3 ) grid%compression=0
IF ( setinitval .EQ. 3 ) grid%cen_lat=initial_data_value
IF ( setinitval .EQ. 3 ) grid%cen_lon=initial_data_value
IF ( setinitval .EQ. 3 ) grid%truelat1=initial_data_value
IF ( setinitval .EQ. 3 ) grid%truelat2=initial_data_value
IF ( setinitval .EQ. 3 ) grid%moad_cen_lat=initial_data_value
IF ( setinitval .EQ. 3 ) grid%stand_lon=initial_data_value
IF ( setinitval .EQ. 3 ) grid%bdyfrq=initial_data_value
IF ( setinitval .EQ. 3 ) grid%iswater=0
IF ( setinitval .EQ. 3 ) grid%isice=0
IF ( setinitval .EQ. 3 ) grid%isurban=0
IF ( setinitval .EQ. 3 ) grid%isoilwater=0
IF ( setinitval .EQ. 3 ) grid%map_proj=0
IF ( setinitval .EQ. 3 ) grid%simulation_start_year=0
IF ( setinitval .EQ. 3 ) grid%simulation_start_month=0
IF ( setinitval .EQ. 3 ) grid%simulation_start_day=0
IF ( setinitval .EQ. 3 ) grid%simulation_start_hour=0
IF ( setinitval .EQ. 3 ) grid%simulation_start_minute=0
IF ( setinitval .EQ. 3 ) grid%simulation_start_second=0
!ENDOFREGISTRYGENERATEDINCLUDE

      ELSE
      
        WRITE( wrf_err_message , * )&
          'Invalid specification of dynamics: dyn_opt = ',dyn_opt
        CALL wrf_error_fatal3 ( "module_domain.b" , 1106 ,  TRIM ( wrf_err_message ) )
      ENDIF

      WRITE(message,*)&
          'alloc_space_field: domain ',id,' ',num_bytes_allocated
      CALL  wrf_debug( 1, message )

   END SUBROUTINE alloc_space_field


!  This routine is used to DEALLOCATE space for a single domain and remove 
!  it from the linked list.  First the pointers in the linked list are fixed 
!  (so the one in the middle can be removed).  Then the domain itself is 
!  DEALLOCATEd via a call to domain_destroy().  

   SUBROUTINE dealloc_space_domain ( id )
      
      IMPLICIT NONE

      !  Input data.

      INTEGER , INTENT(IN)            :: id

      !  Local data.

      TYPE(domain) , POINTER          :: grid
      LOGICAL                         :: found

      !  Initializations required to start the routine.

      grid => head_grid
      old_grid => head_grid
      found = .FALSE.

      !  The identity of the domain to delete is based upon the "id".
      !  We search all of the possible grids.  It is required to find a domain
      !  otherwise it is a fatal error.  

      find_grid : DO WHILE ( ASSOCIATED(grid) ) 
         IF ( grid%id == id ) THEN
            found = .TRUE.
            old_grid%next => grid%next
            CALL domain_destroy( grid )
            EXIT find_grid
         END IF
         old_grid => grid
         grid     => grid%next
      END DO find_grid

      IF ( .NOT. found ) THEN
         WRITE ( wrf_err_message , * ) 'module_domain: ', &
           'dealloc_space_domain: Could not de-allocate grid id ',id
         CALL wrf_error_fatal3 ( "module_domain.b" , 1158 ,  TRIM( wrf_err_message ) ) 
      END IF

   END SUBROUTINE dealloc_space_domain



!  This routine is used to DEALLOCATE space for a single domain type.  
!  First, the field data are all removed through a CALL to the 
!  dealloc_space_field routine.  Then the pointer to the domain
!  itself is DEALLOCATEd.

   SUBROUTINE domain_destroy ( grid )
      
      IMPLICIT NONE

      !  Input data.

      TYPE(domain) , POINTER          :: grid

      CALL dealloc_space_field ( grid )
      DEALLOCATE( grid%parents )
      DEALLOCATE( grid%nests )
      ! clean up time manager bits
      CALL domain_clock_destroy( grid )
      CALL domain_alarms_destroy( grid )
      IF ( ASSOCIATED( grid%i_start ) ) THEN
        DEALLOCATE( grid%i_start ) 
      ENDIF
      IF ( ASSOCIATED( grid%i_end ) ) THEN
        DEALLOCATE( grid%i_end )
      ENDIF
      IF ( ASSOCIATED( grid%j_start ) ) THEN
        DEALLOCATE( grid%j_start )
      ENDIF
      IF ( ASSOCIATED( grid%j_end ) ) THEN
        DEALLOCATE( grid%j_end )
      ENDIF
      DEALLOCATE( grid )
      NULLIFY( grid )

   END SUBROUTINE domain_destroy

   RECURSIVE SUBROUTINE show_nest_subtree ( grid )
      TYPE(domain), POINTER :: grid
      INTEGER myid
      INTEGER kid
      IF ( .NOT. ASSOCIATED( grid ) ) RETURN
      myid = grid%id
      write(0,*)'show_nest_subtree ',myid
      DO kid = 1, max_nests
        IF ( ASSOCIATED( grid%nests(kid)%ptr ) ) THEN
          IF ( grid%nests(kid)%ptr%id .EQ. myid ) THEN
            CALL wrf_error_fatal3 ( "module_domain.b" , 1211 ,  'show_nest_subtree: nest hierarchy corrupted' )
          ENDIF
          CALL show_nest_subtree( grid%nests(kid)%ptr )
        ENDIF
      ENDDO
   END SUBROUTINE show_nest_subtree
   

!

!  This routine DEALLOCATEs each gridded field for this domain.  For each type of
!  different array (1d, 2d, 3d, etc.), the space for each pointer is DEALLOCATEd
!  for every -1 (i.e., each different meteorological field).

   SUBROUTINE dealloc_space_field ( grid )
      
      IMPLICIT NONE

      !  Input data.

      TYPE(domain)              , POINTER :: grid

      !  Local data.

      INTEGER                             :: dyn_opt, ierr

      CALL nl_get_dyn_opt( 1, dyn_opt )

      IF      ( .FALSE. )           THEN

      ELSE IF ( dyn_opt == DYN_NMM ) THEN
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/nmm_deallocs.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
IF ( ASSOCIATED( grid%lu_index ) ) THEN 
  DEALLOCATE(grid%lu_index,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%lu_index. ')
 endif
  NULLIFY(grid%lu_index)
ENDIF
IF ( ASSOCIATED( grid%lu_mask ) ) THEN 
  DEALLOCATE(grid%lu_mask,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%lu_mask. ')
 endif
  NULLIFY(grid%lu_mask)
ENDIF
IF ( ASSOCIATED( grid%nmm_p_gc ) ) THEN 
  DEALLOCATE(grid%nmm_p_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_p_gc. ')
 endif
  NULLIFY(grid%nmm_p_gc)
ENDIF
IF ( ASSOCIATED( grid%vegcat ) ) THEN 
  DEALLOCATE(grid%vegcat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%vegcat. ')
 endif
  NULLIFY(grid%vegcat)
ENDIF
IF ( ASSOCIATED( grid%soilcat ) ) THEN 
  DEALLOCATE(grid%soilcat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilcat. ')
 endif
  NULLIFY(grid%soilcat)
ENDIF
IF ( ASSOCIATED( grid%input_soil_cat ) ) THEN 
  DEALLOCATE(grid%input_soil_cat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%input_soil_cat. ')
 endif
  NULLIFY(grid%input_soil_cat)
ENDIF
IF ( ASSOCIATED( grid%nmm_tsk_gc ) ) THEN 
  DEALLOCATE(grid%nmm_tsk_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_tsk_gc. ')
 endif
  NULLIFY(grid%nmm_tsk_gc)
ENDIF
IF ( ASSOCIATED( grid%xice_gc ) ) THEN 
  DEALLOCATE(grid%xice_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%xice_gc. ')
 endif
  NULLIFY(grid%xice_gc)
ENDIF
IF ( ASSOCIATED( grid%nmm_ght_gc ) ) THEN 
  DEALLOCATE(grid%nmm_ght_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_ght_gc. ')
 endif
  NULLIFY(grid%nmm_ght_gc)
ENDIF
IF ( ASSOCIATED( grid%nmm_rh_gc ) ) THEN 
  DEALLOCATE(grid%nmm_rh_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_rh_gc. ')
 endif
  NULLIFY(grid%nmm_rh_gc)
ENDIF
IF ( ASSOCIATED( grid%nmm_v_gc ) ) THEN 
  DEALLOCATE(grid%nmm_v_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_v_gc. ')
 endif
  NULLIFY(grid%nmm_v_gc)
ENDIF
IF ( ASSOCIATED( grid%nmm_u_gc ) ) THEN 
  DEALLOCATE(grid%nmm_u_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_u_gc. ')
 endif
  NULLIFY(grid%nmm_u_gc)
ENDIF
IF ( ASSOCIATED( grid%nmm_t_gc ) ) THEN 
  DEALLOCATE(grid%nmm_t_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_t_gc. ')
 endif
  NULLIFY(grid%nmm_t_gc)
ENDIF
IF ( ASSOCIATED( grid%nmm_rwmr_gc ) ) THEN 
  DEALLOCATE(grid%nmm_rwmr_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_rwmr_gc. ')
 endif
  NULLIFY(grid%nmm_rwmr_gc)
ENDIF
IF ( ASSOCIATED( grid%nmm_snmr_gc ) ) THEN 
  DEALLOCATE(grid%nmm_snmr_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_snmr_gc. ')
 endif
  NULLIFY(grid%nmm_snmr_gc)
ENDIF
IF ( ASSOCIATED( grid%nmm_clwmr_gc ) ) THEN 
  DEALLOCATE(grid%nmm_clwmr_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_clwmr_gc. ')
 endif
  NULLIFY(grid%nmm_clwmr_gc)
ENDIF
IF ( ASSOCIATED( grid%nmm_cice_gc ) ) THEN 
  DEALLOCATE(grid%nmm_cice_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_cice_gc. ')
 endif
  NULLIFY(grid%nmm_cice_gc)
ENDIF
IF ( ASSOCIATED( grid%nmm_rimef_gc ) ) THEN 
  DEALLOCATE(grid%nmm_rimef_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_rimef_gc. ')
 endif
  NULLIFY(grid%nmm_rimef_gc)
ENDIF
IF ( ASSOCIATED( grid%snoalb ) ) THEN 
  DEALLOCATE(grid%snoalb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%snoalb. ')
 endif
  NULLIFY(grid%snoalb)
ENDIF
IF ( ASSOCIATED( grid%nmm_greenfrac_gc ) ) THEN 
  DEALLOCATE(grid%nmm_greenfrac_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_greenfrac_gc. ')
 endif
  NULLIFY(grid%nmm_greenfrac_gc)
ENDIF
IF ( ASSOCIATED( grid%nmm_albedo12m_gc ) ) THEN 
  DEALLOCATE(grid%nmm_albedo12m_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_albedo12m_gc. ')
 endif
  NULLIFY(grid%nmm_albedo12m_gc)
ENDIF
IF ( ASSOCIATED( grid%soilcbot_gc ) ) THEN 
  DEALLOCATE(grid%soilcbot_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilcbot_gc. ')
 endif
  NULLIFY(grid%soilcbot_gc)
ENDIF
IF ( ASSOCIATED( grid%soilctop_gc ) ) THEN 
  DEALLOCATE(grid%soilctop_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilctop_gc. ')
 endif
  NULLIFY(grid%soilctop_gc)
ENDIF
IF ( ASSOCIATED( grid%nmm_tmn_gc ) ) THEN 
  DEALLOCATE(grid%nmm_tmn_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_tmn_gc. ')
 endif
  NULLIFY(grid%nmm_tmn_gc)
ENDIF
IF ( ASSOCIATED( grid%nmm_htv_gc ) ) THEN 
  DEALLOCATE(grid%nmm_htv_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_htv_gc. ')
 endif
  NULLIFY(grid%nmm_htv_gc)
ENDIF
IF ( ASSOCIATED( grid%nmm_ht_gc ) ) THEN 
  DEALLOCATE(grid%nmm_ht_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_ht_gc. ')
 endif
  NULLIFY(grid%nmm_ht_gc)
ENDIF
IF ( ASSOCIATED( grid%landusef_gc ) ) THEN 
  DEALLOCATE(grid%landusef_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%landusef_gc. ')
 endif
  NULLIFY(grid%landusef_gc)
ENDIF
IF ( ASSOCIATED( grid%nmm_vlon_gc ) ) THEN 
  DEALLOCATE(grid%nmm_vlon_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_vlon_gc. ')
 endif
  NULLIFY(grid%nmm_vlon_gc)
ENDIF
IF ( ASSOCIATED( grid%nmm_vlat_gc ) ) THEN 
  DEALLOCATE(grid%nmm_vlat_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_vlat_gc. ')
 endif
  NULLIFY(grid%nmm_vlat_gc)
ENDIF
IF ( ASSOCIATED( grid%nmm_hlon_gc ) ) THEN 
  DEALLOCATE(grid%nmm_hlon_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_hlon_gc. ')
 endif
  NULLIFY(grid%nmm_hlon_gc)
ENDIF
IF ( ASSOCIATED( grid%nmm_hlat_gc ) ) THEN 
  DEALLOCATE(grid%nmm_hlat_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_hlat_gc. ')
 endif
  NULLIFY(grid%nmm_hlat_gc)
ENDIF
IF ( ASSOCIATED( grid%nmm_hbm2 ) ) THEN 
  DEALLOCATE(grid%nmm_hbm2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_hbm2. ')
 endif
  NULLIFY(grid%nmm_hbm2)
ENDIF
IF ( ASSOCIATED( grid%nmm_hbm3 ) ) THEN 
  DEALLOCATE(grid%nmm_hbm3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_hbm3. ')
 endif
  NULLIFY(grid%nmm_hbm3)
ENDIF
IF ( ASSOCIATED( grid%nmm_vbm2 ) ) THEN 
  DEALLOCATE(grid%nmm_vbm2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_vbm2. ')
 endif
  NULLIFY(grid%nmm_vbm2)
ENDIF
IF ( ASSOCIATED( grid%nmm_vbm3 ) ) THEN 
  DEALLOCATE(grid%nmm_vbm3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_vbm3. ')
 endif
  NULLIFY(grid%nmm_vbm3)
ENDIF
IF ( ASSOCIATED( grid%nmm_sm ) ) THEN 
  DEALLOCATE(grid%nmm_sm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_sm. ')
 endif
  NULLIFY(grid%nmm_sm)
ENDIF
IF ( ASSOCIATED( grid%nmm_sice ) ) THEN 
  DEALLOCATE(grid%nmm_sice,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_sice. ')
 endif
  NULLIFY(grid%nmm_sice)
ENDIF
IF ( ASSOCIATED( grid%nmm_pd ) ) THEN 
  DEALLOCATE(grid%nmm_pd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_pd. ')
 endif
  NULLIFY(grid%nmm_pd)
ENDIF
IF ( ASSOCIATED( grid%nmm_pd_b ) ) THEN 
  DEALLOCATE(grid%nmm_pd_b,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_pd_b. ')
 endif
  NULLIFY(grid%nmm_pd_b)
ENDIF
IF ( ASSOCIATED( grid%nmm_pd_bt ) ) THEN 
  DEALLOCATE(grid%nmm_pd_bt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_pd_bt. ')
 endif
  NULLIFY(grid%nmm_pd_bt)
ENDIF
IF ( ASSOCIATED( grid%nmm_fis ) ) THEN 
  DEALLOCATE(grid%nmm_fis,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_fis. ')
 endif
  NULLIFY(grid%nmm_fis)
ENDIF
IF ( ASSOCIATED( grid%nmm_res ) ) THEN 
  DEALLOCATE(grid%nmm_res,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_res. ')
 endif
  NULLIFY(grid%nmm_res)
ENDIF
IF ( ASSOCIATED( grid%nmm_t ) ) THEN 
  DEALLOCATE(grid%nmm_t,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_t. ')
 endif
  NULLIFY(grid%nmm_t)
ENDIF
IF ( ASSOCIATED( grid%nmm_t_b ) ) THEN 
  DEALLOCATE(grid%nmm_t_b,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_t_b. ')
 endif
  NULLIFY(grid%nmm_t_b)
ENDIF
IF ( ASSOCIATED( grid%nmm_t_bt ) ) THEN 
  DEALLOCATE(grid%nmm_t_bt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_t_bt. ')
 endif
  NULLIFY(grid%nmm_t_bt)
ENDIF
IF ( ASSOCIATED( grid%nmm_q ) ) THEN 
  DEALLOCATE(grid%nmm_q,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_q. ')
 endif
  NULLIFY(grid%nmm_q)
ENDIF
IF ( ASSOCIATED( grid%nmm_q_b ) ) THEN 
  DEALLOCATE(grid%nmm_q_b,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_q_b. ')
 endif
  NULLIFY(grid%nmm_q_b)
ENDIF
IF ( ASSOCIATED( grid%nmm_q_bt ) ) THEN 
  DEALLOCATE(grid%nmm_q_bt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_q_bt. ')
 endif
  NULLIFY(grid%nmm_q_bt)
ENDIF
IF ( ASSOCIATED( grid%nmm_u ) ) THEN 
  DEALLOCATE(grid%nmm_u,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_u. ')
 endif
  NULLIFY(grid%nmm_u)
ENDIF
IF ( ASSOCIATED( grid%nmm_u_b ) ) THEN 
  DEALLOCATE(grid%nmm_u_b,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_u_b. ')
 endif
  NULLIFY(grid%nmm_u_b)
ENDIF
IF ( ASSOCIATED( grid%nmm_u_bt ) ) THEN 
  DEALLOCATE(grid%nmm_u_bt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_u_bt. ')
 endif
  NULLIFY(grid%nmm_u_bt)
ENDIF
IF ( ASSOCIATED( grid%nmm_v ) ) THEN 
  DEALLOCATE(grid%nmm_v,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_v. ')
 endif
  NULLIFY(grid%nmm_v)
ENDIF
IF ( ASSOCIATED( grid%nmm_v_b ) ) THEN 
  DEALLOCATE(grid%nmm_v_b,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_v_b. ')
 endif
  NULLIFY(grid%nmm_v_b)
ENDIF
IF ( ASSOCIATED( grid%nmm_v_bt ) ) THEN 
  DEALLOCATE(grid%nmm_v_bt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_v_bt. ')
 endif
  NULLIFY(grid%nmm_v_bt)
ENDIF
IF ( ASSOCIATED( grid%nmm_told ) ) THEN 
  DEALLOCATE(grid%nmm_told,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_told. ')
 endif
  NULLIFY(grid%nmm_told)
ENDIF
IF ( ASSOCIATED( grid%nmm_uold ) ) THEN 
  DEALLOCATE(grid%nmm_uold,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_uold. ')
 endif
  NULLIFY(grid%nmm_uold)
ENDIF
IF ( ASSOCIATED( grid%nmm_vold ) ) THEN 
  DEALLOCATE(grid%nmm_vold,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_vold. ')
 endif
  NULLIFY(grid%nmm_vold)
ENDIF
IF ( ASSOCIATED( grid%nmm_dx_nmm ) ) THEN 
  DEALLOCATE(grid%nmm_dx_nmm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_dx_nmm. ')
 endif
  NULLIFY(grid%nmm_dx_nmm)
ENDIF
IF ( ASSOCIATED( grid%nmm_wpdar ) ) THEN 
  DEALLOCATE(grid%nmm_wpdar,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_wpdar. ')
 endif
  NULLIFY(grid%nmm_wpdar)
ENDIF
IF ( ASSOCIATED( grid%nmm_cpgfu ) ) THEN 
  DEALLOCATE(grid%nmm_cpgfu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_cpgfu. ')
 endif
  NULLIFY(grid%nmm_cpgfu)
ENDIF
IF ( ASSOCIATED( grid%nmm_curv ) ) THEN 
  DEALLOCATE(grid%nmm_curv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_curv. ')
 endif
  NULLIFY(grid%nmm_curv)
ENDIF
IF ( ASSOCIATED( grid%nmm_fcp ) ) THEN 
  DEALLOCATE(grid%nmm_fcp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_fcp. ')
 endif
  NULLIFY(grid%nmm_fcp)
ENDIF
IF ( ASSOCIATED( grid%nmm_fdiv ) ) THEN 
  DEALLOCATE(grid%nmm_fdiv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_fdiv. ')
 endif
  NULLIFY(grid%nmm_fdiv)
ENDIF
IF ( ASSOCIATED( grid%nmm_f ) ) THEN 
  DEALLOCATE(grid%nmm_f,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_f. ')
 endif
  NULLIFY(grid%nmm_f)
ENDIF
IF ( ASSOCIATED( grid%nmm_fad ) ) THEN 
  DEALLOCATE(grid%nmm_fad,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_fad. ')
 endif
  NULLIFY(grid%nmm_fad)
ENDIF
IF ( ASSOCIATED( grid%nmm_ddmpu ) ) THEN 
  DEALLOCATE(grid%nmm_ddmpu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_ddmpu. ')
 endif
  NULLIFY(grid%nmm_ddmpu)
ENDIF
IF ( ASSOCIATED( grid%nmm_ddmpv ) ) THEN 
  DEALLOCATE(grid%nmm_ddmpv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_ddmpv. ')
 endif
  NULLIFY(grid%nmm_ddmpv)
ENDIF
IF ( ASSOCIATED( grid%nmm_deta ) ) THEN 
  DEALLOCATE(grid%nmm_deta,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_deta. ')
 endif
  NULLIFY(grid%nmm_deta)
ENDIF
IF ( ASSOCIATED( grid%nmm_rdeta ) ) THEN 
  DEALLOCATE(grid%nmm_rdeta,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_rdeta. ')
 endif
  NULLIFY(grid%nmm_rdeta)
ENDIF
IF ( ASSOCIATED( grid%nmm_aeta ) ) THEN 
  DEALLOCATE(grid%nmm_aeta,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_aeta. ')
 endif
  NULLIFY(grid%nmm_aeta)
ENDIF
IF ( ASSOCIATED( grid%nmm_f4q2 ) ) THEN 
  DEALLOCATE(grid%nmm_f4q2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_f4q2. ')
 endif
  NULLIFY(grid%nmm_f4q2)
ENDIF
IF ( ASSOCIATED( grid%nmm_etax ) ) THEN 
  DEALLOCATE(grid%nmm_etax,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_etax. ')
 endif
  NULLIFY(grid%nmm_etax)
ENDIF
IF ( ASSOCIATED( grid%nmm_dfl ) ) THEN 
  DEALLOCATE(grid%nmm_dfl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_dfl. ')
 endif
  NULLIFY(grid%nmm_dfl)
ENDIF
IF ( ASSOCIATED( grid%nmm_deta1 ) ) THEN 
  DEALLOCATE(grid%nmm_deta1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_deta1. ')
 endif
  NULLIFY(grid%nmm_deta1)
ENDIF
IF ( ASSOCIATED( grid%nmm_aeta1 ) ) THEN 
  DEALLOCATE(grid%nmm_aeta1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_aeta1. ')
 endif
  NULLIFY(grid%nmm_aeta1)
ENDIF
IF ( ASSOCIATED( grid%nmm_eta1 ) ) THEN 
  DEALLOCATE(grid%nmm_eta1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_eta1. ')
 endif
  NULLIFY(grid%nmm_eta1)
ENDIF
IF ( ASSOCIATED( grid%nmm_deta2 ) ) THEN 
  DEALLOCATE(grid%nmm_deta2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_deta2. ')
 endif
  NULLIFY(grid%nmm_deta2)
ENDIF
IF ( ASSOCIATED( grid%nmm_aeta2 ) ) THEN 
  DEALLOCATE(grid%nmm_aeta2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_aeta2. ')
 endif
  NULLIFY(grid%nmm_aeta2)
ENDIF
IF ( ASSOCIATED( grid%nmm_eta2 ) ) THEN 
  DEALLOCATE(grid%nmm_eta2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_eta2. ')
 endif
  NULLIFY(grid%nmm_eta2)
ENDIF
IF ( ASSOCIATED( grid%nmm_em ) ) THEN 
  DEALLOCATE(grid%nmm_em,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_em. ')
 endif
  NULLIFY(grid%nmm_em)
ENDIF
IF ( ASSOCIATED( grid%nmm_emt ) ) THEN 
  DEALLOCATE(grid%nmm_emt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_emt. ')
 endif
  NULLIFY(grid%nmm_emt)
ENDIF
IF ( ASSOCIATED( grid%nmm_adt ) ) THEN 
  DEALLOCATE(grid%nmm_adt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_adt. ')
 endif
  NULLIFY(grid%nmm_adt)
ENDIF
IF ( ASSOCIATED( grid%nmm_adu ) ) THEN 
  DEALLOCATE(grid%nmm_adu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_adu. ')
 endif
  NULLIFY(grid%nmm_adu)
ENDIF
IF ( ASSOCIATED( grid%nmm_adv ) ) THEN 
  DEALLOCATE(grid%nmm_adv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_adv. ')
 endif
  NULLIFY(grid%nmm_adv)
ENDIF
IF ( ASSOCIATED( grid%nmm_em_loc ) ) THEN 
  DEALLOCATE(grid%nmm_em_loc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_em_loc. ')
 endif
  NULLIFY(grid%nmm_em_loc)
ENDIF
IF ( ASSOCIATED( grid%nmm_emt_loc ) ) THEN 
  DEALLOCATE(grid%nmm_emt_loc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_emt_loc. ')
 endif
  NULLIFY(grid%nmm_emt_loc)
ENDIF
IF ( ASSOCIATED( grid%nmm_pdsl ) ) THEN 
  DEALLOCATE(grid%nmm_pdsl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_pdsl. ')
 endif
  NULLIFY(grid%nmm_pdsl)
ENDIF
IF ( ASSOCIATED( grid%nmm_pdslo ) ) THEN 
  DEALLOCATE(grid%nmm_pdslo,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_pdslo. ')
 endif
  NULLIFY(grid%nmm_pdslo)
ENDIF
IF ( ASSOCIATED( grid%nmm_psdt ) ) THEN 
  DEALLOCATE(grid%nmm_psdt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_psdt. ')
 endif
  NULLIFY(grid%nmm_psdt)
ENDIF
IF ( ASSOCIATED( grid%nmm_div ) ) THEN 
  DEALLOCATE(grid%nmm_div,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_div. ')
 endif
  NULLIFY(grid%nmm_div)
ENDIF
IF ( ASSOCIATED( grid%nmm_few ) ) THEN 
  DEALLOCATE(grid%nmm_few,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_few. ')
 endif
  NULLIFY(grid%nmm_few)
ENDIF
IF ( ASSOCIATED( grid%nmm_fne ) ) THEN 
  DEALLOCATE(grid%nmm_fne,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_fne. ')
 endif
  NULLIFY(grid%nmm_fne)
ENDIF
IF ( ASSOCIATED( grid%nmm_fns ) ) THEN 
  DEALLOCATE(grid%nmm_fns,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_fns. ')
 endif
  NULLIFY(grid%nmm_fns)
ENDIF
IF ( ASSOCIATED( grid%nmm_fse ) ) THEN 
  DEALLOCATE(grid%nmm_fse,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_fse. ')
 endif
  NULLIFY(grid%nmm_fse)
ENDIF
IF ( ASSOCIATED( grid%nmm_omgalf ) ) THEN 
  DEALLOCATE(grid%nmm_omgalf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_omgalf. ')
 endif
  NULLIFY(grid%nmm_omgalf)
ENDIF
IF ( ASSOCIATED( grid%nmm_petdt ) ) THEN 
  DEALLOCATE(grid%nmm_petdt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_petdt. ')
 endif
  NULLIFY(grid%nmm_petdt)
ENDIF
IF ( ASSOCIATED( grid%nmm_rtop ) ) THEN 
  DEALLOCATE(grid%nmm_rtop,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_rtop. ')
 endif
  NULLIFY(grid%nmm_rtop)
ENDIF
IF ( ASSOCIATED( grid%nmm_pblh ) ) THEN 
  DEALLOCATE(grid%nmm_pblh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_pblh. ')
 endif
  NULLIFY(grid%nmm_pblh)
ENDIF
IF ( ASSOCIATED( grid%nmm_lpbl ) ) THEN 
  DEALLOCATE(grid%nmm_lpbl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_lpbl. ')
 endif
  NULLIFY(grid%nmm_lpbl)
ENDIF
IF ( ASSOCIATED( grid%nmm_ustar ) ) THEN 
  DEALLOCATE(grid%nmm_ustar,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_ustar. ')
 endif
  NULLIFY(grid%nmm_ustar)
ENDIF
IF ( ASSOCIATED( grid%nmm_z0 ) ) THEN 
  DEALLOCATE(grid%nmm_z0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_z0. ')
 endif
  NULLIFY(grid%nmm_z0)
ENDIF
IF ( ASSOCIATED( grid%nmm_z0base ) ) THEN 
  DEALLOCATE(grid%nmm_z0base,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_z0base. ')
 endif
  NULLIFY(grid%nmm_z0base)
ENDIF
IF ( ASSOCIATED( grid%nmm_ths ) ) THEN 
  DEALLOCATE(grid%nmm_ths,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_ths. ')
 endif
  NULLIFY(grid%nmm_ths)
ENDIF
IF ( ASSOCIATED( grid%nmm_mavail ) ) THEN 
  DEALLOCATE(grid%nmm_mavail,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_mavail. ')
 endif
  NULLIFY(grid%nmm_mavail)
ENDIF
IF ( ASSOCIATED( grid%nmm_qsh ) ) THEN 
  DEALLOCATE(grid%nmm_qsh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_qsh. ')
 endif
  NULLIFY(grid%nmm_qsh)
ENDIF
IF ( ASSOCIATED( grid%nmm_twbs ) ) THEN 
  DEALLOCATE(grid%nmm_twbs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_twbs. ')
 endif
  NULLIFY(grid%nmm_twbs)
ENDIF
IF ( ASSOCIATED( grid%nmm_qwbs ) ) THEN 
  DEALLOCATE(grid%nmm_qwbs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_qwbs. ')
 endif
  NULLIFY(grid%nmm_qwbs)
ENDIF
IF ( ASSOCIATED( grid%nmm_prec ) ) THEN 
  DEALLOCATE(grid%nmm_prec,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_prec. ')
 endif
  NULLIFY(grid%nmm_prec)
ENDIF
IF ( ASSOCIATED( grid%nmm_aprec ) ) THEN 
  DEALLOCATE(grid%nmm_aprec,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_aprec. ')
 endif
  NULLIFY(grid%nmm_aprec)
ENDIF
IF ( ASSOCIATED( grid%nmm_acprec ) ) THEN 
  DEALLOCATE(grid%nmm_acprec,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_acprec. ')
 endif
  NULLIFY(grid%nmm_acprec)
ENDIF
IF ( ASSOCIATED( grid%nmm_cuprec ) ) THEN 
  DEALLOCATE(grid%nmm_cuprec,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_cuprec. ')
 endif
  NULLIFY(grid%nmm_cuprec)
ENDIF
IF ( ASSOCIATED( grid%nmm_lspa ) ) THEN 
  DEALLOCATE(grid%nmm_lspa,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_lspa. ')
 endif
  NULLIFY(grid%nmm_lspa)
ENDIF
IF ( ASSOCIATED( grid%nmm_ddata ) ) THEN 
  DEALLOCATE(grid%nmm_ddata,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_ddata. ')
 endif
  NULLIFY(grid%nmm_ddata)
ENDIF
IF ( ASSOCIATED( grid%nmm_accliq ) ) THEN 
  DEALLOCATE(grid%nmm_accliq,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_accliq. ')
 endif
  NULLIFY(grid%nmm_accliq)
ENDIF
IF ( ASSOCIATED( grid%nmm_sno ) ) THEN 
  DEALLOCATE(grid%nmm_sno,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_sno. ')
 endif
  NULLIFY(grid%nmm_sno)
ENDIF
IF ( ASSOCIATED( grid%nmm_si ) ) THEN 
  DEALLOCATE(grid%nmm_si,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_si. ')
 endif
  NULLIFY(grid%nmm_si)
ENDIF
IF ( ASSOCIATED( grid%nmm_cldefi ) ) THEN 
  DEALLOCATE(grid%nmm_cldefi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_cldefi. ')
 endif
  NULLIFY(grid%nmm_cldefi)
ENDIF
IF ( ASSOCIATED( grid%nmm_deep ) ) THEN 
  DEALLOCATE(grid%nmm_deep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_deep. ')
 endif
  NULLIFY(grid%nmm_deep)
ENDIF
IF ( ASSOCIATED( grid%nmm_rf ) ) THEN 
  DEALLOCATE(grid%nmm_rf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_rf. ')
 endif
  NULLIFY(grid%nmm_rf)
ENDIF
IF ( ASSOCIATED( grid%nmm_th10 ) ) THEN 
  DEALLOCATE(grid%nmm_th10,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_th10. ')
 endif
  NULLIFY(grid%nmm_th10)
ENDIF
IF ( ASSOCIATED( grid%nmm_q10 ) ) THEN 
  DEALLOCATE(grid%nmm_q10,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_q10. ')
 endif
  NULLIFY(grid%nmm_q10)
ENDIF
IF ( ASSOCIATED( grid%nmm_pshltr ) ) THEN 
  DEALLOCATE(grid%nmm_pshltr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_pshltr. ')
 endif
  NULLIFY(grid%nmm_pshltr)
ENDIF
IF ( ASSOCIATED( grid%nmm_tshltr ) ) THEN 
  DEALLOCATE(grid%nmm_tshltr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_tshltr. ')
 endif
  NULLIFY(grid%nmm_tshltr)
ENDIF
IF ( ASSOCIATED( grid%nmm_qshltr ) ) THEN 
  DEALLOCATE(grid%nmm_qshltr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_qshltr. ')
 endif
  NULLIFY(grid%nmm_qshltr)
ENDIF
IF ( ASSOCIATED( grid%nmm_q2 ) ) THEN 
  DEALLOCATE(grid%nmm_q2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_q2. ')
 endif
  NULLIFY(grid%nmm_q2)
ENDIF
IF ( ASSOCIATED( grid%nmm_q2_b ) ) THEN 
  DEALLOCATE(grid%nmm_q2_b,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_q2_b. ')
 endif
  NULLIFY(grid%nmm_q2_b)
ENDIF
IF ( ASSOCIATED( grid%nmm_q2_bt ) ) THEN 
  DEALLOCATE(grid%nmm_q2_bt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_q2_bt. ')
 endif
  NULLIFY(grid%nmm_q2_bt)
ENDIF
IF ( ASSOCIATED( grid%nmm_t_adj ) ) THEN 
  DEALLOCATE(grid%nmm_t_adj,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_t_adj. ')
 endif
  NULLIFY(grid%nmm_t_adj)
ENDIF
IF ( ASSOCIATED( grid%nmm_t_old ) ) THEN 
  DEALLOCATE(grid%nmm_t_old,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_t_old. ')
 endif
  NULLIFY(grid%nmm_t_old)
ENDIF
IF ( ASSOCIATED( grid%nmm_zero_3d ) ) THEN 
  DEALLOCATE(grid%nmm_zero_3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_zero_3d. ')
 endif
  NULLIFY(grid%nmm_zero_3d)
ENDIF
IF ( ASSOCIATED( grid%nmm_w0avg ) ) THEN 
  DEALLOCATE(grid%nmm_w0avg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_w0avg. ')
 endif
  NULLIFY(grid%nmm_w0avg)
ENDIF
IF ( ASSOCIATED( grid%nmm_akhs_out ) ) THEN 
  DEALLOCATE(grid%nmm_akhs_out,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_akhs_out. ')
 endif
  NULLIFY(grid%nmm_akhs_out)
ENDIF
IF ( ASSOCIATED( grid%nmm_akms_out ) ) THEN 
  DEALLOCATE(grid%nmm_akms_out,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_akms_out. ')
 endif
  NULLIFY(grid%nmm_akms_out)
ENDIF
IF ( ASSOCIATED( grid%nmm_albase ) ) THEN 
  DEALLOCATE(grid%nmm_albase,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_albase. ')
 endif
  NULLIFY(grid%nmm_albase)
ENDIF
IF ( ASSOCIATED( grid%nmm_albedo ) ) THEN 
  DEALLOCATE(grid%nmm_albedo,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_albedo. ')
 endif
  NULLIFY(grid%nmm_albedo)
ENDIF
IF ( ASSOCIATED( grid%nmm_cnvbot ) ) THEN 
  DEALLOCATE(grid%nmm_cnvbot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_cnvbot. ')
 endif
  NULLIFY(grid%nmm_cnvbot)
ENDIF
IF ( ASSOCIATED( grid%nmm_cnvtop ) ) THEN 
  DEALLOCATE(grid%nmm_cnvtop,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_cnvtop. ')
 endif
  NULLIFY(grid%nmm_cnvtop)
ENDIF
IF ( ASSOCIATED( grid%nmm_czen ) ) THEN 
  DEALLOCATE(grid%nmm_czen,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_czen. ')
 endif
  NULLIFY(grid%nmm_czen)
ENDIF
IF ( ASSOCIATED( grid%nmm_czmean ) ) THEN 
  DEALLOCATE(grid%nmm_czmean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_czmean. ')
 endif
  NULLIFY(grid%nmm_czmean)
ENDIF
IF ( ASSOCIATED( grid%nmm_epsr ) ) THEN 
  DEALLOCATE(grid%nmm_epsr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_epsr. ')
 endif
  NULLIFY(grid%nmm_epsr)
ENDIF
IF ( ASSOCIATED( grid%nmm_gffc ) ) THEN 
  DEALLOCATE(grid%nmm_gffc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_gffc. ')
 endif
  NULLIFY(grid%nmm_gffc)
ENDIF
IF ( ASSOCIATED( grid%nmm_glat ) ) THEN 
  DEALLOCATE(grid%nmm_glat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_glat. ')
 endif
  NULLIFY(grid%nmm_glat)
ENDIF
IF ( ASSOCIATED( grid%nmm_glon ) ) THEN 
  DEALLOCATE(grid%nmm_glon,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_glon. ')
 endif
  NULLIFY(grid%nmm_glon)
ENDIF
IF ( ASSOCIATED( grid%nmm_nmm_tsk ) ) THEN 
  DEALLOCATE(grid%nmm_nmm_tsk,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_nmm_tsk. ')
 endif
  NULLIFY(grid%nmm_nmm_tsk)
ENDIF
IF ( ASSOCIATED( grid%nmm_hdac ) ) THEN 
  DEALLOCATE(grid%nmm_hdac,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_hdac. ')
 endif
  NULLIFY(grid%nmm_hdac)
ENDIF
IF ( ASSOCIATED( grid%nmm_hdacv ) ) THEN 
  DEALLOCATE(grid%nmm_hdacv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_hdacv. ')
 endif
  NULLIFY(grid%nmm_hdacv)
ENDIF
IF ( ASSOCIATED( grid%nmm_mxsnal ) ) THEN 
  DEALLOCATE(grid%nmm_mxsnal,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_mxsnal. ')
 endif
  NULLIFY(grid%nmm_mxsnal)
ENDIF
IF ( ASSOCIATED( grid%nmm_radin ) ) THEN 
  DEALLOCATE(grid%nmm_radin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_radin. ')
 endif
  NULLIFY(grid%nmm_radin)
ENDIF
IF ( ASSOCIATED( grid%nmm_radot ) ) THEN 
  DEALLOCATE(grid%nmm_radot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_radot. ')
 endif
  NULLIFY(grid%nmm_radot)
ENDIF
IF ( ASSOCIATED( grid%nmm_sigt4 ) ) THEN 
  DEALLOCATE(grid%nmm_sigt4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_sigt4. ')
 endif
  NULLIFY(grid%nmm_sigt4)
ENDIF
IF ( ASSOCIATED( grid%nmm_tg ) ) THEN 
  DEALLOCATE(grid%nmm_tg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_tg. ')
 endif
  NULLIFY(grid%nmm_tg)
ENDIF
IF ( ASSOCIATED( grid%nmm_dfrlg ) ) THEN 
  DEALLOCATE(grid%nmm_dfrlg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_dfrlg. ')
 endif
  NULLIFY(grid%nmm_dfrlg)
ENDIF
IF ( ASSOCIATED( grid%nmm_lvl ) ) THEN 
  DEALLOCATE(grid%nmm_lvl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_lvl. ')
 endif
  NULLIFY(grid%nmm_lvl)
ENDIF
IF ( ASSOCIATED( grid%nmm_cwm ) ) THEN 
  DEALLOCATE(grid%nmm_cwm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_cwm. ')
 endif
  NULLIFY(grid%nmm_cwm)
ENDIF
IF ( ASSOCIATED( grid%nmm_cwm_b ) ) THEN 
  DEALLOCATE(grid%nmm_cwm_b,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_cwm_b. ')
 endif
  NULLIFY(grid%nmm_cwm_b)
ENDIF
IF ( ASSOCIATED( grid%nmm_cwm_bt ) ) THEN 
  DEALLOCATE(grid%nmm_cwm_bt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_cwm_bt. ')
 endif
  NULLIFY(grid%nmm_cwm_bt)
ENDIF
IF ( ASSOCIATED( grid%nmm_f_ice ) ) THEN 
  DEALLOCATE(grid%nmm_f_ice,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_f_ice. ')
 endif
  NULLIFY(grid%nmm_f_ice)
ENDIF
IF ( ASSOCIATED( grid%nmm_f_rain ) ) THEN 
  DEALLOCATE(grid%nmm_f_rain,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_f_rain. ')
 endif
  NULLIFY(grid%nmm_f_rain)
ENDIF
IF ( ASSOCIATED( grid%nmm_f_rimef ) ) THEN 
  DEALLOCATE(grid%nmm_f_rimef,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_f_rimef. ')
 endif
  NULLIFY(grid%nmm_f_rimef)
ENDIF
IF ( ASSOCIATED( grid%nmm_cldfra ) ) THEN 
  DEALLOCATE(grid%nmm_cldfra,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_cldfra. ')
 endif
  NULLIFY(grid%nmm_cldfra)
ENDIF
IF ( ASSOCIATED( grid%nmm_sr ) ) THEN 
  DEALLOCATE(grid%nmm_sr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_sr. ')
 endif
  NULLIFY(grid%nmm_sr)
ENDIF
IF ( ASSOCIATED( grid%nmm_cfrach ) ) THEN 
  DEALLOCATE(grid%nmm_cfrach,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_cfrach. ')
 endif
  NULLIFY(grid%nmm_cfrach)
ENDIF
IF ( ASSOCIATED( grid%nmm_cfracl ) ) THEN 
  DEALLOCATE(grid%nmm_cfracl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_cfracl. ')
 endif
  NULLIFY(grid%nmm_cfracl)
ENDIF
IF ( ASSOCIATED( grid%nmm_cfracm ) ) THEN 
  DEALLOCATE(grid%nmm_cfracm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_cfracm. ')
 endif
  NULLIFY(grid%nmm_cfracm)
ENDIF
IF ( ASSOCIATED( grid%nmm_islope ) ) THEN 
  DEALLOCATE(grid%nmm_islope,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_islope. ')
 endif
  NULLIFY(grid%nmm_islope)
ENDIF
IF ( ASSOCIATED( grid%nmm_dzsoil ) ) THEN 
  DEALLOCATE(grid%nmm_dzsoil,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_dzsoil. ')
 endif
  NULLIFY(grid%nmm_dzsoil)
ENDIF
IF ( ASSOCIATED( grid%nmm_rtdpth ) ) THEN 
  DEALLOCATE(grid%nmm_rtdpth,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_rtdpth. ')
 endif
  NULLIFY(grid%nmm_rtdpth)
ENDIF
IF ( ASSOCIATED( grid%nmm_sldpth ) ) THEN 
  DEALLOCATE(grid%nmm_sldpth,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_sldpth. ')
 endif
  NULLIFY(grid%nmm_sldpth)
ENDIF
IF ( ASSOCIATED( grid%nmm_cmc ) ) THEN 
  DEALLOCATE(grid%nmm_cmc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_cmc. ')
 endif
  NULLIFY(grid%nmm_cmc)
ENDIF
IF ( ASSOCIATED( grid%nmm_grnflx ) ) THEN 
  DEALLOCATE(grid%nmm_grnflx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_grnflx. ')
 endif
  NULLIFY(grid%nmm_grnflx)
ENDIF
IF ( ASSOCIATED( grid%nmm_pctsno ) ) THEN 
  DEALLOCATE(grid%nmm_pctsno,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_pctsno. ')
 endif
  NULLIFY(grid%nmm_pctsno)
ENDIF
IF ( ASSOCIATED( grid%nmm_soiltb ) ) THEN 
  DEALLOCATE(grid%nmm_soiltb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_soiltb. ')
 endif
  NULLIFY(grid%nmm_soiltb)
ENDIF
IF ( ASSOCIATED( grid%nmm_vegfrc ) ) THEN 
  DEALLOCATE(grid%nmm_vegfrc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_vegfrc. ')
 endif
  NULLIFY(grid%nmm_vegfrc)
ENDIF
IF ( ASSOCIATED( grid%nmm_shdmin ) ) THEN 
  DEALLOCATE(grid%nmm_shdmin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_shdmin. ')
 endif
  NULLIFY(grid%nmm_shdmin)
ENDIF
IF ( ASSOCIATED( grid%nmm_shdmax ) ) THEN 
  DEALLOCATE(grid%nmm_shdmax,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_shdmax. ')
 endif
  NULLIFY(grid%nmm_shdmax)
ENDIF
IF ( ASSOCIATED( grid%nmm_sh2o ) ) THEN 
  DEALLOCATE(grid%nmm_sh2o,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_sh2o. ')
 endif
  NULLIFY(grid%nmm_sh2o)
ENDIF
IF ( ASSOCIATED( grid%nmm_smc ) ) THEN 
  DEALLOCATE(grid%nmm_smc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_smc. ')
 endif
  NULLIFY(grid%nmm_smc)
ENDIF
IF ( ASSOCIATED( grid%nmm_stc ) ) THEN 
  DEALLOCATE(grid%nmm_stc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_stc. ')
 endif
  NULLIFY(grid%nmm_stc)
ENDIF
IF ( ASSOCIATED( grid%nmm_dwdtmn ) ) THEN 
  DEALLOCATE(grid%nmm_dwdtmn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_dwdtmn. ')
 endif
  NULLIFY(grid%nmm_dwdtmn)
ENDIF
IF ( ASSOCIATED( grid%nmm_dwdtmx ) ) THEN 
  DEALLOCATE(grid%nmm_dwdtmx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_dwdtmx. ')
 endif
  NULLIFY(grid%nmm_dwdtmx)
ENDIF
IF ( ASSOCIATED( grid%nmm_dwdt ) ) THEN 
  DEALLOCATE(grid%nmm_dwdt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_dwdt. ')
 endif
  NULLIFY(grid%nmm_dwdt)
ENDIF
IF ( ASSOCIATED( grid%nmm_pdwdt ) ) THEN 
  DEALLOCATE(grid%nmm_pdwdt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_pdwdt. ')
 endif
  NULLIFY(grid%nmm_pdwdt)
ENDIF
IF ( ASSOCIATED( grid%nmm_pint ) ) THEN 
  DEALLOCATE(grid%nmm_pint,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_pint. ')
 endif
  NULLIFY(grid%nmm_pint)
ENDIF
IF ( ASSOCIATED( grid%nmm_w ) ) THEN 
  DEALLOCATE(grid%nmm_w,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_w. ')
 endif
  NULLIFY(grid%nmm_w)
ENDIF
IF ( ASSOCIATED( grid%nmm_z ) ) THEN 
  DEALLOCATE(grid%nmm_z,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_z. ')
 endif
  NULLIFY(grid%nmm_z)
ENDIF
IF ( ASSOCIATED( grid%nmm_acfrcv ) ) THEN 
  DEALLOCATE(grid%nmm_acfrcv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_acfrcv. ')
 endif
  NULLIFY(grid%nmm_acfrcv)
ENDIF
IF ( ASSOCIATED( grid%nmm_acfrst ) ) THEN 
  DEALLOCATE(grid%nmm_acfrst,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_acfrst. ')
 endif
  NULLIFY(grid%nmm_acfrst)
ENDIF
IF ( ASSOCIATED( grid%nmm_ssroff ) ) THEN 
  DEALLOCATE(grid%nmm_ssroff,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_ssroff. ')
 endif
  NULLIFY(grid%nmm_ssroff)
ENDIF
IF ( ASSOCIATED( grid%nmm_bgroff ) ) THEN 
  DEALLOCATE(grid%nmm_bgroff,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_bgroff. ')
 endif
  NULLIFY(grid%nmm_bgroff)
ENDIF
IF ( ASSOCIATED( grid%nmm_rlwin ) ) THEN 
  DEALLOCATE(grid%nmm_rlwin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_rlwin. ')
 endif
  NULLIFY(grid%nmm_rlwin)
ENDIF
IF ( ASSOCIATED( grid%nmm_rlwout ) ) THEN 
  DEALLOCATE(grid%nmm_rlwout,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_rlwout. ')
 endif
  NULLIFY(grid%nmm_rlwout)
ENDIF
IF ( ASSOCIATED( grid%nmm_rlwtoa ) ) THEN 
  DEALLOCATE(grid%nmm_rlwtoa,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_rlwtoa. ')
 endif
  NULLIFY(grid%nmm_rlwtoa)
ENDIF
IF ( ASSOCIATED( grid%nmm_alwin ) ) THEN 
  DEALLOCATE(grid%nmm_alwin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_alwin. ')
 endif
  NULLIFY(grid%nmm_alwin)
ENDIF
IF ( ASSOCIATED( grid%nmm_alwout ) ) THEN 
  DEALLOCATE(grid%nmm_alwout,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_alwout. ')
 endif
  NULLIFY(grid%nmm_alwout)
ENDIF
IF ( ASSOCIATED( grid%nmm_alwtoa ) ) THEN 
  DEALLOCATE(grid%nmm_alwtoa,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_alwtoa. ')
 endif
  NULLIFY(grid%nmm_alwtoa)
ENDIF
IF ( ASSOCIATED( grid%nmm_rswin ) ) THEN 
  DEALLOCATE(grid%nmm_rswin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_rswin. ')
 endif
  NULLIFY(grid%nmm_rswin)
ENDIF
IF ( ASSOCIATED( grid%nmm_rswinc ) ) THEN 
  DEALLOCATE(grid%nmm_rswinc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_rswinc. ')
 endif
  NULLIFY(grid%nmm_rswinc)
ENDIF
IF ( ASSOCIATED( grid%nmm_rswout ) ) THEN 
  DEALLOCATE(grid%nmm_rswout,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_rswout. ')
 endif
  NULLIFY(grid%nmm_rswout)
ENDIF
IF ( ASSOCIATED( grid%nmm_rswtoa ) ) THEN 
  DEALLOCATE(grid%nmm_rswtoa,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_rswtoa. ')
 endif
  NULLIFY(grid%nmm_rswtoa)
ENDIF
IF ( ASSOCIATED( grid%nmm_aswin ) ) THEN 
  DEALLOCATE(grid%nmm_aswin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_aswin. ')
 endif
  NULLIFY(grid%nmm_aswin)
ENDIF
IF ( ASSOCIATED( grid%nmm_aswout ) ) THEN 
  DEALLOCATE(grid%nmm_aswout,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_aswout. ')
 endif
  NULLIFY(grid%nmm_aswout)
ENDIF
IF ( ASSOCIATED( grid%nmm_aswtoa ) ) THEN 
  DEALLOCATE(grid%nmm_aswtoa,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_aswtoa. ')
 endif
  NULLIFY(grid%nmm_aswtoa)
ENDIF
IF ( ASSOCIATED( grid%nmm_sfcshx ) ) THEN 
  DEALLOCATE(grid%nmm_sfcshx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_sfcshx. ')
 endif
  NULLIFY(grid%nmm_sfcshx)
ENDIF
IF ( ASSOCIATED( grid%nmm_sfclhx ) ) THEN 
  DEALLOCATE(grid%nmm_sfclhx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_sfclhx. ')
 endif
  NULLIFY(grid%nmm_sfclhx)
ENDIF
IF ( ASSOCIATED( grid%nmm_subshx ) ) THEN 
  DEALLOCATE(grid%nmm_subshx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_subshx. ')
 endif
  NULLIFY(grid%nmm_subshx)
ENDIF
IF ( ASSOCIATED( grid%nmm_snopcx ) ) THEN 
  DEALLOCATE(grid%nmm_snopcx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_snopcx. ')
 endif
  NULLIFY(grid%nmm_snopcx)
ENDIF
IF ( ASSOCIATED( grid%nmm_sfcuvx ) ) THEN 
  DEALLOCATE(grid%nmm_sfcuvx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_sfcuvx. ')
 endif
  NULLIFY(grid%nmm_sfcuvx)
ENDIF
IF ( ASSOCIATED( grid%nmm_potevp ) ) THEN 
  DEALLOCATE(grid%nmm_potevp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_potevp. ')
 endif
  NULLIFY(grid%nmm_potevp)
ENDIF
IF ( ASSOCIATED( grid%nmm_potflx ) ) THEN 
  DEALLOCATE(grid%nmm_potflx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_potflx. ')
 endif
  NULLIFY(grid%nmm_potflx)
ENDIF
IF ( ASSOCIATED( grid%nmm_tlmin ) ) THEN 
  DEALLOCATE(grid%nmm_tlmin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_tlmin. ')
 endif
  NULLIFY(grid%nmm_tlmin)
ENDIF
IF ( ASSOCIATED( grid%nmm_tlmax ) ) THEN 
  DEALLOCATE(grid%nmm_tlmax,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_tlmax. ')
 endif
  NULLIFY(grid%nmm_tlmax)
ENDIF
IF ( ASSOCIATED( grid%nmm_rlwtt ) ) THEN 
  DEALLOCATE(grid%nmm_rlwtt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_rlwtt. ')
 endif
  NULLIFY(grid%nmm_rlwtt)
ENDIF
IF ( ASSOCIATED( grid%nmm_rswtt ) ) THEN 
  DEALLOCATE(grid%nmm_rswtt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_rswtt. ')
 endif
  NULLIFY(grid%nmm_rswtt)
ENDIF
IF ( ASSOCIATED( grid%nmm_tcucn ) ) THEN 
  DEALLOCATE(grid%nmm_tcucn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_tcucn. ')
 endif
  NULLIFY(grid%nmm_tcucn)
ENDIF
IF ( ASSOCIATED( grid%nmm_train ) ) THEN 
  DEALLOCATE(grid%nmm_train,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_train. ')
 endif
  NULLIFY(grid%nmm_train)
ENDIF
IF ( ASSOCIATED( grid%nmm_ncfrcv ) ) THEN 
  DEALLOCATE(grid%nmm_ncfrcv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_ncfrcv. ')
 endif
  NULLIFY(grid%nmm_ncfrcv)
ENDIF
IF ( ASSOCIATED( grid%nmm_ncfrst ) ) THEN 
  DEALLOCATE(grid%nmm_ncfrst,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_ncfrst. ')
 endif
  NULLIFY(grid%nmm_ncfrst)
ENDIF
IF ( ASSOCIATED( grid%nmm_ihe ) ) THEN 
  DEALLOCATE(grid%nmm_ihe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_ihe. ')
 endif
  NULLIFY(grid%nmm_ihe)
ENDIF
IF ( ASSOCIATED( grid%nmm_ihw ) ) THEN 
  DEALLOCATE(grid%nmm_ihw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_ihw. ')
 endif
  NULLIFY(grid%nmm_ihw)
ENDIF
IF ( ASSOCIATED( grid%nmm_ive ) ) THEN 
  DEALLOCATE(grid%nmm_ive,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_ive. ')
 endif
  NULLIFY(grid%nmm_ive)
ENDIF
IF ( ASSOCIATED( grid%nmm_ivw ) ) THEN 
  DEALLOCATE(grid%nmm_ivw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_ivw. ')
 endif
  NULLIFY(grid%nmm_ivw)
ENDIF
IF ( ASSOCIATED( grid%nmm_irad ) ) THEN 
  DEALLOCATE(grid%nmm_irad,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_irad. ')
 endif
  NULLIFY(grid%nmm_irad)
ENDIF
IF ( ASSOCIATED( grid%nmm_iheg ) ) THEN 
  DEALLOCATE(grid%nmm_iheg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_iheg. ')
 endif
  NULLIFY(grid%nmm_iheg)
ENDIF
IF ( ASSOCIATED( grid%nmm_ihwg ) ) THEN 
  DEALLOCATE(grid%nmm_ihwg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_ihwg. ')
 endif
  NULLIFY(grid%nmm_ihwg)
ENDIF
IF ( ASSOCIATED( grid%nmm_iveg ) ) THEN 
  DEALLOCATE(grid%nmm_iveg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_iveg. ')
 endif
  NULLIFY(grid%nmm_iveg)
ENDIF
IF ( ASSOCIATED( grid%nmm_ivwg ) ) THEN 
  DEALLOCATE(grid%nmm_ivwg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_ivwg. ')
 endif
  NULLIFY(grid%nmm_ivwg)
ENDIF
IF ( ASSOCIATED( grid%nmm_iradg ) ) THEN 
  DEALLOCATE(grid%nmm_iradg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_iradg. ')
 endif
  NULLIFY(grid%nmm_iradg)
ENDIF
IF ( ASSOCIATED( grid%nmm_n_iup_h ) ) THEN 
  DEALLOCATE(grid%nmm_n_iup_h,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_n_iup_h. ')
 endif
  NULLIFY(grid%nmm_n_iup_h)
ENDIF
IF ( ASSOCIATED( grid%nmm_n_iup_v ) ) THEN 
  DEALLOCATE(grid%nmm_n_iup_v,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_n_iup_v. ')
 endif
  NULLIFY(grid%nmm_n_iup_v)
ENDIF
IF ( ASSOCIATED( grid%nmm_n_iup_adh ) ) THEN 
  DEALLOCATE(grid%nmm_n_iup_adh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_n_iup_adh. ')
 endif
  NULLIFY(grid%nmm_n_iup_adh)
ENDIF
IF ( ASSOCIATED( grid%nmm_n_iup_adv ) ) THEN 
  DEALLOCATE(grid%nmm_n_iup_adv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_n_iup_adv. ')
 endif
  NULLIFY(grid%nmm_n_iup_adv)
ENDIF
IF ( ASSOCIATED( grid%nmm_iup_h ) ) THEN 
  DEALLOCATE(grid%nmm_iup_h,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_iup_h. ')
 endif
  NULLIFY(grid%nmm_iup_h)
ENDIF
IF ( ASSOCIATED( grid%nmm_iup_v ) ) THEN 
  DEALLOCATE(grid%nmm_iup_v,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_iup_v. ')
 endif
  NULLIFY(grid%nmm_iup_v)
ENDIF
IF ( ASSOCIATED( grid%nmm_iup_adh ) ) THEN 
  DEALLOCATE(grid%nmm_iup_adh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_iup_adh. ')
 endif
  NULLIFY(grid%nmm_iup_adh)
ENDIF
IF ( ASSOCIATED( grid%nmm_iup_adv ) ) THEN 
  DEALLOCATE(grid%nmm_iup_adv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_iup_adv. ')
 endif
  NULLIFY(grid%nmm_iup_adv)
ENDIF
IF ( ASSOCIATED( grid%imask_nostag ) ) THEN 
  DEALLOCATE(grid%imask_nostag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%imask_nostag. ')
 endif
  NULLIFY(grid%imask_nostag)
ENDIF
IF ( ASSOCIATED( grid%imask_xstag ) ) THEN 
  DEALLOCATE(grid%imask_xstag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%imask_xstag. ')
 endif
  NULLIFY(grid%imask_xstag)
ENDIF
IF ( ASSOCIATED( grid%imask_ystag ) ) THEN 
  DEALLOCATE(grid%imask_ystag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%imask_ystag. ')
 endif
  NULLIFY(grid%imask_ystag)
ENDIF
IF ( ASSOCIATED( grid%imask_xystag ) ) THEN 
  DEALLOCATE(grid%imask_xystag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%imask_xystag. ')
 endif
  NULLIFY(grid%imask_xystag)
ENDIF
IF ( ASSOCIATED( grid%sm000007 ) ) THEN 
  DEALLOCATE(grid%sm000007,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sm000007. ')
 endif
  NULLIFY(grid%sm000007)
ENDIF
IF ( ASSOCIATED( grid%sm007028 ) ) THEN 
  DEALLOCATE(grid%sm007028,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sm007028. ')
 endif
  NULLIFY(grid%sm007028)
ENDIF
IF ( ASSOCIATED( grid%sm028100 ) ) THEN 
  DEALLOCATE(grid%sm028100,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sm028100. ')
 endif
  NULLIFY(grid%sm028100)
ENDIF
IF ( ASSOCIATED( grid%sm100255 ) ) THEN 
  DEALLOCATE(grid%sm100255,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sm100255. ')
 endif
  NULLIFY(grid%sm100255)
ENDIF
IF ( ASSOCIATED( grid%st000007 ) ) THEN 
  DEALLOCATE(grid%st000007,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%st000007. ')
 endif
  NULLIFY(grid%st000007)
ENDIF
IF ( ASSOCIATED( grid%st007028 ) ) THEN 
  DEALLOCATE(grid%st007028,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%st007028. ')
 endif
  NULLIFY(grid%st007028)
ENDIF
IF ( ASSOCIATED( grid%st028100 ) ) THEN 
  DEALLOCATE(grid%st028100,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%st028100. ')
 endif
  NULLIFY(grid%st028100)
ENDIF
IF ( ASSOCIATED( grid%st100255 ) ) THEN 
  DEALLOCATE(grid%st100255,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%st100255. ')
 endif
  NULLIFY(grid%st100255)
ENDIF
IF ( ASSOCIATED( grid%sm000010 ) ) THEN 
  DEALLOCATE(grid%sm000010,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sm000010. ')
 endif
  NULLIFY(grid%sm000010)
ENDIF
IF ( ASSOCIATED( grid%sm010040 ) ) THEN 
  DEALLOCATE(grid%sm010040,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sm010040. ')
 endif
  NULLIFY(grid%sm010040)
ENDIF
IF ( ASSOCIATED( grid%sm040100 ) ) THEN 
  DEALLOCATE(grid%sm040100,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sm040100. ')
 endif
  NULLIFY(grid%sm040100)
ENDIF
IF ( ASSOCIATED( grid%sm100200 ) ) THEN 
  DEALLOCATE(grid%sm100200,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sm100200. ')
 endif
  NULLIFY(grid%sm100200)
ENDIF
IF ( ASSOCIATED( grid%sm010200 ) ) THEN 
  DEALLOCATE(grid%sm010200,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sm010200. ')
 endif
  NULLIFY(grid%sm010200)
ENDIF
IF ( ASSOCIATED( grid%soilm000 ) ) THEN 
  DEALLOCATE(grid%soilm000,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilm000. ')
 endif
  NULLIFY(grid%soilm000)
ENDIF
IF ( ASSOCIATED( grid%soilm005 ) ) THEN 
  DEALLOCATE(grid%soilm005,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilm005. ')
 endif
  NULLIFY(grid%soilm005)
ENDIF
IF ( ASSOCIATED( grid%soilm020 ) ) THEN 
  DEALLOCATE(grid%soilm020,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilm020. ')
 endif
  NULLIFY(grid%soilm020)
ENDIF
IF ( ASSOCIATED( grid%soilm040 ) ) THEN 
  DEALLOCATE(grid%soilm040,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilm040. ')
 endif
  NULLIFY(grid%soilm040)
ENDIF
IF ( ASSOCIATED( grid%soilm160 ) ) THEN 
  DEALLOCATE(grid%soilm160,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilm160. ')
 endif
  NULLIFY(grid%soilm160)
ENDIF
IF ( ASSOCIATED( grid%soilm300 ) ) THEN 
  DEALLOCATE(grid%soilm300,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilm300. ')
 endif
  NULLIFY(grid%soilm300)
ENDIF
IF ( ASSOCIATED( grid%sw000010 ) ) THEN 
  DEALLOCATE(grid%sw000010,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sw000010. ')
 endif
  NULLIFY(grid%sw000010)
ENDIF
IF ( ASSOCIATED( grid%sw010040 ) ) THEN 
  DEALLOCATE(grid%sw010040,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sw010040. ')
 endif
  NULLIFY(grid%sw010040)
ENDIF
IF ( ASSOCIATED( grid%sw040100 ) ) THEN 
  DEALLOCATE(grid%sw040100,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sw040100. ')
 endif
  NULLIFY(grid%sw040100)
ENDIF
IF ( ASSOCIATED( grid%sw100200 ) ) THEN 
  DEALLOCATE(grid%sw100200,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sw100200. ')
 endif
  NULLIFY(grid%sw100200)
ENDIF
IF ( ASSOCIATED( grid%sw010200 ) ) THEN 
  DEALLOCATE(grid%sw010200,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sw010200. ')
 endif
  NULLIFY(grid%sw010200)
ENDIF
IF ( ASSOCIATED( grid%soilw000 ) ) THEN 
  DEALLOCATE(grid%soilw000,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilw000. ')
 endif
  NULLIFY(grid%soilw000)
ENDIF
IF ( ASSOCIATED( grid%soilw005 ) ) THEN 
  DEALLOCATE(grid%soilw005,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilw005. ')
 endif
  NULLIFY(grid%soilw005)
ENDIF
IF ( ASSOCIATED( grid%soilw020 ) ) THEN 
  DEALLOCATE(grid%soilw020,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilw020. ')
 endif
  NULLIFY(grid%soilw020)
ENDIF
IF ( ASSOCIATED( grid%soilw040 ) ) THEN 
  DEALLOCATE(grid%soilw040,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilw040. ')
 endif
  NULLIFY(grid%soilw040)
ENDIF
IF ( ASSOCIATED( grid%soilw160 ) ) THEN 
  DEALLOCATE(grid%soilw160,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilw160. ')
 endif
  NULLIFY(grid%soilw160)
ENDIF
IF ( ASSOCIATED( grid%soilw300 ) ) THEN 
  DEALLOCATE(grid%soilw300,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilw300. ')
 endif
  NULLIFY(grid%soilw300)
ENDIF
IF ( ASSOCIATED( grid%st000010 ) ) THEN 
  DEALLOCATE(grid%st000010,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%st000010. ')
 endif
  NULLIFY(grid%st000010)
ENDIF
IF ( ASSOCIATED( grid%st010040 ) ) THEN 
  DEALLOCATE(grid%st010040,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%st010040. ')
 endif
  NULLIFY(grid%st010040)
ENDIF
IF ( ASSOCIATED( grid%st040100 ) ) THEN 
  DEALLOCATE(grid%st040100,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%st040100. ')
 endif
  NULLIFY(grid%st040100)
ENDIF
IF ( ASSOCIATED( grid%st100200 ) ) THEN 
  DEALLOCATE(grid%st100200,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%st100200. ')
 endif
  NULLIFY(grid%st100200)
ENDIF
IF ( ASSOCIATED( grid%st010200 ) ) THEN 
  DEALLOCATE(grid%st010200,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%st010200. ')
 endif
  NULLIFY(grid%st010200)
ENDIF
IF ( ASSOCIATED( grid%soilt000 ) ) THEN 
  DEALLOCATE(grid%soilt000,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilt000. ')
 endif
  NULLIFY(grid%soilt000)
ENDIF
IF ( ASSOCIATED( grid%soilt005 ) ) THEN 
  DEALLOCATE(grid%soilt005,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilt005. ')
 endif
  NULLIFY(grid%soilt005)
ENDIF
IF ( ASSOCIATED( grid%soilt020 ) ) THEN 
  DEALLOCATE(grid%soilt020,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilt020. ')
 endif
  NULLIFY(grid%soilt020)
ENDIF
IF ( ASSOCIATED( grid%soilt040 ) ) THEN 
  DEALLOCATE(grid%soilt040,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilt040. ')
 endif
  NULLIFY(grid%soilt040)
ENDIF
IF ( ASSOCIATED( grid%soilt160 ) ) THEN 
  DEALLOCATE(grid%soilt160,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilt160. ')
 endif
  NULLIFY(grid%soilt160)
ENDIF
IF ( ASSOCIATED( grid%soilt300 ) ) THEN 
  DEALLOCATE(grid%soilt300,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilt300. ')
 endif
  NULLIFY(grid%soilt300)
ENDIF
IF ( ASSOCIATED( grid%landmask ) ) THEN 
  DEALLOCATE(grid%landmask,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%landmask. ')
 endif
  NULLIFY(grid%landmask)
ENDIF
IF ( ASSOCIATED( grid%topostdv ) ) THEN 
  DEALLOCATE(grid%topostdv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%topostdv. ')
 endif
  NULLIFY(grid%topostdv)
ENDIF
IF ( ASSOCIATED( grid%toposlpx ) ) THEN 
  DEALLOCATE(grid%toposlpx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%toposlpx. ')
 endif
  NULLIFY(grid%toposlpx)
ENDIF
IF ( ASSOCIATED( grid%toposlpy ) ) THEN 
  DEALLOCATE(grid%toposlpy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%toposlpy. ')
 endif
  NULLIFY(grid%toposlpy)
ENDIF
IF ( ASSOCIATED( grid%greenmax ) ) THEN 
  DEALLOCATE(grid%greenmax,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%greenmax. ')
 endif
  NULLIFY(grid%greenmax)
ENDIF
IF ( ASSOCIATED( grid%greenmin ) ) THEN 
  DEALLOCATE(grid%greenmin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%greenmin. ')
 endif
  NULLIFY(grid%greenmin)
ENDIF
IF ( ASSOCIATED( grid%albedomx ) ) THEN 
  DEALLOCATE(grid%albedomx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%albedomx. ')
 endif
  NULLIFY(grid%albedomx)
ENDIF
IF ( ASSOCIATED( grid%slopecat ) ) THEN 
  DEALLOCATE(grid%slopecat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%slopecat. ')
 endif
  NULLIFY(grid%slopecat)
ENDIF
IF ( ASSOCIATED( grid%toposoil ) ) THEN 
  DEALLOCATE(grid%toposoil,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%toposoil. ')
 endif
  NULLIFY(grid%toposoil)
ENDIF
IF ( ASSOCIATED( grid%landusef ) ) THEN 
  DEALLOCATE(grid%landusef,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%landusef. ')
 endif
  NULLIFY(grid%landusef)
ENDIF
IF ( ASSOCIATED( grid%soilctop ) ) THEN 
  DEALLOCATE(grid%soilctop,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilctop. ')
 endif
  NULLIFY(grid%soilctop)
ENDIF
IF ( ASSOCIATED( grid%soilcbot ) ) THEN 
  DEALLOCATE(grid%soilcbot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilcbot. ')
 endif
  NULLIFY(grid%soilcbot)
ENDIF
IF ( ASSOCIATED( grid%moist ) ) THEN 
  DEALLOCATE(grid%moist,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%moist. ')
 endif
  NULLIFY(grid%moist)
ENDIF
IF ( ASSOCIATED( grid%scalar ) ) THEN 
  DEALLOCATE(grid%scalar,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%scalar. ')
 endif
  NULLIFY(grid%scalar)
ENDIF
IF ( ASSOCIATED( grid%scalar_b ) ) THEN 
  DEALLOCATE(grid%scalar_b,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%scalar_b. ')
 endif
  NULLIFY(grid%scalar_b)
ENDIF
IF ( ASSOCIATED( grid%scalar_bt ) ) THEN 
  DEALLOCATE(grid%scalar_bt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%scalar_bt. ')
 endif
  NULLIFY(grid%scalar_bt)
ENDIF
IF ( ASSOCIATED( grid%chem ) ) THEN 
  DEALLOCATE(grid%chem,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%chem. ')
 endif
  NULLIFY(grid%chem)
ENDIF
IF ( ASSOCIATED( grid%smois ) ) THEN 
  DEALLOCATE(grid%smois,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%smois. ')
 endif
  NULLIFY(grid%smois)
ENDIF
IF ( ASSOCIATED( grid%tslb ) ) THEN 
  DEALLOCATE(grid%tslb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%tslb. ')
 endif
  NULLIFY(grid%tslb)
ENDIF
IF ( ASSOCIATED( grid%gsw ) ) THEN 
  DEALLOCATE(grid%gsw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%gsw. ')
 endif
  NULLIFY(grid%gsw)
ENDIF
IF ( ASSOCIATED( grid%xlat ) ) THEN 
  DEALLOCATE(grid%xlat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%xlat. ')
 endif
  NULLIFY(grid%xlat)
ENDIF
IF ( ASSOCIATED( grid%xlong ) ) THEN 
  DEALLOCATE(grid%xlong,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%xlong. ')
 endif
  NULLIFY(grid%xlong)
ENDIF
IF ( ASSOCIATED( grid%xland ) ) THEN 
  DEALLOCATE(grid%xland,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%xland. ')
 endif
  NULLIFY(grid%xland)
ENDIF
IF ( ASSOCIATED( grid%raincv ) ) THEN 
  DEALLOCATE(grid%raincv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%raincv. ')
 endif
  NULLIFY(grid%raincv)
ENDIF
IF ( ASSOCIATED( grid%psfc ) ) THEN 
  DEALLOCATE(grid%psfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%psfc. ')
 endif
  NULLIFY(grid%psfc)
ENDIF
IF ( ASSOCIATED( grid%th2 ) ) THEN 
  DEALLOCATE(grid%th2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%th2. ')
 endif
  NULLIFY(grid%th2)
ENDIF
IF ( ASSOCIATED( grid%t2 ) ) THEN 
  DEALLOCATE(grid%t2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%t2. ')
 endif
  NULLIFY(grid%t2)
ENDIF
IF ( ASSOCIATED( grid%u10 ) ) THEN 
  DEALLOCATE(grid%u10,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%u10. ')
 endif
  NULLIFY(grid%u10)
ENDIF
IF ( ASSOCIATED( grid%v10 ) ) THEN 
  DEALLOCATE(grid%v10,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%v10. ')
 endif
  NULLIFY(grid%v10)
ENDIF
IF ( ASSOCIATED( grid%xice ) ) THEN 
  DEALLOCATE(grid%xice,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%xice. ')
 endif
  NULLIFY(grid%xice)
ENDIF
IF ( ASSOCIATED( grid%smstav ) ) THEN 
  DEALLOCATE(grid%smstav,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%smstav. ')
 endif
  NULLIFY(grid%smstav)
ENDIF
IF ( ASSOCIATED( grid%smstot ) ) THEN 
  DEALLOCATE(grid%smstot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%smstot. ')
 endif
  NULLIFY(grid%smstot)
ENDIF
IF ( ASSOCIATED( grid%sfcrunoff ) ) THEN 
  DEALLOCATE(grid%sfcrunoff,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sfcrunoff. ')
 endif
  NULLIFY(grid%sfcrunoff)
ENDIF
IF ( ASSOCIATED( grid%udrunoff ) ) THEN 
  DEALLOCATE(grid%udrunoff,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%udrunoff. ')
 endif
  NULLIFY(grid%udrunoff)
ENDIF
IF ( ASSOCIATED( grid%ivgtyp ) ) THEN 
  DEALLOCATE(grid%ivgtyp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%ivgtyp. ')
 endif
  NULLIFY(grid%ivgtyp)
ENDIF
IF ( ASSOCIATED( grid%isltyp ) ) THEN 
  DEALLOCATE(grid%isltyp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%isltyp. ')
 endif
  NULLIFY(grid%isltyp)
ENDIF
IF ( ASSOCIATED( grid%vegfra ) ) THEN 
  DEALLOCATE(grid%vegfra,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%vegfra. ')
 endif
  NULLIFY(grid%vegfra)
ENDIF
IF ( ASSOCIATED( grid%sfcevp ) ) THEN 
  DEALLOCATE(grid%sfcevp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sfcevp. ')
 endif
  NULLIFY(grid%sfcevp)
ENDIF
IF ( ASSOCIATED( grid%grdflx ) ) THEN 
  DEALLOCATE(grid%grdflx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%grdflx. ')
 endif
  NULLIFY(grid%grdflx)
ENDIF
IF ( ASSOCIATED( grid%albbck ) ) THEN 
  DEALLOCATE(grid%albbck,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%albbck. ')
 endif
  NULLIFY(grid%albbck)
ENDIF
IF ( ASSOCIATED( grid%sfcexc ) ) THEN 
  DEALLOCATE(grid%sfcexc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sfcexc. ')
 endif
  NULLIFY(grid%sfcexc)
ENDIF
IF ( ASSOCIATED( grid%acsnow ) ) THEN 
  DEALLOCATE(grid%acsnow,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%acsnow. ')
 endif
  NULLIFY(grid%acsnow)
ENDIF
IF ( ASSOCIATED( grid%acsnom ) ) THEN 
  DEALLOCATE(grid%acsnom,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%acsnom. ')
 endif
  NULLIFY(grid%acsnom)
ENDIF
IF ( ASSOCIATED( grid%rmol ) ) THEN 
  DEALLOCATE(grid%rmol,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rmol. ')
 endif
  NULLIFY(grid%rmol)
ENDIF
IF ( ASSOCIATED( grid%snow ) ) THEN 
  DEALLOCATE(grid%snow,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%snow. ')
 endif
  NULLIFY(grid%snow)
ENDIF
IF ( ASSOCIATED( grid%canwat ) ) THEN 
  DEALLOCATE(grid%canwat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%canwat. ')
 endif
  NULLIFY(grid%canwat)
ENDIF
IF ( ASSOCIATED( grid%sst ) ) THEN 
  DEALLOCATE(grid%sst,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sst. ')
 endif
  NULLIFY(grid%sst)
ENDIF
IF ( ASSOCIATED( grid%weasd ) ) THEN 
  DEALLOCATE(grid%weasd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%weasd. ')
 endif
  NULLIFY(grid%weasd)
ENDIF
IF ( ASSOCIATED( grid%znt ) ) THEN 
  DEALLOCATE(grid%znt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%znt. ')
 endif
  NULLIFY(grid%znt)
ENDIF
IF ( ASSOCIATED( grid%mol ) ) THEN 
  DEALLOCATE(grid%mol,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%mol. ')
 endif
  NULLIFY(grid%mol)
ENDIF
IF ( ASSOCIATED( grid%tke_myj ) ) THEN 
  DEALLOCATE(grid%tke_myj,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%tke_myj. ')
 endif
  NULLIFY(grid%tke_myj)
ENDIF
IF ( ASSOCIATED( grid%el_myj ) ) THEN 
  DEALLOCATE(grid%el_myj,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%el_myj. ')
 endif
  NULLIFY(grid%el_myj)
ENDIF
IF ( ASSOCIATED( grid%exch_h ) ) THEN 
  DEALLOCATE(grid%exch_h,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%exch_h. ')
 endif
  NULLIFY(grid%exch_h)
ENDIF
IF ( ASSOCIATED( grid%thz0 ) ) THEN 
  DEALLOCATE(grid%thz0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%thz0. ')
 endif
  NULLIFY(grid%thz0)
ENDIF
IF ( ASSOCIATED( grid%qz0 ) ) THEN 
  DEALLOCATE(grid%qz0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%qz0. ')
 endif
  NULLIFY(grid%qz0)
ENDIF
IF ( ASSOCIATED( grid%uz0 ) ) THEN 
  DEALLOCATE(grid%uz0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%uz0. ')
 endif
  NULLIFY(grid%uz0)
ENDIF
IF ( ASSOCIATED( grid%vz0 ) ) THEN 
  DEALLOCATE(grid%vz0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%vz0. ')
 endif
  NULLIFY(grid%vz0)
ENDIF
IF ( ASSOCIATED( grid%flhc ) ) THEN 
  DEALLOCATE(grid%flhc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%flhc. ')
 endif
  NULLIFY(grid%flhc)
ENDIF
IF ( ASSOCIATED( grid%flqc ) ) THEN 
  DEALLOCATE(grid%flqc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%flqc. ')
 endif
  NULLIFY(grid%flqc)
ENDIF
IF ( ASSOCIATED( grid%qsg ) ) THEN 
  DEALLOCATE(grid%qsg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%qsg. ')
 endif
  NULLIFY(grid%qsg)
ENDIF
IF ( ASSOCIATED( grid%qvg ) ) THEN 
  DEALLOCATE(grid%qvg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%qvg. ')
 endif
  NULLIFY(grid%qvg)
ENDIF
IF ( ASSOCIATED( grid%qcg ) ) THEN 
  DEALLOCATE(grid%qcg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%qcg. ')
 endif
  NULLIFY(grid%qcg)
ENDIF
IF ( ASSOCIATED( grid%soilt1 ) ) THEN 
  DEALLOCATE(grid%soilt1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilt1. ')
 endif
  NULLIFY(grid%soilt1)
ENDIF
IF ( ASSOCIATED( grid%tsnav ) ) THEN 
  DEALLOCATE(grid%tsnav,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%tsnav. ')
 endif
  NULLIFY(grid%tsnav)
ENDIF
IF ( ASSOCIATED( grid%nmm_psfc_out ) ) THEN 
  DEALLOCATE(grid%nmm_psfc_out,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_psfc_out. ')
 endif
  NULLIFY(grid%nmm_psfc_out)
ENDIF
IF ( ASSOCIATED( grid%uz0h ) ) THEN 
  DEALLOCATE(grid%uz0h,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%uz0h. ')
 endif
  NULLIFY(grid%uz0h)
ENDIF
IF ( ASSOCIATED( grid%vz0h ) ) THEN 
  DEALLOCATE(grid%vz0h,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%vz0h. ')
 endif
  NULLIFY(grid%vz0h)
ENDIF
IF ( ASSOCIATED( grid%dudt ) ) THEN 
  DEALLOCATE(grid%dudt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dudt. ')
 endif
  NULLIFY(grid%dudt)
ENDIF
IF ( ASSOCIATED( grid%dvdt ) ) THEN 
  DEALLOCATE(grid%dvdt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dvdt. ')
 endif
  NULLIFY(grid%dvdt)
ENDIF
IF ( ASSOCIATED( grid%qsfc ) ) THEN 
  DEALLOCATE(grid%qsfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%qsfc. ')
 endif
  NULLIFY(grid%qsfc)
ENDIF
IF ( ASSOCIATED( grid%akhs ) ) THEN 
  DEALLOCATE(grid%akhs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%akhs. ')
 endif
  NULLIFY(grid%akhs)
ENDIF
IF ( ASSOCIATED( grid%akms ) ) THEN 
  DEALLOCATE(grid%akms,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%akms. ')
 endif
  NULLIFY(grid%akms)
ENDIF
IF ( ASSOCIATED( grid%htop ) ) THEN 
  DEALLOCATE(grid%htop,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%htop. ')
 endif
  NULLIFY(grid%htop)
ENDIF
IF ( ASSOCIATED( grid%hbot ) ) THEN 
  DEALLOCATE(grid%hbot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%hbot. ')
 endif
  NULLIFY(grid%hbot)
ENDIF
IF ( ASSOCIATED( grid%htopr ) ) THEN 
  DEALLOCATE(grid%htopr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%htopr. ')
 endif
  NULLIFY(grid%htopr)
ENDIF
IF ( ASSOCIATED( grid%hbotr ) ) THEN 
  DEALLOCATE(grid%hbotr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%hbotr. ')
 endif
  NULLIFY(grid%hbotr)
ENDIF
IF ( ASSOCIATED( grid%htopd ) ) THEN 
  DEALLOCATE(grid%htopd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%htopd. ')
 endif
  NULLIFY(grid%htopd)
ENDIF
IF ( ASSOCIATED( grid%hbotd ) ) THEN 
  DEALLOCATE(grid%hbotd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%hbotd. ')
 endif
  NULLIFY(grid%hbotd)
ENDIF
IF ( ASSOCIATED( grid%htops ) ) THEN 
  DEALLOCATE(grid%htops,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%htops. ')
 endif
  NULLIFY(grid%htops)
ENDIF
IF ( ASSOCIATED( grid%hbots ) ) THEN 
  DEALLOCATE(grid%hbots,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%hbots. ')
 endif
  NULLIFY(grid%hbots)
ENDIF
IF ( ASSOCIATED( grid%cuppt ) ) THEN 
  DEALLOCATE(grid%cuppt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%cuppt. ')
 endif
  NULLIFY(grid%cuppt)
ENDIF
IF ( ASSOCIATED( grid%cprate ) ) THEN 
  DEALLOCATE(grid%cprate,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%cprate. ')
 endif
  NULLIFY(grid%cprate)
ENDIF
IF ( ASSOCIATED( grid%f_ice_phy ) ) THEN 
  DEALLOCATE(grid%f_ice_phy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%f_ice_phy. ')
 endif
  NULLIFY(grid%f_ice_phy)
ENDIF
IF ( ASSOCIATED( grid%f_rain_phy ) ) THEN 
  DEALLOCATE(grid%f_rain_phy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%f_rain_phy. ')
 endif
  NULLIFY(grid%f_rain_phy)
ENDIF
IF ( ASSOCIATED( grid%f_rimef_phy ) ) THEN 
  DEALLOCATE(grid%f_rimef_phy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%f_rimef_phy. ')
 endif
  NULLIFY(grid%f_rimef_phy)
ENDIF
IF ( ASSOCIATED( grid%mass_flux ) ) THEN 
  DEALLOCATE(grid%mass_flux,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%mass_flux. ')
 endif
  NULLIFY(grid%mass_flux)
ENDIF
IF ( ASSOCIATED( grid%apr_gr ) ) THEN 
  DEALLOCATE(grid%apr_gr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%apr_gr. ')
 endif
  NULLIFY(grid%apr_gr)
ENDIF
IF ( ASSOCIATED( grid%apr_w ) ) THEN 
  DEALLOCATE(grid%apr_w,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%apr_w. ')
 endif
  NULLIFY(grid%apr_w)
ENDIF
IF ( ASSOCIATED( grid%apr_mc ) ) THEN 
  DEALLOCATE(grid%apr_mc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%apr_mc. ')
 endif
  NULLIFY(grid%apr_mc)
ENDIF
IF ( ASSOCIATED( grid%apr_st ) ) THEN 
  DEALLOCATE(grid%apr_st,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%apr_st. ')
 endif
  NULLIFY(grid%apr_st)
ENDIF
IF ( ASSOCIATED( grid%apr_as ) ) THEN 
  DEALLOCATE(grid%apr_as,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%apr_as. ')
 endif
  NULLIFY(grid%apr_as)
ENDIF
IF ( ASSOCIATED( grid%apr_capma ) ) THEN 
  DEALLOCATE(grid%apr_capma,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%apr_capma. ')
 endif
  NULLIFY(grid%apr_capma)
ENDIF
IF ( ASSOCIATED( grid%apr_capme ) ) THEN 
  DEALLOCATE(grid%apr_capme,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%apr_capme. ')
 endif
  NULLIFY(grid%apr_capme)
ENDIF
IF ( ASSOCIATED( grid%apr_capmi ) ) THEN 
  DEALLOCATE(grid%apr_capmi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%apr_capmi. ')
 endif
  NULLIFY(grid%apr_capmi)
ENDIF
IF ( ASSOCIATED( grid%xf_ens ) ) THEN 
  DEALLOCATE(grid%xf_ens,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%xf_ens. ')
 endif
  NULLIFY(grid%xf_ens)
ENDIF
IF ( ASSOCIATED( grid%pr_ens ) ) THEN 
  DEALLOCATE(grid%pr_ens,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%pr_ens. ')
 endif
  NULLIFY(grid%pr_ens)
ENDIF
IF ( ASSOCIATED( grid%rthften ) ) THEN 
  DEALLOCATE(grid%rthften,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rthften. ')
 endif
  NULLIFY(grid%rthften)
ENDIF
IF ( ASSOCIATED( grid%rqvften ) ) THEN 
  DEALLOCATE(grid%rqvften,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rqvften. ')
 endif
  NULLIFY(grid%rqvften)
ENDIF
IF ( ASSOCIATED( grid%snowh ) ) THEN 
  DEALLOCATE(grid%snowh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%snowh. ')
 endif
  NULLIFY(grid%snowh)
ENDIF
IF ( ASSOCIATED( grid%rhosn ) ) THEN 
  DEALLOCATE(grid%rhosn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rhosn. ')
 endif
  NULLIFY(grid%rhosn)
ENDIF
IF ( ASSOCIATED( grid%smfr3d ) ) THEN 
  DEALLOCATE(grid%smfr3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%smfr3d. ')
 endif
  NULLIFY(grid%smfr3d)
ENDIF
IF ( ASSOCIATED( grid%keepfr3dflag ) ) THEN 
  DEALLOCATE(grid%keepfr3dflag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%keepfr3dflag. ')
 endif
  NULLIFY(grid%keepfr3dflag)
ENDIF
IF ( ASSOCIATED( grid%mp_restart_state ) ) THEN 
  DEALLOCATE(grid%mp_restart_state,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%mp_restart_state. ')
 endif
  NULLIFY(grid%mp_restart_state)
ENDIF
IF ( ASSOCIATED( grid%tbpvs_state ) ) THEN 
  DEALLOCATE(grid%tbpvs_state,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%tbpvs_state. ')
 endif
  NULLIFY(grid%tbpvs_state)
ENDIF
IF ( ASSOCIATED( grid%tbpvs0_state ) ) THEN 
  DEALLOCATE(grid%tbpvs0_state,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%tbpvs0_state. ')
 endif
  NULLIFY(grid%tbpvs0_state)
ENDIF
IF ( ASSOCIATED( grid%lu_state ) ) THEN 
  DEALLOCATE(grid%lu_state,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%lu_state. ')
 endif
  NULLIFY(grid%lu_state)
ENDIF
!ENDOFREGISTRYGENERATEDINCLUDE
      ELSE
        WRITE( wrf_err_message , * )'dealloc_space_field: ', &
          'Invalid specification of dynamics: dyn_opt = ',dyn_opt
        CALL wrf_error_fatal3 ( "module_domain.b" , 1260 ,  TRIM ( wrf_err_message ) )
      ENDIF

   END SUBROUTINE dealloc_space_field

!
!
   RECURSIVE SUBROUTINE find_grid_by_id ( id, in_grid, result_grid )
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: id
      TYPE(domain), POINTER     :: in_grid 
      TYPE(domain), POINTER     :: result_grid
! <DESCRIPTION>
! This is a recursive subroutine that traverses the domain hierarchy rooted
! at the input argument <em>in_grid</em>, a pointer to TYPE(domain), and returns
! a pointer to the domain matching the integer argument <em>id</em> if it exists.
!
! </DESCRIPTION>
      TYPE(domain), POINTER     :: grid_ptr
      INTEGER                   :: kid
      LOGICAL                   :: found
      found = .FALSE.
      IF ( ASSOCIATED( in_grid ) ) THEN
      IF ( in_grid%id .EQ. id ) THEN
         result_grid => in_grid
      ELSE
         grid_ptr => in_grid
         DO WHILE ( ASSOCIATED( grid_ptr ) .AND. .NOT. found )
            DO kid = 1, max_nests
               IF ( ASSOCIATED( grid_ptr%nests(kid)%ptr ) .AND. .NOT. found ) THEN
                  CALL find_grid_by_id ( id, grid_ptr%nests(kid)%ptr, result_grid )
                  IF ( ASSOCIATED( result_grid ) ) THEN
                    IF ( result_grid%id .EQ. id ) found = .TRUE.
                  ENDIF
               ENDIF
            ENDDO
            IF ( .NOT. found ) grid_ptr => grid_ptr%sibling
         ENDDO
      ENDIF
      ENDIF
      RETURN
   END SUBROUTINE find_grid_by_id


   FUNCTION first_loc_integer ( array , search ) RESULT ( loc ) 
 
      IMPLICIT NONE

      !  Input data.

      INTEGER , INTENT(IN) , DIMENSION(:) :: array
      INTEGER , INTENT(IN)                :: search

      !  Output data.

      INTEGER                             :: loc

!<DESCRIPTION>
!  This routine is used to find a specific domain identifier in an array
!  of domain identifiers.
!
!</DESCRIPTION>
      
      !  Local data.

      INTEGER :: loop

      loc = -1
      find : DO loop = 1 , SIZE(array)
         IF ( search == array(loop) ) THEN         
            loc = loop
            EXIT find
         END IF
      END DO find

   END FUNCTION first_loc_integer
!
   SUBROUTINE init_module_domain
   END SUBROUTINE init_module_domain


! <DESCRIPTION>
!
! The following routines named domain_*() are convenience routines that 
! eliminate many duplicated bits of code.  They provide shortcuts for the 
! most common operations on the domain_clock field of TYPE(domain).  
!
! </DESCRIPTION>

      FUNCTION domain_get_current_time ( grid ) RESULT ( current_time ) 
        IMPLICIT NONE
! <DESCRIPTION>
! This convenience function returns the current time for domain grid.  
!
! </DESCRIPTION>
        TYPE(domain), INTENT(IN) :: grid
        ! result
        TYPE(WRFU_Time) :: current_time
        ! locals
        INTEGER :: rc
        CALL WRFU_ClockGet( grid%domain_clock, CurrTime=current_time, &
                            rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL wrf_error_fatal3 ( "module_domain.b" , 1363 ,  &
            'domain_get_current_time:  WRFU_ClockGet failed' )
        ENDIF
      END FUNCTION domain_get_current_time


      FUNCTION domain_get_start_time ( grid ) RESULT ( start_time ) 
        IMPLICIT NONE
! <DESCRIPTION>
! This convenience function returns the start time for domain grid.  
!
! </DESCRIPTION>
        TYPE(domain), INTENT(IN) :: grid
        ! result
        TYPE(WRFU_Time) :: start_time
        ! locals
        INTEGER :: rc
        CALL WRFU_ClockGet( grid%domain_clock, StartTime=start_time, &
                            rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL wrf_error_fatal3 ( "module_domain.b" , 1383 ,  &
            'domain_get_start_time:  WRFU_ClockGet failed' )
        ENDIF
      END FUNCTION domain_get_start_time


      FUNCTION domain_get_stop_time ( grid ) RESULT ( stop_time ) 
        IMPLICIT NONE
! <DESCRIPTION>
! This convenience function returns the stop time for domain grid.  
!
! </DESCRIPTION>
        TYPE(domain), INTENT(IN) :: grid
        ! result
        TYPE(WRFU_Time) :: stop_time
        ! locals
        INTEGER :: rc
        CALL WRFU_ClockGet( grid%domain_clock, StopTime=stop_time, &
                            rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL wrf_error_fatal3 ( "module_domain.b" , 1403 ,  &
            'domain_get_stop_time:  WRFU_ClockGet failed' )
        ENDIF
      END FUNCTION domain_get_stop_time


      FUNCTION domain_get_time_step ( grid ) RESULT ( time_step ) 
        IMPLICIT NONE
! <DESCRIPTION>
! This convenience function returns the time step for domain grid.  
!
! </DESCRIPTION>
        TYPE(domain), INTENT(IN) :: grid
        ! result
        TYPE(WRFU_TimeInterval) :: time_step
        ! locals
        INTEGER :: rc
        CALL WRFU_ClockGet( grid%domain_clock, timeStep=time_step, &
                            rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL wrf_error_fatal3 ( "module_domain.b" , 1423 ,  &
            'domain_get_time_step:  WRFU_ClockGet failed' )
        ENDIF
      END FUNCTION domain_get_time_step


      FUNCTION domain_get_advanceCount ( grid ) RESULT ( advanceCount ) 
        IMPLICIT NONE
! <DESCRIPTION>
! This convenience function returns the time step for domain grid.  
! Also converts from INTEGER(WRFU_KIND_I8) to INTEGER.  
!
! </DESCRIPTION>
        TYPE(domain), INTENT(IN) :: grid
        ! result
        INTEGER :: advanceCount
        ! locals
        INTEGER(WRFU_KIND_I8) :: advanceCountLcl
        INTEGER :: rc
        CALL WRFU_ClockGet( grid%domain_clock, &
                            advanceCount=advanceCountLcl, &
                            rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL wrf_error_fatal3 ( "module_domain.b" , 1446 ,  &
            'domain_get_advanceCount:  WRFU_ClockGet failed' )
        ENDIF
        advanceCount = advanceCountLcl
      END FUNCTION domain_get_advanceCount


      SUBROUTINE domain_alarms_destroy ( grid )
        IMPLICIT NONE
! <DESCRIPTION>
! This convenience routine destroys and deallocates all alarms associated 
! with grid.  
!
! </DESCRIPTION>
        TYPE(domain), INTENT(INOUT) :: grid
        !  Local data.
        INTEGER                     :: alarmid

        IF ( ASSOCIATED( grid%alarms ) .AND. &
             ASSOCIATED( grid%alarms_created ) ) THEN
          DO alarmid = 1, MAX_WRF_ALARMS
            IF ( grid%alarms_created( alarmid ) ) THEN
              CALL WRFU_AlarmDestroy( grid%alarms( alarmid ) )
              grid%alarms_created( alarmid ) = .FALSE.
            ENDIF
          ENDDO
          DEALLOCATE( grid%alarms )
          NULLIFY( grid%alarms )
          DEALLOCATE( grid%alarms_created )
          NULLIFY( grid%alarms_created )
        ENDIF
      END SUBROUTINE domain_alarms_destroy


      SUBROUTINE domain_clock_destroy ( grid )
        IMPLICIT NONE
! <DESCRIPTION>
! This convenience routine destroys and deallocates the domain clock.  
!
! </DESCRIPTION>
        TYPE(domain), INTENT(INOUT) :: grid
        IF ( ASSOCIATED( grid%domain_clock ) ) THEN
          IF ( grid%domain_clock_created ) THEN
            CALL WRFU_ClockDestroy( grid%domain_clock )
            grid%domain_clock_created = .FALSE.
          ENDIF
          DEALLOCATE( grid%domain_clock )
          NULLIFY( grid%domain_clock )
        ENDIF
      END SUBROUTINE domain_clock_destroy


      FUNCTION domain_last_time_step ( grid ) RESULT ( LAST_TIME ) 
        IMPLICIT NONE
! <DESCRIPTION>
! This convenience function returns .TRUE. if this is the last time 
! step for domain grid.  Thanks to Tom Black.  
!
! </DESCRIPTION>
        TYPE(domain), INTENT(IN) :: grid
        ! result
        LOGICAL :: LAST_TIME
        LAST_TIME =   domain_get_stop_time( grid ) .EQ. &
                    ( domain_get_current_time( grid ) + &
                      domain_get_time_step( grid ) )
      END FUNCTION domain_last_time_step



      FUNCTION domain_clockisstoptime ( grid ) RESULT ( is_stop_time ) 
        IMPLICIT NONE
! <DESCRIPTION>
! This convenience function returns .TRUE. iff grid%clock has reached its 
! stop time.  
!
! </DESCRIPTION>
        TYPE(domain), INTENT(IN) :: grid
        ! result
        LOGICAL :: is_stop_time
        INTEGER :: rc
        is_stop_time = WRFU_ClockIsStopTime( grid%domain_clock , rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL wrf_error_fatal3 ( "module_domain.b" , 1528 ,  &
            'domain_clockisstoptime:  WRFU_ClockIsStopTime() failed' )
        ENDIF
      END FUNCTION domain_clockisstoptime



      FUNCTION domain_clockisstopsubtime ( grid ) RESULT ( is_stop_subtime ) 
        IMPLICIT NONE
! <DESCRIPTION>
! This convenience function returns .TRUE. iff grid%clock has reached its 
! grid%stop_subtime.  
!
! </DESCRIPTION>
        TYPE(domain), INTENT(IN) :: grid
        ! result
        LOGICAL :: is_stop_subtime
        INTEGER :: rc
        TYPE(WRFU_TimeInterval) :: timeStep
        TYPE(WRFU_Time) :: currentTime
        LOGICAL :: positive_timestep
        is_stop_subtime = .FALSE.
        CALL domain_clock_get( grid, time_step=timeStep, &
                                     current_time=currentTime )
        positive_timestep = ESMF_TimeIntervalIsPositive( timeStep )
        IF ( positive_timestep ) THEN
! hack for bug in PGI 5.1-x
!        IF ( currentTime .GE. grid%stop_subtime ) THEN
          IF ( ESMF_TimeGE( currentTime, grid%stop_subtime ) ) THEN
            is_stop_subtime = .TRUE.
          ENDIF
        ELSE
! hack for bug in PGI 5.1-x
!        IF ( currentTime .LE. grid%stop_subtime ) THEN
          IF ( ESMF_TimeLE( currentTime, grid%stop_subtime ) ) THEN
            is_stop_subtime = .TRUE.
          ENDIF
        ENDIF
      END FUNCTION domain_clockisstopsubtime




      FUNCTION domain_get_sim_start_time ( grid ) RESULT ( simulationStartTime ) 
        IMPLICIT NONE
! <DESCRIPTION>
! This convenience routine returns simulation start time for domain grid as 
! a time instant.  
!
! If this is not a restart run, the start_time of head_grid%clock is returned 
! instead.  
!
! Note that simulation start time remains constant through restarts while 
! the start_time of head_grid%clock always refers to the start time of the 
! current run (restart or otherwise).  
!
! </DESCRIPTION>
        TYPE(domain), INTENT(IN) :: grid
        ! result
        TYPE(WRFU_Time) :: simulationStartTime
        ! Locals
        INTEGER :: rc
        INTEGER :: simulation_start_year,   simulation_start_month, &
                   simulation_start_day,    simulation_start_hour , &
                   simulation_start_minute, simulation_start_second
        CALL nl_get_simulation_start_year   ( 1, simulation_start_year   )
        CALL nl_get_simulation_start_month  ( 1, simulation_start_month  )
        CALL nl_get_simulation_start_day    ( 1, simulation_start_day    )
        CALL nl_get_simulation_start_hour   ( 1, simulation_start_hour   )
        CALL nl_get_simulation_start_minute ( 1, simulation_start_minute )
        CALL nl_get_simulation_start_second ( 1, simulation_start_second )
        CALL WRFU_TimeSet( simulationStartTime,       &
                           YY=simulation_start_year,  &
                           MM=simulation_start_month, &
                           DD=simulation_start_day,   &
                           H=simulation_start_hour,   &
                           M=simulation_start_minute, &
                           S=simulation_start_second, &
                           rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL nl_get_start_year   ( 1, simulation_start_year   )
          CALL nl_get_start_month  ( 1, simulation_start_month  )
          CALL nl_get_start_day    ( 1, simulation_start_day    )
          CALL nl_get_start_hour   ( 1, simulation_start_hour   )
          CALL nl_get_start_minute ( 1, simulation_start_minute )
          CALL nl_get_start_second ( 1, simulation_start_second )
          CALL wrf_debug( 150, "WARNING:  domain_get_sim_start_time using head_grid start time from namelist" )
          CALL WRFU_TimeSet( simulationStartTime,       &
                             YY=simulation_start_year,  &
                             MM=simulation_start_month, &
                             DD=simulation_start_day,   &
                             H=simulation_start_hour,   &
                             M=simulation_start_minute, &
                             S=simulation_start_second, &
                             rc=rc )
        ENDIF
        RETURN
      END FUNCTION domain_get_sim_start_time

      FUNCTION domain_get_time_since_sim_start ( grid ) RESULT ( time_since_sim_start ) 
        IMPLICIT NONE
! <DESCRIPTION>
! This convenience function returns the time elapsed since start of 
! simulation for domain grid.  
!
! Note that simulation start time remains constant through restarts while 
! the start_time of grid%clock always refers to the start time of the 
! current run (restart or otherwise).  
!
! </DESCRIPTION>
        TYPE(domain), INTENT(IN) :: grid
        ! result
        TYPE(WRFU_TimeInterval) :: time_since_sim_start
        ! locals
        TYPE(WRFU_Time) :: lcl_currtime, lcl_simstarttime
        lcl_simstarttime = domain_get_sim_start_time( grid )
        lcl_currtime = domain_get_current_time ( grid )
        time_since_sim_start = lcl_currtime - lcl_simstarttime
      END FUNCTION domain_get_time_since_sim_start




      SUBROUTINE domain_clock_get( grid, current_time,                &
                                         current_timestr,             &
                                         current_timestr_frac,        &
                                         start_time, start_timestr,   &
                                         stop_time, stop_timestr,     &
                                         time_step, time_stepstr,     &
                                         time_stepstr_frac,           &
                                         advanceCount,                &
                                         currentDayOfYearReal,        &
                                         minutesSinceSimulationStart, &
                                         timeSinceSimulationStart,    &
                                         simulationStartTime,         &
                                         simulationStartTimeStr )
        IMPLICIT NONE
        TYPE(domain),            INTENT(IN)              :: grid
        TYPE(WRFU_Time),         INTENT(  OUT), OPTIONAL :: current_time
        CHARACTER (LEN=*),       INTENT(  OUT), OPTIONAL :: current_timestr
        CHARACTER (LEN=*),       INTENT(  OUT), OPTIONAL :: current_timestr_frac
        TYPE(WRFU_Time),         INTENT(  OUT), OPTIONAL :: start_time
        CHARACTER (LEN=*),       INTENT(  OUT), OPTIONAL :: start_timestr
        TYPE(WRFU_Time),         INTENT(  OUT), OPTIONAL :: stop_time
        CHARACTER (LEN=*),       INTENT(  OUT), OPTIONAL :: stop_timestr
        TYPE(WRFU_TimeInterval), INTENT(  OUT), OPTIONAL :: time_step
        CHARACTER (LEN=*),       INTENT(  OUT), OPTIONAL :: time_stepstr
        CHARACTER (LEN=*),       INTENT(  OUT), OPTIONAL :: time_stepstr_frac
        INTEGER,                 INTENT(  OUT), OPTIONAL :: advanceCount
        ! currentDayOfYearReal = 0.0 at 0Z on 1 January, 0.5 at 12Z on 
        ! 1 January, etc.
        REAL,                    INTENT(  OUT), OPTIONAL :: currentDayOfYearReal
        ! Time at which simulation started.  If this is not a restart run, 
        ! start_time is returned instead.  
        TYPE(WRFU_Time),         INTENT(  OUT), OPTIONAL :: simulationStartTime
        CHARACTER (LEN=*),       INTENT(  OUT), OPTIONAL :: simulationStartTimeStr
        ! time interval since start of simulation, includes effects of 
        ! restarting even when restart uses a different timestep
        TYPE(WRFU_TimeInterval), INTENT(  OUT), OPTIONAL :: timeSinceSimulationStart
        ! minutes since simulation start date
        REAL,                    INTENT(  OUT), OPTIONAL :: minutesSinceSimulationStart
! <DESCRIPTION>
! This convenience routine returns clock information for domain grid in 
! various forms.  The caller is responsible for ensuring that character 
! string actual arguments are big enough.  
!
! </DESCRIPTION>
        ! Locals
        TYPE(WRFU_Time) :: lcl_currtime, lcl_stoptime, lcl_starttime
        TYPE(WRFU_Time) :: lcl_simulationStartTime
        TYPE(WRFU_TimeInterval) :: lcl_time_step, lcl_timeSinceSimulationStart
        INTEGER :: days, seconds, Sn, Sd, rc
        CHARACTER (LEN=256) :: tmp_str
        CHARACTER (LEN=256) :: frac_str
        REAL(WRFU_KIND_R8) :: currentDayOfYearR8
        IF ( PRESENT( start_time ) ) THEN
          start_time = domain_get_start_time ( grid )
        ENDIF
        IF ( PRESENT( start_timestr ) ) THEN
          lcl_starttime = domain_get_start_time ( grid )
          CALL wrf_timetoa ( lcl_starttime, start_timestr )
        ENDIF
        IF ( PRESENT( time_step ) ) THEN
          time_step = domain_get_time_step ( grid )
        ENDIF
        IF ( PRESENT( time_stepstr ) ) THEN
          lcl_time_step = domain_get_time_step ( grid )
          CALL WRFU_TimeIntervalGet( lcl_time_step, &
                                     timeString=time_stepstr, rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
            CALL wrf_error_fatal3 ( "module_domain.b" , 1718 ,  &
              'domain_clock_get:  WRFU_TimeIntervalGet() failed' )
          ENDIF
        ENDIF
        IF ( PRESENT( time_stepstr_frac ) ) THEN
          lcl_time_step = domain_get_time_step ( grid )
          CALL WRFU_TimeIntervalGet( lcl_time_step, timeString=tmp_str, &
                                     Sn=Sn, Sd=Sd, rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
            CALL wrf_error_fatal3 ( "module_domain.b" , 1727 ,  &
              'domain_clock_get:  WRFU_TimeIntervalGet() failed' )
          ENDIF
          CALL fraction_to_string( Sn, Sd, frac_str )
          time_stepstr_frac = TRIM(tmp_str)//TRIM(frac_str)
        ENDIF
        IF ( PRESENT( advanceCount ) ) THEN
          advanceCount = domain_get_advanceCount ( grid )
        ENDIF
        ! This duplication avoids assignment of time-manager objects 
        ! which works now in ESMF 2.2.0 but may not work in the future 
        ! if these objects become "deep".  We have already been bitten 
        ! by this when the clock objects were changed from "shallow" to 
        ! "deep".  Once again, adherence to orthodox canonical form by 
        ! ESMF would avoid all this crap.  
        IF ( PRESENT( current_time ) ) THEN
          current_time = domain_get_current_time ( grid )
        ENDIF
        IF ( PRESENT( current_timestr ) ) THEN
          lcl_currtime = domain_get_current_time ( grid )
          CALL wrf_timetoa ( lcl_currtime, current_timestr )
        ENDIF
        ! current time string including fractional part, if present
        IF ( PRESENT( current_timestr_frac ) ) THEN
          lcl_currtime = domain_get_current_time ( grid )
          CALL wrf_timetoa ( lcl_currtime, tmp_str )
          CALL WRFU_TimeGet( lcl_currtime, Sn=Sn, Sd=Sd, rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
            CALL wrf_error_fatal3 ( "module_domain.b" , 1755 ,  &
              'domain_clock_get:  WRFU_TimeGet() failed' )
          ENDIF
          CALL fraction_to_string( Sn, Sd, frac_str )
          current_timestr_frac = TRIM(tmp_str)//TRIM(frac_str)
        ENDIF
        IF ( PRESENT( stop_time ) ) THEN
          stop_time = domain_get_stop_time ( grid )
        ENDIF
        IF ( PRESENT( stop_timestr ) ) THEN
          lcl_stoptime = domain_get_stop_time ( grid )
          CALL wrf_timetoa ( lcl_stoptime, stop_timestr )
        ENDIF
        IF ( PRESENT( currentDayOfYearReal ) ) THEN
          lcl_currtime = domain_get_current_time ( grid )
          CALL WRFU_TimeGet( lcl_currtime, dayOfYear_r8=currentDayOfYearR8, &
                             rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
            CALL wrf_error_fatal3 ( "module_domain.b" , 1773 ,  &
                   'domain_clock_get:  WRFU_TimeGet(dayOfYear_r8) failed' )
          ENDIF
          currentDayOfYearReal = REAL( currentDayOfYearR8 ) - 1.0
        ENDIF
        IF ( PRESENT( simulationStartTime ) ) THEN
          simulationStartTime = domain_get_sim_start_time( grid )
        ENDIF
        IF ( PRESENT( simulationStartTimeStr ) ) THEN
          lcl_simulationStartTime = domain_get_sim_start_time( grid )
          CALL wrf_timetoa ( lcl_simulationStartTime, simulationStartTimeStr )
        ENDIF
        IF ( PRESENT( timeSinceSimulationStart ) ) THEN
          timeSinceSimulationStart = domain_get_time_since_sim_start( grid )
        ENDIF
        IF ( PRESENT( minutesSinceSimulationStart ) ) THEN
          lcl_timeSinceSimulationStart = domain_get_time_since_sim_start( grid )
          CALL WRFU_TimeIntervalGet( lcl_timeSinceSimulationStart, &
                                     D=days, S=seconds, Sn=Sn, Sd=Sd, rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
            CALL wrf_error_fatal3 ( "module_domain.b" , 1793 ,  &
                   'domain_clock_get:  WRFU_TimeIntervalGet() failed' )
          ENDIF
          ! get rid of hard-coded constants
          minutesSinceSimulationStart = ( REAL( days ) * 24. * 60. ) + &
                                        ( REAL( seconds ) / 60. )
          IF ( Sd /= 0 ) THEN
            minutesSinceSimulationStart = minutesSinceSimulationStart + &
                                          ( ( REAL( Sn ) / REAL( Sd ) ) / 60. )
          ENDIF
        ENDIF
        RETURN
      END SUBROUTINE domain_clock_get

      FUNCTION domain_clockisstarttime ( grid ) RESULT ( is_start_time ) 
        IMPLICIT NONE
! <DESCRIPTION>
! This convenience function returns .TRUE. iff grid%clock is at its 
! start time.  
!
! </DESCRIPTION>
        TYPE(domain), INTENT(IN) :: grid
        ! result
        LOGICAL :: is_start_time
        TYPE(WRFU_Time) :: start_time, current_time
        CALL domain_clock_get( grid, current_time=current_time, &
                                     start_time=start_time )
        is_start_time = ( current_time == start_time )
      END FUNCTION domain_clockisstarttime

      FUNCTION domain_clockissimstarttime ( grid ) RESULT ( is_sim_start_time ) 
        IMPLICIT NONE
! <DESCRIPTION>
! This convenience function returns .TRUE. iff grid%clock is at the 
! simulation start time.  (It returns .FALSE. during a restart run.)  
!
! </DESCRIPTION>
        TYPE(domain), INTENT(IN) :: grid
        ! result
        LOGICAL :: is_sim_start_time
        TYPE(WRFU_Time) :: simulationStartTime, current_time
        CALL domain_clock_get( grid, current_time=current_time, &
                                     simulationStartTime=simulationStartTime )
        is_sim_start_time = ( current_time == simulationStartTime )
      END FUNCTION domain_clockissimstarttime




      SUBROUTINE domain_clock_create( grid, StartTime, &
                                            StopTime,  &
                                            TimeStep )
        IMPLICIT NONE
        TYPE(domain),            INTENT(INOUT) :: grid
        TYPE(WRFU_Time),         INTENT(IN   ) :: StartTime
        TYPE(WRFU_Time),         INTENT(IN   ) :: StopTime
        TYPE(WRFU_TimeInterval), INTENT(IN   ) :: TimeStep
! <DESCRIPTION>
! This convenience routine creates the domain_clock for domain grid and 
! sets associated flags.  
!
! </DESCRIPTION>
        ! Locals
        INTEGER :: rc
        grid%domain_clock = WRFU_ClockCreate( TimeStep= TimeStep,  &
                                              StartTime=StartTime, &
                                              StopTime= StopTime,  &
                                              rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL wrf_error_fatal3 ( "module_domain.b" , 1862 ,  &
            'domain_clock_create:  WRFU_ClockCreate() failed' )
        ENDIF
        grid%domain_clock_created = .TRUE.
        RETURN
      END SUBROUTINE domain_clock_create



      SUBROUTINE domain_alarm_create( grid, alarm_id, interval, &
                                            begin_time, end_time )
        USE module_utility
        IMPLICIT NONE
        TYPE(domain), POINTER :: grid
        INTEGER, INTENT(IN) :: alarm_id
        TYPE(WRFU_TimeInterval), INTENT(IN), OPTIONAL :: interval
        TYPE(WRFU_TimeInterval), INTENT(IN), OPTIONAL :: begin_time
        TYPE(WRFU_TimeInterval), INTENT(IN), OPTIONAL :: end_time
! <DESCRIPTION>
! This convenience routine creates alarm alarm_id for domain grid and 
! sets associated flags.  
!
! </DESCRIPTION>
        ! Locals
        INTEGER :: rc
!$$$ TBH:  Ideally, this could be simplified by passing all optional actual 
!$$$ TBH:  args into AlarmCreate.  However, since operations are performed on 
!$$$ TBH:  the actual args in-place in the calls, they must be present for the 
!$$$ TBH:  operations themselves to be defined.  Grrr...  
        LOGICAL :: interval_only, all_args, no_args
        TYPE(WRFU_Time) :: startTime
        interval_only = .FALSE.
        all_args = .FALSE.
        no_args = .FALSE.
        IF ( ( .NOT. PRESENT( begin_time ) ) .AND. &
             ( .NOT. PRESENT( end_time   ) ) .AND. &
             (       PRESENT( interval   ) ) ) THEN
           interval_only = .TRUE.
        ELSE IF ( ( .NOT. PRESENT( begin_time ) ) .AND. &
                  ( .NOT. PRESENT( end_time   ) ) .AND. &
                  ( .NOT. PRESENT( interval   ) ) ) THEN
           no_args = .TRUE.
        ELSE IF ( (       PRESENT( begin_time ) ) .AND. &
                  (       PRESENT( end_time   ) ) .AND. &
                  (       PRESENT( interval   ) ) ) THEN
           all_args = .TRUE.
        ELSE
           CALL wrf_error_fatal3 ( "module_domain.b" , 1909 ,  &
             'ERROR in domain_alarm_create:  bad argument list' )
        ENDIF
        CALL domain_clock_get( grid, start_time=startTime )
        IF ( interval_only ) THEN
           grid%io_intervals( alarm_id ) = interval
           grid%alarms( alarm_id ) = &
             WRFU_AlarmCreate( clock=grid%domain_clock, &
                               RingInterval=interval,   &
                               rc=rc )
        ELSE IF ( no_args ) THEN
           grid%alarms( alarm_id ) = &
             WRFU_AlarmCreate( clock=grid%domain_clock, &
                               RingTime=startTime,      &
                               rc=rc )
        ELSE IF ( all_args ) THEN
           grid%io_intervals( alarm_id ) = interval
           grid%alarms( alarm_id ) = &
             WRFU_AlarmCreate( clock=grid%domain_clock,         &
                               RingTime=startTime + begin_time, &
                               RingInterval=interval,           &
                               StopTime=startTime + end_time,   &
                               rc=rc )
        ENDIF
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL wrf_error_fatal3 ( "module_domain.b" , 1934 ,  &
            'domain_alarm_create:  WRFU_AlarmCreate() failed' )
        ENDIF
        grid%alarms_created( alarm_id ) = .TRUE.
      END SUBROUTINE domain_alarm_create



      SUBROUTINE domain_clock_set( grid, current_timestr, &
                                         stop_timestr,    &
                                         time_step_seconds )
        IMPLICIT NONE
        TYPE(domain),      INTENT(INOUT)           :: grid
        CHARACTER (LEN=*), INTENT(IN   ), OPTIONAL :: current_timestr
        CHARACTER (LEN=*), INTENT(IN   ), OPTIONAL :: stop_timestr
        INTEGER,           INTENT(IN   ), OPTIONAL :: time_step_seconds
! <DESCRIPTION>
! This convenience routine sets clock information for domain grid.  
! The caller is responsible for ensuring that character string actual 
! arguments are big enough.  
!
! </DESCRIPTION>
        ! Locals
        TYPE(WRFU_Time) :: lcl_currtime, lcl_stoptime
        TYPE(WRFU_TimeInterval) :: tmpTimeInterval
        INTEGER :: rc
        IF ( PRESENT( current_timestr ) ) THEN
          CALL wrf_atotime( current_timestr(1:19), lcl_currtime )
          CALL WRFU_ClockSet( grid%domain_clock, currTime=lcl_currtime, &
                              rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
            CALL wrf_error_fatal3 ( "module_domain.b" , 1965 ,  &
              'domain_clock_set:  WRFU_ClockSet(CurrTime) failed' )
          ENDIF
        ENDIF
        IF ( PRESENT( stop_timestr ) ) THEN
          CALL wrf_atotime( stop_timestr(1:19), lcl_stoptime )
          CALL WRFU_ClockSet( grid%domain_clock, stopTime=lcl_stoptime, &
                              rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
            CALL wrf_error_fatal3 ( "module_domain.b" , 1974 ,  &
              'domain_clock_set:  WRFU_ClockSet(StopTime) failed' )
          ENDIF
        ENDIF
        IF ( PRESENT( time_step_seconds ) ) THEN
          CALL WRFU_TimeIntervalSet( tmpTimeInterval, &
                                     S=time_step_seconds, rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
            CALL wrf_error_fatal3 ( "module_domain.b" , 1982 ,  &
              'domain_clock_set:  WRFU_TimeIntervalSet failed' )
          ENDIF
          CALL WRFU_ClockSet ( grid%domain_clock,        &
                               timeStep=tmpTimeInterval, &
                               rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
            CALL wrf_error_fatal3 ( "module_domain.b" , 1989 ,  &
              'domain_clock_set:  WRFU_ClockSet(TimeStep) failed' )
          ENDIF
        ENDIF
        RETURN
      END SUBROUTINE domain_clock_set


      ! Debug routine to print key clock information.  
      ! Printed lines include pre_str.  
      SUBROUTINE domain_clockprint ( level, grid, pre_str )
        IMPLICIT NONE
        INTEGER,           INTENT( IN) :: level
        TYPE(domain),      INTENT( IN) :: grid
        CHARACTER (LEN=*), INTENT( IN) :: pre_str
        CALL wrf_clockprint ( level, grid%domain_clock, pre_str )
        RETURN
      END SUBROUTINE domain_clockprint


      ! Advance the clock associated with grid.  
      ! Also updates several derived time quantities in grid state.  
      SUBROUTINE domain_clockadvance ( grid )
        IMPLICIT NONE
        TYPE(domain), INTENT(INOUT) :: grid
        INTEGER :: rc
        CALL domain_clockprint ( 250, grid, &
          'DEBUG domain_clockadvance():  before WRFU_ClockAdvance,' )
        CALL WRFU_ClockAdvance( grid%domain_clock, rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL wrf_error_fatal3 ( "module_domain.b" , 2019 ,  &
            'domain_clockadvance:  WRFU_ClockAdvance() failed' )
        ENDIF
        CALL domain_clockprint ( 250, grid, &
          'DEBUG domain_clockadvance():  after WRFU_ClockAdvance,' )
        ! Update derived time quantities in grid state.
        ! These are initialized in setup_timekeeping().
        CALL domain_clock_get( grid, minutesSinceSimulationStart=grid%xtime )
        CALL domain_clock_get( grid, currentDayOfYearReal=grid%julian )
        RETURN
      END SUBROUTINE domain_clockadvance



      ! Set grid%gmt, grid%julday, and grid%julyr from simulation-start-date.  
      ! Set start_of_simulation to TRUE iff current_time == simulation_start_time
      SUBROUTINE domain_setgmtetc ( grid, start_of_simulation )
        IMPLICIT NONE
        TYPE (domain), INTENT(INOUT) :: grid
        LOGICAL,       INTENT(  OUT) :: start_of_simulation
        ! locals
        CHARACTER (LEN=132)          :: message
        TYPE(WRFU_Time)              :: simStartTime
        INTEGER                      :: hr, mn, sec, ms, rc
        CALL domain_clockprint(150, grid, &
          'DEBUG domain_setgmtetc():  get simStartTime from clock,')
        CALL domain_clock_get( grid, simulationStartTime=simStartTime, &
                                     simulationStartTimeStr=message )
        CALL WRFU_TimeGet( simStartTime, YY=grid%julyr, dayOfYear=grid%julday, &
                           H=hr, M=mn, S=sec, MS=ms, rc=rc)
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL wrf_error_fatal3 ( "module_domain.b" , 2050 ,  &
            'domain_setgmtetc:  WRFU_TimeGet() failed' )
        ENDIF
        WRITE( wrf_err_message , * ) 'DEBUG domain_setgmtetc():  simulation start time = [',TRIM( message ),']'
        CALL wrf_debug( 150, TRIM(wrf_err_message) )
        grid%gmt=hr+real(mn)/60.+real(sec)/3600.+real(ms)/(1000*3600)
        WRITE( wrf_err_message , * ) 'DEBUG domain_setgmtetc():  julyr,hr,mn,sec,ms,julday = ', &
                                     grid%julyr,hr,mn,sec,ms,grid%julday
        CALL wrf_debug( 150, TRIM(wrf_err_message) )
        WRITE( wrf_err_message , * ) 'DEBUG domain_setgmtetc():  gmt = ',grid%gmt
        CALL wrf_debug( 150, TRIM(wrf_err_message) )
        start_of_simulation = domain_ClockIsSimStartTime(grid)
        RETURN
      END SUBROUTINE domain_setgmtetc
     


      ! Set pointer to current grid.  
      ! To begin with, current grid is not set.  
      SUBROUTINE set_current_grid_ptr( grid_ptr )
        IMPLICIT NONE
        TYPE(domain), POINTER :: grid_ptr
!PRINT *,DEBUG:  begin set_current_grid_ptr()
!IF ( ASSOCIATED( grid_ptr ) ) THEN
!  PRINT *,DEBUG:  set_current_grid_ptr():  current_grid is associated
!ELSE
!  PRINT *,DEBUG:  set_current_grid_ptr():  current_grid is NOT associated
!ENDIF
        current_grid_set = .TRUE.
        current_grid => grid_ptr
!PRINT *,DEBUG:  end set_current_grid_ptr()
      END SUBROUTINE set_current_grid_ptr



!******************************************************************************
! BEGIN TEST SECTION
!   Code in the test section is used to test domain methods.  
!   This code should probably be moved elsewhere, eventually.  
!******************************************************************************

      ! Private utility routines for domain_time_test.  
      SUBROUTINE domain_time_test_print ( pre_str, name_str, res_str )
        IMPLICIT NONE
        CHARACTER (LEN=*), INTENT(IN) :: pre_str
        CHARACTER (LEN=*), INTENT(IN) :: name_str
        CHARACTER (LEN=*), INTENT(IN) :: res_str
        CHARACTER (LEN=512) :: out_str
        WRITE (out_str,                                            &
          FMT="('DOMAIN_TIME_TEST ',A,':  ',A,' = ',A)") &
          TRIM(pre_str), TRIM(name_str), TRIM(res_str)
        CALL wrf_debug( 0, TRIM(out_str) )
      END SUBROUTINE domain_time_test_print

      ! Test adjust_io_timestr 
      SUBROUTINE test_adjust_io_timestr( TI_h, TI_m, TI_s, &
        CT_yy,  CT_mm,  CT_dd,  CT_h,  CT_m,  CT_s,        &
        ST_yy,  ST_mm,  ST_dd,  ST_h,  ST_m,  ST_s,        &
        res_str, testname )
        INTEGER, INTENT(IN) :: TI_H
        INTEGER, INTENT(IN) :: TI_M
        INTEGER, INTENT(IN) :: TI_S
        INTEGER, INTENT(IN) :: CT_YY
        INTEGER, INTENT(IN) :: CT_MM  ! month
        INTEGER, INTENT(IN) :: CT_DD  ! day of month
        INTEGER, INTENT(IN) :: CT_H
        INTEGER, INTENT(IN) :: CT_M
        INTEGER, INTENT(IN) :: CT_S
        INTEGER, INTENT(IN) :: ST_YY
        INTEGER, INTENT(IN) :: ST_MM  ! month
        INTEGER, INTENT(IN) :: ST_DD  ! day of month
        INTEGER, INTENT(IN) :: ST_H
        INTEGER, INTENT(IN) :: ST_M
        INTEGER, INTENT(IN) :: ST_S
        CHARACTER (LEN=*), INTENT(IN) :: res_str
        CHARACTER (LEN=*), INTENT(IN) :: testname
        ! locals
        TYPE(WRFU_TimeInterval) :: TI
        TYPE(WRFU_Time) :: CT, ST
        LOGICAL :: test_passed
        INTEGER :: rc
        CHARACTER(LEN=WRFU_MAXSTR) :: TI_str, CT_str, ST_str, computed_str
        ! TI
        CALL WRFU_TimeIntervalSet( TI, H=TI_H, M=TI_M, S=TI_S, rc=rc )
        CALL wrf_check_error( WRFU_SUCCESS, rc, &
                              'FAIL:  '//TRIM(testname)//'WRFU_TimeIntervalSet() ', &
                              "module_domain.b" , &
                              2137  )
        CALL WRFU_TimeIntervalGet( TI, timeString=TI_str, rc=rc )
        CALL wrf_check_error( WRFU_SUCCESS, rc, &
                              'FAIL:  '//TRIM(testname)//'WRFU_TimeGet() ', &
                              "module_domain.b" , &
                              2142  )
        ! CT
        CALL WRFU_TimeSet( CT, YY=CT_YY, MM=CT_MM, DD=CT_DD , &
                                H=CT_H,   M=CT_M,   S=CT_S, rc=rc )
        CALL wrf_check_error( WRFU_SUCCESS, rc, &
                              'FAIL:  '//TRIM(testname)//'WRFU_TimeSet() ', &
                              "module_domain.b" , &
                              2149  )
        CALL WRFU_TimeGet( CT, timeString=CT_str, rc=rc )
        CALL wrf_check_error( WRFU_SUCCESS, rc, &
                              'FAIL:  '//TRIM(testname)//'WRFU_TimeGet() ', &
                              "module_domain.b" , &
                              2154  )
        ! ST
        CALL WRFU_TimeSet( ST, YY=ST_YY, MM=ST_MM, DD=ST_DD , &
                                H=ST_H,   M=ST_M,   S=ST_S, rc=rc )
        CALL wrf_check_error( WRFU_SUCCESS, rc, &
                              'FAIL:  '//TRIM(testname)//'WRFU_TimeSet() ', &
                              "module_domain.b" , &
                              2161  )
        CALL WRFU_TimeGet( ST, timeString=ST_str, rc=rc )
        CALL wrf_check_error( WRFU_SUCCESS, rc, &
                              'FAIL:  '//TRIM(testname)//'WRFU_TimeGet() ', &
                              "module_domain.b" , &
                              2166  )
        ! Test
        CALL adjust_io_timestr ( TI, CT, ST, computed_str )
        ! check result
        test_passed = .FALSE.
        IF ( LEN_TRIM(res_str) == LEN_TRIM(computed_str) ) THEN
          IF ( res_str(1:LEN_TRIM(res_str)) == computed_str(1:LEN_TRIM(computed_str)) ) THEN
            test_passed = .TRUE.
          ENDIF
        ENDIF
        ! print result
        IF ( test_passed ) THEN
          WRITE(*,FMT='(A)') 'PASS:  '//TRIM(testname)
        ELSE
          WRITE(*,*) 'FAIL:  ',TRIM(testname),':  adjust_io_timestr(',    &
            TRIM(TI_str),',',TRIM(CT_str),',',TRIM(ST_str),')  expected <', &
            TRIM(res_str),'>  but computed <',TRIM(computed_str),'>'
        ENDIF
      END SUBROUTINE test_adjust_io_timestr

      ! Print lots of time-related information for testing and debugging.  
      ! Printed lines include pre_str and special string DOMAIN_TIME_TEST 
      ! suitable for grepping by test scripts.  
      ! Returns immediately unless self_test_domain has been set to .true. in 
      ! namelist /time_control/ .  
      SUBROUTINE domain_time_test ( grid, pre_str )
        IMPLICIT NONE
        TYPE(domain),      INTENT(IN) :: grid
        CHARACTER (LEN=*), INTENT(IN) :: pre_str
        ! locals
        LOGICAL, SAVE :: one_time_tests_done = .FALSE.
        REAL :: minutesSinceSimulationStart
        INTEGER :: advance_count, rc
        REAL :: currentDayOfYearReal
        TYPE(WRFU_TimeInterval) :: timeSinceSimulationStart
        TYPE(WRFU_Time) :: simulationStartTime
        CHARACTER (LEN=512) :: res_str
        LOGICAL :: self_test_domain
        !
        ! NOTE:  test_adjust_io_timestr() (see below) is a self-test that 
        !        prints PASS/FAIL/ERROR messages in a standard format.  All 
        !        of the other tests should be strucutred the same way, 
        !        someday.  
        !
        CALL nl_get_self_test_domain( 1, self_test_domain )
        IF ( self_test_domain ) THEN
          CALL domain_clock_get( grid, advanceCount=advance_count )
          WRITE ( res_str, FMT="(I8.8)" ) advance_count
          CALL domain_time_test_print( pre_str, 'advanceCount', res_str )
          CALL domain_clock_get( grid, currentDayOfYearReal=currentDayOfYearReal )
          WRITE ( res_str, FMT='(F10.6)' ) currentDayOfYearReal
          CALL domain_time_test_print( pre_str, 'currentDayOfYearReal', res_str )
          CALL domain_clock_get( grid, minutesSinceSimulationStart=minutesSinceSimulationStart )
          WRITE ( res_str, FMT='(F10.6)' ) minutesSinceSimulationStart
          CALL domain_time_test_print( pre_str, 'minutesSinceSimulationStart', res_str )
          CALL domain_clock_get( grid, current_timestr=res_str )
          CALL domain_time_test_print( pre_str, 'current_timestr', res_str )
          CALL domain_clock_get( grid, current_timestr_frac=res_str )
          CALL domain_time_test_print( pre_str, 'current_timestr_frac', res_str )
          CALL domain_clock_get( grid, timeSinceSimulationStart=timeSinceSimulationStart )
          CALL WRFU_TimeIntervalGet( timeSinceSimulationStart, timeString=res_str, rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
            CALL wrf_error_fatal3 ( "module_domain.b" , 2228 ,  &
              'domain_time_test:  WRFU_TimeIntervalGet() failed' )
          ENDIF
          CALL domain_time_test_print( pre_str, 'timeSinceSimulationStart', res_str )
          ! The following tests should only be done once, the first time this 
          ! routine is called.  
          IF ( .NOT. one_time_tests_done ) THEN
            one_time_tests_done = .TRUE.
            CALL domain_clock_get( grid, simulationStartTimeStr=res_str )
            CALL domain_time_test_print( pre_str, 'simulationStartTime', res_str )
            CALL domain_clock_get( grid, start_timestr=res_str )
            CALL domain_time_test_print( pre_str, 'start_timestr', res_str )
            CALL domain_clock_get( grid, stop_timestr=res_str )
            CALL domain_time_test_print( pre_str, 'stop_timestr', res_str )
            CALL domain_clock_get( grid, time_stepstr=res_str )
            CALL domain_time_test_print( pre_str, 'time_stepstr', res_str )
            CALL domain_clock_get( grid, time_stepstr_frac=res_str )
            CALL domain_time_test_print( pre_str, 'time_stepstr_frac', res_str )
            ! Test adjust_io_timestr()
            !     CT = 2000-01-26_00:00:00   (current time)
            !     ST = 2000-01-24_12:00:00   (start time)
            !     TI = 00000_03:00:00        (time interval)
            ! the resulting time string should be:
            !     2000-01-26_00:00:00
            CALL test_adjust_io_timestr( TI_h=3, TI_m=0, TI_s=0,          &
              CT_yy=2000,  CT_mm=1,  CT_dd=26,  CT_h=0,  CT_m=0,  CT_s=0, &
              ST_yy=2000,  ST_mm=1,  ST_dd=24,  ST_h=12, ST_m=0,  ST_s=0, &
              res_str='2000-01-26_00:00:00', testname='adjust_io_timestr_1' )
            ! this should fail (and does)
            !  CALL test_adjust_io_timestr( TI_h=3, TI_m=0, TI_s=0,          &
            !    CT_yy=2000,  CT_mm=1,  CT_dd=26,  CT_h=0,  CT_m=0,  CT_s=0, &
            !    ST_yy=2000,  ST_mm=1,  ST_dd=24,  ST_h=12, ST_m=0,  ST_s=0, &
            !    res_str=2000-01-26_00:00:01, testname=adjust_io_timestr_FAIL1 )
          ENDIF
        ENDIF
        RETURN
      END SUBROUTINE domain_time_test

!******************************************************************************
! END TEST SECTION
!******************************************************************************


END MODULE module_domain


! The following routines are outside this module to avoid build dependences.  


! Get current time as a string (current time from clock attached to the 
! current_grid).  Includes fractional part, if present.  
! Returns empty string if current_grid is not set or if timing has not yet 
! been set up on current_grid.  
SUBROUTINE get_current_time_string( time_str )
  USE module_domain
  IMPLICIT NONE
  CHARACTER (LEN=*), INTENT(OUT) :: time_str
  ! locals
  INTEGER :: debug_level_lcl
!PRINT *,DEBUG:  begin get_current_time_string()
  time_str = ''
  IF ( current_grid_set ) THEN
!$$$DEBUG
!PRINT *,DEBUG:  get_current_time_string():  checking association of current_grid...
!IF ( ASSOCIATED( current_grid ) ) THEN
!  PRINT *,DEBUG:  get_current_time_string():  current_grid is associated
!ELSE
!  PRINT *,DEBUG:  get_current_time_string():  current_grid is NOT associated
!ENDIF
!$$$END DEBUG
    IF ( current_grid%time_set ) THEN
!PRINT *,DEBUG:  get_current_time_string():  calling domain_clock_get()
      ! set debug_level to zero and clear current_grid_set to avoid recursion
      CALL get_wrf_debug_level( debug_level_lcl )
      CALL set_wrf_debug_level ( 0 )
      current_grid_set = .FALSE.
      CALL domain_clock_get( current_grid, current_timestr_frac=time_str )
      ! restore debug_level and current_grid_set
      CALL set_wrf_debug_level ( debug_level_lcl )
      current_grid_set = .TRUE.
!PRINT *,DEBUG:  get_current_time_string():  back from domain_clock_get()
    ENDIF
  ENDIF
!PRINT *,DEBUG:  end get_current_time_string()
END SUBROUTINE get_current_time_string


! Get current domain name as a string of form "d<NN>" where "<NN>" is 
! grid%id printed in two characters, with leading zero if needed ("d01", 
! "d02", etc.).  
! Return empty string if current_grid not set.  
SUBROUTINE get_current_grid_name( grid_str )
  USE module_domain
  IMPLICIT NONE
  CHARACTER (LEN=*), INTENT(OUT) :: grid_str
  grid_str = ''
  IF ( current_grid_set ) THEN
    WRITE(grid_str,FMT="('d',I2.2)") current_grid%id
  ENDIF
END SUBROUTINE get_current_grid_name



