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
TYPE fdob_type
integer                                  :: domain_tot
integer                                  :: domain_init
integer                                  :: ieodi
integer                                  :: iwtsig
integer                                  :: nstat
integer                                  :: ktaur
integer                                  :: sn_maxcg
integer                                  :: we_maxcg
integer                                  :: sn_end
integer                                  :: levidn(max_domains)
real                                     :: ds_cg
real                                     :: window
real                                     :: rtlast
real                                     :: datend
real                                     :: rinfmn
real                                     :: rinfmx
real                                     :: pfree
real                                     :: dcon
real                                     :: dpsmx
real                                     :: tfaci
real                                     :: xn
real      ,DIMENSION(:,:)     ,POINTER   :: varobs
real      ,DIMENSION(:,:)     ,POINTER   :: errf
real      ,DIMENSION(:)       ,POINTER   :: timeob
real      ,DIMENSION(:)       ,POINTER   :: nlevs_ob
real      ,DIMENSION(:)       ,POINTER   :: lev_in_ob
real      ,DIMENSION(:)       ,POINTER   :: plfo
real      ,DIMENSION(:)       ,POINTER   :: elevob
real      ,DIMENSION(:)       ,POINTER   :: rio
real      ,DIMENSION(:)       ,POINTER   :: rjo
real      ,DIMENSION(:)       ,POINTER   :: rko
END TYPE fdob_type
!ENDOFREGISTRYGENERATEDINCLUDE

   TYPE domain

! SEE THE INCLUDE FILE FOR DEFINITIONS OF STATE FIELDS WITHIN THE DOMAIN DATA STRUCTURE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/state_struct.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
real                                     :: cfn
real                                     :: cfn1
integer                                  :: step_number
real                                     :: rdx
real                                     :: rdy
real                                     :: dts
real                                     :: dtseps
real                                     :: resm
real                                     :: zetatop
real                                     :: cf1
real                                     :: cf2
real                                     :: cf3
integer                                  :: number_at_same_level
integer                                  :: itimestep
real                                     :: xtime
real                                     :: julian
integer                                  :: lbc_fid
logical                                  :: tiled
logical                                  :: patched
real                                     :: xi
real                                     :: xj
real                                     :: vc_i
real                                     :: vc_j
real                                     :: dtbc
integer                                  :: ifndsnowh
integer                                  :: ifndsoilw
real                                     :: declin_urb
real                                     :: u_frame
real                                     :: v_frame
real                                     :: p_top
real                                     :: em_lat_ll_t
real                                     :: em_lat_ul_t
real                                     :: em_lat_ur_t
real                                     :: em_lat_lr_t
real                                     :: em_lat_ll_u
real                                     :: em_lat_ul_u
real                                     :: em_lat_ur_u
real                                     :: em_lat_lr_u
real                                     :: em_lat_ll_v
real                                     :: em_lat_ul_v
real                                     :: em_lat_ur_v
real                                     :: em_lat_lr_v
real                                     :: em_lat_ll_d
real                                     :: em_lat_ul_d
real                                     :: em_lat_ur_d
real                                     :: em_lat_lr_d
real                                     :: em_lon_ll_t
real                                     :: em_lon_ul_t
real                                     :: em_lon_ur_t
real                                     :: em_lon_lr_t
real                                     :: em_lon_ll_u
real                                     :: em_lon_ul_u
real                                     :: em_lon_ur_u
real                                     :: em_lon_lr_u
real                                     :: em_lon_ll_v
real                                     :: em_lon_ul_v
real                                     :: em_lon_ur_v
real                                     :: em_lon_lr_v
real                                     :: em_lon_ll_d
real                                     :: em_lon_ul_d
real                                     :: em_lon_ur_d
real                                     :: em_lon_lr_d
integer                                  :: stepcu
integer                                  :: stepra
integer                                  :: landuse_isice
integer                                  :: landuse_lucats
integer                                  :: landuse_luseas
integer                                  :: landuse_isn
integer                                  :: stepbl
logical                                  :: warm_rain
logical                                  :: adv_moist_cond
integer                                  :: stepfg
logical                                  :: moved
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
logical                                  :: input_from_hires
character*256                               :: rsmas_data_path
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
character*256                               :: auxinput1_inname
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
integer                                  :: diag_print
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
integer                                  :: num_metgrid_levels
real                                     :: p_top_requested
integer                                  :: interp_type
logical                                  :: lowest_lev_from_sfc
integer                                  :: lagrange_order
integer                                  :: force_sfc_in_vinterp
real                                     :: zap_close_levels
logical                                  :: sfcp_to_sfcp
logical                                  :: adjust_heights
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
integer                                  :: blend_width
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
integer                                  :: vortex_interval
integer                                  :: max_vortex_speed
integer                                  :: corral_dist
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
real                                     :: max_dz
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
integer                                  :: num_months
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
integer                                  :: co2tf
integer                                  :: ra_call_offset
real                                     :: cam_abs_freq_s
integer                                  :: levsiz
integer                                  :: paerlev
integer                                  :: cam_abs_dim1
integer                                  :: cam_abs_dim2
logical                                  :: cu_rad_feedback
real                                     :: fgdt
integer                                  :: grid_fdda
integer                                  :: if_no_pbl_nudging_uv
integer                                  :: if_no_pbl_nudging_t
integer                                  :: if_no_pbl_nudging_q
integer                                  :: if_zfac_uv
integer                                  :: k_zfac_uv
integer                                  :: if_zfac_t
integer                                  :: k_zfac_t
integer                                  :: if_zfac_q
integer                                  :: k_zfac_q
real                                     :: guv
real                                     :: gt
real                                     :: gq
real                                     :: dtramp_min
integer                                  :: if_ramping
integer                                  :: obs_nudge_opt
integer                                  :: max_obs
integer                                  :: nobs_ndg_vars
integer                                  :: nobs_err_flds
real                                     :: fdda_start
real                                     :: fdda_end
integer                                  :: obs_nudge_wind
real                                     :: obs_coef_wind
integer                                  :: obs_nudge_temp
real                                     :: obs_coef_temp
integer                                  :: obs_nudge_mois
real                                     :: obs_coef_mois
integer                                  :: obs_nudge_pstr
real                                     :: obs_coef_pstr
real                                     :: obs_rinxy
real                                     :: obs_rinsig
real                                     :: obs_twindo
integer                                  :: obs_npfi
integer                                  :: obs_ionf
integer                                  :: obs_idynin
real                                     :: obs_dtramp
logical                                  :: obs_ipf_in4dob
logical                                  :: obs_ipf_errob
logical                                  :: obs_ipf_nudob
integer                                  :: dyn_opt
integer                                  :: rk_ord
integer                                  :: w_damping
integer                                  :: diff_opt
integer                                  :: km_opt
integer                                  :: damp_opt
real                                     :: zdamp
real                                     :: dampcoef
real                                     :: khdif
real                                     :: kvdif
real                                     :: diff_6th_factor
integer                                  :: diff_6th_opt
real                                     :: smdiv
real                                     :: emdiv
real                                     :: epssm
logical                                  :: non_hydrostatic
integer                                  :: time_step_sound
integer                                  :: h_mom_adv_order
integer                                  :: v_mom_adv_order
integer                                  :: h_sca_adv_order
integer                                  :: v_sca_adv_order
logical                                  :: pd_moist
logical                                  :: pd_chem
logical                                  :: pd_scalar
logical                                  :: pd_tke
logical                                  :: top_radiation
real                                     :: mix_cr_len
real                                     :: tke_upper_bound
real                                     :: kh_tke_upper_bound
real                                     :: kv_tke_upper_bound
real                                     :: tke_drag_coefficient
real                                     :: tke_heat_flux
logical                                  :: pert_coriolis
logical                                  :: mix_full_fields
real                                     :: base_pres
real                                     :: base_temp
real                                     :: base_lapse
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
TYPE(fdob_type)                               :: fdob
real      ,DIMENSION(:,:)     ,POINTER   :: lu_index
real      ,DIMENSION(:,:)     ,POINTER   :: lu_mask
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_u_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_v_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_t_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_rh_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_ght_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_p_gc
real      ,DIMENSION(:,:)     ,POINTER   :: em_xlat_gc
real      ,DIMENSION(:,:)     ,POINTER   :: em_xlong_gc
real      ,DIMENSION(:,:)     ,POINTER   :: em_ht_gc
real      ,DIMENSION(:,:)     ,POINTER   :: em_tsk_gc
real      ,DIMENSION(:,:)     ,POINTER   :: em_tavgsfc
real      ,DIMENSION(:,:)     ,POINTER   :: em_tmn_gc
real      ,DIMENSION(:,:)     ,POINTER   :: em_pslv_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_greenfrac
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_albedo12m
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_pd_gc
real      ,DIMENSION(:,:)     ,POINTER   :: em_psfc_gc
real      ,DIMENSION(:,:)     ,POINTER   :: em_intq_gc
real      ,DIMENSION(:,:)     ,POINTER   :: em_pdhs
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_qv_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_u_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_u_2
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_u_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_u_bt
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_ru
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_ru_m
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_ru_tend
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_u_save
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_v_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_v_2
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_v_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_v_bt
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_rv
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_rv_m
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_rv_tend
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_v_save
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_w_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_w_2
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_w_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_w_bt
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_ww
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_rw
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_ww_m
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_ph_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_ph_2
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_ph_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_ph_bt
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_phb
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_phb_fine
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_ph0
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_php
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_t_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_t_2
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_t_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_t_bt
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_t_init
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_tp_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_tp_2
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_t_save
real      ,DIMENSION(:,:)     ,POINTER   :: em_mu_1
real      ,DIMENSION(:,:)     ,POINTER   :: em_mu_2
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_mu_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_mu_bt
real      ,DIMENSION(:,:)     ,POINTER   :: em_mub
real      ,DIMENSION(:,:)     ,POINTER   :: em_mub_fine
real      ,DIMENSION(:,:)     ,POINTER   :: em_mu0
real      ,DIMENSION(:,:)     ,POINTER   :: em_mudf
real      ,DIMENSION(:,:)     ,POINTER   :: em_muu
real      ,DIMENSION(:,:)     ,POINTER   :: em_muv
real      ,DIMENSION(:,:)     ,POINTER   :: em_mut
real      ,DIMENSION(:,:)     ,POINTER   :: em_muts
real      ,DIMENSION(:,:)     ,POINTER   :: nest_pos
real      ,DIMENSION(:,:)     ,POINTER   :: nest_mask
real      ,DIMENSION(:,:)     ,POINTER   :: ht_coarse
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_tke_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_tke_2
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_p
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_al
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_alt
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_alb
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_zx
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_zy
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_rdz
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_rdzw
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_pb
real      ,DIMENSION(:,:)     ,POINTER   :: em_sr
real      ,DIMENSION(:,:)     ,POINTER   :: em_potevp
real      ,DIMENSION(:,:)     ,POINTER   :: em_snopcx
real      ,DIMENSION(:,:)     ,POINTER   :: em_soiltb
real      ,DIMENSION(:)       ,POINTER   :: em_fnm
real      ,DIMENSION(:)       ,POINTER   :: em_fnp
real      ,DIMENSION(:)       ,POINTER   :: em_rdnw
real      ,DIMENSION(:)       ,POINTER   :: em_rdn
real      ,DIMENSION(:)       ,POINTER   :: em_dnw
real      ,DIMENSION(:)       ,POINTER   :: em_dn
real      ,DIMENSION(:)       ,POINTER   :: em_znu
real      ,DIMENSION(:)       ,POINTER   :: em_znw
real      ,DIMENSION(:)       ,POINTER   :: em_t_base
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_z
real      ,DIMENSION(:,:)     ,POINTER   :: q2
real      ,DIMENSION(:,:)     ,POINTER   :: t2
real      ,DIMENSION(:,:)     ,POINTER   :: th2
real      ,DIMENSION(:,:)     ,POINTER   :: psfc
real      ,DIMENSION(:,:)     ,POINTER   :: u10
real      ,DIMENSION(:,:)     ,POINTER   :: v10
real      ,DIMENSION(:,:)     ,POINTER   :: uratx
real      ,DIMENSION(:,:)     ,POINTER   :: vratx
real      ,DIMENSION(:,:)     ,POINTER   :: tratx
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_obs_savwt
integer   ,DIMENSION(:,:)     ,POINTER   :: imask_nostag
integer   ,DIMENSION(:,:)     ,POINTER   :: imask_xstag
integer   ,DIMENSION(:,:)     ,POINTER   :: imask_ystag
integer   ,DIMENSION(:,:)     ,POINTER   :: imask_xystag
real      ,DIMENSION(:,:,:,:) ,POINTER   :: moist
real      ,DIMENSION(:,:,:,:,:),POINTER   :: moist_b
real      ,DIMENSION(:,:,:,:,:),POINTER   :: moist_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: chem
real      ,DIMENSION(:,:,:,:) ,POINTER   :: scalar
real      ,DIMENSION(:,:,:,:,:),POINTER   :: scalar_b
real      ,DIMENSION(:,:,:,:,:),POINTER   :: scalar_bt
real      ,DIMENSION(:)       ,POINTER   :: fcx
real      ,DIMENSION(:)       ,POINTER   :: gcx
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
real      ,DIMENSION(:,:)     ,POINTER   :: shdmax
real      ,DIMENSION(:,:)     ,POINTER   :: shdmin
real      ,DIMENSION(:,:)     ,POINTER   :: snoalb
real      ,DIMENSION(:,:)     ,POINTER   :: slopecat
real      ,DIMENSION(:,:)     ,POINTER   :: toposoil
real      ,DIMENSION(:,:,:)   ,POINTER   :: landusef
real      ,DIMENSION(:,:,:)   ,POINTER   :: soilctop
real      ,DIMENSION(:,:,:)   ,POINTER   :: soilcbot
real      ,DIMENSION(:,:)     ,POINTER   :: soilcat
real      ,DIMENSION(:,:)     ,POINTER   :: vegcat
real      ,DIMENSION(:,:,:)   ,POINTER   :: tslb
real      ,DIMENSION(:)       ,POINTER   :: zs
real      ,DIMENSION(:)       ,POINTER   :: dzs
real      ,DIMENSION(:)       ,POINTER   :: dzr
real      ,DIMENSION(:)       ,POINTER   :: dzb
real      ,DIMENSION(:)       ,POINTER   :: dzg
real      ,DIMENSION(:,:,:)   ,POINTER   :: smois
real      ,DIMENSION(:,:,:)   ,POINTER   :: sh2o
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
real      ,DIMENSION(:,:)     ,POINTER   :: sfcexc
real      ,DIMENSION(:,:)     ,POINTER   :: acsnow
real      ,DIMENSION(:,:)     ,POINTER   :: acsnom
real      ,DIMENSION(:,:)     ,POINTER   :: snow
real      ,DIMENSION(:,:)     ,POINTER   :: snowh
real      ,DIMENSION(:,:)     ,POINTER   :: rhosn
real      ,DIMENSION(:,:)     ,POINTER   :: canwat
real      ,DIMENSION(:,:)     ,POINTER   :: sst
real      ,DIMENSION(:,:)     ,POINTER   :: tr_urb2d
real      ,DIMENSION(:,:)     ,POINTER   :: tb_urb2d
real      ,DIMENSION(:,:)     ,POINTER   :: tg_urb2d
real      ,DIMENSION(:,:)     ,POINTER   :: tc_urb2d
real      ,DIMENSION(:,:)     ,POINTER   :: qc_urb2d
real      ,DIMENSION(:,:)     ,POINTER   :: uc_urb2d
real      ,DIMENSION(:,:)     ,POINTER   :: xxxr_urb2d
real      ,DIMENSION(:,:)     ,POINTER   :: xxxb_urb2d
real      ,DIMENSION(:,:)     ,POINTER   :: xxxg_urb2d
real      ,DIMENSION(:,:)     ,POINTER   :: xxxc_urb2d
real      ,DIMENSION(:,:,:)   ,POINTER   :: trl_urb3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: tbl_urb3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: tgl_urb3d
real      ,DIMENSION(:,:)     ,POINTER   :: sh_urb2d
real      ,DIMENSION(:,:)     ,POINTER   :: lh_urb2d
real      ,DIMENSION(:,:)     ,POINTER   :: g_urb2d
real      ,DIMENSION(:,:)     ,POINTER   :: rn_urb2d
real      ,DIMENSION(:,:)     ,POINTER   :: ts_urb2d
real      ,DIMENSION(:,:)     ,POINTER   :: frc_urb2d
integer   ,DIMENSION(:,:)     ,POINTER   :: utype_urb2d
real      ,DIMENSION(:,:)     ,POINTER   :: cosz_urb2d
real      ,DIMENSION(:,:)     ,POINTER   :: omg_urb2d
real      ,DIMENSION(:,:,:)   ,POINTER   :: smfr3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: keepfr3dflag
real      ,DIMENSION(:,:,:)   ,POINTER   :: tke_myj
real      ,DIMENSION(:,:,:)   ,POINTER   :: el_myj
real      ,DIMENSION(:,:,:)   ,POINTER   :: exch_h
real      ,DIMENSION(:,:)     ,POINTER   :: ct
real      ,DIMENSION(:,:)     ,POINTER   :: thz0
real      ,DIMENSION(:,:)     ,POINTER   :: z0
real      ,DIMENSION(:,:)     ,POINTER   :: qz0
real      ,DIMENSION(:,:)     ,POINTER   :: uz0
real      ,DIMENSION(:,:)     ,POINTER   :: vz0
real      ,DIMENSION(:,:)     ,POINTER   :: qsfc
real      ,DIMENSION(:,:)     ,POINTER   :: akhs
real      ,DIMENSION(:,:)     ,POINTER   :: akms
integer   ,DIMENSION(:,:)     ,POINTER   :: kpbl
real      ,DIMENSION(:,:)     ,POINTER   :: htop
real      ,DIMENSION(:,:)     ,POINTER   :: hbot
real      ,DIMENSION(:,:)     ,POINTER   :: htopr
real      ,DIMENSION(:,:)     ,POINTER   :: hbotr
real      ,DIMENSION(:,:)     ,POINTER   :: cutop
real      ,DIMENSION(:,:)     ,POINTER   :: cubot
real      ,DIMENSION(:,:)     ,POINTER   :: cuppt
real      ,DIMENSION(:,:)     ,POINTER   :: rswtoa
real      ,DIMENSION(:,:)     ,POINTER   :: rlwtoa
real      ,DIMENSION(:,:)     ,POINTER   :: czmean
real      ,DIMENSION(:,:)     ,POINTER   :: cfracl
real      ,DIMENSION(:,:)     ,POINTER   :: cfracm
real      ,DIMENSION(:,:)     ,POINTER   :: cfrach
real      ,DIMENSION(:,:)     ,POINTER   :: acfrst
integer   ,DIMENSION(:,:)     ,POINTER   :: ncfrst
real      ,DIMENSION(:,:)     ,POINTER   :: acfrcv
integer   ,DIMENSION(:,:)     ,POINTER   :: ncfrcv
real      ,DIMENSION(:,:,:,:) ,POINTER   :: ozmixm
real      ,DIMENSION(:)       ,POINTER   :: pin
real      ,DIMENSION(:,:)     ,POINTER   :: m_ps_1
real      ,DIMENSION(:,:)     ,POINTER   :: m_ps_2
real      ,DIMENSION(:,:,:,:) ,POINTER   :: aerosolc_1
real      ,DIMENSION(:,:,:,:) ,POINTER   :: aerosolc_2
real      ,DIMENSION(:)       ,POINTER   :: m_hybi
real      ,DIMENSION(:,:,:)   ,POINTER   :: f_ice_phy
real      ,DIMENSION(:,:,:)   ,POINTER   :: f_rain_phy
real      ,DIMENSION(:,:,:)   ,POINTER   :: f_rimef_phy
real      ,DIMENSION(:,:,:)   ,POINTER   :: h_diabatic
real      ,DIMENSION(:,:)     ,POINTER   :: msft
real      ,DIMENSION(:,:)     ,POINTER   :: msfu
real      ,DIMENSION(:,:)     ,POINTER   :: msfv
real      ,DIMENSION(:,:)     ,POINTER   :: f
real      ,DIMENSION(:,:)     ,POINTER   :: e
real      ,DIMENSION(:,:)     ,POINTER   :: sina
real      ,DIMENSION(:,:)     ,POINTER   :: cosa
real      ,DIMENSION(:,:)     ,POINTER   :: ht
real      ,DIMENSION(:,:)     ,POINTER   :: ht_fine
real      ,DIMENSION(:,:)     ,POINTER   :: ht_int
real      ,DIMENSION(:,:)     ,POINTER   :: ht_input
real      ,DIMENSION(:,:)     ,POINTER   :: tsk
real      ,DIMENSION(:,:)     ,POINTER   :: tsk_save
real      ,DIMENSION(:)       ,POINTER   :: u_base
real      ,DIMENSION(:)       ,POINTER   :: v_base
real      ,DIMENSION(:)       ,POINTER   :: qv_base
real      ,DIMENSION(:)       ,POINTER   :: z_base
real      ,DIMENSION(:,:,:)   ,POINTER   :: rthcuten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rqvcuten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rqrcuten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rqccuten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rqscuten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rqicuten
real      ,DIMENSION(:,:,:)   ,POINTER   :: w0avg
real      ,DIMENSION(:,:)     ,POINTER   :: rainc
real      ,DIMENSION(:,:)     ,POINTER   :: rainnc
real      ,DIMENSION(:,:)     ,POINTER   :: raincv
real      ,DIMENSION(:,:)     ,POINTER   :: rainncv
real      ,DIMENSION(:,:)     ,POINTER   :: rainbl
real      ,DIMENSION(:,:)     ,POINTER   :: snownc
real      ,DIMENSION(:,:)     ,POINTER   :: graupelnc
real      ,DIMENSION(:,:)     ,POINTER   :: snowncv
real      ,DIMENSION(:,:)     ,POINTER   :: graupelncv
real      ,DIMENSION(:,:)     ,POINTER   :: nca
integer   ,DIMENSION(:,:)     ,POINTER   :: lowlyr
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
real      ,DIMENSION(:,:,:)   ,POINTER   :: rthraten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rthratenlw
real      ,DIMENSION(:,:,:)   ,POINTER   :: rthratensw
real      ,DIMENSION(:,:,:)   ,POINTER   :: cldfra
real      ,DIMENSION(:,:)     ,POINTER   :: swdown
real      ,DIMENSION(:,:)     ,POINTER   :: swdownc
real      ,DIMENSION(:,:)     ,POINTER   :: gsw
real      ,DIMENSION(:,:)     ,POINTER   :: glw
real      ,DIMENSION(:,:)     ,POINTER   :: swcf
real      ,DIMENSION(:,:)     ,POINTER   :: lwcf
real      ,DIMENSION(:,:)     ,POINTER   :: olr
real      ,DIMENSION(:,:)     ,POINTER   :: xlat
real      ,DIMENSION(:,:)     ,POINTER   :: xlong
real      ,DIMENSION(:,:)     ,POINTER   :: em_xlat_u
real      ,DIMENSION(:,:)     ,POINTER   :: em_xlong_u
real      ,DIMENSION(:,:)     ,POINTER   :: em_xlat_v
real      ,DIMENSION(:,:)     ,POINTER   :: em_xlong_v
real      ,DIMENSION(:,:)     ,POINTER   :: albedo
real      ,DIMENSION(:,:)     ,POINTER   :: albbck
real      ,DIMENSION(:,:)     ,POINTER   :: emiss
real      ,DIMENSION(:,:)     ,POINTER   :: cldefi
real      ,DIMENSION(:,:,:)   ,POINTER   :: rublten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rvblten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rthblten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rqvblten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rqcblten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rqiblten
real      ,DIMENSION(:)       ,POINTER   :: mp_restart_state
real      ,DIMENSION(:)       ,POINTER   :: tbpvs_state
real      ,DIMENSION(:)       ,POINTER   :: tbpvs0_state
real      ,DIMENSION(:)       ,POINTER   :: lu_state
real      ,DIMENSION(:,:)     ,POINTER   :: tmn
real      ,DIMENSION(:,:)     ,POINTER   :: xland
real      ,DIMENSION(:,:)     ,POINTER   :: znt
real      ,DIMENSION(:,:)     ,POINTER   :: ust
real      ,DIMENSION(:,:)     ,POINTER   :: rmol
real      ,DIMENSION(:,:)     ,POINTER   :: mol
real      ,DIMENSION(:,:)     ,POINTER   :: pblh
real      ,DIMENSION(:,:)     ,POINTER   :: capg
real      ,DIMENSION(:,:)     ,POINTER   :: thc
real      ,DIMENSION(:,:)     ,POINTER   :: hfx
real      ,DIMENSION(:,:)     ,POINTER   :: qfx
real      ,DIMENSION(:,:)     ,POINTER   :: lh
real      ,DIMENSION(:,:)     ,POINTER   :: flhc
real      ,DIMENSION(:,:)     ,POINTER   :: flqc
real      ,DIMENSION(:,:)     ,POINTER   :: qsg
real      ,DIMENSION(:,:)     ,POINTER   :: qvg
real      ,DIMENSION(:,:)     ,POINTER   :: qcg
real      ,DIMENSION(:,:)     ,POINTER   :: soilt1
real      ,DIMENSION(:,:)     ,POINTER   :: tsnav
real      ,DIMENSION(:,:)     ,POINTER   :: snowc
real      ,DIMENSION(:,:)     ,POINTER   :: mavail
real      ,DIMENSION(:,:)     ,POINTER   :: tkesfcf
real      ,DIMENSION(:,:,:)   ,POINTER   :: taucldi
real      ,DIMENSION(:,:,:)   ,POINTER   :: taucldc
real      ,DIMENSION(:,:,:)   ,POINTER   :: defor11
real      ,DIMENSION(:,:,:)   ,POINTER   :: defor22
real      ,DIMENSION(:,:,:)   ,POINTER   :: defor12
real      ,DIMENSION(:,:,:)   ,POINTER   :: defor33
real      ,DIMENSION(:,:,:)   ,POINTER   :: defor13
real      ,DIMENSION(:,:,:)   ,POINTER   :: defor23
real      ,DIMENSION(:,:,:)   ,POINTER   :: xkmv
real      ,DIMENSION(:,:,:)   ,POINTER   :: xkmh
real      ,DIMENSION(:,:,:)   ,POINTER   :: xkmhd
real      ,DIMENSION(:,:,:)   ,POINTER   :: xkhv
real      ,DIMENSION(:,:,:)   ,POINTER   :: xkhh
real      ,DIMENSION(:,:,:)   ,POINTER   :: div
real      ,DIMENSION(:,:,:)   ,POINTER   :: bn2
real      ,DIMENSION(:,:,:)   ,POINTER   :: rundgdten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rvndgdten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rthndgdten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rqvndgdten
real      ,DIMENSION(:,:)     ,POINTER   :: rmundgdten
real      ,DIMENSION(:,:,:,:) ,POINTER   :: fdda3d
real      ,DIMENSION(:,:,:,:) ,POINTER   :: fdda2d
real      ,DIMENSION(:,:,:,:) ,POINTER   :: abstot
real      ,DIMENSION(:,:,:,:) ,POINTER   :: absnxt
real      ,DIMENSION(:,:,:)   ,POINTER   :: emstot
real      ,DIMENSION(:,:)     ,POINTER   :: dpsdt
real      ,DIMENSION(:,:)     ,POINTER   :: dmudt
real      ,DIMENSION(:,:)     ,POINTER   :: pk1m
real      ,DIMENSION(:,:)     ,POINTER   :: mu_2m
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

CONTAINS

   SUBROUTINE adjust_domain_dims_for_move( grid , dx, dy )
    IMPLICIT NONE

    TYPE( domain ), POINTER   :: grid
    INTEGER, INTENT(IN) ::  dx, dy

    data_ordering : SELECT CASE ( model_data_order )
       CASE  ( DATA_ORDER_XYZ )
            grid%sm31  =             grid%sm31 + dx
            grid%em31  =             grid%em31 + dx
            grid%sm32  =             grid%sm32 + dy
            grid%em32  =             grid%em32 + dy
            grid%sp31  =             grid%sp31 + dx
            grid%ep31  =             grid%ep31 + dx
            grid%sp32  =             grid%sp32 + dy
            grid%ep32  =             grid%ep32 + dy
            grid%sd31  =             grid%sd31 + dx
            grid%ed31  =             grid%ed31 + dx
            grid%sd32  =             grid%sd32 + dy
            grid%ed32  =             grid%ed32 + dy

       CASE  ( DATA_ORDER_YXZ )
            grid%sm31  =             grid%sm31 + dy
            grid%em31  =             grid%em31 + dy
            grid%sm32  =             grid%sm32 + dx
            grid%em32  =             grid%em32 + dx
            grid%sp31  =             grid%sp31 + dy
            grid%ep31  =             grid%ep31 + dy
            grid%sp32  =             grid%sp32 + dx
            grid%ep32  =             grid%ep32 + dx
            grid%sd31  =             grid%sd31 + dy
            grid%ed31  =             grid%ed31 + dy
            grid%sd32  =             grid%sd32 + dx
            grid%ed32  =             grid%ed32 + dx

       CASE  ( DATA_ORDER_ZXY )
            grid%sm32  =             grid%sm32 + dx
            grid%em32  =             grid%em32 + dx
            grid%sm33  =             grid%sm33 + dy
            grid%em33  =             grid%em33 + dy
            grid%sp32  =             grid%sp32 + dx
            grid%ep32  =             grid%ep32 + dx
            grid%sp33  =             grid%sp33 + dy
            grid%ep33  =             grid%ep33 + dy
            grid%sd32  =             grid%sd32 + dx
            grid%ed32  =             grid%ed32 + dx
            grid%sd33  =             grid%sd33 + dy
            grid%ed33  =             grid%ed33 + dy

       CASE  ( DATA_ORDER_ZYX )
            grid%sm32  =             grid%sm32 + dy
            grid%em32  =             grid%em32 + dy
            grid%sm33  =             grid%sm33 + dx
            grid%em33  =             grid%em33 + dx
            grid%sp32  =             grid%sp32 + dy
            grid%ep32  =             grid%ep32 + dy
            grid%sp33  =             grid%sp33 + dx
            grid%ep33  =             grid%ep33 + dx
            grid%sd32  =             grid%sd32 + dy
            grid%ed32  =             grid%ed32 + dy
            grid%sd33  =             grid%sd33 + dx
            grid%ed33  =             grid%ed33 + dx

       CASE  ( DATA_ORDER_XZY )
            grid%sm31  =             grid%sm31 + dx
            grid%em31  =             grid%em31 + dx
            grid%sm33  =             grid%sm33 + dy
            grid%em33  =             grid%em33 + dy
            grid%sp31  =             grid%sp31 + dx
            grid%ep31  =             grid%ep31 + dx
            grid%sp33  =             grid%sp33 + dy
            grid%ep33  =             grid%ep33 + dy
            grid%sd31  =             grid%sd31 + dx
            grid%ed31  =             grid%ed31 + dx
            grid%sd33  =             grid%sd33 + dy
            grid%ed33  =             grid%ed33 + dy

       CASE  ( DATA_ORDER_YZX )
            grid%sm31  =             grid%sm31 + dy
            grid%em31  =             grid%em31 + dy
            grid%sm33  =             grid%sm33 + dx
            grid%em33  =             grid%em33 + dx
            grid%sp31  =             grid%sp31 + dy
            grid%ep31  =             grid%ep31 + dy
            grid%sp33  =             grid%sp33 + dx
            grid%ep33  =             grid%ep33 + dx
            grid%sd31  =             grid%sd31 + dy
            grid%ed31  =             grid%ed31 + dy
            grid%sd33  =             grid%sd33 + dx
            grid%ed33  =             grid%ed33 + dx

    END SELECT data_ordering


    RETURN

   END SUBROUTINE adjust_domain_dims_for_move


   SUBROUTINE get_ijk_from_grid (  grid ,                   &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe    )

    IMPLICIT NONE

    TYPE( domain ), INTENT (IN)  :: grid
    INTEGER, INTENT(OUT) ::                                 &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe

    data_ordering : SELECT CASE ( model_data_order )
       CASE  ( DATA_ORDER_XYZ )
           ids             = grid%sd31 
           ide             = grid%ed31 
           jds             = grid%sd32 
           jde             = grid%ed32 
           kds             = grid%sd33 
           kde             = grid%ed33 
           ims             = grid%sm31 
           ime             = grid%em31 
           jms             = grid%sm32 
           jme             = grid%em32 
           kms             = grid%sm33 
           kme             = grid%em33 
           ips             = grid%sp31 
           ipe             = grid%ep31 
           jps             = grid%sp32 
           jpe             = grid%ep32 
           kps             = grid%sp33 
           kpe             = grid%ep33 

       CASE  ( DATA_ORDER_YXZ )
           ids             = grid%sd32 
           ide             = grid%ed32 
           jds             = grid%sd31 
           jde             = grid%ed31 
           kds             = grid%sd33 
           kde             = grid%ed33 
           ims             = grid%sm32 
           ime             = grid%em32 
           jms             = grid%sm31 
           jme             = grid%em31 
           kms             = grid%sm33 
           kme             = grid%em33 
           ips             = grid%sp32 
           ipe             = grid%ep32 
           jps             = grid%sp31 
           jpe             = grid%ep31 
           kps             = grid%sp33 
           kpe             = grid%ep33 

       CASE  ( DATA_ORDER_ZXY )
           ids             = grid%sd32 
           ide             = grid%ed32 
           jds             = grid%sd33 
           jde             = grid%ed33 
           kds             = grid%sd31 
           kde             = grid%ed31 
           ims             = grid%sm32 
           ime             = grid%em32 
           jms             = grid%sm33 
           jme             = grid%em33 
           kms             = grid%sm31 
           kme             = grid%em31 
           ips             = grid%sp32 
           ipe             = grid%ep32 
           jps             = grid%sp33 
           jpe             = grid%ep33 
           kps             = grid%sp31 
           kpe             = grid%ep31 

       CASE  ( DATA_ORDER_ZYX )
           ids             = grid%sd33 
           ide             = grid%ed33 
           jds             = grid%sd32 
           jde             = grid%ed32 
           kds             = grid%sd31 
           kde             = grid%ed31 
           ims             = grid%sm33 
           ime             = grid%em33 
           jms             = grid%sm32 
           jme             = grid%em32 
           kms             = grid%sm31 
           kme             = grid%em31 
           ips             = grid%sp33 
           ipe             = grid%ep33 
           jps             = grid%sp32 
           jpe             = grid%ep32 
           kps             = grid%sp31 
           kpe             = grid%ep31 

       CASE  ( DATA_ORDER_XZY )
           ids             = grid%sd31 
           ide             = grid%ed31 
           jds             = grid%sd33 
           jde             = grid%ed33 
           kds             = grid%sd32 
           kde             = grid%ed32 
           ims             = grid%sm31 
           ime             = grid%em31 
           jms             = grid%sm33 
           jme             = grid%em33 
           kms             = grid%sm32 
           kme             = grid%em32 
           ips             = grid%sp31 
           ipe             = grid%ep31 
           jps             = grid%sp33 
           jpe             = grid%ep33 
           kps             = grid%sp32 
           kpe             = grid%ep32 

       CASE  ( DATA_ORDER_YZX )
           ids             = grid%sd33 
           ide             = grid%ed33 
           jds             = grid%sd31 
           jde             = grid%ed31 
           kds             = grid%sd32 
           kde             = grid%ed32 
           ims             = grid%sm33 
           ime             = grid%em33 
           jms             = grid%sm31 
           jme             = grid%em31 
           kms             = grid%sm32 
           kme             = grid%em32 
           ips             = grid%sp33 
           ipe             = grid%ep33 
           jps             = grid%sp31 
           jpe             = grid%ep31 
           kps             = grid%sp32 
           kpe             = grid%ep32 

    END SELECT data_ordering
   END SUBROUTINE get_ijk_from_grid

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
        CALL wrf_error_fatal3 ( "module_domain.b" , 778 ,  TRIM ( wrf_err_message ) )
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
        CALL wrf_error_fatal3 ( "module_domain.b" , 1110 ,  message )

      ELSE IF ( dyn_opt == DYN_EM ) THEN
        IF ( grid%id .EQ. 1 ) CALL wrf_message ( &
          'DYNAMICS OPTION: Eulerian Mass Coordinate ')
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/em_allocs.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
ALLOCATE(grid%lu_index(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%lu_index(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%lu_index=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%lu_mask(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%lu_mask(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%em_u_gc(sm31:em31,1:model_config_rec%num_metgrid_levels,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_u_gc(sm31:em31,1:model_config_rec%num_metgrid_levels,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_u_gc=initial_data_value
ELSE
ALLOCATE(grid%em_u_gc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_u_gc(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_v_gc(sm31:em31,1:model_config_rec%num_metgrid_levels,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_v_gc(sm31:em31,1:model_config_rec%num_metgrid_levels,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_v_gc=initial_data_value
ELSE
ALLOCATE(grid%em_v_gc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_v_gc(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_t_gc(sm31:em31,1:model_config_rec%num_metgrid_levels,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_t_gc(sm31:em31,1:model_config_rec%num_metgrid_levels,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_t_gc=initial_data_value
ELSE
ALLOCATE(grid%em_t_gc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_t_gc(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_rh_gc(sm31:em31,1:model_config_rec%num_metgrid_levels,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_rh_gc(sm31:em31,1:model_config_rec%num_metgrid_levels,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_rh_gc=initial_data_value
ELSE
ALLOCATE(grid%em_rh_gc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_rh_gc(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_ght_gc(sm31:em31,1:model_config_rec%num_metgrid_levels,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_ght_gc(sm31:em31,1:model_config_rec%num_metgrid_levels,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_ght_gc=initial_data_value
ELSE
ALLOCATE(grid%em_ght_gc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_ght_gc(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_p_gc(sm31:em31,1:model_config_rec%num_metgrid_levels,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_p_gc(sm31:em31,1:model_config_rec%num_metgrid_levels,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_p_gc=initial_data_value
ELSE
ALLOCATE(grid%em_p_gc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_p_gc(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_xlat_gc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_xlat_gc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_xlat_gc=initial_data_value
ELSE
ALLOCATE(grid%em_xlat_gc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_xlat_gc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_xlong_gc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_xlong_gc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_xlong_gc=initial_data_value
ELSE
ALLOCATE(grid%em_xlong_gc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_xlong_gc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_ht_gc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_ht_gc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_ht_gc=initial_data_value
ELSE
ALLOCATE(grid%em_ht_gc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_ht_gc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_tsk_gc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_tsk_gc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_tsk_gc=initial_data_value
ELSE
ALLOCATE(grid%em_tsk_gc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_tsk_gc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_tavgsfc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_tavgsfc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_tavgsfc=initial_data_value
ELSE
ALLOCATE(grid%em_tavgsfc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_tavgsfc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_tmn_gc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_tmn_gc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_tmn_gc=initial_data_value
ELSE
ALLOCATE(grid%em_tmn_gc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_tmn_gc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_pslv_gc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_pslv_gc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_pslv_gc=initial_data_value
ELSE
ALLOCATE(grid%em_pslv_gc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_pslv_gc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_greenfrac(sm31:em31,1:12,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_greenfrac(sm31:em31,1:12,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_greenfrac=initial_data_value
ELSE
ALLOCATE(grid%em_greenfrac(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_greenfrac(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_albedo12m(sm31:em31,1:12,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_albedo12m(sm31:em31,1:12,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_albedo12m=initial_data_value
ELSE
ALLOCATE(grid%em_albedo12m(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_albedo12m(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_pd_gc(sm31:em31,1:model_config_rec%num_metgrid_levels,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_pd_gc(sm31:em31,1:model_config_rec%num_metgrid_levels,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_pd_gc=initial_data_value
ELSE
ALLOCATE(grid%em_pd_gc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_pd_gc(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_psfc_gc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_psfc_gc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_psfc_gc=initial_data_value
ELSE
ALLOCATE(grid%em_psfc_gc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_psfc_gc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_intq_gc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_intq_gc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_intq_gc=initial_data_value
ELSE
ALLOCATE(grid%em_intq_gc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_intq_gc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_pdhs(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_pdhs(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_pdhs=initial_data_value
ELSE
ALLOCATE(grid%em_pdhs(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_pdhs(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_qv_gc(sm31:em31,1:model_config_rec%num_metgrid_levels,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_qv_gc(sm31:em31,1:model_config_rec%num_metgrid_levels,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_qv_gc=initial_data_value
ELSE
ALLOCATE(grid%em_qv_gc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_qv_gc(1,1,1).  ')
 endif
ENDIF
IF(IAND(1,tl).NE.0)THEN
ALLOCATE(grid%em_u_1(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_u_1(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_u_1=initial_data_value
ELSE
ALLOCATE(grid%em_u_1(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_u_1(1,1,1).  ')
 endif
ENDIF
IF(IAND(2,tl).NE.0)THEN
ALLOCATE(grid%em_u_2(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_u_2(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_u_2=initial_data_value
ELSE
ALLOCATE(grid%em_u_2(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_u_2(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_u_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_u_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_u_b=initial_data_value
ELSE
ALLOCATE(grid%em_u_b(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_u_b(1,1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_u_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_u_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_u_bt=initial_data_value
ELSE
ALLOCATE(grid%em_u_bt(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_u_bt(1,1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_ru(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_ru(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_ru=initial_data_value
ELSE
ALLOCATE(grid%em_ru(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_ru(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_ru_m(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_ru_m(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_ru_m=initial_data_value
ELSE
ALLOCATE(grid%em_ru_m(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_ru_m(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_ru_tend(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_ru_tend(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_ru_tend=initial_data_value
ELSE
ALLOCATE(grid%em_ru_tend(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_ru_tend(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_u_save(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_u_save(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_u_save=initial_data_value
ELSE
ALLOCATE(grid%em_u_save(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_u_save(1,1,1).  ')
 endif
ENDIF
IF(IAND(1,tl).NE.0)THEN
ALLOCATE(grid%em_v_1(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_v_1(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_v_1=initial_data_value
ELSE
ALLOCATE(grid%em_v_1(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_v_1(1,1,1).  ')
 endif
ENDIF
IF(IAND(2,tl).NE.0)THEN
ALLOCATE(grid%em_v_2(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_v_2(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_v_2=initial_data_value
ELSE
ALLOCATE(grid%em_v_2(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_v_2(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_v_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_v_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_v_b=initial_data_value
ELSE
ALLOCATE(grid%em_v_b(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_v_b(1,1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_v_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_v_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_v_bt=initial_data_value
ELSE
ALLOCATE(grid%em_v_bt(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_v_bt(1,1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_rv(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_rv(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_rv=initial_data_value
ELSE
ALLOCATE(grid%em_rv(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_rv(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_rv_m(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_rv_m(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_rv_m=initial_data_value
ELSE
ALLOCATE(grid%em_rv_m(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_rv_m(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_rv_tend(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_rv_tend(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_rv_tend=initial_data_value
ELSE
ALLOCATE(grid%em_rv_tend(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_rv_tend(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_v_save(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_v_save(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_v_save=initial_data_value
ELSE
ALLOCATE(grid%em_v_save(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_v_save(1,1,1).  ')
 endif
ENDIF
IF(IAND(1,tl).NE.0)THEN
ALLOCATE(grid%em_w_1(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_w_1(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_w_1=initial_data_value
ELSE
ALLOCATE(grid%em_w_1(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_w_1(1,1,1).  ')
 endif
ENDIF
IF(IAND(2,tl).NE.0)THEN
ALLOCATE(grid%em_w_2(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_w_2(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_w_2=initial_data_value
ELSE
ALLOCATE(grid%em_w_2(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_w_2(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_w_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_w_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_w_b=initial_data_value
ELSE
ALLOCATE(grid%em_w_b(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_w_b(1,1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_w_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_w_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_w_bt=initial_data_value
ELSE
ALLOCATE(grid%em_w_bt(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_w_bt(1,1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_ww(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_ww(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_ww=initial_data_value
ELSE
ALLOCATE(grid%em_ww(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_ww(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_rw(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_rw(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_rw=initial_data_value
ELSE
ALLOCATE(grid%em_rw(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_rw(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_ww_m(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_ww_m(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_ww_m=initial_data_value
ELSE
ALLOCATE(grid%em_ww_m(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_ww_m(1,1,1).  ')
 endif
ENDIF
IF(IAND(1,tl).NE.0)THEN
ALLOCATE(grid%em_ph_1(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_ph_1(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_ph_1=initial_data_value
ELSE
ALLOCATE(grid%em_ph_1(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_ph_1(1,1,1).  ')
 endif
ENDIF
IF(IAND(2,tl).NE.0)THEN
ALLOCATE(grid%em_ph_2(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_ph_2(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_ph_2=initial_data_value
ELSE
ALLOCATE(grid%em_ph_2(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_ph_2(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_ph_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_ph_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_ph_b=initial_data_value
ELSE
ALLOCATE(grid%em_ph_b(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_ph_b(1,1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_ph_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_ph_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_ph_bt=initial_data_value
ELSE
ALLOCATE(grid%em_ph_bt(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_ph_bt(1,1,1,1).  ')
 endif
ENDIF
ALLOCATE(grid%em_phb(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_phb(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_phb=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_phb_fine(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_phb_fine(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_phb_fine=initial_data_value
ELSE
ALLOCATE(grid%em_phb_fine(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_phb_fine(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_ph0(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_ph0(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_ph0=initial_data_value
ELSE
ALLOCATE(grid%em_ph0(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_ph0(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_php(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_php(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_php=initial_data_value
ELSE
ALLOCATE(grid%em_php(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_php(1,1,1).  ')
 endif
ENDIF
IF(IAND(1,tl).NE.0)THEN
ALLOCATE(grid%em_t_1(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_t_1(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_t_1=initial_data_value
ELSE
ALLOCATE(grid%em_t_1(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_t_1(1,1,1).  ')
 endif
ENDIF
IF(IAND(2,tl).NE.0)THEN
ALLOCATE(grid%em_t_2(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_t_2(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_t_2=initial_data_value
ELSE
ALLOCATE(grid%em_t_2(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_t_2(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_t_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_t_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_t_b=initial_data_value
ELSE
ALLOCATE(grid%em_t_b(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_t_b(1,1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_t_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_t_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_t_bt=initial_data_value
ELSE
ALLOCATE(grid%em_t_bt(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_t_bt(1,1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_t_init(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_t_init(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_t_init=initial_data_value
ELSE
ALLOCATE(grid%em_t_init(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_t_init(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
IF(IAND(1,tl).NE.0)THEN
ALLOCATE(grid%em_tp_1(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_tp_1(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_tp_1=initial_data_value
ELSE
ALLOCATE(grid%em_tp_1(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_tp_1(1,1,1).  ')
 endif
ENDIF
ELSE
ALLOCATE(grid%em_tp_1(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_tp_1(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
IF(IAND(2,tl).NE.0)THEN
ALLOCATE(grid%em_tp_2(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_tp_2(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_tp_2=initial_data_value
ELSE
ALLOCATE(grid%em_tp_2(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_tp_2(1,1,1).  ')
 endif
ENDIF
ELSE
ALLOCATE(grid%em_tp_2(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_tp_2(1,1,1).  ')
 endif
ENDIF
ALLOCATE(grid%em_t_save(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_t_save(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_t_save=initial_data_value
IF(IAND(1,tl).NE.0)THEN
ALLOCATE(grid%em_mu_1(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_mu_1(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_mu_1=initial_data_value
ELSE
ALLOCATE(grid%em_mu_1(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_mu_1(1,1).  ')
 endif
ENDIF
IF(IAND(2,tl).NE.0)THEN
ALLOCATE(grid%em_mu_2(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_mu_2(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_mu_2=initial_data_value
ELSE
ALLOCATE(grid%em_mu_2(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_mu_2(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_mu_b(max(ed31,ed33),1,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_mu_b(max(ed31,ed33),1,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_mu_b=initial_data_value
ELSE
ALLOCATE(grid%em_mu_b(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_mu_b(1,1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_mu_bt(max(ed31,ed33),1,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_mu_bt(max(ed31,ed33),1,spec_bdy_width,4). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_mu_bt=initial_data_value
ELSE
ALLOCATE(grid%em_mu_bt(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_mu_bt(1,1,1,1).  ')
 endif
ENDIF
ALLOCATE(grid%em_mub(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_mub(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_mub=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_mub_fine(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_mub_fine(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_mub_fine=initial_data_value
ELSE
ALLOCATE(grid%em_mub_fine(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_mub_fine(1,1).  ')
 endif
ENDIF
ALLOCATE(grid%em_mu0(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_mu0(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_mu0=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_mudf(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_mudf(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_mudf=initial_data_value
ELSE
ALLOCATE(grid%em_mudf(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_mudf(1,1).  ')
 endif
ENDIF
ALLOCATE(grid%em_muu(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_muu(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_muu=initial_data_value
ALLOCATE(grid%em_muv(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_muv(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_muv=initial_data_value
ALLOCATE(grid%em_mut(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_mut(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_mut=initial_data_value
ALLOCATE(grid%em_muts(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_muts(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_muts=initial_data_value
ALLOCATE(grid%nest_pos(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nest_pos(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nest_pos=initial_data_value
ALLOCATE(grid%nest_mask(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nest_mask(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nest_mask=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%ht_coarse(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ht_coarse(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ht_coarse=initial_data_value
ELSE
ALLOCATE(grid%ht_coarse(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ht_coarse(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
IF(IAND(1,tl).NE.0)THEN
ALLOCATE(grid%em_tke_1(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_tke_1(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_tke_1=initial_data_value
ELSE
ALLOCATE(grid%em_tke_1(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_tke_1(1,1,1).  ')
 endif
ENDIF
ELSE
ALLOCATE(grid%em_tke_1(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_tke_1(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
IF(IAND(2,tl).NE.0)THEN
ALLOCATE(grid%em_tke_2(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_tke_2(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_tke_2=initial_data_value
ELSE
ALLOCATE(grid%em_tke_2(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_tke_2(1,1,1).  ')
 endif
ENDIF
ELSE
ALLOCATE(grid%em_tke_2(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_tke_2(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_p(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_p(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_p=initial_data_value
ELSE
ALLOCATE(grid%em_p(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_p(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_al(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_al(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_al=initial_data_value
ELSE
ALLOCATE(grid%em_al(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_al(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_alt(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_alt(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_alt=initial_data_value
ELSE
ALLOCATE(grid%em_alt(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_alt(1,1,1).  ')
 endif
ENDIF
ALLOCATE(grid%em_alb(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_alb(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_alb=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_zx(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_zx(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_zx=initial_data_value
ELSE
ALLOCATE(grid%em_zx(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_zx(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_zy(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_zy(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_zy=initial_data_value
ELSE
ALLOCATE(grid%em_zy(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_zy(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_rdz(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_rdz(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_rdz=initial_data_value
ELSE
ALLOCATE(grid%em_rdz(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_rdz(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_rdzw(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_rdzw(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_rdzw=initial_data_value
ELSE
ALLOCATE(grid%em_rdzw(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_rdzw(1,1,1).  ')
 endif
ENDIF
ALLOCATE(grid%em_pb(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_pb(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_pb=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_sr(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_sr(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_sr=initial_data_value
ELSE
ALLOCATE(grid%em_sr(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_sr(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_potevp(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_potevp(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_potevp=initial_data_value
ELSE
ALLOCATE(grid%em_potevp(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_potevp(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_snopcx(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_snopcx(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_snopcx=initial_data_value
ELSE
ALLOCATE(grid%em_snopcx(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_snopcx(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_soiltb(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_soiltb(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_soiltb=initial_data_value
ELSE
ALLOCATE(grid%em_soiltb(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_soiltb(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_fnm(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_fnm(sm32:em32). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_fnm=initial_data_value
ELSE
ALLOCATE(grid%em_fnm(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_fnm(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_fnp(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_fnp(sm32:em32). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_fnp=initial_data_value
ELSE
ALLOCATE(grid%em_fnp(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_fnp(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_rdnw(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_rdnw(sm32:em32). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_rdnw=initial_data_value
ELSE
ALLOCATE(grid%em_rdnw(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_rdnw(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_rdn(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_rdn(sm32:em32). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_rdn=initial_data_value
ELSE
ALLOCATE(grid%em_rdn(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_rdn(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_dnw(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_dnw(sm32:em32). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_dnw=initial_data_value
ELSE
ALLOCATE(grid%em_dnw(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_dnw(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_dn(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_dn(sm32:em32). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_dn=initial_data_value
ELSE
ALLOCATE(grid%em_dn(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_dn(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_znu(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_znu(sm32:em32). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_znu=initial_data_value
ELSE
ALLOCATE(grid%em_znu(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_znu(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_znw(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_znw(sm32:em32). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_znw=initial_data_value
ELSE
ALLOCATE(grid%em_znw(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_znw(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_t_base(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_t_base(sm32:em32). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_t_base=initial_data_value
ELSE
ALLOCATE(grid%em_t_base(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_t_base(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_z(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_z(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_z=initial_data_value
ELSE
ALLOCATE(grid%em_z(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_z(1,1,1).  ')
 endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%cfn=initial_data_value
IF ( setinitval .EQ. 3 ) grid%cfn1=initial_data_value
IF ( setinitval .EQ. 3 ) grid%step_number=0
ALLOCATE(grid%q2(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%q2(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%q2=initial_data_value
ALLOCATE(grid%t2(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%t2(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%t2=initial_data_value
ALLOCATE(grid%th2(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%th2(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%th2=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%psfc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%psfc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%psfc=initial_data_value
ELSE
ALLOCATE(grid%psfc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%psfc(1,1).  ')
 endif
ENDIF
ALLOCATE(grid%u10(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%u10(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%u10=initial_data_value
ALLOCATE(grid%v10(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%v10(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%v10=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%uratx(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%uratx(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%uratx=initial_data_value
ELSE
ALLOCATE(grid%uratx(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%uratx(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%vratx(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%vratx(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%vratx=initial_data_value
ELSE
ALLOCATE(grid%vratx(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%vratx(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%tratx(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tratx(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tratx=initial_data_value
ELSE
ALLOCATE(grid%tratx(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tratx(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%em_obs_savwt(1:model_config_rec%nobs_err_flds,sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_obs_savwt(1:model_config_rec%nobs_err_flds,sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_obs_savwt=initial_data_value
ELSE
ALLOCATE(grid%em_obs_savwt(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_obs_savwt(1,1,1,1).  ')
 endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%rdx=initial_data_value
IF ( setinitval .EQ. 3 ) grid%rdy=initial_data_value
IF ( setinitval .EQ. 3 ) grid%dts=initial_data_value
IF ( setinitval .EQ. 3 ) grid%dtseps=initial_data_value
IF ( setinitval .EQ. 3 ) grid%resm=initial_data_value
IF ( setinitval .EQ. 3 ) grid%zetatop=initial_data_value
IF ( setinitval .EQ. 3 ) grid%cf1=initial_data_value
IF ( setinitval .EQ. 3 ) grid%cf2=initial_data_value
IF ( setinitval .EQ. 3 ) grid%cf3=initial_data_value
IF ( setinitval .EQ. 3 ) grid%number_at_same_level=0
IF ( setinitval .EQ. 3 ) grid%itimestep=0
IF ( setinitval .EQ. 3 ) grid%xtime=initial_data_value
IF ( setinitval .EQ. 3 ) grid%julian=initial_data_value
IF ( setinitval .EQ. 3 ) grid%lbc_fid=0
IF ( setinitval .EQ. 3 ) grid%tiled=.FALSE.
IF ( setinitval .EQ. 3 ) grid%patched=.FALSE.
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%imask_nostag(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%imask_nostag(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%imask_xstag(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%imask_xstag(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%imask_ystag(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%imask_ystag(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%imask_xystag(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%imask_xystag(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%imask_xystag=0
ELSE
ALLOCATE(grid%imask_xystag(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%imask_xystag(1,1).  ')
 endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%xi=initial_data_value
IF ( setinitval .EQ. 3 ) grid%xj=initial_data_value
IF ( setinitval .EQ. 3 ) grid%vc_i=initial_data_value
IF ( setinitval .EQ. 3 ) grid%vc_j=initial_data_value
ALLOCATE(grid%moist(sm31:em31,sm32:em32,sm33:em33,num_moist),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%moist(sm31:em31,sm32:em32,sm33:em33,num_moist). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%moist=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%moist_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4,num_moist),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%moist_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4,num_moist). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%moist_b=initial_data_value
ELSE
ALLOCATE(grid%moist_b(1,1,1,1,num_moist),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%moist_b(1,1,1,1,num_moist).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%moist_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4,num_moist),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%moist_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4,num_moist). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%moist_bt=initial_data_value
ELSE
ALLOCATE(grid%moist_bt(1,1,1,1,num_moist),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%moist_bt(1,1,1,1,num_moist).  ')
 endif
ENDIF
ALLOCATE(grid%chem(sm31:em31,sm32:em32,sm33:em33,num_chem),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%chem(sm31:em31,sm32:em32,sm33:em33,num_chem). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%chem=initial_data_value
ALLOCATE(grid%scalar(sm31:em31,sm32:em32,sm33:em33,num_scalar),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%scalar(sm31:em31,sm32:em32,sm33:em33,num_scalar). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%scalar=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%scalar_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4,num_scalar),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%scalar_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4,num_scalar). ')
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
ALLOCATE(grid%scalar_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4,num_scalar),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%scalar_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4,num_scalar). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%scalar_bt=initial_data_value
ELSE
ALLOCATE(grid%scalar_bt(1,1,1,1,num_scalar),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%scalar_bt(1,1,1,1,num_scalar).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%fcx(1:model_config_rec%spec_bdy_width),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fcx(1:model_config_rec%spec_bdy_width). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fcx=initial_data_value
ELSE
ALLOCATE(grid%fcx(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fcx(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%gcx(1:model_config_rec%spec_bdy_width),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%gcx(1:model_config_rec%spec_bdy_width). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%gcx=initial_data_value
ELSE
ALLOCATE(grid%gcx(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%gcx(1).  ')
 endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%dtbc=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%sm000007(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm000007(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%sm007028(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm007028(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%sm028100(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm028100(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%sm100255(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm100255(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%st000007(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st000007(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%st007028(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st007028(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%st028100(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st028100(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%st100255(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st100255(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%sm000010(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm000010(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%sm010040(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm010040(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%sm040100(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm040100(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%sm100200(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm100200(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%sm010200(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm010200(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%soilm000(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilm000(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%soilm005(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilm005(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%soilm020(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilm020(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%soilm040(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilm040(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%soilm160(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilm160(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%soilm300(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilm300(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%sw000010(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sw000010(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%sw010040(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sw010040(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%sw040100(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sw040100(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%sw100200(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sw100200(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%sw010200(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sw010200(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%soilw000(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilw000(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%soilw005(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilw005(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%soilw020(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilw020(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%soilw040(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilw040(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%soilw160(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilw160(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%soilw300(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilw300(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%st000010(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st000010(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%st010040(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st010040(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%st040100(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st040100(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%st100200(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st100200(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%st010200(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st010200(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%soilt000(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilt000(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%soilt005(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilt005(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%soilt020(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilt020(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%soilt040(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilt040(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%soilt160(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilt160(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%soilt300(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilt300(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilt300=initial_data_value
ELSE
ALLOCATE(grid%soilt300(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilt300(1,1).  ')
 endif
ENDIF
ALLOCATE(grid%landmask(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%landmask(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%landmask=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%topostdv(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%topostdv(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%toposlpx(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%toposlpx(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%toposlpy(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%toposlpy(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%shdmax(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%shdmax(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%shdmax=initial_data_value
ELSE
ALLOCATE(grid%shdmax(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%shdmax(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%shdmin(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%shdmin(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%shdmin=initial_data_value
ELSE
ALLOCATE(grid%shdmin(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%shdmin(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%snoalb(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%snoalb(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%slopecat(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%slopecat(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%toposoil(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%toposoil(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%landusef(sm31:em31,1:model_config_rec%num_land_cat,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%landusef(sm31:em31,1:model_config_rec%num_land_cat,sm33:em33). ')
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
ALLOCATE(grid%soilctop(sm31:em31,1:model_config_rec%num_soil_cat,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilctop(sm31:em31,1:model_config_rec%num_soil_cat,sm33:em33). ')
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
ALLOCATE(grid%soilcbot(sm31:em31,1:model_config_rec%num_soil_cat,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilcbot(sm31:em31,1:model_config_rec%num_soil_cat,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilcbot=initial_data_value
ELSE
ALLOCATE(grid%soilcbot(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilcbot(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilcat(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilcat(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%vegcat(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%vegcat(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%vegcat=initial_data_value
ELSE
ALLOCATE(grid%vegcat(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%vegcat(1,1).  ')
 endif
ENDIF
ALLOCATE(grid%tslb(sm31:em31,1:model_config_rec%num_soil_layers,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tslb(sm31:em31,1:model_config_rec%num_soil_layers,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tslb=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%zs(1:model_config_rec%num_soil_layers),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%zs(1:model_config_rec%num_soil_layers). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%zs=initial_data_value
ELSE
ALLOCATE(grid%zs(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%zs(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%dzs(1:model_config_rec%num_soil_layers),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%dzs(1:model_config_rec%num_soil_layers). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dzs=initial_data_value
ELSE
ALLOCATE(grid%dzs(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%dzs(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%dzr(1:model_config_rec%num_soil_layers),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%dzr(1:model_config_rec%num_soil_layers). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dzr=initial_data_value
ELSE
ALLOCATE(grid%dzr(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%dzr(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%dzb(1:model_config_rec%num_soil_layers),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%dzb(1:model_config_rec%num_soil_layers). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dzb=initial_data_value
ELSE
ALLOCATE(grid%dzb(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%dzb(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%dzg(1:model_config_rec%num_soil_layers),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%dzg(1:model_config_rec%num_soil_layers). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dzg=initial_data_value
ELSE
ALLOCATE(grid%dzg(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%dzg(1).  ')
 endif
ENDIF
ALLOCATE(grid%smois(sm31:em31,1:model_config_rec%num_soil_layers,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%smois(sm31:em31,1:model_config_rec%num_soil_layers,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%smois=initial_data_value
ALLOCATE(grid%sh2o(sm31:em31,1:model_config_rec%num_soil_layers,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sh2o(sm31:em31,1:model_config_rec%num_soil_layers,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sh2o=initial_data_value
ALLOCATE(grid%xice(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xice(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xice=initial_data_value
ALLOCATE(grid%smstav(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%smstav(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%smstav=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%smstot(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%smstot(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%smstot=initial_data_value
ELSE
ALLOCATE(grid%smstot(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%smstot(1,1).  ')
 endif
ENDIF
ALLOCATE(grid%sfcrunoff(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sfcrunoff(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sfcrunoff=initial_data_value
ALLOCATE(grid%udrunoff(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%udrunoff(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%udrunoff=initial_data_value
ALLOCATE(grid%ivgtyp(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ivgtyp(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ivgtyp=0
ALLOCATE(grid%isltyp(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%isltyp(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%isltyp=0
ALLOCATE(grid%vegfra(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%vegfra(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%vegfra=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%sfcevp(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sfcevp(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%grdflx(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%grdflx(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%sfcexc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sfcexc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sfcexc=initial_data_value
ELSE
ALLOCATE(grid%sfcexc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sfcexc(1,1).  ')
 endif
ENDIF
ALLOCATE(grid%acsnow(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%acsnow(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%acsnow=initial_data_value
ALLOCATE(grid%acsnom(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%acsnom(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%acsnom=initial_data_value
ALLOCATE(grid%snow(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%snow(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%snow=initial_data_value
ALLOCATE(grid%snowh(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%snowh(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%snowh=initial_data_value
ALLOCATE(grid%rhosn(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rhosn(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rhosn=initial_data_value
ALLOCATE(grid%canwat(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%canwat(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%canwat=initial_data_value
ALLOCATE(grid%sst(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sst(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sst=initial_data_value
IF ( setinitval .EQ. 3 ) grid%ifndsnowh=0
IF ( setinitval .EQ. 3 ) grid%ifndsoilw=0
ALLOCATE(grid%tr_urb2d(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tr_urb2d(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tr_urb2d=initial_data_value
ALLOCATE(grid%tb_urb2d(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tb_urb2d(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tb_urb2d=initial_data_value
ALLOCATE(grid%tg_urb2d(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tg_urb2d(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tg_urb2d=initial_data_value
ALLOCATE(grid%tc_urb2d(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tc_urb2d(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tc_urb2d=initial_data_value
ALLOCATE(grid%qc_urb2d(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%qc_urb2d(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%qc_urb2d=initial_data_value
ALLOCATE(grid%uc_urb2d(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%uc_urb2d(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%uc_urb2d=initial_data_value
ALLOCATE(grid%xxxr_urb2d(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xxxr_urb2d(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xxxr_urb2d=initial_data_value
ALLOCATE(grid%xxxb_urb2d(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xxxb_urb2d(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xxxb_urb2d=initial_data_value
ALLOCATE(grid%xxxg_urb2d(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xxxg_urb2d(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xxxg_urb2d=initial_data_value
ALLOCATE(grid%xxxc_urb2d(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xxxc_urb2d(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xxxc_urb2d=initial_data_value
ALLOCATE(grid%trl_urb3d(sm31:em31,1:model_config_rec%num_soil_layers,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%trl_urb3d(sm31:em31,1:model_config_rec%num_soil_layers,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%trl_urb3d=initial_data_value
ALLOCATE(grid%tbl_urb3d(sm31:em31,1:model_config_rec%num_soil_layers,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tbl_urb3d(sm31:em31,1:model_config_rec%num_soil_layers,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tbl_urb3d=initial_data_value
ALLOCATE(grid%tgl_urb3d(sm31:em31,1:model_config_rec%num_soil_layers,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tgl_urb3d(sm31:em31,1:model_config_rec%num_soil_layers,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tgl_urb3d=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%sh_urb2d(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sh_urb2d(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sh_urb2d=initial_data_value
ELSE
ALLOCATE(grid%sh_urb2d(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sh_urb2d(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%lh_urb2d(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%lh_urb2d(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%lh_urb2d=initial_data_value
ELSE
ALLOCATE(grid%lh_urb2d(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%lh_urb2d(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%g_urb2d(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%g_urb2d(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%g_urb2d=initial_data_value
ELSE
ALLOCATE(grid%g_urb2d(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%g_urb2d(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rn_urb2d(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rn_urb2d(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rn_urb2d=initial_data_value
ELSE
ALLOCATE(grid%rn_urb2d(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rn_urb2d(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%ts_urb2d(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ts_urb2d(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ts_urb2d=initial_data_value
ELSE
ALLOCATE(grid%ts_urb2d(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ts_urb2d(1,1).  ')
 endif
ENDIF
ALLOCATE(grid%frc_urb2d(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%frc_urb2d(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%frc_urb2d=initial_data_value
ALLOCATE(grid%utype_urb2d(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%utype_urb2d(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%utype_urb2d=0
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%cosz_urb2d(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%cosz_urb2d(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cosz_urb2d=initial_data_value
ELSE
ALLOCATE(grid%cosz_urb2d(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%cosz_urb2d(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%omg_urb2d(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%omg_urb2d(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%omg_urb2d=initial_data_value
ELSE
ALLOCATE(grid%omg_urb2d(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%omg_urb2d(1,1).  ')
 endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%declin_urb=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%smfr3d(sm31:em31,1:model_config_rec%num_soil_layers,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%smfr3d(sm31:em31,1:model_config_rec%num_soil_layers,sm33:em33). ')
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
ALLOCATE(grid%keepfr3dflag(sm31:em31,1:model_config_rec%num_soil_layers,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%keepfr3dflag(sm31:em31,1:model_config_rec%num_soil_layers,sm33:em33). ')
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
ALLOCATE(grid%el_myj(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%el_myj(sm31:em31,sm32:em32,sm33:em33). ')
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
ALLOCATE(grid%exch_h(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%exch_h(sm31:em31,sm32:em32,sm33:em33). ')
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
ALLOCATE(grid%ct(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ct(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ct=initial_data_value
ELSE
ALLOCATE(grid%ct(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ct(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%thz0(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%thz0(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%z0(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%z0(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%z0=initial_data_value
ELSE
ALLOCATE(grid%z0(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%z0(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%qz0(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%qz0(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%uz0(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%uz0(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%vz0(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%vz0(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%qsfc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%qsfc(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%akhs(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%akhs(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%akms(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%akms(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%kpbl(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%kpbl(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%kpbl=0
ELSE
ALLOCATE(grid%kpbl(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%kpbl(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%htop(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%htop(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%hbot(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%hbot(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%htopr(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%htopr(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%hbotr(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%hbotr(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%cutop(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%cutop(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cutop=initial_data_value
ELSE
ALLOCATE(grid%cutop(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%cutop(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%cubot(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%cubot(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cubot=initial_data_value
ELSE
ALLOCATE(grid%cubot(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%cubot(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%cuppt(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%cuppt(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%rswtoa(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rswtoa(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rswtoa=initial_data_value
ELSE
ALLOCATE(grid%rswtoa(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rswtoa(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rlwtoa(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rlwtoa(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rlwtoa=initial_data_value
ELSE
ALLOCATE(grid%rlwtoa(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rlwtoa(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%czmean(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%czmean(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%czmean=initial_data_value
ELSE
ALLOCATE(grid%czmean(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%czmean(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%cfracl(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%cfracl(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cfracl=initial_data_value
ELSE
ALLOCATE(grid%cfracl(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%cfracl(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%cfracm(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%cfracm(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cfracm=initial_data_value
ELSE
ALLOCATE(grid%cfracm(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%cfracm(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%cfrach(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%cfrach(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cfrach=initial_data_value
ELSE
ALLOCATE(grid%cfrach(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%cfrach(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%acfrst(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%acfrst(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%acfrst=initial_data_value
ELSE
ALLOCATE(grid%acfrst(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%acfrst(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%ncfrst(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ncfrst(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ncfrst=0
ELSE
ALLOCATE(grid%ncfrst(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ncfrst(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%acfrcv(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%acfrcv(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%acfrcv=initial_data_value
ELSE
ALLOCATE(grid%acfrcv(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%acfrcv(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%ncfrcv(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ncfrcv(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ncfrcv=0
ELSE
ALLOCATE(grid%ncfrcv(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ncfrcv(1,1).  ')
 endif
ENDIF
ALLOCATE(grid%ozmixm(sm31:em31,1:model_config_rec%levsiz,sm33:em33,num_ozmixm),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ozmixm(sm31:em31,1:model_config_rec%levsiz,sm33:em33,num_ozmixm). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ozmixm=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%pin(1:model_config_rec%levsiz),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%pin(1:model_config_rec%levsiz). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%pin=initial_data_value
ELSE
ALLOCATE(grid%pin(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%pin(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
IF(IAND(1,tl).NE.0)THEN
ALLOCATE(grid%m_ps_1(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%m_ps_1(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%m_ps_1=initial_data_value
ELSE
ALLOCATE(grid%m_ps_1(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%m_ps_1(1,1).  ')
 endif
ENDIF
ELSE
ALLOCATE(grid%m_ps_1(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%m_ps_1(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
IF(IAND(2,tl).NE.0)THEN
ALLOCATE(grid%m_ps_2(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%m_ps_2(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%m_ps_2=initial_data_value
ELSE
ALLOCATE(grid%m_ps_2(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%m_ps_2(1,1).  ')
 endif
ENDIF
ELSE
ALLOCATE(grid%m_ps_2(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%m_ps_2(1,1).  ')
 endif
ENDIF
IF(IAND(1,tl).NE.0)THEN
ALLOCATE(grid%aerosolc_1(sm31:em31,1:model_config_rec%paerlev,sm33:em33,num_aerosolc),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%aerosolc_1(sm31:em31,1:model_config_rec%paerlev,sm33:em33,num_aerosolc). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%aerosolc_1=initial_data_value
ELSE
ALLOCATE(grid%aerosolc_1(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%aerosolc_1(1,1,1,1).  ')
 endif
ENDIF
IF(IAND(2,tl).NE.0)THEN
ALLOCATE(grid%aerosolc_2(sm31:em31,1:model_config_rec%paerlev,sm33:em33,num_aerosolc),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%aerosolc_2(sm31:em31,1:model_config_rec%paerlev,sm33:em33,num_aerosolc). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%aerosolc_2=initial_data_value
ELSE
ALLOCATE(grid%aerosolc_2(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%aerosolc_2(1,1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%m_hybi(1:model_config_rec%paerlev),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%m_hybi(1:model_config_rec%paerlev). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%m_hybi=initial_data_value
ELSE
ALLOCATE(grid%m_hybi(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%m_hybi(1).  ')
 endif
ENDIF
ALLOCATE(grid%f_ice_phy(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%f_ice_phy(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%f_ice_phy=initial_data_value
ALLOCATE(grid%f_rain_phy(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%f_rain_phy(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%f_rain_phy=initial_data_value
ALLOCATE(grid%f_rimef_phy(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%f_rimef_phy(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%f_rimef_phy=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%h_diabatic(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%h_diabatic(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%h_diabatic=initial_data_value
ELSE
ALLOCATE(grid%h_diabatic(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%h_diabatic(1,1,1).  ')
 endif
ENDIF
ALLOCATE(grid%msft(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%msft(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%msft=initial_data_value
ALLOCATE(grid%msfu(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%msfu(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%msfu=initial_data_value
ALLOCATE(grid%msfv(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%msfv(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%msfv=initial_data_value
ALLOCATE(grid%f(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%f(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%f=initial_data_value
ALLOCATE(grid%e(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%e(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%e=initial_data_value
ALLOCATE(grid%sina(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sina(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sina=initial_data_value
ALLOCATE(grid%cosa(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%cosa(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cosa=initial_data_value
ALLOCATE(grid%ht(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ht(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ht=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%ht_fine(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ht_fine(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ht_fine=initial_data_value
ELSE
ALLOCATE(grid%ht_fine(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ht_fine(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%ht_int(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ht_int(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ht_int=initial_data_value
ELSE
ALLOCATE(grid%ht_int(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ht_int(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%ht_input(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ht_input(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ht_input=initial_data_value
ELSE
ALLOCATE(grid%ht_input(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ht_input(1,1).  ')
 endif
ENDIF
ALLOCATE(grid%tsk(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tsk(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tsk=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%tsk_save(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tsk_save(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tsk_save=initial_data_value
ELSE
ALLOCATE(grid%tsk_save(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tsk_save(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%u_base(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%u_base(sm32:em32). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%u_base=initial_data_value
ELSE
ALLOCATE(grid%u_base(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%u_base(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%v_base(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%v_base(sm32:em32). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%v_base=initial_data_value
ELSE
ALLOCATE(grid%v_base(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%v_base(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%qv_base(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%qv_base(sm32:em32). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%qv_base=initial_data_value
ELSE
ALLOCATE(grid%qv_base(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%qv_base(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%z_base(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%z_base(sm32:em32). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%z_base=initial_data_value
ELSE
ALLOCATE(grid%z_base(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%z_base(1).  ')
 endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%u_frame=initial_data_value
IF ( setinitval .EQ. 3 ) grid%v_frame=initial_data_value
IF ( setinitval .EQ. 3 ) grid%p_top=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lat_ll_t=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lat_ul_t=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lat_ur_t=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lat_lr_t=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lat_ll_u=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lat_ul_u=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lat_ur_u=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lat_lr_u=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lat_ll_v=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lat_ul_v=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lat_ur_v=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lat_lr_v=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lat_ll_d=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lat_ul_d=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lat_ur_d=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lat_lr_d=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lon_ll_t=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lon_ul_t=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lon_ur_t=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lon_lr_t=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lon_ll_u=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lon_ul_u=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lon_ur_u=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lon_lr_u=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lon_ll_v=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lon_ul_v=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lon_ur_v=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lon_lr_v=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lon_ll_d=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lon_ul_d=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lon_ur_d=initial_data_value
IF ( setinitval .EQ. 3 ) grid%em_lon_lr_d=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rthcuten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rthcuten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rthcuten=initial_data_value
ELSE
ALLOCATE(grid%rthcuten(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rthcuten(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rqvcuten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rqvcuten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqvcuten=initial_data_value
ELSE
ALLOCATE(grid%rqvcuten(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rqvcuten(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rqrcuten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rqrcuten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqrcuten=initial_data_value
ELSE
ALLOCATE(grid%rqrcuten(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rqrcuten(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rqccuten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rqccuten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqccuten=initial_data_value
ELSE
ALLOCATE(grid%rqccuten(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rqccuten(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rqscuten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rqscuten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqscuten=initial_data_value
ELSE
ALLOCATE(grid%rqscuten(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rqscuten(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rqicuten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rqicuten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqicuten=initial_data_value
ELSE
ALLOCATE(grid%rqicuten(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rqicuten(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%w0avg(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%w0avg(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%w0avg=initial_data_value
ELSE
ALLOCATE(grid%w0avg(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%w0avg(1,1,1).  ')
 endif
ENDIF
ALLOCATE(grid%rainc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rainc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rainc=initial_data_value
ALLOCATE(grid%rainnc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rainnc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rainnc=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%raincv(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%raincv(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%rainncv(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rainncv(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rainncv=initial_data_value
ELSE
ALLOCATE(grid%rainncv(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rainncv(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rainbl(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rainbl(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rainbl=initial_data_value
ELSE
ALLOCATE(grid%rainbl(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rainbl(1,1).  ')
 endif
ENDIF
ALLOCATE(grid%snownc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%snownc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%snownc=initial_data_value
ALLOCATE(grid%graupelnc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%graupelnc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%graupelnc=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%snowncv(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%snowncv(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%snowncv=initial_data_value
ELSE
ALLOCATE(grid%snowncv(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%snowncv(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%graupelncv(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%graupelncv(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%graupelncv=initial_data_value
ELSE
ALLOCATE(grid%graupelncv(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%graupelncv(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nca(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nca(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nca=initial_data_value
ELSE
ALLOCATE(grid%nca(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nca(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%lowlyr(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%lowlyr(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%lowlyr=0
ELSE
ALLOCATE(grid%lowlyr(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%lowlyr(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%mass_flux(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%mass_flux(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%apr_gr(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_gr(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%apr_w(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_w(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%apr_mc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_mc(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%apr_st(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_st(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%apr_as(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_as(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%apr_capma(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_capma(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%apr_capme(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_capme(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%apr_capmi(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_capmi(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%xf_ens(sm31:em31,sm33:em33,1:model_config_rec%ensdim),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xf_ens(sm31:em31,sm33:em33,1:model_config_rec%ensdim). ')
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
ALLOCATE(grid%pr_ens(sm31:em31,sm33:em33,1:model_config_rec%ensdim),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%pr_ens(sm31:em31,sm33:em33,1:model_config_rec%ensdim). ')
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
ALLOCATE(grid%rthften(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rthften(sm31:em31,sm32:em32,sm33:em33). ')
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
ALLOCATE(grid%rqvften(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rqvften(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqvften=initial_data_value
ELSE
ALLOCATE(grid%rqvften(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rqvften(1,1,1).  ')
 endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%stepcu=0
ALLOCATE(grid%rthraten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rthraten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rthraten=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rthratenlw(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rthratenlw(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rthratenlw=initial_data_value
ELSE
ALLOCATE(grid%rthratenlw(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rthratenlw(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rthratensw(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rthratensw(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rthratensw=initial_data_value
ELSE
ALLOCATE(grid%rthratensw(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rthratensw(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%cldfra(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%cldfra(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cldfra=initial_data_value
ELSE
ALLOCATE(grid%cldfra(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%cldfra(1,1,1).  ')
 endif
ENDIF
ALLOCATE(grid%swdown(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%swdown(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%swdown=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%swdownc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%swdownc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%swdownc=initial_data_value
ELSE
ALLOCATE(grid%swdownc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%swdownc(1,1).  ')
 endif
ENDIF
ALLOCATE(grid%gsw(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%gsw(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%gsw=initial_data_value
ALLOCATE(grid%glw(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%glw(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%glw=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%swcf(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%swcf(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%swcf=initial_data_value
ELSE
ALLOCATE(grid%swcf(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%swcf(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%lwcf(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%lwcf(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%lwcf=initial_data_value
ELSE
ALLOCATE(grid%lwcf(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%lwcf(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%olr(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%olr(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%olr=initial_data_value
ELSE
ALLOCATE(grid%olr(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%olr(1,1).  ')
 endif
ENDIF
ALLOCATE(grid%xlat(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xlat(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xlat=initial_data_value
ALLOCATE(grid%xlong(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xlong(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xlong=initial_data_value
ALLOCATE(grid%em_xlat_u(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_xlat_u(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_xlat_u=initial_data_value
ALLOCATE(grid%em_xlong_u(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_xlong_u(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_xlong_u=initial_data_value
ALLOCATE(grid%em_xlat_v(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_xlat_v(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_xlat_v=initial_data_value
ALLOCATE(grid%em_xlong_v(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%em_xlong_v(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_xlong_v=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%albedo(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%albedo(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%albedo=initial_data_value
ELSE
ALLOCATE(grid%albedo(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%albedo(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%albbck(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%albbck(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%emiss(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%emiss(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%emiss=initial_data_value
ELSE
ALLOCATE(grid%emiss(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%emiss(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%cldefi(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%cldefi(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cldefi=initial_data_value
ELSE
ALLOCATE(grid%cldefi(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%cldefi(1,1).  ')
 endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%stepra=0
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rublten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rublten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rublten=initial_data_value
ELSE
ALLOCATE(grid%rublten(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rublten(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rvblten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rvblten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rvblten=initial_data_value
ELSE
ALLOCATE(grid%rvblten(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rvblten(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rthblten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rthblten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rthblten=initial_data_value
ELSE
ALLOCATE(grid%rthblten(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rthblten(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rqvblten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rqvblten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqvblten=initial_data_value
ELSE
ALLOCATE(grid%rqvblten(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rqvblten(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rqcblten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rqcblten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqcblten=initial_data_value
ELSE
ALLOCATE(grid%rqcblten(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rqcblten(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rqiblten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rqiblten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqiblten=initial_data_value
ELSE
ALLOCATE(grid%rqiblten(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rqiblten(1,1,1).  ')
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
ALLOCATE(grid%tmn(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tmn(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tmn=initial_data_value
ALLOCATE(grid%xland(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xland(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xland=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%znt(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%znt(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%ust(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ust(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ust=initial_data_value
ELSE
ALLOCATE(grid%ust(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ust(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rmol(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rmol(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%mol(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%mol(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%pblh(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%pblh(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%pblh=initial_data_value
ELSE
ALLOCATE(grid%pblh(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%pblh(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%capg(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%capg(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%capg=initial_data_value
ELSE
ALLOCATE(grid%capg(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%capg(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%thc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%thc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%thc=initial_data_value
ELSE
ALLOCATE(grid%thc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%thc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%hfx(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%hfx(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%hfx=initial_data_value
ELSE
ALLOCATE(grid%hfx(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%hfx(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%qfx(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%qfx(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%qfx=initial_data_value
ELSE
ALLOCATE(grid%qfx(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%qfx(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%lh(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%lh(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%lh=initial_data_value
ELSE
ALLOCATE(grid%lh(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%lh(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%flhc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%flhc(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%flqc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%flqc(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%qsg(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%qsg(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%qvg(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%qvg(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%qcg(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%qcg(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%soilt1(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilt1(sm31:em31,sm33:em33). ')
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
ALLOCATE(grid%tsnav(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tsnav(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tsnav=initial_data_value
ELSE
ALLOCATE(grid%tsnav(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tsnav(1,1).  ')
 endif
ENDIF
ALLOCATE(grid%snowc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%snowc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%snowc=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%mavail(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%mavail(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%mavail=initial_data_value
ELSE
ALLOCATE(grid%mavail(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%mavail(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%tkesfcf(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tkesfcf(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tkesfcf=initial_data_value
ELSE
ALLOCATE(grid%tkesfcf(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tkesfcf(1,1).  ')
 endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%stepbl=0
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%taucldi(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%taucldi(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%taucldi=initial_data_value
ELSE
ALLOCATE(grid%taucldi(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%taucldi(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%taucldc(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%taucldc(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%taucldc=initial_data_value
ELSE
ALLOCATE(grid%taucldc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%taucldc(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%defor11(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%defor11(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%defor11=initial_data_value
ELSE
ALLOCATE(grid%defor11(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%defor11(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%defor22(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%defor22(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%defor22=initial_data_value
ELSE
ALLOCATE(grid%defor22(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%defor22(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%defor12(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%defor12(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%defor12=initial_data_value
ELSE
ALLOCATE(grid%defor12(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%defor12(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%defor33(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%defor33(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%defor33=initial_data_value
ELSE
ALLOCATE(grid%defor33(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%defor33(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%defor13(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%defor13(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%defor13=initial_data_value
ELSE
ALLOCATE(grid%defor13(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%defor13(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%defor23(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%defor23(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%defor23=initial_data_value
ELSE
ALLOCATE(grid%defor23(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%defor23(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%xkmv(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xkmv(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xkmv=initial_data_value
ELSE
ALLOCATE(grid%xkmv(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xkmv(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%xkmh(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xkmh(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xkmh=initial_data_value
ELSE
ALLOCATE(grid%xkmh(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xkmh(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%xkmhd(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xkmhd(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xkmhd=initial_data_value
ELSE
ALLOCATE(grid%xkmhd(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xkmhd(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%xkhv(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xkhv(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xkhv=initial_data_value
ELSE
ALLOCATE(grid%xkhv(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xkhv(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%xkhh(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xkhh(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xkhh=initial_data_value
ELSE
ALLOCATE(grid%xkhh(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xkhh(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%div(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%div(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%div=initial_data_value
ELSE
ALLOCATE(grid%div(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%div(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%bn2(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%bn2(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%bn2=initial_data_value
ELSE
ALLOCATE(grid%bn2(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%bn2(1,1,1).  ')
 endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%warm_rain=.FALSE.
IF ( setinitval .EQ. 3 ) grid%adv_moist_cond=.FALSE.
IF ( setinitval .EQ. 3 ) grid%stepfg=0
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rundgdten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rundgdten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rundgdten=initial_data_value
ELSE
ALLOCATE(grid%rundgdten(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rundgdten(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rvndgdten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rvndgdten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rvndgdten=initial_data_value
ELSE
ALLOCATE(grid%rvndgdten(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rvndgdten(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rthndgdten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rthndgdten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rthndgdten=initial_data_value
ELSE
ALLOCATE(grid%rthndgdten(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rthndgdten(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rqvndgdten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rqvndgdten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqvndgdten=initial_data_value
ELSE
ALLOCATE(grid%rqvndgdten(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rqvndgdten(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rmundgdten(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rmundgdten(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rmundgdten=initial_data_value
ELSE
ALLOCATE(grid%rmundgdten(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rmundgdten(1,1).  ')
 endif
ENDIF
ALLOCATE(grid%fdda3d(sm31:em31,sm32:em32,sm33:em33,num_fdda3d),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fdda3d(sm31:em31,sm32:em32,sm33:em33,num_fdda3d). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fdda3d=initial_data_value
ALLOCATE(grid%fdda2d(sm31:em31,1:1,sm33:em33,num_fdda2d),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fdda2d(sm31:em31,1:1,sm33:em33,num_fdda2d). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fdda2d=initial_data_value
IF ( setinitval .EQ. 3 ) grid%moved=.FALSE.
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%abstot(sm31:em31,sm32:em32,1:model_config_rec%cam_abs_dim2,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%abstot(sm31:em31,sm32:em32,1:model_config_rec%cam_abs_dim2,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%abstot=initial_data_value
ELSE
ALLOCATE(grid%abstot(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%abstot(1,1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%absnxt(sm31:em31,sm32:em32,1:model_config_rec%cam_abs_dim1,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%absnxt(sm31:em31,sm32:em32,1:model_config_rec%cam_abs_dim1,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%absnxt=initial_data_value
ELSE
ALLOCATE(grid%absnxt(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%absnxt(1,1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%emstot(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%emstot(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%emstot=initial_data_value
ELSE
ALLOCATE(grid%emstot(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%emstot(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%dpsdt(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%dpsdt(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dpsdt=initial_data_value
ELSE
ALLOCATE(grid%dpsdt(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%dpsdt(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%dmudt(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%dmudt(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dmudt=initial_data_value
ELSE
ALLOCATE(grid%dmudt(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%dmudt(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%pk1m(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%pk1m(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%pk1m=initial_data_value
ELSE
ALLOCATE(grid%pk1m(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%pk1m(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%mu_2m(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%mu_2m(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%mu_2m=initial_data_value
ELSE
ALLOCATE(grid%mu_2m(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%mu_2m(1,1).  ')
 endif
ENDIF
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
IF ( setinitval .EQ. 3 ) grid%input_from_hires=.FALSE.
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
IF ( setinitval .EQ. 3 ) grid%diag_print=0
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
IF ( setinitval .EQ. 3 ) grid%num_metgrid_levels=0
IF ( setinitval .EQ. 3 ) grid%p_top_requested=initial_data_value
IF ( setinitval .EQ. 3 ) grid%interp_type=0
IF ( setinitval .EQ. 3 ) grid%lowest_lev_from_sfc=.FALSE.
IF ( setinitval .EQ. 3 ) grid%lagrange_order=0
IF ( setinitval .EQ. 3 ) grid%force_sfc_in_vinterp=0
IF ( setinitval .EQ. 3 ) grid%zap_close_levels=initial_data_value
IF ( setinitval .EQ. 3 ) grid%sfcp_to_sfcp=.FALSE.
IF ( setinitval .EQ. 3 ) grid%adjust_heights=.FALSE.
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
IF ( setinitval .EQ. 3 ) grid%blend_width=0
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
IF ( setinitval .EQ. 3 ) grid%vortex_interval=0
IF ( setinitval .EQ. 3 ) grid%max_vortex_speed=0
IF ( setinitval .EQ. 3 ) grid%corral_dist=0
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
IF ( setinitval .EQ. 3 ) grid%max_dz=initial_data_value
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
IF ( setinitval .EQ. 3 ) grid%num_months=0
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
IF ( setinitval .EQ. 3 ) grid%co2tf=0
IF ( setinitval .EQ. 3 ) grid%ra_call_offset=0
IF ( setinitval .EQ. 3 ) grid%cam_abs_freq_s=initial_data_value
IF ( setinitval .EQ. 3 ) grid%levsiz=0
IF ( setinitval .EQ. 3 ) grid%paerlev=0
IF ( setinitval .EQ. 3 ) grid%cam_abs_dim1=0
IF ( setinitval .EQ. 3 ) grid%cam_abs_dim2=0
IF ( setinitval .EQ. 3 ) grid%cu_rad_feedback=.FALSE.
IF ( setinitval .EQ. 3 ) grid%fgdt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%grid_fdda=0
IF ( setinitval .EQ. 3 ) grid%if_no_pbl_nudging_uv=0
IF ( setinitval .EQ. 3 ) grid%if_no_pbl_nudging_t=0
IF ( setinitval .EQ. 3 ) grid%if_no_pbl_nudging_q=0
IF ( setinitval .EQ. 3 ) grid%if_zfac_uv=0
IF ( setinitval .EQ. 3 ) grid%k_zfac_uv=0
IF ( setinitval .EQ. 3 ) grid%if_zfac_t=0
IF ( setinitval .EQ. 3 ) grid%k_zfac_t=0
IF ( setinitval .EQ. 3 ) grid%if_zfac_q=0
IF ( setinitval .EQ. 3 ) grid%k_zfac_q=0
IF ( setinitval .EQ. 3 ) grid%guv=initial_data_value
IF ( setinitval .EQ. 3 ) grid%gt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%gq=initial_data_value
IF ( setinitval .EQ. 3 ) grid%dtramp_min=initial_data_value
IF ( setinitval .EQ. 3 ) grid%if_ramping=0
IF ( setinitval .EQ. 3 ) grid%obs_nudge_opt=0
IF ( setinitval .EQ. 3 ) grid%max_obs=0
IF ( setinitval .EQ. 3 ) grid%nobs_ndg_vars=0
IF ( setinitval .EQ. 3 ) grid%nobs_err_flds=0
IF ( setinitval .EQ. 3 ) grid%fdda_start=initial_data_value
IF ( setinitval .EQ. 3 ) grid%fdda_end=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_nudge_wind=0
IF ( setinitval .EQ. 3 ) grid%obs_coef_wind=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_nudge_temp=0
IF ( setinitval .EQ. 3 ) grid%obs_coef_temp=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_nudge_mois=0
IF ( setinitval .EQ. 3 ) grid%obs_coef_mois=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_nudge_pstr=0
IF ( setinitval .EQ. 3 ) grid%obs_coef_pstr=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_rinxy=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_rinsig=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_twindo=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_npfi=0
IF ( setinitval .EQ. 3 ) grid%obs_ionf=0
IF ( setinitval .EQ. 3 ) grid%obs_idynin=0
IF ( setinitval .EQ. 3 ) grid%obs_dtramp=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_ipf_in4dob=.FALSE.
IF ( setinitval .EQ. 3 ) grid%obs_ipf_errob=.FALSE.
IF ( setinitval .EQ. 3 ) grid%obs_ipf_nudob=.FALSE.
IF ( setinitval .EQ. 3 ) grid%dyn_opt=0
IF ( setinitval .EQ. 3 ) grid%rk_ord=0
IF ( setinitval .EQ. 3 ) grid%w_damping=0
IF ( setinitval .EQ. 3 ) grid%diff_opt=0
IF ( setinitval .EQ. 3 ) grid%km_opt=0
IF ( setinitval .EQ. 3 ) grid%damp_opt=0
IF ( setinitval .EQ. 3 ) grid%zdamp=initial_data_value
IF ( setinitval .EQ. 3 ) grid%dampcoef=initial_data_value
IF ( setinitval .EQ. 3 ) grid%khdif=initial_data_value
IF ( setinitval .EQ. 3 ) grid%kvdif=initial_data_value
IF ( setinitval .EQ. 3 ) grid%diff_6th_factor=initial_data_value
IF ( setinitval .EQ. 3 ) grid%diff_6th_opt=0
IF ( setinitval .EQ. 3 ) grid%smdiv=initial_data_value
IF ( setinitval .EQ. 3 ) grid%emdiv=initial_data_value
IF ( setinitval .EQ. 3 ) grid%epssm=initial_data_value
IF ( setinitval .EQ. 3 ) grid%non_hydrostatic=.FALSE.
IF ( setinitval .EQ. 3 ) grid%time_step_sound=0
IF ( setinitval .EQ. 3 ) grid%h_mom_adv_order=0
IF ( setinitval .EQ. 3 ) grid%v_mom_adv_order=0
IF ( setinitval .EQ. 3 ) grid%h_sca_adv_order=0
IF ( setinitval .EQ. 3 ) grid%v_sca_adv_order=0
IF ( setinitval .EQ. 3 ) grid%pd_moist=.FALSE.
IF ( setinitval .EQ. 3 ) grid%pd_chem=.FALSE.
IF ( setinitval .EQ. 3 ) grid%pd_scalar=.FALSE.
IF ( setinitval .EQ. 3 ) grid%pd_tke=.FALSE.
IF ( setinitval .EQ. 3 ) grid%top_radiation=.FALSE.
IF ( setinitval .EQ. 3 ) grid%mix_cr_len=initial_data_value
IF ( setinitval .EQ. 3 ) grid%tke_upper_bound=initial_data_value
IF ( setinitval .EQ. 3 ) grid%kh_tke_upper_bound=initial_data_value
IF ( setinitval .EQ. 3 ) grid%kv_tke_upper_bound=initial_data_value
IF ( setinitval .EQ. 3 ) grid%tke_drag_coefficient=initial_data_value
IF ( setinitval .EQ. 3 ) grid%tke_heat_flux=initial_data_value
IF ( setinitval .EQ. 3 ) grid%pert_coriolis=.FALSE.
IF ( setinitval .EQ. 3 ) grid%mix_full_fields=.FALSE.
IF ( setinitval .EQ. 3 ) grid%base_pres=initial_data_value
IF ( setinitval .EQ. 3 ) grid%base_temp=initial_data_value
IF ( setinitval .EQ. 3 ) grid%base_lapse=initial_data_value
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
IF ( setinitval .EQ. 3 ) grid%fdob%domain_tot=0
IF ( setinitval .EQ. 3 ) grid%fdob%domain_init=0
IF ( setinitval .EQ. 3 ) grid%fdob%ieodi=0
IF ( setinitval .EQ. 3 ) grid%fdob%iwtsig=0
IF ( setinitval .EQ. 3 ) grid%fdob%nstat=0
IF ( setinitval .EQ. 3 ) grid%fdob%ktaur=0
IF ( setinitval .EQ. 3 ) grid%fdob%sn_maxcg=0
IF ( setinitval .EQ. 3 ) grid%fdob%we_maxcg=0
IF ( setinitval .EQ. 3 ) grid%fdob%sn_end=0
IF ( setinitval .EQ. 3 ) grid%fdob%levidn(max_domains)=0
IF ( setinitval .EQ. 3 ) grid%fdob%ds_cg=initial_data_value
IF ( setinitval .EQ. 3 ) grid%fdob%window=initial_data_value
IF ( setinitval .EQ. 3 ) grid%fdob%rtlast=initial_data_value
IF ( setinitval .EQ. 3 ) grid%fdob%datend=initial_data_value
IF ( setinitval .EQ. 3 ) grid%fdob%rinfmn=initial_data_value
IF ( setinitval .EQ. 3 ) grid%fdob%rinfmx=initial_data_value
IF ( setinitval .EQ. 3 ) grid%fdob%pfree=initial_data_value
IF ( setinitval .EQ. 3 ) grid%fdob%dcon=initial_data_value
IF ( setinitval .EQ. 3 ) grid%fdob%dpsmx=initial_data_value
IF ( setinitval .EQ. 3 ) grid%fdob%tfaci=initial_data_value
IF ( setinitval .EQ. 3 ) grid%fdob%xn=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%fdob%varobs(1:model_config_rec%nobs_ndg_vars,1:model_config_rec%max_obs),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fdob%varobs(1:model_config_rec%nobs_ndg_vars,1:model_config_rec%max_obs). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fdob%varobs=initial_data_value
ELSE
ALLOCATE(grid%fdob%varobs(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fdob%varobs(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%fdob%errf(1:model_config_rec%nobs_err_flds,1:model_config_rec%max_obs),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fdob%errf(1:model_config_rec%nobs_err_flds,1:model_config_rec%max_obs). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fdob%errf=initial_data_value
ELSE
ALLOCATE(grid%fdob%errf(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fdob%errf(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%fdob%timeob(1:model_config_rec%max_obs),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fdob%timeob(1:model_config_rec%max_obs). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fdob%timeob=initial_data_value
ELSE
ALLOCATE(grid%fdob%timeob(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fdob%timeob(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%fdob%nlevs_ob(1:model_config_rec%max_obs),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fdob%nlevs_ob(1:model_config_rec%max_obs). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fdob%nlevs_ob=initial_data_value
ELSE
ALLOCATE(grid%fdob%nlevs_ob(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fdob%nlevs_ob(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%fdob%lev_in_ob(1:model_config_rec%max_obs),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fdob%lev_in_ob(1:model_config_rec%max_obs). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fdob%lev_in_ob=initial_data_value
ELSE
ALLOCATE(grid%fdob%lev_in_ob(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fdob%lev_in_ob(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%fdob%plfo(1:model_config_rec%max_obs),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fdob%plfo(1:model_config_rec%max_obs). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fdob%plfo=initial_data_value
ELSE
ALLOCATE(grid%fdob%plfo(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fdob%plfo(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%fdob%elevob(1:model_config_rec%max_obs),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fdob%elevob(1:model_config_rec%max_obs). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fdob%elevob=initial_data_value
ELSE
ALLOCATE(grid%fdob%elevob(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fdob%elevob(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%fdob%rio(1:model_config_rec%max_obs),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fdob%rio(1:model_config_rec%max_obs). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fdob%rio=initial_data_value
ELSE
ALLOCATE(grid%fdob%rio(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fdob%rio(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%fdob%rjo(1:model_config_rec%max_obs),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fdob%rjo(1:model_config_rec%max_obs). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fdob%rjo=initial_data_value
ELSE
ALLOCATE(grid%fdob%rjo(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fdob%rjo(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%fdob%rko(1:model_config_rec%max_obs),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fdob%rko(1:model_config_rec%max_obs). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fdob%rko=initial_data_value
ELSE
ALLOCATE(grid%fdob%rko(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fdob%rko(1).  ')
 endif
ENDIF
!ENDOFREGISTRYGENERATEDINCLUDE

      ELSE
      
        WRITE( wrf_err_message , * )&
          'Invalid specification of dynamics: dyn_opt = ',dyn_opt
        CALL wrf_error_fatal3 ( "module_domain.b" , 1142 ,  TRIM ( wrf_err_message ) )
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
         CALL wrf_error_fatal3 ( "module_domain.b" , 1194 ,  TRIM( wrf_err_message ) ) 
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
            CALL wrf_error_fatal3 ( "module_domain.b" , 1247 ,  'show_nest_subtree: nest hierarchy corrupted' )
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

      ELSE IF ( dyn_opt == DYN_EM ) THEN
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/em_deallocs.inc'
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
IF ( ASSOCIATED( grid%em_u_gc ) ) THEN 
  DEALLOCATE(grid%em_u_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_u_gc. ')
 endif
  NULLIFY(grid%em_u_gc)
ENDIF
IF ( ASSOCIATED( grid%em_v_gc ) ) THEN 
  DEALLOCATE(grid%em_v_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_v_gc. ')
 endif
  NULLIFY(grid%em_v_gc)
ENDIF
IF ( ASSOCIATED( grid%em_t_gc ) ) THEN 
  DEALLOCATE(grid%em_t_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_t_gc. ')
 endif
  NULLIFY(grid%em_t_gc)
ENDIF
IF ( ASSOCIATED( grid%em_rh_gc ) ) THEN 
  DEALLOCATE(grid%em_rh_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_rh_gc. ')
 endif
  NULLIFY(grid%em_rh_gc)
ENDIF
IF ( ASSOCIATED( grid%em_ght_gc ) ) THEN 
  DEALLOCATE(grid%em_ght_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_ght_gc. ')
 endif
  NULLIFY(grid%em_ght_gc)
ENDIF
IF ( ASSOCIATED( grid%em_p_gc ) ) THEN 
  DEALLOCATE(grid%em_p_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_p_gc. ')
 endif
  NULLIFY(grid%em_p_gc)
ENDIF
IF ( ASSOCIATED( grid%em_xlat_gc ) ) THEN 
  DEALLOCATE(grid%em_xlat_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_xlat_gc. ')
 endif
  NULLIFY(grid%em_xlat_gc)
ENDIF
IF ( ASSOCIATED( grid%em_xlong_gc ) ) THEN 
  DEALLOCATE(grid%em_xlong_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_xlong_gc. ')
 endif
  NULLIFY(grid%em_xlong_gc)
ENDIF
IF ( ASSOCIATED( grid%em_ht_gc ) ) THEN 
  DEALLOCATE(grid%em_ht_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_ht_gc. ')
 endif
  NULLIFY(grid%em_ht_gc)
ENDIF
IF ( ASSOCIATED( grid%em_tsk_gc ) ) THEN 
  DEALLOCATE(grid%em_tsk_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_tsk_gc. ')
 endif
  NULLIFY(grid%em_tsk_gc)
ENDIF
IF ( ASSOCIATED( grid%em_tavgsfc ) ) THEN 
  DEALLOCATE(grid%em_tavgsfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_tavgsfc. ')
 endif
  NULLIFY(grid%em_tavgsfc)
ENDIF
IF ( ASSOCIATED( grid%em_tmn_gc ) ) THEN 
  DEALLOCATE(grid%em_tmn_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_tmn_gc. ')
 endif
  NULLIFY(grid%em_tmn_gc)
ENDIF
IF ( ASSOCIATED( grid%em_pslv_gc ) ) THEN 
  DEALLOCATE(grid%em_pslv_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_pslv_gc. ')
 endif
  NULLIFY(grid%em_pslv_gc)
ENDIF
IF ( ASSOCIATED( grid%em_greenfrac ) ) THEN 
  DEALLOCATE(grid%em_greenfrac,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_greenfrac. ')
 endif
  NULLIFY(grid%em_greenfrac)
ENDIF
IF ( ASSOCIATED( grid%em_albedo12m ) ) THEN 
  DEALLOCATE(grid%em_albedo12m,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_albedo12m. ')
 endif
  NULLIFY(grid%em_albedo12m)
ENDIF
IF ( ASSOCIATED( grid%em_pd_gc ) ) THEN 
  DEALLOCATE(grid%em_pd_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_pd_gc. ')
 endif
  NULLIFY(grid%em_pd_gc)
ENDIF
IF ( ASSOCIATED( grid%em_psfc_gc ) ) THEN 
  DEALLOCATE(grid%em_psfc_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_psfc_gc. ')
 endif
  NULLIFY(grid%em_psfc_gc)
ENDIF
IF ( ASSOCIATED( grid%em_intq_gc ) ) THEN 
  DEALLOCATE(grid%em_intq_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_intq_gc. ')
 endif
  NULLIFY(grid%em_intq_gc)
ENDIF
IF ( ASSOCIATED( grid%em_pdhs ) ) THEN 
  DEALLOCATE(grid%em_pdhs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_pdhs. ')
 endif
  NULLIFY(grid%em_pdhs)
ENDIF
IF ( ASSOCIATED( grid%em_qv_gc ) ) THEN 
  DEALLOCATE(grid%em_qv_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_qv_gc. ')
 endif
  NULLIFY(grid%em_qv_gc)
ENDIF
IF ( ASSOCIATED( grid%em_u_1 ) ) THEN 
  DEALLOCATE(grid%em_u_1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_u_1. ')
 endif
  NULLIFY(grid%em_u_1)
ENDIF
IF ( ASSOCIATED( grid%em_u_2 ) ) THEN 
  DEALLOCATE(grid%em_u_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_u_2. ')
 endif
  NULLIFY(grid%em_u_2)
ENDIF
IF ( ASSOCIATED( grid%em_u_b ) ) THEN 
  DEALLOCATE(grid%em_u_b,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_u_b. ')
 endif
  NULLIFY(grid%em_u_b)
ENDIF
IF ( ASSOCIATED( grid%em_u_bt ) ) THEN 
  DEALLOCATE(grid%em_u_bt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_u_bt. ')
 endif
  NULLIFY(grid%em_u_bt)
ENDIF
IF ( ASSOCIATED( grid%em_ru ) ) THEN 
  DEALLOCATE(grid%em_ru,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_ru. ')
 endif
  NULLIFY(grid%em_ru)
ENDIF
IF ( ASSOCIATED( grid%em_ru_m ) ) THEN 
  DEALLOCATE(grid%em_ru_m,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_ru_m. ')
 endif
  NULLIFY(grid%em_ru_m)
ENDIF
IF ( ASSOCIATED( grid%em_ru_tend ) ) THEN 
  DEALLOCATE(grid%em_ru_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_ru_tend. ')
 endif
  NULLIFY(grid%em_ru_tend)
ENDIF
IF ( ASSOCIATED( grid%em_u_save ) ) THEN 
  DEALLOCATE(grid%em_u_save,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_u_save. ')
 endif
  NULLIFY(grid%em_u_save)
ENDIF
IF ( ASSOCIATED( grid%em_v_1 ) ) THEN 
  DEALLOCATE(grid%em_v_1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_v_1. ')
 endif
  NULLIFY(grid%em_v_1)
ENDIF
IF ( ASSOCIATED( grid%em_v_2 ) ) THEN 
  DEALLOCATE(grid%em_v_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_v_2. ')
 endif
  NULLIFY(grid%em_v_2)
ENDIF
IF ( ASSOCIATED( grid%em_v_b ) ) THEN 
  DEALLOCATE(grid%em_v_b,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_v_b. ')
 endif
  NULLIFY(grid%em_v_b)
ENDIF
IF ( ASSOCIATED( grid%em_v_bt ) ) THEN 
  DEALLOCATE(grid%em_v_bt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_v_bt. ')
 endif
  NULLIFY(grid%em_v_bt)
ENDIF
IF ( ASSOCIATED( grid%em_rv ) ) THEN 
  DEALLOCATE(grid%em_rv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_rv. ')
 endif
  NULLIFY(grid%em_rv)
ENDIF
IF ( ASSOCIATED( grid%em_rv_m ) ) THEN 
  DEALLOCATE(grid%em_rv_m,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_rv_m. ')
 endif
  NULLIFY(grid%em_rv_m)
ENDIF
IF ( ASSOCIATED( grid%em_rv_tend ) ) THEN 
  DEALLOCATE(grid%em_rv_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_rv_tend. ')
 endif
  NULLIFY(grid%em_rv_tend)
ENDIF
IF ( ASSOCIATED( grid%em_v_save ) ) THEN 
  DEALLOCATE(grid%em_v_save,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_v_save. ')
 endif
  NULLIFY(grid%em_v_save)
ENDIF
IF ( ASSOCIATED( grid%em_w_1 ) ) THEN 
  DEALLOCATE(grid%em_w_1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_w_1. ')
 endif
  NULLIFY(grid%em_w_1)
ENDIF
IF ( ASSOCIATED( grid%em_w_2 ) ) THEN 
  DEALLOCATE(grid%em_w_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_w_2. ')
 endif
  NULLIFY(grid%em_w_2)
ENDIF
IF ( ASSOCIATED( grid%em_w_b ) ) THEN 
  DEALLOCATE(grid%em_w_b,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_w_b. ')
 endif
  NULLIFY(grid%em_w_b)
ENDIF
IF ( ASSOCIATED( grid%em_w_bt ) ) THEN 
  DEALLOCATE(grid%em_w_bt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_w_bt. ')
 endif
  NULLIFY(grid%em_w_bt)
ENDIF
IF ( ASSOCIATED( grid%em_ww ) ) THEN 
  DEALLOCATE(grid%em_ww,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_ww. ')
 endif
  NULLIFY(grid%em_ww)
ENDIF
IF ( ASSOCIATED( grid%em_rw ) ) THEN 
  DEALLOCATE(grid%em_rw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_rw. ')
 endif
  NULLIFY(grid%em_rw)
ENDIF
IF ( ASSOCIATED( grid%em_ww_m ) ) THEN 
  DEALLOCATE(grid%em_ww_m,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_ww_m. ')
 endif
  NULLIFY(grid%em_ww_m)
ENDIF
IF ( ASSOCIATED( grid%em_ph_1 ) ) THEN 
  DEALLOCATE(grid%em_ph_1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_ph_1. ')
 endif
  NULLIFY(grid%em_ph_1)
ENDIF
IF ( ASSOCIATED( grid%em_ph_2 ) ) THEN 
  DEALLOCATE(grid%em_ph_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_ph_2. ')
 endif
  NULLIFY(grid%em_ph_2)
ENDIF
IF ( ASSOCIATED( grid%em_ph_b ) ) THEN 
  DEALLOCATE(grid%em_ph_b,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_ph_b. ')
 endif
  NULLIFY(grid%em_ph_b)
ENDIF
IF ( ASSOCIATED( grid%em_ph_bt ) ) THEN 
  DEALLOCATE(grid%em_ph_bt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_ph_bt. ')
 endif
  NULLIFY(grid%em_ph_bt)
ENDIF
IF ( ASSOCIATED( grid%em_phb ) ) THEN 
  DEALLOCATE(grid%em_phb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_phb. ')
 endif
  NULLIFY(grid%em_phb)
ENDIF
IF ( ASSOCIATED( grid%em_phb_fine ) ) THEN 
  DEALLOCATE(grid%em_phb_fine,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_phb_fine. ')
 endif
  NULLIFY(grid%em_phb_fine)
ENDIF
IF ( ASSOCIATED( grid%em_ph0 ) ) THEN 
  DEALLOCATE(grid%em_ph0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_ph0. ')
 endif
  NULLIFY(grid%em_ph0)
ENDIF
IF ( ASSOCIATED( grid%em_php ) ) THEN 
  DEALLOCATE(grid%em_php,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_php. ')
 endif
  NULLIFY(grid%em_php)
ENDIF
IF ( ASSOCIATED( grid%em_t_1 ) ) THEN 
  DEALLOCATE(grid%em_t_1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_t_1. ')
 endif
  NULLIFY(grid%em_t_1)
ENDIF
IF ( ASSOCIATED( grid%em_t_2 ) ) THEN 
  DEALLOCATE(grid%em_t_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_t_2. ')
 endif
  NULLIFY(grid%em_t_2)
ENDIF
IF ( ASSOCIATED( grid%em_t_b ) ) THEN 
  DEALLOCATE(grid%em_t_b,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_t_b. ')
 endif
  NULLIFY(grid%em_t_b)
ENDIF
IF ( ASSOCIATED( grid%em_t_bt ) ) THEN 
  DEALLOCATE(grid%em_t_bt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_t_bt. ')
 endif
  NULLIFY(grid%em_t_bt)
ENDIF
IF ( ASSOCIATED( grid%em_t_init ) ) THEN 
  DEALLOCATE(grid%em_t_init,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_t_init. ')
 endif
  NULLIFY(grid%em_t_init)
ENDIF
IF ( ASSOCIATED( grid%em_tp_1 ) ) THEN 
  DEALLOCATE(grid%em_tp_1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_tp_1. ')
 endif
  NULLIFY(grid%em_tp_1)
ENDIF
IF ( ASSOCIATED( grid%em_tp_2 ) ) THEN 
  DEALLOCATE(grid%em_tp_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_tp_2. ')
 endif
  NULLIFY(grid%em_tp_2)
ENDIF
IF ( ASSOCIATED( grid%em_t_save ) ) THEN 
  DEALLOCATE(grid%em_t_save,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_t_save. ')
 endif
  NULLIFY(grid%em_t_save)
ENDIF
IF ( ASSOCIATED( grid%em_mu_1 ) ) THEN 
  DEALLOCATE(grid%em_mu_1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_mu_1. ')
 endif
  NULLIFY(grid%em_mu_1)
ENDIF
IF ( ASSOCIATED( grid%em_mu_2 ) ) THEN 
  DEALLOCATE(grid%em_mu_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_mu_2. ')
 endif
  NULLIFY(grid%em_mu_2)
ENDIF
IF ( ASSOCIATED( grid%em_mu_b ) ) THEN 
  DEALLOCATE(grid%em_mu_b,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_mu_b. ')
 endif
  NULLIFY(grid%em_mu_b)
ENDIF
IF ( ASSOCIATED( grid%em_mu_bt ) ) THEN 
  DEALLOCATE(grid%em_mu_bt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_mu_bt. ')
 endif
  NULLIFY(grid%em_mu_bt)
ENDIF
IF ( ASSOCIATED( grid%em_mub ) ) THEN 
  DEALLOCATE(grid%em_mub,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_mub. ')
 endif
  NULLIFY(grid%em_mub)
ENDIF
IF ( ASSOCIATED( grid%em_mub_fine ) ) THEN 
  DEALLOCATE(grid%em_mub_fine,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_mub_fine. ')
 endif
  NULLIFY(grid%em_mub_fine)
ENDIF
IF ( ASSOCIATED( grid%em_mu0 ) ) THEN 
  DEALLOCATE(grid%em_mu0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_mu0. ')
 endif
  NULLIFY(grid%em_mu0)
ENDIF
IF ( ASSOCIATED( grid%em_mudf ) ) THEN 
  DEALLOCATE(grid%em_mudf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_mudf. ')
 endif
  NULLIFY(grid%em_mudf)
ENDIF
IF ( ASSOCIATED( grid%em_muu ) ) THEN 
  DEALLOCATE(grid%em_muu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_muu. ')
 endif
  NULLIFY(grid%em_muu)
ENDIF
IF ( ASSOCIATED( grid%em_muv ) ) THEN 
  DEALLOCATE(grid%em_muv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_muv. ')
 endif
  NULLIFY(grid%em_muv)
ENDIF
IF ( ASSOCIATED( grid%em_mut ) ) THEN 
  DEALLOCATE(grid%em_mut,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_mut. ')
 endif
  NULLIFY(grid%em_mut)
ENDIF
IF ( ASSOCIATED( grid%em_muts ) ) THEN 
  DEALLOCATE(grid%em_muts,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_muts. ')
 endif
  NULLIFY(grid%em_muts)
ENDIF
IF ( ASSOCIATED( grid%nest_pos ) ) THEN 
  DEALLOCATE(grid%nest_pos,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nest_pos. ')
 endif
  NULLIFY(grid%nest_pos)
ENDIF
IF ( ASSOCIATED( grid%nest_mask ) ) THEN 
  DEALLOCATE(grid%nest_mask,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nest_mask. ')
 endif
  NULLIFY(grid%nest_mask)
ENDIF
IF ( ASSOCIATED( grid%ht_coarse ) ) THEN 
  DEALLOCATE(grid%ht_coarse,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%ht_coarse. ')
 endif
  NULLIFY(grid%ht_coarse)
ENDIF
IF ( ASSOCIATED( grid%em_tke_1 ) ) THEN 
  DEALLOCATE(grid%em_tke_1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_tke_1. ')
 endif
  NULLIFY(grid%em_tke_1)
ENDIF
IF ( ASSOCIATED( grid%em_tke_2 ) ) THEN 
  DEALLOCATE(grid%em_tke_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_tke_2. ')
 endif
  NULLIFY(grid%em_tke_2)
ENDIF
IF ( ASSOCIATED( grid%em_p ) ) THEN 
  DEALLOCATE(grid%em_p,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_p. ')
 endif
  NULLIFY(grid%em_p)
ENDIF
IF ( ASSOCIATED( grid%em_al ) ) THEN 
  DEALLOCATE(grid%em_al,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_al. ')
 endif
  NULLIFY(grid%em_al)
ENDIF
IF ( ASSOCIATED( grid%em_alt ) ) THEN 
  DEALLOCATE(grid%em_alt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_alt. ')
 endif
  NULLIFY(grid%em_alt)
ENDIF
IF ( ASSOCIATED( grid%em_alb ) ) THEN 
  DEALLOCATE(grid%em_alb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_alb. ')
 endif
  NULLIFY(grid%em_alb)
ENDIF
IF ( ASSOCIATED( grid%em_zx ) ) THEN 
  DEALLOCATE(grid%em_zx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_zx. ')
 endif
  NULLIFY(grid%em_zx)
ENDIF
IF ( ASSOCIATED( grid%em_zy ) ) THEN 
  DEALLOCATE(grid%em_zy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_zy. ')
 endif
  NULLIFY(grid%em_zy)
ENDIF
IF ( ASSOCIATED( grid%em_rdz ) ) THEN 
  DEALLOCATE(grid%em_rdz,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_rdz. ')
 endif
  NULLIFY(grid%em_rdz)
ENDIF
IF ( ASSOCIATED( grid%em_rdzw ) ) THEN 
  DEALLOCATE(grid%em_rdzw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_rdzw. ')
 endif
  NULLIFY(grid%em_rdzw)
ENDIF
IF ( ASSOCIATED( grid%em_pb ) ) THEN 
  DEALLOCATE(grid%em_pb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_pb. ')
 endif
  NULLIFY(grid%em_pb)
ENDIF
IF ( ASSOCIATED( grid%em_sr ) ) THEN 
  DEALLOCATE(grid%em_sr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_sr. ')
 endif
  NULLIFY(grid%em_sr)
ENDIF
IF ( ASSOCIATED( grid%em_potevp ) ) THEN 
  DEALLOCATE(grid%em_potevp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_potevp. ')
 endif
  NULLIFY(grid%em_potevp)
ENDIF
IF ( ASSOCIATED( grid%em_snopcx ) ) THEN 
  DEALLOCATE(grid%em_snopcx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_snopcx. ')
 endif
  NULLIFY(grid%em_snopcx)
ENDIF
IF ( ASSOCIATED( grid%em_soiltb ) ) THEN 
  DEALLOCATE(grid%em_soiltb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_soiltb. ')
 endif
  NULLIFY(grid%em_soiltb)
ENDIF
IF ( ASSOCIATED( grid%em_fnm ) ) THEN 
  DEALLOCATE(grid%em_fnm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_fnm. ')
 endif
  NULLIFY(grid%em_fnm)
ENDIF
IF ( ASSOCIATED( grid%em_fnp ) ) THEN 
  DEALLOCATE(grid%em_fnp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_fnp. ')
 endif
  NULLIFY(grid%em_fnp)
ENDIF
IF ( ASSOCIATED( grid%em_rdnw ) ) THEN 
  DEALLOCATE(grid%em_rdnw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_rdnw. ')
 endif
  NULLIFY(grid%em_rdnw)
ENDIF
IF ( ASSOCIATED( grid%em_rdn ) ) THEN 
  DEALLOCATE(grid%em_rdn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_rdn. ')
 endif
  NULLIFY(grid%em_rdn)
ENDIF
IF ( ASSOCIATED( grid%em_dnw ) ) THEN 
  DEALLOCATE(grid%em_dnw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_dnw. ')
 endif
  NULLIFY(grid%em_dnw)
ENDIF
IF ( ASSOCIATED( grid%em_dn ) ) THEN 
  DEALLOCATE(grid%em_dn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_dn. ')
 endif
  NULLIFY(grid%em_dn)
ENDIF
IF ( ASSOCIATED( grid%em_znu ) ) THEN 
  DEALLOCATE(grid%em_znu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_znu. ')
 endif
  NULLIFY(grid%em_znu)
ENDIF
IF ( ASSOCIATED( grid%em_znw ) ) THEN 
  DEALLOCATE(grid%em_znw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_znw. ')
 endif
  NULLIFY(grid%em_znw)
ENDIF
IF ( ASSOCIATED( grid%em_t_base ) ) THEN 
  DEALLOCATE(grid%em_t_base,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_t_base. ')
 endif
  NULLIFY(grid%em_t_base)
ENDIF
IF ( ASSOCIATED( grid%em_z ) ) THEN 
  DEALLOCATE(grid%em_z,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_z. ')
 endif
  NULLIFY(grid%em_z)
ENDIF
IF ( ASSOCIATED( grid%q2 ) ) THEN 
  DEALLOCATE(grid%q2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%q2. ')
 endif
  NULLIFY(grid%q2)
ENDIF
IF ( ASSOCIATED( grid%t2 ) ) THEN 
  DEALLOCATE(grid%t2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%t2. ')
 endif
  NULLIFY(grid%t2)
ENDIF
IF ( ASSOCIATED( grid%th2 ) ) THEN 
  DEALLOCATE(grid%th2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%th2. ')
 endif
  NULLIFY(grid%th2)
ENDIF
IF ( ASSOCIATED( grid%psfc ) ) THEN 
  DEALLOCATE(grid%psfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%psfc. ')
 endif
  NULLIFY(grid%psfc)
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
IF ( ASSOCIATED( grid%uratx ) ) THEN 
  DEALLOCATE(grid%uratx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%uratx. ')
 endif
  NULLIFY(grid%uratx)
ENDIF
IF ( ASSOCIATED( grid%vratx ) ) THEN 
  DEALLOCATE(grid%vratx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%vratx. ')
 endif
  NULLIFY(grid%vratx)
ENDIF
IF ( ASSOCIATED( grid%tratx ) ) THEN 
  DEALLOCATE(grid%tratx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%tratx. ')
 endif
  NULLIFY(grid%tratx)
ENDIF
IF ( ASSOCIATED( grid%em_obs_savwt ) ) THEN 
  DEALLOCATE(grid%em_obs_savwt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_obs_savwt. ')
 endif
  NULLIFY(grid%em_obs_savwt)
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
IF ( ASSOCIATED( grid%moist ) ) THEN 
  DEALLOCATE(grid%moist,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%moist. ')
 endif
  NULLIFY(grid%moist)
ENDIF
IF ( ASSOCIATED( grid%moist_b ) ) THEN 
  DEALLOCATE(grid%moist_b,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%moist_b. ')
 endif
  NULLIFY(grid%moist_b)
ENDIF
IF ( ASSOCIATED( grid%moist_bt ) ) THEN 
  DEALLOCATE(grid%moist_bt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%moist_bt. ')
 endif
  NULLIFY(grid%moist_bt)
ENDIF
IF ( ASSOCIATED( grid%chem ) ) THEN 
  DEALLOCATE(grid%chem,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%chem. ')
 endif
  NULLIFY(grid%chem)
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
IF ( ASSOCIATED( grid%fcx ) ) THEN 
  DEALLOCATE(grid%fcx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%fcx. ')
 endif
  NULLIFY(grid%fcx)
ENDIF
IF ( ASSOCIATED( grid%gcx ) ) THEN 
  DEALLOCATE(grid%gcx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%gcx. ')
 endif
  NULLIFY(grid%gcx)
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
IF ( ASSOCIATED( grid%shdmax ) ) THEN 
  DEALLOCATE(grid%shdmax,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%shdmax. ')
 endif
  NULLIFY(grid%shdmax)
ENDIF
IF ( ASSOCIATED( grid%shdmin ) ) THEN 
  DEALLOCATE(grid%shdmin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%shdmin. ')
 endif
  NULLIFY(grid%shdmin)
ENDIF
IF ( ASSOCIATED( grid%snoalb ) ) THEN 
  DEALLOCATE(grid%snoalb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%snoalb. ')
 endif
  NULLIFY(grid%snoalb)
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
IF ( ASSOCIATED( grid%soilcat ) ) THEN 
  DEALLOCATE(grid%soilcat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilcat. ')
 endif
  NULLIFY(grid%soilcat)
ENDIF
IF ( ASSOCIATED( grid%vegcat ) ) THEN 
  DEALLOCATE(grid%vegcat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%vegcat. ')
 endif
  NULLIFY(grid%vegcat)
ENDIF
IF ( ASSOCIATED( grid%tslb ) ) THEN 
  DEALLOCATE(grid%tslb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%tslb. ')
 endif
  NULLIFY(grid%tslb)
ENDIF
IF ( ASSOCIATED( grid%zs ) ) THEN 
  DEALLOCATE(grid%zs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%zs. ')
 endif
  NULLIFY(grid%zs)
ENDIF
IF ( ASSOCIATED( grid%dzs ) ) THEN 
  DEALLOCATE(grid%dzs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dzs. ')
 endif
  NULLIFY(grid%dzs)
ENDIF
IF ( ASSOCIATED( grid%dzr ) ) THEN 
  DEALLOCATE(grid%dzr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dzr. ')
 endif
  NULLIFY(grid%dzr)
ENDIF
IF ( ASSOCIATED( grid%dzb ) ) THEN 
  DEALLOCATE(grid%dzb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dzb. ')
 endif
  NULLIFY(grid%dzb)
ENDIF
IF ( ASSOCIATED( grid%dzg ) ) THEN 
  DEALLOCATE(grid%dzg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dzg. ')
 endif
  NULLIFY(grid%dzg)
ENDIF
IF ( ASSOCIATED( grid%smois ) ) THEN 
  DEALLOCATE(grid%smois,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%smois. ')
 endif
  NULLIFY(grid%smois)
ENDIF
IF ( ASSOCIATED( grid%sh2o ) ) THEN 
  DEALLOCATE(grid%sh2o,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sh2o. ')
 endif
  NULLIFY(grid%sh2o)
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
IF ( ASSOCIATED( grid%snow ) ) THEN 
  DEALLOCATE(grid%snow,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%snow. ')
 endif
  NULLIFY(grid%snow)
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
IF ( ASSOCIATED( grid%tr_urb2d ) ) THEN 
  DEALLOCATE(grid%tr_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%tr_urb2d. ')
 endif
  NULLIFY(grid%tr_urb2d)
ENDIF
IF ( ASSOCIATED( grid%tb_urb2d ) ) THEN 
  DEALLOCATE(grid%tb_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%tb_urb2d. ')
 endif
  NULLIFY(grid%tb_urb2d)
ENDIF
IF ( ASSOCIATED( grid%tg_urb2d ) ) THEN 
  DEALLOCATE(grid%tg_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%tg_urb2d. ')
 endif
  NULLIFY(grid%tg_urb2d)
ENDIF
IF ( ASSOCIATED( grid%tc_urb2d ) ) THEN 
  DEALLOCATE(grid%tc_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%tc_urb2d. ')
 endif
  NULLIFY(grid%tc_urb2d)
ENDIF
IF ( ASSOCIATED( grid%qc_urb2d ) ) THEN 
  DEALLOCATE(grid%qc_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%qc_urb2d. ')
 endif
  NULLIFY(grid%qc_urb2d)
ENDIF
IF ( ASSOCIATED( grid%uc_urb2d ) ) THEN 
  DEALLOCATE(grid%uc_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%uc_urb2d. ')
 endif
  NULLIFY(grid%uc_urb2d)
ENDIF
IF ( ASSOCIATED( grid%xxxr_urb2d ) ) THEN 
  DEALLOCATE(grid%xxxr_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%xxxr_urb2d. ')
 endif
  NULLIFY(grid%xxxr_urb2d)
ENDIF
IF ( ASSOCIATED( grid%xxxb_urb2d ) ) THEN 
  DEALLOCATE(grid%xxxb_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%xxxb_urb2d. ')
 endif
  NULLIFY(grid%xxxb_urb2d)
ENDIF
IF ( ASSOCIATED( grid%xxxg_urb2d ) ) THEN 
  DEALLOCATE(grid%xxxg_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%xxxg_urb2d. ')
 endif
  NULLIFY(grid%xxxg_urb2d)
ENDIF
IF ( ASSOCIATED( grid%xxxc_urb2d ) ) THEN 
  DEALLOCATE(grid%xxxc_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%xxxc_urb2d. ')
 endif
  NULLIFY(grid%xxxc_urb2d)
ENDIF
IF ( ASSOCIATED( grid%trl_urb3d ) ) THEN 
  DEALLOCATE(grid%trl_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%trl_urb3d. ')
 endif
  NULLIFY(grid%trl_urb3d)
ENDIF
IF ( ASSOCIATED( grid%tbl_urb3d ) ) THEN 
  DEALLOCATE(grid%tbl_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%tbl_urb3d. ')
 endif
  NULLIFY(grid%tbl_urb3d)
ENDIF
IF ( ASSOCIATED( grid%tgl_urb3d ) ) THEN 
  DEALLOCATE(grid%tgl_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%tgl_urb3d. ')
 endif
  NULLIFY(grid%tgl_urb3d)
ENDIF
IF ( ASSOCIATED( grid%sh_urb2d ) ) THEN 
  DEALLOCATE(grid%sh_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sh_urb2d. ')
 endif
  NULLIFY(grid%sh_urb2d)
ENDIF
IF ( ASSOCIATED( grid%lh_urb2d ) ) THEN 
  DEALLOCATE(grid%lh_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%lh_urb2d. ')
 endif
  NULLIFY(grid%lh_urb2d)
ENDIF
IF ( ASSOCIATED( grid%g_urb2d ) ) THEN 
  DEALLOCATE(grid%g_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%g_urb2d. ')
 endif
  NULLIFY(grid%g_urb2d)
ENDIF
IF ( ASSOCIATED( grid%rn_urb2d ) ) THEN 
  DEALLOCATE(grid%rn_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rn_urb2d. ')
 endif
  NULLIFY(grid%rn_urb2d)
ENDIF
IF ( ASSOCIATED( grid%ts_urb2d ) ) THEN 
  DEALLOCATE(grid%ts_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%ts_urb2d. ')
 endif
  NULLIFY(grid%ts_urb2d)
ENDIF
IF ( ASSOCIATED( grid%frc_urb2d ) ) THEN 
  DEALLOCATE(grid%frc_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%frc_urb2d. ')
 endif
  NULLIFY(grid%frc_urb2d)
ENDIF
IF ( ASSOCIATED( grid%utype_urb2d ) ) THEN 
  DEALLOCATE(grid%utype_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%utype_urb2d. ')
 endif
  NULLIFY(grid%utype_urb2d)
ENDIF
IF ( ASSOCIATED( grid%cosz_urb2d ) ) THEN 
  DEALLOCATE(grid%cosz_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%cosz_urb2d. ')
 endif
  NULLIFY(grid%cosz_urb2d)
ENDIF
IF ( ASSOCIATED( grid%omg_urb2d ) ) THEN 
  DEALLOCATE(grid%omg_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%omg_urb2d. ')
 endif
  NULLIFY(grid%omg_urb2d)
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
IF ( ASSOCIATED( grid%ct ) ) THEN 
  DEALLOCATE(grid%ct,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%ct. ')
 endif
  NULLIFY(grid%ct)
ENDIF
IF ( ASSOCIATED( grid%thz0 ) ) THEN 
  DEALLOCATE(grid%thz0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%thz0. ')
 endif
  NULLIFY(grid%thz0)
ENDIF
IF ( ASSOCIATED( grid%z0 ) ) THEN 
  DEALLOCATE(grid%z0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%z0. ')
 endif
  NULLIFY(grid%z0)
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
IF ( ASSOCIATED( grid%kpbl ) ) THEN 
  DEALLOCATE(grid%kpbl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%kpbl. ')
 endif
  NULLIFY(grid%kpbl)
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
IF ( ASSOCIATED( grid%cutop ) ) THEN 
  DEALLOCATE(grid%cutop,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%cutop. ')
 endif
  NULLIFY(grid%cutop)
ENDIF
IF ( ASSOCIATED( grid%cubot ) ) THEN 
  DEALLOCATE(grid%cubot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%cubot. ')
 endif
  NULLIFY(grid%cubot)
ENDIF
IF ( ASSOCIATED( grid%cuppt ) ) THEN 
  DEALLOCATE(grid%cuppt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%cuppt. ')
 endif
  NULLIFY(grid%cuppt)
ENDIF
IF ( ASSOCIATED( grid%rswtoa ) ) THEN 
  DEALLOCATE(grid%rswtoa,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rswtoa. ')
 endif
  NULLIFY(grid%rswtoa)
ENDIF
IF ( ASSOCIATED( grid%rlwtoa ) ) THEN 
  DEALLOCATE(grid%rlwtoa,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rlwtoa. ')
 endif
  NULLIFY(grid%rlwtoa)
ENDIF
IF ( ASSOCIATED( grid%czmean ) ) THEN 
  DEALLOCATE(grid%czmean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%czmean. ')
 endif
  NULLIFY(grid%czmean)
ENDIF
IF ( ASSOCIATED( grid%cfracl ) ) THEN 
  DEALLOCATE(grid%cfracl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%cfracl. ')
 endif
  NULLIFY(grid%cfracl)
ENDIF
IF ( ASSOCIATED( grid%cfracm ) ) THEN 
  DEALLOCATE(grid%cfracm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%cfracm. ')
 endif
  NULLIFY(grid%cfracm)
ENDIF
IF ( ASSOCIATED( grid%cfrach ) ) THEN 
  DEALLOCATE(grid%cfrach,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%cfrach. ')
 endif
  NULLIFY(grid%cfrach)
ENDIF
IF ( ASSOCIATED( grid%acfrst ) ) THEN 
  DEALLOCATE(grid%acfrst,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%acfrst. ')
 endif
  NULLIFY(grid%acfrst)
ENDIF
IF ( ASSOCIATED( grid%ncfrst ) ) THEN 
  DEALLOCATE(grid%ncfrst,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%ncfrst. ')
 endif
  NULLIFY(grid%ncfrst)
ENDIF
IF ( ASSOCIATED( grid%acfrcv ) ) THEN 
  DEALLOCATE(grid%acfrcv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%acfrcv. ')
 endif
  NULLIFY(grid%acfrcv)
ENDIF
IF ( ASSOCIATED( grid%ncfrcv ) ) THEN 
  DEALLOCATE(grid%ncfrcv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%ncfrcv. ')
 endif
  NULLIFY(grid%ncfrcv)
ENDIF
IF ( ASSOCIATED( grid%ozmixm ) ) THEN 
  DEALLOCATE(grid%ozmixm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%ozmixm. ')
 endif
  NULLIFY(grid%ozmixm)
ENDIF
IF ( ASSOCIATED( grid%pin ) ) THEN 
  DEALLOCATE(grid%pin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%pin. ')
 endif
  NULLIFY(grid%pin)
ENDIF
IF ( ASSOCIATED( grid%m_ps_1 ) ) THEN 
  DEALLOCATE(grid%m_ps_1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%m_ps_1. ')
 endif
  NULLIFY(grid%m_ps_1)
ENDIF
IF ( ASSOCIATED( grid%m_ps_2 ) ) THEN 
  DEALLOCATE(grid%m_ps_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%m_ps_2. ')
 endif
  NULLIFY(grid%m_ps_2)
ENDIF
IF ( ASSOCIATED( grid%aerosolc_1 ) ) THEN 
  DEALLOCATE(grid%aerosolc_1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%aerosolc_1. ')
 endif
  NULLIFY(grid%aerosolc_1)
ENDIF
IF ( ASSOCIATED( grid%aerosolc_2 ) ) THEN 
  DEALLOCATE(grid%aerosolc_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%aerosolc_2. ')
 endif
  NULLIFY(grid%aerosolc_2)
ENDIF
IF ( ASSOCIATED( grid%m_hybi ) ) THEN 
  DEALLOCATE(grid%m_hybi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%m_hybi. ')
 endif
  NULLIFY(grid%m_hybi)
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
IF ( ASSOCIATED( grid%h_diabatic ) ) THEN 
  DEALLOCATE(grid%h_diabatic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%h_diabatic. ')
 endif
  NULLIFY(grid%h_diabatic)
ENDIF
IF ( ASSOCIATED( grid%msft ) ) THEN 
  DEALLOCATE(grid%msft,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%msft. ')
 endif
  NULLIFY(grid%msft)
ENDIF
IF ( ASSOCIATED( grid%msfu ) ) THEN 
  DEALLOCATE(grid%msfu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%msfu. ')
 endif
  NULLIFY(grid%msfu)
ENDIF
IF ( ASSOCIATED( grid%msfv ) ) THEN 
  DEALLOCATE(grid%msfv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%msfv. ')
 endif
  NULLIFY(grid%msfv)
ENDIF
IF ( ASSOCIATED( grid%f ) ) THEN 
  DEALLOCATE(grid%f,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%f. ')
 endif
  NULLIFY(grid%f)
ENDIF
IF ( ASSOCIATED( grid%e ) ) THEN 
  DEALLOCATE(grid%e,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%e. ')
 endif
  NULLIFY(grid%e)
ENDIF
IF ( ASSOCIATED( grid%sina ) ) THEN 
  DEALLOCATE(grid%sina,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sina. ')
 endif
  NULLIFY(grid%sina)
ENDIF
IF ( ASSOCIATED( grid%cosa ) ) THEN 
  DEALLOCATE(grid%cosa,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%cosa. ')
 endif
  NULLIFY(grid%cosa)
ENDIF
IF ( ASSOCIATED( grid%ht ) ) THEN 
  DEALLOCATE(grid%ht,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%ht. ')
 endif
  NULLIFY(grid%ht)
ENDIF
IF ( ASSOCIATED( grid%ht_fine ) ) THEN 
  DEALLOCATE(grid%ht_fine,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%ht_fine. ')
 endif
  NULLIFY(grid%ht_fine)
ENDIF
IF ( ASSOCIATED( grid%ht_int ) ) THEN 
  DEALLOCATE(grid%ht_int,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%ht_int. ')
 endif
  NULLIFY(grid%ht_int)
ENDIF
IF ( ASSOCIATED( grid%ht_input ) ) THEN 
  DEALLOCATE(grid%ht_input,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%ht_input. ')
 endif
  NULLIFY(grid%ht_input)
ENDIF
IF ( ASSOCIATED( grid%tsk ) ) THEN 
  DEALLOCATE(grid%tsk,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%tsk. ')
 endif
  NULLIFY(grid%tsk)
ENDIF
IF ( ASSOCIATED( grid%tsk_save ) ) THEN 
  DEALLOCATE(grid%tsk_save,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%tsk_save. ')
 endif
  NULLIFY(grid%tsk_save)
ENDIF
IF ( ASSOCIATED( grid%u_base ) ) THEN 
  DEALLOCATE(grid%u_base,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%u_base. ')
 endif
  NULLIFY(grid%u_base)
ENDIF
IF ( ASSOCIATED( grid%v_base ) ) THEN 
  DEALLOCATE(grid%v_base,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%v_base. ')
 endif
  NULLIFY(grid%v_base)
ENDIF
IF ( ASSOCIATED( grid%qv_base ) ) THEN 
  DEALLOCATE(grid%qv_base,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%qv_base. ')
 endif
  NULLIFY(grid%qv_base)
ENDIF
IF ( ASSOCIATED( grid%z_base ) ) THEN 
  DEALLOCATE(grid%z_base,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%z_base. ')
 endif
  NULLIFY(grid%z_base)
ENDIF
IF ( ASSOCIATED( grid%rthcuten ) ) THEN 
  DEALLOCATE(grid%rthcuten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rthcuten. ')
 endif
  NULLIFY(grid%rthcuten)
ENDIF
IF ( ASSOCIATED( grid%rqvcuten ) ) THEN 
  DEALLOCATE(grid%rqvcuten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rqvcuten. ')
 endif
  NULLIFY(grid%rqvcuten)
ENDIF
IF ( ASSOCIATED( grid%rqrcuten ) ) THEN 
  DEALLOCATE(grid%rqrcuten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rqrcuten. ')
 endif
  NULLIFY(grid%rqrcuten)
ENDIF
IF ( ASSOCIATED( grid%rqccuten ) ) THEN 
  DEALLOCATE(grid%rqccuten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rqccuten. ')
 endif
  NULLIFY(grid%rqccuten)
ENDIF
IF ( ASSOCIATED( grid%rqscuten ) ) THEN 
  DEALLOCATE(grid%rqscuten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rqscuten. ')
 endif
  NULLIFY(grid%rqscuten)
ENDIF
IF ( ASSOCIATED( grid%rqicuten ) ) THEN 
  DEALLOCATE(grid%rqicuten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rqicuten. ')
 endif
  NULLIFY(grid%rqicuten)
ENDIF
IF ( ASSOCIATED( grid%w0avg ) ) THEN 
  DEALLOCATE(grid%w0avg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%w0avg. ')
 endif
  NULLIFY(grid%w0avg)
ENDIF
IF ( ASSOCIATED( grid%rainc ) ) THEN 
  DEALLOCATE(grid%rainc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rainc. ')
 endif
  NULLIFY(grid%rainc)
ENDIF
IF ( ASSOCIATED( grid%rainnc ) ) THEN 
  DEALLOCATE(grid%rainnc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rainnc. ')
 endif
  NULLIFY(grid%rainnc)
ENDIF
IF ( ASSOCIATED( grid%raincv ) ) THEN 
  DEALLOCATE(grid%raincv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%raincv. ')
 endif
  NULLIFY(grid%raincv)
ENDIF
IF ( ASSOCIATED( grid%rainncv ) ) THEN 
  DEALLOCATE(grid%rainncv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rainncv. ')
 endif
  NULLIFY(grid%rainncv)
ENDIF
IF ( ASSOCIATED( grid%rainbl ) ) THEN 
  DEALLOCATE(grid%rainbl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rainbl. ')
 endif
  NULLIFY(grid%rainbl)
ENDIF
IF ( ASSOCIATED( grid%snownc ) ) THEN 
  DEALLOCATE(grid%snownc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%snownc. ')
 endif
  NULLIFY(grid%snownc)
ENDIF
IF ( ASSOCIATED( grid%graupelnc ) ) THEN 
  DEALLOCATE(grid%graupelnc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%graupelnc. ')
 endif
  NULLIFY(grid%graupelnc)
ENDIF
IF ( ASSOCIATED( grid%snowncv ) ) THEN 
  DEALLOCATE(grid%snowncv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%snowncv. ')
 endif
  NULLIFY(grid%snowncv)
ENDIF
IF ( ASSOCIATED( grid%graupelncv ) ) THEN 
  DEALLOCATE(grid%graupelncv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%graupelncv. ')
 endif
  NULLIFY(grid%graupelncv)
ENDIF
IF ( ASSOCIATED( grid%nca ) ) THEN 
  DEALLOCATE(grid%nca,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nca. ')
 endif
  NULLIFY(grid%nca)
ENDIF
IF ( ASSOCIATED( grid%lowlyr ) ) THEN 
  DEALLOCATE(grid%lowlyr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%lowlyr. ')
 endif
  NULLIFY(grid%lowlyr)
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
IF ( ASSOCIATED( grid%rthraten ) ) THEN 
  DEALLOCATE(grid%rthraten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rthraten. ')
 endif
  NULLIFY(grid%rthraten)
ENDIF
IF ( ASSOCIATED( grid%rthratenlw ) ) THEN 
  DEALLOCATE(grid%rthratenlw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rthratenlw. ')
 endif
  NULLIFY(grid%rthratenlw)
ENDIF
IF ( ASSOCIATED( grid%rthratensw ) ) THEN 
  DEALLOCATE(grid%rthratensw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rthratensw. ')
 endif
  NULLIFY(grid%rthratensw)
ENDIF
IF ( ASSOCIATED( grid%cldfra ) ) THEN 
  DEALLOCATE(grid%cldfra,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%cldfra. ')
 endif
  NULLIFY(grid%cldfra)
ENDIF
IF ( ASSOCIATED( grid%swdown ) ) THEN 
  DEALLOCATE(grid%swdown,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%swdown. ')
 endif
  NULLIFY(grid%swdown)
ENDIF
IF ( ASSOCIATED( grid%swdownc ) ) THEN 
  DEALLOCATE(grid%swdownc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%swdownc. ')
 endif
  NULLIFY(grid%swdownc)
ENDIF
IF ( ASSOCIATED( grid%gsw ) ) THEN 
  DEALLOCATE(grid%gsw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%gsw. ')
 endif
  NULLIFY(grid%gsw)
ENDIF
IF ( ASSOCIATED( grid%glw ) ) THEN 
  DEALLOCATE(grid%glw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%glw. ')
 endif
  NULLIFY(grid%glw)
ENDIF
IF ( ASSOCIATED( grid%swcf ) ) THEN 
  DEALLOCATE(grid%swcf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%swcf. ')
 endif
  NULLIFY(grid%swcf)
ENDIF
IF ( ASSOCIATED( grid%lwcf ) ) THEN 
  DEALLOCATE(grid%lwcf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%lwcf. ')
 endif
  NULLIFY(grid%lwcf)
ENDIF
IF ( ASSOCIATED( grid%olr ) ) THEN 
  DEALLOCATE(grid%olr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%olr. ')
 endif
  NULLIFY(grid%olr)
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
IF ( ASSOCIATED( grid%em_xlat_u ) ) THEN 
  DEALLOCATE(grid%em_xlat_u,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_xlat_u. ')
 endif
  NULLIFY(grid%em_xlat_u)
ENDIF
IF ( ASSOCIATED( grid%em_xlong_u ) ) THEN 
  DEALLOCATE(grid%em_xlong_u,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_xlong_u. ')
 endif
  NULLIFY(grid%em_xlong_u)
ENDIF
IF ( ASSOCIATED( grid%em_xlat_v ) ) THEN 
  DEALLOCATE(grid%em_xlat_v,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_xlat_v. ')
 endif
  NULLIFY(grid%em_xlat_v)
ENDIF
IF ( ASSOCIATED( grid%em_xlong_v ) ) THEN 
  DEALLOCATE(grid%em_xlong_v,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_xlong_v. ')
 endif
  NULLIFY(grid%em_xlong_v)
ENDIF
IF ( ASSOCIATED( grid%albedo ) ) THEN 
  DEALLOCATE(grid%albedo,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%albedo. ')
 endif
  NULLIFY(grid%albedo)
ENDIF
IF ( ASSOCIATED( grid%albbck ) ) THEN 
  DEALLOCATE(grid%albbck,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%albbck. ')
 endif
  NULLIFY(grid%albbck)
ENDIF
IF ( ASSOCIATED( grid%emiss ) ) THEN 
  DEALLOCATE(grid%emiss,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%emiss. ')
 endif
  NULLIFY(grid%emiss)
ENDIF
IF ( ASSOCIATED( grid%cldefi ) ) THEN 
  DEALLOCATE(grid%cldefi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%cldefi. ')
 endif
  NULLIFY(grid%cldefi)
ENDIF
IF ( ASSOCIATED( grid%rublten ) ) THEN 
  DEALLOCATE(grid%rublten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rublten. ')
 endif
  NULLIFY(grid%rublten)
ENDIF
IF ( ASSOCIATED( grid%rvblten ) ) THEN 
  DEALLOCATE(grid%rvblten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rvblten. ')
 endif
  NULLIFY(grid%rvblten)
ENDIF
IF ( ASSOCIATED( grid%rthblten ) ) THEN 
  DEALLOCATE(grid%rthblten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rthblten. ')
 endif
  NULLIFY(grid%rthblten)
ENDIF
IF ( ASSOCIATED( grid%rqvblten ) ) THEN 
  DEALLOCATE(grid%rqvblten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rqvblten. ')
 endif
  NULLIFY(grid%rqvblten)
ENDIF
IF ( ASSOCIATED( grid%rqcblten ) ) THEN 
  DEALLOCATE(grid%rqcblten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rqcblten. ')
 endif
  NULLIFY(grid%rqcblten)
ENDIF
IF ( ASSOCIATED( grid%rqiblten ) ) THEN 
  DEALLOCATE(grid%rqiblten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rqiblten. ')
 endif
  NULLIFY(grid%rqiblten)
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
IF ( ASSOCIATED( grid%tmn ) ) THEN 
  DEALLOCATE(grid%tmn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%tmn. ')
 endif
  NULLIFY(grid%tmn)
ENDIF
IF ( ASSOCIATED( grid%xland ) ) THEN 
  DEALLOCATE(grid%xland,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%xland. ')
 endif
  NULLIFY(grid%xland)
ENDIF
IF ( ASSOCIATED( grid%znt ) ) THEN 
  DEALLOCATE(grid%znt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%znt. ')
 endif
  NULLIFY(grid%znt)
ENDIF
IF ( ASSOCIATED( grid%ust ) ) THEN 
  DEALLOCATE(grid%ust,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%ust. ')
 endif
  NULLIFY(grid%ust)
ENDIF
IF ( ASSOCIATED( grid%rmol ) ) THEN 
  DEALLOCATE(grid%rmol,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rmol. ')
 endif
  NULLIFY(grid%rmol)
ENDIF
IF ( ASSOCIATED( grid%mol ) ) THEN 
  DEALLOCATE(grid%mol,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%mol. ')
 endif
  NULLIFY(grid%mol)
ENDIF
IF ( ASSOCIATED( grid%pblh ) ) THEN 
  DEALLOCATE(grid%pblh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%pblh. ')
 endif
  NULLIFY(grid%pblh)
ENDIF
IF ( ASSOCIATED( grid%capg ) ) THEN 
  DEALLOCATE(grid%capg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%capg. ')
 endif
  NULLIFY(grid%capg)
ENDIF
IF ( ASSOCIATED( grid%thc ) ) THEN 
  DEALLOCATE(grid%thc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%thc. ')
 endif
  NULLIFY(grid%thc)
ENDIF
IF ( ASSOCIATED( grid%hfx ) ) THEN 
  DEALLOCATE(grid%hfx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%hfx. ')
 endif
  NULLIFY(grid%hfx)
ENDIF
IF ( ASSOCIATED( grid%qfx ) ) THEN 
  DEALLOCATE(grid%qfx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%qfx. ')
 endif
  NULLIFY(grid%qfx)
ENDIF
IF ( ASSOCIATED( grid%lh ) ) THEN 
  DEALLOCATE(grid%lh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%lh. ')
 endif
  NULLIFY(grid%lh)
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
IF ( ASSOCIATED( grid%snowc ) ) THEN 
  DEALLOCATE(grid%snowc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%snowc. ')
 endif
  NULLIFY(grid%snowc)
ENDIF
IF ( ASSOCIATED( grid%mavail ) ) THEN 
  DEALLOCATE(grid%mavail,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%mavail. ')
 endif
  NULLIFY(grid%mavail)
ENDIF
IF ( ASSOCIATED( grid%tkesfcf ) ) THEN 
  DEALLOCATE(grid%tkesfcf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%tkesfcf. ')
 endif
  NULLIFY(grid%tkesfcf)
ENDIF
IF ( ASSOCIATED( grid%taucldi ) ) THEN 
  DEALLOCATE(grid%taucldi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%taucldi. ')
 endif
  NULLIFY(grid%taucldi)
ENDIF
IF ( ASSOCIATED( grid%taucldc ) ) THEN 
  DEALLOCATE(grid%taucldc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%taucldc. ')
 endif
  NULLIFY(grid%taucldc)
ENDIF
IF ( ASSOCIATED( grid%defor11 ) ) THEN 
  DEALLOCATE(grid%defor11,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%defor11. ')
 endif
  NULLIFY(grid%defor11)
ENDIF
IF ( ASSOCIATED( grid%defor22 ) ) THEN 
  DEALLOCATE(grid%defor22,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%defor22. ')
 endif
  NULLIFY(grid%defor22)
ENDIF
IF ( ASSOCIATED( grid%defor12 ) ) THEN 
  DEALLOCATE(grid%defor12,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%defor12. ')
 endif
  NULLIFY(grid%defor12)
ENDIF
IF ( ASSOCIATED( grid%defor33 ) ) THEN 
  DEALLOCATE(grid%defor33,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%defor33. ')
 endif
  NULLIFY(grid%defor33)
ENDIF
IF ( ASSOCIATED( grid%defor13 ) ) THEN 
  DEALLOCATE(grid%defor13,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%defor13. ')
 endif
  NULLIFY(grid%defor13)
ENDIF
IF ( ASSOCIATED( grid%defor23 ) ) THEN 
  DEALLOCATE(grid%defor23,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%defor23. ')
 endif
  NULLIFY(grid%defor23)
ENDIF
IF ( ASSOCIATED( grid%xkmv ) ) THEN 
  DEALLOCATE(grid%xkmv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%xkmv. ')
 endif
  NULLIFY(grid%xkmv)
ENDIF
IF ( ASSOCIATED( grid%xkmh ) ) THEN 
  DEALLOCATE(grid%xkmh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%xkmh. ')
 endif
  NULLIFY(grid%xkmh)
ENDIF
IF ( ASSOCIATED( grid%xkmhd ) ) THEN 
  DEALLOCATE(grid%xkmhd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%xkmhd. ')
 endif
  NULLIFY(grid%xkmhd)
ENDIF
IF ( ASSOCIATED( grid%xkhv ) ) THEN 
  DEALLOCATE(grid%xkhv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%xkhv. ')
 endif
  NULLIFY(grid%xkhv)
ENDIF
IF ( ASSOCIATED( grid%xkhh ) ) THEN 
  DEALLOCATE(grid%xkhh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%xkhh. ')
 endif
  NULLIFY(grid%xkhh)
ENDIF
IF ( ASSOCIATED( grid%div ) ) THEN 
  DEALLOCATE(grid%div,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%div. ')
 endif
  NULLIFY(grid%div)
ENDIF
IF ( ASSOCIATED( grid%bn2 ) ) THEN 
  DEALLOCATE(grid%bn2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%bn2. ')
 endif
  NULLIFY(grid%bn2)
ENDIF
IF ( ASSOCIATED( grid%rundgdten ) ) THEN 
  DEALLOCATE(grid%rundgdten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rundgdten. ')
 endif
  NULLIFY(grid%rundgdten)
ENDIF
IF ( ASSOCIATED( grid%rvndgdten ) ) THEN 
  DEALLOCATE(grid%rvndgdten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rvndgdten. ')
 endif
  NULLIFY(grid%rvndgdten)
ENDIF
IF ( ASSOCIATED( grid%rthndgdten ) ) THEN 
  DEALLOCATE(grid%rthndgdten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rthndgdten. ')
 endif
  NULLIFY(grid%rthndgdten)
ENDIF
IF ( ASSOCIATED( grid%rqvndgdten ) ) THEN 
  DEALLOCATE(grid%rqvndgdten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rqvndgdten. ')
 endif
  NULLIFY(grid%rqvndgdten)
ENDIF
IF ( ASSOCIATED( grid%rmundgdten ) ) THEN 
  DEALLOCATE(grid%rmundgdten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rmundgdten. ')
 endif
  NULLIFY(grid%rmundgdten)
ENDIF
IF ( ASSOCIATED( grid%fdda3d ) ) THEN 
  DEALLOCATE(grid%fdda3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%fdda3d. ')
 endif
  NULLIFY(grid%fdda3d)
ENDIF
IF ( ASSOCIATED( grid%fdda2d ) ) THEN 
  DEALLOCATE(grid%fdda2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%fdda2d. ')
 endif
  NULLIFY(grid%fdda2d)
ENDIF
IF ( ASSOCIATED( grid%abstot ) ) THEN 
  DEALLOCATE(grid%abstot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%abstot. ')
 endif
  NULLIFY(grid%abstot)
ENDIF
IF ( ASSOCIATED( grid%absnxt ) ) THEN 
  DEALLOCATE(grid%absnxt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%absnxt. ')
 endif
  NULLIFY(grid%absnxt)
ENDIF
IF ( ASSOCIATED( grid%emstot ) ) THEN 
  DEALLOCATE(grid%emstot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%emstot. ')
 endif
  NULLIFY(grid%emstot)
ENDIF
IF ( ASSOCIATED( grid%dpsdt ) ) THEN 
  DEALLOCATE(grid%dpsdt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dpsdt. ')
 endif
  NULLIFY(grid%dpsdt)
ENDIF
IF ( ASSOCIATED( grid%dmudt ) ) THEN 
  DEALLOCATE(grid%dmudt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dmudt. ')
 endif
  NULLIFY(grid%dmudt)
ENDIF
IF ( ASSOCIATED( grid%pk1m ) ) THEN 
  DEALLOCATE(grid%pk1m,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%pk1m. ')
 endif
  NULLIFY(grid%pk1m)
ENDIF
IF ( ASSOCIATED( grid%mu_2m ) ) THEN 
  DEALLOCATE(grid%mu_2m,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%mu_2m. ')
 endif
  NULLIFY(grid%mu_2m)
ENDIF
IF ( ASSOCIATED( grid%fdob%varobs ) ) THEN 
  DEALLOCATE(grid%fdob%varobs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%fdob%varobs. ')
 endif
  NULLIFY(grid%fdob%varobs)
ENDIF
IF ( ASSOCIATED( grid%fdob%errf ) ) THEN 
  DEALLOCATE(grid%fdob%errf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%fdob%errf. ')
 endif
  NULLIFY(grid%fdob%errf)
ENDIF
IF ( ASSOCIATED( grid%fdob%timeob ) ) THEN 
  DEALLOCATE(grid%fdob%timeob,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%fdob%timeob. ')
 endif
  NULLIFY(grid%fdob%timeob)
ENDIF
IF ( ASSOCIATED( grid%fdob%nlevs_ob ) ) THEN 
  DEALLOCATE(grid%fdob%nlevs_ob,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%fdob%nlevs_ob. ')
 endif
  NULLIFY(grid%fdob%nlevs_ob)
ENDIF
IF ( ASSOCIATED( grid%fdob%lev_in_ob ) ) THEN 
  DEALLOCATE(grid%fdob%lev_in_ob,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%fdob%lev_in_ob. ')
 endif
  NULLIFY(grid%fdob%lev_in_ob)
ENDIF
IF ( ASSOCIATED( grid%fdob%plfo ) ) THEN 
  DEALLOCATE(grid%fdob%plfo,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%fdob%plfo. ')
 endif
  NULLIFY(grid%fdob%plfo)
ENDIF
IF ( ASSOCIATED( grid%fdob%elevob ) ) THEN 
  DEALLOCATE(grid%fdob%elevob,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%fdob%elevob. ')
 endif
  NULLIFY(grid%fdob%elevob)
ENDIF
IF ( ASSOCIATED( grid%fdob%rio ) ) THEN 
  DEALLOCATE(grid%fdob%rio,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%fdob%rio. ')
 endif
  NULLIFY(grid%fdob%rio)
ENDIF
IF ( ASSOCIATED( grid%fdob%rjo ) ) THEN 
  DEALLOCATE(grid%fdob%rjo,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%fdob%rjo. ')
 endif
  NULLIFY(grid%fdob%rjo)
ENDIF
IF ( ASSOCIATED( grid%fdob%rko ) ) THEN 
  DEALLOCATE(grid%fdob%rko,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%fdob%rko. ')
 endif
  NULLIFY(grid%fdob%rko)
ENDIF
!ENDOFREGISTRYGENERATEDINCLUDE
      ELSE
        WRITE( wrf_err_message , * )'dealloc_space_field: ', &
          'Invalid specification of dynamics: dyn_opt = ',dyn_opt
        CALL wrf_error_fatal3 ( "module_domain.b" , 1296 ,  TRIM ( wrf_err_message ) )
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
          CALL wrf_error_fatal3 ( "module_domain.b" , 1399 ,  &
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
          CALL wrf_error_fatal3 ( "module_domain.b" , 1419 ,  &
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
          CALL wrf_error_fatal3 ( "module_domain.b" , 1439 ,  &
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
          CALL wrf_error_fatal3 ( "module_domain.b" , 1459 ,  &
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
          CALL wrf_error_fatal3 ( "module_domain.b" , 1482 ,  &
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
          CALL wrf_error_fatal3 ( "module_domain.b" , 1564 ,  &
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
            CALL wrf_error_fatal3 ( "module_domain.b" , 1754 ,  &
              'domain_clock_get:  WRFU_TimeIntervalGet() failed' )
          ENDIF
        ENDIF
        IF ( PRESENT( time_stepstr_frac ) ) THEN
          lcl_time_step = domain_get_time_step ( grid )
          CALL WRFU_TimeIntervalGet( lcl_time_step, timeString=tmp_str, &
                                     Sn=Sn, Sd=Sd, rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
            CALL wrf_error_fatal3 ( "module_domain.b" , 1763 ,  &
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
            CALL wrf_error_fatal3 ( "module_domain.b" , 1791 ,  &
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
            CALL wrf_error_fatal3 ( "module_domain.b" , 1809 ,  &
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
            CALL wrf_error_fatal3 ( "module_domain.b" , 1829 ,  &
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
          CALL wrf_error_fatal3 ( "module_domain.b" , 1898 ,  &
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
           CALL wrf_error_fatal3 ( "module_domain.b" , 1945 ,  &
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
          CALL wrf_error_fatal3 ( "module_domain.b" , 1970 ,  &
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
            CALL wrf_error_fatal3 ( "module_domain.b" , 2001 ,  &
              'domain_clock_set:  WRFU_ClockSet(CurrTime) failed' )
          ENDIF
        ENDIF
        IF ( PRESENT( stop_timestr ) ) THEN
          CALL wrf_atotime( stop_timestr(1:19), lcl_stoptime )
          CALL WRFU_ClockSet( grid%domain_clock, stopTime=lcl_stoptime, &
                              rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
            CALL wrf_error_fatal3 ( "module_domain.b" , 2010 ,  &
              'domain_clock_set:  WRFU_ClockSet(StopTime) failed' )
          ENDIF
        ENDIF
        IF ( PRESENT( time_step_seconds ) ) THEN
          CALL WRFU_TimeIntervalSet( tmpTimeInterval, &
                                     S=time_step_seconds, rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
            CALL wrf_error_fatal3 ( "module_domain.b" , 2018 ,  &
              'domain_clock_set:  WRFU_TimeIntervalSet failed' )
          ENDIF
          CALL WRFU_ClockSet ( grid%domain_clock,        &
                               timeStep=tmpTimeInterval, &
                               rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
            CALL wrf_error_fatal3 ( "module_domain.b" , 2025 ,  &
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
          CALL wrf_error_fatal3 ( "module_domain.b" , 2055 ,  &
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
          CALL wrf_error_fatal3 ( "module_domain.b" , 2086 ,  &
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
                              2173  )
        CALL WRFU_TimeIntervalGet( TI, timeString=TI_str, rc=rc )
        CALL wrf_check_error( WRFU_SUCCESS, rc, &
                              'FAIL:  '//TRIM(testname)//'WRFU_TimeGet() ', &
                              "module_domain.b" , &
                              2178  )
        ! CT
        CALL WRFU_TimeSet( CT, YY=CT_YY, MM=CT_MM, DD=CT_DD , &
                                H=CT_H,   M=CT_M,   S=CT_S, rc=rc )
        CALL wrf_check_error( WRFU_SUCCESS, rc, &
                              'FAIL:  '//TRIM(testname)//'WRFU_TimeSet() ', &
                              "module_domain.b" , &
                              2185  )
        CALL WRFU_TimeGet( CT, timeString=CT_str, rc=rc )
        CALL wrf_check_error( WRFU_SUCCESS, rc, &
                              'FAIL:  '//TRIM(testname)//'WRFU_TimeGet() ', &
                              "module_domain.b" , &
                              2190  )
        ! ST
        CALL WRFU_TimeSet( ST, YY=ST_YY, MM=ST_MM, DD=ST_DD , &
                                H=ST_H,   M=ST_M,   S=ST_S, rc=rc )
        CALL wrf_check_error( WRFU_SUCCESS, rc, &
                              'FAIL:  '//TRIM(testname)//'WRFU_TimeSet() ', &
                              "module_domain.b" , &
                              2197  )
        CALL WRFU_TimeGet( ST, timeString=ST_str, rc=rc )
        CALL wrf_check_error( WRFU_SUCCESS, rc, &
                              'FAIL:  '//TRIM(testname)//'WRFU_TimeGet() ', &
                              "module_domain.b" , &
                              2202  )
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
            CALL wrf_error_fatal3 ( "module_domain.b" , 2264 ,  &
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



