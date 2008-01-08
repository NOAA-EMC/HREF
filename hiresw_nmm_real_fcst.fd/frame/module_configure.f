!WRF:DRIVER_LAYER:CONFIGURATION
!
MODULE module_configure

   USE module_driver_constants
   USE module_state_description
   USE module_wrf_error

   TYPE model_config_rec_type
      SEQUENCE
! Statements that declare namelist variables are in this file
! Note that the namelist is SEQUENCE and generated such that the first item is an
! integer, first_item_in_struct and the last is an integer last_item_in_struct
! this provides a way of converting this to a buffer for passing to and from
! the driver.
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/namelist_defines.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
integer    :: first_item_in_struct
integer :: time_step
integer :: time_step_fract_num
integer :: time_step_fract_den
logical :: restart
integer :: max_dom
integer :: dyn_opt
integer :: rk_ord
integer :: diff_opt
integer :: km_opt
integer :: damp_opt
integer :: isfflx
integer :: ifsnow
integer :: icloud
integer :: num_soil_layers
integer :: num_land_cat
integer :: num_soil_cat
integer :: spec_bdy_width
integer :: spec_zone
integer :: relax_zone
integer :: ensdim
integer :: tile_sz_x
integer :: tile_sz_y
integer :: numtiles
integer :: debug_level
integer :: irand
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
integer , DIMENSION(max_domains) :: grid_id
integer , DIMENSION(max_domains) :: level
integer , DIMENSION(max_domains) :: s_we
integer , DIMENSION(max_domains) :: e_we
integer , DIMENSION(max_domains) :: s_sn
integer , DIMENSION(max_domains) :: e_sn
integer , DIMENSION(max_domains) :: s_vert
integer , DIMENSION(max_domains) :: e_vert
integer , DIMENSION(max_domains) :: history_interval
integer , DIMENSION(max_domains) :: auxhist1_interval
integer , DIMENSION(max_domains) :: auxhist2_interval
integer , DIMENSION(max_domains) :: auxhist3_interval
integer , DIMENSION(max_domains) :: auxhist4_interval
integer , DIMENSION(max_domains) :: auxhist5_interval
integer , DIMENSION(max_domains) :: auxinput1_interval
integer , DIMENSION(max_domains) :: auxinput2_interval
integer , DIMENSION(max_domains) :: auxinput3_interval
integer , DIMENSION(max_domains) :: auxinput4_interval
integer , DIMENSION(max_domains) :: auxinput5_interval
integer :: restart_interval
integer , DIMENSION(max_domains) :: frames_per_outfile
integer , DIMENSION(max_domains) :: time_step_sound
integer , DIMENSION(max_domains) :: parent_id
integer , DIMENSION(max_domains) :: i_parent_start
integer , DIMENSION(max_domains) :: j_parent_start
integer , DIMENSION(max_domains) :: shw
integer , DIMENSION(max_domains) :: parent_grid_ratio
integer , DIMENSION(max_domains) :: parent_time_step_ratio
integer , DIMENSION(max_domains) :: moad_grid_ratio
integer , DIMENSION(max_domains) :: moad_time_step_ratio
logical , DIMENSION(max_domains) :: non_hydrostatic
real , DIMENSION(max_domains) :: dx
real , DIMENSION(max_domains) :: dy
real , DIMENSION(max_domains) :: dt
real , DIMENSION(max_domains) :: ztop
real , DIMENSION(max_domains) :: zdamp
real , DIMENSION(max_domains) :: dampcoef
real , DIMENSION(max_domains) :: smdiv
real , DIMENSION(max_domains) :: emdiv
real , DIMENSION(max_domains) :: epssm
real , DIMENSION(max_domains) :: khdif
real , DIMENSION(max_domains) :: kvdif
real , DIMENSION(max_domains) :: mix_cr_len
real , DIMENSION(max_domains) :: tke_upper_bound
real , DIMENSION(max_domains) :: kh_tke_upper_bound
real , DIMENSION(max_domains) :: kv_tke_upper_bound
real , DIMENSION(max_domains) :: radt
real , DIMENSION(max_domains) :: bldt
real , DIMENSION(max_domains) :: cudt
real , DIMENSION(max_domains) :: gsmdt
integer , DIMENSION(max_domains) :: julyr
integer , DIMENSION(max_domains) :: julday
real , DIMENSION(max_domains) :: gmt
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
logical , DIMENSION(max_domains) :: specified
logical , DIMENSION(max_domains) :: top_radiation
integer , DIMENSION(max_domains) :: idtad
integer , DIMENSION(max_domains) :: nsoil
integer , DIMENSION(max_domains) :: nphs
integer , DIMENSION(max_domains) :: ncnvc
integer , DIMENSION(max_domains) :: nrads
integer , DIMENSION(max_domains) :: nradl
integer , DIMENSION(max_domains) :: sigma
integer , DIMENSION(max_domains) :: chem_opt
integer , DIMENSION(max_domains) :: mp_physics
integer , DIMENSION(max_domains) :: ra_lw_physics
integer , DIMENSION(max_domains) :: ra_sw_physics
integer , DIMENSION(max_domains) :: bl_sfclay_physics
integer , DIMENSION(max_domains) :: bl_surface_physics
integer , DIMENSION(max_domains) :: bl_pbl_physics
integer , DIMENSION(max_domains) :: cu_physics
integer , DIMENSION(max_domains) :: h_mom_adv_order
integer , DIMENSION(max_domains) :: v_mom_adv_order
integer , DIMENSION(max_domains) :: h_sca_adv_order
integer , DIMENSION(max_domains) :: v_sca_adv_order
integer :: io_form_input
integer :: io_form_auxinput1
integer :: io_form_auxinput2
integer :: io_form_auxinput3
integer :: io_form_auxinput4
integer :: io_form_auxinput5
integer :: io_form_history
integer :: io_form_auxhist1
integer :: io_form_auxhist2
integer :: io_form_auxhist3
integer :: io_form_auxhist4
integer :: io_form_auxhist5
integer :: io_form_restart
integer :: io_form_boundary
integer :: interval_seconds
integer :: real_data_init_type
real , DIMENSION(max_domains) :: cen_lat
real , DIMENSION(max_domains) :: cen_lon
real , DIMENSION(max_domains) :: truelat1
real , DIMENSION(max_domains) :: truelat2
real , DIMENSION(max_domains) :: bdyfrq
integer , DIMENSION(max_domains) :: iswater
integer , DIMENSION(max_domains) :: isice
integer , DIMENSION(max_domains) :: map_proj
integer    :: last_item_in_struct
!ENDOFREGISTRYGENERATEDINCLUDE
   END TYPE model_config_rec_type

   TYPE grid_config_rec_type
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/namelist_defines2.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
integer    :: first_item_in_struct
integer :: time_step
integer :: time_step_fract_num
integer :: time_step_fract_den
logical :: restart
integer :: max_dom
integer :: dyn_opt
integer :: rk_ord
integer :: diff_opt
integer :: km_opt
integer :: damp_opt
integer :: isfflx
integer :: ifsnow
integer :: icloud
integer :: num_soil_layers
integer :: num_land_cat
integer :: num_soil_cat
integer :: spec_bdy_width
integer :: spec_zone
integer :: relax_zone
integer :: ensdim
integer :: tile_sz_x
integer :: tile_sz_y
integer :: numtiles
integer :: debug_level
integer :: irand
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
integer :: grid_id
integer :: level
integer :: s_we
integer :: e_we
integer :: s_sn
integer :: e_sn
integer :: s_vert
integer :: e_vert
integer :: history_interval
integer :: auxhist1_interval
integer :: auxhist2_interval
integer :: auxhist3_interval
integer :: auxhist4_interval
integer :: auxhist5_interval
integer :: auxinput1_interval
integer :: auxinput2_interval
integer :: auxinput3_interval
integer :: auxinput4_interval
integer :: auxinput5_interval
integer :: restart_interval
integer :: frames_per_outfile
integer :: time_step_sound
integer :: parent_id
integer :: i_parent_start
integer :: j_parent_start
integer :: shw
integer :: parent_grid_ratio
integer :: parent_time_step_ratio
integer :: moad_grid_ratio
integer :: moad_time_step_ratio
logical :: non_hydrostatic
real :: dx
real :: dy
real :: dt
real :: ztop
real :: zdamp
real :: dampcoef
real :: smdiv
real :: emdiv
real :: epssm
real :: khdif
real :: kvdif
real :: mix_cr_len
real :: tke_upper_bound
real :: kh_tke_upper_bound
real :: kv_tke_upper_bound
real :: radt
real :: bldt
real :: cudt
real :: gsmdt
integer :: julyr
integer :: julday
real :: gmt
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
logical :: nested
logical :: specified
logical :: top_radiation
integer :: idtad
integer :: nsoil
integer :: nphs
integer :: ncnvc
integer :: nrads
integer :: nradl
integer :: sigma
integer :: chem_opt
integer :: mp_physics
integer :: ra_lw_physics
integer :: ra_sw_physics
integer :: bl_sfclay_physics
integer :: bl_surface_physics
integer :: bl_pbl_physics
integer :: cu_physics
integer :: h_mom_adv_order
integer :: v_mom_adv_order
integer :: h_sca_adv_order
integer :: v_sca_adv_order
integer :: io_form_input
integer :: io_form_auxinput1
integer :: io_form_auxinput2
integer :: io_form_auxinput3
integer :: io_form_auxinput4
integer :: io_form_auxinput5
integer :: io_form_history
integer :: io_form_auxhist1
integer :: io_form_auxhist2
integer :: io_form_auxhist3
integer :: io_form_auxhist4
integer :: io_form_auxhist5
integer :: io_form_restart
integer :: io_form_boundary
integer :: interval_seconds
integer :: real_data_init_type
real :: cen_lat
real :: cen_lon
real :: truelat1
real :: truelat2
real :: bdyfrq
integer :: iswater
integer :: isice
integer :: map_proj
integer    :: last_item_in_struct
!ENDOFREGISTRYGENERATEDINCLUDE
   END TYPE grid_config_rec_type

   TYPE(model_config_rec_type) :: model_config_rec

!STARTOFREGISTRYGENERATEDINCLUDE 'inc/scalar_tables.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
  INTEGER :: moist_index_table( param_num_moist, max_domains )
  INTEGER :: moist_num_table( max_domains )
  INTEGER :: chem_index_table( param_num_chem, max_domains )
  INTEGER :: chem_num_table( max_domains )
!ENDOFREGISTRYGENERATEDINCLUDE

! special entries (put here but not enshrined in Registry for one reason or other)

   CHARACTER (LEN=4) :: mminlu = '    '         ! character string for landuse table

   PRIVATE read_namelist_data

CONTAINS


! Model layer, even though it does I/O -- special case of namelist I/O.

   SUBROUTINE initial_config
      IMPLICIT NONE

      INTEGER              :: io_status, nml_unit

! define as temporaries
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/namelist_defines.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
integer    :: first_item_in_struct
integer :: time_step
integer :: time_step_fract_num
integer :: time_step_fract_den
logical :: restart
integer :: max_dom
integer :: dyn_opt
integer :: rk_ord
integer :: diff_opt
integer :: km_opt
integer :: damp_opt
integer :: isfflx
integer :: ifsnow
integer :: icloud
integer :: num_soil_layers
integer :: num_land_cat
integer :: num_soil_cat
integer :: spec_bdy_width
integer :: spec_zone
integer :: relax_zone
integer :: ensdim
integer :: tile_sz_x
integer :: tile_sz_y
integer :: numtiles
integer :: debug_level
integer :: irand
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
integer , DIMENSION(max_domains) :: grid_id
integer , DIMENSION(max_domains) :: level
integer , DIMENSION(max_domains) :: s_we
integer , DIMENSION(max_domains) :: e_we
integer , DIMENSION(max_domains) :: s_sn
integer , DIMENSION(max_domains) :: e_sn
integer , DIMENSION(max_domains) :: s_vert
integer , DIMENSION(max_domains) :: e_vert
integer , DIMENSION(max_domains) :: history_interval
integer , DIMENSION(max_domains) :: auxhist1_interval
integer , DIMENSION(max_domains) :: auxhist2_interval
integer , DIMENSION(max_domains) :: auxhist3_interval
integer , DIMENSION(max_domains) :: auxhist4_interval
integer , DIMENSION(max_domains) :: auxhist5_interval
integer , DIMENSION(max_domains) :: auxinput1_interval
integer , DIMENSION(max_domains) :: auxinput2_interval
integer , DIMENSION(max_domains) :: auxinput3_interval
integer , DIMENSION(max_domains) :: auxinput4_interval
integer , DIMENSION(max_domains) :: auxinput5_interval
integer :: restart_interval
integer , DIMENSION(max_domains) :: frames_per_outfile
integer , DIMENSION(max_domains) :: time_step_sound
integer , DIMENSION(max_domains) :: parent_id
integer , DIMENSION(max_domains) :: i_parent_start
integer , DIMENSION(max_domains) :: j_parent_start
integer , DIMENSION(max_domains) :: shw
integer , DIMENSION(max_domains) :: parent_grid_ratio
integer , DIMENSION(max_domains) :: parent_time_step_ratio
integer , DIMENSION(max_domains) :: moad_grid_ratio
integer , DIMENSION(max_domains) :: moad_time_step_ratio
logical , DIMENSION(max_domains) :: non_hydrostatic
real , DIMENSION(max_domains) :: dx
real , DIMENSION(max_domains) :: dy
real , DIMENSION(max_domains) :: dt
real , DIMENSION(max_domains) :: ztop
real , DIMENSION(max_domains) :: zdamp
real , DIMENSION(max_domains) :: dampcoef
real , DIMENSION(max_domains) :: smdiv
real , DIMENSION(max_domains) :: emdiv
real , DIMENSION(max_domains) :: epssm
real , DIMENSION(max_domains) :: khdif
real , DIMENSION(max_domains) :: kvdif
real , DIMENSION(max_domains) :: mix_cr_len
real , DIMENSION(max_domains) :: tke_upper_bound
real , DIMENSION(max_domains) :: kh_tke_upper_bound
real , DIMENSION(max_domains) :: kv_tke_upper_bound
real , DIMENSION(max_domains) :: radt
real , DIMENSION(max_domains) :: bldt
real , DIMENSION(max_domains) :: cudt
real , DIMENSION(max_domains) :: gsmdt
integer , DIMENSION(max_domains) :: julyr
integer , DIMENSION(max_domains) :: julday
real , DIMENSION(max_domains) :: gmt
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
logical , DIMENSION(max_domains) :: specified
logical , DIMENSION(max_domains) :: top_radiation
integer , DIMENSION(max_domains) :: idtad
integer , DIMENSION(max_domains) :: nsoil
integer , DIMENSION(max_domains) :: nphs
integer , DIMENSION(max_domains) :: ncnvc
integer , DIMENSION(max_domains) :: nrads
integer , DIMENSION(max_domains) :: nradl
integer , DIMENSION(max_domains) :: sigma
integer , DIMENSION(max_domains) :: chem_opt
integer , DIMENSION(max_domains) :: mp_physics
integer , DIMENSION(max_domains) :: ra_lw_physics
integer , DIMENSION(max_domains) :: ra_sw_physics
integer , DIMENSION(max_domains) :: bl_sfclay_physics
integer , DIMENSION(max_domains) :: bl_surface_physics
integer , DIMENSION(max_domains) :: bl_pbl_physics
integer , DIMENSION(max_domains) :: cu_physics
integer , DIMENSION(max_domains) :: h_mom_adv_order
integer , DIMENSION(max_domains) :: v_mom_adv_order
integer , DIMENSION(max_domains) :: h_sca_adv_order
integer , DIMENSION(max_domains) :: v_sca_adv_order
integer :: io_form_input
integer :: io_form_auxinput1
integer :: io_form_auxinput2
integer :: io_form_auxinput3
integer :: io_form_auxinput4
integer :: io_form_auxinput5
integer :: io_form_history
integer :: io_form_auxhist1
integer :: io_form_auxhist2
integer :: io_form_auxhist3
integer :: io_form_auxhist4
integer :: io_form_auxhist5
integer :: io_form_restart
integer :: io_form_boundary
integer :: interval_seconds
integer :: real_data_init_type
real , DIMENSION(max_domains) :: cen_lat
real , DIMENSION(max_domains) :: cen_lon
real , DIMENSION(max_domains) :: truelat1
real , DIMENSION(max_domains) :: truelat2
real , DIMENSION(max_domains) :: bdyfrq
integer , DIMENSION(max_domains) :: iswater
integer , DIMENSION(max_domains) :: isice
integer , DIMENSION(max_domains) :: map_proj
integer    :: last_item_in_struct
!ENDOFREGISTRYGENERATEDINCLUDE

! Statements that specify the namelists
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/namelist_statements.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
NAMELIST /namelist_01/ time_step
NAMELIST /namelist_01/ time_step_fract_num
NAMELIST /namelist_01/ time_step_fract_den
NAMELIST /namelist_01/ restart
NAMELIST /namelist_01/ max_dom
NAMELIST /namelist_01/ dyn_opt
NAMELIST /namelist_01/ rk_ord
NAMELIST /namelist_01/ diff_opt
NAMELIST /namelist_01/ km_opt
NAMELIST /namelist_01/ damp_opt
NAMELIST /namelist_01/ isfflx
NAMELIST /namelist_01/ ifsnow
NAMELIST /namelist_01/ icloud
NAMELIST /namelist_01/ num_soil_layers
NAMELIST /namelist_01/ num_land_cat
NAMELIST /namelist_01/ num_soil_cat
NAMELIST /namelist_01/ spec_bdy_width
NAMELIST /namelist_01/ spec_zone
NAMELIST /namelist_01/ relax_zone
NAMELIST /namelist_01/ ensdim
NAMELIST /namelist_01/ tile_sz_x
NAMELIST /namelist_01/ tile_sz_y
NAMELIST /namelist_01/ numtiles
NAMELIST /namelist_01/ debug_level
NAMELIST /namelist_01/ irand
NAMELIST /namelist_02/ run_days
NAMELIST /namelist_02/ run_hours
NAMELIST /namelist_02/ run_minutes
NAMELIST /namelist_02/ run_seconds
NAMELIST /namelist_02/ start_year
NAMELIST /namelist_02/ start_month
NAMELIST /namelist_02/ start_day
NAMELIST /namelist_02/ start_hour
NAMELIST /namelist_02/ start_minute
NAMELIST /namelist_02/ start_second
NAMELIST /namelist_02/ end_year
NAMELIST /namelist_02/ end_month
NAMELIST /namelist_02/ end_day
NAMELIST /namelist_02/ end_hour
NAMELIST /namelist_02/ end_minute
NAMELIST /namelist_02/ end_second
NAMELIST /namelist_02/ grid_id
NAMELIST /namelist_02/ level
NAMELIST /namelist_02/ s_we
NAMELIST /namelist_02/ e_we
NAMELIST /namelist_02/ s_sn
NAMELIST /namelist_02/ e_sn
NAMELIST /namelist_02/ s_vert
NAMELIST /namelist_02/ e_vert
NAMELIST /namelist_02/ history_interval
NAMELIST /namelist_02/ auxhist1_interval
NAMELIST /namelist_02/ auxhist2_interval
NAMELIST /namelist_02/ auxhist3_interval
NAMELIST /namelist_02/ auxhist4_interval
NAMELIST /namelist_02/ auxhist5_interval
NAMELIST /namelist_02/ auxinput1_interval
NAMELIST /namelist_02/ auxinput2_interval
NAMELIST /namelist_02/ auxinput3_interval
NAMELIST /namelist_02/ auxinput4_interval
NAMELIST /namelist_02/ auxinput5_interval
NAMELIST /namelist_02/ restart_interval
NAMELIST /namelist_02/ frames_per_outfile
NAMELIST /namelist_02/ time_step_sound
NAMELIST /namelist_02/ parent_id
NAMELIST /namelist_02/ i_parent_start
NAMELIST /namelist_02/ j_parent_start
NAMELIST /namelist_02/ shw
NAMELIST /namelist_02/ parent_grid_ratio
NAMELIST /namelist_02/ parent_time_step_ratio
NAMELIST /namelist_02/ moad_grid_ratio
NAMELIST /namelist_02/ moad_time_step_ratio
NAMELIST /namelist_03/ non_hydrostatic
NAMELIST /namelist_03/ dx
NAMELIST /namelist_03/ dy
NAMELIST /namelist_03/ ztop
NAMELIST /namelist_03/ zdamp
NAMELIST /namelist_03/ dampcoef
NAMELIST /namelist_03/ smdiv
NAMELIST /namelist_03/ emdiv
NAMELIST /namelist_03/ epssm
NAMELIST /namelist_03/ khdif
NAMELIST /namelist_03/ kvdif
NAMELIST /namelist_03/ mix_cr_len
NAMELIST /namelist_03/ tke_upper_bound
NAMELIST /namelist_03/ kh_tke_upper_bound
NAMELIST /namelist_03/ kv_tke_upper_bound
NAMELIST /namelist_03/ radt
NAMELIST /namelist_03/ bldt
NAMELIST /namelist_03/ cudt
NAMELIST /namelist_03/ gsmdt
NAMELIST /namelist_03/ julyr
NAMELIST /namelist_03/ julday
NAMELIST /namelist_03/ gmt
NAMELIST /namelist_04/ periodic_x
NAMELIST /namelist_04/ symmetric_xs
NAMELIST /namelist_04/ symmetric_xe
NAMELIST /namelist_04/ open_xs
NAMELIST /namelist_04/ open_xe
NAMELIST /namelist_04/ periodic_y
NAMELIST /namelist_04/ symmetric_ys
NAMELIST /namelist_04/ symmetric_ye
NAMELIST /namelist_04/ open_ys
NAMELIST /namelist_04/ open_ye
NAMELIST /namelist_04/ nested
NAMELIST /namelist_04/ specified
NAMELIST /namelist_04/ top_radiation
NAMELIST /namelist_04/ idtad
NAMELIST /namelist_04/ nsoil
NAMELIST /namelist_04/ nphs
NAMELIST /namelist_04/ ncnvc
NAMELIST /namelist_04/ nrads
NAMELIST /namelist_04/ nradl
NAMELIST /namelist_04/ sigma
NAMELIST /namelist_04/ chem_opt
NAMELIST /namelist_04/ mp_physics
NAMELIST /namelist_04/ ra_lw_physics
NAMELIST /namelist_04/ ra_sw_physics
NAMELIST /namelist_04/ bl_sfclay_physics
NAMELIST /namelist_04/ bl_surface_physics
NAMELIST /namelist_04/ bl_pbl_physics
NAMELIST /namelist_04/ cu_physics
NAMELIST /namelist_04/ h_mom_adv_order
NAMELIST /namelist_04/ v_mom_adv_order
NAMELIST /namelist_04/ h_sca_adv_order
NAMELIST /namelist_04/ v_sca_adv_order
NAMELIST /namelist_04/ io_form_input
NAMELIST /namelist_04/ io_form_auxinput1
NAMELIST /namelist_04/ io_form_auxinput2
NAMELIST /namelist_04/ io_form_auxinput3
NAMELIST /namelist_04/ io_form_auxinput4
NAMELIST /namelist_04/ io_form_auxinput5
NAMELIST /namelist_04/ io_form_history
NAMELIST /namelist_04/ io_form_auxhist1
NAMELIST /namelist_04/ io_form_auxhist2
NAMELIST /namelist_04/ io_form_auxhist3
NAMELIST /namelist_04/ io_form_auxhist4
NAMELIST /namelist_04/ io_form_auxhist5
NAMELIST /namelist_04/ io_form_restart
NAMELIST /namelist_04/ io_form_boundary
NAMELIST /namelist_05/ interval_seconds
NAMELIST /namelist_05/ real_data_init_type
!ENDOFREGISTRYGENERATEDINCLUDE

      OPEN ( UNIT   = 10               ,      &
             FILE   = "namelist.input" ,      &
             FORM   = "FORMATTED"      ,      &
             STATUS = "OLD"            ,      &
             IOSTAT = io_status         )

      IF ( io_status .NE. 0 ) THEN
        CALL WRF_ERROR_FATAL ( 'ERROR OPENING namelist.input' )
      ENDIF

      nml_unit = 10

! Statements that set the namelist vars to default vals
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/namelist_defaults.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
time_step_fract_num = 0
time_step_fract_den = 1
restart = .false.
max_dom = 1
dyn_opt = 1
rk_ord = 3
diff_opt = 1
km_opt = 1
damp_opt = 1
isfflx = 1
ifsnow = 0
icloud = 1
num_soil_layers = 5
num_land_cat = 24
num_soil_cat = 16
spec_bdy_width = 5
spec_zone = 1
relax_zone = 4
ensdim = 1
tile_sz_x = 0
tile_sz_y = 0
numtiles = 1
debug_level = 0
irand = 0
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
grid_id = 1
level = 1
s_we = 1
e_we = 32
s_sn = 1
e_sn = 32
s_vert = 1
e_vert = 31
history_interval = -1
auxhist1_interval = -1
auxhist2_interval = -1
auxhist3_interval = -1
auxhist4_interval = -1
auxhist5_interval = -1
auxinput1_interval = -1
auxinput2_interval = -1
auxinput3_interval = -1
auxinput4_interval = -1
auxinput5_interval = -1
restart_interval = -1
frames_per_outfile = 10
time_step_sound = 10
parent_id = 0
i_parent_start = 1
j_parent_start = 1
shw = 1
parent_grid_ratio = 1
parent_time_step_ratio = 1
moad_grid_ratio = 1
moad_time_step_ratio = 1
non_hydrostatic = .true.
dx = 200
dy = 200
dt = 2.
ztop = 15000.
zdamp = 5000.
dampcoef = 0.2
smdiv = 0.
emdiv = 0.
epssm = .1
khdif = 0
kvdif = 0
mix_cr_len = 200.
tke_upper_bound = 1000.
kh_tke_upper_bound = 1000.
kv_tke_upper_bound = 100.
radt = 0
bldt = 0
cudt = 0
gsmdt = 0
julyr = 0
julday = 1
gmt = 0.
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
nested = .false.
specified = .false.
top_radiation = .false.
idtad = 2
nsoil = 4
nphs = 12
ncnvc = 4
nrads = 180
nradl = 180
sigma = 1
chem_opt = 0
mp_physics = 0
ra_lw_physics = 0
ra_sw_physics = 0
bl_sfclay_physics = 0
bl_surface_physics = 0
bl_pbl_physics = 0
cu_physics = 0
h_mom_adv_order = 3
v_mom_adv_order = 3
h_sca_adv_order = 3
v_sca_adv_order = 3
io_form_input = 2
io_form_auxinput1 = 0
io_form_auxinput2 = 0
io_form_auxinput3 = 0
io_form_auxinput4 = 0
io_form_auxinput5 = 0
io_form_history = 2
io_form_auxhist1 = 0
io_form_auxhist2 = 0
io_form_auxhist3 = 0
io_form_auxhist4 = 0
io_form_auxhist5 = 0
io_form_restart = 2
io_form_boundary = 2
interval_seconds = 43200
real_data_init_type = 1
cen_lat = 0
cen_lon = 0
truelat1 = 0
truelat2 = 0
bdyfrq = 0
iswater = 0
isice = 0
map_proj = 0
!ENDOFREGISTRYGENERATEDINCLUDE

! Statements that read the namelist are in this file
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/config_reads.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
! Contains namelist statements for module_config.F.
!
 READ  ( UNIT = nml_unit , NML = namelist_01 , ERR = 9200 )
 WRITE ( UNIT = *                  , NML = namelist_01 )
 READ  ( UNIT = nml_unit , NML = namelist_02 , ERR = 9200 )
 WRITE ( UNIT = *                  , NML = namelist_02 )
 READ  ( UNIT = nml_unit , NML = namelist_03 , ERR = 9200 )
 WRITE ( UNIT = *                  , NML = namelist_03 )
 READ  ( UNIT = nml_unit , NML = namelist_04 , ERR = 9200 )
 WRITE ( UNIT = *                  , NML = namelist_04 )
 READ  ( UNIT = nml_unit , NML = namelist_05 , ERR = 9200 )
 WRITE ( UNIT = *                  , NML = namelist_05 )
!ENDOFREGISTRYGENERATEDINCLUDE

! Statements that assign the variables to the cfg record are in this file
! except the namelist_derived variables where are assigned below
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/config_assigns.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
! Contains config assign statements for module_domain.F.
 model_config_rec % time_step                  =  time_step 
 model_config_rec % time_step_fract_num        =  time_step_fract_num 
 model_config_rec % time_step_fract_den        =  time_step_fract_den 
 model_config_rec % restart                    =  restart 
 model_config_rec % max_dom                    =  max_dom 
 model_config_rec % dyn_opt                    =  dyn_opt 
 model_config_rec % rk_ord                     =  rk_ord 
 model_config_rec % diff_opt                   =  diff_opt 
 model_config_rec % km_opt                     =  km_opt 
 model_config_rec % damp_opt                   =  damp_opt 
 model_config_rec % isfflx                     =  isfflx 
 model_config_rec % ifsnow                     =  ifsnow 
 model_config_rec % icloud                     =  icloud 
 model_config_rec % num_soil_layers            =  num_soil_layers 
 model_config_rec % num_land_cat               =  num_land_cat 
 model_config_rec % num_soil_cat               =  num_soil_cat 
 model_config_rec % spec_bdy_width             =  spec_bdy_width 
 model_config_rec % spec_zone                  =  spec_zone 
 model_config_rec % relax_zone                 =  relax_zone 
 model_config_rec % ensdim                     =  ensdim 
 model_config_rec % tile_sz_x                  =  tile_sz_x 
 model_config_rec % tile_sz_y                  =  tile_sz_y 
 model_config_rec % numtiles                   =  numtiles 
 model_config_rec % debug_level                =  debug_level 
 model_config_rec % irand                      =  irand 
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
 model_config_rec % grid_id                    =  grid_id 
 model_config_rec % level                      =  level 
 model_config_rec % s_we                       =  s_we 
 model_config_rec % e_we                       =  e_we 
 model_config_rec % s_sn                       =  s_sn 
 model_config_rec % e_sn                       =  e_sn 
 model_config_rec % s_vert                     =  s_vert 
 model_config_rec % e_vert                     =  e_vert 
 model_config_rec % history_interval           =  history_interval 
 model_config_rec % auxhist1_interval          =  auxhist1_interval 
 model_config_rec % auxhist2_interval          =  auxhist2_interval 
 model_config_rec % auxhist3_interval          =  auxhist3_interval 
 model_config_rec % auxhist4_interval          =  auxhist4_interval 
 model_config_rec % auxhist5_interval          =  auxhist5_interval 
 model_config_rec % auxinput1_interval         =  auxinput1_interval 
 model_config_rec % auxinput2_interval         =  auxinput2_interval 
 model_config_rec % auxinput3_interval         =  auxinput3_interval 
 model_config_rec % auxinput4_interval         =  auxinput4_interval 
 model_config_rec % auxinput5_interval         =  auxinput5_interval 
 model_config_rec % restart_interval           =  restart_interval 
 model_config_rec % frames_per_outfile         =  frames_per_outfile 
 model_config_rec % time_step_sound            =  time_step_sound 
 model_config_rec % parent_id                  =  parent_id 
 model_config_rec % i_parent_start             =  i_parent_start 
 model_config_rec % j_parent_start             =  j_parent_start 
 model_config_rec % shw                        =  shw 
 model_config_rec % parent_grid_ratio          =  parent_grid_ratio 
 model_config_rec % parent_time_step_ratio     =  parent_time_step_ratio 
 model_config_rec % moad_grid_ratio            =  moad_grid_ratio 
 model_config_rec % moad_time_step_ratio       =  moad_time_step_ratio 
 model_config_rec % non_hydrostatic            =  non_hydrostatic 
 model_config_rec % dx                         =  dx 
 model_config_rec % dy                         =  dy 
 model_config_rec % dt                         =  dt 
 model_config_rec % ztop                       =  ztop 
 model_config_rec % zdamp                      =  zdamp 
 model_config_rec % dampcoef                   =  dampcoef 
 model_config_rec % smdiv                      =  smdiv 
 model_config_rec % emdiv                      =  emdiv 
 model_config_rec % epssm                      =  epssm 
 model_config_rec % khdif                      =  khdif 
 model_config_rec % kvdif                      =  kvdif 
 model_config_rec % mix_cr_len                 =  mix_cr_len 
 model_config_rec % tke_upper_bound            =  tke_upper_bound 
 model_config_rec % kh_tke_upper_bound         =  kh_tke_upper_bound 
 model_config_rec % kv_tke_upper_bound         =  kv_tke_upper_bound 
 model_config_rec % radt                       =  radt 
 model_config_rec % bldt                       =  bldt 
 model_config_rec % cudt                       =  cudt 
 model_config_rec % gsmdt                      =  gsmdt 
 model_config_rec % julyr                      =  julyr 
 model_config_rec % julday                     =  julday 
 model_config_rec % gmt                        =  gmt 
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
 model_config_rec % nested                     =  nested 
 model_config_rec % specified                  =  specified 
 model_config_rec % top_radiation              =  top_radiation 
 model_config_rec % idtad                      =  idtad 
 model_config_rec % nsoil                      =  nsoil 
 model_config_rec % nphs                       =  nphs 
 model_config_rec % ncnvc                      =  ncnvc 
 model_config_rec % nrads                      =  nrads 
 model_config_rec % nradl                      =  nradl 
 model_config_rec % sigma                      =  sigma 
 model_config_rec % chem_opt                   =  chem_opt 
 model_config_rec % mp_physics                 =  mp_physics 
 model_config_rec % ra_lw_physics              =  ra_lw_physics 
 model_config_rec % ra_sw_physics              =  ra_sw_physics 
 model_config_rec % bl_sfclay_physics          =  bl_sfclay_physics 
 model_config_rec % bl_surface_physics         =  bl_surface_physics 
 model_config_rec % bl_pbl_physics             =  bl_pbl_physics 
 model_config_rec % cu_physics                 =  cu_physics 
 model_config_rec % h_mom_adv_order            =  h_mom_adv_order 
 model_config_rec % v_mom_adv_order            =  v_mom_adv_order 
 model_config_rec % h_sca_adv_order            =  h_sca_adv_order 
 model_config_rec % v_sca_adv_order            =  v_sca_adv_order 
 model_config_rec % io_form_input              =  io_form_input 
 model_config_rec % io_form_auxinput1          =  io_form_auxinput1 
 model_config_rec % io_form_auxinput2          =  io_form_auxinput2 
 model_config_rec % io_form_auxinput3          =  io_form_auxinput3 
 model_config_rec % io_form_auxinput4          =  io_form_auxinput4 
 model_config_rec % io_form_auxinput5          =  io_form_auxinput5 
 model_config_rec % io_form_history            =  io_form_history 
 model_config_rec % io_form_auxhist1           =  io_form_auxhist1 
 model_config_rec % io_form_auxhist2           =  io_form_auxhist2 
 model_config_rec % io_form_auxhist3           =  io_form_auxhist3 
 model_config_rec % io_form_auxhist4           =  io_form_auxhist4 
 model_config_rec % io_form_auxhist5           =  io_form_auxhist5 
 model_config_rec % io_form_restart            =  io_form_restart 
 model_config_rec % io_form_boundary           =  io_form_boundary 
 model_config_rec % interval_seconds           =  interval_seconds 
 model_config_rec % real_data_init_type        =  real_data_init_type 
 model_config_rec % cen_lat                    =  cen_lat 
 model_config_rec % cen_lon                    =  cen_lon 
 model_config_rec % truelat1                   =  truelat1 
 model_config_rec % truelat2                   =  truelat2 
 model_config_rec % bdyfrq                     =  bdyfrq 
 model_config_rec % iswater                    =  iswater 
 model_config_rec % isice                      =  isice 
 model_config_rec % map_proj                   =  map_proj 
!ENDOFREGISTRYGENERATEDINCLUDE

      CLOSE ( UNIT = 10 , IOSTAT = io_status )

      IF ( io_status .NE. 0 ) THEN
        CALL WRF_ERROR_FATAL ( 'ERROR CLOSING namelist.input' )
      ENDIF

      RETURN
9200  CONTINUE
      CALL wrf_error_fatal( 'module_configure: initial_config: error reading namelist' )

   END SUBROUTINE initial_config

   SUBROUTINE get_config_as_buffer( buffer, buflen, ncopied )
! note that model_config_rec_type must be defined as a sequence derived type
      INTEGER,   INTENT(INOUT) ::  buffer(*)
      INTEGER,   INTENT(IN)    ::  buflen
      INTEGER,   INTENT(OUT)   ::  ncopied
!      TYPE(model_config_rec_type) :: model_config_rec
      INTEGER :: nbytes
      CALL wrf_num_bytes_between ( model_config_rec%last_item_in_struct ,   &
                                   model_config_rec%first_item_in_struct ,  &
                                   nbytes )
!      nbytes = loc(model_config_rec%last_item_in_struct) - &
!               loc(model_config_rec%first_item_in_struct)
      IF ( nbytes .gt. buflen ) THEN
	CALL wrf_error_fatal( "get_config_rec_as_buffer: buffer size to small for config_rec" )
      ENDIF
      CALL wrf_mem_copy( model_config_rec, buffer, nbytes )
      ncopied = nbytes
      RETURN
   END SUBROUTINE get_config_as_buffer

   SUBROUTINE set_config_as_buffer( buffer, buflen )
! note that model_config_rec_type must be defined as a sequence derived type
      INTEGER,   INTENT(INOUT) ::  buffer(*)
      INTEGER,   INTENT(IN)    ::  buflen
!      TYPE(model_config_rec_type) :: model_config_rec
      INTEGER :: nbytes
      CALL wrf_num_bytes_between ( model_config_rec%last_item_in_struct ,  &
                                   model_config_rec%first_item_in_struct , &
                                   nbytes )
!      nbytes = loc(model_config_rec%last_item_in_struct) - &
!               loc(model_config_rec%first_item_in_struct)
      IF ( nbytes .gt. buflen ) THEN
	CALL wrf_error_fatal( "set_config_rec_as_buffer: buffer length too small to fill model config record" )
      ENDIF
      CALL wrf_mem_copy( buffer, model_config_rec, nbytes )
      RETURN
   END SUBROUTINE set_config_as_buffer

   SUBROUTINE model_to_grid_config_rec ( id_id , model_config_rec , grid_config_rec )
      INTEGER , INTENT(IN)                         ::  id_id
      TYPE ( model_config_rec_type ) , INTENT(IN)  ::  model_config_rec
      TYPE ( grid_config_rec_type  ) , INTENT(OUT) ::  grid_config_rec
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/config_assigns.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
! Contains config assign statements for module_domain.F.
 grid_config_rec % time_step                  = model_config_rec % time_step 
 grid_config_rec % time_step_fract_num        = model_config_rec % time_step_fract_num 
 grid_config_rec % time_step_fract_den        = model_config_rec % time_step_fract_den 
 grid_config_rec % restart                    = model_config_rec % restart 
 grid_config_rec % max_dom                    = model_config_rec % max_dom 
 grid_config_rec % dyn_opt                    = model_config_rec % dyn_opt 
 grid_config_rec % rk_ord                     = model_config_rec % rk_ord 
 grid_config_rec % diff_opt                   = model_config_rec % diff_opt 
 grid_config_rec % km_opt                     = model_config_rec % km_opt 
 grid_config_rec % damp_opt                   = model_config_rec % damp_opt 
 grid_config_rec % isfflx                     = model_config_rec % isfflx 
 grid_config_rec % ifsnow                     = model_config_rec % ifsnow 
 grid_config_rec % icloud                     = model_config_rec % icloud 
 grid_config_rec % num_soil_layers            = model_config_rec % num_soil_layers 
 grid_config_rec % num_land_cat               = model_config_rec % num_land_cat 
 grid_config_rec % num_soil_cat               = model_config_rec % num_soil_cat 
 grid_config_rec % spec_bdy_width             = model_config_rec % spec_bdy_width 
 grid_config_rec % spec_zone                  = model_config_rec % spec_zone 
 grid_config_rec % relax_zone                 = model_config_rec % relax_zone 
 grid_config_rec % ensdim                     = model_config_rec % ensdim 
 grid_config_rec % tile_sz_x                  = model_config_rec % tile_sz_x 
 grid_config_rec % tile_sz_y                  = model_config_rec % tile_sz_y 
 grid_config_rec % numtiles                   = model_config_rec % numtiles 
 grid_config_rec % debug_level                = model_config_rec % debug_level 
 grid_config_rec % irand                      = model_config_rec % irand 
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
 grid_config_rec % grid_id                    = model_config_rec % grid_id (id_id)
 grid_config_rec % level                      = model_config_rec % level (id_id)
 grid_config_rec % s_we                       = model_config_rec % s_we (id_id)
 grid_config_rec % e_we                       = model_config_rec % e_we (id_id)
 grid_config_rec % s_sn                       = model_config_rec % s_sn (id_id)
 grid_config_rec % e_sn                       = model_config_rec % e_sn (id_id)
 grid_config_rec % s_vert                     = model_config_rec % s_vert (id_id)
 grid_config_rec % e_vert                     = model_config_rec % e_vert (id_id)
 grid_config_rec % history_interval           = model_config_rec % history_interval (id_id)
 grid_config_rec % auxhist1_interval          = model_config_rec % auxhist1_interval (id_id)
 grid_config_rec % auxhist2_interval          = model_config_rec % auxhist2_interval (id_id)
 grid_config_rec % auxhist3_interval          = model_config_rec % auxhist3_interval (id_id)
 grid_config_rec % auxhist4_interval          = model_config_rec % auxhist4_interval (id_id)
 grid_config_rec % auxhist5_interval          = model_config_rec % auxhist5_interval (id_id)
 grid_config_rec % auxinput1_interval         = model_config_rec % auxinput1_interval (id_id)
 grid_config_rec % auxinput2_interval         = model_config_rec % auxinput2_interval (id_id)
 grid_config_rec % auxinput3_interval         = model_config_rec % auxinput3_interval (id_id)
 grid_config_rec % auxinput4_interval         = model_config_rec % auxinput4_interval (id_id)
 grid_config_rec % auxinput5_interval         = model_config_rec % auxinput5_interval (id_id)
 grid_config_rec % restart_interval           = model_config_rec % restart_interval 
 grid_config_rec % frames_per_outfile         = model_config_rec % frames_per_outfile (id_id)
 grid_config_rec % time_step_sound            = model_config_rec % time_step_sound (id_id)
 grid_config_rec % parent_id                  = model_config_rec % parent_id (id_id)
 grid_config_rec % i_parent_start             = model_config_rec % i_parent_start (id_id)
 grid_config_rec % j_parent_start             = model_config_rec % j_parent_start (id_id)
 grid_config_rec % shw                        = model_config_rec % shw (id_id)
 grid_config_rec % parent_grid_ratio          = model_config_rec % parent_grid_ratio (id_id)
 grid_config_rec % parent_time_step_ratio     = model_config_rec % parent_time_step_ratio (id_id)
 grid_config_rec % moad_grid_ratio            = model_config_rec % moad_grid_ratio (id_id)
 grid_config_rec % moad_time_step_ratio       = model_config_rec % moad_time_step_ratio (id_id)
 grid_config_rec % non_hydrostatic            = model_config_rec % non_hydrostatic (id_id)
 grid_config_rec % dx                         = model_config_rec % dx (id_id)
 grid_config_rec % dy                         = model_config_rec % dy (id_id)
 grid_config_rec % dt                         = model_config_rec % dt (id_id)
 grid_config_rec % ztop                       = model_config_rec % ztop (id_id)
 grid_config_rec % zdamp                      = model_config_rec % zdamp (id_id)
 grid_config_rec % dampcoef                   = model_config_rec % dampcoef (id_id)
 grid_config_rec % smdiv                      = model_config_rec % smdiv (id_id)
 grid_config_rec % emdiv                      = model_config_rec % emdiv (id_id)
 grid_config_rec % epssm                      = model_config_rec % epssm (id_id)
 grid_config_rec % khdif                      = model_config_rec % khdif (id_id)
 grid_config_rec % kvdif                      = model_config_rec % kvdif (id_id)
 grid_config_rec % mix_cr_len                 = model_config_rec % mix_cr_len (id_id)
 grid_config_rec % tke_upper_bound            = model_config_rec % tke_upper_bound (id_id)
 grid_config_rec % kh_tke_upper_bound         = model_config_rec % kh_tke_upper_bound (id_id)
 grid_config_rec % kv_tke_upper_bound         = model_config_rec % kv_tke_upper_bound (id_id)
 grid_config_rec % radt                       = model_config_rec % radt (id_id)
 grid_config_rec % bldt                       = model_config_rec % bldt (id_id)
 grid_config_rec % cudt                       = model_config_rec % cudt (id_id)
 grid_config_rec % gsmdt                      = model_config_rec % gsmdt (id_id)
 grid_config_rec % julyr                      = model_config_rec % julyr (id_id)
 grid_config_rec % julday                     = model_config_rec % julday (id_id)
 grid_config_rec % gmt                        = model_config_rec % gmt (id_id)
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
 grid_config_rec % nested                     = model_config_rec % nested (id_id)
 grid_config_rec % specified                  = model_config_rec % specified (id_id)
 grid_config_rec % top_radiation              = model_config_rec % top_radiation (id_id)
 grid_config_rec % idtad                      = model_config_rec % idtad (id_id)
 grid_config_rec % nsoil                      = model_config_rec % nsoil (id_id)
 grid_config_rec % nphs                       = model_config_rec % nphs (id_id)
 grid_config_rec % ncnvc                      = model_config_rec % ncnvc (id_id)
 grid_config_rec % nrads                      = model_config_rec % nrads (id_id)
 grid_config_rec % nradl                      = model_config_rec % nradl (id_id)
 grid_config_rec % sigma                      = model_config_rec % sigma (id_id)
 grid_config_rec % chem_opt                   = model_config_rec % chem_opt (id_id)
 grid_config_rec % mp_physics                 = model_config_rec % mp_physics (id_id)
 grid_config_rec % ra_lw_physics              = model_config_rec % ra_lw_physics (id_id)
 grid_config_rec % ra_sw_physics              = model_config_rec % ra_sw_physics (id_id)
 grid_config_rec % bl_sfclay_physics          = model_config_rec % bl_sfclay_physics (id_id)
 grid_config_rec % bl_surface_physics         = model_config_rec % bl_surface_physics (id_id)
 grid_config_rec % bl_pbl_physics             = model_config_rec % bl_pbl_physics (id_id)
 grid_config_rec % cu_physics                 = model_config_rec % cu_physics (id_id)
 grid_config_rec % h_mom_adv_order            = model_config_rec % h_mom_adv_order (id_id)
 grid_config_rec % v_mom_adv_order            = model_config_rec % v_mom_adv_order (id_id)
 grid_config_rec % h_sca_adv_order            = model_config_rec % h_sca_adv_order (id_id)
 grid_config_rec % v_sca_adv_order            = model_config_rec % v_sca_adv_order (id_id)
 grid_config_rec % io_form_input              = model_config_rec % io_form_input 
 grid_config_rec % io_form_auxinput1          = model_config_rec % io_form_auxinput1 
 grid_config_rec % io_form_auxinput2          = model_config_rec % io_form_auxinput2 
 grid_config_rec % io_form_auxinput3          = model_config_rec % io_form_auxinput3 
 grid_config_rec % io_form_auxinput4          = model_config_rec % io_form_auxinput4 
 grid_config_rec % io_form_auxinput5          = model_config_rec % io_form_auxinput5 
 grid_config_rec % io_form_history            = model_config_rec % io_form_history 
 grid_config_rec % io_form_auxhist1           = model_config_rec % io_form_auxhist1 
 grid_config_rec % io_form_auxhist2           = model_config_rec % io_form_auxhist2 
 grid_config_rec % io_form_auxhist3           = model_config_rec % io_form_auxhist3 
 grid_config_rec % io_form_auxhist4           = model_config_rec % io_form_auxhist4 
 grid_config_rec % io_form_auxhist5           = model_config_rec % io_form_auxhist5 
 grid_config_rec % io_form_restart            = model_config_rec % io_form_restart 
 grid_config_rec % io_form_boundary           = model_config_rec % io_form_boundary 
 grid_config_rec % interval_seconds           = model_config_rec % interval_seconds 
 grid_config_rec % real_data_init_type        = model_config_rec % real_data_init_type 
 grid_config_rec % cen_lat                    = model_config_rec % cen_lat (id_id)
 grid_config_rec % cen_lon                    = model_config_rec % cen_lon (id_id)
 grid_config_rec % truelat1                   = model_config_rec % truelat1 (id_id)
 grid_config_rec % truelat2                   = model_config_rec % truelat2 (id_id)
 grid_config_rec % bdyfrq                     = model_config_rec % bdyfrq (id_id)
 grid_config_rec % iswater                    = model_config_rec % iswater (id_id)
 grid_config_rec % isice                      = model_config_rec % isice (id_id)
 grid_config_rec % map_proj                   = model_config_rec % map_proj (id_id)
!ENDOFREGISTRYGENERATEDINCLUDE
   END SUBROUTINE model_to_grid_config_rec

! Include the definitions of all the routines that return a namelist values
! back to the driver. These are generated by the registry

   SUBROUTINE init_module_configure
     IMPLICIT NONE
     ! Local vars

     INTEGER i , j

     DO j = 1, max_domains
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/scalar_tables_init.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
  moist_num_table( j ) = 1
  chem_num_table( j ) = 1
!ENDOFREGISTRYGENERATEDINCLUDE
     END DO
   END SUBROUTINE init_module_configure

END MODULE module_configure

! Special (outside registry)
SUBROUTINE get_mminlu ( retval )
  USE module_configure
  CHARACTER(LEN=4)  :: retval
  retval(1:4) = mminlu(1:4)   ! mminlu is defined in module_configure
  RETURN
END SUBROUTINE get_mminlu
SUBROUTINE set_mminlu ( inval )
  USE module_configure
  CHARACTER(LEN=4) :: inval
  mminlu(1:4) = inval(1:4)    ! mminlu is defined in module_configure
  RETURN
END SUBROUTINE set_mminlu

!STARTOFREGISTRYGENERATEDINCLUDE 'inc/get_nl_config.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
SUBROUTINE get_time_step ( time_step )
  USE module_configure
  integer , INTENT(OUT) :: time_step
  time_step = model_config_rec%time_step
  RETURN
END SUBROUTINE get_time_step
SUBROUTINE get_time_step_fract_num ( time_step_fract_num )
  USE module_configure
  integer , INTENT(OUT) :: time_step_fract_num
  time_step_fract_num = model_config_rec%time_step_fract_num
  RETURN
END SUBROUTINE get_time_step_fract_num
SUBROUTINE get_time_step_fract_den ( time_step_fract_den )
  USE module_configure
  integer , INTENT(OUT) :: time_step_fract_den
  time_step_fract_den = model_config_rec%time_step_fract_den
  RETURN
END SUBROUTINE get_time_step_fract_den
SUBROUTINE get_restart ( restart )
  USE module_configure
  logical , INTENT(OUT) :: restart
  restart = model_config_rec%restart
  RETURN
END SUBROUTINE get_restart
SUBROUTINE get_max_dom ( max_dom )
  USE module_configure
  integer , INTENT(OUT) :: max_dom
  max_dom = model_config_rec%max_dom
  RETURN
END SUBROUTINE get_max_dom
SUBROUTINE get_dyn_opt ( dyn_opt )
  USE module_configure
  integer , INTENT(OUT) :: dyn_opt
  dyn_opt = model_config_rec%dyn_opt
  RETURN
END SUBROUTINE get_dyn_opt
SUBROUTINE get_rk_ord ( rk_ord )
  USE module_configure
  integer , INTENT(OUT) :: rk_ord
  rk_ord = model_config_rec%rk_ord
  RETURN
END SUBROUTINE get_rk_ord
SUBROUTINE get_diff_opt ( diff_opt )
  USE module_configure
  integer , INTENT(OUT) :: diff_opt
  diff_opt = model_config_rec%diff_opt
  RETURN
END SUBROUTINE get_diff_opt
SUBROUTINE get_km_opt ( km_opt )
  USE module_configure
  integer , INTENT(OUT) :: km_opt
  km_opt = model_config_rec%km_opt
  RETURN
END SUBROUTINE get_km_opt
SUBROUTINE get_damp_opt ( damp_opt )
  USE module_configure
  integer , INTENT(OUT) :: damp_opt
  damp_opt = model_config_rec%damp_opt
  RETURN
END SUBROUTINE get_damp_opt
SUBROUTINE get_isfflx ( isfflx )
  USE module_configure
  integer , INTENT(OUT) :: isfflx
  isfflx = model_config_rec%isfflx
  RETURN
END SUBROUTINE get_isfflx
SUBROUTINE get_ifsnow ( ifsnow )
  USE module_configure
  integer , INTENT(OUT) :: ifsnow
  ifsnow = model_config_rec%ifsnow
  RETURN
END SUBROUTINE get_ifsnow
SUBROUTINE get_icloud ( icloud )
  USE module_configure
  integer , INTENT(OUT) :: icloud
  icloud = model_config_rec%icloud
  RETURN
END SUBROUTINE get_icloud
SUBROUTINE get_num_soil_layers ( num_soil_layers )
  USE module_configure
  integer , INTENT(OUT) :: num_soil_layers
  num_soil_layers = model_config_rec%num_soil_layers
  RETURN
END SUBROUTINE get_num_soil_layers
SUBROUTINE get_num_land_cat ( num_land_cat )
  USE module_configure
  integer , INTENT(OUT) :: num_land_cat
  num_land_cat = model_config_rec%num_land_cat
  RETURN
END SUBROUTINE get_num_land_cat
SUBROUTINE get_num_soil_cat ( num_soil_cat )
  USE module_configure
  integer , INTENT(OUT) :: num_soil_cat
  num_soil_cat = model_config_rec%num_soil_cat
  RETURN
END SUBROUTINE get_num_soil_cat
SUBROUTINE get_spec_bdy_width ( spec_bdy_width )
  USE module_configure
  integer , INTENT(OUT) :: spec_bdy_width
  spec_bdy_width = model_config_rec%spec_bdy_width
  RETURN
END SUBROUTINE get_spec_bdy_width
SUBROUTINE get_spec_zone ( spec_zone )
  USE module_configure
  integer , INTENT(OUT) :: spec_zone
  spec_zone = model_config_rec%spec_zone
  RETURN
END SUBROUTINE get_spec_zone
SUBROUTINE get_relax_zone ( relax_zone )
  USE module_configure
  integer , INTENT(OUT) :: relax_zone
  relax_zone = model_config_rec%relax_zone
  RETURN
END SUBROUTINE get_relax_zone
SUBROUTINE get_ensdim ( ensdim )
  USE module_configure
  integer , INTENT(OUT) :: ensdim
  ensdim = model_config_rec%ensdim
  RETURN
END SUBROUTINE get_ensdim
SUBROUTINE get_tile_sz_x ( tile_sz_x )
  USE module_configure
  integer , INTENT(OUT) :: tile_sz_x
  tile_sz_x = model_config_rec%tile_sz_x
  RETURN
END SUBROUTINE get_tile_sz_x
SUBROUTINE get_tile_sz_y ( tile_sz_y )
  USE module_configure
  integer , INTENT(OUT) :: tile_sz_y
  tile_sz_y = model_config_rec%tile_sz_y
  RETURN
END SUBROUTINE get_tile_sz_y
SUBROUTINE get_numtiles ( numtiles )
  USE module_configure
  integer , INTENT(OUT) :: numtiles
  numtiles = model_config_rec%numtiles
  RETURN
END SUBROUTINE get_numtiles
SUBROUTINE get_debug_level ( debug_level )
  USE module_configure
  integer , INTENT(OUT) :: debug_level
  debug_level = model_config_rec%debug_level
  RETURN
END SUBROUTINE get_debug_level
SUBROUTINE get_irand ( irand )
  USE module_configure
  integer , INTENT(OUT) :: irand
  irand = model_config_rec%irand
  RETURN
END SUBROUTINE get_irand
SUBROUTINE get_run_days ( run_days )
  USE module_configure
  integer , INTENT(OUT) :: run_days
  run_days = model_config_rec%run_days
  RETURN
END SUBROUTINE get_run_days
SUBROUTINE get_run_hours ( run_hours )
  USE module_configure
  integer , INTENT(OUT) :: run_hours
  run_hours = model_config_rec%run_hours
  RETURN
END SUBROUTINE get_run_hours
SUBROUTINE get_run_minutes ( run_minutes )
  USE module_configure
  integer , INTENT(OUT) :: run_minutes
  run_minutes = model_config_rec%run_minutes
  RETURN
END SUBROUTINE get_run_minutes
SUBROUTINE get_run_seconds ( run_seconds )
  USE module_configure
  integer , INTENT(OUT) :: run_seconds
  run_seconds = model_config_rec%run_seconds
  RETURN
END SUBROUTINE get_run_seconds
SUBROUTINE get_start_year ( id_id , start_year )
  USE module_configure
  integer , INTENT(OUT) :: start_year
  INTEGER id_id
  start_year = model_config_rec%start_year(id_id)
  RETURN
END SUBROUTINE get_start_year
SUBROUTINE get_start_month ( id_id , start_month )
  USE module_configure
  integer , INTENT(OUT) :: start_month
  INTEGER id_id
  start_month = model_config_rec%start_month(id_id)
  RETURN
END SUBROUTINE get_start_month
SUBROUTINE get_start_day ( id_id , start_day )
  USE module_configure
  integer , INTENT(OUT) :: start_day
  INTEGER id_id
  start_day = model_config_rec%start_day(id_id)
  RETURN
END SUBROUTINE get_start_day
SUBROUTINE get_start_hour ( id_id , start_hour )
  USE module_configure
  integer , INTENT(OUT) :: start_hour
  INTEGER id_id
  start_hour = model_config_rec%start_hour(id_id)
  RETURN
END SUBROUTINE get_start_hour
SUBROUTINE get_start_minute ( id_id , start_minute )
  USE module_configure
  integer , INTENT(OUT) :: start_minute
  INTEGER id_id
  start_minute = model_config_rec%start_minute(id_id)
  RETURN
END SUBROUTINE get_start_minute
SUBROUTINE get_start_second ( id_id , start_second )
  USE module_configure
  integer , INTENT(OUT) :: start_second
  INTEGER id_id
  start_second = model_config_rec%start_second(id_id)
  RETURN
END SUBROUTINE get_start_second
SUBROUTINE get_end_year ( id_id , end_year )
  USE module_configure
  integer , INTENT(OUT) :: end_year
  INTEGER id_id
  end_year = model_config_rec%end_year(id_id)
  RETURN
END SUBROUTINE get_end_year
SUBROUTINE get_end_month ( id_id , end_month )
  USE module_configure
  integer , INTENT(OUT) :: end_month
  INTEGER id_id
  end_month = model_config_rec%end_month(id_id)
  RETURN
END SUBROUTINE get_end_month
SUBROUTINE get_end_day ( id_id , end_day )
  USE module_configure
  integer , INTENT(OUT) :: end_day
  INTEGER id_id
  end_day = model_config_rec%end_day(id_id)
  RETURN
END SUBROUTINE get_end_day
SUBROUTINE get_end_hour ( id_id , end_hour )
  USE module_configure
  integer , INTENT(OUT) :: end_hour
  INTEGER id_id
  end_hour = model_config_rec%end_hour(id_id)
  RETURN
END SUBROUTINE get_end_hour
SUBROUTINE get_end_minute ( id_id , end_minute )
  USE module_configure
  integer , INTENT(OUT) :: end_minute
  INTEGER id_id
  end_minute = model_config_rec%end_minute(id_id)
  RETURN
END SUBROUTINE get_end_minute
SUBROUTINE get_end_second ( id_id , end_second )
  USE module_configure
  integer , INTENT(OUT) :: end_second
  INTEGER id_id
  end_second = model_config_rec%end_second(id_id)
  RETURN
END SUBROUTINE get_end_second
SUBROUTINE get_grid_id ( id_id , grid_id )
  USE module_configure
  integer , INTENT(OUT) :: grid_id
  INTEGER id_id
  grid_id = model_config_rec%grid_id(id_id)
  RETURN
END SUBROUTINE get_grid_id
SUBROUTINE get_level ( id_id , level )
  USE module_configure
  integer , INTENT(OUT) :: level
  INTEGER id_id
  level = model_config_rec%level(id_id)
  RETURN
END SUBROUTINE get_level
SUBROUTINE get_s_we ( id_id , s_we )
  USE module_configure
  integer , INTENT(OUT) :: s_we
  INTEGER id_id
  s_we = model_config_rec%s_we(id_id)
  RETURN
END SUBROUTINE get_s_we
SUBROUTINE get_e_we ( id_id , e_we )
  USE module_configure
  integer , INTENT(OUT) :: e_we
  INTEGER id_id
  e_we = model_config_rec%e_we(id_id)
  RETURN
END SUBROUTINE get_e_we
SUBROUTINE get_s_sn ( id_id , s_sn )
  USE module_configure
  integer , INTENT(OUT) :: s_sn
  INTEGER id_id
  s_sn = model_config_rec%s_sn(id_id)
  RETURN
END SUBROUTINE get_s_sn
SUBROUTINE get_e_sn ( id_id , e_sn )
  USE module_configure
  integer , INTENT(OUT) :: e_sn
  INTEGER id_id
  e_sn = model_config_rec%e_sn(id_id)
  RETURN
END SUBROUTINE get_e_sn
SUBROUTINE get_s_vert ( id_id , s_vert )
  USE module_configure
  integer , INTENT(OUT) :: s_vert
  INTEGER id_id
  s_vert = model_config_rec%s_vert(id_id)
  RETURN
END SUBROUTINE get_s_vert
SUBROUTINE get_e_vert ( id_id , e_vert )
  USE module_configure
  integer , INTENT(OUT) :: e_vert
  INTEGER id_id
  e_vert = model_config_rec%e_vert(id_id)
  RETURN
END SUBROUTINE get_e_vert
SUBROUTINE get_history_interval ( id_id , history_interval )
  USE module_configure
  integer , INTENT(OUT) :: history_interval
  INTEGER id_id
  history_interval = model_config_rec%history_interval(id_id)
  RETURN
END SUBROUTINE get_history_interval
SUBROUTINE get_auxhist1_interval ( id_id , auxhist1_interval )
  USE module_configure
  integer , INTENT(OUT) :: auxhist1_interval
  INTEGER id_id
  auxhist1_interval = model_config_rec%auxhist1_interval(id_id)
  RETURN
END SUBROUTINE get_auxhist1_interval
SUBROUTINE get_auxhist2_interval ( id_id , auxhist2_interval )
  USE module_configure
  integer , INTENT(OUT) :: auxhist2_interval
  INTEGER id_id
  auxhist2_interval = model_config_rec%auxhist2_interval(id_id)
  RETURN
END SUBROUTINE get_auxhist2_interval
SUBROUTINE get_auxhist3_interval ( id_id , auxhist3_interval )
  USE module_configure
  integer , INTENT(OUT) :: auxhist3_interval
  INTEGER id_id
  auxhist3_interval = model_config_rec%auxhist3_interval(id_id)
  RETURN
END SUBROUTINE get_auxhist3_interval
SUBROUTINE get_auxhist4_interval ( id_id , auxhist4_interval )
  USE module_configure
  integer , INTENT(OUT) :: auxhist4_interval
  INTEGER id_id
  auxhist4_interval = model_config_rec%auxhist4_interval(id_id)
  RETURN
END SUBROUTINE get_auxhist4_interval
SUBROUTINE get_auxhist5_interval ( id_id , auxhist5_interval )
  USE module_configure
  integer , INTENT(OUT) :: auxhist5_interval
  INTEGER id_id
  auxhist5_interval = model_config_rec%auxhist5_interval(id_id)
  RETURN
END SUBROUTINE get_auxhist5_interval
SUBROUTINE get_auxinput1_interval ( id_id , auxinput1_interval )
  USE module_configure
  integer , INTENT(OUT) :: auxinput1_interval
  INTEGER id_id
  auxinput1_interval = model_config_rec%auxinput1_interval(id_id)
  RETURN
END SUBROUTINE get_auxinput1_interval
SUBROUTINE get_auxinput2_interval ( id_id , auxinput2_interval )
  USE module_configure
  integer , INTENT(OUT) :: auxinput2_interval
  INTEGER id_id
  auxinput2_interval = model_config_rec%auxinput2_interval(id_id)
  RETURN
END SUBROUTINE get_auxinput2_interval
SUBROUTINE get_auxinput3_interval ( id_id , auxinput3_interval )
  USE module_configure
  integer , INTENT(OUT) :: auxinput3_interval
  INTEGER id_id
  auxinput3_interval = model_config_rec%auxinput3_interval(id_id)
  RETURN
END SUBROUTINE get_auxinput3_interval
SUBROUTINE get_auxinput4_interval ( id_id , auxinput4_interval )
  USE module_configure
  integer , INTENT(OUT) :: auxinput4_interval
  INTEGER id_id
  auxinput4_interval = model_config_rec%auxinput4_interval(id_id)
  RETURN
END SUBROUTINE get_auxinput4_interval
SUBROUTINE get_auxinput5_interval ( id_id , auxinput5_interval )
  USE module_configure
  integer , INTENT(OUT) :: auxinput5_interval
  INTEGER id_id
  auxinput5_interval = model_config_rec%auxinput5_interval(id_id)
  RETURN
END SUBROUTINE get_auxinput5_interval
SUBROUTINE get_restart_interval ( restart_interval )
  USE module_configure
  integer , INTENT(OUT) :: restart_interval
  restart_interval = model_config_rec%restart_interval
  RETURN
END SUBROUTINE get_restart_interval
SUBROUTINE get_frames_per_outfile ( id_id , frames_per_outfile )
  USE module_configure
  integer , INTENT(OUT) :: frames_per_outfile
  INTEGER id_id
  frames_per_outfile = model_config_rec%frames_per_outfile(id_id)
  RETURN
END SUBROUTINE get_frames_per_outfile
SUBROUTINE get_time_step_sound ( id_id , time_step_sound )
  USE module_configure
  integer , INTENT(OUT) :: time_step_sound
  INTEGER id_id
  time_step_sound = model_config_rec%time_step_sound(id_id)
  RETURN
END SUBROUTINE get_time_step_sound
SUBROUTINE get_parent_id ( id_id , parent_id )
  USE module_configure
  integer , INTENT(OUT) :: parent_id
  INTEGER id_id
  parent_id = model_config_rec%parent_id(id_id)
  RETURN
END SUBROUTINE get_parent_id
SUBROUTINE get_i_parent_start ( id_id , i_parent_start )
  USE module_configure
  integer , INTENT(OUT) :: i_parent_start
  INTEGER id_id
  i_parent_start = model_config_rec%i_parent_start(id_id)
  RETURN
END SUBROUTINE get_i_parent_start
SUBROUTINE get_j_parent_start ( id_id , j_parent_start )
  USE module_configure
  integer , INTENT(OUT) :: j_parent_start
  INTEGER id_id
  j_parent_start = model_config_rec%j_parent_start(id_id)
  RETURN
END SUBROUTINE get_j_parent_start
SUBROUTINE get_shw ( id_id , shw )
  USE module_configure
  integer , INTENT(OUT) :: shw
  INTEGER id_id
  shw = model_config_rec%shw(id_id)
  RETURN
END SUBROUTINE get_shw
SUBROUTINE get_parent_grid_ratio ( id_id , parent_grid_ratio )
  USE module_configure
  integer , INTENT(OUT) :: parent_grid_ratio
  INTEGER id_id
  parent_grid_ratio = model_config_rec%parent_grid_ratio(id_id)
  RETURN
END SUBROUTINE get_parent_grid_ratio
SUBROUTINE get_parent_time_step_ratio ( id_id , parent_time_step_ratio )
  USE module_configure
  integer , INTENT(OUT) :: parent_time_step_ratio
  INTEGER id_id
  parent_time_step_ratio = model_config_rec%parent_time_step_ratio(id_id)
  RETURN
END SUBROUTINE get_parent_time_step_ratio
SUBROUTINE get_moad_grid_ratio ( id_id , moad_grid_ratio )
  USE module_configure
  integer , INTENT(OUT) :: moad_grid_ratio
  INTEGER id_id
  moad_grid_ratio = model_config_rec%moad_grid_ratio(id_id)
  RETURN
END SUBROUTINE get_moad_grid_ratio
SUBROUTINE get_moad_time_step_ratio ( id_id , moad_time_step_ratio )
  USE module_configure
  integer , INTENT(OUT) :: moad_time_step_ratio
  INTEGER id_id
  moad_time_step_ratio = model_config_rec%moad_time_step_ratio(id_id)
  RETURN
END SUBROUTINE get_moad_time_step_ratio
SUBROUTINE get_non_hydrostatic ( id_id , non_hydrostatic )
  USE module_configure
  logical , INTENT(OUT) :: non_hydrostatic
  INTEGER id_id
  non_hydrostatic = model_config_rec%non_hydrostatic(id_id)
  RETURN
END SUBROUTINE get_non_hydrostatic
SUBROUTINE get_dx ( id_id , dx )
  USE module_configure
  real , INTENT(OUT) :: dx
  INTEGER id_id
  dx = model_config_rec%dx(id_id)
  RETURN
END SUBROUTINE get_dx
SUBROUTINE get_dy ( id_id , dy )
  USE module_configure
  real , INTENT(OUT) :: dy
  INTEGER id_id
  dy = model_config_rec%dy(id_id)
  RETURN
END SUBROUTINE get_dy
SUBROUTINE get_dt ( id_id , dt )
  USE module_configure
  real , INTENT(OUT) :: dt
  INTEGER id_id
  dt = model_config_rec%dt(id_id)
  RETURN
END SUBROUTINE get_dt
SUBROUTINE get_ztop ( id_id , ztop )
  USE module_configure
  real , INTENT(OUT) :: ztop
  INTEGER id_id
  ztop = model_config_rec%ztop(id_id)
  RETURN
END SUBROUTINE get_ztop
SUBROUTINE get_zdamp ( id_id , zdamp )
  USE module_configure
  real , INTENT(OUT) :: zdamp
  INTEGER id_id
  zdamp = model_config_rec%zdamp(id_id)
  RETURN
END SUBROUTINE get_zdamp
SUBROUTINE get_dampcoef ( id_id , dampcoef )
  USE module_configure
  real , INTENT(OUT) :: dampcoef
  INTEGER id_id
  dampcoef = model_config_rec%dampcoef(id_id)
  RETURN
END SUBROUTINE get_dampcoef
SUBROUTINE get_smdiv ( id_id , smdiv )
  USE module_configure
  real , INTENT(OUT) :: smdiv
  INTEGER id_id
  smdiv = model_config_rec%smdiv(id_id)
  RETURN
END SUBROUTINE get_smdiv
SUBROUTINE get_emdiv ( id_id , emdiv )
  USE module_configure
  real , INTENT(OUT) :: emdiv
  INTEGER id_id
  emdiv = model_config_rec%emdiv(id_id)
  RETURN
END SUBROUTINE get_emdiv
SUBROUTINE get_epssm ( id_id , epssm )
  USE module_configure
  real , INTENT(OUT) :: epssm
  INTEGER id_id
  epssm = model_config_rec%epssm(id_id)
  RETURN
END SUBROUTINE get_epssm
SUBROUTINE get_khdif ( id_id , khdif )
  USE module_configure
  real , INTENT(OUT) :: khdif
  INTEGER id_id
  khdif = model_config_rec%khdif(id_id)
  RETURN
END SUBROUTINE get_khdif
SUBROUTINE get_kvdif ( id_id , kvdif )
  USE module_configure
  real , INTENT(OUT) :: kvdif
  INTEGER id_id
  kvdif = model_config_rec%kvdif(id_id)
  RETURN
END SUBROUTINE get_kvdif
SUBROUTINE get_mix_cr_len ( id_id , mix_cr_len )
  USE module_configure
  real , INTENT(OUT) :: mix_cr_len
  INTEGER id_id
  mix_cr_len = model_config_rec%mix_cr_len(id_id)
  RETURN
END SUBROUTINE get_mix_cr_len
SUBROUTINE get_tke_upper_bound ( id_id , tke_upper_bound )
  USE module_configure
  real , INTENT(OUT) :: tke_upper_bound
  INTEGER id_id
  tke_upper_bound = model_config_rec%tke_upper_bound(id_id)
  RETURN
END SUBROUTINE get_tke_upper_bound
SUBROUTINE get_kh_tke_upper_bound ( id_id , kh_tke_upper_bound )
  USE module_configure
  real , INTENT(OUT) :: kh_tke_upper_bound
  INTEGER id_id
  kh_tke_upper_bound = model_config_rec%kh_tke_upper_bound(id_id)
  RETURN
END SUBROUTINE get_kh_tke_upper_bound
SUBROUTINE get_kv_tke_upper_bound ( id_id , kv_tke_upper_bound )
  USE module_configure
  real , INTENT(OUT) :: kv_tke_upper_bound
  INTEGER id_id
  kv_tke_upper_bound = model_config_rec%kv_tke_upper_bound(id_id)
  RETURN
END SUBROUTINE get_kv_tke_upper_bound
SUBROUTINE get_radt ( id_id , radt )
  USE module_configure
  real , INTENT(OUT) :: radt
  INTEGER id_id
  radt = model_config_rec%radt(id_id)
  RETURN
END SUBROUTINE get_radt
SUBROUTINE get_bldt ( id_id , bldt )
  USE module_configure
  real , INTENT(OUT) :: bldt
  INTEGER id_id
  bldt = model_config_rec%bldt(id_id)
  RETURN
END SUBROUTINE get_bldt
SUBROUTINE get_cudt ( id_id , cudt )
  USE module_configure
  real , INTENT(OUT) :: cudt
  INTEGER id_id
  cudt = model_config_rec%cudt(id_id)
  RETURN
END SUBROUTINE get_cudt
SUBROUTINE get_gsmdt ( id_id , gsmdt )
  USE module_configure
  real , INTENT(OUT) :: gsmdt
  INTEGER id_id
  gsmdt = model_config_rec%gsmdt(id_id)
  RETURN
END SUBROUTINE get_gsmdt
SUBROUTINE get_julyr ( id_id , julyr )
  USE module_configure
  integer , INTENT(OUT) :: julyr
  INTEGER id_id
  julyr = model_config_rec%julyr(id_id)
  RETURN
END SUBROUTINE get_julyr
SUBROUTINE get_julday ( id_id , julday )
  USE module_configure
  integer , INTENT(OUT) :: julday
  INTEGER id_id
  julday = model_config_rec%julday(id_id)
  RETURN
END SUBROUTINE get_julday
SUBROUTINE get_gmt ( id_id , gmt )
  USE module_configure
  real , INTENT(OUT) :: gmt
  INTEGER id_id
  gmt = model_config_rec%gmt(id_id)
  RETURN
END SUBROUTINE get_gmt
SUBROUTINE get_periodic_x ( id_id , periodic_x )
  USE module_configure
  logical , INTENT(OUT) :: periodic_x
  INTEGER id_id
  periodic_x = model_config_rec%periodic_x(id_id)
  RETURN
END SUBROUTINE get_periodic_x
SUBROUTINE get_symmetric_xs ( id_id , symmetric_xs )
  USE module_configure
  logical , INTENT(OUT) :: symmetric_xs
  INTEGER id_id
  symmetric_xs = model_config_rec%symmetric_xs(id_id)
  RETURN
END SUBROUTINE get_symmetric_xs
SUBROUTINE get_symmetric_xe ( id_id , symmetric_xe )
  USE module_configure
  logical , INTENT(OUT) :: symmetric_xe
  INTEGER id_id
  symmetric_xe = model_config_rec%symmetric_xe(id_id)
  RETURN
END SUBROUTINE get_symmetric_xe
SUBROUTINE get_open_xs ( id_id , open_xs )
  USE module_configure
  logical , INTENT(OUT) :: open_xs
  INTEGER id_id
  open_xs = model_config_rec%open_xs(id_id)
  RETURN
END SUBROUTINE get_open_xs
SUBROUTINE get_open_xe ( id_id , open_xe )
  USE module_configure
  logical , INTENT(OUT) :: open_xe
  INTEGER id_id
  open_xe = model_config_rec%open_xe(id_id)
  RETURN
END SUBROUTINE get_open_xe
SUBROUTINE get_periodic_y ( id_id , periodic_y )
  USE module_configure
  logical , INTENT(OUT) :: periodic_y
  INTEGER id_id
  periodic_y = model_config_rec%periodic_y(id_id)
  RETURN
END SUBROUTINE get_periodic_y
SUBROUTINE get_symmetric_ys ( id_id , symmetric_ys )
  USE module_configure
  logical , INTENT(OUT) :: symmetric_ys
  INTEGER id_id
  symmetric_ys = model_config_rec%symmetric_ys(id_id)
  RETURN
END SUBROUTINE get_symmetric_ys
SUBROUTINE get_symmetric_ye ( id_id , symmetric_ye )
  USE module_configure
  logical , INTENT(OUT) :: symmetric_ye
  INTEGER id_id
  symmetric_ye = model_config_rec%symmetric_ye(id_id)
  RETURN
END SUBROUTINE get_symmetric_ye
SUBROUTINE get_open_ys ( id_id , open_ys )
  USE module_configure
  logical , INTENT(OUT) :: open_ys
  INTEGER id_id
  open_ys = model_config_rec%open_ys(id_id)
  RETURN
END SUBROUTINE get_open_ys
SUBROUTINE get_open_ye ( id_id , open_ye )
  USE module_configure
  logical , INTENT(OUT) :: open_ye
  INTEGER id_id
  open_ye = model_config_rec%open_ye(id_id)
  RETURN
END SUBROUTINE get_open_ye
SUBROUTINE get_nested ( id_id , nested )
  USE module_configure
  logical , INTENT(OUT) :: nested
  INTEGER id_id
  nested = model_config_rec%nested(id_id)
  RETURN
END SUBROUTINE get_nested
SUBROUTINE get_specified ( id_id , specified )
  USE module_configure
  logical , INTENT(OUT) :: specified
  INTEGER id_id
  specified = model_config_rec%specified(id_id)
  RETURN
END SUBROUTINE get_specified
SUBROUTINE get_top_radiation ( id_id , top_radiation )
  USE module_configure
  logical , INTENT(OUT) :: top_radiation
  INTEGER id_id
  top_radiation = model_config_rec%top_radiation(id_id)
  RETURN
END SUBROUTINE get_top_radiation
SUBROUTINE get_idtad ( id_id , idtad )
  USE module_configure
  integer , INTENT(OUT) :: idtad
  INTEGER id_id
  idtad = model_config_rec%idtad(id_id)
  RETURN
END SUBROUTINE get_idtad
SUBROUTINE get_nsoil ( id_id , nsoil )
  USE module_configure
  integer , INTENT(OUT) :: nsoil
  INTEGER id_id
  nsoil = model_config_rec%nsoil(id_id)
  RETURN
END SUBROUTINE get_nsoil
SUBROUTINE get_nphs ( id_id , nphs )
  USE module_configure
  integer , INTENT(OUT) :: nphs
  INTEGER id_id
  nphs = model_config_rec%nphs(id_id)
  RETURN
END SUBROUTINE get_nphs
SUBROUTINE get_ncnvc ( id_id , ncnvc )
  USE module_configure
  integer , INTENT(OUT) :: ncnvc
  INTEGER id_id
  ncnvc = model_config_rec%ncnvc(id_id)
  RETURN
END SUBROUTINE get_ncnvc
SUBROUTINE get_nrads ( id_id , nrads )
  USE module_configure
  integer , INTENT(OUT) :: nrads
  INTEGER id_id
  nrads = model_config_rec%nrads(id_id)
  RETURN
END SUBROUTINE get_nrads
SUBROUTINE get_nradl ( id_id , nradl )
  USE module_configure
  integer , INTENT(OUT) :: nradl
  INTEGER id_id
  nradl = model_config_rec%nradl(id_id)
  RETURN
END SUBROUTINE get_nradl
SUBROUTINE get_sigma ( id_id , sigma )
  USE module_configure
  integer , INTENT(OUT) :: sigma
  INTEGER id_id
  sigma = model_config_rec%sigma(id_id)
  RETURN
END SUBROUTINE get_sigma
SUBROUTINE get_chem_opt ( id_id , chem_opt )
  USE module_configure
  integer , INTENT(OUT) :: chem_opt
  INTEGER id_id
  chem_opt = model_config_rec%chem_opt(id_id)
  RETURN
END SUBROUTINE get_chem_opt
SUBROUTINE get_mp_physics ( id_id , mp_physics )
  USE module_configure
  integer , INTENT(OUT) :: mp_physics
  INTEGER id_id
  mp_physics = model_config_rec%mp_physics(id_id)
  RETURN
END SUBROUTINE get_mp_physics
SUBROUTINE get_ra_lw_physics ( id_id , ra_lw_physics )
  USE module_configure
  integer , INTENT(OUT) :: ra_lw_physics
  INTEGER id_id
  ra_lw_physics = model_config_rec%ra_lw_physics(id_id)
  RETURN
END SUBROUTINE get_ra_lw_physics
SUBROUTINE get_ra_sw_physics ( id_id , ra_sw_physics )
  USE module_configure
  integer , INTENT(OUT) :: ra_sw_physics
  INTEGER id_id
  ra_sw_physics = model_config_rec%ra_sw_physics(id_id)
  RETURN
END SUBROUTINE get_ra_sw_physics
SUBROUTINE get_bl_sfclay_physics ( id_id , bl_sfclay_physics )
  USE module_configure
  integer , INTENT(OUT) :: bl_sfclay_physics
  INTEGER id_id
  bl_sfclay_physics = model_config_rec%bl_sfclay_physics(id_id)
  RETURN
END SUBROUTINE get_bl_sfclay_physics
SUBROUTINE get_bl_surface_physics ( id_id , bl_surface_physics )
  USE module_configure
  integer , INTENT(OUT) :: bl_surface_physics
  INTEGER id_id
  bl_surface_physics = model_config_rec%bl_surface_physics(id_id)
  RETURN
END SUBROUTINE get_bl_surface_physics
SUBROUTINE get_bl_pbl_physics ( id_id , bl_pbl_physics )
  USE module_configure
  integer , INTENT(OUT) :: bl_pbl_physics
  INTEGER id_id
  bl_pbl_physics = model_config_rec%bl_pbl_physics(id_id)
  RETURN
END SUBROUTINE get_bl_pbl_physics
SUBROUTINE get_cu_physics ( id_id , cu_physics )
  USE module_configure
  integer , INTENT(OUT) :: cu_physics
  INTEGER id_id
  cu_physics = model_config_rec%cu_physics(id_id)
  RETURN
END SUBROUTINE get_cu_physics
SUBROUTINE get_h_mom_adv_order ( id_id , h_mom_adv_order )
  USE module_configure
  integer , INTENT(OUT) :: h_mom_adv_order
  INTEGER id_id
  h_mom_adv_order = model_config_rec%h_mom_adv_order(id_id)
  RETURN
END SUBROUTINE get_h_mom_adv_order
SUBROUTINE get_v_mom_adv_order ( id_id , v_mom_adv_order )
  USE module_configure
  integer , INTENT(OUT) :: v_mom_adv_order
  INTEGER id_id
  v_mom_adv_order = model_config_rec%v_mom_adv_order(id_id)
  RETURN
END SUBROUTINE get_v_mom_adv_order
SUBROUTINE get_h_sca_adv_order ( id_id , h_sca_adv_order )
  USE module_configure
  integer , INTENT(OUT) :: h_sca_adv_order
  INTEGER id_id
  h_sca_adv_order = model_config_rec%h_sca_adv_order(id_id)
  RETURN
END SUBROUTINE get_h_sca_adv_order
SUBROUTINE get_v_sca_adv_order ( id_id , v_sca_adv_order )
  USE module_configure
  integer , INTENT(OUT) :: v_sca_adv_order
  INTEGER id_id
  v_sca_adv_order = model_config_rec%v_sca_adv_order(id_id)
  RETURN
END SUBROUTINE get_v_sca_adv_order
SUBROUTINE get_io_form_input ( io_form_input )
  USE module_configure
  integer , INTENT(OUT) :: io_form_input
  io_form_input = model_config_rec%io_form_input
  RETURN
END SUBROUTINE get_io_form_input
SUBROUTINE get_io_form_auxinput1 ( io_form_auxinput1 )
  USE module_configure
  integer , INTENT(OUT) :: io_form_auxinput1
  io_form_auxinput1 = model_config_rec%io_form_auxinput1
  RETURN
END SUBROUTINE get_io_form_auxinput1
SUBROUTINE get_io_form_auxinput2 ( io_form_auxinput2 )
  USE module_configure
  integer , INTENT(OUT) :: io_form_auxinput2
  io_form_auxinput2 = model_config_rec%io_form_auxinput2
  RETURN
END SUBROUTINE get_io_form_auxinput2
SUBROUTINE get_io_form_auxinput3 ( io_form_auxinput3 )
  USE module_configure
  integer , INTENT(OUT) :: io_form_auxinput3
  io_form_auxinput3 = model_config_rec%io_form_auxinput3
  RETURN
END SUBROUTINE get_io_form_auxinput3
SUBROUTINE get_io_form_auxinput4 ( io_form_auxinput4 )
  USE module_configure
  integer , INTENT(OUT) :: io_form_auxinput4
  io_form_auxinput4 = model_config_rec%io_form_auxinput4
  RETURN
END SUBROUTINE get_io_form_auxinput4
SUBROUTINE get_io_form_auxinput5 ( io_form_auxinput5 )
  USE module_configure
  integer , INTENT(OUT) :: io_form_auxinput5
  io_form_auxinput5 = model_config_rec%io_form_auxinput5
  RETURN
END SUBROUTINE get_io_form_auxinput5
SUBROUTINE get_io_form_history ( io_form_history )
  USE module_configure
  integer , INTENT(OUT) :: io_form_history
  io_form_history = model_config_rec%io_form_history
  RETURN
END SUBROUTINE get_io_form_history
SUBROUTINE get_io_form_auxhist1 ( io_form_auxhist1 )
  USE module_configure
  integer , INTENT(OUT) :: io_form_auxhist1
  io_form_auxhist1 = model_config_rec%io_form_auxhist1
  RETURN
END SUBROUTINE get_io_form_auxhist1
SUBROUTINE get_io_form_auxhist2 ( io_form_auxhist2 )
  USE module_configure
  integer , INTENT(OUT) :: io_form_auxhist2
  io_form_auxhist2 = model_config_rec%io_form_auxhist2
  RETURN
END SUBROUTINE get_io_form_auxhist2
SUBROUTINE get_io_form_auxhist3 ( io_form_auxhist3 )
  USE module_configure
  integer , INTENT(OUT) :: io_form_auxhist3
  io_form_auxhist3 = model_config_rec%io_form_auxhist3
  RETURN
END SUBROUTINE get_io_form_auxhist3
SUBROUTINE get_io_form_auxhist4 ( io_form_auxhist4 )
  USE module_configure
  integer , INTENT(OUT) :: io_form_auxhist4
  io_form_auxhist4 = model_config_rec%io_form_auxhist4
  RETURN
END SUBROUTINE get_io_form_auxhist4
SUBROUTINE get_io_form_auxhist5 ( io_form_auxhist5 )
  USE module_configure
  integer , INTENT(OUT) :: io_form_auxhist5
  io_form_auxhist5 = model_config_rec%io_form_auxhist5
  RETURN
END SUBROUTINE get_io_form_auxhist5
SUBROUTINE get_io_form_restart ( io_form_restart )
  USE module_configure
  integer , INTENT(OUT) :: io_form_restart
  io_form_restart = model_config_rec%io_form_restart
  RETURN
END SUBROUTINE get_io_form_restart
SUBROUTINE get_io_form_boundary ( io_form_boundary )
  USE module_configure
  integer , INTENT(OUT) :: io_form_boundary
  io_form_boundary = model_config_rec%io_form_boundary
  RETURN
END SUBROUTINE get_io_form_boundary
SUBROUTINE get_interval_seconds ( interval_seconds )
  USE module_configure
  integer , INTENT(OUT) :: interval_seconds
  interval_seconds = model_config_rec%interval_seconds
  RETURN
END SUBROUTINE get_interval_seconds
SUBROUTINE get_real_data_init_type ( real_data_init_type )
  USE module_configure
  integer , INTENT(OUT) :: real_data_init_type
  real_data_init_type = model_config_rec%real_data_init_type
  RETURN
END SUBROUTINE get_real_data_init_type
SUBROUTINE get_cen_lat ( id_id , cen_lat )
  USE module_configure
  real , INTENT(OUT) :: cen_lat
  INTEGER id_id
  cen_lat = model_config_rec%cen_lat(id_id)
  RETURN
END SUBROUTINE get_cen_lat
SUBROUTINE get_cen_lon ( id_id , cen_lon )
  USE module_configure
  real , INTENT(OUT) :: cen_lon
  INTEGER id_id
  cen_lon = model_config_rec%cen_lon(id_id)
  RETURN
END SUBROUTINE get_cen_lon
SUBROUTINE get_truelat1 ( id_id , truelat1 )
  USE module_configure
  real , INTENT(OUT) :: truelat1
  INTEGER id_id
  truelat1 = model_config_rec%truelat1(id_id)
  RETURN
END SUBROUTINE get_truelat1
SUBROUTINE get_truelat2 ( id_id , truelat2 )
  USE module_configure
  real , INTENT(OUT) :: truelat2
  INTEGER id_id
  truelat2 = model_config_rec%truelat2(id_id)
  RETURN
END SUBROUTINE get_truelat2
SUBROUTINE get_bdyfrq ( id_id , bdyfrq )
  USE module_configure
  real , INTENT(OUT) :: bdyfrq
  INTEGER id_id
  bdyfrq = model_config_rec%bdyfrq(id_id)
  RETURN
END SUBROUTINE get_bdyfrq
SUBROUTINE get_iswater ( id_id , iswater )
  USE module_configure
  integer , INTENT(OUT) :: iswater
  INTEGER id_id
  iswater = model_config_rec%iswater(id_id)
  RETURN
END SUBROUTINE get_iswater
SUBROUTINE get_isice ( id_id , isice )
  USE module_configure
  integer , INTENT(OUT) :: isice
  INTEGER id_id
  isice = model_config_rec%isice(id_id)
  RETURN
END SUBROUTINE get_isice
SUBROUTINE get_map_proj ( id_id , map_proj )
  USE module_configure
  integer , INTENT(OUT) :: map_proj
  INTEGER id_id
  map_proj = model_config_rec%map_proj(id_id)
  RETURN
END SUBROUTINE get_map_proj
SUBROUTINE set_time_step ( time_step )
  USE module_configure
  integer , INTENT(IN) :: time_step
  model_config_rec%time_step = time_step 
  RETURN
END SUBROUTINE set_time_step
SUBROUTINE set_time_step_fract_num ( time_step_fract_num )
  USE module_configure
  integer , INTENT(IN) :: time_step_fract_num
  model_config_rec%time_step_fract_num = time_step_fract_num 
  RETURN
END SUBROUTINE set_time_step_fract_num
SUBROUTINE set_time_step_fract_den ( time_step_fract_den )
  USE module_configure
  integer , INTENT(IN) :: time_step_fract_den
  model_config_rec%time_step_fract_den = time_step_fract_den 
  RETURN
END SUBROUTINE set_time_step_fract_den
SUBROUTINE set_restart ( restart )
  USE module_configure
  logical , INTENT(IN) :: restart
  model_config_rec%restart = restart 
  RETURN
END SUBROUTINE set_restart
SUBROUTINE set_max_dom ( max_dom )
  USE module_configure
  integer , INTENT(IN) :: max_dom
  model_config_rec%max_dom = max_dom 
  RETURN
END SUBROUTINE set_max_dom
SUBROUTINE set_dyn_opt ( dyn_opt )
  USE module_configure
  integer , INTENT(IN) :: dyn_opt
  model_config_rec%dyn_opt = dyn_opt 
  RETURN
END SUBROUTINE set_dyn_opt
SUBROUTINE set_rk_ord ( rk_ord )
  USE module_configure
  integer , INTENT(IN) :: rk_ord
  model_config_rec%rk_ord = rk_ord 
  RETURN
END SUBROUTINE set_rk_ord
SUBROUTINE set_diff_opt ( diff_opt )
  USE module_configure
  integer , INTENT(IN) :: diff_opt
  model_config_rec%diff_opt = diff_opt 
  RETURN
END SUBROUTINE set_diff_opt
SUBROUTINE set_km_opt ( km_opt )
  USE module_configure
  integer , INTENT(IN) :: km_opt
  model_config_rec%km_opt = km_opt 
  RETURN
END SUBROUTINE set_km_opt
SUBROUTINE set_damp_opt ( damp_opt )
  USE module_configure
  integer , INTENT(IN) :: damp_opt
  model_config_rec%damp_opt = damp_opt 
  RETURN
END SUBROUTINE set_damp_opt
SUBROUTINE set_isfflx ( isfflx )
  USE module_configure
  integer , INTENT(IN) :: isfflx
  model_config_rec%isfflx = isfflx 
  RETURN
END SUBROUTINE set_isfflx
SUBROUTINE set_ifsnow ( ifsnow )
  USE module_configure
  integer , INTENT(IN) :: ifsnow
  model_config_rec%ifsnow = ifsnow 
  RETURN
END SUBROUTINE set_ifsnow
SUBROUTINE set_icloud ( icloud )
  USE module_configure
  integer , INTENT(IN) :: icloud
  model_config_rec%icloud = icloud 
  RETURN
END SUBROUTINE set_icloud
SUBROUTINE set_num_soil_layers ( num_soil_layers )
  USE module_configure
  integer , INTENT(IN) :: num_soil_layers
  model_config_rec%num_soil_layers = num_soil_layers 
  RETURN
END SUBROUTINE set_num_soil_layers
SUBROUTINE set_num_land_cat ( num_land_cat )
  USE module_configure
  integer , INTENT(IN) :: num_land_cat
  model_config_rec%num_land_cat = num_land_cat 
  RETURN
END SUBROUTINE set_num_land_cat
SUBROUTINE set_num_soil_cat ( num_soil_cat )
  USE module_configure
  integer , INTENT(IN) :: num_soil_cat
  model_config_rec%num_soil_cat = num_soil_cat 
  RETURN
END SUBROUTINE set_num_soil_cat
SUBROUTINE set_spec_bdy_width ( spec_bdy_width )
  USE module_configure
  integer , INTENT(IN) :: spec_bdy_width
  model_config_rec%spec_bdy_width = spec_bdy_width 
  RETURN
END SUBROUTINE set_spec_bdy_width
SUBROUTINE set_spec_zone ( spec_zone )
  USE module_configure
  integer , INTENT(IN) :: spec_zone
  model_config_rec%spec_zone = spec_zone 
  RETURN
END SUBROUTINE set_spec_zone
SUBROUTINE set_relax_zone ( relax_zone )
  USE module_configure
  integer , INTENT(IN) :: relax_zone
  model_config_rec%relax_zone = relax_zone 
  RETURN
END SUBROUTINE set_relax_zone
SUBROUTINE set_ensdim ( ensdim )
  USE module_configure
  integer , INTENT(IN) :: ensdim
  model_config_rec%ensdim = ensdim 
  RETURN
END SUBROUTINE set_ensdim
SUBROUTINE set_tile_sz_x ( tile_sz_x )
  USE module_configure
  integer , INTENT(IN) :: tile_sz_x
  model_config_rec%tile_sz_x = tile_sz_x 
  RETURN
END SUBROUTINE set_tile_sz_x
SUBROUTINE set_tile_sz_y ( tile_sz_y )
  USE module_configure
  integer , INTENT(IN) :: tile_sz_y
  model_config_rec%tile_sz_y = tile_sz_y 
  RETURN
END SUBROUTINE set_tile_sz_y
SUBROUTINE set_numtiles ( numtiles )
  USE module_configure
  integer , INTENT(IN) :: numtiles
  model_config_rec%numtiles = numtiles 
  RETURN
END SUBROUTINE set_numtiles
SUBROUTINE set_debug_level ( debug_level )
  USE module_configure
  integer , INTENT(IN) :: debug_level
  model_config_rec%debug_level = debug_level 
  RETURN
END SUBROUTINE set_debug_level
SUBROUTINE set_irand ( irand )
  USE module_configure
  integer , INTENT(IN) :: irand
  model_config_rec%irand = irand 
  RETURN
END SUBROUTINE set_irand
SUBROUTINE set_run_days ( run_days )
  USE module_configure
  integer , INTENT(IN) :: run_days
  model_config_rec%run_days = run_days 
  RETURN
END SUBROUTINE set_run_days
SUBROUTINE set_run_hours ( run_hours )
  USE module_configure
  integer , INTENT(IN) :: run_hours
  model_config_rec%run_hours = run_hours 
  RETURN
END SUBROUTINE set_run_hours
SUBROUTINE set_run_minutes ( run_minutes )
  USE module_configure
  integer , INTENT(IN) :: run_minutes
  model_config_rec%run_minutes = run_minutes 
  RETURN
END SUBROUTINE set_run_minutes
SUBROUTINE set_run_seconds ( run_seconds )
  USE module_configure
  integer , INTENT(IN) :: run_seconds
  model_config_rec%run_seconds = run_seconds 
  RETURN
END SUBROUTINE set_run_seconds
SUBROUTINE set_start_year ( id_id , start_year )
  USE module_configure
  integer , INTENT(IN) :: start_year
  INTEGER id_id
  model_config_rec%start_year(id_id) = start_year
  RETURN
END SUBROUTINE set_start_year
SUBROUTINE set_start_month ( id_id , start_month )
  USE module_configure
  integer , INTENT(IN) :: start_month
  INTEGER id_id
  model_config_rec%start_month(id_id) = start_month
  RETURN
END SUBROUTINE set_start_month
SUBROUTINE set_start_day ( id_id , start_day )
  USE module_configure
  integer , INTENT(IN) :: start_day
  INTEGER id_id
  model_config_rec%start_day(id_id) = start_day
  RETURN
END SUBROUTINE set_start_day
SUBROUTINE set_start_hour ( id_id , start_hour )
  USE module_configure
  integer , INTENT(IN) :: start_hour
  INTEGER id_id
  model_config_rec%start_hour(id_id) = start_hour
  RETURN
END SUBROUTINE set_start_hour
SUBROUTINE set_start_minute ( id_id , start_minute )
  USE module_configure
  integer , INTENT(IN) :: start_minute
  INTEGER id_id
  model_config_rec%start_minute(id_id) = start_minute
  RETURN
END SUBROUTINE set_start_minute
SUBROUTINE set_start_second ( id_id , start_second )
  USE module_configure
  integer , INTENT(IN) :: start_second
  INTEGER id_id
  model_config_rec%start_second(id_id) = start_second
  RETURN
END SUBROUTINE set_start_second
SUBROUTINE set_end_year ( id_id , end_year )
  USE module_configure
  integer , INTENT(IN) :: end_year
  INTEGER id_id
  model_config_rec%end_year(id_id) = end_year
  RETURN
END SUBROUTINE set_end_year
SUBROUTINE set_end_month ( id_id , end_month )
  USE module_configure
  integer , INTENT(IN) :: end_month
  INTEGER id_id
  model_config_rec%end_month(id_id) = end_month
  RETURN
END SUBROUTINE set_end_month
SUBROUTINE set_end_day ( id_id , end_day )
  USE module_configure
  integer , INTENT(IN) :: end_day
  INTEGER id_id
  model_config_rec%end_day(id_id) = end_day
  RETURN
END SUBROUTINE set_end_day
SUBROUTINE set_end_hour ( id_id , end_hour )
  USE module_configure
  integer , INTENT(IN) :: end_hour
  INTEGER id_id
  model_config_rec%end_hour(id_id) = end_hour
  RETURN
END SUBROUTINE set_end_hour
SUBROUTINE set_end_minute ( id_id , end_minute )
  USE module_configure
  integer , INTENT(IN) :: end_minute
  INTEGER id_id
  model_config_rec%end_minute(id_id) = end_minute
  RETURN
END SUBROUTINE set_end_minute
SUBROUTINE set_end_second ( id_id , end_second )
  USE module_configure
  integer , INTENT(IN) :: end_second
  INTEGER id_id
  model_config_rec%end_second(id_id) = end_second
  RETURN
END SUBROUTINE set_end_second
SUBROUTINE set_grid_id ( id_id , grid_id )
  USE module_configure
  integer , INTENT(IN) :: grid_id
  INTEGER id_id
  model_config_rec%grid_id(id_id) = grid_id
  RETURN
END SUBROUTINE set_grid_id
SUBROUTINE set_level ( id_id , level )
  USE module_configure
  integer , INTENT(IN) :: level
  INTEGER id_id
  model_config_rec%level(id_id) = level
  RETURN
END SUBROUTINE set_level
SUBROUTINE set_s_we ( id_id , s_we )
  USE module_configure
  integer , INTENT(IN) :: s_we
  INTEGER id_id
  model_config_rec%s_we(id_id) = s_we
  RETURN
END SUBROUTINE set_s_we
SUBROUTINE set_e_we ( id_id , e_we )
  USE module_configure
  integer , INTENT(IN) :: e_we
  INTEGER id_id
  model_config_rec%e_we(id_id) = e_we
  RETURN
END SUBROUTINE set_e_we
SUBROUTINE set_s_sn ( id_id , s_sn )
  USE module_configure
  integer , INTENT(IN) :: s_sn
  INTEGER id_id
  model_config_rec%s_sn(id_id) = s_sn
  RETURN
END SUBROUTINE set_s_sn
SUBROUTINE set_e_sn ( id_id , e_sn )
  USE module_configure
  integer , INTENT(IN) :: e_sn
  INTEGER id_id
  model_config_rec%e_sn(id_id) = e_sn
  RETURN
END SUBROUTINE set_e_sn
SUBROUTINE set_s_vert ( id_id , s_vert )
  USE module_configure
  integer , INTENT(IN) :: s_vert
  INTEGER id_id
  model_config_rec%s_vert(id_id) = s_vert
  RETURN
END SUBROUTINE set_s_vert
SUBROUTINE set_e_vert ( id_id , e_vert )
  USE module_configure
  integer , INTENT(IN) :: e_vert
  INTEGER id_id
  model_config_rec%e_vert(id_id) = e_vert
  RETURN
END SUBROUTINE set_e_vert
SUBROUTINE set_history_interval ( id_id , history_interval )
  USE module_configure
  integer , INTENT(IN) :: history_interval
  INTEGER id_id
  model_config_rec%history_interval(id_id) = history_interval
  RETURN
END SUBROUTINE set_history_interval
SUBROUTINE set_auxhist1_interval ( id_id , auxhist1_interval )
  USE module_configure
  integer , INTENT(IN) :: auxhist1_interval
  INTEGER id_id
  model_config_rec%auxhist1_interval(id_id) = auxhist1_interval
  RETURN
END SUBROUTINE set_auxhist1_interval
SUBROUTINE set_auxhist2_interval ( id_id , auxhist2_interval )
  USE module_configure
  integer , INTENT(IN) :: auxhist2_interval
  INTEGER id_id
  model_config_rec%auxhist2_interval(id_id) = auxhist2_interval
  RETURN
END SUBROUTINE set_auxhist2_interval
SUBROUTINE set_auxhist3_interval ( id_id , auxhist3_interval )
  USE module_configure
  integer , INTENT(IN) :: auxhist3_interval
  INTEGER id_id
  model_config_rec%auxhist3_interval(id_id) = auxhist3_interval
  RETURN
END SUBROUTINE set_auxhist3_interval
SUBROUTINE set_auxhist4_interval ( id_id , auxhist4_interval )
  USE module_configure
  integer , INTENT(IN) :: auxhist4_interval
  INTEGER id_id
  model_config_rec%auxhist4_interval(id_id) = auxhist4_interval
  RETURN
END SUBROUTINE set_auxhist4_interval
SUBROUTINE set_auxhist5_interval ( id_id , auxhist5_interval )
  USE module_configure
  integer , INTENT(IN) :: auxhist5_interval
  INTEGER id_id
  model_config_rec%auxhist5_interval(id_id) = auxhist5_interval
  RETURN
END SUBROUTINE set_auxhist5_interval
SUBROUTINE set_auxinput1_interval ( id_id , auxinput1_interval )
  USE module_configure
  integer , INTENT(IN) :: auxinput1_interval
  INTEGER id_id
  model_config_rec%auxinput1_interval(id_id) = auxinput1_interval
  RETURN
END SUBROUTINE set_auxinput1_interval
SUBROUTINE set_auxinput2_interval ( id_id , auxinput2_interval )
  USE module_configure
  integer , INTENT(IN) :: auxinput2_interval
  INTEGER id_id
  model_config_rec%auxinput2_interval(id_id) = auxinput2_interval
  RETURN
END SUBROUTINE set_auxinput2_interval
SUBROUTINE set_auxinput3_interval ( id_id , auxinput3_interval )
  USE module_configure
  integer , INTENT(IN) :: auxinput3_interval
  INTEGER id_id
  model_config_rec%auxinput3_interval(id_id) = auxinput3_interval
  RETURN
END SUBROUTINE set_auxinput3_interval
SUBROUTINE set_auxinput4_interval ( id_id , auxinput4_interval )
  USE module_configure
  integer , INTENT(IN) :: auxinput4_interval
  INTEGER id_id
  model_config_rec%auxinput4_interval(id_id) = auxinput4_interval
  RETURN
END SUBROUTINE set_auxinput4_interval
SUBROUTINE set_auxinput5_interval ( id_id , auxinput5_interval )
  USE module_configure
  integer , INTENT(IN) :: auxinput5_interval
  INTEGER id_id
  model_config_rec%auxinput5_interval(id_id) = auxinput5_interval
  RETURN
END SUBROUTINE set_auxinput5_interval
SUBROUTINE set_restart_interval ( restart_interval )
  USE module_configure
  integer , INTENT(IN) :: restart_interval
  model_config_rec%restart_interval = restart_interval 
  RETURN
END SUBROUTINE set_restart_interval
SUBROUTINE set_frames_per_outfile ( id_id , frames_per_outfile )
  USE module_configure
  integer , INTENT(IN) :: frames_per_outfile
  INTEGER id_id
  model_config_rec%frames_per_outfile(id_id) = frames_per_outfile
  RETURN
END SUBROUTINE set_frames_per_outfile
SUBROUTINE set_time_step_sound ( id_id , time_step_sound )
  USE module_configure
  integer , INTENT(IN) :: time_step_sound
  INTEGER id_id
  model_config_rec%time_step_sound(id_id) = time_step_sound
  RETURN
END SUBROUTINE set_time_step_sound
SUBROUTINE set_parent_id ( id_id , parent_id )
  USE module_configure
  integer , INTENT(IN) :: parent_id
  INTEGER id_id
  model_config_rec%parent_id(id_id) = parent_id
  RETURN
END SUBROUTINE set_parent_id
SUBROUTINE set_i_parent_start ( id_id , i_parent_start )
  USE module_configure
  integer , INTENT(IN) :: i_parent_start
  INTEGER id_id
  model_config_rec%i_parent_start(id_id) = i_parent_start
  RETURN
END SUBROUTINE set_i_parent_start
SUBROUTINE set_j_parent_start ( id_id , j_parent_start )
  USE module_configure
  integer , INTENT(IN) :: j_parent_start
  INTEGER id_id
  model_config_rec%j_parent_start(id_id) = j_parent_start
  RETURN
END SUBROUTINE set_j_parent_start
SUBROUTINE set_shw ( id_id , shw )
  USE module_configure
  integer , INTENT(IN) :: shw
  INTEGER id_id
  model_config_rec%shw(id_id) = shw
  RETURN
END SUBROUTINE set_shw
SUBROUTINE set_parent_grid_ratio ( id_id , parent_grid_ratio )
  USE module_configure
  integer , INTENT(IN) :: parent_grid_ratio
  INTEGER id_id
  model_config_rec%parent_grid_ratio(id_id) = parent_grid_ratio
  RETURN
END SUBROUTINE set_parent_grid_ratio
SUBROUTINE set_parent_time_step_ratio ( id_id , parent_time_step_ratio )
  USE module_configure
  integer , INTENT(IN) :: parent_time_step_ratio
  INTEGER id_id
  model_config_rec%parent_time_step_ratio(id_id) = parent_time_step_ratio
  RETURN
END SUBROUTINE set_parent_time_step_ratio
SUBROUTINE set_moad_grid_ratio ( id_id , moad_grid_ratio )
  USE module_configure
  integer , INTENT(IN) :: moad_grid_ratio
  INTEGER id_id
  model_config_rec%moad_grid_ratio(id_id) = moad_grid_ratio
  RETURN
END SUBROUTINE set_moad_grid_ratio
SUBROUTINE set_moad_time_step_ratio ( id_id , moad_time_step_ratio )
  USE module_configure
  integer , INTENT(IN) :: moad_time_step_ratio
  INTEGER id_id
  model_config_rec%moad_time_step_ratio(id_id) = moad_time_step_ratio
  RETURN
END SUBROUTINE set_moad_time_step_ratio
SUBROUTINE set_non_hydrostatic ( id_id , non_hydrostatic )
  USE module_configure
  logical , INTENT(IN) :: non_hydrostatic
  INTEGER id_id
  model_config_rec%non_hydrostatic(id_id) = non_hydrostatic
  RETURN
END SUBROUTINE set_non_hydrostatic
SUBROUTINE set_dx ( id_id , dx )
  USE module_configure
  real , INTENT(IN) :: dx
  INTEGER id_id
  model_config_rec%dx(id_id) = dx
  RETURN
END SUBROUTINE set_dx
SUBROUTINE set_dy ( id_id , dy )
  USE module_configure
  real , INTENT(IN) :: dy
  INTEGER id_id
  model_config_rec%dy(id_id) = dy
  RETURN
END SUBROUTINE set_dy
SUBROUTINE set_dt ( id_id , dt )
  USE module_configure
  real , INTENT(IN) :: dt
  INTEGER id_id
  model_config_rec%dt(id_id) = dt
  RETURN
END SUBROUTINE set_dt
SUBROUTINE set_ztop ( id_id , ztop )
  USE module_configure
  real , INTENT(IN) :: ztop
  INTEGER id_id
  model_config_rec%ztop(id_id) = ztop
  RETURN
END SUBROUTINE set_ztop
SUBROUTINE set_zdamp ( id_id , zdamp )
  USE module_configure
  real , INTENT(IN) :: zdamp
  INTEGER id_id
  model_config_rec%zdamp(id_id) = zdamp
  RETURN
END SUBROUTINE set_zdamp
SUBROUTINE set_dampcoef ( id_id , dampcoef )
  USE module_configure
  real , INTENT(IN) :: dampcoef
  INTEGER id_id
  model_config_rec%dampcoef(id_id) = dampcoef
  RETURN
END SUBROUTINE set_dampcoef
SUBROUTINE set_smdiv ( id_id , smdiv )
  USE module_configure
  real , INTENT(IN) :: smdiv
  INTEGER id_id
  model_config_rec%smdiv(id_id) = smdiv
  RETURN
END SUBROUTINE set_smdiv
SUBROUTINE set_emdiv ( id_id , emdiv )
  USE module_configure
  real , INTENT(IN) :: emdiv
  INTEGER id_id
  model_config_rec%emdiv(id_id) = emdiv
  RETURN
END SUBROUTINE set_emdiv
SUBROUTINE set_epssm ( id_id , epssm )
  USE module_configure
  real , INTENT(IN) :: epssm
  INTEGER id_id
  model_config_rec%epssm(id_id) = epssm
  RETURN
END SUBROUTINE set_epssm
SUBROUTINE set_khdif ( id_id , khdif )
  USE module_configure
  real , INTENT(IN) :: khdif
  INTEGER id_id
  model_config_rec%khdif(id_id) = khdif
  RETURN
END SUBROUTINE set_khdif
SUBROUTINE set_kvdif ( id_id , kvdif )
  USE module_configure
  real , INTENT(IN) :: kvdif
  INTEGER id_id
  model_config_rec%kvdif(id_id) = kvdif
  RETURN
END SUBROUTINE set_kvdif
SUBROUTINE set_mix_cr_len ( id_id , mix_cr_len )
  USE module_configure
  real , INTENT(IN) :: mix_cr_len
  INTEGER id_id
  model_config_rec%mix_cr_len(id_id) = mix_cr_len
  RETURN
END SUBROUTINE set_mix_cr_len
SUBROUTINE set_tke_upper_bound ( id_id , tke_upper_bound )
  USE module_configure
  real , INTENT(IN) :: tke_upper_bound
  INTEGER id_id
  model_config_rec%tke_upper_bound(id_id) = tke_upper_bound
  RETURN
END SUBROUTINE set_tke_upper_bound
SUBROUTINE set_kh_tke_upper_bound ( id_id , kh_tke_upper_bound )
  USE module_configure
  real , INTENT(IN) :: kh_tke_upper_bound
  INTEGER id_id
  model_config_rec%kh_tke_upper_bound(id_id) = kh_tke_upper_bound
  RETURN
END SUBROUTINE set_kh_tke_upper_bound
SUBROUTINE set_kv_tke_upper_bound ( id_id , kv_tke_upper_bound )
  USE module_configure
  real , INTENT(IN) :: kv_tke_upper_bound
  INTEGER id_id
  model_config_rec%kv_tke_upper_bound(id_id) = kv_tke_upper_bound
  RETURN
END SUBROUTINE set_kv_tke_upper_bound
SUBROUTINE set_radt ( id_id , radt )
  USE module_configure
  real , INTENT(IN) :: radt
  INTEGER id_id
  model_config_rec%radt(id_id) = radt
  RETURN
END SUBROUTINE set_radt
SUBROUTINE set_bldt ( id_id , bldt )
  USE module_configure
  real , INTENT(IN) :: bldt
  INTEGER id_id
  model_config_rec%bldt(id_id) = bldt
  RETURN
END SUBROUTINE set_bldt
SUBROUTINE set_cudt ( id_id , cudt )
  USE module_configure
  real , INTENT(IN) :: cudt
  INTEGER id_id
  model_config_rec%cudt(id_id) = cudt
  RETURN
END SUBROUTINE set_cudt
SUBROUTINE set_gsmdt ( id_id , gsmdt )
  USE module_configure
  real , INTENT(IN) :: gsmdt
  INTEGER id_id
  model_config_rec%gsmdt(id_id) = gsmdt
  RETURN
END SUBROUTINE set_gsmdt
SUBROUTINE set_julyr ( id_id , julyr )
  USE module_configure
  integer , INTENT(IN) :: julyr
  INTEGER id_id
  model_config_rec%julyr(id_id) = julyr
  RETURN
END SUBROUTINE set_julyr
SUBROUTINE set_julday ( id_id , julday )
  USE module_configure
  integer , INTENT(IN) :: julday
  INTEGER id_id
  model_config_rec%julday(id_id) = julday
  RETURN
END SUBROUTINE set_julday
SUBROUTINE set_gmt ( id_id , gmt )
  USE module_configure
  real , INTENT(IN) :: gmt
  INTEGER id_id
  model_config_rec%gmt(id_id) = gmt
  RETURN
END SUBROUTINE set_gmt
SUBROUTINE set_periodic_x ( id_id , periodic_x )
  USE module_configure
  logical , INTENT(IN) :: periodic_x
  INTEGER id_id
  model_config_rec%periodic_x(id_id) = periodic_x
  RETURN
END SUBROUTINE set_periodic_x
SUBROUTINE set_symmetric_xs ( id_id , symmetric_xs )
  USE module_configure
  logical , INTENT(IN) :: symmetric_xs
  INTEGER id_id
  model_config_rec%symmetric_xs(id_id) = symmetric_xs
  RETURN
END SUBROUTINE set_symmetric_xs
SUBROUTINE set_symmetric_xe ( id_id , symmetric_xe )
  USE module_configure
  logical , INTENT(IN) :: symmetric_xe
  INTEGER id_id
  model_config_rec%symmetric_xe(id_id) = symmetric_xe
  RETURN
END SUBROUTINE set_symmetric_xe
SUBROUTINE set_open_xs ( id_id , open_xs )
  USE module_configure
  logical , INTENT(IN) :: open_xs
  INTEGER id_id
  model_config_rec%open_xs(id_id) = open_xs
  RETURN
END SUBROUTINE set_open_xs
SUBROUTINE set_open_xe ( id_id , open_xe )
  USE module_configure
  logical , INTENT(IN) :: open_xe
  INTEGER id_id
  model_config_rec%open_xe(id_id) = open_xe
  RETURN
END SUBROUTINE set_open_xe
SUBROUTINE set_periodic_y ( id_id , periodic_y )
  USE module_configure
  logical , INTENT(IN) :: periodic_y
  INTEGER id_id
  model_config_rec%periodic_y(id_id) = periodic_y
  RETURN
END SUBROUTINE set_periodic_y
SUBROUTINE set_symmetric_ys ( id_id , symmetric_ys )
  USE module_configure
  logical , INTENT(IN) :: symmetric_ys
  INTEGER id_id
  model_config_rec%symmetric_ys(id_id) = symmetric_ys
  RETURN
END SUBROUTINE set_symmetric_ys
SUBROUTINE set_symmetric_ye ( id_id , symmetric_ye )
  USE module_configure
  logical , INTENT(IN) :: symmetric_ye
  INTEGER id_id
  model_config_rec%symmetric_ye(id_id) = symmetric_ye
  RETURN
END SUBROUTINE set_symmetric_ye
SUBROUTINE set_open_ys ( id_id , open_ys )
  USE module_configure
  logical , INTENT(IN) :: open_ys
  INTEGER id_id
  model_config_rec%open_ys(id_id) = open_ys
  RETURN
END SUBROUTINE set_open_ys
SUBROUTINE set_open_ye ( id_id , open_ye )
  USE module_configure
  logical , INTENT(IN) :: open_ye
  INTEGER id_id
  model_config_rec%open_ye(id_id) = open_ye
  RETURN
END SUBROUTINE set_open_ye
SUBROUTINE set_nested ( id_id , nested )
  USE module_configure
  logical , INTENT(IN) :: nested
  INTEGER id_id
  model_config_rec%nested(id_id) = nested
  RETURN
END SUBROUTINE set_nested
SUBROUTINE set_specified ( id_id , specified )
  USE module_configure
  logical , INTENT(IN) :: specified
  INTEGER id_id
  model_config_rec%specified(id_id) = specified
  RETURN
END SUBROUTINE set_specified
SUBROUTINE set_top_radiation ( id_id , top_radiation )
  USE module_configure
  logical , INTENT(IN) :: top_radiation
  INTEGER id_id
  model_config_rec%top_radiation(id_id) = top_radiation
  RETURN
END SUBROUTINE set_top_radiation
SUBROUTINE set_idtad ( id_id , idtad )
  USE module_configure
  integer , INTENT(IN) :: idtad
  INTEGER id_id
  model_config_rec%idtad(id_id) = idtad
  RETURN
END SUBROUTINE set_idtad
SUBROUTINE set_nsoil ( id_id , nsoil )
  USE module_configure
  integer , INTENT(IN) :: nsoil
  INTEGER id_id
  model_config_rec%nsoil(id_id) = nsoil
  RETURN
END SUBROUTINE set_nsoil
SUBROUTINE set_nphs ( id_id , nphs )
  USE module_configure
  integer , INTENT(IN) :: nphs
  INTEGER id_id
  model_config_rec%nphs(id_id) = nphs
  RETURN
END SUBROUTINE set_nphs
SUBROUTINE set_ncnvc ( id_id , ncnvc )
  USE module_configure
  integer , INTENT(IN) :: ncnvc
  INTEGER id_id
  model_config_rec%ncnvc(id_id) = ncnvc
  RETURN
END SUBROUTINE set_ncnvc
SUBROUTINE set_nrads ( id_id , nrads )
  USE module_configure
  integer , INTENT(IN) :: nrads
  INTEGER id_id
  model_config_rec%nrads(id_id) = nrads
  RETURN
END SUBROUTINE set_nrads
SUBROUTINE set_nradl ( id_id , nradl )
  USE module_configure
  integer , INTENT(IN) :: nradl
  INTEGER id_id
  model_config_rec%nradl(id_id) = nradl
  RETURN
END SUBROUTINE set_nradl
SUBROUTINE set_sigma ( id_id , sigma )
  USE module_configure
  integer , INTENT(IN) :: sigma
  INTEGER id_id
  model_config_rec%sigma(id_id) = sigma
  RETURN
END SUBROUTINE set_sigma
SUBROUTINE set_chem_opt ( id_id , chem_opt )
  USE module_configure
  integer , INTENT(IN) :: chem_opt
  INTEGER id_id
  model_config_rec%chem_opt(id_id) = chem_opt
  RETURN
END SUBROUTINE set_chem_opt
SUBROUTINE set_mp_physics ( id_id , mp_physics )
  USE module_configure
  integer , INTENT(IN) :: mp_physics
  INTEGER id_id
  model_config_rec%mp_physics(id_id) = mp_physics
  RETURN
END SUBROUTINE set_mp_physics
SUBROUTINE set_ra_lw_physics ( id_id , ra_lw_physics )
  USE module_configure
  integer , INTENT(IN) :: ra_lw_physics
  INTEGER id_id
  model_config_rec%ra_lw_physics(id_id) = ra_lw_physics
  RETURN
END SUBROUTINE set_ra_lw_physics
SUBROUTINE set_ra_sw_physics ( id_id , ra_sw_physics )
  USE module_configure
  integer , INTENT(IN) :: ra_sw_physics
  INTEGER id_id
  model_config_rec%ra_sw_physics(id_id) = ra_sw_physics
  RETURN
END SUBROUTINE set_ra_sw_physics
SUBROUTINE set_bl_sfclay_physics ( id_id , bl_sfclay_physics )
  USE module_configure
  integer , INTENT(IN) :: bl_sfclay_physics
  INTEGER id_id
  model_config_rec%bl_sfclay_physics(id_id) = bl_sfclay_physics
  RETURN
END SUBROUTINE set_bl_sfclay_physics
SUBROUTINE set_bl_surface_physics ( id_id , bl_surface_physics )
  USE module_configure
  integer , INTENT(IN) :: bl_surface_physics
  INTEGER id_id
  model_config_rec%bl_surface_physics(id_id) = bl_surface_physics
  RETURN
END SUBROUTINE set_bl_surface_physics
SUBROUTINE set_bl_pbl_physics ( id_id , bl_pbl_physics )
  USE module_configure
  integer , INTENT(IN) :: bl_pbl_physics
  INTEGER id_id
  model_config_rec%bl_pbl_physics(id_id) = bl_pbl_physics
  RETURN
END SUBROUTINE set_bl_pbl_physics
SUBROUTINE set_cu_physics ( id_id , cu_physics )
  USE module_configure
  integer , INTENT(IN) :: cu_physics
  INTEGER id_id
  model_config_rec%cu_physics(id_id) = cu_physics
  RETURN
END SUBROUTINE set_cu_physics
SUBROUTINE set_h_mom_adv_order ( id_id , h_mom_adv_order )
  USE module_configure
  integer , INTENT(IN) :: h_mom_adv_order
  INTEGER id_id
  model_config_rec%h_mom_adv_order(id_id) = h_mom_adv_order
  RETURN
END SUBROUTINE set_h_mom_adv_order
SUBROUTINE set_v_mom_adv_order ( id_id , v_mom_adv_order )
  USE module_configure
  integer , INTENT(IN) :: v_mom_adv_order
  INTEGER id_id
  model_config_rec%v_mom_adv_order(id_id) = v_mom_adv_order
  RETURN
END SUBROUTINE set_v_mom_adv_order
SUBROUTINE set_h_sca_adv_order ( id_id , h_sca_adv_order )
  USE module_configure
  integer , INTENT(IN) :: h_sca_adv_order
  INTEGER id_id
  model_config_rec%h_sca_adv_order(id_id) = h_sca_adv_order
  RETURN
END SUBROUTINE set_h_sca_adv_order
SUBROUTINE set_v_sca_adv_order ( id_id , v_sca_adv_order )
  USE module_configure
  integer , INTENT(IN) :: v_sca_adv_order
  INTEGER id_id
  model_config_rec%v_sca_adv_order(id_id) = v_sca_adv_order
  RETURN
END SUBROUTINE set_v_sca_adv_order
SUBROUTINE set_io_form_input ( io_form_input )
  USE module_configure
  integer , INTENT(IN) :: io_form_input
  model_config_rec%io_form_input = io_form_input 
  RETURN
END SUBROUTINE set_io_form_input
SUBROUTINE set_io_form_auxinput1 ( io_form_auxinput1 )
  USE module_configure
  integer , INTENT(IN) :: io_form_auxinput1
  model_config_rec%io_form_auxinput1 = io_form_auxinput1 
  RETURN
END SUBROUTINE set_io_form_auxinput1
SUBROUTINE set_io_form_auxinput2 ( io_form_auxinput2 )
  USE module_configure
  integer , INTENT(IN) :: io_form_auxinput2
  model_config_rec%io_form_auxinput2 = io_form_auxinput2 
  RETURN
END SUBROUTINE set_io_form_auxinput2
SUBROUTINE set_io_form_auxinput3 ( io_form_auxinput3 )
  USE module_configure
  integer , INTENT(IN) :: io_form_auxinput3
  model_config_rec%io_form_auxinput3 = io_form_auxinput3 
  RETURN
END SUBROUTINE set_io_form_auxinput3
SUBROUTINE set_io_form_auxinput4 ( io_form_auxinput4 )
  USE module_configure
  integer , INTENT(IN) :: io_form_auxinput4
  model_config_rec%io_form_auxinput4 = io_form_auxinput4 
  RETURN
END SUBROUTINE set_io_form_auxinput4
SUBROUTINE set_io_form_auxinput5 ( io_form_auxinput5 )
  USE module_configure
  integer , INTENT(IN) :: io_form_auxinput5
  model_config_rec%io_form_auxinput5 = io_form_auxinput5 
  RETURN
END SUBROUTINE set_io_form_auxinput5
SUBROUTINE set_io_form_history ( io_form_history )
  USE module_configure
  integer , INTENT(IN) :: io_form_history
  model_config_rec%io_form_history = io_form_history 
  RETURN
END SUBROUTINE set_io_form_history
SUBROUTINE set_io_form_auxhist1 ( io_form_auxhist1 )
  USE module_configure
  integer , INTENT(IN) :: io_form_auxhist1
  model_config_rec%io_form_auxhist1 = io_form_auxhist1 
  RETURN
END SUBROUTINE set_io_form_auxhist1
SUBROUTINE set_io_form_auxhist2 ( io_form_auxhist2 )
  USE module_configure
  integer , INTENT(IN) :: io_form_auxhist2
  model_config_rec%io_form_auxhist2 = io_form_auxhist2 
  RETURN
END SUBROUTINE set_io_form_auxhist2
SUBROUTINE set_io_form_auxhist3 ( io_form_auxhist3 )
  USE module_configure
  integer , INTENT(IN) :: io_form_auxhist3
  model_config_rec%io_form_auxhist3 = io_form_auxhist3 
  RETURN
END SUBROUTINE set_io_form_auxhist3
SUBROUTINE set_io_form_auxhist4 ( io_form_auxhist4 )
  USE module_configure
  integer , INTENT(IN) :: io_form_auxhist4
  model_config_rec%io_form_auxhist4 = io_form_auxhist4 
  RETURN
END SUBROUTINE set_io_form_auxhist4
SUBROUTINE set_io_form_auxhist5 ( io_form_auxhist5 )
  USE module_configure
  integer , INTENT(IN) :: io_form_auxhist5
  model_config_rec%io_form_auxhist5 = io_form_auxhist5 
  RETURN
END SUBROUTINE set_io_form_auxhist5
SUBROUTINE set_io_form_restart ( io_form_restart )
  USE module_configure
  integer , INTENT(IN) :: io_form_restart
  model_config_rec%io_form_restart = io_form_restart 
  RETURN
END SUBROUTINE set_io_form_restart
SUBROUTINE set_io_form_boundary ( io_form_boundary )
  USE module_configure
  integer , INTENT(IN) :: io_form_boundary
  model_config_rec%io_form_boundary = io_form_boundary 
  RETURN
END SUBROUTINE set_io_form_boundary
SUBROUTINE set_interval_seconds ( interval_seconds )
  USE module_configure
  integer , INTENT(IN) :: interval_seconds
  model_config_rec%interval_seconds = interval_seconds 
  RETURN
END SUBROUTINE set_interval_seconds
SUBROUTINE set_real_data_init_type ( real_data_init_type )
  USE module_configure
  integer , INTENT(IN) :: real_data_init_type
  model_config_rec%real_data_init_type = real_data_init_type 
  RETURN
END SUBROUTINE set_real_data_init_type
SUBROUTINE set_cen_lat ( id_id , cen_lat )
  USE module_configure
  real , INTENT(IN) :: cen_lat
  INTEGER id_id
  model_config_rec%cen_lat(id_id) = cen_lat
  RETURN
END SUBROUTINE set_cen_lat
SUBROUTINE set_cen_lon ( id_id , cen_lon )
  USE module_configure
  real , INTENT(IN) :: cen_lon
  INTEGER id_id
  model_config_rec%cen_lon(id_id) = cen_lon
  RETURN
END SUBROUTINE set_cen_lon
SUBROUTINE set_truelat1 ( id_id , truelat1 )
  USE module_configure
  real , INTENT(IN) :: truelat1
  INTEGER id_id
  model_config_rec%truelat1(id_id) = truelat1
  RETURN
END SUBROUTINE set_truelat1
SUBROUTINE set_truelat2 ( id_id , truelat2 )
  USE module_configure
  real , INTENT(IN) :: truelat2
  INTEGER id_id
  model_config_rec%truelat2(id_id) = truelat2
  RETURN
END SUBROUTINE set_truelat2
SUBROUTINE set_bdyfrq ( id_id , bdyfrq )
  USE module_configure
  real , INTENT(IN) :: bdyfrq
  INTEGER id_id
  model_config_rec%bdyfrq(id_id) = bdyfrq
  RETURN
END SUBROUTINE set_bdyfrq
SUBROUTINE set_iswater ( id_id , iswater )
  USE module_configure
  integer , INTENT(IN) :: iswater
  INTEGER id_id
  model_config_rec%iswater(id_id) = iswater
  RETURN
END SUBROUTINE set_iswater
SUBROUTINE set_isice ( id_id , isice )
  USE module_configure
  integer , INTENT(IN) :: isice
  INTEGER id_id
  model_config_rec%isice(id_id) = isice
  RETURN
END SUBROUTINE set_isice
SUBROUTINE set_map_proj ( id_id , map_proj )
  USE module_configure
  integer , INTENT(IN) :: map_proj
  INTEGER id_id
  model_config_rec%map_proj(id_id) = map_proj
  RETURN
END SUBROUTINE set_map_proj
!ENDOFREGISTRYGENERATEDINCLUDE

SUBROUTINE set_scalar_indices_from_config ( idomain , dummy2, dummy1 )
  USE module_driver_constants
  USE module_state_description
  USE module_wrf_error
  USE module_configure
  IMPLICIT NONE
  INTEGER , INTENT(IN)  :: idomain
  INTEGER               :: dummy1
  INTEGER               :: dummy2

!STARTOFREGISTRYGENERATEDINCLUDE 'inc/scalar_indices.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
  P_qv = 1
  P_qc = 1
  P_qr = 1
  P_qi = 1
  P_qs = 1
  P_qg = 1
  IF (model_config_rec%dyn_opt==1)THEN
  END IF
  IF (model_config_rec%dyn_opt==2)THEN
  END IF
  IF (model_config_rec%dyn_opt==3)THEN
  END IF
  IF (model_config_rec%dyn_opt==4)THEN
  END IF
  IF (model_config_rec%dyn_opt==5)THEN
  END IF
  IF (model_config_rec%mp_physics(idomain)==0)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
  END IF
  IF (model_config_rec%mp_physics(idomain)==1)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
  END IF
  IF (model_config_rec%mp_physics(idomain)==2)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   IF ( moist_index_table( PARAM_qg , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qg = moist_num_table(idomain)
     moist_index_table( PARAM_qg , idomain ) = P_qg
   ELSE
     P_qg = moist_index_table( PARAM_qg , idomain )
   END IF
  END IF
  IF (model_config_rec%mp_physics(idomain)==3)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
  END IF
  IF (model_config_rec%mp_physics(idomain)==4)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
  END IF
  IF (model_config_rec%mp_physics(idomain)==5)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
  END IF
  IF (model_config_rec%mp_physics(idomain)==99)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
  END IF
  IF (model_config_rec%ra_lw_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%ra_lw_physics(idomain)==99)THEN
  END IF
  IF (model_config_rec%ra_sw_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%ra_sw_physics(idomain)==2)THEN
  END IF
  IF (model_config_rec%ra_sw_physics(idomain)==99)THEN
  END IF
  IF (model_config_rec%bl_sfclay_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%bl_sfclay_physics(idomain)==2)THEN
  END IF
  IF (model_config_rec%bl_surface_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%bl_surface_physics(idomain)==2)THEN
  END IF
  IF (model_config_rec%bl_surface_physics(idomain)==3)THEN
  END IF
  IF (model_config_rec%bl_surface_physics(idomain)==99)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==2)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==3)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==99)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==2)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==3)THEN
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
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/scalar_indices_init.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
  num_moist = moist_num_table( idomain )
  num_chem = chem_num_table( idomain )
!ENDOFREGISTRYGENERATEDINCLUDE
  RETURN
END SUBROUTINE set_scalar_indices_from_config
