!WRF:DRIVER_LAYER:DOMAIN_OBJECT
!
!  Following are the routines contained within this MODULE:

!  alloc_space_domain                1. Allocate the space for a single domain (constants
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
   USE module_wrf_error
   USE esmf_mod

   CHARACTER (LEN=80) program_name

   !  An entire domain.  This contains multiple meteorological fields by having
   !  arrays (such as "data_3d") of pointers for each field.  Also inside each
   !  domain is a link to a couple of other domains, one is just the "next"
   !  domain that is to be stored, the other is the next domain which happens to
   !  also be on the "same_level".

   TYPE domain_ptr
      TYPE(domain), POINTER :: ptr
   END TYPE domain_ptr

   INTEGER, PARAMETER :: HISTORY_ALARM=1, AUXHIST1_ALARM=2, AUXHIST2_ALARM=3, &
                         AUXHIST3_ALARM=4, AUXHIST4_ALARM=5, AUXHIST5_ALARM=6, &
                         RESTART_ALARM=7, BOUNDARY_ALARM=8, ALARM_SUBTIME=9

   TYPE domain

! SEE THE INCLUDE FILE FOR DEFINITIONS OF STATE FIELDS WITHIN THE DOMAIN DATA STRUCTURE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/state_struct.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
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
integer                                  :: nmm_nclod
integer                                  :: nmm_nprec
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
real                                     :: em_cfn
real                                     :: em_cfn1
real                                     :: em_epsts
integer                                  :: em_step_number
real                                     :: em_u_frame
real                                     :: em_v_frame
real                                     :: em_p_top
integer                                  :: em_stepcu
integer                                  :: em_stepra
integer                                  :: em_stepbl
logical                                  :: em_warm_rain
real                                     :: em_rdx
real                                     :: em_rdy
real                                     :: em_dts
real                                     :: em_dtseps
real                                     :: em_resm
real                                     :: em_zetatop
real                                     :: em_cf1
real                                     :: em_cf2
real                                     :: em_cf3
real                                     :: eh_u_frame
real                                     :: eh_v_frame
real                                     :: eh_p_top
integer                                  :: eh_stepcu
integer                                  :: eh_stepra
integer                                  :: eh_stepbl
logical                                  :: eh_warm_rain
real                                     :: eh_rdx
real                                     :: eh_rdy
real                                     :: eh_dts
real                                     :: eh_dtseps
real                                     :: eh_resm
real                                     :: eh_zetatop
real                                     :: eh_cf1
real                                     :: eh_cf2
real                                     :: eh_cf3
real                                     :: dtbc
integer                                  :: number_at_same_level
integer                                  :: itimestep
integer                                  :: oid
integer                                  :: auxhist1_oid
integer                                  :: auxhist2_oid
integer                                  :: auxhist3_oid
integer                                  :: auxhist4_oid
integer                                  :: auxhist5_oid
integer                                  :: auxinput1_oid
integer                                  :: auxinput2_oid
integer                                  :: auxinput3_oid
integer                                  :: auxinput4_oid
integer                                  :: auxinput5_oid
integer                                  :: nframes
integer                                  :: lbc_fid
logical                                  :: tiled
logical                                  :: patched
logical                                  :: input_from_file
logical                                  :: write_metadata
integer                                  :: time_step
integer                                  :: time_step_fract_num
integer                                  :: time_step_fract_den
logical                                  :: restart
integer                                  :: max_dom
integer                                  :: dyn_opt
integer                                  :: rk_ord
integer                                  :: diff_opt
integer                                  :: km_opt
integer                                  :: damp_opt
integer                                  :: isfflx
integer                                  :: ifsnow
integer                                  :: icloud
integer                                  :: num_soil_layers
integer                                  :: num_land_cat
integer                                  :: num_soil_cat
integer                                  :: spec_bdy_width
integer                                  :: spec_zone
integer                                  :: relax_zone
integer                                  :: ensdim
integer                                  :: tile_sz_x
integer                                  :: tile_sz_y
integer                                  :: numtiles
integer                                  :: debug_level
integer                                  :: irand
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
integer                                  :: grid_id
integer                                  :: level
integer                                  :: s_we
integer                                  :: e_we
integer                                  :: s_sn
integer                                  :: e_sn
integer                                  :: s_vert
integer                                  :: e_vert
integer                                  :: history_interval
integer                                  :: auxhist1_interval
integer                                  :: auxhist2_interval
integer                                  :: auxhist3_interval
integer                                  :: auxhist4_interval
integer                                  :: auxhist5_interval
integer                                  :: auxinput1_interval
integer                                  :: auxinput2_interval
integer                                  :: auxinput3_interval
integer                                  :: auxinput4_interval
integer                                  :: auxinput5_interval
integer                                  :: restart_interval
integer                                  :: frames_per_outfile
integer                                  :: time_step_sound
integer                                  :: parent_id
integer                                  :: i_parent_start
integer                                  :: j_parent_start
integer                                  :: shw
integer                                  :: parent_grid_ratio
integer                                  :: parent_time_step_ratio
integer                                  :: moad_grid_ratio
integer                                  :: moad_time_step_ratio
logical                                  :: non_hydrostatic
real                                     :: dx
real                                     :: dy
real                                     :: dt
real                                     :: ztop
real                                     :: zdamp
real                                     :: dampcoef
real                                     :: smdiv
real                                     :: emdiv
real                                     :: epssm
real                                     :: khdif
real                                     :: kvdif
real                                     :: mix_cr_len
real                                     :: tke_upper_bound
real                                     :: kh_tke_upper_bound
real                                     :: kv_tke_upper_bound
real                                     :: radt
real                                     :: bldt
real                                     :: cudt
real                                     :: gsmdt
integer                                  :: julyr
integer                                  :: julday
real                                     :: gmt
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
logical                                  :: specified
logical                                  :: top_radiation
integer                                  :: idtad
integer                                  :: nsoil
integer                                  :: nphs
integer                                  :: ncnvc
integer                                  :: nrads
integer                                  :: nradl
integer                                  :: sigma
integer                                  :: chem_opt
integer                                  :: mp_physics
integer                                  :: ra_lw_physics
integer                                  :: ra_sw_physics
integer                                  :: bl_sfclay_physics
integer                                  :: bl_surface_physics
integer                                  :: bl_pbl_physics
integer                                  :: cu_physics
integer                                  :: h_mom_adv_order
integer                                  :: v_mom_adv_order
integer                                  :: h_sca_adv_order
integer                                  :: v_sca_adv_order
integer                                  :: io_form_input
integer                                  :: io_form_auxinput1
integer                                  :: io_form_auxinput2
integer                                  :: io_form_auxinput3
integer                                  :: io_form_auxinput4
integer                                  :: io_form_auxinput5
integer                                  :: io_form_history
integer                                  :: io_form_auxhist1
integer                                  :: io_form_auxhist2
integer                                  :: io_form_auxhist3
integer                                  :: io_form_auxhist4
integer                                  :: io_form_auxhist5
integer                                  :: io_form_restart
integer                                  :: io_form_boundary
integer                                  :: interval_seconds
integer                                  :: real_data_init_type
real                                     :: cen_lat
real                                     :: cen_lon
real                                     :: truelat1
real                                     :: truelat2
real                                     :: bdyfrq
integer                                  :: iswater
integer                                  :: isice
integer                                  :: map_proj
real      ,DIMENSION(:,:,:)   ,POINTER   :: exp_x_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: exp_x_2
real      ,DIMENSION(:,:,:,:) ,POINTER   :: nmm_pd_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: nmm_pd_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: nmm_t_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: nmm_t_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: nmm_q_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: nmm_q_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: nmm_u_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: nmm_u_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: nmm_v_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: nmm_v_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: nmm_q2_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: nmm_q2_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: nmm_cwm_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: nmm_cwm_bt
integer   ,DIMENSION(:,:)     ,POINTER   :: nmm_lmh
integer   ,DIMENSION(:,:)     ,POINTER   :: nmm_lmv
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_hbm2
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_hbm3
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_vbm2
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_vbm3
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_sm
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_sice
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_htm
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_vtm
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_pd
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_fis
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_res
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_t
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_q
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_u
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_v
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
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_adt
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_adu
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_adv
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
integer   ,DIMENSION(:,:)     ,POINTER   :: nmm_lpbl
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_ustar
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_z0
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_z0base
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_ths
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_qs
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_twbs
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_qwbs
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_prec
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_aprec
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_acprec
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_cuprec
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
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_t_adj
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_t_old
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_zero_3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_w0avg
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
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_f_ice
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_f_rain
real      ,DIMENSION(:,:,:)   ,POINTER   :: nmm_f_rimef
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_sr
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_u00
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_cfrach
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_cfracl
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_cfracm
integer   ,DIMENSION(:,:)     ,POINTER   :: nmm_lc
integer   ,DIMENSION(:)       ,POINTER   :: nmm_ul
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_islope
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
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_totswdn
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_totlwdn
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_rswin
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
integer   ,DIMENSION(:,:,:)   ,POINTER   :: nmm_indx3_wrk
integer   ,DIMENSION(:)       ,POINTER   :: nmm_n_iup_h
integer   ,DIMENSION(:)       ,POINTER   :: nmm_n_iup_v
integer   ,DIMENSION(:)       ,POINTER   :: nmm_n_iup_adh
integer   ,DIMENSION(:)       ,POINTER   :: nmm_n_iup_adv
integer   ,DIMENSION(:,:)     ,POINTER   :: nmm_iup_h
integer   ,DIMENSION(:,:)     ,POINTER   :: nmm_iup_v
integer   ,DIMENSION(:,:)     ,POINTER   :: nmm_iup_adh
integer   ,DIMENSION(:,:)     ,POINTER   :: nmm_iup_adv
real      ,DIMENSION(:,:,:)   ,POINTER   :: slt_cs
real      ,DIMENSION(:,:,:)   ,POINTER   :: slt_cd
real      ,DIMENSION(:,:,:)   ,POINTER   :: slt_ds
real      ,DIMENSION(:,:,:)   ,POINTER   :: slt_dd
real      ,DIMENSION(:,:,:)   ,POINTER   :: slt_cs_tx
real      ,DIMENSION(:,:,:)   ,POINTER   :: slt_cd_tx
real      ,DIMENSION(:,:,:)   ,POINTER   :: slt_ds_tx
real      ,DIMENSION(:,:,:)   ,POINTER   :: slt_dd_tx
real      ,DIMENSION(:,:,:)   ,POINTER   :: slt_cs_ty
real      ,DIMENSION(:,:,:)   ,POINTER   :: slt_cd_ty
real      ,DIMENSION(:,:,:)   ,POINTER   :: slt_ds_ty
real      ,DIMENSION(:,:,:)   ,POINTER   :: slt_dd_ty
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_ru_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_ru_2
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_u_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_u_2
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_ru_m
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_rv_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_rv_2
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_v_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_v_2
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_rv_m
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_rw_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_rw_2
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_w_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_w_2
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_rrp_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_rrp_2
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_rr_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_rr_2
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_tke_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_tke_2
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_rtp_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_rtp_2
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_tp_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_tp_2
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_t_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_t_2
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_rom_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_rom_2
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_u_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_u_2
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_ru
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_v_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_v_2
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_rv
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_w_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_w_2
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_ww
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_ph_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_ph_2
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_phb
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_ph0
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_php
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_t_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_t_2
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_t_init
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_tp_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_tp_2
real      ,DIMENSION(:,:)     ,POINTER   :: em_mu_1
real      ,DIMENSION(:,:)     ,POINTER   :: em_mu_2
real      ,DIMENSION(:,:)     ,POINTER   :: em_mub
real      ,DIMENSION(:,:)     ,POINTER   :: em_mu0
real      ,DIMENSION(:,:)     ,POINTER   :: em_mudf
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
real      ,DIMENSION(:,:)     ,POINTER   :: em_q2
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
real      ,DIMENSION(:,:,:,:) ,POINTER   :: moist_1
real      ,DIMENSION(:,:,:,:) ,POINTER   :: moist_2
real      ,DIMENSION(:,:,:,:) ,POINTER   :: chem_1
real      ,DIMENSION(:,:,:,:) ,POINTER   :: chem_2
real      ,DIMENSION(:,:,:,:) ,POINTER   :: eh_ru_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: eh_ru_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: eh_rv_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: eh_rv_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: eh_rtp_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: eh_rtp_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: eh_rrp_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: eh_rrp_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: eh_rqv_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: eh_rqv_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_u_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_u_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_v_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_v_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_ph_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_ph_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_t_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_t_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_mu_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_mu_bt
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_rqv_b
real      ,DIMENSION(:,:,:,:) ,POINTER   :: em_rqv_bt
real      ,DIMENSION(:,:,:)   ,POINTER   :: smois
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_pip
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_pp
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_du
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_dv
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_pib
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_r
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_rtb
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_rrb
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_zx
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_zy
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_z
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_pb
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_pb
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_pb8w
real      ,DIMENSION(:,:)     ,POINTER   :: eh_zeta_z
real      ,DIMENSION(:,:)     ,POINTER   :: eh_z_zeta
real      ,DIMENSION(:,:)     ,POINTER   :: eh_cofwr
real      ,DIMENSION(:)       ,POINTER   :: eh_cofrz
real      ,DIMENSION(:)       ,POINTER   :: eh_rdzu
real      ,DIMENSION(:)       ,POINTER   :: eh_rdzw
real      ,DIMENSION(:)       ,POINTER   :: eh_fzm
real      ,DIMENSION(:)       ,POINTER   :: eh_fzp
real      ,DIMENSION(:)       ,POINTER   :: eh_zeta
real      ,DIMENSION(:)       ,POINTER   :: eh_zetaw
real      ,DIMENSION(:)       ,POINTER   :: eh_dzeta
real      ,DIMENSION(:)       ,POINTER   :: eh_dzetaw
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
real      ,DIMENSION(:)       ,POINTER   :: em_fcx
real      ,DIMENSION(:)       ,POINTER   :: em_gcx
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_tslb
real      ,DIMENSION(:)       ,POINTER   :: em_zs
real      ,DIMENSION(:)       ,POINTER   :: em_dzs
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_h_diabatic
real      ,DIMENSION(:,:)     ,POINTER   :: em_msft
real      ,DIMENSION(:,:)     ,POINTER   :: em_msfu
real      ,DIMENSION(:,:)     ,POINTER   :: em_msfv
real      ,DIMENSION(:,:)     ,POINTER   :: em_f
real      ,DIMENSION(:,:)     ,POINTER   :: em_e
real      ,DIMENSION(:,:)     ,POINTER   :: em_sina
real      ,DIMENSION(:,:)     ,POINTER   :: em_cosa
real      ,DIMENSION(:,:)     ,POINTER   :: em_ht
real      ,DIMENSION(:,:)     ,POINTER   :: em_tsk
real      ,DIMENSION(:)       ,POINTER   :: em_u_base
real      ,DIMENSION(:)       ,POINTER   :: em_v_base
real      ,DIMENSION(:)       ,POINTER   :: em_qv_base
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_rthcuten
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_rqvcuten
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_rqrcuten
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_rqccuten
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_rqscuten
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_rqicuten
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_w0avg
real      ,DIMENSION(:,:)     ,POINTER   :: em_rainc
real      ,DIMENSION(:,:)     ,POINTER   :: em_rainnc
real      ,DIMENSION(:,:)     ,POINTER   :: em_raincv
real      ,DIMENSION(:,:)     ,POINTER   :: em_rainncv
real      ,DIMENSION(:,:)     ,POINTER   :: em_rainbl
real      ,DIMENSION(:,:)     ,POINTER   :: em_nca
integer   ,DIMENSION(:,:)     ,POINTER   :: em_lowlyr
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_rthraten
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_rthratenlw
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_rthratensw
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_cldfra
real      ,DIMENSION(:,:)     ,POINTER   :: em_gsw
real      ,DIMENSION(:,:)     ,POINTER   :: em_glw
real      ,DIMENSION(:,:)     ,POINTER   :: em_xlat
real      ,DIMENSION(:,:)     ,POINTER   :: em_xlong
real      ,DIMENSION(:,:)     ,POINTER   :: em_albedo
real      ,DIMENSION(:,:)     ,POINTER   :: em_emiss
real      ,DIMENSION(:,:)     ,POINTER   :: em_lu_index
real      ,DIMENSION(:,:)     ,POINTER   :: em_cldefi
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_rublten
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_rvblten
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_rthblten
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_rqvblten
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_rqcblten
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_rqiblten
real      ,DIMENSION(:,:)     ,POINTER   :: em_tmn
real      ,DIMENSION(:,:)     ,POINTER   :: em_xland
real      ,DIMENSION(:,:)     ,POINTER   :: em_ust
real      ,DIMENSION(:,:)     ,POINTER   :: em_pblh
real      ,DIMENSION(:,:)     ,POINTER   :: em_capg
real      ,DIMENSION(:,:)     ,POINTER   :: em_thc
real      ,DIMENSION(:,:)     ,POINTER   :: em_hfx
real      ,DIMENSION(:,:)     ,POINTER   :: em_qfx
real      ,DIMENSION(:,:)     ,POINTER   :: em_snowc
real      ,DIMENSION(:,:)     ,POINTER   :: em_mavail
real      ,DIMENSION(:,:)     ,POINTER   :: em_tkesfcf
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_taucldi
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_taucldc
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_defor11
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_defor22
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_defor12
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_defor33
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_defor13
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_defor23
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_xkmv
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_xkmh
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_xkmhd
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_xkhv
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_xkhh
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_div
real      ,DIMENSION(:,:,:)   ,POINTER   :: em_bn2
real      ,DIMENSION(:)       ,POINTER   :: eh_fcx
real      ,DIMENSION(:)       ,POINTER   :: eh_gcx
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_tslb
real      ,DIMENSION(:)       ,POINTER   :: eh_zs
real      ,DIMENSION(:)       ,POINTER   :: eh_dzs
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_h_diabatic
real      ,DIMENSION(:,:)     ,POINTER   :: eh_msft
real      ,DIMENSION(:,:)     ,POINTER   :: eh_msfu
real      ,DIMENSION(:,:)     ,POINTER   :: eh_msfv
real      ,DIMENSION(:,:)     ,POINTER   :: eh_f
real      ,DIMENSION(:,:)     ,POINTER   :: eh_e
real      ,DIMENSION(:,:)     ,POINTER   :: eh_sina
real      ,DIMENSION(:,:)     ,POINTER   :: eh_cosa
real      ,DIMENSION(:,:)     ,POINTER   :: eh_ht
real      ,DIMENSION(:,:)     ,POINTER   :: eh_tsk
real      ,DIMENSION(:)       ,POINTER   :: eh_u_base
real      ,DIMENSION(:)       ,POINTER   :: eh_v_base
real      ,DIMENSION(:)       ,POINTER   :: eh_qv_base
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_rthcuten
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_rqvcuten
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_rqrcuten
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_rqccuten
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_rqscuten
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_rqicuten
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_w0avg
real      ,DIMENSION(:,:)     ,POINTER   :: eh_rainc
real      ,DIMENSION(:,:)     ,POINTER   :: eh_rainnc
real      ,DIMENSION(:,:)     ,POINTER   :: eh_raincv
real      ,DIMENSION(:,:)     ,POINTER   :: eh_rainncv
real      ,DIMENSION(:,:)     ,POINTER   :: eh_rainbl
real      ,DIMENSION(:,:)     ,POINTER   :: eh_nca
integer   ,DIMENSION(:,:)     ,POINTER   :: eh_lowlyr
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_rthraten
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_rthratenlw
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_rthratensw
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_cldfra
real      ,DIMENSION(:,:)     ,POINTER   :: eh_gsw
real      ,DIMENSION(:,:)     ,POINTER   :: eh_glw
real      ,DIMENSION(:,:)     ,POINTER   :: eh_xlat
real      ,DIMENSION(:,:)     ,POINTER   :: eh_xlong
real      ,DIMENSION(:,:)     ,POINTER   :: eh_albedo
real      ,DIMENSION(:,:)     ,POINTER   :: eh_emiss
real      ,DIMENSION(:,:)     ,POINTER   :: eh_lu_index
real      ,DIMENSION(:,:)     ,POINTER   :: eh_cldefi
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_rublten
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_rvblten
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_rthblten
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_rqvblten
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_rqcblten
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_rqiblten
real      ,DIMENSION(:,:)     ,POINTER   :: eh_tmn
real      ,DIMENSION(:,:)     ,POINTER   :: eh_xland
real      ,DIMENSION(:,:)     ,POINTER   :: eh_ust
real      ,DIMENSION(:,:)     ,POINTER   :: eh_pblh
real      ,DIMENSION(:,:)     ,POINTER   :: eh_capg
real      ,DIMENSION(:,:)     ,POINTER   :: eh_thc
real      ,DIMENSION(:,:)     ,POINTER   :: eh_hfx
real      ,DIMENSION(:,:)     ,POINTER   :: eh_qfx
real      ,DIMENSION(:,:)     ,POINTER   :: eh_snowc
real      ,DIMENSION(:,:)     ,POINTER   :: eh_mavail
real      ,DIMENSION(:,:)     ,POINTER   :: eh_tkesfcf
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_taucldi
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_taucldc
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_defor11
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_defor22
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_defor12
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_defor33
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_defor13
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_defor23
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_xkmv
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_xkmh
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_xkmhd
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_xkhv
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_xkhh
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_div
real      ,DIMENSION(:,:,:)   ,POINTER   :: eh_bn2
real      ,DIMENSION(:,:)     ,POINTER   :: eh_q2
real      ,DIMENSION(:,:)     ,POINTER   :: th2
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
real      ,DIMENSION(:,:)     ,POINTER   :: snow
real      ,DIMENSION(:,:)     ,POINTER   :: canwat
real      ,DIMENSION(:,:)     ,POINTER   :: sst
real      ,DIMENSION(:,:)     ,POINTER   :: weasd
real      ,DIMENSION(:,:)     ,POINTER   :: mol
real      ,DIMENSION(:,:)     ,POINTER   :: znt
real      ,DIMENSION(:,:,:)   ,POINTER   :: tke_myj
real      ,DIMENSION(:,:)     ,POINTER   :: thz0
real      ,DIMENSION(:,:)     ,POINTER   :: qz0
real      ,DIMENSION(:,:)     ,POINTER   :: uz0
real      ,DIMENSION(:,:)     ,POINTER   :: vz0
real      ,DIMENSION(:,:)     ,POINTER   :: uz0h
real      ,DIMENSION(:,:)     ,POINTER   :: vz0h
real      ,DIMENSION(:,:,:)   ,POINTER   :: dudt
real      ,DIMENSION(:,:,:)   ,POINTER   :: dvdt
real      ,DIMENSION(:,:)     ,POINTER   :: qsfc
real      ,DIMENSION(:,:)     ,POINTER   :: akhs
real      ,DIMENSION(:,:)     ,POINTER   :: akms
real      ,DIMENSION(:,:)     ,POINTER   :: htop
real      ,DIMENSION(:,:)     ,POINTER   :: hbot
real      ,DIMENSION(:,:)     ,POINTER   :: cuppt
real      ,DIMENSION(:,:,:)   ,POINTER   :: t0eta
real      ,DIMENSION(:,:,:)   ,POINTER   :: q0eta
real      ,DIMENSION(:,:,:)   ,POINTER   :: p0eta
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
real      ,DIMENSION(:,:,:)   ,POINTER   :: smfr3d
!ENDOFREGISTRYGENERATEDINCLUDE

      INTEGER                                             :: comms( max_comms )

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

      TYPE(domain) , POINTER                              :: next
      TYPE(domain) , POINTER                              :: same_level

      LOGICAL      , DIMENSION ( 4 )                      :: bdy_mask         ! which boundaries are on processor


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
      Type(ESMF_Clock)                                    :: domain_clock
      Type(ESMF_Time)                                     :: start_time, stop_time, current_time
      Type(ESMF_Time)                                     :: start_subtime, stop_subtime
      Type(ESMF_Time)                                     :: this_bdy_time, next_bdy_time
      Type(ESMF_TimeInterval) :: step_time
      Type(ESMF_Alarm), pointer :: alarms(:)

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
   
CONTAINS

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

   CALL get_spec_bdy_width( spec_bdy_width )


   bdy_mask = .true.     ! only one processor so all 4 boundaries are there

! this is a trivial version -- 1 patch per processor; 
! use version in module_dm to compute for DM
   sp1 = sd1 ; sp2 = sd2 ; sp3 = sd3
   ep1 = ed1 ; ep2 = ed2 ; ep3 = ed3
   SELECT CASE ( model_data_order )
      CASE ( DATA_ORDER_XYZ )
         sm1  = sp1 - bdx ; em1 = ep1 + bdx
         sm2  = sp2 - bdy ; em2 = ep2 + bdy
         sm3  = sp3       ; em3 = ep3
      CASE ( DATA_ORDER_YXZ )
         sm1 = sp1 - bdy ; em1 = ep1 + bdy
         sm2 = sp2 - bdx ; em2 = ep2 + bdx
         sm3 = sp3       ; em3 = ep3
      CASE ( DATA_ORDER_ZXY )
         sm1 = sp1       ; em1 = ep1
         sm2 = sp2 - bdx ; em2 = ep2 + bdx
         sm3 = sp3 - bdy ; em3 = ep3 + bdy
      CASE ( DATA_ORDER_ZYX )
         sm1 = sp1       ; em1 = ep1
         sm2 = sp2 - bdy ; em2 = ep2 + bdy
         sm3 = sp3 - bdx ; em3 = ep3 + bdx
      CASE ( DATA_ORDER_XZY )
         sm1 = sp1 - bdx ; em1 = ep1 + bdx
         sm2 = sp2       ; em2 = ep2
         sm3 = sp3 - bdy ; em3 = ep3 + bdy
      CASE ( DATA_ORDER_YZX )
         sm1 = sp1 - bdy ; em1 = ep1 + bdy
         sm2 = sp2       ; em2 = ep2
         sm3 = sp3 - bdx ; em3 = ep3 + bdx
   END SELECT
   sm1x = sm1       ; em1x = em1    ! just copy
   sm2x = sm2       ; em2x = em2
   sm3x = sm3       ; em3x = em3
   sm1y = sm1       ; em1y = em1    ! just copy
   sm2y = sm2       ; em2y = em2
   sm3y = sm3       ; em3y = em3
! assigns mostly just to suppress warning messages that INTENT OUT vars not assigned
   sp1x = sp1 ; ep1x = ep1 ; sp2x = sp2 ; ep2x = ep2 ; sp3x = sp3 ; ep3x = ep3
   sp1y = sp1 ; ep1y = ep1 ; sp2y = sp2 ; ep2y = ep2 ; sp3y = sp3 ; ep3y = ep3


   RETURN
   END SUBROUTINE wrf_patch_domain


!

!  This subroutine is used to ALLOCATE the separate TYPE(domain) space for
!  each domain that is required.  This routine is called initially from a 
!  routine after the input information is available.  This routine is also
!  called during the model run whenever new domains are requested.

!  This routine uses the information contained in the MODULE declarations,
!  most important of which is the pointer to the head of the linked list
!  ("head_grid").  All of the data in the argument list is input that is
!  transferred to the domain for permanent storage.  The pointers are
!  nullified prior to use.

!  This routine calls the SUBROUTINE to ALLOCATE the space for each of the
!  required meteorological fields, alloc_space_fields.

   SUBROUTINE alloc_and_configure_domain ( domain_id , grid , parent, kid )
      
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


! This next step uses information that is listed in the registry as namelist_derived
! to properly size the domain and the patches; this in turn is stored in the new_grid
! data structure


      data_ordering : SELECT CASE ( model_data_order )
        CASE  ( DATA_ORDER_XYZ )

          CALL get_s_we( domain_id , sd1 )
          CALL get_e_we( domain_id , ed1 )
          CALL get_s_sn( domain_id , sd2 )
          CALL get_e_sn( domain_id , ed2 )
          CALL get_s_vert( domain_id , sd3 )
          CALL get_e_vert( domain_id , ed3 )

        CASE  ( DATA_ORDER_YXZ )

          CALL get_s_sn( domain_id , sd1 )
          CALL get_e_sn( domain_id , ed1 )
          CALL get_s_we( domain_id , sd2 )
          CALL get_e_we( domain_id , ed2 )
          CALL get_s_vert( domain_id , sd3 )
          CALL get_e_vert( domain_id , ed3 )

        CASE  ( DATA_ORDER_ZXY )

          CALL get_s_vert( domain_id , sd1 )
          CALL get_e_vert( domain_id , ed1 )
          CALL get_s_we( domain_id , sd2 )
          CALL get_e_we( domain_id , ed2 )
          CALL get_s_sn( domain_id , sd3 )
          CALL get_e_sn( domain_id , ed3 )

        CASE  ( DATA_ORDER_ZYX )

          CALL get_s_vert( domain_id , sd1 )
          CALL get_e_vert( domain_id , ed1 )
          CALL get_s_sn( domain_id , sd2 )
          CALL get_e_sn( domain_id , ed2 )
          CALL get_s_we( domain_id , sd3 )
          CALL get_e_we( domain_id , ed3 )

        CASE  ( DATA_ORDER_XZY )

          CALL get_s_we( domain_id , sd1 )
          CALL get_e_we( domain_id , ed1 )
          CALL get_s_vert( domain_id , sd2 )
          CALL get_e_vert( domain_id , ed2 )
          CALL get_s_sn( domain_id , sd3 )
          CALL get_e_sn( domain_id , ed3 )

        CASE  ( DATA_ORDER_YZX )

          CALL get_s_sn( domain_id , sd1 )
          CALL get_e_sn( domain_id , ed1 )
          CALL get_s_vert( domain_id , sd2 )
          CALL get_e_vert( domain_id , ed2 )
          CALL get_s_we( domain_id , sd3 )
          CALL get_e_we( domain_id , ed3 )

      END SELECT data_ordering


      if ( num_time_levels > 3 ) then
        WRITE ( wrf_err_message , * ) 'module_domain: alloc_and_configure_domain: Incorrect value for num_time_levels ', &
                                       num_time_levels
        CALL wrf_error_fatal ( TRIM ( wrf_err_message ) )
      endif

      IF (ASSOCIATED(parent)) THEN
        parent_id = parent%id
        parent_domdesc = parent%domdesc
      ELSE
        parent_id = -1
        parent_domdesc = -1
      ENDIF

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

write(0,*)'calling alloc_space_field in module_domain'
!	call summary()
      CALL alloc_space_field ( new_grid, domain_id ,                   &
                               sd1, ed1, sd2, ed2, sd3, ed3, &
                               sm1,  em1,  sm2,  em2,  sm3,  em3, &
                               sm1x, em1x, sm2x, em2x, sm3x, em3x, &   ! x-xpose
                               sm1y, em1y, sm2y, em2y, sm3y, em3y  &   ! y-xpose
      )
write(0,*)'back from alloc_space_field in module_domain'


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


   END SUBROUTINE alloc_and_configure_domain

!

!  This routine ALLOCATEs the required space for the meteorological fields
!  for a specific domain.  The fields are simply ALLOCATEd as an -1.  They
!  are referenced as wind, temperature, moisture, etc. in routines that are
!  below this top-level of data allocation and management (in the solve routine
!  and below).

   SUBROUTINE alloc_space_field ( grid,   id,                         &
                                  sd31, ed31, sd32, ed32, sd33, ed33, &
                                  sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                  sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                  sm31y, em31y, sm32y, em32y, sm33y, em33y )

      
      USE module_configure
      IMPLICIT NONE
 

      !  Input data.

      TYPE(domain)               , POINTER          :: grid
      INTEGER , INTENT(IN)            :: id
      INTEGER , INTENT(IN)            :: sd31, ed31, sd32, ed32, sd33, ed33
      INTEGER , INTENT(IN)            :: sm31, em31, sm32, em32, sm33, em33
      INTEGER , INTENT(IN)            :: sm31x, em31x, sm32x, em32x, sm33x, em33x
      INTEGER , INTENT(IN)            :: sm31y, em31y, sm32y, em32y, sm33y, em33y

      !  Local data.
      INTEGER dyn_opt, idum1, idum2, spec_bdy_width
      INTEGER num_bytes_allocated
      REAL    initial_data_value
      CHARACTER (LEN=256) message

      !declare ierr variable for error checking ALLOCATE calls
      INTEGER ierr

      INTEGER                              :: loop

      CALL get_initial_data_value ( initial_data_value )

      CALL get_dyn_opt( dyn_opt )
      CALL get_spec_bdy_width( spec_bdy_width )

      CALL set_scalar_indices_from_config( id , idum1 , idum2 )

      num_bytes_allocated = 0 


      IF      ( .FALSE. )           THEN

      ELSE IF ( dyn_opt == DYN_NMM ) THEN
        CALL wrf_message ( 'DYNAMICS OPTION: nmm dyncore' )
!	call summary()
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/nmm_allocs.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
ALLOCATE(grid%nmm_pd_b(max(ed31,ed33),1,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_pd_b(max(ed31,ed33),1,spec_bdy_width,4). ')
 endif
  grid%nmm_pd_b=initial_data_value
ALLOCATE(grid%nmm_pd_bt(max(ed31,ed33),1,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_pd_bt(max(ed31,ed33),1,spec_bdy_width,4). ')
 endif
  grid%nmm_pd_bt=initial_data_value
ALLOCATE(grid%nmm_t_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_t_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  grid%nmm_t_b=initial_data_value
ALLOCATE(grid%nmm_t_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_t_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  grid%nmm_t_bt=initial_data_value
ALLOCATE(grid%nmm_q_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_q_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  grid%nmm_q_b=initial_data_value
ALLOCATE(grid%nmm_q_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_q_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  grid%nmm_q_bt=initial_data_value
ALLOCATE(grid%nmm_u_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_u_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  grid%nmm_u_b=initial_data_value
ALLOCATE(grid%nmm_u_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_u_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  grid%nmm_u_bt=initial_data_value
ALLOCATE(grid%nmm_v_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_v_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  grid%nmm_v_b=initial_data_value
ALLOCATE(grid%nmm_v_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_v_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  grid%nmm_v_bt=initial_data_value
ALLOCATE(grid%nmm_q2_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_q2_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  grid%nmm_q2_b=initial_data_value
ALLOCATE(grid%nmm_q2_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_q2_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  grid%nmm_q2_bt=initial_data_value
ALLOCATE(grid%nmm_cwm_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_cwm_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  grid%nmm_cwm_b=initial_data_value
ALLOCATE(grid%nmm_cwm_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_cwm_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  grid%nmm_cwm_bt=initial_data_value
ALLOCATE(grid%nmm_lmh(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_lmh(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_lmh=0
ALLOCATE(grid%nmm_lmv(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_lmv(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_lmv=0
ALLOCATE(grid%nmm_hbm2(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_hbm2(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_hbm2=initial_data_value
ALLOCATE(grid%nmm_hbm3(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_hbm3(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_hbm3=initial_data_value
ALLOCATE(grid%nmm_vbm2(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_vbm2(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_vbm2=initial_data_value
ALLOCATE(grid%nmm_vbm3(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_vbm3(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_vbm3=initial_data_value
ALLOCATE(grid%nmm_sm(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_sm(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_sm=initial_data_value
ALLOCATE(grid%nmm_sice(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_sice(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_sice=initial_data_value
ALLOCATE(grid%nmm_htm(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_htm(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%nmm_htm=initial_data_value
ALLOCATE(grid%nmm_vtm(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_vtm(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%nmm_vtm=initial_data_value
ALLOCATE(grid%nmm_pd(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_pd(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_pd=initial_data_value
ALLOCATE(grid%nmm_fis(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_fis(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_fis=initial_data_value
ALLOCATE(grid%nmm_res(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_res(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_res=initial_data_value
ALLOCATE(grid%nmm_t(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_t(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%nmm_t=initial_data_value
ALLOCATE(grid%nmm_q(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_q(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%nmm_q=initial_data_value
ALLOCATE(grid%nmm_u(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_u(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%nmm_u=initial_data_value
ALLOCATE(grid%nmm_v(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_v(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%nmm_v=initial_data_value
ALLOCATE(grid%nmm_told(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_told(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%nmm_told=initial_data_value
ALLOCATE(grid%nmm_uold(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_uold(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%nmm_uold=initial_data_value
ALLOCATE(grid%nmm_vold(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_vold(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%nmm_vold=initial_data_value
ALLOCATE(grid%nmm_dx_nmm(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_dx_nmm(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_dx_nmm=initial_data_value
ALLOCATE(grid%nmm_wpdar(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_wpdar(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_wpdar=initial_data_value
ALLOCATE(grid%nmm_cpgfu(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_cpgfu(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_cpgfu=initial_data_value
ALLOCATE(grid%nmm_curv(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_curv(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_curv=initial_data_value
ALLOCATE(grid%nmm_fcp(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_fcp(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_fcp=initial_data_value
ALLOCATE(grid%nmm_fdiv(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_fdiv(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_fdiv=initial_data_value
ALLOCATE(grid%nmm_f(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_f(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_f=initial_data_value
ALLOCATE(grid%nmm_fad(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_fad(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_fad=initial_data_value
ALLOCATE(grid%nmm_ddmpu(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_ddmpu(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_ddmpu=initial_data_value
ALLOCATE(grid%nmm_ddmpv(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_ddmpv(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_ddmpv=initial_data_value
ALLOCATE(grid%nmm_deta(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_deta(sm32:em32). ')
 endif
  grid%nmm_deta=initial_data_value
ALLOCATE(grid%nmm_rdeta(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_rdeta(sm32:em32). ')
 endif
  grid%nmm_rdeta=initial_data_value
ALLOCATE(grid%nmm_aeta(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_aeta(sm32:em32). ')
 endif
  grid%nmm_aeta=initial_data_value
ALLOCATE(grid%nmm_f4q2(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_f4q2(sm32:em32). ')
 endif
  grid%nmm_f4q2=initial_data_value
ALLOCATE(grid%nmm_etax(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_etax(sm32:em32). ')
 endif
  grid%nmm_etax=initial_data_value
ALLOCATE(grid%nmm_dfl(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_dfl(sm32:em32). ')
 endif
  grid%nmm_dfl=initial_data_value
ALLOCATE(grid%nmm_deta1(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_deta1(sm32:em32). ')
 endif
  grid%nmm_deta1=initial_data_value
ALLOCATE(grid%nmm_aeta1(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_aeta1(sm32:em32). ')
 endif
  grid%nmm_aeta1=initial_data_value
ALLOCATE(grid%nmm_eta1(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_eta1(sm32:em32). ')
 endif
  grid%nmm_eta1=initial_data_value
ALLOCATE(grid%nmm_deta2(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_deta2(sm32:em32). ')
 endif
  grid%nmm_deta2=initial_data_value
ALLOCATE(grid%nmm_aeta2(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_aeta2(sm32:em32). ')
 endif
  grid%nmm_aeta2=initial_data_value
ALLOCATE(grid%nmm_eta2(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_eta2(sm32:em32). ')
 endif
  grid%nmm_eta2=initial_data_value
ALLOCATE(grid%nmm_em(1:2500),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_em(1:2500). ')
 endif
  grid%nmm_em=initial_data_value
ALLOCATE(grid%nmm_emt(1:2500),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_emt(1:2500). ')
 endif
  grid%nmm_emt=initial_data_value
ALLOCATE(grid%nmm_adt(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_adt(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%nmm_adt=initial_data_value
ALLOCATE(grid%nmm_adu(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_adu(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%nmm_adu=initial_data_value
ALLOCATE(grid%nmm_adv(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_adv(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%nmm_adv=initial_data_value
ALLOCATE(grid%nmm_em_loc(1:2500),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_em_loc(1:2500). ')
 endif
  grid%nmm_em_loc=initial_data_value
ALLOCATE(grid%nmm_emt_loc(1:2500),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_emt_loc(1:2500). ')
 endif
  grid%nmm_emt_loc=initial_data_value
grid%nmm_dy_nmm=initial_data_value
grid%nmm_cpgfv=initial_data_value
grid%nmm_en=initial_data_value
grid%nmm_ent=initial_data_value
grid%nmm_f4d=initial_data_value
grid%nmm_f4q=initial_data_value
grid%nmm_ef4t=initial_data_value
grid%nmm_dlmd=initial_data_value
grid%nmm_dphd=initial_data_value
grid%nmm_pdtop=initial_data_value
grid%nmm_pt=initial_data_value
ALLOCATE(grid%nmm_pdsl(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_pdsl(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_pdsl=initial_data_value
ALLOCATE(grid%nmm_pdslo(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_pdslo(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_pdslo=initial_data_value
ALLOCATE(grid%nmm_psdt(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_psdt(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_psdt=initial_data_value
ALLOCATE(grid%nmm_div(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_div(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%nmm_div=initial_data_value
ALLOCATE(grid%nmm_few(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_few(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%nmm_few=initial_data_value
ALLOCATE(grid%nmm_fne(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_fne(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%nmm_fne=initial_data_value
ALLOCATE(grid%nmm_fns(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_fns(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%nmm_fns=initial_data_value
ALLOCATE(grid%nmm_fse(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_fse(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%nmm_fse=initial_data_value
ALLOCATE(grid%nmm_omgalf(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_omgalf(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%nmm_omgalf=initial_data_value
ALLOCATE(grid%nmm_petdt(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_petdt(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%nmm_petdt=initial_data_value
ALLOCATE(grid%nmm_rtop(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_rtop(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%nmm_rtop=initial_data_value
ALLOCATE(grid%nmm_lpbl(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_lpbl(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_lpbl=0
ALLOCATE(grid%nmm_ustar(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_ustar(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_ustar=initial_data_value
ALLOCATE(grid%nmm_z0(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_z0(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_z0=initial_data_value
ALLOCATE(grid%nmm_z0base(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_z0base(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_z0base=initial_data_value
ALLOCATE(grid%nmm_ths(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_ths(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_ths=initial_data_value
ALLOCATE(grid%nmm_qs(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_qs(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_qs=initial_data_value
ALLOCATE(grid%nmm_twbs(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_twbs(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_twbs=initial_data_value
ALLOCATE(grid%nmm_qwbs(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_qwbs(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_qwbs=initial_data_value
ALLOCATE(grid%nmm_prec(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_prec(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_prec=initial_data_value
ALLOCATE(grid%nmm_aprec(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_aprec(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_aprec=initial_data_value
ALLOCATE(grid%nmm_acprec(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_acprec(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_acprec=initial_data_value
ALLOCATE(grid%nmm_cuprec(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_cuprec(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_cuprec=initial_data_value
ALLOCATE(grid%nmm_accliq(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_accliq(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_accliq=initial_data_value
ALLOCATE(grid%nmm_sno(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_sno(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_sno=initial_data_value
ALLOCATE(grid%nmm_si(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_si(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_si=initial_data_value
ALLOCATE(grid%nmm_cldefi(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_cldefi(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_cldefi=initial_data_value
ALLOCATE(grid%nmm_deep(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_deep(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_deep=initial_data_value
ALLOCATE(grid%nmm_rf(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_rf(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_rf=initial_data_value
ALLOCATE(grid%nmm_th10(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_th10(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_th10=initial_data_value
ALLOCATE(grid%nmm_q10(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_q10(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_q10=initial_data_value
ALLOCATE(grid%nmm_pshltr(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_pshltr(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_pshltr=initial_data_value
ALLOCATE(grid%nmm_tshltr(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_tshltr(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_tshltr=initial_data_value
ALLOCATE(grid%nmm_qshltr(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_qshltr(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_qshltr=initial_data_value
ALLOCATE(grid%nmm_q2(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_q2(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%nmm_q2=initial_data_value
ALLOCATE(grid%nmm_t_adj(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_t_adj(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%nmm_t_adj=initial_data_value
ALLOCATE(grid%nmm_t_old(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_t_old(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%nmm_t_old=initial_data_value
ALLOCATE(grid%nmm_zero_3d(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_zero_3d(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%nmm_zero_3d=initial_data_value
ALLOCATE(grid%nmm_w0avg(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_w0avg(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%nmm_w0avg=initial_data_value
ALLOCATE(grid%nmm_albase(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_albase(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_albase=initial_data_value
ALLOCATE(grid%nmm_albedo(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_albedo(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_albedo=initial_data_value
ALLOCATE(grid%nmm_cnvbot(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_cnvbot(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_cnvbot=initial_data_value
ALLOCATE(grid%nmm_cnvtop(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_cnvtop(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_cnvtop=initial_data_value
ALLOCATE(grid%nmm_czen(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_czen(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_czen=initial_data_value
ALLOCATE(grid%nmm_czmean(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_czmean(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_czmean=initial_data_value
ALLOCATE(grid%nmm_epsr(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_epsr(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_epsr=initial_data_value
ALLOCATE(grid%nmm_gffc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_gffc(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_gffc=initial_data_value
ALLOCATE(grid%nmm_glat(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_glat(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_glat=initial_data_value
ALLOCATE(grid%nmm_glon(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_glon(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_glon=initial_data_value
ALLOCATE(grid%nmm_nmm_tsk(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_nmm_tsk(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_nmm_tsk=initial_data_value
ALLOCATE(grid%nmm_hdac(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_hdac(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_hdac=initial_data_value
ALLOCATE(grid%nmm_hdacv(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_hdacv(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_hdacv=initial_data_value
ALLOCATE(grid%nmm_mxsnal(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_mxsnal(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_mxsnal=initial_data_value
ALLOCATE(grid%nmm_radin(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_radin(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_radin=initial_data_value
ALLOCATE(grid%nmm_radot(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_radot(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_radot=initial_data_value
ALLOCATE(grid%nmm_sigt4(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_sigt4(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_sigt4=initial_data_value
ALLOCATE(grid%nmm_tg(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_tg(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_tg=initial_data_value
ALLOCATE(grid%nmm_dfrlg(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_dfrlg(sm32:em32). ')
 endif
  grid%nmm_dfrlg=initial_data_value
ALLOCATE(grid%nmm_lvl(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_lvl(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_lvl=0
ALLOCATE(grid%nmm_cwm(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_cwm(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%nmm_cwm=initial_data_value
ALLOCATE(grid%nmm_f_ice(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_f_ice(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%nmm_f_ice=initial_data_value
ALLOCATE(grid%nmm_f_rain(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_f_rain(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%nmm_f_rain=initial_data_value
ALLOCATE(grid%nmm_f_rimef(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_f_rimef(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%nmm_f_rimef=initial_data_value
ALLOCATE(grid%nmm_sr(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_sr(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_sr=initial_data_value
ALLOCATE(grid%nmm_u00(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_u00(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_u00=initial_data_value
ALLOCATE(grid%nmm_cfrach(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_cfrach(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_cfrach=initial_data_value
ALLOCATE(grid%nmm_cfracl(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_cfracl(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_cfracl=initial_data_value
ALLOCATE(grid%nmm_cfracm(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_cfracm(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_cfracm=initial_data_value
ALLOCATE(grid%nmm_lc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_lc(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_lc=0
ALLOCATE(grid%nmm_ul(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_ul(sm32:em32). ')
 endif
  grid%nmm_ul=0
ALLOCATE(grid%nmm_islope(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_islope(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_islope=initial_data_value
ALLOCATE(grid%nmm_dzsoil(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_dzsoil(sm32:em32). ')
 endif
  grid%nmm_dzsoil=initial_data_value
ALLOCATE(grid%nmm_rtdpth(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_rtdpth(sm32:em32). ')
 endif
  grid%nmm_rtdpth=initial_data_value
ALLOCATE(grid%nmm_sldpth(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_sldpth(sm32:em32). ')
 endif
  grid%nmm_sldpth=initial_data_value
ALLOCATE(grid%nmm_cmc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_cmc(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_cmc=initial_data_value
ALLOCATE(grid%nmm_grnflx(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_grnflx(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_grnflx=initial_data_value
ALLOCATE(grid%nmm_pctsno(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_pctsno(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_pctsno=initial_data_value
ALLOCATE(grid%nmm_soiltb(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_soiltb(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_soiltb=initial_data_value
ALLOCATE(grid%nmm_vegfrc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_vegfrc(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_vegfrc=initial_data_value
ALLOCATE(grid%nmm_shdmin(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_shdmin(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_shdmin=initial_data_value
ALLOCATE(grid%nmm_shdmax(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_shdmax(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_shdmax=initial_data_value
ALLOCATE(grid%nmm_sh2o(sm31:em31,model_config_rec%num_soil_layers,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_sh2o(sm31:em31,model_config_rec%num_soil_layers,sm33:em33). ')
 endif
  grid%nmm_sh2o=initial_data_value
ALLOCATE(grid%nmm_smc(sm31:em31,model_config_rec%num_soil_layers,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_smc(sm31:em31,model_config_rec%num_soil_layers,sm33:em33). ')
 endif
  grid%nmm_smc=initial_data_value
ALLOCATE(grid%nmm_stc(sm31:em31,model_config_rec%num_soil_layers,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_stc(sm31:em31,model_config_rec%num_soil_layers,sm33:em33). ')
 endif
  grid%nmm_stc=initial_data_value
ALLOCATE(grid%nmm_dwdtmn(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_dwdtmn(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_dwdtmn=initial_data_value
ALLOCATE(grid%nmm_dwdtmx(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_dwdtmx(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_dwdtmx=initial_data_value
ALLOCATE(grid%nmm_dwdt(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_dwdt(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%nmm_dwdt=initial_data_value
ALLOCATE(grid%nmm_pdwdt(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_pdwdt(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%nmm_pdwdt=initial_data_value
ALLOCATE(grid%nmm_pint(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_pint(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%nmm_pint=initial_data_value
ALLOCATE(grid%nmm_w(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_w(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%nmm_w=initial_data_value
ALLOCATE(grid%nmm_z(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_z(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%nmm_z=initial_data_value
ALLOCATE(grid%nmm_acfrcv(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_acfrcv(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_acfrcv=initial_data_value
ALLOCATE(grid%nmm_acfrst(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_acfrst(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_acfrst=initial_data_value
ALLOCATE(grid%nmm_ssroff(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_ssroff(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_ssroff=initial_data_value
ALLOCATE(grid%nmm_bgroff(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_bgroff(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_bgroff=initial_data_value
ALLOCATE(grid%nmm_rlwin(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_rlwin(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_rlwin=initial_data_value
ALLOCATE(grid%nmm_rlwout(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_rlwout(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_rlwout=initial_data_value
ALLOCATE(grid%nmm_rlwtoa(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_rlwtoa(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_rlwtoa=initial_data_value
ALLOCATE(grid%nmm_alwin(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_alwin(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_alwin=initial_data_value
ALLOCATE(grid%nmm_alwout(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_alwout(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_alwout=initial_data_value
ALLOCATE(grid%nmm_alwtoa(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_alwtoa(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_alwtoa=initial_data_value
ALLOCATE(grid%nmm_totswdn(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_totswdn(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_totswdn=initial_data_value
ALLOCATE(grid%nmm_totlwdn(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_totlwdn(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_totlwdn=initial_data_value
ALLOCATE(grid%nmm_rswin(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_rswin(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_rswin=initial_data_value
ALLOCATE(grid%nmm_rswout(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_rswout(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_rswout=initial_data_value
ALLOCATE(grid%nmm_rswtoa(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_rswtoa(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_rswtoa=initial_data_value
ALLOCATE(grid%nmm_aswin(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_aswin(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_aswin=initial_data_value
ALLOCATE(grid%nmm_aswout(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_aswout(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_aswout=initial_data_value
ALLOCATE(grid%nmm_aswtoa(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_aswtoa(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_aswtoa=initial_data_value
ALLOCATE(grid%nmm_sfcshx(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_sfcshx(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_sfcshx=initial_data_value
ALLOCATE(grid%nmm_sfclhx(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_sfclhx(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_sfclhx=initial_data_value
ALLOCATE(grid%nmm_subshx(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_subshx(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_subshx=initial_data_value
ALLOCATE(grid%nmm_snopcx(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_snopcx(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_snopcx=initial_data_value
ALLOCATE(grid%nmm_sfcuvx(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_sfcuvx(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_sfcuvx=initial_data_value
ALLOCATE(grid%nmm_potevp(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_potevp(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_potevp=initial_data_value
ALLOCATE(grid%nmm_potflx(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_potflx(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_potflx=initial_data_value
ALLOCATE(grid%nmm_tlmin(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_tlmin(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_tlmin=initial_data_value
ALLOCATE(grid%nmm_tlmax(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_tlmax(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_tlmax=initial_data_value
ALLOCATE(grid%nmm_rlwtt(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_rlwtt(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%nmm_rlwtt=initial_data_value
ALLOCATE(grid%nmm_rswtt(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_rswtt(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%nmm_rswtt=initial_data_value
ALLOCATE(grid%nmm_tcucn(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_tcucn(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%nmm_tcucn=initial_data_value
ALLOCATE(grid%nmm_train(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_train(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%nmm_train=initial_data_value
ALLOCATE(grid%nmm_ncfrcv(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_ncfrcv(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_ncfrcv=0
ALLOCATE(grid%nmm_ncfrst(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_ncfrst(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_ncfrst=0
grid%nmm_nclod=0
grid%nmm_nprec=0
grid%nmm_nheat=0
grid%nmm_nrdlw=0
grid%nmm_nrdsw=0
grid%nmm_nsrfc=0
grid%nmm_avrain=initial_data_value
grid%nmm_avcnvc=initial_data_value
grid%nmm_aratim=initial_data_value
grid%nmm_acutim=initial_data_value
grid%nmm_ardlw=initial_data_value
grid%nmm_ardsw=initial_data_value
grid%nmm_asrfc=initial_data_value
grid%nmm_aphtim=initial_data_value
ALLOCATE(grid%nmm_ihe(sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_ihe(sm33:em33). ')
 endif
  grid%nmm_ihe=0
ALLOCATE(grid%nmm_ihw(sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_ihw(sm33:em33). ')
 endif
  grid%nmm_ihw=0
ALLOCATE(grid%nmm_ive(sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_ive(sm33:em33). ')
 endif
  grid%nmm_ive=0
ALLOCATE(grid%nmm_ivw(sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_ivw(sm33:em33). ')
 endif
  grid%nmm_ivw=0
ALLOCATE(grid%nmm_irad(sm31:em31),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_irad(sm31:em31). ')
 endif
  grid%nmm_irad=0
ALLOCATE(grid%nmm_iheg(1:2500),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_iheg(1:2500). ')
 endif
  grid%nmm_iheg=0
ALLOCATE(grid%nmm_ihwg(1:2500),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_ihwg(1:2500). ')
 endif
  grid%nmm_ihwg=0
ALLOCATE(grid%nmm_iveg(1:2500),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_iveg(1:2500). ')
 endif
  grid%nmm_iveg=0
ALLOCATE(grid%nmm_ivwg(1:2500),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_ivwg(1:2500). ')
 endif
  grid%nmm_ivwg=0
ALLOCATE(grid%nmm_iradg(1:2000),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_iradg(1:2000). ')
 endif
  grid%nmm_iradg=0
ALLOCATE(grid%nmm_indx3_wrk(-3:3,1:2500,0:6),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_indx3_wrk(-3:3,1:2500,0:6). ')
 endif
  grid%nmm_indx3_wrk=0
ALLOCATE(grid%nmm_n_iup_h(sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_n_iup_h(sm33:em33). ')
 endif
  grid%nmm_n_iup_h=0
ALLOCATE(grid%nmm_n_iup_v(sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_n_iup_v(sm33:em33). ')
 endif
  grid%nmm_n_iup_v=0
ALLOCATE(grid%nmm_n_iup_adh(sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_n_iup_adh(sm33:em33). ')
 endif
  grid%nmm_n_iup_adh=0
ALLOCATE(grid%nmm_n_iup_adv(sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_n_iup_adv(sm33:em33). ')
 endif
  grid%nmm_n_iup_adv=0
ALLOCATE(grid%nmm_iup_h(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_iup_h(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_iup_h=0
ALLOCATE(grid%nmm_iup_v(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_iup_v(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_iup_v=0
ALLOCATE(grid%nmm_iup_adh(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_iup_adh(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_iup_adh=0
ALLOCATE(grid%nmm_iup_adv(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%nmm_iup_adv(sm31:em31,sm33:em33). ')
 endif
  grid%nmm_iup_adv=0
ALLOCATE(grid%sm000010(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%sm000010(sm31:em31,sm33:em33). ')
 endif
  grid%sm000010=initial_data_value
ALLOCATE(grid%sm010040(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%sm010040(sm31:em31,sm33:em33). ')
 endif
  grid%sm010040=initial_data_value
ALLOCATE(grid%sm040100(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%sm040100(sm31:em31,sm33:em33). ')
 endif
  grid%sm040100=initial_data_value
ALLOCATE(grid%sm100200(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%sm100200(sm31:em31,sm33:em33). ')
 endif
  grid%sm100200=initial_data_value
ALLOCATE(grid%sm010200(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%sm010200(sm31:em31,sm33:em33). ')
 endif
  grid%sm010200=initial_data_value
ALLOCATE(grid%soilm000(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%soilm000(sm31:em31,sm33:em33). ')
 endif
  grid%soilm000=initial_data_value
ALLOCATE(grid%soilm005(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%soilm005(sm31:em31,sm33:em33). ')
 endif
  grid%soilm005=initial_data_value
ALLOCATE(grid%soilm020(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%soilm020(sm31:em31,sm33:em33). ')
 endif
  grid%soilm020=initial_data_value
ALLOCATE(grid%soilm040(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%soilm040(sm31:em31,sm33:em33). ')
 endif
  grid%soilm040=initial_data_value
ALLOCATE(grid%soilm160(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%soilm160(sm31:em31,sm33:em33). ')
 endif
  grid%soilm160=initial_data_value
ALLOCATE(grid%soilm300(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%soilm300(sm31:em31,sm33:em33). ')
 endif
  grid%soilm300=initial_data_value
ALLOCATE(grid%sw000010(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%sw000010(sm31:em31,sm33:em33). ')
 endif
  grid%sw000010=initial_data_value
ALLOCATE(grid%sw010040(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%sw010040(sm31:em31,sm33:em33). ')
 endif
  grid%sw010040=initial_data_value
ALLOCATE(grid%sw040100(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%sw040100(sm31:em31,sm33:em33). ')
 endif
  grid%sw040100=initial_data_value
ALLOCATE(grid%sw100200(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%sw100200(sm31:em31,sm33:em33). ')
 endif
  grid%sw100200=initial_data_value
ALLOCATE(grid%sw010200(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%sw010200(sm31:em31,sm33:em33). ')
 endif
  grid%sw010200=initial_data_value
ALLOCATE(grid%soilw000(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%soilw000(sm31:em31,sm33:em33). ')
 endif
  grid%soilw000=initial_data_value
ALLOCATE(grid%soilw005(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%soilw005(sm31:em31,sm33:em33). ')
 endif
  grid%soilw005=initial_data_value
ALLOCATE(grid%soilw020(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%soilw020(sm31:em31,sm33:em33). ')
 endif
  grid%soilw020=initial_data_value
ALLOCATE(grid%soilw040(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%soilw040(sm31:em31,sm33:em33). ')
 endif
  grid%soilw040=initial_data_value
ALLOCATE(grid%soilw160(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%soilw160(sm31:em31,sm33:em33). ')
 endif
  grid%soilw160=initial_data_value
ALLOCATE(grid%soilw300(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%soilw300(sm31:em31,sm33:em33). ')
 endif
  grid%soilw300=initial_data_value
ALLOCATE(grid%st000010(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%st000010(sm31:em31,sm33:em33). ')
 endif
  grid%st000010=initial_data_value
ALLOCATE(grid%st010040(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%st010040(sm31:em31,sm33:em33). ')
 endif
  grid%st010040=initial_data_value
ALLOCATE(grid%st040100(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%st040100(sm31:em31,sm33:em33). ')
 endif
  grid%st040100=initial_data_value
ALLOCATE(grid%st100200(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%st100200(sm31:em31,sm33:em33). ')
 endif
  grid%st100200=initial_data_value
ALLOCATE(grid%st010200(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%st010200(sm31:em31,sm33:em33). ')
 endif
  grid%st010200=initial_data_value
ALLOCATE(grid%soilt000(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%soilt000(sm31:em31,sm33:em33). ')
 endif
  grid%soilt000=initial_data_value
ALLOCATE(grid%soilt005(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%soilt005(sm31:em31,sm33:em33). ')
 endif
  grid%soilt005=initial_data_value
ALLOCATE(grid%soilt020(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%soilt020(sm31:em31,sm33:em33). ')
 endif
  grid%soilt020=initial_data_value
ALLOCATE(grid%soilt040(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%soilt040(sm31:em31,sm33:em33). ')
 endif
  grid%soilt040=initial_data_value
ALLOCATE(grid%soilt160(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%soilt160(sm31:em31,sm33:em33). ')
 endif
  grid%soilt160=initial_data_value
ALLOCATE(grid%soilt300(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%soilt300(sm31:em31,sm33:em33). ')
 endif
  grid%soilt300=initial_data_value
ALLOCATE(grid%landmask(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%landmask(sm31:em31,sm33:em33). ')
 endif
  grid%landmask=initial_data_value
ALLOCATE(grid%topostdv(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%topostdv(sm31:em31,sm33:em33). ')
 endif
  grid%topostdv=initial_data_value
ALLOCATE(grid%toposlpx(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%toposlpx(sm31:em31,sm33:em33). ')
 endif
  grid%toposlpx=initial_data_value
ALLOCATE(grid%toposlpy(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%toposlpy(sm31:em31,sm33:em33). ')
 endif
  grid%toposlpy=initial_data_value
ALLOCATE(grid%greenmax(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%greenmax(sm31:em31,sm33:em33). ')
 endif
  grid%greenmax=initial_data_value
ALLOCATE(grid%greenmin(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%greenmin(sm31:em31,sm33:em33). ')
 endif
  grid%greenmin=initial_data_value
ALLOCATE(grid%albedomx(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%albedomx(sm31:em31,sm33:em33). ')
 endif
  grid%albedomx=initial_data_value
ALLOCATE(grid%slopecat(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%slopecat(sm31:em31,sm33:em33). ')
 endif
  grid%slopecat=initial_data_value
ALLOCATE(grid%toposoil(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%toposoil(sm31:em31,sm33:em33). ')
 endif
  grid%toposoil=initial_data_value
ALLOCATE(grid%landusef(sm31:em31,model_config_rec%num_land_cat,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%landusef(sm31:em31,model_config_rec%num_land_cat,sm33:em33). ')
 endif
  grid%landusef=initial_data_value
ALLOCATE(grid%soilctop(sm31:em31,model_config_rec%num_soil_cat,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%soilctop(sm31:em31,model_config_rec%num_soil_cat,sm33:em33). ')
 endif
  grid%soilctop=initial_data_value
ALLOCATE(grid%soilcbot(sm31:em31,model_config_rec%num_soil_cat,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%soilcbot(sm31:em31,model_config_rec%num_soil_cat,sm33:em33). ')
 endif
  grid%soilcbot=initial_data_value
ALLOCATE(grid%moist_1(sm31:em31,sm32:em32,sm33:em33,num_moist),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%moist_1(sm31:em31,sm32:em32,sm33:em33,num_moist). ')
 endif
  grid%moist_1=initial_data_value
ALLOCATE(grid%moist_2(sm31:em31,sm32:em32,sm33:em33,num_moist),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%moist_2(sm31:em31,sm32:em32,sm33:em33,num_moist). ')
 endif
  grid%moist_2=initial_data_value
ALLOCATE(grid%chem_1(sm31:em31,sm32:em32,sm33:em33,num_chem),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%chem_1(sm31:em31,sm32:em32,sm33:em33,num_chem). ')
 endif
  grid%chem_1=initial_data_value
ALLOCATE(grid%chem_2(sm31:em31,sm32:em32,sm33:em33,num_chem),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%chem_2(sm31:em31,sm32:em32,sm33:em33,num_chem). ')
 endif
  grid%chem_2=initial_data_value
ALLOCATE(grid%smois(sm31:em31,model_config_rec%num_soil_layers,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%smois(sm31:em31,model_config_rec%num_soil_layers,sm33:em33). ')
 endif
  grid%smois=initial_data_value
grid%dtbc=initial_data_value
ALLOCATE(grid%th2(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%th2(sm31:em31,sm33:em33). ')
 endif
  grid%th2=initial_data_value
ALLOCATE(grid%u10(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%u10(sm31:em31,sm33:em33). ')
 endif
  grid%u10=initial_data_value
ALLOCATE(grid%v10(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%v10(sm31:em31,sm33:em33). ')
 endif
  grid%v10=initial_data_value
ALLOCATE(grid%xice(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%xice(sm31:em31,sm33:em33). ')
 endif
  grid%xice=initial_data_value
ALLOCATE(grid%smstav(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%smstav(sm31:em31,sm33:em33). ')
 endif
  grid%smstav=initial_data_value
ALLOCATE(grid%smstot(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%smstot(sm31:em31,sm33:em33). ')
 endif
  grid%smstot=initial_data_value
ALLOCATE(grid%sfcrunoff(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%sfcrunoff(sm31:em31,sm33:em33). ')
 endif
  grid%sfcrunoff=initial_data_value
ALLOCATE(grid%udrunoff(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%udrunoff(sm31:em31,sm33:em33). ')
 endif
  grid%udrunoff=initial_data_value
ALLOCATE(grid%ivgtyp(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%ivgtyp(sm31:em31,sm33:em33). ')
 endif
  grid%ivgtyp=0
ALLOCATE(grid%isltyp(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%isltyp(sm31:em31,sm33:em33). ')
 endif
  grid%isltyp=0
ALLOCATE(grid%vegfra(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%vegfra(sm31:em31,sm33:em33). ')
 endif
  grid%vegfra=initial_data_value
ALLOCATE(grid%sfcevp(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%sfcevp(sm31:em31,sm33:em33). ')
 endif
  grid%sfcevp=initial_data_value
ALLOCATE(grid%grdflx(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%grdflx(sm31:em31,sm33:em33). ')
 endif
  grid%grdflx=initial_data_value
ALLOCATE(grid%albbck(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%albbck(sm31:em31,sm33:em33). ')
 endif
  grid%albbck=initial_data_value
ALLOCATE(grid%sfcexc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%sfcexc(sm31:em31,sm33:em33). ')
 endif
  grid%sfcexc=initial_data_value
ALLOCATE(grid%acsnow(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%acsnow(sm31:em31,sm33:em33). ')
 endif
  grid%acsnow=initial_data_value
ALLOCATE(grid%acsnom(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%acsnom(sm31:em31,sm33:em33). ')
 endif
  grid%acsnom=initial_data_value
ALLOCATE(grid%snow(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%snow(sm31:em31,sm33:em33). ')
 endif
  grid%snow=initial_data_value
ALLOCATE(grid%canwat(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%canwat(sm31:em31,sm33:em33). ')
 endif
  grid%canwat=initial_data_value
ALLOCATE(grid%sst(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%sst(sm31:em31,sm33:em33). ')
 endif
  grid%sst=initial_data_value
ALLOCATE(grid%weasd(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%weasd(sm31:em31,sm33:em33). ')
 endif
  grid%weasd=initial_data_value
ALLOCATE(grid%mol(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%mol(sm31:em31,sm33:em33). ')
 endif
  grid%mol=initial_data_value
ALLOCATE(grid%znt(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%znt(sm31:em31,sm33:em33). ')
 endif
  grid%znt=initial_data_value
ALLOCATE(grid%tke_myj(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%tke_myj(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%tke_myj=initial_data_value
ALLOCATE(grid%thz0(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%thz0(sm31:em31,sm33:em33). ')
 endif
  grid%thz0=initial_data_value
ALLOCATE(grid%qz0(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%qz0(sm31:em31,sm33:em33). ')
 endif
  grid%qz0=initial_data_value
ALLOCATE(grid%uz0(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%uz0(sm31:em31,sm33:em33). ')
 endif
  grid%uz0=initial_data_value
ALLOCATE(grid%vz0(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%vz0(sm31:em31,sm33:em33). ')
 endif
  grid%vz0=initial_data_value
ALLOCATE(grid%uz0h(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%uz0h(sm31:em31,sm33:em33). ')
 endif
  grid%uz0h=initial_data_value
ALLOCATE(grid%vz0h(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%vz0h(sm31:em31,sm33:em33). ')
 endif
  grid%vz0h=initial_data_value
ALLOCATE(grid%dudt(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%dudt(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%dudt=initial_data_value
ALLOCATE(grid%dvdt(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%dvdt(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%dvdt=initial_data_value
ALLOCATE(grid%qsfc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%qsfc(sm31:em31,sm33:em33). ')
 endif
  grid%qsfc=initial_data_value
ALLOCATE(grid%akhs(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%akhs(sm31:em31,sm33:em33). ')
 endif
  grid%akhs=initial_data_value
ALLOCATE(grid%akms(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%akms(sm31:em31,sm33:em33). ')
 endif
  grid%akms=initial_data_value
ALLOCATE(grid%htop(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%htop(sm31:em31,sm33:em33). ')
 endif
  grid%htop=initial_data_value
ALLOCATE(grid%hbot(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%hbot(sm31:em31,sm33:em33). ')
 endif
  grid%hbot=initial_data_value
ALLOCATE(grid%cuppt(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%cuppt(sm31:em31,sm33:em33). ')
 endif
  grid%cuppt=initial_data_value
ALLOCATE(grid%t0eta(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%t0eta(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%t0eta=initial_data_value
ALLOCATE(grid%q0eta(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%q0eta(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%q0eta=initial_data_value
ALLOCATE(grid%p0eta(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%p0eta(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%p0eta=initial_data_value
ALLOCATE(grid%f_ice_phy(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%f_ice_phy(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%f_ice_phy=initial_data_value
ALLOCATE(grid%f_rain_phy(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%f_rain_phy(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%f_rain_phy=initial_data_value
ALLOCATE(grid%f_rimef_phy(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%f_rimef_phy(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%f_rimef_phy=initial_data_value
ALLOCATE(grid%mass_flux(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%mass_flux(sm31:em31,sm33:em33). ')
 endif
  grid%mass_flux=initial_data_value
ALLOCATE(grid%apr_gr(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%apr_gr(sm31:em31,sm33:em33). ')
 endif
  grid%apr_gr=initial_data_value
ALLOCATE(grid%apr_w(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%apr_w(sm31:em31,sm33:em33). ')
 endif
  grid%apr_w=initial_data_value
ALLOCATE(grid%apr_mc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%apr_mc(sm31:em31,sm33:em33). ')
 endif
  grid%apr_mc=initial_data_value
ALLOCATE(grid%apr_st(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%apr_st(sm31:em31,sm33:em33). ')
 endif
  grid%apr_st=initial_data_value
ALLOCATE(grid%apr_as(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%apr_as(sm31:em31,sm33:em33). ')
 endif
  grid%apr_as=initial_data_value
ALLOCATE(grid%apr_capma(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%apr_capma(sm31:em31,sm33:em33). ')
 endif
  grid%apr_capma=initial_data_value
ALLOCATE(grid%apr_capme(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%apr_capme(sm31:em31,sm33:em33). ')
 endif
  grid%apr_capme=initial_data_value
ALLOCATE(grid%apr_capmi(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%apr_capmi(sm31:em31,sm33:em33). ')
 endif
  grid%apr_capmi=initial_data_value
ALLOCATE(grid%xf_ens(sm31:em31,sm33:em33,model_config_rec%ensdim),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%xf_ens(sm31:em31,sm33:em33,model_config_rec%ensdim). ')
 endif
  grid%xf_ens=initial_data_value
ALLOCATE(grid%pr_ens(sm31:em31,sm33:em33,model_config_rec%ensdim),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%pr_ens(sm31:em31,sm33:em33,model_config_rec%ensdim). ')
 endif
  grid%pr_ens=initial_data_value
ALLOCATE(grid%rthften(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%rthften(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%rthften=initial_data_value
ALLOCATE(grid%rqvften(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%rqvften(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%rqvften=initial_data_value
ALLOCATE(grid%snowh(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%snowh(sm31:em31,sm33:em33). ')
 endif
  grid%snowh=initial_data_value
ALLOCATE(grid%smfr3d(sm31:em31,model_config_rec%num_soil_layers,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%smfr3d(sm31:em31,model_config_rec%num_soil_layers,sm33:em33). ')
 endif
  grid%smfr3d=initial_data_value
grid%number_at_same_level=0
grid%itimestep=0
grid%oid=0
grid%auxhist1_oid=0
grid%auxhist2_oid=0
grid%auxhist3_oid=0
grid%auxhist4_oid=0
grid%auxhist5_oid=0
grid%auxinput1_oid=0
grid%auxinput2_oid=0
grid%auxinput3_oid=0
grid%auxinput4_oid=0
grid%auxinput5_oid=0
grid%nframes=0
grid%lbc_fid=0
grid%time_step=0
grid%time_step_fract_num=0
grid%time_step_fract_den=0
grid%max_dom=0
grid%dyn_opt=0
grid%rk_ord=0
grid%diff_opt=0
grid%km_opt=0
grid%damp_opt=0
grid%isfflx=0
grid%ifsnow=0
grid%icloud=0
grid%num_soil_layers=0
grid%num_land_cat=0
grid%num_soil_cat=0
grid%spec_bdy_width=0
grid%spec_zone=0
grid%relax_zone=0
grid%ensdim=0
grid%tile_sz_x=0
grid%tile_sz_y=0
grid%numtiles=0
grid%debug_level=0
grid%irand=0
grid%run_days=0
grid%run_hours=0
grid%run_minutes=0
grid%run_seconds=0
grid%start_year=0
grid%start_month=0
grid%start_day=0
grid%start_hour=0
grid%start_minute=0
grid%start_second=0
grid%end_year=0
grid%end_month=0
grid%end_day=0
grid%end_hour=0
grid%end_minute=0
grid%end_second=0
grid%grid_id=0
grid%level=0
grid%s_we=0
grid%e_we=0
grid%s_sn=0
grid%e_sn=0
grid%s_vert=0
grid%e_vert=0
grid%history_interval=0
grid%auxhist1_interval=0
grid%auxhist2_interval=0
grid%auxhist3_interval=0
grid%auxhist4_interval=0
grid%auxhist5_interval=0
grid%auxinput1_interval=0
grid%auxinput2_interval=0
grid%auxinput3_interval=0
grid%auxinput4_interval=0
grid%auxinput5_interval=0
grid%restart_interval=0
grid%frames_per_outfile=0
grid%time_step_sound=0
grid%parent_id=0
grid%i_parent_start=0
grid%j_parent_start=0
grid%shw=0
grid%parent_grid_ratio=0
grid%parent_time_step_ratio=0
grid%moad_grid_ratio=0
grid%moad_time_step_ratio=0
grid%dx=initial_data_value
grid%dy=initial_data_value
grid%dt=initial_data_value
grid%ztop=initial_data_value
grid%zdamp=initial_data_value
grid%dampcoef=initial_data_value
grid%smdiv=initial_data_value
grid%emdiv=initial_data_value
grid%epssm=initial_data_value
grid%khdif=initial_data_value
grid%kvdif=initial_data_value
grid%mix_cr_len=initial_data_value
grid%tke_upper_bound=initial_data_value
grid%kh_tke_upper_bound=initial_data_value
grid%kv_tke_upper_bound=initial_data_value
grid%radt=initial_data_value
grid%bldt=initial_data_value
grid%cudt=initial_data_value
grid%gsmdt=initial_data_value
grid%julyr=0
grid%julday=0
grid%gmt=initial_data_value
grid%idtad=0
grid%nsoil=0
grid%nphs=0
grid%ncnvc=0
grid%nrads=0
grid%nradl=0
grid%sigma=0
grid%chem_opt=0
grid%mp_physics=0
grid%ra_lw_physics=0
grid%ra_sw_physics=0
grid%bl_sfclay_physics=0
grid%bl_surface_physics=0
grid%bl_pbl_physics=0
grid%cu_physics=0
grid%h_mom_adv_order=0
grid%v_mom_adv_order=0
grid%h_sca_adv_order=0
grid%v_sca_adv_order=0
grid%io_form_input=0
grid%io_form_auxinput1=0
grid%io_form_auxinput2=0
grid%io_form_auxinput3=0
grid%io_form_auxinput4=0
grid%io_form_auxinput5=0
grid%io_form_history=0
grid%io_form_auxhist1=0
grid%io_form_auxhist2=0
grid%io_form_auxhist3=0
grid%io_form_auxhist4=0
grid%io_form_auxhist5=0
grid%io_form_restart=0
grid%io_form_boundary=0
grid%interval_seconds=0
grid%real_data_init_type=0
grid%cen_lat=initial_data_value
grid%cen_lon=initial_data_value
grid%truelat1=initial_data_value
grid%truelat2=initial_data_value
grid%bdyfrq=initial_data_value
grid%iswater=0
grid%isice=0
grid%map_proj=0
!ENDOFREGISTRYGENERATEDINCLUDE

!### 13. Edit frame/module_domain.F to add case for DYN_EXP to
!### alloc_space_field.  (This is a bug;
!### one should never have to edit the framework code; will fix this in
!### coming versions).  Same goes for share/start_domain.F, although this
!### is not a framework routine.
      ELSE
        WRITE( wrf_err_message , * )'Invalid specification of dynamics: dyn_opt = ',dyn_opt
        CALL wrf_error_fatal ( TRIM ( wrf_err_message ) )
      ENDIF

      WRITE(message,*)'alloc_space_field: domain ',id,' ',num_bytes_allocated
      CALL  wrf_message( message )

   END SUBROUTINE alloc_space_field

!

!  This routine is used to DEALLOCATE space for a single domain.  First
!  the pointers in the linked list are fixed (so the one in the middle can
!  be removed).  Second, the field data are all removed through a CALL to 
!  the dealloc_space_domain routine.  Finally, the pointer to the domain
!  itself is DEALLOCATEd.

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
            CALL dealloc_space_field ( grid )
            DEALLOCATE(grid)
            EXIT find_grid
         END IF
         old_grid => grid
         grid     => grid%next
      END DO find_grid

      IF ( .NOT. found ) THEN
         WRITE ( wrf_err_message , * ) 'module_domain: dealloc_space_domain: Could not de-allocate grid id ',id
         CALL wrf_error_fatal ( TRIM( wrf_err_message ) ) 
      END IF

   END SUBROUTINE dealloc_space_domain

!

!  This routine DEALLOCATEs each gridded field for this domain.  For each type of
!  different array (1d, 2d, 3d, etc.), the space for each pointer is DEALLOCATEd
!  for every -1 (i.e., each different meteorological field).

   SUBROUTINE dealloc_space_field ( grid )
      
      IMPLICIT NONE

      !  Input data.

      TYPE(domain)              , POINTER :: grid

      !  Local data.

      INTEGER                             :: loop

   END SUBROUTINE dealloc_space_field

!
!
   RECURSIVE SUBROUTINE find_grid_by_id ( id, in_grid, result_grid )
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: id
      TYPE(domain), POINTER     :: in_grid 
      TYPE(domain), POINTER     :: result_grid
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

!  This routine is used to find a specific domain identifier in an array
!  of domain identifiers.

   FUNCTION first_loc_integer ( array , search ) RESULT ( loc ) 
 
      IMPLICIT NONE

      !  Input data.

      INTEGER , INTENT(IN) , DIMENSION(:) :: array
      INTEGER , INTENT(IN)                :: search

      !  Output data.

      INTEGER                             :: loc
      
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

END MODULE module_domain
