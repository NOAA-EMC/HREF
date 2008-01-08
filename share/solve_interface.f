!WRF:MEDIATION_LAYER:ADT_BARRIER
!

SUBROUTINE solve_interface ( grid ) 

   USE module_domain
   USE module_timing
   USE module_driver_constants
   USE module_configure
   USE module_wrf_error

   IMPLICIT NONE

   INTERFACE

SUBROUTINE solve_nmm      ( grid , config_flags ,   &
!
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/nmm_dummy_args.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
pd_b,pd_bt,t_b,t_bt,q_b,q_bt,u_b,u_bt,v_b,v_bt,q2_b,q2_bt,cwm_b,cwm_bt,lmh,lmv,hbm2,hbm3,vbm2,vbm3,sm,sice,htm,vtm,pd,fis,res,t, &
q,u,v,told,uold,vold,dx_nmm,wpdar,cpgfu,curv,fcp,fdiv,f,fad,ddmpu,ddmpv,deta,rdeta,aeta,f4q2,etax,dfl,deta1,aeta1,eta1,deta2, &
aeta2,eta2,em,emt,adt,adu,adv,em_loc,emt_loc,pdsl,pdslo,psdt,div,few,fne,fns,fse,omgalf,petdt,rtop,lpbl,ustar,z0,z0base,ths,qs, &
twbs,qwbs,prec,aprec,acprec,cuprec,accliq,sno,si,cldefi,deep,rf,th10,q10,pshltr,tshltr,qshltr,q2,t_adj,t_old,zero_3d,w0avg, &
albase,albedo,cnvbot,cnvtop,czen,czmean,epsr,gffc,glat,glon,nmm_tsk,hdac,hdacv,mxsnal,radin,radot,sigt4,tg,dfrlg,lvl,cwm,f_ice, &
f_rain,f_rimef,sr,u00,cfrach,cfracl,cfracm,lc,ul,islope,dzsoil,rtdpth,sldpth,cmc,grnflx,pctsno,soiltb,vegfrc,shdmin,shdmax,sh2o, &
smc,stc,dwdtmn,dwdtmx,dwdt,pdwdt,pint,w,z,acfrcv,acfrst,ssroff,bgroff,rlwin,rlwout,rlwtoa,alwin,alwout,alwtoa,totswdn,totlwdn, &
rswin,rswout,rswtoa,aswin,aswout,aswtoa,sfcshx,sfclhx,subshx,snopcx,sfcuvx,potevp,potflx,tlmin,tlmax,rlwtt,rswtt,tcucn,train, &
ncfrcv,ncfrst,ihe,ihw,ive,ivw,irad,iheg,ihwg,iveg,ivwg,iradg,indx3_wrk,n_iup_h,n_iup_v,n_iup_adh,n_iup_adv,iup_h,iup_v,iup_adh, &
iup_adv,sm000010,sm010040,sm040100,sm100200,sm010200,soilm000,soilm005,soilm020,soilm040,soilm160,soilm300,sw000010,sw010040, &
sw040100,sw100200,sw010200,soilw000,soilw005,soilw020,soilw040,soilw160,soilw300,st000010,st010040,st040100,st100200,st010200, &
soilt000,soilt005,soilt020,soilt040,soilt160,soilt300,landmask,topostdv,toposlpx,toposlpy,greenmax,greenmin,albedomx,slopecat, &
toposoil,landusef,soilctop,soilcbot,moist_1,moist_2,chem_1,chem_2,smois,th2,u10,v10,xice,smstav,smstot,sfcrunoff,udrunoff, &
ivgtyp,isltyp,vegfra,sfcevp,grdflx,albbck,sfcexc,acsnow,acsnom,snow,canwat,sst,weasd,mol,znt,tke_myj,thz0,qz0,uz0,vz0,uz0h,vz0h, &
dudt,dvdt,qsfc,akhs,akms,htop,hbot,cuppt,t0eta,q0eta,p0eta,f_ice_phy,f_rain_phy,f_rimef_phy,mass_flux,apr_gr,apr_w,apr_mc, &
apr_st,apr_as,apr_capma,apr_capme,apr_capmi,xf_ens,pr_ens,rthften,rqvften,snowh,smfr3d &
!ENDOFREGISTRYGENERATEDINCLUDE
!
                 )

   USE module_domain
   USE module_configure
   USE module_driver_constants

   !  Input data.
   TYPE(domain) , INTENT(INOUT)                  :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)      :: config_flags

!STARTOFREGISTRYGENERATEDINCLUDE 'inc/nmm_dummy_decl.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
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
integer                                  :: nclod
integer                                  :: nprec
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
real      ,DIMENSION(max(grid%ed31,grid%ed33),1,grid%spec_bdy_width,4)           :: pd_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),1,grid%spec_bdy_width,4)           :: pd_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: t_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: t_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: q_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: q_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: u_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: u_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: v_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: v_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: q2_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: q2_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: cwm_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: cwm_bt
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: lmh
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: lmv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: hbm2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: hbm3
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: vbm2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: vbm3
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sm
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sice
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: htm
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: vtm
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: pd
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: fis
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: res
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: t
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: q
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: u
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: v
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: told
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: uold
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: vold
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: dx_nmm
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: wpdar
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: cpgfu
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: curv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: fcp
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: fdiv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: f
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: fad
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: ddmpu
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: ddmpv
real      ,DIMENSION(grid%sm32:grid%em32)           :: deta
real      ,DIMENSION(grid%sm32:grid%em32)           :: rdeta
real      ,DIMENSION(grid%sm32:grid%em32)           :: aeta
real      ,DIMENSION(grid%sm32:grid%em32)           :: f4q2
real      ,DIMENSION(grid%sm32:grid%em32)           :: etax
real      ,DIMENSION(grid%sm32:grid%em32)           :: dfl
real      ,DIMENSION(grid%sm32:grid%em32)           :: deta1
real      ,DIMENSION(grid%sm32:grid%em32)           :: aeta1
real      ,DIMENSION(grid%sm32:grid%em32)           :: eta1
real      ,DIMENSION(grid%sm32:grid%em32)           :: deta2
real      ,DIMENSION(grid%sm32:grid%em32)           :: aeta2
real      ,DIMENSION(grid%sm32:grid%em32)           :: eta2
real      ,DIMENSION(1:2500)             :: em
real      ,DIMENSION(1:2500)             :: emt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: adt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: adu
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: adv
real      ,DIMENSION(1:2500)             :: em_loc
real      ,DIMENSION(1:2500)             :: emt_loc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: pdsl
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: pdslo
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: psdt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: div
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: few
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: fne
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: fns
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: fse
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: omgalf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: petdt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rtop
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: lpbl
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: ustar
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: z0
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: z0base
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: ths
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: qs
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: twbs
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: qwbs
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: prec
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: aprec
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: acprec
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: cuprec
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: accliq
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sno
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: si
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: cldefi
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: deep
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: rf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: th10
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: q10
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: pshltr
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: tshltr
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: qshltr
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: q2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: t_adj
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: t_old
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: zero_3d
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: w0avg
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: albase
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: albedo
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: cnvbot
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: cnvtop
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: czen
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: czmean
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: epsr
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: gffc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: glat
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: glon
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: nmm_tsk
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: hdac
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: hdacv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: mxsnal
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: radin
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: radot
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sigt4
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: tg
real      ,DIMENSION(grid%sm32:grid%em32)           :: dfrlg
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: lvl
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: cwm
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: f_ice
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: f_rain
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: f_rimef
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sr
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: u00
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: cfrach
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: cfracl
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: cfracm
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: lc
integer   ,DIMENSION(grid%sm32:grid%em32)           :: ul
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: islope
real      ,DIMENSION(grid%sm32:grid%em32)           :: dzsoil
real      ,DIMENSION(grid%sm32:grid%em32)           :: rtdpth
real      ,DIMENSION(grid%sm32:grid%em32)           :: sldpth
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: cmc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: grnflx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: pctsno
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soiltb
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: vegfrc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: shdmin
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: shdmax
real      ,DIMENSION(grid%sm31:grid%em31,grid%num_soil_layers,grid%sm33:grid%em33)           :: sh2o
real      ,DIMENSION(grid%sm31:grid%em31,grid%num_soil_layers,grid%sm33:grid%em33)           :: smc
real      ,DIMENSION(grid%sm31:grid%em31,grid%num_soil_layers,grid%sm33:grid%em33)           :: stc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: dwdtmn
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: dwdtmx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: dwdt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: pdwdt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: pint
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: w
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: z
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: acfrcv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: acfrst
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: ssroff
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: bgroff
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: rlwin
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: rlwout
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: rlwtoa
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: alwin
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: alwout
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: alwtoa
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: totswdn
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: totlwdn
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: rswin
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: rswout
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: rswtoa
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: aswin
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: aswout
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: aswtoa
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sfcshx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sfclhx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: subshx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: snopcx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sfcuvx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: potevp
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: potflx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: tlmin
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: tlmax
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rlwtt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rswtt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: tcucn
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: train
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: ncfrcv
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: ncfrst
integer   ,DIMENSION(grid%sm33:grid%em33)           :: ihe
integer   ,DIMENSION(grid%sm33:grid%em33)           :: ihw
integer   ,DIMENSION(grid%sm33:grid%em33)           :: ive
integer   ,DIMENSION(grid%sm33:grid%em33)           :: ivw
integer   ,DIMENSION(grid%sm31:grid%em31)           :: irad
integer   ,DIMENSION(1:2500)             :: iheg
integer   ,DIMENSION(1:2500)             :: ihwg
integer   ,DIMENSION(1:2500)             :: iveg
integer   ,DIMENSION(1:2500)             :: ivwg
integer   ,DIMENSION(1:2000)             :: iradg
integer   ,DIMENSION(-3:3,1:2500,0:6)           :: indx3_wrk
integer   ,DIMENSION(grid%sm33:grid%em33)           :: n_iup_h
integer   ,DIMENSION(grid%sm33:grid%em33)           :: n_iup_v
integer   ,DIMENSION(grid%sm33:grid%em33)           :: n_iup_adh
integer   ,DIMENSION(grid%sm33:grid%em33)           :: n_iup_adv
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: iup_h
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: iup_v
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: iup_adh
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: iup_adv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sm000010
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sm010040
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sm040100
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sm100200
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sm010200
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilm000
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilm005
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilm020
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilm040
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilm160
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilm300
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sw000010
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sw010040
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sw040100
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sw100200
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sw010200
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilw000
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilw005
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilw020
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilw040
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilw160
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilw300
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: st000010
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: st010040
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: st040100
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: st100200
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: st010200
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilt000
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilt005
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilt020
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilt040
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilt160
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: soilt300
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: landmask
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: topostdv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: toposlpx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: toposlpy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: greenmax
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: greenmin
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: albedomx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: slopecat
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: toposoil
real      ,DIMENSION(grid%sm31:grid%em31,grid%num_land_cat,grid%sm33:grid%em33)           :: landusef
real      ,DIMENSION(grid%sm31:grid%em31,grid%num_soil_cat,grid%sm33:grid%em33)           :: soilctop
real      ,DIMENSION(grid%sm31:grid%em31,grid%num_soil_cat,grid%sm33:grid%em33)           :: soilcbot
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: moist_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: moist_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_chem)           :: chem_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_chem)           :: chem_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%num_soil_layers,grid%sm33:grid%em33)           :: smois
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: th2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: u10
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: v10
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: xice
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: smstav
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: smstot
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sfcrunoff
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: udrunoff
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: ivgtyp
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: isltyp
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: vegfra
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sfcevp
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: grdflx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: albbck
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sfcexc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: acsnow
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: acsnom
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: snow
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: canwat
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: sst
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: weasd
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: mol
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: znt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: tke_myj
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: thz0
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: qz0
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: uz0
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: vz0
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: uz0h
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: vz0h
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: dudt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: dvdt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: qsfc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: akhs
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: akms
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: htop
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: hbot
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: cuppt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: t0eta
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: q0eta
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: p0eta
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: f_ice_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: f_rain_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: f_rimef_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: mass_flux
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: apr_gr
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: apr_w
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: apr_mc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: apr_st
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: apr_as
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: apr_capma
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: apr_capme
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: apr_capmi
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%ensdim)           :: xf_ens
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%ensdim)           :: pr_ens
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rthften
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rqvften
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: snowh
real      ,DIMENSION(grid%sm31:grid%em31,grid%num_soil_layers,grid%sm33:grid%em33)           :: smfr3d
!ENDOFREGISTRYGENERATEDINCLUDE

END SUBROUTINE solve_nmm

   END INTERFACE

   TYPE(domain) , INTENT(INOUT)  :: grid
   TYPE (grid_config_rec_type)   :: config_flags

   INTEGER     :: idum1, idum2


   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
   CALL set_scalar_indices_from_config ( grid%id , idum1 , idum2 )


   IF ( .FALSE.                         ) THEN

   ELSE IF ( config_flags%dyn_opt == DYN_NMM  ) THEN
     CALL solve_nmm  ( grid , config_flags , &
!
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/nmm_actual_args.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
grid%nmm_pd_b,grid%nmm_pd_bt,grid%nmm_t_b,grid%nmm_t_bt,grid%nmm_q_b,grid%nmm_q_bt,grid%nmm_u_b,grid%nmm_u_bt,grid%nmm_v_b, &
grid%nmm_v_bt,grid%nmm_q2_b,grid%nmm_q2_bt,grid%nmm_cwm_b,grid%nmm_cwm_bt,grid%nmm_lmh,grid%nmm_lmv,grid%nmm_hbm2,grid%nmm_hbm3, &
grid%nmm_vbm2,grid%nmm_vbm3,grid%nmm_sm,grid%nmm_sice,grid%nmm_htm,grid%nmm_vtm,grid%nmm_pd,grid%nmm_fis,grid%nmm_res, &
grid%nmm_t,grid%nmm_q,grid%nmm_u,grid%nmm_v,grid%nmm_told,grid%nmm_uold,grid%nmm_vold,grid%nmm_dx_nmm,grid%nmm_wpdar, &
grid%nmm_cpgfu,grid%nmm_curv,grid%nmm_fcp,grid%nmm_fdiv,grid%nmm_f,grid%nmm_fad,grid%nmm_ddmpu,grid%nmm_ddmpv,grid%nmm_deta, &
grid%nmm_rdeta,grid%nmm_aeta,grid%nmm_f4q2,grid%nmm_etax,grid%nmm_dfl,grid%nmm_deta1,grid%nmm_aeta1,grid%nmm_eta1, &
grid%nmm_deta2,grid%nmm_aeta2,grid%nmm_eta2,grid%nmm_em,grid%nmm_emt,grid%nmm_adt,grid%nmm_adu,grid%nmm_adv,grid%nmm_em_loc, &
grid%nmm_emt_loc,grid%nmm_pdsl,grid%nmm_pdslo,grid%nmm_psdt,grid%nmm_div,grid%nmm_few,grid%nmm_fne,grid%nmm_fns,grid%nmm_fse, &
grid%nmm_omgalf,grid%nmm_petdt,grid%nmm_rtop,grid%nmm_lpbl,grid%nmm_ustar,grid%nmm_z0,grid%nmm_z0base,grid%nmm_ths,grid%nmm_qs, &
grid%nmm_twbs,grid%nmm_qwbs,grid%nmm_prec,grid%nmm_aprec,grid%nmm_acprec,grid%nmm_cuprec,grid%nmm_accliq,grid%nmm_sno, &
grid%nmm_si,grid%nmm_cldefi,grid%nmm_deep,grid%nmm_rf,grid%nmm_th10,grid%nmm_q10,grid%nmm_pshltr,grid%nmm_tshltr, &
grid%nmm_qshltr,grid%nmm_q2,grid%nmm_t_adj,grid%nmm_t_old,grid%nmm_zero_3d,grid%nmm_w0avg,grid%nmm_albase,grid%nmm_albedo, &
grid%nmm_cnvbot,grid%nmm_cnvtop,grid%nmm_czen,grid%nmm_czmean,grid%nmm_epsr,grid%nmm_gffc,grid%nmm_glat,grid%nmm_glon, &
grid%nmm_nmm_tsk,grid%nmm_hdac,grid%nmm_hdacv,grid%nmm_mxsnal,grid%nmm_radin,grid%nmm_radot,grid%nmm_sigt4,grid%nmm_tg, &
grid%nmm_dfrlg,grid%nmm_lvl,grid%nmm_cwm,grid%nmm_f_ice,grid%nmm_f_rain,grid%nmm_f_rimef,grid%nmm_sr,grid%nmm_u00, &
grid%nmm_cfrach,grid%nmm_cfracl,grid%nmm_cfracm,grid%nmm_lc,grid%nmm_ul,grid%nmm_islope,grid%nmm_dzsoil,grid%nmm_rtdpth, &
grid%nmm_sldpth,grid%nmm_cmc,grid%nmm_grnflx,grid%nmm_pctsno,grid%nmm_soiltb,grid%nmm_vegfrc,grid%nmm_shdmin,grid%nmm_shdmax, &
grid%nmm_sh2o,grid%nmm_smc,grid%nmm_stc,grid%nmm_dwdtmn,grid%nmm_dwdtmx,grid%nmm_dwdt,grid%nmm_pdwdt,grid%nmm_pint,grid%nmm_w, &
grid%nmm_z,grid%nmm_acfrcv,grid%nmm_acfrst,grid%nmm_ssroff,grid%nmm_bgroff,grid%nmm_rlwin,grid%nmm_rlwout,grid%nmm_rlwtoa, &
grid%nmm_alwin,grid%nmm_alwout,grid%nmm_alwtoa,grid%nmm_totswdn,grid%nmm_totlwdn,grid%nmm_rswin,grid%nmm_rswout,grid%nmm_rswtoa, &
grid%nmm_aswin,grid%nmm_aswout,grid%nmm_aswtoa,grid%nmm_sfcshx,grid%nmm_sfclhx,grid%nmm_subshx,grid%nmm_snopcx,grid%nmm_sfcuvx, &
grid%nmm_potevp,grid%nmm_potflx,grid%nmm_tlmin,grid%nmm_tlmax,grid%nmm_rlwtt,grid%nmm_rswtt,grid%nmm_tcucn,grid%nmm_train, &
grid%nmm_ncfrcv,grid%nmm_ncfrst,grid%nmm_ihe,grid%nmm_ihw,grid%nmm_ive,grid%nmm_ivw,grid%nmm_irad,grid%nmm_iheg,grid%nmm_ihwg, &
grid%nmm_iveg,grid%nmm_ivwg,grid%nmm_iradg,grid%nmm_indx3_wrk,grid%nmm_n_iup_h,grid%nmm_n_iup_v,grid%nmm_n_iup_adh, &
grid%nmm_n_iup_adv,grid%nmm_iup_h,grid%nmm_iup_v,grid%nmm_iup_adh,grid%nmm_iup_adv,grid%sm000010,grid%sm010040,grid%sm040100, &
grid%sm100200,grid%sm010200,grid%soilm000,grid%soilm005,grid%soilm020,grid%soilm040,grid%soilm160,grid%soilm300,grid%sw000010, &
grid%sw010040,grid%sw040100,grid%sw100200,grid%sw010200,grid%soilw000,grid%soilw005,grid%soilw020,grid%soilw040,grid%soilw160, &
grid%soilw300,grid%st000010,grid%st010040,grid%st040100,grid%st100200,grid%st010200,grid%soilt000,grid%soilt005,grid%soilt020, &
grid%soilt040,grid%soilt160,grid%soilt300,grid%landmask,grid%topostdv,grid%toposlpx,grid%toposlpy,grid%greenmax,grid%greenmin, &
grid%albedomx,grid%slopecat,grid%toposoil,grid%landusef,grid%soilctop,grid%soilcbot,grid%moist_1,grid%moist_2,grid%chem_1, &
grid%chem_2,grid%smois,grid%th2,grid%u10,grid%v10,grid%xice,grid%smstav,grid%smstot,grid%sfcrunoff,grid%udrunoff,grid%ivgtyp, &
grid%isltyp,grid%vegfra,grid%sfcevp,grid%grdflx,grid%albbck,grid%sfcexc,grid%acsnow,grid%acsnom,grid%snow,grid%canwat,grid%sst, &
grid%weasd,grid%mol,grid%znt,grid%tke_myj,grid%thz0,grid%qz0,grid%uz0,grid%vz0,grid%uz0h,grid%vz0h,grid%dudt,grid%dvdt, &
grid%qsfc,grid%akhs,grid%akms,grid%htop,grid%hbot,grid%cuppt,grid%t0eta,grid%q0eta,grid%p0eta,grid%f_ice_phy,grid%f_rain_phy, &
grid%f_rimef_phy,grid%mass_flux,grid%apr_gr,grid%apr_w,grid%apr_mc,grid%apr_st,grid%apr_as,grid%apr_capma,grid%apr_capme, &
grid%apr_capmi,grid%xf_ens,grid%pr_ens,grid%rthften,grid%rqvften,grid%snowh,grid%smfr3d &
!ENDOFREGISTRYGENERATEDINCLUDE
!
               )

! ###### 4. Edit share/solve_interface.F to add call to experimental core


   ELSE

     WRITE( wrf_err_message , * ) 'Invalid dynamics option: dyn_opt = ',config_flags%dyn_opt
     CALL wrf_error_fatal ( TRIM ( wrf_err_message ) )
     
   END IF

END SUBROUTINE solve_interface

