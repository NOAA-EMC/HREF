!#define NO_RESTRICT_ACCEL
!#define NO_GFDLETAINIT
!#define NO_UPSTREAM_ADVECTION
!----------------------------------------------------------------------
!
      SUBROUTINE START_DOMAIN_NMM(GRID,                                &
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
     &           )
!----------------------------------------------------------------------
!
      USE MODULE_DOMAIN
      USE MODULE_DRIVER_CONSTANTS
      USE module_model_constants
      USE MODULE_CONFIGURE
      USE MODULE_WRF_ERROR
      USE MODULE_MPP
      USE MODULE_CTLBLK
      USE module_dm
!
      USE MODULE_PHYS
!jm      USE MODULE_CLDWTR
!jm      USE MODULE_SOIL
!jm      USE MODULE_NHYDRO
      USE MODULE_IGWAVE_ADJUST,ONLY: PDTE, PFDHT, DDAMP
      USE MODULE_ADVECTION,    ONLY: ADVE, VAD2, HAD2
      USE MODULE_NONHY_DYNAM,  ONLY: VADZ, HADZ
      USE MODULE_DIFFUSION_NMM,ONLY: HDIFF
      USE MODULE_BNDRY_COND,   ONLY: BOCOH, BOCOV
      USE MODULE_PHYSICS_INIT
      USE MODULE_RA_GFDLETA
!!!!!tlb
      USE module_ext_internal
!!!!!tlb
!----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!----------------------------------------------------------------------
!***
!***  INPUT DATA
!***
      TYPE(DOMAIN),INTENT(INOUT) :: GRID
!
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
!
      TYPE(GRID_CONFIG_REC_TYPE) :: CONFIG_FLAGS
!
!
!***
!***  LOCAL DATA
!***
      INTEGER :: IDS,IDE,JDS,JDE,KDS,KDE                               &
     &          ,IMS,IME,JMS,JME,KMS,KME                                &
     &          ,IPS,IPE,JPS,JPE,KPS,KPE
!
      INTEGER :: ERROR,LOOP

      REAL,ALLOCATABLE,DIMENSION(:) :: PHALF
!
      REAL :: EPSB=0.1,EPSIN=9.8
!
      INTEGER :: JHL=7
!
      INTEGER :: I,IEND,IER,IERR,IFE,IFS,IHH,IHL,IHRSTB,II,IRTN        &
     &          ,ISIZ1,ISIZ2,ISTART,IX,J,J00,JFE,JFS,JHH,JJ             &
     &          ,JM1,JM2,JM3,JP1,JP2,JP3,JX                             &
     &          ,K,K400,KBI,KBI2,KCCO2,KNT,KNTI,KOFF,KOFV               &
     &          ,LB,LLMH,LMHK,LMVK,LRECBC                               &
     &          ,N,NMAP,NRADLH,NRADSH,NREC,NS,RECL,STAT                 &
     &          ,STEPBL,STEPCU,STEPRA
!
      INTEGER :: ILPAD2,IRPAD2,JBPAD2,JTPAD2
      INTEGER :: ITS,ITE,JTS,JTE,KTS,KTE
!
      REAL :: ADDL,APELM,APELMNW,APEM1,CAPA,CLOGES,DPLM,DZLM,EPS,ESE   &
     &       ,FAC1,FAC2,PDIF,PLM,PM1,PSFCK,PSS,PSUM,QLM,RANG,RCOS1      &
     &       ,RCOS2,RSIN1,SLPM,TERM1,THLM,TIME,TLM,TSFCK,ULM,VLM
!
!!!   REAL :: BLDT,CWML,EXNSFC,G_INV,PLYR,PSFC,ROG,SFCZ,THSIJ,TL
      REAL :: CWML,EXNSFC,G_INV,PLYR,PSFC,ROG,SFCZ,THSIJ,TL
      REAL :: TSTART,TEND,TPREC,THEAT,TCLOD,TRDSW,TRDLW,TSRFC

!
!!!   REAL,ALLOCATABLE,DIMENSION(:,:) :: RAINBL,RAINNC,RAINNC           &
      INTEGER,ALLOCATABLE,DIMENSION(:,:) :: LOWLYR
      REAL,ALLOCATABLE,DIMENSION(:) :: SFULL,SMID
!state    real   DZS             l        dyn_em      -         Z     ir 
!state    real  CLDFRA          ikj      dyn_em        1         -      r
!state    real  RQCBLTEN        ikj      dyn_em        1         -      r
!state    real  RQIBLTEN        ikj      dyn_em        1         -      r
!state    real  RQVBLTEN        ikj      dyn_em        1         -      r
!state    real  RTHBLTEN        ikj      dyn_em        1         -      r
!state    real  RUBLTEN         ikj      dyn_em        1         -      r
!state    real  RVBLTEN         ikj      dyn_em        1         -      r
!state    real  RQCCUTEN        ikj      dyn_em        1         -      r
!state    real  RQICUTEN        ikj      dyn_em        1         -      r
!state    real  RQRCUTEN        ikj      dyn_em        1         -      r
!state    real  RQSCUTEN        ikj      dyn_em        1         -      r
!state    real  RQVCUTEN        ikj      dyn_em        1         -      r
!state    real  RTHCUTEN        ikj      dyn_em        1         -      r
!state    real  RTHRATEN        ikj      dyn_em        1         -      r
!state    real  RTHRATENLW      ikj      dyn_em        1         -      r
!state    real  RTHRATENSW      ikj      dyn_em        1         -      r
!state    real   TSLB           ilj       dyn_em      1         Z     irh
!state    real   ZS              l        dyn_em      -         Z     ir 
      REAL,ALLOCATABLE,DIMENSION(:) :: DZS,ZS
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: CLDFRA &
     &                                  ,RQCBLTEN,RQIBLTEN              &
     &                                  ,RQVBLTEN,RTHBLTEN              &
     &                                  ,RUBLTEN,RVBLTEN                &
     &                                  ,RQCCUTEN,RQICUTEN,RQRCUTEN     &
     &                                  ,RQSCUTEN,RQVCUTEN,RTHCUTEN     &
     &                                  ,RTHRATEN                       &
     &                                  ,RTHRATENLW,RTHRATENSW          &
     &                                  ,TSLB
      REAL,ALLOCATABLE,DIMENSION(:,:) :: EMISS,GLW,GSW,HFX   &
     &                                  ,LU_INDEX,MAVAIL,NCA,PBLH       &
     &                                  ,QFX,RAINBL,RAINC,RAINNC        &
     &                                  ,RAINCV,RAINNCV                 &
     &                                  ,SNOWC,THC,TMN,TSFC             &
     &                                  ,XLAND,XLAT,XLONG

      REAL,ALLOCATABLE,DIMENSION(:,:) :: Z0_DUM
!
!!!   REAL,ALLOCATABLE,DIMENSION(:,:,:) :: W0AVG,ZINT,ZMID
      LOGICAL :: E_BDY,N_BDY,S_BDY,W_BDY,WARM_RAIN
      integer :: jam,retval
      character(20) :: seeout="hi08.t00z.nhbmeso"
      real :: dummyx(791)
      integer myproc
      real :: dsig,dsigsum,pdbot,pdtot,rpdtot
      real :: fisx,ht,prodx,rg
      integer :: i_t=096,j_t=195,n_t=11
      integer :: i_u=49,j_u=475,n_u=07
      integer :: i_v=49,j_v=475,n_v=07
!
!----------------------------------------------------------------------
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/nmm_scalar_derefs.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
 dy_nmm = grid%nmm_dy_nmm
 cpgfv = grid%nmm_cpgfv
 en = grid%nmm_en
 ent = grid%nmm_ent
 f4d = grid%nmm_f4d
 f4q = grid%nmm_f4q
 ef4t = grid%nmm_ef4t
 upstrm = grid%nmm_upstrm
 dlmd = grid%nmm_dlmd
 dphd = grid%nmm_dphd
 pdtop = grid%nmm_pdtop
 pt = grid%nmm_pt
 micro_start = grid%nmm_micro_start
 hydro = grid%nmm_hydro
 nclod = grid%nmm_nclod
 nprec = grid%nmm_nprec
 nheat = grid%nmm_nheat
 nrdlw = grid%nmm_nrdlw
 nrdsw = grid%nmm_nrdsw
 nsrfc = grid%nmm_nsrfc
 avrain = grid%nmm_avrain
 avcnvc = grid%nmm_avcnvc
 aratim = grid%nmm_aratim
 acutim = grid%nmm_acutim
 ardlw = grid%nmm_ardlw
 ardsw = grid%nmm_ardsw
 asrfc = grid%nmm_asrfc
 aphtim = grid%nmm_aphtim
 dtbc = grid%dtbc
 number_at_same_level = grid%number_at_same_level
 itimestep = grid%itimestep
 oid = grid%oid
 auxhist1_oid = grid%auxhist1_oid
 auxhist2_oid = grid%auxhist2_oid
 auxhist3_oid = grid%auxhist3_oid
 auxhist4_oid = grid%auxhist4_oid
 auxhist5_oid = grid%auxhist5_oid
 auxinput1_oid = grid%auxinput1_oid
 auxinput2_oid = grid%auxinput2_oid
 auxinput3_oid = grid%auxinput3_oid
 auxinput4_oid = grid%auxinput4_oid
 auxinput5_oid = grid%auxinput5_oid
 nframes = grid%nframes
 lbc_fid = grid%lbc_fid
 tiled = grid%tiled
 patched = grid%patched
 input_from_file = grid%input_from_file
 write_metadata = grid%write_metadata
 time_step = grid%time_step
 time_step_fract_num = grid%time_step_fract_num
 time_step_fract_den = grid%time_step_fract_den
 restart = grid%restart
 max_dom = grid%max_dom
 dyn_opt = grid%dyn_opt
 rk_ord = grid%rk_ord
 diff_opt = grid%diff_opt
 km_opt = grid%km_opt
 damp_opt = grid%damp_opt
 isfflx = grid%isfflx
 ifsnow = grid%ifsnow
 icloud = grid%icloud
 num_soil_layers = grid%num_soil_layers
 num_land_cat = grid%num_land_cat
 num_soil_cat = grid%num_soil_cat
 spec_bdy_width = grid%spec_bdy_width
 spec_zone = grid%spec_zone
 relax_zone = grid%relax_zone
 ensdim = grid%ensdim
 tile_sz_x = grid%tile_sz_x
 tile_sz_y = grid%tile_sz_y
 numtiles = grid%numtiles
 debug_level = grid%debug_level
 irand = grid%irand
 run_days = grid%run_days
 run_hours = grid%run_hours
 run_minutes = grid%run_minutes
 run_seconds = grid%run_seconds
 start_year = grid%start_year
 start_month = grid%start_month
 start_day = grid%start_day
 start_hour = grid%start_hour
 start_minute = grid%start_minute
 start_second = grid%start_second
 end_year = grid%end_year
 end_month = grid%end_month
 end_day = grid%end_day
 end_hour = grid%end_hour
 end_minute = grid%end_minute
 end_second = grid%end_second
 grid_id = grid%grid_id
 level = grid%level
 s_we = grid%s_we
 e_we = grid%e_we
 s_sn = grid%s_sn
 e_sn = grid%e_sn
 s_vert = grid%s_vert
 e_vert = grid%e_vert
 history_interval = grid%history_interval
 auxhist1_interval = grid%auxhist1_interval
 auxhist2_interval = grid%auxhist2_interval
 auxhist3_interval = grid%auxhist3_interval
 auxhist4_interval = grid%auxhist4_interval
 auxhist5_interval = grid%auxhist5_interval
 auxinput1_interval = grid%auxinput1_interval
 auxinput2_interval = grid%auxinput2_interval
 auxinput3_interval = grid%auxinput3_interval
 auxinput4_interval = grid%auxinput4_interval
 auxinput5_interval = grid%auxinput5_interval
 restart_interval = grid%restart_interval
 frames_per_outfile = grid%frames_per_outfile
 time_step_sound = grid%time_step_sound
 parent_id = grid%parent_id
 i_parent_start = grid%i_parent_start
 j_parent_start = grid%j_parent_start
 shw = grid%shw
 parent_grid_ratio = grid%parent_grid_ratio
 parent_time_step_ratio = grid%parent_time_step_ratio
 moad_grid_ratio = grid%moad_grid_ratio
 moad_time_step_ratio = grid%moad_time_step_ratio
 non_hydrostatic = grid%non_hydrostatic
 dx = grid%dx
 dy = grid%dy
 dt = grid%dt
 ztop = grid%ztop
 zdamp = grid%zdamp
 dampcoef = grid%dampcoef
 smdiv = grid%smdiv
 emdiv = grid%emdiv
 epssm = grid%epssm
 khdif = grid%khdif
 kvdif = grid%kvdif
 mix_cr_len = grid%mix_cr_len
 tke_upper_bound = grid%tke_upper_bound
 kh_tke_upper_bound = grid%kh_tke_upper_bound
 kv_tke_upper_bound = grid%kv_tke_upper_bound
 radt = grid%radt
 bldt = grid%bldt
 cudt = grid%cudt
 gsmdt = grid%gsmdt
 julyr = grid%julyr
 julday = grid%julday
 gmt = grid%gmt
 periodic_x = grid%periodic_x
 symmetric_xs = grid%symmetric_xs
 symmetric_xe = grid%symmetric_xe
 open_xs = grid%open_xs
 open_xe = grid%open_xe
 periodic_y = grid%periodic_y
 symmetric_ys = grid%symmetric_ys
 symmetric_ye = grid%symmetric_ye
 open_ys = grid%open_ys
 open_ye = grid%open_ye
 nested = grid%nested
 specified = grid%specified
 top_radiation = grid%top_radiation
 idtad = grid%idtad
 nsoil = grid%nsoil
 nphs = grid%nphs
 ncnvc = grid%ncnvc
 nrads = grid%nrads
 nradl = grid%nradl
 sigma = grid%sigma
 chem_opt = grid%chem_opt
 mp_physics = grid%mp_physics
 ra_lw_physics = grid%ra_lw_physics
 ra_sw_physics = grid%ra_sw_physics
 bl_sfclay_physics = grid%bl_sfclay_physics
 bl_surface_physics = grid%bl_surface_physics
 bl_pbl_physics = grid%bl_pbl_physics
 cu_physics = grid%cu_physics
 h_mom_adv_order = grid%h_mom_adv_order
 v_mom_adv_order = grid%v_mom_adv_order
 h_sca_adv_order = grid%h_sca_adv_order
 v_sca_adv_order = grid%v_sca_adv_order
 io_form_input = grid%io_form_input
 io_form_auxinput1 = grid%io_form_auxinput1
 io_form_auxinput2 = grid%io_form_auxinput2
 io_form_auxinput3 = grid%io_form_auxinput3
 io_form_auxinput4 = grid%io_form_auxinput4
 io_form_auxinput5 = grid%io_form_auxinput5
 io_form_history = grid%io_form_history
 io_form_auxhist1 = grid%io_form_auxhist1
 io_form_auxhist2 = grid%io_form_auxhist2
 io_form_auxhist3 = grid%io_form_auxhist3
 io_form_auxhist4 = grid%io_form_auxhist4
 io_form_auxhist5 = grid%io_form_auxhist5
 io_form_restart = grid%io_form_restart
 io_form_boundary = grid%io_form_boundary
 interval_seconds = grid%interval_seconds
 real_data_init_type = grid%real_data_init_type
 cen_lat = grid%cen_lat
 cen_lon = grid%cen_lon
 truelat1 = grid%truelat1
 truelat2 = grid%truelat2
 bdyfrq = grid%bdyfrq
 iswater = grid%iswater
 isice = grid%isice
 map_proj = grid%map_proj
!ENDOFREGISTRYGENERATEDINCLUDE
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
!
      CALL GET_IJK_FROM_GRID(GRID,                                     &
     &                       IDS,IDE,JDS,JDE,KDS,KDE,                  &
     &                       IMS,IME,JMS,JME,KMS,KME,                  &
     &                       IPS,IPE,JPS,JPE,KPS,KPE)
!
      ITS=IPS
      ITE=IPE
      JTS=JPS
      JTE=JPE
      KTS=KPS
      KTE=KPE

      CALL model_to_grid_config_rec(grid%id,model_config_rec           &
     &                             ,config_flags)
!
        RESTRT=config_flags%restart
        write(0,*) 'set RESTRT to: ', RESTRT

      IF(IME.GT. 2500 )THEN
        WRITE(wrf_err_message,*)                                       &
         'start_domain_nmm ime (',ime,') > ',2500,    &
         '. Increase NMM_MAX_DIM in configure.wrf, clean, and recompile.'
        CALL WRF_ERROR_FATAL(wrf_err_message)
      ENDIF
!
      IF(JME.GT. 2500 )THEN
        WRITE(wrf_err_message,*)                                       &
         'start_domain_nmm jme (',jme,') > ',2500,    &
         '. Increase NMM_MAX_DIM in configure.wrf, clean, and recompile.'
        CALL WRF_ERROR_FATAL(wrf_err_message)
      ENDIF
!
!---------------------------------------------------------------------- 
!
      WRITE(0,196)IHRST,IDAT
      WRITE(LIST,196)IHRST,IDAT
  196 FORMAT(' FORECAST BEGINS ',I2,' GMT ',2(I2,'/'),I4)
!!!!!!tlb
!!!! For now, set NPES to 1
      NPES=1
!!!!!!tlb
      MY_IS_GLB=IPS
      MY_IE_GLB=IPE-1
      MY_JS_GLB=JPS
      MY_JE_GLB=JPE-1
!
      IM=IPE-1
      JM=JPE-1
!!!!!!!!!
!! All "my" variables defined below have had the IDE or JDE specification
!! reduced by 1
!!!!!!!!!!!

      MYIS=MAX(IDS,IPS)
      MYIE=MIN(IDE-1,IPE)
      MYJS=MAX(JDS,JPS)
      MYJE=MIN(JDE-1,JPE)

      MYIS1  =MAX(IDS+1,IPS)
      MYIE1  =MIN(IDE-2,IPE)
      MYJS2  =MAX(JDS+2,JPS)
      MYJE2  =MIN(JDE-3,JPE)
!
      MYIS_P1=MAX(IDS,IPS-1)
      MYIE_P1=MIN(IDE-1,IPE+1)
      MYIS_P2=MAX(IDS,IPS-2)
      MYIE_P2=MIN(IDE-1,IPE+2)
      MYIS_P3=MAX(IDS,IPS-3)
      MYIE_P3=MIN(IDE-1,IPE+3)
      MYJS_P3=MAX(JDS,JPS-3)
      MYJE_P3=MIN(JDE-1,JPE+3)
      MYIS_P4=MAX(IDS,IPS-4)
      MYIE_P4=MIN(IDE-1,IPE+4)
      MYJS_P4=MAX(JDS,JPS-4)
      MYJE_P4=MIN(JDE-1,JPE+4)
      MYIS_P5=MAX(IDS,IPS-5)
      MYIE_P5=MIN(IDE-1,IPE+5)
      MYJS_P5=MAX(JDS,JPS-5)
      MYJE_P5=MIN(JDE-1,JPE+5)
!
      MYIS1_P1=MAX(IDS+1,IPS-1)
      MYIE1_P1=MIN(IDE-2,IPE+1)
      MYIS1_P2=MAX(IDS+1,IPS-2)
      MYIE1_P2=MIN(IDE-2,IPE+2)
!
      MYJS1_P1=MAX(JDS+1,JPS-1)
      MYJS2_P1=MAX(JDS+2,JPS-1)
      MYJE1_P1=MIN(JDE-2,JPE+1)
      MYJE2_P1=MIN(JDE-3,JPE+1)
      MYJS1_P2=MAX(JDS+1,JPS-2)
      MYJE1_P2=MIN(JDE-2,JPE+2)
      MYJS2_P2=MAX(JDS+2,JPS-2)
      MYJE2_P2=MIN(JDE-3,JPE+2)
      MYJS1_P3=MAX(JDS+1,JPS-3)
      MYJE1_P3=MIN(JDE-2,JPE+3)
      MYJS2_P3=MAX(JDS+2,JPS-3)
      MYJE2_P3=MIN(JDE-3,JPE+3)
!!!!!!!!!!!
!

!     if(mype.eq.n_t)then
!       do ns=1,nsoil
!         write(0,*) enter START sh2o=,sh2o(i_t,ns,j_t)
!       enddo
!     endif
      DO J=MYJS_P4,MYJE_P4
        IHEG(J)=MOD(J+1,2)
        IHWG(J)=IHEG(J)-1
        IVEG(J)=MOD(J,2)
        IVWG(J)=IVEG(J)-1
      ENDDO
!
      DO J=MYJS_P4,MYJE_P4
        IVW(J)=IVWG(J)
        IVE(J)=IVEG(J)
        IHE(J)=IHEG(J)
        IHW(J)=IHWG(J)
      ENDDO
!
      CAPA=R_D/CP
      LM=KPE-KPS+1
!
      IFS=IPS
      JFS=JPS
      JFE=MIN(JPE,JDE-1)
      IFE=MIN(IPE,IDE-1)
!
      IF(.NOT.RESTRT)THEN
       call wrf_get_myproc(mype)
        DO J=JFS,JFE
        DO I=IFS,IFE
          LLMH=LMH(I,J)
          KOFF=KPE-1-LLMH
          PDSL(I,J)  =PD(I,J)*RES(I,J)
          PREC(I,J)  =0.
          ACPREC(I,J)=0.
          CUPREC(I,J)=0.
          rg=1./g
          ht=fis(i,j)*rg
!!!       fisx=ht*g
          fisx=max(fis(i,j),0.)
          prodx=Z0(I,J)*Z0MAX
!     if(mype.eq.n_t.and.i.eq.i_t.and.j.eq.j_t)then
!       write(0,*) INIT z0=,z0(i,j), z0max=,z0max, z0land=,z0land
!       write(0,*) z0sea=,z0sea, fis=,ht*g, fisx=,fisx
!     endif
          Z0(I,J)    =SM(I,J)*Z0SEA+(1.-SM(I,J))*                      &
     &                (Z0(I,J)*Z0MAX+FISx    *FCM+Z0LAND)
!!!  &                (prodx        +FISx    *FCM+Z0LAND)
!     if(mype.eq.n_t.and.i.eq.i_t.and.j.eq.j_t)then
!       write(0,*) INIT z0=,z0(i,j)
!     endif
          QS(I,J)    =0.
          AKMS(I,J)  =0.
          AKHS(I,J)  =0.
          TWBS(I,J)  =0.
          QWBS(I,J)  =0.
          CLDEFI(I,J)=1.
!!!!          HTOP(I,J)  =REAL(LLMH)
!!!!          HBOT(I,J)  =REAL(LLMH)
          HTOP(I,J)  =REAL(KTS)
          HBOT(I,J)  =REAL(KTE)
!***
!***  AT THIS POINT, WE MUST CALCULATE THE INITIAL POTENTIAL TEMPERATURE
!***  OF THE SURFACE AND OF THE SUBGROUND.
!***  EXTRAPOLATE DOWN FOR INITIAL SURFACE POTENTIAL TEMPERATURE.
!***  ALSO DO THE SHELTER PRESSURE.
!***
          PM1=AETA1(KOFF+1)*PDTOP+AETA2(KOFF+1)*PDSL(I,J)+PT
          APEM1=(1.E5/PM1)**CAPA

        IF (NMM_TSK(I,J) .ge. 200.) THEN ! have a specific skin temp, use it
               THS(I,J)=NMM_TSK(I,J)*(1.+P608*Q(I,KOFF+1,J))*APEM1
               TSFCK=NMM_TSK(I,J)*(1.+P608*Q(I,KOFF+1,J))
	ELSE ! use lowest layer as a proxy
          THS(I,J)=T(I,KOFF+1,J)*(1.+P608*Q(I,KOFF+1,J))*APEM1
          TSFCK=T(I,KOFF+1,J)*(1.+P608*Q(I,KOFF+1,J))
	ENDIF

	if (I .eq. IFE/2 .and. J .eq. JFE/2) then
	write(6,*) 'I,J,T(I,KOFF+1,J),NMM_TSK(I,J):: ', I,J,T(I,KOFF+1,J),NMM_TSK(I,J)
	write(6,*) 'THS(I,J): ', THS(I,J)
	endif

          PSFCK=PD(I,J)+PDTOP+PT
!
          IF(SM(I,J).LT.0.5) THEN
            QS(I,J)=PQ0/PSFCK*EXP(A2*(TSFCK-A3)/(TSFCK-A4))
          ELSEIF(SM(I,J).GT.0.5) THEN
            THS(I,J)=SST(I,J)*(1.E5/(PD(I,J)+PDTOP+PT))**CAPA
          ENDIF
!
          TERM1=-0.068283/T(I,KOFF+1,J)
          PSHLTR(I,J)=(PD(I,J)+PDTOP+PT)*EXP(TERM1)
!
          USTAR(I,J)=0.1
          THZ0(I,J)=THS(I,J)
          QZ0(I,J)=QS(I,J)
          UZ0(I,J)=0.
          VZ0(I,J)=0.
! 
        ENDDO
        ENDDO
!***
!***  INITIALIZE CLOUD FIELDS
!***
        DO J=JFS,JFE
          DO K=KPS,KPE
          DO I=IFS,IFE
            CWM(I,K,J)=0.
          ENDDO
          ENDDO
        ENDDO
!***
!***  INITIALIZE ACCUMULATOR ARRAYS TO ZERO.
!***
        ARDSW=0.0
        ARDLW=0.0
        ASRFC=0.0
        AVRAIN=0.0
        AVCNVC=0.0
!
        DO J=JFS,JFE
        DO I=IFS,IFE
          ACFRCV(I,J)=0.
          NCFRCV(I,J)=0
          ACFRST(I,J)=0.
          NCFRST(I,J)=0
          ACSNOW(I,J)=0.
          ACSNOM(I,J)=0.
          SSROFF(I,J)=0.
          BGROFF(I,J)=0.
          ALWIN(I,J) =0.
          ALWOUT(I,J)=0.
          ALWTOA(I,J)=0.
          ASWIN(I,J) =0.
          ASWOUT(I,J)=0.
          ASWTOA(I,J)=0.
          SFCSHX(I,J)=0.
          SFCLHX(I,J)=0.
          SUBSHX(I,J)=0.
          SNOPCX(I,J)=0.
          SFCUVX(I,J)=0.
          SFCEVP(I,J)=0.
          POTEVP(I,J)=0.
          POTFLX(I,J)=0.
        ENDDO
        ENDDO
!***
!***  INITIALIZE SATURATION SPECIFIC HUMIDITY OVER THE WATER.
!***
        EPS=R_D/R_V
!
        DO J=JFS,JFE
        DO I=IFS,IFE
          IF(SM(I,J).GT.0.5)THEN
            CLOGES =-CM1/SST(I,J)-CM2*ALOG10(SST(I,J))+CM3
            ESE    = 10.**(CLOGES+2.)
            QS(I,J)= SM(I,J)*EPS*ESE/(PD(I,J)+PDTOP+PT-ESE*(1.-EPS))
          ENDIF
        ENDDO
        ENDDO
!***  
!***  INITIALIZE TURBULENT KINETIC ENERGY (TKE) TO A SMALL
!***  VALUE (EPSQ2) ABOVE GROUND.  SET TKE TO ZERO IN THE
!***  THE LOWEST MODEL LAYER.  IN THE LOWEST TWO ATMOSPHERIC
!***  ETA LAYERS SET TKE TO A SMALL VALUE (Q2INI).
!***
        DO J=JFS,JFE
        DO K=KPS,KPE-1
        DO I=IFS,IFE
          Q2(I,K,J)=HTM(I,K+1,J)*HBM2(I,J)*EPSQ2
        ENDDO
        ENDDO
        ENDDO
!
        DO J=JFS,JFE
        DO I=IFS,IFE
          Q2(I,LM,J)    = 0.
          LLMH          = LMH(I,J)
          Q2(I,LLMH-2,J)= HBM2(I,J)*Q2INI
          Q2(I,LLMH-1,J)= HBM2(I,J)*Q2INI
        ENDDO
        ENDDO
!***  
!***  PAD ABOVE GROUND SPECIFIC HUMIDITY IF IT IS TOO SMALL.
!***  INITIALIZE LATENT HEATING ACCUMULATION ARRAYS.
!***
        DO J=JFS,JFE
        DO K=KPS,KPE
        DO I=IFS,IFE
          IF(Q(I,K,J).LT.EPSQ)Q(I,K,J)=EPSQ*HTM(I,K,J)
          TRAIN(I,K,J)=0.
          TCUCN(I,K,J)=0.
        ENDDO
        ENDDO
        ENDDO
!
!----------------------------------------------------------------------
!***  END OF SCRATCH START INITIALIZATION BLOCK.
!----------------------------------------------------------------------
!
        CALL wrf_message('INIT:  INITIALIZED ARRAYS FOR CLEAN START')
!       IF(wrf_dm_on_monitor)THEN
!          WRITE(LIST,*)INIT:  INITIALIZED ARRAYS FOR CLEAN START
!        ENDIF
      ENDIF ! <--- (not restart)


!
!----------------------------------------------------------------------
!***  INITIALIZE PHYSICS VARIABLES IF STARTING THIS RUN FROM SCRATCH.
!----------------------------------------------------------------------
!
      IF(NEST)THEN
        DO J=JFS,JFE
        DO I=IFS,IFE
!
          LLMH=LMH(I,J)
          KOFF=KPE-1-LLMH
!
          IF(T(I,KOFF+1,J).EQ.0.)THEN
            T(I,KOFF+1,J)=T(I,KOFF+2,J)
          ENDIF
!
          TERM1=-0.068283/T(I,KOFF+1,J)
          PSHLTR(I,J)=(PD(I,J)+PDTOP+PT)*EXP(TERM1)
        ENDDO
        ENDDO
      ENDIF
!
      IF(.NOT.RESTRT)THEN
        DO J=JFS,JFE
        DO I=IFS,IFE
          LLMH=LMH(I,J)
          KOFF=KPE-1-LLMH
          PDSL(I,J)  =PD(I,J)*RES(I,J)
          PREC(I,J)  =0.
          ACPREC(I,J)=0.
          CUPREC(I,J)=0.
!          Z0(I,J)    =SM(I,J)*Z0SEA+(1.-SM(I,J))*                      &
!                      (FIS(I,J)*FCM+Z0LAND+Z0(I,J))
          QS(I,J)    =0.
          AKMS(I,J)  =0.
          AKHS(I,J)  =0.
          TWBS(I,J)  =0.
          QWBS(I,J)  =0.
          CLDEFI(I,J)=1.
!!!!          HTOP(I,J)  =REAL(LLMH)
!!!!          HBOT(I,J)  =REAL(LLMH)
          HTOP(I,J)  =REAL(KTS)
          HBOT(I,J)  =REAL(KTE)
!***
!***  AT THIS POINT, WE MUST CALCULATE THE INITIAL POTENTIAL TEMPERATURE
!***  OF THE SURFACE AND OF THE SUBGROUND.
!***  EXTRAPOLATE DOWN FOR INITIAL SURFACE POTENTIAL TEMPERATURE.
!***  ALSO DO THE SHELTER PRESSURE.
!***
          PM1=AETA1(KOFF+1)*PDTOP+AETA2(KOFF+1)*PDSL(I,J)+PT
          APEM1=(1.E5/PM1)**CAPA
        IF (NMM_TSK(I,J) .ge. 200.) THEN ! have a specific skin temp, use it
               THS(I,J)=NMM_TSK(I,J)*(1.+P608*Q(I,KOFF+1,J))*APEM1
               TSFCK=NMM_TSK(I,J)*(1.+P608*Q(I,KOFF+1,J))
	ELSE ! use lowest layer as a proxy
          THS(I,J)=T(I,KOFF+1,J)*(1.+P608*Q(I,KOFF+1,J))*APEM1
          TSFCK=T(I,KOFF+1,J)*(1.+P608*Q(I,KOFF+1,J))
	ENDIF
!!!!!          THS(I,J)=T(I,KOFF+1,J)*(1.+P608*Q(I,KOFF+1,J))*APEM1
!               THS(I,J)=NMM_TSK(I,J)*(1.+P608*Q(I,KOFF+1,J))*APEM1
!!          TSFCK=T(I,KOFF+1,J)*(1.+P608*Q(I,KOFF+1,J))
!               TSFCK=NMM_TSK(I,J)*(1.+P608*Q(I,KOFF+1,J))

          PSFCK=PD(I,J)+PDTOP+PT
!
          IF(SM(I,J).LT.0.5) THEN
            QS(I,J)=PQ0/PSFCK*EXP(A2*(TSFCK-A3)/(TSFCK-A4))
          ELSEIF(SM(I,J).GT.0.5) THEN
!reinstated below 1020
            THS(I,J)=SST(I,J)*(1.E5/(PD(I,J)+PDTOP+PT))**CAPA
          ENDIF

	IF (THS(I,J) .lt. 200) then
	write(6,*) 'bad THS in start_domain_nmm: ', I,J,THS(I,J)
	endif
!
          TERM1=-0.068283/T(I,KOFF+1,J)
          PSHLTR(I,J)=(PD(I,J)+PDTOP+PT)*EXP(TERM1)
!
          USTAR(I,J)=0.1
          THZ0(I,J)=THS(I,J)
          QZ0(I,J)=QS(I,J)
          UZ0(I,J)=0.
          VZ0(I,J)=0.
! 
        ENDDO
        ENDDO
!***
!***  INITIALIZE CLOUD FIELDS
!***
        DO J=JFS,JFE
          DO K=KPS,KPE
          DO I=IFS,IFE
            CWM(I,K,J)=0.
          ENDDO
          ENDDO
        ENDDO
!***
!***  INITIALIZE ACCUMULATOR ARRAYS TO ZERO.
!***
        ARDSW=0.0
        ARDLW=0.0
        ASRFC=0.0
        AVRAIN=0.0
        AVCNVC=0.0
!
        DO J=JFS,JFE
        DO I=IFS,IFE
          ACFRCV(I,J)=0.
          NCFRCV(I,J)=0
          ACFRST(I,J)=0.
          NCFRST(I,J)=0
          ACSNOW(I,J)=0.
          ACSNOM(I,J)=0.
          SSROFF(I,J)=0.
          BGROFF(I,J)=0.
          ALWIN(I,J) =0.
          ALWOUT(I,J)=0.
          ALWTOA(I,J)=0.
          ASWIN(I,J) =0.
          ASWOUT(I,J)=0.
          ASWTOA(I,J)=0.
          SFCSHX(I,J)=0.
          SFCLHX(I,J)=0.
          SUBSHX(I,J)=0.
          SNOPCX(I,J)=0.
          SFCUVX(I,J)=0.
          SFCEVP(I,J)=0.
          POTEVP(I,J)=0.
          POTFLX(I,J)=0.
        ENDDO
        ENDDO
!***
!***  INITIALIZE SATURATION SPECIFIC HUMIDITY OVER THE WATER.
!***
        EPS=R_D/R_V
!
        DO J=JFS,JFE
        DO I=IFS,IFE
          IF(SM(I,J).GT.0.5)THEN
            CLOGES =-CM1/SST(I,J)-CM2*ALOG10(SST(I,J))+CM3
            ESE    = 10.**(CLOGES+2.)
            QS(I,J)= SM(I,J)*EPS*ESE/(PD(I,J)+PDTOP+PT-ESE*(1.-EPS))
          ENDIF
        ENDDO
        ENDDO
!***  
!***  INITIALIZE TURBULENT KINETIC ENERGY (TKE) TO A SMALL
!***  VALUE (EPSQ2) ABOVE GROUND.  SET TKE TO ZERO IN THE
!***  THE LOWEST MODEL LAYER.  IN THE LOWEST TWO ATMOSPHERIC
!***  ETA LAYERS SET TKE TO A SMALL VALUE (Q2INI).
!***
        DO J=JFS,JFE
        DO K=KPS,KPE-1
        DO I=IFS,IFE
!!!tlb    Q2(I,K,J)=HTM(I,K+1,J)*HBM2(I,J)*EPSQ2
          Q2(I,K+1,J)=HTM(I,K,J)*HBM2(I,J)*EPSQ2
        ENDDO
        ENDDO
        ENDDO
!
        DO J=JFS,JFE
        DO I=IFS,IFE
          Q2(I,KPS,J)    = 0.
          LLMH          = LMH(I,J)
          Q2(I,KOFF+2,J)= HBM2(I,J)*Q2INI
          Q2(I,KOFF+3,J)= HBM2(I,J)*Q2INI
        ENDDO
        ENDDO
!***  
!***  PAD ABOVE GROUND SPECIFIC HUMIDITY IF IT IS TOO SMALL.
!***  INITIALIZE LATENT HEATING ACCUMULATION ARRAYS.
!***
        DO J=JFS,JFE
        DO K=KPS,KPE
        DO I=IFS,IFE
          IF(Q(I,K,J).LT.EPSQ)Q(I,K,J)=EPSQ*HTM(I,K,J)
          TRAIN(I,K,J)=0.
          TCUCN(I,K,J)=0.
        ENDDO
        ENDDO
        ENDDO
!
!----------------------------------------------------------------------
!***  END OF SCRATCH START INITIALIZATION BLOCK.
!----------------------------------------------------------------------
!
        CALL wrf_message('INIT:  INITIALIZED ARRAYS FOR CLEAN START')
      ENDIF
!
!----------------------------------------------------------------------
!***  RESTART INITIALIZING.  CHECK TO SEE IF WE NEED TO ZERO
!***  ACCUMULATION ARRAYS.
!----------------------------------------------------------------------
!   IF(RESTRT)THEN

      TSPH=3600./DT ! needed?

!!!!  How do we pass in this TSTART information in reality???

      TSTART=00.0
      TPREC=06.0
      THEAT=06.0
      TCLOD=06.0
      TRDSW=03.0
      TRDLW=03.0
      TSRFC=03.0

      NSTART = INT(TSTART*TSPH+0.5)

      NTSD = NSTART  ! (this NTSD value not honored by integrate)


!! want non-zero values for NPREC, NHEAT type vars to avoid problems
!! with mod statements below.

      NPREC  = INT(TPREC *TSPH+0.5)
      NHEAT  = INT(THEAT *TSPH+0.5)
      NCLOD  = INT(TCLOD *TSPH+0.5)
      NRDSW  = INT(TRDSW *TSPH+0.5)
      NRDLW  = INT(TRDLW *TSPH+0.5)
      NSRFC  = INT(TSRFC *TSPH+0.5)

	write(0,*) 'TRDLW, TSPH, NRDLW: ', TRDLW, TSPH, NRDLW

!
!***
!***    AVERAGE CLOUD AMOUNT ARRAY
!***

   IF(RESTRT)THEN    ! believe moving here is more appropriate

        IF(MOD(NTSD,NCLOD).LT.NPHS)THEN
          CALL wrf_message('  ZERO AVG CLD AMT ARRAY')
          DO J=JFS,JFE
          DO I=IFS,IFE
            ACFRCV(I,J)=0.
            NCFRCV(I,J)=0
            ACFRST(I,J)=0.
            NCFRST(I,J)=0
          ENDDO
          ENDDO
        ENDIF
!***  
!***     GRID-SCALE AND CONVECTIVE LATENT HEATING ARRAYS.
!***  
        IF(MOD(NTSD,NHEAT).LT.NCNVC)THEN
          CALL wrf_message('  ZERO ACCUM LATENT HEATING ARRAYS')
!
          AVRAIN=0.
          AVCNVC=0.
          DO J=JFS,JFE
          DO K=KPS,KPE
          DO I=IFS,IFE
            TRAIN(I,K,J)=0.
            TCUCN(I,K,J)=0.
          ENDDO
          ENDDO
          ENDDO
        ENDIF
!***
!***  IF THIS IS NOT A NESTED RUN, INITIALIZE TKE
!***
!       IF(.NOT.NEST)THEN
!         DO K=1,LM
!           DO J=JFS,JFE
!           DO I=IFS,IFE
!             Q2(I,K,J)=AMAX1(Q2(I,K,J)*HBM2(I,J),EPSQ2)
!           ENDDO
!           ENDDO
!         ENDDO
!       ENDIF
!***
!***  CLOUD EFFICIENCY
!***
!       DO J=JFS,JFE
!       DO I=IFS,IFE
!!!       CLDEFI(I,J)=AVGEFI*SM(I,J)+STEFI*(1.-SM(I,J))
!         CLDEFI(I,J)=1.
!       ENDDO
!       ENDDO
!***
!***  TOTAL AND CONVECTIVE PRECIPITATION ARRAYS.
!***  TOTAL SNOW AND SNOW MELT ARRAYS.
!***  STORM SURFACE AND BASE GROUND RUN OFF ARRAYS.
!     
        IF(MOD(NTSD,NPREC).LT.NPHS)THEN
          CALL wrf_message('  ZERO ACCUM PRECIP ARRAYS')
          DO J=JFS,JFE
          DO I=IFS,IFE
            ACPREC(I,J)=0.
            CUPREC(I,J)=0.
            ACSNOW(I,J)=0.
            ACSNOM(I,J)=0.
            SSROFF(I,J)=0.
            BGROFF(I,J)=0.
          ENDDO
          ENDDO
        ENDIF
!***  
!***  LONG WAVE RADIATION ARRAYS.
!***  
        IF(MOD(NTSD,NRDLW).LT.NPHS)THEN
          CALL wrf_message('  ZERO ACCUM LW RADTN ARRAYS')
          ARDLW=0.
          DO J=JFS,JFE
          DO I=IFS,IFE
            ALWIN(I,J) =0.
            ALWOUT(I,J)=0.
            ALWTOA(I,J)=0.
          ENDDO
          ENDDO
        ENDIF
!***  
!***  SHORT WAVE RADIATION ARRAYS.
!***  
        IF(MOD(NTSD,NRDSW).LT.NPHS)THEN
          CALL wrf_message('  ZERO ACCUM SW RADTN ARRAYS')
          ARDSW=0.
          DO J=JFS,JFE
          DO I=IFS,IFE
            ASWIN(I,J) =0.
            ASWOUT(I,J)=0.
            ASWTOA(I,J)=0.
          ENDDO
          ENDDO
        ENDIF
!***  
!***  SURFACE SENSIBLE AND LATENT HEAT FLUX ARRAYS.
!***  
        IF(MOD(NTSD,NSRFC).LT.NPHS)THEN
          CALL wrf_message('  ZERO ACCUM SFC FLUX ARRAYS')
          ASRFC=0.
          DO J=JFS,JFE
          DO I=IFS,IFE
            SFCSHX(I,J)=0.
            SFCLHX(I,J)=0.
            SUBSHX(I,J)=0.
            SNOPCX(I,J)=0.
            SFCUVX(I,J)=0.
            SFCEVP(I,J)=0.
            POTEVP(I,J)=0.
            POTFLX(I,J)=0.
          ENDDO
          ENDDO
        ENDIF
!***
!***  ENDIF FOR RESTART FILE ACCUMULATION ZERO BLOCK.
!***
        CALL wrf_message('INIT:  INITIALIZED ARRAYS FOR RESTART START')
      ENDIF
!
      DO J=JFS,JFE
      DO K=KPS,KPE
      DO I=IFS,IFE
        ZERO_3D(I,K,J)=0.
      ENDDO
      ENDDO
      ENDDO
!----------------------------------------------------------------------
!
!***  FLAG FOR INITIALIZING ARRAYS, LOOKUP TABLES, & CONSTANTS USED IN
!***  MICROPHYSICS AND RADIATION
!
!----------------------------------------------------------------------
!
      MICRO_START=.TRUE.
!
!----------------------------------------------------------------------
!***
!***  INITIALIZE ADVECTION TENDENCIES TO ZERO SO THAT
!***  BOUNDARY POINTS WILL ALWAYS BE ZERO
!***
      DO J=JFS,JFE
      DO K=KPS,KPE
      DO I=IFS,IFE
        ADT(I,K,J)=0.
        ADU(I,K,J)=0.
        ADV(I,K,J)=0.
      ENDDO
      ENDDO
      ENDDO
!----------------------------------------------------------------------
!***
!***  SET INDEX ARRAYS FOR UPSTREAM ADVECTION
!***
!----------------------------------------------------------------------
      DO J=JFS,JFE
        N_IUP_H(J)=0
        N_IUP_V(J)=0
        N_IUP_ADH(J)=0
        N_IUP_ADV(J)=0
!
        DO I=IFS,IFE
          IUP_H(I,J)=-999
          IUP_V(I,J)=-999
          IUP_ADH(I,J)=-999
          IUP_ADV(I,J)=-999
        ENDDO
!
      ENDDO

!
!***  N_IUP_H HOLDS THE NUMBER OF MASS POINTS NEEDED IN EACH ROW
!***  FOR UPSTREAM ADVECTION (FULL ROWS IN THE 3RD THROUGH 7TH
!***  ROWS FROM THE SOUTH AND NORTH GLOBAL BOUNDARIES AND 
!***  FOUR POINTS ADJACENT TO THE WEST AND EAST GLOBAL BOUNDARIES
!***  ON ALL OTHER INTERNAL ROWS).  SIMILARLY FOR N_IUP_V.
!***  BECAUSE OF HORIZONTAL OPERATIONS, THESE POINTS EXTEND OUTSIDE 
!***  OF THE UPSTREAM REGION SOMEWHAT.
!***  N_IUP_ADH HOLDS THE NUMBER OF MASS POINTS NEEDED IN EACH ROW
!***  FOR THE COMPUTATION OF THE TENDENCIES THEMSELVES (ADT, ADQ2M
!***  AND ADQ2L); SPECIFICALLY THESE TENDENCIES ARE ONLY DONE IN
!***  THE UPSTREAM REGION.
!***  N_IUP_ADV HOLDS THE NUMBER OF MASS POINTS NEEDED IN EACH ROW
!***  FOR THE VELOCITY POINT TENDENCIES.
!***  IUP_H AND IUP_V HOLD THE ACTUAL I VALUES USED IN EACH ROW.
!***  LIKEWISE FOR IUP_ADH AND IUP_ADV. 
!***  ALSO, SET UPSTRM FOR THOSE TASKS AROUND THE GLOBAL EDGE.
!
      UPSTRM=.FALSE.
!
      S_BDY=(JPS==JDS)
      N_BDY=(JPE==JDE)
      W_BDY=(IPS==IDS)
      E_BDY=(IPE==IDE)
!
      JTPAD2=2
      JBPAD2=2
      IRPAD2=2
      ILPAD2=2
!
      IF(S_BDY)THEN
        UPSTRM=.TRUE.
        JBPAD2=0
!
        DO JJ=1,7
          J=JJ      ! -MY_JS_GLB+1
          KNTI=0
          DO I=MYIS_P2,MYIE_P2
            IUP_H(IMS+KNTI,J)=I
            IUP_V(IMS+KNTI,J)=I
            KNTI=KNTI+1
          ENDDO
          N_IUP_H(J)=KNTI
          N_IUP_V(J)=KNTI
        ENDDO
!
        DO JJ=3,5
          J=JJ      ! -MY_JS_GLB+1
          KNTI=0
          ISTART=MYIS1_P2
          IEND=MYIE1_P2
          IF(E_BDY)IEND=IEND-MOD(JJ+1,2)
          DO I=ISTART,IEND
            IUP_ADH(IMS+KNTI,J)=I
            KNTI=KNTI+1
          ENDDO
          N_IUP_ADH(J)=KNTI
!
          KNTI=0
          ISTART=MYIS1_P2
          IEND=MYIE1_P2
          IF(E_BDY)IEND=IEND-MOD(JJ,2)
          DO I=ISTART,IEND
            IUP_ADV(IMS+KNTI,J)=I
            KNTI=KNTI+1
          ENDDO
          N_IUP_ADV(J)=KNTI
        ENDDO
      ENDIF
!
      IF(N_BDY)THEN
        UPSTRM=.TRUE.
        JTPAD2=0
!
        DO JJ=JDE-7, JDE-1 ! JM-6,JM
          J=JJ      ! -MY_JS_GLB+1
          KNTI=0
          DO I=MYIS_P2,MYIE_P2
            IUP_H(IMS+KNTI,J)=I
            IUP_V(IMS+KNTI,J)=I
            KNTI=KNTI+1
          ENDDO
          N_IUP_H(J)=KNTI
          N_IUP_V(J)=KNTI
        ENDDO
!
        DO JJ=JDE-5, JDE-3 ! JM-4,JM-2
          J=JJ      ! -MY_JS_GLB+1
          KNTI=0
          ISTART=MYIS1_P2
          IEND=MYIE1_P2
          IF(E_BDY)IEND=IEND-MOD(JJ+1,2)
          DO I=ISTART,IEND
            IUP_ADH(IMS+KNTI,J)=I
            KNTI=KNTI+1
          ENDDO
          N_IUP_ADH(J)=KNTI
!
          KNTI=0
          ISTART=MYIS1_P2
          IEND=MYIE1_P2
          IF(E_BDY)IEND=IEND-MOD(JJ,2)
          DO I=ISTART,IEND
            IUP_ADV(IMS+KNTI,J)=I
            KNTI=KNTI+1
          ENDDO
          N_IUP_ADV(J)=KNTI
        ENDDO
      ENDIF
!
      IF(W_BDY)THEN
        UPSTRM=.TRUE.
        ILPAD2=0
        DO JJ=8,JDE-8   ! JM-7
          IF(JJ.GE.MY_JS_GLB-2.AND.JJ.LE.MY_JE_GLB+2)THEN
            J=JJ      ! -MY_JS_GLB+1
!
            DO I=1,4
              IUP_H(IMS+I-1,J)=I
              IUP_V(IMS+I-1,J)=I
            ENDDO
            N_IUP_H(J)=4
            N_IUP_V(J)=4
          ENDIF
        ENDDO
!
        DO JJ=6,JDE-6   ! JM-5
          IF(JJ.GE.MY_JS_GLB-2.AND.JJ.LE.MY_JE_GLB+2)THEN
            J=JJ      ! -MY_JS_GLB+1
            KNTI=0
            IEND=2+MOD(JJ,2)
            DO I=2,IEND
              IUP_ADH(IMS+KNTI,J)=I
              KNTI=KNTI+1
            ENDDO
            N_IUP_ADH(J)=KNTI
!
            KNTI=0
            IEND=2+MOD(JJ+1,2)
            DO I=2,IEND
              IUP_ADV(IMS+KNTI,J)=I
              KNTI=KNTI+1
            ENDDO
            N_IUP_ADV(J)=KNTI
!
          ENDIF
        ENDDO
      ENDIF
!
      CALL WRF_GET_NPROCX(INPES)
!
      IF(E_BDY)THEN
        UPSTRM=.TRUE.
        IRPAD2=0
        DO JJ=8,JDE-8   ! JM-7
          IF(JJ.GE.MY_JS_GLB-2.AND.JJ.LE.MY_JE_GLB+2)THEN
            J=JJ      ! -MY_JS_GLB+1
            IEND=IM-MOD(JJ+1,2)
            ISTART=IEND-3
!
!***  IN CASE THERE IS ONLY A SINGLE GLOBAL TASK IN THE
!***  I DIRECTION THEN WE MUST ADD THE WESTSIDE UPSTREAM
!***  POINTS TO THE EASTSIDE POINTS IN EACH ROW.
!
            KNTI=0
            IF(INPES.EQ.1)KNTI=N_IUP_H(J)
!
            DO II=ISTART,IEND
              I=II      ! -MY_IS_GLB+1
              IUP_H(IMS+KNTI,J)=I
              KNTI=KNTI+1
            ENDDO
            N_IUP_H(J)=KNTI
          ENDIF
        ENDDO
!
        DO JJ=6,JDE-6   ! JM-5
          IF(JJ.GE.MY_JS_GLB-2.AND.JJ.LE.MY_JE_GLB+2)THEN
            J=JJ      ! -MY_JS_GLB+1
            IEND=IM-1-MOD(JJ+1,2)
            ISTART=IEND-MOD(JJ,2)
            KNTI=0
            IF(INPES.EQ.1)KNTI=N_IUP_ADH(J)
            DO II=ISTART,IEND
              I=II      ! -MY_IS_GLB+1
              IUP_ADH(IMS+KNTI,J)=I
              KNTI=KNTI+1
            ENDDO
            N_IUP_ADH(J)=KNTI
          ENDIF
        ENDDO
!***
        DO JJ=8,JDE-8  ! JM-7
          IF(JJ.GE.MY_JS_GLB-2.AND.JJ.LE.MY_JE_GLB+2)THEN
            J=JJ      ! -MY_JS_GLB+1
            IEND=IM-MOD(JJ,2)
            ISTART=IEND-3
            KNTI=0
            IF(INPES.EQ.1)KNTI=N_IUP_V(J)
!
            DO II=ISTART,IEND
              I=II      ! -MY_IS_GLB+1
              IUP_V(IMS+KNTI,J)=I
              KNTI=KNTI+1
            ENDDO
            N_IUP_V(J)=KNTI
          ENDIF
        ENDDO
!
        DO JJ=6,JDE-6  !  JM-5
          IF(JJ.GE.MY_JS_GLB-2.AND.JJ.LE.MY_JE_GLB+2)THEN
            J=JJ      ! -MY_JS_GLB+1
            IEND=IM-1-MOD(JJ,2)
            ISTART=IEND-MOD(JJ+1,2)
            KNTI=0
            IF(INPES.EQ.1)KNTI=N_IUP_ADV(J)
            DO II=ISTART,IEND
              I=II      ! -MY_IS_GLB+1
              IUP_ADV(IMS+KNTI,J)=I
              KNTI=KNTI+1
            ENDDO
            N_IUP_ADV(J)=KNTI
          ENDIF
        ENDDO
      ENDIF
!----------------------------------------------------------------------
!!!!!!!!!!!!!!!!!!!!tlb
!!!Read in EM and EMT from the original NMM nhb file
!!!   call int_get_fresh_handle( retval )
!!!   close(retval)
!!!   open(unit=retval,file=seeout,form=UNFORMATTED,iostat=ier)
!!!!!!do j=1,128
!!!     read(seeout)
!!!!!!  read(55)
!!!!!!enddo
!!!   read(seeout)dummyx,em,emt
!!!!!!read(55)dummyx,em,emt
!!!   close(retval)
      jam=6+2*(JDE-JDS-1-9)
!     read(55)(em(j),j=1,jam),(emt(j),j=1,jam)
!!!!!!!!!!!!!!!!!!!!tlb
!
!***  EXTRACT EM AND EMT FOR THE LOCAL SUBDOMAINS
!
      DO J=MYJS_P5,MYJE_P5
        EM_LOC(J)=-9.E9
        EMT_LOC(J)=-9.E9
      ENDDO
!!!   IF(IBROW==1)THEN
      IF(S_BDY)THEN
        DO J=3,5
          EM_LOC(J)=EM(J-2)
          EMT_LOC(J)=EMT(J-2)
        ENDDO
      ENDIF
!!!   IF(ITROW==1)THEN
      IF(N_BDY)THEN
        KNT=3
        DO JJ=JDE-5,JDE-3 ! JM-4,JM-2
          KNT=KNT+1
          J=JJ      ! -MY_JS_GLB+1
          EM_LOC(J)=EM(KNT)
          EMT_LOC(J)=EMT(KNT)
        ENDDO
      ENDIF
!!!   IF(ILCOL==1)THEN
      IF(W_BDY)THEN
        KNT=6
        DO JJ=6,JDE-6 ! JM-5
          KNT=KNT+1
          IF(JJ.GE.MY_JS_GLB-2.AND.JJ.LE.MY_JE_GLB+2)THEN
            J=JJ      ! -MY_JS_GLB+1
            EM_LOC(J)=EM(KNT)
            EMT_LOC(J)=EMT(KNT)
          ENDIF
        ENDDO
      ENDIF
!!!   IF(IRCOL==1)THEN
      IF(E_BDY)THEN
        KNT=6+JDE-11 ! JM-10
        DO JJ=6,JDE-6 ! JM-5
          KNT=KNT+1
          IF(JJ.GE.MY_JS_GLB-2.AND.JJ.LE.MY_JE_GLB+2)THEN
            J=JJ      ! -MY_JS_GLB+1
            EM_LOC(J)=EM(KNT)
            EMT_LOC(J)=EMT(KNT)
          ENDIF
        ENDDO
      ENDIF
!
!***
!*** SET ZERO-VALUE FOR SOME OUTPUT DIAGNOSTIC ARRAYS
!***
!     if(mype.eq.n_t)then
!       write(0,*) nstart=,nstart, sm=,sm(i_t,j_t) &
!    &,            sice=,sice(i_t,j_t)
!       do ns=1,nsoil
!         write(0,*) START sh2o=,sh2o(i_t,ns,j_t)
!       enddo
!     endif
      IF(NSTART.EQ.0)THEN
!
        DO J=JFS,JFE
        DO I=IFS,IFE
          PCTSNO(I,J)=-999.0
          IF(SM(I,J).LT.0.5)THEN
            IF(SICE(I,J).GT.0.5)THEN
!***
!***  SEA-ICE CASE
!***
              SMSTAV(I,J)=1.0
              SMSTOT(I,J)=1.0
              SSROFF(I,J)=0.0
              BGROFF(I,J)=0.0
              CMC(I,J)=0.0
              DO NS=1,NSOIL
                SMC(I,NS,J)=1.0
!               SH2O(I,NS,J)=0.05
                SH2O(I,NS,J)=1.0
              ENDDO
            ENDIF
          ELSE
!***
!***  WATER CASE
!***
            SMSTAV(I,J)=1.0
            SMSTOT(I,J)=1.0
            SSROFF(I,J)=0.0
            BGROFF(I,J)=0.0
            SOILTB(I,J)=273.16
            GRNFLX(I,J)=0.
            SUBSHX(I,J)=0.0
            ACSNOW(I,J)=0.0
            ACSNOM(I,J)=0.0
            SNOPCX(I,J)=0.0
            CMC(I,J)=0.0
            SNO(I,J)=0.0
            DO NS=1,NSOIL
              SMC(I,NS,J)=1.0
              STC(I,NS,J)=273.16
!             SH2O(I,NS,J)=0.05
              SH2O(I,NS,J)=1.0
            ENDDO
          ENDIF
!
        ENDDO
        ENDDO
!
        APHTIM=0.0
        ARATIM=0.0
        ACUTIM=0.0
!
      ENDIF
!
!----------------------------------------------------------------------
!***  INITIALIZE RADTN VARIABLES
!***  CALCULATE THE NUMBER OF STEPS AT EACH POINT.
!***  THE ARRAY LVL WILL COORDINATE VERTICAL LOCATIONS BETWEEN
!***  THE LIFTED WORKING ARRAYS AND THE FUNDAMENTAL MODEL ARRAYS.
!***  LVL HOLDS THE HEIGHT (IN MODEL LAYERS) OF THE TOPOGRAPHY AT
!***  EACH GRID POINT.
!----------------------------------------------------------------------
!   
      DO J=JFS,JFE
      DO I=IFS,IFE
        LVL(I,J)=LM-LMH(I,J)
      ENDDO
      ENDDO
!***
!***  DETERMINE MODEL LAYER LIMITS FOR HIGH(3), MIDDLE(2),
!***  AND LOW(1) CLOUDS.  ALSO FIND MODEL LAYER THAT IS JUST BELOW
!***  (HEIGHT-WISE) 400 MB. (K400)
!*** 
      K400=0
      PSUM=PT
      SLPM=101325.
      PDIF=SLPM-PT
      DO K=1,LM
        PSUM=PSUM+DETA(K)*PDIF
        IF(LPTOP(3).EQ.0)THEN
          IF(PSUM.GT.PHITP)LPTOP(3)=K
        ELSEIF(LPTOP(2).EQ.0)THEN
          IF(PSUM.GT.PMDHI)LPTOP(2)=K
        ELSEIF(K400.EQ.0)THEN
          IF(PSUM.GT.P400)K400=K
        ELSEIF(LPTOP(1).EQ.0)THEN
          IF(PSUM.GT.PLOMD)LPTOP(1)=K
        ENDIF
      ENDDO
!***
!*** CALL GRADFS ONCE TO CALC. CONSTANTS AND GET O3 DATA
!***
      KCCO2=0
!***
!*** CALCULATE THE MIDLAYER PRESSURES IN THE STANDARD ATMOSPHERE
!***
      PSS=101325.
      PDIF=PSS-PT
!
      ALLOCATE(PHALF(LM+1),STAT=I)
!
      DO K=KPS,KPE-1
        PHALF(K+1)=AETA(K)*PDIF+PT
      ENDDO
      
!
      PHALF(1)=0.
      PHALF(LM+1)=PSS
!***
!!!   CALL GRADFS(PHALF,KCCO2,NUNIT_CO2)
!***
!***  CALL SOLARD TO CALCULATE NON-DIMENSIONAL SUN-EARTH DISTANCE
!***
!!!   IF(MYPE.EQ.0)CALL SOLARD(SUN_DIST)
!!!   CALL MPI_BCAST(SUN_DIST,1,MPI_REAL,0,MPI_COMM_COMP,IRTN)

!***
!***  CALL ZENITH SIMPLY TO GET THE DAY OF THE YEAR FOR
!***  THE SETUP OF THE OZONE DATA
!***
      TIME=(NTSD-1)*DT
!
!!!   CALL ZENITH(TIME,DAYI,HOUR)
!
      ADDL=0.
      IF(MOD(IDAT(3),4).EQ.0)ADDL=1.
!
!!!   CALL O3CLIM
!
!
      DEALLOCATE(PHALF)
!----------------------------------------------------------------------
!***  SOME INITIAL VALUES RELATED TO TURBULENCE SCHEME
!----------------------------------------------------------------------
!
      DO J=JFS,JFE
      DO I=IFS,IFE
!***
!***  TRY A SIMPLE LINEAR INTERP TO GET 2/10 M VALUES
!***
        PDSL(I,J)=PD(I,J)*RES(I,J)
        LMHK=LMH(I,J)
        LMVK=LMV(I,J)
!
        KOFF=KPE-1-LMHK
        KOFV=KPE-1-LMVK
!
        ULM=U(I,KOFV+1,J)
        VLM=V(I,KOFV+1,J)
        TLM=T(I,KOFF+1,J)
        QLM=Q(I,KOFF+1,J)
        PLM=AETA1(KOFF+1)*PDTOP+AETA2(KOFF+1)*PDSL(I,J)+PT
        APELM=(1.0E5/PLM)**CAPA
        APELMNW=(1.0E5/PSHLTR(I,J))**CAPA
        THLM=TLM*APELM
        DPLM=(DETA1(KOFF+1)*PDTOP+DETA2(KOFF+1)*PDSL(I,J))*0.5
        DZLM=R_D*DPLM*TLM*(1.+P608*QLM)/(G*PLM)
        FAC1=10./DZLM
        FAC2=(DZLM-10.)/DZLM
        IF(DZLM.LE.10.)THEN
          FAC1=1.
          FAC2=0.
        ENDIF
!
        IF(.NOT.RESTRT)THEN
          TH10(I,J)=FAC2*THS(I,J)+FAC1*THLM
          Q10(I,J)=FAC2*QS(I,J)+FAC1*QLM
          U10(I,J)=ULM
          V10(I,J)=VLM
        ENDIF
!
        FAC1=2./DZLM
        FAC2=(DZLM-2.)/DZLM
        IF(DZLM.LE.2.)THEN
          FAC1=1.
          FAC2=0.
        ENDIF
!
        IF(.NOT.RESTRT.OR.NEST)THEN
!mp          TSHLTR(I,J)=FAC2*THS(I,J)+FAC1*THLM
          TSHLTR(I,J)=0.2*THS(I,J)+0.8*THLM
!mp          QSHLTR(I,J)=FAC2*QS(I,J)+FAC1*QLM
          QSHLTR(I,J)=0.2*QS(I,J)+0.8*QLM
!     if(i.eq.10.and.j.eq.3)then
!       write(0,12541)qshltr(i,j)
!       write(0,12542)qs(i,j),qlm,fac1,fac2
12541   format(' qshltr=',z8)
12542   format(' qs=',z8,' qlm=',z8,' fac1=',z8,' fac2=',z8)
!     endif
        ENDIF
!***
!***  NEED TO CONVERT TO THETA IF IS THE RESTART CASE
!***  AS CHKOUT.f WILL CONVERT TO TEMPERATURE
!***
        IF(RESTRT)THEN
          TSHLTR(I,J)=TSHLTR(I,J)*APELMNW
        ENDIF
      ENDDO
      ENDDO
!
!----------------------------------------------------------------------
!***  INITIALIZE TAU-1 VALUES FOR ADAMS-BASHFORTH 
!----------------------------------------------------------------------
!
      DO J=jfs,jfe
        DO K=KPS,KPE
        DO I=ifs,ife
          TOLD(I,K,J)=T(I,K,J)   ! T AT TAU-1
          UOLD(I,K,J)=U(I,K,J)   ! U AT TAU-1
          VOLD(I,K,J)=V(I,K,J)   ! V AT TAU-1
        ENDDO
        ENDDO
      ENDDO
!
!----------------------------------------------------------------------
!***  INITIALIZE NONHYDROSTATIC QUANTITIES
!----------------------------------------------------------------------
!
!!!!	SHOULD DWDT BE REDEFINED IF RESTRT?

        IF(.NOT.RESTRT.OR.NEST)THEN
      DO J=jfs,jfe
        DO K=KPS,KPE
        DO I=ifs,ife
          DWDT(I,K,J)=1.
        ENDDO
        ENDDO
      ENDDO
	ENDIF
!***
      IF(SIGMA.EQ.1)THEN
        DO J=jfs,jfe
        DO I=ifs,ife
          PDSL(I,J)=PD(I,J)
        ENDDO
        ENDDO
      ELSE
        DO J=jfs,jfe
        DO I=ifs,ife
          PDSL(I,J)=RES(I,J)*PD(I,J)
        ENDDO
        ENDDO
      ENDIF
!
!***
!
!
!!!!	SHOULD PINT,Z,W BE REDEFINED IF RESTRT?

      write(0,*)' restrt=',restrt,' nest=',nest
      write(0,*)' ifs=',ifs,' ife=',ife
      write(0,*)' jfs=',jfs,' jfe=',jfe
      write(0,*)' kps=',kps,' kpe=',kpe
        IF(.NOT.RESTRT.OR.NEST)THEN
      DO J=jfs,jfe
        DO K=KPS,KPE
        DO I=ifs,ife
          PINT(I,K,J)=ETA1(K)*PDTOP+ETA2(K)*PDSL(I,J)+PT
          Z(I,K,J)=PINT(I,K,J)
          W(I,K,J)=0.
        ENDDO
        ENDDO
      ENDDO
	ENDIF

!----------------------------------------------------------------------
!***  RESTRICTING THE ACCELERATION ALONG THE BOUNDARIES
!----------------------------------------------------------------------
!
      DO J=jfs,jfe
      DO I=ifs,ife
        DWDTMN(I,J)=-EPSIN
        DWDTMX(I,J)= EPSIN
      ENDDO
      ENDDO


!
!***
      IF(JHL.GT.1)THEN
        JHH=JDE-1-JHL+1 ! JM-JHL+1
        IHL=JHL/2+1
!
        DO J=1,JHL
          IF(J.GE.MY_JS_GLB-JBPAD2.AND.J.LE.MY_JE_GLB+JTPAD2)THEN
            JX=J      ! -MY_JS_GLB+1
            DO I=1,IDE-1 ! IM
              IF(I.GE.MY_IS_GLB-ILPAD2.AND.I.LE.MY_IE_GLB+IRPAD2)THEN
                IX=I      ! -MY_IS_GLB+1
                DWDTMN(IX,JX)=-EPSB
                DWDTMX(IX,JX)= EPSB
              ENDIF
            ENDDO
          ENDIF
        ENDDO
!
        DO J=JHH,JDE-1   ! JM
          IF(J.GE.MY_JS_GLB-JBPAD2.AND.J.LE.MY_JE_GLB+JTPAD2)THEN
            JX=J      ! -MY_JS_GLB+1
            DO I=1,IDE-1 ! IM
              IF(I.GE.MY_IS_GLB-ILPAD2.AND.I.LE.MY_IE_GLB+IRPAD2)THEN
                IX=I      ! -MY_IS_GLB+1
                DWDTMN(IX,JX)=-EPSB
                DWDTMX(IX,JX)= EPSB
              ENDIF
            ENDDO
          ENDIF
        ENDDO
!
        DO J=1,JDE-1 ! JM
          IF(J.GE.MY_JS_GLB-JBPAD2.AND.J.LE.MY_JE_GLB+JTPAD2)THEN
            JX=J      ! -MY_JS_GLB+1
            DO I=1,IHL
              IF(I.GE.MY_IS_GLB-ILPAD2.AND.I.LE.MY_IE_GLB+IRPAD2)THEN
                IX=I      ! -MY_IS_GLB+1
                DWDTMN(IX,JX)=-EPSB
                DWDTMX(IX,JX)= EPSB
              ENDIF
            ENDDO
          ENDIF
        ENDDO
!
        DO J=1,JDE-1 ! JM
          IF(J.GE.MY_JS_GLB-JBPAD2.AND.J.LE.MY_JE_GLB+JTPAD2)THEN
            JX=J      ! -MY_JS_GLB+1
             ! moved this line to inside the J-loop, 20030429, jm
            IHH=IDE-1-IHL+MOD(j,2) ! IM-IHL+MOD(J,2)
            DO I=IHH,IDE-1 ! IM
              IF(I.GE.MY_IS_GLB-ILPAD2.AND.I.LE.MY_IE_GLB+IRPAD2)THEN
                IX=I      ! -MY_IS_GLB+1
                DWDTMN(IX,JX)=-EPSB
                DWDTMX(IX,JX)= EPSB
              ENDIF
            ENDDO
          ENDIF
        ENDDO
!
      ENDIF


!-----------------------------------------------------------------------
!***  CALL THE GENERAL PHYSICS INITIALIZATION
!-----------------------------------------------------------------------
!

      ALLOCATE(SFULL(KMS:KME),STAT=I)
      ALLOCATE(SMID(KMS:KME),STAT=I)
      ALLOCATE(EMISS(IMS:IME,JMS:JME),STAT=I)
      ALLOCATE(GLW(IMS:IME,JMS:JME),STAT=I)
      ALLOCATE(GSW(IMS:IME,JMS:JME),STAT=I)
      ALLOCATE(HFX(IMS:IME,JMS:JME),STAT=I)
      ALLOCATE(LOWLYR(IMS:IME,JMS:JME),STAT=I)
      ALLOCATE(LU_INDEX(IMS:IME,JMS:JME),STAT=I)
      ALLOCATE(MAVAIL(IMS:IME,JMS:JME),STAT=I)
      ALLOCATE(NCA(IMS:IME,JMS:JME),STAT=I)
      ALLOCATE(PBLH(IMS:IME,JMS:JME),STAT=I)
      ALLOCATE(QFX(IMS:IME,JMS:JME),STAT=I)
      ALLOCATE(RAINBL(IMS:IME,JMS:JME),STAT=I)
      ALLOCATE(RAINC(IMS:IME,JMS:JME),STAT=I)
      ALLOCATE(RAINCV(IMS:IME,JMS:JME),STAT=I)
      ALLOCATE(RAINNC(IMS:IME,JMS:JME),STAT=I)
      ALLOCATE(RAINNCV(IMS:IME,JMS:JME),STAT=I)

      ALLOCATE(ZS(KMS:KME),STAT=I)
      ALLOCATE(SNOWC(IMS:IME,JMS:JME),STAT=I)
      ALLOCATE(THC(IMS:IME,JMS:JME),STAT=I)
      ALLOCATE(TMN(IMS:IME,JMS:JME),STAT=I)
      ALLOCATE(TSFC(IMS:IME,JMS:JME),STAT=I)
      ALLOCATE(XLAND(IMS:IME,JMS:JME),STAT=I)
      ALLOCATE(XLAT(IMS:IME,JMS:JME),STAT=I)
      ALLOCATE(XLONG(IMS:IME,JMS:JME),STAT=I)
      ALLOCATE(Z0_DUM(IMS:IME,JMS:JME),STAT=I)

!state    real   DZS             l        dyn_em      -         Z     ir 
      ALLOCATE(DZS(KMS:KME),STAT=I)
!state    real  CLDFRA          ikj      dyn_em        1         -      r
      ALLOCATE(CLDFRA(IMS:IME,KMS:KME, JMS:JME),STAT=I)
!state    real  RQCBLTEN        ikj      dyn_em        1         -      r
      ALLOCATE(RQCBLTEN(IMS:IME,KMS:KME,JMS:JME),STAT=I)
!state    real  RQIBLTEN        ikj      dyn_em        1         -      r
      ALLOCATE(RQIBLTEN(IMS:IME,KMS:KME,JMS:JME),STAT=I)
!state    real  RQVBLTEN        ikj      dyn_em        1         -      r
      ALLOCATE(RQVBLTEN(IMS:IME,KMS:KME,JMS:JME),STAT=I)
!state    real  RTHBLTEN        ikj      dyn_em        1         -      r
      ALLOCATE(RTHBLTEN(IMS:IME,KMS:KME,JMS:JME),STAT=I)
!state    real  RUBLTEN         ikj      dyn_em        1         -      r
      ALLOCATE(RUBLTEN(IMS:IME,KMS:KME,JMS:JME),STAT=I)
!state    real  RVBLTEN         ikj      dyn_em        1         -      r
      ALLOCATE(RVBLTEN(IMS:IME,KMS:KME,JMS:JME),STAT=I)
!state    real  RQCCUTEN        ikj      dyn_em        1         -      r
      ALLOCATE(RQCCUTEN(IMS:IME,KMS:KME,JMS:JME),STAT=I)
!state    real  RQICUTEN        ikj      dyn_em        1         -      r
      ALLOCATE(RQICUTEN(IMS:IME,KMS:KME,JMS:JME),STAT=I)
!state    real  RQRCUTEN        ikj      dyn_em        1         -      r
      ALLOCATE(RQRCUTEN(IMS:IME,KMS:KME,JMS:JME),STAT=I)
!state    real  RQSCUTEN        ikj      dyn_em        1         -      r
      ALLOCATE(RQSCUTEN(IMS:IME,KMS:KME,JMS:JME),STAT=I)
!state    real  RQVCUTEN        ikj      dyn_em        1         -      r
      ALLOCATE(RQVCUTEN(IMS:IME,KMS:KME,JMS:JME),STAT=I)
!state    real  RTHCUTEN        ikj      dyn_em        1         -      r
      ALLOCATE(RTHCUTEN(IMS:IME,KMS:KME,JMS:JME),STAT=I)
!state    real  RTHRATEN        ikj      dyn_em        1         -      r
      ALLOCATE(RTHRATEN(IMS:IME,KMS:KME,JMS:JME),STAT=I)
!state    real  RTHRATENLW      ikj      dyn_em        1         -      r
      ALLOCATE(RTHRATENLW(IMS:IME,KMS:KME,JMS:JME),STAT=I)
!state    real  RTHRATENSW      ikj      dyn_em        1         -      r
      ALLOCATE(RTHRATENSW(IMS:IME,KMS:KME,JMS:JME),STAT=I)
!state    real   TSLB           ilj       dyn_em      1         Z     irh
      ALLOCATE(TSLB(IMS:IME,KMS:KME,JMS:JME),STAT=I)
!state    real   ZS              l        dyn_em      -         Z     ir 

!-----------------------------------------------------------------------
!jm added set of g_inv
      G_INV=1./G
      ROG=R_D*G_INV
      RADT=NRADS/60.
      BLDT=NPHS*DT/60.
      CUDT=NCNVC*DT/60.
      GSMDT=NPHS*DT/60.
!
      write(0,*)' kte=',kte
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        SFCZ=FIS(I,J)*G_INV
!!!!    ZINT(I,KTS,J)=SFCZ
        PDSL(I,J)=PD(I,J)*RES(I,J)
        PSFC=PINT(I,KTS,J)
        EXNSFC=(1.E5/PSFC)**CAPA
        XLAND(I,J)=SM(I,J)+1.
        THSIJ=(SST(I,J)*EXNSFC)*(XLAND(I,J)-1.)                         &
     &        +THS(I,J)*(2.-SM(I,J))
        TSFC(I,J)=THSIJ/EXNSFC
!
        DO K=KTS,KTE-1
          PLYR=(PINT(I,K,J)+PINT(I,K+1,J))*0.5
          TL=T(I,K,J)
          CWML=CWM(I,K,J)
!!!       ZINT(I,K+1,J)=ZINT(I,K,J)+TL/PLYR                             &
!!!  &                  *(DETA1(K)*PDTOP+DETA2(K)*PDSL(I,J))*ROG        &
!!!  &                  *(Q(I,K,J)*P608-CWML+1.)
        ENDDO
!
        DO K=KTS,KTE
!!!       ZMID(I,K,J)=0.5*(ZINT(I,K,J)+ZINT(I,K+1,J))
        ENDDO
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  RECREATE SIGMA VALUES AT LAYER INTERFACES FOR THE FULL VERTICAL
!***  DOMAIN FROM THICKNESS VALUES FOR THE TWO SUBDOMAINS.
!***  NOTE: KTE=NUMBER OF LAYERS PLUS ONE
!-----------------------------------------------------------------------
!
      PDTOT=101325.-PT
      RPDTOT=1./PDTOT
      PDBOT=PDTOT-PDTOP
      SFULL(KTS)=1.
      SFULL(KTE)=0.
      DO K=KTS+1,KTE
        DSIG=(DETA1(K-1)*PDTOP+DETA2(K-1)*PDBOT)*RPDTOT
        dsigsum=dsigsum+dsig
        SFULL(K)=SFULL(K-1)-DSIG
        SMID(K-1)=0.5*(SFULL(K-1)+SFULL(K))
      ENDDO
      dsig=(deta1(kte-1)*pdtop+deta2(kte-1)*pdbot)*rpdtot
      dsigsum=dsigsum+dsig
      SMID(KTE-1)=0.5*(SFULL(KTE-1)+SFULL(KTE))
!
!-----------------------------------------------------------------------

       LU_INDEX=IVGTYP

      CALL PHY_INIT(GRID,GRID%ID,CONFIG_FLAGS,DT,sfull,smid             &
     &             ,PT,TSFC,RADT,BLDT,CUDT,GSMDT                        &
     &             ,RTHCUTEN, RQVCUTEN, RQRCUTEN                        &
     &             ,RQCCUTEN, RQSCUTEN, RQICUTEN                        &
     &             ,RUBLTEN,RVBLTEN,RTHBLTEN                            &
     &             ,RQVBLTEN,RQCBLTEN,RQIBLTEN                          &
     &             ,RTHRATEN,RTHRATENLW,RTHRATENSW                      &
     &             ,STEPBL,STEPRA,STEPCU                                &
     &             ,W0AVG, RAINNC, RAINC, RAINCV, RAINNCV               &
     &             ,NCA                                                 &
     &             ,CLDEFI,LOWLYR                                       &
     &             ,MASS_FLUX                                           &
     &             ,RTHFTEN, RQVFTEN                                    &
     &             ,CLDFRA,GLW,GSW,EMISS,LU_INDEX                       &
     &             ,XLAT,XLONG,ALBEDO,ALBBCK,GMT,JULYR,JULDAY           &
     &             ,TMN,XLAND,ZNT,Z0,USTAR,MOL,PBLH,TKE_MYJ             &
     &             ,THC,SNOWC,MAVAIL,HFX,QFX,RAINBL                     &
     &             ,TSLB,ZS,DZS,num_soil_layers,warm_rain               &
     &             ,APR_GR,APR_W,APR_MC,APR_ST,APR_AS                   &
     &             ,APR_CAPMA,APR_CAPME,APR_CAPMI                       &
     &             ,XICE,VEGFRA,SNOW,CANWAT,SMSTAV                      &
     &             ,SMSTOT, SFCRUNOFF,UDRUNOFF,GRDFLX,ACSNOW            &
     &             ,ACSNOM,IVGTYP,ISLTYP,SFCEVP,SMOIS                   &
     &             ,SH2O, SNOWH, SMFR3D                                 &  ! temporary
     &             ,DX,DY,F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY              &
     &             ,ids, ide, jds, jde, kds, kde                        &
     &             ,ims, ime, jms, jme, kms, kme                        &
     &             ,its, ite, jts, jte, kts, kte)
!-----------------------------------------------------------------------
!
      DO J=JMS,JME
      DO I=IMS,IME
        APREC(I,J)=RAINNC(I,J)*1.E-3
        CUPREC(I,J)=RAINCV(I,J)*1.E-3
      ENDDO
      ENDDO
!
      DEALLOCATE(SFULL)
      DEALLOCATE(SMID)
      DEALLOCATE(CLDFRA)
      DEALLOCATE(DZS)
      DEALLOCATE(EMISS)
      DEALLOCATE(GLW)
      DEALLOCATE(GSW)
      DEALLOCATE(HFX)
      DEALLOCATE(LOWLYR)
      DEALLOCATE(LU_INDEX)
      DEALLOCATE(MAVAIL)
      DEALLOCATE(NCA)
      DEALLOCATE(PBLH)
      DEALLOCATE(QFX)
      DEALLOCATE(RAINBL)
      DEALLOCATE(RAINC)
      DEALLOCATE(RAINCV)
      DEALLOCATE(RAINNC)
      DEALLOCATE(RAINNCV)
      DEALLOCATE(RQCBLTEN)
      DEALLOCATE(RQIBLTEN)
      DEALLOCATE(RQVBLTEN)
      DEALLOCATE(RTHBLTEN)
      DEALLOCATE(RUBLTEN)
      DEALLOCATE(RVBLTEN)
      DEALLOCATE(RQCCUTEN)
      DEALLOCATE(RQICUTEN)
      DEALLOCATE(RQRCUTEN)
      DEALLOCATE(RQSCUTEN)
      DEALLOCATE(RQVCUTEN)
      DEALLOCATE(RTHCUTEN)
      DEALLOCATE(RTHRATEN)
      DEALLOCATE(RTHRATENLW)
      DEALLOCATE(RTHRATENSW)
      DEALLOCATE(SNOWC)
      DEALLOCATE(THC)
      DEALLOCATE(TMN)
      DEALLOCATE(TSFC)
      DEALLOCATE(TSLB)
      DEALLOCATE(XLAND)
      DEALLOCATE(XLAT)
      DEALLOCATE(XLONG)
      DEALLOCATE(ZS)
!-----------------------------------------------------------------------
!----------------------------------------------------------------------
        DO J=jfs,jfe
        DO I=ifs,ife
          DWDTMN(I,J)=DWDTMN(I,J)*HBM3(I,J)
          DWDTMX(I,J)=DWDTMX(I,J)*HBM3(I,J)
        ENDDO
        ENDDO
!----------------------------------------------------------------------
!***  INITIALIZE 3RD INDEX IN WORKING ARRAYS USED IN PFDHT, DDAMP, AND  
!***  HZADV.  THESE ARRAYS MUST HAVE AN EXTENT OF MORE THAN 1 IN J DUE 
!***  TO THE MANY DIFFERENCES AND AVERAGES THAT ARE COMPUTED IN J
!***  OR BECAUSE THE ARRAY IS SIMPLY REFERENCED AT MORE THAN ONE J.
!***  THE WORKING "SPACE" SPANS FROM 3 ROWS SOUTH TO 3 ROWS NORTH
!***  OF THE ROW FOR WHICH THE PRIMARY COMPUTATION IS BEING DONE
!***  THUS THE 3RD DIMENSION CAN VARY FROM -3 TO +3 ALTHOUGH ALL OF
!***  THESE ARRAYS DO NOT NEED TO SPAN THAT MANY ROWS.  FOR INSTANCE,
!***  SOME OF THE ARRAYS ARE ONLY USED FROM 2 ROWS SOUTH TO 1 ROW 
!***  NORTH, OR FROM 1 ROW SOUTH TO THE CENTRAL ROW.  AS THE INTEGRATION
!***  MOVES NORTHWARD, THE SOUTHERNMOST I,K SLAB IS DROPPED FOR EACH
!***  WORKING ARRAY AND THE NORTHERNMOST IS GENERATED.  SO AS NOT TO
!***  HAVE TO ACTUALLY MOVE ANY OF THE I,K SLABS NORTHWARD, THE 3RD
!***  INDEX IS CYCLED THROUGH THE EXTENT OF EACH ARRAYS J DIMENSION.
!***  THE FOLLOWING WILL FILL AN ARRAY WITH THE VALUES OF THE 3RD
!***  INDEX FOR EACH THESE VARIATIONS OF J EXTENTS FOR ALL Js IN
!***  THE LOCAL DOMAIN.
!----------------------------------------------------------------------
!
!***  CASE 0: J EXTENT IS -3 TO 3
!
      KNT=0
      DO J=MYJS2_P2,MYJE2_P2
        KNT=KNT+1
        JP3=KNT+2-7*((KNT+5)/7)
        JP2=JP3-1+7*((4-JP3)/7)
        JP1=JP2-1+7*((4-JP2)/7)
        J00=JP1-1+7*((4-JP1)/7)
        JM1=J00-1+7*((4-J00)/7)
        JM2=JM1-1+7*((4-JM1)/7)
        JM3=JM2-1+7*((4-JM2)/7)
        INDX3_WRK(3,KNT,0)=JP3
        INDX3_WRK(2,KNT,0)=JP2
        INDX3_WRK(1,KNT,0)=JP1
        INDX3_WRK(0,KNT,0)=J00
        INDX3_WRK(-1,KNT,0)=JM1
        INDX3_WRK(-2,KNT,0)=JM2
        INDX3_WRK(-3,KNT,0)=JM3
      ENDDO
!
!***  CASE 1: J EXTENT IS -2 TO 2
!
      KNT=0
      DO J=MYJS2_P2,MYJE2_P2
        KNT=KNT+1
        JP2=KNT+1-5*((KNT+3)/5)
        JP1=JP2-1+5*((3-JP2)/5)
        J00=JP1-1+5*((3-JP1)/5)
        JM1=J00-1+5*((3-J00)/5)
        JM2=JM1-1+5*((3-JM1)/5)
        INDX3_WRK(3,KNT,1)=999
        INDX3_WRK(2,KNT,1)=JP2
        INDX3_WRK(1,KNT,1)=JP1
        INDX3_WRK(0,KNT,1)=J00
        INDX3_WRK(-1,KNT,1)=JM1
        INDX3_WRK(-2,KNT,1)=JM2
        INDX3_WRK(-3,KNT,1)=999
      ENDDO
!
!***  CASE 2: J EXTENT IS -2 TO 1
!
      KNT=0
      DO J=MYJS2_P2,MYJE2_P2
        KNT=KNT+1
        JP1=KNT-4*((KNT+2)/4)
        J00=JP1-1+4*((2-JP1)/4)
        JM1=J00-1+4*((2-J00)/4)
        JM2=JM1-1+4*((2-JM1)/4)
        INDX3_WRK(3,KNT,2)=999
        INDX3_WRK(2,KNT,2)=999
        INDX3_WRK(1,KNT,2)=JP1
        INDX3_WRK(0,KNT,2)=J00
        INDX3_WRK(-1,KNT,2)=JM1
        INDX3_WRK(-2,KNT,2)=JM2
        INDX3_WRK(-3,KNT,2)=999
      ENDDO
!
!***  CASE 3: J EXTENT IS -1 TO 2
!
      KNT=0
      DO J=MYJS2_P2,MYJE2_P2
        KNT=KNT+1
        JP2=KNT+1-4*((KNT+2)/4)
        JP1=JP2-1+4*((3-JP2)/4)
        J00=JP1-1+4*((3-JP1)/4)
        JM1=J00-1+4*((3-J00)/4)
        INDX3_WRK(3,KNT,3)=999
        INDX3_WRK(2,KNT,3)=JP2
        INDX3_WRK(1,KNT,3)=JP1
        INDX3_WRK(0,KNT,3)=J00
        INDX3_WRK(-1,KNT,3)=JM1
        INDX3_WRK(-2,KNT,3)=999
        INDX3_WRK(-3,KNT,3)=999
      ENDDO
!
!***  CASE 4: J EXTENT IS -1 TO 1
!
      KNT=0
      DO J=MYJS2_P2,MYJE2_P2
        KNT=KNT+1
        JP1=KNT-3*((KNT+1)/3)
        J00=JP1-1+3*((2-JP1)/3)
        JM1=J00-1+3*((2-J00)/3)
        INDX3_WRK(3,KNT,4)=999
        INDX3_WRK(2,KNT,4)=999
        INDX3_WRK(1,KNT,4)=JP1
        INDX3_WRK(0,KNT,4)=J00
        INDX3_WRK(-1,KNT,4)=JM1
        INDX3_WRK(-2,KNT,4)=999
        INDX3_WRK(-3,KNT,4)=999
      ENDDO
!
!***  CASE 5: J EXTENT IS -1 TO 0
!
      KNT=0
      DO J=MYJS2_P2,MYJE2_P2
        KNT=KNT+1
        J00=-MOD(KNT+1,2)
        JM1=-1-J00
        INDX3_WRK(3,KNT,5)=999
        INDX3_WRK(2,KNT,5)=999
        INDX3_WRK(1,KNT,5)=999
        INDX3_WRK(0,KNT,5)=J00
        INDX3_WRK(-1,KNT,5)=JM1
        INDX3_WRK(-2,KNT,5)=999
        INDX3_WRK(-3,KNT,5)=999
      ENDDO
!
!***  CASE 6: J EXTENT IS 0 TO 1
!
      KNT=0
      DO J=MYJS2_P2,MYJE2_P2
        KNT=KNT+1
        JP1=MOD(KNT,2)
        J00=1-JP1
        INDX3_WRK(3,KNT,6)=999
        INDX3_WRK(2,KNT,6)=999
        INDX3_WRK(1,KNT,6)=JP1
        INDX3_WRK(0,KNT,6)=J00
        INDX3_WRK(-1,KNT,6)=999
        INDX3_WRK(-2,KNT,6)=999
        INDX3_WRK(-3,KNT,6)=999
      ENDDO
!     if(mype.eq.n_t)then
!       do ns=1,nsoil
!         write(0,*) exit START sh2o=,sh2o(i_t,ns,j_t)
!       enddo
!     endif


!STARTOFREGISTRYGENERATEDINCLUDE 'inc/nmm_scalar_derefs.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
 grid%nmm_dy_nmm = dy_nmm
 grid%nmm_cpgfv = cpgfv
 grid%nmm_en = en
 grid%nmm_ent = ent
 grid%nmm_f4d = f4d
 grid%nmm_f4q = f4q
 grid%nmm_ef4t = ef4t
 grid%nmm_upstrm = upstrm
 grid%nmm_dlmd = dlmd
 grid%nmm_dphd = dphd
 grid%nmm_pdtop = pdtop
 grid%nmm_pt = pt
 grid%nmm_micro_start = micro_start
 grid%nmm_hydro = hydro
 grid%nmm_nclod = nclod
 grid%nmm_nprec = nprec
 grid%nmm_nheat = nheat
 grid%nmm_nrdlw = nrdlw
 grid%nmm_nrdsw = nrdsw
 grid%nmm_nsrfc = nsrfc
 grid%nmm_avrain = avrain
 grid%nmm_avcnvc = avcnvc
 grid%nmm_aratim = aratim
 grid%nmm_acutim = acutim
 grid%nmm_ardlw = ardlw
 grid%nmm_ardsw = ardsw
 grid%nmm_asrfc = asrfc
 grid%nmm_aphtim = aphtim
 grid%dtbc = dtbc
 grid%number_at_same_level = number_at_same_level
 grid%itimestep = itimestep
 grid%oid = oid
 grid%auxhist1_oid = auxhist1_oid
 grid%auxhist2_oid = auxhist2_oid
 grid%auxhist3_oid = auxhist3_oid
 grid%auxhist4_oid = auxhist4_oid
 grid%auxhist5_oid = auxhist5_oid
 grid%auxinput1_oid = auxinput1_oid
 grid%auxinput2_oid = auxinput2_oid
 grid%auxinput3_oid = auxinput3_oid
 grid%auxinput4_oid = auxinput4_oid
 grid%auxinput5_oid = auxinput5_oid
 grid%nframes = nframes
 grid%lbc_fid = lbc_fid
 grid%tiled = tiled
 grid%patched = patched
 grid%input_from_file = input_from_file
 grid%write_metadata = write_metadata
 grid%time_step = time_step
 grid%time_step_fract_num = time_step_fract_num
 grid%time_step_fract_den = time_step_fract_den
 grid%restart = restart
 grid%max_dom = max_dom
 grid%dyn_opt = dyn_opt
 grid%rk_ord = rk_ord
 grid%diff_opt = diff_opt
 grid%km_opt = km_opt
 grid%damp_opt = damp_opt
 grid%isfflx = isfflx
 grid%ifsnow = ifsnow
 grid%icloud = icloud
 grid%num_soil_layers = num_soil_layers
 grid%num_land_cat = num_land_cat
 grid%num_soil_cat = num_soil_cat
 grid%spec_bdy_width = spec_bdy_width
 grid%spec_zone = spec_zone
 grid%relax_zone = relax_zone
 grid%ensdim = ensdim
 grid%tile_sz_x = tile_sz_x
 grid%tile_sz_y = tile_sz_y
 grid%numtiles = numtiles
 grid%debug_level = debug_level
 grid%irand = irand
 grid%run_days = run_days
 grid%run_hours = run_hours
 grid%run_minutes = run_minutes
 grid%run_seconds = run_seconds
 grid%start_year = start_year
 grid%start_month = start_month
 grid%start_day = start_day
 grid%start_hour = start_hour
 grid%start_minute = start_minute
 grid%start_second = start_second
 grid%end_year = end_year
 grid%end_month = end_month
 grid%end_day = end_day
 grid%end_hour = end_hour
 grid%end_minute = end_minute
 grid%end_second = end_second
 grid%grid_id = grid_id
 grid%level = level
 grid%s_we = s_we
 grid%e_we = e_we
 grid%s_sn = s_sn
 grid%e_sn = e_sn
 grid%s_vert = s_vert
 grid%e_vert = e_vert
 grid%history_interval = history_interval
 grid%auxhist1_interval = auxhist1_interval
 grid%auxhist2_interval = auxhist2_interval
 grid%auxhist3_interval = auxhist3_interval
 grid%auxhist4_interval = auxhist4_interval
 grid%auxhist5_interval = auxhist5_interval
 grid%auxinput1_interval = auxinput1_interval
 grid%auxinput2_interval = auxinput2_interval
 grid%auxinput3_interval = auxinput3_interval
 grid%auxinput4_interval = auxinput4_interval
 grid%auxinput5_interval = auxinput5_interval
 grid%restart_interval = restart_interval
 grid%frames_per_outfile = frames_per_outfile
 grid%time_step_sound = time_step_sound
 grid%parent_id = parent_id
 grid%i_parent_start = i_parent_start
 grid%j_parent_start = j_parent_start
 grid%shw = shw
 grid%parent_grid_ratio = parent_grid_ratio
 grid%parent_time_step_ratio = parent_time_step_ratio
 grid%moad_grid_ratio = moad_grid_ratio
 grid%moad_time_step_ratio = moad_time_step_ratio
 grid%non_hydrostatic = non_hydrostatic
 grid%dx = dx
 grid%dy = dy
 grid%dt = dt
 grid%ztop = ztop
 grid%zdamp = zdamp
 grid%dampcoef = dampcoef
 grid%smdiv = smdiv
 grid%emdiv = emdiv
 grid%epssm = epssm
 grid%khdif = khdif
 grid%kvdif = kvdif
 grid%mix_cr_len = mix_cr_len
 grid%tke_upper_bound = tke_upper_bound
 grid%kh_tke_upper_bound = kh_tke_upper_bound
 grid%kv_tke_upper_bound = kv_tke_upper_bound
 grid%radt = radt
 grid%bldt = bldt
 grid%cudt = cudt
 grid%gsmdt = gsmdt
 grid%julyr = julyr
 grid%julday = julday
 grid%gmt = gmt
 grid%periodic_x = periodic_x
 grid%symmetric_xs = symmetric_xs
 grid%symmetric_xe = symmetric_xe
 grid%open_xs = open_xs
 grid%open_xe = open_xe
 grid%periodic_y = periodic_y
 grid%symmetric_ys = symmetric_ys
 grid%symmetric_ye = symmetric_ye
 grid%open_ys = open_ys
 grid%open_ye = open_ye
 grid%nested = nested
 grid%specified = specified
 grid%top_radiation = top_radiation
 grid%idtad = idtad
 grid%nsoil = nsoil
 grid%nphs = nphs
 grid%ncnvc = ncnvc
 grid%nrads = nrads
 grid%nradl = nradl
 grid%sigma = sigma
 grid%chem_opt = chem_opt
 grid%mp_physics = mp_physics
 grid%ra_lw_physics = ra_lw_physics
 grid%ra_sw_physics = ra_sw_physics
 grid%bl_sfclay_physics = bl_sfclay_physics
 grid%bl_surface_physics = bl_surface_physics
 grid%bl_pbl_physics = bl_pbl_physics
 grid%cu_physics = cu_physics
 grid%h_mom_adv_order = h_mom_adv_order
 grid%v_mom_adv_order = v_mom_adv_order
 grid%h_sca_adv_order = h_sca_adv_order
 grid%v_sca_adv_order = v_sca_adv_order
 grid%io_form_input = io_form_input
 grid%io_form_auxinput1 = io_form_auxinput1
 grid%io_form_auxinput2 = io_form_auxinput2
 grid%io_form_auxinput3 = io_form_auxinput3
 grid%io_form_auxinput4 = io_form_auxinput4
 grid%io_form_auxinput5 = io_form_auxinput5
 grid%io_form_history = io_form_history
 grid%io_form_auxhist1 = io_form_auxhist1
 grid%io_form_auxhist2 = io_form_auxhist2
 grid%io_form_auxhist3 = io_form_auxhist3
 grid%io_form_auxhist4 = io_form_auxhist4
 grid%io_form_auxhist5 = io_form_auxhist5
 grid%io_form_restart = io_form_restart
 grid%io_form_boundary = io_form_boundary
 grid%interval_seconds = interval_seconds
 grid%real_data_init_type = real_data_init_type
 grid%cen_lat = cen_lat
 grid%cen_lon = cen_lon
 grid%truelat1 = truelat1
 grid%truelat2 = truelat2
 grid%bdyfrq = bdyfrq
 grid%iswater = iswater
 grid%isice = isice
 grid%map_proj = map_proj
!ENDOFREGISTRYGENERATEDINCLUDE

   RETURN


END SUBROUTINE start_domain_nmm

