!NCEP_MESO:MEDIATION_LAYER:SOLVER
!
!***********************************************************************
!

SUBROUTINE SOLVE_NMM(GRID,CONFIG_FLAGS,                                 &
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

!-----------------------------------------------------------------------
      USE MODULE_DOMAIN
      USE MODULE_CONFIGURE
      USE MODULE_MODEL_CONSTANTS
      USE MODULE_STATE_DESCRIPTION
      USE MODULE_CTLBLK
      USE MODULE_MPP
      USE MODULE_DM
      USE MODULE_PHYS
      USE MODULE_IGWAVE_ADJUST,		ONLY: PDTE, PFDHT, DDAMP, VTOA
      USE MODULE_ADVECTION,		ONLY: ADVE, VAD2, HAD2
      USE MODULE_NONHY_DYNAM,		ONLY: EPS, VADZ, HADZ
      USE MODULE_DIFFUSION_NMM, 	ONLY: HDIFF
      USE MODULE_BNDRY_COND,		ONLY: BOCOH, BOCOV
      USE MODULE_EXT_INTERNAL
!     integer :: npes
!     integer :: MPI_COMM_COMP
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
!***  INPUT DATA
!
      TYPE(DOMAIN),TARGET :: GRID
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
      TYPE(GRID_CONFIG_REC_TYPE),INTENT(IN) :: CONFIG_FLAGS
!
!-----------------------------------------------------------------------
!
!***  LOCAL VARIABLES
!
!-----------------------------------------------------------------------
      INTEGER :: IDS,IDE,JDS,JDE,KDS,KDE,                               &
                 IMS,IME,JMS,JME,KMS,KME,                               & 
                 IPS,IPE,JPS,JPE,KPS,KPE
!
      INTEGER :: ITS,ITE,JTS,JTE,KTS,KTE

      INTEGER :: N_MOIST
      INTEGER :: I,ICLTEND,J,JC,LB,k,ierr,mpi_comm_world,irtn
      INTEGER :: IJDS,IJDE,IDF,JDF,KDF
      INTEGER MYPROC
      INTEGER RC
      CHARACTER*80 :: message
!
      REAL :: GPS
      REAL,DIMENSION(GRID%SM31:GRID%EM31,GRID%SM33:GRID%EM33) :: PBLH 

      LOGICAL :: LAST_TIME
!
      real*8 :: timef
      real,save :: solve_tim,exch_tim,pdte_tim,adve_tim,vtoa_tim        &
     &,            vadz_tim,hadz_tim,eps_tim,vad2_tim,had2_tim          &
     &,            radiation_tim,rdtemp_tim,turbl_tim,cltend_tim        &
     &,            cucnvc_tim,gsmdrive_tim,hdiff_tim,bocoh_tim          &
     &,            pfdht_tim,ddamp_tim,bocov_tim,sum_tim
      real :: btim,btimx

!	real, dimension(6,7) :: t_save
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------


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

      CALL WRF_GET_MYPROC(MYPROC)
!
      CALL GET_IJK_FROM_GRID(grid,                                      &
                             ids,ide,jds,jde,kds,kde,                   &
                             ims,ime,jms,jme,kms,kme,                   &
                             ips,ipe,jps,jpe,kps,kpe )
      CALL wrf_debug(100,"dyn_nmm/solve_nmm.F: warning SIGMA hard coded") 
!
      SIGMA=1 
      HYDRO=.FALSE.
      IHRST=GRID%GMT

!	write(0,*) set IHRST to : , IHRST

      IJDS=MIN(IDS, JDS)
      IJDE=MAX(IDE, JDE)

      IDF=IDE-1
      JDF=JDE-1
      KDF=KDE-1
!
!-----------------------------------------------------------------------
!
!***  FOR NOW SET CONTROLS FOR TILES TO PATCHES
!
      ITS=IPS
      ITE=MIN(IPE,IDF)
      JTS=JPS
      JTE=MIN(JPE,JDF)
      KTS=KPS
      KTE=MIN(KPE,KDF)
!-----------------------------------------------------------------------
      if(ntsd.eq.0)then
        solve_tim=0.
        exch_tim=0.
        pdte_tim=0.
        adve_tim=0.
        vtoa_tim=0.
        vadz_tim=0.
        hadz_tim=0.
        eps_tim=0.
        vad2_tim=0.
        had2_tim=0.
        radiation_tim=0.
        rdtemp_tim=0.
        turbl_tim=0.
        cltend_tim=0.
        cucnvc_tim=0.
        gsmdrive_tim=0.
        hdiff_tim=0.
        bocoh_tim=0.
        pfdht_tim=0.
        ddamp_tim=0.
        bocov_tim=0.
      endif
!-----------------------------------------------------------------------
      N_MOIST = NUM_MOIST
!
      MYIS1   =MAX(IDS+1,IPS  )
      MYIE1   =MIN(IDF-1,IPE  )
!
      MYIS_P1 =MAX(IDS  ,IPS-1)
      MYIE_P1 =MIN(IDF  ,IPE+1)
      MYIS_P2 =MAX(IDS  ,IPS-2)
!
      MYIE_P2 =MIN(IDF  ,IPE+2)
!
      MYIS_P3 =MAX(IDS  ,IPS-3)
      MYIE_P3 =MIN(IDF  ,IPE+3)
      MYIS_P4 =MAX(IDS  ,IPS-4)
      MYIE_P4 =MIN(IDF  ,IPE+4)
      MYJS_P4 =MAX(JDS  ,JPS-4)
      MYJE_P4 =MIN(JDF  ,JPE+4)
      MYIS_P5 =MAX(IDS  ,IPS-5)
      MYIE_P5 =MIN(IDF  ,IPE+5)
      MYJS_P5 =MAX(JDS  ,JPS-5)
      MYJE_P5 =MIN(JDF  ,JPE+5)
      MYIS1_P1=MAX(IDS+1,IPS-1)
      MYIE1_P1=MIN(IDF-1,IPE+1)
      MYIS1_P2=MAX(IDS+1,IPS-2)
      MYIE1_P2=MIN(IDF-1,IPE+2)
      MYJS2_P2=MAX(JDS+2,JPS-2)
      MYJE2_P2=MIN(JDF-2,JPE+2)
!
      DO J=MYJS_P4,MYJE_P4
        IHEG(J)=MOD(J+1,2)
        IHWG(J)=IHEG(J)-1
        IVEG(J)=MOD(J,2)
        IVWG(J)=IVEG(J)-1
      ENDDO

      DO J=MYJS_P4,MYJE_P4
        IVW(J)=IVWG(J)
        IVE(J)=IVEG(J)
        IHE(J)=IHEG(J)
        IHW(J)=IHWG(J)
      ENDDO
!
!***  LATERAL POINTS IN THE BOUNDARY ARRAYS
!
      LB=2*(IDF-IDS+1)+(JDF-JDS+1)-3
!
!***  APPROXIMATE GRIDPOINT SPACING (METERS)
!
      JC=JMS+(JME-JMS)/2
      GPS=SQRT(DX_NMM(IMS,JC)**2+DY_NMM**2)
!
!***  TIMESTEPS PER HOUR
!
      TSPH=3600./DT
!-----------------------------------------------------------------------
!
      PBLH = 0.

      DO J=JMS,JME
      DO I=IMS,IME
        PBLH(I,J)=-1.
      ENDDO
      ENDDO
!
      NBOCO=0
!-----------------------------------------------------------------------
!***
!***               THE MAIN TIME INTEGRATION LOOP
!***
!-----------------------------------------------------------------------
!
!***  NTSD IS THE TIMESTEP COUNTER (Number of Time Steps Done)
!
!-----------------------------------------------------------------------
!
!***
!***  FIRST TIME THROUGH TOTAL_TIME_STEPS IS ZERO.
!***  IT IS INCREMENTED IN SOLVE_INTERFACE.
!***
      CALL ESMF_ClockGetAdvanceCount(grid%domain_clock,NTSD,rc)
      LAST_TIME=grid%stop_time.EQ.grid%current_time+grid%step_time

!      CALL ESMF_TimeGetString( grid%current_time, message, rc=rc )
       WRITE(0,*)' SOLVE_NMM CALLED: TIMESTEP IS ',NTSD
!      WRITE(message,*) SOLVE_NMM CALLED: TIMESTEP IS ,NTSD           &
!               , FCST TIME IS,TRIM(message)
!      CALL wrf_message( message )

      CALL WRF_GET_MYPROC(MYPE)
      CALL WRF_GET_DM_COMMUNICATOR ( mpi_comm_world )
!
      btim=timef()
!


!test insert


!***
!***  LONG WAVE RADIATION ARRAYS.
!***
!tst        IF(MOD(NTSD,NRDLW).LT.NPHS)THEN
        IF(MOD(NTSD,NRDLW).EQ.0 .and. MOD(NTSD,NRDLW).LT.NPHS)THEN
          CALL wrf_message('  ZERO ACCUM LW RADTN ARRAYS')
          ARDLW=0.
          DO J=JTS,JTE
          DO I=ITS,ITE
            ALWIN(I,J) =0.
            ALWOUT(I,J)=0.
            ALWTOA(I,J)=0.
          ENDDO
          ENDDO
        ENDIF

!***
!***  SHORT WAVE RADIATION ARRAYS.
!***
!tst        IF(MOD(NTSD,NRDSW).LT.NPHS)THEN
        IF(MOD(NTSD,NRDSW).EQ.0 .and. MOD(NTSD,NRDSW).LT.NPHS)THEN
          CALL wrf_message('  ZERO ACCUM SW RADTN ARRAYS')
          ARDSW=0.
          DO J=JTS,JTE
          DO I=ITS,ITE
            ASWIN(I,J) =0.
            ASWOUT(I,J)=0.
            ASWTOA(I,J)=0.
          ENDDO
          ENDDO
        ENDIF
!***
!***  SURFACE SENSIBLE AND LATENT HEAT FLUX ARRAYS.
!***
!tst        IF(MOD(NTSD,NSRFC).LT.NPHS)THEN
        IF(MOD(NTSD,NSRFC).EQ.0)THEN
          CALL wrf_message('  ZERO ACCUM SFC FLUX ARRAYS')
          ASRFC=0.
          DO J=JTS,JTE
          DO I=ITS,ITE
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


!end test insert

      IF(NTSD.EQ.0)THEN
        FIRST=.TRUE.
      btimx=timef()
      exch_tim=exch_tim+timef()-btimx
        GO TO 2003
      ENDIF

	do J=JTS,JTE
	do K=KTS,KTE-1
	do I=ITS,ITE
	if (T(I,K,J) .gt. 330. .or. T(I,K,J) .lt. 180) then
	write(0,*) 'strange T, top solve_nmm..I,K,J,T(I,K,J): ', I,K,J,T(I,K,J)
	endif
	if ( abs(U(I,K,J)) .gt. 120.) then
	write(0,*) 'strange U, top solve_nmm..I,K,J,U(I,K,J): ', I,K,J,U(I,K,J)
	endif
	if ( abs(V(I,K,J)) .gt. 120.) then
	write(0,*) 'strange V, top solve_nmm..I,K,J,V(I,K,J): ', I,K,J,V(I,K,J)
	endif
	enddo
	enddo
	enddo

!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
 2000 CONTINUE
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***  PRESSURE TENDENCY, SIGMA DOT, VERTICAL PART OF OMEGA-ALPHA
!-----------------------------------------------------------------------
!
      btimx=timef()
      exch_tim=exch_tim+timef()-btimx

      CALL wrf_debug ( 100 , 'nmm: in pdte' )

      btimx=timef()

      CALL PDTE(                                                        &
     &            NTSD,DT,PT,ETA2,RES,HYDRO                             &
     &           ,HTM,HBM2                                              &
     &           ,PD,PDSL,PDSLO                                         &
     &           ,PETDT,DIV,PSDT                                        &
     &           ,IHE,IHW,IVE,IVW,INDX3_WRK                             &
     &           ,IDS,IDF,JDS,JDF,KDS,KDE                               &
     &           ,IMS,IME,JMS,JME,KMS,KME                               &
     &           ,ITS,ITE,JTS,JTE,KTS,KTE)
	do J=JTS,JTE
	do K=KTS,KTE-1
	do I=ITS,ITE
	if (T(I,K,J) .gt. 330. .or. T(I,K,J) .lt. 180) then
	write(0,*) 'strange T post PDTE...I,K,J,T(I,K,J): ', I,K,J,T(I,K,J)
	endif
	enddo
	enddo
	enddo

      pdte_tim=pdte_tim+timef()-btimx
      CALL wrf_debug ( 100 , 'nmm: out of pdte' )
!
!-----------------------------------------------------------------------
!***  ADVECTION OF T, U, AND V
!-----------------------------------------------------------------------
!
      btimx=timef()
      exch_tim=exch_tim+timef()-btimx

      CALL wrf_debug ( 100 , 'nmm: in adve' )

      btimx=timef()

      CALL ADVE(NTSD,DT,DETA1,DETA2,PDTOP                               &
     &         ,CURV,F,FAD,F4D,EM_LOC,EMT_LOC,EN,ENT,DX_NMM,DY_NMM      &
     &         ,HTM,HBM2,VTM,VBM2,LMH,LMV                               &
     &         ,T,U,V,PDSLO,TOLD,UOLD,VOLD                              &
     &         ,PETDT,UPSTRM                                            &
     &         ,FEW,FNS,FNE,FSE                                         &
     &         ,ADT,ADU,ADV                                             & 
     &         ,N_IUP_H,N_IUP_V                                         &
     &         ,N_IUP_ADH,N_IUP_ADV                                     &
     &         ,IUP_H,IUP_V,IUP_ADH,IUP_ADV                             &
     &         ,IHE,IHW,IVE,IVW,INDX3_WRK                               &
     &         ,IDS,IDF,JDS,JDF,KDS,KDE                                 &
     &         ,IMS,IME,JMS,JME,KMS,KME                                 &
     &         ,ITS,ITE,JTS,JTE,KTS,KTE)
      adve_tim=adve_tim+timef()-btimx
	do J=JTS,JTE
	do K=KTS,KTE-1
	do I=ITS,ITE
	if (T(I,K,J) .gt. 330. .or. T(I,K,J) .lt. 180) then
	write(0,*) 'strange T post ADVE...I,K,J,T(I,K,J): ', I,K,J,T(I,K,J)
	endif
	enddo
	enddo
	enddo

      CALL wrf_debug ( 100 , 'nmm: out of adve' )

!-----------------------------------------------------------------------
!***  PRESSURE TENDENCY, ETA/SIGMADOT, VERTICAL PART OF OMEGA-ALPHA TERM
!-----------------------------------------------------------------------
!
      CALL wrf_debug ( 100 , 'nmm: in vtoa' )

      btimx=timef()

      CALL VTOA(                                                        &
     &          NTSD,DT,PT,ETA2                                         &
     &         ,HTM,HBM2,EF4T                                           &
     &         ,T,DWDT,RTOP,OMGALF                                      &
     &         ,PINT,DIV,PSDT,RES                                       &
     &         ,IHE,IHW,IVE,IVW,INDX3_WRK                               &
     &         ,IDS,IDF,JDS,JDF,KDS,KDE                                 &
     &         ,IMS,IME,JMS,JME,KMS,KME                                 &
     &         ,ITS,ITE,JTS,JTE,KTS,KTE)
      vtoa_tim=vtoa_tim+timef()-btimx
	do J=JTS,JTE
	do K=KTS,KTE-1
	do I=ITS,ITE
	if (T(I,K,J) .gt. 330. .or. T(I,K,J) .lt. 180) then
	write(0,*) 'strange T post VTOA...I,K,J,T(I,K,J): ', I,K,J,T(I,K,J)
	endif
	enddo
	enddo
	enddo

      CALL wrf_debug ( 100 , 'nmm: out of vtoa' )

!-----------------------------------------------------------------------
!***  VERTICAL ADVECTION OF HEIGHT
!-----------------------------------------------------------------------
!
      CALL wrf_debug ( 100 , 'nmm: in vadz' )

      btimx=timef()

      CALL VADZ(NTSD,DT,FIS,SIGMA,DFL,HTM,HBM2                          &
     &         ,DETA1,DETA2,PDTOP                                       &
     &         ,PINT,PDSL,PDSLO,PETDT                                   &
     &         ,RTOP,T,Q,CWM,Z,W,DWDT,PDWDT                             &
     &         ,IHE,IHW,IVE,IVW,INDX3_WRK                               &
     &         ,IDS,IDF,JDS,JDF,KDS,KDE                                 &
     &         ,IMS,IME,JMS,JME,KMS,KME                                 &
     &         ,ITS,ITE,JTS,JTE,KTS,KTE)
	do J=JTS,JTE
	do K=KTS,KTE-1
	do I=ITS,ITE
	if (T(I,K,J) .gt. 330. .or. T(I,K,J) .lt. 180) then
	write(0,*) 'strange T post VADZ...I,K,J,T(I,K,J): ', I,K,J,T(I,K,J)
	endif
	enddo
	enddo
	enddo

      vadz_tim=vadz_tim+timef()-btimx

      CALL wrf_debug ( 100 , 'nmm: out of vadz' )
!
!-----------------------------------------------------------------------
!***  HORIZONTAL ADVECTION OF HEIGHT
!-----------------------------------------------------------------------
!
      btimx=timef()
      exch_tim=exch_tim+timef()-btimx

      CALL wrf_debug ( 100 , 'nmm: in hadz' )

      btimx=timef()

      CALL HADZ(NTSD,DT,HYDRO,HTM,HBM2,DETA1,DETA2,PDTOP                &
     &         ,DX_NMM,DY_NMM,FAD                                       &
     &         ,FEW,FNS,FNE,FSE                                         &
     &         ,PDSL,U,V,W,Z                                            &
     &         ,IHE,IHW,IVE,IVW,INDX3_WRK                               &
     &         ,IDS,IDF,JDS,JDF,KDS,KDE                                 &
     &         ,IMS,IME,JMS,JME,KMS,KME                                 &
     &         ,ITS,ITE,JTS,JTE,KTS,KTE)
      hadz_tim=hadz_tim+timef()-btimx
	do J=JTS,JTE
	do K=KTS,KTE-1
	do I=ITS,ITE
	if (T(I,K,J) .gt. 330. .or. T(I,K,J) .lt. 180) then
	write(0,*) 'strange T post HADZ...I,K,J,T(I,K,J): ', I,K,J,T(I,K,J)
	endif
	enddo
	enddo
	enddo

      CALL wrf_debug ( 100 , 'nmm: out of hadz' )
!
!-----------------------------------------------------------------------
!***  ADVECTION OF W
!-----------------------------------------------------------------------
!
      btimx=timef()
      exch_tim=exch_tim+timef()-btimx

      CALL wrf_debug ( 100 , 'nmm: in eps' )

      btimx=timef()

      CALL EPS(NTSD,DT,HYDRO,DX_NMM,DY_NMM,FAD                          &
     &        ,DETA1,DETA2,PDTOP,PT                                     &
     &        ,HTM,HBM2,HBM3,LMH                                        &
     &        ,PDSL,PDSLO,PINT,RTOP,PETDT,PDWDT                         &
     &        ,DWDT,DWDTMN,DWDTMX                                       &
     &        ,FNS,FEW,FNE,FSE                                          &
     &        ,T,U,V,W,Q,CWM                                            &
     &        ,IHE,IHW,IVE,IVW,INDX3_WRK                                &
     &        ,IDS,IDF,JDS,JDF,KDS,KDE                                  &
     &        ,IMS,IME,JMS,JME,KMS,KME                                  &
     &        ,ITS,ITE,JTS,JTE,KTS,KTE)
      eps_tim=eps_tim+timef()-btimx
	do J=JTS,JTE
	do K=KTS,KTE-1
	do I=ITS,ITE
	if (T(I,K,J) .gt. 330. .or. T(I,K,J) .lt. 180) then
	write(0,*) 'strange T post EPS...I,K,J,T(I,K,J): ', I,K,J,T(I,K,J)
	endif
	enddo
	enddo
	enddo

      CALL wrf_debug ( 100 , 'nmm: out of eps' )
!
!-----------------------------------------------------------------------
!***  ADVECTION OF Q, TKE, AND CLOUD WATER
!-----------------------------------------------------------------------
!
      IF(MOD(NTSD,IDTAD).EQ.0)THEN
        CALL wrf_debug ( 100 , 'nmm: in vad2' )
        btimx=timef()
        CALL VAD2(NTSD,DT,IDTAD,DX_NMM,DY_NMM                           &
     &           ,AETA1,AETA2,DETA1,DETA2,PDSL,PDTOP                    &
     &           ,HBM2,LMH                                              &
     &           ,Q,Q2,CWM,PETDT                                        &
     &           ,N_IUP_H,N_IUP_V                                       &
     &           ,N_IUP_ADH,N_IUP_ADV                                   &
     &           ,IUP_H,IUP_V,IUP_ADH,IUP_ADV                           &
     &           ,IHE,IHW,IVE,IVW,INDX3_WRK                             &
     &           ,IDS,IDF,JDS,JDF,KDS,KDE                               &
     &           ,IMS,IME,JMS,JME,KMS,KME                               &
     &           ,ITS,ITE,JTS,JTE,KTS,KTE)
        vad2_tim=vad2_tim+timef()-btimx
	do J=JTS,JTE
	do K=KTS,KTE-1
	do I=ITS,ITE
	if (T(I,K,J) .gt. 330. .or. T(I,K,J) .lt. 180) then
	write(0,*) 'strange T post VAD2...I,K,J,T(I,K,J): ', I,K,J,T(I,K,J)
	endif
	enddo
	enddo
	enddo

        CALL wrf_debug ( 100 , 'nmm: out of vad2' )
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(MOD(NTSD,IDTAD).EQ.0)THEN

        btimx=timef()
        exch_tim=exch_tim+timef()-btimx

        CALL wrf_debug ( 100 , 'nmm: in had2' )

        btimx=timef()

        CALL HAD2(                                                     &
     &            NTSD,DT,IDTAD,DX_NMM,DY_NMM                          &
     &           ,AETA1,AETA2,DETA1,DETA2,PDSL,PDTOP                   &
     &           ,HTM,HBM2,HBM3,LMH                                    &
     &           ,Q,Q2,CWM,U,V,Z,HYDRO                                 &
     &           ,N_IUP_H,N_IUP_V                                      &
     &           ,N_IUP_ADH,N_IUP_ADV                                  &
     &           ,IUP_H,IUP_V,IUP_ADH,IUP_ADV                          &
     &           ,IHE,IHW,IVE,IVW,INDX3_WRK                            &
     &           ,IDS,IDF,JDS,JDF,KDS,KDE                              &
     &           ,IMS,IME,JMS,JME,KMS,KME                              &
     &           ,ITS,ITE,JTS,JTE,KTS,KTE)
        had2_tim=had2_tim+timef()-btimx
        CALL wrf_debug ( 100 , 'nmm: out of had2' )
	do J=JTS,JTE
	do K=KTS,KTE-1
	do I=ITS,ITE
	if (T(I,K,J) .gt. 330. .or. T(I,K,J) .lt. 180) then
	write(0,*) 'strange T post HAD2...I,K,J,T(I,K,J): ', I,K,J,T(I,K,J)
	endif
	enddo
	enddo
	enddo
      ENDIF
!
!----------------------------------------------------------------------
!***  RADIATION
!----------------------------------------------------------------------
!
      IF(MOD(NTSD,NRADS).EQ.0.OR.MOD(NTSD,NRADL).EQ.0)THEN

        btimx=timef()
	
	do J=JTS,JTE
	do K=KTS,KTE-1
	do I=ITS,ITE
	if (T(I,K,J) .gt. 330. .or. T(I,K,J) .lt. 180) then
	write(0,*) 'strange T into RAD...I,K,J,T(I,K,J): ', I,K,J,T(I,K,J)
	endif
	enddo
	enddo
	enddo

        CALL RADIATION(NTSD,DT,JULDAY,JULYR,IHRST,NPHS,GLAT,GLON       &
     &                ,NRADS,NRADL                                     &
     &                ,DETA1,DETA2,AETA1,AETA2,ETA1,ETA2,PDTOP,PT      &
     &                ,PD,RES,PINT,T,Q,CWM,THS,ALBEDO,EPSR             &
     &                ,F_ICE,F_RAIN                                    &
!    &                ,SM,HBM2,LMH,ZERO_3D,MOIST,N_MOIST,RESTRT        &
     &                ,SM,HBM2,LMH,ZERO_3D,N_MOIST,RESTRT              &
     &                ,RLWTT,RSWTT,RLWIN,RSWIN,RSWOUT                  &
     &                ,TOTSWDN,TOTLWDN,RLWTOA,RSWTOA,CZMEAN            &
     &                ,CFRACL,CFRACM,CFRACH,SIGT4                      &
     &                ,ACFRST,NCFRST,ACFRCV,NCFRCV                     &
     &                ,CUPPT,VEGFRC,SNO,HTOP,HBOT                      &
     &                ,CONFIG_FLAGS                                    &
     &                ,IDS,IDF,JDS,JDF,KDS,KDE                         &
     &                ,IMS,IME,JMS,JME,KMS,KME                         &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)
        radiation_tim=radiation_tim+timef()-btimx

	do J=JTS,JTE
	do I=ITS,ITE
	if (CFRACL(I,J) .gt. 0 .and. CFRACL(I,J) .lt. 5000.) then
!	write(0,*) CFRACL: , I,J,CFRACL(I,J)
	endif
	enddo
	enddo

      ENDIF
!
!----------------------------------------------------------------------
!***  APPLY TEMPERATURE TENDENCY DUE TO RADIATION
!----------------------------------------------------------------------
!
      btimx=timef()

      CALL RDTEMP(NTSD,DT,JULDAY,JULYR,IHRST,GLAT,GLON                 &
     &           ,CZEN,CZMEAN,T,RSWTT,RLWTT,HTM,HBM2                   &
     &           ,IDS,IDF,JDS,JDF,KDS,KDE                              &
     &           ,IMS,IME,JMS,JME,KMS,KME                              &
     &           ,ITS,ITE,JTS,JTE,KTS,KTE)
      rdtemp_tim=rdtemp_tim+timef()-btimx

!
!----------------------------------------------------------------------
!***  TURBULENT PROCESSES 
!----------------------------------------------------------------------
!
      IF(MOD(NTSD,NPHS).EQ.0)THEN
        btimx=timef()

        CALL TURBL(GRID                                                &
     &            ,NTSD,DT,NPHS,RESTRT                                 &
     &            ,N_MOIST,NUM_SOIL_LAYERS,SLDPTH,DZSOIL               &
     &            ,DETA1,DETA2,AETA1,AETA2,ETA1,ETA2,PDTOP,PT          &
!old     &            ,SM,LMH,HTM,VTM,HBM2,VBM2,DX_NMM,DFL                 &
     &            ,SM,LMH,HTM,VTM,HBM2,VBM2,DX_NMM,DFRLG               &
     &            ,CZEN,CZMEAN,SIGT4,TOTLWDN,TOTSWDN,RADOT             &
     &            ,PD,RES,PINT,T,Q,CWM,F_ICE,F_RAIN,Q2,U,V             &
     &            ,THS,SST,PREC,SNO,ZERO_3D                            &
     &            ,FIS,Z0,Z0BASE,USTAR,PBLH,LPBL,AKHS,AKMS             &
     &            ,THZ0,QZ0,UZ0,VZ0,QS                                 &
     &            ,STC,SMC,CMC,SMSTAV,SMSTOT,SSROFF,BGROFF             &
     &            ,IVGTYP,ISLTYP,VEGFRC,SHDMIN,SHDMAX,GRNFLX           &
     &            ,SFCEXC,ACSNOW,ACSNOM,SNOPCX,SICE,TG,SOILTB          &
     &            ,ALBASE,MXSNAL,ALBEDO,SH2O,SI,EPSR                   &
     &            ,U10,V10,TH10,Q10,TSHLTR,QSHLTR,PSHLTR               &
     &            ,TWBS,QWBS,SFCSHX,SFCLHX,SFCEVP                      &
     &            ,POTEVP,POTFLX,SUBSHX                                &
     &            ,APHTIM,ARDSW,ARDLW,ASRFC                            &
     &            ,RSWOUT,RSWTOA,RLWTOA                                &
     &            ,ASWIN,ASWOUT,ASWTOA,ALWIN,ALWOUT,ALWTOA             &
     &            ,UZ0H,VZ0H,DUDT,DVDT                                 & !jm
     &            ,CONFIG_FLAGS                                        &
     &            ,IHE,IHW,IVE,IVW                                     &
     &            ,IDS,IDF,JDS,JDF,KDS,KDE                             &
     &            ,IMS,IME,JMS,JME,KMS,KME                             &
     &            ,ITS,ITE,JTS,JTE,KTS,KTE)
        turbl_tim=turbl_tim+timef()-btimx

!----------------------------------------------------------------------
!*** STORE ORIGINAL TEMPERATURE ARRAY
!----------------------------------------------------------------------

        btimx=timef()
        exch_tim=exch_tim+timef()-btimx

        ICLTEND=-1
        CALL wrf_debug ( 100 , 'nmm: in cltend' )

        btimx=timef()
 
        CALL CLTEND(ICLTEND,NPHS,T,T_OLD,T_ADJ                         &
                   ,IDS,IDF,JDS,JDF,KDS,KDE                            &
                   ,IMS,IME,JMS,JME,KMS,KME                            &
                   ,ITS,ITE,JTS,JTE,KTS,KTE)
        cltend_tim=cltend_tim+timef()-btimx
        CALL wrf_debug ( 100 , 'nmm: out of cltend' )
      ENDIF
!
!----------------------------------------------------------------------
!***  CONVECTIVE PRECIPITATION
!----------------------------------------------------------------------
      IF(MOD(NTSD,NCNVC).EQ.0.AND.                                     &
     &   CONFIG_FLAGS%CU_PHYSICS.EQ.KFETASCHEME)THEN
        btimx=timef()
        exch_tim=exch_tim+timef()-btimx
      ENDIF
      btimx=timef()

	if (NCNVC .ne. 999) then
      CALL CUCNVC(NTSD,DT,NCNVC,GPS,RESTRT,HYDRO                       &
!    &           ,CLDEFI,LMH,MOIST,N_MOIST,ENSDIM                      &
     &           ,CLDEFI,LMH,N_MOIST,ENSDIM                            &
     &           ,DETA1,DETA2,AETA1,AETA2,ETA1,ETA2                    &
     &           ,PDTOP,PT,PD,RES,PINT,T,Q,TCUCN                       &
     &           ,OMGALF,U,V,VTM,W,Z,FIS,W0AVG                         &
     &           ,PREC,ACPREC,CUPREC,CUPPT                             &
     &           ,SM,HBM2,LPBL,HBOT,HTOP,CNVBOT,CNVTOP                 &
     &           ,AVCNVC,ACUTIM,ZERO_3D,IHE,IHW                        &
     &           ,CONFIG_FLAGS                                         &
     &           ,IDS,IDF,JDS,JDF,KDS,KDE                              &
     &           ,IMS,IME,JMS,JME,KMS,KME                              &
     &           ,ITS,ITE,JTS,JTE,KTS,KTE)
      cucnvc_tim=cucnvc_tim+timef()-btimx
	else
	write(0,*) 'skipped CUCNVC'
	endif
!----------------------------------------------------------------------
!***  GRIDSCALE MICROPHYSICS (CONDENSATION & PRECIPITATION)
!----------------------------------------------------------------------
!
      IF(MOD(NTSD,NPHS).EQ.0)THEN
        btimx=timef()
!       CALL GSMDRIVE(NTSD,DT,NPHS,MOIST,N_MOIST                       &
        CALL GSMDRIVE(NTSD,DT,NPHS,N_MOIST                             &
     &               ,DX_NMM(ITS,JC),DY,LMH,SM,HBM2,FIS                &
     &               ,DETA1,DETA2,AETA1,AETA2,ETA1,ETA2                &
     &               ,PDTOP,PT,PD,RES,PINT,T,Q,CWM,TRAIN               &
     &               ,F_ICE,F_RAIN,F_RIMEF,SR                          &
     &               ,PREC,ACPREC,ZERO_3D                              &
     &               ,CONFIG_FLAGS                                     &
     &               ,IDS,IDF,JDS,JDF,KDS,KDE                          &
     &               ,IMS,IME,JMS,JME,KMS,KME                          &
     &               ,ITS,ITE,JTS,JTE,KTS,KTE)
      gsmdrive_tim=gsmdrive_tim+timef()-btimx
!----------------------------------------------------------------------
!***  CALCULATE TEMP TENDENCIES AND RESTORE ORIGINAL TEMPS
!----------------------------------------------------------------------
!       btim=timef()
        ICLTEND=0
        CALL wrf_debug ( 100 , 'nmm: in cltend' )

        btimx=timef()

        CALL CLTEND(ICLTEND,NPHS,T,T_OLD,T_ADJ                         &
     &             ,IDS,IDF,JDS,JDF,KDS,KDE                            &
     &             ,IMS,IME,JMS,JME,KMS,KME                            &
     &             ,ITS,ITE,JTS,JTE,KTS,KTE)
        cltend_tim=cltend_tim+timef()-btimx
        CALL wrf_debug ( 100 , 'nmm: out of cltend' )
      ENDIF
!
!----------------------------------------------------------------------
!***  UPDATE TEMP TENDENCIES FROM CLOUD PROCESSES EVERY TIME STEP
!----------------------------------------------------------------------
!
      ICLTEND=1
      CALL wrf_debug ( 100 , 'nmm: in cltend' )

      btimx=timef()

      CALL CLTEND(ICLTEND,NPHS,T,T_OLD,T_ADJ                           &
     &           ,IDS,IDF,JDS,JDF,KDS,KDE                              &
     &           ,IMS,IME,JMS,JME,KMS,KME                              &
     &           ,ITS,ITE,JTS,JTE,KTS,KTE)
      cltend_tim=cltend_tim+timef()-btimx
      CALL wrf_debug ( 100 , 'nmm: out of cltend' )
!
!----------------------------------------------------------------------
!***  LATERAL DIFFUSION
!----------------------------------------------------------------------
!
      btimx=timef()
      exch_tim=exch_tim+timef()-btimx

      CALL wrf_debug ( 100 , 'nmm: in hdiff' )

      btimx=timef()
	do J=JTS,JTE
	do K=KTS,KTE-1
	do I=ITS,ITE
	if (T(I,K,J) .gt. 330. .or. T(I,K,J) .lt. 180) then
	write(0,*) 'strange T into HDIFF...I,K,J,T(I,K,J): ', I,K,J,T(I,K,J)
	endif
	if ( abs(U(I,K,J)) .gt. 120.) then
	write(0,*) 'strange U, into HDIFF..I,K,J,U(I,K,J): ', I,K,J,U(I,K,J)
	endif
	if ( abs(V(I,K,J)) .gt. 120.) then
	write(0,*) 'strange V, into HDIFF..I,K,J,V(I,K,J): ', I,K,J,V(I,K,J)
	endif
	enddo
	enddo
	enddo

	if (ITS .lt. 60 .and. ITE .ge. 90 .and. &
                  JTS .lt. 450 .and. JTE .eq. 501 .and. &
                (NTSD .gt. 8500 .or. mod(NTSD,20) .eq. 0)) then

!	write(0,*) T into HDIFF
!	do J=494,488,-1
!	write(0,633) (T(I,2,J),I=68,73)
!	enddo

!	write(0,*) U into HDIFF
!	do J=494,488,-1
!	write(0,634) (U(I,2,J),I=68,73)
!	enddo

!	write(0,*) V into HDIFF
!	do J=494,488,-1
!	write(0,634) (V(I,2,J),I=68,73)
!	enddo

!	do K=KTS,KTE-1
!	write(0,*) K, T(70,K,490): , K, T(70,K,490)
!	enddo

	endif

   633	format(6(f6.1,1x))
   634	format(6(f5.1,1x))

	
!	goto 979

      CALL HDIFF(NTSD,DT,FIS,DY_NMM,HDAC,HDACV                         &
     &          ,HTM,HBM2,VTM,DETA1,SIGMA                              &
     &          ,T,Q,U,V,Q2,Z,W                                        &
!oldstyle     &          ,T,Q,U,V,Q2,Z                                    &
     &          ,IHE,IHW,IVE,IVW,INDX3_WRK                             &
     &          ,IDS,IDF,JDS,JDF,KDS,KDE                               &
     &          ,IMS,IME,JMS,JME,KMS,KME                               &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE)

!	if (ITS .lt. 60 .and. ITE .ge. 90 .and. &
!                             JTS .lt. 450 .and. JTE .eq. 501) then
!	write(0,*) T out of HDIFF
!	do J=494,488,-1
!	write(0,633) (T(I,2,J),I=68,73)
!	enddo
!	endif

  979	continue

	do J=JTS,JTE
	do K=KTS,KTE-1
	do I=ITS,ITE
	if (T(I,K,J) .gt. 330. .or. T(I,K,J) .lt. 180) then
	write(0,*) 'strange T exiting HDIFF...I,K,J,T(I,K,J): ', I,K,J,T(I,K,J)
	endif
	if ( abs(U(I,K,J)) .gt. 120.) then
	write(0,*) 'strange U, exiting HDIFF..I,K,J,U(I,K,J): ', I,K,J,U(I,K,J)
	endif
	if ( abs(V(I,K,J)) .gt. 120.) then
	write(0,*) 'strange V, exiting HDIFF..I,K,J,V(I,K,J): ', I,K,J,V(I,K,J)
	endif
	enddo
	enddo
	enddo
      hdiff_tim=hdiff_tim+timef()-btimx
      CALL wrf_debug ( 100 , 'nmm: out of hdiff' )
!
!----------------------------------------------------------------------
!***  UPDATING BOUNDARY VALUES AT HEIGHT POINTS
!----------------------------------------------------------------------
!
      CALL wrf_debug ( 100 , 'nmm: in bocoh' )

      btimx=timef()
      exch_tim=exch_tim+timef()-btimx

      btimx=timef()

      CALL BOCOH(NTSD,DT,NEST,NUNIT_NBC,NBOCO,LAST_TIME,TSPH           &
     &          ,LB,ETA1,ETA2,PDTOP,PT,RES,HTM                         &
     &          ,PD_B,T_B,Q_B,U_B,V_B,Q2_B,CWM_B                       &
     &          ,PD_BT,T_BT,Q_BT,U_BT,V_BT,Q2_BT,CWM_BT                &
     &          ,PD,T,Q,Q2,CWM,PINT                                    &
     &          ,IJDS,IJDE,SPEC_BDY_WIDTH                              &
     &          ,IHE,IHW,IVE,IVW,INDX3_WRK                             &
     &          ,IDS,IDF,JDS,JDF,KDS,KDE                               &
     &          ,IMS,IME,JMS,JME,KMS,KME                               &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE)
      bocoh_tim=bocoh_tim+timef()-btimx
!

      CALL wrf_debug ( 100 , 'nmm: out of bocoh' )
!
!----------------------------------------------------------------------
!***  IS IT TIME FOR A CHECK POINT ON THE MODEL HISTORY FILE?
!----------------------------------------------------------------------
!
 2003 CONTINUE

	do J=JTS,JTE
	do K=KTS,KTE-1
	do I=ITS,ITE
	if (T(I,K,J) .gt. 330. .or. T(I,K,J) .lt. 180) then
	write(0,*) 'strange T initial...I,K,J,T(I,K,J): ', I,K,J,T(I,K,J)
	endif
	enddo
	enddo
	enddo
!
!----------------------------------------------------------------------
!***  PRESSURE GRD, CORIOLIS, DIVERGENCE, AND HORIZ PART OF OMEGA-ALPHA
!----------------------------------------------------------------------
!
      btimx=timef()
      exch_tim=exch_tim+timef()-btimx

      CALL wrf_debug ( 100 , 'nmm: in pfdht' )
!
      btimx=timef()

      CALL PFDHT(NTSD,LAST_TIME,PT,DETA1,DETA2,PDTOP,RES,FIS           &
     &          ,HYDRO,SIGMA,FIRST,DX_NMM,DY_NMM                       &
     &          ,HTM,HBM2,VTM,VBM2,VBM3                                &
     &          ,FDIV,FCP,WPDAR,DFL,CPGFU,CPGFV                        &
     &          ,PD,PDSL,T,Q,U,V,CWM,OMGALF,PINT,DWDT                  &
     &          ,RTOP,DIV,FEW,FNS,FNE,FSE                              &
     &          ,IHE,IHW,IVE,IVW,INDX3_WRK                             &
     &          ,IDS,IDF,JDS,JDF,KDS,KDE                               &
     &          ,IMS,IME,JMS,JME,KMS,KME                               &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE)
      pfdht_tim=pfdht_tim+timef()-btimx
	do J=JTS,JTE
	do K=KTS,KTE-1
	do I=ITS,ITE
	if (T(I,K,J) .gt. 330. .or. T(I,K,J) .lt. 180) then
	write(0,*) 'strange T post PFDHT...I,K,J,T(I,K,J): ', I,K,J,T(I,K,J)
	endif
	enddo
	enddo
	enddo
!
!----------------------------------------------------------------------
!***  DIVERGENCE DAMPING
!----------------------------------------------------------------------
!
      btimx=timef()
      exch_tim=exch_tim+timef()-btimx

      CALL wrf_debug ( 100 , 'nmm: in ddamp' )
      btimx=timef()
      CALL DDAMP(NTSD,DT,DETA1,DETA2,PDSL,PDTOP,DIV,HBM2,VTM           &
     &          ,T,U,V,DDMPU,DDMPV                                     &
     &          ,IHE,IHW,IVE,IVW,INDX3_WRK                             &
     &          ,IDS,IDF,JDS,JDF,KDS,KDE                               &
     &          ,IMS,IME,JMS,JME,KMS,KME                               &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE)
      ddamp_tim=ddamp_tim+timef()-btimx
      CALL wrf_debug ( 100 , 'nmm: out of ddamp' )
	do J=JTS,JTE
	do K=KTS,KTE-1
	do I=ITS,ITE
	if (T(I,K,J) .gt. 330. .or. T(I,K,J) .lt. 180) then
	write(0,*) 'strange T post DDAMP...I,K,J,T(I,K,J): ', I,K,J,T(I,K,J)
	endif
	enddo
	enddo
	enddo
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!
      IF(FIRST.AND.NTSD.EQ.0)THEN
        FIRST=.FALSE.
      btimx=timef()
      exch_tim=exch_tim+timef()-btimx
        GO TO 2000
      ENDIF
!
!----------------------------------------------------------------------
!***  UPDATING BOUNDARY VALUES AT VELOCITY POINTS
!----------------------------------------------------------------------
!
      btimx=timef()
      exch_tim=exch_tim+timef()-btimx

      CALL wrf_debug ( 100 , 'nmm: in bocov' )

      btimx=timef()
      CALL BOCOV(NTSD,DT,LB,VTM,U_B,V_B,U_BT,V_BT                      &
     &          ,U,V                                                   &
     &          ,IJDS,IJDE,SPEC_BDY_WIDTH                              &
     &          ,IHE,IHW,IVE,IVW,INDX3_WRK                             &
     &          ,IDS,IDF,JDS,JDF,KDS,KDE                               &
     &          ,IMS,IME,JMS,JME,KMS,KME                               &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE )
      bocov_tim=bocov_tim+timef()-btimx
        CALL wrf_debug ( 100 , 'nmm: out of bocov' )
      solve_tim=solve_tim+timef()-btim
!----------------------------------------------------------------------
	do J=JTS,JTE
	do K=KTS,KTE-1
	do I=ITS,ITE
	if (T(I,K,J) .gt. 330. .or. T(I,K,J) .lt. 180) then
	write(0,*) 'strange T end solve_nmm...I,K,J,T(I,K,J): ', I,K,J,T(I,K,J)
	endif
	if ( abs(U(I,K,J)) .gt. 120.) then
	write(0,*) 'strange U, end solve_nmm..I,K,J,U(I,K,J): ', I,K,J,U(I,K,J)
	endif
	if ( abs(V(I,K,J)) .gt. 120.) then
	write(0,*) 'strange V, end solve_nmm..I,K,J,V(I,K,J): ', I,K,J,V(I,K,J)
	endif
	enddo
	enddo
	enddo
      sum_tim=pdte_tim+adve_tim+vtoa_tim+vadz_tim+hadz_tim+eps_tim     &
     &       +vad2_tim+had2_tim+radiation_tim+rdtemp_tim+turbl_tim     &
     &       +cltend_tim+cucnvc_tim+gsmdrive_tim+hdiff_tim             &
     &       +bocoh_tim+pfdht_tim+ddamp_tim+bocov_tim+exch_tim
      write(0,*)' ntsd=',ntsd,' solve_tim=',solve_tim*1.e-3 &
     &         ,' sum_tim=',sum_tim*1.e-3
     if(ntsd.ge.7000 .and. mod(ntsd,100) .eq. 0)then
      write(0,*)' ntsd=',ntsd,' solve_tim=',solve_tim*1.e-3 &
     &         ,' sum_tim=',sum_tim*1.e-3
      write(0,*)' pdte_tim=',pdte_tim*1.e-3,' pct=',pdte_tim/sum_tim*100.
      write(0,*)' adve_tim=',adve_tim*1.e-3,' pct=',adve_tim/sum_tim*100.
      write(0,*)' vtoa_tim=',vtoa_tim*1.e-3,' pct=',vtoa_tim/sum_tim*100.
      write(0,*)' vadz_tim=',vadz_tim*1.e-3,' pct=',vadz_tim/sum_tim*100.
      write(0,*)' hadz_tim=',hadz_tim*1.e-3,' pct=',hadz_tim/sum_tim*100.
      write(0,*)' eps_tim=',eps_tim*1.e-3,' pct=',eps_tim/sum_tim*100.
      write(0,*)' vad2_tim=',vad2_tim*1.e-3,' pct=',vad2_tim/sum_tim*100.
      write(0,*)' had2_tim=',had2_tim*1.e-3,' pct=',had2_tim/sum_tim*100.
      write(0,*)' radiation_tim=',radiation_tim*1.e-3,' pct=',radiation_tim/sum_tim*100.
      write(0,*)' rdtemp_tim=',rdtemp_tim*1.e-3,' pct=',rdtemp_tim/sum_tim*100.
      write(0,*)' turbl_tim=',turbl_tim*1.e-3,' pct=',turbl_tim/sum_tim*100.
      write(0,*)' cltend_tim=',cltend_tim*1.e-3,' pct=',cltend_tim/sum_tim*100.
      write(0,*)' cucnvc_tim=',cucnvc_tim*1.e-3,' pct=',cucnvc_tim/sum_tim*100.
      write(0,*)' gsmdrive_tim=',gsmdrive_tim*1.e-3,' pct=',gsmdrive_tim/sum_tim*100.
      write(0,*)' hdiff_tim=',hdiff_tim*1.e-3,' pct=',hdiff_tim/sum_tim*100.
      write(0,*)' bocoh_tim=',bocoh_tim*1.e-3,' pct=',bocoh_tim/sum_tim*100.
      write(0,*)' pfdht_tim=',pfdht_tim*1.e-3,' pct=',pfdht_tim/sum_tim*100.
      write(0,*)' ddamp_tim=',ddamp_tim*1.e-3,' pct=',ddamp_tim/sum_tim*100.
      write(0,*)' bocov_tim=',bocov_tim*1.e-3,' pct=',bocov_tim/sum_tim*100.
      write(0,*)' exch_tim=',exch_tim*1.e-3,' pct=',exch_tim/sum_tim*100.
      endif
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

      Return
!----------------------------------------------------------------------
!**********************************************************************
!**********************************************************************
!*************    EXIT FROM THE TIME LOOP    **************************
!**********************************************************************
!**********************************************************************
!----------------------------------------------------------------------
      END SUBROUTINE SOLVE_NMM
!----------------------------------------------------------------------
!----------------------------------------------------------------------
      SUBROUTINE TWR(ARRAY,KK,FIELD,NTSD,MYPE,NPES,MPI_COMM_COMP       &
     &              ,IDS,IDE,JDS,JDE,KDS,KDE                           &
     &              ,IMS,IME,JMS,JME,KMS,KME                           &
     &              ,ITS,ITE,JTS,JTE,KTS,KTE)
!**********************************************************************
      USE MODULE_EXT_INTERNAL
!
      INCLUDE "mpif.h"
!----------------------------------------------------------------------
      IMPLICIT NONE
!----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE                    &
     &                     ,KK,MPI_COMM_COMP,MYPE,NPES,NTSD
!
      REAL,DIMENSION(IMS:IME,KMS:KME+KK,JMS:JME),INTENT(IN) :: ARRAY
!
      CHARACTER(*),INTENT(IN) :: FIELD
!
!*** LOCAL VARIABLES
!
      INTEGER,DIMENSION(MPI_STATUS_SIZE) :: JSTAT
      INTEGER,DIMENSION(MPI_STATUS_SIZE,4) :: STATUS_ARRAY
      INTEGER,DIMENSION(2) :: IM_REM,JM_REM,IT_REM,JT_REM
!
      INTEGER :: I,IENDX,IER,IPE,IRECV,IRTN,ISEND,IUNIT                &
     &          ,J,K,N,NLEN,NSIZE
      INTEGER :: ITS_REM,ITE_REM,JTS_REM,JTE_REM
!
      REAL,DIMENSION(IDS:IDE,JDS:JDE) :: TWRITE
      REAL,ALLOCATABLE,DIMENSION(:) :: VALUES
      CHARACTER(5) :: TIMESTEP
      CHARACTER(6) :: FMT
      CHARACTER(12) :: FILENAME
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
!
      IF(NTSD.LE.9)THEN
        FMT='(I1.1)'
        NLEN=1
      ELSEIF(NTSD.LE.99)THEN
        FMT='(I2.2)'
        NLEN=2
      ELSEIF(NTSD.LE.999)THEN
        FMT='(I3.3)'
        NLEN=3
      ELSEIF(NTSD.LE.9999)THEN
        FMT='(I4.4)'
        NLEN=4
      ELSEIF(NTSD.LE.99999)THEN
        FMT='(I5.5)'
        NLEN=5
      ENDIF
      WRITE(TIMESTEP,FMT)NTSD
      FILENAME=FIELD//'_'//TIMESTEP(1:NLEN)
!
      IF(MYPE.EQ.0)THEN
        CALL INT_GET_FRESH_HANDLE(IUNIT)
        CLOSE(IUNIT)
        OPEN(UNIT=IUNIT,FILE=FILENAME,FORM='UNFORMATTED',IOSTAT=IER)
      ENDIF
!
!----------------------------------------------------------------------
!!!!  DO 500 K=KTS,KTE+KK     !Unflipped
!!!!  DO 500 K=KTE+KK,KTS,-1
      DO 500 K=KDE-1,KDS,-1   !Write LM layers top down for checking
!----------------------------------------------------------------------
!
      IF(MYPE.EQ.0)THEN
        DO J=JTS,JTE
        DO I=ITS,ITE
          TWRITE(I,J)=ARRAY(I,K,J)
        ENDDO
        ENDDO
!
        DO IPE=1,NPES-1
          CALL MPI_RECV(IT_REM,2,MPI_INTEGER,IPE,IPE                    &
     &                 ,MPI_COMM_COMP,JSTAT,IRECV)
          CALL MPI_RECV(JT_REM,2,MPI_INTEGER,IPE,IPE                    &
     &                 ,MPI_COMM_COMP,JSTAT,IRECV)
!
          ITS_REM=IT_REM(1)
          ITE_REM=IT_REM(2)
          JTS_REM=JT_REM(1)
          JTE_REM=JT_REM(2)
!
          NSIZE=(ITE_REM-ITS_REM+1)*(JTE_REM-JTS_REM+1)
          ALLOCATE(VALUES(1:NSIZE))
!
          CALL MPI_RECV(VALUES,NSIZE,MPI_REAL,IPE,IPE                   &
     &                 ,MPI_COMM_COMP,JSTAT,IRECV)
          N=0
          DO J=JTS_REM,JTE_REM
            DO I=ITS_REM,ITE_REM
              N=N+1
              TWRITE(I,J)=VALUES(N)
            ENDDO
          ENDDO
!
          DEALLOCATE(VALUES)
!
        ENDDO
!
!----------------------------------------------------------------------
      ELSE
        NSIZE=(ITE-ITS+1)*(JTE-JTS+1)
        ALLOCATE(VALUES(1:NSIZE))
!
        N=0
        DO J=JTS,JTE
        DO I=ITS,ITE
          N=N+1
          VALUES(N)=ARRAY(I,K,J)
        ENDDO
        ENDDO
!
        IT_REM(1)=ITS
        IT_REM(2)=ITE
        JT_REM(1)=JTS
        JT_REM(2)=JTE
!
        CALL MPI_SEND(IT_REM,2,MPI_INTEGER,0,MYPE                       &
     &               ,MPI_COMM_COMP,ISEND)
        CALL MPI_SEND(JT_REM,2,MPI_INTEGER,0,MYPE                       &
     &               ,MPI_COMM_COMP,ISEND)
!
        CALL MPI_SEND(VALUES,NSIZE,MPI_REAL,0,MYPE                      &
     &               ,MPI_COMM_COMP,ISEND)
!
        DEALLOCATE(VALUES)
!
      ENDIF
!----------------------------------------------------------------------
!
      CALL MPI_BARRIER(MPI_COMM_COMP,IRTN)
!
      IF(MYPE.EQ.0)THEN
!
        DO J=JDS,JDE-1
          IENDX=IDE-1
          IF(MOD(J,2).EQ.0)IENDX=IENDX-1
          WRITE(IUNIT)(TWRITE(I,J),I=1,IENDX)
        ENDDO
!
      ENDIF
!
!----------------------------------------------------------------------
  500 CONTINUE
!
      IF(MYPE.EQ.0)CLOSE(IUNIT)
!----------------------------------------------------------------------
!
      END SUBROUTINE TWR
