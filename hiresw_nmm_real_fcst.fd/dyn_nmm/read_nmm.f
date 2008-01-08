!WRF:MEDIATION_LAYER:
!

SUBROUTINE med_read_nmm ( grid , config_flags , ntsd, dt_from_file, tstart_from_file, tend_from_file, &
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
  ! Driver layer
   USE module_domain
   USE module_io_domain
  ! Model layer
   USE module_configure
   USE module_bc_time_utilities
!----------------------------------------------------------------------

   IMPLICIT NONE

!----------------------------------------------------------------------

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

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

!----------------------------------------------------------------------
  ! Local

   REAL, DIMENSION(1:2*2500,2)           :: PDB
   REAL, DIMENSION(1:2*2500,grid%sd32:grid%ed32-1,2) :: TB,QB,UB,VB,Q2B,CWMB

   INTEGER :: NUNIT_PARMETA=10,NUNIT_FCSTDATA=11                  &
             ,NUNIT_NHB=12,NUNIT_CO2=14,NUNIT_Z0=22
   INTEGER :: NMAP,NRADSH,NRADLH,NTDDMP
   INTEGER :: IDS,IDE,JDS,JDE,KDS,KDE
   INTEGER :: IPS,IPE,JPS,JPE,KPS,KPE
   INTEGER :: IMS,IME,JMS,JME,KMS,KME
   INTEGER :: IM,JM,LM,NROOT,INPES,JNPES,NFCST,NUNIT_NBC,LISTB
   INTEGER :: I,J,K,IHRST,JAM,NTSD,IHRSTB,IHH,IHL
   INTEGER :: KBI,KBI2,LRECBC
   INTEGER :: N,ISTART,LB,NREC
   INTEGER,DIMENSION(3) :: IDAT,IDATB
   LOGICAL :: RESTRT,SINGLRST,NEST,RUN,RUNB
   REAL :: TSTART,TEND,TPREC,THEAT,TCLOD,TRDSW,TRDLW,TSRFC
   REAL :: BCHR,TSTEPS,TSPH,TBOCO
   REAL,DIMENSION(39) :: SPL
   REAL,DIMENSION(99) :: TSHDE
   REAL,ALLOCATABLE,DIMENSION(:) :: TEMP1
   REAL,ALLOCATABLE,DIMENSION(:,:) :: TEMP
   INTEGER,ALLOCATABLE,DIMENSION(:,:) :: ITEMP
   REAL,ALLOCATABLE,DIMENSION(:,:,:) :: HOLD
   REAL ::                      TDDAMP                     &
                                            ,ETA
   REAL :: PLQ,RDPQ,RDTHEQ,STHEQ,THE0Q
   REAL :: ROS,CS,DS,ROI,CI,DI                             &
          ,PL,THL,RDQ,RDTH,RDP,RDTHE                       &
                ,QS0,SQS,STHE,THE0
!!!tlb   REAL :: PTBL,TTBL                                       &
   REAL :: WBD,SBD,TLM0D,TPH0D,R,        CMLD,DP30               &
    ,X1P,Y1P,IXM,IYM
   INTEGER :: NN, mype
   REAL :: dt_from_file
   REAL :: tstart_from_file, tend_from_file
   real :: dtx


!**********************************************************************
!
!***  Temporary fix for reading in lookup tables
!
   INTEGER,PARAMETER :: ITB=76,JTB=134,ITBQ=152,JTBQ=440
   REAL,DIMENSION(ITB,JTB) :: PTBL
   REAL,DIMENSION(JTB,ITB) :: TTBL
   REAL,DIMENSION(JTBQ,ITBQ) :: TTBLQ
!**********************************************************************
   CHARACTER*256 mess
!----------------------------------------------------------------------
! small file with global dimensions
   NAMELIST /PARMNMM/ IM,JM,LM,NSOIL,NROOT,INPES,JNPES
!
! another small file with forecast parameters
   NAMELIST /FCSTDATA/                                            &
    TSTART,TEND,RESTRT,SINGLRST,NMAP,TSHDE,SPL                    &
   ,NPHS,NCNVC,NRADSH,NRADLH,NTDDMP                               &
   ,TPREC,THEAT,TCLOD,TRDSW,TRDLW,TSRFC                           &
   ,NEST,HYDRO
!----------------------------------------------------------------------
!**********************************************************************
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

!
   REWIND NUNIT_PARMETA
   READ(NUNIT_PARMETA,PARMNMM)
   NSOIL=4
      write(0,*)' assigned nsoil=',nsoil
   CALL wrf_debug ( 100 , 'nmm: read global dimensions file' )

! temporarily produce array limits here
!   IDS=1
!   IDE=IM
!   JDS=1
!   JDE=JM
!   KDS=1
!   KDE=LM

!----------------------------------------------------------------------
  CALL get_ijk_from_grid (  grid ,                   &
                            ids, ide, jds, jde, kds, kde,    &
                            ims, ime, jms, jme, kms, kme,    &
                            ips, ipe, jps, jpe, kps, kpe    )

! GLOBAL GRID DIMS ARE WHAT WRF CONSIDERS UNSTAGGERED
   ide = ide - 1
   jde = jde - 1
   kde = kde - 1
   NSOIL=4

   CALL wrf_debug(100,'in mediation_read_nmm')
   WRITE(mess,*)'ids,ide,jds,jde,kds,kde ',ids,ide,jds,jde,kds,kde
   CALL wrf_debug(100,mess)

!----------------------------------------------------------------------
! read constants file
      write(0,*)' before allocates and nhb nsoil=',nsoil
   ALLOCATE(TEMP1(1:NSOIL),STAT=I)
   ALLOCATE(ITEMP(IDS:IDE,JDS:JDE),STAT=I)
   ALLOCATE(TEMP(IDS:IDE,JDS:JDE),STAT=I)
   ALLOCATE(HOLD(IDS:IDE,JDS:JDE,KDS:KDE),STAT=I)
!
!----------------------------------------------------------------------
! read z0 file
      READ(NUNIT_Z0)TEMP
      DO J=JDS,JDE
      DO I=IDS,IDE
        Z0(I,J)=TEMP(I,J)
      ENDDO
      ENDDO
!----------------------------------------------------------------------
!
   READ(NUNIT_NHB) NFCST,NUNIT_NBC,LISTB,DT,IDTAD,SIGMA
      write(0,*)' read_nmm sigma=',sigma
   dt_from_file = dt
   WRITE( mess, * ) 'NFCST = ',NFCST,'  DT  = ',DT
   WRITE( 0, * ) 'NFCST = ',NFCST,'  DT  = ',DT,' NHB=',NUNIT_NHB 
   CALL wrf_debug(100, mess)
!----------------------------------------------------------------------
   READ(NUNIT_NHB) ITEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       LMH(I,J)=ITEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   READ(NUNIT_NHB) ITEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       LMV(I,J)=ITEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       HBM2(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   DO J=JDS,JDE
     DO I=IDS,IDE
       HBM3(I,J)=0.
     ENDDO
   ENDDO
!
   DO J=JDS,JDE
     IHWG(J)=MOD(J+1,2)-1
     IF(J.GE.JDS+3.AND.J.LE.JDE-3)THEN
       IHL=2-IHWG(J)
!      IHWG=MOD(J+1,2)-1
!      IHL=2-IHWG
       IHL=2-IHWG(J)
       IHH=IDE-2
       DO I=IDS,IDE
         IF(I.GE.IHL.AND.I.LE.IHH)HBM3(I,J)=1.
       ENDDO
     ENDIF
   ENDDO
!----------------------------------------------------------------------
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       VBM2(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       VBM3(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       SM(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       SICE(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   DO K=KDE,KDS,-1
     READ(NUNIT_NHB)((HOLD(I,J,K),I=IDS,IDE),J=JDS,JDE)
   ENDDO
   CALL wrf_debug ( 100 , 'nmm: read HTM into HOLD' )
   DO K=KDS,KDE
     DO J=JDS,JDE
       DO I=IDS,IDE
         HTM(I,K,J)=HOLD(I,J,K)
       ENDDO
     ENDDO
   ENDDO
   CALL wrf_debug ( 100 , 'nmm: read of record' )
!----------------------------------------------------------------------
   DO K=KDE,KDS,-1
     READ(NUNIT_NHB)((HOLD(I,J,K),I=IDS,IDE),J=JDS,JDE)
   ENDDO
   CALL wrf_debug ( 100 , 'nmm: read VTM into HOLD' )
   DO K=KDS,KDE
     DO J=JDS,JDE
       DO I=IDS,IDE
         VTM(I,K,J)=HOLD(I,J,K)
       ENDDO
     ENDDO
   ENDDO
   CALL wrf_debug ( 100 , 'nmm: read VTM' )
!----------------------------------------------------------------------
   JAM=6+2*(JDE-JDS-9)
   READ(NUNIT_NHB)DY_NMM,CPGFV,EN,ENT,R,PT,TDDAMP                 &
                 ,F4D,F4Q,EF4T,PDTOP                              &
                 ,(DETA(KME-K),K=KMS,KME-1)                       &
                 ,(AETA(KME-K),K=KMS,KME-1)                       &
                 ,(F4Q2(KME-K),K=KMS,KME-1)                       &
                 ,(ETAX(KME+1-K),K=KMS,KME)                       &
                 ,(DFL(KME+1-K),K=KMS,KME)                        &
                 ,(DETA1(KME-K),K=KMS,KME-1)                      &
                 ,(AETA1(KME-K),K=KMS,KME-1)                      &
                 ,(ETA1(KME+1-K),K=KMS,KME)                       &
                 ,(DETA2(KME-K),K=KMS,KME-1)                      &
                 ,(AETA2(KME-K),K=KMS,KME-1)                      &
                 ,(ETA2(KME+1-K),K=KMS,KME)                       &
                 ,(EM(K),K=1,JAM)                                 &
                 ,(EMT(K),K=1,JAM)
   CALL wrf_debug ( 100 , 'nmm: read NMM_DX_NMM' )
!----------------------------------------------------------------------
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       DX_NMM(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   CALL wrf_debug ( 100 , 'nmm: read NMM_WPDAR' )
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       WPDAR(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   CALL wrf_debug ( 100 , 'nmm: read NMM_CPGFU' )
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       CPGFU(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   CALL wrf_debug ( 100 , 'nmm: read NMM_CURV' )
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       CURV(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   CALL wrf_debug ( 100 , 'nmm: read NMM_FCP' )
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       FCP(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   READ(NUNIT_NHB) TEMP
   CALL wrf_debug ( 100 , 'nmm: read NMM_FDIV' )
   DO J=JDS,JDE
     DO I=IDS,IDE
       FDIV(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   READ(NUNIT_NHB) TEMP
   CALL wrf_debug ( 100 , 'nmm: read NMM_FAD' )
   DO J=JDS,JDE
     DO I=IDS,IDE
       FAD(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   CALL wrf_debug ( 100 , 'nmm: read NMM_F' )
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       F(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   CALL wrf_debug ( 100 , 'nmm: read NMM_DDMPU' )
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       DDMPU(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   CALL wrf_debug ( 100 , 'nmm: read NMM_DDMPV' )
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       DDMPV(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   CALL wrf_debug ( 100 , 'nmm: read NMM_GLAT' )
   READ(NUNIT_NHB) PT, TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       GLAT(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   CALL wrf_debug ( 100 , 'nmm: read NMM_GLON' )
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       GLON(I,J)=-TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   CALL wrf_debug ( 100 , 'nmm: read PLQ,RDPQ,RDTHEQ,STHEQ,THE0Q' )
   READ(NUNIT_NHB)PLQ,RDPQ,RDTHEQ,STHEQ,THE0Q
!                ,(STHEQ(K),K=1,ITBQ)                             &
!                ,(THE0Q(K),K=1,ITBQ)
!----------------------------------------------------------------------
   CALL wrf_debug ( 100 , 'nmm: read ROS,CS,DS,ROI,CI,DI' )
   READ(NUNIT_NHB)ROS,CS,DS,ROI,CI,DI                             &
           ,PL,THL,RDQ,RDTH,RDP,RDTHE                             &
           ,(DETA(KME-K),K=KMS,KME-1)                             &
           ,(AETA(KME-K),K=KMS,KME-1)                             &
           ,(DFRLG(KME+1-K),K=KMS,KME)                            &
           ,(DETA1(KME-K),K=KMS,KME-1)                            &
           ,(AETA1(KME-K),K=KMS,KME-1)                            &
           ,(DETA2(KME-K),K=KMS,KME-1)                            &
           ,(AETA2(KME-K),K=KMS,KME-1)                            &
           ,QS0,SQS,STHE,THE0
!          ,(QS0(K),K=1,JTB)                                      &
!          ,(SQS(K),K=1,JTB)                                      &
!          ,(STHE(K),K=1,ITB)                                     &                
!          ,(THE0(K),K=1,ITB)
!----------------------------------------------------------------------
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       MXSNAL(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       EPSR(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       TG(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       GFFC(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       SST(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       ALBASE(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       HDAC(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       HDACV(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
!!!tlb   READ(NUNIT_NHB) TEMP
    READ(NUNIT_NHB) TTBLQ
!   DO J=JDS,JDE
!     DO I=IDS,IDE
!       TTBLQ(I,J)=TEMP(I,J)
!     ENDDO
!   ENDDO
!----------------------------------------------------------------------
   CALL wrf_debug ( 100 , 'nmm: read PTBL,TTBL' )
        READ(NUNIT_NHB)PTBL,TTBL                                       &
                ,R,PT,TSPH                                             &
                ,WBD,SBD,TLM0D,TPH0D,DLMD,DPHD,CMLD,DP30               &
                ,X1P,Y1P,IXM,IYM                                       &
                ,(DETA(KME-K),K=KMS,KME-1)                             &
                ,(AETA(KME-K),K=KMS,KME-1)                             &
                ,(ETAX(KME+1-K),K=KMS,KME)                             &
                ,(DETA1(KME-K),K=KMS,KME-1)                            &
                ,(AETA1(KME-K),K=KMS,KME-1)                            &
                ,(ETA1(KME+1-K),K=KMS,KME)                             &
                ,(DETA2(KME-K),K=KMS,KME-1)                            &
                ,(AETA2(KME-K),K=KMS,KME-1)                            &
                ,(ETA2(KME+1-K),K=KMS,KME)                              
!----------------------------------------------------------------------
   READ(NUNIT_NHB) ITEMP
    DO J=JDS,JDE
      DO I=IDS,IDE
        IVGTYP(I,J)=ITEMP(I,J)
      ENDDO
    ENDDO
!----------------------------------------------------------------------
   READ(NUNIT_NHB) ITEMP
    DO J=JDS,JDE
      DO I=IDS,IDE
        ISLTYP(I,J)=ITEMP(I,J)
      ENDDO
    ENDDO
!----------------------------------------------------------------------
   READ(NUNIT_NHB) ITEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       ISLOPE(I,J)=ITEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       VEGFRC(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   READ(NUNIT_NHB) (SLDPTH(N),N=1,NSOIL)
!----------------------------------------------------------------------
   READ(NUNIT_NHB) (RTDPTH(N),N=1,NSOIL)
!----------------------------------------------------------------------
   CALL wrf_debug ( 100 , 'nmm: read constants file' )

   REWIND NUNIT_FCSTDATA
   READ(NUNIT_FCSTDATA,FCSTDATA)
   tstart_from_file = tstart
   tend_from_file   = tend
   CALL wrf_debug ( 100 , 'nmm: read forecast parameters file' )
!----------------------------------------------------------------------

   nrads = nint(nradsh*tsph)
   nradl = nint(nradlh*tsph)
!----------------------------------------------------------------------
!
! INITIAL CONDITIONS
!
!----------------------------------------------------------------------
   REWIND NFCST
   READ(NFCST)RUN,IDAT,IHRST,NTSD
   IF(NTSD.EQ.1)NTSD=0
!----------------------------------------------------------------------
   READ(NFCST) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       PD(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   READ(NFCST) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       RES(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   READ(NFCST) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       FIS(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
   CALL wrf_debug ( 100 , 'nmm: read FIS' )
!----------------------------------------------------------------------
   DO K=KDE,KDS,-1
     READ(NFCST)((HOLD(I,J,K),I=IDS,IDE),J=JDS,JDE)
   ENDDO
   CALL wrf_debug ( 100 , 'nmm: read U into HOLD' )
   DO K=KDS,KDE
     DO J=JDS,JDE
       DO I=IDS,IDE
         U(I,K,J)=HOLD(I,J,K)
       ENDDO
     ENDDO
   ENDDO
   CALL wrf_debug ( 100 , 'nmm: read U' )
!----------------------------------------------------------------------
   DO K=KDE,KDS,-1
     READ(NFCST)((HOLD(I,J,K),I=IDS,IDE),J=JDS,JDE)
   ENDDO
   DO K=KDS,KDE
     DO J=JDS,JDE
       DO I=IDS,IDE
         V(I,K,J)=HOLD(I,J,K)
       ENDDO
     ENDDO
   ENDDO
   CALL wrf_debug ( 100 , 'nmm: read V' )
!----------------------------------------------------------------------
   DO K=KDE,KDS,-1
     READ(NFCST)((HOLD(I,J,K),I=IDS,IDE),J=JDS,JDE)
   ENDDO
   DO K=KDS,KDE
     DO J=JDS,JDE
       DO I=IDS,IDE
         T(I,K,J)=HOLD(I,J,K)
       ENDDO
     ENDDO
   ENDDO
   CALL wrf_debug ( 100 , 'nmm: read T' )
!----------------------------------------------------------------------
   DO K=KDE,KDS,-1
     READ(NFCST)((HOLD(I,J,K),I=IDS,IDE),J=JDS,JDE)
   ENDDO
   DO K=KDS,KDE
     DO J=JDS,JDE
       DO I=IDS,IDE
         Q(I,K,J)=HOLD(I,J,K)
       ENDDO
     ENDDO
   ENDDO
   CALL wrf_debug ( 100 , 'nmm: read Q' )
!----------------------------------------------------------------------
   READ(NFCST)((SI(I,J),I=IDS,IDE),J=JDS,JDE)
   READ(NFCST)((SNO(I,J),I=IDS,IDE),J=JDS,JDE)
!  READ(NFCST)(((SMC(I,J,N),I=IDS,IDE),J=JDS,JDE),N=1,NSOIL)
   READ(NFCST)(((hold(I,J,N),I=IDS,IDE),J=JDS,JDE),N=1,NSOIL)
      do k=1,nsoil
        do j=jds,jde
        do i=ids,ide
          smc(i,k,j)=hold(i,j,k)
        enddo
        enddo
      enddo
   READ(NFCST)((CMC(I,J),I=IDS,IDE),J=JDS,JDE)
!  READ(NFCST)(((STC(I,J,N),I=IDS,IDE),J=JDS,JDE),N=1,NSOIL)
   READ(NFCST)(((hold(I,J,N),I=IDS,IDE),J=JDS,JDE),N=1,NSOIL)
   do k=1,nsoil
     do j=jds,jde
     do i=ids,ide
       stc(i,k,j)=hold(i,j,k)
     enddo
     enddo
   enddo
!  READ(NFCST)(((SH2O(I,J,N),I=IDS,IDE),J=JDS,JDE),N=1,NSOIL)
   READ(NFCST)(((hold(I,J,N),I=IDS,IDE),J=JDS,JDE),N=1,NSOIL)
   do k=1,nsoil
     do j=jds,jde
     do i=ids,ide
       sh2o(i,k,j)=hold(i,j,k)
!      sh2o(i,k,j)=0.05
     enddo
     enddo
   enddo
   CALL wrf_debug ( 100 , 'nmm: read initial conditions file' )


!!!!!!!!!!!!!!!!!!!!!!!!!!
ENTRY med_read_nmm_bdy ( grid , config_flags , ntsd , dt_from_file, tstart_from_file, tend_from_file, &
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
!!!!!!!!!!!!!!!!!!!!!!!!!!



!----------------------------------------------------------------------
!***  READ BOUNDARY CONDITIONS.
!----------------------------------------------------------------------
!
      DT = dt_from_file
  CALL get_ijk_from_grid (  grid ,                   &
                            ids, ide, jds, jde, kds, kde,    &
                            ims, ime, jms, jme, kms, kme,    &
                            ips, ipe, jps, jpe, kps, kpe    )

! GLOBAL GRID DIMS ARE WHAT WRF CONSIDERS UNSTAGGERED
   ide = ide - 1
   jde = jde - 1
   kde = kde - 1
   NSOIL=4

   CALL wrf_debug(100,'in mediation_read_nmm')
   WRITE(mess,*)'ids,ide,jds,jde,kds,kde ',ids,ide,jds,jde,kds,kde
   CALL wrf_debug(100,mess)

      mype = 0
      IF(MYPE.EQ.0)THEN
        IF(NEST)THEN
          KBI=2*IM+JM-3
          KBI2=KBI-4
          LRECBC=4*(1+(1+6*LM)*KBI*2+(KBI+KBI2)*(LM+1))
          OPEN(UNIT=NUNIT_NBC,ACCESS='DIRECT',RECL=LRECBC)
          read(nunit_nbc,rec=2) bchr
        ENDIF
!
        IF(.NOT.NEST)REWIND NUNIT_NBC
!
        IF(NEST)THEN
          READ(NUNIT_NBC,REC=1)RUNB,IDATB,IHRSTB,TBOCO
        ELSE
          READ(NUNIT_NBC)RUNB,IDATB,IHRSTB,TBOCO
        ENDIF
      ENDIF
!
!      CALL MPI_BCAST(RUNB,1,MPI_LOGICAL,0,MPI_COMM_COMP,IRTN)
!      CALL MPI_BCAST(IDATB,3,MPI_INTEGER,0,MPI_COMM_COMP,IRTN)
!      CALL MPI_BCAST(IHRSTB,1,MPI_INTEGER,0,MPI_COMM_COMP,IRTN)
!      CALL MPI_BCAST(TBOCO,1,MPI_REAL,0,MPI_COMM_COMP,IRTN)
!
!      CALL MPI_BARRIER(MPI_COMM_COMP,IRTN)
!
      ISTART=NINT(TSTART)
      LB=2*(IDE-IDS+1)+(JDE-JDS+1)-3
!

      IF(MYPE.EQ.0.AND..NOT.NEST)THEN
!
        READ(NUNIT_NBC)BCHR
  205   READ(NUNIT_NBC)((PDB(N,I),N=1,LB),I=1,2)
        READ(NUNIT_NBC)(((TB(N,K,I),N=1,LB),K=KDE,KDS,-1),I=1,2)
        READ(NUNIT_NBC)(((QB(N,K,I),N=1,LB),K=KDE,KDS,-1),I=1,2)
        READ(NUNIT_NBC)(((UB(N,K,I),N=1,LB),K=KDE,KDS,-1),I=1,2)
        READ(NUNIT_NBC)(((VB(N,K,I),N=1,LB),K=KDE,KDS,-1),I=1,2)
        READ(NUNIT_NBC)(((Q2B(N,K,I),N=1,LB),K=KDE,KDS,-1),I=1,2)
        READ(NUNIT_NBC)(((CWMB(N,K,I),N=1,LB),K=KDE,KDS,-1),I=1,2)
!
        IF(ISTART.EQ.NINT(BCHR))THEN
          IF(ISTART.GT.0)READ(NUNIT_NBC)BCHR
          GO TO 215
        ELSE
          READ(NUNIT_NBC)BCHR
        ENDIF
!
        IF(ISTART.GE.NINT(BCHR))THEN
          GO TO 205
        ELSEIF(ISTART.LT.NINT(BCHR))THEN
          TSTEPS=ISTART*TSPH
!
          DO N=1,LB
            PDB(N,1)=PDB(N,1)+PDB(N,2)*DT*TSTEPS
          ENDDO
!
          DO K=1,LM
          DO N=1,LB
            TB(N,K,1)=TB(N,K,1)+TB(N,K,2)*DT*TSTEPS
            QB(N,K,1)=QB(N,K,1)+QB(N,K,2)*DT*TSTEPS
            UB(N,K,1)=UB(N,K,1)+UB(N,K,2)*DT*TSTEPS
            VB(N,K,1)=VB(N,K,1)+VB(N,K,2)*DT*TSTEPS
            Q2B(N,K,1)=Q2B(N,K,1)+Q2B(N,K,2)*DT*TSTEPS
            CWMB(N,K,1)=CWMB(N,K,1)+CWMB(N,K,2)*DT*TSTEPS
          ENDDO
          ENDDO
          GO TO 215
        ENDIF
      ENDIF
!
      IF(MYPE.EQ.0.AND.NEST)THEN
        NREC=1
!
  210   NREC=NREC+1
        READ(NUNIT_NBC,REC=NREC)BCHR
!
        IF(ISTART.EQ.NINT(BCHR))THEN
!!!!!     IF(ISTART.GT.0)READ(NUNIT_NBC,REC=NREC+1)BCHR
          GO TO 215
        ELSE
          GO TO 210
        ENDIF
      ENDIF
!
  215 CONTINUE

      IF(NTSD.EQ.0)THEN
        IF(MYPE.EQ.0.AND..NOT.NEST.AND.ISTART.GE.NINT(BCHR))THEN
          BACKSPACE NUNIT_NBC
          BACKSPACE NUNIT_NBC
          BACKSPACE NUNIT_NBC
          BACKSPACE NUNIT_NBC
          BACKSPACE NUNIT_NBC
          BACKSPACE NUNIT_NBC
          BACKSPACE NUNIT_NBC
!          WRITE(LIST,*)  BACKSPACE UNIT NBC=,NUNIT_NBC
        ENDIF
      ENDIF

      IF(MYPE.EQ.0.AND.NEST)THEN
          NREC=NINT(((NTSD-1)*DT)/3600.)+2
          READ(NUNIT_NBC,REC=NREC)BCHR                                  &
                         ,((PDB(N,NN),N=1,LB),NN=1,2)                  &
                         ,(((TB(N,K,NN),N=1,LB),K=KDE,KDS,-1),NN=1,2)   &
                         ,(((QB(N,K,NN),N=1,LB),K=KDE,KDS,-1),NN=1,2)   &
                         ,(((UB(N,K,NN),N=1,LB),K=KDE,KDS,-1),NN=1,2)   &
                         ,(((VB(N,K,NN),N=1,LB),K=KDE,KDS,-1),NN=1,2)   &
                        ,(((Q2B(N,K,NN),N=1,LB),K=KDE,KDS,-1),NN=1,2)   &
                       ,(((CWMB(N,K,NN),N=1,LB),K=KDE,KDS,-1),NN=1,2)
      ENDIF

! Copy the bounary into the WRF framework boundary data structs

      N=1
!
!***  SOUTH BOUNDARY
!
      DO I=1,IDE
        PD_B(I,1,1,P_YSB) = PDB(N,1)
        PD_BT(I,1,1,P_YSB) = PDB(N,2)
        N=N+1
      ENDDO
!
!***  NORTH BOUNDARY
!
      DO I=1,IDE
        PD_B(I,1,1,P_YEB) = PDB(N,1)
        PD_BT(I,1,1,P_YEB) = PDB(N,2)
        N=N+1
      ENDDO
!
!***  WEST BOUNDARY
!
      DO J=3,JDE-2,2
        PD_B(J,1,1,P_XSB) = PDB(N,1)
        PD_BT(J,1,1,P_XSB) = PDB(N,2)
        N=N+1
      ENDDO
!
!***  EAST BOUNDARY
!
      DO J=3,JDE-2,2
        PD_B(J,1,1,P_XEB) = PDB(N,1)
        PD_BT(J,1,1,P_XEB) = PDB(N,2)
        N=N+1
      ENDDO
!
      DO K=KDS,KDE
        N=1
!
!***  SOUTH BOUNDARY
!
        DO I=1,IDE
          T_B(I,k,1,P_YSB) = TB(N,k,1)
          T_BT(I,k,1,P_YSB) = TB(N,k,2)
          Q_B(I,k,1,P_YSB) = QB(N,k,1)
          Q_BT(I,k,1,P_YSB) = QB(N,k,2)
          Q2_B(I,k,1,P_YSB) = Q2B(N,k,1)
          Q2_BT(I,k,1,P_YSB) = Q2B(N,k,2)
          CWM_B(I,k,1,P_YSB) = CWMB(N,k,1)
          CWM_BT(I,k,1,P_YSB) = CWMB(N,k,2)
          N=N+1
        ENDDO
!
!***  NORTH BOUNDARY
!
        DO I=1,IDE
          T_B(I,k,1,P_YEB) = TB(N,k,1)
          T_BT(I,k,1,P_YEB) = TB(N,k,2)
          Q_B(I,k,1,P_YEB) = QB(N,k,1)
          Q_BT(I,k,1,P_YEB) = QB(N,k,2)
          Q2_B(I,k,1,P_YEB) = Q2B(N,k,1)
          Q2_BT(I,k,1,P_YEB) = Q2B(N,k,2)
          CWM_B(I,k,1,P_YEB) = CWMB(N,k,1)
          CWM_BT(I,k,1,P_YEB) = CWMB(N,k,2)
          N=N+1
        ENDDO
!
!***  WEST BOUNDARY
!
        DO J=3,JDE-2,2
          T_B(J,k,1,P_XSB) = TB(N,k,1)
          T_BT(J,k,1,P_XSB) = TB(N,k,2)
          Q_B(J,k,1,P_XSB) = QB(N,k,1)
          Q_BT(J,k,1,P_XSB) = QB(N,k,2)
          Q2_B(J,k,1,P_XSB) = Q2B(N,k,1)
          Q2_BT(J,k,1,P_XSB) = Q2B(N,k,2)
          CWM_B(J,k,1,P_XSB) = CWMB(N,k,1)
          CWM_BT(J,k,1,P_XSB) = CWMB(N,k,2)
          N=N+1
        ENDDO
!
!***  EAST BOUNDARY
!
        DO J=3,JDE-2,2
          T_B(J,k,1,P_XEB) = TB(N,k,1)
          T_BT(J,k,1,P_XEB) = TB(N,k,2)
      if(k.eq.1.and.j.eq.79)then
        write(0,62510)ntsd,nrec
        write(0,62511)p_xeb,t_b(j,k,1,p_xeb),t_bt(j,k,1,p_xeb)
62510   format(' ntsd=',i5,' nrec=',i5)
62511   format(' p_xeb=',i2,' t_b=',z8,' t_bt=',z8)
      endif
          Q_B(J,k,1,P_XEB) = QB(N,k,1)
          Q_BT(J,k,1,P_XEB) = QB(N,k,2)
          Q2_B(J,k,1,P_XEB) = Q2B(N,k,1)
          Q2_BT(J,k,1,P_XEB) = Q2B(N,k,2)
          CWM_B(J,k,1,P_XEB) = CWMB(N,k,1)
          CWM_BT(J,k,1,P_XEB) = CWMB(N,k,2)
          N=N+1
        ENDDO
      ENDDO

      DO K=KDS,KDE
        N=1
!
!***  SOUTH BOUNDARY
!
        DO I=1,IDE-1
          U_B(I,k,1,P_YSB) = UB(N,k,1)
          U_BT(I,k,1,P_YSB) = UB(N,k,2)
          V_B(I,k,1,P_YSB) = VB(N,k,1)
          V_BT(I,k,1,P_YSB) = VB(N,k,2)
          N=N+1
        ENDDO
!
!***  NORTH BOUNDARY
!
        DO I=1,IDE-1
          U_B(I,k,1,P_YEB) = UB(N,k,1)
          U_BT(I,k,1,P_YEB) = UB(N,k,2)
          V_B(I,k,1,P_YEB) = VB(N,k,1)
          V_BT(I,k,1,P_YEB) = VB(N,k,2)
          N=N+1
        ENDDO
!
!***  WEST BOUNDARY
!
        DO J=2,JDE-1,2
          U_B(J,k,1,P_XSB) = UB(N,k,1)
          U_BT(J,k,1,P_XSB) = UB(N,k,2)
          V_B(J,k,1,P_XSB) = VB(N,k,1)
          V_BT(J,k,1,P_XSB) = VB(N,k,2)
          N=N+1
        ENDDO
!
!***  EAST BOUNDARY
!
        DO J=2,JDE-1,2
          U_B(J,k,1,P_XEB) = UB(N,k,1)
          U_BT(J,k,1,P_XEB) = UB(N,k,2)
          V_B(J,k,1,P_XEB) = VB(N,k,1)
          V_BT(J,k,1,P_XEB) = VB(N,k,2)
          N=N+1
        ENDDO
      ENDDO

!
!      CALL MPI_BCAST(BCHR,1,MPI_REAL,0,MPI_COMM_COMP,IRTN)
!
!      CALL MPI_BARRIER(MPI_COMM_COMP,IRTN)
!
!      IF(MYPE.EQ.0)WRITE(LIST,*)  READ UNIT NBC=,NUNIT_NBC
!
!***
!***  COMPUTE THE 1ST TIME FOR BOUNDARY CONDITION READ
!***
!
!      NBOCO=NINT(BCHR*TSPH)
!

!

   DEALLOCATE(TEMP1,STAT=I)
   DEALLOCATE(ITEMP,STAT=I)
   DEALLOCATE(TEMP,STAT=I)
   DEALLOCATE(HOLD,STAT=I)

      CALL wrf_debug ( 100 , 'nmm: returnomatic' )

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
END SUBROUTINE med_read_nmm

