!REAL:MODEL_LAYER:INITIALIZATION

!  This MODULE holds the routines which are used to perform various initializations
!  for the individual domains, specifically for the Eulerian, mass-based coordinate.

!-----------------------------------------------------------------------

MODULE module_initialize

   USE module_bc
   USE module_configure
   USE module_domain
   USE module_io_domain
   USE module_model_constants
   USE module_si_io_nmm
   USE module_state_description
   USE module_timing
   USE module_soil_pre


CONTAINS

!-------------------------------------------------------------------

   SUBROUTINE init_domain ( grid )

      IMPLICIT NONE

      !  Input space and data.  No gridded meteorological data has been stored, though.

!     TYPE (domain), POINTER :: grid 
      TYPE (domain)          :: grid 

      !  Local data.

      INTEGER :: dyn_opt 
      INTEGER :: idum1, idum2



      CALL get_dyn_opt ( dyn_opt )
      
      CALL set_scalar_indices_from_config ( head_grid%id , idum1, idum2 )

      IF (      dyn_opt .eq. 1 &
           .or. dyn_opt .eq. 2 &
           .or. dyn_opt .eq. 3 &
                                          ) THEN
	CALL wrf_error_fatal ( "no RK version within dyn_nmm, dyn_opt wrong in namelist, wrf_error_fataling" )

     ELSEIF ( dyn_opt .eq. 4 ) THEN

	write(0,*) 'dyn_opt=4, call init_domain_nmm'

        CALL init_domain_nmm (grid, &
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


      ELSE
         WRITE(0,*)' init_domain: unknown or unimplemented dyn_opt = ',dyn_opt
	CALL wrf_error_fatal ( "ERROR-dyn_opt-wrong-in-namelist" )
      ENDIF

   END SUBROUTINE init_domain

!-------------------------------------------------------------------
!---------------------------------------------------------------------
   SUBROUTINE init_domain_nmm ( grid, &
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

!!!!!!       USE module_optional_si_input
      IMPLICIT NONE

      !  Input space and data.  No gridded meteorological data has been stored, though.

!     TYPE (domain), POINTER :: grid
      TYPE (domain)          :: grid

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

      TYPE (grid_config_rec_type)              :: config_flags

      !  Local domain indices and counters.

      INTEGER :: num_veg_cat , num_soil_top_cat , num_soil_bot_cat

      INTEGER                             ::                       &
                                     ids, ide, jds, jde, kds, kde, &
                                     ims, ime, jms, jme, kms, kme, &
                                     its, ite, jts, jte, kts, kte, &
                                     i, j, k, NNXP, NNYP

      !  Local data

	CHARACTER(LEN=19):: start_date

      INTEGER :: error
      REAL    :: p_surf, p_level
      REAL    :: cof1, cof2
      REAL    :: qvf , qvf1 , qvf2 , pd_surf
      REAL    :: p00 , t00 , a
      REAL    :: hold_znw, rmin,rmax

      LOGICAL :: stretch_grid, dry_sounding, debug, log_flag_sst

        REAL, ALLOCATABLE,DIMENSION(:,:):: ADUM2D,SNOWC,HT

        INTEGER, ALLOCATABLE, DIMENSION(:):: KHL2,KVL2,KHH2,KVH2, &
                                             KHLA,KHHA,KVLA,KVHA

        INTEGER, ALLOCATABLE, DIMENSION(:,:):: LU_INDEX

        REAL, ALLOCATABLE, DIMENSION(:):: DXJ,WPDARJ,CPGFUJ,CURVJ, &
                                          FCPJ,FDIVJ,EMJ,EMTJ,FADJ, &
                                          HDACJ,DDMPUJ,DDMPVJ

!!! in module_si_io_nmm        REAL:: TPH0D,TLM0D
        REAL:: TPH0,WB,SB,DLM,DPH,TDLM,TDPH
        REAL:: WBI,SBI,EBI,ANBI,STPH0,CTPH0
        REAL:: TSPH,DTAD,DTCF
        REAL:: ACDT,CDDAMP,TPH,DXP,TLM,FP
        REAL:: CTPH,STPH
        REAL:: WBD,SBD
        REAL:: RSNOW,SNOFAC
        REAL, PARAMETER:: SALP=2.60
	REAL, PARAMETER:: SNUP=0.040
	REAL:: SMCSUM,STCSUM,APH,TERM1


	INTEGER:: KHH,KVH,JAM,JA, IHL, IHH, L
	INTEGER:: II,JJ,ISRCH,ISUM

        REAL, PARAMETER:: DTR=0.01745329
        REAL, PARAMETER:: W_NMM=0.10
!tstval        REAL, PARAMETER:: W_NMM=0.08
!        REAL, PARAMETER:: W_NMM=0.07 ! AKNMM test
!0904        REAL, PARAMETER:: COAC=0.2
!        REAL, PARAMETER:: COAC=0.75 ! was 0.75
        REAL, PARAMETER:: COAC=0.2 ! was 0.75
!        REAL, PARAMETER:: CODAMP=6.4
        REAL, PARAMETER:: CODAMP=20.0
        REAL, PARAMETER:: TWOM=.00014584
        REAL, PARAMETER:: CP=1004.6
        REAL, PARAMETER:: DFC=1.0  ! was 1.0
        REAL, PARAMETER:: DDFC=1.0 ! was 1.0
        REAL, PARAMETER:: ROI=916.6
        REAL, PARAMETER:: R=287.04
        REAL, PARAMETER:: CI=2060.0
        REAL, PARAMETER:: ROS=1500.
        REAL, PARAMETER:: CS=1339.2
        REAL, PARAMETER:: DS=0.050
        REAL, PARAMETER:: AKS=.0000005
        REAL, PARAMETER:: DZG=2.85
        REAL, PARAMETER:: DI=.1000
        REAL, PARAMETER:: AKI=0.000001075
        REAL, PARAMETER:: DZI=2.0
        REAL, PARAMETER:: THL=210.
        REAL, PARAMETER:: PLQ=70000.
	REAL, PARAMETER:: ERAD=6371200.
	REAL, PARAMETER:: TG0=258.16
	REAL, PARAMETER:: TGA=30.0

        if (ALLOCATED(ADUM2D)) DEALLOCATE(ADUM2D)

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

      SELECT CASE ( model_data_order )
         CASE ( DATA_ORDER_ZXY )
            kds = grid%sd31 ; kde = grid%ed31 ;
            ids = grid%sd32 ; ide = grid%ed32 ;
            jds = grid%sd33 ; jde = grid%ed33 ;

            kms = grid%sm31 ; kme = grid%em31 ;
            ims = grid%sm32 ; ime = grid%em32 ;
            jms = grid%sm33 ; jme = grid%em33 ;

            kts = grid%sp31 ; kte = grid%ep31 ; ! tile is entire patch
            its = grid%sp32 ; ite = grid%ep32 ; ! tile is entire patch
            jts = grid%sp33 ; jte = grid%ep33 ; ! tile is entire patch

         CASE ( DATA_ORDER_XYZ )
            ids = grid%sd31 ; ide = grid%ed31 ;
            jds = grid%sd32 ; jde = grid%ed32 ;
            kds = grid%sd33 ; kde = grid%ed33 ;

            ims = grid%sm31 ; ime = grid%em31 ;
            jms = grid%sm32 ; jme = grid%em32 ;
            kms = grid%sm33 ; kme = grid%em33 ;

            its = grid%sp31 ; ite = grid%ep31 ; ! tile is entire patch
            jts = grid%sp32 ; jte = grid%ep32 ; ! tile is entire patch
            kts = grid%sp33 ; kte = grid%ep33 ; ! tile is entire patch

         CASE ( DATA_ORDER_XZY )
            ids = grid%sd31 ; ide = grid%ed31 ;
            kds = grid%sd32 ; kde = grid%ed32 ;
            jds = grid%sd33 ; jde = grid%ed33 ;

            ims = grid%sm31 ; ime = grid%em31 ;
            kms = grid%sm32 ; kme = grid%em32 ;
            jms = grid%sm33 ; jme = grid%em33 ;

            its = grid%sp31 ; ite = grid%ep31 ; ! tile is entire patch
            kts = grid%sp32 ; kte = grid%ep32 ; ! tile is entire patch
            jts = grid%sp33 ; jte = grid%ep33 ; ! tile is entire patch

      END SELECT

	
	DT=float(TIME_STEP)
	
	write(0,*) 'DT= ', dt

	NNXP=IDE-1
	NNYP=JDE-1

	write(0,*) 'nnxp,nnyp: ', nnxp,nnyp
	write(0,*) 'IDE, JDE: ', IDE,JDE

        JAM=6+2*(NNYP-10)

        ALLOCATE(ADUM2D(grid%sm31:grid%em31,grid%sm33:grid%em33))
        ALLOCATE(KHL2(NNYP),KVL2(NNYP),KHH2(NNYP),KVH2(NNYP))
        ALLOCATE(DXJ(NNYP),WPDARJ(NNYP),CPGFUJ(NNYP),CURVJ(NNYP))
        ALLOCATE(FCPJ(NNYP),FDIVJ(NNYP),EMJ(NNYP),EMTJ(NNYP),FADJ(NNYP))
        ALLOCATE(HDACJ(NNYP),DDMPUJ(NNYP),DDMPVJ(NNYP))
        ALLOCATE(KHLA(JAM),KHHA(JAM))
        ALLOCATE(KVLA(JAM),KVHA(JAM))


    CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
	write(6,*) 'cen_lat: ', config_flags%cen_lat
	write(6,*) 'cen_lon: ', config_flags%cen_lon
!	write(6,*) truelat?: , config_flags%moad_stand_lats(1)
	write(6,*) 'dx: ', config_flags%dx
	write(6,*) 'dy: ', config_flags%dy
	write(6,*) 'config_flags%start_year: ', config_flags%start_year
	write(6,*) 'config_flags%start_month: ', config_flags%start_month
	write(6,*) 'config_flags%start_day: ', config_flags%start_day
	write(6,*) 'config_flags%start_hour: ', config_flags%start_hour


	write(start_date,435) config_flags%start_year, config_flags%start_month, &
 		config_flags%start_day, config_flags%start_hour
  435	format(I4,'-',I2.2,'-',I2.2,'_',I2.2,':00:00')
	
	dlmd=config_flags%dx
	dphd=config_flags%dy

!	tph0d=global_meta%moad_known_lat
!	tlm0d=global_meta%moad_known_lon

!	tph0d=config_flags%cen_lat
!	tlm0d=config_flags%cen_lon

	write(6,*) 'set dlmd, dphd, tph0d, tlm0d:: ',  dlmd, dphd, tph0d, tlm0d

!==========================================================================

!!

 !  Check to see if the boundary conditions are set 
 !  properly in the namelist file.
 !  This checks for sufficiency and redundancy.

      CALL boundary_condition_check( config_flags, bdyzone, error, grid%id )

      !  Some sort of "this is the first time" initialization.  Who knows.

      grid%itimestep=0

      !  Pull in the info in the namelist to compare it to the input data.

      real_data_init_type = model_config_rec%real_data_init_type

	write(6,*) 'call read_si'
!	write(6,*) start_date:: , start_date(1:19)
	write(6,*) 'current_date:: ', current_date(1:19)
	current_date(11:11)='_'
	write(6,*) 'current_date:: ', current_date(1:19)

!	write(6,*) st_levels_input, into read_si: , st_levels_input
        CALL read_si   ( grid ,ide-1,jde-1,kde-1,dx,pt, current_date(1:19))
!	write(6,*) st_levels_input, from read_si: , st_levels_input
	

	write(6,*) 'back from read_si'

	PDTOP=PDTOP_in

	do J=1,JTE
	  do I=1,ITE
             SST(I,J)=sst_input(I,J)
             XICE(I,J)=seaice_input(I,J)
             WEASD(I,J)=snow_input(I,J)
             SM(I,J)=sm_in(I,J)
             CMC(I,J)=cmc_in(I,J)
	     VEGFRA(I,J)=green_frac_input(I,J)
!new
	     VEGFRC(I,J)=green_frac_input(I,J)
!new
	     NMM_TSK(I,J)=skintemp_input(I,J)
	     PD(I,J)=pd_input(I,J)
!	     PSFC(I,J)=psfc_in(I,J)
             FIS(I,J)=ter_input(I,J)
	     GLAT(I,J)=lat_mass(I,J)
	     GLON(I,J)=lon_mass(I,J)
	     ALBASE(I,J)=albase_input(I,J)
	     MXSNAL(I,J)=mxsnal_input(I,J)
	     ISLOPE(I,J)=islope_input(I,J)
	     GREENMAX(I,J)=green_frac_max(I,J)
	     GREENMIN(I,J)=green_frac_min(I,J)

!	for completeness
	     SM000010(I,J)=SM000010_input(I,J)
	     SM010040(I,J)=SM010040_input(I,J)
	     SM040100(I,J)=SM040100_input(I,J)
	     SM100200(I,J)=SM100200_input(I,J)
	     ST000010(I,J)=ST000010_input(I,J)
	     ST010040(I,J)=ST010040_input(I,J)
	     ST040100(I,J)=ST040100_input(I,J)
	     ST100200(I,J)=ST100200_input(I,J)
          enddo
        enddo

	write(6,*) 'PD(1,1), post read_si: ', pd(1,1)

	write(6,*) 'loop limits new: ', min(IDE-1,ITE), min(JDE-1,JTE),KDE-1

	do J=JTS,min(JDE-1,JTE)
	 do K=KDS,KDE-1
	  do I=ITS,min(IDE-1,ITE)

	HTM(I,K,J)=HTM_in(I,J,K)
	VTM(I,K,J)=VTM_in(I,J,K)
	U(I,K,J)=U_input(I,J,K)
	V(I,K,J)=V_input(I,J,K)
	T(I,K,J)=T_input(I,J,K)
	Q(I,K,J)=Q_input(I,J,K)
	
	  enddo
         enddo
        enddo

	write(6,*) 'filled big 3d arrays'

!!!	these 1-d arrays are backwards somehow

	do K=KDS,KDE
	ETAX(K)=ETAX_in(KDE-K+1)
	ETA1(K)=ETA1_in(KDE-K+1)
	ETA2(K)=ETA2_in(KDE-K+1)
	DFL(K)=dfl_input(KDE-K+1)
!new
        DFRLG(K)=dfl_input(KDE-K+1)/9.81
!new

	enddo

        write(0,*) 'DFL in module_initialize_real: ', DFL
        write(0,*) 'DFRLG in module_initialize_real: ', DFRLG

	do K=KDS,KDE-1
	DETA(K)=DETA_in(KDE-K)
	DETA1(K)=DETA1_in(KDE-K)
	DETA2(K)=DETA2_in(KDE-K)
	AETA(K)=AETA_in(KDE-K)
	AETA1(K)=AETA1_in(KDE-K)
	AETA2(K)=AETA2_in(KDE-K)
	enddo

	write(6,*) 'DETA1: ', DETA1
	write(6,*) 'DETA2: ', DETA2

      !  Take the data from the input file and store it in the variables that
      !  use the WRF naming and ordering conventions.

	write(6,*) 'j lim: ', MIN(jte,jde-1)
	write(6,*) 'i lim: ', MIN(ite,ide-1)

!!! WEASD has "snow water equivalent" in mm

       DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)

      IF(SM(I,J).GT.0.9) THEN

       IF (XICE(I,J) .gt. 0) then
         SI(I,J)=1.0
       ENDIF

!  SEA
              EPSR(I,J)=.97
              GFFC(I,J)=0.
              ALBEDO(I,J)=.06
              ALBASE(I,J)=.06
              IF(SI (I,J).GT.0.    ) THEN
!  SEA-ICE
                 SM(I,J)=0.
                 SI(I,J)=0.
                 SICE(I,J)=1.
                 GFFC(I,J)=0. ! just leave zero as irrelevant
                ALBEDO(I,J)=.60
                 ALBASE(I,J)=.60
              ENDIF
           ELSE

        SI(I,J)=5.0*WEASD(I,J)/1000.
!  LAND
        EPSR(I,J)=1.0
        GFFC(I,J)=0.0 ! just leave zero as irrelevant
        SICE(I,J)=0.
        SNO(I,J)=SI(I,J)*.20
           ENDIF
        ENDDO
        ENDDO

! DETERMINE ALBEDO OVER LAND
       DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)
          IF(SM(I,J).LT.0.9.AND.SICE(I,J).LT.0.9) THEN
! SNOWFREE ALBEDO
            IF ( (SNO(I,J) .EQ. 0.0) .OR. &
                (ALBASE(I,J) .GE. MXSNAL(I,J) ) ) THEN
              ALBEDO(I,J) = ALBASE(I,J)
            ELSE
! MODIFY ALBEDO IF SNOWCOVER:
! BELOW SNOWDEPTH THRESHOLD...
              IF (SNO(I,J) .LT. SNUP) THEN
                RSNOW = SNO(I,J)/SNUP
                SNOFAC = 1. - ( EXP(-SALP*RSNOW) - RSNOW*EXP(-SALP))
! ABOVE SNOWDEPTH THRESHOLD...
              ELSE
                SNOFAC = 1.0
              ENDIF
! CALCULATE ALBEDO ACCOUNTING FOR SNOWDEPTH AND VGFRCK
              ALBEDO(I,J) = ALBASE(I,J) &
               + (1.0-VEGFRA(I,J))*SNOFAC*(MXSNAL(I,J)-ALBASE(I,J))
            ENDIF
          END IF

! this block probably superfluous.  Meant to guarantee land/sea agreement
	if (SM(I,J) .gt. 0.5) then
		landmask(I,J)=0.0
	else
		landmask(I,J)=1.0
	endif


        ENDDO
      ENDDO

      !  For bl_surface_physics = 1, we want to use close to a 10 cm value
      !  for the bottom level of the soil temps.

      IF      ( ( model_config_rec%bl_surface_physics(grid%id) .EQ. 1 ) .AND. &
                ( flag_st000010 .EQ. 1 ) ) THEN
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               soiltb(i,j) = st000010(i,j)
            END DO
         END DO
!      ELSE IF ( ( model_config_rec%bl_surface_physics(grid%id) .EQ. 1 ) .AND. &
!                ( flag_soilt020 .EQ. 1 ) ) THEN
!         DO j = jts , MIN(jde-1,jte)
!            DO i = its , MIN(ide-1,ite)
!               soiltb(i,j) = soilt020(i,j)
!            END DO
!         END DO
      END IF


  !  Adjust the various soil temperature values depending on the difference in
  !  in elevation between the current models elevation and the incoming datas
  !  orography.

	write(6,*) 'flag_toposoil= ', flag_toposoil
         
      IF ( ( flag_toposoil .EQ. 1 ) ) THEN 

        ALLOCATE(HT(ims:ime,jms:jme))

        DO J=jms,jme
          DO I=ims,ime
              HT(I,J)=FIS(I,J)/9.81
          END DO
        END DO
           
!	if (maxval(toposoil) .gt. 100.) then

!  being avoided.  Cant remember the history here.  Something to revisit eventually.
!
!1219 might be simply a matter of including TOPOSOIL 
!
!    CODE NOT TESTED AT NCEP USING THIS FUNCTIONALITY, 
!    SO TO BE SAFE WILL AVOID FOR RETRO RUNS.

!	write(6,*) calling adjust_soil_temp_new
!        CALL adjust_soil_temp_new ( soiltb , 2 , &
!                       nmm_tsk , ht , toposoil , landmask, flag_toposoil , &
!                       st000010 , st010040 , st040100 , st100200 , st010200 , &
!                       flag_st000010 , flag_st010040 , flag_st040100 , &
!                       flag_st100200 , flag_st010200 , &
!                       soilt000 , soilt005 , soilt020 , soilt040 , &
!                       soilt160 , soilt300 , &
!                       flag_soilt000 , flag_soilt005 , flag_soilt020 , &
!                       flag_soilt040 , flag_soilt160 , flag_soilt300 , &
!                       ids , ide , jds , jde , kds , kde , &
!                       ims , ime , jms , jme , kms , kme , &
!                       its , ite , jts , jte , kts , kte )
!	endif

      END IF

	write(6,*) 'flag_sst before define is: ', flag_sst
!	FLAG_SST=1

         DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)

!	if (SM(I,J) .gt. 0.5) then
!            NMM_TSK(I,J)=0.
!	else
!	    SST(I,J)=0.
!	endif

	if (SM(I,J) .lt. 0.5) then
            SST(I,J)=0.
	endif

	    ENDDO
         ENDDO

	write(6,*) 'SST into NMM_TSK/SST branch'
	do J=jde-1,jts,-jte/20
	write(6,635) (SST(I,J),I=1,ide-1,ite/12)
	enddo

	write(6,*) 'NMM_TSK into NMM_TSK/SST branch'
	do J=jde-1,jts,-jte/20
	write(6,635) (NMM_TSK(I,J),I=1,ide-1,ite/12)
	enddo

         DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)

	if (SM(I,J) .gt. 0.5) then
          if (SST(I,J) .eq. 0) then
	    SST(I,J)=NMM_TSK(I,J)
          endif
	    NMM_TSK(I,J)=0.
	endif

!                  ADUM2D(I,J)=AMAX1(NMM_TSK(I,J),SST(I,J))
!
!	if (SM(I,J) .gt. 0.5) then
!	    SST(I,J)=ADUM2D(I,J)
!            NMM_TSK(I,J)=0.
!	else
!	    SST(I,J)=0.
!            NMM_TSK(I,J)=ADUM2D(I,J)
!	endif
		
	if ( (NMM_TSK(I,J)+SST(I,J)) .lt. 200. .or. &
             (NMM_TSK(I,J)+SST(I,J)) .gt. 350. ) then
	write(6,*) 'TSK, SST trouble at : ', I,J
	write(6,*) 'SM= ', SM(I,J)
	write(6,*) 'NMM_TSK(I,J), SST(I,J): ', NMM_TSK(I,J), SST(I,J)
	endif
            ENDDO
         ENDDO

	write(6,*) 'SM'
	do J=jde-1,jts,-jte/20
	write(6,635) (sm(i,J),I=1,ite,ite/12)
	enddo

	write(6,*) 'SST/NMM_TSK'
	do J=jde-1,jts,-jte/20
	write(6,635) (SST(I,J)+NMM_TSK(I,J),I=1,ide-1,ite/12)
	enddo

  635	format(20(f5.1,1x))

	write(6,*) 'ite,ide-1: ', ite,ide-1

         DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)
               IF ( ( landmask(i,j) .LT. 0.5 ) .AND. ( flag_sst .EQ. 1 ) ) THEN

!temporary tg,soiltb stuff

!                  soiltb(i,j) = sst(i,j)
!                  tg(i,j) = sst(i,j)
!curious               ELSE IF (  landmask(i,j) .LT. 0.5 ) THEN
               ELSE IF (  landmask(i,j) .GT. 0.5 ) THEN
!                  soiltb(i,j) = nmm_tsk(i,j)
!                      tg(i,j) = ST100200_input(I,J)
               END IF

!soiltb not used by NMM as an input...just set to zero
                   soiltb(i,j) = 0.
            END DO
         END DO

!      END IF

      !  Process the LSM data.
   
      IF ( real_data_init_type .EQ. 1 ) THEN
   
         num_veg_cat      = SIZE ( landusef , DIM=2 )
         num_soil_top_cat = SIZE ( soilctop , DIM=2 )
         num_soil_bot_cat = SIZE ( soilcbot , DIM=2 )

	do J=1,min(JDE-1,JTE)
	do K=1,num_veg_cat
	do I=1,min(IDE-1,ITE)
	landusef(I,K,J)=landuse_frac_input(I,J,K)
	enddo
	enddo
	enddo

	write(6,*) 'num_soil_top_cat: ', num_soil_top_cat

	do J=1,min(JDE-1,JTE)
	do K=1,num_soil_top_cat
	do I=1,min(IDE-1,ITE)
	soilctop(I,K,J)=soil_top_cat_input(I,J,K)
	enddo
	enddo
	enddo	

	do J=1,min(JDE-1,JTE)
	do K=1,num_soil_bot_cat
	do I=1,min(IDE-1,ITE)
	soilcbot(I,K,J)=soil_bot_cat_input(I,J,K)
	enddo
	enddo
	enddo	

! 	sm (1=water, 0=land)
!	xland (2=water, 1=land)

	write(6,*) 'call process_percent_cat_new... num_veg, num_soil:: ', num_Veg_cat, &
      num_soil_top_cat, num_soil_bot_cat

	write(6,*) 'call process_percent_cat_new with iswater: ', &
                   model_config_rec%iswater(grid%id)
         CALL process_percent_cat_new ( ADUM2D , &
                         landusef , soilctop , soilcbot , &
                         isltyp , ivgtyp , &
                         num_veg_cat , num_soil_top_cat , num_soil_bot_cat , &
                         ids , ide , jds , jde , kds , kde , &
                         ims , ime , jms , jme , kms , kme , &
                         its , ite , jts , jte , kts , kte , &
                         model_config_rec%iswater(grid%id) )

	write(6,*) 'IVGTYP'
	do J=jde-1,jts,-jte/15
	write(6,635) (float(ivgtyp(i,J)),I=1,ite,ite/10)
	enddo

	write(6,*) 'return process_percent_cat_new'

         DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)

	if (SICE(I,J) .eq. 1.0) then
!!!! change vegtyp and sltyp to fit seaice (desireable]
        ISLTYP(I,J)=16
	IVGTYP(I,J)=24
	endif

	if (SM(I,J)+SICE(I,J) .ne. ADUM2D(I,J)-1.) then
!	write(6,*) land mask disagrees at: , I,J
!	write(6,*) SM,xland,landmask: , SM(I,J), ADUM2D(I,J)-1., &
!                                         landmask(I,J)
	endif
	    ENDDO
         ENDDO


   !  Land use categories, dominant soil and vegetation types (if available).

	allocate(lu_index(ims:ime,jms:jme))
   
          DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)
               lu_index(i,j) = ivgtyp(i,j)
            END DO
         END DO

      END IF

!	write(6,*) st_levels_input: , st_levels_input

	write(6,*) 'STC specs:: ', size(stc,dim=1),size(stc,dim=2),size(stc,dim=3)


!	write(6,*) st_input(2)
	do J=jde-1,jts,-jte/15
!	write(6,635) (st_input(I,J,2),I=1,ide-1,ite/10)
	enddo

!	write(6,*) st_input(5)
	do J=jde-1,jts,-jte/15
!	write(6,635) (st_input(I,J,5),I=1,ide-1,ite/10)
	enddo
   
	if (flag_sst .eq. 1) log_flag_sst=.true.
	if (flag_sst .eq. 0) log_flag_sst=.false.

!	write(6,*) num_st_levels_input before mod: , num_st_levels_input
!	num_st_levels_input=10
!	num_sm_levels_input=10

	write(6,*) 'st_input dimensions:: ', size(st_input,dim=1),size(st_input,dim=2), &
                                             size(st_input,dim=3)
	write(6,*) 'landmask_input size: ', size(landmask_input,dim=1),  &
                                            size(landmask_input,dim=2)

	write(6,*) 'landmask_input extremes: ', minval(landmask_input),maxval(landmask_input)

	write(6,*) 'NMM_TSK into process_soil_real_old:'
	do J=jde-1,jts,-jte/15
	write(6,635) (NMM_TSK(I,J),I=1,ide-1,ite/12)
	enddo

      TPH0=TPH0D*DTR
      WBD=-((nnxp-1)*DLMD)
      WB= WBD*DTR
      SBD=-((nnyp/2)*DPHD)
      SB= SBD*DTR
      DLM=DLMD*DTR
      DPH=DPHD*DTR
      TDLM=DLM+DLM
      TDPH=DPH+DPH
      WBI=WB+TDLM
      SBI=SB+TDPH
      EBI=WB+(nnxp-2)*TDLM
      ANBI=SB+(nnyp-3)*DPH
      STPH0=SIN(TPH0)
      CTPH0=COS(TPH0)
      TSPH=3600./DT

          DO J=1,NNYP
           TLM=WB-TDLM+MOD(J,2)*DLM
           TPH=SB+float(J-1)*DPH
           STPH=SIN(TPH)
           CTPH=COS(TPH)
           DO I=1,NNXP
             TLM=TLM+TDLM
	     TERM1=(STPH0*CTPH*COS(TLM)+CTPH0*STPH)
             APH=ASIN(TERM1)   
             TG(I,J)=TG0+TGA*COS(APH)-FIS(I,J)/3333.
	if (mod(I,60) .eq. 0 .and. mod(J,90) .eq. 0) &
                                 write(0,*) 'I,J,TG: ', I,J,TG(I,J)
             FP=TWOM*(TERM1)
             F(I,J)=0.5*DT*FP
           ENDDO
          ENDDO

!                              ime,jme  ime,jme   ime,jme
!old  CALL process_soil_real_old ( NMM_TSK, soiltb , ADUM2D, &

  CALL process_soil_real_old ( NMM_TSK, TG , ADUM2D, &
     landmask_input, sst_input, &
     st_input, sm_input, st_levels_input, sm_levels_input, &
     sldpth, dzsoil, stc, smc, log_flag_sst, &
     st000010_input , st010040_input , st040100_input , st100200_input , &
     st010200_input , &
     sm000010_input , sm010040_input , sm040100_input , sm100200_input , &
     sm010200_input , &
     ids , ide , jds , jde , kds , kde , &
     ims , ime , jms , jme , kms , kme , &
     its , ite-1 , jts , jte-1, kts , kte , &
     model_config_rec%bl_surface_physics(grid%id) , &
     model_config_rec%num_soil_layers ,  &
     model_config_rec%real_data_init_type , &
     num_st_levels_input , num_sm_levels_input )

	write(6,*) 'sldpth: ', sldpth
	write(6,*) 'dzsoil: ', dzsoil

!!! 	zero out NMM_TSK at water points again

         DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)
	if (SM(I,J) .gt. 0.5) then
            NMM_TSK(I,J)=0.
	endif
            END DO
         END DO


!	call wrf_error_fatal ("just want to quit")

!!	check on STC

          DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)

	IF (SICE(I,J) .eq. 1.0) then
          DO L = 1, grid%num_soil_layers
	    STC(I,L,J)=271.16    ! TG value used by Eta/NMM
	  END DO
	END IF
		
	    END DO
          END DO

!hardwire soil stuff for time being

	RTDPTH=0.
	RTDPTH(1)=0.1
	RTDPTH(2)=0.3
	RTDPTH(3)=0.6

	SLDPTH=0.
	SLDPTH(1)=0.1
	SLDPTH(2)=0.3
	SLDPTH(3)=0.6
	SLDPTH(4)=1.0

	write(6,*) 'SLDPTH: ', SLDPTH(1:4)
	write(6,*) 'RTDPTH: ', RTDPTH(1:4)

!!! main body of nmm_specific starts here

!
! Gopals doings : LMH and LMV should be flipped for start_domain_nmm.F
!                   this is consistent with Toms version
!
       do J=1,nnyp
        do I=1,nnxp
         LMH(I,J)= kme-1        !1
         LMV(I,J)= kme-1        !1
         RES(I,J)=1.
        enddo
       enddo


!! HBM2
        call zero2d(HBM2,nnxp,nnyp)

       do J=3,nnyp-2
       do I=2,nnxp-2+mod(J,2)
       HBM2(I,J)=1.
       enddo
       enddo

!! HBM3
        call zero2d(HBM3,nnxp,nnyp)

	do J=1,NNYP
          IHWG(J)=mod(J+1,2)-1
          IF (J .ge. 4 .and. J .le. NNYP-3) THEN
            IHL=2-IHWG(J)
            IHH=NNXP-2
            do I=1,NNXP
              IF (I .ge. IHL  .and. I .le. IHH) HBM3(I,J)=1.
            enddo
          ENDIF
        enddo

!! VBM2
          call zero2d(VBM2,nnxp,nnyp)

       do J=3,nnyp-2
       do I=2,nnxp-1-mod(J,2)
       VBM2(I,J)=1.
       enddo
       enddo


!! VBM3
        call zero2d(VBM3,nnxp,nnyp)

       do J=4,nnyp-3
       do I=3-mod(J,2),nnxp-2
       VBM3(I,J)=1.
       enddo
       enddo

	write(6,*) 'TPH0D:: ', TPH0D

!	call wrf_error_fatal ("stopping after tph0d def")

!	DTAD=1 in const.f of intrst code
      DTAD=1.0

!	IDTCF=DTCF, IDTCF=4
      DTCF=4.0 ! used?

      DY_NMM=ERAD*DPH
      CPGFV=-DT/(48.*DY_NMM)
      EN= DT/( 4.*DY_NMM)*DTAD
      ENT=DT/(16.*DY_NMM)*DTAD

        write(6,*) 'DY_NMM,CPGFV,EN,ENT: ', DY_NMM,CPGFV,EN,ENT

        DO J=1,nnyp
         KHL2(J)=NNXP*(J-1)-(J-1)/2+2
         KVL2(J)=NNXP*(J-1)-J/2+2
         KHH2(J)=NNXP*J-J/2-1
         KVH2(J)=NNXP*J-(J+1)/2-1
        ENDDO

        TPH=SB-DPH

	write(6,*) 'CODAMP= ', CODAMP
	write(6,*) 'COAC= ', COAC
	write(6,*) 'W_NMM: ', W_NMM

        DO J=1,NNYP
         TPH=SB+float(J-1)*DPH
         DXP=ERAD*DLM*COS(TPH)
         DXJ(J)=DXP
         WPDARJ(J)=-W_NMM * &
     ((ERAD*DLM*AMIN1(COS(ANBI),COS(SBI)))**2+DY_NMM**2)/ &
                   (DT*32.*DXP*DY_NMM)

         CPGFUJ(J)=-DT/(48.*DXP)
         CURVJ(J)=.5*DT*TAN(TPH)/ERAD
         FCPJ(J)=DT/(CP*192.*DXP*DY_NMM)
         FDIVJ(J)=1./(12.*DXP*DY_NMM)
         EMJ(J)= DT/( 4.*DXP)*DTAD
         EMTJ(J)=DT/(16.*DXP)*DTAD
         FADJ(J)=-DT/(48.*DXP*DY_NMM)*DTAD
         ACDT=DT*SQRT((ERAD*DLM*AMIN1(COS(ANBI),COS(SBI)))**2+DY_NMM**2)
         CDDAMP=CODAMP*ACDT
         HDACJ(J)=COAC*ACDT/(4.*DXP*DY_NMM)
         DDMPUJ(J)=CDDAMP/DXP
         DDMPVJ(J)=CDDAMP/DY_NMM
        ENDDO

          JA=0
          DO 161 J=3,5
          JA=JA+1
          KHLA(JA)=2
          KHHA(JA)=NNXP-1-MOD(J+1,2)
 161      EMT(JA)=EMTJ(J)
          DO 162 J=NNYP-4,NNYP-2
          JA=JA+1
          KHLA(JA)=2
          KHHA(JA)=NNXP-1-MOD(J+1,2)
 162      EMT(JA)=EMTJ(J)
          DO 163 J=6,NNYP-5
          JA=JA+1
          KHLA(JA)=2
          KHHA(JA)=2+MOD(J,2)
 163      EMT(JA)=EMTJ(J)
          DO 164 J=6,NNYP-5
          JA=JA+1
          KHLA(JA)=NNXP-2
          KHHA(JA)=NNXP-1-MOD(J+1,2)
 164      EMT(JA)=EMTJ(J)

! --------------SPREADING OF UPSTREAM VELOCITY-POINT ADVECTION FACTOR----

      JA=0
              DO 171 J=3,5
          JA=JA+1
          KVLA(JA)=2
          KVHA(JA)=NNXP-1-MOD(J,2)
 171      EM(JA)=EMJ(J)
              DO 172 J=NNYP-4,NNYP-2
          JA=JA+1
          KVLA(JA)=2
          KVHA(JA)=NNXP-1-MOD(J,2)
 172      EM(JA)=EMJ(J)
              DO 173 J=6,NNYP-5
          JA=JA+1
          KVLA(JA)=2
          KVHA(JA)=2+MOD(J+1,2)
 173      EM(JA)=EMJ(J)
              DO 174 J=6,NNYP-5
          JA=JA+1
          KVLA(JA)=NNXP-2
          KVHA(JA)=NNXP-1-MOD(J,2)
 174      EM(JA)=EMJ(J)

! --------------DERIVED VERTICAL GRID CONSTANTS--------------------------

      EF4T=.5*DT/CP
      F4Q =   -DT*DTAD
      F4D =-.5*DT*DTAD

!       DO L=KDS,KDE
       DO L=KDS,KDE-1
        RDETA(L)=1./DETA(L)
        F4Q2(L)=-.25*DT*DTAD/DETA(L)
       ENDDO


        DO J=1,NNYP
        DO I=1,NNXP
          DX_NMM(I,J)=DXJ(J)
          WPDAR(I,J)=WPDARJ(J)*HBM2(I,J)
          CPGFU(I,J)=CPGFUJ(J)*VBM2(I,J)
          CURV(I,J)=CURVJ(J)*VBM2(I,J)
          FCP(I,J)=FCPJ(J)*HBM2(I,J)
          FDIV(I,J)=FDIVJ(J)*HBM2(I,J)
          FAD(I,J)=FADJ(J)
          HDACV(I,J)=HDACJ(J)*VBM2(I,J)
          HDAC(I,J)=HDACJ(J)*1.25*HBM2(I,J)
        ENDDO
        ENDDO


      DO J=3,NNYP-2
        IF (J.LE.5.OR.J.GE.NNYP-4) THEN
          KHH=NNXP-2+MOD(J,2)
          DO I=2,KHH
           HDAC(I,J)=HDAC(I,J)* DFC
          ENDDO
        ELSE
          KHH=2+MOD(J,2)
          DO I=2,KHH
           HDAC(I,J)=HDAC(I,J)* DFC
          ENDDO
          KHH=NNXP-2+MOD(J,2)
          DO I=NNXP-2,KHH
           HDAC(I,J)=HDAC(I,J)* DFC
          ENDDO
        ENDIF
      ENDDO


      DO J=1,NNYP
      DO I=1,NNXP
        DDMPU(I,J)=DDMPUJ(J)*VBM2(I,J)
        DDMPV(I,J)=DDMPVJ(J)*VBM2(I,J)
        HDACV(I,J)=HDACV(I,J)*VBM2(I,J)
      ENDDO
      ENDDO
! --------------INCREASING DIFFUSION ALONG THE BOUNDARIES----------------

      DO J=3,NNYP-2
        IF (J.LE.5.OR.J.GE.NNYP-4) THEN
          KVH=NNXP-1-MOD(J,2)
          DO I=2,KVH
            DDMPU(I,J)=DDMPU(I,J)*DDFC
            DDMPV(I,J)=DDMPV(I,J)*DDFC
            HDACV(I,J)=HDACV(I,J)* DFC
          ENDDO
        ELSE
          KVH=3-MOD(J,2)
          DO I=2,KVH
            DDMPU(I,J)=DDMPU(I,J)*DDFC
            DDMPV(I,J)=DDMPV(I,J)*DDFC
            HDACV(I,J)=HDACV(I,J)* DFC
          ENDDO
          KVH=NNXP-1-MOD(J,2)
          DO I=NNXP-2,KVH
            DDMPU(I,J)=DDMPU(I,J)*DDFC
            DDMPV(I,J)=DDMPV(I,J)*DDFC
            HDACV(I,J)=HDACV(I,J)* DFC
          ENDDO
        ENDIF
      ENDDO

	write(6,*) ' grid%num_soil_layers = ',  grid%num_soil_layers

	write(6,*) 'STC(1)'
	do J=jde-1,jts,-jte/15
	write(6,635) (stc(I,1,J),I=1,ide-1,ite/12)
	enddo

!	call wrf_error_fatal("what was STC")
	
	do J=JDS,JDE-1
	  do I=IDS,IDE-1
	    if (mod(I,50) .eq. 0 .and. mod(J,50) .eq. 0 .and. &
                      STC(I,1,J) .lt. 200. .and. SM(I,J) .eq. 0) then
	      write(6,*) 'troublesome STC...I,J,STC,SM,SICE,SMC: ',& 
                   I,J,STC(I,1,J),SM(I,J),SICE(I,J),SMC(I,1,J)
	    endif

	if (SM(I,J) .eq. 0 .and. SMC(I,1,J) .gt. 0.5 .and. SICE(I,J) .eq. 0) then
	write(6,*) 'wet on land point: ', I,J,SMC(I,1,J),SICE(I,J)
	endif

	  enddo
	enddo

	write(6,*) 'STC(50,1,1): ', STC(50,1,1)
	write(6,*) 'SMC(50,1,1): ', SMC(50,1,1)
	write(6,*) 'ISLTYP(50,1): ', ISLTYP(50,1)
	write(6,*) 'SH2O(50,1,1): ', SH2O(50,1,1)


      call NMM_SH2O(IMS,IME,JMS,JME,NNXP,NNYP,4,ISLTYP, &
                             SM,SICE,STC,SMC,SH2O)

	write(6,*) 'returned with SH2O(50,1,1): ', SH2O(50,1,1)


!! must be a better place to put this, but will eliminate "phantom"
!! wind points here (no wind point on eastern boundary of odd numbered rows)

	do K=1,KDE-1
          do J=1,JDE-1,2
             u(IDE-1,K,J)=0.
             v(IDE-1,K,J)=0.
          enddo
        enddo

  969	continue

	write(6,*) 'NMM_TSK leaving init_domain_nmm'
	do J=jde-1,jts,-jte/15
	write(6,635) (NMM_TSK(I,J),I=1,ide-1,ite/12)
	enddo

	write(6,*) 'leaving init_domain_nmm'
!
! Gopals doings : following lines  moved to namelist_input4 in Registry 
!
!	write(6,*) hardwire
!	 sigma=.true.
!	IDTAD=2  
!	NSOIL=4
!	NPHS=4
!	NCNVC=4
 write(6,*)'STUFF MOVED TO REGISTRY:',IDTAD,NSOIL,NRADL,NRADS,NPHS,NCNVC,sigma
!==================================================================================

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

   END SUBROUTINE init_domain_nmm

!--------------------------------------------------------------------
      SUBROUTINE NMM_SH2O(IMS,IME,JMS,JME,IM,JM,NSOIL,ISLTPK,&
			SM,SICE,STC,SMC,SH2O)

!!        INTEGER, PARAMETER:: NSOTYP=9
!        INTEGER, PARAMETER:: NSOTYP=16
        INTEGER, PARAMETER:: NSOTYP=19 !!!!!!!!MAYBE???

        REAL :: PSIS(NSOTYP),BETA(NSOTYP),SMCMAX(NSOTYP)
        REAL :: STC(IMS:IME,NSOIL,JMS:JME), &
                SMC(IMS:IME,NSOIL,JMS:JME)
        REAL :: SH2O(IMS:IME,NSOIL,JMS:JME),SICE(IMS:IME,JMS:JME),&
                SM(IMS:IME,JMS:JME)
        REAL :: HLICE,GRAV,T0,BLIM
        INTEGER :: ISLTPK(IMS:IME,JMS:JME)

! Constants used in cold start SH2O initialization
      DATA HLICE/3.335E5/,GRAV/9.81/,T0/273.15/
      DATA BLIM/5.5/
!      DATA PSIS /0.04,0.62,0.47,0.14,0.10,0.26,0.14,0.36,0.04/
!      DATA BETA /4.26,8.72,11.55,4.74,10.73,8.17,6.77,5.25,4.26/
!      DATA SMCMAX /0.421,0.464,0.468,0.434,0.406, &
!                  0.465,0.404,0.439,0.421/

	
!!!	 NOT SURE...PSIS=SATPSI, BETA=BB??

	DATA PSIS /0.069, 0.036, 0.141, 0.759, 0.759, 0.355,   &
                   0.135, 0.617, 0.263, 0.098, 0.324, 0.468,   &
                   0.355, 0.000, 0.069, 0.036, 0.468, 0.069, 0.069  /

	DATA BETA/2.79,  4.26,  4.74,  5.33,  5.33,  5.25,    &
                  6.66,  8.72,  8.17, 10.73, 10.39, 11.55,    &
                  5.25,  0.00,  2.79,  4.26, 11.55, 2.79, 2.79 /

	DATA SMCMAX/0.339, 0.421, 0.434, 0.476, 0.476, 0.439,  &
                    0.404, 0.464, 0.465, 0.406, 0.468, 0.468,  &
                    0.439, 1.000, 0.200, 0.421, 0.468, 0.200, 0.339/

	write(6,*) 'define SH2O over IM,JM: ', IM,JM
        DO K=1,NSOIL
         DO J=1,JM
          DO I=1,IM
!tst
	IF (SMC(I,K,J) .gt. SMCMAX(ISLTPK(I,J))) then
! if (K .eq. 1) then
!  write(0,*) I,J,reducing SMC from  ,I,J,SMC(I,K,J), to , SMCMAX(ISLTPK(I,J))
!  endif
	SMC(I,K,J)=SMCMAX(ISLTPK(I,J))
	ENDIF
!tst

        IF ( (SM(I,J) .lt. 0.5) .and. (SICE(I,J) .lt. 0.5) ) THEN

        IF (ISLTPK(I,J) .gt. 19) THEN
                WRITE(6,*) 'FORCING ISLTPK at : ', I,J
                ISLTPK(I,J)=9
        ELSEIF (ISLTPK(I,J) .le. 0) then
                WRITE(6,*) 'FORCING ISLTPK at : ', I,J
                ISLTPK(I,J)=1
        ENDIF


! cold start:  determine liquid soil water content (SH2O)
! SH2O <= SMC for T < 273.149K (-0.001C)

           IF (STC(I,K,J) .LT. 273.149) THEN

! first guess following explicit solution for Flerchinger Eqn from Koren
! et al, JGR, 1999, Eqn 17 (KCOUNT=0 in FUNCTION FRH2O).

              BX = BETA(ISLTPK(I,J))
              IF ( BETA(ISLTPK(I,J)) .GT. BLIM ) BX = BLIM

	if ( GRAV*(-PSIS(ISLTPK(I,J))) .eq. 0 ) then
	write(6,*) 'TROUBLE'
	write(6,*) 'I,J: ', i,J
	write(6,*) 'grav, isltpk, psis(isltpk): ', grav,isltpk(I,J),&
                 psis(isltpk(I,J))
	endif

	if (BX .eq. 0 .or. STC(I,K,J) .eq. 0) then
		write(6,*) 'I,J,BX, STC: ', I,J,BX,STC(I,K,J)
	endif
              FK = (((HLICE/(GRAV*(-PSIS(ISLTPK(I,J)))))* &
                  ((STC(I,K,J)-T0)/STC(I,K,J)))** &
                  (-1/BX))*SMCMAX(ISLTPK(I,J))
              IF (FK .LT. 0.02) FK = 0.02
              SH2O(I,K,J) = MIN ( FK, SMC(I,K,J) )
! ----------------------------------------------------------------------
! now use iterative solution for liquid soil water content using
! FUNCTION FRH2O (from the Eta "NOAH" land-surface model) with the
! initial guess for SH2O from above explicit first guess.

              SH2O(I,K,J)=FRH2O_init(STC(I,K,J),SMC(I,K,J),SH2O(I,K,J), &
                         SMCMAX(ISLTPK(I,J)),BETA(ISLTPK(I,J)), &
                         PSIS(ISLTPK(I,J)))

            ELSE ! above freezing
              SH2O(I,K,J)=SMC(I,K,J)
            ENDIF


	ELSE   ! water point
              SH2O(I,K,J)=SMC(I,K,J)

        ENDIF ! test on land/ice/sea
	if (SH2O(I,K,J) .gt. SMCMAX(ISLTPK(I,J))) then
	write(0,*) 'SH2O > THAN SMCMAX ', I,J,SH2O(I,K,J),SMCMAX(ISLTPK(I,J)),SMC(I,K,J)
	endif


         ENDDO
        ENDDO
       ENDDO

        END SUBROUTINE NMM_SH2O

!-------------------------------------------------------------------

        subroutine zero2d(adum2d,nnxp,nnyp)

        integer I,J,NNXP,NNYP
        real adum2d(nnxp,nnyp)

        do J=1,nnyp
        do I=1,nnxp
        adum2d(I,J)=0.
        enddo
        enddo

        end subroutine zero2d


!-------------------------------------------------------------------
      FUNCTION FRH2O_init(TKELV,SMC,SH2O,SMCMAX,B,PSIS)

      IMPLICIT NONE

! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!  PURPOSE:  CALCULATE AMOUNT OF SUPERCOOLED LIQUID SOIL WATER CONTENT
!  IF TEMPERATURE IS BELOW 273.15K (T0).  REQUIRES NEWTON-TYPE ITERATION
!  TO SOLVE THE NONLINEAR IMPLICIT EQUATION GIVEN IN EQN 17 OF
!  KOREN ET AL. (1999, JGR, VOL 104(D16), 19569-19585).
! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
! New version (JUNE 2001): much faster and more accurate newton iteration
! achieved by first taking log of eqn cited above -- less than 4
! (typically 1 or 2) iterations achieves convergence.  Also, explicit
! 1-step solution option for special case of parameter Ck=0, which reduces
! the original implicit equation to a simpler explicit form, known as the
! ""Flerchinger Eqn". Improved handling of solution in the limit of
! freezing point temperature T0.
!
! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
! INPUT:
!
!   TKELV.........Temperature (Kelvin)
!   SMC...........Total soil moisture content (volumetric)
!   SH2O..........Liquid soil moisture content (volumetric)
!   SMCMAX........Saturation soil moisture content (from REDPRM)
!   B.............Soil type "B" parameter (from REDPRM)
!   PSIS..........Saturated soil matric potential (from REDPRM)
!
! OUTPUT:
!   FRH2O.........supercooled liquid water content.
! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      REAL B
      REAL BLIM
      REAL BX
      REAL CK
      REAL DENOM
      REAL DF
      REAL DH2O
      REAL DICE
      REAL DSWL
      REAL ERROR
      REAL FK
      REAL FRH2O_init
      REAL GS
      REAL HLICE
      REAL PSIS
      REAL SH2O
      REAL SMC
      REAL SMCMAX
      REAL SWL
      REAL SWLK
      REAL TKELV
      REAL T0

      INTEGER NLOG
      INTEGER KCOUNT
      PARAMETER (CK=8.0)
!      PARAMETER (CK=0.0)
      PARAMETER (BLIM=5.5)
!      PARAMETER (BLIM=7.0)
      PARAMETER (ERROR=0.005)

      PARAMETER (HLICE=3.335E5)
      PARAMETER (GS = 9.81)
      PARAMETER (DICE=920.0)
      PARAMETER (DH2O=1000.0)
      PARAMETER (T0=273.15)

!  ###   LIMITS ON PARAMETER B: B < 5.5  (use parameter BLIM)  ####
!  ###   SIMULATIONS SHOWED IF B > 5.5 UNFROZEN WATER CONTENT  ####
!  ###   IS NON-REALISTICALLY HIGH AT VERY LOW TEMPERATURES    ####
!  ################################################################
!
      BX = B
      IF ( B .GT. BLIM ) BX = BLIM
! ------------------------------------------------------------------

! INITIALIZING ITERATIONS COUNTER AND ITERATIVE SOLUTION FLAG.
      NLOG=0
      KCOUNT=0

! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! C  IF TEMPERATURE NOT SIGNIFICANTLY BELOW FREEZING (T0), SH2O = SMC
! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      IF (TKELV .GT. (T0 - 1.E-3)) THEN

        FRH2O_init=SMC

      ELSE

! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       IF (CK .NE. 0.0) THEN

! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! CCCCCCCCC OPTION 1: ITERATED SOLUTION FOR NONZERO CK CCCCCCCCCCC
! CCCCCCCCCCCC IN KOREN ET AL, JGR, 1999, EQN 17 CCCCCCCCCCCCCCCCC

! INITIAL GUESS FOR SWL (frozen content)
        SWL = SMC-SH2O
! KEEP WITHIN BOUNDS.
         IF (SWL .GT. (SMC-0.02)) SWL=SMC-0.02
         IF(SWL .LT. 0.) SWL=0.
! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! C  START OF ITERATIONS
! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        DO WHILE (NLOG .LT. 10 .AND. KCOUNT .EQ. 0)
         NLOG = NLOG+1
         DF = ALOG(( PSIS*GS/HLICE ) * ( ( 1.+CK*SWL )**2. ) * &
             ( SMCMAX/(SMC-SWL) )**BX) - ALOG(-(TKELV-T0)/TKELV)
         DENOM = 2. * CK / ( 1.+CK*SWL ) + BX / ( SMC - SWL )
         SWLK = SWL - DF/DENOM
! BOUNDS USEFUL FOR MATHEMATICAL SOLUTION.
         IF (SWLK .GT. (SMC-0.02)) SWLK = SMC - 0.02
         IF(SWLK .LT. 0.) SWLK = 0.
! MATHEMATICAL SOLUTION BOUNDS APPLIED.
         DSWL=ABS(SWLK-SWL)
         SWL=SWLK
! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! CC IF MORE THAN 10 ITERATIONS, USE EXPLICIT METHOD (CK=0 APPROX.)
! CC WHEN DSWL LESS OR EQ. ERROR, NO MORE ITERATIONS REQUIRED.
! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
         IF ( DSWL .LE. ERROR )  THEN
           KCOUNT=KCOUNT+1
         END IF
        END DO
! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! C  END OF ITERATIONS
! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! BOUNDS APPLIED WITHIN DO-BLOCK ARE VALID FOR PHYSICAL SOLUTION.
        FRH2O_init = SMC - SWL

! CCCCCCCCCCCCCCCCCCCCCCCC END OPTION 1 CCCCCCCCCCCCCCCCCCCCCCCCCCC

       ENDIF

       IF (KCOUNT .EQ. 0) THEN
!         Print*,Flerchinger used in NEW version. Iterations=,NLOG

! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! CCCCC OPTION 2: EXPLICIT SOLUTION FOR FLERCHINGER EQ. i.e. CK=0 CCCCCCCC
! CCCCCCCCCCCCC IN KOREN ET AL., JGR, 1999, EQN 17  CCCCCCCCCCCCCCC

        FK=(((HLICE/(GS*(-PSIS)))*((TKELV-T0)/TKELV))**(-1/BX))*SMCMAX
! APPLY PHYSICAL BOUNDS TO FLERCHINGER SOLUTION
        IF (FK .LT. 0.02) FK = 0.02
        FRH2O_init = MIN ( FK, SMC )

! CCCCCCCCCCCCCCCCCCCCCCCCC END OPTION 2 CCCCCCCCCCCCCCCCCCCCCCCCCC

       ENDIF

      ENDIF

	RETURN

      END FUNCTION FRH2O_init


!--------------------------------------------------------------------

   SUBROUTINE const_module_initialize ( p00 , t00 , a ) 
      IMPLICIT NONE
      REAL , PARAMETER :: sea_level_pressure_base    = 100000.
      REAL , PARAMETER :: sea_level_temperature_base =    290.
      REAL , PARAMETER :: temp_diff_1000_to_300_mb   =     50.
      REAL , INTENT(OUT) :: p00 , t00 , a
      p00 = sea_level_pressure_base
      t00 = sea_level_temperature_base
      a   = temp_diff_1000_to_300_mb
   END SUBROUTINE const_module_initialize

!---------------------------------------------------------------------

   SUBROUTINE init_module_initialize
   END SUBROUTINE init_module_initialize

!---------------------------------------------------------------------

END MODULE module_initialize
