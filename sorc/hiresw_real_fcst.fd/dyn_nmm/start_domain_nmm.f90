




      SUBROUTINE START_DOMAIN_NMM(GRID, allowed_to_read                &

!STARTOFREGISTRYGENERATEDINCLUDE 'inc/dummy_args.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
,x_1,x_2,lu_index,lu_mask,p_gc,vegcat,soilcat,input_soil_cat,tsk_gc,xice_gc,ght_gc,rh_gc,v_gc,u_gc,t_gc,snoalb,greenfrac_gc, &
albedo12m_gc,soilcbot_gc,soilctop_gc,tmn_gc,htv_gc,ht_gc,landusef_gc,vlon_gc,vlat_gc,hlon_gc,hlat_gc,hbm2,hbm3,vbm2,vbm3,sm, &
sice,pd,pd_bxs,pd_bxe,pd_bys,pd_bye,pd_btxs,pd_btxe,pd_btys,pd_btye,fis,res,t,t_bxs,t_bxe,t_bys,t_bye,t_btxs,t_btxe,t_btys, &
t_btye,q,q_bxs,q_bxe,q_bys,q_bye,q_btxs,q_btxe,q_btys,q_btye,u,u_bxs,u_bxe,u_bys,u_bye,u_btxs,u_btxe,u_btys,u_btye,v,v_bxs, &
v_bxe,v_bys,v_bye,v_btxs,v_btxe,v_btys,v_btye,told,uold,vold,hcoeff,dfi_pd,dfi_pint,dfi_dwdt,dfi_t,dfi_q,dfi_u,dfi_v,dfi_q2, &
dfi_cwm,dfi_rrw,dfi_stc,dfi_smc,dfi_sh2o,dfi_snow,dfi_snowh,dfi_canwat,dfi_nmm_tsk,dfi_snowc,dx_nmm,wpdar,cpgfu,curv,fcp,fdiv,f, &
fad,ddmpu,ddmpv,deta,rdeta,aeta,f4q2,etax,dfl,deta1,aeta1,eta1,deta2,aeta2,eta2,em,emt,adt,adu,adv,em_loc,emt_loc,pdsl,pdslo, &
psdt,div,few,fne,fns,fse,omgalf,petdt,rtop,pblh,lpbl,mixht,ustar,z0,z0base,ths,mavail,qsh,twbs,qwbs,taux,tauy,prec,aprec,acprec, &
cuprec,lspa,ddata,accliq,sno,si,cldefi,deep,rf,th10,q10,pshltr,tshltr,qshltr,q2,q2_bxs,q2_bxe,q2_bys,q2_bye,q2_btxs,q2_btxe, &
q2_btys,q2_btye,t_adj,t_old,zero_3d,w0avg,akhs_out,akms_out,albase,albedo,cnvbot,cnvtop,czen,czmean,embck,epsr,gffc,glat,glon, &
nmm_tsk,hdac,hdacv,mxsnal,radin,radot,sigt4,tg,dfrlg,lvl,cwm,cwm_bxs,cwm_bxe,cwm_bys,cwm_bye,cwm_btxs,cwm_btxe,cwm_btys, &
cwm_btye,rrw,rrw_bxs,rrw_bxe,rrw_bys,rrw_bye,rrw_btxs,rrw_btxe,rrw_btys,rrw_btye,f_ice,f_rain,f_rimef,cldfra,sr,cfrach,cfracl, &
cfracm,islope,dzsoil,rtdpth,sldpth,cmc,grnflx,pctsno,soiltb,vegfrc,shdmin,shdmax,sh2o,smc,stc,hstdv,hcnvx,hasyw,hasys,hasysw, &
hasynw,hlenw,hlens,hlensw,hlennw,hangl,hanis,hslop,hzmax,crot,srot,ugwdsfc,vgwdsfc,dwdtmn,dwdtmx,dwdt,pdwdt,pint,w,z,acfrcv, &
acfrst,ssroff,bgroff,rlwin,rlwout,rlwtoa,alwin,alwout,alwtoa,rswin,rswinc,rswout,rswtoa,aswin,aswout,aswtoa,sfcshx,sfclhx, &
subshx,snopcx,sfcuvx,potevp,potflx,tlmin,tlmax,t02_min,t02_max,rh02_min,rh02_max,rlwtt,rswtt,tcucn,train,ncfrcv,ncfrst,max10mw, &
max10u,max10v,maxupdr,maxdndr,maxhlcy,maxdbz,ihe,ihw,ive,ivw,irad,iheg,ihwg,iveg,ivwg,iradg,n_iup_h,n_iup_v,n_iup_adh,n_iup_adv, &
iup_h,iup_v,iup_adh,iup_adv,imask_nostag,imask_xstag,imask_ystag,imask_xystag,sm000007,sm007028,sm028100,sm100255,st000007, &
st007028,st028100,st100255,sm000010,sm010040,sm040100,sm100200,sm010200,soilm000,soilm005,soilm020,soilm040,soilm160,soilm300, &
sw000010,sw010040,sw040100,sw100200,sw010200,soilw000,soilw005,soilw020,soilw040,soilw160,soilw300,st000010,st010040,st040100, &
st100200,st010200,soilt000,soilt005,soilt020,soilt040,soilt160,soilt300,landmask,topostdv,toposlpx,toposlpy,greenmax,greenmin, &
albedomx,slopecat,toposoil,landusef,soilctop,soilcbot,ts_hour,ts_u,ts_v,ts_q,ts_t,ts_psfc,ts_tsk,ts_tslb,ts_clw,moist,dfi_moist, &
scalar,scalar_bxs,scalar_bxe,scalar_bys,scalar_bye,scalar_btxs,scalar_btxe,scalar_btys,scalar_btye,dfi_scalar,dfi_scalar_bxs, &
dfi_scalar_bxe,dfi_scalar_bys,dfi_scalar_bye,dfi_scalar_btxs,dfi_scalar_btxe,dfi_scalar_btys,dfi_scalar_btye,chem,smois,tslb, &
gsw,xlat,xlong,xland,raincv,psfc,th2,t2,u10,v10,xice,lai,smstav,smstot,sfcrunoff,udrunoff,ivgtyp,isltyp,vegfra,sfcevp,grdflx, &
albbck,sfcexc,snotime,acsnow,acsnom,rmol,snow,canwat,sst,weasd,znt,mol,noahres,tke_myj,el_myj,exch_h,exch_m,thz0,qz0,uz0,vz0, &
flhc,flqc,qsg,qvg,qcg,soilt1,tsnav,psfc_out,uz0h,vz0h,dudt,dvdt,qsfc,akhs,akms,htop,hbot,htopr,hbotr,htopd,hbotd,htops,hbots, &
cuppt,cprate,f_ice_phy,f_rain_phy,f_rimef_phy,mass_flux,apr_gr,apr_w,apr_mc,apr_st,apr_as,apr_capma,apr_capme,apr_capmi,xf_ens, &
pr_ens,rthften,rqvften,snowh,rhosn,smfr3d,keepfr3dflag,mp_restart_state,tbpvs_state,tbpvs0_state,lu_state &
!ENDOFREGISTRYGENERATEDINCLUDE

     &           )


      USE MODULE_DOMAIN
      USE MODULE_DRIVER_CONSTANTS
      USE module_model_constants
      USE MODULE_CONFIGURE
      USE MODULE_WRF_ERROR
      USE MODULE_MPP
      USE MODULE_CTLBLK
      USE MODULE_DM

      USE MODULE_IGWAVE_ADJUST,ONLY: PDTE, PFDHT, DDAMP
      USE MODULE_ADVECTION,    ONLY: ADVE, VAD2, HAD2
      USE MODULE_NONHY_DYNAM,  ONLY: VADZ, HADZ
      USE MODULE_DIFFUSION_NMM,ONLY: HDIFF
      USE MODULE_BNDRY_COND,   ONLY: BOCOH, BOCOV
      USE MODULE_PHYSICS_INIT
      USE MODULE_GWD


      USE MODULE_EXT_INTERNAL




      IMPLICIT NONE





      TYPE(DOMAIN),INTENT(INOUT) :: GRID
      LOGICAL , INTENT(IN)       :: allowed_to_read

!STARTOFREGISTRYGENERATEDINCLUDE 'inc/dummy_decl.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
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
integer                                  :: nodyn_dummy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32)           :: x_1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32)           :: x_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: lu_index
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: lu_mask
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,1:grid%num_metgrid_levels)           :: p_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: vegcat
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: soilcat
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: input_soil_cat
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: tsk_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: xice_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,1:grid%num_metgrid_levels)           :: ght_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,1:grid%num_metgrid_levels)           :: rh_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,1:grid%num_metgrid_levels)           :: v_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,1:grid%num_metgrid_levels)           :: u_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,1:grid%num_metgrid_levels)           :: t_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: snoalb
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,1:12)           :: greenfrac_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,1:12)           :: albedo12m_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,1:grid%num_soil_cat)           :: soilcbot_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,1:grid%num_soil_cat)           :: soilctop_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: tmn_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: htv_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: ht_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,1:grid%num_land_cat)           :: landusef_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: vlon_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: vlat_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hlon_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hlat_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hbm2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hbm3
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: vbm2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: vbm3
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sm
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sice
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: pd
real      ,DIMENSION(grid%sm32:grid%em32,1,grid%spec_bdy_width)           :: pd_bxs
real      ,DIMENSION(grid%sm32:grid%em32,1,grid%spec_bdy_width)           :: pd_bxe
real      ,DIMENSION(grid%sm31:grid%em31,1,grid%spec_bdy_width)           :: pd_bys
real      ,DIMENSION(grid%sm31:grid%em31,1,grid%spec_bdy_width)           :: pd_bye
real      ,DIMENSION(grid%sm32:grid%em32,1,grid%spec_bdy_width)           :: pd_btxs
real      ,DIMENSION(grid%sm32:grid%em32,1,grid%spec_bdy_width)           :: pd_btxe
real      ,DIMENSION(grid%sm31:grid%em31,1,grid%spec_bdy_width)           :: pd_btys
real      ,DIMENSION(grid%sm31:grid%em31,1,grid%spec_bdy_width)           :: pd_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: fis
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: res
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: t
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width)           :: t_bxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width)           :: t_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width)           :: t_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width)           :: t_bye
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width)           :: t_btxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width)           :: t_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width)           :: t_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width)           :: t_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: q
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width)           :: q_bxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width)           :: q_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width)           :: q_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width)           :: q_bye
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width)           :: q_btxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width)           :: q_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width)           :: q_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width)           :: q_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: u
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width)           :: u_bxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width)           :: u_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width)           :: u_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width)           :: u_bye
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width)           :: u_btxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width)           :: u_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width)           :: u_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width)           :: u_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: v
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width)           :: v_bxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width)           :: v_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width)           :: v_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width)           :: v_bye
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width)           :: v_btxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width)           :: v_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width)           :: v_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width)           :: v_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: told
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: uold
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: vold
real      ,DIMENSION(1:grid%dfi_time_dim)           :: hcoeff
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: dfi_pd
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: dfi_pint
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: dfi_dwdt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: dfi_t
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: dfi_q
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: dfi_u
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: dfi_v
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: dfi_q2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: dfi_cwm
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: dfi_rrw
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%num_soil_layers,grid%sm32:grid%em32)           :: dfi_stc
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%num_soil_layers,grid%sm32:grid%em32)           :: dfi_smc
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%num_soil_layers,grid%sm32:grid%em32)           :: dfi_sh2o
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: dfi_snow
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: dfi_snowh
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: dfi_canwat
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: dfi_nmm_tsk
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: dfi_snowc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: dx_nmm
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: wpdar
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: cpgfu
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: curv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: fcp
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: fdiv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: f
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: fad
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: ddmpu
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: ddmpv
real      ,DIMENSION(grid%sm33:grid%em33)           :: deta
real      ,DIMENSION(grid%sm33:grid%em33)           :: rdeta
real      ,DIMENSION(grid%sm33:grid%em33)           :: aeta
real      ,DIMENSION(grid%sm33:grid%em33)           :: f4q2
real      ,DIMENSION(grid%sm33:grid%em33)           :: etax
real      ,DIMENSION(grid%sm33:grid%em33)           :: dfl
real      ,DIMENSION(grid%sm33:grid%em33)           :: deta1
real      ,DIMENSION(grid%sm33:grid%em33)           :: aeta1
real      ,DIMENSION(grid%sm33:grid%em33)           :: eta1
real      ,DIMENSION(grid%sm33:grid%em33)           :: deta2
real      ,DIMENSION(grid%sm33:grid%em33)           :: aeta2
real      ,DIMENSION(grid%sm33:grid%em33)           :: eta2
real      ,DIMENSION(1:2600)             :: em
real      ,DIMENSION(1:2600)             :: emt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: adt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: adu
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: adv
real      ,DIMENSION(1:2600)             :: em_loc
real      ,DIMENSION(1:2600)             :: emt_loc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: pdsl
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: pdslo
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: psdt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: div
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: few
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: fne
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: fns
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: fse
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: omgalf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: petdt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rtop
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: pblh
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: lpbl
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: mixht
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: ustar
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: z0
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: z0base
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: ths
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: mavail
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: qsh
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: twbs
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: qwbs
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: taux
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: tauy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: prec
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: aprec
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: acprec
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: cuprec
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: lspa
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: ddata
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: accliq
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sno
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: si
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: cldefi
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: deep
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: rf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: th10
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: q10
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: pshltr
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: tshltr
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: qshltr
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: q2
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width)           :: q2_bxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width)           :: q2_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width)           :: q2_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width)           :: q2_bye
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width)           :: q2_btxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width)           :: q2_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width)           :: q2_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width)           :: q2_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: t_adj
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: t_old
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: zero_3d
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32)           :: w0avg
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: akhs_out
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: akms_out
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: albase
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: albedo
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: cnvbot
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: cnvtop
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: czen
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: czmean
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: embck
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: epsr
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: gffc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: glat
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: glon
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: nmm_tsk
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hdac
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hdacv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: mxsnal
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: radin
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: radot
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sigt4
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: tg
real      ,DIMENSION(grid%sm33:grid%em33)           :: dfrlg
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: lvl
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: cwm
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width)           :: cwm_bxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width)           :: cwm_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width)           :: cwm_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width)           :: cwm_bye
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width)           :: cwm_btxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width)           :: cwm_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width)           :: cwm_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width)           :: cwm_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rrw
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width)           :: rrw_bxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width)           :: rrw_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width)           :: rrw_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width)           :: rrw_bye
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width)           :: rrw_btxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width)           :: rrw_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width)           :: rrw_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width)           :: rrw_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32)           :: f_ice
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32)           :: f_rain
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32)           :: f_rimef
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: cldfra
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sr
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: cfrach
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: cfracl
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: cfracm
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: islope
real      ,DIMENSION(grid%sm33:grid%em33)           :: dzsoil
real      ,DIMENSION(grid%sm33:grid%em33)           :: rtdpth
real      ,DIMENSION(grid%sm33:grid%em33)           :: sldpth
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: cmc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: grnflx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: pctsno
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: soiltb
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: vegfrc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: shdmin
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: shdmax
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%num_soil_layers,grid%sm32:grid%em32)           :: sh2o
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%num_soil_layers,grid%sm32:grid%em32)           :: smc
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%num_soil_layers,grid%sm32:grid%em32)           :: stc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hstdv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hcnvx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hasyw
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hasys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hasysw
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hasynw
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hlenw
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hlens
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hlensw
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hlennw
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hangl
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hanis
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hslop
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hzmax
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: crot
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: srot
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: ugwdsfc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: vgwdsfc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: dwdtmn
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: dwdtmx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: dwdt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: pdwdt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: pint
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: w
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: z
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: acfrcv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: acfrst
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: ssroff
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: bgroff
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: rlwin
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: rlwout
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: rlwtoa
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: alwin
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: alwout
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: alwtoa
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: rswin
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: rswinc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: rswout
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: rswtoa
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: aswin
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: aswout
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: aswtoa
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sfcshx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sfclhx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: subshx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: snopcx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sfcuvx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: potevp
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: potflx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: tlmin
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: tlmax
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: t02_min
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: t02_max
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: rh02_min
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: rh02_max
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rlwtt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rswtt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: tcucn
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: train
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: ncfrcv
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: ncfrst
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: max10mw
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: max10u
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: max10v
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: maxupdr
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: maxdndr
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: maxhlcy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: maxdbz
integer   ,DIMENSION(grid%sm32:grid%em32)           :: ihe
integer   ,DIMENSION(grid%sm32:grid%em32)           :: ihw
integer   ,DIMENSION(grid%sm32:grid%em32)           :: ive
integer   ,DIMENSION(grid%sm32:grid%em32)           :: ivw
integer   ,DIMENSION(grid%sm31:grid%em31)           :: irad
integer   ,DIMENSION(1:2600)             :: iheg
integer   ,DIMENSION(1:2600)             :: ihwg
integer   ,DIMENSION(1:2600)             :: iveg
integer   ,DIMENSION(1:2600)             :: ivwg
integer   ,DIMENSION(1:2000)             :: iradg
integer   ,DIMENSION(grid%sm32:grid%em32)           :: n_iup_h
integer   ,DIMENSION(grid%sm32:grid%em32)           :: n_iup_v
integer   ,DIMENSION(grid%sm32:grid%em32)           :: n_iup_adh
integer   ,DIMENSION(grid%sm32:grid%em32)           :: n_iup_adv
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: iup_h
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: iup_v
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: iup_adh
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: iup_adv
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: imask_nostag
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: imask_xstag
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: imask_ystag
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: imask_xystag
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sm000007
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sm007028
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sm028100
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sm100255
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: st000007
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: st007028
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: st028100
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: st100255
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sm000010
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sm010040
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sm040100
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sm100200
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sm010200
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: soilm000
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: soilm005
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: soilm020
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: soilm040
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: soilm160
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: soilm300
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sw000010
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sw010040
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sw040100
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sw100200
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sw010200
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: soilw000
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: soilw005
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: soilw020
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: soilw040
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: soilw160
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: soilw300
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: st000010
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: st010040
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: st040100
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: st100200
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: st010200
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: soilt000
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: soilt005
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: soilt020
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: soilt040
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: soilt160
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: soilt300
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: landmask
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: topostdv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: toposlpx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: toposlpy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: greenmax
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: greenmin
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: albedomx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: slopecat
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: toposoil
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%num_land_cat,grid%sm32:grid%em32)           :: landusef
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%num_soil_cat,grid%sm32:grid%em32)           :: soilctop
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%num_soil_cat,grid%sm32:grid%em32)           :: soilcbot
real      ,DIMENSION(1:grid%ts_buf_size,1:grid%max_ts_locs)           :: ts_hour
real      ,DIMENSION(1:grid%ts_buf_size,1:grid%max_ts_locs)           :: ts_u
real      ,DIMENSION(1:grid%ts_buf_size,1:grid%max_ts_locs)           :: ts_v
real      ,DIMENSION(1:grid%ts_buf_size,1:grid%max_ts_locs)           :: ts_q
real      ,DIMENSION(1:grid%ts_buf_size,1:grid%max_ts_locs)           :: ts_t
real      ,DIMENSION(1:grid%ts_buf_size,1:grid%max_ts_locs)           :: ts_psfc
real      ,DIMENSION(1:grid%ts_buf_size,1:grid%max_ts_locs)           :: ts_tsk
real      ,DIMENSION(1:grid%ts_buf_size,1:grid%max_ts_locs)           :: ts_tslb
real      ,DIMENSION(1:grid%ts_buf_size,1:grid%max_ts_locs)           :: ts_clw
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: moist
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_moist)           :: dfi_moist
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_scalar)           :: scalar
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_scalar)           :: scalar_bxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_scalar)           :: scalar_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_scalar)           :: scalar_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_scalar)           :: scalar_bye
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_scalar)           :: scalar_btxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_scalar)           :: scalar_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_scalar)           :: scalar_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_scalar)           :: scalar_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32,num_dfi_scalar)           :: dfi_scalar
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bye
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32,num_chem)           :: chem
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%num_soil_layers,grid%sm32:grid%em32)           :: smois
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%num_soil_layers,grid%sm32:grid%em32)           :: tslb
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: gsw
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: xlat
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: xlong
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: xland
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: raincv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: psfc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: th2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: t2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: u10
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: v10
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: xice
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: lai
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: smstav
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: smstot
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sfcrunoff
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: udrunoff
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: ivgtyp
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: isltyp
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: vegfra
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sfcevp
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: grdflx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: albbck
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sfcexc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: snotime
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: acsnow
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: acsnom
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: rmol
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: snow
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: canwat
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sst
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: weasd
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: znt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: mol
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: noahres
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: tke_myj
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32)           :: el_myj
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32)           :: exch_h
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32)           :: exch_m
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: thz0
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: qz0
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: uz0
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: vz0
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: flhc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: flqc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: qsg
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: qvg
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: qcg
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: soilt1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: tsnav
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: psfc_out
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: uz0h
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: vz0h
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: dudt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: dvdt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: qsfc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: akhs
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: akms
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: htop
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hbot
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: htopr
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hbotr
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: htopd
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hbotd
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: htops
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hbots
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: cuppt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: cprate
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32)           :: f_ice_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32)           :: f_rain_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32)           :: f_rimef_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: mass_flux
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: apr_gr
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: apr_w
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: apr_mc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: apr_st
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: apr_as
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: apr_capma
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: apr_capme
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: apr_capmi
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,1:grid%ensdim)           :: xf_ens
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,1:grid%ensdim)           :: pr_ens
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32)           :: rthften
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32)           :: rqvften
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: snowh
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: rhosn
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%num_soil_layers,grid%sm32:grid%em32)           :: smfr3d
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%num_soil_layers,grid%sm32:grid%em32)           :: keepfr3dflag
real      ,DIMENSION(1:7501)             :: mp_restart_state
real      ,DIMENSION(1:7501)             :: tbpvs_state
real      ,DIMENSION(1:7501)             :: tbpvs0_state
real      ,DIMENSION(1:7501)             :: lu_state
!ENDOFREGISTRYGENERATEDINCLUDE

      TYPE(GRID_CONFIG_REC_TYPE) :: CONFIG_FLAGS





      INTEGER :: IDS,IDE,JDS,JDE,KDS,KDE                               &
     &          ,IMS,IME,JMS,JME,KMS,KME                                &
     &          ,IPS,IPE,JPS,JPE,KPS,KPE

      INTEGER :: ERROR,LOOP

      REAL,ALLOCATABLE,DIMENSION(:) :: PHALF

      REAL :: EPSB=0.1,EPSIN=9.8

      INTEGER :: JHL=7

      INTEGER :: I,IEND,IER,IFE,IFS,IHH,IHL,IHRSTB,II,IRTN          &
     &          ,ISIZ1,ISIZ2,ISTART,ISTAT,IX,J,J00,JFE,JFS,JHH,JJ       &
     &          ,JM1,JM2,JM3,JP1,JP2,JP3,JX,KK                          &
     &          ,K,K400,KBI,KBI2,KCCO2,KNT,KNTI                         &
     &          ,LB,LRECBC,L                                            &
     &          ,N,NMAP,NRADLH,NRADSH,NREC,NS,RECL,STAT                 &
     &          ,STEPBL,STEPCU,STEPRA

      INTEGER :: MY_E,MY_N,MY_S,MY_W                                    &
     &          ,MY_NE,MY_NW,MY_SE,MY_SW,MYI,MYJ,NPE

      INTEGER :: I_M

      INTEGER :: ILPAD2,IRPAD2,JBPAD2,JTPAD2
      INTEGER :: ITS,ITE,JTS,JTE,KTS,KTE,NTSD_current

      INTEGER,DIMENSION(3) :: LPTOP

      REAL :: ADDL,APELM,APELMNW,APEM1,CAPA,CLOGES,DPLM,DZLM,EPS,ESE   &
     &       ,FAC1,FAC2,PDIF,PLM,PM1,PSFCK,PSS,PSUM,QLM,RANG           &
     &       ,SLPM,TERM1,THLM,TIME,TLM,TSFCK,ULM,VLM


      REAL :: CWML,EXNSFC,G_INV,PLYR,PSURF,ROG,SFCZ,THSIJ,TL
      REAL :: TEND



      INTEGER,ALLOCATABLE,DIMENSION(:,:) :: ITEMP,LOWLYR
      REAL,ALLOCATABLE,DIMENSION(:) :: SFULL,SMID
      REAL,ALLOCATABLE,DIMENSION(:) :: DZS,ZS
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: RQCBLTEN,RQIBLTEN            &
     &                                    ,RQVBLTEN,RTHBLTEN            &
     &                                    ,RUBLTEN,RVBLTEN              &
     &                                    ,RQCCUTEN,RQICUTEN,RQRCUTEN   &
     &                                    ,RQSCUTEN,RQVCUTEN,RTHCUTEN   &
     &                                    ,RTHRATEN                     &
     &                                    ,RTHRATENLW,RTHRATENSW
      REAL,ALLOCATABLE,DIMENSION(:,:) :: EMISS,EMTEMP,GLW,HFX           &
     &                                  ,NCA                            &
     &                                  ,QFX,RAINBL,RAINC,RAINNC        &
     &                                  ,RAINNCV                        &
     &                                  ,SNOWC,THC,TMN,TSFC

      REAL,ALLOCATABLE,DIMENSION(:,:) :: Z0_DUM, ALBEDO_DUM

      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: ZINT,RRI,CONVFAC,ZMID
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: T_TRANS,PINT_TRANS
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: CLDFRA_TRANS
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: CLDFRA_OLD
      LOGICAL :: E_BDY,N_BDY,S_BDY,W_BDY,WARM_RAIN,ADV_MOIST_COND
      LOGICAL :: START_OF_SIMULATION
      LOGICAL :: LRESTART


      integer :: jam,retval
      CHARACTER(LEN=255) :: message
      integer myproc
      real :: dsig,dsigsum,pdbot,pdtot,rpdtot
      real :: fisx,ht,prodx,rg
      integer :: i_t=096,j_t=195,n_t=11
      integer :: i_u=49,j_u=475,n_u=07
      integer :: i_v=49,j_v=475,n_v=07
      integer :: num_ozmixm, num_aerosolc
      real :: cen_lat,cen_lon,dtphs   
      integer :: num_urban_layers

      INTEGER :: hr, mn, sec, ms, rc, hr_start
      TYPE(WRFU_Time) :: currentTime,startTime

      INTEGER :: interval_seconds, restart_interval


 
      REAL,DIMENSION(0:30) :: VZ0TBL_24
      VZ0TBL_24= (/0.,                                                 &
     &            1.00,  0.07,  0.07,  0.07,  0.07,  0.15,             &
     &            0.08,  0.03,  0.05,  0.86,  0.80,  0.85,             &
     &            2.65,  1.09,  0.80,  0.001, 0.04,  0.05,             &
     &            0.01,  0.04,  0.06,  0.05,  0.03,  0.001,            &
     &            0.000, 0.000, 0.000, 0.000, 0.000, 0.000/)
 



!STARTOFREGISTRYGENERATEDINCLUDE 'inc/scalar_derefs.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
! BEGIN SCALAR DEREFS
 moved = grid%moved
 ntsd = grid%ntsd
 nstart_hour = grid%nstart_hour
 hcoeff_tot = grid%hcoeff_tot
 dy_nmm = grid%dy_nmm
 cpgfv = grid%cpgfv
 en = grid%en
 ent = grid%ent
 f4d = grid%f4d
 f4q = grid%f4q
 ef4t = grid%ef4t
 upstrm = grid%upstrm
 dlmd = grid%dlmd
 dphd = grid%dphd
 pdtop = grid%pdtop
 pt = grid%pt
 micro_start = grid%micro_start
 hydro = grid%hydro
 nphs0 = grid%nphs0
 nprec = grid%nprec
 nclod = grid%nclod
 nheat = grid%nheat
 nrdlw = grid%nrdlw
 nrdsw = grid%nrdsw
 nsrfc = grid%nsrfc
 avrain = grid%avrain
 avcnvc = grid%avcnvc
 aratim = grid%aratim
 acutim = grid%acutim
 ardlw = grid%ardlw
 ardsw = grid%ardsw
 asrfc = grid%asrfc
 aphtim = grid%aphtim
 imicrogram = grid%imicrogram
 dtbc = grid%dtbc
 landuse_isice = grid%landuse_isice
 landuse_lucats = grid%landuse_lucats
 landuse_luseas = grid%landuse_luseas
 landuse_isn = grid%landuse_isn
 number_at_same_level = grid%number_at_same_level
 itimestep = grid%itimestep
 xtime = grid%xtime
 julian = grid%julian
 lbc_fid = grid%lbc_fid
 tiled = grid%tiled
 patched = grid%patched
 oid = grid%oid
 auxhist1_oid = grid%auxhist1_oid
 auxhist2_oid = grid%auxhist2_oid
 auxhist3_oid = grid%auxhist3_oid
 auxhist4_oid = grid%auxhist4_oid
 auxhist5_oid = grid%auxhist5_oid
 auxhist6_oid = grid%auxhist6_oid
 auxhist7_oid = grid%auxhist7_oid
 auxhist8_oid = grid%auxhist8_oid
 auxhist9_oid = grid%auxhist9_oid
 auxhist10_oid = grid%auxhist10_oid
 auxhist11_oid = grid%auxhist11_oid
 auxinput1_oid = grid%auxinput1_oid
 auxinput2_oid = grid%auxinput2_oid
 auxinput3_oid = grid%auxinput3_oid
 auxinput4_oid = grid%auxinput4_oid
 auxinput5_oid = grid%auxinput5_oid
 auxinput6_oid = grid%auxinput6_oid
 auxinput7_oid = grid%auxinput7_oid
 auxinput8_oid = grid%auxinput8_oid
 auxinput9_oid = grid%auxinput9_oid
 auxinput10_oid = grid%auxinput10_oid
 auxinput11_oid = grid%auxinput11_oid
 nodyn_dummy = grid%nodyn_dummy
! END SCALAR DEREFS
!ENDOFREGISTRYGENERATEDINCLUDE





      CALL GET_IJK_FROM_GRID(GRID,                                     &
     &                       IDS,IDE,JDS,JDE,KDS,KDE,                  &
     &                       IMS,IME,JMS,JME,KMS,KME,                  &
     &                       IPS,IPE,JPS,JPE,KPS,KPE)

      ITS=IPS
      ITE=IPE
      JTS=JPS
      JTE=JPE
      KTS=KPS
      KTE=KPE

      CALL model_to_grid_config_rec(grid%id,model_config_rec           &
     &                             ,config_flags)

        RESTRT=config_flags%restart



        CALL domain_clock_get( grid, current_time=currentTime )
        CALL WRFU_TimeGet( currentTime, YY=grid%julyr, dayOfYear=grid%julday, &
                           H=hr, M=mn, S=sec, MS=ms, rc=rc)

	write(0,*) 'WRFU_Timeget results: ', grid%julyr, grid%julday, hr, mn, sec

        CALL domain_clock_get( grid, simulationStartTime=startTime )
        CALL WRFU_TimeGet( startTime, YY=grid%julyr, dayOfYear=grid%julday, &
                           H=hr_start, M=mn, S=sec, MS=ms, rc=rc)

	write(0,*) 'WRFU_Timeget results for start_time: ', grid%julyr, grid%julday, hr_start, mn, sec



	grid%tstart=hr-hr_start

	write(0,*) 'have a grid%tstart : ', grid%tstart



      IF(IME>2600 )THEN
        WRITE(wrf_err_message,*)                                       &
         'start_domain_nmm ime (',ime,') > ',2600,    &
         '. Increase NMM_MAX_DIM in configure.wrf, clean, and recompile.'
        CALL wrf_error_fatal3("start_domain_nmm.b",214,&
wrf_err_message)
      ENDIF

      IF(JME>2600 )THEN
        WRITE(wrf_err_message,*)                                       &
         'start_domain_nmm jme (',jme,') > ',2600,    &
         '. Increase NMM_MAX_DIM in configure.wrf, clean, and recompile.'
        CALL wrf_error_fatal3("start_domain_nmm.b",222,&
wrf_err_message)
      ENDIF



      WRITE(message,196)IHRST,IDAT
      CALL wrf_message(trim(message))
  196 FORMAT(' FORECAST BEGINS ',I2,' GMT ',2(I2,'/'),I4)




      CALL nl_get_interval_seconds(GRID%ID, interval_seconds)
      CALL nl_get_restart_interval(GRID%ID, restart_interval)
      IF (MOD(restart_interval*60,interval_seconds) /= 0) THEN
         WRITE(wrf_err_message,*)' restart_interval is not integer multiplier of interval_seconds'
         CALL wrf_error_fatal3("start_domain_nmm.b",256,&
wrf_err_message)
      END IF



      NPES=1

      MY_IS_GLB=IPS
      MY_IE_GLB=IPE-1
      MY_JS_GLB=JPS
      MY_JE_GLB=JPE-1

      IM=IPE-1
      JM=JPE-1





      MYIS=MAX(IDS,IPS)
      MYIE=MIN(IDE-1,IPE)
      MYJS=MAX(JDS,JPS)
      MYJE=MIN(JDE-1,JPE)

      MYIS1  =MAX(IDS+1,IPS)
      MYIE1  =MIN(IDE-2,IPE)
      MYJS2  =MAX(JDS+2,JPS)
      MYJE2  =MIN(JDE-3,JPE)

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

      MYIS1_P1=MAX(IDS+1,IPS-1)
      MYIE1_P1=MIN(IDE-2,IPE+1)
      MYIS1_P2=MAX(IDS+1,IPS-2)
      MYIE1_P2=MIN(IDE-2,IPE+2)

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



      CALL WRF_GET_MYPROC(MYPROC)
      MYPE=MYPROC





















      call wrf_get_nprocx(inpes)
      call wrf_get_nprocy(jnpes)

      allocate(itemp(inpes,jnpes),stat=istat)
      npe=0

      do j=1,jnpes
      do i=1,inpes
        itemp(i,j)=npe
        if(npe==mype)then
          myi=i
          myj=j
        endif
        npe=npe+1
      enddo
      enddo

      my_n=-1
      if(myj+1<=jnpes)my_n=itemp(myi,myj+1)

      my_e=-1
      if(myi+1<=inpes)my_e=itemp(myi+1,myj)

      my_s=-1
      if(myj-1>=1)my_s=itemp(myi,myj-1)

      my_w=-1
      if(myi-1>=1)my_w=itemp(myi-1,myj)

      my_ne=-1
      if((myi+1<=inpes).and.(myj+1<=jnpes)) &
         my_ne=itemp(myi+1,myj+1)

      my_se=-1
      if((myi+1<=inpes).and.(myj-1>=1)) &
         my_se=itemp(myi+1,myj-1)

      my_sw=-1
      if((myi-1>=1).and.(myj-1>=1)) &
         my_sw=itemp(myi-1,myj-1)

      my_nw=-1
      if((myi-1>=1).and.(myj+1<=jnpes)) &
         my_nw=itemp(myi-1,myj+1)










      deallocate(itemp)
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_1.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_1_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_2.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_2_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_3.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_3_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_4.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_4_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_5.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_5_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_6.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_6_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_7.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_7_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_8.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_8_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_9.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_9_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_10.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_10_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_11.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_11_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_12.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_12_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_13.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_13_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_14.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_14_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_15.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_15_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_16.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_16_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_17.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_17_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_18.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_18_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_19.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_19_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_20.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_20_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_21.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_21_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_22.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_22_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_23.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_23_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_24.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_24_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_25.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_25_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_26.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_26_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_27.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_27_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_28.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_28_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_29.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_29_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_30.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_30_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_31.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_31_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_32.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_32_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_33.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_33_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_34.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_34_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_35.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_35_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_36.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_36_sub ( grid, &
  config_flags, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_37.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_37_sub ( grid, &
  config_flags, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_38.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_38_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_39.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_39_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE

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

      CAPA=R_D/CP
      LM=KPE-KPS+1

      IFS=IPS
      JFS=JPS
      JFE=MIN(JPE,JDE-1)
      IFE=MIN(IPE,IDE-1)

      IF(.NOT.RESTRT)THEN
        DO J=JFS,JFE
        DO I=IFS,IFE
          PDSL(I,J)  =PD(I,J)*RES(I,J)
          PREC(I,J)  =0.
          IF(allowed_to_read)ACPREC(I,J)=0.  
          CUPREC(I,J)=0.
          rg=1./g
          ht=fis(i,j)*rg






          QSH(I,J)   =0.
          AKMS(I,J)  =0.
          AKHS(I,J)  =0.
          TWBS(I,J)  =0.
          QWBS(I,J)  =0.
          IF(allowed_to_read)THEN       
          CLDEFI(I,J)=1.
          HTOP(I,J)  =REAL(KTS)
          HTOPD(I,J) =REAL(KTS)
          HTOPS(I,J) =REAL(KTS)
          HBOT(I,J)  =REAL(KTE)
          HBOTD(I,J) =REAL(KTE)
          HBOTS(I,J) =REAL(KTE)
          ENDIF















      IF(allowed_to_read)THEN       

          PM1=AETA1(KTS)*PDTOP+AETA2(KTS)*PDSL(I,J)+PT
          APEM1=(1.E5/PM1)**CAPA

        IF(NMM_TSK(I,J)>=200.)THEN         
          THS(I,J)=NMM_TSK(I,J)*APEM1
          TSFCK=NMM_TSK(I,J)
	ELSE                               
          THS(I,J)=T(I,J,KTS)*APEM1
          TSFCK=T(I,J,KTS)
	ENDIF

          PSFCK=PD(I,J)+PDTOP+PT

          IF(SM(I,J)<0.5) THEN
            QSH(I,J)=PQ0/PSFCK*EXP(A2*(TSFCK-A3)/(TSFCK-A4))
          ELSEIF(SM(I,J)>0.5) THEN
            THS(I,J)=SST(I,J)*(1.E5/(PD(I,J)+PDTOP+PT))**CAPA
          ENDIF

          TERM1=-0.068283/T(I,J,KTS)
          PSHLTR(I,J)=(PD(I,J)+PDTOP+PT)*EXP(TERM1)

          USTAR(I,J)=0.1
          THZ0(I,J)=THS(I,J)
          QZ0(I,J)=QSH(I,J)
          UZ0(I,J)=0.
          VZ0(I,J)=0.

      ENDIF  

        ENDDO
        ENDDO




      IF (MAXVAL(CWM) .gt. 0. .and. MAXVAL(CWM) .lt. 1.) then
        CALL wrf_message('appear to have CWM values...do not zero')
      ELSE
        IF(allowed_to_read)THEN       
        CALL wrf_message('zeroing CWM')
        DO K=KPS,KPE
          DO J=JFS,JFE
          DO I=IFS,IFE
            CWM(I,J,K)=0.
          ENDDO
          ENDDO
        ENDDO
        ENDIF
      ENDIF



        ARDSW=0.0
        ARDLW=0.0
        ASRFC=0.0
        AVRAIN=0.0
        AVCNVC=0.0

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



        EPS=R_D/R_V

      IF(allowed_to_read)THEN       
        DO J=JFS,JFE
        DO I=IFS,IFE
          IF(SM(I,J)>0.5)THEN
            CLOGES =-CM1/SST(I,J)-CM2*ALOG10(SST(I,J))+CM3
            ESE    = 10.**(CLOGES+2.)
            QSH(I,J)= SM(I,J)*EPS*ESE/(PD(I,J)+PDTOP+PT-ESE*(1.-EPS))
          ENDIF
        ENDDO
        ENDDO
      ENDIF








      IF (MAXVAL(Q2) .gt. epsq2 .and. MAXVAL(Q2) .lt. 200.) then
        CALL wrf_message('appear to have Q2 values...do not zero')
      ELSE
      IF(allowed_to_read)THEN       
        CALL wrf_message('zeroing Q2')
        DO K=KPS,KPE-1
        DO J=JFS,JFE
        DO I=IFS,IFE
          Q2(I,J,K)=HBM2(I,J)*EPSQ2
        ENDDO
        ENDDO
        ENDDO

        DO J=JFS,JFE
        DO I=IFS,IFE
          Q2(I,J,LM)    = 0.
          Q2(I,J,KTE-2)= HBM2(I,J)*Q2INI
          Q2(I,J,KTE-1)= HBM2(I,J)*Q2INI
        ENDDO
        ENDDO
      ENDIF
      ENDIF




        DO K=KPS,KPE
        DO J=JFS,JFE
        DO I=IFS,IFE
          IF(Q(I,J,K)<EPSQ)Q(I,J,K)=EPSQ
          TRAIN(I,J,K)=0.
          TCUCN(I,J,K)=0.
        ENDDO
        ENDDO
        ENDDO




        DO J=JFS,JFE
        DO I=IFS,IFE
          TLMAX(I,J)=T(I,J,KPS)
          TLMIN(I,J)=T(I,J,KPS)
        ENDDO
        ENDDO





        CALL wrf_message('INIT:  INITIALIZED ARRAYS FOR CLEAN START')
      ENDIF 

      IF(NEST)THEN
        DO J=JFS,JFE
        DO I=IFS,IFE

          IF(T(I,J,KTS)==0.)THEN
            T(I,J,KTS)=T(I,J,KTS+1)
          ENDIF

          TERM1=-0.068283/T(I,J,KTS)
          PSHLTR(I,J)=(PD(I,J)+PDTOP+PT)*EXP(TERM1)
        ENDDO
        ENDDO
      ENDIF






      TSPH=3600./GRID%DT 
      NPHS0=GRID%NPHS

      IF(MYPE==0)THEN
        WRITE( wrf_err_message, * )' start_nmm TSTART=',grid%tstart
        CALL wrf_debug( 1, TRIM(wrf_err_message) )
        WRITE( wrf_err_message, * )' start_nmm TPREC=',grid%tprec
        CALL wrf_debug( 1, TRIM(wrf_err_message) )
        WRITE( wrf_err_message, * )' start_nmm THEAT=',grid%theat
        CALL wrf_debug( 1, TRIM(wrf_err_message) )
        WRITE( wrf_err_message, * )' start_nmm TCLOD=',grid%tclod
        CALL wrf_debug( 1, TRIM(wrf_err_message) )
        WRITE( wrf_err_message, * )' start_nmm TRDSW=',grid%trdsw
        CALL wrf_debug( 1, TRIM(wrf_err_message) )
        WRITE( wrf_err_message, * )' start_nmm TRDLW=',grid%trdlw
        CALL wrf_debug( 1, TRIM(wrf_err_message) )
        WRITE( wrf_err_message, * )' start_nmm TSRFC=',grid%tsrfc
        CALL wrf_debug( 1, TRIM(wrf_err_message) )
        WRITE( wrf_err_message, * )' start_nmm PCPFLG=',grid%pcpflg
        CALL wrf_debug( 1, TRIM(wrf_err_message) )
      ENDIF

      NSTART = INT(grid%TSTART*TSPH+0.5)

	write(0,*) 'here with grid%TSTART, NSTART: ', grid%TSTART, NSTART
      NTSD = NSTART
	write(0,*) 'NTSD now: ', ntsd





      NPREC  = INT(grid%TPREC *TSPH+0.5)
      NHEAT  = INT(grid%THEAT *TSPH+0.5)
      NCLOD  = INT(grid%TCLOD *TSPH+0.5)
      NRDSW  = INT(grid%TRDSW *TSPH+0.5)
      NRDLW  = INT(grid%TRDLW *TSPH+0.5)
      NSRFC  = INT(grid%TSRFC *TSPH+0.5)








      MICRO_START=.TRUE.






      DO J=JFS,JFE
      DO I=IFS,IFE
        ADT(I,J)=0.
        ADU(I,J)=0.
        ADV(I,J)=0.
      ENDDO
      ENDDO





      DO J=JFS,JFE
        N_IUP_H(J)=0
        N_IUP_V(J)=0
        N_IUP_ADH(J)=0
        N_IUP_ADV(J)=0

        DO I=IFS,IFE
          IUP_H(I,J)=-999
          IUP_V(I,J)=-999
          IUP_ADH(I,J)=-999
          IUP_ADV(I,J)=-999
        ENDDO

      ENDDO



















      UPSTRM=.FALSE.

      S_BDY=(JPS==JDS)
      N_BDY=(JPE==JDE)
      W_BDY=(IPS==IDS)
      E_BDY=(IPE==IDE)

      JTPAD2=2
      JBPAD2=2
      IRPAD2=2
      ILPAD2=2

      IF(S_BDY)THEN
        UPSTRM=.TRUE.
        JBPAD2=0

        DO JJ=1,7
          J=JJ      
          KNTI=0
          DO I=MYIS_P2,MYIE_P2
            IUP_H(IMS+KNTI,J)=I
            IUP_V(IMS+KNTI,J)=I
            KNTI=KNTI+1
          ENDDO
          N_IUP_H(J)=KNTI
          N_IUP_V(J)=KNTI
        ENDDO

        DO JJ=3,5
          J=JJ      
          KNTI=0
          ISTART=MYIS1_P2
          IEND=MYIE1_P2
          IF(E_BDY)IEND=IEND-MOD(JJ+1,2)
          DO I=ISTART,IEND
            IUP_ADH(IMS+KNTI,J)=I
            KNTI=KNTI+1
          ENDDO
          N_IUP_ADH(J)=KNTI

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

      IF(N_BDY)THEN
        UPSTRM=.TRUE.
        JTPAD2=0

        DO JJ=JDE-7, JDE-1 
          J=JJ      
          KNTI=0
          DO I=MYIS_P2,MYIE_P2
            IUP_H(IMS+KNTI,J)=I
            IUP_V(IMS+KNTI,J)=I
            KNTI=KNTI+1
          ENDDO
          N_IUP_H(J)=KNTI
          N_IUP_V(J)=KNTI
        ENDDO

        DO JJ=JDE-5, JDE-3 
          J=JJ      
          KNTI=0
          ISTART=MYIS1_P2
          IEND=MYIE1_P2
          IF(E_BDY)IEND=IEND-MOD(JJ+1,2)
          DO I=ISTART,IEND
            IUP_ADH(IMS+KNTI,J)=I
            KNTI=KNTI+1
          ENDDO
          N_IUP_ADH(J)=KNTI

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

      IF(W_BDY)THEN
        UPSTRM=.TRUE.
        ILPAD2=0
        DO JJ=8,JDE-8   
          IF(JJ>=MY_JS_GLB-2.AND.JJ<=MY_JE_GLB+2)THEN
            J=JJ      

            DO I=1,4
              IUP_H(IMS+I-1,J)=I
              IUP_V(IMS+I-1,J)=I
            ENDDO
            N_IUP_H(J)=4
            N_IUP_V(J)=4
          ENDIF
        ENDDO

        DO JJ=6,JDE-6   
          IF(JJ>=MY_JS_GLB-2.AND.JJ<=MY_JE_GLB+2)THEN
            J=JJ      
            KNTI=0
            IEND=2+MOD(JJ,2)
            DO I=2,IEND
              IUP_ADH(IMS+KNTI,J)=I
              KNTI=KNTI+1
            ENDDO
            N_IUP_ADH(J)=KNTI

            KNTI=0
            IEND=2+MOD(JJ+1,2)
            DO I=2,IEND
              IUP_ADV(IMS+KNTI,J)=I
              KNTI=KNTI+1
            ENDDO
            N_IUP_ADV(J)=KNTI

          ENDIF
        ENDDO
      ENDIF

      CALL WRF_GET_NPROCX(INPES)

      IF(E_BDY)THEN
        UPSTRM=.TRUE.
        IRPAD2=0
        DO JJ=8,JDE-8   
          IF(JJ>=MY_JS_GLB-2.AND.JJ<=MY_JE_GLB+2)THEN
            J=JJ      
            IEND=IM-MOD(JJ+1,2)
            ISTART=IEND-3





            KNTI=0
            IF(INPES.EQ.1)KNTI=N_IUP_H(J)

            DO II=ISTART,IEND
              I=II      
              IUP_H(IMS+KNTI,J)=I
              KNTI=KNTI+1
            ENDDO
            N_IUP_H(J)=KNTI
          ENDIF
        ENDDO

        DO JJ=6,JDE-6   
          IF(JJ>=MY_JS_GLB-2.AND.JJ<=MY_JE_GLB+2)THEN
            J=JJ      
            IEND=IM-1-MOD(JJ+1,2)
            ISTART=IEND-MOD(JJ,2)
            KNTI=0
            IF(INPES==1)KNTI=N_IUP_ADH(J)
            DO II=ISTART,IEND
              I=II      
              IUP_ADH(IMS+KNTI,J)=I
              KNTI=KNTI+1
            ENDDO
            N_IUP_ADH(J)=KNTI
          ENDIF
        ENDDO

        DO JJ=8,JDE-8  
          IF(JJ>=MY_JS_GLB-2.AND.JJ<=MY_JE_GLB+2)THEN
            J=JJ      
            IEND=IM-MOD(JJ,2)
            ISTART=IEND-3
            KNTI=0
            IF(INPES==1)KNTI=N_IUP_V(J)

            DO II=ISTART,IEND
              I=II      
              IUP_V(IMS+KNTI,J)=I
              KNTI=KNTI+1
            ENDDO
            N_IUP_V(J)=KNTI
          ENDIF
        ENDDO

        DO JJ=6,JDE-6  
          IF(JJ>=MY_JS_GLB-2.AND.JJ<=MY_JE_GLB+2)THEN
            J=JJ      
            IEND=IM-1-MOD(JJ,2)
            ISTART=IEND-MOD(JJ+1,2)
            KNTI=0
            IF(INPES==1)KNTI=N_IUP_ADV(J)
            DO II=ISTART,IEND
              I=II      
              IUP_ADV(IMS+KNTI,J)=I
              KNTI=KNTI+1
            ENDDO
            N_IUP_ADV(J)=KNTI
          ENDIF
        ENDDO
      ENDIF

      jam=6+2*(JDE-JDS-1-9)



      DO J=MYJS_P5,MYJE_P5
        EM_LOC(J)=-9.E9
        EMT_LOC(J)=-9.E9
      ENDDO

      IF(S_BDY)THEN
        DO J=3,5
          EM_LOC(J)=EM(J-2)
          EMT_LOC(J)=EMT(J-2)
        ENDDO
      ENDIF

      IF(N_BDY)THEN
        KNT=3
        DO JJ=JDE-5,JDE-3 
          KNT=KNT+1
          J=JJ      
          EM_LOC(J)=EM(KNT)
          EMT_LOC(J)=EMT(KNT)
        ENDDO
      ENDIF

      IF(W_BDY)THEN
        KNT=6
        DO JJ=6,JDE-6 
          KNT=KNT+1
          IF(JJ>=MY_JS_GLB-2.AND.JJ<=MY_JE_GLB+2)THEN
            J=JJ      
            EM_LOC(J)=EM(KNT)
            EMT_LOC(J)=EMT(KNT)
          ENDIF
        ENDDO
      ENDIF

      IF(E_BDY)THEN
        KNT=6+JDE-11 
        DO JJ=6,JDE-6 
          KNT=KNT+1
          IF(JJ>=MY_JS_GLB-2.AND.JJ<=MY_JE_GLB+2)THEN
            J=JJ      
            EM_LOC(J)=EM(KNT)
            EMT_LOC(J)=EMT(KNT)
          ENDIF
        ENDDO
      ENDIF




      IF(NSTART.EQ.0)THEN

         GRID%NSOIL= GRID%NUM_SOIL_LAYERS
        DO J=JFS,JFE
        DO I=IFS,IFE
          PCTSNO(I,J)=-999.0
          IF(SM(I,J)<0.5)THEN
              CMC(I,J)=0.0

            IF(SICE(I,J)>0.5)THEN



              SMSTAV(I,J)=1.0
              SMSTOT(I,J)=1.0
              SSROFF(I,J)=0.0
              BGROFF(I,J)=0.0
              CMC(I,J)=0.0
              DO NS=1,GRID%NSOIL
                SMC(I,NS,J)=1.0

                SH2O(I,NS,J)=1.0
              ENDDO
            ENDIF
          ELSE



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
            DO NS=1,GRID%NSOIL
              SMC(I,NS,J)=1.0
              STC(I,NS,J)=273.16

              SH2O(I,NS,J)=1.0
            ENDDO
          ENDIF

        ENDDO
        ENDDO

        APHTIM=0.0
        ARATIM=0.0
        ACUTIM=0.0

      ENDIF










      DO J=JFS,JFE
      DO I=IFS,IFE
        LVL(I,J)=LM-KTE
      ENDDO
      ENDDO





      K400=0
      PSUM=PT
      SLPM=101325.
      PDIF=SLPM-PT
      DO K=1,LM
        PSUM=PSUM+DETA(K)*PDIF
        IF(LPTOP(3)==0)THEN
          IF(PSUM>PHITP)LPTOP(3)=K
        ELSEIF(LPTOP(2)==0)THEN
          IF(PSUM>PMDHI)LPTOP(2)=K
        ELSEIF(K400==0)THEN
          IF(PSUM>P400)K400=K
        ELSEIF(LPTOP(1)==0)THEN
          IF(PSUM>PLOMD)LPTOP(1)=K
        ENDIF
      ENDDO



      KCCO2=0



      PSS=101325.
      PDIF=PSS-PT

      ALLOCATE(PHALF(LM+1),STAT=I)

      DO K=KPS,KPE-1
        PHALF(K+1)=AETA(K)*PDIF+PT
      ENDDO
      

      PHALF(1)=0.
      PHALF(LM+1)=PSS












      TIME=(NTSD-1)*GRID%DT



      ADDL=0.
      IF(MOD(IDAT(3),4)==0)ADDL=1.




      DEALLOCATE(PHALF)




      IF(allowed_to_read.and.(.NOT.RESTRT))THEN       

      DO J=JFS,JFE
      DO I=IFS,IFE



        PDSL(I,J)=PD(I,J)*RES(I,J)

        ULM=U(I,J,KTS)
        VLM=V(I,J,KTS)
        TLM=T(I,J,KTS)
        QLM=Q(I,J,KTS)
        PLM=AETA1(KTS)*PDTOP+AETA2(KTS)*PDSL(I,J)+PT
        APELM=(1.0E5/PLM)**CAPA
          TERM1=-0.068283/T(I,J,KTS)
          PSHLTR(I,J)=(PD(I,J)+PDTOP+PT)*EXP(TERM1)
        APELMNW=(1.0E5/PSHLTR(I,J))**CAPA
        THLM=TLM*APELM
        DPLM=(DETA1(KTS)*PDTOP+DETA2(KTS)*PDSL(I,J))*0.5
        DZLM=R_D*DPLM*TLM*(1.+P608*QLM)/(G*PLM)
        FAC1=10./DZLM
        FAC2=(DZLM-10.)/DZLM
        IF(DZLM<=10.)THEN
          FAC1=1.
          FAC2=0.
        ENDIF

        IF(.NOT.RESTRT)THEN
          TH10(I,J)=FAC2*THS(I,J)+FAC1*THLM
          Q10(I,J)=FAC2*QSH(I,J)+FAC1*QLM
          U10(I,J)=ULM
          V10(I,J)=VLM
        ENDIF








        IF(.NOT.RESTRT.OR.NEST)THEN

          IF ( (THLM-THS(I,J))>2.0) THEN  
            FAC1=0.3
            FAC2=0.7
          ELSE
            FAC1=0.8
            FAC2=0.2
          ENDIF

          TSHLTR(I,J)=FAC2*THS(I,J)+FAC1*THLM

          QSHLTR(I,J)=FAC2*QSH(I,J)+FAC1*QLM

        ENDIF









      ENDDO
      ENDDO

      END IF 





      IF(.NOT.RESTRT)THEN
        DO K=KPS,KPE
          DO J=JFS,JFE
          DO I=ifs,ife
          TOLD(I,J,K)=T(I,J,K)   
          UOLD(I,J,K)=U(I,J,K)   
          VOLD(I,J,K)=V(I,J,K)   
          ENDDO
          ENDDO
        ENDDO
      ENDIF







      IF((.NOT.RESTRT.OR.NEST).AND. allowed_to_read)THEN 
        DO K=KPS,KPE
          DO J=JFS,JFE
          DO I=IFS,IFE
            DWDT(I,J,K)=1.
          ENDDO
          ENDDO
        ENDDO
      ENDIF

      IF(GRID%SIGMA==1)THEN
        DO J=JFS,JFE
        DO I=IFS,IFE
          PDSL(I,J)=PD(I,J)
        ENDDO
        ENDDO
      ELSE
        DO J=JFS,JFE
        DO I=IFS,IFE
          PDSL(I,J)=RES(I,J)*PD(I,J)
        ENDDO
        ENDDO
      ENDIF






      WRITE( wrf_err_message, * )' restrt=',restrt,' nest=',nest
        CALL wrf_debug( 0, TRIM(wrf_err_message) )
      WRITE( wrf_err_message, * )' pdtop=',pdtop,' pt=',pt
        CALL wrf_debug( 0, TRIM(wrf_err_message) )

      IF(.NOT.RESTRT.OR.NEST)THEN
        DO K=KPS,KPE
        DO J=JFS,JFE
        DO I=IFS,IFE
          PINT(I,J,K)=ETA1(K)*PDTOP+ETA2(K)*PDSL(I,J)+PT
          Z(I,J,K)=PINT(I,J,K)
          W(I,J,K)=0.
        ENDDO
        ENDDO
        ENDDO
      ENDIF

        DO K=KTS,KTE-1
        DO J=JFS,JFE
        DO I=IFS,IFE
          RTOP(I,J,K)=(Q(I,J,K)*P608-CWM(I,J,K)+1.)*T(I,J,K)*R_D/ &
                      ((PINT(I,J,K+1)+PINT(I,J,K))*0.5)
        ENDDO
        ENDDO
        ENDDO





      DO J=JFS,JFE
      DO I=IFS,IFE
        DWDTMN(I,J)=-EPSIN
        DWDTMX(I,J)= EPSIN
      ENDDO
      ENDDO


      IF(JHL>1)THEN
        JHH=JDE-1-JHL+1 
        IHL=JHL/2+1

        DO J=1,JHL
          IF(J>=MY_JS_GLB-JBPAD2.AND.J<=MY_JE_GLB+JTPAD2)THEN
            JX=J      
            DO I=1,IDE-1 
              IF(I>=MY_IS_GLB-ILPAD2.AND.I<=MY_IE_GLB+IRPAD2)THEN
                IX=I      
                DWDTMN(IX,JX)=-EPSB
                DWDTMX(IX,JX)= EPSB
              ENDIF
            ENDDO
          ENDIF
        ENDDO

        DO J=JHH,JDE-1   
          IF(J>=MY_JS_GLB-JBPAD2.AND.J<=MY_JE_GLB+JTPAD2)THEN
            JX=J      
            DO I=1,IDE-1 
              IF(I>=MY_IS_GLB-ILPAD2.AND.I<=MY_IE_GLB+IRPAD2)THEN
                IX=I      
                DWDTMN(IX,JX)=-EPSB
                DWDTMX(IX,JX)= EPSB
              ENDIF
            ENDDO
          ENDIF
        ENDDO

        DO J=1,JDE-1 
          IF(J>=MY_JS_GLB-JBPAD2.AND.J<=MY_JE_GLB+JTPAD2)THEN
            JX=J      
            DO I=1,IHL
              IF(I>=MY_IS_GLB-ILPAD2.AND.I<=MY_IE_GLB+IRPAD2)THEN
                IX=I      
                DWDTMN(IX,JX)=-EPSB
                DWDTMX(IX,JX)= EPSB
              ENDIF
            ENDDO
          ENDIF
        ENDDO

        DO J=1,JDE-1 
          IF(J>=MY_JS_GLB-JBPAD2.AND.J<=MY_JE_GLB+JTPAD2)THEN
            JX=J      
             
            IHH=IDE-1-IHL+MOD(J,2) 
            DO I=IHH,IDE-1 
              IF(I>=MY_IS_GLB-ILPAD2.AND.I<=MY_IE_GLB+IRPAD2)THEN
                IX=I      
                DWDTMN(IX,JX)=-EPSB
                DWDTMX(IX,JX)= EPSB
              ENDIF
            ENDDO
          ENDIF
        ENDDO

      ENDIF







      ALLOCATE(SFULL(KMS:KME),STAT=I)           ; SFULL    = 0.
      ALLOCATE(SMID(KMS:KME),STAT=I)            ; SMID     = 0.
      ALLOCATE(EMISS(IMS:IME,JMS:JME),STAT=I)   ; EMISS    = 0.
      ALLOCATE(EMTEMP(IMS:IME,JMS:JME),STAT=I)  ; EMTEMP   = 0.
      ALLOCATE(GLW(IMS:IME,JMS:JME),STAT=I)     ; GLW      = 0.
      ALLOCATE(HFX(IMS:IME,JMS:JME),STAT=I)     ; HFX      = 0.
      ALLOCATE(LOWLYR(IMS:IME,JMS:JME),STAT=I)  ; LOWLYR   = 0.

      ALLOCATE(NCA(IMS:IME,JMS:JME),STAT=I)     ; NCA      = 0.
      ALLOCATE(QFX(IMS:IME,JMS:JME),STAT=I)     ; QFX      = 0.
      ALLOCATE(RAINBL(IMS:IME,JMS:JME),STAT=I)  ; RAINBL   = 0.
      ALLOCATE(RAINC(IMS:IME,JMS:JME),STAT=I)   ; RAINC    = 0.
      ALLOCATE(RAINNC(IMS:IME,JMS:JME),STAT=I)  ; RAINNC   = 0.
      ALLOCATE(RAINNCV(IMS:IME,JMS:JME),STAT=I) ; RAINNCV  = 0.

      ALLOCATE(ZS(KMS:KME),STAT=I)              ; ZS       = 0.
      ALLOCATE(SNOWC(IMS:IME,JMS:JME),STAT=I)   ; SNOWC    = 0.
      ALLOCATE(THC(IMS:IME,JMS:JME),STAT=I)     ; THC      = 0.
      ALLOCATE(TMN(IMS:IME,JMS:JME),STAT=I)     ; TMN      = 0.
      ALLOCATE(TSFC(IMS:IME,JMS:JME),STAT=I)    ; TSFC     = 0.
      ALLOCATE(Z0_DUM(IMS:IME,JMS:JME),STAT=I)  ; Z0_DUM   = 0.
      ALLOCATE(ALBEDO_DUM(IMS:IME,JMS:JME),STAT=I)  ; ALBEDO_DUM   = 0.

      ALLOCATE(DZS(KMS:KME),STAT=I)                         ; DZS = 0.
      ALLOCATE(RQCBLTEN(IMS:IME,KMS:KME,JMS:JME),STAT=I)    ; RQCBLTEN = 0.
      ALLOCATE(RQIBLTEN(IMS:IME,KMS:KME,JMS:JME),STAT=I)    ; RQIBLTEN = 0.
      ALLOCATE(RQVBLTEN(IMS:IME,KMS:KME,JMS:JME),STAT=I)    ; RQVBLTEN =  0.
      ALLOCATE(RTHBLTEN(IMS:IME,KMS:KME,JMS:JME),STAT=I)    ; RTHBLTEN =  0.
      ALLOCATE(RUBLTEN(IMS:IME,KMS:KME,JMS:JME),STAT=I)     ; RUBLTEN = 0.
      ALLOCATE(RVBLTEN(IMS:IME,KMS:KME,JMS:JME),STAT=I)     ; RVBLTEN = 0.
      ALLOCATE(RQCCUTEN(IMS:IME,KMS:KME,JMS:JME),STAT=I)    ; RQCCUTEN = 0.
      ALLOCATE(RQICUTEN(IMS:IME,KMS:KME,JMS:JME),STAT=I)    ; RQICUTEN  = 0.
      ALLOCATE(RQRCUTEN(IMS:IME,KMS:KME,JMS:JME),STAT=I)    ; RQRCUTEN = 0.
      ALLOCATE(RQSCUTEN(IMS:IME,KMS:KME,JMS:JME),STAT=I)    ; RQSCUTEN = 0.
      ALLOCATE(RQVCUTEN(IMS:IME,KMS:KME,JMS:JME),STAT=I)    ; RQVCUTEN = 0.
      ALLOCATE(RTHCUTEN(IMS:IME,KMS:KME,JMS:JME),STAT=I)    ; RTHCUTEN = 0.
      ALLOCATE(RTHRATEN(IMS:IME,KMS:KME,JMS:JME),STAT=I)    ; RTHRATEN  = 0.
      ALLOCATE(RTHRATENLW(IMS:IME,KMS:KME,JMS:JME),STAT=I)  ; RTHRATENLW = 0.
      ALLOCATE(RTHRATENSW(IMS:IME,KMS:KME,JMS:JME),STAT=I)  ; RTHRATENSW = 0.
      ALLOCATE(ZINT(IMS:IME,KMS:KME,JMS:JME),STAT=I)  ; ZINT = 0.
      ALLOCATE(CONVFAC(IMS:IME,KMS:KME,JMS:JME),STAT=I)  ; CONVFAC = 0.
      ALLOCATE(PINT_TRANS(IMS:IME,KMS:KME,JMS:JME),STAT=I)  ; PINT_TRANS = 0.
      ALLOCATE(T_TRANS(IMS:IME,KMS:KME,JMS:JME),STAT=I)  ;  T_TRANS = 0.
      ALLOCATE(RRI(IMS:IME,KMS:KME,JMS:JME),STAT=I)  ;  RRI = 0.
      ALLOCATE(CLDFRA_TRANS(IMS:IME,KMS:KME,JMS:JME),STAT=I)  ; CLDFRA_TRANS = 0.
      ALLOCATE(CLDFRA_OLD(IMS:IME,KMS:KME,JMS:JME),STAT=I)  ; CLDFRA_OLD = 0.


      G_INV=1./G
      ROG=R_D*G_INV
      GRID%RADT=GRID%NRADS*GRID%DT/60.
      GRID%BLDT=GRID%NPHS*GRID%DT/60.
      GRID%CUDT=GRID%NCNVC*GRID%DT/60.
      GRID%GSMDT=GRID%NPHS*GRID%DT/60.

      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        SFCZ=FIS(I,J)*G_INV
        ZINT(I,KTS,J)=SFCZ
        PDSL(I,J)=PD(I,J)*RES(I,J)
        PSURF=PINT(I,J,KTS)
        EXNSFC=(1.E5/PSURF)**CAPA
        XLAND(I,J)=SM(I,J)+1.
        THSIJ=(SST(I,J)*EXNSFC)*(XLAND(I,J)-1.)                         &
     &        +THS(I,J)*(2.-SM(I,J))
        TSFC(I,J)=THSIJ/EXNSFC

        DO K=KTS,KTE-1
          PLYR=(PINT(I,J,K)+PINT(I,J,K+1))*0.5
          TL=T(I,J,K)
          CWML=CWM(I,J,K)
          RRI(I,K,J)=R_D*TL*(1.+P608*Q(I,J,K))/PLYR
          ZINT(I,K+1,J)=ZINT(I,K,J)+TL/PLYR                             & 
                     *(DETA1(K)*PDTOP+DETA2(K)*PDSL(I,J))*ROG        & 
                     *(Q(I,J,K)*P608-CWML+1.)
        ENDDO




      ENDDO
      ENDDO







      PDTOT=101325.-PT
      RPDTOT=1./PDTOT
      PDBOT=PDTOT-PDTOP
      SFULL(KTS)=1.
      SFULL(KTE)=0.
      DSIGSUM = 0.
      DO K=KTS+1,KTE
        DSIG=(DETA1(K-1)*PDTOP+DETA2(K-1)*PDBOT)*RPDTOT
        DSIGSUM=DSIGSUM+DSIG
        SFULL(K)=SFULL(K-1)-DSIG
        SMID(K-1)=0.5*(SFULL(K-1)+SFULL(K))
      ENDDO
      DSIG=(DETA1(KTE-1)*PDTOP+DETA2(KTE-1)*PDBOT)*RPDTOT
      DSIGSUM=DSIGSUM+DSIG
      SMID(KTE-1)=0.5*(SFULL(KTE-1)+SFULL(KTE))



      LU_INDEX=IVGTYP

      IF(.NOT.RESTRT)THEN
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          Z0_DUM(I,J)=Z0(I,J) 
          ALBEDO_DUM(I,J)=ALBEDO(I,J) 
        ENDDO
        ENDDO
      ENDIF


                                                                                                                                              
      IF(.NOT.RESTRT)THEN
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE

          IF(SM(I,J)==0)then
            Z0BASE(I,J)=VZ0TBL_24(IVGTYP(I,J))+Z0LAND
          ELSE
            Z0BASE(I,J)=VZ0TBL_24(IVGTYP(I,J))+Z0SEA
          ENDIF

        ENDDO
        ENDDO
      ENDIF


      num_ozmixm=1
      num_aerosolc=1







      CALL domain_setgmtetc( GRID, START_OF_SIMULATION )

      if(restrt) then
        CALL domain_clock_get( grid, current_time=currentTime )
        CALL WRFU_TimeGet( currentTime, YY=grid%julyr, dayOfYear=grid%julday, &
                           H=hr, M=mn, S=sec, MS=ms, rc=rc)
        grid%gmt=hr+real(mn)/60.+real(sec)/3600.+real(ms)/(1000*3600)
        WRITE( wrf_err_message , * ) 'DEBUG start_domain_nmm():  gmt = ',grid%gmt
        CALL wrf_debug( 150, TRIM(wrf_err_message) )
      endif




      MOVED = .FALSE.

      IF (GRID%RESTART) THEN
         LRESTART = GRID%RESTART
      ELSE
         IF (moved) THEN
            LRESTART = .TRUE.
         ELSE
            LRESTART = .FALSE.
         ENDIF
      END IF

      CALL PHY_INIT(GRID%ID,CONFIG_FLAGS,GRID%DT,LRESTART,SFULL,SMID    &
     &             ,PT,TSFC,GRID%RADT,GRID%BLDT,GRID%CUDT,GRID%GSMDT    &
     &             ,RTHCUTEN, RQVCUTEN, RQRCUTEN                        &
     &             ,RQCCUTEN, RQSCUTEN, RQICUTEN                        &
     &             ,RUBLTEN,RVBLTEN,RTHBLTEN                            &
     &             ,RQVBLTEN,RQCBLTEN,RQIBLTEN                          &
     &             ,RTHRATEN,RTHRATENLW,RTHRATENSW                      &
     &             ,STEPBL,STEPRA,STEPCU                                &
     &             ,W0AVG, RAINNC, RAINC, RAINCV, RAINNCV               &
     &             ,NCA,GRID%SWRAD_SCAT                                 &
     &             ,CLDEFI,LOWLYR                                       &
     &             ,MASS_FLUX                                           &
     &             ,RTHFTEN, RQVFTEN                                    &
     &             ,CLDFRA_TRANS,CLDFRA_OLD,GLW,GSW,EMISS,EMTEMP,LU_INDEX&
     &             ,GRID%LANDUSE_ISICE, GRID%LANDUSE_LUCATS             &
     &             ,GRID%LANDUSE_LUSEAS, GRID%LANDUSE_ISN               &
     &             ,GRID%LU_STATE                                       &
     &             ,XLAT,XLONG,ALBEDO,ALBBCK                            &
     &             ,GRID%GMT,GRID%JULYR,GRID%JULDAY                     &
     &             ,GRID%LEVSIZ, NUM_OZMIXM, NUM_AEROSOLC, GRID%PAERLEV &
     &             ,TMN,XLAND,ZNT,Z0,USTAR,MOL,PBLH,TKE_MYJ             &
     &             ,EXCH_H,THC,SNOWC,MAVAIL,HFX,QFX,RAINBL              &
     &             ,STC,ZS,DZS,GRID%NUM_SOIL_LAYERS,WARM_RAIN           &
     &             ,ADV_MOIST_COND                                      &
     &             ,APR_GR,APR_W,APR_MC,APR_ST,APR_AS                   &
     &             ,APR_CAPMA,APR_CAPME,APR_CAPMI                       &
     &             ,XICE,XICE,VEGFRA,SNOW,CANWAT,SMSTAV                 &
     &             ,SMSTOT, SFCRUNOFF,UDRUNOFF,GRDFLX,ACSNOW            &
     &             ,ACSNOM,IVGTYP,ISLTYP,SFCEVP,SMC                     &
     &             ,SH2O, SNOWH, SMFR3D                                 &  
     &             ,grid%SNOALB                                         &
     &             ,GRID%DX,GRID%DY,F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY    &
     &             ,MP_RESTART_STATE,TBPVS_STATE,TBPVS0_STATE           &
     &             ,.TRUE.,moved,START_OF_SIMULATION                    &
     &             ,1                                                   & 
     &             ,IDS, IDE, JDS, JDE, KDS, KDE                        &
     &             ,IMS, IME, JMS, JME, KMS, KME                        &
     &             ,ITS, ITE, JTS, JTE, KTS, KTE                        &
     &             ,NUM_URBAN_LAYERS                                    &
     &                )




      CALL nl_get_cen_lat(GRID%ID, CEN_LAT)    
      CALL nl_get_cen_lon(GRID%ID, CEN_LON)    
      DTPHS=grid%dt*grid%nphs
      CALL GWD_init(DTPHS,GRID%DX,GRID%DY,CEN_LAT,CEN_LON,RESTRT        &
     &              ,GLAT,GLON,CROT,SROT,HANGL                          &
     &              ,IDS,IDE,JDS,JDE,KDS,KDE                            &
     &              ,IMS,IME,JMS,JME,KMS,KME                            &
     &              ,ITS,ITE,JTS,JTE,KTS,KTE )
      IF(.NOT.RESTRT)THEN
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          UGWDsfc(I,J)=0.
          VGWDsfc(I,J)=0.
        ENDDO
        ENDDO
      ENDIF



      IF(NSTART==0)THEN

        DO J=JMS,JME
        DO I=IMS,IME
          Z0(I,J)=Z0BASE(I,J)
        ENDDO
        ENDDO

        DO K=KMS,KME
        DO J=JMS,JME
        DO I=IMS,IME
          CLDFRA(I,J,K)=CLDFRA_TRANS(I,K,J)
        ENDDO
        ENDDO
        ENDDO

      ENDIF





      IF (.NOT. RESTRT) THEN




        CALL wrf_message('Initializng moist(:,:,:, Qv) from Q')
        DO K=KPS,KPE
        DO J=JFS,JFE
        DO I=IFS,IFE
           moist(I,J,K,P_QV) = Q(I,J,K) / (1.-Q(I,J,K))                 
        enddo      
        enddo      
        enddo      
     



     
        IF (.not. (MAXVAL(CWM).gt.0. .and. MAXVAL(CWM).lt.1.) ) then    
          do i_m = 2, num_moist
          if (i_m.ne.p_qv) &
     &       CALL wrf_message(' summing moist(:,:,:,i_m) into CWM array')
          DO K=KPS,KPE
          DO J=JFS,JFE
          DO I=IFS,IFE
            IF ( (moist(I,J,K,i_m).gt.EPSQ) .and. (i_m.ne.p_qv) ) THEN  
               CWM(I,J,K) = CWM(I,J,K) + moist(I,J,K,i_m)               
            ENDIF  
          enddo    
          enddo
          enddo
          enddo

          IF (.not. ( (maxval(F_ICE)+maxval(F_RAIN)) .gt. EPSQ) ) THEN
            CALL wrf_message(' computing F_ICE')
            do i_m = 2, num_moist
            DO J=JFS,JFE
            DO K=KPS,KPE
            DO I=IFS,IFE
              IF ( (moist(I,J,K,i_m).gt.EPSQ) .and. &
     &               ( (i_m.eq.p_qi).or.(i_m.eq.p_qs).or.(i_m.eq.p_qg) ) ) THEN
                 F_ICE(I,K,J) = F_ICE(I,K,J) + moist(I,J,K,i_m)
              ENDIF
              if (model_config_rec%mp_physics(grid%id).EQ.ETAMPNEW) then
                if ((i_m.eq.p_qi).or.(i_m.eq.p_qg) ) then
                  moist(I,J,K,p_qs)=moist(I,J,K,p_qs)+moist(I,J,K,i_m)
                  moist(I,J,K,i_m) =0.
                endif
              endif
            enddo
            enddo
            enddo
            enddo
            CALL wrf_message(' computing F_RAIN')

            DO J=JFS,JFE
            DO K=KPS,KPE
            DO I=IFS,IFE
              IF(F_ICE(i,k,j)<=EPSQ)THEN
                F_ICE(I,K,J)=0.
              ELSE
                F_ICE(I,K,J) = F_ICE(I,K,J)/CWM(I,J,K)
              ENDIF
              IF ( (moist(I,J,K,p_qr)+moist(I,J,K,p_qc)).gt.EPSQ) THEN
                IF(moist(i,j,k,p_qr)<=EPSQ)THEN
                  F_RAIN(I,K,J)=0.
                ELSE
                  F_RAIN(I,K,J) = moist(i,j,k,p_qr) &
     &                    / (moist(i,j,k,p_qr)+moist(i,j,k,p_qc))
                ENDIF
              ENDIF
            enddo
            enddo
            enddo
          ENDIF
        ENDIF


        IF (maxval(F_ICE) .gt. 0.) THEN
         do J=JMS,JME
         do K=KMS,KME
         do I=IMS,IME
          F_ICE_PHY(I,K,J)=F_ICE(I,K,J)
         enddo
         enddo
         enddo
        ENDIF

        IF (maxval(F_RAIN) .gt. 0.) THEN
         do J=JMS,JME
         do K=KMS,KME
         do I=IMS,IME
          F_RAIN_PHY(I,K,J)=F_RAIN(I,K,J)
         enddo
         enddo
         enddo
        ENDIF

        IF (maxval(F_RIMEF) .gt. 0.) THEN
          do J=JMS,JME
          do K=KMS,KME
          do I=IMS,IME
            F_RIMEF_PHY(I,K,J)=F_RIMEF(I,K,J)
          enddo
          enddo
          enddo
        ENDIF
      ENDIF

      IF (.NOT. RESTRT) THEN
  
        IF(MAXVAL(ALBEDO_DUM)>0.)THEN
          DO J=JMS,JME
          DO I=IMS,IME
            ALBEDO(I,J)=ALBEDO_DUM(I,J)
          ENDDO
          ENDDO
        ENDIF
      ENDIF

      IF(.NOT.RESTRT)THEN
        DO J=JMS,JME
        DO I=IMS,IME
          APREC(I,J)=RAINNC(I,J)*1.E-3
          CUPREC(I,J)=RAINCV(I,J)*1.E-3
        ENDDO
        ENDDO
      ENDIF


      DEALLOCATE(SFULL)
      DEALLOCATE(SMID)
      DEALLOCATE(DZS)
      DEALLOCATE(EMISS)
      DEALLOCATE(EMTEMP)
      DEALLOCATE(GLW)
      DEALLOCATE(HFX)
      DEALLOCATE(LOWLYR)

      DEALLOCATE(NCA)
      DEALLOCATE(QFX)
      DEALLOCATE(RAINBL)
      DEALLOCATE(RAINC)
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
      DEALLOCATE(ZINT)
      DEALLOCATE(CONVFAC)
      DEALLOCATE(RRI)
      DEALLOCATE(SNOWC)
      DEALLOCATE(THC)
      DEALLOCATE(TMN)
      DEALLOCATE(TSFC)
      DEALLOCATE(ZS)
      DEALLOCATE(PINT_TRANS)
      DEALLOCATE(T_TRANS)
      DEALLOCATE(CLDFRA_TRANS)
      DEALLOCATE(CLDFRA_OLD)


        DO J=jfs,jfe
        DO I=ifs,ife
          DWDTMN(I,J)=DWDTMN(I,J)*HBM3(I,J)
          DWDTMX(I,J)=DWDTMX(I,J)*HBM3(I,J)
        ENDDO
        ENDDO


!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_1.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_1_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_2.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_2_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_3.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_3_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_4.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_4_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_5.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_5_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_6.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_6_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_7.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_7_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_8.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_8_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_9.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_9_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_10.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_10_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_11.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_11_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_12.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_12_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_13.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_13_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_14.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_14_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_15.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_15_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_15B.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_15B_sub ( grid, &
  num_moist, &
  moist, &
  num_scalar, &
  scalar, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_16.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_16_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_17.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_17_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_18.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_18_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_19.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_19_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_20.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_20_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_21.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_21_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_22.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_22_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_23.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_23_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_24.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_24_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_25.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_25_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_26.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_26_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_27.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_27_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_28.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_28_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_29.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_29_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_30.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_30_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_31.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_31_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_32.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_32_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_33.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_33_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_34.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_34_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_35.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_35_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_36.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_36_sub ( grid, &
  config_flags, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_37.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_37_sub ( grid, &
  config_flags, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_38.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_38_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_INIT_39.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_INIT_39_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/scalar_derefs.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
! BEGIN SCALAR DEREFS
 grid%moved    = moved
 grid%ntsd    = ntsd
 grid%nstart_hour    = nstart_hour
 grid%hcoeff_tot    = hcoeff_tot
 grid%dy_nmm    = dy_nmm
 grid%cpgfv    = cpgfv
 grid%en    = en
 grid%ent    = ent
 grid%f4d    = f4d
 grid%f4q    = f4q
 grid%ef4t    = ef4t
 grid%upstrm    = upstrm
 grid%dlmd    = dlmd
 grid%dphd    = dphd
 grid%pdtop    = pdtop
 grid%pt    = pt
 grid%micro_start    = micro_start
 grid%hydro    = hydro
 grid%nphs0    = nphs0
 grid%nprec    = nprec
 grid%nclod    = nclod
 grid%nheat    = nheat
 grid%nrdlw    = nrdlw
 grid%nrdsw    = nrdsw
 grid%nsrfc    = nsrfc
 grid%avrain    = avrain
 grid%avcnvc    = avcnvc
 grid%aratim    = aratim
 grid%acutim    = acutim
 grid%ardlw    = ardlw
 grid%ardsw    = ardsw
 grid%asrfc    = asrfc
 grid%aphtim    = aphtim
 grid%imicrogram    = imicrogram
 grid%dtbc    = dtbc
 grid%landuse_isice    = landuse_isice
 grid%landuse_lucats    = landuse_lucats
 grid%landuse_luseas    = landuse_luseas
 grid%landuse_isn    = landuse_isn
 grid%number_at_same_level    = number_at_same_level
 grid%itimestep    = itimestep
 grid%xtime    = xtime
 grid%julian    = julian
 grid%lbc_fid    = lbc_fid
 grid%tiled    = tiled
 grid%patched    = patched
 grid%oid    = oid
 grid%auxhist1_oid    = auxhist1_oid
 grid%auxhist2_oid    = auxhist2_oid
 grid%auxhist3_oid    = auxhist3_oid
 grid%auxhist4_oid    = auxhist4_oid
 grid%auxhist5_oid    = auxhist5_oid
 grid%auxhist6_oid    = auxhist6_oid
 grid%auxhist7_oid    = auxhist7_oid
 grid%auxhist8_oid    = auxhist8_oid
 grid%auxhist9_oid    = auxhist9_oid
 grid%auxhist10_oid    = auxhist10_oid
 grid%auxhist11_oid    = auxhist11_oid
 grid%auxinput1_oid    = auxinput1_oid
 grid%auxinput2_oid    = auxinput2_oid
 grid%auxinput3_oid    = auxinput3_oid
 grid%auxinput4_oid    = auxinput4_oid
 grid%auxinput5_oid    = auxinput5_oid
 grid%auxinput6_oid    = auxinput6_oid
 grid%auxinput7_oid    = auxinput7_oid
 grid%auxinput8_oid    = auxinput8_oid
 grid%auxinput9_oid    = auxinput9_oid
 grid%auxinput10_oid    = auxinput10_oid
 grid%auxinput11_oid    = auxinput11_oid
 grid%nodyn_dummy    = nodyn_dummy
! END SCALAR DEREFS
!ENDOFREGISTRYGENERATEDINCLUDE

   RETURN


END SUBROUTINE START_DOMAIN_NMM

