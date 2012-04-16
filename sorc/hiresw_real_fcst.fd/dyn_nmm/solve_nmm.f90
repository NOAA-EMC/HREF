



















      SUBROUTINE SOLVE_NMM(GRID,CONFIG_FLAGS                            &







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


     &           )

      USE MODULE_DOMAIN,                ONLY : DOMAIN, GET_IJK_FROM_GRID
      USE MODULE_CONFIGURE,             ONLY : GRID_CONFIG_REC_TYPE
      USE MODULE_MODEL_CONSTANTS
      USE MODULE_STATE_DESCRIPTION
      USE MODULE_CTLBLK



      USE MODULE_IGWAVE_ADJUST,         ONLY: PDTE,PFDHT,DDAMP,VTOA
      USE MODULE_ADVECTION,             ONLY: ADVE,VAD2,HAD2,VAD2_SCAL,HAD2_SCAL &

                                             ,adv2,mono 


      USE MODULE_NONHY_DYNAM,           ONLY: EPS,VADZ,HADZ
      USE MODULE_DIFFUSION_NMM,         ONLY: HDIFF
      USE MODULE_BNDRY_COND,            ONLY: BOCOH,BOCOV
      USE MODULE_PHYSICS_CALLS
      USE MODULE_EXT_INTERNAL
      USE MODULE_PRECIP_ADJUST
      USE MODULE_NEST_UTIL     


      IMPLICIT NONE







      TYPE(DOMAIN),TARGET :: GRID

















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





      TYPE(GRID_CONFIG_REC_TYPE),INTENT(IN) :: CONFIG_FLAGS








      LOGICAL :: euler
      INTEGER :: idtadt
      INTEGER :: idtadc
integer,parameter:: &
 kss=1 &                     
,kse=4                       

integer:: &
  ks                         



      INTEGER :: IDS,IDE,JDS,JDE,KDS,KDE                                &
     &          ,IMS,IME,JMS,JME,KMS,KME                                & 
     &          ,IPS,IPE,JPS,JPE,KPS,KPE                                &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE

      INTEGER :: I,ICLTEND,IDF,IRTN,J,JC,JDF,K,KDF,LB,N_MOIST &
     &          ,NTSD_current,L
      integer :: ierr
      INTEGER,SAVE :: NTSD_restart

      INTEGER :: MYPROC
      INTEGER :: KVH,NTSD_rad,RC
      INTEGER :: NUM_OZMIXM,NUM_AEROSOLC

      REAL :: DT_INV,FICE,FRAIN,GPS,QI,QR,QW,WC,SFENTH

      LOGICAL :: LAST_TIME,OPERATIONAL_PHYSICS, micro_has_been_read

      CHARACTER(80) :: MESSAGE


      INTEGER :: ISTAT
      REAL,ALLOCATABLE,SAVE,DIMENSION(:,:,:) :: PPTDAT





      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: TTEN,QTEN
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: RTHRATEN,RTHBLTEN,RQVBLTEN

      REAL,ALLOCATABLE,DIMENSION(:,:)      ::  upindx



      LOGICAL wrf_dm_on_monitor
      EXTERNAL wrf_dm_on_monitor




      real,save :: solve_tim,exch_tim,pdte_tim,adve_tim,vtoa_tim        &
     &,            vadz_tim,hadz_tim,eps_tim,vad2_tim,had2_tim          &
     &,            radiation_tim,rdtemp_tim,turbl_tim,cltend_tim        &
     &,            cucnvc_tim,gsmdrive_tim,hdiff_tim,bocoh_tim          &
     &,            pfdht_tim,ddamp_tim,bocov_tim,uv_htov_tim,sum_tim    &
     &,            adjppt_tim
      real,save :: exch_tim_max
      real :: btim,btimx
      real :: et_max,this_tim,magw,magw2,pup,plow
      real :: P1D, T1D, Q1D, C1D, FI1D, FR1D, FS1D, CUREFL, DBZ1
      real :: P1Da, T1Da, Q1Da, C1Da, FI1Da, FR1Da, FS1Da
      real :: P1Db, T1Db, Q1Db, C1Db, FI1Db, FR1Db, FS1Db
      real :: wgta, wgtb, ZMID, ZMIDP1
      integer :: n_print_time


      REAL, PARAMETER :: DMRmin=.05e-3, DMRmax=1.e-3,           &
     &      DelDMR=1.e-6,XMRmin=1.e6*DMRmin, XMRmax=1.e6*DMRmax
      INTEGER, PARAMETER :: MDRmin=XMRmin, MDRmax=XMRmax
      REAL, save, DIMENSION(MDRmin:MDRmax)::    MASSR






      REAL, PARAMETER :: DMImin=.05e-3, DMImax=1.e-3,           &
     &      DelDMI=1.e-6,XMImin=1.e6*DMImin



      REAL,save::  C_N0r0,           &
     &  CN0r0, CN0r_DMRmin, CN0r_DMRmax,      &
     &  RQR_DRmin, RQR_DRmax




      real*8 :: timef




real,save:: &
 sumdrrw




real,allocatable,save,dimension(:,:,:,:):: & 
 szj &                       
,s1z &                       
,spz &                       
,tcs                         























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


































      CALL DOMAIN_CLOCK_GET(GRID,ADVANCEcOUNT=NTSD_current)



      IF(NTSD_current==0)THEN

        micro_has_been_read=.false.
        IF(GRID%RESTART.AND.GRID%TSTART>0.)THEN
          IHRST=NSTART_HOUR
          NTSD_restart=NTSD
        ELSEIF(GRID%TSTART>0 .and. model_config_rec%dfi_opt > 0) THEN
          IHRST=NSTART_HOUR
          NTSD_restart=NTSD
	write(0,*) 'GRID%TSTART in check: ', GRID%TSTART
	write(0,*) 'setting IHRST, NTSD_restart because of DFI', IHRST, NTSD_restart 
        ELSEIF(GRID%TSTART<0 .and. model_config_rec%dfi_opt > 0) THEN
          IHRST=NSTART_HOUR
          NTSD_restart=NTSD
	write(0,*) 'GRID%TSTART in check: ', GRID%TSTART
	write(0,*) 'setting IHRST, NTSD_restart because of DFI', IHRST, NTSD_restart 

        ELSE
          IHRST=GRID%GMT
          NSTART_HOUR=IHRST
          NTSD_restart=0
        ENDIF

      ELSE

        micro_has_been_read=.true.

      ENDIF



      NTSD=NTSD_restart+NTSD_current
      LAST_TIME=domain_last_time_step(GRID)




        WRITE(MESSAGE,125)NTSD,NTSD*GRID%DT/3600.
  125   FORMAT(' SOLVE_NMM: TIMESTEP IS ',I5,'   TIME IS ',F7.3,' HOURS')
        CALL WRF_MESSAGE(TRIM(MESSAGE))




      euler=model_config_rec%euler_adv
      idtadt=model_config_rec%idtadt
      idtadc=model_config_rec%idtadc



      CALL WRF_GET_DM_COMMUNICATOR(MPI_COMM_COMP)
      CALL WRF_GET_NPROC(NPES)
      CALL WRF_GET_MYPROC(MYPROC)
      MYPE=MYPROC




      CALL GET_IJK_FROM_GRID(GRID                                       &
     &                      ,IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                      ,IMS,IME,JMS,JME,KMS,KME                    &
     &                      ,IPS,IPE,JPS,JPE,KPS,KPE )






      CALL SET_TILES(GRID,IDS,IDE,JDS,JDE,IPS,IPE,JPS,JPE)








      OPERATIONAL_PHYSICS=.FALSE.

      IF(CONFIG_FLAGS%RA_SW_PHYSICS    ==GFDLSWSCHEME.AND.              &
     &   CONFIG_FLAGS%RA_LW_PHYSICS    ==GFDLLWSCHEME.AND.              &
     &   CONFIG_FLAGS%SF_SFCLAY_PHYSICS==MYJSFCSCHEME.AND.              &
     &   CONFIG_FLAGS%BL_PBL_PHYSICS   ==MYJPBLSCHEME.AND.              &
     &   CONFIG_FLAGS%CU_PHYSICS       ==BMJSCHEME.AND.                 &
     &   CONFIG_FLAGS%MP_PHYSICS       ==ETAMPNEW)THEN

        OPERATIONAL_PHYSICS=.TRUE.

      ENDIF





      ALLOCATE(TTEN(IMS:IME,KMS:KME,JMS:JME),STAT=ISTAT)
      ALLOCATE(QTEN(IMS:IME,KMS:KME,JMS:JME),STAT=ISTAT)
      ALLOCATE(RTHBLTEN(IMS:IME,KMS:KME,JMS:JME),STAT=ISTAT)
      ALLOCATE(RQVBLTEN(IMS:IME,KMS:KME,JMS:JME),STAT=ISTAT)
      ALLOCATE(RTHRATEN(IMS:IME,KMS:KME,JMS:JME),STAT=ISTAT)

      IF(CONFIG_FLAGS%CU_PHYSICS==GDSCHEME)THEN
        DO J=JMS,JME
        DO K=KMS,KME
        DO I=IMS,IME
          TTEN(I,K,J)=T(I,J,K)
          QTEN(I,K,J)=Q(I,J,K)
        ENDDO
        ENDDO
        ENDDO
      ENDIF

      GRID%SIGMA=1 
      HYDRO=.FALSE.


      IDF=IDE-1
      JDF=JDE-1
      KDF=KDE-1






      ITS=IPS
      ITE=MIN(IPE,IDF)
      JTS=JPS
      JTE=MIN(JPE,JDF)
      KTS=KPS
      KTE=MIN(KPE,KDF)
      if(ntsd==0)then
        write(message,*)' its=',its,' ite=',ite
        call wrf_message(trim(message))
        write(message,*)' jts=',jts,' jte=',jte
        call wrf_message(trim(message))
        write(message,*)' kts=',kts,' kte=',kte
        call wrf_message(trim(message))

if(euler) then
  
  allocate (szj(ims:ime,jms:jme,kms:kme,kss:kse),stat=istat)
  allocate (s1z(ims:ime,jms:jme,kms:kme,kss:kse),stat=istat)
  allocate (spz(ims:ime,jms:jme,kms:kme,kss:kse),stat=istat)
  allocate (tcs(ims:ime,jms:jme,kms:kme,kss:kse),stat=istat)
endif


      endif





      if(ntsd==0)then
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
        uv_htov_tim=0.
        exch_tim_max=0.
        adjppt_tim=0.
      endif

      N_MOIST=NUM_MOIST

      DO J=max(jds+(0),jts-(4)),min(jde-(0),jte+(4))
        IHEG(J)=MOD(J+1,2)
        IHWG(J)=IHEG(J)-1
        IVEG(J)=MOD(J,2)
        IVWG(J)=IVEG(J)-1
      ENDDO

      DO J=max(jds+(0),jts-(4)),min(jde-(0),jte+(4))
        IVW(J)=IVWG(J)
        IVE(J)=IVEG(J)
        IHE(J)=IHEG(J)
        IHW(J)=IHWG(J)
      ENDDO



      LB=2*(IDF-IDS+1)+(JDF-JDS+1)-3



      JC=JMS+(JME-JMS)/2
      GPS=SQRT(DX_NMM(IMS,JC)**2+DY_NMM**2)



      TSPH=3600./GRID%DT

      n_print_time=nint(3600./grid%dt)   


      NBOCO=0








      IF(GRID%PCPFLG.AND..NOT.ALLOCATED(PPTDAT))THEN
        ALLOCATE(PPTDAT(IMS:IME,JMS:JME,3),STAT=ISTAT)
      ENDIF












      IF (NTSD==0) THEN
        IF (GRID%PCPFLG) THEN
          CALL READPCP(PPTDAT,DDATA,LSPA                                &
     &      ,IDS,IDE,JDS,JDE,KDS,KDE                                    &
     &      ,IMS,IME,JMS,JME,KMS,KME                                    &
     &      ,ITS,ITE,JTS,JTE,KTS,KTE)
        ENDIF
      ENDIF


      btim=timef()





      CALL BUCKETS(NTSD,NPREC,NSRFC,NRDSW,NRDLW                         &
     &            ,GRID%RESTART,GRID%TSTART                             &
     &            ,NCLOD,NHEAT,GRID%NPHS,TSPH                           &
     &            ,ACPREC,CUPREC,ACSNOW,ACSNOM,SSROFF,BGROFF            &
     &            ,SFCEVP,POTEVP,SFCSHX,SFCLHX,SUBSHX,SNOPCX            &
     &            ,SFCUVX,POTFLX                                        &
     &            ,ARDSW,ASWIN,ASWOUT,ASWTOA                            &
     &            ,ARDLW,ALWIN,ALWOUT,ALWTOA                            &
     &            ,ACFRST,NCFRST,ACFRCV,NCFRCV                          &
     &            ,AVCNVC,AVRAIN,TCUCN,TRAIN                            &
     &            ,ASRFC                                                &
     &            ,T,TLMAX,TLMIN,TSHLTR,PSHLTR,QSHLTR                   &
     &            ,T02_MAX,T02_MIN,RH02_MAX,RH02_MIN                    &
     &            ,IDS,IDE,JDS,JDE,KDS,KDE                              &
     &            ,IMS,IME,JMS,JME,KMS,KME                              &
     &            ,ITS,ITE,JTS,JTE,KTS,KTE)


      IF(NTSD==0)THEN
        FIRST=.TRUE.

        btimx=timef()


      if(euler) then
        sumdrrw=0.

        DO K=KTS,KTE
          DO J=JMS,JME
            DO I=IMS,IME
              rrw(i,j,k)=0. 
              if(i.ge.ide/2-6.and.i.le.ide/2+6.and. &
                 j.ge.jde/2-6.and.j.le.jde/2+6 .and. K .eq. KTE/2) then
                rrw(i,j,k)=10.0 

              endif
            enddo
          enddo
        enddo
        do ks=kss,kse
          DO K=KMS,KME
            DO J=JMS,JME
              DO I=IMS,IME
                szj(i,j,k,ks)=0.
                s1z(i,j,k,ks)=0.
                spz(i,j,k,ks)=0.
                tcs(i,j,k,ks)=0.
              enddo
            enddo
          enddo
        enddo
      endif











CALL HALO_NMM_A_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )



      IF(CONFIG_FLAGS%MP_PHYSICS/=ETAMPNEW)THEN






CALL HALO_NMM_A_3_sub ( grid, &
  num_moist, &
  moist, &
  num_scalar, &
  scalar, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

      ENDIF



      if(euler) then
        DO K=KTS,KTE
          DO J=JMS,JME
            DO I=IMS,IME
              spz(i,j,k,1)=sqrt(max(q  (i,j,k),epsq))
              spz(i,j,k,2)=sqrt(max(cwm(i,j,k),epsq))
              spz(i,j,k,4)=sqrt(max(rrw(i,j,k),0.  ))
            enddo
          enddo
        enddo
        DO J=JMS,JME
          DO I=IMS,IME
            spz(i,j,kte,3)=sqrt(max((q2(i,j,kte)+epsq2)*0.5,epsq2))
          enddo
        enddo
        DO K=KTE-1,KTS,-1
          DO J=JMS,JME
            DO I=IMS,IME
              spz(i,j,k,3)=sqrt(max((q2(i,j,k)+q2(i,j,k+1))*0.5,epsq2))
            enddo
          enddo
        enddo
      endif










        exch_tim=exch_tim+timef()-btimx






        GO TO 2003
      ENDIF



 2000 CONTINUE







      btimx=timef()







CALL HALO_NMM_D_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


      exch_tim=exch_tim+timef()-btimx





      btimx=timef()


      CALL PDTE(                                                        &
     &            GRID,MYPE,MPI_COMM_COMP,                              &
     &            NTSD,GRID%DT,PT,ETA2,RES,HYDRO,HBM2                   &
     &           ,PD,PDSL,PDSLO                                         &
     &           ,PETDT,DIV,PSDT                                        &
     &           ,IHE,IHW,IVE,IVW                                       &
     &           ,IDS,IDF,JDS,JDF,KDS,KDE                               &
     &           ,IMS,IME,JMS,JME,KMS,KME                               &
     &           ,ITS,ITE,JTS,JTE,KTS,KTE)


      pdte_tim=pdte_tim+timef()-btimx






      btimx=timef()







CALL HALO_NMM_F_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL HALO_NMM_F1_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


      exch_tim=exch_tim+timef()-btimx




      btimx=timef()

      CALL ADVE(NTSD,GRID%DT,DETA1,DETA2,PDTOP                          &
     &         ,CURV,F,FAD,F4D,EM_LOC,EMT_LOC,EN,ENT,DX_NMM,DY_NMM      &
     &         ,HBM2,VBM2                                               &
     &         ,T,U,V,PDSLO,TOLD,UOLD,VOLD                              &
     &         ,PETDT,UPSTRM                                            &
     &         ,FEW,FNS,FNE,FSE                                         &
     &         ,ADT,ADU,ADV                                             & 
     &         ,N_IUP_H,N_IUP_V                                         &
     &         ,N_IUP_ADH,N_IUP_ADV                                     &
     &         ,IUP_H,IUP_V,IUP_ADH,IUP_ADV                             &
     &         ,IHE,IHW,IVE,IVW                                         &
     &         ,IDS,IDF,JDS,JDF,KDS,KDE                                 &
     &         ,IMS,IME,JMS,JME,KMS,KME                                 &
     &         ,ITS,ITE,JTS,JTE,KTS,KTE)

      adve_tim=adve_tim+timef()-btimx



      if(euler) then 

        if(config_flags%mp_physics.ne.etampnew .and. config_flags%dfi_opt .eq. 0) then
          write(0,*) 'Stopping.  Only etampnew implemented w. euler'
          stop
        endif


        if(mod(ntsd,idtadt).eq.0) then
          btimx=timef()






CALL HALO_NMM_I_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

          exch_tim=exch_tim+timef()-btimx

          btimx=timef()

          DO K=KTS,KTE
            DO J=JMS,JME
              DO I=IMS,IME
                szj(i,j,k,1)=max(q  (i,j,k),epsq)
                szj(i,j,k,2)=max(cwm(i,j,k),epsq)
                szj(i,j,k,4)=max(rrw(i,j,k),0.  )
              enddo
            enddo
          enddo
          DO J=JMS,JME
            DO I=IMS,IME
              szj(i,j,kte,3)=max((q2 (i,j,kte)+epsq2)*0.5,epsq2)
            enddo
          enddo
          DO K=KTE-1,KTS,-1
            DO J=JMS,JME
              DO I=IMS,IME
                szj(i,j,k,3)=max((q2 (i,j,k)+q2 (i,j,k+1))*0.5,epsq2)
              enddo
            enddo
          enddo

          call adv2 &
          (UPSTRM &
          ,mype,kss,kse &
          ,ids,ide,jds,jde,kds,kde &
          ,ims,ime,jms,jme,kms,kme &
          ,its,ite,jts,jte,kts,kte &
          ,N_IUP_H &
          ,N_IUP_ADH &
          ,IUP_H,IUP_ADH &
          ,ENT &
          ,idtadt &
          ,grid%dt,pdtop &
          ,ihe,ihw,ive,ivw &
          ,deta1,deta2 &
          ,EMT_LOC &
          ,fad,hbm2,pdsl,pdslo &
          ,petdt &
          ,UOLD,VOLD &
          ,szj,spz &
          
          ,fne,fse,few,fns,s1z,tcs)

          call mono &
          ( &
           GRID%DOMDESC, &
           mype,ntsd,ntsd*grid%dt/3600.,kss,kse &
          ,ids,ide,jds,jde,kds,kde &
          ,ims,ime,jms,jme,kms,kme &
          ,its,ite,jts,jte,kts,kte &
          ,idtadt &
          ,dy_nmm,pdtop &
          ,sumdrrw &
          ,ihe,ihw &
          ,deta1,deta2 &
          ,dx_nmm,hbm2,pdsl &
          ,szj &

          ,s1z &
          ,tcs )

          do ks=kss,kse 
            DO K=KTS,KTE
              DO J=max(jds+(2),jts-(0)),min(jde-(2),jte+(0))
                DO I=max(ids+(1),its-(0)),min(ide-(1),ite+(0))
                  szj(i,j,k,ks)=szj(i,j,k,ks)+tcs(i,j,k,ks)
                enddo
              enddo
            enddo
          enddo 

          DO K=KTS,KTE
            DO J=max(jds+(2),jts-(0)),min(jde-(2),jte+(0))
              DO I=max(ids+(1),its-(0)),min(ide-(1),ite+(0))
                q  (i,j,k)=szj(i,j,k,1)
                cwm(i,j,k)=szj(i,j,k,2)
                rrw(i,j,k)=szj(i,j,k,4)



              enddo
            enddo
          enddo

          DO J=max(jds+(2),jts-(0)),min(jde-(2),jte+(0))
            DO I=max(ids+(1),its-(0)),min(ide-(1),ite+(0))
              q2(i,j,kte)=max(szj(i,j,kte,3)+szj(i,j,kte,3)-epsq2 &
                             ,epsq2)
            enddo
          enddo

          DO K=KTE-1,KTS+1,-1
            DO J=max(jds+(2),jts-(0)),min(jde-(2),jte+(0))
              DO I=max(ids+(1),its-(0)),min(ide-(1),ite+(0))
                if(k>kts)then
                  q2(i,j,k)=max(szj(i,j,k,3)+szj(i,j,k,3)-q2(i,j,k+1) &
                               ,epsq2)
                else
                  q2(i,j,k)=q2(i,j,k+1)
                endif
              enddo
            enddo
          enddo










        IF(.NOT.OPERATIONAL_PHYSICS)THEN
          DO K=KTS,KTE
            DO J=max(jds+(0),jts-(0)),min(jde-(0),jte+(0))
              DO I=max(ids+(0),its-(0)),min(ide-(0),ite+(0))
                MOIST(I,J,K,P_QV)=Q(I,J,K)/(1.-Q(I,J,K))
                WC = CWM(I,J,K)
                QI = 0.
                QR = 0.
                QW = 0.
                FICE=F_ICE(I,K,J)
                FRAIN=F_RAIN(I,K,J)

                IF(FICE>=1.)THEN
                  QI=WC
                ELSEIF(FICE<=0.)THEN
                  QW=WC
                ELSE
                  QI=FICE*WC
                  QW=WC-QI
                ENDIF

                IF(QW>0..AND.FRAIN>0.)THEN
                  IF(FRAIN>=1.)THEN
                    QR=QW
                    QW=0.
                  ELSE
                    QR=FRAIN*QW
                    QW=QW-QR
                  ENDIF
                ENDIF

                MOIST(I,J,K,P_QC)=QW
                MOIST(I,J,K,P_QR)=QR
                MOIST(I,J,K,P_QI)=0.
                MOIST(I,J,K,P_QS)=QI
                MOIST(I,J,K,P_QG)=0.
              ENDDO
            ENDDO
          ENDDO
        ENDIF

        had2_tim=had2_tim+timef()-btimx

        endif



      endif 







      btimx=timef()

      CALL VTOA(                                                        &
     &          GRID,                                                   &
     &          NTSD,GRID%DT,PT,ETA2                                    &
     &         ,HBM2,EF4T                                               &
     &         ,T,DWDT,RTOP,OMGALF                                      &
     &         ,PINT,DIV,PSDT,RES                                       &
     &         ,IHE,IHW,IVE,IVW                                         &
     &         ,IDS,IDF,JDS,JDF,KDS,KDE                                 &
     &         ,IMS,IME,JMS,JME,KMS,KME                                 &
     &         ,ITS,ITE,JTS,JTE,KTS,KTE)

      vtoa_tim=vtoa_tim+timef()-btimx





      btimx=timef()




      CALL VADZ(NTSD,GRID%DT,FIS,GRID%SIGMA,DFL,HBM2                    &
     &         ,DETA1,DETA2,PDTOP                                       &
     &         ,PINT,PDSL,PDSLO,PETDT                                   &
     &         ,RTOP,T,Q,CWM,Z,W,DWDT,PDWDT                             &
     &         ,IHE,IHW,IVE,IVW                                         &
     &         ,IDS,IDF,JDS,JDF,KDS,KDE                                 &
     &         ,IMS,IME,JMS,JME,KMS,KME                                 &
     &         ,ITS,ITE,JTS,JTE,KTS,KTE)

      vadz_tim=vadz_tim+timef()-btimx





      btimx=timef()







CALL HALO_NMM_G_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


      exch_tim=exch_tim+timef()-btimx





      btimx=timef()

      CALL HADZ(NTSD,GRID%DT,HYDRO,HBM2,DETA1,DETA2,PDTOP               &
     &         ,DX_NMM,DY_NMM,FAD                                       &
     &         ,FEW,FNS,FNE,FSE                                         &
     &         ,PDSL,U,V,W,Z                                            &
     &         ,IHE,IHW,IVE,IVW                                         &
     &         ,IDS,IDF,JDS,JDF,KDS,KDE                                 &
     &         ,IMS,IME,JMS,JME,KMS,KME                                 &
     &         ,ITS,ITE,JTS,JTE,KTS,KTE)

      hadz_tim=hadz_tim+timef()-btimx





      btimx=timef()







CALL HALO_NMM_H_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


      exch_tim=exch_tim+timef()-btimx





      btimx=timef()

      CALL EPS(NTSD,GRID%DT,HYDRO,DX_NMM,DY_NMM,FAD                     &
     &        ,DETA1,DETA2,PDTOP,PT                                     &
     &        ,HBM2,HBM3                                                &
     &        ,PDSL,PDSLO,PINT,RTOP,PETDT,PDWDT                         &
     &        ,DWDT,DWDTMN,DWDTMX                                       &
     &        ,FNS,FEW,FNE,FSE                                          &
     &        ,T,U,V,W,Q,CWM                                            &
     &        ,IHE,IHW,IVE,IVW                                          &
     &        ,IDS,IDF,JDS,JDF,KDS,KDE                                  &
     &        ,IMS,IME,JMS,JME,KMS,KME                                  &
     &        ,ITS,ITE,JTS,JTE,KTS,KTE)

      eps_tim=eps_tim+timef()-btimx


      if(.not.euler) then 





      IF(MOD(NTSD,GRID%IDTAD)==0)THEN
        btimx=timef()

        vad2_micro_check: IF(CONFIG_FLAGS%MP_PHYSICS==ETAMPNEW)THEN
          CALL VAD2(NTSD,GRID%DT,GRID%IDTAD,DX_NMM,DY_NMM               &
     &             ,AETA1,AETA2,DETA1,DETA2,PDSL,PDTOP,HBM2             &
     &             ,Q,Q2,CWM,PETDT                                      &
     &             ,N_IUP_H,N_IUP_V                                     &
     &             ,N_IUP_ADH,N_IUP_ADV                                 &
     &             ,IUP_H,IUP_V,IUP_ADH,IUP_ADV                         &
     &             ,IHE,IHW,IVE,IVW                                     &
     &             ,IDS,IDF,JDS,JDF,KDS,KDE                             &
     &             ,IMS,IME,JMS,JME,KMS,KME                             &
     &             ,ITS,ITE,JTS,JTE,KTS,KTE)

        ELSE vad2_micro_check
          CALL VAD2_SCAL(NTSD,GRID%DT,GRID%IDTAD,DX_NMM,DY_NMM          &
     &                  ,AETA1,AETA2,DETA1,DETA2,PDSL,PDTOP             &
     &                  ,HBM2                                           &
     &                  ,Q2,PETDT                                       &
     &                  ,N_IUP_H,N_IUP_V                                &
     &                  ,N_IUP_ADH,N_IUP_ADV                            &
     &                  ,IUP_H,IUP_V,IUP_ADH,IUP_ADV                    &
     &                  ,IHE,IHW,IVE,IVW                                &
     &                  ,1,1                                            &
     &                  ,IDS,IDF,JDS,JDF,KDS,KDE                        &
     &                  ,IMS,IME,JMS,JME,KMS,KME                        &
     &                  ,ITS,ITE,JTS,JTE,KTS,KTE)                              
     
          CALL VAD2_SCAL(NTSD,GRID%DT,GRID%IDTAD,DX_NMM,DY_NMM          &
     &                  ,AETA1,AETA2,DETA1,DETA2,PDSL,PDTOP             &
     &                  ,HBM2                                           &
     &                  ,MOIST,PETDT                                    &
     &                  ,N_IUP_H,N_IUP_V                                &
     &                  ,N_IUP_ADH,N_IUP_ADV                            &
     &                  ,IUP_H,IUP_V,IUP_ADH,IUP_ADV                    &
     &                  ,IHE,IHW,IVE,IVW                                &
     &                  ,NUM_MOIST,2                                    &
     &                  ,IDS,IDF,JDS,JDF,KDS,KDE                        &
     &                  ,IMS,IME,JMS,JME,KMS,KME                        &
     &                  ,ITS,ITE,JTS,JTE,KTS,KTE)

          CALL VAD2_SCAL(NTSD,GRID%DT,GRID%IDTAD,DX_NMM,DY_NMM          &
     &                  ,AETA1,AETA2,DETA1,DETA2,PDSL,PDTOP             &
     &                  ,HBM2                                           &
     &                  ,SCALAR,PETDT                                   &
     &                  ,N_IUP_H,N_IUP_V                                &
     &                  ,N_IUP_ADH,N_IUP_ADV                            &
     &                  ,IUP_H,IUP_V,IUP_ADH,IUP_ADV                    &
     &                  ,IHE,IHW,IVE,IVW                                &
     &                  ,NUM_SCALAR,2                                   &
     &                  ,IDS,IDF,JDS,JDF,KDS,KDE                        &
     &                  ,IMS,IME,JMS,JME,KMS,KME                        &
     &                  ,ITS,ITE,JTS,JTE,KTS,KTE)


          DO K=KTS,KTE
          DO J=max(jds+(0),jts-(0)),min(jde-(0),jte+(0))
          DO I=max(ids+(0),its-(0)),min(ide-(0),ite+(0))
            Q(I,J,K)=MOIST(I,J,K,P_QV)/(1.+MOIST(I,J,K,P_QV))
          ENDDO
          ENDDO   
          ENDDO   

        ENDIF vad2_micro_check

        vad2_tim=vad2_tim+timef()-btimx

      ENDIF










      IF(MOD(NTSD,GRID%IDTAD)==0)THEN
        btimx=timef()







CALL HALO_NMM_I_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


        IF(CONFIG_FLAGS%MP_PHYSICS/=ETAMPNEW)THEN






CALL HALO_NMM_I_3_sub ( grid, &
  num_moist, &
  moist, &
  num_scalar, &
  scalar, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

        ENDIF


        exch_tim=exch_tim+timef()-btimx





        btimx=timef()


        had2_micro_check: IF(CONFIG_FLAGS%MP_PHYSICS==ETAMPNEW)THEN


          CALL HAD2(                                                   &
     &              GRID%DOMDESC,                                      &
     &              NTSD,GRID%DT,GRID%IDTAD,DX_NMM,DY_NMM              &
     &             ,AETA1,AETA2,DETA1,DETA2,PDSL,PDTOP                 &
     &             ,HBM2,HBM3                                          &
     &             ,Q,Q2,CWM,U,V,Z,HYDRO                               &
     &             ,N_IUP_H,N_IUP_V                                    &
     &             ,N_IUP_ADH,N_IUP_ADV                                &
     &             ,IUP_H,IUP_V,IUP_ADH,IUP_ADV                        &
     &             ,IHE,IHW,IVE,IVW                                    &
     &             ,IDS,IDF,JDS,JDF,KDS,KDE                            &
     &             ,IMS,IME,JMS,JME,KMS,KME                            &
     &             ,ITS,ITE,JTS,JTE,KTS,KTE)








          IF(.NOT.OPERATIONAL_PHYSICS)THEN
            DO K=KTS,KTE
            DO J=max(jds+(0),jts-(0)),min(jde-(0),jte+(0))
            DO I=max(ids+(0),its-(0)),min(ide-(0),ite+(0))
              MOIST(I,J,K,P_QV)=Q(I,J,K)/(1.-Q(I,J,K))
              WC = CWM(I,J,K)
              QI = 0.
              QR = 0.
              QW = 0.
              FICE=F_ICE(I,K,J)
              FRAIN=F_RAIN(I,K,J)

              IF(FICE>=1.)THEN
                QI=WC
              ELSEIF(FICE<=0.)THEN
                QW=WC
              ELSE
                QI=FICE*WC
                QW=WC-QI
              ENDIF

              IF(QW>0..AND.FRAIN>0.)THEN
                IF(FRAIN>=1.)THEN
                  QR=QW
                  QW=0.
                ELSE
                  QR=FRAIN*QW
                  QW=QW-QR
                ENDIF
              ENDIF

              MOIST(I,J,K,P_QC)=QW
              MOIST(I,J,K,P_QR)=QR
              MOIST(I,J,K,P_QI)=0.
              MOIST(I,J,K,P_QS)=QI
              MOIST(I,J,K,P_QG)=0.
            ENDDO
            ENDDO
            ENDDO
          ENDIF


        ELSE had2_micro_check


          CALL HAD2_SCAL(                                              &
     &                   GRID%DOMDESC,                                 &
     &                   NTSD,GRID%DT,GRID%IDTAD,DX_NMM,DY_NMM         &
     &                  ,AETA1,AETA2,DETA1,DETA2,PDSL,PDTOP            &
     &                  ,HBM2,HBM3                                     &
     &                  ,Q2,U,V,Z,HYDRO                                &
     &                  ,N_IUP_H,N_IUP_V                               &
     &                  ,N_IUP_ADH,N_IUP_ADV                           &
     &                  ,IUP_H,IUP_V,IUP_ADH,IUP_ADV                   &
     &                  ,IHE,IHW,IVE,IVW                               &
     &                  ,1,1                                           &
     &                  ,IDS,IDF,JDS,JDF,KDS,KDE                       &
     &                  ,IMS,IME,JMS,JME,KMS,KME                       &
     &                  ,ITS,ITE,JTS,JTE,KTS,KTE)

          CALL HAD2_SCAL(                                              &
     &                   GRID%DOMDESC,                                 &
     &                   NTSD,GRID%DT,GRID%IDTAD,DX_NMM,DY_NMM         &
     &                  ,AETA1,AETA2,DETA1,DETA2,PDSL,PDTOP            &
     &                  ,HBM2,HBM3                                     &
     &                  ,MOIST,U,V,Z,HYDRO                             &
     &                  ,N_IUP_H,N_IUP_V                               &
     &                  ,N_IUP_ADH,N_IUP_ADV                           &
     &                  ,IUP_H,IUP_V,IUP_ADH,IUP_ADV                   &
     &                  ,IHE,IHW,IVE,IVW                               &
     &                  ,NUM_MOIST,2                                   &
     &                  ,IDS,IDF,JDS,JDF,KDS,KDE                       &
     &                  ,IMS,IME,JMS,JME,KMS,KME                       &
     &                  ,ITS,ITE,JTS,JTE,KTS,KTE)

          CALL HAD2_SCAL(                                              &
     &                   GRID%DOMDESC,                                 &
     &                   NTSD,GRID%DT,GRID%IDTAD,DX_NMM,DY_NMM         &
     &                  ,AETA1,AETA2,DETA1,DETA2,PDSL,PDTOP            &
     &                  ,HBM2,HBM3                                     &
     &                  ,SCALAR,U,V,Z,HYDRO                            &
     &                  ,N_IUP_H,N_IUP_V                               &
     &                  ,N_IUP_ADH,N_IUP_ADV                           &
     &                  ,IUP_H,IUP_V,IUP_ADH,IUP_ADV                   &
     &                  ,IHE,IHW,IVE,IVW                               &
     &                  ,NUM_SCALAR,2                                  &
     &                  ,IDS,IDF,JDS,JDF,KDS,KDE                       &
     &                  ,IMS,IME,JMS,JME,KMS,KME                       &
     &                  ,ITS,ITE,JTS,JTE,KTS,KTE)                             

          DO K=KTS,KTE 
          DO J=max(jds+(0),jts-(0)),min(jde-(0),jte+(0))
          DO I=max(ids+(0),its-(0)),min(ide-(0),ite+(0))
            Q(I,J,K)=MOIST(I,J,K,P_QV)/(1.+MOIST(I,J,K,P_QV))           
          ENDDO
          ENDDO    
          ENDDO   


        ENDIF had2_micro_check


        had2_tim=had2_tim+timef()-btimx
      ENDIF


      endif 














      NUM_OZMIXM=1
      NUM_AEROSOLC=1

      IF(NTSD<=0)THEN
        NTSD_rad=NTSD
      ELSE




        NTSD_rad=NTSD+1
      ENDIF

      IF(MOD(NTSD_rad,GRID%NRADS)==0.OR.                               &
     &   MOD(NTSD_rad,GRID%NRADL)==0)THEN

        btimx=timef()
        IF(OPERATIONAL_PHYSICS)THEN
          CALL UPDATE_MOIST(MOIST,Q,CWM,F_ICE,F_RAIN,N_MOIST           &
     &                     ,IDS,IDF,JDS,JDF,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE)
        ENDIF

        CALL RADIATION(NTSD_rad,GRID%DT,GRID%JULDAY,GRID%JULYR         &
     &                ,GRID%XTIME,GRID%JULIAN                          &
     &                ,IHRST,GRID%NPHS                                 &
     &                ,GLAT,GLON,GRID%NRADS,GRID%NRADL                 &
     &                ,DETA1,DETA2,AETA1,AETA2,ETA1,ETA2,PDTOP,PT      &
     &                ,PD,RES,PINT,T,Q,MOIST,THS,ALBEDO,EPSR           &
     &                ,F_ICE,F_RAIN                                    &
     &                ,SM,HBM2,CLDFRA,N_MOIST,RESTRT                   &
     &                ,RLWTT,RSWTT,RLWIN,RSWIN,RSWINC,RSWOUT           &
     &                ,RLWTOA,RSWTOA,CZMEAN                            &
     &                ,CFRACL,CFRACM,CFRACH,SIGT4                      &
     &                ,ACFRST,NCFRST,ACFRCV,NCFRCV                     &
     &                ,CUPPT,VEGFRC,SNO,HTOP,HBOT                      &
     &                ,Z,SICE,NUM_AEROSOLC,NUM_OZMIXM                  &
     &                ,GRID,CONFIG_FLAGS                               &
     &                ,RTHRATEN                                        &  
     &                ,IDS,IDF,JDS,JDF,KDS,KDE                         &
     &                ,IMS,IME,JMS,JME,KMS,KME                         &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)

        DO J=JMS,JME
        DO I=IMS,IME
          GSW(I,J)=RSWIN(I,J)-RSWOUT(I,J)
        ENDDO
        ENDDO






        radiation_tim=radiation_tim+timef()-btimx
      ENDIF





      btimx=timef()






      CALL RDTEMP(NTSD,GRID%DT,GRID%JULDAY,GRID%JULYR                  &
     &           ,GRID%XTIME,IHRST,GLAT,GLON                           &
     &           ,CZEN,CZMEAN,T,RSWTT,RLWTT,HBM2                       &
     &           ,IDS,IDF,JDS,JDF,KDS,KDE                              &
     &           ,IMS,IME,JMS,JME,KMS,KME                              &
     &           ,ITS,ITE,JTS,JTE,KTS,KTE)

      rdtemp_tim=rdtemp_tim+timef()-btimx






      IF(MOD(NTSD,GRID%NPHS)==0)THEN

        btimx=timef()

        IF(OPERATIONAL_PHYSICS                                         &
     &    .AND.MOD(NTSD_rad,GRID%NRADS)/=0                             &
     &    .AND.MOD(NTSD_rad,GRID%NRADL)/=0)THEN
          CALL UPDATE_MOIST(MOIST,Q,CWM,F_ICE,F_RAIN,N_MOIST           &
     &                     ,IDS,IDF,JDS,JDF,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE)
        ENDIF

        CALL TURBL(NTSD,GRID%DT,GRID%NPHS,RESTRT                       &
     &            ,N_MOIST,GRID%NUM_SOIL_LAYERS,SLDPTH,DZSOIL          &
     &            ,DETA1,DETA2,AETA1,AETA2,ETA1,ETA2,PDTOP,PT          &
     &            ,SM,HBM2,VBM2,DX_NMM,DFRLG                           &
     &            ,CZEN,CZMEAN,SIGT4,RLWIN,RSWIN,RADOT                 &
     &            ,PD,RES,PINT,T,Q,CWM,F_ICE,F_RAIN,SR                 &
     &            ,Q2,U,V,THS,NMM_TSK,SST,PREC,SNO                     &
     &            ,FIS,Z0,Z0BASE,USTAR,MIXHT,PBLH,LPBL,EL_MYJ          &   
     &            ,MOIST,RMOL,MOL                                      &
     &            ,EXCH_H,EXCH_M,F,AKHS,AKMS,AKHS_OUT,AKMS_OUT         &
     &            ,THZ0,QZ0,UZ0,VZ0,QSH,MAVAIL                         &
     &            ,STC,SMC,CMC,SMSTAV,SMSTOT,SSROFF,BGROFF             &
     &            ,IVGTYP,ISLTYP,VEGFRC,SHDMIN,SHDMAX,GRNFLX           &
     &            ,SNOTIME                                             &
     &            ,SFCEXC,ACSNOW,ACSNOM,SNOPCX,SICE,TG,SOILTB          &
     &            ,ALBASE,MXSNAL,ALBEDO,SH2O,SI,EPSR,EMBCK             &
     &            ,U10,V10,TH10,Q10,TSHLTR,QSHLTR,PSHLTR               &
     &            ,T2,QSG,QVG,QCG,SOILT1,TSNAV,SMFR3D,KEEPFR3DFLAG     &
     &            ,TWBS,QWBS,TAUX,TAUY,SFCSHX,SFCLHX,SFCEVP                      &
     &            ,POTEVP,POTFLX,SUBSHX                                &
     &            ,APHTIM,ARDSW,ARDLW,ASRFC                            &
     &            ,RSWOUT,RSWTOA,RLWTOA                                &
     &            ,ASWIN,ASWOUT,ASWTOA,ALWIN,ALWOUT,ALWTOA             &
     &            ,UZ0H,VZ0H,DUDT,DVDT,UGWDsfc,VGWDsfc,SFENTH          & 
     &            ,RTHBLTEN,RQVBLTEN                                   & 
     &            ,GRID%PCPFLG,DDATA                                   &
     &            ,HSTDV,HCNVX,HASYW,HASYS,HASYSW,HASYNW,HLENW,HLENS   & 
     &            ,HLENSW,HLENNW,HANGL,HANIS,HSLOP,HZMAX,CROT,SROT     & 
     &            ,GRID,CONFIG_FLAGS                                   &
     &            ,IHE,IHW,IVE,IVW                                     &
     &            ,IDS,IDF,JDS,JDF,KDS,KDE                             &
     &            ,IMS,IME,JMS,JME,KMS,KME                             &
     &            ,ITS,ITE,JTS,JTE,KTS,KTE)






        turbl_tim=turbl_tim+timef()-btimx

        btimx=timef()







CALL HALO_NMM_TURBL_A_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )








CALL HALO_NMM_TURBL_B_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


        exch_tim=exch_tim+timef()-btimx







        btimx=timef()
        CALL UV_H_TO_V(NTSD,GRID%DT,GRID%NPHS,UZ0H,VZ0H,UZ0,VZ0         &
     &                ,DUDT,DVDT,U,V,HBM2,IVE,IVW                       &
     &                ,IDS,IDF,JDS,JDF,KDS,KDE                          &
     &                ,IMS,IME,JMS,JME,KMS,KME                          &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)
        uv_htov_tim=uv_htov_tim+timef()-btimx





        btimx=timef()







CALL HALO_NMM_J_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


        IF(CONFIG_FLAGS%MP_PHYSICS/=ETAMPNEW)THEN






CALL HALO_NMM_J_3_sub ( grid, &
  num_moist, &
  moist, &
  num_scalar, &
  scalar, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

        ENDIF


        exch_tim=exch_tim+timef()-btimx





        ICLTEND=-1
        btimx=timef()

        CALL CLTEND(ICLTEND,GRID%NPHS,T,T_OLD,T_ADJ                    &
     &             ,IDS,IDF,JDS,JDF,KDS,KDE                            &
     &             ,IMS,IME,JMS,JME,KMS,KME                            &
     &             ,ITS,ITE,JTS,JTE,KTS,KTE)

        cltend_tim=cltend_tim+timef()-btimx
      ENDIF





      IF(MOD(NTSD,GRID%NCNVC)==0.AND.                                  &
     &   CONFIG_FLAGS%CU_PHYSICS==KFETASCHEME)THEN

        btimx=timef()







CALL HALO_NMM_C_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


        exch_tim=exch_tim+timef()-btimx




      ENDIF

      convection: IF(CONFIG_FLAGS%CU_PHYSICS/=0)THEN
        btimx=timef()



        IF(CONFIG_FLAGS%CU_PHYSICS==GDSCHEME)THEN
          DT_INV=1./GRID%DT
          DO J=JMS,JME
          DO K=KMS,KME
          DO I=IMS,IME
            TTEN(I,K,J)=(T(I,J,K)-TTEN(I,K,J))*DT_INV
            QTEN(I,K,J)=(Q(I,J,K)-QTEN(I,K,J))*DT_INV
          ENDDO
          ENDDO
          ENDDO
        ENDIF



        IF(OPERATIONAL_PHYSICS                                         &
     &    .AND.MOD(NTSD_rad,GRID%NRADS)/=0                             &
     &    .AND.MOD(NTSD_rad,GRID%NRADL)/=0                             &
     &    .AND.MOD(NTSD,GRID%NPHS)/=0)THEN
          CALL UPDATE_MOIST(MOIST,Q,CWM,F_ICE,F_RAIN,N_MOIST           &
     &                     ,IDS,IDF,JDS,JDF,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE)
        ENDIF


        CALL CUCNVC(NTSD,GRID%DT,GRID%NCNVC,GRID%NRADS,GRID%NRADL      &
     &             ,GPS,RESTRT,HYDRO,CLDEFI,N_MOIST,GRID%ENSDIM        &
     &             ,MOIST                                              &
     &             ,DETA1,DETA2,AETA1,AETA2,ETA1,ETA2                  &
     &             ,F_ICE,F_RAIN                                       &

     &             ,APR_GR,APR_W,APR_MC,TTEN,QTEN                      &
     &             ,APR_ST,APR_AS,APR_CAPMA                            &
     &             ,APR_CAPME,APR_CAPMI                                &
     &             ,MASS_FLUX,XF_ENS                                   &
     &             ,PR_ENS,GSW                                         &

     &             ,PDTOP,PT,PD,RES,PINT,T,Q,CWM,TCUCN                 &
     &             ,OMGALF,U,V,W,Z,FIS,W0AVG                           &
     &             ,PREC,ACPREC,CUPREC,CUPPT,CPRATE                    &
     &             ,SM,HBM2,LPBL,CNVBOT,CNVTOP                         &
     &             ,HTOP,HBOT,HTOPD,HBOTD,HTOPS,HBOTS                  &
     &             ,RTHBLTEN,RQVBLTEN,RTHRATEN                         & 
     &             ,AVCNVC,ACUTIM,IHE,IHW                              &
     &             ,GRID,CONFIG_FLAGS                                  &
     &             ,IDS,IDF,JDS,JDF,KDS,KDE                            &
     &             ,IMS,IME,JMS,JME,KMS,KME                            &
     &             ,IPS,IPE,JPS,JPE,KPS,KPE                            &
     &             ,ITS,ITE,JTS,JTE,KTS,KTE)


        cucnvc_tim=cucnvc_tim+timef()-btimx

      ENDIF convection





      IF(MOD(NTSD,GRID%NPHS)==0)THEN
        btimx=timef()

        CALL GSMDRIVE(NTSD,GRID%DT,GRID%NPHS,N_MOIST                   &
     &               ,DX_NMM(ITS,JC),GRID%DY,SM,HBM2,FIS               &
     &               ,DETA1,DETA2,AETA1,AETA2,ETA1,ETA2                &
     &               ,PDTOP,PT,PD,RES,PINT,T,Q,CWM,TRAIN               &
     &               ,MOIST,SCALAR,NUM_SCALAR                          &
     &               ,F_ICE,F_RAIN,F_RIMEF,SR                          &
     &               ,PREC,ACPREC,AVRAIN                               &
     &               ,MP_RESTART_STATE                                 &
     &               ,TBPVS_STATE                                      &
     &               ,TBPVS0_STATE                                     &
     &               ,GRID,CONFIG_FLAGS                                &
     &               ,IDS,IDF,JDS,JDF,KDS,KDE                          &
     &               ,IMS,IME,JMS,JME,KMS,KME                          &
     &               ,ITS,ITE,JTS,JTE,KTS,KTE)

        gsmdrive_tim=gsmdrive_tim+timef()-btimx





        IF (GRID%PCPFLG) THEN
          btimx=timef()

          CALL CHKSNOW(NTSD,GRID%DT,GRID%NPHS,SR,PPTDAT                 &
     &      ,IDS,IDE,JDS,JDE,KDS,KDE                                    &
     &      ,IMS,IME,JMS,JME,KMS,KME                                    &
     &      ,ITS,ITE,JTS,JTE,KTS,KTE)
          CALL ADJPPT(NTSD,GRID%DT,GRID%NPHS,PREC,LSPA,PPTDAT,DDATA     &
     &      ,IDS,IDE,JDS,JDE,KDS,KDE                                    &
     &      ,IMS,IME,JMS,JME,KMS,KME                                    &
     &      ,ITS,ITE,JTS,JTE,KTS,KTE)

          adjppt_tim=adjppt_tim+timef()-btimx
        ENDIF





        ICLTEND=0
        btimx=timef()

        CALL CLTEND(ICLTEND,GRID%NPHS,T,T_OLD,T_ADJ                    &
     &             ,IDS,IDF,JDS,JDF,KDS,KDE                            &
     &             ,IMS,IME,JMS,JME,KMS,KME                            &
     &             ,ITS,ITE,JTS,JTE,KTS,KTE)

        cltend_tim=cltend_tim+timef()-btimx
      ENDIF





      ICLTEND=1
      btimx=timef()

      CALL CLTEND(ICLTEND,GRID%NPHS,T,T_OLD,T_ADJ                      &
     &           ,IDS,IDF,JDS,JDF,KDS,KDE                              &
     &           ,IMS,IME,JMS,JME,KMS,KME                              &
     &           ,ITS,ITE,JTS,JTE,KTS,KTE)

      cltend_tim=cltend_tim+timef()-btimx





      btimx=timef()







CALL HALO_NMM_K_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


      exch_tim=exch_tim+timef()-btimx





      btimx=timef()

      CALL HDIFF(NTSD,GRID%DT,FIS,DY_NMM,HDAC,HDACV                    &
     &          ,HBM2,DETA1,GRID%SIGMA                                 &
     &          ,T,Q,cwm,U,V,Q2,Z,W,SM,SICE                                &
     &          ,IHE,IHW,IVE,IVW                                       &
     &          ,IDS,IDF,JDS,JDF,KDS,KDE                               &
     &          ,IMS,IME,JMS,JME,KMS,KME                               &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE)

      IF(.NOT.OPERATIONAL_PHYSICS)THEN
        DO K=KTS,KTE
        DO J=max(jds+(0),jts-(0)),min(jde-(0),jte+(0))
        DO I=max(ids+(0),its-(0)),min(ide-(0),ite+(0))

          MOIST(I,J,K,P_QV)=Q(I,J,K)/(1.-Q(I,J,K))           
        ENDDO
        ENDDO
        ENDDO
      ENDIF

      hdiff_tim=hdiff_tim+timef()-btimx





      btimx=timef()







CALL HALO_NMM_L_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )








CALL HALO_NMM_L_3_sub ( grid, &
  num_moist, &
  moist, &
  num_scalar, &
  scalar, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )



      exch_tim=exch_tim+timef()-btimx





      btimx=timef()


      CALL BOCOH(GRID%ID,NTSD,GRID%DT,NEST,NUNIT_NBC,NBOCO,LAST_TIME,TSPH &
     &          ,LB,ETA1,ETA2,PDTOP,PT,RES                              &
     &          ,PD_BXS,PD_BXE,PD_BYS,PD_BYE,T_BXS,T_BXE,T_BYS,T_BYE    &
     &          ,Q_BXS,Q_BXE,Q_BYS,Q_BYE,U_BXS,U_BXE,U_BYS,U_BYE,V_BXS  &
     &          ,V_BXE,V_BYS,V_BYE,Q2_BXS,Q2_BXE,Q2_BYS,Q2_BYE,CWM_BXS  &
     &          ,CWM_BXE,CWM_BYS,CWM_BYE,PD_BTXS,PD_BTXE,PD_BTYS        &
     &          ,PD_BTYE,T_BTXS,T_BTXE,T_BTYS,T_BTYE,Q_BTXS,Q_BTXE      &
     &          ,Q_BTYS,Q_BTYE,U_BTXS,U_BTXE,U_BTYS,U_BTYE,V_BTXS       &
     &          ,V_BTXE,V_BTYS,V_BTYE,Q2_BTXS,Q2_BTXE,Q2_BTYS,Q2_BTYE   &
     &          ,CWM_BTXS,CWM_BTXE,CWM_BTYS,CWM_BTYE,PD,T,Q,Q2,CWM,PINT &
     &          ,MOIST,N_MOIST,SCALAR,NUM_SCALAR                        &
     &          ,GRID%SPEC_BDY_WIDTH,Z                                  &
     &          ,IHE,IHW,IVE,IVW                                        &
     &          ,IDS,IDF,JDS,JDF,KDS,KDE                                &
     &          ,IMS,IME,JMS,JME,KMS,KME                                &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE)

  978	continue

 

      bocoh_tim=bocoh_tim+timef()-btimx











 2003 CONTINUE





      btimx=timef()







CALL HALO_NMM_A_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


      IF(CONFIG_FLAGS%MP_PHYSICS/=ETAMPNEW)THEN






CALL HALO_NMM_A_3_sub ( grid, &
  num_moist, &
  moist, &
  num_scalar, &
  scalar, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

      ENDIF


      exch_tim=exch_tim+timef()-btimx





      btimx=timef()

      CALL PFDHT(NTSD,LAST_TIME,PT,DETA1,DETA2,PDTOP,RES,FIS           &
     &          ,HYDRO,GRID%SIGMA,FIRST,DX_NMM,DY_NMM                  &
     &          ,HBM2,VBM2,VBM3                                        &
     &          ,FDIV,FCP,WPDAR,DFL,CPGFU,CPGFV                        &
     &          ,PD,PDSL,T,Q,U,V,CWM,OMGALF,PINT,DWDT                  &
     &          ,RTOP,DIV,FEW,FNS,FNE,FSE                              &
     &          ,IHE,IHW,IVE,IVW                                       &
     &          ,IDS,IDF,JDS,JDF,KDS,KDE                               &
     &          ,IMS,IME,JMS,JME,KMS,KME                               &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE)


      pfdht_tim=pfdht_tim+timef()-btimx





      btimx=timef()







CALL HALO_NMM_B_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


      exch_tim=exch_tim+timef()-btimx





      btimx=timef()

      CALL DDAMP(NTSD,GRID%DT,DETA1,DETA2,PDSL,PDTOP,DIV,HBM2          &
     &          ,T,U,V,DDMPU,DDMPV                                     &
     &          ,IHE,IHW,IVE,IVW                                       &
     &          ,IDS,IDF,JDS,JDF,KDS,KDE                               &
     &          ,IMS,IME,JMS,JME,KMS,KME                               &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE)

      ddamp_tim=ddamp_tim+timef()-btimx




      IF(FIRST.AND.NTSD==0)THEN
        FIRST=.FALSE.
        btimx=timef()







CALL HALO_NMM_A_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


        exch_tim=exch_tim+timef()-btimx





        GO TO 2000
      ENDIF





      btimx=timef()







CALL HALO_NMM_C_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


      exch_tim=exch_tim+timef()-btimx





      btimx=timef()


      CALL BOCOV(GRID%ID,NTSD,GRID%DT,LB,U_BXS,U_BXE,U_BYS,U_BYE,V_BXS &
     &          ,V_BXE,V_BYS,V_BYE,U_BTXS,U_BTXE,U_BTYS,U_BTYE,V_BTXS  &
     &          ,V_BTXE,V_BTYS,V_BTYE,U,V                              &
     &          ,GRID%SPEC_BDY_WIDTH                                   &
     &          ,IHE,IHW,IVE,IVW                                       &
     &          ,IDS,IDF,JDS,JDF,KDS,KDE                               &
     &          ,IMS,IME,JMS,JME,KMS,KME                               &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE )
  
  979   continue
 

      bocov_tim=bocov_tim+timef()-btimx





      DO K=KTS,KTE
      DO J=JTS,JTE
      DO I=ITS,ITE
        TKE_MYJ(I,J,K)=0.5*Q2(I,J,K) 
      ENDDO
      ENDDO
      ENDDO



      IF(LAST_TIME.AND.ALLOCATED(PPTDAT))THEN
        DEALLOCATE(PPTDAT,STAT=ISTAT)
      ENDIF



      solve_tim=solve_tim+timef()-btim











        if (.not. micro_has_been_read) then

        CALL MICRO_INIT(MASSI,MASSR,RQR_DRmin,RQR_DRmax,CN0r0,CN0r_DMRmin,CN0r_DMRmax)

        endif

        IF(.NOT.ALLOCATED(UPINDX))ALLOCATE(UPINDX(IMS:IME,JMS:JME))







      IF(MOD(NTSD,GRID%IDTAD)==0)THEN


      DO J=JTS,MIN(JTE,JDF)
      DO I=ITS,MIN(ITE,IDF)
      DO K=2,KTE-1

       ZMID=(Z(I,J,K) + Z(I,J,K+1))/2.
       ZMIDP1=(Z(I,J,K+1) + Z(I,J,K+2))/2.

        if ( (ZMID-Z(I,J,KTS)) .le. 1000. &
        .and.(ZMIDP1-Z(I,J,KTS)) .ge. 1000.) then

        ZMID=ZMID-Z(I,J,KTS)
        ZMIDP1=ZMIDP1-Z(I,J,KTS)

        wgta=1.-((ZMIDP1-1000.)/(ZMIDP1-ZMID))
        wgtb=1.-wgta

          P1Da=AETA1(K+1)*PDTOP+AETA2(K+1)*PD(I,J)+PT
          T1Da=T(I,J,K+1)
          Q1Da=Q(I,J,K+1)
          C1Da=CWM(I,J,K+1)
          FI1Da=F_ICE(I,K+1,J)
          FR1Da=F_RAIN(I,K+1,J)
          FS1Da=F_RIMEF(I,K+1,J)

          P1Db=AETA1(K)*PDTOP+AETA2(K)*PD(I,J)+PT
          T1Db=T(I,J,K)
          Q1Db=Q(I,J,K)
          C1Db=CWM(I,J,K)
          FI1Db=F_ICE(I,K,J)
          FR1Db=F_RAIN(I,K,J)
          FS1Db=F_RIMEF(I,K,J)

        P1D=wgta*P1Da+wgtb*P1Db
        T1D=wgta*T1Da+wgtb*T1Db
        Q1D=wgta*Q1Da+wgtb*Q1Db
        C1D=wgta*C1Da+wgtb*C1Db
        FI1D=wgta*FI1Da+wgtb*FI1Db
        FR1D=wgta*FR1Da+wgtb*FR1Db
        FS1D=wgta*FS1Da+wgtb*FS1Db
          CUREFL=0.

        if (RQR_DRmin .eq. 0.) then
        call wrf_error_fatal3("",2834,&
"bad RQR_DRmin")
        endif

        if (C1D .ge. 1.e-12) then
          CALL CALMICT(P1D,T1D,Q1D,C1D,FI1D,FR1D,FS1D,CUREFL, &
                       DBZ1, TBPVS_STATE,CN0r_DMRmin, CN0r_DMRmax, RQR_DRmin, RQR_DRmax, CN0R0,  MASSI, &
                        NX, I, J)
        else
               DBZ1=-20.
        endif

          MAXDBZ(I,J)=max(MAXDBZ(I,J),DBZ1)









       endif

      ENDDO
      ENDDO
      ENDDO

      ENDIF




      DO K=KTS,KTE-1
      DO J=JTS,MIN(JTE,JDF)
      DO I=ITS,MIN(ITE,IDF)
        plow=AETA1(K)*PDTOP+AETA2(K)*PD(I,J)+PT
        pup=AETA1(K+1)*PDTOP+AETA2(K+1)*PD(I,J)+PT
        if (plow .ge. 40000. .and. pup  .le. 40000.) then
        UPINDX(I,J)=K
        endif
      ENDDO
      ENDDO
      ENDDO

      DO K=KTS,KTE-1
      DO J=JTS,MIN(JTE,JDF)
      DO I=ITS,MIN(ITE,IDF)
        if (K .le. UPINDX(I,J)) then
        maxupdr(I,J)=max(maxupdr(I,J),w(I,J,K))
        maxdndr(I,J)=min(maxdndr(I,J),w(I,J,K))
        endif
      ENDDO
      ENDDO
      ENDDO

      DO J=JTS,MIN(JTE,JDF)
      DO I=ITS,MIN(ITE,IDF)
       magw2=(U10(I,J)**2.+V10(I,J)**2.)
       magw=magw2**(0.5)
        if (NTSD >= 2) then
          if (magw .gt. max10mw(I,J)) then
           max10u(I,J)=U10(I,J)
           max10v(I,J)=V10(I,J)
           max10mw(I,J)=MAX(max10mw(I,J), magw)
          endif
        endif

      ENDDO
      ENDDO

        CALL calc_uphlcy(U,V,W,Z,maxhlcy,DX_NMM,DY_NMM &
                          ,IDS,IDF,JDS,JDF,KDS,KDE                                            &
                          ,IMS,IME,JMS,JME,KMS,KME                                            &
                          ,ITS,ITE,JTS,JTE,KTS,KTE                                            )













	










      sum_tim=pdte_tim+adve_tim+vtoa_tim+vadz_tim+hadz_tim+eps_tim     &
     &       +vad2_tim+had2_tim+radiation_tim+rdtemp_tim+turbl_tim     &
     &       +cltend_tim+cucnvc_tim+gsmdrive_tim+hdiff_tim             &
     &       +bocoh_tim+pfdht_tim+ddamp_tim+bocov_tim+uv_htov_tim      &
     &       +exch_tim+adjppt_tim

      if(mod(ntsd,n_print_time)==0)then
        write(message,*)' ntsd=',ntsd,' solve_tim=',solve_tim*1.e-3          &
     &           ,' sum_tim=',sum_tim*1.e-3
        call wrf_message(trim(message))
        write(message,*)' pdte_tim=',pdte_tim*1.e-3,' pct=',pdte_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' adve_tim=',adve_tim*1.e-3,' pct=',adve_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' vtoa_tim=',vtoa_tim*1.e-3,' pct=',vtoa_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' vadz_tim=',vadz_tim*1.e-3,' pct=',vadz_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' hadz_tim=',hadz_tim*1.e-3,' pct=',hadz_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' eps_tim=',eps_tim*1.e-3,' pct=',eps_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' vad2_tim=',vad2_tim*1.e-3,' pct=',vad2_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' had2_tim=',had2_tim*1.e-3,' pct=',had2_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' radiation_tim=',radiation_tim*1.e-3,' pct=',radiation_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' rdtemp_tim=',rdtemp_tim*1.e-3,' pct=',rdtemp_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' turbl_tim=',turbl_tim*1.e-3,' pct=',turbl_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' cltend_tim=',cltend_tim*1.e-3,' pct=',cltend_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' cucnvc_tim=',cucnvc_tim*1.e-3,' pct=',cucnvc_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' gsmdrive_tim=',gsmdrive_tim*1.e-3,' pct=',gsmdrive_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' adjppt_tim=',adjppt_tim*1.e-3,' pct=',adjppt_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' hdiff_tim=',hdiff_tim*1.e-3,' pct=',hdiff_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' bocoh_tim=',bocoh_tim*1.e-3,' pct=',bocoh_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' pfdht_tim=',pfdht_tim*1.e-3,' pct=',pfdht_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' ddamp_tim=',ddamp_tim*1.e-3,' pct=',ddamp_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' bocov_tim=',bocov_tim*1.e-3,' pct=',bocov_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' uv_h_to_v_tim=',uv_htov_tim*1.e-3,' pct=',uv_htov_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' exch_tim=',exch_tim*1.e-3,' pct=',exch_tim/sum_tim*100.
        call wrf_message(trim(message))




        call field_stats(t,mype,mpi_comm_comp                          &
     &                  ,ids,ide,jds,jde,kds,kde                       &
     &                  ,ims,ime,jms,jme,kms,kme                       &
     &                  ,its,ite,jts,jte,kts,kte)


        write(0,*) 'max fields reset'
      DO J=JTS,MIN(JTE,JDE-1)
      DO I=ITS,MIN(ITE,IDE-1)
       max10mw(I,J)=-999.
       max10u(I,J)=-999.
       max10v(I,J)=-999.
       maxupdr(I,J)=-999.
       maxdndr(I,J)=999.
       maxhlcy(I,J)=-999.
       maxdbz(I,J)=-20.
      ENDDO
      ENDDO

      endif


      DEALLOCATE(TTEN,STAT=ISTAT)
      DEALLOCATE(QTEN,STAT=ISTAT)
      DEALLOCATE(RTHRATEN,STAT=ISTAT)
      DEALLOCATE(RTHBLTEN,STAT=ISTAT)
      DEALLOCATE(RQVBLTEN,STAT=ISTAT)








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


      Return







      END SUBROUTINE SOLVE_NMM



      SUBROUTINE TWR(ARRAY,KK,FIELD,NTSD,MYPE,NPES,MPI_COMM_COMP       &
     &              ,IDS,IDE,JDS,JDE,KDS,KDE                           &
     &              ,IMS,IME,JMS,JME,KMS,KME                           &
     &              ,ITS,ITE,JTS,JTE,KTS,KTE)


      USE MODULE_EXT_INTERNAL

      IMPLICIT NONE
      INCLUDE "mpif.h"


      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE                    &
     &                     ,KK,MPI_COMM_COMP,MYPE,NPES,NTSD

      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME+KK),INTENT(IN) :: ARRAY

      CHARACTER(*),INTENT(IN) :: FIELD



      INTEGER,DIMENSION(MPI_STATUS_SIZE) :: JSTAT
      INTEGER,DIMENSION(MPI_STATUS_SIZE,4) :: STATUS_ARRAY
      INTEGER,DIMENSION(2) :: IM_REM,JM_REM,IT_REM,JT_REM

      INTEGER :: I,IENDX,IER,IPE,IRECV,IRTN,ISEND,IUNIT                &
     &          ,J,K,N,NLEN,NSIZE
      INTEGER :: ITS_REM,ITE_REM,JTS_REM,JTE_REM

      REAL,DIMENSION(IDS:IDE,JDS:JDE) :: TWRITE
      REAL,ALLOCATABLE,DIMENSION(:) :: VALUES
      CHARACTER(5) :: TIMESTEP
      CHARACTER(6) :: FMT
      CHARACTER(12) :: FILENAME




      IF(NTSD<=9)THEN
        FMT='(I1.1)'
        NLEN=1
      ELSEIF(NTSD<=99)THEN
        FMT='(I2.2)'
        NLEN=2
      ELSEIF(NTSD<=999)THEN
        FMT='(I3.3)'
        NLEN=3
      ELSEIF(NTSD<=9999)THEN
        FMT='(I4.4)'
        NLEN=4
      ELSEIF(NTSD<=99999)THEN
        FMT='(I5.5)'
        NLEN=5
      ENDIF
      WRITE(TIMESTEP,FMT)NTSD
      FILENAME=FIELD//'_'//TIMESTEP(1:NLEN)

      IF(MYPE==0)THEN
        CALL INT_GET_FRESH_HANDLE(IUNIT)
        CLOSE(IUNIT)
        OPEN(UNIT=IUNIT,FILE=FILENAME,FORM='UNFORMATTED',IOSTAT=IER)
      ENDIF




      DO 500 K=KDE-1,KDS,-1   


      IF(MYPE==0)THEN
        DO J=JTS,JTE
        DO I=ITS,ITE
          TWRITE(I,J)=ARRAY(I,J,K)
        ENDDO
        ENDDO

        DO IPE=1,NPES-1
          CALL MPI_RECV(IT_REM,2,MPI_INTEGER,IPE,IPE                    &
     &                 ,MPI_COMM_COMP,JSTAT,IRECV)
          CALL MPI_RECV(JT_REM,2,MPI_INTEGER,IPE,IPE                    &
     &                 ,MPI_COMM_COMP,JSTAT,IRECV)

          ITS_REM=IT_REM(1)
          ITE_REM=IT_REM(2)
          JTS_REM=JT_REM(1)
          JTE_REM=JT_REM(2)

          NSIZE=(ITE_REM-ITS_REM+1)*(JTE_REM-JTS_REM+1)
          ALLOCATE(VALUES(1:NSIZE))

          CALL MPI_RECV(VALUES,NSIZE,MPI_REAL,IPE,IPE                   &
     &                 ,MPI_COMM_COMP,JSTAT,IRECV)
          N=0
          DO J=JTS_REM,JTE_REM
            DO I=ITS_REM,ITE_REM
              N=N+1
              TWRITE(I,J)=VALUES(N)
            ENDDO
          ENDDO

          DEALLOCATE(VALUES)

        ENDDO


      ELSE
        NSIZE=(ITE-ITS+1)*(JTE-JTS+1)
        ALLOCATE(VALUES(1:NSIZE))

        N=0
        DO J=JTS,JTE
        DO I=ITS,ITE
          N=N+1
          VALUES(N)=ARRAY(I,J,K)
        ENDDO
        ENDDO

        IT_REM(1)=ITS
        IT_REM(2)=ITE
        JT_REM(1)=JTS
        JT_REM(2)=JTE

        CALL MPI_SEND(IT_REM,2,MPI_INTEGER,0,MYPE                       &
     &               ,MPI_COMM_COMP,ISEND)
        CALL MPI_SEND(JT_REM,2,MPI_INTEGER,0,MYPE                       &
     &               ,MPI_COMM_COMP,ISEND)

        CALL MPI_SEND(VALUES,NSIZE,MPI_REAL,0,MYPE                      &
     &               ,MPI_COMM_COMP,ISEND)

        DEALLOCATE(VALUES)

      ENDIF


      CALL MPI_BARRIER(MPI_COMM_COMP,IRTN)

      IF(MYPE==0)THEN

        DO J=JDS,JDE-1
          IENDX=IDE-1
          IF(MOD(J,2)==0)IENDX=IENDX-1
          WRITE(IUNIT)(TWRITE(I,J),I=1,IENDX)
        ENDDO

      ENDIF



  500 CONTINUE

      IF(MYPE==0)CLOSE(IUNIT)


      END SUBROUTINE TWR



      SUBROUTINE VWR(ARRAY,KK,FIELD,NTSD,MYPE,NPES,MPI_COMM_COMP       &
     &              ,IDS,IDE,JDS,JDE,KDS,KDE                           &
     &              ,IMS,IME,JMS,JME,KMS,KME                           &
     &              ,ITS,ITE,JTS,JTE,KTS,KTE)


      USE MODULE_EXT_INTERNAL

      IMPLICIT NONE
      INCLUDE "mpif.h"


      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE                    &
     &                     ,KK,MPI_COMM_COMP,MYPE,NPES,NTSD

      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME+KK),INTENT(IN) :: ARRAY

      CHARACTER(*),INTENT(IN) :: FIELD



      INTEGER,DIMENSION(MPI_STATUS_SIZE) :: JSTAT
      INTEGER,DIMENSION(MPI_STATUS_SIZE,4) :: STATUS_ARRAY
      INTEGER,DIMENSION(2) :: IM_REM,JM_REM,IT_REM,JT_REM

      INTEGER :: I,IENDX,IER,IPE,IRECV,IRTN,ISEND,IUNIT                &
     &          ,J,K,L,N,NLEN,NSIZE
      INTEGER :: ITS_REM,ITE_REM,JTS_REM,JTE_REM

      REAL,DIMENSION(IDS:IDE,JDS:JDE) :: TWRITE
      REAL,ALLOCATABLE,DIMENSION(:) :: VALUES
      CHARACTER(5) :: TIMESTEP
      CHARACTER(6) :: FMT
      CHARACTER(12) :: FILENAME
      LOGICAL :: OPENED




      IF(NTSD<=9)THEN
        FMT='(I1.1)'
        NLEN=1
      ELSEIF(NTSD<=99)THEN
        FMT='(I2.2)'
        NLEN=2
      ELSEIF(NTSD<=999)THEN
        FMT='(I3.3)'
        NLEN=3
      ELSEIF(NTSD<=9999)THEN
        FMT='(I4.4)'
        NLEN=4
      ELSEIF(NTSD<=99999)THEN
        FMT='(I5.5)'
        NLEN=5
      ENDIF
      WRITE(TIMESTEP,FMT)NTSD
      FILENAME=FIELD//'_'//TIMESTEP(1:NLEN)

      IF(MYPE==0)THEN
        CALL INT_GET_FRESH_HANDLE(IUNIT)
        CLOSE(IUNIT)
        OPEN(UNIT=IUNIT,FILE=FILENAME,FORM='UNFORMATTED',IOSTAT=IER)
      ENDIF



















      DO 500 K=KDE-1,KDS,-1   


      IF(MYPE==0)THEN
        DO J=JTS,JTE
        DO I=ITS,ITE
          TWRITE(I,J)=ARRAY(I,J,K)
        ENDDO
        ENDDO

        DO IPE=1,NPES-1
          CALL MPI_RECV(IT_REM,2,MPI_INTEGER,IPE,IPE                    &
     &                 ,MPI_COMM_COMP,JSTAT,IRECV)
          CALL MPI_RECV(JT_REM,2,MPI_INTEGER,IPE,IPE                    &
     &                 ,MPI_COMM_COMP,JSTAT,IRECV)

          ITS_REM=IT_REM(1)
          ITE_REM=IT_REM(2)
          JTS_REM=JT_REM(1)
          JTE_REM=JT_REM(2)

          NSIZE=(ITE_REM-ITS_REM+1)*(JTE_REM-JTS_REM+1)
          ALLOCATE(VALUES(1:NSIZE))

          CALL MPI_RECV(VALUES,NSIZE,MPI_REAL,IPE,IPE                   &
     &                 ,MPI_COMM_COMP,JSTAT,IRECV)
          N=0
          DO J=JTS_REM,JTE_REM
            DO I=ITS_REM,ITE_REM
              N=N+1
              TWRITE(I,J)=VALUES(N)
            ENDDO
          ENDDO

          DEALLOCATE(VALUES)

        ENDDO


      ELSE
        NSIZE=(ITE-ITS+1)*(JTE-JTS+1)
        ALLOCATE(VALUES(1:NSIZE))

        N=0
        DO J=JTS,JTE
        DO I=ITS,ITE
          N=N+1
          VALUES(N)=ARRAY(I,J,K)
        ENDDO
        ENDDO

        IT_REM(1)=ITS
        IT_REM(2)=ITE
        JT_REM(1)=JTS
        JT_REM(2)=JTE

        CALL MPI_SEND(IT_REM,2,MPI_INTEGER,0,MYPE                       &
     &               ,MPI_COMM_COMP,ISEND)
        CALL MPI_SEND(JT_REM,2,MPI_INTEGER,0,MYPE                       &
     &               ,MPI_COMM_COMP,ISEND)

        CALL MPI_SEND(VALUES,NSIZE,MPI_REAL,0,MYPE                      &
     &               ,MPI_COMM_COMP,ISEND)

        DEALLOCATE(VALUES)

      ENDIF


      CALL MPI_BARRIER(MPI_COMM_COMP,IRTN)

      IF(MYPE==0)THEN

        DO J=JDS,JDE-1
          IENDX=IDE-1
          IF(MOD(J,2)==1)IENDX=IENDX-1
          WRITE(IUNIT)(TWRITE(I,J),I=1,IENDX)
        ENDDO

      ENDIF


  500 CONTINUE

      IF(MYPE==0)CLOSE(IUNIT)


      END SUBROUTINE VWR



      SUBROUTINE EXIT(NAME,PINT,T,Q,U,V,Q2,W                           &
     &               ,NTSD,MYPE,MPI_COMM_COMP                          &
     &               ,IDS,IDE,JDS,JDE,KDS,KDE                          &
     &               ,IMS,IME,JMS,JME,KMS,KME                          &
     &               ,ITS,ITE,JTS,JTE,KTS,KTE)


      USE MODULE_EXT_INTERNAL


      IMPLICIT NONE

      INCLUDE "mpif.h"

      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE                    &
     &                     ,MYPE,MPI_COMM_COMP,NTSD

      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(IN) :: PINT,T,Q   &
                                                           ,U,V,Q2,W
      CHARACTER(*),INTENT(IN) :: NAME

      INTEGER :: I,J,K,IEND,IERR,IRET
      CHARACTER(256) :: ERRMESS
      LOGICAL :: E_BDY,S_BDY

      IRET=0
  100 FORMAT(' EXIT ',A,' AT NTSD=',I5)
      IEND=ITE
      S_BDY=(JTS==JDS)
      E_BDY=(ITE==IDE-1)

      DO K=KTS,KTE
      DO J=JTS,JTE
      IEND=ITE
      IF(E_BDY.AND.MOD(J,2)==0)IEND=ITE-1

      DO I=ITS,IEND
        IF(T(I,J,K)>330..OR.T(I,J,K)<180..OR.T(I,J,K)/=T(I,J,K))THEN
          WRITE(errmess,100)NAME,NTSD
          CALL wrf_message(trim(errmess))
          WRITE(errmess,200)I,J,K,T(I,J,K),MYPE,NTSD
          CALL wrf_message(trim(errmess))
  200     FORMAT(' BAD VALUE I=',I3,' J=',I3,' K=',I2,' T=',E12.5      &
     &,          ' MYPE=',I3,' NTSD=',I5)
          IRET=666
          return

  205     FORMAT(' EXIT ',A,' TEMPERATURE=',E12.5                      &
     &,          ' AT (',I3,',',I2,',',I3,')',' MYPE=',I3)


        ELSEIF(Q(I,J,K)<-1.E-4.OR.Q(I,J,K)>30.E-3                      &
     &         .OR.Q(I,J,K)/=Q(I,J,K))THEN
          WRITE(errmess,100)NAME,NTSD
          CALL wrf_message(trim(errmess))
          WRITE(errmess,300)I,J,K,Q(I,J,K),MYPE,NTSD
          CALL wrf_message(trim(errmess))
  300     FORMAT(' BAD VALUE I=',I3,' J=',I3,' K=',I2,' Q=',E12.5      &
     &,          ' MYPE=',I3,' NTSD=',I5)
          IRET=666
          return

  305     FORMAT(' EXIT ',A,' SPEC HUMIDITY=',E12.5                    &
     &,          ' AT (',I3,',',I2,',',I3,')',' MYPE=',I3)


        ELSEIF(PINT(I,J,K)<0..OR.PINT(I,J,K)/=PINT(I,J,K))THEN
          WRITE(errmess,100)NAME,NTSD
          CALL wrf_message(trim(errmess))
          WRITE(errmess,315)I,J,K,PINT(I,J,K),MYPE,NTSD
          CALL wrf_message(trim(errmess))
  315     FORMAT(' BAD VALUE I=',I3,' J=',I3,' K=',I2,' PINT=',E12.5      &
     &,          ' MYPE=',I3,' NTSD=',I5)
          IRET=666
          return


        ELSEIF(W(I,J,K)/=W(I,J,K))THEN
          WRITE(errmess,100)NAME,NTSD
          CALL wrf_message(trim(errmess))
          WRITE(errmess,325)I,J,K,W(I,J,K),MYPE,NTSD
          CALL wrf_message(trim(errmess))
  325     FORMAT(' BAD VALUE I=',I3,' J=',I3,' K=',I2,' W=',E12.5      &
     &,          ' MYPE=',I3,' NTSD=',I5)
          IRET=666
          return


        ENDIF
      ENDDO
      ENDDO
      ENDDO

      DO K=KTS,KTE
      DO J=JTS,JTE
      IEND=ITE
      IF(E_BDY.AND.MOD(J,2)==1)IEND=ITE-1
      DO I=ITS,IEND
        IF(ABS(U(I,J,K))>125..OR.ABS(V(I,J,K))>125.                    &
     &         .OR.U(I,J,K)/=U(I,J,K).OR.V(I,J,K)/=V(I,J,K))THEN
          WRITE(errmess,100)NAME,NTSD
          CALL wrf_message(trim(errmess))
          WRITE(errmess,400)I,J,K,U(I,J,K),V(I,J,K),MYPE,NTSD
          CALL wrf_message(trim(errmess))
  400     FORMAT(' BAD VALUE I=',I3,' J=',I3,' K=',I2,' U=',E12.5      &
     &,          ' V=',E12.5,' MYPE=',I3,' NTSD=',I5)
          IRET=666
          return

  405     FORMAT(' EXIT ',A,' U=',E12.5,' V=',E12.5                    &
     &,          ' AT (',I3,',',I2,',',I3,')',' MYPE=',I3)


        ENDIF
      ENDDO
      ENDDO
      ENDDO

      END SUBROUTINE EXIT



      SUBROUTINE TIME_STATS(TIME_LCL,NAME,NTSD,MYPE,NPES,MPI_COMM_COMP)


      USE MODULE_EXT_INTERNAL


      IMPLICIT NONE

      INCLUDE "mpif.h"

      INTEGER,INTENT(IN) :: MPI_COMM_COMP,MYPE,NPES,NTSD
      REAL,INTENT(IN) :: TIME_LCL

      CHARACTER(*),INTENT(IN) :: NAME



      INTEGER,DIMENSION(MPI_STATUS_SIZE) :: JSTAT
      INTEGER,DIMENSION(MPI_STATUS_SIZE,4) :: STATUS_ARRAY
      INTEGER,ALLOCATABLE,DIMENSION(:) :: ID_PE,IPE_SORT

      INTEGER :: IPE,IPE_MAX,IPE_MEDIAN,IPE_MIN,IRECV,IRTN,ISEND       &
     &          ,N,N_MEDIAN,NLEN

      REAL,ALLOCATABLE,DIMENSION(:) :: TIME,SORT_TIME
      REAL,DIMENSION(2) :: REMOTE
      REAL :: TIME_MAX,TIME_MEAN,TIME_MEDIAN,TIME_MIN

      CHARACTER(5) :: TIMESTEP
      CHARACTER(6) :: FMT
      CHARACTER(25) :: TITLE
      CHARACTER(LEN=256) :: message




      IF(NTSD<=9)THEN
        FMT='(I1.1)'
        NLEN=1
      ELSEIF(NTSD<=99)THEN
        FMT='(I2.2)'
        NLEN=2
      ELSEIF(NTSD<=999)THEN
        FMT='(I3.3)'
        NLEN=3
      ELSEIF(NTSD<=9999)THEN
        FMT='(I4.4)'
        NLEN=4
      ELSEIF(NTSD<=99999)THEN
        FMT='(I5.5)'
        NLEN=5
      ENDIF
      WRITE(TIMESTEP,FMT)NTSD
      TITLE=NAME//'_'//TIMESTEP(1:NLEN)



      IF(MYPE==0)THEN
        ALLOCATE(TIME(1:NPES))
        ALLOCATE(SORT_TIME(1:NPES))
        ALLOCATE(ID_PE(1:NPES))
        ALLOCATE(IPE_SORT(1:NPES))

        TIME(1)=TIME_LCL
        ID_PE(1)=MYPE



        DO IPE=1,NPES-1
          CALL MPI_RECV(REMOTE,2,MPI_REAL,IPE,IPE                      &
     &                 ,MPI_COMM_COMP,JSTAT,IRECV)

          TIME(IPE+1)=REMOTE(1)
          ID_PE(IPE+1)=NINT(REMOTE(2))
        ENDDO




        TIME_MEAN=0.
        TIME_MAX=-1.
        TIME_MIN=1.E10
        IPE_MAX=-1
        IPE_MIN=-1

        DO N=1,NPES
          TIME_MEAN=TIME_MEAN+TIME(N)

          IF(TIME(N)>TIME_MAX)THEN
            TIME_MAX=TIME(N)
            IPE_MAX=ID_PE(N)
          ENDIF

          IF(TIME(N)<TIME_MIN)THEN
            TIME_MIN=TIME(N)
            IPE_MIN=ID_PE(N)
          ENDIF

        ENDDO

        TIME_MAX=TIME_MAX*1.E-3
        TIME_MIN=TIME_MIN*1.E-3
        TIME_MEAN=TIME_MEAN*1.E-3/REAL(NPES)



        CALL SORT(TIME,NPES,SORT_TIME,IPE_SORT)
        N_MEDIAN=(NPES+1)/2
        TIME_MEDIAN=SORT_TIME(N_MEDIAN)*1.E-3
        IPE_MEDIAN=IPE_SORT(N_MEDIAN)


      ELSE



        REMOTE(1)=TIME_LCL
        REMOTE(2)=REAL(MYPE)

        CALL MPI_SEND(REMOTE,2,MPI_REAL,0,MYPE,MPI_COMM_COMP,ISEND)

      ENDIF


      CALL MPI_BARRIER(MPI_COMM_COMP,IRTN)



      IF(MYPE==0)THEN
        WRITE(message,100)TITLE
        CALL wrf_message(trim(message))
        WRITE(message,105)TIME_MAX,IPE_MAX
        CALL wrf_message(trim(message))
        WRITE(message,110)TIME_MIN,IPE_MIN
        CALL wrf_message(trim(message))
        WRITE(message,115)TIME_MEDIAN,IPE_MEDIAN
        CALL wrf_message(trim(message))
        WRITE(message,120)TIME_MEAN
        CALL wrf_message(trim(message))
  100   FORMAT(' Time for ',A)
  105   FORMAT(' Maximum=',G11.5,' for PE ',I2.2)
  110   FORMAT(' Minimum=',G11.5,' for PE ',I2.2)
  115   FORMAT(' Median =',G11.5,' for PE ',I2.2)
  120   FORMAT(' Mean   =',G11.5)
      ENDIF


      END SUBROUTINE TIME_STATS




      SUBROUTINE SORT(DATA,NPES,DATA_SORTED,IPE_SORTED)






      IMPLICIT NONE

      INTEGER,INTENT(IN) :: NPES
      REAL,DIMENSION(NPES),INTENT(IN) :: DATA

      INTEGER,DIMENSION(NPES),INTENT(OUT) :: IPE_SORTED
      REAL,DIMENSION(NPES),INTENT(OUT) :: DATA_SORTED

      TYPE :: DATA_LINK
        REAL :: VALUE
        INTEGER :: IPE
        TYPE(DATA_LINK),POINTER :: NEXT_VALUE
      END TYPE





      INTEGER :: ISTAT,N

      TYPE(DATA_LINK),POINTER :: HEAD,TAIL  
      TYPE(DATA_LINK),POINTER :: PTR_NEW    
      TYPE(DATA_LINK),POINTER :: PTR1,PTR2  



      pe_loop: DO N=1,NPES
        ALLOCATE(PTR_NEW,STAT=ISTAT)  
        PTR_NEW%VALUE=DATA(N)
        PTR_NEW%IPE=N-1







        main: IF(N==1)THEN
          HEAD=>PTR_NEW
          TAIL=>HEAD
          NULLIFY(PTR_NEW%NEXT_VALUE)





        ELSE
          check: IF(PTR_NEW%VALUE<HEAD%VALUE)THEN
            PTR_NEW%NEXT_VALUE=>HEAD
            HEAD=>PTR_NEW





          ELSEIF(PTR_NEW%VALUE>=TAIL%VALUE)THEN
            TAIL%NEXT_VALUE=>PTR_NEW  
                                      
                                      
            TAIL=>PTR_NEW
            NULLIFY(TAIL%NEXT_VALUE)





          ELSE
            PTR1=>HEAD
            PTR2=>PTR1%NEXT_VALUE

            search: DO
              IF((PTR_NEW%VALUE>=PTR1%VALUE).AND.                      &
     &           (PTR_NEW%VALUE<PTR2%VALUE))THEN
                PTR_NEW%NEXT_VALUE=>PTR2
                PTR1%NEXT_VALUE=>PTR_NEW
                EXIT search
              ENDIF

              PTR1=>PTR2
              PTR2=>PTR2%NEXT_VALUE
            ENDDO search

          ENDIF check

        ENDIF main

      ENDDO pe_loop





      PTR1=>HEAD

      DO N=1,NPES

        DATA_SORTED(N)=PTR1%VALUE
        IPE_SORTED(N)=PTR1%IPE
        PTR1=>PTR1%NEXT_VALUE
      ENDDO

      DEALLOCATE(PTR_NEW)
      NULLIFY (HEAD,TAIL,PTR1,PTR2)

      END SUBROUTINE SORT



      SUBROUTINE FIELD_STATS(FIELD,MYPE,MPI_COMM_COMP                  &
     &                      ,IDS,IDE,JDS,JDE,KDS,KDE                   &
     &                      ,IMS,IME,JMS,JME,KMS,KME                   &
     &                      ,ITS,ITE,JTS,JTE,KTS,KTE)





      IMPLICIT NONE

      INCLUDE "mpif.h"


      INTEGER,INTENT(IN) :: MPI_COMM_COMP,MYPE
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE

      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(IN) :: FIELD





      INTEGER,PARAMETER :: DOUBLE=SELECTED_REAL_KIND(15,300)

      INTEGER :: I,IEND,IRTN,I_BY_J,J,K,KFLIP

      REAL :: FIKJ,FMAXK,FMINK
      REAL(KIND=DOUBLE) :: F_MEAN,POINTS,RMS,ST_DEV,SUMFK,SUMF2K
      REAL,DIMENSION(KTS:KTE) :: FMAX,FMAX_0,FMIN,FMIN_0
      REAL(KIND=DOUBLE),DIMENSION(KTS:KTE) :: SUMF,SUMF_0,SUMF2,SUMF2_0
 
      CHARACTER(LEN=256) :: message


      I_BY_J=(IDE-IDS)*(JDE-JDS)-(JDE-JDS-1)/2  
                                                
                                                
                                                

      layer_loop:  DO K=KTS,KTE

        FMAXK=-1.E10
        FMINK=1.E10
        SUMFK=0.
        SUMF2K=0.

        DO J=JTS,JTE
          IEND=MIN(ITE,IDE-1)
          IF(MOD(J,2)==0.AND.ITE==IDE-1)IEND=IEND-1
          DO I=ITS,IEND
            FIKJ=FIELD(I,J,K)
            FMAXK=MAX(FMAXK,FIKJ)
            FMINK=MIN(FMINK,FIKJ)
            SUMFK=SUMFK+FIKJ
            SUMF2K=SUMF2K+FIKJ*FIKJ
          ENDDO
        ENDDO

        FMAX(K)=FMAXK
        FMIN(K)=FMINK
        SUMF(K)=SUMFK
        SUMF2(K)=SUMF2K

      ENDDO layer_loop





      CALL MPI_REDUCE(SUMF,SUMF_0,KTE,MPI_REAL8,MPI_SUM,0              &
     &               ,MPI_COMM_COMP,IRTN)
      CALL MPI_REDUCE(SUMF2,SUMF2_0,KTE,MPI_REAL8,MPI_SUM,0            &
     &               ,MPI_COMM_COMP,IRTN)
      CALL MPI_REDUCE(FMAX,FMAX_0,KTE,MPI_REAL,MPI_MAX,0               &
     &               ,MPI_COMM_COMP,IRTN)
      CALL MPI_REDUCE(FMIN,FMIN_0,KTE,MPI_REAL,MPI_MIN,0               &
     &               ,MPI_COMM_COMP,IRTN)

      IF(MYPE==0)THEN
        POINTS=I_BY_J
        DO K=KTE,KTS,-1
          F_MEAN=SUMF_0(K)/POINTS
          ST_DEV=SQRT((POINTS*SUMF2_0(K)-SUMF_0(K)*SUMF_0(K))/         &
     &                (POINTS*(POINTS-1)))
          RMS=SQRT(SUMF2_0(K)/POINTS)
          KFLIP=KTE-K+1
          WRITE(message,101)KFLIP,FMAX_0(K),FMIN_0(K)
          CALL wrf_message(trim(message))
          WRITE(message,102)F_MEAN,ST_DEV,RMS
          CALL wrf_message(trim(message))
  101     FORMAT(' LAYER=',I2,' MAX=',E13.6,' MIN=',E13.6)
  102     FORMAT(9X,' MEAN=',E13.6,' STDEV=',E13.6,' RMS=',E13.6)
        ENDDO
      ENDIF

      END SUBROUTINE FIELD_STATS

      FUNCTION TIMEF()
      REAL*8 TIMEF
      INTEGER :: IC,IR
      CALL SYSTEM_CLOCK(COUNT=IC,COUNT_RATE=IR)
      TIMEF=REAL(IC)/REAL(IR)*1000.0
      END

      SUBROUTINE calc_uphlcy(U,V,W,Z,maxhlcy,DX,DY                                            &
                          ,IDS,IDF,JDS,JDF,KDS,KDE                                            &
                          ,IMS,IME,JMS,JME,KMS,KME                                            &
                          ,ITS,ITE,JTS,JTE,KTS,KTE                                            )

      REAL, PARAMETER:: HLOWER=2000.
      REAL, PARAMETER:: HUPPER=5000.

      REAL :: U(IMS:IME,JMS:JME,KMS:KME)
      REAL :: V(IMS:IME,JMS:JME,KMS:KME)
      REAL :: W(IMS:IME,JMS:JME,KMS:KME)
      REAL :: Z(IMS:IME,JMS:JME,KMS:KME)
      REAL :: maxhlcy(IMS:IME,JMS:JME)
      REAL :: DX(IMS:IME,JMS:JME), UPDHEL(IMS:IME,JMS:JME)
      INTEGER:: UPINDX(IMS:IME,JMS:JME), IHW(JMS:JME), IHE(JMS:JME)

      DO J=JTS,min(JTE,JDF)
        IHW(J)=-MOD(J,2)
        IHE(J)=IHW(J)+1
      ENDDO




      UPDHEL=0.

      R2DY=1./(2.*DY)

      DO J=max(JTS,2),min(JTE,JDF-1)
      DO I=max(ITS,2),min(ITE,IDF-1)

      R2DX=1./(2.*DX(I,J))

       l_loop: DO L=KTS,KTE-1
        ZMIDLOC=0.5*(Z(I,J,L)+Z(I,J,L+1))





        IF ( (ZMIDLOC - Z(I,J,1)) .ge. HLOWER  .AND. &
            (ZMIDLOC - Z(I,J,1)) .le. HUPPER ) THEN
           DZ=(Z(I,J,L+1)-Z(I,J,L))





           IF (W(I,J,L) .lt. 0) THEN




              UPDHEL(I,J)=0.
              EXIT l_loop

           ENDIF

           DVDX   = (V(I+IHE(J),J,L)-V(I+IHW(J),J,L))*R2DX
           DUDY   = (U(I,J+1,L)-U(I,J-1,L))*R2DY








         UPDHEL(I,J)=UPDHEL(I,J)+(DVDX-DUDY)*W(I,J,L)*DZ


        ENDIF
      ENDDO l_loop

      if (UPDHEL(I,J) .gt. maxhlcy(I,J)) maxhlcy(I,J)=UPDHEL(I,J)

      ENDDO
      ENDDO



      END SUBROUTINE calc_uphlcy

      SUBROUTINE CALMICT(P1D,T1D,Q1D,C1D,FI1D,FR1D,FS1D,CUREFL, &
                       DBZ1, TBPVS_STATE,CN0r_DMRmin, CN0r_DMRmax, RQR_DRmin, RQR_DRmax,CN0R0,MASSI, NX, I, J)

        IMPLICIT NONE

      REAL, PARAMETER :: &
      FMW=18.015,FMD=28.964,EPS=FMW/FMD,ONEPS=1.-EPS,TFRZ=273.15, &
      PQ0=379.90516,A2=17.2693882,A3=273.16,A4=35.86, EPSQ=1.E-12, &
      QCLDmin=1.E-6,  &
      STBOL=1./5.67E-8, DBZmin=-20., abscoef=8.0, abscoefi=5.0, &
      XLAI=4.0, &
      RHmin=1.0E-6, &
      SMALL=1.E-6 

      REAL, PARAMETER :: DMImin=.05e-3, DMImax=1.e-3, &
                         XMImin=1.e6*DMImin, XMImax=1.e6*DMImax
      INTEGER, PARAMETER :: MDImin=XMImin, MDImax=XMImax




       REAL MASSI(MDImin:MDImax)
       INTEGER :: NX
       REAL, INTENT(IN):: TBPVS_STATE(NX)





      REAL, PARAMETER :: DMRmin=.05E-3, DMRmax=1.E-3, DelDMR=1.E-6, &
         XMRmin=1.E6*DMRmin, XMRmax=1.E6*DMRmax
      INTEGER, PARAMETER :: MDRmin=XMRmin, MDRmax=XMRmax

      INTEGER INDEXS, INDEXR, I, J
      REAL, PARAMETER :: Cice=1.634e13

      REAL ::  NLICE, N0r,Ztot,Zrain,Zice,Zconv
      REAL ::  P1D,T1D,Q1D,C1D, &
               FI1D,FR1D,FS1D,CUREFL, &
               QW1,QI1,QR1,QS1, &
               DBZ1

      real :: CN0r_DMRmax, CN0r_DMRmin, RQR_DRmin, RQR_DRmax, CN0R0
      REAL :: FLARGE, FSMALL, WV, ESAT, RD, TC, WC, FPVS_MINE, D608, RHO, &
              QSAT, RRHO, RQR, ZMIN, Fice, Frain, Rimef , XLI, QICE, DRmm, &
              DLI, XSIMASS, XLIMASS, DUM, WVQW, QSIGRD, QLICE, FLIMASS

      REAL, PARAMETER ::                                         &
     &  RHgrd=1.                                                        &
     & ,T_ICE=-40.                                                      &
     & ,NLImax=5.E3                                                     &
     & ,NLImin=1.E3                                                     &
     & ,N0r0=8.E6                                                       &
     & ,N0rmin=1.E4



        RD=287.04
        D608=0.608





      Zmin=10.**(0.1*DBZmin)
          QW1=0.
          QI1=0.
          QR1=0.
          QS1=0.
          DBZ1=DBZmin




          Zrain=0.            
          Zice=0.             
          Zconv=CUREFL   
          IF (C1D .LE. EPSQ) THEN



            GO TO 10
          ELSE
            WC=C1D
          ENDIF






          TC=T1D-TFRZ
          Fice=FI1D
          Frain=FR1D
          IF (TC.LE.T_ICE .OR. Fice.GE.1.) THEN
            QI1=WC
          ELSE IF (Fice .LE. 0.) THEN
            QW1=WC
          ELSE
            QI1=Fice*WC
            QW1=WC-QI1
          ENDIF
          IF (QW1.GT.0. .AND. Frain.GT.0.) THEN
            IF (Frain .GE. 1.) THEN
              QR1=QW1
              QW1=0.
            ELSE
              QR1=Frain*QW1
              QW1=QW1-QR1
            ENDIF
          ENDIF
          WV=Q1D/(1.-Q1D)



          ESAT=1000.*FPVS_MINE(T1D,TBPVS_STATE)

        if (I .eq. 44 .and. J .eq. 247) then
        write(0,*) 'T1D, FPVS_MINE(T1D), ESAT: ', T1D, FPVS_MINE(T1D, TBPVS_STATE), ESAT
        endif

          QSAT=EPS*ESAT/(P1D-ESAT)
          RHO=P1D/(RD*T1D*(1.+D608*Q1D))

        if (I .eq. 44 .and. J .eq. 247) then
        write(0,*) 'QR1, RHO: ', QR1, RHO
        endif

          RRHO=1./RHO
  
  
  
          IF (QR1 .GT. EPSQ) THEN
            RQR=RHO*QR1
        if (I .eq. 44 .and. J .eq. 247) then
        write(0,*) 'RQR, RQR_DRmin, RQR_DRmax: ', RQR, RQR_DRmin, RQR_DRmax
        endif
            IF (RQR .LE. RQR_DRmin) THEN
              N0r=MAX(N0rmin, CN0r_DMRmin*RQR)
              INDEXR=MDRmin
            ELSE IF (RQR .GE. RQR_DRmax) THEN
              N0r=CN0r_DMRmax*RQR
              INDEXR=MDRmax
            ELSE
              N0r=N0r0
              INDEXR=MAX( XMRmin, MIN(CN0r0*RQR**.25, XMRmax) )
            ENDIF
  
  
  
            DRmm=1.e-3*REAL(INDEXR)
            Zrain=0.72*N0r*DRmm*DRmm*DRmm*DRmm*DRmm*DRmm*DRmm
        if (I .eq. 44 .and. J .eq. 247) then
        write(0,*) 'DRmm, N0r, Zrain: ', DRmm, N0R, Zrain
        endif

          ENDIF        




          IF (QI1 .GT. EPSQ) THEN
            QICE=QI1
            RHO=P1D/(RD*T1D*(1.+ONEPS*Q1D))
            RRHO=1./RHO
            QSIgrd=RHgrd*QSAT
            WVQW=WV+QW1














            IF (TC.GE.0. .OR. WVQW.LT.QSIgrd) THEN
              FLARGE=1.
            ELSE
              FLARGE=.2
              IF (TC.GE.-8. .AND. TC.LE.-3.) FLARGE=.5*FLARGE
            ENDIF
            FSMALL=(1.-FLARGE)/FLARGE
            XSIMASS=RRHO*MASSI(MDImin)*FSMALL
            DUM=XMImax*EXP(.0536*TC)
            INDEXS=MIN(MDImax, MAX(MDImin, INT(DUM) ) )
            RimeF=AMAX1(1., FS1D )
            XLIMASS=RRHO*RimeF*MASSI(INDEXS)
            FLIMASS=XLIMASS/(XLIMASS+XSIMASS)
            QLICE=FLIMASS*QICE
            NLICE=QLICE/XLIMASS
            IF (NLICE.LT.NLImin .OR. NLICE.GT.NLImax) THEN



              DUM=MAX(NLImin, MIN(NLImax, NLICE) )
              XLI=RHO*(QICE/DUM-XSIMASS)/RimeF
              IF (XLI .LE. MASSI(MDImin) ) THEN
                INDEXS=MDImin
              ELSE IF (XLI .LE. MASSI(450) ) THEN
                DLI=9.5885E5*XLI**.42066         
                INDEXS=MIN(MDImax, MAX(MDImin, INT(DLI) ) )
              ELSE IF (XLI .LE. MASSI(MDImax) ) THEN
                DLI=3.9751E6*XLI**.49870         
                INDEXS=MIN(MDImax, MAX(MDImin, INT(DLI) ) )
              ELSE
                INDEXS=MDImax





                IF (DUM .GE. NLImax) &
                 RimeF=RHO*(QICE/NLImax-XSIMASS)/MASSI(INDEXS)
              ENDIF             
              XLIMASS=RRHO*RimeF*MASSI(INDEXS)
              FLIMASS=XLIMASS/(XLIMASS+XSIMASS)
              QLICE=FLIMASS*QICE
              NLICE=QLICE/XLIMASS
            ENDIF               
            QS1=AMIN1(QI1, QLICE)
            QI1=AMAX1(0., QI1-QS1)
   
   
   
   
   
   
   
   
   
   
            Zice=Cice*RHO*RHO*QLICE*QLICE/NLICE
          ENDIF                 


10        Ztot=Zrain+Zice+Zconv
          IF (Ztot .GT. Zmin)  DBZ1= 10.*ALOG10(Ztot)

        if (DBZ1 .gt. 64.5) then
        write(0,*) 'Zice, Zrain, Zconv, Ztot, DBZ1: ', Zice, Zrain, Zconv, Ztot, DBZ1
        write(0,*) 'Cice, rho, qlice, nlice: ', Cice, rho, qlice, nlice
        endif







      RETURN
      END SUBROUTINE CALMICT


                     REAL   FUNCTION FPVS_MINE(T, TBPVS_STATE)


































      IMPLICIT NONE
        integer, parameter :: NX=7501
      REAL, intent(in) :: TBPVS_STATE(NX)
      real,INTENT(IN) :: T
      real XJ
      integer :: JX

        real, parameter:: XMIN=180.0
        real, parameter:: XMAX=330.0

      REAL, PARAMETER :: XINC=(XMAX-XMIN)/(NX-1)
      REAL, PARAMETER :: C1XPVS=1.-XMIN/XINC
      REAL, PARAMETER :: C2XPVS=1./XINC


      XJ=MIN(MAX(C1XPVS+C2XPVS*T,1.),FLOAT(NX))
      JX=MIN(XJ,NX-1.)
      FPVS_MINE=TBPVS_STATE(JX)+(XJ-JX)*(TBPVS_STATE(JX+1)-TBPVS_STATE(JX))

      END FUNCTION FPVS_MINE




        SUBROUTINE MICRO_INIT(MASSI,MASSR,RQR_DRmin,RQR_DRmax,CN0r0,CN0r_DMRmin,CN0r_DMRmax)


        IMPLICIT NONE

      LOGICAL , EXTERNAL      :: wrf_dm_on_monitor
      LOGICAL :: opened

      REAL ::  C_N0r0,           &
     &  CN0r0, CN0r_DMRmin, CN0r_DMRmax,      &
     &  RQR_DRmin, RQR_DRmax

      REAL, PARAMETER :: DMImin=.05e-3, DMImax=1.e-3, &
       XMImin=1.e6*DMImin, XMImax=1.e6*DMImax

      INTEGER, PARAMETER :: MDImin=XMImin, MDImax=XMImax

      REAL, PARAMETER :: DMRmin=.05E-3, DMRmax=1.E-3, DelDMR=1.E-6, &
        XMRmin=1.E6*DMRmin, XMRmax=1.E6*DMRmax, N0r0=8.E6, N0rmin=1.e4

      INTEGER, PARAMETER :: MDRmin=XMRmin, MDRmax=XMRmax

        REAL, PARAMETER :: RHOL=1000.

       REAL ::  MASSR(MDRmin:MDRmax),MASSI(MDImin:MDImax)
       INTEGER :: I, etampnew_unit1
       REAL :: PI





        IF ( wrf_dm_on_monitor() ) THEN
          DO i = 31,99
            INQUIRE ( i , OPENED = opened )
            IF ( .NOT. opened ) THEN
              etampnew_unit1 = i
              GOTO 2061
            ENDIF
          ENDDO
          etampnew_unit1 = -1
 2061     CONTINUE

      OPEN (UNIT=etampnew_unit1,FILE="ETAMPNEW_DATA",FORM="UNFORMATTED")

        REWIND(etampnew_unit1)
      DO I=1,3
        READ(etampnew_unit1)
      ENDDO
      READ(etampnew_unit1) MASSR
        write(0,*) 'read MASSR ', MASSR(MDRmin), MASSR(MDRmin+400), MASSR(MDRmax)
      DO I=1,5
        READ(etampnew_unit1)
        write(0,*) 'past READ: ', I
      ENDDO
        write(0,*) 'to MASSI read'
        write(0,*) 'size(MASSI): ', size(MASSI)
      READ(etampnew_unit1) MASSI
      CLOSE(etampnew_unit1)
      RQR_DRmin=N0r0*MASSR(MDRmin)    
      RQR_DRmax=N0r0*MASSR(MDRmax)    
      PI=ACOS(-1.)
      C_N0r0=PI*RHOL*N0r0
      CN0r0=1.E6/C_N0r0**.25
      CN0r_DMRmin=1./(PI*RHOL*DMRmin**4)
      CN0r_DMRmax=1./(PI*RHOL*DMRmax**4)
        ENDIF

        CALL wrf_dm_bcast_bytes ( MASSR , size ( MASSR ) * 4 )
        CALL wrf_dm_bcast_bytes ( MASSI , size ( MASSI ) * 4 )
        CALL wrf_dm_bcast_bytes ( RQR_DRmin, 4 )
        CALL wrf_dm_bcast_bytes ( RQR_DRmax, 4 )
        CALL wrf_dm_bcast_bytes ( CN0r0, 4 )
        CALL wrf_dm_bcast_bytes ( CN0r_DMRmin, 4 )
        CALL wrf_dm_bcast_bytes ( CN0r_DMRmax, 4 )
      print *,'MICROINIT: MDRmin, MASSR(MDRmin)=',MDRmin,MASSR(MDRmin)
      print *,'MICROINIT: MDRmax, MASSR(MDRmax)=',MDRmax,MASSR(MDRmax)
        print*, 'RQR_DRmin, RQR_DRmax: ', RQR_DRmin, RQR_DRmax

        END SUBROUTINE MICRO_INIT
