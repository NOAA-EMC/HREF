SUBROUTINE shift_domain_nmm ( grid , disp_x, disp_y &

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

                           )
   USE module_domain
   USE module_timing
   USE module_configure
   USE module_dm
   USE module_timing
   IMPLICIT NONE
  
   INTEGER disp_x, disp_y       
   TYPE(domain) , POINTER                     :: grid

  
   INTEGER  :: i, j, ii
   INTEGER  :: px, py       
   INTEGER  :: ids , ide , jds , jde , kds , kde , &
               ims , ime , jms , jme , kms , kme , &
               ips , ipe , jps , jpe , kps , kpe
   TYPE (grid_config_rec_type)  :: config_flags

   LOGICAL :: E_BDY,N_BDY,S_BDY,W_BDY

   CHARACTER(LEN=255) :: message

   
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

!STARTOFREGISTRYGENERATEDINCLUDE 'inc/data_calls.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
!ENDOFREGISTRYGENERATEDINCLUDE

   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

   CALL get_ijk_from_grid (  grid ,                           &
                             ids, ide, jds, jde, kds, kde,    &
                             ims, ime, jms, jme, kms, kme,    &
                             ips, ipe, jps, jpe, kps, kpe     )


   S_BDY=(JPS==JDS)
   N_BDY=(JPE==JDE)
   W_BDY=(IPS==IDS)
   E_BDY=(IPE==IDE)

   write(message,*)' S_BDY,N_BDY,W_BDY,E_BDY ', S_BDY,N_BDY,W_BDY,E_BDY
   CALL wrf_message(trim(message))

   imask_nostag=0

   IF ( disp_x > 0 ) THEN
      IF ( E_BDY ) THEN 
         DO J=jps,min(jde-1,jpe)
         DO I=ips,min(ide-1,ipe-2-mod(j+1,2)) 
            imask_nostag(i,j) = 1
         END DO
         END DO
      ELSE
         DO J=jps,min(jde-1,jpe)
         DO I=ips,min(ide-1,ipe)
            imask_nostag(i,j) = 1
         END DO
         END DO
      END IF
   END IF
   IF ( disp_x < 0 ) THEN
      IF ( W_BDY ) THEN 
         DO J=jps,min(jde-1,jpe)
         DO I=ips+1,min(ide-1,ipe)
            imask_nostag(i,j) = 1
         END DO
         END DO
      ELSE
         DO J=jps,min(jde-1,jpe)
         DO I=ips,min(ide-1,ipe)
            imask_nostag(i,j) = 1
         END DO
         END DO
      END IF
   END IF
   IF ( disp_y > 0 ) THEN
      IF ( N_BDY ) THEN 
         DO J=jps,min(jde-1,jpe-3)
         DO I=ips,min(ide-1,ipe)
            imask_nostag(i,j) = 1
         END DO
         END DO
      ELSE
         DO J=jps,min(jde-1,jpe)
         DO I=ips,min(ide-1,ipe)
            imask_nostag(i,j) = 1
         END DO
         END DO
      END IF
   END IF
   IF ( disp_y < 0 ) THEN
      IF ( S_BDY ) THEN
         DO J=jps+2,min(jde-1,jpe)
         DO I=ips,min(ide-1,ipe)
            imask_nostag(i,j) = 1
         END DO
         END DO
      ELSE
         DO J=jps,min(jde-1,jpe)
         DO I=ips,min(ide-1,ipe)
            imask_nostag(i,j) = 1
         END DO
         END DO
      END IF
   END IF





   px = isign(grid%parent_grid_ratio,disp_x)
   py = isign(grid%parent_grid_ratio,disp_y)

   if (grid%id==2.and.ims<30.and.30<ime.and.jms<44.and.44<jme) then
     write(message,*)' before shift rlwin ', grid%rlwin(30,44),px,py,disp_x,disp_y
     CALL wrf_message(trim(message))
   endif


   do ii = 1,abs(disp_x)

IF ( SIZE(grid%x_1,1)*SIZE(grid%x_1,3) .GT. 1 ) THEN
grid%x_1 (ips:min(ide-1,ipe),:,jms:jme) = grid%x_1 (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%x_2,1)*SIZE(grid%x_2,3) .GT. 1 ) THEN
grid%x_2 (ips:min(ide-1,ipe),:,jms:jme) = grid%x_2 (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%lu_index,1)*SIZE(grid%lu_index,2) .GT. 1 ) THEN
grid%lu_index (ips:min(ide-1,ipe),jms:jme) = grid%lu_index (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%lu_mask,1)*SIZE(grid%lu_mask,2) .GT. 1 ) THEN
grid%lu_mask (ips:min(ide-1,ipe),jms:jme) = grid%lu_mask (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%p_gc,1)*SIZE(grid%p_gc,2) .GT. 1 ) THEN
grid%p_gc (ips:min(ide-1,ipe),jms:jme,:) = grid%p_gc (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%vegcat,1)*SIZE(grid%vegcat,2) .GT. 1 ) THEN
grid%vegcat (ips:min(ide-1,ipe),jms:jme) = grid%vegcat (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilcat,1)*SIZE(grid%soilcat,2) .GT. 1 ) THEN
grid%soilcat (ips:min(ide-1,ipe),jms:jme) = grid%soilcat (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%input_soil_cat,1)*SIZE(grid%input_soil_cat,2) .GT. 1 ) THEN
grid%input_soil_cat (ips:min(ide-1,ipe),jms:jme) = grid%input_soil_cat (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tsk_gc,1)*SIZE(grid%tsk_gc,2) .GT. 1 ) THEN
grid%tsk_gc (ips:min(ide-1,ipe),jms:jme) = grid%tsk_gc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%xice_gc,1)*SIZE(grid%xice_gc,2) .GT. 1 ) THEN
grid%xice_gc (ips:min(ide-1,ipe),jms:jme) = grid%xice_gc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%ght_gc,1)*SIZE(grid%ght_gc,2) .GT. 1 ) THEN
grid%ght_gc (ips:min(ide-1,ipe),jms:jme,:) = grid%ght_gc (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%rh_gc,1)*SIZE(grid%rh_gc,2) .GT. 1 ) THEN
grid%rh_gc (ips:min(ide-1,ipe),jms:jme,:) = grid%rh_gc (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%v_gc,1)*SIZE(grid%v_gc,2) .GT. 1 ) THEN
grid%v_gc (ips:min(ide-1,ipe),jms:jme,:) = grid%v_gc (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%u_gc,1)*SIZE(grid%u_gc,2) .GT. 1 ) THEN
grid%u_gc (ips:min(ide-1,ipe),jms:jme,:) = grid%u_gc (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%t_gc,1)*SIZE(grid%t_gc,2) .GT. 1 ) THEN
grid%t_gc (ips:min(ide-1,ipe),jms:jme,:) = grid%t_gc (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%snoalb,1)*SIZE(grid%snoalb,2) .GT. 1 ) THEN
grid%snoalb (ips:min(ide-1,ipe),jms:jme) = grid%snoalb (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%greenfrac_gc,1)*SIZE(grid%greenfrac_gc,2) .GT. 1 ) THEN
grid%greenfrac_gc (ips:min(ide-1,ipe),jms:jme,:) = grid%greenfrac_gc (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%albedo12m_gc,1)*SIZE(grid%albedo12m_gc,2) .GT. 1 ) THEN
grid%albedo12m_gc (ips:min(ide-1,ipe),jms:jme,:) = grid%albedo12m_gc (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%soilcbot_gc,1)*SIZE(grid%soilcbot_gc,2) .GT. 1 ) THEN
grid%soilcbot_gc (ips:min(ide-1,ipe),jms:jme,:) = grid%soilcbot_gc (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%soilctop_gc,1)*SIZE(grid%soilctop_gc,2) .GT. 1 ) THEN
grid%soilctop_gc (ips:min(ide-1,ipe),jms:jme,:) = grid%soilctop_gc (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%tmn_gc,1)*SIZE(grid%tmn_gc,2) .GT. 1 ) THEN
grid%tmn_gc (ips:min(ide-1,ipe),jms:jme) = grid%tmn_gc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%htv_gc,1)*SIZE(grid%htv_gc,2) .GT. 1 ) THEN
grid%htv_gc (ips:min(ide-1,ipe),jms:jme) = grid%htv_gc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%ht_gc,1)*SIZE(grid%ht_gc,2) .GT. 1 ) THEN
grid%ht_gc (ips:min(ide-1,ipe),jms:jme) = grid%ht_gc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%landusef_gc,1)*SIZE(grid%landusef_gc,2) .GT. 1 ) THEN
grid%landusef_gc (ips:min(ide-1,ipe),jms:jme,:) = grid%landusef_gc (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%vlon_gc,1)*SIZE(grid%vlon_gc,2) .GT. 1 ) THEN
grid%vlon_gc (ips:min(ide-1,ipe),jms:jme) = grid%vlon_gc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%vlat_gc,1)*SIZE(grid%vlat_gc,2) .GT. 1 ) THEN
grid%vlat_gc (ips:min(ide-1,ipe),jms:jme) = grid%vlat_gc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hlon_gc,1)*SIZE(grid%hlon_gc,2) .GT. 1 ) THEN
grid%hlon_gc (ips:min(ide-1,ipe),jms:jme) = grid%hlon_gc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hlat_gc,1)*SIZE(grid%hlat_gc,2) .GT. 1 ) THEN
grid%hlat_gc (ips:min(ide-1,ipe),jms:jme) = grid%hlat_gc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hbm2,1)*SIZE(grid%hbm2,2) .GT. 1 ) THEN
grid%hbm2 (ips:min(ide-1,ipe),jms:jme) = grid%hbm2 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hbm3,1)*SIZE(grid%hbm3,2) .GT. 1 ) THEN
grid%hbm3 (ips:min(ide-1,ipe),jms:jme) = grid%hbm3 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%vbm2,1)*SIZE(grid%vbm2,2) .GT. 1 ) THEN
grid%vbm2 (ips:min(ide-1,ipe),jms:jme) = grid%vbm2 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%vbm3,1)*SIZE(grid%vbm3,2) .GT. 1 ) THEN
grid%vbm3 (ips:min(ide-1,ipe),jms:jme) = grid%vbm3 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sm,1)*SIZE(grid%sm,2) .GT. 1 ) THEN
grid%sm (ips:min(ide-1,ipe),jms:jme) = grid%sm (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sice,1)*SIZE(grid%sice,2) .GT. 1 ) THEN
grid%sice (ips:min(ide-1,ipe),jms:jme) = grid%sice (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%pd,1)*SIZE(grid%pd,2) .GT. 1 ) THEN
grid%pd (ips:min(ide-1,ipe),jms:jme) = grid%pd (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%fis,1)*SIZE(grid%fis,2) .GT. 1 ) THEN
grid%fis (ips:min(ide-1,ipe),jms:jme) = grid%fis (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%res,1)*SIZE(grid%res,2) .GT. 1 ) THEN
grid%res (ips:min(ide-1,ipe),jms:jme) = grid%res (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%t,1)*SIZE(grid%t,2) .GT. 1 ) THEN
grid%t (ips:min(ide-1,ipe),jms:jme,:) = grid%t (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%q,1)*SIZE(grid%q,2) .GT. 1 ) THEN
grid%q (ips:min(ide-1,ipe),jms:jme,:) = grid%q (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%u,1)*SIZE(grid%u,2) .GT. 1 ) THEN
grid%u (ips:min(ide-1,ipe),jms:jme,:) = grid%u (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%v,1)*SIZE(grid%v,2) .GT. 1 ) THEN
grid%v (ips:min(ide-1,ipe),jms:jme,:) = grid%v (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%told,1)*SIZE(grid%told,2) .GT. 1 ) THEN
grid%told (ips:min(ide-1,ipe),jms:jme,:) = grid%told (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%uold,1)*SIZE(grid%uold,2) .GT. 1 ) THEN
grid%uold (ips:min(ide-1,ipe),jms:jme,:) = grid%uold (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%vold,1)*SIZE(grid%vold,2) .GT. 1 ) THEN
grid%vold (ips:min(ide-1,ipe),jms:jme,:) = grid%vold (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%dfi_pd,1)*SIZE(grid%dfi_pd,2) .GT. 1 ) THEN
grid%dfi_pd (ips:min(ide-1,ipe),jms:jme) = grid%dfi_pd (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%dfi_pint,1)*SIZE(grid%dfi_pint,2) .GT. 1 ) THEN
grid%dfi_pint (ips:min(ide-1,ipe),jms:jme,:) = grid%dfi_pint (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%dfi_dwdt,1)*SIZE(grid%dfi_dwdt,2) .GT. 1 ) THEN
grid%dfi_dwdt (ips:min(ide-1,ipe),jms:jme,:) = grid%dfi_dwdt (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%dfi_t,1)*SIZE(grid%dfi_t,2) .GT. 1 ) THEN
grid%dfi_t (ips:min(ide-1,ipe),jms:jme,:) = grid%dfi_t (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%dfi_q,1)*SIZE(grid%dfi_q,2) .GT. 1 ) THEN
grid%dfi_q (ips:min(ide-1,ipe),jms:jme,:) = grid%dfi_q (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%dfi_u,1)*SIZE(grid%dfi_u,2) .GT. 1 ) THEN
grid%dfi_u (ips:min(ide-1,ipe),jms:jme,:) = grid%dfi_u (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%dfi_v,1)*SIZE(grid%dfi_v,2) .GT. 1 ) THEN
grid%dfi_v (ips:min(ide-1,ipe),jms:jme,:) = grid%dfi_v (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%dfi_q2,1)*SIZE(grid%dfi_q2,2) .GT. 1 ) THEN
grid%dfi_q2 (ips:min(ide-1,ipe),jms:jme,:) = grid%dfi_q2 (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%dfi_cwm,1)*SIZE(grid%dfi_cwm,2) .GT. 1 ) THEN
grid%dfi_cwm (ips:min(ide-1,ipe),jms:jme,:) = grid%dfi_cwm (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%dfi_rrw,1)*SIZE(grid%dfi_rrw,2) .GT. 1 ) THEN
grid%dfi_rrw (ips:min(ide-1,ipe),jms:jme,:) = grid%dfi_rrw (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%dfi_stc,1)*SIZE(grid%dfi_stc,3) .GT. 1 ) THEN
grid%dfi_stc (ips:min(ide-1,ipe),:,jms:jme) = grid%dfi_stc (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%dfi_smc,1)*SIZE(grid%dfi_smc,3) .GT. 1 ) THEN
grid%dfi_smc (ips:min(ide-1,ipe),:,jms:jme) = grid%dfi_smc (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%dfi_sh2o,1)*SIZE(grid%dfi_sh2o,3) .GT. 1 ) THEN
grid%dfi_sh2o (ips:min(ide-1,ipe),:,jms:jme) = grid%dfi_sh2o (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%dfi_snow,1)*SIZE(grid%dfi_snow,2) .GT. 1 ) THEN
grid%dfi_snow (ips:min(ide-1,ipe),jms:jme) = grid%dfi_snow (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%dfi_snowh,1)*SIZE(grid%dfi_snowh,2) .GT. 1 ) THEN
grid%dfi_snowh (ips:min(ide-1,ipe),jms:jme) = grid%dfi_snowh (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%dfi_canwat,1)*SIZE(grid%dfi_canwat,2) .GT. 1 ) THEN
grid%dfi_canwat (ips:min(ide-1,ipe),jms:jme) = grid%dfi_canwat (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%dfi_nmm_tsk,1)*SIZE(grid%dfi_nmm_tsk,2) .GT. 1 ) THEN
grid%dfi_nmm_tsk (ips:min(ide-1,ipe),jms:jme) = grid%dfi_nmm_tsk (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%dfi_snowc,1)*SIZE(grid%dfi_snowc,2) .GT. 1 ) THEN
grid%dfi_snowc (ips:min(ide-1,ipe),jms:jme) = grid%dfi_snowc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%dx_nmm,1)*SIZE(grid%dx_nmm,2) .GT. 1 ) THEN
grid%dx_nmm (ips:min(ide-1,ipe),jms:jme) = grid%dx_nmm (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%wpdar,1)*SIZE(grid%wpdar,2) .GT. 1 ) THEN
grid%wpdar (ips:min(ide-1,ipe),jms:jme) = grid%wpdar (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%cpgfu,1)*SIZE(grid%cpgfu,2) .GT. 1 ) THEN
grid%cpgfu (ips:min(ide-1,ipe),jms:jme) = grid%cpgfu (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%curv,1)*SIZE(grid%curv,2) .GT. 1 ) THEN
grid%curv (ips:min(ide-1,ipe),jms:jme) = grid%curv (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%fcp,1)*SIZE(grid%fcp,2) .GT. 1 ) THEN
grid%fcp (ips:min(ide-1,ipe),jms:jme) = grid%fcp (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%fdiv,1)*SIZE(grid%fdiv,2) .GT. 1 ) THEN
grid%fdiv (ips:min(ide-1,ipe),jms:jme) = grid%fdiv (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%f,1)*SIZE(grid%f,2) .GT. 1 ) THEN
grid%f (ips:min(ide-1,ipe),jms:jme) = grid%f (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%fad,1)*SIZE(grid%fad,2) .GT. 1 ) THEN
grid%fad (ips:min(ide-1,ipe),jms:jme) = grid%fad (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%ddmpu,1)*SIZE(grid%ddmpu,2) .GT. 1 ) THEN
grid%ddmpu (ips:min(ide-1,ipe),jms:jme) = grid%ddmpu (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%ddmpv,1)*SIZE(grid%ddmpv,2) .GT. 1 ) THEN
grid%ddmpv (ips:min(ide-1,ipe),jms:jme) = grid%ddmpv (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%adt,1)*SIZE(grid%adt,2) .GT. 1 ) THEN
grid%adt (ips:min(ide-1,ipe),jms:jme) = grid%adt (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%adu,1)*SIZE(grid%adu,2) .GT. 1 ) THEN
grid%adu (ips:min(ide-1,ipe),jms:jme) = grid%adu (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%adv,1)*SIZE(grid%adv,2) .GT. 1 ) THEN
grid%adv (ips:min(ide-1,ipe),jms:jme) = grid%adv (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%pdsl,1)*SIZE(grid%pdsl,2) .GT. 1 ) THEN
grid%pdsl (ips:min(ide-1,ipe),jms:jme) = grid%pdsl (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%pdslo,1)*SIZE(grid%pdslo,2) .GT. 1 ) THEN
grid%pdslo (ips:min(ide-1,ipe),jms:jme) = grid%pdslo (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%psdt,1)*SIZE(grid%psdt,2) .GT. 1 ) THEN
grid%psdt (ips:min(ide-1,ipe),jms:jme) = grid%psdt (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%div,1)*SIZE(grid%div,2) .GT. 1 ) THEN
grid%div (ips:min(ide-1,ipe),jms:jme,:) = grid%div (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%few,1)*SIZE(grid%few,2) .GT. 1 ) THEN
grid%few (ips:min(ide-1,ipe),jms:jme,:) = grid%few (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%fne,1)*SIZE(grid%fne,2) .GT. 1 ) THEN
grid%fne (ips:min(ide-1,ipe),jms:jme,:) = grid%fne (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%fns,1)*SIZE(grid%fns,2) .GT. 1 ) THEN
grid%fns (ips:min(ide-1,ipe),jms:jme,:) = grid%fns (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%fse,1)*SIZE(grid%fse,2) .GT. 1 ) THEN
grid%fse (ips:min(ide-1,ipe),jms:jme,:) = grid%fse (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%omgalf,1)*SIZE(grid%omgalf,2) .GT. 1 ) THEN
grid%omgalf (ips:min(ide-1,ipe),jms:jme,:) = grid%omgalf (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%petdt,1)*SIZE(grid%petdt,2) .GT. 1 ) THEN
grid%petdt (ips:min(ide-1,ipe),jms:jme,:) = grid%petdt (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%rtop,1)*SIZE(grid%rtop,2) .GT. 1 ) THEN
grid%rtop (ips:min(ide-1,ipe),jms:jme,:) = grid%rtop (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%pblh,1)*SIZE(grid%pblh,2) .GT. 1 ) THEN
grid%pblh (ips:min(ide-1,ipe),jms:jme) = grid%pblh (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%lpbl,1)*SIZE(grid%lpbl,2) .GT. 1 ) THEN
grid%lpbl (ips:min(ide-1,ipe),jms:jme) = grid%lpbl (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%mixht,1)*SIZE(grid%mixht,2) .GT. 1 ) THEN
grid%mixht (ips:min(ide-1,ipe),jms:jme) = grid%mixht (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%ustar,1)*SIZE(grid%ustar,2) .GT. 1 ) THEN
grid%ustar (ips:min(ide-1,ipe),jms:jme) = grid%ustar (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%z0,1)*SIZE(grid%z0,2) .GT. 1 ) THEN
grid%z0 (ips:min(ide-1,ipe),jms:jme) = grid%z0 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%z0base,1)*SIZE(grid%z0base,2) .GT. 1 ) THEN
grid%z0base (ips:min(ide-1,ipe),jms:jme) = grid%z0base (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%ths,1)*SIZE(grid%ths,2) .GT. 1 ) THEN
grid%ths (ips:min(ide-1,ipe),jms:jme) = grid%ths (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%mavail,1)*SIZE(grid%mavail,2) .GT. 1 ) THEN
grid%mavail (ips:min(ide-1,ipe),jms:jme) = grid%mavail (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%qsh,1)*SIZE(grid%qsh,2) .GT. 1 ) THEN
grid%qsh (ips:min(ide-1,ipe),jms:jme) = grid%qsh (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%twbs,1)*SIZE(grid%twbs,2) .GT. 1 ) THEN
grid%twbs (ips:min(ide-1,ipe),jms:jme) = grid%twbs (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%qwbs,1)*SIZE(grid%qwbs,2) .GT. 1 ) THEN
grid%qwbs (ips:min(ide-1,ipe),jms:jme) = grid%qwbs (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%taux,1)*SIZE(grid%taux,2) .GT. 1 ) THEN
grid%taux (ips:min(ide-1,ipe),jms:jme) = grid%taux (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tauy,1)*SIZE(grid%tauy,2) .GT. 1 ) THEN
grid%tauy (ips:min(ide-1,ipe),jms:jme) = grid%tauy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%prec,1)*SIZE(grid%prec,2) .GT. 1 ) THEN
grid%prec (ips:min(ide-1,ipe),jms:jme) = grid%prec (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%aprec,1)*SIZE(grid%aprec,2) .GT. 1 ) THEN
grid%aprec (ips:min(ide-1,ipe),jms:jme) = grid%aprec (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%acprec,1)*SIZE(grid%acprec,2) .GT. 1 ) THEN
grid%acprec (ips:min(ide-1,ipe),jms:jme) = grid%acprec (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%cuprec,1)*SIZE(grid%cuprec,2) .GT. 1 ) THEN
grid%cuprec (ips:min(ide-1,ipe),jms:jme) = grid%cuprec (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%lspa,1)*SIZE(grid%lspa,2) .GT. 1 ) THEN
grid%lspa (ips:min(ide-1,ipe),jms:jme) = grid%lspa (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%ddata,1)*SIZE(grid%ddata,2) .GT. 1 ) THEN
grid%ddata (ips:min(ide-1,ipe),jms:jme) = grid%ddata (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%accliq,1)*SIZE(grid%accliq,2) .GT. 1 ) THEN
grid%accliq (ips:min(ide-1,ipe),jms:jme) = grid%accliq (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sno,1)*SIZE(grid%sno,2) .GT. 1 ) THEN
grid%sno (ips:min(ide-1,ipe),jms:jme) = grid%sno (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%si,1)*SIZE(grid%si,2) .GT. 1 ) THEN
grid%si (ips:min(ide-1,ipe),jms:jme) = grid%si (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%cldefi,1)*SIZE(grid%cldefi,2) .GT. 1 ) THEN
grid%cldefi (ips:min(ide-1,ipe),jms:jme) = grid%cldefi (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%deep,1)*SIZE(grid%deep,2) .GT. 1 ) THEN
grid%deep (ips:min(ide-1,ipe),jms:jme) = grid%deep (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rf,1)*SIZE(grid%rf,2) .GT. 1 ) THEN
grid%rf (ips:min(ide-1,ipe),jms:jme) = grid%rf (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%th10,1)*SIZE(grid%th10,2) .GT. 1 ) THEN
grid%th10 (ips:min(ide-1,ipe),jms:jme) = grid%th10 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%q10,1)*SIZE(grid%q10,2) .GT. 1 ) THEN
grid%q10 (ips:min(ide-1,ipe),jms:jme) = grid%q10 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%pshltr,1)*SIZE(grid%pshltr,2) .GT. 1 ) THEN
grid%pshltr (ips:min(ide-1,ipe),jms:jme) = grid%pshltr (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tshltr,1)*SIZE(grid%tshltr,2) .GT. 1 ) THEN
grid%tshltr (ips:min(ide-1,ipe),jms:jme) = grid%tshltr (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%qshltr,1)*SIZE(grid%qshltr,2) .GT. 1 ) THEN
grid%qshltr (ips:min(ide-1,ipe),jms:jme) = grid%qshltr (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%q2,1)*SIZE(grid%q2,2) .GT. 1 ) THEN
grid%q2 (ips:min(ide-1,ipe),jms:jme,:) = grid%q2 (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%t_adj,1)*SIZE(grid%t_adj,2) .GT. 1 ) THEN
grid%t_adj (ips:min(ide-1,ipe),jms:jme,:) = grid%t_adj (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%t_old,1)*SIZE(grid%t_old,2) .GT. 1 ) THEN
grid%t_old (ips:min(ide-1,ipe),jms:jme,:) = grid%t_old (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%zero_3d,1)*SIZE(grid%zero_3d,2) .GT. 1 ) THEN
grid%zero_3d (ips:min(ide-1,ipe),jms:jme,:) = grid%zero_3d (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%w0avg,1)*SIZE(grid%w0avg,3) .GT. 1 ) THEN
grid%w0avg (ips:min(ide-1,ipe),:,jms:jme) = grid%w0avg (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%akhs_out,1)*SIZE(grid%akhs_out,2) .GT. 1 ) THEN
grid%akhs_out (ips:min(ide-1,ipe),jms:jme) = grid%akhs_out (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%akms_out,1)*SIZE(grid%akms_out,2) .GT. 1 ) THEN
grid%akms_out (ips:min(ide-1,ipe),jms:jme) = grid%akms_out (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%albase,1)*SIZE(grid%albase,2) .GT. 1 ) THEN
grid%albase (ips:min(ide-1,ipe),jms:jme) = grid%albase (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%albedo,1)*SIZE(grid%albedo,2) .GT. 1 ) THEN
grid%albedo (ips:min(ide-1,ipe),jms:jme) = grid%albedo (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%cnvbot,1)*SIZE(grid%cnvbot,2) .GT. 1 ) THEN
grid%cnvbot (ips:min(ide-1,ipe),jms:jme) = grid%cnvbot (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%cnvtop,1)*SIZE(grid%cnvtop,2) .GT. 1 ) THEN
grid%cnvtop (ips:min(ide-1,ipe),jms:jme) = grid%cnvtop (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%czen,1)*SIZE(grid%czen,2) .GT. 1 ) THEN
grid%czen (ips:min(ide-1,ipe),jms:jme) = grid%czen (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%czmean,1)*SIZE(grid%czmean,2) .GT. 1 ) THEN
grid%czmean (ips:min(ide-1,ipe),jms:jme) = grid%czmean (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%embck,1)*SIZE(grid%embck,2) .GT. 1 ) THEN
grid%embck (ips:min(ide-1,ipe),jms:jme) = grid%embck (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%epsr,1)*SIZE(grid%epsr,2) .GT. 1 ) THEN
grid%epsr (ips:min(ide-1,ipe),jms:jme) = grid%epsr (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%gffc,1)*SIZE(grid%gffc,2) .GT. 1 ) THEN
grid%gffc (ips:min(ide-1,ipe),jms:jme) = grid%gffc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%glat,1)*SIZE(grid%glat,2) .GT. 1 ) THEN
grid%glat (ips:min(ide-1,ipe),jms:jme) = grid%glat (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%glon,1)*SIZE(grid%glon,2) .GT. 1 ) THEN
grid%glon (ips:min(ide-1,ipe),jms:jme) = grid%glon (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%nmm_tsk,1)*SIZE(grid%nmm_tsk,2) .GT. 1 ) THEN
grid%nmm_tsk (ips:min(ide-1,ipe),jms:jme) = grid%nmm_tsk (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hdac,1)*SIZE(grid%hdac,2) .GT. 1 ) THEN
grid%hdac (ips:min(ide-1,ipe),jms:jme) = grid%hdac (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hdacv,1)*SIZE(grid%hdacv,2) .GT. 1 ) THEN
grid%hdacv (ips:min(ide-1,ipe),jms:jme) = grid%hdacv (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%mxsnal,1)*SIZE(grid%mxsnal,2) .GT. 1 ) THEN
grid%mxsnal (ips:min(ide-1,ipe),jms:jme) = grid%mxsnal (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%radin,1)*SIZE(grid%radin,2) .GT. 1 ) THEN
grid%radin (ips:min(ide-1,ipe),jms:jme) = grid%radin (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%radot,1)*SIZE(grid%radot,2) .GT. 1 ) THEN
grid%radot (ips:min(ide-1,ipe),jms:jme) = grid%radot (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sigt4,1)*SIZE(grid%sigt4,2) .GT. 1 ) THEN
grid%sigt4 (ips:min(ide-1,ipe),jms:jme) = grid%sigt4 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tg,1)*SIZE(grid%tg,2) .GT. 1 ) THEN
grid%tg (ips:min(ide-1,ipe),jms:jme) = grid%tg (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%lvl,1)*SIZE(grid%lvl,2) .GT. 1 ) THEN
grid%lvl (ips:min(ide-1,ipe),jms:jme) = grid%lvl (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%cwm,1)*SIZE(grid%cwm,2) .GT. 1 ) THEN
grid%cwm (ips:min(ide-1,ipe),jms:jme,:) = grid%cwm (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%rrw,1)*SIZE(grid%rrw,2) .GT. 1 ) THEN
grid%rrw (ips:min(ide-1,ipe),jms:jme,:) = grid%rrw (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%f_ice,1)*SIZE(grid%f_ice,3) .GT. 1 ) THEN
grid%f_ice (ips:min(ide-1,ipe),:,jms:jme) = grid%f_ice (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%f_rain,1)*SIZE(grid%f_rain,3) .GT. 1 ) THEN
grid%f_rain (ips:min(ide-1,ipe),:,jms:jme) = grid%f_rain (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%f_rimef,1)*SIZE(grid%f_rimef,3) .GT. 1 ) THEN
grid%f_rimef (ips:min(ide-1,ipe),:,jms:jme) = grid%f_rimef (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%cldfra,1)*SIZE(grid%cldfra,2) .GT. 1 ) THEN
grid%cldfra (ips:min(ide-1,ipe),jms:jme,:) = grid%cldfra (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%sr,1)*SIZE(grid%sr,2) .GT. 1 ) THEN
grid%sr (ips:min(ide-1,ipe),jms:jme) = grid%sr (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%cfrach,1)*SIZE(grid%cfrach,2) .GT. 1 ) THEN
grid%cfrach (ips:min(ide-1,ipe),jms:jme) = grid%cfrach (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%cfracl,1)*SIZE(grid%cfracl,2) .GT. 1 ) THEN
grid%cfracl (ips:min(ide-1,ipe),jms:jme) = grid%cfracl (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%cfracm,1)*SIZE(grid%cfracm,2) .GT. 1 ) THEN
grid%cfracm (ips:min(ide-1,ipe),jms:jme) = grid%cfracm (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%islope,1)*SIZE(grid%islope,2) .GT. 1 ) THEN
grid%islope (ips:min(ide-1,ipe),jms:jme) = grid%islope (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%cmc,1)*SIZE(grid%cmc,2) .GT. 1 ) THEN
grid%cmc (ips:min(ide-1,ipe),jms:jme) = grid%cmc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%grnflx,1)*SIZE(grid%grnflx,2) .GT. 1 ) THEN
grid%grnflx (ips:min(ide-1,ipe),jms:jme) = grid%grnflx (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%pctsno,1)*SIZE(grid%pctsno,2) .GT. 1 ) THEN
grid%pctsno (ips:min(ide-1,ipe),jms:jme) = grid%pctsno (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soiltb,1)*SIZE(grid%soiltb,2) .GT. 1 ) THEN
grid%soiltb (ips:min(ide-1,ipe),jms:jme) = grid%soiltb (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%vegfrc,1)*SIZE(grid%vegfrc,2) .GT. 1 ) THEN
grid%vegfrc (ips:min(ide-1,ipe),jms:jme) = grid%vegfrc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%shdmin,1)*SIZE(grid%shdmin,2) .GT. 1 ) THEN
grid%shdmin (ips:min(ide-1,ipe),jms:jme) = grid%shdmin (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%shdmax,1)*SIZE(grid%shdmax,2) .GT. 1 ) THEN
grid%shdmax (ips:min(ide-1,ipe),jms:jme) = grid%shdmax (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sh2o,1)*SIZE(grid%sh2o,3) .GT. 1 ) THEN
grid%sh2o (ips:min(ide-1,ipe),:,jms:jme) = grid%sh2o (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%smc,1)*SIZE(grid%smc,3) .GT. 1 ) THEN
grid%smc (ips:min(ide-1,ipe),:,jms:jme) = grid%smc (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%stc,1)*SIZE(grid%stc,3) .GT. 1 ) THEN
grid%stc (ips:min(ide-1,ipe),:,jms:jme) = grid%stc (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%hstdv,1)*SIZE(grid%hstdv,2) .GT. 1 ) THEN
grid%hstdv (ips:min(ide-1,ipe),jms:jme) = grid%hstdv (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hcnvx,1)*SIZE(grid%hcnvx,2) .GT. 1 ) THEN
grid%hcnvx (ips:min(ide-1,ipe),jms:jme) = grid%hcnvx (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hasyw,1)*SIZE(grid%hasyw,2) .GT. 1 ) THEN
grid%hasyw (ips:min(ide-1,ipe),jms:jme) = grid%hasyw (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hasys,1)*SIZE(grid%hasys,2) .GT. 1 ) THEN
grid%hasys (ips:min(ide-1,ipe),jms:jme) = grid%hasys (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hasysw,1)*SIZE(grid%hasysw,2) .GT. 1 ) THEN
grid%hasysw (ips:min(ide-1,ipe),jms:jme) = grid%hasysw (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hasynw,1)*SIZE(grid%hasynw,2) .GT. 1 ) THEN
grid%hasynw (ips:min(ide-1,ipe),jms:jme) = grid%hasynw (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hlenw,1)*SIZE(grid%hlenw,2) .GT. 1 ) THEN
grid%hlenw (ips:min(ide-1,ipe),jms:jme) = grid%hlenw (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hlens,1)*SIZE(grid%hlens,2) .GT. 1 ) THEN
grid%hlens (ips:min(ide-1,ipe),jms:jme) = grid%hlens (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hlensw,1)*SIZE(grid%hlensw,2) .GT. 1 ) THEN
grid%hlensw (ips:min(ide-1,ipe),jms:jme) = grid%hlensw (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hlennw,1)*SIZE(grid%hlennw,2) .GT. 1 ) THEN
grid%hlennw (ips:min(ide-1,ipe),jms:jme) = grid%hlennw (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hangl,1)*SIZE(grid%hangl,2) .GT. 1 ) THEN
grid%hangl (ips:min(ide-1,ipe),jms:jme) = grid%hangl (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hanis,1)*SIZE(grid%hanis,2) .GT. 1 ) THEN
grid%hanis (ips:min(ide-1,ipe),jms:jme) = grid%hanis (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hslop,1)*SIZE(grid%hslop,2) .GT. 1 ) THEN
grid%hslop (ips:min(ide-1,ipe),jms:jme) = grid%hslop (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hzmax,1)*SIZE(grid%hzmax,2) .GT. 1 ) THEN
grid%hzmax (ips:min(ide-1,ipe),jms:jme) = grid%hzmax (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%crot,1)*SIZE(grid%crot,2) .GT. 1 ) THEN
grid%crot (ips:min(ide-1,ipe),jms:jme) = grid%crot (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%srot,1)*SIZE(grid%srot,2) .GT. 1 ) THEN
grid%srot (ips:min(ide-1,ipe),jms:jme) = grid%srot (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%ugwdsfc,1)*SIZE(grid%ugwdsfc,2) .GT. 1 ) THEN
grid%ugwdsfc (ips:min(ide-1,ipe),jms:jme) = grid%ugwdsfc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%vgwdsfc,1)*SIZE(grid%vgwdsfc,2) .GT. 1 ) THEN
grid%vgwdsfc (ips:min(ide-1,ipe),jms:jme) = grid%vgwdsfc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%dwdtmn,1)*SIZE(grid%dwdtmn,2) .GT. 1 ) THEN
grid%dwdtmn (ips:min(ide-1,ipe),jms:jme) = grid%dwdtmn (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%dwdtmx,1)*SIZE(grid%dwdtmx,2) .GT. 1 ) THEN
grid%dwdtmx (ips:min(ide-1,ipe),jms:jme) = grid%dwdtmx (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%dwdt,1)*SIZE(grid%dwdt,2) .GT. 1 ) THEN
grid%dwdt (ips:min(ide-1,ipe),jms:jme,:) = grid%dwdt (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%pdwdt,1)*SIZE(grid%pdwdt,2) .GT. 1 ) THEN
grid%pdwdt (ips:min(ide-1,ipe),jms:jme,:) = grid%pdwdt (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%pint,1)*SIZE(grid%pint,2) .GT. 1 ) THEN
grid%pint (ips:min(ide-1,ipe),jms:jme,:) = grid%pint (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%w,1)*SIZE(grid%w,2) .GT. 1 ) THEN
grid%w (ips:min(ide-1,ipe),jms:jme,:) = grid%w (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%z,1)*SIZE(grid%z,2) .GT. 1 ) THEN
grid%z (ips:min(ide-1,ipe),jms:jme,:) = grid%z (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%acfrcv,1)*SIZE(grid%acfrcv,2) .GT. 1 ) THEN
grid%acfrcv (ips:min(ide-1,ipe),jms:jme) = grid%acfrcv (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%acfrst,1)*SIZE(grid%acfrst,2) .GT. 1 ) THEN
grid%acfrst (ips:min(ide-1,ipe),jms:jme) = grid%acfrst (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%ssroff,1)*SIZE(grid%ssroff,2) .GT. 1 ) THEN
grid%ssroff (ips:min(ide-1,ipe),jms:jme) = grid%ssroff (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%bgroff,1)*SIZE(grid%bgroff,2) .GT. 1 ) THEN
grid%bgroff (ips:min(ide-1,ipe),jms:jme) = grid%bgroff (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rlwin,1)*SIZE(grid%rlwin,2) .GT. 1 ) THEN
grid%rlwin (ips:min(ide-1,ipe),jms:jme) = grid%rlwin (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rlwout,1)*SIZE(grid%rlwout,2) .GT. 1 ) THEN
grid%rlwout (ips:min(ide-1,ipe),jms:jme) = grid%rlwout (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rlwtoa,1)*SIZE(grid%rlwtoa,2) .GT. 1 ) THEN
grid%rlwtoa (ips:min(ide-1,ipe),jms:jme) = grid%rlwtoa (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%alwin,1)*SIZE(grid%alwin,2) .GT. 1 ) THEN
grid%alwin (ips:min(ide-1,ipe),jms:jme) = grid%alwin (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%alwout,1)*SIZE(grid%alwout,2) .GT. 1 ) THEN
grid%alwout (ips:min(ide-1,ipe),jms:jme) = grid%alwout (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%alwtoa,1)*SIZE(grid%alwtoa,2) .GT. 1 ) THEN
grid%alwtoa (ips:min(ide-1,ipe),jms:jme) = grid%alwtoa (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rswin,1)*SIZE(grid%rswin,2) .GT. 1 ) THEN
grid%rswin (ips:min(ide-1,ipe),jms:jme) = grid%rswin (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rswinc,1)*SIZE(grid%rswinc,2) .GT. 1 ) THEN
grid%rswinc (ips:min(ide-1,ipe),jms:jme) = grid%rswinc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rswout,1)*SIZE(grid%rswout,2) .GT. 1 ) THEN
grid%rswout (ips:min(ide-1,ipe),jms:jme) = grid%rswout (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rswtoa,1)*SIZE(grid%rswtoa,2) .GT. 1 ) THEN
grid%rswtoa (ips:min(ide-1,ipe),jms:jme) = grid%rswtoa (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%aswin,1)*SIZE(grid%aswin,2) .GT. 1 ) THEN
grid%aswin (ips:min(ide-1,ipe),jms:jme) = grid%aswin (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%aswout,1)*SIZE(grid%aswout,2) .GT. 1 ) THEN
grid%aswout (ips:min(ide-1,ipe),jms:jme) = grid%aswout (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%aswtoa,1)*SIZE(grid%aswtoa,2) .GT. 1 ) THEN
grid%aswtoa (ips:min(ide-1,ipe),jms:jme) = grid%aswtoa (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sfcshx,1)*SIZE(grid%sfcshx,2) .GT. 1 ) THEN
grid%sfcshx (ips:min(ide-1,ipe),jms:jme) = grid%sfcshx (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sfclhx,1)*SIZE(grid%sfclhx,2) .GT. 1 ) THEN
grid%sfclhx (ips:min(ide-1,ipe),jms:jme) = grid%sfclhx (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%subshx,1)*SIZE(grid%subshx,2) .GT. 1 ) THEN
grid%subshx (ips:min(ide-1,ipe),jms:jme) = grid%subshx (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%snopcx,1)*SIZE(grid%snopcx,2) .GT. 1 ) THEN
grid%snopcx (ips:min(ide-1,ipe),jms:jme) = grid%snopcx (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sfcuvx,1)*SIZE(grid%sfcuvx,2) .GT. 1 ) THEN
grid%sfcuvx (ips:min(ide-1,ipe),jms:jme) = grid%sfcuvx (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%potevp,1)*SIZE(grid%potevp,2) .GT. 1 ) THEN
grid%potevp (ips:min(ide-1,ipe),jms:jme) = grid%potevp (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%potflx,1)*SIZE(grid%potflx,2) .GT. 1 ) THEN
grid%potflx (ips:min(ide-1,ipe),jms:jme) = grid%potflx (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tlmin,1)*SIZE(grid%tlmin,2) .GT. 1 ) THEN
grid%tlmin (ips:min(ide-1,ipe),jms:jme) = grid%tlmin (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tlmax,1)*SIZE(grid%tlmax,2) .GT. 1 ) THEN
grid%tlmax (ips:min(ide-1,ipe),jms:jme) = grid%tlmax (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%t02_min,1)*SIZE(grid%t02_min,2) .GT. 1 ) THEN
grid%t02_min (ips:min(ide-1,ipe),jms:jme) = grid%t02_min (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%t02_max,1)*SIZE(grid%t02_max,2) .GT. 1 ) THEN
grid%t02_max (ips:min(ide-1,ipe),jms:jme) = grid%t02_max (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rh02_min,1)*SIZE(grid%rh02_min,2) .GT. 1 ) THEN
grid%rh02_min (ips:min(ide-1,ipe),jms:jme) = grid%rh02_min (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rh02_max,1)*SIZE(grid%rh02_max,2) .GT. 1 ) THEN
grid%rh02_max (ips:min(ide-1,ipe),jms:jme) = grid%rh02_max (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rlwtt,1)*SIZE(grid%rlwtt,2) .GT. 1 ) THEN
grid%rlwtt (ips:min(ide-1,ipe),jms:jme,:) = grid%rlwtt (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%rswtt,1)*SIZE(grid%rswtt,2) .GT. 1 ) THEN
grid%rswtt (ips:min(ide-1,ipe),jms:jme,:) = grid%rswtt (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%tcucn,1)*SIZE(grid%tcucn,2) .GT. 1 ) THEN
grid%tcucn (ips:min(ide-1,ipe),jms:jme,:) = grid%tcucn (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%train,1)*SIZE(grid%train,2) .GT. 1 ) THEN
grid%train (ips:min(ide-1,ipe),jms:jme,:) = grid%train (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%ncfrcv,1)*SIZE(grid%ncfrcv,2) .GT. 1 ) THEN
grid%ncfrcv (ips:min(ide-1,ipe),jms:jme) = grid%ncfrcv (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%ncfrst,1)*SIZE(grid%ncfrst,2) .GT. 1 ) THEN
grid%ncfrst (ips:min(ide-1,ipe),jms:jme) = grid%ncfrst (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%max10mw,1)*SIZE(grid%max10mw,2) .GT. 1 ) THEN
grid%max10mw (ips:min(ide-1,ipe),jms:jme) = grid%max10mw (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%max10u,1)*SIZE(grid%max10u,2) .GT. 1 ) THEN
grid%max10u (ips:min(ide-1,ipe),jms:jme) = grid%max10u (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%max10v,1)*SIZE(grid%max10v,2) .GT. 1 ) THEN
grid%max10v (ips:min(ide-1,ipe),jms:jme) = grid%max10v (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%maxupdr,1)*SIZE(grid%maxupdr,2) .GT. 1 ) THEN
grid%maxupdr (ips:min(ide-1,ipe),jms:jme) = grid%maxupdr (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%maxdndr,1)*SIZE(grid%maxdndr,2) .GT. 1 ) THEN
grid%maxdndr (ips:min(ide-1,ipe),jms:jme) = grid%maxdndr (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%maxhlcy,1)*SIZE(grid%maxhlcy,2) .GT. 1 ) THEN
grid%maxhlcy (ips:min(ide-1,ipe),jms:jme) = grid%maxhlcy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%maxdbz,1)*SIZE(grid%maxdbz,2) .GT. 1 ) THEN
grid%maxdbz (ips:min(ide-1,ipe),jms:jme) = grid%maxdbz (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%iup_h,1)*SIZE(grid%iup_h,2) .GT. 1 ) THEN
grid%iup_h (ips:min(ide-1,ipe),jms:jme) = grid%iup_h (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%iup_v,1)*SIZE(grid%iup_v,2) .GT. 1 ) THEN
grid%iup_v (ips:min(ide-1,ipe),jms:jme) = grid%iup_v (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%iup_adh,1)*SIZE(grid%iup_adh,2) .GT. 1 ) THEN
grid%iup_adh (ips:min(ide-1,ipe),jms:jme) = grid%iup_adh (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%iup_adv,1)*SIZE(grid%iup_adv,2) .GT. 1 ) THEN
grid%iup_adv (ips:min(ide-1,ipe),jms:jme) = grid%iup_adv (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%imask_nostag,1)*SIZE(grid%imask_nostag,2) .GT. 1 ) THEN
grid%imask_nostag (ips:min(ide-1,ipe),jms:jme) = grid%imask_nostag (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%imask_xstag,1)*SIZE(grid%imask_xstag,2) .GT. 1 ) THEN
grid%imask_xstag (ips:min(ide-1,ipe),jms:jme) = grid%imask_xstag (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%imask_ystag,1)*SIZE(grid%imask_ystag,2) .GT. 1 ) THEN
grid%imask_ystag (ips:min(ide-1,ipe),jms:jme) = grid%imask_ystag (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%imask_xystag,1)*SIZE(grid%imask_xystag,2) .GT. 1 ) THEN
grid%imask_xystag (ips:min(ide-1,ipe),jms:jme) = grid%imask_xystag (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sm000007,1)*SIZE(grid%sm000007,2) .GT. 1 ) THEN
grid%sm000007 (ips:min(ide-1,ipe),jms:jme) = grid%sm000007 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sm007028,1)*SIZE(grid%sm007028,2) .GT. 1 ) THEN
grid%sm007028 (ips:min(ide-1,ipe),jms:jme) = grid%sm007028 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sm028100,1)*SIZE(grid%sm028100,2) .GT. 1 ) THEN
grid%sm028100 (ips:min(ide-1,ipe),jms:jme) = grid%sm028100 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sm100255,1)*SIZE(grid%sm100255,2) .GT. 1 ) THEN
grid%sm100255 (ips:min(ide-1,ipe),jms:jme) = grid%sm100255 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%st000007,1)*SIZE(grid%st000007,2) .GT. 1 ) THEN
grid%st000007 (ips:min(ide-1,ipe),jms:jme) = grid%st000007 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%st007028,1)*SIZE(grid%st007028,2) .GT. 1 ) THEN
grid%st007028 (ips:min(ide-1,ipe),jms:jme) = grid%st007028 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%st028100,1)*SIZE(grid%st028100,2) .GT. 1 ) THEN
grid%st028100 (ips:min(ide-1,ipe),jms:jme) = grid%st028100 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%st100255,1)*SIZE(grid%st100255,2) .GT. 1 ) THEN
grid%st100255 (ips:min(ide-1,ipe),jms:jme) = grid%st100255 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sm000010,1)*SIZE(grid%sm000010,2) .GT. 1 ) THEN
grid%sm000010 (ips:min(ide-1,ipe),jms:jme) = grid%sm000010 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sm010040,1)*SIZE(grid%sm010040,2) .GT. 1 ) THEN
grid%sm010040 (ips:min(ide-1,ipe),jms:jme) = grid%sm010040 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sm040100,1)*SIZE(grid%sm040100,2) .GT. 1 ) THEN
grid%sm040100 (ips:min(ide-1,ipe),jms:jme) = grid%sm040100 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sm100200,1)*SIZE(grid%sm100200,2) .GT. 1 ) THEN
grid%sm100200 (ips:min(ide-1,ipe),jms:jme) = grid%sm100200 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sm010200,1)*SIZE(grid%sm010200,2) .GT. 1 ) THEN
grid%sm010200 (ips:min(ide-1,ipe),jms:jme) = grid%sm010200 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilm000,1)*SIZE(grid%soilm000,2) .GT. 1 ) THEN
grid%soilm000 (ips:min(ide-1,ipe),jms:jme) = grid%soilm000 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilm005,1)*SIZE(grid%soilm005,2) .GT. 1 ) THEN
grid%soilm005 (ips:min(ide-1,ipe),jms:jme) = grid%soilm005 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilm020,1)*SIZE(grid%soilm020,2) .GT. 1 ) THEN
grid%soilm020 (ips:min(ide-1,ipe),jms:jme) = grid%soilm020 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilm040,1)*SIZE(grid%soilm040,2) .GT. 1 ) THEN
grid%soilm040 (ips:min(ide-1,ipe),jms:jme) = grid%soilm040 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilm160,1)*SIZE(grid%soilm160,2) .GT. 1 ) THEN
grid%soilm160 (ips:min(ide-1,ipe),jms:jme) = grid%soilm160 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilm300,1)*SIZE(grid%soilm300,2) .GT. 1 ) THEN
grid%soilm300 (ips:min(ide-1,ipe),jms:jme) = grid%soilm300 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sw000010,1)*SIZE(grid%sw000010,2) .GT. 1 ) THEN
grid%sw000010 (ips:min(ide-1,ipe),jms:jme) = grid%sw000010 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sw010040,1)*SIZE(grid%sw010040,2) .GT. 1 ) THEN
grid%sw010040 (ips:min(ide-1,ipe),jms:jme) = grid%sw010040 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sw040100,1)*SIZE(grid%sw040100,2) .GT. 1 ) THEN
grid%sw040100 (ips:min(ide-1,ipe),jms:jme) = grid%sw040100 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sw100200,1)*SIZE(grid%sw100200,2) .GT. 1 ) THEN
grid%sw100200 (ips:min(ide-1,ipe),jms:jme) = grid%sw100200 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sw010200,1)*SIZE(grid%sw010200,2) .GT. 1 ) THEN
grid%sw010200 (ips:min(ide-1,ipe),jms:jme) = grid%sw010200 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilw000,1)*SIZE(grid%soilw000,2) .GT. 1 ) THEN
grid%soilw000 (ips:min(ide-1,ipe),jms:jme) = grid%soilw000 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilw005,1)*SIZE(grid%soilw005,2) .GT. 1 ) THEN
grid%soilw005 (ips:min(ide-1,ipe),jms:jme) = grid%soilw005 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilw020,1)*SIZE(grid%soilw020,2) .GT. 1 ) THEN
grid%soilw020 (ips:min(ide-1,ipe),jms:jme) = grid%soilw020 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilw040,1)*SIZE(grid%soilw040,2) .GT. 1 ) THEN
grid%soilw040 (ips:min(ide-1,ipe),jms:jme) = grid%soilw040 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilw160,1)*SIZE(grid%soilw160,2) .GT. 1 ) THEN
grid%soilw160 (ips:min(ide-1,ipe),jms:jme) = grid%soilw160 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilw300,1)*SIZE(grid%soilw300,2) .GT. 1 ) THEN
grid%soilw300 (ips:min(ide-1,ipe),jms:jme) = grid%soilw300 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%st000010,1)*SIZE(grid%st000010,2) .GT. 1 ) THEN
grid%st000010 (ips:min(ide-1,ipe),jms:jme) = grid%st000010 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%st010040,1)*SIZE(grid%st010040,2) .GT. 1 ) THEN
grid%st010040 (ips:min(ide-1,ipe),jms:jme) = grid%st010040 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%st040100,1)*SIZE(grid%st040100,2) .GT. 1 ) THEN
grid%st040100 (ips:min(ide-1,ipe),jms:jme) = grid%st040100 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%st100200,1)*SIZE(grid%st100200,2) .GT. 1 ) THEN
grid%st100200 (ips:min(ide-1,ipe),jms:jme) = grid%st100200 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%st010200,1)*SIZE(grid%st010200,2) .GT. 1 ) THEN
grid%st010200 (ips:min(ide-1,ipe),jms:jme) = grid%st010200 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilt000,1)*SIZE(grid%soilt000,2) .GT. 1 ) THEN
grid%soilt000 (ips:min(ide-1,ipe),jms:jme) = grid%soilt000 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilt005,1)*SIZE(grid%soilt005,2) .GT. 1 ) THEN
grid%soilt005 (ips:min(ide-1,ipe),jms:jme) = grid%soilt005 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilt020,1)*SIZE(grid%soilt020,2) .GT. 1 ) THEN
grid%soilt020 (ips:min(ide-1,ipe),jms:jme) = grid%soilt020 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilt040,1)*SIZE(grid%soilt040,2) .GT. 1 ) THEN
grid%soilt040 (ips:min(ide-1,ipe),jms:jme) = grid%soilt040 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilt160,1)*SIZE(grid%soilt160,2) .GT. 1 ) THEN
grid%soilt160 (ips:min(ide-1,ipe),jms:jme) = grid%soilt160 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilt300,1)*SIZE(grid%soilt300,2) .GT. 1 ) THEN
grid%soilt300 (ips:min(ide-1,ipe),jms:jme) = grid%soilt300 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%landmask,1)*SIZE(grid%landmask,2) .GT. 1 ) THEN
grid%landmask (ips:min(ide-1,ipe),jms:jme) = grid%landmask (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%topostdv,1)*SIZE(grid%topostdv,2) .GT. 1 ) THEN
grid%topostdv (ips:min(ide-1,ipe),jms:jme) = grid%topostdv (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%toposlpx,1)*SIZE(grid%toposlpx,2) .GT. 1 ) THEN
grid%toposlpx (ips:min(ide-1,ipe),jms:jme) = grid%toposlpx (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%toposlpy,1)*SIZE(grid%toposlpy,2) .GT. 1 ) THEN
grid%toposlpy (ips:min(ide-1,ipe),jms:jme) = grid%toposlpy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%greenmax,1)*SIZE(grid%greenmax,2) .GT. 1 ) THEN
grid%greenmax (ips:min(ide-1,ipe),jms:jme) = grid%greenmax (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%greenmin,1)*SIZE(grid%greenmin,2) .GT. 1 ) THEN
grid%greenmin (ips:min(ide-1,ipe),jms:jme) = grid%greenmin (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%albedomx,1)*SIZE(grid%albedomx,2) .GT. 1 ) THEN
grid%albedomx (ips:min(ide-1,ipe),jms:jme) = grid%albedomx (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%slopecat,1)*SIZE(grid%slopecat,2) .GT. 1 ) THEN
grid%slopecat (ips:min(ide-1,ipe),jms:jme) = grid%slopecat (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%toposoil,1)*SIZE(grid%toposoil,2) .GT. 1 ) THEN
grid%toposoil (ips:min(ide-1,ipe),jms:jme) = grid%toposoil (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%landusef,1)*SIZE(grid%landusef,3) .GT. 1 ) THEN
grid%landusef (ips:min(ide-1,ipe),:,jms:jme) = grid%landusef (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%soilctop,1)*SIZE(grid%soilctop,3) .GT. 1 ) THEN
grid%soilctop (ips:min(ide-1,ipe),:,jms:jme) = grid%soilctop (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%soilcbot,1)*SIZE(grid%soilcbot,3) .GT. 1 ) THEN
grid%soilcbot (ips:min(ide-1,ipe),:,jms:jme) = grid%soilcbot (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
  DO itrace = PARAM_FIRST_SCALAR, num_moist
IF ( SIZE(grid%moist,1)*SIZE(grid%moist,2) .GT. 1 ) THEN
grid%moist (ips:min(ide-1,ipe),jms:jme,:,itrace) = grid%moist (ips+px:min(ide-1,ipe)+px,jms:jme,:,itrace)
ENDIF
  ENDDO
  DO itrace = PARAM_FIRST_SCALAR, num_dfi_moist
IF ( SIZE(grid%dfi_moist,1)*SIZE(grid%dfi_moist,2) .GT. 1 ) THEN
grid%dfi_moist (ips:min(ide-1,ipe),jms:jme,:,itrace) = grid%dfi_moist (ips+px:min(ide-1,ipe)+px,jms:jme,:,itrace)
ENDIF
  ENDDO
  DO itrace = PARAM_FIRST_SCALAR, num_scalar
IF ( SIZE(grid%scalar,1)*SIZE(grid%scalar,2) .GT. 1 ) THEN
grid%scalar (ips:min(ide-1,ipe),jms:jme,:,itrace) = grid%scalar (ips+px:min(ide-1,ipe)+px,jms:jme,:,itrace)
ENDIF
  ENDDO
  DO itrace = PARAM_FIRST_SCALAR, num_dfi_scalar
IF ( SIZE(grid%dfi_scalar,1)*SIZE(grid%dfi_scalar,3) .GT. 1 ) THEN
grid%dfi_scalar (ips:min(ide-1,ipe),:,jms:jme,itrace) = grid%dfi_scalar (ips+px:min(ide-1,ipe)+px,:,jms:jme,itrace)
ENDIF
  ENDDO
  DO itrace = PARAM_FIRST_SCALAR, num_chem
IF ( SIZE(grid%chem,1)*SIZE(grid%chem,3) .GT. 1 ) THEN
grid%chem (ips:min(ide-1,ipe),:,jms:jme,itrace) = grid%chem (ips+px:min(ide-1,ipe)+px,:,jms:jme,itrace)
ENDIF
  ENDDO
IF ( SIZE(grid%smois,1)*SIZE(grid%smois,3) .GT. 1 ) THEN
grid%smois (ips:min(ide-1,ipe),:,jms:jme) = grid%smois (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%tslb,1)*SIZE(grid%tslb,3) .GT. 1 ) THEN
grid%tslb (ips:min(ide-1,ipe),:,jms:jme) = grid%tslb (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%gsw,1)*SIZE(grid%gsw,2) .GT. 1 ) THEN
grid%gsw (ips:min(ide-1,ipe),jms:jme) = grid%gsw (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%xlat,1)*SIZE(grid%xlat,2) .GT. 1 ) THEN
grid%xlat (ips:min(ide-1,ipe),jms:jme) = grid%xlat (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%xlong,1)*SIZE(grid%xlong,2) .GT. 1 ) THEN
grid%xlong (ips:min(ide-1,ipe),jms:jme) = grid%xlong (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%xland,1)*SIZE(grid%xland,2) .GT. 1 ) THEN
grid%xland (ips:min(ide-1,ipe),jms:jme) = grid%xland (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%raincv,1)*SIZE(grid%raincv,2) .GT. 1 ) THEN
grid%raincv (ips:min(ide-1,ipe),jms:jme) = grid%raincv (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%psfc,1)*SIZE(grid%psfc,2) .GT. 1 ) THEN
grid%psfc (ips:min(ide-1,ipe),jms:jme) = grid%psfc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%th2,1)*SIZE(grid%th2,2) .GT. 1 ) THEN
grid%th2 (ips:min(ide-1,ipe),jms:jme) = grid%th2 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%t2,1)*SIZE(grid%t2,2) .GT. 1 ) THEN
grid%t2 (ips:min(ide-1,ipe),jms:jme) = grid%t2 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%u10,1)*SIZE(grid%u10,2) .GT. 1 ) THEN
grid%u10 (ips:min(ide-1,ipe),jms:jme) = grid%u10 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%v10,1)*SIZE(grid%v10,2) .GT. 1 ) THEN
grid%v10 (ips:min(ide-1,ipe),jms:jme) = grid%v10 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%xice,1)*SIZE(grid%xice,2) .GT. 1 ) THEN
grid%xice (ips:min(ide-1,ipe),jms:jme) = grid%xice (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%lai,1)*SIZE(grid%lai,2) .GT. 1 ) THEN
grid%lai (ips:min(ide-1,ipe),jms:jme) = grid%lai (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%smstav,1)*SIZE(grid%smstav,2) .GT. 1 ) THEN
grid%smstav (ips:min(ide-1,ipe),jms:jme) = grid%smstav (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%smstot,1)*SIZE(grid%smstot,2) .GT. 1 ) THEN
grid%smstot (ips:min(ide-1,ipe),jms:jme) = grid%smstot (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sfcrunoff,1)*SIZE(grid%sfcrunoff,2) .GT. 1 ) THEN
grid%sfcrunoff (ips:min(ide-1,ipe),jms:jme) = grid%sfcrunoff (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%udrunoff,1)*SIZE(grid%udrunoff,2) .GT. 1 ) THEN
grid%udrunoff (ips:min(ide-1,ipe),jms:jme) = grid%udrunoff (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%ivgtyp,1)*SIZE(grid%ivgtyp,2) .GT. 1 ) THEN
grid%ivgtyp (ips:min(ide-1,ipe),jms:jme) = grid%ivgtyp (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%isltyp,1)*SIZE(grid%isltyp,2) .GT. 1 ) THEN
grid%isltyp (ips:min(ide-1,ipe),jms:jme) = grid%isltyp (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%vegfra,1)*SIZE(grid%vegfra,2) .GT. 1 ) THEN
grid%vegfra (ips:min(ide-1,ipe),jms:jme) = grid%vegfra (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sfcevp,1)*SIZE(grid%sfcevp,2) .GT. 1 ) THEN
grid%sfcevp (ips:min(ide-1,ipe),jms:jme) = grid%sfcevp (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%grdflx,1)*SIZE(grid%grdflx,2) .GT. 1 ) THEN
grid%grdflx (ips:min(ide-1,ipe),jms:jme) = grid%grdflx (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%albbck,1)*SIZE(grid%albbck,2) .GT. 1 ) THEN
grid%albbck (ips:min(ide-1,ipe),jms:jme) = grid%albbck (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sfcexc,1)*SIZE(grid%sfcexc,2) .GT. 1 ) THEN
grid%sfcexc (ips:min(ide-1,ipe),jms:jme) = grid%sfcexc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%snotime,1)*SIZE(grid%snotime,2) .GT. 1 ) THEN
grid%snotime (ips:min(ide-1,ipe),jms:jme) = grid%snotime (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%acsnow,1)*SIZE(grid%acsnow,2) .GT. 1 ) THEN
grid%acsnow (ips:min(ide-1,ipe),jms:jme) = grid%acsnow (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%acsnom,1)*SIZE(grid%acsnom,2) .GT. 1 ) THEN
grid%acsnom (ips:min(ide-1,ipe),jms:jme) = grid%acsnom (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rmol,1)*SIZE(grid%rmol,2) .GT. 1 ) THEN
grid%rmol (ips:min(ide-1,ipe),jms:jme) = grid%rmol (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%snow,1)*SIZE(grid%snow,2) .GT. 1 ) THEN
grid%snow (ips:min(ide-1,ipe),jms:jme) = grid%snow (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%canwat,1)*SIZE(grid%canwat,2) .GT. 1 ) THEN
grid%canwat (ips:min(ide-1,ipe),jms:jme) = grid%canwat (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sst,1)*SIZE(grid%sst,2) .GT. 1 ) THEN
grid%sst (ips:min(ide-1,ipe),jms:jme) = grid%sst (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%weasd,1)*SIZE(grid%weasd,2) .GT. 1 ) THEN
grid%weasd (ips:min(ide-1,ipe),jms:jme) = grid%weasd (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%znt,1)*SIZE(grid%znt,2) .GT. 1 ) THEN
grid%znt (ips:min(ide-1,ipe),jms:jme) = grid%znt (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%mol,1)*SIZE(grid%mol,2) .GT. 1 ) THEN
grid%mol (ips:min(ide-1,ipe),jms:jme) = grid%mol (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%noahres,1)*SIZE(grid%noahres,2) .GT. 1 ) THEN
grid%noahres (ips:min(ide-1,ipe),jms:jme) = grid%noahres (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tke_myj,1)*SIZE(grid%tke_myj,2) .GT. 1 ) THEN
grid%tke_myj (ips:min(ide-1,ipe),jms:jme,:) = grid%tke_myj (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%el_myj,1)*SIZE(grid%el_myj,3) .GT. 1 ) THEN
grid%el_myj (ips:min(ide-1,ipe),:,jms:jme) = grid%el_myj (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%exch_h,1)*SIZE(grid%exch_h,3) .GT. 1 ) THEN
grid%exch_h (ips:min(ide-1,ipe),:,jms:jme) = grid%exch_h (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%exch_m,1)*SIZE(grid%exch_m,3) .GT. 1 ) THEN
grid%exch_m (ips:min(ide-1,ipe),:,jms:jme) = grid%exch_m (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%thz0,1)*SIZE(grid%thz0,2) .GT. 1 ) THEN
grid%thz0 (ips:min(ide-1,ipe),jms:jme) = grid%thz0 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%qz0,1)*SIZE(grid%qz0,2) .GT. 1 ) THEN
grid%qz0 (ips:min(ide-1,ipe),jms:jme) = grid%qz0 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%uz0,1)*SIZE(grid%uz0,2) .GT. 1 ) THEN
grid%uz0 (ips:min(ide-1,ipe),jms:jme) = grid%uz0 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%vz0,1)*SIZE(grid%vz0,2) .GT. 1 ) THEN
grid%vz0 (ips:min(ide-1,ipe),jms:jme) = grid%vz0 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%flhc,1)*SIZE(grid%flhc,2) .GT. 1 ) THEN
grid%flhc (ips:min(ide-1,ipe),jms:jme) = grid%flhc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%flqc,1)*SIZE(grid%flqc,2) .GT. 1 ) THEN
grid%flqc (ips:min(ide-1,ipe),jms:jme) = grid%flqc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%qsg,1)*SIZE(grid%qsg,2) .GT. 1 ) THEN
grid%qsg (ips:min(ide-1,ipe),jms:jme) = grid%qsg (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%qvg,1)*SIZE(grid%qvg,2) .GT. 1 ) THEN
grid%qvg (ips:min(ide-1,ipe),jms:jme) = grid%qvg (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%qcg,1)*SIZE(grid%qcg,2) .GT. 1 ) THEN
grid%qcg (ips:min(ide-1,ipe),jms:jme) = grid%qcg (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilt1,1)*SIZE(grid%soilt1,2) .GT. 1 ) THEN
grid%soilt1 (ips:min(ide-1,ipe),jms:jme) = grid%soilt1 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tsnav,1)*SIZE(grid%tsnav,2) .GT. 1 ) THEN
grid%tsnav (ips:min(ide-1,ipe),jms:jme) = grid%tsnav (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%psfc_out,1)*SIZE(grid%psfc_out,2) .GT. 1 ) THEN
grid%psfc_out (ips:min(ide-1,ipe),jms:jme) = grid%psfc_out (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%uz0h,1)*SIZE(grid%uz0h,2) .GT. 1 ) THEN
grid%uz0h (ips:min(ide-1,ipe),jms:jme) = grid%uz0h (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%vz0h,1)*SIZE(grid%vz0h,2) .GT. 1 ) THEN
grid%vz0h (ips:min(ide-1,ipe),jms:jme) = grid%vz0h (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%dudt,1)*SIZE(grid%dudt,2) .GT. 1 ) THEN
grid%dudt (ips:min(ide-1,ipe),jms:jme,:) = grid%dudt (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%dvdt,1)*SIZE(grid%dvdt,2) .GT. 1 ) THEN
grid%dvdt (ips:min(ide-1,ipe),jms:jme,:) = grid%dvdt (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%qsfc,1)*SIZE(grid%qsfc,2) .GT. 1 ) THEN
grid%qsfc (ips:min(ide-1,ipe),jms:jme) = grid%qsfc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%akhs,1)*SIZE(grid%akhs,2) .GT. 1 ) THEN
grid%akhs (ips:min(ide-1,ipe),jms:jme) = grid%akhs (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%akms,1)*SIZE(grid%akms,2) .GT. 1 ) THEN
grid%akms (ips:min(ide-1,ipe),jms:jme) = grid%akms (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%htop,1)*SIZE(grid%htop,2) .GT. 1 ) THEN
grid%htop (ips:min(ide-1,ipe),jms:jme) = grid%htop (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hbot,1)*SIZE(grid%hbot,2) .GT. 1 ) THEN
grid%hbot (ips:min(ide-1,ipe),jms:jme) = grid%hbot (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%htopr,1)*SIZE(grid%htopr,2) .GT. 1 ) THEN
grid%htopr (ips:min(ide-1,ipe),jms:jme) = grid%htopr (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hbotr,1)*SIZE(grid%hbotr,2) .GT. 1 ) THEN
grid%hbotr (ips:min(ide-1,ipe),jms:jme) = grid%hbotr (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%htopd,1)*SIZE(grid%htopd,2) .GT. 1 ) THEN
grid%htopd (ips:min(ide-1,ipe),jms:jme) = grid%htopd (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hbotd,1)*SIZE(grid%hbotd,2) .GT. 1 ) THEN
grid%hbotd (ips:min(ide-1,ipe),jms:jme) = grid%hbotd (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%htops,1)*SIZE(grid%htops,2) .GT. 1 ) THEN
grid%htops (ips:min(ide-1,ipe),jms:jme) = grid%htops (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hbots,1)*SIZE(grid%hbots,2) .GT. 1 ) THEN
grid%hbots (ips:min(ide-1,ipe),jms:jme) = grid%hbots (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%cuppt,1)*SIZE(grid%cuppt,2) .GT. 1 ) THEN
grid%cuppt (ips:min(ide-1,ipe),jms:jme) = grid%cuppt (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%cprate,1)*SIZE(grid%cprate,2) .GT. 1 ) THEN
grid%cprate (ips:min(ide-1,ipe),jms:jme) = grid%cprate (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%f_ice_phy,1)*SIZE(grid%f_ice_phy,3) .GT. 1 ) THEN
grid%f_ice_phy (ips:min(ide-1,ipe),:,jms:jme) = grid%f_ice_phy (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%f_rain_phy,1)*SIZE(grid%f_rain_phy,3) .GT. 1 ) THEN
grid%f_rain_phy (ips:min(ide-1,ipe),:,jms:jme) = grid%f_rain_phy (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%f_rimef_phy,1)*SIZE(grid%f_rimef_phy,3) .GT. 1 ) THEN
grid%f_rimef_phy (ips:min(ide-1,ipe),:,jms:jme) = grid%f_rimef_phy (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%mass_flux,1)*SIZE(grid%mass_flux,2) .GT. 1 ) THEN
grid%mass_flux (ips:min(ide-1,ipe),jms:jme) = grid%mass_flux (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%apr_gr,1)*SIZE(grid%apr_gr,2) .GT. 1 ) THEN
grid%apr_gr (ips:min(ide-1,ipe),jms:jme) = grid%apr_gr (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%apr_w,1)*SIZE(grid%apr_w,2) .GT. 1 ) THEN
grid%apr_w (ips:min(ide-1,ipe),jms:jme) = grid%apr_w (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%apr_mc,1)*SIZE(grid%apr_mc,2) .GT. 1 ) THEN
grid%apr_mc (ips:min(ide-1,ipe),jms:jme) = grid%apr_mc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%apr_st,1)*SIZE(grid%apr_st,2) .GT. 1 ) THEN
grid%apr_st (ips:min(ide-1,ipe),jms:jme) = grid%apr_st (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%apr_as,1)*SIZE(grid%apr_as,2) .GT. 1 ) THEN
grid%apr_as (ips:min(ide-1,ipe),jms:jme) = grid%apr_as (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%apr_capma,1)*SIZE(grid%apr_capma,2) .GT. 1 ) THEN
grid%apr_capma (ips:min(ide-1,ipe),jms:jme) = grid%apr_capma (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%apr_capme,1)*SIZE(grid%apr_capme,2) .GT. 1 ) THEN
grid%apr_capme (ips:min(ide-1,ipe),jms:jme) = grid%apr_capme (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%apr_capmi,1)*SIZE(grid%apr_capmi,2) .GT. 1 ) THEN
grid%apr_capmi (ips:min(ide-1,ipe),jms:jme) = grid%apr_capmi (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rthften,1)*SIZE(grid%rthften,3) .GT. 1 ) THEN
grid%rthften (ips:min(ide-1,ipe),:,jms:jme) = grid%rthften (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%rqvften,1)*SIZE(grid%rqvften,3) .GT. 1 ) THEN
grid%rqvften (ips:min(ide-1,ipe),:,jms:jme) = grid%rqvften (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%snowh,1)*SIZE(grid%snowh,2) .GT. 1 ) THEN
grid%snowh (ips:min(ide-1,ipe),jms:jme) = grid%snowh (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rhosn,1)*SIZE(grid%rhosn,2) .GT. 1 ) THEN
grid%rhosn (ips:min(ide-1,ipe),jms:jme) = grid%rhosn (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%smfr3d,1)*SIZE(grid%smfr3d,3) .GT. 1 ) THEN
grid%smfr3d (ips:min(ide-1,ipe),:,jms:jme) = grid%smfr3d (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%keepfr3dflag,1)*SIZE(grid%keepfr3dflag,3) .GT. 1 ) THEN
grid%keepfr3dflag (ips:min(ide-1,ipe),:,jms:jme) = grid%keepfr3dflag (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
!ENDOFREGISTRYGENERATEDINCLUDE
   enddo



   do ii = 1,abs(disp_y)

IF ( SIZE(grid%x_1,1)*SIZE(grid%x_1,3) .GT. 1 ) THEN
grid%x_1 (ims:ime,:,jps:min(jde-1,jpe)) = grid%x_1 (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%x_2,1)*SIZE(grid%x_2,3) .GT. 1 ) THEN
grid%x_2 (ims:ime,:,jps:min(jde-1,jpe)) = grid%x_2 (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%lu_index,1)*SIZE(grid%lu_index,2) .GT. 1 ) THEN
grid%lu_index (ims:ime,jps:min(jde-1,jpe)) = grid%lu_index (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%lu_mask,1)*SIZE(grid%lu_mask,2) .GT. 1 ) THEN
grid%lu_mask (ims:ime,jps:min(jde-1,jpe)) = grid%lu_mask (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%p_gc,1)*SIZE(grid%p_gc,2) .GT. 1 ) THEN
grid%p_gc (ims:ime,jps:min(jde-1,jpe),:) = grid%p_gc (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%vegcat,1)*SIZE(grid%vegcat,2) .GT. 1 ) THEN
grid%vegcat (ims:ime,jps:min(jde-1,jpe)) = grid%vegcat (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soilcat,1)*SIZE(grid%soilcat,2) .GT. 1 ) THEN
grid%soilcat (ims:ime,jps:min(jde-1,jpe)) = grid%soilcat (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%input_soil_cat,1)*SIZE(grid%input_soil_cat,2) .GT. 1 ) THEN
grid%input_soil_cat (ims:ime,jps:min(jde-1,jpe)) = grid%input_soil_cat (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%tsk_gc,1)*SIZE(grid%tsk_gc,2) .GT. 1 ) THEN
grid%tsk_gc (ims:ime,jps:min(jde-1,jpe)) = grid%tsk_gc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%xice_gc,1)*SIZE(grid%xice_gc,2) .GT. 1 ) THEN
grid%xice_gc (ims:ime,jps:min(jde-1,jpe)) = grid%xice_gc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%ght_gc,1)*SIZE(grid%ght_gc,2) .GT. 1 ) THEN
grid%ght_gc (ims:ime,jps:min(jde-1,jpe),:) = grid%ght_gc (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%rh_gc,1)*SIZE(grid%rh_gc,2) .GT. 1 ) THEN
grid%rh_gc (ims:ime,jps:min(jde-1,jpe),:) = grid%rh_gc (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%v_gc,1)*SIZE(grid%v_gc,2) .GT. 1 ) THEN
grid%v_gc (ims:ime,jps:min(jde-1,jpe),:) = grid%v_gc (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%u_gc,1)*SIZE(grid%u_gc,2) .GT. 1 ) THEN
grid%u_gc (ims:ime,jps:min(jde-1,jpe),:) = grid%u_gc (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%t_gc,1)*SIZE(grid%t_gc,2) .GT. 1 ) THEN
grid%t_gc (ims:ime,jps:min(jde-1,jpe),:) = grid%t_gc (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%snoalb,1)*SIZE(grid%snoalb,2) .GT. 1 ) THEN
grid%snoalb (ims:ime,jps:min(jde-1,jpe)) = grid%snoalb (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%greenfrac_gc,1)*SIZE(grid%greenfrac_gc,2) .GT. 1 ) THEN
grid%greenfrac_gc (ims:ime,jps:min(jde-1,jpe),:) = grid%greenfrac_gc (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%albedo12m_gc,1)*SIZE(grid%albedo12m_gc,2) .GT. 1 ) THEN
grid%albedo12m_gc (ims:ime,jps:min(jde-1,jpe),:) = grid%albedo12m_gc (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%soilcbot_gc,1)*SIZE(grid%soilcbot_gc,2) .GT. 1 ) THEN
grid%soilcbot_gc (ims:ime,jps:min(jde-1,jpe),:) = grid%soilcbot_gc (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%soilctop_gc,1)*SIZE(grid%soilctop_gc,2) .GT. 1 ) THEN
grid%soilctop_gc (ims:ime,jps:min(jde-1,jpe),:) = grid%soilctop_gc (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%tmn_gc,1)*SIZE(grid%tmn_gc,2) .GT. 1 ) THEN
grid%tmn_gc (ims:ime,jps:min(jde-1,jpe)) = grid%tmn_gc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%htv_gc,1)*SIZE(grid%htv_gc,2) .GT. 1 ) THEN
grid%htv_gc (ims:ime,jps:min(jde-1,jpe)) = grid%htv_gc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%ht_gc,1)*SIZE(grid%ht_gc,2) .GT. 1 ) THEN
grid%ht_gc (ims:ime,jps:min(jde-1,jpe)) = grid%ht_gc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%landusef_gc,1)*SIZE(grid%landusef_gc,2) .GT. 1 ) THEN
grid%landusef_gc (ims:ime,jps:min(jde-1,jpe),:) = grid%landusef_gc (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%vlon_gc,1)*SIZE(grid%vlon_gc,2) .GT. 1 ) THEN
grid%vlon_gc (ims:ime,jps:min(jde-1,jpe)) = grid%vlon_gc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%vlat_gc,1)*SIZE(grid%vlat_gc,2) .GT. 1 ) THEN
grid%vlat_gc (ims:ime,jps:min(jde-1,jpe)) = grid%vlat_gc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hlon_gc,1)*SIZE(grid%hlon_gc,2) .GT. 1 ) THEN
grid%hlon_gc (ims:ime,jps:min(jde-1,jpe)) = grid%hlon_gc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hlat_gc,1)*SIZE(grid%hlat_gc,2) .GT. 1 ) THEN
grid%hlat_gc (ims:ime,jps:min(jde-1,jpe)) = grid%hlat_gc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hbm2,1)*SIZE(grid%hbm2,2) .GT. 1 ) THEN
grid%hbm2 (ims:ime,jps:min(jde-1,jpe)) = grid%hbm2 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hbm3,1)*SIZE(grid%hbm3,2) .GT. 1 ) THEN
grid%hbm3 (ims:ime,jps:min(jde-1,jpe)) = grid%hbm3 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%vbm2,1)*SIZE(grid%vbm2,2) .GT. 1 ) THEN
grid%vbm2 (ims:ime,jps:min(jde-1,jpe)) = grid%vbm2 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%vbm3,1)*SIZE(grid%vbm3,2) .GT. 1 ) THEN
grid%vbm3 (ims:ime,jps:min(jde-1,jpe)) = grid%vbm3 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sm,1)*SIZE(grid%sm,2) .GT. 1 ) THEN
grid%sm (ims:ime,jps:min(jde-1,jpe)) = grid%sm (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sice,1)*SIZE(grid%sice,2) .GT. 1 ) THEN
grid%sice (ims:ime,jps:min(jde-1,jpe)) = grid%sice (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%pd,1)*SIZE(grid%pd,2) .GT. 1 ) THEN
grid%pd (ims:ime,jps:min(jde-1,jpe)) = grid%pd (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%fis,1)*SIZE(grid%fis,2) .GT. 1 ) THEN
grid%fis (ims:ime,jps:min(jde-1,jpe)) = grid%fis (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%res,1)*SIZE(grid%res,2) .GT. 1 ) THEN
grid%res (ims:ime,jps:min(jde-1,jpe)) = grid%res (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%t,1)*SIZE(grid%t,2) .GT. 1 ) THEN
grid%t (ims:ime,jps:min(jde-1,jpe),:) = grid%t (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%q,1)*SIZE(grid%q,2) .GT. 1 ) THEN
grid%q (ims:ime,jps:min(jde-1,jpe),:) = grid%q (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%u,1)*SIZE(grid%u,2) .GT. 1 ) THEN
grid%u (ims:ime,jps:min(jde-1,jpe),:) = grid%u (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%v,1)*SIZE(grid%v,2) .GT. 1 ) THEN
grid%v (ims:ime,jps:min(jde-1,jpe),:) = grid%v (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%told,1)*SIZE(grid%told,2) .GT. 1 ) THEN
grid%told (ims:ime,jps:min(jde-1,jpe),:) = grid%told (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%uold,1)*SIZE(grid%uold,2) .GT. 1 ) THEN
grid%uold (ims:ime,jps:min(jde-1,jpe),:) = grid%uold (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%vold,1)*SIZE(grid%vold,2) .GT. 1 ) THEN
grid%vold (ims:ime,jps:min(jde-1,jpe),:) = grid%vold (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%dfi_pd,1)*SIZE(grid%dfi_pd,2) .GT. 1 ) THEN
grid%dfi_pd (ims:ime,jps:min(jde-1,jpe)) = grid%dfi_pd (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%dfi_pint,1)*SIZE(grid%dfi_pint,2) .GT. 1 ) THEN
grid%dfi_pint (ims:ime,jps:min(jde-1,jpe),:) = grid%dfi_pint (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%dfi_dwdt,1)*SIZE(grid%dfi_dwdt,2) .GT. 1 ) THEN
grid%dfi_dwdt (ims:ime,jps:min(jde-1,jpe),:) = grid%dfi_dwdt (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%dfi_t,1)*SIZE(grid%dfi_t,2) .GT. 1 ) THEN
grid%dfi_t (ims:ime,jps:min(jde-1,jpe),:) = grid%dfi_t (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%dfi_q,1)*SIZE(grid%dfi_q,2) .GT. 1 ) THEN
grid%dfi_q (ims:ime,jps:min(jde-1,jpe),:) = grid%dfi_q (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%dfi_u,1)*SIZE(grid%dfi_u,2) .GT. 1 ) THEN
grid%dfi_u (ims:ime,jps:min(jde-1,jpe),:) = grid%dfi_u (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%dfi_v,1)*SIZE(grid%dfi_v,2) .GT. 1 ) THEN
grid%dfi_v (ims:ime,jps:min(jde-1,jpe),:) = grid%dfi_v (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%dfi_q2,1)*SIZE(grid%dfi_q2,2) .GT. 1 ) THEN
grid%dfi_q2 (ims:ime,jps:min(jde-1,jpe),:) = grid%dfi_q2 (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%dfi_cwm,1)*SIZE(grid%dfi_cwm,2) .GT. 1 ) THEN
grid%dfi_cwm (ims:ime,jps:min(jde-1,jpe),:) = grid%dfi_cwm (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%dfi_rrw,1)*SIZE(grid%dfi_rrw,2) .GT. 1 ) THEN
grid%dfi_rrw (ims:ime,jps:min(jde-1,jpe),:) = grid%dfi_rrw (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%dfi_stc,1)*SIZE(grid%dfi_stc,3) .GT. 1 ) THEN
grid%dfi_stc (ims:ime,:,jps:min(jde-1,jpe)) = grid%dfi_stc (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%dfi_smc,1)*SIZE(grid%dfi_smc,3) .GT. 1 ) THEN
grid%dfi_smc (ims:ime,:,jps:min(jde-1,jpe)) = grid%dfi_smc (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%dfi_sh2o,1)*SIZE(grid%dfi_sh2o,3) .GT. 1 ) THEN
grid%dfi_sh2o (ims:ime,:,jps:min(jde-1,jpe)) = grid%dfi_sh2o (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%dfi_snow,1)*SIZE(grid%dfi_snow,2) .GT. 1 ) THEN
grid%dfi_snow (ims:ime,jps:min(jde-1,jpe)) = grid%dfi_snow (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%dfi_snowh,1)*SIZE(grid%dfi_snowh,2) .GT. 1 ) THEN
grid%dfi_snowh (ims:ime,jps:min(jde-1,jpe)) = grid%dfi_snowh (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%dfi_canwat,1)*SIZE(grid%dfi_canwat,2) .GT. 1 ) THEN
grid%dfi_canwat (ims:ime,jps:min(jde-1,jpe)) = grid%dfi_canwat (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%dfi_nmm_tsk,1)*SIZE(grid%dfi_nmm_tsk,2) .GT. 1 ) THEN
grid%dfi_nmm_tsk (ims:ime,jps:min(jde-1,jpe)) = grid%dfi_nmm_tsk (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%dfi_snowc,1)*SIZE(grid%dfi_snowc,2) .GT. 1 ) THEN
grid%dfi_snowc (ims:ime,jps:min(jde-1,jpe)) = grid%dfi_snowc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%dx_nmm,1)*SIZE(grid%dx_nmm,2) .GT. 1 ) THEN
grid%dx_nmm (ims:ime,jps:min(jde-1,jpe)) = grid%dx_nmm (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%wpdar,1)*SIZE(grid%wpdar,2) .GT. 1 ) THEN
grid%wpdar (ims:ime,jps:min(jde-1,jpe)) = grid%wpdar (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%cpgfu,1)*SIZE(grid%cpgfu,2) .GT. 1 ) THEN
grid%cpgfu (ims:ime,jps:min(jde-1,jpe)) = grid%cpgfu (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%curv,1)*SIZE(grid%curv,2) .GT. 1 ) THEN
grid%curv (ims:ime,jps:min(jde-1,jpe)) = grid%curv (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%fcp,1)*SIZE(grid%fcp,2) .GT. 1 ) THEN
grid%fcp (ims:ime,jps:min(jde-1,jpe)) = grid%fcp (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%fdiv,1)*SIZE(grid%fdiv,2) .GT. 1 ) THEN
grid%fdiv (ims:ime,jps:min(jde-1,jpe)) = grid%fdiv (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%f,1)*SIZE(grid%f,2) .GT. 1 ) THEN
grid%f (ims:ime,jps:min(jde-1,jpe)) = grid%f (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%fad,1)*SIZE(grid%fad,2) .GT. 1 ) THEN
grid%fad (ims:ime,jps:min(jde-1,jpe)) = grid%fad (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%ddmpu,1)*SIZE(grid%ddmpu,2) .GT. 1 ) THEN
grid%ddmpu (ims:ime,jps:min(jde-1,jpe)) = grid%ddmpu (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%ddmpv,1)*SIZE(grid%ddmpv,2) .GT. 1 ) THEN
grid%ddmpv (ims:ime,jps:min(jde-1,jpe)) = grid%ddmpv (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%adt,1)*SIZE(grid%adt,2) .GT. 1 ) THEN
grid%adt (ims:ime,jps:min(jde-1,jpe)) = grid%adt (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%adu,1)*SIZE(grid%adu,2) .GT. 1 ) THEN
grid%adu (ims:ime,jps:min(jde-1,jpe)) = grid%adu (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%adv,1)*SIZE(grid%adv,2) .GT. 1 ) THEN
grid%adv (ims:ime,jps:min(jde-1,jpe)) = grid%adv (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%pdsl,1)*SIZE(grid%pdsl,2) .GT. 1 ) THEN
grid%pdsl (ims:ime,jps:min(jde-1,jpe)) = grid%pdsl (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%pdslo,1)*SIZE(grid%pdslo,2) .GT. 1 ) THEN
grid%pdslo (ims:ime,jps:min(jde-1,jpe)) = grid%pdslo (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%psdt,1)*SIZE(grid%psdt,2) .GT. 1 ) THEN
grid%psdt (ims:ime,jps:min(jde-1,jpe)) = grid%psdt (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%div,1)*SIZE(grid%div,2) .GT. 1 ) THEN
grid%div (ims:ime,jps:min(jde-1,jpe),:) = grid%div (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%few,1)*SIZE(grid%few,2) .GT. 1 ) THEN
grid%few (ims:ime,jps:min(jde-1,jpe),:) = grid%few (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%fne,1)*SIZE(grid%fne,2) .GT. 1 ) THEN
grid%fne (ims:ime,jps:min(jde-1,jpe),:) = grid%fne (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%fns,1)*SIZE(grid%fns,2) .GT. 1 ) THEN
grid%fns (ims:ime,jps:min(jde-1,jpe),:) = grid%fns (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%fse,1)*SIZE(grid%fse,2) .GT. 1 ) THEN
grid%fse (ims:ime,jps:min(jde-1,jpe),:) = grid%fse (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%omgalf,1)*SIZE(grid%omgalf,2) .GT. 1 ) THEN
grid%omgalf (ims:ime,jps:min(jde-1,jpe),:) = grid%omgalf (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%petdt,1)*SIZE(grid%petdt,2) .GT. 1 ) THEN
grid%petdt (ims:ime,jps:min(jde-1,jpe),:) = grid%petdt (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%rtop,1)*SIZE(grid%rtop,2) .GT. 1 ) THEN
grid%rtop (ims:ime,jps:min(jde-1,jpe),:) = grid%rtop (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%pblh,1)*SIZE(grid%pblh,2) .GT. 1 ) THEN
grid%pblh (ims:ime,jps:min(jde-1,jpe)) = grid%pblh (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%lpbl,1)*SIZE(grid%lpbl,2) .GT. 1 ) THEN
grid%lpbl (ims:ime,jps:min(jde-1,jpe)) = grid%lpbl (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%mixht,1)*SIZE(grid%mixht,2) .GT. 1 ) THEN
grid%mixht (ims:ime,jps:min(jde-1,jpe)) = grid%mixht (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%ustar,1)*SIZE(grid%ustar,2) .GT. 1 ) THEN
grid%ustar (ims:ime,jps:min(jde-1,jpe)) = grid%ustar (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%z0,1)*SIZE(grid%z0,2) .GT. 1 ) THEN
grid%z0 (ims:ime,jps:min(jde-1,jpe)) = grid%z0 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%z0base,1)*SIZE(grid%z0base,2) .GT. 1 ) THEN
grid%z0base (ims:ime,jps:min(jde-1,jpe)) = grid%z0base (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%ths,1)*SIZE(grid%ths,2) .GT. 1 ) THEN
grid%ths (ims:ime,jps:min(jde-1,jpe)) = grid%ths (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%mavail,1)*SIZE(grid%mavail,2) .GT. 1 ) THEN
grid%mavail (ims:ime,jps:min(jde-1,jpe)) = grid%mavail (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%qsh,1)*SIZE(grid%qsh,2) .GT. 1 ) THEN
grid%qsh (ims:ime,jps:min(jde-1,jpe)) = grid%qsh (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%twbs,1)*SIZE(grid%twbs,2) .GT. 1 ) THEN
grid%twbs (ims:ime,jps:min(jde-1,jpe)) = grid%twbs (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%qwbs,1)*SIZE(grid%qwbs,2) .GT. 1 ) THEN
grid%qwbs (ims:ime,jps:min(jde-1,jpe)) = grid%qwbs (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%taux,1)*SIZE(grid%taux,2) .GT. 1 ) THEN
grid%taux (ims:ime,jps:min(jde-1,jpe)) = grid%taux (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%tauy,1)*SIZE(grid%tauy,2) .GT. 1 ) THEN
grid%tauy (ims:ime,jps:min(jde-1,jpe)) = grid%tauy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%prec,1)*SIZE(grid%prec,2) .GT. 1 ) THEN
grid%prec (ims:ime,jps:min(jde-1,jpe)) = grid%prec (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%aprec,1)*SIZE(grid%aprec,2) .GT. 1 ) THEN
grid%aprec (ims:ime,jps:min(jde-1,jpe)) = grid%aprec (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%acprec,1)*SIZE(grid%acprec,2) .GT. 1 ) THEN
grid%acprec (ims:ime,jps:min(jde-1,jpe)) = grid%acprec (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%cuprec,1)*SIZE(grid%cuprec,2) .GT. 1 ) THEN
grid%cuprec (ims:ime,jps:min(jde-1,jpe)) = grid%cuprec (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%lspa,1)*SIZE(grid%lspa,2) .GT. 1 ) THEN
grid%lspa (ims:ime,jps:min(jde-1,jpe)) = grid%lspa (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%ddata,1)*SIZE(grid%ddata,2) .GT. 1 ) THEN
grid%ddata (ims:ime,jps:min(jde-1,jpe)) = grid%ddata (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%accliq,1)*SIZE(grid%accliq,2) .GT. 1 ) THEN
grid%accliq (ims:ime,jps:min(jde-1,jpe)) = grid%accliq (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sno,1)*SIZE(grid%sno,2) .GT. 1 ) THEN
grid%sno (ims:ime,jps:min(jde-1,jpe)) = grid%sno (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%si,1)*SIZE(grid%si,2) .GT. 1 ) THEN
grid%si (ims:ime,jps:min(jde-1,jpe)) = grid%si (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%cldefi,1)*SIZE(grid%cldefi,2) .GT. 1 ) THEN
grid%cldefi (ims:ime,jps:min(jde-1,jpe)) = grid%cldefi (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%deep,1)*SIZE(grid%deep,2) .GT. 1 ) THEN
grid%deep (ims:ime,jps:min(jde-1,jpe)) = grid%deep (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%rf,1)*SIZE(grid%rf,2) .GT. 1 ) THEN
grid%rf (ims:ime,jps:min(jde-1,jpe)) = grid%rf (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%th10,1)*SIZE(grid%th10,2) .GT. 1 ) THEN
grid%th10 (ims:ime,jps:min(jde-1,jpe)) = grid%th10 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%q10,1)*SIZE(grid%q10,2) .GT. 1 ) THEN
grid%q10 (ims:ime,jps:min(jde-1,jpe)) = grid%q10 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%pshltr,1)*SIZE(grid%pshltr,2) .GT. 1 ) THEN
grid%pshltr (ims:ime,jps:min(jde-1,jpe)) = grid%pshltr (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%tshltr,1)*SIZE(grid%tshltr,2) .GT. 1 ) THEN
grid%tshltr (ims:ime,jps:min(jde-1,jpe)) = grid%tshltr (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%qshltr,1)*SIZE(grid%qshltr,2) .GT. 1 ) THEN
grid%qshltr (ims:ime,jps:min(jde-1,jpe)) = grid%qshltr (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%q2,1)*SIZE(grid%q2,2) .GT. 1 ) THEN
grid%q2 (ims:ime,jps:min(jde-1,jpe),:) = grid%q2 (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%t_adj,1)*SIZE(grid%t_adj,2) .GT. 1 ) THEN
grid%t_adj (ims:ime,jps:min(jde-1,jpe),:) = grid%t_adj (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%t_old,1)*SIZE(grid%t_old,2) .GT. 1 ) THEN
grid%t_old (ims:ime,jps:min(jde-1,jpe),:) = grid%t_old (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%zero_3d,1)*SIZE(grid%zero_3d,2) .GT. 1 ) THEN
grid%zero_3d (ims:ime,jps:min(jde-1,jpe),:) = grid%zero_3d (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%w0avg,1)*SIZE(grid%w0avg,3) .GT. 1 ) THEN
grid%w0avg (ims:ime,:,jps:min(jde-1,jpe)) = grid%w0avg (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%akhs_out,1)*SIZE(grid%akhs_out,2) .GT. 1 ) THEN
grid%akhs_out (ims:ime,jps:min(jde-1,jpe)) = grid%akhs_out (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%akms_out,1)*SIZE(grid%akms_out,2) .GT. 1 ) THEN
grid%akms_out (ims:ime,jps:min(jde-1,jpe)) = grid%akms_out (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%albase,1)*SIZE(grid%albase,2) .GT. 1 ) THEN
grid%albase (ims:ime,jps:min(jde-1,jpe)) = grid%albase (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%albedo,1)*SIZE(grid%albedo,2) .GT. 1 ) THEN
grid%albedo (ims:ime,jps:min(jde-1,jpe)) = grid%albedo (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%cnvbot,1)*SIZE(grid%cnvbot,2) .GT. 1 ) THEN
grid%cnvbot (ims:ime,jps:min(jde-1,jpe)) = grid%cnvbot (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%cnvtop,1)*SIZE(grid%cnvtop,2) .GT. 1 ) THEN
grid%cnvtop (ims:ime,jps:min(jde-1,jpe)) = grid%cnvtop (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%czen,1)*SIZE(grid%czen,2) .GT. 1 ) THEN
grid%czen (ims:ime,jps:min(jde-1,jpe)) = grid%czen (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%czmean,1)*SIZE(grid%czmean,2) .GT. 1 ) THEN
grid%czmean (ims:ime,jps:min(jde-1,jpe)) = grid%czmean (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%embck,1)*SIZE(grid%embck,2) .GT. 1 ) THEN
grid%embck (ims:ime,jps:min(jde-1,jpe)) = grid%embck (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%epsr,1)*SIZE(grid%epsr,2) .GT. 1 ) THEN
grid%epsr (ims:ime,jps:min(jde-1,jpe)) = grid%epsr (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%gffc,1)*SIZE(grid%gffc,2) .GT. 1 ) THEN
grid%gffc (ims:ime,jps:min(jde-1,jpe)) = grid%gffc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%glat,1)*SIZE(grid%glat,2) .GT. 1 ) THEN
grid%glat (ims:ime,jps:min(jde-1,jpe)) = grid%glat (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%glon,1)*SIZE(grid%glon,2) .GT. 1 ) THEN
grid%glon (ims:ime,jps:min(jde-1,jpe)) = grid%glon (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%nmm_tsk,1)*SIZE(grid%nmm_tsk,2) .GT. 1 ) THEN
grid%nmm_tsk (ims:ime,jps:min(jde-1,jpe)) = grid%nmm_tsk (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hdac,1)*SIZE(grid%hdac,2) .GT. 1 ) THEN
grid%hdac (ims:ime,jps:min(jde-1,jpe)) = grid%hdac (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hdacv,1)*SIZE(grid%hdacv,2) .GT. 1 ) THEN
grid%hdacv (ims:ime,jps:min(jde-1,jpe)) = grid%hdacv (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%mxsnal,1)*SIZE(grid%mxsnal,2) .GT. 1 ) THEN
grid%mxsnal (ims:ime,jps:min(jde-1,jpe)) = grid%mxsnal (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%radin,1)*SIZE(grid%radin,2) .GT. 1 ) THEN
grid%radin (ims:ime,jps:min(jde-1,jpe)) = grid%radin (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%radot,1)*SIZE(grid%radot,2) .GT. 1 ) THEN
grid%radot (ims:ime,jps:min(jde-1,jpe)) = grid%radot (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sigt4,1)*SIZE(grid%sigt4,2) .GT. 1 ) THEN
grid%sigt4 (ims:ime,jps:min(jde-1,jpe)) = grid%sigt4 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%tg,1)*SIZE(grid%tg,2) .GT. 1 ) THEN
grid%tg (ims:ime,jps:min(jde-1,jpe)) = grid%tg (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%lvl,1)*SIZE(grid%lvl,2) .GT. 1 ) THEN
grid%lvl (ims:ime,jps:min(jde-1,jpe)) = grid%lvl (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%cwm,1)*SIZE(grid%cwm,2) .GT. 1 ) THEN
grid%cwm (ims:ime,jps:min(jde-1,jpe),:) = grid%cwm (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%rrw,1)*SIZE(grid%rrw,2) .GT. 1 ) THEN
grid%rrw (ims:ime,jps:min(jde-1,jpe),:) = grid%rrw (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%f_ice,1)*SIZE(grid%f_ice,3) .GT. 1 ) THEN
grid%f_ice (ims:ime,:,jps:min(jde-1,jpe)) = grid%f_ice (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%f_rain,1)*SIZE(grid%f_rain,3) .GT. 1 ) THEN
grid%f_rain (ims:ime,:,jps:min(jde-1,jpe)) = grid%f_rain (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%f_rimef,1)*SIZE(grid%f_rimef,3) .GT. 1 ) THEN
grid%f_rimef (ims:ime,:,jps:min(jde-1,jpe)) = grid%f_rimef (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%cldfra,1)*SIZE(grid%cldfra,2) .GT. 1 ) THEN
grid%cldfra (ims:ime,jps:min(jde-1,jpe),:) = grid%cldfra (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%sr,1)*SIZE(grid%sr,2) .GT. 1 ) THEN
grid%sr (ims:ime,jps:min(jde-1,jpe)) = grid%sr (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%cfrach,1)*SIZE(grid%cfrach,2) .GT. 1 ) THEN
grid%cfrach (ims:ime,jps:min(jde-1,jpe)) = grid%cfrach (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%cfracl,1)*SIZE(grid%cfracl,2) .GT. 1 ) THEN
grid%cfracl (ims:ime,jps:min(jde-1,jpe)) = grid%cfracl (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%cfracm,1)*SIZE(grid%cfracm,2) .GT. 1 ) THEN
grid%cfracm (ims:ime,jps:min(jde-1,jpe)) = grid%cfracm (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%islope,1)*SIZE(grid%islope,2) .GT. 1 ) THEN
grid%islope (ims:ime,jps:min(jde-1,jpe)) = grid%islope (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%cmc,1)*SIZE(grid%cmc,2) .GT. 1 ) THEN
grid%cmc (ims:ime,jps:min(jde-1,jpe)) = grid%cmc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%grnflx,1)*SIZE(grid%grnflx,2) .GT. 1 ) THEN
grid%grnflx (ims:ime,jps:min(jde-1,jpe)) = grid%grnflx (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%pctsno,1)*SIZE(grid%pctsno,2) .GT. 1 ) THEN
grid%pctsno (ims:ime,jps:min(jde-1,jpe)) = grid%pctsno (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soiltb,1)*SIZE(grid%soiltb,2) .GT. 1 ) THEN
grid%soiltb (ims:ime,jps:min(jde-1,jpe)) = grid%soiltb (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%vegfrc,1)*SIZE(grid%vegfrc,2) .GT. 1 ) THEN
grid%vegfrc (ims:ime,jps:min(jde-1,jpe)) = grid%vegfrc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%shdmin,1)*SIZE(grid%shdmin,2) .GT. 1 ) THEN
grid%shdmin (ims:ime,jps:min(jde-1,jpe)) = grid%shdmin (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%shdmax,1)*SIZE(grid%shdmax,2) .GT. 1 ) THEN
grid%shdmax (ims:ime,jps:min(jde-1,jpe)) = grid%shdmax (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sh2o,1)*SIZE(grid%sh2o,3) .GT. 1 ) THEN
grid%sh2o (ims:ime,:,jps:min(jde-1,jpe)) = grid%sh2o (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%smc,1)*SIZE(grid%smc,3) .GT. 1 ) THEN
grid%smc (ims:ime,:,jps:min(jde-1,jpe)) = grid%smc (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%stc,1)*SIZE(grid%stc,3) .GT. 1 ) THEN
grid%stc (ims:ime,:,jps:min(jde-1,jpe)) = grid%stc (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hstdv,1)*SIZE(grid%hstdv,2) .GT. 1 ) THEN
grid%hstdv (ims:ime,jps:min(jde-1,jpe)) = grid%hstdv (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hcnvx,1)*SIZE(grid%hcnvx,2) .GT. 1 ) THEN
grid%hcnvx (ims:ime,jps:min(jde-1,jpe)) = grid%hcnvx (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hasyw,1)*SIZE(grid%hasyw,2) .GT. 1 ) THEN
grid%hasyw (ims:ime,jps:min(jde-1,jpe)) = grid%hasyw (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hasys,1)*SIZE(grid%hasys,2) .GT. 1 ) THEN
grid%hasys (ims:ime,jps:min(jde-1,jpe)) = grid%hasys (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hasysw,1)*SIZE(grid%hasysw,2) .GT. 1 ) THEN
grid%hasysw (ims:ime,jps:min(jde-1,jpe)) = grid%hasysw (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hasynw,1)*SIZE(grid%hasynw,2) .GT. 1 ) THEN
grid%hasynw (ims:ime,jps:min(jde-1,jpe)) = grid%hasynw (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hlenw,1)*SIZE(grid%hlenw,2) .GT. 1 ) THEN
grid%hlenw (ims:ime,jps:min(jde-1,jpe)) = grid%hlenw (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hlens,1)*SIZE(grid%hlens,2) .GT. 1 ) THEN
grid%hlens (ims:ime,jps:min(jde-1,jpe)) = grid%hlens (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hlensw,1)*SIZE(grid%hlensw,2) .GT. 1 ) THEN
grid%hlensw (ims:ime,jps:min(jde-1,jpe)) = grid%hlensw (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hlennw,1)*SIZE(grid%hlennw,2) .GT. 1 ) THEN
grid%hlennw (ims:ime,jps:min(jde-1,jpe)) = grid%hlennw (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hangl,1)*SIZE(grid%hangl,2) .GT. 1 ) THEN
grid%hangl (ims:ime,jps:min(jde-1,jpe)) = grid%hangl (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hanis,1)*SIZE(grid%hanis,2) .GT. 1 ) THEN
grid%hanis (ims:ime,jps:min(jde-1,jpe)) = grid%hanis (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hslop,1)*SIZE(grid%hslop,2) .GT. 1 ) THEN
grid%hslop (ims:ime,jps:min(jde-1,jpe)) = grid%hslop (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hzmax,1)*SIZE(grid%hzmax,2) .GT. 1 ) THEN
grid%hzmax (ims:ime,jps:min(jde-1,jpe)) = grid%hzmax (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%crot,1)*SIZE(grid%crot,2) .GT. 1 ) THEN
grid%crot (ims:ime,jps:min(jde-1,jpe)) = grid%crot (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%srot,1)*SIZE(grid%srot,2) .GT. 1 ) THEN
grid%srot (ims:ime,jps:min(jde-1,jpe)) = grid%srot (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%ugwdsfc,1)*SIZE(grid%ugwdsfc,2) .GT. 1 ) THEN
grid%ugwdsfc (ims:ime,jps:min(jde-1,jpe)) = grid%ugwdsfc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%vgwdsfc,1)*SIZE(grid%vgwdsfc,2) .GT. 1 ) THEN
grid%vgwdsfc (ims:ime,jps:min(jde-1,jpe)) = grid%vgwdsfc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%dwdtmn,1)*SIZE(grid%dwdtmn,2) .GT. 1 ) THEN
grid%dwdtmn (ims:ime,jps:min(jde-1,jpe)) = grid%dwdtmn (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%dwdtmx,1)*SIZE(grid%dwdtmx,2) .GT. 1 ) THEN
grid%dwdtmx (ims:ime,jps:min(jde-1,jpe)) = grid%dwdtmx (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%dwdt,1)*SIZE(grid%dwdt,2) .GT. 1 ) THEN
grid%dwdt (ims:ime,jps:min(jde-1,jpe),:) = grid%dwdt (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%pdwdt,1)*SIZE(grid%pdwdt,2) .GT. 1 ) THEN
grid%pdwdt (ims:ime,jps:min(jde-1,jpe),:) = grid%pdwdt (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%pint,1)*SIZE(grid%pint,2) .GT. 1 ) THEN
grid%pint (ims:ime,jps:min(jde-1,jpe),:) = grid%pint (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%w,1)*SIZE(grid%w,2) .GT. 1 ) THEN
grid%w (ims:ime,jps:min(jde-1,jpe),:) = grid%w (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%z,1)*SIZE(grid%z,2) .GT. 1 ) THEN
grid%z (ims:ime,jps:min(jde-1,jpe),:) = grid%z (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%acfrcv,1)*SIZE(grid%acfrcv,2) .GT. 1 ) THEN
grid%acfrcv (ims:ime,jps:min(jde-1,jpe)) = grid%acfrcv (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%acfrst,1)*SIZE(grid%acfrst,2) .GT. 1 ) THEN
grid%acfrst (ims:ime,jps:min(jde-1,jpe)) = grid%acfrst (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%ssroff,1)*SIZE(grid%ssroff,2) .GT. 1 ) THEN
grid%ssroff (ims:ime,jps:min(jde-1,jpe)) = grid%ssroff (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%bgroff,1)*SIZE(grid%bgroff,2) .GT. 1 ) THEN
grid%bgroff (ims:ime,jps:min(jde-1,jpe)) = grid%bgroff (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%rlwin,1)*SIZE(grid%rlwin,2) .GT. 1 ) THEN
grid%rlwin (ims:ime,jps:min(jde-1,jpe)) = grid%rlwin (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%rlwout,1)*SIZE(grid%rlwout,2) .GT. 1 ) THEN
grid%rlwout (ims:ime,jps:min(jde-1,jpe)) = grid%rlwout (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%rlwtoa,1)*SIZE(grid%rlwtoa,2) .GT. 1 ) THEN
grid%rlwtoa (ims:ime,jps:min(jde-1,jpe)) = grid%rlwtoa (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%alwin,1)*SIZE(grid%alwin,2) .GT. 1 ) THEN
grid%alwin (ims:ime,jps:min(jde-1,jpe)) = grid%alwin (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%alwout,1)*SIZE(grid%alwout,2) .GT. 1 ) THEN
grid%alwout (ims:ime,jps:min(jde-1,jpe)) = grid%alwout (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%alwtoa,1)*SIZE(grid%alwtoa,2) .GT. 1 ) THEN
grid%alwtoa (ims:ime,jps:min(jde-1,jpe)) = grid%alwtoa (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%rswin,1)*SIZE(grid%rswin,2) .GT. 1 ) THEN
grid%rswin (ims:ime,jps:min(jde-1,jpe)) = grid%rswin (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%rswinc,1)*SIZE(grid%rswinc,2) .GT. 1 ) THEN
grid%rswinc (ims:ime,jps:min(jde-1,jpe)) = grid%rswinc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%rswout,1)*SIZE(grid%rswout,2) .GT. 1 ) THEN
grid%rswout (ims:ime,jps:min(jde-1,jpe)) = grid%rswout (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%rswtoa,1)*SIZE(grid%rswtoa,2) .GT. 1 ) THEN
grid%rswtoa (ims:ime,jps:min(jde-1,jpe)) = grid%rswtoa (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%aswin,1)*SIZE(grid%aswin,2) .GT. 1 ) THEN
grid%aswin (ims:ime,jps:min(jde-1,jpe)) = grid%aswin (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%aswout,1)*SIZE(grid%aswout,2) .GT. 1 ) THEN
grid%aswout (ims:ime,jps:min(jde-1,jpe)) = grid%aswout (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%aswtoa,1)*SIZE(grid%aswtoa,2) .GT. 1 ) THEN
grid%aswtoa (ims:ime,jps:min(jde-1,jpe)) = grid%aswtoa (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sfcshx,1)*SIZE(grid%sfcshx,2) .GT. 1 ) THEN
grid%sfcshx (ims:ime,jps:min(jde-1,jpe)) = grid%sfcshx (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sfclhx,1)*SIZE(grid%sfclhx,2) .GT. 1 ) THEN
grid%sfclhx (ims:ime,jps:min(jde-1,jpe)) = grid%sfclhx (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%subshx,1)*SIZE(grid%subshx,2) .GT. 1 ) THEN
grid%subshx (ims:ime,jps:min(jde-1,jpe)) = grid%subshx (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%snopcx,1)*SIZE(grid%snopcx,2) .GT. 1 ) THEN
grid%snopcx (ims:ime,jps:min(jde-1,jpe)) = grid%snopcx (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sfcuvx,1)*SIZE(grid%sfcuvx,2) .GT. 1 ) THEN
grid%sfcuvx (ims:ime,jps:min(jde-1,jpe)) = grid%sfcuvx (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%potevp,1)*SIZE(grid%potevp,2) .GT. 1 ) THEN
grid%potevp (ims:ime,jps:min(jde-1,jpe)) = grid%potevp (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%potflx,1)*SIZE(grid%potflx,2) .GT. 1 ) THEN
grid%potflx (ims:ime,jps:min(jde-1,jpe)) = grid%potflx (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%tlmin,1)*SIZE(grid%tlmin,2) .GT. 1 ) THEN
grid%tlmin (ims:ime,jps:min(jde-1,jpe)) = grid%tlmin (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%tlmax,1)*SIZE(grid%tlmax,2) .GT. 1 ) THEN
grid%tlmax (ims:ime,jps:min(jde-1,jpe)) = grid%tlmax (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%t02_min,1)*SIZE(grid%t02_min,2) .GT. 1 ) THEN
grid%t02_min (ims:ime,jps:min(jde-1,jpe)) = grid%t02_min (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%t02_max,1)*SIZE(grid%t02_max,2) .GT. 1 ) THEN
grid%t02_max (ims:ime,jps:min(jde-1,jpe)) = grid%t02_max (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%rh02_min,1)*SIZE(grid%rh02_min,2) .GT. 1 ) THEN
grid%rh02_min (ims:ime,jps:min(jde-1,jpe)) = grid%rh02_min (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%rh02_max,1)*SIZE(grid%rh02_max,2) .GT. 1 ) THEN
grid%rh02_max (ims:ime,jps:min(jde-1,jpe)) = grid%rh02_max (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%rlwtt,1)*SIZE(grid%rlwtt,2) .GT. 1 ) THEN
grid%rlwtt (ims:ime,jps:min(jde-1,jpe),:) = grid%rlwtt (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%rswtt,1)*SIZE(grid%rswtt,2) .GT. 1 ) THEN
grid%rswtt (ims:ime,jps:min(jde-1,jpe),:) = grid%rswtt (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%tcucn,1)*SIZE(grid%tcucn,2) .GT. 1 ) THEN
grid%tcucn (ims:ime,jps:min(jde-1,jpe),:) = grid%tcucn (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%train,1)*SIZE(grid%train,2) .GT. 1 ) THEN
grid%train (ims:ime,jps:min(jde-1,jpe),:) = grid%train (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%ncfrcv,1)*SIZE(grid%ncfrcv,2) .GT. 1 ) THEN
grid%ncfrcv (ims:ime,jps:min(jde-1,jpe)) = grid%ncfrcv (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%ncfrst,1)*SIZE(grid%ncfrst,2) .GT. 1 ) THEN
grid%ncfrst (ims:ime,jps:min(jde-1,jpe)) = grid%ncfrst (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%max10mw,1)*SIZE(grid%max10mw,2) .GT. 1 ) THEN
grid%max10mw (ims:ime,jps:min(jde-1,jpe)) = grid%max10mw (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%max10u,1)*SIZE(grid%max10u,2) .GT. 1 ) THEN
grid%max10u (ims:ime,jps:min(jde-1,jpe)) = grid%max10u (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%max10v,1)*SIZE(grid%max10v,2) .GT. 1 ) THEN
grid%max10v (ims:ime,jps:min(jde-1,jpe)) = grid%max10v (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%maxupdr,1)*SIZE(grid%maxupdr,2) .GT. 1 ) THEN
grid%maxupdr (ims:ime,jps:min(jde-1,jpe)) = grid%maxupdr (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%maxdndr,1)*SIZE(grid%maxdndr,2) .GT. 1 ) THEN
grid%maxdndr (ims:ime,jps:min(jde-1,jpe)) = grid%maxdndr (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%maxhlcy,1)*SIZE(grid%maxhlcy,2) .GT. 1 ) THEN
grid%maxhlcy (ims:ime,jps:min(jde-1,jpe)) = grid%maxhlcy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%maxdbz,1)*SIZE(grid%maxdbz,2) .GT. 1 ) THEN
grid%maxdbz (ims:ime,jps:min(jde-1,jpe)) = grid%maxdbz (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%iup_h,1)*SIZE(grid%iup_h,2) .GT. 1 ) THEN
grid%iup_h (ims:ime,jps:min(jde-1,jpe)) = grid%iup_h (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%iup_v,1)*SIZE(grid%iup_v,2) .GT. 1 ) THEN
grid%iup_v (ims:ime,jps:min(jde-1,jpe)) = grid%iup_v (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%iup_adh,1)*SIZE(grid%iup_adh,2) .GT. 1 ) THEN
grid%iup_adh (ims:ime,jps:min(jde-1,jpe)) = grid%iup_adh (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%iup_adv,1)*SIZE(grid%iup_adv,2) .GT. 1 ) THEN
grid%iup_adv (ims:ime,jps:min(jde-1,jpe)) = grid%iup_adv (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%imask_nostag,1)*SIZE(grid%imask_nostag,2) .GT. 1 ) THEN
grid%imask_nostag (ims:ime,jps:min(jde-1,jpe)) = grid%imask_nostag (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%imask_xstag,1)*SIZE(grid%imask_xstag,2) .GT. 1 ) THEN
grid%imask_xstag (ims:ime,jps:min(jde-1,jpe)) = grid%imask_xstag (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%imask_ystag,1)*SIZE(grid%imask_ystag,2) .GT. 1 ) THEN
grid%imask_ystag (ims:ime,jps:min(jde-1,jpe)) = grid%imask_ystag (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%imask_xystag,1)*SIZE(grid%imask_xystag,2) .GT. 1 ) THEN
grid%imask_xystag (ims:ime,jps:min(jde-1,jpe)) = grid%imask_xystag (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sm000007,1)*SIZE(grid%sm000007,2) .GT. 1 ) THEN
grid%sm000007 (ims:ime,jps:min(jde-1,jpe)) = grid%sm000007 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sm007028,1)*SIZE(grid%sm007028,2) .GT. 1 ) THEN
grid%sm007028 (ims:ime,jps:min(jde-1,jpe)) = grid%sm007028 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sm028100,1)*SIZE(grid%sm028100,2) .GT. 1 ) THEN
grid%sm028100 (ims:ime,jps:min(jde-1,jpe)) = grid%sm028100 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sm100255,1)*SIZE(grid%sm100255,2) .GT. 1 ) THEN
grid%sm100255 (ims:ime,jps:min(jde-1,jpe)) = grid%sm100255 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%st000007,1)*SIZE(grid%st000007,2) .GT. 1 ) THEN
grid%st000007 (ims:ime,jps:min(jde-1,jpe)) = grid%st000007 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%st007028,1)*SIZE(grid%st007028,2) .GT. 1 ) THEN
grid%st007028 (ims:ime,jps:min(jde-1,jpe)) = grid%st007028 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%st028100,1)*SIZE(grid%st028100,2) .GT. 1 ) THEN
grid%st028100 (ims:ime,jps:min(jde-1,jpe)) = grid%st028100 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%st100255,1)*SIZE(grid%st100255,2) .GT. 1 ) THEN
grid%st100255 (ims:ime,jps:min(jde-1,jpe)) = grid%st100255 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sm000010,1)*SIZE(grid%sm000010,2) .GT. 1 ) THEN
grid%sm000010 (ims:ime,jps:min(jde-1,jpe)) = grid%sm000010 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sm010040,1)*SIZE(grid%sm010040,2) .GT. 1 ) THEN
grid%sm010040 (ims:ime,jps:min(jde-1,jpe)) = grid%sm010040 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sm040100,1)*SIZE(grid%sm040100,2) .GT. 1 ) THEN
grid%sm040100 (ims:ime,jps:min(jde-1,jpe)) = grid%sm040100 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sm100200,1)*SIZE(grid%sm100200,2) .GT. 1 ) THEN
grid%sm100200 (ims:ime,jps:min(jde-1,jpe)) = grid%sm100200 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sm010200,1)*SIZE(grid%sm010200,2) .GT. 1 ) THEN
grid%sm010200 (ims:ime,jps:min(jde-1,jpe)) = grid%sm010200 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soilm000,1)*SIZE(grid%soilm000,2) .GT. 1 ) THEN
grid%soilm000 (ims:ime,jps:min(jde-1,jpe)) = grid%soilm000 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soilm005,1)*SIZE(grid%soilm005,2) .GT. 1 ) THEN
grid%soilm005 (ims:ime,jps:min(jde-1,jpe)) = grid%soilm005 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soilm020,1)*SIZE(grid%soilm020,2) .GT. 1 ) THEN
grid%soilm020 (ims:ime,jps:min(jde-1,jpe)) = grid%soilm020 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soilm040,1)*SIZE(grid%soilm040,2) .GT. 1 ) THEN
grid%soilm040 (ims:ime,jps:min(jde-1,jpe)) = grid%soilm040 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soilm160,1)*SIZE(grid%soilm160,2) .GT. 1 ) THEN
grid%soilm160 (ims:ime,jps:min(jde-1,jpe)) = grid%soilm160 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soilm300,1)*SIZE(grid%soilm300,2) .GT. 1 ) THEN
grid%soilm300 (ims:ime,jps:min(jde-1,jpe)) = grid%soilm300 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sw000010,1)*SIZE(grid%sw000010,2) .GT. 1 ) THEN
grid%sw000010 (ims:ime,jps:min(jde-1,jpe)) = grid%sw000010 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sw010040,1)*SIZE(grid%sw010040,2) .GT. 1 ) THEN
grid%sw010040 (ims:ime,jps:min(jde-1,jpe)) = grid%sw010040 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sw040100,1)*SIZE(grid%sw040100,2) .GT. 1 ) THEN
grid%sw040100 (ims:ime,jps:min(jde-1,jpe)) = grid%sw040100 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sw100200,1)*SIZE(grid%sw100200,2) .GT. 1 ) THEN
grid%sw100200 (ims:ime,jps:min(jde-1,jpe)) = grid%sw100200 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sw010200,1)*SIZE(grid%sw010200,2) .GT. 1 ) THEN
grid%sw010200 (ims:ime,jps:min(jde-1,jpe)) = grid%sw010200 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soilw000,1)*SIZE(grid%soilw000,2) .GT. 1 ) THEN
grid%soilw000 (ims:ime,jps:min(jde-1,jpe)) = grid%soilw000 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soilw005,1)*SIZE(grid%soilw005,2) .GT. 1 ) THEN
grid%soilw005 (ims:ime,jps:min(jde-1,jpe)) = grid%soilw005 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soilw020,1)*SIZE(grid%soilw020,2) .GT. 1 ) THEN
grid%soilw020 (ims:ime,jps:min(jde-1,jpe)) = grid%soilw020 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soilw040,1)*SIZE(grid%soilw040,2) .GT. 1 ) THEN
grid%soilw040 (ims:ime,jps:min(jde-1,jpe)) = grid%soilw040 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soilw160,1)*SIZE(grid%soilw160,2) .GT. 1 ) THEN
grid%soilw160 (ims:ime,jps:min(jde-1,jpe)) = grid%soilw160 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soilw300,1)*SIZE(grid%soilw300,2) .GT. 1 ) THEN
grid%soilw300 (ims:ime,jps:min(jde-1,jpe)) = grid%soilw300 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%st000010,1)*SIZE(grid%st000010,2) .GT. 1 ) THEN
grid%st000010 (ims:ime,jps:min(jde-1,jpe)) = grid%st000010 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%st010040,1)*SIZE(grid%st010040,2) .GT. 1 ) THEN
grid%st010040 (ims:ime,jps:min(jde-1,jpe)) = grid%st010040 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%st040100,1)*SIZE(grid%st040100,2) .GT. 1 ) THEN
grid%st040100 (ims:ime,jps:min(jde-1,jpe)) = grid%st040100 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%st100200,1)*SIZE(grid%st100200,2) .GT. 1 ) THEN
grid%st100200 (ims:ime,jps:min(jde-1,jpe)) = grid%st100200 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%st010200,1)*SIZE(grid%st010200,2) .GT. 1 ) THEN
grid%st010200 (ims:ime,jps:min(jde-1,jpe)) = grid%st010200 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soilt000,1)*SIZE(grid%soilt000,2) .GT. 1 ) THEN
grid%soilt000 (ims:ime,jps:min(jde-1,jpe)) = grid%soilt000 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soilt005,1)*SIZE(grid%soilt005,2) .GT. 1 ) THEN
grid%soilt005 (ims:ime,jps:min(jde-1,jpe)) = grid%soilt005 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soilt020,1)*SIZE(grid%soilt020,2) .GT. 1 ) THEN
grid%soilt020 (ims:ime,jps:min(jde-1,jpe)) = grid%soilt020 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soilt040,1)*SIZE(grid%soilt040,2) .GT. 1 ) THEN
grid%soilt040 (ims:ime,jps:min(jde-1,jpe)) = grid%soilt040 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soilt160,1)*SIZE(grid%soilt160,2) .GT. 1 ) THEN
grid%soilt160 (ims:ime,jps:min(jde-1,jpe)) = grid%soilt160 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soilt300,1)*SIZE(grid%soilt300,2) .GT. 1 ) THEN
grid%soilt300 (ims:ime,jps:min(jde-1,jpe)) = grid%soilt300 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%landmask,1)*SIZE(grid%landmask,2) .GT. 1 ) THEN
grid%landmask (ims:ime,jps:min(jde-1,jpe)) = grid%landmask (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%topostdv,1)*SIZE(grid%topostdv,2) .GT. 1 ) THEN
grid%topostdv (ims:ime,jps:min(jde-1,jpe)) = grid%topostdv (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%toposlpx,1)*SIZE(grid%toposlpx,2) .GT. 1 ) THEN
grid%toposlpx (ims:ime,jps:min(jde-1,jpe)) = grid%toposlpx (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%toposlpy,1)*SIZE(grid%toposlpy,2) .GT. 1 ) THEN
grid%toposlpy (ims:ime,jps:min(jde-1,jpe)) = grid%toposlpy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%greenmax,1)*SIZE(grid%greenmax,2) .GT. 1 ) THEN
grid%greenmax (ims:ime,jps:min(jde-1,jpe)) = grid%greenmax (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%greenmin,1)*SIZE(grid%greenmin,2) .GT. 1 ) THEN
grid%greenmin (ims:ime,jps:min(jde-1,jpe)) = grid%greenmin (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%albedomx,1)*SIZE(grid%albedomx,2) .GT. 1 ) THEN
grid%albedomx (ims:ime,jps:min(jde-1,jpe)) = grid%albedomx (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%slopecat,1)*SIZE(grid%slopecat,2) .GT. 1 ) THEN
grid%slopecat (ims:ime,jps:min(jde-1,jpe)) = grid%slopecat (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%toposoil,1)*SIZE(grid%toposoil,2) .GT. 1 ) THEN
grid%toposoil (ims:ime,jps:min(jde-1,jpe)) = grid%toposoil (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%landusef,1)*SIZE(grid%landusef,3) .GT. 1 ) THEN
grid%landusef (ims:ime,:,jps:min(jde-1,jpe)) = grid%landusef (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soilctop,1)*SIZE(grid%soilctop,3) .GT. 1 ) THEN
grid%soilctop (ims:ime,:,jps:min(jde-1,jpe)) = grid%soilctop (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soilcbot,1)*SIZE(grid%soilcbot,3) .GT. 1 ) THEN
grid%soilcbot (ims:ime,:,jps:min(jde-1,jpe)) = grid%soilcbot (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
  DO itrace = PARAM_FIRST_SCALAR, num_moist
IF ( SIZE(grid%moist,1)*SIZE(grid%moist,2) .GT. 1 ) THEN
grid%moist (ims:ime,jps:min(jde-1,jpe),:,itrace) = grid%moist (ims:ime,jps+py:min(jde-1,jpe)+py,:,itrace)
ENDIF
  ENDDO
  DO itrace = PARAM_FIRST_SCALAR, num_dfi_moist
IF ( SIZE(grid%dfi_moist,1)*SIZE(grid%dfi_moist,2) .GT. 1 ) THEN
grid%dfi_moist (ims:ime,jps:min(jde-1,jpe),:,itrace) = grid%dfi_moist (ims:ime,jps+py:min(jde-1,jpe)+py,:,itrace)
ENDIF
  ENDDO
  DO itrace = PARAM_FIRST_SCALAR, num_scalar
IF ( SIZE(grid%scalar,1)*SIZE(grid%scalar,2) .GT. 1 ) THEN
grid%scalar (ims:ime,jps:min(jde-1,jpe),:,itrace) = grid%scalar (ims:ime,jps+py:min(jde-1,jpe)+py,:,itrace)
ENDIF
  ENDDO
  DO itrace = PARAM_FIRST_SCALAR, num_dfi_scalar
IF ( SIZE(grid%dfi_scalar,1)*SIZE(grid%dfi_scalar,3) .GT. 1 ) THEN
grid%dfi_scalar (ims:ime,:,jps:min(jde-1,jpe),itrace) = grid%dfi_scalar (ims:ime,:,jps+py:min(jde-1,jpe)+py,itrace)
ENDIF
  ENDDO
  DO itrace = PARAM_FIRST_SCALAR, num_chem
IF ( SIZE(grid%chem,1)*SIZE(grid%chem,3) .GT. 1 ) THEN
grid%chem (ims:ime,:,jps:min(jde-1,jpe),itrace) = grid%chem (ims:ime,:,jps+py:min(jde-1,jpe)+py,itrace)
ENDIF
  ENDDO
IF ( SIZE(grid%smois,1)*SIZE(grid%smois,3) .GT. 1 ) THEN
grid%smois (ims:ime,:,jps:min(jde-1,jpe)) = grid%smois (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%tslb,1)*SIZE(grid%tslb,3) .GT. 1 ) THEN
grid%tslb (ims:ime,:,jps:min(jde-1,jpe)) = grid%tslb (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%gsw,1)*SIZE(grid%gsw,2) .GT. 1 ) THEN
grid%gsw (ims:ime,jps:min(jde-1,jpe)) = grid%gsw (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%xlat,1)*SIZE(grid%xlat,2) .GT. 1 ) THEN
grid%xlat (ims:ime,jps:min(jde-1,jpe)) = grid%xlat (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%xlong,1)*SIZE(grid%xlong,2) .GT. 1 ) THEN
grid%xlong (ims:ime,jps:min(jde-1,jpe)) = grid%xlong (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%xland,1)*SIZE(grid%xland,2) .GT. 1 ) THEN
grid%xland (ims:ime,jps:min(jde-1,jpe)) = grid%xland (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%raincv,1)*SIZE(grid%raincv,2) .GT. 1 ) THEN
grid%raincv (ims:ime,jps:min(jde-1,jpe)) = grid%raincv (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%psfc,1)*SIZE(grid%psfc,2) .GT. 1 ) THEN
grid%psfc (ims:ime,jps:min(jde-1,jpe)) = grid%psfc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%th2,1)*SIZE(grid%th2,2) .GT. 1 ) THEN
grid%th2 (ims:ime,jps:min(jde-1,jpe)) = grid%th2 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%t2,1)*SIZE(grid%t2,2) .GT. 1 ) THEN
grid%t2 (ims:ime,jps:min(jde-1,jpe)) = grid%t2 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%u10,1)*SIZE(grid%u10,2) .GT. 1 ) THEN
grid%u10 (ims:ime,jps:min(jde-1,jpe)) = grid%u10 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%v10,1)*SIZE(grid%v10,2) .GT. 1 ) THEN
grid%v10 (ims:ime,jps:min(jde-1,jpe)) = grid%v10 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%xice,1)*SIZE(grid%xice,2) .GT. 1 ) THEN
grid%xice (ims:ime,jps:min(jde-1,jpe)) = grid%xice (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%lai,1)*SIZE(grid%lai,2) .GT. 1 ) THEN
grid%lai (ims:ime,jps:min(jde-1,jpe)) = grid%lai (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%smstav,1)*SIZE(grid%smstav,2) .GT. 1 ) THEN
grid%smstav (ims:ime,jps:min(jde-1,jpe)) = grid%smstav (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%smstot,1)*SIZE(grid%smstot,2) .GT. 1 ) THEN
grid%smstot (ims:ime,jps:min(jde-1,jpe)) = grid%smstot (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sfcrunoff,1)*SIZE(grid%sfcrunoff,2) .GT. 1 ) THEN
grid%sfcrunoff (ims:ime,jps:min(jde-1,jpe)) = grid%sfcrunoff (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%udrunoff,1)*SIZE(grid%udrunoff,2) .GT. 1 ) THEN
grid%udrunoff (ims:ime,jps:min(jde-1,jpe)) = grid%udrunoff (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%ivgtyp,1)*SIZE(grid%ivgtyp,2) .GT. 1 ) THEN
grid%ivgtyp (ims:ime,jps:min(jde-1,jpe)) = grid%ivgtyp (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%isltyp,1)*SIZE(grid%isltyp,2) .GT. 1 ) THEN
grid%isltyp (ims:ime,jps:min(jde-1,jpe)) = grid%isltyp (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%vegfra,1)*SIZE(grid%vegfra,2) .GT. 1 ) THEN
grid%vegfra (ims:ime,jps:min(jde-1,jpe)) = grid%vegfra (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sfcevp,1)*SIZE(grid%sfcevp,2) .GT. 1 ) THEN
grid%sfcevp (ims:ime,jps:min(jde-1,jpe)) = grid%sfcevp (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%grdflx,1)*SIZE(grid%grdflx,2) .GT. 1 ) THEN
grid%grdflx (ims:ime,jps:min(jde-1,jpe)) = grid%grdflx (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%albbck,1)*SIZE(grid%albbck,2) .GT. 1 ) THEN
grid%albbck (ims:ime,jps:min(jde-1,jpe)) = grid%albbck (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sfcexc,1)*SIZE(grid%sfcexc,2) .GT. 1 ) THEN
grid%sfcexc (ims:ime,jps:min(jde-1,jpe)) = grid%sfcexc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%snotime,1)*SIZE(grid%snotime,2) .GT. 1 ) THEN
grid%snotime (ims:ime,jps:min(jde-1,jpe)) = grid%snotime (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%acsnow,1)*SIZE(grid%acsnow,2) .GT. 1 ) THEN
grid%acsnow (ims:ime,jps:min(jde-1,jpe)) = grid%acsnow (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%acsnom,1)*SIZE(grid%acsnom,2) .GT. 1 ) THEN
grid%acsnom (ims:ime,jps:min(jde-1,jpe)) = grid%acsnom (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%rmol,1)*SIZE(grid%rmol,2) .GT. 1 ) THEN
grid%rmol (ims:ime,jps:min(jde-1,jpe)) = grid%rmol (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%snow,1)*SIZE(grid%snow,2) .GT. 1 ) THEN
grid%snow (ims:ime,jps:min(jde-1,jpe)) = grid%snow (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%canwat,1)*SIZE(grid%canwat,2) .GT. 1 ) THEN
grid%canwat (ims:ime,jps:min(jde-1,jpe)) = grid%canwat (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sst,1)*SIZE(grid%sst,2) .GT. 1 ) THEN
grid%sst (ims:ime,jps:min(jde-1,jpe)) = grid%sst (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%weasd,1)*SIZE(grid%weasd,2) .GT. 1 ) THEN
grid%weasd (ims:ime,jps:min(jde-1,jpe)) = grid%weasd (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%znt,1)*SIZE(grid%znt,2) .GT. 1 ) THEN
grid%znt (ims:ime,jps:min(jde-1,jpe)) = grid%znt (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%mol,1)*SIZE(grid%mol,2) .GT. 1 ) THEN
grid%mol (ims:ime,jps:min(jde-1,jpe)) = grid%mol (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%noahres,1)*SIZE(grid%noahres,2) .GT. 1 ) THEN
grid%noahres (ims:ime,jps:min(jde-1,jpe)) = grid%noahres (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%tke_myj,1)*SIZE(grid%tke_myj,2) .GT. 1 ) THEN
grid%tke_myj (ims:ime,jps:min(jde-1,jpe),:) = grid%tke_myj (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%el_myj,1)*SIZE(grid%el_myj,3) .GT. 1 ) THEN
grid%el_myj (ims:ime,:,jps:min(jde-1,jpe)) = grid%el_myj (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%exch_h,1)*SIZE(grid%exch_h,3) .GT. 1 ) THEN
grid%exch_h (ims:ime,:,jps:min(jde-1,jpe)) = grid%exch_h (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%exch_m,1)*SIZE(grid%exch_m,3) .GT. 1 ) THEN
grid%exch_m (ims:ime,:,jps:min(jde-1,jpe)) = grid%exch_m (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%thz0,1)*SIZE(grid%thz0,2) .GT. 1 ) THEN
grid%thz0 (ims:ime,jps:min(jde-1,jpe)) = grid%thz0 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%qz0,1)*SIZE(grid%qz0,2) .GT. 1 ) THEN
grid%qz0 (ims:ime,jps:min(jde-1,jpe)) = grid%qz0 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%uz0,1)*SIZE(grid%uz0,2) .GT. 1 ) THEN
grid%uz0 (ims:ime,jps:min(jde-1,jpe)) = grid%uz0 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%vz0,1)*SIZE(grid%vz0,2) .GT. 1 ) THEN
grid%vz0 (ims:ime,jps:min(jde-1,jpe)) = grid%vz0 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%flhc,1)*SIZE(grid%flhc,2) .GT. 1 ) THEN
grid%flhc (ims:ime,jps:min(jde-1,jpe)) = grid%flhc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%flqc,1)*SIZE(grid%flqc,2) .GT. 1 ) THEN
grid%flqc (ims:ime,jps:min(jde-1,jpe)) = grid%flqc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%qsg,1)*SIZE(grid%qsg,2) .GT. 1 ) THEN
grid%qsg (ims:ime,jps:min(jde-1,jpe)) = grid%qsg (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%qvg,1)*SIZE(grid%qvg,2) .GT. 1 ) THEN
grid%qvg (ims:ime,jps:min(jde-1,jpe)) = grid%qvg (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%qcg,1)*SIZE(grid%qcg,2) .GT. 1 ) THEN
grid%qcg (ims:ime,jps:min(jde-1,jpe)) = grid%qcg (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soilt1,1)*SIZE(grid%soilt1,2) .GT. 1 ) THEN
grid%soilt1 (ims:ime,jps:min(jde-1,jpe)) = grid%soilt1 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%tsnav,1)*SIZE(grid%tsnav,2) .GT. 1 ) THEN
grid%tsnav (ims:ime,jps:min(jde-1,jpe)) = grid%tsnav (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%psfc_out,1)*SIZE(grid%psfc_out,2) .GT. 1 ) THEN
grid%psfc_out (ims:ime,jps:min(jde-1,jpe)) = grid%psfc_out (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%uz0h,1)*SIZE(grid%uz0h,2) .GT. 1 ) THEN
grid%uz0h (ims:ime,jps:min(jde-1,jpe)) = grid%uz0h (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%vz0h,1)*SIZE(grid%vz0h,2) .GT. 1 ) THEN
grid%vz0h (ims:ime,jps:min(jde-1,jpe)) = grid%vz0h (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%dudt,1)*SIZE(grid%dudt,2) .GT. 1 ) THEN
grid%dudt (ims:ime,jps:min(jde-1,jpe),:) = grid%dudt (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%dvdt,1)*SIZE(grid%dvdt,2) .GT. 1 ) THEN
grid%dvdt (ims:ime,jps:min(jde-1,jpe),:) = grid%dvdt (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%qsfc,1)*SIZE(grid%qsfc,2) .GT. 1 ) THEN
grid%qsfc (ims:ime,jps:min(jde-1,jpe)) = grid%qsfc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%akhs,1)*SIZE(grid%akhs,2) .GT. 1 ) THEN
grid%akhs (ims:ime,jps:min(jde-1,jpe)) = grid%akhs (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%akms,1)*SIZE(grid%akms,2) .GT. 1 ) THEN
grid%akms (ims:ime,jps:min(jde-1,jpe)) = grid%akms (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%htop,1)*SIZE(grid%htop,2) .GT. 1 ) THEN
grid%htop (ims:ime,jps:min(jde-1,jpe)) = grid%htop (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hbot,1)*SIZE(grid%hbot,2) .GT. 1 ) THEN
grid%hbot (ims:ime,jps:min(jde-1,jpe)) = grid%hbot (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%htopr,1)*SIZE(grid%htopr,2) .GT. 1 ) THEN
grid%htopr (ims:ime,jps:min(jde-1,jpe)) = grid%htopr (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hbotr,1)*SIZE(grid%hbotr,2) .GT. 1 ) THEN
grid%hbotr (ims:ime,jps:min(jde-1,jpe)) = grid%hbotr (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%htopd,1)*SIZE(grid%htopd,2) .GT. 1 ) THEN
grid%htopd (ims:ime,jps:min(jde-1,jpe)) = grid%htopd (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hbotd,1)*SIZE(grid%hbotd,2) .GT. 1 ) THEN
grid%hbotd (ims:ime,jps:min(jde-1,jpe)) = grid%hbotd (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%htops,1)*SIZE(grid%htops,2) .GT. 1 ) THEN
grid%htops (ims:ime,jps:min(jde-1,jpe)) = grid%htops (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hbots,1)*SIZE(grid%hbots,2) .GT. 1 ) THEN
grid%hbots (ims:ime,jps:min(jde-1,jpe)) = grid%hbots (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%cuppt,1)*SIZE(grid%cuppt,2) .GT. 1 ) THEN
grid%cuppt (ims:ime,jps:min(jde-1,jpe)) = grid%cuppt (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%cprate,1)*SIZE(grid%cprate,2) .GT. 1 ) THEN
grid%cprate (ims:ime,jps:min(jde-1,jpe)) = grid%cprate (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%f_ice_phy,1)*SIZE(grid%f_ice_phy,3) .GT. 1 ) THEN
grid%f_ice_phy (ims:ime,:,jps:min(jde-1,jpe)) = grid%f_ice_phy (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%f_rain_phy,1)*SIZE(grid%f_rain_phy,3) .GT. 1 ) THEN
grid%f_rain_phy (ims:ime,:,jps:min(jde-1,jpe)) = grid%f_rain_phy (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%f_rimef_phy,1)*SIZE(grid%f_rimef_phy,3) .GT. 1 ) THEN
grid%f_rimef_phy (ims:ime,:,jps:min(jde-1,jpe)) = grid%f_rimef_phy (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%mass_flux,1)*SIZE(grid%mass_flux,2) .GT. 1 ) THEN
grid%mass_flux (ims:ime,jps:min(jde-1,jpe)) = grid%mass_flux (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%apr_gr,1)*SIZE(grid%apr_gr,2) .GT. 1 ) THEN
grid%apr_gr (ims:ime,jps:min(jde-1,jpe)) = grid%apr_gr (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%apr_w,1)*SIZE(grid%apr_w,2) .GT. 1 ) THEN
grid%apr_w (ims:ime,jps:min(jde-1,jpe)) = grid%apr_w (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%apr_mc,1)*SIZE(grid%apr_mc,2) .GT. 1 ) THEN
grid%apr_mc (ims:ime,jps:min(jde-1,jpe)) = grid%apr_mc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%apr_st,1)*SIZE(grid%apr_st,2) .GT. 1 ) THEN
grid%apr_st (ims:ime,jps:min(jde-1,jpe)) = grid%apr_st (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%apr_as,1)*SIZE(grid%apr_as,2) .GT. 1 ) THEN
grid%apr_as (ims:ime,jps:min(jde-1,jpe)) = grid%apr_as (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%apr_capma,1)*SIZE(grid%apr_capma,2) .GT. 1 ) THEN
grid%apr_capma (ims:ime,jps:min(jde-1,jpe)) = grid%apr_capma (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%apr_capme,1)*SIZE(grid%apr_capme,2) .GT. 1 ) THEN
grid%apr_capme (ims:ime,jps:min(jde-1,jpe)) = grid%apr_capme (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%apr_capmi,1)*SIZE(grid%apr_capmi,2) .GT. 1 ) THEN
grid%apr_capmi (ims:ime,jps:min(jde-1,jpe)) = grid%apr_capmi (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%rthften,1)*SIZE(grid%rthften,3) .GT. 1 ) THEN
grid%rthften (ims:ime,:,jps:min(jde-1,jpe)) = grid%rthften (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%rqvften,1)*SIZE(grid%rqvften,3) .GT. 1 ) THEN
grid%rqvften (ims:ime,:,jps:min(jde-1,jpe)) = grid%rqvften (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%snowh,1)*SIZE(grid%snowh,2) .GT. 1 ) THEN
grid%snowh (ims:ime,jps:min(jde-1,jpe)) = grid%snowh (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%rhosn,1)*SIZE(grid%rhosn,2) .GT. 1 ) THEN
grid%rhosn (ims:ime,jps:min(jde-1,jpe)) = grid%rhosn (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%smfr3d,1)*SIZE(grid%smfr3d,3) .GT. 1 ) THEN
grid%smfr3d (ims:ime,:,jps:min(jde-1,jpe)) = grid%smfr3d (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%keepfr3dflag,1)*SIZE(grid%keepfr3dflag,3) .GT. 1 ) THEN
grid%keepfr3dflag (ims:ime,:,jps:min(jde-1,jpe)) = grid%keepfr3dflag (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
!ENDOFREGISTRYGENERATEDINCLUDE
   enddo

   if (grid%id==2.and.ims<30.and.30<ime.and.jms<44.and.44<jme) then
     write(message,*)' after  shift rlwin ', grid%rlwin(30,44),px,py
     CALL wrf_message(trim(message))
   endif

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

END SUBROUTINE shift_domain_nmm
