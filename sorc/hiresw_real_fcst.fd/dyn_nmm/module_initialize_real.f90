






MODULE module_initialize_real

   USE module_bc
   USE module_configure
   USE module_domain
   USE module_io_domain
   USE module_model_constants

   USE module_state_description
   USE module_timing
   USE module_soil_pre
   USE module_dm
   USE module_ext_internal

   INTEGER :: internal_time_loop
      INTEGER:: MPI_COMM_COMP,MYPE

CONTAINS



   SUBROUTINE init_domain ( grid )

      IMPLICIT NONE

      


      TYPE (domain)          :: grid

      

      INTEGER :: idum1, idum2

      CALL set_scalar_indices_from_config ( head_grid%id , idum1, idum2 )

        CALL init_domain_nmm (grid &

!STARTOFREGISTRYGENERATEDINCLUDE 'inc/actual_args.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
,grid%x_1,grid%x_2,grid%lu_index,grid%lu_mask,grid%p_gc,grid%vegcat,grid%soilcat,grid%input_soil_cat,grid%tsk_gc,grid%xice_gc, &
grid%ght_gc,grid%rh_gc,grid%v_gc,grid%u_gc,grid%t_gc,grid%snoalb,grid%greenfrac_gc,grid%albedo12m_gc,grid%soilcbot_gc, &
grid%soilctop_gc,grid%tmn_gc,grid%htv_gc,grid%ht_gc,grid%landusef_gc,grid%vlon_gc,grid%vlat_gc,grid%hlon_gc,grid%hlat_gc, &
grid%hbm2,grid%hbm3,grid%vbm2,grid%vbm3,grid%sm,grid%sice,grid%pd,grid%pd_bxs,grid%pd_bxe,grid%pd_bys,grid%pd_bye,grid%pd_btxs, &
grid%pd_btxe,grid%pd_btys,grid%pd_btye,grid%fis,grid%res,grid%t,grid%t_bxs,grid%t_bxe,grid%t_bys,grid%t_bye,grid%t_btxs, &
grid%t_btxe,grid%t_btys,grid%t_btye,grid%q,grid%q_bxs,grid%q_bxe,grid%q_bys,grid%q_bye,grid%q_btxs,grid%q_btxe,grid%q_btys, &
grid%q_btye,grid%u,grid%u_bxs,grid%u_bxe,grid%u_bys,grid%u_bye,grid%u_btxs,grid%u_btxe,grid%u_btys,grid%u_btye,grid%v, &
grid%v_bxs,grid%v_bxe,grid%v_bys,grid%v_bye,grid%v_btxs,grid%v_btxe,grid%v_btys,grid%v_btye,grid%told,grid%uold,grid%vold, &
grid%hcoeff,grid%dfi_pd,grid%dfi_pint,grid%dfi_dwdt,grid%dfi_t,grid%dfi_q,grid%dfi_u,grid%dfi_v,grid%dfi_q2,grid%dfi_cwm, &
grid%dfi_rrw,grid%dfi_stc,grid%dfi_smc,grid%dfi_sh2o,grid%dfi_snow,grid%dfi_snowh,grid%dfi_canwat,grid%dfi_nmm_tsk, &
grid%dfi_snowc,grid%dx_nmm,grid%wpdar,grid%cpgfu,grid%curv,grid%fcp,grid%fdiv,grid%f,grid%fad,grid%ddmpu,grid%ddmpv,grid%deta, &
grid%rdeta,grid%aeta,grid%f4q2,grid%etax,grid%dfl,grid%deta1,grid%aeta1,grid%eta1,grid%deta2,grid%aeta2,grid%eta2,grid%em, &
grid%emt,grid%adt,grid%adu,grid%adv,grid%em_loc,grid%emt_loc,grid%pdsl,grid%pdslo,grid%psdt,grid%div,grid%few,grid%fne,grid%fns, &
grid%fse,grid%omgalf,grid%petdt,grid%rtop,grid%pblh,grid%lpbl,grid%mixht,grid%ustar,grid%z0,grid%z0base,grid%ths,grid%mavail, &
grid%qsh,grid%twbs,grid%qwbs,grid%taux,grid%tauy,grid%prec,grid%aprec,grid%acprec,grid%cuprec,grid%lspa,grid%ddata,grid%accliq, &
grid%sno,grid%si,grid%cldefi,grid%deep,grid%rf,grid%th10,grid%q10,grid%pshltr,grid%tshltr,grid%qshltr,grid%q2,grid%q2_bxs, &
grid%q2_bxe,grid%q2_bys,grid%q2_bye,grid%q2_btxs,grid%q2_btxe,grid%q2_btys,grid%q2_btye,grid%t_adj,grid%t_old,grid%zero_3d, &
grid%w0avg,grid%akhs_out,grid%akms_out,grid%albase,grid%albedo,grid%cnvbot,grid%cnvtop,grid%czen,grid%czmean,grid%embck, &
grid%epsr,grid%gffc,grid%glat,grid%glon,grid%nmm_tsk,grid%hdac,grid%hdacv,grid%mxsnal,grid%radin,grid%radot,grid%sigt4,grid%tg, &
grid%dfrlg,grid%lvl,grid%cwm,grid%cwm_bxs,grid%cwm_bxe,grid%cwm_bys,grid%cwm_bye,grid%cwm_btxs,grid%cwm_btxe,grid%cwm_btys, &
grid%cwm_btye,grid%rrw,grid%rrw_bxs,grid%rrw_bxe,grid%rrw_bys,grid%rrw_bye,grid%rrw_btxs,grid%rrw_btxe,grid%rrw_btys, &
grid%rrw_btye,grid%f_ice,grid%f_rain,grid%f_rimef,grid%cldfra,grid%sr,grid%cfrach,grid%cfracl,grid%cfracm,grid%islope, &
grid%dzsoil,grid%rtdpth,grid%sldpth,grid%cmc,grid%grnflx,grid%pctsno,grid%soiltb,grid%vegfrc,grid%shdmin,grid%shdmax,grid%sh2o, &
grid%smc,grid%stc,grid%hstdv,grid%hcnvx,grid%hasyw,grid%hasys,grid%hasysw,grid%hasynw,grid%hlenw,grid%hlens,grid%hlensw, &
grid%hlennw,grid%hangl,grid%hanis,grid%hslop,grid%hzmax,grid%crot,grid%srot,grid%ugwdsfc,grid%vgwdsfc,grid%dwdtmn,grid%dwdtmx, &
grid%dwdt,grid%pdwdt,grid%pint,grid%w,grid%z,grid%acfrcv,grid%acfrst,grid%ssroff,grid%bgroff,grid%rlwin,grid%rlwout,grid%rlwtoa, &
grid%alwin,grid%alwout,grid%alwtoa,grid%rswin,grid%rswinc,grid%rswout,grid%rswtoa,grid%aswin,grid%aswout,grid%aswtoa, &
grid%sfcshx,grid%sfclhx,grid%subshx,grid%snopcx,grid%sfcuvx,grid%potevp,grid%potflx,grid%tlmin,grid%tlmax,grid%t02_min, &
grid%t02_max,grid%rh02_min,grid%rh02_max,grid%rlwtt,grid%rswtt,grid%tcucn,grid%train,grid%ncfrcv,grid%ncfrst,grid%max10mw, &
grid%max10u,grid%max10v,grid%maxupdr,grid%maxdndr,grid%maxhlcy,grid%maxdbz,grid%ihe,grid%ihw,grid%ive,grid%ivw,grid%irad, &
grid%iheg,grid%ihwg,grid%iveg,grid%ivwg,grid%iradg,grid%n_iup_h,grid%n_iup_v,grid%n_iup_adh,grid%n_iup_adv,grid%iup_h, &
grid%iup_v,grid%iup_adh,grid%iup_adv,grid%imask_nostag,grid%imask_xstag,grid%imask_ystag,grid%imask_xystag,grid%sm000007, &
grid%sm007028,grid%sm028100,grid%sm100255,grid%st000007,grid%st007028,grid%st028100,grid%st100255,grid%sm000010,grid%sm010040, &
grid%sm040100,grid%sm100200,grid%sm010200,grid%soilm000,grid%soilm005,grid%soilm020,grid%soilm040,grid%soilm160,grid%soilm300, &
grid%sw000010,grid%sw010040,grid%sw040100,grid%sw100200,grid%sw010200,grid%soilw000,grid%soilw005,grid%soilw020,grid%soilw040, &
grid%soilw160,grid%soilw300,grid%st000010,grid%st010040,grid%st040100,grid%st100200,grid%st010200,grid%soilt000,grid%soilt005, &
grid%soilt020,grid%soilt040,grid%soilt160,grid%soilt300,grid%landmask,grid%topostdv,grid%toposlpx,grid%toposlpy,grid%greenmax, &
grid%greenmin,grid%albedomx,grid%slopecat,grid%toposoil,grid%landusef,grid%soilctop,grid%soilcbot,grid%ts_hour,grid%ts_u, &
grid%ts_v,grid%ts_q,grid%ts_t,grid%ts_psfc,grid%ts_tsk,grid%ts_tslb,grid%ts_clw,grid%moist,grid%dfi_moist,grid%scalar, &
grid%scalar_bxs,grid%scalar_bxe,grid%scalar_bys,grid%scalar_bye,grid%scalar_btxs,grid%scalar_btxe,grid%scalar_btys, &
grid%scalar_btye,grid%dfi_scalar,grid%dfi_scalar_bxs,grid%dfi_scalar_bxe,grid%dfi_scalar_bys,grid%dfi_scalar_bye, &
grid%dfi_scalar_btxs,grid%dfi_scalar_btxe,grid%dfi_scalar_btys,grid%dfi_scalar_btye,grid%chem,grid%smois,grid%tslb,grid%gsw, &
grid%xlat,grid%xlong,grid%xland,grid%raincv,grid%psfc,grid%th2,grid%t2,grid%u10,grid%v10,grid%xice,grid%lai,grid%smstav, &
grid%smstot,grid%sfcrunoff,grid%udrunoff,grid%ivgtyp,grid%isltyp,grid%vegfra,grid%sfcevp,grid%grdflx,grid%albbck,grid%sfcexc, &
grid%snotime,grid%acsnow,grid%acsnom,grid%rmol,grid%snow,grid%canwat,grid%sst,grid%weasd,grid%znt,grid%mol,grid%noahres, &
grid%tke_myj,grid%el_myj,grid%exch_h,grid%exch_m,grid%thz0,grid%qz0,grid%uz0,grid%vz0,grid%flhc,grid%flqc,grid%qsg,grid%qvg, &
grid%qcg,grid%soilt1,grid%tsnav,grid%psfc_out,grid%uz0h,grid%vz0h,grid%dudt,grid%dvdt,grid%qsfc,grid%akhs,grid%akms,grid%htop, &
grid%hbot,grid%htopr,grid%hbotr,grid%htopd,grid%hbotd,grid%htops,grid%hbots,grid%cuppt,grid%cprate,grid%f_ice_phy, &
grid%f_rain_phy,grid%f_rimef_phy,grid%mass_flux,grid%apr_gr,grid%apr_w,grid%apr_mc,grid%apr_st,grid%apr_as,grid%apr_capma, &
grid%apr_capme,grid%apr_capmi,grid%xf_ens,grid%pr_ens,grid%rthften,grid%rqvften,grid%snowh,grid%rhosn,grid%smfr3d, &
grid%keepfr3dflag,grid%mp_restart_state,grid%tbpvs_state,grid%tbpvs0_state,grid%lu_state &
!ENDOFREGISTRYGENERATEDINCLUDE

      )

   END SUBROUTINE init_domain



   SUBROUTINE init_domain_nmm ( grid &

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

      USE module_optional_input
      IMPLICIT NONE

      


      TYPE (domain)          :: grid

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

      TYPE (grid_config_rec_type)              :: config_flags

      

      INTEGER :: num_veg_cat , num_soil_top_cat , num_soil_bot_cat

      INTEGER                             ::                       &
                                     ids, ide, jds, jde, kds, kde, &
                                     ims, ime, jms, jme, kms, kme, &
                                     its, ite, jts, jte, kts, kte, &
                                     ips, ipe, jps, jpe, kps, kpe, &
                                     i, j, k, NNXP, NNYP

      

        CHARACTER(LEN=19):: start_date


        LOGICAL,EXTERNAL :: WRF_DM_ON_MONITOR


              REAL,ALLOCATABLE    :: SICE_G(:,:), SM_G(:,:)
              INTEGER, ALLOCATABLE::  IHE_G(:),IHW_G(:)
              INTEGER, ALLOCATABLE::  ITEMP(:,:)
              INTEGER :: my_e,my_n,my_s,my_w,my_ne,my_nw,my_se,my_sw,myi,myj,npe
              INTEGER :: istat,inpes,jnpes


      CHARACTER (LEN=255) :: message

      INTEGER :: error
      REAL    :: p_surf, p_level
      REAL    :: cof1, cof2
      REAL    :: qvf , qvf1 , qvf2 , pd_surf
      REAL    :: p00 , t00 , a
      REAL    :: hold_znw, rmin,rmax

      REAL :: p_top_requested , ptsgm
      INTEGER :: num_metgrid_levels, ICOUNT
      REAL , DIMENSION(max_eta) :: eta_levels

      LOGICAL :: stretch_grid, dry_sounding, debug, log_flag_sst, hyb_coor, boundary_flux

        REAL, ALLOCATABLE,DIMENSION(:,:),save:: ADUM2D,SNOWC,HT,TG_ALT, &
                                           PDVP,PSFC_OUTV

        REAL, ALLOCATABLE,DIMENSION(:,:,:),save:: P3D_OUT,P3DV_OUT,P3DV_IN, &
                                             QTMP,QTMP2

        INTEGER, ALLOCATABLE, DIMENSION(:),save:: KHL2,KVL2,KHH2,KVH2, &
                                             KHLA,KHHA,KVLA,KVHA



        REAL, ALLOCATABLE, DIMENSION(:),save:: DXJ,WPDARJ,CPGFUJ,CURVJ, &
                                          FCPJ,FDIVJ,EMJ,EMTJ,FADJ, &
                                          HDACJ,DDMPUJ,DDMPVJ

        REAL, ALLOCATABLE,DIMENSION(:),SAVE:: SG1,SG2,DSG1,DSG2, &
                                              SGML1,SGML2


         REAL , DIMENSION(100) :: lqmi
         integer iicount

        REAL:: TPH0D,TLM0D
        REAL:: TPH0,WB,SB,TDLM,TDPH
        REAL:: WBI,SBI,EBI,ANBI,STPH0,CTPH0
        REAL:: TSPH,DTAD,DTCF
        REAL:: ACDT,CDDAMP,DXP,FP
        REAL:: WBD,SBD
        REAL:: RSNOW,SNOFAC
        REAL, PARAMETER:: SALP=2.60
        REAL, PARAMETER:: SNUP=0.040
        REAL:: SMCSUM,STCSUM,SEAICESUM,FISX
        REAL:: cur_smc, aposs_smc

        INTEGER,PARAMETER:: DOUBLE=SELECTED_REAL_KIND(15,300)
        REAL(KIND=DOUBLE):: TERM1,APH,TLM,TPH,DLM,DPH,STPH,CTPH

        INTEGER:: KHH,KVH,JAM,JA, IHL, IHH, L
        INTEGER:: II,JJ,ISRCH,ISUM,ITER,Ilook,Jlook

        REAL, PARAMETER:: DTR=0.01745329
        REAL, PARAMETER:: W_NMM=0.08
        REAL, PARAMETER:: COAC=1.6

        REAL, PARAMETER:: CODAMP=15.0
        REAL, PARAMETER:: TWOM=.00014584
        REAL, PARAMETER:: CP=1004.6
        REAL, PARAMETER:: DFC=1.0
        REAL, PARAMETER:: DDFC=8.0
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

       IF ( CONFIG_FLAGS%FRACTIONAL_SEAICE == 1 ) THEN
          CALL wrf_error_fatal3("module_initialize_real.b",184,&
"NMM cannot use FRACTIONAL_SEAICE = 1")
       ENDIF

      if ( config_flags%bl_pbl_physics == BOULACSCHEME ) then
         call wrf_error_fatal3("module_initialize_real.b",189,&
"Cannot use BOULAC PBL with NMM")
      endif

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

      SELECT CASE ( model_data_order )
         CASE ( DATA_ORDER_ZXY )
            kds = grid%sd31 ; kde = grid%ed31 ;
            ids = grid%sd32 ; ide = grid%ed32 ;
            jds = grid%sd33 ; jde = grid%ed33 ;

            kms = grid%sm31 ; kme = grid%em31 ;
            ims = grid%sm32 ; ime = grid%em32 ;
            jms = grid%sm33 ; jme = grid%em33 ;

            kts = grid%sp31 ; kte = grid%ep31 ; 
            its = grid%sp32 ; ite = grid%ep32 ; 
            jts = grid%sp33 ; jte = grid%ep33 ; 

         CASE ( DATA_ORDER_XYZ )
            ids = grid%sd31 ; ide = grid%ed31 ;
            jds = grid%sd32 ; jde = grid%ed32 ;
            kds = grid%sd33 ; kde = grid%ed33 ;

            ims = grid%sm31 ; ime = grid%em31 ;
            jms = grid%sm32 ; jme = grid%em32 ;
            kms = grid%sm33 ; kme = grid%em33 ;

            its = grid%sp31 ; ite = grid%ep31 ; 
            jts = grid%sp32 ; jte = grid%ep32 ; 
            kts = grid%sp33 ; kte = grid%ep33 ; 

         CASE ( DATA_ORDER_XZY )
            ids = grid%sd31 ; ide = grid%ed31 ;
            kds = grid%sd32 ; kde = grid%ed32 ;
            jds = grid%sd33 ; jde = grid%ed33 ;

            ims = grid%sm31 ; ime = grid%em31 ;
            kms = grid%sm32 ; kme = grid%em32 ;
            jms = grid%sm33 ; jme = grid%em33 ;

            its = grid%sp31 ; ite = grid%ep31 ; 
            kts = grid%sp32 ; kte = grid%ep32 ; 
            jts = grid%sp33 ; jte = grid%ep33 ; 

      END SELECT

      CALL WRF_GET_MYPROC(MYPE)
      CALL WRF_GET_DM_COMMUNICATOR(MPI_COMM_COMP)
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

        grid%DT=float(grid%TIME_STEP)

        NNXP=min(ITE,IDE-1)
        NNYP=min(JTE,JDE-1)

        write(message,*) 'IDE, JDE: ', IDE,JDE
        write(message,*) 'NNXP, NNYP: ', NNXP,NNYP
        CALL wrf_message(message)

        JAM=6+2*(JDE-JDS-10)

        if (internal_time_loop .eq. 1) then
          ALLOCATE(ADUM2D(grid%sm31:grid%em31,jms:jme))
          ALLOCATE(KHL2(JTS:NNYP),KVL2(JTS:NNYP),KHH2(JTS:NNYP),KVH2(JTS:NNYP))
          ALLOCATE(DXJ(JTS:NNYP),WPDARJ(JTS:NNYP),CPGFUJ(JTS:NNYP),CURVJ(JTS:NNYP))
          ALLOCATE(FCPJ(JTS:NNYP),FDIVJ(JTS:NNYP),&
                   FADJ(JTS:NNYP))
          ALLOCATE(HDACJ(JTS:NNYP),DDMPUJ(JTS:NNYP),DDMPVJ(JTS:NNYP))
          ALLOCATE(KHLA(JAM),KHHA(JAM))
          ALLOCATE(KVLA(JAM),KVHA(JAM))
        endif


    CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

        write(message,*) 'cen_lat: ', config_flags%cen_lat
        CALL wrf_debug(100,message)
        write(message,*) 'cen_lon: ', config_flags%cen_lon
        CALL wrf_debug(100,message)
        write(message,*) 'dx: ', config_flags%dx
        CALL wrf_debug(100,message)
        write(message,*) 'dy: ', config_flags%dy
        CALL wrf_debug(100,message)
        write(message,*) 'config_flags%start_year: ', config_flags%start_year
        CALL wrf_debug(100,message)
        write(message,*) 'config_flags%start_month: ', config_flags%start_month
        CALL wrf_debug(100,message)
        write(message,*) 'config_flags%start_day: ', config_flags%start_day
        CALL wrf_debug(100,message)
        write(message,*) 'config_flags%start_hour: ', config_flags%start_hour
        CALL wrf_debug(100,message)

        write(start_date,435) config_flags%start_year, config_flags%start_month, &
                config_flags%start_day, config_flags%start_hour
  435   format(I4,'-',I2.2,'-',I2.2,'_',I2.2,':00:00')

        dlmd=config_flags%dx
        dphd=config_flags%dy
        tph0d=config_flags%cen_lat
        tlm0d=config_flags%cen_lon





 
 
 

      CALL boundary_condition_check( config_flags, bdyzone, error, grid%id )

      

      grid%itimestep=0

      

      grid%real_data_init_type = model_config_rec%real_data_init_type
      write(message,*) 'what is flag_metgrid: ', flag_metgrid
      CALL wrf_message(message)

     IF ( flag_metgrid .EQ. 1 ) THEN  

     num_metgrid_levels = grid%num_metgrid_levels

	IF (ght_gc(its,jts,num_metgrid_levels/2) .lt. ght_gc(its,jts,num_metgrid_levels/2+1)) THEN


           write(message,*) 'normal ground up file order'
           hyb_coor=.false.
           CALL wrf_message(message)

        ELSE

           hyb_coor=.true.
           write(message,*) 'reverse the order of coordinate'
           CALL wrf_message(message)

           CALL reverse_vert_coord(ght_gc, 2, num_metgrid_levels &
     &,                            IDS,IDE,JDS,JDE,KDS,KDE        &
     &,                            IMS,IME,JMS,JME,KMS,KME        &
     &,                            ITS,ITE,JTS,JTE,KTS,KTE )

           CALL reverse_vert_coord(p_gc, 2, num_metgrid_levels &
     &,                            IDS,IDE,JDS,JDE,KDS,KDE        &
     &,                            IMS,IME,JMS,JME,KMS,KME        &
     &,                            ITS,ITE,JTS,JTE,KTS,KTE )

           CALL reverse_vert_coord(t_gc, 2, num_metgrid_levels &
     &,                            IDS,IDE,JDS,JDE,KDS,KDE        &
     &,                            IMS,IME,JMS,JME,KMS,KME        &
     &,                            ITS,ITE,JTS,JTE,KTS,KTE )

           CALL reverse_vert_coord(u_gc, 2, num_metgrid_levels &
     &,                            IDS,IDE,JDS,JDE,KDS,KDE        &
     &,                            IMS,IME,JMS,JME,KMS,KME        &
     &,                            ITS,ITE,JTS,JTE,KTS,KTE )

           CALL reverse_vert_coord(v_gc, 2, num_metgrid_levels &
     &,                            IDS,IDE,JDS,JDE,KDS,KDE        &
     &,                            IMS,IME,JMS,JME,KMS,KME        &
     &,                            ITS,ITE,JTS,JTE,KTS,KTE )

           CALL reverse_vert_coord(rh_gc, 2, num_metgrid_levels &
     &,                            IDS,IDE,JDS,JDE,KDS,KDE        &
     &,                            IMS,IME,JMS,JME,KMS,KME        &
     &,                            ITS,ITE,JTS,JTE,KTS,KTE )

        endif


        IF (hyb_coor) THEN
                           
                           
                           
          write(message,*) 'min, max of ht_gc before adjust: ', minval(ht_gc), maxval(ht_gc)
          CALL wrf_debug(100,message)
          ICOUNT=0
          DO J=JTS,min(JTE,JDE-1)
           DO I=ITS,min(ITE,IDE-1)
             IF ((ht_gc(I,J) - ght_gc(I,J,2)) .LT. -150.) THEN
               ht_gc(I,J)=ght_gc(I,J,2)-150.
               IF (ICOUNT .LT. 20) THEN
                 write(message,*) 'increasing NMM topo toward RUC  ', I,J
                 CALL wrf_debug(100,message)
                 ICOUNT=ICOUNT+1
               ENDIF
             ELSEIF ((ht_gc(I,J) - ght_gc(I,J,2)) .GT. 150.) THEN
               ht_gc(I,J)=ght_gc(I,J,2)+150.
               IF (ICOUNT .LT. 20) THEN
                 write(message,*) 'decreasing NMM topo toward RUC ', I,J
                 CALL wrf_debug(100,message)
                 ICOUNT=ICOUNT+1
               ENDIF
             ENDIF
           END DO
          END DO

          write(message,*) 'min, max of ht_gc after correct: ', minval(ht_gc), maxval(ht_gc)
          CALL wrf_debug(100,message)
        ENDIF

      CALL boundary_smooth(ht_gc,landmask, grid, 12 , 12 &
     &,                          IDS,IDE,JDS,JDE,KDS,KDE             &
     &,                          IMS,IME,JMS,JME,KMS,KME             &
     &,                          ITS,ITE,JTS,JTE,KTS,KTE )

       DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)
           if (LANDMASK(I,J) .gt. 0.5) SM(I,J)=0.
           if (LANDMASK(I,J) .le. 0.5) SM(I,J)=1.
        if (tsk_gc(I,J) .gt. 0.) then
           NMM_TSK(I,J)=tsk_gc(I,J)
        else
           NMM_TSK(I,J)=t_gc(I,J,1) 
        endif

           GLAT(I,J)=hlat_gc(I,J)*DEGRAD
           GLON(I,J)=hlon_gc(I,J)*DEGRAD
           WEASD(I,J)=SNOW(I,J)
           XICE(I,J)=XICE_gc(I,J)
         ENDDO
       ENDDO


     num_metgrid_levels = grid%num_metgrid_levels
     eta_levels(1:kde) = model_config_rec%eta_levels(1:kde)
     ptsgm             = model_config_rec%ptsgm
     p_top_requested = grid%p_top_requested
     pt=p_top_requested
      boundary_flux=model_config_rec%boundary_flux
        write(0,*) 'boundary_flux now: ', boundary_flux


        if (internal_time_loop .eq. 1) then

        if (eta_levels(1) .ne. 1.0) then
          write(message,*) '********************************************************************* '
          CALL wrf_message(message)
          write(message,*) '**  eta_levels appears not to be specified in the namelist'
          CALL wrf_message(message)
          write(message,*) '**  We will call compute_nmm_levels to define layer thicknesses.'
          CALL wrf_message(message)
          write(message,*) '**  These levels should be reasonable for running the model, '
          CALL wrf_message(message)
          write(message,*) '**  but may not be ideal for the simulation being made.  Consider   '
          CALL wrf_message(message)
          write(message,*) '**  defining your own levels by specifying eta_levels in the model   '
          CALL wrf_message(message)
          write(message,*) '**  namelist. '
          CALL wrf_message(message)
          write(message,*) '********************************************************************** '
          CALL wrf_message(message)

          CALL compute_nmm_levels(KDE,p_top_requested,eta_levels)

          DO L=1,KDE
            write(message,*) 'L, eta_levels(L) returned :: ', L,eta_levels(L)
            CALL wrf_message(message)
          ENDDO

        endif

        write(message,*) 'KDE-1: ', KDE-1
        CALL wrf_debug(1,message)
        allocate(SG1(1:KDE-1))
        allocate(SG2(1:KDE-1))
        allocate(DSG1(1:KDE-1))
        allocate(DSG2(1:KDE-1))
        allocate(SGML1(1:KDE))
        allocate(SGML2(1:KDE))

      CALL define_nmm_vertical_coord (kde-1, ptsgm, pt,pdtop, eta_levels, &
                                      ETA1,DETA1,AETA1,             &
                                      ETA2,DETA2,AETA2, DFL, DFRLG         )

       DO L=KDS,KDE-1
        DETA(L)=eta_levels(L)-eta_levels(L+1)
       ENDDO
        endif

        if (.NOT. allocated(PDVP))     allocate(PDVP(IMS:IME,JMS:JME))
        if (.NOT. allocated(P3D_OUT))  allocate(P3D_OUT(IMS:IME,JMS:JME,KDS:KDE-1))
        if (.NOT. allocated(PSFC_OUTV)) allocate(PSFC_OUTV(IMS:IME,JMS:JME))
        if (.NOT. allocated(P3DV_OUT)) allocate(P3DV_OUT(IMS:IME,JMS:JME,KDS:KDE-1))
        if (.NOT. allocated(P3DV_IN))  allocate(P3DV_IN(IMS:IME,JMS:JME,num_metgrid_levels))

        write(message,*) 'num_metgrid_levels: ', num_metgrid_levels
        CALL wrf_message(message)

       DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)
           FIS(I,J)=ht_gc(I,J)*g


        IF ( p_gc(I,J,1) .ne. 200100. .AND.  (abs(ht_gc(I,J)-ght_gc(I,J,1)) .lt. 0.01) .AND. ht_gc(I,J) .ne. 0) THEN
          IF (mod(I,10) .eq. 0 .and. mod(J,10) .eq. 0) THEN
            write(message,*) 'ht_gc and toposoil to swap, flag_soilhgt ::: ', &
                          I,J, ht_gc(I,J),toposoil(I,J),flag_soilhgt
            CALL wrf_debug(10,message)
          ENDIF
                IF ( ( flag_soilhgt.EQ. 1 ) ) THEN
                 ght_gc(I,J,1)=toposoil(I,J)
                ENDIF
        ENDIF

         ENDDO
       ENDDO

      CALL compute_nmm_surfacep (ht_gc, ght_gc, p_gc , t_gc               &
     &,                          psfc_out, num_metgrid_levels  &
     &,                          IDS,IDE,JDS,JDE,KDS,KDE             &
     &,                          IMS,IME,JMS,JME,KMS,KME             &
     &,                          ITS,ITE,JTS,JTE,KTS,KTE ) 

      CALL compute_3d_pressure (psfc_out,AETA1,AETA2   &
     &,            pdtop,pt,pd,p3d_out                 &
     &,            IDS,IDE,JDS,JDE,KDS,KDE             &
     &,            IMS,IME,JMS,JME,KMS,KME             &
     &,            ITS,ITE,JTS,JTE,KTS,KTE )

        ips=its ; ipe=ite ;  jps=jts ; jpe=jte ; kps=kts ; kpe=kte
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_MG2.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_MG2_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE

!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_MG3.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_MG3_sub ( grid, &
  config_flags, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE

       do K=1,num_metgrid_levels
        do J=JTS,min(JTE,JDE-1)
         do I=ITS,min(ITE,IDE-1)

         IF (K .eq. KTS) THEN
           IF (J .eq. JDS .and. I .lt. IDE-1) THEN  
             PDVP(I,J)=0.5*(PD(I,J)+PD(I+1,J))
             PSFC_OUTV(I,J)=0.5*(PSFC_OUT(I,J)+PSFC_OUT(I+1,J))
           ELSEIF (J .eq. JDE-1 .and. I .lt. IDE-1) THEN 
             PDVP(I,J)=0.5*(PD(I,J)+PD(I+1,J))
             PSFC_OUTV(I,J)=0.5*(PSFC_OUT(I,J)+PSFC_OUT(I+1,J))
           ELSEIF (I .eq. IDS .and. mod(J,2) .eq. 0) THEN 
             PDVP(I,J)=0.5*(PD(I,J-1)+PD(I,J+1))
             PSFC_OUTV(I,J)=0.5*(PSFC_OUT(I,J-1)+PSFC_OUT(I,J+1))
           ELSEIF (I .eq. IDE-1 .and. mod(J,2) .eq. 0) THEN 
             PDVP(I,J)=0.5*(PD(I,J-1)+PD(I,J+1))
             PSFC_OUTV(I,J)=0.5*(PSFC_OUT(I,J-1)+PSFC_OUT(I,J+1))
           ELSEIF (I .eq. IDE-1 .and. mod(J,2) .eq. 1) THEN 
             PDVP(I,J)=PD(I,J)
             PSFC_OUTV(I,J)=PSFC_OUT(I,J)
           ELSEIF (mod(J,2) .eq. 0) THEN 
             PDVP(I,J)=0.25*(PD(I,J)+PD(I-1,J)+PD(I,J+1)+PD(I,J-1))
             PSFC_OUTV(I,J)=0.25*(PSFC_OUT(I,J)+PSFC_OUT(I-1,J)+ &
                                  PSFC_OUT(I,J+1)+PSFC_OUT(I,J-1))
           ELSE 
             PDVP(I,J)=0.25*(PD(I,J)+PD(I+1,J)+PD(I,J+1)+PD(I,J-1))
             PSFC_OUTV(I,J)=0.25*(PSFC_OUT(I,J)+PSFC_OUT(I+1,J)+ &
                                  PSFC_OUT(I,J+1)+PSFC_OUT(I,J-1))
           ENDIF
          ENDIF

           IF (J .eq. JDS .and. I .lt. IDE-1) THEN  
             P3DV_IN(I,J,K)=0.5*(p_gc(I,J,K)+p_gc(I+1,J,K))
           ELSEIF (J .eq. JDE-1 .and. I .lt. IDE-1) THEN 
             P3DV_IN(I,J,K)=0.5*(p_gc(I,J,K)+p_gc(I+1,J,K))
           ELSEIF (I .eq. IDS .and. mod(J,2) .eq. 0) THEN 
             P3DV_IN(I,J,K)=0.5*(p_gc(I,J-1,K)+p_gc(I,J+1,K))
           ELSEIF (I .eq. IDE-1 .and. mod(J,2) .eq. 0) THEN 
             P3DV_IN(I,J,K)=0.5*(p_gc(I,J-1,K)+p_gc(I,J+1,K))
           ELSEIF (I .eq. IDE-1 .and. mod(J,2) .eq. 1) THEN 
             P3DV_IN(I,J,K)=p_gc(I,J,K)
           ELSEIF (mod(J,2) .eq. 0) THEN 
             P3DV_IN(I,J,K)=0.25*(p_gc(I,J,K)+p_gc(I-1,J,K) + &
                                  p_gc(I,J+1,K)+p_gc(I,J-1,K))
           ELSE 
             P3DV_IN(I,J,K)=0.25*(p_gc(I,J,K)+p_gc(I+1,J,K) + &
                                  p_gc(I,J+1,K)+p_gc(I,J-1,K))
           ENDIF

         enddo
        enddo
       enddo

      CALL compute_3d_pressure (psfc_outv,AETA1,AETA2   &
     &,            pdtop,pt,pdvp,p3dv_out              &
     &,            IDS,IDE,JDS,JDE,KDS,KDE             &
     &,            IMS,IME,JMS,JME,KMS,KME             &
     &,            ITS,ITE,JTS,JTE,KTS,KTE )

      CALL interp_press2press_lin(p_gc, p3d_out        &
     &,            t_gc, T,num_metgrid_levels          &
     &,            .TRUE.,.TRUE.,.TRUE.               & 
     &,            IDS,IDE,JDS,JDE,KDS,KDE             &
     &,            IMS,IME,JMS,JME,KMS,KME             &
     &,            ITS,ITE,JTS,JTE,KTS,KTE, internal_time_loop )


      CALL interp_press2press_lin(p3dv_in, p3dv_out        &
     &,            u_gc, U,num_metgrid_levels          &
     &,            .FALSE.,.TRUE.,.FALSE.              &
     &,            IDS,IDE,JDS,JDE,KDS,KDE             &
     &,            IMS,IME,JMS,JME,KMS,KME             &
     &,            ITS,ITE,JTS,JTE,KTS,KTE, internal_time_loop )

      CALL interp_press2press_lin(p3dv_in, p3dv_out        &
     &,            V_gc, V,num_metgrid_levels          &
     &,            .FALSE.,.TRUE.,.FALSE.              &
     &,            IDS,IDE,JDS,JDE,KDS,KDE             &
     &,            IMS,IME,JMS,JME,KMS,KME             &
     &,            ITS,ITE,JTS,JTE,KTS,KTE, internal_time_loop )

       IF (hyb_coor) THEN
       CALL wind_adjust(p3dv_in,p3dv_out,U_gc,V_gc,U,V  &
     &,                 num_metgrid_levels,5000.        &
     &,                 IDS,IDE,JDS,JDE,KDS,KDE           &
     &,                 IMS,IME,JMS,JME,KMS,KME           &
     &,                 ITS,ITE,JTS,JTE,KTS,KTE )
       ENDIF

       IF (boundary_flux) THEN
        write(0,*) 'call egrid_boundary_winds'
        call egrid_boundary_winds(PDVP, U, V, &
     &                            p3dv_in,  U_gc, V_gc,     &
     &                            DETA1, DETA2,pdtop,pt,     &
     &                            IDS,IDE,JDS,JDE,KDS,KDE,          &
     &                            IMS,IME,JMS,JME,KMS,KME,          &
     &                            ITS,ITE,JTS,JTE,KTS,KTE,num_metgrid_levels  )
       ENDIF



	if (.not. allocated(qtmp)) then
         ALLOCATE(qtmp(IMS:IME,JMS:JME,num_metgrid_levels))
         ALLOCATE(qtmp2(IMS:IME,JMS:JME,num_metgrid_levels))
        endif

            CALL rh_to_mxrat (rh_gc, t_gc, p_gc, qtmp , .TRUE. , &
                        ids , ide , jds , jde , 1 , num_metgrid_levels , &
                        ims , ime , jms , jme , 1 , num_metgrid_levels , &
                        its , ite , jts , jte , 1 , num_metgrid_levels )

       do K=1,num_metgrid_levels
        do J=JTS,min(JTE,JDE-1)
         do I=ITS,min(ITE,IDE-1)
           QTMP2(I,J,K)=QTMP(I,J,K)/(1.0+QTMP(I,J,K))
         end do
        end do
       end do

      CALL interp_press2press_log(p_gc, p3d_out        &
     &,            QTMP2, Q,num_metgrid_levels          &
     &,            .FALSE.,.TRUE.                      &
     &,            IDS,IDE,JDS,JDE,KDS,KDE             &
     &,            IMS,IME,JMS,JME,KMS,KME             &
     &,            ITS,ITE,JTS,JTE,KTS,KTE, internal_time_loop )




         
         
         

        if (internal_time_loop .eq. 1) then

         CALL monthly_interp_to_date ( greenfrac_gc , current_date , vegfra , &
                                       ids , ide , jds , jde , kds , kde , &
                                       ims , ime , jms , jme , kms , kme , &
                                       its , ite , jts , jte , kts , kte )

         CALL monthly_interp_to_date ( albedo12m_gc , current_date , albbck , &
                                       ids , ide , jds , jde , kds , kde , &
                                       ims , ime , jms , jme , kms , kme , &
                                       its , ite , jts , jte , kts , kte )

         

         CALL monthly_min_max ( greenfrac_gc , shdmin , shdmax , &
                                ids , ide , jds , jde , kds , kde , &
                                ims , ime , jms , jme , kms , kme , &
                                its , ite , jts , jte , kts , kte )

         

         DO j = jts, MIN(jte,jde-1)
           DO i = its, MIN(ite,ide-1)

              shdmax(i,j) = shdmax(i,j) * 100.
              shdmin(i,j) = shdmin(i,j) * 100.
              VEGFRC(I,J)=VEGFRA(I,J)
           END DO
         END DO

         
         

         DO j = jts, MIN(jte,jde-1)
           DO i = its, MIN(ite,ide-1)
              if (albbck(i,j) .lt. 5.) then
                  write(message,*) 'reset albedo to 8%...  I,J,albbck:: ', I,J,albbck(I,J)
                  CALL wrf_debug(10,message)
                  albbck(I,J)=8.
              endif
              albbck(i,j) = albbck(i,j) / 100.
              snoalb(i,j) = snoalb(i,j) / 100.
              IF ( landmask(i,j) .LT. 0.5 ) THEN
                 albbck(i,j) = 0.08
                 snoalb(i,j) = 0.08
              END IF
              albase(i,j)=albbck(i,j)
              mxsnal(i,j)=snoalb(i,j)
           END DO
         END DO

         endif




     END IF     

        if (internal_time_loop .eq. 1) then



       DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)

      IF(SM(I,J).GT.0.9) THEN

       IF (XICE(I,J) .gt. 0) then
         SI(I,J)=1.0
       ENDIF


              EPSR(I,J)=.97
              EMBCK(I,J)=.97
              GFFC(I,J)=0.
              ALBEDO(I,J)=.06
              ALBASE(I,J)=.06
              IF(SI (I,J).GT.0.    ) THEN

                 SM(I,J)=0.
                 SI(I,J)=0.
                 SICE(I,J)=1.
                 GFFC(I,J)=0. 
                 ALBEDO(I,J)=.60
                 ALBASE(I,J)=.60
              ENDIF
           ELSE

        SI(I,J)=5.0*WEASD(I,J)/1000.

        EPSR(I,J)=1.0
        EMBCK(I,J)=1.0
        GFFC(I,J)=0.0 
        SICE(I,J)=0.
        SNO(I,J)=SI(I,J)*.20
           ENDIF
        ENDDO
        ENDDO


       DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)
          IF(SM(I,J).LT.0.9.AND.SICE(I,J).LT.0.9) THEN

            IF ( (SNO(I,J) .EQ. 0.0) .OR. &
                (ALBASE(I,J) .GE. MXSNAL(I,J) ) ) THEN
              ALBEDO(I,J) = ALBASE(I,J)
            ELSE


              IF (SNO(I,J) .LT. SNUP) THEN
                RSNOW = SNO(I,J)/SNUP
                SNOFAC = 1. - ( EXP(-SALP*RSNOW) - RSNOW*EXP(-SALP))

              ELSE
                SNOFAC = 1.0
              ENDIF

              ALBEDO(I,J) = ALBASE(I,J) &
               + (1.0-VEGFRA(I,J))*SNOFAC*(MXSNAL(I,J)-ALBASE(I,J))
            ENDIF
          END IF
          SI(I,J)=5.0*WEASD(I,J)
          SNO(I,J)=WEASD(I,J)


          VEGFRA(I,J)=VEGFRA(I,J)*100.

        ENDDO
      ENDDO


        ALLOCATE(SM_G(IDS:IDE,JDS:JDE),SICE_G(IDS:IDE,JDS:JDE))

        CALL WRF_PATCH_TO_GLOBAL_REAL( SICE(IMS,JMS)           &
     &,                                SICE_G,grid%DOMDESC         &
     &,                               'z','xy'                       &
     &,                                IDS,IDE-1,JDS,JDE-1,1,1          &
     &,                                IMS,IME,JMS,JME,1,1              &
     &,                                ITS,ITE,JTS,JTE,1,1 )

        CALL WRF_PATCH_TO_GLOBAL_REAL( SM(IMS,JMS)           &
     &,                                SM_G,grid%DOMDESC         &
     &,                               'z','xy'                       &
     &,                                IDS,IDE-1,JDS,JDE-1,1,1          &
     &,                                IMS,IME,JMS,JME,1,1              &
     &,                                ITS,ITE,JTS,JTE,1,1 )


        IF (WRF_DM_ON_MONITOR()) THEN

  637   format(40(f3.0,1x))

        allocate(IHE_G(JDS:JDE-1),IHW_G(JDS:JDE-1))
       DO j = JDS, JDE-1
          IHE_G(J)=MOD(J+1,2)
          IHW_G(J)=IHE_G(J)-1
       ENDDO

      DO ITER=1,10
       DO j = jds+1, (jde-1)-1
         DO i = ids+1, (ide-1)-1



        IF (SM_G(I,J) .ge. 0.9) THEN
          SEAICESUM=SICE_G(I+IHE_G(J),J+1)+SICE_G(I+IHW_G(J),J+1)+ &
                    SICE_G(I+IHE_G(J),J-1)+SICE_G(I+IHW_G(J),J-1)
          IF (SEAICESUM .ge. 1. .and. SEAICESUM .lt. 3.) THEN

            IF ((SICE_G(I+IHE_G(J),J+1).lt.0.1 .and. SM_G(I+IHE_G(J),J+1).lt.0.1) .OR. &
                (SICE_G(I+IHW_G(J),J+1).lt.0.1 .and. SM_G(I+IHW_G(J),J+1).lt.0.1) .OR. &
                (SICE_G(I+IHE_G(J),J-1).lt.0.1 .and. SM_G(I+IHE_G(J),J-1).lt.0.1) .OR. &
                (SICE_G(I+IHW_G(J),J-1).lt.0.1 .and. SM_G(I+IHW_G(J),J-1).lt.0.1)) THEN



              write(message,*) 'making seaice (1): ', I,J
              CALL wrf_debug(100,message)
              SICE_G(I,J)=1.0
              SM_G(I,J)=0.

            ENDIF

          ELSEIF (SEAICESUM .ge. 3) THEN



            write(message,*) 'making seaice (2): ', I,J
            CALL wrf_debug(100,message)
            SICE_G(I,J)=1.0
            SM_G(I,J)=0.
          ENDIF

        ENDIF

        ENDDO
       ENDDO
      ENDDO

        ENDIF

        CALL WRF_GLOBAL_TO_PATCH_REAL( SICE_G, SICE           &
     &,                                grid%DOMDESC         &
     &,                               'z','xy'                       &
     &,                                IDS,IDE-1,JDS,JDE-1,1,1          &
     &,                                IMS,IME,JMS,JME,1,1              &
     &,                                ITS,ITE,JTS,JTE,1,1 )

        CALL WRF_GLOBAL_TO_PATCH_REAL( SM_G,SM           &
     &,                                grid%DOMDESC         &
     &,                               'z','xy'                       &
     &,                                IDS,IDE-1,JDS,JDE-1,1,1          &
     &,                                IMS,IME,JMS,JME,1,1              &
     &,                                ITS,ITE,JTS,JTE,1,1 )

        IF (WRF_DM_ON_MONITOR()) THEN

        DEALLOCATE(SM_G,SICE_G)
        DEALLOCATE(IHE_G,IHW_G)

        ENDIF











       DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)

          IF (SM(I,J) .gt. 0.5) THEN
                landmask(I,J)=0.0
          ELSEIF (SM(I,J) .lt. 0.5 .and. SICE(I,J) .gt. 0.9) then
                landmask(I,J)=0.0
          ELSEIF (SM(I,J) .lt. 0.5 .and. SICE(I,J) .lt. 0.1) then
                landmask(I,J)=1.0
          ELSE
                write(message,*) 'missed point in landmask definition ' , I,J
                CALL wrf_message(message)
                landmask(I,J)=0.0
          ENDIF

        IF (SICE(I,J) .gt. 0.5 .and. NMM_TSK(I,J) .lt. 0.1 .and. SST(I,J) .gt. 0.) THEN
           write(message,*) 'set NMM_TSK to: ', SST(I,J)
           CALL wrf_message(message)
           NMM_TSK(I,J)=SST(I,J)
           SST(I,J)=0.
        endif

        ENDDO
      ENDDO

      
      

      IF      ( ( model_config_rec%sf_surface_physics(grid%id) .EQ. 1 ) .AND. &
                ( flag_st000010 .EQ. 1 ) ) THEN
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               soiltb(i,j) = st000010(i,j)
            END DO
         END DO
      END IF

  
  
  

      IF ( ( flag_toposoil .EQ. 1 ) ) THEN

        ALLOCATE(HT(ims:ime,jms:jme))

        DO J=jms,jme
          DO I=ims,ime
              HT(I,J)=FIS(I,J)/9.81
          END DO
        END DO
























      END IF

      

      
      
      
      

      IF ( config_flags%surface_input_source .EQ. 1 ) THEN
         vegcat (its,jts) = 0
         soilcat(its,jts) = 0
      END IF

      
      
      

      IF ((soilcat(its,jts) .LT. 0.5) .AND. (vegcat(its,jts) .LT. 0.5)) THEN

         num_veg_cat      = SIZE ( landusef_gc , DIM=3 )
         num_soil_top_cat = SIZE ( soilctop_gc , DIM=3 )
         num_soil_bot_cat = SIZE ( soilcbot_gc , DIM=3 )

        do J=JMS,JME
        do K=1,num_veg_cat
        do I=IMS,IME
           landusef(I,K,J)=landusef_gc(I,J,K)
        enddo
        enddo
        enddo

        do J=JMS,JME
        do K=1,num_soil_top_cat
        do I=IMS,IME
           soilctop(I,K,J)=soilctop_gc(I,J,K)
        enddo
        enddo
        enddo

        do J=JMS,JME
        do K=1,num_soil_bot_cat
        do I=IMS,IME
           soilcbot(I,K,J)=soilcbot_gc(I,J,K)
        enddo
        enddo
        enddo





        write(message,*) 'landmask into process_percent_cat_new'

        CALL wrf_debug(1,message)
        do J=JTE,JTS,-(((JTE-JTS)/20)+1)
        write(message,641) (landmask(I,J),I=ITS,min(ITE,IDE-1),((ITE-ITS)/15)+1)
        CALL wrf_debug(1,message)
        enddo
  641   format(25(f3.0,1x))

         CALL process_percent_cat_new ( landmask , &
                         landusef , soilctop , soilcbot , &
                         isltyp , ivgtyp , &
                         num_veg_cat , num_soil_top_cat , num_soil_bot_cat , &
                         ids , ide , jds , jde , kds , kde , &
                         ims , ime , jms , jme , kms , kme , &
                         its , ite , jts , jte , kts , kte , &
                         model_config_rec%iswater(grid%id) )

         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               vegcat(i,j)  = ivgtyp(i,j)
               soilcat(i,j) = isltyp(i,j)
            END DO
         END DO

       ELSE

         

         IF ( soilcat(its,jts) .GT. 0.5 ) THEN
            DO j = jts, MIN(jde-1,jte)
               DO i = its, MIN(ide-1,ite)
                  isltyp(i,j) = NINT( soilcat(i,j) )
               END DO
            END DO
         END IF
         IF ( vegcat(its,jts) .GT. 0.5 ) THEN
            DO j = jts, MIN(jde-1,jte)
               DO i = its, MIN(ide-1,ite)
                  ivgtyp(i,j) = NINT( vegcat(i,j) )
               END DO
            END DO
         END IF

       ENDIF

        DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)

        IF (SICE(I,J) .lt. 0.1) THEN
          IF (landmask(I,J) .gt. 0.5 .and. sm(I,J) .gt. 0.5) THEN
                write(message,*) 'land mask and SM both > 0.5: ', &
                                           I,J,landmask(I,J),sm(I,J)
                CALL wrf_message(message)
                SM(I,J)=0.
          ELSEIF (landmask(I,J) .lt. 0.5 .and. sm(I,J) .lt. 0.5) THEN
                write(message,*) 'land mask and SM both < 0.5: ', &
                                           I,J, landmask(I,J),sm(I,J)
                CALL wrf_message(message)
                SM(I,J)=1.
          ENDIF
        ELSE
          IF (landmask(I,J) .gt. 0.5 .and. SM(I,J)+SICE(I,J) .gt. 0.9) then
            write(message,*) 'landmask says LAND, SM/SICE say SEAICE: ', I,J
          ENDIF
        ENDIF

           ENDDO
        ENDDO

         DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)

        if (SICE(I,J) .gt. 0.9) then
        ISLTYP(I,J)=16
        IVGTYP(I,J)=24
        endif

            ENDDO
         ENDDO

         DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)

        if (SM(I,J) .lt. 0.5) then
            SST(I,J)=0.
        endif

        if (SM(I,J) .gt. 0.5) then
          if (SST(I,J) .lt. 0.1) then
            SST(I,J)=NMM_TSK(I,J)
          endif
            NMM_TSK(I,J)=0.
        endif

        IF ( (NMM_TSK(I,J)+SST(I,J)) .lt. 200. .or. &
             (NMM_TSK(I,J)+SST(I,J)) .gt. 350. ) THEN
          write(message,*) 'TSK, SST trouble at : ', I,J
          CALL wrf_message(message)
          write(message,*) 'SM, NMM_TSK,SST ', SM(I,J),NMM_TSK(I,J),SST(I,J)
          CALL wrf_message(message)
        ENDIF

            ENDDO
         ENDDO

        write(message,*) 'SM'
        CALL wrf_message(message)

        DO J=min(jde-1,jte),jts,-((jte-jts)/15+1)
          write(message,635) (sm(i,J),I=its,ite,((ite-its)/10)+1)
          CALL wrf_message(message)
        END DO

        write(message,*) 'SST/NMM_TSK'
        CALL wrf_debug(10,message)
        DO J=min(jde-1,jte),jts,-((jte-jts)/15+1)
          write(message,635) (SST(I,J)+NMM_TSK(I,J),I=ITS,min(ide-1,ite),((ite-its)/10)+1)
          CALL wrf_debug(10,message)
        END DO

  635   format(20(f5.1,1x))

         DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)
               IF ( ( landmask(i,j) .LT. 0.5 ) .AND. ( flag_sst .EQ. 1 ) ) THEN
                  soiltb(i,j) = sst(i,j)
               ELSE IF (  landmask(i,j) .GT. 0.5 ) THEN
                  soiltb(i,j) = nmm_tsk(i,j)
               END IF
            END DO
         END DO



   



          DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)
               lu_index(i,j) = ivgtyp(i,j)
            END DO
          END DO

        if (flag_sst .eq. 1) log_flag_sst=.true.
        if (flag_sst .eq. 0) log_flag_sst=.false.

        write(message,*) 'st_input dimensions: ', size(st_input,dim=1), &
                                  size(st_input,dim=2),size(st_input,dim=3)
        CALL wrf_debug(100,message)












   IF (.NOT. ALLOCATED(TG_ALT))ALLOCATE(TG_ALT(grid%sm31:grid%em31,jms:jme))

      TPH0=TPH0D*DTR
      WBD=-(((ide-1)-1)*DLMD)
      WB= WBD*DTR
      SBD=-(((jde-1)/2)*DPHD)
      SB= SBD*DTR
      DLM=DLMD*DTR
      DPH=DPHD*DTR
      TDLM=DLM+DLM
      TDPH=DPH+DPH
      WBI=WB+TDLM
      SBI=SB+TDPH
      EBI=WB+(ide-2)*TDLM
      ANBI=SB+(jde-2)*DPH
      STPH0=SIN(TPH0)
      CTPH0=COS(TPH0)
      TSPH=3600./GRID%DT
         DO J=JTS,min(JTE,JDE-1)
           TLM=WB-TDLM+MOD(J,2)*DLM   
           TPH=SB+float(J-1)*DPH
           STPH=SIN(TPH)
           CTPH=COS(TPH)
           DO I=ITS,MIN(ITE,IDE-1)

        if (I .eq. ITS) THEN
             TLM=TLM+TDLM*ITS
        else
             TLM=TLM+TDLM
        endif

             TERM1=(STPH0*CTPH*COS(TLM)+CTPH0*STPH)
             FP=TWOM*(TERM1)
             F(I,J)=0.5*GRID%DT*FP
           ENDDO
         ENDDO
         DO J=JTS,min(JTE,JDE-1)
           TLM=WB-TDLM+MOD(J+1,2)*DLM   
           TPH=SB+float(J-1)*DPH
           STPH=SIN(TPH)
           CTPH=COS(TPH)
           DO I=ITS,MIN(ITE,IDE-1)

        if (I .eq. ITS) THEN
             TLM=TLM+TDLM*ITS
        else
             TLM=TLM+TDLM
        endif

             TERM1=(STPH0*CTPH*COS(TLM)+CTPH0*STPH)
             TERM1=MIN(TERM1,1.0D0)
             TERM1=MAX(TERM1,-1.0D0)
             APH=ASIN(TERM1)
             TG_ALT(I,J)=TG0+TGA*COS(APH)-FIS(I,J)/3333.
           ENDDO
         ENDDO

            DO j = jts, MIN(jde-1,jte)
               DO i = its, MIN(ide-1,ite)







        if (TG(I,J) .lt. 200.) then   
                                      
                TG(I,J)=TG_ALT(I,J)
        endif

       if (TG(I,J) .lt. 200. .or. TG(I,J) .gt. 320.) then
               write(message,*) 'problematic TG point at : ', I,J
               CALL wrf_message( message )
       endif

        adum2d(i,j)=nmm_tsk(I,J)+sst(I,J)

               END DO
            END DO



        write(message,*) 'call process_soil_real with num_st_levels_input: ',  num_st_levels_input
        CALL wrf_message( message )



  CALL process_soil_real ( adum2d, TG , &
     landmask, sst, &
     st_input, sm_input, sw_input, &
     st_levels_input , sm_levels_input , &
     sw_levels_input , &
     sldpth , dzsoil , stc , smc , sh2o,  &
     flag_sst , flag_soilt000, flag_soilm000, &
     ids , ide , jds , jde , kds , kde , &
     ims , ime , jms , jme , kms , kme , &
     its , ite , jts , jte , kts , kte , &
     model_config_rec%sf_surface_physics(grid%id) , &
     model_config_rec%num_soil_layers ,  &
     model_config_rec%real_data_init_type , &
     num_st_levels_input , num_sm_levels_input , &
     num_sw_levels_input , &
     num_st_levels_alloc , num_sm_levels_alloc , &
     num_sw_levels_alloc )









       lqmi(1:num_soil_top_cat) = &
       (/0.045, 0.057, 0.065, 0.067, 0.034, 0.078, 0.10,     &
         0.089, 0.095, 0.10,  0.070, 0.068, 0.078, 0.0,      &
         0.004, 0.065 /) 




          account_for_zero_soil_moisture : SELECT CASE ( model_config_rec%sf_surface_physics(grid%id) )

             CASE ( LSMSCHEME )
                iicount = 0
                IF      ( FLAG_SM000010 .EQ. 1 ) THEN
                   DO j = jts, MIN(jde-1,jte)
                      DO i = its, MIN(ide-1,ite)
                         IF ((landmask(i,j).gt.0.5) .and. (stc(i,1,j) .gt. 200) .and. &
                            (stc(i,1,j) .lt. 400) .and. (smc(i,1,j) .lt. 0.005)) then
                            write(message,*) 'Noah > Noah: bad soil moisture at i,j = ',i,j,smc(i,:,j)
                            CALL wrf_message(message)
                            iicount = iicount + 1
                            smc(i,:,j) = 0.005
                         END IF
                      END DO
                   END DO
                   IF ( iicount .GT. 0 ) THEN
                   write(message,*) 'Noah -> Noah: total number of small soil moisture locations= ',&
                                                                                        iicount
                   CALL wrf_message(message)
                   END IF
                ELSE IF ( FLAG_SOILM000 .EQ. 1 ) THEN
                   DO j = jts, MIN(jde-1,jte)
                      DO i = its, MIN(ide-1,ite)
                         smc(i,:,j) = smc(i,:,j) + lqmi(isltyp(i,j))
                      END DO
                   END DO
                   DO j = jts, MIN(jde-1,jte)
                      DO i = its, MIN(ide-1,ite)
                         IF ((landmask(i,j).gt.0.5) .and. (stc(i,1,j) .gt. 200) .and. &
                            (stc(i,1,j) .lt. 400) .and. (smc(i,1,j) .lt. 0.004)) then
                            write(message,*) 'RUC -> Noah: bad soil moisture at i,j = ' &
                                                                     ,i,j,smc(i,:,j)
                            CALL wrf_message(message)
                            iicount = iicount + 1
                            smc(i,:,j) = 0.004
                         END IF
                      END DO
                   END DO
                   IF ( iicount .GT. 0 ) THEN
                   write(message,*) 'RUC -> Noah: total number of small soil moisture locations = ',&
                                                                                       iicount
                   CALL wrf_message(message)
                   END IF
                END IF
               CASE ( RUCLSMSCHEME )
                iicount = 0
                IF      ( FLAG_SM000010 .EQ. 1 ) THEN
                   DO j = jts, MIN(jde-1,jte)
                      DO i = its, MIN(ide-1,ite)
                         smc(i,:,j) = MAX ( smc(i,:,j) - lqmi(isltyp(i,j)) , 0. )
                      END DO
                   END DO
                ELSE IF ( FLAG_SOILM000 .EQ. 1 ) THEN
                   
                END IF

          END SELECT account_for_zero_soil_moisture



         DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)
        if (SM(I,J) .gt. 0.5) then
            NMM_TSK(I,J)=0.
        endif
            END DO
         END DO



          DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)

        IF (SICE(I,J) .gt. 0.9) then
          DO L = 1, grid%num_soil_layers
            STC(I,L,J)=271.16    
          END DO
        END IF

        IF (SM(I,J) .gt. 0.9) then
          DO L = 1, grid%num_soil_layers
            STC(I,L,J)=273.16    
          END DO
        END IF

            END DO
          END DO

         DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)

        if (SM(I,J) .lt. 0.1 .and. STC(I,1,J) .lt. 0.1) THEN
          write(message,*) 'troublesome SM,STC,SMC value: ', I,J,SM(I,J), stc(I,1,J),smc(I,1,J)
          CALL wrf_message(message)
        do JJ=J-1,J+1
        do L=1, grid%num_soil_layers
        do II=I-1,I+1

	if (II .ge. its .and. II .le. MIN(ide-1,ite) .and. &
            JJ .ge. jts .and. JJ .le. MIN(jde-1,jte)) then

        STC(I,L,J)=amax1(STC(I,L,J),STC(II,L,JJ))
        cur_smc=SMC(I,L,J)

        if ( SMC(II,L,JJ) .gt. 0.005 .and. SMC(II,L,JJ) .lt. 1.0) then
		aposs_smc=SMC(II,L,JJ)

        if ( cur_smc .eq. 0 ) then
                cur_smc=aposs_smc
                SMC(I,L,J)=cur_smc
        else
                cur_smc=amin1(cur_smc,aposs_smc)
                cur_smc=amin1(cur_smc,aposs_smc)
                SMC(I,L,J)=cur_smc
        endif
	endif

	endif 

        enddo
        enddo
        enddo
        write(message,*) 'STC, SMC(1) now: ',  stc(I,1,J),smc(I,1,J)
        CALL wrf_message(message)
        endif

        if (STC(I,1,J) .lt. 0.1) then
          write(message,*) 'QUITTING DUE TO STILL troublesome STC value: ', I,J, stc(I,1,J),smc(I,1,J)
          call wrf_error_fatal3("module_initialize_real.b",1527,&
message)
        endif

        ENDDO
        ENDDO








       SLDPTH=0.
       SLDPTH(1)=0.1
       SLDPTH(2)=0.3
       SLDPTH(3)=0.6
       SLDPTH(4)=1.0



        do J=jts,min(jte,jde-1)
        do I=its,min(ite,ide-1)
         RES(I,J)=1.
        enddo
        enddo



        HBM2=0.

       do J=jts,min(jte,jde-1)
        do I=its,min(ite,ide-1)

        IF ( (J .ge. 3 .and. J .le. (jde-1)-2) .AND. &
             (I .ge. 2 .and. I .le. (ide-1)-2+mod(J,2)) ) THEN
       HBM2(I,J)=1.
        ENDIF
       enddo
       enddo


        HBM3=0.



       do J=jts,min(jte,jde-1)
          IHWG(J)=mod(J+1,2)-1
          IF (J .ge. 4 .and. J .le. (jde-1)-3) THEN
            IHL=(ids+1)-IHWG(J)
            IHH=(ide-1)-2
            do I=its,min(ite,ide-1)
              IF (I .ge. IHL  .and. I .le. IHH) HBM3(I,J)=1.
            enddo
          ENDIF
        enddo



       VBM2=0.

       do J=jts,min(jte,jde-1)
       do I=its,min(ite,ide-1)

        IF ( (J .ge. 3 .and. J .le. (jde-1)-2)  .AND.  &
             (I .ge. 2 .and. I .le. (ide-1)-1-mod(J,2)) ) THEN

           VBM2(I,J)=1.

        ENDIF

       enddo
       enddo



        VBM3=0.

       do J=jts,min(jte,jde-1)
       do I=its,min(ite,ide-1)

        IF ( (J .ge. 4 .and. J .le. (jde-1)-3)  .AND.  &
             (I .ge. 3-mod(J,2) .and. I .le. (ide-1)-2) ) THEN
         VBM3(I,J)=1.
        ENDIF

       enddo
       enddo

      DTAD=1.0

      DTCF=4.0 

      DY_NMM=ERAD*DPH
      CPGFV=-GRID%DT/(48.*DY_NMM)
      EN= GRID%DT/( 4.*DY_NMM)*DTAD
      ENT=GRID%DT/(16.*DY_NMM)*DTAD

        DO J=jts,nnyp
         KHL2(J)=(IDE-1)*(J-1)-(J-1)/2+2
         KVL2(J)=(IDE-1)*(J-1)-J/2+2
         KHH2(J)=(IDE-1)*J-J/2-1
         KVH2(J)=(IDE-1)*J-(J+1)/2-1
        ENDDO

        TPH=SB-DPH

        DO J=jts,min(jte,jde-1)
         TPH=SB+float(J-1)*DPH
         DXP=ERAD*DLM*COS(TPH)
         DXJ(J)=DXP
         WPDARJ(J)=-W_NMM * &
     ((ERAD*DLM*AMIN1(COS(ANBI),COS(SBI)))**2+DY_NMM**2)/ &
                   (GRID%DT*32.*DXP*DY_NMM)

         CPGFUJ(J)=-GRID%DT/(48.*DXP)
         CURVJ(J)=.5*GRID%DT*TAN(TPH)/ERAD
         FCPJ(J)=GRID%DT/(CP*192.*DXP*DY_NMM)
         FDIVJ(J)=1./(12.*DXP*DY_NMM)


         FADJ(J)=-GRID%DT/(48.*DXP*DY_NMM)*DTAD
         ACDT=GRID%DT*SQRT((ERAD*DLM*AMIN1(COS(ANBI),COS(SBI)))**2+DY_NMM**2)
         CDDAMP=CODAMP*ACDT
         HDACJ(J)=COAC*ACDT/(4.*DXP*DY_NMM)
         DDMPUJ(J)=CDDAMP/DXP
         DDMPVJ(J)=CDDAMP/DY_NMM
        ENDDO

          DO J=JTS,min(JTE,JDE-1)
           TLM=WB-TDLM+MOD(J,2)*DLM
           TPH=SB+float(J-1)*DPH
           STPH=SIN(TPH)
           CTPH=COS(TPH)
           DO I=ITS,MIN(ITE,IDE-1)

        if (I .eq. ITS) THEN
             TLM=TLM+TDLM*ITS
        else
             TLM=TLM+TDLM
        endif

             FP=TWOM*(CTPH0*STPH+STPH0*CTPH*COS(TLM))
             F(I,J)=0.5*GRID%DT*FP

           ENDDO
          ENDDO



      EF4T=.5*GRID%DT/CP
      F4Q =   -GRID%DT*DTAD
      F4D =-.5*GRID%DT*DTAD

       DO L=KDS,KDE-1
        RDETA(L)=1./DETA(L)
        F4Q2(L)=-.25*GRID%DT*DTAD/DETA(L)
       ENDDO

        DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
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

        DO J=JTS, MIN(JDE-1,JTE)

        IF (J.LE.5.OR.J.GE.(JDE-1)-4) THEN

               KHH=(IDE-1)-2+MOD(J,2) 
               DO I=ITS,MIN(IDE-1,ITE)
                 IF (I .ge. 2 .and. I .le. KHH) THEN
                   HDAC(I,J)=HDAC(I,J)* DFC
                 ENDIF
               ENDDO

        ELSE

          KHH=2+MOD(J,2)
               DO I=ITS,MIN(IDE-1,ITE)
                 IF (I .ge. 2 .and. I .le. KHH) THEN
                    HDAC(I,J)=HDAC(I,J)* DFC
                 ENDIF
               ENDDO

          KHH=(IDE-1)-2+MOD(J,2)

               DO I=ITS,MIN(IDE-1,ITE)
                 IF (I .ge. (IDE-1)-2 .and. I .le. KHH) THEN
                   HDAC(I,J)=HDAC(I,J)* DFC
                 ENDIF
               ENDDO
        ENDIF
      ENDDO

      DO J=JTS,min(JTE,JDE-1)
      DO I=ITS,min(ITE,IDE-1)
        DDMPU(I,J)=DDMPUJ(J)*VBM2(I,J)
        DDMPV(I,J)=DDMPVJ(J)*VBM2(I,J)
        HDACV(I,J)=HDACV(I,J)*VBM2(I,J)
      ENDDO
      ENDDO


        DO J=JTS,MIN(JDE-1,JTE)
        IF (J.LE.5.OR.J.GE.JDE-1-4) THEN
          KVH=(IDE-1)-1-MOD(J,2)
          DO I=ITS,min(IDE-1,ITE)
            IF (I .ge. 2 .and.  I .le. KVH) THEN
             DDMPU(I,J)=DDMPU(I,J)*DDFC
             DDMPV(I,J)=DDMPV(I,J)*DDFC
             HDACV(I,J)=HDACV(I,J)* DFC
            ENDIF
          ENDDO
        ELSE
          KVH=3-MOD(J,2)
          DO I=ITS,min(IDE-1,ITE)
           IF (I .ge. 2 .and.  I .le. KVH) THEN
            DDMPU(I,J)=DDMPU(I,J)*DDFC
            DDMPV(I,J)=DDMPV(I,J)*DDFC
            HDACV(I,J)=HDACV(I,J)* DFC
           ENDIF
          ENDDO
          KVH=(IDE-1)-1-MOD(J,2)
          DO I=ITS,min(IDE-1,ITE)
           IF (I .ge. IDE-1-2 .and. I .le. KVH) THEN
            DDMPU(I,J)=DDMPU(I,J)*DDFC
            DDMPV(I,J)=DDMPV(I,J)*DDFC
            HDACV(I,J)=HDACV(I,J)* DFC
           ENDIF
          ENDDO
        ENDIF
      ENDDO

        write(message,*) 'STC(1)'
        CALL wrf_message(message)
        DO J=min(jde-1,jte),jts,-((jte-jts)/15+1)
          write(message,635) (stc(I,1,J),I=its,min(ite,ide-1),(ite-its)/12+1)
          CALL wrf_message(message)
        ENDDO

        write(message,*) 'SMC(1)'
        CALL wrf_message(message)
        DO J=min(jde-1,jte),jts,-((jte-jts)/15+1)
          write(message,635) (smc(I,1,J),I=its,min(ite,ide-1),(ite-its)/12+1)
          CALL wrf_message(message)
        ENDDO

          DO j = jts, MIN(jde-1,jte)
          DO i=  ITS, MIN(IDE-1,ITE)

          if (SM(I,J) .lt. 0.1 .and. SMC(I,1,J) .gt. 0.5 .and. SICE(I,J) .lt. 0.1) then
            write(message,*) 'very moist on land point: ', I,J,SMC(I,1,J)
            CALL wrf_debug(10,message)
          endif

          enddo
        enddo



        IF (wrf_dm_on_monitor()) THEN   

        ALLOCATE(EMJ(JDS:JDE-1),EMTJ(JDS:JDE-1))

        DO J=JDS,JDE-1
         TPH=SB+float(J-1)*DPH
         DXP=ERAD*DLM*COS(TPH)
         EMJ(J)= GRID%DT/( 4.*DXP)*DTAD
         EMTJ(J)=GRID%DT/(16.*DXP)*DTAD
        ENDDO

          JA=0
          DO 161 J=3,5
          JA=JA+1
          KHLA(JA)=2
          KHHA(JA)=(IDE-1)-1-MOD(J+1,2)
 161      EMT(JA)=EMTJ(J)
          DO 162 J=(JDE-1)-4,(JDE-1)-2
          JA=JA+1
          KHLA(JA)=2
          KHHA(JA)=(IDE-1)-1-MOD(J+1,2)
 162      EMT(JA)=EMTJ(J)
          DO 163 J=6,(JDE-1)-5
          JA=JA+1
          KHLA(JA)=2
          KHHA(JA)=2+MOD(J,2)
 163      EMT(JA)=EMTJ(J)
          DO 164 J=6,(JDE-1)-5
          JA=JA+1
          KHLA(JA)=(IDE-1)-2
          KHHA(JA)=(IDE-1)-1-MOD(J+1,2)
 164      EMT(JA)=EMTJ(J)



      JA=0
              DO 171 J=3,5
          JA=JA+1
          KVLA(JA)=2
          KVHA(JA)=(IDE-1)-1-MOD(J,2)
 171      EM(JA)=EMJ(J)
              DO 172 J=(JDE-1)-4,(JDE-1)-2
          JA=JA+1
          KVLA(JA)=2
          KVHA(JA)=(IDE-1)-1-MOD(J,2)
 172      EM(JA)=EMJ(J)
              DO 173 J=6,(JDE-1)-5
          JA=JA+1
          KVLA(JA)=2
          KVHA(JA)=2+MOD(J+1,2)
 173      EM(JA)=EMJ(J)
              DO 174 J=6,(JDE-1)-5
          JA=JA+1
          KVLA(JA)=(IDE-1)-2
          KVHA(JA)=(IDE-1)-1-MOD(J,2)
 174      EM(JA)=EMJ(J)

   696  continue
        ENDIF 

      call NMM_SH2O(IMS,IME,JMS,JME,ITS,NNXP,JTS,NNYP,grid%num_soil_layers,ISLTYP, &
                             SM,SICE,STC,SMC,SH2O)




        IF (   abs(IDE-1-ITE) .eq. 1 ) THEN 
          write(message,*) 'zero phantom winds'
          CALL wrf_message(message)
          DO K=1,KDE-1
            DO J=JDS,JDE-1,2
              IF (J .ge. JTS .and. J .le. JTE) THEN
                u(IDE-1,J,K)=0.
                v(IDE-1,J,K)=0.
              ENDIF
            ENDDO
          ENDDO
        ENDIF

  969   continue

         DO j = jms, jme
            DO i = ims, ime
             fisx=max(fis(i,j),0.)
             Z0(I,J)    =SM(I,J)*Z0SEA+(1.-SM(I,J))*                      &
     &                (0.*Z0MAX+FISx    *FCM+Z0LAND)
            ENDDO
          ENDDO

        write(message,*) 'Z0 over memory, leaving module_initialize_real'
        CALL wrf_message(message)
        DO J=JME,JMS,-((JME-JMS)/20+1)
          write(message,635) (Z0(I,J),I=IMS,IME,(IME-IMS)/14+1)
          CALL wrf_message(message)
        ENDDO


        endif 

        write(message,*) 'leaving init_domain_nmm'
        CALL wrf_message( TRIM(message) )

       write(message,*)'STUFF MOVED TO REGISTRY:',grid%IDTAD,          &
     & grid%NSOIL,grid%NRADL,grid%NRADS,grid%NPHS,grid%NCNVC,grid%sigma
       CALL wrf_message( TRIM(message) )



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

   END SUBROUTINE init_domain_nmm



  SUBROUTINE define_nmm_vertical_coord ( LM, PTSGM, PT, PDTOP,HYBLEVS, &
                                         SG1,DSG1,SGML1,         &
                                         SG2,DSG2,SGML2,DFL, DFRLG            )

        IMPLICIT NONE






        INTEGER ::  LM, LPT2, L
        REAL    ::  PTSGM, PT, PL, PT2, PDTOP
        REAL    ::  RGOG, PSIG,PHYB,PHYBM
        REAL, PARAMETER  :: Rd           =  287.04  
        REAL, PARAMETER :: CP=1004.6,GAMMA=.0065,PRF0=101325.,T0=288.
        REAL, PARAMETER :: g=9.81

        REAL, DIMENSION(LM)   :: DSG,DSG1,DSG2
        REAL, DIMENSION(LM)   :: SGML1,SGML2
        REAL, DIMENSION(LM+1) :: SG1,SG2,HYBLEVS,DFL,DFRLG

        CHARACTER(LEN=255)    :: message

        LPT2=LM+1

        write(message,*) 'pt= ', pt
        CALL wrf_message(message)

        DO L=LM+1,1,-1
          pl=HYBLEVS(L)*(101325.-pt)+pt
          if(pl.lt.ptSGm) LPT2=l
        ENDDO

      IF(LPT2.lt.LM+1) THEN
        pt2=HYBLEVS(LPT2)*(101325.-pt)+pt
      ELSE
        pt2=pt
      ENDIF

      write(message,*) '*** Sigma system starts at ',pt2,' Pa, from level ',LPT2
      CALL wrf_message(message)

      pdtop=pt2-pt

        write(message,*) 'allocating DSG,DSG1,DSG2 as ', LM
        CALL wrf_debug(10,message)

        DSG=-99.

      DO L=1,LM
        DSG(L)=HYBLEVS(L)- HYBLEVS(L+1)
      ENDDO

        DSG1=0.
        DSG2=0.

      DO L=LM,1,-1

       IF(L.ge.LPT2) then
        DSG1(L)=DSG(L)
       ELSE
        DSG2(L)=DSG(L)
       ENDIF

      ENDDO

        SGML1=-99.
        SGML2=-99.

       IF(LPT2.le.LM+1) THEN

        DO L=LM+1,LPT2,-1
        SG2(L)=0.
        ENDDO

       DO L=LPT2,2,-1
        SG2(L-1)=SG2(L)+DSG2(L-1)
       ENDDO

        DO L=LPT2-1,1,-1
        SG2(L)=SG2(L)/SG2(1)
        ENDDO
        SG2(1)=1.

       DO L=LPT2-1,1,-1
        DSG2(L)=SG2(L)-SG2(L+1)
        SGML2(l)=(SG2(l)+SG2(l+1))*0.5
       ENDDO

      ENDIF

      DO L=LM,LPT2,-1
        DSG2(L)=0.
        SGML2(L)=0.
      ENDDO



        SG1(LM+1)=0.

      DO L=LM+1,LPT2,-1
       SG1(L-1)=SG1(L)+DSG1(L-1)
      ENDDO

      DO L=LM,LPT2,-1
       SG1(L)=SG1(L)/SG1(LPT2-1)
      ENDDO

        SG1(LPT2-1)=1.

       do l=LPT2-2,1,-1
        SG1(l)=1.
       enddo


      DO L=LM,LPT2,-1
       DSG1(L)=SG1(L)-SG1(L+1)
       SGML1(L)=(SG1(L)+SG1(L+1))*0.5
      ENDDO

      DO L=LPT2-1,1,-1
               DSG1(L)=0.
               SGML1(L)=1.
      ENDDO

 1000 format('l,hyblevs,psig,SG1,SG2,phyb,phybm')
 1100 format(' ',i4,f7.4,f10.2,2f7.4,2f10.2)

      write(message,1000)
      CALL wrf_debug(100,message)

      do l=1,LM+1
        psig=HYBLEVS(L)*(101325.-pt)+pt
        phyb=SG1(l)*pdtop+SG2(l)*(101325.-pdtop-pt)+pt
        if(l.lt.LM+1) then
          phybm=SGML1(l)*pdtop+SGML2(l)*(101325.-pdtop-pt)+pt
        else
          phybm=-99.
        endif

        write(message,1100) l,HYBLEVS(L),psig &
                      ,SG1(l),SG2(l),phyb,phybm
        CALL wrf_debug(100,message)
      enddo


  632   format(f9.6)

       write(message,*) 'SG1'
       CALL wrf_debug(100,message)
       do L=LM+1,1,-1
       write(message,632) SG1(L)
       CALL wrf_debug(100,message)
       enddo

       write(message,*) 'SG2'
       CALL wrf_debug(100,message)
       do L=LM+1,1,-1
       write(message,632) SG2(L)
       CALL wrf_debug(100,message)
       enddo

       write(message,*) 'DSG1'
       CALL wrf_debug(100,message)
       do L=LM,1,-1
       write(message,632) DSG1(L)
       CALL wrf_debug(100,message)
       enddo

       write(message,*) 'DSG2'
       CALL wrf_debug(100,message)
       do L=LM,1,-1
       write(message,632) DSG2(L)
       CALL wrf_debug(100,message)
       enddo

       write(message,*) 'SGML1'
       CALL wrf_debug(100,message)
       do L=LM,1,-1
       write(message,632) SGML1(L)
       CALL wrf_debug(100,message)
       enddo

       write(message,*) 'SGML2'
       CALL wrf_debug(100,message)
       do L=LM,1,-1
       write(message,632) SGML2(L)
       CALL wrf_debug(100,message)
       enddo

      rgog=(rd*gamma)/g
      DO L=1,LM+1
        DFL(L)=g*T0*(1.-((pt+SG1(L)*pdtop+SG2(L)*(101325.-pt2)) &
                       /101325.)**rgog)/gamma
        DFRLG(L)=DFL(L)/g
       write(message,*) 'L, DFL(L): ', L, DFL(L)
       CALL wrf_debug(100,message)
      ENDDO

  END SUBROUTINE define_nmm_vertical_coord





  SUBROUTINE compute_nmm_surfacep ( TERRAIN_HGT_T, Z3D_IN, PRESS3D_IN, T3D_IN   &
     &,                             psfc_out,generic           &
     &,                             IDS,IDE,JDS,JDE,KDS,KDE             &
     &,                             IMS,IME,JMS,JME,KMS,KME             &
     &,                             ITS,ITE,JTS,JTE,KTS,KTE  )

	
       IMPLICIT NONE

       real, allocatable:: dum2d(:,:),DUM2DB(:,:)

       integer :: IDS,IDE,JDS,JDE,KDS,KDE
       integer :: IMS,IME,JMS,JME,KMS,KME
       integer :: ITS,ITE,JTS,JTE,KTS,KTE,Ilook,Jlook
       integer :: I,J,II,generic,L,KINSERT,K,bot_lev,LL
       integer :: IHE(JMS:JME),IHW(JMS:JME), loopinc,iloopinc
	
       real :: TERRAIN_HGT_T(IMS:IME,JMS:JME)
       real :: Z3D_IN(IMS:IME,JMS:JME,generic)
       real :: T3D_IN(IMS:IME,JMS:JME,generic)
       real :: PRESS3D_IN(IMS:IME,JMS:JME,generic)
       real :: PSFC_IN(IMS:IME,JMS:JME),TOPO_IN(IMS:IME,JMS:JME)
       real :: psfc_out(IMS:IME,JMS:JME),rincr(IMS:IME,JMS:JME)
       real :: dif1,dif2,dif3,dif4,dlnpdz,BOT_INPUT_HGT,BOT_INPUT_PRESS,dpdz,rhs
       real :: zin(generic),pin(generic)

       character (len=255) :: message
	
       logical :: DEFINED_PSFC(IMS:IME,JMS:JME), DEFINED_PSFCB(IMS:IME,JMS:JME)



	Ilook=25
        Jlook=25

       DO j = JMS, JME
          IHE(J)=MOD(J+1,2)
          IHW(J)=IHE(J)-1
       ENDDO

       DO J=JMS,JME
       DO I=IMS,IME
          DEFINED_PSFC(I,J)=.FALSE.
          DEFINED_PSFCB(I,J)=.FALSE.
        IF (PRESS3D_IN(I,J,1) .ne. 200100.) THEN
          PSFC_IN(I,J)=PRESS3D_IN(I,J,1)
          TOPO_IN(I,J)=Z3D_IN(I,J,1)
        ELSE
          PSFC_IN(I,J)=PRESS3D_IN(I,J,2)
          TOPO_IN(I,J)=Z3D_IN(I,J,2)
        ENDIF
       ENDDO
       ENDDO



        II_loop: do II=1,8

        CYCLE II_loop

	do J=JTS+1,min(JTE,JDE-1)-1
         do I=ITS+1,min(ITE,IDE-1)-1
         rincr(I,J)=0.

       if (PSFC_IN(I,J) .gt. 100000.          .and. &
           PSFC_IN(I+IHE(J),J+1) .gt. 100000. .and. &
           PSFC_IN(I+IHE(J),J-1) .gt. 100000. .and. &
           PSFC_IN(I+IHW(J),J+1) .gt. 100000. .and. &
           PSFC_IN(I+IHW(J),J-1) .gt. 100000. ) then

       dif1=abs(PSFC_IN(I,J)-PSFC_IN(I+IHE(J),J+1))
       dif2=abs(PSFC_IN(I,J)-PSFC_IN(I+IHE(J),J-1))
       dif3=abs(PSFC_IN(I,J)-PSFC_IN(I+IHW(J),J+1))
       dif4=abs(PSFC_IN(I,J)-PSFC_IN(I+IHW(J),J-1))

        if (max(dif1,dif2,dif3,dif4) .lt. 200. .and. TOPO_IN(I,J).le. 0.5 .and. &
            TOPO_IN(I+IHE(J),J+1) .le. 0.5 .and. &
            TOPO_IN(I+IHW(J),J+1) .le. 0.5 .and. &
            TOPO_IN(I+IHE(J),J-1) .le. 0.5 .and. &
            TOPO_IN(I+IHW(J),J-1) .lt. 0.5) then

        rincr(I,J)=0.125*( 4.*PSFC_IN(I,J)+ &
                            PSFC_IN(I+IHE(J),J+1)+PSFC_IN(I+IHE(J),J-1)+ &
                            PSFC_IN(I+IHW(J),J+1)+PSFC_IN(I+IHW(J),J-1) ) &
                          - PSFC_IN(I,J)






         endif
         endif

        ENDDO
        ENDDO

       DO J=JTS+1,min(JTE,JDE-1)-1
         DO I=ITS+1,min(ITE,IDE-1)-1
           PSFC_IN(I,J)=PSFC_IN(I,J) + rincr(I,J)
         ENDDO
       ENDDO




         end do II_loop

       ALLOCATE(DUM2D(IMS:IME,JMS:JME))

       DO J=JMS,JME
        DO I=IMS,IME
         DUM2D(I,J)=-9.
        END DO
       END DO

       DO J=JTS,min(JTE,JDE-1)
        I_loop: DO I=ITS,min(ITE,IDE-1)

         IF (PSFC_IN(I,J) .lt. 0.1) THEN
           write(message,*) 'QUITTING BECAUSE I,J, PSFC_IN: ', I,J,PSFC_IN(I,J)
           call wrf_error_fatal3("module_initialize_real.b",2244,&
message)
         ENDIF

         BOT_INPUT_PRESS=PSFC_IN(I,J)
         BOT_INPUT_HGT=TOPO_IN(I,J)

         IF (I .eq. Ilook .AND. J .eq. Jlook) THEN

	   write(message,*) ' TERRAIN_HGT_T: ', I,J, TERRAIN_HGT_T(I,J)
           CALL wrf_message(message)
	   write(message,*) ' PSFC_IN, TOPO_IN: ', &
                            I, J, PSFC_IN(I,J),TOPO_IN(I,J)
           CALL wrf_message(message)

           DO L=1,generic
	     write(message,*) ' L,PRESS3D_IN, Z3D_IN: ', &
                             I,J,L, PRESS3D_IN(I,J,L),Z3D_IN(I,J,L)
             CALL wrf_debug(10,message)
           END DO
         ENDIF

       DO L=2,generic-1

         IF ( PRESS3D_IN(i,j,L) .gt. PSFC_IN(I,J) .AND.  &
             Z3D_IN(I,J,L) .lt. TERRAIN_HGT_T(I,J) .AND. &
             Z3D_IN(I,J,L+1) .gt. TERRAIN_HGT_T(I,J) ) THEN

           BOT_INPUT_PRESS=PRESS3D_IN(i,j,L)
           BOT_INPUT_HGT=Z3D_IN(I,J,L)







          ENDIF
       END DO	



       IF ( PRESS3D_IN(i,j,1) .ne. 200100. .AND. &
          (PSFC_IN(I,J) .gt. PRESS3D_IN(i,j,2) .OR. &
           TOPO_IN(I,J) .lt. Z3D_IN(I,J,2)) ) THEN        

         IF (J .eq. JTS .AND. I .eq. ITS) THEN
            write(message,*) 'hydro check - should only be for isobaric input'
            CALL wrf_message(message)
         ENDIF

	 IF (Z3D_IN(I,J,2) .ne. TOPO_IN(I,J)) THEN
           dpdz=(PRESS3D_IN(i,j,2)-PSFC_IN(I,J))/(Z3D_IN(I,J,2)-TOPO_IN(I,J))
           rhs=-9.81*((PRESS3D_IN(i,j,2)+ PSFC_IN(I,J))/2.)/(287.04* T3D_IN(I,J,2))

	   IF ( abs(PRESS3D_IN(i,j,2)-PSFC_IN(I,J)) .gt. 290.) THEN
             IF (dpdz .lt. 1.05*rhs .OR. dpdz .gt. 0.95*rhs) THEN
                write(message,*) 'I,J,P(2),Psfc,Z(2),Zsfc: ', &
                    I,J,PRESS3D_IN(i,j,2),PSFC_IN(I,J),Z3D_IN(I,J,2),TOPO_IN(I,J)
               IF (mod(I,5).eq.0 .AND. mod(J,5).eq.0) CALL wrf_debug(50,message)
	      CYCLE I_loop
             ENDIF

           ENDIF

         ELSE 

	  IF (PRESS3D_IN(i,j,2) .eq. PSFC_IN(I,J)) THEN


          ELSE



	    CYCLE I_loop
	  ENDIF

         ENDIF

         IF ( abs(PRESS3D_IN(i,j,2)-PSFC_IN(I,J)) .gt. 290.) THEN
           IF (PRESS3D_IN(i,j,2) .lt. PSFC_IN(I,J) .and. &
                          Z3D_IN(I,J,2) .lt. TOPO_IN(I,J)) THEN


	     CYCLE I_loop
           ELSEIF (PRESS3D_IN(i,j,2) .gt. PSFC_IN(I,J) .AND.  &
                  Z3D_IN(I,J,2) .gt. TOPO_IN(I,J)) THEN


             CYCLE I_loop
           ENDIF
         ENDIF
       ENDIF



        DO L=3,6
          IF ( PRESS3D_IN(i,j,1) .ne. 200100. .AND. &
             (((PSFC_IN(I,J)-PRESS3D_IN(i,j,L)) .lt. 400.) .OR. &
               TOPO_IN(I,J) .lt. Z3D_IN(I,J,L))) then

	    IF (Z3D_IN(I,J,L) .ne. TOPO_IN(I,J)) THEN
              dpdz=(PRESS3D_IN(i,j,L)-PSFC_IN(I,J))/ &
                   (Z3D_IN(I,J,L)-TOPO_IN(I,J))
              rhs=-9.81*((PRESS3D_IN(i,j,L)+ PSFC_IN(I,J))/2.)/ &
                        (287.04*T3D_IN(I,J,L))
              IF ( abs(PRESS3D_IN(i,j,L)-PSFC_IN(I,J)) .gt. 290.) THEN
                IF (dpdz .lt. 1.05*rhs .or. dpdz .gt. 0.95*rhs) THEN
                  write(message,*) 'I,J,L,Piso,Psfc,Ziso,Zsfc: ', &
                                    I,J,L,PRESS3D_IN(i,j,L),PSFC_IN(I,J),&
                                    Z3D_IN(I,J,L),TOPO_IN(I,J)
                  IF (mod(I,5).eq.0 .AND. mod(J,5).eq.0) &
                                               CALL wrf_debug(50,message)
	          CYCLE I_loop
                ENDIF
              ENDIF
            ELSE
	      IF (PRESS3D_IN(i,j,2) .eq. PSFC_IN(I,J)) THEN


              ELSE
	        CYCLE I_loop
              ENDIF
            ENDIF
          ENDIF

	  IF ( abs(PRESS3D_IN(i,j,L)-PSFC_IN(I,J)) .gt. 290.) THEN
            IF (PRESS3D_IN(i,j,L) .lt. PSFC_IN(I,J) .AND. &
                    Z3D_IN(I,J,L) .lt. TOPO_IN(I,J)) THEN
              CYCLE I_loop
            ELSEIF (PRESS3D_IN(i,j,L) .gt. PSFC_IN(I,J) .AND.  &
                    Z3D_IN(I,J,L) .gt. TOPO_IN(I,J)) THEN
             CYCLE I_loop
            ENDIF
          ENDIF
        END DO


        IF (TERRAIN_HGT_T(I,J) .eq. BOT_INPUT_HGT ) THEN
           dum2d(I,J)=BOT_INPUT_PRESS

	  IF (BOT_INPUT_HGT .ne. 0. .and. (BOT_INPUT_HGT-INT(BOT_INPUT_HGT) .ne. 0.) ) THEN
	    write(message,*) 'with BOT_INPUT_HGT: ', BOT_INPUT_HGT, &
                             'set dum2d to bot_input_pres: ', I,J,dum2d(I,J)
            CALL wrf_message(message)
          ENDIF

          IF (dum2d(I,J) .lt. 50000. .OR. dum2d(I,J) .gt. 109000.) THEN
            write(message,*) 'bad dum2d(a): ', I,J,DUM2D(I,J)
            CALL wrf_message(message)
          ENDIF

        ELSEIF (TERRAIN_HGT_T(I,J) .lt. BOT_INPUT_HGT ) THEN



          IF ( BOT_INPUT_PRESS-PRESS3D_IN(I,J,2) .gt. 500. ) THEN
            dlnpdz= (log(BOT_INPUT_PRESS)-log(PRESS3D_IN(i,j,2)) ) / &
                     (BOT_INPUT_HGT-Z3D_IN(i,j,2))
            IF (I .eq. Ilook .and. J .eq. Jlook) THEN
              write(message,*) 'I,J,dlnpdz(a): ', I,J,dlnpdz
              CALL wrf_message(message)
            ENDIF

          ELSE


            IF ( abs(BOT_INPUT_PRESS - PRESS3D_IN(i,j,3)) .gt. 290. ) THEN

              dlnpdz= (log(BOT_INPUT_PRESS)-log(PRESS3D_IN(i,j,3)) ) / &
                      (BOT_INPUT_HGT-Z3D_IN(i,j,3))

              IF (I .eq. Ilook .and. J .eq. Jlook) then
               write(message,*) 'p diff: ', BOT_INPUT_PRESS, PRESS3D_IN(i,j,3)
               CALL wrf_message(message)
               write(message,*) 'z diff: ', BOT_INPUT_HGT, Z3D_IN(i,j,3)
               CALL wrf_message(message)
              ENDIF
	
            ELSE



              FIND_THICK:  DO LL=4,7
               IF( abs(BOT_INPUT_PRESS - PRESS3D_IN(i,j,LL)) .gt. 290.) THEN
                 dlnpdz= (log(BOT_INPUT_PRESS)-log(PRESS3D_IN(i,j,LL)) ) / &
                   (BOT_INPUT_HGT-Z3D_IN(i,j,LL))
                EXIT FIND_THICK
               ENDIF
              END DO FIND_THICK

            ENDIF

          ENDIF

        dum2d(I,J)= exp(log(BOT_INPUT_PRESS) + dlnpdz * &
                        (TERRAIN_HGT_T(I,J) - BOT_INPUT_HGT) )

         IF (dum2d(I,J) .lt. 50000. .or. dum2d(I,J) .gt. 108000.) THEN
           write(message,*) 'bad dum2d(b): ', I,J,DUM2D(I,J)
           CALL wrf_message(message)
           write(message,*) 'BOT_INPUT_PRESS, dlnpdz, TERRAIN_HGT_T, BOT_INPUT_HGT: ', &
                BOT_INPUT_PRESS, dlnpdz, TERRAIN_HGT_T(I,J), BOT_INPUT_HGT
           CALL wrf_message(message)
           write(message,*) 'Z3D_IN: ', Z3D_IN(I,J,1:10)
           CALL wrf_message(message)
           write(message,*) 'PRESS3D_IN: ', PRESS3D_IN(I,J,1:10)
           CALL wrf_message(message)
         ENDIF

        ELSE 

          DO L=2,generic-1
            IF (TERRAIN_HGT_T(I,J) .gt. Z3D_IN(i,j,L) .AND. &
                  TERRAIN_HGT_T(I,J) .lt. Z3D_IN(i,j,L+1) ) THEN
               dlnpdz= (log(PRESS3D_IN(i,j,l))-log(PRESS3D_IN(i,j,L+1)) ) / &
                       (Z3D_IN(i,j,l)-Z3D_IN(i,j,L+1))
               dum2d(I,J)= log(PRESS3D_IN(i,j,l)) +   &
                           dlnpdz * (TERRAIN_HGT_T(I,J) - Z3D_IN(i,j,L) )
               dum2d(i,j)=exp(dum2d(i,j))
               IF (dum2d(I,J) .lt. 50000. .or. dum2d(I,J) .gt. 108000.) THEN
                 write(message,*) 'bad dum2d(c): ', I,J,DUM2D(I,J)
                 CALL wrf_message(message)
               ENDIF
            ENDIF
          ENDDO


          IF (dum2d(I,J) .eq. -9 .AND. BOT_INPUT_HGT .lt. TERRAIN_HGT_T(I,J) &
              .AND. TERRAIN_HGT_T(I,J) .lt. Z3D_IN(I,J,2)) then

            IF (mod(I,50) .eq. 0 .AND. mod(J,50) .eq. 0) THEN
              write(message,*) 'I,J,BOT_INPUT_HGT, bot_pres, TERRAIN_HGT_T: ',  &
                 I,J,BOT_INPUT_HGT, BOT_INPUT_PRESS, TERRAIN_HGT_T(I,J)
              CALL wrf_message(message)
            ENDIF

            dlnpdz= (log(PSFC_IN(i,j))-log(PRESS3D_IN(i,j,2)) ) / &
                    (TOPO_IN(i,j)-Z3D_IN(i,j,2))
            dum2d(I,J)= log(PSFC_IN(i,j)) +   &
                        dlnpdz * (TERRAIN_HGT_T(I,J) - TOPO_IN(i,j) )
            dum2d(i,j)= exp(dum2d(i,j))
            IF (dum2d(I,J) .lt. 50000. .or. dum2d(I,J) .gt. 108000.) THEN
              write(message,*) 'bad dum2d(d): ', I,J,DUM2D(I,J)
              CALL wrf_message(message)
            ENDIF
          ENDIF

          IF (dum2d(I,J) .eq. -9.) THEN
            write(message,*) 'must have flukey situation in new ', I,J
            CALL wrf_message(message)
            write(message,*) 'I,J,BOT_INPUT_HGT, bot_pres, TERRAIN_HGT_T: ',  &
                       I,J,BOT_INPUT_HGT, BOT_INPUT_PRESS, TERRAIN_HGT_T(I,J)
            CALL wrf_message(message)

            DO L=1,generic-1
              IF ( TERRAIN_HGT_T(I,J) .eq. Z3D_IN(i,j,L) ) THEN

                dum2d(i,j)=PRESS3D_IN(I,J,L)
                IF (dum2d(I,J) .lt. 50000. .or. dum2d(I,J) .gt. 108000.) THEN
                  write(message,*) 'bad dum2d(e): ', I,J,DUM2D(I,J)
                  CALL wrf_message(message)
                ENDIF
              ENDIF
            ENDDO

            IF ( TERRAIN_HGT_T(I,J) .eq. TOPO_IN(I,J)) THEN
              dum2d(I,J)=PSFC_IN(I,J)
              IF (dum2d(I,J) .lt. 50000. .or. dum2d(I,J) .gt. 108000.) THEN
                write(message,*) 'bad dum2d(f): ', I,J,DUM2D(I,J)
                CALL wrf_message(message)
              ENDIF
             write(message,*) 'matched input topo, psfc: ', I,J,TOPO_IN(I,J),PSFC_IN(I,J)
             CALL wrf_message(message)
            ENDIF

            IF (dum2d(I,J) .eq. -9.) THEN
              CALL wrf_error_fatal3("module_initialize_real.b",2521,&
"quitting due to undefined surface pressure")
            ENDIF
          ENDIF

          DEFINED_PSFC(I,J)=.TRUE.

	  IF (I .eq. Ilook .AND. J .eq. Jlook) THEN
	    write(message,*) 'newstyle psfc: ', I,J,dum2d(I,J)
            CALL wrf_message(message)
          ENDIF

        ENDIF

        ENDDO I_loop
        ENDDO

        write(message,*) 'psfc points (new style)'
        CALL wrf_message(message)
	loopinc=max( (JTE-JTS)/20,1)
	iloopinc=max( (ITE-ITS)/10,1)

        DO J=min(JTE,JDE-1),JTS,-loopinc
          write(message,633) (dum2d(I,J)/100.,I=ITS,min(ITE,IDE-1),iloopinc)
        END DO

  633   format(35(f5.0,1x))

        write(message,*) 'PSFC extremes (new style)'
        CALL wrf_message(message)
        write(message,*) minval(dum2d,MASK=DEFINED_PSFC),maxval(dum2d,MASK=DEFINED_PSFC)
        CALL wrf_message(message)



        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)

          IF (DEFINED_PSFC(I,J) .AND. dum2d(I,J) .lt. 50000. ) THEN
            IF (TERRAIN_HGT_T(I,J) .gt. 4500.) THEN
              WRITE(message,*) 'low surface pressure allowed because surface height is: ', TERRAIN_HGT_T(I,J)
              CALL wrf_debug(2,message)
            ELSE
              CALL wrf_error_fatal3("module_initialize_real.b",2564,&
"quit due to unrealistic surface pressure")
            ENDIF
          ENDIF

          IF (DEFINED_PSFC(I,J) .AND. dum2d(I,J) .gt. 108000. ) THEN
            IF (TERRAIN_HGT_T(I,J) .lt. -10.) THEN
              WRITE(message,*) 'high surface pressure allowed because surface height is: ', TERRAIN_HGT_T(I,J)
              CALL wrf_debug(2,message)
            ELSE
              CALL wrf_error_fatal3("module_initialize_real.b",2574,&
"quit due to unrealistic surface pressure")
            ENDIF
          ENDIF

         END DO
        END DO





       ALLOCATE (DUM2DB(IMS:IME,JMS:JME))
       DO J=JMS,JME
        DO I=IMS,IME
         DUM2DB(I,J)=-9.
        END DO
       END DO

       DO J=JTS,min(JTE,JDE-1)
       DO I=ITS,min(ITE,IDE-1)

        IF (TERRAIN_HGT_T(I,J) .lt. Z3D_IN(i,j,2)) THEN 

          IF ( abs(PRESS3D_IN(i,j,2)-PRESS3D_IN(i,j,3)) .gt. 290.) THEN
            dlnpdz= (log(PRESS3D_IN(i,j,2))-log(PRESS3D_IN(i,j,3)) ) / &
                    (Z3D_IN(i,j,2)-Z3D_IN(i,j,3))
          ELSE
            dlnpdz= (log(PRESS3D_IN(i,j,2))-log(PRESS3D_IN(i,j,4)) ) / &
                    (Z3D_IN(i,j,2)-Z3D_IN(i,j,4))
          ENDIF

          DUM2DB(I,J)= exp( log(PRESS3D_IN(i,j,2)) + dlnpdz * &
                           (TERRAIN_HGT_T(I,J) - Z3D_IN(i,j,2)) )

	  IF (I .eq. Ilook .and. J .eq. Jlook) THEN
	    write(message,*) 'I,K, trad: dlnpdz, press_in(2), terrain_t, Z3D_IN(2): ', I,J,dlnpdz, &
                             PRESS3D_IN(i,j,2), TERRAIN_HGT_T(I,J), Z3D_IN(i,j,2)
            CALL wrf_message(message)
          ENDIF

          DEFINED_PSFCB(i,j)=.true.

        ELSEIF (TERRAIN_HGT_T(I,J) .gt. Z3D_IN(i,j,2)) THEN 

        DO L=2,generic-1
          IF (TERRAIN_HGT_T(I,J) .gt. Z3D_IN(i,j,L) .AND. &
              TERRAIN_HGT_T(I,J) .lt. Z3D_IN(i,j,L+1) ) THEN

            dlnpdz= (log(PRESS3D_IN(i,j,l))-log(PRESS3D_IN(i,j,L+1)) ) / &
                    (Z3D_IN(i,j,l)-Z3D_IN(i,j,L+1))

            DUM2DB(I,J)= log(PRESS3D_IN(i,j,l)) +   &
                         dlnpdz * (TERRAIN_HGT_T(I,J) - Z3D_IN(i,j,L) )
            DUM2DB(i,j)=exp(DUM2DB(i,j))

	    DEFINED_PSFCB(i,j)=.true.

            IF (DUM2DB(I,J) .lt. 13000.) THEN
              write(message,*) 'I,J,L,terrain,Z3d(L),z3d(L+1),p3d(L),p3d(l+1): ', I,J,L, &
                                TERRAIN_HGT_T(I,J),Z3D_IN(I,J,L),Z3D_IN(I,J,L+1),PRESS3D_IN(I,J,L), &
                                PRESS3D_IN(I,J,L+1)
              CALL wrf_error_fatal3("module_initialize_real.b",2636,&
message)
            ENDIF
          ENDIF
        ENDDO

        ELSEIF (TERRAIN_HGT_T(I,J) .eq. Z3D_IN(i,j,2)) THEN
          DUM2DB(i,j)=PRESS3D_IN(I,J,2)
	  DEFINED_PSFCB(i,j)=.true.
        ENDIF

        IF (DUM2DB(I,J) .eq. -9.) THEN
          write(message,*) 'must have flukey situation in trad ', I,J
          CALL wrf_message(message)
          DO L=1,generic-1
            IF ( TERRAIN_HGT_T(I,J) .eq. Z3D_IN(i,j,L) ) THEN
              DUM2DB(i,j)=PRESS3D_IN(I,J,L)
              DEFINED_PSFCB(i,j)=.true.
            ENDIF
          ENDDO
        ENDIF

        IF (DUM2DB(I,J) .eq. -9.) THEN
          write(message,*) 'HOPELESS PSFC, I QUIT'
          CALL wrf_error_fatal3("module_initialize_real.b",2660,&
message)
        ENDIF

	if (I .eq. Ilook .and. J .eq. Jlook) THEN
	  write(message,*) ' traditional psfc: ', I,J,DUM2DB(I,J)
          CALL wrf_message(message)
        ENDIF

       ENDDO
       ENDDO

       write(message,*) 'psfc points (traditional)'
       CALL wrf_message(message)
       DO J=min(JTE,JDE-1),JTS,-loopinc
         write(message,633) (DUM2DB(I,J)/100.,I=its,min(ite,IDE-1),iloopinc)
         CALL wrf_message(message)
       ENDDO

       write(message,*) 'PSFC extremes (traditional)'
       CALL wrf_message(message)
       write(message,*) minval(DUM2DB,MASK=DEFINED_PSFCB),maxval(DUM2DB,MASK=DEFINED_PSFCB)
       CALL wrf_message(message)

        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)

          IF (DEFINED_PSFCB(I,J) .AND. dum2db(I,J) .lt. 50000. ) THEN
            IF (TERRAIN_HGT_T(I,J) .gt. 4500.) THEN
              WRITE(message,*) 'low surface pressure allowed because surface height is: ', TERRAIN_HGT_T(I,J)
              CALL wrf_debug(2,message)
            ELSE
              CALL wrf_error_fatal3("module_initialize_real.b",2692,&
"quit due to unrealistic surface pressure")
            ENDIF
          ENDIF

          IF (DEFINED_PSFCB(I,J) .AND. dum2db(I,J) .gt. 108000. ) THEN
            IF (TERRAIN_HGT_T(I,J) .lt. -10.) THEN
              WRITE(message,*) 'high surface pressure allowed because surface height is: ', TERRAIN_HGT_T(I,J)
              CALL wrf_debug(2,message)
            ELSE
              CALL wrf_error_fatal3("module_initialize_real.b",2702,&
"quit due to unrealistic surface pressure")
            ENDIF
          ENDIF











         ENDDO
        ENDDO



       DO J=JTS,min(JTE,JDE-1)
       DO I=ITS,min(ITE,IDE-1)
         IF (DEFINED_PSFCB(I,J) .and. DEFINED_PSFC(I,J)) THEN

          IF (  abs(dum2d(I,J)-DUM2DB(I,J)) .gt. 400.) THEN
	     write(message,*) 'BIG DIFF I,J, dum2d, DUM2DB: ', I,J,dum2d(I,J),DUM2DB(I,J)
             CALL wrf_message(message)
          ENDIF


          psfc_out(I,J)=0.5*(dum2d(I,J)+DUM2DB(I,J))

         ELSEIF (DEFINED_PSFC(I,J)) THEN
           psfc_out(I,J)=dum2d(I,J)
         ELSEIF (DEFINED_PSFCB(I,J)) THEN
           psfc_out(I,J)=DUM2DB(I,J)
         ELSE
	   write(message,*) 'I,J,dum2d,DUM2DB: ', I,J,dum2d(I,J),DUM2DB(I,J)
           CALL wrf_message(message)
	   write(message,*) 'I,J,DEFINED_PSFC(I,J),DEFINED_PSFCB(I,J): ', I,J,DEFINED_PSFC(I,J),DEFINED_PSFCB(I,J)
           CALL wrf_message(message)
	   call wrf_error_fatal3("module_initialize_real.b",2743,&
"psfc_out completely undefined")
         ENDIF

	IF (I .eq. Ilook .AND. J .eq. Jlook) THEN
	  write(message,*) ' combined psfc: ', I,J,psfc_out(I,J)
          CALL wrf_message(message)
        ENDIF

          IF (PSFC_OUT(I,J) .lt. 50000. ) THEN
            IF (TERRAIN_HGT_T(I,J) .gt. 4500.) THEN
              WRITE(message,*) 'low surface pressure allowed because surface height is: ', TERRAIN_HGT_T(I,J)
              CALL wrf_debug(2,message)
            ELSE
	      write(message,*) 'possibly bad combo on psfc_out: ', I,J, psfc_out(I,J)
              CALL wrf_debug(2,message)
	      write(message,*) 'DEFINED_PSFC, dum2d: ', DEFINED_PSFC(I,J),dum2d(I,J)
              CALL wrf_debug(2,message)
	      write(message,*) 'DEFINED_PSFCB, DUM2DB: ', DEFINED_PSFCB(I,J),DUM2DB(I,J)
              CALL wrf_debug(2,message)
              CALL wrf_error_fatal3("module_initialize_real.b",2763,&
"quit due to unrealistic surface pressure")
            ENDIF
          ENDIF

          IF (PSFC_OUT(I,J) .gt. 108000. ) THEN
            IF (TERRAIN_HGT_T(I,J) .lt. -10.) THEN
              WRITE(message,*) 'high surface pressure allowed because surface height is: ', TERRAIN_HGT_T(I,J)
              CALL wrf_debug(2,message)
            ELSE
	      write(message,*) 'possibly bad combo on psfc_out: ', I,J, psfc_out(I,J)
              CALL wrf_debug(2,message)
	      write(message,*) 'DEFINED_PSFC, dum2d: ', DEFINED_PSFC(I,J),dum2d(I,J)
              CALL wrf_debug(2,message)
	      write(message,*) 'DEFINED_PSFCB, DUM2DB: ', DEFINED_PSFCB(I,J),DUM2DB(I,J)
              CALL wrf_debug(2,message)
              CALL wrf_error_fatal3("module_initialize_real.b",2779,&
"quit due to unrealistic surface pressure")
            ENDIF
          ENDIF

       ENDDO
       ENDDO
	
	deallocate(dum2d,dum2db)

	END SUBROUTINE compute_nmm_surfacep




      SUBROUTINE compute_3d_pressure(psfc_out,SGML1,SGML2,pdtop,pt       &
     &,                              pd,p3d_out                          &
     &,                              IDS,IDE,JDS,JDE,KDS,KDE             &
     &,                              IMS,IME,JMS,JME,KMS,KME             &
     &,                              ITS,ITE,JTS,JTE,KTS,KTE )


        INTEGER          :: IDS,IDE,JDS,JDE,KDS,KDE
        INTEGER          :: IMS,IME,JMS,JME,KMS,KME
        INTEGER          :: ITS,ITE,JTS,JTE,KTS,KTE

        REAL, INTENT(IN) :: psfc_out(IMS:IME,JMS:JME)
        REAL, INTENT(IN) :: SGML1(KDE),SGML2(KDE),pdtop,pt

        REAL, INTENT(OUT):: p3d_out(IMS:IME,JMS:JME,KDS:KDE-1)
        REAL, INTENT(OUT):: PD(IMS:IME,JMS:JME)

        CHARACTER (len=255) :: message




        DO J=JTS,min(JTE,JDE-1)
          DO I=ITS,min(ITE,IDE-1)
             PD(I,J)=psfc_out(I,J)-PDTOP-PT
          ENDDO
        ENDDO

        DO J=JTS,min(JTE,JDE-1)
         DO K=KDS,KDE-1
          DO I=ITS,min(ITE,IDE-1)
           p3d_out(I,J,K)=PD(I,J)*SGML2(K)+PDTOP*SGML1(K)+PT

	IF (p3d_out(I,J,K) .ge. psfc_out(I,J) .or. p3d_out(I,J,K) .le. pt) THEN
           write(message,*) 'I,K,J,p3d_out: ', I,K,J,p3d_out(I,J,K)
           CALL wrf_error_fatal3("module_initialize_real.b",2829,&
message)
 	ENDIF

          ENDDO
         ENDDO
        ENDDO

	END SUBROUTINE compute_3d_pressure





  SUBROUTINE interp_press2press_lin(press_in,press_out, &
                                    data_in, data_out,generic          &
     &,                             extrapolate,ignore_lowest,TFIELD    &
     &,                             IDS,IDE,JDS,JDE,KDS,KDE             &
     &,                             IMS,IME,JMS,JME,KMS,KME             &
     &,                             ITS,ITE,JTS,JTE,KTS,KTE, internal_time )

    
    

    INTEGER                            :: IDS,IDE,JDS,JDE,KDS,KDE
    INTEGER                            :: IMS,IME,JMS,JME,KMS,KME
    INTEGER                            :: ITS,ITE,JTS,JTE,KTS,KTE,generic
    INTEGER                            :: internal_time


    REAL, INTENT(IN)                   :: press_in(IMS:IME,JMS:JME,generic)
    REAL, INTENT(IN)                   :: press_out(IMS:IME,JMS:JME,KDS:KDE-1)

    REAL, INTENT(IN)                   :: data_in(IMS:IME,JMS:JME,generic)
    REAL, INTENT(OUT)                  :: data_out(IMS:IME,JMS:JME,KMS:KME)
    LOGICAL, INTENT(IN)                :: extrapolate, ignore_lowest, TFIELD
    LOGICAL                            :: col_smooth

    INTEGER                            :: i,j
    INTEGER                            :: k,kk
    REAL                               :: desired_press
    REAL                               :: dvaldlnp,dlnp,tadiabat,tiso

    REAL, PARAMETER                    :: ADIAFAC=9.81/1004.
    REAL, PARAMETER                    :: TSTEXTRAPFAC=.0065



      DO K=KMS,KME
      DO J=JMS,JME
      DO I=IMS,IME
        DATA_OUT(I,J,K)=-99999.9
      ENDDO
      ENDDO
      ENDDO

    IF (ignore_lowest) then
       LMIN=2
    ELSE
       LMIN=1
    ENDIF

    DO j = JTS, min(JTE,JDE-1)
      test_i: DO i = ITS, min(ITE,IDE-1)

     IF (internal_time_loop .gt. 1) THEN
        IF (J .ne. JDS .and. J .ne. JDE-1 .and. &
          I .ne. IDS .and. I .ne. IDE-1 ) THEN

          CYCLE test_i
        ENDIF
     ENDIF


       col_smooth=.false.

        output_loop: DO k = KDS,KDE-1

          desired_press = press_out(i,j,k)

        if (K .gt. KDS) then
	if (TFIELD .and. col_smooth .and. desired_press .le. press_in(i,j,LMIN) &
                                    .and. press_out(i,j,k-1) .ge. press_in(i,j,LMIN)) then
	  MAX_SMOOTH=K


        endif
        endif



          IF (desired_press .GT. press_in(i,j,LMIN)) THEN
           IF (TFIELD .and. K .eq. 1  .and. (desired_press - press_in(i,j,LMIN)) .gt. 3000.) then
            col_smooth=.TRUE.   
           ENDIF
	

            IF ((desired_press - press_in(i,j,LMIN)).LT. 50.) THEN 
               data_out(i,j,k) = data_in(i,j,LMIN)
            ELSE
              IF (extrapolate) THEN
                
                
                
                

                
                

                if (TFIELD) then
                  tiso=0.5*(data_in(i,j,1)+data_in(i,j,2))
                endif


                IF ( (press_in(i,j,LMIN)-press_in(i,j,LMIN+1)) .GT. 500.) THEN 
                  dlnp     = log(press_in(i,j,LMIN))-log(press_in(i,j,LMIN+1))
                  dvaldlnp = (data_in(i,j,LMIN) - data_in(i,j,LMIN+1)) / dlnp
                ELSE                                                           
                  dlnp     = log(press_in(i,j,LMIN))-log(press_in(i,j,LMIN+5))
                  dvaldlnp = (data_in(i,j,LMIN) - data_in(i,j,LMIN+5)) / dlnp
                ENDIF
                data_out(i,j,k) = data_in(i,j,LMIN) + dvaldlnp * &
                               ( log(desired_press)-log(press_in(i,j,LMIN)) )

	if (TFIELD .and. data_out(i,j,k) .lt. tiso-0.2) then


          dvaldlnp=max(dvaldlnp, -1.0/ &
                                log( press_in(i,j,LMIN) / &
                                   ( press_in(i,j,LMIN)-1000.)  ))

          data_out(I,J,K)= data_in(i,j,LMIN) + dvaldlnp * &
                               ( log(desired_press)-log(press_in(i,j,LMIN)) )

        elseif (TFIELD .and. data_out(i,j,k) .gt. tiso+0.2) then


          dvaldlnp=min(dvaldlnp, 0.8/ &
                                log( press_in(i,j,LMIN) / &
                                   ( press_in(i,j,LMIN)-1000.)  ))

          data_out(I,J,K)= data_in(i,j,LMIN) + dvaldlnp * &
                               ( log(desired_press)-log(press_in(i,j,LMIN)) )

         endif

              ELSE
                data_out(i,j,k) = data_in(i,j,LMIN)
              ENDIF
            ENDIF
          ELSE IF (desired_press .LT. press_in(i,j,generic)) THEN
            IF ( (press_in(i,j,generic) - desired_press) .LT. 10.) THEN
               data_out(i,j,k) = data_in(i,j,generic)
            ELSE
              IF (extrapolate) THEN
                
                IF ((press_in(i,j,generic-1)-press_in(i,j,generic)).GT.50.) THEN
                  dlnp    =log(press_in(i,j,generic))-log(press_in(i,j,generic-1))
                  dvaldlnp=(data_in(i,j,generic)-data_in(i,j,generic-1))/dlnp
                ELSE
                  dlnp    =log(press_in(i,j,generic))-log(press_in(i,j,generic-2))
                  dvaldlnp=(data_in(i,j,generic)-data_in(i,j,generic-2))/dlnp
                ENDIF
                data_out(i,j,k) =  data_in(i,j,generic) + &
                  dvaldlnp * (log(desired_press)-log(press_in(i,j,generic)))
              ELSE
                data_out(i,j,k) = data_in(i,j,generic)
              ENDIF
            ENDIF
          ELSE
            

            input_loop:  DO kk = LMIN, generic-1
              IF (desired_press .EQ. press_in(i,j,kk) )THEN
                data_out(i,j,k) = data_in(i,j,kk)
                EXIT input_loop
              ELSE IF ( (desired_press .LT. press_in(i,j,kk)) .AND. &
                        (desired_press .GT. press_in(i,j,kk+1)) ) THEN



         dlnp = log(press_in(i,j,kk)) - log(press_in(i,j,kk+1))
         dvaldlnp = (data_in(i,j,kk)-data_in(i,j,kk+1))/dlnp
         data_out(i,j,k) = data_in(i,j,kk+1)+ &
                           dvaldlnp*(log(desired_press)-log(press_in(i,j,kk+1)))

                EXIT input_loop
              ENDIF

            ENDDO input_loop
          ENDIF
        ENDDO output_loop

	if (col_smooth) then
       do K=max(KDS,MAX_SMOOTH-4),MAX_SMOOTH+4
       data_out(I,J,K)=0.5*(data_out(I,J,K)+data_out(I,J,K+1))
       enddo
        endif

      ENDDO test_i
    ENDDO
  END SUBROUTINE interp_press2press_lin

  SUBROUTINE wind_adjust(press_in,press_out, &
                                    U_in, V_in,U_out,V_out           &
     &,                             generic,depth_replace    &
     &,                             IDS,IDE,JDS,JDE,KDS,KDE             &
     &,                             IMS,IME,JMS,JME,KMS,KME             &
     &,                             ITS,ITE,JTS,JTE,KTS,KTE )

    INTEGER                            :: IDS,IDE,JDS,JDE,KDS,KDE
    INTEGER                            :: IMS,IME,JMS,JME,KMS,KME
    INTEGER                            :: ITS,ITE,JTS,JTE,KTS,KTE,generic
    INTEGER                            :: MAXLIN,MAXLOUT

    REAL, INTENT(IN)                   :: press_in(IMS:IME,JMS:JME,generic)
    REAL, INTENT(IN)                   :: press_out(IMS:IME,JMS:JME,KDS:KDE-1)
    REAL, INTENT(IN)                   :: U_in(IMS:IME,JMS:JME,generic)
    REAL, INTENT(IN)                   :: V_in(IMS:IME,JMS:JME,generic)
    REAL, INTENT(INOUT)                :: U_out(IMS:IME,KMS:KME,JMS:JME)
    REAL, INTENT(INOUT)                :: V_out(IMS:IME,KMS:KME,JMS:JME)
    REAL                               :: p1d_in(generic)
    REAL                               :: p1d_out(KDS:KDE-1)


    DO j = JTS, min(JTE,JDE-1)
      DO i = ITS, min(ITE,IDE-1)


         IF(  (press_in(I,J,2)-press_out(I,J,1)) .gt. 200.) then

        U_out(I,1,J)=U_in(I,J,2)
        V_out(I,1,J)=V_in(I,J,2)

   INLOOP: DO L=2,generic
	p1d_in(L)=-9999.
        IF (  (press_in(I,J,2)-press_in(I,J,L)) .lt. depth_replace) THEN
          p1d_in(L)=(press_in(I,J,2)-press_in(I,J,L))
          MAXLIN=L
        ELSE
          p1d_in(L)=(press_in(I,J,2)-press_in(I,J,L))
          EXIT INLOOP
        ENDIF
    END DO INLOOP

   OUTLOOP: DO L=KDS,KDE-1
	p1d_out(L)=-9999.
        IF (  (press_out(I,J,1)-press_out(I,J,L)) .lt. depth_replace) THEN
          p1d_out(L)=(press_out(I,J,1)-press_out(I,J,L))
          MAXLOUT=L
        ELSE
          EXIT OUTLOOP
        ENDIF
    END DO OUTLOOP

        DO L=1,MAXLOUT
	ptarg=p1d_out(L)

    FINDLOOP:   DO LL=2,MAXLIN

         if (p1d_in(LL) .lt. ptarg .and. p1d_in(LL+1) .gt. ptarg) then

           dlnp=log(p1d_in(LL))-log(p1d_in(LL+1))
           dudlnp=(U_in(I,J,LL)-U_in(I,J,LL+1))/dlnp
           dvdlnp=(V_in(I,J,LL)-V_in(I,J,LL+1))/dlnp
           U_out(I,L,J)=U_in(I,J,LL)+dudlnp*(log(ptarg)-log(p1d_in(LL)))
           V_out(I,L,J)=V_in(I,J,LL)+dvdlnp*(log(ptarg)-log(p1d_in(LL)))

           EXIT FINDLOOP
         endif

   END DO FINDLOOP
        END DO   


        ENDIF

      ENDDO
    ENDDO



  END SUBROUTINE wind_adjust


  SUBROUTINE interp_press2press_log(press_in,press_out, &
                                    data_in, data_out, generic          &
     &,                             extrapolate,ignore_lowest           &
     &,                             IDS,IDE,JDS,JDE,KDS,KDE             &
     &,                             IMS,IME,JMS,JME,KMS,KME             &
     &,                             ITS,ITE,JTS,JTE,KTS,KTE, internal_time )

    
    

    INTEGER                            :: IDS,IDE,JDS,JDE,KDS,KDE
    INTEGER                            :: IMS,IME,JMS,JME,KMS,KME
    INTEGER                            :: ITS,ITE,JTS,JTE,KTS,KTE,generic
    INTEGER                            :: internal_time


    REAL, INTENT(IN)                   :: press_in(IMS:IME,JMS:JME,generic)
    REAL, INTENT(IN)                   :: press_out(IMS:IME,JMS:JME,KDS:KDE-1)


    REAL                               :: data_in(IMS:IME,JMS:JME,generic)
    REAL, INTENT(OUT)                  :: data_out(IMS:IME,JMS:JME,KMS:KME)
    LOGICAL, INTENT(IN)                :: extrapolate, ignore_lowest

    INTEGER                            :: i,j
    INTEGER                            :: k,kk
    REAL                               :: desired_press
    REAL                               :: dlnvaldlnp,dlnp


      DO K=1,generic
      DO j = JTS, min(JTE,JDE-1)
      DO i = ITS, min(ITE,IDE-1)
        DATA_IN(I,J,K)=max(DATA_in(I,J,K),1.e-13)
      ENDDO
      ENDDO
      ENDDO

      DO K=KMS,KME
      DO J=JMS,JME
      DO I=IMS,IME
        DATA_OUT(I,J,K)=-99999.9
      ENDDO
      ENDDO
      ENDDO

    IF (ignore_lowest) then
       LMIN=2
    ELSE
       LMIN=1
    ENDIF

    DO j = JTS, min(JTE,JDE-1)
     test_i:  DO i = ITS, min(ITE,IDE-1)

      IF (internal_time .gt. 1) THEN
        IF (J .ne. JDS .and. J .ne. JDE-1 .and. &
            I .ne. IDS .and. I .ne. IDE-1 ) THEN

          CYCLE test_i
        ENDIF
      ENDIF


        output_loop: DO k = KDS,KDE-1

          desired_press = press_out(i,j,k)

          IF (desired_press .GT. press_in(i,j,LMIN)) THEN

            IF ((desired_press - press_in(i,j,LMIN)).LT. 10.) THEN 
               data_out(i,j,k) = data_in(i,j,LMIN)
            ELSE
              IF (extrapolate) THEN
                
                
                
                

                
                

                IF ( (press_in(i,j,LMIN)-press_in(i,j,LMIN+1)) .GT. 100.) THEN
                  dlnp     = log(press_in(i,j,LMIN))-log(press_in(i,j,LMIN+1))
                  dlnvaldlnp = ( log(data_in(i,j,LMIN)) - log(data_in(i,j,LMIN+1)) ) / dlnp

                ELSE

                  dlnp     = log(press_in(i,j,LMIN))-log(press_in(i,j,LMIN+2))
                  dlnvaldlnp = (log(data_in(i,j,LMIN)) - log(data_in(i,j,LMIN+2))) / dlnp

                ENDIF

                data_out(i,j,k) = exp(log(data_in(i,j,LMIN)) + dlnvaldlnp * &
                               ( log(desired_press)-log(press_in(i,j,LMIN))))
              ELSE
                data_out(i,j,k) = data_in(i,j,LMIN)
              ENDIF
            ENDIF
          ELSE IF (desired_press .LT. press_in(i,j,generic)) THEN
            IF ( (press_in(i,j,generic) - desired_press) .LT. 10.) THEN
               data_out(i,j,k) = data_in(i,j,generic)
            ELSE
              IF (extrapolate) THEN
                
                IF ((press_in(i,j,generic-1)-press_in(i,j,generic)).GT.50.) THEN
                  dlnp    =log(press_in(i,j,generic))-log(press_in(i,j,generic-1))
                  dlnvaldlnp=(log(data_in(i,j,generic))-log(data_in(i,j,generic-1)))/dlnp
                ELSE
                  dlnp    =log(press_in(i,j,generic))-log(press_in(i,j,generic-2))
                  dlnvaldlnp=(log(data_in(i,j,generic))-log(data_in(i,j,generic-2)))/dlnp
                ENDIF
                data_out(i,j,k) =  exp(log(data_in(i,j,generic)) + &
                          dlnvaldlnp * (log(desired_press)-log(press_in(i,j,generic))))
              ELSE
                data_out(i,j,k) = data_in(i,j,generic)
              ENDIF
            ENDIF
          ELSE
            

            input_loop:  DO kk = LMIN, generic-1
              IF (desired_press .EQ. press_in(i,j,kk) )THEN
                data_out(i,j,k) = data_in(i,j,kk)
                EXIT input_loop
              ELSE IF ( (desired_press .LT. press_in(i,j,kk)) .AND. &
                        (desired_press .GT. press_in(i,j,kk+1)) ) THEN



         dlnp = log(press_in(i,j,kk)) - log(press_in(i,j,kk+1))
         dlnvaldlnp = (log(data_in(i,j,kk))-log(data_in(i,j,kk+1)))/dlnp
         data_out(i,j,k) = exp(log(data_in(i,j,kk+1))+ &
                          dlnvaldlnp*(log(desired_press)-log(press_in(i,j,kk+1))))

                EXIT input_loop

              ENDIF

            ENDDO input_loop
          ENDIF
        ENDDO output_loop
      ENDDO test_i
    ENDDO
  END SUBROUTINE interp_press2press_log


   SUBROUTINE rh_to_mxrat (rh, t, p, q , wrt_liquid , &
                           ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      LOGICAL , INTENT(IN)        :: wrt_liquid



      REAL , DIMENSION(ims:ime,jms:jme,kms:kme) , INTENT(IN)     :: p , t
      REAL , DIMENSION(ims:ime,jms:jme,kms:kme) , INTENT(INOUT)  :: rh
      REAL , DIMENSION(ims:ime,jms:jme,kms:kme) , INTENT(OUT)    :: q

      

      INTEGER                     :: i , j , k

      REAL                        :: ew , q1 , t1

      REAL,         PARAMETER     :: T_REF       = 0.0
      REAL,         PARAMETER     :: MW_AIR      = 28.966
      REAL,         PARAMETER     :: MW_VAP      = 18.0152

      REAL,         PARAMETER     :: A0       = 6.107799961
      REAL,         PARAMETER     :: A1       = 4.436518521e-01
      REAL,         PARAMETER     :: A2       = 1.428945805e-02
      REAL,         PARAMETER     :: A3       = 2.650648471e-04
      REAL,         PARAMETER     :: A4       = 3.031240396e-06
      REAL,         PARAMETER     :: A5       = 2.034080948e-08
      REAL,         PARAMETER     :: A6       = 6.136820929e-11

      REAL,         PARAMETER     :: ES0 = 6.1121

      REAL,         PARAMETER     :: C1       = 9.09718
      REAL,         PARAMETER     :: C2       = 3.56654
      REAL,         PARAMETER     :: C3       = 0.876793
      REAL,         PARAMETER     :: EIS      = 6.1071
      REAL                        :: RHS
      REAL,         PARAMETER     :: TF       = 273.16
      REAL                        :: TK

      REAL                        :: ES
      REAL                        :: QS
      REAL,         PARAMETER     :: EPS         = 0.622
      REAL,         PARAMETER     :: SVP1        = 0.6112
      REAL,         PARAMETER     :: SVP2        = 17.67
      REAL,         PARAMETER     :: SVP3        = 29.65
      REAL,         PARAMETER     :: SVPT0       = 273.15

      
      
      
      

         DO k = kts , kte
      DO j = jts , MIN ( jde-1 , jte )
            DO i = its , MIN (ide-1 , ite )
                  rh(i,j,k) = MIN ( MAX ( rh(i,j,k) ,  1. ) , 100. )
            END DO
         END DO
      END DO

      IF ( wrt_liquid ) THEN
            DO k = kts , kte
         DO j = jts , MIN ( jde-1 , jte )
               DO i = its , MIN (ide-1 , ite )
                  es=svp1*10.*EXP(svp2*(t(i,j,k)-svpt0)/(t(i,j,k)-svp3))
                  qs=eps*es/(p(i,j,k)/100.-es)
                  q(i,j,k)=MAX(.01*rh(i,j,k)*qs,0.0)
               END DO
            END DO
         END DO

      ELSE
            DO k = kts , kte
         DO j = jts , MIN ( jde-1 , jte )
               DO i = its , MIN (ide-1 , ite )

                  t1 = t(i,j,k) - 273.16

                  

                  IF ( t1 .lt. -200. ) THEN
                     q(i,j,k) = 0

                  ELSE

                     

                     IF ( ( t1 .GE. t_ref ) .AND. ( t1 .GE. -47.) ) THEN    
                        ew = a0 + t1 * (a1 + t1 * (a2 + t1 * (a3 + t1 * (a4 + t1 * (a5 + t1 * a6)))))

                     ELSE IF ( ( t1 .GE. t_ref ) .AND. ( t1 .LT. -47. ) ) then 
                        ew = es0 * exp(17.67 * t1 / ( t1 + 243.5))

                     ELSE
                        tk = t(i,j,k)
                        rhs = -c1 * (tf / tk - 1.) - c2 * alog10(tf / tk) +  &
                               c3 * (1. - tk / tf) +      alog10(eis)
                        ew = 10. ** rhs

                     END IF

                     

                     ew = MAX ( ew , 0. ) * rh(i,j,k) * 0.01

                     
                     
                     
                     

                     q1 = mw_vap * ew
                     q1 = q1 / (q1 + mw_air * (p(i,j,k)/100. - ew))

                     q(i,j,k) = q1 / (1. - q1 )

                  END IF

               END DO
            END DO
         END DO

      END IF

   END SUBROUTINE rh_to_mxrat



      SUBROUTINE  boundary_smooth(h, landmask, grid, nsmth , nrow &
     &,                          IDS,IDE,JDS,JDE,KDS,KDE             &
     &,                          IMS,IME,JMS,JME,KMS,KME             &
     &,                          ITS,ITE,JTS,JTE,KTS,KTE )

	implicit none

      TYPE (domain)          :: grid

        integer :: IDS,IDE,JDS,JDE,KDS,KDE
        integer :: IMS,IME,JMS,JME,KMS,KME
        integer :: ITS,ITE,JTS,JTE,KTS,KTE
        integer:: ihw(JDS:JDE-1),ihe(JDS:JDE-1),nsmth,nrow
        real::    h(IMS:IME,JMS:JME),landmask(IMS:IME,JMS:JME)
        real ::   h_old(IMS:IME,JMS:JME)
        real ::   hbms(IDS:IDE-1,JDS:JDE-1)
        real ::   hse(IDS:IDE-1,JDS:JDE-1)
        real ::   hne(IDS:IDE-1,JDS:JDE-1)
        integer :: IPS,IPE,JPS,JPE,KPS,KPE
        integer :: ihl, ihh, m2l, ibas,jmelin
        integer :: I,J,KS,IOFFSET,JSTART,JEND
        character (len=255) :: message

	ips=its
        ipe=ite
	jps=jts
        jpe=jte
	kps=kts
        kpe=kte

        do j= JTS,min(JTE,JDE-1)
         ihw(J)=-mod(J,2)
         ihe(j)=ihw(J)+1
        end do

        do J=JTS,min(JTE,JDE-1)
         do I=ITS,min(ITE,IDE-1)
           hbms(I,J)=landmask(I,J)
         enddo
        enddo

        jmelin=(JDE-1)-nrow+1
        ibas=nrow/2
        m2l=mod(nrow,2)

        do j=jts,min(jte,jde-1)
         ihl=ibas+mod(j,2)+m2l*mod(J+1,2)
         ihh=(IDE-1)-ibas-m2l*mod(J+1,2)
         do i=its,min(ite,ide-1)
          if (I .ge. ihl .and. I .le. ihh .and. J .ge. nrow .and. J .le. jmelin) then
           hbms(I,J)=0.
          endif
         end do
        end do

  634	format(30(f2.0,1x))

        do KS=1,nsmth

         grid%ht_gc=h
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_MG.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_MG_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
         h=grid%ht_gc
         h_old=grid%ht_gc

          do J=JTS,min(JTE,JDE-1)
           do I=ITS, min(ITE,IDE-1)
              if (I .ge. (IDS+mod(J,2)) .and. J .gt. JDS .and. J .lt. JDE-1 .and. I .lt. IDE-1) then
                h(i,j)= ( h_old(i+ihe(j),j+1) + h_old(i+ihw(j),j-1) + h_old(i+ihe(j),j-1) + h_old(i+ihw(j),j+1) - &
                        4. *h_old(i,j) )*hbms(i,j)*0.125+h_old(i,j)
              endif

           enddo
          enddo



        if (hbms(1,1) .eq. 1 .and. ITS .le. 1 .and. JTS .le. 1) then
        h(1,1)=0.75*h(1,1)+0.125*h(1+ihe(1),2)+  &
                                 0.0625*(h(2,1)+h(1,3))
        endif

        if (hbms(IDE-1,1) .eq. 1 .and. ITE .ge. IDE-2 .and. JTS .le. 1) then
        h(IDE-1,1)=0.75*h(IDE-1,1)+0.125*h(IDE-1+ihw(1),2)+ &
                                 0.0625*(h(IDE-1-1,1)+h(IDE-1,3))
        endif

        if (hbms(1,JDE-1) .eq. 1 .and. ITS .le. 1 .and. JTE .ge. JDE-2) then
        h(1,JDE-1)=0.75*h(1,JDE-1)+0.125*h(1+ihe(JDE-1),JDE-1-1)+ &
                                 0.0625*(h(2,JDE-1)+h(1,JDE-1-2))
        endif

        if (hbms(IDE-1,JDE-1) .eq. 1 .and. ITE .ge. IDE-2 .and. JTE .ge. JDE-2) then
        h(IDE-1,JDE-1)=0.75*h(IDE-1,JDE-1)+0.125*h(IDE-1+ihw(JDE-1),JDE-1-1)+ &
                                 0.0625*(h(IDE-1-1,JDE-1)+h(IDE-1,JDE-1-2))
        endif

        do J=JMS,JME
         do I=IMS,IME
         grid%ht_gc(I,J)=h(I,J)
         enddo
        enddo
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_MG.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_MG_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
         do J=JMS,JME
         do I=IMS,IME
         h(I,J)=grid%ht_gc(I,J)
         enddo
        enddo



	if (JTS .eq. JDS) then
        J=JTS

        do I=ITS,ITE
        if (I .ge. IDS+1 .and. I .le. IDE-2) then
        if (hbms(I,J) .eq. 1) then
        h(I,J)=0.75*h(I,J)+0.125*(h(I+ihw(J),J+1)+h(I+ihe(J),J+1))
        endif
        endif
        enddo

        endif


        if (JTE .eq. JDE) then
        J=JDE-1
	write(message,*) 'DOING N BOUND SMOOTHING for J= ', J
        CALL wrf_message(message)
         do I=ITS,min(ITE,IDE-1)
          if (hbms(I,J) .eq. 1 .and. I .ge. IDS+1 .and. I .le. IDE-2) then
           h(I,J)=0.75*h(I,J)+0.125*(h(I+ihw(J),J-1)+h(I+ihe(J),J-1))
          endif
         enddo
	endif


        if (ITS .eq. IDS) then
         I=ITS
         do J=JTS,min(JTE,JDE-1)
          if (hbms(I,J) .eq. 1 .and. J .ge. JDS+2 .and. J .le. JDE-3 .and. mod(J,2) .eq. 1) then
           h(I,J)=0.75*h(I,J)+0.125*(h(I+ihe(J),J+1)+h(I+ihe(J),J-1))
          endif
         enddo
	endif


	if (ITE .eq. IDE) then
	write(message,*) 'DOING E BOUND SMOOTHING for I= ', min(ITE,IDE-1)
        CALL wrf_message(message)
         I=min(ITE,IDE-1)
         do J=JTS,min(JTE,JDE-1)
          if (hbms(I,J) .eq. 1  .and. J .ge. JDS+2 .and. J .le. JDE-3 .and. mod(J,2) .eq. 1) then
           h(I,J)=0.75*h(I,J)+0.125*(h(I+ihw(J),J+1)+h(I+ihw(J),J-1))
          endif
         enddo
	endif

        enddo   

        do J=JMS,JME
         do I=IMS,IME
         grid%ht_gc(I,J)=h(I,J)
         enddo
        enddo
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_NMM_MG.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_NMM_MG_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
        do J=JMS,JME
         do I=IMS,IME
         h(I,J)=grid%ht_gc(I,J)
        enddo
        enddo



          if (JTS .eq. JDS) then
           if (ITE .eq. IDE) then
              IOFFSET=1
           else
              IOFFSET=0
           endif

           do i=its,min(ITE,IDE-1)-IOFFSET
             h(i,2)=0.25*(h(i,1)+h(i+1,1)+ &
                          h(i,3)+h(i+1,3))
           enddo
          endif


          if (JTE .eq. JDE) then
           if (ITE .eq. IDE) then
              IOFFSET=1
           else
              IOFFSET=0
           endif
           do i=its,min(ITE,IDE-1)-IOFFSET
             h(i,(JDE-1)-1)=0.25*(h(i,(JDE-1)-2)+h(i+1,(JDE-1)-2)+ &
                                h(i,JDE-1)+h(i+1,JDE-1))
           enddo
          endif

           if (JTS .eq. 1) then
             JSTART=4
           else
             JSTART=JTS+mod(JTS,2) 
           endif

           if (JTE .eq. JDE) then
             JEND=(JDE-1)-3
           else
             JEND=JTE
           endif

          if (ITS .eq. IDS) then


          do j=JSTART,JEND,2
            h(1,j)=0.25*(h(1,j-1)+h(2,j-1)+ &
                         h(1,j+1)+h(2,j+1))

          enddo
          endif
	

          if (ITE .eq. IDE) then

          do j=JSTART,JEND,2
            h((IDE-1)-1,j)=0.25*(h((IDE-1)-1,j-1)+h((IDE-1),j-1)+        &
                                 h((IDE-1)-1,j+1)+h((IDE-1),j+1))
          enddo
          endif


       END SUBROUTINE boundary_smooth



   SUBROUTINE monthly_interp_to_date ( field_in , date_str , field_out , &
                      ids , ide , jds , jde , kds , kde , &
                      ims , ime , jms , jme , kms , kme , &
                      its , ite , jts , jte , kts , kte )

   
   

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      CHARACTER (LEN=24) , INTENT(IN) :: date_str
      REAL , DIMENSION(ims:ime,jms:jme,12) , INTENT(IN)  :: field_in
      REAL , DIMENSION(ims:ime,   jms:jme) , INTENT(OUT) :: field_out

      

      INTEGER :: i , j , l
      INTEGER , DIMENSION(0:13) :: middle
      INTEGER :: target_julyr , target_julday , target_date
      INTEGER :: julyr , julday , int_month, next_month
      REAL :: gmt
      CHARACTER (LEN=4) :: yr
      CHARACTER (LEN=2) :: mon , day15


      WRITE(day15,FMT='(I2.2)') 15
      DO l = 1 , 12
         WRITE(mon,FMT='(I2.2)') l
         CALL get_julgmt ( date_str(1:4)//'-'//mon//'-'//day15//'_'//'00:00:00.0000' , julyr , julday , gmt )
         middle(l) = julyr*1000 + julday
      END DO

      l = 0
      middle(l) = middle( 1) - 31

      l = 13
      middle(l) = middle(12) + 31

      CALL get_julgmt ( date_str , target_julyr , target_julday , gmt )
      target_date = target_julyr * 1000 + target_julday
      find_month : DO l = 0 , 12
         IF ( ( middle(l) .LT. target_date ) .AND. ( middle(l+1) .GE. target_date ) ) THEN
            DO j = jts , MIN ( jde-1 , jte )
               DO i = its , MIN (ide-1 , ite )
                  int_month = MOD ( l , 12 )
                  IF ( int_month .EQ. 0 ) int_month = 12

	IF (int_month == 12) THEN
            next_month=1
        ELSE
            next_month=int_month+1
        ENDIF

                  field_out(i,j) =  ( field_in(i,j,next_month) * ( target_date - middle(l)   ) + &
                                      field_in(i,j,int_month  ) * ( middle(l+1) - target_date ) ) / &
                                    ( middle(l+1) - middle(l) )
               END DO
            END DO
            EXIT find_month
         END IF
      END DO find_month
   END SUBROUTINE monthly_interp_to_date


   SUBROUTINE monthly_min_max ( field_in , field_min , field_max , &
                      ids , ide , jds , jde , kds , kde , &
                      ims , ime , jms , jme , kms , kme , &
                      its , ite , jts , jte , kts , kte )

   

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      REAL , DIMENSION(ims:ime,jms:jme,12) , INTENT(IN)  :: field_in
      REAL , DIMENSION(ims:ime,   jms:jme) , INTENT(OUT) :: field_min , field_max

      

      INTEGER :: i , j , l
      REAL :: minner , maxxer

      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            minner = field_in(i,j,1)
            maxxer = field_in(i,j,1)
            DO l = 2 , 12
               IF ( field_in(i,j,l) .LT. minner ) THEN
                  minner = field_in(i,j,l)
               END IF
               IF ( field_in(i,j,l) .GT. maxxer ) THEN
                  maxxer = field_in(i,j,l)
               END IF
            END DO
            field_min(i,j) = minner
            field_max(i,j) = maxxer
         END DO
      END DO

   END SUBROUTINE monthly_min_max



  SUBROUTINE reverse_vert_coord  ( field, start_z, end_z                &
     &,                             IDS,IDE,JDS,JDE,KDS,KDE             &
     &,                             IMS,IME,JMS,JME,KMS,KME             &
     &,                             ITS,ITE,JTS,JTE,KTS,KTE )

	IMPLICIT NONE

        INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                       ims , ime , jms , jme , kms , kme , &
                                       its , ite , jts , jte , kts , kte,  &
                                       start_z, end_z

        REAL, INTENT(INOUT)         :: field(IMS:IME,JMS:JME,end_z)


        INTEGER                     :: I,J,L
        REAL, ALLOCATABLE           :: dum3d(:,:,:)

        allocate(dum3d(IMS:IME,JMS:JME,end_z))

        DO L=start_z,end_z
          DO J=jts,min(jte,jde-1)
            DO I=its,min(ite,ide-1)
	      dum3d(I,J,L)=field(I,J,end_z-L+start_z)
            END DO
          END DO
        END DO

        DO L=start_z,end_z
          DO J=jts,min(jte,jde-1)
            DO I=its,min(ite,ide-1)
              field(I,J,L)=dum3d(I,J,L)
            END DO
          END DO
        END DO

        DEALLOCATE(dum3d)

        END SUBROUTINE reverse_vert_coord




        SUBROUTINE compute_nmm_levels(ninterface, ptop, eta_levels)

        USE module_model_constants

        IMPLICIT NONE

        character(len=132):: message
        integer        ::  ninterface,Lthick,L
        real, parameter:: gamma=.0065
        real, parameter:: t_stand=288.
        real, parameter:: p_stand=101325.

        real           ::  maxdz_compute, ptop
        real           ::  plower,pupper,tlay, sum

        real             :: eta_levels(ninterface)
        real, allocatable:: Z(:)
        real, allocatable:: deta_levels_spline(:)

        logical:: print_pbl_warn



        allocate(Z(ninterface))
        allocate(deta_levels_spline(ninterface-1))

        CALL compute_eta_spline(ninterface-1,deta_levels_spline,ptop)

        sum=0.
        DO L=1,ninterface-1
          sum=sum+deta_levels_spline(L)
        ENDDO

        eta_levels(1)=1.00

        DO L=2,ninterface
          eta_levels(L)=eta_levels(L-1)-deta_levels_spline(L-1)
        ENDDO

        eta_levels(ninterface)=0.00

        DO L=2,ninterface-1
          eta_levels(L)=0.5*(eta_levels(L))+0.25*(eta_levels(L-1)+eta_levels(L+1))
        ENDDO

        Z(1)=0.
        maxdz_compute=0.
        print_pbl_warn=.false.

        DO L=2,ninterface
          tlay=max( t_stand-gamma*Z(L-1), 216.5)
          plower=ptop+(p_stand-ptop)*eta_levels(L-1)
          pupper=ptop+(p_stand-ptop)*eta_levels(L)
          Z(L)=Z(L-1)+(tlay*r_d/g)*(log(plower)-log(pupper))

          if (plower .gt. 85000. .and. pupper .lt. 85000. .and. L .lt. 10) then
            print_pbl_warn=.true.
          endif

          write(message,*) 'L, eta(l), pupper, Z(L): ', L, eta_levels(L),pupper,Z(L)
          CALL wrf_debug(100,message)

          if (Z(L)-Z(L-1) .gt. maxdz_compute) then
            Lthick=L
          endif

          maxdz_compute=max(maxdz_compute,Z(L)-Z(L-1))
        ENDDO

        if (print_pbl_warn) then
          write(message,*) 'WARNING - PBL MAY BE POORLY RESOLVED WITH NUMBER OF VERTICAL LEVELS'
          CALL wrf_message(message)
          write(message,*) '        - CONSIDER INCREASING THE VERTICAL RESOLUTION'
          CALL wrf_message(message)
        endif

        write(message,*) 'thickest layer was: ', maxdz_compute , 'meters thick at level: ', Lthick
        CALL wrf_message(message)

        END SUBROUTINE compute_nmm_levels



     SUBROUTINE compute_eta_spline(LM, dsg, ptop)

     IMPLICIT NONE

     real:: dsg(LM), ptop, sum, rsum
     real, allocatable:: xold(:),dold(:)
     real, allocatable:: xnew(:),sgm(:)
     real, allocatable:: pps(:),qqs(:),y2s(:)
     integer nlev,LM,L,KOLD

    IF (LM .ge. 46) THEN
     KOLD=9
     allocate(xold(KOLD))
     allocate(dold(KOLD))

     xold(1)=.00
     dold(1)=.006
     xold(2)=.13
     dold(2)=.009
     xold(3)=.19
     dold(3)=.012
     xold(4)=.30
     dold(4)=.036
     xold(5)=.42
     dold(5)=.041
     xold(6)=.56
     dold(6)=.040
     xold(7)=.69
     dold(7)=.018

     if (ptop .ge. 2000.) then
      xold(8)=.90
      dold(8)=.012
      xold(9)=1.0
      dold(9)=.006
     else
      xold(8)=.90
      dold(8)=.008
      xold(9)=1.0
      dold(9)=.003
     endif

    ELSE

     KOLD=8
     allocate(xold(KOLD))
     allocate(dold(KOLD))

     xold(1)=.00
     dold(1)=.006
     xold(2)=.18
     dold(2)=.015
     xold(3)=.32
     dold(3)=.035
     xold(4)=.50
     dold(4)=.040
     xold(5)=.68
     dold(5)=.030
     xold(6)=.75
     dold(6)=.017
     xold(7)=.85
     dold(7)=.012

     if (ptop .ge. 2000.) then
      xold(8)=1.0
      dold(8)=.013
     else
      xold(8)=1.0
      dold(8)=.008
     endif

    ENDIF

        allocate(xnew(lm))
        allocate(sgm(lm))
        allocate(pps(lm))
        allocate(qqs(lm))
        allocate(y2s(lm))

    DO L=1,LM
       xnew(l)=float(l-1)/float(lm-1)
    ENDDO

    y2s=0.

    CALL spline(kold,kold,xold,dold,y2s,lm,xnew,dsg,pps,qqs)

    sum=0.
    DO l=1,lm
       sum=sum+dsg(l)
    ENDDO

    rsum=1./sum
    sgm(1)=0.

    DO L=1,lm-1
     dsg(l)=dsg(l)*rsum
     sgm(l+1)=sgm(l)+dsg(l)
    ENDDO
    sgm(lm+1)=1.
    dsg(lm)=sgm(lm+1)-sgm(lm)

    END SUBROUTINE compute_eta_spline



     subroutine spline(JTBX,NOLD,XOLD,YOLD,Y2,NNEW,XNEW,YNEW,P,Q)




























      IMPLICIT NONE

      INTEGER,INTENT(IN) :: JTBX,NNEW,NOLD
      REAL,DIMENSION(JTBX),INTENT(IN) :: XNEW,XOLD,YOLD
      REAL,DIMENSION(JTBX),INTENT(INOUT) :: P,Q,Y2
      REAL,DIMENSION(JTBX),INTENT(OUT) :: YNEW

      INTEGER :: K,K1,K2,KOLD,NOLDM1
      REAL :: AK,BK,CK,DEN,DX,DXC,DXL,DXR,DYDXL,DYDXR                   &
     &       ,RDX,RTDXC,X,XK,XSQ,Y2K,Y2KP1

      NOLDM1=NOLD-1

      DXL=XOLD(2)-XOLD(1)
      DXR=XOLD(3)-XOLD(2)
      DYDXL=(YOLD(2)-YOLD(1))/DXL
      DYDXR=(YOLD(3)-YOLD(2))/DXR
      RTDXC=0.5/(DXL+DXR)

      P(1)= RTDXC*(6.*(DYDXR-DYDXL)-DXL*Y2(1))
      Q(1)=-RTDXC*DXR

      first_loop: DO K=3,NOLD-1
        IF(NOLD==3) exit first_loop   
        DXL=DXR
        DYDXL=DYDXR
        DXR=XOLD(K+1)-XOLD(K)
        DYDXR=(YOLD(K+1)-YOLD(K))/DXR
        DXC=DXL+DXR
        DEN=1./(DXL*Q(K-2)+DXC+DXC)
        P(K-1)= DEN*(6.*(DYDXR-DYDXL)-DXL*P(K-2))
        Q(K-1)=-DEN*DXR
      END DO first_loop

      DO K=NOLDM1,2,-1
         Y2(K)=P(K-1)+Q(K-1)*Y2(K+1)
      END DO


      second_loop:  DO K1=1,NNEW+1
        XK=XNEW(K1)
        third_loop:  DO K2=2,NOLD
          IF(XOLD(K2)>XK)THEN
            KOLD=K2-1
            exit third_loop
          ENDIF
        END DO third_loop

        IF (XOLD(K2) .le. XK) THEN
          YNEW(K1)=YOLD(NOLD)
          CYCLE second_loop
        ENDIF

        IF (K1 .eq. 1 .or. K .ne. KOLD) THEN
          K=KOLD
          Y2K=Y2(K)
          Y2KP1=Y2(K+1)
          DX=XOLD(K+1)-XOLD(K)
          RDX=1./DX
          AK=.1666667*RDX*(Y2KP1-Y2K)
          BK=0.5*Y2K
          CK=RDX*(YOLD(K+1)-YOLD(K))-.1666667*DX*(Y2KP1+Y2K+Y2K)
        ENDIF

        X=XK-XOLD(K)
        XSQ=X*X
        YNEW(K1)=AK*XSQ*X+BK*XSQ+CK*X+YOLD(K)
      END DO second_loop

      END SUBROUTINE SPLINE



      SUBROUTINE egrid_boundary_winds (pd, u, v, psl_in, u_in, v_in, &
     &                            dsg1, dsg2, PDTOP,pt,             &
     &                            IDS,IDE,JDS,JDE,KDS,KDE,          &
     &                            IMS,IME,JMS,JME,KMS,KME,          &
     &                            ITS,ITE,JTS,JTE,KTS,KTE, num_metgrid_levels  )

       REAL, INTENT(IN):: PD(IMS:IME,JMS:JME) 
       REAL, INTENT(IN):: PSL_IN(IMS:IME,JMS:JME,num_metgrid_levels)
       REAL, INTENT(IN):: U_IN(IMS:IME,JMS:JME,num_metgrid_levels)
       REAL, INTENT(IN):: V_IN(IMS:IME,JMS:JME,num_metgrid_levels)
       REAL, INTENT(IN):: dsg1(KME),dsg2(KME),PDTOP,PT
       REAL, INTENT(INOUT):: U(IMS:IME,JMS:JME,KMS:KME),V(IMS:IME,JMS:JME,KMS:KME)
       REAL            :: ptop, psfc_inc







       INTEGER :: LD, LDMIN

        write(0,*) 'KME is: ', KME

        ptop=pt+(dsg1(KME-1)*pdtop)*0.5   

        write(0,*) 'ptop is: ', ptop



   s_boundary:  if (JTS .eq. 1) then
       DO I=ITS,min(ITE,IDE-2)
          sdph=0.
          spfp=0.
          spfh=0.
        psfc_inc=(dsg1(1)*pdtop+dsg2(1)*PD(I,1))*0.5
        if (I .eq. 1) write(0,*) 'psfc_inc is: ', psfc_inc

        if (PSL_IN(I,1,1) .gt. PSL_IN(I,1,2)) then
        ldmin=1
        else
        ldmin=2
        endif



       DO LD=LDMIN,num_metgrid_levels-1



          if((PSL_IN(I,1,ld)+PSL_IN(I,1,ld+1))*0.5.le.PD(I,1)+PDTOP+pt-psfc_inc .and. &
             (PSL_IN(I,1,LD)+PSL_IN(I,1,ld+1))*0.5 .ge. PTOP ) then
            if (I .eq. ITS) write(0,*) 'LD, PSL_IN(LD):: ', LD, PSL_IN(I,1,ld)
            spfp=(V_IN(I,1,ld)+V_IN(I,1,ld+1))* &
               (PSL_IN(I,1,ld)-PSL_IN(I,1,ld+1))*0.5+spfp
          endif
       ENDDO

      DO l=1,KME-1
        dphl=(dsg1(l)*pdtop+dsg2(l)*pd(I,1))
        sdph=dphl+sdph
        spfh=v(i,1,l)*dphl+spfh
      ENDDO

      dvel=(spfp-spfh)/sdph

      amdvel=max(abs(dvel),amdvel)

          do l=1,KME-1
      v(i,1,l)=v(i,1,l)+dvel
          enddo

       write(0,*) 's boundary I, dvel: ', I, dvel

        END DO
      print*,'south max dvel=',amdvel

        endif s_boundary


   n_boundary:  if (JTE .ge. JDE-2) then

        JEND=min(JTE,JDE-1)


          amdvel=0.
              do i=ITS,min(ITE,IDE-2)
        psfc_inc=(dsg1(1)*pdtop+dsg2(1)*PD(I,JEND))*0.5
          sdph=0.
          spfp=0.
          spfh=0.

        if (PSL_IN(I,JEND,1) .gt. PSL_IN(I,JEND,2)) then
        ldmin=1
        else
        ldmin=2
        endif

          do ld=LDMIN,num_metgrid_levels-1

          if((PSL_IN(I,JEND,ld)+PSL_IN(I,JEND,ld+1))*0.5.le.PD(I,JEND)+PDTOP+pt-psfc_inc .and. &
             (PSL_IN(I,JEND,LD)+PSL_IN(I,JEND,ld+1))*0.5 .ge. PTOP ) then

      spfp=(V_IN(I,JEND,ld)+V_IN(I,JEND,ld+1))* &
           (PSL_IN(I,JEND,ld)-PSL_IN(I,JEND,ld+1))*0.5+spfp
          endif
          enddo



          do l=1,KME-1
      dphl=(dsg1(l)*pdtop+dsg2(l)*pd(I,JEND))
      sdph=dphl+sdph
      spfh=v(i,JEND,l)*dphl+spfh
          enddo

      dvel=(spfp-spfh)/sdph
      amdvel=max(abs(dvel),amdvel)

          do l=1,KME-1
      v(i,JEND,l)=v(i,JEND,l)+dvel
          enddo
              enddo
      write(0,*) 'north max dvel=',amdvel

       endif n_boundary



  w_boundary:   if (ITS .eq. 1)  then

        JEND=min(JTE,JDE-2)

          amdvel=0.
              do j=JTS,JEND
        psfc_inc=(dsg1(1)*pdtop+dsg2(1)*PD(1,J))*0.5
          sdph=0.
          spfp=0.
          spfh=0.

        if (PSL_IN(1,J,1) .gt. PSL_IN(1,J,2)) then
        ldmin=1
        else
        ldmin=2
        endif


        if (mod(J,2) .eq. 0) then

          do ld=ldmin,num_metgrid_levels-1
          if((PSL_IN(1,J,ld)+PSL_IN(1,J,ld+1))*0.5.lt.PD(1,J)+PDTOP+PT-psfc_inc .and. &
             (PSL_IN(1,J,ld)+PSL_IN(1,J,ld+1))*0.5 .ge. PTOP) then
      spfp=(u_in(1,j,ld)+u_in(1,j,ld+1))*  &
           (PSL_IN(1,J,ld)-PSL_IN(1,J,ld+1))*0.5+spfp
          endif
          enddo




          do l=1,KME-1
      dphl=(dsg1(l)*pdtop+dsg2(l)*pd(1,J))
      sdph=dphl+sdph
      spfh=u(1,j,l)*dphl+spfh
          enddo

      dvel=(spfp-spfh)/sdph
      amdvel=max(abs(dvel),amdvel)

          do l=1,KME-1
      u(1,j,l)=u(1,j,l)+dvel
          enddo

              endif  
              enddo  
      write(0,*) 'west max dvel=',amdvel

        endif w_boundary



  e_boundary:  if (ITE .ge. IDE-2) then

        IEND=IDE-1

          amdvel=0.
              do j=JTS,min(JTE,JDE-2)
        psfc_inc=(dsg1(1)*pdtop+dsg2(1)*PD(IEND,J))*0.5
          sdph=0.
          spfp=0.
          spfh=0.

        if (PSL_IN(IEND,J,1) .gt. PSL_IN(IEND,J,2)) then
        ldmin=1
        else
        ldmin=2
        endif


        if (mod(J,2) .eq. 0) then

          do ld=LDMIN,num_metgrid_levels-1

        if (J .eq. 200) then
        write(6,*) 'U_IN(IEND,j,ld): ', LD, U_IN(IEND,j,ld)
        endif

          if((PSL_IN(IEND,J,ld)+PSL_IN(IEND,J,ld+1))*0.5.le.pd(IEND,J)+pdtop+pt-psfc_inc .and. &
             (PSL_IN(IEND,J,ld)+PSL_IN(IEND,J,ld+1))*0.5.ge. PTOP)  then

      spfp=(u_in(IEND,j,ld)+u_in(IEND,j,ld+1))* &
     &     (PSL_IN(IEND,j,ld)-PSL_IN(IEND,j,ld+1))*0.5+spfp

          endif
          enddo



          do l=1,KME-1

        if (J .eq. 200) then
        write(6,*) 'U_OUT(IEND,J,L): ', L, U(IEND,J,L)
        endif

      dphl=(dsg1(l)*pdtop+dsg2(l)*pd(IEND,J))

        if (J .eq. 200) then
        write(6,*) 'L, dsg1, pdtop, dsg2, pd, dphl: ', L, dsg1(L), pdtop, dsg2(L), pd(IEND,J), dphl
        endif

      sdph=dphl+sdph
      spfh=u(IEND,j,l)*dphl+spfh
        if (J .eq. 200) then
        write(6,*) 'L, flux(L), total out flux: ', L, dphl*u(IEND,J,L), spfh
        endif
          enddo

        if (J .eq. 200) then
        write(6,*) 'spfp, spfh, sdph: ', spfp, spfh, sdph
        write(6,*) 'spfp-spfh, sdph: ', spfp-spfh, sdph
        endif

      dvel=(spfp-spfh)/sdph
      amdvel=max(abs(dvel),amdvel)



          do l=1,KME-1
      u(IEND,j,l)=u(IEND,j,l)+dvel
          enddo
              endif 
              enddo
      write(0,*) 'east max dvel=',amdvel


     endif e_boundary


      END SUBROUTINE egrid_boundary_winds



      SUBROUTINE NMM_SH2O(IMS,IME,JMS,JME,ISTART,IM,JSTART,JM,&
                        NSOIL,ISLTPK, &
                        SM,SICE,STC,SMC,SH2O)



        INTEGER, PARAMETER:: NSOTYP=19 

        REAL :: PSIS(NSOTYP),BETA(NSOTYP),SMCMAX(NSOTYP)
        REAL :: STC(IMS:IME,NSOIL,JMS:JME), &
                SMC(IMS:IME,NSOIL,JMS:JME)
        REAL :: SH2O(IMS:IME,NSOIL,JMS:JME),SICE(IMS:IME,JMS:JME),&
                SM(IMS:IME,JMS:JME)
        REAL :: HLICE,GRAV,T0,BLIM
        INTEGER :: ISLTPK(IMS:IME,JMS:JME)
        CHARACTER(LEN=255) :: message


      DATA HLICE/3.335E5/,GRAV/9.81/,T0/273.15/
      DATA BLIM/5.5/








        DATA PSIS /0.069, 0.036, 0.141, 0.759, 0.759, 0.355,   &
                   0.135, 0.617, 0.263, 0.098, 0.324, 0.468,   &
                   0.355, 0.000, 0.069, 0.036, 0.468, 0.069, 0.069  /

        DATA BETA/2.79,  4.26,  4.74,  5.33,  5.33,  5.25,    &
                  6.66,  8.72,  8.17, 10.73, 10.39, 11.55,    &
                  5.25,  0.00,  2.79,  4.26, 11.55, 2.79, 2.79 /

        DATA SMCMAX/0.339, 0.421, 0.434, 0.476, 0.476, 0.439,  &
                    0.404, 0.464, 0.465, 0.406, 0.468, 0.468,  &
                    0.439, 1.000, 0.200, 0.421, 0.468, 0.200, 0.339/

        DO K=1,NSOIL
         DO J=JSTART,JM
          DO I=ISTART,IM


        IF (SMC(I,K,J) .gt. SMCMAX(ISLTPK(I,J))) then
  if (K .eq. 1) then
    write(message,*) 'I,J,reducing SMC from ' ,I,J,SMC(I,K,J), 'to ', SMCMAX(ISLTPK(I,J))
    CALL wrf_debug(100,message)
  endif
        SMC(I,K,J)=SMCMAX(ISLTPK(I,J))
        ENDIF


        IF ( (SM(I,J) .lt. 0.5) .and. (SICE(I,J) .lt. 0.5) ) THEN

        IF (ISLTPK(I,J) .gt. 19) THEN
                WRITE(message,*) 'FORCING ISLTPK at : ', I,J
                CALL wrf_message(message)
                ISLTPK(I,J)=9
        ELSEIF (ISLTPK(I,J) .le. 0) then
                WRITE(message,*) 'FORCING ISLTPK at : ', I,J
                CALL wrf_message(message)
                ISLTPK(I,J)=1
        ENDIF





           IF (STC(I,K,J) .LT. 273.149) THEN




              BX = BETA(ISLTPK(I,J))
              IF ( BETA(ISLTPK(I,J)) .GT. BLIM ) BX = BLIM

        if ( GRAV*(-PSIS(ISLTPK(I,J))) .eq. 0 ) then
        write(message,*) 'TROUBLE'
        CALL wrf_message(message)
        write(message,*) 'I,J: ', i,J
        CALL wrf_message(message)
        write(message,*) 'grav, isltpk, psis(isltpk): ', grav,isltpk(I,J),&
                 psis(isltpk(I,J))
        CALL wrf_message(message)
        endif

        if (BX .eq. 0 .or. STC(I,K,J) .eq. 0) then
                write(message,*) 'TROUBLE -- I,J,BX, STC: ', I,J,BX,STC(I,K,J)
                CALL wrf_message(message)
        endif
              FK = (((HLICE/(GRAV*(-PSIS(ISLTPK(I,J)))))* &
                  ((STC(I,K,J)-T0)/STC(I,K,J)))** &
                  (-1/BX))*SMCMAX(ISLTPK(I,J))
              IF (FK .LT. 0.02) FK = 0.02
              SH2O(I,K,J) = MIN ( FK, SMC(I,K,J) )





              SH2O(I,K,J)=FRH2O_init(STC(I,K,J),SMC(I,K,J),SH2O(I,K,J), &
                         SMCMAX(ISLTPK(I,J)),BETA(ISLTPK(I,J)), &
                         PSIS(ISLTPK(I,J)))

            ELSE 
              SH2O(I,K,J)=SMC(I,K,J)
            ENDIF


        ELSE   
              SH2O(I,K,J)=SMC(I,K,J)

        ENDIF 
        if (SH2O(I,K,J) .gt. SMCMAX(ISLTPK(I,J))) then
          write(message,*) 'SH2O > THAN SMCMAX ', I,J,SH2O(I,K,J),SMCMAX(ISLTPK(I,J)),SMC(I,K,J)
          CALL wrf_message(message)
        endif

         ENDDO
        ENDDO
       ENDDO

        END SUBROUTINE NMM_SH2O



      FUNCTION FRH2O_init(TKELV,SMC,SH2O,SMCMAX,B,PSIS)

      IMPLICIT NONE































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

      PARAMETER (BLIM=5.5)

      PARAMETER (ERROR=0.005)

      PARAMETER (HLICE=3.335E5)
      PARAMETER (GS = 9.81)
      PARAMETER (DICE=920.0)
      PARAMETER (DH2O=1000.0)
      PARAMETER (T0=273.15)






      BX = B
      IF ( B .GT. BLIM ) BX = BLIM



      NLOG=0
      KCOUNT=0






      IF (TKELV .GT. (T0 - 1.E-3)) THEN

        FRH2O_init=SMC

      ELSE


       IF (CK .NE. 0.0) THEN






        SWL = SMC-SH2O

         IF (SWL .GT. (SMC-0.02)) SWL=SMC-0.02
         IF(SWL .LT. 0.) SWL=0.



        DO WHILE (NLOG .LT. 10 .AND. KCOUNT .EQ. 0)
         NLOG = NLOG+1
         DF = ALOG(( PSIS*GS/HLICE ) * ( ( 1.+CK*SWL )**2. ) * &
             ( SMCMAX/(SMC-SWL) )**BX) - ALOG(-(TKELV-T0)/TKELV)
         DENOM = 2. * CK / ( 1.+CK*SWL ) + BX / ( SMC - SWL )
         SWLK = SWL - DF/DENOM

         IF (SWLK .GT. (SMC-0.02)) SWLK = SMC - 0.02
         IF(SWLK .LT. 0.) SWLK = 0.

         DSWL=ABS(SWLK-SWL)
         SWL=SWLK




         IF ( DSWL .LE. ERROR )  THEN
           KCOUNT=KCOUNT+1
         END IF
        END DO




        FRH2O_init = SMC - SWL



       ENDIF

       IF (KCOUNT .EQ. 0) THEN






        FK=(((HLICE/(GS*(-PSIS)))*((TKELV-T0)/TKELV))**(-1/BX))*SMCMAX

        IF (FK .LT. 0.02) FK = 0.02
        FRH2O_init = MIN ( FK, SMC )



       ENDIF

      ENDIF

        RETURN

      END FUNCTION FRH2O_init




   SUBROUTINE init_module_initialize
   END SUBROUTINE init_module_initialize



END MODULE module_initialize_real
