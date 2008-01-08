!-----------------------------------------------------------------------
!
!NCEP_MESO:MEDIATION_LAYER:SOLVER
!
!-----------------------------------------------------------------------
! these define the various loop range variables
! that were defined in module_MPP. Defined as macros
! here to allow thread-safety/tile callability


! these define the various loop range variables
! that were defined in module_MPP. Defined as macros
! here to allow thread-safety/tile callability





!-----------------------------------------------------------------------
!
      SUBROUTINE SOLVE_NMM(GRID,CONFIG_FLAGS                            &
!
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/nmm_dummy_args.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
,lu_index,lu_mask,p_gc,vegcat,soilcat,input_soil_cat,tsk_gc,xice_gc,ght_gc,rh_gc,v_gc,u_gc,t_gc,rwmr_gc,snmr_gc,clwmr_gc,cice_gc, &
rimef_gc,snoalb,greenfrac_gc,albedo12m_gc,soilcbot_gc,soilctop_gc,tmn_gc,htv_gc,ht_gc,landusef_gc,vlon_gc,vlat_gc,hlon_gc, &
hlat_gc,hbm2,hbm3,vbm2,vbm3,sm,sice,pd,pd_b,pd_bt,fis,res,t,t_b,t_bt,q,q_b,q_bt,u,u_b,u_bt,v,v_b,v_bt,told,uold,vold,dx_nmm, &
wpdar,cpgfu,curv,fcp,fdiv,f,fad,ddmpu,ddmpv,deta,rdeta,aeta,f4q2,etax,dfl,deta1,aeta1,eta1,deta2,aeta2,eta2,em,emt,adt,adu,adv, &
em_loc,emt_loc,pdsl,pdslo,psdt,div,few,fne,fns,fse,omgalf,petdt,rtop,pblh,lpbl,ustar,z0,z0base,ths,mavail,qsh,twbs,qwbs,prec, &
aprec,acprec,cuprec,lspa,ddata,accliq,sno,si,cldefi,deep,rf,th10,q10,pshltr,tshltr,qshltr,q2,q2_b,q2_bt,t_adj,t_old,zero_3d, &
w0avg,akhs_out,akms_out,albase,albedo,cnvbot,cnvtop,czen,czmean,epsr,gffc,glat,glon,nmm_tsk,hdac,hdacv,mxsnal,radin,radot,sigt4, &
tg,dfrlg,lvl,cwm,cwm_b,cwm_bt,f_ice,f_rain,f_rimef,cldfra,sr,cfrach,cfracl,cfracm,islope,dzsoil,rtdpth,sldpth,cmc,grnflx,pctsno, &
soiltb,vegfrc,shdmin,shdmax,sh2o,smc,stc,dwdtmn,dwdtmx,dwdt,pdwdt,pint,w,z,acfrcv,acfrst,ssroff,bgroff,rlwin,rlwout,rlwtoa, &
alwin,alwout,alwtoa,rswin,rswinc,rswout,rswtoa,aswin,aswout,aswtoa,sfcshx,sfclhx,subshx,snopcx,sfcuvx,potevp,potflx,tlmin,tlmax, &
rlwtt,rswtt,tcucn,train,ncfrcv,ncfrst,ihe,ihw,ive,ivw,irad,iheg,ihwg,iveg,ivwg,iradg,n_iup_h,n_iup_v,n_iup_adh,n_iup_adv,iup_h, &
iup_v,iup_adh,iup_adv,imask_nostag,imask_xstag,imask_ystag,imask_xystag,sm000007,sm007028,sm028100,sm100255,st000007,st007028, &
st028100,st100255,sm000010,sm010040,sm040100,sm100200,sm010200,soilm000,soilm005,soilm020,soilm040,soilm160,soilm300,sw000010, &
sw010040,sw040100,sw100200,sw010200,soilw000,soilw005,soilw020,soilw040,soilw160,soilw300,st000010,st010040,st040100,st100200, &
st010200,soilt000,soilt005,soilt020,soilt040,soilt160,soilt300,landmask,topostdv,toposlpx,toposlpy,greenmax,greenmin,albedomx, &
slopecat,toposoil,landusef,soilctop,soilcbot,moist,scalar,scalar_b,scalar_bt,chem,smois,tslb,gsw,xlat,xlong,xland,raincv,psfc, &
th2,t2,u10,v10,xice,smstav,smstot,sfcrunoff,udrunoff,ivgtyp,isltyp,vegfra,sfcevp,grdflx,albbck,sfcexc,acsnow,acsnom,rmol,snow, &
canwat,sst,weasd,znt,mol,tke_myj,el_myj,exch_h,thz0,qz0,uz0,vz0,flhc,flqc,qsg,qvg,qcg,soilt1,tsnav,psfc_out,uz0h,vz0h,dudt,dvdt, &
qsfc,akhs,akms,htop,hbot,htopr,hbotr,htopd,hbotd,htops,hbots,cuppt,cprate,f_ice_phy,f_rain_phy,f_rimef_phy,mass_flux,apr_gr, &
apr_w,apr_mc,apr_st,apr_as,apr_capma,apr_capme,apr_capmi,xf_ens,pr_ens,rthften,rqvften,snowh,rhosn,smfr3d,keepfr3dflag, &
mp_restart_state,tbpvs_state,tbpvs0_state,lu_state &
!ENDOFREGISTRYGENERATEDINCLUDE
!
     &           )
!-----------------------------------------------------------------------
      USE MODULE_DOMAIN
      USE MODULE_CONFIGURE
      USE MODULE_MODEL_CONSTANTS
      USE MODULE_STATE_DESCRIPTION
      USE MODULE_CTLBLK
      USE MODULE_DM
      USE MODULE_IGWAVE_ADJUST,		ONLY: PDTE,PFDHT,DDAMP,VTOA
      USE MODULE_ADVECTION,		ONLY: ADVE,VAD2,HAD2,VAD2_SCAL,HAD2_SCAL
      USE MODULE_NONHY_DYNAM,		ONLY: EPS,VADZ,HADZ
      USE MODULE_DIFFUSION_NMM, 	ONLY: HDIFF
      USE MODULE_BNDRY_COND,		ONLY: BOCOH,BOCOV
      USE MODULE_PHYSICS_CALLS
      USE MODULE_EXCHANGE,              ONLY: HALO_EXCH
      USE MODULE_EXT_INTERNAL
      USE MODULE_PRECIP_ADJUST
      USE MODULE_NEST_UTIL     ! USEs module_MPP (contains MYPE,NPES,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
      INCLUDE "mpif.h"
!-----------------------------------------------------------------------
!
!***  INPUT DATA
!
!-----------------------------------------------------------------------
!
      TYPE(DOMAIN),TARGET :: GRID
!
!***  DEFINITIONS OF DUMMY ARGUMENTS TO THIS ROUTINE (GENERATED FROM REGISTRY)
!
! NOTE, REGISTRY NO LONGER GENERATES DUMMY ARGUMENTS OR DUMMY ARGUMENT
! DECLARATIONS FOR RCONFIG ENTRIES. THEY ARE STILL PART OF STATE. ACCESS
! TO THESE VARIABLES IS NOW THROUGH GRID STRUCTURE, AS MODIFIED BELOW.
! AFFECTED VARIABLES: SIGMA, DT, NPHS, IDTAD, NRADS, NRADL, JULDAY,
! JULYR, NUM_SOIL_LAYERS, NCNVC, ENSDIM, DY, AND SPEC_BDY_WIDTH.
! JM, 20050819
!
!----------------------------
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/nmm_dummy_decl.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
integer                                  :: ntsd
integer                                  :: nstart_hour
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
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,1:grid%num_metgrid_levels)           :: rwmr_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,1:grid%num_metgrid_levels)           :: snmr_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,1:grid%num_metgrid_levels)           :: clwmr_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,1:grid%num_metgrid_levels)           :: cice_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,1:grid%num_metgrid_levels)           :: rimef_gc
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
real      ,DIMENSION(max(grid%ed31,grid%ed32),1,grid%spec_bdy_width,4)           :: pd_b
real      ,DIMENSION(max(grid%ed31,grid%ed32),1,grid%spec_bdy_width,4)           :: pd_bt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: fis
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: res
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: t
real      ,DIMENSION(max(grid%ed31,grid%ed32),grid%sd33:grid%ed33,grid%spec_bdy_width,4)           :: t_b
real      ,DIMENSION(max(grid%ed31,grid%ed32),grid%sd33:grid%ed33,grid%spec_bdy_width,4)           :: t_bt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: q
real      ,DIMENSION(max(grid%ed31,grid%ed32),grid%sd33:grid%ed33,grid%spec_bdy_width,4)           :: q_b
real      ,DIMENSION(max(grid%ed31,grid%ed32),grid%sd33:grid%ed33,grid%spec_bdy_width,4)           :: q_bt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: u
real      ,DIMENSION(max(grid%ed31,grid%ed32),grid%sd33:grid%ed33,grid%spec_bdy_width,4)           :: u_b
real      ,DIMENSION(max(grid%ed31,grid%ed32),grid%sd33:grid%ed33,grid%spec_bdy_width,4)           :: u_bt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: v
real      ,DIMENSION(max(grid%ed31,grid%ed32),grid%sd33:grid%ed33,grid%spec_bdy_width,4)           :: v_b
real      ,DIMENSION(max(grid%ed31,grid%ed32),grid%sd33:grid%ed33,grid%spec_bdy_width,4)           :: v_bt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: told
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: uold
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: vold
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
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: ustar
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: z0
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: z0base
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: ths
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: mavail
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: qsh
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: twbs
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: qwbs
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
real      ,DIMENSION(max(grid%ed31,grid%ed32),grid%sd33:grid%ed33,grid%spec_bdy_width,4)           :: q2_b
real      ,DIMENSION(max(grid%ed31,grid%ed32),grid%sd33:grid%ed33,grid%spec_bdy_width,4)           :: q2_bt
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
real      ,DIMENSION(max(grid%ed31,grid%ed32),grid%sd33:grid%ed33,grid%spec_bdy_width,4)           :: cwm_b
real      ,DIMENSION(max(grid%ed31,grid%ed32),grid%sd33:grid%ed33,grid%spec_bdy_width,4)           :: cwm_bt
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
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rlwtt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rswtt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: tcucn
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: train
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: ncfrcv
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: ncfrst
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
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: moist
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_scalar)           :: scalar
real      ,DIMENSION(max(grid%ed31,grid%ed32),grid%sd33:grid%ed33,grid%spec_bdy_width,4,num_scalar)           :: scalar_b
real      ,DIMENSION(max(grid%ed31,grid%ed32),grid%sd33:grid%ed33,grid%spec_bdy_width,4,num_scalar)           :: scalar_bt
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
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: acsnow
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: acsnom
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: rmol
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: snow
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: canwat
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sst
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: weasd
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: znt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: mol
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: tke_myj
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32)           :: el_myj
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32)           :: exch_h
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
!----------------------------
!
!***  STRUCTURE THAT CONTAINS RUN-TIME CONFIGURATION (NAMELIST) DATA FOR DOMAIN
!
      TYPE(GRID_CONFIG_REC_TYPE),INTENT(IN) :: CONFIG_FLAGS
!
!-----------------------------------------------------------------------
!
!***  LOCAL VARIABLES
!
!-----------------------------------------------------------------------
      INTEGER :: IDS,IDE,JDS,JDE,KDS,KDE                                &
     &          ,IMS,IME,JMS,JME,KMS,KME                                & 
     &          ,IPS,IPE,JPS,JPE,KPS,KPE                                &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE
!
      INTEGER :: I,ICLTEND,IDF,IJDE,IJDS,IRTN,J,JC,JDF,K,KDF,LB,N_MOIST &
     &          ,NTSD_current
      integer :: ierr
      INTEGER,SAVE :: NTSD_restart
!     INTEGER :: MPI_COMM_COMP,MYPE,MYPROC,NPES
      INTEGER :: MYPROC
      INTEGER :: KVH,NTSD_rad,RC
      INTEGER :: NUM_OZMIXM,NUM_AEROSOLC
!
      REAL :: DT_INV,FICE,FRAIN,GPS,QI,QR,QW,WC
!
      LOGICAL :: LAST_TIME,OPERATIONAL_PHYSICS
!
      CHARACTER(80) :: MESSAGE
!
!***  For precip assimilation:
      INTEGER :: ISTAT
      REAL,ALLOCATABLE,SAVE,DIMENSION(:,:,:) :: PPTDAT
!
!-----------------------------------------------------------------------
!***  For physics compatibility with other packages
!-----------------------------------------------------------------------
!
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: TTEN,QTEN
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: RTHRATEN,RTHBLTEN,RQVBLTEN
!
!-----------------------------------------------------------------------
!***  These must be in place due to assimilation cycling.
!-----------------------------------------------------------------------
!
      REAL,ALLOCATABLE,DIMENSION(:,:)      :: HDAC_BIG,HDACV_BIG
      REAL,ALLOCATABLE,DIMENSION(:,:),SAVE :: DDMPU_LOC,DDMPV_LOC
!
      REAL,SAVE :: DDFC=8.
!
!-----------------------------------------------------------------------
!
      LOGICAL wrf_dm_on_monitor
      EXTERNAL wrf_dm_on_monitor
!
!-----------------------------------------------------------------------
!***  TIMING VARIABLES
!-----------------------------------------------------------------------
      real,save :: solve_tim,exch_tim,pdte_tim,adve_tim,vtoa_tim        &
     &,            vadz_tim,hadz_tim,eps_tim,vad2_tim,had2_tim          &
     &,            radiation_tim,rdtemp_tim,turbl_tim,cltend_tim        &
     &,            cucnvc_tim,gsmdrive_tim,hdiff_tim,bocoh_tim          &
     &,            pfdht_tim,ddamp_tim,bocov_tim,uv_htov_tim,sum_tim    &
     &,            adjppt_tim
      real,save :: exch_tim_max
      real :: btim,btimx
      real :: et_max,this_tim
      integer :: n_print_time
!
      real*8 :: timef
!-----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
!
! LIMIT THE NUMBER OF ARGUMENTS IF COMPILED WITH -DLIMIT_ARGS BY COPYING
! SCALAR (NON-ARRAY) ARGUMENTS OUT OF THE GRID DATA STRUCTURE INTO LOCALLY
! DEFINED COPIES (DEFINED IN EM_DUMMY_DECL.INC, ABOVE, AS THEY ARE IF THEY
! ARE ARGUMENTS).  AN EQUIVALENT INCLUDE OF EM_SCALAR_DEREFS.INC APPEARS
! AT THE END OF THE ROUTINE TO COPY BACK ANY CHNAGED NON-ARRAY VALUES.
! THE DEFINITION OF COPY_IN OR COPY_OUT BEFORE THE INCLUDE DEFINES THE
! DIRECTION OF THE COPY.  NMM_SCALAR_DEREFS IS GENERATED FROM REGISTRY.
!
!-----------------------------------------------------------------------
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/nmm_scalar_derefs.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
! BEGIN SCALAR DEREFS
 ntsd = grid%nmm_ntsd
 nstart_hour = grid%nmm_nstart_hour
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
 nphs0 = grid%nmm_nphs0
 nprec = grid%nmm_nprec
 nclod = grid%nmm_nclod
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
! END SCALAR DEREFS
!ENDOFREGISTRYGENERATEDINCLUDE
!-----------------------------------------------------------------------
!
! TRICK PROBLEMATIC COMPILERS INTO NOT PERFORMING COPY-IN/COPY-OUT BY ADDING
! INDICES TO ARRAY ARGUMENTS IN THE CALL STATEMENTS IN THIS ROUTINE.
! IT HAS THE EFFECT OF PASSING ONLY THE FIRST ELEMENT OF THE ARRAY, RATHER
! THAN THE ENTIRE ARRAY.  SEE:
! http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
!
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!
! NEEDED BY SOME COMM LAYERS, E.G. RSL.  IF NEEDED, nmm_data_calls.inc IS
! GENERATED FROM THE REGISTRY.  THE DEFINITION OF REGISTER_I1 ALLOWS
! I1 DATA TO BE COMMUNICATED IN THIS ROUTINE IF NECESSARY.
!
!-----------------------------------------------------------------------
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/nmm_data_calls.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
!ENDOFREGISTRYGENERATEDINCLUDE
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
      CALL WRF_GET_MYPROC(MYPROC)
      MYPE=MYPROC
!-----------------------------------------------------------------------
!
!***  OBTAIN DIMENSION INFORMATION STORED IN THE GRID DATA STRUCTURE.
!
      CALL GET_IJK_FROM_GRID(GRID                                       &
     &                      ,IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                      ,IMS,IME,JMS,JME,KMS,KME                    &
     &                      ,IPS,IPE,JPS,JPE,KPS,KPE )
!-----------------------------------------------------------------------
!
!***  COMPUTE THESE STARTING AND STOPPING LOCATIONS FOR EACH TILE AND
!***  NUMBER OF TILES.
!***  SEE: http://www.mmm.ucar.edu/wrf/WG2/topics/settiles
!
      CALL SET_TILES(GRID,IDS,IDE,JDS,JDE,IPS,IPE,JPS,JPE)
!
!-----------------------------------------------------------------------
!***  SET FLAG FOR THE OPERATIONAL PHYSICS SUITE.
!***  THIS WILL BE USED TO SAVE CLOCKTIME BY SKIPPING
!***  FREQUENT UPDATES OF THE MOIST ARRAY AND INSTEAD
!***  UPDATE IT ONLY WHEN IT IS NEEDED FOR PHYSICS.
!-----------------------------------------------------------------------
!
      OPERATIONAL_PHYSICS=.FALSE.
!
      IF(CONFIG_FLAGS%RA_SW_PHYSICS    ==GFDLSWSCHEME.AND.              &
     &   CONFIG_FLAGS%RA_LW_PHYSICS    ==GFDLLWSCHEME.AND.              &
     &   CONFIG_FLAGS%SF_SFCLAY_PHYSICS==MYJSFCSCHEME.AND.              &
     &   CONFIG_FLAGS%BL_PBL_PHYSICS   ==MYJPBLSCHEME.AND.              &
     &   CONFIG_FLAGS%CU_PHYSICS       ==BMJSCHEME.AND.                 &
     &   CONFIG_FLAGS%MP_PHYSICS       ==ETAMPNEW)THEN
!
        OPERATIONAL_PHYSICS=.TRUE.
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!***  TTEN, QTEN are used by GD convection scheme
!
      ALLOCATE(TTEN(IMS:IME,KMS:KME,JMS:JME),STAT=ISTAT)
      ALLOCATE(QTEN(IMS:IME,KMS:KME,JMS:JME),STAT=ISTAT)
      ALLOCATE(RTHBLTEN(IMS:IME,KMS:KME,JMS:JME),STAT=ISTAT)
      ALLOCATE(RQVBLTEN(IMS:IME,KMS:KME,JMS:JME),STAT=ISTAT)
      ALLOCATE(RTHRATEN(IMS:IME,KMS:KME,JMS:JME),STAT=ISTAT)
!
!
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
!
      GRID%SIGMA=1 
      HYDRO=.FALSE.
!
      IJDS=MIN(IDS,JDS)
      IJDE=MAX(IDE,JDE)
!
      IDF=IDE-1
      JDF=JDE-1
      KDF=KDE-1
!
!-----------------------------------------------------------------------
!
!***  FOR NOW SET CONTROLS FOR TILES TO PATCHES
!
!-----------------------------------------------------------------------
      ITS=IPS
      ITE=MIN(IPE,IDF)
      JTS=JPS
      JTE=MIN(JPE,JDF)
      KTS=KPS
      KTE=MIN(KPE,KDF)
      if(ntsd==0)then
        write(0,*)' its=',its,' ite=',ite
        write(0,*)' jts=',jts,' jte=',jte
        write(0,*)' kts=',kts,' kte=',kte
      endif
!-----------------------------------------------------------------------
!***  SET TIMING VARIABLES TO ZERO AT START OF FORECAST.
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
      N_MOIST=NUM_MOIST
!
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
      TSPH=3600./GRID%DT
!
      n_print_time=nint(3600./grid%dt)   ! Print stats once per hour
!-----------------------------------------------------------------------
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
!***
!***  ADVANCE_count STARTS AT ZERO FOR ALL RUNS (REGULAR AND RESTART).
!***
!-----------------------------------------------------------------------
!
      CALL DOMAIN_CLOCK_GET(GRID,ADVANCEcOUNT=NTSD_current)
!
      IF(NTSD_current==0)THEN
        IF(GRID%RESTART.AND.GRID%TSTART>0.)THEN
          IHRST=NSTART_HOUR
          NTSD_restart=NTSD+1
        ELSE
          IHRST=GRID%GMT
          NSTART_HOUR=IHRST
          NTSD_restart=0
        ENDIF
      ENDIF
!
      NTSD=NTSD_restart+NTSD_current
      LAST_TIME=domain_last_time_step(GRID)

!!!
!	IF (NTSD == 1) THEN
!	CALL START()
!        ENDIF
!!!
!
!-----------------------------------------------------------------------
!
!!!!! IF(WRF_DM_ON_MONITOR() )THEN
        WRITE(MESSAGE,125)NTSD,NTSD*GRID%DT/3600.
  125   FORMAT(' SOLVE_NMM: TIMESTEP IS ',I5,'   TIME IS ',F7.3,' HOURS')
        CALL WRF_MESSAGE(TRIM(MESSAGE))
!!!!  ENDIF
!
!-----------------------------------------------------------------------
!
      CALL WRF_GET_DM_COMMUNICATOR(MPI_COMM_COMP)
      CALL WRF_GET_NPROC(NPES)
!
!
!-----------------------------------------------------------------------
!***  ALLOCATE PPTDAT ARRAY (PRECIP ASSIM):
!-----------------------------------------------------------------------
!
      IF(GRID%PCPFLG.AND..NOT.ALLOCATED(PPTDAT))THEN
        ALLOCATE(PPTDAT(IMS:IME,JMS:JME,3),STAT=ISTAT)
      ENDIF
!
!-----------------------------------------------------------------------
!***
!***      Call READPCP to
!***            1) READ IN PRECIPITATION FOR HOURS 1, 2 and 3;
!***            2) Initialize DDATA to 999. (this is the amount
!***               of input precip allocated to each physics time step
!***               in ADJPPT; TURBL/SURFCE, which uses DDATA, is called
!***               before ADJPPT)
!***            3) Initialize LSPA to zero
!***
!-----------------------------------------------------------------------
      IF (NTSD==0) THEN
        IF (GRID%PCPFLG) THEN
          CALL READPCP(PPTDAT,DDATA,LSPA                                &
     &      ,IDS,IDE,JDS,JDE,KDS,KDE                                    &
     &      ,IMS,IME,JMS,JME,KMS,KME                                    &
     &      ,ITS,ITE,JTS,JTE,KTS,KTE)
        ENDIF
      ENDIF
!-----------------------------------------------------------------------
!
      btim=timef()
!
!-----------------------------------------------------------------------
!***  ZERO OUT ACCUMULATED QUANTITIES WHEN NEEDED.
!-----------------------------------------------------------------------
!
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
     &            ,T,TLMAX,TLMIN                                        &
     &            ,IDS,IDE,JDS,JDE,KDS,KDE                              &
     &            ,IMS,IME,JMS,JME,KMS,KME                              &
     &            ,ITS,ITE,JTS,JTE,KTS,KTE)
!-----------------------------------------------------------------------
!
      IF(NTSD==0)THEN
        FIRST=.TRUE.
!       call hpm_init()
        btimx=timef()
!
!-----------------------------------------------------------------------
!#    include "HALO_NMM_A.inc"
        CALL HALO_EXCH(PD,1,T,KTE,U,KTE,V,KTE,Q,KTE,CWM,KTE,DWDT,KTE    &
     &                ,DIV,KTE,PINT,KTE+1,2,2                           &
     &                ,MYPE,MPI_COMM_COMP                               &
     &                ,IDS,IDE,JDS,JDE,KDS,KDE                          &
     &                ,IMS,IME,JMS,JME,KMS,KME                          &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)
!
!-----------------------------------------------------------------------
      IF(CONFIG_FLAGS%MP_PHYSICS/=ETAMPNEW)THEN
!#    include "HALO_NMM_A_3.inc"
        CALL HALO_EXCH(MOIST,KTE,N_MOIST,2,2                            &
     &                ,MYPE,MPI_COMM_COMP                               &
     &                ,IDS,IDE,JDS,JDE,KDS,KDE                          &
     &                ,IMS,IME,JMS,JME,KMS,KME                          &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)
        CALL HALO_EXCH(SCALAR,KTE,NUM_SCALAR,2,2                        &
     &                ,MYPE,MPI_COMM_COMP                               &
     &                ,IDS,IDE,JDS,JDE,KDS,KDE                          &
     &                ,IMS,IME,JMS,JME,KMS,KME                          &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)
      ENDIF
!-----------------------------------------------------------------------
!
!***  Only for chemistry:
!
!
!-----------------------------------------------------------------------
!***  USE THE FOLLOWING VARIABLES TO KEEP TRACK OF EXCHANGE TIMES.
!-----------------------------------------------------------------------
        exch_tim=exch_tim+timef()-btimx
!       this_tim=timef()-btimx
!       call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max           &
!    &                    ,mpi_comm_comp,irtn)
!       exch_tim_max=exch_tim_max+et_max
!-----------------------------------------------------------------------
!
        GO TO 2003
      ENDIF
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
!-----------------
!#    include "HALO_NMM_D.inc"
      CALL HALO_EXCH(PD,1,2,2 &
     &              ,MYPE,MPI_COMM_COMP                                 &
     &              ,IDS,IDE,JDS,JDE,KDS,KDE                            &
     &              ,IMS,IME,JMS,JME,KMS,KME                            &
     &              ,ITS,ITE,JTS,JTE,KTS,KTE)
!-----------------
      exch_tim=exch_tim+timef()-btimx
!     this_tim=timef()-btimx
!     call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max             &
!    &                  ,mpi_comm_comp,irtn)
!     exch_tim_max=exch_tim_max+et_max
!
      btimx=timef()
!
      CALL PDTE(                                                        &
     &            GRID,MYPE,MPI_COMM_COMP                               &
     &           ,NTSD,GRID%DT,PT,ETA2,RES,HYDRO,HBM2                   &
     &           ,PD,PDSL,PDSLO                                         &
     &           ,PETDT,DIV,PSDT                                        &
     &           ,IHE,IHW,IVE,IVW                                       &
     &           ,IDS,IDF,JDS,JDF,KDS,KDE                               &
     &           ,IMS,IME,JMS,JME,KMS,KME                               &
     &           ,ITS,ITE,JTS,JTE,KTS,KTE)

      pdte_tim=pdte_tim+timef()-btimx
!
!-----------------------------------------------------------------------
!***  ADVECTION OF T, U, AND V
!-----------------------------------------------------------------------
!
      btimx=timef()
!-----------------
!#    include "HALO_NMM_F.inc"
!#    include "HALO_NMM_F1.inc"
      CALL HALO_EXCH(T,KTE,U,KTE,V,KTE,2,2                              &
     &              ,MYPE,MPI_COMM_COMP                                 &
     &              ,IDS,IDE,JDS,JDE,KDS,KDE                            &
     &              ,IMS,IME,JMS,JME,KMS,KME                            &
     &              ,ITS,ITE,JTS,JTE,KTS,KTE)
      CALL HALO_EXCH(PDSLO,1,4,4                                        &
     &              ,MYPE,MPI_COMM_COMP                                 &
     &              ,IDS,IDE,JDS,JDE,KDS,KDE                            &
     &              ,IMS,IME,JMS,JME,KMS,KME                            &
     &              ,ITS,ITE,JTS,JTE,KTS,KTE)
!-----------------
      exch_tim=exch_tim+timef()-btimx
!     this_tim=timef()-btimx
!     call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max             &
!    &                  ,mpi_comm_comp,irtn)
!     exch_tim_max=exch_tim_max+et_max
      btimx=timef()
!
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
!
      adve_tim=adve_tim+timef()-btimx
!
!-----------------------------------------------------------------------
!***  PRESSURE TENDENCY, ETA/SIGMADOT, VERTICAL PART OF OMEGA-ALPHA TERM
!-----------------------------------------------------------------------
!
      btimx=timef()
!
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
!
      vtoa_tim=vtoa_tim+timef()-btimx
!
!-----------------------------------------------------------------------
!***  VERTICAL ADVECTION OF HEIGHT
!-----------------------------------------------------------------------
!
      btimx=timef()
!
      CALL VADZ(NTSD,GRID%DT,FIS,GRID%SIGMA,DFL,HBM2                    &
     &         ,DETA1,DETA2,PDTOP                                       &
     &         ,PINT,PDSL,PDSLO,PETDT                                   &
     &         ,RTOP,T,Q,CWM,Z,W,DWDT,PDWDT                             &
     &         ,IHE,IHW,IVE,IVW                                         &
     &         ,IDS,IDF,JDS,JDF,KDS,KDE                                 &
     &         ,IMS,IME,JMS,JME,KMS,KME                                 &
     &         ,ITS,ITE,JTS,JTE,KTS,KTE)

      vadz_tim=vadz_tim+timef()-btimx
!
!-----------------------------------------------------------------------
!***  HORIZONTAL ADVECTION OF HEIGHT
!-----------------------------------------------------------------------
!
      btimx=timef()
!-----------------
!#    include "HALO_NMM_G.inc"
      CALL HALO_EXCH(U,KTE,V,KTE,Z,KTE+1,2,2                            &
     &              ,MYPE,MPI_COMM_COMP                                 &
     &              ,IDS,IDE,JDS,JDE,KDS,KDE                            &
     &              ,IMS,IME,JMS,JME,KMS,KME                            &
     &              ,ITS,ITE,JTS,JTE,KTS,KTE)
!-----------------
      exch_tim=exch_tim+timef()-btimx
!     this_tim=timef()-btimx
!     call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max             &
!    &                  ,mpi_comm_comp,irtn)
!     exch_tim_max=exch_tim_max+et_max
!
      btimx=timef()
!
      CALL HADZ(NTSD,GRID%DT,HYDRO,HBM2,DETA1,DETA2,PDTOP               &
     &         ,DX_NMM,DY_NMM,FAD                                       &
     &         ,FEW,FNS,FNE,FSE                                         &
     &         ,PDSL,U,V,W,Z                                            &
     &         ,IHE,IHW,IVE,IVW                                         &
     &         ,IDS,IDF,JDS,JDF,KDS,KDE                                 &
     &         ,IMS,IME,JMS,JME,KMS,KME                                 &
     &         ,ITS,ITE,JTS,JTE,KTS,KTE)
!
      hadz_tim=hadz_tim+timef()-btimx
!
!-----------------------------------------------------------------------
!***  ADVECTION OF W
!-----------------------------------------------------------------------
!
      btimx=timef()
!-----------------
!#    include "HALO_NMM_H.inc"
      CALL HALO_EXCH(W,KTE+1,2,2                                        &
     &              ,MYPE,MPI_COMM_COMP                                 &
     &              ,IDS,IDE,JDS,JDE,KDS,KDE                            &
     &              ,IMS,IME,JMS,JME,KMS,KME                            &
     &              ,ITS,ITE,JTS,JTE,KTS,KTE)
!-----------------
      exch_tim=exch_tim+timef()-btimx
!     this_tim=timef()-btimx
!     call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max             &
!    &                  ,mpi_comm_comp,irtn)
!     exch_tim_max=exch_tim_max+et_max
!
      btimx=timef()
!
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
!
      eps_tim=eps_tim+timef()-btimx
!
!-----------------------------------------------------------------------
!***  VERTICAL ADVECTION OF Q, TKE, AND CLOUD WATER
!-----------------------------------------------------------------------
!
      IF(MOD(NTSD,GRID%IDTAD)==0)THEN
        btimx=timef()
!
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
!
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
!
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
!

          DO K=KTS,KTE
          DO J=max(jds+(0),jts-(0)),min(jde-(0),jte+(0))
          DO I=max(ids+(0),its-(0)),min(ide-(0),ite+(0))
            Q(I,J,K)=MOIST(I,J,K,P_QV)/(1.+MOIST(I,J,K,P_QV))
          ENDDO
          ENDDO   
          ENDDO   
!
        ENDIF vad2_micro_check
!
        vad2_tim=vad2_tim+timef()-btimx
!
      ENDIF
!    
!-----------------------------------------------------------------------
!***  VERTICAL ADVECTION OF CHEMISTRY
!-----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
!***  HORIZONTAL ADVECTION OF Q, TKE, AND CLOUD WATER
!-----------------------------------------------------------------------
!
      IF(MOD(NTSD,GRID%IDTAD)==0)THEN
        btimx=timef()
!-----------------
!#    include "HALO_NMM_I.inc"
        CALL HALO_EXCH(Q,KTE,Q2,KTE,CWM,KTE,3,3                         &
     &                ,MYPE,MPI_COMM_COMP                               &
     &                ,IDS,IDE,JDS,JDE,KDS,KDE                          &
     &                ,IMS,IME,JMS,JME,KMS,KME                          &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)
!
        IF(CONFIG_FLAGS%MP_PHYSICS/=ETAMPNEW)THEN
!#    include "HALO_NMM_I_3.inc"
        CALL HALO_EXCH(MOIST,KTE,N_MOIST,3,3                            &
     &                ,MYPE,MPI_COMM_COMP                               &
     &                ,IDS,IDE,JDS,JDE,KDS,KDE                          &
     &                ,IMS,IME,JMS,JME,KMS,KME                          &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)
        CALL HALO_EXCH(SCALAR,KTE,NUM_SCALAR,3,3                        &
     &                ,MYPE,MPI_COMM_COMP                               &
     &                ,IDS,IDE,JDS,JDE,KDS,KDE                          &
     &                ,IMS,IME,JMS,JME,KMS,KME                          &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)
        ENDIF
!
!-----------------
        exch_tim=exch_tim+timef()-btimx
!       this_tim=timef()-btimx
!       call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max           &
!    &                    ,mpi_comm_comp,irtn)
!       exch_tim_max=exch_tim_max+et_max
!
        btimx=timef()
!
!-----------------------------------------------------------------------
        had2_micro_check: IF(CONFIG_FLAGS%MP_PHYSICS==ETAMPNEW)THEN
!-----------------------------------------------------------------------
!
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
!
!***  UPDATE MOIST ARRAY.
!***  REMEMBER THAT MOIST IS ONLY USED WITH THE PHYSICS AND THUS
!***  THE P_QV SLOT (=2) IS MIXING RATIO, NOT SPECIFIC HUMIDITY.
!***  ALTHOUGH MOIST IS ONLY USED FOR PHYSICS IN OPERATIONS, IT IS 
!***  UPDATED HERE FROM Q EVERY ADVECTION TIMESTEP FOR NON-OPERATIONAL
!***  CONFIGURATIONS WHERE IT MAY BE USED OUTSIDE OF THE PHYSICS.
!
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
!
              IF(FICE>=1.)THEN
                QI=WC
              ELSEIF(FICE<=0.)THEN
                QW=WC
              ELSE
                QI=FICE*WC
                QW=WC-QI
              ENDIF
!
              IF(QW>0..AND.FRAIN>0.)THEN
                IF(FRAIN>=1.)THEN
                  QR=QW
                  QW=0.
                ELSE
                  QR=FRAIN*QW
                  QW=QW-QR
                ENDIF
              ENDIF
!
              MOIST(I,J,K,P_QC)=QW
              MOIST(I,J,K,P_QR)=QR
              MOIST(I,J,K,P_QI)=0.
              MOIST(I,J,K,P_QS)=QI
              MOIST(I,J,K,P_QG)=0.
            ENDDO
            ENDDO
            ENDDO
          ENDIF
!
!-----------------------------------------------------------------------
        ELSE had2_micro_check
!-----------------------------------------------------------------------
!
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
!        
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
!
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
!    
          DO K=KTS,KTE 
          DO J=max(jds+(0),jts-(0)),min(jde-(0),jte+(0))
          DO I=max(ids+(0),its-(0)),min(ide-(0),ite+(0))
            Q(I,J,K)=MOIST(I,J,K,P_QV)/(1.+MOIST(I,J,K,P_QV))           
          ENDDO
          ENDDO    
          ENDDO   
!
!-----------------------------------------------------------------------
        ENDIF had2_micro_check
!-----------------------------------------------------------------------
!
        had2_tim=had2_tim+timef()-btimx
      ENDIF
!
!-----------------------------------------------------------------------
!***  HORIZONTAL ADVECTION OF CHEMISTRY
!-----------------------------------------------------------------------
!
!
!----------------------------------------------------------------------
!***  RADIATION
!----------------------------------------------------------------------
!
!***  When allocating CAM radiation 4d arrays (ozmixm, aerosolc), 
!***  the following two scalars are not needed.
!
      NUM_OZMIXM=1
      NUM_AEROSOLC=1
!
      IF(NTSD<=0)THEN
        NTSD_rad=NTSD
      ELSE
!
!***  Call radiation just BEFORE the top of the hour
!***  so that updated fields are written to history files.
!
        NTSD_rad=NTSD+1
      ENDIF
!
      IF(MOD(NTSD_rad,GRID%NRADS)==0.OR.                               &
     &   MOD(NTSD_rad,GRID%NRADL)==0)THEN
!
        btimx=timef()
        IF(OPERATIONAL_PHYSICS)THEN
          CALL UPDATE_MOIST(MOIST,Q,CWM,F_ICE,F_RAIN,N_MOIST           &
     &                     ,IDS,IDF,JDS,JDF,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE)
        ENDIF
!
!	goto 6999

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
!
        DO J=JMS,JME
        DO I=IMS,IME
          GSW(I,J)=RSWIN(I,J)-RSWOUT(I,J)
        ENDDO
        ENDDO

  6999 	continue
!
!                            ***  NOTE  ***
! RLWIN/RSWIN  - downward longwave/shortwave at the surface (formerly TOTLWDN/TOTSWDN)
! RSWINC - CLEAR-SKY downward shortwave at the surface (new for AQ)
!                            ***  NOTE  ***
!
        radiation_tim=radiation_tim+timef()-btimx
      ENDIF
!
!----------------------------------------------------------------------
!***  APPLY TEMPERATURE TENDENCY DUE TO RADIATION
!----------------------------------------------------------------------
!
      btimx=timef()
!
      CALL RDTEMP(NTSD,GRID%DT,GRID%JULDAY,GRID%JULYR,IHRST,GLAT,GLON  &
     &           ,CZEN,CZMEAN,T,RSWTT,RLWTT,HBM2                       &
     &           ,IDS,IDF,JDS,JDF,KDS,KDE                              &
     &           ,IMS,IME,JMS,JME,KMS,KME                              &
     &           ,ITS,ITE,JTS,JTE,KTS,KTE)
!
      rdtemp_tim=rdtemp_tim+timef()-btimx
!
!----------------------------------------------------------------------
!***  TURBULENT PROCESSES 
!----------------------------------------------------------------------
!
      IF(MOD(NTSD,GRID%NPHS)==0)THEN
!
        btimx=timef()
!
        IF(OPERATIONAL_PHYSICS                                         &
     &    .AND.MOD(NTSD_rad,GRID%NRADS)/=0                             &
     &    .AND.MOD(NTSD_rad,GRID%NRADL)/=0)THEN
          CALL UPDATE_MOIST(MOIST,Q,CWM,F_ICE,F_RAIN,N_MOIST           &
     &                     ,IDS,IDF,JDS,JDF,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE)
        ENDIF
!
        CALL TURBL(NTSD,GRID%DT,GRID%NPHS,RESTRT                       &
     &            ,N_MOIST,GRID%NUM_SOIL_LAYERS,SLDPTH,DZSOIL          &
     &            ,DETA1,DETA2,AETA1,AETA2,ETA1,ETA2,PDTOP,PT          &
     &            ,SM,HBM2,VBM2,DX_NMM,DFRLG                           &
     &            ,CZEN,CZMEAN,SIGT4,RLWIN,RSWIN,RADOT                 &
     &            ,PD,RES,PINT,T,Q,CWM,F_ICE,F_RAIN,SR                 &
     &            ,Q2,U,V,THS,NMM_TSK,SST,PREC,SNO                     &
     &            ,FIS,Z0,Z0BASE,USTAR,PBLH,LPBL,EL_MYJ                &
     &            ,MOIST,RMOL                                          &
     &            ,EXCH_H,AKHS,AKMS,AKHS_OUT,AKMS_OUT                  &
     &            ,THZ0,QZ0,UZ0,VZ0,QSH,MAVAIL                         &
     &            ,STC,SMC,CMC,SMSTAV,SMSTOT,SSROFF,BGROFF             &
     &            ,IVGTYP,ISLTYP,VEGFRC,SHDMIN,SHDMAX,GRNFLX           &
     &            ,SFCEXC,ACSNOW,ACSNOM,SNOPCX,SICE,TG,SOILTB          &
     &            ,ALBASE,MXSNAL,ALBEDO,SH2O,SI,EPSR                   &
     &            ,U10,V10,TH10,Q10,TSHLTR,QSHLTR,PSHLTR               &
     &            ,T2,QSG,QVG,QCG,SOILT1,TSNAV,SMFR3D,KEEPFR3DFLAG     &
     &            ,TWBS,QWBS,SFCSHX,SFCLHX,SFCEVP                      &
     &            ,POTEVP,POTFLX,SUBSHX                                &
     &            ,APHTIM,ARDSW,ARDLW,ASRFC                            &
     &            ,RSWOUT,RSWTOA,RLWTOA                                &
     &            ,ASWIN,ASWOUT,ASWTOA,ALWIN,ALWOUT,ALWTOA             &
     &            ,UZ0H,VZ0H,DUDT,DVDT                                 & 
     &            ,RTHBLTEN,RQVBLTEN                                   & 
     &            ,GRID%PCPFLG,DDATA                                   &
     &            ,GRID,CONFIG_FLAGS                                   &
     &            ,IHE,IHW,IVE,IVW                                     &
     &            ,IDS,IDF,JDS,JDF,KDS,KDE                             &
     &            ,IMS,IME,JMS,JME,KMS,KME                             &
     &            ,ITS,ITE,JTS,JTE,KTS,KTE)
!
!                     ***  NOTE  ***
! RLWIN/RSWIN - downward longwave/shortwave at the surface
!                     ***  NOTE  ***
!
        turbl_tim=turbl_tim+timef()-btimx
!
        btimx=timef()
!-----------------
!# include "HALO_NMM_TURBL_A.inc"
        CALL HALO_EXCH(UZ0H,1,VZ0H,1,1,1                                &
     &                ,MYPE,MPI_COMM_COMP                               &
     &                ,IDS,IDE,JDS,JDE,KDS,KDE                          &
     &                ,IMS,IME,JMS,JME,KMS,KME                          &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)
!-----------------
!# include "HALO_NMM_TURBL_B.inc"
      CALL HALO_EXCH(DUDT,KTE,DVDT,KTE,1,1                              &
     &              ,MYPE,MPI_COMM_COMP                                 &
     &              ,IDS,IDE,JDS,JDE,KDS,KDE                            &
     &              ,IMS,IME,JMS,JME,KMS,KME                            &
     &              ,ITS,ITE,JTS,JTE,KTS,KTE)
!-----------------
        exch_tim=exch_tim+timef()-btimx
!       this_tim=timef()-btimx
!       call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max           &
!    &                    ,mpi_comm_comp,irtn)
!       exch_tim_max=exch_tim_max+et_max
!
!***  INTERPOLATE WINDS FROM H POINTS BACK TO V POINTS.
!
        btimx=timef()
        CALL UV_H_TO_V(NTSD,GRID%DT,GRID%NPHS,UZ0H,VZ0H,UZ0,VZ0         &
     &                ,DUDT,DVDT,U,V,HBM2,IVE,IVW                       &
     &                ,IDS,IDF,JDS,JDF,KDS,KDE                          &
     &                ,IMS,IME,JMS,JME,KMS,KME                          &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)
        uv_htov_tim=uv_htov_tim+timef()-btimx
!
!----------------------------------------------------------------------
!*** STORE ORIGINAL TEMPERATURE ARRAY
!----------------------------------------------------------------------
!
        btimx=timef()
!-----------------
!#    include "HALO_NMM_J.inc"
        CALL HALO_EXCH(PD,1,UZ0,1,VZ0,1,T,KTE,Q,KTE,CWM,KTE,1,1         &
     &                ,MYPE,MPI_COMM_COMP                               &
     &                ,IDS,IDE,JDS,JDE,KDS,KDE                          &
     &                ,IMS,IME,JMS,JME,KMS,KME                          &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)
!
        IF(CONFIG_FLAGS%MP_PHYSICS/=ETAMPNEW)THEN
!#    include "HALO_NMM_J_3.inc"
          CALL HALO_EXCH(MOIST,KTE,N_MOIST,1,1                          &
     &                  ,MYPE,MPI_COMM_COMP                             &
     &                  ,IDS,IDE,JDS,JDE,KDS,KDE                        &
     &                  ,IMS,IME,JMS,JME,KMS,KME                        &
     &                  ,ITS,ITE,JTS,JTE,KTS,KTE)
          CALL HALO_EXCH(SCALAR,KTE,NUM_SCALAR,1,1                      &
     &                  ,MYPE,MPI_COMM_COMP                             &
     &                  ,IDS,IDE,JDS,JDE,KDS,KDE                        &
     &                  ,IMS,IME,JMS,JME,KMS,KME                        &
     &                  ,ITS,ITE,JTS,JTE,KTS,KTE)

        ENDIF
!
!-----------------
        exch_tim=exch_tim+timef()-btimx
!       this_tim=timef()-btimx
!       call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max           &
!    &                    ,mpi_comm_comp,irtn)
!       exch_tim_max=exch_tim_max+et_max
!
        ICLTEND=-1
        btimx=timef()
! 
        CALL CLTEND(ICLTEND,GRID%NPHS,T,T_OLD,T_ADJ                    &
     &             ,IDS,IDF,JDS,JDF,KDS,KDE                            &
     &             ,IMS,IME,JMS,JME,KMS,KME                            &
     &             ,ITS,ITE,JTS,JTE,KTS,KTE)
!
        cltend_tim=cltend_tim+timef()-btimx
      ENDIF
!
!----------------------------------------------------------------------
!***  CONVECTIVE PRECIPITATION
!----------------------------------------------------------------------
!
      IF(MOD(NTSD,GRID%NCNVC)==0.AND.                                  &
     &   CONFIG_FLAGS%CU_PHYSICS==KFETASCHEME)THEN
!
        btimx=timef()
!-----------------
!#    include "HALO_NMM_C.inc"
        CALL HALO_EXCH(U,KTE,V,KTE,1,1                                 &
     &                ,MYPE,MPI_COMM_COMP                              &
     &                ,IDS,IDE,JDS,JDE,KDS,KDE                         &
     &                ,IMS,IME,JMS,JME,KMS,KME                         &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)
!-----------------
        exch_tim=exch_tim+timef()-btimx
!       this_tim=timef()-btimx
!       call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max          &
!    &                    ,mpi_comm_comp,irtn)
!       exch_tim_max=exch_tim_max+et_max
      ENDIF
!
!----------------------------------------------------------------------
!
      convection: IF(CONFIG_FLAGS%CU_PHYSICS/=0)THEN
        btimx=timef()
!
!***  GET TENDENCIES FOR GD SCHEME.
!    
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
!
!***  UPDATE THE MOIST ARRAY IF NEEDED.
!
        IF(OPERATIONAL_PHYSICS                                         &
     &    .AND.MOD(NTSD_rad,GRID%NRADS)/=0                             &
     &    .AND.MOD(NTSD_rad,GRID%NRADL)/=0                             &
     &    .AND.MOD(NTSD,GRID%NPHS)/=0)THEN
          CALL UPDATE_MOIST(MOIST,Q,CWM,F_ICE,F_RAIN,N_MOIST           &
     &                     ,IDS,IDF,JDS,JDF,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE)
        ENDIF
!
        CALL CUCNVC(NTSD,GRID%DT,GRID%NCNVC,GRID%NRADS,GRID%NRADL      &
     &             ,GPS,RESTRT,HYDRO,CLDEFI,N_MOIST,GRID%ENSDIM        &
     &             ,MOIST                                              &
     &             ,DETA1,DETA2,AETA1,AETA2,ETA1,ETA2                  &
     &             ,F_ICE,F_RAIN                                       &
!***  Changes for other cu schemes, most for GD scheme
     &             ,APR_GR,APR_W,APR_MC,TTEN,QTEN                      &
     &             ,APR_ST,APR_AS,APR_CAPMA                            &
     &             ,APR_CAPME,APR_CAPMI                                &
     &             ,MASS_FLUX,XF_ENS                                   &
     &             ,PR_ENS,GSW                                         &
!
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
     &             ,ITS,ITE,JTS,JTE,KTS,KTE)
!
        cucnvc_tim=cucnvc_tim+timef()-btimx
!
      ENDIF convection
!
!----------------------------------------------------------------------
!***  GRIDSCALE MICROPHYSICS (CONDENSATION & PRECIPITATION)
!----------------------------------------------------------------------
!
      IF(MOD(NTSD,GRID%NPHS)==0)THEN
        btimx=timef()
!
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
!
        gsmdrive_tim=gsmdrive_tim+timef()-btimx
!
!-----------------------------------------------------------------------
!---------PRECIPITATION ASSIMILATION------------------------------------
!-----------------------------------------------------------------------
!
        IF (GRID%PCPFLG) THEN
          btimx=timef()
!
          CALL CHKSNOW(NTSD,GRID%DT,GRID%NPHS,SR,PPTDAT                 &
     &      ,IDS,IDE,JDS,JDE,KDS,KDE                                    &
     &      ,IMS,IME,JMS,JME,KMS,KME                                    &
     &      ,ITS,ITE,JTS,JTE,KTS,KTE)
          CALL ADJPPT(NTSD,GRID%DT,GRID%NPHS,PREC,LSPA,PPTDAT,DDATA     &
     &      ,IDS,IDE,JDS,JDE,KDS,KDE                                    &
     &      ,IMS,IME,JMS,JME,KMS,KME                                    &
     &      ,ITS,ITE,JTS,JTE,KTS,KTE)
!
          adjppt_tim=adjppt_tim+timef()-btimx
        ENDIF
!
!----------------------------------------------------------------------
!***  CALCULATE TEMP TENDENCIES AND RESTORE ORIGINAL TEMPS
!----------------------------------------------------------------------
!      
        ICLTEND=0
        btimx=timef()
!
        CALL CLTEND(ICLTEND,GRID%NPHS,T,T_OLD,T_ADJ                    &
     &             ,IDS,IDF,JDS,JDF,KDS,KDE                            &
     &             ,IMS,IME,JMS,JME,KMS,KME                            &
     &             ,ITS,ITE,JTS,JTE,KTS,KTE)
!
        cltend_tim=cltend_tim+timef()-btimx
      ENDIF
!
!----------------------------------------------------------------------
!***  UPDATE TEMP TENDENCIES FROM CLOUD PROCESSES EVERY TIME STEP
!----------------------------------------------------------------------
!
      ICLTEND=1
      btimx=timef()
!
      CALL CLTEND(ICLTEND,GRID%NPHS,T,T_OLD,T_ADJ                      &
     &           ,IDS,IDF,JDS,JDF,KDS,KDE                              &
     &           ,IMS,IME,JMS,JME,KMS,KME                              &
     &           ,ITS,ITE,JTS,JTE,KTS,KTE)
!
      cltend_tim=cltend_tim+timef()-btimx
!
!----------------------------------------------------------------------
!***  LATERAL DIFFUSION
!----------------------------------------------------------------------
!
      btimx=timef()
!-----------------
!#    include "HALO_NMM_K.inc"
      CALL HALO_EXCH(Q2,KTE,1,1                                         &
     &              ,MYPE,MPI_COMM_COMP                                 &
     &              ,IDS,IDE,JDS,JDE,KDS,KDE                            &
     &              ,IMS,IME,JMS,JME,KMS,KME                            &
     &              ,ITS,ITE,JTS,JTE,KTS,KTE)
      CALL HALO_EXCH(T,KTE,U,KTE,V,KTE,Q,KTE,W,KTE+1,Z,KTE+1,2,2        &
     &              ,MYPE,MPI_COMM_COMP                                 &
     &              ,IDS,IDE,JDS,JDE,KDS,KDE                            &
     &              ,IMS,IME,JMS,JME,KMS,KME                            &
     &              ,ITS,ITE,JTS,JTE,KTS,KTE)
!-----------------
      exch_tim=exch_tim+timef()-btimx
!     this_tim=timef()-btimx
!     call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max             &
!    &                  ,mpi_comm_comp,irtn)
!     exch_tim_max=exch_tim_max+et_max
!
      btimx=timef()
!
!***  INFLATE HDAC AND HDACV.
!***  THIS IS EQUIVALENT TO INCREASING COAC FROM 0.75 to 1.6.
!

!	if (mod(ntsd,15) .eq. 0) then
!	write(6,*) allocated HDAC_BIG, HDACV_BIG: , ALLOCATED(HDAC_BIG), ALLOCATED(HDACV_BIG)
!	write(6,*) summary before HDAC_BIG, HDACV_BIG allocates
!	call summary()
!	endif
	

      IF(.NOT.ALLOCATED(HDAC_BIG ))ALLOCATE(HDAC_BIG(IMS:IME,JMS:JME))
      IF(.NOT.ALLOCATED(HDACV_BIG))ALLOCATE(HDACV_BIG(IMS:IME,JMS:JME))

!	if (mod(ntsd,15) .eq. 0) then
!	write(6,*) summary after HDAC_BIG, HDACV_BIG allocates
!	call summary()
!	endif
!

!	HDAC based on COAC=1.6 (already large value)

      DO J=JTS,MIN(JTE,JDE-1)
      DO I=ITS,MIN(ITE,IDE-1)
!        HDAC_BIG(I,J) =2.13333*HDAC(I,J)
!        HDACV_BIG(I,J)=2.13333*HDACV(I,J)
!        HDAC_BIG(I,J) =HDAC(I,J)/2.1333
!        HDACV_BIG(I,J)=HDACV(I,J)/2.1333
      ENDDO
      ENDDO
!
!      CALL HDIFF(NTSD,GRID%DT,FIS,DY_NMM,HDAC_BIG,HDACV_BIG            &
      CALL HDIFF(NTSD,GRID%DT,FIS,DY_NMM,HDAC,HDACV            &
     &          ,HBM2,DETA1,GRID%SIGMA                                 &
     &          ,T,Q,U,V,Q2,Z,W,SM,SICE                                &
     &          ,IHE,IHW,IVE,IVW                                       &
     &          ,IDS,IDF,JDS,JDF,KDS,KDE                               &
     &          ,IMS,IME,JMS,JME,KMS,KME                               &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE)


!mp
	DEALLOCATE(HDAC_BIG)
	DEALLOCATE(HDACV_BIG)
!mp
!
      IF(.NOT.OPERATIONAL_PHYSICS)THEN
        DO K=KTS,KTE
        DO J=max(jds+(0),jts-(0)),min(jde-(0),jte+(0))
        DO I=max(ids+(0),its-(0)),min(ide-(0),ite+(0))
!!!       MOIST(I,J,K,P_QV)=MAX(0.,Q(I,J,K)/(1.-Q(I,J,K)))
          MOIST(I,J,K,P_QV)=Q(I,J,K)/(1.-Q(I,J,K))
        ENDDO
        ENDDO
        ENDDO
      ENDIF
!
      hdiff_tim=hdiff_tim+timef()-btimx
!
!----------------------------------------------------------------------
!***  UPDATING BOUNDARY VALUES AT HEIGHT POINTS
!----------------------------------------------------------------------
!
      btimx=timef()
!-----------------
!#    include "HALO_NMM_L.inc"
      CALL HALO_EXCH(PD,1,T,KTE,Q,KTE,CWM,KTE,Q2,KTE,1,1                &
     &              ,MYPE,MPI_COMM_COMP                                 &
     &              ,IDS,IDE,JDS,JDE,KDS,KDE                            &
     &              ,IMS,IME,JMS,JME,KMS,KME                            &
     &              ,ITS,ITE,JTS,JTE,KTS,KTE)
!
!#    include "HALO_NMM_L_3.inc"
      CALL HALO_EXCH(MOIST,KTE,N_MOIST,1,1                              &
     &              ,MYPE,MPI_COMM_COMP                                 &
     &              ,IDS,IDE,JDS,JDE,KDS,KDE                            &
     &              ,IMS,IME,JMS,JME,KMS,KME                            &
     &              ,ITS,ITE,JTS,JTE,KTS,KTE)
      IF(CONFIG_FLAGS%MP_PHYSICS/=ETAMPNEW)THEN
        CALL HALO_EXCH(SCALAR,KTE,NUM_SCALAR,1,1                        &
     &                ,MYPE,MPI_COMM_COMP                               &
     &                ,IDS,IDE,JDS,JDE,KDS,KDE                          &
     &                ,IMS,IME,JMS,JME,KMS,KME                          &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)
      ENDIF
!
!-----------------
      exch_tim=exch_tim+timef()-btimx
!     this_tim=timef()-btimx
!     call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max             &
!    &                  ,mpi_comm_comp,irtn)
!     exch_tim_max=exch_tim_max+et_max
!
      btimx=timef()
!
      CALL BOCOH(GRID%ID,NTSD,GRID%DT,NEST,NUNIT_NBC,NBOCO,LAST_TIME,TSPH & 
     &          ,LB,ETA1,ETA2,PDTOP,PT,RES                             &
     &          ,PD_B,T_B,Q_B,U_B,V_B,Q2_B,CWM_B                       &
     &          ,PD_BT,T_BT,Q_BT,U_BT,V_BT,Q2_BT,CWM_BT                &
     &          ,PD,T,Q,Q2,CWM,PINT                                    &
     &          ,MOIST,N_MOIST,SCALAR,NUM_SCALAR                       &
     &          ,IJDS,IJDE,GRID%SPEC_BDY_WIDTH,Z                       &
     &          ,IHE,IHW,IVE,IVW                                       &
     &          ,IDS,IDF,JDS,JDF,KDS,KDE                               &
     &          ,IMS,IME,JMS,JME,KMS,KME                               &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE)
!
      bocoh_tim=bocoh_tim+timef()-btimx
!     if(mod(ntsd,n_print_time)==0)then
!       call twr(t,0,t,ntsd,mype,npes,mpi_comm_comp &
!    &          ,ids,ide,jds,jde,kds,kde                               &
!    &          ,ims,ime,jms,jme,kms,kme                               &
!    &          ,its,ite,jts,jte,kts,kte)
!     endif
!
!----------------------------------------------------------------------
!***  IS IT TIME FOR A CHECK POINT ON THE MODEL HISTORY FILE?
!----------------------------------------------------------------------
!
 2003 CONTINUE
!
!----------------------------------------------------------------------
!***  PRESSURE GRD, CORIOLIS, DIVERGENCE, AND HORIZ PART OF OMEGA-ALPHA
!----------------------------------------------------------------------
!
      btimx=timef()
!-----------------
!#    include "HALO_NMM_A.inc"
      CALL HALO_EXCH(PD,1,T,KTE,U,KTE,V,KTE,Q,KTE,CWM,KTE,DWDT,KTE      &
     &              ,DIV,KTE,PINT,KTE+1,2,2                             &
     &              ,MYPE,MPI_COMM_COMP                                 &
     &              ,IDS,IDE,JDS,JDE,KDS,KDE                            &
     &              ,IMS,IME,JMS,JME,KMS,KME                            &
     &              ,ITS,ITE,JTS,JTE,KTS,KTE)
!
      IF(CONFIG_FLAGS%MP_PHYSICS/=ETAMPNEW)THEN
!#    include "HALO_NMM_A_3.inc"
        CALL HALO_EXCH(MOIST,KTE,N_MOIST,2,2                            &
     &                ,MYPE,MPI_COMM_COMP                               &
     &                ,IDS,IDE,JDS,JDE,KDS,KDE                          &
     &                ,IMS,IME,JMS,JME,KMS,KME                          &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)
        CALL HALO_EXCH(SCALAR,KTE,NUM_SCALAR,2,2                        &
     &                ,MYPE,MPI_COMM_COMP                               &
     &                ,IDS,IDE,JDS,JDE,KDS,KDE                          &
     &                ,IMS,IME,JMS,JME,KMS,KME                          &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)
      ENDIF
!
!-----------------
      exch_tim=exch_tim+timef()-btimx
!     this_tim=timef()-btimx
!     call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max             &
!    &                  ,mpi_comm_comp,irtn)
!     exch_tim_max=exch_tim_max+et_max
!
      btimx=timef()
!
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
!
      pfdht_tim=pfdht_tim+timef()-btimx
!
!----------------------------------------------------------------------
!***  DIVERGENCE DAMPING
!----------------------------------------------------------------------
!
      btimx=timef()
!-----------------
!#    include "HALO_NMM_B.inc"
      CALL HALO_EXCH(DIV,KTE,2,2                                        &
     &              ,MYPE,MPI_COMM_COMP                                 &
     &              ,IDS,IDE,JDS,JDE,KDS,KDE                            &
     &              ,IMS,IME,JMS,JME,KMS,KME                            &
     &              ,ITS,ITE,JTS,JTE,KTS,KTE)
!-----------------
      exch_tim=exch_tim+timef()-btimx
!     this_tim=timef()-btimx
!     call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max            &
!    &                  ,mpi_comm_comp,irtn)
!     exch_tim_max=exch_tim_max+et_max
!
      btimx=timef()
!
      CALL DDAMP(NTSD,GRID%DT,DETA1,DETA2,PDSL,PDTOP,DIV,HBM2          &
     &          ,T,U,V,DDMPU,DDMPV                                     &
     &          ,IHE,IHW,IVE,IVW                                       &
     &          ,IDS,IDF,JDS,JDF,KDS,KDE                               &
     &          ,IMS,IME,JMS,JME,KMS,KME                               &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE)
!
      ddamp_tim=ddamp_tim+timef()-btimx
!
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!
      IF(FIRST.AND.NTSD==0)THEN
        FIRST=.FALSE.
        btimx=timef()
!-----------------
!#    include "HALO_NMM_A.inc"
        CALL HALO_EXCH(PD,1,T,KTE,U,KTE,V,KTE,Q,KTE,CWM,KTE,DWDT,KTE   &
     &                ,DIV,KTE,PINT,KTE+1,2,2                          &
     &                ,MYPE,MPI_COMM_COMP                              &
     &                ,IDS,IDE,JDS,JDE,KDS,KDE                         &
     &                ,IMS,IME,JMS,JME,KMS,KME                         &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)
!-----------------
        exch_tim=exch_tim+timef()-btimx
!       this_tim=timef()-btimx
!       call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max          &
!    &                    ,mpi_comm_comp,irtn)
!       exch_tim_max=exch_tim_max+et_max
!
        GO TO 2000
      ENDIF
!
!----------------------------------------------------------------------
!***  UPDATING BOUNDARY VALUES AT VELOCITY POINTS
!----------------------------------------------------------------------
!
      btimx=timef()
!-----------------
!#    include "HALO_NMM_C.inc"
      CALL HALO_EXCH(U,KTE,V,KTE,1,1                                    &
     &              ,MYPE,MPI_COMM_COMP                                 &
     &              ,IDS,IDE,JDS,JDE,KDS,KDE                            &
     &              ,IMS,IME,JMS,JME,KMS,KME                            &
     &              ,ITS,ITE,JTS,JTE,KTS,KTE)
!-----------------
      exch_tim=exch_tim+timef()-btimx
!     this_tim=timef()-btimx
!     call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max            &
!    &                  ,mpi_comm_comp,irtn)
!     exch_tim_max=exch_tim_max+et_max
!
      btimx=timef()
!
      CALL BOCOV(GRID%ID,NTSD,GRID%DT,LB,U_B,V_B,U_BT,V_BT             & 
     &          ,U,V                                                   &
     &          ,IJDS,IJDE,GRID%SPEC_BDY_WIDTH                         &
     &          ,IHE,IHW,IVE,IVW                                       &
     &          ,IDS,IDF,JDS,JDF,KDS,KDE                               &
     &          ,IMS,IME,JMS,JME,KMS,KME                               &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE )
!
      bocov_tim=bocov_tim+timef()-btimx
!
!----------------------------------------------------------------------
!***  COPY THE NMM VARIABLE Q2 TO THE WRF VARIABLE TKE_MYJ
!----------------------------------------------------------------------
!
      DO K=KTS,KTE
      DO J=JTS,JTE
      DO I=ITS,ITE
        TKE_MYJ(I,J,K)=0.5*Q2(I,J,K) !TKE is q squared over 2
      ENDDO
      ENDDO
      ENDDO
!
!----------------------------------------------------------------------
!
      IF(LAST_TIME.AND.ALLOCATED(PPTDAT))THEN
        DEALLOCATE(PPTDAT,STAT=ISTAT)
      ENDIF
!
!----------------------------------------------------------------------
!
      solve_tim=solve_tim+timef()-btim
!
!----------------------------------------------------------------------
!***  PRINT TIMING VARIABLES WHEN DESIRED.
!----------------------------------------------------------------------
!
      sum_tim=pdte_tim+adve_tim+vtoa_tim+vadz_tim+hadz_tim+eps_tim     &
     &       +vad2_tim+had2_tim+radiation_tim+rdtemp_tim+turbl_tim     &
     &       +cltend_tim+cucnvc_tim+gsmdrive_tim+hdiff_tim             &
     &       +bocoh_tim+pfdht_tim+ddamp_tim+bocov_tim+uv_htov_tim      &
     &       +exch_tim+adjppt_tim
!
      if(mod(ntsd,n_print_time)==0)then
        write(0,*)' ntsd=',ntsd,' solve_tim=',solve_tim*1.e-3          &
     &           ,' sum_tim=',sum_tim*1.e-3
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
        write(0,*)' adjppt_tim=',adjppt_tim*1.e-3,' pct=',adjppt_tim/sum_tim*100.
        write(0,*)' hdiff_tim=',hdiff_tim*1.e-3,' pct=',hdiff_tim/sum_tim*100.
        write(0,*)' bocoh_tim=',bocoh_tim*1.e-3,' pct=',bocoh_tim/sum_tim*100.
        write(0,*)' pfdht_tim=',pfdht_tim*1.e-3,' pct=',pfdht_tim/sum_tim*100.
        write(0,*)' ddamp_tim=',ddamp_tim*1.e-3,' pct=',ddamp_tim/sum_tim*100.
        write(0,*)' bocov_tim=',bocov_tim*1.e-3,' pct=',bocov_tim/sum_tim*100.
        write(0,*)' uv_h_to_v_tim=',uv_htov_tim*1.e-3,' pct=',uv_htov_tim/sum_tim*100.
        write(0,*)' exch_tim=',exch_tim*1.e-3,' pct=',exch_tim/sum_tim*100.
!        call time_stats(exch_tim,exchange,ntsd,mype,npes,mpi_comm_comp)
!        write(0,*) exch_tim_max=,exch_tim_max*1.e-3
!
        call field_stats(t,mype,mpi_comm_comp                          &
     &                  ,ids,ide,jds,jde,kds,kde                       &
     &                  ,ims,ime,jms,jme,kms,kme                       &
     &                  ,its,ite,jts,jte,kts,kte)
      endif

!!!
!	if (mod(ntsd,120) .eq. 0) then
!	CALL SUMMARY()
!	endif
!!!
!
!     if(last_time)then
      DEALLOCATE(TTEN,STAT=ISTAT)
      DEALLOCATE(QTEN,STAT=ISTAT)
      DEALLOCATE(RTHRATEN,STAT=ISTAT)
      DEALLOCATE(RTHBLTEN,STAT=ISTAT)
      DEALLOCATE(RQVBLTEN,STAT=ISTAT)
!
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/nmm_scalar_derefs.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
! BEGIN SCALAR DEREFS
 grid%nmm_ntsd = ntsd
 grid%nmm_nstart_hour = nstart_hour
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
 grid%nmm_nphs0 = nphs0
 grid%nmm_nprec = nprec
 grid%nmm_nclod = nclod
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
! END SCALAR DEREFS
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
!**********************************************************************
!----------------------------------------------------------------------
      SUBROUTINE TWR(ARRAY,KK,FIELD,NTSD,MYPE,NPES,MPI_COMM_COMP       &
     &              ,IDS,IDE,JDS,JDE,KDS,KDE                           &
     &              ,IMS,IME,JMS,JME,KMS,KME                           &
     &              ,ITS,ITE,JTS,JTE,KTS,KTE)
!----------------------------------------------------------------------
!**********************************************************************
      USE MODULE_EXT_INTERNAL
!
      IMPLICIT NONE
      INCLUDE "mpif.h"
!----------------------------------------------------------------------
!----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE                    &
     &                     ,KK,MPI_COMM_COMP,MYPE,NPES,NTSD
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME+KK),INTENT(IN) :: ARRAY
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
!
      IF(MYPE==0)THEN
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
      IF(MYPE==0)THEN
        DO J=JTS,JTE
        DO I=ITS,ITE
          TWRITE(I,J)=ARRAY(I,J,K)
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
          VALUES(N)=ARRAY(I,J,K)
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
      IF(MYPE==0)THEN
!
        DO J=JDS,JDE-1
          IENDX=IDE-1
          IF(MOD(J,2)==0)IENDX=IENDX-1
          WRITE(IUNIT)(TWRITE(I,J),I=1,IENDX)
        ENDDO
!
      ENDIF
!
!----------------------------------------------------------------------
  500 CONTINUE
!
      IF(MYPE==0)CLOSE(IUNIT)
!----------------------------------------------------------------------
!
      END SUBROUTINE TWR
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
      SUBROUTINE VWR(ARRAY,KK,FIELD,NTSD,MYPE,NPES,MPI_COMM_COMP       &
     &              ,IDS,IDE,JDS,JDE,KDS,KDE                           &
     &              ,IMS,IME,JMS,JME,KMS,KME                           &
     &              ,ITS,ITE,JTS,JTE,KTS,KTE)
!----------------------------------------------------------------------
!**********************************************************************
      USE MODULE_EXT_INTERNAL
!
      IMPLICIT NONE
      INCLUDE "mpif.h"
!----------------------------------------------------------------------
!----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE                    &
     &                     ,KK,MPI_COMM_COMP,MYPE,NPES,NTSD
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME+KK),INTENT(IN) :: ARRAY
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
     &          ,J,K,L,N,NLEN,NSIZE
      INTEGER :: ITS_REM,ITE_REM,JTS_REM,JTE_REM
!
      REAL,DIMENSION(IDS:IDE,JDS:JDE) :: TWRITE
      REAL,ALLOCATABLE,DIMENSION(:) :: VALUES
      CHARACTER(5) :: TIMESTEP
      CHARACTER(6) :: FMT
      CHARACTER(12) :: FILENAME
      LOGICAL :: OPENED
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
!
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
!
      IF(MYPE==0)THEN
        CALL INT_GET_FRESH_HANDLE(IUNIT)
        CLOSE(IUNIT)
        OPEN(UNIT=IUNIT,FILE=FILENAME,FORM='UNFORMATTED',IOSTAT=IER)
      ENDIF
!     IF(MYPE==0)THEN
!       OPEN_UNIT: DO L=51,99
!         INQUIRE(L,OPENED=OPENED)
!         IF(.NOT.OPENED)THEN
!           IUNIT=L
!           OPEN(UNIT=IUNIT,FILE=FILENAME,STATUS=NEW                 &
!               ,FORM=UNFORMATTED,IOSTAT=IER)
!           IF(IER/=0)THEN
!             WRITE(0,*) Opening file error=,IER
!             WRITE(6,*) Opening file error=,IER
!           ENDIF
!           EXIT OPEN_UNIT
!         ENDIF
!       ENDDO OPEN_UNIT
!     ENDIF
!
!----------------------------------------------------------------------
!!!!  DO 500 K=KTS,KTE+KK     !Unflipped
!!!!  DO 500 K=KTE+KK,KTS,-1
      DO 500 K=KDE-1,KDS,-1   !Write LM layers top down for checking
!----------------------------------------------------------------------
!
      IF(MYPE==0)THEN
        DO J=JTS,JTE
        DO I=ITS,ITE
          TWRITE(I,J)=ARRAY(I,J,K)
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
          VALUES(N)=ARRAY(I,J,K)
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
      IF(MYPE==0)THEN
!
        DO J=JDS,JDE-1
          IENDX=IDE-1
          IF(MOD(J,2)==1)IENDX=IENDX-1
          WRITE(IUNIT)(TWRITE(I,J),I=1,IENDX)
        ENDDO
!
      ENDIF
!
!----------------------------------------------------------------------
  500 CONTINUE
!
      IF(MYPE==0)CLOSE(IUNIT)
!----------------------------------------------------------------------
!
      END SUBROUTINE VWR
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
      SUBROUTINE EXIT(NAME,PINT,T,Q,U,V,Q2,W                           &
     &               ,NTSD,MYPE,MPI_COMM_COMP                          &
     &               ,IDS,IDE,JDS,JDE,KDS,KDE                          &
     &               ,IMS,IME,JMS,JME,KMS,KME                          &
     &               ,ITS,ITE,JTS,JTE,KTS,KTE)
!----------------------------------------------------------------------
!**********************************************************************
      USE MODULE_EXT_INTERNAL
!
!----------------------------------------------------------------------
      IMPLICIT NONE
!----------------------------------------------------------------------
      INCLUDE "mpif.h"
!----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE                    &
     &                     ,MYPE,MPI_COMM_COMP,NTSD      
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(IN) :: PINT,T,Q   &
                                                           ,U,V,Q2,W
      CHARACTER(*),INTENT(IN) :: NAME
!
      INTEGER :: I,J,K,IEND,IERR,IRET
      CHARACTER(256) :: ERRMESS
      LOGICAL :: E_BDY,S_BDY
!----------------------------------------------------------------------
      IRET=0
  100 FORMAT(' EXIT ',A,' AT NTSD=',I5)
      IEND=ITE
      S_BDY=(JTS==JDS)
      E_BDY=(ITE==IDE-1)
!
      DO K=KTS,KTE
      DO J=JTS,JTE
      IEND=ITE
      IF(E_BDY.AND.MOD(J,2)==0)IEND=ITE-1
!
      DO I=ITS,IEND
        IF(T(I,J,K)>330..OR.T(I,J,K)<180..OR.T(I,J,K)/=T(I,J,K))THEN
          WRITE(0,100)NAME,NTSD
          WRITE(0,200)I,J,K,T(I,J,K),MYPE,NTSD
  200     FORMAT(' BAD VALUE I=',I3,' J=',I3,' K=',I2,' T=',E12.5      &
     &,          ' MYPE=',I3,' NTSD=',I5)
          IRET=666
          return
!         WRITE(ERRMESS,205)NAME,T(I,J,K),I,J,K,MYPE
  205     FORMAT(' EXIT ',A,' TEMPERATURE=',E12.5                      &
     &,          ' AT (',I3,',',I2,',',I3,')',' MYPE=',I3)
!         CALL wrf_error_fatal3 ( "solve_nmm.b" , 2231 , ERRMESS)
!         CALL MPI_ABORT(MPI_COMM_WORLD,1,IERR)
        ELSEIF(Q(I,J,K)<-1.E-4.OR.Q(I,J,K)>30.E-3                      &
     &         .OR.Q(I,J,K)/=Q(I,J,K))THEN
          WRITE(0,100)NAME,NTSD
          WRITE(0,300)I,J,K,Q(I,J,K),MYPE,NTSD
  300     FORMAT(' BAD VALUE I=',I3,' J=',I3,' K=',I2,' Q=',E12.5      &
     &,          ' MYPE=',I3,' NTSD=',I5)
          IRET=666
          return
!         WRITE(ERRMESS,305)NAME,Q(I,J,K),I,J,K,MYPE
  305     FORMAT(' EXIT ',A,' SPEC HUMIDITY=',E12.5                    &
     &,          ' AT (',I3,',',I2,',',I3,')',' MYPE=',I3)
!         CALL wrf_error_fatal3 ( "solve_nmm.b" , 2244 , ERRMESS)
!         CALL MPI_ABORT(MPI_COMM_WORLD,1,IERR)
        ELSEIF(PINT(I,J,K)<0..OR.PINT(I,J,K)/=PINT(I,J,K))THEN
          WRITE(0,100)NAME,NTSD
          WRITE(0,315)I,J,K,PINT(I,J,K),MYPE,NTSD
  315     FORMAT(' BAD VALUE I=',I3,' J=',I3,' K=',I2,' PINT=',E12.5      &
     &,          ' MYPE=',I3,' NTSD=',I5)
          IRET=666
          return
!         CALL wrf_error_fatal3 ( "solve_nmm.b" , 2253 , ERRMESS)
!         CALL MPI_ABORT(MPI_COMM_WORLD,1,IERR)
        ELSEIF(W(I,J,K)/=W(I,J,K))THEN
          WRITE(0,100)NAME,NTSD
          WRITE(0,325)I,J,K,W(I,J,K),MYPE,NTSD
  325     FORMAT(' BAD VALUE I=',I3,' J=',I3,' K=',I2,' W=',E12.5      &
     &,          ' MYPE=',I3,' NTSD=',I5)
          IRET=666
          return
!         CALL wrf_error_fatal3 ( "solve_nmm.b" , 2262 , ERRMESS)
!         CALL MPI_ABORT(MPI_COMM_WORLD,1,IERR)
        ENDIF
      ENDDO
      ENDDO
      ENDDO
!
      DO K=KTS,KTE
      DO J=JTS,JTE
      IEND=ITE
      IF(E_BDY.AND.MOD(J,2)==1)IEND=ITE-1
      DO I=ITS,IEND
        IF(ABS(U(I,J,K))>125..OR.ABS(V(I,J,K))>125.                    &
     &         .OR.U(I,J,K)/=U(I,J,K).OR.V(I,J,K)/=V(I,J,K))THEN
          WRITE(0,100)NAME,NTSD
          WRITE(0,400)I,J,K,U(I,J,K),V(I,J,K),MYPE,NTSD
  400     FORMAT(' BAD VALUE I=',I3,' J=',I3,' K=',I2,' U=',E12.5      &
     &,          ' V=',E12.5,' MYPE=',I3,' NTSD=',I5)
          IRET=666
          return
!         WRITE(ERRMESS,405)NAME,U(I,J,K),V(I,J,K),I,J,K,MYPE
  405     FORMAT(' EXIT ',A,' U=',E12.5,' V=',E12.5                    &
     &,          ' AT (',I3,',',I2,',',I3,')',' MYPE=',I3)
!         CALL wrf_error_fatal3 ( "solve_nmm.b" , 2285 , ERRMESS)
!         CALL MPI_ABORT(MPI_COMM_WORLD,1,IERR)
        ENDIF
      ENDDO
      ENDDO
      ENDDO
!----------------------------------------------------------------------
      END SUBROUTINE EXIT
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
      SUBROUTINE TIME_STATS(TIME_LCL,NAME,NTSD,MYPE,NPES,MPI_COMM_COMP)
!----------------------------------------------------------------------
!**********************************************************************
      USE MODULE_EXT_INTERNAL
!
!----------------------------------------------------------------------
      IMPLICIT NONE
!----------------------------------------------------------------------
      INCLUDE "mpif.h"
!----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: MPI_COMM_COMP,MYPE,NPES,NTSD
      REAL,INTENT(IN) :: TIME_LCL
!
      CHARACTER(*),INTENT(IN) :: NAME
!
!*** LOCAL VARIABLES
!
      INTEGER,DIMENSION(MPI_STATUS_SIZE) :: JSTAT
      INTEGER,DIMENSION(MPI_STATUS_SIZE,4) :: STATUS_ARRAY
      INTEGER,ALLOCATABLE,DIMENSION(:) :: ID_PE,IPE_SORT
!
      INTEGER :: IPE,IPE_MAX,IPE_MEDIAN,IPE_MIN,IRECV,IRTN,ISEND       &
     &          ,N,N_MEDIAN,NLEN
!
      REAL,ALLOCATABLE,DIMENSION(:) :: TIME,SORT_TIME
      REAL,DIMENSION(2) :: REMOTE
      REAL :: TIME_MAX,TIME_MEAN,TIME_MEDIAN,TIME_MIN
!
      CHARACTER(5) :: TIMESTEP
      CHARACTER(6) :: FMT
      CHARACTER(25) :: TITLE
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
!
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
!
!----------------------------------------------------------------------
!
      IF(MYPE==0)THEN
        ALLOCATE(TIME(1:NPES))
        ALLOCATE(SORT_TIME(1:NPES))
        ALLOCATE(ID_PE(1:NPES))
        ALLOCATE(IPE_SORT(1:NPES))
!
        TIME(1)=TIME_LCL
        ID_PE(1)=MYPE
!
!***  COLLECT TIMES AND PE VALUES FROM OTHER PEs
!
        DO IPE=1,NPES-1
          CALL MPI_RECV(REMOTE,2,MPI_REAL,IPE,IPE                      &
     &                 ,MPI_COMM_COMP,JSTAT,IRECV)
!
          TIME(IPE+1)=REMOTE(1)
          ID_PE(IPE+1)=NINT(REMOTE(2))
        ENDDO
!
!***  NOW GET STATS.
!***  FIRST THE MAX, MIN, AND MEAN TIMES.
!
        TIME_MEAN=0.
        TIME_MAX=-1.
        TIME_MIN=1.E10
        IPE_MAX=-1
        IPE_MIN=-1
!
        DO N=1,NPES
          TIME_MEAN=TIME_MEAN+TIME(N)
!
          IF(TIME(N)>TIME_MAX)THEN
            TIME_MAX=TIME(N)
            IPE_MAX=ID_PE(N)
          ENDIF
!
          IF(TIME(N)<TIME_MIN)THEN
            TIME_MIN=TIME(N)
            IPE_MIN=ID_PE(N)
          ENDIF
!
        ENDDO
!
        TIME_MAX=TIME_MAX*1.E-3
        TIME_MIN=TIME_MIN*1.E-3
        TIME_MEAN=TIME_MEAN*1.E-3/REAL(NPES)
!
!***  THEN THE MEDIAN TIME.
!
        CALL SORT(TIME,NPES,SORT_TIME,IPE_SORT)
        N_MEDIAN=(NPES+1)/2
        TIME_MEDIAN=SORT_TIME(N_MEDIAN)*1.E-3
        IPE_MEDIAN=IPE_SORT(N_MEDIAN)
!
!----------------------------------------------------------------------
      ELSE
!
!***  SEND TIME AND PE VALUE TO PE0.
!
        REMOTE(1)=TIME_LCL
        REMOTE(2)=REAL(MYPE)
!
        CALL MPI_SEND(REMOTE,2,MPI_REAL,0,MYPE,MPI_COMM_COMP,ISEND)
!
      ENDIF
!----------------------------------------------------------------------
!
      CALL MPI_BARRIER(MPI_COMM_COMP,IRTN)
!
!***  WRITE RESULTS
!
      IF(MYPE==0)THEN
        WRITE(0,100)TITLE
        WRITE(0,105)TIME_MAX,IPE_MAX
        WRITE(0,110)TIME_MIN,IPE_MIN
        WRITE(0,115)TIME_MEDIAN,IPE_MEDIAN
        WRITE(0,120)TIME_MEAN
  100   FORMAT(' Time for ',A)
  105   FORMAT(' Maximum=',G11.5,' for PE ',I2.2)
  110   FORMAT(' Minimum=',G11.5,' for PE ',I2.2)
  115   FORMAT(' Median =',G11.5,' for PE ',I2.2)
  120   FORMAT(' Mean   =',G11.5)
      ENDIF
!----------------------------------------------------------------------
!
      END SUBROUTINE TIME_STATS
!
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
      SUBROUTINE SORT(DATA,NPES,DATA_SORTED,IPE_SORTED)
!----------------------------------------------------------------------
!***
!***  SORT DATA FROM MULTIPLE PEs.  SEND BACK THE SORTED DATA ITEMS
!***  ALONG WITH THE ASSOCIATED TASK IDs.
!***
!----------------------------------------------------------------------
      IMPLICIT NONE
!----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: NPES
      REAL,DIMENSION(NPES),INTENT(IN) :: DATA
!
      INTEGER,DIMENSION(NPES),INTENT(OUT) :: IPE_SORTED
      REAL,DIMENSION(NPES),INTENT(OUT) :: DATA_SORTED
!----------------------------------------------------------------------
      TYPE :: DATA_LINK
        REAL :: VALUE
        INTEGER :: IPE
        TYPE(DATA_LINK),POINTER :: NEXT_VALUE
      END TYPE
!----------------------------------------------------------------------
!
!***  LOCAL VARIABLES
!
!----------------------------------------------------------------------
      INTEGER :: ISTAT,N
!
      TYPE(DATA_LINK),POINTER :: HEAD,TAIL  ! Smallest, largest
      TYPE(DATA_LINK),POINTER :: PTR_NEW    ! Each new value
      TYPE(DATA_LINK),POINTER :: PTR1,PTR2  ! Working pointers
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
      pe_loop: DO N=1,NPES
        ALLOCATE(PTR_NEW,STAT=ISTAT)  ! Location for next data items
        PTR_NEW%VALUE=DATA(N)
        PTR_NEW%IPE=N-1
!
!----------------------------------------------------------------------
!***  DETERMINE WHERE IN LIST TO INSERT VALUE.
!***  FIRST THE INITIAL DATA VALUE.
!----------------------------------------------------------------------
!
!       main: IF(.NOT.ASSOCIATED(HEAD))THEN
        main: IF(N==1)THEN
          HEAD=>PTR_NEW
          TAIL=>HEAD
          NULLIFY(PTR_NEW%NEXT_VALUE)
!
!----------------------------------------------------------------------
!***  THE NEW VALUE IS LESS THAN THE SMALLEST VALUE ALREADY SORTED.
!----------------------------------------------------------------------
!
        ELSE
          check: IF(PTR_NEW%VALUE<HEAD%VALUE)THEN
            PTR_NEW%NEXT_VALUE=>HEAD
            HEAD=>PTR_NEW
!
!----------------------------------------------------------------------
!***  THE NEW VALUE IS GREATER THAN THE LARGEST VALUE ALREADY SORTED.
!----------------------------------------------------------------------
!
          ELSEIF(PTR_NEW%VALUE>=TAIL%VALUE)THEN
            TAIL%NEXT_VALUE=>PTR_NEW  ! This is what connects the former
                                      ! final value in the list to
                                      ! the new value being appended.
            TAIL=>PTR_NEW
            NULLIFY(TAIL%NEXT_VALUE)
!
!----------------------------------------------------------------------
!***  THE NEW VALUE IS IN BETWEEN VALUES ALREADY SORTED.
!----------------------------------------------------------------------
!
          ELSE
            PTR1=>HEAD
            PTR2=>PTR1%NEXT_VALUE
!
            search: DO
              IF((PTR_NEW%VALUE>=PTR1%VALUE).AND.                      &
     &           (PTR_NEW%VALUE<PTR2%VALUE))THEN
                PTR_NEW%NEXT_VALUE=>PTR2
                PTR1%NEXT_VALUE=>PTR_NEW
                EXIT search
              ENDIF
!
              PTR1=>PTR2
              PTR2=>PTR2%NEXT_VALUE
            ENDDO search
!
          ENDIF check
!
        ENDIF main
!
      ENDDO pe_loop
!
!----------------------------------------------------------------------
!***  COLLECT THE SORTED NUMBERS FROM THE LINKED LIST.
!----------------------------------------------------------------------
!
      PTR1=>HEAD
!
      DO N=1,NPES
!       IF(.NOT.ASSOCIATED(PTR_NEW))EXIT
        DATA_SORTED(N)=PTR1%VALUE
        IPE_SORTED(N)=PTR1%IPE
        PTR1=>PTR1%NEXT_VALUE
      ENDDO
!
      DEALLOCATE(PTR_NEW)
      NULLIFY (HEAD,TAIL,PTR1,PTR2)
!----------------------------------------------------------------------
      END SUBROUTINE SORT
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
      SUBROUTINE FIELD_STATS(FIELD,MYPE,MPI_COMM_COMP                  &
     &                      ,IDS,IDE,JDS,JDE,KDS,KDE                   &
     &                      ,IMS,IME,JMS,JME,KMS,KME                   &
     &                      ,ITS,ITE,JTS,JTE,KTS,KTE)
!----------------------------------------------------------------------
!***
!***  GENERATE STANDARD LAYER STATISTICS FOR THE DESIRED FIELD.
!***
!----------------------------------------------------------------------
      IMPLICIT NONE
!----------------------------------------------------------------------
      INCLUDE "mpif.h"
!----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: MPI_COMM_COMP,MYPE
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(IN) :: FIELD
!
!----------------------------------------------------------------------
!***  LOCAL
!----------------------------------------------------------------------
!
      INTEGER,PARAMETER :: DOUBLE=SELECTED_REAL_KIND(15,300)
!
      INTEGER :: I,IEND,IRTN,I_BY_J,J,K,KFLIP
!
      REAL :: FIKJ,FMAXK,FMINK
      REAL(KIND=DOUBLE) :: F_MEAN,POINTS,RMS,ST_DEV,SUMFK,SUMF2K
      REAL,DIMENSION(KTS:KTE) :: FMAX,FMAX_0,FMIN,FMIN_0
      REAL(KIND=DOUBLE),DIMENSION(KTS:KTE) :: SUMF,SUMF_0,SUMF2,SUMF2_0
!----------------------------------------------------------------------
!
      I_BY_J=(IDE-IDS)*(JDE-JDS)-(JDE-JDS-1)/2  ! This assumes that
                                                ! IDE and JDE are each
                                                ! one greater than the
                                                ! true grid size.
!
      layer_loop:  DO K=KTS,KTE
!
        FMAXK=-1.E10
        FMINK=1.E10
        SUMFK=0.
        SUMF2K=0.
!
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
!
        FMAX(K)=FMAXK
        FMIN(K)=FMINK
        SUMF(K)=SUMFK
        SUMF2(K)=SUMF2K
!
      ENDDO layer_loop
!
!----------------------------------------------------------------------
!***  GLOBAL STATS
!----------------------------------------------------------------------
!
      CALL MPI_REDUCE(SUMF,SUMF_0,KTE,MPI_REAL8,MPI_SUM,0              &
     &               ,MPI_COMM_COMP,IRTN)
      CALL MPI_REDUCE(SUMF2,SUMF2_0,KTE,MPI_REAL8,MPI_SUM,0            &
     &               ,MPI_COMM_COMP,IRTN)
      CALL MPI_REDUCE(FMAX,FMAX_0,KTE,MPI_REAL,MPI_MAX,0               &
     &               ,MPI_COMM_COMP,IRTN)
      CALL MPI_REDUCE(FMIN,FMIN_0,KTE,MPI_REAL,MPI_MIN,0               &
     &               ,MPI_COMM_COMP,IRTN)
!
      IF(MYPE==0)THEN
        POINTS=I_BY_J
        DO K=KTE,KTS,-1
          F_MEAN=SUMF_0(K)/POINTS
          ST_DEV=SQRT((POINTS*SUMF2_0(K)-SUMF_0(K)*SUMF_0(K))/         &
     &                (POINTS*(POINTS-1)))
          RMS=SQRT(SUMF2_0(K)/POINTS)
          KFLIP=KTE-K+1
          WRITE(0,101)KFLIP,FMAX_0(K),FMIN_0(K)
          WRITE(0,102)F_MEAN,ST_DEV,RMS
  101     FORMAT(' LAYER=',I2,' MAX=',E13.6,' MIN=',E13.6)
  102     FORMAT(9X,' MEAN=',E13.6,' STDEV=',E13.6,' RMS=',E13.6)
        ENDDO
      ENDIF
!----------------------------------------------------------------------
      END SUBROUTINE FIELD_STATS
!----------------------------------------------------------------------
      FUNCTION TIMEF()
      REAL*8 TIMEF
      INTEGER :: IC,IR
      CALL SYSTEM_CLOCK(COUNT=IC,COUNT_RATE=IR)
      TIMEF=REAL(IC)/REAL(IR)*1000.0
      END
