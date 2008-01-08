!STARTOFREGISTRYGENERATEDINCLUDE 'inc/nodyn_dummy_decl.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
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
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: lu_index
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: lu_mask
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: vegcat
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: soilcat
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: input_soil_cat
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: xice_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: snoalb
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,1:grid%num_soil_cat)           :: soilcbot_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,1:grid%num_soil_cat)           :: soilctop_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,1:grid%num_land_cat)           :: landusef_gc
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
#undef COPY_IN
#undef COPY_OUT
!ENDOFREGISTRYGENERATEDINCLUDE
