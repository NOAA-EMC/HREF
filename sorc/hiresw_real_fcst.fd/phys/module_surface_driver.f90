

MODULE module_surface_driver
CONTAINS

   SUBROUTINE surface_driver(                                         &
     &           acgrdflx,achfx,aclhf                                 &
     &          ,acsnom,acsnow,akhs,akms,albedo,br,canwat             &
     &          ,chklowq,dt,dx,dz8w,dzs,glw                           &
     &          ,grdflx,gsw,swdown,gz1oz0,hfx,ht,ifsnow,isfflx        &
     &          ,fractional_seaice                                    &
     &          ,isltyp,itimestep,julian_in,ivgtyp,lowlyr,mavail,rmol &
     &          ,num_soil_layers,p8w,pblh,pi_phy,pshltr,psih          &
     &          ,psim,p_phy,q10,q2,qfx,taux,tauy,qsfc,qshltr,qz0      &
     &          ,raincv,rho,sfcevp,sfcexc,sfcrunoff                   &
     &          ,smois,smstav,smstot,snoalb,snow,snowc,snowh,stepbl   &
     &          ,th10,th2,thz0,th_phy,tmn,tshltr,tsk,tslb             &
     &          ,tyr,tyra,tdly,tlag,lagday,nyear,nday,tmn_update,yr   &
     &          ,t_phy,u10,udrunoff,ust,uz0,u_frame,u_phy,v10,vegfra  &
     &          ,vz0,v_frame,v_phy,warm_rain,wspd,xice,xland,z,znt,zs &
     &          ,xicem,isice,iswater,ct,tke_myj,sfenth                &
     &          ,albbck,embck,lh,sh2o,shdmax,shdmin,z0                &
     &          ,flqc,flhc,psfc,sst,sstsk,dtw,sst_update,sst_skin,t2,emiss               &
     &          ,sf_sfclay_physics,sf_surface_physics,ra_lw_physics   &
     &          ,landusef,soilctop,soilcbot,ra,rs,nlcat,nscat,vegf_px & 
     &          ,snowncv, anal_interval, lai, pxlsm_smois_init        & 
     &          ,pxlsm_soil_nudge                                     & 
            
     &          ,declin_urb,cosz_urb2d,omg_urb2d,xlat_urb2d           & 
     &          ,num_roof_layers, num_wall_layers                     & 
     &          ,num_road_layers, dzr, dzb, dzg                       & 
     &          ,tr_urb2d,tb_urb2d,tg_urb2d,tc_urb2d,qc_urb2d         & 
     &          ,uc_urb2d                                             & 
     &          ,xxxr_urb2d,xxxb_urb2d,xxxg_urb2d,xxxc_urb2d          & 
     &          ,trl_urb3d,tbl_urb3d,tgl_urb3d                        & 
     &          ,sh_urb2d,lh_urb2d,g_urb2d,rn_urb2d,ts_urb2d          & 
     &          ,frc_urb2d, utype_urb2d                               & 
     &          , ids,ide,jds,jde,kds,kde                             &
     &          , ims,ime,jms,jme,kms,kme                             &
     &          , i_start,i_end,j_start,j_end,kts,kte,num_tiles       &
             
     &           ,qv_curr, qc_curr, qr_curr                           &
     &           ,qi_curr, qs_curr, qg_curr                           &
             
     &           ,f_qv,f_qc,f_qr                                      &
     &           ,f_qi,f_qs,f_qg                                      &
             
     &          ,capg,hol,mol                                         &
     &          ,rainncv,rainbl,regime,thc                            &
     &          ,qsg,qvg,qcg,soilt1,tsnav                             &
     &          ,smfr3d,keepfr3dflag                                  &
             
     &          ,potevp,snopcx,soiltb,sr                              &
             
     &          ,t2_ndg_old, q2_ndg_old, t2_ndg_new, q2_ndg_new       &
     &          ,sn_ndg_old, sn_ndg_new                               &
     &          ,t2obs, q2obs                                         &
             
     &          ,uratx,vratx,tratx                                    &
             
     &          ,omlcall,oml_hml0,oml_gamma                           &
     &          ,tml,t0ml,hml,h0ml,huml,hvml,f                        &
     &          ,ustm,ck,cka,cd,cda,isftcflx                          &
     &         ,isurban, mminlu                                       &
     &          ,snotime                                              &
     &           ,rdlai2d                                             &
     &          ,usemonalb                                            &
     &          ,noahres                                              &
             
     &          ,bldt,curr_secs,adapt_step_flag                       &
         
     &          ,sf_urban_physics,gmt,xlat,xlong,julday               &
     &          ,num_urban_layers                                     & 
     &          ,trb_urb4d,tw1_urb4d,tw2_urb4d,tgb_urb4d           & 
     &          ,sfw1_urb3d,sfw2_urb3d,sfr_urb3d,sfg_urb3d            & 
     &          ,a_u_bep,a_v_bep,a_t_bep,a_q_bep                      &
     &          ,b_u_bep,b_v_bep,b_t_bep,b_q_bep                      &
     &          ,sf_bep,vl_bep                                        &
     &          ,a_e_bep,b_e_bep,dlg_bep                              &
     &          ,dl_u_bep                                             &                          
         
     &                                                             )
              
   USE module_state_description, ONLY : SFCLAYSCHEME              &
                                       ,MYJSFCSCHEME              &
                                       ,QNSESFCSCHEME             &
                                       ,GFSSFCSCHEME              &
                                       ,PXSFCSCHEME               &
                                       ,SLABSCHEME                &
                                       ,LSMSCHEME                 &
                                       ,RUCLSMSCHEME              &
                                       ,PXLSMSCHEME               &
                                       ,GFDLSFCSCHEME             &
                                       ,GFDLSLAB 


   USE module_model_constants


   USE module_sf_sfclay
   USE module_sf_myjsfc
   USE module_sf_qnsesfc
   USE module_sf_gfs
   USE module_sf_noahdrv
   USE module_sf_ruclsm
   USE module_sf_pxsfclay
   USE module_sf_pxlsm

   USE module_sf_gfdl

   USE module_sf_slab

   USE module_sf_sfcdiags
   USE module_sf_sstskin
   USE module_sf_tmnupdate


   
   
   
   
   
   
   
   
   
   
   

   
   

   IMPLICIT NONE




















































































































































   INTEGER, INTENT(IN) ::                                             &
     &           ids,ide,jds,jde,kds,kde                              &
     &          ,ims,ime,jms,jme,kms,kme                              &
     &          ,kts,kte,num_tiles

   INTEGER, INTENT(IN)::   FRACTIONAL_SEAICE

   INTEGER, INTENT(IN)::   NLCAT
   INTEGER, INTENT(IN)::   NSCAT

   INTEGER, INTENT(IN) :: sf_sfclay_physics, sf_surface_physics,      &
                          sf_urban_physics,ra_lw_physics, sst_update
   INTEGER, INTENT(IN),OPTIONAL :: sst_skin, tmn_update

   INTEGER, DIMENSION(num_tiles), INTENT(IN) ::                       &
     &           i_start,i_end,j_start,j_end

   INTEGER, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT )::  ISLTYP
   INTEGER, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT)::   IVGTYP
   INTEGER, DIMENSION( ims:ime , jms:jme ), INTENT(IN )::   LOWLYR
   INTEGER, INTENT(IN )::   IFSNOW
   INTEGER, INTENT(IN )::   ISFFLX
   INTEGER, INTENT(IN )::   ITIMESTEP
   INTEGER, INTENT(IN )::   NUM_SOIL_LAYERS
   REAL,    INTENT(IN ),OPTIONAL ::   JULIAN_in
   INTEGER, INTENT(IN )::   LAGDAY
   INTEGER, INTENT(IN )::   STEPBL
   INTEGER, INTENT(IN )::   ISICE
   INTEGER, INTENT(IN )::   ISWATER
   INTEGER, INTENT(IN ), OPTIONAL :: ISURBAN
   CHARACTER(LEN=*), INTENT(IN ), OPTIONAL :: MMINLU
   LOGICAL, INTENT(IN )::   WARM_RAIN
   INTEGER, INTENT(INOUT ),OPTIONAL ::   NYEAR
   INTEGER, INTENT(INOUT ),OPTIONAL ::   NDAY
   INTEGER, INTENT(IN ),OPTIONAL ::   YR
   REAL , INTENT(IN )::   U_FRAME
   REAL , INTENT(IN )::   V_FRAME
   real , intent(IN )::   SFENTH
   REAL, DIMENSION( ims:ime , 1:num_soil_layers, jms:jme ), INTENT(INOUT)::   SMOIS
   REAL, DIMENSION( ims:ime , 1:num_soil_layers, jms:jme ), INTENT(INOUT)::   TSLB
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN )::   GLW
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN )::   GSW,SWDOWN
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN )::   HT
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN )::   RAINCV
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT)::   SST
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT ),OPTIONAL ::   SSTSK
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT ),OPTIONAL ::   DTW
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT)::   TMN
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT ),OPTIONAL ::   TYR
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT ),OPTIONAL ::   TYRA
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT ),OPTIONAL ::   TDLY
   REAL, DIMENSION( ims:ime , 1:lagday , jms:jme ), INTENT(INOUT ),OPTIONAL ::   TLAG
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT)::   VEGFRA
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN )::   XICE
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT)::   XLAND
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT)::   XICEM
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT)::   MAVAIL
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT)::   SNOALB
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   ACSNOW
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   SNOTIME
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   AKHS
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   AKMS
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   ALBEDO
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   CANWAT

   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   GRDFLX
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   HFX
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   RMOL
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   PBLH
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   Q2
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   QFX
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(OUT):: TAUX
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(OUT):: TAUY
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   QSFC
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   QZ0
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   SFCRUNOFF
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   SMSTAV
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   SMSTOT
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   SNOW
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   SNOWC
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   SNOWH
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   TH2
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   THZ0
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   TSK
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   UDRUNOFF
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   UST
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   UZ0
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   VZ0
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   WSPD
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   ZNT
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   BR
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   CHKLOWQ
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   GZ1OZ0
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   PSHLTR
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   PSIH
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   PSIM
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   Q10
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   QSHLTR
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   TH10
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   TSHLTR
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   U10
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   V10
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   PSFC
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)::   ACSNOM
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)::   SFCEVP
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT),OPTIONAL ::   ACHFX
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT),OPTIONAL ::   ACLHF
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT),OPTIONAL ::   ACGRDFLX
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)::   SFCEXC
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)::   FLHC
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)::   FLQC
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) ::   CT
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN )::   DZ8W
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN )::   P8W
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN )::   PI_PHY
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN )::   P_PHY
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN )::   RHO
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN )::   TH_PHY
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN )::   T_PHY
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN )::   U_PHY
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN )::   V_PHY
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN )::   Z

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::   TKE_MYJ
   REAL, DIMENSION(1:num_soil_layers), INTENT(IN)::   DZS
   REAL, DIMENSION(1:num_soil_layers), INTENT(IN)::   ZS
   REAL, INTENT(IN )::   DT
   REAL, INTENT(IN )::   DX
   REAL,       INTENT(IN   ),OPTIONAL    ::     bldt
   REAL,       INTENT(IN   ),OPTIONAL    ::     curr_secs
   LOGICAL,    INTENT(IN   ),OPTIONAL    ::     adapt_step_flag



   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT )::   ALBBCK  
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT )::   EMBCK
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT )::   LH
   REAL, DIMENSION( ims:ime , 1:num_soil_layers, jms:jme ), INTENT(INOUT)::   SH2O
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN )::   SHDMAX
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN )::   SHDMIN
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT )::   Z0


   REAL, OPTIONAL, INTENT(IN  )   ::                                   GMT 
   INTEGER, OPTIONAL, INTENT(IN  ) ::                               JULDAY
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN   )        ::XLAT, XLONG
   INTEGER, INTENT(IN )::   NUM_URBAN_LAYERS
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: trb_urb4d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tw1_urb4d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tw2_urb4d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tgb_urb4d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: sfw1_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: sfw2_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: sfr_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: sfg_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_u_bep   
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_v_bep   
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_t_bep   
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_e_bep   
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_q_bep   
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_u_bep   
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_v_bep   
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_t_bep   
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_e_bep   
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_q_bep   
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::vl_bep    
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::dlg_bep   
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::sf_bep  
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::dl_u_bep  




   REAL, DIMENSION( ims:ime , jms:jme ), OPTIONAL, INTENT(INOUT )::   TML, T0ML, HML, H0ML, HUML, HVML
   REAL, DIMENSION( ims:ime , jms:jme ), OPTIONAL, INTENT(IN    )::   F
   REAL, DIMENSION( ims:ime , jms:jme ), OPTIONAL, INTENT(OUT   )::   CK, CKA, CD, CDA, USTM



   INTEGER, OPTIONAL, INTENT(IN )::   ISFTCFLX
   INTEGER, OPTIONAL, INTENT(IN )::   OMLCALL
   REAL   , OPTIONAL, INTENT(IN )::   OML_HML0
   REAL   , OPTIONAL, INTENT(IN )::   OML_GAMMA



   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(OUT)::   uratx  
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(OUT)::   vratx  
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(OUT)::   tratx  



   INTEGER, OPTIONAL, INTENT(IN)    :: pxlsm_smois_init, pxlsm_soil_nudge, ANAL_INTERVAL
   REAL, DIMENSION( ims:ime, NLCAT, jms:jme ) , OPTIONAL, INTENT(INOUT)::   LANDUSEF
   REAL, DIMENSION( ims:ime, NSCAT, jms:jme ) , OPTIONAL, INTENT(INOUT)::   SOILCTOP, SOILCBOT
   REAL, DIMENSION( ims:ime , jms:jme ), OPTIONAL, INTENT(INOUT)::   VEGF_PX
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   RA
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   RS
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   LAI
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(OUT)::   T2OBS
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(OUT)::   Q2OBS

   REAL,       DIMENSION( ims:ime,  jms:jme ),                           &
               OPTIONAL, INTENT(INOUT)    ::      t2_ndg_old,            &
                                                  q2_ndg_old,            &
                                                  t2_ndg_new,            &
                                                  q2_ndg_new,            &
                                                  sn_ndg_old,            &
                                                  sn_ndg_new








   LOGICAL, INTENT(IN), OPTIONAL ::                             &
                                                      f_qv      &
                                                     ,f_qc      &
                                                     ,f_qr      &
                                                     ,f_qi      &
                                                     ,f_qs      &
                                                     ,f_qg

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         OPTIONAL, INTENT(INOUT) ::                              &
                      
                      
                      qv_curr, qc_curr, qr_curr                  &
                     ,qi_curr, qs_curr, qg_curr
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(IN)   ::   snowncv
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   capg
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   emiss
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   hol
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   mol
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   regime
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(IN )::     rainncv
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   RAINBL
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   t2
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(IN )::     thc
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   qsg
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   qvg
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   qcg
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   soilt1
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   tsnav
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   potevp 
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   snopcx 
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   soiltb 
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   sr 
   REAL, DIMENSION( ims:ime, 1:num_soil_layers, jms:jme ), OPTIONAL, INTENT(INOUT)::   smfr3d
   REAL, DIMENSION( ims:ime, 1:num_soil_layers, jms:jme ), OPTIONAL, INTENT(INOUT)::   keepfr3dflag

   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(OUT), OPTIONAL  ::   NOAHRES



   REAL,       DIMENSION( ims:ime, kms:kme, jms:jme ) ::v_phytmp
   REAL,       DIMENSION( ims:ime, kms:kme, jms:jme ) ::u_phytmp

   REAL,       DIMENSION( ims:ime, jms:jme )          ::  ZOL

   REAL,       DIMENSION( ims:ime, jms:jme )          ::          &
                                                             QGH, &
                                                             CHS, &
                                                             CPM, &
                                                            CHS2, &
                                                            CQS2

   REAL    :: DTMIN,DTBL

   INTEGER :: i,J,K,NK,jj,ij,n
   INTEGER :: gfdl_ntsflg
   LOGICAL :: radiation, myj, frpcpn
   LOGICAL, INTENT(in), OPTIONAL :: rdlai2d
   LOGICAL, INTENT(in), OPTIONAL :: usemonalb
   REAL    :: julian
   REAL    :: total_depth,mid_point_depth
   REAL    :: tconst,tprior,tnew,yrday,deltat



     REAL, OPTIONAL, INTENT(IN) :: DECLIN_URB                                 
     REAL, OPTIONAL , DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: COSZ_URB2D  
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: OMG_URB2D   
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: XLAT_URB2D  
     INTEGER,  INTENT(IN) :: num_roof_layers                         
     INTEGER,  INTENT(IN) :: num_wall_layers                         
     INTEGER,  INTENT(IN) :: num_road_layers                         
     REAL, OPTIONAL, DIMENSION(1:num_soil_layers), INTENT(IN) :: DZR          
     REAL, OPTIONAL, DIMENSION(1:num_soil_layers), INTENT(IN) :: DZB          
     REAL, OPTIONAL, DIMENSION(1:num_soil_layers), INTENT(IN) :: DZG          

     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)  :: TR_URB2D 
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)  :: TB_URB2D 
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)  :: TG_URB2D 
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT):: TC_URB2D 
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT):: QC_URB2D 
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT):: UC_URB2D 
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT):: XXXR_URB2D 
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT):: XXXB_URB2D 
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT):: XXXG_URB2D 
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT):: XXXC_URB2D 
     REAL, OPTIONAL, DIMENSION( ims:ime , 1:num_soil_layers, jms:jme ), &       
           INTENT(INOUT)  :: TRL_URB3D                                 
     REAL, OPTIONAL, DIMENSION( ims:ime , 1:num_soil_layers, jms:jme ), &       
           INTENT(INOUT)  :: TBL_URB3D                                 
     REAL, OPTIONAL, DIMENSION( ims:ime , 1:num_soil_layers, jms:jme ), &       
           INTENT(INOUT)  :: TGL_URB3D                                 
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)  :: SH_URB2D 
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT):: LH_URB2D 
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT):: G_URB2D  
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT):: RN_URB2D 
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT):: TS_URB2D 

     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)  :: FRC_URB2D  
     INTEGER, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)  :: UTYPE_URB2D  

     REAL,  DIMENSION( ims:ime, jms:jme )  :: PSIM_URB2D  
     REAL,  DIMENSION( ims:ime, jms:jme )  :: PSIH_URB2D  
     REAL,  DIMENSION( ims:ime, jms:jme )  :: GZ1OZ0_URB2D  

     REAL,  DIMENSION( ims:ime, jms:jme )  :: AKMS_URB2D  
     REAL,  DIMENSION( ims:ime, jms:jme )  :: U10_URB2D   
     REAL,  DIMENSION( ims:ime, jms:jme )  :: V10_URB2D   
     REAL,  DIMENSION( ims:ime, jms:jme )  :: TH2_URB2D   
     REAL,  DIMENSION( ims:ime, jms:jme )  :: Q2_URB2D    
     REAL,  DIMENSION( ims:ime, jms:jme )  :: UST_URB2D  


     REAL, DIMENSION( ims:ime, jms:jme ) :: HFX_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: QFX_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: LH_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: QSFC_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: TSK_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: ZNT_SEA

     REAL, DIMENSION( ims:ime, jms:jme ) :: CHS_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: CHS2_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: CQS2_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: CPM_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: FLHC_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: FLQC_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: QGH_SEA

     REAL, DIMENSION( ims:ime, jms:jme ) :: PSIH_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: PBLH_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: RMOL_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: UST_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: QZ0_SEA

   INTEGER :: isisfc
   REAL :: xice_threshold




   CHARACTER*256 :: message
   REAL    :: next_bl_time
   LOGICAL :: run_param
   LOGICAL :: do_adapt






  if (sf_sfclay_physics .eq. 0) return


  isisfc = 0
  if ( fractional_seaice == 0 ) then
     xice_threshold = 0.5
  else if ( fractional_seaice == 1 ) then
     xice_threshold = 0.02
  endif


  v_phytmp = 0.
  u_phytmp = 0.
  ZOL = 0.
  QGH = 0.
  CHS = 0.
  CPM = 0.
  CHS2 = 0.
  DTMIN = 0.
  DTBL = 0.



  IF ( PRESENT( rainncv ) .AND. PRESENT( rainbl ) ) THEN
    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij, i, j, k )
    DO ij = 1 , num_tiles
      DO j=j_start(ij),j_end(ij)
      DO i=i_start(ij),i_end(ij)
         RAINBL(i,j) = RAINBL(i,j) + RAINCV(i,j) + RAINNCV(i,j)
         RAINBL(i,j) = MAX (RAINBL(i,j), 0.0)
      ENDDO
      ENDDO
    ENDDO
    !$OMP END PARALLEL DO
  ELSE IF ( PRESENT( rainbl ) ) THEN
    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij, i, j, k )
    DO ij = 1 , num_tiles
      DO j=j_start(ij),j_end(ij)
      DO i=i_start(ij),i_end(ij)
         RAINBL(i,j) = RAINBL(i,j) + RAINCV(i,j)
         RAINBL(i,j) = MAX (RAINBL(i,j), 0.0)
      ENDDO
      ENDDO
    ENDDO
    !$OMP END PARALLEL DO
  ENDIF

  IF (sst_update .EQ. 1) THEN
    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij, i, j, k )
    DO ij = 1 , num_tiles
      DO j=j_start(ij),j_end(ij)
      DO i=i_start(ij),i_end(ij)
        IF ( XLAND(i,j) .GT. 1.5 .AND. XICE(I,J) .GE. XICE_THRESHOLD .AND. XICEM(I,J) .LT. XICE_THRESHOLD ) THEN

          XICEM(I,J) = XICE(I,J)
          XLAND(I,J) = 1.
          IVGTYP(I,J) = ISICE
          ISLTYP(I,J) = 16
          VEGFRA(I,J) = 0.
          TMN(I,J) = 271.4
          DO nk = 1, num_soil_layers
            TSLB(I,NK,J) = TSK(I,J)
            SMOIS(I,NK,J) = 1.0
            SH2O(I,NK,J) = 0.0
          ENDDO
        ENDIF
        IF(XLAND(i,j) .GT. 1.5) THEN
          TSK(i,j)   =SST(i,j)
          TSLB(i,1,j)=SST(i,j)
        ENDIF
        IF ( XLAND(i,j) .LT. 1.5 .AND. XICEM(I,J) .GE. XICE_THRESHOLD .AND. XICE(I,J) .LT. XICE_THRESHOLD ) THEN

          XICEM(I,J) = XICE(I,J)
          XLAND(I,J) = 2.
          IVGTYP(I,J) = ISWATER
          ISLTYP(I,J) = 14
          VEGFRA(I,J) = 0.
          TMN(I,J) = SST(I,J)
          DO nk = 1, num_soil_layers
            TSLB(I,NK,J) = SST(I,J)
            SMOIS(I,NK,J) = 1.0
            SH2O(I,NK,J) = 1.0
          ENDDO
        ENDIF
      ENDDO
      ENDDO
    IF(PRESENT(SST_SKIN))THEN
    IF (sst_skin .EQ. 1) THEN
 
        CALL wrf_debug( 100, 'in SST_UPDATE' )
        CALL sst_skin_update(xland,glw,gsw,hfx,qfx,tsk,ust,         &
                emiss,dtw,sstsk,dt,stbolt,                          &
                ids, ide, jds, jde, kds, kde,                       &
                ims, ime, jms, jme, kms, kme,                       &
                i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte   )
        DO j=j_start(ij),j_end(ij)
          DO i=i_start(ij),i_end(ij)
            IF(XLAND(i,j) .GT. 1.5)TSK(i,j)=SSTSK(i,j)
          ENDDO
        ENDDO
    ENDIF
    ENDIF
    ENDDO
    !$OMP END PARALLEL DO
  ENDIF
  IF(PRESENT(TMN_UPDATE))THEN
  IF (tmn_update .EQ. 1) THEN
      CALL tmnupdate(tsk,tmn,tlag,tyr,tyra,tdly,nday,nyear,lagday, &
                julian_in, dt, yr,                                  &
                ids, ide, jds, jde, kds, kde,                       &
                ims, ime, jms, jme, kms, kme,                       &
                i_start,i_end, j_start,j_end, kts,kte, num_tiles   )

  ENDIF
  ENDIF






  IF ( (itimestep .EQ. 1) .OR. (MOD(itimestep,STEPBL) .EQ. 0) ) THEN
    run_param = .TRUE.
  ELSE
    run_param = .FALSE.
  ENDIF
  IF (PRESENT(adapt_step_flag)) THEN
    IF ((adapt_step_flag)) THEN
      IF ( (itimestep .EQ. 1) .OR. (bldt .EQ. 0) .OR. &
           ( CURR_SECS + dt >= ( INT( CURR_SECS / ( bldt * 60 ) + 1 ) * bldt * 60) ) ) THEN
        run_param = .TRUE.
      ELSE
        run_param = .FALSE.
      ENDIF
    ENDIF
  ENDIF

  IF ( run_param ) then



  radiation = .false.
  myj = .false.
  frpcpn = .false.

  IF (ra_lw_physics .gt. 0) radiation = .true.




     DTMIN=DT/60.



    if (PRESENT(adapt_step_flag)) then
       if (adapt_step_flag) then
          do_adapt = .TRUE.
       else
          do_adapt = .FALSE.
       endif
    else
       do_adapt = .FALSE.
    endif

    if (PRESENT(BLDT)) then
       if (bldt .eq. 0) then
          DTBL = dt
       ELSE
          if (do_adapt) then
             call wrf_message("WARNING: When using an adaptive time-step the boundary layer"// &
                              " time-step should be 0 (i.e., equivalent to model time-step).  "// &
                              "In order to proceed, for boundary layer calculations, the "// &
                              "boundary layer time-step"// &
                              " will be rounded to the nearest minute, possibly resulting in"// &
                              " innacurate results.")
             DTBL=bldt*60
          else
             DTBL=DT*STEPBL
          endif
       endif
    else
       DTBL=DT*STEPBL
    endif




     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij, i, j, k )
     DO ij = 1 , num_tiles
       DO j=j_start(ij),j_end(ij)
       DO i=i_start(ij),i_end(ij)

          PSFC(I,J)=p8w(I,kts,J)

          DO k=kts,kte
            v_phytmp(i,k,j)=v_phy(i,k,j)+v_frame
            u_phytmp(i,k,j)=u_phy(i,k,j)+u_frame
          ENDDO
       ENDDO
       ENDDO
     ENDDO
     !$OMP END PARALLEL DO

     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij, i, j, k )
     DO ij = 1 , num_tiles
     sfclay_select: SELECT CASE(sf_sfclay_physics)

     CASE (SFCLAYSCHEME)



       IF (PRESENT(qv_curr)                            .AND.    &
           PRESENT(mol)        .AND.  PRESENT(regime)  .AND.    &
                                                      .TRUE. ) THEN
         CALL wrf_debug( 100, 'in SFCLAY' )
         IF ( FRACTIONAL_SEAICE == 1 ) THEN
            isisfc = 1
            CALL SFCLAY_SEAICE_WRAPPER(u_phytmp,v_phytmp,t_phy,qv_curr,&
                 p_phy,dz8w,cp,g,rcp,r_d,xlv,psfc,chs,chs2,cqs2,cpm, &
                 znt,ust,pblh,mavail,zol,mol,regime,psim,psih,       &
                 xland,hfx,qfx,lh,tsk,flhc,flqc,qgh,qsfc,rmol,       &
                 u10,v10,th2,t2,q2,                                  &
                 gz1oz0,wspd,br,isfflx,dx,                           &
                 svp1,svp2,svp3,svpt0,ep_1,ep_2,karman,eomeg,stbolt, &
                 P1000mb,                                            &
                 XICE,SST,TSK_SEA,                                                  &
                 CHS2_SEA,CHS_SEA,CPM_SEA,CQS2_SEA,FLHC_SEA,FLQC_SEA,               &
                 HFX_SEA,LH_SEA,QFX_SEA,QGH_SEA,QSFC_SEA,ZNT_SEA,                   &
                 ITIMESTEP,XICE_THRESHOLD,                                          &
                 ids,ide, jds,jde, kds,kde,                          &
                 ims,ime, jms,jme, kms,kme,                          &
                 i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte,    &
                 ustm,ck,cka,cd,cda,isftcflx                         )
         ELSE
         CALL SFCLAY(u_phytmp,v_phytmp,t_phy,qv_curr,&
               p_phy,dz8w,cp,g,rcp,r_d,xlv,psfc,chs,chs2,cqs2,cpm, &
               znt,ust,pblh,mavail,zol,mol,regime,psim,psih,       &
               xland,hfx,qfx,lh,tsk,flhc,flqc,qgh,qsfc,rmol,       &
               u10,v10,th2,t2,q2,                                  &
               gz1oz0,wspd,br,isfflx,dx,                           &
               svp1,svp2,svp3,svpt0,ep_1,ep_2,karman,eomeg,stbolt, &
               P1000mb,                                            &
               ids,ide, jds,jde, kds,kde,                          &
               ims,ime, jms,jme, kms,kme,                          &
               i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte,    &
               ustm,ck,cka,cd,cda,isftcflx                         )

         ENDIF
       ELSE
         CALL wrf_error_fatal3("",903,&
'Lacking arguments for SFCLAY in surface driver')
       ENDIF


     CASE (PXSFCSCHEME)
       CALL wrf_error_fatal3("",909,&
'PX Surface Layer scheme cannot be used with NMM')

      CASE (MYJSFCSCHEME)
       IF (PRESENT(qv_curr)    .AND.  PRESENT(qc_curr) .AND.    &
                                                      .TRUE. ) THEN
        myj =.true.

        CALL wrf_debug(100,'in MYJSFC')
        IF ( FRACTIONAL_SEAICE == 1 ) THEN
           isisfc = 1
           CALL MYJSFC_SEAICE_WRAPPER(itimestep,ht,dz8w,             &
                p_phy,p8w,th_phy,t_phy,                              &
                qv_curr,qc_curr,                                     &
                u_phy,v_phy,tke_myj,                                 &
                tsk,qsfc,thz0,qz0,uz0,vz0,                           &
                lowlyr,                                              &
                xland,                                               &
                XICE_THRESHOLD,                                      & 
                XICE, SST,                                           & 
                CHS_SEA, CHS2_SEA, CQS2_SEA, CPM_SEA,            &
                FLHC_SEA, FLQC_SEA, QSFC_SEA, &
                QGH_SEA, QZ0_SEA, HFX_SEA, QFX_SEA, LH_SEA,         &
                TSK_SEA,                                             &
                ust,znt,z0,pblh,mavail,rmol,                         &
                akhs,akms,                                           &
                br,                                                 &
                chs,chs2,cqs2,hfx,qfx,lh,flhc,flqc,qgh,cpm,ct,       &
                u10,v10,t2,th2,tshltr,th10,q2,qshltr,q10,pshltr,               &
                p1000mb,                                             &
                ids,ide, jds,jde, kds,kde,                           &
                ims,ime, jms,jme, kms,kme,                           &
                i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte    )
        ELSE
            CALL MYJSFC(itimestep,ht,dz8w,                         &
              p_phy,p8w,th_phy,t_phy,                              &
              qv_curr,qc_curr,                                      &
              u_phy,v_phy,tke_myj,                                 &
              tsk,qsfc,thz0,qz0,uz0,vz0,                           &
              lowlyr,                                              &
              xland,                                               &
              ust,znt,z0,pblh,mavail,rmol,                         &
              akhs,akms,                                           &
              br,                                                 &
              chs,chs2,cqs2,hfx,qfx,lh,flhc,flqc,qgh,cpm,ct,       &
              u10,v10,t2,th2,tshltr,th10,q2,qshltr,q10,pshltr,               &
              p1000mb,                                             &
              ids,ide, jds,jde, kds,kde,                           &
              ims,ime, jms,jme, kms,kme,                           &
              i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte    )

        ENDIF
       ELSE
         CALL wrf_error_fatal3("",962,&
'Lacking arguments for MYJSFC in surface driver')
       ENDIF

      CASE (QNSESFCSCHEME)
       IF (PRESENT(qv_curr)    .AND.  PRESENT(qc_curr) .AND.    &
                                                      .TRUE. ) THEN
            CALL wrf_debug(100,'in QNSESFC')
            CALL QNSESFC(itimestep,ht,dz8w,                         &
              p_phy,p8w,th_phy,t_phy,                              &
              qv_curr,qc_curr,                                     &
              u_phy,v_phy,tke_myj,                                 &
              tsk,qsfc,thz0,qz0,uz0,vz0,                           &
              lowlyr,                                              &
              xland,                                               &
              ust,znt,z0,pblh,mavail,rmol,                         &
              akhs,akms,                                           &
              br,                                                 &
              chs,chs2,cqs2,hfx,qfx,lh,flhc,flqc,qgh,cpm,ct,       &
              u10,v10,tshltr,th10,qshltr,q10,pshltr,               &
              ids,ide, jds,jde, kds,kde,                           &
              ims,ime, jms,jme, kms,kme,                           &
              i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte    )
       ELSE
         CALL wrf_error_fatal3("",986,&
'Lacking arguments for QNSESFC in surface driver')
       ENDIF

     CASE (GFSSFCSCHEME)
       IF (PRESENT(qv_curr) .AND. .TRUE. ) THEN
       CALL wrf_debug( 100, 'in GFSSFC' )
       IF (FRACTIONAL_SEAICE == 1) THEN
          isisfc = 1
          CALL SF_GFS_SEAICE_WRAPPER(u_phytmp,v_phytmp,t_phy,qv_curr, &
               p_phy,CP,RCP,R_d,XLV,PSFC,CHS,CHS2,CQS2,CPM,        &
               ZNT,UST,PSIM,PSIH,                                  &
               XLAND,HFX,QFX,LH,TSK,FLHC,FLQC,                     &
               QGH,QSFC,U10,V10,                                   &
               GZ1OZ0,WSPD,BR,ISFFLX,                              &
               EP_1,EP_2,KARMAN,itimestep,                         &
               XICE_THRESHOLD,                              &
               CHS_SEA, CHS2_SEA, CPM_SEA, CQS2_SEA,        &
               FLHC_SEA, FLQC_SEA,                          &
               HFX_SEA, LH_SEA, QFX_SEA, QGH_SEA, QSFC_SEA, &
               UST_SEA, ZNT_SEA, SST, XICE,                 &
               ids,ide, jds,jde, kds,kde,                          &
               ims,ime, jms,jme, kms,kme,                          &
               i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte    )
      ELSE
         CALL SF_GFS(u_phytmp,v_phytmp,t_phy,qv_curr,              &
               p_phy,CP,RCP,R_d,XLV,PSFC,CHS,CHS2,CQS2,CPM,        &
               ZNT,UST,PSIM,PSIH,                                  &
               XLAND,HFX,QFX,LH,TSK,FLHC,FLQC,                     &
               QGH,QSFC,U10,V10,                                   &
               GZ1OZ0,WSPD,BR,ISFFLX,                              &
               EP_1,EP_2,KARMAN,itimestep,                         &
               ids,ide, jds,jde, kds,kde,                          &
               ims,ime, jms,jme, kms,kme,                          &
               i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte    )
      ENDIF
        CALL wrf_debug(100,'in SFCDIAGS')
       ELSE
         CALL wrf_error_fatal3("",1024,&
'Lacking arguments for SF_GFS in surface driver')
      ENDIF



    CASE (GFDLSFCSCHEME)
       CALL wrf_debug( 100, 'in GFDLSFC' )

      IF(sf_surface_physics .eq. 88)THEN
        GFDL_NTSFLG=1
      ELSE
        GFDL_NTSFLG=0
      ENDIF

      CALL SF_GFDL(u_phytmp,v_phytmp,t_phy,qv_curr,p_phy, &
                   CP,RCP,R_d,XLV,PSFC,CHS,CHS2,CQS2,CPM,                 &
                   DTBL, SMOIS,num_soil_layers,ISLTYP,ZNT,UST,PSIM,PSIH,                          &  
                   XLAND,HFX,QFX,TAUX,TAUY,LH,GSW,GLW,TSK,FLHC,FLQC,  & 
                   QGH,QSFC,U10,V10,                              &
                   GZ1OZ0,WSPD,BR,ISFFLX,                         &
                   EP_1,EP_2,KARMAN,GFDL_NTSFLG,SFENTH,           &
                   ids,ide, jds,jde, kds,kde,                     &
                   ims,ime, jms,jme, kms,kme,                             &
                   i_start(ij),i_end(ij),j_start(ij),j_end(ij),kts,kte    )
           DO j=j_start(ij),j_end(ij)
           DO i=i_start(ij),i_end(ij)
              CHKLOWQ(I,J)= 1.0
           ENDDO
           ENDDO

     CASE DEFAULT

       WRITE( message , * )                                &
   'The sfclay option does not exist: sf_sfclay_physics = ', sf_sfclay_physics
       CALL wrf_error_fatal3("",1059,&
message )

     END SELECT sfclay_select


     IF(PRESENT(uratx) .and. PRESENT(vratx) .and. PRESENT(tratx))THEN
        DO J=j_start(ij),j_end(ij)
        DO I=i_start(ij),i_end(ij)
           IF(ABS(U10(I,J)) .GT. 1.E-10) THEN
              uratx(I,J) = U_PHYTMP(I,1,J)/U10(I,J)
           ELSE
              uratx(I,J) = 1.2
           END IF
           IF(ABS(V10(I,J)) .GT. 1.E-10) THEN
              vratx(I,J) = V_PHYTMP(I,1,J)/V10(I,J)
           ELSE
              vratx(I,J) = 1.2
           END IF

           tratx(I,J) = (T_PHY(I,1,J)*(P1000mb*0.001/(P_PHY(I,1,J)/1000.))**RCP)  &
                        /TH2(I,J)
        ENDDO
        ENDDO
     ENDIF

     ENDDO
     !$OMP END PARALLEL DO

     IF (ISFFLX.EQ.0 ) GOTO 430
     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij, i, j, k )
     DO ij = 1 , num_tiles

     sfc_select: SELECT CASE(sf_surface_physics)

     CASE (SLABSCHEME)

       IF (PRESENT(qv_curr)                            .AND.    &
           PRESENT(capg)        .AND.    &
                                                      .TRUE. ) THEN
           DO j=j_start(ij),j_end(ij)
           DO i=i_start(ij),i_end(ij)

              CQS2(I,J)= CQS2(I,J)*MAVAIL(I,J)
           ENDDO
           ENDDO

           IF ( FRACTIONAL_SEAICE == 1 ) THEN
              CALL wrf_error_fatal3("",1108,&
'SLAB scheme cannot be used with fractional seaice')
           ENDIF
        CALL wrf_debug(100,'in SLAB')
          CALL SLAB(t_phy,qv_curr,p_phy,flhc,flqc,  &
             psfc,xland,tmn,hfx,qfx,lh,tsk,qsfc,chklowq,          &
             gsw,glw,capg,thc,snowc,emiss,mavail,                 &
             dtbl,rcp,xlv,dtmin,ifsnow,                           &
             svp1,svp2,svp3,svpt0,ep_2,karman,eomeg,stbolt,       &
             tslb,zs,dzs,num_soil_layers,radiation,               &
             p1000mb,                                             &
             ids,ide, jds,jde, kds,kde,                           &
             ims,ime, jms,jme, kms,kme,                           &
             i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte,&
             tml,t0ml,hml,h0ml,huml,hvml,ust,u_phy,v_phy,f,g,     &
             omlcall,oml_gamma                                    )

           DO j=j_start(ij),j_end(ij)
           DO i=i_start(ij),i_end(ij)
              SFCEVP(I,J)= SFCEVP(I,J) + QFX(I,J)*DTBL
              IF(PRESENT(ACHFX))ACHFX(I,J)=ACHFX(I,J) + HFX(I,J)*DT
              IF(PRESENT(ACLHF))ACLHF(I,J)=ACLHF(I,J) + LH(I,J)*DT
           ENDDO
           ENDDO

        CALL wrf_debug(100,'in SFCDIAGS')
          CALL SFCDIAGS(hfx,qfx,tsk,qsfc,chs2,cqs2,t2,th2,q2,      &
                     psfc,cp,r_d,rcp,                              &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
             i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte    )

       ENDIF

     CASE (LSMSCHEME)

       IF (PRESENT(qv_curr)    .AND.  PRESENT(rainbl)        .AND.    &
















                                                      .TRUE. ) THEN

         IF( PRESENT(sr) ) THEN
           frpcpn=.true.
         ENDIF
         IF ( FRACTIONAL_SEAICE == 1) THEN
            IF ( isisfc == 1 ) THEN
               
            ELSE
               
               
               
               
               DO j = j_start(ij) , j_end(ij)
                  DO i = i_start(ij) , i_end(ij)
                     IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .AND. ( XICE(i,j) .LE. 1 ) ) THEN
                        IF ( SST(i,j) .LT. 271.4 ) THEN
                           SST(i,j) = 271.4
                        ENDIF
                        TSK_SEA(i,j) = SST(i,j)
                        
                        TSK(i,j) = ( TSK(i,j) - (1.-XICE(i,j)) *SST(i,j) ) / XICE(i,j)
                        IF (XICE(i,j).lt.0.2 .and. TSK(i,j).lt.253.15) THEN
                           TSK(i,j) = 253.15
                        ENDIF
                        IF (XICE(i,j).lt.0.1 .and. TSK(i,j).lt.263.15) THEN
                           TSK(i,j) = 263.15
                        ENDIF
                     ELSE
                        TSK_SEA(i,j) = TSK(i,j)
                     ENDIF
                  ENDDO
               ENDDO
            ENDIF
         ENDIF

         CALL wrf_debug(100,'in NOAH DRV')
         CALL lsm(dz8w,qv_curr,p8w,t_phy,tsk,                 &
                hfx,qfx,lh,grdflx,qgh,gsw,swdown,glw,smstav,smstot,    &
                sfcrunoff,udrunoff,ivgtyp,isltyp,isurban,isice,vegfra,        &
                albedo,albbck,znt,z0, tmn,xland,xice, emiss, embck,    &
                snowc,qsfc,rainbl,                              &
                mminlu,                                         &
                num_soil_layers,dtbl,dzs,itimestep,             &
                smois,tslb,snow,canwat,                         &
                chs, chs2, cqs2, cpm,rcp,SR,chklowq,lai,qz0,    &
                myj,frpcpn,                                     &
		sh2o,snowh,                                     & 
                u_phy,v_phy,                                    & 
                snoalb,shdmin,shdmax,                           & 
                snotime,                                        & 
                acsnom,acsnow,                                  & 
                snopcx,                                         & 
                potevp,                                         & 
                xice_threshold,                                 &
                rdlai2d,usemonalb,                              &
                br,                                             & 
                  NOAHRES,                                      &
                ids,ide, jds,jde, kds,kde,                      &
                ims,ime, jms,jme, kms,kme,                      &
                i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte,    &
                sf_urban_physics                                &

                ,tr_urb2d,tb_urb2d,tg_urb2d,tc_urb2d,qc_urb2d,  & 
                uc_urb2d,                                       & 
                xxxr_urb2d,xxxb_urb2d,xxxg_urb2d,xxxc_urb2d,    & 
                trl_urb3d,tbl_urb3d,tgl_urb3d,                  & 
                sh_urb2d,lh_urb2d,g_urb2d,rn_urb2d,ts_urb2d,    & 
                psim_urb2d,psih_urb2d,u10_urb2d,v10_urb2d,      & 
                GZ1OZ0_urb2d, AKMS_URB2D,                       & 
                th2_urb2d,q2_urb2d,ust_urb2d,                   & 
                declin_urb,cosz_urb2d,omg_urb2d,                & 
                xlat_urb2d,                                     & 
                num_roof_layers, num_wall_layers,               & 
                num_road_layers, DZR, DZB, DZG,                 & 
                FRC_URB2D, UTYPE_URB2D,                         & 
                num_urban_layers,                               & 
                trb_urb4d,tw1_urb4d,tw2_urb4d,tgb_urb4d,        & 
                sfw1_urb3d,sfw2_urb3d,sfr_urb3d,sfg_urb3d,      & 
                th_phy,rho,p_phy,ust,                           & 
                gmt,julday,xlong,xlat,                          & 
                a_u_bep,a_v_bep,a_t_bep,a_q_bep,                & 
                a_e_bep,b_u_bep,b_v_bep,                        & 
                b_t_bep,b_q_bep,b_e_bep,dlg_bep,                & 
                dl_u_bep,sf_bep,vl_bep                          & 
                )
         IF ( FRACTIONAL_SEAICE == 1 ) THEN
            IF ( isisfc .EQ. 1 ) THEN
               DO j=j_start(ij),j_end(ij)
                  DO i=i_start(ij),i_end(ij)
                     IF ( ( XICE(I,J) .GE. XICE_THRESHOLD) .AND. ( XICE(i,j) .LE. 1.0 ) ) THEN
                        
                        flhc(i,j) = ( flhc(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * flhc_sea(i,j) )
                        flqc(i,j) = ( flqc(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * flqc_sea(i,j) )
                        cpm(i,j)  = ( cpm(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * cpm_sea(i,j)  )
                        cqs2(i,j) = ( cqs2(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * cqs2_sea(i,j) )
                        chs2(i,j) = ( chs2(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * chs2_sea(i,j) )
                        chs(i,j)  = ( chs(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * chs_sea(i,j)  )
                        qsfc(i,j) = ( qsfc(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * qsfc_sea(i,j) )
                        qgh(i,j)  = ( qgh(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * qgh_sea(i,j)  )
                        qz0(i,j)  = ( qz0(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * qz0_sea(i,j)  )
                        hfx(i,j)  = ( hfx(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * hfx_sea(i,j)  )
                        qfx(i,j)  = ( qfx(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * qfx_sea(i,j)  )
                        lh(i,j)   = ( lh(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * lh_sea(i,j)   )
                        tsk(i,j)  = ( tsk(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * tsk_sea(i,j)  )
                     ENDIF
                  ENDDO
               ENDDO
            ELSE
               DO j = j_start(ij) , j_end(ij)
                  DO i = i_start(ij) , i_end(ij)
                     IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .AND. ( XICE(i,j) .LE. 1.0 ) ) THEN
                        
                        tsk(i,j) = ( tsk(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * tsk_sea(i,j) )
                     ENDIF
                  ENDDO
               ENDDO
            ENDIF
         ENDIF
           DO j=j_start(ij),j_end(ij)
           DO i=i_start(ij),i_end(ij)

               SFCEVP(I,J)= SFCEVP(I,J) + QFX(I,J)*DTBL
               SFCEXC(I,J)= CHS(I,J)
               IF(PRESENT(ACHFX))ACHFX(I,J)=ACHFX(I,J) + HFX(I,J)*DT
               IF(PRESENT(ACLHF))ACLHF(I,J)=ACLHF(I,J) + LH(I,J)*DT
               IF(PRESENT(ACGRDFLX))ACGRDFLX(I,J)=ACGRDFLX(I,J) + GRDFLX(I,J)*DT
           ENDDO
           ENDDO

          CALL SFCDIAGS(HFX,QFX,TSK,QSFC,CHS2,CQS2,T2,TH2,Q2,      &
                     PSFC,CP,R_d,RCP,                              &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
             i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte    )

     IF(SF_URBAN_PHYSICS.eq.1) THEN
       DO j=j_start(ij),j_end(ij)                             
         DO i=i_start(ij),i_end(ij)                           
          IF( IVGTYP(I,J) == ISURBAN .or. IVGTYP(I,J) == 31 .or. &  
              IVGTYP(I,J) == 32 .or. IVGTYP(I,J) == 33 ) THEN 







             U10(I,J)  = U10_URB2D(I,J)                       
             V10(I,J)  = V10_URB2D(I,J)                       
             PSIM(I,J) = PSIM_URB2D(I,J)                      
             PSIH(I,J) = PSIH_URB2D(I,J)                      
             GZ1OZ0(I,J) = GZ1OZ0_URB2D(I,J)                  

             AKHS(I,J) = CHS(I,J)                             
             AKMS(I,J) = AKMS_URB2D(I,J)                      
           END IF                                             
         ENDDO                                                
       ENDDO                                                  
     ENDIF

     IF(SF_URBAN_PHYSICS.eq.2) THEN
       DO j=j_start(ij),j_end(ij)                             
         DO i=i_start(ij),i_end(ij)                           
          IF( IVGTYP(I,J) == ISURBAN .or. IVGTYP(I,J) == 31 .or. &  
              IVGTYP(I,J) == 32 .or. IVGTYP(I,J) == 33 ) THEN 
            T2(I,J)   = TH_PHY(i,1,j)/((1.E5/PSFC(I,J))**RCP) 
            TH2(I,J) = TH_PHY(i,1,j) 
            Q2(I,J)   = qv_curr(i,1,j)  
            U10(I,J)  = U_phy(I,1,J)                       
            V10(I,J)  = V_phy(I,1,J)                       
           END IF                                             
         ENDDO                                                
       ENDDO                                                  
     ENDIF



       ELSE
         CALL wrf_error_fatal3("",1340,&
'Lacking arguments for LSM in surface driver')
       ENDIF

     CASE (RUCLSMSCHEME)
       IF (PRESENT(qv_curr)    .AND.  PRESENT(qc_curr) .AND.    &

           PRESENT(qsg)        .AND.  PRESENT(qvg)     .AND.    &
           PRESENT(qcg)        .AND.  PRESENT(soilt1)  .AND.    &
           PRESENT(tsnav)      .AND.  PRESENT(smfr3d)  .AND.    &
           PRESENT(keepfr3dflag) .AND. PRESENT(rainbl) .AND.    &
                                                      .TRUE. ) THEN

           IF( PRESENT(sr) ) THEN
               frpcpn=.true.
           ELSE
               SR = 1.
           ENDIF
           CALL wrf_debug(100,'in RUC LSM')
           IF ( FRACTIONAL_SEAICE == 1 ) THEN
              IF ( isisfc == 1 ) THEN
                 
                 
                 
              ELSE
                 
                 
                 
                 
                 DO j = j_start(ij) , j_end(ij)
                    DO i = i_start(ij) , i_end(ij)
                       IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .AND. ( XICE(i,j) .LE. 1.0 ) ) THEN
                          TSK_SEA(i,j) = SST(i,j)
                          IF ( SST(i,j) .LT. 271. ) THEN
                             SST(i,j) = 271.4
                             TSK_SEA(i,j) = SST(i,j)
                          endif
                          TSK(i,j) = ( TSK(i,j) - (1.-XICE(i,j)) *SST(i,j) ) / XICE(i,j)
                          IF ( ( XICE(i,j) .LT. 0.2 ) .AND. ( TSK(i,j) .LT. 253.15 ) ) THEN
                             TSK(i,j) = 253.15
                          ENDIF
                          IF ( ( XICE(i,j).LT.0.1 ) .AND. ( TSK(i,j).lt.263.15 ) ) THEN
                             TSK(i,j) = 263.15
                          ENDIF
                       ELSE
                          TSK_SEA(i,j) = TSK(i,j)
                       ENDIF
                    ENDDO
                 ENDDO
              ENDIF
           ENDIF

           CALL LSMRUC(dtbl,itimestep,num_soil_layers,          &
                zs,rainbl,snow,snowh,snowc,sr,frpcpn,           &
                dz8w,p8w,t_phy,qv_curr,qc_curr,rho,             & 
                glw,gsw,emiss,chklowq,                          &
                chs,flqc,flhc,mavail,canwat,vegfra,albedo,znt,  &
                snoalb, albbck,                                 &   
                qsfc,qsg,qvg,qcg,soilt1,tsnav,                  &
                tmn,ivgtyp,isltyp,xland,xice,                   &
                cp,g,xlv,stbolt,                                &
                smois,sh2o,smstav,smstot,tslb,tsk,hfx,qfx,lh,   &
                sfcrunoff,udrunoff,sfcexc,                      &
                sfcevp,grdflx,acsnow,                           &
                smfr3d,keepfr3dflag,                            &
                myj,                                            &
                ids,ide, jds,jde, kds,kde,                      &
                ims,ime, jms,jme, kms,kme,                      &
                i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte    )

           IF ( FRACTIONAL_SEAICE == 1 ) THEN
              if ( isisfc == 1 ) then
                 
                 
                 
                 DO j=j_start(ij),j_end(ij)
                    DO i=i_start(ij),i_end(ij)
                       IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .and. ( XICE(i,j) .LE. 1.0 ) ) THEN
                          flhc(i,j) = ( flhc(i,j) * XICE(i,j) ) + ( (1.-XICE(i,j)) * flhc_sea(i,j) )
                          flqc(i,j) = ( flqc(i,j) * XICE(i,j) ) + ( (1.-XICE(i,j)) * flqc_sea(i,j) )
                          cpm(i,j)  = ( cpm(i,j)  * XICE(i,j) ) + ( (1.-XICE(i,j)) * cpm_sea(i,j)  )
                          cqs2(i,j) = ( cqs2(i,j) * XICE(i,j) ) + ( (1.-XICE(i,j)) * cqs2_sea(i,j) )
                          chs2(i,j) = ( chs2(i,j) * XICE(i,j) ) + ( (1.-XICE(i,j)) * chs2_sea(i,j) )
                          chs(i,j)  = ( chs(i,j)  * XICE(i,j) ) + ( (1.-XICE(i,j)) * chs_sea(i,j)  )
                          qsfc(i,j) = ( qsfc(i,j) * XICE(i,j) ) + ( (1.-XICE(i,j)) * QSFC_SEA(i,j) )
                          qgh(i,j)  = ( qgh(i,j)  * XICE(i,j) ) + ( (1.-XICE(i,j)) * qgh_sea(i,j)  )
                          hfx(i,j)  = ( hfx(i,j)  * XICE(i,j) ) + ( (1.-XICE(i,j)) * HFX_SEA(i,j)  )
                          qfx(i,j)  = ( qfx(i,j)  * XICE(i,j) ) + ( (1.-XICE(i,j)) * QFX_SEA(i,j)  )
                          lh(i,j)   = ( lh(i,j)   * XICE(i,j) ) + ( (1.-XICE(i,j)) * LH_SEA(i,j)   )
                          tsk(i,j)  = ( tsk(i,j)  * XICE(i,j) ) + ( (1.-XICE(i,j)) * TSK_SEA(i,j)  )
                       ENDIF
                    ENDDO
                 ENDDO
              else
                 
                 
                 
                 DO j = j_start(ij) , j_end(ij)
                    DO i = i_start(ij) , i_end(ij)
                       IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .and. ( XICE(i,j) .LE. 1.0 ) ) THEN
                          tsk(i,j) = ( tsk(i,j) * XICE(i,j) ) + ( (1.-XICE(i,j)) * TSK_SEA(i,j) )
                       ENDIF
                    ENDDO
                 ENDDO
              endif
           ENDIF

             
          CALL SFCDIAGS(HFX,QFX,TSK,QVG,CHS2,CQS2,T2,TH2,Q2,      &
                     PSFC,CP,R_d,RCP,                              &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
             i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte    )



       ELSE
         CALL wrf_error_fatal3("",1457,&
'Lacking arguments for RUCLSM in surface driver')
       ENDIF

     CASE (PXLSMSCHEME)
       IF (PRESENT(qv_curr)    .AND.  PRESENT(qc_curr) .AND.    &
           PRESENT(emiss)      .AND.  PRESENT(t2)      .AND.    &
           PRESENT(qsg)        .AND.  PRESENT(qvg)     .AND.    &
           PRESENT(qcg)        .AND.  PRESENT(soilt1)  .AND.    &
           PRESENT(tsnav)      .AND.  PRESENT(smfr3d)  .AND.    &
           PRESENT(keepfr3dflag) .AND. PRESENT(rainbl) .AND.    &
                                                      .TRUE. ) THEN
          IF ( FRACTIONAL_SEAICE == 1 ) THEN

             CALL wrf_error_fatal3("",1471,&
"PXLSM not adapted for FRACTIONAL_SEAICE=1 option")

             IF ( ISISFC .EQ. 1 ) THEN
                
                
                
             ELSE
                
                
                
                
                DO j = j_start(ij) , j_end(ij)
                   DO i=i_start(ij) , i_end(ij)
                      IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .AND. ( XICE(i,j) .LE. 1.0 ) ) THEN
                         TSK_SEA(i,j) = SST(i,j)
                         IF ( SST(i,j) .LT. 271. ) THEN
                            SST(i,j) = 271.4
                            TSK_SEA(i,j) = SST(i,j)
                         ENDIF
                         TSK(i,j) = ( TSK(i,j) - (1.-XICE(i,j)) *SST(i,j) ) / XICE(i,j)
                         IF ( ( XICE(i,j) .LT. 0.2 ) .AND. ( TSK(i,j) .lt. 253.15 ) ) THEN
                            TSK(i,j) = 253.15
                         ENDIF
                         IF ( ( XICE(i,j) .LT. 0.1 ) .AND. ( TSK(i,j) .LT. 263.15 ) ) THEN
                            TSK(i,j) = 263.15
                         ENDIF
                      ELSE
                         TSK_SEA(i,j) = TSK(i,j)
                      ENDIF
                   ENDDO
                ENDDO
             ENDIF
          ENDIF
          CALL wrf_debug(100,'in P-X LSM')
          CALL PXLSM(u_phy, v_phy, dz8w, qv_curr, t_phy, th_phy, rho,&
                     psfc, gsw, glw, rainbl, emiss,                  &
                     ITIMESTEP, num_soil_layers, DT, anal_interval,  &
                     xland, albbck, albedo, snoalb, smois, tslb,     &
                     mavail,T2, Q2,                                  &
                     zs, dzs, psih,                                  &
                     landusef,soilctop,soilcbot,vegfra, vegf_px,     &
                     isltyp,ra,rs,lai,nlcat,nscat,                   &
                     hfx,qfx,lh,tsk,znt,canwat,                      &
                     grdflx,shdmin,shdmax,                           &
                     snowc,pblh,rmol,ust,capg,dtbl,                  &
                     t2_ndg_old,t2_ndg_new,q2_ndg_old,q2_ndg_new,    &
                     sn_ndg_old, sn_ndg_new, snow, snowh,snowncv,    &
                     t2obs, q2obs, pxlsm_smois_init, pxlsm_soil_nudge, &
                     ids,ide, jds,jde, kds,kde,                      &
                     ims,ime, jms,jme, kms,kme,                      &
                     i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte)
          IF ( FRACTIONAL_SEAICE == 1 ) THEN
             IF ( ISISFC .EQ. 1 ) THEN
                
                
                
                DO j = j_start(ij) , j_end(ij)
                   DO i = i_start(ij) , i_end(ij)
                      IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .AND. ( XICE(i,j) .LE. 1.0 ) ) THEN
                         flhc(i,j) = ( flhc(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * flhc_sea(i,j) )
                         flqc(i,j) = ( flqc(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * flqc_sea(i,j) )
                         cpm(i,j)  = ( cpm(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * cpm_sea(i,j)  )
                         cqs2(i,j) = ( cqs2(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * cqs2_sea(i,j) )
                         chs2(i,j) = ( chs2(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * chs2_sea(i,j) )
                         chs(i,j)  = ( chs(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * chs_sea(i,j)  )
                         qsfc(i,j) = ( qsfc(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * QSFC_SEA(i,j) )
                         qgh(i,j)  = ( qgh(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * QGH_SEA(i,j)  )
                         hfx(i,j)  = ( hfx(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * HFX_SEA(i,j)  )
                         qfx(i,j)  = ( qfx(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * QFX_SEA(i,j)  )
                         lh(i,j)   = ( lh(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * LH_SEA(i,j)   )
                         tsk(i,j)  = ( tsk(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * TSK_SEA(i,j)  )
                         psih(i,j) = ( psih(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * PSIH_SEA(i,j) )
                         pblh(i,j) = ( pblh(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * PBLH_SEA(i,j) )
                         rmol(i,j) = ( rmol(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * RMOL_SEA(i,j) )
                         ust(i,j)  = ( ust(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * UST_SEA(i,j)  )
                      ENDIF
                   ENDDO
                ENDDO
             ELSE
                
                
                
                DO j=j_start(ij),j_end(ij)
                   DO i=i_start(ij),i_end(ij)
                      IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .AND. ( XICE(i,j) .LE. 1.0 ) ) THEN
                         tsk(i,j)=tsk(i,j)*XICE(i,j)+(1.0-XICE(i,j))*TSK_SEA(i,j)
                      ENDIF
                   ENDDO
                ENDDO
             ENDIF
          ENDIF
           DO j=j_start(ij),j_end(ij)
           DO i=i_start(ij),i_end(ij)
              CHKLOWQ(I,J)= 1.0
              TH2(I,J) = T2(I,J)*(1.E5/PSFC(I,J))**RCP
              SFCEVP(I,J)= SFCEVP(I,J) + QFX(I,J)*DTBL
           ENDDO
           ENDDO

       ELSE
         CALL wrf_error_fatal3("",1572,&
'Lacking arguments for P-X LSM in surface driver')
       ENDIF

     CASE DEFAULT

       IF ( itimestep .eq. 1 ) THEN
       WRITE( message , * ) &
        'No land surface physics option is used: sf_surface_physics = ', sf_surface_physics
        CALL wrf_message ( message )
       ENDIF

     END SELECT sfc_select

     ENDDO
     !$OMP END PARALLEL DO

 430 CONTINUE



     IF ( PRESENT( rainbl ) ) THEN
       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij, i, j, k )
       DO ij = 1 , num_tiles
         DO j=j_start(ij),j_end(ij)
         DO i=i_start(ij),i_end(ij)
            RAINBL(i,j) = 0.
         ENDDO
         ENDDO
       ENDDO
       !$OMP END PARALLEL DO
     ENDIF

   ENDIF

   END SUBROUTINE surface_driver




   subroutine myjsfc_seaice_wrapper(ITIMESTEP,HT,DZ,      &
        &     PMID,PINT,TH,T,QV,QC,U,V,Q2,                &
        &     TSK,QSFC,THZ0,QZ0,UZ0,VZ0,                  &
        &     LOWLYR,XLAND,                               &
        &     XICE_THRESHOLD,                             &  
        &     XICE,SST,                                   &  
        &     CHS_SEA, CHS2_SEA, CQS2_SEA, CPM_SEA,       &  
        &     FLHC_SEA, FLQC_SEA, QSFC_SEA,               &  
        &     QGH_SEA, QZ0_SEA, HFX_SEA, QFX_SEA,         &  
        &     FLX_LH_SEA, TSK_SEA,                        &  
        &     USTAR,ZNT,Z0BASE,PBLH,MAVAIL,RMOL,          &
        &     AKHS,AKMS,                                  &
        &     BR,                                         &
        &     CHS,CHS2,CQS2,HFX,QFX,FLX_LH,FLHC,FLQC,     &
        &     QGH,CPM,CT,                                 &
        &     U10,V10,T02,TH02,TSHLTR,TH10,Q02,QSHLTR,Q10,PSHLTR,          &
        &     P1000,                                        &
        &     IDS,IDE,JDS,JDE,KDS,KDE,                        &
        &     IMS,IME,JMS,JME,KMS,KME,                        &
        &     ITS,ITE,JTS,JTE,KTS,KTE )

     USE module_sf_myjsfc

     IMPLICIT NONE

     INTEGER,                                INTENT(IN)    :: ITIMESTEP
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(IN)    :: HT
     REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN)    :: DZ
     REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN)    :: PMID
     REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN)    :: PINT
     REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN)    :: TH
     REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN)    :: T
     REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN)    :: QV
     REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN)    :: QC
     REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN)    :: U
     REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN)    :: V
     REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN)    :: Q2   

     
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT)    :: TSK

     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: QSFC
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: THZ0
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: QZ0
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: UZ0
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: VZ0
     INTEGER,DIMENSION(IMS:IME,JMS:JME),     INTENT(IN)    :: LOWLYR
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(IN)    :: XLAND
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(IN)    :: XICE       
     
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: SST        
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: BR
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: CHS_SEA    
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: CHS2_SEA   
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: CQS2_SEA   
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: CPM_SEA    
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: QZ0_SEA   
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: QSFC_SEA   
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: QGH_SEA   
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: FLHC_SEA  
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: FLQC_SEA  
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: HFX_SEA    
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: QFX_SEA    
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: FLX_LH_SEA 
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: TSK_SEA    
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: USTAR
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: ZNT
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(IN)    :: Z0BASE
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: PBLH
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(IN)    :: MAVAIL
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: RMOL
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: AKHS
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: AKMS
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: CHS
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: CHS2
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: CQS2
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: HFX
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: QFX
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: FLX_LH
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: FLHC
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: FLQC
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: QGH
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: CPM
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: CT
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: U10
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: V10
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: T02
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: TH02
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: TSHLTR
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: TH10
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: Q02
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: QSHLTR
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: Q10
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: PSHLTR
     REAL,                                   INTENT(IN)    :: P1000
     REAL,                                   INTENT(IN)    :: XICE_THRESHOLD
     INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE,       &
          &                IMS,IME,JMS,JME,KMS,KME,       &
          &                ITS,ITE,JTS,JTE,KTS,KTE


     
     INTEGER :: i
     INTEGER :: j
     REAL, DIMENSION( ims:ime, jms:jme ) :: ct_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: u10_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: v10_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: t02_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: th02_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: tshltr_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: pshltr_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: qshltr_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: th10_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: q02_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: q10_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: thz0_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: uz0_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: vz0_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: ustar_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: pblh_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: rmol_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: akhs_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: akms_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: xland_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: mavail_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: znt_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: z0base_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: br_sea

     REAL, DIMENSION( ims:ime, jms:jme ) :: QSFC_HOLD
     REAL, DIMENSION( ims:ime, jms:jme ) :: QZ0_HOLD
     REAL, DIMENSION( ims:ime, jms:jme ) :: THZ0_HOLD
     REAL, DIMENSION( ims:ime, jms:jme ) :: UZ0_HOLD
     REAL, DIMENSION( ims:ime, jms:jme ) :: VZ0_HOLD
     REAL, DIMENSION( ims:ime, jms:jme ) :: USTAR_HOLD
     REAL, DIMENSION( ims:ime, jms:jme ) :: ZNT_HOLD
     REAL, DIMENSION( ims:ime, jms:jme ) :: PBLH_HOLD
     REAL, DIMENSION( ims:ime, jms:jme ) :: RMOL_HOLD
     REAL, DIMENSION( ims:ime, jms:jme ) :: AKHS_HOLD
     REAL, DIMENSION( ims:ime, jms:jme ) :: AKMS_HOLD
     REAL :: PSFC

     
     

     
     DO j = JTS , JTE
        DO i = ITS , ITE
           IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .and. ( XICE(i,j) .LE. 1.0 ) ) THEN

              TSK_SEA(i,j) = SST(i,j)

              IF ( SST(i,j) .LT. 271.4 ) THEN
                 SST(i,j) = 271.4
                 TSK_SEA(i,j) = SST(i,j)
              ENDIF

              IF ( ( SST(i,j) .GT. 273.0 ) .AND. ( itimestep .LE. 3 ) ) THEN
                 
                 IF ( XICE(i,j) .GE. 0.6 ) THEN
                    SST(i,j) = 271.4
                    TSK_SEA(i,j) = SST(i,j)
                 ELSEIF ( XICE(i,j) .GE. 0.4 ) THEN
                    SST(i,j) = 273.
                    TSK_SEA(i,j) = SST(i,j)
                 ELSEIF ( ( XICE(i,j) .GE. 0.2 ) .and. ( SST(i,j).GT.275. ) ) THEN
                    SST(i,j) = 275.
                    TSK_SEA(i,j) = SST(i,j)
                 ELSEIF (SST(i,j).GT.278.) THEN
                    SST(i,j) = 278.
                    TSK_SEA(i,j) = SST(i,j)
                 ENDIF
              ENDIF

              
              

              
              
              
              TSK(i,j) = ( TSK(i,j) - (1.0-XICE(i,j)) *SST(i,j) ) / XICE(i,j)

              IF (XICE(i,j).lt.0.2 .and. TSK(i,j).lt.253.15) THEN
                 TSK(i,j) = 253.15
              ENDIF
              IF (XICE(i,j).lt.0.1 .and. TSK(i,j).lt.263.15) THEN
                 TSK(i,j) = 263.15
              ENDIF

              
              
              PSFC = PINT(I,LOWLYR(I,J),J)
              QSFC_SEA(i,j) = PQ0SEA/PSFC*EXP(A2S*(TSK(i,j)-A3S)/(TSK(i,j)-A4S))
              QSFC(i,j) = QSFC(i,j) - (1.0-XICE(i,j)) * QSFC_SEA(i,j) / XICE(i,j)

              HFX_SEA(i,j)  = HFX(i,j)
              QFX_SEA(i,j)  = QFX(i,j)
              FLX_LH_SEA(i,j)   = FLX_LH(i,j)

           ELSE
              TSK_SEA(i,j) = TSK(i,j)
           ENDIF
        ENDDO
     ENDDO







     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     




     
     QSFC_HOLD  = QSFC
     QZ0_HOLD   = QZ0
     THZ0_HOLD  = THZ0
     UZ0_HOLD   = UZ0
     VZ0_HOLD   = VZ0
     USTAR_HOLD = USTAR
     ZNT_HOLD   = ZNT
     PBLH_HOLD  = PBLH
     RMOL_HOLD  = RMOL
     AKHS_HOLD  = AKHS
     AKMS_HOLD  = AKMS



     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     

     
     CALL MYJSFC ( ITIMESTEP, HT, DZ,                              &  
          &        PMID, PINT, TH, T, QV, QC, U, V, Q2,            &  
          &        TSK, QSFC, THZ0, QZ0, UZ0, VZ0,                 &  
          &        LOWLYR, XLAND,                                  &  
          &        USTAR, ZNT, Z0BASE, PBLH, MAVAIL, RMOL,         &  
          &        AKHS, AKMS,                                     &  
          &        BR,                                             &  
          &        CHS, CHS2, CQS2, HFX, QFX, FLX_LH, FLHC, FLQC,  &  
          &        QGH, CPM, CT, U10, V10, T02,                    &  
          &        TH02, TSHLTR, TH10, Q02,                        &  
          &        QSHLTR, Q10, PSHLTR,                            &  
          &        P1000,                                        &  
          &        ids,ide, jds,jde, kds,kde,                      &
          &        ims,ime, jms,jme, kms,kme,                      &
          &        its,ite, jts,jte, kts,kte    )

     
     DO j = JTS, JTE
        DO i = ITS, ITE
           IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .AND. ( XICE(i,j) .LE. 1.0 ) ) THEN
              XLAND_SEA(i,j)=2.
              MAVAIL_SEA(I,J)  = 1.
              ZNT_SEA(I,J) = 0.0001
              Z0BASE_SEA(I,J) = ZNT_SEA(I,J)
              IF ( SST(i,j) .LT. 271.4 ) THEN
                 SST(i,j) = 271.4
              ENDIF
              TSK_SEA(i,j) = SST(i,j)
              PSFC = PINT(I,LOWLYR(I,J),J)
              QSFC_SEA(I,J) = PQ0SEA/PSFC*EXP(A2S*(TSK_SEA(i,j)-A3S)/(TSK_SEA(i,j)-A4S))
           ELSE
              
              XLAND_SEA(i,j)=xland(i,j)
              MAVAIL_SEA(i,j) = mavail(i,j)
              ZNT_SEA(I,J)    = ZNT_HOLD(I,J)
              Z0BASE_SEA(I,J) = Z0BASE(I,J)
              TSK_SEA(i,j)  = TSK(i,j)
              QSFC_SEA(i,j) = QSFC_HOLD(i,j)
           ENDIF
        ENDDO
     ENDDO

     QZ0_SEA  = QZ0_HOLD
     THZ0_SEA = THZ0_HOLD
     UZ0_SEA  = UZ0_HOLD
     VZ0_SEA  = VZ0_HOLD
     USTAR_SEA = USTAR_HOLD
     PBLH_SEA = PBLH_HOLD
     RMOL_SEA = RMOL_HOLD
     AKHS_SEA = AKHS_HOLD
     AKMS_SEA = AKMS_HOLD




     CALL MYJSFC ( ITIMESTEP, HT, DZ,                                                          & 
          &        PMID, PINT, TH, T, QV, QC, U, V, Q2,                                        & 
          &        TSK_SEA, QSFC_SEA, THZ0_SEA, QZ0_SEA, UZ0_SEA, VZ0_SEA,                     & 
          &        LOWLYR, XLAND_SEA,                                                    & 
          &        USTAR_SEA, ZNT_SEA, Z0BASE_SEA, PBLH_SEA, MAVAIL_SEA, RMOL_SEA,             & 
          &        AKHS_SEA, AKMS_SEA,                                                         & 
          &        BR_SEA,                                                                     & 
          &        CHS_SEA, CHS2_SEA, CQS2_SEA, HFX_SEA, QFX_SEA, FLX_LH_SEA, FLHC_SEA,        & 
          &        FLQC_SEA, QGH_SEA, CPM_SEA, CT_SEA, U10_SEA, V10_SEA, T02_SEA, TH02_SEA,    & 
          &        TSHLTR_SEA, TH10_SEA, Q02_SEA, QSHLTR_SEA, Q10_SEA, PSHLTR_SEA,             & 
          &        p1000,                                                                    & 
          &        ids,ide, jds,jde, kds,kde,                                                  &
          &        ims,ime, jms,jme, kms,kme,                                                  &
          &        its,ite, jts,jte, kts,kte    )





     DO j = JTS, JTE
        DO i = ITS, ITE
           IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .and. ( XICE(i,j) .LE. 1.0 ) ) THEN
              

              
              
              
              
              
              CT(i,j)     = CT(i,j)     * XICE(i,j) + (1.0-XICE(i,j)) * CT_SEA (i,j)
              
              
              
              
              PSHLTR(i,j) = PSHLTR(i,j) * XICE(i,j) + (1.0-XICE(i,j)) * PSHLTR_SEA(i,j)
              
              
              QSHLTR(i,j) = QSHLTR(i,j) * XICE(i,j) + (1.0-XICE(i,j)) * QSHLTR_SEA(i,j)
              Q02(i,j)    = Q02(i,j)    * XICE(i,j) + (1.0-XICE(i,j)) * Q02_SEA(i,j)
              Q10(i,j)    = Q10(i,j)    * XICE(i,j) + (1.0-XICE(i,j)) * Q10_SEA(i,j)
              TH02(i,j)   = TH02(i,j)   * XICE(i,j) + (1.0-XICE(i,j)) * TH02_SEA(i,j)
              TH10(i,j)   = TH10(i,j)   * XICE(i,j) + (1.0-XICE(i,j)) * TH10_SEA(i,j)
              TSHLTR(i,j) = TSHLTR(i,j) * XICE(i,j) + (1.0-XICE(i,j)) * TSHLTR_SEA(i,j)
              T02(i,j)    = T02(i,j)    * XICE(i,j) + (1.0-XICE(i,j)) * T02_SEA(i,j)
              U10(i,j)    = U10(i,j)    * XICE(i,j) + (1.0-XICE(i,j)) * U10_SEA(i,j)
              V10(i,j)    = V10(i,j)    * XICE(i,j) + (1.0-XICE(i,j)) * V10_SEA(i,j)

              
              
              THZ0(i,j)   = THZ0(i,j)   * XICE(i,j) + (1.0-XICE(i,j)) * THZ0_SEA(i,j)
              
              UZ0(i,j)    = UZ0(i,j)    * XICE(i,j) + (1.0-XICE(i,j)) * UZ0_SEA(i,j)
              VZ0(i,j)    = VZ0(i,j)    * XICE(i,j) + (1.0-XICE(i,j)) * VZ0_SEA(i,j)
              USTAR(i,j)  = USTAR(i,j)  * XICE(i,j) + (1.0-XICE(i,j)) * USTAR_SEA(i,j)
              
              PBLH(i,j)   = PBLH(i,j)   * XICE(i,j) + (1.0-XICE(i,j)) * PBLH_SEA(i,j)
              RMOL(i,j)   = RMOL(i,j)   * XICE(i,j) + (1.0-XICE(i,j)) * RMOL_SEA(i,j)
              AKHS(i,j)   = AKHS(i,j)   * XICE(i,j) + (1.0-XICE(i,j)) * AKHS_SEA(i,j)
              AKMS(i,j)   = AKMS(i,j)   * XICE(i,j) + (1.0-XICE(i,j)) * AKMS_SEA(i,j)

              
           ELSE
              
           ENDIF
        ENDDO
     ENDDO

   END SUBROUTINE myjsfc_seaice_wrapper




   SUBROUTINE sf_gfs_seaice_wrapper(U3D,V3D,T3D,QV3D,P3D,        &
		 CP,ROVCP,R,XLV,PSFC,CHS,CHS2,CQS2,CPM,          &
                     ZNT,UST,PSIM,PSIH,                          &
                     XLAND,HFX,QFX,LH,TSK,FLHC,FLQC,             &
                     QGH,QSFC,U10,V10,                           &
                     GZ1OZ0,WSPD,BR,ISFFLX,                      &
                     EP1,EP2,KARMAN,itimestep,                   &
                     XICE_THRESHOLD,                             &
                     CHS_SEA, CHS2_SEA, CPM_SEA, CQS2_SEA,       &
                     FLHC_SEA, FLQC_SEA,                         &
                     HFX_SEA, LH_SEA, QFX_SEA, QGH_SEA, QSFC_SEA,&
                     UST_SEA, ZNT_SEA, SST, XICE,                &
                     ids,ide, jds,jde, kds,kde,                  &
                     ims,ime, jms,jme, kms,kme,                  &
                     its,ite, jts,jte, kts,kte                   )
     USE module_sf_gfs
     implicit none

     INTEGER, INTENT(IN) ::             ids,ide, jds,jde, kds,kde,      &
                                        ims,ime, jms,jme, kms,kme,      &
                                        its,ite, jts,jte, kts,kte,      &
                                        ISFFLX,itimestep

      REAL,    INTENT(IN) ::                                            &
                                        CP,                             &
                                        EP1,                            &
                                        EP2,                            &
                                        KARMAN,                         &
                                        R,                              &
                                        ROVCP,                          &
                                        XLV

      REAL,    DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) ::      &
                                        P3D,                            &
                                        QV3D,                           &
                                        T3D,                            &
                                        U3D,                            &
                                        V3D

      REAL,    DIMENSION(ims:ime, jms:jme), INTENT(IN) ::               &
                                        TSK,                            &
                                        PSFC,                           &
                                        XLAND

      REAL,    DIMENSION(ims:ime, jms:jme), INTENT(INOUT) ::            &
                                        UST,                            &
                                        ZNT

      REAL, DIMENSION(ims:ime, jms:jme), INTENT(OUT) ::                 &
                                        BR,                             &
                                        CHS,                            &
                                        CHS2,                           &
                                        CPM,                            &
                                        CQS2,                           &
                                        FLHC,                           &
                                        FLQC,                           &
                                        GZ1OZ0,                         &
                                        HFX,                            &
                                        LH,                             &
                                        PSIM,                           &
                                        PSIH,                           &
                                        QFX,                            &
                                        QGH,                            &
                                        QSFC,                           &
                                        U10,                            &
                                        V10,                            &
                                        WSPD

      REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN) ::                  &
                                        XICE
      REAL, DIMENSION(ims:ime, jms:jme), INTENT(OUT) ::                 &
                                        CHS_SEA,                        &
                                        CHS2_SEA,                       &
                                        CPM_SEA,                        &
                                        CQS2_SEA,                       &
                                        FLHC_SEA,                       &
                                        FLQC_SEA,                       &
                                        HFX_SEA,                        &
                                        LH_SEA,                         &
                                        QFX_SEA,                        &
                                        QGH_SEA,                        &
                                        QSFC_SEA,                       &
                                        UST_SEA,                        &
                                        ZNT_SEA
      REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) ::               &
                                        SST

      REAL,                              INTENT(IN)    ::               &
                                        XICE_THRESHOLD




      INTEGER :: I
      INTEGER :: J
      REAL, DIMENSION(ims:ime, jms:jme) ::                              &
                                        BR_SEA,                         &
                                        GZ1OZ0_SEA,                     &
                                        PSIM_SEA,                       &
                                        PSIH_SEA,                       &
                                        U10_SEA,                        &
                                        V10_SEA,                        &
                                        WSPD_SEA,                       &
                                        XLAND_SEA,                &
                                        TSK_SEA,                        &
                                        UST_HOLD,                       &
                                        ZNT_HOLD,                       &
                                        TSK_LOCAL


     DO j = JTS , JTE
        DO i = ITS , ITE
           IF ( ( XICE(i,j) .GE. XICE_THRESHOLD ) .and. ( XICE(I,J) .LE. 1.0 ) ) THEN
              

              IF ( SST(i,j) .LT. 271.4 ) THEN
                 SST(i,j) = 271.4
              ENDIF

              IF ( SST(i,j) .GT. 273. .and. itimestep .le. 3) then
                 
                 IF ( XICE(i,j) .GE. 0.6 ) THEN
                    SST(i,j) = 271.4
                 ELSEIF ( XICE(i,j) .GE. 0.4 ) THEN
                    SST(i,j) = 273.
                 ELSEIF (XICE(i,j).GE.0.2 .and. SST(i,j).GT.275.) THEN
                    SST(i,j) = 275.
                 ELSEIF (SST(i,j).GT.278.) THEN
                    SST(i,j) = 278.
                 ENDIF
              ENDIF
              TSK_SEA(i,j) = SST(i,j)

              
              
              

              TSK_LOCAL(i,j) = ( TSK(i,j) - (1.0-XICE(i,j)) * SST(i,j) ) / XICE(i,j)

              IF ( ( XICE(i,j) .LT. 0.2 ) .AND. ( TSK_LOCAL(i,j) .LT. 253.15 ) ) THEN
                 TSK_LOCAL(i,j) = 253.15
              ENDIF
              IF ( ( XICE(i,j) .LT. 0.1 ) .and. ( TSK_LOCAL(i,j) .LT. 263.15 ) ) THEN
                 TSK_LOCAL(i,j) = 263.15
              ENDIF

           ELSE
              
              TSK_LOCAL(i,j) = TSK(i,j)
           ENDIF

        ENDDO
     ENDDO





























     ZNT_HOLD = ZNT
     UST_HOLD = UST
























     CALL SF_GFS(U3D,V3D,T3D,QV3D,P3D,                  &
          CP,ROVCP,R,XLV,PSFC,CHS,CHS2,CQS2,CPM_SEA,    &
          ZNT,UST,PSIM,PSIH,                            &
          XLAND,HFX,QFX,LH,TSK_LOCAL,FLHC,FLQC,         &
          QGH,QSFC,U10,V10,                             &
          GZ1OZ0,WSPD,BR,ISFFLX,                        &
          EP1,EP2,KARMAN,ITIMESTEP,                     &
          ids,ide, jds,jde, kds,kde,                    &
          ims,ime, jms,jme, kms,kme,                    &
          its,ite, jts,jte, kts,kte                     )



     DO j = JTS , JTE
        DO i = ITS , ITE
           IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .and. ( XICE(i,j) .LE. 1.0 ) ) THEN
              
              XLAND_SEA(i,j)=2.
              ZNT_SEA(I,J) = 0.0001
              IF ( SST(i,j) .LT. 271.4 ) THEN
                 SST(i,j) = 271.4
              ENDIF
              TSK_SEA(i,j) = SST(i,j)
           ELSE
              
              XLAND_SEA(i,j)=xland(i,j)
              ZNT_SEA(I,J) = ZNT_HOLD(I,J)
              UST_SEA(i,j) = UST_HOLD(i,j)
              TSK_SEA(i,j) = TSK(i,j)
           ENDIF
        ENDDO
     ENDDO

     
     
     CALL SF_GFS(U3D,V3D,T3D,QV3D,P3D,                  &
          CP,ROVCP,R,XLV,PSFC,CHS_SEA,CHS2_SEA,CQS2_SEA,CPM,        &
          ZNT_SEA,UST_SEA,PSIM_SEA,PSIH_SEA,                        &
          XLAND,HFX_SEA,QFX_SEA,LH_SEA,TSK_SEA,FLHC_SEA,FLQC_SEA,   &
          QGH_SEA,QSFC_SEA,U10_SEA,V10_SEA,                         &
          GZ1OZ0_SEA,WSPD_SEA,BR_SEA,ISFFLX,                        &
          EP1,EP2,KARMAN,ITIMESTEP,                     &
          ids,ide, jds,jde, kds,kde,                    &
          ims,ime, jms,jme, kms,kme,                    &
          its,ite, jts,jte, kts,kte                     )



     DO j = JTS , JTE
        DO i = ITS , ITE
           
           
           IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .and. ( XICE(i,j) .LE. 1.0 ) ) THEN
              
              

              BR(i,j)     = ( BR(i,j)     * XICE(i,j) ) + ( (1.0-XICE(i,j)) * BR_SEA(i,j)     )
              
              
              
              
              
              
              GZ1OZ0(i,j) = ( GZ1OZ0(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * GZ1OZ0_SEA(i,j) )
              
              
              PSIM(i,j)   = ( PSIM(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * PSIM_SEA(i,j)   )
              PSIH(i,j)   = ( PSIH(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * PSIH_SEA(i,j)   )
              
              
              
              U10(i,j)    = ( U10(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * U10_SEA(i,j)    )
              V10(i,j)    = ( V10(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * V10_SEA(i,j)    )
              WSPD(i,j)   = ( WSPD(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * WSPD_SEA(i,j)   )
              
              

           ENDIF
        ENDDO
     ENDDO

   END SUBROUTINE sf_gfs_seaice_wrapper




   SUBROUTINE sfclay_seaice_wrapper(U3D,V3D,T3D,QV3D,P3D,dz8w,     &
                     CP,G,ROVCP,R,XLV,PSFC,CHS,CHS2,CQS2,CPM,      &
                     ZNT,UST,PBLH,MAVAIL,ZOL,MOL,REGIME,PSIM,PSIH, &
                     XLAND,HFX,QFX,LH,TSK,FLHC,FLQC,QGH,QSFC,RMOL, &
                     U10,V10,TH2,T2,Q2,                            &
                     GZ1OZ0,WSPD,BR,ISFFLX,DX,                     &
                     SVP1,SVP2,SVP3,SVPT0,EP1,EP2,                 &
                     KARMAN,EOMEG,STBOLT,                          &
                     P1000,                                      &
XICE,SST,TSK_SEA,                                                  &
CHS2_SEA,CHS_SEA,CPM_SEA,CQS2_SEA,FLHC_SEA,FLQC_SEA,               &
HFX_SEA,LH_SEA,QFX_SEA,QGH_SEA,QSFC_SEA,ZNT_SEA,                   &
ITIMESTEP,XICE_THRESHOLD,                                          &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte,                    &
                     ustm,ck,cka,cd,cda,isftcflx                   )
     USE module_sf_sfclay
     implicit none

     INTEGER,  INTENT(IN )   ::        ids,ide, jds,jde, kds,kde,  &
                                       ims,ime, jms,jme, kms,kme,  &
                                       its,ite, jts,jte, kts,kte

     INTEGER,  INTENT(IN )   ::        ISFFLX
     REAL,     INTENT(IN )   ::        SVP1,SVP2,SVP3,SVPT0
     REAL,     INTENT(IN )   ::        EP1,EP2,KARMAN,EOMEG,STBOLT
     REAL,     INTENT(IN )   ::        P1000

     REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
               INTENT(IN   )   ::                           dz8w

     REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
               INTENT(IN   )   ::                           QV3D, &
                                                             P3D, &
                                                             T3D

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(IN   )               ::             MAVAIL, &
                                                            PBLH, &
                                                           XLAND, &
                                                             TSK
     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(OUT  )               ::                U10, &
                                                             V10, &
                                                             TH2, &
                                                              T2, &
                                                              Q2, &
                                                            QSFC
     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)               ::             REGIME, &
                                                             HFX, &
                                                             QFX, &
                                                              LH, &
                                                         MOL,RMOL

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)   ::                 GZ1OZ0,WSPD,BR, &
                                                        PSIM,PSIH

     REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
               INTENT(IN   )   ::                            U3D, &
                                                             V3D

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(IN   )               ::               PSFC

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)   ::                            ZNT, &
                                                             ZOL, &
                                                             UST, &
                                                             CPM, &
                                                            CHS2, &
                                                            CQS2, &
                                                             CHS

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)   ::                      FLHC,FLQC

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)   ::                                 &
                                                              QGH

     REAL,     INTENT(IN   )               ::   CP,G,ROVCP,R,XLV,DX

     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme )              , &
               INTENT(OUT)     ::              ck,cka,cd,cda,ustm

     INTEGER,  OPTIONAL,  INTENT(IN )   ::     ISFTCFLX




     INTEGER,  INTENT(IN)               ::      ITIMESTEP
     REAL,     INTENT(IN)               ::      XICE_THRESHOLD
     REAL,     DIMENSION( ims:ime, jms:jme ),                     &
               INTENT(IN)               ::      XICE
     REAL,     DIMENSION( ims:ime, jms:jme ),                     &
               INTENT(INOUT)            ::      SST
     REAL,     DIMENSION( ims:ime, jms:jme ),                     &
               INTENT(OUT)              ::      TSK_SEA,          &
                                                CHS2_SEA,         &
                                                CHS_SEA,          &
                                                CPM_SEA,          &
                                                CQS2_SEA,         &
                                                FLHC_SEA,         &
                                                FLQC_SEA,         &
                                                HFX_SEA,          &
                                                LH_SEA,           &
                                                QFX_SEA,          &
                                                QGH_SEA,          &
                                                QSFC_SEA,         &
                                                ZNT_SEA




     INTEGER :: I, J
     REAL,     DIMENSION( ims:ime, jms:jme ) :: XLAND_SEA,        &
                                                MAVAIL_sea,       &
                                                TSK_LOCAL,        &
                                                BR_HOLD,          &
                                                CHS2_HOLD,        &
                                                CHS_HOLD,         &
                                                CPM_HOLD,         &
                                                CQS2_HOLD,        &
                                                FLHC_HOLD,        &
                                                FLQC_HOLD,        &
                                                GZ1OZ0_HOLD,      &
                                                HFX_HOLD,         &
                                                LH_HOLD,          &
                                                MOL_HOLD,         &
                                                PSIH_HOLD,        &
                                                PSIM_HOLD,        &
                                                QFX_HOLD,         &
                                                QGH_HOLD,         &
                                                REGIME_HOLD,      &
                                                RMOL_HOLD,        &
                                                UST_HOLD,         &
                                                WSPD_HOLD,        &
                                                ZNT_HOLD,         &
                                                ZOL_HOLD,         &
                                                CD_SEA,           &
                                                CDA_SEA,          &
                                                CK_SEA,           &
                                                CKA_SEA,          &
                                                Q2_SEA,           &
                                                T2_SEA,           &
                                                TH2_SEA,          &
                                                U10_SEA,          &
                                                USTM_SEA,         &
                                                V10_SEA

     REAL,     DIMENSION( ims:ime, jms:jme ) ::                   &
                                                BR_SEA,           &
                                                GZ1OZ0_SEA,       &
                                                MOL_SEA,          &
                                                PSIH_SEA,         &
                                                PSIM_SEA,         &
                                                REGIME_SEA,       &
                                                RMOL_SEA,         &
                                                UST_SEA,          &
                                                WSPD_SEA,         &
                                                ZOL_SEA

      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      

     DO j = JTS , JTE
        DO i = ITS , ITE
           IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .AND. ( XICE(i,j) .LE. 1.0 ) )  THEN

              IF ( SST(i,j) .LT. 271.4 ) THEN
                 SST(i,j) = 271.4
              ENDIF
              IF ( SST(i,j) .GT. 273. .AND. itimestep .le. 3) THEN
                 IF ( XICE(i,j) .GE. 0.6 ) THEN
                    SST(i,j) = 271.4
                 ELSEIF ( XICE(i,j) .GE. 0.4 ) THEN
                    SST(i,j) = 273.
                 ELSEIF (XICE(i,j).GE.0.2 .and. SST(i,j).GT.275.) THEN
                    SST(i,j) = 275.
                 ELSEIF (SST(i,j).GT.278.) THEN
                    SST(i,j) = 278.
                 ENDIF
              ENDIF
              TSK_SEA(i,j) = SST(i,j)

              TSK_LOCAL(i,j) = ( TSK(i,j) - (1.0-XICE(i,j)) *SST(i,j) ) / XICE(i,j)
              IF (XICE(i,j) .lt. 0.2 .and. TSK(i,j) .lt. 253.15) THEN
                 TSK_LOCAL(i,j) = 253.15
              ENDIF
              IF (XICE(i,j) .lt. 0.1 .and. TSK(i,j) .lt. 263.15) THEN
                 TSK_LOCAL(i,j) = 263.15
              ENDIF
           ELSE
              TSK_SEA(i,j) = TSK(i,j)
              TSK_LOCAL(i,j) = TSK(i,j)
           ENDIF
        ENDDO
     ENDDO





     BR_HOLD   = BR
     CHS2_HOLD = CHS2
     CHS_HOLD  = CHS
     CPM_HOLD  = CPM
     CQS2_HOLD = CQS2
     FLHC_HOLD = FLHC
     FLQC_HOLD = FLQC
     GZ1OZ0_HOLD = GZ1OZ0
     HFX_HOLD  = HFX
     LH_HOLD   = LH
     MOL_HOLD  = MOL
     PSIH_HOLD = PSIH
     PSIM_HOLD = PSIM
     QFX_HOLD  = QFX
     QGH_HOLD  = QGH
     REGIME_HOLD = REGIME
     RMOL_HOLD = RMOL
     UST_HOLD  = UST
     WSPD_HOLD = WSPD
     ZNT_HOLD  = ZNT
     ZOL_HOLD  = ZOL



     
     
     
     
     
     
     
     
     
     
     


     
     call sfclay(U3D,V3D,T3D,QV3D,P3D,dz8w,                    & 
                 CP,G,ROVCP,R,XLV,PSFC,CHS,CHS2,CQS2,CPM,      & 
                 ZNT,UST,PBLH,MAVAIL,ZOL,MOL,REGIME,PSIM,PSIH, &
                 XLAND,HFX,QFX,LH,TSK_LOCAL,FLHC,FLQC,QGH,QSFC,RMOL, &
                 U10,V10,TH2,T2,Q2,                            &
                 GZ1OZ0,WSPD,BR,ISFFLX,DX,                     &
                 SVP1,SVP2,SVP3,SVPT0,EP1,EP2,                 &
                 KARMAN,EOMEG,STBOLT,                          &
                 P1000,                                      &
                 ids,ide, jds,jde, kds,kde,                    &
                 ims,ime, jms,jme, kms,kme,                    &
                 its,ite, jts,jte, kts,kte,                    &
                 ustm,ck,cka,cd,cda,isftcflx                   )

     
     DO j = JTS , JTE
        DO i = ITS , ITE
           IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .and. ( XICE(i,j) .LE. 1.0 ) ) THEN
              XLAND_SEA(i,j)=2.
              MAVAIL_SEA(I,J)  =1.
              ZNT_SEA(I,J) = 0.0001
              TSK_SEA(i,j) = SST(i,j)
              IF ( SST(i,j) .LT. 271.4 ) THEN
                 SST(i,j) = 271.4
                 TSK_SEA(i,j) = SST(i,j)
              ENDIF
           ELSE
              XLAND_SEA(i,j) = XLAND(i,j)
              MAVAIL_SEA(i,j) = MAVAIL(i,j)
              ZNT_SEA(i,j)  = ZNT_HOLD(i,j)
              TSK_SEA(i,j) = TSK_LOCAL(i,j)
           ENDIF
        ENDDO
     ENDDO

     
     BR_SEA   = BR_HOLD
     CHS2_SEA = CHS2_HOLD
     CHS_SEA  = CHS_HOLD
     CPM_SEA  = CPM_HOLD
     CQS2_SEA = CQS2_HOLD
     FLHC_SEA = FLHC_HOLD
     FLQC_SEA = FLQC_HOLD
     GZ1OZ0_SEA = GZ1OZ0_HOLD
     HFX_SEA  = HFX_HOLD
     LH_SEA   = LH_HOLD
     MOL_SEA  = MOL_HOLD
     PSIH_SEA = PSIH_HOLD
     PSIM_SEA = PSIM_HOLD
     QFX_SEA  = QFX_HOLD
     QGH_SEA  = QGH_HOLD
     REGIME_SEA = REGIME_HOLD
     RMOL_SEA = RMOL_HOLD
     UST_SEA  = UST_HOLD
     WSPD_SEA = WSPD_HOLD
     ZOL_SEA  = ZOL_HOLD

     
     call sfclay(U3D,V3D,T3D,QV3D,P3D,dz8w,                    & 
                 CP,G,ROVCP,R,XLV,PSFC,                        & 
                 CHS_SEA,CHS2_SEA,CQS2_SEA,CPM_SEA,            & 
                 ZNT_SEA,UST_SEA,                              & 
                 PBLH,MAVAIL_SEA,                              & 
                 ZOL_SEA,MOL_SEA,REGIME_SEA,PSIM_SEA,PSIH_SEA, & 
                 XLAND_SEA,                              & 
                 HFX_SEA,QFX_SEA,LH_SEA,                       & 
                 TSK_SEA,                                      & 
                 FLHC_SEA,FLQC_SEA,QGH_SEA,QSFC_sea,RMOL_SEA,  & 
                 U10_sea,V10_sea,TH2_sea,T2_sea,Q2_sea,        & 
                 GZ1OZ0_SEA,WSPD_SEA,BR_SEA,                   & 
                 ISFFLX,DX,                                    &
                 SVP1,SVP2,SVP3,SVPT0,EP1,EP2,                 &
                 KARMAN,EOMEG,STBOLT,                          &
                 P1000,                                      &
                 ids,ide, jds,jde, kds,kde,                    &
                 ims,ime, jms,jme, kms,kme,                    &
                 its,ite, jts,jte, kts,kte,                    & 
                 ustm_sea,ck_sea,cka_sea,cd_sea,cda_sea,isftcflx   )

     DO j = JTS , JTE
        DO i = ITS, ITE
           IF ( ( XICE(I,J) .GE. XICE_THRESHOLD )  .and.( XICE(i,j) .LE. 1.0 ) ) THEN
              
              br(i,j)     = ( br(i,j)     * XICE(i,j) ) + ( (1.0-XICE(i,j)) * br_sea(i,j)     )
              
              
              
              
              
              
              gz1oz0(i,j) = ( gz1oz0(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * gz1oz0_sea(i,j) )
              
              
              mol(i,j)    = ( mol(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * mol_sea(i,j)    )
              psih(i,j)   = ( psih(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * psih_sea(i,j)   )
              psim(i,j)   = ( psim(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * psim_sea(i,j)   )
              
              
              if ( XICE(i,j).GE. 0.5 ) regime(i,j) = regime_hold(i,j)
              rmol(i,j)   = ( rmol(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * rmol_sea(i,j)   )
              ust(i,j)    = ( ust(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * ust_sea(i,j)    )
              wspd(i,j)   = ( wspd(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * wspd_sea(i,j)   )
              zol(i,j)    = ( zol(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * zol_sea(i,j)    )
              
              IF ( PRESENT ( CD ) ) THEN
                 CD(i,j)     = ( CD(i,j)     * XICE(i,j) ) + ( (1.0-XICE(i,j)) * CD_sea(i,j)     )
              ENDIF
              IF ( PRESENT ( CDA ) ) THEN
                 CDA(i,j)     = ( CDA(i,j)     * XICE(i,j) ) + ( (1.0-XICE(i,j)) * CDA_sea(i,j)     )
              ENDIF
              IF ( PRESENT ( CK ) ) THEN
                 CK(i,j)     = ( CK(i,j)     * XICE(i,j) ) + ( (1.0-XICE(i,j)) * CK_sea(i,j)     )
              ENDIF
              IF ( PRESENT ( CKA ) ) THEN
                 CKA(i,j)     = ( CKA(i,j)     * XICE(i,j) ) + ( (1.0-XICE(i,j)) * CKA_sea(i,j)     )
              ENDIF
              q2(i,j)     = ( q2(i,j)     * XICE(i,j) ) + ( (1.0-XICE(i,j)) * q2_sea(i,j)     )
              
              t2(i,j)     = ( t2(i,j)     * XICE(i,j) ) + ( (1.0-XICE(i,j)) * t2_sea(i,j)     )
              th2(i,j)    = ( th2(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * th2_sea(i,j)    )
              u10(i,j)    = ( u10(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * u10_sea(i,j)    )
              IF ( PRESENT ( USTM ) ) THEN
                 USTM(i,j)    = ( USTM(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * USTM_sea(i,j)    )
              ENDIF
              v10(i,j)    = ( v10(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * v10_sea(i,j)    )
           ENDIF
        END DO
     END DO



   END SUBROUTINE sfclay_seaice_wrapper




   SUBROUTINE pxsfclay_seaice_wrapper(U3D,V3D,T3D,TH3D,QV3D,P3D,dz8w, &
                     CP,G,ROVCP,R,XLV,PSFC,CHS,CHS2,CQS2,CPM,      &
                     ZNT,UST,PBLH,MAVAIL,ZOL,MOL,REGIME,PSIM,PSIH, &
                     XLAND,HFX,QFX,LH,TSK,FLHC,FLQC,QGH,QSFC,RMOL, &
                     U10,V10,                                      &
                     GZ1OZ0,WSPD,BR,ISFFLX,DX,                     &
                     SVP1,SVP2,SVP3,SVPT0,EP1,EP2,KARMAN,          &
XICE, SST, ITIMESTEP, XICE_THRESHOLD,                              &
CHS_SEA, CHS2_SEA, CPM_SEA, CQS2_SEA, FLHC_SEA, FLQC_SEA,          &
HFX_SEA, LH_SEA, QFX_SEA, QGH_SEA, QSFC_SEA, TSK_SEA,  &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte                     )
     USE module_sf_pxsfclay
     implicit none
     INTEGER,  INTENT(IN )   ::        ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       its,ite, jts,jte, kts,kte

     INTEGER,  INTENT(IN )   ::        ISFFLX
     REAL,     INTENT(IN )   ::        SVP1,SVP2,SVP3,SVPT0
     REAL,     INTENT(IN )   ::        EP1,EP2,KARMAN

     REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
               INTENT(IN   )   ::                           dz8w

     REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
               INTENT(IN   )   ::                           QV3D, &
                                                             P3D, &
                                                             T3D, &
                                                            TH3D

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(IN   )               ::             MAVAIL, &
                                                            PBLH, &
                                                           XLAND, &
                                                             TSK
     REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
               INTENT(IN   )   ::                            U3D, &
                                                             V3D

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(IN   )               ::               PSFC

     REAL,     INTENT(IN   )                  ::   CP,G,ROVCP,R,XLV,DX

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(OUT  )               ::                U10, &
                                                             V10, &
                                                            QSFC
     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)               ::             REGIME, &
                                                             HFX, &
                                                             QFX, &
                                                              LH, &
                                                         MOL,RMOL
     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)   ::                 GZ1OZ0,WSPD,BR, &
                                                       PSIM,PSIH

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)   ::                            ZNT, &
                                                             ZOL, &
                                                             UST, &
                                                             CPM, &
                                                            CHS2, &
                                                            CQS2, &
                                                             CHS

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)   ::                      FLHC,FLQC

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)   ::                            QGH





     INTEGER,  INTENT(IN)                           :: ITIMESTEP
     REAL,     INTENT(IN)                           :: XICE_THRESHOLD
     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(IN)                           ::      XICE
     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(OUT)                        ::     TSK_SEA
     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)              ::                 SST




     INTEGER :: I, J
     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(OUT)    ::                         CHS_SEA, &
                                                        CHS2_SEA, &
                                                         CPM_SEA, &
                                                        CQS2_SEA, &
                                                        FLHC_SEA, &
                                                        FLQC_SEA, &
                                                         HFX_SEA, &
                                                          LH_SEA, &
                                                         QFX_SEA, &
                                                         QGH_SEA, &
                                                        QSFC_SEA

     REAL,     DIMENSION( ims:ime, jms:jme ) ::          BR_HOLD, &
                                                        CHS_HOLD, &
                                                       CHS2_HOLD, &
                                                        CPM_HOLD, &
                                                       CQS2_HOLD, &
                                                       FLHC_HOLD, &
                                                       FLQC_HOLD, &
                                                     GZ1OZ0_HOLD, &
                                                        HFX_HOLD, &
                                                         LH_HOLD, &
                                                        MOL_HOLD, &
                                                       PSIH_HOLD, &
                                                       PSIM_HOLD, &
                                                        QFX_HOLD, &
                                                        QGH_HOLD, &
                                                     REGIME_HOLD, &
                                                       RMOL_HOLD, &
                                                        UST_HOLD, &
                                                       WSPD_HOLD, &
                                                        ZNT_HOLD, &
                                                        ZOL_HOLD, &
                                                       TSK_LOCAL

     REAL,     DIMENSION( ims:ime, jms:jme ) ::        XLAND_SEA, &
                                                      MAVAIL_SEA, &
                                                          BR_SEA, &
                                                      GZ1OZ0_SEA, &
                                                         MOL_SEA, &
                                                        PSIH_SEA, &
                                                        PSIM_SEA, &
                                                      REGIME_SEA, &
                                                        RMOL_SEA, &
                                                         UST_SEA, &
                                                        WSPD_SEA, &
                                                         ZNT_SEA, &
                                                         ZOL_SEA, &
                                                         U10_SEA, &
                                                         V10_SEA

     DO j = JTS , JTE
        DO i = ITS , ITE
           IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .AND. ( XICE(i,j) .LE. 1.0 ) )  THEN

              IF ( SST(i,j) .LT. 271.4 ) THEN
                 SST(i,j) = 271.4
              ENDIF
              IF ( SST(i,j) .GT. 273. .AND. itimestep .le. 3) THEN
                 IF ( XICE(i,j) .GE. 0.6 ) THEN
                    SST(i,j) = 271.4
                 ELSEIF ( XICE(i,j) .GE. 0.4 ) THEN
                    SST(i,j) = 273.
                 ELSEIF (XICE(i,j).GE.0.2 .and. SST(i,j).GT.275.) THEN
                    SST(i,j) = 275.
                 ELSEIF (SST(i,j).GT.278.) THEN
                    SST(i,j) = 278.
                 ENDIF
              ENDIF
              TSK_SEA(i,j) = SST(i,j)

              TSK_LOCAL(i,j) = ( TSK(i,j) - (1.0-XICE(i,j)) *SST(i,j) ) / XICE(i,j)
              IF (XICE(i,j) .lt. 0.2 .and. TSK(i,j) .lt. 253.15) THEN
                 TSK_LOCAL(i,j) = 253.15
              ENDIF
              IF (XICE(i,j) .lt. 0.1 .and. TSK(i,j) .lt. 263.15) THEN
                 TSK_LOCAL(i,j) = 263.15
              ENDIF
           ELSE
              TSK_SEA(i,j) = TSK(i,j)
              TSK_LOCAL(i,j) = TSK(i,j)
           ENDIF
        ENDDO
     ENDDO





     BR_HOLD     = BR
     CHS_HOLD    = CHS
     CHS2_HOLD   = CHS2
     CPM_HOLD    = CPM
     CQS2_HOLD   = CQS2
     FLHC_HOLD   = FLHC
     FLQC_HOLD   = FLQC
     GZ1OZ0_HOLD = GZ1OZ0
     HFX_HOLD    = HFX
     LH_HOLD     = LH
     MOL_HOLD    = MOL
     PSIH_HOLD   = PSIH
     PSIM_HOLD   = PSIM
     QFX_HOLD    = QFX
     QGH_HOLD    = QGH
     REGIME_HOLD = REGIME
     RMOL_HOLD   = RMOL
     UST_HOLD    = UST
     WSPD_HOLD   = WSPD
     ZNT_HOLD    = ZNT
     ZOL_HOLD    = ZOL



     
     
     


     CALL pxsfclay(U3D,V3D,T3D,TH3D,QV3D,P3D,dz8w,                 &
                     CP,G,ROVCP,R,XLV,PSFC,CHS,CHS2,CQS2,CPM,      &
                     ZNT,UST,PBLH,MAVAIL,ZOL,MOL,REGIME,PSIM,PSIH, &
                     XLAND,HFX,QFX,LH,TSK_LOCAL,FLHC,FLQC,QGH,QSFC,RMOL, &
                     U10,V10,                                      &
                     GZ1OZ0,WSPD,BR,ISFFLX,DX,                     &
                     SVP1,SVP2,SVP3,SVPT0,EP1,EP2,KARMAN,          &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte                     )

     DO j = JTS , JTE
        DO i= ITS , ITE
           IF( ( XICE(I,J) .GE. XICE_THRESHOLD ) .and. ( XICE(i,j) .LE. 1.0 ) ) THEN
              
              XLAND_SEA(i,j)=2.
              MAVAIL_SEA(I,J)  =1.
              ZNT_SEA(I,J) = 0.0001
              TSK_SEA(i,j)  = SST(i,j)
              if ( SST(i,j) .LT. 271.4 ) then
                 SST(i,j) = 271.4
                 TSK_SEA(i,j) = SST(i,j)
              endif
           ELSE
              XLAND_SEA(i,j)=xland(i,j)
              MAVAIL_SEA(i,j) = mavail(i,j)
              ZNT_SEA(I,J)  = ZNT_HOLD(I,J)
              TSK_SEA(i,j)  = TSK(i,j)
           ENDIF
        ENDDO
     ENDDO

     
     BR_SEA     = BR_HOLD
     CHS_SEA    = CHS_HOLD
     CHS2_SEA   = CHS2_HOLD
     CPM_SEA    = CPM_HOLD
     CQS2_SEA   = CQS2_HOLD
     FLHC_SEA   = FLHC_HOLD
     FLQC_SEA   = FLQC_HOLD
     GZ1OZ0_SEA = GZ1OZ0_HOLD
     HFX_SEA    = HFX_HOLD
     LH_SEA     = LH_HOLD
     MOL_SEA    = MOL_HOLD
     PSIH_SEA   = PSIH_HOLD
     PSIM_SEA   = PSIM_HOLD
     QFX_SEA    = QFX_HOLD
     QGH_SEA    = QGH_HOLD
     REGIME_SEA = REGIME_HOLD
     RMOL_SEA   = RMOL_HOLD
     UST_SEA    = UST_HOLD
     WSPD_SEA   = WSPD_HOLD
     ZOL_SEA    = ZOL_HOLD


     
     
     
     CALL pxsfclay(U3D,V3D,T3D,TH3D,QV3D,P3D,dz8w,                 &
                     CP,G,ROVCP,R,XLV,PSFC,CHS_SEA,CHS2_SEA,CQS2_SEA,CPM_SEA,      &
                     ZNT_SEA,UST_SEA,PBLH,MAVAIL_SEA,ZOL_SEA,MOL_SEA,REGIME_SEA,PSIM_SEA,PSIH_SEA, &
                     XLAND_SEA,HFX_SEA,QFX_SEA,LH_SEA,TSK_SEA,FLHC_SEA,FLQC_SEA,QGH_SEA,QSFC_SEA,RMOL_SEA, &
                     U10_SEA,V10_SEA,                              &
                     GZ1OZ0_SEA,WSPD_SEA,BR_SEA,ISFFLX,DX,         &
                     SVP1,SVP2,SVP3,SVPT0,EP1,EP2,KARMAN,          &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte                     )

     DO j = JTS , JTE
        DO i = ITS , ITE
           IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .and. ( XICE(i,j) .LE. 1.0 ) ) THEN
              
              br(i,j)     = ( br(i,j)     * XICE(i,j) ) + ( (1.0-XICE(i,j)) * br_sea(i,j)     )
              gz1oz0(i,j) = ( gz1oz0(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * gz1oz0_sea(i,j) )
              mol(i,j)    = ( mol(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * mol_sea(i,j)    )
              psih(i,j)   = ( psih(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * psih_sea(i,j)   )
              psim(i,j)   = ( psim(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * psim_sea(i,j)   )
              rmol(i,j)   = ( rmol(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * rmol_sea(i,j)   )
              ust(i,j)    = ( ust(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * ust_sea(i,j)    )
              wspd(i,j)   = ( wspd(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * wspd_sea(i,j)   )
              zol(i,j)    = ( zol(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * zol_sea(i,j)    )
              
              
              
              
              
              
              
              
              
              
              

              
              u10(i,j) = ( u10(i,j)       * XICE(i,j) ) + ( (1.0-XICE(i,j)) * u10_sea(i,j)    )
              v10(i,j) = ( v10(i,j)       * XICE(i,j) ) + ( (1.0-XICE(i,j)) * v10_sea(i,j)    )
              
           ENDIF
        ENDDO
     ENDDO

   END SUBROUTINE pxsfclay_seaice_wrapper




END MODULE module_surface_driver
