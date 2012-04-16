









MODULE module_physics_init

   USE module_state_description
   USE module_model_constants
   USE module_configure
   USE module_dm , ONLY : wrf_dm_maxval

CONTAINS



   SUBROUTINE phy_init ( id, config_flags, DT, restart, zfull, zhalf,     &
                         p_top, TSK,RADT,BLDT,CUDT,MPDT,         &
                         RTHCUTEN, RQVCUTEN, RQRCUTEN,           &
                         RQCCUTEN, RQSCUTEN, RQICUTEN,           &
                         RUBLTEN,RVBLTEN,RTHBLTEN,               &
                         RQVBLTEN,RQCBLTEN,RQIBLTEN,             &
                         RTHRATEN,RTHRATENLW,RTHRATENSW,         &
                         STEPBL,STEPRA,STEPCU,                   &
                         W0AVG, RAINNC, RAINC, RAINCV, RAINNCV,  &
                         NCA,swrad_scat,                         &
                         CLDEFI,LOWLYR,                          &
                         MASS_FLUX,                              &
                         RTHFTEN, RQVFTEN,                       &
                         CLDFRA,CLDFRA_OLD,GLW,GSW,EMISS,EMBCK,  & 
                         LU_INDEX,                               &
                         landuse_ISICE, landuse_LUCATS,          &
                         landuse_LUSEAS, landuse_ISN,            &
                         lu_state,                               &
                         XLAT,XLONG,ALBEDO,ALBBCK,GMT,JULYR,JULDAY,&
                         levsiz, n_ozmixm, n_aerosolc, paerlev,  &
                         TMN,XLAND,ZNT,Z0,UST,MOL,PBLH,TKE_MYJ,  &
                         EXCH_H,THC,SNOWC,MAVAIL,HFX,QFX,RAINBL, &
                         TSLB,ZS,DZS,num_soil_layers,warm_rain,  &
                         adv_moist_cond,                         &
                         APR_GR,APR_W,APR_MC,APR_ST,APR_AS,      &
                         APR_CAPMA,APR_CAPME,APR_CAPMI,          &
                         XICE,XICEM,VEGFRA,SNOW,CANWAT,SMSTAV,   &
                         SMSTOT, SFCRUNOFF,UDRUNOFF,GRDFLX,ACSNOW,&
                         ACSNOM,IVGTYP,ISLTYP, SFCEVP, SMOIS,    &
                         SH2O, SNOWH, SMFR3D,                    &  
                         SNOALB,                                 &
                         DX,DY,F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY, &
                         mp_restart_state,tbpvs_state,tbpvs0_state,&
                         allowed_to_read, moved, start_of_simulation,&
                         LAGDAY,                                 &
                         ids, ide, jds, jde, kds, kde,           &
                         ims, ime, jms, jme, kms, kme,           &
                         its, ite, jts, jte, kts, kte,           &
                         NUM_URBAN_LAYERS,                       &
                         ozmixm,pin,                             &    
                         m_ps_1,m_ps_2,m_hybi,aerosolc_1,aerosolc_2,& 
                         RUNDGDTEN,RVNDGDTEN,RTHNDGDTEN,         &    
                         RPHNDGDTEN,RQVNDGDTEN,RMUNDGDTEN,       &    
                         FGDT,STEPFG,                            &    
                         cugd_tten,cugd_ttens,cugd_qvten,        &    
                         cugd_qvtens,cugd_qcten,                 &    


                         DZR, DZB, DZG,                          & 
                         TR_URB2D,TB_URB2D,TG_URB2D,TC_URB2D,    & 
                         QC_URB2D, XXXR_URB2D,XXXB_URB2D,        & 
                         XXXG_URB2D, XXXC_URB2D,                 & 
                         TRL_URB3D, TBL_URB3D, TGL_URB3D,        & 
                         SH_URB2D, LH_URB2D, G_URB2D, RN_URB2D,  & 
                         TS_URB2D, FRC_URB2D, UTYPE_URB2D,       & 
                         TRB_URB4D,TW1_URB4D,TW2_URB4D,          & 
                         TGB_URB4D,SFW1_URB3D,SFW2_URB3D,        & 
                         SFR_URB3D,SFG_URB3D,                    & 
                         A_U_BEP,A_V_BEP,A_T_BEP,A_Q_BEP,        & 
                         A_E_BEP,B_U_BEP,B_V_BEP,                & 
                         B_T_BEP,B_Q_BEP,B_E_BEP,DLG_BEP,        & 
                         DL_U_BEP,SF_BEP,VL_BEP,                 & 
                         TML,T0ML,HML,H0ML,HUML,HVML,            & 
                         itimestep,                              & 
                         TYR,TYRA,TDLY,TLAG,NYEAR,NDAY,tmn_update,   &
                         ACHFX,ACLHF,ACGRDFLX                   &
                         )


   USE module_domain
   USE module_wrf_error
   IMPLICIT NONE

   TYPE (grid_config_rec_type)              :: config_flags

   INTEGER , INTENT(IN)        :: id
   INTEGER , INTENT(IN) ,OPTIONAL       :: tmn_update
   LOGICAL , INTENT(OUT)       :: warm_rain,adv_moist_cond

   LOGICAL, PARAMETER          :: FNDSOILW=.true., FNDSNOWH=.true.
   INTEGER , INTENT(IN)        :: ids, ide, jds, jde, kds, kde,  &
                                  ims, ime, jms, jme, kms, kme,  &
                                  its, ite, jts, jte, kts, kte

   INTEGER , INTENT(IN)        :: num_soil_layers
   INTEGER , INTENT(IN)        :: lagday
   INTEGER , INTENT(OUT) ,OPTIONAL      :: nyear, nday

   LOGICAL,  INTENT(IN)        :: start_of_simulation
   REAL,     INTENT(IN)        :: DT, p_top, DX, DY
   LOGICAL,  INTENT(IN)        :: restart
   REAL,     INTENT(IN)        :: RADT,BLDT,CUDT,MPDT
   REAL,     INTENT(IN)        :: swrad_scat

   REAL,     DIMENSION( kms:kme ) , INTENT(IN) :: zfull, zhalf
   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(IN) :: TSK, XLAT, XLONG

   INTEGER,      INTENT(IN   )    ::   levsiz, n_ozmixm
   INTEGER,      INTENT(IN   )    ::   paerlev, n_aerosolc

   REAL,  DIMENSION( ims:ime, levsiz, jms:jme, n_ozmixm ), OPTIONAL, &
          INTENT(INOUT) ::                                  OZMIXM

   REAL,  DIMENSION(levsiz), OPTIONAL, INTENT(INOUT)  ::        PIN

   REAL,  DIMENSION(ims:ime,jms:jme), OPTIONAL, INTENT(INOUT)  :: m_ps_1,m_ps_2
   REAL,  DIMENSION(paerlev), OPTIONAL,INTENT(INOUT)  ::          m_hybi
   REAL,  DIMENSION( ims:ime, paerlev, jms:jme, n_aerosolc ), OPTIONAL, &
          INTENT(INOUT) ::                    aerosolc_1, aerosolc_2

   REAL,     DIMENSION( ims:ime , 1:num_soil_layers , jms:jme ),&
                 INTENT(INOUT) :: SMOIS, SH2O,TSLB
   REAL,     DIMENSION( ims:ime , 1:num_soil_layers , jms:jme ), INTENT(OUT) :: SMFR3D

   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(INOUT)    ::                           SNOW, &
                                                         SNOWC, &
                                                         SNOWH, &
                                                        CANWAT, &
                                                        SMSTAV, &
                                                        SMSTOT, &
                                                     SFCRUNOFF, &
                                                   UDRUNOFF, &
                                                        SFCEVP, &
                                                        GRDFLX, &
                                                        ACSNOW, &
                                                          XICE, &
                                                         XICEM, &
                                                        VEGFRA, &
                                                        ACSNOM

   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            OPTIONAL, INTENT(INOUT)    ::                ACHFX, &
                                                         ACLHF, &
                                                      ACGRDFLX

   INTEGER, DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(INOUT)    ::                         IVGTYP, &
                                                        ISLTYP



   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::    &
             RTHRATEN, RTHRATENLW, RTHRATENSW, CLDFRA

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , OPTIONAL, INTENT(OUT) :: &
             CLDFRA_OLD

   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT) ::         &
             GSW,ALBEDO,ALBBCK,GLW,EMISS,EMBCK                          
   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT) ::   SNOALB


   REAL,     INTENT(IN) :: GMT

   INTEGER , INTENT(OUT) :: STEPRA, STEPBL, STEPCU
   INTEGER , INTENT(IN) :: JULYR, JULDAY



   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::    &
             RTHCUTEN, RQVCUTEN, RQRCUTEN, RQCCUTEN, RQSCUTEN,   &
             RQICUTEN

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) :: W0AVG

   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(OUT) :: MASS_FLUX,   &
                      APR_GR,APR_W,APR_MC,APR_ST,APR_AS,          &
                      APR_CAPMA,APR_CAPME,APR_CAPMI

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::    &
             RTHFTEN, RQVFTEN

   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(OUT) ::           &
             RAINNC, RAINC, RAINCV, RAINNCV

   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(OUT) :: CLDEFI, NCA

   INTEGER,  DIMENSION( ims:ime , jms:jme ) , INTENT(OUT) :: LOWLYR



   


   REAL,     DIMENSION(1:num_soil_layers),      INTENT(INOUT) :: ZS,DZS

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::    &
             RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,RQCBLTEN,RQIBLTEN,EXCH_H,TKE_MYJ
   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , OPTIONAL, INTENT(OUT) ::    &
             cugd_tten,cugd_ttens,cugd_qvten,                &
             cugd_qvtens,cugd_qcten
   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT) ::         &
             XLAND,ZNT,Z0,UST,MOL,LU_INDEX,                         &
             PBLH,THC,MAVAIL,HFX,QFX,RAINBL
   INTEGER , INTENT(INOUT)  :: landuse_ISICE, landuse_LUCATS
   INTEGER , INTENT(INOUT)  :: landuse_LUSEAS, landuse_ISN
   REAL    , INTENT(INOUT)  , DIMENSION( : ) :: lu_state

   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT) :: TMN
   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT),OPTIONAL :: TYR
   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT),OPTIONAL :: TYRA
   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT),OPTIONAL :: TDLY
   REAL,     DIMENSION( ims:ime , 1:lagday , jms:jme ) , INTENT(INOUT),OPTIONAL :: TLAG


   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::   &
             F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY
   REAL, DIMENSION(:), INTENT(INOUT)   :: mp_restart_state,tbpvs_state,tbpvs0_state
   LOGICAL,  INTENT(IN)  :: allowed_to_read, moved


   REAL,     DIMENSION( ims:ime , jms:jme ) , OPTIONAL, INTENT(INOUT) ::    &
             TML,T0ML,HML,H0ML,HUML,HVML


   REAL,     OPTIONAL, INTENT(IN) :: FGDT
   INTEGER , OPTIONAL, INTENT(OUT) :: STEPFG
   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , OPTIONAL, INTENT(OUT) ::    &
             RUNDGDTEN, RVNDGDTEN, RTHNDGDTEN, RPHNDGDTEN, RQVNDGDTEN
   REAL,     DIMENSION( ims:ime , jms:jme ) , OPTIONAL, INTENT(OUT) ::    &
             RMUNDGDTEN





   REAL, OPTIONAL, DIMENSION(1:num_soil_layers), INTENT(INOUT) :: DZR    
   REAL, OPTIONAL, DIMENSION(1:num_soil_layers), INTENT(INOUT) :: DZB    
   REAL, OPTIONAL, DIMENSION(1:num_soil_layers), INTENT(INOUT) :: DZG    

   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TR_URB2D 
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TB_URB2D 
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TG_URB2D 
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TC_URB2D 
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: QC_URB2D 
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXR_URB2D 
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXB_URB2D 
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXG_URB2D 
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXC_URB2D 




   REAL, OPTIONAL, DIMENSION(ims:ime, 1:num_soil_layers, jms:jme), INTENT(INOUT) :: TRL_URB3D  
   REAL, OPTIONAL, DIMENSION(ims:ime, 1:num_soil_layers, jms:jme), INTENT(INOUT) :: TBL_URB3D  
   REAL, OPTIONAL, DIMENSION(ims:ime, 1:num_soil_layers, jms:jme), INTENT(INOUT) :: TGL_URB3D  

   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: SH_URB2D 
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: LH_URB2D 
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: G_URB2D 
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: RN_URB2D 
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TS_URB2D 
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: FRC_URB2D 
   INTEGER, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: UTYPE_URB2D 

   INTEGER , INTENT(IN)        :: num_urban_layers
   REAL, OPTIONAL, DIMENSION(ims:ime, 1:num_urban_layers, jms:jme), INTENT(INOUT) :: TRB_URB4D 
   REAL, OPTIONAL, DIMENSION(ims:ime, 1:num_urban_layers, jms:jme), INTENT(INOUT) :: TW1_URB4D 
   REAL, OPTIONAL, DIMENSION(ims:ime, 1:num_urban_layers, jms:jme), INTENT(INOUT) :: TW2_URB4D 
   REAL, OPTIONAL, DIMENSION(ims:ime, 1:num_urban_layers, jms:jme), INTENT(INOUT) :: TGB_URB4D 
   REAL, OPTIONAL, DIMENSION(ims:ime, 1:num_urban_layers, jms:jme), INTENT(INOUT) :: SFG_URB3D 
   REAL, OPTIONAL, DIMENSION(ims:ime, 1:num_urban_layers, jms:jme), INTENT(INOUT) :: SFR_URB3D 
   REAL, OPTIONAL, DIMENSION(ims:ime, 1:num_urban_layers, jms:jme), INTENT(INOUT) :: SFW1_URB3D 
   REAL, OPTIONAL, DIMENSION(ims:ime, 1:num_urban_layers, jms:jme), INTENT(INOUT) :: SFW2_URB3D 
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: A_U_BEP
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: A_V_BEP
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: A_T_BEP
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: A_Q_BEP
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: A_E_BEP
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: B_U_BEP
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: B_V_BEP
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: B_T_BEP
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: B_Q_BEP
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: B_E_BEP
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: VL_BEP
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: DLG_BEP
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme,jms:jme), INTENT(INOUT) :: SF_BEP
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: DL_U_BEP


   INTEGER, OPTIONAL, INTENT(IN) :: itimestep



   REAL    :: ALBLND,ZZLND,ZZWTR,THINLD,XMAVA,CEN_LAT,pptop
   REAL,     DIMENSION( kms:kme )  :: sfull, shalf
   REAL :: obs_twindo_cg, obs_twindo

   CHARACTER*256 :: MMINLU_loc
   CHARACTER*80 :: message
   INTEGER :: ISWATER
   INTEGER :: ISURBAN
   INTEGER :: sf_urban_physics
   INTEGER :: omlcall
   REAL    :: oml_hml0
   LOGICAL :: usemonalb
   LOGICAL :: rdmaxalb

   INTEGER :: i, j, k, itf, jtf, n
integer myproc



   sf_urban_physics=config_flags%sf_urban_physics
   usemonalb=config_flags%usemonalb
   rdmaxalb=config_flags%rdmaxalb



   sfull = 0.
   shalf = 0.

   CALL wrf_debug(100,'top of phy_init')

   WRITE(wrf_err_message,*) 'phy_init:  start_of_simulation = ',start_of_simulation
   CALL wrf_debug ( 100, TRIM(wrf_err_message) )

   itf=min0(ite,ide-1)
   jtf=min0(jte,jde-1)

   ZZLND=0.1
   ZZWTR=0.0001
   THINLD=0.04
   ALBLND=0.2
   XMAVA=0.3

   if (.not.usemonalb) CALL wrf_error_fatal3("",349,&
'usemonalb should always be true for NMM')

   CALL nl_get_cen_lat(id,cen_lat)
   CALL wrf_debug(100,'calling nl_get_iswater, nl_get_mminlu_loc')
   CALL nl_get_iswater(id,iswater)
   CALL nl_get_isurban(id,isurban)
	write(0,*) 'mminlu_loc before get: ', mminlu_loc
   CALL nl_get_mminlu( 1, mminlu_loc )
	write(0,*) 'post nl_get_mminlu, mminlu_loc is: ', mminlu_loc
   CALL wrf_debug(100,'after nl_get_iswater, nl_get_mminlu_loc')
        write(0,*) 'mminlu_loc is: ', mminlu_loc

        mminlu_loc='USGS'



  IF(.not.restart)THEN


   IF ( .NOT. moved ) THEN
   DO j=jts,jtf
   DO i=its,itf
      XLAND(i,j)=1.
      GSW(i,j)=0.
      GLW(i,j)=0.
      UST(i,j)=0.
      MOL(i,j)=0.0
      PBLH(i,j)=0.0
      HFX(i,j)=0.
      QFX(i,j)=0.
      RAINBL(i,j)=0.
      RAINNCV(i,j)=0.
      ACSNOW(i,j)=0.
      DO k=kms,kme  
         EXCH_H(i,k,j) = 0.
      END DO
   ENDDO
   ENDDO
   ENDIF

   IF(PRESENT(TMN_UPDATE))THEN
   if(tmn_update.eq.1) then
   nyear=1
   nday=1
   DO j=jts,jtf
   DO i=its,itf
      TYR(i,j)=TMN(i,j)
      TYRA(i,j)=TMN(i,j)
      TDLY(i,j)=TMN(i,j)
    DO n=1,lagday
      TLAG(i,n,j)=TMN(i,j)
    ENDDO
   ENDDO
   ENDDO
   endif
   ENDIF



   DO j=jts,jtf
   DO i=its,itf
     IF(XLAND(i,j) .LT. 1.5)THEN
       IF(mminlu_loc .EQ. '    ') ALBBCK(i,j)=ALBLND
       EMBCK(i,j)=0.85
       ALBEDO(i,j)=ALBBCK(i,j)
       EMISS(i,j)=EMBCK(i,j)
       THC(i,j)=THINLD
       ZNT(i,j)=ZZLND
       MAVAIL(i,j)=XMAVA
     ELSE
       IF(mminlu_loc .EQ. '    ') ALBBCK(i,j)=0.08
       ALBEDO(i,j)=ALBBCK(i,j)
       EMBCK(i,j)=0.98
       EMISS(i,j)=EMBCK(i,j)
       THC(i,j)=THINLD
       ZNT(i,j)=ZZWTR
       MAVAIL(i,j)=1.0
     ENDIF

   ENDDO
   ENDDO

   CALL wrf_debug ( 200 , 'module_start: phy_init: Before call to landuse_init' )

   IF(mminlu_loc .ne. '    ')THEN


	write(0,*) 'mminlu_loc into landuse_init: ', mminlu_loc
	write(0,*) 'trim(mminlu_loc) into landuse_init: ', trim(mminlu_loc)

     CALL landuse_init(lu_index, snowc, albedo, albbck, mavail, emiss, embck,            &
                znt, Z0, thc, xland, xice, xicem, julday, cen_lat, iswater, &
                TRIM ( mminlu_loc ) ,                               &
                landuse_ISICE, landuse_LUCATS,                      &
                landuse_LUSEAS, landuse_ISN,                        &
                lu_state,                                           &
                allowed_to_read , usemonalb ,                       &
                ids, ide, jds, jde, kds, kde,                       &
                ims, ime, jms, jme, kms, kme,                       &
                its, ite, jts, jte, kts, kte                       )
   ENDIF

  ENDIF




   CALL z2sigma(zfull,zhalf,sfull,shalf,p_top,pptop,config_flags, &
                allowed_to_read,                                  &
                kds,kde,kms,kme,kts,kte)







   CALL wrf_debug ( 200 , 'module_start: phy_init: Before call to ra_init' )

   CALL ra_init(id,STEPRA,RADT,DT,RTHRATEN,RTHRATENLW,             &
                RTHRATENSW,CLDFRA,EMISS,cen_lat,JULYR,JULDAY,GMT,    &
                levsiz,XLAT,n_ozmixm,                           &
                cldfra_old,                                     & 
                ozmixm,pin,                                     & 
                m_ps_1,m_ps_2,m_hybi,aerosolc_1,aerosolc_2,     & 
                paerlev,n_aerosolc,                             &
                sfull,shalf,pptop,swrad_scat,                   &
                config_flags,restart,                           &
                allowed_to_read, start_of_simulation,           &
                ids, ide, jds, jde, kds, kde,                   &
                ims, ime, jms, jme, kms, kme,                   &
                its, ite, jts, jte, kts, kte                    )

   CALL wrf_debug ( 200 , 'module_start: phy_init: Before call to bl_init' )
   CALL bl_init(STEPBL,BLDT,DT,RUBLTEN,RVBLTEN,RTHBLTEN,        &
                RQVBLTEN,RQCBLTEN,RQIBLTEN,TSK,TMN,             &
                config_flags,restart,UST,LOWLYR,TSLB,ZS,DZS,    &
                num_soil_layers,TKE_MYJ,                        &
                EXCH_H,VEGFRA,                                  &
                SNOW,SNOWC, CANWAT,SMSTAV,                      &
                SMSTOT, SFCRUNOFF,UDRUNOFF,ACSNOW,ACSNOM,       &
                IVGTYP,ISLTYP,ISURBAN,SMOIS,SMFR3D,MAVAIL,      &
                SNOWH,SH2O,SNOALB,FNDSOILW,FNDSNOWH,RDMAXALB,   &
                Z0,XLAND,XICE,                                  &
                SFCEVP,GRDFLX,                                  &
                TRIM (MMINLU_LOC),                              &
                allowed_to_read ,                               &
                start_of_simulation ,                           &
                DZR, DZB, DZG,                                  & 
                TR_URB2D,TB_URB2D,TG_URB2D,TC_URB2D,QC_URB2D,   & 
                XXXR_URB2D,XXXB_URB2D,XXXG_URB2D,XXXC_URB2D,    & 
                TRL_URB3D, TBL_URB3D, TGL_URB3D,                & 
                SH_URB2D, LH_URB2D, G_URB2D, RN_URB2D,          & 
                TS_URB2D, FRC_URB2D, UTYPE_URB2D,               & 
                SF_URBAN_PHYSICS,                               & 
                NUM_URBAN_LAYERS,                               & 
                TRB_URB4D,TW1_URB4D,TW2_URB4D,                  & 
                TGB_URB4D,SFW1_URB3D,SFW2_URB3D,                & 
                SFR_URB3D,SFG_URB3D,                            & 
                A_U_BEP,A_V_BEP,A_T_BEP,A_Q_BEP,                & 
                A_E_BEP,B_U_BEP,B_V_BEP,                        & 
                B_T_BEP,B_Q_BEP,B_E_BEP,DLG_BEP,                & 
                DL_U_BEP,SF_BEP,VL_BEP,                         & 
                ids, ide, jds, jde, kds, kde,                   &
                ims, ime, jms, jme, kms, kme,                   &
                its, ite, jts, jte, kts, kte,                   &
                ACHFX,ACLHF,ACGRDFLX,                           &
                oml_hml0, omlcall,                              & 
                TML,T0ML,HML,H0ML,HUML,HVML                     ) 

   CALL wrf_debug ( 200 , 'module_start: phy_init: Before call to cu_init' )

   CALL cu_init(STEPCU,CUDT,DT,RTHCUTEN,RQVCUTEN,RQRCUTEN,      &
                RQCCUTEN,RQSCUTEN,RQICUTEN,NCA,RAINC,           &
                RAINCV,W0AVG,config_flags,restart,              &
                CLDEFI,LOWLYR,MASS_FLUX,                        &
                RTHFTEN, RQVFTEN,                               &
                APR_GR,APR_W,APR_MC,APR_ST,APR_AS,              &
                APR_CAPMA,APR_CAPME,APR_CAPMI,                  &
                cugd_tten,cugd_ttens,cugd_qvten,                &
                cugd_qvtens,cugd_qcten,                         &
                allowed_to_read, start_of_simulation,           &
                ids, ide, jds, jde, kds, kde,                   &
                ims, ime, jms, jme, kms, kme,                   &
                its, ite, jts, jte, kts, kte                    )

   CALL wrf_debug ( 200 , 'module_start: phy_init: Before call to mp_init' )

   CALL mp_init(RAINNC,config_flags,restart,warm_rain,          &
                adv_moist_cond,                                 &
                MPDT, DT, DX, DY, LOWLYR,                       &
                F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY,               &
                mp_restart_state,tbpvs_state,tbpvs0_state,      &
                allowed_to_read, start_of_simulation,           &
                ids, ide, jds, jde, kds, kde,                   &
                ims, ime, jms, jme, kms, kme,                   &
                its, ite, jts, jte, kts, kte                    )


   END SUBROUTINE phy_init


   SUBROUTINE landuse_init(lu_index, snowc, albedo, albbck, mavail, emiss, embck, &
                znt,Z0,thc,xland, xice, xicem, julday, cen_lat, iswater, mminlu,  &
                ISICE, LUCATS, LUSEAS, ISN,                         &
                lu_state,                                           &
                allowed_to_read , usemonalb ,                       &
                ids, ide, jds, jde, kds, kde,                       &
                ims, ime, jms, jme, kms, kme,                       &
                its, ite, jts, jte, kts, kte                       )

   USE module_wrf_error
   IMPLICIT NONE


   INTEGER , INTENT(IN)           :: ids, ide, jds, jde, kds, kde,   &
                                     ims, ime, jms, jme, kms, kme,   &
                                     its, ite, jts, jte, kts, kte

   INTEGER , INTENT(IN)           :: iswater, julday
   REAL    , INTENT(IN)           :: cen_lat
   CHARACTER(LEN=*), INTENT(IN)        :: mminlu
   LOGICAL,  INTENT(IN)           :: allowed_to_read , usemonalb
   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(IN   ) :: lu_index, snowc, xice
   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(OUT  ) :: albedo, albbck, mavail, emiss, &
                                                               embck,                         &
                                                               znt, Z0, thc, xland, xicem
   INTEGER , INTENT(INOUT)  :: ISICE, LUCATS, LUSEAS, ISN
   REAL    , INTENT(INOUT)  , DIMENSION( : ) :: lu_state



   CHARACTER*256 LUTYPE
   CHARACTER*80 :: message
   INTEGER  :: landuse_unit, LS, LC, LI, LUN, NSN
   INTEGER  :: i, j, itf, jtf, is, cats, seas, curs
   INTEGER , PARAMETER :: OPEN_OK = 0
   INTEGER :: ierr
   INTEGER , PARAMETER :: max_cats = 100 , max_seas = 12
   REAL    , DIMENSION( max_cats, max_seas ) :: ALBD, SLMO, SFEM, SFZ0, THERIN, SFHC
   REAL    , DIMENSION( max_cats )     :: SCFX




   LOGICAL :: found_lu, end_of_file
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor



   CALL wrf_debug( 100 , 'top of landuse_init' )

   NSN=-1  


   IF ( 6*(max_cats*max_seas)+1*max_cats .GT. 7501 ) THEN
      WRITE(message,*)'landuse_init: lu_state overflow. Make Registry dimspec p > ',6*(max_cats*max_seas)+1*max_cats
   ENDIF
   curs = 1
   DO cats = 1, max_cats
     SCFX(cats) =           lu_state(curs)         ; curs = curs + 1
     DO seas = 1, max_seas
       ALBD(cats,seas) =    lu_state(curs)         ; curs = curs + 1
       SLMO(cats,seas) =    lu_state(curs)         ; curs = curs + 1
       SFEM(cats,seas) =    lu_state(curs)         ; curs = curs + 1
       SFZ0(cats,seas) =    lu_state(curs)         ; curs = curs + 1
       SFHC(cats,seas) =    lu_state(curs)         ; curs = curs + 1
       THERIN(cats,seas) =  lu_state(curs)         ; curs = curs + 1
     ENDDO
   ENDDO


   ISN=1
   IF(JULDAY.LT.105.OR.JULDAY.GT.288)ISN=2
   IF(CEN_LAT.LT.0.0)ISN=3-ISN

   FOUND_LU = .TRUE.
   IF ( allowed_to_read ) THEN
      landuse_unit = 29
      IF ( wrf_dm_on_monitor() ) THEN
        OPEN(landuse_unit, FILE='LANDUSE.TBL',FORM='FORMATTED',STATUS='OLD',IOSTAT=ierr)
        IF ( ierr .NE. OPEN_OK ) THEN
          WRITE(message,FMT='(A)') &
          'module_physics_init.F: LANDUSE_INIT: open failure for LANDUSE.TBL'
          CALL wrf_error_fatal3("",634,&
message )
        END IF
      ENDIF


      IF(MMINLU.EQ.'OLD ')THEN

        ISICE=11
      ELSE IF(MMINLU(1:4).EQ.'USGS')THEN

        ISICE=24
      ELSE IF(MMINLU.EQ.'SiB ')THEN

        ISICE=16
      ELSE IF(MMINLU.EQ.'LW12')THEN

        ISICE=3
      ELSE IF (MMINLU .EQ. 'MODIFIED_IGBP_MODIS_NOAH') THEN
        ISICE = 15
      ELSE
         call wrf_error_fatal3("",655,&
"INPUT LandUse not found: "//TRIM(MMINLU))
      ENDIF
      call wrf_message ( 'INPUT LandUse = "' // TRIM(MMINLU) // '"' )
      FOUND_LU = .FALSE.
      end_of_file = .FALSE.

 1999 CONTINUE
      IF ( wrf_dm_on_monitor() ) THEN
        READ (landuse_unit,*,END=2002)LUTYPE
        GOTO 2003
 2002   CONTINUE
        CALL wrf_message( 'INPUT FILE FOR LANDUSE REACHED END OF FILE' )
        end_of_file = .TRUE.
 2003   CONTINUE
        IF ( .NOT. end_of_file ) READ (landuse_unit,*)LUCATS,LUSEAS
        FOUND_LU = LUTYPE.EQ.MMINLU
      ENDIF
      CALL wrf_dm_bcast_bytes (end_of_file, 4 )
      IF ( .NOT. end_of_file ) THEN
        CALL wrf_dm_bcast_string(lutype, 256)
        CALL wrf_dm_bcast_bytes (lucats,  4 )
        CALL wrf_dm_bcast_bytes (luseas,  4 )
        CALL wrf_dm_bcast_bytes (found_lu,  4 )
        IF(FOUND_LU)THEN
          LUN=LUCATS
          NSN=LUSEAS
            PRINT *, 'LANDUSE TYPE = "' // TRIM (LUTYPE) // '" FOUND',        &
                   LUCATS,' CATEGORIES',LUSEAS,' SEASONS',     &
                   ' WATER CATEGORY = ',ISWATER,               &
                   ' SNOW CATEGORY = ',ISICE
        ENDIF
        DO ls=1,luseas
          if ( wrf_dm_on_monitor() ) then
            READ (landuse_unit,*)
          endif
          DO LC=1,LUCATS
            IF(found_lu)THEN
              IF ( wrf_dm_on_monitor() ) THEN
                READ (landuse_unit,*)LI,ALBD(LC,LS),SLMO(LC,LS),SFEM(LC,LS),        &
                           SFZ0(LC,LS),THERIN(LC,LS),SCFX(LC),SFHC(LC,LS)
              ENDIF
              CALL wrf_dm_bcast_bytes (LI,  4 )
              IF(LC.NE.LI)CALL wrf_error_fatal3("",698,&
'module_start: MISSING LANDUSE UNIT ' )
            ELSE
              IF ( wrf_dm_on_monitor() ) THEN
                READ (landuse_unit,*)
              ENDIF
            ENDIF
          ENDDO
        ENDDO
        IF(NSN.EQ.1.AND.FOUND_LU) THEN
           ISN = 1
        END IF
        CALL wrf_dm_bcast_bytes (albd,   max_cats * max_seas * 4 )
        CALL wrf_dm_bcast_bytes (slmo,   max_cats * max_seas * 4 )
        CALL wrf_dm_bcast_bytes (sfem,   max_cats * max_seas * 4 )
        CALL wrf_dm_bcast_bytes (sfz0,   max_cats * max_seas * 4 )
        CALL wrf_dm_bcast_bytes (therin, max_cats * max_seas * 4 )
        CALL wrf_dm_bcast_bytes (sfhc,   max_cats * max_seas * 4 )
        CALL wrf_dm_bcast_bytes (scfx,   max_cats *            4 )
      ENDIF

      IF(.NOT. found_lu .AND. .NOT. end_of_file ) GOTO 1999


      IF(.NOT. found_lu .OR. end_of_file )THEN
        CALL wrf_message ( 'LANDUSE IN INPUT FILE DOES NOT MATCH LUTABLE: TABLE NOT USED' )
      ENDIF
    ENDIF  

    IF(FOUND_LU)THEN

      itf = min0(ite, ide-1)
      jtf = min0(jte, jde-1)
      IF(usemonalb)CALL wrf_message ( 'Climatological albedo is used instead of table values' )
      DO j = jts, jtf
        DO i = its, itf
          IS=nint(lu_index(i,j))
          
          IF(IS.LT.0.OR.IS.GT.LUN.AND.allowed_to_read)THEN
            WRITE ( wrf_err_message , * ) 'ERROR: LANDUSE OUTSIDE RANGE =',IS,' AT ',I,J,' LUN= ',LUN
            CALL wrf_error_fatal3("",738,&
TRIM ( wrf_err_message ) )
          ENDIF

          IF(IS.EQ.0)THEN
            IS=ISWATER
          ENDIF
          IF(.NOT.usemonalb)ALBBCK(I,J)=ALBD(IS,ISN)/100.
          ALBEDO(I,J)=ALBBCK(I,J)
          IF(SNOWC(I,J) .GT. 0.5)ALBEDO(I,J)=ALBBCK(I,J)*(1.+SCFX(IS))
          THC(I,J)=THERIN(IS,ISN)/100.
          Z0(I,J)=SFZ0(IS,ISN)/100.
          ZNT(I,J)=Z0(I,J)
          EMBCK(I,J)=SFEM(IS,ISN)
          EMISS(I,J)=EMBCK(I,J)
          MAVAIL(I,J)=SLMO(IS,ISN)
          IF(IS.NE.ISWATER)THEN
            XLAND(I,J)=1.0
          ELSE
            XLAND(I,J)=2.0
          ENDIF

          XICEM(I,J)=XICE(I,J)
          IF(XICE(I,J).GT.0.5)THEN
            XLAND(I,J)=1.0
            ALBBCK(I,J)=ALBD(ISICE,ISN)/100.
            ALBEDO(I,J)=ALBBCK(I,J)
            THC(I,J)=THERIN(ISICE,ISN)/100.
            Z0(I,J)=SFZ0(ISICE,ISN)/100.
            ZNT(I,J)=Z0(I,J)
            EMBCK(I,J)=SFEM(ISICE,ISN)
            EMISS(I,J)=EMBCK(I,J)
            MAVAIL(I,J)=SLMO(ISICE,ISN)
          ENDIF
        ENDDO
      ENDDO
    ENDIF
    if ( wrf_dm_on_monitor() .and. allowed_to_read ) then
      CLOSE (landuse_unit)
    endif
    CALL wrf_debug( 100 , 'returning from of landuse_init' )


    curs = 1
    DO cats = 1, max_cats
      lu_state(curs) = SCFX(cats)                 ; curs = curs + 1
      DO seas = 1, max_seas
        lu_state(curs) = ALBD(cats,seas)          ; curs = curs + 1
        lu_state(curs) = SLMO(cats,seas)          ; curs = curs + 1
        lu_state(curs) = SFEM(cats,seas)          ; curs = curs + 1
        lu_state(curs) = SFZ0(cats,seas)          ; curs = curs + 1
        lu_state(curs) = SFHC(cats,seas)          ; curs = curs + 1
        lu_state(curs) = THERIN(cats,seas)        ; curs = curs + 1
      ENDDO
    ENDDO


   END SUBROUTINE landuse_init


   SUBROUTINE ra_init(id,STEPRA,RADT,DT,RTHRATEN,RTHRATENLW,       &
                      RTHRATENSW,CLDFRA,EMISS,cen_lat,JULYR,JULDAY,GMT,    &
                      levsiz,XLAT,n_ozmixm,                           &
                      cldfra_old,                                     & 
                      ozmixm,pin,                                     & 
                      m_ps_1,m_ps_2,m_hybi,aerosolc_1,aerosolc_2,     & 
                      paerlev,n_aerosolc,                             &
                      sfull,shalf,pptop,swrad_scat,                  &
                      config_flags,restart,                          &
                      allowed_to_read, start_of_simulation,          &
                      ids, ide, jds, jde, kds, kde,                  &
                      ims, ime, jms, jme, kms, kme,                  &
                      its, ite, jts, jte, kts, kte                   )

   USE module_ra_rrtm
   USE module_ra_rrtmg_lw
   USE module_ra_rrtmg_sw
   USE module_ra_cam
   USE module_ra_sw
   USE module_ra_gsfcsw
   USE module_ra_gfdleta
   USE module_ra_hs
   USE module_domain

   IMPLICIT NONE

   INTEGER,  INTENT(IN)           :: id
   TYPE (grid_config_rec_type)    :: config_flags
   LOGICAL , INTENT(IN)           :: restart
   LOGICAL,  INTENT(IN)           :: allowed_to_read

   INTEGER , INTENT(IN)           :: ids, ide, jds, jde, kds, kde,   &
                                     ims, ime, jms, jme, kms, kme,   &
                                     its, ite, jts, jte, kts, kte

   INTEGER , INTENT(IN)           :: JULDAY,JULYR
   REAL ,    INTENT(IN)           :: DT, RADT, cen_lat, GMT, pptop,  &
                                     swrad_scat
   LOGICAL,  INTENT(IN)           :: start_of_simulation

   INTEGER,      INTENT(IN   )    ::   levsiz, n_ozmixm
   INTEGER,      INTENT(IN   )    ::   paerlev, n_aerosolc

   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(IN) ::  XLAT

   REAL,  DIMENSION( ims:ime, levsiz, jms:jme, n_ozmixm ), OPTIONAL,      &
          INTENT(INOUT) ::                                  OZMIXM

   REAL,  DIMENSION(ims:ime,jms:jme), OPTIONAL, INTENT(INOUT)  :: m_ps_1,m_ps_2
   REAL,  DIMENSION(paerlev), OPTIONAL, INTENT(INOUT)  ::         m_hybi
   REAL,  DIMENSION( ims:ime, paerlev, jms:jme, n_aerosolc ), OPTIONAL,     &
          INTENT(INOUT) ::                      aerosolc_1, aerosolc_2

   REAL,  DIMENSION(levsiz), OPTIONAL, INTENT(INOUT)  ::          PIN

   INTEGER , INTENT(INOUT)        :: STEPRA
   INTEGER :: isn

   REAL , DIMENSION( kms:kme ) , INTENT(IN) :: sfull, shalf
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::           &
                                                           RTHRATEN, &
                                                         RTHRATENLW, &
                                                         RTHRATENSW, &
                                                             CLDFRA

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , OPTIONAL, INTENT(OUT) :: &
                                                         CLDFRA_OLD

   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT) :: EMISS
   LOGICAL :: etalw = .false.
   LOGICAL :: camlw = .false.
   LOGICAL :: etamp = .false.
   integer :: month,iday
   INTEGER :: i, j, k, itf, jtf, ktf


   jtf=min0(jte,jde-1)
   ktf=min0(kte,kde-1)
   itf=min0(ite,ide-1)





    STEPRA = nint(RADT*60./DT)
    STEPRA = max(STEPRA,1)



   IF(start_of_simulation)THEN
     DO j=jts,jtf
     DO k=kts,ktf
     DO i=its,itf
        RTHRATEN(i,k,j)=0.
        RTHRATENLW(i,k,j)=0.
        RTHRATENSW(i,k,j)=0.
        CLDFRA(i,k,j)=0.
     ENDDO
     ENDDO
     ENDDO

     if( present(cldfra_old) ) then
        DO j=jts,jtf
        DO k=kts,ktf
        DO i=its,itf
           cldfra_old(i,k,j) = 0.
        ENDDO
        ENDDO
        ENDDO
     end if
   ENDIF



   mp_select: SELECT CASE(config_flags%mp_physics)

        CASE (ETAMPNEW)
             etamp = .true.

   END SELECT mp_select



   lwrad_select: SELECT CASE(config_flags%ra_lw_physics)

        CASE (RRTMSCHEME)
             CALL rrtminit(                                 &
                           allowed_to_read ,                &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           its, ite, jts, jte, kts, kte     )

        CASE (CAMLWSCHEME)
             IF ( PRESENT( OZMIXM ) .AND. PRESENT( PIN ) .AND. &
                  PRESENT(M_PS_1) .AND. PRESENT(M_PS_2) .AND.  &
                  PRESENT(M_HYBI) .AND. PRESENT(AEROSOLC_1)    &
                  .AND. PRESENT(AEROSOLC_2)) THEN
             CALL camradinit(                                  &
                         R_D,R_V,CP,G,STBOLT,EP_2,shalf,pptop, &
                         ozmixm,pin,levsiz,XLAT,n_ozmixm,      &
                         m_ps_1,m_ps_2,m_hybi,aerosolc_1,aerosolc_2,&
                         paerlev, n_aerosolc,              &
                         ids, ide, jds, jde, kds, kde,     &
                         ims, ime, jms, jme, kms, kme,     &
                         its, ite, jts, jte, kts, kte      )
             ELSE
                CALL wrf_error_fatal3("",944,&
'arguments not present for calling cam radiation' )
             ENDIF

             camlw = .true.

        CASE (RRTMG_LWSCHEME)
             CALL rrtmg_lwinit(                             &
                           allowed_to_read ,                &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           its, ite, jts, jte, kts, kte     )

        CASE (GFDLLWSCHEME)
             CALL nl_get_start_month(id,month)
             CALL nl_get_start_day(id,iday)
             CALL gfdletainit(emiss,sfull,shalf,pptop,      &
                              julyr,month,iday,gmt,         &
                              config_flags,allowed_to_read, &
                              ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              its, ite, jts, jte, kts, kte  )
             etalw = .true.
        CASE (HELDSUAREZ)
             CALL hsinit(RTHRATEN,restart,             &
                         ids, ide, jds, jde, kds, kde, &
                         ims, ime, jms, jme, kms, kme, &
                         its, ite, jts, jte, kts, kte )
        CASE DEFAULT

   END SELECT lwrad_select


   swrad_select: SELECT CASE(config_flags%ra_sw_physics)

        CASE (SWRADSCHEME)
             CALL swinit(                                  &
                         swrad_scat,                       &
                         allowed_to_read ,                 &
                         ids, ide, jds, jde, kds, kde,     &
                         ims, ime, jms, jme, kms, kme,     &
                         its, ite, jts, jte, kts, kte      )

        CASE (CAMSWSCHEME)
             IF(.not.camlw)THEN
             CALL camradinit(                              &
                         R_D,R_V,CP,G,STBOLT,EP_2,shalf,pptop,               &
                         ozmixm,pin,levsiz,XLAT,n_ozmixm,     &
                         m_ps_1,m_ps_2,m_hybi,aerosolc_1,aerosolc_2,&
                         paerlev, n_aerosolc,              &
                         ids, ide, jds, jde, kds, kde,     &
                         ims, ime, jms, jme, kms, kme,     &
                         its, ite, jts, jte, kts, kte      )
             ENDIF

        CASE (GSFCSWSCHEME)
             CALL gsfc_swinit(cen_lat, allowed_to_read )

        CASE (RRTMG_SWSCHEME)
             CALL rrtmg_swinit(                             &
                           allowed_to_read ,                &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           its, ite, jts, jte, kts, kte     )

        CASE (GFDLSWSCHEME)
             IF(.not.etalw)THEN
             CALL nl_get_start_month(id,month)
             CALL nl_get_start_day(id,iday)
             CALL gfdletainit(emiss,sfull,shalf,pptop,      &
                              julyr,month,iday,gmt,         &
                              config_flags,allowed_to_read, &
                              ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              its, ite, jts, jte, kts, kte  )
             ENDIF

        CASE DEFAULT

   END SELECT swrad_select

   END SUBROUTINE ra_init

   SUBROUTINE bl_init(STEPBL,BLDT,DT,RUBLTEN,RVBLTEN,RTHBLTEN,  &
                RQVBLTEN,RQCBLTEN,RQIBLTEN,TSK,TMN,             &
                config_flags,restart,UST,LOWLYR,TSLB,ZS,DZS,    &
                num_soil_layers,TKE_MYJ,                        &
                EXCH_H,VEGFRA,                                  &
                SNOW,SNOWC, CANWAT,SMSTAV,                      &
                SMSTOT, SFCRUNOFF,UDRUNOFF,ACSNOW,ACSNOM,       &
                IVGTYP,ISLTYP,ISURBAN,SMOIS,SMFR3D,mavail,      &
                SNOWH,SH2O,SNOALB,FNDSOILW,FNDSNOWH,RDMAXALB,   &
                Z0,XLAND,XICE,                                  &
                SFCEVP,GRDFLX,                                  &
                MMINLU,                                         &
                allowed_to_read,                                &
                start_of_simulation,                            &

                DZR, DZB, DZG,                                  & 
                TR_URB2D,TB_URB2D,TG_URB2D,TC_URB2D,QC_URB2D,   & 
                XXXR_URB2D,XXXB_URB2D,XXXG_URB2D,XXXC_URB2D,    & 
                TRL_URB3D, TBL_URB3D, TGL_URB3D,                & 
                SH_URB2D,LH_URB2D,G_URB2D,RN_URB2D,             & 
                TS_URB2D, FRC_URB2D, UTYPE_URB2D,               &
                SF_URBAN_PHYSICS,                               & 
                NUM_URBAN_LAYERS,                               & 
                TRB_URB4D,TW1_URB4D,TW2_URB4D,                  & 
                TGB_URB4D,SFW1_URB3D,SFW2_URB3D,                & 
                SFR_URB3D,SFG_URB3D,                            & 
                A_U_BEP,A_V_BEP,A_T_BEP,A_Q_BEP,                & 
                A_E_BEP,B_U_BEP,B_V_BEP,                        & 
                B_T_BEP,B_Q_BEP,B_E_BEP,DLG_BEP,                & 
                DL_U_BEP,SF_BEP,VL_BEP,                         & 
                ids, ide, jds, jde, kds, kde,                   &
                ims, ime, jms, jme, kms, kme,                   &
                its, ite, jts, jte, kts, kte,                   &
                ACHFX,ACLHF,ACGRDFLX,                           &
                oml_hml0, omlcall,                              & 
                TML,T0ML,HML,H0ML,HUML,HVML                     ) 

   USE module_sf_sfclay
   USE module_sf_slab
   USE module_sf_pxsfclay
   USE module_bl_ysu
   USE module_bl_mrf
   USE module_bl_gfs
   USE module_bl_acm
   USE module_sf_myjsfc
   USE module_sf_qnsesfc
   USE module_sf_noahdrv
   USE module_sf_urban
   USE module_sf_bep                                  
   USE module_sf_ruclsm
   USE module_sf_pxlsm
   USE module_bl_myjpbl
   USE module_bl_myjurb
   USE module_bl_boulac
   USE module_bl_qnsepbl

   USE module_sf_gfdl

   IMPLICIT NONE

   TYPE (grid_config_rec_type) ::     config_flags
   LOGICAL , INTENT(IN)        :: restart
   LOGICAL, INTENT(IN)         ::   FNDSOILW, FNDSNOWH
   LOGICAL, INTENT(IN)         ::   RDMAXALB

   INTEGER , INTENT(IN)        ::     ids, ide, jds, jde, kds, kde, &
                                      ims, ime, jms, jme, kms, kme, &
                                      its, ite, jts, jte, kts, kte
   INTEGER , INTENT(IN)        ::     num_soil_layers
   INTEGER , INTENT(IN)        ::     SF_URBAN_PHYSICS 

   REAL ,    INTENT(IN)        ::     DT, BLDT
   INTEGER , INTENT(INOUT)     ::     STEPBL

   REAL,     DIMENSION( ims:ime , 1:num_soil_layers , jms:jme ),    &
             INTENT(OUT) :: SMFR3D

   REAL,     DIMENSION( ims:ime , 1:num_soil_layers , jms:jme ),&
                   INTENT(INOUT) :: SMOIS,SH2O,TSLB

   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(INOUT)    ::                           SNOW, &
                                                         SNOWH, &
                                                         SNOWC, &
                                                        SNOALB, &
                                                        CANWAT, &
                                                        MAVAIL, &
                                                        SMSTAV, &
                                                        SMSTOT, &
                                                     SFCRUNOFF, &
                                                      UDRUNOFF, &
                                                        ACSNOW, &
                                                        VEGFRA, &
                                                        ACSNOM, &
                                                        SFCEVP, &
                                                        GRDFLX, &
                                                           UST, &
                                                            Z0, &
                                                         XLAND, &
                                                         XICE

   INTEGER, DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(INOUT)    ::                         IVGTYP, &
                                                        ISLTYP, &
                                                        LOWLYR


   REAL,     DIMENSION(1:num_soil_layers), INTENT(INOUT)  ::  ZS,DZS

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::       &
                                                           RUBLTEN, &
                                                           RVBLTEN, &
 						          EXCH_H,   &
                                                          RTHBLTEN, &
                                                          RQVBLTEN, &
                                                          RQCBLTEN, &
                                                          RQIBLTEN, &
                                                          TKE_MYJ
   REAL,  DIMENSION( ims:ime , jms:jme ) , INTENT(IN) ::     TSK
   REAL,  DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT) ::  TMN
   CHARACTER(LEN=*), INTENT(IN)   :: MMINLU
   LOGICAL,  INTENT(IN)           :: allowed_to_read
   INTEGER,  INTENT(IN)           :: ISURBAN
   INTEGER :: isn, isfc
   INTEGER :: k





    REAL, OPTIONAL, DIMENSION(1:num_soil_layers), INTENT(INOUT) :: DZR  
    REAL, OPTIONAL, DIMENSION(1:num_soil_layers), INTENT(INOUT) :: DZB  
    REAL, OPTIONAL, DIMENSION(1:num_soil_layers), INTENT(INOUT) :: DZG  
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TR_URB2D 
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TB_URB2D 
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TG_URB2D 
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TC_URB2D 
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: QC_URB2D 
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXR_URB2D 
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXB_URB2D 
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXG_URB2D 
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXC_URB2D 
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: SH_URB2D 
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: LH_URB2D 
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: G_URB2D 
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: RN_URB2D 
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TS_URB2D 
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: FRC_URB2D 
    INTEGER, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: UTYPE_URB2D 



    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_soil_layers, jms:jme ), INTENT(INOUT) :: TRL_URB3D 
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_soil_layers, jms:jme ), INTENT(INOUT) :: TBL_URB3D 
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_soil_layers, jms:jme ), INTENT(INOUT) :: TGL_URB3D 

    INTEGER , INTENT(IN)        ::     num_urban_layers
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: TRB_URB4D 
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: TW1_URB4D 
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: TW2_URB4D 
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: TGB_URB4D 
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: SFW1_URB3D 
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: SFW2_URB3D 
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: SFR_URB3D 
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: SFG_URB3D 
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: A_U_BEP 
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: A_V_BEP 
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: A_T_BEP 
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: A_Q_BEP 
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: A_E_BEP 
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: B_U_BEP 
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: B_V_BEP 
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: B_T_BEP 
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: B_Q_BEP 
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: B_E_BEP 
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: VL_BEP
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: DLG_BEP 
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme,jms:jme),INTENT(INOUT) :: SF_BEP
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: DL_U_BEP

   REAL,  DIMENSION( ims:ime , jms:jme ) , OPTIONAL, INTENT(INOUT) ::    &
                                        ACHFX,ACLHF,ACGRDFLX

   REAL,  DIMENSION( ims:ime , jms:jme ) , OPTIONAL, INTENT(INOUT) ::    &
                                        TML,T0ML,HML,H0ML,HUML,HVML
   INTEGER,  OPTIONAL,  INTENT(IN) :: omlcall
   REAL,  OPTIONAL,  INTENT(IN) :: oml_hml0
   LOGICAL,  INTENT(IN) :: start_of_simulation
   INTEGER :: i,j





   STEPBL = nint(BLDT*60./DT)
   STEPBL = max(STEPBL,1)


   IF(PRESENT(ACHFX))THEN
   IF(.not.restart)THEN
     DO j=jts,jte
     DO i=its,ite
        ACHFX(i,j)=0.
        ACLHF(i,j)=0.
        ACGRDFLX(i,j)=0.
        SFCEVP(i,j)=0.
     ENDDO
     ENDDO
   ENDIF
   ENDIF



   sfclay_select: SELECT CASE(config_flags%sf_sfclay_physics)

      CASE (SFCLAYSCHEME)
           CALL sfclayinit( allowed_to_read )
           isfc = 1
      CASE (PXSFCSCHEME)
           CALL pxsfclayinit( allowed_to_read )
           isfc = 1
      CASE (MYJSFCSCHEME)
           CALL myjsfcinit(LOWLYR,UST,                         &
                                      Z0,                      &
                                          XLAND,XICE,          &
                         IVGTYP,restart,                       &
                         allowed_to_read ,                     &
                         ids, ide, jds, jde, kds, kde,         &
                         ims, ime, jms, jme, kms, kme,         &
                         its, ite, jts, jte, kts, kte          )
           isfc = 2

      CASE (QNSESFCSCHEME)
           CALL qnsesfcinit(LOWLYR,UST,                         &
                                      Z0,                      &
                                          XLAND,XICE,          &
                         IVGTYP,restart,                       &
                         allowed_to_read ,                     &
                         ids, ide, jds, jde, kds, kde,         &
                         ims, ime, jms, jme, kms, kme,         &
                         its, ite, jts, jte, kts, kte          )
           isfc = 2

      CASE (GFSSFCSCHEME)
           CALL myjsfcinit(LOWLYR,UST,                         &
                                      Z0,                      &
                                          XLAND,XICE,          &
                         IVGTYP,restart,                       &
                         allowed_to_read ,                     &
                         ids, ide, jds, jde, kds, kde,         &
                         ims, ime, jms, jme, kms, kme,         &
                         its, ite, jts, jte, kts, kte          )
           isfc = 1
      CASE (GFDLSFCSCHEME)
           CALL myjsfcinit(LOWLYR,UST,                         &
                                      Z0,                      &
                                          XLAND,XICE,          &
                         IVGTYP,restart,                       &
                         allowed_to_read ,                     &
                         ids, ide, jds, jde, kds, kde,         &
                         ims, ime, jms, jme, kms, kme,         &
                         its, ite, jts, jte, kts, kte          )
           isfc = 1


      CASE DEFAULT

   END SELECT sfclay_select




   sfc_select: SELECT CASE(config_flags%sf_surface_physics)

      CASE (SLABSCHEME)

           CALL slabinit(TSK,TMN,                              &
                         TSLB,ZS,DZS,num_soil_layers,          &
                         allowed_to_read ,start_of_simulation ,&
                         ids, ide, jds, jde, kds, kde,         &
                         ims, ime, jms, jme, kms, kme,         &
                         its, ite, jts, jte, kts, kte,         &
                         oml_hml0, omlcall,                    &
                         tml, t0ml, hml, h0ml, huml, hvml      )

     CASE (GFDLSLAB)
           CALL hwrfsfcinit(isn,XICE,VEGFRA,SNOW,SNOWC, CANWAT,SMSTAV, &
                     SMSTOT, SFCRUNOFF,UDRUNOFF,GRDFLX,ACSNOW,     &
                     ACSNOM,IVGTYP,ISLTYP,TSLB,SMOIS,DZS,SFCEVP,   &
                     TMN,                                          &
                     num_soil_layers,                              &
                     allowed_to_read ,                             &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte                     )

      CASE (LSMSCHEME)
          CALL LSMINIT(VEGFRA,SNOW,SNOWC,SNOWH,CANWAT,SMSTAV,  &
                     SMSTOT, SFCRUNOFF,UDRUNOFF,ACSNOW,        &
                     ACSNOM,IVGTYP,ISLTYP,TSLB,SMOIS,SH2O,ZS,DZS, &
                     MMINLU,                                   &
                     SNOALB, FNDSOILW, FNDSNOWH, RDMAXALB,     &
                     num_soil_layers, restart,                 &
                     allowed_to_read ,                         &
                     ids,ide, jds,jde, kds,kde,                &
                     ims,ime, jms,jme, kms,kme,                &
                     its,ite, jts,jte, kts,kte                 )


          IF ((SF_URBAN_PHYSICS.eq.1).OR.(SF_URBAN_PHYSICS.EQ.2)) THEN

             IF ( PRESENT( FRC_URB2D ) .AND. PRESENT( UTYPE_URB2D )) THEN

                CALL urban_param_init(DZR,DZB,DZG,num_soil_layers                    & 
                                )


                CALL urban_var_init(ISURBAN,TSK,TSLB,TMN,IVGTYP,                     & 
                              ims,ime,jms,jme,kms,kme,num_soil_layers,               & 

                              restart,                                         & 
                              XXXR_URB2D,XXXB_URB2D,XXXG_URB2D,XXXC_URB2D,     & 
                              TR_URB2D,TB_URB2D,TG_URB2D,TC_URB2D,QC_URB2D,    & 
                              TRL_URB3D,TBL_URB3D,TGL_URB3D,                   & 
                              SH_URB2D,LH_URB2D,G_URB2D,RN_URB2D, TS_URB2D,    & 
                              num_urban_layers,                                & 
                              TRB_URB4D,TW1_URB4D,TW2_URB4D,TGB_URB4D,         & 
                              SFW1_URB3D,SFW2_URB3D,SFR_URB3D,SFG_URB3D,       & 
                              A_U_BEP,A_V_BEP,A_T_BEP,A_Q_BEP,                 & 
                              A_E_BEP,B_U_BEP,B_V_BEP,                         & 
                              B_T_BEP,B_Q_BEP,B_E_BEP,DLG_BEP,                 & 
                              DL_U_BEP,SF_BEP,VL_BEP,                          & 
                              FRC_URB2D, UTYPE_URB2D)                            
             ELSE
                CALL wrf_error_fatal3("",1361,&
'arguments not present for calling urban model' )
             ENDIF
          ENDIF

      CASE (RUCLSMSCHEME)


           CALL lsmrucinit( SMFR3D,TSLB,SMOIS,ISLTYP,mavail,       &
                     num_soil_layers, restart,                     &
                     allowed_to_read ,                             &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte                     )

      CASE (PXLSMSCHEME)
          CALL LSMINIT(VEGFRA,SNOW,SNOWC,SNOWH,CANWAT,SMSTAV,  &
                     SMSTOT, SFCRUNOFF,UDRUNOFF,ACSNOW,        &
                     ACSNOM,IVGTYP,ISLTYP,TSLB,SMOIS,SH2O,ZS,DZS, &
                     MMINLU,                                   &
                     SNOALB, FNDSOILW, FNDSNOWH, RDMAXALB,     &
                     num_soil_layers, restart,                 &
                     allowed_to_read ,                         &
                     ids,ide, jds,jde, kds,kde,                &
                     ims,ime, jms,jme, kms,kme,                &
                     its,ite, jts,jte, kts,kte                 )

      CASE DEFAULT

   END SELECT sfc_select



   pbl_select: SELECT CASE(config_flags%bl_pbl_physics)

      CASE (YSUSCHEME)
           if(isfc .ne. 1)CALL wrf_error_fatal &
            ( 'module_physics_init: use sfclay scheme for this pbl option' )
           CALL ysuinit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,    &
                        RQCBLTEN,RQIBLTEN,P_QI,               &
                        PARAM_FIRST_SCALAR,                   &
                        restart,                              &
                        allowed_to_read ,                     &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        its, ite, jts, jte, kts, kte          )
      CASE (MRFSCHEME)
           if(isfc .ne. 1)CALL wrf_error_fatal &
            ( 'module_physics_init: use sfclay scheme for this pbl option' )
           CALL mrfinit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,    &
                        RQCBLTEN,RQIBLTEN,P_QI,               &
                        PARAM_FIRST_SCALAR,                   &
                        restart,                              &
                        allowed_to_read ,                     &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        its, ite, jts, jte, kts, kte          )
      CASE (ACMPBLSCHEME)
           if(isfc .ne. 1)CALL wrf_error_fatal &
            ( 'module_physics_init: use sfclay scheme for this pbl option' )
           CALL acminit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,    &
                        RQCBLTEN,RQIBLTEN,P_QI,               &
                        PARAM_FIRST_SCALAR,                   &
                        restart,                              &
                        allowed_to_read ,                     &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        its, ite, jts, jte, kts, kte          )
      CASE (GFSSCHEME)
           if(isfc .ne. 1)CALL wrf_error_fatal &
            ( 'module_physics_init: use sfclay scheme for this pbl option' )
           CALL gfsinit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,    &
                        RQCBLTEN,RQIBLTEN,P_QI,               &
                        PARAM_FIRST_SCALAR,                   &
                        restart,                              &
                        allowed_to_read ,                     &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        its, ite, jts, jte, kts, kte          )
      CASE (MYJPBLSCHEME)
           if(isfc .ne. 2)CALL wrf_error_fatal &
            ( 'module_physics_init: use myjsfc scheme for this pbl option' )
          IF (SF_URBAN_PHYSICS.eq.2) THEN
           CALL myjurbinit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN, &
                        TKE_MYJ,EXCH_H,restart,               &
                        allowed_to_read ,                     &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        its, ite, jts, jte, kts, kte          )
          ELSE

           CALL myjpblinit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN, &
                        TKE_MYJ,EXCH_H,restart,               &
                        allowed_to_read ,                     &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        its, ite, jts, jte, kts, kte          )
          END IF
      CASE (QNSEPBLSCHEME)
           CALL qnsepblinit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN, &
                        TKE_MYJ,EXCH_H,restart,               &
                        allowed_to_read ,                     &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        its, ite, jts, jte, kts, kte          )


      CASE DEFAULT

   END SELECT pbl_select


   END SUBROUTINE bl_init


   SUBROUTINE cu_init(STEPCU,CUDT,DT,RTHCUTEN,RQVCUTEN,RQRCUTEN,  &
                      RQCCUTEN,RQSCUTEN,RQICUTEN,NCA,RAINC,       &
                      RAINCV,W0AVG,config_flags,restart,          &
                      CLDEFI,LOWLYR,MASS_FLUX,                    &
                      RTHFTEN, RQVFTEN,                           &
                      APR_GR,APR_W,APR_MC,APR_ST,APR_AS,          &
                      APR_CAPMA,APR_CAPME,APR_CAPMI,              &
                      cugd_tten,cugd_ttens,cugd_qvten,            &
                      cugd_qvtens,cugd_qcten,                     &
                      allowed_to_read, start_of_simulation,       &
                      ids, ide, jds, jde, kds, kde,               &
                      ims, ime, jms, jme, kms, kme,               &
                      its, ite, jts, jte, kts, kte                )

   USE module_cu_kf
   USE module_cu_kfeta
   USE MODULE_CU_BMJ
   USE module_cu_gd,  ONLY : GDINIT
   USE module_cu_g3,  ONLY : G3INIT
   USE module_cu_sas

   IMPLICIT NONE

   TYPE (grid_config_rec_type) ::     config_flags
   LOGICAL , INTENT(IN)        :: restart


   INTEGER , INTENT(IN)        :: ids, ide, jds, jde, kds, kde,   &
                                  ims, ime, jms, jme, kms, kme,   &
                                  its, ite, jts, jte, kts, kte

   REAL ,    INTENT(IN)        :: DT, CUDT
   LOGICAL , INTENT(IN)        :: start_of_simulation
   LOGICAL , INTENT(IN)        :: allowed_to_read
   INTEGER , INTENT(INOUT)     :: STEPCU

   REAL ,   DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) ::    &
            RTHCUTEN, RQVCUTEN, RQCCUTEN, RQRCUTEN, RQICUTEN, RQSCUTEN
   REAL ,   DIMENSION( ims:ime , kms:kme , jms:jme ) , OPTIONAL, INTENT(INOUT) ::    &
                        cugd_tten,cugd_ttens,cugd_qvten,            &
                        cugd_qvtens,cugd_qcten

   REAL ,   DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) :: W0AVG

   REAL,    DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::    &
            RTHFTEN, RQVFTEN

   REAL ,   DIMENSION( ims:ime , jms:jme ), INTENT(OUT):: RAINC, RAINCV

   REAL ,   DIMENSION( ims:ime , jms:jme ), INTENT(OUT):: CLDEFI

   REAL ,   DIMENSION( ims:ime , jms:jme ), INTENT(INOUT):: NCA

   REAL ,   DIMENSION( ims:ime , jms:jme ), INTENT(INOUT):: MASS_FLUX,   &
                                   APR_GR,APR_W,APR_MC,APR_ST,APR_AS,    &
                                   APR_CAPMA,APR_CAPME,APR_CAPMI

   INTEGER, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT):: LOWLYR



  INTEGER :: i,j,itf,jtf





   itf=min0(ite,ide-1)
   jtf=min0(jte,jde-1)

   STEPCU = nint(CUDT*60./DT)
   STEPCU = max(STEPCU,1)



   IF(start_of_simulation)THEN
     DO j=jts,jtf
     DO i=its,itf
        RAINC(i,j)=0.
        RAINCV(i,j)=0.
     ENDDO
     ENDDO
   ENDIF

   cps_select: SELECT CASE(config_flags%cu_physics)

     CASE (KFSCHEME)
          CALL kfinit(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN,        &
                      RQICUTEN,RQSCUTEN,NCA,W0AVG,P_QI,P_QS,      &
                      PARAM_FIRST_SCALAR,restart,                 &
                      allowed_to_read ,                           &
                      ids, ide, jds, jde, kds, kde,               &
                      ims, ime, jms, jme, kms, kme,               &
                      its, ite, jts, jte, kts, kte                )

     CASE (BMJSCHEME)
          CALL bmjinit(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN,       &
                      CLDEFI,LOWLYR,cp,r_d,restart,               &
                      allowed_to_read ,                           &
                      ids, ide, jds, jde, kds, kde,               &
                      ims, ime, jms, jme, kms, kme,               &
                      its, ite, jts, jte, kts, kte                )

     CASE (KFETASCHEME)
          CALL kf_eta_init(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN,   &
                      RQICUTEN,RQSCUTEN,NCA,W0AVG,P_QI,P_QS,      &
                      SVP1,SVP2,SVP3,SVPT0,                       &
                      PARAM_FIRST_SCALAR,restart,                 &
                      allowed_to_read ,                           &
                      ids, ide, jds, jde, kds, kde,               &
                      ims, ime, jms, jme, kms, kme,               &
                      its, ite, jts, jte, kts, kte                )
     CASE (GDSCHEME)
          CALL gdinit(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQICUTEN,        &
                      MASS_FLUX,cp,restart,                       &
                      P_QC,P_QI,PARAM_FIRST_SCALAR,               &
                      RTHFTEN, RQVFTEN,                           &
                      APR_GR,APR_W,APR_MC,APR_ST,APR_AS,          &
                      APR_CAPMA,APR_CAPME,APR_CAPMI,              &
                      allowed_to_read ,                           &
                      ids, ide, jds, jde, kds, kde,               &
                      ims, ime, jms, jme, kms, kme,               &
                      its, ite, jts, jte, kts, kte                )
     CASE (SASSCHEME)
          CALL sasinit(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQICUTEN,       &
                      restart,P_QC,P_QI,PARAM_FIRST_SCALAR,       &
                      allowed_to_read ,                           &
                      ids, ide, jds, jde, kds, kde,               &
                      ims, ime, jms, jme, kms, kme,               &
                      its, ite, jts, jte, kts, kte                )

     CASE DEFAULT

   END SELECT cps_select

   END SUBROUTINE cu_init


   SUBROUTINE mp_init(RAINNC,config_flags,restart,warm_rain,      &
                      adv_moist_cond,                             &
                      MPDT, DT, DX, DY, LOWLYR,                   & 
                      F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY,           & 
                      mp_restart_state,tbpvs_state,tbpvs0_state,   & 
                      allowed_to_read, start_of_simulation,       &
                      ids, ide, jds, jde, kds, kde,               &
                      ims, ime, jms, jme, kms, kme,               &
                      its, ite, jts, jte, kts, kte                )

   USE module_mp_wsm3
   USE module_mp_wsm5
   USE module_mp_wsm6
   USE module_mp_etanew
   USE module_mp_thompson
   USE module_mp_thompson07
   USE module_mp_morr_two_moment
   USE module_mp_wdm5
   USE module_mp_wdm6

   IMPLICIT NONE


   TYPE (grid_config_rec_type) ::     config_flags
   LOGICAL , INTENT(IN)        :: restart
   LOGICAL , INTENT(OUT)       :: warm_rain,adv_moist_cond
   REAL    , INTENT(IN)        :: MPDT, DT, DX, DY
   LOGICAL , INTENT(IN)        :: start_of_simulation

   INTEGER , INTENT(IN)        :: ids, ide, jds, jde, kds, kde,   &
                                  ims, ime, jms, jme, kms, kme,   &
                                  its, ite, jts, jte, kts, kte

   INTEGER , DIMENSION( ims:ime , jms:jme ) ,INTENT(INOUT)  :: LOWLYR
   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT) :: RAINNC
   REAL,     DIMENSION( ims:ime , kms:kme, jms:jme ) , INTENT(INOUT) :: &
                                  F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY
   REAL , DIMENSION(:) ,INTENT(INOUT)  :: mp_restart_state,tbpvs_state,tbpvs0_state
   LOGICAL , INTENT(IN)  :: allowed_to_read


   INTEGER :: i, j, itf, jtf

   warm_rain = .false.
   adv_moist_cond = .true.
   itf=min0(ite,ide-1)
   jtf=min0(jte,jde-1)

   IF(start_of_simulation)THEN
     DO j=jts,jtf
     DO i=its,itf
        RAINNC(i,j) = 0.
     ENDDO
     ENDDO
   ENDIF

   mp_select: SELECT CASE(config_flags%mp_physics)

     CASE (KESSLERSCHEME)
          warm_rain = .true.
     CASE (WSM3SCHEME)
          CALL wsm3init(rhoair0,rhowater,rhosnow,cliq,cpv, allowed_to_read )
     CASE (WSM5SCHEME)
          CALL wsm5init(rhoair0,rhowater,rhosnow,cliq,cpv, allowed_to_read )
     CASE (WSM6SCHEME)
          CALL wsm6init(rhoair0,rhowater,rhosnow,cliq,cpv, allowed_to_read )
     CASE (ETAMPNEW)
         adv_moist_cond = .false.
         CALL etanewinit (MPDT,DT,DX,DY,LOWLYR,restart,           &
                          F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY,       &
                          mp_restart_state,tbpvs_state,tbpvs0_state,&
                          allowed_to_read,                        &
                          ids, ide, jds, jde, kds, kde,           &
                          ims, ime, jms, jme, kms, kme,           &
                          its, ite, jts, jte, kts, kte            )
     CASE (THOMPSON)


         IF(start_of_simulation.or.restart.or.config_flags%cycling)CALL thompson_init

     CASE (THOMPSON07)
         IF(start_of_simulation.or.restart.or.config_flags%cycling)CALL thompson07_init

     CASE (MORR_TWO_MOMENT)
         CALL morr_two_moment_init
     CASE (WDM5SCHEME)
          CALL wdm5init(rhoair0,rhowater,rhosnow,cliq,cpv,n_ccn0,allowed_to_read )
     CASE (WDM6SCHEME)
          CALL wdm6init(rhoair0,rhowater,rhosnow,cliq,cpv,n_ccn0,allowed_to_read )

     CASE DEFAULT

   END SELECT mp_select

   END SUBROUTINE mp_init



   SUBROUTINE z2sigma(zf,zh,sf,sh,p_top,pptop,config_flags, &
                allowed_to_read , &
                kds,kde,kms,kme,kts,kte)
   IMPLICIT NONE

   INTEGER, INTENT(IN) :: kds,kde,kms,kme,kts,kte
   REAL , DIMENSION( kms:kme ), INTENT(IN) :: zf,zh
   REAL , DIMENSION( kms:kme ), INTENT(OUT):: sf,sh
   REAL , INTENT(IN) :: p_top
   REAL , INTENT(OUT) :: pptop
   TYPE (grid_config_rec_type)              :: config_flags
   LOGICAL , INTENT(IN) :: allowed_to_read

   REAL R, G, TS, GAMMA, PS, ZTROP, TSTRAT, PTROP, Z, T, P, ZTOP, PTOP
   INTEGER K

   IF(zf(kde/2) .GT. 1.0)THEN



      r=287.05
      g=9.80665
      ts=288.15
      gamma=-6.5/1000.
      ps=1013.25
      ztrop=11000.
      tstrat=ts+gamma*ztrop
      ptrop=ps*(tstrat/ts)**(-g/(gamma*r))

      do k=kde,kds,-1

        z=zf(k)
        if(z.le.ztrop)then
          t=ts+gamma*z
          p=ps*(t/ts)**(-g/(gamma*r))
        else
          t=tstrat
          p=ptrop*exp(-g*(z-ztrop)/(r*tstrat))
        endif
        if(k.eq.kde)then
          ztop=zf(k)
          ptop=p
        endif
        sf(k)=(p-ptop)/(ps-ptop)

        if(k.ne.kds)then
        z=0.5*(zf(k)+zf(k-1))
        if(z.le.ztrop)then
          t=ts+gamma*z
          p=ps*(t/ts)**(-g/(gamma*r))
        else
          t=tstrat
          p=ptrop*exp(-g*(z-ztrop)/(r*tstrat))
        endif
        sh(k-1)=(p-ptop)/(ps-ptop)
        endif
      enddo
      pptop=ptop/10.
   ELSE

      do k=kde,kds,-1


         sf(k)=zf(k)
         if(k .ne. kde)sh(k)=zh(k)
      enddo
      pptop=p_top/1000.

   ENDIF

   END SUBROUTINE z2sigma

END MODULE module_physics_init
