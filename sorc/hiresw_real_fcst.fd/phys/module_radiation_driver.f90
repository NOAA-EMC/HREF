

MODULE module_radiation_driver
CONTAINS




   SUBROUTINE radiation_driver (                                          &
               itimestep,dt ,lw_physics,sw_physics ,NPHS                  &
              ,RTHRATENLW ,RTHRATENSW ,RTHRATEN                           &
              ,ACSWUPT,ACSWUPTC,ACSWDNT,ACSWDNTC                          & 
              ,ACSWUPB,ACSWUPBC,ACSWDNB,ACSWDNBC                          & 
              ,ACLWUPT,ACLWUPTC,ACLWDNT,ACLWDNTC                          & 
              ,ACLWUPB,ACLWUPBC,ACLWDNB,ACLWDNBC                          & 
              ,  SWUPT,  SWUPTC,  SWDNT,  SWDNTC                          & 
              ,  SWUPB,  SWUPBC,  SWDNB,  SWDNBC                          & 
              ,  LWUPT,  LWUPTC,  LWDNT,  LWDNTC                          & 
              ,  LWUPB,  LWUPBC,  LWDNB,  LWDNBC                          & 
              ,LWCF,SWCF,OLR                                              & 
              ,SWUPFLX,SWUPFLXC,SWDNFLX,SWDNFLXC                          & 
              ,LWUPFLX,LWUPFLXC,LWDNFLX,LWDNFLXC                          & 
              ,GLW, GSW, SWDOWN, XLAT, XLONG, ALBEDO                      &
              ,EMISS, rho, p8w, p , pi , dz8w ,t, t8w, GMT                &
              ,XLAND, XICE, TSK, HTOP,HBOT,HTOPR,HBOTR, CUPPT, VEGFRA, SNOW     &
              ,julyr, JULDAY, julian, YR, xtime, RADT, STEPRA, ICLOUD, warm_rain     &
              ,declin_urb,COSZ_URB2D, omg_urb2d                           & 
              ,ra_call_offset,RSWTOA,RLWTOA, CZMEAN                       &
              ,CFRACL, CFRACM, CFRACH                                     &
              ,ACFRST,NCFRST,ACFRCV,NCFRCV,SWDOWNC                        &
              ,z                                                          &
              ,levsiz, n_ozmixm, n_aerosolc, paerlev                      &
              ,cam_abs_dim1, cam_abs_dim2, cam_abs_freq_s                 &
              ,ozmixm,pin                                                 & 
              ,m_ps_1,m_ps_2,aerosolc_1,aerosolc_2,m_hybi0                & 
              ,abstot, absnxt, emstot                                     & 
              ,taucldi, taucldc                                           & 
              ,ids, ide, jds, jde, kds, kde                               &
              ,ims, ime, jms, jme, kms, kme                               &
              ,i_start, i_end                                             &
              ,j_start, j_end                                             &
              ,kts, kte                                                   &
              ,num_tiles, CURR_SECS, adapt_step_flag                      &
              ,qv,qc,qr,qi,qs,qg,qndrop                                   &
              ,f_qv,f_qc,f_qr,f_qi,f_qs,f_qg,f_qndrop                     &
              ,CLDFRA ,Pb                                                 &
              ,f_ice_phy,f_rain_phy                                       &
              ,pm2_5_dry, pm2_5_water, pm2_5_dry_ec                       &
              ,tauaer300, tauaer400, tauaer600, tauaer999                 & 
              ,gaer300, gaer400, gaer600, gaer999                         & 
              ,waer300, waer400, waer600, waer999                         & 
              ,qc_adjust ,qi_adjust                                       & 
              ,cu_rad_feedback, aer_ra_feedback                           & 
              ,ht,dx,dy,sina,cosa,shadowmask,slope_rad ,topo_shading      ) 




   USE module_state_description, ONLY : RRTMSCHEME, GFDLLWSCHEME        &
                                       ,RRTMG_LWSCHEME, RRTMG_SWSCHEME  &
                                       ,SWRADSCHEME, GSFCSWSCHEME       &
                                       ,GFDLSWSCHEME, CAMLWSCHEME, CAMSWSCHEME &
                                       ,HELDSUAREZ
   USE module_model_constants
   USE module_wrf_error , ONLY : wrf_err_message



   USE module_ra_sw , ONLY : swrad
   USE module_ra_gsfcsw , ONLY : gsfcswrad
   USE module_ra_rrtm , ONLY : rrtmlwrad
   USE module_ra_rrtmg_lw , ONLY : rrtmg_lwrad
   USE module_ra_rrtmg_sw , ONLY : rrtmg_swrad
   USE module_ra_cam , ONLY : camrad
   USE module_ra_gfdleta , ONLY : etara
   USE module_ra_hs , ONLY : hsrad

   
   
   
   
   
   
   
   
   
   

   IMPLICIT NONE






















































































































































   INTEGER,      INTENT(IN   )    ::   ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                                         kts,kte, &
                                       num_tiles

   INTEGER, INTENT(IN)            :: lw_physics, sw_physics

   INTEGER, DIMENSION(num_tiles), INTENT(IN) ::                       &
                i_start,i_end,j_start,j_end

   INTEGER,      INTENT(IN   )    ::   STEPRA,ICLOUD,ra_call_offset
   INTEGER,      INTENT(IN   )    ::   levsiz, n_ozmixm
   INTEGER,      INTENT(IN   )    ::   paerlev, n_aerosolc, cam_abs_dim1, cam_abs_dim2
   REAL,      INTENT(IN   )       ::   cam_abs_freq_s

   LOGICAL,      INTENT(IN   )    ::   warm_rain

   REAL,      INTENT(IN   )       ::   RADT

   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(IN   )  ::                                 XLAND, &
                                                            XICE, &
                                                             TSK, &
                                                          VEGFRA, &
                                                            SNOW 
   REAL,  DIMENSION( ims:ime, levsiz, jms:jme, n_ozmixm ),  OPTIONAL,    &
          INTENT(IN   ) ::                                  OZMIXM

   REAL,  DIMENSION(levsiz), OPTIONAL, INTENT(IN )  ::     PIN

   REAL,  DIMENSION(ims:ime,jms:jme), OPTIONAL, INTENT(IN )  ::      m_ps_1,m_ps_2
   REAL,  DIMENSION( ims:ime, paerlev, jms:jme, n_aerosolc ), OPTIONAL, &
          INTENT(IN   ) ::                       aerosolc_1, aerosolc_2
   REAL,  DIMENSION(paerlev), OPTIONAL, &
          INTENT(IN   ) ::                           m_hybi0

   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(INOUT)  ::                                  HTOP, &
                                                            HBOT, &
                                                           HTOPR, &
                                                           HBOTR, &
                                                           CUPPT

   INTEGER, INTENT(IN   )  ::   julyr

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         INTENT(IN ) ::                                     dz8w, &
                                                               z, &
                                                             p8w, &
                                                               p, &
                                                              pi, &
                                                               t, &
                                                             t8w, &
                                                             rho

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), OPTIONAL ,       &
         INTENT(IN ) ::  tauaer300,tauaer400,tauaer600,tauaer999, & 
                                 gaer300,gaer400,gaer600,gaer999, & 
                                 waer300,waer400,waer600,waer999, & 
                                 qc_adjust, qi_adjust

   LOGICAL, OPTIONAL :: cu_rad_feedback

   INTEGER, INTENT(IN   ), OPTIONAL  ::   aer_ra_feedback




   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), OPTIONAL ,       &
         INTENT(IN ) ::                                pm2_5_dry, &
                                                     pm2_5_water, &
                                                    pm2_5_dry_ec

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         INTENT(INOUT)  ::                              RTHRATEN, &
                                                      RTHRATENLW, &
                                                      RTHRATENSW











   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT) ::&
                      ACSWUPT,ACSWUPTC,ACSWDNT,ACSWDNTC,          &
                      ACSWUPB,ACSWUPBC,ACSWDNB,ACSWDNBC,          &
                      ACLWUPT,ACLWUPTC,ACLWDNT,ACLWDNTC,          &
                      ACLWUPB,ACLWUPBC,ACLWDNB,ACLWDNBC


   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT) ::&
                        SWUPT,  SWUPTC,  SWDNT,  SWDNTC,          &
                        SWUPB,  SWUPBC,  SWDNB,  SWDNBC,          &
                        LWUPT,  LWUPTC,  LWDNT,  LWDNTC,          &
                        LWUPB,  LWUPBC,  LWDNB,  LWDNBC


   REAL, DIMENSION( ims:ime, kms:kme+2, jms:jme ),                &
         OPTIONAL, INTENT(INOUT) ::                               &
                               SWUPFLX,SWUPFLXC,SWDNFLX,SWDNFLXC, &
                               LWUPFLX,LWUPFLXC,LWDNFLX,LWDNFLXC

   REAL, DIMENSION( ims:ime, jms:jme ),          OPTIONAL ,       &
         INTENT(INOUT)  ::                                  SWCF, &
                                                            LWCF, &
                                                             OLR



   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(IN   )  ::                                  XLAT, &
                                                           XLONG, &
                                                          ALBEDO, &
                                                           EMISS

   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(INOUT)  ::                                   GSW, &
                                                             GLW

   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)  ::   SWDOWN

   REAL, INTENT(IN  )   ::                                GMT,dt, &
                                                   julian, xtime
   INTEGER, INTENT(IN  ),OPTIONAL ::                          YR

   INTEGER, INTENT(IN  ) ::                    JULDAY, itimestep
   REAL, INTENT(IN ),OPTIONAL     ::                    CURR_SECS
   LOGICAL, INTENT(IN ),OPTIONAL  ::              ADAPT_STEP_FLAG

   INTEGER,INTENT(IN)                                       :: NPHS
   REAL, DIMENSION( ims:ime, jms:jme ),INTENT(OUT)          ::    &
                                                      CFRACH,     & 
                                                      CFRACL,     & 
                                                      CFRACM,     & 
                                                      CZMEAN        
   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(INOUT)  ::                                        &
                                                      RLWTOA,     & 
                                                      RSWTOA,     & 
                                                      ACFRST,     & 
                                                      ACFRCV        

   INTEGER,DIMENSION( ims:ime, jms:jme ),INTENT(INOUT)        ::  &
                                                          NCFRST, &  
                                                          NCFRCV     


   REAL, DIMENSION( ims:ime, kms:kme, cam_abs_dim2, jms:jme ), OPTIONAL ,&
         INTENT(INOUT)  ::                                  abstot
   REAL, DIMENSION( ims:ime, kms:kme, cam_abs_dim1, jms:jme ), OPTIONAL ,&
         INTENT(INOUT)  ::                                  absnxt
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),               OPTIONAL ,&
         INTENT(INOUT)  ::                                  emstot




   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         OPTIONAL,                                                &
         INTENT(INOUT) ::                                 CLDFRA

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                     &
         OPTIONAL,                                                   &
         INTENT(IN   ) ::                                            &
                                                          F_ICE_PHY, &
                                                         F_RAIN_PHY

   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         OPTIONAL,                                                &
         INTENT(OUT) ::                                   SWDOWNC

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         OPTIONAL,                                                &
         INTENT(INOUT ) ::                                        &
                                                               pb &
                                        ,qv,qc,qr,qi,qs,qg,qndrop

   LOGICAL, OPTIONAL ::     f_qv,f_qc,f_qr,f_qi,f_qs,f_qg,f_qndrop

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         OPTIONAL,                                                &
         INTENT(INOUT)  ::                       taucldi,taucldc



     REAL, OPTIONAL, INTENT(IN) :: dx,dy
     INTEGER, OPTIONAL, INTENT(IN) :: slope_rad,topo_shading
     REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(IN)  :: sina,cosa,ht
     INTEGER, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(IN) :: shadowmask




   REAL, DIMENSION( ims:ime, jms:jme ) ::             GLAT,GLON
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) ::    CEMISS
   REAL, DIMENSION( ims:ime, jms:jme ) ::             coszr

   REAL    ::    DECLIN,SOLCON 
   INTEGER ::    i,j,k,its,ite,jts,jte,ij
   INTEGER ::    STEPABS
   LOGICAL ::    gfdl_lw,gfdl_sw
   LOGICAL ::    doabsems
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

   REAL    ::    OBECL,SINOB,SXLONG,ARG,DECDEG,                  &
                 DJUL,RJUL,ECCFAC
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) :: qi_temp,qc_temp
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) :: qi_save,qc_save

   REAL    ::    next_rad_time
   LOGICAL ::    run_param



   REAL, OPTIONAL, INTENT(OUT) :: DECLIN_URB  
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme), INTENT(OUT) :: COSZ_URB2D  
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme), INTENT(OUT) :: omg_urb2d   


   if (lw_physics .eq. 0 .and. sw_physics .eq. 0)         return












   IF ( (itimestep .EQ. 1) .OR. (MOD(itimestep,STEPRA) .EQ. 1 + ra_call_offset) .OR. &
        (STEPRA .EQ. 1) ) THEN
     run_param = .TRUE.
   ELSE
     run_param = .FALSE.
   ENDIF
   IF (PRESENT(adapt_step_flag)) THEN
     IF ((adapt_step_flag)) THEN
       IF ( (itimestep .EQ. 1) .OR. (radt .EQ. 0) .OR. &
           ( CURR_SECS + dt >= ( INT( CURR_SECS / ( radt * 60 ) + 1 ) * radt * 60) ) ) THEN
         run_param = .TRUE.
       ELSE
         run_param = .FALSE.
       ENDIF
     ENDIF
   ENDIF

   Radiation_step: IF ( run_param ) then


     STEPABS = nint(cam_abs_freq_s/(dt*STEPRA))*STEPRA
     IF (itimestep .eq. 1 .or. mod(itimestep,STEPABS) .eq. 1 + ra_call_offset &
                                        .or. STEPABS .eq. 1 ) THEN
       doabsems = .true.
     ELSE
       doabsems = .false.
     ENDIF
   IF (PRESENT(adapt_step_flag)) THEN
     IF ((adapt_step_flag)) THEN
       IF ( (itimestep .EQ. 1) .OR. (cam_abs_freq_s .EQ. 0) .OR. &
           ( CURR_SECS + dt >= ( INT( CURR_SECS / ( cam_abs_freq_s ) + 1 ) * cam_abs_freq_s) ) ) THEN
         doabsems = .true.
       ELSE
         doabsems = .false.
       ENDIF
     ENDIF
   ENDIF

   gfdl_lw = .false.
   gfdl_sw = .false.


   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij ,i,j,k,its,ite,jts,jte)

   DO ij = 1 , num_tiles
     its = i_start(ij)
     ite = i_end(ij)
     jts = j_start(ij)
     jte = j_end(ij)



     DO j=jts,jte
     DO i=its,ite
        GSW(I,J)=0.
        GLW(I,J)=0.
        SWDOWN(I,J)=0.
        GLAT(I,J)=XLAT(I,J)*DEGRAD
        GLON(I,J)=XLONG(I,J)*DEGRAD
     ENDDO
     ENDDO

     DO j=jts,jte
     DO k=kts,kte+1
     DO i=its,ite
        RTHRATEN(I,K,J)=0.
        RTHRATENLW(I,K,J)=0.
        RTHRATENSW(I,K,J)=0.








        CEMISS(I,K,J)=0.0
     ENDDO
     ENDDO
     ENDDO

     IF ( PRESENT( SWUPFLX ) ) THEN
        DO j=jts,jte
        DO k=kts,kte+2
        DO i=its,ite
           SWUPFLX(I,K,J) = 0.0
           SWDNFLX(I,K,J) = 0.0
           SWUPFLXC(I,K,J) = 0.0
           SWDNFLXC(I,K,J) = 0.0
           LWUPFLX(I,K,J) = 0.0
           LWDNFLX(I,K,J) = 0.0
           LWUPFLXC(I,K,J) = 0.0
           LWDNFLXC(I,K,J) = 0.0
        ENDDO
        ENDDO
        ENDDO
     ENDIF



     IF ( PRESENT( cu_rad_feedback ) ) THEN
       IF ( PRESENT( qc ) .AND. PRESENT( qc_adjust ) .AND. cu_rad_feedback ) THEN
          DO j=jts,jte
          DO k=kts,kte
          DO i=its,ite
            qc_save(i,k,j) = qc(i,k,j)
            qc(i,k,j) = qc(i,k,j) + qc_adjust(i,k,j)
          ENDDO
          ENDDO
          ENDDO
       ENDIF
       IF ( PRESENT( qi ) .AND. PRESENT( qi_adjust ) .AND. cu_rad_feedback ) THEN
          DO j=jts,jte
          DO k=kts,kte
          DO i=its,ite
            qi_save(i,k,j) = qi(i,k,j)
            qi(i,k,j) = qi(i,k,j) + qi_adjust(i,k,j)
          ENDDO
          ENDDO
          ENDDO
       ENDIF
     ENDIF



     if(PRESENT(qc) .and. PRESENT(F_QC)) then
        DO j=jts,jte
        DO k=kts,kte
        DO i=its,ite
           qc_temp(I,K,J)=qc(I,K,J)
        ENDDO
        ENDDO
        ENDDO
     else
        DO j=jts,jte
        DO k=kts,kte
        DO i=its,ite
           qc_temp(I,K,J)=0.
        ENDDO
        ENDDO
        ENDDO
     endif
     if(PRESENT(qr) .and. PRESENT(F_QR)) then
        DO j=jts,jte
        DO k=kts,kte
        DO i=its,ite
           qc_temp(I,K,J) = qc_temp(I,K,J) + qr(I,K,J)
        ENDDO
        ENDDO
        ENDDO
     endif




     CALL radconst(XTIME,DECLIN,SOLCON,JULIAN,               &
                   DEGRAD,DPD                                )


     if(present(DECLIN_URB))DECLIN_URB=DECLIN  

     lwrad_cldfra_select: SELECT CASE(lw_physics)

        CASE (GFDLLWSCHEME)




        CASE (CAMLWSCHEME)

     IF ( PRESENT ( CLDFRA ) .AND.                           &
          PRESENT(F_QC) .AND. PRESENT ( F_QI ) ) THEN


   CALL cal_cldfra2(CLDFRA,qv,qc,qi,qs,                     &
                   F_QV,F_QC,F_QI,F_QS,t,p,                &
                   F_ICE_PHY,F_RAIN_PHY,                   &
                   ids,ide, jds,jde, kds,kde,              &
                   ims,ime, jms,jme, kms,kme,              &
                   its,ite, jts,jte, kts,kte               )
     ENDIF

        CASE (RRTMG_LWSCHEME)

     IF ( PRESENT ( CLDFRA ) .AND.                           &
          PRESENT(F_QC) .AND. PRESENT ( F_QI ) ) THEN


   CALL cal_cldfra2(CLDFRA,qv,qc,qi,qs,                     &
                   F_QV,F_QC,F_QI,F_QS,t,p,                &
                   F_ICE_PHY,F_RAIN_PHY,                   &
                   ids,ide, jds,jde, kds,kde,              &
                   ims,ime, jms,jme, kms,kme,              &
                   its,ite, jts,jte, kts,kte               )
     ENDIF
 
        CASE DEFAULT

     IF ( PRESENT ( CLDFRA ) .AND.                           &
          PRESENT(F_QC) .AND. PRESENT ( F_QI ) ) THEN
       CALL cal_cldfra(CLDFRA,qc,qi,F_QC,F_QI,               &
                       ids,ide, jds,jde, kds,kde,            &
                       ims,ime, jms,jme, kms,kme,            &
                       its,ite, jts,jte, kts,kte             )
     ENDIF

     END SELECT lwrad_cldfra_select    

     lwrad_select: SELECT CASE(lw_physics)


        CASE (RRTMSCHEME)
             CALL wrf_debug (100, 'CALL rrtm')

             CALL RRTMLWRAD(                                        &
                  RTHRATEN=RTHRATEN,GLW=GLW,OLR=RLWTOA,EMISS=EMISS  &
                 ,QV3D=QV                                           &
                 ,QC3D=QC                                           &
                 ,QR3D=QR                                           &
                 ,QI3D=QI                                           &
                 ,QS3D=QS                                           &
                 ,QG3D=QG                                           &
                 ,P8W=p8w,P3D=p,PI3D=pi,DZ8W=dz8w,TSK=tsk,T3D=t     &
                 ,T8W=t8w,RHO3D=rho, CLDFRA3D=CLDFRA,R=R_d,G=G      &
                 ,F_QV=F_QV,F_QC=F_QC,F_QR=F_QR                     &
                 ,F_QI=F_QI,F_QS=F_QS,F_QG=F_QG                     &
                 ,ICLOUD=icloud,WARM_RAIN=warm_rain                 &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &     
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                    )

        CASE (GFDLLWSCHEME)

             CALL wrf_debug (100, 'CALL gfdllw')

             IF ( PRESENT(F_QV) .AND. PRESENT(F_QC) .AND.                     &
                  PRESENT(F_QS) .AND. PRESENT(qs)   .AND.                     &
                  PRESENT(qv)   .AND. PRESENT(qc)   ) THEN
               IF ( F_QV .AND. F_QC .AND. F_QS) THEN
                 gfdl_lw  = .true.
                 CALL ETARA(                                        &
                  DT=dt,XLAND=xland                                 &
                 ,P8W=p8w,DZ8W=dz8w,RHO_PHY=rho,P_PHY=p,T=t         &
                 ,QV=qv,QW=qc_temp,QI=qi,QS=qs                      &
                 ,TSK2D=tsk,GLW=GLW,RSWIN=SWDOWN,GSW=GSW            &
                 ,RSWINC=SWDOWNC,CLDFRA=CLDFRA,PI3D=pi              &
                 ,GLAT=glat,GLON=glon,HTOP=htop,HBOT=hbot           &
                 ,HBOTR=hbotr, HTOPR=htopr                          &
                 ,ALBEDO=albedo,CUPPT=cuppt                         &
                 ,VEGFRA=vegfra,SNOW=snow,G=g,GMT=gmt               &
                 ,NSTEPRA=stepra,NPHS=nphs,ITIMESTEP=itimestep      &
                 ,XTIME=xtime,JULIAN=julian                         &
                 ,COSZ_URB2D=COSZ_URB2D  ,OMG_URB2D=omg_urb2d       &
                 ,JULYR=julyr,JULDAY=julday                         &
                 ,GFDL_LW=gfdl_lw,GFDL_SW=gfdl_sw                   &
                 ,CFRACL=cfracl,CFRACM=cfracm,CFRACH=cfrach         &
                 ,ACFRST=acfrst,NCFRST=ncfrst                       &
                 ,ACFRCV=acfrcv,NCFRCV=ncfrcv                       &
                 ,RSWTOA=rswtoa,RLWTOA=rlwtoa,CZMEAN=czmean         &
                 ,THRATEN=rthraten,THRATENLW=rthratenlw             &
                 ,THRATENSW=rthratensw                              &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &     
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                    )
               ELSE
                 CALL wrf_error_fatal3("",745,&
'Can not call ETARA (1a). Missing moisture fields.')
               ENDIF
             ELSE
               CALL wrf_error_fatal3("",749,&
'Can not call ETARA (1b). Missing moisture fields.')
             ENDIF

        CASE (CAMLWSCHEME)

             CALL wrf_debug(100, 'CALL camrad lw')
             IF(cam_abs_dim1 .ne. 4 .or. cam_abs_dim2 .ne. kde .or.  &
                paerlev .ne. 29 .or. levsiz .ne. 59 )THEN
               WRITE( wrf_err_message , * ) &
'set paerlev=29, levsiz=59, cam_abs_dim1=4, and cam_abs_dim2=number of levels (e_vert) in physics namelist for CAM radiation'
               CALL wrf_error_fatal3("",760,&
wrf_err_message )
             ENDIF
             IF ( PRESENT( OZMIXM ) .AND. PRESENT( PIN ) .AND. &
                  PRESENT(M_PS_1) .AND. PRESENT(M_PS_2) .AND.  &
                  PRESENT(M_HYBI0) .AND. PRESENT(AEROSOLC_1)    &
                  .AND. PRESENT(AEROSOLC_2) ) THEN
             CALL CAMRAD(RTHRATENLW=RTHRATEN,RTHRATENSW=RTHRATENSW,    &
                     dolw=.true.,dosw=.false.,                         &
                     SWUPT=SWUPT,SWUPTC=SWUPTC,                        &
                     SWDNT=SWDNT,SWDNTC=SWDNTC,                        &
                     LWUPT=LWUPT,LWUPTC=LWUPTC,                        &
                     LWDNT=LWDNT,LWDNTC=LWDNTC,                        &
                     SWUPB=SWUPB,SWUPBC=SWUPBC,                        &
                     SWDNB=SWDNB,SWDNBC=SWDNBC,                        &
                     LWUPB=LWUPB,LWUPBC=LWUPBC,                        &
                     LWDNB=LWDNB,LWDNBC=LWDNBC,                        &
                     SWCF=SWCF,LWCF=LWCF,OLR=RLWTOA,CEMISS=CEMISS,     &
                     TAUCLDC=TAUCLDC,TAUCLDI=TAUCLDI,COSZR=COSZR,      &
                     GSW=GSW,GLW=GLW,XLAT=XLAT,XLONG=XLONG,            &
                     ALBEDO=ALBEDO,t_phy=t,TSK=TSK,EMISS=EMISS         &
                    ,QV3D=qv                                           &
                    ,QC3D=qc                                           &
                    ,QR3D=qr                                           &
                    ,QI3D=qi                                           &
                    ,QS3D=qs                                           &
                    ,QG3D=qg                                           &
                    ,F_QV=f_qv,F_QC=f_qc,F_QR=f_qr                     &
                    ,F_QI=f_qi,F_QS=f_qs,F_QG=f_qg                     &
                    ,f_ice_phy=f_ice_phy,f_rain_phy=f_rain_phy         &
                    ,p_phy=p,p8w=p8w,z=z,pi_phy=pi,rho_phy=rho,        &
                     dz8w=dz8w,                                        &
                     CLDFRA=CLDFRA,XLAND=XLAND,XICE=XICE,SNOW=SNOW,    &
                     ozmixm=ozmixm,pin0=pin,levsiz=levsiz,             &
                     num_months=n_ozmixm,                              &
                     m_psp=m_ps_1,m_psn=m_ps_2,aerosolcp=aerosolc_1,   &
                     aerosolcn=aerosolc_2,m_hybi0=m_hybi0,             &
                     paerlev=paerlev, naer_c=n_aerosolc,               &
                     cam_abs_dim1=cam_abs_dim1, cam_abs_dim2=cam_abs_dim2, &
                     GMT=GMT,JULDAY=JULDAY,JULIAN=JULIAN,YR=YR,DT=DT,XTIME=XTIME,DECLIN=DECLIN,  &
                     SOLCON=SOLCON,RADT=RADT,DEGRAD=DEGRAD,n_cldadv=3  &
                   ,abstot_3d=abstot,absnxt_3d=absnxt,emstot_3d=emstot &
                   ,doabsems=doabsems                               &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &     
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                    )
             ELSE
                CALL wrf_error_fatal3("",808,&
'arguments not present for calling cam radiation' )
             ENDIF

        CASE (RRTMG_LWSCHEME)
             CALL wrf_debug (100, 'CALL rrtmg_lw')

             CALL RRTMG_LWRAD(                                      &
                  RTHRATENLW=RTHRATEN,                              &
                  LWUPT=LWUPT,LWUPTC=LWUPTC,                        &
                  LWDNT=LWDNT,LWDNTC=LWDNTC,                        &
                  LWUPB=LWUPB,LWUPBC=LWUPBC,                        &
                  LWDNB=LWDNB,LWDNBC=LWDNBC,                        &
                  GLW=GLW,OLR=RLWTOA,LWCF=LWCF,                     &
                  EMISS=EMISS,                                      &
                  P8W=p8w,P3D=p,PI3D=pi,DZ8W=dz8w,TSK=tsk,T3D=t,    &
                  T8W=t8w,RHO3D=rho,R=R_d,G=G,                      &
                  ICLOUD=icloud,WARM_RAIN=warm_rain,CLDFRA3D=CLDFRA,&
                  XLAND=XLAND,XICE=XICE,SNOW=SNOW,                  &
                  QV3D=QV,QC3D=QC,QR3D=QR,                          &
                  QI3D=QI,QS3D=QS,QG3D=QG,                          &
                  F_QV=F_QV,F_QC=F_QC,F_QR=F_QR,                    &
                  F_QI=F_QI,F_QS=F_QS,F_QG=F_QG,                    &
                  IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde,&
                  IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme,&
                  ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte,&
                  LWUPFLX=LWUPFLX,LWUPFLXC=LWUPFLXC,                &
                  LWDNFLX=LWDNFLX,LWDNFLXC=LWDNFLXC                 &
                                                                    )

        CASE (HELDSUAREZ)
             CALL wrf_debug (100, 'CALL heldsuarez')

             CALL HSRAD(RTHRATEN,p8w,p,pi,dz8w,t,          &
                     t8w, rho, R_d,G,CP, dt, xlat, degrad, &
                     ids,ide, jds,jde, kds,kde,            &
                     ims,ime, jms,jme, kms,kme,            &
                     its,ite, jts,jte, kts,kte            )

        CASE DEFAULT
  
             WRITE( wrf_err_message , * ) 'The longwave option does not exist: lw_physics = ', lw_physics
             CALL wrf_error_fatal3("",850,&
wrf_err_message )
           
     END SELECT lwrad_select    

     IF (lw_physics .gt. 0 .and. .not.gfdl_lw) THEN
        DO j=jts,jte
        DO k=kts,kte
        DO i=its,ite
           RTHRATENLW(I,K,J)=RTHRATEN(I,K,J)

           IF(PRESENT(OLR) .AND. K .EQ. 1)OLR(I,J)=RLWTOA(I,J)
        ENDDO
        ENDDO
        ENDDO
     ENDIF

     swrad_cldfra_select: SELECT CASE(sw_physics)

        CASE (CAMSWSCHEME)

     IF ( PRESENT ( CLDFRA ) .AND.                           &
          PRESENT(F_QC) .AND. PRESENT ( F_QI ) ) THEN


   CALL cal_cldfra2(CLDFRA,qv,qc,qi,qs,                     &
                   F_QV,F_QC,F_QI,F_QS,t,p,                &
                   F_ICE_PHY,F_RAIN_PHY,                   &
                   ids,ide, jds,jde, kds,kde,              &
                   ims,ime, jms,jme, kms,kme,              &
                   its,ite, jts,jte, kts,kte               )
     ENDIF
 
        CASE (RRTMG_SWSCHEME)

     IF ( PRESENT ( CLDFRA ) .AND.                           &
          PRESENT(F_QC) .AND. PRESENT ( F_QI ) ) THEN


   CALL cal_cldfra2(CLDFRA,qv,qc,qi,qs,                     &
                   F_QV,F_QC,F_QI,F_QS,t,p,                &
                   F_ICE_PHY,F_RAIN_PHY,                   &
                   ids,ide, jds,jde, kds,kde,              &
                   ims,ime, jms,jme, kms,kme,              &
                   its,ite, jts,jte, kts,kte               )
     ENDIF

        CASE DEFAULT

     END SELECT swrad_cldfra_select    

     swrad_select: SELECT CASE(sw_physics)

        CASE (SWRADSCHEME)
             CALL wrf_debug(100, 'CALL swrad')
             CALL SWRAD(                                               &
                     DT=dt,RTHRATEN=rthraten,GSW=gsw                   &
                    ,XLAT=xlat,XLONG=xlong,ALBEDO=albedo               &
                    ,RHO_PHY=rho,T3D=t                                 &
                    ,P3D=p,PI3D=pi,DZ8W=dz8w,GMT=gmt                   &
                    ,R=r_d,CP=cp,G=g,JULDAY=julday                     &
                    ,XTIME=xtime,DECLIN=declin,SOLCON=solcon           &

                    ,RADFRQ=radt,ICLOUD=icloud,DEGRAD=degrad           &
                    ,warm_rain=warm_rain                               &
                    ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &     
                    ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                    ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                    ,COSZ_URB2D=COSZ_URB2D ,OMG_URB2D=omg_urb2d        & 
                    ,QV3D=qv                                           &
                    ,QC3D=qc                                           &
                    ,QR3D=qr                                           &
                    ,QI3D=qi                                           &
                    ,QS3D=qs                                           &
                    ,QG3D=qg                                           &
                    ,F_QV=f_qv,F_QC=f_qc,F_QR=f_qr                     &
                    ,F_QI=f_qi,F_QS=f_qs,F_QG=f_qg                     &
                    ,slope_rad=slope_rad,topo_shading=topo_shading     &
                    ,shadowmask=shadowmask                             &
                    ,ht=ht,dx=dx,dy=dy,sina=sina,cosa=cosa             )

        CASE (GSFCSWSCHEME)
             CALL wrf_debug(100, 'CALL gsfcswrad')
             CALL GSFCSWRAD(                                           &
                     RTHRATEN=rthraten,GSW=gsw,XLAT=xlat,XLONG=xlong   &
                    ,ALB=albedo,T3D=t,P3D=p,P8W3D=p8w,pi3D=pi          &
                    ,DZ8W=dz8w,RHO_PHY=rho                             &
                    ,CLDFRA3D=cldfra,RSWTOA=rswtoa                     &
                    ,GMT=gmt,CP=cp,G=g                                 &

                    ,JULDAY=julday,XTIME=xtime                         &
                    ,DECLIN=declin,SOLCON=solcon                       &
                    ,RADFRQ=radt,DEGRAD=degrad                         &
                    ,TAUCLDI=taucldi,TAUCLDC=taucldc                   &
                    ,WARM_RAIN=warm_rain                               &
                    ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &     
                    ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                    ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                    ,COSZ_URB2D=COSZ_URB2D ,OMG_URB2D=omg_urb2d        & 
                    ,QV3D=qv                                           &
                    ,QC3D=qc                                           &
                    ,QR3D=qr                                           &
                    ,QI3D=qi                                           &
                    ,QS3D=qs                                           &
                    ,QG3D=qg                                           &
                    ,QNDROP3D=qndrop                                   &
                    ,F_QV=f_qv,F_QC=f_qc,F_QR=f_qr                     &
                    ,F_QI=f_qi,F_QS=f_qs,F_QG=f_qg                     &
                    ,F_QNDROP=f_qndrop                                 &
                                                                       )
        CASE (CAMSWSCHEME)
             CALL wrf_debug(100, 'CALL camrad sw')
             IF(cam_abs_dim1 .ne. 4 .or. cam_abs_dim2 .ne. kde .or.  &
                paerlev .ne. 29 .or. levsiz .ne. 59 )THEN
               WRITE( wrf_err_message , * ) &
'set paerlev=29, levsiz=59, cam_abs_dim1=4, and cam_abs_dim2=number of levels (e_vert) in physics namelist for CAM radiation'
               CALL wrf_error_fatal3("",966,&
wrf_err_message )
             ENDIF
             IF ( PRESENT( OZMIXM ) .AND. PRESENT( PIN ) .AND. &
                  PRESENT(M_PS_1) .AND. PRESENT(M_PS_2) .AND.  &
                  PRESENT(M_HYBI0) .AND. PRESENT(AEROSOLC_1)    &
                  .AND. PRESENT(AEROSOLC_2) ) THEN
             CALL CAMRAD(RTHRATENLW=RTHRATEN,RTHRATENSW=RTHRATENSW,    &
                     dolw=.false.,dosw=.true.,                         &
                     SWUPT=SWUPT,SWUPTC=SWUPTC,                        &
                     SWDNT=SWDNT,SWDNTC=SWDNTC,                        &
                     LWUPT=LWUPT,LWUPTC=LWUPTC,                        &
                     LWDNT=LWDNT,LWDNTC=LWDNTC,                        &
                     SWUPB=SWUPB,SWUPBC=SWUPBC,                        &
                     SWDNB=SWDNB,SWDNBC=SWDNBC,                        &
                     LWUPB=LWUPB,LWUPBC=LWUPBC,                        &
                     LWDNB=LWDNB,LWDNBC=LWDNBC,                        &
                     SWCF=SWCF,LWCF=LWCF,OLR=RLWTOA,CEMISS=CEMISS,     &
                     TAUCLDC=TAUCLDC,TAUCLDI=TAUCLDI,COSZR=COSZR,      &
                     GSW=GSW,GLW=GLW,XLAT=XLAT,XLONG=XLONG,            &
                     ALBEDO=ALBEDO,t_phy=t,TSK=TSK,EMISS=EMISS         &
                    ,QV3D=qv                                           &
                    ,QC3D=qc                                           &
                    ,QR3D=qr                                           &
                    ,QI3D=qi                                           &
                    ,QS3D=qs                                           &
                    ,QG3D=qg                                           &
                    ,F_QV=f_qv,F_QC=f_qc,F_QR=f_qr                     &
                    ,F_QI=f_qi,F_QS=f_qs,F_QG=f_qg                     &
                    ,f_ice_phy=f_ice_phy,f_rain_phy=f_rain_phy         &
                    ,p_phy=p,p8w=p8w,z=z,pi_phy=pi,rho_phy=rho,        &
                     dz8w=dz8w,                                        &
                     CLDFRA=CLDFRA,XLAND=XLAND,XICE=XICE,SNOW=SNOW,    &
                     ozmixm=ozmixm,pin0=pin,levsiz=levsiz,             &
                     num_months=n_ozmixm,                              &
                     m_psp=m_ps_1,m_psn=m_ps_2,aerosolcp=aerosolc_1,   &
                     aerosolcn=aerosolc_2,m_hybi0=m_hybi0,             &
                     paerlev=paerlev, naer_c=n_aerosolc,               &
                     cam_abs_dim1=cam_abs_dim1, cam_abs_dim2=cam_abs_dim2, &
                     GMT=GMT,JULDAY=JULDAY,JULIAN=JULIAN,YR=YR,DT=DT,XTIME=XTIME,DECLIN=DECLIN,  &
                     SOLCON=SOLCON,RADT=RADT,DEGRAD=DEGRAD,n_cldadv=3  &
                   ,abstot_3d=abstot,absnxt_3d=absnxt,emstot_3d=emstot &
                   ,doabsems=doabsems                               &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &     
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                    )
             ELSE
                CALL wrf_error_fatal3("",1014,&
'arguments not present for calling cam radiation' )
             ENDIF
             DO j=jts,jte
             DO k=kts,kte
             DO i=its,ite
                RTHRATEN(I,K,J)=RTHRATEN(I,K,J)+RTHRATENSW(I,K,J)
             ENDDO
             ENDDO
             ENDDO

        CASE (RRTMG_SWSCHEME)
             CALL wrf_debug(100, 'CALL rrtmg_sw')
             CALL RRTMG_SWRAD(                                         &
                     RTHRATENSW=RTHRATENSW,                            &
                     SWUPT=SWUPT,SWUPTC=SWUPTC,                        &
                     SWDNT=SWDNT,SWDNTC=SWDNTC,                        &
                     SWUPB=SWUPB,SWUPBC=SWUPBC,                        &
                     SWDNB=SWDNB,SWDNBC=SWDNBC,                        &
                     SWCF=SWCF,GSW=GSW,                                &
                     XTIME=XTIME,GMT=GMT,XLAT=XLAT,XLONG=XLONG,        &
                     RADT=RADT,DEGRAD=DEGRAD,DECLIN=DECLIN,            &
                     COSZR=COSZR,JULDAY=JULDAY,SOLCON=SOLCON,          &
                     ALBEDO=ALBEDO,t3d=t,t8w=t8w,TSK=TSK,              &
                     p3d=p,p8w=p8w,pi3d=pi,rho3d=rho,                  &
                     dz8w=dz8w,CLDFRA3D=CLDFRA,R=R_D,G=G,              &
                     ICLOUD=icloud,WARM_RAIN=warm_rain,                &
                     XLAND=XLAND,XICE=XICE,SNOW=SNOW,                  &
                     QV3D=qv,QC3D=qc,QR3D=qr,                          &
                     QI3D=qi,QS3D=qs,QG3D=qg,                          &
                     F_QV=f_qv,F_QC=f_qc,F_QR=f_qr,                    &
                     F_QI=f_qi,F_QS=f_qs,F_QG=f_qg,                    &
                     IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde,&
                     IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme,&
                     ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte,&
                     SWUPFLX=SWUPFLX,SWUPFLXC=SWUPFLXC,                &
                     SWDNFLX=SWDNFLX,SWDNFLXC=SWDNFLXC                 &
                                                                       )
             DO j=jts,jte
             DO k=kts,kte
             DO i=its,ite
                RTHRATEN(I,K,J)=RTHRATEN(I,K,J)+RTHRATENSW(I,K,J)
             ENDDO
             ENDDO
             ENDDO

        CASE (GFDLSWSCHEME)

             CALL wrf_debug (100, 'CALL gfdlsw')

             IF ( PRESENT(F_QV) .AND. PRESENT(F_QC) .AND.                     &
                  PRESENT(F_QS) .AND. PRESENT(qs)   .AND.                     &
                  PRESENT(qv)   .AND. PRESENT(qc) ) THEN
               IF ( F_QV .AND. F_QC .AND. F_QS ) THEN
                 gfdl_sw = .true.
                 CALL ETARA(                                        &
                  DT=dt,XLAND=xland                                 &
                 ,P8W=p8w,DZ8W=dz8w,RHO_PHY=rho,P_PHY=p,T=t         &
                 ,QV=qv,QW=qc_temp,QI=qi,QS=qs                      &
                 ,TSK2D=tsk,GLW=GLW,RSWIN=SWDOWN,GSW=GSW            &
                 ,RSWINC=SWDOWNC,CLDFRA=CLDFRA,PI3D=pi              &
                 ,GLAT=glat,GLON=glon,HTOP=htop,HBOT=hbot           &
                 ,HBOTR=hbotr, HTOPR=htopr                          &
                 ,ALBEDO=albedo,CUPPT=cuppt                         &
                 ,VEGFRA=vegfra,SNOW=snow,G=g,GMT=gmt               &
                 ,NSTEPRA=stepra,NPHS=nphs,ITIMESTEP=itimestep      &
                 ,XTIME=xtime,JULIAN=julian                         &
                 ,COSZ_URB2D=COSZ_URB2D ,OMG_URB2D=omg_urb2d        &
                 ,JULYR=julyr,JULDAY=julday                         &
                 ,GFDL_LW=gfdl_lw,GFDL_SW=gfdl_sw                   &
                 ,CFRACL=cfracl,CFRACM=cfracm,CFRACH=cfrach         &
                 ,ACFRST=acfrst,NCFRST=ncfrst                       &
                 ,ACFRCV=acfrcv,NCFRCV=ncfrcv                       &
                 ,RSWTOA=rswtoa,RLWTOA=rlwtoa,CZMEAN=czmean         &
                 ,THRATEN=rthraten,THRATENLW=rthratenlw             &
                 ,THRATENSW=rthratensw                              &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &     
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                    )
               ELSE
                 CALL wrf_error_fatal3("",1095,&
'Can not call ETARA (2a). Missing moisture fields.')
               ENDIF
             ELSE
               CALL wrf_error_fatal3("",1099,&
'Can not call ETARA (2b). Missing moisture fields.')
             ENDIF

        CASE (0)

           
           
           IF (lw_physics /= HELDSUAREZ) THEN
             WRITE( wrf_err_message , * ) &
'You have selected a longwave radiation option, but not a shortwave option (sw_physics = 0, lw_physics = ',lw_physics,')'
             CALL wrf_error_fatal3("",1110,&
wrf_err_message )
           END IF

        CASE DEFAULT

             WRITE( wrf_err_message , * ) 'The shortwave option does not exist: sw_physics = ', sw_physics
             CALL wrf_error_fatal3("",1117,&
wrf_err_message )

     END SELECT swrad_select    

     IF (sw_physics .gt. 0 .and. .not.gfdl_sw) THEN
        DO j=jts,jte
        DO k=kts,kte
        DO i=its,ite
           RTHRATENSW(I,K,J)=RTHRATEN(I,K,J)-RTHRATENLW(I,K,J)
        ENDDO
        ENDDO
        ENDDO

        DO j=jts,jte
        DO i=its,ite
           SWDOWN(I,J)=GSW(I,J)/(1.-ALBEDO(I,J))
        ENDDO
        ENDDO

     ENDIF

     IF ( PRESENT( cu_rad_feedback ) ) THEN
       IF ( PRESENT( qc  ) .AND. PRESENT( qc_adjust ) .AND. cu_rad_feedback ) THEN
           DO j=jts,jte
           DO k=kts,kte
           DO i=its,ite
             qc(i,k,j) = qc_save(i,k,j)
           ENDDO
           ENDDO
           ENDDO
        ENDIF
        IF ( PRESENT( qi  ) .AND. PRESENT( qi_adjust ) .AND. cu_rad_feedback ) THEN
           DO j=jts,jte
           DO k=kts,kte
           DO i=its,ite
             qi(i,k,j) = qi_save(i,k,j)
           ENDDO
           ENDDO
           ENDDO
        ENDIF
      ENDIF

   ENDDO
   !$OMP END PARALLEL DO

   ENDIF Radiation_step

     accumulate_lw_select: SELECT CASE(lw_physics)

     CASE (CAMLWSCHEME,RRTMG_LWSCHEME)
   IF(PRESENT(LWUPTC))THEN
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij ,i,j,k,its,ite,jts,jte)

   DO ij = 1 , num_tiles
     its = i_start(ij)
     ite = i_end(ij)
     jts = j_start(ij)
     jte = j_end(ij)

        DO j=jts,jte
        DO i=its,ite
           ACLWUPT(I,J) = ACLWUPT(I,J) + LWUPT(I,J)*DT
           ACLWUPTC(I,J) = ACLWUPTC(I,J) + LWUPTC(I,J)*DT
           ACLWDNT(I,J) = ACLWDNT(I,J) + LWDNT(I,J)*DT
           ACLWDNTC(I,J) = ACLWDNTC(I,J) + LWDNTC(I,J)*DT
           ACLWUPB(I,J) = ACLWUPB(I,J) + LWUPB(I,J)*DT
           ACLWUPBC(I,J) = ACLWUPBC(I,J) + LWUPBC(I,J)*DT
           ACLWDNB(I,J) = ACLWDNB(I,J) + LWDNB(I,J)*DT
           ACLWDNBC(I,J) = ACLWDNBC(I,J) + LWDNBC(I,J)*DT
        ENDDO
        ENDDO
   ENDDO
   !$OMP END PARALLEL DO
   ENDIF
     CASE DEFAULT
     END SELECT accumulate_lw_select

     accumulate_sw_select: SELECT CASE(sw_physics)

     CASE (CAMSWSCHEME,RRTMG_SWSCHEME)
   IF(PRESENT(SWUPTC))THEN
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij ,i,j,k,its,ite,jts,jte)

   DO ij = 1 , num_tiles
     its = i_start(ij)
     ite = i_end(ij)
     jts = j_start(ij)
     jte = j_end(ij)

        DO j=jts,jte
        DO i=its,ite
           ACSWUPT(I,J) = ACSWUPT(I,J) + SWUPT(I,J)*DT
           ACSWUPTC(I,J) = ACSWUPTC(I,J) + SWUPTC(I,J)*DT
           ACSWDNT(I,J) = ACSWDNT(I,J) + SWDNT(I,J)*DT
           ACSWDNTC(I,J) = ACSWDNTC(I,J) + SWDNTC(I,J)*DT
           ACSWUPB(I,J) = ACSWUPB(I,J) + SWUPB(I,J)*DT
           ACSWUPBC(I,J) = ACSWUPBC(I,J) + SWUPBC(I,J)*DT
           ACSWDNB(I,J) = ACSWDNB(I,J) + SWDNB(I,J)*DT
           ACSWDNBC(I,J) = ACSWDNBC(I,J) + SWDNBC(I,J)*DT
        ENDDO
        ENDDO
   ENDDO
   !$OMP END PARALLEL DO
   ENDIF

     CASE DEFAULT
     END SELECT accumulate_sw_select

   END SUBROUTINE radiation_driver

   SUBROUTINE pre_radiation_driver ( grid, config_flags                   &
              ,itimestep, ra_call_offset                                  &
              ,XLAT, XLONG, GMT, julian, xtime, RADT, STEPRA              &
              ,ht,dx,dy,sina,cosa,shadowmask,slope_rad ,topo_shading      &
              ,shadlen,ht_shad,ht_loc                                     &
              ,ht_shad_bxs, ht_shad_bxe                                   &
              ,ht_shad_bys, ht_shad_bye                                   &
              ,nested, min_ptchsz                                         &
              ,spec_bdy_width                                             &
              ,ids, ide, jds, jde, kds, kde                               &
              ,ims, ime, jms, jme, kms, kme                               &
              ,ips, ipe, jps, jpe, kps, kpe                               &
              ,i_start, i_end                                             &
              ,j_start, j_end                                             &
              ,kts, kte                                                   &
              ,num_tiles                                                  )

   USE module_domain
   USE module_dm
   USE module_bc
   USE module_model_constants

   IMPLICIT NONE

   INTEGER,      INTENT(IN   )    ::   ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       ips,ipe, jps,jpe, kps,kpe, &
                                                         kts,kte, &
                                       num_tiles

   TYPE(domain)                   , INTENT(INOUT)  :: grid
   TYPE(grid_config_rec_type   ) ,   INTENT(IN   ) :: config_flags

   INTEGER, INTENT(IN  ) :: itimestep, ra_call_offset, stepra,    &
                            slope_rad, topo_shading,              &
                            spec_bdy_width

   INTEGER, INTENT(INOUT) :: min_ptchsz

   INTEGER, DIMENSION(num_tiles), INTENT(IN) ::                   &
                i_start,i_end,j_start,j_end

   REAL, INTENT(IN  )   :: GMT, radt, julian, xtime, dx, dy, shadlen

   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(IN   )  ::                                  XLAT, &
                                                           XLONG, &
                                                              HT, &
                                                            SINA, &
                                                            COSA

   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)  ::  ht_shad,ht_loc

   REAL,  DIMENSION( jms:jme , kds:kde , spec_bdy_width ),        &
                      INTENT(IN   ) :: ht_shad_bxs, ht_shad_bxe
   REAL,  DIMENSION( ims:ime , kds:kde , spec_bdy_width ),        &
                      INTENT(IN   ) :: ht_shad_bys, ht_shad_bye

   INTEGER, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(INOUT)  ::                            shadowmask

   LOGICAL,      INTENT(IN   )    :: nested



   INTEGER :: niter,ni,psx,psy,idum,jdum,i,j,ij
   REAL :: DECLIN,SOLCON


   if (itimestep .eq. 1) then
     psx = ipe-ips+1
     psy = jpe-jps+1
     min_ptchsz = min(psx,psy)
     idum = 0
     jdum = 0
   endif

   if (itimestep .eq. 1) then
     call wrf_dm_minval_integer (psx,idum,jdum)
     call wrf_dm_minval_integer (psy,idum,jdum)
     min_ptchsz = min(psx,psy)
   endif


   
   if ((topo_shading.eq.1).and.(itimestep .eq. 1 .or. &
        mod(itimestep,STEPRA) .eq. 1 + ra_call_offset))  then



   
   CALL radconst(XTIME,DECLIN,SOLCON,JULIAN,DEGRAD,DPD)
   

     do j=jms,jme
     do i=ims,ime
       ht_loc(i,j) = ht(i,j)
     enddo
     enddo

     if ((ids.eq.ips).and.(ide.eq.ipe).and.(jds.eq.jps).and.(jde.eq.jpe)) then
       niter = 1
     else
       niter = int(shadlen/(dx*min_ptchsz)+3)
     endif



    IF( nested ) THEN

      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij )

      DO ij = 1 , num_tiles

           CALL spec_bdyfield(ht_shad,                         &
                               ht_shad_bxs, ht_shad_bxe,       &
                               ht_shad_bys, ht_shad_bye,       &
                               'm', config_flags, spec_bdy_width, 2,&
                               ids,ide, jds,jde, 1  ,1  ,  & 
                               ims,ime, jms,jme, 1  ,1  ,  & 
                               ips,ipe, jps,jpe, 1  ,1  ,  & 
                               i_start(ij), i_end(ij),         &
                               j_start(ij), j_end(ij),         &
                               1    , 1             )
      ENDDO
    ENDIF

     do ni = 1, niter

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij,i,j )
         do ij = 1 , num_tiles

         call toposhad_init (ht_shad,ht_loc,                         &
                       shadowmask,nested,ni,                         &
                       ids,ide, jds,jde, kds,kde,                    &
                       ims,ime, jms,jme, kms,kme,                    &
                       ips,min(ipe,ide-1), jps,min(jpe,jde-1), kps,kpe,      &
                       i_start(ij),min(i_end(ij), ide-1),j_start(ij),&
                       min(j_end(ij), jde-1), kts, kte               )

         enddo
   !$OMP END PARALLEL DO


   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij,i,j )
       do ij = 1 , num_tiles

       call toposhad (xlat,xlong,sina,cosa,xtime,gmt,radt,declin,    &
                       dx,dy,ht_shad,ht_loc,ni,                      &
                       shadowmask,shadlen,                           &
                       ids,ide, jds,jde, kds,kde,                    &
                       ims,ime, jms,jme, kms,kme,                    &
                       ips,min(ipe,ide-1), jps,min(jpe,jde-1), kps,kpe,        &
                       i_start(ij),min(i_end(ij), ide-1),j_start(ij),&
                       min(j_end(ij), jde-1), kts, kte               )

       enddo
   !$OMP END PARALLEL DO

     enddo
   endif

   END SUBROUTINE pre_radiation_driver





   SUBROUTINE radconst(XTIME,DECLIN,SOLCON,JULIAN,                   &
                       DEGRAD,DPD                                    )

   USE module_wrf_error
   IMPLICIT NONE



   REAL, INTENT(IN   )      ::       DEGRAD,DPD,XTIME,JULIAN
   REAL, INTENT(OUT  )      ::       DECLIN,SOLCON
   REAL                     ::       OBECL,SINOB,SXLONG,ARG,  &
                                     DECDEG,DJUL,RJUL,ECCFAC







   DECLIN=0.
   SOLCON=0.


        
   OBECL=23.5*DEGRAD
   SINOB=SIN(OBECL)
        

        
   IF(JULIAN.GE.80.)SXLONG=DPD*(JULIAN-80.)
   IF(JULIAN.LT.80.)SXLONG=DPD*(JULIAN+285.)
   SXLONG=SXLONG*DEGRAD
   ARG=SINOB*SIN(SXLONG)
   DECLIN=ASIN(ARG)
   DECDEG=DECLIN/DEGRAD

   DJUL=JULIAN*360./365.
   RJUL=DJUL*DEGRAD
   ECCFAC=1.000110+0.034221*COS(RJUL)+0.001280*SIN(RJUL)+0.000719*  &
          COS(2*RJUL)+0.000077*SIN(2*RJUL)
   SOLCON=1370.*ECCFAC
   
   END SUBROUTINE radconst





   SUBROUTINE cal_cldfra(CLDFRA,QC,QI,F_QC,F_QI,                     &
          ids,ide, jds,jde, kds,kde,                                 &
          ims,ime, jms,jme, kms,kme,                                 &
          its,ite, jts,jte, kts,kte                                  )

   IMPLICIT NONE

   INTEGER,  INTENT(IN   )   ::           ids,ide, jds,jde, kds,kde, &
                                          ims,ime, jms,jme, kms,kme, &
                                          its,ite, jts,jte, kts,kte


   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(OUT  ) ::    &
                                                             CLDFRA

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::    &
                                                                 QI, &
                                                                 QC

   LOGICAL,INTENT(IN) :: F_QC,F_QI

   REAL thresh
   INTEGER:: i,j,k















     thresh=1.0e-6

     IF ( f_qi .AND. f_qc ) THEN
        DO j = jts,jte
        DO k = kts,kte
        DO i = its,ite
           IF ( QC(i,k,j)+QI(I,k,j) .gt. thresh) THEN
              CLDFRA(i,k,j)=1.
           ELSE
              CLDFRA(i,k,j)=0.
           ENDIF
        ENDDO
        ENDDO
        ENDDO
     ELSE IF ( f_qc ) THEN
        DO j = jts,jte
        DO k = kts,kte
        DO i = its,ite
           IF ( QC(i,k,j) .gt. thresh) THEN
              CLDFRA(i,k,j)=1.
           ELSE
              CLDFRA(i,k,j)=0.
           ENDIF
        ENDDO
        ENDDO
        ENDDO
     ELSE
        DO j = jts,jte
        DO k = kts,kte
        DO i = its,ite
           CLDFRA(i,k,j)=0.
        ENDDO
        ENDDO
        ENDDO
     ENDIF

   END SUBROUTINE cal_cldfra











   SUBROUTINE cal_cldfra2(CLDFRA, QV, QC, QI, QS,                     &
                         F_QV, F_QC, F_QI, F_QS, t_phy, p_phy,       &
                         F_ICE_PHY,F_RAIN_PHY,                       &
          ids,ide, jds,jde, kds,kde,                                 &
          ims,ime, jms,jme, kms,kme,                                 &
          its,ite, jts,jte, kts,kte                                  )

   IMPLICIT NONE

   INTEGER,  INTENT(IN   )   ::           ids,ide, jds,jde, kds,kde, &
                                          ims,ime, jms,jme, kms,kme, &
                                          its,ite, jts,jte, kts,kte


   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(OUT  ) ::    &
                                                             CLDFRA

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::    &
                                                                 QV, &
                                                                 QI, &
                                                                 QC, &
                                                                 QS, &
                                                              t_phy, &
                                                              p_phy, &
                                                          F_ICE_PHY, &
                                                         F_RAIN_PHY

   LOGICAL,INTENT(IN) :: F_QC,F_QI,F_QV,F_QS


   INTEGER:: i,j,k
   REAL    :: RHUM, tc, esw, esi, weight, qvsw, qvsi, qvs_weight, QIMID, QWMID, QCLD, DENOM, ARG, SUBSAT

   REAL    ,PARAMETER :: ALPHA0=100., GAMMA=0.49, QCLDMIN=1.E-12,    &
                                        PEXP=0.25, RHGRID=1.0
   REAL    , PARAMETER ::  SVP1=0.61078
   REAL    , PARAMETER ::  SVP2=17.2693882
   REAL    , PARAMETER ::  SVPI2=21.8745584
   REAL    , PARAMETER ::  SVP3=35.86
   REAL    , PARAMETER ::  SVPI3=7.66
   REAL    , PARAMETER ::  SVPT0=273.15
   REAL    , PARAMETER ::  r_d = 287.
   REAL    , PARAMETER ::  r_v = 461.6
   REAL    , PARAMETER ::  ep_2=r_d/r_v









































    DO j = jts,jte
    DO k = kts,kte
    DO i = its,ite
      tc         = t_phy(i,k,j) - SVPT0
      esw     = 1000.0 * SVP1 * EXP( SVP2  * tc / ( t_phy(i,k,j) - SVP3  ) )
      esi     = 1000.0 * SVP1 * EXP( SVPI2 * tc / ( t_phy(i,k,j) - SVPI3 ) )
      QVSW = EP_2 * esw / ( p_phy(i,k,j) - esw )
      QVSI = EP_2 * esi / ( p_phy(i,k,j) - esi )

      IF ( F_QI .and. F_QC .and. F_QS) THEN
        QCLD=QI(i,k,j)+QC(i,k,j)+QS(I,k,j)
        IF (QCLD .LT. QCLDMIN) THEN
          weight = 0.
        ELSE
          weight = (QI(i,k,j)+QS(I,k,j)) / QCLD
        ENDIF
      ELSE IF ( F_QC ) THEN






      QIMID=QC(i,k,j)*F_ICE_PHY(i,k,j)
      QWMID=(QC(i,k,j)-QIMID)*(1.-F_RAIN_PHY(i,k,j))






      QCLD=QWMID+QIMID
        IF (QCLD .LT. QCLDMIN) THEN
          weight = 0.
        ELSE
          weight = F_ICE_PHY(i,k,j)
        ENDIF

      ELSE
        CLDFRA(i,k,j)=0.
      ENDIF 


      QVS_WEIGHT = (1-weight)*QVSW + weight*QVSI
      RHUM=QV(i,k,j)/QVS_WEIGHT   



      IF (QCLD .LT. QCLDMIN) THEN



        CLDFRA(i,k,j)=0.
      ELSEIF(RHUM.GE.RHGRID)THEN




        CLDFRA(i,k,j)=1.
      ELSE




        SUBSAT=MAX(1.E-10,RHGRID*QVS_WEIGHT-QV(i,k,j))
        DENOM=(SUBSAT)**GAMMA
        ARG=MAX(-6.9, -ALPHA0*QCLD/DENOM)    

        RHUM=MAX(1.E-10, RHUM)
        CLDFRA(i,k,j)=(RHUM/RHGRID)**PEXP*(1.-EXP(ARG))



        IF (CLDFRA(i,k,j) .LT. .01) CLDFRA(i,k,j)=0.
      ENDIF          
    ENDDO          
    ENDDO          
    ENDDO          

   END SUBROUTINE cal_cldfra2


   SUBROUTINE toposhad_init(ht_shad,ht_loc,shadowmask,nested,iter,   &
                       ids,ide, jds,jde, kds,kde,                    & 
                       ims,ime, jms,jme, kms,kme,                    &
                       ips,ipe, jps,jpe, kps,kpe,                    &
                       its,ite, jts,jte, kts,kte                     )

   USE module_model_constants

 implicit none

   INTEGER,    INTENT(IN) ::           ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       ips,ipe, jps,jpe, kps,kpe, &
                                       its,ite, jts,jte, kts,kte

   LOGICAL, INTENT(IN)      :: nested

   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)  ::  ht_shad, ht_loc

   INTEGER, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)  :: shadowmask
   INTEGER, INTENT(IN)      :: iter



   INTEGER :: i, j

 if (iter.eq.1) then


   do j=jts,jte
   do i=its,ite
     shadowmask(i,j) = 0
   ENDDO
   ENDDO



   IF ( nested ) THEN  
     do j=max(jts,jds+2),min(jte,jde-3)
     do i=max(its,ids+2),min(ite,ide-3)
       ht_shad(i,j) = ht_loc(i,j)-0.001
     ENDDO
     ENDDO
   ELSE
     do j=jts,jte
     do i=its,ite
       ht_shad(i,j) = ht_loc(i,j)-0.001
     ENDDO
     ENDDO
   ENDIF

   IF ( nested ) THEN  
     if (its.eq.ids) then
       do j=jts,jte
         if (ht_shad(its,j) .gt. ht_loc(its,j)) then
           shadowmask(its,j) = 1
           ht_loc(its,j) = ht_shad(its,j)
         endif
         if (ht_shad(its+1,j) .gt. ht_loc(its+1,j)) then
           shadowmask(its+1,j) = 1
           ht_loc(its+1,j) = ht_shad(its+1,j)
         endif
       enddo
     endif
     if (ite.eq.ide-1) then
       do j=jts,jte
         if (ht_shad(ite,j) .gt. ht_loc(ite,j)) then
           shadowmask(ite,j) = 1
           ht_loc(ite,j) = ht_shad(ite,j)
         endif
         if (ht_shad(ite-1,j) .gt. ht_loc(ite-1,j)) then
           shadowmask(ite-1,j) = 1
           ht_loc(ite-1,j) = ht_shad(ite-1,j)
         endif
       enddo
     endif
     if (jts.eq.jds) then
       do i=its,ite
         if (ht_shad(i,jts) .gt. ht_loc(i,jts)) then
           shadowmask(i,jts) = 1
           ht_loc(i,jts) = ht_shad(i,jts)
         endif
         if (ht_shad(i,jts+1) .gt. ht_loc(i,jts+1)) then
           shadowmask(i,jts+1) = 1
           ht_loc(i,jts+1) = ht_shad(i,jts+1)
         endif
       enddo
     endif
     if (jte.eq.jde-1) then
       do i=its,ite
         if (ht_shad(i,jte) .gt. ht_loc(i,jte)) then
           shadowmask(i,jte) = 1
           ht_loc(i,jte) = ht_shad(i,jte)
         endif
         if (ht_shad(i,jte-1) .gt. ht_loc(i,jte-1)) then
           shadowmask(i,jte-1) = 1
           ht_loc(i,jte-1) = ht_shad(i,jte-1)
         endif
       enddo
     endif
   ENDIF

 else




   if ((its.ne.ids).and.(its.eq.ips)) then
     do j=jts-2,jte+2
       ht_loc(its-1,j) = max(ht_loc(its-1,j),ht_shad(its-1,j))
       ht_loc(its-2,j) = max(ht_loc(its-2,j),ht_shad(its-2,j))
     enddo
   endif
   if ((ite.ne.ide-1).and.(ite.eq.ipe)) then
     do j=jts-2,jte+2
       ht_loc(ite+1,j) = max(ht_loc(ite+1,j),ht_shad(ite+1,j))
       ht_loc(ite+2,j) = max(ht_loc(ite+2,j),ht_shad(ite+2,j))
     enddo
   endif
   if ((jts.ne.jds).and.(jts.eq.jps)) then
     do i=its-2,ite+2
       ht_loc(i,jts-1) = max(ht_loc(i,jts-1),ht_shad(i,jts-1))
       ht_loc(i,jts-2) = max(ht_loc(i,jts-2),ht_shad(i,jts-2))
     enddo
   endif
   if ((jte.ne.jde-1).and.(jte.eq.jpe)) then
     do i=its-2,ite+2
       ht_loc(i,jte+1) = max(ht_loc(i,jte+1),ht_shad(i,jte+1))
       ht_loc(i,jte+2) = max(ht_loc(i,jte+2),ht_shad(i,jte+2))
     enddo
   endif

 endif

   END SUBROUTINE toposhad_init




   SUBROUTINE toposhad(xlat,xlong,sina,cosa,xtime,gmt,radfrq,declin, &
                       dx,dy,ht_shad,ht_loc,iter,                    &
                       shadowmask,shadlen,                    &
                       ids,ide, jds,jde, kds,kde,                    & 
                       ims,ime, jms,jme, kms,kme,                    &
                       ips,ipe, jps,jpe, kps,kpe,                    &
                       its,ite, jts,jte, kts,kte                     )


   USE module_model_constants

 implicit none

   INTEGER,    INTENT(IN) ::           ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       ips,ipe, jps,jpe, kps,kpe, &
                                       its,ite, jts,jte, kts,kte

   INTEGER,   INTENT(IN) ::      iter

   REAL, INTENT(IN)      ::        RADFRQ,XTIME,DECLIN,dx,dy,gmt,shadlen

   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN)  :: XLAT, XLONG, sina, cosa

   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)  ::  ht_shad,ht_loc

   INTEGER, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)  :: shadowmask



   REAL :: pi, xt24, wgt, ri, rj, argu, sol_azi, topoelev, dxabs, tloctm, hrang, xxlat, csza
   INTEGER :: gpshad, ii, jj, i1, i2, j1, j2, i, j



 XT24=MOD(XTIME+RADFRQ*0.5,1440.)
 pi = 4.*atan(1.)
 gpshad = int(shadlen/dx+1.)

 if (iter.eq.1) then  


   j_loop1: DO J=jts,jte
   i_loop1: DO I=its,ite

     TLOCTM=GMT+XT24/60.+XLONG(i,j)/15.
     HRANG=15.*(TLOCTM-12.)*DEGRAD
     XXLAT=XLAT(i,j)*DEGRAD
     CSZA=SIN(XXLAT)*SIN(DECLIN)+COS(XXLAT)*COS(DECLIN)*COS(HRANG)

     if (csza.lt.1.e-2) then   
     shadowmask(i,j) = 0
     ht_shad(i,j) = ht_loc(i,j)-0.001
     goto 120
     endif



     argu=(csza*sin(XXLAT)-sin(DECLIN))/(sin(acos(csza))*cos(XXLAT))
     if (argu.gt.1) argu = 1
     if (argu.lt.-1) argu = -1
     sol_azi = sign(acos(argu),sin(HRANG))+pi  
     if (cosa(i,j).ge.0) then
       sol_azi = sol_azi + asin(sina(i,j))  
     else
       sol_azi = sol_azi + pi - asin(sina(i,j)) 
     endif



          if ((sol_azi.gt.1.75*pi).or.(sol_azi.lt.0.25*pi)) then 

            do jj = j+1,j+gpshad
              ri = i + (jj-j)*tan(sol_azi)
              i1 = int(ri) 
              i2 = i1+1
              wgt = ri-i1
              dxabs = sqrt((dy*(jj-j))**2+(dx*(ri-i))**2)
              if ((jj.ge.jpe+1).or.(i1.le.ips-1).or.(i2.ge.ipe+1)) then
                if (shadowmask(i,j).eq.0) shadowmask(i,j) = -1
                goto 120
              endif
              topoelev=atan((wgt*ht_loc(i2,jj)+(1.-wgt)*ht_loc(i1,jj)-ht_loc(i,j))/dxabs)
              if (sin(topoelev).ge.csza) then
                shadowmask(i,j) = 1
                ht_shad(i,j) = max(ht_shad(i,j),ht_loc(i,j)+dxabs*(tan(topoelev)-tan(asin(csza))))
              endif
            enddo

          else if (sol_azi.lt.0.75*pi) then  
            do ii = i+1,i+gpshad
              rj = j - (ii-i)*tan(pi/2.+sol_azi)
              j1 = int(rj)
              j2 = j1+1
              wgt = rj-j1
              dxabs = sqrt((dx*(ii-i))**2+(dy*(rj-j))**2)
              if ((ii.ge.ipe+1).or.(j1.le.jps-1).or.(j2.ge.jpe+1)) then
                if (shadowmask(i,j).eq.0) shadowmask(i,j) = -1
                goto 120
              endif
              topoelev=atan((wgt*ht_loc(ii,j2)+(1.-wgt)*ht_loc(ii,j1)-ht_loc(i,j))/dxabs)
              if (sin(topoelev).ge.csza) then
                shadowmask(i,j) = 1
                ht_shad(i,j) = max(ht_shad(i,j),ht_loc(i,j)+dxabs*(tan(topoelev)-tan(asin(csza))))
              endif
            enddo

          else if (sol_azi.lt.1.25*pi) then 
            do jj = j-1,j-gpshad,-1
              ri = i + (jj-j)*tan(sol_azi)
              i1 = int(ri)
              i2 = i1+1
              wgt = ri-i1
              dxabs = sqrt((dy*(jj-j))**2+(dx*(ri-i))**2)
              if ((jj.le.jps-1).or.(i1.le.ips-1).or.(i2.ge.ipe+1)) then
                if (shadowmask(i,j).eq.0) shadowmask(i,j) = -1
                goto 120
              endif
              topoelev=atan((wgt*ht_loc(i2,jj)+(1.-wgt)*ht_loc(i1,jj)-ht_loc(i,j))/dxabs)
              if (sin(topoelev).ge.csza) then
                shadowmask(i,j) = 1
                ht_shad(i,j) = max(ht_shad(i,j),ht_loc(i,j)+dxabs*(tan(topoelev)-tan(asin(csza))))
              endif
            enddo

          else                          
            do ii = i-1,i-gpshad,-1
              rj = j - (ii-i)*tan(pi/2.+sol_azi)
              j1 = int(rj)
              j2 = j1+1
              wgt = rj-j1
              dxabs = sqrt((dx*(ii-i))**2+(dy*(rj-j))**2)
              if ((ii.le.ips-1).or.(j1.le.jps-1).or.(j2.ge.jpe+1)) then
                if (shadowmask(i,j).eq.0) shadowmask(i,j) = -1
                goto 120
              endif
              topoelev=atan((wgt*ht_loc(ii,j2)+(1.-wgt)*ht_loc(ii,j1)-ht_loc(i,j))/dxabs)
              if (sin(topoelev).ge.csza) then
                shadowmask(i,j) = 1
                ht_shad(i,j) = max(ht_shad(i,j),ht_loc(i,j)+dxabs*(tan(topoelev)-tan(asin(csza))))
              endif
            enddo
          endif

 120      continue

   ENDDO i_loop1
   ENDDO j_loop1

 else   


   j_loop2: DO J=jts,jte
   i_loop2: DO I=its,ite



       TLOCTM=GMT+XT24/60.+XLONG(i,j)/15.
       HRANG=15.*(TLOCTM-12.)*DEGRAD
       XXLAT=XLAT(i,j)*DEGRAD
       CSZA=SIN(XXLAT)*SIN(DECLIN)+COS(XXLAT)*COS(DECLIN)*COS(HRANG)



       argu=(csza*sin(XXLAT)-sin(DECLIN))/(sin(acos(csza))*cos(XXLAT))
       if (argu.gt.1) argu = 1
       if (argu.lt.-1) argu = -1
       sol_azi = sign(acos(argu),sin(HRANG))+pi  
       if (cosa(i,j).ge.0) then
         sol_azi = sol_azi + asin(sina(i,j))  
       else
         sol_azi = sol_azi + pi - asin(sina(i,j)) 
       endif



          if ((sol_azi.gt.1.75*pi).or.(sol_azi.lt.0.25*pi)) then 

            do jj = j+1,j+gpshad
              ri = i + (jj-j)*tan(sol_azi)
              i1 = int(ri) 
              i2 = i1+1
              wgt = ri-i1
              dxabs = sqrt((dy*(jj-j))**2+(dx*(ri-i))**2)
              if ((jj.ge.min(jde,jpe+3)).or.(i1.le.max(ids-1,ips-3)).or.(i2.ge.min(ide,ipe+3))) goto 220
              topoelev=atan((wgt*ht_loc(i2,jj)+(1.-wgt)*ht_loc(i1,jj)-ht_loc(i,j))/dxabs)
              if (sin(topoelev).ge.csza) then
                shadowmask(i,j) = 1
                ht_shad(i,j) = max(ht_shad(i,j),ht_loc(i,j)+dxabs*(tan(topoelev)-tan(asin(csza))))
              endif
            enddo

          else if (sol_azi.lt.0.75*pi) then  
            do ii = i+1,i+gpshad
              rj = j - (ii-i)*tan(pi/2.+sol_azi)
              j1 = int(rj)
              j2 = j1+1
              wgt = rj-j1
              dxabs = sqrt((dx*(ii-i))**2+(dy*(rj-j))**2)
              if ((ii.ge.min(ide,ipe+3)).or.(j1.le.max(jds-1,jps-3)).or.(j2.ge.min(jde,jpe+3))) goto 220
              topoelev=atan((wgt*ht_loc(ii,j2)+(1.-wgt)*ht_loc(ii,j1)-ht_loc(i,j))/dxabs)
              if (sin(topoelev).ge.csza) then
                shadowmask(i,j) = 1
                ht_shad(i,j) = max(ht_shad(i,j),ht_loc(i,j)+dxabs*(tan(topoelev)-tan(asin(csza))))
              endif
            enddo

          else if (sol_azi.lt.1.25*pi) then 
            do jj = j-1,j-gpshad,-1
              ri = i + (jj-j)*tan(sol_azi)
              i1 = int(ri)
              i2 = i1+1
              wgt = ri-i1
              dxabs = sqrt((dy*(jj-j))**2+(dx*(ri-i))**2)
              if ((jj.le.max(jds-1,jps-3)).or.(i1.le.max(ids-1,ips-3)).or.(i2.ge.min(ide,ipe+3))) goto 220
              topoelev=atan((wgt*ht_loc(i2,jj)+(1.-wgt)*ht_loc(i1,jj)-ht_loc(i,j))/dxabs)
              if (sin(topoelev).ge.csza) then
                shadowmask(i,j) = 1
                ht_shad(i,j) = max(ht_shad(i,j),ht_loc(i,j)+dxabs*(tan(topoelev)-tan(asin(csza))))
              endif
            enddo

          else                          
            do ii = i-1,i-gpshad,-1
              rj = j - (ii-i)*tan(pi/2.+sol_azi)
              j1 = int(rj)
              j2 = j1+1
              wgt = rj-j1
              dxabs = sqrt((dx*(ii-i))**2+(dy*(rj-j))**2)
              if ((ii.le.max(ids-1,ips-3)).or.(j1.le.max(jds-1,jps-3)).or.(j2.ge.min(jde,jpe+3))) goto 220
              topoelev=atan((wgt*ht_loc(ii,j2)+(1.-wgt)*ht_loc(ii,j1)-ht_loc(i,j))/dxabs)
              if (sin(topoelev).ge.csza) then
                shadowmask(i,j) = 1
                ht_shad(i,j) = max(ht_shad(i,j),ht_loc(i,j)+dxabs*(tan(topoelev)-tan(asin(csza))))
              endif
            enddo
          endif

 220      continue


   ENDDO i_loop2
   ENDDO j_loop2

 endif 

   END SUBROUTINE toposhad

END MODULE module_radiation_driver
