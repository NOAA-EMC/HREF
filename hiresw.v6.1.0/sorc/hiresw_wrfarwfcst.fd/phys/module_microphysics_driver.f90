


MODULE module_microphysics_driver
CONTAINS

SUBROUTINE microphysics_driver( &
                       th, rho, pi_phy, p &
                      ,ht, dz8w, p8w, dt,dx,dy &
                      ,mp_physics, spec_zone &
                      ,specified, channel_switch &
                      ,warm_rain &
                      ,t8w &
                      ,chem_opt, progn &
                      ,cldfra, cldfra_old, exch_h, nsource &
                      ,qlsink, precr, preci, precs, precg &
                      ,xland,snowh,itimestep &
                      ,f_ice_phy,f_rain_phy,f_rimef_phy &
                      ,lowlyr,sr, id &
                      ,ids,ide, jds,jde, kds,kde &
                      ,ims,ime, jms,jme, kms,kme &
                      ,ips,ipe, jps,jpe, kps,kpe &
                      ,i_start,i_end,j_start,j_end,kts,kte &
                      ,num_tiles, naer &


                      ,dlf,dlf2,t_phy,p_hyd,p8w_hyd,tke_pbl,z_at_w,qfx &
                      ,rliq,turbtype3d,smaw3d,wsedl3d,cldfra_old_mp &
                      ,cldfra_mp,cldfra_mp_all,cldfrai,cldfral,cldfra_conv&
                      ,alt &
                      ,accum_mode,aitken_mode,coarse_mode &
                      ,icwmrsh3d,icwmrdp3d,shfrc3d,cmfmc3d,cmfmc2_3d &
                      ,config_flags,fnm,fnp,rh_old_mp,lcd_old_mp &






                      ,qv_curr,qc_curr,qr_curr,qi_curr,qs_curr,qg_curr &
                      ,qndrop_curr,qni_curr,qh_curr,qnh_curr &
                      ,qzr_curr,qzi_curr,qzs_curr,qzg_curr,qzh_curr &
                      ,qns_curr,qnr_curr,qng_curr,qnn_curr,qnc_curr &
                      ,qvolg_curr &
                      ,f_qv,f_qc,f_qr,f_qi,f_qs,f_qg,f_qndrop,f_qni &
                      ,f_qns,f_qnr,f_qng,f_qnc,f_qnn,f_qh,f_qnh &
                      , f_qzr,f_qzi,f_qzs,f_qzg,f_qzh &
                      ,f_qvolg &
                      ,qrcuten, qscuten, qicuten, mu &
                      ,qt_curr,f_qt &
                      ,mp_restart_state,tbpvs_state,tbpvs0_state &
                      ,hail,ice2 &

                      ,w ,z &
                      ,rainnc, rainncv &
                      ,snownc, snowncv &
                      ,hailnc, hailncv &
                      ,graupelnc, graupelncv &




                      ,refl_10cm &


                      ,ri_curr &
                      ,diagflag, do_radar_ref &
                      ,diagflagalt &
                                                   )
   USE module_state_description, ONLY : &
                     KESSLERSCHEME, LINSCHEME, SBU_YLINSCHEME, WSM3SCHEME, WSM5SCHEME &
                    ,WSM6SCHEME, ETAMPNEW, ETAMPOLD, THOMPSON, MORR_TWO_MOMENT &
                    ,GSFCGCESCHEME, WDM5SCHEME, WDM6SCHEME, NSSL_2MOM, NSSL_2MOMCCN &
                    ,NSSL_1MOM,NSSL_1MOMLFO &
                    ,MILBRANDT2MOM , CAMMGMPSCHEME
   USE module_model_constants
   USE module_wrf_error
   USE module_configure, only: grid_config_rec_type
   USE module_mp_kessler
   USE module_mp_lin
   USE module_mp_sbu_ylin
   USE module_mp_wsm3
   USE module_mp_wsm5
   USE module_mp_wsm6
   USE module_mp_etanew
   USE module_mp_etaold
   USE module_mp_thompson
   USE module_mp_gsfcgce
   USE module_mp_morr_two_moment
   USE module_mp_wdm5
   USE module_mp_wdm6
   USE module_mp_milbrandt2mom
   USE module_mp_cammgmp_driver, ONLY: CAMMGMP
   USE module_mp_nssl_2mom
   USE module_mp_HWRF
   USE module_mixactivate, only: prescribe_aerosol_mixactivate
   USE module_utility, ONLY: WRFU_Clock, WRFU_Alarm
   USE module_domain, ONLY : HISTORY_ALARM, Is_alarm_tstep
   IMPLICIT NONE
   TYPE(grid_config_rec_type), INTENT(IN ) , OPTIONAL :: config_flags
   INTEGER, INTENT(IN ) :: mp_physics
   LOGICAL, INTENT(IN ) :: specified
   INTEGER, OPTIONAL, INTENT(IN ) :: chem_opt, progn
   INTEGER, OPTIONAL, INTENT(IN ) :: hail, ice2
   INTEGER, INTENT(IN ) :: ids,ide, jds,jde, kds,kde
   INTEGER, INTENT(IN ) :: ims,ime, jms,jme, kms,kme
   INTEGER, OPTIONAL, INTENT(IN ) :: ips,ipe, jps,jpe, kps,kpe
   INTEGER, INTENT(IN ) :: kts,kte
   INTEGER, INTENT(IN ) :: itimestep,num_tiles,spec_zone
   INTEGER, DIMENSION(num_tiles), INTENT(IN) :: &
     & i_start,i_end,j_start,j_end
   LOGICAL, INTENT(IN ) :: warm_rain
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ), &
         INTENT(INOUT) :: th
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ), &
         INTENT(IN ) :: &
                                                                 rho, &
                                                                dz8w, &
                                                                 p8w, &
                                                              pi_phy, &
                                                                   p
   REAL,INTENT(IN), OPTIONAL ::accum_mode,aitken_mode,coarse_mode
   REAL , DIMENSION( kms:kme ) , &
        INTENT(IN ) , OPTIONAL :: fnm, &
                                                                fnp
   REAL, DIMENSION( ims:ime, jms:jme ), &
        INTENT(IN), OPTIONAL :: &
                                                                 qfx, &
                                                                 rliq
 REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), &
      INTENT(IN), OPTIONAL :: &
                                                                 dlf, &
                                                                dlf2, &
                                                               t_phy, &
                                                               p_hyd, &
                                                             p8w_hyd, &
                                                              z_at_w, &
                                                             tke_pbl, &
                                                          turbtype3d, &
                                                              smaw3d, &
                                                                 alt, &
                                                           icwmrsh3d, &
                                                           icwmrdp3d, &
                                                             shfrc3d, &
                                                             cmfmc3d, &
                                                           cmfmc2_3d
 REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), &
      INTENT(INOUT) , OPTIONAL :: &
                                                       cldfra_old_mp, &
                                                           rh_old_mp, &
                                                          lcd_old_mp
REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), &
      INTENT(INOUT) , OPTIONAL:: &
                                                            wsedl3d, &
                                                          cldfra_mp, &
                                                      cldfra_mp_all, &
                                                            cldfrai, &
                                                            cldfral, &
                                                        cldfra_conv
   REAL, INTENT(INOUT), DIMENSION(ims:ime, kms:kme, jms:jme ) :: &
                                     F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY
   REAL, OPTIONAL, INTENT(OUT), DIMENSION(ims:ime, kms:kme, jms:jme ) :: &
         qlsink, &
         precr, &
         preci, &
         precs, &
         precg
   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN) :: XLAND
   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN), OPTIONAL :: SNOWH
   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(OUT) :: SR
   REAL, INTENT(IN ) :: dt,dx,dy
   INTEGER, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT) :: LOWLYR
   REAL, OPTIONAL, DIMENSION( ims:ime , kms:kme, jms:jme ) , INTENT(OUT) :: refl_10cm
   LOGICAL, OPTIONAL, INTENT(IN ) :: channel_switch
   REAL, OPTIONAL, INTENT(INOUT ) :: naer
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), &
         OPTIONAL, &
         INTENT(INOUT ) :: &
                  w, z, t8w &
                 ,cldfra, cldfra_old, exch_h &
                 ,qv_curr,qc_curr,qr_curr,qi_curr,qs_curr,qg_curr &
                 ,qt_curr,qndrop_curr,qni_curr,qh_curr,qnh_curr &
                 ,qns_curr,qnr_curr,qng_curr,qnn_curr,qnc_curr &
                 ,qzr_curr,qzi_curr,qzs_curr,qzg_curr,qzh_curr &
                 ,qvolg_curr
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), &
         OPTIONAL, &
         INTENT(IN) :: qrcuten, qscuten, qicuten
   REAL, DIMENSION( ims:ime, jms:jme ), &
         OPTIONAL, &
         INTENT(IN) :: mu
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), &
         OPTIONAL, &
         INTENT(INOUT) :: ri_curr
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme ), &
         OPTIONAL, &
         INTENT(OUT ) :: &
                  nsource
   REAL, DIMENSION( ims:ime , jms:jme ), &
         INTENT(INOUT), &
         OPTIONAL :: &
                                                           RAINNC &
                                                         ,RAINNCV &
                                                          ,SNOWNC &
                                                         ,SNOWNCV &
                                                       ,GRAUPELNC &
                                                      ,GRAUPELNCV &
                                                          ,HAILNC &
                                                         ,HAILNCV
   INTEGER,OPTIONAL,INTENT(IN ) :: id
   REAL , DIMENSION( ims:ime , jms:jme ) , OPTIONAL , &
         INTENT(IN) :: ht
   REAL, DIMENSION (:), OPTIONAL, INTENT(INOUT) :: mp_restart_state &
                                         ,tbpvs_state,tbpvs0_state
   LOGICAL, OPTIONAL :: f_qv,f_qc,f_qr,f_qi,f_qs,f_qg,f_qndrop,f_qni,f_qt &
                       ,f_qns,f_qnr,f_qng,f_qnn,f_qnc,f_qh,f_qnh,f_qzr &
                       ,f_qzi,f_qzs,f_qzg,f_qzh,f_qvolg
   LOGICAL, OPTIONAL, INTENT(IN) :: diagflag, diagflagalt
   INTEGER, OPTIONAL, INTENT(IN) :: do_radar_ref
   INTEGER :: i,j,k,its,ite,jts,jte,ij,sz,n
   LOGICAL :: channel
   REAL :: z0, z1, z2, w1, w2
   channel = .FALSE.
   IF ( PRESENT ( channel_switch ) ) channel = channel_switch
   if (mp_physics .eq. 0) return
   IF( specified ) THEN
     sz = spec_zone
   ELSE
     sz = 0
   ENDIF
   !$OMP PARALLEL DO &
   !$OMP PRIVATE ( ij, its, ite, jts, jte, i,j,k,n )
   DO ij = 1 , num_tiles
       IF (channel) THEN
         its = max(i_start(ij),ids)
         ite = min(i_end(ij),ide-1)
       ELSE
         its = max(i_start(ij),ids+sz)
         ite = min(i_end(ij),ide-1-sz)
       ENDIF
       jts = max(j_start(ij),jds+sz)
       jte = min(j_end(ij),jde-1-sz)
       IF( PRESENT(qlsink) ) qlsink(its:ite,kts:kte,jts:jte) = 0.
       IF( PRESENT(precr ) ) precr(its:ite,kts:kte,jts:jte) = 0.
       IF( PRESENT(preci ) ) preci(its:ite,kts:kte,jts:jte) = 0.
       IF( PRESENT(precs ) ) precs(its:ite,kts:kte,jts:jte) = 0.
       IF( PRESENT(precg ) ) precg(its:ite,kts:kte,jts:jte) = 0.
       IF( PRESENT(chem_opt) .AND. PRESENT(progn) ) THEN
       IF( chem_opt==0 .AND. progn==1 .AND. (mp_physics==LINSCHEME .OR. mp_physics==MORR_TWO_MOMENT)) THEN
          IF( PRESENT( QNDROP_CURR ) ) THEN
             CALL wrf_debug ( 100 , 'microphysics_driver: calling prescribe_aerosol_mixactivate' )
             call prescribe_aerosol_mixactivate ( &
                  id, itimestep, dt, naer, &
                  rho, th, pi_phy, w, cldfra, cldfra_old, &
                  z, dz8w, p8w, t8w, exch_h, &
                  qv_curr, qc_curr, qi_curr, qndrop_curr, &
                  nsource, &
                  ids,ide, jds,jde, kds,kde, &
                  ims,ime, jms,jme, kms,kme, &
                  its,ite, jts,jte, kts,kte, &
                  F_QC=f_qc, F_QI=f_qi )
          END IF
       ELSE IF( progn==1 .AND. mp_physics/=LINSCHEME .AND. mp_physics/=MORR_TWO_MOMENT) THEN
             call wrf_error_fatal3("<stdin>",522,&
"SETTINGS ERROR: Prognostic cloud droplet number can only be used with the mp_physics=LINSCHEME or MORRISON.")
       END IF
       END IF
     micro_select: SELECT CASE(mp_physics)
        CASE (KESSLERSCHEME)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling kessler' )
             IF ( PRESENT( QV_CURR ) .AND. PRESENT( QC_CURR ) .AND. &
                                           PRESENT( QR_CURR ) .AND. &
                  PRESENT( RAINNC ) .AND. PRESENT ( RAINNCV ) .AND. &
                                           PRESENT( Z )) THEN
               CALL kessler( &
                  T=th &
                 ,QV=qv_curr &
                 ,QC=qc_curr &
                 ,QR=qr_curr &
                 ,RHO=rho, PII=pi_phy,DT_IN=dt, Z=z, XLV=xlv, CP=cp &
                 ,EP2=ep_2,SVP1=svp1,SVP2=svp2 &
                 ,SVP3=svp3,SVPT0=svpt0,RHOWATER=rhowater &
                 ,DZ8W=dz8w &
                 ,RAINNC=rainnc,RAINNCV=rainncv &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                    )
             ELSE
                CALL wrf_error_fatal3("<stdin>",550,&
'arguments not present for calling kessler' )
             ENDIF
        CASE (THOMPSON)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling thompson' )
             IF ( PRESENT( QV_CURR ) .AND. PRESENT ( QC_CURR ) .AND. &
                  PRESENT( QR_CURR ) .AND. PRESENT ( QI_CURR ) .AND. &
                  PRESENT( QS_CURR ) .AND. PRESENT ( QG_CURR ) .AND. &
                  PRESENT( QNR_CURR) .AND. PRESENT ( QNI_CURR) .AND. &
                  PRESENT( RAINNC ) .AND. PRESENT ( RAINNCV ) ) THEN
             CALL mp_gt_driver( &
                     QV=qv_curr, &
                     QC=qc_curr, &
                     QR=qr_curr, &
                     QI=qi_curr, &
                     QS=qs_curr, &
                     QG=qg_curr, &
                     NI=qni_curr, &
                     NR=qnr_curr, &
                     TH=th, &
                     PII=pi_phy, &
                     P=p, &
                     DZ=dz8w, &
                     DT_IN=dt, &
                     ITIMESTEP=itimestep, &
                     RAINNC=RAINNC, &
                     RAINNCV=RAINNCV, &
                     SNOWNC=SNOWNC, &
                     SNOWNCV=SNOWNCV, &
                     GRAUPELNC=GRAUPELNC, &
                     GRAUPELNCV=GRAUPELNCV, &
                     SR=SR, &
                     REFL_10CM=refl_10cm, &
                     diagflag=diagflag, &
                     do_radar_ref=do_radar_ref, &
                 IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde, &
                 IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme, &
                 ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte)
             ELSE
                CALL wrf_error_fatal3("<stdin>",591,&
'arguments not present for calling thompson_et_al' )
             ENDIF
    CASE (MORR_TWO_MOMENT)
         CALL wrf_debug(100, 'microphysics_driver: calling morrison two moment')
         IF (PRESENT (QV_CURR) .AND. PRESENT (QC_CURR) .AND. &
             PRESENT (QR_CURR) .AND. PRESENT (QI_CURR) .AND. &
         PRESENT (QS_CURR) .AND. PRESENT (QG_CURR) .AND. &
         PRESENT (QR_CURR) .AND. PRESENT (QI_CURR) .AND. &
         PRESENT (QNS_CURR) .AND. PRESENT (QNI_CURR).AND. &
         PRESENT (QNR_CURR) .AND. PRESENT (QNG_CURR).AND. &
         PRESENT (MU) .AND. PRESENT (QSCUTEN).AND. &
         PRESENT (QRCUTEN) .AND. PRESENT (QICUTEN).AND. &
         PRESENT (RAINNC ) .AND. PRESENT (RAINNCV) .AND. &
         PRESENT (Z ) .AND.PRESENT ( W ) ) THEN
         CALL mp_morr_two_moment( &
                     ITIMESTEP=itimestep, &
                     TH=th, &
                     QV=qv_curr, &
                     QC=qc_curr, &
                     QR=qr_curr, &
                     QI=qi_curr, &
                     QS=qs_curr, &
                     QG=qg_curr, &
                     NI=qni_curr, &
                     NS=qns_curr, &
                     NR=qnr_curr, &
                     NG=qng_curr, &
                     RHO=rho, &
                     PII=pi_phy, &
                     P=p, &
                     DT_IN=dt, &
                     DZ=dz8w, &
                     HT=ht, &
                     W=w &
                    ,RAINNC=RAINNC &
                    ,RAINNCV=RAINNCV &
                    ,SR=SR &
                    ,REFL_10CM=refl_10cm &
                    ,diagflag=diagflag &
                    ,do_radar_ref=do_radar_ref &
                    ,qrcuten=qrcuten &
                    ,qscuten=qscuten &
                    ,qicuten=qicuten &
                    ,mu=mu &
                    ,F_QNDROP=f_qndrop &
                 ,QNDROP=qndrop_curr &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                 ,QLSINK=qlsink &
                 ,PRECR=precr,PRECI=preci,PRECS=precs,PRECG=precg &
                                                                    )
        ELSE
           Call wrf_error_fatal3("<stdin>",647,&
'arguments not present for calling morrison two moment')
        ENDIF
    CASE (MILBRANDT2MOM)
         CALL wrf_debug(100, 'microphysics_driver: calling milbrandt2mom')
         IF (PRESENT (QV_CURR) .AND. &
             PRESENT (QC_CURR) .AND. PRESENT (QNC_CURR) .AND. &
             PRESENT (QR_CURR) .AND. PRESENT (QNR_CURR) .AND. &
             PRESENT (QI_CURR) .AND. PRESENT (QNI_CURR) .AND. &
             PRESENT (QS_CURR) .AND. PRESENT (QNS_CURR) .AND. &
             PRESENT (QG_CURR) .AND. PRESENT (QNG_CURR) .AND. &
             PRESENT (QH_CURR) .AND. PRESENT (QNH_CURR) .AND. &
             PRESENT (RAINNC ) .AND. PRESENT (RAINNCV) .AND. &
             PRESENT (SNOWNC ) .AND. PRESENT (SNOWNCV) .AND. &
             PRESENT (HAILNC ) .AND. PRESENT (HAILNCV) .AND. &
             PRESENT (GRAUPELNC).AND.PRESENT (GRAUPELNCV).AND. &
             PRESENT (Z ) .AND. PRESENT ( W ) ) THEN
         CALL mp_milbrandt2mom_driver( &
                     ITIMESTEP=itimestep, &
                     TH=th, &
                     QV=qv_curr, &
                     QC=qc_curr, &
                     QR=qr_curr, &
                     QI=qi_curr, &
                     QS=qs_curr, &
                     QG=qg_curr, &
                     QH=qh_curr, &
                     NC=qnc_curr, &
                     NR=qnr_curr, &
                     NI=qni_curr, &
                     NS=qns_curr, &
                     NG=qng_curr, &
                     NH=qnh_curr, &
                     PII=pi_phy, &
                     P=p, &
                     DT_IN=dt, &
                     DZ=dz8w, &
                     W=w, &
                     RAINNC = RAINNC, &
                     RAINNCV = RAINNCV, &
                     SNOWNC = SNOWNC, &
                     SNOWNCV = SNOWNCV, &
                     HAILNC = HAILNC, &
                     HAILNCV = HAILNCV, &
                     GRPLNC = GRAUPELNC, &
                     GRPLNCV = GRAUPELNCV, &
                     SR=SR, &
                     Zet = refl_10cm, &
                  IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde, &
                  IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme, &
                  ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                    )
        ELSE
           Call wrf_error_fatal3("<stdin>",705,&
'arguments not present for calling milbrandt2mom')
        ENDIF
    CASE (NSSL_1MOM)
         CALL wrf_debug(100, 'microphysics_driver: calling nssl1mom')
         IF (PRESENT (QV_CURR) .AND. &
             PRESENT (QC_CURR) .AND. &
             PRESENT (QR_CURR) .AND. &
             PRESENT (QI_CURR) .AND. &
             PRESENT (QS_CURR) .AND. &
             PRESENT (QG_CURR) .AND. &
             PRESENT (QH_CURR) .AND. &
             PRESENT (RAINNC ) .AND. PRESENT (RAINNCV) .AND. &
             PRESENT (SNOWNC ) .AND. PRESENT (SNOWNCV) .AND. &
             PRESENT (HAILNC ) .AND. PRESENT (HAILNCV) .AND. &
             PRESENT (GRAUPELNC).AND.PRESENT (GRAUPELNCV).AND. &
             PRESENT (Z ) .AND. PRESENT ( W ) .AND. &
             PRESENT (QVOLG_CURR) ) THEN
         CALL nssl_2mom_driver( &
                     ITIMESTEP=itimestep, &
                     TH=th, &
                     QV=qv_curr, &
                     QC=qc_curr, &
                     QR=qr_curr, &
                     QI=qi_curr, &
                     QS=qs_curr, &
                     QH=qg_curr, &
                     QHL=qh_curr, &
                     CCW=qnc_curr, &
                     CRW=qnr_curr, &
                     CCI=qni_curr, &
                     CSW=qns_curr, &
                     CHW=qng_curr, &
                     CHL=qnh_curr, &
                     VHW=qvolg_curr, &
                     PII=pi_phy, &
                     P=p, &
                     W=w, &
                     DZ=dz8w, &
                     DTP=dt, &
                     DN=rho, &
                     RAINNC = RAINNC, &
                     RAINNCV = RAINNCV, &
                     SNOWNC = SNOWNC, &
                     SNOWNCV = SNOWNCV, &
                     HAILNC = HAILNC, &
                     HAILNCV = HAILNCV, &
                     GRPLNC = GRAUPELNC, &
                     GRPLNCV = GRAUPELNCV, &
                     SR=SR, &
                     dbz = refl_10cm, &
                     diagflag = diagflag, &
                  IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde, &
                  IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme, &
                  ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                    )
        ELSE
           Call wrf_error_fatal3("<stdin>",814,&
'arguments not present for calling nssl_2mom')
        ENDIF
    CASE (NSSL_1MOMLFO)
         CALL wrf_debug(100, 'microphysics_driver: calling nssl1mom')
         IF (PRESENT (QV_CURR) .AND. &
             PRESENT (QC_CURR) .AND. &
             PRESENT (QR_CURR) .AND. &
             PRESENT (QI_CURR) .AND. &
             PRESENT (QS_CURR) .AND. &
             PRESENT (QG_CURR) .AND. &
             PRESENT (RAINNC ) .AND. PRESENT (RAINNCV) .AND. &
             PRESENT (SNOWNC ) .AND. PRESENT (SNOWNCV) .AND. &
             PRESENT (GRAUPELNC).AND.PRESENT (GRAUPELNCV).AND. &
             PRESENT (Z ) .AND. PRESENT ( W ) ) THEN
         CALL nssl_2mom_driver( &
                     ITIMESTEP=itimestep, &
                     TH=th, &
                     QV=qv_curr, &
                     QC=qc_curr, &
                     QR=qr_curr, &
                     QI=qi_curr, &
                     QS=qs_curr, &
                     QH=qg_curr, &
                     PII=pi_phy, &
                     P=p, &
                     W=w, &
                     DZ=dz8w, &
                     DTP=dt, &
                     DN=rho, &
                     RAINNC = RAINNC, &
                     RAINNCV = RAINNCV, &
                     SNOWNC = SNOWNC, &
                     SNOWNCV = SNOWNCV, &
                     GRPLNC = GRAUPELNC, &
                     GRPLNCV = GRAUPELNCV, &
                     SR=SR, &
                     dbz = refl_10cm, &
                     diagflag = diagflag, &
                  IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde, &
                  IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme, &
                  ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                    )
        ELSE
           Call wrf_error_fatal3("<stdin>",862,&
'arguments not present for calling nssl_2mom')
        ENDIF
    CASE (NSSL_2MOM)
         CALL wrf_debug(100, 'microphysics_driver: calling nssl2mom')
         IF (PRESENT (QV_CURR) .AND. &
             PRESENT (QC_CURR) .AND. PRESENT (QNC_CURR) .AND. &
             PRESENT (QR_CURR) .AND. PRESENT (QNR_CURR) .AND. &
             PRESENT (QI_CURR) .AND. PRESENT (QNI_CURR) .AND. &
             PRESENT (QS_CURR) .AND. PRESENT (QNS_CURR) .AND. &
             PRESENT (QG_CURR) .AND. PRESENT (QNG_CURR) .AND. &
             PRESENT (QH_CURR) .AND. PRESENT (QNH_CURR) .AND. &
             PRESENT (RAINNC ) .AND. PRESENT (RAINNCV) .AND. &
             PRESENT (SNOWNC ) .AND. PRESENT (SNOWNCV) .AND. &
             PRESENT (HAILNC ) .AND. PRESENT (HAILNCV) .AND. &
             PRESENT (GRAUPELNC).AND.PRESENT (GRAUPELNCV).AND. &
             PRESENT (Z ) .AND. PRESENT ( W ) .AND. &
             PRESENT (QVOLG_CURR) ) THEN
         CALL nssl_2mom_driver( &
                     ITIMESTEP=itimestep, &
                     TH=th, &
                     QV=qv_curr, &
                     QC=qc_curr, &
                     QR=qr_curr, &
                     QI=qi_curr, &
                     QS=qs_curr, &
                     QH=qg_curr, &
                     QHL=qh_curr, &
                     CCW=qnc_curr, &
                     CRW=qnr_curr, &
                     CCI=qni_curr, &
                     CSW=qns_curr, &
                     CHW=qng_curr, &
                     CHL=qnh_curr, &
                     VHW=qvolg_curr, &
                     PII=pi_phy, &
                     P=p, &
                     W=w, &
                     DZ=dz8w, &
                     DTP=dt, &
                     DN=rho, &
                     RAINNC = RAINNC, &
                     RAINNCV = RAINNCV, &
                     SNOWNC = SNOWNC, &
                     SNOWNCV = SNOWNCV, &
                     HAILNC = HAILNC, &
                     HAILNCV = HAILNCV, &
                     GRPLNC = GRAUPELNC, &
                     GRPLNCV = GRAUPELNCV, &
                     SR=SR, &
                     dbz = refl_10cm, &
                     diagflag = diagflag, &
                  IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde, &
                  IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme, &
                  ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                    )
        ELSE
           Call wrf_error_fatal3("<stdin>",922,&
'arguments not present for calling nssl_2mom')
        ENDIF
    CASE (NSSL_2MOMCCN)
         CALL wrf_debug(100, 'microphysics_driver: calling nssl_2momccn')
         IF (PRESENT (QV_CURR) .AND. &
             PRESENT (QC_CURR) .AND. PRESENT (QNC_CURR) .AND. &
             PRESENT (QR_CURR) .AND. PRESENT (QNR_CURR) .AND. &
             PRESENT (QI_CURR) .AND. PRESENT (QNI_CURR) .AND. &
             PRESENT (QS_CURR) .AND. PRESENT (QNS_CURR) .AND. &
             PRESENT (QG_CURR) .AND. PRESENT (QNG_CURR) .AND. &
             PRESENT (QH_CURR) .AND. PRESENT (QNH_CURR) .AND. &
             PRESENT (RAINNC ) .AND. PRESENT (RAINNCV) .AND. &
             PRESENT (SNOWNC ) .AND. PRESENT (SNOWNCV) .AND. &
             PRESENT (HAILNC ) .AND. PRESENT (HAILNCV) .AND. &
             PRESENT (GRAUPELNC).AND.PRESENT (GRAUPELNCV).AND. &
             PRESENT (Z ) .AND. PRESENT ( W ) .AND. &
             PRESENT (QVOLG_CURR) .AND. PRESENT( QNN_CURR ) ) THEN
         CALL nssl_2mom_driver( &
                     ITIMESTEP=itimestep, &
                     TH=th, &
                     QV=qv_curr, &
                     QC=qc_curr, &
                     QR=qr_curr, &
                     QI=qi_curr, &
                     QS=qs_curr, &
                     QH=qg_curr, &
                     QHL=qh_curr, &
                     CCW=qnc_curr, &
                     CRW=qnr_curr, &
                     CCI=qni_curr, &
                     CSW=qns_curr, &
                     CHW=qng_curr, &
                     CHL=qnh_curr, &
                     VHW=qvolg_curr, &
                     cn=qnn_curr, &
                     PII=pi_phy, &
                     P=p, &
                     W=w, &
                     DZ=dz8w, &
                     DTP=dt, &
                     DN=rho, &
                     RAINNC = RAINNC, &
                     RAINNCV = RAINNCV, &
                     SNOWNC = SNOWNC, &
                     SNOWNCV = SNOWNCV, &
                     HAILNC = HAILNC, &
                     HAILNCV = HAILNCV, &
                     GRPLNC = GRAUPELNC, &
                     GRPLNCV = GRAUPELNCV, &
                     SR=SR, &
                     dbz = refl_10cm, &
                     diagflag = diagflag, &
                  IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde, &
                  IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme, &
                  ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                    )
        ELSE
           Call wrf_error_fatal3("<stdin>",983,&
'arguments not present for calling nssl_2momccn')
        ENDIF
        CASE (GSFCGCESCHEME)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling GSFCGCE' )
             IF ( PRESENT( QV_CURR ) .AND. PRESENT ( QC_CURR ) .AND. &
                  PRESENT( QR_CURR ) .AND. PRESENT ( QI_CURR ) .AND. &
                  PRESENT( QS_CURR ) .AND. &
                  PRESENT( RAINNC ) .AND. PRESENT ( RAINNCV ) .AND. &
                  PRESENT( HAIL ) .AND. PRESENT ( ICE2 ) .AND. &
                  PRESENT( Z ) .AND. PRESENT ( W ) ) THEN
               CALL gsfcgce( &
                  TH=th &
                 ,QV=qv_curr &
                 ,QL=qc_curr &
                 ,QR=qr_curr &
                 ,QI=qi_curr &
                 ,QS=qs_curr &
                 ,RHO=rho, PII=pi_phy, P=p, DT_IN=dt, Z=z &
                 ,HT=ht, DZ8W=dz8w, GRAV=G &
                 ,RHOWATER=rhowater, RHOSNOW=rhosnow &
                 ,ITIMESTEP=itimestep &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                 ,RAINNC=rainnc, RAINNCV=rainncv &
                 ,SNOWNC=snownc, SNOWNCV=snowncv ,SR=sr &
                 ,GRAUPELNC=graupelnc ,GRAUPELNCV=graupelncv &
                 ,REFL_10CM=refl_10cm &
                 ,diagflag=diagflag &
                 ,do_radar_ref=do_radar_ref &
                 ,F_QG=f_qg &
                 ,QG=qg_curr &
                 ,IHAIL=hail, ICE2=ice2 &
                                                                    )
             ELSE
                CALL wrf_error_fatal3("<stdin>",1027,&
'arguments not present for calling GSFCGCE' )
             ENDIF
        CASE (LINSCHEME)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling lin_et_al' )
             IF ( PRESENT( QV_CURR ) .AND. PRESENT ( QC_CURR ) .AND. &
                  PRESENT( QR_CURR ) .AND. PRESENT ( QI_CURR ) .AND. &
                  PRESENT( QS_CURR ) .AND. &
                  PRESENT( RAINNC ) .AND. PRESENT ( RAINNCV ) .AND. &
                  PRESENT( Z ) ) THEN
               CALL lin_et_al( &
                  TH=th &
                 ,QV=qv_curr &
                 ,QL=qc_curr &
                 ,QR=qr_curr &
                 ,QI=qi_curr &
                 ,QS=qs_curr &
                 ,QLSINK=qlsink &
                 ,RHO=rho, PII=pi_phy, P=p, DT_IN=dt, Z=z &
                 ,HT=ht, DZ8W=dz8w, GRAV=G, CP=cp &
                 ,RAIR=r_d, RVAPOR=R_v &
                 ,XLS=xls, XLV=xlv, XLF=xlf &
                 ,RHOWATER=rhowater, RHOSNOW=rhosnow &
                 ,EP2=ep_2,SVP1=svp1,SVP2=svp2 &
                 ,SVP3=svp3,SVPT0=svpt0 &
                 ,RAINNC=rainnc, RAINNCV=rainncv &
                 ,SNOWNC=snownc, SNOWNCV=snowncv &
                 ,GRAUPELNC=graupelnc, GRAUPELNCV=graupelncv, SR=sr &
                 ,REFL_10CM=refl_10cm &
                 ,diagflag=diagflag &
                 ,do_radar_ref=do_radar_ref &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                 ,PRECR=precr,PRECI=preci,PRECS=precs,PRECG=precg &
                 ,F_QG=f_qg, F_QNDROP=f_qndrop &
                 ,QG=qg_curr &
                 ,QNDROP=qndrop_curr &
                                                                    )
             ELSE
                CALL wrf_error_fatal3("<stdin>",1068,&
'arguments not present for calling lin_et_al' )
             ENDIF
       CASE (SBU_YLINSCHEME)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling sbu_ylin' )
             IF ( PRESENT( QV_CURR ) .AND. PRESENT ( QC_CURR ) .AND. &
                  PRESENT( QR_CURR ) .AND. PRESENT ( QI_CURR ) .AND. &
                  PRESENT( QS_CURR ) .AND. &
                  PRESENT( RI_CURR ) .AND. &
                  PRESENT( RAINNC ) .AND. PRESENT ( RAINNCV ) .AND. &
                  PRESENT( Z ) ) THEN
               CALL sbu_ylin( &
                  TH=th &
                 ,QV=qv_curr &
                 ,QL=qc_curr &
                 ,QR=qr_curr &
                 ,QI=qi_curr &
                 ,QS=qs_curr &
                 ,RI3D=ri_curr &
                 ,RHO=rho, PII=pi_phy, P=p, DT_IN=dt, Z=z &
                 ,HT=ht, DZ8W=dz8w &
                 ,RAINNC=rainnc, RAINNCV=rainncv &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                     )
             ELSE
                CALL wrf_error_fatal3("<stdin>",1110,&
'arguments not present for calling sbu_ylin' )
             ENDIF
        CASE (WSM3SCHEME)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling wsm3' )
             IF ( PRESENT( QV_CURR ) .AND. PRESENT ( QC_CURR ) .AND. &
                  PRESENT( QR_CURR ) .AND. &
                  PRESENT( RAINNC ) .AND. PRESENT ( RAINNCV ) .AND. &
                  PRESENT( W ) ) THEN
             CALL wsm3( &
                  TH=th &
                 ,Q=qv_curr &
                 ,QCI=qc_curr &
                 ,QRS=qr_curr &
                 ,W=w,DEN=rho,PII=pi_phy,P=p,DELZ=dz8w &
                 ,DELT=dt,G=g,CPD=cp,CPV=cpv &
                 ,RD=r_d,RV=r_v,T0C=svpt0 &
                 ,EP1=ep_1, EP2=ep_2, QMIN=epsilon &
                 ,XLS=xls, XLV0=xlv, XLF0=xlf &
                 ,DEN0=rhoair0, DENR=rhowater &
                 ,CLIQ=cliq,CICE=cice,PSAT=psat &
                 ,RAIN=rainnc ,RAINNCV=rainncv &
                 ,SNOW=snownc ,SNOWNCV=snowncv &
                 ,SR=sr &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                    )
             ELSE
                CALL wrf_error_fatal3("<stdin>",1141,&
'arguments not present for calling wsm3' )
             ENDIF
        CASE (WSM5SCHEME)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling wsm5' )
             IF ( PRESENT( QV_CURR ) .AND. PRESENT ( QC_CURR ) .AND. &
                  PRESENT( QR_CURR ) .AND. PRESENT ( QI_CURR ) .AND. &
                  PRESENT( QS_CURR ) .AND. &
                  PRESENT( RAINNC ) .AND. PRESENT ( RAINNCV ) ) THEN
             CALL wsm5( &
                  TH=th &
                 ,Q=qv_curr &
                 ,QC=qc_curr &
                 ,QR=qr_curr &
                 ,QI=qi_curr &
                 ,QS=qs_curr &
                 ,DEN=rho,PII=pi_phy,P=p,DELZ=dz8w &
                 ,DELT=dt,G=g,CPD=cp,CPV=cpv &
                 ,RD=r_d,RV=r_v,T0C=svpt0 &
                 ,EP1=ep_1, EP2=ep_2, QMIN=epsilon &
                 ,XLS=xls, XLV0=xlv, XLF0=xlf &
                 ,DEN0=rhoair0, DENR=rhowater &
                 ,CLIQ=cliq,CICE=cice,PSAT=psat &
                 ,RAIN=rainnc ,RAINNCV=rainncv &
                 ,SNOW=snownc ,SNOWNCV=snowncv &
                 ,SR=sr &
                 ,REFL_10CM=refl_10cm &
                 ,diagflag=diagflag &
                 ,do_radar_ref=do_radar_ref &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                    )
             ELSE
                CALL wrf_error_fatal3("<stdin>",1176,&
'arguments not present for calling wsm5' )
             ENDIF
        CASE (WSM6SCHEME)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling wsm6' )
             IF ( PRESENT( QV_CURR ) .AND. PRESENT ( QC_CURR ) .AND. &
                  PRESENT( QR_CURR ) .AND. PRESENT ( QI_CURR ) .AND. &
                  PRESENT( QS_CURR ) .AND. PRESENT ( QG_CURR ) .AND. &
                  PRESENT( RAINNC ) .AND. PRESENT ( RAINNCV ) ) THEN
             CALL wsm6( &
                  TH=th &
                 ,Q=qv_curr &
                 ,QC=qc_curr &
                 ,QR=qr_curr &
                 ,QI=qi_curr &
                 ,QS=qs_curr &
                 ,QG=qg_curr &
                 ,DEN=rho,PII=pi_phy,P=p,DELZ=dz8w &
                 ,DELT=dt,G=g,CPD=cp,CPV=cpv &
                 ,RD=r_d,RV=r_v,T0C=svpt0 &
                 ,EP1=ep_1, EP2=ep_2, QMIN=epsilon &
                 ,XLS=xls, XLV0=xlv, XLF0=xlf &
                 ,DEN0=rhoair0, DENR=rhowater &
                 ,CLIQ=cliq,CICE=cice,PSAT=psat &
                 ,RAIN=rainnc ,RAINNCV=rainncv &
                 ,SNOW=snownc ,SNOWNCV=snowncv &
                 ,SR=sr &
                 ,REFL_10CM=refl_10cm &
                 ,diagflag=diagflag &
                 ,diagflagalt=diagflagalt &
                 ,do_radar_ref=do_radar_ref &
                 ,GRAUPEL=graupelnc ,GRAUPELNCV=graupelncv &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                    )
             ELSE
                CALL wrf_error_fatal3("<stdin>",1214,&
'arguments not present for calling wsm6' )
             ENDIF
        CASE (WDM5SCHEME)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling wdm5' )
             IF ( PRESENT( QV_CURR ) .AND. PRESENT ( QC_CURR ) .AND. &
                  PRESENT( QR_CURR ) .AND. PRESENT ( QI_CURR ) .AND. &
                  PRESENT( QS_CURR ) .AND. PRESENT( QNN_CURR ) .AND. &
                  PRESENT ( QNC_CURR ) .AND. PRESENT( QNR_CURR ).AND. &
                  PRESENT( RAINNC ) .AND. PRESENT ( RAINNCV ) ) THEN
             CALL wdm5( &
                  TH=th &
                 ,Q=qv_curr &
                 ,QC=qc_curr &
                 ,QR=qr_curr &
                 ,QI=qi_curr &
                 ,QS=qs_curr &
                 ,NN=qnn_curr &
                 ,NC=qnc_curr &
                 ,NR=qnr_curr &
                 ,DEN=rho,PII=pi_phy,P=p,DELZ=dz8w &
                 ,DELT=dt,G=g,CPD=cp,CPV=cpv,CCN0=n_ccn0 &
                 ,RD=r_d,RV=r_v,T0C=svpt0 &
                 ,EP1=ep_1, EP2=ep_2, QMIN=epsilon &
                 ,XLS=xls, XLV0=xlv, XLF0=xlf &
                 ,DEN0=rhoair0, DENR=rhowater &
                 ,CLIQ=cliq,CICE=cice,PSAT=psat &
                 ,RAIN=rainnc ,RAINNCV=rainncv &
                 ,SNOW=snownc ,SNOWNCV=snowncv &
                 ,SR=sr &
                 ,REFL_10CM=refl_10cm &
                 ,diagflag=diagflag &
                 ,do_radar_ref=do_radar_ref &
                 ,ITIMESTEP=itimestep &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                    )
             ELSE
                CALL wrf_error_fatal3("<stdin>",1254,&
'arguments not present for calling wdm5')
             ENDIF
       CASE (WDM6SCHEME)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling wdm6' )
             IF ( PRESENT( QV_CURR ) .AND. PRESENT ( QC_CURR ) .AND. &
                  PRESENT( QR_CURR ) .AND. PRESENT ( QI_CURR ) .AND. &
                  PRESENT( QS_CURR ) .AND. PRESENT ( QG_CURR ) .AND. &
                  PRESENT( QNN_CURR ) .AND. PRESENT ( QNC_CURR ) .AND. &
                  PRESENT( QNR_CURR ).AND. &
                 PRESENT( RAINNC ) .AND. PRESENT ( RAINNCV ) ) THEN
             CALL wdm6( &
                  TH=th &
                 ,Q=qv_curr &
                 ,QC=qc_curr &
                 ,QR=qr_curr &
                 ,QI=qi_curr &
                 ,QS=qs_curr &
                 ,QG=qg_curr &
                 ,NN=qnn_curr &
                 ,NC=qnc_curr &
                 ,NR=qnr_curr &
                 ,DEN=rho,PII=pi_phy,P=p,DELZ=dz8w &
                 ,DELT=dt,G=g,CPD=cp,CPV=cpv,CCN0=n_ccn0 &
                 ,RD=r_d,RV=r_v,T0C=svpt0 &
                 ,EP1=ep_1, EP2=ep_2, QMIN=epsilon &
                 ,XLS=xls, XLV0=xlv, XLF0=xlf &
                 ,DEN0=rhoair0, DENR=rhowater &
                 ,CLIQ=cliq,CICE=cice,PSAT=psat &
                 ,RAIN=rainnc ,RAINNCV=rainncv &
                 ,SNOW=snownc ,SNOWNCV=snowncv &
                 ,SR=sr &
                 ,REFL_10CM=refl_10cm &
                 ,diagflag=diagflag &
                 ,do_radar_ref=do_radar_ref &
                 ,GRAUPEL=graupelnc ,GRAUPELNCV=graupelncv &
                 ,ITIMESTEP=itimestep &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                    )
             ELSE
               CALL wrf_error_fatal3("<stdin>",1297,&
'arguments not present for calling wdm6')
             ENDIF
        CASE (ETAMPNEW)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling etampnew')
             IF ( PRESENT( qv_curr ) .AND. PRESENT( qt_curr ) .AND. &
                  PRESENT( RAINNC ) .AND. PRESENT ( RAINNCV ) .AND. &
                  PRESENT( mp_restart_state ) .AND. &
                  PRESENT( tbpvs_state ) .AND. &
                  PRESENT( tbpvs0_state ) ) THEN
               CALL ETAMP_NEW( &
                  ITIMESTEP=itimestep,DT=dt,DX=dx,DY=dy &
                 ,DZ8W=dz8w,RHO_PHY=rho,P_PHY=p,PI_PHY=pi_phy,TH_PHY=th &
                 ,QV=qv_curr &
                 ,QC=qc_curr &
                 ,QS=qs_curr &
                 ,QR=qr_curr &
                 ,QT=qt_curr &
                 ,LOWLYR=LOWLYR,SR=SR &
                 ,F_ICE_PHY=F_ICE_PHY,F_RAIN_PHY=F_RAIN_PHY &
                 ,F_RIMEF_PHY=F_RIMEF_PHY &
                 ,RAINNC=rainnc,RAINNCV=rainncv &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                 ,MP_RESTART_STATE=mp_restart_state &
                 ,TBPVS_STATE=tbpvs_state,TBPVS0_STATE=tbpvs0_state &
                                                                    )
             ELSE
                CALL wrf_error_fatal3("<stdin>",1327,&
'arguments not present for calling etampnew' )
             ENDIF
          CASE (CAMMGMPSCHEME)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling CAMMGMPSCHEME')
             IF ( PRESENT( z ) .AND. PRESENT( ht ) .AND. &
                  PRESENT( qs_curr ) .AND. &
                  PRESENT( qv_curr ) .AND. PRESENT( qc_curr ) .AND. &
                  PRESENT( qi_curr ) .AND. PRESENT( f_qc ) .AND. &
                  PRESENT( qr_curr ) .AND. PRESENT( qndrop_curr ) .AND. &
                  PRESENT( f_qi ) .AND. PRESENT( qnc_curr ) .AND. &
                  PRESENT( RAINNCV ) .AND. PRESENT( SNOWNCV ) .AND. &
                  PRESENT( qns_curr ) .AND. PRESENT( qnr_curr ) .AND. &
                  PRESENT( qni_curr ) .AND. PRESENT( RAINNC ) ) THEN
                CALL CAMMGMP(ITIMESTEP=itimestep,DT=dt,P8W=p8w_hyd,P_HYD=p_hyd &
                     ,T_PHY=t_phy,PI_PHY=pi_phy,Z_AT_W=z_at_w,QFX=qfx &
                     ,TKE_PBL=tke_pbl,TURBTYPE3D=turbtype3d,SMAW3D=smaw3d &
                     ,DLF3D=dlf,DLF2_3D=dlf2,RLIQ2D=rliq,Z_SEA_LEVEL=z &
                     ,KVH3D=exch_h,HT=ht,ALT=alt,ACCUM_MODE=accum_mode &
                     ,AITKEN_MODE=aitken_mode,COARSE_MODE=coarse_mode &
                     ,ICWMRSH3D=icwmrsh3d,ICWMRDP3D=icwmrdp3d,SHFRC3D=shfrc3d &
                     ,CMFMC3D=cmfmc3d,CMFMC2_3D=cmfmc2_3d &
                     ,CONFIG_FLAGS=config_flags,F_ICE_PHY=f_ice_phy &
                     ,F_RAIN_PHY=f_rain_phy &
                     ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                     ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                     ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                     ,TH=th,CLDFRA_OLD_MP=cldfra_old_mp,CLDFRA_MP=cldfra_mp &
                     ,CLDFRA_MP_ALL=cldfra_mp_all,CLDFRAI=cldfrai,CLDFRAL=cldfral &
                     ,CLDFRA_CONV=cldfra_conv,WSEDL3D=wsedl3d &
                     ,RAINNC=rainnc,RAINNCV=rainncv,SNOWNC=snownc,SNOWNCV=snowncv &
                     ,SR=sr,QV_CURR=qv_curr,QC_CURR=qc_curr,QI_CURR=qi_curr &
                     ,QS_CURR=qs_curr,QR_CURR=qr_curr,NC3D=qnc_curr &
                     ,NI3D=qni_curr,NS3D=qns_curr,NR3D=qnr_curr,QNDROP=qndrop_curr&
                     ,RH_OLD_MP=rh_old_mp,LCD_OLD_MP=lcd_old_mp &
                     ,XLAND=XLAND,SNOWH=SNOWH &
                     )
             ELSE
                CALL wrf_error_fatal3("<stdin>",1367,&
'arguments not present for calling CAMMGMP SCHEME' )
             ENDIF
        CASE (ETAMPOLD)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling etampold')
             IF ( PRESENT( qv_curr ) .AND. PRESENT( qt_curr ) .AND. &
                  PRESENT( RAINNC ) .AND. PRESENT ( RAINNCV ) .AND. &
                  PRESENT( mp_restart_state ) .AND. &
                  PRESENT( tbpvs_state ) .AND. &
                  PRESENT( tbpvs0_state ) ) THEN
               CALL ETAMP_OLD( &
                  ITIMESTEP=itimestep,DT=dt,DX=dx,DY=dy &
                 ,DZ8W=dz8w,RHO_PHY=rho,P_PHY=p,PI_PHY=pi_phy,TH_PHY=th &
                 ,QV=qv_curr &
                 ,QC=qc_curr &
                 ,QS=qs_curr &
                 ,QR=qr_curr &
                 ,QT=qt_curr &
                 ,LOWLYR=LOWLYR,SR=SR &
                 ,F_ICE_PHY=F_ICE_PHY,F_RAIN_PHY=F_RAIN_PHY &
                 ,F_RIMEF_PHY=F_RIMEF_PHY &
                 ,RAINNC=rainnc,RAINNCV=rainncv &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                 ,MP_RESTART_STATE=mp_restart_state &
                 ,TBPVS_STATE=tbpvs_state,TBPVS0_STATE=tbpvs0_state &
                                                                    )
             ELSE
                CALL wrf_error_fatal3("<stdin>",1398,&
'arguments not present for calling etampold' )
             ENDIF
      CASE DEFAULT
         WRITE( wrf_err_message , * ) 'The microphysics option does not exist: mp_physics = ', mp_physics
         CALL wrf_error_fatal3("<stdin>",1405,&
wrf_err_message )
      END SELECT micro_select
   ENDDO
   !$OMP END PARALLEL DO
   CALL wrf_debug ( 200 , 'microphysics_driver: returning from' )
   RETURN
   END SUBROUTINE microphysics_driver
END MODULE module_microphysics_driver
