


MODULE module_pbl_driver
CONTAINS


   SUBROUTINE pbl_driver(                                          &
                  itimestep,dt,u_frame,v_frame                     &
                 ,bldt,curr_secs,adapt_step_flag                   &
                 ,rublten,rvblten,rthblten                         &
                 ,tsk,xland,znt,ht                                 &
                 ,ust,pblh,hfx,qfx,grdflx                          &
                 ,u_phy,v_phy,th_phy,rho                           &
                 ,p_phy,pi_phy,p8w,t_phy,dz8w,z                    &
                 ,tke_myj,el_myj,exch_h,exch_m,akhs,akms           &
                 ,thz0,qz0,uz0,vz0,qsfc,f                          &
                 ,lowlyr,u10,v10                                   &
                 ,psim,psih,gz1oz0, wspd,br,chklowq                &
                 ,bl_pbl_physics, ra_lw_physics, dx                &
                 ,stepbl,warm_rain                                 &
                 ,kpbl,mixht,ct,lh,snow,xice                       &
                 ,znu, znw, mut, p_top                             &
                 ,qke,tsq,qsq,cov,rmol,ch,qcg,grav_settling        &
                 ,ids,ide, jds,jde, kds,kde                        &
                 ,ims,ime, jms,jme, kms,kme                        &
                 ,i_start,i_end, j_start,j_end, kts,kte, num_tiles &
             
                 ,hol, mol, regime                                 &
             
                 ,gwd_opt                                          &
                 ,dusfcg,dvsfcg,var2d,oc12d                        &
                 ,oa1,oa2,oa3,oa4,ol1,ol2,ol3,ol4                  &
             
                 ,qv_curr, qc_curr, qr_curr                        &
                 ,qi_curr, qs_curr, qg_curr                        &
                 ,rqvblten,rqcblten,rqiblten                       &
                 ,rqrblten,rqsblten,rqgblten                       &
             
                 ,f_qv,f_qc,f_qr                                   &
                 ,f_qi,f_qs,f_qg                                   &

               ,frc_urb2d                                  &
               ,a_u_bep,a_v_bep,a_t_bep,a_q_bep            &
               ,b_u_bep,b_v_bep,b_t_bep,b_q_bep            &
               ,sf_bep,vl_bep                              &
               ,sf_sfclay_physics,sf_urban_physics         &
               ,a_e_bep,b_e_bep,dlg_bep                              &
               ,dl_u_bep                                             &
                                                                     )       

   USE module_state_description, ONLY :                            &
                   YSUSCHEME,MRFSCHEME,GFSSCHEME,MYJPBLSCHEME,ACMPBLSCHEME,QNSEPBLSCHEME,&
                   MYJSFCSCHEME

   USE module_model_constants



   USE module_bl_myjpbl
   USE module_bl_qnsepbl
   USE module_bl_ysu
   USE module_bl_mrf
   USE module_bl_gfs
   USE module_bl_acm
   USE module_bl_gwdo
   USE module_bl_myjurb
   USE module_bl_boulac

   
   
   
   
   
   
   
   
   
   
   
   

   IMPLICIT NONE







































































































































   INTEGER,    INTENT(IN   )    ::     bl_pbl_physics, ra_lw_physics,sf_sfclay_physics,sf_urban_physics 

   INTEGER,    INTENT(IN   )    ::     ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       kts,kte, num_tiles

   INTEGER, DIMENSION(num_tiles), INTENT(IN) ::                   &
  &                                    i_start,i_end,j_start,j_end

   INTEGER,    INTENT(IN   )    ::     itimestep,STEPBL
   INTEGER,    DIMENSION( ims:ime , jms:jme ),                    &
               INTENT(IN   )    ::                        LOWLYR

   LOGICAL,      INTENT(IN   )    ::   warm_rain

   REAL,       DIMENSION( kms:kme ),                              &
               OPTIONAL, INTENT(IN   )    ::               znu,   &
                                                           znw

   REAL,       INTENT(IN   )    ::     DT,DX
   REAL,       INTENT(IN   ),OPTIONAL    ::     bldt
   REAL,       INTENT(IN   ),OPTIONAL    ::     curr_secs
   LOGICAL,    INTENT(IN   ),OPTIONAL    ::     adapt_step_flag


   REAL,       DIMENSION( ims:ime, kms:kme, jms:jme ),            &
               INTENT(IN   )    ::                         p_phy, &
                                                          pi_phy, &
                                                             p8w, &
                                                             rho, &
                                                           t_phy, &
                                                           u_phy, &
                                                           v_phy, &
                                                            dz8w, &
                                                               z, &
                                                          th_phy


   REAL,       DIMENSION( ims:ime , jms:jme ),                    &
               INTENT(IN   )    ::                         XLAND, &
                                                              HT, &
                                                            PSIM, &
                                                            PSIH, &
                                                          GZ1OZ0, &
                                                              BR, &
                                                               F, &
                                                         CHKLOWQ

   REAL,       DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)    ::                           TSK, &
                                                             UST, &
                                                            PBLH, &
                                                             HFX, &
                                                             QFX, &
                                                             ZNT, &
                                                            QSFC, &
                                                            AKHS, &
                                                            AKMS, &
                                                           MIXHT, &
                                                             QZ0, &
                                                            THZ0, &
                                                             UZ0, &
                                                             VZ0, &
                                                              CT, &
                                                          GRDFLX, &
                                                             U10, &
                                                             V10, &
                                                            WSPD


   REAL,       DIMENSION( ims:ime, kms:kme, jms:jme ),            &
               INTENT(INOUT)    ::                       RUBLTEN, &
                                                         RVBLTEN, &
                                                        RTHBLTEN, &
                                                  EXCH_H,EXCH_M,TKE_MYJ

   REAL,       DIMENSION( ims:ime, kms:kme, jms:jme ), &
        &OPTIONAL, INTENT(INOUT) :: &
        & qke,tsq,qsq,cov

   REAL,    DIMENSION( ims:ime , jms:jme ), &
        &OPTIONAL, INTENT(IN) ::  &
        & qcg, rmol, ch

   
   INTEGER, OPTIONAL, INTENT(IN)  :: grav_settling
   




   REAL,       DIMENSION( ims:ime, kms:kme, jms:jme ),            &
               INTENT(OUT)    ::                          EL_MYJ

   REAL ,                             INTENT(IN   )  ::  u_frame, &
                                                         v_frame


   INTEGER,    DIMENSION( ims:ime , jms:jme ),                    &
               INTENT(INOUT) ::                             KPBL

   REAL,       DIMENSION( ims:ime , jms:jme ),                    &
               INTENT(IN)    :: XICE, SNOW, LH


   real, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) ::FRC_URB2D   
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_u_bep        
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_v_bep        
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_t_bep        
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_q_bep        

   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_e_bep        
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_u_bep        
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_v_bep        
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_t_bep        
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_q_bep        

   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_e_bep        

   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::dlg_bep        
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::dl_u_bep        

   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::sf_bep           
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::vl_bep            
 













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
                     ,qi_curr, qs_curr, qg_curr                  &
                     ,rqvblten,rqcblten,rqrblten                 &
                     ,rqiblten,rqsblten,rqgblten

   REAL,       DIMENSION( ims:ime, jms:jme )                    , &
               OPTIONAL                                         , &
               INTENT(INOUT)    ::                           HOL, &
                                                             MOL, &
                                                          REGIME
   REAL,       DIMENSION( ims:ime, jms:jme )                    , &
               OPTIONAL                                         , &
               INTENT(IN)    ::                           mut

   INTEGER,    OPTIONAL, INTENT(IN)    ::               gwd_opt
   REAL,       OPTIONAL, INTENT(IN)    ::               p_top

  real,   dimension( ims:ime, jms:jme )                                      , &
          optional                                                           , &
             intent(inout  )   ::                                      dusfcg, &
                                                                       dvsfcg

  real,   dimension( ims:ime, jms:jme )                                      , &
          optional                                                           , &
             intent(in  )   ::                                          var2d, &
                                                                        oc12d, &
                                                              oa1,oa2,oa3,oa4, &
                                                              ol1,ol2,ol3,ol4



   REAL,       DIMENSION( ims:ime, kms:kme, jms:jme ) ::v_phytmp
   REAL,       DIMENSION( ims:ime, kms:kme, jms:jme ) ::u_phytmp

   REAL,       DIMENSION( ims:ime, jms:jme )          ::  TSKOLD, &
                                                          USTOLD, &
                                                          ZNTOLD, &
                                                             ZOL, &
                                                            PSFC

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme )::a_u        
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme )::a_v        
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme )::a_t        
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme )::a_q        

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme )::b_u        
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme )::b_v        
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme )::b_t        
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme )::b_q        

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme )::sf           
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme )  ::vl            
   REAL    :: DTMIN,DTBL


   INTEGER :: initflag




   INTEGER :: i,J,K,NK,jj,ij,its,ite,jts,jte
   LOGICAL :: radiation
   LOGICAL :: flag_bep
   LOGICAL :: flag_myjsfc
   LOGICAL :: flag_qv, flag_qc, flag_qr, flag_qi, flag_qs, flag_qg
   CHARACTER*256 :: message
   REAL    :: next_bl_time
   LOGICAL :: run_param
   LOGICAL :: do_adapt
   integer iu_bep,iurb,idiff
   real seamask,thsk,zzz,unew,vnew,tnew,qnew,umom,vmom




    SELECT CASE(sf_urban_physics)
      CASE DEFAULT
      flag_bep=.false.
    End Select
    SELECT CASE(sf_sfclay_physics)
    CASE (MYJSFCSCHEME)
       flag_myjsfc=.true.
    CASE DEFAULT
       flag_myjsfc=.false.
    END SELECT

  flag_qv = .FALSE. ; IF ( PRESENT( F_QV ) ) flag_qv = F_QV
  flag_qc = .FALSE. ; IF ( PRESENT( F_QC ) ) flag_qc = F_QC
  flag_qr = .FALSE. ; IF ( PRESENT( F_QR ) ) flag_qr = F_QR
  flag_qi = .FALSE. ; IF ( PRESENT( F_QI ) ) flag_qi = F_QI
  flag_qs = .FALSE. ; IF ( PRESENT( F_QS ) ) flag_qs = F_QS
  flag_qg = .FALSE. ; IF ( PRESENT( F_QG ) ) flag_qg = F_QG




  if (bl_pbl_physics .eq. 0) return




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

 IF (run_param) THEN
  radiation = .false.
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


       idiff=0


   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij,i,j,k )

   DO ij = 1 , num_tiles
      DO j=j_start(ij),j_end(ij)
      DO i=i_start(ij),i_end(ij)
         TSKOLD(i,j)=TSK(i,j)
         USTOLD(i,j)=UST(i,j)
         ZNTOLD(i,j)=ZNT(i,j)





         DO k=kts,kte
            v_phytmp(i,k,j)=v_phy(i,k,j)+v_frame
            u_phytmp(i,k,j)=u_phy(i,k,j)+u_frame
         ENDDO



         PSFC(I,J)=p8w(I,kms,J)

         DO k=kts,min(kte+1,kde)
            RTHBLTEN(I,K,J)=0.
            RUBLTEN(I,K,J)=0.
            RVBLTEN(I,K,J)=0.
            IF ( PRESENT( RQCBLTEN )) RQCBLTEN(I,K,J)=0.
            IF ( PRESENT( RQVBLTEN )) RQVBLTEN(I,K,J)=0.
         ENDDO

         IF (flag_QI .AND. PRESENT(RQIBLTEN) ) THEN
            DO k=kts,min(kte+1,kde)
               RQIBLTEN(I,K,J)=0.
            ENDDO
         ENDIF
      ENDDO
      ENDDO

      
   ENDDO
   !$OMP END PARALLEL DO

  !$OMP PARALLEL DO   &
  !$OMP PRIVATE ( ij, i,j,k, its, ite, jts, jte )
  DO ij = 1 , num_tiles

   its = i_start(ij)
   ite = i_end(ij)
   jts = j_start(ij)
   jte = j_end(ij)

   pbl_select: SELECT CASE(bl_pbl_physics)

      CASE (YSUSCHEME)
        CALL wrf_debug(100,'in YSU PBL')
           IF ( PRESENT( qv_curr )  .AND. PRESENT( qc_curr )  .AND. &
                PRESENT( qi_curr )                            .AND. &
                PRESENT( rqvblten ) .AND. PRESENT( rqcblten ) .AND. &
                PRESENT( rqiblten )                           .AND. &
                PRESENT( hol      ) ) THEN
             CALL ysu(                                              &
               U3D=u_phytmp,V3D=v_phytmp,TH3D=th_phy,T3D=t_phy      &
              ,QV3D=qv_curr,QC3D=qc_curr,QI3D=qi_curr               &
              ,P3D=p_phy,P3DI=p8w,PI3D=pi_phy                       &
              ,RUBLTEN=rublten,RVBLTEN=rvblten                      &
              ,RTHBLTEN=rthblten,RQVBLTEN=rqvblten                  &
              ,RQCBLTEN=rqcblten,RQIBLTEN=rqiblten                  &
              ,FLAG_QI=flag_qi                                      &
              ,CP=cp,G=g,ROVCP=rcp,RD=r_D,ROVG=rovg                 &
              ,DZ8W=dz8w,Z=z,XLV=XLV,RV=r_v,PSFC=PSFC               &
              ,ZNU=znu,ZNW=znw,MUT=mut,P_TOP=p_top                  &
              ,ZNT=znt,UST=ust,ZOL=zol,HOL=hol,HPBL=pblh            &
              ,PSIM=psim,PSIH=psih,XLAND=xland                      &
              ,HFX=hfx,QFX=qfx,TSK=tskold,GZ1OZ0=gz1oz0             &
              ,U10=u10,V10=v10                                      &
              ,WSPD=wspd,BR=br,DT=dtbl,DTMIN=dtmin,KPBL2D=kpbl      &
              ,SVP1=svp1,SVP2=svp2,SVP3=svp3,SVPT0=svpt0            &
              ,EP1=ep_1,EP2=ep_2,KARMAN=karman,EOMEG=eomeg          &
              ,STBOLT=stbolt,EXCH_H=exch_h,REGIME=regime            &
              ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde      &
              ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme      &
              ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte      &
                                                                    )
           ELSE
               CALL wrf_error_fatal3("",616,&
'Lack arguments to call YSU pbl')
           ENDIF

      CASE (MRFSCHEME)
           IF ( PRESENT( qv_curr )  .AND. PRESENT( qc_curr )  .AND. &
                PRESENT( rqvblten ) .AND. PRESENT( rqcblten ) .AND. &
                PRESENT( hol      )                           .AND. &
                                                        .TRUE.  ) THEN

             CALL wrf_debug(100,'in MRF')
             CALL mrf(                                              &
               U3D=u_phytmp,V3D=v_phytmp,TH3D=th_phy,T3D=t_phy      &
              ,QV3D=qv_curr                                         &
              ,QC3D=qc_curr                                         &
              ,QI3D=qi_curr                                         &
              ,P3D=p_phy,PI3D=pi_phy                                &
              ,RUBLTEN=rublten,RVBLTEN=rvblten                      &
              ,RTHBLTEN=rthblten,RQVBLTEN=rqvblten                  &
              ,RQCBLTEN=rqcblten,RQIBLTEN=rqiblten                  &
              ,CP=cp,G=g,ROVCP=rcp,R=r_d,ROVG=rovg                  &
              ,DZ8W=dz8w,Z=z,XLV=xlv,RV=r_v,PSFC=psfc               &
              ,P1000MB=p1000mb                                      &
              ,ZNT=znt,UST=ust,ZOL=zol,HOL=hol                      &
              ,PBL=pblh,PSIM=psim,PSIH=psih                         &
              ,XLAND=xland,HFX=hfx,QFX=qfx,TSK=tskold               &
              ,GZ1OZ0=gz1oz0,WSPD=wspd,BR=br                        &
              ,DT=dtbl,DTMIN=dtmin,KPBL2D=kpbl                      &
              ,SVP1=svp1,SVP2=svp2,SVP3=svp3,SVPT0=svpt0            &
              ,EP1=ep_1,EP2=ep_2,KARMAN=karman,EOMEG=eomeg          &
              ,STBOLT=stbolt,REGIME=regime                          &
              ,FLAG_QI=flag_qi                                      &
              ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde      &
              ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme      &
              ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte      &
                                                                    )
           ELSE
               CALL wrf_error_fatal3("",653,&
'Lack arguments to call MRF pbl')
           ENDIF

      CASE (GFSSCHEME)
           IF ( PRESENT( qv_curr )  .AND. PRESENT( qc_curr )  .AND. &
                PRESENT( rqvblten ) .AND. PRESENT( rqcblten ) .AND. &
                                                        .TRUE.  ) THEN
             CALL wrf_debug(100,'in GFS')
             CALL bl_gfs(                                           &
               U3D=u_phytmp,V3D=v_phytmp                            &
              ,TH3D=th_phy,T3D=t_phy                                &
              ,QV3D=qv_curr,QC3D=qc_curr,QI3D=qi_curr               &
              ,P3D=p_phy,PI3D=pi_phy                                &
              ,RUBLTEN=rublten,RVBLTEN=rvblten,RTHBLTEN=rthblten    &
              ,RQVBLTEN=rqvblten,RQCBLTEN=rqcblten                  &
              ,RQIBLTEN=rqiblten                                    &
              ,CP=cp,G=g,ROVCP=rcp,R=r_d,ROVG=rovg,FLAG_QI=flag_qi  &
              ,DZ8W=dz8w,z=z,PSFC=psfc                              &
              ,UST=ust,PBL=pblh,PSIM=psim,PSIH=psih                 &
              ,HFX=hfx,QFX=qfx,TSK=tskold,GZ1OZ0=gz1oz0             &
              ,WSPD=wspd,BR=br                                      &
              ,DT=dtbl,KPBL2D=kpbl,EP1=ep_1,KARMAN=karman           &
              ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde      &
              ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme      &
              ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte      &
                                                                    )
           ELSE
               CALL wrf_error_fatal3("",681,&
'Lack arguments to call GFS pbl')
           ENDIF

      CASE (MYJPBLSCHEME)
           IF ( PRESENT( qv_curr )  .AND. PRESENT( qc_curr )  .AND. &
                PRESENT( rqvblten ) .AND. PRESENT( rqcblten ) .AND. &
                                                        .TRUE.  ) THEN

             CALL wrf_debug(100,'in MYJPBL')
            IF ( .not.flag_bep .and. idiff.ne.1) THEN
             CALL myjpbl(DT=dt,STEPBL=stepbl,HT=ht,DZ=dz8w          &
              ,PMID=p_phy,PINT=p8w,TH=th_phy,T=t_phy,EXNER=pi_phy   &
              ,QV=qv_curr,QCW=qc_curr,QCI=qi_curr,QCS=qs_curr       & 
              ,QCR=qr_curr,QCG=qg_curr                              & 
              ,U=u_phy,V=v_phy,RHO=rho                              &
              ,TSK=tsk,QSFC=qsfc,CHKLOWQ=chklowq,THZ0=thz0          &
              ,QZ0=qz0,UZ0=uz0,VZ0=vz0                              &
              ,LOWLYR=lowlyr                                        &
              ,XLAND=xland,SICE=xice,SNOW=snow                      &
              ,TKE_MYJ=tke_myj,EXCH_H=exch_h,USTAR=ust,ZNT=znt      &
              ,EL_MYJ=el_myj,PBLH=pblh,KPBL=kpbl,CT=ct              &
              ,AKHS=akhs,AKMS=akms,ELFLX=lh,MIXHT=mixht             &  
              ,RUBLTEN=rublten,RVBLTEN=rvblten,RTHBLTEN=rthblten    &
              ,RQVBLTEN=rqvblten,RQCBLTEN=rqcblten                  &
              ,RQIBLTEN=rqiblten,RQSBLTEN=rqsblten                  & 
              ,RQRBLTEN=rqrblten,RQGBLTEN=rqgblten                  & 
              ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde      &
              ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme      &
              ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte      &
                                                                    )
            ELSE 


             CALL myjurb(IDIFF=idiff,FLAG_BEP=flag_bep              &
              ,DT=dtbl,STEPBL=stepbl,HT=ht,DZ=dz8w                  &
              ,PMID=p_phy,PINT=p8w,TH=th_phy,T=t_phy,EXNER=pi_phy   &
              ,QV=qv_curr, CWM=qc_curr                              &
              ,U=u_phy,V=v_phy,RHO=rho                              &
              ,TSK=tsk,QSFC=qsfc,CHKLOWQ=chklowq,THZ0=thz0          &
              ,QZ0=qz0,UZ0=uz0,VZ0=vz0                              &
              ,LOWLYR=lowlyr                                        &
              ,XLAND=xland,SICE=xice,SNOW=snow                      &
              ,TKE_MYJ=tke_myj,EXCH_H=exch_h,EXCH_M=exch_m          &
              ,USTAR=ust,ZNT=znt                                    &
              ,EL_MYJ=el_myj,PBLH=pblh,KPBL=kpbl,CT=ct              &
              ,AKHS=akhs,AKMS=akms,ELFLX=lh                         &
              ,RUBLTEN=rublten,RVBLTEN=rvblten,RTHBLTEN=rthblten    &
              ,RQVBLTEN=rqvblten,RQCBLTEN=rqcblten                  &

              ,FRC_URB2D=frc_urb2d                                  &
              ,A_U_BEP=a_u_bep,A_V_BEP=a_v_bep,A_T_BEP=a_t_bep      &
              ,A_Q_BEP=a_q_bep                                      &
              ,A_E_BEP=a_e_bep,B_U_BEP=b_u_bep,B_V_BEP=b_v_bep      &
              ,B_T_BEP=b_t_bep,B_Q_BEP=b_q_bep                      &
              ,B_E_BEP=b_e_bep,DLG_BEP=dlg_bep                      &
              ,DL_U_BEP=dl_u_bep,SF_BEP=sf_bep,VL_BEP=vl_bep        &

              ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde      &
              ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme      &
              ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte      &
                                                                    )
            ENDIF

           ELSE
               CALL wrf_error_fatal3("",746,&
'Lack arguments to call MYJ pbl')
           ENDIF
 
      CASE (QNSEPBLSCHEME)
           IF ( PRESENT( qv_curr )  .AND. PRESENT( qc_curr )  .AND. &
                PRESENT( rqvblten ) .AND. PRESENT( rqcblten ) .AND. &
                                                        .TRUE.  ) THEN
             CALL wrf_debug(100,'in QNSEPBL')
             CALL qnsepbl(                                           &
               DT=dt,STEPBL=stepbl,HT=ht,DZ=dz8w                    &
              ,PMID=p_phy,PINT=p8w,TH=th_phy,T=t_phy,EXNER=pi_phy   &
              ,QV=qv_curr, CWM=qc_curr                              &
              ,U=u_phy,V=v_phy,RHO=rho                              &
              ,TSK=tsk,QSFC=qsfc,CHKLOWQ=chklowq,THZ0=thz0          &
              ,QZ0=qz0,UZ0=uz0,VZ0=vz0,CORF=f                       &
              ,LOWLYR=lowlyr                                        &
              ,XLAND=xland,SICE=xice,SNOW=snow                      &
              ,TKE=tke_myj,EXCH_H=exch_h,EXCH_M=exch_m,USTAR=ust,ZNT=znt      &
              ,EL_MYJ=el_myj,PBLH=pblh,KPBL=kpbl,CT=ct              &
              ,AKHS=akhs,AKMS=akms,ELFLX=lh                         &
              ,RUBLTEN=rublten,RVBLTEN=rvblten,RTHBLTEN=rthblten    &
              ,RQVBLTEN=rqvblten,RQCBLTEN=rqcblten                  &
              ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde      &
              ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme      &
              ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte      &
                                                                    )
           ELSE
               CALL wrf_error_fatal3("",774,&
'Lack arguments to call QNSE pbl')
           ENDIF

      CASE (ACMPBLSCHEME)
           
           
           IF ( PRESENT( qv_curr )  .AND. PRESENT( qc_curr )  .AND. &
                PRESENT( rqvblten ) .AND. PRESENT( rqcblten ) .AND. &
                                                        .TRUE.  ) THEN
             CALL wrf_debug(100,'in ACM PBL')

             CALL ACMPBL(                                                        &
               XTIME=itimestep, DTPBL=dtbl, ZNW=znw, SIGMAH=znu               &
              ,U3D=u_phytmp, V3D=v_phytmp, PP3D=p_phy, DZ8W=dz8w, TH3D=th_phy, T3D=t_phy            &
              ,QV3D=qv_curr, QC3D=qc_curr, QI3D=qi_curr, RR3D=rho                &
              ,UST=UST, HFX=HFX, QFX=QFX, TSK=tsk                               &
              ,PSFC=PSFC, EP1=EP_1, G=g, ROVCP=rcp,RD=r_D,CPD=cp                 &
              ,PBLH=pblh, KPBL2D=kpbl, REGIME=regime                            &
              ,GZ1OZ0=gz1oz0,WSPD=wspd,PSIM=psim, MUT=mut                        &
              ,RUBLTEN=rublten,RVBLTEN=rvblten,RTHBLTEN=rthblten                 &
              ,RQVBLTEN=rqvblten,RQCBLTEN=rqcblten,RQIBLTEN=rqiblten             &
              ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde                   &
              ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme                   &
              ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte                   &   
                                                                      )
           ELSE
               CALL wrf_error_fatal3("",801,&
'Lack arguments to call ACM2 pbl')
           ENDIF



     CASE DEFAULT

       WRITE( message , * ) 'The pbl option does not exist: bl_pbl_physics = ', bl_pbl_physics
       CALL wrf_error_fatal3("",810,&
message )

   END SELECT pbl_select

   IF (PRESENT(gwd_opt)) THEN
       IF(gwd_opt .EQ. 1)THEN
             CALL gwdo(                                              &
               U3D=u_phytmp,V3D=v_phytmp,T3D=t_phy      &
              ,QV3D=qv_curr                                         &
              ,P3D=p_phy,P3DI=p8w,PI3D=pi_phy,Z=z                        &
              ,RUBLTEN=rublten,RVBLTEN=rvblten                      &
              ,DUSFCG=dusfcg,DVSFCG=dvsfcg &
              ,VAR2D=var2d,OC12D=oc12d     &
              ,OA2D1=oa1,OA2D2=oa2,OA2D3=oa3,OA2D4=oa4  &
              ,OL2D1=ol1,OL2D2=ol2,OL2D3=ol3,OL2D4=ol4  &
              ,CP=cp,G=g,RD=r_d                           &
              ,RV=r_v,EP1=ep_1,PI=3.141592653                        &
              ,DT=dtbl,DX=dx,KPBL2D=kpbl,ITIMESTEP=itimestep      &
              ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde      &
              ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme      &
              ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte      )
       ENDIF
   ENDIF



   ENDDO
   !$OMP END PARALLEL DO

   ENDIF

 


   END SUBROUTINE pbl_driver


          SUBROUTINE diff3d(DT,CP,DZ,TH ,QV,QC,T,U,V,RHO                              &
              ,EXCH_H,EXCH_M                   &  
              ,RUBLTEN,RVBLTEN,RTHBLTEN    &
              ,RQVBLTEN,RQCBLTEN                  &
              ,WU,WV,WT,WQ                 &
              ,A_U,A_V,A_T,A_Q      &
              ,B_U,B_V,B_T,B_Q      &
              ,SF,VL        &
              ,IDS,IDE,JDS,JDE,KDS,KDE      &
              ,IMS,IME,JMS,JME,KMS,KME      &
              ,ITS,ITE,JTS,JTE,KTS,KTE      &
                                                                    )



















      IMPLICIT NONE


      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE


      real DT,CP
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: DZ 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: TH 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: QV 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: QC 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: T  
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: U 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: V 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: RHO 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: EXCH_H 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: EXCH_M 
      
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: A_U 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: B_U 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: A_V 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: B_V 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: A_T 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: B_T 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: A_Q 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: B_Q 
    
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: VL 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: SF 

      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT) :: RUBLTEN 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT) :: RVBLTEN 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT) :: RTHBLTEN 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT) :: RQVBLTEN 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT) :: RQCBLTEN 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT) :: WU 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT) :: WV 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT) :: WT 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT) :: WQ 

     REAL ELOCP 


      real u1d(kms:kme),v1d(kms:kme),exch_h1d(kms:kme)
      real the1d(kms:kme) 
      real exch_m1d(kms:kme),qv1d(kms:kme),qc1d(kms:kme)
      real dz1d(kms:kme),rho1d(kms:kme),rhoz1d(kms:kme)
      real sf1d(kms:kme),vl1d(kms:kme)   
      real a_u1d(kms:kme),b_u1d(kms:kme)
      real a_v1d(kms:kme),b_v1d(kms:kme)
      real a_t1d(kms:kme),b_t1d(kms:kme)
      real a_q1d(kms:kme),b_q1d(kms:kme)
      real a_qc1d(kms:kme),b_qc1d(kms:kme)
      real wu1d(kms:kme),wv1d(kms:kme),wt1d(kms:kme),wq1d(kms:kme),wqc1d(kms:kme)
      real thnew

      integer i,k,j  

      ELOCP=2.72E6/CP
      u1d=0.
      v1d=0.
      exch_h1d=0.
      exch_m1d=0.
      qv1d=0.
      qc1d=0.
      dz1d=0.
      rho1d=0.
      rhoz1d=0.
      sf1d=0.
      vl1d=0.
      a_u1d=0.
      a_v1d=0.
      a_t1d=0.
      a_q1d=0.
      a_qc1d=0.
      b_u1d=0.
      b_v1d=0.
      b_t1d=0.
      b_q1d=0.
      b_qc1d=0.
       
      do j=jts,jte
      do i=its,ite



       do k=kts,kte
        u1d(k)=U(i,k,j)
        v1d(k)=V(i,k,j)
        the1d(k)=TH(i,k,j)*(QC(i,k,j)*(-ELOCP/T(i,k,j))+1)
        qv1d(k)=qv(i,k,j)
        dz1d(k)=dz(i,k,j)
        rho1d(k)=rho(i,k,j) 
        a_u1d(k)=a_u(i,k,j)
        b_u1d(k)=b_u(i,k,j)
        a_v1d(k)=a_v(i,k,j)
        b_v1d(k)=b_v(i,k,j)
        a_t1d(k)=a_t(i,k,j)
        b_t1d(k)=b_t(i,k,j)
        a_q1d(k)=a_q(i,k,j)
        b_q1d(k)=b_q(i,k,j)
        a_qc1d(k)=0.
        b_qc1d(k)=0.
        vl1d(k)=vl(i,k,j)
        sf1d(k)=sf(i,k,j)
       enddo
       sf1d(kte+1)=1. 
       do k=kts,kte    
        exch_h1d(k)=exch_h(i,k,j)
        exch_m1d(k)=exch_m(i,k,j)
       enddo
       exch_h1d(kts)=0.

       exch_m1d(kts)=0.

        rhoz1d(kts)=rho1d(kts)
        do k=kts+1,kte
         rhoz1d(k)=(rho1d(k)*dz1d(k-1)+rho1d(k-1)*dz1d(k))/       &
     &                      (dz1d(k-1)+dz1d(k))
        enddo
        rhoz1d(kte+1)=rho1d(kte)



          
       call diff(kms,kme,kts,kte,dt,u1d,rho1d,rhoz1d,exch_m1d,a_u1d,b_u1d,sf1d, &
     &            vl1d,dz1d,wu1d) 



       call diff(kms,kme,kts,kte,dt,v1d,rho1d,rhoz1d,exch_m1d,a_v1d,b_v1d,sf1d, &
     &            vl1d,dz1d,wv1d) 



       call diff(kms,kme,kts,kte,dt,the1d,rho1d,rhoz1d,exch_h1d,a_t1d,b_t1d,sf1d, &
     &            vl1d,dz1d,wt1d) 



       call diff(kms,kme,kts,kte,dt,qv1d,rho1d,rhoz1d,exch_h1d,a_q1d,b_q1d,sf1d, &
     &            vl1d,dz1d,wq1d) 



       call diff(kms,kme,kts,kte,dt,qc1d,rho1d,rhoz1d,exch_h1d,a_qc1d,b_qc1d,sf1d, &
     &            vl1d,dz1d,wqc1d)        




        do k=kts,kte
          rublten(i,k,j)=(u1d(k)-u(i,k,j))/dt
          rvblten(i,k,j)=(v1d(k)-v(i,k,j))/dt
          thnew=the1d(k)/(QC(i,k,j)*(-ELOCP/T(i,k,j))+1)
          rthblten(i,k,j)=(thnew-th(i,k,j))/dt
          rqvblten(i,k,j)=(qv1d(k)-qv(i,k,j))/dt
          rqcblten(i,k,j)=(qc1d(k)-qc(i,k,j))/dt
          wu(i,k,j)=wu1d(k)
          wv(i,k,j)=wv1d(k)
          wt(i,k,j)=wt1d(k)
          wq(i,k,j)=wq1d(k)
        enddo
      enddo
      enddo 


        
      END SUBROUTINE diff3d



       subroutine diff(kms,kme,kts,kte,dt,co,da,daz,cd,aa,bb,sf,vl,dz,fc)




















        implicit none
        integer iz,iz1,izf
        integer kms,kme,kts,kte
        real dt,dzv
        real co(kms:kme),cd(kms:kme),dz(kms:kme)
        real da(kms:kme),daz(kms:kme)
        real cddz(kms:kme),fc(kms:kme),df(kms:kme)
        real a(kms:kme,3),c(kms:kme)
        real sf(kms:kme),vl(kms:kme)
        real aa(kms:kme),bb(kms:kme)

        


        cddz(kts)=sf(kts)*daz(kts)*cd(kts)/dz(kts)
        do iz=kts+1,kte
         cddz(iz)=2.*sf(iz)*daz(iz)*cd(iz)/(dz(iz)+dz(iz-1))
        enddo
        cddz(kte+1)=sf(kte+1)*daz(kte+1)*cd(kte+1)/dz(kte)

          iz1=1
          izf=1

          do iz=iz1,kte-1

           dzv=vl(iz)*dz(iz)
           a(iz,1)=-cddz(iz)*dt/dzv/da(iz)
           a(iz,2)=1+dt*(cddz(iz)+cddz(iz+1))/dzv/da(iz)-aa(iz)*dt
           a(iz,3)=-cddz(iz+1)*dt/dzv/da(iz)
           c(iz)=co(iz)+bb(iz)*dt
          enddo

          do iz=kte-(izf-1),kte
           a(iz,1)=0.
           a(iz,2)=1
           a(iz,3)=0.
           c(iz)=co(iz)
          enddo
          call invert (kms,kme,kts,kte,a,c,co)
           


          do iz=kts,iz1
           fc(iz)=0.
          enddo

          do iz=iz1+1,kte
           fc(iz)=-(cddz(iz)*(co(iz)-co(iz-1)))/da(iz)
          enddo



       return
       end subroutine diff


       subroutine invert(kms,kme,kts,kte,a,c,x)













       implicit none
       integer kms,kme,kts,kte,in
       real a(kms:kme,3),c(kms:kme),x(kms:kme)

        do in=kte-1,kts,-1
         c(in)=c(in)-a(in,3)*c(in+1)/a(in+1,2)
         a(in,2)=a(in,2)-a(in,3)*a(in+1,1)/a(in+1,2)
        enddo

        do in=kts+1,kte
         c(in)=c(in)-a(in,1)*c(in-1)/a(in-1,2)
        enddo

        do in=kts,kte
         x(in)=c(in)/a(in,2)
        enddo

        return
        end subroutine invert















END MODULE module_pbl_driver
