


MODULE module_cumulus_driver
CONTAINS
   SUBROUTINE cumulus_driver(grid                                     &
                 
                     ,ids,ide, jds,jde, kds,kde                       &
                     ,ims,ime, jms,jme, kms,kme                       &
                     ,ips,ipe, jps,jpe, kps,kpe                       &
                     ,i_start,i_end,j_start,j_end,kts,kte,num_tiles   &
                 
                 
                     ,u,v,th,t,w                                      &
                     ,p,pi,rho                                        &
                 
                     ,itimestep,dt,dx,cudt,curr_secs,adapt_step_flag  &
                     ,rainc,raincv,pratec,nca                         &
                     ,dz8w,p8w,forcet,forceq                          &
                     ,w0avg,stepcu,gsw                                &
                     ,cldefi,lowlyr,xland,cu_act_flag,warm_rain       &
                     ,htop,hbot,kpbl,ht                               &  
                     ,ensdim,maxiens,maxens,maxens2,maxens3           &
                     ,periodic_x,periodic_y                           &
                 
                     ,cu_physics                                      &
                 
                     ,qv_curr, qc_curr, qr_curr                       &
                     ,qi_curr, qs_curr, qg_curr                       & 
                     ,qv_prev, qc_prev, qr_prev                       & 
                     ,qi_prev, qs_prev, qg_prev                       &
                 
                     ,apr_gr,apr_w,apr_mc,apr_st,apr_as,apr_capma     &
                     ,apr_capme,apr_capmi,edt_out,clos_choice         &
                     ,mass_flux,xf_ens,pr_ens,cugd_avedx,imomentum    &
                     ,cugd_tten,cugd_qvten,cugd_qcten                 &
                     ,cugd_ttens,cugd_qvtens                          &
                     ,gd_cloud,gd_cloud2      &
                 
                     ,rqvcuten,rqccuten,rqrcuten                      &
                     ,rqicuten,rqscuten,rqgcuten                      &
                     ,rqvblten,rqvften                                &
                     ,rthcuten,rthraten,rthblten,rthften              &
                 
                     ,f_qv,f_qc,f_qr                                  &
                     ,f_qi,f_qs,f_qg                                  &
                                                                      )

   USE module_model_constants
   USE module_state_description, ONLY:     KFSCHEME,BMJSCHEME         &
                                          ,KFETASCHEME,GDSCHEME       &
                                          ,G3SCHEME                   &
                                          ,SASSCHEME



   USE module_cu_kf
   USE module_cu_bmj
   USE module_dm
   USE module_domain, ONLY: domain
   USE module_cu_kfeta
   USE module_cu_gd, ONLY : GRELLDRV
   USE module_cu_g3, ONLY : G3DRV,CONV_GRELL_SPREAD3D
   USE module_cu_sas

   
   
   
   
   

   IMPLICIT NONE







































































































































   INTEGER,      INTENT(IN   )    ::                             &
                                      ids,ide, jds,jde, kds,kde, &
                                      ims,ime, jms,jme, kms,kme, &
                                                        kts,kte, &
                                      itimestep, num_tiles
   LOGICAL periodic_x, periodic_y
   TYPE(domain) , INTENT(INOUT)          :: grid
   INTEGER, DIMENSION(num_tiles), INTENT(IN) ::                       &
     &           i_start,i_end,j_start,j_end

   INTEGER,      INTENT(IN   )    ::                             &
                  ensdim,maxiens,maxens,maxens2,maxens3

   INTEGER, OPTIONAL,     INTENT(IN   )    ::                    &
                   cugd_avedx,clos_choice

   INTEGER,      INTENT(IN   )    ::   cu_physics
   INTEGER,      INTENT(IN   )    ::   STEPCU
   LOGICAL,      INTENT(IN   )    ::   warm_rain

   INTEGER,DIMENSION( ims:ime, jms:jme ),                        &
           INTENT(IN ) ::                                LOWLYR

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         INTENT(IN ) ::                                          &
                                                           dz8w  &
                                                      ,     p8w  &
                                                      ,       p  &
                                                      ,      pi  &
                                                      ,       u  &
                                                      ,       v  &
                                                      ,      th  &
                                                      ,       t  &
                                                      ,     rho  &
                                                      ,       w

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         INTENT(INOUT)  ::                                       &
                                                          W0AVG

   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN) ::           &
                                                 GSW,HT,XLAND

   REAL, DIMENSION( ims:ime , jms:jme ),                         &
          INTENT(INOUT) ::                                RAINC  &
                                                    ,    RAINCV  &
                                                    ,       NCA  & 
                                                    ,      HTOP  & 
                                                    ,      HBOT  &
                                                    ,    CLDEFI 
 

   REAL, DIMENSION( ims:ime , jms:jme ),INTENT(INOUT),OPTIONAL :: &
        PRATEC
   REAL, DIMENSION( ims:ime , jms:jme ) :: tmppratec
                                                    
   INTEGER, DIMENSION( ims:ime , jms:jme ),                      &
                    INTENT(IN) ::                          KPBL


   LOGICAL, DIMENSION( ims:ime , jms:jme ),                      &
          INTENT(INOUT) :: CU_ACT_FLAG

   REAL,  INTENT(IN   ) :: DT, DX
   INTEGER,      INTENT(IN   ),OPTIONAL    ::                             &
                                      ips,ipe, jps,jpe, kps,kpe,imomentum
   REAL,  INTENT(IN   ),OPTIONAL :: CUDT
   REAL,  INTENT(IN   ),OPTIONAL :: CURR_SECS
   LOGICAL,INTENT(IN   ),OPTIONAL    ::     adapt_step_flag
   REAL   :: cudt_pass, curr_secs_pass
   LOGICAL :: adapt_step_flag_pass




   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         OPTIONAL, INTENT(INOUT) ::                              &
                      
                      
                      qv_curr, qc_curr, qr_curr                  &
                     ,qi_curr, qs_curr, qg_curr                  & 
                     ,qv_prev, qc_prev, qr_prev                  & 
                     ,qi_prev, qs_prev, qg_prev                  &
                      
                     ,rqvcuten,rqccuten,rqrcuten                 &
                     ,rqicuten,rqscuten,rqgcuten                 &
                     ,rqvblten,rqvften                           &
                     ,rthraten,rthblten                          &
                     ,cugd_tten,cugd_qvten,cugd_qcten            &
                     ,cugd_ttens,cugd_qvtens                     &
                                                      ,   forcet &
                                                      ,   forceq &
                     ,rthften,rthcuten

   REAL, DIMENSION( ims:ime , jms:jme ),                         &
                    OPTIONAL,                                    &
                    INTENT(INOUT) ::                             &
                apr_gr,apr_w,apr_mc,apr_st,apr_as,apr_capma      &
               ,apr_capme,apr_capmi,edt_out                      &
                                                    , MASS_FLUX
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         OPTIONAL, INTENT(INOUT) ::                              &
                  GD_CLOUD,GD_CLOUD2
   REAL, DIMENSION( ims:ime , jms:jme , 1:ensdim ),              &
          OPTIONAL,                                              &
          INTENT(INOUT) ::                       XF_ENS, PR_ENS








   LOGICAL, INTENT(IN), OPTIONAL ::                             &
                                                      f_qv      &
                                                     ,f_qc      &
                                                     ,f_qr      &
                                                     ,f_qi      &
                                                     ,f_qs      &
                                                     ,f_qg




   INTEGER :: i,j,k,its,ite,jts,jte,ij



   if (.not. PRESENT(CURR_SECS)) then
      curr_secs_pass = -1
   else 
      curr_secs_pass = curr_secs
   endif

   if (.not. PRESENT(CUDT)) then
      cudt_pass = -1
   else
      cudt_pass = cudt
   endif

   if (.not. PRESENT(adapt_step_flag)) then
      adapt_step_flag_pass = .false.
   else
      adapt_step_flag_pass = adapt_step_flag
   endif

   

   if ( PRESENT ( pratec ) ) then
      tmppratec(:,:) = pratec(:,:)
   else
      tmppratec(:,:) = 0.
   end if


   IF (cu_physics .eq. 0) return





      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij ,its,ite,jts,jte, i,j,k)

      DO ij = 1 , num_tiles
        its = i_start(ij)
        ite = i_end(ij)
        jts = j_start(ij)
        jte = j_end(ij)


   cps_select: SELECT CASE(cu_physics)

     CASE (KFSCHEME)
          CALL wrf_debug(100,'in kfcps')

          CALL KFCPS(                                           &
              
                DT=dt ,KTAU=itimestep ,DX=dx , CUDT=cudt_pass   &
               ,CURR_SECS=curr_secs_pass                        &
               ,ADAPT_STEP_FLAG=adapt_step_flag_pass            &
               ,RHO=rho                                         &
               ,U=u ,V=v ,TH=th ,T=t ,W=w                       &
               ,PCPS=p ,PI=pi                                   &
               ,XLV0=xlv0 ,XLV1=xlv1 ,XLS0=xls0 ,XLS1=xls1      &
               ,RAINCV=raincv, PRATEC=tmppratec, NCA=nca        &
               ,DZ8W=dz8w                                       &
               ,W0AVG=w0avg                                     &
               ,CP=cp ,R=r_d ,G=g ,EP1=ep_1 ,EP2=ep_2           &
               ,SVP1=svp1 ,SVP2=svp2 ,SVP3=svp3 ,SVPT0=svpt0    &
               ,STEPCU=stepcu                                   &
               ,CU_ACT_FLAG=cu_act_flag                         &
               ,WARM_RAIN=warm_rain                             &
               ,QV=qv_curr                                      &
               ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde &
               ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme &
               ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte &
              
               ,RTHCUTEN=rthcuten ,RQVCUTEN=rqvcuten            &
               ,RQCCUTEN=rqccuten ,RQRCUTEN=rqrcuten            &
               ,RQICUTEN=rqicuten ,RQSCUTEN=rqscuten            &
               ,F_QV=f_qv,F_QC=f_qc,F_QR=f_qr                   &
               ,F_QI=f_qi,F_QS=f_qs                             &
                                                                )

     CASE (BMJSCHEME)
          CALL wrf_debug(100,'in bmj_cps')
          CALL BMJDRV(                                          &
                TH=th,T=T ,RAINCV=raincv, PRATEC=tmppratec      &
               ,RHO=rho                                         &
               ,DT=dt ,ITIMESTEP=itimestep ,STEPCU=stepcu       &
               ,CUDT=cudt_pass                                  &
               ,CURR_SECS=curr_secs_pass                        &
               ,ADAPT_STEP_FLAG=adapt_step_flag_pass            &
               ,CUTOP=htop, CUBOT=hbot, KPBL=kpbl               &
               ,DZ8W=dz8w ,PINT=p8w, PMID=p, PI=pi              &
               ,CP=cp ,R=r_d ,ELWV=xlv ,ELIV=xls ,G=g           &
               ,TFRZ=svpt0 ,D608=ep_1 ,CLDEFI=cldefi            &
               ,LOWLYR=lowlyr ,XLAND=xland                      &
               ,CU_ACT_FLAG=cu_act_flag                         &
               ,QV=qv_curr                                      &
               ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde &
               ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme &
               ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte &
              
               ,RTHCUTEN=rthcuten ,RQVCUTEN=rqvcuten            &
                                                                )

     CASE (KFETASCHEME)
          CALL wrf_debug(100,'in kf_eta_cps')
          CALL KF_ETA_CPS(                                      &
                U=u ,V=v ,TH=th ,T=t ,W=w ,RHO=rho              &
               ,CUDT=cudt_pass                                  &
               ,CURR_SECS=curr_secs_pass                        &
               ,ADAPT_STEP_FLAG=adapt_step_flag_pass            &
               ,RAINCV=raincv, PRATEC=tmppratec, NCA=nca        &
               ,DZ8W=dz8w                                       &
               ,PCPS=p, PI=pi ,W0AVG=W0AVG                      &
               ,CUTOP=HTOP,CUBOT=HBOT                           &
               ,XLV0=XLV0 ,XLV1=XLV1 ,XLS0=XLS0 ,XLS1=XLS1      &
               ,CP=CP ,R=R_d ,G=G ,EP1=EP_1 ,EP2=EP_2           &
               ,SVP1=SVP1 ,SVP2=SVP2 ,SVP3=SVP3 ,SVPT0=SVPT0    &
               ,DT=dt ,KTAU=itimestep ,DX=dx                    &
               ,STEPCU=stepcu                                   &
               ,CU_ACT_FLAG=cu_act_flag ,WARM_RAIN=warm_rain    &
               ,QV=qv_curr                                      &
               ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde &
               ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme &
               ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte &
              
               ,RTHCUTEN=rthcuten                               &
               ,RQVCUTEN=rqvcuten ,RQCCUTEN=rqccuten            &
               ,RQRCUTEN=rqrcuten ,RQICUTEN=rqicuten            &
               ,RQSCUTEN=rqscuten                               &
               ,F_QV=f_qv,F_QC=f_qc,F_QR=f_qr                   &
               ,F_QI=f_qi,F_QS=f_qs                             &
                                                                )

     CASE (GDSCHEME)
          CALL wrf_debug(100,'in grelldrv')
          CALL GRELLDRV(                                        &
                DT=dt, ITIMESTEP=itimestep, DX=dx               &
               ,U=u,V=v,T=t,W=w ,RHO=rho                        &
               ,P=p,PI=pi ,Q=qv_curr ,RAINCV=raincv             &
               ,DZ8W=dz8w,P8W=p8w,XLV=xlv,CP=cp,G=g,R_V=r_v     &
               ,PRATEC=tmppratec                                &
               ,APR_GR=apr_gr,APR_W=apr_w,APR_MC=apr_mc         &
               ,APR_ST=apr_st,APR_AS=apr_as                     &
               ,APR_CAPMA=apr_capma,APR_CAPME=apr_capme         &
               ,APR_CAPMI=apr_capmi,MASS_FLUX=mass_flux         &
               ,XF_ENS=xf_ens,PR_ENS=pr_ens,HT=ht               &
               ,xland=xland,gsw=gsw                             &
               ,GDC=gd_cloud,GDC2=gd_cloud2 &
               ,ENSDIM=ensdim,MAXIENS=maxiens,MAXENS=maxens     &
               ,MAXENS2=maxens2,MAXENS3=maxens3                 &
               ,STEPCU=STEPCU,htop=htop,hbot=hbot               &
               ,CU_ACT_FLAG=CU_ACT_FLAG,warm_rain=warm_rain     &
               ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde &
               ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme &
               ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte &
               ,PERIODIC_X=periodic_x,PERIODIC_Y=periodic_y     &
              
               ,RTHCUTEN=RTHCUTEN ,RTHFTEN=forcet               &
               ,RQICUTEN=RQICUTEN ,RQVFTEN=forceq               &
               ,RTHRATEN=RTHRATEN,RTHBLTEN=RTHBLTEN             &
               ,RQVCUTEN=RQVCUTEN,RQCCUTEN=RQCCUTEN             &
               ,RQVBLTEN=RQVBLTEN                               &
               ,F_QV=f_qv,F_QC=f_qc,F_QR=f_qr                   &
               ,F_QI=f_qi,F_QS=f_qs                             &
                                                                )
     CASE (SASSCHEME)
                                                                                                                                           
          IF ( adapt_step_flag_pass ) THEN
            WRITE( wrf_err_message , * ) 'The SAS cumulus option will not work properly with an adaptive time step'
            CALL wrf_error_fatal3("",503,&
wrf_err_message )
          END IF
          CALL wrf_debug(100,'in cu_sas')
          CALL CU_SAS(                                          &
                DT=dt,ITIMESTEP=itimestep,STEPCU=STEPCU         &
               ,RAINCV=RAINCV,PRATEC=tmpPRATEC,HTOP=HTOP,HBOT=HBOT &
               ,U3D=u,V3D=v,W=w,T3D=t,PI3D=pi,RHO3D=rho         &
               ,QV3D=QV_CURR,QC3D=QC_CURR,QI3D=QI_CURR          &
               ,DZ8W=dz8w,PCPS=p,P8W=p8w,XLAND=XLAND            &
               ,CU_ACT_FLAG=CU_ACT_FLAG                         &
               ,CUDT=cudt_pass                                  &
               ,CURR_SECS=curr_secs_pass                        &
               ,ADAPT_STEP_FLAG=adapt_step_flag_pass            &
               ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde &
               ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme &
               ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte &
              
               ,RTHCUTEN=RTHCUTEN,RQVCUTEN=RQVCUTEN             &
               ,RQCCUTEN=RQCCUTEN,RQICUTEN=RQICUTEN             &
               ,F_QV=f_qv,F_QC=f_qc,F_QR=f_qr                   &
               ,F_QI=f_qi,F_QS=f_qs                             &
                                                                )
     CASE (G3SCHEME)
          CALL wrf_debug(100,'in grelldrv')
          CALL G3DRV(                                           &
                DT=dt, ITIMESTEP=itimestep, DX=dx               &
               ,U=u,V=v,T=t,W=w ,RHO=rho                        &
               ,P=p,PI=pi,Q=qv_curr,RAINCV=raincv               &
               ,DZ8W=dz8w ,P8W=p8w,XLV=xlv,CP=cp,G=g,R_V=r_v    &
               ,APR_GR=apr_gr,APR_W=apr_w,APR_MC=apr_mc         &
               ,APR_ST=apr_st,APR_AS=apr_as,PRATEC=tmppratec    &
               ,APR_CAPMA=apr_capma,APR_CAPME=apr_capme         &
               ,APR_CAPMI=apr_capmi,MASS_FLUX=mass_flux         &
               ,XF_ENS=xf_ens,PR_ENS=pr_ens,HT=ht               &
               ,xland=xland,gsw=gsw,edt_out=edt_out             &
               ,GDC=gd_cloud,GDC2=gd_cloud2                     &
               ,cugd_tten=cugd_tten,cugd_qvten=cugd_qvten       &
               ,cugd_ttens=cugd_ttens,cugd_qvtens=cugd_qvtens   &
               ,cugd_qcten=cugd_qcten,cugd_avedx=cugd_avedx     &
               ,imomentum=imomentum                             &
               ,ENSDIM=ensdim,MAXIENS=maxiens,MAXENS=maxens     &
               ,MAXENS2=maxens2,MAXENS3=maxens3,ichoice=clos_choice &
               ,STEPCU=STEPCU,htop=htop,hbot=hbot               &
               ,CU_ACT_FLAG=CU_ACT_FLAG,warm_rain=warm_rain     &
               ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde &
               ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme &
               ,IPS=ips,IPE=ipe,JPS=jps,JPE=jpe,KPS=kps,KPE=kpe &
               ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte &
               ,PERIODIC_X=periodic_x,PERIODIC_Y=periodic_y     &
              
               ,RTHCUTEN=RTHCUTEN ,RTHFTEN=forcet               &
               ,RQICUTEN=RQICUTEN ,RQVFTEN=forceq               &
               ,RQVCUTEN=RQVCUTEN,RQCCUTEN=RQCCUTEN             &
               ,F_QV=f_qv,F_QC=f_qc,F_QR=f_qr                   &
               ,F_QI=f_qi,F_QS=f_qs                             &
                                                                )

     CASE DEFAULT 

         WRITE( wrf_err_message , * ) 'The cumulus option does not exist: cu_physics = ', cu_physics
         CALL wrf_error_fatal3("",564,&
wrf_err_message )

   END SELECT cps_select

      ENDDO
      !$OMP END PARALLEL DO

      
      
      
      if (PRESENT(PRATEC)) then
         pratec(:,:) = tmppratec(:,:)
      endif
   END SUBROUTINE cumulus_driver

END MODULE module_cumulus_driver
