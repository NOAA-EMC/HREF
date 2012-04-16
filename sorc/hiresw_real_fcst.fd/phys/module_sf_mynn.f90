MODULE module_sf_mynn

  USE module_model_constants, only: &
       &p1000mb, cp, xlv, ep_2

  USE module_sf_sfclay, ONLY: sfclayinit
  USE module_bl_mynn,   only: tv0, mym_condensation
  
  IMPLICIT NONE

  REAL, PARAMETER :: xlvcp=xlv/cp, ep_3=1.-ep_2
 
  REAL, PARAMETER :: wmin=0.1    
  REAL, PARAMETER :: zm2h=7.4    

  REAL, PARAMETER :: charnock=0.016, bvisc=0.165e-5, z0hsea=5.e-5

  REAL, PARAMETER :: VCONVC=1.
  REAL, PARAMETER :: czo=charnock
  
  REAL, DIMENSION(0:1000 ),SAVE          :: PSIMTB,PSIHTB

  INTEGER :: mynn_level

CONTAINS

  SUBROUTINE mynn_sf_init_driver(allowed_to_read,level)

    LOGICAL, INTENT(in) :: allowed_to_read
    INTEGER, INTENT(in) :: level


       
    CALL sfclayinit(allowed_to_read)

    mynn_level=level

  END SUBROUTINE mynn_sf_init_driver





   SUBROUTINE SFCLAY_mynn(U3D,V3D,T3D,QV3D,P3D,dz8w,                    &
                     CP,G,ROVCP,R,XLV,PSFC,CHS,CHS2,CQS2,CPM,      &
                     ZNT,UST,PBLH,MAVAIL,ZOL,MOL,REGIME,PSIM,PSIH, &
                     XLAND,HFX,QFX,LH,TSK,FLHC,FLQC,QGH,QSFC,RMOL, &
                     U10,V10,TH2,T2,Q2,                            &
                     GZ1OZ0,WSPD,BR,ISFFLX,DX,                     &
                     SVP1,SVP2,SVP3,SVPT0,EP1,EP2,                 &
                     KARMAN,EOMEG,STBOLT,                          &
                     &itimestep,ch,th3d,pi3d,qc3d,&
                     &tsq,qsq,cov,qcg,&
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte                     )

      IMPLICIT NONE






































































      INTEGER,  INTENT(IN )   ::        ids,ide, jds,jde, kds,kde, &
                                        ims,ime, jms,jme, kms,kme, &
                                        its,ite, jts,jte, kts,kte

      INTEGER,  INTENT(IN )   ::        ISFFLX
      REAL,     INTENT(IN )   ::        SVP1,SVP2,SVP3,SVPT0
      REAL,     INTENT(IN )   ::        EP1,EP2,KARMAN,EOMEG,STBOLT

      REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
                INTENT(IN   )   ::                           dz8w
                                        
      REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
                INTENT(IN   )   ::                           QV3D, &
                                                              P3D, &
                                                              T3D, &
                                                              &QC3D,&
                                              &th3d,pi3d,tsq,qsq,cov
      INTEGER, INTENT(in) :: itimestep

      REAL,     DIMENSION( ims:ime, jms:jme ), INTENT(IN) ::&
           &    qcg
      
      REAL,     DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) ::&
           & ch



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




      REAL,     DIMENSION( its:ite ) ::                       U1D, &
                                                              V1D, &
                                                             QV1D, &
                                                              P1D, &
                                                              T1D,qc1d

      REAL,     DIMENSION( its:ite ) ::                    dz8w1d

      REAL,     DIMENSION( its:ite ) ::  vt1,vq1
      REAL,     DIMENSION(kts:kts+1) ::  thl, qw, vt, vq
      REAL                           ::  ql

      INTEGER ::  I,J,K, levflag

      levflag = mynn_level

      DO J=jts,jte
        DO i=its,ite
          dz8w1d(I) = dz8w(i,kts,j)
        ENDDO
   
        DO i=its,ite
           U1D(i) =U3D(i,kts,j)
           V1D(i) =V3D(i,kts,j)
           QV1D(i)=QV3D(i,kts,j)
           QC1D(i)=QC3D(i,kts,j)
           P1D(i) =P3D(i,kts,j)
           T1D(i) =T3D(i,kts,j)
        ENDDO

        IF (itimestep==1) THEN
           DO i=its,ite
              vt1(i)=0.
              vq1(i)=0.
           ENDDO
        ELSE
           DO i=its,ite
              do k = kts,kts+1
                ql = qc3d(i,k,j)/(1.+qc3d(i,k,j))
                qw (k) = qv3d(i,k,j)/(1.+qv3d(i,k,j)) + ql
                thl(k) = th3d(i,k,j)-xlvcp*ql/pi3d(i,k,j)
              end do


              CALL mym_condensation (kts,kts+1, &
                   &            levflag, &
                   &            dz8w(i,kts:kts+1,j), &
                   &            thl(kts:kts+1), qw(kts:kts+1), &
                   &            p3d(i,kts:kts+1,j),&
                   &            pi3d(i,kts:kts+1,j), &
                   &            tsq(i,kts:kts+1,j), &
                   &            qsq(i,kts:kts+1,j), &
                   &            cov(i,kts:kts+1,j), &
                   &            vt(kts:kts+1), vq(kts:kts+1))

              vt1(i) = vt(kts)
              vq1(i) = vq(kts)
           ENDDO
        ENDIF


        CALL SFCLAY1D_mynn(J,U1D,V1D,T1D,QV1D,P1D,dz8w1d,               &
                CP,G,ROVCP,R,XLV,PSFC(ims,j),CHS(ims,j),CHS2(ims,j),&
                CQS2(ims,j),CPM(ims,j),PBLH(ims,j), RMOL(ims,j),   &
                ZNT(ims,j),UST(ims,j),MAVAIL(ims,j),ZOL(ims,j),    &
                MOL(ims,j),REGIME(ims,j),PSIM(ims,j),PSIH(ims,j),  &
                XLAND(ims,j),HFX(ims,j),QFX(ims,j),TSK(ims,j),     &
                U10(ims,j),V10(ims,j),TH2(ims,j),T2(ims,j),        &
                Q2(ims,j),FLHC(ims,j),FLQC(ims,j),QGH(ims,j),      &
                QSFC(ims,j),LH(ims,j),                             &
                GZ1OZ0(ims,j),WSPD(ims,j),BR(ims,j),ISFFLX,DX,     &
                SVP1,SVP2,SVP3,SVPT0,EP1,EP2,KARMAN,EOMEG,STBOLT,  &
                ch(ims,j),vt1,vq1,qc1d,qcg(ims,j),&
                ids,ide, jds,jde, kds,kde,                         &
                ims,ime, jms,jme, kms,kme,                         &
                its,ite, jts,jte, kts,kte                          )
      ENDDO


    END SUBROUTINE SFCLAY_MYNN



   SUBROUTINE SFCLAY1D_mynn(J,UX,VX,T1D,QV1D,P1D,dz8w1d,                &
                     CP,G,ROVCP,R,XLV,PSFCPA,CHS,CHS2,CQS2,CPM,PBLH,RMOL, &
                     ZNT,UST,MAVAIL,ZOL,MOL,REGIME,PSIM,PSIH,      &
                     XLAND,HFX,QFX,TSK,                            &
                     U10,V10,TH2,T2,Q2,FLHC,FLQC,QGH,              &
                     QSFC,LH,GZ1OZ0,WSPD,BR,ISFFLX,DX,             &
                     SVP1,SVP2,SVP3,SVPT0,EP1,EP2,                 &
                     KARMAN,EOMEG,STBOLT,                          &
                     ch,vt1,vq1,qc1d,qcg,&
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte                     )

      IMPLICIT NONE

      REAL,     PARAMETER     ::        XKA=2.4E-5
      REAL,     PARAMETER     ::        PRT=1.


      INTEGER,  INTENT(IN )   ::        ids,ide, jds,jde, kds,kde, &
                                        ims,ime, jms,jme, kms,kme, &
                                        its,ite, jts,jte, kts,kte, &
                                        J

      INTEGER,  INTENT(IN )   ::        ISFFLX
      REAL,     INTENT(IN )   ::        SVP1,SVP2,SVP3,SVPT0
      REAL,     INTENT(IN )   ::        EP1,EP2,KARMAN,EOMEG,STBOLT


      REAL,     DIMENSION( ims:ime )                             , &
                INTENT(IN   )               ::             MAVAIL, &
                                                             PBLH, &
                                                            XLAND, &
                                                              TSK

      REAL,     DIMENSION( ims:ime )                             , &
                INTENT(IN   )               ::             PSFCPA

      REAL,     DIMENSION( ims:ime )                             , &
                INTENT(INOUT)               ::             REGIME, &
                                                              HFX, &
                                                              QFX, &
                                                         MOL,RMOL


      REAL,     DIMENSION( ims:ime )                             , &
                INTENT(INOUT)   ::                 GZ1OZ0,WSPD,BR, &
                                                        PSIM,PSIH

      REAL,     DIMENSION( ims:ime )                             , &
                INTENT(INOUT)   ::                            ZNT, &
                                                              ZOL, &
                                                              UST, &
                                                              CPM, &
                                                             CHS2, &
                                                             CQS2, &
                                                              CHS

      REAL,     DIMENSION( ims:ime )                             , &
                INTENT(INOUT)   ::                      FLHC,FLQC

      REAL,     DIMENSION( ims:ime )                             , &
                INTENT(INOUT)   ::                                 &
                                                              QGH

      REAL,     DIMENSION( ims:ime )                             , &
                INTENT(OUT)     ::                        U10,V10, &
                                                TH2,T2,Q2,QSFC,LH


                                    
      REAL,     INTENT(IN   )               ::   CP,G,ROVCP,R,XLV,DX


      REAL,     DIMENSION( its:ite ),  INTENT(IN   )   ::  dz8w1d

      REAL,     DIMENSION( its:ite ),  INTENT(IN   )   ::      UX, &
                                                               VX, &
                                                             QV1D, &
                                                              P1D, &
                                                              T1D,qc1d
 
 
      REAL, DIMENSION( ims:ime ), INTENT(IN) ::&

           &qcg
      
      REAL,     DIMENSION( ims:ime ), INTENT(INOUT) ::&
           & ch


       REAL, DIMENSION( its:ite ), INTENT(IN) :: vt1,vq1




      REAL,     DIMENSION( its:ite ) :: z_t,z_q

      REAL :: thl1,sqv1,sqc1,exner1,sqvg,sqcg,vv,ww

      REAL,     DIMENSION( its:ite )        ::                 ZA, &
                                                        THVX,ZQKL, &
                                                           ZQKLP1, &
                                                           THX,QX, &
                                                            PSIH2, &
                                                            PSIM2, &
                                                            PSIH10, &
                                                            PSIM10, &
                                                           GZ2OZ0, &
                                                           GZ10OZ0

      REAL,     DIMENSION( its:ite )        ::                     &
                                                      RHOX,GOVRTH, &
                                                            TGDSA

      REAL,     DIMENSION( its:ite)         ::          SCR3,SCR4
      REAL,     DIMENSION( its:ite )        ::         THGB, PSFC

      REAL,     DIMENSION( its:ite )        :: GZ2OZt,GZ10OZt,GZ1OZt


      INTEGER                               ::                 KL

      INTEGER ::  N,I,K,KK,L,NZOL,NK,NZOL2,NZOL10

      REAL    ::  PL,THCON,TVCON,E1
      REAL    ::  ZL,TSKV,DTHVDZ,DTHVM,VCONV,RZOL,RZOL2,RZOL10,ZOL2,ZOL10
      REAL    ::  DTG,PSIX,USTM,DTTHX,PSIX10,PSIT,PSIT2,PSIQ,PSIQ2
      REAL    ::  FLUXC,VSGD
      real :: restar




      KL=kte

      DO i=its,ite

         PSFC(I)=PSFCPA(I)/1000.
      ENDDO



      DO 5 I=its,ite                                   
        TGDSA(I)=TSK(I)                                    

        THGB(I)=TSK(I)*(100./PSFC(I))**ROVCP                
    5 CONTINUE                                               









   10 CONTINUE                                                     





                                                             
   26 CONTINUE                                               
                                                   


                                                                                 
      DO 30 I=its,ite

         PL=P1D(I)/1000.
         SCR3(I)=T1D(I)                                                   
         THCON=(100./PL)**ROVCP                                                 
         THX(I)=SCR3(I)*THCON                                               
         SCR4(I)=SCR3(I)                                                    
         THVX(I)=THX(I)                                                     
         QX(I)=0.                                                             
   30 CONTINUE                                                                 

      DO I=its,ite
         QGH(I)=0.                                                                
         FLHC(I)=0.                                                               
         FLQC(I)=0.                                                               
         CPM(I)=CP                                                                
      ENDDO


      DO 50 I=its,ite
         QX(I)=QV1D(I)/(1.+QV1D(I))                                                    
         TVCON=(1.+EP1*QX(I))                                      
         THVX(I)=THX(I)*TVCON                                               
         SCR4(I)=SCR3(I)*TVCON                                              
   50 CONTINUE                                                                 

      DO 60 I=its,ite
        E1=SVP1*EXP(SVP2*(TGDSA(I)-SVPT0)/(TGDSA(I)-SVP3))                       
        QSFC(I)=EP2*E1/(PSFC(I)-ep_3*E1)                                                 


        E1=SVP1*EXP(SVP2*(T1D(I)-SVPT0)/(T1D(I)-SVP3))
        PL=P1D(I)/1000.
        QGH(I)=EP2*E1/(PL-ep_3*E1)
        CPM(I)=CP*(1.+0.8*QX(I)/(1.-qx(i)))

60      CONTINUE                                                                   
   80 CONTINUE
                                                                                 


                                                                                 
      DO 90 I=its,ite
        ZQKLP1(I)=0.
        RHOX(I)=PSFC(I)*1000./(R*SCR4(I))                                       
   90 CONTINUE                                                                   

      DO 110 I=its,ite                                                   
           ZQKL(I)=dz8w1d(I)+ZQKLP1(I)
  110 CONTINUE                                                                 

      DO 120 I=its,ite
         ZA(I)=0.5*(ZQKL(I)+ZQKLP1(I))                                        
  120 CONTINUE                                                                 

      DO 160 I=its,ite
        GOVRTH(I)=G/THX(I)                                                    
  160 CONTINUE                                                                   
                                                                                 


                   
        DO 260 I=its,ite

        IF((XLAND(I)-1.5).GE.0)THEN 




           z_t(i)=z0hsea
           z_q(i)=z_t(i)
        ELSE
           z_t(i)=ZNT(I)/zm2h
           z_q(i)=z_t(i)
        ENDIF



        GZ1OZ0(I)=ALOG(ZA(I)/ZNT(I))
        GZ1OZt(I)=ALOG(ZA(I)/z_t(i))
           
        GZ2OZ0(I)=ALOG(2./ZNT(I))                                        
        GZ2OZt(I)=ALOG(2./z_t(i))                                        
        GZ10OZ0(I)=ALOG(10./ZNT(I)) 
        GZ10OZt(I)=ALOG(10./z_t(i)) 

        WSPD(I)=SQRT(UX(I)*UX(I)+VX(I)*VX(I))                        



        exner1=(p1d(i)/p1000mb)**ROVCP
        sqc1=qc1d(i)/(1.+qc1d(i))
        sqv1=qx(i)
        thl1=THX(I)-xlvcp/exner1*sqc1
        sqvg=qsfc(i)
        sqcg=qcg(i)/(1.+qcg(i))

        vv = thl1-THGB(I)
        ww = mavail(i)*(sqv1-sqvg) + (sqc1-sqcg)

        TSKV=THGB(I)*(1.+EP1*QSFC(I)*MAVAIL(I))                     



        DTHVDZ= (vt1(i) + 1.0)*vv+&
               &(vq1(i) + tv0)*ww







        if (xland(i).lt.1.5) then
        fluxc = max(hfx(i)/rhox(i)/cp                    &
              + ep1*tskv*qfx(i)/rhox(i),0.)
        VCONV = vconvc*(g/tgdsa(i)*pblh(i)*fluxc)**.33
        else
        IF(-DTHVDZ.GE.0)THEN
          DTHVM=-DTHVDZ
        ELSE
          DTHVM=0.
        ENDIF
        VCONV = 2.*SQRT(DTHVM)
        endif

        VSGD = 0.32 * (max(dx/5000.-1.,0.))**.33
        WSPD(I)=SQRT(WSPD(I)*WSPD(I)+VCONV*VCONV+vsgd*vsgd)
        WSPD(I)=AMAX1(WSPD(I),wmin)
        BR(I)=GOVRTH(I)*ZA(I)*DTHVDZ/(WSPD(I)*WSPD(I))                        

        IF(MOL(I).LT.0.)BR(I)=AMIN1(BR(I),0.0)

        RMOL(I)=-GOVRTH(I)*DTHVDZ*ZA(I)*KARMAN


  260 CONTINUE                                                                   

























      DO 320 I=its,ite



        IF(BR(I).LT.0.)GOTO 310                                                  



        IF(BR(I).LT.0.2)GOTO 270                                                 
        REGIME(I)=1.                                                           
        PSIM(I)=-10.*GZ1OZ0(I)                                                   

        PSIM(I)=AMAX1(PSIM(I),-10.)                                              
        PSIH(I)=PSIM(I)                                                          
        PSIM10(I)=10./ZA(I)*PSIM(I)
        PSIM10(I)=AMAX1(PSIM10(I),-10.)                               
        PSIH10(I)=PSIM10(I)                                          
        PSIM2(I)=2./ZA(I)*PSIM(I)
        PSIM2(I)=AMAX1(PSIM2(I),-10.)                              
        PSIH2(I)=PSIM2(I)                                         


        IF(UST(I).LT.0.01)THEN
           RMOL(I)=BR(I)*GZ1OZ0(I) 
        ELSE
           RMOL(I)=KARMAN*GOVRTH(I)*ZA(I)*MOL(I)/(UST(I)*UST(I)) 
        ENDIF
        RMOL(I)=AMIN1(RMOL(I),9.999) 
        RMOL(I) = RMOL(I)/ZA(I) 

        GOTO 320                                    
                                                                



  270   IF(BR(I).EQ.0.0)GOTO 280                                                 
        REGIME(I)=2.                                                           
        PSIM(I)=-5.0*BR(I)*GZ1OZ0(I)/(1.1-5.0*BR(I))                             

        PSIM(I)=AMAX1(PSIM(I),-10.)                                              

        PSIH(I)=PSIM(I)                                                          
        PSIM10(I)=10./ZA(I)*PSIM(I)
        PSIM10(I)=AMAX1(PSIM10(I),-10.)                               
        PSIH10(I)=PSIM10(I)                                          
        PSIM2(I)=2./ZA(I)*PSIM(I)
        PSIM2(I)=AMAX1(PSIM2(I),-10.)                              
        PSIH2(I)=PSIM2(I)                                         


        
        
        
        
        ZOL(I) = BR(I)*GZ1OZ0(I)/(1.00001-5.0*BR(I))

        if ( ZOL(I) .GT. 0.5 ) then 
           
           
           
           ZOL(I) = ( 1.89*GZ1OZ0(I) + 44.2 ) * BR(I)*BR(I)    &
                + ( 1.18*GZ1OZ0(I) - 1.37 ) * BR(I)
           ZOL(I)=AMIN1(ZOL(I),9.999)
        end if

        
        RMOL(I)= ZOL(I)/ZA(I)


        GOTO 320                                                                 



  280   REGIME(I)=3.                                                           
        PSIM(I)=0.0                                                              
        PSIH(I)=PSIM(I)                                                          
        PSIM10(I)=0.                                                   
        PSIH10(I)=PSIM10(I)                                           
        PSIM2(I)=0.                                                  
        PSIH2(I)=PSIM2(I)                                           

                                                                                 
        IF(UST(I).LT.0.01)THEN                                                 
          ZOL(I)=BR(I)*GZ1OZ0(I)                                               
        ELSE                                                                     
          ZOL(I)=KARMAN*GOVRTH(I)*ZA(I)*MOL(I)/(UST(I)*UST(I)) 
        ENDIF                                                                    
        RMOL(I) = ZOL(I)/ZA(I)  

        GOTO 320                                                                 



  310   CONTINUE                                                                 
        REGIME(I)=4.                                                           
        IF(UST(I).LT.0.01)THEN                                                 
          ZOL(I)=BR(I)*GZ1OZ0(I)                                               
        ELSE                                                                     
          ZOL(I)=KARMAN*GOVRTH(I)*ZA(I)*MOL(I)/(UST(I)*UST(I))
        ENDIF                                                                    
        ZOL10=10./ZA(I)*ZOL(I)                                    
        ZOL2=2./ZA(I)*ZOL(I)                                     
        ZOL(I)=AMIN1(ZOL(I),0.)                                              
        ZOL(I)=AMAX1(ZOL(I),-9.9999)                                         
        ZOL10=AMIN1(ZOL10,0.)                                          
        ZOL10=AMAX1(ZOL10,-9.9999)                                    
        ZOL2=AMIN1(ZOL2,0.)                                          
        ZOL2=AMAX1(ZOL2,-9.9999)                                    
        NZOL=INT(-ZOL(I)*100.)                                                 
        RZOL=-ZOL(I)*100.-NZOL                                                 
        NZOL10=INT(-ZOL10*100.)                                        
        RZOL10=-ZOL10*100.-NZOL10                                     
        NZOL2=INT(-ZOL2*100.)                                        
        RZOL2=-ZOL2*100.-NZOL2                                      
        PSIM(I)=PSIMTB(NZOL)+RZOL*(PSIMTB(NZOL+1)-PSIMTB(NZOL))                  
        PSIH(I)=PSIHTB(NZOL)+RZOL*(PSIHTB(NZOL+1)-PSIHTB(NZOL))                  
        PSIM10(I)=PSIMTB(NZOL10)+RZOL10*(PSIMTB(NZOL10+1)-PSIMTB(NZOL10))                                                    
        PSIH10(I)=PSIHTB(NZOL10)+RZOL10*(PSIHTB(NZOL10+1)-PSIHTB(NZOL10))
        PSIM2(I)=PSIMTB(NZOL2)+RZOL2*(PSIMTB(NZOL2+1)-PSIMTB(NZOL2))    
        PSIH2(I)=PSIHTB(NZOL2)+RZOL2*(PSIHTB(NZOL2+1)-PSIHTB(NZOL2))   





        PSIH(I)=AMIN1(PSIH(I),0.9*GZ1OZ0(I))
        PSIM(I)=AMIN1(PSIM(I),0.9*GZ1OZ0(I))
        PSIH2(I)=AMIN1(PSIH2(I),0.9*GZ2OZ0(I))
        PSIM10(I)=AMIN1(PSIM10(I),0.9*GZ10OZ0(I))


        RMOL(I) = ZOL(I)/ZA(I)  

  320 CONTINUE                                                                   




      DO 330 I=its,ite

        DTG=THX(I)-THGB(I)                                                   
        PSIX=GZ1OZ0(I)-PSIM(I)                                                   
        PSIX10=GZ10OZ0(I)-PSIM10(I)



        PSIT=AMAX1(GZ1OZt(I)-PSIH(I),2.)



        PSIQ=ALOG(za(i)/z_q(i))-PSIH(I)   
        PSIT2=GZ2OZt(I)-PSIH2(I)                                     
        PSIQ2=ALOG(2./z_q(i))-PSIH2(I)                                   

        UST(I)=0.5*UST(I)+0.5*KARMAN*WSPD(I)/PSIX                                             
        U10(I)=UX(I)*PSIX10/PSIX                                    
        V10(I)=VX(I)*PSIX10/PSIX                                   
        TH2(I)=THGB(I)+DTG*PSIT2/PSIT                                
        Q2(I)=QSFC(I)+(QX(I)-QSFC(I))*PSIQ2/PSIQ                   
        T2(I) = TH2(I)*(PSFC(I)/100.)**ROVCP                     







        IF((XLAND(I)-1.5).LT.0.)THEN
          UST(I)=AMAX1(UST(I),0.1)
        ENDIF


 1002   format(f15.12,2x,f15.12,2x,f15.12,2x,f15.12,2x,f15.12)
        MOL(I)=KARMAN*DTG/PSIT/PRT                              
  330 CONTINUE                                                                   

  335 CONTINUE                                                                   
                                                                                  

                                                                                 
      DO i=its,ite
        QFX(i)=0.                                                              
        HFX(i)=0.                                                              
      ENDDO

      IF (ISFFLX.EQ.0) GOTO 410                                                
                                                                                 

                                                                                 
      DO 360 I=its,ite

        IF((XLAND(I)-1.5).GE.0)THEN                                            
           ZNT(I)=CZO*UST(I)*UST(I)/G+bvisc/ust(i)                                   




           z_t(i)=z0hsea
           z_q(i)=z_t(i)

        ENDIF    





        FLQC(I)=RHOX(I)*MAVAIL(I)*UST(I)*KARMAN/(   & 
             &             ALOG(za(i)/z_q(i))-PSIH(I))          

        DTTHX=ABS(THX(I)-THGB(I))                                            
        IF(DTTHX.GT.1.E-5)THEN                                                   
          FLHC(I)=CPM(I)*RHOX(I)*UST(I)*MOL(I)/(THX(I)-THGB(I))          

 1001   format(f8.5,2x,f12.7,2x,f12.10,2x,f12.10,2x,f13.10,2x,f12.8,f12.8,2x,i3)
        ELSE                                                                     
          FLHC(I)=0.                                                             
        ENDIF                                                                    
  360 CONTINUE                                                                   






      DO 370 I=its,ite
        QFX(I)=FLQC(I)*(QSFC(I)-QX(I))                                     
        QFX(I)=AMAX1(QFX(I),0.)                                            
        LH(I)=XLV*QFX(I)
  370 CONTINUE                                                                 
                                                                                


  390 CONTINUE                                                                 
      DO 400 I=its,ite
        IF(XLAND(I)-1.5.GT.0.)THEN                                           
          HFX(I)=FLHC(I)*(THGB(I)-THX(I))                                
        ELSEIF(XLAND(I)-1.5.LT.0.)THEN                                       
          HFX(I)=FLHC(I)*(THGB(I)-THX(I))                                
          HFX(I)=AMAX1(HFX(I),-250.)                                       
        ENDIF                                                                  
  400 CONTINUE                                                                 
         
      DO I=its,ite





         CHS(I)=UST(I)*KARMAN/(ALOG(ZA(I)/z_t(i))- &
              &PSIH(I))





         ch(i)=flhc(i)/( cpm(i)*rhox(i) )











         CQS2(I)=UST(I)*KARMAN/(ALOG(2.0/z_q(i))-PSIH2(I))
         CHS2(I)=UST(I)*KARMAN/(GZ2OZt(I)-PSIH2(I))


      ENDDO
                                                                        
  410 CONTINUE                                                                   











    END SUBROUTINE SFCLAY1D_mynn


END MODULE module_sf_mynn
