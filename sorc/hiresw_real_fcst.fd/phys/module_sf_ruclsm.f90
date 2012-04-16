

MODULE module_sf_ruclsm

  USE module_model_constants
  USE module_wrf_error


        INTEGER :: LUCATS , BARE, NATURAL
        integer, PARAMETER :: NLUS=50
        CHARACTER*8 LUTYPE
        INTEGER, DIMENSION(1:NLUS) :: NROTBL
        real, dimension(1:NLUS) ::  SNUPTBL, RSTBL, RGLTBL, HSTBL, LAITBL,         &
                                    ALBTBL, Z0TBL, LEMITBL, PCTBL, SHDTBL, MAXALB
        REAL ::   TOPT_DATA,CMCMAX_DATA,CFACTR_DATA,RSMAX_DATA


        INTEGER :: SLCATS
        INTEGER, PARAMETER :: NSLTYPE=30
        CHARACTER*8 SLTYPE
        REAL, DIMENSION (1:NSLTYPE) :: BB,DRYSMC,HC,                           &
        MAXSMC, REFSMC,SATPSI,SATDK,SATDW, WLTSMC,QTZ


        INTEGER :: SLPCATS
        INTEGER, PARAMETER :: NSLOPE=30
        REAL, DIMENSION (1:NSLOPE) :: SLOPE_DATA
        REAL ::  SBETA_DATA,FXEXP_DATA,CSOIL_DATA,SALP_DATA,REFDK_DATA,           &
                 REFKDT_DATA,FRZK_DATA,ZBOT_DATA,  SMLOW_DATA,SMHIGH_DATA,        &
                        CZIL_DATA

        CHARACTER*256  :: err_message


CONTAINS


    SUBROUTINE LSMRUC(                                           &
                   DT,KTAU,NSL,ZS,                               &
                   RAINBL,SNOW,SNOWH,SNOWC,FRZFRAC,frpcpn,       &
                   Z3D,P8W,T3D,QV3D,QC3D,RHO3D,                  & 
                   GLW,GSW,EMISS,CHKLOWQ, CHS,                   & 
                   FLQC,FLHC,MAVAIL,CANWAT,VEGFRA,ALB,ZNT,       &
                   SNOALB,ALBBCK,                                &  
                   QSFC,QSG,QVG,QCG,SOILT1,TSNAV,                &
                   TBOT,IVGTYP,ISLTYP,XLAND,XICE,                &
                   CP,G0,LV,STBOLT,                              &
                   SOILMOIS,SH2O,SMAVAIL,SMMAX,                  &
                   TSO,SOILT,HFX,QFX,LH,                         &
                   SFCRUNOFF,UDRUNOFF,SFCEXC,                    &
                   SFCEVP,GRDFLX,ACSNOW,                         &
                   SMFR3D,KEEPFR3DFLAG,                          &
                   myj,                                          &
                   ids,ide, jds,jde, kds,kde,                    &
                   ims,ime, jms,jme, kms,kme,                    &
                   its,ite, jts,jte, kts,kte                     )

   IMPLICIT NONE






































































   INTEGER,     PARAMETER            ::     nvegclas=24+3

   REAL,       INTENT(IN   )    ::     DT
   LOGICAL,    INTENT(IN   )    ::     myj,frpcpn
   INTEGER,    INTENT(IN   )    ::     ktau, nsl,                 &
                                       ims,ime, jms,jme, kms,kme, &
                                       ids,ide, jds,jde, kds,kde, &
                                       its,ite, jts,jte, kts,kte

   REAL,    DIMENSION( ims:ime, kms:kme, jms:jme )            , &
            INTENT(IN   )    ::                           QV3D, &
                                                          QC3D, &
                                                           p8w, &
                                                         rho3D, &
                                                           T3D, &
                                                           z3D

   REAL,       DIMENSION( ims:ime , jms:jme ),                   &
               INTENT(IN   )    ::                       RAINBL, &
                                                            GLW, &
                                                            GSW, &
                                                         SNOALB, &
                                                         ALBBCK, &
                                                           FLHC, &
                                                           FLQC, &
                                                           CHS , &
                                                          EMISS, &
                                                           XICE, &
                                                          XLAND, &
                                                         VEGFRA, &
                                                           TBOT

   REAL,       DIMENSION( 1:nsl), INTENT(IN   )      ::      ZS

   REAL,       DIMENSION( ims:ime , jms:jme ),                   &
               INTENT(INOUT)    ::                               &
                                                           SNOW, & 
                                                          SNOWH, &
                                                          SNOWC, &
                                                         CANWAT, & 
                                                            ALB, &
                                                         MAVAIL, & 
                                                         SFCEXC, &
                                                            ZNT

   REAL,       DIMENSION( ims:ime , jms:jme ),                   &
               INTENT(IN   )    ::                               &
                                                        FRZFRAC

   INTEGER,    DIMENSION( ims:ime , jms:jme ),                   &
               INTENT(IN   )    ::                       IVGTYP, &
                                                         ISLTYP

   REAL, INTENT(IN   )          ::              CP,G0,LV,STBOLT
 
   REAL,       DIMENSION( ims:ime , 1:nsl, jms:jme )           , &
               INTENT(INOUT)    ::                 SOILMOIS,SH2O,TSO

   REAL,       DIMENSION( ims:ime, jms:jme )                   , &
               INTENT(INOUT)    ::                        SOILT, &
                                                            HFX, &
                                                            QFX, &
                                                             LH, &
                                                         SFCEVP, &
                                                      SFCRUNOFF, &
                                                       UDRUNOFF, &
                                                         GRDFLX, &
                                                         ACSNOW, &
                                                            QVG, &
                                                            QCG, &
                                                           QSFC, &
                                                            QSG, &
                                                        CHKLOWQ, &
                                                         SOILT1, &
                                                          TSNAV

   REAL,       DIMENSION( ims:ime, jms:jme )                   , & 
               INTENT(INOUT)    ::                      SMAVAIL, &
                                                          SMMAX

   REAL,       DIMENSION( its:ite, jts:jte )    ::          DEW, &
                                                             PC, &
                                                        RUNOFF1, &
                                                        RUNOFF2, &
                                                         EMISSL, &
                                                           ZNTL, &
                                                        LMAVAIL, &
                                                          SMELT, &
                                                           SNOH, &
                                                          SNFLX, &
                                                           SNOM, &
                                                           EDIR, &
                                                             EC, &
                                                            ETT, &
                                                         SUBLIM, &
                                                           sflx, &
                                                          EVAPL, &
                                                          PRCPL, &
                                                          XICED, &
                                                        INFILTR


   REAL,       DIMENSION( ims:ime, 1:nsl, jms:jme)               &
                                             ::    KEEPFR3DFLAG, &
                                                         SMFR3D

   REAL                                                          &
                             ::                           RHOCS, &
                                                          RHOSN, &
                                                           BCLH, &
                                                            DQM, &
                                                           KSAT, &
                                                           PSIS, &
                                                           QMIN, &
                                                          QWRTZ, &
                                                            REF, &
                                                           WILT, &
                                                        CANWATR, &
                                                       SNOWFRAC, &
                                                          SNHEI, &
                                                           SNWE

   REAL                                      ::              CN, &
                                                         SAT,CW, &
                                                           C1SN, &
                                                           C2SN, &
                                                         KQWRTZ, &
                                                           KICE, &
                                                            KWT


   REAL,     DIMENSION(1:NSL)                ::          ZSMAIN, &
                                                         ZSHALF, &
                                                         DTDZS2

   REAL,     DIMENSION(1:2*(nsl-2))          ::           DTDZS

   REAL,     DIMENSION(1:4001)               ::             TBQ


   REAL,     DIMENSION( 1:nsl )              ::         SOILM1D, & 
                                                          TSO1D, &
                                                        SOILICE, &
                                                        SOILIQW, &
                                                       SMFRKEEP

   REAL,     DIMENSION( 1:nsl )              ::          KEEPFR
                                                

   REAL                           ::                        RSM, &
                                                      SNWEPRINT, &
                                                     SNHEIPRINT

   REAL                           ::                     PRCPMS, &
                                                        NEWSNMS, &
                                                           PATM, &
                                                           TABS, &
                                                          QVATM, &
                                                          QCATM, &
                                                          Q2SAT, &
                                                         SATFLG, &
                                                         CONFLX, &
                                                            RHO, &
                                                           QKMS, &
                                                           TKMS, &
                                                       INFILTRP
   REAL      ::  cq,r61,r273,arp,brp,x,evs,eis

   INTEGER   ::  NROOT
   INTEGER   ::  ILAND,ISOIL
 
   INTEGER, DIMENSION ( 1:nvegclas )          ::        IFOREST

   INTEGER   ::  I,J,K,NZS,NZS1,NDDZS
   INTEGER   ::  k1,l,k2,kp,km




         NZS=NSL
         NDDZS=2*(nzs-2)


        CQ=173.15-.05
        R273=1./273.15
        R61=6.1153*0.62198
        ARP=77455.*41.9/461.525
        BRP=64.*41.9/461.525

        DO K=1,4001
          CQ=CQ+.05

        EVS=EXP(17.67*(CQ-273.15)/(CQ-29.65))
        EIS=EXP(22.514-6.15E3/CQ)
        if(CQ.ge.273.15) then

        tbq(k) = R61*evs
        else
        tbq(k) = R61*eis
        endif

        END DO




     if(ktau+1.eq.1) then
     DO J=jts,jte
         DO i=its,ite
            do k=1,nsl

       keepfr3dflag(i,k,j)=0.
       sh2o        (i,k,j)=0.
            enddo

           snowc(i,j) = min(1.,snowh(i,j)/0.1)

           soilt1(i,j)=soilt(i,j)
           tsnav(i,j) =0.5*(soilt(i,j)+tso(i,1,j))-273.
           qcg  (i,j) =0.
           patm=P8w(i,kms,j)*1.e-2
           QSG  (i,j) = QSN(SOILT(i,j),TBQ)/PATM
           qvg  (i,j) = QSG(i,j)*mavail(i,j)

           qsfc(i,j) = qsg(i,j)/(1.+qsg(i,j))
           SMELT(i,j) = 0.
           SNOM (i,j) = 0.
           SNFLX(i,j) = 0.
           DEW  (i,j) = 0.
           PC   (i,j) = 0.
           zntl (i,j) = 0.
           RUNOFF1(i,j) = 0.
           RUNOFF2(i,j) = 0.
           emissl (i,j) = 0.

            canwat(i,j)=0.




           chklowq(i,j) = 1.
           infiltr(i,j) = 0.
           snoh  (i,j) = 0.
           edir  (i,j) = 0.
           ec    (i,j) = 0.
           ett   (i,j) = 0.
           sublim(i,j) = 0.
           sflx  (i,j) = 0.
           evapl (i,j) = 0.
           prcpl (i,j) = 0.
         ENDDO
     ENDDO

        do k=1,nsl
           soilice(k)=0.
           soiliqw(k)=0.
        enddo
     endif



        PRCPMS = 0.



   DO J=jts,jte

      DO i=its,ite

    IF ( wrf_at_debug_level(3000) ) THEN
      print *,' IN LSMRUC ','ims,ime,jms,jme,its,ite,jts,jte,nzs', &
                ims,ime,jms,jme,its,ite,jts,jte,nzs
      print *,' IVGTYP, ISLTYP ', ivgtyp(i,j),isltyp(i,j)
      print *,' MAVAIL ', mavail(i,j)
      print *,' SOILT,QVG,P8w',soilt(i,j),qvg(i,j),p8w(i,1,j)
      print *, 'LSMRUC, I,J,xland, QFX,HFX from SFCLAY',i,j,xland(i,j), &
                  qfx(i,j),hfx(i,j)
      print *, ' GSW, GLW =',gsw(i,j),glw(i,j)
      print *, 'SOILT, TSO start of time step =',soilt(i,j),(tso(i,k,j),k=1,nsl)
      print *, 'SOILMOIS start of time step =',(soilmois(i,k,j),k=1,nsl)
      print *, 'SMFROZEN start of time step =',(smfr3d(i,k,j),k=1,nsl)
      print *, ' I,J=, after SFCLAY CHS,FLHC ',i,j,chs(i,j),flhc(i,j)
      print *, 'LSMRUC, IVGTYP,ISLTYP,ZNT,ALB = ', ivgtyp(i,j),isltyp(i,j),znt(i,j),alb(i,j),i,j
      print *, 'LSMRUC  I,J,DT,RAINBL =',I,J,dt,RAINBL(i,j)
      print *, 'XLAND ---->, ivgtype,isoiltyp,i,j',xland(i,j),ivgtyp(i,j),isltyp(i,j),i,j
    ENDIF


         ILAND     = IVGTYP(i,j)
         ISOIL     = ISLTYP(I,J)
         TABS      = T3D(i,kms,j)
         QVATM     = QV3D(i,kms,j)
         QCATM     = QC3D(i,kms,j)
         PATM      = P8w(i,kms,j)*1.e-5


         CONFLX    = Z3D(i,kms,j)


         RHO       = RHO3D(I,kms,J)

       IF(FRPCPN) THEN
         PRCPMS    = (RAINBL(i,j)/DT*1.e-3)*(1-FRZFRAC(I,J))
         NEWSNMS  = (RAINBL(i,j)/DT*1.e-3)*FRZFRAC(I,J)
       ELSE
          if (tabs.le.273.15) then
         PRCPMS    = 0.
         NEWSNMS   = RAINBL(i,j)/DT*1.e-3
          else
         PRCPMS    = RAINBL(i,j)/DT*1.e-3
         NEWSNMS   = 0.
          endif
       ENDIF




        if   (myj)   then
         QKMS=CHS(i,j)
         TKMS=CHS(i,j)
        else
         QKMS=FLQC(I,J)/RHO/MAVAIL(I,J)
         TKMS=FLHC(I,J)/RHO/CP
        endif

         SNWE=SNOW(I,J)*1.E-3
         SNHEI=SNOWH(I,J)
         CANWATR=CANWAT(I,J)*1.E-3
         SNOWFRAC=SNOWC(I,J)


             zsmain(1)=0.
             zshalf(1)=0.
          do k=2,nzs
             zsmain(k)= zs(k)
             zshalf(k)=0.5*(zsmain(k-1) + zsmain(k))
          enddo





        NZS1=NZS-1

    IF ( wrf_at_debug_level(3000) ) THEN
         print *,' DT,NZS1, ZSMAIN, ZSHALF --->', dt,nzs1,zsmain,zshalf
    ENDIF

        DO  K=2,NZS1
          K1=2*K-3
          K2=K1+1
          X=DT/2./(ZSHALF(K+1)-ZSHALF(K))
          DTDZS(K1)=X/(ZSMAIN(K)-ZSMAIN(K-1))
          DTDZS2(K-1)=X
          DTDZS(K2)=X/(ZSMAIN(K+1)-ZSMAIN(K))
        END DO


        CN=0.5     
        SAT=0.0004   

        CW =4.183E6





        KQWRTZ=7.7
        KICE=2.2
        KWT=0.57




        c1sn=0.026

        c2sn=21.



        NROOT= 4


       if(SNOWH(i,j).gt.0.) then
        RHOSN = SNOW(i,j)/SNOWH(i,j)
       else
        RHOSN = 300.
       endif


     CALL SOILVEGIN  ( ILAND,ISOIL,MYJ,IFOREST,                     &
                     EMISSL(I,J),PC(i,j),ZNT(I,J),QWRTZ,       &

                     RHOCS,BCLH,DQM,KSAT,PSIS,QMIN,REF,WILT     )


     IF(iforest(ivgtyp(i,j)).ne.1) THEN

         do k=2,nzs
         if(zsmain(k).ge.0.4) then
            NROOT=K
            goto  111
         endif
         enddo

     ELSE

         do k=2,nzs
         if(zsmain(k).ge.1.1) then
            NROOT=K
            goto  111
         endif
         enddo
     ENDIF
 111   continue


    IF ( wrf_at_debug_level(3000) ) THEN
         print *,' ZS, ZSMAIN, ZSHALF, CONFLX --->', zs,zsmain,zshalf,conflx
         print *,'NROOT, iforest, ivgtyp, i,j ', nroot,iforest(ivgtyp(i,j)),ivgtyp(I,J),I,J
    ENDIF




        IF((XLAND(I,J)-1.5).GE.0. .or. XICE(I,J).gt.0.5)THEN

           SMAVAIL(I,J)=1.0
             SMMAX(I,J)=1.0

           LMAVAIL(I,J)=1.0

           ILAND=16
           ISOIL=14

           patm=P8w(i,kms,j)*1.e-2
           qvg  (i,j) = QSN(SOILT(i,j),TBQ)/PATM
           qsfc(i,j) = qvg(i,j)/(1.+qvg(i,j))
           CHKLOWQ(I,J)=1.
           Q2SAT=QSN(TABS,TBQ)/PATM

            DO K=1,NZS
              SOILMOIS(I,K,J)=1.0
              TSO(I,K,J)= SOILT(I,J)
            ENDDO

    IF ( wrf_at_debug_level(3000) ) THEN
              PRINT*,'  water point, I=',I,                      &
              'J=',J, 'SOILT=', SOILT(i,j)
    ENDIF


       if(xice(i,j).gt.0.5) then


           XICED(i,j)=1.
       else
           XICED(i,j)=0.
       endif

         IF(XICED(I,J).NE.1.)  SNOW(I,J)=0.
         IF(XICED(I,J).GT.0.5)THEN

    IF ( wrf_at_debug_level(3000) ) THEN
              PRINT*,' sea-ice at water point, I=',I,            &
              'J=',J
    ENDIF
            ILAND = 24
            ISOIL = 16

            SMAVAIL(I,J)=1.0
            SMMAX(I,J)=1.0
            LMAVAIL(I,J)=1.0


            SOILT(I,J)=MIN(271.,TABS)
            ZNT(I,J)=0.011


            DO K=1,NZS
              SOILMOIS(I,K,J)=1.0
              TSO(I,K,J)= MIN(273.15,SOILT(I,J))
            ENDDO
          ENDIF


      if (myj) then
        IF((QVATM.GE.Q2SAT*0.95).AND.QVATM.LT.qvg(I,J))THEN

          SATFLG=0.
        ELSE
          SATFLG=1.0
        ENDIF
      else
          SATFLG=1.0
      endif
          QFX(I,J)=QFX(I,J)*SATFLG


           ELSE














           DO k=1,nzs

              soilm1d (k) = min(max(0.,soilmois(i,k,j)),dqm)
              tso1d   (k) = tso(i,k,j)
           ENDDO 

           do k=1,nzs
              smfrkeep(k) = smfr3d(i,k,j)
              keepfr  (k) = keepfr3dflag(i,k,j)
           enddo


              LMAVAIL(I,J)=max(0.00001,min(1.,soilmois(i,1,j)/dqm))

    IF ( wrf_at_debug_level(3000) ) THEN
   print *,'LAND, i,j,tso1d,soilm1d,PATM,TABS,QVATM,QCATM,RHO',  &
                  i,j,tso1d,soilm1d,PATM,TABS,QVATM,QCATM,RHO
   print *,'CONFLX =',CONFLX 
   print *,'SMFRKEEP,KEEPFR   ',SMFRKEEP,KEEPFR
    ENDIF


         CALL SFCTMP (dt,ktau,conflx,i,j,                        &

                nzs,nddzs,nroot,                                 &
                iland,isoil,xland(i,j),ivgtyp(i,j),              &  
                PRCPMS,NEWSNMS,SNWE,SNHEI,SNOWFRAC,RHOSN,        &
                PATM,TABS,QVATM,QCATM,RHO,                       &
                GLW(I,J),GSW(I,J),EMISSL(I,J),                   &
                QKMS,TKMS,PC(I,J),LMAVAIL(I,J),                  &
                canwatr,vegfra(I,J),alb(I,J),znt(I,J),           &
                snoalb(i,j),albbck(i,j),                         &   
                myj,                                             &

                QWRTZ,                                           &
                rhocs,dqm,qmin,ref,                              &
                wilt,psis,bclh,ksat,                             &
                sat,cn,zsmain,zshalf,DTDZS,DTDZS2,tbq,           &

                cp,g0,lv,stbolt,cw,c1sn,c2sn,                    &
                KQWRTZ,KICE,KWT,                                 &

                snweprint,snheiprint,rsm,                        &
                soilm1d,tso1d,smfrkeep,keepfr,                   &
                soilt(I,J),soilt1(i,j),tsnav(i,j),dew(I,J),      &
                qvg(I,J),qsg(I,J),qcg(I,J),SMELT(I,J),           &
                SNOH(I,J),SNFLX(I,J),SNOM(I,J),ACSNOW(I,J),      &
                edir(I,J),ec(I,J),ett(I,J),qfx(I,J),          &
                lh(I,J),hfx(I,J),sflx(I,J),sublim(I,J),          &
                evapl(I,J),prcpl(I,J),runoff1(I,J),              &
                runoff2(I,J),soilice,soiliqw,infiltrp)





        smavail(i,j) = 0.
        smmax (i,j)  = 0.  

      do k=1,nzs-1
        smavail(i,j)=smavail(i,j)+(qmin+soilm1d(k))*             &
                    (zshalf(k+1)-zshalf(k))
        smmax (i,j) =smmax (i,j)+(qmin+dqm)*                     &
                    (zshalf(k+1)-zshalf(k))
      enddo

        smavail(i,j)=smavail(i,j)+(qmin+soilm1d(nzs))*           &
                    (zsmain(nzs)-zshalf(nzs))
        smmax (i,j) =smmax (i,j)+(qmin+dqm)*                     &
                    (zsmain(nzs)-zshalf(nzs))


        SFCRUNOFF(I,J) = SFCRUNOFF(I,J)+RUNOFF1(I,J)*DT*1000.0
        UDRUNOFF (I,J) = UDRUNOFF(I,J)+RUNOFF2(I,J)*1000.0
        SMAVAIL  (I,J) = SMAVAIL(I,J) * 1000.
        SMMAX    (I,J) = SMMAX(I,J) * 1000.
        SFCEXC   (I,J) = TKMS



        QSFC(I,J) = QVG(I,J)/(1.+QVG(I,J))
        patm=P8w(i,kms,j)*1.e-2
        Q2SAT=QSN(TABS,TBQ)/PATM

      if (myj) then
        IF((QVATM.GE.Q2SAT*0.95).AND.QVATM.LT.qvg(I,J))THEN
          CHKLOWQ(I,J)=0.
        ELSE
          CHKLOWQ(I,J)=1.
        ENDIF
      else
          CHKLOWQ(I,J)=1.
      endif

    IF ( wrf_at_debug_level(3000) ) THEN
      if(CHKLOWQ(I,J).eq.0.) then
   print *,'i,j,CHKLOWQ',  &
                  i,j,CHKLOWQ(I,J)
      endif
    ENDIF

        MAVAIL (i,j) = LMAVAIL(I,J)  

        SNOW   (i,j) = SNWE*1000.
        SNOWH  (I,J) = SNHEI 
        CANWAT (I,J) = CANWATR*1000.

    IF ( wrf_at_debug_level(3000) ) THEN
       print *,' LAND, I=,J=, QFX, HFX after SFCTMP', i,j,lh(i,j),hfx(i,j)
    ENDIF

        SFCEVP (I,J) = SFCEVP (I,J) + QFX (I,J) * DT
        GRDFLX (I,J) = -1. * sflx(I,J)

    IF ( wrf_at_debug_level(3000) ) THEN
       print *,' QFX after change, LH ', i,j, QFX(i,j),LH(I,J)
    ENDIF

       SNOWC(I,J)=SNOWFRAC







        INFILTR(I,J) = INFILTRP


    IF ( wrf_at_debug_level(3000) ) THEN
   print *,'LAND, i,j,tso1d,soilm1d - end of time step',         &
                  i,j,tso1d,soilm1d
    ENDIF

        do k=1,nzs

             soilmois(i,k,j) = soilm1d(k)
             sh2o    (i,k,j) = soiliqw(k)
                  tso(i,k,j) = tso1d(k)
        enddo

        do k=1,nzs
             smfr3d(i,k,j) = smfrkeep(k) 
           keepfr3dflag(i,k,j) = keepfr (k)
        enddo

        ENDIF

      ENDDO

   ENDDO


   END SUBROUTINE LSMRUC




   SUBROUTINE SFCTMP (delt,ktau,conflx,i,j,                      &

                nzs,nddzs,nroot,                                 &
                ILAND,ISOIL,XLAND,IVGTYP,                        &
                PRCPMS,NEWSNMS,SNWE,SNHEI,SNOWFRAC,RHOSN,        &
                PATM,TABS,QVATM,QCATM,rho,                       &
                GLW,GSW,EMISS,QKMS,TKMS,PC,                      &
                MAVAIL,CST,VEGFRA,ALB,ZNT,                       &
                ALB_SNOW,ALB_SNOW_FREE,                          &
                MYJ,                                             &

                QWRTZ,rhocs,dqm,qmin,ref,wilt,psis,bclh,ksat,    &
                sat,cn,zsmain,zshalf,DTDZS,DTDZS2,tbq,           &

                cp,g0,lv,stbolt,cw,c1sn,c2sn,                    &
                KQWRTZ,KICE,KWT,                                 &

                snweprint,snheiprint,rsm,                        &
                soilm1d,ts1d,smfrkeep,keepfr,soilt,soilt1,       &
                tsnav,dew,qvg,qsg,qcg,                           &
                SMELT,SNOH,SNFLX,SNOM,ACSNOW,                    &
                edir1,ec1,ett1,eeta,qfx,hfx,s,sublim,            &
                evapl,prcpl,runoff1,runoff2,soilice,             &
                soiliqw,infiltr)

       IMPLICIT NONE




   INTEGER,  INTENT(IN   )   ::  i,j,nroot,ktau,nzs            , &
                                 nddzs                             

   REAL,     INTENT(IN   )   ::  DELT,CONFLX
   REAL,     INTENT(IN   )   ::  C1SN,C2SN
   LOGICAL,    INTENT(IN   )    ::     myj

   REAL                                                        , &
            INTENT(IN   )    ::                            PATM, &
                                                           TABS, &
                                                          QVATM, &
                                                          QCATM
   REAL                                                        , &
            INTENT(IN   )    ::                             GLW, &
                                                            GSW, &
                                                             PC, &
                                                       ALB_SNOW, &
                                                  ALB_SNOW_FREE, &
                                                         VEGFRA, &
                                                          XLAND, &
                                                            RHO, &
                                                           QKMS, &
                                                           TKMS
                                                             
   INTEGER,   INTENT(IN   )  ::                          IVGTYP

   REAL                                                        , &
            INTENT(INOUT)    ::                           EMISS, &
                                                         MAVAIL, &
                                                       SNOWFRAC, &
                                                            ALB, &
                                                            CST


   REAL                      ::                                  &
                                                          RHOCS, &
                                                           BCLH, &
                                                            DQM, &
                                                           KSAT, &
                                                           PSIS, &
                                                           QMIN, &
                                                          QWRTZ, &
                                                            REF, &
                                                            SAT, &
                                                           WILT

   REAL,     INTENT(IN   )   ::                              CN, &
                                                             CW, &
                                                             CP, &
                                                             G0, &
                                                             LV, &
                                                         STBOLT, &
                                                         KQWRTZ, &
                                                           KICE, &
                                                            KWT

   REAL,     DIMENSION(1:NZS), INTENT(IN)  ::            ZSMAIN, &
                                                         ZSHALF, &
                                                         DTDZS2 

   REAL,     DIMENSION(1:NDDZS), INTENT(IN)  ::           DTDZS

   REAL,     DIMENSION(1:4001), INTENT(IN)  ::              TBQ




   REAL,     DIMENSION( 1:nzs )                                , &
             INTENT(INOUT)   ::                            TS1D, & 
                                                        SOILM1D, &
                                                       SMFRKEEP
   REAL,  DIMENSION( 1:nzs )                                   , &
             INTENT(INOUT)   ::                          KEEPFR
          

   INTEGER, INTENT(INOUT)    ::                     ILAND,ISOIL


   REAL                                                        , &
             INTENT(INOUT)   ::                             DEW, &
                                                          EDIR1, &
                                                            EC1, &
                                                           ETT1, &
                                                           EETA, &
                                                          EVAPL, &
                                                        INFILTR, &
                                                          RHOSN, & 
                                                         SUBLIM, &
                                                          PRCPL, &
                                                            QVG, &
                                                            QSG, &
                                                            QCG, &
                                                            QFX, &
                                                            HFX, &
                                                              S, &  
                                                        RUNOFF1, &
                                                        RUNOFF2, &
                                                         ACSNOW, &
                                                           SNWE, &
                                                          SNHEI, &
                                                          SMELT, &
                                                           SNOM, &
                                                           SNOH, &
                                                          SNFLX, &
                                                          SOILT, &
                                                         SOILT1, &
                                                          TSNAV, &
                                                            ZNT


   REAL,     DIMENSION(1:NZS), INTENT(OUT)  ::          SOILICE, &
                                                        SOILIQW

   REAL,     INTENT(OUT)                    ::              RSM, &  
                                                      SNWEPRINT, &
                                                     SNHEIPRINT


   INTEGER ::  K,ILNB

   REAL    ::  BSN, XSN, RHONEWSN                              , &
               RAINF, SNTH, NEWSN, PRCPMS, NEWSNMS             , &
               T3, UPFLUX, XINET
   REAL    ::  snhei_crit, keep_snow_albedo

   REAL    ::  RNET,GSWNEW,EMISSN,ALBSN,ZNTSN
   REAL    ::  VEGFRAC


        integer,   parameter      ::      ilsnow=99 
        
    IF ( wrf_at_debug_level(3000) ) THEN
        print *,' in SFCTMP',i,j,nzs,nddzs,nroot,                 &
                 SNWE,RHOSN,SNOM,SMELT,TS1D
    ENDIF













        NEWSN=0.
        RAINF = 0.
        RSM=0.
        INFILTR=0.
        VEGFRAC=0.01*VEGFRA

    IF ( wrf_at_debug_level(3000) ) THEN
        print *,'I,J,KTAU,QKMS,TKMS', i,j,ktau,qkms,tkms
        print *,'GSW, GLW, SOILT, STBOLT, EMISS',           &
                 GSW, GLW, SOILT, STBOLT, EMISS
    ENDIF


         SNHEI   = SNWE * 1000. / RHOSN

         T3      = STBOLT*SOILT*SOILT*SOILT
         UPFLUX  = T3 *SOILT
         XINET   = EMISS*(GLW-UPFLUX)
         RNET    = GSW + XINET



	      if(snhei.gt.0.0081*1.e3/rhosn) then

        BSN=delt/3600.*c1sn*exp(0.08*tsnav-c2sn*rhosn*1.e-3)
       if(bsn*snwe*100..lt.1.e-4) goto 777
        XSN=rhosn*(exp(bsn*snwe*100.)-1.)/(bsn*snwe*100.)
        rhosn=MIN(MAX(100.,XSN),400.)

 777   continue

      else
        rhosn     =200.
        rhonewsn  =100.
      endif



           newsn=newsnms*delt



           acsnow=acsnow+newsn

       IF(NEWSN.GE.1.E-8) THEN



    IF ( wrf_at_debug_level(3000) ) THEN
      print *, 'THERE IS NEW SNOW, newsn', newsn
    ENDIF
        if(tabs.lt.258.15) then

          rhonewsn=100.

        else
          rhonewsn=1.e3*max((0.10+0.0017*(Tabs-273.15+15.)**1.5) &
                                 , 0.10)


          rhonewsn=MIN(rhonewsn,400.)

        endif





         xsn=(rhosn*snwe+rhonewsn*newsn)/                         &
             (snwe+newsn)
         rhosn=MIN(MAX(100.,XSN),400.)


         snwe=snwe+newsn
         snhei=snwe*1.E3/rhosn
         NEWSN=NEWSN*1.E3/rhosn
        endif









       IF(PRCPMS.NE.0.) THEN







           RAINF=1.
       ENDIF







        IF(SNHEI.GT.0.0) THEN


         EMISS = 0.91








         SNHEI_CRIT=0.01601*1.e3/rhosn
         SNOWFRAC=MIN(1.,SNHEI/(2.*SNHEI_CRIT))

         KEEP_SNOW_ALBEDO = 0.
      IF (NEWSN.GT.0.) KEEP_SNOW_ALBEDO = 1.


         GSWNEW=GSW/(1.-ALB)

       ALB   = MAX(keep_snow_albedo*alb_snow,                   &
            MIN((alb_snow_free +                                &
           (alb_snow - alb_snow_free) *                         &
           (snhei/(2.*SNHEI_CRIT))), alb_snow))


         gswnew=gswnew*(1.-alb)

        XINET   = EMISS*(GLW-UPFLUX)
        RNET    = GSWnew + XINET

         CALL SNOWSOIL (                                        & 
            i,j,isoil,delt,ktau,conflx,nzs,nddzs,nroot,         &
            ILAND,PRCPMS,RAINF,NEWSN,snhei,SNWE,snowfrac,       &
            RHOSN,PATM,QVATM,QCATM,                             &
            GLW,GSWnew,EMISS,RNET,IVGTYP,                       &
            QKMS,TKMS,PC,CST,                                   &
            RHO,VEGFRAC,ALB,ZNT,                                &
            MYJ,                                                &

            QWRTZ,rhocs,dqm,qmin,ref,wilt,psis,bclh,ksat,       &
            sat,cn,zsmain,zshalf,DTDZS,DTDZS2,tbq,              & 

            lv,CP,G0,cw,stbolt,tabs,                            &
            KQWRTZ,KICE,KWT,                                    &

            ilnb,snweprint,snheiprint,rsm,                      &
            soilm1d,ts1d,smfrkeep,keepfr,                       &
            dew,soilt,soilt1,tsnav,qvg,qsg,qcg,                 &
            SMELT,SNOH,SNFLX,SNOM,edir1,ec1,ett1,eeta,          &
            qfx,hfx,s,sublim,prcpl,runoff1,runoff2,             &
            mavail,soilice,soiliqw,infiltr                      )

         if(snhei.eq.0.) then




         alb=alb_snow_free

         endif

        ELSE

           snheiprint=0.
           snweprint=0.

         CALL SOIL(                                             &

            i,j,iland,isoil,delt,ktau,conflx,nzs,nddzs,nroot,   &
            PRCPMS,RAINF,PATM,QVATM,QCATM,GLW,GSW,              &
            EMISS,RNET,QKMS,TKMS,PC,cst,rho,vegfrac,            &

            QWRTZ,rhocs,dqm,qmin,ref,wilt,                      &
            psis,bclh,ksat,sat,cn,                              &
            zsmain,zshalf,DTDZS,DTDZS2,tbq,                     &

            lv,CP,G0,cw,stbolt,tabs,                            &
            KQWRTZ,KICE,KWT,                                    &

            soilm1d,ts1d,smfrkeep,keepfr,                       &
            dew,soilt,qvg,qsg,qcg,edir1,ec1,                    &
            ett1,eeta,qfx,hfx,s,evapl,prcpl,runoff1,            &
            runoff2,mavail,soilice,soiliqw,                     &
            infiltr)

        ENDIF







   END SUBROUTINE SFCTMP



       FUNCTION QSN(TN,T)

   REAL,     DIMENSION(1:4001),  INTENT(IN   )   ::  T
   REAL,     INTENT(IN  )   ::  TN

      REAL    QSN, R,R1,R2
      INTEGER I

       R=(TN-173.15)/.05+1.
       I=INT(R)
       IF(I.GE.1) goto 10
       I=1
       R=1.
  10   IF(I.LE.4000) GOTO 20
       I=4000
       R=4001.
  20   R1=T(I)
       R2=R-I
       QSN=(T(I+1)-R1)*R2 + R1




  END FUNCTION QSN



        SUBROUTINE SOIL (                                    &

            i,j,iland,isoil,delt,ktau,conflx,nzs,nddzs,nroot,&
            PRCPMS,RAINF,PATM,QVATM,QCATM,                   &
            GLW,GSW,EMISS,RNET,                              &
            QKMS,TKMS,PC,cst,rho,vegfrac,                    &

            QWRTZ,rhocs,dqm,qmin,ref,wilt,psis,bclh,ksat,    &
            sat,cn,zsmain,zshalf,DTDZS,DTDZS2,tbq,           &

            xlv,CP,G0_P,cw,stbolt,TABS,                      &
            KQWRTZ,KICE,KWT,                                 &

            soilmois,tso,smfrkeep,keepfr,                    &
            dew,soilt,qvg,qsg,qcg,                           &
            edir1,ec1,ett1,eeta,qfx,hfx,s,evapl,             &
            prcpl,runoff1,runoff2,mavail,soilice,            &
            soiliqw,infiltrp)


























































        IMPLICIT NONE




   INTEGER,  INTENT(IN   )   ::  nroot,ktau,nzs                , &
                                 nddzs                    
   INTEGER,  INTENT(IN   )   ::  i,j,iland,isoil
   REAL,     INTENT(IN   )   ::  DELT,CONFLX

   REAL,                                                         &
            INTENT(IN   )    ::                            PATM, &
                                                          QVATM, &
                                                          QCATM

   REAL,                                                         &
            INTENT(IN   )    ::                             GLW, &
                                                            GSW, &
                                                          EMISS, &
                                                            RHO, &
                                                             PC, &
                                                        VEGFRAC, &
                                                           QKMS, &
                                                           TKMS


   REAL,                                                         &
            INTENT(IN   )    ::                           RHOCS, &
                                                           BCLH, &
                                                            DQM, &
                                                           KSAT, &
                                                           PSIS, &
                                                           QMIN, &
                                                          QWRTZ, &
                                                            REF, &
                                                           WILT

   REAL,     INTENT(IN   )   ::                              CN, &
                                                             CW, &
                                                         KQWRTZ, &
                                                           KICE, &
                                                            KWT, &
                                                            XLV, &
                                                            g0_p


   REAL,     DIMENSION(1:NZS), INTENT(IN)  ::            ZSMAIN, &
                                                         ZSHALF, &
                                                         DTDZS2

   REAL,     DIMENSION(1:NDDZS), INTENT(IN)  ::           DTDZS

   REAL,     DIMENSION(1:4001), INTENT(IN)  ::              TBQ




   REAL,     DIMENSION( 1:nzs )                                , &
             INTENT(INOUT)   ::                             TSO, &
                                                       SOILMOIS, &
                                                       SMFRKEEP

   REAL,     DIMENSION( 1:nzs )                                , &
             INTENT(INOUT)   ::                          KEEPFR


   REAL,                                                         &
             INTENT(INOUT)   ::                             DEW, &
                                                            CST, &
                                                          EDIR1, &
                                                            EC1, &
                                                           ETT1, &
                                                           EETA, &
                                                          EVAPL, &
                                                          PRCPL, &
                                                         MAVAIL, &
                                                            QVG, &
                                                            QSG, &
                                                            QCG, &
                                                           RNET, &
                                                            QFX, &
                                                            HFX, &
                                                              S, &
                                                            SAT, &
                                                        RUNOFF1, &
                                                        RUNOFF2, &
                                                          SOILT


   REAL,     DIMENSION(1:NZS), INTENT(OUT)  ::          SOILICE, &
                                                        SOILIQW



   REAL    ::  INFILTRP, transum                               , &
               RAINF,  PRCPMS                                  , &
               TABS, T3, UPFLUX, XINET
   REAL    ::  CP,G0,LV,STBOLT,xlmelt,dzstop                   , &
               can,epot,fac,fltot,ft,fq,hft                    , &
               q1,ras,rhoice,sph                               , &
               trans,zn,ci,cvw,tln,tavln,pi                    , &
               DD1,CMC2MS,DRYCAN,WETCAN                        , &
               INFMAX,RIW
   REAL,     DIMENSION(1:NZS)  ::  transp,cap,diffu,hydro      , &
                                   thdif,tranf,tav,soilmoism   , &
                                   soilicem,soiliqwm,detal     , &
                                   fwsat,lwsat,told,smold

   REAL                        ::  drip

   INTEGER ::  nzs1,nzs2,k





        RHOICE=900.
        CI=RHOICE*2100.
        XLMELT=3.335E+5
        cvw=cw

        SAT=0.0004
        prcpl=prcpms


        DO K=1,NZS
          TRANSP   (K)=0.
          soilmoism(k)=0.
          soilice  (k)=0.
          soiliqw  (k)=0.
          soilicem (k)=0.
          soiliqwm (k)=0.
          lwsat    (k)=0.
          fwsat    (k)=0.
          tav      (k)=0.
          cap      (k)=0.
          thdif    (k)=0.
          diffu    (k)=0.
          hydro    (k)=0.   
          tranf    (k)=0.
          detal    (k)=0.
          told     (k)=0.
          smold    (k)=0.
        ENDDO

          NZS1=NZS-1
          NZS2=NZS-2
        dzstop=1./(zsmain(2)-zsmain(1))
        RAS=RHO*1.E-3
        RIW=rhoice*1.e-3



         DO K=1,NZS

         tln=log(tso(k)/273.15)
         if(tln.lt.0.) then
           soiliqw(k)=(dqm+qmin)*(XLMELT*                        &
         (tso(k)-273.15)/tso(k)/9.81/psis)                       &
          **(-1./bclh)-qmin
           soiliqw(k)=max(0.,soiliqw(k))
           soiliqw(k)=min(soiliqw(k),soilmois(k))
           soilice(k)=(soilmois(k)-soiliqw(k))/RIW


       if(keepfr(k).eq.1.) then
           soilice(k)=min(soilice(k),smfrkeep(k))
           soiliqw(k)=max(0.,soilmois(k)-soilice(k)*riw)
       endif

         else
           soilice(k)=0.
           soiliqw(k)=soilmois(k)
         endif

          ENDDO

          DO K=1,NZS1

         tav(k)=0.5*(tso(k)+tso(k+1))
         soilmoism(k)=0.5*(soilmois(k)+soilmois(k+1))
         tavln=log(tav(k)/273.15)

         if(tavln.lt.0.) then
           soiliqwm(k)=(dqm+qmin)*(XLMELT*                       &
         (tav(k)-273.15)/tav(k)/9.81/psis)                       &
          **(-1./bclh)-qmin
           fwsat(k)=dqm-soiliqwm(k)
           lwsat(k)=soiliqwm(k)+qmin
           soiliqwm(k)=max(0.,soiliqwm(k))
           soiliqwm(k)=min(soiliqwm(k), soilmoism(k))
           soilicem(k)=(soilmoism(k)-soiliqwm(k))/riw

       if(keepfr(k).eq.1.) then
           soilicem(k)=min(soilicem(k),                          &
                   0.5*(smfrkeep(k)+smfrkeep(k+1)))
           soiliqwm(k)=max(0.,soilmoism(k)-soilicem(k)*riw)
           fwsat(k)=dqm-soiliqwm(k)
           lwsat(k)=soiliqwm(k)+qmin
       endif

         else
           soilicem(k)=0.
           soiliqwm(k)=soilmoism(k)
           lwsat(k)=dqm+qmin
           fwsat(k)=0.
         endif

          ENDDO

          do k=1,nzs
           if(soilice(k).gt.0.) then
             smfrkeep(k)=soilice(k)
           else
             smfrkeep(k)=soilmois(k)/riw
           endif
          enddo





          CALL SOILPROP(                                          &

               nzs,fwsat,lwsat,tav,keepfr,                        &
               soilmois,soiliqw,soilice,                          &
               soilmoism,soiliqwm,soilicem,                       &

               QWRTZ,rhocs,dqm,qmin,psis,bclh,ksat,               &

               riw,xlmelt,CP,G0_P,cvw,ci,                         &
               kqwrtz,kice,kwt,                                   &

               thdif,diffu,hydro,cap)



 
        DRIP=0.
        DD1=0.

        FQ=QKMS

        Q1=-QKMS*RAS*(QVATM - QSG)

        DEW=0.
        IF(QVATM.GE.QSG)THEN
          DEW=FQ*(QVATM-QSG)
        ENDIF
        IF(DEW.NE.0.)THEN
          DD1=CST+DELT*(PRCPMS +DEW*RAS)*vegfrac
        ELSE
          DD1=CST+                                                 &
            DELT*(PRCPMS+RAS*FQ*(QVATM-QSG)                        &
           *(CST/SAT)**CN)*vegfrac
        ENDIF

        IF(DD1.LT.0.) DD1=0.
        if(vegfrac.eq.0.)then
          cst=0.
          drip=0.
        endif
        IF (vegfrac.GT.0.) THEN
          CST=DD1
        IF(CST.GT.SAT) THEN
          CST=SAT
          DRIP=DD1-SAT
        ENDIF
        ENDIF





          WETCAN=(CST/SAT)**CN
          DRYCAN=1.-WETCAN






           CALL TRANSF(                                       &

              nzs,nroot,soiliqw,tabs,                         &

              dqm,qmin,ref,wilt,zshalf,                       &

              tranf,transum)



          do k=1,nzs
           told(k)=tso(k)
           smold(k)=soilmois(k)
          enddo





        CALL SOILTEMP(                                        &

             i,j,iland,isoil,                                 &
             delt,ktau,conflx,nzs,nddzs,nroot,                &
             PRCPMS,RAINF,                                    &
             PATM,TABS,QVATM,QCATM,EMISS,RNET,                &
             QKMS,TKMS,PC,rho,vegfrac,                        &
             thdif,cap,drycan,wetcan,                         & 
             transum,dew,mavail,                              &

             dqm,qmin,bclh,zsmain,zshalf,DTDZS,tbq,           &

             xlv,CP,G0_P,cvw,stbolt,                          &

             tso,soilt,qvg,qsg,qcg)




        ETT1=0.
        DEW=0.

        IF(QVATM.GE.QSG)THEN
          DEW=QKMS*(QVATM-QSG)
          DO K=1,NZS
            TRANSP(K)=0.
          ENDDO
        ELSE
          DO K=1,NROOT
            TRANSP(K)=VEGFRAC*RAS*QKMS*                       &
                    (QVATM-QSG)*                              &
                     PC*TRANF(K)*DRYCAN/ZSHALF(NROOT+1)
               IF(TRANSP(K).GT.0.) TRANSP(K)=0.
            ETT1=ETT1-TRANSP(K)
          ENDDO
          DO k=nroot+1,nzs
            transp(k)=0.
          enddo
        ENDIF


         DO K=1,NZS

           tln=log(tso(k)/273.15)
         if(tln.lt.0.) then
           soiliqw(k)=(dqm+qmin)*(XLMELT*                     &
          (tso(k)-273.15)/tso(k)/9.81/psis)                   & 
           **(-1./bclh)-qmin
           soiliqw(k)=max(0.,soiliqw(k))
           soiliqw(k)=min(soiliqw(k),soilmois(k))
           soilice(k)=(soilmois(k)-soiliqw(k))/riw

       if(keepfr(k).eq.1.) then
           soilice(k)=min(soilice(k),smfrkeep(k))
           soiliqw(k)=max(0.,soilmois(k)-soilice(k)*riw)
       endif

         else
           soilice(k)=0.
           soiliqw(k)=soilmois(k)
         endif
         ENDDO

       INFMAX=999.


       if((dqm+qmin-riw*soilicem(1)).lt.0.12)                 &
               INFMAX=0.





          CALL SOILMOIST (                                     &

               delt,nzs,nddzs,DTDZS,DTDZS2,                    &
               zsmain,zshalf,diffu,hydro,                      &
               QSG,QVG,QCG,QCATM,QVATM,-PRCPMS,                &
               QKMS,TRANSP,DRIP,DEW,0.,SOILICE,VEGFRAC,        &

               DQM,QMIN,REF,KSAT,RAS,INFMAX,                   &

               SOILMOIS,MAVAIL,RUNOFF1,                        &
               RUNOFF2,INFILTRP)
        








 
        do k=1,nzs
       if (soilice(k).gt.0.) then
          if(tso(k).gt.told(k).and.soilmois(k).gt.smold(k)) then
              keepfr(k)=1.
          else
              keepfr(k)=0.
          endif
       endif
        enddo


          T3      = STBOLT*SOILT*SOILT*SOILT
          UPFLUX  = T3 *SOILT
          XINET   = EMISS*(GLW-UPFLUX)
          RNET    = GSW + XINET
          HFT=-TKMS*CP*RHO*(TABS-SOILT)
          Q1=-QKMS*RAS*(QVATM - QSG)
          EDIR1 =-(1.-vegfrac)*QKMS*RAS*                       &
                       (QVATM-QVG)
        IF (Q1.LE.0.) THEN

          EC1=0.
          EDIR1=0.
          ETT1=0.

          QFX=- XLV*RHO*DEW
          EETA= QFX/XLV
        ELSE

          EC1 = Q1 * WETCAN
          CMC2MS=CST/DELT
         if(EC1.gt.CMC2MS) cst=0.
          EC1=MIN(CMC2MS,EC1)*vegfrac
          EETA = (EDIR1 + EC1 + ETT1)*1.E3

          QFX= XLV * EETA
        ENDIF
          EVAPL=QFX/XLV
          S=THDIF(1)*CAP(1)*DZSTOP*(TSO(1)-TSO(2))
          HFX=HFT
          FLTOT=RNET-HFT-QFX-S

 222    CONTINUE

 1123    FORMAT(I5,8F12.3)
 1133    FORMAT(I7,8E12.4)
  123   format(i6,f6.2,7f8.1)
  122   FORMAT(1X,2I3,6F8.1,F8.3,F8.2)





   END SUBROUTINE SOIL



        SUBROUTINE SNOWSOIL (                                  &

             i,j,isoil,delt,ktau,conflx,nzs,nddzs,nroot,       &
             ILAND,PRCPMS,RAINF,NEWSNOW,snhei,SNWE,SNOWFRAC,   &
             RHOSN,                                            &
             PATM,QVATM,QCATM,                                 &
             GLW,GSW,EMISS,RNET,IVGTYP,                        &
             QKMS,TKMS,PC,cst,rho,vegfrac,alb,znt,             & 
             MYJ,                                              &

             QWRTZ,rhocs,dqm,qmin,ref,wilt,psis,bclh,ksat,     &
             sat,cn,zsmain,zshalf,DTDZS,DTDZS2,tbq,            &

             xlv,CP,G0_P,cw,stbolt,TABS,                       &
             KQWRTZ,KICE,KWT,                                  &

             ilnb,snweprint,snheiprint,rsm,                    &
             soilmois,tso,smfrkeep,keepfr,                     &
             dew,soilt,soilt1,tsnav,                           &
             qvg,qsg,qcg,SMELT,SNOH,SNFLX,SNOM,                &
             edir1,ec1,ett1,eeta,qfx,hfx,s,sublim,             &
             prcpl,runoff1,runoff2,mavail,soilice,             &
             soiliqw,infiltrp                                  )





































































        IMPLICIT NONE



   INTEGER,  INTENT(IN   )   ::  nroot,ktau,nzs                , &
                                 nddzs                         
   INTEGER,  INTENT(IN   )   ::  i,j,isoil

   REAL,     INTENT(IN   )   ::  DELT,CONFLX,PRCPMS            , &
                                 RAINF,NEWSNOW

   LOGICAL,    INTENT(IN   )    ::     myj


   REAL,                                                         &
            INTENT(IN   )    ::                            PATM, &
                                                          QVATM, &
                                                          QCATM

   REAL                                                        , &
            INTENT(IN   )    ::                             GLW, &
                                                            GSW, &
                                                            RHO, &
                                                             PC, &
                                                        VEGFRAC, &
                                                           QKMS, &
                                                           TKMS

   INTEGER,  INTENT(IN   )   ::                          IVGTYP

   REAL                                                        , &
            INTENT(IN   )    ::                           RHOCS, &
                                                           BCLH, &
                                                            DQM, &
                                                           KSAT, &
                                                           PSIS, &
                                                           QMIN, &
                                                          QWRTZ, &
                                                            REF, &
                                                            SAT, &
                                                           WILT

   REAL,     INTENT(IN   )   ::                              CN, &
                                                             CW, &
                                                            XLV, &
                                                           G0_P, & 
                                                         KQWRTZ, &
                                                           KICE, &
                                                            KWT 


   REAL,     DIMENSION(1:NZS), INTENT(IN)  ::            ZSMAIN, &
                                                         ZSHALF, &
                                                         DTDZS2

   REAL,     DIMENSION(1:NDDZS), INTENT(IN)  ::           DTDZS

   REAL,     DIMENSION(1:4001), INTENT(IN)  ::              TBQ




   REAL,     DIMENSION(  1:nzs )                               , &
             INTENT(INOUT)   ::                             TSO, &
                                                       SOILMOIS, &
                                                       SMFRKEEP

   REAL,  DIMENSION( 1:nzs )                                   , &
             INTENT(INOUT)   ::                          KEEPFR


   INTEGER,  INTENT(INOUT)    ::                           ILAND



   REAL                                                        , &
             INTENT(INOUT)   ::                             DEW, &
                                                            CST, &
                                                          EDIR1, &
                                                            EC1, &
                                                           ETT1, &
                                                           EETA, &
                                                          RHOSN, &
                                                         SUBLIM, &
                                                          PRCPL, &
                                                            ALB, &
                                                          EMISS, &
                                                            ZNT, &
                                                         MAVAIL, &
                                                            QVG, &
                                                            QSG, &
                                                            QCG, &
                                                            QFX, &
                                                            HFX, &
                                                              S, &
                                                        RUNOFF1, &
                                                        RUNOFF2, &
                                                           SNWE, &
                                                          SNHEI, &
                                                          SMELT, &
                                                           SNOM, &
                                                           SNOH, &
                                                          SNFLX, &
                                                          SOILT, &
                                                         SOILT1, &
                                                       SNOWFRAC, &
                                                          TSNAV

   INTEGER, INTENT(INOUT)    ::                            ILNB


   REAL,     DIMENSION(1:NZS), INTENT(OUT)  ::          SOILICE, &
                                                        SOILIQW

   REAL,     INTENT(OUT)                    ::              RSM, &
                                                      SNWEPRINT, &
                                                     SNHEIPRINT



   INTEGER ::  nzs1,nzs2,k

   REAL    ::  INFILTRP, RHONEWSN,TRANSUM                      , &
               SNTH, NEWSN                                     , &
               TABS, T3, UPFLUX, XINET                         , &
               BETA, SNWEPR,EPDT,PP
   REAL    ::  CP,G0,LV,xlvm,STBOLT,xlmelt,dzstop              , &
               can,epot,fac,fltot,ft,fq,hft                    , &
               q1,ras,rhoice,sph                               , &
               trans,zn,ci,cvw,tln,tavln,pi                    , &
               DD1,CMC2MS,DRYCAN,WETCAN                        , &
               INFMAX,RIW,DELTSN,H,UMVEG

   REAL,     DIMENSION(1:NZS)  ::  transp,cap,diffu,hydro      , &
                                   thdif,tranf,tav,soilmoism   , &
                                   soilicem,soiliqwm,detal     , &
                                   fwsat,lwsat,told,smold
   REAL                                     ::             drip

   REAL                                     ::             RNET



        cvw=cw
        XLMELT=3.335E+5

        XLVm=XLV+XLMELT



         ILAND=99









           DELTSN=0.0301*1.e3/rhosn
           snth=0.01601*1.e3/rhosn

        RHOICE=900.
        CI=RHOICE*2100.
        RAS=RHO*1.E-3
        RIW=rhoice*1.e-3
        MAVAIL=1.
        RSM=0.

        DO K=1,NZS
          TRANSP     (K)=0.
          soilmoism  (k)=0.
          soiliqwm   (k)=0.
          soilice    (k)=0.
          soilicem   (k)=0.
          lwsat      (k)=0.
          fwsat      (k)=0.
          tav        (k)=0.
          cap        (k)=0.
          diffu      (k)=0.
          hydro      (k)=0.
          thdif      (k)=0.  
          tranf      (k)=0.
          detal      (k)=0.
          told       (k)=0.
          smold      (k)=0. 
        ENDDO

        snweprint=0.
        snheiprint=0.
        prcpl=prcpms






          NZS1=NZS-1
          NZS2=NZS-2
        DZSTOP=1./(zsmain(2)-zsmain(1))





         DO K=1,NZS

         tln=log(tso(k)/273.15)
         if(tln.lt.0.) then
           soiliqw(k)=(dqm+qmin)*(XLMELT*                          &
         (tso(k)-273.15)/tso(k)/9.81/psis)                         &
          **(-1./bclh)-qmin
           soiliqw(k)=max(0.,soiliqw(k))
           soiliqw(k)=min(soiliqw(k),soilmois(k))
           soilice(k)=(soilmois(k)-soiliqw(k))/riw


       if(keepfr(k).eq.1.) then
           soilice(k)=min(soilice(k),smfrkeep(k))
           soiliqw(k)=max(0.,soilmois(k)-soilice(k)*rhoice*1.e-3)
       endif

         else
           soilice(k)=0.
           soiliqw(k)=soilmois(k)
         endif

          ENDDO

          DO K=1,NZS1

         tav(k)=0.5*(tso(k)+tso(k+1))
         soilmoism(k)=0.5*(soilmois(k)+soilmois(k+1))
         tavln=log(tav(k)/273.15)

         if(tavln.lt.0.) then
           soiliqwm(k)=(dqm+qmin)*(XLMELT*                         &
         (tav(k)-273.15)/tav(k)/9.81/psis)                         &
          **(-1./bclh)-qmin
           fwsat(k)=dqm-soiliqwm(k)
           lwsat(k)=soiliqwm(k)+qmin
           soiliqwm(k)=max(0.,soiliqwm(k))
           soiliqwm(k)=min(soiliqwm(k), soilmoism(k))
           soilicem(k)=(soilmoism(k)-soiliqwm(k))/riw

       if(keepfr(k).eq.1.) then
           soilicem(k)=min(soilicem(k),                            &
                    0.5*(smfrkeep(k)+smfrkeep(k+1)))
           soiliqwm(k)=max(0.,soilmoism(k)-soilicem(k)*riw)
           fwsat(k)=dqm-soiliqwm(k)
           lwsat(k)=soiliqwm(k)+qmin
       endif

         else
           soilicem(k)=0.
           soiliqwm(k)=soilmoism(k)
           lwsat(k)=dqm+qmin
           fwsat(k)=0.

         endif
          ENDDO

          do k=1,nzs
           if(soilice(k).gt.0.) then
             smfrkeep(k)=soilice(k)
           else
             smfrkeep(k)=soilmois(k)/riw
           endif
          enddo









          CALL SOILPROP(                                         &

               nzs,fwsat,lwsat,tav,keepfr,                       &
               soilmois,soiliqw,soilice,                         &
               soilmoism,soiliqwm,soilicem,                      &

               QWRTZ,rhocs,dqm,qmin,psis,bclh,ksat,              & 

               riw,xlmelt,CP,G0_P,cvw,ci,                        &
               kqwrtz,kice,kwt,                                  &

               thdif,diffu,hydro,cap)



 
        DRIP=0.
        SMELT=0.
        DD1=0.
        H=1.

        FQ=QKMS





        DEW=0.
        UMVEG=1.-vegfrac
        EPOT = -FQ*(QVATM-QSG) 

      IF(vegfrac.EQ.0.) then
           cst=0.
           drip=0.
      ELSE
       IF(EPOT.GE.0.) THEN

         DD1=CST+(NEWSNOW*RHOSN*1.E-3                            &

              -DELT*(RAS*EPOT                            &

              *(CST/SAT)**CN)) *vegfrac
        ELSE

         DEW = - EPOT
         DD1=CST+(NEWSNOW*RHOSN*1.E-3+delt*(               &

                     +DEW*RAS)) *vegfrac
       ENDIF

        IF(DD1.LT.0.) DD1=0.
      IF (vegfrac.GT.0.) THEN
          CST=DD1
        IF(CST.GT.SAT*vegfrac) THEN
          CST=SAT*vegfrac
          DRIP=DD1-SAT*vegfrac
        ENDIF
      ENDIF












   SNWE = SNWE                                                   &
                  +0.10*prcpms*delt


       ENDIF
 
        DRIP=0.
        SNHEI=SNWE*1.e3/RHOSN
          SNWEPR=SNWE


         BETA=1.
         EPDT = EPOT * RAS *DELT*UMVEG
         IF(SNWEPR.LE.EPDT) THEN 
            BETA=SNWEPR/max(1.e-8,EPDT)
            SNWE=0.
            SNHEI=0.
         ENDIF

          WETCAN=(CST/SAT)**CN
          DRYCAN=1.-WETCAN




           CALL TRANSF(                                       &

              nzs,nroot,soiliqw,tabs,                         &

              dqm,qmin,ref,wilt,zshalf,                       & 

              tranf,transum)


          do k=1,nzs
           told(k)=tso(k)
           smold(k)=soilmois(k)
          enddo





    IF ( wrf_at_debug_level(3000) ) THEN
print *, 'TSO before calling SNOWTEMP: ', tso
    ENDIF
        CALL SNOWTEMP(                                        &

             i,j,iland,isoil,                                 &
             delt,ktau,conflx,nzs,nddzs,nroot,                &
             snwe,snwepr,snhei,newsnow,snowfrac,              &
             beta,deltsn,snth,rhosn,                          &
             PRCPMS,RAINF,                                    &
             PATM,TABS,QVATM,QCATM,                           &
             GLW,GSW,EMISS,RNET,                              &
             QKMS,TKMS,PC,rho,vegfrac,                        &
             thdif,cap,drycan,wetcan,cst,                     &
             tranf,transum,dew,mavail,                        &

             dqm,qmin,psis,bclh,                              &
             zsmain,zshalf,DTDZS,tbq,                         &

             xlvm,CP,G0_P,cvw,stbolt,                         &

             snweprint,snheiprint,rsm,                        &
             tso,soilt,soilt1,tsnav,qvg,qsg,qcg,              &
             smelt,snoh,snflx,ilnb)



         DEW=0.
         ETT1=0.
         PP=PATM*1.E3
         QSG= QSN(SOILT,TBQ)/PP
         EPOT = -FQ*(QVATM-QSG)
       IF(EPOT.GE.0.) THEN

          DO K=1,NROOT
            TRANSP(K)=vegfrac*RAS*FQ*(QVATM-QSG)              &
                     *PC*tranf(K)*DRYCAN/zshalf(NROOT+1)
           IF(TRANSP(K).GT.0.) TRANSP(K)=0.
            ETT1=ETT1-TRANSP(K)
          ENDDO
          DO k=nroot+1,nzs
            transp(k)=0.
          enddo

        ELSE

          DEW=-EPOT
          DO K=1,NZS
            TRANSP(K)=0.
          ENDDO
        ETT1=0.
        ENDIF


         DO K=1,NZS
         tln=log(tso(k)/273.15)
         if(tln.lt.0.) then
           soiliqw(k)=(dqm+qmin)*(XLMELT*                    &
         (tso(k)-273.15)/tso(k)/9.81/psis)                   &
          **(-1./bclh)-qmin
           soiliqw(k)=max(0.,soiliqw(k))
           soiliqw(k)=min(soiliqw(k),soilmois(k))
           soilice(k)=(soilmois(k)-soiliqw(k))/riw

       if(keepfr(k).eq.1.) then
           soilice(k)=min(soilice(k),smfrkeep(k))
           soiliqw(k)=max(0.,soilmois(k)-soilice(k)*riw)
       endif

         else
           soilice(k)=0.
           soiliqw(k)=soilmois(k)
         endif
         ENDDO

       INFMAX=999.


        soilicem(1)=0.5*(soilice(1)+soilice(2))
       if((dqm+qmin-riw*soilicem(1)).lt.0.12)                &
               INFMAX=0.





                CALL SOILMOIST (                                   &

               delt,nzs,nddzs,DTDZS,DTDZS2,                        &
               zsmain,zshalf,diffu,hydro,                          &

               QSG,QVG,QCG,QCATM,QVATM,-0.9*PRCPMS,                &

               0.,TRANSP,0.,                                       &
               0.,SMELT,soilice,vegfrac,                           &

               DQM,QMIN,REF,KSAT,RAS,INFMAX,                       &

               soilmois,MAVAIL,RUNOFF1,                            &
               RUNOFF2,infiltrp) 
 

        if(snwe.ne.0.) then
         CST=(1.-min(1.,smelt/snwe))*CST
        else
         CST=0.
        endif


         IF(SNHEI.EQ.0.)  then
          tsnav=soilt-273.15
          CALL SNOWFREE(ivgtyp,myj,emiss,                          & 
                        znt,iland)
          smelt=smelt+snwe/delt
          rsm=0.

         ENDIF


















        do k=1,nzs
       if (soilice(k).gt.0.) then
          if(tso(k).gt.told(k).and.soilmois(k).gt.smold(k)) then
              keepfr(k)=1.
          else
              keepfr(k)=0.
          endif
       endif
        enddo


        T3      = STBOLT*SOILT*SOILT*SOILT
        UPFLUX  = T3 *SOILT
        XINET   = EMISS*(GLW-UPFLUX)   
        RNET    = GSW + XINET
        HFT=- TKMS*CP*RHO*(TABS-SOILT)
        Q1 = - FQ*RAS* (QVATM - QSG)
        EDIR1 = Q1*UMVEG *BETA

        IF (Q1.LT.0.) THEN

         EC1=0.
         EDIR1=0.
         ETT1=0.

         DEW=FQ*(QVATM-QSG)
        QFX= -XLVm*RHO*DEW
        sublim=QFX/XLVm
        eeta=QFX/XLVm
       ELSE

        EC1 = Q1 * WETCAN 
        CMC2MS=CST/DELT 
        if(EC1.gt.CMC2MS) cst=0.
        EC1=MIN(CMC2MS,EC1)*vegfrac
        EETA = (EDIR1 + EC1 + ETT1)*1.E3

        QFX= XLVm * EETA
        sublim=(EDIR1 + EC1)*1.E3
       ENDIF
        s=THDIF(1)*CAP(1)*dzstop*(tso(1)-tso(2))
        HFX=HFT
        FLTOT=RNET-HFT-QFX-S

 222     CONTINUE

 1123    FORMAT(I5,8F12.3)
 1133    FORMAT(I7,8E12.4)
  123   format(i6,f6.2,7f8.1)
 122    FORMAT(1X,2I3,6F8.1,F8.3,F8.2)





   END SUBROUTINE SNOWSOIL



           SUBROUTINE SOILTEMP(                             &

           i,j,iland,isoil,                                 &
           delt,ktau,conflx,nzs,nddzs,nroot,                &
           PRCPMS,RAINF,PATM,TABS,QVATM,QCATM,              &
           EMISS,RNET,                                      &
           QKMS,TKMS,PC,RHO,VEGFRAC,                        &
           THDIF,CAP,DRYCAN,WETCAN,                         &
           TRANSUM,DEW,MAVAIL,                              &

           DQM,QMIN,BCLH,                                   &
           ZSMAIN,ZSHALF,DTDZS,TBQ,                         &

           XLV,CP,G0_P,CVW,STBOLT,                          &

           TSO,SOILT,QVG,QSG,QCG)

















































        IMPLICIT NONE




   INTEGER,  INTENT(IN   )   ::  nroot,ktau,nzs                , &
                                 nddzs                         
   INTEGER,  INTENT(IN   )   ::  i,j,iland,isoil
   REAL,     INTENT(IN   )   ::  DELT,CONFLX,PRCPMS, RAINF
   REAL,     INTENT(INOUT)   ::  DRYCAN,WETCAN,TRANSUM

   REAL,                                                         &
            INTENT(IN   )    ::                            PATM, &
                                                          QVATM, &
                                                          QCATM

   REAL                                                        , &
            INTENT(IN   )    ::                                  &
                                                          EMISS, &
                                                            RHO, &
                                                           RNET, &  
                                                             PC, &
                                                        VEGFRAC, &
                                                            DEW, & 
                                                           QKMS, &
                                                           TKMS


   REAL                                                        , &
            INTENT(IN   )    ::                                  &
                                                           BCLH, &
                                                            DQM, &
                                                           QMIN

   REAL,     INTENT(IN   )   ::                              CP, &
                                                            CVW, &
                                                            XLV, &
                                                         STBOLT, &
                                                           TABS, &
                                                           G0_P


   REAL,     DIMENSION(1:NZS), INTENT(IN)  ::            ZSMAIN, &
                                                         ZSHALF, &
                                                          THDIF, &
                                                            CAP

   REAL,     DIMENSION(1:NDDZS), INTENT(IN)  ::           DTDZS

   REAL,     DIMENSION(1:4001), INTENT(IN)  ::              TBQ




   REAL,     DIMENSION( 1:nzs )                                , &
             INTENT(INOUT)   ::                             TSO


   REAL                                                        , &
             INTENT(INOUT)   ::                                  &
                                                         MAVAIL, &
                                                            QVG, &
                                                            QSG, &
                                                            QCG, &
                                                          SOILT




   REAL    ::  x,x1,x2,x4,dzstop,can,ft,sph                    , &
               tn,trans,umveg,denom

   REAL    ::  FKT,D1,D2,D9,D10,DID,R211,R21,R22,R6,R7,D11     , &
               PI,H,FKQ,R210,AA,BB,PP,Q1,QS1,TS1,TQ2,TX2       , &
               TDENOM

   REAL    ::  C,CC,AA1,RHCS,H1

   REAL,     DIMENSION(1:NZS)  ::                   cotso,rhtso

   INTEGER ::  nzs1,nzs2,k,k1,kn,kk




          NZS1=NZS-1
          NZS2=NZS-2
        dzstop=1./(ZSMAIN(2)-ZSMAIN(1))

        do k=1,nzs
           cotso(k)=0.
           rhtso(k)=0.
        enddo








        cotso(1)=0.
        rhtso(1)=TSO(NZS)
        DO 33 K=1,NZS2
          KN=NZS-K
          K1=2*KN-3
          X1=DTDZS(K1)*THDIF(KN-1)
          X2=DTDZS(K1+1)*THDIF(KN)
          FT=TSO(KN)+X1*(TSO(KN-1)-TSO(KN))                             &
             -X2*(TSO(KN)-TSO(KN+1))
          DENOM=1.+X1+X2-X2*cotso(K)
          cotso(K+1)=X1/DENOM
          rhtso(K+1)=(FT+X2*rhtso(K))/DENOM
   33  CONTINUE




        RHCS=CAP(1)
        H=MAVAIL
        IF(DEW.NE.0.)THEN
          DRYCAN=0.
          WETCAN=1.
        ENDIf
        TRANS=PC*TRANSUM*DRYCAN/ZSHALF(NROOT+1)
        CAN=WETCAN+TRANS
        UMVEG=1.-VEGFRAC
        FKT=TKMS
        D1=cotso(NZS1)
        D2=rhtso(NZS1)
        TN=SOILT
        D9=THDIF(1)*RHCS*dzstop
        D10=TKMS*CP*RHO
        R211=.5*CONFLX/DELT
        R21=R211*CP*RHO
        R22=.5/(THDIF(1)*DELT*dzstop**2)
        R6=EMISS *STBOLT*.5*TN**4
        R7=R6/TN
        D11=RNET+R6
        TDENOM=D9*(1.-D1+R22)+D10+R21+R7                              &
              +RAINF*CVW*PRCPMS
        FKQ=QKMS*RHO
        R210=R211*RHO
        C=VEGFRAC*FKQ*CAN
        CC=C*XLV/TDENOM
        AA=XLV*(FKQ*UMVEG+R210)/TDENOM
        BB=(D10*TABS+R21*TN+XLV*(QVATM*                               &
        (FKQ*UMVEG+C)                                                 & 
        +R210*QVG)+D11+D9*(D2+R22*TN)                                 &
        +RAINF*CVW*PRCPMS*max(273.15,TABS)                            &
         )/TDENOM
        AA1=AA+CC
        PP=PATM*1.E3
        AA1=AA1/PP
    IF ( wrf_at_debug_level(3000) ) THEN
        PRINT *,' VILKA-1'
        print *,'D10,TABS,R21,TN,QVATM,FKQ,UMVEG,VEGFRAC,CAN',        &
                 D10,TABS,R21,TN,QVATM,FKQ,UMVEG,VEGFRAC,CAN
        print *,'RNET, EMISS, STBOLT, SOILT',RNET, EMISS, STBOLT, SOILT
        print *,'R210,QVG,D11,D9,D2,R22,RAINF,CVW,PRCPMS,TDENOM',     &
                 R210,QVG,D11,D9,D2,R22,RAINF,CVW,PRCPMS,TDENOM
        print *,'tn,aa1,bb,pp,umveg,fkq,r210,vegfrac',                &
                 tn,aa1,bb,pp,umveg,fkq,r210,vegfrac
    ENDIF
        CALL VILKA(TN,AA1,BB,PP,QS1,TS1,TBQ,KTAU,i,j,iland,isoil)
        TQ2=QVATM+QCATM
        TX2=TQ2*(1.-H)
        Q1=TX2+H*QS1
        IF(Q1.LT.QS1) GOTO 100


   90   QVG=QS1
        QSG=QS1
        TSO(1)=TS1
        QCG=Q1-QS1
        GOTO 200
  100   BB=BB-AA*TX2
        AA=(AA*H+CC)/PP
    IF ( wrf_at_debug_level(3000) ) THEN
        PRINT *,' VILKA-2'
        print *,'D10,TABS,R21,TN,QVATM,FKQ,UMVEG,VEGFRAC,CAN',        &
                 D10,TABS,R21,TN,QVATM,FKQ,UMVEG,VEGFRAC,CAN
        print *,'R210,QVG,D11,D9,D2,R22,RAINF,CVW,PRCPMS,TDENOM',     &
                 R210,QVG,D11,D9,D2,R22,RAINF,CVW,PRCPMS,TDENOM

        print *,'tn,aa1,bb,pp,umveg,fkq,r210,vegfrac',                &
                 tn,aa1,bb,pp,umveg,fkq,r210,vegfrac
    ENDIF

        CALL VILKA(TN,AA,BB,PP,QS1,TS1,TBQ,KTAU,i,j,iland,isoil)
        Q1=TX2+H*QS1
        IF(Q1.GT.QS1) GOTO 90
        QSG=QS1
        QVG=Q1
        TSO(1)=TS1
        QCG=0.
  200   CONTINUE


          SOILT=TS1


          DO K=2,NZS
            KK=NZS-K+1
            TSO(K)=rhtso(KK)+cotso(KK)*TSO(K-1)
          END DO




   END SUBROUTINE SOILTEMP



           SUBROUTINE SNOWTEMP(                                    & 

           i,j,iland,isoil,                                        &
           delt,ktau,conflx,nzs,nddzs,nroot,                       &
           snwe,snwepr,snhei,newsnow,snowfrac,                     &
           beta,deltsn,snth,rhosn,                                 &
           PRCPMS,RAINF,                                           &
           PATM,TABS,QVATM,QCATM,                                  &
           GLW,GSW,EMISS,RNET,                                     &
           QKMS,TKMS,PC,RHO,VEGFRAC,                               &
           THDIF,CAP,DRYCAN,WETCAN,CST,                            &
           TRANF,TRANSUM,DEW,MAVAIL,                               &

           DQM,QMIN,PSIS,BCLH,                                     &
           ZSMAIN,ZSHALF,DTDZS,TBQ,                                &

           XLVM,CP,G0_P,CVW,STBOLT,                                &

           SNWEPRINT,SNHEIPRINT,RSM,                               &
           TSO,SOILT,SOILT1,TSNAV,QVG,QSG,QCG,                     &
           SMELT,SNOH,SNFLX,ILNB)

















































        IMPLICIT NONE



   INTEGER,  INTENT(IN   )   ::  nroot,ktau,nzs                , &
                                 nddzs                             

   INTEGER,  INTENT(IN   )   ::  i,j,iland,isoil
   REAL,     INTENT(IN   )   ::  DELT,CONFLX,PRCPMS            , &
                                 RAINF,NEWSNOW,DELTSN,SNTH     , &
                                 TABS,TRANSUM,SNWEPR


   REAL,                                                         &
            INTENT(IN   )    ::                            PATM, &
                                                          QVATM, &
                                                          QCATM

   REAL                                                        , &
            INTENT(IN   )    ::                             GLW, &
                                                            GSW, &
                                                            RHO, &
                                                             PC, &
                                                        VEGFRAC, &
                                                           QKMS, &
                                                           TKMS


   REAL                                                        , &
            INTENT(IN   )    ::                                  &
                                                           BCLH, &
                                                            DQM, &
                                                           PSIS, &
                                                           QMIN

   REAL,     INTENT(IN   )   ::                              CP, &
                                                            CVW, &
                                                         STBOLT, &
                                                           XLVM, &
                                                            G0_P


   REAL,     DIMENSION(1:NZS), INTENT(IN)  ::            ZSMAIN, &
                                                         ZSHALF, &
                                                          THDIF, &
                                                            CAP, &
                                                          TRANF 

   REAL,     DIMENSION(1:NDDZS), INTENT(IN)  ::           DTDZS

   REAL,     DIMENSION(1:4001), INTENT(IN)  ::              TBQ




   REAL,     DIMENSION(  1:nzs )                               , &
             INTENT(INOUT)   ::                             TSO



   REAL                                                        , &
             INTENT(INOUT)   ::                             DEW, &
                                                            CST, &
                                                          RHOSN, &
                                                          EMISS, &
                                                         MAVAIL, &
                                                            QVG, &
                                                            QSG, &
                                                            QCG, &
                                                           SNWE, &
                                                          SNHEI, &
                                                       SNOWFRAC, &
                                                          SMELT, &
                                                           SNOH, &
                                                          SNFLX, &
                                                          SOILT, &
                                                         SOILT1, &
                                                          TSNAV

   REAL,     INTENT(INOUT)                  ::   DRYCAN, WETCAN           

   REAL,     INTENT(OUT)                    ::              RSM, &
                                                      SNWEPRINT, &
                                                     SNHEIPRINT
   INTEGER,  INTENT(OUT)                    ::             ilnb



   INTEGER ::  nzs1,nzs2,k,k1,kn,kk

   REAL    ::  x,x1,x2,x4,dzstop,can,ft,sph,                     &
               tn,trans,umveg,denom

   REAL    ::  cotsn,rhtsn,xsn1,ddzsn1,x1sn1,ftsnow,denomsn

   REAL    ::  t3,upflux,xinet,ras,                              &
               xlmelt,rhocsn,thdifsn,                            &
               beta,epot,xsn,ddzsn,x1sn,d1sn,d2sn,d9sn,r22sn

   REAL    ::  fso,fsn,                                          &
               FKT,D1,D2,D9,D10,DID,R211,R21,R22,R6,R7,D11,      &
               PI,H,FKQ,R210,AA,BB,PP,Q1,QS1,TS1,TQ2,TX2,        &
               TDENOM,C,CC,AA1,RHCS,H1,                          &
               tsob, snprim, sh1, sh2,                           &
               smeltg,snohg,snodif,soh,                          &
               CMC2MS,TNOLD,QGOLD,SNOHGNEW                            

   REAL,     DIMENSION(1:NZS)  ::  transp,cotso,rhtso
   REAL                        ::                         edir1, &
                                                            ec1, &
                                                           ett1, &
                                                           eeta, &
                                                              s, &
                                                            qfx, &
                                                            hfx

   REAL                        :: RNET,rsmfrac,soiltfrac,hsn



       do k=1,nzs
          transp   (k)=0.
          cotso    (k)=0.
          rhtso    (k)=0.
       enddo

    IF ( wrf_at_debug_level(3000) ) THEN
print *, 'SNOWTEMP: SNHEI,SNTH,SOILT1: ',SNHEI,SNTH,SOILT1,soilt 
    ENDIF
        XLMELT=3.335E+5
        RHOCSN=2090.* RHOSN
        THDIFSN = 0.265/RHOCSN
        RAS=RHO*1.E-3

        SOILTFRAC=SOILT

        SMELT=0.
        SOH=0.
        SMELTG=0.
        SNOHG=0.
        SNODIF=0.
        RSM = 0.
        RSMFRAC = 0.
        fsn=1.
        fso=0.
        hsn=snhei

          NZS1=NZS-1
          NZS2=NZS-2

        QGOLD=QVG
        TNOLD=SOILT
        DZSTOP=1./(ZSMAIN(2)-ZSMAIN(1))










        cotso(1)=0.
        rhtso(1)=TSO(NZS)
        DO 33 K=1,NZS2
          KN=NZS-K
          K1=2*KN-3
          X1=DTDZS(K1)*THDIF(KN-1)
          X2=DTDZS(K1+1)*THDIF(KN)
          FT=TSO(KN)+X1*(TSO(KN-1)-TSO(KN))                           &
             -X2*(TSO(KN)-TSO(KN+1))
          DENOM=1.+X1+X2-X2*cotso(K)
          cotso(K+1)=X1/DENOM
          rhtso(K+1)=(FT+X2*rhtso(K))/DENOM
   33  CONTINUE


       IF(SNHEI.GE.SNTH) then

        if(snhei.le.DELTSN+SNTH) then

         ilnb=1
         snprim=snhei
         soilt1=tso(1)
         tsob=tso(1)
         XSN = DELT/2./(zshalf(2)+0.5*SNPRIM)
         DDZSN = XSN / SNPRIM
         X1SN = DDZSN * thdifsn
         X2 = DTDZS(1)*THDIF(1)
         FT = TSO(1)+X1SN*(SOILT-TSO(1))                              &
              -X2*(TSO(1)-TSO(2))
         DENOM = 1. + X1SN + X2 -X2*cotso(NZS1)
         cotso(NZS)=X1SN/DENOM
         rhtso(NZS)=(FT+X2*rhtso(NZS1))/DENOM
         cotsn=cotso(NZS)
         rhtsn=rhtso(NZS)

         tsnav=0.5*(soilt+tso(1))                                     &
                     -273.15

        else

         ilnb=2
         snprim=deltsn
         tsob=soilt1
         XSN = DELT/2./(0.5*SNHEI)
         XSN1= DELT/2./(zshalf(2)+0.5*(SNHEI-DELTSN))
         DDZSN = XSN / DELTSN
         DDZSN1 = XSN1 / (SNHEI-DELTSN)
         X1SN = DDZSN * thdifsn
         X1SN1 = DDZSN1 * thdifsn
         X2 = DTDZS(1)*THDIF(1)
         FT = TSO(1)+X1SN1*(SOILT1-TSO(1))                            &
              -X2*(TSO(1)-TSO(2))
         DENOM = 1. + X1SN1 + X2 - X2*cotso(NZS1)
         cotso(nzs)=x1sn1/denom
         rhtso(nzs)=(ft+x2*rhtso(nzs1))/denom
         ftsnow = soilt1+x1sn*(soilt-soilt1)                          &
               -x1sn1*(soilt1-tso(1))
         denomsn = 1. + X1SN + X1SN1 - X1SN1*cotso(NZS)
         cotsn=x1sn/denomsn
         rhtsn=(ftsnow+X1SN1*rhtso(NZS))/denomsn

         tsnav=0.5/snhei*((soilt+soilt1)*deltsn                       &
                     +(soilt1+tso(1))*(SNHEI-DELTSN))                 &
                     -273.15
        endif
       ENDIF

       IF(SNHEI.LT.SNTH.AND.SNHEI.GT.0.) then


         fsn=SNHEI/(SNHEI+zsmain(2))
         fso=1.-fsn
         soilt1=tso(1)
         tsob=tso(2)
         snprim=SNHEI+zsmain(2)
         XSN = DELT/2./((zshalf(3)-zsmain(2))+0.5*snprim)
         DDZSN = XSN /snprim
         X1SN = DDZSN * (fsn*thdifsn+fso*thdif(1))
         X2=DTDZS(2)*THDIF(2)
         FT=TSO(2)+X1SN*(SOILT-TSO(2))-                              &
                       X2*(TSO(2)-TSO(3))
         denom = 1. + x1sn + x2 - x2*cotso(nzs-2)
         cotso(nzs1) = x1sn/denom
         rhtso(nzs1)=(FT+X2*rhtso(NZS-2))/denom
         tsnav=0.5*(soilt+tso(1))                                    &
                     -273.15
       ENDIF




        ETT1=0.
        EPOT=-QKMS*(QVATM-QSG)
        RHCS=CAP(1)
        H=MAVAIL
        IF(DEW.NE.0.)THEN
          DRYCAN=0.
          WETCAN=1.
        ENDIF
        TRANS=PC*TRANSUM*DRYCAN/ZSHALF(NROOT+1)
        CAN=WETCAN+TRANS
        UMVEG=1.-VEGFRAC
        FKT=TKMS
        D1=cotso(NZS1)
        D2=rhtso(NZS1)
        TN=SOILT
        D9=THDIF(1)*RHCS*dzstop
        D10=TKMS*CP*RHO
        R211=.5*CONFLX/DELT
        R21=R211*CP*RHO
        R22=.5/(THDIF(1)*DELT*dzstop**2)
        R6=EMISS *STBOLT*.5*TN**4
        R7=R6/TN
        D11=RNET+R6

      IF(SNHEI.GE.SNTH) THEN

        if(snhei.le.DELTSN+SNTH) then

          D1SN = cotso(NZS)
          D2SN = rhtso(NZS)
        else

          D1SN = cotsn
          D2SN = rhtsn
        endif
        D9SN= THDIFSN*RHOCSN / SNPRIM
        R22SN = SNPRIM*SNPRIM*0.5/(THDIFSN*DELT)
      ENDIF

       IF(SNHEI.LT.SNTH.AND.SNHEI.GT.0.) then

         D1SN = D1
         D2SN = D2
         D9SN = (fsn*THDIFSN*RHOCSN+fso*THDIF(1)*RHCS)/              &
                 snprim
         R22SN = snprim*snprim*0.5                                   &
                 /((fsn*THDIFSN+fso*THDIF(1))*delt)
      ENDIF

      IF(SNHEI.eq.0.)then

        D9SN = D9
        R22SN = R22
        D1SN = D1
        D2SN = D2
    IF ( wrf_at_debug_level(3000) ) THEN
        print *,' SNHEI = 0, D9SN,R22SN,D1SN,D2SN: ',D9SN,R22SN,D1SN,D2SN
    ENDIF
      ENDIF



        TDENOM = D9SN*(1.-D1SN +R22SN)+D10+R21+R7                    &
              +RAINF*CVW*PRCPMS                                      &
              +RHOCSN*NEWSNOW/DELT

        FKQ=QKMS*RHO
        R210=R211*RHO
        C=VEGFRAC*FKQ*CAN
        CC=C*XLVM/TDENOM
        AA=XLVM*(BETA*FKQ*UMVEG+R210)/TDENOM
        BB=(D10*TABS+R21*TN+XLVM*(QVATM*                             &
        (BETA*FKQ*UMVEG+C)                                           &
        +R210*QVG)+D11+D9SN*(D2SN+R22SN*TN)                          &
        +RAINF*CVW*PRCPMS*max(273.15,TABS)                           &
        + RHOCSN*NEWSNOW/DELT*min(273.15,TABS)                       &
         )/TDENOM
        AA1=AA+CC
        PP=PATM*1.E3
        AA1=AA1/PP
    IF ( wrf_at_debug_level(3000) ) THEN
        print *,'VILKA-SNOW'
        print *,'tn,aa1,bb,pp,umveg,fkq,r210,vegfrac',               &
                 tn,aa1,bb,pp,umveg,fkq,r210,vegfrac
    ENDIF

        CALL VILKA(TN,AA1,BB,PP,QS1,TS1,TBQ,KTAU,i,j,iland,isoil)
        TQ2=QVATM+QCATM
        TX2=TQ2*(1.-H)
        Q1=TX2+H*QS1

   90   QVG=QS1
        QSG=QS1
        QCG=Q1-QS1


        SOILT=TS1

    IF ( wrf_at_debug_level(3000) ) THEN
        print *,' AFTER VILKA-SNOW'
        print *,' TS1,QS1: ', ts1,qs1
    ENDIF


       IF(SNHEI.GE.SNTH) THEN

        if(snhei.gt.DELTSN+SNTH) then

          SOILT1=rhtsn+cotsn*SOILT
          TSO(1)=rhtso(NZS)+cotso(NZS)*SOILT1
          tsob=soilt1
        else

          TSO(1)=rhtso(NZS)+cotso(NZS)*SOILT
          SOILT1=TSO(1)
          tsob=tso(1)
        endif
       ELSE

         TSO(1)=SOILT
         SOILT1=SOILT
         tsob=SOILT
       ENDIF


          DO K=2,NZS
            KK=NZS-K+1
            TSO(K)=rhtso(KK)+cotso(KK)*TSO(K-1)
          END DO




       if(SNHEI.LT.SNTH.AND.SNHEI.GT.0.)then
          tso(1)=tso(2)+(soilt-tso(2))*fso
          SOILT1=TSO(1)
          tsob=tso(2)

       endif


   IF(SOILT.GE.273.15.AND.SNHEI.GT.0.) THEN

        soiltfrac=snowfrac*273.15+(1.-snowfrac)*SOILT
        soilt=soiltfrac
         QSG= QSN(soilt,TBQ)/PP

         QVG=QSG
        T3      = STBOLT*SOILT*SOILT*SOILT
        UPFLUX  = T3 * SOILT
        XINET   = EMISS*(GLW-UPFLUX)
        RNET = GSW + XINET
         EPOT = -QKMS*(QVATM-QSG)
         Q1=EPOT*RAS

        IF (Q1.LE.0.) THEN

          DEW=-EPOT
          DO K=1,NZS
            TRANSP(K)=0.
          ENDDO

        QFX= XLVM*RHO*DEW
        EETA=QFX/XLVM
       ELSE

          DO K=1,NROOT
            TRANSP(K)=-VEGFRAC*q1                                     &
                      *PC*TRANF(K)*DRYCAN/zshalf(NROOT+1)
           IF(TRANSP(K).GT.0.) TRANSP(K)=0.
            ETT1=ETT1-TRANSP(K)
          ENDDO
          DO k=nroot+1,nzs
            transp(k)=0.
          enddo

        EDIR1 = Q1*UMVEG * BETA
        EC1 = Q1 * WETCAN *VEGFRAC
        CMC2MS=CST/DELT
        EC1=MIN(CMC2MS,EC1)
        EETA = (EDIR1 + EC1 + ETT1)*1.E3

        QFX= - XLVM * EETA
       ENDIF

         HFX=D10*(TABS-soilt)


       IF(SNHEI.GE.SNTH)then
         SOH=thdifsn*RHOCSN*(soilt-TSOB)/SNPRIM

         SNFLX=SOH
       ELSE
         SOH=(fsn*thdifsn*rhocsn+fso*thdif(1)*rhcs)*                  &
              (soilt-TSOB)/snprim

         SNFLX=SOH
       ENDIF

         X= (R21+D9SN*R22SN)*(soilt-TNOLD) +                         &

            XLVM*R210*(QSG-QGOLD)

        SNOH=RNET+QFX +HFX                                           & 
                  +RHOCSN*NEWSNOW/DELT*(min(273.15,TABS)-TN)         &
                  -SOH-X+RAINF*CVW*PRCPMS*                           &
                  (max(273.15,TABS)-TN) 
        SNOH=AMAX1(0.,SNOH)

        SMELT= SNOH /XLMELT*1.E-3
        SMELT=AMIN1(SMELT,SNWEPR/DELT-BETA*EPOT*RAS)


        SNOHGNEW=SMELT*XLMELT*1.E3
        SNODIF=AMAX1(0.,(SNOH-SNOHGNEW))

        SNOH=SNOHGNEW




       if(snwe.gt.0.) then
         rsmfrac=min(0.18,(max(0.08,0.10/snwe*0.13)))
       else
         rsmfrac=0.13
       endif

         rsm=rsmfrac*smelt*delt
        SMELT=SMELT-rsm/delt



        SNWE = AMAX1(0.,(SNWEPR-                                      &
                    (SMELT+BETA*EPOT*RAS)*DELT                        &

                                         ) )




        if(snwe.le.rsm) then
           smelt=smelt+rsm/delt
           snwe=0.
           rsm=0.
           SOILT=SNODIF*DELT/RHCS*ZSHALF(2)                          &  
                   +soiltfrac

        else




           
          if(snwe.gt.snth*rhosn*1.e-3) then
         xsn=(rhosn*(snwe-rsm)+1.e3*rsm)/                            &
             snwe
         rhosn=MIN(XSN,400.)

        RHOCSN=2090.* RHOSN
        thdifsn = 0.265/RHOCSN
          endif  

         SOILT=SOILTFRAC

        endif



      ELSE
               EPOT=-QKMS*(QVATM-QSG)
               SNWE = AMAX1(0.,(SNWEPR-                               &
                    BETA*EPOT*RAS*DELT))


      ENDIF





        SNHEI=SNWE *1.E3 / RHOSN





        IF(TSO(1).GE.273.15.AND.SNHEI.GT.0.) THEN
         soiltfrac=snowfrac*273.15+(1.-snowfrac)*TSO(1)

        SNOHG=(TSO(1)-soiltfrac)*(RHCS*zshalf(2)+                       &
               RHOCSN*0.5*SNHEI) / DELT
        SNOHG=AMAX1(0.,SNOHG)
        SNODIF=0. 

        SMELTG=SNOHG/XLMELT*1.E-3

       if(SNWE-SMELTG*DELT.ge.rsm) then

        SNWE = AMAX1(0.,SNWE-SMELTG*DELT)
       else
           smeltg=snwe/delt
           snwe=0.
           rsm=0.
       endif

        SNOHGNEW=SMELTG*XLMELT*1.e3
        SNODIF=AMAX1(0.,(SNOHG-SNOHGNEW))
          TSO(1)=soiltfrac
         if(snwe.eq.0.)then
        TSO(1)=SNODIF*DELT/RHCS*zshalf(2) + soiltfrac

          endif

        SMELT=SMELT+SMELTG
        SNOH=SNOH+SNOHGNEW

       ENDIF

        SNHEI=SNWE *1.E3 / RHOSN

        snweprint=snwe




        snheiprint=snweprint*1.E3 / RHOSN

    IF ( wrf_at_debug_level(3000) ) THEN
print *, 'snweprint : ',snweprint
print *, 'D9SN,SOILT,TSOB : ', D9SN,SOILT,TSOB
    ENDIF

      SNFLX=D9SN*(SOILT-TSOB)
      IF(SNHEI.GT.0.) THEN
        if(ilnb.gt.1) then
          tsnav=0.5/snhei*((soilt+soilt1)*deltsn                     &
                    +(soilt1+tso(1))*(SNHEI-DELTSN))                 &
                       -273.15
        else
          tsnav=0.5*(soilt+tso(1)) - 273.15
        endif
      ENDIF




   END SUBROUTINE SNOWTEMP



        SUBROUTINE SOILMOIST (                                  &

              DELT,NZS,NDDZS,DTDZS,DTDZS2,                      &
              ZSMAIN,ZSHALF,DIFFU,HYDRO,                        &
              QSG,QVG,QCG,QCATM,QVATM,PRCP,                     &
              QKMS,TRANSP,DRIP,                                 &
              DEW,SMELT,SOILICE,VEGFRAC,                        &

              DQM,QMIN,REF,KSAT,RAS,INFMAX,                     &

              SOILMOIS,MAVAIL,RUNOFF,RUNOFF2,INFILTRP)








































        IMPLICIT NONE


   REAL,     INTENT(IN   )   ::  DELT
   INTEGER,  INTENT(IN   )   ::  NZS,NDDZS



   REAL,     DIMENSION(1:NZS), INTENT(IN   )  ::         ZSMAIN, &
                                                         ZSHALF, &
                                                          DIFFU, &
                                                          HYDRO, &
                                                         TRANSP, &
                                                        SOILICE, &
                                                         DTDZS2

   REAL,     DIMENSION(1:NDDZS), INTENT(IN)  ::           DTDZS

   REAL,     INTENT(IN   )   ::    QSG,QVG,QCG,QCATM,QVATM     , &
                                   QKMS,VEGFRAC,DRIP,PRCP      , &
                                   DEW,SMELT                   , &
                                   DQM,QMIN,REF,KSAT,RAS
                         


   REAL,     DIMENSION(  1:nzs )                               , &

             INTENT(INOUT)   ::                        SOILMOIS     
                                                  
   REAL,     INTENT(INOUT)   ::  MAVAIL,RUNOFF,RUNOFF2,INFILTRP, &
                                                        INFMAX



   REAL,     DIMENSION( 1:nzs )  ::  COSMC,RHSMC

   REAL    ::  DZS,R1,R2,R3,R4,R5,R6,R7,R8,R9,R10
   REAL    ::  REFKDT,REFDK,DELT1,F1MAX,F2MAX
   REAL    ::  F1,F2,FD,KDT,VAL,DDT,PX
   REAL    ::  QQ,UMVEG,INFMAX1,TRANS
   REAL    ::  TOTLIQ,FLX,FLXSAT,QTOT
   REAL    ::  DID,X1,X2,X4,DENOM,Q2,Q4
   REAL    ::  dice,fcr,acrt,frzx,sum,cvfrz

   INTEGER ::  NZS1,NZS2,K,KK,K1,KN,ialp1,jj,jk




          NZS1=NZS-1                                                            
          NZS2=NZS-2

 118      format(6(10Pf23.19))

           do k=1,nzs
            cosmc(k)=0.
            rhsmc(k)=0.
           enddo
 
        DID=(ZSMAIN(NZS)-ZSHALF(NZS))*2.
        X1=ZSMAIN(NZS)-ZSMAIN(NZS1)







        DENOM=(1.+DIFFU(nzs1)/X1/DID*DELT+HYDRO(NZS)/(2.*DID)*DELT)
        COSMC(1)=DELT*(DIFFU(nzs1)/DID/X1                                 &
                    +HYDRO(NZS1)/2./DID)/DENOM
        RHSMC(1)=(SOILMOIS(NZS)+TRANSP(NZS)*DELT/                         &
               DID)/DENOM

        DO 330 K=1,NZS2
          KN=NZS-K
          K1=2*KN-3
          X4=2.*DTDZS(K1)*DIFFU(KN-1)
          X2=2.*DTDZS(K1+1)*DIFFU(KN)
          Q4=X4+HYDRO(KN-1)*DTDZS2(KN-1)
          Q2=X2-HYDRO(KN+1)*DTDZS2(KN-1)
          DENOM=1.+X2+X4-Q2*COSMC(K)
          COSMC(K+1)=Q4/DENOM
 330      RHSMC(K+1)=(SOILMOIS(KN)+Q2*RHSMC(K)                            &
                   +TRANSP(KN)                                            &
                   /(ZSHALF(KN+1)-ZSHALF(KN))                             &
                   *DELT)/DENOM



          TRANS=TRANSP(1)
          UMVEG=1.-VEGFRAC

          RUNOFF=0.
          RUNOFF2=0.
          DZS=ZSMAIN(2)
          R1=COSMC(NZS1)
          R2= RHSMC(NZS1)
          R3=DIFFU(1)/DZS
          R4=R3+HYDRO(1)*.5          
          R5=R3-HYDRO(2)*.5
          R6=QKMS*RAS













  191   format (f23.19)

        TOTLIQ=UMVEG*PRCP-DRIP/DELT-UMVEG*DEW*RAS-SMELT


        FLX=TOTLIQ
        INFILTRP=TOTLIQ










         CVFRZ = 3.


         REFKDT=3.
         REFDK=3.4341E-6
         DELT1=DELT/86400.
         F1MAX=DQM*ZSHALF(2)
         F2MAX=DQM*(ZSHALF(3)-ZSHALF(2))
         F1=F1MAX*(1.-SOILMOIS(1)/DQM)
         F2=F2MAX*(1.-SOILMOIS(2)/DQM)
         FD=F1+F2
         KDT=REFKDT*KSAT/REFDK
         VAL=(1.-EXP(-KDT*DELT1))
         DDT = FD*VAL
         PX= - TOTLIQ * DELT
         IF(PX.LT.0.0) PX = 0.0
       if(ddt.eq.0.) then
         infmax1=ksat
        else
         INFMAX1 = (PX*(DDT/(PX+DDT)))/DELT
         INFMAX1 = MIN(INFMAX1, KSAT)
        endif






          DICE = soilice(1)*zshalf(2)
      DO K=2,NZS1
          DICE = DICE + ( ZSHALF(K+1) - ZSHALF(K) ) * soilice(k)
      ENDDO
         FRZX= 0.28*((dqm+qmin)/ref) * (0.400 / 0.482)
         FCR = 1.
         IF ( DICE .GT. 1.E-2) THEN
           ACRT = CVFRZ * FRZX / DICE
           SUM = 1.
           IALP1 = CVFRZ - 1
           DO JK = 1,IALP1
              K = 1
              DO JJ = JK+1, IALP1
                K = K * JJ
              END DO
              SUM = SUM + (ACRT ** ( CVFRZ-JK)) / FLOAT (K)
           END DO
           FCR = 1. - EXP(-ACRT) * SUM
         END IF

         INFMAX1 = INFMAX1* FCR
         INFMAX1 = MIN(INFMAX1, KSAT)


         INFMAX = MIN(INFMAX,INFMAX1)

          IF (-TOTLIQ.GE.INFMAX)THEN
            RUNOFF=-TOTLIQ-INFMAX
            FLX=-INFMAX
          ENDIF

          INFILTRP=FLX


          R7=.5*DZS/DELT
          R4=R4+R7
          FLX=FLX-SOILMOIS(1)*R7
          R8=UMVEG*R6
          QTOT=QVATM+QCATM
          R9=TRANS
          R10=QTOT-QSG

          IF(R10.LE.0.) THEN
            QQ=(R5*R2-FLX+R9)/(R4-R5*R1-R10*R8/(REF-QMIN))
            FLXSAT=-DQM*(R4-R5*R1-R10*R8/(REF-QMIN))                &
                   +R5*R2+R9
          ELSE

            QQ=(R2*R5-FLX+R8*(QTOT-QCG-QVG)+R9)/(R4-R1*R5)
            FLXSAT=-DQM*(R4-R1*R5)+R2*R5+R8*(QTOT-QVG-QCG)+R9
          END IF

          IF(QQ.LT.0.) THEN
            SOILMOIS(1)=0.

          ELSE IF(QQ.GT.DQM) THEN

            SOILMOIS(1)=DQM
            RUNOFF2=runoff2+(FLXSAT-FLX)*DELT
            RUNOFF=RUNOFF+(FLXSAT-FLX)
          ELSE
            SOILMOIS(1)=max(1.e-8,QQ)
          END IF



          DO K=2,NZS-1
            KK=NZS-K+1
            QQ=COSMC(KK)*SOILMOIS(K-1)+RHSMC(KK)

           IF (QQ.LT.0.) THEN
            SOILMOIS(K)=0.

           ELSE IF(QQ.GT.DQM) THEN

            SOILMOIS(K)=DQM
             IF(K.EQ.NZS)THEN
               RUNOFF2=RUNOFF2+(QQ-DQM)*(ZSMAIN(K)-ZSHALF(K))
             ELSE
               RUNOFF2=RUNOFF2+(QQ-DQM)*(ZSMAIN(K+1)-ZSHALF(K))
             ENDIF
           ELSE
            SOILMOIS(K)=max(1.e-8,QQ)
           END IF
          END DO


           MAVAIL=min(1.,SOILMOIS(1)/DQM)
          if (MAVAIL.EQ.0.) MAVAIL=.00001




    END SUBROUTINE SOILMOIST



            SUBROUTINE SOILPROP(                                  &

         nzs,fwsat,lwsat,tav,keepfr,                              &
         soilmois,soiliqw,soilice,                                &
         soilmoism,soiliqwm,soilicem,                             &

         QWRTZ,rhocs,dqm,qmin,psis,bclh,ksat,                     &

         riw,xlmelt,CP,G0_P,cvw,ci,                               & 
         kqwrtz,kice,kwt,                                         &

         thdif,diffu,hydro,cap)




















        IMPLICIT NONE



   INTEGER, INTENT(IN   )    ::                            NZS
   REAL                                                        , &
            INTENT(IN   )    ::                           RHOCS, &
                                                           BCLH, &
                                                            DQM, &
                                                           KSAT, &
                                                           PSIS, &
                                                          QWRTZ, &  
                                                           QMIN

   REAL,    DIMENSION(  1:nzs )                                , &
            INTENT(IN   )    ::                        SOILMOIS, &
                                                         keepfr


   REAL,     INTENT(IN   )   ::                              CP, &
                                                            CVW, &
                                                            RIW, &  
                                                         kqwrtz, &
                                                           kice, &
                                                            kwt, &
                                                         XLMELT, &
                                                            G0_P




   REAL,     DIMENSION(1:NZS)                                  , &
            INTENT(INOUT)  ::      cap,diffu,hydro             , &
                                   thdif,tav                   , &
                                   soilmoism                   , &
                                   soiliqw,soilice             , &
                                   soilicem,soiliqwm           , &
                                   fwsat,lwsat


   REAL,     DIMENSION(1:NZS)  ::  hk,detal,kasat,kjpl

   REAL    ::  x,x1,x2,x4,ws,wd,fact,fach,facd,psif,ci
   REAL    ::  tln,tavln,tn,pf,a,am,ame,h
   INTEGER ::  nzs1,k


   REAL    ::  kzero,gamd,kdry,kas,x5,sr,ke       
               

         nzs1=nzs-1


         kzero =2.       


         do k=1,nzs
            detal (k)=0.
            kasat (k)=0.
            kjpl  (k)=0.
            hk    (k)=0.
         enddo

           ws=dqm+qmin
           x1=xlmelt/(g0_p*psis)
           x2=x1/bclh*ws
           x4=(bclh+1.)/bclh

           gamd=(1.-ws)*2700.
           kdry=(0.135*gamd+64.7)/(2700.-0.947*gamd)
           kas=kqwrtz**qwrtz*kzero**(1.-qwrtz)

         DO K=1,NZS1
           tn=tav(k) - 273.15
           wd=ws - riw*soilicem(k)
           psif=psis*100.*(wd/(soiliqwm(k)+qmin))**bclh            &
                * (ws/wd)**3.

           pf=log10(abs(psif))
           fact=1.+riw*soilicem(k)

         IF(PF.LE.5.2) THEN
           HK(K)=420.*EXP(-(PF+2.7))*fact
         ELSE
           HK(K)=.1744*fact
         END IF

           IF(soilicem(k).NE.0.AND.TN.LT.0.) then



              DETAL(K)=273.15*X2/(TAV(K)*TAV(K))*                  &
                     (TAV(K)/(X1*TN))**X4

              if(keepfr(k).eq.1.) then
                 detal(k)=0.
              endif

           ENDIF


           kasat(k)=kas**(1.-ws)*kice**fwsat(k)                    &
                    *kwt**lwsat(k)

           X5=(soilmoism(k)+qmin)/ws
         if(soilicem(k).eq.0.) then
           sr=max(0.101,x5)
           ke=log10(sr)+1.



         else
           ke=x5
         endif

           kjpl(k)=ke*(kasat(k)-kdry)+kdry


            CAP(K)=(1.-WS)*RHOCS                                    &
                  + (soiliqwm(K)+qmin)*CVW                          &
                  + soilicem(K)*CI                                  &
                  + (dqm-soilmoism(k))*CP*1.2                       &
            - DETAL(K)*1.e3*xlmelt

           a=RIW*soilicem(K)

        if((ws-a).lt.0.12)then
           diffu(K)=0.
        else
           H=max(0.,(soilmoism(K)-a)/(max(1.e-8,(dqm-a))))
           facd=1.
        if(a.ne.0.)facd=1.-a/max(1.e-8,soilmoism(K))
          ame=max(1.e-8,dqm-riw*soilicem(K))

          diffu(K)=-BCLH*KSAT*PSIS/ame*                             &
                  (dqm/ame)**3.                                     &
                  *H**(BCLH+2.)*facd
         endif








            thdif(K)=KJPL(K)/CAP(K)

         END DO

         DO K=1,NZS

         if((ws-riw*soilice(k)).lt.0.12)then
            hydro(k)=0.
         else
            fach=1.
          if(soilice(k).ne.0.)                                     &
             fach=1.-riw*soilice(k)/max(1.e-8,soilmois(k))
         am=max(1.e-8,dqm-riw*soilice(k))

          hydro(K)=KSAT/am*                                        & 
                  (soiliqw(K)/am)                                  &
                  **(2.*BCLH+2.)                                   &
                  * fach
         endif

       ENDDO





   END SUBROUTINE SOILPROP



           SUBROUTINE TRANSF(                                    &

              nzs,nroot,soiliqw,tabs,                            &

              dqm,qmin,ref,wilt,zshalf,                          &

              tranf,transum)










        IMPLICIT NONE




   INTEGER,  INTENT(IN   )   ::  nroot,nzs

   REAL                                                        , &
            INTENT(IN   )    ::                            TABS

   REAL                                                        , &
            INTENT(IN   )    ::                             DQM, &
                                                           QMIN, &
                                                            REF, &
                                                           WILT

   REAL,     DIMENSION(1:NZS), INTENT(IN)  ::          soiliqw,  &
                                                         ZSHALF


   REAL,     DIMENSION(1:NZS), INTENT(OUT)  ::            TRANF
   REAL,     INTENT(OUT)  ::                            TRANSUM  


   REAL    ::  totliq, did
   INTEGER ::  k


   REAL    ::  gx,sm1,sm2,sm3,sm4,ap0,ap1,ap2,ap3,ap4
   REAL    ::  FTEM
   REAL,     DIMENSION(1:NZS)   ::           PART


        do k=1,nzs
           part(k)=0.
        enddo

        transum=0.
        totliq=soiliqw(1)+qmin
           sm1=totliq
           sm2=sm1*sm1
           sm3=sm2*sm1
           sm4=sm3*sm1
           ap0=0.299
           ap1=-8.152
           ap2=61.653
           ap3=-115.876
           ap4=59.656
           gx=ap0+ap1*sm1+ap2*sm2+ap3*sm3+ap4*sm4
          if(totliq.ge.ref) gx=1.
          if(totliq.le.0.) gx=0.
          if(gx.gt.1.) gx=1.
          if(gx.lt.0.) gx=0.
        DID=zshalf(2)
          part(1)=DID*gx


        IF (TABS .LE. 302.15) THEN
          FTEM = 1.0 / (1.0 + EXP(-0.41 * (TABS - 282.05)))
        ELSE
          FTEM = 1.0 / (1.0 + EXP(0.5 * (TABS - 314.0)))
        ENDIF

        IF(TOTLIQ.GT.REF) THEN
          TRANF(1)=DID
        ELSE IF(TOTLIQ.LE.WILT) THEN
          TRANF(1)=0.
        ELSE
          TRANF(1)=(TOTLIQ-WILT)/(REF-WILT)*DID
        ENDIF 


          TRANF(1)=TRANF(1)*FTEM

        DO K=2,NROOT
        totliq=soiliqw(k)+qmin
           sm1=totliq
           sm2=sm1*sm1
           sm3=sm2*sm1
           sm4=sm3*sm1
           gx=ap0+ap1*sm1+ap2*sm2+ap3*sm3+ap4*sm4
          if(totliq.ge.ref) gx=1.
          if(totliq.le.0.) gx=0.
          if(gx.gt.1.) gx=1.
          if(gx.lt.0.) gx=0.
          DID=zshalf(K+1)-zshalf(K)
          part(k)=did*gx
        IF(totliq.GE.REF) THEN
          TRANF(K)=DID
        ELSE IF(totliq.LE.WILT) THEN
          TRANF(K)=0.
        ELSE
          TRANF(K)=(totliq-WILT)                                &
                /(REF-WILT)*DID
        ENDIF


          TRANF(k)=TRANF(k)*FTEM
        END DO


          transum=0.
        DO K=1,NROOT
          transum=transum+tranf(k)
        END DO




   END SUBROUTINE TRANSF



       SUBROUTINE VILKA(TN,D1,D2,PP,QS,TS,TT,NSTEP,ii,j,iland,isoil)




   REAL,     DIMENSION(1:4001),  INTENT(IN   )   ::  TT
   REAL,     INTENT(IN  )   ::  TN,D1,D2,PP
   INTEGER,  INTENT(IN  )   ::  NSTEP,ii,j,iland,isoil

   REAL,     INTENT(OUT  )  ::  QS, TS

   REAL    ::  F1,T1,T2,RN
   INTEGER ::  I,I1
     
       I=(TN-1.7315E2)/.05+1
       T1=173.1+FLOAT(I)*.05
       F1=T1+D1*TT(I)-D2
       I1=I-F1/(.05+D1*(TT(I+1)-TT(I)))
       I=I1
       IF(I.GT.4000.OR.I.LT.1) GOTO 1
  10   I1=I
       T1=173.1+FLOAT(I)*.05
       F1=T1+D1*TT(I)-D2
       RN=F1/(.05+D1*(TT(I+1)-TT(I)))
       I=I-INT(RN)                      
       IF(I.GT.4000.OR.I.LT.1) GOTO 1
       IF(I1.NE.I) GOTO 10
       TS=T1-.05*RN
       QS=(TT(I)+(TT(I)-TT(I+1))*RN)/PP
       GOTO 20
   1   PRINT *,'     AVOST IN VILKA      '

       PRINT *,TN,D1,D2,PP,NSTEP,I,TT(i),ii,j,iland,isoil
       CALL wrf_error_fatal3("module_sf_ruclsm.b",4051,&
'     AVOST IN VILKA      ' )
   20  CONTINUE



   END SUBROUTINE VILKA



     SUBROUTINE SOILVEGIN  ( IVGTYP,ISLTYP,MYJ,                         &
                     IFOREST,EMISS,PC,ZNT,QWRTZ,                        &
                     RHOCS,BCLH,DQM,KSAT,PSIS,QMIN,REF,WILT        )




















   IMPLICIT NONE

      integer,   parameter      ::      nsoilclas=19
      integer,   parameter      ::      nvegclas=24
      integer,   parameter      ::      iwater=16
      integer,   parameter      ::      ilsnow=99


























         REAL  LQMA(nsoilclas),LRHC(nsoilclas),                       &
               LPSI(nsoilclas),LQMI(nsoilclas),                       &
               LBCL(nsoilclas),LKAS(nsoilclas),                       &
               LWIL(nsoilclas),LREF(nsoilclas),                       &
               DATQTZ(nsoilclas)








     DATA LQMA /0.395, 0.410, 0.435, 0.485, 0.485, 0.451, 0.420,      &
                0.477, 0.476, 0.426, 0.492, 0.482, 0.451, 1.0,        &
                0.20,  0.435, 0.468, 0.200, 0.339/






        DATA LREF /0.174, 0.179, 0.249, 0.369, 0.369, 0.314, 0.299,   &
                   0.357, 0.391, 0.316, 0.409, 0.400, 0.314, 1.,      &
                   0.1,   0.249, 0.454, 0.17,  0.236/






        DATA LWIL/0.068, 0.075, 0.114, 0.179, 0.179, 0.155, 0.175,    &
                  0.218, 0.250, 0.219, 0.283, 0.286, 0.155, 0.0,      &
                  0.006, 0.114, 0.030, 0.006, 0.01/





        DATA LQMI/0.045, 0.057, 0.065, 0.067, 0.034, 0.078, 0.10,     &
                  0.089, 0.095, 0.10,  0.070, 0.068, 0.078, 0.0,      &
                  0.004, 0.065, 0.020, 0.004, 0.008/







       DATA LPSI/0.121, 0.090, 0.218, 0.786, 0.786, 0.478, 0.299,     &
                 0.356, 0.630, 0.153, 0.490, 0.405, 0.478, 0.0,       &
                 0.121, 0.218, 0.468, 0.069, 0.069/







        DATA LKAS/1.76E-4, 1.56E-4, 3.47E-5, 7.20E-6, 7.20E-6,         &
                  6.95E-6, 6.30E-6, 1.70E-6, 2.45E-6, 2.17E-6,         &
                  1.03E-6, 1.28E-6, 6.95E-6, 0.0,     1.41E-4,         &
                  3.47E-5, 1.28E-6, 1.41E-4, 1.76E-4/






        DATA LBCL/4.05,  4.38,  4.90,  5.30,  5.30,  5.39,  7.12,      &
                  7.75,  8.52, 10.40, 10.40, 11.40,  5.39,  0.0,       &
                  4.05,  4.90, 11.55,  2.79,  2.79/

        DATA LRHC /1.47,1.41,1.34,1.27,1.27,1.21,1.18,1.32,1.23,       &
                   1.18,1.15,1.09,1.21,4.18,2.03,2.10,1.09,2.03,1.47/

        DATA DATQTZ/0.92,0.82,0.60,0.25,0.10,0.40,0.60,0.10,0.35,      &
                    0.52,0.10,0.25,0.00,0.,0.60,0.0,0.25,0.60,0.92/




































         REAL LALB(nvegclas),LMOI(nvegclas),LEMI(nvegclas),            &
              LROU(nvegclas),LTHI(nvegclas),LSIG(nvegclas),            &
              LPC(nvegclas), NROTBL(nvegclas)






        DATA  LALB/.18,.17,.18,.18,.18,.16,.19,.22,.20,.20,.16,.14,     &
                   .12,.12,.13,.08,.14,.14,.25,.15,.15,.15,.25,.55/
        DATA LEMI/.88,4*.92,.93,.92,.88,.9,.92,.93,.94,                 &
                  .95,.95,.94,.98,.95,.95,.85,.92,.93,.92,.85,.95/



         DATA LROU/.5,.06,.075,.065,.05,.2,.075,.1,.11,.15,.5,.5,           & 
                   .5,.5,.5,.0001,.2,.4,.05,.1,.15,.1,.065,.05/

        DATA LMOI/.1,.3,.5,.25,.25,.35,.15,.1,.15,.15,.3,.3,            &
                  .5,.3,.3,1.,.6,.35,.02,.5,.5,.5,.02,.95/




       DATA LPC /0.4,0.3,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,5*0.55,0.,0.55,0.55,                   &
                 0.3,0.3,0.4,0.4,0.3,0./








   INTEGER      ::                &
                                                         IVGTYP, &
                                                         ISLTYP

   LOGICAL,    INTENT(IN   )    ::     myj

   REAL                                                        , &
            INTENT (  OUT)            ::                     pc

   REAL                                                        , &
            INTENT (INOUT   )         ::                  emiss, &
                                                            znt

   REAL                                                        , &
            INTENT(  OUT)    ::                           RHOCS, &
                                                           BCLH, &
                                                            DQM, &
                                                           KSAT, &
                                                           PSIS, &
                                                           QMIN, &
                                                          QWRTZ, &
                                                            REF, &
                                                           WILT

   INTEGER, DIMENSION( 1:(nvegclas+3) )                            , &
            INTENT (  OUT)            ::                iforest



   INTEGER, DIMENSION( 1:(nvegclas+3) )   ::   if1
   INTEGER   ::   kstart, kfin, lstart, lfin
   INTEGER   ::   i,j,k




        DATA IF1/12*0,1,1,1,12*0/

          do k=1,nvegclas+3
             iforest(k)=if1(k)
          enddo



        EMISS = LEMITBL(IVGTYP)








        ZNT   = Z0TBL(IVGTYP)



        PC     = PCTBL(IVGTYP)

        RHOCS  = HC(ISLTYP)*1.E6


          BCLH   = BB(ISLTYP)
          DQM    = MAXSMC(ISLTYP)-                               &
                   DRYSMC(ISLTYP)
          KSAT   = SATDK(ISLTYP)
          PSIS   = - SATPSI(ISLTYP)
          QMIN   = DRYSMC(ISLTYP)
          REF    = REFSMC(ISLTYP)
          WILT   = WLTSMC(ISLTYP)
          QWRTZ  = QTZ(ISLTYP)












   END SUBROUTINE SOILVEGIN



      SUBROUTINE SNOWFREE (ivgtyp,myj,emiss,znt,iland)







   IMPLICIT NONE

   integer,   parameter      ::      nvegclas=24


   INTEGER                   ::      IVGTYP

   LOGICAL,    INTENT(IN   )    ::     myj

   REAL,     INTENT(INOUT)   ::                                 &
                                                         emiss, &
                                                           znt  
   INTEGER,  INTENT(INOUT)   ::      ILAND
 

   REAL,    DIMENSION( 1:nvegclas )   ::                  LALB, &
                                                          LEMI, &
                                                       LROU_MYJ,&
                                                          LROU




        DATA  LALB/.18,.17,.18,.18,.18,.16,.19,.22,.20,.20,.16,.14,     &
                   .12,.12,.13,.08,.14,.14,.25,.15,.15,.15,.25,.55/
        DATA LEMI/.88,4*.92,.93,.92,.88,.9,.92,.93,.94,                 &
                  .95,.95,.94,.98,.95,.95,.85,.92,.93,.92,.85,.95/





         DATA LROU/.5,.06,.075,.065,.05,.2,.075,.1,.11,.15,.5,.5,           &
                   .5,.5,.5,.0001,.2,.4,.05,.1,.15,.1,.065,.05/


        DATA LROU_MYJ/1.0,.07,.07,.07,.07,.15,.08,.03,.05,.86,.8,.85,       &
                  2.65,1.09,.8,.001,.04,.05,.01,.04,.06,.05,.03,.001/





        EMISS  = LEMITBL(IVGTYP)




        ZNT    = Z0TBL(IVGTYP)


        ILAND  =      IVGTYP





   END SUBROUTINE SNOWFREE

  SUBROUTINE LSMRUCINIT( SMFR3D,TSLB,SMOIS,ISLTYP,mavail,          &
                     nzs, restart,                                 &
                     allowed_to_read ,                             &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte                     )
   IMPLICIT NONE


   INTEGER,  INTENT(IN   )   ::     ids,ide, jds,jde, kds,kde,  &
                                    ims,ime, jms,jme, kms,kme,  &
                                    its,ite, jts,jte, kts,kte,  &
                                    nzs

   REAL, DIMENSION( ims:ime, 1:nzs, jms:jme )                    , &
            INTENT(IN)    ::                                 TSLB, &
                                                            SMOIS

   INTEGER, DIMENSION( ims:ime, jms:jme )                        , &
            INTENT(INOUT)    ::                            ISLTYP

   REAL, DIMENSION( ims:ime, 1:nzs, jms:jme )                    , &
            INTENT(INOUT)    ::                            SMFR3D

   REAL, DIMENSION( ims:ime, jms:jme )                           , &
            INTENT(INOUT)    ::                            MAVAIL

   REAL, DIMENSION ( 1:nzs )  ::                           SOILIQW

   LOGICAL , INTENT(IN) :: restart, allowed_to_read 


  INTEGER ::  I,J,L,itf,jtf
  REAL    ::  RIW,XLMELT,TLN,DQM,REF,PSIS,QMIN,BCLH

   INTEGER                   :: errflag





        RIW=900.*1.e-3
        XLMELT=3.335E+5


   IF ( allowed_to_read ) THEN
     CALL wrf_message( 'INITIALIZE THREE LSM RELATED TABLES' )
     CALL  RUCLSM_PARM_INIT
   ENDIF

 IF(.not.restart)THEN

   itf=min0(ite,ide-1)
   jtf=min0(jte,jde-1)

   errflag = 0
   DO j = jts,jtf
     DO i = its,itf
       IF ( ISLTYP( i,j ) .LT. 1 ) THEN
         errflag = 1
         WRITE(err_message,*)"module_sf_ruclsm.F: lsminit: out of range ISLTYP ",i,j,ISLTYP( i,j )
         CALL wrf_message(err_message)
       ENDIF
     ENDDO
   ENDDO
   IF ( errflag .EQ. 1 ) THEN
      CALL wrf_error_fatal3("module_sf_ruclsm.b",4488,&
"module_sf_ruclsm.F: lsminit: out of range value "// &
                            "of ISLTYP. Is this field in the input?" )
   ENDIF

   DO J=jts,jtf
       DO I=its,itf






          DQM    = MAXSMC   (ISLTYP(I,J)) -                               &
                   DRYSMC   (ISLTYP(I,J))
          REF    = REFSMC   (ISLTYP(I,J))
          PSIS   = - SATPSI (ISLTYP(I,J))
          QMIN   = DRYSMC   (ISLTYP(I,J))
          BCLH   = BB       (ISLTYP(I,J))




    if(isltyp(i,j).ne.14) then
           mavail(i,j) = max(0.00001,min(1.,smois(i,1,j)/dqm))

    else
           mavail(i,j) = 1.
    endif
         DO L=1,NZS
    if(isltyp(i,j).ne.14) then

         tln=log(TSLB(i,l,j)/273.15)

         if(tln.lt.0.) then
           soiliqw(l)=(dqm+qmin)*(XLMELT*                        &
         (tslb(i,l,j)-273.15)/tslb(i,l,j)/9.81/psis)             &
          **(-1./bclh)-qmin
           soiliqw(l)=max(0.,soiliqw(l))
           soiliqw(l)=min(soiliqw(l),smois(i,l,j))
           smfr3d(i,l,j)=(smois(i,l,j)-soiliqw(l))/RIW

         else
           smfr3d(i,l,j)=0.
         endif
    else

       smfr3d(i,l,j)=0.
    endif

          ENDDO


    ENDDO
   ENDDO

 ENDIF

  END SUBROUTINE lsmrucinit


        SUBROUTINE RUCLSM_PARM_INIT


        character*8 :: MMINLU, MMINSL

        MMINLU='USGS-RUC'
        MMINSL='STAS-RUC'
        call RUCLSM_SOILVEGPARM( MMINLU, MMINSL)


        END SUBROUTINE RUCLSM_PARM_INIT



        SUBROUTINE RUCLSM_SOILVEGPARM( MMINLU, MMINSL)


        IMPLICIT NONE

        integer :: LUMATCH, IINDEX, LC, NUM_SLOPE
        integer :: ierr
        INTEGER , PARAMETER :: OPEN_OK = 0

        character*8 :: MMINLU, MMINSL
        character*128 :: mess , message, vege_parm_string
        logical, external :: wrf_dm_on_monitor





























       IF ( wrf_dm_on_monitor() ) THEN

        OPEN(19, FILE='VEGPARM.TBL',FORM='FORMATTED',STATUS='OLD',IOSTAT=ierr)
        IF(ierr .NE. OPEN_OK ) THEN
          WRITE(message,FMT='(A)') &
          'module_sf_ruclsm.F: soil_veg_gen_parm: failure opening VEGPARM.TBL'
          CALL wrf_error_fatal3("module_sf_ruclsm.b",4618,&
message )
        END IF

        WRITE ( mess, * ) 'INPUT VEGPARM FOR ',MMINLU
        CALL wrf_message( mess )

        LUMATCH=0

 2000   FORMAT (A8)
        READ (19,'(A)') vege_parm_string
        outer : DO 
           READ (19,2000,END=2002)LUTYPE
           READ (19,*)LUCATS,IINDEX

           WRITE( mess , * ) 'VEGPARM FOR ',LUTYPE,' FOUND', LUCATS,' CATEGORIES'
           CALL wrf_message( mess )

           IF(LUTYPE.NE.MMINLU)THEN    
              write ( mess , * ) 'Skipping ', LUTYPE, ' table'
              CALL wrf_message( mess )
              DO LC=1,LUCATS
                 READ (19,*)
              ENDDO
              inner : DO               
                 READ (19,'(A)',END=2002) vege_parm_string
                 IF (TRIM(vege_parm_string) .EQ. "Vegetation Parameters") THEN
                    EXIT inner
                 END IF
               ENDDO inner
           ELSE
              LUMATCH=1
              write ( mess , * ) 'Found ', LUTYPE, ' table'
              CALL wrf_message( mess )
              EXIT outer                
           END IF

        ENDDO outer

        IF (LUMATCH == 1) then
           write ( mess , * ) 'Reading ',LUTYPE,' table'
           CALL wrf_message( mess )
           DO LC=1,LUCATS
              READ (19,*)IINDEX,ALBTBL(LC),Z0TBL(LC),LEMITBL(LC),PCTBL(LC), &
                         SHDTBL(LC),NROTBL(LC),RSTBL(LC),RGLTBL(LC),         &
                         HSTBL(LC),SNUPTBL(LC),LAITBL(LC),MAXALB(LC)
           ENDDO

           READ (19,*)
           READ (19,*)TOPT_DATA
           READ (19,*)
           READ (19,*)CMCMAX_DATA
           READ (19,*)
           READ (19,*)CFACTR_DATA
           READ (19,*)
           READ (19,*)RSMAX_DATA
           READ (19,*)
           READ (19,*)BARE
           READ (19,*)
           READ (19,*)NATURAL
        ENDIF

 2002   CONTINUE
        CLOSE (19)

        IF (LUMATCH == 0) then
           CALL wrf_error_fatal3("module_sf_ruclsm.b",4684,&
"Land Use Dataset '"//MMINLU//"' not found in VEGPARM.TBL.")
        ENDIF

      END IF

      CALL wrf_dm_bcast_string  ( LUTYPE  , 8 )
      CALL wrf_dm_bcast_integer ( LUCATS  , 1 )
      CALL wrf_dm_bcast_integer ( IINDEX  , 1 )
      CALL wrf_dm_bcast_integer ( LUMATCH , 1 )
      CALL wrf_dm_bcast_real    ( ALBTBL  , NLUS )
      CALL wrf_dm_bcast_real    ( Z0TBL   , NLUS )
      CALL wrf_dm_bcast_real    ( LEMITBL , NLUS )
      CALL wrf_dm_bcast_real    ( PCTBL   , NLUS )
      CALL wrf_dm_bcast_real    ( SHDTBL  , NLUS )
      CALL wrf_dm_bcast_real    ( NROTBL  , NLUS )
      CALL wrf_dm_bcast_real    ( RSTBL   , NLUS )
      CALL wrf_dm_bcast_real    ( RGLTBL  , NLUS )
      CALL wrf_dm_bcast_real    ( HSTBL   , NLUS )
      CALL wrf_dm_bcast_real    ( SNUPTBL , NLUS )
      CALL wrf_dm_bcast_real    ( LAITBL  , NLUS )
      CALL wrf_dm_bcast_real    ( MAXALB  , NLUS )
      CALL wrf_dm_bcast_real    ( TOPT_DATA    , 1 )
      CALL wrf_dm_bcast_real    ( CMCMAX_DATA  , 1 )
      CALL wrf_dm_bcast_real    ( CFACTR_DATA  , 1 )
      CALL wrf_dm_bcast_real    ( RSMAX_DATA  , 1 )
      CALL wrf_dm_bcast_integer ( BARE    , 1 )




      IF ( wrf_dm_on_monitor() ) THEN
        OPEN(19, FILE='SOILPARM.TBL',FORM='FORMATTED',STATUS='OLD',IOSTAT=ierr)
        IF(ierr .NE. OPEN_OK ) THEN
          WRITE(message,FMT='(A)') &
          'module_sf_ruclsm.F: soil_veg_gen_parm: failure opening SOILPARM.TBL'
          CALL wrf_error_fatal3("module_sf_ruclsm.b",4720,&
message )
        END IF

        WRITE(mess,*) 'INPUT SOIL TEXTURE CLASSIFICAION = ',MMINSL
        CALL wrf_message( mess )

        LUMATCH=0

        READ (19,*)
        READ (19,2000,END=2003)SLTYPE
        READ (19,*)SLCATS,IINDEX
        IF(SLTYPE.NE.MMINSL)THEN
          DO LC=1,SLCATS
              READ (19,*) IINDEX,BB(LC),DRYSMC(LC),HC(LC),MAXSMC(LC),&
                        REFSMC(LC),SATPSI(LC),SATDK(LC), SATDW(LC),   &
                        WLTSMC(LC), QTZ(LC)
          ENDDO
        ENDIF
        READ (19,*)
        READ (19,2000,END=2003)SLTYPE
        READ (19,*)SLCATS,IINDEX
 
        IF(SLTYPE.EQ.MMINSL)THEN
            WRITE( mess , * ) 'SOIL TEXTURE CLASSIFICATION = ',SLTYPE,' FOUND', &
                  SLCATS,' CATEGORIES'
            CALL wrf_message ( mess )
          LUMATCH=1
        ENDIF
            IF(SLTYPE.EQ.MMINSL)THEN
          DO LC=1,SLCATS
              READ (19,*) IINDEX,BB(LC),DRYSMC(LC),HC(LC),MAXSMC(LC),&
                        REFSMC(LC),SATPSI(LC),SATDK(LC), SATDW(LC),   &
                        WLTSMC(LC), QTZ(LC)
          ENDDO
           ENDIF

 2003   CONTINUE

        CLOSE (19)
      ENDIF

      CALL wrf_dm_bcast_integer ( LUMATCH , 1 )
      CALL wrf_dm_bcast_string  ( SLTYPE  , 8 )
      CALL wrf_dm_bcast_string  ( MMINSL  , 8 )  
      CALL wrf_dm_bcast_integer ( SLCATS  , 1 )
      CALL wrf_dm_bcast_integer ( IINDEX  , 1 )
      CALL wrf_dm_bcast_real    ( BB      , NSLTYPE )
      CALL wrf_dm_bcast_real    ( DRYSMC  , NSLTYPE )
      CALL wrf_dm_bcast_real    ( HC      , NSLTYPE )
      CALL wrf_dm_bcast_real    ( MAXSMC  , NSLTYPE )
      CALL wrf_dm_bcast_real    ( REFSMC  , NSLTYPE )
      CALL wrf_dm_bcast_real    ( SATPSI  , NSLTYPE )
      CALL wrf_dm_bcast_real    ( SATDK   , NSLTYPE )
      CALL wrf_dm_bcast_real    ( SATDW   , NSLTYPE )
      CALL wrf_dm_bcast_real    ( WLTSMC  , NSLTYPE )
      CALL wrf_dm_bcast_real    ( QTZ     , NSLTYPE )

      IF(LUMATCH.EQ.0)THEN
          CALL wrf_message( 'SOIl TEXTURE IN INPUT FILE DOES NOT ' )
          CALL wrf_message( 'MATCH SOILPARM TABLE'                 )
          CALL wrf_error_fatal3("module_sf_ruclsm.b",4781,&
'INCONSISTENT OR MISSING SOILPARM FILE' )
      ENDIF




      IF ( wrf_dm_on_monitor() ) THEN
        OPEN(19, FILE='GENPARM.TBL',FORM='FORMATTED',STATUS='OLD',IOSTAT=ierr)
        IF(ierr .NE. OPEN_OK ) THEN
          WRITE(message,FMT='(A)') &
          'module_sf_ruclsm.F: soil_veg_gen_parm: failure opening GENPARM.TBL'
          CALL wrf_error_fatal3("module_sf_ruclsm.b",4793,&
message )
        END IF

        READ (19,*)
        READ (19,*)
        READ (19,*) NUM_SLOPE

          SLPCATS=NUM_SLOPE

          DO LC=1,SLPCATS
              READ (19,*)SLOPE_DATA(LC)
          ENDDO

          READ (19,*)
          READ (19,*)SBETA_DATA
          READ (19,*)
          READ (19,*)FXEXP_DATA
          READ (19,*)
          READ (19,*)CSOIL_DATA
          READ (19,*)
          READ (19,*)SALP_DATA
          READ (19,*)
          READ (19,*)REFDK_DATA
          READ (19,*)
          READ (19,*)REFKDT_DATA
          READ (19,*)
          READ (19,*)FRZK_DATA
          READ (19,*)
          READ (19,*)ZBOT_DATA
          READ (19,*)
          READ (19,*)CZIL_DATA
          READ (19,*)
          READ (19,*)SMLOW_DATA
          READ (19,*)
          READ (19,*)SMHIGH_DATA
        CLOSE (19)
      ENDIF

      CALL wrf_dm_bcast_integer ( NUM_SLOPE    ,  1 )
      CALL wrf_dm_bcast_integer ( SLPCATS      ,  1 )
      CALL wrf_dm_bcast_real    ( SLOPE_DATA   ,  NSLOPE )
      CALL wrf_dm_bcast_real    ( SBETA_DATA   ,  1 )
      CALL wrf_dm_bcast_real    ( FXEXP_DATA   ,  1 )
      CALL wrf_dm_bcast_real    ( CSOIL_DATA   ,  1 )
      CALL wrf_dm_bcast_real    ( SALP_DATA    ,  1 )
      CALL wrf_dm_bcast_real    ( REFDK_DATA   ,  1 )
      CALL wrf_dm_bcast_real    ( REFKDT_DATA  ,  1 )
      CALL wrf_dm_bcast_real    ( FRZK_DATA    ,  1 )
      CALL wrf_dm_bcast_real    ( ZBOT_DATA    ,  1 )
      CALL wrf_dm_bcast_real    ( CZIL_DATA    ,  1 )
      CALL wrf_dm_bcast_real    ( SMLOW_DATA   ,  1 )
      CALL wrf_dm_bcast_real    ( SMHIGH_DATA  ,  1 )



      END SUBROUTINE RUCLSM_SOILVEGPARM



  SUBROUTINE SOILIN (ISLTYP, DQM, REF, PSIS, QMIN, BCLH )


























         integer,   parameter      ::      nsoilclas=19

         integer, intent ( in)  ::                          isltyp
         real,    intent ( out) ::               dqm,ref,qmin,psis

         REAL  LQMA(nsoilclas),LREF(nsoilclas),LBCL(nsoilclas),       &
               LPSI(nsoilclas),LQMI(nsoilclas)








     DATA LQMA /0.395, 0.410, 0.435, 0.485, 0.485, 0.451, 0.420,      &
                0.477, 0.476, 0.426, 0.492, 0.482, 0.451, 1.0,        &
                0.20,  0.435, 0.468, 0.200, 0.339/


        DATA LREF /0.174, 0.179, 0.249, 0.369, 0.369, 0.314, 0.299,   &
                   0.357, 0.391, 0.316, 0.409, 0.400, 0.314, 1.,      &
                   0.1,   0.249, 0.454, 0.17,  0.236/


        DATA LQMI/0.045, 0.057, 0.065, 0.067, 0.034, 0.078, 0.10,     &
                  0.089, 0.095, 0.10,  0.070, 0.068, 0.078, 0.0,      &
                  0.004, 0.065, 0.020, 0.004, 0.008/


       DATA LPSI/0.121, 0.090, 0.218, 0.786, 0.786, 0.478, 0.299,     &
                 0.356, 0.630, 0.153, 0.490, 0.405, 0.478, 0.0,       &
                 0.121, 0.218, 0.468, 0.069, 0.069/


        DATA LBCL/4.05,  4.38,  4.90,  5.30,  5.30,  5.39,  7.12,      &
                  7.75,  8.52, 10.40, 10.40, 11.40,  5.39,  0.0,       &
                  4.05,  4.90, 11.55,  2.79,  2.79/


          DQM    = LQMA(ISLTYP)-                               &
                   LQMI(ISLTYP)
          REF    = LREF(ISLTYP)
          PSIS   = - LPSI(ISLTYP)
          QMIN   = LQMI(ISLTYP)
          BCLH   = LBCL(ISLTYP)

  END SUBROUTINE SOILIN

END MODULE module_sf_ruclsm
