MODULE module_sf_mynn
  USE module_model_constants, only: &
       &p1000mb, cp, xlv, ep_2
  USE module_sf_sfclay, ONLY: sfclayinit
  USE module_bl_mynn, only: tv0, mym_condensation
  IMPLICIT NONE
  REAL, PARAMETER :: xlvcp=xlv/cp, ep_3=1.-ep_2
  REAL, PARAMETER :: wmin=0.1
  REAL, PARAMETER :: zm2h=7.4
  REAL, PARAMETER :: charnock=0.016, bvisc=1.5e-5, z0hsea=5.e-5
  REAL, PARAMETER :: VCONVC=1.0
  REAL, PARAMETER :: SNOWZ0=0.012
  REAL, DIMENSION(0:1000 ),SAVE :: PSIMTB,PSIHTB
CONTAINS
  SUBROUTINE mynn_sf_init_driver(allowed_to_read)
    LOGICAL, INTENT(in) :: allowed_to_read
    CALL sfclayinit(allowed_to_read)
  END SUBROUTINE mynn_sf_init_driver
   SUBROUTINE SFCLAY_mynn(U3D,V3D,T3D,QV3D,P3D,dz8w, &
                     CP,G,ROVCP,R,XLV,PSFC,CHS,CHS2,CQS2,CPM, &
                     ZNT,UST,PBLH,MAVAIL,ZOL,MOL,REGIME,PSIM,PSIH, &
                     XLAND,HFX,QFX,LH,TSK,FLHC,FLQC,QGH,QSFC,RMOL, &
                     U10,V10,TH2,T2,Q2, &
                     GZ1OZ0,WSPD,BR,ISFFLX,DX, &
                     SVP1,SVP2,SVP3,SVPT0,EP1,EP2, &
                     KARMAN,EOMEG,STBOLT, &
                     itimestep,ch,th3d,pi3d,qc3d, &
                     tsq,qsq,cov,qcg, &
                     ids,ide, jds,jde, kds,kde, &
                     ims,ime, jms,jme, kms,kme, &
                     its,ite, jts,jte, kts,kte, &
                     ustm,ck,cka,cd,cda,isftcflx,iz0tlnd )
      IMPLICIT NONE
      INTEGER, INTENT(IN ) :: ids,ide, jds,jde, kds,kde, &
                                        ims,ime, jms,jme, kms,kme, &
                                        its,ite, jts,jte, kts,kte
      INTEGER, INTENT(IN ) :: ISFFLX
      REAL, INTENT(IN ) :: SVP1,SVP2,SVP3,SVPT0
      REAL, INTENT(IN ) :: EP1,EP2,KARMAN,EOMEG,STBOLT
      REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) , &
                INTENT(IN ) :: dz8w
      REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) , &
                INTENT(IN ) :: QV3D, &
                                                              P3D, &
                                                              T3D, &
                                                             QC3D,&
                                            th3d,pi3d,tsq,qsq,cov
      INTEGER, INTENT(in) :: itimestep
      REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) ::&
           & qcg
      REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) ::&
           & ch
      REAL, DIMENSION( ims:ime, jms:jme ) , &
                INTENT(IN ) :: MAVAIL, &
                                                             PBLH, &
                                                            XLAND, &
                                                              TSK
      REAL, DIMENSION( ims:ime, jms:jme ) , &
                INTENT(OUT ) :: U10, &
                                                              V10, &
                                                              TH2, &
                                                               T2, &
                                                               Q2
      REAL, DIMENSION( ims:ime, jms:jme ) , &
                INTENT(INOUT) :: REGIME, &
                                                              HFX, &
                                                              QFX, &
                                                               LH, &
                                                    MOL,RMOL,QSFC
      REAL, DIMENSION( ims:ime, jms:jme ) , &
                INTENT(INOUT) :: GZ1OZ0,WSPD,BR, &
                                                        PSIM,PSIH
      REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) , &
                INTENT(IN ) :: U3D, &
                                                              V3D
      REAL, DIMENSION( ims:ime, jms:jme ) , &
                INTENT(IN ) :: PSFC
      REAL, DIMENSION( ims:ime, jms:jme ) , &
                INTENT(INOUT) :: ZNT, &
                                                              ZOL, &
                                                              UST, &
                                                              CPM, &
                                                             CHS2, &
                                                             CQS2, &
                                                              CHS
      REAL, DIMENSION( ims:ime, jms:jme ) , &
                INTENT(INOUT) :: FLHC,FLQC
      REAL, DIMENSION( ims:ime, jms:jme ) , &
                INTENT(INOUT) :: QGH
      REAL, INTENT(IN ) :: CP,G,ROVCP,R,XLV,DX
      REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ) , &
                INTENT(OUT) :: ck,cka,cd,cda,ustm
      INTEGER, OPTIONAL, INTENT(IN ) :: ISFTCFLX, IZ0TLND
      REAL, DIMENSION( its:ite ) :: U1D, &
                                                              V1D, &
                                                             QV1D, &
                                                              P1D, &
                                                         T1D,qc1d
      REAL, DIMENSION( its:ite ) :: dz8w1d
      REAL, DIMENSION( its:ite ) :: vt1,vq1
      REAL, DIMENSION(kts:kts+1) :: thl, qw, vt, vq
      REAL :: ql
      INTEGER :: I,J,K
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
              UST(i,j)=MAX(0.025*SQRT(U1D(i)*U1D(i) + V1D(i)*V1D(i)),0.001)
              MOL(i,j)=0.
           ENDDO
        ELSE
           DO i=its,ite
              do k = kts,kts+1
                ql = qc3d(i,k,j)/(1.+qc3d(i,k,j))
                qw(k) = qv3d(i,k,j)/(1.+qv3d(i,k,j)) + ql
                thl(k) = th3d(i,k,j)-xlvcp*ql/pi3d(i,k,j)
              end do
              CALL mym_condensation (kts,kts+1, &
                   & dz8w(i,kts:kts+1,j), &
                   & thl(kts:kts+1), qw(kts:kts+1), &
                   & p3d(i,kts:kts+1,j),&
                   & pi3d(i,kts:kts+1,j), &
                   & tsq(i,kts:kts+1,j), &
                   & qsq(i,kts:kts+1,j), &
                   & cov(i,kts:kts+1,j), &
                   & vt(kts:kts+1), vq(kts:kts+1))
              vt1(i) = vt(kts)
              vq1(i) = vq(kts)
           ENDDO
        ENDIF
        CALL SFCLAY1D_mynn(J,U1D,V1D,T1D,QV1D,P1D,dz8w1d, &
                CP,G,ROVCP,R,XLV,PSFC(ims,j),CHS(ims,j),CHS2(ims,j),&
                CQS2(ims,j),CPM(ims,j),PBLH(ims,j), RMOL(ims,j), &
                ZNT(ims,j),UST(ims,j),MAVAIL(ims,j),ZOL(ims,j), &
                MOL(ims,j),REGIME(ims,j),PSIM(ims,j),PSIH(ims,j), &
                XLAND(ims,j),HFX(ims,j),QFX(ims,j),TSK(ims,j), &
                U10(ims,j),V10(ims,j),TH2(ims,j),T2(ims,j), &
                Q2(ims,j),FLHC(ims,j),FLQC(ims,j),QGH(ims,j), &
                QSFC(ims,j),LH(ims,j), &
                GZ1OZ0(ims,j),WSPD(ims,j),BR(ims,j),ISFFLX,DX, &
                SVP1,SVP2,SVP3,SVPT0,EP1,EP2,KARMAN,EOMEG,STBOLT, &
                ch(ims,j),vt1,vq1,qc1d,qcg(ims,j),&
                itimestep,&
                ids,ide, jds,jde, kds,kde, &
                ims,ime, jms,jme, kms,kme, &
                its,ite, jts,jte, kts,kte &
                ,isftcflx,iz0tlnd, &
                USTM(ims,j),CK(ims,j),CKA(ims,j), &
                CD(ims,j),CDA(ims,j) &
                                                                   )
      ENDDO
    END SUBROUTINE SFCLAY_MYNN
   SUBROUTINE SFCLAY1D_mynn(J,UX,VX,T1D,QV1D,P1D,dz8w1d, &
                     CP,G,ROVCP,R,XLV,PSFCPA,CHS,CHS2,CQS2,CPM,PBLH,RMOL, &
                     ZNT,UST,MAVAIL,ZOL,MOL,REGIME,PSIM,PSIH, &
                     XLAND,HFX,QFX,TSK, &
                     U10,V10,TH2,T2,Q2,FLHC,FLQC,QGH, &
                     QSFC,LH,GZ1OZ0,WSPD,BR,ISFFLX,DX, &
                     SVP1,SVP2,SVP3,SVPT0,EP1,EP2, &
                     KARMAN,EOMEG,STBOLT, &
                     ch,vt1,vq1,qc1d,qcg, &
                     itimestep, &
                     ids,ide, jds,jde, kds,kde, &
                     ims,ime, jms,jme, kms,kme, &
                     its,ite, jts,jte, kts,kte, &
                     isftcflx, iz0tlnd, &
                     ustm,ck,cka,cd,cda )
      IMPLICIT NONE
      REAL, PARAMETER :: XKA=2.4E-5
      REAL, PARAMETER :: PRT=1.
      INTEGER, INTENT(IN ) :: ids,ide, jds,jde, kds,kde, &
                                        ims,ime, jms,jme, kms,kme, &
                                        its,ite, jts,jte, kts,kte, &
                                        J
      INTEGER, INTENT(in) :: itimestep
      INTEGER, INTENT(IN ) :: ISFFLX
      REAL, INTENT(IN ) :: SVP1,SVP2,SVP3,SVPT0
      REAL, INTENT(IN ) :: EP1,EP2,KARMAN,EOMEG,STBOLT
      REAL, DIMENSION( ims:ime ) , &
                INTENT(IN ) :: MAVAIL, &
                                                             PBLH, &
                                                            XLAND, &
                                                              TSK
      REAL, DIMENSION( ims:ime ) , &
                INTENT(IN ) :: PSFCPA
      REAL, DIMENSION( ims:ime ) , &
                INTENT(INOUT) :: REGIME, &
                                                              HFX, &
                                                              QFX, &
                                                         MOL,RMOL
      REAL, DIMENSION( ims:ime ) , &
                INTENT(INOUT) :: GZ1OZ0,WSPD,BR, &
                                                        PSIM,PSIH
      REAL, DIMENSION( ims:ime ) , &
                INTENT(INOUT) :: ZNT, &
                                                              ZOL, &
                                                              UST, &
                                                              CPM, &
                                                             CHS2, &
                                                             CQS2, &
                                                              CHS
      REAL, DIMENSION( its:ite ) :: zratio,BRi,wstar,qstar,&
                                          resist,logres,Rreynolds, &
                                         niters,psixrat,psitrat
      REAL, DIMENSION( ims:ime ) , &
                INTENT(INOUT) :: FLHC,FLQC
      REAL, DIMENSION( ims:ime ) , &
                INTENT(INOUT) :: QGH,QSFC
      REAL, DIMENSION( ims:ime ) , &
                INTENT(OUT) :: U10,V10, &
                                                TH2,T2,Q2,LH
      REAL, INTENT(IN) :: CP,G,ROVCP,R,XLV,DX
      REAL, DIMENSION( its:ite ), INTENT(IN ) :: dz8w1d
      REAL, DIMENSION( its:ite ), INTENT(IN ) :: UX, &
                                                               VX, &
                                                             QV1D, &
                                                              P1D, &
                                                         T1D,qc1d
      REAL, DIMENSION( ims:ime ), INTENT(IN) :: qcg
      REAL, DIMENSION( ims:ime ), INTENT(INOUT) :: ch
      REAL, DIMENSION( its:ite ), INTENT(IN) :: vt1,vq1
      REAL, OPTIONAL, DIMENSION( ims:ime ) , &
                INTENT(OUT) :: ck,cka,cd,cda,ustm
      INTEGER, OPTIONAL, INTENT(IN ) :: ISFTCFLX, IZ0TLND
      REAL, DIMENSION( its:ite ) :: z_t,z_q
      REAL :: thl1,sqv1,sqc1,exner1,sqvg,sqcg,vv,ww
      REAL, DIMENSION( its:ite ) :: ZA, &
                                                        THVX,ZQKL, &
                                                           THX,QX, &
                                                            PSIH2, &
                                                            PSIM2, &
                                                           PSIH10, &
                                                           PSIM10, &
                                                           GZ2OZ0, &
                                                          GZ10OZ0, &
                                                            WSPDI
      REAL, DIMENSION( its:ite ) :: RHOX,GOVRTH
      REAL, DIMENSION( its:ite) :: SCR4
      REAL, DIMENSION( its:ite ) :: THGB, PSFC, QSFCMR
      REAL, DIMENSION( its:ite ) :: GZ2OZt,GZ10OZt,GZ1OZt
      INTEGER :: N,I,K,KK,L,NZOL,NK,NZOL2,NZOL10, ITER
      INTEGER, PARAMETER :: ITMAX=5
      REAL :: PL,THCON,TVCON,E1
      REAL :: ZL,TSKV,DTHVDZ,DTHVM,VCONV,RZOL,RZOL2,RZOL10,ZOL2,ZOL10
      REAL :: DTG,PSIX,DTTHX,DTHDZ,PSIX10,PSIT,PSIT2,PSIT10, &
                  PSIQ,PSIQ2,PSIQ10
      REAL :: FLUXC,VSGD
      real :: restar,VISC,psilim,DQG,OLDUST,OLDTST
      DO I=its,ite
         PSFC(I)=PSFCPA(I)/1000.
         THGB(I)=TSK(I)*(100./PSFC(I))**ROVCP
      ENDDO
      DO 30 I=its,ite
         PL=P1D(I)/1000.
         THCON=(100./PL)**ROVCP
         THX(I)=T1D(I)*THCON
         SCR4(I)=T1D(I)
         THVX(I)=THX(I)
         QX(I)=0.
   30 CONTINUE
      DO I=its,ite
         niters(I)=0.
         QGH(I)=0.
         CPM(I)=CP
         IF (itimestep .LE. 1) THEN
           qstar(I)=0.0
         ENDIF
      ENDDO
      DO 50 I=its,ite
         QX(I)=QV1D(I)/(1.+QV1D(I))
         TVCON=(1.+EP1*QX(I))
         THVX(I)=THX(I)*TVCON
         SCR4(I)=T1D(I)*TVCON
   50 CONTINUE
      DO 60 I=its,ite
        IF (TSK(I) .LT. 273.15) THEN
           E1=SVP1*EXP(4648*(1./273.15 - 1./TSK(I)) - &
                  11.64*LOG(273.15/TSK(I)) + 0.02265*(273.15 - TSK(I)))
        ELSE
           E1=SVP1*EXP(SVP2*(TSK(I)-SVPT0)/(TSK(I)-SVP3))
        ENDIF
        QSFC(I)=EP2*E1/(PSFC(I)-ep_3*E1)
        QSFCMR(I)=EP2*E1/(PSFC(I)-E1)
        IF (TSK(I) .LT. 273.15) THEN
           E1=SVP1*EXP(4648*(1./273.15 - 1./T1D(I)) - &
                  11.64*LOG(273.15/T1D(I)) + 0.02265*(273.15 - T1D(I)))
        ELSE
           E1=SVP1*EXP(SVP2*(T1D(I)-SVPT0)/(T1D(I)-SVP3))
        ENDIF
        PL=P1D(I)/1000.
        QGH(I)=EP2*E1/(PL-ep_3*E1)
        CPM(I)=CP*(1.+0.84*QX(I)/(1.-qx(i)))
   60 CONTINUE
   80 CONTINUE
      DO I=its,ite
         RHOX(I)=PSFC(I)*1000./(R*SCR4(I))
         ZQKL(I)=dz8w1d(I)
         ZA(I)=0.5*ZQKL(I)
         GOVRTH(I)=G/THX(I)
      ENDDO
      DO I=its,ite
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
         DTHDZ=(THX(I)-THGB(I))
         DTHVDZ= (vt1(i) + 1.0)*vv + (vq1(i) + tv0)*ww
         IF (xland(i).lt.1.5) then
            fluxc = max(hfx(i)/rhox(i)/cp &
                + ep1*tskv*qfx(i)/rhox(i),0.)
            WSTAR(I) = vconvc*(g/TSK(i)*pblh(i)*fluxc)**.33
         ELSE
            IF(-DTHVDZ.GE.0)THEN
               DTHVM=-DTHVDZ
            ELSE
               DTHVM=0.
            ENDIF
            fluxc = max(hfx(i)/rhox(i)/cp &
                 + ep1*tskv*qfx(i)/rhox(i),0.)
            WSTAR(I) = 1.25*(g/TSK(i)*pblh(i)*fluxc)**.33
         ENDIF
         VSGD = 0.32 * (max(dx/5000.-1.,0.))**.33
         WSPD(I)=SQRT(WSPD(I)*WSPD(I)+WSTAR(I)*WSTAR(I)+vsgd*vsgd)
         WSPD(I)=MAX(WSPD(I),wmin)
         BR(I)=GOVRTH(I)*ZA(I)*DTHVDZ/(WSPD(I)*WSPD(I))
         BR(I)=MAX(BR(I),-2.0)
         BR(I)=MIN(BR(I),1.0)
         BRi(I)=BR(I)
      ENDDO
 1006 format(A,F7.3,A,f9.4,A,f9.5,A,f9.4)
 1007 format(A,F2.0,A,f6.2,A,f7.3,A,f7.2)
DO I=its,ite
   ITER = 1
   DO WHILE (ITER .LE. ITMAX)
      niters(I)=ITER
      VISC=(1.32+0.009*(T1D(I)-273.15))*1.E-5
      IF((XLAND(I)-1.5).GE.0)THEN
          VISC=(1.32+0.009*(T1D(I)-273.15))*1.E-5
          IF ( PRESENT(ISFTCFLX) ) THEN
             IF ( ISFTCFLX .EQ. 0 ) THEN
                CALL charnock_1955(ZNT(i),UST(i),WSPD(i),visc)
             ELSEIF ( ISFTCFLX .EQ. 1 .OR. ISFTCFLX .EQ. 2 ) THEN
                CALL davis_etal_2008(ZNT(i),UST(i))
             ELSEIF ( ISFTCFLX .EQ. 3 ) THEN
                CALL Taylor_Yelland_2001(ZNT(i),UST(i),WSPD(i))
             ELSEIF ( ISFTCFLX .EQ. 4 ) THEN
                CALL charnock_1955(ZNT(i),UST(i),WSPD(i),visc)
             ENDIF
          ELSE
             CALL charnock_1955(ZNT(i),UST(i),WSPD(i),visc)
          ENDIF
          restar=MAX(ust(i)*ZNT(i)/visc, 0.1)
          IF ( PRESENT(ISFTCFLX) ) THEN
             IF ( ISFTCFLX .EQ. 0 ) THEN
                CALL fairall_2001(z_t(i),z_q(i),restar,UST(i),visc)
             ELSEIF ( ISFTCFLX .EQ. 1 ) THEN
                CALL fairall_2001(z_t(i),z_q(i),restar,UST(i),visc)
             ELSEIF ( ISFTCFLX .EQ. 2 ) THEN
                CALL garratt_1992(z_t(i),z_q(i),ZNT(i),restar,XLAND(I))
             ELSEIF ( ISFTCFLX .EQ. 3 ) THEN
                CALL fairall_2001(z_t(i),z_q(i),restar,UST(i),visc)
             ELSEIF ( ISFTCFLX .EQ. 4 ) THEN
                CALL zilitinkevich_1995(ZNT(i),z_t(i),z_q(i),restar,&
                                   UST(I),KARMAN,XLAND(I),IZ0TLND)
             ENDIF
          ELSE
             CALL fairall_2001(z_t(i),z_q(i),restar,UST(i),visc)
          ENDIF
       ELSE
          VISC=(1.32+0.009*(T1D(I)-273.15))*1.E-5
          restar=MAX(ust(i)*ZNT(i)/visc, 0.1)
          IF ( ZNT(i) .LE. SNOWZ0 .AND. TSK(I) .LE. 273.15 ) THEN
             CALL Andreas_2002(ZNT(i),restar,z_t(i),z_q(i))
          ELSE
             IF ( PRESENT(IZ0TLND) ) THEN
                IF ( IZ0TLND .LE. 1 ) THEN
                   CALL zilitinkevich_1995(ZNT(i),z_t(i),z_q(i),restar,&
                                  UST(I),KARMAN,XLAND(I),IZ0TLND)
                ELSEIF ( IZ0TLND .EQ. 2 ) THEN
                   CALL Yang_2008(ZNT(i),z_t(i),z_q(i),UST(i),MOL(I),&
                                  qstar(I),restar,visc,XLAND(I))
                ELSEIF ( IZ0TLND .EQ. 3 ) THEN
                   CALL garratt_1992(z_t(i),z_q(i),ZNT(i),restar,XLAND(I))
                ENDIF
             ELSE
                CALL zilitinkevich_1995(ZNT(i),z_t(i),z_q(i),restar,&
                                        UST(I),KARMAN,XLAND(I),0)
             ENDIF
          ENDIF
       ENDIF
       zratio(i)=znt(i)/z_t(i)
       Rreynolds(I)=restar
       GZ1OZ0(I)= LOG((ZA(I)+ZNT(I))/ZNT(I))
       GZ1OZt(I)= LOG((ZA(I)+z_t(i))/z_t(i))
       GZ2OZ0(I)= LOG((2.0+ZNT(I))/ZNT(I))
       GZ2OZt(I)= LOG((2.0+z_t(i))/z_t(i))
       GZ10OZ0(I)=LOG((10.+ZNT(I))/ZNT(I))
       GZ10OZt(I)=LOG((10.+z_t(i))/z_t(i))
     IF (BR(I) .GT. 0.2) THEN
        REGIME(I)=1.
        IF (ITER .EQ. 1 .AND. itimestep .LE. 1) THEN
           CALL Li_etal_2010(ZOL(I),BR(I),ZA(I)/ZNT(I),zratio(I))
        ELSE
           ZOL(I)=ZA(I)*KARMAN*9.81*MOL(I)/(THX(I)*MAX(UST(I),0.001)**2)
           ZOL(I)=MAX(ZOL(I),0.0)
           ZOL(I)=MIN(ZOL(I),20.0)
        ENDIF
        IF((XLAND(I)-1.5).GE.0)THEN
           CALL PSI_DyerHicks(PSIM(I),PSIH(I),ZOL(I), z_t(I), ZNT(I), ZA(I))
        ELSE
           CALL PSI_DyerHicks(PSIM(I),PSIH(I),ZOL(I), z_t(I), ZNT(I), ZA(I))
        ENDIF
        psilim = -10.
        PSIM(I)=MAX(PSIM(I),psilim)
        PSIH(I)=MAX(PSIH(I),psilim)
        PSIM10(I)=10./ZA(I)*PSIM(I)
        PSIM10(I)=MAX(PSIM10(I),psilim)
        PSIH10(I)=PSIM10(I)
        PSIM2(I)=2./ZA(I)*PSIM(I)
        PSIM2(I)=MAX(PSIM2(I),psilim)
        PSIH2(I)=PSIM2(I)
        RMOL(I) = ZOL(I)/ZA(I)
     ELSEIF(BR(I) .GT. 0. .AND. BR(I) .LE. 0.2) THEN
        REGIME(I)=2.
        IF (ITER .EQ. 1 .AND. itimestep .LE. 1) THEN
           CALL Li_etal_2010(ZOL(I),BR(I),ZA(I)/ZNT(I),zratio(I))
        ELSE
           ZOL(I)=ZA(I)*KARMAN*9.81*MOL(I)/(THX(I)*MAX(UST(I),0.001)**2)
           ZOL(I)=MAX(ZOL(I),0.0)
           ZOL(I)=MIN(ZOL(I),5.0)
        ENDIF
        IF((XLAND(I)-1.5).GE.0)THEN
           CALL PSI_DyerHicks(PSIM(I),PSIH(I),ZOL(I), z_t(I), ZNT(I), ZA(I))
        ELSE
           CALL PSI_DyerHicks(PSIM(I),PSIH(I),ZOL(I), z_t(I), ZNT(I), ZA(I))
        ENDIF
        psilim = -10.
        PSIM(I)=MAX(PSIM(I),psilim)
        PSIH(I)=MAX(PSIH(I),psilim)
        PSIM10(I)=10./ZA(I)*PSIM(I)
        PSIM10(I)=MAX(PSIM10(I),psilim)
        PSIH10(I)=PSIM10(I)
        PSIM2(I)=2./ZA(I)*PSIM(I)
        PSIM2(I)=MAX(PSIM2(I),psilim)
        PSIH2(I)=PSIM2(I)
        RMOL(I)= ZOL(I)/ZA(I)
     ELSEIF(BR(I) .EQ. 0.) THEN
        REGIME(I)=3.
        PSIM(I)=0.0
        PSIH(I)=PSIM(I)
        PSIM10(I)=0.
        PSIH10(I)=PSIM10(I)
        PSIM2(I)=0.
        PSIH2(I)=PSIM2(I)
        IF(UST(I) .LT. 0.01)THEN
          ZOL(I)=BR(I)*GZ1OZ0(I)
        ELSE
          ZOL(I)=KARMAN*GOVRTH(I)*ZA(I)*MOL(I)/(UST(I)*UST(I))
        ENDIF
        RMOL(I) = ZOL(I)/ZA(I)
     ELSEIF(BR(I) .LT. 0.)THEN
        REGIME(I)=4.
        IF (ITER .EQ. 1 .AND. itimestep .LE. 1) THEN
           CALL Li_etal_2010(ZOL(I),BR(I),ZA(I)/ZNT(I),zratio(I))
        ELSE
           ZOL(I)=ZA(I)*KARMAN*9.81*MOL(I)/(THX(I)*MAX(UST(I),0.001)**2)
           ZOL(I)=MAX(ZOL(I),-10.0)
           ZOL(I)=MIN(ZOL(I),0.0)
        ENDIF
        ZOL10=10./ZA(I)*ZOL(I)
        ZOL2=2./ZA(I)*ZOL(I)
        ZOL(I)=MIN(ZOL(I),0.)
        ZOL(I)=MAX(ZOL(I),-9.9999)
        ZOL10=MIN(ZOL10,0.)
        ZOL10=MAX(ZOL10,-9.9999)
        ZOL2=MIN(ZOL2,0.)
        ZOL2=MAX(ZOL2,-9.9999)
        NZOL=INT(-ZOL(I)*100.)
        RZOL=-ZOL(I)*100.-NZOL
        NZOL10=INT(-ZOL10*100.)
        RZOL10=-ZOL10*100.-NZOL10
        NZOL2=INT(-ZOL2*100.)
        RZOL2=-ZOL2*100.-NZOL2
        IF((XLAND(I)-1.5).GE.0)THEN
           CALL PSI_DyerHicks(PSIM(I),PSIH(I),ZOL(I), z_t(I), ZNT(I), ZA(I))
        ELSE
           CALL PSI_DyerHicks(PSIM(I),PSIH(I),ZOL(I), z_t(I), ZNT(I), ZA(I))
        ENDIF
        PSIM10(I)=PSIMTB(NZOL10)+RZOL10*(PSIMTB(NZOL10+1)-PSIMTB(NZOL10))
        PSIH10(I)=PSIHTB(NZOL10)+RZOL10*(PSIHTB(NZOL10+1)-PSIHTB(NZOL10))
        PSIM2(I)=PSIMTB(NZOL2)+RZOL2*(PSIMTB(NZOL2+1)-PSIMTB(NZOL2))
        PSIH2(I)=PSIHTB(NZOL2)+RZOL2*(PSIHTB(NZOL2+1)-PSIHTB(NZOL2))
        PSIH(I)=MIN(PSIH(I),0.9*GZ1OZ0(I))
        PSIM(I)=MIN(PSIM(I),0.9*GZ1OZ0(I))
        PSIH2(I)=MIN(PSIH2(I),0.9*GZ2OZ0(I))
        PSIM10(I)=MIN(PSIM10(I),0.9*GZ10OZ0(I))
        RMOL(I) = ZOL(I)/ZA(I)
     ENDIF
      GZ1OZ0(I) =LOG((ZA(I)+ZNT(I))/ZNT(I))
      GZ10OZ0(I)=LOG((10.+ZNT(I))/ZNT(I))
      PSIX=GZ1OZ0(I)-PSIM(I)
      PSIX10=GZ10OZ0(I)-PSIM10(I)
      OLDUST = UST(I)
      UST(I)=0.5*UST(I)+0.5*KARMAN*WSPD(I)/PSIX
      WSPDI(I)=SQRT(UX(I)*UX(I)+VX(I)*VX(I))
      IF ( PRESENT(USTM) ) THEN
         USTM(I)=0.5*USTM(I)+0.5*KARMAN*WSPDI(I)/PSIX
      ENDIF
      IF ((XLAND(I)-1.5).LT.0.) THEN
          UST(I)=MAX(UST(I),0.01)
          USTM(I)=UST(I)
      ENDIF
      GZ1OZt(I)= LOG((ZA(I)+z_t(i))/z_t(i))
      GZ2OZt(I)= LOG((2.0+z_t(i))/z_t(i))
      PSIT=MAX(LOG((ZA(I)+z_t(i))/z_t(i))-PSIH(I) ,2.0)
      PSIT2=MAX(LOG((2.0+z_t(i))/z_t(i))-PSIH2(I) ,2.0)
      resist(I)=PSIT
      logres(I)=GZ1OZt(I)
      PSIQ=MAX(LOG((za(i)+z_q(i))/z_q(I))-PSIH(I) ,2.0)
      PSIQ2=MAX(LOG((2.0+z_q(i))/z_q(I))-PSIH2(I) ,2.0)
      IF((XLAND(I)-1.5).GE.0)THEN
         ZL=ZNT(I)
      ELSE
         ZL=0.01
      ENDIF
      DTG=THX(I)-THGB(I)
      OLDTST=MOL(I)
      MOL(I)=KARMAN*DTG/PSIT/PRT
      DQG=(QX(i)-qsfc(i))*1000.
      qstar(I)=KARMAN*DQG/PSIQ/PRT
      if (ZA(i) .gt. 7.0 .and. ZA(i) .lt. 13.0) then
         U10(I)=UX(I)
         V10(I)=VX(I)
      else
         U10(I)=UX(I)*PSIX10/PSIX
         V10(I)=VX(I)*PSIX10/PSIX
      endif
      psixrat(I)=PSIX10/PSIX
      psitrat(I)=PSIT2/PSIT
      TH2(I)=THGB(I)+DTG*PSIT2/PSIT
      Q2(I)=QSFCMR(I)+(QV1D(I)-QSFCMR(I))*PSIQ2/PSIQ
      T2(I) = TH2(I)*(PSFC(I)/100.)**ROVCP
      IF (ITER .GE. 2) THEN
         IF (ABS(OLDTST-MOL(I)) .lt. 0.01) THEN
            ITER = ITMAX+1
         END IF
      ENDIF
      ITER = ITER + 1
   ENDDO
ENDDO
 1001 format(A,F2.0, A,f10.4,A,f5.3, A,f11.5)
 1002 format(A,f7.2, A,f7.2, A,f7.2, A,f10.3)
 1003 format(A,f7.2, A,f7.2, A,f10.3,A,f10.3)
 1004 format(A,f11.3,A,f9.7, A,f9.7, A,f6.2, A,f10.3)
 1005 format(A,f9.2,A,f6.4,A,f7.4,A,f7.4)
DO I=its,ite
   IF (ISFFLX .LT. 1) THEN
       QFX(i) = 0.
       HFX(i) = 0.
       FLHC(I) = 0.
       FLQC(I) = 0.
       LH(I) = 0.
       CHS(I) = 0.
       CH(I) = 0.
       CHS2(i) = 0.
       CQS2(i) = 0.
       IF(PRESENT(ck) .and. PRESENT(cd) .and. PRESENT(cka) .and. PRESENT(cda)) THEN
           Ck(I) = 0.
           Cd(I) = 0.
           Cka(I)= 0.
           Cda(I)= 0.
       ENDIF
   ELSE
      PSIX=GZ1OZ0(I)-PSIM(I)
      PSIX10=GZ10OZ0(I)-PSIM10(I)
      PSIT=MAX(LOG((ZA(I)+z_t(i))/z_t(i))-PSIH(I) ,2.0)
      PSIT2=MAX(LOG((2.0+z_t(i))/z_t(i))-PSIH2(I) ,2.0)
      PSIT10=MAX(LOG((10.0+z_t(i))/z_t(i))-PSIH10(I) ,2.0)
      PSIQ=MAX(LOG((za(i)+z_q(i))/z_q(I))-PSIH(I) ,2.0)
      PSIQ2=MAX(LOG((2.0+z_q(i))/z_q(I))-PSIH2(I) ,2.0)
      PSIQ10=MAX(LOG((10.0+z_q(i))/z_q(I))-PSIH10(I) ,2.0)
      IF((XLAND(I)-1.5).GE.0)THEN
         ZL=ZNT(I)
      ELSE
         ZL=0.01
      ENDIF
      FLQC(I)=RHOX(I)*MAVAIL(I)*UST(I)*KARMAN/PSIQ
      DTTHX=ABS(THX(I)-THGB(I))
      IF(DTTHX.GT.1.E-5)THEN
         FLHC(I)=CPM(I)*RHOX(I)*UST(I)*MOL(I)/(THX(I)-THGB(I))
      ELSE
         FLHC(I)=0.
      ENDIF
      QFX(I)=FLQC(I)*(QSFCMR(I)-QV1D(I))
      QFX(I)=MAX(QFX(I),-0.02)
      LH(I)=XLV*QFX(I)
      IF(XLAND(I)-1.5.GT.0.)THEN
         HFX(I)=FLHC(I)*(THGB(I)-THX(I))
         IF ( PRESENT(ISFTCFLX) ) THEN
            IF ( ISFTCFLX.NE.0 ) THEN
               HFX(I)=HFX(I)+RHOX(I)*USTM(I)*USTM(I)*WSPDI(I)
            ENDIF
         ENDIF
      ELSEIF(XLAND(I)-1.5.LT.0.)THEN
         HFX(I)=FLHC(I)*(THGB(I)-THX(I))
         HFX(I)=MAX(HFX(I),-250.)
      ENDIF
      CHS(I)=UST(I)*KARMAN/PSIT
      ch(i)=flhc(i)/( cpm(i)*rhox(i) )
      CQS2(I)=UST(I)*KARMAN/PSIQ2
      CHS2(I)=UST(I)*KARMAN/PSIT2
      IF(PRESENT(ck) .and. PRESENT(cd) .and. PRESENT(cka) .and. PRESENT(cda)) THEN
         Ck(I)=(karman/psix10)*(karman/psiq10)
         Cd(I)=(karman/psix10)*(karman/psix10)
         Cka(I)=(karman/psix)*(karman/psiq)
         Cda(I)=(karman/psix)*(karman/psix)
      ENDIF
   ENDIF
ENDDO
END SUBROUTINE SFCLAY1D_mynn
   SUBROUTINE zilitinkevich_1995(Z_0,Zt,Zq,restar,ustar,KARMAN,landsea,IZ0TLND)
       IMPLICIT NONE
       REAL, INTENT(IN) :: Z_0,restar,ustar,KARMAN,landsea
       INTEGER, OPTIONAL, INTENT(IN):: IZ0TLND
       REAL, INTENT(OUT) :: Zt,Zq
       REAL :: CZIL
       IF (landsea-1.5 .GT. 0) THEN
          IF (restar .LT. 0.1) THEN
             Zt = Z_0*EXP(KARMAN*2.0)
             Zt = MIN( Zt, 6.0e-5)
             Zt = MAX( Zt, 2.0e-9)
             Zq = Z_0*EXP(KARMAN*3.0)
             Zq = MIN( Zq, 6.0e-5)
             Zq = MAX( Zq, 2.0e-9)
          ELSE
             Zt = Z_0*EXP(-KARMAN*(4.0*SQRT(restar)-3.2))
             Zt = MIN( Zt, 6.0e-5)
             Zt = MAX( Zt, 2.0e-9)
             Zq = Z_0*EXP(-KARMAN*(4.0*SQRT(restar)-4.2))
             Zq = MIN( Zt, 6.0e-5)
             Zq = MAX( Zt, 2.0e-9)
          ENDIF
       ELSE
          IF ( IZ0TLND .EQ. 1 ) THEN
             CZIL = 10.0 ** ( -0.40 * ( Z_0 / 0.07 ) )
          ELSE
             CZIL = 0.10
          END IF
          Zt = Z_0*EXP(-KARMAN*CZIL*SQRT(restar))
          Zt = MIN( Zt, Z_0)
          Zq = Z_0*EXP(-KARMAN*CZIL*SQRT(restar))
          Zq = MIN( Zq, Z_0)
       ENDIF
       return
   END SUBROUTINE zilitinkevich_1995
   SUBROUTINE davis_etal_2008(Z_0,ustar)
       IMPLICIT NONE
       REAL, INTENT(IN) :: ustar
       REAL, INTENT(OUT) :: Z_0
       REAL :: ZW, ZN1, ZN2
       REAL, PARAMETER :: G=9.81, OZO=1.59E-5
       ZW = MIN((ustar/1.06)**(0.3),1.0)
       ZN1 = 0.011*ustar*ustar/G + OZO
       ZN2 = 10.*exp(-9.5*ustar**(-.3333)) + &
             0.11*1.5E-5/AMAX1(ustar,0.01)
       Z_0 = (1.0-ZW) * ZN1 + ZW * ZN2
       Z_0 = MAX( Z_0, 1.27e-7)
       Z_0 = MIN( Z_0, 2.85e-3)
       return
   END SUBROUTINE davis_etal_2008
   SUBROUTINE Taylor_Yelland_2001(Z_0,ustar,wsp10)
       IMPLICIT NONE
       REAL, INTENT(IN) :: ustar,wsp10
       REAL, INTENT(OUT) :: Z_0
       REAL, parameter :: g=9.81, pi=3.14159265
       REAL :: hs, Tp, Lp
        hs = 0.0248*(wsp10**2.)
        Tp = 0.729*MAX(wsp10,0.1)
        Lp = g*Tp**2/(2*pi)
       Z_0 = 1200.*hs*(hs/Lp)**4.5
       Z_0 = MAX( Z_0, 1.27e-7)
       Z_0 = MIN( Z_0, 2.85e-3)
       return
   END SUBROUTINE Taylor_Yelland_2001
   SUBROUTINE charnock_1955(Z_0,ustar,wsp10,visc)
       IMPLICIT NONE
       REAL, INTENT(IN) :: ustar, visc, wsp10
       REAL, INTENT(OUT) :: Z_0
       REAL, PARAMETER :: G=9.81, CZO2=0.011
       REAL :: CZC
       CZC = CZO2 + 0.007*MIN(MAX((wsp10-10.)/8., 0.), 1.0)
       Z_0 = CZC*ustar*ustar/G + (0.11*visc/MAX(ustar,0.1))
       Z_0 = MAX( Z_0, 1.27e-7)
       Z_0 = MIN( Z_0, 2.85e-3)
       return
   END SUBROUTINE charnock_1955
   SUBROUTINE garratt_1992(Zt,Zq,Z_0,Ren,landsea)
       IMPLICIT NONE
       REAL, INTENT(IN) :: Ren, Z_0,landsea
       REAL, INTENT(OUT) :: Zt,Zq
       REAL :: Rq
       REAL, PARAMETER :: e=2.71828183
       IF (landsea-1.5 .GT. 0) THEN
          Zt = Z_0*EXP(2.0 - (2.48*(Ren**0.25)))
          Zq = Z_0*EXP(2.0 - (2.28*(Ren**0.25)))
          Zq = MIN( Zq, 5.5e-5)
          Zq = MAX( Zq, 2.0e-9)
          Zt = MIN( Zt, 5.5e-5)
          Zt = MAX( Zt, 2.0e-9)
       ELSE
          Zq = Z_0/(e**2.)
          Zt = Zq
       ENDIF
       return
    END SUBROUTINE garratt_1992
    SUBROUTINE fairall_2001(Zt,Zq,Ren,ustar,visc)
       IMPLICIT NONE
       REAL, INTENT(IN) :: Ren,ustar,visc
       REAL, INTENT(OUT) :: Zt,Zq
       IF (Ren .le. 2.) then
          Zt = (5.5e-5)*(Ren**(-0.60))
          Zq = 0.2*visc/MAX(ustar,0.1)
       ELSE
          Zt = (5.5e-5)*(Ren**(-0.60))
          Zq = Zt
       ENDIF
       Zt = MIN(Zt,1.0e-4)
       Zt = MAX(Zt,2.0e-9)
       Zq = MIN(Zt,1.0e-4)
       Zq = MAX(Zt,2.0e-9)
       return
    END SUBROUTINE fairall_2001
    SUBROUTINE Yang_2008(Z_0,Zt,Zq,ustar,tstar,qst,Ren,visc,landsea)
       IMPLICIT NONE
       REAL, INTENT(IN) :: Z_0, Ren, ustar, tstar, qst, visc, landsea
       REAL :: ht, tstar2
       REAL, INTENT(OUT) :: Zt,Zq
       REAL, PARAMETER :: Renc=350., beta=0.5, e=2.71828183
       ht = Renc*visc/MAX(ustar,0.01)
       tstar2 = MIN(tstar, 0.0)
       Zt = ht * EXP(-beta*(ustar**0.5)*(ABS(tstar2)**1.0))
       Zq = Zt
       Zt = MIN(Zt, Z_0/2.0)
       Zq = MIN(Zq, Z_0/2.0)
       return
    END SUBROUTINE Yang_2008
    SUBROUTINE Andreas_2002(Z_0,Ren,Zt,Zq)
       IMPLICIT NONE
       REAL, INTENT(IN) :: Z_0, Ren
       REAL, INTENT(OUT) :: Zt, Zq
       REAL :: Ren2
       REAL, PARAMETER :: bt0_s=1.25, bt0_t=0.149, bt0_r=0.317, &
                           bt1_s=0.0, bt1_t=-0.55, bt1_r=-0.565, &
                           bt2_s=0.0, bt2_t=0.0, bt2_r=-0.183
       REAL, PARAMETER :: bq0_s=1.61, bq0_t=0.351, bq0_r=0.396, &
                           bq1_s=0.0, bq1_t=-0.628, bq1_r=-0.512, &
                           bq2_s=0.0, bq2_t=0.0, bq2_r=-0.180
       Ren2 = Ren
       IF (Ren2 .gt. 1000.) Ren2 = 1000.
       IF (Ren2 .le. 0.135) then
          Zt = Z_0*EXP(bt0_s + bt1_s*LOG(Ren2) + bt2_s*LOG(Ren2)**2)
          Zq = Z_0*EXP(bq0_s + bq1_s*LOG(Ren2) + bq2_s*LOG(Ren2)**2)
       ELSE IF (Ren2 .gt. 0.135 .AND. Ren2 .lt. 2.5) then
          Zt = Z_0*EXP(bt0_t + bt1_t*LOG(Ren2) + bt2_t*LOG(Ren2)**2)
          Zq = Z_0*EXP(bq0_t + bq1_t*LOG(Ren2) + bq2_t*LOG(Ren2)**2)
       ELSE
          Zt = Z_0*EXP(bt0_r + bt1_r*LOG(Ren2) + bt2_r*LOG(Ren2)**2)
          Zq = Z_0*EXP(bq0_r + bq1_r*LOG(Ren2) + bq2_r*LOG(Ren2)**2)
       ENDIF
       return
    END SUBROUTINE Andreas_2002
    SUBROUTINE PSI_Hogstrom_1996(psi_m, psi_h, zL, Zt, Z_0, Za)
       IMPLICIT NONE
       REAL, INTENT(IN) :: zL, Zt, Z_0, Za
       REAL, INTENT(OUT) :: psi_m, psi_h
       REAL :: x, x0, y, y0, zmL, zhL
       zmL = Z_0*zL/Za
       zhL = Zt*zL/Za
       IF (zL .gt. 0.) THEN
          psi_m = -5.3*(zL - zmL)
          psi_h = -8.0*(zL - zhL)
       ELSE
          x = (1.-19.0*zL)**0.25
          x0= (1.-19.0*zmL)**0.25
          y = (1.-11.6*zL)**0.5
          y0= (1.-11.6*zhL)**0.5
          psi_m = 2.*LOG((1.+x)/(1.+x0)) + LOG((1.+x**2.)/(1.+x0**2.)) - &
                  2.*ATAN(x) + 2*ATAN(x0)
          psi_h = 2.*LOG((1.+y)/(1.+y0))
       ENDIF
       return
    END SUBROUTINE PSI_Hogstrom_1996
    SUBROUTINE PSI_DyerHicks(psi_m, psi_h, zL, Zt, Z_0, Za)
       IMPLICIT NONE
       REAL, INTENT(IN) :: zL, Zt, Z_0, Za
       REAL, INTENT(OUT) :: psi_m, psi_h
       REAL :: x, x0, y, y0, zmL, zhL
       zmL = Z_0*zL/Za
       zhL = Zt*zL/Za
       IF (zL .gt. 0.) THEN
          psi_m = -5.0*(zL - zmL)
          psi_h = -5.0*(zL - zhL)
       ELSE
          x = (1.-16.*zL)**0.25
          x0= (1.-16.*zmL)**0.25
          y = (1.-16.*zL)**0.5
          y0= (1.-16.*zhL)**0.5
          psi_m = 2.*LOG((1.+x)/(1.+x0)) + LOG((1.+x**2.)/(1.+x0**2.)) - &
                  2.*ATAN(x) + 2*ATAN(x0)
          psi_h = 2.*LOG((1.+y)/(1.+y0))
       ENDIF
       return
    END SUBROUTINE PSI_DyerHicks
    SUBROUTINE PSI_Beljaars_Holtslag_1991(psi_m, psi_h, zL)
       IMPLICIT NONE
       REAL, INTENT(IN) :: zL
       REAL, INTENT(OUT) :: psi_m, psi_h
       REAL, PARAMETER :: a=1., b=0.666, c=5., d=0.35
       IF (zL .lt. 0.) THEN
          WRITE(*,*)"WARNING: Universal stability functions from"
          WRITE(*,*)"        Beljaars and Holtslag (1991) should only"
          WRITE(*,*)"        be used in the stable regime!"
          psi_m = 0.
          psi_h = 0.
       ELSE
          psi_m = -(a*zL + b*(zL -(c/d))*exp(-d*zL) + (b*c/d))
          psi_h = -((1.+.666*a*zL)**1.5 + &
                  b*(zL - (c/d))*exp(-d*zL) + (b*c/d) -1.)
       ENDIF
       return
    END SUBROUTINE PSI_Beljaars_Holtslag_1991
    SUBROUTINE PSI_Zilitinkevich_Esau_2007(psi_m, psi_h, zL)
       IMPLICIT NONE
       REAL, INTENT(IN) :: zL
       REAL, INTENT(OUT) :: psi_m, psi_h
       REAL, PARAMETER :: Cm=3.0, Ct=2.5
       IF (zL .lt. 0.) THEN
          WRITE(*,*)"WARNING: Universal stability function from"
          WRITE(*,*)"        Zilitinkevich and Esau (2007) should only"
          WRITE(*,*)"        be used in the stable regime!"
          psi_m = 0.
          psi_h = 0.
       ELSE
          psi_m = -Cm*(zL**(5./6.))
          psi_h = -Ct*(zL**(4./5.))
       ENDIF
       return
    END SUBROUTINE PSI_Zilitinkevich_Esau_2007
    SUBROUTINE PSI_Businger_1971(psi_m, psi_h, zL)
       IMPLICIT NONE
       REAL, INTENT(IN) :: zL
       REAL, INTENT(OUT) :: psi_m, psi_h
       REAL :: x, y
       REAL, PARAMETER :: Pi180 = 3.14159265/180.
       IF (zL .lt. 0.) THEN
          x = (1. - 15.0*zL)**0.25
          y = (1. - 9.0*zL)**0.5
          psi_m = LOG(((1.+x)/2.)**2.) + LOG((1.+x**2.)/2.) - &
                           2.*ATAN(x) + Pi180*90.
          psi_h = 2.*LOG((1.+y)/2.)
       ELSE
          psi_m = -4.7*zL
          psi_h = -(4.7/0.74)*zL
       ENDIF
       return
    END SUBROUTINE PSI_Businger_1971
    SUBROUTINE PSI_Suselj_Sood_2010(psi_m, psi_h, zL)
       IMPLICIT NONE
       REAL, INTENT(IN) :: zL
       REAL, INTENT(OUT) :: psi_m, psi_h
       REAL, PARAMETER :: Rfc=0.19, Ric=0.183, PHIT=0.8
       IF (zL .gt. 0.) THEN
          psi_m = -(zL/Rfc + 1.1223*EXP(1.-1.6666/zL))
          psi_h = -(zL*Ric/((Rfc**2.)*5.) + 7.09*(zL**1.1091))
       ELSE
          psi_m = 0.9904*LOG(1. - 14.264*zL)
          psi_h = 1.0103*LOG(1. - 16.3066*zL)
       ENDIF
       return
    END SUBROUTINE PSI_Suselj_Sood_2010
    SUBROUTINE Li_etal_2010(zL, Rib, zaz0, z0zt)
       IMPLICIT NONE
       REAL, INTENT(OUT) :: zL
       REAL, INTENT(IN) :: Rib, zaz0, z0zt
       REAL :: alfa, beta, zaz02, z0zt2
       REAL, PARAMETER :: au11=0.045, bu11=0.003, bu12=0.0059, &
                           bu21=-0.0828, bu22=0.8845, bu31=0.1739, &
                           bu32=-0.9213, bu33=-0.1057
       REAL, PARAMETER :: aw11=0.5738, aw12=-0.4399, aw21=-4.901,&
                           aw22=52.50, bw11=-0.0539, bw12=1.540, &
                           bw21=-0.669, bw22=-3.282
       REAL, PARAMETER :: as11=0.7529, as21=14.94, bs11=0.1569,&
                           bs21=-0.3091, bs22=-1.303
       zaz02=zaz0
       IF (zaz0 .lt. 100.0) zaz02=100.
       IF (zaz0 .gt. 100000.0) zaz02=100000.
       z0zt2=z0zt
       IF (z0zt .lt. 0.5) z0zt2=0.5
       IF (z0zt .gt. 100.0) z0zt2=100.
       alfa = LOG(zaz02)
       beta = LOG(z0zt2)
       IF (Rib .le. 0.0) THEN
          zL = au11*alfa*Rib**2 + ( &
                  (bu11*beta + bu12)*alfa**2 + &
                  (bu21*beta + bu22)*alfa + &
                  (bu31*beta**2 + bu32*beta + bu33))*Rib
          zL = MAX(zL,-15.)
          zL = MIN(zL,0.)
       ELSEIF (Rib .gt. 0.0 .AND. Rib .le. 0.2) THEN
          zL = ((aw11*beta + aw12)*alfa + &
                (aw21*beta + aw22))*Rib**2 + &
               ((bw11*beta + bw12)*alfa + &
                (bw21*beta + bw22))*Rib
          zL = MIN(zL,4.)
          zL = MAX(zL,0.)
       ELSE
          zL = (as11*alfa + as21)*Rib + bs11*alfa + &
                bs21*beta + bs22
          zL = MIN(zL,20.)
          zL = MAX(zL,1.)
       ENDIF
       return
    END SUBROUTINE Li_etal_2010
END MODULE module_sf_mynn
