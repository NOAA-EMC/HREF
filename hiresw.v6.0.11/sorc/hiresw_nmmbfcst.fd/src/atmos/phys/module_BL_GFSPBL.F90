!----------------------------------------------------------------------
!
      MODULE MODULE_BL_GFSPBL
!
!-----------------------------------------------------------------------
!
!***  THE GFS PBL SCHEME
!
!     0910-2010    Created by Weiguo Wang to use GFS PBL 
!-----------------------------------------------------------------------
!
      USE MODULE_INCLUDE
!
       USE MODULE_CONSTANTS,ONLY : cp99 => CP,ELWV,                &
     &                            g99 =>G, rd99 => r_d,            &
     &                            ep199 => ep_1, CAPPA, PQ0,ELIV,  &
                                  AA2 =>A2, AA3=>A3, AA4=>A4, P608
       use machine     , only : kind_phys

!
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      PRIVATE
!
      PUBLIC :: GFSPBL
!
!-----------------------------------------------------------------------
!
      REAL,PARAMETER :: VKARMAN=0.4
      REAL,PARAMETER :: XLV=ELWV, RLIVWV=ELIV/XLV
!-----------------------------------------------------------------------
!
      CONTAINS
!
!-----------------------------------------------------------------------
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!-----------------------------------------------------------------------
!
! REFERENCES:  moved from GFS 
!
! ABSTRACT:
!
!-----------------------------------------------------------------------
        SUBROUTINE GFSPBL(DT,NPHS,DP,AIRDEN                              &
     &                    ,RIB                                            &
     &                    ,PMID,PINT,T, ZINT                              &
     &                    ,NUM_WATER,WATER                                &
     &                    ,P_QV,P_QC,P_QR,P_QI,P_QS,P_QG                  &
     &                    ,U,V                                            &
     &                    ,USTAR                                          &
     &                    ,SHEAT, LHEAT                                   &
                          ,XLAND                                          &
     &                    ,AKHS,AKMS                                      &
                          ,THZ0,QZ0                                       &
                          ,QSFC                                           &
                          ,TSK,SNOW,SICE,CHKLOWQ                          &
                          ,factrs,rswtt,rlwtt                             &
     &                    ,PBLH,PBLK                                      &     !! out below
     &                    ,MIXHT                                          &
     &                    ,RUBLTEN                                        &
     &                    ,RVBLTEN                                        &
     &                    ,RTHBLTEN                                       &
     &                    ,RQVBLTEN                                       &
     &                    ,RQCBLTEN                                       &
     &                    ,RQRBLTEN                                       &
     &                    ,RQIBLTEN                                       &
     &                    ,RQSBLTEN                                       &
     &                    ,RQGBLTEN                                       &
     &                   ,IDS,IDE,JDS,JDE,KDS,KDE                        &
     &                   ,IMS,IME,JMS,JME,KMS,KME                        &
     &                   ,ITS,ITE,JTS,JTE,KTS,KTE)

!----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!----------------------------------------------------------------------
       REAL,PARAMETER :: A2S=17.2693882,A3S=273.16,A4S=35.86
       REAL,PARAMETER :: SEAFC=0.98,PQ0SEA=PQ0*SEAFC

      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
!
      INTEGER,INTENT(IN) :: NPHS, NUM_WATER
      INTEGER,INTENT(IN) :: P_QV,P_QC,P_QR,P_QI,P_QS,P_QG
!
!
      REAL,INTENT(IN) :: DT
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: SHEAT, LHEAT,RIB,USTAR ,XLAND
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: AKHS,AKMS,factrs
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: SICE, SNOW, TSK ,CHKLOWQ
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: QZ0, THZ0
!
      REAL,DIMENSION(IMS:IME,JMS:JME,1:KTE),INTENT(IN)::  RSWTT, RLWTT

      REAL,DIMENSION(IMS:IME,JMS:JME,1:KTE),INTENT(IN) :: DP,PMID,AIRDEN
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(IN) :: PINT,ZINT
      REAL,DIMENSION(IMS:IME,JMS:JME,1:KTE),INTENT(IN) :: U,V,T

      REAL,DIMENSION(IMS:IME,JMS:JME,1:KTE,NUM_WATER),INTENT(IN)::   &
     &                                                  WATER             !in z, (1:LM)
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: MIXHT,PBLH,QSFC
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: PBLK
!
!
      REAL,DIMENSION(IMS:IME,JMS:JME,1:KTE)                            &
     &    ,INTENT(OUT) ::                                              &
     &                                         RQCBLTEN                &
     &                                        ,RUBLTEN,RVBLTEN         &
     &                                        ,RTHBLTEN,RQVBLTEN       &
     &                                        ,RQRBLTEN,RQIBLTEN       &
     &                                        ,RQSBLTEN,RQGBLTEN       

!
!
!
!----------------------------------------------------------------------
!***
!***  LOCAL VARIABLES
!***
      INTEGER :: I,J,K,LM,KFLIP,K1,nvdiff,ntcw, levs , ipr
      INTEGER, DIMENSION(1) :: kpbl, kinver
      REAL(kind=kind_phys) :: dtp, surface,xkzm_m,xkzm_h,xkzm_s
      REAL(kind=kind_phys), DIMENSION(1,KTE-1) :: dkt
      REAL(kind=kind_phys), DIMENSION(1,KTE) :: dvdt,dudt,dtdt,ugrs,vgrs,tgrs,swh,  &
                                   hlw,del,prsl,prslk,phil
      REAL(kind=kind_phys), DIMENSION(1,KTE+1) :: prsi,phii,prsik
      REAL(kind=kind_phys), DIMENSION(1)       :: xmu, psk,rb,ffmm,ffhh,tsea,qss,hflx,& 
                                   evap,stress,wind, &
                                   dusfc1,dvsfc1,dtsfc1,dqsfc1,hpbl,gamt,gamq,pii
      REAL(kind=kind_phys), DIMENSION(1,KTE,NUM_WATER-1) :: dqdt ,qgrs
       real a96, a97, temp1, plow, tz0,seamask,qz0ss
      REAL :: QKLOW, CWMKLOW,RHOKLOW,QFC1,EXNSFC, PSFC, THSK, zmid1
      LOGICAL :: lpr, lprnt
!----------------------------------------------------------------------
    !  lpr=.true.  !.false.
      lpr=.false.  !.false.
      ipr=0
      lprnt=.false.
    !  dtp=DT*float(NPHS)
      dtp=DT*float(NPHS)/2.0

      LM = KTE
      levs = LM
      nvdiff = NUM_WATER -1     !! p_qv = 2, 
      ntcw  = P_QC-1    !-- Mix only cloud water; mixing of other species may not be robust
   !
      kinver(1) = levs          !! temp
      xkzm_m = 3.0
     !! xkzm_h = 1.0
      xkzm_h = 0.05  ! 0.1  !0.0  !1.0 !0.1  !0.2 !#0.5
      xkzm_s = 0.2              !! background diffusivity, see compns_physics.f in gfs/phys
!--------------------------------------------------------------
!.......................................................................
!$omp parallel do                &
!$omp     private(k,j,i)
!.......................................................................
      DO K=KMS,KME
      DO J=JMS,JME
      DO I=IMS,IME
      RQCBLTEN(I,J,K) = 0.0
      RQVBLTEN(I,J,K) = 0.0
      RQRBLTEN(I,J,K) = 0.0
      RQIBLTEN(I,J,K) = 0.0
      RQSBLTEN(I,J,K) = 0.0
      RQGBLTEN(I,J,K) = 0.0
      RTHBLTEN(I,J,K) = 0.0
      RUBLTEN(I,J,K) = 0.0
      RVBLTEN(I,J,K) = 0.0
      ENDDO
      ENDDO
      ENDDO
!.......................................................................
!$omp end parallel do
!.......................................................................

!.......................................................................
!$omp parallel do                &
!$omp     private(j,i)
!.......................................................................
      DO J=JMS,JME
      DO I=IMS,IME
       PBLH(I,J) = 0.9
      ENDDO
      ENDDO
!.......................................................................
!$omp end parallel do
!.......................................................................

!!         write(0,*)'inside GFSBL',water(35,17,lm-5:lm,p_qv)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! THIS PART FOLLOWS MYJPBL TO UPDATE QSFC AND QZ0
!!!  NOTE: THIS PART IS SUPPOSED TO BE DONE CORRECTLY IN JSFC.F90, BUT IT IS NOT. THIS IS 
!!!  WHY WE UPDATE HERE. BE CAREFUL ABOUT DIFFERENCES BETWEEN SPECIFIC HUMIDITY AND 
!!!  MIXING RATIO.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   
       if(lpr) write(0,*)'old qsfc,qz0,thz0,tsk',qsfc(35,17),qz0(35,17),thz0(35,17),tsk(35,17)         
!--------------------------------------------------------------
!.......................................................................
!$omp parallel do                &
!$omp     private(j,i,k,qklow,cwmklow,rhoklow,thsk,seamask,qfc1,psfc,exnsfc)
!.......................................................................
      DO J=JTS,JTE
      DO I=ITS,ITE
           K=LM
           QKLOW=WATER(I,J,K,P_QV)/(1.0+ WATER(I,J,K,P_QV))
           CWMKLOW=WATER(I,J,K,P_QC)+WATER(I,J,K,P_QR)+WATER(I,J,K,P_QI)+ &
                   WATER(I,J,K,P_QS)+WATER(I,J,K,P_QG)
           RHOKLOW=PMID(I,J,K)/(RD99*T(I,J,K)*(1.+P608*QKLOW-CWMKLOW))
           THSK=TSK(I,J)*(1.E5/PINT(I,J,LM+1))**CAPPA

  !
  !***  COUNTING DOWNWARD FROM THE TOP, THE EXCHANGE COEFFICIENTS AKH
  !***  ARE DEFINED ON THE BOTTOMS OF THE LAYERS KTS TO KTE-1.  THESE COEFFICIENTS
  !***  ARE ALSO MULTIPLIED BY THE DENSITY AT THE BOTTOM INTERFACE LEVEL.
  !
  !
  !
                SEAMASK=XLAND(I,J)-1.
                THZ0(I,J)=(1.-SEAMASK)*THSK+SEAMASK*THZ0(I,J)
  !
                IF(SEAMASK<0.5)THEN
                  QFC1=XLV*CHKLOWQ(I,J)*AKHS(I,J)*RHOKLOW
  !
                  IF(SNOW(I,J)>0..OR.SICE(I,J)>0.5)THEN
                    QFC1=QFC1*RLIVWV
                  ENDIF
  !
                  IF(QFC1>0.)THEN
                    QSFC(I,J)=QKLOW+LHEAT(I,J)/QFC1
                  ENDIF
  !
                ELSE
                  PSFC=PINT(I,J,KTE+1)
                  EXNSFC=(1.E5/PSFC)**CAPPA
  
                 QSFC(I,J)=PQ0SEA/PSFC                                      &
          &         *EXP(AA2*(THSK-AA3*EXNSFC)/(THSK-AA4*EXNSFC))
               ENDIF
 !
               QZ0 (I,J)=(1.-SEAMASK)*QSFC(I,J)+SEAMASK*QZ0 (I,J)
      ENDDO
      ENDDO
!.......................................................................
!$omp end parallel do
!.......................................................................
      if(lpr) write(0,*)'new qsfc,qz0,thz0',qsfc(35,17),qz0(35,17),thz0(35,17)         
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!END OF UPDATING QSFC, QZ0 !!!!!!!!!!!!!!!!!!!!!!!!!

!--------------------------------------------------------------
!.......................................................................
!$omp parallel do                                                                     &
!$omp     private(j,i,k,dvdt,dudt,dtdt,k1,dqdt,kflip,prsi,prsik,phii,ugrs,vgrs,       &
!$omp            tgrs,qgrs,del,prsl,prslk,swh,hlw,zmid1,phil,xmu,surface,seamask,plow,&
!$omp             tz0,hflx,evap,rb,ffmm,ffhh,tsea,qss,wind,stress,kpbl,dusfc1,dvsfc1, &
!$omp             dqsfc1,hpbl,gamt,gamq,dkt,pii)
!.......................................................................
      DO J=JTS,JTE
      DO I=ITS,ITE

           DO K=1,LM  
            dvdt(1,K) = 0.0
            dudt(1,K) = 0.0
            dtdt(1,K) = 0.0
             DO k1=1,nvdiff 
              dqdt(1,K,K1) = 0.0
             ENDDO
           ENDDO
         
           DO K=1,LM+1
              KFLIP = LM+1+1-K
              prsi(1,K)  = PINT(I,J,KFLIP)   !! pa
              prsik(1,K) = (prsi(1,K)*1.e-5)**CAPPA
              phii(1,K)  = ZINT(I,J,KFLIP)*G99
           ENDDO
          DO K=1,LM
             KFLIP = LM+1-K
              ugrs(1,K) = U(I,J,KFLIP)
              vgrs(1,K) = V(I,J,KFLIP)
              tgrs(1,K) = T(I,J,KFLIP)
     !! mixing ratio to specific humidity
              qgrs(1,K,1) = WATER(I,J,KFLIP,P_QV)/(1.0+WATER(I,J,KFLIP,P_QV))
              DO k1=2,nvdiff
                qgrs(1,K,K1) = WATER(I,J,KFLIP,K1+1)
              ENDDO 

        !       if (i == 35 .and. j == 17) then 
        !         write(0,*)'qgrs(1:5)=',qgrs(1,1:5,1)
        !         write(0,*)'water = ', water(i,j,lm-5:lm,p_qv)
        !       endif

              del(1,K)  = DP(I,J,KFLIP)     !! pa
              prsl(1,K) = PMID(I,J,KFLIP)  !! pa
              prslk(1,K)= (prsl(1,K)*1.0e-5)**CAPPA
             !! phil(1,K) = G99*0.5*(ZINT(I,J,KFLIP)+ZINT(I,J,KFLIP+1))
             !! phil(1,K) = 0.5*(phii(1,K)+phii(1,K+1)) 
              swh(1,K) = RSWTT(I,J,KFLIP)    !!0.0  
              hlw(1,K) = RLWTT(I,J,KFLIP)    !!0.0 
                 zmid1=zint(i,j,kflip+1)+pmid(i,j,kflip)/airden(i,j,kflip)/g99 &
                          *alog(pint(i,j,kflip+1)/pmid(i,j,kflip))
              !!write(0,*)'K=',phil(1,k)/g99
                 phil(1,K)=zmid1*G99 
              !!write(0,*)'K=,new',phil(1,k)/g99
           ENDDO

              xmu(1) = factrs(I,J) 

              surface=phii(1,1)
              phil(1,:)=phil(1,:)-surface  !!phii(1,1)  !! surface=0
              phii(1,:)=phii(1,:)-surface  !!phii(1,1)

           seamask = xland(i,j) - 1.0
           plow    = pint(i,j,LM+1)
           tz0     = thz0(i,j)*(plow*1.0e-05)**CAPPA
           hflx(1) = SHEAT(I,J)/AIRDEN(I,J,LM)/CP99            ! W/m2 to K m/s
           evap(1) = LHEAT(I,J)/AIRDEN(I,J,LM)/XLV
           

           rb(1) = max(RIB(I,J),-5000.0)
           ffmm(1) =USTAR(I,J)*VKARMAN/AKMS(I,J)
           ffhh(1) =USTAR(I,J)*VKARMAN/AKHS(I,J)
           !  ffmm(1) = alog(phil(1,1)/9.8/0.05)
           !  ffhh(1) = ffmm(1)
           tsea(1) = 0.0    ! not in use
           qss(1)  = 0.0    ! not in use
           WIND(1)   = SQRT(ugrs(1,1)*ugrs(1,1)+vgrs(1,1)*vgrs(1,1))
           WIND(1)   = max(WIND(1),1.0d0)
           stress(1) = USTAR(I,J)*USTAR(I,J)
           KPBL(1)  = 1.0
           dusfc1(1) = 0.0
           dvsfc1(1) = 0.0
           dtsfc1(1) = 0.0
           dqsfc1(1) = 0.0
           hpbl(1)   = phil(1,1)/9.8 !! 0.0
           gamt(1)   = 0.0
           gamq(1)   = 0.0
           dkt(1,:)    = 0.0 
           pii(1)    = 1.0 
          call moninq(1,1,levs,nvdiff,ntcw,dvdt,dudt,dtdt,dqdt,         &
     &     ugrs,vgrs,tgrs,qgrs,swh,hlw,xmu,                             &
  !!   &     prsik(1,1),rb,ffmm,ffhh,tsea,qss,hflx,evap,stress,wind,kpbl, &
     &     pii,rb,ffmm,ffhh,tsea,qss,hflx,evap,stress,wind,kpbl, &
     &     prsi,del,prsl,prslk,phii,phil,dtp,                           &
     &     dusfc1,dvsfc1,dtsfc1,dqsfc1,hpbl,gamt,gamq,dkt,              &
     &     kinver,xkzm_m,xkzm_h,xkzm_s                                  &    
     &    ,lprnt,ipr)
!! AFTER CALLING, flip Z , then back to NEMS variables                 
          
           DO K=1,LM
             KFLIP = LM+1-K
             RUBLTEN(I,J,K)  = dudt(1,KFLIP) 
             RVBLTEN(I,J,K)  = dvdt(1,KFLIP)                      
             RTHBLTEN(I,J,K) = dtdt(1,KFLIP)/prslk(1,KFLIP)  !! /EXNER(I,J,K)
        !!     RTHBLTEN(I,J,K) = dtdt(1,KFLIP)*prsik(1,1)/prslk(1,KFLIP)  !! /EXNER(I,J,K)
        !!     IF(P_QV .GT. 1) RQVBLTEN(I,J,K) = dqdt(1,KFLIP,P_QV-1)/(1.0-qgrs(1,kflip,1))  ! to mixing ratio
             IF(P_QV .GT. 1) RQVBLTEN(I,J,K) = dqdt(1,KFLIP,P_QV-1)/(1.0-qgrs(1,kflip,1))**2  ! to mixing ratio
             IF(P_QC .GT. 1) RQCBLTEN(I,J,K) = dqdt(1,KFLIP,P_QC-1)
             IF(P_QR .GT. 1) RQRBLTEN(I,J,K) = dqdt(1,KFLIP,P_QR-1)
             IF(P_QI .GT. 1) RQIBLTEN(I,J,K) = dqdt(1,KFLIP,P_QI-1)
             IF(P_QS .GT. 1) RQSBLTEN(I,J,K) = dqdt(1,KFLIP,P_QS-1)
             IF(P_QG .GT. 1) RQGBLTEN(I,J,K) = dqdt(1,KFLIP,P_QG-1)
          ENDDO

             PBLH(I,J)  = hpbl(1)
             PBLK(I,J)  = LM+1-kpbl(1)
             MIXHT(I,J)  = hpbl(1)
 
      ENDDO
      ENDDO
!.......................................................................
!$omp end parallel do
!.......................................................................

         IF (lpr ) Then
      write(0,*)'max pblh=',maxval(pblh), minval(pblh)
      write(0,*)'max T=',maxval(T(its:ite,jts:jte,1:lm)), minval(T(its:ite,jts:jte,1:lm))
      write(0,*)'max u=',maxval(u), minval(u)
      write(0,*)'max v=',maxval(v), minval(v)
      write(0,*)'max water=',maxval(water), minval(water)
      write(0,*)'max dudt,dvdt=',maxval(abs(dudt)),maxval(abs(dvdt))
      write(0,*)'max dqdt,dqdt=',maxval(dqdt),minval(dqdt)
      write(0,*)'max dqvdt,dqvdt=',maxval(dqdt(1,:,1)),minval(dqdt(1,:,1))
      write(0,*)'max qv=',maxval(water(:,:,:,p_qv))
      write(0,*)'min qv=',minval(water(:,:,:,p_qv)),'its,ite,jts,jte=',its,ite,jts,jte
      write(0,*)'max lheat=',maxval(abs(lheat(its:ite,jts:jte))),maxloc(abs(lheat(its:ite,jts:jte))) 
      write(0,*)'max sheat=',maxval(abs(sheat(its:ite,jts:jte))),maxloc(abs(sheat(its:ite,jts:jte))) 
        ENDIF
!----------------------------------------------------------------------
      END SUBROUTINE GFSPBL
!----------------------------------------------------------------------
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      END MODULE MODULE_BL_GFSPBL
!
!-----------------------------------------------------------------------
