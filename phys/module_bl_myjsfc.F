!
MODULE MODULE_BL_MYJSFC
USE module_model_constants
!
!----------------------------------------------------------------------
!
! REFERENCES:  Janjic (1996), 11th Conf. on NWP, Norfolk, VA
!              Janjic (1994), Mon. Wea. Rev.
!              Mellor and Yamada (1982), Rev. Geophys. Space Phys.
!              Mellor and Yamada (1974), J. Atmos. Sci.
!
! ABSTRACT:
!     MYJ updates the turbulent kinetic energy with the production/
!     dissipation term and the vertical diffusion term
!     (using an implicit formulation) from Mellor-Yamada
!     Level 2.5 as extended by Janjic.  Exchange coefficients for
!     the surface and for all layer interfaces are then computed from
!     Mellor-Yamada Level 2.
!     The turbulent vertical exchange is then executed.
!
!----------------------------------------------------------------------
!
      INTEGER :: KTMQ2=1 ! Iteration count for vertical diffusion 
      INTEGER :: ITRMX=5 ! Iteration count for mixing length computation
!
!     REAL,PARAMETER :: G=9.81,PI=3.1415926,R_D=287.,R_V=461.6         &
!                      ,VKARMAN=0.4
      REAL,PARAMETER :: PI=3.1415926        &
                       ,VKARMAN=0.4
!     REAL,PARAMETER :: CP=7.*R_D/2.
      REAL,PARAMETER :: CAPA=R_D/CP
      REAL,PARAMETER :: EPS1=1.E-12,EPS2=0.
!      REAL,PARAMETER :: EPSL=0.32,          EPSRU=1.E-12,EPSRS=1.E-12  &
      REAL,PARAMETER :: EPSL=0.10,          EPSRU=1.E-12,EPSRS=1.E-12  &
                       ,EPSTRB=1.E-24
      REAL,PARAMETER :: EPSA=1.E-8,EPSIT=1.E-4,EPSU2=1.E-4,EPSUST=0.07 
      REAL,PARAMETER :: ALPH=0.20,BETA=1./270.,EL0MAX=1000.,ELFC=0.23  &
                       ,FH=1.01,GAM1=0.2222222222222222222,PRT=1.
      REAL,PARAMETER :: A1=0.659888514560862645                        &
                       ,A2a=0.6574209922667784586                       &
                       ,B1=11.87799326209552761                        &
                       ,B2=7.226971804046074028                        &
                       ,C1=0.000830955950095854396
      REAL,PARAMETER :: A2S=17.2693882,A3S=273.16,A4S=35.86
      REAL,PARAMETER :: SEAFC=0.98,PQ0SEA=PQ0*SEAFC
      REAL,PARAMETER :: CZIL=.20,ELZ0=0.,ESQ=0.2,EXCM=0.001            &
                       ,FHNEU=0.8,GLKBR=10.,GLKBS=30.                  &
                       ,QVISC=2.1E-5,RFC=0.191,RIC=0.183,SMALL=0.35    &
                       ,SQPR=0.84,SQSC=0.84,SQVISC=258.2,TVISC=2.1E-5  &
                       ,USTC=0.7,USTR=0.225,VISC=1.5E-5                &
                       ,WOLD=0.15,WWST=1.2,ZTMAX=1.,ZTFC=1.,ZTMIN=-5.
!
      REAL,PARAMETER :: BTG=BETA*G,CZIV=SMALL*GLKBS                    &
!                      ,EP_1=R_V/R_D-1.,GRRS=GLKBR/GLKBS               &
                       ,GRRS=GLKBR/GLKBS               &
                       ,RB1=1./B1,RTVISC=1./TVISC,RVISC=1./VISC        &
                       ,ZQRZT=SQSC/SQPR
!
      REAL,PARAMETER :: ADNH= 9.*A1*A2a*A2a*(12.*A1+3.*B2)*BTG*BTG       &                  
                       ,ADNM=18.*A1*A1*A2a*(B2-3.*A2a)*BTG               & 
                       ,ANMH=-9.*A1*A2a*A2a*BTG*BTG                      &
                       ,ANMM=-3.*A1*A2a*(3.*A2a+3.*B2*C1+18.*A1*C1-B2)   &
                                      *BTG                             &   
                       ,BDNH= 3.*A2a*(7.*A1+B2)*BTG                     &
                       ,BDNM= 6.*A1*A1                                 &
                       ,BEQH= A2a*B1*BTG+3.*A2a*(7.*A1+B2)*BTG           &
                       ,BEQM=-A1*B1*(1.-3.*C1)+6.*A1*A1                &
                       ,BNMH=-A2a*BTG                                   &     
                       ,BNMM=A1*(1.-3.*C1)                             &
                       ,BSHH=9.*A1*A2a*A2a*BTG                           &
                       ,BSHM=18.*A1*A1*A2a*C1                           &
                       ,BSMH=-3.*A1*A2a*(3.*A2a+3.*B2*C1+12.*A1*C1-B2)   &
                                      *BTG                             &
                       ,CESH=A2a                                        &
                       ,CESM=A1*(1.-3.*C1)                             &
                       ,CNV=EP_1*G/BTG                                 &
                       ,ELFCS=VKARMAN*BTG                              &
                       ,FZQ1=RTVISC*QVISC*ZQRZT                        &
                       ,FZQ2=RTVISC*QVISC*ZQRZT                        &
                       ,FZT1=RVISC *TVISC*SQPR                         &
                       ,FZT2=CZIV*GRRS*TVISC*SQPR                      &
                       ,FZU1=CZIV*VISC                                 &
                       ,PIHF=0.5*PI                                    &
                       ,RFAC=RIC/(FHNEU*RFC*RFC)                       &
                       ,RQVISC=1./QVISC                                &
                       ,RRIC=1./RIC                                    &
                       ,USTFC=0.018/G                                  &
                       ,WNEW=1.-WOLD                                   &
                       ,WWST2=WWST*WWST                                &
                       ,ZILFC=-CZIL*VKARMAN*SQVISC
CONTAINS
!
!----------------------------------------------------------------------
      SUBROUTINE MYJSFC(HT,DZ                                          &
                       ,PMID,PINT,TH,T,QV,U,V,RHO                      &
                       ,TSK,QSFC,THZ0,QZ0,UZ0,VZ0                      &
                       ,LOWLYR,XLAND                                   &
                       ,USTAR,ZNT,PBLH,MAVAIL                          &
                       ,AKHS,AKMS                                      &
                       ,CHS,CHS2,HFX,QFX,FLHC,FLQC,QGH,CPM             &
                       ,U10,V10,TH2,T2,Q2,TSHLTR,TH10,QSHLTR,Q10       &
                       ,IDS,IDE,JDS,JDE,KDS,KDE                        &
                       ,IMS,IME,JMS,JME,KMS,KME                        &
                       ,ITS,ITE,JTS,JTE,KTS,KTE)
!----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
                           ,IMS,IME,JMS,JME,KMS,KME                    &
                           ,ITS,ITE,JTS,JTE,KTS,KTE
!

      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: LOWLYR
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: HT,XLAND,PBLH,TSK,MAVAIL
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: DZ         &
                                                           ,PMID,PINT  &
                                                           ,QV,RHO     &
                                                           ,T,TH,U,V   
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: HFX,QFX           &
                                                    ,Q10,QSHLTR,Q2     &
                                                    ,TH10,TSHLTR,TH2,T2&
                                                    ,U10,V10
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: AKHS,AKMS,QSFC
!
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: QZ0,THZ0        &
                                                      ,USTAR,UZ0,VZ0   &
                                                      ,ZNT
!
      REAL,DIMENSION(ITS:ITE,JTS:JTE),INTENT(OUT) :: FLHC,FLQC         &
                                                    ,QGH,CPM           &
                                                    ,CHS,CHS2
!----------------------------------------------------------------------
!***
!***  LOCAL VARIABLES
!***
      INTEGER :: I,J,K,KFLIP,LMH
!
      INTEGER,DIMENSION(ITS:ITE,JTS:JTE) :: LPBL
!
      REAL :: DQDT,DTDIF,DTDT,DUDT,DVDT,PLOW,PSFC,PTOP         &
             ,RATIOMX,RWMSK,SEAMASK,ULOW,VLOW,WMSK
!
      REAL,DIMENSION(KTS:KTE) :: PK,Q2K,QK,THK,TK,UK,VK
!
      REAL,DIMENSION(KTS:KTE-1) :: AKHK,AKMK,EL,GH,GM
!
      REAL,DIMENSION(KTS:KTE+1) :: ZHK
!
      REAL,DIMENSION(ITS:ITE,JTS:JTE) :: CHKLOWQ,CT,THSK
!
      REAL,DIMENSION(ITS:ITE,KTS:KTE+1,JTS:JTE) :: ZINT
!
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
!
!***  MAKE PREPARATIONS
!
!----------------------------------------------------------------------
      DO J=JTS,JTE
      DO K=KTS,KTE+1
      DO I=ITS,ITE
        ZINT(I,K,J)=0.
      ENDDO
      ENDDO
      ENDDO
!
      DO J=JTS,JTE
      DO I=ITS,ITE
        ZINT(I,KTE+1,J)=HT(I,J)     ! Z at bottom of lowest sigma layer
!
!!!!!!!!!
!!!!!! UNCOMMENT THESE LINES IF USING ETA COORDINATES
!!!!!!!!!
!!!!!!  ZINT(I,KTE+1,J)=1.E-4       ! Z of bottom of lowest eta layer
!!!!!!  ZHK(KTE+1)=1.E-4            ! Z of bottom of lowest eta layer
!
      ENDDO
      ENDDO
!
      DO J=JTS,JTE
      DO K=KTE,KTS,-1
        KFLIP=KTE+1-K
        DO I=ITS,ITE
          ZINT(I,K,J)=ZINT(I,K+1,J)+DZ(I,KFLIP,J)
        ENDDO
      ENDDO
      ENDDO
!
!----------------------------------------------------------------------

	write(0,*) 'diagnose over JTS,JTE: ', JTS,JTE
	write(0,*) 'diagnose over ITS,ITE: ', ITS,ITE

        setup_integration:  DO J=JTS,JTE
!----------------------------------------------------------------------
!
        DO I=ITS,ITE
!
!***  LOWEST LAYER ABOVE GROUND MUST BE FLIPPED
!
          LMH=KTE-LOWLYR(I,J)+1
!
          PTOP=PINT(I,KTE+1,J)      ! KTE+1=KME
          PSFC=PINT(I,LOWLYR(I,J),J)
! Define THSK here 
          THSK(I,J)=TSK(I,J)/(PSFC*1.E-5)**CAPA
!
!***  CONVERT LAND MASK (1 FOR SEA; 0 FOR LAND)
!
          SEAMASK=XLAND(I,J)-1.
!
!***  FILL 1-D VERTICAL ARRAYS
!***  AND FLIP DIRECTION SINCE MYJ SCHEME
!***  COUNTS DOWNWARD FROM THE DOMAIN'S TOP
!
          DO K=KTE,KTS,-1
            KFLIP=KTE+1-K
            THK(K)=TH(I,KFLIP,J)
            TK(K)=T(I,KFLIP,J)
            RATIOMX=QV(I,KFLIP,J)
            QK(K)=RATIOMX/(1.+RATIOMX)
            PK(K)=PMID(I,KFLIP,J)
!
!
!***  COMPUTE THE HEIGHTS OF THE LAYER INTERFACES
!
            ZHK(K)=ZINT(I,K,J)
!
          ENDDO
          ZHK(KTE+1)=HT(I,J)          ! Z at bottom of lowest sigma layer
!
          DO K=KTE,KTS,-1
            KFLIP=KTE+1-K
            UK(K)=U(I,KFLIP,J)
            VK(K)=V(I,KFLIP,J)
          ENDDO
!***
!***  FIND THE SURFACE EXCHANGE COEFFICIENTS
!***
          PLOW=PK(LMH)
          ULOW=UK(LMH)
          VLOW=VK(LMH)
!
          CALL SFCDIF(LMH,SEAMASK,THSK(I,J),TSK(I,J),QSFC(I,J)         &
                     ,UZ0(I,J),VZ0(I,J),THZ0(I,J),QZ0(I,J)             &
                     ,USTAR(I,J),ZNT(I,J),CT(I,J)                      &
                     ,AKMS(I,J),AKHS(I,J),PBLH(I,J),MAVAIL(I,J)        &
                     ,CHS(I,J),CHS2(I,J),HFX(I,J),QFX(I,J)             &
                     ,FLHC(I,J),FLQC(I,J),QGH(I,J),CPM(I,J)            &
                     ,PLOW,ULOW,VLOW,THK,QK,ZHK,PSFC                   &
                     ,U10(I,J),V10(I,J),TSHLTR(I,J),T2(I,J),TH10(I,J)  &
                     ,QSHLTR(I,J),Q10(I,J)                             &
                     ,IDS,IDE,JDS,JDE,KDS,KDE                          &
                     ,IMS,IME,JMS,JME,KMS,KME                          &
                     ,ITS,ITE,JTS,JTE,KTS,KTE)
!----------------------------------------------------------------------
! diagnostics for output
          TH2(I,J)=TSHLTR(I,J)
          Q2(I,J)=QSHLTR(I,J)/(1.-QSHLTR(I,J))
!
        ENDDO
!
!----------------------------------------------------------------------
      ENDDO setup_integration
!----------------------------------------------------------------------
!
      END SUBROUTINE MYJSFC
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!----------------------------------------------------------------------
      SUBROUTINE SFCDIF(LMH,SEAMASK,THS,TSK,QS,UZ0,VZ0,THZ0,QZ0        &
                       ,USTAR,Z0,CT,AKMS,AKHS,PBLH,WETM                &
                       ,CHS,CHS2,HFX,QFX,FLHC,FLQC,QGH,CPM             &
                       ,PLOW,ULOW,VLOW,TH,Q,Z,PSFC                     &
                       ,U10,V10,TH02,T02,TH10,Q02,Q10                  &
                       ,IDS,IDE,JDS,JDE,KDS,KDE                        &
                       ,IMS,IME,JMS,JME,KMS,KME                        &
                       ,ITS,ITE,JTS,JTE,KTS,KTE)
!     ****************************************************************
!     *                                                              *
!     *                       SURFACE LAYER                          *
!     *                                                              *
!     ****************************************************************
!----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
                           ,IMS,IME,JMS,JME,KMS,KME                    &
                           ,ITS,ITE,JTS,JTE,KTS,KTE
!
      INTEGER,INTENT(IN) :: LMH
!
      REAL,INTENT(IN) :: PBLH,PLOW,PSFC,SEAMASK,THS,ULOW,VLOW,TSK,WETM
!
      REAL,DIMENSION(KTS:KTE),INTENT(IN) :: Q,TH
      REAL,DIMENSION(KTS:KTE+1),INTENT(IN) :: Z
!
      REAL,INTENT(OUT) :: CHS,CHS2,HFX,QFX,FLHC,FLQC, &
                          QGH,CPM,CT,Q02,Q10,TH02,T02,TH10,U10,V10
      REAL,INTENT(INOUT) :: AKHS,AKMS,QZ0,THZ0,USTAR,UZ0,VZ0,Z0,QS
!----------------------------------------------------------------------
!***
!***  LOCAL VARIABLES
!***
      INTEGER :: ITR
!
      REAL :: ALPHA,BTGH,CXCH,DTHV,DU2,HLFLX,HSFLX,PSHZ,PSMZ,QLOW      &
             ,RDZ,RIB,RLMA,RLMN,RLMO,RLMP,RLOGT,RLOGU,RWGH,SIMH,SIMM   &
             ,THLOW,UMFLX,USTARK,VMFLX,WGHT,WSTAR2,X,XLT,XLT4,XLU,XLU4 &
             ,XT,XT4,XU,XU4,ZETALT,ZETALU,ZETAT,ZETAU,ZQ,ZSL,ZSLT,ZSLU &
             ,ZT,ZU
!
!***  DIAGNOSTICS
!
      REAL :: AKHS02,AKHS10,AKMS02,AKMS10,EKMS10,PSHLTR,QSAT10,QSAT2   &
             ,RLNT02,RLNT10,RLNU10,SIMH02,SIMH10,SIMM10,T10            &
             ,TERM1,TLOW,RLOW,U10E,V10E,XLT02,XLT024,XLT10,XLT104      &
             ,XLU10,XLU104,XU10,XU104,ZT02,ZT10,ZTAT02,ZTAT10          &
             ,ZTAU,ZTAU10,ZU10,ZUUZ
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
      THLOW=TH(LMH)
      QLOW=Q(LMH)
!   FOR FIRST TIMESTEP QS=0.
      IF(QS.LT.1.E-10)QS=QLOW
!
      Z0=(1.-SEAMASK)*Z0+SEAMASK*AMAX1(USTFC*USTAR*USTAR,1.59E-5)
!
!----------------------------------------------------------------------
!***  VISCOUS SUBLAYER
!----------------------------------------------------------------------
!
      IF(SEAMASK.GT.0.5.AND.USTAR.LT.USTC)THEN
!
!----------------------------------------------------------------------
        IF(USTAR.LT.USTR)THEN
          ZU=FZU1*SQRT(SQRT(Z0*USTAR*RVISC))/USTAR
          WGHT=AKMS*ZU*RVISC
          RWGH=WGHT/(WGHT+1.)
          UZ0=(ULOW*RWGH+UZ0)*0.5
          VZ0=(VLOW*RWGH+VZ0)*0.5
!
          ZT=FZT1*ZU
          WGHT=AKHS*ZT*RTVISC
!         THZ0=((WGHT*THLOW+THS)/(WGHT+1.)+THZ0)*0.5
          THZ0=(WGHT*THLOW+THS)/(WGHT+1.)
!
          ZQ=FZQ1*ZT
          WGHT=AKHS*ZQ*RQVISC
!         QZ0=((WGHT*QLOW+QS)/(WGHT+1.)+QZ0)*0.5
          QZ0=(WGHT*QLOW+QS)/(WGHT+1.)
        ENDIF
!
        IF(USTAR.GE.USTR.AND.USTAR.LT.USTC)THEN
          ZU=Z0
          UZ0=0.
          VZ0=0.
!
          ZT=FZT2*SQRT(SQRT(Z0*USTAR*RVISC))/USTAR
          WGHT=AKHS*ZT*RTVISC
!         THZ0=((WGHT*THLOW+THS)/(WGHT+1.)+THZ0)*0.5
          THZ0=(WGHT*THLOW+THS)/(WGHT+1.)
!
          ZQ=FZQ2*ZT
          WGHT=AKHS*ZQ*RQVISC
!         QZ0 =((WGHT*QLOW+QS)/(WGHT+1.)+QZ0)*0.5
          QZ0=(WGHT*QLOW+QS)/(WGHT+1.)
        ENDIF
!----------------------------------------------------------------------
      ELSE
!----------------------------------------------------------------------
!
        ZU=Z0
!
!----------------------------------------------------------------------
!***
!***  USE THE FOLLOWING IF THE ARRAY FOR EFFECTIVE SFC ROUGHNESS (ZEFF)
!***  IS AVAILABLE
!***
!       IF(SEAMASK.LE.0.5)THEN
!         IF(ULOW.EQ.0.) ULOW=EPSU2
!         ALPHA=ABS(ATAN(VLOW/ULOW)+PIHF-EPSA)
!         X=ALPHA/PIFR
!         ML=1+X
!         MH=1+MOD(ML,4)
!         WLOW=X-ML+1
!         ZU=WLOW*ZEFF(ML)+(1.-WLOW)*ZEFF(MH)
!       ENDIF
!----------------------------------------------------------------------
        UZ0=0.
        VZ0=0.
!
        ZT=Z0
        THZ0=THS
!
        ZQ=Z0
        QZ0=QS
!----------------------------------------------------------------------
      ENDIF
!----------------------------------------------------------------------
      ZSL=(Z(LMH)-Z(LMH+1))*0.5
!----------------------------------------------------------------------
!
!***  THE FOLLOWING LINE IS FOR USE WITH EFFECTIVE Z0 (ZEFF)
!
!     ZU=AMIN1(ZU,0.5*ZSL)
!----------------------------------------------------------------------
      RDZ=1./ZSL
      CXCH=EXCM*RDZ
!----------------------------------------------------------------------
      IF(SEAMASK.GT.0.5)THEN
        DTHV=(EP_1*QLOW+1.)*THLOW-(EP_1*QZ0+1.)*THZ0
      ELSE
        DTHV=(QLOW-QZ0)*CNV+THLOW-THZ0
        ZT=Z0*ZTFC
      ENDIF
!
      DU2=AMAX1((ULOW-UZ0)**2+(VLOW-VZ0)**2,EPSU2)
!----------------------------------------------------------------------
      RIB=BTG*DTHV*ZSL/DU2
!
!----------------------------------------------------------------------
!***  BELJAARS CORRECTION OF USTAR
!----------------------------------------------------------------------
!
      BTGH=BTG*PBLH
      WSTAR2=WWST2*ABS(BTGH*AKHS*DTHV)**(2./3.)
      USTAR=AMAX1(SQRT(AKMS*SQRT(DU2+WSTAR2)),EPSUST)
!
!----------------------------------------------------------------------
!***  ZILITINKEVITCH FIX FOR ZT
!----------------------------------------------------------------------
!
      IF(SEAMASK.LT.0.5)ZT=EXP(ZILFC*SQRT(USTAR*Z0))*Z0
!
!----------------------------------------------------------------------
      IF(SEAMASK.GT.0.5.AND.RIB.GE.RIC)THEN
!----------------------------------------------------------------------
        AKMS=AMAX1( VISC*RDZ,CXCH)
        AKHS=AMAX1(TVISC*RDZ,CXCH)
!----------------------------------------------------------------------
      ELSE
!----------------------------------------------------------------------
        ZSLU=ZSL+ZU
        ZSLT=ZSL+ZT
!
        RLOGU=ALOG(ZSLU/ZU)
        RLOGT=ALOG(ZSLT/ZT)
!
        RLMO=ELFCS*AKHS*DTHV/USTAR**3
!
!----------------------------------------------------------------------
!***  SEA POINTS FIRST
!----------------------------------------------------------------------
!
        IF(SEAMASK.GT.0.5)THEN
!
!----------------------------------------------------------------------
!***  1./MONIN-OBUKKHOV LENGTH-SCALE
!----------------------------------------------------------------------
!
          sea_iteration_loop: DO ITR=1,ITRMX
!
            ZETALT=AMAX1(ZSLT*RLMO,ZTMIN)
            RLMO=ZETALT/ZSLT
            ZETALU=ZSLU*RLMO
!
            ZETAU=ZU*RLMO
            ZETAT=ZT*RLMO
!
!----------------------------------------------------------------------
!***  LL FUNCTIONS OVER SEA
!----------------------------------------------------------------------
!
            IF(RLMO.LT.0.)THEN
              PSMZ=PSLMU(ZETAU)
              SIMM=       PSLMU(ZETALU)-PSMZ+RLOGU
              PSHZ=PSLHU(ZETAT)
              SIMH=FHNEU*(PSLHU(ZETALT)-PSHZ+RLOGT)
            ELSE
              PSMZ=PSLMS(ZETAU)
              SIMM=       PSLMS(ZETALU)-PSMZ+RLOGU
              PSHZ=PSLHS(ZETAT)
              SIMH=FHNEU*(PSLHS(ZETALT)-PSHZ+RLOGT)
            ENDIF
!
!----------------------------------------------------------------------
!***  BELJAARS CORRECTION FOR USTAR
!----------------------------------------------------------------------
!
            USTAR=AMAX1(SQRT(AKMS*SQRT(DU2+WSTAR2)),EPSUST)
!----------------------------------------------------------------------
            USTARK=USTAR*VKARMAN
            AKMS=AMAX1(USTARK/SIMM,CXCH)
            AKHS=AMAX1(USTARK/SIMH,CXCH)
!----------------------------------------------------------------------
            WSTAR2=WWST2*ABS(BTGH*AKHS*DTHV)**(2./3.)
            RLMN=ELFCS*AKHS*DTHV/USTAR**3
!----------------------------------------------------------------------
            RLMP=RLMO
            RLMA=RLMO*WOLD+RLMN*WNEW
!----------------------------------------------------------------------
!           IF(ABS((RLMN-RLMO)/RLMA).LT.EPSIT)GO TO 110
!----------------------------------------------------------------------
            RLMO=RLMA
!----------------------------------------------------------------------
          ENDDO sea_iteration_loop
!----------------------------------------------------------------------
  110     CONTINUE
!
!----------------------------------------------------------------------
!***  END OF SEA POINT PROCESSING
!----------------------------------------------------------------------
!
        ELSE
!
!----------------------------------------------------------------------
!***  NOW LAND POINTS
!----------------------------------------------------------------------
!
          land_iteration_loop: DO ITR=1,ITRMX
!
!----------------------------------------------------------------------
!***  1./MONIN-OBUKKHOV LENGTH-SCALE
!----------------------------------------------------------------------
!
            ZETALT=AMAX1(ZSLT*RLMO,ZTMIN)
            RLMO=ZETALT/ZSLT
            ZETALU=ZSLU*RLMO
!
            ZETAU=ZU*RLMO
            ZETAT=ZT*RLMO
!
!----------------------------------------------------------------------
!***  PAULSON 1970 FUNCTIONS OVER LAND W RAD. SKIN T
!----------------------------------------------------------------------
!
            IF(RLMO.LT.0.)THEN
              XLU4=1.-16.*ZETALU
              XLT4=1.-16.*ZETALT
              XU4 =1.-16.*ZETAU
              XT4 =1.-16.*ZETAT
!
              XLU=SQRT(SQRT(XLU4))
              XLT=SQRT(SQRT(XLT4))
              XU =SQRT(SQRT(XU4))
              XT =SQRT(SQRT(XT4))
!
              PSMZ=PSPMU(XU)
              SIMM=PSPMU(XLU)-PSMZ+RLOGU
              PSHZ=PSPHU(XT)
              SIMH=PSPHU(XLT)-PSHZ+RLOGT
            ELSE
              ZETAU=AMIN1(ZETAU,ZTMAX)
              ZETAT=AMIN1(ZETAT,ZTMAX)
              ZETALU=AMIN1(ZETALU,ZTMAX)
              ZETALT=AMIN1(ZETALT,ZTMAX)
              PSMZ=PSPMS(ZETAU)
              SIMM=PSPMS(ZETALU)-PSMZ+RLOGU
              PSHZ=PSPHS(ZETAT)
              SIMH=PSPHS(ZETALT)-PSHZ+RLOGT
            ENDIF
!
!----------------------------------------------------------------------
!***  BELJAARS CORRECTION FOR USTAR
!----------------------------------------------------------------------
!
            USTAR=AMAX1(SQRT(AKMS*SQRT(DU2+WSTAR2)),EPSUST)
!
!----------------------------------------------------------------------
!***  ZILITINKEVITCH FIX FOR ZT
!----------------------------------------------------------------------
!
            ZT=EXP(ZILFC*SQRT(USTAR*Z0))*Z0
            ZSLT=ZSL+ZT
            RLOGT=ALOG(ZSLT/ZT)
!----------------------------------------------------------------------
            USTARK=USTAR*VKARMAN
            AKMS=AMAX1(USTARK/SIMM,CXCH)
            AKHS=AMAX1(USTARK/SIMH,CXCH)
!----------------------------------------------------------------------
            WSTAR2=WWST2*ABS(BTGH*AKHS*DTHV)**(2./3.)
            RLMN=ELFCS*AKHS*DTHV/USTAR**3
!----------------------------------------------------------------------
            RLMP=RLMO
            RLMA=RLMO*WOLD+RLMN*WNEW
!----------------------------------------------------------------------
!           IF(ABS((RLMN-RLMO)/RLMA).LT.EPSIT)GO TO 210
!----------------------------------------------------------------------
            RLMO=RLMA
!----------------------------------------------------------------------
          ENDDO land_iteration_loop
!----------------------------------------------------------------------
  210     CONTINUE
!
!----------------------------------------------------------------------
!***  END OF LAND POINT PROCESSING AND SEA-LAND BRANCHING
!----------------------------------------------------------------------
!
        ENDIF
!
!----------------------------------------------------------------------
!***  END OF TURBULENCE-NO TURBULENCE BRANCHING
!----------------------------------------------------------------------
!
      ENDIF
!
!----------------------------------------------------------------------
!***  COUNTERGRADIENT FIX
!----------------------------------------------------------------------
!
!     HV=-AKHS*DTHV
!     IF(HV.GT.0.)THEN
!       FCT=-10.*(BTG)**(-1./3.)
!       CT=FCT*(HV/(HPBL*HPBL))**(2./3.)
!     ELSE
        CT=0.
!     ENDIF
!
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!***  THE FOLLOWING DIAGNOSTIC BLOCK PRODUCES 2-m and 10-m VALUES
!***  FOR TEMPERATURE, MOISTURE, AND WINDS.  IT IS DONE HERE SINCE
!***  THE VARIOUS QUANTITIES NEEDED FOR THE COMPUTATION ARE LOST
!***  UPON EXIT FROM THE ROTUINE.
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!
      UMFLX=AKMS*(ULOW -UZ0 )
      VMFLX=AKMS*(VLOW -VZ0 )
      HSFLX=AKHS*(THLOW-THZ0)
      HLFLX=AKHS*(QLOW -QZ0 )
!----------------------------------------------------------------------
!
      IF(SEAMASK.GT.0.5.AND.RIB.GE.RIC)THEN
!
!----------------------------------------------------------------------
        AKMS10=AMAX1( VISC/10.,CXCH)
        AKHS02=AMAX1(TVISC/02.,CXCH)
        AKHS10=AMAX1(TVISC/10.,CXCH)
!----------------------------------------------------------------------
      ELSE
!----------------------------------------------------------------------
        ZU10=ZU+10.
        ZT02=ZT+02.
        ZT10=ZT+10.
!
        RLNU10=ALOG(ZU10/ZU)
        RLNT02=ALOG(ZT02/ZT)
        RLNT10=ALOG(ZT10/ZT)
!
        ZTAU10=ZU10*RLMP
        ZTAT02=ZT02*RLMP
        ZTAT10=ZT10*RLMP
!
!----------------------------------------------------------------------
!***  LL FUNCTIONS OVER SEA
!----------------------------------------------------------------------
!
        IF(SEAMASK.GT.0.5)THEN
!
!----------------------------------------------------------------------
          IF(RLMP.LT.0.)THEN
            SIMM10=       PSLMU(ZTAU10)-PSMZ+RLNU10
            SIMH02=FHNEU*(PSLHU(ZTAT02)-PSHZ+RLNT02)
            SIMH10=FHNEU*(PSLHU(ZTAT10)-PSHZ+RLNT10)
          ELSE
            SIMM10=       PSLMS(ZTAU10)-PSMZ+RLNU10
            SIMH02=FHNEU*(PSLHS(ZTAT02)-PSHZ+RLNT02)
            SIMH10=FHNEU*(PSLHS(ZTAT10)-PSHZ+RLNT10)
          ENDIF
!
!----------------------------------------------------------------------
!***  PAULSON 1970 FUNCTIONS OVER LAND W RAD. SKIN T
!----------------------------------------------------------------------
!
        ELSE
!
!----------------------------------------------------------------------
          IF(RLMP.LT.0.)THEN
            XLU104=1.-16.*ZTAU10
            XLT024=1.-16.*ZTAT02
            XLT104=1.-16.*ZTAT10
!
            XLU10=SQRT(SQRT(XLU104))
            XLT02=SQRT(SQRT(XLT024))
            XLT10=SQRT(SQRT(XLT104))
!
            SIMM10=PSPMU(XLU10)-PSMZ+RLNU10
            SIMH02=PSPHU(XLT02)-PSHZ+RLNT02
            SIMH10=PSPHU(XLT10)-PSHZ+RLNT10
          ELSE
            ZTAU10=AMIN1(ZTAU10,ZTMAX)
            ZTAT02=AMIN1(ZTAT02,ZTMAX)
            ZTAT10=AMIN1(ZTAT10,ZTMAX)
!
            SIMM10=PSPMS(ZTAU10)-PSMZ+RLNU10
            SIMH02=PSPHS(ZTAT02)-PSHZ+RLNT02
            SIMH10=PSPHS(ZTAT10)-PSHZ+RLNT10
          ENDIF
!----------------------------------------------------------------------
        ENDIF
!----------------------------------------------------------------------
        AKMS10=AMAX1(USTARK/SIMM10,CXCH)
        AKHS02=AMAX1(USTARK/SIMH02,CXCH)
        AKHS10=AMAX1(USTARK/SIMH10,CXCH)
!----------------------------------------------------------------------
      ENDIF
!----------------------------------------------------------------------
      U10 =UMFLX/AKMS10+UZ0
      V10 =VMFLX/AKMS10+VZ0
      TH02=HSFLX/AKHS02+THZ0
      TH10=HSFLX/AKHS10+THZ0
!
!----------------------------------------------------------------------
!***  CONSTRAIN Q SO THAT IT CANNOT EXCEED QSAT AT 2M AND 10M
!----------------------------------------------------------------------
!
      TLOW=THLOW*(PLOW*1.E-5)**CAPA
      TERM1=-0.068283/TLOW
      PSHLTR=PSFC*EXP(TERM1)
      T02=TH02*(PSHLTR*1.E-5)**CAPA
      QSAT2=PQ0/PSHLTR*EXP(A2S*(T02-A3S)/(T02-A4S))
      Q02=HLFLX/AKHS02+QZ0
!
      IF(Q02.LT.0.)THEN
        IF(QLOW .GT.0.)THEN
          Q02=QLOW
        ELSE
          Q02=0.0001
        ENDIF
      ENDIF
!
      IF(Q02.GT.QSAT2)THEN
        Q02=QSAT2
      ENDIF
!
      T10=TH10*(PSHLTR*1.E-5)**CAPA
      QSAT10=PQ0/PSHLTR*EXP(A2S*(T10-A3S)/(T10-A4S))
      Q10=HLFLX/AKHS10+QZ0
      IF(Q10.LT.0.)THEN
        IF(QLOW.GT.0.)THEN
          Q10=QLOW
        ELSE
          Q10=0.0001
        ENDIF
      ENDIF
!
      IF(Q10.GT.QSAT10)THEN
        Q10=QSAT10
      ENDIF
!
!----------------------------------------------------------------------
!***  CALCULATION OF 10-M WINDS
!----------------------------------------------------------------------
!
      U10E=U10
      V10E=V10
!
      IF(SEAMASK.LT.0.5)  THEN
!
!***  CHOOSE THE EQUIVALENT Z0 HERE:
!
        ZUUZ=AMIN1(ZU*0.50,0.10)
        ZU=AMAX1(ZU*0.10,ZUUZ)
        ZU10=ZU+10.
        RLNU10=ALOG(ZU10/ZU)
        ZTAU=ZU*RLMP
        ZTAU10=ZU10*RLMP
!
        IF(RLMP.LT.0.)THEN
          XLU104=1.-16.*ZTAU10
          XU104 =1.-16.*ZTAU
          XLU10=SQRT(SQRT(XLU104))
          XU10 =SQRT(SQRT(XU104))
          SIMM10=PSPMU(XLU10)-PSPMU(XU10)+RLNU10
        ELSE
          ZTAU10=AMIN1(ZTAU10,ZTMAX)
          SIMM10=PSPMS(ZTAU10)-PSPMS(ZTAU)+RLNU10
        ENDIF
!----------------------------------------------------------------------
        EKMS10=AMAX1(USTARK/SIMM10,CXCH)
        U10E=UMFLX/EKMS10+UZ0
        V10E=VMFLX/EKMS10+VZ0
      ENDIF
!----------------------------------------------------------------------
!
      U10=U10E
      V10=V10E
!----------------------------------------------------------------------
!***  SET OTHER WRF DRIVER ARRAYS
!----------------------------------------------------------------------
      RLOW=PLOW/(R_D*TLOW)
      CHS=AKHS
      CHS2=AKHS02
      HFX=-RLOW*CP*HSFLX
      QFX=-RLOW*HLFLX*WETM
      FLHC=RLOW*CP*AKHS
      FLQC=RLOW*AKHS*WETM
      QGH=PQ0/PSHLTR*EXP(A2S*(TSK-A3S)/(TSK-A4S))
      CPM=CP*(1.+0.8*QLOW)
!
!----------------------------------------------------------------------
!
CONTAINS
!
!----------------------------------------------------------------------
        REAL FUNCTION PSLMU(ARG)
!
        REAL :: ARG
!
        PSLMU=-0.96*ALOG(1.0-4.5*ARG)
!
        END FUNCTION PSLMU
!----------------------------------------------------------------------
        REAL FUNCTION PSLMS(ARG)
!
        REAL :: ARG
!
        PSLMS=ARG*RRIC-2.076*(1.-1./(ARG+1.))
!
        END FUNCTION PSLMS
!----------------------------------------------------------------------
        REAL FUNCTION PSLHU(ARG)
!
        REAL :: ARG
!
        PSLHU=-0.96*ALOG(1.0-4.5*ARG)
!
        END FUNCTION PSLHU
!----------------------------------------------------------------------
        REAL FUNCTION PSLHS(ARG)
!
        REAL :: ARG
!
        PSLHS=ARG*RFAC-2.076*(1.-1./(ARG+1.))
!
        END FUNCTION PSLHS
!----------------------------------------------------------------------
        REAL FUNCTION PSPMU(ARG)
!
        REAL :: ARG
!
        PSPMU=-2.*ALOG((ARG+1.)*0.5)-ALOG((ARG*ARG+1.)*0.5)            &
              +2.*ATAN(ARG)-PIHF
!
        END FUNCTION PSPMU
!----------------------------------------------------------------------
        REAL FUNCTION PSPHU(ARG)
!
        REAL :: ARG
!
        PSPHU=-2.*ALOG((ARG*ARG+1.)*0.5)
!
        END FUNCTION PSPHU
!----------------------------------------------------------------------
        REAL FUNCTION PSPHS(ARG)
!
        REAL :: ARG
!
        PSPHS=5.*ARG
!
        END FUNCTION PSPHS
!----------------------------------------------------------------------
        REAL FUNCTION PSPMS(ARG)
!
        REAL :: ARG
!
        PSPMS=5.*ARG
!
        END FUNCTION PSPMS
!----------------------------------------------------------------------
!
      END SUBROUTINE SFCDIF
!
      SUBROUTINE MYJSFCINIT(LOWLYR,USTAR,restart                      &
                        ,IDS,IDE,JDS,JDE,KDS,KDE                      &
                        ,IMS,IME,JMS,JME,KMS,KME                      &
                        ,ITS,ITE,JTS,JTE,KTS,KTE)
!---------------------------------------------------------------------
      IMPLICIT NONE
!---------------------------------------------------------------------
      LOGICAL , INTENT(IN) :: restart
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                   &
                           ,IMS,IME,JMS,JME,KMS,KME                   &
                           ,ITS,ITE,JTS,JTE,KTS,KTE
      INTEGER,DIMENSION(IMS:,JMS:),INTENT(INOUT) :: LOWLYR
      REAL,DIMENSION(IMS:,JMS:),INTENT(INOUT) :: USTAR

!
      INTEGER :: I,J,K,ITF,JTF,KTF

!---------------------------------------------------------------------

      JTF=MIN0(JTE,JDE-1)
      KTF=MIN0(KTE,KDE-1)
      ITF=MIN0(ITE,IDE-1)
!
!
!***  FOR NOW, ASSUME SIGMA MODE FOR LOWEST MODEL LAYER
!
      DO J=JTS,JTF
      DO I=ITS,ITF
        LOWLYR(I,J)=1
        USTAR(I,J)=EPSUST
      ENDDO
      ENDDO

      END SUBROUTINE MYJSFCINIT

      END MODULE MODULE_BL_MYJSFC
