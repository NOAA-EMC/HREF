!-----------------------------------------------------------------------
!
      MODULE MODULE_BL_MYJPBL
!
!-----------------------------------------------------------------------
!
!***  THE MYJ PBL SCHEME
!
!-----------------------------------------------------------------------
!
      USE MODULE_INCLUDE
!
      USE MODULE_CONSTANTS,ONLY : A2,A3,A4,CP,ELIV,ELWV,EP_1,EPSQ,EPSQ2 &
                                 ,G,P608,PI,PQ0,R_D,R_V,RHOWATER        &
                                 ,STBOLT,CAPPA
!
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      PRIVATE
!
      PUBLIC :: MYJPBL_INIT, MYJPBL
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!***  FOR MYJ TURBULENCE
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      REAL,PARAMETER :: VKARMAN=0.4
      REAL,PARAMETER :: XLS=ELIV,XLV=ELWV
      REAL,PARAMETER :: RLIVWV=XLS/XLV,ELOCP=2.72E6/CP
      REAL,PARAMETER :: EPS1=1.E-12,EPS2=0.
      REAL,PARAMETER :: EPSL=0.32,EPSRU=1.E-7,EPSRS=1.E-7               &
                       ,EPSTRB=1.E-24
      REAL,PARAMETER :: EPSA=1.E-8,EPSIT=1.E-4  &
                       ,FH=1.01
      REAL,PARAMETER :: ALPH=0.30,BETA=1./273.,EL0MAX=1000.,EL0MIN=1.   &
                       ,ELFC=0.23*0.5,GAM1=0.2222222222222222222        &
                       ,PRT=1.
      REAL,PARAMETER :: A1=0.659888514560862645                         &
                       ,A2x=0.6574209922667784586                       &
                       ,B1=11.87799326209552761                         &
                       ,B2=7.226971804046074028                         &
                       ,C1=0.000830955950095854396
      REAL,PARAMETER :: ELZ0=0.,ESQ=5.0
!
      REAL,PARAMETER :: SEAFC=0.98,PQ0SEA=PQ0*SEAFC
!
      REAL,PARAMETER :: BTG=BETA*G                                      &
                       ,ESQHF=0.5*5.0                                   &
                       ,RB1=1./B1
!
      REAL,PARAMETER :: ADNH= 9.*A1*A2x*A2x*(12.*A1+3.*B2)*BTG*BTG      &
                       ,ADNM=18.*A1*A1*A2x*(B2-3.*A2x)*BTG              &
                       ,ANMH=-9.*A1*A2x*A2x*BTG*BTG                     &
                       ,ANMM=-3.*A1*A2x*(3.*A2x+3.*B2*C1+18.*A1*C1-B2)  &
                                      *BTG                              &
                       ,BDNH= 3.*A2x*(7.*A1+B2)*BTG                     &
                       ,BDNM= 6.*A1*A1                                  &
                       ,BEQH= A2x*B1*BTG+3.*A2x*(7.*A1+B2)*BTG          &
                       ,BEQM=-A1*B1*(1.-3.*C1)+6.*A1*A1                 &
                       ,BNMH=-A2x*BTG                                   &
                       ,BNMM=A1*(1.-3.*C1)                              &
                       ,BSHH=9.*A1*A2x*A2x*BTG                          &
                       ,BSHM=18.*A1*A1*A2x*C1                           &
                       ,BSMH=-3.*A1*A2x*(3.*A2x+3.*B2*C1+12.*A1*C1-B2)  &
                                      *BTG                              &
                       ,CESH=A2x                                        &
                       ,CESM=A1*(1.-3.*C1)                              &
                       ,CNV=EP_1*G/BTG                                  &
                       ,ELFCS=VKARMAN*BTG
!
!-----------------------------------------------------------------------
!***  FREE TERM IN THE EQUILIBRIUM EQUATION FOR (L/Q)**2
!-----------------------------------------------------------------------
!
      REAL,PARAMETER :: AEQH=9.*A1*A2x*A2x*B1*BTG*BTG                   &
                            +9.*A1*A2x*A2x*(12.*A1+3.*B2)*BTG*BTG       &
                       ,AEQM=3.*A1*A2x*B1*(3.*A2x+3.*B2*C1+18.*A1*C1-B2)&
                            *BTG+18.*A1*A1*A2x*(B2-3.*A2x)*BTG
!
!-----------------------------------------------------------------------
!***  FORBIDDEN TURBULENCE AREA
!-----------------------------------------------------------------------
!
      REAL,PARAMETER :: REQU=-AEQH/AEQM                                 &
                       ,EPSGH=1.E-9,EPSGM=REQU*EPSGH
!
!-----------------------------------------------------------------------
!***  NEAR ISOTROPY FOR SHEAR TURBULENCE, WW/Q2 LOWER LIMIT
!-----------------------------------------------------------------------
!
      REAL,PARAMETER :: UBRYL=(18.*REQU*A1*A1*A2x*B2*C1*BTG             &
                               +9.*A1*A2x*A2x*B2*BTG*BTG)               &
                              /(REQU*ADNM+ADNH)                         &
                       ,UBRY=(1.+EPSRS)*UBRYL,UBRY3=3.*UBRY
!
      REAL,PARAMETER :: AUBH=27.*A1*A2x*A2x*B2*BTG*BTG-ADNH*UBRY3       &
                       ,AUBM=54.*A1*A1*A2x*B2*C1*BTG -ADNM*UBRY3        &
                       ,BUBH=(9.*A1*A2x+3.*A2x*B2)*BTG-BDNH*UBRY3       &
                       ,BUBM=18.*A1*A1*C1           -BDNM*UBRY3         &
                       ,CUBR=1.                     -     UBRY3         &
                       ,RCUBR=1./CUBR
!
!-----------------------------------------------------------------------
!
      CONTAINS
!
!-----------------------------------------------------------------------
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!-----------------------------------------------------------------------
!
! REFERENCES:  Janjic (2001), NCEP Office Note 437
!              Mellor and Yamada (1982), Rev. Geophys. Space Phys.
!
! ABSTRACT:
!     MYJ UPDATES THE TURBULENT KINETIC ENERGY WITH THE PRODUCTION/
!     DISSIPATION TERM AND THE VERTICAL DIFFUSION TERM
!     (USING AN IMPLICIT FORMULATION) FROM MELLOR-YAMADA
!     LEVEL 2.5 AS EXTENDED BY JANJIC.  EXCHANGE COEFFICIENTS FOR
!     THE SURFACE AND FOR ALL LAYER INTERFACES ARE COMPUTED FROM
!     MONIN-OBUKHOV THEORY.
!     THE TURBULENT VERTICAL EXCHANGE IS THEN EXECUTED.
!
!-----------------------------------------------------------------------
      SUBROUTINE MYJPBL(DT,NPHS,HT,DZ                                  &
     &                 ,PHMID,PHINT,TH,T,EXNER,Q,CWM,U,V               &
     &                 ,TSK,QSFC,CHKLOWQ,THZ0,QZ0,UZ0,VZ0              &
     &                 ,XLAND,SICE,SNOW                                &
     &                 ,Q2,EXCH_H,USTAR,Z0,EL_MYJ,PBLH,KPBL,CT         &
     &                 ,AKHS,AKMS,ELFLX,MIXHT                          &
     &                 ,RUBLTEN,RVBLTEN,RTHBLTEN,RQBLTEN,RQCBLTEN      &
     &                 ,IDS,IDE,JDS,JDE                                &
     &                 ,IMS,IME,JMS,JME                                &
     &                 ,ITS,ITE,JTS,JTE,LM)
!----------------------------------------------------------------------
      IMPLICIT NONE
!----------------------------------------------------------------------
!
!------------------------
!***  Argument Variables
!------------------------
!
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE                            &
     &                     ,IMS,IME,JMS,JME                            &
     &                     ,ITS,ITE,JTS,JTE,LM
!
      INTEGER,INTENT(IN) :: NPHS
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: KPBL
!
      REAL,INTENT(IN) :: DT
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: HT,SICE,SNOW       &
     &                                             ,TSK,XLAND
!
      REAL,DIMENSION(IMS:IME,JMS:JME,1:LM),INTENT(IN) :: DZ,EXNER,PHMID
!
      REAL,DIMENSION(IMS:IME,JMS:JME,1:LM+1),INTENT(IN) :: PHINT
!
      REAL,DIMENSION(IMS:IME,JMS:JME,1:LM),INTENT(IN) :: Q,CWM,U,V,T,TH
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: MIXHT,PBLH
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: AKHS,AKMS
!
      REAL,DIMENSION(IMS:IME,JMS:JME,1:LM),INTENT(OUT) :: EL_MYJ
!
      REAL,DIMENSION(IMS:IME,JMS:JME,1:LM),INTENT(OUT) :: RQCBLTEN     &
     &                                                   ,RUBLTEN      &
     &                                                   ,RVBLTEN      &
     &                                                   ,RTHBLTEN     &
     &                                                   ,RQBLTEN
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: CT,QSFC,QZ0     &
     &                                                ,THZ0,USTAR      &
     &                                                ,UZ0,VZ0,Z0
!
      REAL,DIMENSION(IMS:IME,JMS:JME,1:LM),INTENT(INOUT) :: EXCH_H,Q2
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: CHKLOWQ,ELFLX
!
!---------------------
!***  Local Variables
!---------------------
!
      INTEGER :: I,J,K,LLOW,LMH,LMXL
!
      INTEGER,DIMENSION(ITS:ITE,JTS:JTE) :: LPBL
!
      REAL :: AKHS_DENS,AKMS_DENS,DELTAZ,DTDIF                         &
     &       ,DTTURBL,DUDT,DVDT,EXNSFC,PSFC,PTOP,QFC1,QLOW             &
     &       ,RDTTURBL,SEAMASK,THNEW,THOLD                             &
     &       ,ULOW,VLOW
!
      REAL,DIMENSION(1:LM) :: CWMK,PK,Q2K,QK,THEK,TK,UK,VK
!
      REAL,DIMENSION(1:LM-1) :: AKHK,AKMK,EL,GH,GM
!
      REAL,DIMENSION(1:LM+1) :: ZHK
!
      REAL,DIMENSION(ITS:ITE,JTS:JTE) :: THSK
!
      REAL,DIMENSION(1:LM) :: RHOK
!
      REAL,DIMENSION(ITS:ITE,JTS:JTE,1:LM) :: APE,THE
!
      REAL,DIMENSION(ITS:ITE,JTS:JTE,1:LM-1) :: AKH,AKM
!
      REAL,DIMENSION(ITS:ITE,JTS:JTE,1:LM+1) :: ZINT
!
!***  Begin debugging
      REAL :: ZSL_DIAG
      INTEGER :: IMD,JMD,PRINT_DIAG
!***  End debugging
!
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
!
!***  Begin debugging
      IMD=(IMS+IME)/2
      JMD=(JMS+JME)/2
!***  End debugging
!
!***  MAKE PREPARATIONS
!
!----------------------------------------------------------------------
      DTTURBL=DT*NPHS
      RDTTURBL=1./DTTURBL
      DTDIF=DTTURBL
!
      DO K=1,LM-1
      DO J=JTS,JTE
      DO I=ITS,ITE
        AKM(I,J,K)=0.
      ENDDO
      ENDDO
      ENDDO
!
      DO K=1,LM+1
      DO J=JTS,JTE
      DO I=ITS,ITE
        ZINT(I,J,K)=0.
      ENDDO
      ENDDO
      ENDDO
!
      DO J=JTS,JTE
      DO I=ITS,ITE
        ZINT(I,J,LM+1)=HT(I,J)     ! Z at bottom of lowest sigma layer
      ENDDO
      ENDDO
!
      DO K=LM,1,-1
        DO J=JTS,JTE
        DO I=ITS,ITE
          ZINT(I,J,K)=ZINT(I,J,K+1)+DZ(I,J,K)
          APE(I,J,K)=1./EXNER(I,J,K)
          THE(I,J,K)=(CWM(I,J,K)*(-ELOCP/T(I,J,K))+1.)*TH(I,J,K)
        ENDDO
        ENDDO
      ENDDO
!
      DO J=JTS,JTE
      DO I=ITS,ITE
        EL_MYJ(I,J,LM)=0.
      ENDDO
      ENDDO
!
!----------------------------------------------------------------------
!.......................................................................
!$omp parallel do &
!$omp private(j,i,lmh,ptop,psfc,seamask,k,tk,thek,qk,cwmk,       &
!$omp         pk,uk,vk,q2k,zhk,lmxl,gm,gh,el,akmk,akhk,deltaz),          &
!$omp         SCHEDULE(dynamic)
!.......................................................................
!----------------------------------------------------------------------
      setup_integration:  DO J=JTS,JTE
!----------------------------------------------------------------------
!
        DO I=ITS,ITE
!
!***  LOWEST LAYER ABOVE GROUND MUST BE FLIPPED
!
          LMH=LM
!
          PTOP=PHINT(I,J,1)
          PSFC=PHINT(I,J,LMH+1)
!
!***  CONVERT LAND MASK (1 FOR SEA; 0 FOR LAND)
!
          SEAMASK=XLAND(I,J)-1.
!
!***  FILL 1-D VERTICAL ARRAYS
!
          DO K=LM,1,-1
            TK(K)=T(I,J,K)
            THEK(K)=THE(I,J,K)
            QK(K)=Q(I,J,K)
            CWMK(K)=CWM(I,J,K)
            PK(K)=PHMID(I,J,K)
            UK(K)=U(I,J,K)
            VK(K)=V(I,J,K)
            Q2K(K)=Q2(I,J,K)
            ZHK(K)=ZINT(I,J,K)
!
          ENDDO
          ZHK(LM+1)=HT(I,J)          ! Z at bottom of lowest sigma layer
!
!***  Begin debugging
!         IF(I==IMD.AND.J==JMD)THEN
!           PRINT_DIAG=1
!         ELSE
!           PRINT_DIAG=0
!         ENDIF
!         IF(I==227.AND.J==363)PRINT_DIAG=2
!***  End debugging
!
!----------------------------------------------------------------------
!***
!***  FIND THE MIXING LENGTH
!***
          CALL MIXLEN(LMH,UK,VK,TK,THEK,QK,CWMK                        &
     &               ,Q2K,ZHK,GM,GH,EL                                 &
     &               ,PBLH(I,J),LPBL(I,J),LMXL,CT(I,J),MIXHT(I,J)      &
     &               ,LM,I,J)
!
!----------------------------------------------------------------------
!***
!***  SOLVE FOR THE PRODUCTION/DISSIPATION OF
!***  THE TURBULENT KINETIC ENERGY
!***
!
          CALL PRODQ2(LMH,DTTURBL,USTAR(I,J),GM,GH,EL,Q2K              &
     &               ,LM,I,J)
!
!----------------------------------------------------------------------
!*** THE MODEL LAYER (COUNTING UPWARD) CONTAINING THE TOP OF THE PBL
!----------------------------------------------------------------------
!
          KPBL(I,J)=LPBL(I,J)
!
!----------------------------------------------------------------------
!***
!***  FIND THE EXCHANGE COEFFICIENTS IN THE FREE ATMOSPHERE
!***
          CALL DIFCOF(LMH,LMXL,GM,GH,EL,TK,Q2K,ZHK,AKMK,AKHK           &
     &               ,LM,I,J,PRINT_DIAG)   ! debug
!
!***  COUNTING DOWNWARD FROM THE TOP, THE EXCHANGE COEFFICIENTS AKH
!***  ARE DEFINED ON THE BOTTOMS OF THE LAYERS 1 TO LM-1.  COUNTING
!***  COUNTING UPWARD FROM THE BOTTOM, THOSE SAME COEFFICIENTS EXCH_H
!***  ARE DEFINED ON THE TOPS OF THE LAYERS 1 TO LM-1.
!
          DO K=1,LM-1
            AKH(I,J,K)=AKHK(K)
            AKM(I,J,K)=AKMK(K)
            DELTAZ=0.5*(ZHK(K)-ZHK(K+2))
            EXCH_H(I,J,K)=AKHK(K)*DELTAZ
          ENDDO
!
!----------------------------------------------------------------------
!***
!***  CARRY OUT THE VERTICAL DIFFUSION OF
!***  TURBULENT KINETIC ENERGY
!***
!
          CALL VDIFQ(LMH,DTDIF,Q2K,EL,ZHK                              &
     &              ,LM)
!
!***  SAVE THE NEW Q2 AND MIXING LENGTH.
!
          DO K=1,LM
            Q2K(K)=AMAX1(Q2K(K),EPSQ2)
            Q2(I,J,K)=Q2K(K)
            IF(K<LM)EL_MYJ(I,J,K)=EL(K)   ! EL IS NOT DEFINED AT LM
          ENDDO
!
        ENDDO
!
!----------------------------------------------------------------------
!
      ENDDO setup_integration
!
!.......................................................................
!$omp end parallel do
!.......................................................................
!----------------------------------------------------------------------
!
!***  CONVERT SURFACE SENSIBLE TEMPERATURE TO POTENTIAL TEMPERATURE.
!
      DO J=JTS,JTE
      DO I=ITS,ITE
        PSFC=PHINT(I,J,LM+1)
        THSK(I,J)=TSK(I,J)*(1.E5/PSFC)**CAPPA
      ENDDO
      ENDDO
!
!----------------------------------------------------------------------
!
!----------------------------------------------------------------------
!.......................................................................
!$omp parallel do  private(i,j,k    &
!$omp  & ,thek,qk,cwmk,zhk,rhok   &
!$omp  & ,akhk,seamask,llow,akhs_dens,qfc1,qlow,psfc,exnsfc,lmh &
!$omp  & ,thold,thnew,zsl_diag,akmk,akms_dens,uk,vk &
!$omp  & ,dudt,dvdt),SCHEDULE(dynamic)
!.......................................................................
!----------------------------------------------------------------------
      main_integration:  DO J=JTS,JTE
!----------------------------------------------------------------------
!
        DO I=ITS,ITE
!
!***  FILL 1-D VERTICAL ARRAYS
!
          DO K=LM,1,-1
            THEK(K)=THE(I,J,K)
            QK(K)=Q(I,J,K)
            CWMK(K)=CWM(I,J,K)
            ZHK(K)=ZINT(I,J,K)
            RHOK(K)=PHMID(I,J,K)/(R_D*T(I,J,K)*(1.+P608*QK(K)-CWMK(K)))
          ENDDO
!
!***  COUNTING DOWNWARD FROM THE TOP, THE EXCHANGE COEFFICIENTS AKH
!***  ARE DEFINED ON THE BOTTOMS OF THE LAYERS 1 TO LM-1.  THESE COEFFICIENTS
!***  ARE ALSO MULTIPLIED BY THE DENSITY AT THE BOTTOM INTERFACE LEVEL.
!
          DO K=1,LM-1
            AKHK(K)=AKH(I,J,K)*0.5*(RHOK(K)+RHOK(K+1))
          ENDDO
!
          ZHK(LM+1)=ZINT(I,J,LM+1)
!
          SEAMASK=XLAND(I,J)-1.
          THZ0(I,J)=(1.-SEAMASK)*THSK(I,J)+SEAMASK*THZ0(I,J)
!
          LLOW=LM
          AKHS_DENS=AKHS(I,J)*RHOK(LM)
!
          IF(SEAMASK<0.5)THEN
            QFC1=XLV*CHKLOWQ(I,J)*AKHS_DENS
!
            IF(SNOW(I,J)>0..OR.SICE(I,J)>0.5)THEN
              QFC1=QFC1*RLIVWV
            ENDIF
!
            IF(QFC1>0.)THEN
              QLOW=QK(LM)
              QSFC(I,J)=QLOW+ELFLX(I,J)/QFC1
            ELSE
!-- Convert back to specific humidity
              QSFC(I,J)=QSFC(I,J)/(1.+QSFC(I,J))
            ENDIF
!
          ELSE
            PSFC=PHINT(I,J,LM+1)
            EXNSFC=(1.E5/PSFC)**CAPPA

            QSFC(I,J)=PQ0SEA/PSFC                                      &
     &         *EXP(A2*(THSK(I,J)-A3*EXNSFC)/(THSK(I,J)-A4*EXNSFC))
          ENDIF
!
          QZ0 (I,J)=(1.-SEAMASK)*QSFC(I,J)+SEAMASK*QZ0 (I,J)
!
!***  LOWEST LAYER ABOVE GROUND MUST BE FLIPPED
!
          LMH=LM
!
!----------------------------------------------------------------------
!***  CARRY OUT THE VERTICAL DIFFUSION OF
!***  TEMPERATURE AND WATER VAPOR
!----------------------------------------------------------------------
!
          CALL VDIFH(DTDIF,LMH,THZ0(I,J),QZ0(I,J)                      &
     &              ,AKHS_DENS,CHKLOWQ(I,J),CT(I,J)                    &
     &              ,THEK,QK,CWMK,AKHK,ZHK,RHOK                        &
     &              ,LM,I,J)
!----------------------------------------------------------------------
!***
!***  COMPUTE PRIMARY VARIABLE TENDENCIES
!***
          DO K=1,LM
            THOLD=TH(I,J,K)
            THNEW=THEK(K)+CWMK(K)*ELOCP*APE(I,J,K)
            RTHBLTEN(I,J,K)=(THNEW-THOLD)*RDTTURBL
            RQBLTEN(I,J,K)=(QK(K)-Q(I,J,K))*RDTTURBL
            RQCBLTEN(I,J,K)=(CWMK(K)-CWM(I,J,K))*RDTTURBL
          ENDDO
!
!*** Begin debugging
!         IF(I==IMD.AND.J==JMD)THEN
!           PRINT_DIAG=0
!         ELSE
!           PRINT_DIAG=0
!         ENDIF
!         IF(I==227.AND.J==363)PRINT_DIAG=0
!*** End debugging
!
        PSFC=.01*PHINT(I,J,LM+1)
        ZSL_DIAG=0.5*DZ(I,J,LM)
!
!*** Begin debugging
!         IF(PRINT_DIAG==1)THEN
!
!           write(6,"(a, 2i5, 2i3, 2f8.2, f6.2, 2f8.2)") &
!           '{turb4 i,j, Kpbl, Kmxl, Psfc, Zsfc, Zsl, Zpbl, Zmxl = ' &
!           , i, j, KPBL(i,j), LM-LMXL+1, PSFC, ZHK(LMH+1), ZSL_diag  &
!           , PBLH(i,j), ZHK(LMXL)-ZHK(LMH+1)
!           write(6,"(a, 2f7.2, f7.3, 3e11.4)") &
!           '{turb4 tsk, thsk, qz0, q**2_0, akhs, exch_0 = ' &
!           , tsk(i,j)-273.15, thsk(i,j), 1000.*qz0(i,j) &
!           , q2(i,1,j), akhs(i,j), akhs(i,j)*ZSL_diag
!           write(6,"(a)") &
!           '{turb5 k, PHmid, PHint_1, Tc, TH, DTH, GH, GM, EL, Q**2, Akh, EXCH_h, Dz, Dp'
!           do k=1,LM/2
!             write(6,"(a,i3, 2f8.2, 2f8.3, 3e12.4, 4e11.4, f7.2, f6.2)") &
!            '{turb5 ', k, .01*phmid(i,k,j),.01*phint(i,k,j), T(i,k,j)-273.15 &
!            , th(i,k,j), DTTURBL*rthblten(i,k,j), GH(K), GM(K) &
!            , el_myj(i,K,j), q2(i,k+1,j), akh(i,K,j) &
!            , exch_h(i,k,j), dz(i,k,j), .01*(phint(i,k,j)-phint(i,k+1,j))
!           enddo
!
!         ELSEIF(PRINT_DIAG==2)THEN
!
!           write(6,"(a, 2i5, 2i3, 2f8.2, f6.2, 2f8.2)") &
!           '}turb4 i,j, Kpbl, Kmxl, Psfc, Zsfc, Zsl, Zpbl, Zmxl = ' &
!           , i, j, KPBL(i,j), LM-LMXL+1, PSFC, ZHK(LMH+1), ZSL_diag  &
!           , PBLH(i,j), ZHK(LMXL)-ZHK(LMH+1)
!           write(6,"(a, 2f7.2, f7.3, 3e11.4)") &
!           '}turb4 tsk, thsk, qz0, q**2_0, akhs, exch_0 = ' &
!           , tsk(i,j)-273.15, thsk(i,j), 1000.*qz0(i,j) &
!           , q2(i,1,j), akhs(i,j), akhs(i,j)*ZSL_diag
!           write(6,"(a)") &
!           '}turb5 k, PHmid, PHint_1, Tc, TH, DTH, GH, GM, EL, Q**2, Akh, EXCH_h, Dz, Dp'
!           do k=1,LM/2
!             write(6,"(a,i3, 2f8.2, 2f8.3, 3e12.4, 4e11.4, f7.2, f6.2)") &
!            '}turb5 ', k, .01*phmid(i,k,j),.01*phint(i,k,j), T(i,k,j)-273.15 &
!            , th(i,k,j), DTTURBL*rthblten(i,k,j), GH(K), GM(K) &
!            , el_myj(i,K,j), q2(i,k+1,j), akh(i,K,j) &
!            , exch_h(i,k,j), dz(i,k,j), .01*(phint(i,k,j)-phint(i,k+1,j))
!           enddo
!         ENDIF
!*** End debugging
!
!----------------------------------------------------------------------
!
!***  FILL 1-D VERTICAL ARRAYS
!
          DO K=1,LM-1
            AKMK(K)=AKM(I,J,K)
            AKMK(K)=AKMK(K)*(RHOK(K)+RHOK(K+1))*0.5
          ENDDO
!
          AKMS_DENS=AKMS(I,J)*RHOK(LM)
!
          DO K=LM,1,-1
            UK(K)=U(I,J,K)
            VK(K)=V(I,J,K)
            ZHK(K)=ZINT(I,J,K)
          ENDDO
          ZHK(LM+1)=ZINT(I,J,LM+1)
!
!----------------------------------------------------------------------
!***  CARRY OUT THE VERTICAL DIFFUSION OF
!***  VELOCITY COMPONENTS
!----------------------------------------------------------------------
!
          CALL VDIFV(LMH,DTDIF,UZ0(I,J),VZ0(I,J)                       &
     &              ,AKMS_DENS,UK,VK,AKMK,ZHK,RHOK                     &
     &              ,LM,I,J)
!
!----------------------------------------------------------------------
!***
!***  COMPUTE PRIMARY VARIABLE TENDENCIES
!***
          DO K=1,LM
            RUBLTEN(I,J,K)=(UK(K)-U(I,J,K))*RDTTURBL
            RVBLTEN(I,J,K)=(VK(K)-V(I,J,K))*RDTTURBL
          ENDDO
!
        ENDDO
!----------------------------------------------------------------------
!
      ENDDO main_integration
!$omp end parallel do
!
!----------------------------------------------------------------------
!
      END SUBROUTINE MYJPBL
!
!----------------------------------------------------------------------
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!----------------------------------------------------------------------
                          SUBROUTINE MIXLEN                            &
!----------------------------------------------------------------------
!   ******************************************************************
!   *                                                                *
!   *                   LEVEL 2.5 MIXING LENGTH                      *
!   *                                                                *
!   ******************************************************************
!
     &(LMH,U,V,T,THE,Q,CWM,Q2,Z,GM,GH,EL,PBLH,LPBL,LMXL,CT,MIXHT       &
     &,LM,I,J)
!----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: LM,I,J
!
      INTEGER,INTENT(IN) :: LMH
!
      INTEGER,INTENT(OUT) :: LMXL,LPBL
!
      REAL,DIMENSION(1:LM),INTENT(IN) :: CWM,Q,Q2,T,THE,U,V
!
      REAL,DIMENSION(1:LM+1),INTENT(IN) :: Z
!
      REAL,INTENT(OUT) :: MIXHT,PBLH
!
      REAL,DIMENSION(1:LM-1),INTENT(OUT) :: EL,GH,GM
!
      REAL,INTENT(INOUT) :: CT
!----------------------------------------------------------------------
!***
!***  LOCAL VARIABLES
!***
      INTEGER :: K,LPBLM
!
      REAL :: A,ADEN,B,BDEN,AUBR,BUBR,BLMX,EL0,ELOQ2X,GHL,GML           &
     &       ,QOL2ST,QOL2UN,QDZL,RDZ,SQ,SREL,SZQ,TEM,THM,VKRMZ
!
      REAL,DIMENSION(1:LM) :: Q1
!
      REAL,DIMENSION(1:LM-1) :: DTH,ELM,REL
!
!----------------------------------------------------------------------
!**********************************************************************
!--------------FIND THE HEIGHT OF THE PBL-------------------------------
      LPBL=LMH
!
      DO K=LMH-1,1,-1
        IF(Q2(K)<=EPSQ2*FH)THEN
          LPBL=K
          GO TO 110
        ENDIF
      ENDDO
!
      LPBL=1
!
!--------------THE HEIGHT OF THE PBL------------------------------------
!
 110  PBLH=Z(LPBL)-Z(LMH+1)
!
!-----------------------------------------------------------------------
      DO K=1,LMH
        Q1(K)=0.
      ENDDO
!
      DO K=1,LMH-1
        DTH(K)=THE(K)-THE(K+1)
      ENDDO
!
      DO K=LMH-2,1,-1
        IF(DTH(K)>0..AND.DTH(K+1)<=0.)THEN
          DTH(K)=DTH(K)+CT
          EXIT
        ENDIF
      ENDDO
!
      CT=0.
!----------------------------------------------------------------------
      DO K=1,LMH-1
        RDZ=2./(Z(K)-Z(K+2))
        GML=((U(K)-U(K+1))**2+(V(K)-V(K+1))**2)*RDZ*RDZ
        GM(K)=MAX(GML,EPSGM)
!
        TEM=(T(K)+T(K+1))*0.5
        THM=(THE(K)+THE(K+1))*0.5
!
        A=THM*P608
        B=(ELOCP/TEM-1.-P608)*THM
!
        GHL=(DTH(K)*((Q(K)+Q(K+1)+CWM(K)+CWM(K+1))*(0.5*P608)+1.)      &
     &     +(Q(K)-Q(K+1)+CWM(K)-CWM(K+1))*A                            &
     &     +(CWM(K)-CWM(K+1))*B)*RDZ
!
        IF(ABS(GHL)<=EPSGH)GHL=EPSGH
        GH(K)=GHL
      ENDDO
!
!----------------------------------------------------------------------
!***  FIND MAXIMUM MIXING LENGTHS AND THE LEVEL OF THE PBL TOP
!----------------------------------------------------------------------
!
      LMXL=LMH
!
      DO K=1,LMH-1
        GML=GM(K)
        GHL=GH(K)
!
        IF(GHL>=EPSGH)THEN
          IF(GML/GHL<=REQU)THEN
            ELM(K)=EPSL
            LMXL=K
          ELSE
            AUBR=(AUBM*GML+AUBH*GHL)*GHL
            BUBR= BUBM*GML+BUBH*GHL
            QOL2ST=(-0.5*BUBR+SQRT(BUBR*BUBR*0.25-AUBR*CUBR))*RCUBR
            ELOQ2X=1./MAX(EPSGH, QOL2ST)
            ELM(K)=MAX(SQRT(ELOQ2X*Q2(K)),EPSL)
          ENDIF
        ELSE
          ADEN=(ADNM*GML+ADNH*GHL)*GHL
          BDEN= BDNM*GML+BDNH*GHL
          QOL2UN=-0.5*BDEN+SQRT(BDEN*BDEN*0.25-ADEN)
          ELOQ2X=1./(QOL2UN+EPSRU)       ! repsr1/qol2un
          ELM(K)=MAX(SQRT(ELOQ2X*Q2(K)),EPSL)
        ENDIF
      ENDDO
!
      IF(ELM(LMH-1)==EPSL)LMXL=LMH
!
!----------------------------------------------------------------------
!***  THE HEIGHT OF THE MIXED LAYER
!----------------------------------------------------------------------
!
      BLMX=Z(LMXL)-Z(LMH+1)
      MIXHT=BLMX
!
!----------------------------------------------------------------------
      DO K=LPBL,LMH
        Q1(K)=SQRT(Q2(K))
      ENDDO
!----------------------------------------------------------------------
      SZQ=0.
      SQ =0.
!
      DO K=1,LMH-1
        QDZL=(Q1(K)+Q1(K+1))*(Z(K+1)-Z(K+2))
        SZQ=(Z(K+1)+Z(K+2)-Z(LMH+1)-Z(LMH+1))*QDZL+SZQ
        SQ=QDZL+SQ
      ENDDO
!
!----------------------------------------------------------------------
!***  COMPUTATION OF ASYMPTOTIC L IN BLACKADAR FORMULA
!----------------------------------------------------------------------
!
      EL0=MIN(ALPH*SZQ*0.5/SQ,EL0MAX)
      EL0=MAX(EL0            ,EL0MIN)
!
!----------------------------------------------------------------------
!***  ABOVE THE PBL TOP
!----------------------------------------------------------------------
!
      LPBLM=MAX(LPBL-1,1)
!
      DO K=1,LPBLM
        EL(K)=MIN((Z(K)-Z(K+2))*ELFC,ELM(K))
        REL(K)=EL(K)/ELM(K)
      ENDDO
!
!----------------------------------------------------------------------
!***  INSIDE THE PBL
!----------------------------------------------------------------------
!
      IF(LPBL<LMH)THEN
        DO K=LPBL,LMH-1
          VKRMZ=(Z(K+1)-Z(LMH+1))*VKARMAN
          EL(K)=MIN(VKRMZ/(VKRMZ/EL0+1.),ELM(K))
          REL(K)=EL(K)/ELM(K)
        ENDDO
      ENDIF
!
      DO K=LPBL+1,LMH-2
        SREL=MIN(((REL(K-1)+REL(K+1))*0.5+REL(K))*0.5,REL(K))
        EL(K)=MAX(SREL*ELM(K),EPSL)
      ENDDO
!
!----------------------------------------------------------------------
      END SUBROUTINE MIXLEN
!----------------------------------------------------------------------
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!----------------------------------------------------------------------
                          SUBROUTINE PRODQ2                            &
!----------------------------------------------------------------------
!   ******************************************************************
!   *                                                                *
!   *            LEVEL 2.5 Q2 PRODUCTION/DISSIPATION                 *
!   *                                                                *
!   ******************************************************************
!
     &(LMH,DTTURBL,USTAR,GM,GH,EL,Q2                                   &
     &,LM,I,J)
!----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: LM,I,J
!
      INTEGER,INTENT(IN) :: LMH
!
      REAL,INTENT(IN) :: DTTURBL,USTAR
!
      REAL,DIMENSION(1:LM-1),INTENT(IN) :: GH,GM
      REAL,DIMENSION(1:LM-1),INTENT(INOUT) :: EL
!
      REAL,DIMENSION(1:LM),INTENT(INOUT) :: Q2
!----------------------------------------------------------------------
!***
!***  LOCAL VARIABLES
!***
      INTEGER :: K
!
      REAL :: ADEN,AEQU,ANUM,ARHS,BDEN,BEQU,BNUM,BRHS,CDEN,CRHS        &
     &       ,DLOQ1,ELOQ11,ELOQ12,ELOQ13,ELOQ21,ELOQ22,ELOQ31,ELOQ32   &
     &       ,ELOQ41,ELOQ42,ELOQ51,ELOQ52,ELOQN,EQOL2,GHL,GML          &
     &       ,RDEN1,RDEN2,RHS2,RHSP1,RHSP2,RHST2
!
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
!
      main_integration: DO K=1,LMH-1
        GML=GM(K)
        GHL=GH(K)
!
!----------------------------------------------------------------------
!***  COEFFICIENTS OF THE EQUILIBRIUM EQUATION
!----------------------------------------------------------------------
!
        AEQU=(AEQM*GML+AEQH*GHL)*GHL
        BEQU= BEQM*GML+BEQH*GHL
!
!----------------------------------------------------------------------
!***  EQUILIBRIUM SOLUTION FOR L/Q
!----------------------------------------------------------------------
!
        EQOL2=-0.5*BEQU+SQRT(BEQU*BEQU*0.25-AEQU)
!
!----------------------------------------------------------------------
!***  IS THERE PRODUCTION/DISSIPATION ?
!----------------------------------------------------------------------
!
        IF((GML+GHL*GHL<=EPSTRB)                                       &
     &   .OR.(GHL>=EPSGH.AND.GML/GHL<=REQU)                            &
     &   .OR.(EQOL2<=EPS2))THEN
!
!----------------------------------------------------------------------
!***  NO TURBULENCE
!----------------------------------------------------------------------
!
          Q2(K)=EPSQ2
          EL(K)=EPSL
!----------------------------------------------------------------------
!
        ELSE
!
!----------------------------------------------------------------------
!***  TURBULENCE
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!***  COEFFICIENTS OF THE TERMS IN THE NUMERATOR
!----------------------------------------------------------------------
!
          ANUM=(ANMM*GML+ANMH*GHL)*GHL
          BNUM= BNMM*GML+BNMH*GHL
!
!----------------------------------------------------------------------
!***  COEFFICIENTS OF THE TERMS IN THE DENOMINATOR
!----------------------------------------------------------------------
!
          ADEN=(ADNM*GML+ADNH*GHL)*GHL
          BDEN= BDNM*GML+BDNH*GHL
          CDEN= 1.
!
!----------------------------------------------------------------------
!***  COEFFICIENTS OF THE NUMERATOR OF THE LINEARIZED EQ.
!----------------------------------------------------------------------
!
          ARHS=-(ANUM*BDEN-BNUM*ADEN)*2.
          BRHS=- ANUM*4.
          CRHS=- BNUM*2.
!
!----------------------------------------------------------------------
!***  INITIAL VALUE OF L/Q
!----------------------------------------------------------------------
!
          DLOQ1=EL(K)/SQRT(Q2(K))
!
!----------------------------------------------------------------------
!***  FIRST ITERATION FOR L/Q, RHS=0
!----------------------------------------------------------------------
!
          ELOQ21=1./EQOL2
          ELOQ11=SQRT(ELOQ21)
          ELOQ31=ELOQ21*ELOQ11
          ELOQ41=ELOQ21*ELOQ21
          ELOQ51=ELOQ21*ELOQ31
!
!----------------------------------------------------------------------
!***  1./DENOMINATOR
!----------------------------------------------------------------------
!
          RDEN1=1./(ADEN*ELOQ41+BDEN*ELOQ21+CDEN)
!
!----------------------------------------------------------------------
!***  D(RHS)/D(L/Q)
!----------------------------------------------------------------------
!
          RHSP1=(ARHS*ELOQ51+BRHS*ELOQ31+CRHS*ELOQ11)*RDEN1*RDEN1
!
!----------------------------------------------------------------------
!***  FIRST-GUESS SOLUTION
!----------------------------------------------------------------------
!
          ELOQ12=ELOQ11+(DLOQ1-ELOQ11)*EXP(RHSP1*DTTURBL)
          ELOQ12=MAX(ELOQ12,EPS1)
!
!----------------------------------------------------------------------
!***  SECOND ITERATION FOR L/Q
!----------------------------------------------------------------------
!
          ELOQ22=ELOQ12*ELOQ12
          ELOQ32=ELOQ22*ELOQ12
          ELOQ42=ELOQ22*ELOQ22
          ELOQ52=ELOQ22*ELOQ32
!
!----------------------------------------------------------------------
!***  1./DENOMINATOR
!----------------------------------------------------------------------
!
          RDEN2=1./(ADEN*ELOQ42+BDEN*ELOQ22+CDEN)
          RHS2 =-(ANUM*ELOQ42+BNUM*ELOQ22)*RDEN2+RB1
          RHSP2= (ARHS*ELOQ52+BRHS*ELOQ32+CRHS*ELOQ12)*RDEN2*RDEN2
          RHST2=RHS2/RHSP2
!
!----------------------------------------------------------------------
!***  CORRECTED SOLUTION
!----------------------------------------------------------------------
!
          ELOQ13=ELOQ12-RHST2+(RHST2+DLOQ1-ELOQ12)*EXP(RHSP2*DTTURBL)
          ELOQ13=AMAX1(ELOQ13,EPS1)
!
!----------------------------------------------------------------------
!***  TWO ITERATIONS IS ENOUGH IN MOST CASES ...
!----------------------------------------------------------------------
!
          ELOQN=ELOQ13
!
          IF(ELOQN>EPS1)THEN
            Q2(K)=EL(K)*EL(K)/(ELOQN*ELOQN)
            Q2(K)=AMAX1(Q2(K),EPSQ2)
            IF(Q2(K)==EPSQ2)THEN
              EL(K)=EPSL
            ENDIF
          ELSE
            Q2(K)=EPSQ2
            EL(K)=EPSL
          ENDIF
!
!----------------------------------------------------------------------
!***  END OF TURBULENT BRANCH
!----------------------------------------------------------------------
!
        ENDIF
!----------------------------------------------------------------------
!***  END OF PRODUCTION/DISSIPATION LOOP
!----------------------------------------------------------------------
!
      ENDDO main_integration
!
!----------------------------------------------------------------------
!***  LOWER BOUNDARY CONDITION FOR Q2
!----------------------------------------------------------------------
!
      Q2(LMH)=AMAX1(B1**(2./3.)*USTAR*USTAR,EPSQ2)
!----------------------------------------------------------------------
!
      END SUBROUTINE PRODQ2
!
!----------------------------------------------------------------------
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!----------------------------------------------------------------------
                           SUBROUTINE DIFCOF                           &
!   ******************************************************************
!   *                                                                *
!   *                LEVEL 2.5 DIFFUSION COEFFICIENTS                *
!   *                                                                *
!   ******************************************************************
     &(LMH,LMXL,GM,GH,EL,T,Q2,Z,AKM,AKH                                &
     &,LM,I,J,PRINT_DIAG)   ! debug
!----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: LM,I,J
!
      INTEGER,INTENT(IN) :: LMH,LMXL
!
      REAL,DIMENSION(1:LM),INTENT(IN) :: Q2,T
      REAL,DIMENSION(1:LM-1),INTENT(IN) :: EL,GH,GM
      REAL,DIMENSION(1:LM+1),INTENT(IN) :: Z
!
      REAL,DIMENSION(1:LM-1),INTENT(OUT) :: AKH,AKM
!----------------------------------------------------------------------
!***
!***  LOCAL VARIABLES
!***
      INTEGER :: K,KINV
!
      REAL :: ADEN,AKMIN,BDEN,BESH,BESM,CDEN,D2T,ELL,ELOQ2,ELOQ4,ELQDZ &
     &       ,ESH,ESM,GHL,GML,Q1L,RDEN,RDZ
!
!*** Begin debugging
      INTEGER,INTENT(IN) :: PRINT_DIAG
!     REAL :: D2Tmin
!*** End debugging
!
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
!
      DO K=1,LMH-1
        ELL=EL(K)
!
        ELOQ2=ELL*ELL/Q2(K)
        ELOQ4=ELOQ2*ELOQ2
!
        GML=GM(K)
        GHL=GH(K)
!
!----------------------------------------------------------------------
!***  COEFFICIENTS OF THE TERMS IN THE DENOMINATOR
!----------------------------------------------------------------------
!
        ADEN=(ADNM*GML+ADNH*GHL)*GHL
        BDEN= BDNM*GML+BDNH*GHL
        CDEN= 1.
!
!----------------------------------------------------------------------
!***  COEFFICIENTS FOR THE SM DETERMINANT
!----------------------------------------------------------------------
!
        BESM=BSMH*GHL
!
!----------------------------------------------------------------------
!***  COEFFICIENTS FOR THE SH DETERMINANT
!----------------------------------------------------------------------
!
        BESH=BSHM*GML+BSHH*GHL
!
!----------------------------------------------------------------------
!***  1./DENOMINATOR
!----------------------------------------------------------------------
!
        RDEN=1./(ADEN*ELOQ4+BDEN*ELOQ2+CDEN)
!
!----------------------------------------------------------------------
!***  SM AND SH
!----------------------------------------------------------------------
!
        ESM=(BESM*ELOQ2+CESM)*RDEN
        ESH=(BESH*ELOQ2+CESH)*RDEN
!
!----------------------------------------------------------------------
!***  DIFFUSION COEFFICIENTS
!----------------------------------------------------------------------
!
        RDZ=2./(Z(K)-Z(K+2))
        Q1L=SQRT(Q2(K))
        ELQDZ=ELL*Q1L*RDZ
        AKM(K)=ELQDZ*ESM
        AKH(K)=ELQDZ*ESH
!----------------------------------------------------------------------
      ENDDO
!----------------------------------------------------------------------
!
!----------------------------------------------------------------------
!***  INVERSIONS
!----------------------------------------------------------------------
!
!     IF(LMXL==LMH)THEN
!       KINV=LMH
!       D2Tmin=0.
!
!       DO K=LMH/2,LMH-1
!         D2T=T(K-1)-2.*T(K)+T(K+1)
!         IF(D2T<D2Tmin)THEN
!           D2Tmin=D2T
!           IF(D2T<0)KINV=K
!         ENDIF
!       ENDDO
!
!       IF(KINV<LMH)THEN
!         DO K=KINV-1,LMH-1
!           RDZ=2./(Z(K)-Z(K+2))
!           AKMIN=0.5*RDZ
!           AKM(K)=MAX(AKM(K),AKMIN)
!           AKH(K)=MAX(AKH(K),AKMIN)
!         ENDDO
!
!*** Begin debugging
!         IF(PRINT_DIAG>0)THEN
!           write(6,"(a,3i3)") '{turb1 lmxl,lmh,kinv=',lmxl,lmh,kinv
!           write(6,"(a,3i3)") '}turb1 lmxl,lmh,kinv=',lmxl,lmh,kinv
!           IF(PRINT_DIAG==1)THEN
!             write(6,"(a)") &
!               '{turb3 k, t, d2t, rdz, z(k), z(k+2), akmin, akh '
!           ELSE
!             write(6,"(a)") &
!               '}turb3 k, t, d2t, rdz, z(k), z(k+2), akmin, akh '
!           ENDIF
!           DO K=LMH-1,KINV-1,-1
!             D2T=T(K-1)-2.*T(K)+T(K+1)
!             RDZ=2./(Z(K)-Z(K+2))
!             AKMIN=0.5*RDZ
!             IF(PRINT_DIAG==1)THEN
!               write(6,"(a,i3,f8.3,2e12.5,2f9.2,2e12.5)") '{turb3 ' &
!               ,k,t(k)-273.15,d2t,rdz,z(k),z(k+2),akmin,akh(k)
!             ELSE
!               write(6,"(a,i3,f8.3,2e12.5,2f9.2,2e12.5)") '}turb3 ' &
!               ,k,t(k)-273.15,d2t,rdz,z(k),z(k+2),akmin,akh(k)
!             ENDIF
!           ENDDO
!         ENDIF     !- IF (print_diag > 0) THEN
!       ENDIF       !- IF(KINV<LMH)THEN
!*** End debugging
!
!     ENDIF
!----------------------------------------------------------------------
!
      END SUBROUTINE DIFCOF
!
!----------------------------------------------------------------------
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!----------------------------------------------------------------------
                           SUBROUTINE VDIFQ                            &
!   ******************************************************************
!   *                                                                *
!   *               VERTICAL DIFFUSION OF Q2 (TKE)                   *
!   *                                                                *
!   ******************************************************************
     &(LMH,DTDIF,Q2,EL,Z                                               &
     &,LM)
!----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: LM
!
      INTEGER,INTENT(IN) :: LMH
!
      REAL,INTENT(IN) :: DTDIF
!
      REAL,DIMENSION(1:LM-1),INTENT(IN) :: EL
      REAL,DIMENSION(1:LM+1),INTENT(IN) :: Z
!
      REAL,DIMENSION(1:LM),INTENT(INOUT) :: Q2
!----------------------------------------------------------------------
!***
!***  LOCAL VARIABLES
!***
      INTEGER :: K
!
      REAL :: ADEN,AKQS,BDEN,BESH,BESM,CDEN,CF,DTOZS,ELL,ELOQ2,ELOQ4   &
     &       ,ELQDZ,ESH,ESM,ESQHF,GHL,GML,Q1L,RDEN,RDZ
!
      REAL,DIMENSION(1:LM-2) :: AKQ,CM,CR,DTOZ,RSQ2
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
!***
!***  VERTICAL TURBULENT DIFFUSION
!***
!----------------------------------------------------------------------
      ESQHF=0.5*ESQ
!
      DO K=1,LMH-2
        DTOZ(K)=(DTDIF+DTDIF)/(Z(K)-Z(K+2))
        AKQ(K)=SQRT((Q2(K)+Q2(K+1))*0.5)*(EL(K)+EL(K+1))*ESQHF         &
     &        /(Z(K+1)-Z(K+2))
        CR(K)=-DTOZ(K)*AKQ(K)
      ENDDO
!
      CM(1)=DTOZ(1)*AKQ(1)+1.
      RSQ2(1)=Q2(1)
!
      DO K=1+1,LMH-2
        CF=-DTOZ(K)*AKQ(K-1)/CM(K-1)
        CM(K)=-CR(K-1)*CF+(AKQ(K-1)+AKQ(K))*DTOZ(K)+1.
        RSQ2(K)=-RSQ2(K-1)*CF+Q2(K)
      ENDDO
!
      DTOZS=(DTDIF+DTDIF)/(Z(LMH-1)-Z(LMH+1))
      AKQS=SQRT((Q2(LMH-1)+Q2(LMH))*0.5)*(EL(LMH-1)+ELZ0)*ESQHF        &
     &    /(Z(LMH)-Z(LMH+1))
!
      CF=-DTOZS*AKQ(LMH-2)/CM(LMH-2)
!
      Q2(LMH-1)=(DTOZS*AKQS*Q2(LMH)-RSQ2(LMH-2)*CF+Q2(LMH-1))          &
     &        /((AKQ(LMH-2)+AKQS)*DTOZS-CR(LMH-2)*CF+1.)
!
      DO K=LMH-2,1,-1
        Q2(K)=(-CR(K)*Q2(K+1)+RSQ2(K))/CM(K)
      ENDDO
!----------------------------------------------------------------------
!
      END SUBROUTINE VDIFQ
!
!----------------------------------------------------------------------
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!---------------------------------------------------------------------
      SUBROUTINE VDIFH(DTDIF,LMH,THZ0,QZ0,RKHS,CHKLOWQ,CT             &
     &                ,THE,Q,CWM,RKH,Z,RHO                            &
     &                ,LM,I,J)
!     ***************************************************************
!     *                                                             *
!     *         VERTICAL DIFFUSION OF MASS VARIABLES                *
!     *                                                             *
!     ***************************************************************
!---------------------------------------------------------------------
!
      IMPLICIT NONE
!
!---------------------------------------------------------------------
      INTEGER,INTENT(IN) :: LM,I,J
!
      INTEGER,INTENT(IN) :: LMH
!
      REAL,INTENT(IN) :: CHKLOWQ,CT,DTDIF,QZ0,RKHS,THZ0
!
      REAL,DIMENSION(1:LM-1),INTENT(IN) :: RKH
      REAL,DIMENSION(1:LM),INTENT(IN) :: RHO
      REAL,DIMENSION(1:LM+1),INTENT(IN) :: Z
      REAL,DIMENSION(1:LM),INTENT(INOUT) :: CWM,Q,THE
!
!----------------------------------------------------------------------
!***
!***  LOCAL VARIABLES
!***
      INTEGER :: K
!
      REAL :: CF,CMB,CMCB,CMQB,CMTB,CTHF,DTOZL,DTOZS                   &
     &       ,RCML,RKHH,RKQS,RSCB,RSQB,RSTB
!
      REAL,DIMENSION(1:LM-1) :: CM,CR,DTOZ,RKCT,RSC,RSQ,RST
!
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
      CTHF=0.5*CT
!
      DO K=1,LMH-1
        DTOZ(K)=DTDIF/(Z(K)-Z(K+1))
        CR(K)=-DTOZ(K)*RKH(K)
        RKCT(K)=RKH(K)*(Z(K)-Z(K+2))*CTHF
      ENDDO
!
      CM(1)=DTOZ(1)*RKH(1)+RHO(1)
!----------------------------------------------------------------------
      RST(1)=-RKCT(1)*DTOZ(1)                                    &
     &         +THE(1)*RHO(1)
      RSQ(1)=Q(1)  *RHO(1)
      RSC(1)=CWM(1)*RHO(1)
!----------------------------------------------------------------------
      DO K=1+1,LMH-1
        DTOZL=DTOZ(K)
        CF=-DTOZL*RKH(K-1)/CM(K-1)
        CM(K)=-CR(K-1)*CF+(RKH(K-1)+RKH(K))*DTOZL+RHO(K)
        RST(K)=-RST(K-1)*CF+(RKCT(K-1)-RKCT(K))*DTOZL+THE(K)*RHO(K)
        RSQ(K)=-RSQ(K-1)*CF+Q(K)  *RHO(K)
        RSC(K)=-RSC(K-1)*CF+CWM(K)*RHO(K)
      ENDDO
!
      DTOZS=DTDIF/(Z(LMH)-Z(LMH+1))
      RKHH=RKH(LMH-1)
!
      CF=-DTOZS*RKHH/CM(LMH-1)
      RKQS=RKHS*CHKLOWQ
!
      CMB=CR(LMH-1)*CF
      CMTB=-CMB+(RKHH+RKHS)*DTOZS+RHO(LMH)
      CMQB=-CMB+(RKHH+RKQS)*DTOZS+RHO(LMH)
      CMCB=-CMB+(RKHH     )*DTOZS+RHO(LMH)
!
      RSTB=-RST(LMH-1)*CF+RKCT(LMH-1)*DTOZS+THE(LMH)*RHO(LMH)
      RSQB=-RSQ(LMH-1)*CF+Q(LMH)  *RHO(LMH)
      RSCB=-RSC(LMH-1)*CF+CWM(LMH)*RHO(LMH)
!----------------------------------------------------------------------
      THE(LMH)=(DTOZS*RKHS*THZ0+RSTB)/CMTB
      Q(LMH)  =(DTOZS*RKQS*QZ0 +RSQB)/CMQB
      CWM(LMH)=(                RSCB)/CMCB
!----------------------------------------------------------------------
      DO K=LMH-1,1,-1
        RCML=1./CM(K)
        THE(K)=(-CR(K)*THE(K+1)+RST(K))*RCML
        Q(K)  =(-CR(K)*  Q(K+1)+RSQ(K))*RCML
        CWM(K)=(-CR(K)*CWM(K+1)+RSC(K))*RCML
      ENDDO
!----------------------------------------------------------------------
!
      END SUBROUTINE VDIFH
!
!---------------------------------------------------------------------
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!---------------------------------------------------------------------
      SUBROUTINE VDIFV(LMH,DTDIF,UZ0,VZ0,RKMS,U,V,RKM,Z,RHO           &
                      ,LM,I,J)
!     ***************************************************************
!     *                                                             *
!     *        VERTICAL DIFFUSION OF VELOCITY COMPONENTS            *
!     *                                                             *
!     ***************************************************************
!---------------------------------------------------------------------
!
      IMPLICIT NONE
!
!---------------------------------------------------------------------
      INTEGER,INTENT(IN) :: LM,I,J
!
      INTEGER,INTENT(IN) :: LMH
!
      REAL,INTENT(IN) :: RKMS,DTDIF,UZ0,VZ0
!
      REAL,DIMENSION(1:LM-1),INTENT(IN) :: RKM
      REAL,DIMENSION(1:LM),INTENT(IN) :: RHO
      REAL,DIMENSION(1:LM+1),INTENT(IN) :: Z
!
      REAL,DIMENSION(1:LM),INTENT(INOUT) :: U,V
!----------------------------------------------------------------------
!***
!***  LOCAL VARIABLES
!***
      INTEGER :: K
!
      REAL :: CF,DTOZAK,DTOZL,DTOZS,RCML,RCMVB,RHOK,RKMH
!
      REAL,DIMENSION(1:LM-1) :: CM,CR,DTOZ,RSU,RSV
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
      DO K=1,LMH-1
        DTOZ(K)=DTDIF/(Z(K)-Z(K+1))
        CR(K)=-DTOZ(K)*RKM(K)
      ENDDO
!
      RHOK=RHO(1)
      CM(1)=DTOZ(1)*RKM(1)+RHOK
      RSU(1)=U(1)*RHOK
      RSV(1)=V(1)*RHOK
!----------------------------------------------------------------------
      DO K=2,LMH-1
        DTOZL=DTOZ(K)
        CF=-DTOZL*RKM(K-1)/CM(K-1)
        RHOK=RHO(K)
        CM(K)=-CR(K-1)*CF+(RKM(K-1)+RKM(K))*DTOZL+RHOK
        RSU(K)=-RSU(K-1)*CF+U(K)*RHOK
        RSV(K)=-RSV(K-1)*CF+V(K)*RHOK
      ENDDO
!----------------------------------------------------------------------
      DTOZS=DTDIF/(Z(LMH)-Z(LMH+1))
      RKMH=RKM(LMH-1)
!
      CF=-DTOZS*RKMH/CM(LMH-1)
      RHOK=RHO(LMH)
      RCMVB=1./((RKMH+RKMS)*DTOZS-CR(LMH-1)*CF+RHOK)
      DTOZAK=DTOZS*RKMS
!----------------------------------------------------------------------
      U(LMH)=(DTOZAK*UZ0-RSU(LMH-1)*CF+U(LMH)*RHOK)*RCMVB
      V(LMH)=(DTOZAK*VZ0-RSV(LMH-1)*CF+V(LMH)*RHOK)*RCMVB
!----------------------------------------------------------------------
      DO K=LMH-1,1,-1
        RCML=1./CM(K)
        U(K)=(-CR(K)*U(K+1)+RSU(K))*RCML
        V(K)=(-CR(K)*V(K+1)+RSV(K))*RCML
      ENDDO
!----------------------------------------------------------------------
!
      END SUBROUTINE VDIFV
!
!-----------------------------------------------------------------------
!
!=======================================================================
      SUBROUTINE MYJPBL_INIT(EXCH_H,RESTART                             &
     &                      ,IDS,IDE,JDS,JDE,LM                         &
     &                      ,IMS,IME,JMS,JME                            &
     &                      ,ITS,ITE,JTS,JTE                         )
!-----------------------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------------------------------
      LOGICAL,INTENT(IN) :: RESTART
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,LM                          &
     &                     ,IMS,IME,JMS,JME                             &
     &                     ,ITS,ITE,JTS,JTE

      REAL,DIMENSION(IMS:IME,JMS:JME,1:LM),INTENT(OUT) ::    EXCH_H
!
      INTEGER :: I,J,K,ITF,JTF,KTF
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      JTF=MIN0(JTE,JDE-1)
      KTF=LM
      ITF=MIN0(ITE,IDE-1)
!
      IF(.NOT.RESTART)THEN
        DO K=1,KTF
        DO J=JTS,JTF
        DO I=ITS,ITF
!         RUBLTEN(I,J,K)=0.
!         RVBLTEN(I,J,K)=0.
!         RTHBLTEN(I,J,K)=0.
!         RQBLTEN(I,J,K)=0.
          EXCH_H(I,J,K)=0.
        ENDDO
        ENDDO
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE MYJPBL_INIT
!
      END MODULE MODULE_BL_MYJPBL
!
!-----------------------------------------------------------------------
