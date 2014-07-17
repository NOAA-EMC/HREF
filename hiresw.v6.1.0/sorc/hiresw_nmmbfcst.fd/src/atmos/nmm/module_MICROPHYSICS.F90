!-----------------------------------------------------------------------
!
      MODULE MODULE_MICROPHYSICS_NMM
!
!-----------------------------------------------------------------------
!
!***  THE MICROPHYSICS DRIVERS AND PACKAGES

!      11-06-2009 W. Wang put NAM micorphysics into a single module   
!      02-10-2010 W. Wang added wsm6 
!-----------------------------------------------------------------------
!
! HISTORY LOG:
!
!    11-06-2009 W. Wang - Put NAM/Ferrier microphysics into 
!                         a single module.  
!
!-----------------------------------------------------------------------
!
      USE MODULE_INCLUDE
!
      USE MODULE_CONSTANTS,ONLY : CICE,CLIQ,CPV,EP_1,EP_2,EPSILON,G     &
                                 ,P608,PSAT,R_D,R_V,RHOAIR0,RHOWATER    &
                                 ,SVPT0,XLF,XLV                         &
                                 ,CAPPA,CP,EPSQ
!
      USE MODULE_CONTROL,ONLY : NMMB_FINALIZE
! MP options
      USE MODULE_MP_ETANEW
      USE MODULE_MP_FER_HIRES
      USE MODULE_MP_WSM6
      USE MODULE_MP_GFS
!
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      PRIVATE
!
!      PUBLIC :: FERRIER_INIT,FPVS,GSMDRIVE,WSM3INIT
      PUBLIC :: GSMDRIVE
!
!-----------------------------------------------------------------------
!
      INTEGER :: MYPE
      REAL, PRIVATE,PARAMETER ::                                     &
!--- Physical constants follow:
           XLS=2.834E6,R_G=1./G
!
      INTEGER,PUBLIC,PARAMETER :: MICRO_RESTART=7501
!
      CONTAINS
!
!-----------------------------------------------------------------------
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!-----------------------------------------------------------------------
      SUBROUTINE GSMDRIVE(ITIMESTEP,DT,NPHS,NUM_WATER                   &
                         ,DX,DY,SM,FIS                                  &
                         ,DSG2,SGML2,PDSG1,PSGML1,PT,PD                 &
                         ,T,Q,CWM,OMGALF,WATER                          &
                         ,TRAIN,SR                                      &
                         ,F_ICE,F_RAIN,F_RIMEF                          &
                         ,P_QV,P_QC,P_QR,P_QI,P_QS,P_QG                 &
                         ,F_QV,F_QC,F_QR,F_QI,F_QS,F_QG                 &
                         ,PREC,ACPREC,AVRAIN                            &
                         ,MP_RESTART_STATE                              &
                         ,TBPVS_STATE,TBPVS0_STATE                      &
                         ,SPECIFIED,NESTED                              &
                         ,MICROPHYSICS                                  &
                         ,TP1,QP1,PSP1                                  &
                         ,IDS,IDE,JDS,JDE,LM                            &
                         ,IMS,IME,JMS,JME                               &
                         ,ITS,ITE,JTS,JTE                               &
                         ,ITS_B1,ITE_B1,JTS_B1,JTE_B1,MPRATES,D_SS)   
!***********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    GSMDRIVE    MICROPHYSICS OUTER DRIVER
!   PRGRMMR: BLACK           ORG: W/NP22     DATE: 02-03-26
!
! ABSTRACT:
!     RADIATION SERVES AS THE INTERFACE BETWEEN THE NMMB PHYSICS COMPONENT
!     AND THE WRF MICROPHYSICS DRIVER.
!
! PROGRAM HISTORY LOG:
!   02-03-26  BLACK      - ORIGINATOR
!   04-11-18  BLACK      - THREADED
!   06-07-31  BLACK      - BUILT INTO NMMB PHYSICS COMPONENT
!   08-08     JANJIC     - Synchronize WATER array and Q.
!
! USAGE: CALL GSMDRIVE FROM PHY_RUN
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM
!$$$
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
!------------------------
!***  Argument Variables
!------------------------
!
      INTEGER,INTENT(IN) :: D_SS,ITIMESTEP,NPHS,NUM_WATER               &
                           ,IDS,IDE,JDS,JDE,LM                          &
                           ,IMS,IME,JMS,JME                             &
                           ,ITS,ITE,JTS,JTE                             &
                           ,ITS_B1,ITE_B1,JTS_B1,JTE_B1                 
!
      INTEGER,INTENT(IN) :: P_QV,P_QC,P_QR,P_QI,P_QS,P_QG
!
      REAL,INTENT(IN) :: DT,DX,DY,PT
!
      REAL,DIMENSION(IMS:IME,JMS:JME,1:LM,D_SS)  :: MPRATES 
!
      REAL,DIMENSION(1:LM),INTENT(IN) :: DSG2,PDSG1,PSGML1,SGML2
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: FIS,PD,SM
!
      REAL,DIMENSION(IMS:IME,JMS:JME,1:LM),INTENT(IN) :: OMGALF
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: ACPREC,PREC      &
                                                      ,AVRAIN              !<-- Was a scalar
!
      REAL,DIMENSION(IMS:IME,JMS:JME,1:LM),INTENT(INOUT) :: CWM,Q,T     &
                                                           ,TRAIN
!
      REAL,DIMENSION(IMS:IME,JMS:JME,1:LM,NUM_WATER),INTENT(INOUT) :: WATER
!
      REAL,DIMENSION(IMS:IME,JMS:JME,1:LM),INTENT(INOUT) :: F_ICE       &
                                                           ,F_RAIN      &
                                                           ,F_RIMEF
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: SR
!
      CHARACTER(99),INTENT(IN) :: MICROPHYSICS
!
      LOGICAL,INTENT(IN) :: NESTED,SPECIFIED
!
      LOGICAL,INTENT(IN) :: F_QV,F_QC,F_QR,F_QI,F_QS,F_QG
!
!***  State Variables for ETAMPNEW Microphysics 
!
      REAL,DIMENSION(:),INTENT(INOUT) :: MP_RESTART_STATE               &
                                        ,TBPVS_STATE,TBPVS0_STATE
!*** GFS microphysics
      REAL, DIMENSION(IMS:IME,JMS:JME,1:LM), INTENT(INOUT) :: TP1,QP1
      REAL, DIMENSION(IMS:IME,JMS:JME), INTENT(INOUT)      :: PSP1
!
!---------------------
!***  Local Variables
!---------------------
!
      INTEGER :: I,IJ,J,K,KFLIP,MP_PHYSICS,N,NTSD
      INTEGER :: ITSLOC,ITELOC,JTSLOC,JTELOC 
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME) :: LOWLYR
!
      REAL :: DPL,DTPHS,PCPCOL,PDSL,PLYR,QI,QR,QW,RDTPHS,TNEW
!
      REAL,DIMENSION(1:LM) :: QL,TL
!
      REAL,DIMENSION(IMS:IME,JMS:JME) :: CUBOT,CUTOP,RAINNC,RAINNCV     &
                                        ,SNOWNC,SNOWNCV,XLAND           & 
                                        ,graupelnc,graupelncv
!
      REAL,DIMENSION(IMS:IME,JMS:JME,1:LM) :: DZ                        &
                                             ,P_PHY,PI_PHY              &
                                             ,RR,TH_PHY
!
      LOGICAL :: WARM_RAIN,F_QT
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      NTSD=ITIMESTEP
      DTPHS=NPHS*DT
      RDTPHS=1./DTPHS
!
!-- AVRAIN was a scalar but changed to a 2D array to allow for updates in ESMF
!
      DO J=JTS,JTE
      DO I=ITS,ITE
         AVRAIN(I,J)=AVRAIN(I,J)+1.
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  NOTE:  THE NMMB HAS IJK STORAGE WITH LAYER 1 AT THE TOP.
!***         THE WRF PHYSICS DRIVERS HAVE IKJ STORAGE WITH LAYER 1
!***         AT THE BOTTOM.
!-----------------------------------------------------------------------
!
!!!   DO J=JTS_B1,JTE_B1
!!!   DO I=ITS_B1,ITE_B1
!.......................................................................
!$omp parallel do                                                       &
!$omp& private(j,i,k,pdsl,kflip,dpl,plyr,ql,tl)
!.......................................................................
      DO J=JTS,JTE
      DO I=ITS,ITE
!
        PDSL=PD(I,J)
        LOWLYR(I,J)=1
        XLAND(I,J)=SM(I,J)+1.
!
!-----------------------------------------------------------------------
!***   FILL RAINNC WITH ZERO (NORMALLY CONTAINS THE NONCONVECTIVE
!***                          ACCUMULATED RAIN BUT NOT YET USED BY NMM)
!***   COULD BE OBTAINED FROM ACPREC AND CUPREC (ACPREC-CUPREC)
!-----------------------------------------------------------------------
!
        RAINNC(I,J)=0.
        SNOWNC(I,J)=0.
        graupelnc(i,j) = 0.0   ! ??
!
!-----------------------------------------------------------------------
!***  FILL THE SINGLE-COLUMN INPUT
!-----------------------------------------------------------------------
!
        DO K=LM,1,-1   ! We are moving down from the top in the flipped arrays
!
          KFLIP=K  ! do not flip for the NMMB arrays
!
          DPL=PDSG1(KFLIP)+DSG2(KFLIP)*PDSL
          PLYR=SGML2(KFLIP)*PDSL+PSGML1(KFLIP)
          TL(K)=T(I,J,KFLIP)
          QL(K)=AMAX1(Q(I,J,KFLIP),EPSQ)
!
          RR(I,J,K)=PLYR/(R_D*TL(K)*(P608*QL(K)+1.))
          PI_PHY(I,J,K)=(PLYR*1.E-5)**CAPPA
          TH_PHY(I,J,K)=TL(K)/PI_PHY(I,J,K)
          P_PHY(I,J,K)=PLYR
          DZ(I,J,K)=DPL*R_G/RR(I,J,K)
!
!-- Arrays that may be needed by other schemes?
!wang         T_PHY(I,J,K)=TL(K)
!wang         P8W(I,K,J)=P8W(I,K+1,J)+PDSG1(KFLIP)+DSG2(KFLIP)*PDSL
!wang         WMID(I,J,K)=-OMGALF(I,J,KFLIP)*CP/(G*DT)
        ENDDO    !- DO K=LM,1,-1
!wang        WMID(I,J,1)=0.   !<---  W in the top model layer must equal zero for WSM3.
      ENDDO    !- DO I=ITS,ITE
      ENDDO    !- DO J=JTS,JTE
!.......................................................................
!$omp end parallel do
!.......................................................................
!
!-----------------------------------------------------------------------
!***  SYNCHRONIZE MIXING RATIO IN WATER ARRAY WITH SPECIFIC HUMIDITY.
!-----------------------------------------------------------------------
!
!.......................................................................
!$omp parallel do                                                       &
!$omp& private(i,j,k)
!.......................................................................
      DO K=1,LM
        DO J=JMS,JME
          DO I=IMS,IME
            WATER(I,J,K,P_QV)=Q(I,J,K)/(1.-Q(I,J,K))
          ENDDO
        ENDDO
      ENDDO
!
!.......................................................................
!$omp end parallel do
!.......................................................................
!-----------------------------------------------------------------------
!
!***  CALL MICROPHYSICS
!
!-----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
!***  TRANSLATE THE MICROPHYSICS OPTIONS IN THE CONFIG FILE TO THEIR
!***  ANALOGS IN THE WRF REGISTRY SO THAT THE WRF MICROPHYSICS DRIVER
!***  REMAINS UNTOUCHED.
!-----------------------------------------------------------------------
!
!     SELECT CASE (TRIM(MICROPHYSICS))
!       CASE ('fer')
!          MP_PHYSICS=5
!       CASE ('kes')
!         MP_PHYSICS=1
!       CASE ('lin')
!         MP_PHYSICS=2
!       CASE ('wsm3')
!         MP_PHYSICS=3
!       CASE ('tho')
!         MP_PHYSICS=8
!       CASE DEFAULT
!         WRITE(0,*)' User selected MICROPHYSICS=',MICROPHYSICS
!         WRITE(0,*)' Improper selection of Microphysics scheme in GSMDRIVE'
!!!       CALL ESMF_Finalize(terminationflag=ESMF_ABORT)
!         CALL NMMB_FINALIZE
!     END SELECT
!
!---------------------------------------------------------------------
!  Check for microphysics type.  We need a clean way to
!  specify these things!
!---------------------------------------------------------------------


        ITSLOC = MAX(ITS_B1,IDS)
        ITELOC = MIN(ITE_B1,IDE-1)
        JTSLOC = MAX(JTS_B1,JDS)
        JTELOC = MIN(JTE_B1,JDE-1)

        micro_select: SELECT CASE (TRIM(MICROPHYSICS))
!
          CASE ('fer')
            CALL ETAMP_NEW(                                                   &
                   ITIMESTEP=ntsd,DT=dtphs,DX=dx,DY=dy                        &
                  ,DZ8W=dz,RHO_PHY=rr,P_PHY=p_phy,PI_PHY=pi_phy,TH_PHY=th_phy &
                  ,QV=water(ims,jms,1,p_qv)                                   &
                  ,QC=water(ims,jms,1,p_qc)                                   &
                  ,QS=water(ims,jms,1,p_qs)                                   &
                  ,QR=water(ims,jms,1,p_qr)                                   &
                  ,QT=cwm                                                     &
                  ,LOWLYR=LOWLYR,SR=SR                                        &
                  ,F_ICE_PHY=F_ICE,F_RAIN_PHY=F_RAIN                          &
                  ,F_RIMEF_PHY=F_RIMEF                                        &
                  ,RAINNC=rainnc,RAINNCV=rainncv                              &
                  ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=1,KDE=LM+1           &
                  ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=1,KME=LM             &
                  ,ITS=itsloc,ITE=iteloc, JTS=jtsloc,JTE=jteloc, KTS=1,KTE=LM &
                  ,MP_RESTART_STATE=mp_restart_state                          &
                  ,TBPVS_STATE=tbpvs_state,TBPVS0_STATE=tbpvs0_state          &
                  ,D_SS=d_ss,MPRATES=mprates                                  &
                                                                            )
          CASE ('fer_hires')
            CALL FER_HIRES(                                                   &
                   ITIMESTEP=ntsd,DT=dtphs,DX=dx,DY=dy                        &
                  ,DZ8W=dz,RHO_PHY=rr,P_PHY=p_phy,PI_PHY=pi_phy,TH_PHY=th_phy &
                  ,QV=water(ims,jms,1,p_qv)                                   &
                  ,QC=water(ims,jms,1,p_qc)                                   &
                  ,QS=water(ims,jms,1,p_qs)                                   &
                  ,QR=water(ims,jms,1,p_qr)                                   &
                  ,QT=cwm                                                     &
                  ,LOWLYR=LOWLYR,SR=SR                                        &
                  ,F_ICE_PHY=F_ICE,F_RAIN_PHY=F_RAIN                          &
                  ,F_RIMEF_PHY=F_RIMEF                                        &
                  ,RAINNC=rainnc,RAINNCV=rainncv                              &
                  ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=1,KDE=LM+1           &
                  ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=1,KME=LM             &
                  ,ITS=itsloc,ITE=iteloc, JTS=jtsloc,JTE=jteloc, KTS=1,KTE=LM &
                  ,MP_RESTART_STATE=mp_restart_state                          &
                  ,TBPVS_STATE=tbpvs_state,TBPVS0_STATE=tbpvs0_state          &
                  ,D_SS=d_ss,MPRATES=mprates)   
          CASE ('gfs')
               !  write(0,*)'before call gfsmp,cwm=',cwm(10,10,:)
               !  write(0,*)'water(p_qi)=',water(10,10,:,p_qi)
               !  write(0,*)'water(p_qc)=',water(10,10,:,p_qc)
               !  write(0,*)'water(p_qv)=',water(10,10,:,p_qv)
            CALL GFSMP(DT=dtphs,                                               &
                   dz8w=dz,rho_phy=rr,p_phy=p_phy,pi_phy=pi_phy,th_phy=th_phy, &
                   SR=SR,QT=CWM, F_ICE_PHY=F_ICE,                              &
                   RAINNC=RAINNC,RAINNCV=RAINNCV,                              &
                   WATER=WATER,P_QV=P_QV,P_QC=P_QC,P_QI=P_QI,                  &
                   NUM_WATER=NUM_WATER,                                        &
                   TP1=TP1,QP1=QP1,PSP1=PSP1,                                  &
                   IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=1,KDE=LM+1,           &
                   IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=1,KME=LM ,            &
                   ITS=itsloc,ITE=iteloc, JTS=jtsloc,JTE=jteloc, KTS=1,KTE=LM )
          CASE ('wsm6')
      !       write(0,*)'call wsm6'
             CALL wsm6(                                             &
                  TH=th_phy                                         &
                 ,Q=water(ims,jms,1,p_qv)                           &
                 ,QC=water(ims,jms,1,p_qc)                          &
                 ,QR=water(ims,jms,1,p_qr)                          &
                 ,QI=water(ims,jms,1,p_qi)                          &
                 ,QS=water(ims,jms,1,p_qs)                          &
                 ,QG=water(ims,jms,1,p_qg)                          &
                 ,DEN=rr,PII=pi_phy,P=p_phy,DELZ=dz                  &
                 ,DELT=dtphs,G=g,CPD=cp,CPV=cpv                        &
                 ,RD=r_d,RV=r_v,T0C=svpt0                           &
                 ,EP1=ep_1, EP2=ep_2, QMIN=epsilon                  &
                 ,XLS=xls, XLV0=xlv, XLF0=xlf                       &
                 ,DEN0=rhoair0, DENR=rhowater                       &
                 ,CLIQ=cliq,CICE=cice,PSAT=psat                     &
                 ,RAIN=rainnc ,RAINNCV=rainncv                      &
                 ,SNOW=snownc ,SNOWNCV=snowncv                      &
                 ,SR=sr                                             &
                 ,GRAUPEL=graupelnc ,GRAUPELNCV=graupelncv          &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=1,KDE=LM+1  &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=1,KME=LM    &
                 ,ITS=itsloc,ITE=iteloc, JTS=jtsloc,JTE=jteloc, KTS=1,KTE=LM &
                 ,D_SS=d_ss,MPRATES=mprates)    
                DO K=1,LM
                DO J=JMS,JME
                DO I=IMS,IME
                 CWM(I,J,K)=water(i,j,k,p_qc)+water(i,j,k,p_qr)+water(i,j,k,p_qi) &
                           +water(i,j,k,p_qs)+water(i,j,k,p_qg)
                ENDDO
                ENDDO
                ENDDO

          CASE DEFAULT
            WRITE(0,*)' The microphysics option does not exist: MICROPHYSICS = ',TRIM(MICROPHYSICS)
            CALL NMMB_FINALIZE

        END SELECT micro_select

!            
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!***  THE FOLLOWING MUST BE RECONCILED WHEN THREADING IS TURNED ON.
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!jaa!$omp parallel do                                                       &
!jaa!$omp& private(ij)
!     DO IJ=1,NUM_TILES
!       CALL MICROPHYSICS_ZERO_OUT(                                     &
!                    WATER,N_MOIST,CONFIG_FLAGS                         &
!                   ,IDS,IDE,JDS,JDE,KDS,KDE                            &
!                   ,IMS,IME,JMS,JME,KMS,KME                            &
!                   ,GRID%I_START(IJ),GRID%I_END(IJ)                    &
!                   ,GRID%J_START(IJ),GRID%J_END(IJ)                    &
!                   ,KTS,KTE                                       )
!     ENDDO
!
!-----------------------------------------------------------------------
!
!.......................................................................
!$omp parallel do                                                       &
!$omp& private(i,j,k,kflip,tnew)
!.......................................................................
      DO J=JTS_B1,JTE_B1
        DO K=1,LM
!wang    KFLIP=LM+1-K
        KFLIP=K  ! no flip
        DO I=ITS_B1,ITE_B1
!
!-----------------------------------------------------------------------
!***  UPDATE TEMPERATURE, SPECIFIC HUMIDITY, CLOUD WATER, AND HEATING.
!-----------------------------------------------------------------------
!
          TNEW=TH_PHY(I,J,K)*PI_PHY(I,J,K)
          TRAIN(I,J,KFLIP)=TRAIN(I,J,KFLIP)+(TNEW-T(I,J,KFLIP))*RDTPHS
          T(I,J,KFLIP)=TNEW
          Q(I,J,KFLIP)=WATER(I,J,K,P_QV)/(1.+WATER(I,J,K,P_QV)) !To s.h.
        ENDDO
        ENDDO
      ENDDO
!.......................................................................
!$omp end parallel do
!.......................................................................
!
!-----------------------------------------------------------------------
!***  UPDATE PRECIPITATION
!-----------------------------------------------------------------------
!
!jaa!$omp parallel do                                                       &
!jaa!$omp& private(i,j,pcpcol)
      DO J=JTS_B1,JTE_B1
      DO I=ITS_B1,ITE_B1
        PCPCOL=RAINNCV(I,J)*1.E-3
        PREC(I,J)=PREC(I,J)+PCPCOL
        ACPREC(I,J)=ACPREC(I,J)+PCPCOL
!
! NOTE: RAINNC IS ACCUMULATED INSIDE MICROPHYSICS BUT NMM ZEROES IT OUT ABOVE
!       SINCE IT IS ONLY A LOCAL ARRAY FOR NOW
!
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE GSMDRIVE
!-----------------------------------------------------------------------
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!-----------------------------------------------------------------------

      END MODULE MODULE_MICROPHYSICS_NMM

!-----------------------------------------------------------------------
