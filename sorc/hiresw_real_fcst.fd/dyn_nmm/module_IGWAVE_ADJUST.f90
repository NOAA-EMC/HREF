



















      MODULE MODULE_IGWAVE_ADJUST


      USE MODULE_MODEL_CONSTANTS

      USE MODULE_MPP,ONLY: MYPE








      INTEGER :: KSMUD=0,LNSDT=7



      CONTAINS


      SUBROUTINE PFDHT(NTSD,LAST_TIME,PT,DETA1,DETA2,PDTOP,RES,FIS      &
     &                ,HYDRO,SIGMA,FIRST,DX,DY                          &
     &                ,HBM2,VBM2,VBM3                                   &
     &                ,FDIV,FCP,WPDAR,DFL,CPGFU,CPGFV                   &
     &                ,PD,PDSL,T,Q,U,V,CWM,OMGALF,PINT,DWDT             &
     &                ,RTOP,DIV,FEW,FNS,FNE,FSE                         &
     &                ,IHE,IHW,IVE,IVW                                  &
     &                ,IDS,IDE,JDS,JDE,KDS,KDE                          &
     &                ,IMS,IME,JMS,JME,KMS,KME                          &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)














































      IMPLICIT NONE


      INCLUDE "mpif.h"


      LOGICAL,INTENT(IN) :: FIRST,HYDRO
      INTEGER,INTENT(IN) :: SIGMA

      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE

      INTEGER, DIMENSION(JMS:JME),INTENT(IN) :: IHE,IHW,IVE,IVW

      INTEGER,INTENT(IN) :: NTSD
      LOGICAL,INTENT(IN) :: LAST_TIME

      REAL,INTENT(IN) :: CPGFV,DY,PDTOP,PT

      REAL,DIMENSION(KMS:KME-1),INTENT(IN) :: DETA1,DETA2

      REAL,DIMENSION(KMS:KME),INTENT(IN) :: DFL

      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: CPGFU,DX,FCP,FDIV   &
     &                                             ,PD,FIS,RES,WPDAR    &
     &                                             ,HBM2,VBM2,VBM3

      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(IN) :: CWM,DWDT    &
     &                                                     ,Q,T

      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(IN) :: PINT

      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT) :: DIV      &
     &                                                        ,OMGALF   &
     &                                                        ,RTOP,U,V

      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(OUT) :: FEW,FNS    &
     &                                                      ,FNE,FSE

      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: PDSL





      INTEGER :: I,J,K, LOC_NPTS, GLB_NPTS, IRET

      REAL :: SLP_STD=101300.0

      REAL :: APELP,DFI,DCNEK,DCSEK,DPFNEK,DPFSEK,DPNEK,DPSEK           &
     &       ,EDIV,FIUP,PRSFRC,PVNEK,PVSEK,RTOPP,VM

      REAL,DIMENSION(ITS-5:ITE+5,JTS-5:JTE+5) :: ADPDNE,ADPDSE          &
     &                                          ,ADPDX,ADPDY,APEL       &
     &                                          ,CNE,CSE,DFDZ,DPDE      &
     &                                          ,DPFEW,DPFNS            &
     &                                          ,FILO,FIM,HM            &
     &                                          ,PCEW,PCNE,PCNS,PCSE    &
     &                                          ,PCXC,PEW,PNE,PNS       &
     &                                          ,PPNE,PPSE,PSE          &
     &                                          ,RDPD,RDPDX,RDPDY       &
     &                                          ,TEW,TNE,TNS,TSE        &
     &                                          ,UDY,VDX






















































!$omp parallel do
      DO K=KMS,KME
        DO J=JMS,JME
        DO I=IMS,IME
          OMGALF(I,J,K)=0.
          DIV(I,J,K)=0.
        ENDDO
        ENDDO
      ENDDO

!$omp parallel do
      DO J=JMS,JME
      DO I=IMS,IME
        PDSL(I,J)=0.
      ENDDO
      ENDDO

!$omp parallel do
      DO J=JTS-5,JTE+5
      DO I=ITS-5,ITE+5
        ADPDNE(I,J)=0.
        ADPDSE(I,J)=0.
        ADPDX(I,J)=0.
        ADPDY(I,J)=0.
        APEL(I,J)=0.
        CNE (I,J)=0.
        CSE (I,J)=0.
        DFDZ(I,J)=0.
        DPDE(I,J)=0.
        DPFEW(I,J)=0.
        DPFNS(I,J)=0.
        FILO(I,J)=0.
        FIM (I,J)=0.
        HM (I,J)=0.
        PCEW(I,J)=0.
        PCNE(I,J)=0.
        PCNS(I,J)=0.
        PCSE(I,J)=0.
        PCXC(I,J)=0.
        PEW (I,J)=0.
        PNE (I,J)=0.
        PNS (I,J)=0.
        PPNE(I,J)=0.
        PPSE(I,J)=0.
        PSE (I,J)=0.
        RDPD(I,J)=0.
        RDPDX(I,J)=0.
        RDPDY(I,J)=0.
        TEW (I,J)=0.
        TNE (I,J)=0.
        TNS (I,J)=0.
        TSE (I,J)=0.
        UDY (I,J)=0.
        VDX (I,J)=0.
      ENDDO
      ENDDO

      IF(SIGMA==1)THEN
!$omp parallel do
        DO J=max(jds+(0),jts-(4)),min(jde-(0),jte+(4))
        DO I=max(ids+(0),its-(4)),min(ide-(0),ite+(4))
          FILO(I,J)=FIS(I,J)
          PDSL(I,J)=PD(I,J)
        ENDDO
        ENDDO
      ELSE
!$omp parallel do
        DO J=max(jds+(0),jts-(4)),min(jde-(0),jte+(4))
        DO I=max(ids+(0),its-(4)),min(ide-(0),ite+(4))
          FILO(I,J)=0.0
          PDSL(I,J)=RES(I,J)*PD(I,J)
        ENDDO
        ENDDO
      ENDIF

      PRSFRC=PDTOP/(SLP_STD-PT)






!$omp parallel do                                                       &
!$omp& private(adpdne,adpdse,adpdx,adpdy,                               &
!$omp&         apel,cne,cse,dcnek,dcsek,dfdz,dpde,dpfew,dpfnek,         &
!$omp&         dpfns,dpfsek,dpnek,ediv,few,fne,fns,fse,hm,              &
!$omp&         pcew,pcne,pcns,pcse,pcxc,pew,pne,pns,ppne,ppse,          &
!$omp&         pse,pvnek,pvsek,rdpd,rdpdx,rdpdy,tew,tne,tns,tse,        &
!$omp&         udy,vdx,vm)


       main_integration : DO K=KTS,KTE







        DO J=max(jds+(0),jts-(4)),min(jde-(0),jte+(4))
        DO I=max(ids+(0),its-(4)),min(ide-(0),ite+(4))

          HM(I,J)=HBM2(I,J)

          APELP=(PINT(I,J,K+1)+PINT(I,J,K))*0.5
          RTOPP=(Q(I,J,K)*P608-CWM(I,J,K)+1.)*T(I,J,K)*R_D/APELP
          DFI=RTOPP*(DETA1(K)*PDTOP+DETA2(K)*PDSL(I,J))

          APEL(I,J)=APELP
          RTOP(I,J,K)=RTOPP
          DFDZ(I,J)=RTOPP

          FIUP=FILO(I,J)+DFI
          FIM(I,J)=FILO(I,J)+FIUP










10281   format(' k=',i2,' q=',z8,' cwm=',z8,' t=',z8,' apelp=',z8,' pdsl=',z8)

          FILO(I,J)=FIUP

        ENDDO
        ENDDO



        DO J=max(jds+(0),jts-(4)),min(jde-(0),jte+(4))
        DO I=max(ids+(0),its-(4)),min(ide-(0),ite+(4))
          DPDE(I,J)=DETA1(K)*PDTOP+DETA2(K)*PDSL(I,J)
        ENDDO
        ENDDO

        DO J=max(jds+(0),jts-(0)),min(jde-(0),jte+(0))
        DO I=max(ids+(0),its-(0)),min(ide-(0),ite+(0))
          RDPD(I,J)=1./DPDE(I,J)
        ENDDO
        ENDDO

        DO J=max(jds+(1),jts-(3)),min(jde-(1),jte+(3))
        DO I=max(ids+(0),its-(3)),min(ide-(0),ite+(3))
          ADPDX(I,J)=DPDE(I+IVW(J),J)+DPDE(I+IVE(J),J)
          ADPDY(I,J)=DPDE(I,J+1)+DPDE(I,J-1)
          RDPDX(I,J)=1./ADPDX(I,J)
          RDPDY(I,J)=1./ADPDY(I,J)
        ENDDO
        ENDDO





        DO J=max(jds+(0),jts-(3)),min(jde-(1),jte+(3))
        DO I=max(ids+(0),its-(3)),min(ide-(0),ite+(3))
          ADPDNE(I,J)=DPDE(I+IHE(J),J+1)+DPDE(I,J)
          PNE(I,J)=(FIM (I+IHE(J),J+1)-FIM (I,J))                       &
     &            *(DWDT(I+IHE(J),J+1,K)+DWDT(I,J,K))





          PPNE(I,J)=PNE(I,J)*ADPDNE(I,J)
          CNE(I,J)=(DFDZ(I+IHE(J),J+1)+DFDZ(I,J))*2.                    &
     &            *(APEL(I+IHE(J),J+1)-APEL(I,J))



          PCNE(I,J)=CNE(I,J)*ADPDNE(I,J)
        ENDDO
        ENDDO

        DO J=max(jds+(1),jts-(3)),min(jde-(0),jte+(3))
        DO I=max(ids+(0),its-(3)),min(ide-(0),ite+(3))
          ADPDSE(I,J)=DPDE(I+IHE(J),J-1)+DPDE(I,J)
          PSE(I,J)=(FIM (I+IHE(J),J-1)-FIM (I,J))                       &
     &            *(DWDT(I+IHE(J),J-1,K)+DWDT(I,J,K))





58391   format(' pse=',z8,' fim=',2(1x,z8),' dwdt=',2(1x,z8),' ihe=',i2)

          PPSE(I,J)=PSE(I,J)*ADPDSE(I,J)
          CSE(I,J)=(DFDZ(I+IHE(J),J-1)+DFDZ(I,J))*2.                    &
     &            *(APEL(I+IHE(J),J-1)-APEL(I,J))



          PCSE(I,J)=CSE(I,J)*ADPDSE(I,J)
        ENDDO
        ENDDO





        DO J=max(jds+(1),jts-(1)),min(jde-(1),jte+(1))
        DO I=max(ids+(0),its-(1)),min(ide-(0),ite+(1))



72451   format(' pne=',2(1x,z8),' pse=',2(1x,z8),' ivw=',i2)
72452   format(' cne=',2(1x,z8),' cse=',2(1x,z8))

          PCXC(I,J)=VBM3(I,J)*                                          &
     &             (PNE(I+IVW(J),J)+CNE(I+IVW(J),J)                     &
     &             +PSE(I+IVW(J),J)+CSE(I+IVW(J),J)                     &
     &             -PNE(I,J-1)-CNE(I,J-1)                               &
     &             -PSE(I,J+1)-CSE(I,J+1))









        ENDDO
        ENDDO



        DO J=max(jds+(2),jts-(0)),min(jde-(2),jte+(0))
        DO I=max(ids+(1),its-(0)),min(ide-(1),ite+(0))



76501   format(' deta1=',z8,' deta2=',z8,' prsfrc=',z8,' wpdar=',z8,' ihe=',i2,' ihw=',i2)
76502   format(' pcxc=',4(1x,z8))

          DIV(I,J,K)=(DETA1(K)*PRSFRC                                   &   
     &               +DETA2(K)*(1.-PRSFRC))*WPDAR(I,J)                  &
     &              *(PCXC(I+IHE(J),J)-PCXC(I,J+1)                      &
     &               +PCXC(I+IHW(J),J)-PCXC(I,J-1))



        ENDDO
        ENDDO





        DO J=max(jds+(1),jts-(2)),min(jde-(1),jte+(2))
        DO I=max(ids+(0),its-(2)),min(ide-(0),ite+(3))
          DPNEK=PNE(I+IVW(J),J)+PNE(I,J-1)
          DPSEK=PSE(I+IVW(J),J)+PSE(I,J+1)
          PEW(I,J)=DPNEK+DPSEK
          PNS(I,J)=DPNEK-DPSEK
          DCNEK=CNE(I+IVW(J),J)+CNE(I,J-1)
          DCSEK=CSE(I+IVW(J),J)+CSE(I,J+1)
          PCEW(I,J)=(DCNEK+DCSEK)*ADPDX(I,J)
          PCNS(I,J)=(DCNEK-DCSEK)*ADPDY(I,J)
        ENDDO
        ENDDO



        IF(.NOT.FIRST)THEN     





          DO J=max(jds+(2),jts-(2)),min(jde-(2),jte+(2))
          DO I=max(ids+(0),its-(2)),min(ide-(1),ite+(2))
            DPFNEK=((PPNE(I+IVW(J),J)+PPNE(I,J-1))                      &
     &             +(PCNE(I+IVW(J),J)+PCNE(I,J-1)))
            DPFNEK=DPFNEK+DPFNEK
            DPFSEK=((PPSE(I+IVW(J),J)+PPSE(I,J+1))                      &
     &             +(PCSE(I+IVW(J),J)+PCSE(I,J+1)))
            DPFSEK=DPFSEK+DPFSEK
            DPFEW(I,J)=DPFNEK+DPFSEK
            DPFNS(I,J)=DPFNEK-DPFSEK
          ENDDO
          ENDDO



          DO J=max(jds+(2),jts-(3)),min(jde-(2),jte+(3))
          DO I=max(ids+(0),its-(2)),min(ide-(1),ite+(2))
            VM=VBM2(I,J)
            U(I,J,K)=(((DPFEW(I,J)+PCEW(I,J))*RDPDX(I,J)                &
     &                 +PEW(I,J))*CPGFU(I,J))*VM+U(I,J,K)
            V(I,J,K)=(((DPFNS(I,J)+PCNS(I,J))*RDPDY(I,J)                &
     &                 +PNS(I,J))*CPGFV)*VM+V(I,J,K)
          ENDDO
          ENDDO



        ENDIF    




        IF(.NOT.LAST_TIME)THEN    





          DO J=max(jds+(1),jts-(2)),min(jde-(1),jte+(2))
          DO I=max(ids+(0),its-(2)),min(ide-(0),ite+(3))
            UDY(I,J)=DY*U(I,J,K)
            FEW(I,J,K)=UDY(I,J)*ADPDX(I,J)
            TEW(I,J)=UDY(I,J)*PCEW(I,J)
            VDX(I,J)=DX(I,J)*V(I,J,K)


77601   format(' udy=',z8,' dy=',z8,' u=',z8)

            FNS(I,J,K)=VDX(I,J)*ADPDY(I,J)
            TNS(I,J)=VDX(I,J)*PCNS(I,J)
          ENDDO
          ENDDO





          DO J=max(jds+(1),jts-(1)),min(jde-(2),jte+(1))
          DO I=max(ids+(0),its-(1)),min(ide-(1),ite+(1))
            PVNEK=(UDY(I+IHE(J),J)+VDX(I+IHE(J),J))                     &
     &           +(UDY(I,J+1)+VDX(I,J+1))
            FNE(I,J,K)=PVNEK*ADPDNE(I,J)



33781   format(' fne=',z8,' dpdne=',2(1x,z8),' ihe=',i2)
33782   format(' udy=',2(1x,z8),' vdx=',2(1x,z8))

            TNE(I,J)=PVNEK*PCNE(I,J)*2.
          ENDDO
          ENDDO

          DO J=max(jds+(2),jts-(1)),min(jde-(1),jte+(1))
          DO I=max(ids+(0),its-(1)),min(ide-(1),ite+(1))
            PVSEK=(UDY(I+IHE(J),J)-VDX(I+IHE(J),J))                     &
     &           +(UDY(I,J-1)-VDX(I,J-1))
            FSE(I,J,K)=PVSEK*ADPDSE(I,J)
            TSE(I,J)=PVSEK*PCSE(I,J)*2.
          ENDDO
          ENDDO





          DO J=max(jds+(2),jts-(0)),min(jde-(2),jte+(0))
          DO I=max(ids+(1),its-(0)),min(ide-(1),ite+(0))
            OMGALF(I,J,K)=(TEW(I+IHE(J),J)+TEW(I+IHW(J),J)              &
     &                    +TNS(I,J+1)     +TNS(I,J-1)                   &
     &                    +TNE(I,J)       +TNE(I+IHW(J),J-1)            &
     &                    +TSE(I,J)       +TSE(I+IHW(J),J+1))           &
     &                    *RDPD(I,J)*FCP(I,J)*HM(I,J)





36311   format(' PFDHT div=',z8,' fdiv=',z8,' ihe=',i2,' ihw=',i2)
36312   format(' few=',2(1x,z8),' fns=',2(1x,z8))
36313   format(' fne=',2(1x,z8),' fse=',2(1x,z8))

            EDIV=(FEW(I+IHE(J),J,K)  +FNS(I,J+1,K)                      &
                 +FNE(I,J,K)         +FSE(I,J,K)                        &
                -(FEW(I+IHW(J),J,K)  +FNS(I,J-1,K)                      &
                 +FNE(I+IHW(J),J-1,K)+FSE(I+IHW(J),J+1,K)))*FDIV(I,J)

            DIV(I,J,K)=(EDIV+DIV(I,J,K))*HM(I,J)

          ENDDO
          ENDDO




        ENDIF   



      ENDDO main_integration





      END SUBROUTINE PFDHT




      SUBROUTINE PDTE(                                                  &
     &                GRID,MYPE,MPI_COMM_COMP,                          &
     &                NTSD,DT,PT,ETA2,RES,HYDRO,HBM2                    &
     &               ,PD,PDSL,PDSLO                                     &
     &               ,PETDT,DIV,PSDT                                    &
     &               ,IHE,IHW,IVE,IVW                                   &                 
     &               ,IDS,IDE,JDS,JDE,KDS,KDE                           &
     &               ,IMS,IME,JMS,JME,KMS,KME                           &
     &               ,ITS,ITE,JTS,JTE,KTS,KTE)












































      USE module_domain
      USE module_dm

      IMPLICIT NONE

      INCLUDE "mpif.h"
      TYPE (DOMAIN) :: GRID
      INTEGER,INTENT(IN) :: MYPE,MPI_COMM_COMP

      LOGICAL,INTENT(IN) :: HYDRO

      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
                           ,IMS,IME,JMS,JME,KMS,KME                     &
                           ,ITS,ITE,JTS,JTE,KTS,KTE

      INTEGER,DIMENSION(JMS:JME),INTENT(IN) :: IHE,IHW,IVE,IVW

      INTEGER,INTENT(IN) :: NTSD

      REAL,INTENT(IN) :: DT,PT

      REAL,DIMENSION(KMS:KME),INTENT(IN) :: ETA2

      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: RES,HBM2   

      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT) :: DIV

      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: PD,PDSL

      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT) :: PETDT

      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: PDSLO,PSDT





      INTEGER :: I,IHH,IHL,IX,J,JHH,JHL,JX,K,KS,NSMUD
      INTEGER :: MY_IS_GLB,MY_IE_GLB,MY_JS_GLB,MY_JE_GLB
      INTEGER :: LOC_NPTS, GLB_NPTS
      INTEGER :: IPS,IPE,JPS,JPE,KPS,KPE, I_BY_J, IRET







      REAL :: PETDTL, TASK_CHANGE, GLOBAL_CHANGE
      REAL :: LOC_N2, GLB_N2

      REAL,DIMENSION(ITS-5:ITE+5,JTS-5:JTE+5) :: HBMS,PNE,PRET,PSE






      DO J=JMS,JME
      DO I=IMS,IME
        PDSLO(I,J)=0.
      ENDDO
      ENDDO

      MY_IS_GLB=ITS
      MY_IE_GLB=ITE
      MY_JS_GLB=JTS
      MY_JE_GLB=JTE































!$omp parallel do                                                       &
!$omp& private(i,j,k)
      DO K=KTE-1,KTS,-1
        DO J=max(jds+(0),jts-(2)),min(jde-(0),jte+(2))
        DO I=max(ids+(0),its-(2)),min(ide-(0),ite+(2))
          DIV(I,J,K)=DIV(I,J,K+1)+DIV(I,J,K)
        ENDDO
        ENDDO
      ENDDO





!$omp parallel do                                                       &
!$omp& private(i,j)
      DO J=max(jds+(0),jts-(2)),min(jde-(0),jte+(2))
      DO I=max(ids+(0),its-(2)),min(ide-(0),ite+(2))
        PSDT(I,J)=-DIV(I,J,KTS)
        PDSLO(I,J)=PDSL(I,J)
      ENDDO
      ENDDO

      DO J=JMS,JME
      DO I=IMS,IME
        PDSL(I,J)=0.
      ENDDO
      ENDDO


                                                
                                                
                                                


!$omp parallel do                                                       &
!$omp& private(i,j)
	TASK_CHANGE=0.
      DO J=max(jds+(0),jts-(2)),min(jde-(0),jte+(2))
      DO I=max(ids+(0),its-(2)),min(ide-(0),ite+(2))




        PD(I,J)=PSDT(I,J)*DT+PD(I,J)
        PRET(I,J)=PSDT(I,J)*RES(I,J)
        PDSL(I,J)=PD(I,J)*RES(I,J)


      ENDDO
      ENDDO













	





!$omp parallel do                                                       &
!$omp& private(i,j,k)
      DO K=KTE-1,KTS,-1
        DO J=max(jds+(0),jts-(2)),min(jde-(0),jte+(2))
        DO I=max(ids+(0),its-(2)),min(ide-(0),ite+(2))
          PETDT(I,J,K)=-(PRET(I,J)*ETA2(K+1)+DIV(I,J,K+1))              &
     &                  *HBM2(I,J)
        ENDDO
        ENDDO
      ENDDO





      nonhydrostatic_smoothing: IF(.NOT.HYDRO.AND.KSMUD.GT.0)THEN

        NSMUD=KSMUD

        DO J=max(jds+(0),jts-(0)),min(jde-(0),jte+(0))
        DO I=max(ids+(0),its-(0)),min(ide-(0),ite+(0))
          HBMS(I,J)=HBM2(I,J)
        ENDDO
        ENDDO

        JHL=LNSDT
        JHH=JDE-JHL+1

!$omp parallel do                                                       &
!$omp& private(i,ihh,ihl,ix,j,jx)
        DO J=JHL,JHH
          IF(J.GE.MY_JS_GLB.AND.J.LE.MY_JE_GLB)THEN
            IHL=JHL/2+1
            IHH=IDE-IHL+MOD(J,2)

            DO I=IHL,IHH
              IF(I.GE.MY_IS_GLB.AND.I.LE.MY_IE_GLB)THEN
                IX=I    
                JX=J    
                HBMS(IX,JX)=0.
              ENDIF
            ENDDO

          ENDIF
        ENDDO







        DO KS=1,NSMUD






!$omp parallel do                                                       &
!$omp& private(i,j,k,petdtl,pne,pse)

          DO K=KTS+1,KTE

            DO J=max(jds+(0),jts-(1)),min(jde-(1),jte+(1))
            DO I=max(ids+(0),its-(1)),min(ide-(1),ite+(1))
              PNE(I,J)=PETDT(I+IHE(J),J+1,K)-PETDT(I,J,K)
            ENDDO
            ENDDO

            DO J=max(jds+(1),jts-(1)),min(jde-(0),jte+(1))
            DO I=max(ids+(0),its-(1)),min(ide-(1),ite+(1))
              PSE(I,J)=PETDT(I+IHE(J),J-1,K)-PETDT(I,J,K)
            ENDDO
            ENDDO

            DO J=max(jds+(2),jts-(0)),min(jde-(2),jte+(0))
            DO I=max(ids+(1),its-(0)),min(ide-(1),ite+(0))
              PETDTL=(PNE(I,J)-PNE(I+IHW(J),J-1)                        &
     &               +PSE(I,J)-PSE(I+IHW(J),J+1))*HBM2(I,J)
              PETDT(I,J,K)=PETDTL*HBMS(I,J)*0.125+PETDT(I,J,K)
            ENDDO
            ENDDO

          ENDDO








CALL HALO_NMM_E_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )



        ENDDO  



      ENDIF nonhydrostatic_smoothing



      END SUBROUTINE PDTE




      SUBROUTINE VTOA(                                                  &
     &                GRID,                                             &
     &                NTSD,DT,PT,ETA2                                   &
     &               ,HBM2,EF4T                                         &
     &               ,T,DWDT,RTOP,OMGALF                                &
     &               ,PINT,DIV,PSDT,RES                                 &
     &               ,IHE,IHW,IVE,IVW                                   &                 
     &               ,IDS,IDE,JDS,JDE,KDS,KDE                           &
     &               ,IMS,IME,JMS,JME,KMS,KME                           &
     &               ,ITS,ITE,JTS,JTE,KTS,KTE)


































      USE MODULE_DOMAIN
      USE MODULE_DM

      IMPLICIT NONE


      TYPE (DOMAIN) :: GRID


      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
                           ,IMS,IME,JMS,JME,KMS,KME                     &
                           ,ITS,ITE,JTS,JTE,KTS,KTE

      INTEGER,DIMENSION(JMS:JME),INTENT(IN) :: IHE,IHW,IVE,IVW

      INTEGER,INTENT(IN) :: NTSD

      REAL,INTENT(IN) :: DT,EF4T,PT

      REAL,DIMENSION(KMS:KME),INTENT(IN) :: ETA2

      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: HBM2,PSDT,RES

      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(IN) :: DIV,DWDT    &
     &                                                     ,RTOP

      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT) :: OMGALF,T  

      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT) :: PINT





      INTEGER :: I,J,K

      REAL,DIMENSION(ITS-5:ITE+5,JTS-5:JTE+5) :: PRET,TPM

      REAL :: DWDTP,RHS,TPMP







!$omp parallel do                                                       &
!$omp& private(i,j)
      DO J=max(jds+(0),jts-(2)),min(jde-(0),jte+(2))
      DO I=max(ids+(0),its-(2)),min(ide-(0),ite+(2))
        PINT(I,J,KTE+1)=PT
        TPM(I,J)=PT+PINT(I,J,KTE)
        PRET(I,J)=PSDT(I,J)*RES(I,J)
      ENDDO
      ENDDO





!$omp parallel do                                                       &
!$omp& private(dwdtp,i,j,rhs,tpmp)
      DO J=max(jds+(0),jts-(0)),min(jde-(0),jte+(0))
      DO I=max(ids+(0),its-(0)),min(ide-(0),ite+(0))
        DWDTP=DWDT(I,J,KTE)
        TPMP=PINT(I,J,KTE)+PINT(I,J,KTE-1)

        RHS=-DIV(I,J,KTE)*RTOP(I,J,KTE)*DWDTP*EF4T
        OMGALF(I,J,KTE)=OMGALF(I,J,KTE)+RHS
        T(I,J,KTE)=OMGALF(I,J,KTE)*HBM2(I,J)+T(I,J,KTE)
        PINT(I,J,KTE)=PRET(I,J)*(ETA2(KTE+1)+ETA2(KTE))*DWDTP*DT        &
     &             +TPM(I,J)-PINT(I,J,KTE+1)

        TPM(I,J)=TPMP
      ENDDO
      ENDDO

!$omp parallel do                                                       &
!$omp& private(dwdtp,i,j,k,rhs,tpmp)
      DO K=KTE-1,KTS+1,-1
        DO J=max(jds+(0),jts-(0)),min(jde-(0),jte+(0))
        DO I=max(ids+(0),its-(0)),min(ide-(0),ite+(0))
          DWDTP=DWDT(I,J,K)
          TPMP=PINT(I,J,K)+PINT(I,J,K-1)

          RHS=-(DIV(I,J,K+1)+DIV(I,J,K))*RTOP(I,J,K)*DWDTP*EF4T
          OMGALF(I,J,K)=OMGALF(I,J,K)+RHS
          T(I,J,K)=OMGALF(I,J,K)*HBM2(I,J)+T(I,J,K)
          PINT(I,J,K)=PRET(I,J)*(ETA2(K+1)+ETA2(K))*DWDTP*DT            &
     &               +TPM(I,J)-PINT(I,J,K+1)

          TPM(I,J)=TPMP
        ENDDO
        ENDDO
      ENDDO

!$omp parallel do                                                       &
!$omp& private(dwdtp,i,j,rhs)
      DO J=max(jds+(0),jts-(0)),min(jde-(0),jte+(0))
      DO I=max(ids+(0),its-(0)),min(ide-(0),ite+(0))

        DWDTP=DWDT(I,J,KTS)




28361   format(' t=',z8,' omgalf=',z8,' rtop=',z8,' dwdtp=',z8)
28362   format(' div=',2(1x,z8),' ef4t=',z8)

        RHS=-(DIV(I,J,KTS+1)+DIV(I,J,KTS))*RTOP(I,J,KTS)*DWDTP*EF4T
        OMGALF(I,J,KTS)=OMGALF(I,J,KTS)+RHS
        T(I,J,KTS)=OMGALF(I,J,KTS)*HBM2(I,J)+T(I,J,KTS)
        PINT(I,J,KTS)=PRET(I,J)*(ETA2(KTS+1)+ETA2(KTS))*DWDTP*DT        &
     &                 +TPM(I,J)-PINT(I,J,KTS+1)
      ENDDO
      ENDDO


      END SUBROUTINE VTOA



      SUBROUTINE DDAMP(NTSD,DT,DETA1,DETA2,PDSL,PDTOP,DIV,HBM2          &
     &                ,T,U,V,DDMPU,DDMPV                                &
     &                ,IHE,IHW,IVE,IVW                                  &              
     &                ,IDS,IDE,JDS,JDE,KDS,KDE                          &
     &                ,IMS,IME,JMS,JME,KMS,KME                          &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)








































      IMPLICIT NONE


      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE

      INTEGER,DIMENSION(JMS:JME),INTENT(IN) :: IHE,IHW,IVE,IVW

      INTEGER,INTENT(IN) :: NTSD

      REAL,INTENT(IN) :: DT,PDTOP

      REAL,DIMENSION(KMS:KME-1),INTENT(IN) :: DETA1,DETA2

      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: DDMPU,DDMPV         &
     &                                             ,HBM2,PDSL

      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT) :: DIV,T    &
     &                                                        ,U,V





      INTEGER :: I,J,K

      REAL :: FCIM,FCXM,RDPDX,RDPDY

      REAL,DIMENSION(ITS-5:ITE+5,JTS-5:JTE+5) :: DIVE,DPDE,PDE          &
     &                                          ,XDIVX,XDIVY





!$omp parallel do                                                       &
!$omp& private(i,j)
      DO J=JTS-5,JTE+5
      DO I=ITS-5,ITE+5
        PDE(I,J)=0.
        DPDE(I,J)=0.
        XDIVX(I,J)=0.
        XDIVY(I,J)=0.
      ENDDO
      ENDDO



      FCXM=1.

      DO J=max(jds+(0),jts-(2)),min(jde-(0),jte+(2))
      DO I=max(ids+(0),its-(2)),min(ide-(0),ite+(2))
        PDE (I,J)=PDSL(I,J)+PDTOP
        DIVE(I,J)=0.
      ENDDO
      ENDDO

      DO K=KTS,KTE
!$omp parallel do
        DO J=max(jds+(0),jts-(2)),min(jde-(0),jte+(2))
        DO I=max(ids+(0),its-(2)),min(ide-(0),ite+(2))
          DIVE(I,J)=DIV(I,J,K)*HBM2(I,J)+DIVE(I,J)
        ENDDO
        ENDDO
      ENDDO

!$omp parallel do                                                       &
!$omp& private(i,j,rdpdx,rdpdy)
      DO J=max(jds+(2),jts-(0)),min(jde-(2),jte+(0))
      DO I=max(ids+(1),its-(1)),min(ide-(1),ite+(1))
        RDPDX=DDMPU(I,J)*FCXM                                           &
     &       /(PDE(I+IVW(J),J)  +PDE(I+IVE(J),J))    
        RDPDY=DDMPV(I,J)*FCXM                                           &
     &       /(PDE(I       ,J-1)+PDE(I     ,J+1))

        XDIVX(I,J)=(DIVE(I+IVE(J),J  )-DIVE(I+IVW(J),J  ))*RDPDX
        XDIVY(I,J)=(DIVE(I       ,J+1)-DIVE(I       ,J-1))*RDPDY
      ENDDO
      ENDDO



      FCIM=1.

!$omp parallel do                                                       &
!$omp& private(dpde,i,j,k,rdpdx,rdpdy)

      DO K=KTS,KTE



        DO J=max(jds+(0),jts-(2)),min(jde-(0),jte+(2))
        DO I=max(ids+(0),its-(1)),min(ide-(0),ite+(1))
          DPDE(I,J)=DETA1(K)*PDTOP+DETA2(K)*PDSL(I,J)
          DIV(I,J,K)=DIV(I,J,K)*HBM2(I,J)
        ENDDO
        ENDDO

        DO J=max(jds+(2),jts-(0)),min(jde-(2),jte+(0))
        DO I=max(ids+(1),its-(1)),min(ide-(1),ite+(1))
          RDPDX=DDMPU(I,J)*FCIM                                        &
     &         /(DPDE(I+IVW(J),J)  +DPDE(I+IVE(J),J))
          RDPDY=DDMPV(I,J)*FCIM                                        &
     &         /(DPDE(I       ,J-1)+DPDE(I       ,J+1))
          U(I,J,K)=((DIV(I+IVE(J),J,K  )-DIV(I+IVW(J),J,K  ))*RDPDX    &
     &             +XDIVX(I,J))+U(I,J,K)
          V(I,J,K)=((DIV(I       ,J+1,K)-DIV(I       ,J-1,K))*RDPDY    &
     &             +XDIVY(I,J))+V(I,J,K)
	if ( V(I,J,K) .ne. V(I,J,K) ) then
	write(0,*) 'bad V from DDAMP: ', I,J,K, V(I,J,K)
	write(0,*) 'DIV(I,J+1,K): ', DIV(I,J+1,K)
	write(0,*) 'DIV(I,J-1,K): ', DIV(I,J-1,K)
	write(0,*) 'RDPDY, XDIVY(I,J): ', RDPDY, XDIVY(I,J)
	endif
        ENDDO
        ENDDO



      ENDDO 



      END SUBROUTINE DDAMP



      END MODULE MODULE_IGWAVE_ADJUST


