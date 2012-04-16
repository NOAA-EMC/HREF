





















      MODULE MODULE_BNDRY_COND


      USE MODULE_STATE_DESCRIPTION
      USE MODULE_MODEL_CONSTANTS

      REAL :: D06666=0.06666666


      CONTAINS


      SUBROUTINE BOCOH(GRIDID,NTSD,DT0,NEST,NBC,NBOCO,LAST_TIME,TSPH    & 
     &                ,LB,ETA1,ETA2,PDTOP,PT,RES                        &
     &                ,PD_BXS, PD_BXE, PD_BYS, PD_BYE                   &
     &                ,T_BXS, T_BXE, T_BYS, T_BYE                       &
     &                ,Q_BXS, Q_BXE, Q_BYS, Q_BYE                       &
     &                ,U_BXS, U_BXE, U_BYS, U_BYE                       &
     &                ,V_BXS, V_BXE, V_BYS, V_BYE                       &
     &                ,Q2_BXS, Q2_BXE, Q2_BYS, Q2_BYE                   &
     &                ,CWM_BXS, CWM_BXE, CWM_BYS, CWM_BYE               &
     &                ,PD_BTXS, PD_BTXE, PD_BTYS, PD_BTYE               &
     &                ,T_BTXS, T_BTXE, T_BTYS, T_BTYE                   &
     &                ,Q_BTXS, Q_BTXE, Q_BTYS, Q_BTYE                   &
     &                ,U_BTXS, U_BTXE, U_BTYS, U_BTYE                   &
     &                ,V_BTXS, V_BTXE, V_BTYS, V_BTYE                   &
     &                ,Q2_BTXS, Q2_BTXE, Q2_BTYS, Q2_BTYE               &
     &                ,CWM_BTXS, CWM_BTXE, CWM_BTYS, CWM_BTYE           &
     &                ,PD,T,Q,Q2,CWM,PINT                               &
     &                ,MOIST,N_MOIST,SCALAR,N_SCALAR                    &
     &                ,SPEC_BDY_WIDTH,Z                                 &  
     &                ,IHE,IHW,IVE,IVW                                  &
     &                ,IDS,IDE,JDS,JDE,KDS,KDE                          &
     &                ,IMS,IME,JMS,JME,KMS,KME                          &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)






















































      IMPLICIT NONE


      LOGICAL,INTENT(IN) :: NEST

      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
      INTEGER,INTENT(IN) :: SPEC_BDY_WIDTH
      INTEGER,INTENT(IN) :: N_MOIST, N_SCALAR

      INTEGER,DIMENSION(JMS:JME),INTENT(IN) :: IHE,IHW,IVE,IVW

      INTEGER,INTENT(IN) :: GRIDID
      INTEGER,INTENT(IN) :: LB,NBC,NTSD
      LOGICAL,INTENT(IN) :: LAST_TIME
      INTEGER,INTENT(INOUT) :: NBOCO

      REAL,INTENT(IN) :: DT0,PDTOP,PT,TSPH

      REAL,DIMENSION(KMS:KME),INTENT(IN) :: ETA1,ETA2

      REAL,DIMENSION(IMS:IME,1,SPEC_BDY_WIDTH)                          &
     &                           ,INTENT(INOUT) :: PD_BYS, PD_BYE       &
     &                                            ,PD_BTYS,PD_BTYE

      REAL,DIMENSION(IMS:IME,KMS:KME,SPEC_BDY_WIDTH)                   &
     &                           ,INTENT(INOUT) :: T_BYS, T_BYE        &
     &                                            ,U_BYS, U_BYE        &
     &                                            ,V_BYS, V_BYE        &
     &                                            ,Q_BYS, Q_BYE        &
     &                                            ,Q2_BYS, Q2_BYE      &
     &                                            ,CWM_BYS, CWM_BYE    &
     &                                            ,T_BTYS, T_BTYE      &
     &                                            ,U_BTYS, U_BTYE      &
     &                                            ,V_BTYS, V_BTYE      &
     &                                            ,Q_BTYS, Q_BTYE      &
     &                                            ,Q2_BTYS, Q2_BTYE    &
     &                                            ,CWM_BTYS, CWM_BTYE  

      REAL,DIMENSION(JMS:JME,1,SPEC_BDY_WIDTH)                         &
     &                           ,INTENT(INOUT) :: PD_BXS, PD_BXE      &
     &                                            ,PD_BTXS,PD_BTXE

      REAL,DIMENSION(JMS:JME,KMS:KME,SPEC_BDY_WIDTH)                   &
     &                           ,INTENT(INOUT) :: T_BXS, T_BXE        &
     &                                            ,U_BXS, U_BXE        &
     &                                            ,V_BXS, V_BXE        &
     &                                            ,Q_BXS, Q_BXE        &
     &                                            ,Q2_BXS, Q2_BXE      &
     &                                            ,CWM_BXS, CWM_BXE    &
     &                                            ,T_BTXS, T_BTXE      &
     &                                            ,U_BTXS, U_BTXE      &
     &                                            ,V_BTXS, V_BTXE      &
     &                                            ,Q_BTXS, Q_BTXE      &
     &                                            ,Q2_BTXS, Q2_BTXE    &
     &                                            ,CWM_BTXS, CWM_BTXE  
                                               

      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: RES
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: PD

      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT) :: CWM      &
     &                                                        ,PINT,Q   &
     &                                                        ,Q2,T,Z

      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME,NUM_MOIST)                 &
     &                                           ,INTENT(INOUT) :: MOIST
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME,NUM_SCALAR)                &
     &                                          ,INTENT(INOUT) :: SCALAR






      INTEGER :: I,IB,IBDY,II,IIM,IM,IRTN,ISIZ1,ISIZ2                &
     &          ,J,JB,JJ,JJM,JM,K,KK,N,NN,NREC,NUMGAS,NV,REC
      INTEGER :: MY_IS_GLB,MY_JS_GLB,MY_IE_GLB,MY_JE_GLB  
      INTEGER :: I_M,ILPAD1,IRPAD1,JBPAD1,JTPAD1

      REAL :: BCHR,CONVFAC,CWK,DT,PLYR,RRI

      LOGICAL :: E_BDY,W_BDY,N_BDY,S_BDY

      CHARACTER(LEN=255) :: message




      IM=IDE-IDS+1
      JM=JDE-JDS+1
      IIM=IM
      JJM=JM

      ISIZ1=2*LB
      ISIZ2=2*LB*(KME-KMS)

      W_BDY=(ITS==IDS)
      E_BDY=(ITE==IDE)
      S_BDY=(JTS==JDS)
      N_BDY=(JTE==JDE)

      ILPAD1=1
      IF(W_BDY)ILPAD1=0
      IRPAD1=1
      IF(E_BDY)IRPAD1=0
      JBPAD1=1
      IF(S_BDY)JBPAD1=0
      JTPAD1=1
      IF(N_BDY)JTPAD1=0

      MY_IS_GLB=ITS
      MY_IE_GLB=ITE
      MY_JS_GLB=JTS
      MY_JE_GLB=JTE

      DT=DT0







      DO IBDY=1,2 



        IF(S_BDY.AND.IBDY==1) THEN 
            JB=1         
            JJ=1         

          DO I=MAX(ITS-1,IDS),MIN(ITE+1,IDE)
            PD_BYS(I,1,JB)=PD_BYS(I,1,JB)+PD_BTYS(I,1,JB)*DT
            PD(I,JJ)=PD_BYS(I,1,JB)
          ENDDO

!$omp parallel do                                                       &
!$omp& private(i,k)
          DO K=KTS,KTE
            DO I=MAX(ITS-1,IDS),MIN(ITE+1,IDE)
              T_BYS(I,K,JB)=T_BYS(I,K,JB)+T_BTYS(I,K,JB)*DT
              Q_BYS(I,K,JB)=Q_BYS(I,K,JB)+Q_BTYS(I,K,JB)*DT
              Q2_BYS(I,K,JB)=Q2_BYS(I,K,JB)+Q2_BTYS(I,K,JB)*DT
              CWM_BYS(I,K,JB)=CWM_BYS(I,K,JB)+CWM_BTYS(I,K,JB)*DT

              T(I,JJ,K)=T_BYS(I,K,JB)
              Q(I,JJ,K)=Q_BYS(I,K,JB)
              Q2(I,JJ,K)=Q2_BYS(I,K,JB)
              CWM(I,JJ,K)=CWM_BYS(I,K,JB)
              PINT(I,JJ,K)=ETA1(K)*PDTOP                                &
     &                    +ETA2(K)*PD(I,JJ)*RES(I,JJ)+PT
            ENDDO
           ENDDO

          ELSEIF(N_BDY.AND.IBDY==2) THEN
            JB=1         
            JJ=JJM       


          DO I=MAX(ITS-1,IDS),MIN(ITE+1,IDE)
            PD_BYE(I,1,JB)=PD_BYE(I,1,JB)+PD_BTYE(I,1,JB)*DT
            PD(I,JJ)=PD_BYE(I,1,JB)
          ENDDO

!$omp parallel do                                                       &
!$omp& private(i,k)
          DO K=KTS,KTE
            DO I=MAX(ITS-1,IDS),MIN(ITE+1,IDE)
              T_BYE(I,K,JB)=T_BYE(I,K,JB)+T_BTYE(I,K,JB)*DT
              Q_BYE(I,K,JB)=Q_BYE(I,K,JB)+Q_BTYE(I,K,JB)*DT
              Q2_BYE(I,K,JB)=Q2_BYE(I,K,JB)+Q2_BTYE(I,K,JB)*DT
              CWM_BYE(I,K,JB)=CWM_BYE(I,K,JB)+CWM_BTYE(I,K,JB)*DT

              T(I,JJ,K)=T_BYE(I,K,JB)
              Q(I,JJ,K)=Q_BYE(I,K,JB)
              Q2(I,JJ,K)=Q2_BYE(I,K,JB)
              CWM(I,JJ,K)=CWM_BYE(I,K,JB)
              PINT(I,JJ,K)=ETA1(K)*PDTOP                                &
     &                    +ETA2(K)*PD(I,JJ)*RES(I,JJ)+PT
            ENDDO
          ENDDO




          DO I_M=1,N_MOIST
            IF(I_M==P_QV)THEN
!$omp parallel do                                                       &
!$omp& private(i,k)
              DO K=KTS,KTE
              DO I=MAX(ITS-1,IDS),MIN(ITE+1,IDE)
                MOIST(I,JJ,K,I_M)=Q(I,JJ,K)/(1.-Q(I,JJ,K))
              ENDDO
              ENDDO
            ELSE
!$omp parallel do                                                       &
!$omp& private(i,k)
              DO K=KTS,KTE
              DO I=MAX(ITS-1,IDS),MIN(ITE+1,IDE)
                MOIST(I,JJ,K,I_M)=0.
              ENDDO
              ENDDO
            ENDIF
          ENDDO
          DO I_M=2,N_SCALAR
!$omp parallel do                                                       &
!$omp& private(i,k)
            DO K=KTS,KTE
            DO I=MAX(ITS-1,IDS),MIN(ITE+1,IDE)
              SCALAR(I,JJ,K,I_M)=0.
            ENDDO
            ENDDO
          ENDDO
        ENDIF
      ENDDO







      DO IBDY=1,2 



        IF(W_BDY.AND.IBDY==1) THEN  
            IB=1         
            II=1         

          DO J=MAX(JTS-1,JDS+3-1),MIN(JTE+1,JDE-2)
            IF(MOD(J,2)==1)THEN
              PD_BXS(J,1,IB)=PD_BXS(J,1,IB)+PD_BTXS(J,1,IB)*DT
              PD(II,J)=PD_BXS(J,1,IB)
            ENDIF
          ENDDO

!$omp parallel do                                                       &
!$omp& private(j,k)
          DO K=KTS,KTE
            DO J=MAX(JTS-1,JDS+3-1),MIN(JTE+1,JDE-2)

              IF(MOD(J,2)==1)THEN
                T_BXS(J,K,IB)=T_BXS(J,K,IB)+T_BTXS(J,K,IB)*DT
                Q_BXS(J,K,IB)=Q_BXS(J,K,IB)+Q_BTXS(J,K,IB)*DT
                Q2_BXS(J,K,IB)=Q2_BXS(J,K,IB)+Q2_BTXS(J,K,IB)*DT
                CWM_BXS(J,K,IB)=CWM_BXS(J,K,IB)+CWM_BTXS(J,K,IB)*DT

                T(II,J,K)=T_BXS(J,K,IB)
                Q(II,J,K)=Q_BXS(J,K,IB)
                Q2(II,J,K)=Q2_BXS(J,K,IB)
                CWM(II,J,K)=CWM_BXS(J,K,IB)
                PINT(II,J,K)=ETA1(K)*PDTOP                              &
     &                      +ETA2(K)*PD(II,J)*RES(II,J)+PT
              ENDIF

            ENDDO
          ENDDO

          ELSEIF(E_BDY.AND.IBDY==2) THEN
            IB=1         
            II=IIM       

          DO J=MAX(JTS-1,JDS+3-1),MIN(JTE+1,JDE-2)
            IF(MOD(J,2)==1)THEN
              PD_BXE(J,1,IB)=PD_BXE(J,1,IB)+PD_BTXE(J,1,IB)*DT
              PD(II,J)=PD_BXE(J,1,IB)
            ENDIF
          ENDDO

!$omp parallel do                                                       &
!$omp& private(j,k)
          DO K=KTS,KTE
            DO J=MAX(JTS-1,JDS+3-1),MIN(JTE+1,JDE-2)

              IF(MOD(J,2)==1)THEN
                T_BXE(J,K,IB)=T_BXE(J,K,IB)+T_BTXE(J,K,IB)*DT
                Q_BXE(J,K,IB)=Q_BXE(J,K,IB)+Q_BTXE(J,K,IB)*DT
                Q2_BXE(J,K,IB)=Q2_BXE(J,K,IB)+Q2_BTXE(J,K,IB)*DT
                CWM_BXE(J,K,IB)=CWM_BXE(J,K,IB)+CWM_BTXE(J,K,IB)*DT

                T(II,J,K)=T_BXE(J,K,IB)
                Q(II,J,K)=Q_BXE(J,K,IB)
                Q2(II,J,K)=Q2_BXE(J,K,IB)
                CWM(II,J,K)=CWM_BXE(J,K,IB)
                PINT(II,J,K)=ETA1(K)*PDTOP                              &
     &                      +ETA2(K)*PD(II,J)*RES(II,J)+PT
              ENDIF

            ENDDO
          ENDDO



          DO I_M=1,N_MOIST
            IF(I_M==P_QV)THEN
!$omp parallel do                                                       &
!$omp& private(j,k)
              DO K=KTS,KTE
              DO J=MAX(JTS-1,JDS+3-1),MIN(JTE+1,JDE-2)
                IF(MOD(J,2)==1)THEN
                  MOIST(II,J,K,I_M)=Q(II,J,K)/(1.-Q(II,J,K))
                ENDIF
              ENDDO
              ENDDO

            ELSE
!$omp parallel do                                                       &
!$omp& private(j,k)
              DO K=KTS,KTE
              DO J=MAX(JTS-1,JDS+3-1),MIN(JTE+1,JDE-2)
                IF(MOD(J,2)==1)THEN
                  MOIST(II,J,K,I_M)=0.
                ENDIF
              ENDDO
              ENDDO

            ENDIF
          ENDDO

          DO I_M=2,N_SCALAR
!$omp parallel do                                                       &
!$omp& private(j,k)
            DO K=KTS,KTE
            DO J=MAX(JTS-1,JDS+3-1),MIN(JTE+1,JDE-2)
              IF(MOD(J,2)==1)THEN
                SCALAR(II,J,K,I_M)=0.
              ENDIF
            ENDDO
            ENDDO
          ENDDO

        ENDIF
      ENDDO








      IF(S_BDY)THEN
        DO I=max(ids+(0),its-(0)),min(ide-(1),ite+(0))
          CWK=PD(I,2)
          PD(I,2)=0.25*(PD(I,1)+PD(I+1,1)+PD(I,3)+PD(I+1,3))



          IF(I<=IDE-1.AND.ABS(CWK-PD(I,2))>=300.)THEN
            WRITE(message,*)'PSEUDO HYDROSTATIC IMBALANCE AT THE SOUTHERN BOUNDARY AT',I,2,'GRID #',GRIDID
            CALL wrf_message(trim(message))
            WRITE(message,*)'             ',CWK/100.
            CALL wrf_message(trim(message))
            WRITE(message,*)PD(I,3)/100.,'               ',PD(I+1,3)/100.
            CALL wrf_message(trim(message))
            WRITE(message,*)'             ',PD(I,2)/100.
            CALL wrf_message(trim(message))
            WRITE(message,*)PD(I,1)/100.,'             ',PD(I+1,1)/100.
            CALL wrf_message(trim(message))
            CALL wrf_message('   ')
          ENDIF

        ENDDO
      ENDIF



      IF(N_BDY)THEN
        DO I=max(ids+(0),its-(0)),min(ide-(1),ite+(0))
          CWK=PD(I,JJM-1)
          PD(I,JJM-1)=0.25*(PD(I,JJM-2)+PD(I+1,JJM-2)                   &
     &                     +PD(I,JJM)+PD(I+1,JJM))



          IF(I<=IDE-1.AND.ABS(CWK-PD(I,JJM-1))>=300.)THEN
            WRITE(message,*)'PSEUDO HYDROSTATIC IMBALANCE AT THE NORTHERN BOUNDARY AT',I,JJM-1,'GRID #',GRIDID
            CALL wrf_message(trim(message))
            WRITE(message,*)'             ',CWK/100.
            CALL wrf_message(trim(message))
            WRITE(message,*)PD(I,JJM)/100.,'               ',PD(I+1,JJM)/100.
            CALL wrf_message(trim(message))
            WRITE(message,*)'             ',PD(I,JJM-1)/100.
            CALL wrf_message(trim(message))
            WRITE(message,*)PD(I,JJM-2)/100.,'             ',PD(I+1,JJM-2)/100.
            CALL wrf_message(trim(message))
            CALL wrf_message('   ')
          ENDIF

        ENDDO
      ENDIF



      IF(W_BDY)THEN
        DO J=4,JM-3,2

          IF(W_BDY.AND.J>=MY_JS_GLB-JBPAD1                              &
     &            .AND.J<=MY_JE_GLB+JTPAD1)THEN
            CWK=PD(1,J)
            JJ=J
            PD(1,JJ)=0.25*(PD(1,JJ-1)+PD(2,JJ-1)+PD(1,JJ+1)+PD(2,JJ+1))



             IF(ABS(CWK-PD(1,JJ))>300.)THEN
              WRITE(message,*)'PSEUDO HYDROSTATIC IMBALANCE AT THE WESTERN BOUNDARY AT',1,JJ,'GRID #',GRIDID
              CALL wrf_message(trim(message))
              WRITE(message,*)'             ',CWK/100.
              CALL wrf_message(trim(message))
              WRITE(message,*)PD(1,JJ+1)/100.,'               ',PD(2,JJ+1)/100.
              CALL wrf_message(trim(message))
              WRITE(message,*)'             ',PD(1,JJ)/100.
              CALL wrf_message(trim(message))
              WRITE(message,*)PD(1,JJ-1)/100.,'               ',PD(2,JJ-1)/100.
              CALL wrf_message(trim(message))
              CALL wrf_message('   ')
            ENDIF

          ENDIF

        ENDDO
      ENDIF



      IF(E_BDY)THEN
        DO J=4,JM-3,2

          IF(E_BDY.AND.J>=MY_JS_GLB-JBPAD1                              &
     &            .AND.J<=MY_JE_GLB+JTPAD1)THEN
            CWK=PD(IIM-1,J)
            JJ=J
            PD(IIM-1,JJ)=0.25*(PD(IIM-1,JJ-1)+PD(IIM,JJ-1)              &
     &                        +PD(IIM-1,JJ+1)+PD(IIM,JJ+1))



             IF(ABS(CWK-PD(IIM-1,JJ))>300.)THEN
              WRITE(message,*)'PSEUDO HYDROSTATIC IMBALANCE AT THE EASTERN BOUNDARY AT',IIM-1,JJ,'GRID #',GRIDID
              CALL wrf_message(trim(message))
              WRITE(message,*)'             ',CWK/100.
              CALL wrf_message(trim(message))
              WRITE(message,*)PD(IIM-1,JJ+1)/100.,'               ',PD(IIM,JJ+1)/100.
              CALL wrf_message(trim(message))
              WRITE(message,*)'             ',PD(IIM-1,JJ)/100.
              CALL wrf_message(trim(message))
              WRITE(message,*)PD(IIM-1,JJ-1)/100.,'               ',PD(IIM,JJ-1)/100.
              CALL wrf_message(trim(message))
              CALL wrf_message('   ')
            ENDIF

          ENDIF

        ENDDO
      ENDIF



!$omp parallel do                                                       &
!$omp& private(i,j,jj,k)
      DO 200 K=KTS,KTE





      IF(S_BDY)THEN
        DO I=max(ids+(0),its-(0)),min(ide-(1),ite+(0))
          T(I,2,K)=(T(I,1,K)+T(I+1,1,K)+T(I,3,K)+T(I+1,3,K))*0.25
          Q(I,2,K)=(Q(I,1,K)+Q(I+1,1,K)+Q(I,3,K)+Q(I+1,3,K))*0.25
          Q2(I,2,K)=(Q2(I,1,K)+Q2(I+1,1,K)+Q2(I,3,K)+Q2(I+1,3,K))*0.25
          CWM(I,2,K)=(CWM(I,1,K)+CWM(I+1,1,K)+CWM(I,3,K)+CWM(I+1,3,K))  &
     &               *0.25
          PINT(I,2,K)=ETA1(K)*PDTOP+ETA2(K)*PD(I,2)*RES(I,2)+PT
        ENDDO

        DO I_M=1,N_MOIST
          IF(I_M==P_QV)THEN
            DO I=max(ids+(0),its-(0)),min(ide-(1),ite+(0))
              MOIST(I,2,K,I_M)=Q(I,2,K)/(1.-Q(I,2,K))
            ENDDO
          ELSE
            DO I=max(ids+(0),its-(0)),min(ide-(1),ite+(0))
              MOIST(I,2,K,I_M)=(MOIST(I,1,K,I_M)                        &
     &                         +MOIST(I+1,1,K,I_M)                      &
     &                         +MOIST(I,3,K,I_M)                        &
     &                         +MOIST(I+1,3,K,I_M))*0.25
            ENDDO
          ENDIF
        ENDDO

        DO I_M=2,N_SCALAR
          DO I=max(ids+(0),its-(0)),min(ide-(1),ite+(0))
            SCALAR(I,2,K,I_M)=(SCALAR(I,1,K,I_M)                        &
     &                        +SCALAR(I+1,1,K,I_M)                      &
     &                        +SCALAR(I,3,K,I_M)                        &
     &                        +SCALAR(I+1,3,K,I_M))*0.25
          ENDDO
        ENDDO

      ENDIF



      IF(N_BDY)THEN
        DO I=max(ids+(0),its-(0)),min(ide-(1),ite+(0))
          T(I,JJM-1,K)=(T(I,JJM-2,K)+T(I+1,JJM-2,K)                     &
     &                 +T(I,JJM,K)+T(I+1,JJM,K))                        &
     &                 *0.25
          Q(I,JJM-1,K)=(Q(I,JJM-2,K)+Q(I+1,JJM-2,K)                     &
     &                 +Q(I,JJM,K)+Q(I+1,JJM,K))                        &
     &                 *0.25
          Q2(I,JJM-1,K)=(Q2(I,JJM-2,K)+Q2(I+1,JJM-2,K)                  &
     &                  +Q2(I,JJM,K)+Q2(I+1,JJM,K))                     &
     &                  *0.25
          CWM(I,JJM-1,K)=(CWM(I,JJM-2,K)+CWM(I+1,JJM-2,K)               &
     &                   +CWM(I,JJM,K)+CWM(I+1,JJM,K))                  &
     &                   *0.25
          PINT(I,JJM-1,K)=ETA1(K)*PDTOP                                 &
     &                   +ETA2(K)*PD(I,JJM-1)*RES(I,JJM-1)+PT
        ENDDO

        DO I_M=1,N_MOIST
          IF(I_M==P_QV)THEN
            DO I=max(ids+(0),its-(0)),min(ide-(1),ite+(0))
              MOIST(I,JJM-1,K,I_M)=Q(I,JJM-1,K)/(1.-Q(I,JJM-1,K))
            ENDDO
          ELSE
            DO I=max(ids+(0),its-(0)),min(ide-(1),ite+(0))
              MOIST(I,JJM-1,K,I_M)=(MOIST(I,JJM-2,K,I_M)                &
     &                             +MOIST(I+1,JJM-2,K,I_M)              &
     &                             +MOIST(I,JJM,K,I_M)                  &
     &                             +MOIST(I+1,JJM,K,I_M))*0.25
            ENDDO

          ENDIF
        ENDDO

        DO I_M=2,N_SCALAR
          DO I=max(ids+(0),its-(0)),min(ide-(1),ite+(0))
            SCALAR(I,JJM-1,K,I_M)=(SCALAR(I,JJM-2,K,I_M)                &
     &                            +SCALAR(I+1,JJM-2,K,I_M)              &
     &                            +SCALAR(I,JJM,K,I_M)                  &
     &                            +SCALAR(I+1,JJM,K,I_M))*0.25
          ENDDO
        ENDDO

      ENDIF



      IF(W_BDY)THEN
        DO J=4,JM-3,2

          IF(W_BDY.AND.J>=MY_JS_GLB-JBPAD1                              &
     &            .AND.J<=MY_JE_GLB+JTPAD1)THEN
            JJ=J
            T(1,JJ,K)=(T(1,JJ-1,K)+T(2,JJ-1,K)                          &
     &                +T(1,JJ+1,K)+T(2,JJ+1,K))                         &
     &                *0.25
            Q(1,JJ,K)=(Q(1,JJ-1,K)+Q(2,JJ-1,K)                          &
     &                +Q(1,JJ+1,K)+Q(2,JJ+1,K))                         &
     &                *0.25
            Q2(1,JJ,K)=(Q2(1,JJ-1,K)+Q2(2,JJ-1,K)                       &
     &                 +Q2(1,JJ+1,K)+Q2(2,JJ+1,K))                      &
     &                 *0.25
            CWM(1,JJ,K)=(CWM(1,JJ-1,K)+CWM(2,JJ-1,K)                    &
     &                  +CWM(1,JJ+1,K)+CWM(2,JJ+1,K))                   &
     &                  *0.25
            PINT(1,JJ,K)=ETA1(K)*PDTOP                                  &
     &                  +ETA2(K)*PD(1,JJ)*RES(1,JJ)+PT

            DO I_M=1,N_MOIST
              IF(I_M==P_QV)THEN
                MOIST(1,JJ,K,I_M)=Q(1,JJ,K)/(1.-Q(1,JJ,K))     
              ELSE  
                MOIST(1,JJ,K,I_M)=(MOIST(1,JJ-1,K,I_M)                  &
     &                            +MOIST(2,JJ-1,K,I_M)                  &
     &                            +MOIST(1,JJ+1,K,I_M)                  &
     &                            +MOIST(2,JJ+1,K,I_M))*0.25
              ENDIF
            ENDDO    

            DO I_M=2,N_SCALAR
              SCALAR(1,JJ,K,I_M)=(SCALAR(1,JJ-1,K,I_M)                  &
     &                           +SCALAR(2,JJ-1,K,I_M)                  &
     &                           +SCALAR(1,JJ+1,K,I_M)                  &
     &                           +SCALAR(2,JJ+1,K,I_M))*0.25
            ENDDO

          ENDIF

        ENDDO

      ENDIF



      IF(E_BDY)THEN
        DO J=4,JM-3,2

          IF(E_BDY.AND.J>=MY_JS_GLB-JBPAD1                              &
     &            .AND.J<=MY_JE_GLB+JTPAD1)THEN
            JJ=J
            T(IIM-1,JJ,K)=(T(IIM-1,JJ-1,K)+T(IIM,JJ-1,K)                &
     &                    +T(IIM-1,JJ+1,K)+T(IIM,JJ+1,K))               &
     &                    *0.25
            Q(IIM-1,JJ,K)=(Q(IIM-1,JJ-1,K)+Q(IIM,JJ-1,K)                &
     &                    +Q(IIM-1,JJ+1,K)+Q(IIM,JJ+1,K))               &
     &                    *0.25
            Q2(IIM-1,JJ,K)=(Q2(IIM-1,JJ-1,K)+Q2(IIM,JJ-1,K)             &
     &                     +Q2(IIM-1,JJ+1,K)+Q2(IIM,JJ+1,K))            &
     &                     *0.25
            CWM(IIM-1,JJ,K)=(CWM(IIM-1,JJ-1,K)+CWM(IIM,JJ-1,K)          &
     &                      +CWM(IIM-1,JJ+1,K)+CWM(IIM,JJ+1,K))         &
     &                      *0.25
            PINT(IIM-1,JJ,K)=ETA1(K)*PDTOP                              &
     &                      +ETA2(K)*PD(IIM-1,JJ)*RES(IIM-1,JJ)+PT

            DO I_M=1,N_MOIST
              IF(I_M==P_QV)THEN
                MOIST(IIM-1,JJ,K,I_M)=Q(IIM-1,JJ,K)/(1.-Q(IIM-1,JJ,K))
              ELSE
                MOIST(IIM-1,JJ,K,I_M)=(MOIST(IIM-1,JJ-1,K,I_M)                   &
     &                                +MOIST(IIM,JJ-1,K,I_M)                     &
     &                                +MOIST(IIM-1,JJ+1,K,I_M)                   &
     &                                +MOIST(IIM,JJ+1,K,I_M))*0.25
                ENDIF
              ENDDO

              DO I_M=2,N_SCALAR
                SCALAR(IIM-1,JJ,K,I_M)=(SCALAR(IIM-1,JJ-1,K,I_M)                    &
     &                                 +SCALAR(IIM,JJ-1,K,I_M)                      &
     &                                 +SCALAR(IIM-1,JJ+1,K,I_M)                    &
     &                                 +SCALAR(IIM,JJ+1,K,I_M))*0.25
              ENDDO

          ENDIF

        ENDDO
      ENDIF


  200 CONTINUE


      END SUBROUTINE BOCOH



      SUBROUTINE BOCOV(GRIDID,NTSD,DT,LB                                &
     &                ,U_BXS,U_BXE,U_BYS,U_BYE                         &  
     &                ,V_BXS,V_BXE,V_BYS,V_BYE                         &  
     &                ,U_BTXS,U_BTXE,U_BTYS,U_BTYE                     &  
     &                ,V_BTXS,V_BTXE,V_BTYS,V_BTYE                     &  
     &                ,U,V                                              &
     &                ,SPEC_BDY_WIDTH                                   &  
     &                ,IHE,IHW,IVE,IVW                                  &
     &                ,IDS,IDE,JDS,JDE,KDS,KDE                          &
     &                ,IMS,IME,JMS,JME,KMS,KME                          &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)
















































      IMPLICIT NONE


      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
      INTEGER,INTENT(IN) :: SPEC_BDY_WIDTH

      INTEGER,DIMENSION(JMS:JME),INTENT(IN) :: IHE,IHW,IVE,IVW

      INTEGER,INTENT(IN) :: GRIDID
      INTEGER,INTENT(IN) :: LB,NTSD

      REAL,INTENT(IN) :: DT

      REAL,DIMENSION(IMS:IME,KMS:KME,SPEC_BDY_WIDTH),INTENT(INOUT) ::     &
     &                                          U_BYS,U_BYE,V_BYS,V_BYE &
     &                                         ,U_BTYS,U_BTYE           &
     &                                         ,V_BTYS,V_BTYE           

      REAL,DIMENSION(JMS:JME,KMS:KME,SPEC_BDY_WIDTH),INTENT(INOUT) ::     &
     &                                          U_BXS,U_BXE,V_BXS,V_BXE &
     &                                         ,U_BTXS,U_BTXE           &
     &                                         ,V_BTXS,V_BTXE 

      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT) :: U,V




      INTEGER :: I,II,IIM,IM,J,JJ,JJM,JM,K,N
      INTEGER :: MY_IS_GLB, MY_JS_GLB,MY_IE_GLB,MY_JE_GLB  
      INTEGER :: IBDY,JB,IB
      INTEGER :: ILPAD1,IRPAD1,JBPAD1,JTPAD1
      LOGICAL :: E_BDY,W_BDY,N_BDY,S_BDY








      IM=IDE-IDS+1
      JM=JDE-JDS+1
      IIM=IM
      JJM=JM

      W_BDY=(ITS==IDS)
      E_BDY=(ITE==IDE)
      S_BDY=(JTS==JDS)
      N_BDY=(JTE==JDE)

      ILPAD1=1
      IF(ITS==IDS)ILPAD1=0
      IRPAD1=1
      IF(ITE==IDE)ILPAD1=0
      JBPAD1=1
      IF(JTS==JDS)JBPAD1=0
      JTPAD1=1
      IF(JTE==JDE)JTPAD1=0

      MY_IS_GLB=ITS
      MY_IE_GLB=ITE
      MY_JS_GLB=JTS
      MY_JE_GLB=JTE






      DO IBDY=1,2  



        IF(S_BDY.AND.IBDY==1) THEN

            JB=1         
            JJ=1         

!$omp parallel do                                                       &
!$omp& private(i,k)
          DO K=KTS,KTE
            DO I=MAX(ITS-1,IDS),MIN(ITE+1,IDE)
              U_BYS(I,K,JB)=U_BYS(I,K,JB)+U_BTYS(I,K,JB)*DT
              V_BYS(I,K,JB)=V_BYS(I,K,JB)+V_BTYS(I,K,JB)*DT
              U(I,JJ,K)=U_BYS(I,K,JB)
              V(I,JJ,K)=V_BYS(I,K,JB)
            ENDDO
          ENDDO


          ELSEIF(N_BDY.AND.IBDY==2) THEN
            JB=1         
            JJ=JJM       

!$omp parallel do                                                       &
!$omp& private(i,k)
          DO K=KTS,KTE
            DO I=MAX(ITS-1,IDS),MIN(ITE+1,IDE)
              U_BYE(I,K,JB)=U_BYE(I,K,JB)+U_BTYE(I,K,JB)*DT
              V_BYE(I,K,JB)=V_BYE(I,K,JB)+V_BTYE(I,K,JB)*DT
              U(I,JJ,K)=U_BYE(I,K,JB)
              V(I,JJ,K)=V_BYE(I,K,JB)
            ENDDO
          ENDDO


          ENDIF
      ENDDO







      DO IBDY=1,2    



        IF(W_BDY.AND.IBDY==1) THEN
            IB=1         
            II=1         

!$omp parallel do                                                       &
!$omp& private(j,k)
          DO K=KTS,KTE
            DO J=MAX(JTS-1,JDS+2-1),MIN(JTE+1,JDE-1)
              IF(MOD(J,2)==0)THEN
                U_BXS(J,K,IB)=U_BXS(J,K,IB)+U_BTXS(J,K,IB)*DT
                V_BXS(J,K,IB)=V_BXS(J,K,IB)+V_BTXS(J,K,IB)*DT
                U(II,J,K)=U_BXS(J,K,IB)
                V(II,J,K)=V_BXS(J,K,IB)
              ENDIF
            ENDDO
          ENDDO

        ELSEIF (E_BDY.AND.IBDY==2) THEN
            IB=1         
            II=IIM       

!$omp parallel do                                                       &
!$omp& private(j,k)
          DO K=KTS,KTE
            DO J=MAX(JTS-1,JDS+2-1),MIN(JTE+1,JDE-1)
              IF(MOD(J,2)==0)THEN
                U_BXE(J,K,IB)=U_BXE(J,K,IB)+U_BTXE(J,K,IB)*DT
                V_BXE(J,K,IB)=V_BXE(J,K,IB)+V_BTXE(J,K,IB)*DT
                U(II,J,K)=U_BXE(J,K,IB)
                V(II,J,K)=V_BXE(J,K,IB)
              ENDIF
            ENDDO
          ENDDO


        ENDIF



      ENDDO








      IF(GRIDID/=1)GO TO 201



!$omp parallel do                                                       &
!$omp& private(i,j,jj,k)
      DO 200 K=KTS,KTE





      IF(S_BDY)THEN
        DO I=max(ids+(1),its-(1)),min(ide-(2),ite+(1))
          IF(V(I,1,K)<0.)U(I,1,K)=2.*U(I,3,K)-U(I,5,K)
        ENDDO
      ENDIF



      IF(N_BDY)THEN
        DO I=max(ids+(1),its-(1)),min(ide-(2),ite+(1))
          IF(V(I,JJM,K)>0.)                                             &
     &        U(I,JJM,K)=2.*U(I,JJM-2,K)-U(I,JJM-4,K)
        ENDDO
      ENDIF



      DO J=4,JM-3,2
        IF(W_BDY)THEN

          IF(W_BDY.AND.J>=MY_JS_GLB-JBPAD1                              &
     &            .AND.J<=MY_JE_GLB+JTPAD1)THEN
            JJ=J
            IF(U(1,JJ,K)<0.)                                            &
     &          V(1,JJ,K)=2.*V(2,JJ,K)-V(3,JJ,K)
          ENDIF

        ENDIF
      ENDDO



      DO J=4,JM-3,2
        IF(E_BDY)THEN

          IF(E_BDY.AND.J>=MY_JS_GLB-JBPAD1                              &
     &            .AND.J<=MY_JE_GLB+JTPAD1)THEN
            JJ=J
            IF(U(IIM,JJ,K)>0.)                                          &
     &          V(IIM,JJ,K)=2.*V(IIM-1,JJ,K)-V(IIM-2,JJ,K)
          ENDIF

        ENDIF
      ENDDO


  200 CONTINUE

  201 CONTINUE









!$omp parallel do                                                       &
!$omp& private(i,j,jj,k)
      DO 300 K=KTS,KTE





      IF(S_BDY.AND.W_BDY)THEN
        U(2,2,K)=D06666*(4.*(U(1,1,K)+U(2,1,K)+U(2,3,K))                &
     &                     + U(1,2,K)+U(1,4,K)+U(2,4,K))
        V(2,2,K)=D06666*(4.*(V(1,1,K)+V(2,1,K)+V(2,3,K))                &
     &                      +V(1,2,K)+V(1,4,K)+V(2,4,K))
      ENDIF



      IF(S_BDY.AND.E_BDY)THEN
        U(IIM-1,2,K)=D06666*(4.*(U(IIM-2,1,K)+U(IIM-1,1,K)              &
     &                          +U(IIM-2,3,K))                          &
     &                          +U(IIM,2,K)+U(IIM,4,K)+U(IIM-1,4,K))
        V(IIM-1,2,K)=D06666*(4.*(V(IIM-2,1,K)+V(IIM-1,1,K)              &
     &                          +V(IIM-2,3,K))                          &
     &                          +V(IIM,2,K)+V(IIM,4,K)+V(IIM-1,4,K))
      ENDIF



      IF(N_BDY.AND.W_BDY)THEN
        U(2,JJM-1,K)=D06666*(4.*(U(1,JJM,K)+U(2,JJM,K)+U(2,JJM-2,K))    &
     &                          +U(1,JJM-1,K)+U(1,JJM-3,K)              &
     &                          +U(2,JJM-3,K))
        V(2,JJM-1,K)=D06666*(4.*(V(1,JJM,K)+V(2,JJM,K)+V(2,JJM-2,K))    &
     &                          +V(1,JJM-1,K)+V(1,JJM-3,K)              &
     &                          +V(2,JJM-3,K))
      ENDIF



      IF(N_BDY.AND.E_BDY)THEN
        U(IIM-1,JJM-1,K)=                                               &
     &    D06666*(4.*(U(IIM-2,JJM,K)+U(IIM-1,JJM,K)+U(IIM-2,JJM-2,K))   &
     &               +U(IIM,JJM-1,K)+U(IIM,JJM-3,K)+U(IIM-1,JJM-3,K))
        V(IIM-1,JJM-1,K)=                                               &
     &    D06666*(4.*(V(IIM-2,JJM,K)+V(IIM-1,JJM,K)+V(IIM-2,JJM-2,K))   &
     &               +V(IIM,JJM-1,K)+V(IIM,JJM-3,K)+V(IIM-1,JJM-3,K))
      ENDIF







      IF(S_BDY)THEN
        DO I=max(ids+(2),its-(0)),min(ide-(2),ite+(0))
          U(I,2,K)=(U(I-1,1,K)+U(I,1,K)+U(I-1,3,K)+U(I,3,K))*0.25
          V(I,2,K)=(V(I-1,1,K)+V(I,1,K)+V(I-1,3,K)+V(I,3,K))*0.25
        ENDDO
      ENDIF



      IF(N_BDY)THEN
        DO I=max(ids+(2),its-(0)),min(ide-(2),ite+(0))
          U(I,JJM-1,K)=(U(I-1,JJM-2,K)+U(I,JJM-2,K)                     &
     &                 +U(I-1,JJM,K)+U(I,JJM,K))*0.25
          V(I,JJM-1,K)=(V(I-1,JJM-2,K)+V(I,JJM-2,K)                     &
     &                 +V(I-1,JJM,K)+V(I,JJM,K))*0.25
        ENDDO
      ENDIF



      DO J=3,JM-2,2
        IF(W_BDY)THEN
          IF(W_BDY.AND.J>=MY_JS_GLB-JBPAD1                              &
     &            .AND.J<=MY_JE_GLB+JTPAD1)THEN
            JJ=J
            U(1,JJ,K)=(U(1,JJ-1,K)+U(2,JJ-1,K)                          &
     &                +U(1,JJ+1,K)+U(2,JJ+1,K))*0.25
            V(1,JJ,K)=(V(1,JJ-1,K)+V(2,JJ-1,K)                          &
     &                +V(1,JJ+1,K)+V(2,JJ+1,K))*0.25


          ENDIF
        ENDIF
      ENDDO



      IF(E_BDY)THEN
        DO J=3,JM-2,2
          IF(E_BDY.AND.J>=MY_JS_GLB-JBPAD1                              &
     &            .AND.J<=MY_JE_GLB+JTPAD1)THEN
            JJ=J
            U(IIM-1,JJ,K)=0.25*(U(IIM-1,JJ-1,K)+U(IIM,JJ-1,K)           &
     &                         +U(IIM-1,JJ+1,K)+U(IIM,JJ+1,K))
            V(IIM-1,JJ,K)=0.25*(V(IIM-1,JJ-1,K)+V(IIM,JJ-1,K)           &
     &                         +V(IIM-1,JJ+1,K)+V(IIM,JJ+1,K))
          ENDIF
        ENDDO
      ENDIF


  300 CONTINUE



      END SUBROUTINE BOCOV



      END MODULE MODULE_BNDRY_COND


