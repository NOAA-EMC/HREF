




      SUBROUTINE BUCKETS(NTSD,NPREC,NSRFC,NRDSW,NRDLW                   &
     &                  ,RESTART,TSTART                                 &
     &                  ,NCLOD,NHEAT,NPHS,TSPH                          &
     &                  ,ACPREC,CUPREC,ACSNOW,ACSNOM,SSROFF,BGROFF      &
     &                  ,SFCEVP,POTEVP,SFCSHX,SFCLHX,SUBSHX,SNOPCX      &
     &                  ,SFCUVX,POTFLX                                  &
     &                  ,ARDSW,ASWIN,ASWOUT,ASWTOA                      &
     &                  ,ARDLW,ALWIN,ALWOUT,ALWTOA                      &
     &                  ,ACFRST,NCFRST,ACFRCV,NCFRCV                    &
     &                  ,AVCNVC,AVRAIN,TCUCN,TRAIN                      &
     &                  ,ASRFC                                          &
     &                  ,T,TLMAX,TLMIN,TSHLTR,PSHLTR,QSHLTR             &
     &                  ,T02_MAX,T02_MIN,RH02_MAX,RH02_MIN              &
     &                  ,IDS,IDE,JDS,JDE,KDS,KDE                        &
     &                  ,IMS,IME,JMS,JME,KMS,KME                        &
     &                  ,ITS,ITE,JTS,JTE,KTS,KTE)





















































      USE MODULE_MODEL_CONSTANTS,ONLY: CP,CPV,R_D,R_V,RCP
      USE MODULE_MP_ETANEW,ONLY: C1XPVS,C1XPVS0,C2XPVS,C2XPVS0          &
                                ,FPVS,FPVS0,NX,TBPVS,TBPVS0             &
                                ,GPVS



      IMPLICIT NONE





      INTEGER,INTENT(IN) :: NCLOD,NHEAT,NPHS,NPREC,NRDLW,NRDSW          &
                           ,NSRFC,NTSD                                  &
                           ,IDS,IDE,JDS,JDE,KDS,KDE                     &
                           ,IMS,IME,JMS,JME,KMS,KME                     &
                           ,ITS,ITE,JTS,JTE,KTS,KTE 

      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: NCFRST,NCFRCV

      REAL,INTENT(IN) :: TSPH,TSTART

      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: PSHLTR,QSHLTR,TSHLTR

      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(IN) :: T

      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: TLMAX,TLMIN

      REAL,INTENT(OUT) :: ARDLW,ARDSW,ASRFC,AVCNVC,AVRAIN

      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: ACPREC,ACSNOM      &
     &                                              ,ACSNOW,ALWIN       &
     &                                              ,ACFRST,ACFRCV      &
     &                                              ,ALWOUT,ALWTOA      &
     &                                              ,ASWIN,ASWOUT       &
     &                                              ,ASWTOA,BGROFF      &
     &                                              ,CUPREC,POTEVP      &
     &                                              ,POTFLX,SFCEVP      &
     &                                              ,RH02_MAX,RH02_MIN  &
     &                                              ,SFCLHX,SFCSHX      &
     &                                              ,SFCUVX,SNOPCX      &
     &                                              ,SSROFF,SUBSHX      &
     &                                              ,T02_MAX,T02_MIN

      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(OUT) :: TCUCN      &
     &                                                      ,TRAIN

      LOGICAL,INTENT(IN) :: RESTART





      INTEGER :: I,J,K,NTSD_BUCKET,NTSPH
      LOGICAL ::  FIRST_PASS=.TRUE.
      LOGICAL ::  WRF_DM_ON_MONITOR
      EXTERNAL WRF_DM_ON_MONITOR

      REAL :: CAPPA_MOIST,RH02,SAT_VAPOR_PRESS,VAPOR_PRESS
      REAL,SAVE :: CP_FACTOR,EPSILON,ONE_MINUS_EPSILON,R_FACTOR
      REAL,SAVE :: P00_INV=1.E-5

      REAL,DIMENSION(ITS:ITE,JTS:JTE) :: T02










      IF(FIRST_PASS)THEN
        FIRST_PASS=.FALSE.

        EPSILON=R_D/R_V
        ONE_MINUS_EPSILON=1.-EPSILON
        R_FACTOR=1./EPSILON-1.
        CP_FACTOR=CPV/CP-1.

        CALL GPVS 
      ENDIF



      NTSD_BUCKET=NTSD









      IF(MOD(NTSD_BUCKET,NPREC)==0)THEN
        DO J=JTS,JTE
        DO I=ITS,ITE
          ACPREC(I,J)=0.
          CUPREC(I,J)=0.
          ACSNOW(I,J)=0.
          ACSNOM(I,J)=0.
          SSROFF(I,J)=0.
          BGROFF(I,J)=0.
          SFCEVP(I,J)=0.
          POTEVP(I,J)=0.
        ENDDO
        ENDDO

        IF ( WRF_DM_ON_MONITOR() ) THEN
        CALL WRF_MESSAGE('ZEROED OUT PRECIP/RUNOFF ARRAYS')
        ENDIF

      ENDIF






      IF(MOD(NTSD_BUCKET,NSRFC)==0)THEN
        ASRFC=0.
        DO J=JTS,JTE
        DO I=ITS,ITE
          SFCSHX(I,J)=0.
          SFCLHX(I,J)=0.
          SUBSHX(I,J)=0.
          SNOPCX(I,J)=0.
          SFCUVX(I,J)=0.
          POTFLX(I,J)=0.
        ENDDO
        ENDDO

        IF ( WRF_DM_ON_MONITOR() ) THEN
        CALL WRF_MESSAGE('ZEROED OUT SFC EVAP/FLUX ARRAYS')
        ENDIF

      ENDIF






      IF(MOD(NTSD_BUCKET,NRDSW)==0)THEN
        ARDSW=0.
        DO J=JTS,JTE
        DO I=ITS,ITE
          ASWIN(I,J) =0.
          ASWOUT(I,J)=0.
          ASWTOA(I,J)=0.
        ENDDO
        ENDDO

        IF ( WRF_DM_ON_MONITOR() ) THEN
        CALL WRF_MESSAGE('ZEROED OUT ACCUMULATED SHORTWAVE FLUX ARRAYS')
        ENDIF

      ENDIF






      IF(MOD(NTSD_BUCKET,NRDLW)==0)THEN
        ARDLW=0.
        DO J=JTS,JTE
        DO I=ITS,ITE
          ALWIN(I,J) =0.
          ALWOUT(I,J)=0.
          ALWTOA(I,J)=0.
        ENDDO
        ENDDO

        IF ( WRF_DM_ON_MONITOR() ) THEN
        CALL WRF_MESSAGE('ZEROED OUT ACCUMULATED LONGWAVE FLUX ARRAYS')
        ENDIF

      ENDIF






      IF(MOD(NTSD_BUCKET,NCLOD)==0)THEN

  
  
  

        DO J=JTS,JTE
        DO I=ITS,ITE
          ACFRCV(I,J)=0.
          ACFRST(I,J)=0.
          NCFRCV(I,J)=0
          NCFRST(I,J)=0
        ENDDO
        ENDDO

        IF ( WRF_DM_ON_MONITOR() ) THEN
        CALL WRF_MESSAGE('ZEROED OUT ACCUMULATED CLOUD FRACTION ARRAYS')
        ENDIF

      ENDIF






      IF(MOD(NTSD_BUCKET,NHEAT)==0)THEN
        AVCNVC=0.
        AVRAIN=0.

        DO K=KTS,KTE
        DO J=JTS,JTE
        DO I=ITS,ITE
          TRAIN(I,J,K)=0.
          TCUCN(I,J,K)=0.
        ENDDO
        ENDDO
        ENDDO

        IF ( WRF_DM_ON_MONITOR() ) THEN
        CALL WRF_MESSAGE('ZEROED OUT ACCUMULATED LATENT HEATING ARRAYS')
        ENDIF

      ENDIF





      NTSPH=NINT(TSPH)
      IF(MOD(NTSD_BUCKET,NTSPH)==0)THEN
        DO J=JTS,JTE
        DO I=ITS,ITE
          TLMAX(I,J)=-999.
          TLMIN(I,J)=999.
          T02_MAX(I,J)=-999.
          T02_MIN(I,J)=999.
        ENDDO
        ENDDO

        IF ( WRF_DM_ON_MONITOR() ) THEN
        CALL WRF_MESSAGE('RESET MAX/MIN TEMPERATURES')
        ENDIF
      ENDIF

      DO J=JTS,JTE
      DO I=ITS,ITE
        TLMAX(I,J)=MAX(TLMAX(I,J),T(I,J,1))         
        TLMIN(I,J)=MIN(TLMIN(I,J),T(I,J,1))         

        CAPPA_MOIST=RCP*(1.+QSHLTR(I,J)*R_FACTOR)/(1.+QSHLTR(I,J)*CP_FACTOR)
        T02(I,J)=TSHLTR(I,J)*(P00_INV*PSHLTR(I,J))**CAPPA_MOIST

        IF(NTSD>0)THEN
          T02_MAX(I,J)=MAX(T02_MAX(I,J),T02(I,J))     
          T02_MIN(I,J)=MIN(T02_MIN(I,J),T02(I,J))     
        ENDIF
      ENDDO
      ENDDO





      IF(MOD(NTSD_BUCKET,NTSPH)==0.OR.NTSD==1)THEN
        DO J=JTS,JTE
        DO I=ITS,ITE
          RH02_MAX(I,J)=-999.
          RH02_MIN(I,J)=999.
        ENDDO
        ENDDO

        IF ( WRF_DM_ON_MONITOR() ) THEN
          CALL WRF_MESSAGE('RESET MAX/MIN RH')
        ENDIF
      ENDIF

      IF(NTSD>0)THEN

        DO J=JTS,JTE
        DO I=ITS,ITE
          VAPOR_PRESS=PSHLTR(I,J)*QSHLTR(I,J)/                          &
                     (EPSILON+QSHLTR(I,J)*ONE_MINUS_EPSILON)


            SAT_VAPOR_PRESS=1.E3*FPVS0(T02(I,J))




          RH02=MIN(VAPOR_PRESS/SAT_VAPOR_PRESS,0.99)

          RH02_MAX(I,J)=MAX(RH02_MAX(I,J),RH02)     
          RH02_MIN(I,J)=MIN(RH02_MIN(I,J),RH02)     
        ENDDO
        ENDDO

      ELSE                         
        DO J=JTS,JTE
        DO I=ITS,ITE
          RH02_MAX(I,J)=0.
          RH02_MIN(I,J)=0.
        ENDDO
        ENDDO

      ENDIF



      END SUBROUTINE BUCKETS


