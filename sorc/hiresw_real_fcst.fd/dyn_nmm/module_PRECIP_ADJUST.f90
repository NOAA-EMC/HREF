



















      MODULE MODULE_PRECIP_ADJUST













      INTEGER :: ITEST=346,JTEST=256,TESTPE=53


      CONTAINS


      SUBROUTINE READPCP(PPTDAT,DDATA,LSPA                              &
     &  ,IDS,IDE,JDS,JDE,KDS,KDE                                        &
     &  ,IMS,IME,JMS,JME,KMS,KME                                        &
     &  ,ITS,ITE,JTS,JTE,KTS,KTE)















      IMPLICIT NONE
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE,                    &
     &                      IMS,IME,JMS,JME,KMS,KME,                    &
     &                      ITS,ITE,JTS,JTE,KTS,KTE
      REAL,DIMENSION(IDS:IDE,JDS:JDE) :: TEMPG
      REAL,DIMENSION(IMS:IME,JMS:JME) :: TEMPL
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: DDATA, LSPA
      REAL,DIMENSION(IMS:IME,JMS:JME,3),INTENT(OUT) :: PPTDAT
      INTEGER :: I, J, IHR
      INTEGER :: MYPE
      CHARACTER*256 :: MESSAGE



      CALL WRF_GET_MYPROC(MYPE)

      TEMPG=999.

      DO IHR=1,3
        IF(MYPE==0)THEN
          READ(40+IHR) ((TEMPG(I,J),I=IDS,IDE-1),J=JDS,JDE-1)
          WRITE(MESSAGE,*) 'IHR=', IHR, ' FINISHED READING PCP TO TEMPG'
          CALL WRF_MESSAGE(MESSAGE)
          CLOSE(40+IHR)

          DO J=JDS,JDE-1
            DO I=IDS,IDE-1


              IF (TEMPG(I,J).LT.900.) TEMPG(I,J)=TEMPG(I,J)*0.001
            ENDDO
          ENDDO
        ENDIF


        CALL DSTRB(TEMPG,TEMPL,1,1,1,1,1                                &
     &,                IDS,IDE,JDS,JDE,KDS,KDE                          &
     &,                IMS,IME,JMS,JME,KMS,KME                          &
     &,                ITS,ITE,JTS,JTE,KTS,KTE)


        DO J=JMS,JME
          DO I=IMS,IME
            PPTDAT(I,J,IHR)=TEMPL(I,J)
          ENDDO
        ENDDO

        IF(MYPE==TESTPE)THEN
          WRITE(MESSAGE,*) 'ADJPPT-READPCP, IHR',IHR, 'PPTDAT=',        &
     &      PPTDAT(ITEST,JTEST,IHR)
          CALL WRF_MESSAGE(MESSAGE)
        ENDIF

      ENDDO





      DDATA=999.
      LSPA=0.

      RETURN
      END SUBROUTINE READPCP

      SUBROUTINE CHKSNOW(NTSD,DT,NPHS,SR,PPTDAT                         &
     &  ,IDS,IDE,JDS,JDE,KDS,KDE                                        &
     &  ,IMS,IME,JMS,JME,KMS,KME                                        &
     &  ,ITS,ITE,JTS,JTE,KTS,KTE)







      IMPLICIT NONE



      INTEGER,INTENT(IN) :: NTSD,NPHS
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE,                    &
     &                      IMS,IME,JMS,JME,KMS,KME,                    &
     &                      ITS,ITE,JTS,JTE,KTS,KTE
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: SR
      REAL,DIMENSION(IMS:IME,JMS:JME,3),INTENT(INOUT) :: PPTDAT
      REAL,INTENT(IN) :: DT
      REAL :: TIMES
      INTEGER :: I, J, IHR
      INTEGER :: MYPE
      CHARACTER*256 :: MESSAGE

      TIMES=NTSD*DT
      IF (MOD(TIMES,3600.) < NPHS*DT) THEN
        IHR=INT(TIMES)/3600+1
        IF (IHR > 3) go to 10
        DO J=max(jds+(2),jts-(0)),min(jde-(2),jte+(0))
        DO I=max(ids+(1),its-(0)),min(ide-(1),ite+(0))
          IF (SR(I,J) >= 0.9) PPTDAT(I,J,IHR) = 999.
        ENDDO
        ENDDO



        CALL WRF_GET_MYPROC(MYPE)

        IF (MYPE==TESTPE) THEN
          WRITE(MESSAGE,1010) TIMES,SR(ITEST,JTEST)
 1010     FORMAT('ADJPPT-CHKSNOW: TIMES, SR=',F6.0,1X,F6.4)
          CALL WRF_MESSAGE(MESSAGE)
        ENDIF
      ENDIF
 10   CONTINUE
      RETURN
      END SUBROUTINE CHKSNOW

      SUBROUTINE ADJPPT(NTSD,DT,NPHS,PREC,LSPA,PPTDAT,DDATA             &
     &  ,IDS,IDE,JDS,JDE,KDS,KDE                                        &
     &  ,IMS,IME,JMS,JME,KMS,KME                                        &
     &  ,ITS,ITE,JTS,JTE,KTS,KTE)























      IMPLICIT NONE


      INTEGER,INTENT(IN) :: NPHS, NTSD
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE,                    &
     &                      IMS,IME,JMS,JME,KMS,KME,                    &
     &                      ITS,ITE,JTS,JTE,KTS,KTE
      REAL,INTENT(IN) :: DT
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: PREC
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: DDATA, LSPA
      REAL,DIMENSION(IMS:IME,JMS:JME,3),INTENT(OUT) :: PPTDAT





      REAL :: DTPHS, FRACT, FRACT1, FRACT2, TIMES, TPHS1, TPHS2
      INTEGER :: I, J, IHR, IHR1, IHR2, NTSP
      INTEGER :: MYPE
      CHARACTER*256 :: MESSAGE



      CALL WRF_GET_MYPROC(MYPE)

      TIMES=NTSD*DT
      IHR=INT(TIMES)/3600+1

      DTPHS=NPHS*DT




      NTSP=NTSD/NPHS+1
      TPHS1=(NTSP-1)*DTPHS
      TPHS2=NTSP*DTPHS

      IHR1=INT(TPHS1)/3600+1
      IHR2=INT(TPHS2)/3600+1



      IF (IHR1 > 3) THEN 
        GO TO 200
      ELSEIF (IHR2 > 3) THEN
        IHR2=3
        FRACT1=(3600.- MOD(INT(TPHS1),3600))/3600.
        FRACT2=0.
      ELSEIF (IHR1 .EQ. IHR2) THEN
         FRACT1=0.5*DTPHS/3600.
         FRACT2=FRACT1
      ELSE
         FRACT1=(3600.- MOD(INT(TPHS1),3600))/3600.
         FRACT2=FLOAT(MOD(INT(TPHS2),3600))/3600.
      ENDIF

      FRACT=FRACT1 + FRACT2

      IF (MYPE==TESTPE) THEN
         WRITE(MESSAGE,1010) NTSD,NTSP,TIMES,IHR1,IHR2,TPHS1,TPHS2,      &
      &    FRACT1,FRACT2
 1010    FORMAT('ADJPPT: NTSD,NTSP,TIMES=',I4,1X,I4,1X,F6.0,' IHR1,IHR2=' &
      &   ,I1,1X,I1,' TPHS1,TPHS2=',F6.0,1X,F6.0,' FRACT1,FRACT2='        &
      &   ,2(1X,F6.4))
        CALL WRF_MESSAGE(MESSAGE)
      ENDIF








      DO J=max(jds+(2),jts-(0)),min(jde-(2),jte+(0))
      DO I=max(ids+(1),its-(0)),min(ide-(1),ite+(0))

        IF (PPTDAT(I,J,IHR1).GT.900..OR.PPTDAT(I,J,IHR2).GT.900.) THEN
          DDATA(I,J) = 999.
          LSPA(I,J) = LSPA(I,J) + PREC(I,J)
          GO TO 100
        ELSE
          IF (IHR2 .LE. 3) then
            DDATA(I,J) = PPTDAT(I,J,IHR1)*FRACT1                        &
     &                 + PPTDAT(I,J,IHR2)*FRACT2
          ELSE
            DDATA(I,J) = PPTDAT(I,J,IHR1)*FRACT1 
          ENDIF

           LSPA(I,J) = LSPA(I,J) + DDATA(I,J)
        ENDIF
        IF (I.EQ.ITEST .AND. J.EQ.JTEST .AND. MYPE.EQ.TESTPE) THEN
          WRITE(MESSAGE,1020) DDATA(I,J), PREC(I,J), LSPA(I,J)
 1020     FORMAT('ADJPPT: DDATA=',E12.6, ' PREC=',E12.6,' LSPA=',E12.6)
          CALL WRF_MESSAGE(MESSAGE)
        ENDIF

 100    CONTINUE
      ENDDO
      ENDDO

 200  CONTINUE

      RETURN
      END SUBROUTINE ADJPPT
END MODULE module_PRECIP_ADJUST
