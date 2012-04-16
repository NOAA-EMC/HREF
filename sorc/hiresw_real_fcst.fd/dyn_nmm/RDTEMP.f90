



      SUBROUTINE RDTEMP(NTSD,DT,JULDAY,JULYR,XTIME,IHRST,GLAT,GLON      &
     &                 ,CZEN,CZMEAN,T,RSWTT,RLWTT,HBM2                  &
     &                 ,IDS,IDE,JDS,JDE,KDS,KDE                         &
     &                 ,IMS,IME,JMS,JME,KMS,KME                         &
     &                 ,ITS,ITE,JTS,JTE,KTS,KTE)


























      USE MODULE_MPP
      USE MODULE_RA_GFDLETA,ONLY : CAL_MON_DAY,ZENITH


      IMPLICIT NONE



      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE

      INTEGER,INTENT(IN) :: IHRST,JULDAY,JULYR,NTSD

      REAL,INTENT(IN) :: DT,XTIME

      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: CZMEAN,GLAT,GLON    &
     &                                             ,HBM2

      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(IN) :: RLWTT       &
     &                                                     ,RSWTT

      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT) :: T

      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: CZEN





      INTEGER :: I,J,JDAY,JMONTH,K

      INTEGER,DIMENSION(3) :: IDAT

      REAL :: DAYI,HOUR,TIMES,TTNDKL

      REAL,DIMENSION(IMS:IME,JMS:JME) :: CZEN2,XLAT2,XLON2

      REAL,DIMENSION(ITS:ITE,JTS:JTE) :: FACTR

      REAL :: DEGRAD=3.1415926/180.
      real :: xlat1,xlon1



      MYIS=MAX(IDS,ITS)
      MYIE=MIN(IDE,ITE)
      MYJS=MAX(JDS,JTS)
      MYJE=MIN(JDE,JTE)





      TIMES=XTIME*60.

      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        XLAT2(I,J)=GLAT(I,J)
        XLON2(I,J)=GLON(I,J)








      ENDDO
      ENDDO

      CALL CAL_MON_DAY(JULDAY,JULYR,JMONTH,JDAY)

      IDAT(1)=JMONTH
      IDAT(2)=JDAY
      IDAT(3)=JULYR

      CALL ZENITH(TIMES,DAYI,HOUR,IDAT,IHRST,XLON2,XLAT2,CZEN2          &
     &           ,MYIS,MYIE,MYJS,MYJE                                   &
     &           ,IDS,IDE,JDS,JDE,KDS,KDE                               &
     &           ,IMS,IME,JMS,JME,KMS,KME                               &
     &           ,ITS,ITE,JTS,JTE,KTS,KTE)

      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        CZEN(I,J)=CZEN2(I,J)
        IF(CZMEAN(I,J)>0.)THEN 
          FACTR(I,J)=CZEN(I,J)/CZMEAN(I,J)
        ELSE
          FACTR(I,J)=0.
        ENDIF
      ENDDO
      ENDDO

      DO K=KTS,KTE
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          TTNDKL=RSWTT(I,J,K)*FACTR(I,J)+RLWTT(I,J,K)
          T(I,J,K)=T(I,J,K)+TTNDKL*DT*HBM2(I,J)
        ENDDO
        ENDDO
      ENDDO

      END SUBROUTINE RDTEMP

