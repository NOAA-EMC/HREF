



      SUBROUTINE CLTEND (ICLTEND,NPHS,T,T_OLD,T_ADJ                    &
                        ,IDS,IDE,JDS,JDE,KDS,KDE                       &
                        ,IMS,IME,JMS,JME,KMS,KME                       &
                        ,ITS,ITE,JTS,JTE,KTS,KTE)






























      USE module_MPP

      IMPLICIT NONE



      INTEGER,INTENT(IN) :: ICLTEND                                    &
                           ,IDS,IDE,JDS,JDE,KDS,KDE                    &
                           ,IMS,IME,JMS,JME,KMS,KME                    &
                           ,ITS,ITE,JTS,JTE,KTS,KTE                    &
                           ,NPHS

      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT) :: T       &
                                                              ,T_ADJ   &
                                                              ,T_OLD



      INTEGER :: I,J,K

      REAL :: DELTPH




      IF(ICLTEND<0)THEN
        DO K=KTS,KTE
        DO J=JTS,JTE
        DO I=ITS,ITE
          T_OLD(I,J,K)=T(I,J,K)
        ENDDO
        ENDDO
        ENDDO
      ELSEIF(ICLTEND==0)THEN
        DO K=KTS,KTE
        DO J=JTS,JTE
        DO I=ITS,ITE
          T_ADJ(I,J,K)=T(I,J,K)-T_OLD(I,J,K)
          T(I,J,K)=T_OLD(I,J,K)
        ENDDO
        ENDDO
        ENDDO
      ELSE
        DELTPH=1./REAL(NPHS)
        DO K=KTS,KTE
        DO J=JTS,JTE
        DO I=ITS,ITE
          T(I,J,K)=T(I,J,K)+DELTPH*T_ADJ(I,J,K)
        ENDDO
        ENDDO
        ENDDO
      ENDIF


      END SUBROUTINE CLTEND


