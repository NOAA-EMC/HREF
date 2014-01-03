!----------------------------------------------------------------------
!
      MODULE MODULE_DIAGNOSE
!
!----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!----------------------------------------------------------------------
!
      CONTAINS
!
!----------------------------------------------------------------------
!######################################################################
!----------------------------------------------------------------------
      SUBROUTINE TWR(ARRAY,KK,FIELD,NTSD,MYPE,NPES,MPI_COMM_COMP       &
                    ,IDS,IDE,JDS,JDE                                   &
                    ,IMS,IME,JMS,JME                                   &
                    ,ITS,ITE,JTS,JTE                                   &
                    ,DOMAIN_ID )
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
      USE MODULE_INCLUDE
      IMPLICIT NONE
!----------------------------------------------------------------------
!
!------------------------
!***  Argument Variables
!------------------------
!
      INTEGER(KIND=KINT),INTENT(IN) :: IDS,IDE,JDS,JDE                 &
                                      ,IMS,IME,JMS,JME                 &
                                      ,ITS,ITE,JTS,JTE                 &
                                      ,KK,MPI_COMM_COMP,MYPE,NPES,NTSD
!
      REAL(KIND=KFPT),DIMENSION(IMS:IME,JMS:JME,KK),INTENT(IN) :: ARRAY
!
      CHARACTER(*),INTENT(IN) :: FIELD
!
      INTEGER(kind=KINT),INTENT(IN),OPTIONAL :: DOMAIN_ID
!
!--------------------
!*** Local Variables
!--------------------
!
      INTEGER :: IUNIT=23
!
      INTEGER,DIMENSION(MPI_STATUS_SIZE) :: JSTAT
      INTEGER,DIMENSION(MPI_STATUS_SIZE,4) :: STATUS_ARRAY
      INTEGER(KIND=KINT),DIMENSION(2) :: IM_REM,JM_REM,IT_REM,JT_REM
!
      INTEGER(KIND=KINT) :: I,IENDX,IER,IPE,IRECV,IRTN,ISEND           &
                           ,J,K,N,NLEN,NSIZE
      INTEGER(KIND=KINT) :: ITS_REM,ITE_REM,JTS_REM,JTE_REM
!
      REAL(KIND=KFPT),DIMENSION(IDS:IDE,JDS:JDE) :: TWRITE
      REAL(KIND=KFPT),ALLOCATABLE,DIMENSION(:) :: VALUES
      CHARACTER(2) :: DOM_ID
      CHARACTER(5) :: TIMESTEP
      CHARACTER(6) :: FMT
      CHARACTER(15) :: FILENAME
!
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
!
      IF(NTSD<=9)THEN
        FMT='(I1.1)'
        NLEN=1
      ELSEIF(NTSD<=99)THEN
        FMT='(I2.2)'
        NLEN=2
      ELSEIF(NTSD<=999)THEN
        FMT='(I3.3)'
        NLEN=3
      ELSEIF(NTSD<=9999)THEN
        FMT='(I4.4)'
        NLEN=4
      ELSEIF(NTSD<=99999)THEN
        FMT='(I5.5)'
        NLEN=5
      ENDIF
      WRITE(TIMESTEP,FMT)NTSD
!
      IF(PRESENT(DOMAIN_ID))THEN
        FMT='(I2.2)'
        WRITE(DOM_ID,FMT)DOMAIN_ID
        FILENAME=FIELD//'_'//'D'//DOM_ID//'_'//TIMESTEP(1:NLEN)
      ELSE
        FILENAME=FIELD//'_'//TIMESTEP(1:NLEN)
      ENDIF
!
      IF(MYPE==0)THEN
        CLOSE(IUNIT)
        OPEN(UNIT=IUNIT,FILE=FILENAME,FORM='UNFORMATTED'               &
            ,STATUS='REPLACE',IOSTAT=IER)
      ENDIF
!
!----------------------------------------------------------------------
      DO 500 K=1,KK
!----------------------------------------------------------------------
!
      IF(MYPE==0)THEN
        DO J=JTS,JTE
        DO I=ITS,ITE
          TWRITE(I,J)=ARRAY(I,J,K)
        ENDDO
        ENDDO
!
        DO IPE=1,NPES-1
          CALL MPI_RECV(IT_REM,2,MPI_INTEGER,IPE,IPE                    &
                       ,MPI_COMM_COMP,JSTAT,IRECV)
          CALL MPI_RECV(JT_REM,2,MPI_INTEGER,IPE,IPE                    &
                       ,MPI_COMM_COMP,JSTAT,IRECV)
!
          ITS_REM=IT_REM(1)
          ITE_REM=IT_REM(2)
          JTS_REM=JT_REM(1)
          JTE_REM=JT_REM(2)
!
          NSIZE=(ITE_REM-ITS_REM+1)*(JTE_REM-JTS_REM+1)
          ALLOCATE(VALUES(1:NSIZE))
!
          CALL MPI_RECV(VALUES,NSIZE,MPI_REAL,IPE,IPE                   &
                       ,MPI_COMM_COMP,JSTAT,IRECV)
          N=0
          DO J=JTS_REM,JTE_REM
            DO I=ITS_REM,ITE_REM
              N=N+1
              TWRITE(I,J)=VALUES(N)
            ENDDO
          ENDDO
!
          DEALLOCATE(VALUES)
!
        ENDDO
!
!----------------------------------------------------------------------
      ELSE
!
        NSIZE=(ITE-ITS+1)*(JTE-JTS+1)
        ALLOCATE(VALUES(1:NSIZE))
!
        N=0
        DO J=JTS,JTE
        DO I=ITS,ITE
          N=N+1
          VALUES(N)=ARRAY(I,J,K)
        ENDDO
        ENDDO
!
        IT_REM(1)=ITS
        IT_REM(2)=ITE
        JT_REM(1)=JTS
        JT_REM(2)=JTE
!
        CALL MPI_SEND(IT_REM,2,MPI_INTEGER,0,MYPE                       &
                     ,MPI_COMM_COMP,ISEND)
        CALL MPI_SEND(JT_REM,2,MPI_INTEGER,0,MYPE                       &
                     ,MPI_COMM_COMP,ISEND)
!
        CALL MPI_SEND(VALUES,NSIZE,MPI_REAL,0,MYPE                      &
                     ,MPI_COMM_COMP,ISEND)
!
        DEALLOCATE(VALUES)
!
      ENDIF
!----------------------------------------------------------------------
!
      CALL MPI_BARRIER(MPI_COMM_COMP,IRTN)
!
      IF(MYPE==0)THEN
!
        DO J=JDS,JDE
          IENDX=IDE
          WRITE(IUNIT)(TWRITE(I,J),I=IDS,IENDX)
        ENDDO
!
      ENDIF
!
!----------------------------------------------------------------------
  500 CONTINUE
!
      IF(MYPE==0)CLOSE(IUNIT)
!----------------------------------------------------------------------
!
      END SUBROUTINE TWR
!
!-----------------------------------------------------------------------
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!----------------------------------------------------------------------
      SUBROUTINE VWR(ARRAY,KK,FIELD,NTSD,MYPE,NPES,MPI_COMM_COMP       &
                    ,IDS,IDE,JDS,JDE                                   &
                    ,IMS,IME,JMS,JME                                   &
                    ,ITS,ITE,JTS,JTE                                   &
                    ,DOMAIN_ID )
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
      USE MODULE_INCLUDE
      IMPLICIT NONE
!----------------------------------------------------------------------
!
!------------------------
!***  Argument Variables
!------------------------
!
      INTEGER(KIND=KINT),INTENT(IN) :: IDS,IDE,JDS,JDE                 &
                                      ,IMS,IME,JMS,JME                 &
                                      ,ITS,ITE,JTS,JTE                 &
                                      ,KK,MPI_COMM_COMP,MYPE,NPES,NTSD
!
      REAL(KIND=KFPT),DIMENSION(IMS:IME,JMS:JME,KK),INTENT(IN) :: ARRAY
!
      CHARACTER(*),INTENT(IN) :: FIELD
!
      INTEGER(kind=KINT),INTENT(IN),OPTIONAL :: DOMAIN_ID
!
!--------------------
!*** Local Variables
!--------------------
!
      INTEGER :: IUNIT=23
!
      INTEGER,DIMENSION(MPI_STATUS_SIZE) :: JSTAT
      INTEGER,DIMENSION(MPI_STATUS_SIZE,4) :: STATUS_ARRAY
      INTEGER(KIND=KINT),DIMENSION(2) :: IM_REM,JM_REM,IT_REM,JT_REM
!
      INTEGER(KIND=KINT) :: I,IENDX,IER,IPE,IRECV,IRTN,ISEND           &
                           ,J,K,N,NLEN,NSIZE
      INTEGER(KIND=KINT) :: ITS_REM,ITE_REM,JTS_REM,JTE_REM
!
      REAL(KIND=KFPT),DIMENSION(IDS:IDE,JDS:JDE) :: TWRITE
      REAL(KIND=KFPT),ALLOCATABLE,DIMENSION(:) :: VALUES
      CHARACTER(2) :: DOM_ID
      CHARACTER(5) :: TIMESTEP
      CHARACTER(6) :: FMT
      CHARACTER(15) :: FILENAME
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
!
      IF(NTSD<=9)THEN
        FMT='(I1.1)'
        NLEN=1
      ELSEIF(NTSD<=99)THEN
        FMT='(I2.2)'
        NLEN=2
      ELSEIF(NTSD<=999)THEN
        FMT='(I3.3)'
        NLEN=3
      ELSEIF(NTSD<=9999)THEN
        FMT='(I4.4)'
        NLEN=4
      ELSEIF(NTSD<=99999)THEN
        FMT='(I5.5)'
        NLEN=5
      ENDIF
      WRITE(TIMESTEP,FMT)NTSD
!
      IF(PRESENT(DOMAIN_ID))THEN
        FMT='(I2.2)'
        WRITE(DOM_ID,FMT)DOMAIN_ID
        FILENAME=FIELD//'_'//'D'//DOM_ID//'_'//TIMESTEP(1:NLEN)
      ELSE
        FILENAME=FIELD//'_'//TIMESTEP(1:NLEN)
      ENDIF
!
      IF(MYPE==0)THEN
        CLOSE(IUNIT)
        OPEN(UNIT=IUNIT,FILE=FILENAME,FORM='UNFORMATTED',IOSTAT=IER)
      ENDIF
!
!----------------------------------------------------------------------
      DO 500 K=1,KK
!----------------------------------------------------------------------
!
      IF(MYPE==0)THEN
        DO J=JTS,JTE
        DO I=ITS,ITE
          TWRITE(I,J)=ARRAY(I,J,K)
        ENDDO
        ENDDO
!
        DO IPE=1,NPES-1
          CALL MPI_RECV(IT_REM,2,MPI_INTEGER,IPE,IPE                    &
                       ,MPI_COMM_COMP,JSTAT,IRECV)
          CALL MPI_RECV(JT_REM,2,MPI_INTEGER,IPE,IPE                    &
                       ,MPI_COMM_COMP,JSTAT,IRECV)
!
          ITS_REM=IT_REM(1)
          ITE_REM=IT_REM(2)
          JTS_REM=JT_REM(1)
          JTE_REM=JT_REM(2)
!
          NSIZE=(ITE_REM-ITS_REM+1)*(JTE_REM-JTS_REM+1)
          ALLOCATE(VALUES(1:NSIZE))
!
          CALL MPI_RECV(VALUES,NSIZE,MPI_REAL,IPE,IPE                   &
                       ,MPI_COMM_COMP,JSTAT,IRECV)
          N=0
          DO J=JTS_REM,JTE_REM
            DO I=ITS_REM,ITE_REM
              N=N+1
              TWRITE(I,J)=VALUES(N)
            ENDDO
          ENDDO
!
          DEALLOCATE(VALUES)
!
        ENDDO
!
!----------------------------------------------------------------------
      ELSE
!
        NSIZE=(ITE-ITS+1)*(JTE-JTS+1)
        ALLOCATE(VALUES(1:NSIZE))
!
        N=0
        DO J=JTS,JTE
        DO I=ITS,ITE
          N=N+1
          VALUES(N)=ARRAY(I,J,K)
        ENDDO
        ENDDO
!
        IT_REM(1)=ITS
        IT_REM(2)=ITE
        JT_REM(1)=JTS
        JT_REM(2)=JTE
!
        CALL MPI_SEND(IT_REM,2,MPI_INTEGER,0,MYPE                       &
                     ,MPI_COMM_COMP,ISEND)
        CALL MPI_SEND(JT_REM,2,MPI_INTEGER,0,MYPE                       &
                     ,MPI_COMM_COMP,ISEND)
!
        CALL MPI_SEND(VALUES,NSIZE,MPI_REAL,0,MYPE                      &
                     ,MPI_COMM_COMP,ISEND)
!
        DEALLOCATE(VALUES)
!
      ENDIF
!----------------------------------------------------------------------
!
      CALL MPI_BARRIER(MPI_COMM_COMP,IRTN)
!
      IF(MYPE==0)THEN
!
        DO J=JDS,JDE-1
          IENDX=IDE-1
          WRITE(IUNIT)(TWRITE(I,J),I=IDS,IENDX)
        ENDDO
!
      ENDIF
!
!----------------------------------------------------------------------
!
  500 CONTINUE
!
      IF(MYPE==0)CLOSE(IUNIT)
!
!----------------------------------------------------------------------
!
      END SUBROUTINE VWR
!
!----------------------------------------------------------------------
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!----------------------------------------------------------------------
      SUBROUTINE LAT_LON_BNDS(ARRAY1,ARRAY2,MYPE,NPES,MPI_COMM_COMP    &
                    ,IDS,IDE,JDS,JDE                                   &
                    ,IMS,IME,JMS,JME                                   &
                    ,ITS,ITE,JTS,JTE                                   &
                    ,DOMAIN_ID )
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
      USE MODULE_INCLUDE
      IMPLICIT NONE
!----------------------------------------------------------------------
!
!------------------------
!***  Argument Variables
!------------------------
!
      INTEGER(KIND=KINT),INTENT(IN) :: IDS,IDE,JDS,JDE                 &
                                      ,IMS,IME,JMS,JME                 &
                                      ,ITS,ITE,JTS,JTE                 &
                                      ,MPI_COMM_COMP,MYPE,NPES
!
      REAL(KIND=KFPT),DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: ARRAY1,ARRAY2
!
      INTEGER(kind=KINT),INTENT(IN),OPTIONAL :: DOMAIN_ID
!
!--------------------
!*** Local Variables
!--------------------
!
      INTEGER :: IUNIT=176
!
      INTEGER,DIMENSION(MPI_STATUS_SIZE) :: JSTAT
      INTEGER,DIMENSION(MPI_STATUS_SIZE,4) :: STATUS_ARRAY
      INTEGER(KIND=KINT),DIMENSION(2) :: IM_REM,JM_REM,IT_REM,JT_REM
!
      INTEGER(KIND=KINT) :: I,IENDX,IER,IPE,IRECV,IRTN,ISEND           &
                           ,J,K,N,NLEN,NSIZE
      INTEGER(KIND=KINT) :: ITS_REM,ITE_REM,JTS_REM,JTE_REM
!
      REAL(KIND=KFPT):: MINLAT,MAXLAT,MINLON,MAXLON
      REAL(KIND=KFPT),DIMENSION(IDS:IDE,JDS:JDE) :: TWRITE
      REAL(KIND=KFPT),ALLOCATABLE,DIMENSION(:) :: VALUES
      CHARACTER(2) :: DOM_ID
      CHARACTER(6) :: FMT
      CHARACTER(15) :: FILENAME
!
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
!
        FMT='(I2.2)'
        WRITE(DOM_ID,FMT)DOMAIN_ID
        FILENAME='lat_lon_bnds_'//DOM_ID
!
      IF(MYPE==0)THEN
        CLOSE(IUNIT)
        OPEN(UNIT=IUNIT,FILE=FILENAME,FORM='UNFORMATTED')
      ENDIF
!
!----------------------------------------------------------------------
      DO K=1,2
!----------------------------------------------------------------------
!
      IF(MYPE==0)THEN
        DO J=JTS,JTE
        DO I=ITS,ITE
          IF(K==1) THEN
            TWRITE(I,J)=ARRAY1(I,J)
          ELSE
            TWRITE(I,J)=ARRAY2(I,J)
          ENDIF
        ENDDO
        ENDDO
!
        DO IPE=1,NPES-1
          CALL MPI_RECV(IT_REM,2,MPI_INTEGER,IPE,IPE                    &
                       ,MPI_COMM_COMP,JSTAT,IRECV)
          CALL MPI_RECV(JT_REM,2,MPI_INTEGER,IPE,IPE                    &
                       ,MPI_COMM_COMP,JSTAT,IRECV)
!
          ITS_REM=IT_REM(1)
          ITE_REM=IT_REM(2)
          JTS_REM=JT_REM(1)
          JTE_REM=JT_REM(2)
!
          NSIZE=(ITE_REM-ITS_REM+1)*(JTE_REM-JTS_REM+1)
          ALLOCATE(VALUES(1:NSIZE))
!
          CALL MPI_RECV(VALUES,NSIZE,MPI_REAL,IPE,IPE                   &
                       ,MPI_COMM_COMP,JSTAT,IRECV)
          N=0
          DO J=JTS_REM,JTE_REM
            DO I=ITS_REM,ITE_REM
              N=N+1
              TWRITE(I,J)=VALUES(N)
            ENDDO
          ENDDO
!
          DEALLOCATE(VALUES)
!
        ENDDO
!
!----------------------------------------------------------------------
      ELSE
!
        NSIZE=(ITE-ITS+1)*(JTE-JTS+1)
        ALLOCATE(VALUES(1:NSIZE))
!
        N=0
        DO J=JTS,JTE
        DO I=ITS,ITE
          N=N+1
          IF(K==1) THEN
            VALUES(N)=ARRAY1(I,J)
          ELSE
            VALUES(N)=ARRAY2(I,J)
          ENDIF
        ENDDO
        ENDDO
!
        IT_REM(1)=ITS
        IT_REM(2)=ITE
        JT_REM(1)=JTS
        JT_REM(2)=JTE
!
        CALL MPI_SEND(IT_REM,2,MPI_INTEGER,0,MYPE                       &
                     ,MPI_COMM_COMP,ISEND)
        CALL MPI_SEND(JT_REM,2,MPI_INTEGER,0,MYPE                       &
                     ,MPI_COMM_COMP,ISEND)
!
        CALL MPI_SEND(VALUES,NSIZE,MPI_REAL,0,MYPE                      &
                     ,MPI_COMM_COMP,ISEND)
!
        DEALLOCATE(VALUES)
!
      ENDIF
!----------------------------------------------------------------------
!
      CALL MPI_BARRIER(MPI_COMM_COMP,IRTN)
!
      IF(MYPE==0)THEN
!
          IF(K==1) THEN
            minlat=minval(TWRITE)
            maxlat=maxval(TWRITE)
          ELSE
            minlon=minval(TWRITE)
            maxlon=maxval(TWRITE)
          ENDIF
!
      ENDIF
!
!----------------------------------------------------------------------
      ENDDO
!
      IF(MYPE==0)THEN
          WRITE(IUNIT)minlat,maxlat,minlon,maxlon
          CLOSE(IUNIT)
      ENDIF
!----------------------------------------------------------------------
!
      END SUBROUTINE LAT_LON_BNDS
!
!-----------------------------------------------------------------------
!######################################################################
!----------------------------------------------------------------------
      SUBROUTINE HMAXMIN(ARRAY,KK,FIELD,NTSD,MYPE,NPES,MPI_COMM_COMP   &
                        ,IDS,IDE,JDS,JDE                               &
                        ,IMS,IME,JMS,JME                               &
                        ,ITS,ITE,JTS,JTE                               &
                        ,DOMAIN_ID )
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
      USE MODULE_INCLUDE
      IMPLICIT NONE
!----------------------------------------------------------------------
!
!------------------------
!***  Argument Variables
!------------------------
!
      INTEGER(KIND=KINT),INTENT(IN) :: IDS,IDE,JDS,JDE                 &
                                      ,IMS,IME,JMS,JME                 &
                                      ,ITS,ITE,JTS,JTE                 &
                                      ,KK,MPI_COMM_COMP,MYPE,NPES,NTSD
!
      REAL(KIND=KFPT),DIMENSION(IMS:IME,JMS:JME,KK),INTENT(IN) :: ARRAY
!
      CHARACTER(*),INTENT(IN) :: FIELD
!
      INTEGER(kind=KINT),INTENT(IN),OPTIONAL :: DOMAIN_ID
!
!--------------------
!*** Local Variables
!--------------------
!
      INTEGER :: IUNIT=23
!
      INTEGER,DIMENSION(MPI_STATUS_SIZE) :: JSTAT
      INTEGER,DIMENSION(MPI_STATUS_SIZE,4) :: STATUS_ARRAY
      INTEGER(KIND=KINT),DIMENSION(2) :: IM_REM,JM_REM,IT_REM,JT_REM
!
      INTEGER(KIND=KINT) :: I,IENDX,IER,IPE,IRECV,IRTN,ISEND           &
                           ,J,K,N,NSIZE
      INTEGER(kind=KINT) :: IMAX,IMIN,JMAX,JMIN
      INTEGER(KIND=KINT) :: ITS_REM,ITE_REM,JTS_REM,JTE_REM
!
      REAL(KIND=KFPT),DIMENSION(IDS:IDE,JDS:JDE) :: ARRAY_FULL
      REAL(KIND=KFPT),ALLOCATABLE,DIMENSION(:) :: VALUES
      REAL(kind=KFPT) :: VALMAX,VALMIN
!
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
!
      IF(MYPE==0)THEN
        WRITE(0,*)' '
        WRITE(0,11101)FIELD,NTSD,DOMAIN_ID
11101   FORMAT(' For ',A,' at timestep ',I4,' on domain #',I2)
      ENDIF
!
!----------------------------------------------------------------------
      DO 500 K=1,KK
!----------------------------------------------------------------------
!
      IF(MYPE==0)THEN
        DO J=JTS,JTE
        DO I=ITS,ITE
          ARRAY_FULL(I,J)=ARRAY(I,J,K)
        ENDDO
        ENDDO
!
        DO IPE=1,NPES-1
          CALL MPI_RECV(IT_REM,2,MPI_INTEGER,IPE,IPE                    &
                       ,MPI_COMM_COMP,JSTAT,IRECV)
          CALL MPI_RECV(JT_REM,2,MPI_INTEGER,IPE,IPE                    &
                       ,MPI_COMM_COMP,JSTAT,IRECV)
!
          ITS_REM=IT_REM(1)
          ITE_REM=IT_REM(2)
          JTS_REM=JT_REM(1)
          JTE_REM=JT_REM(2)
!
          NSIZE=(ITE_REM-ITS_REM+1)*(JTE_REM-JTS_REM+1)
          ALLOCATE(VALUES(1:NSIZE))
!
          CALL MPI_RECV(VALUES,NSIZE,MPI_REAL,IPE,IPE                   &
                       ,MPI_COMM_COMP,JSTAT,IRECV)
          N=0
          DO J=JTS_REM,JTE_REM
            DO I=ITS_REM,ITE_REM
              N=N+1
              ARRAY_FULL(I,J)=VALUES(N)
            ENDDO
          ENDDO
!
          DEALLOCATE(VALUES)
!
        ENDDO
!
!----------------------------------------------------------------------
      ELSE
!
        NSIZE=(ITE-ITS+1)*(JTE-JTS+1)
        ALLOCATE(VALUES(1:NSIZE))
!
        N=0
        DO J=JTS,JTE
        DO I=ITS,ITE
          N=N+1
          VALUES(N)=ARRAY(I,J,K)
        ENDDO
        ENDDO
!
        IT_REM(1)=ITS
        IT_REM(2)=ITE
        JT_REM(1)=JTS
        JT_REM(2)=JTE
!
        CALL MPI_SEND(IT_REM,2,MPI_INTEGER,0,MYPE                       &
                     ,MPI_COMM_COMP,ISEND)
        CALL MPI_SEND(JT_REM,2,MPI_INTEGER,0,MYPE                       &
                     ,MPI_COMM_COMP,ISEND)
!
        CALL MPI_SEND(VALUES,NSIZE,MPI_REAL,0,MYPE                      &
                     ,MPI_COMM_COMP,ISEND)
!
        DEALLOCATE(VALUES)
!
      ENDIF
!----------------------------------------------------------------------
!
      CALL MPI_BARRIER(MPI_COMM_COMP,IRTN)
!
      IF(MYPE==0)THEN
!
        VALMAX=-1.E9
        IMAX=0
        JMAX=0
        VALMIN= 1.E9
        IMIN=0
        JMIN=0
!
        DO J=JDS,JDE-1
        DO I=IDS,IDE-1
          IF(ARRAY_FULL(I,J)>VALMAX)THEN
            VALMAX=ARRAY_FULL(I,J)
            IMAX=I 
            JMAX=J 
          ENDIF
          IF(ARRAY_FULL(I,J)<VALMIN)THEN
            VALMIN=ARRAY_FULL(I,J)
            IMIN=I 
            JMIN=J 
          ENDIF
        ENDDO
        ENDDO
!
        WRITE(0,*)' '
        WRITE(0,11102)VALMAX,IMAX,JMAX,K
        WRITE(0,11103)VALMIN,IMIN,JMIN,K
11102   FORMAT(' Max value is ',E12.5,' at(',I4,',',I4,',',I2,')')
11103   FORMAT(' Min value is ',E12.5,' at(',I4,',',I4,',',I2,')')
!
      ENDIF
!
!----------------------------------------------------------------------
!
  500 CONTINUE
!
!----------------------------------------------------------------------
!
      END SUBROUTINE HMAXMIN
!
!-----------------------------------------------------------------------
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!----------------------------------------------------------------------
      SUBROUTINE VMAXMIN(ARRAY,KK,FIELD,NTSD,MYPE,NPES,MPI_COMM_COMP   &
                        ,IDS,IDE,JDS,JDE                               &
                        ,IMS,IME,JMS,JME                               &
                        ,ITS,ITE,JTS,JTE                               &
                        ,DOMAIN_ID )
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
      USE MODULE_INCLUDE
      IMPLICIT NONE
!----------------------------------------------------------------------
!
!------------------------
!***  Argument Variables
!------------------------
!
      INTEGER(kind=KINT),INTENT(IN) :: IDS,IDE,JDS,JDE                 &
                                      ,IMS,IME,JMS,JME                 &
                                      ,ITS,ITE,JTS,JTE                 &
                                      ,KK,MPI_COMM_COMP,MYPE,NPES,NTSD
!
      REAL(kind=KFPT),DIMENSION(IMS:IME,JMS:JME,KK),INTENT(IN) :: ARRAY
!
      CHARACTER(*),INTENT(IN) :: FIELD
!
      INTEGER(kind=KINT),INTENT(IN),OPTIONAL :: DOMAIN_ID
!
!--------------------
!*** Local Variables
!--------------------
!
      INTEGER :: IUNIT=23
!
      INTEGER,DIMENSION(MPI_STATUS_SIZE) :: JSTAT
      INTEGER,DIMENSION(MPI_STATUS_SIZE,4) :: STATUS_ARRAY
      INTEGER(kind=KINT),DIMENSION(2) :: IM_REM,JM_REM,IT_REM,JT_REM
!
      INTEGER(kind=KINT) :: I,IENDX,IER,IPE,IRECV,IRTN,ISEND           &
                           ,J,K,N,NSIZE
      INTEGER(kind=KINT) :: IMAX,IMIN,JMAX,JMIN
      INTEGER(kind=KINT) :: ITS_REM,ITE_REM,JTS_REM,JTE_REM
!
      REAL(kind=KFPT),DIMENSION(IDS:IDE,JDS:JDE) :: ARRAY_FULL
      REAL(kind=KFPT),ALLOCATABLE,DIMENSION(:) :: VALUES
      REAL(kind=KFPT) :: VALMAX,VALMIN
!
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
!
      IF(MYPE==0)THEN
        WRITE(0,*)' '
        WRITE(0,11101)FIELD,NTSD,DOMAIN_ID
11101   FORMAT(' For ',A,' at timestep ',I4,' on domain #',I2)
      ENDIF
!
!----------------------------------------------------------------------
      DO 500 K=1,KK
!----------------------------------------------------------------------
!
      IF(MYPE==0)THEN
        DO J=JTS,JTE
        DO I=ITS,ITE
          ARRAY_FULL(I,J)=ARRAY(I,J,K)
        ENDDO
        ENDDO
!
        DO IPE=1,NPES-1
          CALL MPI_RECV(IT_REM,2,MPI_INTEGER,IPE,IPE                    &
                       ,MPI_COMM_COMP,JSTAT,IRECV)
          CALL MPI_RECV(JT_REM,2,MPI_INTEGER,IPE,IPE                    &
                       ,MPI_COMM_COMP,JSTAT,IRECV)
!
          ITS_REM=IT_REM(1)
          ITE_REM=IT_REM(2)
          JTS_REM=JT_REM(1)
          JTE_REM=JT_REM(2)
!
          NSIZE=(ITE_REM-ITS_REM+1)*(JTE_REM-JTS_REM+1)
          ALLOCATE(VALUES(1:NSIZE))
!
          CALL MPI_RECV(VALUES,NSIZE,MPI_REAL,IPE,IPE                   &
                       ,MPI_COMM_COMP,JSTAT,IRECV)
          N=0
          DO J=JTS_REM,JTE_REM
            DO I=ITS_REM,ITE_REM
              N=N+1
              ARRAY_FULL(I,J)=VALUES(N)
            ENDDO
          ENDDO
!
          DEALLOCATE(VALUES)
!
        ENDDO
!
!----------------------------------------------------------------------
!
      ELSE
!
        NSIZE=(ITE-ITS+1)*(JTE-JTS+1)
        ALLOCATE(VALUES(1:NSIZE))
!
        N=0
        DO J=JTS,JTE
        DO I=ITS,ITE
          N=N+1
          VALUES(N)=ARRAY(I,J,K)
        ENDDO
        ENDDO
!
        IT_REM(1)=ITS
        IT_REM(2)=ITE
        JT_REM(1)=JTS
        JT_REM(2)=JTE
!
        CALL MPI_SEND(IT_REM,2,MPI_INTEGER,0,MYPE                       &
                     ,MPI_COMM_COMP,ISEND)
        CALL MPI_SEND(JT_REM,2,MPI_INTEGER,0,MYPE                       &
                     ,MPI_COMM_COMP,ISEND)
!
        CALL MPI_SEND(VALUES,NSIZE,MPI_REAL,0,MYPE                      &
                     ,MPI_COMM_COMP,ISEND)
!
        DEALLOCATE(VALUES)
!
      ENDIF
!----------------------------------------------------------------------
!
      CALL MPI_BARRIER(MPI_COMM_COMP,IRTN)
!
      IF(MYPE==0)THEN
!
        VALMAX=-1.E9
        IMAX=0
        JMAX=0
        VALMIN= 1.E9
        IMIN=0
        JMIN=0
!
        DO J=JDS,JDE-1
        DO I=IDS,IDE-1
          IF(ARRAY_FULL(I,J)>VALMAX)THEN
            VALMAX=ARRAY_FULL(I,J)
            IMAX=I 
            JMAX=J 
          ENDIF
          IF(ARRAY_FULL(I,J)<VALMIN)THEN
            VALMIN=ARRAY_FULL(I,J)
            IMIN=I 
            JMIN=J 
          ENDIF
        ENDDO
        ENDDO
!
        WRITE(0,*)' '
        WRITE(0,11102)VALMAX,IMAX,JMAX,K
        WRITE(0,11103)VALMIN,IMIN,JMIN,K
11102   FORMAT(' Max value is ',E12.5,' at(',I4,',',I4,',',I2,')')
11103   FORMAT(' Min value is ',E12.5,' at(',I4,',',I4,',',I2,')')
!
      ENDIF
!
!----------------------------------------------------------------------
!
  500 CONTINUE
!
!----------------------------------------------------------------------
!
      END SUBROUTINE VMAXMIN
!
!-----------------------------------------------------------------------
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!----------------------------------------------------------------------
      SUBROUTINE EXIT(NAME,PINT,T,Q,U,V,Q2,W                           &
                     ,NTSD,MYPE,ID_DOM,MPI_COMM_COMP                   &
                     ,IDS,IDE,JDS,JDE,LM                               &
                     ,IMS,IME,JMS,JME                                  &
                     ,ITS,ITE,JTS,JTE)
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
      IMPLICIT NONE
!----------------------------------------------------------------------
      INCLUDE "mpif.h"
!----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,LM                         &
                           ,IMS,IME,JMS,JME                            &
                           ,ITS,ITE,JTS,JTE                            &
                           ,ID_DOM                                     &
                           ,MYPE,MPI_COMM_COMP,NTSD
!
      REAL,DIMENSION(IMS:IME,JMS:JME,LM),INTENT(IN) :: T,Q,U,V,Q2,W
      REAL,DIMENSION(IMS:IME,JMS:JME,LM+1),INTENT(IN) :: PINT
      CHARACTER(*),INTENT(IN) :: NAME
!
      INTEGER :: I,J,K,IEND,IERR,IRET
      CHARACTER(256) :: ERRMESS
      LOGICAL :: E_BDY,S_BDY
!----------------------------------------------------------------------
      IRET=0
  100 FORMAT(' EXIT ',A,' AT NTSD=',I5)
      IEND=ITE
      S_BDY=(JTS==JDS)
      E_BDY=(ITE==IDE-1)
!
      DO K=1,LM
      DO J=JTS,JTE
      IEND=ITE
      IF(E_BDY.AND.MOD(J,2)==0)IEND=ITE-1
!
      DO I=ITS,IEND
        IF(T(I,J,K)>330..OR.T(I,J,K)<180..OR.T(I,J,K)/=T(I,J,K))THEN
          WRITE(0,100)NAME,NTSD
          WRITE(0,200)I,J,K,T(I,J,K),MYPE,ID_DOM,NTSD
  200     FORMAT(' BAD VALUE I=',I3,' J=',I3,' K=',I2,' T=',E12.5      &
                ,' MYPE=',I3,' ID_DOM=',I2,' NTSD=',I5)
          IRET=666
          return
!         WRITE(ERRMESS,205)NAME,T(I,J,K),I,J,K,MYPE
  205     FORMAT(' EXIT ',A,' TEMPERATURE=',E12.5                      &
                ,' AT (',I3,',',I2,',',I3,')',' MYPE=',I3)
!         CALL MPI_ABORT(MPI_COMM_WORLD,1,IERR)
        ELSEIF(Q(I,J,K)<-1.5E-4.OR.Q(I,J,K)>30.E-3                     &
               .OR.Q(I,J,K)/=Q(I,J,K))THEN
          WRITE(0,100)NAME,NTSD
          WRITE(0,300)I,J,K,Q(I,J,K),MYPE,ID_DOM,NTSD
  300     FORMAT(' BAD VALUE I=',I3,' J=',I3,' K=',I2,' Q=',E12.5      &
                ,' MYPE=',I3,' ID_DOM=',I2,' NTSD=',I5)
          IRET=666
          return
!         WRITE(ERRMESS,305)NAME,Q(I,J,K),I,J,K,MYPE
  305     FORMAT(' EXIT ',A,' SPEC HUMIDITY=',E12.5                    &
                ,' AT (',I3,',',I2,',',I3,')',' MYPE=',I3)
!         CALL MPI_ABORT(MPI_COMM_WORLD,1,IERR)
        ELSEIF(PINT(I,J,K)<0..OR.PINT(I,J,K)/=PINT(I,J,K))THEN
          WRITE(0,100)NAME,NTSD
          WRITE(0,315)I,J,K,PINT(I,J,K),MYPE,ID_DOM,NTSD
  315     FORMAT(' BAD VALUE I=',I3,' J=',I3,' K=',I2,' PINT=',E12.5      &
                ,' MYPE=',I3,' ID_DOM=',I2,' NTSD=',I5)
          IRET=666
          return
!         CALL MPI_ABORT(MPI_COMM_WORLD,1,IERR)
        ELSEIF(W(I,J,K)/=W(I,J,K))THEN
          WRITE(0,100)NAME,NTSD
          WRITE(0,325)I,J,K,W(I,J,K),MYPE,ID_DOM,NTSD
  325     FORMAT(' BAD VALUE I=',I3,' J=',I3,' K=',I2,' W=',E12.5      &
                ,' MYPE=',I3,' ID_DOM=',I2,' NTSD=',I5)
          IRET=666
          return
!         CALL MPI_ABORT(MPI_COMM_WORLD,1,IERR)
        ENDIF
      ENDDO
      ENDDO
      ENDDO
!
      DO K=1,LM
      DO J=JTS,JTE
      IEND=ITE
      IF(E_BDY.AND.MOD(J,2)==1)IEND=ITE-1
      DO I=ITS,IEND
        IF(ABS(U(I,J,K))>125..OR.ABS(V(I,J,K))>125.                    &
               .OR.U(I,J,K)/=U(I,J,K).OR.V(I,J,K)/=V(I,J,K))THEN
          WRITE(0,100)NAME,NTSD
          WRITE(0,400)I,J,K,U(I,J,K),V(I,J,K),MYPE,NTSD
  400     FORMAT(' BAD VALUE I=',I3,' J=',I3,' K=',I2,' U=',E12.5      &
                ,' V=',E12.5,' MYPE=',I3,' NTSD=',I5)
          IRET=666
          return
!         WRITE(ERRMESS,405)NAME,U(I,J,K),V(I,J,K),I,J,K,MYPE
  405     FORMAT(' EXIT ',A,' U=',E12.5,' V=',E12.5                    &
                ,' AT (',I3,',',I2,',',I3,')',' MYPE=',I3)
!         CALL MPI_ABORT(MPI_COMM_WORLD,1,IERR)
        ENDIF
      ENDDO
      ENDDO
      ENDDO
!----------------------------------------------------------------------
      END SUBROUTINE EXIT
!----------------------------------------------------------------------
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!-----------------------------------------------------------------------
      SUBROUTINE EXIT_PHY(NAME,T,Q,U,V,Q2                              &
                         ,NTSD,MYPE,MPI_COMM_COMP                      &
                         ,IDS,IDE,JDS,JDE,LM                           &
                         ,IMS,IME,JMS,JME                              &
                         ,ITS,ITE,JTS,JTE)
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
      IMPLICIT NONE
!----------------------------------------------------------------------
      INCLUDE "mpif.h"
!----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,LM                         &
                           ,IMS,IME,JMS,JME                            &
                           ,ITS,ITE,JTS,JTE                            &
                           ,MYPE,MPI_COMM_COMP,NTSD
!
      REAL,DIMENSION(IMS:IME,JMS:JME,LM),INTENT(IN) :: T,Q,U,V,Q2
      CHARACTER(*),INTENT(IN) :: NAME
!
      INTEGER :: I,J,K,IEND,IERR,IRET
      CHARACTER(256) :: ERRMESS
      LOGICAL :: E_BDY,S_BDY
!----------------------------------------------------------------------
      IRET=0
  100 FORMAT(' EXIT ',A,' AT NTSD=',I5)
      IEND=ITE
      S_BDY=(JTS==JDS)
      E_BDY=(ITE==IDE-1)
!
      DO K=1,LM
      DO J=JTS,JTE
      IEND=ITE
      IF(E_BDY.AND.MOD(J,2)==0)IEND=ITE-1
!
      DO I=ITS,IEND
        IF(T(I,J,K)>330..OR.T(I,J,K)<180..OR.T(I,J,K)/=T(I,J,K))THEN
          WRITE(0,100)NAME,NTSD
          WRITE(0,200)I,J,K,T(I,J,K),MYPE,NTSD
  200     FORMAT(' BAD VALUE I=',I3,' J=',I3,' K=',I2,' T=',E12.5      &
                ,' MYPE=',I3,' NTSD=',I5)
          IRET=666
          return
!         WRITE(ERRMESS,205)NAME,T(I,J,K),I,J,K,MYPE
  205     FORMAT(' EXIT ',A,' TEMPERATURE=',E12.5                      &
                ,' AT (',I3,',',I2,',',I3,')',' MYPE=',I3)
!         CALL MPI_ABORT(MPI_COMM_WORLD,1,IERR)
        ELSEIF(Q(I,J,K)<-1.5E-4.OR.Q(I,J,K)>30.E-3                     &
               .OR.Q(I,J,K)/=Q(I,J,K))THEN
          WRITE(0,100)NAME,NTSD
          WRITE(0,300)I,J,K,Q(I,J,K),MYPE,NTSD
  300     FORMAT(' BAD VALUE I=',I3,' J=',I3,' K=',I2,' Q=',E12.5      &
                ,' MYPE=',I3,' NTSD=',I5)
          IRET=666
          return
!         WRITE(ERRMESS,305)NAME,Q(I,J,K),I,J,K,MYPE
  305     FORMAT(' EXIT ',A,' SPEC HUMIDITY=',E12.5                    &
                ,' AT (',I3,',',I2,',',I3,')',' MYPE=',I3)
!         CALL MPI_ABORT(MPI_COMM_WORLD,1,IERR)
!       ELSEIF(Q2(I,J,K)<-1.E-4.OR.Q2(I,J,K)>100.                      &
        ELSEIF(Q2(I,J,K)<-0.15.OR.Q2(I,J,K)>100.                      &
               .OR.Q2(I,J,K)/=Q2(I,J,K))THEN
          WRITE(0,100)NAME,NTSD
          WRITE(0,310)I,J,K,Q2(I,J,K),MYPE,NTSD
  310     FORMAT(' BAD VALUE I=',I3,' J=',I3,' K=',I2,' Q2=',E12.5     &
                ,' MYPE=',I3,' NTSD=',I5)
          IRET=666
          return
!         WRITE(ERRMESS,315)NAME,Q2(I,J,K),I,J,K,MYPE
  315     FORMAT(' EXIT ',A,' TKE=',E12.5                              &
                ,' AT (',I3,',',I2,',',I3,')',' MYPE=',I3)
!         CALL MPI_ABORT(MPI_COMM_WORLD,1,IERR)
        ENDIF
      ENDDO
      ENDDO
      ENDDO
!
      DO K=1,LM
      DO J=JTS,JTE
      IEND=ITE
      IF(E_BDY.AND.MOD(J,2)==1)IEND=ITE-1
      DO I=ITS,IEND
        IF(ABS(U(I,J,K))>125..OR.ABS(V(I,J,K))>125.                    &
               .OR.U(I,J,K)/=U(I,J,K).OR.V(I,J,K)/=V(I,J,K))THEN
          WRITE(0,100)NAME,NTSD
          WRITE(0,400)I,J,K,U(I,J,K),V(I,J,K),MYPE,NTSD
  400     FORMAT(' BAD VALUE I=',I3,' J=',I3,' K=',I2,' U=',E12.5      &
                ,' V=',E12.5,' MYPE=',I3,' NTSD=',I5)
          IRET=666
          return
!         WRITE(ERRMESS,405)NAME,U(I,J,K),V(I,J,K),I,J,K,MYPE
  405     FORMAT(' EXIT ',A,' U=',E12.5,' V=',E12.5                    &
                ,' AT (',I3,',',I2,',',I3,')',' MYPE=',I3)
!         CALL MPI_ABORT(MPI_COMM_WORLD,1,IERR)
        ENDIF
      ENDDO
      ENDDO
      ENDDO
!----------------------------------------------------------------------
      END SUBROUTINE EXIT_PHY
!----------------------------------------------------------------------
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!-----------------------------------------------------------------------
      SUBROUTINE FIELD_STATS(FIELD,MYPE,MPI_COMM_COMP,LM &
                            ,ITS,ITE,JTS,JTE &
                            ,IMS,IME,JMS,JME &
                            ,IDS,IDE,JDS,JDE)
!----------------------------------------------------------------------
!***
!***  GENERATE STANDARD STATISTICS FOR THE DESIRED FIELD.
!***
!----------------------------------------------------------------------
      USE MODULE_INCLUDE
      IMPLICIT NONE
!----------------------------------------------------------------------
!
      INTEGER(KIND=KINT),INTENT(IN) :: LM,MPI_COMM_COMP,MYPE
      INTEGER(KIND=KINT),INTENT(IN) :: ITS,ITE,JTS,JTE &
                                      ,IMS,IME,JMS,JME &
                                      ,IDS,IDE,JDS,JDE
!
      REAL(KIND=KFPT),DIMENSION(IMS:IME,JMS:JME,1:LM) &
                                                  ,INTENT(IN) :: FIELD
!
!----------------------------------------------------------------------
!***  LOCAL
!----------------------------------------------------------------------
!
      INTEGER(KIND=KINT) :: I,IRTN,I_BY_J,J,K,KFLIP
!
      REAL(KIND=KFPT) :: FIJK,FMAXK,FMINK
      REAL(KIND=KDBL) :: F_MEAN,POINTS,RMS,ST_DEV,SUMFK,SUMF2K
      REAL(KIND=KFPT),DIMENSION(1:LM) :: FMAX,FMAX_0,FMIN,FMIN_0
      REAL(KIND=KDBL),DIMENSION(1:LM) :: SUMF,SUMF_0,SUMF2,SUMF2_0
!----------------------------------------------------------------------
!
      I_BY_J=(IDE-IDS+1)*(JDE-JDS+1)
!
      layer_loop:  DO K=1,LM
!
        FMAXK=-1.E10
        FMINK=1.E10
        SUMFK=0.
        SUMF2K=0.
!
        DO J=JTS,JTE
          DO I=ITS,ITE
            FIJK=FIELD(I,J,K)
            FMAXK=MAX(FMAXK,FIJK)
            FMINK=MIN(FMINK,FIJK)
            SUMFK=SUMFK+FIJK
            SUMF2K=SUMF2K+FIJK*FIJK
          ENDDO
        ENDDO
!
        FMAX(K)=FMAXK
        FMIN(K)=FMINK
        SUMF(K)=SUMFK
        SUMF2(K)=SUMF2K
!
      ENDDO layer_loop
!
!----------------------------------------------------------------------
!***  GLOBAL STATS
!----------------------------------------------------------------------
!
      CALL MPI_REDUCE(SUMF,SUMF_0,LM,MPI_REAL8,MPI_SUM,0              &
                     ,MPI_COMM_COMP,IRTN)
      CALL MPI_REDUCE(SUMF2,SUMF2_0,LM,MPI_REAL8,MPI_SUM,0            &
                     ,MPI_COMM_COMP,IRTN)
      CALL MPI_REDUCE(FMAX,FMAX_0,LM,MPI_REAL,MPI_MAX,0               &
                     ,MPI_COMM_COMP,IRTN)
      CALL MPI_REDUCE(FMIN,FMIN_0,LM,MPI_REAL,MPI_MIN,0               &
                     ,MPI_COMM_COMP,IRTN)
!
      IF(MYPE==0)THEN
        POINTS=I_BY_J
        DO K=1,LM
          F_MEAN=SUMF_0(K)/POINTS
          ST_DEV=SQRT((POINTS*SUMF2_0(K)-SUMF_0(K)*SUMF_0(K))/         &
                      (POINTS*(POINTS-1)))
          RMS=SQRT(SUMF2_0(K)/POINTS)
          WRITE(0,101)K,FMAX_0(K),FMIN_0(K)
          WRITE(0,102)F_MEAN,ST_DEV,RMS
  101     FORMAT(' LAYER=',I2,' MAX=',E13.6,' MIN=',E13.6)
  102     FORMAT(9X,' MEAN=',E13.6,' STDEV=',E13.6,' RMS=',E13.6)
        ENDDO
      ENDIF
!----------------------------------------------------------------------
!
      END SUBROUTINE FIELD_STATS
!
!-----------------------------------------------------------------------
!#######################################################################
!-----------------------------------------------------------------------
      SUBROUTINE WRT_PCP(ARRAY,MYPE,NPES,MPI_COMM_COMP,MY_DOMAIN_ID    &
                    ,IHOUR                                             &
                    ,IDS,IDE,JDS,JDE                                   &
                    ,IMS,IME,JMS,JME                                   &
                    ,ITS,ITE,JTS,JTE)
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
      USE MODULE_INCLUDE
      IMPLICIT NONE
!----------------------------------------------------------------------
!----------------------------------------------------------------------
      INTEGER(KIND=KINT),INTENT(IN) :: IDS,IDE,JDS,JDE                 &
                                      ,IMS,IME,JMS,JME                 &
                                      ,ITS,ITE,JTS,JTE                 &
                                      ,MPI_COMM_COMP,MYPE,MY_DOMAIN_ID &
                                      ,NPES,IHOUR
!
      REAL(KIND=KFPT),DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: ARRAY
!
!*** LOCAL VARIABLES
!
      INTEGER,DIMENSION(MPI_STATUS_SIZE) :: JSTAT
      INTEGER(KIND=KINT),DIMENSION(2)    :: IT_REM,JT_REM
!
      INTEGER(KIND=KINT) :: I,J,N,NN,IPE,IRECV,ISEND,NSIZE,IER,NUNIT_PCP
      INTEGER(KIND=KINT) :: ITS_REM,ITE_REM,JTS_REM,JTE_REM
!
      REAL(KIND=KFPT),DIMENSION(IDS:IDE,JDS:JDE) :: TWRITE
      REAL(KIND=KFPT),ALLOCATABLE,DIMENSION(:)   :: VALUES
!
      CHARACTER(14) :: FILENAME
      CHARACTER(6)  :: FMT_ID='(I2.2)',FMT_HR='(I1.1)'
      CHARACTER(2)  :: CHAR_ID
      CHARACTER(1)  :: CHAR_HR
      LOGICAL       :: OPENED
!
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
!
      IF(MYPE==0)THEN
        DO J=JTS,JTE
        DO I=ITS,ITE
          TWRITE(I,J)=ARRAY(I,J)
        ENDDO
        ENDDO
!
        DO IPE=1,NPES-1
          CALL MPI_RECV(IT_REM,2,MPI_INTEGER,IPE,IPE                    &
                       ,MPI_COMM_COMP,JSTAT,IRECV)
          CALL MPI_RECV(JT_REM,2,MPI_INTEGER,IPE,IPE                    &
                       ,MPI_COMM_COMP,JSTAT,IRECV)
!
          ITS_REM=IT_REM(1)
          ITE_REM=IT_REM(2)
          JTS_REM=JT_REM(1)
          JTE_REM=JT_REM(2)
!
          NSIZE=(ITE_REM-ITS_REM+1)*(JTE_REM-JTS_REM+1)
          ALLOCATE(VALUES(1:NSIZE))
!
          CALL MPI_RECV(VALUES,NSIZE,MPI_REAL,IPE,IPE                   &
                       ,MPI_COMM_COMP,JSTAT,IRECV)
          N=0
          DO J=JTS_REM,JTE_REM
            DO I=ITS_REM,ITE_REM
              N=N+1
              TWRITE(I,J)=VALUES(N)
            ENDDO
          ENDDO
!
          DEALLOCATE(VALUES)
!
        ENDDO
!
        WRITE(CHAR_ID,FMT_ID)MY_DOMAIN_ID
        WRITE(CHAR_HR,FMT_HR)IHOUR
        FILENAME='pcp.hr'//CHAR_HR//'.'//CHAR_ID//'.bin'
!
        DO NN=51,99
          INQUIRE(NN,opened=OPENED)
          IF(.NOT.OPENED)THEN
            NUNIT_PCP=NN
            EXIT
          ENDIF
        ENDDO
!
        OPEN(unit=NUNIT_PCP,file=FILENAME,form='UNFORMATTED'        &
            ,STATUS='REPLACE',IOSTAT=IER)
        IF(IER/=0)THEN
          WRITE(0,*)' Failed to open ',FILENAME,' in WRT_PCP ier=',IER
        ENDIF
        WRITE(NUNIT_PCP)((TWRITE(I,J)*1000.,I=IDS,IDE),J=JDS,JDE)
        CLOSE(NUNIT_PCP)
!
      ELSE
!
        NSIZE=(ITE-ITS+1)*(JTE-JTS+1)
        ALLOCATE(VALUES(1:NSIZE))
!
        N=0
        DO J=JTS,JTE
        DO I=ITS,ITE
          N=N+1
          VALUES(N)=ARRAY(I,J)
        ENDDO
        ENDDO
!
        IT_REM(1)=ITS
        IT_REM(2)=ITE
        JT_REM(1)=JTS
        JT_REM(2)=JTE
!
        CALL MPI_SEND(IT_REM,2,MPI_INTEGER,0,MYPE                       &
                     ,MPI_COMM_COMP,ISEND)
        CALL MPI_SEND(JT_REM,2,MPI_INTEGER,0,MYPE                       &
                     ,MPI_COMM_COMP,ISEND)
!
        CALL MPI_SEND(VALUES,NSIZE,MPI_REAL,0,MYPE                      &
                     ,MPI_COMM_COMP,ISEND)
!
        DEALLOCATE(VALUES)
!
      ENDIF
!
!----------------------------------------------------------------------
!
!----------------------------------------------------------------------
!
      END SUBROUTINE WRT_PCP
!
!----------------------------------------------------------------------
!
!----------------------------------------------------------------------
      SUBROUTINE CALMICT(P1D,T1D,Q1D,C1D,FI1D,FR1D,FS1D,CUREFL, &
                       DBZ1,I,J, Ilook,Jlook) 

      USE MODULE_MP_ETANEW, ONLY : FERRIER_INIT, GPVS,FPVS,FPVS0,NX,RQR_DRmin, &
                                   RQR_DRmax,MASSI,CN0R0,CN0r_DMRmin,CN0r_DMRmax
      USE MODULE_CONSTANTS, ONLY : R_D,P608

      IMPLICIT NONE

      REAL, PARAMETER :: &
      FMW=18.015,FMD=28.964,EPS=FMW/FMD,ONEPS=1.-EPS,TFRZ=273.15, &
      PQ0=379.90516,EPSQ=1.E-12, &
      QCLDmin=1.E-6,  &
      STBOL=1./5.67E-8, DBZmin=-20., abscoef=8.0, abscoefi=5.0, &
      XLAI=4.0, &
      RHmin=1.0E-6, &
      SMALL=1.E-6 !  RHgrd=1.0

      REAL, PARAMETER :: DMImin=.05e-3, DMImax=1.e-3, &
                         XMImin=1.e6*DMImin, XMImax=1.e6*DMImax
      INTEGER, PARAMETER :: MDImin=XMImin, MDImax=XMImax
!
!-----------------------------------------------------------------------
!
!--- Mean rain drop diameters vary from 50 microns to 1000 microns (1 mm)
!
      REAL, PARAMETER :: DMRmin=.05E-3, DMRmax=1.E-3, DelDMR=1.E-6,        &
         XMRmin=1.E6*DMRmin, XMRmax=1.E6*DMRmax
      INTEGER, PARAMETER :: MDRmin=XMRmin, MDRmax=XMRmax

      INTEGER INDEXS, INDEXR, I, J, Ilook, Jlook
      REAL, PARAMETER :: Cice=1.634e13

      REAL ::  NLICE, N0r,Ztot,Zrain,Zice,Zconv
      REAL ::  P1D,T1D,Q1D,C1D,                                            &
               FI1D,FR1D,FS1D,CUREFL,                                      &
               QW1,QI1,QR1,QS1,                                            &
               DBZ1

      REAL :: FLARGE, FSMALL, WV, ESAT, TC, WC, FPVS_MINE,  RHO,  &
              QSAT, RRHO, RQR, ZMIN, Fice, Frain, Rimef , XLI, QICE, DRmm, &
              DLI, XSIMASS, XLIMASS, DUM, WVQW, QSIGRD, QLICE, FLIMASS

      REAL, PARAMETER ::                                                   &
     &  RHgrd=1.                                                           &
     & ,T_ICE=-40.                                                         &
     & ,NLImax=5.E3                                                        &
     & ,NLImin=1.E3                                                        &
     & ,N0r0=8.E6                                                          &
     & ,N0rmin=1.E4

! ---------
      Zmin=10.**(0.1*DBZmin)
          QW1=0.
          QI1=0.
          QR1=0.
          QS1=0.
          DBZ1=DBZmin
!          DBZR1=DBZmin
!          DBZI1=DBZmin
!          DBZC1=DBZmin

          Zrain=0.            !--- Radar reflectivity from rain
          Zice=0.             !--- Radar reflectivity from ice
          Zconv=CUREFL   !--- Radar reflectivity from convection
          IF (C1D .LE. EPSQ) THEN
!
!--- Skip rest of calculatiions if no condensate is present
!
            GO TO 10
          ELSE
            WC=C1D
          ENDIF
!
!--- Code below is from GSMDRIVE for determining:
!    QI1 - total ice (cloud ice & snow) mixing ratio
!    QW1 - cloud water mixing ratio
!    QR1 - rain mixing ratio

          TC=T1D-TFRZ
          Fice=FI1D
          Frain=FR1D
          IF (TC.LE.T_ICE .OR. Fice.GE.1.) THEN
            QI1=WC
          ELSE IF (Fice .LE. 0.) THEN
            QW1=WC
          ELSE
            QI1=Fice*WC
            QW1=WC-QI1
          ENDIF

          IF (QW1.GT.0. .AND. Frain.GT.0.) THEN
            IF (Frain .GE. 1.) THEN
              QR1=QW1
              QW1=0.
            ELSE
              QR1=Frain*QW1
              QW1=QW1-QR1
            ENDIF
          ENDIF
          WV=Q1D/(1.-Q1D)

!--- Saturation vapor pressure w/r/t water ( >=0C ) or ice ( <0C )
!
          ESAT=1000.*FPVS0(T1D)
          QSAT=EPS*ESAT/(P1D-ESAT)
          RHO=P1D/(R_D*T1D*(1.+P608*Q1D))

          RRHO=1./RHO
  !
  !--- Based on code from GSMCOLUMN in model to determine reflectivity from rain
  !

          IF (QR1 .GT. EPSQ) THEN
            RQR=RHO*QR1
            IF (RQR .LE. RQR_DRmin) THEN
              N0r=MAX(N0rmin, CN0r_DMRmin*RQR)
              INDEXR=MDRmin
            ELSE IF (RQR .GE. RQR_DRmax) THEN
              N0r=CN0r_DMRmax*RQR
              INDEXR=MDRmax
            ELSE
              N0r=N0r0
              INDEXR=MAX( XMRmin, MIN(CN0r0*RQR**.25, XMRmax) )
            ENDIF
  !
  !--- INDEXR is the mean drop size in microns; convert to mm
  !
            DRmm=1.e-3*REAL(INDEXR)
            Zrain=0.72*N0r*DRmm*DRmm*DRmm*DRmm*DRmm*DRmm*DRmm
          ENDIF        !--- End IF (QR1 .GT. EPSQ) block
!
!--- Based on code from GSMCOLUMN in model to determine partition of
!    total ice into cloud ice & snow (precipitation ice)
!
          IF (QI1 .GT. EPSQ) THEN
            QICE=QI1
            RHO=P1D/(R_D*T1D*(1.+ONEPS*Q1D))
            RRHO=1./RHO
            QSIgrd=RHgrd*QSAT
            WVQW=WV+QW1
!
! * FLARGE  - ratio of number of large ice to total (large & small) ice
! * FSMALL  - ratio of number of small ice crystals to large ice particles
!  ->  Small ice particles are assumed to have a mean diameter of 50 microns.
!  * XSIMASS - used for calculating small ice mixing ratio
!  * XLIMASS - used for calculating large ice mixing ratio
!  * INDEXS  - mean size of snow to the nearest micron (units of microns)
!  * RimeF   - Rime Factor, which is the mass ratio of total (unrimed &
!              rimed) ice mass to the unrimed ice mass (>=1)
!  * FLIMASS - mass fraction of large ice
!  * QTICE   - time-averaged mixing ratio of total ice
!  * QLICE   - time-averaged mixing ratio of large ice
!  * NLICE   - time-averaged number concentration of large ice
!
            IF (TC.GE.0. .OR. WVQW.LT.QSIgrd) THEN
              FLARGE=1.
            ELSE
              FLARGE=.2
              IF (TC.GE.-8. .AND. TC.LE.-3.) FLARGE=.5*FLARGE
            ENDIF
            FSMALL=(1.-FLARGE)/FLARGE
            XSIMASS=RRHO*MASSI(MDImin)*FSMALL
      if(tc<-150..or.tc>100.)then
        write(0,67311)i,j,tc
67311   format(' CALMICT i=',i3,' j=',i3,' tc=',e13.6)
      endif
            DUM=XMImax*EXP(.0536*TC)
            INDEXS=MIN(MDImax, MAX(MDImin, INT(DUM) ) )
            RimeF=AMAX1(1., FS1D )
            XLIMASS=RRHO*RimeF*MASSI(INDEXS)
            FLIMASS=XLIMASS/(XLIMASS+XSIMASS)
            QLICE=FLIMASS*QICE
            NLICE=QLICE/XLIMASS
            IF (NLICE.LT.NLImin .OR. NLICE.GT.NLImax) THEN
!
!--- Force NLICE to be between NLImin and NLImax
!
              DUM=MAX(NLImin, MIN(NLImax, NLICE) )
              XLI=RHO*(QICE/DUM-XSIMASS)/RimeF
              IF (XLI .LE. MASSI(MDImin) ) THEN
                INDEXS=MDImin
              ELSE IF (XLI .LE. MASSI(450) ) THEN
                DLI=9.5885E5*XLI**.42066         ! DLI in microns
                INDEXS=MIN(MDImax, MAX(MDImin, INT(DLI) ) )
              ELSE IF (XLI .LE. MASSI(MDImax) ) THEN
                DLI=3.9751E6*XLI**.49870         ! DLI in microns
                INDEXS=MIN(MDImax, MAX(MDImin, INT(DLI) ) )
              ELSE
                INDEXS=MDImax
!
!--- 8/22/01: Increase density of large ice if maximum limits
!    are reached for number concentration (NLImax) and mean size
!    (MDImax).  Done to increase fall out of ice.
!
                IF (DUM .GE. NLImax) &
                 RimeF=RHO*(QICE/NLImax-XSIMASS)/MASSI(INDEXS)
              ENDIF             ! End IF (XLI .LE. MASSI(MDImin) )
              XLIMASS=RRHO*RimeF*MASSI(INDEXS)
              FLIMASS=XLIMASS/(XLIMASS+XSIMASS)
              QLICE=FLIMASS*QICE
              NLICE=QLICE/XLIMASS
            ENDIF               ! End IF (NLICE.LT.NLImin ...
            QS1=AMIN1(QI1, QLICE)
            QI1=AMAX1(0., QI1-QS1)
   !
   !--- Equation (C.8) in Ferrier (1994, JAS, p. 272), which when
   !    converted from cgs units to mks units results in the same
   !    value for Cice, which is equal to the {} term below:
   !
   !    Zi={.224*720*(10**18)/[(PI*RHOL)**2]}*(RHO*QLICE)**2/NLICE,
   !    where RHOL=1000 kg/m**3 is the density of liquid water
   !
   !--- Valid only for exponential ice distributions
   !
            Zice=Cice*RHO*RHO*QLICE*QLICE/NLICE
          ENDIF                 ! End IF (QI1 .GT. 0.) THEN
!
!---  Calculate total (convective + grid-scale) radar reflectivity
10        Ztot=Zrain+Zice+Zconv
          IF (Ztot .GT. Zmin)  DBZ1= 10.*ALOG10(Ztot)
      RETURN
      END SUBROUTINE CALMICT

!----------------------------------------------------------------------
!
!----------------------------------------------------------------------
!

      SUBROUTINE MAX_FIELDS(T,Q,U                     &
                           ,V,CW                      &
                           ,F_RAIN,F_ICE              &
                           ,F_RIMEF                   &
                           ,Z,W,PINT,PD               &
                           ,CPRATE,HTOP               &
                           ,T2,U10,V10                &
                           ,PSHLTR,TSHLTR,QSHLTR      &
                           ,SGML2,PSGML1              &
                           ,REFDMAX                   &
                           ,UPVVELMAX,DNVVELMAX       &
                           ,TLMAX,TLMIN               &
                           ,T02MAX,T02MIN             &
                           ,RH02MAX,RH02MIN           &
                           ,U10MAX,V10MAX,TH10,T10    &
                           ,SPD10MAX,T10AVG,PSFCAVG   &
                           ,AKHS,AKMS                 &
                           ,AKHSAVG,AKMSAVG           &
                           ,SNO,SNOAVG                &
                           ,UPHLMAX                   &
                           ,DT,NPHS,NTSD              &
                           ,DXH,DYH                   &
                           ,FIS                       &
                           ,ITS,ITE,JTS,JTE           & 
                           ,IMS,IME,JMS,JME           &
                           ,IDE,JDE                   & 
                           ,ITS_B1,ITE_B1             &
                           ,JTS_B1,JTE_B1             &
                           ,LM,NCOUNT,FIRST_NMM)

      USE MODULE_MP_ETANEW, ONLY : FERRIER_INIT, GPVS,FPVS,FPVS0,NX
      USE MODULE_CONSTANTS, ONLY : R_D,R_V,CPV,CP,G

      IMPLICIT NONE
       
      REAL, PARAMETER :: RCP=R_D/CP, P00_INV=1.E-5

      INTEGER,INTENT(IN) :: ITS,ITE,JTS,JTE,IMS,IME,JMS,JME,LM,NTSD
      INTEGER,INTENT(IN) :: ITS_B1,ITE_B1,JTS_B1,JTE_B1
      INTEGER,INTENT(IN) :: IDE,JDE,NPHS

      REAL, DIMENSION(IMS:IME,JMS:JME,1:LM),INTENT(IN) :: T, Q, U, V, CW   & 
                                              ,F_RAIN,F_ICE,F_RIMEF        &
                                              ,W,Z

      REAL,DIMENSION(IMS:IME,JMS:JME,1:LM+1),INTENT(IN) :: PINT

      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: PD,CPRATE,HTOP         &
                                                    ,T2,U10,V10            &
                                                    ,PSHLTR,TSHLTR,QSHLTR  &
                                                    ,TH10,AKHS             &
                                                    ,AKMS,SNO,FIS   

      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT):: REFDMAX               & 
                                                   ,UPVVELMAX,DNVVELMAX     &
                                                   ,TLMAX,TLMIN             &
                                                   ,T02MAX,T02MIN           &
                                                   ,RH02MAX,RH02MIN         &
                                                   ,U10MAX,V10MAX           &
                                                   ,SPD10MAX,T10AVG,PSFCAVG &
                                                   ,UPHLMAX,T10,AKHSAVG     &
                                                   ,AKMSAVG,SNOAVG 

      REAL, INTENT(IN) :: DYH, DXH(1:JDE)
      LOGICAL,INTENT(INOUT) :: FIRST_NMM
      REAL, INTENT(IN) :: SGML2(LM),PSGML1(LM), DT
 
      INTEGER :: UPINDX(IMS:IME,JMS:JME)
      REAL, DIMENSION(IMS:IME,JMS:JME) :: P10

      REAL, DIMENSION(IMS:IME,JMS:JME) :: ZINTSFC 
      REAL, DIMENSION(IMS:IME,JMS:JME,LM) :: PMID 

      REAL :: PLOW, PUP,WGTa,WGTb,ZMIDloc,ZMIDP1
      REAL :: P1Da,P1Db,P1D(2)
      REAL :: T1Da,T1Db,T1D(2),fact
      REAL :: Q1Da,Q1Db,Q1D(2)
      REAL :: C1Da,C1Db,C1D(2)
      REAL :: FR1Da,FR1Db,FR1D(2)
      REAL :: FI1Da,FI1Db,FI1D(2)
      REAL :: FS1Da,FS1Db,FS1D(2),DBZ1(2)

      REAL :: CUPRATE, CUREFL, CUREFL_I, ZFRZ, DBZ1avg, FCTR, DELZ
      REAL :: T02, RH02, TERM
      REAL :: CAPPA_MOIST, VAPOR_PRESS, SAT_VAPOR_PRESS
      REAL, SAVE :: R_FACTOR,CP_FACTOR, EPSILON, ONE_MINUS_EPSILON
      REAL, SAVE:: DTPHS, RDTPHS
      REAL :: MAGW2

      INTEGER :: LCTOP
      INTEGER :: I,J,L,NCOUNT,LL, RC, Ilook,Jlook


!***  COMPUTE AND SAVE THE FACTORS IN R AND CP TO ACCOUNT FOR
!***  WATER VAPOR IN THE AIR.
!***
!***  RECALL: R  = Rd * (1. + Q * (1./EPSILON - 1.))
!***          CP = CPd * (1. + Q * (CPv/CPd - 1.))

      Ilook=99
      Jlook=275

!     IF (FIRST_NMM) THEN
        DTPHS=DT*NPHS
        RDTPHS=3.6e6/DTPHS
        EPSILON=R_D/R_V
        ONE_MINUS_EPSILON=1.-EPSILON
        R_FACTOR=1./EPSILON-1.
        CP_FACTOR=CPV/CP-1.
! Make sure saturation vapor pressure lookup table is initialized
        CALL GPVS
        FIRST_NMM=.false.
!     ENDIF

      DO L=1,LM
       DO J=JTS,JTE
        DO I=ITS,ITE
         PMID(I,J,L)=PSGML1(L)+SGML2(L)*PD(I,J)
        ENDDO
       ENDDO
      ENDDO
!
      DO J=JTS,JTE
       DO I=ITS,ITE
         ZINTSFC(I,J)=FIS(I,J)/g
       ENDDO
      ENDDO
!
!     WON'T BOTHER TO REBUILD HEIGHTS AS IS DONE IN POST.
!     THE NONHYDROSTATIC MID-LAYER Z VALUES MATCH CLOSELY ENOUGH
!     AT 1000 m AGL
!
      DO J=JTS,JTE
       DO I=ITS,ITE
 L_LOOP: DO L=1,LM-1
          PLOW= PMID(I,J,L+1)
          PUP=  PMID(I,J,L)
          IF (PLOW .ge. 40000. .and. PUP .le. 40000.) THEN
            UPINDX(I,J)=L
            exit L_LOOP
          ENDIF 
         ENDDO L_LOOP
       ENDDO
      ENDDO
!
!xxx  DO J=JTS,JTE
!xxx   DO I=ITS,ITE
      DO J=JTS_B1,JTE_B1
       DO I=ITS_B1,ITE_B1
  vloop: DO L=8,LM-1
          IF ( (Z(I,J,L+1)-ZINTSFC(I,J)) .LE. 1000.                &
          .AND.(Z(I,J,L)-ZINTSFC(I,J))   .GE. 1000.)  THEN
            ZMIDP1=Z(I,J,L)
            ZMIDloc=Z(I,J,L+1)
            P1D(1)=PMID(I,J,L)
            P1D(2)=PMID(I,J,L+1)
            T1D(1)=T(I,J,L)
            T1D(2)=T(I,J,L+1)
            Q1D(1)=Q(I,J,L)
            Q1D(2)=Q(I,J,L+1)
            C1D(1)=CW(I,J,L)
            C1D(2)=CW(I,J,L+1)
            FR1D(1)=F_RAIN(I,J,L)
            FR1D(2)=F_RAIN(I,J,L+1)
            FI1D(1)=F_ICE(I,J,L)
            FI1D(2)=F_ICE(I,J,L+1)
            FS1D(1)=F_RIMEF(I,J,L)
            FS1D(2)=F_RIMEF(I,J,L+1)
            EXIT vloop
          ENDIF
        ENDDO vloop
!
!!! INITIAL CUREFL VALUE WITHOUT REDUCTION ABOVE FREEZING LEVEL
!
        CUPRATE=RDTPHS*CPRATE(I,J)
        CUREFL=300.*CUPRATE**1.4
        ZFRZ=Z(I,J,LM)
!
 culoop: IF (CUREFL > 0) THEN
 vloop2:  DO L=1,LM
            IF (T(I,J,L) >= 273.15) THEN
              ZFRZ=Z(I,J,L)
              EXIT vloop2
            ENDIF
          ENDDO vloop2
!
          LCTOP=NINT(HTOP(I,J))
          CUREFL_I=-2./MAX( 1000., Z(I,J,LCTOP)-ZFRZ )
!
 vloop3:  DO L=1,LM
            FCTR=0.
            IF (L >= LCTOP .and. L <= LM) THEN
              DELZ=Z(I,J,L)-ZFRZ
              IF (DELZ <= 0.) THEN
                FCTR=1.        !-- Below the highest freezing level
              ELSE
       !
       !--- Reduce convective radar reflectivity above freezing level
       !
                FCTR=10.**(CUREFL_I*DELZ)
              ENDIF
            ENDIF
          ENDDO vloop3
          CUREFL=FCTR*CUREFL
         ENDIF culoop
!
         DO LL=1,2
          IF (C1D(LL) .GE. 1.e-12 .OR. CUREFL .GT. 0.) then
           CALL CALMICT(P1D(LL),T1D(LL),Q1D(LL),C1D(LL), &
                        FI1D(LL),FR1D(LL),FS1D(LL),CUREFL, &
                        DBZ1(LL), I, J, Ilook, Jlook) 
          ELSE
           DBZ1(LL)=-20.
          ENDIF 
         ENDDO
         FACT=(1000.+ZINTSFC(I,J)-ZMIDloc)/(ZMIDloc-ZMIDP1)
         DBZ1avg=DBZ1(2)+(DBZ1(2)-DBZ1(1))*FACT
         REFDMAX(I,J)=max(REFDMAX(I,J),DBZ1avg)
       ENDDO
      ENDDO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      DO L=1,LM
       DO J=JTS,JTE
        DO I=ITS,ITE
         IF (L >= UPINDX(I,J)) THEN
           UPVVELMAX(I,J)=max(UPVVELMAX(I,J),W(I,J,L))
           DNVVELMAX(I,J)=min(DNVVELMAX(I,J),W(I,J,L))
         ENDIF
        ENDDO
       ENDDO
      ENDDO
!
      DO J=JTS,JTE
      DO I=ITS,ITE
        TLMAX(I,J)=MAX(TLMAX(I,J),T(I,J,LM))  !<--- Hourly max lowest layer T
        TLMIN(I,J)=MIN(TLMIN(I,J),T(I,J,LM))  !<--- Hourly min lowest layer T
        IF (NTSD > 0) THEN
          CAPPA_MOIST=RCP*(1.+QSHLTR(I,J)*R_FACTOR)/(1.+QSHLTR(I,J)*CP_FACTOR)
          T02=TSHLTR(I,J)*(P00_INV*PSHLTR(I,J))**CAPPA_MOIST
          T02MAX(I,J)=MAX(T02MAX(I,J),T02)  !<--- Hourly max 2m T
          T02MIN(I,J)=MIN(T02MIN(I,J),T02)  !<--- Hourly min 2m T
!
          VAPOR_PRESS=PSHLTR(I,J)*QSHLTR(I,J)/                          &
                     (EPSILON+QSHLTR(I,J)*ONE_MINUS_EPSILON)
          SAT_VAPOR_PRESS=1.E3*FPVS0(T02)
          RH02=MIN(VAPOR_PRESS/SAT_VAPOR_PRESS,0.99)
!
          RH02MAX(I,J)=MAX(RH02MAX(I,J),RH02)     !<--- Hourly max shelter RH
          RH02MIN(I,J)=MIN(RH02MIN(I,J),RH02)     !<--- Hourly min shelter RH
!
          MAGW2=(U10(I,J)**2.+V10(I,J)**2.)
          IF (MAGW2 .gt. SPD10MAX(I,J)) THEN
            U10MAX(I,J)=U10(I,J)                 !<--- U assoc with Hrly max 10m wind speed
            V10MAX(I,J)=V10(I,J)                 !<--- V assoc with Hrly max 10m wind speed
            SPD10MAX(I,J)=MAGW2
          ENDIF
	ENDIF
      ENDDO
      ENDDO

      CALL CALC_UPHLCY(U,V,W,Z,ZINTSFC,UPHLMAX,DXH,DYH          &
                      ,IMS,IME,JMS,JME                          &
                      ,ITS,ITE,JTS,JTE,IDE,JDE,LM)

      NCOUNT=NCOUNT+1
      DO J=JTS,JTE
       DO I=ITS,ITE
        TERM=-0.273133/T2(I,J)
        P10(I,J)=PSHLTR(I,J)*exp(TERM)
        T10(I,J)=TH10(I,J)*(P10(I,J)/1.e5)**RCP
        T10AVG(I,J)=T10AVG(I,J)*(NCOUNT-1)+T10(I,J)
        T10AVG(I,J)=T10AVG(I,J)/NCOUNT
        PSFCAVG(I,J)=PSFCAVG(I,J)*(NCOUNT-1)+PINT(I,J,LM+1)
        PSFCAVG(I,J)=PSFCAVG(I,J)/NCOUNT
        AKHSAVG(I,J)=AKHSAVG(I,J)*(NCOUNT-1)+AKHS(I,J)
        AKHSAVG(I,J)=AKHSAVG(I,J)/NCOUNT
        AKMSAVG(I,J)=AKMSAVG(I,J)*(NCOUNT-1)+AKMS(I,J)
        AKMSAVG(I,J)=AKMSAVG(I,J)/NCOUNT
        IF (SNO(I,J) > 0.) THEN
         SNOAVG(I,J)=SNOAVG(I,J)*(NCOUNT-1)+1
        ELSE 
         SNOAVG(I,J)=SNOAVG(I,J)*(NCOUNT-1)
        ENDIF
        SNOAVG(I,J)=SNOAVG(I,J)/NCOUNT
       ENDDO
      ENDDO

      END SUBROUTINE MAX_FIELDS
!
!----------------------------------------------------------------------
      SUBROUTINE CALC_UPHLCY(U,V,W,Z,ZINTSFC,UPHLMAX,DX,DY            &
                            ,IMS,IME,JMS,JME                          &
                            ,ITS,ITE,JTS,JTE,IDE,JDE,LM)

      INTEGER, INTENT(IN) :: IMS,IME,JMS,JME,ITS,ITE
      INTEGER, INTENT(IN) :: JTS,JTE,IDE,JDE,LM
      REAL,INTENT(IN) :: U(IMS:IME,JMS:JME,LM)
      REAL,INTENT(IN) :: V(IMS:IME,JMS:JME,LM)
      REAL,INTENT(IN) :: W(IMS:IME,JMS:JME,LM)
      REAL,INTENT(IN) :: Z(IMS:IME,JMS:JME,LM)
      REAL,INTENT(IN) :: ZINTSFC(IMS:IME,JMS:JME)
      REAL,INTENT(INOUT) :: UPHLMAX(IMS:IME,JMS:JME)
      REAL,INTENT(IN) :: DX(1:JDE),DY

! local variables

      REAL :: UPHL   (IMS:IME,JMS:JME)
      INTEGER :: I,J,L
      REAL :: R2DX,R2DY,DZ,ZMIDLOC
      REAL :: DUDY,DVDX

      REAL, PARAMETER:: HLOWER=2000.
      REAL, PARAMETER:: HUPPER=5000.

      do J=JMS,JME
       do I=IMS,IME
          UPHL(I,J)=0.
       enddo
      enddo

      R2DY=1./(2.*DY)
 J_LOOP: DO J=MAX(JTS,2),MIN(JTE,JDE-1)

	IF (DX(J) .LT. 0.1) THEN
        CYCLE J_LOOP
	ENDIF

        R2DX=1./(2.*DX(J))
        DO I=MAX(ITS,2),MIN(ITE,IDE-1)
  L_LOOP:  DO L=1,LM-1
             ZMIDLOC=Z(I,J,L)
             IF ( (ZMIDLOC - ZINTSFC(I,J)) .ge. HLOWER  .AND. &
                  (ZMIDLOC - ZINTSFC(I,J)) .le. HUPPER ) THEN
               DZ=(Z(I,J,L)-Z(I,J,L+1))
!
!*             ANY DOWNWARD MOTION IN 2-5 km LAYER KILLS COMPUTATION AND
!*             SETS RESULTANT UPDRAFT HELICTY TO ZERO
!
               IF (W(I,J,L) .lt. 0) THEN
                 UPHL(I,J)=0.
                 EXIT l_loop
               ENDIF

               DVDX=(V(I+1,J,L)-V(I-1,J,L))*R2DX
               DUDY=(U(I,J+1,L)-U(I,J-1,L))*R2DY
               UPHL(I,J)=UPHL(I,J)+(DVDX-DUDY)*W(I,J,L)*DZ
             ENDIF
           ENDDO L_LOOP
           UPHLMAX(I,J)=MAX(UPHL(I,J),UPHLMAX(I,J))
        ENDDO
      ENDDO J_LOOP
     
      END SUBROUTINE CALC_UPHLCY
!
!----------------------------------------------------------------------
!
!----------------------------------------------------------------------
!
      SUBROUTINE CALMICT_HR(P1D,T1D,Q1D,C1D,FI1D,FR1D,FS1D,CUREFL, &
                       DBZ1,I,J, Ilook,Jlook) 

      USE MODULE_MP_FER_HIRES, ONLY : FERRIER_INIT_HR, GPVS_HR,FPVS,FPVS0,NX,RQR_DRmin, &
                                   RQR_DRmax,MASSI,CN0R0,CN0r_DMRmin,CN0r_DMRmax
      USE MODULE_CONSTANTS,    ONLY : R_D,P608

      IMPLICIT NONE

      REAL, PARAMETER :: &
      FMW=18.015,FMD=28.964,EPS=FMW/FMD,ONEPS=1.-EPS,TFRZ=273.15, &
      PQ0=379.90516,EPSQ=1.E-12, &
      QCLDmin=1.E-6,  &
      STBOL=1./5.67E-8, DBZmin=-20., abscoef=8.0, abscoefi=5.0, &
      XLAI=4.0, &
      RHmin=1.0E-6, &
      SMALL=1.E-6 !  RHgrd=1.0

      REAL, PARAMETER :: DMImin=.05e-3, DMImax=1.e-3, &
                         XMImin=1.e6*DMImin, XMImax=1.e6*DMImax
      INTEGER, PARAMETER :: MDImin=XMImin, MDImax=XMImax
!
!-----------------------------------------------------------------------
!
!--- Mean rain drop diameters vary from 50 microns to 1000 microns (1 mm)
!
      REAL, PARAMETER :: DMRmin=.05E-3, DMRmax=1.E-3, DelDMR=1.E-6,        &
         XMRmin=1.E6*DMRmin, XMRmax=1.E6*DMRmax
      INTEGER, PARAMETER :: MDRmin=XMRmin, MDRmax=XMRmax

      INTEGER INDEXS, INDEXR, I, J, Ilook, Jlook
      REAL, PARAMETER :: Cice=1.634e13

      REAL ::  NLICE, N0r,Ztot,Zrain,Zice,Zconv
      REAL ::  P1D,T1D,Q1D,C1D,                                            &
               FI1D,FR1D,FS1D,CUREFL,                                      &
               QW1,QI1,QR1,QS1,                                            &
               DBZ1

      REAL :: FLARGE, FSMALL, WV, ESAT, TC, WC, FPVS_MINE,  RHO,  &
              QSAT, RRHO, RQR, ZMIN, Fice, Frain, Rimef , XLI, QICE, DRmm, &
              DLI, XSIMASS, XLIMASS, DUM, WVQW, QSIGRD, QLICE, FLIMASS

      REAL, PARAMETER ::                                                   &
     &  RHgrd=1.                                                           &
     & ,T_ICE=-40.                                                         &
     & ,NLImax=5.E3                                                        &
     & ,NLImin=1.E3                                                        &
     & ,N0r0=8.E6                                                          &
     & ,N0rmin=1.E4

! ---------
      Zmin=10.**(0.1*DBZmin)
          QW1=0.
          QI1=0.
          QR1=0.
          QS1=0.
          DBZ1=DBZmin
!          DBZR1=DBZmin
!          DBZI1=DBZmin
!          DBZC1=DBZmin

          Zrain=0.            !--- Radar reflectivity from rain
          Zice=0.             !--- Radar reflectivity from ice
          Zconv=CUREFL   !--- Radar reflectivity from convection
          IF (C1D .LE. EPSQ) THEN
!
!--- Skip rest of calculatiions if no condensate is present
!
            GO TO 10
          ELSE
            WC=C1D
          ENDIF
!
!--- Code below is from GSMDRIVE for determining:
!    QI1 - total ice (cloud ice & snow) mixing ratio
!    QW1 - cloud water mixing ratio
!    QR1 - rain mixing ratio

          TC=T1D-TFRZ
          Fice=FI1D
          Frain=FR1D
          IF (TC.LE.T_ICE .OR. Fice.GE.1.) THEN
            QI1=WC
          ELSE IF (Fice .LE. 0.) THEN
            QW1=WC
          ELSE
            QI1=Fice*WC
            QW1=WC-QI1
          ENDIF

          IF (QW1.GT.0. .AND. Frain.GT.0.) THEN
            IF (Frain .GE. 1.) THEN
              QR1=QW1
              QW1=0.
            ELSE
              QR1=Frain*QW1
              QW1=QW1-QR1
            ENDIF
          ENDIF
          WV=Q1D/(1.-Q1D)

!--- Saturation vapor pressure w/r/t water ( >=0C ) or ice ( <0C )
!
          ESAT=1000.*FPVS0(T1D)
          QSAT=EPS*ESAT/(P1D-ESAT)
          RHO=P1D/(R_D*T1D*(1.+P608*Q1D))

          RRHO=1./RHO
  !
  !--- Based on code from GSMCOLUMN in model to determine reflectivity from rain
  !

          IF (QR1 .GT. EPSQ) THEN
            RQR=RHO*QR1
            IF (RQR .LE. RQR_DRmin) THEN
              N0r=MAX(N0rmin, CN0r_DMRmin*RQR)
              INDEXR=MDRmin
            ELSE IF (RQR .GE. RQR_DRmax) THEN
              N0r=CN0r_DMRmax*RQR
              INDEXR=MDRmax
            ELSE
              N0r=N0r0
              INDEXR=MAX( XMRmin, MIN(CN0r0*RQR**.25, XMRmax) )
            ENDIF
  !
  !--- INDEXR is the mean drop size in microns; convert to mm
  !
            DRmm=1.e-3*REAL(INDEXR)
            Zrain=0.72*N0r*DRmm*DRmm*DRmm*DRmm*DRmm*DRmm*DRmm
          ENDIF        !--- End IF (QR1 .GT. EPSQ) block
!
!--- Based on code from GSMCOLUMN in model to determine partition of
!    total ice into cloud ice & snow (precipitation ice)
!
          IF (QI1 .GT. EPSQ) THEN
            QICE=QI1
            RHO=P1D/(R_D*T1D*(1.+ONEPS*Q1D))
            RRHO=1./RHO
            QSIgrd=RHgrd*QSAT
            WVQW=WV+QW1
!
! * FLARGE  - ratio of number of large ice to total (large & small) ice
! * FSMALL  - ratio of number of small ice crystals to large ice particles
!  ->  Small ice particles are assumed to have a mean diameter of 50 microns.
!  * XSIMASS - used for calculating small ice mixing ratio
!  * XLIMASS - used for calculating large ice mixing ratio
!  * INDEXS  - mean size of snow to the nearest micron (units of microns)
!  * RimeF   - Rime Factor, which is the mass ratio of total (unrimed &
!              rimed) ice mass to the unrimed ice mass (>=1)
!  * FLIMASS - mass fraction of large ice
!  * QTICE   - time-averaged mixing ratio of total ice
!  * QLICE   - time-averaged mixing ratio of large ice
!  * NLICE   - time-averaged number concentration of large ice
!
            IF (TC.GE.0. .OR. WVQW.LT.QSIgrd) THEN
              FLARGE=1.
            ELSE
              FLARGE=.2
              IF (TC.GE.-8. .AND. TC.LE.-3.) FLARGE=.5*FLARGE
            ENDIF
            FSMALL=(1.-FLARGE)/FLARGE
            XSIMASS=RRHO*MASSI(MDImin)*FSMALL
      if(tc<-150..or.tc>100.)then
        write(0,68311)i,j,tc
68311   format(' CALMICT_HR i=',i3,' j=',i3,' tc=',e13.6)
      endif
            DUM=XMImax*EXP(.0536*TC)
            INDEXS=MIN(MDImax, MAX(MDImin, INT(DUM) ) )
            RimeF=AMAX1(1., FS1D )
            XLIMASS=RRHO*RimeF*MASSI(INDEXS)
            FLIMASS=XLIMASS/(XLIMASS+XSIMASS)
            QLICE=FLIMASS*QICE
            NLICE=QLICE/XLIMASS
            IF (NLICE.LT.NLImin .OR. NLICE.GT.NLImax) THEN
!
!--- Force NLICE to be between NLImin and NLImax
!
              DUM=MAX(NLImin, MIN(NLImax, NLICE) )
              XLI=RHO*(QICE/DUM-XSIMASS)/RimeF
              IF (XLI .LE. MASSI(MDImin) ) THEN
                INDEXS=MDImin
              ELSE IF (XLI .LE. MASSI(450) ) THEN
                DLI=9.5885E5*XLI**.42066         ! DLI in microns
                INDEXS=MIN(MDImax, MAX(MDImin, INT(DLI) ) )
              ELSE IF (XLI .LE. MASSI(MDImax) ) THEN
                DLI=3.9751E6*XLI**.49870         ! DLI in microns
                INDEXS=MIN(MDImax, MAX(MDImin, INT(DLI) ) )
              ELSE
                INDEXS=MDImax
!
!--- 8/22/01: Increase density of large ice if maximum limits
!    are reached for number concentration (NLImax) and mean size
!    (MDImax).  Done to increase fall out of ice.
!
                IF (DUM .GE. NLImax) &
                 RimeF=RHO*(QICE/NLImax-XSIMASS)/MASSI(INDEXS)
              ENDIF             ! End IF (XLI .LE. MASSI(MDImin) )
              XLIMASS=RRHO*RimeF*MASSI(INDEXS)
              FLIMASS=XLIMASS/(XLIMASS+XSIMASS)
              QLICE=FLIMASS*QICE
              NLICE=QLICE/XLIMASS
            ENDIF               ! End IF (NLICE.LT.NLImin ...
            QS1=AMIN1(QI1, QLICE)
            QI1=AMAX1(0., QI1-QS1)
   !
   !--- Equation (C.8) in Ferrier (1994, JAS, p. 272), which when
   !    converted from cgs units to mks units results in the same
   !    value for Cice, which is equal to the {} term below:
   !
   !    Zi={.224*720*(10**18)/[(PI*RHOL)**2]}*(RHO*QLICE)**2/NLICE,
   !    where RHOL=1000 kg/m**3 is the density of liquid water
   !
   !--- Valid only for exponential ice distributions
   !
            Zice=Cice*RHO*RHO*QLICE*QLICE/NLICE
          ENDIF                 ! End IF (QI1 .GT. 0.) THEN
!
!---  Calculate total (convective + grid-scale) radar reflectivity
10        Ztot=Zrain+Zice+Zconv
          IF (Ztot .GT. Zmin)  DBZ1= 10.*ALOG10(Ztot)
      RETURN
      END SUBROUTINE CALMICT_HR

!----------------------------------------------------------------------
!
!----------------------------------------------------------------------
!

      SUBROUTINE MAX_FIELDS_HR(T,Q,U                  &
                           ,V,CW                      &
                           ,F_RAIN,F_ICE              &
                           ,F_RIMEF                   &
                           ,Z,W,PINT,PD               &
                           ,CPRATE,HTOP               &
                           ,T2,U10,V10                &
                           ,PSHLTR,TSHLTR,QSHLTR      &
                           ,SGML2,PSGML1              &
                           ,REFDMAX                   &
                           ,UPVVELMAX,DNVVELMAX       &
                           ,TLMAX,TLMIN               &
                           ,T02MAX,T02MIN             &
                           ,RH02MAX,RH02MIN           &
                           ,U10MAX,V10MAX,TH10,T10    &
                           ,SPD10MAX,T10AVG,PSFCAVG   &
                           ,AKHS,AKMS                 &
                           ,AKHSAVG,AKMSAVG           &
                           ,SNO,SNOAVG                &
                           ,UPHLMAX                   &
                           ,DT,NPHS,NTSD              &
                           ,DXH,DYH                   &
                           ,FIS                       &
                           ,ITS,ITE,JTS,JTE           & 
                           ,IMS,IME,JMS,JME           &
                           ,IDE,JDE                   & 
                           ,ITS_B1,ITE_B1             &
                           ,JTS_B1,JTE_B1             &
                           ,LM,NCOUNT,FIRST_NMM)

      USE MODULE_MP_FER_HIRES, ONLY : FERRIER_INIT_HR, GPVS_HR,FPVS,FPVS0,NX
      USE MODULE_CONSTANTS,    ONLY : R_D,R_V,CPV,CP,G

      IMPLICIT NONE
       
      REAL, PARAMETER :: RCP=R_D/CP, P00_INV=1.E-5

      INTEGER,INTENT(IN) :: ITS,ITE,JTS,JTE,IMS,IME,JMS,JME,LM,NTSD
      INTEGER,INTENT(IN) :: ITS_B1,ITE_B1,JTS_B1,JTE_B1
      INTEGER,INTENT(IN) :: IDE,JDE,NPHS

      REAL, DIMENSION(IMS:IME,JMS:JME,1:LM),INTENT(IN) :: T, Q, U, V, CW   & 
                                              ,F_RAIN,F_ICE,F_RIMEF        &
                                              ,W,Z

      REAL,DIMENSION(IMS:IME,JMS:JME,1:LM+1),INTENT(IN) :: PINT

      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: PD,CPRATE,HTOP         &
                                                    ,T2,U10,V10            &
                                                    ,PSHLTR,TSHLTR,QSHLTR  &
                                                    ,TH10,AKHS             &
                                                    ,AKMS,SNO,FIS   

      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT):: REFDMAX               & 
                                                   ,UPVVELMAX,DNVVELMAX     &
                                                   ,TLMAX,TLMIN             &
                                                   ,T02MAX,T02MIN           &
                                                   ,RH02MAX,RH02MIN         &
                                                   ,U10MAX,V10MAX           &
                                                   ,SPD10MAX,T10AVG,PSFCAVG &
                                                   ,UPHLMAX,T10,AKHSAVG     &
                                                   ,AKMSAVG,SNOAVG 

      REAL, INTENT(IN) :: DYH, DXH(1:JDE)
      LOGICAL,INTENT(INOUT) :: FIRST_NMM
      REAL, INTENT(IN) :: SGML2(LM),PSGML1(LM), DT
 
      INTEGER :: UPINDX(IMS:IME,JMS:JME)
      REAL, DIMENSION(IMS:IME,JMS:JME) :: P10

      REAL, DIMENSION(IMS:IME,JMS:JME) :: ZINTSFC 
      REAL, DIMENSION(IMS:IME,JMS:JME,LM) :: PMID 

      REAL :: PLOW, PUP,WGTa,WGTb,ZMIDloc,ZMIDP1
      REAL :: P1Da,P1Db,P1D(2)
      REAL :: T1Da,T1Db,T1D(2),fact
      REAL :: Q1Da,Q1Db,Q1D(2)
      REAL :: C1Da,C1Db,C1D(2)
      REAL :: FR1Da,FR1Db,FR1D(2)
      REAL :: FI1Da,FI1Db,FI1D(2)
      REAL :: FS1Da,FS1Db,FS1D(2),DBZ1(2)

      REAL :: CUPRATE, CUREFL, CUREFL_I, ZFRZ, DBZ1avg, FCTR, DELZ
      REAL :: T02, RH02, TERM
      REAL,SAVE :: CAPPA_MOIST, VAPOR_PRESS, EPSILON, ONE_MINUS_EPSILON, SAT_VAPOR_PRESS
      REAL :: R_FACTOR,CP_FACTOR
      REAL, SAVE:: DTPHS, RDTPHS
      REAL :: MAGW2

      INTEGER :: LCTOP
      INTEGER :: I,J,L,NCOUNT,LL, RC, Ilook,Jlook


!***  COMPUTE AND SAVE THE FACTORS IN R AND CP TO ACCOUNT FOR
!***  WATER VAPOR IN THE AIR.
!***
!***  RECALL: R  = Rd * (1. + Q * (1./EPSILON - 1.))
!***          CP = CPd * (1. + Q * (CPv/CPd - 1.))

      Ilook=99
      Jlook=275

!     IF (FIRST_NMM) THEN
        DTPHS=DT*NPHS
        RDTPHS=3.6e6/DTPHS
        EPSILON=R_D/R_V
        ONE_MINUS_EPSILON=1.-EPSILON
        R_FACTOR=1./EPSILON-1.
        CP_FACTOR=CPV/CP-1.
! Make sure saturation vapor pressure lookup table is initialized
        CALL GPVS_HR
        FIRST_NMM=.false.
!     ENDIF

      DO L=1,LM
       DO J=JTS,JTE
        DO I=ITS,ITE
         PMID(I,J,L)=PSGML1(L)+SGML2(L)*PD(I,J)
        ENDDO
       ENDDO
      ENDDO
!
      DO J=JTS,JTE
       DO I=ITS,ITE
         ZINTSFC(I,J)=FIS(I,J)/g
       ENDDO
      ENDDO
!
!     WON'T BOTHER TO REBUILD HEIGHTS AS IS DONE IN POST.
!     THE NONHYDROSTATIC MID-LAYER Z VALUES MATCH CLOSELY ENOUGH
!     AT 1000 m AGL
!
      DO J=JTS,JTE
       DO I=ITS,ITE
 L_LOOP: DO L=1,LM-1
          PLOW= PMID(I,J,L+1)
          PUP=  PMID(I,J,L)
          IF (PLOW .ge. 40000. .and. PUP .le. 40000.) THEN
            UPINDX(I,J)=L
            exit L_LOOP
          ENDIF 
         ENDDO L_LOOP
       ENDDO
      ENDDO
!
!xxx  DO J=JTS,JTE
!xxx   DO I=ITS,ITE
      DO J=JTS_B1,JTE_B1
       DO I=ITS_B1,ITE_B1
  vloop: DO L=8,LM-1
          IF ( (Z(I,J,L+1)-ZINTSFC(I,J)) .LE. 1000.                &
          .AND.(Z(I,J,L)-ZINTSFC(I,J))   .GE. 1000.)  THEN
            ZMIDP1=Z(I,J,L)
            ZMIDloc=Z(I,J,L+1)
            P1D(1)=PMID(I,J,L)
            P1D(2)=PMID(I,J,L+1)
            T1D(1)=T(I,J,L)
            T1D(2)=T(I,J,L+1)
            Q1D(1)=Q(I,J,L)
            Q1D(2)=Q(I,J,L+1)
            C1D(1)=CW(I,J,L)
            C1D(2)=CW(I,J,L+1)
            FR1D(1)=F_RAIN(I,J,L)
            FR1D(2)=F_RAIN(I,J,L+1)
            FI1D(1)=F_ICE(I,J,L)
            FI1D(2)=F_ICE(I,J,L+1)
            FS1D(1)=F_RIMEF(I,J,L)
            FS1D(2)=F_RIMEF(I,J,L+1)
            EXIT vloop
          ENDIF
        ENDDO vloop
!
!!! INITIAL CUREFL VALUE WITHOUT REDUCTION ABOVE FREEZING LEVEL
!
        CUPRATE=RDTPHS*CPRATE(I,J)
        CUREFL=300.*CUPRATE**1.4
        ZFRZ=Z(I,J,LM)
!
 culoop: IF (CUREFL > 0) THEN
 vloop2:  DO L=1,LM
            IF (T(I,J,L) >= 273.15) THEN
              ZFRZ=Z(I,J,L)
              EXIT vloop2
            ENDIF
          ENDDO vloop2
!
!     if(i<lbound(htop,1).or.i>ubound(htop,1).or.j<lbound(htop,2).or.j>ubound(htop,2))then
!       write(0,*)' MAX_FIELD i=',i,' j=',j,' lbound(htop)=',lbound(htop),' ubound=',ubound(htop)
!     endif
          LCTOP=NINT(HTOP(I,J))
          CUREFL_I=-2./MAX( 1000., Z(I,J,LCTOP)-ZFRZ )
!
 vloop3:  DO L=1,LM
            FCTR=0.
            IF (L >= LCTOP .and. L <= LM) THEN
              DELZ=Z(I,J,L)-ZFRZ
              IF (DELZ <= 0.) THEN
                FCTR=1.        !-- Below the highest freezing level
              ELSE
       !
       !--- Reduce convective radar reflectivity above freezing level
       !
                FCTR=10.**(CUREFL_I*DELZ)
              ENDIF
            ENDIF
          ENDDO vloop3
          CUREFL=FCTR*CUREFL
         ENDIF culoop
!
         DO LL=1,2
          IF (C1D(LL) .GE. 1.e-12 .OR. CUREFL .GT. 0.) then
           CALL CALMICT_HR(P1D(LL),T1D(LL),Q1D(LL),C1D(LL), &
                           FI1D(LL),FR1D(LL),FS1D(LL),CUREFL, &
                           DBZ1(LL), I, J, Ilook, Jlook) 
          ELSE
           DBZ1(LL)=-20.
          ENDIF 
         ENDDO
         FACT=(1000.+ZINTSFC(I,J)-ZMIDloc)/(ZMIDloc-ZMIDP1)
         DBZ1avg=DBZ1(2)+(DBZ1(2)-DBZ1(1))*FACT
         REFDMAX(I,J)=max(REFDMAX(I,J),DBZ1avg)
       ENDDO
      ENDDO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      DO L=1,LM
       DO J=JTS,JTE
        DO I=ITS,ITE
         IF (L >= UPINDX(I,J)) THEN
           UPVVELMAX(I,J)=max(UPVVELMAX(I,J),W(I,J,L))
           DNVVELMAX(I,J)=min(DNVVELMAX(I,J),W(I,J,L))
         ENDIF
        ENDDO
       ENDDO
      ENDDO
!
      DO J=JTS,JTE
      DO I=ITS,ITE
        TLMAX(I,J)=MAX(TLMAX(I,J),T(I,J,LM))  !<--- Hourly max lowest layer T
        TLMIN(I,J)=MIN(TLMIN(I,J),T(I,J,LM))  !<--- Hourly min lowest layer T
        IF (NTSD > 0) THEN
          CAPPA_MOIST=RCP*(1.+QSHLTR(I,J)*R_FACTOR)/(1.+QSHLTR(I,J)*CP_FACTOR)
          T02=TSHLTR(I,J)*(P00_INV*PSHLTR(I,J))**CAPPA_MOIST
          T02MAX(I,J)=MAX(T02MAX(I,J),T02)  !<--- Hourly max 2m T
          T02MIN(I,J)=MIN(T02MIN(I,J),T02)  !<--- Hourly min 2m T
!
          VAPOR_PRESS=PSHLTR(I,J)*QSHLTR(I,J)/                          &
                     (EPSILON+QSHLTR(I,J)*ONE_MINUS_EPSILON)
          SAT_VAPOR_PRESS=1.E3*FPVS0(T02)
          RH02=MIN(VAPOR_PRESS/SAT_VAPOR_PRESS,0.99)
!
          RH02MAX(I,J)=MAX(RH02MAX(I,J),RH02)     !<--- Hourly max shelter RH
          RH02MIN(I,J)=MIN(RH02MIN(I,J),RH02)     !<--- Hourly min shelter RH
!
          MAGW2=(U10(I,J)**2.+V10(I,J)**2.)
          IF (MAGW2 .gt. SPD10MAX(I,J)) THEN
            U10MAX(I,J)=U10(I,J)                 !<--- U assoc with Hrly max 10m wind speed
            V10MAX(I,J)=V10(I,J)                 !<--- V assoc with Hrly max 10m wind speed
            SPD10MAX(I,J)=MAGW2
          ENDIF
	ENDIF
      ENDDO
      ENDDO

      CALL CALC_UPHLCY(U,V,W,Z,ZINTSFC,UPHLMAX,DXH,DYH          & 
                      ,IMS,IME,JMS,JME                          &
                      ,ITS,ITE,JTS,JTE,IDE,JDE,LM)

      NCOUNT=NCOUNT+1
      DO J=JTS,JTE
       DO I=ITS,ITE
        TERM=-0.273133/T2(I,J)
        P10(I,J)=PSHLTR(I,J)*exp(TERM)
        T10(I,J)=TH10(I,J)*(P10(I,J)/1.e5)**RCP
        T10AVG(I,J)=T10AVG(I,J)*(NCOUNT-1)+T10(I,J)
        T10AVG(I,J)=T10AVG(I,J)/NCOUNT
        PSFCAVG(I,J)=PSFCAVG(I,J)*(NCOUNT-1)+PINT(I,J,LM+1)
        PSFCAVG(I,J)=PSFCAVG(I,J)/NCOUNT
        AKHSAVG(I,J)=AKHSAVG(I,J)*(NCOUNT-1)+AKHS(I,J)
        AKHSAVG(I,J)=AKHSAVG(I,J)/NCOUNT
        AKMSAVG(I,J)=AKMSAVG(I,J)*(NCOUNT-1)+AKMS(I,J)
        AKMSAVG(I,J)=AKMSAVG(I,J)/NCOUNT
        IF (SNO(I,J) > 0.) THEN
         SNOAVG(I,J)=SNOAVG(I,J)*(NCOUNT-1)+1
        ELSE 
         SNOAVG(I,J)=SNOAVG(I,J)*(NCOUNT-1)
        ENDIF
        SNOAVG(I,J)=SNOAVG(I,J)/NCOUNT
       ENDDO
      ENDDO

      END SUBROUTINE MAX_FIELDS_HR
 
!----------------------------------------------------------------------
!
!----------------------------------------------------------------------
!

      SUBROUTINE MAX_FIELDS_w6(T,Q,U,V,Z,W            &
                           ,WATER,PINT,PD             &
                           ,CPRATE,HTOP               &
                           ,T2,U10,V10                &
                           ,PSHLTR,TSHLTR,QSHLTR      &
                           ,SGML2,PSGML1              &
                           ,REFDMAX                   &
                           ,UPVVELMAX,DNVVELMAX       &
                           ,TLMAX,TLMIN               &
                           ,T02MAX,T02MIN             &
                           ,RH02MAX,RH02MIN           &
                           ,U10MAX,V10MAX,TH10,T10    &
                           ,SPD10MAX,T10AVG,PSFCAVG   &
                           ,AKHS,AKMS                 &
                           ,AKHSAVG,AKMSAVG           &
                           ,SNO,SNOAVG                &
                           ,UPHLMAX                   &
                           ,DT,NPHS,NTSD              &
                           ,DXH,DYH                   &
                           ,FIS                       &
                           ,P_QR,P_QS,P_QG            &
                           ,ITS,ITE,JTS,JTE           & 
                           ,IMS,IME,JMS,JME           &
                           ,IDE,JDE                   & 
                           ,ITS_B1,ITE_B1             &
                           ,JTS_B1,JTE_B1             &
                           ,LM,NUM_WATER              &
                           ,NCOUNT,FIRST_NMM)

      USE MODULE_CONSTANTS, ONLY : R_D,R_V,CPV,CP,G,CLIQ,PSAT,P608,XLV,TIW

      IMPLICIT NONE
       
      REAL, PARAMETER :: RCP=R_D/CP, P00_INV=1.E-5, TTP=TIW+0.01

      INTEGER,INTENT(IN) :: ITS,ITE,JTS,JTE,IMS,IME,JMS,JME,LM,NTSD
      INTEGER,INTENT(IN) :: ITS_B1,ITE_B1,JTS_B1,JTE_B1
      INTEGER,INTENT(IN) :: IDE,JDE,NPHS,NUM_WATER
      INTEGER,INTENT(IN) :: P_QR,P_QS,P_QG

      REAL, DIMENSION(IMS:IME,JMS:JME,1:LM),INTENT(IN) :: T,Q,U,V,Z,W

      REAL,DIMENSION(IMS:IME,JMS:JME,1:LM,NUM_WATER),INTENT(INOUT) :: WATER

      REAL,DIMENSION(IMS:IME,JMS:JME,1:LM+1),INTENT(IN) :: PINT

      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: PD,CPRATE,HTOP         &
                                                    ,T2,U10,V10            &
                                                    ,PSHLTR,TSHLTR,QSHLTR  &
                                                    ,TH10,AKHS             &
                                                    ,AKMS,SNO,FIS   

      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT):: REFDMAX               & 
                                                   ,UPVVELMAX,DNVVELMAX     &
                                                   ,TLMAX,TLMIN             &
                                                   ,T02MAX,T02MIN           &
                                                   ,RH02MAX,RH02MIN         &
                                                   ,U10MAX,V10MAX           &
                                                   ,SPD10MAX,T10AVG,PSFCAVG &
                                                   ,UPHLMAX,T10,AKHSAVG     &
                                                   ,AKMSAVG,SNOAVG 

      REAL, INTENT(IN) :: DYH, DXH(1:JDE)
      LOGICAL,INTENT(INOUT) :: FIRST_NMM
      REAL, INTENT(IN) :: SGML2(LM),PSGML1(LM), DT
 
      INTEGER :: UPINDX(IMS:IME,JMS:JME)
      REAL, DIMENSION(IMS:IME,JMS:JME) :: P10

      REAL, DIMENSION(IMS:IME,JMS:JME) :: ZINTSFC 
      REAL, DIMENSION(IMS:IME,JMS:JME,LM) :: PMID 

      REAL :: PLOW, PUP,WGTa,WGTb,ZMIDloc,ZMIDP1
      REAL :: P1Da,P1Db,P1D(2)
      REAL :: T1Da,T1Db,T1D(2),fact
      REAL :: Q1Da,Q1Db,Q1D(2)
      REAL :: QQR(2),QQS(2),QQG(2),QPCP,DENS,N0S
      REAL :: DBZR,DBZS,DBZG,DBZ1(2)
      REAL, PARAMETER :: N0S0=2.E6,N0Smax=1.E11,ALPHA=0.12   &
             ,N0G=4.E6,RHOS=100.,RHOG=500.,ZRADR=3.631E9     &
             ,DBZmin=-20.

      REAL :: CUPRATE, CUREFL, CUREFL_I, ZFRZ, DBZ1avg, FCTR, DELZ
      REAL :: T02, RH02, TERM, TREF
      REAL,SAVE :: CAPPA_MOIST, VAPOR_PRESS, EPSILON, ONE_MINUS_EPSILON, SAT_VAPOR_PRESS
      REAL :: R_FACTOR,CP_FACTOR
      REAL, SAVE:: DTPHS, RDTPHS, XA,XB,ZRADS,ZRADG,ZMIN
      REAL :: MAGW2

      INTEGER :: LCTOP
      INTEGER :: I,J,L,NCOUNT,LL, RC, Ilook,Jlook


!***  COMPUTE AND SAVE THE FACTORS IN R AND CP TO ACCOUNT FOR
!***  WATER VAPOR IN THE AIR.
!***
!***  RECALL: R  = Rd * (1. + Q * (1./EPSILON - 1.))
!***          CP = CPd * (1. + Q * (CPv/CPd - 1.))

      Ilook=99
      Jlook=275

!      IF (FIRST_NMM) THEN
        DTPHS=DT*NPHS
        RDTPHS=3.6e6/DTPHS
        EPSILON=R_D/R_V
        ONE_MINUS_EPSILON=1.-EPSILON
        R_FACTOR=1./EPSILON-1.
        CP_FACTOR=CPV/CP-1.
!-- For calculating vapor pressure
        XA=(CLIQ-CPV)/R_V
        XB=XA+XLV/(R_V*TTP)
!-- For calculating radar reflectivity
        ZRADS=2.17555E13*RHOS**0.25
        ZRADG=2.17555E13*RHOG**0.25/N0G**0.75
        ZMIN=10.**(0.1*DBZmin)
        FIRST_NMM=.false.
!      ENDIF

      DO L=1,LM
       DO J=JTS,JTE
        DO I=ITS,ITE
         PMID(I,J,L)=PSGML1(L)+SGML2(L)*PD(I,J)
        ENDDO
       ENDDO
      ENDDO
!
      DO J=JTS,JTE
       DO I=ITS,ITE
         ZINTSFC(I,J)=FIS(I,J)/g
       ENDDO
      ENDDO
!
!     WON'T BOTHER TO REBUILD HEIGHTS AS IS DONE IN POST.
!     THE NONHYDROSTATIC MID-LAYER Z VALUES MATCH CLOSELY ENOUGH
!     AT 1000 m AGL
!
      DO J=JTS,JTE
       DO I=ITS,ITE
 L_LOOP: DO L=1,LM-1
          PLOW= PMID(I,J,L+1)
          PUP=  PMID(I,J,L)
          IF (PLOW .ge. 40000. .and. PUP .le. 40000.) THEN
            UPINDX(I,J)=L
            exit L_LOOP
          ENDIF 
         ENDDO L_LOOP
       ENDDO
      ENDDO
!
!!      DO J=JTS,JTE
!!       DO I=ITS,ITE
      DO J=JTS_B1,JTE_B1
       DO I=ITS_B1,ITE_B1
  vloop: DO L=8,LM-1
          IF ( (Z(I,J,L+1)-ZINTSFC(I,J)) .LE. 1000.                &
          .AND.(Z(I,J,L)-ZINTSFC(I,J))   .GE. 1000.)  THEN
            ZMIDP1=Z(I,J,L)
            ZMIDloc=Z(I,J,L+1)
            P1D(1)=PMID(I,J,L)
            P1D(2)=PMID(I,J,L+1)
            T1D(1)=T(I,J,L)
            T1D(2)=T(I,J,L+1)
            Q1D(1)=Q(I,J,L)
            Q1D(2)=Q(I,J,L+1)
            QQR(1)=WATER(I,J,L,P_QR)
            QQR(2)=WATER(I,J,L+1,P_QR)
            QQS(1)=WATER(I,J,L,P_QS)
            QQS(2)=WATER(I,J,L+1,P_QS)
            QQG(1)=WATER(I,J,L,P_QG)
            QQG(2)=WATER(I,J,L+1,P_QG)
            EXIT vloop
          ENDIF
        ENDDO vloop
!
!!! INITIAL CUREFL VALUE WITHOUT REDUCTION ABOVE FREEZING LEVEL
!
        CUPRATE=RDTPHS*CPRATE(I,J)
        CUREFL=0.
        IF (CUPRATE>0.) CUREFL=300.*CUPRATE**1.4
!
!-- Ignore convective vertical profile effects when the freezing 
!   level is below 1000 m AGL, approximate using the surface value
!
         DO LL=1,2
           DBZ1(LL)=CUREFL
           QPCP=QQR(LL)+QQS(LL)+QQG(LL)
!-- A higher threshold can be used for calculating radar reflectivities
!   above DBZmin=-20 dBZ; note the DBZ arrays below are actually in
!   Z units of mm**6/m**3
           IF (QPCP>1.E-8) THEN
              DBZR=0.
              DBZS=0.
              DBZG=0.
              DENS=P1D(LL)/(R_D*T1D(LL)*(Q1D(LL)*P608+1.0))
              IF(QQR(LL)>1.E-8) DBZR=ZRADR*((QQR(LL)*DENS)**1.75)
              IF(QQS(LL)>1.E-8) THEN
                 N0S=N0S0*MAX(1., EXP(ALPHA*(TIW-T1D(LL) ) ) )
                 N0S=MIN(N0S, N0Smax)
                 DBZS=ZRADS*((QQS(LL)*DENS)**1.75)/N0S**0.75
              ENDIF
              IF(QQG(LL)>1.E-8) DBZG=ZRADG*((QQG(LL)*DENS)**1.75)
              DBZ1(LL)=DBZ1(LL)+DBZR+DBZS+DBZG
           ENDIF 
         ENDDO
!-- Vertical interpolation of Z (units of mm**6/m**3)
         FACT=(1000.+ZINTSFC(I,J)-ZMIDloc)/(ZMIDloc-ZMIDP1)
         DBZ1avg=DBZ1(2)+(DBZ1(2)-DBZ1(1))*FACT
!-- Convert to dBZ (10*logZ) as the last step
         IF (DBZ1avg>ZMIN) THEN
            DBZ1avg=10.*ALOG10(DBZ1avg)
         ELSE
            DBZ1avg=DBZmin
         ENDIF
         REFDMAX(I,J)=max(REFDMAX(I,J),DBZ1avg)
       ENDDO
      ENDDO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      DO L=1,LM
       DO J=JTS,JTE
        DO I=ITS,ITE
         IF (L >= UPINDX(I,J)) THEN
           UPVVELMAX(I,J)=max(UPVVELMAX(I,J),W(I,J,L))
           DNVVELMAX(I,J)=min(DNVVELMAX(I,J),W(I,J,L))
         ENDIF
        ENDDO
       ENDDO
      ENDDO
!
      DO J=JTS,JTE
      DO I=ITS,ITE
        TLMAX(I,J)=MAX(TLMAX(I,J),T(I,J,LM))  !<--- Hourly max lowest layer T
        TLMIN(I,J)=MIN(TLMIN(I,J),T(I,J,LM))  !<--- Hourly min lowest layer T
        IF (NTSD > 0) THEN
          CAPPA_MOIST=RCP*(1.+QSHLTR(I,J)*R_FACTOR)/(1.+QSHLTR(I,J)*CP_FACTOR)
          T02=TSHLTR(I,J)*(P00_INV*PSHLTR(I,J))**CAPPA_MOIST
          T02MAX(I,J)=MAX(T02MAX(I,J),T02)  !<--- Hourly max 2m T
          T02MIN(I,J)=MIN(T02MIN(I,J),T02)  !<--- Hourly min 2m T
!
          VAPOR_PRESS=PSHLTR(I,J)*QSHLTR(I,J)/                          &
                     (EPSILON+QSHLTR(I,J)*ONE_MINUS_EPSILON)

!-- Adapted from WSM6 code:
          TREF=TTP/T02
          SAT_VAPOR_PRESS=PSAT*EXP(LOG(TREF)*(XA))*EXP(XB*(1.-TREF))

          RH02=MIN(VAPOR_PRESS/SAT_VAPOR_PRESS,0.99)
!
          RH02MAX(I,J)=MAX(RH02MAX(I,J),RH02)     !<--- Hourly max shelter RH
          RH02MIN(I,J)=MIN(RH02MIN(I,J),RH02)     !<--- Hourly min shelter RH
!
          MAGW2=(U10(I,J)**2.+V10(I,J)**2.)
          IF (MAGW2 .gt. SPD10MAX(I,J)) THEN
            U10MAX(I,J)=U10(I,J)                 !<--- U assoc with Hrly max 10m wind speed
            V10MAX(I,J)=V10(I,J)                 !<--- V assoc with Hrly max 10m wind speed
            SPD10MAX(I,J)=MAGW2
          ENDIF
	ENDIF
      ENDDO
      ENDDO

      CALL CALC_UPHLCY(U,V,W,Z,ZINTSFC,UPHLMAX,DXH,DYH          & 
                      ,IMS,IME,JMS,JME                          &
                      ,ITS,ITE,JTS,JTE,IDE,JDE,LM)

      NCOUNT=NCOUNT+1
      DO J=JTS,JTE
       DO I=ITS,ITE
        TERM=-0.273133/T2(I,J)
        P10(I,J)=PSHLTR(I,J)*exp(TERM)
        T10(I,J)=TH10(I,J)*(P10(I,J)/1.e5)**RCP
        T10AVG(I,J)=T10AVG(I,J)*(NCOUNT-1)+T10(I,J)
        T10AVG(I,J)=T10AVG(I,J)/NCOUNT
        PSFCAVG(I,J)=PSFCAVG(I,J)*(NCOUNT-1)+PINT(I,J,LM+1)
        PSFCAVG(I,J)=PSFCAVG(I,J)/NCOUNT
        AKHSAVG(I,J)=AKHSAVG(I,J)*(NCOUNT-1)+AKHS(I,J)
        AKHSAVG(I,J)=AKHSAVG(I,J)/NCOUNT
        AKMSAVG(I,J)=AKMSAVG(I,J)*(NCOUNT-1)+AKMS(I,J)
        AKMSAVG(I,J)=AKMSAVG(I,J)/NCOUNT
        IF (SNO(I,J) > 0.) THEN
         SNOAVG(I,J)=SNOAVG(I,J)*(NCOUNT-1)+1
        ELSE 
         SNOAVG(I,J)=SNOAVG(I,J)*(NCOUNT-1)
        ENDIF
        SNOAVG(I,J)=SNOAVG(I,J)/NCOUNT
       ENDDO
      ENDDO

      END SUBROUTINE MAX_FIELDS_W6
!
!----------------------------------------------------------------------
!
      END MODULE MODULE_DIAGNOSE
!
!----------------------------------------------------------------------
