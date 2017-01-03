!-----------------------------------------------------------------------
      SUBROUTINE ONE_RECORD_TO_IJ(PREFIX,NEST_TOP_RATIO,IM,JM)
!
      USE GRIB_MOD
!
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      CHARACTER(LEN=*), INTENT(IN) :: PREFIX
      INTEGER, INTENT(IN) :: NEST_TOP_RATIO
      INTEGER, INTENT(IN) :: IM,JM
!
!-----------------------------------------------------------------------
!
      INTEGER,PARAMETER :: NUM_FIELDS=8
!
      INTEGER :: IUNIT_IN=20,IUNIT_OUT=21
!
      REAL, PARAMETER :: G=9.81
!
! These names must be identical to the grib file names created by gridgen_sfc
!
      CHARACTER(len=15),DIMENSION(1:NUM_FIELDS) :: FILEIN=(/            &
                                                   'snowfree_albedo'    &
                                                  ,'elevtiles      '    &
                                                  ,'soiltiles      '    &
                                                  ,'vegtiles       '    &
                                                  ,'mxsnoalb       '    &
                                                  ,'slmask         '    &
                                                  ,'tbot           '    &
                                                  ,'vegfrac        '    &
                                                   /)
!
! These names must be identical to the FIELD_NAME in the Move Bundle
!
      CHARACTER(len=6),DIMENSION(1:NUM_FIELDS) :: FILEOUT=(/            &
                                                    'ALBASE'            &
                                                   ,'FIS   '            &
                                                   ,'ISLTYP'            &
                                                   ,'IVGTYP'            &
                                                   ,'MXSNAL'            &
                                                   ,'SM    '            &
                                                   ,'TG    '            &
                                                   ,'VEGFRC'            &
                                                   /)
!
!-----------------------------------------------------------------------
!
      CHARACTER(LEN=128) :: FILENAME
      CHARACTER(LEN=2) :: ID_SFC_FILE
!
      TYPE(GRIBFIELD) :: GFLD
      INTEGER, DIMENSION(200) :: JIDS,JPDT,JGDT
!
      INTEGER :: I,J,K,IERR,NF
!
      INTEGER,DIMENSION(1:IM,1:JM) :: IARRAY_IJ
!
      REAL,DIMENSION(1:IM,1:JM) :: ARRAY_IJ
!
      REAL :: DUMMY
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      field_loop: DO NF=1,NUM_FIELDS
!
!-----------------------------------------------------------------------
!
        WRITE(FILENAME,"(A,A,A,A)") PREFIX,"_",TRIM(FILEIN(NF)),".grb2"
        CALL BAOPENR(IUNIT_IN,TRIM(FILENAME),IERR)
        IF (IERR/=0) THEN
           WRITE(0,*)' ierr .ne. 0 for baopenr ',TRIM(FILENAME), IERR
           STOP
        END IF

        JIDS=-9999
        JPDT=-9999
        JGDT=-9999
        CALL GETGB2(IUNIT_IN,IUNIT_IN,0,-1,JIDS,-1,JPDT,-1,JGDT,.TRUE.,K,GFLD,IERR)
        IF (IERR/=0) THEN
          WRITE(0,*)' ierr .ne. 0 for getgb2 ',TRIM(FILENAME), IERR
          STOP
        ELSE
          ARRAY_IJ = RESHAPE(GFLD%FLD, (/ IM, JM /))
        END IF
        CALL GF_FREE(GFLD)

        CALL BACLOSE(IUNIT_IN,IERR)
        IF (IERR/=0) THEN
          WRITE(0,*)' ierr .ne. 0 for baclose ',TRIM(FILENAME), IERR
          STOP
        END IF
!
!-----------------------------------------------------------------------
!
        IF(NEST_TOP_RATIO<=9)THEN
          WRITE(ID_SFC_FILE,'(I1.1)')NEST_TOP_RATIO
        ELSEIF(NEST_TOP_RATIO>=10)THEN
          WRITE(ID_SFC_FILE,'(I2.2)')NEST_TOP_RATIO
        ENDIF
!
        FILENAME=TRIM(FILEOUT(NF))//'_ij_'//ID_SFC_FILE

        OPEN(unit  =IUNIT_OUT                                             &
            ,file  =TRIM(FILENAME)                                        &
            ,form  ='unformatted'                                         &
            ,status='replace'                                             &
            ,iostat=IERR)
!
        IF(IERR/=0)THEN
          WRITE(0,*)' Failed to open output file ',TRIM(FILEOUT(NF))
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!***  For FIS we must multiply the elevation by G.
!-----------------------------------------------------------------------
!
        IF(FILEIN(NF)(1:9)=='elevtiles')THEN
          DO J=1,JM
          DO I=1,IM
            ARRAY_IJ(I,J)=ARRAY_IJ(I,J)*G
          ENDDO
          ENDDO
        ENDIF
!
!-----------------------------------------------------------------------
!***  For the seamask we must flip the values so that 1=>sea, 0=>land.
!-----------------------------------------------------------------------
!
        IF(FILEIN(NF)(1:6)=='slmask')THEN
          DO J=1,JM
          DO I=1,IM
            IF(ARRAY_IJ(I,J)>0.5)THEN
              ARRAY_IJ(I,J)=0.
            ELSE
              ARRAY_IJ(I,J)=1.
            ENDIF
          ENDDO
          ENDDO
        ENDIF
!
!-----------------------------------------------------------------------
!***  Albedos and vegetation fraction must be multiplied by 0.01.
!***  Each has its own dummy value.
!-----------------------------------------------------------------------
!
        IF(FILEIN(NF)(1:15)=='snowfree_albedo'                          &
                  .OR.                                                  &
           FILEIN(NF)(1:8)=='mxsnoalb'                                  &
                  .OR.                                                  &
           FILEIN(NF)(1:7)=='vegfrac'                                   &
                                     )THEN
!
          IF(FILEIN(NF)(1:15)=='snowfree_albedo')THEN
            DUMMY=0.06
          ELSEIF(FILEIN(NF)(1:8)=='mxsnoalb')THEN
            DUMMY=0.08
          ELSEIF(FILEIN(NF)(1:7)=='vegfrac')THEN
            DUMMY=0.0
          ENDIF
!
          DO J=1,JM
          DO I=1,IM
            IF(ARRAY_IJ(I,J)<1000.)THEN
              ARRAY_IJ(I,J)=ARRAY_IJ(I,J)*0.01
            ELSE
              ARRAY_IJ(I,J)=DUMMY
            ENDIF
          ENDDO
          ENDDO
!
        ENDIF 
!-----------------------------------------------------------------------
!***  TG has a dummy value.
!-----------------------------------------------------------------------
!
        IF(FILEIN(NF)(1:4)=='tbot')THEN
          DUMMY=273.16
          DO J=1,JM
          DO I=1,IM
            IF(ARRAY_IJ(I,J)<1000.)THEN
              ARRAY_IJ(I,J)=ARRAY_IJ(I,J)
            ELSE
              ARRAY_IJ(I,J)=DUMMY
            ENDIF
          ENDDO
          ENDDO
!
        ENDIF
!
!-----------------------------------------------------------------------
!***  Soil and vegetation types must be converted to integers.
!***  At water points they have nonsense values so change them
!***  to zero.
!-----------------------------------------------------------------------
!
        IF(FILEIN(NF)(1:9)=='soiltiles'                                 &
                    .OR.                                                &
           FILEIN(NF)(1:8)=='vegtiles'                                  &
                                     )THEN
!
          DO J=1,JM
          DO I=1,IM
            IARRAY_IJ(I,J)=NINT(ARRAY_IJ(I,J))
            IF(IARRAY_IJ(I,J)>100)THEN
              IARRAY_IJ(I,J)=0
            ENDIF
          ENDDO
          ENDDO
!
!-----------------------------------------------------------------------
!***  Write the integer arrays.
!-----------------------------------------------------------------------
!
          DO J=1,JM
            WRITE(IUNIT_OUT)IARRAY_IJ(1:IM,J)
          ENDDO
!
!-----------------------------------------------------------------------
!***  Write the Real arrays.
!-----------------------------------------------------------------------
!
        ELSE
!
          DO J=1,JM
            WRITE(IUNIT_OUT)ARRAY_IJ(1:IM,J)
          ENDDO
!
        ENDIF
!
!-----------------------------------------------------------------------
!
        CLOSE(IUNIT_OUT)
!
!-----------------------------------------------------------------------
!
      ENDDO field_loop
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE ONE_RECORD_TO_IJ
!
!-----------------------------------------------------------------------
