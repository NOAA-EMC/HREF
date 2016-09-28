!-----------------------------------------------------------------------
      SUBROUTINE SFC_GRIB2NC(PREFIX,NEST_TOP_RATIO,IM,JM)
!
      USE GRIB_MOD
      USE GRIDINFO_MODULE
      USE NETCDF
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
      INTEGER,PARAMETER :: NUM_FIELDS=6
!
      INTEGER :: IUNIT_IN=20
!
      REAL, PARAMETER :: G=9.81
!
! These names must be identical to the grib file names created by gridgen_sfc
!
      CHARACTER(len=15),DIMENSION(1:NUM_FIELDS) :: FILEIN=(/            &
                                                   'elevtiles      '    &
                                                  ,'soiltiles      '    &
                                                  ,'vegtiles       '    &
                                                  ,'mxsnoalb       '    &
                                                  ,'slmask         '    &
                                                  ,'tbot           '    &
                                                   /)
!
! These names must be identical to the FIELD_NAME in the Move Bundle
!
      CHARACTER(len=6),DIMENSION(1:NUM_FIELDS) :: FILEOUT=(/            &
                                                    'FIS   '            &
                                                   ,'ISLTYP'            &
                                                   ,'IVGTYP'            &
                                                   ,'MXSNAL'            &
                                                   ,'SM    '            &
                                                   ,'TG    '            &
                                                   /)
!
!-----------------------------------------------------------------------
!
      CHARACTER(LEN=128) :: FILENAME
      CHARACTER(LEN=20) :: NETCDF_FILENAME
      CHARACTER(LEN=2) :: ID_SFC_FILE
!
      TYPE(GRIBFIELD) :: GFLD
      INTEGER, DIMENSION(200) :: JIDS,JPDT,JGDT
!
      INTEGER :: I,J,K,IERR,NF,NR
!
      INTEGER,DIMENSION(1:IM,1:JM) :: IARRAY_IJ
!
      REAL,DIMENSION(1:IM,1:JM) :: ARRAY_IJ
      LOGICAL*1, DIMENSION(1:IM,1:JM) :: BMAPCHECK
!
      REAL,DIMENSION(1:IM,1:JM,4) :: ALBASE4
      REAL,DIMENSION(1:IM,1:JM,12) :: VEGFRC12
!
      REAL :: DUMMY
!
      LOGICAL :: INTEGER_DATA
      INTEGER :: NCID,VAR_ID,XDIM_ID,XVAR_ID,YDIM_ID,YVAR_ID
      INTEGER,DIMENSION(1:2) :: DIM_IDS
      REAL,DIMENSION(:),ALLOCATABLE :: X_INDX,Y_INDX
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      ALLOCATE(X_INDX(1:IM))
      ALLOCATE(Y_INDX(1:JM))
!
      DO I=1,IM
        X_INDX(I)=REAL(I)
      ENDDO
!
      DO J=1,JM
        Y_INDX(J)=REAL(J)
      ENDDO
!
!***  Loop over all fields/grib files which contain only one record
!
      field_loop: DO NF=1,NUM_FIELDS
!
!-----------------------------------------------------------------------
!
        INTEGER_DATA=FILEOUT(NF)=='ISLTYP'                              &
                           .OR.                                         &
                     FILEOUT(NF)=='IVGTYP'
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
          IF(GFLD%IBMAP .eq. 0 .or. GFLD%IBMAP .eq. 254) THEN
           BMAPCHECK = RESHAPE(GFLD%BMAP, (/ IM, JM /))
          ELSE
           BMAPCHECK = .true.
          ENDIF
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
!-----------------------------------------------------------------------
!***  Create the netCDF file.
!-----------------------------------------------------------------------
!
        NETCDF_FILENAME=TRIM(FILEOUT(NF))//'_'//TRIM(ID_SFC_FILE)//'.nc'

        CALL CHECK(NF90_CREATE(NETCDF_FILENAME                          &
                              ,NF90_NOCLOBBER                           &
                              ,NCID ) )                                    !<-- The netCDF file's ID
!
!-----------------------------------------------------------------------
!***  Define the netCDF file dimensions.
!-----------------------------------------------------------------------
!
        CALL CHECK(NF90_DEF_DIM(NCID                                    &
                               ,'x_indx'                                &
                               ,IM                                      &
                               ,XDIM_ID ) )                                !<-- The X dimension's ID
!
        CALL CHECK(NF90_DEF_DIM(NCID                                    &
                               ,'y_indx'                                &
                               ,JM                                      &
                               ,YDIM_ID ) )                                !<-- The Y dimension's ID
!
!-----------------------------------------------------------------------
!***  Define the index variables.
!-----------------------------------------------------------------------
!
        CALL CHECK(NF90_DEF_VAR(NCID                                    &
                               ,'x_indx'                                &
                               ,NF90_REAL                               &
                               ,XDIM_ID                                 &
                               ,XVAR_ID ) )
!
        CALL CHECK(NF90_DEF_VAR(NCID                                    &
                               ,'y_indx'                                &
                               ,NF90_REAL                               &
                               ,YDIM_ID                                 &
                               ,YVAR_ID ) )
!
        DIM_IDS=(/XDIM_ID,YDIM_ID/)
!
!-----------------------------------------------------------------------
!***  Define the data variable.
!-----------------------------------------------------------------------
!
        IF(INTEGER_DATA)THEN
          CALL CHECK(NF90_DEF_VAR(NCID                                  &
                                 ,TRIM(FILEOUT(NF))                     &
                                 ,NF90_INT                              &
                                 ,DIM_IDS                               &
                                 ,VAR_ID ) )
        ELSE
          CALL CHECK(NF90_DEF_VAR(NCID                                  &
                                 ,TRIM(FILEOUT(NF))                     &
                                 ,NF90_FLOAT                            &
                                 ,DIM_IDS                               &
                                 ,VAR_ID ) )
        ENDIF
!
        CALL CHECK(NF90_ENDDEF(NCID))
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
!***  Albedo must be multiplied by 0.01.
!-----------------------------------------------------------------------
!
        IF(FILEIN(NF)(1:8)=='mxsnoalb')THEN
          DO J=1,JM
          DO I=1,IM
           IF(BMAPCHECK(I,J))THEN
            IF(ARRAY_IJ(I,J)<1000.)THEN
              ARRAY_IJ(I,J)=ARRAY_IJ(I,J)*0.01
            ELSE
              ARRAY_IJ(I,J)=0.08
            ENDIF
           ELSE
            ARRAY_IJ(I,J)=0.08
           ENDIF
          ENDDO
          ENDDO
        ENDIF 
!
!-----------------------------------------------------------------------
!***  TG has a dummy value.
!-----------------------------------------------------------------------
!
        IF(FILEIN(NF)(1:4)=='tbot')THEN
          DO J=1,JM
          DO I=1,IM
           IF(BMAPCHECK(I,J))THEN
            IF(ARRAY_IJ(I,J)<1000.)THEN
              ARRAY_IJ(I,J)=ARRAY_IJ(I,J)
            ELSE
              ARRAY_IJ(I,J)=273.16
            ENDIF
           ELSE
            ARRAY_IJ(I,J)=273.16
           ENDIF
          ENDDO
          ENDDO
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
           IF(BMAPCHECK(I,J))THEN
            IARRAY_IJ(I,J)=NINT(ARRAY_IJ(I,J))
            IF(IARRAY_IJ(I,J)>100)THEN
              IARRAY_IJ(I,J)=0
            ENDIF
           ELSE
            if(FILEIN(NF)(1:8)=='vegtiles') then 
             if (use_igbp) then
              IARRAY_IJ(I,J)=17 ! IGBP water
             else
              IARRAY_IJ(I,J)=16 ! USGS water
             endif
            else
             IARRAY_IJ(I,J)=14 ! SOIL TYPE for water
            endif
           ENDIF
          ENDDO
          ENDDO
!
        ENDIF
!
!-----------------------------------------------------------------------
!***  Insert the data into the netCDF file.
!-----------------------------------------------------------------------
!
        CALL CHECK(NF90_PUT_VAR(NCID                                    &
                               ,XVAR_ID                                 &
                               ,X_INDX ) )
!
        CALL CHECK(NF90_PUT_VAR(NCID                                    &
                               ,YVAR_ID                                 &
                               ,Y_INDX ) )
!
        IF(INTEGER_DATA)THEN
          CALL CHECK(NF90_PUT_VAR(NCID                                  &
                                 ,VAR_ID                                &
                                 ,IARRAY_IJ ) )
        ELSE
          CALL CHECK(NF90_PUT_VAR(NCID                                  &
                                 ,VAR_ID                                &
                                 ,ARRAY_IJ ) )
        ENDIF
!
        CALL CHECK(NF90_CLOSE(NCID))
!
!-----------------------------------------------------------------------
!
      ENDDO field_loop
!
!***  Linearly interpolate in time ALBASE and VEGFRC
!
!***  ALBASE data is quarterly (4 records)
!
      WRITE(FILENAME,"(A,A)") PREFIX,"_snowfree_albedo.grb2"
      CALL BAOPENR(IUNIT_IN,TRIM(FILENAME),IERR)
      IF (IERR/=0) THEN
         WRITE(0,*)' ierr .ne. 0 for baopenr ',TRIM(FILENAME), IERR
         STOP
      END IF

      JIDS=-9999
      JPDT=-9999
      JGDT=-9999

      DO NR=1,4

      CALL GETGB2(IUNIT_IN,IUNIT_IN,NR-1,-1,JIDS,-1,JPDT,-1,JGDT,.TRUE.,K,GFLD,IERR)
      IF (IERR/=0) THEN
        WRITE(0,*)' ierr .ne. 0 for getgb2 ',TRIM(FILENAME), IERR
        STOP
      ELSE
        ALBASE4(:,:,NR) = RESHAPE(GFLD%FLD, (/ IM, JM /))
        BMAPCHECK(:,:) = RESHAPE(GFLD%BMAP, (/ IM, JM /))
      END IF
      CALL GF_FREE(GFLD)

      END DO

      CALL quarterly_interp_to_date ( ALBASE4, start_date(1)//'.0000' , ARRAY_IJ , &
                                      1 , IM , 1 , JM , 1 , 1 , &
                                      1 , IM , 1 , JM , 1 , 1 , &
                                      1 , IM , 1 , JM , 1 , 1 )


      DO J=1,JM
      DO I=1,IM

      IF (BMAPCHECK(I,J)) THEN
        IF(ARRAY_IJ(I,J)<1000.)THEN
          ARRAY_IJ(I,J)=ARRAY_IJ(I,J)*0.01
        ELSE
          ARRAY_IJ(I,J)=0.06
        ENDIF
      ELSE
        ARRAY_IJ(I,J)=0.06
      ENDIF

      ENDDO
      ENDDO

      CALL BACLOSE(IUNIT_IN,IERR)
      IF (IERR/=0) THEN
        WRITE(0,*)' ierr .ne. 0 for baclose ',TRIM(FILENAME), IERR
        STOP
      END IF

      IF(NEST_TOP_RATIO<=9)THEN
        WRITE(ID_SFC_FILE,'(I1.1)')NEST_TOP_RATIO
      ELSEIF(NEST_TOP_RATIO>=10)THEN
        WRITE(ID_SFC_FILE,'(I2.2)')NEST_TOP_RATIO
      ENDIF
!
!-----------------------------------------------------------------------
!***  Create the netCDF file.
!-----------------------------------------------------------------------
!
      NETCDF_FILENAME='ALBASE_'//TRIM(ID_SFC_FILE)//'.nc'

      CALL CHECK(NF90_CREATE(NETCDF_FILENAME                            &
                            ,NF90_NOCLOBBER                             &
                            ,NCID ) )                                    !<-- The netCDF file's ID
!
!-----------------------------------------------------------------------
!***  Define the netCDF file dimensions.
!-----------------------------------------------------------------------
!
      CALL CHECK(NF90_DEF_DIM(NCID                                      &
                             ,'x_indx'                                  &
                             ,IM                                        &
                             ,XDIM_ID ) )                                !<-- The X dimension's ID
!
      CALL CHECK(NF90_DEF_DIM(NCID                                      &
                             ,'y_indx'                                  &
                             ,JM                                        &
                             ,YDIM_ID ) )                                !<-- The Y dimension's ID
!
!-----------------------------------------------------------------------
!***  Define the index variables.
!-----------------------------------------------------------------------
!
      CALL CHECK(NF90_DEF_VAR(NCID                                      &
                             ,'x_indx'                                  &
                             ,NF90_REAL                                 &
                             ,XDIM_ID                                   &
                             ,XVAR_ID ) )
!
      CALL CHECK(NF90_DEF_VAR(NCID                                      &
                             ,'y_indx'                                  &
                             ,NF90_REAL                                 &
                             ,YDIM_ID                                   &
                             ,YVAR_ID ) )
!
      DIM_IDS=(/XDIM_ID,YDIM_ID/)
!
!-----------------------------------------------------------------------
!***  Define the data variable.
!-----------------------------------------------------------------------
!
      CALL CHECK(NF90_DEF_VAR(NCID                                      &
                             ,'ALBASE'                                  &
                             ,NF90_FLOAT                                &
                             ,DIM_IDS                                   &
                             ,VAR_ID ) )
!
      CALL CHECK(NF90_ENDDEF(NCID))
!
!
!-----------------------------------------------------------------------
!***  Insert the data into the netCDF file.
!-----------------------------------------------------------------------
!
      CALL CHECK(NF90_PUT_VAR(NCID                                      &
                             ,XVAR_ID                                   &
                             ,X_INDX ) )
!
      CALL CHECK(NF90_PUT_VAR(NCID                                      &
                             ,YVAR_ID                                   &
                             ,Y_INDX ) )
!
      CALL CHECK(NF90_PUT_VAR(NCID                                      &
                             ,VAR_ID                                    &
                             ,ARRAY_IJ ) )
!
      CALL CHECK(NF90_CLOSE(NCID))
!
!-----------------------------------------------------------------------
!
!***  VEGFRC is monthly (12 records).
!
      WRITE(FILENAME,"(A,A)") PREFIX,"_vegfrac.grb2"
      CALL BAOPENR(IUNIT_IN,TRIM(FILENAME),IERR)
      IF (IERR/=0) THEN
         WRITE(0,*)' ierr .ne. 0 for baopenr ',TRIM(FILENAME), IERR
         STOP
      END IF

      JIDS=-9999
      JPDT=-9999
      JGDT=-9999

      DO NR=1,12

      CALL GETGB2(IUNIT_IN,IUNIT_IN,NR-1,-1,JIDS,-1,JPDT,-1,JGDT,.TRUE.,K,GFLD,IERR)
      IF (IERR/=0) THEN
        WRITE(0,*)' ierr .ne. 0 for getgb2 ',TRIM(FILENAME), IERR
        STOP
      ELSE
        VEGFRC12(:,:,NR) = RESHAPE(GFLD%FLD, (/ IM, JM /))
      END IF
      CALL GF_FREE(GFLD)

      END DO

      CALL monthly_interp_to_date ( VEGFRC12, start_date(1)//'.0000' , ARRAY_IJ , &
                                    1 , IM , 1 , JM , 1 , 1 , &
                                    1 , IM , 1 , JM , 1 , 1 , &
                                    1 , IM , 1 , JM , 1 , 1 )


      DO J=1,JM
      DO I=1,IM
        IF(ARRAY_IJ(I,J)<1000.)THEN
          ARRAY_IJ(I,J)=ARRAY_IJ(I,J)*0.01
        ELSE
          ARRAY_IJ(I,J)=0.0
        ENDIF
      ENDDO
      ENDDO

      CALL BACLOSE(IUNIT_IN,IERR)
      IF (IERR/=0) THEN
        WRITE(0,*)' ierr .ne. 0 for baclose ',TRIM(FILENAME), IERR
        STOP
      END IF

      IF(NEST_TOP_RATIO<=9)THEN
        WRITE(ID_SFC_FILE,'(I1.1)')NEST_TOP_RATIO
      ELSEIF(NEST_TOP_RATIO>=10)THEN
        WRITE(ID_SFC_FILE,'(I2.2)')NEST_TOP_RATIO
      ENDIF
!
!-----------------------------------------------------------------------
!***  Create the netCDF file.
!-----------------------------------------------------------------------
!
      NETCDF_FILENAME='VEGFRC_'//TRIM(ID_SFC_FILE)//'.nc'

      CALL CHECK(NF90_CREATE(NETCDF_FILENAME                            &
                            ,NF90_NOCLOBBER                             &
                            ,NCID ) )                                    !<-- The netCDF file's ID
!
!-----------------------------------------------------------------------
!***  Define the netCDF file dimensions.
!-----------------------------------------------------------------------
!
      CALL CHECK(NF90_DEF_DIM(NCID                                      &
                             ,'x_indx'                                  &
                             ,IM                                        &
                             ,XDIM_ID ) )                                !<-- The X dimension's ID
!
      CALL CHECK(NF90_DEF_DIM(NCID                                      &
                             ,'y_indx'                                  &
                             ,JM                                        &
                             ,YDIM_ID ) )                                !<-- The Y dimension's ID
!
!-----------------------------------------------------------------------
!***  Define the index variables.
!-----------------------------------------------------------------------
!
      CALL CHECK(NF90_DEF_VAR(NCID                                      &
                             ,'x_indx'                                  &
                             ,NF90_REAL                                 &
                             ,XDIM_ID                                   &
                             ,XVAR_ID ) )
!
      CALL CHECK(NF90_DEF_VAR(NCID                                      &
                             ,'y_indx'                                  &
                             ,NF90_REAL                                 &
                             ,YDIM_ID                                   &
                             ,YVAR_ID ) )
!
      DIM_IDS=(/XDIM_ID,YDIM_ID/)
!
!-----------------------------------------------------------------------
!***  Define the data variable.
!-----------------------------------------------------------------------
!
      CALL CHECK(NF90_DEF_VAR(NCID                                      &
                             ,'VEGFRC'                                  &
                             ,NF90_FLOAT                                &
                             ,DIM_IDS                                   &
                             ,VAR_ID ) )
!
      CALL CHECK(NF90_ENDDEF(NCID))
!
!
!-----------------------------------------------------------------------
!***  Insert the data into the netCDF file.
!-----------------------------------------------------------------------
!
      CALL CHECK(NF90_PUT_VAR(NCID                                      &
                             ,XVAR_ID                                   &
                             ,X_INDX ) )
!        
      CALL CHECK(NF90_PUT_VAR(NCID                                      &
                             ,YVAR_ID                                   &
                             ,Y_INDX ) )
!
      CALL CHECK(NF90_PUT_VAR(NCID                                      &
                             ,VAR_ID                                    &
                             ,ARRAY_IJ ) )
!
      CALL CHECK(NF90_CLOSE(NCID))
!
!-----------------------------------------------------------------------
!
CONTAINS


      SUBROUTINE CHECK(RC)
!
      IMPLICIT NONE
!
      INTEGER,INTENT(IN) :: RC
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      IF(RC/=NF90_NOERR)THEN
        WRITE(*,*)TRIM(ADJUSTL(NF90_STRERROR(RC)))
!       WRITE(0,11101)RC
11101   FORMAT(' ERROR: RC=',I5)
      ENDIF
!
      END SUBROUTINE CHECK
!
!-----------------------------------------------------------------------
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!-----------------------------------------------------------------------
!
!***  The following routines are copied from nemsinterp (module_vinterp_routines.F90)
!
   SUBROUTINE monthly_interp_to_date ( field_in , date_str , field_out , &
                      ids , ide , jds , jde , kds , kde , &
                      ims , ime , jms , jme , kms , kme , &
                      its , ite , jts , jte , kts , kte )

!  Linearly in time interpolate data to a current valid time.  The data is
!  assumed to come in "monthly", valid at the 15th of every month.

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      CHARACTER (LEN=24) , INTENT(IN) :: date_str
      REAL , DIMENSION(ims:ime,jms:jme,12) , INTENT(IN)  :: field_in
      REAL , DIMENSION(ims:ime,   jms:jme) , INTENT(OUT) :: field_out

      !  Local vars

      INTEGER :: i , j , l
      INTEGER , DIMENSION(0:13) :: middle
      INTEGER :: target_julyr , target_julday , target_date
      INTEGER :: julyr , julday , int_month, next_month
      REAL :: gmt
      CHARACTER (LEN=4) :: yr
      CHARACTER (LEN=2) :: mon , day15


      WRITE(day15,FMT='(I2.2)') 15
      DO l = 1 , 13
         WRITE(mon,FMT='(I2.2)') l
         CALL get_julgmt ( date_str(1:4)//'-'//mon//'-'//day15//'_'//'00:00:00.0000' , julyr , julday , gmt)
         middle(L) = julyr*1000 + julday
      END DO

      l = 0
      middle(l) = middle( 1) - 30

      l = 13
      middle(l) = middle(12) + 30

      CALL get_julgmt ( date_str , target_julyr , target_julday , gmt )
      target_date = target_julyr * 1000 + target_julday

      find_month : DO l = 0 , 12
         IF ( ( middle(l) .LT. target_date ) .AND. ( middle(l+1) .GE.  target_date ) ) THEN
            DO j = jts , MIN ( jde-1 , jte )
               DO i = its , MIN (ide-1 , ite )
                  int_month = MOD ( l , 12 )
                  IF ( int_month .EQ. 0 ) int_month=12

                  IF (int_month == 12) THEN
                      next_month=1
                  ELSE
                      next_month=int_month+1
                  ENDIF

                  field_out(i,j) =  ( field_in(i,j,next_month) * ( target_date - middle(l)   ) + &
                                      field_in(i,j,int_month  ) * ( middle(l+1) - target_date ) ) / &
                                    ( middle(l+1) - middle(l) )

               END DO
            END DO
            EXIT find_month
         END IF
      END DO find_month

   END SUBROUTINE monthly_interp_to_date

! -------------------------------------------------------------------

   SUBROUTINE quarterly_interp_to_date( field_in , date_str , field_out , &
                      ids , ide , jds , jde , kds , kde , &
                      ims , ime , jms , jme , kms , kme , &
                      its , ite , jts , jte , kts , kte )

!  Linearly in time interpolate data to a current valid time.  The data is
!  assumed to come in "quarterly", valid at the 15th of every month.

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      CHARACTER (LEN=24) , INTENT(IN) :: date_str
      REAL , DIMENSION(ims:ime,jms:jme,4) , INTENT(IN)  :: field_in
      REAL , DIMENSION(ims:ime,   jms:jme) , INTENT(OUT) :: field_out

      !  Local vars

      INTEGER :: i , j , l, LL
      INTEGER , DIMENSION(0:5) :: middle
      INTEGER :: target_julyr , target_julday , target_date
      INTEGER :: julyr , julday , int_quart, next_quart
      REAL :: gmt
      CHARACTER (LEN=4) :: yr
      CHARACTER (LEN=2) :: mon , day30


      WRITE(day30,FMT='(I2.2)') 30
      LL=0
      DO l = 1 , 10, 3
      LL=LL+1
         WRITE(mon,FMT='(I2.2)') l
         CALL get_julgmt ( date_str(1:4)//'-'//mon//'-'//day30//'_'//'00:00:00.0000' , julyr , julday , gmt)
         middle(LL) = julyr*1000 + julday
      END DO

      l = 0
      middle(l) = middle( 1) - 90

      l = 5
      middle(l) = middle(4) + 90

      CALL get_julgmt ( date_str , target_julyr , target_julday , gmt )
      target_date = target_julyr * 1000 + target_julday
      find_quarter : DO l = 0 , 4
         IF ( ( middle(l) .LT. target_date ) .AND. ( middle(l+1) .GE.  target_date ) ) THEN
                  int_quart = MOD ( l , 4 )
                  IF ( int_quart .EQ. 0 ) int_quart = 4

        IF (int_quart == 4) THEN
            next_quart=1
        ELSE
            next_quart=int_quart+1
        ENDIF

            DO j = jts , MIN ( jde-1 , jte )
               DO i = its , MIN (ide-1 , ite )
                  field_out(i,j) =  ( field_in(i,j,next_quart) * ( target_date - middle(l)   ) + &
                                      field_in(i,j,int_quart  ) * ( middle(l+1) - target_date ) ) / &
                                    ( middle(l+1) - middle(l) )
               END DO
            END DO
            EXIT find_quarter
         END IF
      END DO find_quarter

   END SUBROUTINE quarterly_interp_to_date

! -------------------------------------------------------------------

   SUBROUTINE get_julgmt(date_str,julyr,julday,gmt)
     IMPLICIT NONE
! Arguments
     CHARACTER (LEN=24) , INTENT(IN) :: date_str
     INTEGER, INTENT(OUT  ) :: julyr
     INTEGER, INTENT(OUT  ) :: julday
     REAL   , INTENT(OUT  ) :: gmt
! Local
     INTEGER :: ny , nm , nd , nh , ni , ns , nt
     INTEGER :: my1, my2, my3, monss
     INTEGER, DIMENSION(12) :: mmd
     DATA MMD/31,28,31,30,31,30,31,31,30,31,30,31/
     CALL split_date_char ( date_str , ny , nm , nd , nh , ni , ns , nt )
     GMT=nh+FLOAT(ni)/60.+FLOAT(ns)/3600.
     MY1=MOD(ny,4)
     MY2=MOD(ny,100)
     MY3=MOD(ny,400)
     IF(MY1.EQ.0.AND.MY2.NE.0.OR.MY3.EQ.0)MMD(2)=29
     JULDAY=nd
     JULYR=ny
     DO MONSS=1,nm-1
       JULDAY=JULDAY+MMD(MONSS)
     ENDDO
   END SUBROUTINE get_julgmt

!---------------------------------------------------------

   SUBROUTINE split_date_char ( date , century_year , month , day , hour , minute , second , ten_thousandth)

      IMPLICIT NONE

      !  Input data.

      CHARACTER(LEN=24) , INTENT(IN) :: date

      !  Output data.

      INTEGER , INTENT(OUT) :: century_year , month , day , hour , minute , second , ten_thousandth

      READ(date,FMT='(    I4)') century_year
      READ(date,FMT='( 5X,I2)') month
      READ(date,FMT='( 8X,I2)') day
      READ(date,FMT='(11X,I2)') hour
      READ(date,FMT='(14X,I2)') minute
      READ(date,FMT='(17X,I2)') second
      READ(date,FMT='(20X,I4)') ten_thousandth

   END SUBROUTINE split_date_char
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE SFC_GRIB2NC
!-----------------------------------------------------------------------
