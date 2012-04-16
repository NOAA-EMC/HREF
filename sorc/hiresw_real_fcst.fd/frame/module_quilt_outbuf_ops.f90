MODULE module_quilt_outbuf_ops






  INTEGER, PARAMETER :: tabsize = 1000
  INTEGER            :: num_entries

  TYPE outrec
    CHARACTER*80                       :: VarName, DateStr, MemoryOrder, Stagger, DimNames(3)
    INTEGER                            :: ndim
    INTEGER, DIMENSION(3)              :: DomainStart, DomainEnd
    INTEGER                            :: FieldType
    REAL,    POINTER, DIMENSION(:,:,:) :: rptr 
    INTEGER, POINTER, DIMENSION(:,:,:) :: iptr
  END TYPE outrec

  TYPE(outrec), DIMENSION(tabsize) :: outbuf_table

CONTAINS

  SUBROUTINE init_outbuf





    IMPLICIT NONE
    INTEGER i
    DO i = 1, tabsize
      outbuf_table(i)%VarName = ""
      outbuf_table(i)%DateStr = ""
      outbuf_table(i)%MemoryOrder = ""
      outbuf_table(i)%Stagger = ""
      outbuf_table(i)%DimNames(1) = ""
      outbuf_table(i)%DimNames(2) = ""
      outbuf_table(i)%DimNames(3) = ""
      outbuf_table(i)%ndim = 0
      NULLIFY( outbuf_table(i)%rptr )
      NULLIFY( outbuf_table(i)%iptr )
    ENDDO
    num_entries = 0
  END SUBROUTINE init_outbuf


  SUBROUTINE write_outbuf ( DataHandle , io_form_arg )









    USE module_state_description
    IMPLICIT NONE
      integer, parameter  :: WRF_FILE_NOT_OPENED                  = 100
      integer, parameter  :: WRF_FILE_OPENED_NOT_COMMITTED        = 101
      integer, parameter  :: WRF_FILE_OPENED_FOR_WRITE            = 102
      integer, parameter  :: WRF_FILE_OPENED_FOR_READ             = 103
      integer, parameter  :: WRF_REAL                             = 104
      integer, parameter  :: WRF_DOUBLE                           = 105
      integer, parameter  :: WRF_FLOAT=WRF_REAL
      integer, parameter  :: WRF_INTEGER                          = 106
      integer, parameter  :: WRF_LOGICAL                          = 107
      integer, parameter  :: WRF_COMPLEX                          = 108
      integer, parameter  :: WRF_DOUBLE_COMPLEX                   = 109
      integer, parameter  :: WRF_FILE_OPENED_FOR_UPDATE           = 110


      integer, parameter  :: WRF_FILE_OPENED_AND_COMMITTED        = 102
    INTEGER , INTENT(IN)  :: DataHandle, io_form_arg
    INTEGER               :: ii,ds1,de1,ds2,de2,ds3,de3
    INTEGER               :: Comm, IOComm, DomainDesc 
    INTEGER               :: Status
    CHARACTER*80          :: mess
    Comm = 0 ; IOComm = 0 ; DomainDesc = 0 
    DO ii = 1, num_entries
      WRITE(mess,*)'writing ', &
                    TRIM(outbuf_table(ii)%DateStr)," ",                                   &
                    TRIM(outbuf_table(ii)%VarName)," ",                                   &
                    TRIM(outbuf_table(ii)%MemoryOrder)
      ds1 = outbuf_table(ii)%DomainStart(1) ; de1 = outbuf_table(ii)%DomainEnd(1)
      ds2 = outbuf_table(ii)%DomainStart(2) ; de2 = outbuf_table(ii)%DomainEnd(2)
      ds3 = outbuf_table(ii)%DomainStart(3) ; de3 = outbuf_table(ii)%DomainEnd(3)

      SELECT CASE ( io_form_arg )

        CASE ( IO_NETCDF   )

          IF ( outbuf_table(ii)%FieldType .EQ. WRF_FLOAT ) THEN

          CALL ext_ncd_write_field ( DataHandle ,                                     &
                                 TRIM(outbuf_table(ii)%DateStr),                      &
                                 TRIM(outbuf_table(ii)%VarName),                      &
                                 outbuf_table(ii)%rptr(ds1:de1,ds2:de2,ds3:de3),      &
                                 outbuf_table(ii)%FieldType,                          &  
                                 Comm, IOComm, DomainDesc ,                           &
                                 TRIM(outbuf_table(ii)%MemoryOrder),                  &
                                 TRIM(outbuf_table(ii)%Stagger),                      &  
                                 outbuf_table(ii)%DimNames ,                          &  
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 Status )

          ELSE IF ( outbuf_table(ii)%FieldType .EQ. WRF_INTEGER ) THEN
          CALL ext_ncd_write_field ( DataHandle ,                                     &
                                 TRIM(outbuf_table(ii)%DateStr),                      &
                                 TRIM(outbuf_table(ii)%VarName),                      &
                                 outbuf_table(ii)%iptr(ds1:de1,ds2:de2,ds3:de3),      &
                                 outbuf_table(ii)%FieldType,                          &  
                                 Comm, IOComm, DomainDesc ,                           &
                                 TRIM(outbuf_table(ii)%MemoryOrder),                  &
                                 TRIM(outbuf_table(ii)%Stagger),                      &  
                                 outbuf_table(ii)%DimNames ,                          &  
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 Status )
          ENDIF
      CASE ( IO_GRIB1   )

          IF ( outbuf_table(ii)%FieldType .EQ. WRF_FLOAT ) THEN

          CALL ext_gr1_write_field ( DataHandle ,                                   &
                                 TRIM(outbuf_table(ii)%DateStr),                      &
                                 TRIM(outbuf_table(ii)%VarName),                      &
                                 outbuf_table(ii)%rptr(ds1:de1,ds2:de2,ds3:de3),      &
                                 outbuf_table(ii)%FieldType,                          &  
                                 Comm, IOComm, DomainDesc ,                           &
                                 TRIM(outbuf_table(ii)%MemoryOrder),                  &
                                 TRIM(outbuf_table(ii)%Stagger),                      &  
                                 outbuf_table(ii)%DimNames ,                          &  
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 Status )

          ELSE IF ( outbuf_table(ii)%FieldType .EQ. WRF_INTEGER ) THEN
          CALL ext_gr1_write_field ( DataHandle ,                                   &
                                 TRIM(outbuf_table(ii)%DateStr),                      &
                                 TRIM(outbuf_table(ii)%VarName),                      &
                                 outbuf_table(ii)%iptr(ds1:de1,ds2:de2,ds3:de3),      &
                                 outbuf_table(ii)%FieldType,                          &  
                                 Comm, IOComm, DomainDesc ,                           &
                                 TRIM(outbuf_table(ii)%MemoryOrder),                  &
                                 TRIM(outbuf_table(ii)%Stagger),                      &  
                                 outbuf_table(ii)%DimNames ,                          &  
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 Status )
          ENDIF
        CASE ( IO_INTIO  )
          IF ( outbuf_table(ii)%FieldType .EQ. WRF_FLOAT ) THEN

          CALL ext_int_write_field ( DataHandle ,                                     &
                                 TRIM(outbuf_table(ii)%DateStr),                      &
                                 TRIM(outbuf_table(ii)%VarName),                      &
                                 outbuf_table(ii)%rptr(ds1:de1,ds2:de2,ds3:de3),      &
                                 outbuf_table(ii)%FieldType,                          &  
                                 Comm, IOComm, DomainDesc ,                           &
                                 TRIM(outbuf_table(ii)%MemoryOrder),                  &
                                 TRIM(outbuf_table(ii)%Stagger),                      &  
                                 outbuf_table(ii)%DimNames ,                          &  
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 Status )

          ELSE IF ( outbuf_table(ii)%FieldType .EQ. WRF_INTEGER ) THEN

          CALL ext_int_write_field ( DataHandle ,                                     &
                                 TRIM(outbuf_table(ii)%DateStr),                      &
                                 TRIM(outbuf_table(ii)%VarName),                      &
                                 outbuf_table(ii)%iptr(ds1:de1,ds2:de2,ds3:de3),      &
                                 outbuf_table(ii)%FieldType,                          &  
                                 Comm, IOComm, DomainDesc ,                           &
                                 TRIM(outbuf_table(ii)%MemoryOrder),                  &
                                 TRIM(outbuf_table(ii)%Stagger),                      &  
                                 outbuf_table(ii)%DimNames ,                          &  
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 Status )

          ENDIF
        CASE DEFAULT
      END SELECT


      IF ( ASSOCIATED( outbuf_table(ii)%rptr) ) DEALLOCATE(outbuf_table(ii)%rptr)
      IF ( ASSOCIATED( outbuf_table(ii)%iptr) ) DEALLOCATE(outbuf_table(ii)%iptr)
      NULLIFY( outbuf_table(ii)%rptr )
      NULLIFY( outbuf_table(ii)%iptr )
    ENDDO
    CALL init_outbuf
  END SUBROUTINE write_outbuf

END MODULE module_quilt_outbuf_ops



  SUBROUTINE store_patch_in_outbuf( inbuf_r, inbuf_i, DateStr, VarName , FieldType, MemoryOrder, Stagger, DimNames, &
                                    DomainStart , DomainEnd , &
                                    MemoryStart , MemoryEnd , &
                                    PatchStart , PatchEnd )














    USE module_quilt_outbuf_ops
    IMPLICIT NONE
      integer, parameter  :: WRF_FILE_NOT_OPENED                  = 100
      integer, parameter  :: WRF_FILE_OPENED_NOT_COMMITTED        = 101
      integer, parameter  :: WRF_FILE_OPENED_FOR_WRITE            = 102
      integer, parameter  :: WRF_FILE_OPENED_FOR_READ             = 103
      integer, parameter  :: WRF_REAL                             = 104
      integer, parameter  :: WRF_DOUBLE                           = 105
      integer, parameter  :: WRF_FLOAT=WRF_REAL
      integer, parameter  :: WRF_INTEGER                          = 106
      integer, parameter  :: WRF_LOGICAL                          = 107
      integer, parameter  :: WRF_COMPLEX                          = 108
      integer, parameter  :: WRF_DOUBLE_COMPLEX                   = 109
      integer, parameter  :: WRF_FILE_OPENED_FOR_UPDATE           = 110


      integer, parameter  :: WRF_FILE_OPENED_AND_COMMITTED        = 102
    INTEGER ,                INTENT(IN) :: FieldType
    REAL    , DIMENSION(*) , INTENT(IN) :: inbuf_r
    INTEGER , DIMENSION(*) , INTENT(IN) :: inbuf_i
    INTEGER , DIMENSION(3) , INTENT(IN) :: DomainStart , DomainEnd , MemoryStart , MemoryEnd , PatchStart , PatchEnd
    CHARACTER*(*)    , INTENT(IN) :: DateStr , VarName, MemoryOrder , Stagger, DimNames(3)

    CHARACTER*120 mess
    INTEGER               :: l,m,n,ii,jj
    LOGICAL               :: found

    
    ii = 1
    found = .false.
    DO WHILE ( .NOT. found .AND. ii .LE. num_entries )
      
      IF ( TRIM(VarName) .EQ. TRIM(outbuf_table(ii)%VarName) ) THEN
        IF ( TRIM(DateStr) .EQ. TRIM(outbuf_table(ii)%DateStr) ) THEN
          IF ( TRIM(MemoryOrder) .EQ. TRIM(outbuf_table(ii)%MemoryOrder) ) THEN
            found = .true.
          ELSE
            CALL wrf_error_fatal3("",281,&
"store_patch_in_outbuf: memory order disagreement")
          ENDIF
        ELSE
          CALL wrf_error_fatal3("",285,&
"store_patch_in_outbuf: multiple dates in buffer")
        ENDIF
      ELSE
        ii = ii + 1
      ENDIF
    ENDDO
    IF ( .NOT. found ) THEN
      num_entries = num_entries + 1
      IF      ( FieldType .EQ. WRF_FLOAT ) THEN
        ALLOCATE( outbuf_table(num_entries)%rptr(DomainStart(1):DomainEnd(1), &
                                                 DomainStart(2):DomainEnd(2),DomainStart(3):DomainEnd(3)) )
      ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN
        ALLOCATE( outbuf_table(num_entries)%iptr(DomainStart(1):DomainEnd(1), &
                                                 DomainStart(2):DomainEnd(2),DomainStart(3):DomainEnd(3)) )
      ELSE
        write(mess,*)"store_patch_in_outbuf: unsupported type ", FieldType
        CALL wrf_error_fatal3("",302,&
mess)
      ENDIF
      outbuf_table(num_entries)%VarName = TRIM(VarName)
      outbuf_table(num_entries)%DateStr = TRIM(DateStr)
      outbuf_table(num_entries)%MemoryOrder = TRIM(MemoryOrder)
      outbuf_table(num_entries)%Stagger = TRIM(Stagger)
      outbuf_table(num_entries)%DimNames(1) = TRIM(DimNames(1))
      outbuf_table(num_entries)%DimNames(2) = TRIM(DimNames(2))
      outbuf_table(num_entries)%DimNames(3) = TRIM(DimNames(3))
      outbuf_table(num_entries)%DomainStart = DomainStart
      outbuf_table(num_entries)%DomainEnd = DomainEnd
      outbuf_table(num_entries)%FieldType = FieldType
      ii = num_entries
    ENDIF
    jj = 1
    IF (  FieldType .EQ. WRF_FLOAT ) THEN
      DO n = PatchStart(3),PatchEnd(3)
        DO m = PatchStart(2),PatchEnd(2)
          DO l = PatchStart(1),PatchEnd(1)
            outbuf_table(ii)%rptr(l,m,n) = inbuf_r(jj)
            jj = jj + 1
          ENDDO
        ENDDO
      ENDDO
    ENDIF
    IF (  FieldType .EQ. WRF_INTEGER ) THEN
      DO n = PatchStart(3),PatchEnd(3)
        DO m = PatchStart(2),PatchEnd(2)
          DO l = PatchStart(1),PatchEnd(1)
            outbuf_table(ii)%iptr(l,m,n) = inbuf_i(jj)
            jj = jj + 1
          ENDDO
        ENDDO
      ENDDO
    ENDIF

    RETURN

  END SUBROUTINE store_patch_in_outbuf



  SUBROUTINE add_to_bufsize_for_field( VarName, Nbytes )









    USE module_quilt_outbuf_ops
    IMPLICIT NONE
    CHARACTER*(*)    , INTENT(IN) :: VarName
    INTEGER          , INTENT(IN) :: Nbytes

    CHARACTER*120 mess
    INTEGER               :: i, ierr
    INTEGER               :: VarNameAsInts( 256 )
    VarNameAsInts( 1 ) = len(trim(VarName))
    DO i = 2, len(trim(VarName)) + 1
      VarNameAsInts( i ) = ICHAR( VarName(i-1:i-1) )
    ENDDO
    CALL add_to_bufsize_for_field_c ( VarNameAsInts, Nbytes )
    RETURN
  END SUBROUTINE add_to_bufsize_for_field
  
  SUBROUTINE store_piece_of_field( inbuf, VarName, Nbytes )









    USE module_quilt_outbuf_ops
    IMPLICIT NONE
    INTEGER ,                INTENT(IN) :: Nbytes
    INTEGER , DIMENSION(*) , INTENT(IN) :: inbuf
    CHARACTER*(*)    , INTENT(IN) :: VarName

    CHARACTER*120 mess
    INTEGER               :: i, ierr
    INTEGER               :: VarNameAsInts( 256 )

    VarNameAsInts( 1 ) = len(trim(VarName))
    DO i = 2, len(trim(VarName)) + 1
      VarNameAsInts( i ) = ICHAR( VarName(i-1:i-1) )
    ENDDO
    CALL store_piece_of_field_c ( inbuf, VarNameAsInts, Nbytes, ierr )
    IF ( ierr .NE. 0 ) CALL wrf_error_fatal3("",396,&
"store_piece_of_field" )
    RETURN
  END SUBROUTINE store_piece_of_field

  SUBROUTINE retrieve_pieces_of_field( outbuf, VarName, obufsz, Nbytes_tot, lret )













    USE module_quilt_outbuf_ops
    IMPLICIT NONE
    INTEGER ,                INTENT(IN) :: obufsz
    INTEGER ,                INTENT(OUT) :: Nbytes_tot
    INTEGER , DIMENSION(*) , INTENT(OUT) :: outbuf
    CHARACTER*(*)    , INTENT(OUT) :: VarName
    LOGICAL                       :: lret   

    CHARACTER*120 mess
    INTEGER               :: i, iret
    INTEGER               :: VarNameAsInts( 256 )

    CALL retrieve_pieces_of_field_c ( outbuf, VarNameAsInts, obufsz, Nbytes_tot, iret )
    IF ( iret .NE.  0 ) THEN
       lret = .FALSE.
    ELSE
       lret = .TRUE.
       VarName = ' '
       DO i = 2, VarNameAsInts(1) + 1
         VarName(i-1:i-1) = CHAR(VarNameAsInts( i ))
       ENDDO
    ENDIF
    RETURN
  END SUBROUTINE retrieve_pieces_of_field

