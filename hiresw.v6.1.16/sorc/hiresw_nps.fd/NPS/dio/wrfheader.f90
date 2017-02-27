module wrfheader

! Dusan Jovic, NCEP, 2008

  implicit none
  private

  public wrfheader_pack
  public wrfheader_unpack

  integer, parameter :: int_field = 530

!  private :: int_pack_string, int_unpack_string

contains

  subroutine wrfheader_pack(hdrbuf, &
                            DateStr,VarName,Units,Description, &
                            FieldType, &
                            MemoryOrder,Stagger, &
                            DimNames,DomainStart,DomainEnd,PatchStart,PatchEnd)

    implicit none

    integer, dimension(:), intent(out) :: hdrbuf
    character(len=*), intent(in) :: DateStr
    character(len=*), intent(in) :: VarName
    character(len=*), intent(in) :: Units
    character(len=*), intent(in) :: Description
    integer, intent(in) :: FieldType
    character(len=*), intent(in) :: MemoryOrder
    character(len=*), intent(in) :: Stagger
    character(len=*), dimension(:), intent(in) :: DimNames
    integer, dimension(:), intent(in) :: DomainStart, DomainEnd
    integer, dimension(:), intent(in) :: PatchStart,  PatchEnd


    integer :: i, n

    if ( size(hdrbuf) < 512 ) then
       write(0,*) " size(hdrbuf) < 512 "
       stop
    end if

    hdrbuf = 0
    hdrbuf(1) = 0 ! deferred -- this will be length of header
    hdrbuf(2) = int_field
    hdrbuf(3) = FieldType

    i = 4
    call int_pack_string( DateStr, hdrbuf(i:), n )         ; i = i + n
    call int_pack_string( VarName, hdrbuf(i:), n )         ; i = i + n
    call int_pack_string( Units,   hdrbuf(i:), n )         ; i = i + n
    call int_pack_string( Description, hdrbuf(i:), n )     ; i = i + n
    call int_pack_string( MemoryOrder, hdrbuf(i:), n )     ; i = i + n
    call int_pack_string( Stagger,     hdrbuf(i:), n )     ; i = i + n
    call int_pack_string( DimNames(1), hdrbuf(i:), n )     ; i = i + n
    call int_pack_string( DimNames(2), hdrbuf(i:), n )     ; i = i + n
    call int_pack_string( DimNames(3), hdrbuf(i:), n )     ; i = i + n
    hdrbuf(i) = DomainStart(1)                             ; i = i+1
    hdrbuf(i) = DomainStart(2)                             ; i = i+1
    hdrbuf(i) = DomainStart(3)                             ; i = i+1
    hdrbuf(i) = DomainEnd(1)                               ; i = i+1
    hdrbuf(i) = DomainEnd(2)                               ; i = i+1
    hdrbuf(i) = DomainEnd(3)                               ; i = i+1
    hdrbuf(i) = PatchStart(1)                              ; i = i+1
    hdrbuf(i) = PatchStart(2)                              ; i = i+1
    hdrbuf(i) = PatchStart(3)                              ; i = i+1
    hdrbuf(i) = PatchEnd(1)                                ; i = i+1
    hdrbuf(i) = PatchEnd(2)                                ; i = i+1
    hdrbuf(i) = PatchEnd(3)                                ; i = i+1

    hdrbuf(1) = (i-1) * BIT_SIZE(hdrbuf(1))  ! return the number in bytes

  end subroutine wrfheader_pack

  subroutine wrfheader_unpack(hdrbuf, &
                              DateStr,VarName,Units,Description, &
                              FieldType, &
                              MemoryOrder,Stagger, &
                              DimNames,DomainStart,DomainEnd,PatchStart,PatchEnd)

    implicit none

    integer, dimension(:), intent(in) :: hdrbuf
    character(len=*), intent(out) :: DateStr
    character(len=*), intent(out) :: VarName
    character(len=*), intent(out) :: Units
    character(len=*), intent(out) :: Description
    integer, intent(out) :: FieldType
    character(len=*), intent(out) :: MemoryOrder
    character(len=*), intent(out) :: Stagger
    character(len=*), dimension(:), intent(out) :: DimNames
    integer, dimension(:), intent(out) :: DomainStart, DomainEnd
    integer, dimension(:), intent(out) :: PatchStart,  PatchEnd


    integer :: hdrbufsize
    integer :: i, n

    if ( size(hdrbuf) < 512 ) then
       write(0,*) " size(hdrbuf) < 512 "
       stop
    end if

    hdrbufsize = hdrbuf(1)
    if ( hdrbuf(2) /= int_field ) then
      write(0,*)'dio_unpack_field_header: hdrbuf(2) ne int_field ',hdrbuf(2),int_field
      stop
    end if
    FieldType = hdrbuf(3)

    i = 4
    call int_unpack_string( DateStr, hdrbuf(i:), n )       ; i = i + n
    call int_unpack_string( VarName, hdrbuf(i:), n )       ; i = i + n
    call int_unpack_string( Units,   hdrbuf(i:), n )       ; i = i + n
    call int_unpack_string( Description, hdrbuf(i:), n )   ; i = i + n
    call int_unpack_string( MemoryOrder, hdrbuf(i:), n )   ; i = i + n
    call int_unpack_string( Stagger,     hdrbuf(i:), n )   ; i = i + n
    call int_unpack_string( DimNames(1), hdrbuf(i:), n )   ; i = i + n
    call int_unpack_string( DimNames(2), hdrbuf(i:), n )   ; i = i + n
    call int_unpack_string( DimNames(3), hdrbuf(i:), n )   ; i = i + n
    DomainStart(1) = hdrbuf(i)                             ; i = i+1
    DomainStart(2) = hdrbuf(i)                             ; i = i+1
    DomainStart(3) = hdrbuf(i)                             ; i = i+1
    DomainEnd(1) = hdrbuf(i)                               ; i = i+1
    DomainEnd(2) = hdrbuf(i)                               ; i = i+1
    DomainEnd(3) = hdrbuf(i)                               ; i = i+1
    PatchStart(1) = hdrbuf(i)                              ; i = i+1
    PatchStart(2) = hdrbuf(i)                              ; i = i+1
    PatchStart(3) = hdrbuf(i)                              ; i = i+1
    PatchEnd(1) = hdrbuf(i)                                ; i = i+1
    PatchEnd(2) = hdrbuf(i)                                ; i = i+1
    PatchEnd(3) = hdrbuf(i)                                ; i = i+1

  end subroutine wrfheader_unpack

! first int is length of string to follow then string encodes as ints
SUBROUTINE int_pack_string ( str, buf, n )
  IMPLICIT NONE
! This routine is used to store a string as a sequence of integers.
! The first integer is the string length.
  CHARACTER(len=*), INTENT(IN)          :: str
  INTEGER, INTENT(OUT), DIMENSION(:) :: buf
  INTEGER, INTENT(OUT)               :: n    ! on return, N is the number of ints stored in buf
!Local
  INTEGER i
!
  n = 1
  buf(n) = LEN(TRIM(str))
  n = n+1
  DO i = 1, LEN(TRIM(str))
    buf(n) = ichar(str(i:i))
    n = n+1
  ENDDO
  n = n - 1
END SUBROUTINE int_pack_string

SUBROUTINE int_unpack_string ( str, buf, n )
  IMPLICIT NONE
! This routine is used to extract a string from a sequence of integers.
! The first integer is the string length.
  CHARACTER(len=*), INTENT(OUT)        :: str
  INTEGER, INTENT(IN), DIMENSION(:) :: buf
  INTEGER, INTENT(OUT)              :: n       ! on return, N is the number of ints copied from buf
!Local
  INTEGER i
  INTEGER strlen

  strlen = buf(1)
  str = ""
  DO i = 1, min(strlen,len(str))
    str(i:i) = char(buf(i+1))
  ENDDO
  n = strlen + 1
END SUBROUTINE int_unpack_string


end module wrfheader
