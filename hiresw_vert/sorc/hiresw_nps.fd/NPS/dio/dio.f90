module dio

! Dusan Jovic, NCEP, 2008

  implicit none
  private

  integer, parameter, public :: integer1_kind = selected_int_kind(1)
  integer, parameter, public :: integer2_kind = selected_int_kind(4)
  integer, parameter, public :: integer4_kind = selected_int_kind(8)
  integer, parameter, public :: integer8_kind = selected_int_kind(16)

  integer, parameter, public :: real4_kind = selected_real_kind(6)
  integer, parameter, public :: real8_kind = selected_real_kind(15)

  integer, parameter, public :: logical1_kind = 1
  integer, parameter, public :: logical4_kind = 4

  integer, parameter :: MAXREC = 1024*8

  integer, parameter, public :: ENDIAN_NATIVE=0
  integer, parameter, public :: ENDIAN_BIG=1
  integer, parameter, public :: ENDIAN_LITTLE=2

  type dio_record
    private
    character(len=32) :: name
    integer :: rank
    integer :: dtype
    integer, dimension(10) :: bounds
    integer(kind=8) :: offset
  end type dio_record

  type,public :: dio_file
    private
    integer flunit
    integer mode
    type(dio_record), dimension(MAXREC) :: records
    integer :: nrec
    integer :: endianness
    integer :: swap
  end type dio_file

  public dio_init
  public dio_finalize
  public dio_open
  public dio_close
  public dio_write
  public dio_read
  public dio_numrec
  public dio_recinfo

  integer, public, parameter :: DIO_INTEGER1=101
  integer, public, parameter :: DIO_INTEGER2=102
  integer, public, parameter :: DIO_INTEGER4=103
  integer, public, parameter :: DIO_INTEGER8=108
  integer, public, parameter :: DIO_REAL4=204
  integer, public, parameter :: DIO_REAL8=208
  integer, public, parameter :: DIO_LOGICAL1=401
  integer, public, parameter :: DIO_LOGICAL4=404
  integer, public, parameter :: DIO_CHARACTER=501

  interface dio_write
    module procedure dio_write0c
    module procedure dio_write_r0_integer1
    module procedure dio_write_r1_integer1
    module procedure dio_write_r2_integer1
    module procedure dio_write_r3_integer1
    module procedure dio_write_r4_integer1
    module procedure dio_write_r0_integer2
    module procedure dio_write_r1_integer2
    module procedure dio_write_r2_integer2
    module procedure dio_write_r3_integer2
    module procedure dio_write_r4_integer2
    module procedure dio_write_r0_integer4
    module procedure dio_write_r1_integer4
    module procedure dio_write_r2_integer4
    module procedure dio_write_r3_integer4
    module procedure dio_write_r4_integer4
    module procedure dio_write_r0_integer8
    module procedure dio_write_r1_integer8
    module procedure dio_write_r2_integer8
    module procedure dio_write_r3_integer8
    module procedure dio_write_r4_integer8
    module procedure dio_write_r0_real4
    module procedure dio_write_r1_real4
    module procedure dio_write_r2_real4
    module procedure dio_write_r3_real4
    module procedure dio_write_r4_real4
    module procedure dio_write_r0_real8
    module procedure dio_write_r1_real8
    module procedure dio_write_r2_real8
    module procedure dio_write_r3_real8
    module procedure dio_write_r4_real8
    module procedure dio_write_r0_logical1
    module procedure dio_write_r1_logical1
    module procedure dio_write_r2_logical1
    module procedure dio_write_r3_logical1
    module procedure dio_write_r4_logical1
    module procedure dio_write_r0_logical4
    module procedure dio_write_r1_logical4
    module procedure dio_write_r2_logical4
    module procedure dio_write_r3_logical4
    module procedure dio_write_r4_logical4
  end interface dio_write

  interface dio_read
    module procedure dio_read0c
    module procedure dio_read_r0_integer1
    module procedure dio_read_r1_integer1
    module procedure dio_read_r2_integer1
    module procedure dio_read_r3_integer1
    module procedure dio_read_r4_integer1
    module procedure dio_read_r0_integer2
    module procedure dio_read_r1_integer2
    module procedure dio_read_r2_integer2
    module procedure dio_read_r3_integer2
    module procedure dio_read_r4_integer2
    module procedure dio_read_r0_integer4
    module procedure dio_read_r1_integer4
    module procedure dio_read_r2_integer4
    module procedure dio_read_r3_integer4
    module procedure dio_read_r4_integer4
    module procedure dio_read_r0_integer8
    module procedure dio_read_r1_integer8
    module procedure dio_read_r2_integer8
    module procedure dio_read_r3_integer8
    module procedure dio_read_r4_integer8
    module procedure dio_read_r0_real4
    module procedure dio_read_r1_real4
    module procedure dio_read_r2_real4
    module procedure dio_read_r3_real4
    module procedure dio_read_r4_real4
    module procedure dio_read_r0_real8
    module procedure dio_read_r1_real8
    module procedure dio_read_r2_real8
    module procedure dio_read_r3_real8
    module procedure dio_read_r4_real8
    module procedure dio_read_r0_logical1
    module procedure dio_read_r1_logical1
    module procedure dio_read_r2_logical1
    module procedure dio_read_r3_logical1
    module procedure dio_read_r4_logical1
    module procedure dio_read_r0_logical4
    module procedure dio_read_r1_logical4
    module procedure dio_read_r2_logical4
    module procedure dio_read_r3_logical4
    module procedure dio_read_r4_logical4
  end interface dio_read

  character(len=4), parameter :: VERSION="DIO1"

contains

  logical function is_little_endian()

    implicit none
    integer :: endian
    call c_endian(endian)

    is_little_endian = (endian == 1)

  end function is_little_endian

  subroutine dio_init(iret)
    implicit none
    integer,optional,intent(out):: iret

    if ( present(iret) ) iret=0

  end subroutine dio_init

  subroutine dio_finalize()
    implicit none
  end subroutine dio_finalize

  subroutine dio_open(dfile,dfname,daction,mode,endianness,iret)
    implicit none
    type(dio_file),intent(inout)         :: dfile
    character(len=*),intent(in)          :: dfname
    character(len=*),intent(in)          :: daction
    character(len=*),optional,intent(in) :: mode
    integer,optional,intent(in)          :: endianness
    integer,optional,intent(out)         :: iret

    integer :: ios
    integer :: n
    integer :: reclen, rank, dtype
    integer(kind=8) :: offset
    integer(kind=8) :: fsize
    integer(kind=8) :: dataseek
    character(len=4) :: fversion
    integer(kind=integer4_kind) :: four
    integer :: fendianness
    character(len=32) :: vname = "                                "
    integer,dimension(:),allocatable :: bounds
    integer :: my_endianness
    integer :: width,size

    if ( present(iret) ) iret=0

    if ( present(mode) ) then
      if (mode == "APPEND" .or. mode == "append") then
        dfile%mode=1
      else if (mode == "OVERWRITE" .or. mode == "overwrite") then
        dfile%mode=0
      else
        write(0,*)"error dio_open unknown mode ", mode
        if ( present(iret) )  then
         iret=-4
         return
       else
         call dio_stop
       endif
      end if
    else
      dfile%mode=1
    end if

    my_endianness = ENDIAN_BIG
    if ( is_little_endian() ) my_endianness = ENDIAN_LITTLE
    write(0,*)'my_endianness =',my_endianness
    write(0,*)'is_little_endian() =',is_little_endian()

    if (daction .eq. "write" .or. daction .eq. "WRITE") then

      if ( present(endianness) ) then
        if (endianness == ENDIAN_BIG ) then
          dfile%endianness = ENDIAN_BIG
        else
          dfile%endianness = ENDIAN_LITTLE
        endif
      else
        dfile%endianness = ENDIAN_BIG
      end if

      write(0,*)'dfile%endianness =',dfile%endianness

      dfile%swap = 0
      if ( my_endianness /= dfile%endianness ) dfile%swap = 1

      call c_fbopen(dfile%flunit,trim(dfname),len(trim(dfname)),"W",ios)
      if ( ios /= 0 ) then
        write(0,*)"error in open rc = ",ios
        if ( present(iret) )  then
          iret=ios
          return
        else
          call dio_stop
        endif
      endif

      width=1
      size=4
      call c_fbwrite(dfile%flunit,VERSION,width,size,dfile%swap,ios)
!d      width=4
!d      size=1
!d      call c_fbwrite(dfile%flunit,dfile%endianness,width,size,dfile%swap,ios)

      do n=1,MAXREC
        dfile%records(n)%name = "                                "
        dfile%records(n)%rank = 0
        dfile%records(n)%dtype = 0
        dfile%records(n)%bounds = 0
        dfile%records(n)%offset = 0
      end do
      dfile%nrec = 0

    elseif (daction .eq. "read" .or. daction .eq. "READ" .or. &
            daction .eq. "readwrite" .or. daction .eq. "READWRITE" ) then

      if (daction .eq. "read" .or. daction .eq. "READ") then
      call c_fbopen(dfile%flunit,trim(dfname),len(trim(dfname)),"R",ios)
      else
      call c_fbopen(dfile%flunit,trim(dfname),len(trim(dfname)),"O",ios)
      end if
      if ( ios /= 0 ) then
        write(0,*)"error in open rc = ",ios
        if ( present(iret) )  then
          iret=ios
          return
        else
          call dio_stop
        endif
      endif

      call c_fbsize(dfile%flunit,fsize,ios)
      dataseek = 0
      call c_fbseekset(dfile%flunit,dataseek,ios) ; if (ios/=0) stop 1

! at this moment we stil do not know the endianness of the file
! we know that the first record is 4 bytes long

      call c_fbread_4bytes(dfile%flunit,four,ios) ; if (ios/=0) stop 1
      if (four ==4) then
        dfile%swap = 0
      else
        dfile%swap = 1
      end if
! rewind back
      dataseek = 0
      call c_fbseekset(dfile%flunit,dataseek,ios) ; if (ios/=0) stop 1

      width=1;
      size=4;
      call c_fbread(dfile%flunit,fversion,width,size,dfile%swap,ios) ; if (ios/=0) stop 1

      if ( fversion /= VERSION ) then
         write(0,*)" error in dio_open version =/ VERSION " ,"|",fversion,"|",VERSION,"|"
         if ( present(iret) )  then
          iret=-2
          return
        else
          call dio_stop
        endif
      end if

!d      width=4;
!d      size=1;
!d      call c_fbread(dfile%flunit,fendianness,width,size,dfile%swap,ios) ; if (ios/=0) stop 1
!d
!d      if ( my_endianness == fendianness .and. dfile%swap == 1) then
!d         write(0,*)" something is wrong: my_endianness == fendianness .and. dfile%swap == 1"
!d         write(0,*)" my_endianness = ",my_endianness
!d         write(0,*)" fendianness = ",fendianness
!d         write(0,*)" dfile%swap = ",dfile%swap
!d         stop 1
!d      end if

      dfile%nrec = 0
      call c_fbtell(dfile%flunit,offset,ios)
      do while(offset < fsize)

        !! name
        vname = "                                "
        width=1
        size=32
        call c_fbread(dfile%flunit,vname,width,size,dfile%swap,ios) ; if (ios/=0) stop 2

        !! rank
        width=4
        size=1
        call c_fbread(dfile%flunit,rank,width,size,dfile%swap,ios) ; if (ios/=0) stop 3

        !! dtype
        width=4
        size=1
        call c_fbread(dfile%flunit,dtype,width,size,dfile%swap,ios) ; if (ios/=0) stop 4

        ! bounds
        allocate(bounds(2*rank))
        width=4
        size=2*rank
        call c_fbread(dfile%flunit,bounds,width,size,dfile%swap,ios) ; if (ios/=0) stop 5

        ! header
        call c_fbseek_record(dfile%flunit,dfile%swap,ios) ; if (ios/=0) stop 6

        ! data
        call c_fbseek_record(dfile%flunit,dfile%swap,ios) ; if (ios/=0) stop 7

        dfile%nrec=dfile%nrec+1
        dfile%records(dfile%nrec)%name = trim(vname)
        dfile%records(dfile%nrec)%rank = rank
        dfile%records(dfile%nrec)%dtype = dtype
        dfile%records(dfile%nrec)%bounds(1:2*rank) = bounds(1:2*rank)
        dfile%records(dfile%nrec)%offset = offset

!        write(0,*)trim(vname),rank,dtype,offset

        deallocate(bounds)

        call c_fbtell(dfile%flunit,offset,ios) ; if (ios/=0) stop 8

      end do


    else
      if ( present(iret) ) then
        iret=-1
        return
      else
        call dio_stop
      endif
    endif

  end subroutine dio_open

  subroutine dio_close(dfile,iret)
    implicit none
    type(dio_file),intent(inout) :: dfile
    integer,optional,intent(out) :: iret

    integer :: ios

    if ( present(iret) ) iret=0

    call c_fbclose(dfile%flunit,ios)
    if ( ios /= 0 ) then
       if ( present(iret) ) then
         return
       else
         call dio_stop
       endif
    endif

  end subroutine dio_close

  subroutine dio_stop()
    implicit none
    write(0,*)' stop in dio_stop'
    stop
  end subroutine dio_stop


  subroutine dio_write_r0_integer1(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(inout) :: dfile
    character(*),intent(in) :: name
    integer(kind=integer1_kind),intent(in) :: data
    integer,dimension(:),optional,intent(in) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, n, nrec
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    if (present(header)) then
       myheader(1:512) = header(1:512)
    end if

    rank=0
    dtype=DIO_integer1
    allocate(bounds(2*rank))

    if (dfile%mode == 1) then
      if ( appendname(dfile,vname) <= 0 ) then
        write(0,*)" record ",trim(vname)," already exists"
        stop
      end if
    else
      nrec = readname(dfile,vname)
      if (nrec<=0) then
        write(0,*)" record ", trim(vname), " does not exist"
        stop
      end if
      call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    end if

    call c_fbwrite_record(dfile%flunit,vname,rank,dtype,1,bounds,data,myheader,dfile%swap,ios)

  end subroutine dio_write_r0_integer1


  subroutine dio_read_r0_integer1(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(in) :: dfile
    character(*),intent(in) :: name
    integer(kind=integer1_kind),intent(out) :: data
    integer,dimension(:),optional,intent(out) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, nrec, n, reclen
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    rank=0
    dtype=DIO_integer1
    allocate(bounds(2*rank))

    nrec = readname(dfile,vname)
    if (nrec<=0) then
      write(0,*)" record ", trim(vname), " does not exist"
      stop
    end if

    call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    call c_fbread_record(dfile%flunit,vname,rank,dtype,1,bounds,data,myheader,dfile%swap,ios)

    if (present(header)) then
       header(1:512) = myheader(1:512)
    end if

  end subroutine dio_read_r0_integer1


  subroutine dio_write_r1_integer1(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(inout) :: dfile
    character(*),intent(in) :: name
    integer(kind=integer1_kind),dimension(:),intent(in) :: data
    integer,dimension(:),optional,intent(in) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, n, nrec
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    if (present(header)) then
       myheader(1:512) = header(1:512)
    end if

    rank=1
    dtype=DIO_integer1
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    if (dfile%mode == 1) then
      if ( appendname(dfile,vname) <= 0 ) then
        write(0,*)" record ",trim(vname)," already exists"
        stop
      end if
    else
      nrec = readname(dfile,vname)
      if (nrec<=0) then
        write(0,*)" record ", trim(vname), " does not exist"
        stop
      end if
      call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    end if

    call c_fbwrite_record(dfile%flunit,vname,rank,dtype,1,bounds,data,myheader,dfile%swap,ios)

  end subroutine dio_write_r1_integer1


  subroutine dio_read_r1_integer1(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(in) :: dfile
    character(*),intent(in) :: name
    integer(kind=integer1_kind),dimension(:),intent(out) :: data
    integer,dimension(:),optional,intent(out) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, nrec, n, reclen
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    rank=1
    dtype=DIO_integer1
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    nrec = readname(dfile,vname)
    if (nrec<=0) then
      write(0,*)" record ", trim(vname), " does not exist"
      stop
    end if

    call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    call c_fbread_record(dfile%flunit,vname,rank,dtype,1,bounds,data,myheader,dfile%swap,ios)

    if (present(header)) then
       header(1:512) = myheader(1:512)
    end if

  end subroutine dio_read_r1_integer1


  subroutine dio_write_r2_integer1(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(inout) :: dfile
    character(*),intent(in) :: name
    integer(kind=integer1_kind),dimension(:,:),intent(in) :: data
    integer,dimension(:),optional,intent(in) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, n, nrec
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    if (present(header)) then
       myheader(1:512) = header(1:512)
    end if

    rank=2
    dtype=DIO_integer1
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    if (dfile%mode == 1) then
      if ( appendname(dfile,vname) <= 0 ) then
        write(0,*)" record ",trim(vname)," already exists"
        stop
      end if
    else
      nrec = readname(dfile,vname)
      if (nrec<=0) then
        write(0,*)" record ", trim(vname), " does not exist"
        stop
      end if
      call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    end if

    call c_fbwrite_record(dfile%flunit,vname,rank,dtype,1,bounds,data,myheader,dfile%swap,ios)

  end subroutine dio_write_r2_integer1


  subroutine dio_read_r2_integer1(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(in) :: dfile
    character(*),intent(in) :: name
    integer(kind=integer1_kind),dimension(:,:),intent(out) :: data
    integer,dimension(:),optional,intent(out) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, nrec, n, reclen
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    rank=2
    dtype=DIO_integer1
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    nrec = readname(dfile,vname)
    if (nrec<=0) then
      write(0,*)" record ", trim(vname), " does not exist"
      stop
    end if

    call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    call c_fbread_record(dfile%flunit,vname,rank,dtype,1,bounds,data,myheader,dfile%swap,ios)

    if (present(header)) then
       header(1:512) = myheader(1:512)
    end if

  end subroutine dio_read_r2_integer1


  subroutine dio_write_r3_integer1(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(inout) :: dfile
    character(*),intent(in) :: name
    integer(kind=integer1_kind),dimension(:,:,:),intent(in) :: data
    integer,dimension(:),optional,intent(in) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, n, nrec
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    if (present(header)) then
       myheader(1:512) = header(1:512)
    end if

    rank=3
    dtype=DIO_integer1
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    if (dfile%mode == 1) then
      if ( appendname(dfile,vname) <= 0 ) then
        write(0,*)" record ",trim(vname)," already exists"
        stop
      end if
    else
      nrec = readname(dfile,vname)
      if (nrec<=0) then
        write(0,*)" record ", trim(vname), " does not exist"
        stop
      end if
      call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    end if

    call c_fbwrite_record(dfile%flunit,vname,rank,dtype,1,bounds,data,myheader,dfile%swap,ios)

  end subroutine dio_write_r3_integer1


  subroutine dio_read_r3_integer1(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(in) :: dfile
    character(*),intent(in) :: name
    integer(kind=integer1_kind),dimension(:,:,:),intent(out) :: data
    integer,dimension(:),optional,intent(out) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, nrec, n, reclen
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    rank=3
    dtype=DIO_integer1
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    nrec = readname(dfile,vname)
    if (nrec<=0) then
      write(0,*)" record ", trim(vname), " does not exist"
      stop
    end if

    call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    call c_fbread_record(dfile%flunit,vname,rank,dtype,1,bounds,data,myheader,dfile%swap,ios)

    if (present(header)) then
       header(1:512) = myheader(1:512)
    end if

  end subroutine dio_read_r3_integer1


  subroutine dio_write_r4_integer1(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(inout) :: dfile
    character(*),intent(in) :: name
    integer(kind=integer1_kind),dimension(:,:,:,:),intent(in) :: data
    integer,dimension(:),optional,intent(in) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, n, nrec
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    if (present(header)) then
       myheader(1:512) = header(1:512)
    end if

    rank=4
    dtype=DIO_integer1
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    if (dfile%mode == 1) then
      if ( appendname(dfile,vname) <= 0 ) then
        write(0,*)" record ",trim(vname)," already exists"
        stop
      end if
    else
      nrec = readname(dfile,vname)
      if (nrec<=0) then
        write(0,*)" record ", trim(vname), " does not exist"
        stop
      end if
      call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    end if

    call c_fbwrite_record(dfile%flunit,vname,rank,dtype,1,bounds,data,myheader,dfile%swap,ios)

  end subroutine dio_write_r4_integer1


  subroutine dio_read_r4_integer1(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(in) :: dfile
    character(*),intent(in) :: name
    integer(kind=integer1_kind),dimension(:,:,:,:),intent(out) :: data
    integer,dimension(:),optional,intent(out) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, nrec, n, reclen
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    rank=4
    dtype=DIO_integer1
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    nrec = readname(dfile,vname)
    if (nrec<=0) then
      write(0,*)" record ", trim(vname), " does not exist"
      stop
    end if

    call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    call c_fbread_record(dfile%flunit,vname,rank,dtype,1,bounds,data,myheader,dfile%swap,ios)

    if (present(header)) then
       header(1:512) = myheader(1:512)
    end if

  end subroutine dio_read_r4_integer1


  subroutine dio_write_r0_integer2(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(inout) :: dfile
    character(*),intent(in) :: name
    integer(kind=integer2_kind),intent(in) :: data
    integer,dimension(:),optional,intent(in) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, n, nrec
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    if (present(header)) then
       myheader(1:512) = header(1:512)
    end if

    rank=0
    dtype=DIO_integer2
    allocate(bounds(2*rank))

    if (dfile%mode == 1) then
      if ( appendname(dfile,vname) <= 0 ) then
        write(0,*)" record ",trim(vname)," already exists"
        stop
      end if
    else
      nrec = readname(dfile,vname)
      if (nrec<=0) then
        write(0,*)" record ", trim(vname), " does not exist"
        stop
      end if
      call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    end if

    call c_fbwrite_record(dfile%flunit,vname,rank,dtype,2,bounds,data,myheader,dfile%swap,ios)

  end subroutine dio_write_r0_integer2


  subroutine dio_read_r0_integer2(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(in) :: dfile
    character(*),intent(in) :: name
    integer(kind=integer2_kind),intent(out) :: data
    integer,dimension(:),optional,intent(out) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, nrec, n, reclen
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    rank=0
    dtype=DIO_integer2
    allocate(bounds(2*rank))

    nrec = readname(dfile,vname)
    if (nrec<=0) then
      write(0,*)" record ", trim(vname), " does not exist"
      stop
    end if

    call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    call c_fbread_record(dfile%flunit,vname,rank,dtype,2,bounds,data,myheader,dfile%swap,ios)
!d    if (dfile%swap==1) then
!d      call swap(data)
!d    end if

    if (present(header)) then
       header(1:512) = myheader(1:512)
    end if

  end subroutine dio_read_r0_integer2


  subroutine dio_write_r1_integer2(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(inout) :: dfile
    character(*),intent(in) :: name
    integer(kind=integer2_kind),dimension(:),intent(in) :: data
    integer,dimension(:),optional,intent(in) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, n, nrec
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    if (present(header)) then
       myheader(1:512) = header(1:512)
    end if

    rank=1
    dtype=DIO_integer2
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    if (dfile%mode == 1) then
      if ( appendname(dfile,vname) <= 0 ) then
        write(0,*)" record ",trim(vname)," already exists"
        stop
      end if
    else
      nrec = readname(dfile,vname)
      if (nrec<=0) then
        write(0,*)" record ", trim(vname), " does not exist"
        stop
      end if
      call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    end if

!d    if (dfile%swap==1) then
!d      call swap(data)
!d    end if
    call c_fbwrite_record(dfile%flunit,vname,rank,dtype,2,bounds,data,myheader,dfile%swap,ios)

  end subroutine dio_write_r1_integer2


  subroutine dio_read_r1_integer2(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(in) :: dfile
    character(*),intent(in) :: name
    integer(kind=integer2_kind),dimension(:),intent(out) :: data
    integer,dimension(:),optional,intent(out) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, nrec, n, reclen
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    rank=1
    dtype=DIO_integer2
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    nrec = readname(dfile,vname)
    if (nrec<=0) then
      write(0,*)" record ", trim(vname), " does not exist"
      stop
    end if

    call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    call c_fbread_record(dfile%flunit,vname,rank,dtype,2,bounds,data,myheader,dfile%swap,ios)
!d    if (dfile%swap==1) then
!d      call swap(data)
!d    end if

    if (present(header)) then
       header(1:512) = myheader(1:512)
    end if

  end subroutine dio_read_r1_integer2


  subroutine dio_write_r2_integer2(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(inout) :: dfile
    character(*),intent(in) :: name
    integer(kind=integer2_kind),dimension(:,:),intent(in) :: data
    integer,dimension(:),optional,intent(in) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, n, nrec
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    if (present(header)) then
       myheader(1:512) = header(1:512)
    end if

    rank=2
    dtype=DIO_integer2
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    if (dfile%mode == 1) then
      if ( appendname(dfile,vname) <= 0 ) then
        write(0,*)" record ",trim(vname)," already exists"
        stop
      end if
    else
      nrec = readname(dfile,vname)
      if (nrec<=0) then
        write(0,*)" record ", trim(vname), " does not exist"
        stop
      end if
      call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    end if

    call c_fbwrite_record(dfile%flunit,vname,rank,dtype,2,bounds,data,myheader,dfile%swap,ios)

  end subroutine dio_write_r2_integer2


  subroutine dio_read_r2_integer2(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(in) :: dfile
    character(*),intent(in) :: name
    integer(kind=integer2_kind),dimension(:,:),intent(out) :: data
    integer,dimension(:),optional,intent(out) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, nrec, n, reclen
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    rank=2
    dtype=DIO_integer2
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    nrec = readname(dfile,vname)
    if (nrec<=0) then
      write(0,*)" record ", trim(vname), " does not exist"
      stop
    end if

    call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    call c_fbread_record(dfile%flunit,vname,rank,dtype,2,bounds,data,myheader,dfile%swap,ios)

    if (present(header)) then
       header(1:512) = myheader(1:512)
    end if

  end subroutine dio_read_r2_integer2


  subroutine dio_write_r3_integer2(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(inout) :: dfile
    character(*),intent(in) :: name
    integer(kind=integer2_kind),dimension(:,:,:),intent(in) :: data
    integer,dimension(:),optional,intent(in) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, n, nrec
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    if (present(header)) then
       myheader(1:512) = header(1:512)
    end if

    rank=3
    dtype=DIO_integer2
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    if (dfile%mode == 1) then
      if ( appendname(dfile,vname) <= 0 ) then
        write(0,*)" record ",trim(vname)," already exists"
        stop
      end if
    else
      nrec = readname(dfile,vname)
      if (nrec<=0) then
        write(0,*)" record ", trim(vname), " does not exist"
        stop
      end if
      call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    end if

    call c_fbwrite_record(dfile%flunit,vname,rank,dtype,2,bounds,data,myheader,dfile%swap,ios)

  end subroutine dio_write_r3_integer2


  subroutine dio_read_r3_integer2(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(in) :: dfile
    character(*),intent(in) :: name
    integer(kind=integer2_kind),dimension(:,:,:),intent(out) :: data
    integer,dimension(:),optional,intent(out) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, nrec, n, reclen
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    rank=3
    dtype=DIO_integer2
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    nrec = readname(dfile,vname)
    if (nrec<=0) then
      write(0,*)" record ", trim(vname), " does not exist"
      stop
    end if

    call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    call c_fbread_record(dfile%flunit,vname,rank,dtype,2,bounds,data,myheader,dfile%swap,ios)

    if (present(header)) then
       header(1:512) = myheader(1:512)
    end if

  end subroutine dio_read_r3_integer2


  subroutine dio_write_r4_integer2(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(inout) :: dfile
    character(*),intent(in) :: name
    integer(kind=integer2_kind),dimension(:,:,:,:),intent(in) :: data
    integer,dimension(:),optional,intent(in) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, n, nrec
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    if (present(header)) then
       myheader(1:512) = header(1:512)
    end if

    rank=4
    dtype=DIO_integer2
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    if (dfile%mode == 1) then
      if ( appendname(dfile,vname) <= 0 ) then
        write(0,*)" record ",trim(vname)," already exists"
        stop
      end if
    else
      nrec = readname(dfile,vname)
      if (nrec<=0) then
        write(0,*)" record ", trim(vname), " does not exist"
        stop
      end if
      call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    end if

    call c_fbwrite_record(dfile%flunit,vname,rank,dtype,2,bounds,data,myheader,dfile%swap,ios)

  end subroutine dio_write_r4_integer2


  subroutine dio_read_r4_integer2(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(in) :: dfile
    character(*),intent(in) :: name
    integer(kind=integer2_kind),dimension(:,:,:,:),intent(out) :: data
    integer,dimension(:),optional,intent(out) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, nrec, n, reclen
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    rank=4
    dtype=DIO_integer2
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    nrec = readname(dfile,vname)
    if (nrec<=0) then
      write(0,*)" record ", trim(vname), " does not exist"
      stop
    end if

    call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    call c_fbread_record(dfile%flunit,vname,rank,dtype,2,bounds,data,myheader,dfile%swap,ios)

    if (present(header)) then
       header(1:512) = myheader(1:512)
    end if

  end subroutine dio_read_r4_integer2


  subroutine dio_write_r0_integer4(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(inout) :: dfile
    character(*),intent(in) :: name
    integer(kind=integer4_kind),intent(in) :: data
    integer,dimension(:),optional,intent(in) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, n, nrec
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    if (present(header)) then
       myheader(1:512) = header(1:512)
    end if

    rank=0
    dtype=DIO_integer4
    allocate(bounds(2*rank))

    if (dfile%mode == 1) then
      if ( appendname(dfile,vname) <= 0 ) then
        write(0,*)" record ",trim(vname)," already exists"
        stop
      end if
    else
      nrec = readname(dfile,vname)
      if (nrec<=0) then
        write(0,*)" record ", trim(vname), " does not exist"
        stop
      end if
      call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    end if

    call c_fbwrite_record(dfile%flunit,vname,rank,dtype,4,bounds,data,myheader,dfile%swap,ios)

  end subroutine dio_write_r0_integer4


  subroutine dio_read_r0_integer4(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(in) :: dfile
    character(*),intent(in) :: name
    integer(kind=integer4_kind),intent(out) :: data
    integer,dimension(:),optional,intent(out) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, nrec, n, reclen
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    rank=0
    dtype=DIO_integer4
    allocate(bounds(2*rank))

    nrec = readname(dfile,vname)
    if (nrec<=0) then
      write(0,*)" record ", trim(vname), " does not exist"
      stop
    end if

    call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    call c_fbread_record(dfile%flunit,vname,rank,dtype,4,bounds,data,myheader,dfile%swap,ios)

    if (present(header)) then
       header(1:512) = myheader(1:512)
    end if

  end subroutine dio_read_r0_integer4


  subroutine dio_write_r1_integer4(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(inout) :: dfile
    character(*),intent(in) :: name
    integer(kind=integer4_kind),dimension(:),intent(in) :: data
    integer,dimension(:),optional,intent(in) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, n, nrec
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    if (present(header)) then
       myheader(1:512) = header(1:512)
    end if

    rank=1
    dtype=DIO_integer4
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    if (dfile%mode == 1) then
      if ( appendname(dfile,vname) <= 0 ) then
        write(0,*)" record ",trim(vname)," already exists"
        stop
      end if
    else
      nrec = readname(dfile,vname)
      if (nrec<=0) then
        write(0,*)" record ", trim(vname), " does not exist"
        stop
      end if
      call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    end if

    call c_fbwrite_record(dfile%flunit,vname,rank,dtype,4,bounds,data,myheader,dfile%swap,ios)

  end subroutine dio_write_r1_integer4


  subroutine dio_read_r1_integer4(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(in) :: dfile
    character(*),intent(in) :: name
    integer(kind=integer4_kind),dimension(:),intent(out) :: data
    integer,dimension(:),optional,intent(out) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, nrec, n, reclen
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    rank=1
    dtype=DIO_integer4
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    nrec = readname(dfile,vname)
    if (nrec<=0) then
      write(0,*)" record ", trim(vname), " does not exist"
      stop
    end if

    call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    call c_fbread_record(dfile%flunit,vname,rank,dtype,4,bounds,data,myheader,dfile%swap,ios)

    if (present(header)) then
       header(1:512) = myheader(1:512)
    end if

  end subroutine dio_read_r1_integer4


  subroutine dio_write_r2_integer4(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(inout) :: dfile
    character(*),intent(in) :: name
    integer(kind=integer4_kind),dimension(:,:),intent(in) :: data
    integer,dimension(:),optional,intent(in) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, n, nrec
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    if (present(header)) then
       myheader(1:512) = header(1:512)
    end if

    rank=2
    dtype=DIO_integer4
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    if (dfile%mode == 1) then
      if ( appendname(dfile,vname) <= 0 ) then
        write(0,*)" record ",trim(vname)," already exists"
        stop
      end if
    else
      nrec = readname(dfile,vname)
      if (nrec<=0) then
        write(0,*)" record ", trim(vname), " does not exist"
        stop
      end if
      call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    end if

    call c_fbwrite_record(dfile%flunit,vname,rank,dtype,4,bounds,data,myheader,dfile%swap,ios)

  end subroutine dio_write_r2_integer4


  subroutine dio_read_r2_integer4(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(in) :: dfile
    character(*),intent(in) :: name
    integer(kind=integer4_kind),dimension(:,:),intent(out) :: data
    integer,dimension(:),optional,intent(out) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, nrec, n, reclen
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    rank=2
    dtype=DIO_integer4
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    nrec = readname(dfile,vname)
    if (nrec<=0) then
      write(0,*)" record ", trim(vname), " does not exist"
      stop
    end if

    call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    call c_fbread_record(dfile%flunit,vname,rank,dtype,4,bounds,data,myheader,dfile%swap,ios)

    if (present(header)) then
       header(1:512) = myheader(1:512)
    end if

  end subroutine dio_read_r2_integer4


  subroutine dio_write_r3_integer4(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(inout) :: dfile
    character(*),intent(in) :: name
    integer(kind=integer4_kind),dimension(:,:,:),intent(in) :: data
    integer,dimension(:),optional,intent(in) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, n, nrec
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    if (present(header)) then
       myheader(1:512) = header(1:512)
    end if

    rank=3
    dtype=DIO_integer4
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    if (dfile%mode == 1) then
      if ( appendname(dfile,vname) <= 0 ) then
        write(0,*)" record ",trim(vname)," already exists"
        stop
      end if
    else
      nrec = readname(dfile,vname)
      if (nrec<=0) then
        write(0,*)" record ", trim(vname), " does not exist"
        stop
      end if
      call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    end if

    call c_fbwrite_record(dfile%flunit,vname,rank,dtype,4,bounds,data,myheader,dfile%swap,ios)

  end subroutine dio_write_r3_integer4


  subroutine dio_read_r3_integer4(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(in) :: dfile
    character(*),intent(in) :: name
    integer(kind=integer4_kind),dimension(:,:,:),intent(out) :: data
    integer,dimension(:),optional,intent(out) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, nrec, n, reclen
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    rank=3
    dtype=DIO_integer4
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    nrec = readname(dfile,vname)
    if (nrec<=0) then
      write(0,*)" record ", trim(vname), " does not exist"
      stop
    end if

    call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    call c_fbread_record(dfile%flunit,vname,rank,dtype,4,bounds,data,myheader,dfile%swap,ios)

    if (present(header)) then
       header(1:512) = myheader(1:512)
    end if

  end subroutine dio_read_r3_integer4


  subroutine dio_write_r4_integer4(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(inout) :: dfile
    character(*),intent(in) :: name
    integer(kind=integer4_kind),dimension(:,:,:,:),intent(in) :: data
    integer,dimension(:),optional,intent(in) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, n, nrec
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    if (present(header)) then
       myheader(1:512) = header(1:512)
    end if

    rank=4
    dtype=DIO_integer4
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    if (dfile%mode == 1) then
      if ( appendname(dfile,vname) <= 0 ) then
        write(0,*)" record ",trim(vname)," already exists"
        stop
      end if
    else
      nrec = readname(dfile,vname)
      if (nrec<=0) then
        write(0,*)" record ", trim(vname), " does not exist"
        stop
      end if
      call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    end if

    call c_fbwrite_record(dfile%flunit,vname,rank,dtype,4,bounds,data,myheader,dfile%swap,ios)

  end subroutine dio_write_r4_integer4


  subroutine dio_read_r4_integer4(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(in) :: dfile
    character(*),intent(in) :: name
    integer(kind=integer4_kind),dimension(:,:,:,:),intent(out) :: data
    integer,dimension(:),optional,intent(out) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, nrec, n, reclen
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    rank=4
    dtype=DIO_integer4
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    nrec = readname(dfile,vname)
    if (nrec<=0) then
      write(0,*)" record ", trim(vname), " does not exist"
      stop
    end if

    call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    call c_fbread_record(dfile%flunit,vname,rank,dtype,4,bounds,data,myheader,dfile%swap,ios)

    if (present(header)) then
       header(1:512) = myheader(1:512)
    end if

  end subroutine dio_read_r4_integer4


  subroutine dio_write_r0_integer8(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(inout) :: dfile
    character(*),intent(in) :: name
    integer(kind=integer8_kind),intent(in) :: data
    integer,dimension(:),optional,intent(in) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, n, nrec
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    if (present(header)) then
       myheader(1:512) = header(1:512)
    end if

    rank=0
    dtype=DIO_integer8
    allocate(bounds(2*rank))

    if (dfile%mode == 1) then
      if ( appendname(dfile,vname) <= 0 ) then
        write(0,*)" record ",trim(vname)," already exists"
        stop
      end if
    else
      nrec = readname(dfile,vname)
      if (nrec<=0) then
        write(0,*)" record ", trim(vname), " does not exist"
        stop
      end if
      call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    end if

    call c_fbwrite_record(dfile%flunit,vname,rank,dtype,8,bounds,data,myheader,dfile%swap,ios)

  end subroutine dio_write_r0_integer8


  subroutine dio_read_r0_integer8(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(in) :: dfile
    character(*),intent(in) :: name
    integer(kind=integer8_kind),intent(out) :: data
    integer,dimension(:),optional,intent(out) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, nrec, n, reclen
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    rank=0
    dtype=DIO_integer8
    allocate(bounds(2*rank))

    nrec = readname(dfile,vname)
    if (nrec<=0) then
      write(0,*)" record ", trim(vname), " does not exist"
      stop
    end if

    call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    call c_fbread_record(dfile%flunit,vname,rank,dtype,8,bounds,data,myheader,dfile%swap,ios)

    if (present(header)) then
       header(1:512) = myheader(1:512)
    end if

  end subroutine dio_read_r0_integer8


  subroutine dio_write_r1_integer8(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(inout) :: dfile
    character(*),intent(in) :: name
    integer(kind=integer8_kind),dimension(:),intent(in) :: data
    integer,dimension(:),optional,intent(in) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, n, nrec
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    if (present(header)) then
       myheader(1:512) = header(1:512)
    end if

    rank=1
    dtype=DIO_integer8
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    if (dfile%mode == 1) then
      if ( appendname(dfile,vname) <= 0 ) then
        write(0,*)" record ",trim(vname)," already exists"
        stop
      end if
    else
      nrec = readname(dfile,vname)
      if (nrec<=0) then
        write(0,*)" record ", trim(vname), " does not exist"
        stop
      end if
      call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    end if

    call c_fbwrite_record(dfile%flunit,vname,rank,dtype,8,bounds,data,myheader,dfile%swap,ios)

  end subroutine dio_write_r1_integer8


  subroutine dio_read_r1_integer8(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(in) :: dfile
    character(*),intent(in) :: name
    integer(kind=integer8_kind),dimension(:),intent(out) :: data
    integer,dimension(:),optional,intent(out) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, nrec, n, reclen
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    rank=1
    dtype=DIO_integer8
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    nrec = readname(dfile,vname)
    if (nrec<=0) then
      write(0,*)" record ", trim(vname), " does not exist"
      stop
    end if

    call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    call c_fbread_record(dfile%flunit,vname,rank,dtype,8,bounds,data,myheader,dfile%swap,ios)

    if (present(header)) then
       header(1:512) = myheader(1:512)
    end if

  end subroutine dio_read_r1_integer8


  subroutine dio_write_r2_integer8(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(inout) :: dfile
    character(*),intent(in) :: name
    integer(kind=integer8_kind),dimension(:,:),intent(in) :: data
    integer,dimension(:),optional,intent(in) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, n, nrec
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    if (present(header)) then
       myheader(1:512) = header(1:512)
    end if

    rank=2
    dtype=DIO_integer8
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    if (dfile%mode == 1) then
      if ( appendname(dfile,vname) <= 0 ) then
        write(0,*)" record ",trim(vname)," already exists"
        stop
      end if
    else
      nrec = readname(dfile,vname)
      if (nrec<=0) then
        write(0,*)" record ", trim(vname), " does not exist"
        stop
      end if
      call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    end if

    call c_fbwrite_record(dfile%flunit,vname,rank,dtype,8,bounds,data,myheader,dfile%swap,ios)

  end subroutine dio_write_r2_integer8


  subroutine dio_read_r2_integer8(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(in) :: dfile
    character(*),intent(in) :: name
    integer(kind=integer8_kind),dimension(:,:),intent(out) :: data
    integer,dimension(:),optional,intent(out) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, nrec, n, reclen
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    rank=2
    dtype=DIO_integer8
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    nrec = readname(dfile,vname)
    if (nrec<=0) then
      write(0,*)" record ", trim(vname), " does not exist"
      stop
    end if

    call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    call c_fbread_record(dfile%flunit,vname,rank,dtype,8,bounds,data,myheader,dfile%swap,ios)

    if (present(header)) then
       header(1:512) = myheader(1:512)
    end if

  end subroutine dio_read_r2_integer8


  subroutine dio_write_r3_integer8(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(inout) :: dfile
    character(*),intent(in) :: name
    integer(kind=integer8_kind),dimension(:,:,:),intent(in) :: data
    integer,dimension(:),optional,intent(in) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, n, nrec
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    if (present(header)) then
       myheader(1:512) = header(1:512)
    end if

    rank=3
    dtype=DIO_integer8
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    if (dfile%mode == 1) then
      if ( appendname(dfile,vname) <= 0 ) then
        write(0,*)" record ",trim(vname)," already exists"
        stop
      end if
    else
      nrec = readname(dfile,vname)
      if (nrec<=0) then
        write(0,*)" record ", trim(vname), " does not exist"
        stop
      end if
      call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    end if

    call c_fbwrite_record(dfile%flunit,vname,rank,dtype,8,bounds,data,myheader,dfile%swap,ios)

  end subroutine dio_write_r3_integer8


  subroutine dio_read_r3_integer8(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(in) :: dfile
    character(*),intent(in) :: name
    integer(kind=integer8_kind),dimension(:,:,:),intent(out) :: data
    integer,dimension(:),optional,intent(out) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, nrec, n, reclen
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    rank=3
    dtype=DIO_integer8
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    nrec = readname(dfile,vname)
    if (nrec<=0) then
      write(0,*)" record ", trim(vname), " does not exist"
      stop
    end if

    call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    call c_fbread_record(dfile%flunit,vname,rank,dtype,8,bounds,data,myheader,dfile%swap,ios)

    if (present(header)) then
       header(1:512) = myheader(1:512)
    end if

  end subroutine dio_read_r3_integer8


  subroutine dio_write_r4_integer8(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(inout) :: dfile
    character(*),intent(in) :: name
    integer(kind=integer8_kind),dimension(:,:,:,:),intent(in) :: data
    integer,dimension(:),optional,intent(in) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, n, nrec
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    if (present(header)) then
       myheader(1:512) = header(1:512)
    end if

    rank=4
    dtype=DIO_integer8
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    if (dfile%mode == 1) then
      if ( appendname(dfile,vname) <= 0 ) then
        write(0,*)" record ",trim(vname)," already exists"
        stop
      end if
    else
      nrec = readname(dfile,vname)
      if (nrec<=0) then
        write(0,*)" record ", trim(vname), " does not exist"
        stop
      end if
      call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    end if

    call c_fbwrite_record(dfile%flunit,vname,rank,dtype,8,bounds,data,myheader,dfile%swap,ios)

  end subroutine dio_write_r4_integer8


  subroutine dio_read_r4_integer8(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(in) :: dfile
    character(*),intent(in) :: name
    integer(kind=integer8_kind),dimension(:,:,:,:),intent(out) :: data
    integer,dimension(:),optional,intent(out) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, nrec, n, reclen
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    rank=4
    dtype=DIO_integer8
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    nrec = readname(dfile,vname)
    if (nrec<=0) then
      write(0,*)" record ", trim(vname), " does not exist"
      stop
    end if

    call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    call c_fbread_record(dfile%flunit,vname,rank,dtype,8,bounds,data,myheader,dfile%swap,ios)

    if (present(header)) then
       header(1:512) = myheader(1:512)
    end if

  end subroutine dio_read_r4_integer8


  subroutine dio_write_r0_real4(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(inout) :: dfile
    character(*),intent(in) :: name
    real(kind=real4_kind),intent(in) :: data
    integer,dimension(:),optional,intent(in) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, n, nrec
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    if (present(header)) then
       myheader(1:512) = header(1:512)
    end if

    rank=0
    dtype=DIO_real4
    allocate(bounds(2*rank))

    if (dfile%mode == 1) then
      if ( appendname(dfile,vname) <= 0 ) then
        write(0,*)" record ",trim(vname)," already exists"
        stop
      end if
    else
      nrec = readname(dfile,vname)
      if (nrec<=0) then
        write(0,*)" record ", trim(vname), " does not exist"
        stop
      end if
      call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    end if

    call c_fbwrite_record(dfile%flunit,vname,rank,dtype,4,bounds,data,myheader,dfile%swap,ios)

  end subroutine dio_write_r0_real4


  subroutine dio_read_r0_real4(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(in) :: dfile
    character(*),intent(in) :: name
    real(kind=real4_kind),intent(out) :: data
    integer,dimension(:),optional,intent(out) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, nrec, n, reclen
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    rank=0
    dtype=DIO_real4
    allocate(bounds(2*rank))

    nrec = readname(dfile,vname)
    if (nrec<=0) then
      write(0,*)" record ", trim(vname), " does not exist"
      stop
    end if

    call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    call c_fbread_record(dfile%flunit,vname,rank,dtype,4,bounds,data,myheader,dfile%swap,ios)

    if (present(header)) then
       header(1:512) = myheader(1:512)
    end if

  end subroutine dio_read_r0_real4


  subroutine dio_write_r1_real4(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(inout) :: dfile
    character(*),intent(in) :: name
    real(kind=real4_kind),dimension(:),intent(in) :: data
    integer,dimension(:),optional,intent(in) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, n, nrec
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    if (present(header)) then
       myheader(1:512) = header(1:512)
    end if

    rank=1
    dtype=DIO_real4
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    if (dfile%mode == 1) then
      if ( appendname(dfile,vname) <= 0 ) then
        write(0,*)" record ",trim(vname)," already exists"
        stop
      end if
    else
      nrec = readname(dfile,vname)
      if (nrec<=0) then
        write(0,*)" record ", trim(vname), " does not exist"
        stop
      end if
      call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    end if

    call c_fbwrite_record(dfile%flunit,vname,rank,dtype,4,bounds,data,myheader,dfile%swap,ios)

  end subroutine dio_write_r1_real4


  subroutine dio_read_r1_real4(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(in) :: dfile
    character(*),intent(in) :: name
    real(kind=real4_kind),dimension(:),intent(out) :: data
    integer,dimension(:),optional,intent(out) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, nrec, n, reclen
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    rank=1
    dtype=DIO_real4
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    nrec = readname(dfile,vname)
    if (nrec<=0) then
      write(0,*)" record ", trim(vname), " does not exist"
      stop
    end if

    call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    call c_fbread_record(dfile%flunit,vname,rank,dtype,4,bounds,data,myheader,dfile%swap,ios)

    if (present(header)) then
       header(1:512) = myheader(1:512)
    end if

  end subroutine dio_read_r1_real4


  subroutine dio_write_r2_real4(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(inout) :: dfile
    character(*),intent(in) :: name
    real(kind=real4_kind),dimension(:,:),intent(in) :: data
    integer,dimension(:),optional,intent(in) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, n, nrec
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    if (present(header)) then
       myheader(1:512) = header(1:512)
    end if

    rank=2
    dtype=DIO_real4
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    if (dfile%mode == 1) then
      if ( appendname(dfile,vname) <= 0 ) then
        write(0,*)" record ",trim(vname)," already exists"
        stop
      end if
    else
      nrec = readname(dfile,vname)
      if (nrec<=0) then
        write(0,*)" record ", trim(vname), " does not exist"
        stop
      end if
      call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    end if

    call c_fbwrite_record(dfile%flunit,vname,rank,dtype,4,bounds,data,myheader,dfile%swap,ios)

  end subroutine dio_write_r2_real4


  subroutine dio_read_r2_real4(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(in) :: dfile
    character(*),intent(in) :: name
    real(kind=real4_kind),dimension(:,:),intent(out) :: data
    integer,dimension(:),optional,intent(out) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, nrec, n, reclen
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    rank=2
    dtype=DIO_real4
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    nrec = readname(dfile,vname)
    if (nrec<=0) then
      write(0,*)" record ", trim(vname), " does not exist"
      stop
    end if

    call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    call c_fbread_record(dfile%flunit,vname,rank,dtype,4,bounds,data,myheader,dfile%swap,ios)

    if (present(header)) then
       header(1:512) = myheader(1:512)
    end if

  end subroutine dio_read_r2_real4


  subroutine dio_write_r3_real4(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(inout) :: dfile
    character(*),intent(in) :: name
    real(kind=real4_kind),dimension(:,:,:),intent(in) :: data
    integer,dimension(:),optional,intent(in) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, n, nrec
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    if (present(header)) then
       myheader(1:512) = header(1:512)
    end if

    rank=3
    dtype=DIO_real4
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    if (dfile%mode == 1) then
      if ( appendname(dfile,vname) <= 0 ) then
        write(0,*)" record ",trim(vname)," already exists"
        stop
      end if
    else
      nrec = readname(dfile,vname)
      if (nrec<=0) then
        write(0,*)" record ", trim(vname), " does not exist"
        stop
      end if
      call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    end if

    call c_fbwrite_record(dfile%flunit,vname,rank,dtype,4,bounds,data,myheader,dfile%swap,ios)

  end subroutine dio_write_r3_real4


  subroutine dio_read_r3_real4(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(in) :: dfile
    character(*),intent(in) :: name
    real(kind=real4_kind),dimension(:,:,:),intent(out) :: data
    integer,dimension(:),optional,intent(out) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, nrec, n, reclen
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    rank=3
    dtype=DIO_real4
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    nrec = readname(dfile,vname)
    if (nrec<=0) then
      write(0,*)" record ", trim(vname), " does not exist"
      stop
    end if

    call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    call c_fbread_record(dfile%flunit,vname,rank,dtype,4,bounds,data,myheader,dfile%swap,ios)

    if (present(header)) then
       header(1:512) = myheader(1:512)
    end if

  end subroutine dio_read_r3_real4


  subroutine dio_write_r4_real4(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(inout) :: dfile
    character(*),intent(in) :: name
    real(kind=real4_kind),dimension(:,:,:,:),intent(in) :: data
    integer,dimension(:),optional,intent(in) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, n, nrec
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    if (present(header)) then
       myheader(1:512) = header(1:512)
    end if

    rank=4
    dtype=DIO_real4
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    if (dfile%mode == 1) then
      if ( appendname(dfile,vname) <= 0 ) then
        write(0,*)" record ",trim(vname)," already exists"
        stop
      end if
    else
      nrec = readname(dfile,vname)
      if (nrec<=0) then
        write(0,*)" record ", trim(vname), " does not exist"
        stop
      end if
      call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    end if

    call c_fbwrite_record(dfile%flunit,vname,rank,dtype,4,bounds,data,myheader,dfile%swap,ios)

  end subroutine dio_write_r4_real4


  subroutine dio_read_r4_real4(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(in) :: dfile
    character(*),intent(in) :: name
    real(kind=real4_kind),dimension(:,:,:,:),intent(out) :: data
    integer,dimension(:),optional,intent(out) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, nrec, n, reclen
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    rank=4
    dtype=DIO_real4
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    nrec = readname(dfile,vname)
    if (nrec<=0) then
      write(0,*)" record ", trim(vname), " does not exist"
      stop
    end if

    call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    call c_fbread_record(dfile%flunit,vname,rank,dtype,4,bounds,data,myheader,dfile%swap,ios)

    if (present(header)) then
       header(1:512) = myheader(1:512)
    end if

  end subroutine dio_read_r4_real4


  subroutine dio_write_r0_real8(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(inout) :: dfile
    character(*),intent(in) :: name
    real(kind=real8_kind),intent(in) :: data
    integer,dimension(:),optional,intent(in) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, n, nrec
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    if (present(header)) then
       myheader(1:512) = header(1:512)
    end if

    rank=0
    dtype=DIO_real8
    allocate(bounds(2*rank))

    if (dfile%mode == 1) then
      if ( appendname(dfile,vname) <= 0 ) then
        write(0,*)" record ",trim(vname)," already exists"
        stop
      end if
    else
      nrec = readname(dfile,vname)
      if (nrec<=0) then
        write(0,*)" record ", trim(vname), " does not exist"
        stop
      end if
      call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    end if

    call c_fbwrite_record(dfile%flunit,vname,rank,dtype,8,bounds,data,myheader,dfile%swap,ios)

  end subroutine dio_write_r0_real8


  subroutine dio_read_r0_real8(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(in) :: dfile
    character(*),intent(in) :: name
    real(kind=real8_kind),intent(out) :: data
    integer,dimension(:),optional,intent(out) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, nrec, n, reclen
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    rank=0
    dtype=DIO_real8
    allocate(bounds(2*rank))

    nrec = readname(dfile,vname)
    if (nrec<=0) then
      write(0,*)" record ", trim(vname), " does not exist"
      stop
    end if

    call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    call c_fbread_record(dfile%flunit,vname,rank,dtype,8,bounds,data,myheader,dfile%swap,ios)

    if (present(header)) then
       header(1:512) = myheader(1:512)
    end if

  end subroutine dio_read_r0_real8


  subroutine dio_write_r1_real8(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(inout) :: dfile
    character(*),intent(in) :: name
    real(kind=real8_kind),dimension(:),intent(in) :: data
    integer,dimension(:),optional,intent(in) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, n, nrec
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    if (present(header)) then
       myheader(1:512) = header(1:512)
    end if

    rank=1
    dtype=DIO_real8
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    if (dfile%mode == 1) then
      if ( appendname(dfile,vname) <= 0 ) then
        write(0,*)" record ",trim(vname)," already exists"
        stop
      end if
    else
      nrec = readname(dfile,vname)
      if (nrec<=0) then
        write(0,*)" record ", trim(vname), " does not exist"
        stop
      end if
      call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    end if

    call c_fbwrite_record(dfile%flunit,vname,rank,dtype,8,bounds,data,myheader,dfile%swap,ios)

  end subroutine dio_write_r1_real8


  subroutine dio_read_r1_real8(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(in) :: dfile
    character(*),intent(in) :: name
    real(kind=real8_kind),dimension(:),intent(out) :: data
    integer,dimension(:),optional,intent(out) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, nrec, n, reclen
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    rank=1
    dtype=DIO_real8
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    nrec = readname(dfile,vname)
    if (nrec<=0) then
      write(0,*)" record ", trim(vname), " does not exist"
      stop
    end if

    call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    call c_fbread_record(dfile%flunit,vname,rank,dtype,8,bounds,data,myheader,dfile%swap,ios)

    if (present(header)) then
       header(1:512) = myheader(1:512)
    end if

  end subroutine dio_read_r1_real8


  subroutine dio_write_r2_real8(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(inout) :: dfile
    character(*),intent(in) :: name
    real(kind=real8_kind),dimension(:,:),intent(in) :: data
    integer,dimension(:),optional,intent(in) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, n, nrec
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    if (present(header)) then
       myheader(1:512) = header(1:512)
    end if

    rank=2
    dtype=DIO_real8
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    if (dfile%mode == 1) then
      if ( appendname(dfile,vname) <= 0 ) then
        write(0,*)" record ",trim(vname)," already exists"
        stop
      end if
    else
      nrec = readname(dfile,vname)
      if (nrec<=0) then
        write(0,*)" record ", trim(vname), " does not exist"
        stop
      end if
      call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    end if

    call c_fbwrite_record(dfile%flunit,vname,rank,dtype,8,bounds,data,myheader,dfile%swap,ios)

  end subroutine dio_write_r2_real8


  subroutine dio_read_r2_real8(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(in) :: dfile
    character(*),intent(in) :: name
    real(kind=real8_kind),dimension(:,:),intent(out) :: data
    integer,dimension(:),optional,intent(out) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, nrec, n, reclen
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    rank=2
    dtype=DIO_real8
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    nrec = readname(dfile,vname)
    if (nrec<=0) then
      write(0,*)" record ", trim(vname), " does not exist"
      stop
    end if

    call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    call c_fbread_record(dfile%flunit,vname,rank,dtype,8,bounds,data,myheader,dfile%swap,ios)

    if (present(header)) then
       header(1:512) = myheader(1:512)
    end if

  end subroutine dio_read_r2_real8


  subroutine dio_write_r3_real8(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(inout) :: dfile
    character(*),intent(in) :: name
    real(kind=real8_kind),dimension(:,:,:),intent(in) :: data
    integer,dimension(:),optional,intent(in) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, n, nrec
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    if (present(header)) then
       myheader(1:512) = header(1:512)
    end if

    rank=3
    dtype=DIO_real8
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    if (dfile%mode == 1) then
      if ( appendname(dfile,vname) <= 0 ) then
        write(0,*)" record ",trim(vname)," already exists"
        stop
      end if
    else
      nrec = readname(dfile,vname)
      if (nrec<=0) then
        write(0,*)" record ", trim(vname), " does not exist"
        stop
      end if
      call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    end if

    call c_fbwrite_record(dfile%flunit,vname,rank,dtype,8,bounds,data,myheader,dfile%swap,ios)

  end subroutine dio_write_r3_real8


  subroutine dio_read_r3_real8(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(in) :: dfile
    character(*),intent(in) :: name
    real(kind=real8_kind),dimension(:,:,:),intent(out) :: data
    integer,dimension(:),optional,intent(out) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, nrec, n, reclen
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    rank=3
    dtype=DIO_real8
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    nrec = readname(dfile,vname)
    if (nrec<=0) then
      write(0,*)" record ", trim(vname), " does not exist"
      stop
    end if

    call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    call c_fbread_record(dfile%flunit,vname,rank,dtype,8,bounds,data,myheader,dfile%swap,ios)

    if (present(header)) then
       header(1:512) = myheader(1:512)
    end if

  end subroutine dio_read_r3_real8


  subroutine dio_write_r4_real8(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(inout) :: dfile
    character(*),intent(in) :: name
    real(kind=real8_kind),dimension(:,:,:,:),intent(in) :: data
    integer,dimension(:),optional,intent(in) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, n, nrec
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    if (present(header)) then
       myheader(1:512) = header(1:512)
    end if

    rank=4
    dtype=DIO_real8
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    if (dfile%mode == 1) then
      if ( appendname(dfile,vname) <= 0 ) then
        write(0,*)" record ",trim(vname)," already exists"
        stop
      end if
    else
      nrec = readname(dfile,vname)
      if (nrec<=0) then
        write(0,*)" record ", trim(vname), " does not exist"
        stop
      end if
      call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    end if

    call c_fbwrite_record(dfile%flunit,vname,rank,dtype,8,bounds,data,myheader,dfile%swap,ios)

  end subroutine dio_write_r4_real8


  subroutine dio_read_r4_real8(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(in) :: dfile
    character(*),intent(in) :: name
    real(kind=real8_kind),dimension(:,:,:,:),intent(out) :: data
    integer,dimension(:),optional,intent(out) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, nrec, n, reclen
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    rank=4
    dtype=DIO_real8
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    nrec = readname(dfile,vname)
    if (nrec<=0) then
      write(0,*)" record ", trim(vname), " does not exist"
      stop
    end if

    call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    call c_fbread_record(dfile%flunit,vname,rank,dtype,8,bounds,data,myheader,dfile%swap,ios)

    if (present(header)) then
       header(1:512) = myheader(1:512)
    end if

  end subroutine dio_read_r4_real8


  subroutine dio_write_r0_logical1(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(inout) :: dfile
    character(*),intent(in) :: name
    logical(kind=logical1_kind),intent(in) :: data
    integer,dimension(:),optional,intent(in) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, n, nrec
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    if (present(header)) then
       myheader(1:512) = header(1:512)
    end if

    rank=0
    dtype=DIO_logical1
    allocate(bounds(2*rank))

    if (dfile%mode == 1) then
      if ( appendname(dfile,vname) <= 0 ) then
        write(0,*)" record ",trim(vname)," already exists"
        stop
      end if
    else
      nrec = readname(dfile,vname)
      if (nrec<=0) then
        write(0,*)" record ", trim(vname), " does not exist"
        stop
      end if
      call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    end if

    call c_fbwrite_record(dfile%flunit,vname,rank,dtype,1,bounds,data,myheader,dfile%swap,ios)

  end subroutine dio_write_r0_logical1


  subroutine dio_read_r0_logical1(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(in) :: dfile
    character(*),intent(in) :: name
    logical(kind=logical1_kind),intent(out) :: data
    integer,dimension(:),optional,intent(out) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, nrec, n, reclen
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    rank=0
    dtype=DIO_logical1
    allocate(bounds(2*rank))

    nrec = readname(dfile,vname)
    if (nrec<=0) then
      write(0,*)" record ", trim(vname), " does not exist"
      stop
    end if

    call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    call c_fbread_record(dfile%flunit,vname,rank,dtype,1,bounds,data,myheader,dfile%swap,ios)

    if (present(header)) then
       header(1:512) = myheader(1:512)
    end if

  end subroutine dio_read_r0_logical1


  subroutine dio_write_r1_logical1(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(inout) :: dfile
    character(*),intent(in) :: name
    logical(kind=logical1_kind),dimension(:),intent(in) :: data
    integer,dimension(:),optional,intent(in) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, n, nrec
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    if (present(header)) then
       myheader(1:512) = header(1:512)
    end if

    rank=1
    dtype=DIO_logical1
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    if (dfile%mode == 1) then
      if ( appendname(dfile,vname) <= 0 ) then
        write(0,*)" record ",trim(vname)," already exists"
        stop
      end if
    else
      nrec = readname(dfile,vname)
      if (nrec<=0) then
        write(0,*)" record ", trim(vname), " does not exist"
        stop
      end if
      call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    end if

    call c_fbwrite_record(dfile%flunit,vname,rank,dtype,1,bounds,data,myheader,dfile%swap,ios)

  end subroutine dio_write_r1_logical1


  subroutine dio_read_r1_logical1(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(in) :: dfile
    character(*),intent(in) :: name
    logical(kind=logical1_kind),dimension(:),intent(out) :: data
    integer,dimension(:),optional,intent(out) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, nrec, n, reclen
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    rank=1
    dtype=DIO_logical1
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    nrec = readname(dfile,vname)
    if (nrec<=0) then
      write(0,*)" record ", trim(vname), " does not exist"
      stop
    end if

    call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    call c_fbread_record(dfile%flunit,vname,rank,dtype,1,bounds,data,myheader,dfile%swap,ios)

    if (present(header)) then
       header(1:512) = myheader(1:512)
    end if

  end subroutine dio_read_r1_logical1


  subroutine dio_write_r2_logical1(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(inout) :: dfile
    character(*),intent(in) :: name
    logical(kind=logical1_kind),dimension(:,:),intent(in) :: data
    integer,dimension(:),optional,intent(in) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, n, nrec
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    if (present(header)) then
       myheader(1:512) = header(1:512)
    end if

    rank=2
    dtype=DIO_logical1
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    if (dfile%mode == 1) then
      if ( appendname(dfile,vname) <= 0 ) then
        write(0,*)" record ",trim(vname)," already exists"
        stop
      end if
    else
      nrec = readname(dfile,vname)
      if (nrec<=0) then
        write(0,*)" record ", trim(vname), " does not exist"
        stop
      end if
      call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    end if

    call c_fbwrite_record(dfile%flunit,vname,rank,dtype,1,bounds,data,myheader,dfile%swap,ios)

  end subroutine dio_write_r2_logical1


  subroutine dio_read_r2_logical1(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(in) :: dfile
    character(*),intent(in) :: name
    logical(kind=logical1_kind),dimension(:,:),intent(out) :: data
    integer,dimension(:),optional,intent(out) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, nrec, n, reclen
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    rank=2
    dtype=DIO_logical1
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    nrec = readname(dfile,vname)
    if (nrec<=0) then
      write(0,*)" record ", trim(vname), " does not exist"
      stop
    end if

    call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    call c_fbread_record(dfile%flunit,vname,rank,dtype,1,bounds,data,myheader,dfile%swap,ios)

    if (present(header)) then
       header(1:512) = myheader(1:512)
    end if

  end subroutine dio_read_r2_logical1


  subroutine dio_write_r3_logical1(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(inout) :: dfile
    character(*),intent(in) :: name
    logical(kind=logical1_kind),dimension(:,:,:),intent(in) :: data
    integer,dimension(:),optional,intent(in) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, n, nrec
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    if (present(header)) then
       myheader(1:512) = header(1:512)
    end if

    rank=3
    dtype=DIO_logical1
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    if (dfile%mode == 1) then
      if ( appendname(dfile,vname) <= 0 ) then
        write(0,*)" record ",trim(vname)," already exists"
        stop
      end if
    else
      nrec = readname(dfile,vname)
      if (nrec<=0) then
        write(0,*)" record ", trim(vname), " does not exist"
        stop
      end if
      call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    end if

    call c_fbwrite_record(dfile%flunit,vname,rank,dtype,1,bounds,data,myheader,dfile%swap,ios)

  end subroutine dio_write_r3_logical1


  subroutine dio_read_r3_logical1(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(in) :: dfile
    character(*),intent(in) :: name
    logical(kind=logical1_kind),dimension(:,:,:),intent(out) :: data
    integer,dimension(:),optional,intent(out) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, nrec, n, reclen
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    rank=3
    dtype=DIO_logical1
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    nrec = readname(dfile,vname)
    if (nrec<=0) then
      write(0,*)" record ", trim(vname), " does not exist"
      stop
    end if

    call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    call c_fbread_record(dfile%flunit,vname,rank,dtype,1,bounds,data,myheader,dfile%swap,ios)

    if (present(header)) then
       header(1:512) = myheader(1:512)
    end if

  end subroutine dio_read_r3_logical1


  subroutine dio_write_r4_logical1(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(inout) :: dfile
    character(*),intent(in) :: name
    logical(kind=logical1_kind),dimension(:,:,:,:),intent(in) :: data
    integer,dimension(:),optional,intent(in) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, n, nrec
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    if (present(header)) then
       myheader(1:512) = header(1:512)
    end if

    rank=4
    dtype=DIO_logical1
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    if (dfile%mode == 1) then
      if ( appendname(dfile,vname) <= 0 ) then
        write(0,*)" record ",trim(vname)," already exists"
        stop
      end if
    else
      nrec = readname(dfile,vname)
      if (nrec<=0) then
        write(0,*)" record ", trim(vname), " does not exist"
        stop
      end if
      call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    end if

    call c_fbwrite_record(dfile%flunit,vname,rank,dtype,1,bounds,data,myheader,dfile%swap,ios)

  end subroutine dio_write_r4_logical1


  subroutine dio_read_r4_logical1(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(in) :: dfile
    character(*),intent(in) :: name
    logical(kind=logical1_kind),dimension(:,:,:,:),intent(out) :: data
    integer,dimension(:),optional,intent(out) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, nrec, n, reclen
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    rank=4
    dtype=DIO_logical1
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    nrec = readname(dfile,vname)
    if (nrec<=0) then
      write(0,*)" record ", trim(vname), " does not exist"
      stop
    end if

    call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    call c_fbread_record(dfile%flunit,vname,rank,dtype,1,bounds,data,myheader,dfile%swap,ios)

    if (present(header)) then
       header(1:512) = myheader(1:512)
    end if

  end subroutine dio_read_r4_logical1


  subroutine dio_write_r0_logical4(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(inout) :: dfile
    character(*),intent(in) :: name
    logical(kind=logical4_kind),intent(in) :: data
    integer,dimension(:),optional,intent(in) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, n, nrec
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    if (present(header)) then
       myheader(1:512) = header(1:512)
    end if

    rank=0
    dtype=DIO_logical4
    allocate(bounds(2*rank))

    if (dfile%mode == 1) then
      if ( appendname(dfile,vname) <= 0 ) then
        write(0,*)" record ",trim(vname)," already exists"
        stop
      end if
    else
      nrec = readname(dfile,vname)
      if (nrec<=0) then
        write(0,*)" record ", trim(vname), " does not exist"
        stop
      end if
      call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    end if

    call c_fbwrite_record(dfile%flunit,vname,rank,dtype,4,bounds,data,myheader,dfile%swap,ios)

  end subroutine dio_write_r0_logical4


  subroutine dio_read_r0_logical4(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(in) :: dfile
    character(*),intent(in) :: name
    logical(kind=logical4_kind),intent(out) :: data
    integer,dimension(:),optional,intent(out) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, nrec, n, reclen
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    rank=0
    dtype=DIO_logical4
    allocate(bounds(2*rank))

    nrec = readname(dfile,vname)
    if (nrec<=0) then
      write(0,*)" record ", trim(vname), " does not exist"
      stop
    end if

    call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    call c_fbread_record(dfile%flunit,vname,rank,dtype,4,bounds,data,myheader,dfile%swap,ios)

    if (present(header)) then
       header(1:512) = myheader(1:512)
    end if

  end subroutine dio_read_r0_logical4


  subroutine dio_write_r1_logical4(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(inout) :: dfile
    character(*),intent(in) :: name
    logical(kind=logical4_kind),dimension(:),intent(in) :: data
    integer,dimension(:),optional,intent(in) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, n, nrec
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    if (present(header)) then
       myheader(1:512) = header(1:512)
    end if

    rank=1
    dtype=DIO_logical4
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    if (dfile%mode == 1) then
      if ( appendname(dfile,vname) <= 0 ) then
        write(0,*)" record ",trim(vname)," already exists"
        stop
      end if
    else
      nrec = readname(dfile,vname)
      if (nrec<=0) then
        write(0,*)" record ", trim(vname), " does not exist"
        stop
      end if
      call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    end if

    call c_fbwrite_record(dfile%flunit,vname,rank,dtype,4,bounds,data,myheader,dfile%swap,ios)

  end subroutine dio_write_r1_logical4


  subroutine dio_read_r1_logical4(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(in) :: dfile
    character(*),intent(in) :: name
    logical(kind=logical4_kind),dimension(:),intent(out) :: data
    integer,dimension(:),optional,intent(out) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, nrec, n, reclen
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    rank=1
    dtype=DIO_logical4
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    nrec = readname(dfile,vname)
    if (nrec<=0) then
      write(0,*)" record ", trim(vname), " does not exist"
      stop
    end if

    call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    call c_fbread_record(dfile%flunit,vname,rank,dtype,4,bounds,data,myheader,dfile%swap,ios)

    if (present(header)) then
       header(1:512) = myheader(1:512)
    end if

  end subroutine dio_read_r1_logical4


  subroutine dio_write_r2_logical4(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(inout) :: dfile
    character(*),intent(in) :: name
    logical(kind=logical4_kind),dimension(:,:),intent(in) :: data
    integer,dimension(:),optional,intent(in) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, n, nrec
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    if (present(header)) then
       myheader(1:512) = header(1:512)
    end if

    rank=2
    dtype=DIO_logical4
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    if (dfile%mode == 1) then
      if ( appendname(dfile,vname) <= 0 ) then
        write(0,*)" record ",trim(vname)," already exists"
        stop
      end if
    else
      nrec = readname(dfile,vname)
      if (nrec<=0) then
        write(0,*)" record ", trim(vname), " does not exist"
        stop
      end if
      call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    end if

    call c_fbwrite_record(dfile%flunit,vname,rank,dtype,4,bounds,data,myheader,dfile%swap,ios)

  end subroutine dio_write_r2_logical4


  subroutine dio_read_r2_logical4(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(in) :: dfile
    character(*),intent(in) :: name
    logical(kind=logical4_kind),dimension(:,:),intent(out) :: data
    integer,dimension(:),optional,intent(out) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, nrec, n, reclen
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    rank=2
    dtype=DIO_logical4
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    nrec = readname(dfile,vname)
    if (nrec<=0) then
      write(0,*)" record ", trim(vname), " does not exist"
      stop
    end if

    call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    call c_fbread_record(dfile%flunit,vname,rank,dtype,4,bounds,data,myheader,dfile%swap,ios)

    if (present(header)) then
       header(1:512) = myheader(1:512)
    end if

  end subroutine dio_read_r2_logical4


  subroutine dio_write_r3_logical4(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(inout) :: dfile
    character(*),intent(in) :: name
    logical(kind=logical4_kind),dimension(:,:,:),intent(in) :: data
    integer,dimension(:),optional,intent(in) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, n, nrec
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    if (present(header)) then
       myheader(1:512) = header(1:512)
    end if

    rank=3
    dtype=DIO_logical4
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    if (dfile%mode == 1) then
      if ( appendname(dfile,vname) <= 0 ) then
        write(0,*)" record ",trim(vname)," already exists"
        stop
      end if
    else
      nrec = readname(dfile,vname)
      if (nrec<=0) then
        write(0,*)" record ", trim(vname), " does not exist"
        stop
      end if
      call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    end if

    call c_fbwrite_record(dfile%flunit,vname,rank,dtype,4,bounds,data,myheader,dfile%swap,ios)

  end subroutine dio_write_r3_logical4


  subroutine dio_read_r3_logical4(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(in) :: dfile
    character(*),intent(in) :: name
    logical(kind=logical4_kind),dimension(:,:,:),intent(out) :: data
    integer,dimension(:),optional,intent(out) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, nrec, n, reclen
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    rank=3
    dtype=DIO_logical4
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    nrec = readname(dfile,vname)
    if (nrec<=0) then
      write(0,*)" record ", trim(vname), " does not exist"
      stop
    end if

    call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    call c_fbread_record(dfile%flunit,vname,rank,dtype,4,bounds,data,myheader,dfile%swap,ios)

    if (present(header)) then
       header(1:512) = myheader(1:512)
    end if

  end subroutine dio_read_r3_logical4


  subroutine dio_write_r4_logical4(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(inout) :: dfile
    character(*),intent(in) :: name
    logical(kind=logical4_kind),dimension(:,:,:,:),intent(in) :: data
    integer,dimension(:),optional,intent(in) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, n, nrec
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    if (present(header)) then
       myheader(1:512) = header(1:512)
    end if

    rank=4
    dtype=DIO_logical4
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    if (dfile%mode == 1) then
      if ( appendname(dfile,vname) <= 0 ) then
        write(0,*)" record ",trim(vname)," already exists"
        stop
      end if
    else
      nrec = readname(dfile,vname)
      if (nrec<=0) then
        write(0,*)" record ", trim(vname), " does not exist"
        stop
      end if
      call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    end if

    call c_fbwrite_record(dfile%flunit,vname,rank,dtype,4,bounds,data,myheader,dfile%swap,ios)

  end subroutine dio_write_r4_logical4


  subroutine dio_read_r4_logical4(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(in) :: dfile
    character(*),intent(in) :: name
    logical(kind=logical4_kind),dimension(:,:,:,:),intent(out) :: data
    integer,dimension(:),optional,intent(out) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, nrec, n, reclen
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    rank=4
    dtype=DIO_logical4
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = lbound(data,dim=n)
       bounds( (n-1)*2 + 2 ) = ubound(data,dim=n)
    end do

    nrec = readname(dfile,vname)
    if (nrec<=0) then
      write(0,*)" record ", trim(vname), " does not exist"
      stop
    end if

    call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    call c_fbread_record(dfile%flunit,vname,rank,dtype,4,bounds,data,myheader,dfile%swap,ios)

    if (present(header)) then
       header(1:512) = myheader(1:512)
    end if

  end subroutine dio_read_r4_logical4

  subroutine dio_write0c(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(inout) :: dfile
    character(*),intent(in) :: name
    character(len=*),intent(in) :: data
    integer,dimension(:),optional,intent(in) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, n, nrec
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    if (present(header)) then
       myheader(1:512) = header(1:512)
    end if

    rank=1
    dtype=DIO_CHARACTER
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = 1
       bounds( (n-1)*2 + 2 ) = len(data)
    end do

    if (dfile%mode == 1) then
      if ( appendname(dfile,vname) <= 0 ) then
        write(0,*)" record ",trim(vname)," already exists"
        stop
      end if
    else
      nrec = readname(dfile,vname)
      if (nrec<=0) then
        write(0,*)" record ", trim(vname), " does not exist"
        stop
      end if
      call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)
    end if

    call c_fbwrite_record(dfile%flunit,vname,rank,dtype,1,bounds,data,myheader,dfile%swap,ios)

  end subroutine dio_write0c

  subroutine dio_read0c(dfile,name,data,header,iret)
    implicit none
    type(dio_file),intent(in) :: dfile
    character(*),intent(in) :: name
    character(len=*),intent(out) :: data
    integer,dimension(:),optional,intent(out) :: header
    integer,optional,intent(out) :: iret

    integer :: ios, nrec, n, reclen
    integer :: rank,dtype
    character(len=32) :: vname
    integer,dimension(:),allocatable :: bounds
    integer,dimension(512) :: myheader = 0

    integer(kind=integer1_kind), dimension(:), allocatable :: istring

    if ( present(iret) ) iret=0

    vname = "                                "
    vname(1:len(trim(name))) = name(1:len(trim(name)))

    nrec = readname(dfile,vname)
    if (nrec<=0) then
      write(0,*)" record ", trim(vname), " does not exist"
      stop
    end if

    rank=1
    dtype=DIO_CHARACTER
    allocate(bounds(2*rank))
    do n=1,rank
       bounds( (n-1)*2 + 1 ) = 1
       bounds( (n-1)*2 + 2 ) = dfile%records(nrec)%bounds(2)
    end do

    do n=1,len(data)
      data(n:n)= " "
    end do

    call c_fbseekset(dfile%flunit,dfile%records(nrec)%offset,ios)

    if (len(data) >= bounds(2)) then
       call c_fbread_record(dfile%flunit,vname,rank,dtype,1,bounds,data,myheader,dfile%swap,ios)
    else 
       allocate(istring(bounds(2)))
       call c_fbread_record(dfile%flunit,vname,rank,dtype,1,bounds,istring,myheader,dfile%swap,ios)
       do n=1,len(data)
          data(n:n)= char(istring(n))
       end do
       deallocate(istring)
    end if

    if (present(header)) then
       header(1:512) = myheader(1:512)
    end if


  end subroutine dio_read0c

  integer function dio_numrec(dfile)
    implicit none
    type(dio_file),intent (in) :: dfile

    dio_numrec = dfile%nrec
  end function dio_numrec

  subroutine dio_recinfo(dfile,nrec,name,rank,dtype,bounds,iret)
    implicit none
    type(dio_file),intent (in) :: dfile
    integer, intent(in) :: nrec
    character(*), intent(out) :: name
    integer, intent(out) :: rank
    integer, intent(out) :: dtype
    integer,dimension(:), intent(out) :: bounds
    integer,optional,intent(out) :: iret

    iret = 0
    if (nrec<=dfile%nrec) then
      name = dfile%records(nrec)%name
      rank = dfile%records(nrec)%rank
      dtype = dfile%records(nrec)%dtype
      bounds = dfile%records(nrec)%bounds
    else
      iret =-1
    end if

  end subroutine dio_recinfo


  integer function readname(dfile,name)
    implicit none
    type(dio_file),intent (in) :: dfile
    character(*), intent(in) :: name

    integer :: n

    readname = 0

    do n=1,MAXREC
!       write(0,*) n," |",trim(name),"|  |",trim(dfile%records(n)%name),"|"
       if (trim(name) == trim(dfile%records(n)%name)) then
          readname = n
          return
       end if
    end do

  end function readname

  integer function appendname(dfile,name)
    implicit none
    type(dio_file),intent (inout) :: dfile
    character(*), intent(in) :: name

    integer :: n
    integer :: ios

    appendname = 0

    do n=1,MAXREC
       if (trim(name) == trim(dfile%records(n)%name)) then
          return
       end if
       if (dfile%records(n)%name == "                                " ) then
          dfile%records(n)%name = trim(name)
          call c_fbtell(dfile%flunit,dfile%records(n)%offset,ios)
          appendname = n
          return
       end if
    end do

  end function appendname

end module dio
