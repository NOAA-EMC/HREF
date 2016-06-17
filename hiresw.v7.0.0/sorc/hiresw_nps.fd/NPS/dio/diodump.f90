! Dusan Jovic, NCEP, 2008

program diodump

  use dio
  implicit none

  type(dio_file) :: rfile

  integer :: iret
  integer :: n
  character(len=32) :: name
  integer :: rank, dtype
  integer,dimension(10) :: bounds
  integer, dimension(512) :: header

  character(len=1024*1024) :: v1c
  real(kind=real4_kind) :: vr4_0
  real(kind=real4_kind), dimension(:), allocatable :: vr4_1
  real(kind=real4_kind), dimension(:,:), allocatable :: vr4_2
  real(kind=real4_kind), dimension(:,:,:), allocatable :: vr4_3
  real(kind=real4_kind), dimension(:,:,:,:), allocatable :: vr4_4
  real(kind=real8_kind) :: vr8_0
  real(kind=real8_kind), dimension(:), allocatable :: vr8_1
  real(kind=real8_kind), dimension(:,:), allocatable :: vr8_2
  real(kind=real8_kind), dimension(:,:,:), allocatable :: vr8_3
  real(kind=real8_kind), dimension(:,:,:,:), allocatable :: vr8_4

  integer(kind=integer4_kind) :: vi4_0
  integer(kind=integer4_kind), dimension(:), allocatable :: vi4_1
  integer(kind=integer4_kind), dimension(:,:), allocatable :: vi4_2
  integer(kind=integer4_kind), dimension(:,:,:), allocatable :: vi4_3
  integer(kind=integer4_kind), dimension(:,:,:,:), allocatable :: vi4_4

  logical :: v0l
  logical, dimension(:), allocatable :: v1l
  logical, dimension(:,:), allocatable :: v2l
  logical, dimension(:,:,:), allocatable :: v3l

  character(len=256) :: fname
  character(len=256) :: carg
  character(len=256) :: dump_variable

  integer :: i,j,k, slen
  integer :: narg, iarg, larg, lstopt
  integer :: iargc, l

  narg=iargc()
  iarg=1
  lstopt=0

!
! parse command line options
!
  do while (iarg <= narg .and. lstopt == 0)
     call getarg(iarg,carg)
     larg=len_trim(carg)
     print *, iarg , trim(carg)
     iarg=iarg+1

     if (carg(1:1) /= '-') then
         lstopt=1
         iarg=iarg-1
     else if (larg == 1) then
        write(0,*) " diodump: invalid option -"
        stop 1
     else

        l=2
        do while(l <= larg)
          if(carg(l:l) == '-') then
              lstopt=1
          else if(carg(l:l)=='V' .or. carg(l:l)=='v' ) then
          
              IF(L.EQ.LARG) THEN
                L=0
                CALL GETARG(IARG,CARG)
                LARG=LEN_TRIM(CARG)
                IARG=IARG+1
              ENDIF
              dump_variable=CARG(L+1:LARG)
              L=LARG

              print *, ' dump_variable= ', dump_variable

          else
             write(0,*) " diodump: invalid option ",carg(l:l)
             stop 1
          end if
          l=l+1
        end do

     end if
  end do
!
! parse command line positional arguments; for now just a filename
!
  call getarg(iarg,fname)

  
  call dio_init(iret=iret)

  call dio_open(rfile,fname,"READ",iret=iret)

  do n=1,dio_numrec(rfile)

     call dio_recinfo(rfile,n,name,rank,dtype,bounds,iret=iret)
     write(0,*)n,name,rank,dtype

     if (dtype == DIO_INTEGER4) then

     if (rank == 0) then
        call dio_read(rfile,name,vi4_0,header=header,iret=iret)
        write(*,101)n,name,rank,dtype,vi4_0
        call dump_header(header)
     else if (rank == 1) then
        allocate(vi4_1(bounds(1):bounds(2)))
        call dio_read(rfile,name,vi4_1,header=header,iret=iret)
        write(*,101)n,name,rank,dtype,minval(vi4_1),maxval(vi4_1)
        call dump_header(header)
        deallocate(vi4_1)
     else if (rank == 2) then
        allocate(vi4_2(bounds(1):bounds(2),bounds(3):bounds(4)))
        call dio_read(rfile,name,vi4_2,header=header,iret=iret)
        write(*,101)n,name,rank,dtype,minval(vi4_2),maxval(vi4_2)
        call dump_header(header)
        deallocate(vi4_2)
     else if (rank == 3) then
        allocate(vi4_3(bounds(1):bounds(2),bounds(3):bounds(4),bounds(5):bounds(6)))
        call dio_read(rfile,name,vi4_3,header=header,iret=iret)
        write(*,101)n,name,rank,dtype,minval(vi4_3),maxval(vi4_3)
        call dump_header(header)
        deallocate(vi4_3)
     else if (rank == 4) then
        allocate(vi4_4(bounds(1):bounds(2),bounds(3):bounds(4),bounds(5):bounds(6),bounds(7):bounds(8)))
        call dio_read(rfile,name,vi4_4,header=header,iret=iret)
        write(*,101)n,name,rank,dtype,minval(vi4_4),maxval(vi4_4)
        call dump_header(header)
        deallocate(vi4_4)
     else
        write(0,*)'unknown rank ',rank
     end if
101  format(I3,1X,A,I3,I4,2I10)

     else if (dtype == DIO_REAL4) then
     if (rank == 0) then
        call dio_read(rfile,name,vr4_0,header=header,iret=iret)
        write(*,102)n,name,rank,dtype,vr4_0
        call dump_header(header)
     else if (rank == 1) then
        allocate(vr4_1(bounds(1):bounds(2)))
        call dio_read(rfile,name,vr4_1,header=header,iret=iret)
        write(*,102)n,name,rank,dtype,minval(vr4_1),maxval(vr4_1)
        call dump_header(header)
        deallocate(vr4_1)
     else if (rank == 2) then
        allocate(vr4_2(bounds(1):bounds(2),bounds(3):bounds(4)))
        call dio_read(rfile,name,vr4_2,header=header,iret=iret)
        write(*,102)n,name,rank,dtype,minval(vr4_2),maxval(vr4_2)
        call dump_header(header)
        deallocate(vr4_2)
     else if (rank == 3) then
        allocate(vr4_3(bounds(1):bounds(2),bounds(3):bounds(4),bounds(5):bounds(6)))
        call dio_read(rfile,name,vr4_3,header=header,iret=iret)
        write(*,'(I3,1X,A,I3,I4,3(A,I3,A,I3),A,2E20.10)')n,name,rank,dtype, &
                  "(",bounds(1),":",bounds(2), &
                  ",",bounds(3),":",bounds(4), &
                  ",",bounds(5),":",bounds(6),")", &
                  minval(vr4_3),maxval(vr4_3)
        call dump_header(header)
!        open(40,file=trim(name))
!        do k=bounds(5),bounds(6)
!        do j=bounds(3),bounds(4)
!        do i=bounds(1),bounds(2)
!           write(40,"(Z8.8)") vr4_3(i,j,k)
!        end do
!        end do
!        end do
!        close(40)
        deallocate(vr4_3)
     else if (rank == 4) then
        allocate(vr4_4(bounds(1):bounds(2),bounds(3):bounds(4),bounds(5):bounds(6),bounds(7):bounds(8)))
        call dio_read(rfile,name,vr4_4,header=header,iret=iret)
        write(*,'(I4,A,I3,I4,4(A,I3,A,I3),A,2E20.10)')n,name,rank,dtype, &
                  "(",bounds(1),":",bounds(2), &
                  ",",bounds(3),":",bounds(4), &
                  ",",bounds(5),":",bounds(6), &
                  ",",bounds(7),":",bounds(8),")", &
                  minval(vr4_4),maxval(vr4_4)
        call dump_header(header)
        deallocate(vr4_4)
     else
        write(0,*)'unknown rank ',rank
     end if
102  format (I3,1X,A,I3,I4,2E20.10)

     else if (dtype == DIO_REAL8) then
     if (rank == 0) then
        call dio_read(rfile,name,vr8_0,header=header,iret=iret)
        write(*,*)n,name,rank,dtype,vr8_0
        call dump_header(header)
     else if (rank == 1) then
        allocate(vr8_1(bounds(1):bounds(2)))
        call dio_read(rfile,name,vr8_1,header=header,iret=iret)
        write(*,*)n,name,rank,dtype,minval(vr8_1),maxval(vr8_1)
        call dump_header(header)
        deallocate(vr8_1)
     else if (rank == 2) then
        allocate(vr8_2(bounds(1):bounds(2),bounds(3):bounds(4)))
        call dio_read(rfile,name,vr8_2,header=header,iret=iret)
        write(*,*)n,name,rank,dtype,minval(vr8_2),maxval(vr8_2)
        call dump_header(header)
        deallocate(vr8_2)
     else if (rank == 3) then
        allocate(vr8_3(bounds(1):bounds(2),bounds(3):bounds(4),bounds(5):bounds(6)))
        call dio_read(rfile,name,vr8_3,header=header,iret=iret)
        write(*,*)n,name,rank, &
                  "(",bounds(1),":",bounds(2), &
                  ",",bounds(3),":",bounds(4), &
                  ",",bounds(5),":",bounds(6),")", &
                  dtype,minval(vr8_3),maxval(vr8_3)
        call dump_header(header)
!        open(40,file=trim(name))
!        do k=bounds(5),bounds(6)
!        do j=bounds(3),bounds(4)
!        do i=bounds(1),bounds(2)
!           write(40,"(Z8.8)") vr8_3(i,j,k)
!        end do
!        end do
!        end do
!        close(40)
        deallocate(vr8_3)
     else
        write(0,*)'unknown rank'
        stop
     end if
!     else if (dtype == DIO_LOGICAL4) then
!     if (rank == 0) then
!        call dio_read(rfile,name,v0l,header=header,iret=iret)
!        write(*,*)n,name,rank,dtype,v0l
!        call dump_header(header)
!     else if (rank == 1) then
!        allocate(v1l(bounds(1):bounds(2)))
!        call dio_read(rfile,name,v1l,header=header,iret=iret)
!        write(*,*)n,name,rank,dtype!,minval(v1l),maxval(v1l)
!        call dump_header(header)
!        deallocate(v1l)
!     else if (rank == 2) then
!        allocate(v2l(bounds(1):bounds(2),bounds(3):bounds(4)))
!        call dio_read(rfile,name,v2l,header=header,iret=iret)
!        write(*,*)n,name,rank,dtype!,minval(v2l),maxval(v2l)
!        call dump_header(header)
!        deallocate(v2l)
!     else if (rank == 3) then
!        allocate(v3l(bounds(1):bounds(2),bounds(3):bounds(4),bounds(5):bounds(6)))
!        call dio_read(rfile,name,v3l,header=header,iret=iret)
!        write(*,*)n,name,rank,dtype!,minval(v3l),maxval(v3l)
!        call dump_header(header)
!        deallocate(v3l)
!     end if


     else if (dtype == DIO_CHARACTER) then
     if (rank == 1) then
        slen = min(len(v1c),bounds(2))
        call dio_read(rfile,name,v1c(1:slen),header=header,iret=iret)
        write(*,103)n,name,rank,dtype,v1c(1:slen)
        call dump_header(header)
     else
        print *, ' unknown rank for DIO_CHARACTER', rank
        stop 1
     end if
103  format (I3,1X,A,I3,I4,1X,A)


     else
        print *, ' unknown dtype ',dtype
        stop 1
     end if

  end do

  call dio_close(rfile,iret=iret)

  call dio_finalize()

  stop

contains

subroutine dump_header(header)

  use wrfheader

  implicit none

  integer, dimension(:), intent(in) :: header

  character(len=32) :: DateStr,VarName,Units,Description,MemoryOrder,Stagger
  integer :: FieldType
  character(len=32), dimension(3) :: DimNames
  integer, dimension(3) :: DomainStart, DomainEnd, PatchStart, PatchEnd

  integer :: i

  if (header(1) == 0) return

  call wrfheader_unpack(header, DateStr=DateStr, &
                                VarName=VarName, &
                                Units=Units, &
                                Description=Description, &
                                FieldType=FieldType, &
                                MemoryOrder=MemoryOrder, &
                                Stagger=Stagger, &
                                DimNames=DimNames, &
                                DomainStart=DomainStart, DomainEnd=DomainEnd, &
                                PatchStart=PatchStart, PatchEnd=PatchEnd &
                       )


!  write(0,'(13A,I2,A,A,A,40I4)') "|",trim(DateStr),"|",trim(VarName),"|",trim(Units), &
!           "|",trim(Description),"|",trim(MemoryOrder),"|",trim(Stagger), &
!           "|",FieldType,"|",DimNames,"|", &
!           DomainStart, DomainEnd,PatchStart, PatchEnd
!
end subroutine dump_header

end program diodump
