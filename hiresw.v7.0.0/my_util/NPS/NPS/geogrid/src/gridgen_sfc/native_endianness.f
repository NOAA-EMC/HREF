 module native_endianness

!----------------------------------------------------------------------
! Dusan Jovic, NCEP/EMC, 2012
!----------------------------------------------------------------------

 implicit none

 private

 public to_native_endianness
 logical, parameter, public :: is_little_endian = ichar(transfer(1,'a')) == 1

 interface to_native_endianness
   module procedure to_native_endianness_i2
   module procedure to_native_endianness_i4
   module procedure to_native_endianness_r4
   module procedure to_native_endianness_r8
 end interface to_native_endianness

 contains

!----------------------------------------------------------------------
! convert 2-byte integer scalar from big-endian to native-endian
!----------------------------------------------------------------------

 subroutine to_native_endianness_i2(i2)

 implicit none

 integer(kind=2), intent(inout) :: i2

 integer(kind=1), dimension(2) :: byte_arr, byte_arr_tmp
 integer :: i

 if (.not.is_little_endian) return

 byte_arr_tmp = transfer (i2, byte_arr_tmp)

 do i = 1, 2
   byte_arr(i) = byte_arr_tmp(3-i)
 end do

 i2 = transfer (byte_arr, i2)

 return

 end subroutine to_native_endianness_i2


!----------------------------------------------------------------------
! convert 4-byte integer scalar from big-endian to native-endian
!----------------------------------------------------------------------

 subroutine to_native_endianness_i4(i4)

 implicit none

 integer(kind=4), intent(inout) :: i4

 integer(kind=1), dimension(4) :: byte_arr, byte_arr_tmp
 integer :: i

 if (.not.is_little_endian) return

 byte_arr_tmp = transfer (i4, byte_arr)

 do i = 1, 4
   byte_arr(i) = byte_arr_tmp(5-i)
 end do

 i4 = transfer (byte_arr, i4)

 return

 end subroutine to_native_endianness_i4

!----------------------------------------------------------------------
! convert 4-byte real scalar from big-endian to native-endian
!----------------------------------------------------------------------

 subroutine to_native_endianness_r4(r4)

 implicit none

 real(kind=4), intent(inout) :: r4

 integer(kind=1), dimension(4) :: byte_arr, byte_arr_tmp
 integer :: i

 if (.not.is_little_endian) return

 byte_arr_tmp = transfer (r4, byte_arr)

 do i = 1, 4
   byte_arr(i) = byte_arr_tmp(5-i)
 end do

 r4 = transfer (byte_arr, r4)

 return

 end subroutine to_native_endianness_r4

!----------------------------------------------------------------------
! convert 8-byte real scalar from big-endian to native-endian
!----------------------------------------------------------------------

 subroutine to_native_endianness_r8(r8)

 implicit none

 real(kind=8), intent(inout) :: r8

 integer(kind=1), dimension(8) :: byte_arr, byte_arr_tmp
 integer :: i

 if (.not.is_little_endian) return

 byte_arr_tmp = transfer (r8, byte_arr)

 do i = 1, 8
   byte_arr(i) = byte_arr_tmp(9-i)
 end do

 r8 = transfer (byte_arr, r8)

 return

 end subroutine to_native_endianness_r8

 end module native_endianness
