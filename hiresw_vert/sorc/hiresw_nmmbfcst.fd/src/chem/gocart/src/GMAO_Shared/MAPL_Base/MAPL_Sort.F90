
#define ASSERT_(A) if(.not.(A))call exit(1)


!  $Id: MAPL_Sort.F90,v 1.6 2009/04/22 15:05:32 f4mjs Exp $

!=============================================================================
!BOP

! !MODULE: MAPL_Sort   -- A utility to sort integers

! !INTERFACE:

module MAPL_SortMod

  implicit none
  private

! !PUBLIC ROUTINES:

  public MAPL_Sort

! !DESCRIPTION:
! 
!   {\tt GEOS\_Sort} is a utility to do a quicksort on integers. The general
!   interface is:
!\bv       
!       subroutine MAPL_Sort(A)
!         integer(kind=[4,8]),       intent(INOUT) :: A(:)
!         integer(kind=4), optional, intent(INOUT) :: B(size(A))
!
!       subroutine MAPL_Sort(A,B)
!         integer(kind=[4,8]),       intent(INOUT) :: A(:)
!         integer(kind=4),           intent(INOUT) :: B(:,:)
!         integer,         optional, intent(IN   ) :: DIM
!\ev
!   {\tt GEOS\_Sort} sorts A in ascending order and reorders the data in B
!   in the same order; i.e., it does the same exchanges to B as were done 
!   to A in sorting it.  If, for example, on input B(:) contains the ordered integers
!   from 1 to size(A), on output it will contain the positions of the elements of
!   the sorted A in the unsorted A. In the second signature, DIM=1 corresponds
!   to a B ordered as B(size(A),:), whereas DIM=2 corresponds to B(:,size(A)).
!   The default is DIM=2. The quicksort is coded in C and does not appear here.

!EOP
!=============================================================================

interface MAPL_Sort
   module procedure SORT1L
   module procedure SORT1R
   module procedure SORT1D
   module procedure SORT1S
   module procedure SORT2L
   module procedure SORT2S
   module procedure SORT2DS
end interface

contains

subroutine SORT1S(A,B)
  integer(kind=4),           intent(INOUT) :: A(:)
  integer(kind=4), optional, intent(INOUT) :: B(:)
  if(present(B)) then
     call QSORTS(A,B,size(A),1)
  else
     call QSORTS(A,A,size(A),0)
  endif
end subroutine SORT1S

subroutine SORT1R(A,B)
  integer(kind=4),           intent(INOUT) :: A(:)
  real   (kind=4),           intent(INOUT) :: B(:)
  call QSORTS(A,B,size(A),1)
end subroutine SORT1R

subroutine SORT1D(A,B)
  integer(kind=4),           intent(INOUT) :: A(:)
  real   (kind=8),           intent(INOUT) :: B(:)
  call QSORTS(A,B,size(A),2)
end subroutine SORT1D

subroutine SORT1L(A,B)
  integer(kind=8), intent(INOUT) :: A(:)
  integer(kind=4), optional, intent(INOUT) :: B(:)
  if(present(B)) then
     call QSORT(A,B,size(A),1)
  else
     call QSORT(A,A,size(A),0)
  endif
end subroutine SORT1L

subroutine SORT2S(A,B,DIM)
  integer(kind=4),   intent(INOUT) :: A(:)
  integer(kind=4),   intent(INOUT) :: B(:,:)
  integer, optional, intent(IN   ) :: DIM

  integer :: uDIM

  if(present(DIM)) then
     uDIM = DIM
  else
     uDIM = 2
  end if
  ASSERT_(uDIM>0 .and. uDIM<3)
  ASSERT_(size(A)==size(B,uDIM))
  if(uDIM==1) then
     call QSORTS(A,B,size(A),-size(B,2))
  else
     call QSORTS(A,B,size(A), size(B,1))
  end if

end subroutine SORT2S



subroutine SORT2DS(B,DIM)
  integer(kind=4),   intent(INOUT) :: B(:,:)
  integer, optional, intent(IN   ) :: DIM

  integer :: uDIM

  if(present(DIM)) then
     uDIM = DIM
  else
     uDIM = 2
  end if

  ASSERT_(uDIM>0 .and. uDIM<3)

  if(uDIM==1) then
     call QSORTS(B(:,1),B(:,2:),size(B,1),-(size(B,2)-1))
  else
     call QSORTS(B(1,:),B(2:,:),size(B,2), (size(B,1)-1))
  end if

end subroutine SORT2DS

subroutine SORT2L(A,B,DIM)
  integer(kind=8),   intent(INOUT) :: A(:)
  integer(kind=4),   intent(INOUT) :: B(:,:)
  integer, optional, intent(IN   ) :: DIM

  integer :: uDIM

  if(present(DIM)) then
     uDIM = DIM
  else
     uDIM = 2
  end if
  ASSERT_(uDIM>0 .and. uDIM<3)
  ASSERT_(size(A)==size(B,uDIM))
  if(uDIM==1) then
     call QSORT(A,B,size(A),-size(B,2))
  else
     call QSORT(A,B,size(A), size(B,1))
  end if
end subroutine SORT2L

end module MAPL_SortMod
