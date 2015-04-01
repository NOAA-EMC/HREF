!----------------------------------------------------------------------
! note: when running in thinned or reduced mode for gfs, the number
! of "i" points decreases towards the pole as described by the
! lonsperlat_mdl array.  all 2d arrays are dimensioned to the 
! 'full' 'i' dimension, but anything beyond i=lonsperlat_mdl(j)
! is not used.  these routines convert between thinned and full
! gaussian grids by filling in (or thinning) points using a
! nearest neighbor approach.   
!----------------------------------------------------------------------

 subroutine fill(dummy)

 use program_setup, only   : imdl, jmdl, lonsperlat_mdl

 implicit none

 integer                 :: j , jj, in, i2, m1, m2

 real                    :: r, x1
 real, intent(inout)     :: dummy(imdl,jmdl)
 real, allocatable       :: dummy1d(:)

 do j = 1, jmdl
   jj = j
   if (j > jmdl/2) jj = jmdl - j + 1
   if (imdl == lonsperlat_mdl(jj)) cycle
   allocate(dummy1d(lonsperlat_mdl(jj)))
   dummy1d=dummy(1:lonsperlat_mdl(jj),j)
   m1 = lonsperlat_mdl(jj)
   m2 = imdl
   r=real(m1)/real(m2)
   do i2=1,m2
     x1=(i2-1)*r
     in=mod(nint(x1),m1)+1
     dummy(i2,j) = dummy1d(in)
   enddo
   deallocate(dummy1d)
 enddo

 end subroutine fill

 subroutine thin(dummy)

 use program_setup, only   : imdl, jmdl, lonsperlat_mdl

 implicit none

 integer :: j , jj

 integer :: in, i2, m1, m2
 real  :: r, x1
 real, intent(inout) :: dummy(imdl,jmdl)
 real, allocatable   :: dummy1d(:)

 allocate(dummy1d(imdl))
 do j = 1, jmdl
   jj = j
   if (j > jmdl/2) jj = jmdl - j + 1
   if (imdl == lonsperlat_mdl(jj)) cycle
   m2 = lonsperlat_mdl(jj)
   m1 = imdl
   r=real(m1)/real(m2)
   dummy1d = dummy(:,j)
   dummy(:,j) = 0.0
   do i2=1,m2
     x1=(i2-1)*r
     in=mod(nint(x1),m1)+1
     dummy(i2,j) = dummy1d(in)
   enddo
 enddo
 deallocate(dummy1d)

 end subroutine thin
