   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: smth_desmth
   !
   ! Purpose: Apply the smoother-desmoother from the MM5 program TERRAIN
   !   (found in smther.F) to array (for non-e grids).
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine smth_desmth(array, lsmask, start_x, end_x, start_y, end_y, start_z, end_z, npass)

      implicit none

      ! Arguments
      integer, intent(in) :: start_x, start_y, start_z
      integer, intent(in) :: end_x, end_y, end_z
      integer, intent(in) :: npass
      real, dimension(start_x:end_x, start_y:end_y, start_z:end_z), intent(inout) :: array
      real, dimension(start_x:end_x, start_y:end_y), intent(in) :: lsmask

      ! Local variables
      integer :: ix, iy, iz, ipass
      real, pointer, dimension(:,:,:) :: scratch, array_save

      allocate(scratch(start_x+1:end_x-1, start_y:end_y, start_z:end_z))
      allocate(array_save(start_x:end_x, start_y:end_y, start_z:end_z))
      array_save = array

      do ipass=1,npass
         !
         ! Smoothing pass
         !
         do iy=start_y,end_y
            do ix=start_x+1,end_x-1
               do iz=start_z,end_z
                  scratch(ix,iy,iz) = 0.5*array(ix,iy,iz) + 0.25*(array(ix-1,iy,iz)+array(ix+1,iy,iz))
               end do
            end do
         end do

         do iy=start_y+1,end_y-1
            do ix=start_x+1,end_x-1
               do iz=start_z,end_z
                  array(ix,iy,iz) = 0.5*scratch(ix,iy,iz) + 0.25*(scratch(ix,iy-1,iz)+scratch(ix,iy+1,iz))
               end do
            end do
         end do
         !
         ! Desmoothing pass
         !
         do iy=start_y,end_y
            do ix=start_x+1,end_x-1
               do iz=start_z,end_z
                  scratch(ix,iy,iz) = 1.52*array(ix,iy,iz) - 0.26*(array(ix-1,iy,iz)+array(ix+1,iy,iz))
               end do
            end do
         end do

         do iy=start_y+1,end_y-1
            do ix=start_x+1,end_x-1
               do iz=start_z,end_z
                  array(ix,iy,iz) = 1.52*scratch(ix,iy,iz) - 0.26*(scratch(ix,iy-1,iz)+scratch(ix,iy+1,iz))
               end do
            end do
         end do

      end do

      deallocate(scratch)

!cggg don't allow smoother to affect any water points.  it makes the waterfall removing
! code less effective.

      do iy=start_y, end_y
      do ix=start_x, end_x     
        if (lsmask(ix,iy) == 0.0) then
          array(ix,iy,:) = array_save(ix,iy,:)
        endif
      enddo
      enddo

      deallocate (array_save)

   end subroutine smth_desmth

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: smth_desmth_egrid
   !
   ! Purpose: Apply the smoother-desmoother from the MM5 program TERRAIN
   !   (found in smther.F) to array.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine smth_desmth_egrid(array, lsmask, start_x, end_x, &
               start_y, end_y, start_z, end_z, npass, hflag)

      implicit none

      ! Arguments
      integer, intent(in) :: start_x, start_y, start_z
      integer, intent(in) :: end_x, end_y, end_z
      integer, intent(in) :: npass
      integer, intent(in) :: hflag
      real, dimension(start_x:end_x, start_y:end_y), intent(in) :: lsmask
      real, dimension(start_x:end_x, start_y:end_y, start_z:end_z), &
               intent(inout) :: array

      ! Local variables
      integer :: ix, iy, iz, ipass
      real, pointer, dimension(:,:,:) :: scratch, array_save
      integer :: ihe(start_y:end_y),ihw(start_y:end_y),istart(start_y:end_y)
      real, parameter:: cenwgt = 1.52
      real, parameter:: endwgt = 0.13

      allocate(scratch(start_x:end_x, start_y:end_y, start_z:end_z))
      allocate(array_save(start_x:end_x, start_y:end_y, start_z:end_z))
      array_save = array

      do iy=start_y,end_y
         if (hflag == 1) then
            ihe(iy) = abs(mod(iy+1,2))
            ihw(iy) = ihe(iy)-1
         else
            ! assign ive,ivw equivs to ihe,ihw
            ihe(iy) = abs(mod(iy,2))
            ihw(iy) = ihe(iy)-1
         end if
      end do

      do iy=start_y,end_y
         if (hflag == 1) then
            if (mod(iy,2) == 0) then
               istart(iy) = start_x
            else
               istart(iy) = start_x+1
            endif
         else ! v points
            if (abs(mod(iy,2)) == 1) then
               istart(iy) = start_x
            else
               istart(iy) = start_x+1
            endif
         endif
      end do

      do ipass=1,npass

         !
         ! Smoothing pass
         !

         do iy=start_y,end_y
            do ix=start_x,end_x
               do iz=start_z,end_z
                 scratch(ix,iy,iz) = array(ix,iy,iz) 
               end do
            end do
         end do

         do iy=start_y+1,end_y-1
            do ix=istart(iy),end_x-1
               do iz=start_z,end_z
                  scratch(ix,iy,iz) = 0.50*array(ix,iy,iz)+ &
                                      0.125*(array(ix+ihw(iy),iy-1,iz)+array(ix+ihe(iy),iy+1,iz)+ &
                                             array(ix+ihw(iy),iy+1,iz)+array(ix+ihe(iy),iy-1,iz))
               end do
            end do
         end do

         !
         ! Desmoothing pass
         !

         do iy=start_y+2,end_y-2
            do ix=istart(iy),end_x-1
               do iz=start_z,end_z
                  array(ix,iy,iz) = cenwgt*scratch(ix,iy,iz) - &
                                    endwgt*(scratch(ix+ihw(iy),iy-1,iz)+scratch(ix+ihe(iy),iy+1,iz) + &
                                            scratch(ix+ihw(iy),iy+1,iz)+scratch(ix+ihe(iy),iy-1,iz))
               end do
            end do
         end do

      end do

      deallocate(scratch)

!cggg don't allow smoother to affect any water points.  it makes the waterfall removing
! code less effective.

      do iy=start_y, end_y
      do ix=start_x, end_x     
        if (lsmask(ix,iy) == 0.0) then
          array(ix,iy,:) = array_save(ix,iy,:)
        endif
      enddo
      enddo

      deallocate (array_save)


 end subroutine smth_desmth_egrid

 subroutine smdhld_egrid(ime,jme,h,s,lines,nsmud)

! INPUTS:

!   IME - x dimension limit (IM)
!   JME - j dimension limit (JM)
!     H - input/output topography field
!     S - input land/sea mask (0=water, 1=land)
! LINES - number of rows over which the more severe 
!         5-point smoothing is applied (=12 in SI)
! NSMUD - number of smoothing passes (=12 in SI)

! OUTPUTS:
!     H - input/output topography field

!cggg setting to zero removes orog from channel islands
      parameter(hthresh=0.0)
!cggg      parameter(hthresh=50.0)
      dimension ihw(jme),ihe(jme)
      dimension h(ime,jme), h_save(ime,jme)   &
               ,hbms(ime,jme),hne(ime,jme),hse(ime,jme)
      real, intent(in) :: s(ime,jme)
!-----------------------------------------------------------------------
          do j=1,jme
      ihw(j)=-mod(j,2)
      ihe(j)=ihw(j)+1
          enddo
!-----------------------------------------------------------------------

              do j=1,jme
          do i=1,ime
      h_save(i,j)=h(i,j)
      hbms(i,j)=s(i,j)
          enddo
              enddo
!
      jmelin=jme-lines+1
      ibas=lines/2
      m2l=mod(lines,2)
!
              do j=lines,jmelin
          ihl=ibas+mod(j,2)+m2l*mod(j+1,2)
          ihh=ime-ibas-m2l*mod(j+1,2)

          do i=ihl,ihh
      hbms(i,j)=0.
          enddo
              enddo
!-----------------------------------------------------------------------
!cggg                  do ks=1,nsmud
                  do ks=1,12

		write(6,*) 'H(1,1): ', h(1,1)
		write(6,*) 'H(3,1): ', h(1,1)
!-----------------------------------------------------------------------
              do j=1,jme-1
          do i=1,ime-1
      hne(i,j)=h(i+ihe(j),j+1)-h(i,j)
          enddo
              enddo
              do j=2,jme
          do i=1,ime-1
      hse(i,j)=h(i+ihe(j),j-1)-h(i,j)
          enddo
              enddo
!
              do j=2,jme-1
          do i=1+mod(j,2),ime-1
      h(i,j)=(hne(i,j)-hne(i+ihw(j),j-1)     &
             +hse(i,j)-hse(i+ihw(j),j+1))*hbms(i,j)*0.125+h(i,j)
          enddo
              enddo
!-----------------------------------------------------------------------

!	special treatment for four corners

	if (hbms(1,1) .eq. 1) then
	h(1,1)=0.75*h(1,1)+0.125*h(1+ihe(1),2)+    &
                                  0.0625*(h(2,1)+h(1,3))
	endif

	if (hbms(ime,1) .eq. 1) then
	h(ime,1)=0.75*h(ime,1)+0.125*h(ime+ihw(1),2)+    &
                                  0.0625*(h(ime-1,1)+h(ime,3))
	endif

	if (hbms(1,jme) .eq. 1) then	
	h(1,jme)=0.75*h(1,jme)+0.125*h(1+ihe(jme),jme-1)+   &
                                 0.0625*(h(2,jme)+h(1,jme-2))
	endif

	if (hbms(ime,jme) .eq. 1) then	
	h(ime,jme)=0.75*h(ime,jme)+0.125*h(ime+ihw(jme),jme-1)+  &
                                  0.0625*(h(ime-1,jme)+h(ime,jme-2))
	endif


!	S bound
	
	J=1
	do I=2,ime-1
	if (hbms(I,J) .eq. 1) then	
	h(I,J)=0.75*h(I,J)+0.125*(h(I+ihw(J),J+1)+h(I+ihe(J),J+1))
	endif
	enddo

!	N bound

	J=JME
	do I=2,ime-1
	if (hbms(I,J) .eq. 1) then	
	h(I,J)=0.75*h(I,J)+0.125*(h(I+ihw(J),J-1)+h(I+ihe(J),J-1))
	endif
	enddo

! 	W bound

	I=1
	do J=3,jme-2
	if (hbms(I,J) .eq. 1) then	
	h(I,J)=0.75*h(I,J)+0.125*(h(I+ihe(J),J+1)+h(I+ihe(J),J-1))
	endif
	enddo

! 	E bound

	I=IME
	do J=3,jme-2
	if (hbms(I,J) .eq. 1) then	
	h(I,J)=0.75*h(I,J)+0.125*(h(I+ihw(J),J+1)+h(I+ihw(J),J-1))
	endif
	enddo


                      enddo   ! end ks loop

!!	touch with a 5-point filter over isolated peaks?

       do ks=1,nsmud

	do J=lines-1,jme-(lines-1)
          ihl=ibas+mod(j,2)+m2l*mod(j+1,2)
          ihh=ime-ibas-m2l*mod(j+1,2)

!	do I=lines-1,ime-(lines-1)
	do I=ihl,ihh

!cggg don't smooth isolated islands
        if ( s(i+ihw(J),J+1) == 0. .and.   &
             s(I+ihe(J),J+1) == 0. .and.   &
             s(i+ihw(J),J-1) == 0. .and.   &
             s(I+ihe(J),J-1) == 0. ) cycle

	if (s(I,J) > 0 ) then

        if( (h(I,J)-h(i+ihw(J),J+1) .gt. hthresh .and.   &
            h(I,J)-h(I+ihe(J),J+1) .gt. hthresh .and.    &
            h(I,J)-h(i+ihw(J),J-1) .gt. hthresh .and.    &
            h(I,J)-h(I+ihe(J),J-1) .gt. hthresh ) ) then


	h(I,J)=h(I,J)+0.125*( h(i+ihw(J),J+1) + h(I+ihe(J),J+1) +   &
        	 	      h(i+ihw(J),J-1) + h(I+ihe(J),J-1) -   &
                              4*h(I,J) )

	endif
	endif
        enddo
        enddo

	enddo

	write(6,*) 'smoothing change to topography:'
	do j=jme,1,-jme/30
	write(6,683) ( (h(i,j)-h_save(i,j)),i=1,ime,ime/15 )
	enddo
	
  683	format(20(f5.0,1x))

      return
      end subroutine smdhld_egrid

 subroutine inner_bound_egrid(ime, jme, h)

! 4-point averaging of mountains along inner boundary-------
! taken from wrf si

 implicit none

 integer             :: i, j
 integer, intent(in) :: ime, jme

 real, intent(inout) :: h(ime,jme)

 do i=1,ime-1
   h(i,2)=0.25*(h(i,1) + h(i+1,1) + h(i,3) + h(i+1,3))
 enddo

 do i=1,ime-1
   h(i,jme-1)=0.25*(h(i,jme-2) + &
                          h(i+1,jme-2) +  &
                          h(i,jme) + h(i+1,jme))
 enddo

 do j=4,jme-3,2
    h(1,j)=0.25*(h(1,j-1) + h(2,j-1)+  &
                            h(1,j+1)+h(2,j+1))
 enddo

 do j=4,jme-3,2
    h(ime-1,j)=0.25*(h(ime-1,j-1)+h(ime,j-1)+  &
                                h(ime-1,j+1)+h(ime,j+1))
 enddo

 return

 end subroutine inner_bound_egrid

      subroutine smdhld(lsmask, h, imdl, jmdl, nsmth)

!-------------------------------------------------------------------
!     this routine will chop mountains with nsmth passes.
!
!     it also smooths the lateral boundaries with a more
!     heavy handed approach.  the number outer row/cols
!     smoothed is controled by variable lines.
!
!     if called with nsmth set to zero, the lateral boundaries
!     will be smoothed only.
!-------------------------------------------------------------------

      implicit none

      integer                 :: count, i, j, ks
      integer                 :: ibnd, jbnd, lines
      integer, intent(in)     :: imdl, jmdl, nsmth

      real, intent(inout)     :: h(imdl,jmdl)
      real                    :: h_old(imdl,jmdl)
      real, parameter         :: hthresh = 10.0
      real, intent(in)        :: lsmask(imdl,jmdl)

!     peak chopping over entire grid

      do ks = 1, nsmth

        h_old = h
        count = 0

        do j = 2, jmdl-1
        do i = 2, imdl-1

!        don't smooth isolated islands
         if ( lsmask(i+1,j) == 0. .and.   &
              lsmask(i-1,j) == 0. .and.   &
              lsmask(i,j+1) == 0. .and.   &
              lsmask(i,j-1) == 0. ) cycle

          if (lsmask(i,j) > 0.0) then

            if( (h_old(i,j)-h_old(i+1,j)) .gt. hthresh .and.   &
                (h_old(i,j)-h_old(i-1,j)) .gt. hthresh .and.    &
                (h_old(i,j)-h_old(i,j-1)) .gt. hthresh .and.    &
                (h_old(i,j)-h_old(i,j+1)) .gt. hthresh  ) then

              count = count + 1

              h(i,j) = ( h_old(i+1,j) + h_old(i-1,j) + h_old(i,j-1) + h_old(i,j+1) - &
                        4.0 * h_old(i,j) )*0.125 + h_old(i,j)

            endif

          endif

        enddo
        enddo

       print*,'- AFTER PASS ',ks, ' NUMBER MTNS CHOPPED IS: ',count
       if (count == 0) exit

      enddo

!     smooth lateral boundary

      lines = 12  ! how many outer rows/columns to smooth
      ibnd  = imdl - lines + 1
      jbnd  = jmdl - lines + 1

      do ks = 1, 12  ! always 12 passes

        h_old = h

        do j = 2, jmdl-1
        do i = 2, imdl-1
          if (i > lines .and. i < ibnd .and. j > lines .and. j < jbnd) cycle
          if (lsmask(i,j) > 0.0) then
            h(i,j) = ( h_old(i+1,j) + h_old(i-1,j) + h_old(i,j-1) + h_old(i,j+1) - &
                       4.0 * h_old(i,j) )*0.125 + h_old(i,j)
          endif
        enddo
        enddo

!	special treatment for four corners

	if (lsmask(1,1) > 0.0) then
          h(1,1) = 0.75*h_old(1,1) + 0.05*h_old(2,2) +  &
                   0.1*(h_old(2,1) + h_old(1,2))
	endif

	if (lsmask(imdl,1) > 0.0) then 
          h(imdl,1) = 0.75*h_old(imdl,1) + 0.05*h_old(imdl-1,2) +  &
                      0.1*(h_old(imdl-1,1)+h_old(imdl,2))
	endif

	if (lsmask(1,jmdl) > 0.0) then	
          h(1,jmdl) = 0.75*h_old(1,jmdl) + 0.05*h_old(2,jmdl-1) +  &
                      0.1*(h_old(2,jmdl)+h_old(1,jmdl-1))
	endif

	if (lsmask(imdl,jmdl) > 0.0) then	
          h(imdl,jmdl) = 0.75*h_old(imdl,jmdl) + 0.05*h_old(imdl-1,jmdl-1) +   &
                                0.1*(h_old(imdl-1,jmdl)+h_old(imdl,jmdl-1))
	endif

!       s bound

        do i = 2, imdl-1
          if (lsmask(i,1) > 0.0) then	
            h(i,1) = 0.7*h_old(i,1) + 0.1*( h_old(i-1,1) + h_old(i+1,1) + h_old(i,2) )
          endif
        enddo

!       n bound

        do i = 2, imdl-1
          if (lsmask(i,jmdl) > 0.0) then	
            h(i,jmdl) = 0.7*h_old(i,jmdl) +  &
                        0.1*( h_old(i-1,jmdl) + h_old(i+1,jmdl) + h_old(i,jmdl-1) )
          endif
        enddo

!       w bound

        do j = 2, jmdl-1
          if (lsmask(1,j) > 0.0) then	
            h(1,j) = 0.7*h_old(1,j) + 0.1*( h_old(1,j-1) + h_old(1,j+1) + h_old(2,j) )
          endif
        enddo

!       e bound

        do j = 2, jmdl-1
          if (lsmask(imdl,j) > 0.0) then	
            h(imdl,j) = 0.7*h_old(imdl,j) +   &
                        0.1*( h_old(imdl,j-1) + h_old(imdl,j+1) + h_old(imdl-1,j) )
          endif
        enddo

      enddo  ! ks loop

      return
      end subroutine smdhld

 subroutine inner_bound(lsmask, h, imdl, jmdl)

 implicit none

 integer                         :: i, j
 integer, intent(in)             :: imdl, jmdl

 real, intent(inout)             :: h(imdl,jmdl)
 real, intent(in)                :: lsmask(imdl,jmdl)

 real, allocatable               :: h_old(:,:)

! apply extra smoothing along inner boundary

 allocate(h_old(imdl,jmdl))

 h_old = h

! s inner bound

 do i = 2, imdl-1
   if (lsmask(i,2) > 0.0) then	
     h(i,2) = 0.25 * (h_old(i,1) + h_old(i-1,2) + h_old(i+1,2) + h_old(i,3))
   endif
 enddo

! n inner bound

 do i = 2, imdl-1
   if (lsmask(i,jmdl-1) > 0.0) then	
     h(i,jmdl-1) = 0.25 * (h_old(i,jmdl) + h_old(i-1,jmdl-1) + &
                           h_old(i+1,jmdl-1) + h_old(i,jmdl-2))
   endif
 enddo

! w inner bound

 do j = 3, jmdl-2
   if (lsmask(2,j) > 0.0) then	
     h(2,j) = 0.25 * (h_old(1,j) + h_old(2,j-1) + h_old(2,j+1) + h_old(3,j))
   endif
 enddo

! e inner bound

 do j = 3, jmdl-2
   if (lsmask(imdl-1,j) > 0.0) then	
     h(imdl-1,j) = 0.25 * (h_old(imdl,j) + h_old(imdl-1,j-1) + &
                           h_old(imdl-1,j+1) + h_old(imdl-2,j))
   endif
 enddo

 deallocate (h_old)

 return

 end subroutine inner_bound
