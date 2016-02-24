	subroutine pmatch_mean(vname,rawdata_mn,vrbl_mn,
     &                         vrbl_mn_pm,lm,lv,jf,iens)

        character(len=4) :: vname
	real :: rawdata_mn(jf,iens,lm),vrbl_mn(jf,lm),vrbl_mn_pm(jf,lm)

	real, allocatable :: rawdata_1d(:),vrbl_mn_hold(:,:)
        integer, allocatable :: listorderfull(:),listorder(:)
        integer :: iplace,lm,lf,jf,JJ
        integer :: ibound_max, ibound_min
        real:: amin,amax

        allocate(listorderfull(jf*iens))
        allocate(listorder(jf))
	allocate(rawdata_1d(jf*iens))
        allocate(vrbl_mn_hold(jf,lm))

	write(0,*) 'pmatch_mean iens, jf, lm: ', iens, jf, lm

        do I=1,iens
        do J=1,jf
        listorder(J)=J
        listorderfull((I-1)*jf+J)=(I-1)*jf+J
        rawdata_1d((I-1)*jf+J)=rawdata_mn(J,I,lv)
        enddo
        enddo

        vrbl_mn_hold=vrbl_mn

        call quick_sort(vrbl_mn,listorder,jf)
        call quick_sort(rawdata_1d,listorderfull,iens*jf)

        write(0,*) 'maxval(rawdata_mn): ',maxval(rawdata_mn(:,:,lv))
        write(0,*) 'maxval(vrbl_mn(:,lv)): ',maxval(vrbl_mn(:,lv))

        vrbl_mn_pm(:,:)=-999.

        ibound_max=0
        ibound_min=0

        do J=1,jf*(iens),iens    ! loop over full ensemble, skipping

        I=1+(J-1)/(iens)
        iplace=listorder(I)

!!!  use unsorted version if looking at iplace

        if (vname .eq.'AP3h') then

         if (vrbl_mn_hold(iplace,lv) .eq. 0) then
          vrbl_mn_pm(iplace,lv)=0.
         else

         amin=9999. 
         amax=-9999.
         do JJ=1,iens
          if (rawdata_mn(iplace,JJ,lv) .gt. amax) then
                amax=rawdata_mn(iplace,JJ,lv)
          endif
          if (rawdata_mn(iplace,JJ,lv) .lt. amin) then
                amin=rawdata_mn(iplace,JJ,lv)
          endif
         enddo
 
!!!!!!!!!!!!!!!!!!!!!!!!!!
         if (rawdata_1d(J) .gt. amax) then

          vrbl_mn_pm(iplace,lv)=amax 
          ibound_max=ibound_max+1

!	write(0,*) 'iplace,amax,rawdata_1d,vrbl_mn: ', iplace,
!     &         amax, rawdata_1d(J),vrbl_mn_hold(iplace,lv)

!	if (vrbl_mn_pm(iplace,lv) .ge. 150.) then	
!	write(0,*) 'BIG val: ',iplace, vrbl_mn_pm(iplace,lv)
!	endif

         elseif (rawdata_1d(J) .lt. amin) then

          vrbl_mn_pm(iplace,lv)=amin
          ibound_min=ibound_min+1

!	write(0,*) 'iplace, amin, rawdata_1d, vrbl_mn: ',iplace,
!     &         amin, rawdata_1d(J),vrbl_mn_hold(iplace,lv)

!	if (vrbl_mn_pm(iplace,lv) .ge. 150.) then	
!	write(0,*) 'BIG val (on min side): ',iplace, vrbl_mn_pm(iplace,lv)
!	endif

         else

          vrbl_mn_pm(iplace,lv)=rawdata_1d(J)

         endif

!!!!!!!!!!!!!!!!!!!!!!!!!!
         endif  ! vrbl_mn_hold check

        endif  ! AP3h

!!! add it to REFD as well?  How much is it slowing down the code?

        if (vname .eq. 'REFD') then


         if (vrbl_mn_hold(iplace,lv) .eq. -20.) then
          vrbl_mn_pm(iplace,lv)=-20.
         else

         amin=9999. 
         amax=-9999.
         do JJ=1,iens
          if (rawdata_mn(iplace,JJ,lv) .gt. amax) then
                amax=rawdata_mn(iplace,JJ,lv)
          endif
          if (rawdata_mn(iplace,JJ,lv) .lt. amin) then
                amin=rawdata_mn(iplace,JJ,lv)
          endif
         enddo

         if (rawdata_1d(J) .gt. amax) then

          vrbl_mn_pm(iplace,lv)=amax 
          ibound_max=ibound_max+1

!	if (vrbl_mn_pm(iplace,lv) .ge. 35.) then	
!	write(0,*) 'refd BIG val: ',iplace,vrbl_mn_pm(iplace,lv),
!     &         rawdata_1d(J),
!     &         vrbl_mn_hold(iplace,lv)
!	endif

         elseif (rawdata_1d(J) .lt. amin) then

          vrbl_mn_pm(iplace,lv)=amin
          ibound_min=ibound_min+1

         else

          vrbl_mn_pm(iplace,lv)=rawdata_1d(J)
         endif

        endif ! vrbl_mn_hold check
        endif ! on REFD

        enddo ! loop over ensemble

! restore the mean value for use in possible blending
        vrbl_mn=vrbl_mn_hold

	write(0,*) 'ibound_min: ', ibound_min
	write(0,*) 'ibound_max: ', ibound_max

	deallocate(listorderfull)
        deallocate(listorder)

	end subroutine pmatch_mean
