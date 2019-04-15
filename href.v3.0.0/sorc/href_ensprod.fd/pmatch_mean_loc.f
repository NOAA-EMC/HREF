	subroutine pmatch_mean_loc(isize,jsize,rawdata_1d,vrbl_mn_2d,
     &         vrbl_mn_pm_2d,
     &         ips,ipe,jps,jpe,iens)

        integer, intent(in) :: ips,ipe,jps,jpe, isize,jsize, iens
!	real :: vrbl_mn(jf,lm),vrbl_mn_pm(jf,lm)

	real, allocatable :: vrbl_mn_hold(:)
        integer, allocatable :: listorderfull(:),listorder(:)
        integer :: iplace,lm,lf,jf,JJ,Iloc,Jloc
        integer :: ibound_max, ibound_min
        real:: amin,amax
        real :: rawdata_1d(isize*jsize*iens)
        real :: vrbl_mn(isize*jsize)
        real :: vrbl_mn_2d(isize,jsize)
        real :: vrbl_mn_pm_2d(isize,jsize)
        real :: vrbl_mn_pm(isize*jsize)

        isize_alt=ipe-ips+1
        jsize_alt=jpe-jps+1

	if (isize .ne. isize_alt) then
	write(0,*) 'mismatch...isize, isize_alt: ', isize, isize_alt
        endif

        jf_loc=isize*jsize

        allocate(listorderfull(jf_loc*iens))
        allocate(listorder(jf_loc))
        allocate(vrbl_mn_hold(jf_loc))

! get a 1D array of the mean from the 2D subset

        vrbl_mn = RESHAPE(vrbl_mn_2d, (/isize*jsize/))

        do I=1,iens
        do JJ=jps,jpe
        do II=ips,ipe
!
        J=(JJ-JPS)*ISIZE+(II-IPS+1)

        Jglb=(JJ-1)*IM + II


        listorder(J)=J
        listorderfull((I-1)*jf_loc+J)=(I-1)*jf_loc+J
	
! force reflectivity type fields to be zero
!         if(jpd1.eq.16.and.(jpd2.eq.195 
!     &                 .or. jpd2.eq.196
!     &                 .or. jpd2.eq.198) .and. 
!     &   rawdata_mn(J,I,lv) .lt. 0.) then
!
!        rawdata_1d((I-1)*jf_loc+J)=0.
!        rawdata_mn_loc(J,I,lv)=0.
!        else
!        rawdata_1d((I-1)*jf_loc+J)=rawdata_mn(J,I,lv)
!        endif

        enddo
        enddo
        enddo

        vrbl_mn_hold=vrbl_mn

! first sort just getting the listorder from vrbl_mn.  The sorted vrbl_mn is not reused.
        call quick_sort(vrbl_mn,listorder,jf_loc)
! second sort puts rawdata_1d in order.  listorderfull ignored 
        call quick_sort(rawdata_1d,listorderfull,iens*jf_loc)

!         write(0,*) 'minval(vrbl_mn(:)): ',minval(vrbl_mn(:))
!         write(0,*) 'maxval(vrbl_mn(:)): ',maxval(vrbl_mn(:))
!         write(0,*) 'minval(rawdata_1d(:)): ', minval(rawdata_1d(:))
!         write(0,*) 'maxval(rawdata_1d(:)): ', maxval(rawdata_1d(:))

        vrbl_mn_pm(:)=-999.

        ibound_max=0
        ibound_min=0

      ens_loop:  do J=1,jf_loc*(iens),iens    ! loop over full ensemble, skipping

         I=1+(J-1)/(iens)
         iplace=listorder(I)

!!!  use unsorted version if looking at iplace

         if(vrbl_mn_hold(iplace).eq.0) then     ! was just APCP...removed pds checks as not passed
            vrbl_mn_pm(iplace)=0.
            cycle ens_loop
         end if

         amin=9999. 
         amax=-9999.
         do JJ=1,iens
          if (rawdata_1d(iplace*JJ) .gt. amax) then
                amax=rawdata_1d(iplace*JJ)
          endif
          if (rawdata_1d(iplace*JJ) .lt. amin) then
                amin=rawdata_1d(iplace*JJ)
          endif
         enddo
 
!!!!!!!!!!!!!!!!!!!!!!!!!!
         if (rawdata_1d(J) .gt. amax) then
          vrbl_mn_pm(iplace)=amax 
          ibound_max=ibound_max+1
         elseif (rawdata_1d(J) .lt. amin) then
          vrbl_mn_pm(iplace)=amin
          ibound_min=ibound_min+1
         else
          vrbl_mn_pm(iplace)=rawdata_1d(J)
         endif

      enddo ens_loop

! restore the mean value for use in possible blending

!        vrbl_mn=vrbl_mn_hold

!	write(0,*) 'ibound_min: ', ibound_min
!	write(0,*) 'ibound_max: ', ibound_max


! put PM mean back on 2D
        do JJ=jps,jpe
          Jloc=JJ-jps+1
        do II=ips,ipe
          Iloc=II-ips+1
          I1D=(JJ-jps)*isize+(II-ips)
          vrbl_mn_pm_2d(Iloc,Jloc)=vrbl_mn_pm(I1D)
        enddo
        enddo

	deallocate(listorderfull)
        deallocate(listorder)
        deallocate(vrbl_mn_hold)

	end subroutine pmatch_mean_loc
