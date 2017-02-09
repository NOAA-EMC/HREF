	subroutine pmatch_mean(vname,rawdata_mn,vrbl_mn,
     &         jpd1,jpd2,jpd10,vrbl_mn_pm,lm,lv,jf,iens)

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

	write(*,*) 'pmatch_mean iens, jf, lm: ', iens, jf, lm

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

        write(6,*) 'maxval(rawdata_mn): ',maxval(rawdata_mn(:,:,lv))
        write(6,*) 'maxval(vrbl_mn(:,lv)): ',maxval(vrbl_mn(:,lv))

        vrbl_mn_pm(:,:)=-999.

        ibound_max=0
        ibound_min=0

      ens_loop:  do J=1,jf*(iens),iens    ! loop over full ensemble, skipping

        I=1+(J-1)/(iens)
        iplace=listorder(I)

!!!  use unsorted version if looking at iplace

      !if (vname .eq.'AP3h') then
  
        if(jpd1.eq.1.and.jpd2.eq.8.and.jpd10.eq.1.and.     !APCP
     +     vrbl_mn_hold(iplace,lv).eq.0) then
          vrbl_mn_pm(iplace,lv)=vrbl_mn_hold(iplace,lv)
          cycle ens_loop
        end if
        if(jpd1.eq.16.and.jpd2.eq.196.and.jpd10.eq.200.and. !REFC
     +     vrbl_mn_hold(iplace,lv).eq.-20.0) then 
          vrbl_mn_pm(iplace,lv)=vrbl_mn_hold(iplace,lv)
          cycle ens_loop
        end if
        if(jpd1.eq.16.and.jpd2.eq.195.and.jpd10.eq.105.and. !REFD
     +     vrbl_mn_hold(iplace,lv).eq.-20.0) then
          vrbl_mn_pm(iplace,lv)=vrbl_mn_hold(iplace,lv)
          cycle ens_loop
        end if
        if(jpd1.eq.16.and.jpd2.eq.195.and.jpd10.eq.105.and. !RETOP
     +     vrbl_mn_hold(iplace,lv).le.0.0) then
          vrbl_mn_pm(iplace,lv)=vrbl_mn_hold(iplace,lv)
          cycle ens_loop
        end if



        !if (vrbl_mn_hold(iplace,lv) .eq. 0) then
        !  vrbl_mn_pm(iplace,lv)=0.
        !else

         amin=999. 
         amax=-999.
         do JJ=1,iens
          if (rawdata_mn(iplace,JJ,lm) .gt. amax) then
                amax=rawdata_mn(iplace,JJ,lm)
          endif
          if (rawdata_mn(iplace,JJ,lm) .lt. amin) then
                amin=rawdata_mn(iplace,JJ,lm)
          endif
         enddo
 
         if (rawdata_1d(J) .gt. amax) then
          vrbl_mn_pm(iplace,lv)=amax 
          ibound_max=ibound_max+1
!	write(*,*) 'iplace, amax, rawdata_1d: ', iplace,
!     &         amax, rawdata_1d(J)
         elseif (rawdata_1d(J) .lt. amin) then
          vrbl_mn_pm(iplace,lv)=amin
          ibound_min=ibound_min+1
!	write(*,*) 'iplace, amin, rawdata_1d: ', iplace,
!     &         amin, rawdata_1d(J)
         else
          vrbl_mn_pm(iplace,lv)=rawdata_1d(J)
         endif

       !endif

      !endif

!!! add it to REFD as well?  How much is it slowing down the code?

        !if (vname .eq. 'REFD') then
        ! if (vrbl_mn_hold(iplace,lv) .eq. -20.) then
        !  vrbl_mn_pm(iplace,lv)=-20.
        ! else
        !  vrbl_mn_pm(iplace,lv)=rawdata_1d(J)
        ! endif
        !endif

       end do ens_loop ! loop over ensemble

!	write(0,*) 'ibound_min: ', ibound_min
!	write(0,*) 'ibound_max: ', ibound_max

	deallocate(listorderfull)
        deallocate(listorder)
        deallocate(rawdata_1d)
        deallocate(vrbl_mn_hold)

	end subroutine pmatch_mean
