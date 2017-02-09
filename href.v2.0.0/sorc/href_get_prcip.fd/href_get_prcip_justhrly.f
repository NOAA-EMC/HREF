C  raw data
       use grib_mod
       real,allocatable,dimension(:,:) ::  dphold !jf,4        
       real,allocatable,dimension(:)   ::  dp1
       integer iyr,imon,idy,ihr
       character*50 gdss(400)
       integer IENS, GRIBID, kgdss(200), lengds,im,jm,km,jf
       character*40 filehead,filename(8), output, outdone
       
       integer ff
       logical do_old
       character*3 fhr(36)
       integer iunit,ounit, pdt9_orig, pdt9_orig_1h ,pdt19_orig_1h
       type(gribfield) :: gfld,gfld_save_1h,gfld_2h,gfld_3h

       data (fhr(i),i=1,36)
     + /'f01','f02','f03','f04','f05','f06','f07','f08','f09',
     + 'f10','f11','f12','f13','f14','f15','f16','f17','f18',
     + 'f19','f20','f21','f22','f23','f24','f25','f26','f27',
     + 'f28','f29','f30','f31','f32','f33','f34','f35','f36'/
 
       GRIBID=227            !namnest grid
	write(0,*) 'to read of filehead, ff'
       read (*,*) filehead, ff, do_old

	write(0,*) 'start prcip code'

cc     RAP has one-hour accumu precip, so only one file is used
cc     NAM has no one-hour accumu precip, so two files are needed

       if(GRIBID.eq.255) then   !For NARRE 13km RAP grid#130
         im=1799
         jm=1059
         jf=im*jm
       else
         call makgds(GRIBID, kgdss, gdss, lengds, ier)
         im=kgdss(2)
         jm=kgdss(3)
         jf=kgdss(2)*kgdss(3)
       end if

       write(*,*) 'jf=',jf

       allocate(dphold(jf,3))
       allocate(dp1(jf))

!! these numbers need to change for hourly

	if (do_old .and. ff .gt. 1) then
         nfile=2
        else
         nfile=1
        endif
	write(0,*) 'do_old, nfile: ', do_old, nfile
       nff=ff/1

        dphold=0.

       do 1000 nf=1,nfile
        
        filename(nf)=filehead(1:14)//fhr(nff)

	write(0,*) 'nff, ff, nf, trim(filename(nf)): ', nff,ff,
     &                nf,trim(filename(nf))

        iunit=20+nf

        jpdtn=8    !APCP's Product Template# is  4.8 

        call baopenr(iunit,filename(nf),ierr)
        write(0,*) 'open ', iunit, filename(nf), 'ierr=',ierr

	if (ierr .eq. 0) then


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	if (mod(nff,3) .eq. 0) then
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	write(0,*) 'START MOD=0 BLOCK'

!  if nfile = 2, read current and previous

!  get 1 h accumulation if possible at 3 hourly time

        jpd1=1
        jpd2=8
        jpd27=1 !1 hr accumulation
        call readGB2(iunit,jpdtn,jpd1,jpd2,jpd27,gfld,ie)  !Large scale APCP

        if (ie.eq.0) then
	write(0,*) 'populate nf (mod=0) of nfile: ', nf, nfile
         dp1(:)=gfld%fld(:)
         if (nf.eq.1) then 
           gfld_save_1h=gfld
  	   write(0,*) 'gfld_save(9) when saved: ', gfld_save_1h%ipdtmpl(9)
	   pdt9_orig_1h=gfld_save_1h%ipdtmpl(9)
	   pdt19_orig_1h=gfld_save_1h%ipdtmpl(19)

	write(0,*) 'pdt9_orig_1h pdt19_orig_1h: ', pdt9_orig_1h,pdt19_orig_1h
           do i=1,gfld_save_1h%ipdtlen
            write(0,*) 'MOD=0 ', i, gfld_save_1h%ipdtmpl(i)
           end do
         end if

        else

! get 3 h accumulation that will be used in MOD=2 block to get the 1 h total

        jpd1=1
        jpd2=8
        jpd27=3 !3 hr accumulation
	write(0,*) 'seek 3 h accum'
        call readGB2(iunit,jpdtn,jpd1,jpd2,jpd27,gfld_3h,ie)  !Large scale APCP
        if (ie.eq.0) then
	write(0,*) 'populate 3 h accum  nf of nfile: ', nf, nfile
        dphold(:,3)=gfld_3h%fld(:)
	endif

       endif

! how define GRIB2 stuff?


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        elseif (mod(nff,3) .eq. 1) then
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! all should have 1 h total

	write(0,*) 'START MOD=1 BLOCK'

        jpd1=1
        jpd2=8
        jpd27=1 !1 hr accumulation
        call readGB2(iunit,jpdtn,jpd1,jpd2,jpd27,gfld,ie)  !Large scale APCP

        if (ie.eq.0) then

	if (nf .eq. 1) then
	write(0,*) 'mod=1, populate nf of nfile: ', nf, nfile
         dp1(:)=gfld%fld(:)
           gfld_save_1h=gfld

           do i=1,gfld_save_1h%ipdtlen
            write(0,*) 'MOD=1 ', i, gfld_save_1h%ipdtmpl(i)
           enddo


        else

	write(0,*) 'in here when 1 h old from 2 h block'
	write(0,*) 'maxval(fld), maxval(hold(2)): ', 
     &         maxval(gfld%fld), maxval(dphold(:,2))
          dp1(:)=dphold(:,2)-gfld%fld(:)
	write(0,*) 'definined dp1 from difference ', maxval(dp1)
	   gfld%fld(:)=dp1(:)
           gfld%ipdtmpl(9)=gfld%ipdtmpl(9)+1
           gfld%ipdtmpl(19)=gfld%ipdtmpl(19)+1
           gfld_save_1h=gfld

	write(0,*) 'here for 1 h accum ending at 2 h time'
	write(0,*) 'ipdtmpl vals: ', gfld%ipdtmpl(9),gfld%ipdtmpl(19)

!           do i=1,gfld_save_1h%ipdtlen
!            write(0,*) 'for 2 h NAM ', i, gfld_save_1h%ipdtmpl(i)
!           end do

        endif

         if (nf.eq.1) then 
           gfld_save_1h=gfld
  	   write(0,*) 'gfld_save_1h(9) when saved: ', gfld_save_1h%ipdtmpl(9)
	   pdt9_orig_1h=gfld_save_1h%ipdtmpl(9)
	   pdt19_orig_1h=gfld_save_1h%ipdtmpl(19)
           do i=1,gfld_save_1h%ipdtlen
            write(0,*) i, gfld_save_1h%ipdtmpl(i)
           end do
         end if
        endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	elseif (mod(nff,3) .eq. 2) then
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	write(0,*) 'START MOD=2 BLOCK'

!  if nfile = 2, read current and previous

         jpd1=1
         jpd2=8
         jpd27=1 !1 hr accumulation

!! NAM nest will find a 1 h total in the hour old
         call readGB2(iunit,jpdtn,jpd1,jpd2,jpd27,gfld,ie)  !Large scale APCP
         if (ie.eq.0) then
 	  write(0,*) 'mod=2, 1h accum  populate nf of nfile: ', nf, nfile
          dp1(:)=gfld%fld(:)
	write(0,*) 'here a ', maxval(dp1)
          dphold(:,1)=gfld%fld(:)
	write(0,*) 'here b'
	write(0,*) 'maxval(dphold(:,2)) ', maxval(dphold(:,2))

	if ( maxval(dphold(:,2)) .gt. 0) then 
	write(0,*) 'inside maxval(dphold(:,2) '
            dp1(:)=dphold(:,2)-dphold(:,1)
	write(0,*) 'maxval dpholds, dp1: ', maxval(dphold(:,1)), 
     +                       maxval(dphold(:,2)), maxval(dp1)
        endif

         if (nf.eq.1) then 
           gfld_save_1h=gfld
  	   write(0,*) 'gfld_save_1h(9) when saved: ', gfld_save_1h%ipdtmpl(9)
	   pdt9_orig_1h=gfld_save_1h%ipdtmpl(9)
           do i=1,gfld_save_1h%ipdtlen
            write(0,*) 'MOD=2 ', i, gfld_save_1h%ipdtmpl(i)
           enddo
         end if

         else

          jpd1=1
          jpd2=8
          jpd27=2 !2 hr accumulation
          call readGB2(iunit,jpdtn,jpd1,jpd2,jpd27,gfld,ie)  !Large scale APCP
          if (ie.eq.0) then
	    write(0,*) 'populate mod=2, 2 h accum nf of nfile: ', nf, nfile
            dphold(:,2)=gfld%fld(:)
	write(0,*) 'maxval(dphold(:,2)) ', maxval(dphold(:,2))
	
	if (nf .eq. 2) then
	dp1(:)=dphold(:,3)-dphold(:,2)
	write(0,*) 'definined dp1 from difference'
	write(0,*) 'maxval(dp1): ', maxval(dp1)
	   gfld%fld(:)=dp1(:)

	write(0,*) 'before increment'
	write(0,*) 'ipdtmpl vals: ', gfld%ipdtmpl(9),gfld%ipdtmpl(19)

           gfld%ipdtmpl(9)=gfld%ipdtmpl(9)+2
           gfld%ipdtmpl(19)=gfld%ipdtmpl(19)+1

	write(0,*) 'here for 1 h accum ending at 3 h time'
	write(0,*) 'ipdtmpl vals: ', gfld%ipdtmpl(9),gfld%ipdtmpl(19)

!
           gfld_save_1h=gfld
	
	endif

          endif ! ie=0
         endif ! ie=0
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   

	endif ! mod(nff,3)
        endif !  ierr=0

	write(0,*) 'to bottom and baclose ' , iunit

        call baclose(iunit,ierr)
        write(*,*) 'close ', filename(nf), 'ierr=',ierr
        nff=nff-1

1000  continue


cccccc  Then call putgb2 to store the calculated data into a grib2 file
c
c      data structure gfld is re-used for pack data since all are same
c      only gfld%fld and gfld%ipdtmpl(27) are different

Cmp   believe gfld%ipdtmpl(9) matters as well
c

c      If a field not in a GRIB2 file, getGB2 output gfld will be crashed. 
c      so use previously saved gfld_save 

       nff=ff/1      
       output='prcip'//filehead(5:14)//fhr(nff)
       outdone='prcipdone'//filehead(9:14)//fhr(nff)

!        ounit=50+nfhr
        ounit=50+nff
        call baopen(ounit,output,ierr)

!!! believe this might not be correct
!!!
!!!
!!! GEMPAK cannot unpack the resulting prcip file completely...date information in PDS
!!! not correct?  Wants everything to be 3 h totals


!!        Add a 1 h total for everyone
	   gfld=gfld_save_1h
	write(0,*) 'put gfld_save_1h into gfld'

	write(0,*) 'define fld as dp1'
	write(0,*) 'maxval(dp1): ', maxval(dp1)
             gfld%fld(:)=dp1(:)
             gfld%ipdtmpl(27)=1
	write(0,*) 'to putgb2 for ounit: ', ounit
	write(0,*) 'maxval(gfld%fld(:)): ', maxval(gfld%fld(:))
             call putgb2(ounit,gfld,ierr)
    
!        write(0,*) 'Pack APCP done for fhr',nfhr
        write(0,*) 'Pack APCP done for fhr',nff

! write "done" file

        call baclose(ounit,ierr) 

	if (filehead(6:8) .eq. 'm03') then
	open(unit=11,file=outdone)
	write(11,*) 'done'
	close(unit=11)
	endif

      stop
      end


      subroutine readGB2(igrb2,jpdtn,jpd1,jpd2,jpd27,gfld,iret)

        use grib_mod

        type(gribfield) :: gfld 
 
        integer jids(200), jpdt(200), jgdt(200)
        integer jpd1,jpd2,jpdtn
        logical :: unpck=.true. 
   

        jids=-9999  !array define center, master/local table, year,month,day, hour, etc, -9999 wildcard to accept any
        jpdt=-9999  !array define Product, to be determined
        jgdt=-9999  !array define Grid , -9999 wildcard to accept any

        jdisc=-1    !discipline#  -1 wildcard 
        jgdtn=-1    !grid template number,    -1 wildcard 
        jskp=0      !Number of fields to be skip, 0 search from beginning
        ifile=0

        jpdt(1)=jpd1   !Category #     
        jpdt(2)=jpd2   !Product # under this category     
        jpdt(27)=jpd27
        write(*,*) jpdtn,jpd1,jpd2,jpd27

         call getgb2(igrb2,ifile,jskp,jdisc,jids,jpdtn,jpdt,jgdtn,jgdt,
     +        unpck, jskp1, gfld,iret)

         
        return
        end 
         
