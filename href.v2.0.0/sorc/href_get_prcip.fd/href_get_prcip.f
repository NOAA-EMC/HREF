C  raw data
       use grib_mod
       real,allocatable,dimension(:,:) :: dp3 !jf,4        
       real,allocatable,dimension(:) :: dp6,dp12,dp24 !jf         
       integer iyr,imon,idy,ihr
       character*50 gdss(400)
       integer IENS, GRIBID, kgdss(200), lengds,im,jm,km,jf
       character*40 filehead,filename(8), output, outdone
       
       integer ff
       character*3 fhr(12)
       integer iunit,ounit, pdt9_orig
       type(gribfield) :: gfld,gfld_save

       data (fhr(i),i=1,12)
     + /'f03','f06','f09','f12','f15','f18','f21',
     +  'f24','f27','f30','f33','f36'/
 
       GRIBID=227            !namnest grid
       read (*,*) filehead, ff

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

       allocate(dp3(jf,8))
       allocate(dp6(jf))
       allocate(dp12(jf))
       allocate(dp24(jf))

       if (ff.ge.24) then
         nfile=8
       else if (ff.lt.24.and.ff.ge.12) then
         nfile=4
       else if (ff.lt.12.and.ff.ge.6) then
         nfile=2
       else
         nfile=1 
       end if
 
       nff=ff/3
       do 1000 nf=1,nfile
        
        filename(nf)=filehead(1:14)//fhr(nff)

        iunit=20+nf

        jpdtn=8    !APCP's Product Template# is  4.8 

        call baopenr(iunit,filename(nf),ierr)
        write(*,*) 'open ', filename(nf), 'ierr=',ierr

	if (ierr .eq. 0) then

        jpd1=1
        jpd2=8
        jpd27=3 !3 hr accumulation
        call readGB2(iunit,jpdtn,jpd1,jpd2,jpd27,gfld,ie)  !Large scale APCP
        if (ie.eq.0) then
	write(0,*) 'populate nf of nfile: ', nf, nfile
         dp3(:,nf)=gfld%fld(:)
         if (nf.eq.1) then 
           gfld_save=gfld
	write(0,*) 'gfld_save%ipdtmpl(9) when saved: ', gfld_save%ipdtmpl(9)
	pdt9_orig=gfld_save%ipdtmpl(9)
           do i=1,gfld_save%ipdtlen
            write(*,*) i, gfld_save%ipdtmpl(i)
           end do
         end if
        else
         write(*,*) '3h readGB2 error=',ie
        end if

	endif

        call baclose(iunit,ierr)
        write(*,*) 'close ', filename(nf), 'ierr=',ierr
        nff=nff-1

1000  continue

        dp6=0.0
        dp24=0.0
        dp12=0.0


       if (ff.ge.24) then
         dp6(:)=dp3(:,1)+dp3(:,2)
         dp12(:)=dp6(:)+dp3(:,3)+dp3(:,4)
         dp24(:)=dp12(:)+dp3(:,5)+dp3(:,6)+dp3(:,7)+dp3(:,8)
       else if (ff.lt.24.and.ff.ge.12) then
         dp6(:)=dp3(:,1)+dp3(:,2)
         dp12(:)=dp6(:)+dp3(:,3)+dp3(:,4)
       else if (ff.lt.12.and.ff.ge.6) then
	write(0,*) 'adding to create dp6'
	write(0,*) 'maxvals of dp3 inputs: ', 
     &          maxval(dp3(:,1)),maxval(dp3(:,2))
         dp6(:)=dp3(:,1)+dp3(:,2)
       end if
            
       do i=382461,382470
        write(*,'(i10,11f8.2)')i,(dp3(i,k),k=1,8),
     +         dp6(i),dp12(i),dp24(i)                                   
       end do


cccccc  Then call putgb2 to store the calculated data into a grib2 file
c
c      data structure gfld is re-used for pack data since all are same
c      only gfld%fld and gfld%ipdtmpl(27) are different

Cmp   believe gfld%ipdtmpl(9) matters as well
c

c      If a field not in a GRIB2 file, getGB2 output gfld will be crashed. 
c      so use previously saved gfld_save 

       nff=ff/3      
       output='prcip'//filehead(5:14)//fhr(nff)
       outdone='prcipdone'//filehead(9:14)//fhr(nff)

        ounit=50+nfhr
        call baopen(ounit,output,ierr)

!!! believe this might not be correct
!!!
!!!
!!! GEMPAK cannot unpack the resulting prcip file completely...date information in PDS
!!! not correct?  Wants everything to be 3 h totals


	write(0,*) 'setting gfld to gfld_save'

	gfld=gfld_save

          if(ff.ge.24) then

             gfld%fld(:)=dp3(:,1)
             gfld%ipdtmpl(27)=3
             call putgb2(ounit,gfld,ierr)

             gfld%fld(:)=dp6(:)
!	write(0,*) 'dp6 gfld%ipdtmpl(9): ', gfld%ipdtmpl(9)
             gfld%ipdtmpl(27)=6
             gfld%ipdtmpl(9)=-3 + pdt9_orig
	write(0,*) 'dp6 gfld%ipdtmpl(9) now: ', gfld%ipdtmpl(9)
             call putgb2(ounit,gfld,ierr)

             gfld%fld(:)=dp12(:)
             gfld%ipdtmpl(27)=12
!	write(0,*) 'dp12 gfld%ipdtmpl(9): ', gfld%ipdtmpl(9)
!	write(0,*) 'dp12 tmpl(16-19): ', gfld%ipdtmpl(16:19)
             gfld%ipdtmpl(9)=-9 + pdt9_orig
	write(0,*) 'gfld%ipdtmpl(9) for dp12 now: ', gfld%ipdtmpl(9)
             call putgb2(ounit,gfld,ierr)

             gfld%fld(:)=dp24(:)
             gfld%ipdtmpl(27)=24
!	write(0,*) 'dp24 gfld%ipdtmpl(9): ', gfld%ipdtmpl(9)
!	write(0,*) 'dp24 tmpl(16-19): ', gfld%ipdtmpl(16:19)
             gfld%ipdtmpl(9)=-21+pdt9_orig
	write(0,*) 'dp24 gfld%ipdtmpl(9) now: ', gfld%ipdtmpl(9)
             call putgb2(ounit,gfld,ierr)

          else if (ff.lt.24.and.ff.ge.12) then

             gfld%fld(:)=dp3(:,1)
             gfld%ipdtmpl(27)=3
             call putgb2(ounit,gfld,ierr)

             gfld%fld(:)=dp6(:)
             gfld%ipdtmpl(27)=6
             gfld%ipdtmpl(9)=-3+pdt9_orig
	write(0,*) 'dp6 gfld%ipdtmpl(9) now: ', gfld%ipdtmpl(9)
             call putgb2(ounit,gfld,ierr)

             gfld%fld(:)=dp12(:)
             gfld%ipdtmpl(27)=12
!	write(0,*) 'dp12(b) gfld%ipdtmpl(9): ', gfld%ipdtmpl(9)
!	write(0,*) 'dp12(b) tmpl(16-19): ', gfld%ipdtmpl(16:19)
             gfld%ipdtmpl(9)=-9+pdt9_orig
	write(0,*) 'dp12(b) gfld%ipdtmpl(9) now: ', gfld%ipdtmpl(9)
             call putgb2(ounit,gfld,ierr)

          else if (ff.lt.12.and.ff.ge.6) then

             gfld%fld(:)=dp3(:,1)
             gfld%ipdtmpl(27)=3
             call putgb2(ounit,gfld,ierr)

             gfld%fld(:)=dp6(:)
             gfld%ipdtmpl(27)=6       
             gfld%ipdtmpl(9)=-3+pdt9_orig
	write(0,*) 'dp6(b) gfld%ipdtmpl(9) now: ', gfld%ipdtmpl(9)
             call putgb2(ounit,gfld,ierr)

           else

             gfld%fld(:)=dp3(:,1)
             gfld%ipdtmpl(27)=3
             call putgb2(ounit,gfld,ierr)

          end if
    
        write(0,*) 'Pack APCP done for fhr',nfhr

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
         
