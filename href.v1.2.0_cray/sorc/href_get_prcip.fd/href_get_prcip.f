C  raw data
       use grib_mod
       real,allocatable,dimension(:,:) :: dp3 !jf,4        
       real,allocatable,dimension(:) :: dp6,dp12,dp24 !jf         
       integer iyr,imon,idy,ihr
       character*50 gdss(400)
       integer IENS, GRIBID, kgdss(200), lengds,im,jm,km,jf
       character*40 filehead,filename(8), output
       
       integer ff
       character*3 fhr(12)
       integer iunit,ounit
       type(gribfield) :: gfld,gfld_save

       data (fhr(i),i=1,12)
     + /'f03','f06','f09','f12','f15','f18','f21',
     +  'f24','f27','f30','f33','f36'/
 
       GRIBID=227            !namnest grid
       read (*,*) filehead, ff

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

        jpd1=1
        jpd2=8
        jpd27=3 !3 hr accumulation
        call readGB2(iunit,jpdtn,jpd1,jpd2,jpd27,gfld,ie)  !Large scale APCP
        if (ie.eq.0) then
         dp3(:,nf)=gfld%fld(:)
         if (nf.eq.1) then 
           gfld_save=gfld
           do i=1,gfld_save%ipdtlen
            write(*,*) i, gfld_save%ipdtmpl(i)
           end do
         end if
        else
         write(*,*) 'readGB2 error=',ie
        end if

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
c

c      If a field not in a GRIB2 file, getGB2 output gfld will be crashed. 
c      so use previously saved gfld_save 

       nff=ff/3      
       output='prcip'//filehead(5:14)//fhr(nff)

        ounit=50+nfhr
        call baopen(ounit,output,ierr)

          if(ff.ge.24) then

             gfld_save%fld(:)=dp3(:,1)
             gfld_save%ipdtmpl(27)=3
             call putgb2(ounit,gfld_save,ierr)

             gfld_save%fld(:)=dp6(:)
             gfld_save%ipdtmpl(27)=6
             call putgb2(ounit,gfld_save,ierr)

             gfld_save%fld(:)=dp12(:)
             gfld_save%ipdtmpl(27)=12
             call putgb2(ounit,gfld_save,ierr)

             gfld_save%fld(:)=dp24(:)
             gfld_save%ipdtmpl(27)=24
             call putgb2(ounit,gfld_save,ierr)

          else if (ff.lt.24.and.ff.ge.12) then

             gfld_save%fld(:)=dp3(:,1)
             gfld_save%ipdtmpl(27)=3
             call putgb2(ounit,gfld_save,ierr)

             gfld_save%fld(:)=dp6(:)
             gfld_save%ipdtmpl(27)=6
             call putgb2(ounit,gfld_save,ierr)

             gfld_save%fld(:)=dp12(:)
             gfld_save%ipdtmpl(27)=12
             call putgb2(ounit,gfld_save,ierr)

          else if (ff.lt.12.and.ff.ge.6) then

             gfld_save%fld(:)=dp3(:,1)
             gfld_save%ipdtmpl(27)=3
             call putgb2(ounit,gfld_save,ierr)

             gfld_save%fld(:)=dp6(:)
             gfld_save%ipdtmpl(27)=6       
             call putgb2(ounit,gfld_save,ierr)

           else

             gfld_save%fld(:)=dp3(:,1)
             gfld_save%ipdtmpl(27)=3
             call putgb2(ounit,gfld_save,ierr)

          end if
    
        write(*,*) 'Pack APCP done for fhr',nfhr

        call baclose(ounit,ierr) 

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
         
