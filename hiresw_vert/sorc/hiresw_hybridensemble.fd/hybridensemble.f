       program hybrid_ensemble

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c This program reads in a low-res ensemble data and hi-res single
c model forecasts and combine them into a new hybrid ensemble based 
c on "Hybrid Ensembling concept" (Du 2004, preprint of the 50th NWP
c Symposium, University of Maryland, College Park, MD, June 14-17,
c 2004, paper P4.2, 5pp, it is available online at 
c http://wwwt.emc.ncep.noaa.gov/mmb/SREF/reference.html
c
c Programmer: Jun Du
c 04/22/2004: initial program
c 04/17/2008: Jun Du - modified to combine Hires-Window runs and SREF
c 03/02/2010: Jun Du - modified for the newest SREF version and added 
c                      west-region domain for the Hanson Dam project
c 12/08/2010: M. pyle - modified some kpds(22) entries to pack more
c                       precision in output files, and to use
c                       Eta type SLP reduction (#130) where available
c 02/17/2012: Jun Du - modified for the new SREFv6.0.0 by adding NMMB
c                      model, getting rid of Eta and RSM models, and 
c                      adjusting membership
c
c cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 2-dimension variable list (the order needs to be followed):
c 1.  t2m
c 2.  u10m
c 3.  v10m
c 4.  rh2m
c 5.  slp
c 6.  precip
c 7.  cape
c 8.  cin
c 9.  500H
c 10  850RH
c 11  850U
c 12  850V
c 13  250U
c 14  250V
c 15  850T
c 16  500T
c 17  250T
c 18  q2m
c 19  sfcp
c 20  sfch

       parameter(im=884,jm=614,jfm=im*jm,nt=41,nv=20)   !maximum dimension
c      parameter(im=884,jm=614,jfm=im*jm,nt=17,nv=20)   !maximum dimension, 3hrly
C      1-7(nmb),8-14(nmm),15-21(arw),22(hi-res nmm),23(hi-res arw)
       parameter(mbase=2,mem=21,iens=mem+mbase)
       parameter(newmem=mbase*(mem+1))
C raw ensemble fcsts
       real var(jfm),var_raw(jfm,iens,nv),var_mean(jfm,nv),
     & ensmean(jfm,nv)
C hybrid ensemble fcsts
       real outdata(jfm),var_new(jfm,nv)

       integer, parameter:: wind_prec=2
       integer, parameter:: temp_prec=2
       integer, parameter:: hght_prec=2
       integer, parameter:: cape_prec=2

       dimension jpds(25),jgds(25),kpds(25),kgds(25)
       character*2 pert
       character*2 hr
       character*17 fname
       character*19 fout
       logical*1 lb(jfm)
       character*3 dywk(7),month(12)
       integer days(12)
       data dywk/'Sun','Mon','Tue','Wed','Thu','Fri','Sat'/
       data month/'jan','feb','mar','apr','may','jun','jul',
     *            'aug','sep','oct','nov','dec'/
       data days/31,28,31,30,31,30,31,31,30,31,30,31/


c Passing over date information
       namelist/namin/iyr,imon,idy,ihr,id_grid,itime

       read(11,namin,end=1000)
       imin=0
1000   continue
       print*, iyr,imon,idy,ihr,imin,id_grid



C redefine data dimension
       if(id_grid.eq.256) then
         i=884                 !HiresWindow (USeast and USwest)
         j=614
         jf=i*j
       endif
       if(id_grid.eq.257) then
         i=825                 !HiresWindow (AK)
         j=603
         jf=i*j
       endif
       if(id_grid.eq.258) then
         i=223                 !HiresWindow (HI and PR)
         j=170
         jf=i*j
       endif
       if(id_grid.eq.259) then
         i=340                 !HiresWindow (HaitiPR)
         j=208
         jf=i*j
       endif

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c read in raw ensemble fcsts data
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       print*,'About to enter fcst time loop'
	print*, ' id_grid, jf: ', id_grid, jf
!       do 111 i00=1,nt                    !number of fcst files
!	write(6,*) 'starting loop 111 for i00: ', i00
c       itime=(i00-1)*3                   !00,03,06,....,48 (01->17). 3-hourly
!        if(i00.le.37) itime=i00-1         !00,01,02,....,36 (01->37). 1-hourly
!        if(i00.gt.37) itime=36+(i00-37)*3 !39,42,45,48      (38->41). 3-hourly

c initialization (otherwise to use previous hour forecast variance if ensemble data doesn't exist
c such as AK grid)
       do i=1,nv
        do irun=1,iens
         do igrid=1,jf
          var_raw(igrid,irun,i)=0.0
         enddo
        enddo
       enddo


       do 222 irun=1,iens                       ! perturbation
       print*,'Ensemble number ',irun
       iunit=10
       write(pert,'(i2.2)') irun
       write(hr,'(i2.2)') itime
       fname='r_gribawips'//pert//'.f'//hr
       print*,'Opening ',fname
       call baopenr(iunit,fname,ierr)
       print*,'itime=',itime
c
       iseek=0
       call skgb(iunit,iseek,llgrib,llskip)
       do while(llgrib.gt.0)  ! llgrib 
        call rdgb(iunit,llgrib,llskip,kpds,kgds,kf,lb,var)
        call skgb(iunit,iseek,llgrib,llskip)

C 2m temperature:
      if(kpds(5).eq.11.and.kpds(6).eq.105.and.kpds(7).eq.2) then
       do igrid=1,jf
        var_raw(igrid,irun,1)=var(igrid)
       enddo
        print*,'t2m(10,1)=',var_raw(10,irun,1)
      endif
C 10m u-component:
      if(kpds(5).eq.33.and.kpds(6).eq.105.and.kpds(7).eq.10) then
       do igrid=1,jf
        var_raw(igrid,irun,2)=var(igrid)
       enddo
        print*,'u10m(10,1)=',var_raw(10,irun,2)
      endif
C 10m v-component:
      if(kpds(5).eq.34.and.kpds(6).eq.105.and.kpds(7).eq.10) then
       do igrid=1,jf
        var_raw(igrid,irun,3)=var(igrid)
       enddo
        print*,'v10m(10,1)=',var_raw(10,irun,3)
      endif
C 2m relative humidity:
      if(kpds(5).eq.52.and.kpds(6).eq.105.and.kpds(7).eq.2) then
       do igrid=1,jf
        var_raw(igrid,irun,4)=var(igrid)
       enddo
        print*,'rh2m(10,1)=',var_raw(10,irun,4)
      endif
C sea level pressure:
      if( (kpds(5).eq.2. .or. kpds(5).eq.130).and.kpds(6).eq.102
     +        .and.kpds(7).eq.0) then
       do igrid=1,jf
        var_raw(igrid,irun,5)=var(igrid)
       enddo
        write(0,*) 'irun, slp(10,1)=',irun, var_raw(10,irun,5)
      endif
C accumulated total precipitation:
      if(kpds(5).eq.61.and.kpds(6).eq.1.and.kpds(7).eq.0) then
       do igrid=1,jf
c 3hrly apcp
       if(itime.eq.03.or.itime.eq.06.or.itime.eq.09.or.itime.eq.12
     &.or.itime.eq.15.or.itime.eq.18.or.itime.eq.21.or.itime.eq.24
     &.or.itime.eq.27.or.itime.eq.30.or.itime.eq.33.or.itime.eq.36
     &.or.itime.eq.39.or.itime.eq.42.or.itime.eq.45.or.itime.eq.48) 
     & var_raw(igrid,irun,6)=var(igrid)  
c 1hrly apcp
       if(itime.eq.02.or.itime.eq.05.or.itime.eq.08.or.itime.eq.11
     &.or.itime.eq.14.or.itime.eq.17.or.itime.eq.20.or.itime.eq.23
     &.or.itime.eq.26.or.itime.eq.29.or.itime.eq.32.or.itime.eq.35)
     & then
        if(irun.ge.1.and.irun.le.7) var_raw(igrid,irun,6)=var(igrid)/2.0   !nmb
        if(irun.ge.8.and.irun.le.14) var_raw(igrid,irun,6)=var(igrid)/2.0  !nmm
        if(irun.ge.15.and.irun.le.21) var_raw(igrid,irun,6)=var(igrid)/3.0 !em
        if(irun.ge.22) var_raw(igrid,irun,6)=var(igrid)/2.0                !hi-res nmm
        if(irun.ge.23) var_raw(igrid,irun,6)=var(igrid)                    !hi-res arw
       endif
c 1hrly apcp
       if(itime.eq.01.or.itime.eq.04.or.itime.eq.07.or.itime.eq.10
     &.or.itime.eq.13.or.itime.eq.16.or.itime.eq.19.or.itime.eq.22
     &.or.itime.eq.25.or.itime.eq.28.or.itime.eq.31.or.itime.eq.34)
     & then
        if(irun.ge.1.and.irun.le.7) var_raw(igrid,irun,6)=var(igrid)       !nmb
        if(irun.ge.8.and.irun.le.14) var_raw(igrid,irun,6)=var(igrid)      !nmm
        if(irun.ge.15.and.irun.le.21) var_raw(igrid,irun,6)=var(igrid)/3.0 !em
        if(irun.ge.22) var_raw(igrid,irun,6)=var(igrid)                    !hi-res nmm
        if(irun.ge.23) var_raw(igrid,irun,6)=var(igrid)                    !hi-res arw
       endif
       enddo
       if(itime.eq.10) print*,'1h-apcp(3000,1)=',var_raw(3000,irun,6)
       if(itime.eq.11) print*,'1h-apcp(3000,1)=',var_raw(3000,irun,6)
       if(itime.eq.12) print*,'3h-apcp(3000,1)=',var_raw(3000,irun,6)

       write(0,*) 'maxval var_raw(:,irun,6): ',
     +      irun,maxval(var_raw(:,irun,6))
      endif
C cape (convective available potential energy):
      if(kpds(5).eq.157.and.kpds(6).eq.1.and.kpds(7).eq.0) then
       do igrid=1,jf
         var_raw(igrid,irun,7)=var(igrid)
       enddo
        print*,'cape(10,1)=',var_raw(10,irun,7)
       endif
C cin (convective inhibition):
      if(kpds(5).eq.156.and.kpds(6).eq.1.and.kpds(7).eq.0) then
       do igrid=1,jf
         var_raw(igrid,irun,8)=var(igrid)
        enddo
        print*,'cin(10,1)=',var_raw(10,irun,8)
      endif
C 500mb geopotential height:
       if(kpds(5).eq.7.and.kpds(6).eq.100.and.kpds(7).eq.500) then
        do igrid=1,jf
         var_raw(igrid,irun,9)=var(igrid) 
        enddo
         print*,'SREF 500h(jf/2,1)=',jf, irun, var_raw(jf/2,irun,9)
       endif
C 850mb relative humidity:
       if(kpds(5).eq.52.and.kpds(6).eq.100.and.kpds(7).eq.850) then
        do igrid=1,jf
         var_raw(igrid,irun,10)=var(igrid)
        enddo
         print*,'850rh(10,1)=',var_raw(10,irun,10)
       endif
C 850mb u-component:
       if(kpds(5).eq.33.and.kpds(6).eq.100.and.kpds(7).eq.850) then
        do igrid=1,jf
         var_raw(igrid,irun,11)=var(igrid)
        enddo
         print*,'850u(10,1)=',var_raw(10,irun,11)
       endif
C 850mb v-component:
       if(kpds(5).eq.34.and.kpds(6).eq.100.and.kpds(7).eq.850) then
        do igrid=1,jf
         var_raw(igrid,irun,12)=var(igrid)
        enddo
         print*,'850v(10,1)=',var_raw(10,irun,12)
       endif
C 250mb u-component:
       if(kpds(5).eq.33.and.kpds(6).eq.100.and.kpds(7).eq.250) then
        do igrid=1,jf
         var_raw(igrid,irun,13)=var(igrid)
        enddo
         print*,'250u(10,1)=',var_raw(10,irun,13)
       endif
C 250mb v-component:
       if(kpds(5).eq.34.and.kpds(6).eq.100.and.kpds(7).eq.250) then
        do igrid=1,jf
         var_raw(igrid,irun,14)=var(igrid)
        enddo
         print*,'250v(10,1)=',var_raw(10,irun,14)
       endif
c 850mb temperature:
       if(kpds(5).eq.11.and.kpds(6).eq.100.and.kpds(7).eq.850) then
        do igrid=1,jf
         var_raw(igrid,irun,15)=var(igrid)
        enddo
         print*,'850t(10,1)=',var_raw(10,irun,15)
       endif
c 500mb temperature:
       if(kpds(5).eq.11.and.kpds(6).eq.100.and.kpds(7).eq.500) then
        do igrid=1,jf
         var_raw(igrid,irun,16)=var(igrid)
        enddo
         print*,'500t(10,1)=',var_raw(10,irun,16)
       endif
c 250mb temperature:
       if(kpds(5).eq.11.and.kpds(6).eq.100.and.kpds(7).eq.250) then
        do igrid=1,jf
         var_raw(igrid,irun,17)=var(igrid)
        enddo
         print*,'250t(10,1)=',var_raw(10,irun,17)
       endif
C 2m specific humidity:
      if(kpds(5).eq.51.and.kpds(6).eq.105.and.kpds(7).eq.2) then
       do igrid=1,jf
        var_raw(igrid,irun,18)=var(igrid)
       enddo
        print*,'q2m(10,1)=',var_raw(10,irun,18)
      endif
C sfc pressure:
      if(kpds(5).eq.1.and.kpds(6).eq.1.and.kpds(7).eq.0) then
       do igrid=1,jf
        var_raw(igrid,irun,19)=var(igrid)
       enddo
        print*,'sfcp(10,1)=',var_raw(10,irun,19)/100.0
      endif
C sfc height:
      if(kpds(5).eq.7.and.kpds(6).eq.1.and.kpds(7).eq.0) then
       do igrid=1,jf
        var_raw(igrid,irun,20)=var(igrid)
       enddo
        print*,'sfch(10,1)=',var_raw(10,irun,20)
      endif
c-----------------------------------------
      enddo        !llgrib
      call baclose(iunit,ierr)

222   continue     !irun

C#########################################
C find ensemble mean
      var_mean=0.0
        write(0,*) 'num vars ,num members ', nv, mem
      do i=1,nv
       do irun=1,mem      
        do igrid=1,jf       

        if (i .eq. 9 .and. igrid .eq. 10) then
        write(0,*) 'adding var_raw: ',irun,mem,var_raw(igrid,irun,i)
        endif

         var_mean(igrid,i)=var_mean(igrid,i)+
     &   var_raw(igrid,irun,i)/mem

        if (i .eq. 9 .and. igrid .eq. 10) then
        write(0,*) 'var_mean(igrid,i) now: ',irun,mem,var_mean(igrid,i)
        endif

        enddo
       enddo

       if(i.eq.1) print*,'mean T2m=',var_mean(10,i)
       if(i.eq.2) print*,'mean U10m=',var_mean(10,i)
       if(i.eq.3) print*,'mean V10m=',var_mean(10,i)
       if(i.eq.4) print*,'mean RH2m=',var_mean(10,i)
       if(i.eq.5) print*,'mean SLP=',var_mean(10,i)
       if(i.eq.6) print*,'mean precip=',var_mean(10,i)
       if(i.eq.7) print*,'mean cape=',var_mean(10,i)
       if(i.eq.8) print*,'mean cin=',var_mean(10,i)
       if(i.eq.9) print*,'mean 500H=',var_mean(jf/2,i)
       if(i.eq.10) print*,'mean 850RH=',var_mean(10,i)
       if(i.eq.11) print*,'mean 850U=',var_mean(10,i)
       if(i.eq.12) print*,'mean 850V=',var_mean(10,i)
       if(i.eq.13) print*,'mean 250U=',var_mean(10,i)
       if(i.eq.14) print*,'mean 250V=',var_mean(10,i)
       if(i.eq.15) print*,'mean 850T=',var_mean(10,i)
       if(i.eq.16) print*,'mean 500T=',var_mean(10,i)
       if(i.eq.17) print*,'mean 250T=',var_mean(10,i)
       if(i.eq.18) print*,'mean q2m=',var_mean(10,i)
       if(i.eq.19) print*,'mean sfcp=',var_mean(10,i)
       if(i.eq.20) print*,'mean sfch=',var_mean(10,i)

       if(i.eq.1) print*,'indiv T2m=',var_raw(10,2,i)
       if(i.eq.2) print*,'indiv U10m=',var_raw(10,2,i)
       if(i.eq.3) print*,'indiv V10m=',var_raw(10,2,i)
       if(i.eq.4) print*,'indiv RH2m=',var_raw(10,2,i)
       if(i.eq.5) print*,'indiv SLP=',var_raw(10,2,i)
       if(i.eq.6) print*,'indiv precip=',var_raw(10,2,i)
       if(i.eq.7) print*,'indiv cape=',var_raw(10,2,i)
       if(i.eq.8) print*,'indiv cin=',var_raw(10,2,i)
       if(i.eq.9) print*,'indiv 500H=',var_raw(jf/2,2,i)
       if(i.eq.10) print*,'indiv 850RH=',var_raw(10,2,i)
       if(i.eq.11) print*,'indiv 850U=',var_raw(10,2,i)
       if(i.eq.12) print*,'indiv 850V=',var_raw(10,2,i)
       if(i.eq.13) print*,'indiv 250U=',var_raw(10,2,i)
       if(i.eq.14) print*,'indiv 250V=',var_raw(10,2,i)
       if(i.eq.15) print*,'indiv 850T=',var_raw(10,2,i)
       if(i.eq.16) print*,'indiv 500T=',var_raw(10,2,i)
       if(i.eq.17) print*,'indiv 250T=',var_raw(10,2,i)
       if(i.eq.18) print*,'indiv q2m=',var_raw(10,2,i)
       if(i.eq.19) print*,'indiv sfcp=',var_raw(10,2,i)
       if(i.eq.20) print*,'indiv sfch=',var_raw(10,2,i)
	
      enddo


C#########################################
c Calculate hybrid new ensemble fcsts by massaging hi-res base runs with low-res ens spread info
      ensmean=0.0
      do 444 m=1,mbase               !number of base models
c     do 444 m=1,1                   !for "keep low-res ensemble" test
       mstart=(m-1)*(mem+1)+1
       mend=m*(mem+1)  !tot new mem = num of base modl x orig ens size + num of base modl
      do 333 irun=mstart,mend 
        print*,'irun=',irun
      do igrid=1,jf                  !grid point

      if(itime.eq.0) then   
c force all 0hr fcsts equal to hires base fcsts
       do i=1,nv
        var_new(igrid,i)=var_raw(igrid,mem+m,i)
       enddo
      else
       do i=1,nv
        base=var_raw(igrid,mem+m,i)    
        if(irun.eq.mend) var_new(igrid,i)=base   !the last mem within each base model group is the base itself

C add low-res ens "spread" to hires run (using difference from ens mean, which contains both IC and model diversity)
        if(irun.ne.mend) var_new(igrid,i)=base+
     &  var_raw(igrid,irun-(m-1)*(mem+1),i)-var_mean(igrid,i)  !adding apread on top of hires base

        if (igrid .eq. jf/2 .and. i .eq. 9 .and. irun .ne. mend) then
      write(0,*) 'irun,mem,var_raw index:',irun, mem, irun-(m-1)*(mem+1)
        write(0,*) 'mem+m, base,var_raw, var_mean, var_new: ', mem+m,  
     &    base,var_raw(igrid,irun-(m-1)*(mem+1),i), var_mean(igrid,i), 
     &    var_new(igrid,i)
        endif

C add difference between hires and low-res members to low-res ens mean
c       if(irun.ne.mend) var_new(igrid,i)=var_mean(igrid,i)+
c    &  base-var_raw(igrid,irun-(m-1)*(mem+1),i)       !adding details on top of low-res mean

C keep low-res ensemble
c       if(irun.ge.1.and.irun.le.mem) var_new(igrid,i)=
c    &  var_raw(igrid,irun,i)

C consider model difference (nmb-nmm,nmb-arw,nmm-arw) to have larger spread
c       if(irun.ge.1.and.irun.le.7) var_new(igrid,i)=base+
c    &  (var_raw(igrid,irun,i)-var_raw(igrid,irun+7,i))   !plus nmb-nmm
c       if(irun.ge.8.and.irun.le.14) var_new(igrid,i)=base+
c    &  (var_raw(igrid,irun-7,i)-var_raw(igrid,irun+7,i)) !plus nmb-arw
c       if(irun.ge.15.and.irun.le.21) var_new(igrid,i)=base+
c    &  (var_raw(igrid,irun-7,i)-var_raw(igrid,irun,i))   !plus nmm-arw
c mem 22 is hiresw-nmm itself

c       if(irun.ge.23.and.irun.le.29) var_new(igrid,i)=base-
c    &  (var_raw(igrid,irun-22,i)-var_raw(igrid,irun-15,i))   !minus nmb-nmm
c       if(irun.ge.30.and.irun.le.36) var_new(igrid,i)=base-
c    &  (var_raw(igrid,irun-29,i)-var_raw(igrid,irun-15,i))   !minus nmb-arw
c       if(irun.ge.37.and.irun.le.43) var_new(igrid,i)=base-
c    &  (var_raw(igrid,irun-29,i)-var_raw(igrid,irun-22,i))   !minus nmm-arw
c mem 44 is hiresw-arw itself
       enddo
      endif   !3hr fcst

      enddo   !grid points

C#########################################
c  write out new ensemble fcsts in GRIB format
      write(pert,'(i2.2)') irun
      write(hr,'(i2.2)') itime
      fout='hybd_pgrb212.'//pert//'.f'//hr
      call baopen(40+irun,fout,ierr)
 
      kpds(1)=7
      kpds(2)=130
      kpds(3)=255
      kpds(4)=0 
c     kpds(5)=variable dependent
c     kpds(6)=variable dependent
c     kpds(7)=variable dependent
      kpds(8)=iyr-(iyr/100)*100
      kpds(9)=imon
      kpds(10)=idy
      kpds(11)=ihr
      kpds(12)=0
      kpds(13)=1
c     kpds(14)=variable dependent
c     kpds(15)=variable dependent
c     kpds(16)=variable dependent
      kpds(17)=-1
      kpds(18)=1
      kpds(19)=2
      kpds(20)=-1
      kpds(21)=21
c     kpds(22)=variable dependent (units decimal scale factor/precision)
c     kpds(23)=2  !if ensemble data
      kpds(23)=0  !if not ensemble data
      kpds(24)=128
      kpds(25)=-1

      kgds(20)=255

C-----------------t2m----------------------
      kpds(5)=11
      kpds(6)=105
      kpds(7)=2
      kpds(14)=itime
      kpds(15)=0
      kpds(16)=0
      kpds(22)=temp_prec
      do igrid=1,jf
       outdata(igrid)=var_new(igrid,1)
       ensmean(igrid,1)=ensmean(igrid,1)+outdata(igrid)
      enddo
      call putgb(40+irun,jf,kpds,kgds,lb,outdata,iret)
C-----------------u10m----------------------
      kpds(5)=33
      kpds(6)=105
      kpds(7)=10
      kpds(14)=itime
      kpds(15)=0
      kpds(16)=0
      kpds(22)=wind_prec
      do igrid=1,jf
       outdata(igrid)=var_new(igrid,2)
       ensmean(igrid,2)=ensmean(igrid,2)+outdata(igrid)
      enddo
      call putgb(40+irun,jf,kpds,kgds,lb,outdata,iret)
C-----------------v10m----------------------
      kpds(5)=34
      kpds(6)=105
      kpds(7)=10
      kpds(14)=itime
      kpds(15)=0
      kpds(16)=0
      kpds(22)=wind_prec
      do igrid=1,jf
       outdata(igrid)=var_new(igrid,3)
       ensmean(igrid,3)=ensmean(igrid,3)+outdata(igrid)
      enddo
      call putgb(40+irun,jf,kpds,kgds,lb,outdata,iret)
C-----------------rh2m----------------------
      kpds(5)=52
      kpds(6)=105
      kpds(7)=2
      kpds(14)=itime
      kpds(15)=0
      kpds(16)=0
      kpds(22)=4
      do igrid=1,jf
       if(var_new(igrid,4).lt.0.0) var_new(igrid,4)=0.0
       if(var_new(igrid,4).gt.100.0) var_new(igrid,4)=100.0
       outdata(igrid)=var_new(igrid,4)
       ensmean(igrid,4)=ensmean(igrid,4)+outdata(igrid)
      enddo
      call putgb(40+irun,jf,kpds,kgds,lb,outdata,iret)
C-----------------slp----------------------
      kpds(5)=130
      kpds(6)=102
      kpds(7)=0
      kpds(14)=itime
      kpds(15)=0
      kpds(16)=0
      kpds(22)=2
      do igrid=1,jf
       outdata(igrid)=var_new(igrid,5)
       ensmean(igrid,5)=ensmean(igrid,5)+outdata(igrid)
      enddo
      call putgb(40+irun,jf,kpds,kgds,lb,outdata,iret)
C-----------1h and 3h apcp----------------------
      if(itime.ge.01) then
       if(itime.eq.03.or.itime.eq.06.or.itime.eq.09.or.itime.eq.12
     &.or.itime.eq.15.or.itime.eq.18.or.itime.eq.21.or.itime.eq.24
     &.or.itime.eq.27.or.itime.eq.30.or.itime.eq.33.or.itime.eq.36
     &.or.itime.eq.39.or.itime.eq.42.or.itime.eq.45.or.itime.eq.48)
     & then
        kpds(14)=itime-3
       else
        kpds(14)=itime-1
       endif
      kpds(5)=61
      kpds(6)=1
      kpds(7)=0
      kpds(15)=itime
      kpds(16)=4
      kpds(22)=4
      do igrid=1,jf
       if(var_new(igrid,6).lt.0.0) var_new(igrid,6)=0.0
       outdata(igrid)=var_new(igrid,6)
       ensmean(igrid,6)=ensmean(igrid,6)+outdata(igrid)
      enddo
      call putgb(40+irun,jf,kpds,kgds,lb,outdata,iret)
      endif
C------------cape---------------------------
      kpds(5)=157
      kpds(6)=1
      kpds(7)=0
      kpds(14)=itime
      kpds(15)=0
      kpds(16)=0
      kpds(22)=cape_prec
      do igrid=1,jf
       if(var_new(igrid,7).lt.0.0) var_new(igrid,7)=0.0
       outdata(igrid)=var_new(igrid,7)
       ensmean(igrid,7)=ensmean(igrid,7)+outdata(igrid)
      enddo
      call putgb(40+irun,jf,kpds,kgds,lb,outdata,iret)
C---------------cin------------------------
      kpds(5)=156
      kpds(6)=1
      kpds(7)=0
      kpds(14)=itime
      kpds(15)=0
      kpds(16)=0
      kpds(22)=cape_prec
      do igrid=1,jf
       outdata(igrid)=var_new(igrid,8)
       ensmean(igrid,8)=ensmean(igrid,8)+outdata(igrid)
      enddo
      call putgb(40+irun,jf,kpds,kgds,lb,outdata,iret)
C-----------------500H-----------------------
      kpds(5)=7
      kpds(6)=100
      kpds(7)=500
      kpds(14)=itime
      kpds(15)=0
      kpds(16)=0
      kpds(22)=hght_prec
      do igrid=1,jf
       outdata(igrid)=var_new(igrid,9)
       ensmean(igrid,9)=ensmean(igrid,9)+outdata(igrid)
      enddo
      call putgb(40+irun,jf,kpds,kgds,lb,outdata,iret)
C-----------------850rh----------------------
      kpds(5)=52
      kpds(6)=100
      kpds(7)=850
      kpds(14)=itime
      kpds(15)=0
      kpds(16)=0
      kpds(22)=4
      do igrid=1,jf
       if(var_new(igrid,10).lt.0.0) var_new(igrid,10)=0.0
       if(var_new(igrid,10).gt.100.0) var_new(igrid,10)=100.0
       outdata(igrid)=var_new(igrid,10)
       ensmean(igrid,10)=ensmean(igrid,10)+outdata(igrid)
      enddo
      call putgb(40+irun,jf,kpds,kgds,lb,outdata,iret)
C-------------850u--------------------------
      kpds(5)=33
      kpds(6)=100
      kpds(7)=850
      kpds(14)=itime
      kpds(15)=0
      kpds(16)=0
      kpds(22)=wind_prec
      do igrid=1,jf
       outdata(igrid)=var_new(igrid,11)
       ensmean(igrid,11)=ensmean(igrid,11)+outdata(igrid)
      enddo
      call putgb(40+irun,jf,kpds,kgds,lb,outdata,iret)
C-------------850v------------------------
      kpds(5)=34
      kpds(6)=100
      kpds(7)=850
      kpds(14)=itime
      kpds(15)=0
      kpds(16)=0
      kpds(22)=wind_prec
      do igrid=1,jf
       outdata(igrid)=var_new(igrid,12)
       ensmean(igrid,12)=ensmean(igrid,12)+outdata(igrid)
      enddo
      call putgb(40+irun,jf,kpds,kgds,lb,outdata,iret)
C-------------250u--------------------------
      kpds(5)=33
      kpds(6)=100
      kpds(7)=250
      kpds(14)=itime
      kpds(15)=0
      kpds(16)=0
      kpds(22)=wind_prec
      do igrid=1,jf
       outdata(igrid)=var_new(igrid,13)
       ensmean(igrid,13)=ensmean(igrid,13)+outdata(igrid)
      enddo
      call putgb(40+irun,jf,kpds,kgds,lb,outdata,iret)
C-------------250v------------------------
      kpds(5)=34
      kpds(6)=100
      kpds(7)=250
      kpds(14)=itime
      kpds(15)=0
      kpds(16)=0
      kpds(22)=wind_prec
      do igrid=1,jf
       outdata(igrid)=var_new(igrid,14)
       ensmean(igrid,14)=ensmean(igrid,14)+outdata(igrid)
      enddo
      call putgb(40+irun,jf,kpds,kgds,lb,outdata,iret)
C-------- 850 temp ---------------------------
      kpds(5)=11
      kpds(6)=100
      kpds(7)=850
      kpds(14)=itime
      kpds(15)=0
      kpds(16)=0
      kpds(22)=temp_prec
      do igrid=1,jf
       outdata(igrid)=var_new(igrid,15)
       ensmean(igrid,15)=ensmean(igrid,15)+outdata(igrid)
      enddo
      call putgb(40+irun,jf,kpds,kgds,lb,outdata,iret)
C-------- 500 temp ---------------------------
      kpds(5)=11
      kpds(6)=100
      kpds(7)=500
      kpds(14)=itime
      kpds(15)=0
      kpds(16)=0
      kpds(22)=temp_prec
      do igrid=1,jf
       outdata(igrid)=var_new(igrid,16)
       ensmean(igrid,16)=ensmean(igrid,16)+outdata(igrid)
      enddo
      call putgb(40+irun,jf,kpds,kgds,lb,outdata,iret)
C-------- 250 temp ---------------------------
      kpds(5)=11
      kpds(6)=100
      kpds(7)=250
      kpds(14)=itime
      kpds(15)=0
      kpds(16)=0
      kpds(22)=temp_prec
      do igrid=1,jf
       outdata(igrid)=var_new(igrid,17)
       ensmean(igrid,17)=ensmean(igrid,17)+outdata(igrid)
      enddo
      call putgb(40+irun,jf,kpds,kgds,lb,outdata,iret)
C-----------------q2m----------------------
      kpds(5)=51
      kpds(6)=105
      kpds(7)=2
      kpds(14)=itime
      kpds(15)=0
      kpds(16)=0
      kpds(22)=4
      do igrid=1,jf
       if(var_new(igrid,18).lt.0.0) var_new(igrid,18)=0.00000001
       outdata(igrid)=var_new(igrid,18)
       ensmean(igrid,18)=ensmean(igrid,18)+outdata(igrid)
      enddo
      call putgb(40+irun,jf,kpds,kgds,lb,outdata,iret)
C-----------------sfcp----------------------
      kpds(5)=1
      kpds(6)=1
      kpds(7)=0
      kpds(14)=itime
      kpds(15)=0
      kpds(16)=0
      kpds(22)=4
      do igrid=1,jf
       outdata(igrid)=var_new(igrid,19)
       ensmean(igrid,19)=ensmean(igrid,19)+outdata(igrid)
      enddo
      call putgb(40+irun,jf,kpds,kgds,lb,outdata,iret)
C-----------------sfch----------------------
      kpds(5)=7
      kpds(6)=1
      kpds(7)=0
      kpds(14)=itime
      kpds(15)=0
      kpds(16)=0
      kpds(22)=4
      do igrid=1,jf
       outdata(igrid)=var_new(igrid,20)
       ensmean(igrid,20)=ensmean(igrid,20)+outdata(igrid)
      enddo
      call putgb(40+irun,jf,kpds,kgds,lb,outdata,iret)

      call baclose(40+irun,ierr)

      iunit=iunit+1
C if want to change initial/reference time to forecast time:
c     ihr=ihr+3
c     if(ihr.ge.24) then
c       idy=idy+1
c       idywk=idywk+1
c       if(idywk.gt.7) idywk=1
c       ihr=0
c     endif
c     if(idy.gt.days(imo)) then
c       imo=imo+1
c       imon=imon+1
c       idy=1
c     endif

333   continue   !irun
444   continue   !mbase

c find the mean of new ensemble and write it out
c     do i=1,nv
c      do igrid=1,jf
c      ensmean(igrid,i)=ensmean(igrid,i)/newmem
c      enddo
c     enddo
      print*,'the last irun=',irun
      write(pert,'(i2.2)') irun
      write(hr,'(i2.2)') itime
      fout='hybd_pgrb212.'//pert//'.f'//hr
      call baopen(40+irun,fout,ierr)
C-----------------t2m----------------------
      kpds(5)=11
      kpds(6)=105
      kpds(7)=2
      kpds(14)=itime
      kpds(15)=0
      kpds(16)=0
      kpds(22)=temp_prec
      do igrid=1,jf
       outdata(igrid)=ensmean(igrid,1)/newmem
      enddo
      call putgb(40+irun,jf,kpds,kgds,lb,outdata,iret)
C-----------------u10m----------------------
      kpds(5)=33
      kpds(6)=105
      kpds(7)=10
      kpds(14)=itime
      kpds(15)=0
      kpds(16)=0
      kpds(22)=wind_prec
      do igrid=1,jf
       outdata(igrid)=ensmean(igrid,2)/newmem
      enddo
      call putgb(40+irun,jf,kpds,kgds,lb,outdata,iret)
C-----------------v10m----------------------
      kpds(5)=34
      kpds(6)=105
      kpds(7)=10
      kpds(14)=itime
      kpds(15)=0
      kpds(16)=0
      kpds(22)=wind_prec
      do igrid=1,jf
       outdata(igrid)=ensmean(igrid,3)/newmem
      enddo
      call putgb(40+irun,jf,kpds,kgds,lb,outdata,iret)
C-----------------rh2m----------------------
      kpds(5)=52
      kpds(6)=105
      kpds(7)=2
      kpds(14)=itime
      kpds(15)=0
      kpds(16)=0
      kpds(22)=4
      do igrid=1,jf
       outdata(igrid)=ensmean(igrid,4)/newmem
      enddo
      call putgb(40+irun,jf,kpds,kgds,lb,outdata,iret)
C-----------------slp----------------------
      kpds(5)=130
      kpds(6)=102
      kpds(7)=0
      kpds(14)=itime
      kpds(15)=0
      kpds(16)=0
      kpds(22)=4
      do igrid=1,jf
       outdata(igrid)=ensmean(igrid,5)/newmem
      enddo
      call putgb(40+irun,jf,kpds,kgds,lb,outdata,iret)
C-----------3h precip----------------------
      if(itime.ge.01) then
       if(itime.eq.03.or.itime.eq.06.or.itime.eq.09.or.itime.eq.12
     &.or.itime.eq.15.or.itime.eq.18.or.itime.eq.21.or.itime.eq.24
     &.or.itime.eq.27.or.itime.eq.30.or.itime.eq.33.or.itime.eq.36
     &.or.itime.eq.39.or.itime.eq.42.or.itime.eq.45.or.itime.eq.48)
     & then
        kpds(14)=itime-3
       else
        kpds(14)=itime-1
       endif
      kpds(5)=61
      kpds(6)=1
      kpds(7)=0
      kpds(15)=itime
      kpds(16)=4
      kpds(22)=3
      do igrid=1,jf
       outdata(igrid)=ensmean(igrid,6)/newmem
      enddo
      call putgb(40+irun,jf,kpds,kgds,lb,outdata,iret)
      endif
C------------cape---------------------------
      kpds(5)=157
      kpds(6)=1
      kpds(7)=0
      kpds(14)=itime
      kpds(15)=0
      kpds(16)=0
      kpds(22)=cape_prec
      do igrid=1,jf
       outdata(igrid)=ensmean(igrid,7)/newmem
      enddo
      call putgb(40+irun,jf,kpds,kgds,lb,outdata,iret)
C---------------cin------------------------
      kpds(5)=156
      kpds(6)=1
      kpds(7)=0
      kpds(14)=itime
      kpds(15)=0
      kpds(16)=0
      kpds(22)=cape_prec
      do igrid=1,jf
       outdata(igrid)=ensmean(igrid,8)/newmem
      enddo
      call putgb(40+irun,jf,kpds,kgds,lb,outdata,iret)
C-----------------500H-----------------------
      kpds(5)=7
      kpds(6)=100
      kpds(7)=500
      kpds(14)=itime
      kpds(15)=0
      kpds(16)=0
      kpds(22)=hght_prec
      do igrid=1,jf
       outdata(igrid)=ensmean(igrid,9)/newmem
      enddo
      call putgb(40+irun,jf,kpds,kgds,lb,outdata,iret)
C-----------------850rh----------------------
      kpds(5)=52
      kpds(6)=100
      kpds(7)=850
      kpds(14)=itime
      kpds(15)=0
      kpds(16)=0
      kpds(22)=4
      do igrid=1,jf
       outdata(igrid)=ensmean(igrid,10)/newmem
      enddo
      call putgb(40+irun,jf,kpds,kgds,lb,outdata,iret)
C-------------850u--------------------------
      kpds(5)=33
      kpds(6)=100
      kpds(7)=850
      kpds(14)=itime
      kpds(15)=0
      kpds(16)=0
      kpds(22)=wind_prec
      do igrid=1,jf
       outdata(igrid)=ensmean(igrid,11)/newmem
      enddo
      call putgb(40+irun,jf,kpds,kgds,lb,outdata,iret)
C-------------850v------------------------
      kpds(5)=34
      kpds(6)=100
      kpds(7)=850
      kpds(14)=itime
      kpds(15)=0
      kpds(16)=0
      kpds(22)=wind_prec
      do igrid=1,jf
       outdata(igrid)=ensmean(igrid,12)/newmem
      enddo
      call putgb(40+irun,jf,kpds,kgds,lb,outdata,iret)
C-------------250u--------------------------
      kpds(5)=33
      kpds(6)=100
      kpds(7)=250
      kpds(14)=itime
      kpds(15)=0
      kpds(16)=0
      kpds(22)=wind_prec
      do igrid=1,jf
       outdata(igrid)=ensmean(igrid,13)/newmem
      enddo
      call putgb(40+irun,jf,kpds,kgds,lb,outdata,iret)
C-------------250v------------------------
      kpds(5)=34
      kpds(6)=100
      kpds(7)=250
      kpds(14)=itime
      kpds(15)=0
      kpds(16)=0
      kpds(22)=wind_prec
      do igrid=1,jf
       outdata(igrid)=ensmean(igrid,14)/newmem
      enddo
      call putgb(40+irun,jf,kpds,kgds,lb,outdata,iret)
C-------- 850 temp ---------------------------
      kpds(5)=11
      kpds(6)=100
      kpds(7)=850
      kpds(14)=itime
      kpds(15)=0
      kpds(16)=0
      kpds(22)=temp_prec
      do igrid=1,jf
       outdata(igrid)=ensmean(igrid,15)/newmem
      enddo
      call putgb(40+irun,jf,kpds,kgds,lb,outdata,iret)
C-------- 500 temp ---------------------------
      kpds(5)=11
      kpds(6)=100
      kpds(7)=500
      kpds(14)=itime
      kpds(15)=0
      kpds(16)=0
      kpds(22)=temp_prec
      do igrid=1,jf
       outdata(igrid)=ensmean(igrid,16)/newmem
      enddo
      call putgb(40+irun,jf,kpds,kgds,lb,outdata,iret)
C-------- 250 temp ---------------------------
      kpds(5)=11
      kpds(6)=100
      kpds(7)=250
      kpds(14)=itime
      kpds(15)=0
      kpds(16)=0
      kpds(22)=temp_prec
      do igrid=1,jf
       outdata(igrid)=ensmean(igrid,17)/newmem
      enddo
      call putgb(40+irun,jf,kpds,kgds,lb,outdata,iret)
C-----------------q2m----------------------
      kpds(5)=51
      kpds(6)=105
      kpds(7)=2
      kpds(14)=itime
      kpds(15)=0
      kpds(16)=0
      kpds(22)=4
      do igrid=1,jf
       outdata(igrid)=ensmean(igrid,18)/newmem
      enddo
      call putgb(40+irun,jf,kpds,kgds,lb,outdata,iret)
C-----------------sfcp----------------------
      kpds(5)=1
      kpds(6)=1
      kpds(7)=0
      kpds(14)=itime
      kpds(15)=0
      kpds(16)=0
      kpds(22)=4
      do igrid=1,jf
       outdata(igrid)=ensmean(igrid,19)/newmem
      enddo
      call putgb(40+irun,jf,kpds,kgds,lb,outdata,iret)
C-----------------sfch----------------------
      kpds(5)=7
      kpds(6)=1
      kpds(7)=0
      kpds(14)=itime
      kpds(15)=0
      kpds(16)=0
      kpds(22)=4
      do igrid=1,jf
       outdata(igrid)=ensmean(igrid,20)/newmem
      enddo
      call putgb(40+irun,jf,kpds,kgds,lb,outdata,iret)

      call baclose(40+irun,ierr)

111   continue   !itime
      print*,'Finished with enswrite'
      stop
      end
C************************************************
      SUBROUTINE RDGB(LUGB,LGRIB,LSKIP,KPDS,KGDS,NDATA,LBMS,DATA)
C
C  READ GRIB FILE
C  INPUT
C    LUGB - LOGICAL UNIT TO READ
C    LGRIB - LENGTH OF GRIB RECORD
C    LSKIP - BYTES TO SKIP FOR GRIB RECORD
C  OUTPUT
C    KPDS(22) - UNPACKED PRODUCT DEFINITION SECTION
C    KGDS(20) - UNPACKED GRID DEFINITION SECTION
C    NDATA    - NUMBER OF DATA POINTS
C    LBMS(NDATA) - LOGICAL BIT MAP
C    DATA(NDATA) - DATA UNPACKED
C
      CHARACTER GRIB(LGRIB)*1
      INTEGER KPDS(25),KGDS(22),KPTR(20)
      LOGICAL LBMS(*)
      REAL DATA(*)
      NDATA=0
      CALL BAREAD(LUGB,LSKIP,LGRIB,LREAD,GRIB)
      IF(LREAD.LT.LGRIB) RETURN
      CALL W3FI63(GRIB,KPDS,KGDS,LBMS,DATA,KPTR,IRET)
      IF(IRET.NE.0) RETURN
      NDATA=KPTR(10)
c     print*,'ndata= ',ndata
      RETURN
      END
C********************************************
C********************************************
      SUBROUTINE SKGB(LUGB,ISEEK,LGRIB,LSKIP)
C
C  SEEK FOR NEXT GRIB1 RECORD WITHIN THE NEXT LSEEK=4096 BYTES
C  INPUT
C    LUGB  - LOGICAL UNIT TO READ
C    ISEEK - BYTES TO SKIP BEFORE SEARCH (SET TO 0 AT START)
C  OUTPUT
C    ISEEK - NUMBER OF BYTES READ SO FAR
C    LGRIB - LENGTH OF GRIB RECORD (0 IF NOT FOUND)
C    LSKIP - BYTES TO SKIP FOR GRIB RECORD
C
      PARAMETER(LSEEK=4096)
      CHARACTER C*(LSEEK)
      CALL BAREAD(LUGB,ISEEK,LSEEK,LREAD,C)
      DO I=0,LREAD-8
        IF(C(I+1:I+4).EQ.'GRIB'.AND.MOVA2I(C(I+8:I+8)).EQ.1) THEN
          LGRIB=MOVA2I(C(I+5:I+5))*65536
     &         +MOVA2I(C(I+6:I+6))*256
     &         +MOVA2I(C(I+7:I+7))
          LSKIP=ISEEK+I
          ISEEK=LSKIP+LGRIB
          RETURN
        ENDIF
      ENDDO
      LGRIB=0
      LSKIP=0
      ISEEK=ISEEK+LSEEK
      RETURN
      END

