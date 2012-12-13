 
C MAIN PROGRAM: SREF_ENS_GEN
C   PRGMMR: DU               ORG: NP21        DATE: 2002-01-02
C
C ABSTRACT: READS IN SREF GRIB AND OUTPUTS ENSEMBLE GRIB 
C           PRODUCTS. 
C
C PROGRAM HISTORY LOG:
c
c Old Version:
c 2001-07-10    Jun Du, initial program
c
c Generic Version: 
c
c 2005-08-01 Author: Binbin Zhou 
c
c       Re-organize the code structure and re-write the program in following features:
c
c        (1) The generic version dosn't fix GRIB# like 212 as old version. It aan process
c            any GRIB#, and the domain (IM, JM) for the defined GRIB# is also flexible. 
c            As long as GRIB# is specified in 'filename', this program can automatically 
c            retrievs the domain (IM, JM, JF=IM*JM), then all arrays associated with this 
c            domain are dynamically allocated (on the fly)
c        (2) Ensemble members (IENS) are not fixed, but depends on the input file "filename'
c            in which, the all of soft linked file name are listed in order. The order and
c            number of files are changeable, the program can automatically allocate them
c        (3) The variables are changeable by using the 'variable.tbl' file, in which, two kinds of
c            variables are defined: (i) direct variables: read from GRIB files. Direct  
c            variables will also be computed for mean/spread an probability depending 
c            on the settings in each variable redcord (ii) derived variables: they are 
c            not read from GRIB files but need to do some derivation from direct variables.
c        (4) Derived variables need some algorithm to derive, Currently, only following derived 
c            variables are defined in this program:
c            (A) Thickness of two (pressure) levels: mean/spread/probability         
c            (B) 3hr, 6hr, 12hr, 24hr Accumulated precipitation: mean/spread/probability 
c            (C) 3hr, 6hr, 12hr, 24hr Accumulated snowfall: mean/spread/probability
c            (D) Precipitation type: probability
c            (E) Dominant precipitation: Mean
c            If want to process other derived product, user must provide their algorithms 
c            and then the computation code should be put into this program       
c        (5) User can request any kind of direct variables, either mean/spread or probability
c            by editing the 'variable.tbl' file (read README file for its gramma)
c 
c change log:
c 2006-01-12:Binbin Zhou
c             Add Geoff Manikin's algorithm to determine dominant precip type 
c 2006-03-01:Binbin Zhou
c             Modify wind spread computation by considering the wind directions, suggested 
c             by Jun Du            
c 2006-03-30: Jun Du: convert the unit from K to C for lifted index 
c
c 2006-04-30: Binbin Z. Add DTRA variance variables 
c
c 2006-05-15: Binbin Zhou
c             Modify to accept Beijing 2008 Olympic Game Grid#137
c
c 2006-07-21: Binbin Z. Add fog probability
c
c 2007-01-09: Binbin Z. Add freezing rain for GFS ensemble
c
c 2007-01-15: Binbin Z. Add Absolute vorticity for global ensemble since GFS has no absolute vorticity output
c 
c 2007-04-05: Binbin Z. Add High resolution WRF testing grid#137 
c
c 2007-10-09: Binbin Z. Add flight-restriction probability computation
c
c
c$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$$$
C

       include 'parm.inc'

C  raw data
      real,allocatable,dimension(:,:,:)   :: precip                   !jf,iens, number of fcst output files
      real,allocatable,dimension(:,:,:)   :: mrk_ice                  !jf,iens, number of fcst output files
      real,allocatable,dimension(:,:,:)   :: mrk_frz                  !jf,iens, number of fcst output files
      real,allocatable,dimension(:,:,:)   :: mrk_snow                 !jf,iens, number of fcst output files
      real,allocatable,dimension(:,:,:,:) :: rawdata_mn, rawdata_pr   !jf,iens,maxvar, maxmlvl

C mean
      real,allocatable,dimension(:,:,:) :: vrbl_mn                    !jf, numvar,maxmlvl
      real,allocatable,dimension(:,:,:) :: derv_mn                    !jf, nderiv,maxmlvl

      real,allocatable,dimension(:,:)   :: prcpmax                    !jf, maxmlvl
      real,allocatable,dimension(:,:)   :: prcpmin                    !jf, maxmlvl
      real,allocatable,dimension(:,:)   :: snowmax                    !jf, maxmlvl
      real,allocatable,dimension(:,:)   :: snowmin                    !jf, maxmlvl
      real,allocatable,dimension(:,:)   :: frznmax                    !jf, maxmlvl
      real,allocatable,dimension(:,:)   :: frznmin                    !jf, maxmlvl


C spread

      real,allocatable,dimension(:,:,:) :: vrbl_sp                    !jf, numvar, maxmlvl
      real,allocatable,dimension(:,:,:) :: derv_sp                    !jf, nderiv, maxmlvl

      real,allocatable,dimension(:) :: ppt3_sp,ppt6_sp,ppt12_sp,
     *                                 ppt24_sp,s12_sp

C probability
      real,allocatable,dimension(:,:,:,:) :: vrbl_pr                   !jf, numvar, maxplvl, maxtlvl
      real,allocatable,dimension(:,:,:,:) :: derv_pr                   !jf, nderiv, maxplvl, maxtlvl

C others 
       logical, allocatable, dimension(:) ::      lb                   !jf
       character*19, allocatable, dimension(:) :: fhead
       real,allocatable,dimension(:) ::           var                  !jf
       real,allocatable,dimension(:) ::           apoint               !iens
       real,allocatable,dimension(:,:,:)  :: ptype_mn,ptype_pr         !jf,maxmlvl,4 

       real,allocatable,dimension(:,:,:) :: derv_dtra                  !jf,maxmlvl,8 for DTRA requests
       real,allocatable, dimension(:)    :: Hsfc                       !surface height for DTRA  
      

C for get grib size jf=im*jm
  
       integer iyr,imon,idy,ihr
       character*50 gdss(400)
       integer IENS, GRIBID, kgdss(200), lengds,im,jm,km,jf,jff

       integer  interval, loutput

cc%%%%%%%  8. To be added if necessary ...........................
C original                                                                                                                                           
       dimension kpds(25),kgds(25)
       dimension kens(5),kprob(2),xprob(2),kclust(16),kmembr(80)
       character gds(42)
       character*13 mnout
       character*15 spout
       character*13 prout
       character*10 date
       character*2 pert2,hrloc
       character*3 hr,pert
       character*19 fname
       character*23 fname_pcp

C for variable table:
        Integer numvar, nderiv, fhr, i000
        Character*4 vname(maxvar)
        Integer k5(maxvar), k6(maxvar)
        Character*1 Msignal(maxvar), Psignal(maxvar)
        Integer Mlvl(maxvar), MeanLevel(maxvar,maxmlvl)
        Integer Plvl(maxvar), ProbLevel(maxvar,maxplvl)
        Integer Tlvl(maxvar)
        Character*1 op(maxvar)
        Real    Thrs(maxvar,maxtlvl)
        Character*4 eps       
        Character*10 models     
        Character*200 oneline                                                                                                                                Integer nsubstr
        Character*20  substr(50)                
  
c    for derived variables
        Character*4 dvname(maxvar)
        Integer dk5(maxvar), dk6(maxvar)
        Character*1 dMsignal(maxvar), dPsignal(maxvar)
        Integer dMlvl(maxvar), dMeanLevel(maxvar,maxmlvl)
        Integer dPlvl(maxvar), dProbLevel(maxvar,maxplvl)
        Character*1 dop(maxvar)
        Integer dTlvl(maxvar)
        Real    dThrs(maxvar,maxtlvl)
        Integer MPairLevel(maxvar,maxmlvl,2)
        Integer PPairLevel(maxvar,maxplvl,2)
        
	Integer timestep, fnn(100)                                                                                                                                                        
        common /tbl/numvar,
     +              vname,k5,k6,Mlvl,Plvl,Tlvl,
     +              MeanLevel,ProbLevel,Thrs,
     +              Msignal,Psignal,op
                                                                                                                                                                
        common /dtbl/nderiv,
     +              dvname,dk5,dk6,dMlvl,dPlvl,dTlvl,
     +              dMeanLevel,dProbLevel,dThrs,
     +              dMsignal,dPsignal,MPairLevel,PPairLevel,dop
                                                                                                                                                                
       namelist/namin/iyr,imon,idy,ihr,fhr


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Read table file to get wanted ensemble variable information
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        nunit=101
        open (nunit, file='variable.tbl', status='old')
                                                                                                                          
        call readtbl(nunit)

        close(nunit)

        write(*,*) 'Check direct variable reading:'
        do i = 1, numvar
          write(*,*) vname(i),k5(i), k6(i),Msignal(i), Mlvl(i),
     +                 (MeanLevel(i,j),j=1,Mlvl(i)),
     +    Psignal(i), Plvl(i), (ProbLevel(i,j),j=1,Plvl(i)),
     +    op(i), Tlvl(i),   (Thrs(i,j),j=1,Tlvl(i))
        end do

        write(*,*) 'Check derived variable reading:'
        do i = 1, nderiv
         if(dk5(i).eq.7.and.dk6(i).eq.101) then
           write(*,*) dvname(i),dk5(i), dk6(i),dMsignal(i),dMlvl(i),
     +              (dMeanLevel(i,j),j=1,dMlvl(i)),
     +              (MPairLevel(i,j,1),j=1,dMlvl(i)),
     +              (MPairLevel(i,j,2),j=1,dMlvl(i)),
     +          dPsignal(i),dPlvl(i),(dProbLevel(i,j),j=1,dPlvl(i)),
     +              (PPairLevel(i,j,1),j=1,dPlvl(i)),
     +              (PPairLevel(i,j,2),j=1,dPlvl(i)),
     +              dop(i), dTlvl(i),   (dThrs(i,j),j=1,dTlvl(i))
         else
           write(*,*) dvname(i),dk5(i), dk6(i),dMsignal(i),dMlvl(i),
     +              (dMeanLevel(i,j),j=1,dMlvl(i)),
     +          dPsignal(i),dPlvl(i),(dProbLevel(i,j),j=1,dPlvl(i)),
     +             dop(i), dTlvl(i),   (dThrs(i,j),j=1,dTlvl(i))
         end if
        end do


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Read filename file to get # of ensemble available, grib#, vertical pressure level,
c    last forecast time, forecast outout interval and grib file heads, 
c    so that arrays can be dynamically allocatable
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       open(1, file='filename', status='old')
       read(1,*) IENS,GRIBID,KM,timestep,interval
       read(1,201) (fnn(i),i=1,timestep)
201    format(<timestep>(i3))
       write(*,*) IENS,GRIBID,KM,timestep,interval
      
       if(GRIBID.eq.256) then
c        im=134                 !2008 Beijing Olympic Game domain
c        jm=101
         im=884                 !HiresWindow (USeast and USwest)
         jm=614
         jf=im*jm
         GRIBID=255
        else
         if(GRIBID.eq.257) then
         im=825                 !HiresWindow (AK)
         jm=603
         jf=im*jm
         GRIBID=255
        else
         if(GRIBID.eq.258) then
         im=223                 !HiresWindow (HI and PR)
         jm=170
         jf=im*jm
         GRIBID=255
       else
         if(GRIBID.eq.259) then
         im=340                 !HiresWindow (HaitiPR)
         jm=208
         jf=im*jm
         GRIBID=255
       else
         call makgds(GRIBID, kgdss, gdss, lengds, ier)
         im=kgdss(2)
         jm=kgdss(3)
         jf=kgdss(2)*kgdss(3)
        end if
        end if
       end if
       endif
 
       write(*,*) 'im, jm, jf=', im, jm, jf
       write(*,*) 'timestep, interval=', 
     +             timestep, interval

c  allocate raw data
       allocate(rawdata_mn(jf,iens,numvar,maxmlvl))
       allocate(rawdata_pr(jf,iens,numvar,maxplvl))

c allocate mean


       allocate(vrbl_mn(jf, numvar, maxmlvl))
       allocate(derv_mn(jf, nderiv, maxmlvl))

       allocate(prcpmax(jf,maxmlvl))
       allocate(prcpmin(jf,maxmlvl))
       allocate(snowmax(jf,maxmlvl))
       allocate(snowmin(jf,maxmlvl))
       allocate(frznmax(jf,maxmlvl))
       allocate(frznmin(jf,maxmlvl))


c spread

       allocate(vrbl_sp(jf, numvar, maxmlvl))
       allocate(derv_sp(jf, nderiv, maxmlvl))

c prob

       allocate(vrbl_pr(jf, numvar, maxplvl, maxtlvl))
       allocate(derv_pr(jf, nderiv, maxplvl, maxtlvl))

c others
       allocate(lb(jf))
       allocate(fhead(iens))   
       allocate(var(jf))
       allocate(apoint(iens)) 
       allocate(ptype_mn(jf,maxmlvl,4))
       allocate(ptype_pr(jf,maxmlvl,4))

       allocate(derv_dtra(jf,maxmlvl,10))
       allocate(Hsfc(jf))

       write(*,*) 'Array allocation done!'

c read file names' head
       write(*,*) iens, ' GRIB files are read:' 
       do i=1,IENS
        read(1,'(A)') oneline
        call ST_CLST( oneline, ' ', ' ', 5, substr, nsubstr, ier ) 
        call ST_RMBL( substr(1), fhead(i), lng, ier)  
        call ST_RMBL( substr(2), models, lng, ier)  
        write(*,*) fhead(i), models
       end do
 
C J. Du note on 3/19/2010: use the first 4 characters of the last model's name 
c to name the final product output files (mean, spread and prob). So, the product
c files name needs to be adjusted when model name changes in production!
       eps=trim(models)
       

       close (1)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


c In these lines put here, we need the date and time of the forecast,
c the name of the file.  We need to open the file.  Then we can point to
c the correct location of the forecast in the forecast file.  If this
c is an ensemble, then we also need another loop for the forecast name
c (cntl, n1, p1, etc.).  That loop should be inside the date/time loop.
c
 
c Passing over date information
c
c      CALL W3TAGB('SREF_COM_GRIB',2002,0002,0074,'NP21')
C
      read(11,namin,end=1000)
        i00=1+fhr/3
        loutput=i00
	write(0,*) 'loutput for allocate: ', loutput

c  allocate raw data
       allocate(precip(jf,iens,loutput))             !save all itime's precip 
       allocate(mrk_ice(jf,iens,loutput))            !save all itime's ice mark
       allocate(mrk_frz(jf,iens,loutput))            !save all itime's freezing rain mark
       allocate(mrk_snow(jf,iens,loutput))           !save all itime's snow mark

      imin=0
1000  continue
      write(*,*) 'YR MON DAY CYCLE MIN:', iyr,imon,idy,ihr,imin

       print*,'About to enter time loop'

C first loop: forecast times-------------------------------------------

	itime=fhr
         print*,'Initializing variables'
         write(*,*) 'itime=',itime,' i00=',i00

c  Second loop: for ensmeble members:----------------------------------------
        DO 2000 irun=1,iens                       ! perturbation members

        if(itime.lt.100) then
         write(hr,'(i2.2)') itime
        else
         write(hr,'(i3.3)') itime
        end if

	write(0,*) 'hr: ', hr
 
        fname=trim(fhead(irun)) // '.f' // trim(hr)

        iunit=10
        write(0,*) 'Opening ',fname
        call baopen(iunit,fname,ierr)
	write(0,*) 'ierr from opening main file: ', ierr

c        print*,'itime=',itime, ' for member# ', irun

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c STEP (1): 
c Read various variables from the grib file, depending on the value
c of kpds(5) (the type of variable) and kpds(7) (the height of the variable.
c Certain derived quantities are at the end of the list.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       iseek=0
       call skgb(iunit,iseek,llgrib,llskip)
       DO while(llgrib.gt.0)  ! llgrib 

        call rdgb(iunit,llgrib,llskip,kpds,kgds,jff,lb,var)
        call skgb(iunit,iseek,llgrib,llskip)

       if (jff.eq.0) var=-999.0

       do nv = 1, numvar

         do lv = 1, Mlvl(nv)
           if(kpds(5).eq.k5(nv).and.kpds(6).eq.k6(nv).and.
     +                       kpds(7).eq.MeanLevel(nv,lv)) then
             do igrid=1,jf
              if(kpds(5).eq.131.and.kpds(6).eq.101.and.
     +                      kpds(7).eq.12900) then
               rawdata_mn(igrid,irun,nv,lv) = var(igrid) - 273.15 !Jun Du: change unit from K to C for lifted index
              else
               rawdata_mn(igrid,irun,nv,lv) = var(igrid)
              end if
             
              if(kpds(5).eq.7.and.kpds(6).eq.1.and.itime.gt.0) then
               rawdata_mn(igrid,irun,nv,lv) = Hsfc(igrid)
              end if

c reprocess precip data based on different models and fcst hours when necessary
              if(kpds(5).eq.61.and.kpds(6).eq.1.and.itime.ge.1) then
                if(models.eq.'sref_eta') then
                  rawdata_mn(igrid,irun,nv,lv)=var(igrid)*1.0
                 else if (models.eq.'sref_rsm') then
                  rawdata_mn(igrid,irun,nv,lv)=var(igrid)*1.0
                 else if (models.eq.'sref_nmm') then
                  rawdata_mn(igrid,irun,nv,lv)=var(igrid)*1.0
                 else if (models.eq.'sref_em') then
                  rawdata_mn(igrid,irun,nv,lv)=var(igrid)*1.0
                end if
              end if
             end do
 
            if(kpds(5).eq.7.and.kpds(6).eq.1.and.itime.eq.0) then
              Hsfc(:) = var (:)
            end if
             
           end if
         end do

         do lv = 1, Plvl(nv)
           if(kpds(5).eq.k5(nv).and.kpds(6).eq.k6(nv).and.
     +                       kpds(7).eq.ProbLevel(nv,lv)) then
             do igrid=1,jf
              if(kpds(5).eq.131.and.kpds(6).eq.101.and.
     +                      kpds(7).eq.12900) then
               rawdata_pr(igrid,irun,nv,lv) = var(igrid) - 273.15 !Jun Du: change unit from K to C for lifted index
              else if (kpds(5).eq.61.and.kpds(6).eq.1.and.
     +                 itime.ge.1) then
                if(models.eq.'sref_eta') then
                 rawdata_pr(igrid,irun,nv,lv)=var(igrid)*1.0
                else if (models.eq.'sref_rsm') then
                 rawdata_pr(igrid,irun,nv,lv)=var(igrid)*1.0
                else if (models.eq.'sref_nmm') then
                 rawdata_pr(igrid,irun,nv,lv)=var(igrid)*1.0
                else if (models.eq.'sref_em') then
                 rawdata_pr(igrid,irun,nv,lv)=var(igrid)*1.0
                else
                  rawdata_pr(igrid,irun,nv,lv) = var(igrid)
                end if
              else
               rawdata_pr(igrid,irun,nv,lv) = var(igrid)
              end if
             end do

           end if
         end do

       end do 
      enddo       ! end of while loop for llgrib
      call baclose(iunit,ierr)

	if (mod(fhr,3) .eq. 0) then

	write(0,*) 'dealing with precip for irun: ', irun

        iunit_pcp=40

	hrmin=fhr-24
        i000=0
	if (hrmin .lt. 0) hrmin=0

	if (hrmin .gt. 0) then
	i000=hrmin/3
	endif

	write(0,*) 'hrmin, i000 before +1: ', hrmin, i000

	do II=hrmin,fhr,3

           i000=i000+1
	write(0,*) 'II, i000: ', II, i000
           if (II .eq. 0) then
             precip(:,irun,i000)=0.
           else

           write(hrloc,633) II
	write(0,*) 'hrloc: ', hrloc
  633      format(I2.2)
           fname_pcp=trim(fhead(irun)) // '.f' // trim(hrloc) // '_pcp'
	write(0,*) 'fname_pcp: ', fname_pcp
           iunit_pcp=iunit_pcp+2
           call baopen(iunit_pcp,fname_pcp,ierr)

           if (ierr .ne. 0) stop

           iseek=0
           call skgb(iunit_pcp,iseek,llgrib2,llskip2)

           call rdgb(iunit_pcp,llgrib2,llskip2,kpds,kgds,
     &               jff,lb,precip(:,irun,i000))

c	   write(0,*) 'defining precip for irun, i000: ', irun, i000

	write(0,*) 'max precip for irun, fhr, hrloc: ', 
     &         irun,fhr,hrloc,maxval(precip(:,irun,i000))

!             call baclose(iunit_pcp,ierr)
           endif
       end do ! II loop
       endif

	write(0,*) 'past II loop'
	
ccc     store accumulated variable-related info:  cccccccccccccccccccc

c precip already stored directly above

c       if(kpds(5).eq.61.and.kpds(6).eq.1.and.kpds(7).eq.0) then
c        ID_PRCP=index_table(k5,k6,61,1,maxvar)
c        if(ID_PRCP.gt.0.and.itime.ge.1) then
c           precip(:,irun,i00)=rawdata_mn(:,irun,ID_PRCP,1)
c        end if 
c        end if


       if(kpds(5).eq.141.and.kpds(6).eq.1.and.kpds(7).eq.0) then
         ID_FRZ=index_table(k5,k6,141,1,maxvar)
         if(ID_FRZ.gt.0.and.itime.ge.1) then
            mrk_frz(:,irun,i00)=rawdata_mn(:,irun,ID_FRZ,1)
         end if
       end if


       if(kpds(5).eq.142.and.kpds(6).eq.1.and.kpds(7).eq.0) then
         ID_ICE=index_table(k5,k6,142,1,maxvar)
         if(ID_ICE.gt.0.and.itime.ge.1) then
            mrk_ice(:,irun,i00)=rawdata_mn(:,irun,ID_ICE,1)
         end if
       end if

       if(kpds(5).eq.143.and.kpds(6).eq.1.and.kpds(7).eq.0) then
         ID_SNOW=index_table(k5,k6,143,1,maxvar)
         if(ID_SNOW.gt.0.and.itime.ge.1) then
            mrk_snow(:,irun,i00)=rawdata_mn(:,irun,ID_SNOW,1)
         end if
       end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       

!      enddo       ! end of while loop for llgrib

!      call baclose(iunit,ierr)

2000  continue    ! end of irun

c      call print_rawdata(rawdata_mn, rawdata_pr, jf, iens,
c     +     2051,2051)

 

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c STEP (2A):
c get mean and spread of direct variables
        write(*,*) 'get mean and spread ----------------------------'
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        do nv = 1, numvar
          if(trim(Msignal(nv)).eq.'M') then
           do lv = 1, Mlvl(nv)
            do igrid=1,jf
              apoint = rawdata_mn(igrid,:,nv,lv)

              if(k5(nv).eq.20.and.k6(nv).eq.1) then                    ! Visibility: conditional mean
                call get_cond_mean (apoint,iens,24056.0,amean,aspread)

               if(igrid.eq.2051) then
                 write(*,*) 'Visibility: at ',igrid
                 write(*,*) '  Visbapoint=',apoint
                 write(*,*) '  amean=',amean, ' aspread=',aspread
               end if

              else if(k5(nv).eq.7.and.k6(nv).eq.215) then              ! Ceiling: conditional mean
                call get_cond_mean (apoint,iens,20000.0,amean,aspread)

              else if(k5(nv).eq.153.and.k6(nv).eq.100) then            ! fog LWC: liquid water content
                apoint = apoint * 1000.0                               !      kg/kg -> g/kg
                call get_cond_mean_lwc(apoint,iens,0.0,amean,aspread)   
                if(igrid.eq.2287) then
                 write(*,*) 'Fog LWC at:', lv, apoint,' mean=',amean
                end if

              else
                call getmean(apoint,iens,amean,aspread)
              end if

              vrbl_mn(igrid,nv,lv)=amean
              vrbl_sp(igrid,nv,lv)=aspread

            end do
           end do
          end if
        end do

        write(*,*) 'finish direct variable mean and spread computation'
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c STEP (2B): 
c get prob for all direct variables at different levels for different thresholds
        write(*,*) 'get prob ----------------------------------------'
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        do nv = 1, numvar
          if(trim(Psignal(nv)).eq.'P') then
           do lv = 1, Plvl(nv)
            do lt = 1, Tlvl(nv)
              do igrid=1,jf
                apoint = rawdata_pr(igrid,:,nv,lv)
 
                 if(trim(op(nv)).ne.'-') then

                  thr1 = Thrs(nv,lt)
                  thr2 = 0.
                  call getprob(apoint,iens,thr1,thr2,op(nv),aprob)
                  vrbl_pr(igrid,nv,lv,lt)=aprob
                 else
                  if(lt.lt.Tlvl(nv)) then
                    thr1 = Thrs(nv,lt)
                    thr2 = Thrs(nv,lt+1)

                    call getprob(apoint,iens,thr1,thr2,op(nv),aprob)
                       vrbl_pr(igrid,nv,lv,lt)=aprob
                   end if
                  end if

              end do
            end do
           end do
          end if
        end do


cc. Compute ceiling since WRF post has no ceiling before it is updated
Cc  Before using new SREF file (which has no ceiling), still use getceiling 

        do nv = 1, numvar    
          if (k5(nv).eq.7 .and. k6(nv).eq.215) then                 
               call getceiling(nv,rawdata_mn,jf,iens,
     +             vrbl_mn, vrbl_sp, vrbl_pr)
          end if
        end do

cc  Compute cloud top above ground level
        do nv = 1, numvar
          if (k5(nv).eq.7 .and. k6(nv).eq.3) then
               call getcldtop(nv,rawdata_mn,jf,iens,
     +             vrbl_mn, vrbl_sp, vrbl_pr)
          end if
        end do       

        write(*,*) 'finish direct variable prob computation'
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c STEP (3):
c get mean/spread/prob for special derived variables
        write(*,*) 'get mean for derived variables -------------------'
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

         do 100 nv = 1, nderiv                       !for all derived var

cc%%%%%%% 1. To  see if there is thickness computation, if yes, do it

           if (dk5(nv).eq.7.and. dk6(nv).eq.101) then 

            call thickness (nv, rawdata_mn, jf, iens,
     +             derv_mn, derv_sp, derv_pr)
           end if
 

cc%%%%%%% 2. To see if there is precipitation type computation, if yes, do it

           if (dk5(nv).eq.140 .or. dk5(nv).eq.193 .or.
     +         dk5(nv).eq.194 .or. dk5(nv).eq.195) then

              call preciptype (nv, rawdata_mn, jf, iens,
     +             derv_mn, derv_pr, ptype_mn, ptype_pr)

           end if

cc%%%%%%% 3. To see if there is accumulated precipitation, if yes, do it

            if(dk5(nv).eq.61.and.dk6(nv).eq.1.and.itime.gt.0) then

	write(0,*) 'calling apcp'

               call apcp(nv,itime,i00,precip,jf,iens,interval,
     +           loutput, derv_mn,derv_sp,derv_pr,prcpmax,prcpmin)

             end if

cc%%%%%%% 4. To see if there is accumulated snow, if yes, do it

            if(dk5(nv).eq.79.and.dk6(nv).eq.1.and.itime.gt.0) then
                                                                                                                                               
              call snow(nv,itime,i00,precip,mrk_ice,mrk_snow,jf,iens,
     +  interval,loutput, derv_mn,derv_sp,derv_pr,snowmax,snowmin)

            end if

cc%%%%%%% 5. To see if there is wind speed computation, if yes, do it

            if(dk5(nv).eq.32) then

               call wind(nv,rawdata_mn,jf,iens,
     +             derv_mn, derv_sp, derv_pr)

            end if


cc%%%%%%% 6. To get DTRA variances, if yes, do it
            if(dk5(nv).eq.501) then
             
               call dtra(nv,rawdata_mn,jf,iens,
     +             derv_dtra)
             
            end if

cc%%%%%%%  7. To get fog, if yes, do it
            if(dk5(nv).eq.146) then
                                                                                                                                                      
               call getfog(nv,rawdata_mn,jf,iens,derv_pr)
                                                         
            end if


cc%%%%%%% 8. To see if there is accumulated freezing rain, if yes, do it

            if(dk5(nv).eq.78.and.dk6(nv).eq.1.and.itime.gt.0) then

              call frzn(nv,itime,i00,precip,mrk_ice,mrk_frz,jf,iens,
     +  interval,loutput, derv_mn,derv_sp,derv_pr,frznmax,frznmin)

            end if
         

        write(*,*) 'finish derived computation for ', dvname(nv)

cc%%%%%%%  9. Add absolute vorticity( for global ensemble )

c            if(dk5(nv).eq.41) then
c               call vorticity (nv,rawdata_mn,im,jm,jf,iens,
c     +             derv_mn, derv_sp, derv_pr)
c            end if


cc%%%%%%%  10. Add flight restriction probability computation

            if(dk5(nv).eq.20. and. dk6(nv).eq.2) then
              call flight_res(nv,rawdata_mn,jf,iens,derv_pr)
            end if

cc%%%%%%%  11. To be added if necessary ...........................

100     continue 

        write(*,*)'>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>'
c        write(*,*) 'precip=', (precip(10,irun,i00),irun=1,iens)


c        call print_result(vrbl_mn,vrbl_sp,vrbl_pr,
c     +     derv_mn,derv_sp,derv_pr,ptype_mn,ptype_pr,derv_dtra,
c     +     prcpmax,prcpmin,snowmax,snowmin,frznmax,frznmin,
c     +     jf,iens,2051,2051)


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c STEP (4A): 
c Write results into grib files for direct variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        call pack(itime,eps,vrbl_mn,vrbl_sp,vrbl_pr,
     +            derv_mn,derv_sp,derv_pr,
     +            ptype_mn,ptype_pr,derv_dtra,
     +            prcpmax,prcpmin,snowmax,snowmin,
     +            frznmax,frznmin,
     +            jf,iens,iyr,imon,idy,ihr,
     +            gribid,lb,kgds)

3001      iunit=iunit+1

        write(*,*) 'Forecast f', itime, ' done!'

c release all space:

       deallocate(precip)
       deallocate(rawdata_mn)
       deallocate(rawdata_pr)
       deallocate(vrbl_mn)
       deallocate(derv_mn)
       deallocate(prcpmax)
       deallocate(prcpmin)
       deallocate(snowmax)
       deallocate(snowmin)
       deallocate(frznmax)
       deallocate(frznmin)
       deallocate(vrbl_sp)
       deallocate(derv_sp)
       deallocate(vrbl_pr)
       deallocate(derv_pr)
       deallocate(lb)
       deallocate(fhead)
       deallocate(var)
       deallocate(apoint)
       deallocate(ptype_mn)
       deallocate(ptype_pr)
       deallocate(derv_dtra)
       deallocate(mrk_ice)
       deallocate(mrk_frz)
       deallocate(mrk_snow)
       deallocate(Hsfc) 

      stop
      end

