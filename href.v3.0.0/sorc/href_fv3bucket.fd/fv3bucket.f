	program fv3bucket

!       Special program to get 1-h and 3-h snow from FV3SAR instantaneous WEASD

        USE GRIB_MOD

!	real pdiff(im,jm)
        real, allocatable :: pdiff(:,:)

        integer :: ihrs1, ihrs2, reset_flag
        integer :: mm,nn,oo,m,n
	character(len=2), dimension(2):: hrs

	character(len=255):: file1,file2,file3,file4,file5,testout
	character(len=150):: dirname
	character(len=150):: filename
	character(len=1):: reflag

	read(5,FMT='(A)') dirname
	read(5,FMT='(A)') filename
        read(5,FMT='(A)') hrs(1)
        read(5,FMT='(A)') hrs(2)
        read(5,FMT='(I1)') reset_flag
        read(5,*) IM, JM

        allocate(pdiff(im,jm))

	write(0,*) 'read reset_flag: ', reset_flag

	n=index(dirname,' ')-1
	m=index(filename,' ')-1

	do I=2,2
	
	file1= dirname(1:n)//'/'//filename(1:m)
     +					//HRS(I-1)//'.grib2'
	file2= dirname(1:n)//'/'//filename(1:m)//HRS(I)//'.grib2'

	read(HRS(I-1), '(I2)' ) ihrs1
	read(HRS(I)  , '(I2)' ) ihrs2

	interv=ihrs2-ihrs1

        write(0,*) 'ihrs1, ihrs2: ', ihrs1, ihrs2
	
	if (interv .eq. 3) then
	  testout= dirname(1:n)//'/PCP3HR'//HRS(I)//'.tm00'
	elseif (interv .eq. 1) then
	  testout= dirname(1:n)//'/PCP1HR'//HRS(I)//'.tm00'
	endif

	mm=index(file1,' ')-1
	nn=index(file2,' ')-1

	mmm=index(testout,' ')-1

	write(0,*) 'file1: ', file1(1:mm)
	write(0,*) 'file2: ', file2(1:nn)
	write(0,*) 'testout: ', testout(1:mmm)

	if (mod(ihrs1,3) .eq. 0 .and. reset_flag .eq. 1) then
	reset_flag=1
	else
	reset_flag=0
	endif

        write(0,*) 'call calc_pdiff with reset_flag: ', reset_flag

	call calc_pdiff(file1(1:mm),file2(1:nn),
     &                  TESTOUT(1:mmm),
     &                  pdiff,reset_flag,ihrs1,interv,IM,JM)

	enddo

	END program fv3bucket

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	SUBROUTINE CALC_PDIFF(FNAME1,FNAME2,TESTOUT,DPRECIP,
     &                           reset_flag,ihrs1,interv,IM,JM)
        USE GRIB_MOD
        USE pdstemplates
	character(*):: FNAME1,FNAME2,testout
	integer:: JPDS(200),JGDS(200), reset_flag,lencheck,ihrs1,interv
	integer:: KPDS1(200),KGDS(200),KPDS2(200),KPDS(200),KGDS2(200)
	logical:: BITMAP(IM*JM),FIRST

        real:: rinc(5)
        integer:: idat(8),jdat(8)

C grib2
      INTEGER :: LUGB,LUGI,J,JDISC,JPDTN,JGDTN
      INTEGER,DIMENSION(:) :: JIDS(200),JPDT(200),JGDT(200)
      INTEGER,DIMENSION(:) :: PDS_SNOW_HOLD(200)
      INTEGER,DIMENSION(:) :: PDS_SNOW_HOLD_EARLY(200)
      LOGICAL :: UNPACK
      INTEGER :: K,IRET
      TYPE(GRIBFIELD) :: GFLD
C grib2
	real:: s_later(IM*JM),s_earlier(IM*JM),sprecip(im*jm)
	

	call baopenr(11,fname1,ierr1)
	call baopenr(12,fname2,ierr2)
	call baopenw(13,testout,ierr3)

	write(0,*) 'baopened ', fname1,fname2,testout

	if ( (ierr1+ierr2+ierr3+ierr4) .ne. 0) then
		write(0,*) 'bad baopen!!! ', ierr1
		write(0,*) 'bad baopen!!! ', ierr2
		write(0,*) 'bad baopen!!! ', ierr3
		write(0,*) 'bad baopen!!! ', ierr4
		STOP
	endif
	
        s_earlier=0.

        JIDS=-9999
        JPDTN=-1
        JPDT=-9999
        JGDTN=-1
        JGDT=-9999
        UNPACK=.true.


        allocate(gfld%fld(im*jm))
        allocate(gfld%idsect(200))
        allocate(gfld%igdtmpl(200))
        allocate(gfld%ipdtmpl(200))
        allocate(gfld%idrtmpl(200))
        allocate(gfld%bmap(im*jm))

C USAGE:    CALL GETGB2(LUGB,LUGI,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,
C    &                  UNPACK,K,GFLD,IRET)

        J=0
        JIDS=-9999
        JPDTN=0
        JPDT=-9999
        JPDT(2)=13
        JGDTN=-1
        JGDT=-9999
        UNPACK=.true.

        call getgb2(11,0,J,0,JIDS,JPDTN,JPDT,JGDTN,JGDT,
     &     UNPACK,K,GFLD,IRET)


!        write(0,*) 'pulled gfld%igdtnum : ', gfld%igdtnum 

	if (IRET .ne. 0) then
	write(0,*) 'bad getgb1 earlier ', IRET
	STOP
	endif


        s_earlier=gfld%fld
        do K=1,gfld%ipdtlen
        PDS_SNOW_HOLD_EARLY(K)=gfld%ipdtmpl(K)
        enddo
        write(0,*) 'maxval(s_earlier): ', maxval(s_earlier)


        JIDS=-9999
        JPDTN=0
        JPDT=-9999
        JPDT(2)=13
        JGDTN=-1
        JGDT=-9999


        call getgb2(12,0,0,0,JIDS,JPDTN,JPDT,JGDTN,JGDT,
     &     UNPACK,K,GFLD,IRET1)

        write(0,*) 'K: ', K

	if (IRET1 .ne. 0) then
	 write(0,*) 'bad getgb later ', IRET1
	STOP
	endif

        write(0,*) 'pulled gfld%ipdtnum: ', gfld%ipdtnum
!        do J=1,29
!        write(0,*) 'ingest J,gfld%ipdtmpl(J): ', J,gfld%ipdtmpl(J)
!        enddo

        write(0,*) 'set s_later to gfld%fld'
        s_later=gfld%fld
        write(0,*) 'maxval(s_later): ', maxval(s_later)

        do K=1,gfld%ipdtlen
        PDS_SNOW_HOLD(K)=gfld%ipdtmpl(K)
        enddo


	if (reset_flag .eq. 1) then
	write(0,*) 'just later value'

	do NPT=1,IM*JM
	sprecip(NPT)=s_later(NPT)
	enddo

	else

	write(0,*) 'take normal difference ', IM*JM

	do NPT=1,IM*JM
	sprecip(NPT)=max(s_later(NPT)-s_earlier(NPT),0.0)
	enddo

	endif

!        do J=1,gfld%ipdtlen


        gfld%ipdtmpl(22)=1
        gfld%ipdtmpl(27)=interv

        write(0,*) 'interval specified in 27: ', interv

! -------------------------------------------


       gfld%ipdtmpl(1)=1
       gfld%ipdtmpl(2)=13
       gfld%ipdtmpl(3)=2
       gfld%ipdtmpl(4)=0
       gfld%ipdtmpl(5)=84
       gfld%ipdtmpl(6)=0 ! hours cutoff
       gfld%ipdtmpl(7)=0 ! minutes cutoff
       gfld%ipdtmpl(8)=1 ! units of hours
       gfld%ipdtmpl(9)=ihrs1 ! earlier forecast time of period?
       gfld%ipdtmpl(10)=1 ! sfc
       gfld%ipdtmpl(11)=0 ! sfc
       gfld%ipdtmpl(12)=0 ! sfc
       gfld%ipdtmpl(13)=255 ! sfc
       gfld%ipdtmpl(14)=0 ! sfc
       gfld%ipdtmpl(15)=0 ! sfc

!!! need to figure out how to do this end of period date stuff right

       rinc=0.
       rinc(2)=float(ihrs1+interv)

       write(0,*) 'rinc: ', rinc
       idat=0
       idat(1)=gfld%idsect(6)
       idat(2)=gfld%idsect(7)
       idat(3)=gfld%idsect(8) 
       idat(5)=gfld%idsect(9)
	write(0,*) 'idat(1:3),idat(5): ', 
     &  idat(1:3),idat(5)

       call w3movdat(rinc,idat,jdat)

	write(0,*) 'jdat(1:3),jdat(5): ', 
     &  jdat(1:3),jdat(5)
       gfld%ipdtmpl(16)=jdat(1)
       gfld%ipdtmpl(17)=jdat(2)
       gfld%ipdtmpl(18)=jdat(3)
       gfld%ipdtmpl(19)=jdat(5)
       gfld%ipdtmpl(20)=0
       gfld%ipdtmpl(21)=0

        gfld%ipdtnum=8
        gfld%ipdtmpl(22)=1
        gfld%ipdtmpl(23)=0
        gfld%ipdtmpl(24)=1 ! accum?
        gfld%ipdtmpl(25)=2 ! fcst hour increments
        gfld%ipdtmpl(26)=1 ! hours?
        gfld%ipdtmpl(27)=interv
        gfld%ipdtmpl(28)=255
        gfld%ipdtmpl(29)=0

        do J=1,29
        write(0,*) 'J,gfld%ipdtmpl(J): ', J,gfld%ipdtmpl(J)
        enddo

        gfld%fld=sprecip
	call putgb2(13,GFLD,IRET)
        write(0,*) 'IRET from putgb2 for sprecip ', IRET

        call baclose(13,IRET)

	write(0,*) 'extremes of snow: ', 
     +		maxval(sprecip)

  633	format(25(f4.1,1x))

	end subroutine calc_pdiff
