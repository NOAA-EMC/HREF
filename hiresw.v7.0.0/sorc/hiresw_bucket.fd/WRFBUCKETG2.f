	program wrfbucket

!	Program will read in the total accumulated precipitation from
!	a series of WRFPRS files, and compute simple differences to
!	get precip buckets with a duration equal to the interval between
!	output times.

        USE GRIB_MOD


!	real pdiff(im,jm)
        real, allocatable :: pdiff(:,:)

        integer :: ihrs1, ihrs2, reset_flag
	character(len=2), dimension(2):: hrs

	character(len=255):: file1,file2,file3,file4,file5,testout
	character(len=150):: dirname
	character(len=150):: filename
	character(len=10):: cycname
	character(len=1):: reflag

	read(5,FMT='(A)') dirname
	read(5,FMT='(A)') filename
c       read(5,FMT='(A)') cycname
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
     +					//HRS(I-1)//'.tm00'
	file2= dirname(1:n)//'/'//filename(1:m)//HRS(I)//'.tm00'

	read(HRS(I-1), '(I2)' ) ihrs1
	read(HRS(I)  , '(I2)' ) ihrs2

	interv=ihrs2-ihrs1

        write(0,*) 'ihrs1, ihrs2: ', ihrs1, ihrs2
	
	if (interv .eq. 3) then
	  testout= dirname(1:n)//'/PCP3HR'//HRS(I)//'.tm00'
	elseif (interv .eq. 1) then
	  testout= dirname(1:n)//'/PCP1HR'//HRS(I)//'.tm00'
	elseif (interv .eq. 6) then
	  testout= dirname(1:n)//'/PCP6HR'//HRS(I)//'.tm00'
	endif

	mm=index(file1,' ')-1
	nn=index(file2,' ')-1
	mmm=index(testout,' ')-1

	write(0,*) 'file1: ', file1(1:mm)
	write(0,*) 'file2: ', file2(1:nn)
	write(0,*) 'testout: ', testout(1:mmm)
	write(0,*) 'file1(mm-12:mm-7): ', file1(mm-12:mm-7)

	if (mod(ihrs1,3) .eq. 0 .and. reset_flag .eq. 1) then
	reset_flag=1
	else
	reset_flag=0
	endif

        write(0,*) 'call calc_pdiff with reset_flag: ', reset_flag

	call calc_pdiff(file1(1:mm),file2(1:nn),TESTOUT(1:mmm),
     &                  pdiff,reset_flag,ihrs1,interv,IM,JM)

	enddo

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	SUBROUTINE CALC_PDIFF(FNAME1,FNAME2,TESTOUT,DPRECIP,
     &                           reset_flag,ihrs1,interv,IM,JM)
C	include "parmeta"
        USE GRIB_MOD
        USE pdstemplates
	character(*):: FNAME1,FNAME2,testout
	integer:: JPDS(200),JGDS(200), reset_flag,lencheck,ihrs1,interv
	integer:: KPDS1(200),KGDS(200),KPDS2(200),KPDS(200),KGDS2(200)
	logical:: BITMAP(IM*JM),FIRST

C grib2
      INTEGER :: LUGB,LUGI,J,JDISC,JPDTN,JGDTN
      INTEGER,DIMENSION(:) :: JIDS(200),JPDT(200),JGDT(200)
      INTEGER,DIMENSION(:) :: PDS_SNOW_HOLD(200),PDS_RAIN_HOLD(200)
      INTEGER,DIMENSION(:) :: PDS_SNOW_HOLD_EARLY(200),
     &                        PDS_RAIN_HOLD_EARLY(200)
      LOGICAL :: UNPACK
      INTEGER :: K,IRET
      TYPE(GRIBFIELD) :: GFLD
C grib2
	real:: p_later(IM*JM),p_earlier(IM*JM),dprecip(im*jm)
	real:: s_later(IM*JM),s_earlier(IM*JM),sprecip(im*jm)
	

	call baopenr(11,fname1,ierr1)
	call baopenr(12,fname2,ierr2)
	call baopenw(13,testout,ierr3)

	write(0,*) 'baopened ', fname1,fname2,testout

	if ( (ierr1+ierr2+ierr3) .ne. 0) then
		write(0,*) 'bad baopen!!! ', ierr1
		write(0,*) 'bad baopen!!! ', ierr2
		write(0,*) 'bad baopen!!! ', ierr3
		STOP
	endif
	
	p_earlier=0.
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
        JPDTN=8
        JPDT=-9999
        JPDT(2)=8
        JGDTN=-1
        JGDT=-9999
        UNPACK=.true.

        call getgb2(11,0,J,0,JIDS,JPDTN,JPDT,JGDTN,JGDT,
     &     UNPACK,K,GFLD,IRET)

!        write(0,*) 'pulled gfld%idsect(1:10): ', gfld%idsect(1:10)
        write(0,*) 'pulled gfld%ipdtnum: ', gfld%ipdtnum
        do J=1,29
        write(0,*) 'J,gfld%ipdtmpl(J): ', J,gfld%ipdtmpl(J)
        enddo

!        write(0,*) 'pulled gfld%igdtnum : ', gfld%igdtnum 

	if (IRET .ne. 0) then
	write(0,*) 'bad getgb1 earlier ', IRET
	STOP
	endif


        p_earlier=gfld%fld
        do K=1,gfld%ipdtlen
        PDS_RAIN_HOLD_EARLY(K)=gfld%ipdtmpl(K)
        enddo
        write(0,*) 'maxval(p_earlier): ', maxval(p_earlier)

        JIDS=-9999
        JPDTN=1
        JPDTN=8
        JPDT=-9999
        JPDT(2)=13
        JGDTN=-1
        JGDT=-9999

        call getgb2(11,0,0,0,JIDS,JPDTN,JPDT,JGDTN,JGDT,
     &     UNPACK,K,GFLD,IRET)

        write(0,*) 'K: ', K

	if (IRET .ne. 0) then
	write(0,*) 'bad getgb snow earlier ', IRET
	STOP
	endif

        s_earlier=gfld%fld

        do K=1,gfld%ipdtlen
        PDS_SNOW_HOLD_EARLY(K)=gfld%ipdtmpl(K)
        enddo

        write(0,*) 'maxval(gfld%fld) for snow earlier: ', 
     &                maxval(gfld%fld)

        JIDS=-9999
        JPDTN=8
        JPDT=-9999
        JPDT(2)=8
        JGDTN=-1
        JGDT=-9999


        call getgb2(12,0,0,0,JIDS,JPDTN,JPDT,JGDTN,JGDT,
     &     UNPACK,K,GFLD,IRET1)

        write(0,*) 'K: ', K

	if (IRET1 .ne. 0) then
	 write(0,*) 'bad getgb later ', IRET1
	STOP
	endif

        write(0,*) 'set p_later to gfld%fld'
        p_later=gfld%fld
        write(0,*) 'maxval(p_later): ', maxval(p_later)

        do K=1,gfld%ipdtlen
        PDS_RAIN_HOLD(K)=gfld%ipdtmpl(K)
        enddo


        JIDS=-9999
        JPDTN=8
        JPDT=-9999
        JPDT(2)=13
        JGDTN=-1
        JGDT=-9999

        write(0,*) 'call getgb2 again'

        call getgb2(12,0,0,0,JIDS,JPDTN,JPDT,JGDTN,JGDT,
     &     UNPACK,K,GFLD,IRET1)

        write(0,*) 'K: ', K

	if (IRET1 .ne. 0) then
	 write(0,*) 'bad getgb snow later ', IRET1
	STOP
	endif

        write(0,*) 'set s_later to gfld%fld'
        s_later=gfld%fld

        do K=1,gfld%ipdtlen
        PDS_SNOW_HOLD(K)=gfld%ipdtmpl(K)
        enddo

        write(0,*) 'maxval(s_later): ', maxval(s_later)

	if (reset_flag .eq. 1) then
	write(0,*) 'just later value'

	do NPT=1,IM*JM
	dprecip(NPT)=p_later(NPT)
	sprecip(NPT)=s_later(NPT)
	enddo

	else

	write(0,*) 'take normal difference ', IM*JM

	do NPT=1,IM*JM
	dprecip(NPT)=p_later(NPT)-p_earlier(NPT)
	sprecip(NPT)=s_later(NPT)-s_earlier(NPT)
	enddo

	endif



!! 	force decimal scaling
!	KPDS2(22)=4
!! 	force decimal scaling


        write(0,*) 'define gfld%fld with dprecip'

!        do J=1,gfld%ipdtlen

!        if (PDS_RAIN_HOLD(J) .ne. PDS_RAIN_HOLD_EARLY(J)) then
!        write(0,*) 'rain diff, J, early, later: ', 
!     &   J, PDS_RAIN_HOLD_EARLY(J), PDS_RAIN_HOLD(J)
!        endif

!        write(0,*) 'J, PDS_RAIN_HOLD_EARLY, PDS_RAIN_HOLD: ', 
!     &       J, PDS_RAIN_HOLD_EARLY(J), PDS_RAIN_HOLD(J)

!        if (PDS_SNOW_HOLD(J) .ne. PDS_SNOW_HOLD_EARLY(J)) then
!        write(0,*) 'snow diff, J, early, later: ', 
!     &   J, PDS_SNOW_HOLD_EARLY(J), PDS_SNOW_HOLD(J)
!        endif
         
!        enddo
        


        gfld%ipdtmpl=PDS_RAIN_HOLD_EARLY
        gfld%ipdtmpl(9)=ihrs1
        do J=16,21
        gfld%ipdtmpl(J)=PDS_RAIN_HOLD(J)
        enddo

        gfld%ipdtmpl(22)=1
        gfld%ipdtmpl(27)=interv

        write(0,*) 'interval specified in 27: ', interv

!        gfld%ipdtmpl(28)=1

        gfld%fld=dprecip

        do J=1,29
        write(0,*) 'at write J,gfld%ipdtmpl(J): ', J,gfld%ipdtmpl(J)
        enddo

	call putgb2(13,GFLD,IRET)
        write(0,*) 'IRET from putgb2 for dprecip', IRET

! -------------------------------------------

        gfld%ipdtmpl=PDS_SNOW_HOLD_EARLY
        gfld%ipdtmpl(9)=ihrs1
        do J=16,21
        gfld%ipdtmpl(J)=PDS_RAIN_HOLD(J)
        enddo
        gfld%ipdtmpl(22)=1
        gfld%ipdtmpl(27)=interv
        gfld%fld=sprecip
	call putgb2(13,GFLD,IRET)
        write(0,*) 'IRET from putgb2 for sprecip ', IRET

        call baclose(13,IRET)

	write(0,*) 'extremes of precip,snow: ', 
     +		maxval(dprecip),maxval(sprecip)

  633	format(25(f4.1,1x))

	end subroutine calc_pdiff
