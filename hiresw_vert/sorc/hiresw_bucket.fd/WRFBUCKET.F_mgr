	program wrfbucket

!	Program will read in the total accumulated precipitation from
!	a series of WRFPRS files, and compute simple differences to
!	get precip buckets with a duration equal to the interval between
!	output times.

!	include "parmeta"

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
	write(6,*) 'read reset_flag: ', reset_flag

	n=index(dirname,' ')-1
	m=index(filename,' ')-1

	do I=2,2
	
	file1= dirname(1:n)//'_'//HRS(I-1)//'/'//filename(1:m)
     +					//HRS(I-1)//'.tm00'
	file2= dirname(1:n)//'_'//HRS(I)//'/'//filename(1:m)//HRS(I)//'.tm00'

	read(HRS(I-1), '(I2)' ) ihrs1
	read(HRS(I)  , '(I2)' ) ihrs2

	interv=ihrs2-ihrs1
	
	if (interv .eq. 3) then
	  testout= dirname(1:n)//'_'//HRS(I)//'/PCP3HR'//HRS(I)//'.tm00'
	elseif (interv .eq. 1) then
	  testout= dirname(1:n)//'_'//HRS(I)//'/PCP1HR'//HRS(I)//'.tm00'
	elseif (interv .eq. 6) then
	  testout= dirname(1:n)//'_'//HRS(I)//'/PCP6HR'//HRS(I)//'.tm00'
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
     &                  pdiff,reset_flag,IM,JM)

	enddo

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	SUBROUTINE CALC_PDIFF(FNAME1,FNAME2,TESTOUT,DPRECIP,reset_flag,IM,JM)
C	include "parmeta"
	character(*):: FNAME1,FNAME2,testout
	integer:: JPDS(200),JGDS(200), reset_flag
	integer:: KPDS1(200),KGDS(200),KPDS2(200),KPDS(200),KGDS2(200)
	logical:: BITMAP(IM*JM),FIRST
	real:: p_later(IM*JM),p_earlier(IM*JM),dprecip(im*jm)
	real:: s_later(IM*JM),s_earlier(IM*JM),sprecip(im*jm)
!	real:: c_later(IM*JM),c_earlier(IM*JM),dconv(im*jm)
!	real:: nc_later(IM*JM),nc_earlier(IM*JM),dnonconv(im*jm)
	

	call baopen(11,fname1,ierr1)
	call baopen(12,fname2,ierr2)
	call baopen(13,testout,ierr3)

C	write(6,*) 'baopened ', fname1,fname2,testout

	if ( (ierr1+ierr2+ierr3) .ne. 0) then
		write(6,*) 'bad baopen!!! ', ierr1
		write(6,*) 'bad baopen!!! ', ierr2
		write(6,*) 'bad baopen!!! ', ierr3
		STOP
	endif
	
	jpds=-1
	jgds=-1

	jpds(5)=61
	jpds(6)=1
	jpds(7)=0

	p_earlier=0.
	nc_earlier=0.
	c_earlier=0.

        call getgb(11,0,im*jm,0,JPDS,JGDS,nwords,KNUM,KPDS1,KGDS,
     &     BITMAP,p_earlier,IRET1)
	if (IRET1 .ne. 0) then
	write(6,*) 'bad getgb1 earlier ', IRET1
	STOP
	endif

	jpds(5)=65
        call getgb(11,0,im*jm,0,JPDS,JGDS,nwords,KNUM,KPDS1,KGDS,
     &     BITMAP,s_earlier,IRET1)
	if (IRET1 .ne. 0) then
	write(6,*) 'bad getgb1 snow earlier ', IRET1
	STOP
	endif

!	jpds(5)=63
!        call getgb(11,0,im*jm,0,JPDS,JGDS,nwords,KNUM,KPDS1,KGDS,
!     &     BITMAP,c_earlier,IRET1)
!	if (IRET1 .ne. 0) then
!	write(6,*) 'bad getgb1 conv earlier ', IRET1
!	STOP
!	endif

!	nc_earlier=p_earlier-c_earlier

	jpds=-1
	jgds=-1

	jpds(5)=61
	jpds(6)=1
	jpds(7)=0

        call getgb(12,0,im*jm,0,JPDS,JGDS,nwords,KNUM,KPDS2,KGDS2,
     &     BITMAP,p_later,IRET1)
	if (IRET1 .ne. 0) then
	 write(6,*) 'bad getgb later ', IRET1
	STOP
	endif

	jpds(5)=65
        call getgb(12,0,im*jm,0,JPDS,JGDS,nwords,KNUM,KPDS2,KGDS2,
     &     BITMAP,s_later,IRET1)
	if (IRET1 .ne. 0) then
	 write(6,*) 'bad getgb snow later ', IRET1
	STOP
	endif

!	jpds(5)=63
!        call getgb(12,0,im*jm,0,JPDS,JGDS,nwords,KNUM,KPDS2,KGDS2,
!     &     BITMAP,c_later,IRET1)
!	if (IRET1 .ne. 0) then
!	 write(6,*) 'bad getgb conv later ', IRET1
!	STOP
!	endif

!	nc_later=p_later-c_later

	if (reset_flag .eq. 1) then
	write(0,*) 'just later value'

	do NPT=1,IM*JM
	dprecip(NPT)=p_later(NPT)
	sprecip(NPT)=s_later(NPT)
	enddo

	else

	write(0,*) 'take normal difference'
	do NPT=1,IM*JM
	dprecip(NPT)=p_later(NPT)-p_earlier(NPT)
	sprecip(NPT)=s_later(NPT)-s_earlier(NPT)
!	dnonconv(NPT)=nc_later(NPT)-nc_earlier(NPT)
!	dconv(NPT)=c_later(NPT)-c_earlier(NPT)
	enddo

	endif


	KPDS2(14)=KPDS1(15)
	KPDS2(16)=4

	do N=1,23
!	write(6,*) 'N,KPDS(N): ', N,KPDS2(N)
	enddo

!! 	force decimal scaling
	KPDS2(22)=4
!! 	force decimal scaling

!	write(6,*) 'nwords, im*jm: ', nwords, im*jm

	KPDS2(5)=61
	call putgb(13,NWORDS,KPDS2,KGDS2,BITMAP,DPRECIP,IRET)
	KPDS2(5)=65
	call putgb(13,NWORDS,KPDS2,KGDS2,BITMAP,SPRECIP,IRET)
!	KPDS2(5)=63
!	call putgb(13,NWORDS,KPDS2,KGDS2,BITMAP,DCONV,IRET)

!	write(6,*) 'extremes of precip, non-conv, and conv: ', 
!     +		maxval(dprecip),maxval(dnonconv),maxval(dconv)
	write(6,*) 'extremes of precip,snow: ', 
     +		maxval(dprecip),maxval(sprecip)

  633	format(25(f4.1,1x))

	end subroutine calc_pdiff
