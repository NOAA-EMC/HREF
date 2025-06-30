	program wrfbucket

!	Program will read in the total accumulated precipitation from
!	a series of WRFPRS files, and compute simple differences to
!	get precip buckets with a duration equal to the interval between
!	output times.

        USE GRIB_MOD

!	real pdiff(im,jm)
        real, allocatable :: pdiff(:,:)

        integer :: ihrs1, ihrs2, reset_flag,JPDTN_USE
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
        read(5,*) JPDTN_USE

        allocate(pdiff(im,jm))

	write(0,*) 'read reset_flag: ', reset_flag
        write(*,*) 'read JPDTN_USE: ', JPDTN_USE

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
	elseif (interv .eq. 6) then
	  testout= dirname(1:n)//'/PCP6HR'//HRS(I)//'.tm00'
	endif

	mm=index(file1,' ')-1
	nn=index(file2,' ')-1
	mmm=index(testout,' ')-1

	write(0,*) 'file1: ', file1(1:mm)
	write(0,*) 'file2: ', file2(1:nn)
	write(0,*) 'testout: ', testout(1:mmm)

!	if (mod(ihrs2,3) .eq. 0 .and. reset_flag .eq. 1) then
!	reset_flag=1
!	else
!	reset_flag=0
!	endif

        write(0,*) 'call calc_pdiff with reset_flag: ', reset_flag

	call calc_pdiff(file1(1:mm),file2(1:nn),TESTOUT(1:mmm),
     &                  pdiff,reset_flag,ihrs1,interv,IM,JM,
     &                  JPDTN_USE)

	enddo

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	SUBROUTINE CALC_PDIFF(FNAME1,FNAME2,TESTOUT,DPRECIP,
     &                           reset_flag,ihrs1,interv,IM,JM,
     &                           JPDTN)
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
      INTEGER,DIMENSION(:) :: PDS_RAIN_HOLD(200)
      INTEGER,DIMENSION(:) :: PDS_RAIN_HOLD_EARLY(200)
      INTEGER,DIMENSION(:) :: PDS_FRZR_HOLD(200)
      INTEGER,DIMENSION(:) :: PDS_FRZR_HOLD_EARLY(200)
      INTEGER :: intv_rec,time_s_rec,time_e_rec,num_time_rec
      LOGICAL :: UNPACK
      INTEGER :: K,IRET
      TYPE(GRIBFIELD) :: GFLD
C grib2
	real:: p_later(IM*JM),p_earlier(IM*JM),dprecip(im*jm)
	real:: frzr_later(IM*JM),frzr_earlier(IM*JM),dfrzr(im*jm)
	
	call baopenr(11,fname1,ierr1)
	call baopenr(12,fname2,ierr2)
	call baopenw(13,testout,ierr3)

	write(0,*) 'baopened ', fname1,fname2,testout

	if ( (ierr1+ierr2+ierr3) .ne. 0) then
		write(0,*) 'bad baopen!!! ', ierr1
		write(0,*) 'bad baopen!!! ', ierr2
		write(0,*) 'bad baopen!!! ', ierr3
		STOP 9
	endif
	
	p_earlier=0.

        JIDS=-9999
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


	if (JPDTN .eq. 11) then
         write(*,*) 'ipdtnum 11 block'
	intv_rec=30
        time_s_rec=19
        time_e_rec=24
        num_time_rec=25

	elseif (JPDTN .eq. 8) then
         write(*,*) 'ipdtnum 8 block'

	intv_rec=27
        time_s_rec=16 
        time_e_rec=21
        num_time_rec=22

        else

	write(*,*) 'unexpected ipdtnum: ', gfld%ipdtnum

        endif



C USAGE:    CALL GETGB2(LUGB,LUGI,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,
C    &                  UNPACK,K,GFLD,IRET)

        J=0

        if ( reset_flag .eq. 0) then

        JIDS=-9999
        JPDT=-9999
        JPDT(1)=1
        JPDT(2)=8
! try force getting the total from 0-h?
        JPDT(9)=0
        JGDTN=-1
        JGDT=-9999
        UNPACK=.true.

	write(*,*) 'JPDT(1:10): ', JPDT(1:10)

        call getgb2(11,0,J,0,JIDS,JPDTN,JPDT,JGDTN,JGDT,
     &     UNPACK,K,GFLD,IRET)

        write(0,*) 'IRET, pulled gfld%ipdtnum: ', IRET, gfld%ipdtnum
        write(0,*) 'early apcp K: ', K
        write(0,*) 'gfld%ipdtmpl(1:10): ', gfld%ipdtmpl(1:10)

	if (IRET .ne. 0) then
	write(0,*) 'bad getgb2 earlier ', IRET
	STOP 99
	endif

        p_earlier=gfld%fld
        do K=1,gfld%ipdtlen
        PDS_RAIN_HOLD_EARLY(K)=gfld%ipdtmpl(K)

	if (K .le. 10) then
	write(0,*) 'K, PDS_RAIN_HOLD_EARLY(K): ', K, PDS_RAIN_HOLD_EARLY(K)
	endif

        enddo
        write(0,*) 'maxval(p_earlier): ', maxval(p_earlier)

        J=0
        JIDS=-9999
        JPDT=-9999
        JPDT(1)=1
        JPDT(2)=225
! try force getting the 0-hr total
        JPDT(9)=0
        JGDTN=-1
        JGDT=-9999
        UNPACK=.true.

        call getgb2(11,0,J,0,JIDS,JPDTN,JPDT,JGDTN,JGDT,
     &     UNPACK,K,GFLD,IRET)

	if (IRET .ne. 0) then
	write(0,*) 'bad getgb2 earlier for FRZR ', IRET
	STOP 99
	endif

        frzr_earlier=gfld%fld
        do K=1,gfld%ipdtlen
        PDS_FRZR_HOLD_EARLY(K)=gfld%ipdtmpl(K)
!	write(0,*) 'K, PDS_FRZR_HOLD_EARLY(K): ', K, PDS_RAIN_HOLD_EARLY(K)
        enddo
        write(0,*) 'maxval(frzr_earlier): ', maxval(frzr_earlier)

        endif  ! make sure reset_flag = 0 

! later apcp
        J=0
        JIDS=-9999
        JPDT=-9999
        JPDT(1)=1
        JPDT(2)=8
! try force getting the 0-hr total
        JPDT(9)=0
!        JPDT(intv_rec)=interv
        write(*,*) 'intv_rec,JPDT(intv_rec):',intv_rec,JPDT(intv_rec)
        JGDTN=-1
        JGDT=-9999

	write(*,*) 'JPDT(1:intv_rec): ',JPDT(1:intv_rec)
        write(0,*) 'JPDTN: ', JPDTN

        call getgb2(12,0,J,0,JIDS,JPDTN,JPDT,JGDTN,JGDT,
     &     UNPACK,K,GFLD,IRET1)

	if (IRET1 .ne. 0) then
         write(0,*) 'p_later K: ', K
	 write(0,*) 'bad APCP getgb later ', IRET1
	STOP 999
	endif

        write(0,*) 'set p_later to gfld%fld'
        p_later=gfld%fld
        write(0,*) 'maxval(p_later): ', maxval(p_later)

        do K=1,gfld%ipdtlen
        PDS_RAIN_HOLD(K)=gfld%ipdtmpl(K)
        enddo

	if (reset_flag .eq. 1) then
	write(0,*) 'just later value'
        PDS_RAIN_HOLD_EARLY=PDS_RAIN_HOLD

	do NPT=1,IM*JM
	dprecip(NPT)=p_later(NPT)
	enddo

	else

	write(0,*) 'take normal difference ', IM*JM

	do NPT=1,IM*JM
	dprecip(NPT)=p_later(NPT)-p_earlier(NPT)
	enddo

	endif


! later frzr
        J=0
        JIDS=-9999
        JPDT=-9999
        JPDT(2)=225
! try force getting the 0-hr total
        JPDT(9)=0
!        JPDT(intv_rec)=interv
        JGDTN=-1
        JGDT=-9999

        call getgb2(12,0,J,0,JIDS,JPDTN,JPDT,JGDTN,JGDT,
     &     UNPACK,K,GFLD,IRET1)

        write(0,*) 'K: ', K

	if (IRET1 .ne. 0) then
	 write(0,*) 'bad FRZR getgb later ', IRET1
	STOP 999
	endif

        write(0,*) 'set frzr_later to gfld%fld'
        frzr_later=gfld%fld
        write(0,*) 'maxval(frzr_later): ', maxval(frzr_later)

        do K=1,gfld%ipdtlen
        PDS_FRZR_HOLD(K)=gfld%ipdtmpl(K)
        enddo

	if (reset_flag .eq. 1) then
	write(0,*) 'just later value'
        PDS_FRZR_HOLD_EARLY=PDS_FRZR_HOLD

	do NPT=1,IM*JM
	dfrzr(NPT)=frzr_later(NPT)
	enddo

	else

	write(0,*) 'take normal difference for frzr ', IM*JM

	do NPT=1,IM*JM
	dfrzr(NPT)=frzr_later(NPT)-frzr_earlier(NPT)
	enddo

	endif

        write(0,*) 'define gfld%fld with dprecip'

        do K=1,gfld%ipdtlen
        gfld%ipdtmpl(K)=PDS_RAIN_HOLD_EARLY(K)
        if (K .le. 10) then
 	write(*,*) 'K, gfld%ipdtmpl(K) for rain: ', gfld%ipdtmpl(K)
        endif
        enddo

        gfld%ipdtmpl(9)=ihrs1

! should this be redefined?
        do J=time_s_rec,time_e_rec
        gfld%ipdtmpl(J)=PDS_RAIN_HOLD(J)
        enddo

        gfld%ipdtmpl(num_time_rec)=1
        gfld%ipdtmpl(intv_rec)=interv

        write(0,*) 'interval specified in intv_rec: ', interv

        write(*,*) 'APCP gfld%ipdtmpl(1:intv_rec) at write: ', 
     +              gfld%ipdtmpl(1:intv_rec)

        gfld%fld=dprecip

	call putgb2(13,GFLD,IRET)

        write(0,*) 'define gfld%fld with dfrzr'

        do K=1,gfld%ipdtlen
        gfld%ipdtmpl(K)=PDS_FRZR_HOLD_EARLY(K)
        enddo

        gfld%ipdtmpl(9)=ihrs1
      
        do J=time_s_rec,time_e_rec
        gfld%ipdtmpl(J)=PDS_FRZR_HOLD(J)
        enddo

        gfld%ipdtmpl(num_time_rec)=1
        gfld%ipdtmpl(intv_rec)=interv

        write(0,*) 'interval specified in intv_rec: ', interv

        gfld%fld=dfrzr

	call putgb2(13,GFLD,IRET)

        write(0,*) 'IRET from putgb2 for dprecip', IRET

! -------------------------------------------

        call baclose(13,IRET)

	write(*,*) 'extremes of precip: ', 
     +		maxval(dprecip)
	write(*,*) 'extremes of frzr: ', 
     +		maxval(dfrzr)

  633	format(25(f4.1,1x))

	end subroutine calc_pdiff
