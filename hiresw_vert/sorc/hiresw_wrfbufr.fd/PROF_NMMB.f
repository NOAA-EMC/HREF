      SUBROUTINE PROF_NMMB(filename,ITAG,INCR)
C
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C   SUBROUTINE:  PROF        PROFILE SOUNDINGS
C   PRGRMMR: BLACK           ORG: W/NP22     DATE: 99-04-22
C
C ABSTRACT:  THIS ROUTINE GENERATES THE RAW PROFILE SOUNDING
C            OUTPUT FILES FROM THE FORECAST RESTRT FILE AND
C            AUXILIARY FILES
C
C PROGRAM HISTORY LOG:
C   99-04-22  T BLACK - ORIGINATOR
C   02-07-01  G MANIKIN - FIXED PROBLEM WITH DHCNVC AND DHRAIN
C                          COMPUTATIONS - SEE COMMENTS BELOW
C   03-04-01  M PYLE - BEGAN CONVERTING FOR WRF
C   04-05-26  M PYLE - MADE CHANGES FOR WRF-NMM
C   08-09-11  M PYLE - NMMB VERSION READING NEMSIO
C   13-11-07  M PYLE - fold in parallel NEMSIO read from post
C
C USAGE:  CALL PROF FROM PROGRAM POST0
C
C   INPUT ARGUMENT LIST:
C     NHB    - THE UNIT NUMBER FOR READING THE NHB FILE
C     LRSTRT - THE UNIT NUMBER FOR READING THE RESTRT FILE
C     ITAG   - THE FORECAST HOUR WE ARE DEALING WITH
C     LCLAS1 - THE UNIT NUMBER FOR WRITING THE PROFILE DATA
C
C   OUTPUT ARGUMENT LIST:
C     NONE
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:
C
C-----------------------------------------------------------------------
c      use vrbls3d
c      use vrbls2d
c      use soil
c      use masks
       use kinds, only             : i_llong
!       use nemsio_module
       use nemsio_module_mpi
C
      include 'wrf_io_flags.h'
      include 'mpif.h'

!      INCLUDE "parmeta"
      INCLUDE "parmsoil"
C-----------------------------------------------------------------------
                             P A R A M E T E R
     & (NSTAT=1500,LCL1ML=15,LCL1SL=52
     &, D608=0.608)
C-----------------------------------------------------------------------
C
C    PARMS FOR HOURLY PROFILER OUTPUT
C      NSTAT - MAX NUMBER OF STATIONS
C      NWORDM - DIMENSION OF OUTPUT ARRAY, MUST BE LARGE ENOUGH
C          TO HOLD ALL VARIABLES
C          (MAX NO MULTI-LAYER VARIABLES*LM + NO OF SINGLE LAYER VARS)
C      LCL1ML - NUMBER OF MULTI-LAYER VARIABLES OUTPUT FOR CLASS 1
C      LCL1SL - NUMBER OF SINGLE LAYER VARIABLES OUTPUT FOR CLASS 1
C
C------------------------------------------------------------------------
                             P A R A M E T E R
     & (ITB=76,JTB=134)
                             P A R A M E T E R
     & (A2=17.2693882,A3=273.16,A4=35.86,PQ0=379.90516,DTR=1.74532925E-2
     &, G=9.81,GI=1./G,RD=287.04,CP=1004.6,CAPA=RD/CP,RHCRIT=0.9999)
C
      PARAMETER (GAMMA=6.5/1000.,ZSL=0.0)
      PARAMETER (TAUCR=RD*GI*290.66,CONST=0.005*G/RD)
      PARAMETER (GORD=G/RD,DP=60.E2)
C------------------------------------------------------------------------

      type(nemsio_gfile) :: nfile, nfile_old
      character(len=20) :: VarName
      character(len=20) :: VcoordName
      character*8,allocatable:: recname(:)
      character*16,allocatable  :: reclevtyp(:)
      integer,allocatable:: reclev(:)
      real, allocatable:: tmp(:)


                             R E A L
     & STNLAT(NSTAT),STNLON(NSTAT)
                             I N T E G E R
     & IDSTN(NSTAT),IHINDX(NSTAT),JHINDX(NSTAT)
     &,             IVINDX(NSTAT),JVINDX(NSTAT)

        REAL, ALLOCATABLE:: DETA(:),RDETA(:),AETA(:),UL(:)
     &,RES(:),FIS(:),THS(:),HBOT(:)
     &,CFRACL(:),CFRACM(:),CFRACH(:),SNO(:)
     &,SOILTB(:),SFCEXC(:),SMSTAV(:),SMSTOT(:)
     &,Z0(:),CZEN(:),CZMEAN(:),SR(:)
     &,ACPREC(:),CUPREC(:),ACSNOW(:),ACSNOM(:)
     &,SSROFF(:),BGROFF(:),SFCSHX(:),SFCLHX(:)
     &,SUBSHX(:),SNOPCX(:),ASWIN(:),ASWOUT(:)
     &,ASWTOA(:),ALWIN(:),ALWOUT(:),ALWTOA(:)
     &,TSHLTR(:),TSHLTR_hold(:),QSHLTR(:),PSHLTR(:)
     &,TH10(:),Q10(:),U10(:),V10(:)
     &,TLMIN(:),TLMAX(:),AVRAIN(:),APHTIM(:),ACUTIM(:)
     &,SMC(:,:),CMC(:),STC(:,:),SH2O(:,:)
     &,VEGFRC(:),POTFLX(:),PSLP(:),PDSL1(:)
     &,EGRID2(:),SM(:),SICE(:)
     &,HBM2(:),FACTR(:)
     &,PTBL(:,:),TTBL(:,:),VEGFRA(:)
     &,T(:,:),Q(:,:),U(:,:),V(:,:),Q2(:,:)
     &,OMGALF(:,:),CWM(:,:),TRAIN(:,:),TCUCN(:,:)
     &,F_RAIN(:,:),F_ICE(:,:),CLDFRA(:,:)
     &,F_RIMEF(:,:)
     &,RSWTT(:,:),RLWTT(:,:),RTOP(:,:)
     &,OMGA(:,:)
     &,PRODAT(:),FPACK(:)
     &,STATPR(:),STACPR(:),STAEVP(:)
     &,STAPOT(:),STASHX(:),STASUB(:),STAPCX(:)
     &,STASWI(:),STASWO(:),STALWI(:),STALWO(:)
     &,STALWT(:),STASWT(:),STASNM(:),STASRF(:)
     &,STABRF(:),STASNO(:),DHCNVC(:,:),DHRAIN(:,:)
     &,STADHC(:),STADHR(:),CPRATE(:)
     &,ACPREC0(:),CUPREC0(:),SFCLHX0(:),POTFLX0(:)
     &,SFCSHX0(:),SUBSHX0(:),SNOPCX0(:),ASWIN0(:)
     &,ASWOUT0(:),ALWIN0(:),ALWOUT0(:),ALWTOA0(:)
     &,ASWTOA0(:),ACSNOW0(:),ACSNOM0(:),SSROFF0(:)
     &,BGROFF0(:),AVRAIN0(:),APHTIM0(:),ACUTIM0(:)
     &,TCUCN0(:,:),TRAIN0(:,:), glat1d(:),glon1d(:)

!        integer, allocatable:: icnt(:),idsp(:)
       integer :: icnt(0:127),idsp(0:127)
C
        real, allocatable:: DUMSOIL(:),DUMSOIL3(:,:,:)
     &,DUM3D(:,:,:),DUM3D2(:,:,:),DUM3DIKJ(:,:,:)
     &,DUM3D3(:,:,:),DUMMY2(:,:),DUMMY(:,:)
     &,PD(:),PDS(:),GDLAT(:,:),GDLON(:,:)
     &,GDLAT2(:,:),GDLON2(:,:)
     &,PMID(:,:),PINT(:,:)
     &,W(:,:),WH(:,:)
	
	real, allocatable:: CROT(:),SROT(:)

	LOGICAL:: PRINT_DIAG
C------------------------------------------------------------------------
C
        integer, allocatable:: IDUM(:,:),LMH(:,:),IDUMMY(:,:)


                             I N T E G E R
     & IDAT(3),IDAT0(3),GDS(200),fldsize,tmpsize,ITAGPREV
       DATA SPVAL/-9999./
C
C------------------------------------------------------------------------
                             L O G I C A L
     & RUN,RESTRT,FRST
C------------------------------------------------------------------------
                             C H A R A C T E R
     & RSTFIL*98,RESTHR*4,LABEL*32,CISTAT*8,CIDSTN(NSTAT)*8
     &,FNAME*98,ENVAR*98,BLANK*4

C	new stuff
      character(len=31) :: varin
      character(len=256) :: fileName,filename_prev
      character(len=256) :: fileName_alt
      integer :: Status
      character(len=19):: startdate,datestr,datestrold
      character SysDepInfo*80
      character(len=3):: ITAGLAB
      character(len=2):: hrp
      character(len=8):: minp
      character(len=2):: IMINLAB

	real:: rinc(5)
	integer:: IDATE(8),JDATE(8),IDATENEW(8)
        integer:: IDATE7(7)

      integer this_offset, this_length
C------------------------------------------------------------------------
      DATA BLANK/'    '/
      DATA hrp /'h_'/
      DATA minp /'m_00.00s'/
      DATA IMINLAB /'00'/
C------------------------------------------------------------------------
C***
C***  READ IN THE INFORMATION FILE ABOUT THE SOUNDINGS
C***

!	write(0,*) 'filename at top ', filename
c	write(0,*) 'startedate= ', startdate

	datestr=startdate

      REWIND 19
C
      READ(19)NUMSTA,IDSTN,STNLAT,STNLON
     1,       IHINDX,JHINDX,IVINDX,JVINDX,CIDSTN
!      WRITE(0,20)NUMSTA
   20 FORMAT('INIT:  NUMBER OF PROFILE STATIONS ',I5)
!	if (ITAG .eq. 0) then
!      WRITE(0,30)(IDSTN(N),STNLAT(N)/DTR,STNLON(N)/DTR
!    1,               IHINDX(N),JHINDX(N),IVINDX(N),JVINDX(N)
!     2,               CIDSTN(N),N=1,NUMSTA)
!	endif
   30 FORMAT(2X,I6,2F8.2,4I8,4X,A8)

c	if (ITAG .eq. 0) then
	  FRST=.TRUE.
c	else 
c	  FRST=.FALSE.
c	endif

      call mpi_init(ierr)
      call mpi_comm_rank(MPI_COMM_WORLD,mype,ierr)
      call mpi_comm_size(MPI_COMM_WORLD,npes,ierr)

        if (MYPE .eq. 0) then
      WRITE(0,20)NUMSTA

	if (ITAG .eq. 0) then
      WRITE(0,30)(IDSTN(N),STNLAT(N)/DTR,STNLON(N)/DTR
     1,               IHINDX(N),JHINDX(N),IVINDX(N),JVINDX(N)
     2,               CIDSTN(N),N=1,NUMSTA)
	endif

        endif
        

       call nemsio_init()
       call nemsio_open(nfile,trim(filename),'read',
     &                  MPI_COMM_WORLD,iret=iret)

        if (mype .eq. 0) then
        write(0,*) 'iret from curr file open: ', iret
	write(0,*) 'filename after open: ', filename
        endif

!!!!
!        if (ITAG .gt. 0) then
!
!        ITAGPREV=ITAG-INCR
!
!        write(ITAGLAB,302) ITAGPREV
!        if (MYPE .eq. 0) then
!        write(0,*) 'produced ITAGLAB: ', itaglab
!        endif
!         len=lnblnk(filename)
!
!        if (MYPE .eq. 0) then
!        write(0,*) 'carried over part: ', filename(1:len-15)
!        endif
!
!        filename_prev=filename(1:len-15)//ITAGLAB//hrp//IMINLAB//minp
!
!        if (MYPE .eq. 0) then
!        write(0,*) 'filename_prev: ', filename_prev(len-16:len)
!        endif
!
!       call nemsio_open(nfile_old,trim(filename_prev),'read',
!     &                  MPI_COMM_WORLD,iret=iret)
!
!        if (MYPE .eq. 0) then
!        write(0,*) 'open of filename_prev iret: ', iret
!        endif
!
!
!        endif
!
!!!!

       call nemsio_getfilehead(nfile,iret=iret,nrec=nrec)

       allocate(recname(nrec),reclevtyp(nrec),reclev(nrec))


       call nemsio_getfilehead(nfile,iret=iret                           &
     &   ,idate=idate(1:7),nfhour=nfhour,recname=recname                 &
     &   ,reclevtyp=reclevtyp,reclev=reclev,nframe=nframe                &
     &   ,dimx=im,dimy=jm,dimz=lm)

!        write(0,*) 'what is nrec: ', nrec

	impf=im+nframe
	jmpf=jm+nframe
	nframed2=nframe/2

!	do I=1,7
!	write(0,*) 'I,IDATE(I): ', I, IDATE(I)
!	enddo


C Getting start time

!!!! MAP THESE PROPERLY FROM file header info

!         IDATE(2)=imn
!         IDATE(3)=iday
!         IDATE(1)=iyear
!         IDATE(5)=ihrst

C Getting tstart

!      tstart=0.
!      call ext_int_get_dom_ti_real(DataHandle,'TSTART',tmp
!     + ,1,ioutcount,istatus)
!      tstart=tmp
!      print*,'status for getting TSTART= ',istatus
!      IF( abs(istatus-0) .GT. 1)THEN
!       PRINT*,'you do not have tstart in your WRF output,
!     + many grid navigation will be read in incorrectly, STOPPING'
!       TOP
!      END IF
!        print*,'TSTART= ',TSTART
!
!        if(tstart.gt. 1.E-2) THEN
!          itstart=nint(tstart)
!          rinc(2)=-1.0*tstart
!          call w3movdat(rinc,idate,idatenew)
!          print *," old start date= ",idate, tstart
!          print *," new start date= ",idatenew, tstart
!          do ier=1,8
!             idate(ier)=idatenew(ier)
!          enddo
C
C reset imn,iyear,iday,ihrst since they are packed into IDAT which
C is written into the profile output file!
C
          imn=IDATE(2)
          iday=IDATE(3)
          iyear=IDATE(1)
!          ihrst=IDATE(5)
          ihrst=IDATE(4)
C

!        endif


        if (mype .eq. 0) then
        write(0,*) 'to big allocate block'
        endif


!! do parallel instead?

      call para_range(1,jm,npes,mype,  
     &  jsta,jend)

      do i = 0, npes - 1
         call para_range(1,jm,npes,i,jsx,jex)
         icnt(i) = (jex-jsx+1)*im
         idsp(i) = (jsx-1)*im
         if ( mype .eq. 0 ) then
           write(0,*) ' i, icnt(i),idsp(i) = ',i,icnt(i),
     &      idsp(i)
         end if
      end do


! force each task to do full range
! this gets everyone past the barrier

!        jsta=1
!        jend=jm

      jsta_2l = max(jsta - 2,  1 )
      jend_2u = min(jend + 2, jm )

        write(0,*) 'jsta, jend: ', jsta, jend

!  The end j row is going to be jend_2u for all variables except for V.
!	JSTA_2L=1
!	JEND_2U=JM

      JS=JSTA_2L
      JE=JEND_2U
      IF (JEND_2U.EQ.JM) THEN
       JEV=JEND_2U+1
      ELSE
       JEV=JEND_2U
      ENDIF

!        call ext_int_get_dom_ti_real(DataHandle,'DX',tmp
!     + ,1,ioutcount,istatus)
!        dxval=nint(tmp*1000.) ! E-grid dlamda in degree
!        write(0,*) 'dxval= ', dxval

!        call ext_int_get_dom_ti_real(DataHandle,'DY',tmp
!     + ,1,ioutcount,istatus)
!        dyval=nint(1000.*tmp)
!        write(0,*) 'dyval= ', dyval

!        call ext_int_get_dom_ti_real(DataHandle,'DT',tmp
!     + ,1,ioutcount,istatus)
!        DT=tmp
!        write(0,*) 'DT= ', DT

	me=0
      VarName='dt'
      if(me == 0)then
        call nemsio_getheadvar(nfile,trim(VarName),garb,iret)
        if (iret /= 0) then
         print*,VarName," not found in file-Assigned missing values"
         dt=spval
        else
         dt=garb
        end if

        NTSD=INT(0.5+ITAG*3600./dt)
        

        if (NPES .gt. 1) then
        call mpi_bcast(NTSD,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
        endif
!        write(0,*) 'NTSD: ', NTSD

      end if

! former parameter statements
!	write(0,*) 'LM: ', LM
        NWORDM=(LCL1ML+1)*LM+2*LCL1SL
        LRECPR=4*(8+9+LCL1ML*LM+LCL1SL)

        if (mype .eq. 0) then
        write(0,*) 'NWORDM, LRECPR: ', NWORDM, LRECPR
        endif

! former parameter statements

       ALLOCATE( DETA(LM),RDETA(LM),AETA(LM),UL(2*LM)
     &,RES(NUMSTA),FIS(NUMSTA),THS(NUMSTA),HBOT(NUMSTA)
     &,CFRACL(NUMSTA),CFRACM(NUMSTA),CFRACH(NUMSTA),SNO(NUMSTA)
     &,SOILTB(NUMSTA),SFCEXC(NUMSTA),SMSTAV(NUMSTA),SMSTOT(NUMSTA)
     &,Z0(NUMSTA),CZEN(NUMSTA),CZMEAN(NUMSTA),SR(NUMSTA))

       ALLOCATE (ACPREC(NUMSTA),CUPREC(NUMSTA)
     &,ACSNOW(NUMSTA),ACSNOM(NUMSTA)
     &,SSROFF(NUMSTA),BGROFF(NUMSTA),SFCSHX(NUMSTA),SFCLHX(NUMSTA)
     &,SUBSHX(NUMSTA),SNOPCX(NUMSTA),ASWIN(NUMSTA),ASWOUT(NUMSTA)
     &,ASWTOA(NUMSTA),ALWIN(NUMSTA),ALWOUT(NUMSTA),ALWTOA(NUMSTA)
     &,TSHLTR(NUMSTA),TSHLTR_hold(NUMSTA),QSHLTR(NUMSTA),PSHLTR(NUMSTA)
     &,TH10(NUMSTA),Q10(NUMSTA),U10(NUMSTA),V10(NUMSTA)
     &,TLMIN(NUMSTA),TLMAX(NUMSTA),AVRAIN(NUMSTA),APHTIM(NUMSTA)
     &,ACUTIM(NUMSTA)
     &,SMC(NUMSTA,NSOIL),CMC(NUMSTA),STC(NUMSTA,NSOIL)
     &,SH2O(NUMSTA,NSOIL)
     &,VEGFRC(NUMSTA),POTFLX(NUMSTA),PSLP(NUMSTA),PDSL1(NUMSTA)
     &,EGRID2(NUMSTA),SM(NUMSTA),SICE(NUMSTA))

       ALLOCATE(HBM2(NUMSTA),FACTR(NUMSTA)
     &,PTBL(ITB,JTB),TTBL(JTB,ITB),VEGFRA(NUMSTA)
     &,T(NUMSTA,LM),Q(NUMSTA,LM),U(NUMSTA,LM),V(NUMSTA,LM)
     &,Q2(NUMSTA,LM)
     &,OMGALF(NUMSTA,LM),CWM(NUMSTA,LM),TRAIN(NUMSTA,LM)
     &,F_RAIN(NUMSTA,LM),F_ICE(NUMSTA,LM),CLDFRA(NUMSTA,LM)
     &,TCUCN(NUMSTA,LM),F_RIMEF(NUMSTA,LM)
     &,RSWTT(NUMSTA,LM),RLWTT(NUMSTA,LM)
     &,RTOP(NUMSTA,LM)
     &,OMGA(NUMSTA,LM)
     &,PRODAT(NWORDM),FPACK(NWORDM)
     &,STATPR(NUMSTA),STACPR(NUMSTA),STAEVP(NUMSTA)
     &,STAPOT(NUMSTA),STASHX(NUMSTA),STASUB(NUMSTA),STAPCX(NUMSTA)
     &,STASWI(NUMSTA),STASWO(NUMSTA),STALWI(NUMSTA),STALWO(NUMSTA)
     &,STALWT(NUMSTA),STASWT(NUMSTA),STASNM(NUMSTA),STASRF(NUMSTA)
     &,STABRF(NUMSTA),STASNO(NUMSTA),DHCNVC(LM,NUMSTA)
     &,DHRAIN(LM,NUMSTA)
     &,STADHC(LM),STADHR(LM),CPRATE(NUMSTA)
     &,ACPREC0(NUMSTA),CUPREC0(NUMSTA),SFCLHX0(NUMSTA),POTFLX0(NUMSTA)
     &,SFCSHX0(NUMSTA),SUBSHX0(NUMSTA),SNOPCX0(NUMSTA),ASWIN0(NUMSTA)
     &,ASWOUT0(NUMSTA),ALWIN0(NUMSTA),ALWOUT0(NUMSTA),ALWTOA0(NUMSTA)
     &,ASWTOA0(NUMSTA),ACSNOW0(NUMSTA),ACSNOM0(NUMSTA),SSROFF0(NUMSTA)
     &,BGROFF0(NUMSTA),AVRAIN0(NUMSTA),APHTIM0(NUMSTA),ACUTIM0(NUMSTA)
     &,TCUCN0(NUMSTA,LM),TRAIN0(NUMSTA,LM))

!        write(0,*) 'past alloc 1'

       ALLOCATE ( DUMSOIL(NSOIL),DUMSOIL3(IM,NSOIL,JM)
     &,DUM3D(IM,JM,LM),DUM3D2(IM,JM,LM+1),DUM3DIKJ(IM,LM,JM)
     &,DUM3D3(IM,JM,LM+1),DUMMY2(IM,JM),DUMMY(IM,JM)
     &,PD(NUMSTA),PDS(NUMSTA)
     &,PMID(NUMSTA,LM),PINT(NUMSTA,LM+1)
     &,W(NUMSTA,LM+1),WH(NUMSTA,LM) )

!        write(0,*) 'past alloc 2'

        ALLOCATE(GDLAT(IM,JSTA_2L:JEND_2U))
        ALLOCATE(GDLON(IM,JSTA_2L:JEND_2U))

!        write(0,*) 'past alloc 3'

        ALLOCATE(GDLAT2(IM,JSTA:JEND))
        ALLOCATE(GDLON2(IM,JSTA:JEND))

!        write(0,*) 'past alloc 4'

      ALLOCATE(IDUM(IM,JM),LMH(IM,JM),IDUMMY(IM,JM))

!        write(0,*) 'past allocated'

	if (ITAG .eq. 0) then
	    PRINT_DIAG=.TRUE.
	else
	    PRINT_DIAG=.FALSE.
	endif

!        write(0,*) 'allocated'

       HBM2=1.0
        if (mype .eq. 0) then   
	write(0,*) 'filename here: ', filename
        endif


! start reading nemsio files using parallel read
      fldsize=(jend-jsta+1)*im
      tmpsize=fldsize*nrec
      allocate(tmp(tmpsize))

       tmp=0.

!        write(0,*) 'shape(tmp): ', shape(tmp)


!        write(0,*) 'call nemsio_denseread'

        call nemsio_denseread(nfile,1,im,jsta,jend,tmp,iret=iret)


!        write(0,*) 'past nemsio_denseread'

      if(iret/=0)then
        write(0,*) 'failure using mpi io read, stopping'
        write(0,*) 'iret: ', iret
        stop
      end if


      varname='glat'
      VcoordName='sfc'
      L=1

!        write(0,*) 'call assignnemsiovar for glat'

!        write(0,*) 'size(tmp): ', size(tmp)
!        write(0,*) 'fldsize*nrec: ', fldsize*nrec

!        write(0,*) 'call assignnemsiovar'
      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u 
     & ,L,nrec,fldsize,spval,tmp 
     & ,recname,reclevtyp,reclev,VarName,VcoordName 
     & ,gdlat)

!        write(0,*) 'return assignnemsiovar'

        do J=JSTA,JEND
        do I=1,IM
        GDLAT2(I,J)=GDLAT(I,J)
        enddo
        enddo


! fldsize based on jsta,jend
! gdlat dimensioned jsta_2l,jend_2u
        
!        write(0,*) 'maxval(gdlat): ', maxval(gdlat)/dtr

        ii=(1+im)/2
        jj=(1+jm)/2

        dummy=0.75

        if (NPES .gt. 1) then

!        write(0,*) 'call gatherv'
!        write(0,*) 'jsta, gdlat(1,jsta): ', jsta,gdlat(1,jsta)
!        write(0,*) 'fldsize, icnt(mype): ', fldsize, icnt(mype)

         call mpi_gatherv(gdlat2,icnt(mype),MPI_REAL,   
     &    dummy,icnt,idsp,MPI_REAL,0,MPI_COMM_WORLD, ierr )

!        write(0,*) 'ierr for gdlat mpi_gatherv: ', ierr
!        write(0,*) 'dummy(1,jsta): ', dummy(1,jsta)
        
        else

        dummy=gdlat2

        endif


        if (MYPE .eq. 0) then

        if(mod(im,2)==0)then

        write(0,*) 'dummy(ii,jj), dummy(ii+1,jj): ', 
     &              dummy(ii,jj), dummy(ii+1,jj)

        write(0,*) 'dummy(ii+1,jj+1), dummy(ii,jj+1): ', 
     &              dummy(ii+1,jj+1), dummy(ii,jj+1)

          cenlat=nint((dummy(ii,jj)+dummy(ii+1,jj)+
     &                 dummy(ii+1,jj+1)+dummy(ii,jj+1))/4.0*1000.)
        write(0,*) 'cenlat(1): ', cenlat
        else
          cenlat=nint(dummy(ii,jj)*1000.)
        write(0,*) 'cenlat(2): ', cenlat
        end if

        cenlat=cenlat/dtr
	TPH0D=cenlat/1000.

        endif

        if (NPES .gt. 1) then
!        write(0,*) 'everybody calling mpi_bcast for tph0d?'
        call mpi_bcast(tph0d,1,MPI_REAL,0,MPI_COMM_WORLD,ierr)
!        write(0,*) 'ierr from bcast: ', ierr
        endif


      varname='glon'
      VcoordName='sfc'
      l=1

      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u 
     & ,L,nrec,fldsize,spval,tmp 
     & ,recname,reclevtyp,reclev,VarName,VcoordName 
     & ,gdlon)

        do J=JSTA,JEND
        do I=1,IM
        GDLON2(I,J)=GDLON(I,J)
        enddo
        enddo

        if (NPES .gt. 1) then
         call mpi_gatherv(gdlon2(1,jsta),icnt(MYPE),MPI_REAL,   
     &    dummy,icnt,idsp,MPI_REAL,0,MPI_COMM_WORLD, ierr )

        write(0,*) 'ierr for mpi_gatherv: ', ierr
        else
        dummy=gdlon2
        endif

        IF (MYPE .eq. 0) THEN

        if(mod(im,2)==0)then
          cenlon=nint((dummy(ii,jj)+dummy(ii+1,jj)+
     &                 dummy(ii+1,jj+1)+dummy(ii,jj+1))/4.0*1000.)
        write(0,*) 'cenlon(1): ', cenlon
        else
          cenlon=nint(dummy(ii,jj)*1000.)
        write(0,*) 'cenlon(2): ', cenlon
        end if

        cenlon=cenlon/dtr

	TLM0D=cenlon/1000.

        write(0,*) 'tlm0d to bcast: ', tlm0d

        ENDIF

        if (NPES .gt. 1) then
        call mpi_bcast(tlm0d,1,MPI_REAL,0,MPI_COMM_WORLD,ierr)
        endif

        write(0,*) 'everybody tph0d, tlm0d: ', tph0d, tlm0d


  981   continue


!-------------------------------------------------------------------

      VarName='sm'
      VcoordName='sfc'
      l=1

!	write(0,*) 'call getnemsandplace for SM'
!        write(0,*) 'size(SM): ', size(sm)
!        write(0,*) 'have NUMSTA into getnemsandplace_para: ', NUMSTA

      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &                    tmp,fldsize,recname,reclevtyp,
     &                    reclev,nrec,spval,VarName,VcoordName, 
     &                    l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,SM) 

        
        if (mype .eq. 0) then
        write(0,*) 'SM(1:20): ', SM(1:20)
        endif



!-------------------------------------------------------------------

      VarName='sice'
      VcoordName='sfc'
      l=1
!	write(0,*) 'call getnemsandplace_para for SICE'

      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,reclevtyp,
     &               reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,SICE) 



!-------------------------------------------------------------------

      VarName='dpres'
      VcoordName='hybrid sig lev'
      l=1
!	write(0,*) 'call getnemsandplace for dpres'
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &        tmp,fldsize,recname,reclevtyp,
     &        reclev,nrec,spval,VarName,VcoordName, 
     &        l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,PD) 


        if (MYPE .eq. 0) then
	write(0,*) 'PD(1:20) : ', PD(1:20)
        endif

!-------------------------------------------------------------------

      VarName='hgt'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &        tmp,fldsize,recname,reclevtyp,
     &        reclev,nrec,spval,VarName,VcoordName, 
     &        l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,FIS) 

        if (MYPE .eq. 0) then
      do N=1,NUMSTA
	if (mod(N,50) .eq. 0) then
	write(0,*) 'N, hgt(N) before grav mult: ', N, FIS(N)
	endif
        FIS(N)=FIS(N)*g
      enddo
        endif

!-------------------------------------------------------------------

          DO N=1,NUMSTA
	   RES(N)=1.0
          END DO


!-------------------------------------------------------------------


      VarName='tmp'
      VcoordName='mid layer'
!	write(0,*) 'calling getnemsandplace for tmp'
      do L=1,LM

!        write(0,*) 'im,jsta,jend,jsta_2l,jend_2u : ', 
!     &              im,jsta,jend,jsta_2l,jend_2u
!        write(0,*) 'lm, size(tmp),fldsize, size(recname): ', 
!     &              lm, size(tmp),fldsize, size(recname)
!        write(0,*) 'size(reclevtyp,reclev): ', size(reclevtyp), 
!     &            size(reclev)
!        write(0,*) 'nrec, spval: ', nrec, spval
!        write(0,*) 'L, varname, vcoordname: ', L, varname, vcoordname
!        write(0,*) 'NUMSTA: ', NUMSTA
!        write(0,*) 'shape(t): ', shape(T)
!        write(0,*) '------------------- '

      call getnemsandplace_3d_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &               lm,icnt,idsp,tmp,fldsize,recname,
     &               reclevtyp,
     &               reclev,nrec,spval,VarName,VcoordName, 
     &               L,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,T) 

        if (MYPE .eq. 0) then
        write(0,*) 'L, T(1,L): ', L, T(1,L)
        endif

      enddo

!-------------------------------------------------------------------

      VarName='spfh'
      VcoordName='mid layer'
      do L=1,LM
      call getnemsandplace_3d_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &               lm,icnt,idsp,tmp,fldsize,recname,
     &               reclevtyp,
     &               reclev,nrec,spval,VarName,VcoordName, 
     &               L,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,Q) 
      enddo

!-------------------------------------------------------------------



      VarName='ugrd'
      VcoordName='mid layer'
      do L=1,LM
      call getnemsandplace_3d_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &               lm,icnt,idsp,tmp,fldsize,recname,
     &               reclevtyp,
     &               reclev,nrec,spval,VarName,VcoordName, 
     &               L,impf,jmpf,nframed2,NUMSTA,IVINDX,JVINDX,U) 
      enddo

!-------------------------------------------------------------------

      VarName='vgrd'
      VcoordName='mid layer'
      do L=1,LM
      call getnemsandplace_3d_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &               lm,icnt,idsp,tmp,fldsize,recname,
     &               reclevtyp,
     &               reclev,nrec,spval,VarName,VcoordName, 
     &               L,impf,jmpf,nframed2,NUMSTA,IVINDX,JVINDX,V) 
        if (MYPE .eq. 0) then
	write(0,*) 'L,T,Q,U,V(13,L):',L,T(13,L),Q(13,L),
     &                               U(13,L),V(13,L)
        endif
      enddo

!-------------------------------------------------------------------


! hardwired nemsio value may be garbage

!        varname='PDTOP'
!      call nemsio_getheadvar(nfile,trim(varname),PDTOP,iret)
!	write(0,*) 'pulled back PDTOP from header: ', PDTOP

!-------------------------------------------------------------------

        varname='pt'
      call nemsio_getheadvar(nfile,trim(varname),PT,iret)
!	write(0,*) 'pulled back PT from header: ', PT

!-------------------------------------------------------------------

      varname='zorl'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,Z0) 

!-------------------------------------------------------------------

      varname='ths'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,THS) 

!-------------------------------------------------------------------

      varname='acprec ' ! accum total precip
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,ACPREC) 

        if (MYPE .eq. 0) then
        write(0,*) 'maxval(acprec): ', maxval(acprec)
        write(0,*) 'found ACPREC(330): ', ACPREC(330)
        endif

        

!-------------------------------------------------------------------

      varname='cuprec' ! accum cumulus precip
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,CUPREC) 

!-------------------------------------------------------------------

      varname='th10'
      VcoordName='10 m above gnd'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,TH10) 

!-------------------------------------------------------------------

      varname='q10'
      VcoordName='10 m above gnd'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,Q10) 

!-------------------------------------------------------------------

      varname='pshltr'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,PSHLTR) 

!-------------------------------------------------------------------

      varname='tshltr'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &          reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &          l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,TSHLTR_hold) 

!-------------------------------------------------------------------

      varname='qshltr'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,QSHLTR) 

        if (MYPE .eq. 0) then
      DO N=1,NUMSTA
!           write(0,*) 'N, TSHLTR_hold(N),PSHLTR(N):',N,TSHLTR_hold(N),
!     &			PSHLTR(N)
	   QSHLTR(N) = QSHLTR(N) / (1.0 + QSHLTR(N))
      ENDDO
        endif


!-------------------------------------------------------------------
!
! Very confusing story ...
!
! Retrieve hbot => It is named CNVBOT in the model and
! with HBOTS (shallow conv) and HBOTD (deep conv) represent
! the 3 sets of convective cloud base/top arrays tied to the frequency
! that history files are written.
!
! IN THE *MODEL*, array HBOT is similar to CNVBOT but is
! used in radiation and is tied to the frequency of radiation updates.
!
! For historical reasons model array CNVBOT is renamed HBOT
! and manipulated throughout the sounding post.
!
      VarName='cnvbot'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,HBOT) 

      DO N=1,NUMSTA
           HBOT(N)= float(LM)-HBOT(N)+1.0
      END DO

!-------------------------------------------------------------------

      VarName='q2'
      VcoordName='mid layer'
      do L=1,LM
      call getnemsandplace_3d_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &               lm,icnt,idsp,tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,Q2) 
      enddo

!-------------------------------------------------------------------

      varname='czen'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,CZEN) 

!-------------------------------------------------------------------

      varname='czmean'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,CZMEAN) 

!-------------------------------------------------------------------

      varname='clwmr'
      VcoordName='mid layer'
      do L=1,LM
      call getnemsandplace_3d_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &               lm,icnt,idsp,tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,CWM) 
      enddo

!-------------------------------------------------------------------

      varname='f_ice'
      VcoordName='mid layer'
      do L=1,LM
      call getnemsandplace_3d_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &               lm,icnt,idsp,tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,F_ICE) 
      enddo

!-------------------------------------------------------------------

      varname='f_rain'
      VcoordName='mid layer'
      do L=1,LM
      call getnemsandplace_3d_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &               lm,icnt,idsp,tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,F_RAIN) 
      enddo

!-------------------------------------------------------------------

      varname='f_rimef'
      VcoordName='mid layer'
      do L=1,LM
      call getnemsandplace_3d_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &               lm,icnt,idsp,tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,F_RIMEF) 
      enddo

!-------------------------------------------------------------------

      varname='cldfra'
      VcoordName='mid layer'
      do L=1,LM
      call getnemsandplace_3d_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &               lm,icnt,idsp,tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,CLDFRA) 
      enddo
!        write(0,*) 'past cldfra'


!        call mpi_barrier(mpi_comm_world, ierr)
!        write(0,*) 'past post-cldfra barrier'

!        STOP

!-------------------------------------------------------------------

      varname='sr'
      VcoordName='sfc'
      L=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,SR) 

!        call mpi_barrier(mpi_comm_world, ierr)
!        write(0,*) 'past post-sr barrier'

!-------------------------------------------------------------------

      varname='cfrach'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,CFRACH) 

!        call mpi_barrier(mpi_comm_world, ierr)
!        write(0,*) 'past post-cfrach barrier'

!-------------------------------------------------------------------

      varname='cfracl'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,CFRACL) 
!        call mpi_barrier(mpi_comm_world, ierr)
!        write(0,*) 'past post-cfracl barrier'

!-------------------------------------------------------------------

      varname='cfracm'
      VcoordName='sfc'
      L=1
!        write(0,*) 'work CFRACM'
!        write(0,*) 'NUMSTA: ', NUMSTA
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName,
     &          l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,CFRACM) 

!        call mpi_barrier(mpi_comm_world, ierr)
!        write(0,*) 'past post-cfracm barrier'

!-------------------------------------------------------------------

      VarName='cmc'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,CMC) 

!-------------------------------------------------------------------

      varname='soiltb'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,SOILTB) 

!-------------------------------------------------------------------

      varname='vegfrc'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,VEGFRC) 

!-------------------------------------------------------------------

      VarName='sh2o'
      VcoordName='soil layer'
!        write(0,*) 'work to NSOIL'
	do L=1,NSOIL

!      call getnemsandplace_3d_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
!     &               lm,icnt,idsp,tmp,fldsize,recname,
!     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
!     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,F_RIMEF) 

      call getnemsandplace_3d_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &               nsoil,icnt,idsp,tmp,fldsize,recname,
     &      reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &      L,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,SH2O) 
      enddo


!-------------------------------------------------------------------

      VarName='smc'
      VcoordName='soil layer'
      do L=1,NSOIL
      call getnemsandplace_3d_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &               nsoil,icnt,idsp,tmp,fldsize,recname,
     &       reclevtyp,reclev,nrec,spval,VarName, 
     &       VcoordName,l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,SMC) 
      enddo

        if (MYPE .eq. 0) then
        write(0,*) 'SMC(1:10,1): ', SMC(1:10,1)
        endif
!-------------------------------------------------------------------

      VarName='stc'
      VcoordName='soil layer'
      do L=1,NSOIL
      call getnemsandplace_3d_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &               nsoil,icnt,idsp,tmp,fldsize,recname,
     &      reclevtyp,reclev,nrec,spval,VarName, 
     &      VcoordName,l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,STC) 
      enddo

!-------------------------------------------------------------------

!        write(0,*) 'past STC'

        if (MYPE .eq. 0) then
        write(0,*) 'STC(1:10,1): ', STC(1:10,1)
        endif

	LP1=LM+1


      VarName='pres'
      VcoordName='layer'
      do L=1,LP1
      call getnemsandplace_3d_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &               LM+1,icnt,idsp,tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,PINT) 
      enddo

!-------------------------------------------------------------------

        if (MYPE .eq. 0) then
        DO L=2,LP1
         DO N=1,NUMSTA
           PMID(N,L-1)=(PINT(N,L-1)+PINT(N,L))*0.5
           if (n.eq.10) then
             write(0,*) n,l,PMID(N,L-1),PINT(N,L),PINT(N,L-1)
           endif

        IF (N .eq. 1 .and. L .eq. LP1) then
	PDTOP=PINT(N,L)-PD(N)-PT
        if (MYPE .eq. 0) then
        write(0,*) 'PINT(N,LP1): ', PINT(N,LP1)
        write(0,*) 'PD(N): ', PD(N)
        write(0,*) 'PT: ', PT
        write(0,*) 'computed PDTOP of: ', PDTOP
        endif
	ENDIF

         end do
        end do

        endif

!-------------------------------------------------------------------

!!!!! CONSTRAIN Q TO A PARTICULAR RH VALUE, FOLLOWING CALRH OF WRFPOST
        if (MYPE .eq. 0) then

        do N=1,NUMSTA
         do L=1,LM
           QC= (PQ0/PMID(N,L)) *EXP(A2*(T(N,L)-A3)/(T(N,L)-A4))
           RH=Q(N,L)/QC
           IF (RH .gt. RHCRIT) THEN
           Q(N,L)=0.999*RHCRIT*QC
           ENDIF
         enddo
        enddo

        endif
!!!!! END RH CONSTRAIN

!-------------------------------------------------------------------


!!!!! DOES NEMSIO REALLY PROVIDE MIDLAYER W RATHER THAN INTERFACE W?

      VarName='vvel'
      VcoordName='mid layer'
      do l=1,lm
      call getnemsandplace_3d_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &               LM,icnt,idsp,tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,WH) 
      enddo

!-------------------------------------------------------------------

      VarName='ssroff'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,SSROFF) 

!-------------------------------------------------------------------

      VarName='bgroff'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,BGROFF) 

!-------------------------------------------------------------------

      VarName='alwin'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,ALWIN) 

!-------------------------------------------------------------------

      VarName='alwout'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &               icnt,idsp,tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,ALWOUT) 
        if (MYPE .eq. 0) then
	write(0,*) 'ALWOUT(10): ', ALWOUT(10)
        endif

!-------------------------------------------------------------------

      VarName='alwtoa'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,ALWTOA) 

!-------------------------------------------------------------------

      VarName='aswin'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,ASWIN) 
        if (MYPE .eq. 0) then
	write(0,*) 'ASWIN(10): ', ASWIN(10)
        endif

!-------------------------------------------------------------------

      VarName='aswout'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,ASWOUT) 

!-------------------------------------------------------------------

      VarName='aswtoa'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,ASWTOA) 

!-------------------------------------------------------------------

      VarName='sfcshx'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,SFCSHX) 

        if (MYPE .eq. 0) then
        write(0,*) 'read SFCSHX(674): ', SFCSHX(674)
        endif

!-------------------------------------------------------------------

      VarName='sfclhx'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,SFCLHX) 

!-------------------------------------------------------------------

      VarName='subshx'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &              reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &              l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,SUBSHX) 

!-------------------------------------------------------------------

      VarName='snopcx'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,SNOPCX) 

!-------------------------------------------------------------------

      VarName='potflx'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &              reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,POTFLX) 

!-------------------------------------------------------------------

      VarName='tlmin'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,TLMIN) 

!-------------------------------------------------------------------

      VarName='tlmax'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,TLMAX) 

!        write(0,*) 'tlmin(13), tlmax(13): ', tlmin(13), tlmax(13)

!-------------------------------------------------------------------

      varname='rlwtt'
      VcoordName='mid layer'
      do l=1,lm
      call getnemsandplace_3d_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &               LM,icnt,idsp,tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,RLWTT) 
      enddo


!-------------------------------------------------------------------

      varname='rswtt'
      VcoordName='mid layer'
      do l=1,lm
      call getnemsandplace_3d_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &               LM,icnt,idsp,tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,RSWTT) 
      enddo

!-------------------------------------------------------------------

      varname='tcucn'
      VcoordName='mid layer'
      do l=1,lm
      call getnemsandplace_3d_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &               LM,icnt,idsp,tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,TCUCN) 
      enddo

!-------------------------------------------------------------------

      varname='train'
      VcoordName='mid layer'
      do l=1,lm
      call getnemsandplace_3d_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &               LM,icnt,idsp,tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,TRAIN) 
      enddo

!-------------------------------------------------------------------

!      VarName='avrain'
!      VcoordName='sfc'
!      l=1
!      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
!     &                    icnt,idsp,
!     &               tmp,fldsize,recname,
!     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
!     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,AVRAIN) 

!-------------------------------------------------------------------

      VarName='nprec'
        call nemsio_getheadvar(nfile,trim(varname),nprec,iret)
        if (iret /= 0) then
          print*,VarName," not found in file-Assigned zero"
        end if


! equivalent to three hours in NTSD
!      write(0,*) 'NPREC= ', NPREC

!-------------------------------------------------------------------

      NPHS=0

      VarName='nphs'
        call nemsio_getheadvar(nfile,trim(varname),nphs,iret)
        if (iret /= 0) then
          print*,VarName," not found in file-Assigned zero"
        end if

!      write(0,*) 'NPHS= ', NPHS

!-------------------------------------------------------------------

      VarName='nclod'
        call nemsio_getheadvar(nfile,trim(varname),nclod,iret)
        if (iret /= 0) then
          print*,VarName," not found in file-Assigned zero"
        end if
!      write(0,*) 'NCLOD= ', NCLOD

!-------------------------------------------------------------------

      VarName='nheat'
        call nemsio_getheadvar(nfile,trim(varname),nheat,iret)
        if (iret /= 0) then
          print*,VarName," not found in file-Assigned zero"
        end if
!      write(0,*) 'NHEAT= ', NHEAT

!-------------------------------------------------------------------
                                                                                             
      VarName='NRDLW'
        call nemsio_getheadvar(nfile,trim(varname),nrdlw,iret)
        if (iret /= 0) then
          print*,VarName," not found in file-Assigned zero"
        end if
!      write(0,*) 'NRDLW= ', NRDLW


!-------------------------------------------------------------------

      VarName='NRDSW'
        call nemsio_getheadvar(nfile,trim(varname),nrdsw,iret)
        if (iret /= 0) then
          print*,VarName," not found in file-Assigned zero"
        end if
!      write(0,*) 'NRDSW= ', NRDSW

!-------------------------------------------------------------------

      VarName='NSRFC'
        call nemsio_getheadvar(nfile,trim(varname),nsrfc,iret)
        if (iret /= 0) then
          print*,VarName," not found in file-Assigned zero"
        end if
!      write(0,*) 'NSRFC= ', NSRFC

!-------------------------------------------------------------------

        if (MYPE .eq. 0) then

      write(0,*) 'NPHS= ', NPHS
      write(0,*) 'NCLOD= ', NCLOD
      write(0,*) 'NHEAT= ', NHEAT
      write(0,*) 'NRDLW= ', NRDLW
      write(0,*) 'NRDSW= ', NRDSW
      write(0,*) 'NSRFC= ', NSRFC

        endif

! setting all counters to 1 because Ratko has divided all fluxes by counters within NEMS model
      avrain=1.0
      avcnvc=1.0
      ardlw=1.0
      ardsw=1.0
      asrfc=1.0



!      APHTIM=0
!      APHTIM0=0
!      ACUTIM=0
!      ACUTIM0=0
      NCNVC=0

       
!          APHTIM=DUMMY2(1,1)

!-------------------------------------------------------------------

      VarName='u10'
      VcoordName='10 m above gnd'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,U10) 

!-------------------------------------------------------------------


      VarName='v10'
      VcoordName='10 m above gnd'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,V10) 

!-------------------------------------------------------------------
  
      VarName='smstav'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,SMSTAV) 

!-------------------------------------------------------------------

      VarName='smstot'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,SMSTOT) 

!-------------------------------------------------------------------

      VarName='vegfrc'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,VEGFRA)

!-------------------------------------------------------------------

      VarName='sfcexc'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,SFCEXC)

!-------------------------------------------------------------------

      VarName='aphtim'
      VcoordName='sfc'
      l=1

      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,APHTIM) 

      VarName='acutim'
      VcoordName='sfc'
      l=1

      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,ACUTIM) 


!-------------------------------------------------------------------

      VarName='acsnow'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,ACSNOW)

!-------------------------------------------------------------------
 
      VarName='acsnom'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,ACSNOM)

!-------------------------------------------------------------------

      VarName='sno'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,SNO)

!-------------------------------------------------------------------

      VarName='cprate'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &              reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &              l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,CPRATE)

!-------------------------------------------------------------------


!!!! DONE GETTING



!!!! issue with OMGA....have only grabbed wh (midlayer w)

      DO L=1,LM
      DO N=1,NUMSTA
	I=IHINDX(N)	
	J=JHINDX(N)

            IF(ABS(T(N,L)).GT.1.0E-3)
     &        OMGA(N,L) = -WH(N,L)*PMID(N,L)*G/
     &                 (RD*T(N,L)*(1.+D608*Q(N,L)))
	
      END DO
      END DO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! DONE INSERT

C------------------------------------------------------------------------
C***
C***  READ QUANTITIES NEEDED FROM THE NHB FILE
C***

	do J=1,JM
	 do I=1,IM
	   LMH(I,J)=LM
	 enddo
	enddo

!!!!!!!!!!!!
!!!!!!!!!!!!  Made modifications down to about here.
!!!!!!!!!!!!

!	write(0,*) 'TLM0D, TPH0D: ', TLM0D, TPH0D

      NTSPH=INT(3600./DT+0.50)

        if (MYPE .eq. 0) then
	write(0,*) 'rot angles defined, and ntsph= ', ntsph
        endif
C
C------------------------------------------------------------------------
C***
c       DO L = 1, LM
c       DO N=1,NUMSTA
!	  Q2(N,L)=0.
c       ENDDO
c       ENDDO

c       DO L=1,LM
c       DO N=1,NUMSTA
!	  TRAIN(N,L)=-9999.
!	  TCUCN(N,L)=-9999.
c       ENDDO
c     ENDDO
C
!      DO N=1,NUMSTA
!	CZEN(N)=-9999.
!      ENDDO
C
c     DO N=1,NUMSTA
c       I=IHINDX(N)	
c       J=JHINDX(N)
c       HBOT(N)=-9999.
c     ENDDO
C
c     DO N=1,NUMSTA
c      PSLP(N)=-9999.
c     ENDDO
c
c     DO N=1,NUMSTA
!	SOILTB(N)=-9999.
!	SMSTOT(N)=-9999.
!	SFCEXC(N)=-9999.
c     ENDDO
C
c     DO N=1,NUMSTA
!	CZMEAN(N)=-9999.
c       U00(N)=-9999.
!       SR(N)=-9999.
c     ENDDO
C
c     DO N=1,NUMSTA
!       SFCSHX(N)=-9999.
!       SFCLHX(N)=-9999.
!       SUBSHX(N)=-9999.
!       SNOPCX(N)=-9999.
c     ENDDO
C
c     DO N=1,NUMSTA
!        ASWIN(N)=-9999.
!        ASWOUT(N)=-9999.
!        ASWTOA(N)=-9999.
!        ALWOUT(N)=-9999.
!        ALWTOA(N)=-9999.
c     ENDDO

!      DO N=1,NUMSTA
!        TH10(N)=-9999.
!        Q10(N)=-9999.
!      ENDDO

C
C------------------------------------------------------------------------
!     DO N=1,NUMSTA
!       POTFLX(N)=-9999.
!       TLMIN(N)=-9999.
!       TLMAX(N)=-9999.
!     ENDDO
C------------------------------------------------------------------------
C***
C***  READ RADIATIVE TEMPERATURE TENDENCIES
C***
!      DO L=1,LM
!        DO N=1,NUMSTA
!         RSWTT(N,L)=-9999.
!         RLWTT(N,L)=-9999.
!        ENDDO
!      ENDDO
C
c     CLOSE(LRSTRT)
C------------------------------------------------------------------------
C***
C***  THE FORECAST HOUR
C***  
c     IFHR=NTSD/NTSPH
	IFHR=ITAG
C------------------------------------------------------------------------
!        goto 1001

      IF(ITAG.GT.0)THEN

        call nemsio_close(nfile,iret=iret)
!        call nemsio_finalize()

        if (mype .eq. 0) then
        write(0,*) 'iret from close of current file: ', iret
        endif

C***
C***  GENERATE THE NAME OF THE PRECEDING RESTRT FILE
C***


        deallocate(recname)
        deallocate(reclevtyp)
        deallocate(reclev)

!        call w3movdat(rinc,idate,jdate)
!        write(DateStrold,301) JDATE(1),JDATE(2),JDATE(3),JDATE(5)
 301    format(i4,'-',i2.2,'-',i2.2,'_',i2.2,':00:00')

!!! not getting to previous hour?
        ITAGPREV=ITAG-INCR
!
!        write(0,*) 'now have ITAG: ', itag

        write(ITAGLAB,302) ITAGPREV
        if (MYPE .eq. 0) then
        write(0,*) 'produced ITAGLAB: ', itaglab
        endif
 302	format(I3.3)

         len=lnblnk(filename)

!!! problem is with the contents of filenme down here.  How corrupted?

        if (MYPE .eq. 0) then
	write(0,*) 'filename later in PROF: ', trim(filename), '_END'
!	write(0,*) 'LEN= ', LEN
        write(0,*) 'carried over part: ', filename(1:len-15)
        endif

        filename_prev=filename(1:len-15)//ITAGLAB//
     &               hrp//IMINLAB//minp

        if (MYPE .eq. 0) then
!        write(0,*) 'new filename is ', trim(filename_prev)
        write(0,*) 'filename_prev: ', filename_prev(len-16:len)
        endif

        iret=27

!       call nemsio_init(iret=iret)
!        if (MYPE .eq. 0) then
!        write(0,*) 'iret from 2nd nemsio_init: ', iret
!        endif

       call nemsio_open(nfile,trim(filename_prev),'read',
     &                  MPI_COMM_WORLD,iret=iret)

        if (MYPE .eq. 0) then
        write(0,*) 'iret from open nfile: ', iret

        if (IRET .ne. 0) STOP

        endif

        call nemsio_getfilehead(nfile,iret=iret,nrec=nrec)

        if (MYPE .eq. 0) then
        write(0,*) 'iret from getfile head for nrec: ', iret
        write(0,*) 'nrec: ', nrec

        if (nrec .le. 0) then 
        call mpi_finalize(mpi_comm_world, ierr)
        endif

        endif

       allocate(recname(nrec),reclevtyp(nrec),reclev(nrec))

       call nemsio_getfilehead(nfile,iret=iret                           &
     &   ,idate=idate(1:7),nfhour=nfhour,recname=recname                 &
     &   ,reclevtyp=reclevtyp,reclev=reclev,nframe=nframe                &
     &   ,dimx=im,dimy=jm,dimz=lm)

        if (MYPE .eq. 0) then
        write(0,*) 'iret from getfilehead of nfile: ', iret
        write(0,*) 'nfhour: ', nfhour

        write(0,*) 'what is nrec: ', nrec
        write(0,*) 'im,jm,lm: ', im,jm,lm
        endif

! start reading nemsio files using parallel read
      fldsize=(jend-jsta+1)*im
!      tmpsize=fldsize*nrec
      tmp=0.
        if (MYPE .eq. 0) then
        write(0,*) 'call nemsio_denseread'
        write(0,*) 'im, jsta, jend: ', im, jsta, jend
        write(0,*) 'size(tmp): ', size(tmp)
        endif

       call nemsio_denseread(nfile,1,im,jsta,jend,tmp,iret=iret)

!        write(0,*) 'min,max of tmp: ', minval(tmp),maxval(tmp)

      if(iret/=0)then
        write(0,*) 'fail using mpi io read, stopping'
        write(0,*) 'iret: ', iret
        stop
      end if

C***
C***  READ THE PREVIOUS RESTRT FILE
C***
C

	write(0,*) 'FROM EARLIER FILE'
! -----------

      varname='acprec' ! accum total precip
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &              tmp,fldsize,recname,
     &              reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &              l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,ACPREC0) 

        if (MYPE .eq. 0) then
        write(0,*) 'maxval(acprec0): ', maxval(acprec0)
        write(0,*) 'found ACPREC0(330): ', ACPREC0(330)
        endif

!-------------------------------------------------------------------

      varname='cuprec' ! accum cumulus precip
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,CUPREC0) 

!-------------------------------------------------------------------

      VarName='ssroff'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,SSROFF0) 

!-------------------------------------------------------------------

      VarName='bgroff'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,BGROFF0) 

!-------------------------------------------------------------------

      VarName='alwin'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,ALWIN0) 

!-------------------------------------------------------------------

      VarName='alwout'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,ALWOUT0) 
	write(0,*) 'ALWOUT0(10): ', ALWOUT0(10)

!-------------------------------------------------------------------

      VarName='alwtoa'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,ALWTOA0) 

!-------------------------------------------------------------------

      VarName='aswin'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,ASWIN0) 

!-------------------------------------------------------------------

      VarName='aswout'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,ASWOUT0) 

!-------------------------------------------------------------------

      VarName='aswtoa'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,ASWTOA0) 

!-------------------------------------------------------------------

      VarName='sfcshx'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &              l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,SFCSHX0) 

        if (MYPE .eq. 0) then
        write(0,*) 'read SFCSHX0(674): ', SFCSHX0(674)
        endif

!-------------------------------------------------------------------

      VarName='sfclhx'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,SFCLHX0) 

!-------------------------------------------------------------------

      VarName='subshx'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,SUBSHX0) 

!-------------------------------------------------------------------

      VarName='snopcx'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,SNOPCX0) 

!-------------------------------------------------------------------

      VarName='potflx'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,POTFLX0) 

!-------------------------------------------------------------------

      varname='tcucn'
      VcoordName='mid layer'
      do l=1,lm
      call getnemsandplace_3d_para(nfile,im,
     &               jsta,jend,jsta_2l,jend_2u,
     &               LM,icnt,idsp,tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,TCUCN0) 


      enddo

!-------------------------------------------------------------------

      varname='train'
      VcoordName='mid layer'
      do l=1,lm
      call getnemsandplace_3d_para(nfile,
     &               im,jsta,jend,jsta_2l,jend_2u,
     &               LM,icnt,idsp,tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,TRAIN0) 
      enddo

!-------------------------------------------------------------------


      VarName='aphtim'
      VcoordName='sfc'
      l=1

      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,APHTIM0) 

!          APHTIM0=DUMMY2(1,1)
!      write(0,*) 'APHTIM0 now : ', APHTIM0


!-----------------------------------------------------------------
      VarName='acutim'
      VcoordName='sfc'
      l=1

      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,ACUTIM0) 


!------------------------------

      VarName='acsnow'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,ACSNOW0) 

!-------------------------------------------------------------------
 
      VarName='acsnom'
      VcoordName='sfc'
      l=1
      call getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,
     &                    icnt,idsp,
     &               tmp,fldsize,recname,
     &               reclevtyp,reclev,nrec,spval,VarName,VcoordName, 
     &               l,impf,jmpf,nframed2,NUMSTA,IHINDX,JHINDX,ACSNOM0) 

!-------------------------------------------------------------------

      ENDIF  ! if test on itag=0? 
C
Cmp 	IDEALLY, WON'T NEED MANY MODS BELOW THIS POINT
C
C------------------------------------------------------------------------
C***
C***  ALL THE DATA IS NOW IN.
C***  CALCULATE CLOUD FRACTION AND CLOUD WATER/ICE ID NUMBER.
C***
C------------------------------------------------------------------------
      UTIM=1.
      US=1.
      CCLIMIT=1.E-3
      CLIMIT =1.E-20
C-----------------------------------------------------------------------
!$OMP parallel do 
      DO N=1,NUMSTA
        PDSL1(N)=PD(N)*RES(N)
      ENDDO
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C***
C***  BEGIN THE PROFILE POSTING CODE.
C***
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C***
C***  USE ZERO IN ACCUMULATION ARRAYS AT APPROPRIATE TIMES
C***
       IF(ITAG .eq. 0) THEN
C
C set TLMAX,TLMIN= Lowest Layer Temp if forecast hour (ITAG)=0
C
         DO N=1,NUMSTA
           TLMIN(N)=T(N,LM)
           TLMAX(N)=T(N,LM)
         ENDDO
	write(0,*) 'here (2)...should only see once'
C
C what would appropriate if test be here?
C
        DO N=1,NUMSTA
C
C*** ZERO ACCUMLATION ARRAYS.
C
          STATPR(N)=0.
          STACPR(N)=0.
          STAEVP(N)=0.
          STAPOT(N)=0.
          STASHX(N)=0.
          STASUB(N)=0.
          STAPCX(N)=0.
          STASWI(N)=0.
          STASWO(N)=0.
          STALWI(N)=0.
          STALWO(N)=0.
          STALWT(N)=0.
          STASWT(N)=0.
          STASNM(N)=0.
          STASNO(N)=0.
          STASRF(N)=0.
          STABRF(N)=0.
          DO L=1,LM
            DHCNVC(L,N)=0.
            DHRAIN(L,N)=0.
          ENDDO
        ENDDO
C
        GO TO 300
       ENDIF
C---------------------------------------------------------------------
C***
C***  WE MUST CHECK TO SEE IF WE ARE 1 HOUR AFTER ANY OF THE 
C***  ACCUMULATION BUCKETS HAVE BEEN EMPTIED.  IF WE ARE AT SUCH A 
C***  TIME THEN WE NEED TO SET TO ZERO THE VARIABLES USED TO HOLD
C***  THE PRECEDING HOUR'S VALUES.
C***
C---------------------------------------------------------------------
C
C
C 	At this point, accumulation buckets are a foreign concept in
C	the WRF model.  
C
C

c     TIME=(NTSD-1)*DT



!!!!!!!!!!!!!! pull NPHS from file

        if (NPHS .eq. 0) then

	write(0,*) 'hardwired NPHS'

	if (DT .eq. 18) then
	NPHS=10
	elseif (DT .eq. 20) then
	NPHS=20
	elseif (DT .ge. 25 .and. DT .le. 28) then 
        NPHS=6
	elseif (DT .ge. 8.9 .and. DT .le. 11) then 
        NPHS=12
        elseif (DT .ge. 7.0 .and. DT .le. 7.5) then
! CONUSNMMB
        NPHS=5
        elseif (DT .le. 6.25) then
! OTHER NMMB DOMAINS
        NPHS=6
        else
        NPHS=6
	endif

        endif

        IFCST=3600*ITAG
	TIME=IFCST
	NTSD=(TIME/DT)+1

	write(0,*) 'TIME, DT, NTSD: ', TIME, DT, NTSD

      RESET0=TIME-(NTSD/NPREC)*NPREC*DT
      RESET1=(NPHS-1)*DT+3600.


!!!! limit resets to task zero???


       bigif:     if (MYPE .eq. 0) then
!        if (MYPE .eq. 0) then
C
!?      IF(MOD(NTSD,NPREC).GE.NPHS.AND.RESET0.LE.RESET1)THEN

	write(0,*) 'RESET0, RESET1 for PREC: ', RESET0, RESET1
	write(0,*) 'NTSD, NPREC, NPHS: ', NTSD, NPREC, NPHS
	write(0,*) 'RESET0 < RESET1 for precip? : ', RESET0, RESET1
	write(0,*) 'MOD < NPHS for precip? : ', mod(NTSD,NPREC),NPHS

      IF(MOD(NTSD,NPREC).GE.NPHS.AND.RESET0.LE.RESET1)THEN
	write(0,*) 'resetting STATPR, etc. to zero'
        DO N=1,NUMSTA
          STATPR(N)=0.
          STACPR(N)=0.
          STASNM(N)=0.
          STASNO(N)=0.
          STASRF(N)=0.
          STABRF(N)=0.
        ENDDO
      ELSE
	write(0,*) 'NOT resetting PRECIP accumulators'
        DO N=1,NUMSTA
          STATPR(N)=ACPREC0(N)*1.E3
          STACPR(N)=CUPREC0(N)*1.E3

        if (N .eq. 330) then
        write(0,*) 'set STATPR(330): ', STATPR(N)
        endif

!          STASNM(N)=ACSNOM0(N)*1.E3
!          STASNO(N)=ACSNOW0(N)*1.E3
!          STASRF(N)=SSROFF0(N)*1.E3
!          STABRF(N)=BGROFF0(N)*1.E3
          STASNM(N)=ACSNOM0(N)
          STASNO(N)=ACSNOW0(N)
          STASRF(N)=SSROFF0(N)
          STABRF(N)=BGROFF0(N)
        ENDDO
      ENDIF          
C
      RESET0=TIME-(NTSD/NRDSW)*NRDSW*DT
	
	write(0,*) 'TIME, DT: ', TIME, DT
	write(0,*) 'RESET0 < RESET1? : ', RESET0, RESET1
	write(0,*) 'MOD < NPHS?: ', MOD(NTSD,NRDSW),NPHS
	
      IF( MOD(NTSD,NRDSW) .GE. NPHS .AND. RESET0 .LE. RESET1)THEN
!      IF( MOD(NTSD,NRDSW) .LE. NPHS .AND. RESET0 .LE. RESET1)THEN
	write(0,*) 'resetting SW accumulators to zero'
       DO N=1,NUMSTA
         STASWI(N)=0.
         STASWO(N)=0.
         STASWT(N)=0.
       ENDDO
      ELSE
	write(0,*) 'NOT resetting SW accumulators'
        DO N=1,NUMSTA
          STASWI(N)=ASWIN0(N)
          STASWO(N)=ASWOUT0(N)
        if (N .eq. 1) write(0,*) 'STASWI, STASWO: ', 
     &  STASWI(N),STASWO(N)

          STASWT(N)=ASWTOA0(N)
        ENDDO
      ENDIF

!	write(0,*) 'STASWI(90): ', STASWI(90)
C
      RESET0=TIME-(NTSD/NRDLW)*NRDLW*DT
!	write(0,*) 'TIME, DT: ', TIME, DT
!	write(0,*) 'RESET0, RESET1 : ', RESET0, RESET1
!	write(0,*) 'NTSD, NRDLW, NPHS: ', NTSD, NRDLW, NPHS
      IF(MOD(NTSD,NRDLW).GE.NPHS.AND.RESET0.LE.RESET1)THEN
!      IF(MOD(NTSD,NRDLW).LE.NPHS.AND.RESET0.LE.RESET1)THEN
	write(0,*) 'resetting STALWI to zero'
       DO N=1,NUMSTA
         STALWI(N)=0.
         STALWO(N)=0.
         STALWT(N)=0.
       ENDDO
      ELSE
	write(0,*) 'setting STALWI to ALWIN0'
        DO N=1,NUMSTA
          STALWI(N)=ALWIN0(N)
          STALWO(N)=ALWOUT0(N)
          STALWT(N)=-ALWTOA0(N)
        ENDDO
      ENDIF
C
      RESET0=TIME-(NTSD/NSRFC)*NSRFC*DT
      IF(MOD(NTSD,NSRFC).GE.NPHS.AND.RESET0.LE.RESET1)THEN
!      IF(MOD(NTSD,NSRFC).LE.NPHS.AND.RESET0.LE.RESET1)THEN
       DO N=1,NUMSTA
         STAEVP(N)=0.
         STAPOT(N)=0.
         STASHX(N)=0.
         STASUB(N)=0.
         STAPCX(N)=0.
       ENDDO
      ELSE
        DO N=1,NUMSTA
          STAEVP(N)=SFCLHX0(N)
          STAPOT(N)=POTFLX0(N)
          STASHX(N)=SFCSHX0(N)
          STASUB(N)=SUBSHX0(N)
          STAPCX(N)=SNOPCX0(N)
        ENDDO
      ENDIF
C
      RESET0=TIME-(NTSD/NHEAT)*NHEAT*DT

        write(0,*) 'RESET0, RESET1: ',RESET0,RESET1
        write(0,*) 'NTSD, NHEAT: ', NTSD, NHEAT
        write(0,*) 'NCNVC: ', NCNVC
!mptest      IF(MOD(NTSD,NHEAT).GE.NCNVC.AND.RESET0.LE.RESET1)THEN
      IF(MOD(NTSD,NHEAT).GE.NPHS.AND.RESET0.LE.RESET1)THEN
        write(0,*) 'resetting DHCNVC, DHRAIN to zero'
       DO N=1,NUMSTA
         DO L=1,LM
           DHCNVC(L,N)=0.
           DHRAIN(L,N)=0.
         ENDDO
       ENDDO
      ELSE
        write(0,*) 'NOT RESET set DHCNVC, DHRAIN to old values'
       DO N=1,NUMSTA
         DO L=1,LM
            DHCNVC(L,N)=TCUCN0(N,L)
            DHRAIN(L,N)=TRAIN0(N,L)
         ENDDO
       ENDDO
      ENDIF


        endif bigif
 
C------------------------------------------------------------------
  300 CONTINUE
C------------------------------------------------------------------

       bigif2:     if (MYPE .eq. 0) then
 
C
C***  FOR ROTATION OF WINDS FROM E-GRID TO GEODETIC ORIENTATION
C***  WE NEED THE TWO QUANTITIES BELOW.
C
      SINPH0=SIN(TPH0D*DTR)
      COSPH0=COS(TPH0D*DTR)
C
C***  INITIAL CALCULATIONS/PREPARATIONS.  WE LOAD SEVERAL
C***  ARRAYS WITH PROFILE VARIABLES.
C
!$OMP parallel do
      DO N=1,NUMSTA
        IF(CZMEAN(N).GT.0.)THEN
          FACTR(N)=CZEN(N)/CZMEAN(N)
        ELSE
          FACTR(N)=0.
        ENDIF
      ENDDO
C
C***  ADJUST SHORTAVE TENDENCIES TO ACCOUNT FOR CHANGE OF SOLAR POSITION
C***  BETWEEN CALLS TO RADIATION
C
!$OMP parallel do
      DO L=1,LM
        DO N=1,NUMSTA
          RSWTT(N,L)=RSWTT(N,L)*FACTR(N)
        ENDDO
      ENDDO
C
C***  COMPUTE RTOP
C

!$OMP parallel do
      DO L=1,LM
        DO N=1,NUMSTA
!          APEL=PT+AETA(L)*PDSL1(N)
          APEL=PMID(N,L)
          RTOP(N,L)=RD*T(N,L)*(1.+0.608*Q(N,L))/APEL
        ENDDO
      ENDDO

!	write(0,*) 'PD(20) (dd): ', PD(20)

C
C***  PDS IS SURFACE PRESSURE.
C
!$OMP parallel do 
	DO N=1,NUMSTA
        PDS(N)=PD(N)+PDTOP+PT
	if (MYPE .eq. 0 .and. PDS(N) .lt. 50000.) then
	write(0,*) 'N,PD,PDTOP,PDS: ', N,PD(N),PDTOP,PDS(N)
	endif
	ENDDO
	write(0,*) 'trial a'
C
C***  EGRID2 IS THE SURFACE TEMPERATURE.
C
!$OMP parallel do 
      DO N=1,NUMSTA
	if (PSHLTR(N) .gt. 1.) then 

	TSHLTR(N)= TSHLTR_hold(N)*(PDS(N)*1.E-5)**CAPA
	if (mod(N,5) .eq. 0) then
	write(0,*) 'N, TSHLTR(N),PSHLTR(N): ', N, TSHLTR(N),PSHLTR(N)
	endif
        EGRID2(N)= THS(N)*(PDS(N)*1.E-5)**CAPA

	if (EGRID2(N) .lt. 200.) then
	write(0,*) 'THETA, PDS, CAPA, TSK: ', N,THS(N),
     &           PDS(N),CAPA,EGRID2(N)
	endif

        IF(ACPREC(N).LT.0.)ACPREC(N)=0.
        IF(CUPREC(N).LT.0.)CUPREC(N)=0.

!!! constrain surface RH

           QC=(PQ0/PSHLTR(N))*EXP(A2*(TSHLTR(N)-A3)/(TSHLTR(N)-A4))
           RH=QSHLTR(N)/QC
           IF (RH .gt. RHCRIT) THEN
c          write(0,*) 'reducing surface RH from: ', RH, ' at N: ', N
           QSHLTR(N)=0.999*RHCRIT*QC
	   ENDIF

	endif
      ENDDO
	write(0,*) 'trial b'
C
C** Compute PSLP using NMC reduction
C
      DO N=1,NUMSTA
       PSFC = PDS(N)
       ZSFC = FIS(N)*GI
       PSLP(N) = PSFC
C
C    COMPUTE LAYER TAU (VIRTUAL TEMP*RD/G).
       TVRT = T(N,LM)*(1.0+D608*Q(N,LM))
       TAU  = TVRT*RD*GI
C
C    COMPUTE TAU AT THE GROUND (Z=ZSFC) AND SEA LEVEL (Z=0)
C    ASSUMING A CONSTANT LAPSE RATE OF GAMMA=6.5DEG/KM.
       TVRSFC = TVRT + (ZSFC- ZSL)*GAMMA
       TAUSFC = TVRSFC*RD*GI
       TVRSL  = TVRT + (ZSFC- ZSL)*GAMMA
       TAUSL  = TVRSL*RD*GI
C
C    IF NEED BE APPLY SHEULL CORRECTION.
       IF ((TAUSL.GT.TAUCR).AND.(TAUSFC.LE.TAUCR)) THEN
          TAUSL=TAUCR
       ELSEIF ((TAUSL.GT.TAUCR).AND.(TAUSFC.GT.TAUCR)) THEN
          TAUSL = TAUCR-CONST*(TAUSFC-TAUCR)**2
       ENDIF
C
C    COMPUTE MEAN TAU.
       TAUAVG = 0.5*(TAUSL+TAUSFC)
C
C    COMPUTE SEA LEVEL PRESSURE.
       IF (FIS(N).GT.1.0) PSLP(N) = PSFC*EXP(ZSFC/TAUAVG)
c      print *,n,idstn(n),pslp(n),tvrt
       
      ENDDO
	write(0,*) 'trial c'
C
C***  SET CYCLE, DATE, AND FORECAST TIME.
C
c      IHR  =NTSD/NTSPH+0.5

!MAYBE      read(datestr,15)iyear,imn,iday,ihrst
         IDATE(2)=imn
         IDATE(3)=iday
         IDATE(1)=iyear
         IDATE(5)=ihrst

	IDAT(3)=IDATE(1)
	IDAT(1)=IDATE(2)
	IDAT(2)=IDATE(3)
	
      IYR  =IDAT(3)
      IMNTH=IDAT(1)
      IDAY =IDAT(2)
c      IFCST=(NTSD-1)*DT

!      IFCST=NTSPH*ITAG
      IFCST=3600*ITAG
	IHR=ITAG
	write(0,*) 'IFCST: ', IFCST
C
      WRITE(0,*)' POST PROFILE FOR ',
     1                       IYR,IMNTH,IDAY,IHR
	write(0,*) 'IHRST= ', IHRST
C
C***  SET RTSPH,RTSCU,RTSRA TO 1. OVER THE NUMBER OF TIMES THE
C***  VARIOUS PHYSICS ROUTINES HAVE BEEN
C***  CALLED SINCE LAST OUTPUT OF PROFILER DATA.  NECESSARY FOR
C***  CORRECT AVERAGING OF VARIABLES.
C
!      IF(APHTIM.GT.0.)THEN
!        RTSPH=1./(APHTIM-APHTIM0)
!      ELSE
!        RTSPH=1.
!      ENDIF

      IF(APHTIM(1).GT.0.)THEN
        RTSPH=1./(APHTIM(1)-APHTIM0(1))
      ELSE
        RTSPH=1.
      ENDIF

	write(0,*) 'APHTIM, RTSPH: ', APHTIM(1), APHTIM(1)-APHTIM0(1), RTSPH
C
C  Counter AVRAIN is reset every 6 or 3-h in WRF-NMM, so for stable
c  precip heating rate use counter for convection (ACUTIM)
c  since in the WRF-NMM microphysics (for stable precip physics)
c  is called at the same time as the convection.
C
	write(0,*) 'trial d'
      IF(ACUTIM(1).GT.0.)THEN
        RTSCU=1./(ACUTIM(1)-ACUTIM0(1))
        RTSRA=1./(ACUTIM(1)-ACUTIM0(1))
      ELSE
        RTSCU=1.
        RTSRA=1.
      ENDIF
	write(0,*) 'ACUTIM, RTSCU: ', ACUTIM(1), ACUTIM(1)-ACUTIM0(1), 
     &       RTSCU
C
c     IF(AVRAIN.GT.0.)THEN
c       RTSRA=1./(AVRAIN-AVRAIN0)
c     ELSE
c       RTSRA=1.
c     ENDIF
c      write(0,*) 'AVRAIN, RTSRA: ', AVRAIN, AVRAIN-AVRAIN0, RTSRA
C
C--------------------------------------------------------------------------
C--------------------------------------------------------------------------
C***
C***  OUTPUT PROFILE DATA.  THE FOLLOWING LOOP IS OVER ALL PROFILE SITES.
C***
C--------------------------------------------------------------------------
	LCLAS1=79
        
        write(0,*) 'LCLAS1 defined: ', LCLAS1

	write(0,*) 'trial e'
        write(0,*) 'LRECPR: ', LRECPR
        OPEN(UNIT=LCLAS1,ACCESS='DIRECT',RECL=LRECPR,IOSTAT=IER)
!        CALL FLUSH(LCLAS1)
!        CALL FLUSH(0)

!        IER=0

         write(0,*) 'opened LCLAS1 IER:', IER

!        write(0,*) 'am I still okay'

C--------------------------------------------------------------------------
	write(0,*) 'RECORD LENGTH = ', LRECPR
        write(0,*) 'NUMSTA: ', NUMSTA

! the station data is collected onto task 0, so limit this portion to that task


      DO 1000 N=1,NUMSTA
!        write(0,*) 'working N: ', N
C
C***  ZERO OUTPUT ARRAY.
C
      DO K=1,NWORDM
        PRODAT(K)=0.
        FPACK(K) =0.
      ENDDO
C
C***  CONSTRUCT HEADER FOR CURRENT PROFILE SITE.  THE HEADER CONTAINS
C***  THE FOLLOWING INFORMATION:  PACKED CYCLE-DATE, FORECAST TIME,
C***  INTEGER STATION ID, STATION LATITUDE, STATION LONGITUDE, STATION
C***  ELEVATION, NUMBER OF VERTICAL LEVELS IN PROFILE, NUMBER OF MULTI-
C***  LEVEL PARAMETERS, NUMBER OF SINGLE LEVEL PARAMETERS, TOTAL LENGTH
C***  (IN WORDS) OF MULTI- AND SINGLE LEVEL DATA, PROFILE CLASS FLAG,
C***  AND A DUMMY WORD FOR FUTURE USE.
C
!	if (N .eq. 20) write(0,*) 'PD(20) (e): ', PD(20)

      IH=IHINDX(N)
      JH=JHINDX(N)
      LMHK     = LMH(IH,JH)
      NWORD2   = 2*LMHK
      NWORD3   = 3*LMHK
      NWORD4   = 4*LMHK
      NWORD5   = 5*LMHK
      NWORD6   = 6*LMHK
      NWORD7   = 7*LMHK
      NWORD8   = 8*LMHK
      NWORD9   = 9*LMHK
      NWORD10  = 10*LMHK
      NWORD11  = 11*LMHK
      NWORD12  = 12*LMHK
      NWORD13  = 13*LMHK
      NWORD14  = 14*LMHK
      NWORD15  = 15*LMHK
      ISTAT    = IDSTN(N)
      CISTAT   = CIDSTN(N)
C
!        write(0,*) 'DTR here: ', DTR
      FPACK(1) = STNLAT(N)/DTR
      FPACK(2) = -STNLON(N)/DTR
!CHANGEDFORNMM      FPACK(2) = STNLON(N)/DTR
      IF(FPACK(2).LT.-180.)FPACK(2)=FPACK(2)+360.
      FPACK(3) = FIS(N)*GI
      FPACK(4) = FLOAT(LMHK)
      FPACK(5) = LCL1ML
      FPACK(6) = LCL1SL
      FPACK(7) = 9+FPACK(5)*FPACK(4)+FPACK(6)
      FPACK(8) = 999.
      FPACK(9) = 999.

C
C***  WIND ROTATION SINES AND COSINES
C

      DLM    = STNLON(N)+TLM0D*DTR
      XX     = COSPH0*COS(STNLAT(N))*COS(DLM)
     1        +SINPH0*SIN(STNLAT(N))
      YY     = -COS(STNLAT(N))*SIN(DLM)
      TLON   = ATAN(YY/XX)
      ALPHA  = ASIN(SINPH0*SIN(TLON)/COS(STNLAT(N)))
      SINALP = SIN(ALPHA)
      COSALP = COS(ALPHA)

!      SINALP = SROT(N)
!      COSALP = CROT(N)
C
C------------------------------------------------------------------
C***  EXTRACT PRESSURE AND TEMPERATURE PROFILES.
C***  EXTRACT/ROTATE U AND V WIND COMPONENT PROFILES.
C***  EXTRACT SPECIFIC HUMIDITY AND TEMPERATURE TENDENCY.
C***  EXTRACT CLOUD WATER, HEATING DUE TO CONVECTION, LARGE
C***  SCALE RAIN, SHORT WAVE RADIATION, LONG WAVE RADIATION,
C***  AND CLOUD FRACTION.
C------------------------------------------------------------------
C
      DO LV=1,LMHK
        LVL=LMHK-LV+1
!        PRODAT(LVL)      = PDSL1(N)*AETA(LV)+PT
        PRODAT(LVL)      = PMID(N,LV)
        PRODAT(LMHK+LVL) = T(N,LV)

C***  ROTATE WINDS
C
        UT     = U(N,LV)
        VT     = V(N,LV)

        PRODAT(NWORD2+LVL) = UT*COSALP+VT*SINALP
        PRODAT(NWORD3+LVL) = VT*COSALP-UT*SINALP
	if (N .eq. 163 .and. LVL .eq. 20) then
	write(0,*) 'N, COSALP, SINALP,UT,VT, new U, new V: ', 
     &    N,COSALP,SINALP,UT,VT,PRODAT(NWORD2+LVL),PRODAT(NWORD3+LVL)
	endif
C
        PRODAT(NWORD4+LVL) = Q(N,LV)
C
        IF(RTOP(N,LV).GT.1.E-12) THEN
           PRODAT(NWORD5+LVL) = OMGA(N,LV)
	ENDIF

!
!--- Separate posting of cloud water (NWORD6), rain (NWORD14), and
!    ice (cloud ice + snow, NWORD13).  The ice and rain fields are
!    *APPENDED* at the end of the section dealing with vertical
!    profile fields  (Ferrier/Manikin 11/30/04)
!
        QLIQ = (1.-F_ice(N,LV))*CWM(N,LV)
        PRODAT(NWORD13+LVL) = F_rain(N,LV)*QLIQ
        PRODAT(NWORD6+LVL) = QLIQ-PRODAT(NWORD7+LVL)
        PRODAT(NWORD14+LVL) = F_ice(N,LV)*CWM(N,LV)
        PRODAT(NWORD7+LVL) = TCUCN(N,LV)
        PRODAT(NWORD8+LVL) = TRAIN(N,LV)
        PRODAT(NWORD9+LVL) = RSWTT(N,LV)
        PRODAT(NWORD10+LVL)= RLWTT(N,LV)
        PRODAT(NWORD11+LVL)= CLDFRA(N,LV)*100.
C
        IF(LV.EQ.1)THEN
          PRODAT(NWORD12+LVL)=Q2(N,LV)
        ELSE
          PRODAT(NWORD12+LVL)=(Q2(N,LV)+Q2(N,LV-1))*0.5
        ENDIF
      ENDDO

!        write(0,*) 'here b'
!        write(0,*) 'is NHEAT zero? : ', NHEAT

C
C***  MODIFY ACCUMLATIONS SO AS TO REPRESENT ACCUMULATED
C***  CHANGE SINCE LAST PROFILE OUTPUT TIME.
C
CGSM  MODIFIED CODE TO ACCOUNT FOR DHCNVC AND DHRAIN BEING
C       COMPUTED FROM TOP DOWN WHILE PRODAT IS FILLED FROM
C       BOTTOM UP 
C
C Suspect this is no longer necessary in WRF-NMM
C
      DO LL=1,LMHK
        LVL=LMHK-LL+1
         STADHC(LL) = PRODAT(NWORD7+LL) - DHCNVC(LVL,N)
         STADHR(LL) = PRODAT(NWORD8+LL) - DHRAIN(LVL,N)

!        if (mod(N,10) .eq. 0) then
!        write(0,*) 'PRODAT, DHCNVC, STADHC: ', PRODAT(NWORD7+LL), 
!     &       DHCNVC(LVL,N), STADHC(LL)
!        endif
C
         DHCNVC(LVL,N) = PRODAT(NWORD7+LL)
         DHRAIN(LVL,N) = PRODAT(NWORD8+LL)
C
        if (NHEAT .ne. 0) then
       IF(MOD(NTSD,NHEAT).LT.NCNVC)THEN

!        if (mod(N,10) .eq. 0) then
!        write(0,*) 'NTSD, NHEAT, NCNVC: ', NTSD, NHEAT, NCNVC
!        write(0,*) 'setting DHCNVC to zero'
!        endif

          DHCNVC(LVL,N) = 0.
          DHRAIN(LVL,N) = 0.
       ENDIF
        endif
      ENDDO
!         write(0,*) 'here c'
C
C***  EXTRACT SINGLE LEVEL DATA.   EGRID2 IS SURFACE TEMPERATURE.
C
      PRODAT(NWORD15+1)  = PSLP  (N)
      PRODAT(NWORD15+2)  = PDS   (N)
      PRODAT(NWORD15+3)  = EGRID2(N)
      PRODAT(NWORD15+4)  = TLMIN (N)
      PRODAT(NWORD15+5)  = TLMAX (N)
      PRODAT(NWORD15+6)  = SMSTAV(N)*100.
      PRODAT(NWORD15+7)  = ACPREC(N)*1000.
      PRODAT(NWORD15+8)  = CUPREC(N)*1000.
      PRODAT(NWORD15+27) = Z0    (N)
C
      STAPRX=PRODAT(NWORD15+7)-STATPR(N)

	if (IDSTN(N) .eq. 724050) then
	write(0,*) 'STAPRX at DCA: ', STAPRX
	endif

	if (STAPRX .gt. 0 .and. mod(N,5) .eq. 0) then
	write(0,*) '1hr precip: ',  N,STAPRX
	endif

        if (mod(N,10) .eq. 0) then
        write(0,*) 'N, STATPR(N), PRODAT(NWORD15+7), STAPRX: ', 
     &              N, STATPR(N), PRODAT(NWORD15+7), STAPRX
        write(0,*) 'N, ACPREC(N), ACPREC0(N): ', 
     &    N, ACPREC(N), ACPREC0(N)

        endif

      STACRX=PRODAT(NWORD15+8)-STACPR(N)

C
C***  ROTATE WINDS
C
      UT     = U10(N)
      VT     = V10(N)
      PRODAT(NWORD15+28) = UT*COSALP+VT*SINALP
      PRODAT(NWORD15+29) = VT*COSALP-UT*SINALP
C
      PRODAT(NWORD15+30) = TH10  (N)
      PRODAT(NWORD15+31) = Q10   (N)
      PRODAT(NWORD15+32) = TSHLTR(N)
	if (N .eq. 50) then
	write(0,*) 'wrote TSHLTR, U10,V10: ', 
     &	PRODAT(NWORD15+32), PRODAT(NWORD15+28),PRODAT(NWORD15+29)
	endif
      PRODAT(NWORD15+33) = QSHLTR(N)
      PRODAT(NWORD15+34) = SFCEXC(N)
      PRODAT(NWORD15+35) = VEGFRC(N)*100.
      PRODAT(NWORD15+36) = CMC   (N)*1000.
      PRODAT(NWORD15+37) = SMC   (N,1)
      PRODAT(NWORD15+38) = SMC   (N,2)
      PRODAT(NWORD15+39) = SMC   (N,3)
      PRODAT(NWORD15+40) = SMC   (N,4)
      PRODAT(NWORD15+41) = STC   (N,1)
      PRODAT(NWORD15+42) = STC   (N,2)
      PRODAT(NWORD15+43) = STC   (N,3)
      PRODAT(NWORD15+44) = STC   (N,4)
      PRODAT(NWORD15+45) = SM    (N) + SICE(N)
      PRODAT(NWORD15+46) = CFRACL(N)*100.
      PRODAT(NWORD15+47) = CFRACM(N)*100.
      PRODAT(NWORD15+48) = CFRACH(N)*100.
      PRODAT(NWORD15+49) = SR    (N)*100.
      PRODAT(NWORD15+50) = NINT(HBOT(N))
      PRODAT(NWORD15+51) = CPRATE(N)
      PRODAT(NWORD15+52) = F_RIMEF(N,LMHK)
C
      PRODAT(NWORD15+9)   = SFCLHX(N)
      PRODAT(NWORD15+10)  = POTFLX(N)
      PRODAT(NWORD15+11)  = SFCSHX(N)
      PRODAT(NWORD15+12)  = SUBSHX(N)
      PRODAT(NWORD15+13)  = SNOPCX(N)
      PRODAT(NWORD15+14)  = ASWIN (N)
      PRODAT(NWORD15+15)  = ASWOUT(N)
      PRODAT(NWORD15+16)  = ALWIN (N)
      PRODAT(NWORD15+17)  = ALWOUT(N)
      PRODAT(NWORD15+18)  =-ALWTOA(N)
      PRODAT(NWORD15+19)  = ASWTOA(N)
!      PRODAT(NWORD15+20)  = ACSNOW(N)*1000.
      PRODAT(NWORD15+20)  = ACSNOW(N)
!      PRODAT(NWORD15+21)  = SMSTOT(N)*1000.
      PRODAT(NWORD15+21)  = SMSTOT(N)
!      PRODAT(NWORD15+22)  = SNO   (N)*1000.
      PRODAT(NWORD15+22)  = SNO   (N)
!      PRODAT(NWORD15+23)  = ACSNOM(N)*1000.
      PRODAT(NWORD15+23)  = ACSNOM(N)
!      PRODAT(NWORD15+24)  = SSROFF(N)*1000.
      PRODAT(NWORD15+24)  = SSROFF(N)
!      PRODAT(NWORD15+25)  = BGROFF(N)*1000.
      PRODAT(NWORD15+25)  = BGROFF(N)
      PRODAT(NWORD15+26)  = SOILTB(N)
!         write(0,*) 'here d'
C
C***  ACCUMULATED CHANGE SINCE LAST PROFILE OUTPUT TIME.
C
      PSFCEVP  = PRODAT(NWORD15+9 ) - STAEVP(N)
      PPOTEVP  = PRODAT(NWORD15+10) - STAPOT(N)
      PSFCSHX  = PRODAT(NWORD15+11) - STASHX(N)
      PSFCSUB  = PRODAT(NWORD15+12) - STASUB(N)
      PSNOPCX  = PRODAT(NWORD15+13) - STAPCX(N)
      PRSWIN   = PRODAT(NWORD15+14) - STASWI(N)

	if (IDSTN(N) .eq. 724050) then
        write(0,*) 'N, PRODAT(NWORD15+11), STASHX(N): ', 
     &     N, PRODAT(NWORD15+11), STASHX(N)
        write(0,*) 'difference for PSFCSHX: ', PSFCSHX
        endif


        if (N .eq. 1) then
        write(0,*) 'PRODAT(NWORD15+14), STASWI(N): ', 
     &              PRODAT(NWORD15+14), STASWI(N)
        write(0,*) 'N, PRSWIN(a): ', N, PRSWIN
        endif

!       write(0,*) 'N, SUBSHX(N),STASUB(N),PSFCSUB ', N,
!    &     SUBSHX(N), STASUB(N), PSFCSUB
      PRSWOUT  = PRODAT(NWORD15+15) - STASWO(N)
      PRLWIN   = PRODAT(NWORD15+16) - STALWI(N)
!	if (N .eq. 90) then
!	write(0,*) 'N, PRODAT(NWORD15+16) , STALWI(N), PRLWIN: ', N,
!     &     PRODAT(NWORD15+16), STALWI(N), PRLWIN
!	endif
      PRLWOUT  = PRODAT(NWORD15+17) - STALWO(N)

	if (N .eq. 1) then
	write(0,*) 'PRODAT(NWORD15+17), STALWO(N), PRLOWOUT: ', 
     &      PRODAT(NWORD15+17), STALWO(N), PRLOWOUT
	endif

      PRLWTOA  = PRODAT(NWORD15+18) - STALWT(N)
      PRSWTOA  = PRODAT(NWORD15+19) - STASWT(N)
      PACSNOW  = PRODAT(NWORD15+20) - STASNO(N)
      PACSNOM  = PRODAT(NWORD15+23) - STASNM(N)
      PSSROFF  = PRODAT(NWORD15+24) - STASRF(N)
      PBGROFF  = PRODAT(NWORD15+25) - STABRF(N)
!	write(0,*) 'N, PACSNOW: ', N, PACSNOW
C***
C***  TRANSFER STATION PROFILE DATA TO "PACKED" OUTPUT ARRAY.
C***
      NN   = 0
      NLEN = FPACK(7)
C	write(0,*) 'NWORD15+41,NWORD15+32 ', NWORD15+41,NWORD15+32
C	write(0,*) 'SOIL TEMP ', PRODAT(NWORD13+41)
C        write(0,*) 'SHELT TEMP ', PRODAT(NWORD13+32) 
C
      DO NL = 10,NLEN
        NN = NL-9
        FPACK(NL) = PRODAT(NN)
!       if(NN.eq.NWORD15+22) then
!         print *,'fpack ',nl,nn,nword15+22,fpack(nl),prodat(nn)
!       endif
      ENDDO
C
C***  REPLACE ACCUMULATED QUANTITIES WITH ACCUMULATION
C***  SINCE LAST PROFILE OUTPUT TIME.
C
      DO LL = 1,LMHK
        FPACK(9+NWORD7+LL) = STADHC(LL)*RTSCU
        FPACK(9+NWORD8+LL) = STADHR(LL)*RTSRA
!        FPACK(9+NWORD7+LL) = -9999.
!        FPACK(9+NWORD8+LL) = -9999.
      ENDDO
C
      FPACK(9+NWORD15+7)  = STAPRX
!	write(0,*) 'precip written to FPACK element: ', 9+NWORD13+7
      FPACK(9+NWORD15+8)  = STACRX
      FPACK(9+NWORD15+9)  = PSFCEVP * RTSPH
      FPACK(9+NWORD15+10) = PPOTEVP * RTSPH

!	if (IDSTN(N) .eq. 724050) then
!	write(0,*) 'PPOTEVP, RTSPH: ', PPOTEVP, RTSPH
!	write(0,*) 'packing PPOTEVP*RTSPH= ', FPACK(9+NWORD13+10)
!	endif

      FPACK(9+NWORD15+11) = PSFCSHX * RTSPH
      FPACK(9+NWORD15+12) = PSFCSUB * RTSPH

!       write(0,*) 'N, SUBSHX(N),STASUB(N),PSFCSUB,FPACK ', N,
!    &     SUBSHX(N), STASUB(N), PSFCSUB, FPACK(9+NWORD15+12)
!      FPACK(9+NWORD15+12) =-PSFCSUB * RTSPH

      FPACK(9+NWORD15+13) = PSNOPCX * RTSPH
      FPACK(9+NWORD15+14) = PRSWIN  * RTSPH

	if (N .eq. 1) then
        write(0,*) 'N, PRSWIN(b): ', N, PRSWIN
	write(0,*) 'N, RTSPH, SWRD: ', N, RTSPH, FPACK(9+NWORD15+14)
	endif

      FPACK(9+NWORD15+15) = PRSWOUT * RTSPH
      FPACK(9+NWORD15+16) = PRLWIN  * RTSPH
      FPACK(9+NWORD15+17) = PRLWOUT * RTSPH
      FPACK(9+NWORD15+18) = PRLWTOA * RTSPH
      FPACK(9+NWORD15+19) = PRSWTOA * RTSPH
      FPACK(9+NWORD15+20) = PACSNOW
      FPACK(9+NWORD15+23) = PACSNOM
      FPACK(9+NWORD15+24) = PSSROFF
      FPACK(9+NWORD15+25) = PBGROFF
C
!      IF(RESTRT)THEN
      IF(ITAG .eq. 0)THEN
        DO LL = 1,LMHK
          FPACK(9+NWORD7+LL) = 0.
          FPACK(9+NWORD8+LL) = 0.
        ENDDO
!        write(0,*) 'here e'
C
        FPACK(9+NWORD15+7)  = 0.
        FPACK(9+NWORD15+8)  = 0.
        FPACK(9+NWORD15+9)  = 0.
        FPACK(9+NWORD15+10) = 0.
        FPACK(9+NWORD15+11) = 0.
        FPACK(9+NWORD15+12) = 0.
        FPACK(9+NWORD15+13) = 0.
        FPACK(9+NWORD15+14) = 0.
        FPACK(9+NWORD15+15) = 0.
        FPACK(9+NWORD15+16) = 0.
        FPACK(9+NWORD15+17) = 0.
        FPACK(9+NWORD15+18) = 0.
        FPACK(9+NWORD15+19) = 0.
        FPACK(9+NWORD15+20) = 0.
        FPACK(9+NWORD15+23) = 0.
        FPACK(9+NWORD15+24) = 0.
        FPACK(9+NWORD15+25) = 0.
      ENDIF
!        write(0,*) 'here f'
C---------------------------------------------------------------------
C***
C***  WRITE PROFILE DATA
C***

      
      NREC=N

      WRITE(LCLAS1,REC=NREC)IHRST,IDAT,IFCST,ISTAT,CISTAT
     1,                    (FPACK(NL),NL=1,NLEN)

      if(nrec.le.5) then
        write(0,*)'IHRST,IDAT,IFCST,ISTAT,CISTAT,NLEN ',
     1     IHRST,IDAT,IFCST,ISTAT,CISTAT,NLEN
      endif

!        endif

C---------------------------------------------------------------------
 1000 CONTINUE
        write(0,*) 'to close of LCLAS1'
      CLOSE(LCLAS1)

        endif bigif2


 1001   continue

C
C***  END OF PROFILE SITE LOOP
C
C***  END PROFILE POSTING CODE.
C---------------------------------------------------------------------
        write(0,*) 'to deallocate block'
        DEALLOCATE( DETA,RDETA,AETA,UL
     &,RES,FIS,THS,HBOT
     &,CFRACL,CFRACM,CFRACH,SNO
     &,SOILTB,SFCEXC,SMSTAV,SMSTOT
     &,Z0,CZEN,CZMEAN,SR
     &,ACPREC,CUPREC,ACSNOW,ACSNOM
     &,SSROFF,BGROFF,SFCSHX,SFCLHX
     &,SUBSHX,SNOPCX,ASWIN,ASWOUT
     &,ASWTOA,ALWIN,ALWOUT,ALWTOA
     &,TSHLTR,TSHLTR_hold,QSHLTR,PSHLTR
     &,TH10,Q10,U10,V10
     &,TLMIN,TLMAX
     &,SMC,CMC,STC,SH2O
     &,VEGFRC,POTFLX,PSLP,PDSL1
     &,EGRID2,SM,SICE
     &,HBM2,FACTR
     &,PTBL,TTBL,VEGFRA
     &,T,Q,U,V,Q2
     &,OMGALF,CWM,TRAIN,TCUCN
     &,F_RAIN,F_ICE,CLDFRA
     &,RSWTT,RLWTT,RTOP
     &,OMGA
     &,PRODAT,FPACK
     &,STATPR,STACPR,STAEVP
     &,STAPOT,STASHX,STASUB,STAPCX
     &,STASWI,STASWO,STALWI,STALWO
     &,STALWT,STASWT,STASNM,STASRF
     &,STABRF,STASNO,DHCNVC,DHRAIN
     &,STADHC,STADHR,CPRATE
     &,ACPREC0,CUPREC0,SFCLHX0,POTFLX0
     &,SFCSHX0,SUBSHX0,SNOPCX0,ASWIN0
     &,ASWOUT0,ALWIN0,ALWOUT0,ALWTOA0
     &,ASWTOA0,ACSNOW0,ACSNOM0,SSROFF0
     &,BGROFF0)
        write(0,*) 'past deallocate block'

        deallocate(tmp) ! icnt,idsp)
   
!      call mpi_file_close(iunit,ierr)
!      call mpi_finalize(mpi_comm_world, ierr)

	write(0,*) 'end of routine PROF_NMMB'

      RETURN
      END
