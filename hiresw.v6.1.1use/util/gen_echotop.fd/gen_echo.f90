        program gen_echo

        USE GRIB_MOD


        integer, parameter:: nx=1401
        integer, parameter:: ny=701
        integer, parameter:: nz=31

        real, parameter:: dbzlim=18.3

        character(len=19) :: fname00,fname06,fname12,fname18
        character(len=24) :: fname00_o,fname06_o,fname12_o,fname18_o

        character(len=19) :: fname03,fname09,fname15,fname21
        character(len=24) :: fname03_o,fname09_o,fname15_o,fname21_o


! C grib2
      INTEGER :: LUGB,LUGI,J,JDISC,JPDTN,JGDTN
      INTEGER,DIMENSION(:) :: JIDS(200),JPDT(200),JGDT(200)
      LOGICAL :: UNPACK
      INTEGER :: K,IRET
      TYPE(GRIBFIELD) :: GFLD
! C grib2

        integer :: levs(nz)

        data levs/18000,16000,15000,14000,13000,12000,11000,10000,9000, &
     &            8500,8000,7500,7000,6500,6000,5500,5000,4500,4000, &
     &            3500,3000,2750,2500,2250,2000,1750,1500,1250,1000, &
     &            750,500/

        real, allocatable::  refd(:,:,:),echotop(:,:)

        allocate(refd(nx,ny,nz))
        allocate(echotop(nx,ny))

        fname00='refd3d.t00z.grb2f00'
        fname06='refd3d.t06z.grb2f00'
        fname12='refd3d.t12z.grb2f00'
        fname18='refd3d.t18z.grb2f00'

        fname03='refd3d.t03z.grb2f00'
        fname09='refd3d.t09z.grb2f00'
        fname15='refd3d.t15z.grb2f00'
        fname21='refd3d.t21z.grb2f00'

        fname00_o='refd3d.t00z.grb2f00_etop'
        fname06_o='refd3d.t06z.grb2f00_etop'
        fname12_o='refd3d.t12z.grb2f00_etop'
        fname18_o='refd3d.t18z.grb2f00_etop'

        fname03_o='refd3d.t03z.grb2f00_etop'
        fname09_o='refd3d.t09z.grb2f00_etop'
        fname15_o='refd3d.t15z.grb2f00_etop'
        fname21_o='refd3d.t21z.grb2f00_etop'

        call baopenr(11,fname00,ierr1)
        call baopenw(19,fname00_o,ierr2)

        call baopenr(12,fname06,ierr1)
        call baopenw(20,fname06_o,ierr2)

        call baopenr(13,fname12,ierr1)
        call baopenw(21,fname12_o,ierr2)

        call baopenr(14,fname18,ierr1)
        call baopenw(22,fname18_o,ierr2)

        call baopenr(15,fname03,ierr1)
        call baopenw(23,fname03_o,ierr2)

        call baopenr(16,fname09,ierr1)
        call baopenw(24,fname09_o,ierr2)

        call baopenr(17,fname15,ierr1)
        call baopenw(25,fname15_o,ierr2)

        call baopenr(18,fname21,ierr1)
        call baopenw(26,fname21_o,ierr2)


        allocate(gfld%fld(nx*ny))
        allocate(gfld%idsect(200))
        allocate(gfld%igdtmpl(200))
        allocate(gfld%ipdtmpl(200))
        allocate(gfld%idrtmpl(200))
        allocate(gfld%bmap(nx*ny))

        
        do IFILE=1,8

        LUB=10+IFILE
        LUB_OUT=18+IFILE


        JIDS=-9999
        JPDTN=-1
        JPDT=-9999
        JGDTN=-1
        JGDT=-9999
        UNPACK=.true.

        do L=1,nz

        J=0
        LUI=0

       JPDT(10) = 102
       JPDT(12) = levs(L)
        JDISC=0
        JPDTN=0

        call getgb2(LUB,LUI,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,  &
     &    UNPACK,K,GFLD,IRET)

        DO KK = 1, nx*ny
          IF(MOD(KK,nx).EQ.0) THEN
            M=IMAX
            N=INT(KK/nx)
          ELSE
            M=MOD(KK,nx)
            N=INT(KK/nx) + 1
          ENDIF

          REFD(M,N,L) = gfld%fld(KK)

        ENDDO


        if (IRET .eq. 0) then
!        write(0,*) 'maxval(gfld%fld): ', maxval(gfld%fld)
!        write(0,*) 'unpacked K: ', K
        else
        write(0,*) 'IRET from getgb2: ', IRET
        endif


        enddo


        call find_echotop(refd,nx,ny,nz,levs,echotop,dbzlim)


        write(0,*) 'min,maxval(echotop): ', minval(echotop),maxval(echotop)

!        do J=701,1,-20
!        write(0,633) (echotop(I,J),i=1,1401,100)
!        enddo
  633   format(15(f7.1,1x))


       GFLD%ipdtnum=0
       GFLD%ipdtmpl(1)=16
       GFLD%ipdtmpl(2)=197
       GFLD%ipdtmpl(10)=200
       GFLD%ipdtmpl(12)=0

       GFLD%idrtnum=40 ! 40 = JPEG
       GFLD%idrtmpl(5)=0
       GFLD%idrtmpl(6)=0
       GFLD%idrtmpl(7)=-1

       gfld%idrtmpl(1)=0

       DEC=-4.0

        DO KK = 1, nx*ny
          IF(MOD(KK,nx).EQ.0) THEN
            M=IMAX
            N=INT(KK/nx)
          ELSE
            M=MOD(KK,nx)
            N=INT(KK/nx) + 1
          ENDIF

           gfld%fld(KK) = ECHOTOP(M,N) 

        ENDDO

       CALL set_scale(gfld, DEC)


        call putgb2(LUB_OUT,gfld,iret)


        enddo



        end program gen_echo


        subroutine find_echotop(refd,nx,ny,nz,levs,echotop,dbzlim)

        real refd(nx,ny,nz), echotop(nx,ny), dbzlim
        integer levs(nz)



        echotop=-1.

        do J=1,ny
        do I=1,nx
 L_loop:    do L=1,nz-1

        if (refd(I,J,L) .lt. dbzlim .and.  &
     &     refd(I,J,L+1) .ge. dbzlim) then
!        echotop(I,J)=levs(L)

        call vinterp_echotop(refd(i,j,l),refd(i,j,L+1), levs(L),levs(L+1), &
                             dbzlim,echotop(I,J))
        exit L_loop
        endif

        enddo L_loop
        enddo
        enddo
        

        end subroutine find_echotop


        subroutine vinterp_echotop (refd_u, refd_l, hgt_u, hgt_l, &
                                dbzlim, echotop)

        real :: refd_u, refd_l
        integer ::  hgt_u, hgt_l
        real :: slope, dz

        if (refd_u .lt. 0.) refd_u=0.

        slope_n=(refd_u-refd_l)
        slope_d=(hgt_u-hgt_l)
        slope=slope_n/slope_d

        dz=(dbzlim-refd_l)/slope
        
        echotop=hgt_l+dz

        end subroutine vinterp_echotop

        SUBROUTINE SET_SCALE(GFLD,DEC)
        USE GRIB_MOD
        USE pdstemplates
        TYPE (GRIBFIELD)  :: GFLD
        LOGICAL*1, allocatable:: locbmap(:)
        real :: DEC



        allocate(locbmap(size(GFLD%fld)))

        if (GFLD%ibmap .eq. 0 .or. GFLD%ibmap .eq. 254) then
        locbmap=GFLD%bmap
        write(0,*) 'used GFLD bmap'
        else
        write(0,*) 'hardwire locbmap to true'
        locbmap=.true.
        endif

! INPUT
!   ibm: integer, bitmap flag (grib2 table 6.0)
!   scl: real, significant digits,OR binary precision if < 0
!   len: integer, field and bitmap length
!   bmap: logical(len), bitmap (.true.: keep, bitmap (.true.: keep, .false.
!   skip)
!   fld: real(len), datafield
! OUTPUT
!   ibs: integer, binary scale factor
!   ids: integer, decimal scale factor
!   nbits: integer, number of bits to pack



        call g2getbits(GFLD%ibmap,DEC,size(GFLD%fld),locbmap,GFLD%fld, &
                      GFLD%idrtmpl(1),GFLD%idrtmpl(2),GFLD%idrtmpl(3),GFLD%idrtmpl(4))

        write(0,*) 'gfld%idrtmpl(2:3) defined, inumbits: ', gfld%idrtmpl(2:4)

        END SUBROUTINE SET_SCALE


       subroutine g2getbits(ibm,scl,len,bmap,g,gmin,ibs,ids,nbits)
!$$$
!   This subroutine is changed from w3 lib getbit to compute the total number of
!   bits,
!   The argument list is modified to have ibm,scl,len,bmap,g,ibs,ids,nbits
!
!  Progrma log:
!    Jun Wang  Apr, 2010
!
! INPUT
!   ibm: integer, bitmap flag (grib2 table 6.0)
!   scl: real, significant digits,OR binary precision if < 0
!   len: integer, field and bitmap length
!   bmap: logical(len), bitmap (.true.: keep, bitmap (.true.: keep, .false.
!   skip)
!   fld: real(len), datafield
! OUTPUT
!   ibs: integer, binary scale factor
!   ids: integer, decimal scale factor
!   nbits: integer, number of bits to pack
!
      IMPLICIT NONE
!
      INTEGER,INTENT(IN)   :: IBM,LEN
      LOGICAL*1,INTENT(IN) :: BMAP(LEN)
      REAL,INTENT(IN)      :: scl,G(LEN)
      INTEGER,INTENT(OUT)  :: IBS,IDS,NBITS
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      INTEGER,PARAMETER    :: MXBIT=16
!
!  NATURAL LOGARITHM OF 2 AND 0.5 PLUS NOMINAL SAFE EPSILON
      real,PARAMETER :: ALOG2=0.69314718056,HPEPS=0.500001
!
!local vars
      INTEGER :: I,I1,icnt,ipo,le,irange
      REAL    :: GROUND,GMIN,GMAX,s,rmin,rmax,range,rr,rng2,po,rln2
!
      DATA       rln2/0.69314718/


! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  ROUND FIELD AND DETERMINE EXTREMES WHERE BITMAP IS ON
      IF(IBM == 255) THEN
        GMAX = G(1)
        GMIN = G(1)
        DO I=2,LEN
          GMAX = MAX(GMAX,G(I))
          GMIN = MIN(GMIN,G(I))
        ENDDO
      ELSE
        do i1=1,len
          if (bmap(i1)) exit
        enddo
!       I1 = 1
!       DO WHILE(I1 <= LEN .AND. .not. BMAP(I1))
!         I1=I1+1
!       ENDDO
        IF(I1 <= LEN) THEN
          GMAX = G(I1)
          GMIN = G(I1)
          DO I=I1+1,LEN
            IF(BMAP(I)) THEN
              GMAX = MAX(GMAX,G(I))
              GMIN = MIN(GMIN,G(I))
            ENDIF
          ENDDO
        ELSE
          GMAX = 0.
          GMIN = 0.
        ENDIF
      ENDIF
!     write(0,*)' GMIN=',GMIN,' GMAX=',GMAX
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  COMPUTE NUMBER OF BITS
      icnt = 0
      ibs = 0
      ids = 0
      range = GMAX - GMIN
!      IF ( range .le. 0.00 ) THEN
      IF ( range .le. 1.e-30 ) THEN
        nbits = 8
        return
      END IF
!*
      IF ( scl .eq. 0.0 ) THEN
          nbits = 8
          RETURN
      ELSE IF ( scl  >  0.0 ) THEN
          ipo = INT (ALOG10 ( range ))
!jw: if range is smaller than computer precision, set nbits=8
          if(ipo<0.and.ipo+scl<-20) then
            print *,'for small range,ipo=',ipo,'ipo+scl=',ipo+scl,'scl=',scl
            nbits=8
            return
          endif

          IF ( range .lt. 1.00 ) ipo = ipo - 1
          po = float(ipo) - scl + 1.
          ids = - INT ( po )
          rr = range * 10. ** ( -po )
          nbits = INT ( ALOG ( rr ) / rln2 ) + 1
      ELSE
          ibs = -NINT ( -scl )
          rng2 = range * 2. ** (-ibs)
          nbits = INT ( ALOG ( rng2 ) / rln2 ) + 1
      END IF
!     write(0,*)'in g2getnits,ibs=',ibs,'ids=',ids,'nbits=',nbits,'range=',range
!*
      IF(nbits <= 0) THEN
        nbits = 0
        IF(ABS(GMIN) >= 1.) THEN
          ids = -int(alog10(abs(gmin)))
        ELSE IF (ABS(GMIN) < 1.0.AND.ABS(GMIN) > 0.0) THEN
          ids = -int(alog10(abs(gmin)))+1
        ELSE
          ids = 0
        ENDIF
      ENDIF
      nbits = min(nbits,MXBIT)
!     write(0,*)'in g2getnits ibs=',ibs,'ids=',ids,'nbits=',nbits
!
      IF ( scl > 0.0 ) THEN
        s=10.0 ** ids
        IF(IBM == 255) THEN
          GROUND = G(1)*s
          GMAX   = GROUND
          GMIN   = GROUND
          DO I=2,LEN
            GMAX = MAX(GMAX,G(I)*s)
            GMIN = MIN(GMIN,G(I)*s)
          ENDDO
        ELSE
          do i1=1,len
            if (bmap(i1)) exit
          enddo
 !        I1=1
 !        DO WHILE(I1.LE.LEN.AND..not.BMAP(I1))
 !          I1=I1+1
 !        ENDDO
          IF(I1 <= LEN) THEN
            GROUND = G(I1)*s
            GMAX   = GROUND
            GMIN   = GROUND
            DO I=I1+1,LEN
              IF(BMAP(I)) THEN
                GMAX = MAX(GMAX,G(I)*S)
                GMIN = MIN(GMIN,G(I)*S)
              ENDIF
            ENDDO
          ELSE
            GMAX = 0.
            GMIN = 0.
          ENDIF
        ENDIF

        range = GMAX-GMIN
        if(GMAX == GMIN) then
          ibs = 0
        else
          ibs = nint(alog(range/(2.**NBITS-0.5))/ALOG2+HPEPS)
        endif
!
      endif
        write(0,*) 'leave g2getbits with GMIN: ', GMIN
!        GFLD%idrtmpl(1)=GMIN
!     write(0,*)'in g2getnits,2ibs=',ibs,'ids=',ids,'nbits=',nbits,'range=',&
!                range, 'scl=',scl,'data=',maxval(g),minval(g)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END subroutine g2getbits
