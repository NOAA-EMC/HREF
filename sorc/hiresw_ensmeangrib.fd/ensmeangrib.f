      PROGRAM ENSMEANGRIB
C  Prgmmr: Hui-Ya Chuang
C  Program avggribV1 reads in two Grib files, average all fields, and then write to a new Grib file
c  using the larger presision of the two files.
C  This program uses a modified version of w3lib and operational bacio lib
C  4 subroutines in w3 lib were modified to pass bit packing information and
C  new libw3_4.a can be found in /nfsuser/g01/emcsrc/wx20hc/avggb/w3lib_w_nbits/
C  Program History log:
C  04-07-23 Hui-Ya Chuang
C
      PARAMETER (MAXPTS=1000000)
      PARAMETER(MBUF=256*1024)
      CHARACTER CBUF1(MBUF),CBUF2(MBUF)
      DIMENSION NID(25),TFLD(MAXPTS),MAX(7),KPDS(200),SFLD(MAXPTS),
     + avgfld(MAXPTS) 
      DIMENSION JGDS(200),KGDS(200),LGDS(200),LPDS(200)
      DIMENSION JENS(5),KENS(5),LENS(5)
      Logical*1 bmp(MAXPTS),bmp2(MAXPTS),bmp3(MAXPTS)
      character(len=80) :: gfile1,gfile2,gfile3
      INTEGER(4) NARG,IARGC,temparg
      real sumdiff,rmse
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  GET ARGUMENTS       
c      print*,
c     +  'Enter 3 file names for 2 input and 1 output (new) files'
c      NARG=IARGC()
c      IF(NARG.NE.3) THEN
c        CALL ERRMSG('avggrib:  Incorrect usage')
c        CALL ERRMSG('Usage: avggrib inputgribfile1 inputgribfile2 outputgribfile')
c        exit
c      ENDIF
      nid=-1
      JGDS=-1
      ICND=0
      IFL1=10
      IFL2=20
      IFL3=50
      IFLI1=0
      IFLI2=0
      isrch=-1
      k=-1
      temparg=1
      CALL GETARG(temparg,gfile1)
      NCGB=LEN_TRIM(gfile1)
      CALL BAOPENR(ifl1,gfile1(1:NCGB),IOS)
      temparg=2
      CALL GETARG(temparg,gfile2)
      NCGB=LEN_TRIM(gfile2)
      CALL BAOPENR(ifl2,gfile2(1:NCGB),IOS)
      if(ios.ne.0)print*,'cant open file2'
      
c      gfile3='./NEWWRF.tm00'
      temparg=3
      CALL GETARG(temparg,gfile3)
      NCGB=LEN_TRIM(gfile3)
      
      CALL BAOPEN(ifl3,gfile3,IOS)
      if(ios.ne.0)print*,'cant open new Grib file' 
      print*,'2 input and 1 output file names= ',gfile1
     +  ,gfile2,gfile3     
          
      do 
        SFLD=0.0
c        CALL GETGBEM(IFL1,IFLI1,MAXPTS,isrch,NID,JGDS,JENS,
c     *              MBUF,CBUF1,NLEN1,NNUM1,MNUM1,NUMPTS,isrch,
c     *     KPDS,KGDS,KENS,BMP,SFLD,ICND)
        CALL GETGBEMN(IFL1,IFLI1,MAXPTS,isrch,NID,JGDS,JENS,
     *              MBUF,CBUF1,NLEN1,NNUM1,MNUM1,NUMPTS,isrch,
     *     KPDS,KGDS,KENS,BMP,SFLD,NBITS1,ICND)   
         k2file1=kpds(2)
	 kfile1decscale=kpds(22) 
         kfile1bms=kpds(4) 
        if (icnd.ne.0) then
          write(6,*) ' ERROR READING/UNPACKING file 1= ',icnd
          exit
        else
          write(6,*)' kpds = ',(kpds(ijk),ijk=1,25)
          jsrch=k
          kpds(2)=-1
c	  kpds(4)=-1
          kpds(22)=-1
          if(kpds(5).ge.61 .and. kpds(5).le.66)kpds(4)=-1
	  TFLD=0.0
c          CALL GETGBEM(IFL2,IFLI2,MAXPTS,jsrch,kpds,kgds,kens,
c     *              MBUF,CBUF2,NLEN2,NNUM2,MNUM2,NUMPTS,K,
c     *     LPDS,LGDS,LENS,BMP,TFLD,ICND)
          CALL GETGBEMN(IFL2,IFLI2,MAXPTS,jsrch,kpds,kgds,kens,
     *              MBUF,CBUF2,NLEN2,NNUM2,MNUM2,NUMPTS,K,
     *     LPDS,LGDS,LENS,BMP2,TFLD,NBITS2,ICND)   
         kpds(2)=k2file1  
         kpds(22)=kfile1decscale
         kpds(4)=kfile1bms

          if (icnd.ne.0) then
            write(6,*) ' ERROR READING/UNPACKING file 2= ',icnd
            continue
c            exit
          go to 101
          endif
          write(6,*)' lpds = ',(lpds(ijk),ijk=1,25)
	  k=0
C check to see if GDS are the same between two Grib files
          if((kgds(1)-lgds(1)).gt.1)then
	   print*,'kgds = ',(kgds(ijk),ijk=1,25)
	   print*,'lgds = ',(lgds(ijk),ijk=1,25)
	   print*,'trying to add different grids, stopping'
	   stop 99
	  end if  
c            
          BMP3=.FALSE.
	  avgfld=0.0
          do n=1,numpts
	    if(BMP(n) .and. BMP2(n))then
             avgfld(n)=(sfld(n)+tfld(n))/2.0
	     BMP3(n)=.TRUE.
	    end if 
c	    avgfld(n)= sfld(n)  !test
          end do
c          if(kpds(5).eq.11)kpds(22)=-3
c	  CALL PUTGB(IFL3,NUMPTS,KPDS,KGDS,BMP,AVGFLD,IRET)
          NBITS=max0(NBITS1,NBITS2)
c	  NBITS=NBITS1  !test
c          IBS=0
c          if(kpds(5).eq.11)IBS=3  !Binary scaling
c	  if(kpds(5).eq.11)NBITS=9
	  if(kpds(5).eq.11)print*,'NBITS Decscale kpds2= '
     +	  ,NBITS,kpds(22),kpds(2)
c          if(kpds(5).eq.11 .and. kpds(6).eq.105)then
c	   do n=100,100+100
c	    print*,'BMP BMP2 BMP3= ',BMP(n),BMP2(n),BMP3(n)
c	   end do
c	  end if  
c	  if(kpds(5).eq.11)print*,'sample T', avgfld(numpts/2)
	  CALL PUTGBEN(IFL3,NUMPTS,KPDS,KGDS,KENS,IBS,NBITS
     +	  ,BMP3,AVGFLD,IRET)
 101      continue        
        endif
      enddo
      stop
      end
