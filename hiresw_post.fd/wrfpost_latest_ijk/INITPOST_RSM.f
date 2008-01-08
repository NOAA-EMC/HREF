      SUBROUTINE INITPOST_RSM
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    INITPOST    INITIALIZE POST FOR RUN
C   PRGRMMR: RUSS TREADON    ORG: W/NP2      DATE: 93-11-10
C     
C ABSTRACT:  THIS ROUTINE INITIALIZES CONSTANTS AND
C   VARIABLES AT THE START OF AN ETA MODEL OR POST 
C   PROCESSOR RUN.
C
C   THIS ROUTINE ASSUMES THAT INTEGERS AND REALS ARE THE SAME SIZE
C   .     
C     
C PROGRAM HISTORY LOG:
C   93-11-10  RUSS TREADON - ADDED DOCBLOC
C   98-05-29  BLACK - CONVERSION OF POST CODE FROM 1-D TO 2-D
C   99-01 20  TUCCILLO - MPI VERSION
C   01-10-25  H CHUANG - MODIFIED TO PROCESS HYBRID MODEL OUTPUT
C   02-06-19  MIKE BALDWIN - WRF VERSION
C   02-08-15  H CHUANG - UNIT CORRECTION AND GENERALIZE PROJECTION OPTIONS
C   02-10-31  H CHUANG - MODIFY TO READ WRF BINARY OUTPUT
C   04-01-20  BINBIN ZHOU - MODIFY TO READ RSM WRF FORMAT FILES   
C             NOTE: All RSM profile variables are defined on mid presure levels (LM)
C                   Only interface presure levels and geopotential heights
C                   have LM+1
C                   RSM vertical index is resverse to ETA, so need flip in getVariableRSM  
C                   in RSM: sfc=1, top=LM+1, in ETA: sfc=LM+1, top=1 
C 
C USAGE:    CALL INIT
C   INPUT ARGUMENT LIST:
C     NONE     
C
C   OUTPUT ARGUMENT LIST: 
C     NONE
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       NONE
C     LIBRARY:
C       COMMON   - CTLBLK
C                  LOOKUP
C                  SOILDEPTH
C
C    
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : CRAY C-90
C$$$  
      use vrbls3d
      use vrbls2d
      use soil
      use masks
C
C     INCLUDE/SET PARAMETERS.
C     
      include 'wrf_io_flags.h'
!      INCLUDE "parmeta"
      INCLUDE "params"
!      INCLUDE "parmsoil"
      INCLUDE "parm.tbl"
      INCLUDE "mpif.h"
C
C     INCLUDE COMMON BLOCKS.
C
      INCLUDE "LOOKUP.comm"
      INCLUDE "CTLBLK.comm"
!      INCLUDE "SOILDEPTH.comm"
      INCLUDE "GRIDSPEC.comm"
C
! This version of INITPOST shows how to initialize, open, read from, and
! close a NetCDF dataset. In order to change it to read an internal (binary)
! dataset, do a global replacement of _ncd_ with _int_. 

!     character(len=32) :: fileName
!     character(len=19) :: DateStr ! 2002-03-05_03:00:00
      character(len=31) :: VarName
      integer :: Status
      character startdate*19,SysDepInfo*80
C 
C     NOTE: SOME INTEGER VARIABLES ARE READ INTO DUMMY ( A REAL ). THIS IS OK
C     AS LONG AS REALS AND INTEGERS ARE THE SAME SIZE.
C
C     ALSO, EXTRACT IS CALLED WITH DUMMY ( A REAL ) EVEN WHEN THE NUMBERS ARE
C     INTEGERS - THIS IS OK AS LONG AS INTEGERS AND REALS ARE THE SAME SIZE.
      LOGICAL RUN,RUNB,RESTRT,SINGLRST
     1,       SIGMA,SUBPOST,NEST,HYDRO
      LOGICAL IOOMG,IOALL
      CHARACTER*32 LABEL
      CHARACTER*40 CONTRL,FILALL,FILMST,FILTMP,FILTKE,FILUNV
     &, FILCLD,FILRAD,FILSFC
      CHARACTER*4 RESTHR
      CHARACTER FNAME*80,ENVAR*50,BLANK*4
      INTEGER IDATB(3),IDATE(8),JDATE(8)
C
C     DECLARE VARIABLES.
C     
      REAL SLDPTH2(NSOIL)
      REAL RINC(5)
      REAL DUM1D (LM)
      REAL DUMMY ( IM, JM )
      REAL DUMMY2 ( IM, JM )
      INTEGER IDUMMY ( IM, JM )
      REAL DUM3D ( IM, JM, LM )
      REAL DUM3D2 ( IM, JM, LM )
!mp
	INTEGER DXVAL,DYVAL,CENLAT,CENLON,TRUELAT1,TRUELAT2
     	INTEGER LATSTART,LONSTART,LATLAST,LONLAST
C
      DATA BLANK/'    '/
       logical frst
       data frst /.true./
C
C***********************************************************************
C     START INIT HERE.
C
      WRITE(6,*)'INITPOST:  ENTER INITPOST'
C     
C     
C     STEP 1.  READ MODEL OUTPUT FILE
C
C
C***
!
! LMH always = LM for sigma-type vert coord
! LMV always = LM for sigma-type vert coord

       do j = jsta_2l, jend_2u
        do i = 1, im
            LMV ( i, j ) = lm
            LMH ( i, j ) = lm
        end do
       end do


! HTM VTM all 1 for sigma-type vert coord

      do l = 1, lm
       do j = jsta_2l, jend_2u
        do i = 1, im
            HTM ( i, j, l ) = 1.0
            VTM ( i, j, l ) = 1.0
        end do
       end do
      end do
!
!  how do I get the filename? 
!      fileName = '/ptmp/wx20mb/wrfout_01_030500'
!      DateStr = '2002-03-05_18:00:00'
!  how do I get the filename?
       if ( frst ) then
         frst = .false.
         call ext_int_ioinit(SysDepInfo,Status)
          print*,'called ioinit', Status
         call ext_int_open_for_read( trim(fileName), 0, 0, " ", 
     &  DataHandle, Status)
          print*,'called open for read', Status
       else
           Status = 0
       endif
       if ( Status /= 0 ) then
         print*,'error opening ',fileName, ' Status = ', Status ; stop
       endif
! get date/time info
!  this routine will get the next time from the file, not using it
      print *,'DateStr before calling ext_int_get_next_time=',DateStr
c      call ext_int_get_next_time(DataHandle, DateStr, Status)
      print *,'DateStri,Status,DataHandle = ',DateStr,Status,DataHandle

!  The end j row is going to be jend_2u for all variables.
      JS=JSTA_2L
      JE=JEND_2U
C
C Getting start time
      call ext_int_get_dom_ti_char(DataHandle
     1 ,'START_DATE',startdate, status )
        print*,'startdate= ',startdate
      jdate=0
      idate=0
      read(startdate,15)iyear,imn,iday,ihrst      
 15   format(i4,1x,i2,1x,i2,1x,i2)
      print*,'start yr mo day hr =',iyear,imn,iday,ihrst
      print*,'processing yr mo day hr ='
     +,idat(3),idat(1),idat(2),idat(4)
      idate(1)=iyear
      idate(2)=imn
      idate(3)=iday
      idate(5)=ihrst
      SDAT(1)=imn
      SDAT(2)=iday
      SDAT(3)=iyear
      jdate(1)=idat(3)
      jdate(2)=idat(1)
      jdate(3)=idat(2)
      jdate(5)=idat(4)
      CALL W3DIFDAT(JDATE,IDATE,2,RINC)
      ifhr=nint(rinc(2))
      print*,' in INITPOST ifhr fileName=',ifhr,fileName
!  OK, since all of the variables are dimensioned/allocated to be
!  the same size, this means we have to be careful int getVariable
!  to not try to get too much data.  For example, 
!  DUM3D is dimensioned IM,JM,LM but there might actually
!  only be im,jm,lm points of data available for a particular variable.  
! get metadata

! get metadata
! NMM does not output mp_physics yet so hard-wire it to Ferrier scheme
!        call ext_int_get_dom_ti_real(DataHandle,'MP_PHYSICS'
!     + ,itmp,1,ioutcount,istatus)
!        imp_physics=itmp
        imp_physics=5
        print*,'MP_PHYSICS= ',imp_physics
        

        call ext_int_get_dom_ti_real(DataHandle,'DX',tmp
     + ,1,ioutcount,istatus)
        dxval=nint(tmp)
        write(6,*) 'dxval= ', dxval
        call ext_int_get_dom_ti_real(DataHandle,'DY',tmp
     + ,1,ioutcount,istatus)
        dyval=nint(tmp)
        write(6,*) 'dyval= ', dyval
        call ext_int_get_dom_ti_real(DataHandle,'DT',tmp
     + ,1,ioutcount,istatus)
        DT=tmp
        write(6,*) 'DT = ', DT
        call ext_int_get_dom_ti_real(DataHandle,'CEN_LAT',tmp
     + ,1,ioutcount,istatus)
        cenlat=nint(tmp*1000.)
        write(6,*) 'cenlat= ', cenlat
        call ext_int_get_dom_ti_real(DataHandle,'CEN_LON',tmp
     + ,1,ioutcount,istatus)
        cenlon=nint(tmp*1000.)
        write(6,*) 'cenlon= ', cenlon
        call ext_int_get_dom_ti_real(DataHandle,'TRUELAT1',tmp
     + ,1,ioutcount,istatus)
        truelat1=nint(tmp*1000.)
        write(6,*) 'truelat1= ', truelat1
        call ext_int_get_dom_ti_real(DataHandle,'TRUELAT2',tmp
     + ,1,ioutcount,istatus)
        truelat2=nint(tmp*1000.)
        write(6,*) 'truelat2= ',truelat2,'(store orientation, deg)'
        call ext_int_get_dom_ti_integer(DataHandle,'MAP_PROJ',itmp
     + ,1,ioutcount,istatus)
        maptype=itmp
        write(6,*) 'maptype is ', maptype

! Binbin Zhou: for polar projection, cenlon is not-useful, should use
! orientation,  so: set
      cenlon = truelat2   
      write(6,*) 'orientation(in cenlon in polar proj)=', cenlon


! get 3-D variables
      print*,'im,jm,lm= ',im,jm,lm
c
      VarName='U'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUM3D,
     &  IM,1,JM,LM,IM,JS,JE,LM)
      do l = 1, lm
       do j = jsta_2l, jend_2u
        do i = 1, im
            u ( i, j, l ) = dum3d ( i, j, l )
        end do
       end do
!  fill up UH which is U at P-points 
       do j = jsta_2l, jend_2u
        do i = 1, im
            UH (I,J,L) = dum3d(I,J,L)
        end do
       end do
      end do
      ii=im/2
      jj=(jsta+jend)/2
      ll=lm
      print*,'UH at ',ii,jj,ll,' = ',UH (ii,jj,ll)
      VarName='V'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUM3D,
     &  IM,1,JM,LM,IM,JS,JE,LM)
      do l = 1, lm
       do j = jsta_2l, jend_2u
        do i = 1, im
            v ( i, j, l ) = dum3d ( i, j, l )
        end do
       end do
!  fill up VH which is V at P-points 
       do j = jsta_2l, jend_2u
        do i = 1, im
          VH(I,J,L) = dum3d(I,J,L)
        end do
       end do
      end do
      print*,'finish reading V'
      print*,'VH at ',ii,jj,ll,' = ',VH (ii,jj,ll)


!  fill up vertical velocity
      VarName='W'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUM3D,
     &  IM,1,JM,LM,IM,JS,JE,LM)
!      do l = 1, lm
!       do j = jsta_2l, jend_2u
!        do i = 1, im
!            w ( i, j, l ) = dum3d ( i, j, l )
!        end do
!       end do
!      end do
!  fill up WH which is W at P-points 
      DO L=1,LM
        DO I=1,IM
         DO J=JSTA_2L,JEND_2U
          OMGA(I,J,L) = DUM3D(I,J,L)
         ENDDO
        ENDDO
      ENDDO
      print*,'OMGA at ',ii,jj,ll,' = ',OMGA (ii,jj,ll)
      print*,'finish reading W'

c
c read geopotential on mid levels/compute geopotential height of mid levels
      VarName='PH'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUM3D,
     &  IM,1,JM,LM,IM,JS,JE,LM)

! ph is geopotential, geopotential height z=ph/9.8
      DO L=1,LM
        DO I=1,IM
         DO J=JS,JE
          ZMID(I,J,L)=DUM3D(I,J,L)/9.8
         ENDDO
        ENDDO
      ENDDO
        DO L=1,LM
         print*,'ZMID at 120,187=', ZMID(120,187,L)
        END DO


c read geopotential on interface levels/compute geopotential height of interface levels
      VarName='PHINT'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUM3D,
     &  IM,1,JM,LM,IM,JS,JE,LM)
     
      DO L=1,LM
       DO I=1,IM
        DO J=JS,JE
         ZINT(I,J,L)=DUM3D(I,J,L)/9.8   !geopotential height
        ENDDO
       ENDDO
      ENDDO
      !Surface (L=LM+1) geopotential height equal to surface height,read in later
      print*,'ZMID at ',ii,jj,ll,' = ',ZMID(ii,jj,ll)      
c      print*,'ZINT at ',ii,jj,ll+1,' = ',ZINT(ii,jj,ll+1)
C
C reading temperature
      VarName='T'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUM3D,
     &  IM,1,JM,LM,IM,JS,JE,LM)
      do l = 1, lm
       do j = jsta_2l, jend_2u
        do i = 1, im
            t ( i, j, l ) = dum3d ( i, j, l )
        end do
       end do
      end do
      do l=1,lm
       print*,'theta at ',ii,jj,l,' = ',T(ii,jj,l)
      end do
      print*,'finish reading T'


c      VarName='MU0'
c      call getVariableRSM(fileName,DateStr,DataHandle,VarName,PT,
c     &  1,1,1,1,1,1,1,1)
       PT=0.0

c
c reading TKE
c      VarName='TKE'
c      call getVariable(fileName,DateStr,DataHandle,'TKE_MYJ',DUM3D,
c     &  IM,1,JM,LM,IM,JS,JE,LM)
      do l = 1, lm
       do j = jsta_2l, jend_2u
        do i = 1, im
            q2 ( i, j, l ) = SPVAL
        end do
       end do
      end do


c
c reading specific humidity
      VarName='QVAPOR'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUM3D,
     &  IM,1,JM,LM,IM,JS,JE,LM)
      do l = 1, lm
       do j = jsta_2l, jend_2u
        do i = 1, im
            q ( i, j, l ) = dum3d ( i, j, l )
        end do
       end do
      end do
      print*,'finish reading specific humidity'
      print*,'Q at ',ii,jj,ll,' = ',Q(ii,jj,ll)

c reading specific cloud water content
      VarName='QCLOUD'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUM3D,
     &  IM,1,JM,LM,IM,JS,JE,LM)
      do l = 1, lm
       do j = jsta_2l, jend_2u
        do i = 1, im
            cwm ( i, j, l ) = dum3d ( i, j, l )
        end do
       end do
      end do
      print*,'finish reading specific cloud water content'
c

c reading pressure on mid levels (All variables are on mid levels) 
      VarName='PB'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUM3D,
     &  IM,1,JM,LM,IM,JS,JE,LM)
      do l = 1, lm
       do j = jsta_2l, jend_2u
        do i = 1, im
            PMID(I,J,L)=DUM3D(I,J,L)
            !now that I have P, convert theta to t, RSM already is actual T
            !cbzhou: t ( i, j, l ) = T(I,J,L)*(PMID(I,J,L)*1.E-5)**CAPA
            !now that I have T,q,P  compute omega from wh
c            omga(I,J,L) = -WH(I,J,L)*pmid(i,j,l)*9.8/
c     &                        (287.04*t(i,j,l)*(1.+0.608*q(i,j,l)))
!             omga(I,J,L) = WH(I,J,L)
	     WH(I,J,L) = OMGA(I,J,L) * (RD*T(I,J,L)*
     &	     (1.+D608*Q(I,J,L)))/(PMID(I,J,L)*G)
        end do
       end do
      end do

c reading pressure on interface levels 
      VarName='PBINT'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUM3D,
     &  IM,1,JM,LM,IM,JS,JE,LM)
      do l = 1, lm
       do j = jsta_2l, jend_2u
        do i = 1, im
            PINT(I,J,L)=DUM3D(I,J,L)
            ALPINT(I,J,L)=ALOG(PINT(I,J,L))
        end do
       end do
      end do
      !surface (LM+1) pressure PINT(I,J,LM+1) will be read in later

c reading soil temperature
      VarName='TSLB'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUM3D,
     &  IM,1,JM,LM,IM,JS,JE,NSOIL)
      do l = 1, nsoil
       do j = jsta_2l, jend_2u
        do i = 1, im
!            stc ( i, j, l ) = dum3d ( i, j, l )
! flip soil layer again because wrf soil variable vertical indexing
! is the same with eta and vertical indexing was flipped for both
! atmospheric and soil layers within getVariable
            stc ( i, j, l ) = dum3d ( i, j, nsoil-l+1)
        end do
       end do
      end do
      print*,'STC at ',ii,jj,N,' = ',stc(ii,jj,1),stc(ii,jj,2)
c
c reading soil moisture
      VarName='SMOIS'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUM3D,
     &  IM,1,JM,LM,IM,JS,JE,NSOIL)
      do l = 1, nsoil
       do j = jsta_2l, jend_2u
        do i = 1, im
            smc ( i, j, l ) = dum3d ( i, j, nsoil-l+1)
        end do
       end do
      end do
      print*,'SMC at ',ii,jj,N,' = ',smc(ii,jj,1),smc(ii,jj,2)

c reading sfc pressure
      VarName='MU'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
      DO I=1,IM
        DO J=JS,JE
           PINT(I,J,LM+1) = DUMMY(I,J)
        ENDDO
      ENDDO
      print*,'PINT at ',ii,jj,lm+1,'=',pint(ii,jj,lm+1)

cbzhou: RSM has PINT data, read it it insead of deriving it
c      DO L=2,LM
c         DO I=1,IM
c            DO J=JSTA_2L,JEND_2U
c              PINT(I,J,L)=EXP((ALOG(PMID(I,J,L-1))+
c     &                 ALOG(PMID(I,J,L)))*0.5)  ! ave of ln p
c              ALPINT(I,J,L)=ALOG(PINT(I,J,L))
c            ENDDO
c         ENDDO
c      END DO
c      print*,'PINT at ',ii,jj,ll,' = ',pint(ii,jj,ll)
c      print*,'T at ',ii,jj,ll,' = ',t(ii,jj,ll)

c
c reading 2 m mixing ratio 
      VarName='Q2'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            QSHLTR ( i, j ) = dummy ( i, j )
        end do
       end do
       print*,'QSHLTR at ',ii,jj,' = ',QSHLTR(ii,jj)
c
c reading 2m theta
      VarName='TH2'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
          TSHLTR ( i, j ) = dummy ( i, j )
          Tv=T(I,J,NINT(LMH(I,J)))*(1.+0.61*Q(I,J,NINT(LMH(I,J))))
c          P2m = PINT(I,J,LM+1)*exp(-0.068283/T(I,J,LMH(I,J)))   !convert to potential T
          P2m = PINT(I,J,LM+1)*exp(-0.068283/Tv)   !convert to potential T
          TSHLTR( i, j ) = TSHLTR( i, j )/(P2m*1.E-5)**CAPA     !according to SURFCE.f
        end do
       end do
       print*,'TSHLTR at ',ii,jj,' = ',TSHLTR(ii,jj)
c
c reading 10 m wind
      VarName='U10'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            U10 ( i, j ) = dummy ( i, j )
        end do
       end do
       print*,'U10 at ',ii,jj,' = ',U10(ii,jj)

      VarName='V10'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            V10 ( i, j ) = dummy ( i, j )
        end do
       end do
       print*,'V10 at ',ii,jj,' = ',V10(ii,jj)

! either assign SLDPTH to be the same as eta (which is original
! setup in WRF LSM) or extract thickness of soil layers from wrf
! output

! assign SLDPTH (soil depth): RSM Model has 2 soil levels: 

         SLDPTH(1)=0.10
         SLDPTH(2)=1.90

c
c reading SMSTAV
c      VarName='SMSTAV'
c      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
c     &  IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
         SMSTAV ( i, j ) = 0.0
        end do
       end do
c
c reading SURFACE RUNOFF 
      VarName='SFROFF'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            SSROFF ( i, j ) = dummy ( i, j )
        end do
       end do
c
c reading UNDERGROUND RUNOFF
c      VarName='UDROFF'
c      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
c     &  IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            BGROFF ( i, j ) = 0.0
        end do
       end do

c reading VEGETATION TYPE 
      VarName='IVGTYP'
      call getIVariable(fileName,DateStr,DataHandle,VarName,IDUMMY
     &  ,IM,1,JM,1,IM,JS,JE,1)
      print*,'IVGTYP at ',ii,jj,' = ',IDUMMY(ii,jj) 
      do j = jsta_2l, jend_2u
        do i = 1, im
            IVGTYP( i, j ) = idummy ( i, j )
        end do
       end do

      VarName='ISLTYP' 
      call getIVariable(fileName,DateStr,DataHandle,VarName,IDUMMY
     &  ,IM,1,JM,1,IM,JS,JE,1)
      do j = jsta_2l, jend_2u
        do i = 1, im
            ISLTYP( i, j ) = idummy ( i, j )
        end do
       end do

      VarName='VEGFRA'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            VEGFRC ( i, j ) = dummy ( i, j )
        end do
       end do
      print*,'VEGFRC at ',ii,jj,' = ',VEGFRC(ii,jj) 

      !accumulated snow (depth)
      VarName='ACSNOW'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            ACSNOW ( i, j ) = dummy ( i, j ) * 0.001
        end do
       end do
      
      !accumulated melted snow 
      VarName='ACSNOM'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            ACSNOM ( i, j ) = dummy ( i, j ) * 0.001
        end do
       end do

      !Snow water equilalent
      VarName='SNOW'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='CANWAT'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            CMC ( i, j ) = dummy ( i, j )
        end do
       end do

      VarName='SST'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            SST ( i, j ) = dummy ( i, j )
        end do
       end do
      print*,'SST at ',ii,jj,' = ',sst(ii,jj)      

c      VarName='WEASD'
c      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
c     &  IM,1,JM,1,IM,JS,JE,1)

c      VarName='TKE_MYJ'
c      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
c     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='THZ0'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            THZ0 ( i, j ) = dummy ( i, j )
        end do
       end do
      print*,'THZ0 at ',ii,jj,' = ',THZ0(ii,jj)

      VarName='QZ0'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            QZ0 ( i, j ) = dummy ( i, j )
        end do
       end do
      print*,'QZ0 at ',ii,jj,' = ',QZ0(ii,jj)

      VarName='UZ0'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            UZ0 ( i, j ) = dummy ( i, j )
        end do
       end do
      print*,'UZ0 at ',ii,jj,' = ',UZ0(ii,jj)

c      VarName='VZ0'
c      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
c     &  IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            VZ0 ( i, j ) = 0.0
        end do
       end do

      VarName='QSFC'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            QS ( i, j ) = dummy ( i, j )
        end do
       end do
      print*,'QS at ',ii,jj,' = ',QS(ii,jj)

      VarName='AKHS'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            AKHS ( i, j ) = dummy ( i, j )
        end do
       end do

      VarName='AKMS'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            AKMS ( i, j ) = dummy ( i, j )
        end do
       end do

    
      VarName='MAPFAC_M'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

c      VarName='F'
c      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
c     &  IM,1,JM,1,IM,JS,JE,1)

c      VarName='E'
c      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
c     &  IM,1,JM,1,IM,JS,JE,1)

c reading terrain height
      VarName='HGT'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            FIS ( i, j ) = dummy ( i, j ) * G
            if(i.eq.80.and.j.eq.42)print*,'Debug: sample fis,zint='
     1,dummy( i, j ),zint(i,j,lm)
            ZINT(i,j,LM+1)=dummy ( i, j )  !BZhou: surface geopotential height=terrain height 
        end do
       end do
       
       print*,'FIS at ',ii,jj,ll,' = ',FIS(ii,jj)
       print*,'ZINT at',ii,jj,LM+1,'=',ZINT(i,j,LM+1)

        write(6,*) 'past getting of HGT'

      VarName='TSK'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            THS ( i, j ) = dummy ( i, j ) ! wait to convert to theta later 
        end do
       end do
c       print*,'THS at ',ii,jj,' = ',THS(ii,jj)
c
c reading p_top, RSM set it to be 0.0 
c      VarName='P_TOP'
c      call getVariableRSM(fileName,DateStr,DataHandle,VarName,PT,
c     &  1,1,1,1,1,1,1,1)
      PT=0.0

      DO I=1,IM
            DO J=JS,JE
                 PINT (I,J,LM+1) = PINT (I,J,LM+1)+PT
                 THS ( i, j ) = THS ( i, j )
     1                       *(P1000/PINT(I,J,NINT(LMH(I,J))+1))**CAPA
                 PINT (I,J,1) = PT
                 ALPINT(I,J,LM+1)=ALOG(PINT(I,J,LM+1))
                 ALPINT(I,J,1)=ALOG(PINT(I,J,1))
            ENDDO
         ENDDO
      print*,'PSFC at ',ii,jj,' = ',PINT (ii,jj,lm+1)
      print*,'THS at ',ii,jj,' = ',THS(ii,jj)

c      VarName='FNM'
c      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUM1D,
c     &  1,1,1,LM,1,1,1,LM)

c      VarName='FNP'
c      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUM1D,
c     &  1,1,1,LM,1,1,1,LM)

c      VarName='RDNW'
c      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUM1D,
c     &  1,1,1,LM,1,1,1,LM)

c      VarName='RDN'
c      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUM1D,
c     &  1,1,1,LM,1,1,1,LM)
     
c      VarName='DNW'
c      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUM1D,
c     &  1,1,1,LM,1,1,1,LM)

c      VarName='DN'
c      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUM1D,
c     &  1,1,1,LM,1,1,1,LM)

c      VarName='ZNU'
c      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUM1D,
c     &  1,1,1,LM,1,1,1,LM)

c      VarName='ZNW'
c      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUM1D,
c     &  1,1,1,LM,1,1,1,LM)

CC
CC PREC is "GRID PRECIPITATION RATE" 
CC RAINC is "ACCUMULATED TOTAL PRECIPITATION" 
CC RAINNC is "ACCUMULATED TOTAL GRID SCALE PRECIPITATION"

        write(6,*) 'getting PREC'
      VarName='PREC'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            PREC ( i, j ) = dummy ( i, j )*0.001
        end do
       end do
       print*,'PREC at ',ii,jj,' = ',PREC(ii,jj)

	write(6,*) 'getting RAINC'
      VarName='RAINC'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            ACPREC ( i, j ) = dummy ( i, j )*0.001 
        end do
       end do
       print*,'CUPREC at ',ii,jj,' = ',CUPREC(ii,jj)

	write(6,*) 'getting RAINNC'
      VarName='RAINNC'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            ANCPRC ( i, j ) = dummy ( i, j )*0.001
        end do
       end do
       print*,'ANCPRC at ',ii,jj,' = ',ANCPRC(ii,jj)
	write(6,*) 'past getting RAINNC'

       !convective = total - grid-scale
       do j = jsta_2l, jend_2u
        do i = 1, im
          CUPREC ( i, j ) = ACPREC ( i, j ) - ANCPRC ( i, j ) 
        end do
       end do



c      VarName='RAINCV'
c      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
c     &  IM,1,JM,1,IM,JS,JE,1)
c       do j = jsta_2l, jend_2u
c        do i = 1, im
c            CUPPT ( i, j ) = 
c        end do
c       end do

c      VarName='RAINBL'
c      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
c     &  IM,1,JM,1,IM,JS,JE,1)

c  downward short wave flux at sfc 
      VarName='GSW'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            RSWIN ( i, j ) = dummy ( i, j )
        end do
       end do
       print*,'RSWIN at ',ii,jj,' = ',RSWIN(ii,jj)


c  downward long wave flux at sfc
      VarName='GLW'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            RLWIN ( i, j ) = dummy ( i, j )
        end do
       end do
       print*,'RLWIN at ',ii,jj,' = ',RLWIN(ii,jj)

! RSM, like NCAR WRF does not output zenith angle so make czen=czmean so that
! RSWIN can be output normally in SURFCE
       do j = jsta_2l, jend_2u
        do i = 1, im
             CZEN ( i, j ) = 1.0
             CZMEAN ( i, j ) = CZEN ( i, j )
        end do
       end do


      VarName='XLAT'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            GDLAT ( i, j ) = dummy ( i, j )
            f(i,j) = 1.454441e-4*sin(gdlat(i,j)*0.01745329)
        end do
       end do


      VarName='XLONG'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            GDLON ( i, j ) = dummy ( i, j )
        end do
       end do
       print*,'GDLON at ', ii,jj,'=',GDLON( ii, jj )
       print*,'read past GDLON'

        do j=jsta_2l, jend_2u
          write(*,*) j, GDLAT ( 1, j ), GDLON(1,j)
        end do

c      VarName='LU_INDEX'
c      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
c     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='TMN'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            TG ( i, j ) = dummy ( i, j )
            SOILTB ( i, j ) = dummy ( i, j )
        end do
       end do
c
! XLAND 1 land 2 sea
      VarName='XLAND'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            SM ( i, j ) = dummy ( i, j ) - 1.0
        end do
       end do
c 
      !instantaneous sensible heat flux
      VarName='HFX'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            TWBS ( i, j ) = dummy ( i, j )
        end do
       end do
       
      !instantaneous latent heat flux
      VarName='QFX'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            QWBS ( i, j ) = dummy ( i, j )
        end do
       end do
 
c  RSM has no averaged sensible and latent heat fluxes

      VarName='SNOWC'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            SNO ( i, j ) = dummy ( i, j )
        end do
       end do

       VarName='HCLDFR'
       call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            CFRACH( i, j ) = dummy ( i, j )*0.01
        end do
       end do  

       VarName='MCLDFR'
       call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            CFRACM( i, j ) = dummy ( i, j )*0.01
        end do
       end do 

       VarName='LCLDFR'
       call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            CFRACL( i, j ) = dummy ( i, j )*0.01
        end do
       end do 

c following cloud top and base height data is not used by wrfpost
c they can be derived from wrfpost
       VarName='HCLDTL'
       call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
       VarName='MCLDTL'
       call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
       VarName='LCLDTL'
       call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
       VarName='HCLDBL'
       call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
       VarName='MCLDBL'
       call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
       VarName='LCLDBL'
       call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

c upward short wave flux at sfc
      VarName='UGSW'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            RSWOUT ( i, j ) = dummy ( i, j )
        end do
       end do
       print*,'RSWOUT at ',ii,jj,' = ',RSWOUT(ii,jj)
                                                                                                                                      
                                                                                                                                      
c  upward long wave flux at sfc
      VarName='UGLW'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            RADOT ( i, j ) = dummy ( i, j )
        end do
       end do
      print*,'RADOT at ',ii,jj,' = ',RADOT(ii,jj)
        
c  ground heat flux
      VarName='GRDFLX'
      call getVariableRSM(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
       do j = jsta_2l, jend_2u
        do i = 1, im
            GRNFLX ( i, j ) = dummy ( i, j )
        end do
       end do
       print*,'GRNFLX at ', ii,jj,'=',GRNFLX ( ii, jj )

c  For hydrometeors fraction, currently, set all are rime:
c After Brad Ferrier give us code, they will be adapted here
      F_RimeF = 1.0
      F_rain = 0.0
      F_ice = 0.0
      CFR = SPVAL/H100
      EXCH_H = SPVAL
      RLWTT = SPVAL
      RSWTT = SPVAL
      TTND = SPVAL
      TCUCN = SPVAL
      TRAIN = SPVAL
      
      
! SET SOME FIELDS TO MISSING VALUES SINCE RSM DOES NOT OUTPUT THESES FIELDS      


       do j = jsta_2l, jend_2u
        do i = 1, im
         SI(I,J)=SPVAL
	 CLDEFI(I,J)=SPVAL
	 TH10(I,J)=SPVAL
	 Q10(I,J)=SPVAL
	 ALBASE(I,J)=SPVAL/H100
	 ALBEDO(I,J)=SPVAL
	 ISLOPE(I,J)=NINT(SPVAL)
	 PCTSNO(I,J)=SPVAL
	 SOILTB(I,J)=SPVAL
	 SSROFF(I,J)=SPVAL/1000.
	 BGROFF(I,J)=SPVAL/1000.
	 SFCEVP(I,J)=SPVAL/1000.
	 SFCEXC(I,J)=SPVAL
	 CNVCFR(I,J)=SPVAL
!	 SM(I,J)=0.
        enddo
       enddo

! pos east
cBZhou all commented for RSM (maptype = 2)
c       call collect_loc(gdlat,dummy)
c       if(me.eq.0)then
c        latstart=nint(dummy(1,1)*1000.)
c        latlast=nint(dummy(im,jm)*1000.)
c       end if
c       write(6,*) 'laststart,latlast B calling bcast= '
c     +, latstart,latlast
c       call mpi_bcast(latstart,1,MPI_INTEGER,0,mpi_comm_comp,irtn)
c       call mpi_bcast(latlast,1,MPI_INTEGER,0,mpi_comm_comp,irtn)
c       write(6,*) 'laststart,latlast A calling bcast= '
c     +, latstart,latlast
c       call collect_loc(gdlon,dummy)
c       if(me.eq.0)then
c        lonstart=nint(dummy(1,1)*1000.)
c        lonlast=nint(dummy(im,jm)*1000.)
c       end if
c       write(6,*)'lonstart,lonlast B calling bcast= '
c     +, lonstart,lonlast
c       call mpi_bcast(lonstart,1,MPI_INTEGER,0,mpi_comm_comp,irtn)
c       call mpi_bcast(lonlast,1,MPI_INTEGER,0,mpi_comm_comp,irtn)
c       write(6,*)'lonstart,lonlast A calling bcast= '
c     +, lonstart,lonlast
c
       if(me.eq.0)then
          latstart=GDLAT(1,1)*1000
          lonstart=GDLON(1,1)*1000
       end if

!!
!! 
!!
        write(6,*) 'filename in INITPOST=', filename,' is'


!MEB not sure how to get these 
       do j = jsta_2l, jend_2u
        do i = 1, im
            DX ( i, j ) = dxval  !MEB ???
            DY ( i, j ) = dyval  !MEB ???
        end do
       end do
!MEB not sure how to get these 

! close up shop
      call ext_int_ioclose ( DataHandle, Status )

! generate look up table for lifted parcel calculations

      THL=210.
      PLQ=70000.
      PT=200.0   !RSM model top pressure = 2. mb, ie 200Pa     

      CALL TABLE(PTBL,TTBL,PT,
     &          RDQ,RDTH,RDP,RDTHE,PL,THL,QS0,SQS,STHE,THE0)

      CALL TABLEQ(TTBLQ,RDPQ,RDTHEQ,PLQ,THL,STHEQ,THE0Q)

C     
C     
      IF(ME.EQ.0)THEN
!       WRITE(6,*)'  NMAP         :  ',NMAP
!       WRITE(6,*)'  TSHDE (POSTED FORECAST HOURS) BELOW:  '
!       WRITE(6,50) (TSHDE(K),K=1,99)
        WRITE(6,*)'  SPL (POSTED PRESSURE LEVELS) BELOW: '
        WRITE(6,51) (SPL(L),L=1,LSM)
   50   FORMAT(14(F4.1,1X))
   51   FORMAT(8(F8.1,1X))
      ENDIF
C     
C     COMPUTE DERIVED TIME STEPPING CONSTANTS.
C
!MEB need to get DT
!      DT = 120. !MEB need to get integraion DT   !read from files
      
      NPHS = 4  !MEB need to get physics DT
      DTQ2 = DT * NPHS  !MEB need to get physics DT
      TSPH = 3600./DT   !MEB need to get DT
!MEB need to get DT

!how am i going to get this information?
!      NPREC  = INT(TPREC *TSPH+D50)
!      NHEAT  = INT(THEAT *TSPH+D50)
!      NCLOD  = INT(TCLOD *TSPH+D50)
!      NRDSW  = INT(TRDSW *TSPH+D50)
!      NRDLW  = INT(TRDLW *TSPH+D50)
!      NSRFC  = INT(TSRFC *TSPH+D50)
!how am i going to get this information?
C     
!     IF(ME.EQ.0)THEN
!       WRITE(6,*)' '
!       WRITE(6,*)'DERIVED TIME STEPPING CONSTANTS'
!       WRITE(6,*)' NPREC,NHEAT,NSRFC :  ',NPREC,NHEAT,NSRFC
!       WRITE(6,*)' NCLOD,NRDSW,NRDLW :  ',NCLOD,NRDSW,NRDLW
!     ENDIF
C
C     COMPUTE DERIVED MAP OUTPUT CONSTANTS.
! SET ALL COUNTERS TO ZEROES SINCE RSM DOES NOT OUTPUT ACCUMULATED AUQNTITIES

      TSRFC=0.  !in case buket does not get emptied
      TRDLW=0.
      
      TRDSW=0.
      
      THEAT=0.
      
      TCLOD=0.
      
      TPREC=0.
      

      DO L = 1,LSM
         ALSL(L) = ALOG(SPL(L))
      END DO
C
CHC WRITE IGDS OUT FOR WEIGHTMAKER TO READ IN AS KGDSIN
        if(me.eq.0)then
        print*,'writing out igds'
        igdout=110
c        open(igdout,file='griddef.out',form='unformatted'
c     +  ,status='unknown')
        if(maptype .eq. 1)THEN  ! Lambert conformal
          WRITE(igdout)3
          WRITE(6,*)'igd(1)=',3
          WRITE(igdout)im
          WRITE(igdout)jm
          WRITE(igdout)LATSTART
          WRITE(igdout)LONSTART
          WRITE(igdout)8
          WRITE(igdout)CENLON
          WRITE(igdout)DXVAL
          WRITE(igdout)DYVAL
          WRITE(igdout)0
          WRITE(igdout)64
          WRITE(igdout)TRUELAT2
          WRITE(igdout)TRUELAT1
          WRITE(igdout)255
        ELSE IF(MAPTYPE .EQ. 2)THEN  !Polar stereographic
          WRITE(igdout)5
          WRITE(igdout)im
          WRITE(igdout)jm
          WRITE(igdout)LATSTART
          WRITE(igdout)LONSTART
          WRITE(igdout)8
          WRITE(igdout)CENLON    !store orientation value for polar
          WRITE(igdout)DXVAL
          WRITE(igdout)DYVAL
          WRITE(igdout)0
          WRITE(igdout)64
          WRITE(igdout)0 
          WRITE(igdout)0
          WRITE(igdout)255
c   check by Binbin
          write(*,*) 'For griddef.out file:'
          WRITE(*,*)5
          WRITE(*,*)im
          WRITE(*,*)jm
          WRITE(*,*)LATSTART
          WRITE(*,*)LONSTART
          WRITE(*,*)8
          WRITE(*,*)CENLON
          WRITE(*,*)DXVAL
          WRITE(*,*)DYVAL
          WRITE(*,*)0
          WRITE(*,*)64
          WRITE(*,*)0  
          WRITE(*,*)0
          WRITE(*,*)255

        ELSE IF(MAPTYPE .EQ. 3)THEN  !Mercator
          WRITE(igdout)1
          WRITE(igdout)im
          WRITE(igdout)jm
          WRITE(igdout)LATSTART
          WRITE(igdout)LONSTART
          WRITE(igdout)8
          WRITE(igdout)latlast
          WRITE(igdout)lonlast
          WRITE(igdout)TRUELAT1
          WRITE(igdout)0
          WRITE(igdout)64
          WRITE(igdout)DXVAL
          WRITE(igdout)DYVAL
          WRITE(igdout)255
        END IF
        end if


      RETURN
      END
