      subroutine getnemsandplace(nfile,im,jm,tmp,fldsize,recnameloc,reclevtyp, &
                            reclev,nrec,spval,VarName,VcoordName               &
                           ,l,impf,jmpf,nframed2,NSTATL,IHINDX,JHINDX,BUFVAR)
!      
      use nemsio_module
      implicit none
      type(nemsio_gfile),intent(inout) :: nfile
      INCLUDE "mpif.h"
!
      integer,intent(in):: fldsize, nrec
      character(len=20),intent(in) :: VarName,VcoordName
      character(len=8) :: recnameloc(nrec)
      character(len=16) :: reclevtyp(nrec)
      integer :: reclev(nrec)
      real,intent(in) :: spval
      real,intent(in) :: tmp(fldsize*nrec)
      integer,intent(in) :: im,jm,l,impf,jmpf,NSTATL,nframed2
      integer :: iret,i,j,N,jsta,jend,jsta_2l,jend_2u
      integer :: IHINDX(NSTATL),JHINDX(NSTATL)
      real ::  dummy(im,jm), BUFVAR(NSTATL)
      real, allocatable:: dum1d(:)

      
!        call nemsio_readrecv(nfile,trim(VarName)                      &  
!        ,trim(VcoordName),l,dum1d,nframe=nframe,iret=iret)	 

        jsta=1
        jend=jm

        jsta_2l=1
        jend_2u=jm

      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u        &
     & ,l,nrec,fldsize,spval,tmp                               &
     & ,recnameloc,reclevtyp,reclev,VarName,VcoordName            &
     & ,dummy)

	  do j=1,jm
	    do i=1,im
	if (I .eq. 99 .and. J .eq. 72) then
	write(0,*) 'dummy(99,72): ', dummy(I,J)
	endif
	    end do
	  end do

	  do N=1,NSTATL
            I=IHINDX(N)
            J=JHINDX(N)
            BUFVAR(N)=DUMMY(I,J)
          enddo

                                                                                          
      
       RETURN
       END    

      subroutine getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,  &
                            tmp,fldsize,recnameloc,reclevtyp, &
                            reclev,nrec,spval,VarName,VcoordName               &
                           ,l,impf,jmpf,nframed2,NSTATL,IHINDX,JHINDX,BUFVAR)
!      
      use nemsio_module
      implicit none
      type(nemsio_gfile),intent(inout) :: nfile
      INCLUDE "mpif.h"
!
      integer,intent(in):: fldsize, nrec
      character(len=20),intent(in) :: VarName,VcoordName
      character(len=8) :: recnameloc(nrec)
      character(len=16) :: reclevtyp(nrec)
      integer :: reclev(nrec)
      real,intent(in) :: spval
      real,intent(in) :: tmp(fldsize*nrec)
      integer,intent(in) :: im,jsta,jend,jsta_2l,jend_2u
      integer,intent(in) :: l,impf,jmpf,nframed2
      integer :: NSTATL
      integer :: iret,i,j,N
      integer :: IHINDX(NSTATL),JHINDX(NSTATL)
      real ::  dummy(im,jsta_2l:jend_2u), BUFVAR(NSTATL)
      real, allocatable:: dum1d(:)

      
!        call nemsio_readrecv(nfile,trim(VarName)                      &  
!        ,trim(VcoordName),l,dum1d,nframe=nframe,iret=iret)	 

        write(0,*) 'mention NSTATL at top(1)'
        writE(0,*) 'NSTATL at top of getnemsandplace_para(2): ', NSTATL
!
!        write(0,*) 'size(BUFVAR)(3): ', size(bufvar)
!        write(0,*) 'shape(dummy)(4): ', shape(dummy)
!
!        write(0,*) 'call assignnemsiovar from para with jsta, jend(5): ', jsta,jend
!        write(0,*) 'jsta_2l, jend_2u(6): ', jsta_2l, jend_2u

        BUFVAR=0.

! does assignnemsiovar call corrupt NSTATL?

!!! apparently yes.  Skipping it allowed getting past barrier
        
!        goto 982



!        write(0,*) 'im,jsta,jend,jsta_2l,jend_2u: ', &
!                    im,jsta,jend,jsta_2l,jend_2u
!        write(0,*) 'l,nrec,fldsize,spval: ', l,nrec,fldsize,spval
!        write(0,*) 'size(tmp): ', size(tmp)
!        write(0,*) 'sizes(recnameloc,reclevtyp,reclev,VarName,VcoordName: ', &
!                   size(recnameloc),size(reclevtyp),size(reclev)
         write(0,*) 'VarName,VcoordName: ', VarName,VcoordName
!        write(0,*) 'size(dummy): ', size(dummy)
!        write(0,*) 'NSTATL pre assignnemsiovar(6b): ', NSTATL

      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u        &
     & ,l,nrec,fldsize,spval,tmp                               &
     & ,recnameloc,reclevtyp,reclev,VarName,VcoordName            &
     & ,dummy)

  982   continue

!        write(0,*) 'past assignnemsiovar in para for jsta,jend(7): ', jsta,jend

!        write(0,*) 'random write(8)'
!        write(0,*) 'about to mention NSTATL(9)'
!        write(0,*) 'loop to NSTATL(10): ' , NSTATL

        if (NSTATL .ne. 27) then
        write(0,*) 'NSTATL was inflated(11)'
!        NSTATL=27
        endif

	  do N=1,NSTATL
            I=IHINDX(N)
            J=JHINDX(N)
!        write(0,*) 'I,J from INDX: ', I,J
        if (J .ge. jsta .and. J .le. jend) then
            BUFVAR(N)=DUMMY(I,J)
!        write(0,*) 'good I,J,N: ', I,J,N
        endif
          enddo

  979   continue
        
!        write(0,*) 'past N do loop for jsta, jend(12): ', jsta, jend

!        write(0,*) 'BUFVAR(1:20): ', BUFVAR(1:20)

                                                                                          
      
       RETURN
       END    

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine getnemsandplace_3d(nfile,im,jm,lm,tmp,fldsize,recnameloc,reclevtyp &
                                ,reclev,nrec,spval,VarName,VcoordName               &
                                ,l,impf,jmpf,nframed2,NSTATL,IHINDX,JHINDX,BUFVAR)
!      
      use nemsio_module
      implicit none
      type(nemsio_gfile),intent(inout) :: nfile
      INCLUDE "mpif.h"
!
      integer,intent(in):: fldsize, nrec
      character(len=20),intent(in) :: VarName,VcoordName
      character(len=8) :: recnameloc(nrec)
      character(len=16) :: reclevtyp(nrec)
      integer :: reclev(nrec)
      real,intent(in) :: spval,tmp(fldsize*nrec)
      integer,intent(in) :: im,jm,lm,l,impf,jmpf,NSTATL
      integer :: iret,i,j,nframed2,nframe,N,recn,ll
      integer :: jsta,jend,jsta_2l,jend_2u,js
      integer :: IHINDX(NSTATL),JHINDX(NSTATL),fldst
      real ::  dummy(im,jm), BUFVAR(NSTATL,LM)
      real, allocatable:: dum1d(:)
      
        nframe=nframed2*2
	allocate(dum1d((impf)*(jmpf)))

!        do L=1,LM

!         call nemsio_readrecv(nfile,trim(VarName)                      &  
!         ,trim(VcoordName),l,dum1d,nframe=nframe,iret=iret)	 

        ll=L

!        write(0,*) 'call getrecn with nrec, varname', nrec, varname

        call getrecn(recnameloc,reclevtyp,reclev,nrec,varname,VcoordName,ll,recn)

!        write(0,*) 'recn,recnameloc: ', recn,recnameloc(recn)

        jsta=1
        jend=jm

        if(recn/=0) then
          fldst=(recn-1)*fldsize
!        write(0,*) 'fldst: ', fldst
          do j=jsta,jend
            js=(j-jsta)*im
            do i=1,im
              dummy(i,j)=tmp(i+js+fldst)
            enddo
          enddo

	  do N=1,NSTATL
            I=IHINDX(N)
            J=JHINDX(N)
            BUFVAR(N,L)=DUMMY(I,J)
          enddo

	end if
	deallocate(dum1d)
                                                                                          
      
       RETURN
       END    


      subroutine getnemsandplace_3d_para(nfile,im,jsta &
                   ,jend,jsta_2l,jend_2u & 
                   ,lm,tmp,fldsize,recnameloc,reclevtyp &
                   ,reclev,nrec,spval,VarName,VcoordName       &
                  ,l,impf,jmpf,nframed2,NSTATL,IHINDX,JHINDX,BUFVAR)
!      
      use nemsio_module
      implicit none
      type(nemsio_gfile),intent(inout) :: nfile
      INCLUDE "mpif.h"
!
      integer,intent(in):: fldsize, nrec
      character(len=20),intent(in) :: VarName,VcoordName
      character(len=8) :: recnameloc(nrec)
      character(len=16) :: reclevtyp(nrec)
      integer :: reclev(nrec)
      real,intent(in) :: spval,tmp(fldsize*nrec)
      integer,intent(in) :: im,jsta,jend,jsta_2l,jend_2u, &
                            lm,l,impf,jmpf,NSTATL
      integer :: iret,i,j,nframed2,nframe,N,recn,ll
      integer :: js
      integer :: IHINDX(NSTATL),JHINDX(NSTATL),fldst
      real ::  dummy(im,jsta_2l:jend_2u), BUFVAR(NSTATL,LM)
      real, allocatable:: dum1d(:)
      
        nframe=nframed2*2

        ll=L
        
!        write(0,*) ' = = = = = = = = '
!        write(0,*) 'im, jsta,jend,jsta_2l,jend_2u: ', &
!                    im,jsta,jend,jsta_2l,jend_2u
!        write(0,*) 'lm, size(tmp),fldsize: ', lm, size(tmp),fldsize
!        write(0,*) 'NSTATL, shape(BUFVAR): ', NSTATL, shape(bufvar)

        write(0,*) 'call getrecn with nrec, varname', nrec, varname

        call getrecn(recnameloc,reclevtyp,reclev,nrec,varname, &
                       VcoordName,ll,recn)

!        write(0,*) 'recn,recnameloc: ', recn,recnameloc(recn)

        if(recn/=0) then
          fldst=(recn-1)*fldsize
!         write(0,*) 'fldst: ', fldst
!        write(0,*) 'jsta, jend: ', jsta, jend
          do j=jsta,jend
            js=(j-jsta)*im
            do i=1,im
              dummy(i,j)=tmp(i+js+fldst)
            enddo
          enddo

	  do N=1,NSTATL
            I=IHINDX(N)
            J=JHINDX(N)
        if (J .ge. jsta .and. J .le. jend) then
            BUFVAR(N,L)=DUMMY(I,J)
        endif
          enddo

	end if
!	deallocate(dum1d)
                                                                                          
      
       RETURN
       END    

      subroutine getnemsandplace_old(nfile,im,jm,spval,VarName,VcoordName               &
                           ,l,impf,jmpf,nframed2,NSTATL,IHINDX,JHINDX,BUFVAR)
!      
      use nemsio_module
      implicit none
      type(nemsio_gfile),intent(inout) :: nfile
      INCLUDE "mpif.h"
!
      character(len=20),intent(in) :: VarName,VcoordName
      real,intent(in) :: spval
      integer,intent(in) :: im,jm,l,impf,jmpf,NSTATL
      integer :: iret,i,j,nframed2,nframe,N
      integer :: IHINDX(NSTATL),JHINDX(NSTATL)
      real ::  dummy(im,jm), BUFVAR(NSTATL)
      real, allocatable:: dum1d(:)

      
        nframe=nframed2*2
	allocate(dum1d((impf)*(jmpf)))
        call nemsio_readrecv(nfile,trim(VarName)                      &  
        ,trim(VcoordName),l,dum1d,nframe=nframe,iret=iret)	 
        if (iret /= 0) then
          print*,VarName,VcoordName,l," not found in NEMS file-Assigned missing values"
          dummy=spval
	else 
	  do j=1,jm
	    do i=1,im
	      dummy(i,j)=dum1d((j-1)*impf+i+nframed2)
	if (I .eq. 99 .and. J .eq. 72) then
	write(0,*) 'dummy(99,72): ', dummy(I,J)
	endif
	    end do
	  end do

	  do N=1,NSTATL
            I=IHINDX(N)
            J=JHINDX(N)
            BUFVAR(N)=DUMMY(I,J)
          enddo

	end if
	deallocate(dum1d)
                                                                                          
      
       RETURN
       END    

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine getnemsandplace_3d_old(nfile,im,jm,lm,spval,VarName,VcoordName               &
                                ,l,impf,jmpf,nframed2,NSTATL,IHINDX,JHINDX,BUFVAR)
!      
      use nemsio_module
      implicit none
      type(nemsio_gfile),intent(inout) :: nfile
      INCLUDE "mpif.h"
!
      character(len=20),intent(in) :: VarName,VcoordName
      real,intent(in) :: spval
      integer,intent(in) :: im,jm,lm,l,impf,jmpf,NSTATL
      integer :: iret,i,j,nframed2,nframe,N
      integer :: IHINDX(NSTATL),JHINDX(NSTATL)
      real ::  dummy(im,jm), BUFVAR(NSTATL,LM)
      real, allocatable:: dum1d(:)
      
        nframe=nframed2*2
	allocate(dum1d((impf)*(jmpf)))

!        do L=1,LM
        call nemsio_readrecv(nfile,trim(VarName)                      &  
        ,trim(VcoordName),l,dum1d,nframe=nframe,iret=iret)	 
        if (iret /= 0) then
          print*,VarName,VcoordName,l," not found in NEMS file-Assigned missing values"
          dummy=spval
	else 
	  do j=1,jm
	    do i=1,im
	      dummy(i,j)=dum1d((j-1)*impf+i+nframed2)
	if (I .eq. 99 .and. J .eq. 72) then
	write(0,*) 'dummy for 3D(99,72): ', dummy(I,J)
	endif
	    end do
	  end do

	  do N=1,NSTATL
            I=IHINDX(N)
            J=JHINDX(N)
            BUFVAR(N,L)=DUMMY(I,J)
          enddo

	end if
	deallocate(dum1d)
                                                                                          
      
       RETURN
       END    
