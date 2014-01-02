      subroutine getnemsandplace(nfile,im,jm,tmp,fldsize,recnameloc,reclevtyp, &
                            reclev,nrec,spval,VarName,VcoordName               &
                           ,l,impf,jmpf,nframed2,NSTATL,IHINDX,JHINDX,BUFVAR)
!      
      use nemsio_module_mpi
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


!!! gather onto root task?

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
       END subroutine getnemsandplace

      subroutine getnemsandplace_para(nfile,im,jsta,jend,jsta_2l,jend_2u,  &
                            icnt,idsp, &
                            tmp,fldsize,recnameloc,reclevtyp, &
                            reclev,nrec,spval,VarName,VcoordName               &
                           ,l,impf,jmpf,nframed2,NSTATL,IHINDX,JHINDX,BUFVAR)
!      
      use nemsio_module_mpi
      implicit none
      type(nemsio_gfile),intent(inout) :: nfile
      INCLUDE "mpif.h"
!
      integer, intent(in) :: icnt(0:127),idsp(0:127)
      integer,intent(in):: fldsize, nrec
      character(len=20),intent(in) :: VarName,VcoordName
      character(len=8) :: recnameloc(nrec)
      character(len=16) :: reclevtyp(nrec)
      integer :: reclev(nrec),ierr
      real,intent(in) :: spval
      real,intent(in) :: tmp(fldsize*nrec)
      integer,intent(in) :: im,jsta,jend,jsta_2l,jend_2u
      integer,intent(in) :: l,impf,jmpf,nframed2
      integer :: NSTATL
      integer :: iret,i,j,N
      integer :: IHINDX(NSTATL),JHINDX(NSTATL)
      real ::  dummy(im,jsta_2l:jend_2u), BUFVAR(NSTATL)
      real :: alldummy(im,jmpf)
      real, allocatable:: dum1d(:)

      
!        call nemsio_readrecv(nfile,trim(VarName)                      &  
!        ,trim(VcoordName),l,dum1d,nframe=nframe,iret=iret)	 

        if (NSTATL .lt. 10 .or.  NSTATL .gt. 1500) then
        write(0,*) 'NSTATL, trim(VarName): ', NSTATL, trim(VarName)
        endif

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
!         write(0,*) 'VarName,VcoordName: ', VarName,VcoordName
!        write(0,*) 'size(dummy): ', size(dummy)
!        write(0,*) 'NSTATL pre assignnemsiovar(6b): ', NSTATL

      call assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u        &
     & ,l,nrec,fldsize,spval,tmp                               &
     & ,recnameloc,reclevtyp,reclev,VarName,VcoordName            &
     & ,dummy)


        call mpi_gatherv(dummy(1,jsta),fldsize,MPI_REAL,         &
     &        alldummy,icnt,idsp,MPI_REAL,0,MPI_COMM_WORLD,ierr)

  982   continue

	  do N=1,NSTATL
            I=IHINDX(N)
            J=JHINDX(N)
            BUFVAR(N)=ALLDUMMY(I,J)

!        if (mod(N,15) .eq. 0) then
!        write(0,*) 'good I,J,N: ', I,J,N
!        endif

          enddo

  979   continue
        

                                                                                          
      
       RETURN
       END subroutine getnemsandplace_para   

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine getnemsandplace_3d(nfile,im,jm,lm,tmp,fldsize,recnameloc,reclevtyp &
                                ,reclev,nrec,spval,VarName,VcoordName               &
                                ,l,impf,jmpf,nframed2,NSTATL,IHINDX,JHINDX,BUFVAR)
!      
      use nemsio_module_mpi
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
       END subroutine getnemsandplace_3d


      subroutine getnemsandplace_3d_para(nfile,im,jsta &
                   ,jend,jsta_2l,jend_2u & 
                   ,lm,icnt,idsp,tmp,fldsize,recnameloc,reclevtyp &
                   ,reclev,nrec,spval,VarName,VcoordName       &
                  ,l,impf,jmpf,nframed2,NSTATL,IHINDX,JHINDX,BUFVAR)
!      
      use nemsio_module_mpi
      implicit none
      type(nemsio_gfile),intent(inout) :: nfile
      INCLUDE "mpif.h"
!
      integer,intent(in):: fldsize, nrec,icnt(0:127),idsp(0:127)
      character(len=20),intent(in) :: VarName,VcoordName
      character(len=8) :: recnameloc(nrec)
      character(len=16) :: reclevtyp(nrec)
      integer :: reclev(nrec)
      real,intent(in) :: spval,tmp(fldsize*nrec)
      integer,intent(in) :: im,jsta,jend,jsta_2l,jend_2u, &
                            lm,l,impf,jmpf,NSTATL
      integer :: iret,i,j,nframed2,nframe,N,recn,ll
      integer :: js,ierr
      integer :: IHINDX(NSTATL),JHINDX(NSTATL),fldst
      real ::  dummy(im,jsta_2l:jend_2u), BUFVAR(NSTATL,LM)
      real :: alldummy(im,jmpf)
      real, allocatable:: dum1d(:)
      
        nframe=nframed2*2

        ll=L
        
!        write(0,*) ' = = = = = = = = '
!        write(0,*) 'im, jsta,jend,jsta_2l,jend_2u: ', &
!                    im,jsta,jend,jsta_2l,jend_2u
!        write(0,*) 'lm, size(tmp),fldsize: ', lm, size(tmp),fldsize
!        write(0,*) 'NSTATL, shape(BUFVAR): ', NSTATL, shape(bufvar)

!        write(0,*) 'call getrecn with nrec, varname', nrec, varname

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

        call mpi_gatherv(dummy(1,jsta),fldsize,MPI_REAL,         &
     &        alldummy,icnt,idsp,MPI_REAL,0,MPI_COMM_WORLD,ierr)

	  do N=1,NSTATL
            I=IHINDX(N)
            J=JHINDX(N)
            BUFVAR(N,L)=ALLDUMMY(I,J)
          enddo

	end if
!	deallocate(dum1d)
                                                                                          
      
       RETURN
       END subroutine getnemsandplace_3d_para    

