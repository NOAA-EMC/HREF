 module mpimod

!----------------------------------------------------------------------
! module does the following:
!
! - breaks up model grid for each mpi task.
! - will gather model data from each task into a single full domain
!   array.
! - will scatter full domain data to its subdomain on each mpi task.
!----------------------------------------------------------------------

 implicit none

! the starting and ending i/j indices of model subgrid for this task.

 integer, public                      :: istart_mdl, iend_mdl
 integer, public                      :: jstart_mdl, jend_mdl
 integer, allocatable, public         :: iend_mdl_4_loops(:)
 integer, public                      :: myrank

 integer, allocatable, private        :: displs_g(:)
 integer, allocatable, private        :: istart_mdl_all(:), iend_mdl_all(:)
 integer, allocatable, private        :: imdl_task(:), jmdl_task(:)
 integer, allocatable, private        :: jstart_mdl_all(:), jend_mdl_all(:)
 integer, allocatable, private        :: ijn(:)
 integer, allocatable, private        :: ltosi(:), ltosj(:)

 interface gather
   module procedure gather_int
   module procedure gather_real
 end interface

 interface scatter 
   module procedure scatter_int
   module procedure scatter_real
 end interface

 contains

!----------------------------------------------------------------------
! for gaussian grids, break up the grid along latitude bands.
!----------------------------------------------------------------------

 subroutine gaussian_mpi_setup(nlon,nlat,lonsperlat)

 implicit none

 include 'mpif.h'

 integer, intent(in)        :: nlon, nlat
 integer, intent(in), optional :: lonsperlat(nlat/2)
 integer                    :: ierr, n, nprocs, npts, remain
 integer                    :: i, j, jj, ns

 call mpi_comm_size(mpi_comm_world, nprocs, ierr)
 call mpi_comm_rank(mpi_comm_world, myrank, ierr)

 npts = nlat / nprocs
 remain = mod(nlat,nprocs)

 allocate (istart_mdl_all(0:nprocs-1))
 allocate (iend_mdl_all(0:nprocs-1))
 allocate (imdl_task(0:nprocs-1))
 istart_mdl_all = 1
 iend_mdl_all   = nlon
 imdl_task  = nlon

 allocate (jstart_mdl_all(0:nprocs-1))
 allocate (jend_mdl_all(0:nprocs-1))
 allocate (jmdl_task(0:nprocs-1))

 jmdl_task = npts
 jmdl_task(0) = jmdl_task(0) + remain

 jstart_mdl_all(0) = 1 
 jend_mdl_all(0) = jmdl_task(0)

 do n = 1, nprocs-1
   jstart_mdl_all(n) = jend_mdl_all(n-1) + 1
   jend_mdl_all(n) = jstart_mdl_all(n) + jmdl_task(n) - 1
 enddo

 allocate(ijn(0:nprocs-1))
 do i = 0, nprocs-1
   ijn(i) = imdl_task(i)*jmdl_task(i)
 enddo

 allocate(displs_g(0:nprocs-1))
 allocate(ltosi(nlat*nlon))
 allocate(ltosj(nlat*nlon))

 displs_g(0)=0
 do n=0,nprocs-1
   if(n/=0) then
     displs_g(n)=displs_g(n-1)+ijn(n-1)
   end if
    do j=1,jmdl_task(n)
      ns=displs_g(n)+(j-1)*imdl_task(n)
      do i=1,imdl_task(n)
        ns=ns+1
        ltosi(ns)=istart_mdl_all(n)+i-1
        ltosj(ns)=jstart_mdl_all(n)+j-1
      end do
    end do
 enddo

 istart_mdl = istart_mdl_all(myrank)
 iend_mdl   = iend_mdl_all(myrank)
 jstart_mdl = jstart_mdl_all(myrank)
 jend_mdl   = jend_mdl_all(myrank)

 allocate (iend_mdl_4_loops(jstart_mdl:jend_mdl))
 if (present(lonsperlat))then
   do j = jstart_mdl, jend_mdl
     jj = j
     if (j > nlat/2) jj = nlat - j + 1
     iend_mdl_4_loops(j) = lonsperlat(jj)
   enddo
 else
   iend_mdl_4_loops = iend_mdl
 endif

 return

 end subroutine gaussian_mpi_setup

!----------------------------------------------------------------------
! for egrids grids, break up the grid into approximate squares.
! adapted from gsi code.
!----------------------------------------------------------------------

 subroutine nam_mpi_setup(imdl,jmdl)

 implicit none

 include 'mpif.h'

 integer, intent(in)     :: imdl, jmdl
 integer                 :: ierr, nprocs
 integer                 :: n, ns, i, j, k, ipts, jjnum
 integer                 :: npts, nrnc, iinum, iicnt, iileft, jrows, jleft, jjleft
 integer, allocatable    :: iiend(:), iistart(:), jjend(:)

 real                    :: anperpe

 call mpi_comm_size(mpi_comm_world, nprocs, ierr)
 call mpi_comm_rank(mpi_comm_world, myrank, ierr)

 allocate (iistart(nprocs+1))
 allocate (jjend(nprocs+1))
 allocate (iiend(nprocs+1))
 allocate (istart_mdl_all(0:nprocs-1))
 allocate (iend_mdl_all(0:nprocs-1))
 allocate (jstart_mdl_all(0:nprocs-1))
 allocate (jend_mdl_all(0:nprocs-1))
 allocate (imdl_task(0:nprocs-1))
 allocate (jmdl_task(0:nprocs-1))

 npts=jmdl*imdl
 anperpe=float(npts)/float(nprocs)

! Start with square subdomains
 nrnc=sqrt(anperpe)
 iinum=jmdl/nrnc
 if (iinum == 0) then
  print*,'%%% set iinum to one '
  iinum = 1
 endif
 iicnt=jmdl/iinum
 iileft=jmdl-iicnt*iinum
 jrows=nprocs/iinum
 jleft=nprocs-jrows*iinum

! Adjust subdomain boundaries
 k=0
 istart_mdl_all=1
 jstart_mdl_all=1
 iistart(1)=1
 do i=1,iinum
   ipts = iicnt
   if(i <= iileft)ipts=ipts+1
   iiend(i)=iistart(i)+ipts-1
   iistart(i+1)=iiend(i)+1
   jjnum=jrows
   if(i <= jleft)jjnum=jrows+1
   do j=1,jjnum
     k=k+1
     jmdl_task(k-1)=ipts
     jstart_mdl_all(k-1)= iistart(i)
     imdl_task(k-1)=imdl/jjnum
     jjleft=imdl-imdl_task(k-1)*jjnum
     if(j <= jjleft)imdl_task(k-1)=imdl_task(k-1)+1
     if(j > 1)istart_mdl_all(k-1)=jjend(j-1)+1
     jjend(j)=istart_mdl_all(k-1)+imdl_task(k-1)-1
     iend_mdl_all(k-1) = istart_mdl_all(k-1) + imdl_task(k-1) - 1
     jend_mdl_all(k-1) = jstart_mdl_all(k-1) + jmdl_task(k-1) - 1
!        if(myrank == 0) &
!            write(6,100) k,istart_mdl_all(k),iend_mdl_all(k),jstart_mdl_all(k),jend_mdl_all(k),imdl_task(k),jmdl_task(k)
   end do
 end do
100 format(' DETER_SUBDOMAIN:  task,istart_mdl_all,iend_mdl_all,jstart_mdl_all,jend_mdl_all,imdl_task,jmdl_task=',8(i6,1x))

 deallocate (iistart, iiend, jjend)

 allocate(ijn(0:nprocs-1))
 do i = 0, nprocs-1
   ijn(i) = imdl_task(i)*jmdl_task(i)
 enddo

 allocate(displs_g(0:nprocs-1))
 allocate(ltosi(jmdl*imdl))
 allocate(ltosj(jmdl*imdl))

 displs_g(0)=0
 do n=0,nprocs-1
    if(n/=0) then
      displs_g(n)=displs_g(n-1)+ijn(n-1)
    end if
! if (myrank == 0) print*,'displs ',n,displs_g(n)
    do j=1,jmdl_task(n)
      ns=displs_g(n)+(j-1)*imdl_task(n)
      do i=1,imdl_task(n)
        ns=ns+1
        ltosi(ns)=istart_mdl_all(n)+i-1
        ltosj(ns)=jstart_mdl_all(n)+j-1
      end do
    end do
 enddo

 istart_mdl = istart_mdl_all(myrank)
 iend_mdl   = iend_mdl_all(myrank)
 jstart_mdl = jstart_mdl_all(myrank)
 jend_mdl   = jend_mdl_all(myrank)

 allocate (iend_mdl_4_loops(jstart_mdl:jend_mdl))
 iend_mdl_4_loops = iend_mdl

 return

 end subroutine nam_mpi_setup

!----------------------------------------------------------------------
! this gathers integer subgrid data from each task (array patch) and
! places it in an array defined for the full model grid (full_2d).
!----------------------------------------------------------------------

 subroutine gather_int(patch, nlon, nlat, full_2d)

 implicit none

 include 'mpif.h'

 integer, allocatable :: full(:)
 integer              :: i,j,k,ierr
 integer, intent(in)  :: nlat, nlon
 integer, intent(in)  :: patch(imdl_task(myrank),jmdl_task(myrank))
 integer, intent(out) :: full_2d(nlon,nlat)

 allocate(full(nlon*nlat))
 
 call mpi_allgatherv(patch, ijn(myrank), mpi_integer,  &
                     full, ijn, displs_g, mpi_integer, mpi_comm_world, ierr)

 do k=1,(nlon*nlat)
   i=ltosi(k)
   j=ltosj(k)
   full_2d(i,j) = full(k)
 end do

 deallocate (full)

 return

 end subroutine gather_int

!----------------------------------------------------------------------
! this gathers floating point subgrid data from each task (array patch)
! and places it in an array defined for the full model grid (full_2d).
!----------------------------------------------------------------------

 subroutine gather_real(patch, nlon, nlat, full_2d)

 implicit none

 include 'mpif.h'

 integer                 :: i, j, k, ierr
 integer, intent(in)     :: nlat, nlon

 real, allocatable       :: full(:)
 real, intent(out)       :: full_2d(nlon,nlat)
 real, intent(in)        :: patch(imdl_task(myrank),jmdl_task(myrank))
 
 allocate(full(nlon*nlat))
 
 call mpi_allgatherv(patch, ijn(myrank), mpi_double_precision,  &
                     full, ijn, displs_g, mpi_double_precision, mpi_comm_world, ierr)

 do k=1,(nlon*nlat)
   i=ltosi(k)
   j=ltosj(k)
   full_2d(i,j) = full(k)
 end do

 deallocate (full)

 return

 end subroutine gather_real

!----------------------------------------------------------------------
! this scatters integer data on the full model grid (full_2d) to each
! mpi task (array patch). 
!----------------------------------------------------------------------

 subroutine scatter_int (patch, nlon, nlat, full_2d)

 implicit none

 include 'mpif.h'

 integer, allocatable      :: full(:)
 integer                   :: i, j, k, ierr
 integer, intent(in)       :: nlat, nlon
 integer, intent(out)      :: patch(imdl_task(myrank),jmdl_task(myrank))
 integer, intent(in )      :: full_2d(nlon,nlat)

 allocate(full(nlon*nlat))

!  Transfer input 1d array to output 2d array
  do k=1,(nlon*nlat)
     i=ltosi(k)
     j=ltosj(k)
     full(k) = full_2d(i,j) 
  end do

 call mpi_scatterv(full, ijn, displs_g, mpi_integer, &
                   patch, ijn(myrank), mpi_integer, 0, mpi_comm_world, ierr)

 deallocate (full)

 return

 end subroutine scatter_int

!----------------------------------------------------------------------
! this scatters real data on the full model grid (full_2d) to each
! mpi task (array patch). 
!----------------------------------------------------------------------

 subroutine scatter_real (patch, nlon, nlat, full_2d)

 implicit none

 include 'mpif.h'

 integer                   :: i, j, k, ierr
 integer, intent(in)       :: nlat, nlon

 real, allocatable         :: full(:)
 real, intent(in )         :: full_2d(nlon,nlat)
 real, intent(out)         :: patch(imdl_task(myrank),jmdl_task(myrank))

 allocate(full(nlon*nlat))

!  Transfer input 1d array to output 2d array
  do k=1,(nlon*nlat)
     i=ltosi(k)
     j=ltosj(k)
     full(k) = full_2d(i,j) 
  end do

 call mpi_scatterv(full, ijn, displs_g, mpi_double_precision, &
                   patch, ijn(myrank), mpi_double_precision, 0, mpi_comm_world, ierr)

 deallocate (full)

 return

 end subroutine scatter_real

 subroutine mpi_cleanup

 implicit none

 deallocate (istart_mdl_all, iend_mdl_all, jstart_mdl_all, jend_mdl_all)
 deallocate (displs_g, ijn)
 deallocate (imdl_task, jmdl_task)
 deallocate (ltosi, ltosj)
 deallocate (iend_mdl_4_loops)

 return

 end subroutine mpi_cleanup 

 end module mpimod
