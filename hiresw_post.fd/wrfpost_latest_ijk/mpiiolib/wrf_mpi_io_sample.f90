!    sample code fragment for parallel read of binary wrf file 3d field.

!   In this case, because the current wrf 3d files are stored in ikj order, this shows
!     how to read in a 3d file by reading in x-z slabs in parallel, with different y ranges
!     on each processor.  This is easy to do.  It is less straightforward to read in parallel
!     subdivisions in other directions.

use mpi, only: mpi_comm_world,mpi_mode_rdonly,mpi_info_null,mpi_real4,mpi_status_size

character(???) filename

integer status(mpi_status_size)
integer nrecs,nx,ny,nz,jstart(npes),jend(npes)
real(4),allocatable::buf(:,:,:)
integer,allocatable:: start_block(:),end_block(:)
integer,allocatable:: start_byte(:),end_byte(:)
integer(8),allocatable:: file_offset(:)
integer(8) offset,this_offset
character(132),allocatable:: datestr_all(:),varname_all(:)
integer,allocatable:: domainend_all(:,:)



!   1.  count up number of records on wrf file 

  call count_recs_wrf_binary_file(in_unit,filename,nrecs)


!   2.  inventory file to get information on each record and absolute starting byte address for
!        each record
!                      (more information is available--it just requires modifying subroutine
!                          inventory_wrf_binary_file to add more output arguments with the
!                           desired information)

  allocate(datestr_all(nrecs),varname_all(nrecs),domainend_all(3,nrecs))
  allocate(start_block(nrecs),end_block(nrecs),start_byte(nrecs),end_byte(nrecs),file_offset(nrecs))

  call inventory_wrf_binary_file(in_unit,filename,nrecs, &
                                 datestr_all,varname_all,domainend_all, &
                                 start_block,end_block,start_byte,end_byte,file_offset)

!  probably important to close this file, before opening it again below with mpi_file_open

  close(in_unit)

!    obtain the header record number for 3d temperature field (mnemonic "T")

  call retrieve_index(index,'T',varname_all,nrecs)

!    retrieve dimension information for this field -- note, assume already it is ikj, but this
!     information is available in the header and can be passed out by modifying 
!      subroutine inventory_wrf_binary_file--see above.

  nx=domainend_all(1,index)
  ny=domainend_all(3,index)
  nz=domainend_all(2,index)

!   starting byte address is for the next record, following the header record--
!     thats why index+1

  offset=file_offset(index+1)


!   open file for read only with mpi-io

   call mpi_file_open(mpi_comm_world,filename,mpi_mode_rdonly,mpi_info_null,iunit,ierror)

!     filename:  character string containing name of file to read

!     mpi_mode_rdonly   :  mpi parameter to open for read only
!                           (for read/write, use mpi_mod_rdwr)

!     mpi_info_null:       mpi parameter to use default mpi-io optimization parameters

!     iunit:               output "handle"  (similar to unit number in traditional fortran read/write
!                                            but assigned by mpi_file_open)
!

!    index in y direction has been partitioned somehow between processors, and 

!     jstart(mype) <= j <= jend(mype)  on processor mype

!     allocate a place to put the block of temperatures

   allocate(buf(nx,nz,jstart(mype):jend(mype))
   
!   calculate starting byte offset from beginning of the file being byte 0.

      this_offset=offset+(jstart(mype)-1)*4*nx*nz  !  the factor 4 is because each word is 4 bytes long

!      calculate word length (4 bytes per word in this case)

      this_length=nx*nz*(jend(mype)-jstart(mype)+1)

      call mpi_file_read_at(iunit,this_offset,buf,this_length,mpi_real4,status,ierror)

   do j=jstart(mype),jend(mype)

     do k=1,nz
       do i=1,nx
       -------- stuff using buf(i,k,j)
       end do
     end do

   end do

!       can write back out if you want to same location in same file, or to another file

!       this is all going on in parallel without problem as long as there is no need to

!       pass information from other ranges of y that are not available on processor mype.


!   when done, don't forget to close file

   call mpi_file_close(iunit,ierror)

