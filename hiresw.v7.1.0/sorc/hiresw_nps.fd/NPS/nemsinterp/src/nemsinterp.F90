!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Program nemsinterp
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
program nemsinterp

   use gridinfo_module
   use parallel_module
   use process_domain_module


   implicit none

   ! Local variables
   integer :: n, ierr
   logical :: extra_row, extra_col, diag_prints

   !
   ! Do general setup
   !

   ! Initialize parallel stuff
    call parallel_start()

   ! Get info about how many nests there are to process, etc.
	if (my_proc_id == 0) then
           diag_prints=.true.
        else
           diag_prints=.false.
	endif
    call get_namelist_params(diag_prints)

   if (gridtype == 'E' .or. gridtype == 'B' .or. gridtype == 'A') then
      extra_col = .false.
      extra_row = .false.
   end if


   !
   ! Now begin the processing work, looping over all domains to be processed 
   !

   if (gridtype == 'E' .or. gridtype == 'B' .or. gridtype == 'A') then
      do n=1,max_dom
      call process_domain(n, extra_row, extra_col )
	write(0,*) 'DONE PROCESSING DOMAIN #: ', N, 'OUT OF : ' , max_dom
      end do  
   end if


   !
   ! Clean up and quit.
   !
   call parallel_finish()

   stop
 
end program nemsinterp

! ------------------------------------------

!
!  Needed to resolve a reference in linked code.  Try to eliminate eventually
!
   SUBROUTINE wrf_abort
      IMPLICIT NONE
#ifdef _MPI
      INCLUDE 'mpif.h'
      INTEGER ierr
      CALL mpi_abort(MPI_COMM_WORLD,1,ierr)
#else
	STOP 1
#endif
   END SUBROUTINE wrf_abort
