! Colarco, May 21, 2007
! sed command to replace F77 comments with F90: sed 's/^c/\!/g'
! F90-version of original CARMA initnew.f routine (see comments below from
! original routine header).

      subroutine initnew ( carma, rc )

!     types
      use carma_types_mod

      implicit none

      integer, intent(out) :: rc

#include "carma_globaer.h"

      rc = 0

#ifdef DEBUG
       write(*,*) '+ initnew'
#endif

!
!
!  @(#) initnew.f  McKie  Oct-1995
!  This routine performs a cold start initialization for a new
!  model simulation.
!
!  Note:  Array dimensions such as NX, NY, NZ, ... are
!  defined in the globaer.h file, and also serve as the
!  upper limits to index values for looping through arrays.
!
!
!  Argument list input:
!    None.
!
!  Argument list output:
!    None.
!
!
!  Include global constants and variables.
!
!      include 'globaer.h'
!
!
!  Define formats.
!
    1 format(/,'Doing a cold start initialization for a new simulation')
!
!---------------------------------------------------------------------------
!
!  Announce entry to this routine.
!
!      if( DEBUG ) write(LUNOPRT,'(/,a)') 'Enter initnew'
!
!
!  Report cold start initialization being done.
!
      write(LUNOPRT,1)
!
!
!  Define simulation title.
!
      simtitle = 'CARMA SAMPLE SIMULATION'
!
!
!  Initialize timestep index.
!
      itime = 0 
!
!
!  Initialize simulation time [s].
!
      time = 0._f
!
!
!  Initialize atmospheric structure.
!
      call initatm ( carma, rc )
!
!
!  Define mapping arrays and time-independent parameters for
!  aerosol and cloud microphysics.
!
      call setupaer ( carma, rc )
      if( rc /= 0) return
!
!
!  Initialize particle concentrations.
!
       call initaer ( carma, rc )
!
!
!  Initialize gas concentrations.
!
!      call initgas ( carma, rc )
!
!
!  Initialize radiative transfer model, or zero the radiative
!  heating rates.
!
#undef DO_RAD
#ifdef DO_RAD
      if( do_rad )then

        call initrad

        do ix = 1,NX
          do iy = 1,NY

            ixy = NX * ( iy - 1 ) + ix 

            call prerad
            call radtran
            call postrad

          enddo
        enddo

      else

        call zerorad

      endif
#endif

!
!
!  Return to caller with cold start initialization complete.
!
      return
      end
