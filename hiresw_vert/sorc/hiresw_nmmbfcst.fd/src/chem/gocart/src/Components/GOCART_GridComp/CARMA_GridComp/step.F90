! Colarco, May 14, 2007
! sed command to replace F77 comments with F90: sed 's/^c/\!/g'
! F90-version of original CARMA step.f routine (see comments below from
! original routine header).

      subroutine step ( carma, rc )

!     types
      use carma_types_mod

      implicit none

      integer, intent(out) :: rc

#include "carma_globaer.h"

#ifdef DEBUG
       write(*,*) '+ step'
#endif

!
!
!  @(#) step.f  McKie  Oct-1995
!  This routine performs all calculations necessary to
!  take one timestep.
!
!  Argument list input:
!    None.
!
!  Argument list output:
!    None.
!
!
!  Include global constants and variables
!
!      include 'globaer.h'
!
!
!  Announce entry to this routine
!
!      if( DEBUG ) write(LUNOPRT,'(/,a)') 'Enter step'
!
!
!  Do pre-timestep processing
!
      call prestep ( carma, rc )
!
!
!  Update model state at new time
!
      call newstate ( carma, rc )
!
!
!  Modify time-step if necessary
!
!      if( do_varstep ) call varstep
!
!
!  Do post-timestep processing
!
!      if( do_step ) call postep 
!
!
!  Return to caller with one timestep taken
!
      rc = 0

      return
      end
