! Colarco, May 21, 2007
! sed command to replace F77 comments with F90: sed 's/^c/\!/g'
! F90-version of original CARMA setuperr.f routine (see comments below from
! original routine header).


      subroutine setuperr ( carma, rc )
!     types
      use carma_types_mod

      implicit none

      integer, intent(out) :: rc

#include "carma_globaer.h"

#ifdef DEBUG
       write(*,*) '+ setuperr'
#endif

!
!
!
!  @(#) setuperr.f  McKie  Nov-1999
!
!
!  This routine is called to handle any run-time set up of error exception
!  handling.  Normally, it is a do-nothing routine.  But system dependent
!  error handling code could be inserted here, e.g. temporarily for debugging.
!
!
      rc = 0

      return
      end
