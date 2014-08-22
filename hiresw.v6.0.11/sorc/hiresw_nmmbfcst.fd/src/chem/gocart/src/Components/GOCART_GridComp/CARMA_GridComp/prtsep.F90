! Colarco, May 21, 2007
! sed command to replace F77 comments with F90: sed 's/^c/\!/g'
! F90-version of original CARMA prtsep.f routine (see comments below from
! original routine header).

      subroutine prtsep ( carma, rc )

!     types
      use carma_types_mod

      implicit none

      integer, intent(out) :: rc

#include "carma_globaer.h"

#ifdef DEBUG
       write(*,*) '+ prtsep'
#endif

!
!
!  @(#) prtsep.f  McKie  Sep-1997
!  This routine outputs separator line to the print file.
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
!  Define formats
!
    1 format(/,79('='),/)
!
!
!  Announce entry to this routine
!
!      if( DEBUG ) write(LUNOPRT,'(/,a)') 'Enter prtsep'
!
!
!  Output the separator line
!
      write(LUNOPRT,1)
!
!
!  Return to caller with separator line output to print file
!
      rc = 0

      return
      end
