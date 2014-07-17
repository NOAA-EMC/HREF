subroutine microslow( carma, rc )

! carma types defs

use carma_types_mod

implicit none

integer :: rc
integer :: ibin, ielem
integer, parameter :: LUNOJAS = 42

!  @(#) microslow.f  McKie  Sep-1997
!  This routine drives the potentially slower microphysics calculations.
!
!  Originally part of microphy.  Now in this separate routine to allow
!  time splitting of coagulation at a different timestep size from
!  other microphysical calcs.
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

#include "carma_globaer.h"

!  Announce entry to this routine.
!
!      if( DEBUG ) write(LUNOPRT,'(/,a)') 'Enter microslow'

#ifdef DEBUG
  write(*,*) '+ microslow'
#endif

rc = 0

!  Do coagulation if requested
!
      if( do_coag )then
!
!
!  Calculate (implicit) particle loss rates for coagulation.
!
        call coagl( carma, rc )

!  Calculate particle production terms and solve for particle 
!  concentrations at end of time step.
!
        do ielem = 1,NELEM
          do ibin = 1,NBIN
            call coagp( carma, rc, ibin,ielem)
            call csolve( carma, rc, ibin,ielem)
          enddo
        enddo

!
!
!  End of conditional coagulation calcs
!
      endif
!
!
!  Return to caller with new particle concentrations.
!
      return
      end
