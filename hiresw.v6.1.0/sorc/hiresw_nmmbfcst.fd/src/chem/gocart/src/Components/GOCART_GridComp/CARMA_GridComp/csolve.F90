subroutine csolve( carma, rc, ibin, ielem )

! carma types

use carma_types_mod

implicit none

integer :: rc
integer :: ibin, ielem
integer :: igroup
integer :: ix, iy, iz
real(kind=f) :: xyzmet
real(kind=f) :: ppd, pls

!  @(#) csolve.f  McKie  Sep-1997
!  This routine calculates new particle concentrations from coagulation
!  microphysical processes.
!
!   The basic form from which the solution is derived is
!   ( new_value - old_value ) / dtime = source_term - loss_rate*new_value
!
!  This routine derived from psolve.f code, in which particle concentrations
!  due to coagulation were formerly included, before the relatively slow
!  coagulation calcs were separated from the other microphysical processes
!  so that time splitting could be applied to these fast & slow calcs.
!
!  Argument list input:
!    ielem, ibin
!
!  Argument list output:
!    None.
!
!  Include global constants and variables
!
!      include 'globaer.h'

#include "carma_globaer.h"

!  Announce entry to this routine
!
!      if( DEBUG ) write(LUNOPRT,'(/,a)') 'Enter csolve'

#ifdef DEBUG
  write(*,*) '+ csolve', ibin, ielem
#endif

!  Define current group & particle number concentration element indices

      igroup = igelem(ielem)         ! particle group

!  Visit each spatial point
!
!      do ixyz = 1,NXYZ

do iz = 1, NZ
  do iy = 1, NY
    do ix = 1, NX

!  Metric scaling factor
!
!        xyzmet = xmet3(ixyz)*ymet3(ixyz)*zmet3(ixyz)

      xyzmet = xmet(ix,iy,iz) * ymet(ix,iy,iz) * zmet(ix,iy,iz)

!  Compute total production rate due to coagulation
!
!        ppd = coagpe(ixyz,ibin,ielem) / xyzmet

        ppd = coagpe(ix,iy,iz,ibin,ielem) / xyzmet

!  Compute total loss rate due to coagulation
!
!        pls = coaglg(ixyz,ibin,igroup) / xyzmet

        pls = coaglg(ix,iy,iz,ibin,igroup) / xyzmet

!  Update net particle number concentration during current timestep
!  due to production and loss rates for coagulation
!
!        pc3(ixyz,ibin,ielem) = ( pc3(ixyz,ibin,ielem) + dtime*ppd ) /
!     $                         ( ONE + pls*dtime )

        pc(ix,iy,iz,ibin,ielem) = ( pc(ix,iy,iz,ibin,ielem) &
                                    + dtime * ppd ) &
                                  /  ( ONE + pls * dtime )

    enddo
  enddo
enddo

!  Return to caller with new particle number concentrations.

rc = 0

return
end
