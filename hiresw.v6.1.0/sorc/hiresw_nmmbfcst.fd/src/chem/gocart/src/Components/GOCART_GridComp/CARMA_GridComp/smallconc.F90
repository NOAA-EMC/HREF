subroutine smallconc(ix,iy,iz,ibin,ielem,carma,rc)

! JAS July 20, 2007 | F90-ize subroutine

use carma_types_mod

implicit none

! Arguments

integer :: ix, iy, iz, ibin, ielem
integer :: rc
! carma declared in carma_globaer.h

! Locals

integer :: igrp, iep
real(kind=f) :: small_val

! carma declaration and pointer aliasing to carma components

#include "carma_globaer.h"

!      subroutine smallconc(ibin,ielem)
!
!
!  @(#) smallconc.f  Ackerman  Oct-1997
!  This routine ensures limits all particle concentrations in a grid box
!  to SMALL_PC.  In bins where this limitation results in the particle 
!  concentration changing, the core mass fraction and second moment fraction 
!  are set to <FIX_COREF>. 
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
!      if( DEBUG ) write(LUNOPRT,'(/,a)') 'Enter smallconc'

      igrp = igelem(ielem)
      iep = ienconc(igrp)

      if( ielem .eq. iep )then

!  Element is particle concentration
 
        pc(ix,iy,iz,ibin,ielem) = max( pc(ix,iy,iz,ibin,ielem), SMALL_PC )

      else

!  Element is core mass or core second moment

        if( itype(ielem) .eq. I_COREMASS .or. &
            itype(ielem) .eq. I_VOLCORE )then

          small_val = SMALL_PC*rmass(ibin,igrp)*FIX_COREF

        elseif( itype(ielem) .eq. I_CORE2MOM )then

          small_val = SMALL_PC*(rmass(ibin,igrp)*FIX_COREF)**2

        endif

        if( pc(ix,iy,iz,ibin,iep) .le. SMALL_PC .or. &
            pc(ix,iy,iz,ibin,ielem) .lt. small_val )then

          pc(ix,iy,iz,ibin,ielem) = small_val
!         pc(ix,iy,iz,ibin,ielem) = 0._f

        endif

      endif  ! ielem .eq. iep

!  Return to caller with particle concentrations limited to SMALL_PC

rc = 0
return
end
