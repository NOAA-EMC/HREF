subroutine coagl( carma, rc )

! carma types defs

use carma_types_mod

implicit none

integer :: rc
integer :: ig, jg, je, igrp
integer :: ix, iy, iz
integer :: i, j

!  @(#) coagl.f  Jensen  Oct-1995
!  This routine calculates coagulation loss rates <coaglgg>.
!  See [Jacobson, et al., Atmos. Env., 28, 1327, 1994] for details
!  on the coagulation algorithm.
!
!  The loss rates for all particle elements in a particle group are equal.
!
!  Include global constants and variables

#include "carma_globaer.h"

!  Announce entry to this routine
!
!      if( DEBUG ) write(LUNOPRT,'(/,a)') 'Enter coagl'

#ifdef DEBUG
  write(*,*) '+ coagl'
#endif

rc = 0

!  Loop over particle groups for which coagulation loss is being
!  calculated.

do ig = 1,NGROUP

!  Loop over particle groups that particle in group ig might
!  collide with.

  do jg = 1,NGROUP

!  Element corresponding to particle number concentration

    je = ienconc(jg)

!  Particle resulting from coagulation between groups <ig> and <jg> goes
!  to group <igrp>

    igrp = icoag(ig,jg)

!  Resulting particle is in same group as particle under consideration --
!  partial loss (muliplies <volx>).

    if( igrp .eq. ig )then

!  Loop over horizontal grid points

      do iy = 1,NY
        do ix = 1, NX

          ckernel => carma%ckernel(ix,iy)%data5d

          do iz = 1, NZ

            if( pconmax(ix,iy,iz,jg) .gt. FEW_PC .and. &
                pconmax(ix,iy,iz,ig) .gt. FEW_PC )then

              do i = 1, NBIN-1
               do j = 1, NBIN

                coaglg(ix,iy,iz,i,ig) = coaglg(ix,iy,iz,i,ig) & 
                          + ckernel(iz,i,j,ig,jg) * pcl(ix,iy,iz,j,je) &
                          * volx(igrp,ig,jg,i,j) 
              enddo
             enddo
            endif
          enddo  ! iz
        enddo  ! ix
      enddo  ! iy

!  Resulting particle is in a different group -- complete loss (no <volx>).

    else if( igrp .ne. ig .and. igrp .ne. 0 )then

!  Loop over horizontal grid points

      do iy = 1,NY
        do ix = 1, NX

          ckernel => carma%ckernel(ix,iy)%data5d

          do iz = 1, NZ

!  Bypass calculation if few particles present

            if( pconmax(ix,iy,iz,jg) .gt. FEW_PC .and. &
                pconmax(ix,iy,iz,ig) .gt. FEW_PC )then

             do i = 1, NBIN
              do j = 1, NBIN

                coaglg(ix,iy,iz,i,ig) = coaglg(ix,iy,iz,i,ig) &
                        +  ckernel(iz,i,j,ig,jg) * pcl(ix,iy,iz,j,je)

              enddo
             enddo
            endif  ! pconmax(ig) * pconmax(jg) > FEW_PC ** 2
          enddo  ! iz
        enddo  ! ix
      enddo  ! iy

    endif  ! igrp .eq. ig ?
  enddo  ! jg
enddo  ! ig


!  Boundary condition: Particles from bin <NBIN> are only lost by
!  coagulating into other elements. (This is taken care of by <NBIN>-1
!  limit above)

!  Return to caller with particle loss rates due to coagulation evaluated.

rc = 0

return
end
