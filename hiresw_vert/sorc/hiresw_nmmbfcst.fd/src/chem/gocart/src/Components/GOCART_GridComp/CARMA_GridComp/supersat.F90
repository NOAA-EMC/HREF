! Colarco, May 24, 2007
! sed command to replace F77 comments with F90: sed 's/^c/\!/g'
! F90-version of original CARMA supersat.f routine (see comments below from
! original routine header).

      subroutine supersat ( ix, iy, iz, carma, rc )

!     types
      use carma_types_mod

      implicit none

!     Inputs
      integer :: ix, iy, iz

!     Outputs
      integer, intent(out) :: rc

!     Local declarations
      integer :: igas
      real(kind=f) :: rvap, gc_mks

#include "carma_globaer.h"

      rc = 0

#ifdef DEBUG
!       write(*,*) '+ supersat'
#endif

!       subroutine supersat
!
!
!  @(#) initgas.f  Ackerman  Dec-1995
!  This routine evaluates supersaturations <supsatl> and <supsati> for all gases.
!
!  Modified  Sep-1997  (McKie)
!  To calculate at one spatial point per call.
!  Globals <ix>, <iy>, <iz>, <ixy>, <ixyz> specify current spatial point's indices.
!
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
!-------------------------------------------------------------------------------
!
!  Announce entry to this routine
!
!      if( DEBUG ) write(LUNOPRT,'(/,a)') 'Enter supersat'
!
!-------------------------------------------------------------------------------
!
!
!   Calculate vapor pressures.
!
      call vaporp ( ix, iy, iz, carma, rc )
!
!
!   Loop over all gases
!
      do igas = 1,NGAS
!
!
!   Define gas constant for this gas
!
        rvap = RGAS/gwtmol(igas)

        gc_mks = gc(ix,iy,iz,igas) / (zmet(ix,iy,iz)*xmet(ix,iy,iz)*ymet(ix,iy,iz))

        supsatl(ix,iy,iz,igas) = ( gc_mks * rvap * t(ix,iy,iz) - &
           pvapl(ix,iy,iz,igas) ) / pvapl(ix,iy,iz,igas)

        supsati(ix,iy,iz,igas) = ( gc_mks * rvap * t(ix,iy,iz) - &
           pvapi(ix,iy,iz,igas) ) / pvapi(ix,iy,iz,igas)

      enddo
!
!
!  Return to caller with supersaturations evaluated.
!
      return
      end
