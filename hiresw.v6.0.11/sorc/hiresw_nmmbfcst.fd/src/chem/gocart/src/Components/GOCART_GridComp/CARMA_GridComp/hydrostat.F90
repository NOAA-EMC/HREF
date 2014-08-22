! Colarco, May 28, 2007
! sed command to replace F77 comments with F90: sed 's/^c/\!/g'
! F90-version of original CARMA hydrostat.f routine (see comments below from
! original routine header).

       subroutine hydrostat ( carma, rc )

!     types
      use carma_types_mod

      implicit none

      integer, intent(out) :: rc

!     Local declarations
      integer :: iz, ios
      real(kind=f), allocatable, dimension(:,:)  :: dp, ptop, pstar, &
                                                    xymet, rhoa_mks

#include "carma_globaer.h"

      rc = 0
      allocate(dp(carma%NX,carma%NY), ptop(carma%NX,carma%NY), &
               pstar(carma%NX,carma%NY), xymet(carma%NX,carma%NY), &
               rhoa_mks(carma%NX,carma%NY), stat=ios)
      if(ios /= 0) then
       rc = 1
       return
      endif

#ifdef DEBUG
      write(*,*) '+ hydrostat'
#endif

!
!
!  @(#) hydrostat.f  Ackerman  Jul-1997
!
!    This routine updates pressure by hydrostatic integration.
!    In sigma coordinates, it also updates vertical metric scale
!    factor <zmet>, temperature <t>, and scaled air density <rhoa>.
!
!  Argument list input:
!
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
!-------------------------------------------------------------------------------
!
!  Announce entry to this routine
!
!      if( DEBUG ) write(LUNOPRT,'(/,a)') 'Enter hydrostat'
!
!
!-------------------------------------------------------------------------------
!
!
!  cartesian coordinates
!
        if( igridv .eq. I_CART )then
!
!
!  <ptop> is pressure at top of layer
!
          iz = NZ
          ptop = p_top
          dp = dz(:,:,iz)*GRAV*rhoa(:,:,iz)
          p(:,:,iz) = ptop * sqrt( 1. + dp/ptop )
   
          do iz = NZ-1,1,-1
            ptop = ptop + dp
            dp = dz(:,:,iz)*GRAV*rhoa(:,:,iz)
            p(:,:,iz) = ptop * sqrt( 1. + dp/ptop )
          enddo
          p_surf = ptop + dp
!
!
!  Sigma coordinates: first integrate for total column mass, then
!  update pressures and scaled air density, then temperatures, and then
!  air density (in cgs units) and vertical metric scale factor.
!
        else if( igridv .eq. I_SIG )then

          pstar = 0._f
          do iz = 1,NZ
            xymet = xmet(:,:,iz) * ymet(:,:,iz)
            pstar = pstar + dz(:,:,iz) * GRAV * rhoa(:,:,iz) / xymet
          enddo
          p_surf = p_top + pstar
!PRC
      pstar = p_surf

          do iz = 1,NZ
            xymet = xmet(:,:,iz) * ymet(:,:,iz)
!            p(:,:,iz) = p_top(:,:) + zc(:,:,iz) * pstar
!PRC - change definition of p, but this balances exactly with input profile
            p(:,:,iz) = zc(:,:,iz) * p_surf
!PRC - it's here the troubles begin.  WHat is PTC anyway?
            rhoa(:,:,iz) = pstar / GRAV * xymet
            t(:,:,iz) = ptc(:,:,iz) / rhoa(:,:,iz) * &
                        ( p(:,:,iz) / PREF )**(R_AIR/CP)
            rhoa_mks = p(:,:,iz) / ( R_AIR * t(:,:,iz) )
            zmet(:,:,iz) = pstar / ( GRAV * rhoa_mks )
          enddo

        endif
!
!
!  Return to caller with pressures hydrostatically balanced.
!
      deallocate(dp, ptop, pstar, xymet, rhoa_mks, stat=ios)
      if(ios /= 0) rc = 1

      return
      end
