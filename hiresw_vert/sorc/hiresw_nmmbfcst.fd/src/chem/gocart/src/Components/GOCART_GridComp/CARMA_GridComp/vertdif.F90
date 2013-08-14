! Colarco, May 14, 2007
! sed command to replace F77 comments with F90: sed 's/^c/\!/g'
! F90-version of original CARMA vertdif.f routine (see comments below from
! original routine header).
!
   subroutine vertdif( ix, iy, km, ibbnd, itbnd, vertdifu, vertdifd, carma, rc )
!
! Define variables and usages
   use carma_types_mod

   implicit none

!  Input/Output
   integer, intent(out)             :: rc         ! Error return code:
                                                  !  0 - all is well
                                                  !  1 - 

!  Input
   integer, intent(in)      :: ix, iy, km, ibbnd, itbnd

!  Output
   real(kind=f), dimension(km+1) :: vertdifu, vertdifd

!  Local
   integer :: k
   real(kind=f) :: dz_avg, rhofact, xex, ttheta

#include "carma_globaer.h"

#ifdef DEBUG
   write(*,*) '+ vertdif'
#endif

!  Initialize: set vertdifd = 0, vertdifu = 0
   vertdifd = 0._f
   vertdifu = 0._f

!
!
!  @(#) vertdif.f  Jensen  Dec-1996
!  This routine calculates vertrical transport rates.
!  Currently treats diffusion only.
!  Not necessarily generalized for irregular grid.
!
!  <vertdifu(k)> is upward vertical transport rate into level k from level k-1.
!  <vertdifd(k)> is downward vertical transport rate into level k-1 from level k.
!
!  Modified  Sep-1997  (McKie)
!  Remove <ixy> from arg list
!  <ixy> now available as a global var in common block.
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
!  Announce entry to this routine.
!
!      if( DEBUG ) write(LUNOPRT,'(/,a)') 'Enter vertdif'
!
!
!  Loop over vertical levels.
!
      do k = 2, NZ

        dz_avg = dz(ix,iy,k)                            ! layer thickness
!
!  Check the vertical coordinate
!  
        if( igridv .eq. I_CART ) then
          rhofact = log(  rhoa(ix,iy,k)/rhoa(ix,iy,k-1) &
                       * zmet(ix,iy,k-1)/zmet(ix,iy,k) )
          xex = rhoa(ix,iy,k-1)/rhoa(ix,iy,k) * &
               zmet(ix,iy,k)/zmet(ix,iy,k-1)
          vertdifu(k) = ( rhofact * dkz(ix,iy,k)/dz_avg ) / &
                        ( 1. - xex )

          vertdifd(k) = vertdifu(k) * xex
!
!  ...else you're in sigma coordinates...
!
        elseif( igridv .eq. I_SIG ) then
          vertdifu(k) = dkz(ix,iy,k)/dz_avg
          vertdifd(k) = dkz(ix,iy,k)/dz_avg
!
!  ...else write an error (maybe redundant)...
!  
        else
!          write(LUNOPRT,*) 'Invalid vertical grid type'
          rc = 1
          return
        endif

      enddo
!
!
!  Fluxes at boundaries specified by user
!
      if( ibbnd .eq. I_FLUX_SPEC ) then
        vertdifu(1) = 0.
        vertdifd(1) = 0.
      endif

      if( itbnd .eq. I_FLUX_SPEC ) then
        vertdifu(NZ+1) = 0.
        vertdifd(NZ+1) = 0.
      endif
!
!
!  Diffusion across boundaries using fixed boundary concentration:
!
      if( ibbnd .eq. I_FIXED_CONC ) then
        dz_avg = dz(ix,iy,1)                            ! layer thickness
        rhofact = log( rhoa(ix,iy,2)/rhoa(ix,iy,1) )
        ttheta = rhofact
        if( ttheta .ge. 0. ) then
          ttheta = min(ttheta,POWMAX)
        else
          ttheta = max(ttheta,-POWMAX)
        endif

        xex = exp(-ttheta)
        if( abs(ONE - xex) .lt. ALMOST_ZERO ) xex = ALMOST_ONE

        vertdifu(1) = ( rhofact * dkz(ix,iy,1)/dz_avg ) / &
                     ( 1. - xex )
        vertdifd(1) = vertdifu(1) * xex
      endif

      if( itbnd .eq. I_FIXED_CONC ) then
        dz_avg = dz(ix,iy,NZ)                            ! layer thickness
        rhofact = log( rhoa(ix,iy,NZ)/rhoa(ix,iy,NZ-1) )
        ttheta = rhofact
        if( ttheta .ge. 0. ) then
          ttheta = min(ttheta,POWMAX)
        else
          ttheta = max(ttheta,-POWMAX)
        endif

        xex = exp(-ttheta)
        if( abs(ONE - xex) .lt. ALMOST_ZERO ) xex = ALMOST_ONE

        vertdifu(NZ+1) = ( rhofact * dkz(ix,iy,NZ+1)/dz_avg ) / &
                         ( 1. - xex )
        vertdifd(NZ+1) = vertdifu(NZ+1) * xex
      endif
!
!
!  Return to caller with vertical diffusion rates.
!
      rc = 0

      return
      end
