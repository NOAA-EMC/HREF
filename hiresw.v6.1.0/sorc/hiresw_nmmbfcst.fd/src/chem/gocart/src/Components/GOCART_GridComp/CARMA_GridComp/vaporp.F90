! Colarco, May 24, 2007
! sed command to replace F77 comments with F90: sed 's/^c/\!/g'
! F90-version of original CARMA vaporp.f routine (see comments below from
! original routine header).

      subroutine vaporp ( ix, iy, iz, carma, rc )

!     types
      use carma_types_mod

      implicit none

!     Inputs
      integer :: ix, iy, iz

!     Outputs
      integer, intent(out) :: rc

!     Local declarations
      integer :: igas
      real(kind=f) :: tt
      real(kind=f), parameter :: BAI = 6.1115e2_f, &
                                 BBI = 23.036_f, &
                                 BCI = 279.82_f, &
                                 BDI = 333.7_f, &
                                 BAL = 6.1121e2_f, &
                                 BBL = 18.729_f, &
                                 BCL = 257.87_f, &
                                 BDL = 227.3_f

#include "carma_globaer.h"

      rc = 0

#ifdef DEBUG
!       write(*,*) '+ vaporp'
#endif

!       subroutine vaporp
!
!
!  @(#) vaporp.f  Ackerman  Dec-1995
!  This routine calculates the vapor pressure for all gases 
!  over the entire spatial grid:
!
!  <pvapl> and <pvapi> are vapor pressures in units of [dyne/cm^2]
!
!  Uses temperature <t> as input.
!
!  Modified  Sep-1997  (McKie)
!  To calculate at one spatial point per call.
!  Globals <ix>, <iy>, <iz>, <ixy>, <ixyz> specify current spatial pt's indices.
!  (actually, only <ixyz> is defined -- the others are meaningless)
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
!  Define coefficients in Buck's formulation for saturation vapor pressures
!  Table 2
!
!     Ice: valid temperature interval -80 - 0 C
!      parameter( BAI = 6.1115_f )
!      parameter( BBI = 23.036_f )              
!      parameter( BCI = 279.82_f )
!      parameter( BDI = 333.7_f  )

!     Liquid: valid temperature interval -40 - +50 C
!      parameter( BAL = 6.1121_f )           
!      parameter( BBL = 18.729_f )
!      parameter( BCL = 257.87_f )
!      parameter( BDL = 227.3_f  )
!
!
!  Define formats
!
    1 format('T = ',1pe12.3,a,3(i6,2x),a,1pe11.3)
!
!-------------------------------------------------------------------------------
!
!  Announce entry to this routine
!
!      if( DEBUG ) write(LUNOPRT,'(/,a)') 'Enter vaporp'
!
!-------------------------------------------------------------------------------
!
!
!  Loop over all gases.
!
      do igas = 1, NGAS
!
!
!  Check for expected gas index
!
          if( igas .eq. 1 )then
!
!
!  Saturation vapor pressure over liquid water and water ice
!  (from Buck [J. Atmos. Sci., 20, 1527, 1981])
!
           tt = t(ix,iy,iz) - 273.16_f

           pvapl(ix,iy,iz,igas) = BAL * &
                               exp( (BBL - tt/BDL)*tt / (tt + BCL) )

           pvapi(ix,iy,iz,igas) = BAI * &
                               exp( (BBI - tt/BDI)*tt / (tt + BCI) )
!
!  Check to see whether temperature is ouside range of validity
!  for parameterizations
!
           if( pvapl(ix,iy,iz,igas) .le. 1.e-13_f ) then
            write(LUNOPRT,1) t(ix,iy,iz), ' too small for ix,iy,iz = ', &
                             ix,iy,iz, ' time = ',time
            stop 1
           endif
!
!
!  Report unexpected gas index
!
          else
            write(LUNOPRT,'(/,a)') 'invalid <igas> in vaporp.f'
            stop 1
          endif
 
      enddo
!
!
!  Return to caller with vapor pressures evaluated.
!
      return
      end
