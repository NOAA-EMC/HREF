! Colarco, Jan. 9, 2007
! sed command to replace F77 comments with F90: sed 's/^c/\!/g'
! F90-version of original CARMA vertadv.f routine (see comments below from
! original routine header).
!
   subroutine vertadv ( ix, iy, km, ibbnd, itbnd, vtrans, cvert, &
                        cvert_bbnd, cvert_tbnd, vertadvu, vertadvd, carma, rc )
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
   real(kind=f) :: vtrans(km+1), cvert(km)
   real(kind=f) :: cvert_bbnd, cvert_tbnd

!  Output
   real(kind=f), dimension(km+1) :: vertadvu, vertadvd

!  Local
   integer, parameter :: d = selected_real_kind(15,307)
   integer :: k
   integer :: nzm1, nzm2, itwo
   real(kind=d), dimension(km) :: cold, dela, delma, aju, ar, al, a6
   real(kind=d) :: dpc, dpc1, dpcm1
   real(kind=d) :: ratt1, ratt2, ratt3, rat1, rat2, rat3, rat4, den1
   real(kind=d) :: com2, x, xpos
   real(kind=d) :: cvert0, cvertnzp1
   real(kind=d) :: one_double

!  Discontinuity test
   real(kind=d), dimension(km) :: dela2, eta
   real(kind=d) :: aldj, ardj, den2, den3

   real(kind=d), parameter :: zero = 0.0, uno = 1.0

#include "carma_globaer.h"

#ifdef DEBUG
   write(*,*) '+ vertadv'
#endif

!
!
!  @(#) vertadv.f  Jensen  Dec-1996
!  This routine calculates vertrical advection rates using
!  Piecewise Polynomial Method [Colela and Woodard, J. Comp. Phys.,
!  54, 174-201, 1984]
!
!  <vertadvu(k)> is upward vertical transport rate into level k
!                from level k-1 [cm/s].
!  <vertadvd(k)> is downward vertical transport rate into level k-1 from level k.
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
!  Local declarations
!
!      dimension dela(NZ), delma(NZ), aju(NZ),
!     $  ar(NZ), al(NZ), a6(NZ), cold(NZ)
!
!
!  Announce entry to this routine.
!
!      if( DEBUG ) write(LUNOPRT,'(/,a)') 'Enter vertadv'
!
!
!  Define local ONE
   one_double = 1._d

!  Store old values of cvert
!
      do k = 1,NZ
        cold(k) = cvert(k)
      enddo
!
!
!  Initialize fluxes to zero
!
      do k = 1,NZ+1
        vertadvu(k) = 0._f
        vertadvd(k) = 0._f
      enddo
!
!
!  First, use cubic fits to estimate concentration values at layer
!  boundaries (eq. 1.6 - 1.8)
!
      do k = 2,NZ-1

        dpc = cvert(k) / dz(ix,iy,k)
        dpc1 = cvert(k+1) / dz(ix,iy,k+1)
        dpcm1 = cvert(k-1) / dz(ix,iy,k-1)
        ratt1 = dz(ix,iy,k) / &
          ( dz(ix,iy,k-1) + dz(ix,iy,k) + dz(ix,iy,k+1) ) 
        ratt2 = ( 2.*dz(ix,iy,k-1) + dz(ix,iy,k) ) / &
                ( dz(ix,iy,k+1) + dz(ix,iy,k) )
        ratt3 = ( 2.*dz(ix,iy,k+1) + dz(ix,iy,k) ) / &
                ( dz(ix,iy,k-1) + dz(ix,iy,k) )
        dela(k) = ratt1 * &
                 ( ratt2*(dpc1-dpc) + ratt3*(dpc-dpcm1) )

! PRC: a better check...
!         if( ( ( (dpc1-dpc) > 0._f .and. (dpc-dpcm1) > 0._f ) .or.    &
!               ( (dpc1-dpc) < 0._f .and. (dpc-dpcm1) < 0._f ) ) .and. &
!             dela(k) .ne. 0._f ) then
        if( (dpc1-dpc)*(dpc-dpcm1) .gt. 0._f .and. dela(k) .ne. 0._f ) then
          delma(k) = min( abs(dela(k)), 2._f*abs(dpc-dpc1), &
               2._f*abs(dpc-dpcm1) ) * sign(one_double,dela(k))
!               2._f*abs(dpc-dpcm1) ) * (abs(dela(k))/dela(k))
        else
          delma(k) = 0._f
        endif

      enddo     ! k = 2,NZ-2

      do k = 2,NZ-2

        dpc = cvert(k) / dz(ix,iy,k)
        dpc1 = cvert(k+1) / dz(ix,iy,k+1)
        dpcm1 = cvert(k-1) / dz(ix,iy,k-1)
        rat1 = dz(ix,iy,k) / &
                ( dz(ix,iy,k) + dz(ix,iy,k+1) )
        rat2 = 2._f * dz(ix,iy,k+1) * dz(ix,iy,k) / &
               ( dz(ix,iy,k) + dz(ix,iy,k+1) )
        rat3 = ( dz(ix,iy,k-1) + dz(ix,iy,k) ) / &
               ( 2._f*dz(ix,iy,k) + dz(ix,iy,k+1) )
        rat4 = ( dz(ix,iy,k+2) + dz(ix,iy,k+1) ) / &
               ( 2._f*dz(ix,iy,k+1) + dz(ix,iy,k) )
        den1 = dz(ix,iy,k-1) + dz(ix,iy,k) + &
               dz(ix,iy,k+1) + dz(ix,iy,k+2)
!
!  <aju(k)> is the estimate for concentration (dn/dz) at layer
!  boundary <k>+1/2.
!
        aju(k) = dpc + rat1*(dpc1-dpc) + 1._f/den1 * &
                 ( rat2*(rat3-rat4)*(dpc1-dpc) - &
                 dz(ix,iy,k)*rat3*delma(k+1) + &
                 dz(ix,iy,k+1)*rat4*delma(k) )

      enddo     ! k = 2,NZ-2
!
!
!  Now construct polynomial functions in each layer
!
      do k = 3,NZ-2

        al(k) = aju(k-1)
        ar(k) = aju(k)

      enddo
!
!  Use linear functions in first two and last two layers
!
      ar(2) = aju(2)
      al(2) = cvert(1)/dz(ix,iy,1) + &
              (zl(ix,iy,2)-zc(ix,iy,1)) / &
              (zc(ix,iy,2)-zc(ix,iy,1)) * &
              (cvert(2)/dz(ix,iy,2)- &
              cvert(1)/dz(ix,iy,1))
      ar(1) = al(2)
      al(1) = cvert(1)/dz(ix,iy,1) - &
              (zc(ix,iy,1)-zl(ix,iy,1)) / &
              (zc(ix,iy,2)-zc(ix,iy,1)) * &
              (cvert(2)/dz(ix,iy,2)- & 
              cvert(1)/dz(ix,iy,1)) 

      al(NZ-1) = aju(NZ-2)
      ar(NZ-1) = cvert(NZ-1)/dz(ix,iy,NZ-1) + &
              (zl(ix,iy,NZ)-zc(ix,iy,NZ-1)) &
              / (zc(ix,iy,NZ)-zc(ix,iy,NZ-1)) * &
              (cvert(NZ)/dz(ix,iy,NZ)- &
              cvert(NZ-1)/dz(ix,iy,NZ-1))
      al(NZ) = ar(NZ-1)
      ar(NZ) = cvert(NZ-1)/dz(ix,iy,NZ-1) + &
              (zl(ix,iy,NZ+1)-zc(ix,iy,NZ-1)) &
              / (zc(ix,iy,NZ)-zc(ix,iy,NZ-1)) * &
              (cvert(NZ)/dz(ix,iy,NZ)- &
              cvert(NZ-1)/dz(ix,iy,NZ-1))
!
!  Ensure that boundary values are not negative
!
      al(1) = max( al(1), zero )
      ar(NZ) = max( ar(NZ), zero )
!
!
!  Discontinuity test (eq. 1.14 - 1.17)
!  This is not coded robustly in single precision.  Only exercise in double precision
!#ifndef SINGLE
!
      do k = 2, NZ-1
        dpc = cvert(k) / dz(ix,iy,k)
        dpc1 = cvert(k+1) / dz(ix,iy,k+1)
        dpcm1 = cvert(k-1) / dz(ix,iy,k-1)
        den1 = dz(ix,iy,k-1)+dz(ix,iy,k)+dz(ix,iy,k+1)
        den2 =               dz(ix,iy,k)+dz(ix,iy,k+1)
        den3 = dz(ix,iy,k-1)+dz(ix,iy,k)
        dela2(k) = 1._f/den1 * ( (dpc1-dpc)/den2 - (dpc-dpcm1)/den3)
      enddo
      eta(:) = 0._f
      do k = 3, NZ-2
        dpc = cvert(k) / dz(ix,iy,k)
        dpc1 = cvert(k+1) / dz(ix,iy,k+1)
        dpcm1 = cvert(k-1) / dz(ix,iy,k-1)
        if(-dela2(k+1)*dela2(k-1) .gt. 0. .and. &
           ( abs(dpc1-dpcm1) - 0.01_f*min(abs(dpc1),abs(dpcm1))) .gt. 0.) then
         eta(k) = -( (dela2(k+1)-dela2(k-1)) / (zc(ix,iy,k+1)-zc(ix,iy,k-1)) ) &
                *  (  (zc(ix,iy,k) - zc(ix,iy,k-1))**3  &
                    + (zc(ix,iy,k+1) - zc(ix,iy,k))**3 ) / (dpc1-dpcm1)
        else
         eta(k) = zero
        endif
        eta(k) = max(zero,min(20._f*(eta(k)-0.05_f),uno))
        aldj = dpcm1 + 0.5_f*delma(k-1)
        ardj = dpc1 - 0.5_f*delma(k+1)
        al(k) = al(k)*(1._f - eta(k)) + aldj*eta(k)
        ar(k) = ar(k)*(1._f - eta(k)) + ardj*eta(k)
      enddo
!#endif
!
!  Next, ensure that polynomial functions do not deviate beyond the
!  range [<al(k)>,<ar(k)>] (eq. 1.10)
!
      do k = 1,NZ

        dpc = cvert(k) / dz(ix,iy,k)
        if( (ar(k)-dpc)*(dpc-al(k)) .le. 0._f ) then
          al(k) = dpc
          ar(k) = dpc
        endif

        if( (ar(k)-al(k))*( dpc - 0.5_f*(al(k)+ar(k)) ) &
            .gt. 1._f/6._f*(ar(k)-al(k))**2 ) &
           al(k) = 3._f*dpc - 2._f*ar(k)

        if( (ar(k)-al(k))*( dpc - 0.5_f*(al(k)+ar(k)) ) &
            .lt. -1._f/6._f*(ar(k)-al(k))**2 ) &
           ar(k) = 3._f*dpc - 2._f*al(k)

      enddo
!
!
!  Calculate fluxes across each layer boundary (eq. 1.5)
!
      do k = 1,NZ

        dpc = cvert(k) / dz(ix,iy,k)
        dela(k) = ar(k) - al(k)
        a6(k) = 6._f * ( dpc - 0.5_f*(ar(k)+al(k)) )

      enddo

      do k = 1,NZ-1

        com2  = ( dz(ix,iy,k) + dz(ix,iy,k+1) ) / 2._f
        x = vtrans(k+1)*dtime/dz(ix,iy,k)
        xpos = abs(x)
!
!  Upward transport rate
!
        if( vtrans(k+1) .gt. 0._f )then

          if( x .lt. 1._f )then
            vertadvu(k+1) = ( vtrans(k+1) * com2 ) * &
                       ( ( ar(k) - 0.5_f*dela(k)*x + &
                       (x/2._f - x**2/3._f)*a6(k) ) / cvert(k) )
!
!  If Courant # > 1, use upwind advection
!
          else
            vertadvu(k+1) = vtrans(k+1)
          endif
!
!  Downward transport rate
!
        elseif( vtrans(k+1) .lt. 0._f )then

          if( x .gt. -1._f )then
            vertadvd(k+1) = ( -vtrans(k+1) * com2 ) * &
                     ( ( al(k+1) + 0.5_f*dela(k+1)*xpos + &
                     ( xpos/2._f - xpos**2/3._f)*a6(k+1) ) / cvert(k+1) )
          else
            vertadvd(k+1) = -vtrans(k+1)
          endif

        endif

      enddo    ! k = 1,NZ-1
!
!
!  Lower boundary transport rates:  If I_FIXED_CONC boundary
!  condition is selected, then use concentration assumed just beyond
!  the lowest layer edge to calculate the transport rate across
!  the bottom boundary of the model.
!
      if( ibbnd .eq. I_FIXED_CONC ) then

        com2  = ( dz(ix,iy,1) + dz(ix,iy,2) ) / 2._f
        x = vtrans(1)*dtime/dz(ix,iy,1)
        xpos = abs(x)
        cvert0 = cvert_bbnd
        if( vtrans(1) .gt. 0._f )then

          if( x .lt. 1._f )then
            vertadvu(1) = vtrans(1)/cvert0*com2 &
                       * ( ar(1) - 0.5_f*dela(1)*x + &
                       (x/2._f - x**2/3._f)*a6(1) )
          else
            vertadvu(1) = vtrans(1)
          endif

        elseif( vtrans(1) .lt. 0._f )then

          if( x .gt. -1._f )then
            vertadvd(1) = -vtrans(1)/ &
                       cvert(1)*com2 &
                       * ( al(1) + 0.5_f*dela(1)*xpos + &
                       (xpos/2._f - xpos**2/3._f)*a6(1) )
          else
            vertadvd(1) = -vtrans(1)
          endif

        endif

      endif
!
!
!  Upper boundary transport rates
!
      if( itbnd .eq. I_FIXED_CONC ) then

        com2  = ( dz(ix,iy,NZ) + dz(ix,iy,NZ-1) ) / 2._f
        x = vtrans(NZ+1)*dtime/dz(ix,iy,NZ)
        xpos = abs(x)
        cvertnzp1 = cvert_tbnd

        if( vtrans(NZ+1) .gt. 0._f )then

          if( x .lt. 1._f )then
            vertadvu(NZ+1) = vtrans(NZ+1)/cvert(NZ)*com2 &
                     * ( ar(NZ) - 0.5_f*dela(NZ)*x + &
                     (x/2._f - x**2/3._f)*a6(NZ) )
          else
            vertadvu(NZ+1) = vtrans(NZ+1)
          endif

        elseif( vtrans(NZ+1) .lt. 0._f )then

          if( x .gt. -1. )then
            vertadvd(NZ+1) = -vtrans(NZ+1)/ &
                     cvertnzp1*com2 &
                     * ( al(NZ) + 0.5_f*dela(NZ)*xpos + &
                     (xpos/2._f - xpos**2/3._f)*a6(NZ) )
          else
            vertadvd(NZ+1) = -vtrans(NZ+1)
          endif

        endif

      endif

!     PRC
!     This is perhaps a better solution to the linear fits across the
!     edge two layers (?)
      if(vtrans(NZ) .gt. 0._f)   vertadvu(NZ) = vtrans(NZ)
      if(vtrans(NZP1) .gt. 0._f) vertadvu(NZP1) = vtrans(NZP1)
      if(vtrans(1) .lt. 0._f)    vertadvd(1) = vtrans(1)
      if(vtrans(2) .lt. 0._f)    vertadvd(2) = vtrans(2)

!
!
!  Return to caller with vertical transport rates.
!

      rc = 0

      return
      end
