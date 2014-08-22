! Colarco, May 14, 2007
! sed command to replace F77 comments with F90: sed 's/^c/\!/g'
! F90-version of original CARMA versol.f routine (see comments below from
! original routine header).
!
! Notes:
!  1) I don't allow divcor for now.
!
   subroutine versol ( ix, iy, km, ibbnd, itbnd, fbot, ftop, &
                       vertadvu, vertadvd, vertdifu, vertdifd, &
                       cvert_bbnd, cvert_tbnd, cvert, carma, rc )
!
! Define variables and usages
   use carma_types_mod

   implicit none

!  Input/Output
   integer, intent(out)             :: rc         ! Error return code:
                                                  !  0 - all is well
                                                  !  1 - 
!  Input/Output
   integer :: ix, iy, km, itbnd, ibbnd
   real(kind=f) :: fbot, ftop
   real(kind=f), dimension(km+1) :: vertadvu, vertadvd, &
                                    vertdifu, vertdifd
   real(kind=f), dimension(km)   :: cvert
   real(kind=f) :: cvert_bbnd, cvert_tbnd

!  Local
   integer :: k
   real(kind=f) :: uc, cour, denom
   real(kind=f), dimension(km)   :: al, bl, dl, ul, el, fl, &
                                    ctempl, ctempu, divcor

#include "carma_globaer.h"

#ifdef DEBUG
   write(*,*) '+ versol'
#endif

   divcor(:) = 0._f

!
!
!  @(#) versol.f  Jensen  Dec-1996
!  This routine solves the vertical transport equation.
!  <cvert> is temporary storage for concentrations (particles,
!  gas, potential temperature) being transported.
!  New values of <cvert> are calculated.
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
!  Declare local variables
!
!      dimension al(NZ), bl(NZ), dl(NZ), ul(NZ),
!     $          el(NZ), fl(NZ), ctempl(NZ), ctempu(NZ)
! 
!
!  Announce entry to this routine.
!
!      if( DEBUG ) write(LUNOPRT,'(/,a)') 'Enter versol'
!
!
!  Determine whether transport should be solved explicitly (uc=0)
!  or implicitly (uc=1).
!
      uc = 0._f
      do k = 1,NZ
        cour = dz(ix,iy,k)/dtime - &
         ( vertdifu(k+1) + vertdifd(k) + vertadvu(k+1) + vertadvd(k) )
        if( cour .lt. 0. .and. uc .ne. 1. )then
          uc = 1.0_f
!          write(LUNOPRT,'(a,i3,7(1x,1pe8.1))')
!     $      'in versol: k dz/dt vdifd vdifu vadvd vadvu cour uc = ',
!     $       k, dz(ix,iy,k)/dtime, vertdifd(k), vertdifu(k+1),
!     $       vertadvd(k), vertadvu(k+1), cour, uc
        endif
      enddo
!
!
!  Store concentrations in local variables (shifted up and down
!  a vertical level).
!
      do k = 2,NZ
        ctempl(k) = cvert(k-1)
        ctempu(k-1) = cvert(k)
      enddo

      if( ibbnd .eq. I_FIXED_CONC ) then
        ctempl(1) = cvert_bbnd
      else
        ctempl(1) = 0._f
      endif

      if( itbnd .eq. I_FIXED_CONC ) then
        ctempu(NZ) = cvert_tbnd
      else
       ctempu(NZ) = 0._f
      endif
!
!
!  Calculate coefficients of the transport equation:
!    al(k)c(k+1) + bl(k)c(k) + ul(k)c(k-1) = dl(k)
!  This part could use a notation.  The equation solved is really
!  a very simply formulated solution to the flux form of the 
!  advection equation:
!    du/dt = d(vu)/dx
!  This is solved explicitly if the cfl condition is met:
!
!     n+1    n            n      n                n    n
!    u    - u  = v      (u    - u  )  +  v      (u  - u   )
!     k      k    k+1/2   k+1    k        k-1/2   k    k-1
!    ---------   ------------------------------------------
!       dt                           dx
!
!  or solved implicitly if it is not:
!
!     n+1    n            n+1    n+1              n+1  n+1
!    u    - u  = v      (u    - u  )  +  v      (u  - u   )
!     k      k    k+1/2   k+1    k        k-1/2   k    k-1
!    ---------   ------------------------------------------
!       dt                           dx
!
!  Where v(k+1/2) = vertadvd(k+1)+vertdifd(k+1)
!    and v(k-1/2) = vertadvu(k)+vertdifu(k)
!  The n+1 time-level terms are the left side of the equation and the
!  n time-level terms are the right side (we know the right side exactly).
!  The switch u is used to flip between implicit and explicit solutions.
!  The resulting set of coefficients are put into a tridiagonal solver
!  (below) with possibly a flux boundary condition implied.
!
      do k = 1,NZ
        al(k) = uc * ( vertdifd(k+1) + vertadvd(k+1) )
        bl(k) = -( uc*(vertdifd(k)+vertdifu(k+1)+ &
                       vertadvd(k)+vertadvu(k+1)) &
                   + dz(ix,iy,k)/dtime )
        ul(k) = uc * ( vertdifu(k) + vertadvu(k) )
        dl(k) = cvert(k) * &
           ( (1.-uc)*(vertdifd(k)+vertdifu(k+1)+ &
                      vertadvd(k)+vertadvu(k+1)) &
           - dz(ix,iy,k)/dtime ) - &
           (1.-uc) * ( (vertdifu(k)+vertadvu(k))*ctempl(k) + &
           (vertdifd(k+1)+vertadvd(k+1))*ctempu(k) ) - &
           divcor(k) * dz(ix,iy,k)
      enddo
!
!
!  Boundary fluxes: <ftop> is the downward flux across the
!  upper boundary; <fbot> is the upward flux across the
!  lower boundary.  Top and bottom in the coordinate sense.
!
       if( itbnd .eq. I_FLUX_SPEC ) dl(NZ) = dl(NZ) - ftop
       if( ibbnd .eq. I_FLUX_SPEC ) dl(1) = dl(1) - fbot
!
!
!  Calculate recursion relations.
!
      el(1) = dl(1)/bl(1)
      fl(1) = al(1)/bl(1)
      do k = 2,NZ
        denom = bl(k) - ul(k) * fl(k-1)
        el(k) = ( dl(k) - ul(k)*el(k-1) ) / denom
        fl(k) = al(k) / denom
      enddo
!
!
!  Calculate new concentrations.
!
      cvert(NZ) = el(NZ)
      do k = NZ-1,1,-1
        cvert(k) = el(k) - fl(k)*cvert(k+1)
      enddo
!
!
!  Return to caller with new concentrations.
!
      rc = 0

      return
      end
