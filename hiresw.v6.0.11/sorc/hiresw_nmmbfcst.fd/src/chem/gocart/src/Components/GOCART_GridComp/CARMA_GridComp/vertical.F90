! Colarco, Jan. 9, 2007
! sed command to replace F77 comments with F90: sed 's/^c/\!/g'
! F90-version of original CARMA vertical.f routine (see comments below from
! original routine header).
!
! Notes:
!  1) #preprocessor directives turn off the advection of air density, 
!     gases, ptc
!  2) outer do loop over iy, ix (old way was ixy)
!  3) hard coded the following assumptions for particle transport
!     ibbnd = itbnd = I_FLUX_SPEC
!     ftop = fbot = 0
!     move out of way divcor portion
!     assumes only particle transport due to sedimentation (no winds, 
!     no diffusion)
!  4) A departure from the original CARMA, have to think this through.  On
!     our assumption of a sigma coordinate, on return from setupvf the
!     particle fall velocity is a positive number.  In here, then, vtrans
!     is equal to the fall velocity (vtrans = vf) and the particles move
!     toward higher sigma (toward the ground).
!
   subroutine vertical ( carma, rc )
!
! Define variables and usages
   use carma_types_mod

   implicit none

!  Input/Output
   integer, intent(out)             :: rc         ! Error return code:
                                                  !  0 - all is well
                                                  !  1 - 
!  Local
   integer :: ix, iy, k, ibin, ielem, ios
   integer, parameter :: ig = 1
   integer :: itbnd, ibbnd
   real(kind=f) :: fbot, ftop
   real(kind=f), pointer, dimension(:) :: vtrans, &
                                          vertadvu, vertadvd, &
                                          vertdifu, vertdifd, &
                                          cvert
   real(kind=f) :: cvert_bbnd, cvert_tbnd
   real(kind=f) :: colwght_before, colwght_after

#include "carma_globaer.h"

#ifdef DEBUG
   write(*,*) '+ vertical'
#endif

   allocate( vtrans(carma%NZP1), vertadvu(carma%NZP1), vertadvd(carma%NZP1), &
             vertdifu(carma%NZP1), vertdifd(carma%NZP1), cvert(carma%NZ), &
             stat=ios)
   if(ios /= 0) then
    rc = 1
    return
   endif

!
!
!  @(#) vertical.f  Jensen  Mar-1995
!  This routine drives the vertical transport calculations.
!
!  Modified  Sep-1997  (McKie)
!  Remove <ixy> from arg list of called routines.
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
!      dimension drho_dt_flux_spec(NZ), drho_dt_fixed_conc(NZ)
!
!
!  Define formats
!
!    3 format(i3,3x,6e13.4)
!    4 format(i3,3x,i3,3x,i3,3x,6f9.2)
!
!
!  Announce entry to this routine.
!
!      if( DEBUG ) write(LUNOPRT,'(/,a)') 'Enter vertical'
! 
!=======================================================================
!
!
!
!  Loop over horizontal grid points
!
   do iy = 1,NY
    do ix = 1, NX

       vf => carma%vf(ix,iy)%data3d

!
!=======================================================================
#undef RHOATRANS
#ifdef RHOATRANS
!
!  Treat vertical transport of air density 
!
!
!  First calculate change in density for the case when boundary
!  fluxes are zero
!
        itbnd = I_FLUX_SPEC
        ibbnd = I_FLUX_SPEC

        ftop = 0.
        fbot = 0.
!
!
!  Store temporary (work) values in <cvert>, 
!  evaluate vertical velocities at layer boundaries,
!  and set divergence correction term to zero.
!
        do k = 1,NZP1
          vtrans(k) = w2(ixy,k)
          if( k .le. NZ )then
            cvert(k) = rhoa2(ixy,k)
            divcor(k) = 0.
          endif
        enddo
        vtrans(1) = 0.
        vtrans(NZP1) = 0.
!
!
!  Since diffusion does not occur for air density, set diffusion
!  fluxes to zero.
!
        do k = 1,NZP1
          vertdifu(k) = 0.
          vertdifd(k) = 0.
        enddo
!
!
!  Calculate particle transport rates due to vertical advection
!  (note: no diffusion for air density), and solve for air density
!  at end of time step.
!
        call vertadv
        call versol

        if( NXY .eq. 1 )then
!
!
!  In 1D, assume assume any vertical divergence is
!  balanced by horizontal convergence: don't allow air density
!  to change in time, but calculate rate of change that would have
!  resulted from advection -- this tendency is then used below for
!  a divergence correction.
!
          do k = 1,NZ
            drho_dt_flux_spec(k) = ( rhoa2(ixy,k) - cvert(k) ) /  &
                         ( rhoa2(ixy,k) * dtime )
          enddo

        else
!
!
!  Update air density when not in 1D.
!
          do k = 1,NZ
            rhoa2(ixy,k) = cvert(k)
          enddo

        endif
!
!
!  Next, calculate change in density for the case when the
!  concentration just beyond the boundary is fixed
!
        itbnd = I_FIXED_CONC
        ibbnd = I_FIXED_CONC

        do k = 1,NZP1
          vtrans(k) = w2(ixy,k)
        enddo
        do k = 1,NZ
          cvert(k) = rhoa2(ixy,k)
        enddo

        cvert_tbnd = cvert(NZ)
        cvert_bbnd = cvert(1)

        call vertadv
        call versol

        if( NXY .eq. 1 )then
          do k = 1,NZ
            drho_dt_fixed_conc(k) = ( rhoa2(ixy,k) - cvert(k) ) / &
                         ( rhoa2(ixy,k) * dtime )
          enddo
        endif
!
#endif
! RHOATRANS
!=======================================================================
!
!  Treat vertical transport of particles.
!
        itbnd = itbnd_pc
        ibbnd = ibbnd_pc

        do ielem = 1,NELEM          ! Loop over particle elements

!          ig = igelem(ielem)        ! particle group

          do ibin = 1,NBIN          ! Loop over particle mass bins
!
!
!  Fluxes across top and bottom of model
!
            ftop = ftoppart(ix,iy,ibin,ielem)
            fbot = fbotpart(ix,iy,ibin,ielem)

!
!
!  Store temporary (work) values in <cvert>, 
!  evaluate vertical velocities at layer boundaries, and
!  when 1D, assign divergence correction term
!
            do k = 1,NZP1
!              vtrans(k) = w2(ixy,k) - vf(k,ibin,ig)
              vtrans(k) = - vf(k,ibin,ig)
            enddo
            if( ibbnd .eq. I_FLUX_SPEC ) vtrans(1) = 0._f
            if( itbnd .eq. I_FLUX_SPEC ) vtrans(NZP1) = 0._f
            cvert_tbnd = 0._f
            cvert_bbnd = 0._f
            if( itbnd .eq. I_FIXED_CONC ) &
              cvert_tbnd = pc(ix,iy,NZ,ibin,ielem)
            if( ibbnd .eq. I_FIXED_CONC ) &
              cvert_bbnd = pc(ix,iy,1,ibin,ielem)
            do k = 1,NZ
              cvert(k) = pc(ix,iy,k,ibin,ielem)
!              if( NXY. eq. 1 ) divcor(k) = cvert(k) *
!     $                                     drho_dt_flux_spec(k)
            enddo
!            if( NXY .eq. 1 ) then
!              if( ibbnd .eq. I_FIXED_CONC )
!     $          divcor(1) = cvert(1) * drho_dt_fixed_conc(1)
!              if( itbnd .eq. I_FIXED_CONC )
!     $          divcor(NZ) = cvert(NZ) * drho_dt_fixed_conc(NZ)
!            endif
!
!
!  Calculate particle transport rates due to vertical advection
!  and vertical diffusion, and solve for concentrations at end of time step.
!
!
            call vertadv( ix, iy, NZ, ibbnd, itbnd, vtrans, cvert, &
                          cvert_bbnd, cvert_tbnd, vertadvu, vertadvd, carma, rc )
            call vertdif( ix, iy, NZ, ibbnd, itbnd, vertdifu, vertdifd, carma, rc )
            call versol ( ix, iy, NZ, ibbnd, itbnd, fbot, ftop, &
                          vertadvu, vertadvd, vertdifu, vertdifd, &
                          cvert_bbnd, cvert_tbnd, cvert, carma, rc )
!
!
!  Update particle concentrations.
!
            do k = 1,NZ
              pc(ix,iy,k,ibin,ielem) = cvert(k)
            enddo

          enddo
        enddo
!
!=======================================================================
#undef GASTRANS
#ifdef GASTRANS
!
!  Treat vertical transport of gases 
!  (for comments, see above treatment of particles).
!
!
        itbnd = itbnd_gc
        ibbnd = ibbnd_gc

        do k = 1,NZP1
          vtrans(k) = w2(ixy,k)
        enddo
        if( ibbnd .eq. I_FLUX_SPEC ) vtrans(1) = 0.
        if( itbnd .eq. I_FLUX_SPEC ) vtrans(NZP1) = 0.

        do igas = 1,NGAS

          ftop = ftopgas(ixy,igas)
          fbot = fbotgas(ixy,igas)
          if( itbnd .eq. I_FIXED_CONC )
     $      cvert_tbnd = gc_topbnd(ixy,igas)
          if( ibbnd .eq. I_FIXED_CONC )
     $      cvert_bbnd = gc_botbnd(ixy,igas)

          do k = 1,NZ
            cvert(k) = gc2(ixy,k,igas)
            if( NXY. eq. 1 ) divcor(k) = cvert(k) * drho_dt_flux_spec(k)
          enddo
          if( NXY .eq. 1 ) then
            if( ibbnd .eq. I_FIXED_CONC )
     $        divcor(1) = cvert(1) * drho_dt_fixed_conc(1)
            if( itbnd .eq. I_FIXED_CONC )
     $        divcor(NZ) = cvert(NZ) * drho_dt_fixed_conc(NZ)
          endif

          call vertadv
          call vertdif
          call versol

          do k = 1,NZ
            gc2(ixy,k,igas) = cvert(k)
          enddo

        enddo
!
#endif
! GASTRANS
!=======================================================================
#undef PTCTRANS
#ifdef PTCTRANS
!
!  Treat vertical transport of potential temperature 
!  (for comments, see above treatment of particles).
!
        itbnd = itbnd_ptc
        ibbnd = ibbnd_ptc
 
        ftop = 0.
        fbot = 0.

        do k = 1,NZP1
          vtrans(k) = w2(ixy,k)
        enddo
        if( ibbnd .eq. I_FLUX_SPEC ) vtrans(1) = 0.
        if( itbnd .eq. I_FLUX_SPEC ) vtrans(NZP1) = 0.
        if( itbnd .eq. I_FIXED_CONC ) cvert_tbnd = ptc_topbnd(ixy)
        if( ibbnd .eq. I_FIXED_CONC ) cvert_bbnd = ptc_botbnd(ixy)
        do k = 1,NZ
          cvert(k) = ptc2(ixy,k)
          if( NXY. eq. 1 ) divcor(k) = cvert(k) * drho_dt_flux_spec(k)
        enddo
        if( NXY .eq. 1 ) then
          if( ibbnd .eq. I_FIXED_CONC )
     $      divcor(1) = cvert(1) * drho_dt_fixed_conc(1)
          if( itbnd .eq. I_FIXED_CONC )
     $      divcor(NZ) = cvert(NZ) * drho_dt_fixed_conc(NZ)
        endif

        call vertadv
        call vertdif
        call versol

        do k = 1,NZ
          ptc2(ixy,k) = cvert(k)
        enddo
!
#endif
! PTCTRANS
!=======================================================================
!
    end do   ! ix
   end do    ! iy
!
!=======================================================================
!
!
!  Return to caller with new particle, gas, and potential temperature
!  concentrations and air density.
!
      rc = 0
      deallocate( vtrans, vertadvd, vertadvu, vertdifd, vertdifu, &
                  cvert, stat=ios)
      if(ios /= 0) rc = 1


      return
      end
