! Colarco, May 24, 2007
! sed command to replace F77 comments with F90: sed 's/^c/\!/g'
! F90-version of original CARMA initaer.f routine (see comments below from
! original routine header).

      subroutine initaer ( carma, rc )

!     types
      use carma_types_mod

      implicit none

      integer, intent(out) :: rc

!     Local declarations
      integer :: ielem, ie, ig, ip, j, iz, iy, ix, jz, &
                 iztop, izbot
      real(kind=f) :: sum, totn, r0, rsig, arg1, arg2
      real(kind=f) :: zz, delz, zmid

#include "carma_globaer.h"

      rc = 0

#ifdef DEBUG
       write(*,*) '+ initaer'
#endif

!  @(#) initaer.f  Jensen  Oct-1995
!  This routine initializes the particle concentrations

if( .not. do_hostmodel )then

!  Initialize particle number densities 
!  Core mass is assumed to be 100% of particle mass
!
      do ielem = 1,NELEM
       ig = igelem(ielem)
       ip = ienconc(ig)
       do j = 1,NBIN
        do iz = 1,NZ
         do iy = 1,NY
          do ix = 1,NX

           if( ielem .eq. ip )then
!
!  Particle number concentration [#/m^3]
!
             pc(ix,iy,iz,j,ielem) = SMALL_PC

           elseif( itype(ielem) .eq. I_COREMASS )then
!
!  Core mass concentration [kg/m^3]
!
             pc(ix,iy,iz,j,ielem) = pc(ix,iy,iz,j,ip) * &
                                    rmass(j,ig) * FIX_COREF

           elseif( itype(ielem) .eq. I_CORE2MOM )then
!
!  Second moment of core mass distribution [ (kg/m^3)^2 ]
!
             pc(ix,iy,iz,j,ielem) = pc(ix,iy,iz,j,ip) * &
                                    (rmass(j,ig)*FIX_COREF)**2._f

           endif
 
          enddo
         enddo
        enddo
       enddo
      enddo
!
!
!  Initial particle distribution: log-normal size distribution 
!  for first particle group (which has only one particle element)
!  in a single column
!
      ig = 1
      ie = ienconc(ig)

      do ix = 1,NX
      do iy = 1,NY
      do iz = 1,NZ
!
!
!  Calculate index <jz> that increases with altitude
!
        if( igridv .eq. I_CART )then
           jz = iz
        else
           jz = NZ + 1 - iz
        endif
!
!  Log-normal parameters:
!  
!    r0   = number mode radius
!    rsig = geometric standard deviation
!    totn = total number concentration
!
        r0   = 1.e-7_f
        rsig = 1.2_f
        totn = 25.e6_f
!
!
!  Smooth gradient of <totn> 
!
!       if( NZ .gt. 1 )then
!         totn = 1.e9_f / 10._f**( (jz-1) / float(NZ-1) )
!       else
!         totn = 1.e8_f
!       endif
!
!
!  Particles in the top layer only
!
!       if( jz .eq. NZ )then
!         totn = 1.e9_f
!       else
!         totn = 0._f
!       endif
!
!
!  Adjust prefactor to yield particle number concentration <ntot>
!
        sum = 0.
        do j = 1,NBIN
          arg1 = dr(j,ig) / ( sqrt(2._f*PI) * r(j,ig) * log(rsig) ) 
          arg2 = -log( r(j,ig) / r0 )**2._f / ( 2._f*log(rsig)**2._f )
          sum  = sum + arg1 * exp( arg2 )
        enddo
        totn = totn / sum

        do j = 1,NBIN

          arg1 = totn * dr(j,ig) / ( sqrt(2._f*PI) * r(j,ig) * log(rsig) ) 
          arg2 = -log( r(j,ig) / r0 )**2._f / ( 2._f*log(rsig)**2._f )

          pc(ix,iy,iz,j,ie) = max( arg1 * exp( arg2 ), SMALL_PC )

        enddo

      enddo
      enddo
      enddo
!
!
!  Log-normal parameters for hydrometeors
!
!      ig = 2
!      ie = ienconc(ig)
!      r0   = 50.e-6_f
!      rsig = 1.4_f
!      totn = 0._f
!
!      do ix = 1,NX
!      do iy = 1,NY
!      do iz = 1,NZ
!
!        sum = 0.
!        do j = 1,NBIN
!          arg1 = dr(j,ig) / ( sqrt(2._f*PI) * r(j,ig) * log(rsig) ) 
!          arg2 = -log( r(j,ig) / r0 )**2._f / ( 2._f*log(rsig)**2._f )
!          sum  = sum + arg1 * exp( arg2 )
!        enddo
!        totn = totn / sum
!
!        do j = 1,NBIN
!
!          arg1 = totn * dr(j,ig) / ( sqrt(2._f*PI) * r(j,ig) * log(rsig) ) 
!          arg2 = -log( r(j,ig) / r0 )**2._f / ( 2._f*log(rsig)**2._f )
!
!          if( zc(ix,iy,iz) .gt. 7.e3_f .and.
!     $        zc(ix,iy,iz) .le. 8.e3_f ) then
!          pc(ix,iy,iz,j,ie) = max( arg1 * exp( arg2 ), SMALL_PC )
!c          pc(ix,iy,iz,j,ie+1) = pc(ix,iy,iz,j,ie) *
!c     $                              rmass(j,ig) * 0.0001_f
!          pc(ix,iy,iz,j,ie+1) = pc(ix,iy,iz,j,ie) *
!     $                              rmass(j,ig) * FIX_COREF
!          endif
!
!        enddo
!
!      enddo
!      enddo
!      enddo
!
!
!  Initialize a single bin with some particles.
!
!      pc(1,1,1,17,2) = 10.
!      pc(1,1,1,21,4) = 20.
!      pc(1,1,1,17,3) = 10.*rmass(20,2)*FIX_COREF
!      pc(1,1,1,21,5) = 20.*rmass(21,3)*FIX_COREF
!
!
!     Initial particle size distribution varying in height
      zz = 0._f
      do iz = NZ, 1, -1
        delz = (zl(1,1,iz+1)-zl(1,1,iz))*p_surf(1,1) / &
                 (rhoa(1,1,iz)/(xmet(1,1,iz)*ymet(1,1,iz)*zmet(1,1,iz))) / &
                 grav 
        zz = zz + delz
        zmid = zz - delz / 2._f
        pc(1,1,iz,:,:) = &
          1.934380e-10_f * exp( - ( ( zmid - 2.e3_f ) / 2.e3_f ) ** 2 ) * &
          (rhoa(1,1,iz)/(xmet(1,1,iz)*ymet(1,1,iz)*zmet(1,1,iz)))
      enddo

endif  ! .not. do_hostmodel

!  PRC August 3, 2007
!  The convention for top and bottom needs to be agreed upon (concur with JAS)
!  I choose that "top" refers to level NZ and "bottom" refers to level 1 in 
!  all vertical coordinate 
!
!  Specify fluxes at top and bottom of model
!  [#/m^2/s for particle number types, kg/m^2/s for core mass, etc.]
!
      ftoppart(:,:,:,:) = 0._f
      fbotpart(:,:,:,:) = 0._f
!
!
!  Scale particle concentrations and boundary fluxes from
!  cartesian coordinates to coordinate system specified by <igrid>
!
!  Pick indices for top and bottom layers
!
      iztop = NZ
      izbot = 1

      do ie = 1, NELEM
        do j = 1,NBIN

         pc(:,:,:,j,ie) = pc(:,:,:,j,ie) * (xmet*ymet*zmet)
         ftoppart(:,:,j,ie) = ftoppart(:,:,j,ie) * (xmet(:,:,iztop)*ymet(:,:,iztop) )
         fbotpart(:,:,j,ie) = fbotpart(:,:,j,ie) * (xmet(:,:,izbot)*ymet(:,:,izbot) )

        enddo
      enddo
!
!
!  Specify the values of <pc> assumed just above(below) the top(bottom)
!  of the model domain.
!  
      do ie = 1, NELEM
        do j = 1,NBIN
         do iy = 1, NY
          do ix = 1, NX
            pc_topbnd(ix,iy,j,ie) = pc(ix,iy,NZ,j,ie)
            pc_botbnd(ix,iy,j,ie) = pc(ix,iy,1,j,ie)
          enddo
         enddo
        enddo
      enddo

!
!
!  Return to caller with particle concentrations initialized.
!
      return
      end
