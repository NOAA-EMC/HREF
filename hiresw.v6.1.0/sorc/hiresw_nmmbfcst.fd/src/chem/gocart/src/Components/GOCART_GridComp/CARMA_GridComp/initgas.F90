! Colarco, May 24, 2007
! sed command to replace F77 comments with F90: sed 's/^c/\!/g'
! F90-version of original CARMA initgas.f routine (see comments below from
! original routine header).

      subroutine initgas ( carma, rc )

!     types
      use carma_types_mod

      implicit none

      integer, intent(out) :: rc

!     Local declarations
      integer :: igas, j, iz, iy, ix, &
                 iztop, izbot, kb, ke, idk
      real(kind=f) :: rh_init, rhi_init, rvap, xyzmet

#include "carma_globaer.h"

      rc = 0

#ifdef DEBUG
       write(*,*) '+ initgas'
#endif

!       subroutine initgas
!
!
!  @(#) initgas.f  Ackerman  Dec-1995
!  This routine initializes the atmospheric profiles of all gases.
!
!    gc       Gas concentration at layer mid-point [g/cm^3]
!
!  Presently the only vertical coordinate is altitude, zl [cm].
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
!   Define formats
!
    1 format(/,'Gas concentrations for ',a,'(initgas)',//, &
        a3, 1x, 4(a11,4x), /) 
    2 format(i3,1x,1p,3(e11.3,4x),0p,f11.3)

!
!-------------------------------------------------------------------------------
!
!  Announce entry to this routine
!
!      if( DEBUG ) write(LUNOPRT,'(/,a)') 'Enter initgas'
!
!-------------------------------------------------------------------------------
!
!
!   Calculate vapor pressures
!
      do iz = 1,NZ
       do iy = 1,NY
        do ix = 1,NX
         call vaporp ( ix, iy, iz, carma, rc )
        enddo
       enddo
      enddo
!
!
!   Parameters for water vapor profile
!
      igas = 1
!
!
!   <rh_init> is initial relative humidity [%]
!
      rh_init = 40._f
      rhi_init = 60._f
      rh_init = 100._f
      rhi_init = 100._f
      rh_init = 80._f
!
!
!   Gas constant for water vapor
!
      rvap = RGAS/gwtmol(igas)
!
!
!   Loop over all spatial dimensions and gases
!
      do igas = 1,NGAS
       do ix = 1,NX
        do iy = 1,NY
         do iz = 1,NZ

           if( igas .eq. 1 )then
!
!
!   Water vapor concentration from relative humidity and vapor pressure
!

             if( zc(ix,iy,iz) .ge. 5.e3 .and. &
                 zc(ix,iy,iz) .le. 6.e3 ) then
              gc(ix,iy,iz,igas) = rh_init/100._f*pvapl(ix,iy,iz,igas) &
                              / ( rvap*t(ix,iy,iz) ) 
             else
              gc(ix,iy,iz,igas) = 0.25_f*pvapi(ix,iy,iz,igas) &
                              / ( rvap*t(ix,iy,iz) )
             endif

!             if( t(ix,iy,iz) .ge. T0 ) then
!               gc(ix,iy,iz,igas) = rh_init/100._f*pvapl(ix,iy,iz,igas) &
!                                   / ( rvap*t(ix,iy,iz) )
!             else
!               gc(ix,iy,iz,igas) = rhi_init/100._f*pvapi(ix,iy,iz,igas) &
!                                   / ( rvap*t(ix,iy,iz) )
!             endif

           else
             write(LUNOPRT,'(/,a)') 'invalid <igas> in initgas.f'
             stop 1
           endif

         enddo
        enddo
       enddo
      enddo
!
!  PRC August 3, 2007
!  The convention for top and bottom needs to be agreed upon (concur with JAS)
!  I choose that "top" refers to level NZ and "bottom" refers to level 1 in 
!  all vertical coordinate 
!
!  Specify fluxes at top and bottom of model [kg/m^2/s]
!
      ftopgas(:,:,:) = 0._f
      fbotgas(:,:,:) = 0._f
!
!
!  Scale particle concentrations and boundary fluxes from 
!  cartesian coordinates to coordinate system specified by <igrid>
!
!
!  Pick indices for top and bottom layers
!
      iztop = NZ
      izbot = 1

      do igas = 1, NGAS

        gc(:,:,:,igas ) = gc(:,:,:,igas) * xmet*ymet*zmet
        ftopgas(:,:,igas) = ftopgas(:,:,igas) * xmet(:,:,iztop)*ymet(:,:,iztop)
        fbotgas(:,:,igas) = fbotgas(:,:,igas) * xmet(:,:,izbot)*ymet(:,:,izbot)

      enddo
!
!
!  Initialize <supsati> and <supsatl>
!
      do iz = 1,NZ
       do iy = 1,NY
        do ix = 1,NX
         call supersat ( ix, iy, iz, carma, rc )
        enddo
       enddo
      enddo
!
!
!  Print gas concentrations at 1 horizontal grid point
!
      ix = 1
      iy = 1
!
!
!  Set vertical loop index to increment downwards
!
      if( igridv .eq. I_CART )then
        kb  = NZ
        ke  = 1
        idk = -1
      else 
        kb  = 1
        ke  = NZ
        idk = 1
      endif

      do igas = 1,NGAS

        write(LUNOPRT,1) gasname(igas), &
                         'iz','zc','gc [kg/m^3]','supsat','T [K]'

        do iz = kb,ke,idk
          xyzmet = xmet(ix,iy,iz)*ymet(ix,iy,iz)*zmet(ix,iy,iz)
          write(LUNOPRT,2) iz, zc(ix,iy,iz), gc(ix,iy,iz,igas)/xyzmet, &
             supsatl(ix,iy,iz,igas), t(ix,iy,iz)
        enddo

      enddo
!
!
!  Specify the values of <gc> assumed just above(below) the top(bottom)
!  of the model domain.  
!
      do igas = 1,NGAS
       do iy = 1, NY
        do ix = 1, NX
          gc_topbnd(ix,iy,igas) = gc(ix,iy,NZ,igas)
          gc_botbnd(ix,iy,igas) = gc(ix,iy,1,igas)
        enddo
       enddo
      enddo
!
!  Return to caller with gas concentrations initialized.
!
      return
      end
