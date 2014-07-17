subroutine prestep( carma, rc )

! JAS July 20, 2007 | F90'ize this subroutine

use carma_types_mod

implicit none

integer :: ix, iy, iz, ibin, ielem, igrp, iep
integer :: rc

#include "carma_globaer.h"

!      subroutine prestep
!
!
!  @(#) prestep.f  McKie  Oct-1995
!  This routine handles all preliminary setup at the beginning
!  of every timestep.  Things that would appropriately be done
!  here include:
!    Input or otherwise define interface quantities from other submodels.
!    Save any model state that is needed to compute tendencies.
!    Save any model state that might be needed for comparison at end of step.
!    Update timestep counter and simulation time.
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
!  Announce entry to this routine
!
!      if( DEBUG ) write(LUNOPRT,'(/,a)') 'Enter prestep'

#ifdef DEBUG
  write(*,*) '+ prestep'
#endif

!  Don't allow particle concentrations to get too small.

!      do ixyz = 1,NXYZ

do ix = 1, NX
  do iy = 1, NY
    do iz = 1, NZ

      do ibin = 1,NBIN
        do ielem = 1,NELEM

!          call smallconc(ibin,ielem)

          call smallconc(ix,iy,iz,ibin,ielem,carma,rc)

        enddo
      enddo

    enddo
  enddo
enddo

!  Set <pcl> to <pc> from previous time step
!  and find maximum for each element.

      do ielem = 1,NELEM

!        pcmax(ielem) = SMALL_PC

        do ibin = 1,NBIN

!          do ixyz = 1,NXYZ

          do ix = 1, NX
          do iy = 1, NY
          do iz = 1, NZ

            pcl(ix,iy,iz,ibin,ielem) = pc(ix,iy,iz,ibin,ielem)

!                pcmax(ielem) = max( pcmax(ielem), pc(ix,iy,iz,ibin,ielem) )

          enddo
          enddo
          enddo

        enddo
      enddo

!  Find maximum particle concentration for each spatial grid box
!  (in units of cm^-3)

      do igrp = 1,NGROUP
        iep = ienconc(igrp)

!        do ixyz = 1,NXYZ
!          pconmax(ixyz,igroup) = SMALL_PC

         do ix = 1, NX
         do iy = 1, NY
         do iz = 1, NZ

           pconmax(ix,iy,iz,igrp) = SMALL_PC
           do ibin = 1,NBIN

!            pconmax(ixyz,igroup) = max( pconmax(ixyz,igroup),
!     $                                  pc3(ixyz,i,iep) )

             pconmax(ix,iy,iz,igrp) = max( pconmax(ix,iy,iz,igrp) &
                                         , pc(ix,iy,iz,ibin,iep) )
           enddo

!          pconmax(ixyz,igroup) = pconmax(ixyz,igroup) /
!     $         ( xmet3(ixyz)*ymet3(ixyz)*zmet3(ixyz) )

           pconmax(ix,iy,iz,igrp) = pconmax(ix,iy,iz,igrp) &
                                    / xmet(ix,iy,iz)       &
                                    / ymet(ix,iy,iz)       &
                                    / zmet(ix,iy,iz)
        enddo  ! iz
        enddo  ! iy
        enddo  ! ix
      enddo  ! igrp


#undef gas
#ifdef gas

!  Set <gcl> to <gc> from previous time step
!  and store old ice and liquid supersaturations for variable time stepping

      do igas = 1,NGAS
        do ixyz = 1,NXYZ
          gcl(ixyz,igas) = gc3(ixyz,igas)
          supsatlold(ixyz,igas) = supsatl3(ixyz,igas)
          supsatiold(ixyz,igas) = supsati3(ixyz,igas)
        enddo
      enddo

! gas
#endif

#undef varstep
#ifdef varstep

!  Store old values of atmospheric properties for variable time stepping.

      do ixyz = 1,NXYZ
        zmetl3(ixyz) = zmet3(ixyz)

!      if( do_varstep )then

        do ixyz = 1,NXYZ
          ptcl(ixyz) = ptc3(ixyz)
          told(ixyz) = t3(ixyz)
          pold(ixyz) = p3(ixyz)
          rhoaold(ixyz) = rhoa3(ixyz)
        enddo

        if( do_parcel )then
          do ixyz = 1,NXYZP1
            zlold(ixyz) = zl3(ixyz)
          enddo
          do ixyz = 1,NXYZ
            zcold(ixyz) = zc3(ixyz)
          enddo
        endif

!      endif

! varstep
#endif

!  Set production terms and loss rates due to slow microphysics
!  processes (coagulation) to zero.
!
      if( ibtime .eq. 0 .or. do_coag )then

        coagpe(1:NX,1:NY,1:NZ,1:NBIN,1:NELEM) = 0._f
        coaglg(1:NX,1:NY,1:NZ,1:NBIN,1:NGROUP) = 0._f

      endif  ! ibtime .eq. 0 .or. do_coag

#undef microfast
#ifdef microfast

!  Set production terms and loss rates due to fast microphysics
!  processes to zero if just starting a simulation.

      if( ibtime .eq. 0 )then
        call zeromicro
      endif

! microfast
#endif

#undef w_hack
#ifdef w_hack

!  Adjust vertical wind speed profile (hack for test simulation)
!
      z_peak = 5.5e5 + 5.e2*time
      do k=3,NZ
        do ix=1,NX
          do iy=1,NY
            w(ix,iy,k) = 5.e2 *
     $                   exp( -1.*(zc(ix,iy,k)-z_peak)**2 /
     $                                 1.e5**2 )
          enddo
        enddo
      enddo

! w_hack
#endif

!  Return to caller with preliminary timestep things completed.

rc = 0
return
end
