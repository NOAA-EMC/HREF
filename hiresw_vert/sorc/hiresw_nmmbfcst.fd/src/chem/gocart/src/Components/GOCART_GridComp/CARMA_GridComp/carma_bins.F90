! Colarco, Aug. 21, 2007
! To make the carma size binning more uniform throughout the code/SLOD
! I extract the size binning routine and place here.

      subroutine carma_bins( NBIN, NGROUP, rhop, &
                             rmin, rmrat, rmassmin, &
                             r, dr, rmass, rmassup, rlow, rup, &
                             dm, vol, rc )

!     types
      use carma_types_mod

      implicit none

!     Input
      integer :: NBIN, NGROUP
      real(kind=f), dimension(NBIN,NGROUP) :: rhop

!     I/O
      real(kind=f), dimension(NGROUP) :: rmin, rmrat, rmassmin
      real(kind=f), dimension(NBIN,NGROUP) :: r, dr, &
                                              rmass, rmassup, &
                                              rlow, rup, dm, vol

!     Output
      integer :: rc

!     Local
      integer :: igrp, j
      real(kind=f) :: vrfact, cpi

      rc = 0

#ifdef DEBUG
       write(*,*) '+ carma_bins'
#endif

!  Set up the particle bins.
!  The following definition applies really to bins set up the CARMA way,
!  i.e., specify rmin and rmrat.
!  For each particle group, the mass of a particle in
!  bin j is <rmrat> times that in bin j-1
!
!    rmass(NBIN,NGROUP)     =  bin center mass [kg]
!    r(NBIN,NGROUP)         =  bin mean (volume-weighted) radius [m]
!    vol(NBIN,NGROUP)       =  bin center volume [m^3]
!    dr(NBIN,NGROUP)        =  bin width in radius space [m]
!    dv(NBIN,NGROUP)        =  bin width in volume space [m^3]
!    dm(NBIN,NGROUP)        =  bin width in mass space [kg]
!
      cpi = 4._f/3._f*PI

!     We have two methods for constructing the particle size bins
!     If rmin provided is negative then must provide valid r, rlow, rup
!     If r provided is negative then must provide valid rmin, rmrat
!     We only check here to see if both are negative.
      if(rmin(NGROUP) .lt. 0._f .and. r(NBIN,NGROUP) .lt. 0._f) then
       rc = 1
       return
      endif

      if(r(NBIN,NGROUP) .lt. 0._f) then

       do igrp = 1, NGROUP

        rmassmin(igrp) = cpi*rhop(1,igrp)*rmin(igrp)**3

        vrfact = ( ( 3._f / 2._f / PI / ( rmrat(igrp) + 1._f ) ) &
                   ** ( 1._f / 3._f ) ) & 
                 * ( rmrat(igrp) ** ( 1._f / 3._f ) - 1._f )

        do j = 1, NBIN

          rmass(j,igrp)   = rmassmin(igrp) * rmrat(igrp)**(j-1)
          rmassup(j,igrp) = 2._f*rmrat(igrp)/(rmrat(igrp)+1._f)* &
                            rmass(j,igrp)
          dm(j,igrp)      = 2._f*(rmrat(igrp)-1._f)/(rmrat(igrp)+1._f)* &
                            rmass(j,igrp)
          vol(j,igrp) = rmass(j,igrp) / rhop(1,igrp)
          r(j,igrp)   = ( rmass(j,igrp)/rhop(1,igrp)/cpi )**(1._f/3._f)
          rup(j,igrp) = ( rmassup(j,igrp)/rhop(1,igrp)/cpi )**  &
                                                                (1._f/3._f)
          dr(j,igrp)  = vrfact*(rmass(j,igrp)/rhop(1,igrp))**(1._f/3._f)
          rlow(j,igrp) = rup(j,igrp) - dr(j,igrp)

        enddo
       enddo

      else   ! supplied the radius, rlow, rup

       rmin(:) = r(1,:)
       if(NBIN .gt. 1) then
        rmrat(:) = (r(2,:)/r(1,:)) ** 3
       else
        rmrat(:) = 2._f
       endif
       dr(:,:) = rup(:,:) - rlow(:,:)
       rmassmin(:) = cpi*rhop(1,:)*r(1,:)**3
       rmass(:,:) = cpi*rhop(:,:)*r(:,:)**3
       vol(:,:) = rmass(:,:) / rhop(:,:)
       rmassup(:,:) = cpi*rhop(:,:)*rup(:,:)**3
       dm(:,:) = rmassup(:,:) - cpi*rhop(:,:)*rlow(:,:)**3
      endif

      return
      end subroutine carma_bins
