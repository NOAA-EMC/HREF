! Colarco
! SLOD: Stupid little offline driver
!
! This is an example driver to call the CARMA code
! Note that here we mimic as if calling from GEOS-4/5
!
! The host model needs to provide temperature, pressure, grid, particle,
! and gas distributions.  Additionally, we assume that logical flags
! relevant to CARMA (e.g., do_coag) come from a resource file.  We mimic that
! here by setting all of these explicitly.
!
! As the CARMA code is currently set up, it defaults to a lat/lon
! and hybrid eta style coordinate grid.
! Requires at least 2 latitudes for a sensible latitude creation (jm >= 2)
! Specify im, jm, km for spatial grid
! Specify nbin for number of bins: default is setup for 5 GOCART style dust
! bins with r, rhop for radius (m) and rhop (kg m-3) appropriate.
! The w_c chem_bundle is to follow the GEOS-4/5 style chem bundle.  You can
! see below what I specify for this.  You need to provide an (im,jm,km) array
! of:
!  delp (pressure thickness of level in Pa)
!  t (temperature in K)
!  rhoa (air density in kg m-3)
! You also need to provide ptop

! ABOVE NEEDS A MUCH BETTER COMMENTARY

      program SLOD

      use SLOD_ChemMod

      implicit none

!     Host model inputs
!     The host model needs to provide information about the spatial grid,
!     temperature and pressure profiles, and particle/gas concentrations
!     Additionally, the host model selects the precision of real numbers

!     Constants
!     ---------
!     These are used only at the SLOD level to help set up the problem.
!     Check carma_constants to ensure consistency with what CARMA will do internally
      real,    parameter :: grav = 9.806_f, &    ! acceleration of gravity
                            rgas =  8.31430_f, & ! universal gas constant [J / K / mole ]
                            pi = 3.1415926535_f  ! ratio of circumference 
                                                 ! to diameter of a circle
!     Spatial dimension
!     -----------------
!     Specify the number of x, y, and z points in your problem
      integer, parameter :: im = 1,  &         ! x-points
                            jm = 1,  &         ! y-points
                            km = 32            ! z-points

!     Host-model provided tracer/atmospheric structure
!     ------------------------------------------------
!     The following are mandatory:
!      o defined spatial grid              w_c%grid%lon, w_c%grid%lat
!      o temperature (3-D)                 t
!      o air density                       rhoa
!      o surface temperature               t_surf
!      o surface pressure                  p_surf
!      o pressure thickness of levels      w_c%delp
!      o relative humidity (scaled 0 - 1)  w_c%rh
!      o tracers                           w_c%qa
!     In our current application, much of the host model structure
!     including tracers, grid, delp, rh, etc., are contained in the
!     so-called chem bundle, from GEOS-4.  In the future this would
!     be relaxed so that the coupler to CARMA would not receive such
!     a specialized structure.
      type(Chem_Bundle)  :: w_c           ! GEOS-4/5 structure of grid/tracer
      real(kind=f), pointer, dimension(:,:,:) :: t=>null(), &    ! temperature
                                                 rhoa=>null()    ! air density
      real(kind=f), pointer, dimension(:,:)   :: t_surf=>null(), & ! surface temperature
                                                 p_surf=>null()   ! surface pressure

!     Host-model provided particle/gas dimensions
!     -------------------------------------------
      integer :: NGROUP = 1
      integer :: NELEM = 1
      integer :: NBIN = 20
      integer, parameter :: NGAS = 0

!     NBIN, NGROUP particle size bin information
!     Some care should be taken to ensure consistency between the particle
!     bins assigned here and what is used internally in CARMA.  Ideally
!     You would specify (rhop, r, rlow, and rup) or (rhop, rmin, rmrat)
!     to CARMA and it would derive everything else.  However, for i/o
!     purposes at this level it is useful to have all that information (see
!     the COAGTEST, for example).  So we provide all the particle grid
!     information needed here.
      real(kind=f), pointer, dimension(:,:) :: rhop=>null(), &
                                               r=>null(), &
                                               dr=>null(), &
                                               rmass=>null(), &
                                               rmassup=>null(), &
                                               rlow=>null(), &
                                               rup=>null(), &
                                               dm=>null(), &
                                               vol=>null()
      real(kind=f), pointer, dimension(:)   :: rmin=>null(), &
                                               rmrat=>null(), &
                                               rmassmin=>null()

!     Timestep size of the computation
      real(kind=f) :: dtime, &   ! time step size [s]
                      endtime    ! ending time [s]
      integer      :: ntime      ! number of steps to iterate for

!     ========================================================================================
!     Local declarations
!     Some sample meteorology is provided on the 32-level grid.  Here we
!     provide the GEOS-like eta coordinate a & b values and a data statement
!     construction of temperature, air density, and surface pressure and
!     temperature.
      real(kind=f), dimension(33)   :: a32, b32           ! hybrid-coordinate definition
      real(kind=f), dimension(73)   :: a72, b72           ! GEOS-5 hybird-coordinate definition
      real(kind=f), dimension(32)   :: t32, &             ! sample temperature profile
                                       rhoa32             ! sample density profile
      real(kind=f), parameter       :: ps = 101346., &    ! default surface pressure [Pa]
                                       ptop = 1., &       ! default top lid pressure [Pa]
                                       ts = 298.          ! default surface temperature [K]

      integer :: i1, i2, j1, j2
      integer, parameter :: lun = 42  ! output file unit number
      integer :: nbeg, nend
      integer :: ibun   ! index within w_c referring to particular ibin, ielem pair
      integer :: i, j, k, n, ibin, ielem, igrp, ier, ib
      integer :: rc
!     Parameter to control particle fall: ifall = 0 use a fixed fall speed
!                                         ifall = 1 calculate fall speed
      integer :: ifall  = 1
!     Parameter to control swelling of particles in fall speed (seasalt only)
!     RHflag = 0 do nothing, = 1 use fitzgerald scheme, = 2 use Gerber scheme
      integer :: RHflag = 0
      real(kind=f) :: z, dz, zmid, p     

!     Diagnostics
      type(Chem_Array), pointer, dimension(:,:) :: fluxout=>null()
      type(Chem_Array), pointer, dimension(:)   :: SLODfluxout=>null()
      real(kind=f), pointer, dimension(:,:)     :: fout=>null()
      real(kind=f), pointer, dimension(:,:,:,:) :: cm0=>null(), &
                                                   cm1=>null()
      real(kind=f), pointer, dimension(:)       :: initq=>null()
      real(kind=f) :: initsum, finalsum, tfout, tt

!     Include the SLOD_Coupler interface and the various data statements
#include "SLOD.h"

!     ========================================================================================

!     Begin the SLOD!  All fear the SLOD!
!     -----------------------------------

!     Local horizontal grid extent
      i1 = 1      ;      i2 = im
      j1 = 1      ;      j2 = jm

!     Setup default particle mapping arrays
      NGROUP = 1
      NELEM  = 1
      NBIN   = 20
      call SLOD_allocateparticles_()

      igrp = NGROUP
      rhop(:,:) = 2000._f
      r(:,:) = -1._f
      rmin(igrp) = 3.e-9_f
      rmrat(igrp) = 2._f
      call carma_bins(       NBIN, NGROUP, rhop, &
                             rmin, rmrat, rmassmin, &
                             r, dr, rmass, rmassup, rlow, rup, &
                             dm, vol, rc )

!     Allocate memory for dynamic arrays
!     The GEOS-4 Chem Bundle
      allocate(w_c%grid%lon(i1:i2), w_c%grid%lat(j1:j2), &
               w_c%delp(i1:i2,j1:j2,km), &
               w_c%rh(i1:i2,j1:j2,km), &
               stat = ier )
      if( ier /= 0 )then
       write(*,*) 'SLOD.F90: allocate error 1'
       stop
      endif

      allocate( t( i1:i2, j1:j2, km ), rhoa( i1:i2, j1:j2, km) &
              , stat = ier )
      if( ier /= 0 )then
       write(*,*) 'SLOD.F90: allocate error 2'
       stop
      endif
      allocate( t_surf(i1:i2,j1:j2), p_surf(i1:i2,j1:j2) &
              , stat = ier )
      if( ier /= 0 )then
       write(*,*) 'SLOD.F90: allocate error 3'
       stop
      endif

!     Set up the grid following the variables above
      call SLOD_initatm_()


!     Time and timesteps to iterate for
      dtime = 1800._f
      endtime = dtime
      ntime = 1

!     Set-up special test cases ( may overwrite the particle definition above)
!     ------------------------------------------------------------------------
#if defined( FALLTEST ) || defined( SSFALLTEST )
      call SLOD_initfalltest_()
#endif
#ifdef COAGTEST
      call SLOD_initcoagtest_()
#endif
#ifdef BCOC
      call SLOD_initBCOC_()
#endif
#ifdef CLDICE
      call SLOD_initCLDICE_()
#endif


!     Allocate the diagnostic arrays
!     ------------------------------------------------------------------------
      allocate( cm0(i1:i2,j1:j2,nbin,NELEM) &
              , cm1(i1:i2,j1:j2,nbin,NELEM) &
              , fluxout(NBIN,NELEM) &
              , SLODfluxout(NBIN*NELEM) &
              , fout(NBIN,NELEM) &
              , initq(NBIN*NELEM) &
              , stat = ier )
      if( ier /= 0 )then
       write(*,*) 'SLOD.F90: allocate error 5'
       stop
      endif

      do ielem = 1, NELEM
        do ibin = 1, NBIN
          n = nbeg + ( ielem - 1 ) * NBIN + ibin - 1
          allocate( fluxout(ibin,ielem)%data2d(i1:i2,j1:j2), &
                    SLODfluxout(n)%data2d(i1:i2,j1:j2), stat = ier )
          if( ier /= 0 )then
            write(*,*) 'SLOD.F90: allocate error fluxout' &
                       , ibin, ielem
            stop
          endif
        enddo
      enddo

!     Save the initial concentrations of a column
      do ibin = nbeg, nend
       initq(ibin) = w_c%qa(ibin)%data3d(i2,j2,km)
      end do

!     Initial column burdens and initialize flux diagnostics
      cm0(:,:,:,:) = 0._f
      do k = 1, km
        do ielem = 1, NELEM
          do ibin = 1, nbin
            ibun = nbeg+(ielem-1)*nbin+ibin-1
            cm0(:,:,ibin,ielem) = cm0(:,:,ibin,ielem) + w_c%delp(:,:,k) &
                     * w_c%qa(ibun)%data3d(:,:,k) &
                     / grav
          enddo
        enddo
      enddo

      do ielem = 1, NELEM
        do ibin = 1, nbin
          ibun = nbeg + ( ielem - 1 ) * NBIN + ibin - 1
          fluxout(ibin,ielem)%data2d = 0._f
          SLODfluxout(ibun)%data2d = 0._f
        enddo
      enddo
      fout(:,:) = 0._f
      tfout = 0._f

! ------------------------------------------------------------------------
!     Do some number of CARMA steps
      do n = 1, ntime
        tt = w_c%qa(nbeg+nbin)%data3d(i2,j2,km)
        tt = rhoa(i2,j2,km)
        call SLOD_Coupler( i1, i2, j1, j2, km, nbeg, nend   & ! subdom 
                                 , NGROUP, NELEM, NBIN, NGAS        &
                                 , w_c = w_c, dtime = dtime &
                                 , t = t, rhoa = rhoa &
                                 , t_sfc = t_surf, p_surf = p_surf  &
                                 , ifall = ifall, rhflag = rhflag   &
                                 , rmin = rmin, rmrat = rmrat       &
! alternatively, pass these
!                                 , r = r, rlow = r, rup = r    &
                                 , rhop = rhop &
                                 , fluxout = SLODfluxout &
                                 , rc = rc                  )
        print *, n, tt, rhoa(i2,j2,km)

        if(rc /= 0) then
         write(*,*) 'SLOD: SLOD_Coupler rc = ', rc
         stop
        endif
        do ielem = 1, NELEM
          do ibin = 1, nbin
            ibun = nbeg + ( ielem - 1 ) * NBIN + ibin - 1
            fout(ibin,ielem) = fout(ibin,ielem) &
                               + SLODfluxout(ibun)%data2d(i2,j2)
          end do
        end do


#if defined( FALLTEST) || defined (SSFALLTEST)
       write(lun,*) n*dtime, km
       z = 0._f
       do k = km, 1, -1
          dz = w_c%delp(i2,j2,k) / rhoa(i2,j2,k) /  grav
          z = z + dz
          zmid = z - dz / 2._f
          write(lun,*) k, zmid, dz, rhoa(i2,j2,k), &
                      w_c%qa(15)%data3d(i2,j2,k) * rhoa(i2,j2,k)
       enddo
#endif


#if defined(COAGTEST) || defined(BCOC) || defined(CLDICE)
       igrp = 1
       ielem = 1

       write(lun,*) n * dtime, NBIN, NELEM

       do ielem = 1, NELEM
       do ibin = 1, NBIN
         ibun = nbeg + ( ielem - 1 ) * NBIN +ibin - 1
         write(lun,*) ibin, r(ibin,igrp), rmass(ibin,igrp), &
                    w_c%qa(ibun)%data3d(i2,j2,km) * rhoa(i2,j2,km) / rmass(ibin,igrp), &
                    w_c%qa(ibun)%data3d(i2,j2,km) * rhoa(i2,j2,km) / rmass(ibin,igrp) &
                    / ( 1._f / log( 10._f ) * dr(ibin,igrp) / r(ibin,igrp) )
       enddo
       enddo
#endif
! ifdef COAGTEST

      end do  ! n
! Finishing the CARMA steps
! ------------------------------------------------------------------------

#if defined( FALLTEST) || defined(SSFALLTEST) || defined(COAGTEST) || defined(BCOC) || defined(CLDICE)
      close(lun)
#endif

!--

!       Total up the fluxes to get an integrated mass flux
        fout = fout*dtime
        do ielem = 1, NELEM
          do ibin = 1, nbin
            tfout = tfout + fout(ibin,ielem)
          end do
        end do


!--


!     Finalize diagnostics
      cm1(:,:,:,:) = 0._f
      do k = 1, km
        do ielem = 1, NELEM
          do ibin = 1, NBIN
            cm1(:,:,ibin,ielem) = cm1(:,:,ibin,ielem) + w_c%delp(:,:,k) & 
                         * w_c%qa(nbeg+(ielem-1)*nbin+ibin-1)%data3d(:,:,k) &
                         / grav
          end do
        end do
      end do

      initsum = 0._f
      finalsum = 0._f
      do ielem = 1, NELEM
        do ibin = 1, NBIN
          initsum = initsum + cm0(i2,j2,ibin,ielem)
          finalsum = finalsum + cm1(i2,j2,ibin,ielem)
        enddo 
      enddo 
      write(*,*) 'SLOD: init and final column mass (kg/m**2)'
      write(*,*) ' ', initsum, finalsum
      write(*,*) 'SLOD: final - init, SLODfluxout (kg/m**2)'
      write(*,*) ' ', finalsum-initsum, tfout


      k = km
      ielem = 1
      write(*,*) 'SLOD: initial and final mixing ratios'
      write(*,*) ' for all bins of elem 1'
      do ibin = 1, NBIN
        ibun = nbeg + ( ielem - 1 ) * NBIN + ibin - 1
        write(*,'( i4, 1p, 2(e15.6) )') ibin, initq(ibin) &
         , w_c%qa( ibun )%data3d(i2,j2,k)
      enddo
 
!     Deallocate arrays
      call SLOD_deallocateparticles_()

      deallocate( w_c%grid%lon, w_c%grid%lat &
                , w_c%delp &
                , stat = ier )
      if( ier /= 0 )then
        write(*,*) 'SLOD.F90: deallocate error 1'
        stop
      endif

      deallocate( t, rhoa &
                , stat = ier )
      if( ier /= 0 )then
        write(*,*) 'SLOD.F90: deallocate error 2'
        stop
      endif

      deallocate( t_surf, p_surf &
                , stat = ier )
      if( ier /= 0 )then
        write(*,*) 'SLOD.F90: deallocate error 3'
        stop
      endif

      do ielem = 1, NELEM
        do ibin = 1, NBIN
          n = nbeg + ( ielem - 1 ) * NBIN + ibin - 1
          deallocate( fluxout(ibin,ielem)%data2d, SLODfluxout(n)%data2d &
                    , stat = ier )
          if( ier /= 0 )then
            write(*,*) 'SLOD.F90: deallocate error'
            write(*,*) ' fluxout(ielem,ibin)', ielem, ibin
            stop
          endif
        enddo
      enddo

      deallocate( cm0, cm1, fluxout, fout, initq &
                , stat = ier )
      if( ier /= 0 )then
        write(*,*) 'SLOD.F90: deallocate error 5'
        stop
      endif

write(*,*) 'SLOD: finished'


CONTAINS

!     =======================================================================
      subroutine SLOD_initatm_()

!     The grid structure below follows from GEOS-4.  The exception we make
!     is that in case jm = 1 (which we choose for computational efficiency
!     in our test problems) the lat_del would break, so we hardwire in
!     that case.

!     longitude grid
      w_c%grid%lon_min = 0._f
      w_c%grid%lon_del = 360._f / im
      w_c%grid%lon_max = w_c%grid%lon_min + (im-1) * w_c%grid%lon_del
      do i = i1, i2
       w_c%grid%lon(i) =  w_c%grid%lon_min + (i-1) * w_c%grid%lon_del
      end do

!     latitude grid
      if(jm .eq. 1) then
       w_c%grid%lat_min =  0._f
       w_c%grid%lat_max =  2._f
       w_c%grid%lat_del =  2._f
       w_c%grid%lat(jm) =  0._f
      else
       w_c%grid%lat_min = -90._f
       w_c%grid%lat_max =  90._f
       w_c%grid%lat_del = ( w_c%grid%lat_max - w_c%grid%lat_min ) / ( jm-1) 
       do j = j1, j2
        w_c%grid%lat(j) = w_c%grid%lat_min + (j-1) * w_c%grid%lat_del
       end do
      endif

!     vertical grid -- We have two special cases (k = 32, k = 72)
!     for which we have specified a & b coefficients, 
!     else we assume a uniform delp
      if( km  .eq. 32 ) then
          w_c%grid%ptop = a32(1)
          do k = 1, km
           w_c%delp(i1:i2,j1:j2,k) = (a32(k+1)-a32(k)) + ps*(b32(k+1)-b32(k))
          end do
      elseif( km .eq. 72 ) then
          w_c%grid%ptop = a72(1)
          do k = 1, km
           w_c%delp(i1:i2,j1:j2,k) = (a72(k+1)-a72(k)) + ps*(b72(k+1)-b72(k))
          end do
      else
           w_c%grid%ptop = ptop
           w_c%delp(:,:,:) = (ps - w_c%grid%ptop) / km
      endif

!     Relative humidity values between 0 - 1; set to 80%
      w_c%rh(:,:,:) = 0.8_f

!     Temperature, pressure, and air density profiles
!     For special case of km = 32 we provide temperature and air density,
!     else we assume an isothermal profile
      t_surf = ts
      p_surf = ps
      if(km .eq. 32) then
       do j = j1, j2
        do i = i1, i2
         rhoa(i,j,:) = rhoa32(:)
         t(i,j,:) = t32(:)
        end do
       end do
      else
       t = ts
       p = ps
       do k = km, 1, -1
        p = p - w_c%delp(i2,j2,k)/2._f  ! pressure at halfway point of layer
        rhoa(i1:i2,j1:j2,k) = p / rgas / t(i1:i2,j1:j2,k)
        p = p - w_c%delp(i2,j2,k)/2._f  ! pressure at top of layer
       enddo
      endif

      return
      end subroutine SLOD_initatm_

!     =======================================================================

      subroutine SLOD_initfalltest_

#ifdef SSFALLTEST
      rhFlag = 2     ! 1 for Fitzgerald, 2 for Gerber
#endif


! If doing the FALLTEST then set up that problem.  Generates
! an output file 'falltest.txt'

      call SLOD_allocateparticles_()

      igrp = NGROUP
      rhop(:,:) = 2000._f
      r(:,:) = -1._f
      rmin(igrp) = 3.e-9_f
      rmrat(igrp) = 2._f
      call carma_bins(       NBIN, NGROUP, rhop, &
                             rmin, rmrat, rmassmin, &
                             r, dr, rmass, rmassup, rlow, rup, &
                             dm, vol, rc )

!     A distribution varying in height
      z = 0._f
      do k = km, 1, -1
        dz = w_c%delp(i2,j2,k) / rhoa(i2,j2,k) / grav 
        z = z + dz
        zmid = z - dz / 2._f
        do i = nbeg, nend
         w_c%qa(i)%data3d(:,:,k) = &
           1.934380e-10_f * exp( - ( ( zmid - 2.e3_f ) / 2.e3_f ) ** 2 ) &
           / rhoa(:,:,k)
        enddo
      enddo

      open(unit=lun,file='falltest.txt',status='unknown')
      write(lun,*) 0., km
      z = 0._f
      do k = km, 1, -1
         dz = w_c%delp(i2,j2,k) / rhoa(i2,j2,k) /  grav
         z = z + dz
         zmid = z - dz / 2._f
         write(lun,*) k, zmid, dz, rhoa(i2,j2,k), &
                     w_c%qa(15)%data3d(i2,j2,k) * rhoa(i2,j2,k)
      enddo

      endtime = 5._f * 86400._f
      ntime = int(endtime / dtime + 0.5)

      end subroutine SLOD_initfalltest_

!     =======================================================================

      subroutine SLOD_initcoagtest_
!     If this is selected we specify an initially mono-disperse
!     particle size distribution in grid box i2,j2,km
!     Generates an output file 'coagtest.txt'
      NELEM = 1
      NGROUP = 1
      NBIN = 20
      call SLOD_allocateparticles_()

      igrp = NGROUP
      rhop(:,:) = 2000._f
      r(:,:) = -1._f
      rmin(:) = 3.e-9_f
      rmrat(:) = 2._f
      call carma_bins(       NBIN, NGROUP, rhop, &
                             rmin, rmrat, rmassmin, &
                             r, dr, rmass, rmassup, rlow, rup, &
                             dm, vol, rc )

      dtime = 600._f
      endtime = 12._f * 3600._f    ! 12 hr test sim
      ntime = int( endtime / dtime + 0.5 )

      if ( NGROUP /= 1 ) then
        write(*,*) 'SLOD: COAGTEST error.  Fix NGROUP'
        stop
      endif

      if ( NELEM /= 1 ) then
        write(*,*) 'SLOD: COAGTEST error.  Fix NELEM'
        stop
      endif

      if ( dtime /= 600._f ) then
        write(*,*) 'SLOD: COAGTEST error.  Fix dtime'
        stop
      endif

!     Initial particle size distribution (monodisperse)
!     10 ** 12 m ** -3 for 1st bin in 1 grid box
      w_c%qa(nbeg)%data3d(i2,j2,km) = 1.934380e-10_f

      open( unit = lun, file = 'coagtest.txt', status = 'unknown' )
      n = 0
      igrp = 1
      ielem = 1

      write(lun,*) n * dtime, NBIN, NELEM

      do ielem = 1, NELEM

      do ibin = 1, NBIN
        ibun = nbeg + ( ielem - 1 ) * NBIN +ibin - 1
        write(lun,*) ibin, r(ibin,igrp), rmass(ibin,igrp), &
                   w_c%qa(ibun)%data3d(i2,j2,km) * rhoa(i2,j2,km) / rmass(ibin,igrp), &
                   w_c%qa(ibun)%data3d(i2,j2,km) * rhoa(i2,j2,km) / rmass(ibin,igrp) &
                   / ( 1._f / log( 10._f ) * dr(ibin,igrp) / r(ibin,igrp) )

      enddo

      enddo

      end subroutine SLOD_initcoagtest_

!     =======================================================================

      subroutine SLOD_initBCOC_
!     Here we set up a problem for coagulating BC/OC particles
      NELEM = 4
      NGROUP = 3
      NBIN = 20

      call SLOD_allocateparticles_()
!     Initial particle mass mixing ratio...very small value for all bins
      do i = nbeg, nend
        w_c%qa(i)%data3d = 1.e-40_f
      end do

      igrp = NGROUP
      rhop(:,1) = 1000._f
!!      rhop(:,2) = 1800._f
!!      rhop(:,3) = 1800._f
      rhop(:,2) = 1000._f
      rhop(:,3) = 1000._f
      r(:,:) = -1._f
      rmin(:) = 3.e-9_f
      rmrat(:) = 2._f
      call carma_bins(       NBIN, NGROUP, rhop, &
                             rmin, rmrat, rmassmin, &
                             r, dr, rmass, rmassup, rlow, rup, &
                             dm, vol, rc )

      dtime = 600._f
      endtime = 12._f * 3600._f    ! 12 hr test sim
      ntime = int( endtime / dtime + 0.5 )

      if ( dtime /= 600._f ) then
        write(*,*) 'SLOD: COAGTEST error.  Fix dtime'
        stop
      endif

!     Initial particle size distribution (monodisperse)
!     10 ** 12 m ** -3 for 1st bin in 1 grid box
      w_c%qa(nbeg)%data3d(i2,j2,km) = 1.934380e-10_f
      w_c%qa(nbeg+nbin)%data3d(i2,j2,km) = 1.934380e-10_f

      open( unit = lun, file = 'bcoctest.txt', status = 'unknown' )
      n = 0
      igrp = 1
      ielem = 1

      write(lun,*) n * dtime, NBIN, NELEM

      do ielem = 1, NELEM

      do ibin = 1, NBIN
        ibun = nbeg + ( ielem - 1 ) * NBIN +ibin - 1
        write(lun,*) ibin, r(ibin,igrp), rmass(ibin,igrp), &
                   w_c%qa(ibun)%data3d(i2,j2,km) * rhoa(i2,j2,km) / rmass(ibin,igrp), &
                   w_c%qa(ibun)%data3d(i2,j2,km) * rhoa(i2,j2,km) / rmass(ibin,igrp) &
                   / ( 1._f / log( 10._f ) * dr(ibin,igrp) / r(ibin,igrp) )
      enddo

      enddo

      end subroutine SLOD_initBCOC_

!     =======================================================================

      subroutine SLOD_initCLDICE_
!     Here we set up a problem for coagulating BC/OC particles
      NELEM = 4
      NGROUP = 2
      NBIN = 20

      call SLOD_allocateparticles_()
!     Initial particle mass mixing ratio...very small value for all bins
      do i = nbeg, nend
        w_c%qa(i)%data3d = 1.e-40_f
      end do

      igrp = NGROUP
      rhop(:,1) = 1000._f
      rhop(:,2) = 1000._f
      r(:,:) = -1._f
      rmin(:) = 3.e-9_f
      rmrat(:) = 2._f
      call carma_bins(       NBIN, NGROUP, rhop, &
                             rmin, rmrat, rmassmin, &
                             r, dr, rmass, rmassup, rlow, rup, &
                             dm, vol, rc )

      dtime = 600._f
      endtime = 12._f * 3600._f    ! 12 hr test sim
      ntime = int( endtime / dtime + 0.5 )

      if ( dtime /= 600._f ) then
        write(*,*) 'SLOD: COAGTEST error.  Fix dtime'
        stop
      endif

!     Initial particle size distribution (monodisperse)
!     10 ** 12 m ** -3 for 1st bin in 1 grid box
      w_c%qa(nbeg)%data3d(i2,j2,km) = 1.934380e-10_f
      w_c%qa(nbeg+nbin)%data3d(i2,j2,km) = 0.5*1.934380e-10_f*1.13e-22
      w_c%qa(nbeg+nbin+nbin)%data3d(i2,j2,km) = 1.934380e-11_f

      open( unit = lun, file = 'cldicetest.txt', status = 'unknown' )
      n = 0
      igrp = 1
      ielem = 1

      write(lun,*) n * dtime, NBIN, NELEM

      do ielem = 1, NELEM

      do ibin = 1, NBIN
        ibun = nbeg + ( ielem - 1 ) * NBIN +ibin - 1
        write(lun,*) ibin, r(ibin,igrp), rmass(ibin,igrp), &
                   w_c%qa(ibun)%data3d(i2,j2,km) * rhoa(i2,j2,km) / rmass(ibin,igrp), &
                   w_c%qa(ibun)%data3d(i2,j2,km) * rhoa(i2,j2,km) / rmass(ibin,igrp) &
                   / ( 1._f / log( 10._f ) * dr(ibin,igrp) / r(ibin,igrp) )
      enddo

      enddo

      end subroutine SLOD_initCLDICE_

!     =======================================================================

      subroutine SLOD_allocateparticles_

!     deallocate the particles if already allocated
      if(associated(w_c%qa)) then
       do i = nbeg, nend
        deallocate(w_c%qa(i)%data3d)
       enddo
       deallocate(w_c%qa)
      endif
      if(associated(r))       deallocate(r)
      if(associated(dr))      deallocate(dr)
      if(associated(rhop))    deallocate(rhop)
      if(associated(rmass))   deallocate(rmass)
      if(associated(rmassup)) deallocate(rmassup)
      if(associated(rlow))    deallocate(rlow)
      if(associated(rup))     deallocate(rup)
      if(associated(dm))      deallocate(dm)
      if(associated(vol))     deallocate(vol)
      if(associated(rmin))    deallocate(rmin)
      if(associated(rmrat))   deallocate(rmrat)
      if(associated(rmassmin))deallocate(rmassmin)

!     Allocate the particles
      allocate ( r(NBIN,NGROUP), dr(NBIN,NGROUP), &
                 rhop(NBIN,NGROUP), rmass(NBIN,NGROUP), &
                 rmassup(NBIN,NGROUP), rlow(NBIN,NGROUP), &
                 rup(NBIN,NGROUP), dm(NBIN,NGROUP), vol(NBIN,NGROUP), &
                 rmin(NGROUP), rmrat(NGROUP), rmassmin(NGROUP) )

!     Local particle spatial grid extent
!     nbeg and nend are useful for mapping between the 1-d vector of
!     tracers from the host model to the (NBIN,NELEM) mapping in CARMA
      nbeg = 1
      nend = nbeg + NBIN * NELEM - 1    ! JAS ignoring gases for now

      allocate( w_c%qa(nbeg:nend))
      do i = nbeg, nend
       allocate( w_c%qa(i)%data3d(i1:i2,j1:j2,km), stat=ier )
       if( ier /= 0 )then
        write(*,*) 'SLOD.F90: allocate error qa(i), i = ', i
        stop
       endif
      enddo

!     Initial particle mass mixing ratio...very small value for all bins
      do i = nbeg, nend
        w_c%qa(i)%data3d = 1.e-30_f
      end do
      
      end subroutine SLOD_allocateparticles_

!     =======================================================================

      subroutine SLOD_deallocateparticles_

!     deallocate the particles if already allocated
      if(associated(w_c%qa)) then
       do i = nbeg, nend
        deallocate(w_c%qa(i)%data3d)
       enddo
       deallocate(w_c%qa)
      endif
      if(associated(r))       deallocate(r)
      if(associated(dr))      deallocate(dr)
      if(associated(rhop))    deallocate(rhop)
      if(associated(rmass))   deallocate(rmass)
      if(associated(rmassup)) deallocate(rmassup)
      if(associated(rlow))    deallocate(rlow)
      if(associated(rup))     deallocate(rup)
      if(associated(dm))      deallocate(dm)
      if(associated(vol))     deallocate(vol)
      if(associated(rmin))    deallocate(rmin)
      if(associated(rmrat))   deallocate(rmrat)
      if(associated(rmassmin))deallocate(rmassmin)

      return
      end subroutine SLOD_deallocateparticles_

end program 
