! Colarco, Jan. 5, 2007
! sed command to replace F77 comments with F90: sed 's/^c/\!/g'
! F90-version of original CARMA setupvf.f routine
!
! NOTES:
!
       subroutine setupvf( carma, rc )


!     types
      use carma_types_mod

      implicit none
real(kind=f),allocatable :: scratch(:,:)
      integer, intent(out) :: rc

!     locals
      integer :: ix, iy, i, j, k, k1, k2, ibin, iz
      integer :: nzm1
      real(kind=f) :: rhoa_mks, vg, rmfp, rkn, expon, x, y, cdrag, xyzmet
      real(kind=f) :: a_mid_k1, a_mid_k2, zmet_k1, zmet_k2, zmet_k, frac

!     Locals for the RH correction
      real, parameter ::  rhow = 1000.  ! Density of water [kg m-3]
!     The following parameters relate to the swelling of seasalt like particles
!     following Fitzgerald, Journal of Applied Meteorology, 1975.
                                    ! soluble fraction of deliqeuscing particle
      real(kind=f), parameter :: epsilon_ = 1._f  
      real(kind=f), parameter :: alphaNaCl = 1.35_f
      real(kind=f) :: alpha, alpha1, alpharat, beta, theta, f1, f2

!     parameter from Gerber 1985 (units require radius in cm, see rcm)
      real(kind=f) :: rcm
      real(kind=f), parameter :: c1=0.7674_f, c2=3.079_f, &
                                 c3=2.573e-11_f, c4=-1.424_f
      real(kind=f) :: sat, rrat
      real(kind=f) :: rLocal, rhopLocal

#include "carma_globaer.h"

       rc = 0

#ifdef DEBUG
       write(*,*) '+ setupvf'
#endif

!
!
!  @(#) setupvf.f  Ackerman Nov-2000
!
!  This routine evaluates particle fall velocities, vf(k) [cm s^-1]
!  and reynolds' numbers based on fall velocities, re(j,i,k) [dimensionless].
!  indices correspond to vertical level <k>, bin index <i>, and aerosol
!  group <j>.
!
!  Method: first use Stokes flow (with Fuchs' size corrections, 
!  valid only for Stokes flow) to estimate fall velocity, then calculate
!  Reynolds' number (Re) (for spheres, Stokes drag coefficient is 24/Re).
!  Then for Re > 1, correct drag coefficient (Cd) for turbulent boundary
!  layer through standard trick to solving the drag problem: 
!  fit y = log( Re ) as a function of x = log( Cd Re^2 ).  
!  We use the data for rigid spheres taken from Figure 10-6 of
!  Pruppacher and Klett (1978):
!
!   Re     Cd
!  -----  ------
!     1    24
!    10     4.3
!   100     1.1
!  1000     0.45
!
!  Note that we ignore the "drag crisis" at Re > 200,000
!  (as discussed on p. 341 and shown in Fig 10-36 of P&K 1978), where
!  Cd drops dramatically to 0.2 for smooth, rigid spheres, and instead 
!  assume Cd = 0.45 for Re > 1,000
!
!  Note that we also ignore hydrodynamic deformation of liquid droplets
!  as well as any breakup due to Rayleigh-Taylor instability.  
!
!  This routine requires that vertical profiles of temperature <t>,
!  air density <rhoa>, and viscosity <rmu> are defined (i.e., initatm.f
!  must be called before this).  The vertical profile with ix = iy = 1
!  is used.
!
!  We assume spherical particles -- call setupvf_old() to use legacy
!  code from old Toon model for non-spherical effects -- use (better
!  yet, fix) at own risk.
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
!  Define formats
!
    1 format(/,'Non-spherical particles specified for group ',i3, &
        ' (ishape=',i3,') but spheres assumed in setupvf.f.',  &
        ' Suggest using/fixing non-spherical code in setupvf_old.f.')
    2 format(/,'Fall velocities and Reynolds'' number in bottom layer', &
        ' (setupvf): ')
    3 format(/,'Particle group ',i3,/,' bin   r [m]        vf [m/s]', &
        '       re'/)
    4 format(i3,3(1pe11.3,4x)) 
    5 format(/,'Particle group ',i3,/,' bin   r [m]        rWet [m]', &
        '     vf [m/s]       re'/)
    6 format(i3,4(1pe11.3,4x)) 
!
!-------------------------------------------------------------------------------
!
!  Announce entry to this routine
!
!      if( DEBUG ) write(LUNOPRT,'(/,a)') 'Enter setupvf'
! 
!-------------------------------------------------------------------------------
!  Loop over horizontal dimension
   do iy = 1, NY
    do ix = 1, NX

      rmu => carma%rmu(ix,iy)%data1d
      bpm => carma%bpm(ix,iy)%data3d
      vf => carma%vf(ix,iy)%data3d
      re => carma%re(ix,iy)%data3d


!
!  Loop over aerosol groups.
!
      do j = 1,NGROUP
!
!  Warning message for non-spherical particles
!
        if( ishape(j) .ne. 1 )then
          write(*,1) j, ishape(j)
        endif

        do k = 1,NZ

          xyzmet = xmet(ix,iy,k)*ymet(ix,iy,k)*zmet(ix,iy,k)
!
!  This is <rhoa> in cartesian coordinates (good old mks units)
!
          rhoa_mks = rhoa(ix,iy,k) / xyzmet
!
!  <vg> is mean thermal velocity of air molecules [m/s]
!
          vg = sqrt(8._f/PI * R_AIR*t(ix,iy,k))
!
!  <rmfp> is mean free path of air molecules [m]
!
! JAS, ref N.A. Fuchs, The mechanics of aerosols, 1964, p. 22.
!  Eqn. 6.2) rmu = 0.499 * air dens * mean vel * mean free path
!  So, rmfp ~ 2 * rmu / rhoa / vg

          rmfp = 2._f*rmu(k) / (rhoa_mks*vg)


!  Loop over particle size bins.

          do i = 1,NBIN
!
!  Cell-wise local value of particle radius and density to account for
!  hydration
!
            rLocal = r(i,j)
            rhopLocal = rhop(ix,iy,k,i,j)
!
!           Check value of rhFlag to see if want to correct
!
            sat = max(relhum(ix,iy,k),tiny(1.0_f)) ! to avoid zero FPE

!           Fitzgerald
            if(rhFlag .eq. 1 .and. sat .ge. 0.80_f) then
!            parameterization blows up for RH > 0.995, so set that as max
!            rh needs to be scaled 0 - 1
             sat = min(0.995_f,sat)
!            Calculate the alpha and beta parameters for the wet particle
!            relative to amonium sulfate
             beta = exp( (0.00077_f*sat) / (1.009_f-sat) )
             if(sat .le. 0.97_f) then
              theta = 1.058_f
             else
              theta = 1.058_f - (0.0155_f*(sat-0.97_f)) /(1.02_f-sat**1.4_f)
             endif
             alpha1 = 1.2_f*exp( (0.066_f*sat) / (theta-sat) )
             f1 = 10.2_f - 23.7_f*sat + 14.5_f*sat**2
             f2 = -6.7_f + 15.5_f*sat - 9.2_f*sat**2
             alpharat = 1._f - f1*(1._f-epsilon_) - f2*(1._f-epsilon_**2.)
             alpha = alphaNaCl * (alpha1*alpharat)
!            radius is the radius of the wet particle
             rLocal    = alpha * r(i,j)**beta
             rrat      = (r(i,j)/rLocal)**3
             rhopLocal = rrat*rhop(ix,iy,k,i,j) + (1._f-rrat)*rhow
!           Gerber
            elseif(rhFlag .eq. 2) then
             sat    = min(0.995_f,sat)
             rcm    = r(i,j)*100._f
             rLocal = 0.01_f * (   c1*rcm**c2 / (c3*rcm**c4-log10(sat)) &
                                 + rcm**3)**(1._f/3._f)
             rrat = (r(i,j)/rLocal)**3
             rhopLocal = rrat*rhop(ix,iy,k,i,j) + (1._f-rrat)*rhow
            endif
!
!  <rkn> is knudsen number
!
            rkn = rmfp/rLocal
!
!  <bpm> is correction term for non-continuum effects.  Also used to 
!  calculate coagulation kernels and diffusion coefficients.
!
! JAS, ref N.A. Fuchs, The mechanics of aerosols, 1964, p. 27
!  Rearrrangement of Eqn 8.5) 
!   vf = 2 / 9 * r ** 2 * rhop * GRAV / rmu
!        * [ 1 + 1.246 Kn + 0.42 * Kn * exp( -0.87 / Kn ) ]  

            expon = -.87_f / rkn
            expon = max(-POWMAX, expon)
            bpm(k,i,j) = 1._f + 1.246_f*rkn + 0.42_f*rkn*exp(expon) 

!  Stokes fall velocity and Reynolds' number

            vf(k,i,j) = (2._f/9._f)*rhopLocal*rLocal**2 &
                        *GRAV*bpm(k,i,j)/rmu(k)

            re(k,i,j) = 2._f*rhoa_mks*rLocal*vf(k,i,j)/rmu(k)

            if( re(k,i,j) .ge. 1._f )then

!   Correct drag coefficient for turbulence 

              x = log( re(k,i,j)/bpm(k,i,j) )
              y = x*(0.83_f - 0.013_f*x)

              re(k,i,j) = exp(y)*bpm(k,i,j)

              if( re(k,i,j) .le. 1.e3_f )then
!  
!  drag coefficient from quadratic fit y(x) when Re < 1,000
!
                vf(k,i,j) = re(k,i,j) * rmu(k) / &
                            (2._f*rLocal*rhoa_mks)
              else

!  drag coefficient = 0.45 independent of Reynolds number when Re > 1,000

                cdrag = 0.45_f 

                vf(k,i,j) = bpm(k,i,j)*                              &
                            sqrt( 8._f*rhopLocal*rLocal*GRAV / &
                            (3._f*cdrag*rhoa_mks) )
              endif

            endif
          enddo    ! <i=1,NBIN>
        enddo      ! <k=1,NZ>

!
!  Interpolate <vf> from layer mid-pts to layer boundaries.
!  <vf(k)> is the fall velocity at the lower edge of the layer
!
        nzm1 = max( 1, NZ-1 )

        do ibin = 1,NBIN

!
!  Set upper boundary before averaging
!
          vf(NZP1,ibin,j) = vf(NZ,ibin,j)

          if( NZ .gt. 1 )then
            vf(NZ,ibin,j) = sqrt( vf(nzm1,ibin,j)*vf(NZ,ibin,j) )

            if( NZ .gt. 2 ) then

              do iz=NZ-1,2,-1
	        vf(iz,ibin,j) = sqrt( vf(iz-1,ibin,j)* &
                  vf(iz,ibin,j) )
	      enddo

	    endif   ! <NZ .gt. 2>
	  endif     ! <NZ .gt. 1>
        enddo       ! <ibin = 1,NBIN>
          
      enddo         ! <j=1,NGROUP>

!
!
!  Constant value if <ifall> = 0
!
      if( ifall .eq. 0 )then
        do j = 1,NGROUP
          do i = 1,NBIN
            do k = 1,NZP1
              vf(k,i,j) = vf_const
            enddo
          enddo
        enddo
      endif
!
!
!  Print out fall velocities and reynolds' numbers in lowest model layer.
!

      k = 1
      if(igridv .eq. I_SIG) k = NZ

      if(ix .eq. NX .and. iy .eq. NY) then
      
       write(LUNOPRT,2)

       do j = 1,NGROUP
        
         if(rhFlag .eq. 0) then
          write(LUNOPRT,3) j
          do i = 1,NBIN
            write(LUNOPRT,4) i,r(i,j),vf(k,i,j),re(k,i,j)
          enddo
         else
          write(LUNOPRT,5) j
          do i = 1, NBIN
            rLocal = r(i,j)
            rhopLocal = rhop(ix,iy,k,i,j)
            sat = max(relhum(ix,iy,k),tiny(1.0_f)) ! to avoid zero FPE

!           Fitzgerald
            if(rhFlag .eq. 1 .and. sat .ge. 0.80_f) then
!            parameterization blows up for RH > 0.995, so set that as max
!            rh needs to be scaled 0 - 1
             sat = min(0.995_f,sat)
!            Calculate the alpha and beta parameters for the wet particle
!            relative to amonium sulfate
             beta = exp( (0.00077_f*sat) / (1.009_f-sat) )
             if(sat .le. 0.97_f) then
              theta = 1.058_f
             else
              theta = 1.058_f - (0.0155_f*(sat-0.97_f)) /(1.02_f-sat**1.4_f)
             endif
             alpha1 = 1.2_f*exp( (0.066_f*sat) / (theta-sat) )
             f1 = 10.2_f - 23.7_f*sat + 14.5_f*sat**2
             f2 = -6.7_f + 15.5_f*sat - 9.2_f*sat**2
             alpharat = 1._f - f1*(1._f-epsilon_) - f2*(1._f-epsilon_**2.)
             alpha = alphaNaCl * (alpha1*alpharat)
!            radius is the radius of the wet particle
             rLocal    = alpha * r(i,j)**beta
             rrat      = (r(i,j)/rLocal)**3
             rhopLocal = rrat*rhop(ix,iy,k,i,j) + (1._f-rrat)*rhow
!           Gerber
            elseif(rhFlag .eq. 2) then
             sat    = min(0.995_f,sat)
             rcm    = r(i,j)*100.
             rLocal = 0.01_f * (   c1*rcm**c2 / (c3*rcm**c4-log10(sat)) &
                                 + rcm**3)**(1._f/3._f)
             rrat = (r(i,j)/rLocal)**3
             rhopLocal = rrat*rhop(ix,iy,k,i,j) + (1._f-rrat)*rhow
            endif
            write(LUNOPRT,6) i,r(i,j),rLocal, vf(k,i,j),re(k,i,j)
          enddo
         endif
       enddo
      endif
!
!
!  Scale cartesian fallspeeds to the appropriate vertical coordinate system.
!  Non--cartesion coordinates are assumed to be positive downward, but
!  vertical velocities in this model are always assumed to be positive upward. 
!
      if( igridv .ne. I_CART )then

         do k=1,NZP1

          if( k .eq. 1 )then
           k1 = 1
           k2 = min( NZ, 2 )
          else if( k .eq. NZP1 )then
           k1 = NZ
           k2 = NZ
          else
           k1 = min( NZ, max( 1, k - 1 ) )
           k2 = k
          endif

          a_mid_k1 = zc(ix,iy,k1)
          a_mid_k2 = zc(ix,iy,k2)

          if(  a_mid_k2 .ne. a_mid_k1 )then
            frac = ( zl(ix,iy,k) - a_mid_k1 ) / ( a_mid_k2 - a_mid_k1 )
          else
            frac = 0.
          endif

          zmet_k1 = zmet(ix,iy,k1)
          zmet_k2 = zmet(ix,iy,k2)
          zmet_k = zmet_k1 + frac * ( zmet_k2 - zmet_k1 )

          do j = 1,NGROUP
            do i = 1,NBIN
              vf(k,i,j) = -vf(k,i,j) / zmet_k
            enddo
          enddo

         enddo

      endif

!  End horizontal loop
    enddo
   enddo
!allocate(scratch(NX,NY))
!do i = 1, NZ
! do ix = 1, NX
!  do iy = 1, NY
!!   scratch(ix,iy) = -carma%vf(ix,iy)%data3d(i,1,1)*zmet(ix,iy,i)
!   scratch(ix,iy) = carma%pc(ix,iy,i,1,1)
!  enddo
! enddo
! call pmaxmin('CARMAvf :',scratch,qmin,qmax,NX*NY,1,1.)
!enddo
!deallocate(scratch)
!
!  Return to caller with particle fall velocities evaluated.
!
      return
      end
