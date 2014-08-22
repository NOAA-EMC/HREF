! Colarco, May 21, 2007
! sed command to replace F77 comments with F90: sed 's/^c/\!/g'
! F90-version of original CARMA initatm.f routine (see comments below from
! original routine header).

       subroutine initatm ( carma, rc )

!     types
      use carma_types_mod

      implicit none

      integer, intent(out) :: rc

!     Local declarations
      integer :: idk, ix, iy, kb, ke, k
      integer :: k1, k2
      real(kind=f) :: a_mid_k1, a_mid_k2, frac, &
                      zmet_k1, zmet_k2, zmet_k
      real(kind=f) :: xyzmet, wsign

#include "carma_globaer.h"

      rc = 0

#ifdef DEBUG
      write(*,*) '+ initatm'
#endif

!
!
!  @(#) initatm.f  McKie  Jul-1997
!  This is the overall grid-related initialization routine.  It is
!  split into subprograms, one for each type of supported grid coord system.
! 
!  A user setting this routine up for a particular modelling problem
!  would typically choose a coordinate system by defining the
!  <igridv> and <igridh> variables to one of the supported symbols
!  (e.g. I_CART, I_SIG for <igridv>, I_LL, I_LC, I_PS, I_ME for <igridh>)
!  in this routine, then do the specific grid setup and atmospheric
!  variables setup for that coordinate system in the appropriate
!  subroutine with name of the form g_*_*.
!
!  This routine initializes various geometrical & atmospheric profiles.
!  The following variables are defined by this routine:
!
!   Variables defined at vertical layer boundaries:
!
!    Geometry:
!      zl       vertical coord
!      zmet     vertical metric scale factor [d(zl)/dz]
!
!    Physical variables:
!      w        vertical velocity
!      u        east-west velocity
!      v        north-south velocity
!      dkz      vertical diffusion coefficient
!
!
!   Variables defined at vertical layer mid-points:
!
!    Geometry:
!      zc       vertical coord
!      xc       x position at center of grid box
!      xu       x position at upper (right) edge of grid box
!      xl       x position at lower (left) edge of grid box
!      dx       x grid-box width
!      xmet     x direction metric scale factor, d(x_distance)/dx 
!      yc       y position at center of grid box
!      yu       y position at upper (right) edge of grid box
!      yl       y position at lower (left) edge of grid box
!      dy       y grid-box width
!      ymet     y direction metric scale factor, d(y_distance)/dy 
!
!    Physical variables:
!      p        Air pressure [dyne/cm^2]
!      rhoa     Scaled air density [g/x_units/y_units/z_units]
!      t        Air temperature [K]
!      ptc      Potential temperature concentration [K g/x_units/y_units/z_units]
!      rmu      Dynamic air viscosity [g/cm/s]
!      thcond   Thermal conductivity of dry air [erg/cm/s/degree_K]
!      dkx      east-west diffusion coefficient
!      dky      north-south diffusion coefficient
!
!  Note on the way the metric scale factors <xmet>, <ymet>, <zmet> are defined:
!
!   These scale factors (the amount of actual measurable cartesion distance
!   per unit change of the generalized coordinate in the direction of that
!   generalized coordinate) are chosen such that:
!
!     <xmet> * <ymet> * <zmet>  .gt.  0.
!     <xmet> * <ymet>  .gt.  0.
!
!   so that any volume (or area concentrations) are carried as positive
!   quantities in the model.
!
!   Since <xmet> and <ymet> are each naturally positive definite for
!   typical coordinates, and since the mathematical definition of <zmet> could
!   be positive or negative depending on whether the vertical coordinate
!   is positive upward (e.g. cartesian altitude) or positive downward 
!   (e.g. pressure or sigma), then in this model <zmet> is chosen to be
!   the absolute value of its mathematical definition (i.e., always positive).
!   The scaling of vertical wind <w> also follows this convention:
!   in all coordinates <w> is positive for updrafts. Hence, to scale vertical
!   wind in pressure or sigma units, multiply dp/dt or d(sigma)/dt by -1/<zmet>.
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
    1 format(i3,1p,6(2x,e11.3))
    2 format(/,'initatm initializations:',/)
    3 format('Error--(initatm) Unknown grid combination: igridv = ',i5, &
             ', igridh = ',i5)
    4 format(/,a,' at ix,iy=',2(1x,i4))
    5 format(/,a3,6(2x,a11))
    6 format(/,'Coordinate grid type: ',a)
    7 format(a,' = ',1p,e11.4)
    8 format(a,' = ',i5)
    9 format('Warning--(init): ', &
             'igridv set to I_CART because do_parcel = .true')
!
!
!  Announce entry to this routine
!
!      if( DEBUG ) write(LUNOPRT,'(/,a)') 'Enter initatm'
!
!
!  Define pointer to the type of grid coordinate system being used:
!  igridv for vertical coordinate, igridh for horizontal coordinates.
!
!  Possible values for igridv:
!       I_CART    cartesian
!       I_SIG     sigma
!
!  Possible values for igridh:
!       I_CART   cartesian
!       I_LL     longitude_latitude
!       I_LC     lambert_conformal
!       I_PS     polar_stereographic
!       I_ME     mercator
!
      if(.not.(do_hostmodel)) then
       igridv = I_SIG
       igridh = I_LL
      endif
!
!  Parcel model in cartesian coordinates only
!  (pressure coordinates would also be fine, but not yet set up)
!  
      if( do_parcel .and. igridv .eq. I_SIG )then
        igridv = I_CART
        write(LUNOPRT,9)
      endif
!
!
!  Do appropriate grid-related initializations depending on grid type
!
      if( igridh .eq. I_CART .and. igridv .eq. I_CART )then

        call g_cart_cart ( carma, rc )

      else if( igridh .eq. I_CART .and. igridv .eq. I_SIG )then

        call g_cart_sig ( carma, rc )

      else if( (igridh .eq. I_LL .and. igridv .eq. I_SIG) )then

        call g_ll_sig ( carma, rc )

#undef PRC
#ifdef PRC
      else if( igridh .eq. I_LC .and. igridv .eq. I_SIG )then

        call g_lc_sig ( carma, rc )

      else if( igridh .eq. I_PS .and. igridv .eq. I_SIG )then

        call g_ps_sig ( carma, rc )

      else if( igridh .eq. I_ME .and. igridv .eq. I_SIG )then

        call g_me_sig ( carma, rc )

#endif
      else
        write(LUNOPRT,3) igridv, igridh
        stop 1
      endif
!
!
!  Scale fields defined at midpoint of vertical layers using metric factors.
!   Rule of thumb:  Any quantity with unit of distance factor ds{x,y,z} should
!   have a factor of 1./{x,y,z}met.  
!
      u(:,:,:)    = u(:,:,:) / xmet(:,:,:)
      v(:,:,:)    = v(:,:,:) / ymet(:,:,:)
      dkx(:,:,:)  = dkx(:,:,:) / ( xmet(:,:,:)**2 )
      dky(:,:,:)  = dky(:,:,:) / ( ymet(:,:,:)**2 )
      ptc(:,:,:)  = ptc(:,:,:)  * ( xmet(:,:,:) * ymet(:,:,:) * zmet(:,:,:) )
      rhoa(:,:,:) = rhoa(:,:,:) * ( xmet(:,:,:) * ymet(:,:,:) * zmet(:,:,:) )

!
!
!  Specify the values of <ptc> assumed just above(below) the top(bottom)
!  of the model domain.
!
      ptc_topbnd(:,:) = ptc(:,:,NZ)
      ptc_botbnd(:,:) = ptc(:,:,1)
!
!
!  Hydrostatically balance initial atmospheric profiles 
!  (consistent with subsequent hydrostat calls)
!
      if( .not. do_parcel )then
! PRC - turn off for now
!        call hydrostat ( carma, rc )
      endif
!
!
!  Define appropriate sign factor for <w>.  Non-vertical-cartesion coordinates
!  are assumed to be positive downward, but <w> in this model is always assumed
!  to be positive upward. 
!
      if( igridv .ne. I_CART ) then
        wsign = -1._f
      else
        wsign = 1._f
      endif
!
!
!  Scale fields defined at boundaries of vertical layers using metric factors.
!   Linearly interpolate/extrapolate nearest 2 vertical midpoint metric factors.
!   Set boundary vertical velocities and diffusion coefficients to zero 
!   (since vertical transport ignores them)
!
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

        do iy=1,NY
         do ix = 1, NX

          a_mid_k1 = zc(ix,iy,k1) 
          a_mid_k2 = zc(ix,iy,k2) 

          if(  a_mid_k2 .ne. a_mid_k1 )then
            frac = ( zl(ix,iy,k) - a_mid_k1 ) / ( a_mid_k2 - a_mid_k1 ) 
          else
            frac = 0._f
          endif

          zmet_k1 = zmet(ix,iy,k1)
          zmet_k2 = zmet(ix,iy,k2)
          zmet_k = zmet_k1 + frac * ( zmet_k2 - zmet_k1 )
          dkz(ix,iy,k) = dkz(ix,iy,k) / ( zmet_k**2 )

          w(ix,iy,k) = w(ix,iy,k) / zmet_k
          w(ix,iy,k) = wsign * w(ix,iy,k)

         enddo
        enddo
      enddo
!
!
!  Announce this routine's results
!
      write(LUNOPRT,2)
!
!
!  Report type of coordinate grid
!
      write(LUNOPRT,8) 'igridv ', igridv
      write(LUNOPRT,8) 'igridh ', igridh
      write(LUNOPRT,6) gridname
!
!
!  Report coord system/grid/projection selection variables
!
      write(LUNOPRT,7) 'dom_llx', dom_llx
      write(LUNOPRT,7) 'dom_lly', dom_lly
      write(LUNOPRT,7) 'dom_urx', dom_urx
      write(LUNOPRT,7) 'dom_ury', dom_ury
      write(LUNOPRT,7) 'rlon0  ', rlon0
      write(LUNOPRT,7) 'rlat0  ', rlat0
      write(LUNOPRT,7) 'rlat1  ', rlat1
      write(LUNOPRT,7) 'rlat2  ', rlat2
      write(LUNOPRT,7) 'hemisph', hemisph

!
!
!  Define indices of a horiz grid pt at which to print atm vert structure
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
      else if( igridv .eq. I_SIG )then
        kb  = 1
        ke  = NZ
        idk = 1
      else
        write(LUNOPRT,8) 'bad igridv = ',igridv
        stop 1
      endif
!
!
!  Print atmospheric structure at horizontal grid point (ix,iy)
!
      write(LUNOPRT,4) 'Sample atm structure', ix, iy
      write(LUNOPRT,5) 'k', 'zc', 'p', 'rhoa', 't', 'rmu', 'thcond'

      do k = kb,ke,idk
        xyzmet = xmet(ix,iy,k)*ymet(ix,iy,k)*zmet(ix,iy,k)
        write(LUNOPRT,1) &
          k,zc(ix,iy,k),p(ix,iy,k),rhoa(ix,iy,k)/xyzmet, &
          t(ix,iy,k),rmu(k),thcond(k)
      enddo

!
!
!  Print detailed vertical structure at horizontal grid point (ix,iy)
!
      write(LUNOPRT,4) 'Vertical grid structure at ',ix, iy
      write(LUNOPRT,5) 'k', 'zc(k)', 'zl(k)','zl(k+1)','w(k)'
      do k=kb,ke,idk
       write(LUNOPRT,1) &
         k, zc(ix,iy,k), &
         zl(ix,iy,k), zl(ix,iy,k+1), w(ix,iy,k)
      enddo 

!
!
!  Return to caller with atmospheric profiles initialized.
!

      return
      end
!
!
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!
!
      subroutine g_cart_cart ( carma, rc )

!     types
      use carma_types_mod

      implicit none

      integer, intent(out) :: rc

!     Local declaration
      integer      :: ix, iy, k
      real(kind=f) :: dxfix, dyfix, t_sfc, dlapse, dzfix, tbot

#include "carma_globaer.h"

      rc = 0

#ifdef DEBUG
      write(*,*) '+ g_cart_cart'
#endif

!
!
!  @(#) g_cart_cart.f  Ackerman  Oct-1995
!  This routine handles atmospheric grid initialization
!  for the horizontal=cartesian and vertical=cartesian grid coordinates.
!
!  For this coord system and grid:
!   Horiz surface is a cartesian x vs y plane,
!   and vertical coordinate is cartesian altitude.
!
!   x,y,z are regular measurable distance in cm.
!   x is positive to the right (east), y is positive north. z is positive up.
!   xmet, ymet metric factors are all unity with no units.
!
!  Modified slightly for use with generalized coordinates.  Jul-1997, McKie.
!  (From v1.11 single geometrical coord system model's initatm.f routine.)
!
!
!  Include global constants and variables
!
!      include 'globaer.h'
!
!
!  Announce entry to this routine
!
!      if( DEBUG ) write(LUNOPRT,'(/,a)') 'Enter g_cart_cart'
!
!
!  Define bogus values for grid selection parameters that have no meaning
!  for this coordinate system.
!
      rlon0 = -999._f
      rlat0 = -999._f
      rlat1 = -999._f
      rlat2 = -999._f
      hemisph = -999._f
!
!
!  Define descriptive text for type of grid
!
      gridname = 'cartesian horizontal, cartesian vertical'
!
!
! For this example, grid spacing is uniform.
!  Define its east-west, north-south fixed grid box size.
!
      dxfix = 1.e3_f
      dyfix = 1.e3_f
      if(do_hostmodel) dxfix = carma%dxfix
      if(do_hostmodel) dyfix = carma%dyfix
!
!
!  Define lower left (southwest) upper right (northest) horizontal
!  domain limits for the grid
!
      dom_llx = 0._f
      dom_lly = 0._f
      dom_urx = NX * dxfix
      dom_ury = NY * dyfix
!
!
!  Specify horizontal grid coordinates and grid box sizes.
!   <dx> and <dy> are the east-west and north-south grid box thicknesses.
!   <xc>, <yc> are the coord at the center of each grid box.
!   <xl> & <xu> are the lower & upper grid box x coord at y=<yc>.
!   <yl> & <yu> are the lower & upper grid box y coord at x=<xc>.
!   <rlon> & <rlat> are longitude & latitude [deg] at planet surface
!     ( <rlon>, <rlat> usually assumed constant in horiz cartesian coord )
!
      do iy = 1,NY
	do ix = 1,NX
          rlon(ix,iy) = -90.
          rlat(ix,iy) = 45.
	  do k = 1,NZ
	    dx(ix,iy,k) = dxfix
            xc(ix,iy,k) = dom_llx + ( ix - .5_f ) * dx(ix,iy,k)
            xl(ix,iy,k) = dom_llx + ( ix - 1 ) * dx(ix,iy,k)
            xu(ix,iy,k) = dom_llx + ix * dx(ix,iy,k)
	    dy(ix,iy,k) = dyfix
            yc(ix,iy,k) = dom_lly + ( iy - .5_f ) * dy(ix,iy,k)
            yl(ix,iy,k) = dom_lly + ( iy - 1 ) * dy(ix,iy,k)
            yu(ix,iy,k) = dom_lly + iy * dy(ix,iy,k)
          enddo
        enddo
      enddo
!
!
!  Define vertical profile of atmospheric variables that are 3-D.
!   In this demo:
!    The vertical profiles do not vary horizontally.
!    Air temp <t> lapse rate is dry adiabatic from a surface value of <t_sfc>,
!    resulting in a constant potential temperature.
!    Pressure <p> is integrated upwards hydrostatically.
!    Air density <rhoa> is a function of <t> & <p>, from dry air equa of state.
!    All units are mks, deg_K.
!
      t_sfc = 273.16_f + 20._f
!
!
!  Define dry lapse rate 
!
      dlapse = GRAV / CP
!
!
!  Define (fixed) layer thickness
!
      dzfix = 50._f
      if(do_hostmodel) dzfix = carma%dzfix
!
!
!  Define surface pressure [N/m^2] 
!
      if(.not.do_hostmodel) p_surf(:,:) = 1013.5_f * RMB2MKS
!
!
!  Visit each horiz grid point & calculate atmospheric properties.
!
      do ix = 1,NX
       do iy = 1,NY
        if(.not.do_hostmodel) t_surf(ix,iy) = t_sfc
!
!
!  Bottom layer is special because it cannot be evaluated from a lower layer.
!
        zbot = 0._f
        k = 1
        zl(ix,iy,k) = zbot

        dz(ix,iy,k) = dzfix
        zl(ix,iy,k+1) = zl(ix,iy,k) + dzfix
        zc(ix,iy,k) = zl(ix,iy,k) + dzfix/2.
  
        if(.not.do_hostmodel) t(ix,iy,k) = t_sfc - dlapse * ( dzfix / 2._f )
        p(ix,iy,k) = p_surf(ix,iy) * &
                     exp( -dzfix/2._f * GRAV/(R_AIR*t_sfc) )
        rhoa(ix,iy,k) = p(ix,iy,k)/(R_AIR*t(ix,iy,k))
        ptc(ix,iy,k) = rhoa(ix,iy,k) * &
                       t(ix,iy,k) * ( PREF/p(ix,iy,k) )**RKAPPA
!
!
!  Integrate pressure upwards hydrostatically.
!
        do k = 2,NZ

          dz(ix,iy,k) = dzfix
          zl(ix,iy,k+1) = zl(ix,iy,k) + dzfix
          zc(ix,iy,k) = zc(ix,iy,k-1) + dzfix

          t(ix,iy,k) = t(ix,iy,k-1) - dlapse*dzfix
          tbot = 0.5 * ( t(ix,iy,k-1) + t(ix,iy,k) )
          p(ix,iy,k) = p(ix,iy,k-1) * &
                       exp( -dzfix*GRAV/(R_AIR*tbot) )
          if(.not.do_hostmodel) rhoa(ix,iy,k) = p(ix,iy,k)/(R_AIR*t(ix,iy,k))
          ptc(ix,iy,k) = rhoa(ix,iy,k) * &
                         t(ix,iy,k) * ( PREF/p(ix,iy,k) )**RKAPPA

        enddo
!
!
!  Pressure at top of model domain [dyne/cm^2]
!
        p_top(ix,iy) = p(ix,iy,NZ) * &
                       exp( -dzfix/2. * GRAV/(R_AIR*t(ix,iy,NZ)) )

       enddo
      enddo
!
!
!  Define the metric factors that convert coord system diffs to actual distance
!   In cartesian coordinates, the coord are directly distance.
!
      xmet = 1._f
      ymet = 1._f
      zmet = 1._f
!
!
!  Define vertical profile of atmospheric variables that are 1-D.
!   In this demo:
!    Air viscosity <rmu> is from Sutherland's equation (using Smithsonian
!     Meteorological Tables, in which there is a misprint -- T is deg_K, not deg_C.
!    Thermal conductivity of dry air <thcond> is from Pruppacher and Klett (1997), Eq. 13-18a.
!    Units of thermal conductivity are J /m /s /deg_C
!
      do ix = 1, NX
      do iy = 1, NY

        rmu => carma%rmu(ix,iy)%data1d
        thcond => carma%thcond(ix,iy)%data1d

        do k=1,NZ
          rmu(k) = rmu_const / ( t(ix,iy,k) + rmu_c ) * &
                    ( t(ix,iy,k) / rmu_t0 )**1.5_f
          thcond(k) = ( 5.69_f + .017_f*( t(ix,iy,k) - T0 ) )*4.186e-3_f
        enddo

      enddo
      enddo
!
!
!  Define vertical wind speed & diffusion coefficients [in metric mks units]
!
      w = 0._f
      dkx = 0._f
      dky = 0._f
      dkz = 0._f
!
!
!  Define horizontal wind speed [in metric mks units]
!
      u = 0._f
      v = 0._f
!
!
!  Return to caller with atm profiles initialized.
!
      return
      end
!
!
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!
!
      subroutine g_cart_sig ( carma, rc )

!     types
      use carma_types_mod

      implicit none

      integer, intent(out) :: rc

!     Local declaration
      integer      :: ix, iy, k
      real(kind=f) :: dxfix, dyfix, t_sfc, dlapse, pt_fix, pstar, expon, factor

#include "carma_globaer.h"

      rc = 0

#ifdef DEBUG
      write(*,*) '+ g_cart_sig'
#endif

!
!
!  @(#) g_cart_sig.f  Ackerman  Aug-1997
!  This routine handles atmospheric grid initialization
!  for the horizontal=cartesian and vertical=sigma grid coordinates.
!
!  For this coord system and grid:
!   x,y are regular measurable distance in cm.
!   x is positive to the right (east), y is positive north. z is positive up.
!   xmet, ymet metric factors are all unity with no units.
!   z is normalized (unitless) pressure, with fixed pressure (p_top) at top of atm.
!   z=0 at top of atm, z=1 at bottom of atm (positive toward planet surface).
!   zmet metric factor makes z space into vertical distance (altitude)
!
!  Modified slightly for use with generalized coordinates.  Jul-1997, McKie.
!  (From v1.11 single geometrical coord system model's initatm.f routine.)
!
!
!  Include global constants and variables
!
!      include 'globaer.h'
!
!
!  Announce entry to this routine
!
!      if( DEBUG ) write(LUNOPRT,'(/,a)') 'Enter g_cart_sig'
!
!
!  Define bogus values for grid selection parameters that have no meaning
!  for this coordinate system.
!
      rlon0 = -999.
      rlat0 = -999.
      rlat1 = -999.
      rlat2 = -999.
      hemisph = -999.
!
!
!  Define descriptive text for type of grid
!
      gridname = 'cartesian horizontal, sigma vertical'
!
!
! For this example, grid spacing is uniform.
!  Define its east-west, north-south fixed grid box size.
!
      dxfix = 1.e3_f
      dyfix = 1.e3_f
      if(do_hostmodel) dxfix = carma%dxfix
      if(do_hostmodel) dyfix = carma%dyfix
!
!
!  Define lower left (southwest) upper right (northest) horizontal
!  domain limits for the grid
!
      dom_llx = 0._f
      dom_lly = 0._f
      dom_urx = NX * dxfix
      dom_ury = NY * dyfix
!
!
!  Specify horizontal grid coordinates and grid box sizes.
!   <dx> and <dy> are the east-west and north-south grid box thicknesses.
!   <xc>, <yc> are the coord at the center of each grid box.
!   <xl> & <xu> are the lower & upper grid box x coord at y=<yc>.
!   <yl> & <yu> are the lower & upper grid box y coord at x=<xc>.
!   <rlon> & <rlat> are longitude & latitude [deg] at planet surface
!     ( <rlon>, <rlat> usually assumed constant in horiz cartesian coord )
!
      do iy = 1,NY
	do ix = 1,NX
          rlon(ix,iy) = -90._f
          rlat(ix,iy) = 45._f
	  do k = 1,NZ
	    dx(ix,iy,k) = dxfix
            xc(ix,iy,k) = dom_llx + ( ix - .5_f ) * dx(ix,iy,k)
            xl(ix,iy,k) = dom_llx + ( ix - 1 ) * dx(ix,iy,k)
            xu(ix,iy,k) = dom_llx + ix * dx(ix,iy,k)
	    dy(ix,iy,k) = dyfix
            yc(ix,iy,k) = dom_lly + ( iy - .5_f ) * dy(ix,iy,k)
            yl(ix,iy,k) = dom_lly + ( iy - 1 ) * dy(ix,iy,k)
            yu(ix,iy,k) = dom_lly + iy * dy(ix,iy,k)
          enddo
        enddo
      enddo
!
!
!  Define vertical profile of atmospheric variables that are 3-D.
!   In this demo:
!    The vertical profiles do not vary horizontally.
!    A constant potential temperature <pt> is defined throughout the atm.
!    Air temp <t> is computed as a function of <p> & <pt>.
!    Air density <rhoa> is a function of <t> & <p>, from dry air equa of state.
!    All units are mks, deg_K.
!
      t_sfc = 273.16_f + 20._f
!
!
!  Define dry lapse rate 
!
      dlapse = GRAV / CP
!
!
!  Define pressures at bottom and top of model domain [N/m^2]
!
      if(.not.do_hostmodel) then
       p_surf(:,:) = 1013.5_f * RMB2MKS
       p_top(:,:) = 900._f * RMB2MKS
      endif
!
!
!
!  Define vertical sigma values at grid box boundaries.
!   These could be enumerated.
!   This demo uses a simple fractional power function.
!   sigma(k)=((k-1)/NZ)**expon, where 0 < expon < 1.
!   This makes sigma intervals near ground smaller than at top of atm.
!   Use expon closer to 0 to get more dramatically changing sigma layers.
!
      expon = .5_f
      factor = 1._f / float(NZ)
      do k=1,NZP1
       if(.not.do_hostmodel) zl(:,:,k) = ( float(k-1) * factor )**expon
      enddo
!
!
!  Compute sigma values at vertical midpoints of grid boxes & vert box size
!
      do k=1,NZ
       zc(:,:,k) = exp(.5_f * ( log(zl(:,:,k)) + log(zl(:,:,k+1)) ) )
       dz(:,:,k) = zl(:,:,k+1) - zl(:,:,k)
      enddo 
!
      do iy=1,NY
        do ix=1,NX
          if(.not.do_hostmodel) t_surf(ix,iy) = t_sfc
          pt_fix = t_surf(ix,iy) * ( PREF / p_surf(ix,iy) )**RKAPPA
!          pstar = p_surf(ix,iy) - p_top(ix,iy)
          pstar = p_surf(ix,iy)
          do k=1,NZ
!            p(ix,iy,k) = p_top(ix,iy) + zc(ix,iy,k) * pstar
            p(ix,iy,k) = zc(ix,iy,k) * pstar
            if(.not.do_hostmodel) t(ix,iy,k) = pt_fix * ( p(ix,iy,k) / PREF )**RKAPPA
            if(.not.do_hostmodel) rhoa(ix,iy,k) = p(ix,iy,k) / ( R_AIR * t(ix,iy,k) )
            if(.not.do_hostmodel) then
             ptc(ix,iy,k) = rhoa(ix,iy,k) * pt_fix
            else
             ptc(ix,iy,k) = rhoa(ix,iy,k) * &
                            t(ix,iy,k) * ( PREF/p(ix,iy,k) )**RKAPPA
            endif
          enddo
        enddo
      enddo
!
!
!  Define horizontal metric factors that convert coord system diffs to actual distance
!   In cartesian coordinates, the coord are directly distance.
!
      xmet = 1._f
      ymet = 1._f
!
!  Define <zmet>, the  metric factor that makes <dz> in
!  arbitrary coord system a true distance.
!  See Toon et al., JAS, vol 45, #15, Aug-1988, p 2125.
!
      do iy=1,NY
        do ix=1,NX
!          pstar = p_surf(ix,iy) - p_top(ix,iy)
          pstar = p_surf(ix,iy)
          do k=1,NZ
            zmet(ix,iy,k) = pstar / ( GRAV * rhoa(ix,iy,k) )
          enddo
        enddo
      enddo
!
!
!  Define vertical profile of atmospheric variables that are 1-D.
!   In this demo:
!    Air viscosity <rmu> is from Sutherland's equation (using Smithsonian
!     Meteorological Tables, in which there is a misprint -- T is deg_K, not deg_C.
!    Thermal conductivity of dry air <thcond> is from Pruppacher and Klett (1997), Eq. 13-18a.
!    Units of thermal conductivity are J /m /s /deg_C
!
      do ix = 1, NX
      do iy = 1, NY

        rmu => carma%rmu(ix,iy)%data1d
        thcond => carma%thcond(ix,iy)%data1d

        do k=1,NZ
          rmu(k) = rmu_const / ( t(ix,iy,k) + rmu_c ) * &
                    ( t(ix,iy,k) / rmu_t0 )**1.5_f
          thcond(k) = ( 5.69_f + .017_f*( t(ix,iy,k) - T0 ) )*4.186e-3_f
        enddo

      enddo
      enddo
!
!
!  Define vertical wind speed & diffusion coefficients [in metric mks units]
!
      w = 0._f
      dkx = 0._f
      dky = 0._f
      dkz = 0._f
!
!
!  Define horizontal wind speed [in metric mks units]
!
      u = 0._f
      v = 0._f
!
!
!  Return to caller with atm profiles initialized.
!
      return
      end
!
!
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!
!
      subroutine g_ll_sig ( carma, rc )

!     types
      use carma_types_mod

      implicit none

      integer, intent(out) :: rc

!     Local declaration
      integer      :: ix, iy, k, ios
      real(kind=f) :: dlapse, expon, factor, t_sfc, pstar, pt_fix

      real(kind=f) :: zlinp32(33), zlinp72(73), zlinp28(29)
      real(kind=f), allocatable :: zlinp(:)

#include "carma_globaer.h"

! GEOS-4 32 layers
      data zlinp32 /0.0003946297, 0.0010458641, 0.0022101921, 0.004055356, &
                    0.0067589656, 0.010508497,  0.015442091,  0.021508439, &
                    0.028614786,  0.036311194,  0.044895645,  0.054417484, &
                    0.06519245,   0.07739816,   0.09113888,   0.10722019, &
                    0.1261391,    0.14839552,   0.1745801,    0.20538752, &
                    0.2416226,    0.28425673,   0.33461738,   0.3942385, &
                    0.46468806,   0.5478581,    0.64369017,   0.74191266, &
                    0.8293952,    0.9024107,    0.95567745,   0.9851099,  1. /
! GEOS-5 72 layers
      data zlinp72 / &
0.000010, 0.000020, 0.000032, 0.000047, 0.000065, 0.000088, 0.000118, 0.000157, &
0.000209, 0.000275, 0.000360, 0.000469, 0.000609, 0.000785, 0.001006, 0.001283, &
0.001629, 0.002057, 0.002585, 0.003233, 0.004022, 0.004980, 0.006134, 0.007519, &
0.009170, 0.011127, 0.013462, 0.016239, 0.019529, 0.023415, 0.027991, 0.033361, &
0.039642, 0.047011, 0.055639, 0.065719, 0.077470, 0.091139, 0.107220, 0.126139, &
0.148396, 0.174580, 0.205480, 0.241995, 0.285103, 0.334526, 0.372094, 0.409682, &
0.447301, 0.484935, 0.522588, 0.560247, 0.597925, 0.635602, 0.673291, 0.698420, &
0.723550, 0.748680, 0.773816, 0.798953, 0.819062, 0.834145, 0.849229, 0.864313, &
0.879398, 0.894482, 0.909567, 0.924653, 0.939740, 0.954825, 0.969912, 0.984999, &
1.000000 /
! MATCH from NCEP reanalyses 28 layer (29 interface)
      data zlinp28 / &
  0.000270000, 0.00657000,  0.0138600,  0.0230900,  0.0346900,  0.0492000,  0.0672300,  0.0894500, &
  0.116540,   0.149160,   0.187830,   0.232860,   0.284210,   0.341370,   0.403340,   0.468600, &
  0.535290,   0.601350,   0.664820,   0.724010,   0.777730,   0.825270,   0.866420,   0.901350, &
  0.930540,   0.954590,   0.974180,   0.990000, 1.00000 /


      rc = 0

      allocate( zlinp(carma%NZP1), stat=ios)
      if(ios /= 0) then
       rc = 1
       return
      endif
      if(NZ .eq. 32) zlinp = zlinp32
      if(NZ .eq. 72) zlinp = zlinp72
      if(NZ .eq. 28) zlinp = zlinp28

#ifdef DEBUG
      write(*,*) '+ g_ll_sig'
#endif

!
!
!  @(#) g_ll_sig.f  McKie  Jul-1997
!  This routine handles atmospheric grid initialization
!  for horizontal=lon/lat and vertical=sigma grid coordinates.
!
!  For this coord system and grid:
!   x is longitude, y is latitude, both in degrees.
!   x is positive east about Greenwich, y is positive north about equator.
!   x,y Grid node position arrays are in degrees.
!   x,y Grid box size are in degrees.
!   xmet, ymet metric factors make degree space into distance on surface.
!   z is normalized (unitless) pressure, with fixed pressure (p_top) at top of atm.
!   z=0 at top of atm, z=1 at bottom of atm (positive toward planet surface).
!   zmet metric factor makes make z space into vertical distance (altitude)
!
!
!  Include global constants and variables
!
!      include 'globaer.h'
!
!
!  Announce entry to this routine
!
!      if( DEBUG ) write(LUNOPRT,'(/,a)') 'Enter g_ll_sig'
!
!
!  Define bogus values for grid selection parameters that have no meaning
!  for this coordinate system.
!
      rlon0 = -999._f
      rlat0 = -999._f
      rlat1 = -999._f
      rlat2 = -999._f
      hemisph = -999._f
!
!
!  Define descriptive text for type of grid
!
      gridname = 'longitude & latitude horizontal, sigma vertical'
!
!
!  Define lower left and upper right lon,lat horizontal domain limits
!
      if(.not.do_hostmodel) then
       dom_llx = -135._f
       dom_urx = -60._f
       dom_lly = 15._f
       dom_ury = 50._f
      endif
!
!
!  Compute uniform spacing of grid nodes, independently in lon & lat directions 
!   (This example uses uniform lon & lat spacing, but variable spacing is possible)
!
      if(.not.do_hostmodel) then
       dlon = ( dom_urx - dom_llx ) / float(NX)
       dlat = ( dom_ury - dom_lly ) / float(NY)
      endif
!
!
!  Compute horizontal grid related things for each grid box:
!   <xc,yc> is lon,lat at grid box center
!   <xl,xu> is lon,lat at left & right edges at yc
!   <yl,yu> is lon,lat at bottom & top edges at xc
!   <dx,dy> is horiz grid box size in lon,lat at xc,yc 
!   <xmet> is metric factor that makes dx in arbitrary coord system a true distance
!   <ymet> is metric factor that makes dy in arbitrary coord system a true distance
!   <rlon> & <rlat> are longitude & latitude [deg] at planet surface
!
!      factor = REARTH * RAD2DEG

! JAS: horizontal metrics xmet and ymet are ratios of the distance in meters
! to the 'distance' in degrees.  ymet should be 2 pi r / 360, which is equal
! to pi r / 180.  pi / 180 is defined as DEG2RAD so factor = REARTH * DEG2RAD 

      factor = REARTH * DEG2RAD
      do iy=1,NY
        do ix=1,NX
          xc(ix,iy,1) = dom_llx + ( ix - .5_f ) * dlon
          yc(ix,iy,1) = dom_lly + ( iy - .5_f ) * dlat
          xl(ix,iy,1) = dom_llx + ( ix - 1 ) * dlon
          yl(ix,iy,1) = dom_lly + ( iy - 1 ) * dlat
          xu(ix,iy,1) = dom_llx + ( ix ) * dlon
          yu(ix,iy,1) = dom_lly + ( iy ) * dlat
          dx(ix,iy,1) = dlon
          dy(ix,iy,1) = dlat
          xmet(ix,iy,1) = factor * cos( DEG2RAD * yc(ix,iy,1) )
          ymet(ix,iy,1) = factor
          do k=2,NZ
           xc(ix,iy,k) = xc(ix,iy,1)
           yc(ix,iy,k) = yc(ix,iy,1)
           xl(ix,iy,k) = xl(ix,iy,1)
           yl(ix,iy,k) = yl(ix,iy,1)
           xu(ix,iy,k) = xu(ix,iy,1)
           yu(ix,iy,k) = yu(ix,iy,1)
           dx(ix,iy,k) = dx(ix,iy,1)
           dy(ix,iy,k) = dy(ix,iy,1)
           xmet(ix,iy,k) = xmet(ix,iy,1)
           ymet(ix,iy,k) = ymet(ix,iy,1)
          enddo
          rlon(ix,iy) = xc(ix,iy,NZ)
          rlat(ix,iy) = yc(ix,iy,NZ)
        enddo
      enddo
!
!
!  Define vertical profile of atmospheric variables that are 3-D.
!   In this demo:
!    The vertical profiles do not vary horizontally.
!    A constant potential temperature <pt> is defined throughout the atm.
!    Air temp <t> is computed as a function of <p> & <pt>.
!    Air density <rhoa> is a function of <t> & <p>, from dry air equa of state.
!    All units are mks, deg_K.
!
      t_sfc = 273.16_f + 20._f
!
!
!  Define dry lapse rate 
!
      dlapse = GRAV / CP
!
!
!  Define pressures at bottom and top of model domain [N/m^2]
!

      if(.not.do_hostmodel) then
       p_surf(:,:) = 1013.5_f * RMB2MKS
       p_top(:,:) = 0.40_f * RMB2MKS
      endif
!
!  define vertical sigma values at grid box boundaries.
!   these could be enumerated.
!   this demo uses a simple fractional power function.
!   sigma(k)=((k-1)/NZ)**expon, where 0 < expon < 1.
!   this makes sigma intervals near ground smaller than at top of atm.
!   use expon closer to 0 to get more dramatically changing sigma layers.
!
      expon = .5_f
      factor = 1._f / float(NZ)
      do k=1,NZP1
       if(.not.do_hostmodel) zl(:,:,k) = ( float(k-1) * factor )**expon
      enddo

! PRC this exists so that you can use the data statements above to specify
! vertical sigma levels different from the definition immediately above
!!     Possibly select other vertical coordinate
!      if( (NZ .eq. 32 .or. NZ .eq. 72 .or. NZ .eq. 28) .and. .not.do_hostmodel) then
!       do k = 1, NZ
!        zl(:,:,k) = zlinp(k)
!       enddo
!       p_top(:,:) = zlinp(1)
!      endif
!
!
!  Compute sigma values at vertical midpoints of grid boxes & vert box size
!
      do k=1,NZ
       zc(:,:,k) = exp(.5_f * ( log(zl(:,:,k)) + log(zl(:,:,k+1)) ) )
!       zc(:,:,k) = .5_f * ( zl(:,:,k) + zl(:,:,k+1) )
       dz(:,:,k) = zl(:,:,k+1) - zl(:,:,k)
      enddo 
      if(.not.do_hostmodel) zc(:,:,1) = 0.5*zl(:,:,2)
!
!
      do iy=1,NY
        do ix=1,NX
          if(.not.do_hostmodel) t_surf(ix,iy) = t_sfc
          pt_fix = t_surf(ix,iy) * ( PREF / p_surf(ix,iy) )**RKAPPA
!          pstar = p_surf(ix,iy) - p_top(ix,iy)
          pstar = p_surf(ix,iy)
          do k=1,NZ
!            p(ix,iy,k) = p_top(ix,iy) + zc(ix,iy,k) * pstar
            p(ix,iy,k) = zc(ix,iy,k) * pstar
            if(.not.do_hostmodel) t(ix,iy,k) = pt_fix * ( p(ix,iy,k) / PREF )**RKAPPA
            if(.not.do_hostmodel) rhoa(ix,iy,k) = p(ix,iy,k) / ( R_AIR * t(ix,iy,k) )
            if(.not.do_hostmodel) then
             ptc(ix,iy,k) = rhoa(ix,iy,k) * pt_fix
            else
             ptc(ix,iy,k) = rhoa(ix,iy,k) * &
                            t(ix,iy,k) * ( PREF/p(ix,iy,k) )**RKAPPA
            endif
          enddo
        enddo
      enddo
!
!
!  Define <zmet>, the  metric factor that makes <dz> in
!  arbitrary coord system a true distance.
!  See Toon et al., JAS, vol 45, #15, Aug-1988, p 2125.
!
      do iy=1,NY
        do ix=1,NX
!          pstar = p_surf(ix,iy) - p_top(ix,iy)
          pstar = p_surf(ix,iy)
          do k=1,NZ
            zmet(ix,iy,k) = pstar / ( GRAV * rhoa(ix,iy,k) )
          enddo
        enddo
      enddo
!
!
!  Define vertical profile of atmospheric variables that are 1-D.
!   In this demo:
!    Air viscosity <rmu> is from Sutherland's equation (using Smithsonian
!     Meteorological Tables, in which there is a misprint -- T is deg_K, not deg_C.
!    Thermal conductivity of dry air <thcond> is from Pruppacher and Klett (1997), Eq. 13-18a.
!    Units of thermal conductivity are J /m /s /deg_C
!

      do ix = 1, NX
      do iy = 1, NY

        rmu => carma%rmu(ix,iy)%data1d
        thcond => carma%thcond(ix,iy)%data1d

        do k=1,NZ
          rmu(k) = rmu_const / ( t(ix,iy,k) + rmu_c ) * &
                    ( t(ix,iy,k) / rmu_t0 )**1.5_f
          thcond(k) = ( 5.69_f + .017_f*( t(ix,iy,k) - T0 ) )*4.186e-3_f
        enddo

      enddo
      enddo
!
!
!  Define vertical wind speed & diffusion coefficients [in metric mks units]
!
      w = 0._f
      dkx = 0._f
      dky = 0._f
      dkz = 0._f
!
!
!  Define horizontal wind speed [in metric mks units]
!
      u = 0._f
      v = 0._f
!
!
!  Return to caller with atm profiles initialized
!
      deallocate(zlinp,stat=ios)
      if(ios /= 0) rc = 1

      return
      end
!
!
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#ifdef PRC
!
!
      subroutine g_lc_sig
!
!
!  @(#) g_lc_sig.f  McKie  Jul-1997
!  This routine handles atmospheric grid initialization
!  for horizontal=lambert_conformal and vertical=sigma grid coordinates.
!
!  For this coord system and grid:
!   Planet surface is projected to a cone whose apex is on unit radius planet's
!   polar axis & whose sides pass through 2 latitudes rlat1 & rlat2.  Projection
!   is from center of unit radius planet along straight line through planet
!   surface point to the cone surface.  The cone is then unwrapped along a
!   longitudinal cut line and spread onto the Pu,Pv projection plane, with a
!   central longitude at rlon0.
!
!   x is Pu, y Pv, both unitless.
!   x is positive to the right, y is positive up.
!   x,y Grid node position arrays are mapped back to lon,lat in degrees.
!   Lon is positive east of Greenwich in range [-180,180].
!   Lat is positive north of equator in range [-90,90].
!   x,y Grid box size are in projection plane Pu,Pv units for a unit sphere.
!   xmet, ymet metric factors make Pu,Pv space into distance on planet surface.
!   z is normalized (unitless) pressure, with fixed pressure (p_top) at top of atm.
!   z=0 at top of atm, z=1 at bottom of atm (positive toward planet surface).
!   zmet metric factor makes z space into vertical distance (altitude)
!
!
!  Include global constants and variables
!
      include 'globaer.h'
!
!
!  Announce entry to this routine
!
      if( DEBUG ) write(LUNOPRT,'(/,a)') 'Enter g_lc_sig'
!
!
!  Define bogus values for grid selection parameters that have no meaning
!  for this coordinate system.
!
      rlat0 = -999.
!
!
!  Define descriptive text for type of grid
!
      gridname = 'Lambert conformal horizontal, sigma vertical'
!
!
!  Define 2 latitudes at which cone passes through planet's surface
!
      rlat1 = 30.
      rlat2 = 60.
!
!
!  Define central longitude
!
      rlon0 = -100.
!
!
!  Define lon,lat for lower left and upper right lon,lat horizontal domain limits
!   (Note that these are independent of rlon0.  I.e. the domain can be
!    anywhere in the projection plane, not necessarily symmetric with rlon0,
!    or even near rlon0.  In this demo, the domain is chosen symmetrically
!    with rlon0.)
!
      dom_llx = -125.30
      dom_lly = 29.90
      dom_urx = -54.30
      dom_ury = 60.
!
!
!  Compute frequently used forward & reverse projection parameters.
!  Hemisphere constant <hemisph> is chosen automatically based on rlat1 & rlat2.
!   <hemisph> indicates the hemisphere opposite to the projection cone's apex:
!     hemisph = -1.   for northern apex (used for northern hemisph projections)
!     hemisph = +1.   for southern apex (used for southern hemisph projections)
!   <hemisph2> is hemisph/2.
!   <cone> is "cone constant" as a function of rlat1 & rlat2.
!
      call parmlc(rlat1,rlat2, hemisph,hemisph2,cone)
!
!
!  Compute Pu,Pv projections of lon,lat domain limits
!
      call projlc(cone,rlon0,hemisph,hemisph2,
     $            1, dom_llx, dom_lly, Pu_ll, Pv_ll)
      call projlc(cone,rlon0,hemisph,hemisph2,
     $            1, dom_urx, dom_ury, Pu_ur, Pv_ur)
!
!
!  Compute uniform spacing of grid nodes, independently in Pu & Pv directions 
!   (This example uses uniform Pu, Pv spacing, but variable spacing is possible)
!
      dPu = ( Pu_ur - Pu_ll ) / float(NX)
      dPv = ( Pv_ur - Pv_ll ) / float(NY)
!
!
!  Compute preliminary horizontal grid related things for each grid box:
!   <xc,yc> is initially Pu,Pv at grid box center
!   <xl,xu> is initially Pu,Pv at left & right edges at yc
!   <yl,yu> is initially Pu,Pv at bottom & top edges at xc
!   <dx,dy> is horiz grid box size in Pu,Pv at xc,yc 
!
      do iy=1,NY
        do ix=1,NX
          xc(ix,iy,1) = Pu_ll + ( ix - .5 ) * dPu
          yc(ix,iy,1) = Pv_ll + ( iy - .5 ) * dPv
          xl(ix,iy,1) = Pu_ll + ( ix - 1 ) * dPu
          yl(ix,iy,1) = Pv_ll + ( iy - 1 ) * dPv
          xu(ix,iy,1) = Pu_ll + ( ix ) * dPu
          yu(ix,iy,1) = Pv_ll + ( iy ) * dPv
          dx(ix,iy,1) = dPu
          dy(ix,iy,1) = dPv
          do k=2,NZ
           xc(ix,iy,k) = xc(ix,iy,1)
           yc(ix,iy,k) = yc(ix,iy,1)
           xl(ix,iy,k) = xl(ix,iy,1)
           yl(ix,iy,k) = yl(ix,iy,1)
           xu(ix,iy,k) = xu(ix,iy,1)
           yu(ix,iy,k) = yu(ix,iy,1)
           dx(ix,iy,k) = dx(ix,iy,1)
           dy(ix,iy,k) = dy(ix,iy,1)
          enddo
        enddo
      enddo
!
!
!  Compute horizontal metric factors by forming ratio of distance along planet's
!  surface to projected grid box distance in x & y directions. 
!   (u=upper, l=lower)
!
!   Grid box in Pu,Pv space is:
!
!                      rlon_yu, rlat_yu
!                          (xc,yu)
!                        +----*----+
!                        |         |
!                        |         |
!      rlon_xl, rlat_xl  *    +    *  rlon_xu, rlat_xu
!           (xl,yc)      |         |      (xu,yc)
!                        |         |
!                        +----*----+
!                          (xc,yl)
!                      rlon_yl, rlat_yl
!
!   <xmet> is metric factor that makes dx in arbitrary coord system a true distance
!   <ymet> is metric factor that makes dy in arbitrary coord system a true distance
!
!  After computing metric factor, reverse project the position info back from
!  Pu,Pv to lon,lat space.
!  (This is position info intended for history output for post-processing)
!   <xc,yc> becomes lon,lat at grid box center
!   <xl,xu> becomes lon,lat at left & right edges at yc
!   <yl,yu> becomes lon,lat at bottom & top edges at xc
!   <dx,dy> remains horiz grid box size in Pu,Pv at xc,yc 
!
      do ixyz=1,NXYZ

        call invplc(cone,rlon0,hemisph,
     $              1, xl3(ixyz),yc3(ixyz), rlon_xl,rlat_xl)
        call invplc(cone,rlon0,hemisph,
     $              1, xu3(ixyz),yc3(ixyz), rlon_xu,rlat_xu)
        call invplc(cone,rlon0,hemisph,
     $              1, xc3(ixyz),yl3(ixyz), rlon_yl,rlat_yl)
        call invplc(cone,rlon0,hemisph,
     $              1, xc3(ixyz),yu3(ixyz), rlon_yu,rlat_yu)

        ds_x = REARTH * sfirdis(rlon_xl,rlat_xl, rlon_xu,rlat_xu)
        ds_y = REARTH * sfirdis(rlon_yl,rlat_yl, rlon_yu,rlat_yu)

        xmet3(ixyz) = ds_x / dx3(ixyz)
        ymet3(ixyz) = ds_y / dy3(ixyz)

        xl3(ixyz) = rlon_xl
        xu3(ixyz) = rlon_xu
        yl3(ixyz) = rlat_yl
        yu3(ixyz) = rlat_yu

        call invplc(cone,rlon0,hemisph,
     $              1, xc3(ixyz),yc3(ixyz), xc3(ixyz),yc3(ixyz))

      enddo
!
!
!  Define 2-D longitude & latitude [deg] at planet surface, used internally by model
!
      do iy=1,NY
        do ix=1,NX
          rlon(ix,iy) = xc(ix,iy,NZ)
          rlat(ix,iy) = yc(ix,iy,NZ)
        enddo
      enddo
!
!
!  Define vertical sigma values at grid box boundaries.
!   These could be enumerated.
!   This demo uses a simple fractional power function.
!   sigma(k)=((k-1)/NZ)**expon, where 0 < expon < 1.
!   This makes sigma intervals near ground smaller than at top of atm.
!   Use expon closer to 0 to get more dramatically changing sigma layers.
!
      expon = .5
      factor = 1. / float(NZ)
      do k=1,NZP1
        zl2(1,k) = ( float(k-1) * factor )**expon
        do ixy=2,NXY
          zl2(ixy,k) = zl2(1,k)
        enddo
      enddo
!
!
!  Compute sigma values at vertical midpoints of grid boxes & vert box size
!
      do k=1,NZ
        zc2(1,k) = .5 * ( zl2(1,k) + zl2(1,k+1) )
        dz2(1,k) = zl2(1,k+1) - zl2(1,k)
        do ixy=2,NXY
          zc2(ixy,k) = zc2(1,k)
          dz2(ixy,k) = dz2(1,k)
        enddo
      enddo 
!
!
!  Define pressures at bottom and top of model domain [dyne/cm^2]
!
      do iy=1,NY
        do ix=1,NX
          p_surf(ix,iy) = 1013.5 * RMB2MKS
          p_top(ix,iy) = 900. * RMB2MKS
        enddo
      enddo
!
!
!  Define vertical profile of atmospheric variables that are 3-D.
!   In this demo:
!    The vertical profiles do not vary horizontally.
!    A constant potential temperature <pt> is defined throughout the atm.
!    Air temp <t> is computed as a function of <p> & <pt>.
!    Air density <rhoa> is a function of <t> & <p>, from dry air equa of state.
!    All units are cgs, deg_K.
!
      t_sfc = 288.

      do iy=1,NY
        do ix=1,NX
          t_surf(ix,iy) = t_sfc
          pt_fix = t_sfc * ( PREF / p_surf(ix,iy) )**RKAPPA
          pstar = p_surf(ix,iy) - p_top(ix,iy)
          do k=1,NZ
            p(ix,iy,k) = p_top(ix,iy) + zc(ix,iy,k) * pstar
            t(ix,iy,k) = pt_fix * ( p(ix,iy,k) / PREF )**RKAPPA
            rhoa(ix,iy,k) = p(ix,iy,k) / ( R_AIR * t(ix,iy,k) )
            ptc(ix,iy,k) = rhoa(ix,iy,k) * pt_fix
          enddo
        enddo
      enddo
!
!
!  Define <zmet>, the  metric factor that makes <dz> in
!  arbitrary coord system a true distance.
!  See Toon et al., JAS, vol 45, #15, Aug-1988, p 2125.
!
      do iy=1,NY
        do ix=1,NX
          pstar = p_surf(ix,iy) - p_top(ix,iy)
          do k=1,NZ
            zmet(ix,iy,k) = pstar / ( GRAV * rhoa(ix,iy,k) )
          enddo
        enddo
      enddo
!
!
!   Compute constants used in viscosity
!
      rmu_0 = 1.8325e-4
      rmu_t0 = 296.16
      rmu_c = 120.
      rmu_const = rmu_0 * (rmu_t0 + rmu_c)
!
!
!  Define vertical profile of atmospheric variables that are 1-D.
!   In this demo:
!    Air viscosity <rmu> is from Sutherland's equation (using Smithsonian
!     Meteorological Tables, in which there is a misprint -- T is deg_K, not deg_C.
!    Thermal conductivity of dry air <thcond> is from Pruppacher and Klett, Eq. 13-16.
!
      do k=1,NZ
        rmu(k) = rmu_const / ( t(1,1,k) + rmu_c ) *
     $              ( t(1,1,k) / rmu_t0 )**1.5d0
        thcond(k) = ( 5.69 + .017*( t(1,1,k) - T0 ) )*4.18e2
      enddo
!
!
!  Define vertical wind speed & diffusion coefficients [in metric cgs units]
!
      do k=1,NZP1
        do ix=1,NX
          do iy=1,NY
            w(ix,iy,k) = 0.
            dkx(ix,iy,k) = 0.e5
            dky(ix,iy,k) = 0.e5
            dkz(ix,iy,k) = 0.e-1
          enddo
        enddo
      enddo
!
!
!  Define horizontal wind speed [in metric cgs units]
!
      do k=1,NZ
        do ix=1,NX
          do iy=1,NY
            u(ix,iy,k) = 0.e4
            v(ix,iy,k) = 0.
          enddo
        enddo
      enddo
!
!
!  Return to caller with atm profiles initialized
!
      return
      end
!
!
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!
!
      subroutine g_ps_sig
!
!
!  @(#) g_ps_sig.f  McKie  Jul-1997
!  This routine handles atmospheric grid initialization
!  for horizontal=polar_stereographic and vertical=sigma grid coordinates.
!
!  For this coord system and grid:
!   Planet surface is projected to a plane tangent to a unit radius planet'
!   surface at latitude rlat0, usually 90 for north pole point or -90 for
!   south pole point, by straight line from pole point specified by
!   hemisph (hemisph=-1 for south pole, +1 for north) through planet surface
!   point to Pu,Pv projection plane.  The plane is oriented with central
!   longitude rlon0 on negative Pv axis. 
!
!   x is Pu, y Pv, both unitless.
!   x is positive to the right, y is positive up.
!   x,y Grid node position arrays are mapped back to lon,lat in degrees.
!   Lon is positive east of Greenwich in range [-180,180].
!   Lat is positive north of equator in range [-90,90].
!   x,y grid box size are in projection plane Pu,Pv units for a unit sphere.
!   xmet, ymet metric factors make Pu,Pv space into distance on planet surface.
!   z is normalized (unitless) pressure, with fixed pressure (p_top) at top of atm.
!   z=0 at top of atm, z=1 at bottom of atm (positive toward planet surface).
!   zmet metric factor makes z space into vertical distance (altitude)
!
!
!  Include global constants and variables
!
      include 'globaer.h'
!
!
!  Announce entry to this routine
!
      if( DEBUG ) write(LUNOPRT,'(/,a)') 'Enter g_lc_sig'
!
!
!  Define bogus values for grid selection parameters that have no meaning
!  for this coordinate system.
!
      rlat1 = -999.
      rlat2 = -999.
!
!
!  Define descriptive text for type of grid
!
      gridname = 'Polar stereographic horizontal, sigma vertical'
!
!
!  Define latitude at which projection plane is tangent to planet
!
      rlat0 = 90.
!
!
!  Define indication of which pole the projection line begins at:
!    <hemisph> = -1.   for south pole point (used for northern hemisph projections)
!    <hemisph> = +1.   for north pole point (used for southern hemisph projections)
!    <rlat0>=90 usually is paired with <hemisph>=-1.
!    <rlat0>=-90 usually is paired with <hemisph>=1.
!
      hemisph = -1.
!
!
!  Define central longitude
!
      rlon0 = -90.
!
!
!  Define lon,lat for lower left and upper right lon,lat horizontal domain limits
!   (Note that these are independent of rlon0.  I.e. the domain can be
!    anywhere in the projection plane, not necessarily symmetric with rlon0,
!    or even near rlon0.  In this demo, the domain is chosen somewhat
!    symmetrically with rlon0.)
!
      dom_llx = -135.00
      dom_lly = 45.
      dom_urx = 45.
      dom_ury = 45.
!
!
!  Compute frequently used forward & reverse projection parameters.
!   <factps> is projection factor computed as a function of rlat0 & hemisph.
!
      call parmps(rlat0,hemisph, factps)
!
!
!  Compute Pu,Pv projections of lon,lat domain limits
!
      call projps(rlon0,hemisph,factps,
     $            1, dom_llx, dom_lly, Pu_ll, Pv_ll)
      call projps(rlon0,hemisph,factps,
     $            1, dom_urx, dom_ury, Pu_ur, Pv_ur)
!
!
!  Compute uniform spacing of grid nodes, independently in Pu & Pv directions 
!   (This example uses uniform Pu, Pv spacing, but variable spacing is possible)
!
      dPu = ( Pu_ur - Pu_ll ) / float(NX)
      dPv = ( Pv_ur - Pv_ll ) / float(NY)
!
!
!  Compute preliminary horizontal grid related things for each grid box:
!   <xc,yc> is initially Pu,Pv at grid box center
!   <xl,xu> is initially Pu,Pv at left & right edges at yc
!   <yl,yu> is initially Pu,Pv at bottom & top edges at xc
!   <dx,dy> is horiz grid box size in Pu,Pv at xc,yc 
!
      do iy=1,NY
        do ix=1,NX
          xc(ix,iy,1) = Pu_ll + ( ix - .5 ) * dPu
          yc(ix,iy,1) = Pv_ll + ( iy - .5 ) * dPv
          xl(ix,iy,1) = Pu_ll + ( ix - 1 ) * dPu
          yl(ix,iy,1) = Pv_ll + ( iy - 1 ) * dPv
          xu(ix,iy,1) = Pu_ll + ( ix ) * dPu
          yu(ix,iy,1) = Pv_ll + ( iy ) * dPv
          dx(ix,iy,1) = dPu
          dy(ix,iy,1) = dPv
          do k=2,NZ
           xc(ix,iy,k) = xc(ix,iy,1)
           yc(ix,iy,k) = yc(ix,iy,1)
           xl(ix,iy,k) = xl(ix,iy,1)
           yl(ix,iy,k) = yl(ix,iy,1)
           xu(ix,iy,k) = xu(ix,iy,1)
           yu(ix,iy,k) = yu(ix,iy,1)
           dx(ix,iy,k) = dx(ix,iy,1)
           dy(ix,iy,k) = dy(ix,iy,1)
          enddo
        enddo
      enddo
!
!
!  Compute horizontal metric factors by forming ratio of distance along planet's
!  surface to projected grid box distance in x & y directions. 
!   (u=upper, l=lower)
!
!   Grid box in Pu,Pv space is:
!
!                      rlon_yu, rlat_yu
!                          (xc,yu)
!                        +----*----+
!                        |         |
!                        |         |
!      rlon_xl, rlat_xl  *    +    *  rlon_xu, rlat_xu
!           (xl,yc)      |         |      (xu,yc)
!                        |         |
!                        +----*----+
!                          (xc,yl)
!                      rlon_yl, rlat_yl
!
!   <xmet> is metric factor that makes dx in arbitrary coord system a true distance
!   <ymet> is metric factor that makes dy in arbitrary coord system a true distance
!
!  After computing metric factor, reverse project the position info back from
!  Pu,Pv to lon,lat space.
!  (This is position info intended for history output for post-processing)
!   <xc,yc> becomes lon,lat at grid box center
!   <xl,xu> becomes lon,lat at left & right edges at yc
!   <yl,yu> becomes lon,lat at bottom & top edges at xc
!   <dx,dy> remains horiz grid box size in Pu,Pv at xc,yc 
!
      do ixyz=1,NXYZ

        call invpps(rlon0,hemisph,factps,
     $              1, xl3(ixyz),yc3(ixyz), rlon_xl,rlat_xl)
        call invpps(rlon0,hemisph,factps,
     $              1, xu3(ixyz),yc3(ixyz), rlon_xu,rlat_xu)
        call invpps(rlon0,hemisph,factps,
     $              1, xc3(ixyz),yl3(ixyz), rlon_yl,rlat_yl)
        call invpps(rlon0,hemisph,factps,
     $              1, xc3(ixyz),yu3(ixyz), rlon_yu,rlat_yu)

        ds_x = REARTH * sfirdis(rlon_xl,rlat_xl, rlon_xu,rlat_xu)
        ds_y = REARTH * sfirdis(rlon_yl,rlat_yl, rlon_yu,rlat_yu)

        xmet3(ixyz) = ds_x / dx3(ixyz)
        ymet3(ixyz) = ds_y / dy3(ixyz)

        xl3(ixyz) = rlon_xl
        xu3(ixyz) = rlon_xu
        yl3(ixyz) = rlat_yl
        yu3(ixyz) = rlat_yu

        call invpps(rlon0,hemisph,factps,
     $              1, xc3(ixyz),yc3(ixyz), xc3(ixyz),yc3(ixyz))

      enddo
!
!
!  Define 2-D longitude & latitude [deg] at planet surface, used internally by model
!
      do iy=1,NY
        do ix=1,NX
          rlon(ix,iy) = xc(ix,iy,NZ)
          rlat(ix,iy) = yc(ix,iy,NZ)
        enddo
      enddo
!
!
!  Define vertical sigma values at grid box boundaries.
!   These could be enumerated.
!   This demo uses a simple fractional power function.
!   sigma(k)=((k-1)/NZ)**expon, where 0 < expon < 1.
!   This makes sigma intervals near ground smaller than at top of atm.
!   Use expon closer to 0 to get more dramatically changing sigma layers.
!
      expon = .5
      factor = 1. / float(NZ)
      do k=1,NZP1
        zl2(1,k) = ( float(k-1) * factor )**expon
        do ixy=2,NXY
          zl2(ixy,k) = zl2(1,k)
        enddo
      enddo
!
!
!  Compute sigma values at vertical midpoints of grid boxes & vert box size
!
      do k=1,NZ
        zc2(1,k) = .5 * ( zl2(1,k) + zl2(1,k+1) )
        dz2(1,k) = zl2(1,k+1) - zl2(1,k)
        do ixy=2,NXY
          zc2(ixy,k) = zc2(1,k)
          dz2(ixy,k) = dz2(1,k)
        enddo
      enddo 
!
!
!  Define pressures at bottom and top of model domain [dyne/cm^2]
!
      do iy=1,NY
        do ix=1,NX
          p_surf(ix,iy) = 1013.5 * RMB2MKS
          p_top(ix,iy) = 900. * RMB2MKS
        enddo
      enddo
!
!
!  Define vertical profile of atmospheric variables that are 3-D.
!   In this demo:
!    The vertical profiles do not vary horizontally.
!    A constant potential temperature <pt> is defined throughout the atm.
!    Air temp <t> is computed as a function of <p> & <pt>.
!    Air density <rhoa> is a function of <t> & <p>, from dry air equa of state.
!    All units are cgs, deg_K.
!
      t_sfc = 288.

      do iy=1,NY
        do ix=1,NX
          t_surf(ix,iy) = t_sfc
          pt_fix = t_sfc * ( PREF / p_surf(ix,iy) )**RKAPPA
          pstar = p_surf(ix,iy) - p_top(ix,iy)
          do k=1,NZ
            p(ix,iy,k) = p_top(ix,iy) + zc(ix,iy,k) * pstar
            t(ix,iy,k) = pt_fix * ( p(ix,iy,k) / PREF )**RKAPPA
            rhoa(ix,iy,k) = p(ix,iy,k) / ( R_AIR * t(ix,iy,k) )
            ptc(ix,iy,k) = rhoa(ix,iy,k) * pt_fix
          enddo
        enddo
      enddo
!
!
!  Define <zmet>, the  metric factor that makes <dz> in
!  arbitrary coord system a true distance.
!  See Toon et al., JAS, vol 45, #15, Aug-1988, p 2125.
!
      do iy=1,NY
        do ix=1,NX
          pstar = p_surf(ix,iy) - p_top(ix,iy)
          do k=1,NZ
            zmet(ix,iy,k) = pstar / ( GRAV * rhoa(ix,iy,k) )
          enddo
        enddo
      enddo
!
!
!   Compute constants used in viscosity
!
      rmu_0 = 1.8325e-4
      rmu_t0 = 296.16
      rmu_c = 120.
      rmu_const = rmu_0 * (rmu_t0 + rmu_c)
!
!
!  Define vertical profile of atmospheric variables that are 1-D.
!   In this demo:
!    Air viscosity <rmu> is from Sutherland's equation (using Smithsonian
!     Meteorological Tables, in which there is a misprint -- T is deg_K, not deg_C.
!    Thermal conductivity of dry air <thcond> is from Pruppacher and Klett, Eq. 13-16.
!
      do k=1,NZ
        rmu(k) = rmu_const / ( t(1,1,k) + rmu_c ) *
     $              ( t(1,1,k) / rmu_t0 )**1.5d0
        thcond(k) = ( 5.69 + .017*( t(1,1,k) - T0 ) )*4.18e2
      enddo
!
!
!  Define vertical wind speed & diffusion coefficients [in metric cgs units]
!
      do k=1,NZP1
        do ix=1,NX
          do iy=1,NY
            w(ix,iy,k) = 0.
            dkx(ix,iy,k) = 0.e5
            dky(ix,iy,k) = 0.e5
            dkz(ix,iy,k) = 0.e-1
          enddo
        enddo
      enddo
!
!
!  Define horizontal wind speed [in metric cgs units]
!
      do k=1,NZ
        do ix=1,NX
          do iy=1,NY
            u(ix,iy,k) = 0.e4
            v(ix,iy,k) = 0.
          enddo
        enddo
      enddo
!
!
!  Return to caller with atm profiles initialized
!
      return
      end
!
!
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!
!
      subroutine g_me_sig
!
!
!  @(#) g_me_sig.f  McKie  Jul-1997
!  This routine handles atmospheric grid initialization
!  for horizontal=Mercator and vertical=sigma grid coordinates.
!
!  For this coord system and grid:
!   Planet surface is projected onto a cylinder whose axis is coincident with
!   a unit radius planet's polar axis.  The cylinder's radius may be less than
!   or same as the unit radius of the planet.
!   The northern hemisphere latitude at which the cyclinder cuts the
!   surface of the planet is at latitude rlat0.
!   Projection is from the center of the planet along a straight line
!   through point on surface of planet, and continuing to surface of cylinder.
!   The cycliner is then unrolled into the projection plane Pu,Pv by a
!   cut along the image of a longitude, and the plane shifted so that
!   longitude rlon0 is in the center of the plane.
!
!   x is Pu, y Pv, both unitless.
!   x is positive to the right, y is positive up.
!   x,y grid node position arrays are mapped back to lon,lat in degrees.
!   Lon is positive east of Greenwich in range [-180,180].
!   Lat is positive north of equator in range [-90,90].
!   x,y grid box size are in projection plane Pu,Pv units for a unit sphere.
!   xmet, ymet metric factors make Pu,Pv space into distance on planet surface.
!   z is normalized (unitless) pressure, with fixed pressure (p_top) at top of atm.
!   z=0 at top of atm, z=1 at bottom of atm (positive toward planet surface).
!   zmet metric factor makes z space into vertical distance (altitude)
!
!
!  Include global constants and variables
!
      include 'globaer.h'
!
!
!  Announce entry to this routine
!
      if( DEBUG ) write(LUNOPRT,'(/,a)') 'Enter g_lc_sig'
!
!
!  Define bogus values for grid selection parameters that have no meaning
!  for this coordinate system.
!
      rlat1 = -999.
      rlat2 = -999.
      hemisph = -999.
!
!
!  Define descriptive text for type of grid
!
      gridname = 'Mercator horizontal, sigma vertical'
!
!
!  Define latitude at which projection cylinder cuts through surface of planet
!
      rlat0 = 0.
!
!
!  Define central longitude
!
      rlon0 = -90.
!
!
!  Define lon,lat for lower left & upper right lon,lat horizontal domain limits
!   (Note that these are independent of rlon0.  I.e. the domain can be
!    anywhere in the projection plane, not necessarily symmetric with rlon0,
!    or even near rlon0.  In this demo, the domain is chosen symmetrically
!    with rlon0.)
!
      dom_llx = -135.00
      dom_lly = -45.
      dom_urx = -45.
      dom_ury = 45.
!
!
!  Compute frequently used forward & reverse projection parameters.
!   <factme> is projection factor computed as a function of rlat0.
!
      call parmme(rlat0, factme)
!
!
!  Compute Pu,Pv projections of lon,lat domain limits
!
      call projme(rlon0,factme,
     $            1, dom_llx, dom_lly, Pu_ll, Pv_ll)
      call projme(rlon0,factme,
     $            1, dom_urx, dom_ury, Pu_ur, Pv_ur)
!
!
!  Compute uniform spacing of grid nodes, independently in Pu & Pv directions 
!   (This example uses uniform Pu, Pv spacing, but variable spacing is possible)
!
      dPu = ( Pu_ur - Pu_ll ) / float(NX)
      dPv = ( Pv_ur - Pv_ll ) / float(NY)
!
!
!  Compute preliminary horizontal grid related things for each grid box:
!   <xc,yc> is initially Pu,Pv at grid box center
!   <xl,xu> is initially Pu,Pv at left & right edges at yc
!   <yl,yu> is initially Pu,Pv at bottom & top edges at xc
!   <dx,dy> is horiz grid box size in Pu,Pv at xc,yc 
!
      do iy=1,NY
        do ix=1,NX
          xc(ix,iy,1) = Pu_ll + ( ix - .5 ) * dPu
          yc(ix,iy,1) = Pv_ll + ( iy - .5 ) * dPv
          xl(ix,iy,1) = Pu_ll + ( ix - 1 ) * dPu
          yl(ix,iy,1) = Pv_ll + ( iy - 1 ) * dPv
          xu(ix,iy,1) = Pu_ll + ( ix ) * dPu
          yu(ix,iy,1) = Pv_ll + ( iy ) * dPv
          dx(ix,iy,1) = dPu
          dy(ix,iy,1) = dPv
          do k=2,NZ
           xc(ix,iy,k) = xc(ix,iy,1)
           yc(ix,iy,k) = yc(ix,iy,1)
           xl(ix,iy,k) = xl(ix,iy,1)
           yl(ix,iy,k) = yl(ix,iy,1)
           xu(ix,iy,k) = xu(ix,iy,1)
           yu(ix,iy,k) = yu(ix,iy,1)
           dx(ix,iy,k) = dx(ix,iy,1)
           dy(ix,iy,k) = dy(ix,iy,1)
          enddo
        enddo
      enddo
!
!
!  Compute horizontal metric factors by forming ratio of distance along planet's
!  surface to projected grid box distance in x & y directions. 
!   (u=upper, l=lower)
!
!   Grid box in Pu,Pv space is:
!
!                      rlon_yu, rlat_yu
!                          (xc,yu)
!                        +----*----+
!                        |         |
!                        |         |
!      rlon_xl, rlat_xl  *    +    *  rlon_xu, rlat_xu
!           (xl,yc)      |         |      (xu,yc)
!                        |         |
!                        +----*----+
!                          (xc,yl)
!                      rlon_yl, rlat_yl
!
!   <xmet> is metric factor that makes dx in arbitrary coord system a true distance
!   <ymet> is metric factor that makes dy in arbitrary coord system a true distance
!
!  After computing metric factor, reverse project the position info back from
!  Pu,Pv to lon,lat space.
!  (This is position info intended for history output for post-processing)
!   <xc,yc> becomes lon,lat at grid box center
!   <xl,xu> becomes lon,lat at left & right edges at yc
!   <yl,yu> becomes lon,lat at bottom & top edges at xc
!   <dx,dy> remains horiz grid box size in Pu,Pv at xc,yc 
!
      do ixyz=1,NXYZ

        call invpme(rlon0,factme,
     $              1, xl3(ixyz),yc3(ixyz), rlon_xl,rlat_xl)
        call invpme(rlon0,factme,
     $              1, xu3(ixyz),yc3(ixyz), rlon_xu,rlat_xu)
        call invpme(rlon0,factme,
     $              1, xc3(ixyz),yl3(ixyz), rlon_yl,rlat_yl)
        call invpme(rlon0,factme,
     $              1, xc3(ixyz),yu3(ixyz), rlon_yu,rlat_yu)

        ds_x = REARTH * sfirdis(rlon_xl,rlat_xl, rlon_xu,rlat_xu)
        ds_y = REARTH * sfirdis(rlon_yl,rlat_yl, rlon_yu,rlat_yu)

        xmet3(ixyz) = ds_x / dx3(ixyz)
        ymet3(ixyz) = ds_y / dy3(ixyz)

        xl3(ixyz) = rlon_xl
        xu3(ixyz) = rlon_xu
        yl3(ixyz) = rlat_yl
        yu3(ixyz) = rlat_yu

        call invpme(rlon0,factme,
     $              1, xc3(ixyz),yc3(ixyz), xc3(ixyz),yc3(ixyz))

      enddo
!
!
!  Define 2-D longitude & latitude [deg] at planet surface, used internally by model
!
      do iy=1,NY
        do ix=1,NX
          rlon(ix,iy) = xc(ix,iy,NZ)
          rlat(ix,iy) = yc(ix,iy,NZ)
        enddo
      enddo
!
!
!  Define vertical sigma values at grid box boundaries.
!   These could be enumerated.
!   This demo uses a simple fractional power function.
!   sigma(k)=((k-1)/NZ)**expon, where 0 < expon < 1.
!   This makes sigma intervals near ground smaller than at top of atm.
!   Use expon closer to 0 to get more dramatically changing sigma layers.
!
      expon = .5
      factor = 1. / float(NZ)
      do k=1,NZP1
        zl2(1,k) = ( float(k-1) * factor )**expon
        do ixy=2,NXY
          zl2(ixy,k) = zl2(1,k)
        enddo
      enddo
!
!
!  Compute sigma values at vertical midpoints of grid boxes & vert box size
!
      do k=1,NZ
        zc2(1,k) = .5 * ( zl2(1,k) + zl2(1,k+1) )
        dz2(1,k) = zl2(1,k+1) - zl2(1,k)
        do ixy=2,NXY
          zc2(ixy,k) = zc2(1,k)
          dz2(ixy,k) = dz2(1,k)
        enddo
      enddo 
!
!
!  Define pressures at bottom and top of model domain [dyne/cm^2]
!
      do iy=1,NY
        do ix=1,NX
          p_surf(ix,iy) = 1013.5 * RMB2MKS
          p_top(ix,iy) = 900. * RMB2MKS
        enddo
      enddo
!
!
!  Define vertical profile of atmospheric variables that are 3-D.
!   In this demo:
!    The vertical profiles do not vary horizontally.
!    A constant potential temperature <pt> is defined throughout the atm.
!    Air temp <t> is computed as a function of <p> & <pt>.
!    Air density <rhoa> is a function of <t> & <p>, from dry air equa of state.
!    All units are cgs, deg_K.
!
      t_sfc = 288.

      do iy=1,NY
        do ix=1,NX
          t_surf(ix,iy) = t_sfc
          pt_fix = t_sfc * ( PREF / p_surf(ix,iy) )**RKAPPA
          pstar = p_surf(ix,iy) - p_top(ix,iy)
          do k=1,NZ
            p(ix,iy,k) = p_top(ix,iy) + zc(ix,iy,k) * pstar
            t(ix,iy,k) = pt_fix * ( p(ix,iy,k) / PREF )**RKAPPA
            rhoa(ix,iy,k) = p(ix,iy,k) / ( R_AIR * t(ix,iy,k) )
            ptc(ix,iy,k) = rhoa(ix,iy,k) * pt_fix
          enddo
        enddo
      enddo
!
!
!  Define <zmet>, the  metric factor that makes <dz> in
!  arbitrary coord system a true distance.
!  See Toon et al., JAS, vol 45, #15, Aug-1988, p 2125.
!
      do iy=1,NY
        do ix=1,NX
          pstar = p_surf(ix,iy) - p_top(ix,iy)
          do k=1,NZ
            zmet(ix,iy,k) = pstar / ( GRAV * rhoa(ix,iy,k) )
          enddo
        enddo
      enddo
!
!
!   Compute constants used in viscosity
!
      rmu_0 = 1.8325e-4
      rmu_t0 = 296.16
      rmu_c = 120.
      rmu_const = rmu_0 * (rmu_t0 + rmu_c)
!
!
!  Define vertical profile of atmospheric variables that are 1-D.
!   In this demo:
!    Air viscosity <rmu> is from Sutherland's equation (using Smithsonian
!     Meteorological Tables, in which there is a misprint -- T is deg_K, not deg_C.
!    Thermal conductivity of dry air <thcond> is from Pruppacher and Klett, Eq. 13-16.
!
      do k=1,NZ
        rmu(k) = rmu_const / ( t(1,1,k) + rmu_c ) *
     $              ( t(1,1,k) / rmu_t0 )**1.5d0
        thcond(k) = ( 5.69 + .017*( t(1,1,k) - T0 ) )*4.18e2
      enddo
!
!
!  Define vertical wind speed & diffusion coefficients [in metric cgs units]
!
      do k=1,NZP1
        do ix=1,NX
          do iy=1,NY
            w(ix,iy,k) = 0.
            dkx(ix,iy,k) = 0.e5
            dky(ix,iy,k) = 0.e5
            dkz(ix,iy,k) = 0.e-1
          enddo
        enddo
      enddo
!
!
!  Define horizontal wind speed [in metric cgs units]
!
      do k=1,NZ
        do ix=1,NX
          do iy=1,NY
            u(ix,iy,k) = 0.e4
            v(ix,iy,k) = 0.
          enddo
        enddo
      enddo
!
!
!  Return to caller with atm profiles initialized
!
      return
      end
#endif
