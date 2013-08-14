       subroutine setupckern ( carma, rc ) 

!      types
       use carma_types_mod

       implicit none

       integer, intent(out) :: rc

! J. A. Smith (JAS) June 2007
!
! Previous versions of CARMA used cgs units.  We are converting to mks units
! with this implementation, so coagulation kernels have units of m**3/s.
!
! CARMA was also originally written such that physical quantities such as
! air viscosity (rmu) and particle sedimentation speed (vf) depended only
! vertical level (k).  For global simulations, we want to capture the fact
! that the Earth has significant variation in these quantities due
! to things such as topography -- fall speeds over the ocean are different
! than fall speeds over the Himalayas for the same vertical level.

!  @(#) setupckern.f  Ackerman Oct-1995 
! 
!  This routine evaluates the coagulation kernels, ckernel(k,j1,j2,i1,i2)
!  [cm^3 s^-1]. Indices correspond to vertical level <k>, aerosol groups
!  <j1,j2> and bins <i1,i2> of colliding particles.
!
!  This routine requires that vertical profiles of temperature <T>,
!  air density <rhoa>, and viscosity <rmu> are defined.
!  (i.e., initatm.f must be called before this)
!  The vertical profile with ix = iy = 1 is used.
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
!  Local declarations
!
!    <e_coll2> is 2-D collision efficiency for current group pair under
!    consideration (for extrapolation of input data)
!

      integer :: ios
      real(kind=f), allocatable :: e_coll2(:,:)

!    <NP_DATA> is number of collector/collected pairs in input data 
!    <NR_DATA> is number of radius bins in input data

      integer, parameter :: NP_DATA = 21, NR_DATA = 12

!    <data_p> are radius ratios (collected/collector)
!    <data_r> are collector drop radii (um)
!    <data_e> are geometric collection efficiencies

      real(kind=f), dimension(NP_DATA) :: data_p
      real(kind=f), dimension(NR_DATA) :: data_r
      real(kind=f), dimension(NP_DATA,NR_DATA) :: data_e

! JAS: for some reason, data_e doesn't initialize with the data statements
!      when setupckern is called a 2nd time.  Leads to log of negative
!      numbers when you take the log of data_e the 2nd time.
!      data_r suffers similar problem
!
!      The solution is to an original copy of the data: orig_r and orig_e
!      and a scaled copy: data_r and data_e

      real(kind=f), dimension(NR_DATA) :: orig_r
      real(kind=f), dimension(NP_DATA,NR_DATA) :: orig_e

      integer :: ip
      integer :: ig, jg
      integer :: ix, iy
      real(kind=f) :: cstick
      integer :: i1, i2, j1, j2, k
      integer :: i

      real(kind=f) :: rhoa_mks
      real(kind=f) :: temp1, temp2

      real(kind=f) :: r1
      real(kind=f) :: di
      real(kind=f) :: gi
      real(kind=f) :: rlbi
      real(kind=f) :: dti1
      real(kind=f) :: dti2
      real(kind=f) :: dti

      real(kind=f) :: r2
      real(kind=f) :: dj
      real(kind=f) :: gj
      real(kind=f) :: rlbj 
      real(kind=f) :: dtj1
      real(kind=f) :: dtj2 
      real(kind=f) :: dtj 

      real(kind=f) :: rp
      real(kind=f) :: dp
      real(kind=f) :: gg
      real(kind=f) :: delt
      real(kind=f) :: term1
      real(kind=f) :: term2
      real(kind=f) :: cbr

      real(kind=f) :: r_larg
      real(kind=f) :: r_smal
      integer :: i_larg
      integer :: i_smal
      integer :: ig_larg
      integer :: ig_smal

      real(kind=f) :: re_larg
      real(kind=f) :: pe 
      real(kind=f) :: pe3 
      real(kind=f) :: ccd 

      real(kind=f) :: e_coll
      real(kind=f) :: vfc_smal
      real(kind=f) :: vfc_larg 
      real(kind=f) :: sk
      real(kind=f) :: e1
      real(kind=f) :: e3
      real(kind=f) :: e_langmuir
      real(kind=f) :: re60

      real(kind=f) :: pr 
      real(kind=f) :: e_fuchs

      integer :: jp, jj, jr

      real(kind=f) :: pblni
      real(kind=f) :: rblni 

      real(kind=f) :: term3
      real(kind=f) :: term4

      real(kind=f) :: beta
      real(kind=f) :: b_coal
      real(kind=f) :: a_coal 
      real(kind=f) :: x_coal 
      real(kind=f) :: e_coal
      real(kind=f) :: vfc_1
      real(kind=f) :: vfc_2
      real(kind=f) :: cgr

#include "carma_globaer.h"

      allocate(e_coll2(carma%NBIN,carma%NBIN), stat = ios)
      if(ios /= 0) then
       rc = 1
       return
      endif

!  Initialization of input data for gravitational collection.
!  The data were compiled by Hall (J. Atmos. Sci. 37, 2486-2507, 1980).
!
      data data_p/0.00_f,0.05_f,0.10_f,0.15_f,0.20_f,0.25_f,0.30_f,0.35_f, &
      0.40_f,0.45_f,0.50_f,0.55_f,0.60_f,0.65_f,0.70_f,0.75_f,0.80_f,0.85_f, &
      0.90_f,0.95_f,1.00_f/

      data orig_r( 1), (orig_e(ip, 1),ip=1,NP_DATA) /   10.0_f, &
      0.0001_f, 0.0001_f, 0.0001_f, 0.0001_f, 0.0140_f, 0.0170_f, 0.0190_f, &
      0.0220_f, 0.0270_f, 0.0300_f, 0.0330_f, 0.0350_f, 0.0370_f, 0.0380_f, &
      0.0380_f, 0.0370_f, 0.0360_f, 0.0350_f, 0.0320_f, 0.0290_f, 0.0270_f /
      data orig_r( 2), (orig_e(ip, 2),ip=1,NP_DATA) /   20.0_f, &
      0.0001_f, 0.0001_f, 0.0001_f, 0.0050_f, 0.0160_f, 0.0220_f, 0.0300_f, &
      0.0430_f, 0.0520_f, 0.0640_f, 0.0720_f, 0.0790_f, 0.0820_f, 0.0800_f, & 
      0.0760_f, 0.0670_f, 0.0570_f, 0.0480_f, 0.0400_f, 0.0330_f, 0.0270_f /
      data orig_r( 3), (orig_e(ip, 3),ip=1,NP_DATA) /   30.0_f, &
      0.0001_f, 0.0001_f, 0.0020_f, 0.0200_f, 0.0400_f, 0.0850_f, 0.1700_f, &
      0.2700_f, 0.4000_f, 0.5000_f, 0.5500_f, 0.5800_f, 0.5900_f, 0.5800_f, &
      0.5400_f, 0.5100_f, 0.4900_f, 0.4700_f, 0.4500_f, 0.4700_f, 0.5200_f /
      data orig_r( 4), (orig_e(ip, 4),ip=1,NP_DATA) /   40.0_f, &
      0.0001_f, 0.0010_f, 0.0700_f, 0.2800_f, 0.5000_f, 0.6200_f, 0.6800_f, &
      0.7400_f, 0.7800_f, 0.8000_f, 0.8000_f, 0.8000_f, 0.7800_f, 0.7700_f, &
      0.7600_f, 0.7700_f, 0.7700, 0.7800, 0.7900, 0.9500, 1.4000_f /
      data orig_r( 5), (orig_e(ip, 5),ip=1,NP_DATA) /   50.0_f, &
      0.0001_f, 0.0050_f, 0.4000_f, 0.6000_f, 0.7000_f, 0.7800_f, 0.8300_f, &
      0.8600_f, 0.8800_f, 0.9000_f, 0.9000_f, 0.9000_f, 0.9000_f, 0.8900_f, &
      0.8800_f, 0.8800_f, 0.8900, 0.9200, 1.0100, 1.3000, 2.3000 /
      data orig_r( 6), (orig_e(ip, 6),ip=1,NP_DATA) /   60.0_f, &
      0.0001_f, 0.0500_f, 0.4300_f, 0.6400_f, 0.7700_f, 0.8400_f, 0.8700_f, &
      0.8900_f, 0.9000_f, 0.9100_f, 0.9100_f, 0.9100_f, 0.9100_f, 0.9100_f, &
      0.9200_f, 0.9300_f, 0.9500_f, 1.0000_f, 1.0300_f, 1.7000_f, 3.0000_f /
      data orig_r( 7), (orig_e(ip, 7),ip=1,NP_DATA) /   70.0_f, &
      0.0001_f, 0.2000_f, 0.5800_f, 0.7500_f, 0.8400_f, 0.8800_f, 0.9000_f, &
      0.9200_f, 0.9400_f, 0.9500_f, 0.9500_f, 0.9500_f, 0.9500_f, 0.9500_f, &
      0.9500_f, 0.9700_f, 1.0000_f, 1.0200_f, 1.0400_f, 2.3000_f, 4.0000_f /
      data orig_r( 8), (orig_e(ip, 8),ip=1,NP_DATA) /  100.0_f, &
      0.0001_f, 0.5000_f, 0.7900_f, 0.9100_f, 0.9500_f, 0.9500_f, 1.0000_f, &
      1.0000_f, 1.0000_f, 1.0000_f, 1.0000_f, 1.0000_f, 1.0000_f, 1.0000_f, &
      1.0000_f, 1.0000_f, 1.0000_f, 1.0000_f, 1.0000_f, 1.0000_f, 1.0000_f /
      data orig_r( 9), (orig_e(ip, 9),ip=1,NP_DATA) /  150.0_f, &
      0.0001_f, 0.7700_f, 0.9300_f, 0.9700_f, 0.9700_f, 1.0000_f, 1.0000_f, &
      1.0000_f, 1.0000_f, 1.0000_f, 1.0000_f, 1.0000_f, 1.0000_f, 1.0000_f, &
      1.0000_f, 1.0000_f, 1.0000_f, 1.0000_f, 1.0000_f, 1.0000_f, 1.0000_f /
      data orig_r(10), (orig_e(ip,10),ip=1,NP_DATA) /  200.0_f, &
      0.0001_f, 0.8700_f, 0.9600_f, 0.9800_f, 1.0000_f, 1.0000_f, 1.0000_f, &
      1.0000_f, 1.0000_f, 1.0000_f, 1.0000_f, 1.0000_f, 1.0000_f, 1.0000_f, &
      1.0000_f, 1.0000_f, 1.0000_f, 1.0000_f, 1.0000_f, 1.0000_f, 1.0000_f /
      data orig_r(11), (orig_e(ip,11),ip=1,NP_DATA) /  300.0_f, &
      0.0001_f, 0.9700_f, 1.0000_f, 1.0000_f, 1.0000_f, 1.0000_f, 1.0000_f, &
      1.0000_f, 1.0000_f, 1.0000_f, 1.0000_f, 1.0000_f, 1.0000_f, 1.0000_f, &
      1.0000_f, 1.0000_f, 1.0000_f, 1.0000_f, 1.0000_f, 1.0000_f, 1.0000_f /
      data orig_r(12), (orig_e(ip,12),ip=1,NP_DATA) / 1000.0_f, &
      0.0001_f, 1.0000_f, 1.0000_f, 1.0000_f, 1.0000_f, 1.0000_f, 1.0000_f, &
      1.0000_f, 1.0000_f, 1.0000_f, 1.0000_f, 1.0000_f, 1.0000_f, 1.0000_f, &
      1.0000_f, 1.0000_f, 1.0000_f, 1.0000_f, 1.0000_f, 1.0000_f, 1.0000_f /

!
!-------------------------------------------------------------------------------
!
!  Announce entry to this routine
!
!      if( DEBUG ) write(LUNOPRT,'(/,a)') 'Enter setupckern'
#ifdef DEBUG
   write(*,*) '+ setupckern'
#endif

rc = 0

!-------------------------------------------------------------------------------
!
!  Fill <icoag>, maintaining diagonal symmetry

      do jg = 2, NGROUP
        do ig = 1, jg-1
          icoag(ig,jg) = icoag(jg,ig)
        enddo
      enddo

!  <cstick> is the probability that two particles that collide
!  through thermal coagulation will stick to each other.

      cstick = 1._f

!  Use constant kernel if <icoagop> = 0

      if( icoagop .eq. 0 )then
        do iy = 1, NY
          do ix = 1, NX
            ckernel => carma%ckernel(ix,iy)%data5d
            ckernel = ck0
          enddo  ! ix
        enddo  ! iy
        rc = 0
        return   ! Return to caller with coagulation kernels evaluated.
      endif
 
      if( icollec .eq. 2 )then

!   Convert <data_r> from um to m and take logarithm of <data_e>.

        do i = 1, NR_DATA
          data_r(i) = orig_r(i)/1.e6_f
          do ip = 1, NP_DATA
            data_e(ip,i) = log(orig_e(ip,i))
          enddo
        enddo

      endif

! Loop over subdomain
 
      do iy = 1, NY
      do ix = 1, NX

! Point local vars in carma_globaer.h to appropriate data in carma structure
 
      rmu => carma%rmu(ix,iy)%data1d
      bpm => carma%bpm(ix,iy)%data3d
      re => carma%re(ix,iy)%data3d
      vf => carma%vf(ix,iy)%data3d
      ckernel => carma%ckernel(ix,iy)%data5d

!  Loop over vertical layers

      do k = 1, NZ

!  This is <rhoa> in Cartesian coordinates.

        rhoa_mks = rhoa(ix,iy,k) / &
                  (xmet(ix,iy,k)*ymet(ix,iy,k)*zmet(ix,iy,k))

        temp1 = BK*t(ix,iy,k)
        temp2 = 6._f*PI*rmu(k)

!  Loop over groups

        do j1 = 1, NGROUP
        do j2 = 1, NGROUP

        if( icoag(j1,j2) .ne. 0 )then

!  First particle

         do i1 = 1, NBIN

          r1 = r(i1,j1)
          di = temp1*bpm(k,i1,j1)/(temp2*r1)
          gi  = sqrt( 8._f*temp1/(PI*rmass(i1,j1)) )
          rlbi = 8._f*di/(PI*gi)
          dti1= (2._f*r1 + rlbi)**3
          dti2= (4._f*r1*r1 + rlbi*rlbi)**1.5_f
          dti = 1._f/(6._f*r1*rlbi)
          dti = dti*(dti1 - dti2) - 2._f*r1

          do i2 = 1, NBIN

!  Second particle

            r2  = r(i2,j2)
            dj  = temp1*bpm(k,i2,j2)/(temp2*r2)
            gj  = sqrt( 8._f*temp1/(PI*rmass(i2,j2)) )
            rlbj = 8._f*dj/(PI*gj)
            dtj1= (2._f*r2 + rlbj)**3
            dtj2= (4._f*r2*r2 + rlbj*rlbj)**1.5_f
            dtj = 1._f/(6._f*r2*rlbj)
            dtj = dtj*(dtj1 - dtj2) - 2._f*r2

!  First calculate thermal coagulation kernel
  
            rp  = r1 + r2
            dp  = di + dj
            gg  = sqrt(gi*gi + gj*gj)*cstick
            delt= sqrt(dti*dti + dtj*dtj)
            term1 = rp/(rp + delt)
            term2 = 4._f*dp/(gg*rp)

!   <cbr> is thermal (brownian) coagulation coefficient
!
! This equation is found in M. Z. Jacobson et al, 1994, Atmospheric
! Environment, 1327-1338.  It (and all the above terms) comes from N. A.
! Fuchs, "The Mechanics of Aerosols," 1964, p. 294.

            cbr = 4._f*PI*rp*dp/(term1 + term2)
 
!   Determine indices of larger and smaller particles (of the pair)

            if (r2 .ge. r1) then
              r_larg = r2
              r_smal = r1
              i_larg = i2
              i_smal = i1
              ig_larg = j2
              ig_smal = j1
            else
              r_larg = r1
              r_smal = r2
              i_larg = i1
              i_smal = i2
              ig_larg = j1
              ig_smal = j2
            endif
 
!   Calculate enhancement of coagulation due to convective diffusion 
!   as described in Pruppacher and Klett.
!
!   Enhancement applies to larger particle.

            re_larg = re(k,i_larg,ig_larg)

!   <pe> is Peclet number.

            pe  = re_larg*rmu(k) / (rhoa_mks*di)
            pe3 = pe**(1._f/3._f)

!   <ccd> is convective diffusion coagulation coefficient

            if( re_larg .lt. 1._f )then
              ccd = 0.45_f*cbr*pe3
            else 
              ccd = 0.45_f*cbr*pe3*re_larg**(1._f/6._f)
            endif

!   Next calculate gravitational collection kernel.  
!
!   First evaluate collection efficiency <e>.
 
            if( icollec .eq. 0 )then

!   constant value

              e_coll = grav_e_coll0

            else if( icollec .eq. 1 )then

!   Find maximum of Langmuir's formulation and Fuchs' value.
!   First calculate Langmuir's efficiency <e_langmuir>.
!
!   <sk> is stokes number.
!   <vfc_{larg,smal}> is the fallspeed in cartesian coordinates.

              vfc_smal = vf(k,i_smal,ig_smal) * zmet(ix,iy,k)
              vfc_larg = vf(k,i_larg,ig_larg) * zmet(ix,iy,k)

              sk = vfc_smal * (vfc_larg - vfc_smal) / (r_larg*GRAV)
 
              if( sk .lt. 0.08333334_f )then
                e1 = 0._f
              else 
                e1 = (sk/(sk + 0.25_f))**2
              endif
 
              if( sk .lt. 1.214_f )then
                e3  = 0._f
              else
                e3  = 1._f/(1._f+.75_f*log(2._f*sk)/(sk-1.214_f))**2
              endif
 
              if( re_larg .lt. 1._f )then
                e_langmuir = e3
              else if( re_larg .gt. 1000._f )then
                e_langmuir = e1
              else if( re_larg .le. 1000._f )then
                re60 = re_larg/60._f
                e_langmuir = (e3  + re60*e1)/(1._f + re60)
              endif

!   Next calculate Fuchs' efficiency (valid for r < 10 um).

              pr = r_smal/r_larg
              e_fuchs   = (pr/(1.414_f*(1. + pr)))**2

              e_coll = max( e_fuchs, e_langmuir )
 
            else if( icollec .eq. 2 )then

!   Interpolate input data (from data statment at beginning of subroutine).

              pr = r_smal/r_larg
 
              if( pr .lt. data_p(2) )then

!   Radius ratio is smaller than lowest nonzero ratio in input data --
!   use constant values (for smaller droplet, if available)
!   as in Beard and Ochs (1984)

                if( i2 .eq. i_larg )then
                  if( i2.eq.1 )then
                    e_coll = 1.e-4_f
                  else
                    e_coll = e_coll2(i1,i2-1)
                  endif
                else
                  if( i2.eq.1 )then
                    e_coll = 1.e-4_f
                  else
                    e_coll = e_coll2(i1-1,i2)
                  endif
                endif

              else

!   Find <jp> such that data_p(jp) <= pr <= data_p(jp+1)                     

                jp = NP_DATA
                do jj = NP_DATA-1, 2, -1
                  if( pr .le. data_p(jj+1) ) jp = jj
                enddo

!   <pblni> is fractional distance of <pr> between points in <data_p> 

                if( jp .lt. NP_DATA )then
                  pblni = (pr - data_p(jp)) &
                       / (data_p(jp+1) - data_p(jp))
                else
                  pblni = 0._f
                endif
 
                if( r_larg .lt. data_r(1) )then

!   Radius of larger particle is smaller than smallest radius in input data -- 
!   assign very small efficiency.

                  e_coll = 1.e-4_f

                else

!    Find <jr> such that data_r(jr) <= r_larg <= data_r(jr+1)

                  jr = NR_DATA
                  do jj = NR_DATA-1, 1, -1
                    if( r_larg .le. data_r(jj+1) ) jr = jj
                  enddo

!   <rblni> is fractional distance of <r_larg> between points in <data_r> 

                  if( jr .lt. NR_DATA )then
                    rblni = (r_larg - data_r(jr)) &
                         / (data_r(jr+1) - data_r(jr))
                  else
                    rblni = 0._f
                  endif
             
!    Bilinear interpolation of logarithm of data.

                  term1 = (1._f-rblni)*(1._f-pblni)*data_e(jp,jr)
                  if( jp .lt. NP_DATA )then
                    term2 = pblni*(1._f-rblni)*data_e(jp+1,jr)
                  else
                    term2 = -100._f
                  endif
                  if( jr .lt. NR_DATA )then
                    term3 = (1._f-pblni)*rblni*data_e(jp,jr+1)
                  else
                    term3 = -100._f
                  endif
                  if( jr .lt. NR_DATA .and. jp .lt. NP_DATA )then
                    term4 = pblni*rblni*data_e(jp+1,jr+1)
                  else
                    term4 = -100._f
                  endif
    
                  e_coll = exp(term1 + term2 + term3 + term4)

                endif  ! r_larg .lt. data_r(1)
              endif  ! pr .lt. data_p(2)

              e_coll2(i1,i2) = e_coll

            endif  ! icollec == 0, 1, or 2

!  Now calculate coalescence efficiency from Beard and Ochs 
!  (J. Geophys. Res. 89, 7165-7169, 1984).
!  
            beta = log(r_smal*1.e4_f) + 0.44_f*log(r_larg*50._f)
            b_coal = 0.0946_f*beta - 0.319_f
            a_coal = sqrt(b_coal**2 + 0.00441_f)
            x_coal = (a_coal-b_coal)**(1._f/3._f) &
                  - (a_coal+b_coal)**(1._f/3._f)
            x_coal = x_coal + 0.459_f

!  Limit extrapolated values to no less than 50% and no more than 100%

            x_coal = max(x_coal,0.5_f)
            e_coal = min(x_coal,1._f)

!  Now use coalescence efficiency and collision efficiency in definition
!  of (geometric) gravitational collection efficiency <cgr>.

            vfc_1 = vf(k,i1,j1) * zmet(ix,iy,k)
            vfc_2 = vf(k,i2,j2) * zmet(ix,iy,k)
            cgr = e_coal * e_coll *  PI * rp**2 * abs( vfc_1 - vfc_2 )

! JAS: This is legacy code that has not been updated for the new mks unit
! convention.
!
!  Long's (1974) kernel that only depends on size of larger droplet
!
!           if( r_larg .le. 50.e-4 )then
!             cgr = 1.1e10 * vol(i_larg,ig_larg)**2
!           else
!             cgr = 6.33e3 * vol(i_larg,ig_larg)
!           endif

!  Now combine all the coagulation and collection kernels into the
!  overall kernel.
!

            ckernel(k,i1,i2,j1,j2) = cbr + ccd + cgr

! JAS: This is legacy code that has not been updated for the new mks unit
! convention.
!
!  To avoid generation of large, non-physical hydrometeors by
!  coagulation, cut down ckernel for large radii
!
!           if( ( r1 .gt. 0.18 .and. r2 .gt. 10.e-4 ) .or.
!    $          ( r2 .gt. 0.18 .and. r1 .gt. 10.e-4 ) ) then
!              ckernel(k,i1,i2,j1,j2) = ckernel(k,i1,i2,j1,j2) / 1.e6
!           endif

          enddo    ! second particle bin
          enddo    ! first particle bin
         endif     ! icoag ne 0 
        enddo      ! second particle group
        enddo      ! first particle group
      enddo        ! vertical level

      enddo  ! ix
      enddo  ! iy

!     Return to caller with coagulation kernels evaluated

      rc = 0

      deallocate( e_coll2, stat=ios)
      if(ios /= 0) rc = 1

      return
      end
