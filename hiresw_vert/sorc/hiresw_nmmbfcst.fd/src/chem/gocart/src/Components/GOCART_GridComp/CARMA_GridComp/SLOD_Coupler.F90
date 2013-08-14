! PRC/JAS -- 5/11/2007
! The purpose of the SLOD_Coupler (s/b renamed, PRC thinks)
! is to unwind the data structures from the host model and put
! everything into simple arrays suitable for passing to CARMA

! SLOD and SLOD_Coupler can handle NGROUP > 1 and NELEM > 1,
! but q-mapping is not ready is carma_create and carma_destroy.  -JAS

! Extensive revision of the SLOD_Coupler - PRC
! The intention is that the SLOD_Coupler is called from the hostmodel
! or the SLOD, either one of which needs to provide enough information to
! drive the underlying CARMA code.
! As this coupler is implemented now it requires as input information about
! the grid (spatial, bin/group/element/gas).
! Optional arguments are for coupling to a host model.  Right now we assume
! the GEOS-4 format ChemBundle w_c is provided by the host model.  This
! contains:
!  1) the tracer array qa
!  2) information about the geographic grid (lon, lat, dlon, lat)
!  3) information about the vertical grid (delp, p_top)
! If w_c is passed in then it is assumed we are doing a host model run and
! the following other optional parameters become mandatory:
!  t (temperature), rhoa (air density), t_sfc (surface temperature),
!  p_surf (surface pressure), rhop (particle density), dtime (timstep)
! and one set of either
!  r, rlow, rup
! or
!  rmin, rmrat
! to specify the particle bin spacing.
! The following arguments remain optional:
!  fluxout (and output flux container), ifall (not implemented, to specify use 
!  of a fixed fall velocity), rhflag (not implemented, but a way to apply an 
!  equilibrium RH correction to particle velocity calculation), and
!  rc (return error code)


      subroutine SLOD_Coupler( i1, i2, j1, j2, km, nbeg, nend   &
                                     , NGROUP, NELEM, NBIN, NGAS        &
                                     , w_c, dtime &
                                     , t, rhoa &
                                     , t_sfc, p_surf &
                                     , ifall, rhflag                    &
                                     , r, rlow, rup  &
                                     , rmin, rmrat &
                                     , rhop, fluxout &
                                     , rc )

      use SLOD_ChemMod
      use carma_main_mod

      implicit none

!     Inputs (required)
      integer :: i1, i2, j1, j2, km, nbeg, nend
      integer :: NGROUP, NELEM, NBIN, NGAS

!     Optional I/O
      type(Chem_Bundle), optional                        :: w_c
      real(kind=f), optional                             :: dtime
      real(kind=f), dimension(i1:i2,j1:j2,km), optional  :: t, rhoa
      real(kind=f), dimension(i1:i2,j1:j2), optional     :: t_sfc, p_surf
      integer, optional                                  :: ifall, rhflag
      real(kind=f), dimension( NBIN, NGROUP ), optional  :: r, rlow, rup, rhop
      real(kind=f), dimension( NGROUP ), optional        :: rmin, rmrat
      type(Chem_Array), pointer, dimension(:), optional  :: fluxout
      integer, optional                                  :: rc

!     Local variables
      integer :: i, j, k, ibin, ielem, n
      integer :: NX, NY, NZ
      real(kind=f), dimension( i1:i2, j1:j2, km, NBIN, NELEM ) :: q_array
      real(kind=f), dimension(km)            :: delp
      real(kind=f) :: dlon, dlat, dom_llx, dom_urx, dom_lly, dom_ury 
      real(kind=f), dimension(i1:i2,j1:j2)    :: p_top
      logical :: do_hostmodel, do_vtran, do_coag
      real(kind=f), parameter :: grav = 9.806_f
      real(kind=f), dimension(NBIN,NELEM) :: cm0, cm1

!  Define values of symbols used to specify horizontal & vertical grid type.
!  A subset of what is potentially allowed in CARMA, this reflects the grids
!  currently possible and for which some action will be taken in a host model
!  request.
!   Possible values for igridv:  I_CART    cartesian,  I_SIG     sigma
!   Possible values for igridh:  I_CART    cartesian   I_LL     longitude_latitude
      integer, parameter :: I_CART = 1
      integer, parameter :: I_SIG = 2
      integer, parameter :: I_LL = 3

!     Are we doing a host model run?  Check inputs
      do_hostmodel = .false.
      if(present(w_c)) then
       do_hostmodel = .true.
!      assume the Chem_Bundle is correct right now
       rc = 0
       if(.not.present(dtime)) rc = rc + 1
       if(.not.present(t)) rc = rc + 2
       if(.not.present(rhoa)) rc = rc + 4
       if(.not.present(t_sfc)) rc = rc + 8
       if(.not.present(p_surf)) rc = rc + 16
       if(.not.present(r) .and. .not.present(rmin)) rc = rc + 32
       if(present(r) .and. present(rmin)) rc = rc + 64
       if(present(r) .and. .not.(present(rlow) .and. present(rup))) rc = rc + 128
       if(present(rmin) .and. .not.present(rmin)) rc = rc + 256
       if(.not.present(rhop)) rc = rc + 512
       if(rc /= 0) return
      endif

!     Set up the do_vtrans and do_coag variables
      do_vtran = .false.
      do_coag = .false.
#if defined( FALLTEST) || defined(SSFALLTEST)
      do_vtran = .true.
#endif
#ifdef COAGTEST
      do_coag = .true.
#endif
#if defined(BCOC) || defined(CLDICE)
      do_coag = .true.
#endif


!     We call CARMA like a column model for now
      NX = 1
      NY = 1
      NZ = km

!     Loop over horizontal spatial points and call CARMA
      do j = j1, j2
       do i = i1, i2

        if(do_hostmodel) then
         do ielem = 1, NELEM
           do ibin = 1, NBIN
             q_array(i,j,:,ibin,ielem) = &
             w_c%qa( nbeg + ( ielem - 1 ) * NBIN + ibin - 1 )%data3d(i,j,:)
           enddo
         enddo
         delp = w_c%delp(i,j,:)
         p_top = w_c%grid%ptop
         dlon = w_c%grid%lon_del
         dlat = w_c%grid%lat_del
         dom_llx = w_c%grid%lon(i)-dlon/2._f
         dom_urx = w_c%grid%lon(i)+dlon/2._f
         dom_lly = w_c%grid%lat(j)-dlat/2._f
         dom_ury = w_c%grid%lat(j)+dlat/2._f
        endif

!       If present, save the initial column value of tracer in fluxout
        if(do_hostmodel .and. present(fluxout)) then
         if(associated(fluxout)) then
          cm0 = 0._f
          cm1 = 0._f
          do n = nbeg, nend
           fluxout(n)%data2d(i,j) = 0._f
          enddo
          do k = 1, km
           do ielem = 1, NELEM
            do ibin = 1, NBIN
             n = nbeg + ( ielem - 1 ) * NBIN + ibin - 1
             cm0(ibin,ielem) = cm0(ibin,ielem) + &
              w_c%delp(i,j,k) * w_c%qa(n)%data3d(i,j,k)/grav
            enddo
           enddo
          enddo
         endif
        endif

        call carma_main( NX, NY, NZ                                    &
             , NGROUP, NELEM, NBIN, NGAS                               &
             , do_hostmodel = do_hostmodel                             &
             , igridv = I_SIG, igridh = I_LL                           &
             , dtime = dtime                                           &
             , t = t(i,j,:), t_surf = t_sfc(i,j)                       &
             , rhoa = rhoa(i,j,:), delp = delp                         &
             , relhum = w_c%rh(i,j,:)                                  &
             , p_surf = p_surf(i,j), p_top = p_top(i,j)                &
             , dlon = dlon, dlat = dlat                                &
             , dom_llx = dom_llx, dom_urx = dom_urx                    &
             , dom_lly = dom_lly, dom_ury = dom_ury                    &
             , q = q_array(i,j,:,:,:), r = r, rlow = rlow, rup = rup   &
             , rmin = rmin, rmrat = rmrat                              &
             , rhop = rhop                                             &
             , do_vtran=do_vtran, do_coag=do_coag                      &
             , rhflag = rhFlag                                         &
             , rc = rc )

        if(rc /= 0) then
         write(*,*) 'SLOD_Coupler: carma_main rc = ', rc
         stop
        endif

        do ielem = 1, NELEM
          do ibin = 1, NBIN
            w_c%qa( nbeg + ( ielem - 1 ) * NBIN + ibin - 1 )%data3d(i,j,:) = &
                                                  q_array(i,j,:,ibin,ielem)
          enddo
        enddo

!       If present, update the fluxout
        if(do_hostmodel .and. present(fluxout)) then
         if(associated(fluxout)) then
          do k = 1, km
           do ielem = 1, NELEM
            do ibin = 1, NBIN
             n = nbeg + ( ielem - 1 ) * NBIN + ibin - 1
             cm1(ibin,ielem) = cm1(ibin,ielem) + &
              w_c%delp(i,j,k) * w_c%qa(n)%data3d(i,j,k)/grav
            enddo
           enddo
          enddo
          do ielem = 1, NELEM
           do ibin = 1, NBIN
            n = nbeg + ( ielem - 1 ) * NBIN + ibin - 1
            fluxout(n)%data2d(i,j) = cm0(ibin,ielem)-cm1(ibin,ielem)
            fluxout(n)%data2d(i,j) = fluxout(n)%data2d(i,j)/dtime
           enddo
          enddo
         endif
        endif

       end do
      end do

      end subroutine
