! PRC/JAS -- 5/11/2007
! This routine receives input from the host model coupler, allocates and
! fills in the carma type (carma_create), calls the carma model, and
! then cleans itself up (carma_destroy).
! Input are flat arrays from the host model coupler, so not special types
! of the host model sit here.  The carma type is introduced here because
! after create we pass this to the carma call.

  module carma_main_mod

  public carma_main

  contains


      subroutine carma_main( NX, NY, NZ                           &
                           , NGROUP, NELEM, NBIN, NGAS            &
                           , do_hostmodel                         &
                           , igridv, igridh                       &
                           , dtime                                &
                           , t, t_surf                            &
                           , rhoa, delp, relhum                   &
                           , p_surf, p_top                        &
                           , dxfix, dyfix, dzfix                  &
                           , dlon, dlat                           &
                           , dom_llx, dom_urx                     &
                           , dom_lly, dom_ury                     &
                           , q, r, rlow, rup, rhop                & 
                           , rmin, rmrat                          &
                           , do_coag, do_vtran                    &
                           , ifall, rhflag                        &
                           , rc )

      use carma_types_mod

      implicit none

!     Inputs

      integer :: NX, NY, NZ
      integer :: NGROUP, NELEM, NBIN, NGAS
      integer, parameter :: NSOLUTE = 0

!     Optional Input/Output

      logical, optional :: do_hostmodel
      integer, optional :: igridv, igridh
      real(kind=f), optional :: dtime
      real(kind=f), optional, dimension( nx, ny, nz ) :: t
      real(kind=f), optional, dimension( nx, ny )     :: t_surf
      real(kind=f), optional, dimension( nx, ny, nz ) :: rhoa, delp, relhum
      real(kind=f), optional, dimension( nx, ny )     :: p_surf, p_top
      real(kind=f), optional :: dxfix, dyfix, dzfix
      real(kind=f), optional :: dlon, dlat
      real(kind=f), optional :: dom_llx, dom_urx
      real(kind=f), optional :: dom_lly, dom_ury
      real(kind=f), optional, dimension( NX, NY, NZ, NBIN, NELEM ) :: q
      real(kind=f), optional, dimension( nbin, ngroup ) :: r, rlow, rup, rhop
      real(kind=f), optional, dimension( ngroup )       :: rmin, rmrat
      logical, optional :: do_coag, do_vtran
      integer, optional :: ifall, rhflag
      integer, optional :: rc

!     Output

!     Local vars

      type( carmatype ) :: carma
      logical :: doing_hostmodel
      integer :: ibin, ielem

!     Executable code

#ifdef DEBUG
      write(*,*) '+ carma_main'
#endif
      rc = 0

!     Is this a do_hostmodel call?
      doing_hostmodel = .false.
      if(present(do_hostmodel)) doing_hostmodel = do_hostmodel

      if(doing_hostmodel) then

       call carma_create(  NX, NY, NZ                                   &
                         , NGROUP, NELEM, NBIN, NGAS, NSOLUTE           &
                         , carma                                        &
                         , do_hostmodel=.true.                          &
                         , igridv=I_SIG, igridh=I_LL                    &
                         , dtime=dtime                                  &
                         , t=t, rhoa=rhoa, t_surf=t_surf, p_surf=p_surf &
                         , p_top=p_top, delp=delp, relhum=relhum        &
                         , dlon = dlon, dlat = dlat                     &
                         , dom_llx = dom_llx, dom_urx = dom_urx         &
                         , dom_lly = dom_lly, dom_ury = dom_ury         &
                         , q=q                                          &
                         , do_coag=do_coag, do_vtran=do_vtran           &
                         , ifall=ifall, rhFlag=rhFlag                   &
                         , r=r, rlow=rlow, rup=rup, rhop=rhop           &
                         , rmin=rmin, rmrat=rmrat                       &
                         , rc=rc )
      else
       call carma_create(  NX, NY, NZ                                   &
                         , NGROUP, NELEM, NBIN, NGAS, NSOLUTE           &
                         , carma, rc=rc)
      endif

      if(rc /= 0) then
       rc = 1000+rc
       return
      endif

      call init( carma, rc )
      if(rc /= 0) then
       rc = 2000+rc
       return
      endif

      call step( carma, rc )
      if(rc /= 0) then
       rc = 3000+rc
       return
      endif

!      PRC: quit is not implemented at present
!      call quit( carma, rc )

      if(doing_hostmodel) then
       call carma_destroy( NX, NY, NZ &
                         , NGROUP, NELEM, NBIN, NGAS &
                         , NSOLUTE &
                         , carma &
                         , t=t, rhoa=rhoa, delp=delp &
                         , q=q &
                         , rc=rc )
      else
       call carma_destroy( NX, NY, NZ &
                         , NGROUP, NELEM, NBIN, NGAS &
                         , NSOLUTE &
                         , carma, rc=rc )
      endif

      if(rc /= 0) then
       rc = 5000+rc
       return
      endif

      end subroutine

  end module carma_main_mod
