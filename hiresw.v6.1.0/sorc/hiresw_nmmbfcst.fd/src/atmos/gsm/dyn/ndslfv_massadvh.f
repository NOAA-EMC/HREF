      subroutine ndslfv_massadvh(grid_gr,
     &                           global_lats_a,lonsperlat,deltim)
!
! a routine to do non-iteration semi-Lagrangain finite volume advection
! considering mass advection together with tracer
! contact: hann-ming henry juang
!
! program log:
! 20110220    Henry Juang initiated and implemented into nems for NDSL
! 20130620    Henry Juang correct wind direction for north-south advection
!
!
      use gfs_dyn_machine , only : kind_grid
      use gfs_dyn_resol_def
      use gfs_dyn_layout1
      use gfs_dyn_vert_def
      use gfs_dyn_coordinate_def
      use gfs_dyn_physcons, kappa => con_rocp
      use gfs_dyn_mpi_def
      implicit none

      real(kind=kind_grid) grid_gr(lonf*lats_node_a_max,lotgr)
      integer,intent(in):: global_lats_a(latg)
      integer,intent(in):: lonsperlat(latg)
      real,   intent(in):: deltim
   
      real (kind=kind_grid), parameter :: rkappa=1.0/kappa

      real	uulon(lonfull,levs,latpart)
      real	vvlon(lonfull,levs,latpart)
      real	qqlon(lonfull,levs*ndslhvar,latpart)
      real	rrlon(lonfull,levs*ndslhvar,latpart)

      real	vvlat(latfull,levs,lonpart)
      real	qqlat(latfull,levs*ndslhvar,lonpart)
      real	rrlat(latfull,levs*ndslhvar,lonpart)

      logical 	lprint

      integer nvars,nlevs
      integer ilan,i,j,n,k,kk,lon,lan,lat,lons_lat,lon_dim,jlonf,irc
      integer k00, kdp, kqq, ktt, kuu, kvv
      integer k0 , kp , kq , kt , ku , kv
      integer kpg, kqg, ktg, kug, kvg
!
      lprint = .true.

      if( lprint ) then
        print *,' enter ndslfv_advect  with mass conservation '
      endif
!
      nvars = ndslhvar
      nlevs = levs*nvars
      k00 = 1
      kdp = k00 + levs
      kqq = kdp + levs
      ktt = kqq + levh
      kuu = ktt + levs
      kvv = kuu + levs
      
!
! =================================================================
!   prepare wind and variable in flux form with gaussina weight
! =================================================================
!
      do lan=1,lats_node_a

        lat = global_lats_a(ipt_lats_node_a-1+lan)
        lon_dim = lon_dims_a(lan)
        lons_lat = lonsperlat(lat)
        jlonf = (lan-1)*lonf
!
! wind at time step n
        do k=1,levs
          kug=g_uu+k-1
          kvg=g_vv+k-1
          do i=1,lons_lat
            ilan=i+jlonf
            uulon(i,k,lan) = grid_gr(ilan,kug)
            vvlon(i,k,lan) = grid_gr(ilan,kvg)
          enddo
        enddo
        if( lprint ) then
          call mymaxmin(uulon(1,1,lan),lons_lat,lonfull,1,' uu ')
          call mymaxmin(vvlon(1,1,lan),lons_lat,lonfull,1,' vv ')
        endif
!
! unit variable for divergence
        do k=1,levs
          do i=1,lons_lat
            qqlon(i,k,lan) = 1.0
          enddo
        enddo
!
! density (pressure layer thickness)  at time step n-1
        do k=1,levs
          kp=kdp+k-1
          kpg=g_dpm+k-1
          do i=1,lons_lat
            ilan=i+jlonf
            qqlon(i,kp,lan) = grid_gr(ilan,kpg) 
          enddo
        enddo
        if( lprint ) then
          call mymaxmin(qqlon(1,1,lan),lons_lat,lonfull,1,' dp ')
        endif

! density * tracer at time n-1
        if( nvars > 1+ntrac ) then
        do k=1,levh
          kp=kdp+mod(k-1,levs)
          kq=kqq+k-1
          kqg=g_rm+k-1
          do i=1,lons_lat
            ilan=i+jlonf
            qqlon(i,kq,lan) = grid_gr(ilan,kqg)*qqlon(i,kp,lan)
          enddo
        enddo
        endif
        if( lprint ) then
        call mymaxmin(qqlon(1,kqq,lan),lons_lat,lonfull,1,' qdp ')
        endif

! density * enthalpy at time n-1
        if( nvars > 2+ntrac ) then
        do k=1,levs
          kp=kdp+k-1
          kt=ktt+k-1
          ktg=g_ttm+k-1
          do i=1,lons_lat
            ilan=i+jlonf
            qqlon(i,kt,lan) = grid_gr(ilan,ktg)*qqlon(i,kp,lan)
          enddo
        enddo
        endif

! density * momentum at time n-1
        if( nvars > 3+ntrac ) then
        do k=1,levs
          kp=kdp+k-1
          ku=kuu+k-1
          kug=g_uum+k-1
          kv=kvv+k-1
          kvg=g_vvm+k-1
          do i=1,lons_lat
            ilan=i+jlonf
            qqlon(i,ku,lan) = grid_gr(ilan,kug)*qqlon(i,kp,lan)
            qqlon(i,kv,lan) = grid_gr(ilan,kvg)*qqlon(i,kp,lan)
          enddo
        enddo
        endif

        if( lprint ) then
        print *,' ------------------------------------------- '
        call mymaxmin(qqlon(1,kdp,lan),lons_lat,lonfull,1,' red  dp ')
        call mymaxmin(qqlon(1,kqq,lan),lons_lat,lonfull,1,' red qdp ')
        endif

        call cyclic_cell_intpx(levs,lons_lat,lonf,uulon(1,1,lan))
        call cyclic_cell_intpx(levs,lons_lat,lonf,vvlon(1,1,lan))
        call cyclic_cell_intpx(nlevs,lons_lat,lonf,qqlon(1,1,lan))

        if( lprint ) then
        print *,' ------------------------------------------- '
        call mymaxmin(qqlon(1,kdp,lan),lonfull,lonfull,1,' full  dp ')
        call mymaxmin(qqlon(1,kqq,lan),lonfull,lonfull,1,' full qdp ')
        endif

        rrlon(:,:,lan) = qqlon(:,:,lan)
!
! first set positive advection in east-west direction with mass conserving
!
        call cyclic_cell_massadvx(lat,levs,nvars,deltim,                    &
     &                   uulon(1,1,lan),rrlon(1,1,lan),1)

        if( lprint ) then
        print *,' done cyclic_cell_advectx '
        print *,' ------------------------------------------- '
        call mymaxmin(rrlon(1,kdp,lan),lonfull,lonfull,1,' 1st x  dp ')
        call mymaxmin(rrlon(1,kqq,lan),lonfull,lonfull,1,' 1st x qdp ')
        print *,' done the first x adv for lan=',lan
        print *,' =========================================== '
        endif
 
      enddo

! ---------------------------------------------------------------------
! mpi para from east-west full grid to north-south full grid
! ---------------------------------------------------------------------
!
! para vvlon, qqlon, and rrlon to vvlat, qqlat, rrlat

!      print *,' ndslfv_advect transport from we to ns '
       call para_we2ns(vvlon,vvlat,levs,global_lats_a,latg)
       call para_we2ns(qqlon,qqlat,nlevs,global_lats_a,latg)
       call para_we2ns(rrlon,rrlat,nlevs,global_lats_a,latg)
!
! ---------------------------------------------------------------------
! -------------- in north-soutn great circle -------------------
! ---------------------------------------------------------------------

       do lon=1,mylonlen
! 
!       print *,' lon=',lon

! convert wind before advy
        do k=1,levs
          ku=kuu+k-1
          kv=kvv+k-1
          do j=1,latfull/2
            rrlat(j,ku,lon) = -rrlat(j,ku,lon)
            rrlat(j,kv,lon) = -rrlat(j,kv,lon)
            qqlat(j,ku,lon) = -qqlat(j,ku,lon)
            qqlat(j,kv,lon) = -qqlat(j,kv,lon)
          enddo
        enddo
!
! first set advection in north-south direction in great circle through two poles
!
        call cyclic_cell_massadvy(levs,nvars,deltim,                        &
     &                   vvlat(1,1,lon),rrlat(1,1,lon),1)

        if( lprint ) then
        print *,' ------------------------------------------- '
        call mymaxmin(rrlat(1,kdp,lan),latfull,latfull,1,' 1st y  dp ')
        call mymaxmin(rrlat(1,kqq,lan),latfull,latfull,1,' 1st y qdp ')
        endif
!
! second set advection in north-south direction in great circle through two poles
!
        call cyclic_cell_massadvy(levs,nvars,deltim,                        &
     &                   vvlat(1,1,lon),qqlat(1,1,lon),1)

        if( lprint ) then
        print *,' ------------------------------------------- '
        call mymaxmin(qqlat(1,kdp,lan),latfull,latfull,1,' 2nd y  dp ')
        call mymaxmin(qqlat(1,kqq,lan),latfull,latfull,1,' 2nd y qdp ')
        print *,' done with y at lon=',lon
        endif
!
! convert wind back after advy
        do k=1,levs
          ku=kuu+k-1
          kv=kvv+k-1
          do j=1,latfull/2
            rrlat(j,ku,lon) = -rrlat(j,ku,lon)
            rrlat(j,kv,lon) = -rrlat(j,kv,lon)
            qqlat(j,ku,lon) = -qqlat(j,ku,lon)
            qqlat(j,kv,lon) = -qqlat(j,kv,lon)
          enddo
        enddo

      enddo

! ----------------------------------------------------------------------
! mpi para from north-south direction to east-west direeectory 
! ----------------------------------------------------------------------
!
! para qqlat and rrlat to qqlon and rrlon
!      print *,' ndslfv_advect transport from ns to we '
       call para_ns2we(qqlat,qqlon,nlevs,global_lats_a,latg)
       call para_ns2we(rrlat,rrlon,nlevs,global_lats_a,latg)

! ---------------------------------------------------------------
! ---------------- back to east-west direction ------------------
! ---------------------------------------------------------------
!      print *,' ndslfv_advect adv loop in x for last '

      do lan=1,lats_node_a

        lat = global_lats_a(ipt_lats_node_a-1+lan)
        lons_lat = lonsperlat(lat)
        jlonf = (lan-1)*lonf

!
! second set advection in x for the second of the pair
!
        call cyclic_cell_massadvx(lat,levs,nvars,deltim,                    &
     &                   uulon(1,1,lan),qqlon(1,1,lan),1)
        
        if( lprint ) then
        print *,' ------------------------------------------- '
        call mymaxmin(qqlon(1,kdp,lan),lonfull,lonfull,1,' 2nd x  dp ')
        call mymaxmin(qqlon(1,kqq,lan),lonfull,lonfull,1,' 2nd x qdp ')
        endif

        do k=1,nlevs
          do i=1,lonfull
            qqlon(i,k,lan) = 0.5 * ( qqlon(i,k,lan) + rrlon(i,k,lan) )
          enddo
        enddo

        if( lprint ) then
        print *,' ------------------------------------------- '
        call mymaxmin(qqlon(1,kdp,lan),lonfull,lonfull,1,'  dp full ')
        call mymaxmin(qqlon(1,kqq,lan),lonfull,lonfull,1,' qdp full ')
        endif
!
! mass conserving interpolation from full grid to reduced grid
!
        call cyclic_cell_intpx(nlevs,lonf,lons_lat,qqlon(1,1,lan))

        if( lprint ) then
        print *,' ------------------------------------------- '
        call mymaxmin(qqlon(1,kdp,lan),lons_lat,lonfull,1,'  dp redu ')
        call mymaxmin(qqlon(1,kqq,lan),lons_lat,lonfull,1,' qdp redu ')
        print *,' finish horizonatal advection at lan=',lan
        endif

! u*dp and v*dp at n+1* 
        if( nvars > 3 + ntrac ) then
        do k=1,levs
          ku=kuu+k-1
          kug=g_u +k-1
          kv=kvv+k-1
          kvg=g_v +k-1
          do i=1,lons_lat
            ilan=i+jlonf
            grid_gr(ilan,kug) = qqlon(i,ku,lan)
            grid_gr(ilan,kvg) = qqlon(i,kv,lan)
          enddo
        enddo
        endif

! t*dp at n+1* 
        if( nvars > 2 + ntrac ) then
        do k=1,levs
          kt=ktt+k-1
          ktg=g_t +k-1
          do i=1,lons_lat
            ilan=i+jlonf
            grid_gr(ilan,ktg) = qqlon(i,kt,lan) 
          enddo
        enddo
        endif

! rq*dp at n+1* 
        if( nvars > 1 + ntrac ) then
        do k=1,levh
          kq=kqq+k-1
          kqg=g_rt+k-1
          do i=1,lons_lat
            ilan=i+jlonf
            grid_gr(ilan,kqg) = qqlon(i,kq,lan)
          enddo
        enddo
        endif

!  dp at n+1*  has only follow Lagrangian advection, 
!              not yet follow coordinate definition
        do k=1,levs
          kp=kdp+k-1
          kpg=g_dpn+k-1
          do i=1,lons_lat
            ilan=i+jlonf
            grid_gr(ilan,kpg) = qqlon(i,kp,lan) 
          enddo
        enddo
!
!  2d at n+1*  has only follow Lagrangian advection, 
!      
!       do k=1,levs
!         kp=kdp+k-1
!         kpg=g_p+k-1 		! temporary used for dp due to advection
!         do i=1,lons_lat
!           ilan=i+jlonf
!           grid_gr(ilan,kpg) = qqlon(i,kp,lan) 
!    &    - grid_gr(ilan,g_dp+k-1)*(qqlon(i,k,lan)-1.0)
!         enddo
!       enddo
!
        if( lprint ) then
        print *,' ------------------------------------------- '
        print *,' finish updating n+1* in grid_gr at lan=',lan
        endif
!
      enddo
!
! 
! ===============================
!
      return
      end
