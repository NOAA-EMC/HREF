      SUBROUTINE do_physics_one_step(deltim,kdt,PHOUR, 
     &                               grid_fld, sfc_fld,
     &                               flx_fld, nst_fld, g3d_fld,
     &                               g2d_fld, aoi_fld, ocn_tmp,
     &                               lats_nodes_r,global_lats_r,
     &                               lonsperlar,XLON,XLAT,COSZDG, 
     &                               HPRIME,SWH,swhc,HLW,hlwc,
!    &                               HTRSWB,HTRLWB,      ! idea add
     &                               FLUXR,SFALB, SLAG,SDEC,CDEC,
     &                               OZPLIN,JINDX1,JINDX2, DDY,
     &                               phy_f3d,  phy_f2d, NBLCK,
     &                               ZHOUR, zhour_dfi,n3, n4, LSOUT,
     &                               COLAT1,CFHOUR1,restart_step)
!!

!!
!! Code Revision:
!! oct 11 2009     Sarah Lu, grid_gr is replaced by grid_fld
!! dec 01 2009     Sarah Lu, add CLDCOV/FCLD check print
!! dec 08 2009     Sarah Lu, add g3d_fld to gloopr calling argument
!! dec 15 2009     Sarah Lu, add g3d_fld to gloopb calling argument;
!!                           add DQDT check print
!! Feb 05 2010     J. Wang, write out restart file
!! Apr 10 2010     Sarah Lu, debug print removed
!! Jul 07 2010     S. Moorthi Added nst_fld and other changes
!! Jul 21 2010     Sarah Lu, output 2d aerosol diag fields
!! Aug 03 2010     Jun Wang, set llsav through ndfi,ldfi
!! Aug 10 2010     Sarah Lu, zerout g2d_fld if needed
!! Aug 25 2010     J. Wang, add half dfi filtered fields output
!! Sep 11 2010     Sarah Lu, g2d_fld zerout call modified
!! Apr 06 2012     Henry Juang, add idea
!! Oct 18 2012     S. Moorthi Added oro_uf and modifications to nst
!! Mar 08 2013     J. Wang, add restart capibility for idea
!! Mar 26 2014     Xingren Wu, add aoi_fld for A/O/I coupling
!! Mar 31 2014     S Moorthi Add ocn_tmp as the input argument and use
!!                           when it contains valid data - for coupled model
!! Jul -- 2014     S Moorthi - merge with GFS, fix init-micro call etc
!! jun    2014     y-t hou,  revised sw sfc spectral component fluxes
!!                           and ocean albedo (no ice contamination) for coupled mdl
!! Sep 16 2014     Shrinivas Moorthi - cleanup and rearrange argumets
!                                       for gloopr and glopb
!! Sep 30 2014     Sarah Lu, remove fscav (the option to compute tracer
!!                           scavenging in GFS is disable)
!!
      USE machine, ONLY: KIND_GRID, KIND_GRID, KIND_RAD, kind_phys
      use resol_def
      use layout1
      use vert_def
      use date_def
      use namelist_physics_def
      use physcons, only : rlapse
      use mpi_def
      use ozne_def
      use gfs_physics_sfc_flx_mod
      use gfs_physics_sfc_flx_set_mod
      use gfs_physics_gridgr_mod,   ONLY: Grid_Var_Data
      use gfs_physics_nst_var_mod,  ONLY: Nst_Var_Data
      use gfs_physics_aoi_var_mod,  ONLY: aoi_var_data
      use gfs_physics_g3d_mod,      ONLY: G3D_Var_Data
      use gfs_physics_g2d_mod,      ONLY: G2D_Var_Data, g2d_zerout
!     use gfs_phy_tracer_config,    ONLY: gfs_phy_tracer_type
      use d3d_def, ONLY: d3d_zero, CLDCOV
! idea add by hmhj
      use module_radsw_parameters,   only : NBDSW
      use module_radlw_parameters,   only : NBDLW
      IMPLICIT NONE
!!     
      TYPE(Sfc_Var_Data)        :: sfc_fld
      TYPE(Flx_Var_Data)        :: flx_fld
      TYPE(Grid_Var_Data)       :: grid_fld 
      TYPE(Nst_Var_Data)        :: nst_fld 
      TYPE(G3D_Var_Data)        :: g3d_fld 
      TYPE(G2D_Var_Data)        :: g2d_fld
      type(aoi_var_data)        :: aoi_fld
!     type(gfs_phy_tracer_type) :: gfs_phy_tracer
!*    REAL(KIND=KIND_GRID)      GRID_GR(lonr*lats_node_r_max,lotgr)
      CHARACTER(16)             :: CFHOUR1
      logical                   :: restart_step
!!     
      REAL(KIND=KIND_EVOD),INTENT(IN)    :: ocn_tmp(lonr,lats_node_r)

      REAL(KIND=KIND_EVOD),INTENT(IN)    :: deltim,PHOUR
      REAL(KIND=KIND_EVOD),INTENT(INOUT) :: ZHOUR,ZHOUR_DFI
!!
      REAL(KIND=KIND_EVOD)  :: delt_cpl  ! xw - add for A/O/I coupling
!!     
      INTEGER n3, n4, nblck
!!
      INTEGER               LATS_NODES_R(NODES)
      integer, dimension(latr) :: global_lats_r, lonsperlar
!!     
      real(kind=kind_evod) colat1, phyhour, phydt, dtp
      real (kind=kind_phys), dimension(lonr,lats_node_r) :: xlon, xlat,
     &                                                     coszdg, sfalb
      real (kind=kind_phys), dimension(ngptc,levs,nblck,lats_node_r) ::
     &                          swh, swhc, hlw, hlwc
      REAL (KIND=KIND_RAD) HPRIME(NMTVR,LONR,LATS_NODE_R),
     &                     FLUXR(nfxr,LONR,LATS_NODE_R)
! idea add by hmhj  - commented by moorthi since unused
!    &,                    HTRSWB(NGPTC,LEVS,NBDSW,NBLCK,LATS_NODE_R)
!    &,                    HTRLWB(NGPTC,LEVS,NBDLW,NBLCK,LATS_NODE_R)

      REAL (kind=kind_phys)
     &     phy_f3d(NGPTC,LEVS,num_p3d+npdf3d,NBLCK,lats_node_r),
     &     phy_f2d(lonr,lats_node_r,num_p2d),
     &     DDY(LATS_NODE_R)

      real(kind=kind_evod) global_times_r(latr,nodes)

      INTEGER JINDX1(LATS_NODE_R),JINDX2(LATS_NODE_R)
      REAL    OZPLIN(LATSOZP,LEVOZP,pl_coeff,timeoz) !OZONE PL Coeff

      REAL(KIND=KIND_EVOD) SLAG,SDEC,CDEC
      INTEGER kdt, IERR,I,J,K,L,LOCL,N,iprint
      LOGICAL LSOUT,ex_out
!
!     real*8 rtc, timer1, timer2

      real (kind=kind_phys) dt_warm, tem1, tem2, omz1
      real (kind=kind_phys), save :: zhour_dfin=0.
!
!     SHOUR   = SHOUR + deltim
      shour   = kdt * deltim
      fhour   = shour / 3600.
      lsfwd   = kdt == 1
!jws
      lssav = .true.
      if(ndfi>0 .and. kdt>ndfi/2 .and. kdt<=ndfi .and. ldfi ) then
        lssav = .false.
      endif
      if(.not.ldfi .and. ndfi>0. and. kdt==ndfi/2+1) then
         zhour = zhour_dfin
      endif
!jwe
      lscca   = mod(KDT ,nsswr) == 0
      lsswr   = mod(KDT ,nsswr) == 1
      lslwr   = mod(KDT ,nslwr) == 1
! test repro
      phyhour = phour + deltim/3600.
      phydt   = deltim
      if(.not. semilag .and. lsfwd) phydt = 0.5*deltim
!
!jw now all the pes are fcst pe
!jw if (.NOT.LIOPE.or.icolor.ne.2) then

      if (nscyc >  0) then
        IF (mod(kdt,nscyc) == 1) THEN
           CALL gcycle(me,LATS_NODE_R,LONSPERLAR,global_lats_r,
     &                ipt_lats_node_r,idate,fhour,fhcyc,
     &                XLON ,XLAT  , sfc_fld, ialb)
        ENDIF
      endif
!
      if (num_p3d  ==  3) then        ! Ferrier Microphysics initialization
      dtp = min(phydt,dtphys)
        call INIT_MICRO(dtp,ngptc,levs,num_p3d,nblck*lats_node_r,
     &                         phy_f3d(1,1,1,1,1), fhour, me)
      endif
!
!-> Coupling insertion

      if (ocn_tmp(1,1) > -99999.0) then
        do j = 1, lats_node_r
          do i = 1, lonr
            if (sfc_fld%slmsk(i,j) < 0.1 .and.
     &          ocn_tmp(i,j) > 150.0) then
                 sfc_fld%TSEA(i,j) = ocn_tmp(i,j)
            endif
          enddo
        enddo
      endif
!<- Coupling insertion

!
      if (nst_fcst > 1) then               ! update tsea with nsst model
        if (ocn_tmp(1,1) > -99999.0) then
          tem1 = 0.5 / omz1
!$omp parallel do private(j,i,tem2,dt_warm)
          do j = 1, lats_node_r
            do i = 1, lonr
              if (sfc_fld%slmsk(i,j) == 0 ) then
                tem2 = 1.0 / nst_fld%xz(i,j)
                sfc_fld%tsea(i,j) = sfc_fld%tsea(i,j)
     &               + (sfc_fld%oro(i,j)-sfc_fld%oro_uf(i,j))*rlapse
                dt_warm = (nst_fld%xt(i,j)+nst_fld%xt(i,j)) * tem2

                if ( nst_fld%xz(i,j) > omz1) then
                  nst_fld%tref(i,j) = sfc_fld%tsea(i,j)
     &             - (1.0-0.5*omz1*tem2) * dt_warm
     &             + nst_fld%z_c(i,j)*nst_fld%dt_cool(i,j)*tem1
                else
                 nst_fld%tref(i,j) = sfc_fld%tsea(i,j)
     &             - (nst_fld%xz(i,j)*dt_warm
     &             -  nst_fld%z_c(i,j)*nst_fld%dt_cool(i,j))*tem1
                endif
                sfc_fld%tsea(i,j) = nst_fld%tref(i,j)
     &              + dt_warm - nst_fld%dt_cool(i,j)
     &              - (sfc_fld%oro(i,j)-sfc_fld%oro_uf(i,j))*rlapse
              endif
            enddo
          enddo
        else
!$omp parallel do private(j,i,dt_warm)
          do j = 1, lats_node_r
            do i = 1, lonr
              if (sfc_fld%slmsk(i,j) == 0 ) then
                dt_warm = (nst_fld%xt(i,j)+nst_fld%xt(i,j))
     &                  /  nst_fld%xz(i,j)
                nst_fld%tref(i,j) = sfc_fld%tsea(i,j)
     &               - dt_warm + nst_fld%dt_cool(i,j)
     &               + (sfc_fld%oro(i,j)-sfc_fld%oro_uf(i,j))*rlapse
              endif
            enddo
          enddo
        endif
      endif

!!

      if (lsswr .or. lslwr) then         ! Radiation Call!
! idea add by hmhj
        if(lsidea) then
          if(lsswr) then
            swh    = 0.
!           htrswb = 0.
          endif
          if(lslwr) then
            hlw    = 0.
!           htrlwb = 0.
          endif
        endif
!*      CALL GLOOPR ( grid_gr,
        CALL GLOOPR ( grid_fld, g3d_fld, aoi_fld, lats_nodes_r,
     &                GLOBAL_LATS_R, LONSPERLAR, phyhour,
     &                deltim, XLON, XLAT, COSZDG, flx_fld%COSZEN,
     &                sfc_fld%SLMSK,  sfc_fld%SNWDPH, sfc_fld%SNCOVR,
     &                sfc_fld%SNOALB, sfc_fld%ZORL,   sfc_fld%TSEA,
     &                HPRIME, SFALB,  sfc_fld%ALVSF,  sfc_fld%ALNSF,
     &                sfc_fld%ALVWF,  sfc_fld%ALNWF,  sfc_fld%FACSF,
     &                sfc_fld%FACWF,  sfc_fld%CV,     sfc_fld%CVT ,
     &                sfc_fld%CVB, SWH, SWHC, HLW, HLWC, flx_fld%SFCNSW,
     &                flx_fld%SFCDLW, sfc_fld%FICE,   sfc_fld%TISFC,
     &                flx_fld%SFCDSW, flx_fld%sfcemis,

     &                flx_fld%TSFLW,  FLUXR, phy_f3d, phy_f2d,
     &                SLAG, SDEC, CDEC, NBLCK, KDT
!    &,               HTRSWB,HTRLWB       !idea add by hmhj
     &                )
!          if (iprint .eq. 1) print*,' me = fin gloopr ',me

      endif
!
!!
!*    call gloopb ( grid_gr,
      call gloopb (grid_fld,     g3d_fld,       sfc_fld,
     &             flx_fld,      aoi_fld,       nst_fld,
     &             lats_nodes_r, global_lats_r, lonsperlar,
     &             phydt,        phyhour,       sfalb,  xlon,
     &             swh,          swhc,          hlw,    hlwc,
!    &             nbdsw,        nbdlw,         HTRSWB, HTRLWB,          !idea add by hmhj
     &             hprime,       slag,          sdec,   cdec,
     &             ozplin,       jindx1,        jindx2, ddy,
     &             phy_f3d,      phy_f2d,       xlat,   nblck,
     &             kdt,          restart_step)
!
!!
!jw      endif !.NOT.LIOPE.or.icolor.ne.2
!--------------------------------------------
!
!      write(0,*)'in do one phys step, lsout=',lsout,'kdt=',kdt, 
!     &   'nszer=',nszer,'ldfi=',ldfi,'ndfi=',ndfi,
!     &   'fhour=',fhour,'zhour=',zhour,'zhour_dfin=',zhour_dfin,
!     &   'zhour_dfi=',zhour_dfi

      if (lsout .and. kdt /= 0 ) then
!WY bug fix.
!-----------
        IF(.NOT. ALLOCATED(SL)) ALLOCATE(SL(levs))
        IF(.NOT. ALLOCATED(SI)) ALLOCATE(SI(levs + 1))
        CALL WRTOUT_physics(phyhour,FHOUR,ZHOUR,IDATE,
     &                      SL,SI,
     &                      sfc_fld, flx_fld, nst_fld, g2d_fld,
     &                      fluxr,
     &                      global_lats_r,lonsperlar,nblck,
!    &                      lats_nodes_r,global_lats_r,lonsperlar,nblck,
     &                      COLAT1,CFHOUR1,pl_coeff,
     &                     'SFC.F','FLX.F','D3D.F')
!
      endif ! if ls_out
!
! A/O/I coupling - changes are needed - for adding the coupling time step
!     Xingren Wu
!
      if (cplflx) then                        ! for NUOPC coupling
        delt_cpl = 3600.*fhout                ! temporary - need change
        if (shour <= deltim) delt_cpl = shour ! temporary - need change
        if (lsout .and. kdt /= 0 ) then       ! temporary - only called at coupling step
          call aoicpl_prep(deltim,delt_cpl,phyhour,fhour,idate,
     &                     aoi_fld,global_lats_r,lonsperlar)
        endif ! if ls_out
      endif   ! if cplflx
!
       IF (kdt > 0 .and. mod(kdt,nsres) == 0) THEN
!           write(0,*)'wrt_restart_physics,kdt=',kdt,'nsres=',nsres
           CALL wrtout_restart_physics(sfc_fld, nst_fld, fhour,idate,
     &                      lats_nodes_r,global_lats_r,lonsperlar,
     &                      phy_f3d, phy_f2d, ngptc, nblck, ens_nam)
       endif
!
      IF (mod(kdt,nszer) == 0 .and. lsout.and.kdt /= 0) THEN
        call flx_init(flx_fld,ierr)
        if(ldfi .and. kdt == ndfi/2) then
         zhour_dfi  = zhour
         zhour_dfin = fhour
        endif
        zhour = fhour
        FLUXR = 0.
!
        if (ldiag3d) then
!         if(me==0) print *, 'LU_CLDCOV: zero out d3d fields'
          call d3d_zero

!         if ( gfs_phy_tracer%doing_GOCART ) then
!           call g2d_zerout(gfs_phy_tracer, g2d_fld)
!         endif

        endif
!
        if ( lgocart ) then
          call g2d_zerout(g2d_fld,ierr)
        endif

      ENDIF
!
      if(ldfi.and.kdt==ndfi) then
         zhour = zhour_dfi
      endif
!     print *,'in phys one,kdt=',kdt,'zhour=',zhour,                   &
!     &  'zhour_dfi=',zhour_dfi,'zhour_dfin=',zhour_dfin
 
      if(ndfi>0 .and. kdt==ndfi .and. ldfi ) then
        ldfi = .false.
      endif
!
      RETURN
      END

      subroutine do_physics_gridcheck(grid_gr,g_pnt,km,
     &                                 global_lats_r,lonsperlar,chr)
      use machine
      use resol_def
      use layout1

      real(kind=kind_grid) grid_gr(lonr*lats_node_r_max,lotgr)
      integer,intent(in):: global_lats_r(latr),g_pnt,km
      integer,intent(in):: lonsperlar(latr)
      character*(*) chr

      integer 	lan,lat,lons_lat,k

      do lan=1,lats_node_r
        lat = global_lats_r(ipt_lats_node_r-1+lan)
        lons_lat = lonsperlar(lat)
!        print *,' gridcheck: lan lat lons_lat ',lan,lat,lons_lat
        do k=1,km
!          print *,' check grid at k=',k
          call mymaxmin(grid_gr(1,g_pnt+k-1),lons_lat,lonr,1,chr)
        enddo
      enddo
 
      return
      end subroutine do_physics_gridcheck
