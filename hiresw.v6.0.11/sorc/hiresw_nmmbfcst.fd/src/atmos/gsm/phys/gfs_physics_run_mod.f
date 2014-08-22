!
! !module: gfs_physics_run_mod --- run module of the grided
!                              component of the gfs physics.
!
! !description: gfs run module.
!
! !revision history:
!
!  november 2004      weiyu yang initial code.
!  may      2005      weiyu yang, updated to the new version of gfs.
!  janusry  2007      hann-ming henry juang for gfs dynamics only
!  july     2007      shrinivas moorthi for gfs physics only
!  november 2007      hann-ming henry juang continue for gfs physics
!  october  2009      jun wang add nsout option
!  oct 11   2009      sarah lu, grid_gr replaced by grid_fld
!  oct 17   2009      sarah lu, q is replaced by tracers(1)
!  dec 08   2009      sarah lu, add g3d_fld to do_physics_one_step
!                     calling argument
!  July     2010      Shrinivas Moorthi - Updated for new physics and added nst
!                     eliminated calls to common_vars
!  jul 21  2010       sarah lu, add g2d_fld to do_physics_one_step
!                     calling argument
!  Aug 03  2010       jun wang, fix lsout for dfi
!  Aug 25  2010       Jun Wang, add zhour_dfi for filtered dfi fields output
!  Oct 18  2010       s. moorthi added fscav to do tstep
!  Dec 23  2010       Sarah Lu, setup fscav from gfs_phy_tracer 
!  Nov 27  2011       Sarah Lu, zerout fcld, dqdt, and wet1
!  Apr 06  2012       Henry Juang, add idea
!  Apr 09  2012       Jun Wang save phys state at 3hr and set back to 
!                     3hr phys state aft dfi
!  Mar 09  2013       Jun Wang add restart step for idea
!
! !interface:
!
      module gfs_physics_run_mod
!
!!uses:
!
      use gfs_physics_internal_state_mod, ONLY: gfs_physics_internal_state
      USE date_def,                       ONLY: fhour
      USE namelist_physics_def,           ONLY: nsout,ldfi,ndfi
      use gfs_phy_tracer_config,          ONLY: gfs_phy_tracer

      implicit none

      contains

      subroutine gfs_physics_run(gis_phy, rc)

      type(gfs_physics_internal_state), pointer, intent(inout) :: gis_phy
      integer, optional,                          intent(out)   :: rc

      real , save	:: timestep=0.0
      integer		rc1, k , i1, i2, i3

!***********************************************************************
!
!     lsfwd      logical true during a first forward step
!     lssav      logical true during a step for which
!                diagnostics are accumulated
!     lscca      logical true during a step for which convective clouds
!                are calculated from convective precipitation rates
!     phour      real forecast hour at the end of the previous timestep
!
!     lsout controls freq. of output
!     nsout controls freq. of output in time steps
!     fhout controls freq. of output in hours
!     nszer time steps between zeroing fluxes
!
!***********************************************************************

!       print *,' enter gfs_physics_run '
!
       rc1 = 0
!
!      print *,' uug=',gis_phy%grid_gr(1,gis_phy%g_u:gis_phy%g_u+gis_phy%levs-1)
!      print *,' pg=',gis_phy%grid_gr(1,gis_phy%g_p:gis_phy%g_p+gis_phy%levs-1)
!      print *,' dpg=',gis_phy%grid_gr(1,gis_phy%g_dp:gis_phy%g_dp+gis_phy%levs-1)
!      call common_to_physics_vars(gis_phy%grid_fld%ps,      &
!                                  gis_phy%grid_fld%t ,      &
!*                                 gis_phy%grid_fld%tracers(1)%flds ,  &
!                                  gis_phy%grid_fld%u ,      &
!                                  gis_phy%grid_fld%v ,      &
!                                  gis_phy%grid_fld%p ,      &
!                                  gis_phy%grid_fld%dp ,     &
!                                  gis_phy%grid_fld%dpdt ,   &
!                                  gis_phy%global_lats_r,                &
!                                  gis_phy%lonsperlar)

!      print *,' end of common_to_physics_vars '

!
! ---------------------------------------------------------------------
! ======================================================================
!                     do one physics time step
! ---------------------------------------------------------------------
       if (.not.ldfi) then
        gis_phy%LSOUT = MOD(gis_phy%kdt ,NSOUT).EQ.0  .or. gis_phy%kdt==1
       else
        gis_phy%LSOUT = (MOD(gis_phy%kdt ,NSOUT).EQ.0 .and.              &
           (gis_phy%kdt<=ndfi/2.or.gis_phy%kdt>ndfi)).or. gis_phy%kdt==1
       endif
!
!       print *,' end of common_to_physics_vars,kdt=',gis_phy%kdt,      &
!         'nsout=',nsout,'lsout=',gis_phy%LSOUT,'zhour=',gis_phy%ZHOUR,   &
!         'ldfi=',ldfi,'ndfi=',ndfi,gis_phy%kdt<=ndfi/2,gis_phy%kdt>ndfi, &
!             gis_phy%kdt<=ndfi/2.or.gis_phy%kdt>ndfi
!       if(gis_phy%kdt==12.and.gis_phy%kdt<=13.or.gis_phy%kdt>=24.and.gis_phy%kdt<=25) then
!       print *,'be phys one,kdt=',gis_phy%kdt,'ps=',maxval(gis_phy%grid_fld%ps), &
!        minval(gis_phy%grid_fld%ps),'t=',maxval(gis_phy%grid_fld%t), &
!        minval(gis_phy%grid_fld%t),'spfh=',maxval(gis_phy%grid_fld%tracers(1)%flds),  &
!        minval(gis_phy%grid_fld%tracers(1)%flds),'tsea=',maxval(gis_phy%sfc_fld%tsea),&
!        minval(gis_phy%sfc_fld%tsea),maxloc(gis_phy%sfc_fld%tsea),maxloc(gis_phy%grid_fld%ps)
!       print *,'      ps1lp(',gis_phy%kdt,')=  ',gis_phy%grid_fld%ps(154,58)
!       endif
       if( ndfi>0 .and. gis_phy%kdt==ndfi/2+1 .and. .not.ldfi )  then
         call dfi_fixwr(2,    &
           gis_phy%nst_fld%xt,     gis_phy%nst_fld%xs,     gis_phy%nst_fld%xu,     &
           gis_phy%nst_fld%xv,     gis_phy%nst_fld%xz,     gis_phy%nst_fld%zm,     &
           gis_phy%nst_fld%xtts,   gis_phy%nst_fld%xzts,   gis_phy%nst_fld%dt_cool,&
           gis_phy%nst_fld%z_c,    gis_phy%nst_fld%c_0,    gis_phy%nst_fld%c_d,    &
           gis_phy%nst_fld%w_0,    gis_phy%nst_fld%w_d,    gis_phy%nst_fld%d_conv, &
           gis_phy%nst_fld%ifd,    gis_phy%nst_fld%tref,   gis_phy%nst_fld%Qrain,  &

           gis_phy%sfc_fld%hice,   gis_phy%sfc_fld%fice,   gis_phy%sfc_fld%tisfc,  &
           gis_phy%sfc_fld%tsea,   gis_phy%sfc_fld%smc,    gis_phy%sfc_fld%sheleg, &
           gis_phy%sfc_fld%stc,    gis_phy%sfc_fld%tg3,    gis_phy%sfc_fld%zorl,   &
           gis_phy%sfc_fld%cv,     gis_phy%sfc_fld%cvb,    gis_phy%sfc_fld%cvt,    &
           gis_phy%sfc_fld%alvsf,  gis_phy%sfc_fld%alvwf,  gis_phy%sfc_fld%alnsf,  &
           gis_phy%sfc_fld%alnwf,  gis_phy%sfc_fld%vfrac,  gis_phy%sfc_fld%canopy, &
           gis_phy%sfc_fld%f10m,   gis_phy%sfc_fld%vtype,  gis_phy%sfc_fld%stype,  &
           gis_phy%sfc_fld%facsf,  gis_phy%sfc_fld%facwf,  gis_phy%sfc_fld%uustar, &
           gis_phy%sfc_fld%ffmm,   gis_phy%sfc_fld%ffhh,   gis_phy%sfc_fld%tprcp,  &
           gis_phy%sfc_fld%srflag, gis_phy%sfc_fld%slc,    gis_phy%sfc_fld%snwdph, &
           gis_phy%sfc_fld%slope,  gis_phy%sfc_fld%shdmin, gis_phy%sfc_fld%shdmax, &
           gis_phy%sfc_fld%snoalb, gis_phy%sfc_fld%sncovr)
       endif
        
!!  gfs_phy_tracer%fscav(1:ntrac) is for all tracers
!!  fscav(1:ntrac-ncld-1) is for ozone + aerosl species

        if ( gis_phy%lgocart ) then
!           i1 = gis_phy%gfs_phy_tracer%ntrac_met+1   ! 1st chemical tracer (excluding o3)
!           i2 = gis_phy%gfs_phy_tracer%ntrac         ! last chemical tracer (excluding o3)
           i1 = gfs_phy_tracer%ntrac_met+1   ! 1st chemical tracer (excluding o3)
           i2 = gfs_phy_tracer%ntrac         ! last chemical tracer (excluding o3)
           gis_phy%fscav(2:gis_phy%ntrac-gis_phy%ncld-1) =      &
                  gfs_phy_tracer%fscav(i1:i2)
!                  gis_phy%gfs_phy_tracer%fscav(i1:i2)
       
           if ( gis_phy%kdt .eq. 2 ) then
	      print *, 'LU_TRC:',i1,i2,                        &
                  gfs_phy_tracer%fscav(i1:i2)
!                  gis_phy%gfs_phy_tracer%fscav(i1:i2)
           endif
!

           i1 = gis_phy%lonr
           i2 = gis_phy%lats_node_r_max
           i3 = gis_phy%levs
           gis_phy%flx_fld%wet1(1:i1,1:i2) = 0.
           gis_phy%g3d_fld%fcld(1:i1,1:i2,1:i3) = 0.
           gis_phy%g3d_fld%dqdt(1:i1,1:i2,1:i3) = 0.
        endif

 
!
! ======================================================================
        call do_physics_one_step(                                         &
                 gis_phy%deltim,   gis_phy%kdt,     gis_phy%phour,        &
!*               gis_phy%grid_gr,  gis_phy%sfc_fld, gis_phy%flx_fld,      &
                 gis_phy%grid_fld, gis_phy%sfc_fld, gis_phy%flx_fld,      &
                 gis_phy%nst_fld,  gis_phy%g3d_fld, gis_phy%g2d_fld,      &
                 gis_phy%lats_nodes_r,   gis_phy%global_lats_r,           &
                 gis_phy%lonsperlar,                                      &
                 gis_phy%XLON,    gis_phy%XLAT,    gis_phy%COSZDG,        &
                 gis_phy%HPRIME,  gis_phy%SWH,     gis_phy%HLW,           &
! idea add by hmhj
                 gis_phy%HTRSWB,  gis_phy%HTRLWB,                         &
                 gis_phy%FLUXR,   gis_phy%SFALB,                          &
                 gis_phy%SLAG,    gis_phy%SDEC,    gis_phy%CDEC,          &
                 gis_phy%OZPLIN,  gis_phy%JINDX1,  gis_phy%JINDX2,        &
                 gis_phy%DDY,                                             &
                 gis_phy%phy_f3d, gis_phy%phy_f2d, gis_phy%NBLCK,         &
                 gis_phy%ZHOUR,   gis_phy%ZHOUR_DFI,                      &
                 gis_phy%N3,      gis_phy%N4,                             &
                 gis_phy%LSOUT,   gis_phy%COLAT1,  gis_phy%CFHOUR1,       &
                 gis_phy%fscav,   gis_phy%restart_step )

!                        
! =======================================================================
!
! save phys fields for digital filter
!
       if( ldfi.and.gis_phy%kdt==ndfi/2 )  then
!         print *,'save phys state, at gis_phy%kdt=',gis_phy%kdt,'ldfi=',ldfi
         call dfi_fixwr(1,   &
           gis_phy%nst_fld%xt,     gis_phy%nst_fld%xs,     gis_phy%nst_fld%xu,     &
           gis_phy%nst_fld%xv,     gis_phy%nst_fld%xz,     gis_phy%nst_fld%zm,     &
           gis_phy%nst_fld%xtts,   gis_phy%nst_fld%xzts,   gis_phy%nst_fld%dt_cool,&
           gis_phy%nst_fld%z_c,    gis_phy%nst_fld%c_0,    gis_phy%nst_fld%c_d,    &
           gis_phy%nst_fld%w_0,    gis_phy%nst_fld%w_d,    gis_phy%nst_fld%d_conv, &
           gis_phy%nst_fld%ifd,    gis_phy%nst_fld%tref,   gis_phy%nst_fld%Qrain,  &

           gis_phy%sfc_fld%hice,   gis_phy%sfc_fld%fice,   gis_phy%sfc_fld%tisfc,  &
           gis_phy%sfc_fld%tsea,   gis_phy%sfc_fld%smc,    gis_phy%sfc_fld%sheleg, &
           gis_phy%sfc_fld%stc,    gis_phy%sfc_fld%tg3,    gis_phy%sfc_fld%zorl,   &
           gis_phy%sfc_fld%cv,     gis_phy%sfc_fld%cvb,    gis_phy%sfc_fld%cvt,    &
           gis_phy%sfc_fld%alvsf,  gis_phy%sfc_fld%alvwf,  gis_phy%sfc_fld%alnsf,  &
           gis_phy%sfc_fld%alnwf,  gis_phy%sfc_fld%vfrac,  gis_phy%sfc_fld%canopy, &
           gis_phy%sfc_fld%f10m,   gis_phy%sfc_fld%vtype,  gis_phy%sfc_fld%stype,  &
           gis_phy%sfc_fld%facsf,  gis_phy%sfc_fld%facwf,  gis_phy%sfc_fld%uustar, &
           gis_phy%sfc_fld%ffmm,   gis_phy%sfc_fld%ffhh,   gis_phy%sfc_fld%tprcp,  &
           gis_phy%sfc_fld%srflag, gis_phy%sfc_fld%slc,    gis_phy%sfc_fld%snwdph, &
           gis_phy%sfc_fld%slope,  gis_phy%sfc_fld%shdmin, gis_phy%sfc_fld%shdmax, &
           gis_phy%sfc_fld%snoalb, gis_phy%sfc_fld%sncovr)
       endif
!
!
! update hour
!
      gis_phy%phour      = fhour

! --------------------------------------------------------------------------
!      call physics_to_common_vars(gis_phy%grid_fld%ps,      &
!                                  gis_phy%grid_fld%t ,      &
!*                                 gis_phy%grid_fld%q ,      &
!                                  gis_phy%grid_fld%tracers(1)%flds, &
!                                  gis_phy%grid_fld%u ,      &
!                                  gis_phy%grid_fld%v ,      &
!                                  gis_phy%grid_fld%p ,      &
!                                  gis_phy%grid_fld%dp ,     &
!                                  gis_phy%grid_fld%dpdt ,   &
!                                  gis_phy%global_lats_r,                &
!                                  gis_phy%lonsperlar)
! --------------------------------------------------------------------------
!
      if(present(rc)) then
          rc = rc1
      end if

      end subroutine gfs_physics_run

      end module gfs_physics_run_mod
