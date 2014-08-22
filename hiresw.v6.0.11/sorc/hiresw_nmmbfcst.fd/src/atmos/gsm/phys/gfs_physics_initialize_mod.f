#include "../../../ESMFVersionDefine.h"

#if (ESMF_MAJOR_VERSION < 5 || ESMF_MINOR_VERSION < 2)
#undef ESMF_520r
#define ESMF_LogFoundError ESMF_LogMsgFoundError
#else
#define ESMF_520r
#endif

! !module: gfs_physics_initialize_mod 
!          --- initialization module of the gridded component of gfs physics.
!
! !description: gfs physics gridded component initialize module.
!
! !revision history:
!
!  november 2004  weiyu yang    initial code.
!  january  2006  s. moorthi    update to the new gfs version
!  august   2006  h. juang      add option to run generalized coordinates
!  january  2007  h. juang      change for dynamics only
!  july     2007  s. moorthi    change for physics only
!  november 2007  h. juang      continue for physics
!  may      2009  j. wang       change for quilt
!  oct 09   2009  Sarah Lu      coord def initialized (lats_nodes_r_fix,
!                               lats_node_r, ipt_lats_node_r)
!  oct 11   2009  Sarah Lu      grid_gr is replaced by grid_fld
!  oct 12   2009  Sarah Lu      initialize start_step
!  oct 16   2009  Sarah Lu      initialize gfs_phy_tracer
!  nov 14   2009  Sarah Lu      flx_fld and sfc_fld allocation modified
!  dec 10   2009  Sarah Lu      initialize lgocart and g3d_fld
!  jan 22   2010  Sarah Lu      increase ngrids_flx and nfxr to include aod 
!  feb 09   2010  Sarah Lu      set tracer_const (ri,cpi) from import state
!  feb 05   2010  J. Wang       put phy_f3d and phy_f2d into restart file
!  apr 09   2010  Sarah Lu      initialize global_lats_r, lonsperlar
!  July     2010  S. Moorthi    Upgrade to new physics + nst model
!  jul 14   2010  Sarah Lu      initialize g2d_fld
!  jul 23   2010  Sarah Lu      initialize ngrids_aer and buff_mult_pieceg
!  July 30  2010  S. Moorthi    Removed allocation of cldcov
!  Aug 19   2010  S. Moorthi    Updated for T574 + added num_reduce to namelist
!  Oct 18   2010  S. Moorthi    Added fscav initialization
!  Dec 23   2010  Sarah Lu      initialize scatter_lats, scatter_lons, g2d_fld%met
!  Mar 27   2010  J. Wang       add zsoil to sfc file
!  Oct 03   2011  W. Yang       Modified for using the ESMF 5.2.0r library.
!  Apr 06   2012  H. Juang      add idea
!
! !interface:
!
      module gfs_physics_initialize_mod

!
!!uses:
!
#ifdef ESMF_520r
      USE esmf
#else
       USE esmf_mod
#endif
      USE gfs_physics_internal_state_mod, ONLY: gfs_physics_internal_state
!     USE mpi_def,                        ONLY: liope
      USE mpi_def,                        ONLY: mc_comp, mc_io, mpi_comm_all_dup, mpi_comm_all
      USE resol_def,                      ONLY: g_dpdt, lotgr, g_p, g_dp, nrcm, g_q,          &
                                                g_gz, g_u, g_v, g_ps, g_t, ntoz,              &
                                                ntcw, lonr, latr, ncld, num_p3d, num_p2d,     &
                                                lsoil, nmtvr, levr, nlunit, ntrac, nxpt,      &
                                                jcap, levs, nypt, jintmx, ngrids_sfcc,        &
!jw
                                                ngrids_sfcc2d,ngrids_sfcc3d,                  &  !jwang
                                                ngrids_aer,                                   &  !sarah lu
                                                ngrids_flx, levp1, lonrx, nfxr, ngrids_gg,    &
                                                levm1, ivssfc, thermodyn_id, sfcpress_id,     &
                                                ivssfc_restart, latrd, latr2, ivsupa, levh,   &
                                                lgocart, scatter_lats, scatter_lons,          &
                                                nr_nst, nf_nst, ngrids_nst, ivsnst
!jw
      use mod_state,                      ONLY: buff_mult_piecea2d,buff_mult_piecea3d,        &  !jwang
                                                buff_mult_piecef,buff_mult_pieceg,            &
                                                buff_mult_piecenst
      use coordinate_def,                 ONLY: ak5,bk5,ck5                      !jwang
      use vert_def,                       only: sl,si              !  Moorthi

      USE ozne_def,                       ONLY: levozc, latsozp, blatc, timeozc, timeoz,      &
                                                kozpl, levozp, pl_time, pl_lat, pl_pres,      &
                                                kozc, dphiozc, latsozc, pl_coeff
      USE namelist_physics_def,           ONLY: ras, jo3, ldiag3d, ngptc, ens_nam,            &
                                                reduced_grid, grid_aldata, nst_fcst, lggfs3d, &
                                                num_reduce
      USE module_ras,                     ONLY: nrcmax, fix_ncld_hr
      USE gg_def,                         ONLY: sinlat_r, coslat_r, wgtcs_r, rcs2_r, wgt_r,   &
                                                colrad_r
      USE layout1,                        ONLY: nodes_comp, lats_node_r_max, lats_node_r, me, &
                                                nodes, lon_dims_ext, lon_dims_r,idrt,         &
                                                ipt_lats_node_r
      USE date_def,                       ONLY: fhour
!*    USE tracer_const,                   ONLY: set_tracer_const
      USE gfs_physics_sfc_flx_set_mod,    ONLY: sfcvar_aldata, flxvar_aldata, flx_init
      USE d3d_def,                        ONLY: d3d_init, d3d_zero
      use machine,                        ONLY : kind_io4
      USE sfcio_module,                   ONLY: sfcio_axdbta
      USE gfs_physics_gridgr_mod,         ONLY: gridvar_aldata  
      USE gfs_physics_g3d_mod,            ONLY: g3d_aldata
      USE gfs_physics_g2d_mod,            ONLY: g2d_aldata
      use gfs_phy_tracer_config,          ONLY: gfs_phy_tracer, tracer_config_init
      use gfs_physics_nst_var_mod
      use module_radsw_parameters,      only: nbdsw
      use module_radlw_parameters,      only: nbdlw
!#ifndef IBM
!     USE omp_lib
!#endif
      implicit none

      include 'mpif.h'

      contains

      subroutine gfs_physics_initialize(gis_phy, rc)

! this subroutine set up the internal state variables,
! allocate internal state arrays for initializing the gfs system.
!----------------------------------------------------------------

      type(gfs_physics_internal_state), pointer, intent(inout) :: gis_phy
      integer,                                   intent(out)   :: rc

      integer 		:: ierr

      integer 		:: j, l, n, ilat, locl, ikey, nrank_all
      integer           :: num_parthds
!!
      real (kind=kind_io4) blatc4
      real (kind=kind_io4), allocatable :: pl_lat4(:)
      real (kind=kind_io4), allocatable :: pl_pres4(:)
      real (kind=kind_io4), allocatable :: pl_time4(:)
!!
!     include 'function2'
!!

! set up gfs internal state (gis_phy) dimension and values for physics etc
!-------------------------------------------------------------------
      me     = gis_phy%me
      nodes  = gis_phy%nodes
!      CALL ESMF_VMGetCurrent(vm, rc = ierr)

      call compns_physics(gis_phy%deltim, gis_phy%iret,  gis_phy%ntrac,	&
                          gis_phy%nxpt,   gis_phy%nypt,  gis_phy%jintmx,&
                          gis_phy%jcap,   gis_phy%levs,  gis_phy%levr,  &
                          gis_phy%lonr,   gis_phy%latr,                 &
                          gis_phy%ntoz,   gis_phy%ntcw,  gis_phy%ncld,  &
                          gis_phy%lsoil,  gis_phy%nmtvr,                &
                          gis_phy%num_p3d, gis_phy%num_p2d,             &
                          gis_phy%thermodyn_id, gis_phy%sfcpress_id,    &
                          gis_phy%nam_gfs_phy%nlunit, me,               &
                          gis_phy%nam_gfs_phy%gfs_phy_namelist)
!
      CALL set_soilveg(me,gis_phy%nam_gfs_phy%nlunit)

!* ri/cpi is filled from dyn export state attributes (Sarah Lu)
!*      call set_tracer_const(gis_phy%ntrac,me,gis_phy%nam_gfs_phy%nlunit)
!
! met+chem tracer specification (Sarah Lu)
! NOTE: This config_init call repeats the init routine in dyc gc.  
!       The redundant calls will be removed in a later revision.  
!       The tracer specification will then be passed in from dyn dc
!
!      call tracer_config_init( gis_phy%gfs_phy_tracer, gis_phy%ntrac,   &
      call tracer_config_init( gis_phy%ntrac,   &
                               gis_phy%ntoz, gis_phy%ntcw,              &
                               gis_phy%ncld,  me )
!      gfs_phy_tracer = gis_phy%gfs_phy_tracer
      gis_phy%lgocart = gfs_phy_tracer%doing_GOCART     ! for internal state
      lgocart = gis_phy%lgocart                         ! for resol_def module
      if( me == 0) then
       write(0,*)'LU_TRC, ntrac     =',gfs_phy_tracer%ntrac, gis_phy%ntrac
       write(0,*)'LU_TRC, ntrac_met =',gfs_phy_tracer%ntrac_met
       write(0,*)'LU_TRC, ntrac_chem=',gfs_phy_tracer%ntrac_chem
       write(0,*)'LU_TRC, lgocart   =',gis_phy%lgocart,lgocart
       do n = 1, gfs_phy_tracer%ntrac
         write(0,*)'LU_TRC, tracer_vname=',gfs_phy_tracer%vname(n)
       enddo
      endif

!
      nlunit  = gis_phy%nam_gfs_phy%nlunit
      ntrac   = gis_phy%ntrac
      nxpt    = gis_phy%nxpt
      nypt    = gis_phy%nypt
      jintmx  = gis_phy%jintmx
      jcap    = gis_phy%jcap
      levs    = gis_phy%levs
      levr    = gis_phy%levr
      lonr    = gis_phy%lonr
      latr    = gis_phy%latr
      ntoz    = gis_phy%ntoz
      ntcw    = gis_phy%ntcw
      ncld    = gis_phy%ncld
      lsoil   = gis_phy%lsoil
      nmtvr   = gis_phy%nmtvr
      num_p3d = gis_phy%num_p3d
      num_p2d = gis_phy%num_p2d
      thermodyn_id = gis_phy%thermodyn_id
      sfcpress_id  = gis_phy%sfcpress_id
      if (gis_phy%nam_gfs_phy%Total_Member <= 1) then
        ens_nam=' '
      else
        write(ens_nam,'("_",I2.2)') gis_phy%nam_gfs_phy%Member_Id
      endif
!
!     ivssfc  = 200501
      ivssfc  = 200509
      ivssfc_restart  = 200509
      if (ivssfc > ivssfc_restart) ivssfc_restart = ivssfc
      ivsnst  = 200907
!
      allocate(gis_phy%zsoil(lsoil))
      if(lsoil==2) then
        gis_phy%zsoil=(/-0.1,-2.0/)   
      elseif(lsoil==4) then
        gis_phy%zsoil=(/-0.1,-0.4,-1.0,-2.0/)
      endif

      ivsupa  = 0
      if (levs .gt. 99) ivsupa  = 200509
!
      levh   = ntrac*levs
      latrd  = latr + 2*jintmx
      latr2  = latr/2
      levm1  = levs-1 
      levp1  = levs+1 
      lonrx  = lonr + 1 + 2*nxpt + 1
!
      ngrids_sfcc = 32+LSOIL*3   ! No CV, CVB, CVT! includes T2M, Q2M, TISFC
!jw
      ngrids_sfcc2d = 32        ! No CV, CVB, CVT! includes T2M, Q2M, TISFC
      ngrids_sfcc3d = LSOIL*3   ! for smc,stc,slc
!     ngrids_flx  = 66+30        ! additional fields (most related to land surface)
!     ngrids_flx  = 66+30+6      ! additional fields (aod)
!     nfxr        = 27
      ngrids_flx  = 66+43+6      ! additional fields (aod)
      nfxr        = 39          ! Add AOD
      ngrids_gg   = 2+LEVS*(4+ntrac)
!
      if (nst_fcst > 0) then         !     For NST model
!       ngrids_nst = 19              ! oceanic fields (for diurnal warming and sub-layer)
         nr_nst = 10                 ! oceanic fields: for diurnal warming modelrun
        nf_nst = 9                   ! oceanic fields: for GSI analysis
        ngrids_nst = nr_nst + nf_nst ! oceanic fields (for diurnal warming and sub-layer)
      else
        ngrids_nst = 0
      endif
!
      if (ntrac-ncld-1 > 0) then
        allocate ( gis_phy%fscav(ntrac-ncld-1), stat = ierr )
        gis_phy%fscav = 0.0
      endif

      allocate(si(levp1))
      allocate(sl(levs))

      allocate ( lon_dims_r(latr), stat = ierr )
      allocate ( lon_dims_ext(latrd), stat = ierr )
!
      allocate(colrad_r(latr), stat = ierr)
      allocate(wgt_r(latr2), stat = ierr)
      allocate(wgtcs_r(latr2), stat = ierr)
      allocate(rcs2_r(latr2), stat = ierr)
      allocate(sinlat_r(latr), stat = ierr)
      allocate(coslat_r(latr), stat = ierr)
!
      allocate ( gis_phy%lats_nodes_r(nodes), stat = ierr )
      allocate ( gis_phy%lats_nodes_ext(nodes), stat = ierr )
      allocate ( gis_phy%global_lats_r(latr), stat = ierr )
!     allocate ( gis_phy%global_lats_ext(latr+2*jintmx+2*nypt*(nodes-1)), stat = ierr )
      allocate ( gis_phy%global_lats_ext(latr), stat = ierr )
      allocate ( gis_phy%lonsperlar(latr), stat = ierr)
      allocate ( gis_phy%lats_nodes_r_fix(nodes), stat = ierr )    !added for mGrid

      write(0,*)' in gfs_physics_initialize reduced_grid=',reduced_grid
      if( reduced_grid ) then
        if (me == 0) print *,' run with reduced gaussian grid '
        call set_lonsgg(gis_phy%lonsperlar,num_reduce,me)
!       write(0,*)' gis_phy%lonsperlar=',gis_phy%lonsperlar
      else
        if (me == 0) print *,' run with full gaussian grid '
        do j=1,latr
          gis_phy%lonsperlar(j) = lonr
        enddo
      endif
!
      g_gz   = 1
      g_ps   = g_gz  + 1
      g_t    = g_ps  + 1     
      g_u    = g_t   + levs
      g_v    = g_u   + levs
      g_q    = g_v   + levs
      g_p    = g_q   + levh
      g_dp   = g_p   + levs
      g_dpdt = g_dp  + levs
       
      lotgr  = g_dpdt+ levs - 1

      gis_phy%g_gz    = g_gz  
      gis_phy%g_ps    = g_ps  
      gis_phy%g_t     = g_t  
      gis_phy%g_u     = g_u  
      gis_phy%g_v     = g_v  
      gis_phy%g_q     = g_q  
      gis_phy%g_p     = g_p  
      gis_phy%g_dp    = g_dp 
      gis_phy%g_dpdt  = g_dpdt 
!
      gis_phy%lotgr = lotgr


      if (ras) then
        if (fix_ncld_hr) then
!         nrcm = min(nrcmax, levs-1) * (gis_phy%deltim/1200) + 0.50001
          nrcm = min(nrcmax, levs-1) * (gis_phy%deltim/1200) + 0.10001
!         nrcm = min(nrcmax, levs-1) * min(1.0,gis_phy%deltim/360) + 0.1
        else
          nrcm = min(nrcmax, levs-1)
        endif
!       nrcm = max(nrcmax, nint((nrcmax*gis_phy%deltim)/600.0))
      else
        nrcm = 1
      endif
!
      if (ntoz .le. 0) then      ! Diagnostic ozone
        rewind (kozc)
        read (kozc,end=101) latsozc, levozc, timeozc, blatc4
  101   if (levozc .lt. 10 .or. levozc .gt. 100) then
          rewind (kozc)
          levozc  = 17
          latsozc = 18
          blatc   = -85.0
        else
          blatc   = blatc4
        endif
        latsozp   = 2
        levozp    = 1
        timeoz    = 1
        pl_coeff  = 0
      else                       ! Prognostic Ozone
        rewind (kozpl)
        read (kozpl) pl_coeff, latsozp, levozp, timeoz
        allocate (pl_lat(latsozp), pl_pres(levozp),pl_time(timeoz+1), stat = ierr)
        allocate (pl_lat4(latsozp), pl_pres4(levozp),pl_time4(timeoz+1), stat = ierr)
        rewind (kozpl)
        read (kozpl) pl_coeff, latsozp, levozp, timeoz, pl_lat4, pl_pres4,  &
                     pl_time4
        pl_pres(:) = pl_pres4(:)
        pl_lat(:)  = pl_lat4(:)
        pl_time(:) = pl_time4(:)
        latsozc = 2
        blatc   = 0.0
      endif
      dphiozc = -(blatc+blatc)/(latsozc-1)
!
      if (me .eq. 0) then

!       print *,' g_gz ',g_gz
!       print *,' g_ps ',g_ps
!       print *,' g_t  ',g_t 
!       print *,' g_u  ',g_u 
!       print *,' g_v  ',g_v 
!       print *,' g_q  ',g_q 
!       print *,' g_p  ',g_p 
!       print *,' g_dp ',g_dp
!       print *,' g_dpdt ',g_dpdt
!       print *,' lotgr ',lotgr
        print *,' latsozp=',latsozp,' levozp=',levozp,' timeoz=',timeoz
        print *,' latsozc=',latsozc,' levozc=',levozc,' timeozc=',        &
                  timeozc, 'dphiozc=',dphiozc
!       print *,' pl_lat=',pl_lat
!       print *,' pl_pres=',pl_pres
!       print *,' pl_time=',pl_time

      endif

!     pl_pres(:) = log(0.1*pl_pres(:))       ! Natural log of pres in cbars
      pl_pres(:) = log(100.0*pl_pres(:))     ! Natural log of pres in Pa
!
      allocate(gis_phy%OZPLIN(LATSOZP,LEVOZP,pl_coeff,timeoz), stat = ierr) !OZONE P-L coeffcients

!      print *,' finished array allocation in gfs_physics_initialize '

!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!!
!!      create io communicator and comp communicator
!!
!      if (me == 0) write(*,*) 'io option ,liope :',liope
!
!jw      call mpi_comm_dup(mpi_comm_all, mpi_comm_all_dup, ierr)
!     call mpi_barrier (mpi_comm_all_dup,               ierr)

!      if (nodes == 1) liope=.false.
!jw      if (liope) then
!jw        call mpi_comm_rank(mpi_comm_all_dup,nrank_all,ierr)
!jw        icolor=1
!jw        ikey=1
!jw        nodes_comp=nodes-1
!jw        if (nrank_all.eq.nodes-1) then
!!  io server
!jw          write(*,*) 'io server task'
!jw          icolor=2
!jw          gis_phy%kcolor=mpi_undefined
!jw          call mpi_comm_split(mpi_comm_all_dup,icolor,ikey,mc_io,ierr)
!jw          call mpi_comm_split(mpi_comm_all_dup,gis_phy%kcolor,ikey,mc_comp,ierr)
!jw        else
!sela     write(*,*) 'compute server task '
!jw          icolor=mpi_undefined
!jw          gis_phy%kcolor=1
!jw          call mpi_comm_split(mpi_comm_all_dup,gis_phy%kcolor,ikey,mc_comp,ierr)
!jw          call mpi_comm_split(mpi_comm_all_dup,icolor,ikey,mc_io,ierr)
!jw          call mpi_comm_size(mc_comp,nodes,ierr)
!jw        endif
!jw      else
!jw        icolor=2
!jw        mc_comp=mpi_comm_all_dup
        nodes_comp=nodes
!jw      endif
!!
!c
!     call f_hpminit(me,"evod")  !jjt hpm stuff
!c
!     call f_hpmstart(25,"get_ls_gftlons")
!c
!!
      call synchro
      call init_countperf(latr)
!$$$      time0=timer()
!jfe  call countperf(0,15,0.)
!
      if (me.eq.0) then
      print 100, jcap,levs
100   format (' smf ',i3,i3,' in gfs physics initialize ')
!#ifdef IBM
      print*,'number of threads is ',num_parthds()
!#else
!     print*,'number of threads is ',omp_get_num_threads()
!#endif
!jw        if (liope) then
!jw          print*,'number of mpi procs is ',nodes
!jw          print*,'number of mpi io procs is 1 (nodes)'
!jw        else
          print*,'number of mpi procs is ',nodes
!jw        endif
      endif
!c
      gis_phy%cons0    =    0.0d0     !constant
      gis_phy%cons0p5  =    0.5d0     !constant
      gis_phy%cons1200 = 1200.d0      !constant
      gis_phy%cons3600 = 3600.d0      !constant
!!
!cxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
!
!c
      if(gis_phy%iret.ne.0) then
        if(me.eq.0) print *,' incompatible physics namelist -',         &
                            ' aborted in gfs_phy_initilize ',gis_phy%iret 
!*                          ' aborted in gfs_phy_initilize'
        call mpi_quit(13)
      endif
!!
!     if predicted ozon is desired set jo3=2
      jo3=2          !using predicted ozone in radiation.
!
      gis_phy%lats_nodes_ext = 0

!       write(0,*)' gis_phy%lonsperlar2d=',gis_phy%lonsperlar
      call getcon_physics(gis_phy%n3,gis_phy%n4,                        &
           gis_phy%lats_nodes_r,gis_phy%global_lats_r,                  &
           gis_phy%lonsperlar,                                          &
           gis_phy%lats_nodes_ext,gis_phy%global_lats_ext,              &
           gis_phy%colat1,gis_phy%idrt)
      idrt=gis_phy%idrt

!       write(0,*)' gis_phy%lonsperlar2c=',gis_phy%lonsperlar

      gis_phy%lats_node_r_max = lats_node_r_max
      gis_phy%lats_nodes_r_fix(:) =  gis_phy%lats_node_r_max 

!* set up scatter_lats and scatter_lons for simple scatter (Sarah Lu)
      allocate(scatter_lats (latr))
      allocate(scatter_lons (latr))
      scatter_lats(1:latr) = gis_phy%global_lats_r(1:latr)
      scatter_lons(1:latr) = gis_phy%lonsperlar(1:latr)

!* change lats_node_r to lats_node_r_max to allow the pointer option
!*    call sfcvar_aldata(lonr, lats_node_r, lsoil, gis_phy%sfc_fld, ierr)
!*    call flxvar_aldata(lonr, lats_node_r, gis_phy%flx_fld, ierr)
      call sfcvar_aldata(lonr, lats_node_r_max, lsoil, gis_phy%sfc_fld, ierr)
      call flxvar_aldata(lonr, lats_node_r_max, gis_phy%flx_fld, ierr)

!      print *,' check after sfc flx var_aldata ' 
!     IF (me == 0) write(*,*) ' in "GFS_Initialize_ESMFMod,lonr,lats_node_r,nr_nst,nf_nst : ',lonr,lats_node_r,nr_nst,nf_nst,'lats_node_r_max=',lats_node_r_max
       write(*,*) ' in "GFS_Initialize_ESMFMod,lonr,lats_node_r,nr_nst,nf_nst : ',lonr,lats_node_r,nr_nst,nf_nst,'lats_node_r_max=',lats_node_r_max

!    Modified by Moorthi
      call nstvar_aldata(lonr,lats_node_r_max,gis_phy%nst_fld,ierr)

!! allocate grid_fld                      --- Sarah Lu                   
      gis_phy%grid_aldata = grid_aldata                                  
      if ( gis_phy%grid_aldata ) then                                    
      print *,'LU_PHY: grid_fld allocated ; copy is used'                
      call gridvar_aldata (lonr, lats_node_r_max, levs,  &               
                           gis_phy%ntrac, gis_phy%grid_fld, ierr)  
      else                                                            
      print *,'LU_PHY: grid_fld not allocated ; pointer is used'     
      endif                                                     

!*    allocate (   gis_phy%grid_gr(lonr*lats_node_r_max,lotgr), stat = ierr )
      ALLOCATE (   gis_phy%XLON(LONR,LATS_NODE_R), stat = ierr)
      ALLOCATE (   gis_phy%XLAT(LONR,LATS_NODE_R), stat = ierr)
      ALLOCATE (   gis_phy%COSZDG(LONR,LATS_NODE_R), stat = ierr)
!*    ALLOCATE (   gis_phy%CLDCOV(LEVS,LONR,LATS_NODE_R), stat = ierr)
!     ALLOCATE (   gis_phy%CLDCOV(LEVS,LONR,LATS_NODE_R_MAX), stat = ierr)
      ALLOCATE (   gis_phy%SFALB(LONR,LATS_NODE_R), stat = ierr)
      ALLOCATE (   gis_phy%HPRIME(NMTVR,LONR,LATS_NODE_R), stat = ierr)
      ALLOCATE (   gis_phy%FLUXR(nfxr,LONR,LATS_NODE_R), stat = ierr)

      gis_phy%NBLCK=LONR/NGPTC+1

      ALLOCATE (   gis_phy%SWH(NGPTC,LEVS,gis_phy%NBLCK,LATS_NODE_R), stat = ierr)
      ALLOCATE (   gis_phy%HLW(NGPTC,LEVS,gis_phy%NBLCK,LATS_NODE_R), stat = ierr)
! idea add by hmhj
      ALLOCATE ( gis_phy%HTRSWB(NGPTC,LEVS,NBDSW,gis_phy%NBLCK,LATS_NODE_R),stat=ierr)
      ALLOCATE ( gis_phy%HTRLWB(NGPTC,LEVS,NBDLW,gis_phy%NBLCK,LATS_NODE_R),stat=ierr)

!
      ALLOCATE (gis_phy%JINDX1(LATS_NODE_R), stat = ierr)
      ALLOCATE (gis_phy%JINDX2(LATS_NODE_R), stat = ierr)
      ALLOCATE (gis_phy%DDY(LATS_NODE_R), stat = ierr)
!
      allocate (gis_phy%phy_f3d(NGPTC,LEVS,gis_phy%NBLCK,lats_node_r,num_p3d), stat = ierr)
      allocate (gis_phy%phy_f2d(lonr,lats_node_r,num_p2d), stat = ierr)
!
      allocate (   gis_phy%fhour_idate(1,5), stat = ierr )
!
!      write(0,*) ' check after lots allocates,size(fhour_idate)= ' ,   &
!        size(gis_phy%fhour_idate,1),size(gis_phy%fhour_idate,2),ierr

      if (ldiag3d) then
!* change lats_node_r to lats_node_r_max for consistency
!*      call d3d_init(ngptc,gis_phy%nblck,lonr,lats_node_r,levs,pl_coeff)
!       call d3d_init(ngptc,gis_phy%nblck,lonr,lats_node_r_max,levs,pl_coeff)
        call d3d_init(ngptc,gis_phy%nblck,lonr,lats_node_r_max,levs,pl_coeff, &
                      ldiag3d,lggfs3d)
      endif

!* allocate g3d_fld and g2d_fld
      if (lgocart) then
        call g3d_aldata (lonr, lats_node_r_max, levs,  &               
                         gis_phy%g3d_fld, ierr)  
!        call g2d_aldata (lonr, lats_node_r_max, gis_phy%gfs_phy_tracer,&
        call g2d_aldata (lonr, lats_node_r_max, gfs_phy_tracer,&
                         gis_phy%g2d_fld, ierr)

        ngrids_aer = 0
        if ( gis_phy%g2d_fld%du%nfld > 0 )   &
          ngrids_aer = ngrids_aer+gis_phy%g2d_fld%du%nfld

        if ( gis_phy%g2d_fld%su%nfld > 0 )   &
          ngrids_aer = ngrids_aer+gis_phy%g2d_fld%su%nfld

        if ( gis_phy%g2d_fld%oc%nfld > 0 )   &
          ngrids_aer = ngrids_aer+gis_phy%g2d_fld%oc%nfld

        if ( gis_phy%g2d_fld%bc%nfld > 0 )   &
          ngrids_aer = ngrids_aer+gis_phy%g2d_fld%bc%nfld

        if ( gis_phy%g2d_fld%ss%nfld > 0 )   &
          ngrids_aer = ngrids_aer+gis_phy%g2d_fld%ss%nfld

        if ( gis_phy%g2d_fld%met%nfld > 0 )   &
          ngrids_aer = ngrids_aer+gis_phy%g2d_fld%met%nfld

!       print *, 'INIT_g2d_fld ngrids_aer = ',ngrids_aer
      endif

!     if (icolor /= 2 .or. .not. liope) then
        if (num_p3d .gt. 0) gis_phy%phy_f3d = 0.0
        if (num_p2d .gt. 0) gis_phy%phy_f2d = 0.0
!     endif
!jw
!jw       allocate(buff_mult_piecea2d(lonr,lats_node_r_max,1:ngrids_sfcc2d+ngrids_nst+1))
       allocate(buff_mult_piecea2d(lonr,lats_node_r_max,1:ngrids_sfcc2d+1))
       allocate(buff_mult_piecea3d(lonr,lats_node_r_max,1:ngrids_sfcc3d+1))
       allocate(buff_mult_piecef(lonr,lats_node_r_max,1:ngrids_flx+1))
       allocate(buff_mult_pieceg(lonr,lats_node_r_max,1:ngrids_aer+1))
       allocate(buff_mult_piecenst(lonr,lats_node_r_max,1:ngrids_nst+1))

       write(0,*)' lonr=',lonr,' lats_node_r_max=',lats_node_r_max,       &
       'ngrids_sfcc2d=',ngrids_sfcc2d,' ngrids_nst=',ngrids_nst

       buff_mult_piecea2d(1:lonr,1:lats_node_r_max,1:ngrids_sfcc2d+1)=0.
       buff_mult_piecea3d(1:lonr,1:lats_node_r_max,1:ngrids_sfcc3d+1)=0.
       buff_mult_piecef(1:lonr,1:lats_node_r_max,1:ngrids_flx+1)=0.
       buff_mult_pieceg(1:lonr,1:lats_node_r_max,1:ngrids_aer+1)=0.
       buff_mult_piecenst(1:lonr,1:lats_node_r_max,1:ngrids_nst+1)=0.
!!
      call countperf(0,18,0.)
!!
!       write(0,*)' gis_phy%lonsperlar2b=',gis_phy%lonsperlar

      call fix_fields(gis_phy%LONSPERLAR,gis_phy%GLOBAL_LATS_R,           &
        gis_phy%XLON,gis_phy%XLAT,gis_phy%sfc_fld,gis_phy%nst_fld,        &
        gis_phy%HPRIME,gis_phy%JINDX1,gis_phy%JINDX2,gis_phy%DDY,         &
        gis_phy%OZPLIN,gis_phy%nam_gfs_phy%sfc_ini,                       &
                       gis_phy%nam_gfs_phy%nst_ini,                       &
        gis_phy%nblck,gis_phy%phy_f3d,gis_phy%phy_f2d )

!      print *,' GISXLAT=',gis_phy%XLAT(1,:)
!!
! coord def (lats_node_r, ipt_lats_node_r, and lats_nodes_a_fix)     
      gis_phy%lats_node_r     = lats_node_r                      
      gis_phy%ipt_lats_node_r = ipt_lats_node_r              

!!
!! debug print (Sarah Lu)
!      if(me==0) then                                                  
!         do j=1,latr                                                
!         print *, 'PHY: lonsperlar=',j,gis_phy%lonsperlar(j)     
!         enddo                                                        
!         print *, 'PHY: lats_node_r_max=',gis_phy%lats_node_r_max 
!         print *, 'PHY: lats_nodes_r=',gis_phy%lats_nodes_r(:)   
!         print *, 'PHY: global_lats_r=',gis_phy%global_lats_r(:)
!      endif                                                          
!      print *, 'PHY:  lats_node_r=',me,gis_phy%lats_node_r      
!      n = 0                                                     
!      do j = 1, gis_phy%lats_node_r                              
!         ilat = gis_phy%global_lats_r(gis_phy%ipt_lats_node_r-1+j)  
!         l =  gis_phy%lonsperlar(ilat)                            
!         n = n + l                                                 
!         print *, 'PHY:, xlat=',me,n,gis_phy%ipt_lats_node_r-1+j,& 
!                   j, ilat, l, 57.29578*gis_phy%xlat(1,j)       
!      enddo                                             
!!
      call countperf(1,18,0.)
!!
      gis_phy%zhour=gis_phy%phour
      gis_phy%FLUXR=0.
!
      call flx_init(gis_phy%flx_fld, ierr)

      call d3d_zero(ldiag3d,lggfs3d)

!     if (ldiag3d) then
!       call d3d_zero
!     endif
!
! initialize start_step (Sarah Lu)
      gis_phy% start_step  = .true.      

      rc = 0
!
!
!       write(0,*)' gis_phy%lonsperlar2=',gis_phy%lonsperlar
!
      end subroutine gfs_physics_initialize
!
! =========================================================================
!
      subroutine set_lonsgg(lonsperlat,num_reduce,me)
!     use resol_def
      use reduce_lons_grid_module, only : reduce_grid           ! hmhj
      integer num_reduce, me                                    ! hmhj
      integer lonsperlat(latr)
       
      integer lonsperlat_62(94)
      integer lonsperlat_126(190)
      integer lonsperlat_170(256)
      integer lonsperlat_190(288)
      integer lonsperlat_254(384)
      integer lonsperlat_382(576)
      integer lonsperlat_510(766)
      integer lonsperlat_574(880)
       
      data lonsperlat_62/                                                &
        30,  30,  30,  40,  48,  56,  60,  72,  72,  80,  90,  90,       &
        96, 110, 110, 120, 120, 128, 144, 144, 144, 144, 154, 160,       &
       160, 168, 168, 180, 180, 180, 180, 180, 180, 192, 192, 192,       &
       192, 192, 192, 192, 192, 192, 192, 192, 192, 192, 192, 47*0 /
       
      data lonsperlat_126      /                                         &
          30,   30,   36,   48,   56,   60,   72,   72,   80,   90,      &
          96,  110,  110,  120,  120,  128,  144,  144,  154,  160,      &
         160,  180,  180,  180,  192,  192,  210,  210,  220,  220,      &
         240,  240,  240,  240,  240,  252,  256,  280,  280,  280,      &
         280,  288,  288,  288,  288,  308,  308,  308,  320,  320,      &
         320,  320,  330,  330,  360,  360,  360,  360,  360,  360,      &
         360,  360,  360,  360,  360,  360,  384,  384,  384,  384,      &
         384,  384,  384,  384,  384,  384,  384,  384,  384,  384,      &
         384,  384,  384,  384,  384,  384,  384,  384,  384,  384,      &
         384,  384,  384,  384,  384, 95*0 /
                                                                                
      data lonsperlat_170 /                                              &
         48,  48,  48,  48,  48,  56,  60,  72,  72,  80,  90,  96,      &
        110, 110, 120, 120, 128, 144, 144, 144, 154, 160, 168, 180,      &
        180, 180, 192, 210, 210, 220, 220, 240, 240, 240, 240, 240,      &
        252, 256, 280, 280, 280, 288, 288, 288, 308, 308, 320, 320,      &
        320, 320, 330, 360, 360, 360, 360, 360, 360, 360, 384, 384,      &
        384, 384, 384, 384, 420, 420, 420, 440, 440, 440, 440, 440,      &
        440, 440, 440, 440, 462, 462, 462, 462, 462, 480, 480, 480,      &
        480, 480, 480, 480, 480, 480, 480, 480, 504, 504, 504, 504,      &
        504, 504, 504, 504, 504, 512, 512, 512, 512, 512, 512, 512,      &
        512, 512, 512, 512, 512, 512, 512, 512, 512, 512, 512, 512,      &
        512, 512, 512, 512, 512, 512, 512, 512, 128*0 /
                                                                                
      data lonsperlat_190 /                                              &
        64,  64,  64,  64,  64,  64,  64,  70,  80,  84,                 &
        88, 110, 110, 110, 120, 126, 132, 140, 144, 154,                 &
       160, 168, 176, 176, 192, 192, 198, 210, 210, 220,                 &
       220, 240, 240, 240, 252, 252, 256, 264, 280, 280,                 &
       280, 288, 308, 308, 308, 320, 320, 320, 330, 336,                 &
       352, 352, 352, 352, 360, 384, 384, 384, 384, 384,                 &
       396, 396, 420, 420, 420, 420, 420, 440, 440, 440,                 &
       440, 440, 448, 448, 462, 462, 462, 480, 480, 480,                 &
       480, 480, 504, 504, 504, 504, 504, 504, 504, 512,                 &
       512, 528, 528, 528, 528, 528, 528, 560, 560, 560,                 &
       560, 560, 560, 560, 560, 560, 560, 560, 560, 560,                 &
       560, 576, 576, 576, 576, 576, 576, 576, 576, 576,                 &
       576, 576, 576, 576, 576, 576, 576, 576, 576, 576,                 &
       576, 576, 576, 576, 576, 576, 576, 576, 576, 576,                 &
       576, 576, 576, 576, 144*   0/
      data lonsperlat_254      /                                         &
          64,   64,   64,   64,   64,   64,   72,   72,   80,   90,      &
          96,  110,  110,  120,  120,  128,  144,  144,  154,  160,      &
         168,  180,  180,  180,  192,  192,  210,  220,  220,  240,      &
         240,  240,  240,  252,  256,  280,  280,  280,  288,  288,      &
         288,  308,  308,  320,  320,  320,  330,  360,  360,  360,      &
         360,  360,  360,  384,  384,  384,  384,  420,  420,  420,      &
         440,  440,  440,  440,  440,  440,  462,  462,  462,  480,      &
         480,  480,  480,  480,  480,  504,  504,  504,  504,  512,      &
         512,  560,  560,  560,  560,  560,  560,  576,  576,  576,      &
         576,  576,  576,  576,  576,  616,  616,  616,  616,  616,      &
         616,  640,  640,  640,  640,  640,  640,  640,  640,  640,      &
         640,  660,  660,  660,  720,  720,  720,  720,  720,  720,      &
         720,  720,  720,  720,  720,  720,  720,  720,  720,  720,      &
         720,  720,  720,  720,  720,  720,  720,  720,  768,  768,      &
         768,  768,  768,  768,  768,  768,  768,  768,  768,  768,      &
         768,  768,  768,  768,  768,  768,  768,  768,  768,  768,      &
         768,  768,  768,  768,  768,  768,  768,  768,  768,  768,      &
         768,  768,  768,  768,  768,  768,  768,  768,  768,  768,      &
         768,  768,  768,  768,  768,  768,  768,  768,  768,  768,      &
         768,  768,  192*0/
                                                                                
      data lonsperlat_382      /                                         &
         64,  64,  64,  64,  64,  64,  64,  70,  80,  84,                &
         88,  96, 110, 110, 120, 126, 132, 140, 144, 154,                &
        160, 168, 176, 180, 192, 192, 198, 210, 220, 220,                &
        224, 240, 240, 252, 252, 256, 264, 280, 280, 280,                &
        288, 308, 308, 308, 320, 320, 330, 336, 352, 352,                &
        352, 360, 384, 384, 384, 384, 396, 396, 420, 420,                &
        420, 420, 440, 440, 440, 448, 448, 462, 462, 480,                &
        480, 480, 504, 504, 504, 504, 512, 528, 528, 528,                &
        560, 560, 560, 560, 560, 560, 576, 576, 616, 616,                &
        616, 616, 616, 616, 616, 616, 630, 630, 640, 640,                &
        660, 660, 660, 660, 672, 672, 704, 704, 704, 704,                &
        704, 704, 720, 720, 720, 768, 768, 768, 768, 768,                &
        768, 768, 768, 768, 768, 792, 792, 792, 792, 792,                &
        840, 840, 840, 840, 840, 840, 840, 840, 840, 840,                &
        880, 880, 880, 880, 880, 880, 880, 880, 880, 880,                &
        896, 896, 896, 896, 924, 924, 924, 924, 924, 924,                &
        960, 960, 960, 960, 960, 960, 960, 960, 960, 960,                &
        990, 990, 990, 990, 990, 990, 990, 990,1008,1008,                &
       1008,1008,1008,1008,1024,1024,1024,1024,1024,1024,                &
       1056,1056,1056,1056,1056,1056,1056,1056,1056,1056,                &
       1120,1120,1120,1120,1120,1120,1120,1120,1120,1120,                &
       1120,1120,1120,1120,1120,1120,1120,1120,1120,1120,                &
       1120,1120,1120,1120,1120,1120,1120,1120,1120,1120,                &
       1120,1152,1152,1152,1152,1152,1152,1152,1152,1152,                &
       1152,1152,1152,1152,1152,1152,1152,1152,1152,1152,                &
       1152,1152,1152,1152,1152,1152,1152,1152,1152,1152,                &
       1152,1152,1152,1152,1152,1152,1152,1152,1152,1152,                &
       1152,1152,1152,1152,1152,1152,1152,1152,1152,1152,                &
       1152,1152,1152,1152,1152,1152,1152,1152, 288*   0/
                                                                                
      data lonsperlat_510      /                                         &
          64,   64,   64,   64,   64,   64,   72,   72,   80,   90,      &
          96,  110,  110,  120,  120,  128,  144,  144,  154,  160,      &
         168,  180,  180,  180,  192,  210,  210,  220,  220,  240,      &
         240,  240,  240,  252,  256,  280,  280,  288,  288,  288,      &
         308,  308,  320,  320,  320,  330,  360,  360,  360,  360,      &
         360,  384,  384,  384,  384,  420,  420,  440,  440,  440,      &
         440,  440,  440,  462,  462,  462,  480,  480,  480,  480,      &
         504,  504,  504,  504,  512,  512,  560,  560,  560,  560,      &
         576,  576,  576,  576,  576,  576,  616,  616,  616,  616,      &
         640,  640,  640,  640,  640,  640,  640,  660,  720,  720,      &
         720,  720,  720,  720,  720,  720,  720,  720,  720,  720,      &
         720,  768,  768,  768,  768,  768,  768,  768,  768,  840,      &
         840,  840,  840,  840,  840,  840,  840,  880,  880,  880,      &
         880,  880,  880,  880,  880,  880,  880,  924,  924,  924,      &
         924,  924,  924,  924,  960,  960,  960,  960,  960,  960,      &
         960,  960,  960,  960,  960,  990,  990,  990, 1008, 1008,      &
        1008, 1008, 1008, 1024, 1024, 1024, 1024, 1024, 1120, 1120,      &
        1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120,      &
        1152, 1152, 1152, 1152, 1152, 1152, 1152, 1152, 1152, 1152,      &
        1152, 1152, 1152, 1152, 1152, 1152, 1232, 1232, 1232, 1232,      &
        1232, 1232, 1232, 1232, 1232, 1232, 1232, 1232, 1260, 1260,      &
        1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260,      &
        1280, 1280, 1280, 1280, 1280, 1280, 1280, 1280, 1280, 1320,      &
        1320, 1320, 1320, 1386, 1386, 1386, 1386, 1386, 1386, 1386,      &
        1386, 1386, 1386, 1386, 1386, 1386, 1386, 1386, 1386, 1386,      &
        1440, 1440, 1440, 1440, 1440, 1440, 1440, 1440, 1440, 1440,      &
        1440, 1440, 1440, 1440, 1440, 1440, 1440, 1440, 1440, 1440,      &
        1440, 1440, 1440, 1440, 1440, 1440, 1440, 1440, 1440, 1440,      &
        1440, 1440, 1440, 1536, 1536, 1536, 1536, 1536, 1536, 1536,      &
        1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,      &
        1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,      &
        1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,      &
        1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,      &
        1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,      &
        1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,      &
        1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,      &
        1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,      &
        1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,      &
        1536, 1536, 1536,  383*0/

      data lonsperlat_574      /                                         &
          18,   28,   32,   42,   48,   56,   64,   72,   80,   84,      &
          90,  110,  110,  110,  120,  126,  132,  140,  144,  154,      &
         160,  168,  176,  176,  192,  192,  198,  210,  210,  220,      &
         224,  240,  240,  252,  252,  256,  264,  280,  280,  288,      &
         288,  308,  308,  308,  320,  320,  330,  330,  352,  352,      &
         352,  360,  384,  384,  384,  384,  396,  396,  420,  420,      &
         420,  420,  440,  440,  440,  448,  462,  462,  462,  480,      &
         480,  480,  504,  504,  504,  504,  512,  528,  528,  528,      &
         560,  560,  560,  560,  560,  576,  576,  576,  616,  616,      &
         616,  616,  616,  616,  630,  630,  630,  640,  660,  660,      &
         660,  660,  672,  672,  704,  704,  704,  704,  704,  720,      &
         720,  720,  768,  768,  768,  768,  768,  768,  768,  768,      &
         770,  792,  792,  792,  792,  840,  840,  840,  840,  840,      &
         840,  840,  840,  880,  880,  880,  880,  880,  880,  880,      &
         896,  896,  896,  896,  924,  924,  924,  924,  924,  960,      &
         960,  960,  960,  960,  960,  960,  990,  990,  990,  990,      &
         990, 1008, 1008, 1008, 1008, 1024, 1024, 1024, 1056, 1056,      &
        1056, 1056, 1056, 1056, 1120, 1120, 1120, 1120, 1120, 1120,      &
        1120, 1120, 1120, 1120, 1120, 1120, 1120, 1152, 1152, 1152,      &
        1152, 1152, 1152, 1152, 1232, 1232, 1232, 1232, 1232, 1232,      &
        1232, 1232, 1232, 1232, 1232, 1232, 1232, 1232, 1232, 1232,      &
        1232, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1280, 1280,      &
        1280, 1280, 1280, 1320, 1320, 1320, 1320, 1320, 1320, 1320,      &
        1320, 1320, 1344, 1344, 1344, 1344, 1344, 1344, 1386, 1386,      &
        1386, 1386, 1386, 1386, 1386, 1386, 1386, 1386, 1386, 1408,      &
        1408, 1408, 1408, 1408, 1440, 1440, 1440, 1440, 1440, 1440,      &
        1440, 1440, 1440, 1440, 1536, 1536, 1536, 1536, 1536, 1536,      &
        1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,      &
        1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,      &
        1536, 1536, 1536, 1536, 1584, 1584, 1584, 1584, 1584, 1584,      &
        1584, 1584, 1584, 1584, 1584, 1584, 1584, 1584, 1584, 1584,      &
        1584, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680,      &
        1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680,      &
        1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680,      &
        1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680,      &
        1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1760, 1760,      &
        1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760,      &
        1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760,      &
        1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760,      &
        1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760,      &
        1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760,      &
        1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760,      &
        1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760,      &
        1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760,      &
         440*0/
                                                                                
      integer i
      if (num_reduce < 0) then
        if (jcap == 62) then
           lonsperlat = lonsperlat_62
        endif
        if (jcap == 126) then
           lonsperlat = lonsperlat_126
        endif
        if (jcap == 170) then
           lonsperlat = lonsperlat_170
        endif
        if (jcap == 190) then
           lonsperlat = lonsperlat_190
        endif
        if (jcap == 254) then
           lonsperlat = lonsperlat_254
        endif
        if (jcap == 382) then
           lonsperlat = lonsperlat_382
        endif
        if (jcap == 510) then
           lonsperlat = lonsperlat_510
        endif
        if (jcap == 574) then
           lonsperlat = lonsperlat_574
        endif
      endif
                                                                                
      if (jcap .ne. 62 .and. jcap .ne. 126 .and. jcap .ne. 170 .and.   &
          jcap .ne. 190 .and. jcap .ne. 574 .and.                        &
          jcap .ne. 254 .and. jcap .ne. 382 .and. jcap .ne. 510) then
! compute reduced grid using juang 2003
         if ( me == 0 ) then
           print*,' non standard resolution  - lonsperlat',              &
                  ' computed locally'
         endif
         call reduce_grid (abs(num_reduce),jcap,latr,lonsperlat)
         if ( me == 0 ) then
           print*,' Reduced grid is computed - lonsperlat '             ! hmhj
         endif
      endif
                                                                                
      if ( me == 0 ) then
        print*,' jcap = ',jcap
        print*,'min,max of lonsperlat = ',minval(lonsperlat),              &
                maxval(lonsperlat)
      endif
      end subroutine set_lonsgg
!
      end module gfs_physics_initialize_mod
