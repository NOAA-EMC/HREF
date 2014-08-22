
! !module: gfs_dynamics_initialize_mod 
!          --- initialize module of the
!              gridded component of the gfs dynamics system.
!              gfs dynamics related initilization 
!
! !description: gfs dynamics gridded component initialize module.
!
! !revision history:
!
!  november 2004  weiyu yang     initial code.
!  january 2006  s. moorthi      update to the new gfs version
!  august 2006   h. juang        add option to run generalized coordinates
!  december 2006 s. moorthi      gfsio included
!  january 2007 h. juang         change for dynamics only
!  May     2008 j. wang          change for gfs wrt grid component
!  Oct 04  2009 sarah lu         init xlon, xlat, lats_nodes_a_fix
!  Oct 05  2009 sarah lu         grid_gr unfolded from 2D to 3D
!  Oct 16  2009 sarah lu         initialize gfs_dyn_tracer
!  november 2009 j. wang         grid_gr_dfi for digital filter
!  Feb 05  2010 j. wang          add option to read in  restart file
!  Aug 19  2010 S. Moorthi       Updated for T574 + added num_reduce to namelist
!  Aug 25  2010 sarah lu         add option to compute tracer global sum
!  Sep 08  2010 J. Wang          changed gfsio file to nemsio file
!  Nov 01  2010 H. Juang         add non-iteration dimensional-split
!                                semi-Lagrangian dynamics (ndslfv)
!                                with mass_dp, process_split options.
!  Dec 16  2010 J. Wang          changed to nemsio library
!  Feb 20  2011 H. Juang         implement into nems for mass_dp and ndsl
!  Feb 28  2011 Sarah Lu         add thermodyn_id and sfcpress_id
!  Apr 06  2012 H. Juang         add idea
!  Sep 20  2012 J. Wang          add sigio option
!  Jan 20  2013 J. Wang          idea diffusion init interface change
!
!
! !interface:
!
      module gfs_dynamics_initialize_mod
!
!!uses:
!
      use gfs_dynamics_getcf_mod
      use gfs_dyn_machine, only : kind_io4
      use nemsio_module , only : nemsio_init
!
      use gfs_dyn_write_state, only : buff_mult_pieceg
      use gfs_dyn_layout1, only : ipt_lats_node_a, lats_node_a_max
      use gfs_dyn_resol_def, only : adiabatic, thermodyn_id, sfcpress_id
      use namelist_dynamics_def, only : fhrot,fhini,num_reduce,nemsio_in
      use gfs_dyn_tracer_config, only: gfs_dyn_tracer, tracer_config_init,gfs_dyn_tracer
      use gfs_dyn_io_header, only: z_r,z
!#ifndef IBM
!     USE omp_lib
!#endif

      implicit none

      contains

      subroutine gfs_dynamics_initialize(gis_dyn, rc)

! this subroutine set up the internal state variables,
! allocate internal state arrays for initializing the gfs system.
!----------------------------------------------------------------
!
      implicit none
!
      type(gfs_dynamics_internal_state), pointer, intent(inout) :: gis_dyn
      integer,                                    intent(out)   :: rc

      integer 		:: ierr

      integer 		:: j, l, n, ilat, locl, ikey, nrank_all
      integer           :: num_parthds
!!
      character(20) cfile


! set up gfs internal state dimension and values for dynamics etc
!-------------------------------------------------------------------
      me     = gis_dyn%me
      if (me == 0)                                                      &
      write(0,*)'in initial,nbefore allocate lonsperlat,',allocated(gis_dyn%lonsperlat),'latg=',latg
!
      nodes  = gis_dyn%nodes
      nlunit = gis_dyn%nam_gfs_dyn%nlunit

      call compns_dynamics(gis_dyn%deltim, gis_dyn%iret, gis_dyn%ntrac,	&
                           gis_dyn%nxpt,   gis_dyn%nypt, gis_dyn%jintmx,&
                           gis_dyn%jcap,                              	&
                           gis_dyn%levs,   gis_dyn%levr, 		&
                           gis_dyn%lonf,   gis_dyn%latg,          	&
                           gis_dyn%ntoz,   gis_dyn%ntcw, gis_dyn%ncld, 	&
                           gis_dyn%spectral_loop,               	&
                     me,   gis_dyn%thermodyn_id,gis_dyn%sfcpress_id,    &
                           gis_dyn%nam_gfs_dyn%nlunit, 			&
                           gis_dyn%nam_gfs_dyn%gfs_dyn_namelist,        &
                           gis_dyn%ndfi)                           ! jw
!
      call get_tracer_const(gis_dyn%ntrac,me,gis_dyn%nam_gfs_dyn%nlunit)
!
! met+chem tracer specification (Sarah Lu)
!
!      call tracer_config_init( gis_dyn%gfs_dyn_tracer, gis_dyn%ntrac,   &
      call tracer_config_init( gis_dyn%ntrac,   &
                               gis_dyn%ntoz, gis_dyn%ntcw,              &
                               gis_dyn%ncld,  me )
!      gfs_dyn_tracer = gis_dyn%gfs_dyn_tracer
      if( me == 0) then
       write(0,*)'LU_TRC, exit tracer_config_init in dyn'
       write(0,*)'LU_TRC, ntrac=     ',gfs_dyn_tracer%ntrac,ntrac
       write(0,*)'LU_TRC, ntrac_met =',gfs_dyn_tracer%ntrac_met
       write(0,*)'LU_TRC, ntrac_chem=',gfs_dyn_tracer%ntrac_chem
       do n = 1, gfs_dyn_tracer%ntrac
        write(0,*)'LU_TRC, tracer_vname=',gfs_dyn_tracer%vname(n, :)
       enddo
      endif
!
      ntrac   = gis_dyn%ntrac
      nxpt    = gis_dyn%nxpt
      nypt    = gis_dyn%nypt
      jintmx  = gis_dyn%jintmx
      jcap    = gis_dyn%jcap
      levs    = gis_dyn%levs
      levr    = gis_dyn%levr
      lonf    = gis_dyn%lonf
      latg    = gis_dyn%latg
      ntoz    = gis_dyn%ntoz
      ntcw    = gis_dyn%ntcw
      ncld    = gis_dyn%ncld
      thermodyn_id = gis_dyn%thermodyn_id
      sfcpress_id  = gis_dyn%sfcpress_id
      nemsio_in    = gis_dyn%nemsio_in

      if (gis_dyn%nam_gfs_dyn%total_member <= 1) then
        ens_nam=' '
      else
        write(ens_nam,'("_",i2.2)') gis_dyn%nam_gfs_dyn%member_id
      endif
!
      levh   = ntrac*levs
      latgd  = latg+ 2*jintmx 
      jcap1  = jcap+1 
      jcap2  = jcap+2 
      latg2  = latg/2 
      levm1  = levs-1 
      levp1  = levs+1 
      lonfx  = lonf + 1 + 2*nxpt+1 
      lnt    = jcap2*jcap1/2 
      lnuv   = jcap2*jcap1 
      lnt2   = 2*lnt 
      lnt22  = 2*lnt+1 
      lnte   = (jcap2/2)*((jcap2/2)+1)-1 
      lnto   = (jcap2/2)*((jcap2/2)+1)-(jcap2/2) 
      lnted  = lnte 
      lntod  = lnto 

!jw      ngrids_gg       = 2+levs*(4+ntrac)
      ngrids_gg       = 2+levs*(5+ntrac)
      gis_dyn%lnt2    = lnt2

      allocate(lat1s_a(0:jcap))
      allocate(lon_dims_a(latgd))
      allocate(lon_dims_ext(latgd))

      allocate(colrad_a(latg2))
      allocate(wgt_a(latg2))
      allocate(wgtcs_a(latg2))
      allocate(rcs2_a(latg2))
      allocate(sinlat_a(latg))
      allocate(coslat_a(latg))
      coslat_a = 0

      allocate(am(levs,levs))
      allocate(bm(levs,levs))
      allocate(cm(levs,levs))
      allocate(dm(levs,levs,jcap1))
      allocate(tor(levs))
      allocate(si(levp1))
      allocate(sik(levp1))
      allocate(sl(levs))
      allocate(slk(levs))
      allocate(del(levs))
      allocate(rdel2(levs))
      allocate(ci(levp1))
      allocate(cl(levs))
      allocate(tov(levs))
      allocate(sv(levs))

      allocate(ak5(levp1))
      allocate(bk5(levp1))
      allocate(ck5(levp1)) 
      allocate(thref(levp1))
      allocate(ck(levs))
      allocate(dbk(levs))
      allocate(bkl(levs))
      allocate(amhyb(levs,levs))
      allocate(bmhyb(levs,levs))
      allocate(smhyb(levs,levs))
      allocate(hmhyb(levs,levs))
      allocate(svhyb(levs))
      allocate(tor_hyb(levs))
      allocate(d_hyb_m(levs,levs,jcap1))
      allocate(dm205_hyb(jcap1,levs,levs))

      allocate(spdmax(levs))

      if (nemsio_in) then
        call nemsio_init(ierr)
      endif
!
      if (me == 0)                                                       &
      write(0,*)'before allocate lonsperlat,',allocated(gis_dyn%lonsperlat),'latg=',latg
      allocate(gis_dyn%lonsperlat(latg))

      if( reduced_grid ) then
        if(me == 0) print *,' run with reduced quardratic grid '
        call set_lonsgg(gis_dyn%lonsperlat,jcap,lonf,latg,num_reduce,me)
      else
        if (me == 0) print *,' run with full gaussian grid '
        do j=1,latg
          gis_dyn%lonsperlat(j) = lonf
        enddo
      endif
!
! spectral location
        p_zem  =         1     !     zeme/o(lnte/od,2,levs),
        p_dim  = p_zem  +levs  !     dime/o(lnte/od,2,levs),
        p_tem  = p_dim  +levs  !     teme/o(lnte/od,2,levs),
      if( .not. ndslfv ) then
        p_rm   = p_tem  +levs  !      rme/o(lnte/od,2,levh),
        p_dpm  = p_rm   +levh  !      qme/o(lnte/od,2),
      else
        p_dpm  = p_tem  +levs  !      rme/o(lnte/od,2,levh),
      endif
        p_qm   = p_dpm  +levs  !      qme/o(lnte/od,2),

        p_ze   = p_qm   +1     !      zee/o(lnte/od,2,levs),
        p_di   = p_ze   +levs  !      die/o(lnte/od,2,levs),
        p_te   = p_di   +levs  !      tee/o(lnte/od,2,levs),
      if( .not. ndslfv ) then
        p_rq   = p_te   +levs  !      rqe/o(lnte/od,2,levh),
        p_dp   = p_rq   +levh  !       qe/o(lnte/od,2),
      else
        p_dp   = p_te   +levs  !      rqe/o(lnte/od,2,levh),
      endif
        p_q    = p_dp   +levs  !       qe/o(lnte/od,2),
        p_dlam = p_q    +1     !  dpdlame/o(lnte/od,2),
        p_dphi = p_dlam +1     !  dpdphie/o(lnte/od,2),
        p_uln  = p_dphi +1     !     ulne/o(lnte/od,2,levs),
        p_vln  = p_uln  +levs  !     vlne/o(lnte/od,2,levs),
        p_zslam= p_vln  +levs  !    zslam/o(lnte/od,2),
        p_zsphi= p_zslam+1     !    zsphi/o(lnte/od,2),
                                                                                
        p_zz   = p_zsphi+1
        p_dpphi= p_zz   +levs
        p_zzphi= p_dpphi+levs
        p_dplam= p_zzphi+levs
        p_zzlam= p_dplam+levs

        p_w    = p_zzlam+levs  !       we/o(lnte/od,2,levs),
        p_x    = p_w    +levs  !       xe/o(lnte/od,2,levs),
        p_y    = p_x    +levs  !       ye/o(lnte/od,2,levs),
      if( .not. ndslfv ) then
        p_rt   = p_y    +levs  !      rte/o(lnte/od,2,levh),
        p_dpn  = p_rt   +levh  !      dpe/o(lnte/od,2,levs),
      else
        p_dpn  = p_y    +levs  !      rte/o(lnte/od,2,levh),
      endif
        p_zq   = p_dpn  +levs  !      zqe/o(lnte/od,2),
        p_gz   = p_zq   +1     !      gze/o(lnte/od,2),
        p_lapgz= p_gz   +1     !      gze/o(lnte/od,2),

      lotls  = p_lapgz

      g_gz   = 1
      g_uum  = g_gz  + 1        !  for grid point
      g_vvm  = g_uum + levs     !  for grid point
      g_ttm  = g_vvm + levs     !  for grid point
      g_rm   = g_ttm + levs     !  for grid point
      g_dpm  = g_rm  + levh     !  for grid point
      g_qm   = g_dpm + levs     !  for grid point

      g_uu   = g_qm  + 1        !  for grid point
      g_vv   = g_uu  + levs     !  for grid point
      g_tt   = g_vv  + levs     !  for grid point
      g_rq   = g_tt  + levs     !  for grid point
      g_dp   = g_rq  + levh     !  for grid point
      g_q    = g_dp  + levs     !  for grid point

      g_u    = g_q   + 1        !  for grid point
      g_v    = g_u   + levs     !  for grid point
      g_t    = g_v   + levs     !  for grid point
      g_rt   = g_t   + levs     !  for grid point
      g_dpn  = g_rt  + levh     !  for grid point
      g_zq   = g_dpn + levs     !  for grid point

      g_p    = g_zq  + 1   	!  for grid point 
      g_dpdt = g_p   + levs   	!  for grid point 
      g_zz   = g_dpdt+ levs     !  for grid point

      g_uup   = g_zz  + levs    !  for grid point
      g_vvp   = g_uup + levs    !  for grid point
      g_ttp   = g_vvp + levs    !  for grid point
      g_rqp   = g_ttp + levs    !  for grid point
      g_dpp   = g_rqp + levh    !  for grid point
      g_zqp   = g_dpp + levs    !  for grid point

      lotgr  = g_zqp
      lotgr6 = 4*levs+1*levh+1
!c
      if( .not. ndslfv ) then
        lots = 6*levs+1*levh+5
        lotd = 6*levs+2*levh+0
        lota = 5*levs+1*levh+2
      else
        lots = 6*levs+5
        lotd = 6*levs
        lota = 5*levs+2
      endif
      lotp = 4*levs
!
        ksz     =1
        ksd     =ksz+levs
        kst     =ksd+levs
      if( .not. ndslfv ) then
        ksr     =kst+levs
        ksdp    =ksr+levh
      else
        ksdp    =kst+levs
      endif
        ksq     =ksdp+levs
        ksplam  =ksq+1
        kspphi  =ksplam+1
        ksu     =kspphi+1
        ksv     =ksu+levs
        kzslam  =ksv+levs
        kzsphi  =kzslam+1
!
        kau     =1
        kav     =kau+levs
        kat     =kav+levs
      if( .not. ndslfv ) then
        kar     =kat+levs
        kadp    =kar+levh
      else
        kadp    =kat+levs
      endif
        kaps    =kadp+levs
        kazs    =kaps+1
        kap2    =kazs+1
!
      kdpphi  =1
      kzzphi  =kdpphi+levs
      kdplam  =kzzphi+levs
      kzzlam  =kdplam+levs
!
        kdtphi  =1
      if( .not. ndslfv ) then
        kdrphi  =kdtphi+levs
        kdtlam  =kdrphi+levh
        kdrlam  =kdtlam+levs
        kdulam  =kdrlam+levh
      else
        kdtlam  =kdtphi+levs
        kdulam  =kdtlam+levs
      endif
        kdvlam  =kdulam+levs
        kduphi  =kdvlam+levs
        kdvphi  =kduphi+levs
!
! point to internal state
        gis_dyn%p_zem   = p_zem            !     zeme/o(lnte/od,2,levs),
        gis_dyn%p_dim   = p_dim            !     dime/o(lnte/od,2,levs),
        gis_dyn%p_tem   = p_tem            !     teme/o(lnte/od,2,levs),
      if( .not. ndslfv ) then
        gis_dyn%p_rm    = p_rm             !      rme/o(lnte/od,2,levh),
      endif
        gis_dyn%p_dpm   = p_dpm            !     dpme/o(lnte/od,2),
        gis_dyn%p_qm    = p_qm             !      qme/o(lnte/od,2),
        gis_dyn%p_zslam = p_zslam          ! hmhj
        gis_dyn%p_zsphi = p_zsphi          ! hmhj
        gis_dyn%p_ze    = p_ze             !      zee/o(lnte/od,2,levs),
        gis_dyn%p_di    = p_di             !      die/o(lnte/od,2,levs),
        gis_dyn%p_te    = p_te             !      tee/o(lnte/od,2,levs),
      if( .not. ndslfv ) then
        gis_dyn%p_rq    = p_rq             !      rqe/o(lnte/od,2,levh),
      endif
        gis_dyn%p_dp    = p_dp             !      dpe/o(lnte/od,2),
        gis_dyn%p_q     = p_q              !       qe/o(lnte/od,2),
        gis_dyn%p_dlam  = p_dlam           !  dpdlame/o(lnte/od,2),
        gis_dyn%p_dphi  = p_dphi           !  dpdphie/o(lnte/od,2),
        gis_dyn%p_uln   = p_uln            !     ulne/o(lnte/od,2,levs),
        gis_dyn%p_vln   = p_vln            !     vlne/o(lnte/od,2,levs),
        gis_dyn%p_w     = p_w              !       we/o(lnte/od,2,levs),
        gis_dyn%p_x     = p_x              !       xe/o(lnte/od,2,levs),
        gis_dyn%p_y     = p_y              !       ye/o(lnte/od,2,levs),
      if( .not. ndslfv ) then
        gis_dyn%p_rt    = p_rt             !      rte/o(lnte/od,2,levh),
      endif
        gis_dyn%p_dpn   = p_dpn            !     dpne/o(lnte/od,2)
        gis_dyn%p_zq    = p_zq             !      zqe/o(lnte/od,2)
        gis_dyn%p_gz    = p_gz             !      gze/o(lnte/od,2),
!
      gis_dyn%g_gz    = g_gz
      gis_dyn%g_uum   = g_uum
      gis_dyn%g_vvm   = g_vvm
      gis_dyn%g_ttm   = g_ttm
      gis_dyn%g_rm    = g_rm
      gis_dyn%g_dpm   = g_dpm
      gis_dyn%g_qm    = g_qm
      gis_dyn%g_uu    = g_uu
      gis_dyn%g_vv    = g_vv
      gis_dyn%g_tt    = g_tt
      gis_dyn%g_rq    = g_rq
      gis_dyn%g_dp    = g_dp
      gis_dyn%g_q     = g_q
      gis_dyn%g_u     = g_u
      gis_dyn%g_v     = g_v
      gis_dyn%g_t     = g_t
      gis_dyn%g_rt    = g_rt
      gis_dyn%g_dpn   = g_dpn
      gis_dyn%g_zq    = g_zq
      gis_dyn%g_p     = g_p
      gis_dyn%g_dpdt  = g_dpdt
      gis_dyn%g_zz    = g_zz
      gis_dyn%g_uup   = g_uup
      gis_dyn%g_vvp   = g_vvp
      gis_dyn%g_ttp   = g_ttp
      gis_dyn%g_rqp    = g_rqp
      gis_dyn%g_dpp    = g_dpp
      gis_dyn%g_zqp    = g_zqp
!
      gis_dyn%lotls = lotls
      gis_dyn%lotgr = lotgr
      gis_dyn%lots = lots 
      gis_dyn%lotd = lotd
      gis_dyn%lota = lota
      gis_dyn%lotp = lotp
!
      allocate(gis_dyn%tee1(levs))

!     print *,' finish dimension in gfs_dynamics_initialize '

!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!!
!!      create io communicator and comp communicator
!!
      if (me == 0) write(0,*) 'io option ,liope :',liope
!!
      nodes_comp=nodes
!c
      call f_hpminit(me,"evod")  !jjt hpm stuff
!c
      call f_hpmstart(25,"get_ls_gftlons")
!c
!!
      call synchro
      call init_countperf(latg)
!$$$      time0=timer()
!jfe  call countperf(0,15,0.)
!
      if (me.eq.0) then
        print 100, jcap,levs
100   format (' smf ',i3,i3,' created august 2000 ev od ri ')
!#ifdef IBM
        print*,'number of threads is',num_parthds()
!#else
!       print*,'number of threads is',omp_get_num_threads()
!#endif
        print*,'number of mpi procs is',nodes
      endif
!
      gis_dyn%cons0    =    0.0d0     !constant
      gis_dyn%cons0p5  =    0.5d0     !constant
      gis_dyn%cons1200 = 1200.d0      !constant
      gis_dyn%cons3600 = 3600.d0      !constant
!
      ls_dim = (jcap1-1)/nodes+1
!!
!cxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
!
      if (me == 0)                                                       &
       write(0,*)'befpre allocate ls_nodes,',allocated(gis_dyn%ls_nodes),&
       'ls_dim=', ls_dim,'nodes=',nodes
!
      allocate (      gis_dyn%ls_node (ls_dim*3) )
      allocate (      gis_dyn%ls_nodes(ls_dim,nodes) )
      allocate (  gis_dyn%max_ls_nodes(nodes) )
!
      allocate (  gis_dyn%lats_nodes_a_fix(nodes))     ! added for mGrid
!
      allocate (  gis_dyn%lats_nodes_a(nodes) )
      allocate ( gis_dyn%global_lats_a(latg) )
!
      allocate (   gis_dyn%lats_nodes_ext(nodes) )
      allocate ( gis_dyn%global_lats_ext(latg+2*jintmx+2*nypt*(nodes-1)) )
!
!
      gis_dyn%iprint = 0
      call get_ls_node( me, gis_dyn%ls_node, ls_max_node, gis_dyn%iprint )
!
!
      len_trie_ls=0
      len_trio_ls=0
      do locl=1,ls_max_node
           gis_dyn%ls_node(locl+  ls_dim)=len_trie_ls
          gis_dyn%ls_node(locl+2*ls_dim)=len_trio_ls
         l=gis_dyn%ls_node(locl)
         len_trie_ls=len_trie_ls+(jcap+3-l)/2
         len_trio_ls=len_trio_ls+(jcap+2-l)/2
      enddo
      if (me == 0) print *,'ls_node=',gis_dyn%ls_node(1:ls_dim),'2dim=',  &
         gis_dyn%ls_node(ls_dim+1:2*ls_dim),'3dim=',  &
         gis_dyn%ls_node(2*ls_dim+1:3*ls_dim)
!c
!c
      allocate (       gis_dyn%epse  (len_trie_ls) )
      allocate (       gis_dyn%epso  (len_trio_ls) )
      allocate (       gis_dyn%epsedn(len_trie_ls) )
      allocate (       gis_dyn%epsodn(len_trio_ls) )
!c
      allocate (      gis_dyn%snnp1ev(len_trie_ls) )
      allocate (      gis_dyn%snnp1od(len_trio_ls) )
!c
      allocate (       gis_dyn%ndexev(len_trie_ls) )
      allocate (       gis_dyn%ndexod(len_trio_ls) )
!c
      allocate (      gis_dyn%plnev_a(len_trie_ls,latg2) )
      allocate (      gis_dyn%plnod_a(len_trio_ls,latg2) )
      allocate (      gis_dyn%pddev_a(len_trie_ls,latg2) )
      allocate (      gis_dyn%pddod_a(len_trio_ls,latg2) )
      allocate (      gis_dyn%plnew_a(len_trie_ls,latg2) )
      allocate (      gis_dyn%plnow_a(len_trio_ls,latg2) )
!c
      gis_dyn%maxstp=36

 
      if(me.eq.0) 							&
        print*,'from compns_dynamics : iret=',gis_dyn%iret		&
       ,' nsout=',nsout,' nsres=',nsres
      if(gis_dyn%iret.ne.0) then
        if(me.eq.0) print *,' incompatible namelist - aborted in main'
        call mpi_quit(13)
      endif
!!
      gis_dyn%lats_nodes_ext = 0
      call getcon_dynamics(gis_dyn%n3,gis_dyn%n4,			&
           gis_dyn%ls_node,gis_dyn%ls_nodes,gis_dyn%max_ls_nodes,       &
           gis_dyn%lats_nodes_a,gis_dyn%global_lats_a,                  &
           gis_dyn%lonsperlat,gis_dyn%lats_node_a_max,                  &
           gis_dyn%lats_nodes_ext,gis_dyn%global_lats_ext,              &
           gis_dyn%epse,gis_dyn%epso,gis_dyn%epsedn,gis_dyn%epsodn,     &
           gis_dyn%snnp1ev,gis_dyn%snnp1od,				&
           gis_dyn%ndexev,gis_dyn%ndexod,  				&
           gis_dyn%plnev_a,gis_dyn%plnod_a,				&
           gis_dyn%pddev_a,gis_dyn%pddod_a, 				&
           gis_dyn%plnew_a,gis_dyn%plnow_a,gis_dyn%colat1)
!
      gis_dyn%lats_node_a=gis_dyn%lats_nodes_a(me+1)
      gis_dyn%ipt_lats_node_a=ipt_lats_node_a
      if (me == 0)                                                       &
      write(0,*)'after getcon_dynamics,lats_node_a=',gis_dyn%lats_node_a &
       ,'ipt_lats_node_a=',gis_dyn%ipt_lats_node_a,'ngptc=',ngptc
!
!     print *,'ls_nodes=',gis_dyn%ls_nodes(1:ls_dim,me+1)
!!
      gis_dyn%nblck=lonf/ngptc+1

!
! initialize coord def (xlon,xlat) and lats_nodes_a_fix
!
      gis_dyn%lats_nodes_a_fix(:) = gis_dyn%lats_node_a_max
      allocate ( gis_dyn%XLON(lonf,gis_dyn%lats_node_a) )
      allocate ( gis_dyn%XLAT(lonf,gis_dyn%lats_node_a) )

      call gfs_dyn_lonlat_para(gis_dyn%global_lats_a,        &
              gis_dyn%xlon, gis_dyn%xlat, gis_dyn%lonsperlat)

      call countperf(0,18,0.)
!!
      call countperf(0,15,0.)
      allocate (   gis_dyn%trie_ls (len_trie_ls,2,lotls) )
      allocate (   gis_dyn%trio_ls (len_trio_ls,2,lotls) )
      allocate (   gis_dyn%grid_gr (lonf,lats_node_a_max,lotgr    ) )
      allocate (   gis_dyn%grid_gr6(lonf,lats_node_a_max,lotgr6*2 ) )
      allocate (   gis_dyn%pwat    (lonf,lats_node_a) )
      allocate (   gis_dyn%ptot    (lonf,lats_node_a) )
      allocate (   gis_dyn%ptrc    (lonf,lats_node_a,ntrac) )         !glbsum
      allocate (   z(lnt2) )
      allocate (   z_r(lnt2) )
!c
      allocate (   gis_dyn%syn_ls_a(4*ls_dim,gis_dyn%lots,latg2) )
      allocate (   gis_dyn%dyn_ls_a(4*ls_dim,gis_dyn%lotd,latg2) )
!c
      allocate (   gis_dyn%syn_gr_a_1(lonfx*gis_dyn%lots,lats_dim_ext) )
      allocate (   gis_dyn%syn_gr_a_2(lonfx*gis_dyn%lots,lats_dim_ext) )
      allocate (   gis_dyn%pyn_gr_a_1(lonfx*gis_dyn%lotp,lats_dim_ext) )
      allocate (   gis_dyn%pyn_gr_a_2(lonfx*gis_dyn%lotp,lats_dim_ext) )
      allocate (   gis_dyn%dyn_gr_a_1(lonfx*gis_dyn%lotd,lats_dim_ext) )
      allocate (   gis_dyn%dyn_gr_a_2(lonfx*gis_dyn%lotd,lats_dim_ext) )
      allocate (   gis_dyn%anl_gr_a_1(lonfx*gis_dyn%lota,lats_dim_ext) )
      allocate (   gis_dyn%anl_gr_a_2(lonfx*gis_dyn%lota,lats_dim_ext) )
!!
!** allocate digital filter vars
      gis_dyn%grid_gr_dfi%z_imp=0;gis_dyn%grid_gr_dfi%ps_imp=0
      gis_dyn%grid_gr_dfi%z_imp=0;gis_dyn%grid_gr_dfi%ps_imp=0
      gis_dyn%grid_gr_dfi%temp_imp=0;gis_dyn%grid_gr_dfi%u_imp=0
      gis_dyn%grid_gr_dfi%v_imp=0;gis_dyn%grid_gr_dfi%tracer_imp=0
      gis_dyn%grid_gr_dfi%p_imp=0;gis_dyn%grid_gr_dfi%dp_imp=0
      gis_dyn%grid_gr_dfi%dpdt_imp=0
      if(gis_dyn%ndfi>0.and. gis_dyn%esmf_sta_list%z_import==1) then
        gis_dyn%grid_gr_dfi%z_imp=1
        allocate( gis_dyn%grid_gr_dfi%hs(lonf,lats_node_a_max,1))
      endif
      if(gis_dyn%ndfi>0.and. gis_dyn%esmf_sta_list%ps_import==1) then
        gis_dyn%grid_gr_dfi%ps_imp=1
        allocate( gis_dyn%grid_gr_dfi%ps(lonf,lats_node_a_max,1))
      endif
      if(gis_dyn%ndfi>0.and. gis_dyn%esmf_sta_list%temp_import==1) then
        gis_dyn%grid_gr_dfi%temp_imp=1
        allocate( gis_dyn%grid_gr_dfi%t(lonf,lats_node_a_max,levs))
      endif
      if(gis_dyn%ndfi>0.and. gis_dyn%esmf_sta_list%u_import==1) then
        gis_dyn%grid_gr_dfi%u_imp=1
        allocate( gis_dyn%grid_gr_dfi%u(lonf,lats_node_a_max,levs))
      endif
      if(gis_dyn%ndfi>0.and. gis_dyn%esmf_sta_list%v_import==1) then
        gis_dyn%grid_gr_dfi%v_imp=1
        allocate( gis_dyn%grid_gr_dfi%v(lonf,lats_node_a_max,levs))
      endif
      if(gis_dyn%ndfi>0.and. gis_dyn%esmf_sta_list%tracer_import==1) then
        gis_dyn%grid_gr_dfi%tracer_imp=1
        allocate( gis_dyn%grid_gr_dfi%tracer(lonf,lats_node_a_max,ntrac*levs))
      endif
      if(gis_dyn%ndfi>0.and. gis_dyn%esmf_sta_list%p_import==1) then
        gis_dyn%grid_gr_dfi%p_imp=1
        allocate( gis_dyn%grid_gr_dfi%p(lonf,lats_node_a_max,levs))
      endif
      if(gis_dyn%ndfi>0.and. gis_dyn%esmf_sta_list%dp_import==1) then
        gis_dyn%grid_gr_dfi%dp_imp=1
        allocate( gis_dyn%grid_gr_dfi%dp(lonf,lats_node_a_max,levs))
      endif
      if(gis_dyn%ndfi>0.and. gis_dyn%esmf_sta_list%dpdt_import==1) then
        gis_dyn%grid_gr_dfi%dpdt_imp=1
        allocate( gis_dyn%grid_gr_dfi%dpdt(lonf,lats_node_a_max,levs))
      endif
!
!## allocate output vars
      allocate(buff_mult_pieceg(lonf,lats_node_a_max,ngrids_gg))
      buff_mult_pieceg = 0.
      adiabatic = gis_dyn%adiabatic
!##

!!
      allocate (      gis_dyn%fhour_idate(1,5) )
!      write(0,*)'after allocate fhour_idate'
!
      if (me == 0) then
        print*, ' lats_dim_a=', lats_dim_a, ' lats_node_a=', lats_node_a
        print*, ' lats_dim_ext=', lats_dim_ext,                           &
                ' lats_node_ext=', lats_node_ext
      endif
!c
      gis_dyn%grid_gr  = 0.0
      gis_dyn%grid_gr6 = 0.0
      gis_dyn%ptot     = 0.0
      gis_dyn%pwat     = 0.0
      gis_dyn%ptrc     = 0.0                            !glbsum

        ilat=lats_node_a
      call countperf(1,15,0.)
!!
!c......................................................................
!c
      call countperf(0,15,0.)
      call f_hpmstop(25)
!c
!      write(0,*) 'number of latitudes ext. :',lats_node_ext,              &
!                  lats_dim_ext,lats_node_a
!!
      call countperf(1,15,0.)
!!
!      print *,' sig_ini=',gis_dyn%nam_gfs_dyn%sig_ini,			  &
!              ' sig_ini2=',gis_dyn%nam_gfs_dyn%sig_ini2 
      call countperf(0,18,0.)
      gis_dyn%pdryini = 0.0

      if (me == 0) then
        print *,' grid_ini=',gis_dyn%nam_gfs_dyn%grid_ini,'fhrot=',fhrot,    &
        'fhini=',fhini,'restart_run=',gis_dyn%restart_run
      endif

      if( .not. gis_dyn%restart_run) then
        if(nemsio_in) then
          cfile=gis_dyn%nam_gfs_dyn%grid_ini
        else
          cfile=gis_dyn%nam_gfs_dyn%sig_ini
        endif
!
        call input_fields(cfile, gis_dyn%pdryini,                            &
          gis_dyn%trie_ls, gis_dyn%trio_ls,  gis_dyn%grid_gr ,               &
          gis_dyn%ls_node, gis_dyn%ls_nodes, gis_dyn%max_ls_nodes,           &
          gis_dyn%global_lats_a, gis_dyn%lonsperlat,                         &
          gis_dyn%epse, gis_dyn%epso, gis_dyn%epsedn, gis_dyn%epsodn,        &
          gis_dyn%plnev_a, gis_dyn%plnod_a,                                  &
          gis_dyn%plnew_a, gis_dyn%plnow_a, gis_dyn%lats_nodes_a,            &
          gis_dyn%pwat, gis_dyn%ptot, gis_dyn%ptrc, gis_dyn%snnp1ev,         &
          gis_dyn%snnp1od)
!
          gis_dyn% start_step  = .true.
          gis_dyn% reset_step  = .false.
          gis_dyn% restart_step  = .false.
      else
        print *,'restart,filename=',trim(gis_dyn%nam_gfs_dyn%grid_ini), &
          trim(gis_dyn%nam_gfs_dyn%grid_ini2),trim(gis_dyn%nam_gfs_dyn%sig_ini), &
          trim(gis_dyn%nam_gfs_dyn%sig_ini2)
        call input_fields_rst(gis_dyn%nam_gfs_dyn%grid_ini,                  &
          gis_dyn%nam_gfs_dyn%grid_ini2,gis_dyn%nam_gfs_dyn%sig_ini,         &
          gis_dyn%nam_gfs_dyn%sig_ini2, gis_dyn%pdryini,                     &
          gis_dyn%trie_ls, gis_dyn%trio_ls,  gis_dyn%grid_gr ,               &
          gis_dyn%ls_node, gis_dyn%ls_nodes, gis_dyn%max_ls_nodes,           &
          gis_dyn%global_lats_a,  gis_dyn%lonsperlat,       &
          gis_dyn%epse, gis_dyn%epso, gis_dyn%plnev_a, gis_dyn%plnod_a,      &
          gis_dyn%plnew_a, gis_dyn%plnow_a, gis_dyn%snnp1ev,gis_dyn%snnp1od, &
          gis_dyn%lats_nodes_a)
!
          gis_dyn% start_step  = .false.
          gis_dyn% reset_step  = .false.
          gis_dyn% restart_step  = .true.
      endif
!!
        call countperf(1,18,0.)
!!
      tov = 0.0
      if (.not. (hybrid.or.gen_coord_hybrid) ) then                   ! hmhj
       call setsig(si,ci,del,sl,cl,rdel2,tov,me)
       am=-8888888.
       bm=-7777777.
       call amhmtm(del,sv,am)
       call bmdi_sig(ci,bm)
      endif
      if( ndslfv ) then
        if(lsidea ) then
          call idea_deldifs_init                                        &
                (SL,gis_dyn%LS_NODE,hybrid,gen_coord_hybrid)
        else
          call deldifs_noq                                              &
                  (gis_dyn%epse,gis_dyn%epse,gis_dyn%epse,              &
                   gis_dyn%epse,gis_dyn%epse,gis_dyn%epse,              &
                   gis_dyn%epso,gis_dyn%epso,                           &
                   gis_dyn%epso,gis_dyn%epso,                           &
                   gis_dyn%cons0,sl,gis_dyn%ls_node,gis_dyn%epse,       &
                   0,hybrid,gen_coord_hybrid)
        endif
      else
        if(lsidea ) then
          CALL idea_deldifs_init                                        &
                (SL,gis_dyn%LS_NODE,hybrid,gen_coord_hybrid)
        else
          call deldifs(gis_dyn%epse,gis_dyn%epse,gis_dyn%epse,		&
                   gis_dyn%epse,gis_dyn%epse,gis_dyn%epse,      	&
                   gis_dyn%epso,gis_dyn%epso,gis_dyn%epso,		&
                   gis_dyn%epso,gis_dyn%epso,gis_dyn%epso,      	& 
                   gis_dyn%cons0,sl,gis_dyn%ls_node,gis_dyn%epse,	&
                   0,hybrid,gen_coord_hybrid)  
        endif
      endif
 
!c
      call f_hpmstart(26,"step1")
!c
!!
      call countperf(1,18,0.)
!!
      gis_dyn%zhour        = fhour
      rc = 0
!
!
      end subroutine gfs_dynamics_initialize
!
! =========================================================================
!
      subroutine set_lonsgg(lonsperlat,jcap,lonf,latg,num_reduce,me)
      use gfs_dyn_reduce_lons_grid_module, only : gfs_dyn_reduce_grid   ! hmhj
      integer jcap,lonf,latg,num_reduce,me
      integer lonsperlat(latg)

      integer lonsperlat_62_192_94(94)
      integer lonsperlat_126_384_190(190)
      integer lonsperlat_170_512_256(256)
      integer lonsperlat_190_576_288(288)
      integer lonsperlat_254_768_384(384)
      integer lonsperlat_382_1152_576(576)
      integer lonsperlat_510_1536_766(766)
      integer lonsperlat_574_1760_880(880)
      integer lonsperlat_764_2304_1152(1152)
      integer lonsperlat_878_2640_1320(1320)
      integer lonsperlat_1278_3840_1920(1920)
! linear grid
      integer lonsperlat_574_1152_576(576)
      integer lonsperlat_1148_2304_1152(1152)

      integer lonsperlat_766_1536_768(768)
      integer lonsperlat_1534_3072_1536(1536)

      integer lonsperlat_1022_2048_1024(1024)
      integer lonsperlat_2046_4096_2048(2048)

      data lonsperlat_62_192_94/                                         &
        30,  30,  30,  40,  48,  56,  60,  72,  72,  80,  90,  90,       &
        96, 110, 110, 120, 120, 128, 144, 144, 144, 144, 154, 160,       &
       160, 168, 168, 180, 180, 180, 180, 180, 180, 192, 192, 192,       &
       192, 192, 192, 192, 192, 192, 192, 192, 192, 192, 192, 47*0 /

      data lonsperlat_126_384_190/                                       &
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
 
      data lonsperlat_170_512_256/                                       &
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
 
      data lonsperlat_190_576_288/                                       &
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
!
      data lonsperlat_254_768_384/                                       &
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
 
      data lonsperlat_382_1152_576/                                      &
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
!T510 
      data lonsperlat_510_1536_766/                                      &
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
!T574
      data lonsperlat_574_1760_880/                                      &
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
 
! T764 
      data lonsperlat_764_2304_1152/                                     &
          18,   22,   30,   40,   44,   56,   60,   66,   72,   80,      &
          88,   96,  110,  110,  112,  120,  126,  132,  140,  154,      &
         154,  160,  168,  176,  180,  192,  192,  198,  210,  220,      &
         220,  224,  240,  240,  252,  252,  256,  264,  280,  280,      &
         288,  308,  308,  308,  308,  320,  330,  330,  336,  352,      &
         352,  360,  360,  384,  384,  384,  396,  396,  420,  420,      &
         420,  420,  440,  440,  440,  448,  448,  462,  462,  480,      &
         480,  480,  504,  504,  504,  504,  512,  528,  528,  560,      &
         560,  560,  560,  560,  576,  576,  576,  616,  616,  616,      &
         616,  616,  616,  616,  630,  630,  640,  660,  660,  660,      &
         660,  672,  672,  704,  704,  704,  704,  704,  720,  720,      &
         720,  768,  768,  768,  768,  768,  768,  768,  770,  792,      &
         792,  792,  840,  840,  840,  840,  840,  840,  840,  840,      &
         840,  880,  880,  880,  880,  880,  880,  896,  896,  896,      &
         924,  924,  924,  924,  924,  960,  960,  960,  960,  960,      &
         960,  990,  990,  990,  990,  990, 1008, 1008, 1008, 1024,      &
        1024, 1024, 1056, 1056, 1056, 1056, 1056, 1056, 1120, 1120,      &
        1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1152,      &
        1152, 1152, 1152, 1152, 1152, 1232, 1232, 1232, 1232, 1232,      &
        1232, 1232, 1232, 1232, 1232, 1232, 1232, 1232, 1232, 1232,      &
        1260, 1260, 1260, 1260, 1260, 1280, 1280, 1280, 1280, 1320,      &
        1320, 1320, 1320, 1320, 1320, 1320, 1344, 1344, 1344, 1344,      &
        1344, 1386, 1386, 1386, 1386, 1386, 1386, 1386, 1386, 1408,      &
        1408, 1408, 1408, 1440, 1440, 1440, 1440, 1440, 1440, 1440,      &
        1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,      &
        1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,      &
        1584, 1584, 1584, 1584, 1584, 1584, 1584, 1584, 1584, 1584,      &
        1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680,      &
        1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680,      &
        1680, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760,      &
        1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760,      &
        1792, 1792, 1792, 1792, 1792, 1792, 1792, 1792, 1848, 1848,      &
        1848, 1848, 1848, 1848, 1848, 1848, 1848, 1848, 1848, 1848,      &
        1848, 1848, 1848, 1920, 1920, 1920, 1920, 1920, 1920, 1920,      &
        1920, 1920, 1920, 1920, 1920, 1920, 1920, 1920, 1920, 1920,      &
        1920, 1920, 1980, 1980, 1980, 1980, 1980, 1980, 1980, 1980,      &
        1980, 1980, 1980, 1980, 1980, 1980, 1980, 1980, 1980, 1980,      &
        2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016,      &
        2016, 2048, 2048, 2048, 2048, 2048, 2048, 2048, 2048, 2048,      &
        2048, 2048, 2112, 2112, 2112, 2112, 2112, 2112, 2112, 2112,      &
        2112, 2112, 2112, 2112, 2112, 2112, 2112, 2112, 2112, 2112,      &
        2112, 2112, 2112, 2112, 2112, 2240, 2240, 2240, 2240, 2240,      &
        2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240,      &
        2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240,      &
        2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240,      &
        2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240,      &
        2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240,      &
        2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2304,      &
        2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304,      &
        2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304,      &
        2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304,      &
        2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304,      &
        2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304,      &
        2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304,      &
        2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304,      &
        2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304,      &
        2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304,      &
        2304, 2304, 2304, 2304, 2304, 2304,  576*0/
!T878
      data lonsperlat_878_2640_1320/                                     &
         18,   28,   32,   42,   48,   56,   64,   72,   80,   84,       &
         96,  110,  110,  120,  120,  126,  132,  140,  154,  154,       &
        160,  168,  176,  180,  192,  192,  210,  210,  220,  220,       &
        240,  240,  240,  252,  252,  264,  280,  280,  280,  288,       &
        308,  308,  308,  320,  320,  330,  330,  336,  352,  352,       &
        360,  384,  384,  384,  384,  396,  396,  420,  420,  420,       &
        420,  440,  440,  440,  448,  462,  462,  480,  480,  480,       &
        504,  504,  504,  504,  512,  528,  528,  560,  560,  560,       &
        560,  560,  560,  576,  576,  616,  616,  616,  616,  616,       &
        616,  630,  630,  630,  640,  660,  660,  660,  672,  672,       &
        704,  704,  704,  704,  704,  704,  720,  720,  768,  768,       &
        768,  768,  768,  768,  768,  768,  792,  792,  792,  792,       &
        840,  840,  840,  840,  840,  840,  840,  840,  880,  880,       &
        880,  880,  880,  880,  896,  896,  896,  924,  924,  924,       &
        924,  924,  960,  960,  960,  960,  960,  960,  990,  990,       &
        990,  990,  990, 1008, 1008, 1008, 1024, 1024, 1056, 1056,       &
       1056, 1056, 1056, 1056, 1120, 1120, 1120, 1120, 1120, 1120,       &
       1120, 1120, 1120, 1120, 1152, 1152, 1152, 1152, 1152, 1152,       &
       1232, 1232, 1232, 1232, 1232, 1232, 1232, 1232, 1232, 1232,       &
       1232, 1232, 1232, 1232, 1260, 1260, 1260, 1260, 1260, 1280,       &
       1280, 1280, 1280, 1320, 1320, 1320, 1320, 1320, 1320, 1320,       &
       1344, 1344, 1344, 1344, 1386, 1386, 1386, 1386, 1386, 1386,       &
       1386, 1386, 1408, 1408, 1408, 1408, 1440, 1440, 1440, 1440,       &
       1440, 1440, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,       &
       1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,       &
       1584, 1584, 1584, 1584, 1584, 1584, 1584, 1584, 1584, 1584,       &
       1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680,       &
       1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1760,       &
       1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760,       &
       1760, 1760, 1760, 1760, 1760, 1792, 1792, 1792, 1792, 1792,       &
       1792, 1848, 1848, 1848, 1848, 1848, 1848, 1848, 1848, 1848,       &
       1848, 1848, 1848, 1848, 1920, 1920, 1920, 1920, 1920, 1920,       &
       1920, 1920, 1920, 1920, 1920, 1920, 1920, 1920, 1920, 1920,       &
       1980, 1980, 1980, 1980, 1980, 1980, 1980, 1980, 1980, 1980,       &
       1980, 1980, 1980, 1980, 2016, 2016, 2016, 2016, 2016, 2016,       &
       2016, 2016, 2016, 2048, 2048, 2048, 2048, 2048, 2048, 2048,       &
       2112, 2112, 2112, 2112, 2112, 2112, 2112, 2112, 2112, 2112,       &
       2112, 2112, 2112, 2112, 2112, 2112, 2112, 2240, 2240, 2240,       &
       2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240,       &
       2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240,       &
       2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240,       &
       2240, 2240, 2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304,       &
       2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304,       &
       2304, 2304, 2310, 2464, 2464, 2464, 2464, 2464, 2464, 2464,       &
       2464, 2464, 2464, 2464, 2464, 2464, 2464, 2464, 2464, 2464,       &
       2464, 2464, 2464, 2464, 2464, 2464, 2464, 2464, 2464, 2464,       &
       2464, 2464, 2464, 2464, 2464, 2464, 2464, 2464, 2464, 2464,       &
       2464, 2464, 2464, 2464, 2464, 2464, 2464, 2464, 2464, 2464,       &
       2464, 2464, 2464, 2464, 2464, 2464, 2464, 2464, 2520, 2520,       &
       2520, 2520, 2520, 2520, 2520, 2520, 2520, 2520, 2520, 2520,       &
       2520, 2520, 2520, 2520, 2520, 2520, 2520, 2520, 2520, 2520,       &
       2520, 2520, 2520, 2520, 2520, 2520, 2560, 2560, 2560, 2560,       &
       2560, 2560, 2560, 2560, 2560, 2560, 2560, 2560, 2560, 2560,       &
       2560, 2560, 2560, 2560, 2560, 2560, 2640, 2640, 2640, 2640,       &
       2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640,       &
       2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640,       &
       2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640,       &
       2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640,       &
       2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640,       &
       2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640,       &
       2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640,       &
       2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640,       &
       2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640,       &
       2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640,       &
       2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640,       &
       2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640,       &
        660*0/
!T1278
      data lonsperlat_1278_3840_1920/                                    &
         18,   24,   32,   42,   48,   56,   64,   70,   80,   84,       &
         90,  110,  110,  110,  120,  126,  132,  140,  154,  154,       &
        160,  168,  176,  180,  192,  192,  198,  210,  220,  220,       &
        224,  240,  240,  252,  252,  264,  264,  280,  280,  288,       &
        308,  308,  308,  320,  320,  330,  330,  336,  352,  352,       &
        360,  384,  384,  384,  384,  396,  396,  420,  420,  420,       &
        420,  440,  440,  440,  448,  462,  462,  480,  480,  480,       &
        504,  504,  504,  504,  512,  528,  528,  560,  560,  560,       &
        560,  560,  576,  576,  576,  616,  616,  616,  616,  616,       &
        616,  630,  630,  640,  640,  660,  660,  660,  672,  672,       &
        704,  704,  704,  704,  704,  720,  720,  768,  768,  768,       &
        768,  768,  768,  768,  768,  792,  792,  792,  792,  840,       &
        840,  840,  840,  840,  840,  840,  840,  880,  880,  880,       &
        880,  880,  880,  896,  896,  896,  924,  924,  924,  924,       &
        960,  960,  960,  960,  960,  960,  990,  990,  990,  990,       &
        990, 1008, 1008, 1008, 1024, 1024, 1056, 1056, 1056, 1056,       &
       1056, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120,       &
       1120, 1152, 1152, 1152, 1152, 1152, 1152, 1232, 1232, 1232,       &
       1232, 1232, 1232, 1232, 1232, 1232, 1232, 1232, 1232, 1232,       &
       1260, 1260, 1260, 1260, 1260, 1280, 1280, 1280, 1320, 1320,       &
       1320, 1320, 1320, 1320, 1320, 1344, 1344, 1344, 1344, 1386,       &
       1386, 1386, 1386, 1386, 1386, 1386, 1408, 1408, 1408, 1440,       &
       1440, 1440, 1440, 1440, 1440, 1536, 1536, 1536, 1536, 1536,       &
       1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,       &
       1536, 1540, 1584, 1584, 1584, 1584, 1584, 1584, 1584, 1584,       &
       1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680,       &
       1680, 1680, 1680, 1680, 1680, 1680, 1760, 1760, 1760, 1760,       &
       1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760,       &
       1792, 1792, 1792, 1792, 1792, 1848, 1848, 1848, 1848, 1848,       &
       1848, 1848, 1848, 1848, 1848, 1848, 1920, 1920, 1920, 1920,       &
       1920, 1920, 1920, 1920, 1920, 1920, 1920, 1920, 1920, 1980,       &
       1980, 1980, 1980, 1980, 1980, 1980, 1980, 1980, 1980, 1980,       &
       2016, 2016, 2016, 2016, 2016, 2016, 2048, 2048, 2048, 2048,       &
       2048, 2048, 2112, 2112, 2112, 2112, 2112, 2112, 2112, 2112,       &
       2112, 2112, 2112, 2112, 2240, 2240, 2240, 2240, 2240, 2240,       &
       2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240,       &
       2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2304, 2304,       &
       2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304,       &
       2304, 2310, 2464, 2464, 2464, 2464, 2464, 2464, 2464, 2464,       &
       2464, 2464, 2464, 2464, 2464, 2464, 2464, 2464, 2464, 2464,       &
       2464, 2464, 2464, 2464, 2464, 2464, 2464, 2464, 2464, 2464,       &
       2464, 2464, 2464, 2520, 2520, 2520, 2520, 2520, 2520, 2520,       &
       2520, 2520, 2520, 2520, 2520, 2560, 2560, 2560, 2560, 2560,       &
       2560, 2560, 2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640,       &
       2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640,       &
       2688, 2688, 2688, 2688, 2688, 2688, 2688, 2688, 2688, 2688,       &
       2688, 2772, 2772, 2772, 2772, 2772, 2772, 2772, 2772, 2772,       &
       2772, 2772, 2772, 2772, 2772, 2772, 2772, 2772, 2772, 2816,       &
       2816, 2816, 2816, 2816, 2816, 2816, 2816, 2816, 2816, 2880,       &
       2880, 2880, 2880, 2880, 2880, 2880, 2880, 2880, 2880, 2880,       &
       2880, 2880, 2880, 2880, 3072, 3072, 3072, 3072, 3072, 3072,       &
       3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072,       &
       3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072,       &
       3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072,       &
       3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072,       &
       3072, 3072, 3080, 3080, 3168, 3168, 3168, 3168, 3168, 3168,       &
       3168, 3168, 3168, 3168, 3168, 3168, 3168, 3168, 3168, 3168,       &
       3168, 3168, 3168, 3168, 3168, 3168, 3168, 3168, 3360, 3360,       &
       3360, 3360, 3360, 3360, 3360, 3360, 3360, 3360, 3360, 3360,       &
       3360, 3360, 3360, 3360, 3360, 3360, 3360, 3360, 3360, 3360,       &
       3360, 3360, 3360, 3360, 3360, 3360, 3360, 3360, 3360, 3360,       &
       3360, 3360, 3360, 3360, 3360, 3360, 3360, 3360, 3360, 3360,       &
       3360, 3360, 3360, 3360, 3360, 3360, 3360, 3360, 3360, 3360,       &
       3360, 3360, 3360, 3360, 3360, 3520, 3520, 3520, 3520, 3520,       &
       3520, 3520, 3520, 3520, 3520, 3520, 3520, 3520, 3520, 3520,       &
       3520, 3520, 3520, 3520, 3520, 3520, 3520, 3520, 3520, 3520,       &
       3520, 3520, 3520, 3520, 3520, 3520, 3520, 3520, 3520, 3520,       &
       3520, 3520, 3520, 3520, 3520, 3520, 3520, 3520, 3520, 3520,       &
       3520, 3520, 3520, 3520, 3520, 3520, 3520, 3520, 3520, 3520,       &
       3584, 3584, 3584, 3584, 3584, 3584, 3584, 3584, 3584, 3584,       &
       3584, 3584, 3584, 3584, 3584, 3584, 3584, 3584, 3584, 3584,       &
       3584, 3584, 3584, 3584, 3584, 3584, 3584, 3696, 3696, 3696,       &
       3696, 3696, 3696, 3696, 3696, 3696, 3696, 3696, 3696, 3696,       &
       3696, 3696, 3696, 3696, 3696, 3696, 3696, 3696, 3696, 3696,       &
       3696, 3696, 3696, 3696, 3696, 3696, 3696, 3696, 3696, 3696,       &
       3696, 3696, 3696, 3696, 3696, 3696, 3696, 3696, 3696, 3696,       &
       3696, 3696, 3696, 3696, 3696, 3696, 3696, 3696, 3696, 3696,       &
       3696, 3696, 3696, 3840, 3840, 3840, 3840, 3840, 3840, 3840,       &
       3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840,       &
       3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840,       &
       3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840,       &
       3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840,       &
       3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840,       &
       3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840,       &
       3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840,       &
       3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840,       &
       3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840,       &
       3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840,       &
       3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840,       &
       3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840,       &
       3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840,       &
       3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840,       &
       3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840,       &
       3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840,       &
       3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840,       &
       3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840,       &
         960*0/

! linear gaussian grid
! TL574
      data lonsperlat_574_1152_576/                                      &
        18,   28,   36,   44,   56,   60,   66,   72,   80,   88,        &
        96,  110,  110,  120,  120,  128,  140,  144,  154,  154,        &
       168,  168,  176,  192,  192,  198,  210,  210,  220,  220,        &
       240,  240,  240,  252,  252,  264,  280,  280,  280,  288,        &
       308,  308,  308,  320,  320,  330,  330,  336,  352,  352,        &
       360,  360,  384,  384,  384,  384,  396,  396,  420,  420,        &
       420,  440,  440,  440,  440,  448,  462,  462,  480,  480,        &
       480,  480,  504,  504,  504,  504,  512,  528,  528,  528,        &
       560,  560,  560,  560,  560,  576,  576,  576,  616,  616,        &
       616,  616,  616,  616,  616,  630,  630,  630,  640,  640,        &
       660,  660,  660,  660,  672,  672,  704,  704,  704,  704,        &
       704,  704,  720,  720,  720,  768,  768,  768,  768,  768,        &
       768,  768,  768,  768,  768,  792,  792,  792,  792,  792,        &
       840,  840,  840,  840,  840,  840,  840,  840,  840,  840,        &
       840,  880,  880,  880,  880,  880,  880,  880,  880,  880,        &
       896,  896,  896,  896,  924,  924,  924,  924,  924,  924,        &
       924,  960,  960,  960,  960,  960,  960,  960,  960,  960,        &
       990,  990,  990,  990,  990,  990,  990,  990,  990, 1008,        &
      1008, 1008, 1008, 1008, 1008, 1024, 1024, 1024, 1024, 1024,        &
      1056, 1056, 1056, 1056, 1056, 1056, 1056, 1056, 1056, 1056,        &
      1056, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120,        &
      1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120,        &
      1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120,        &
      1120, 1120, 1152, 1152, 1152, 1152, 1152, 1152, 1152, 1152,        &
      1152, 1152, 1152, 1152, 1152, 1152, 1152, 1152, 1152, 1152,        &
      1152, 1152, 1152, 1152, 1152, 1152, 1152, 1152, 1152, 1152,        &
      1152, 1152, 1152, 1152, 1152, 1152, 1152, 1152, 1152, 1152,        &
      1152, 1152, 1152, 1152, 1152, 1152, 1152, 1152, 1152, 1152,        &
      1152, 1152, 1152, 1152, 1152, 1152, 1152, 1152, 288*0/

!TL1148
      data lonsperlat_1148_2304_1152/                                    &
        18,   28,   36,   44,   56,   60,   66,   72,   80,   88,        &
        96,  110,  110,  120,  126,  128,  140,  144,  154,  154,        &
       168,  168,  176,  192,  192,  198,  210,  210,  220,  224,        &
       240,  240,  240,  252,  256,  264,  280,  280,  280,  288,        &
       308,  308,  308,  320,  320,  330,  336,  352,  352,  352,        &
       360,  384,  384,  384,  384,  396,  396,  420,  420,  420,        &
       440,  440,  440,  440,  448,  462,  462,  480,  480,  480,        &
       504,  504,  504,  504,  512,  528,  528,  560,  560,  560,        &
       560,  560,  560,  576,  576,  616,  616,  616,  616,  616,        &
       616,  616,  630,  630,  640,  660,  660,  660,  660,  672,        &
       672,  704,  704,  704,  704,  704,  720,  720,  720,  768,        &
       768,  768,  768,  768,  768,  768,  770,  792,  792,  792,        &
       840,  840,  840,  840,  840,  840,  840,  840,  880,  880,        &
       880,  880,  880,  880,  880,  896,  896,  896,  924,  924,        &
       924,  924,  924,  960,  960,  960,  960,  960,  960,  990,        &
       990,  990,  990,  990, 1008, 1008, 1008, 1024, 1024, 1024,        &
      1056, 1056, 1056, 1056, 1056, 1056, 1120, 1120, 1120, 1120,        &
      1120, 1120, 1120, 1120, 1120, 1120, 1120, 1152, 1152, 1152,        &
      1152, 1152, 1152, 1232, 1232, 1232, 1232, 1232, 1232, 1232,        &
      1232, 1232, 1232, 1232, 1232, 1232, 1232, 1260, 1260, 1260,        &
      1260, 1260, 1260, 1280, 1280, 1280, 1320, 1320, 1320, 1320,        &
      1320, 1320, 1320, 1320, 1344, 1344, 1344, 1344, 1344, 1386,        &
      1386, 1386, 1386, 1386, 1386, 1386, 1386, 1408, 1408, 1408,        &
      1408, 1440, 1440, 1440, 1440, 1440, 1440, 1536, 1536, 1536,        &
      1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,        &
      1536, 1536, 1536, 1536, 1536, 1536, 1536, 1540, 1584, 1584,        &
      1584, 1584, 1584, 1584, 1584, 1584, 1584, 1680, 1680, 1680,        &
      1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680,        &
      1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1760,        &
      1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760,        &
      1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1792, 1792,        &
      1792, 1792, 1792, 1792, 1792, 1792, 1848, 1848, 1848, 1848,        &
      1848, 1848, 1848, 1848, 1848, 1848, 1848, 1848, 1848, 1848,        &
      1920, 1920, 1920, 1920, 1920, 1920, 1920, 1920, 1920, 1920,        &
      1920, 1920, 1920, 1920, 1920, 1920, 1920, 1920, 1920, 1980,        &
      1980, 1980, 1980, 1980, 1980, 1980, 1980, 1980, 1980, 1980,        &
      1980, 1980, 1980, 1980, 1980, 1980, 1980, 2016, 2016, 2016,        &
      2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2048, 2048,        &
      2048, 2048, 2048, 2048, 2048, 2048, 2048, 2048, 2048, 2112,        &
      2112, 2112, 2112, 2112, 2112, 2112, 2112, 2112, 2112, 2112,        &
      2112, 2112, 2112, 2112, 2112, 2112, 2112, 2112, 2112, 2112,        &
      2112, 2112, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240,        &
      2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240,        &
      2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240,        &
      2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240,        &
      2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240,        &
      2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240,        &
      2240, 2240, 2240, 2240, 2240, 2304, 2304, 2304, 2304, 2304,        &
      2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304,        &
      2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304,        &
      2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304,        &
      2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304,        &
      2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304,        &
      2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304,        &
      2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304,        &
      2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304,        &
      2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304,        &
      2304, 2304, 2304, 2304, 2304, 2304, 576*0/

!TL766
      data lonsperlat_766_1536_768/                                      &
        18,   28,   36,   44,   56,   60,   66,   72,   80,   88,        &
        96,  110,  110,  120,  126,  128,  140,  144,  154,  154,        &
       168,  168,  176,  192,  192,  198,  210,  210,  220,  224,        &
       240,  240,  240,  252,  256,  264,  280,  280,  280,  288,        &
       308,  308,  308,  320,  320,  330,  330,  352,  352,  352,        &
       360,  384,  384,  384,  384,  396,  396,  420,  420,  420,        &
       420,  440,  440,  440,  448,  462,  462,  462,  480,  480,        &
       504,  504,  504,  504,  512,  512,  528,  528,  560,  560,        &
       560,  560,  560,  560,  576,  576,  616,  616,  616,  616,        &
       616,  616,  616,  630,  630,  640,  640,  660,  660,  660,        &
       660,  672,  672,  704,  704,  704,  704,  704,  720,  720,        &
       720,  768,  768,  768,  768,  768,  768,  768,  768,  768,        &
       792,  792,  792,  792,  840,  840,  840,  840,  840,  840,        &
       840,  840,  840,  880,  880,  880,  880,  880,  880,  880,        &
       896,  896,  896,  924,  924,  924,  924,  924,  924,  960,        &
       960,  960,  960,  960,  960,  960,  990,  990,  990,  990,        &
       990,  990, 1008, 1008, 1008, 1024, 1024, 1024, 1024, 1056,        &
      1056, 1056, 1056, 1056, 1056, 1056, 1120, 1120, 1120, 1120,        &
      1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120,        &
      1152, 1152, 1152, 1152, 1152, 1152, 1152, 1232, 1232, 1232,        &
      1232, 1232, 1232, 1232, 1232, 1232, 1232, 1232, 1232, 1232,        &
      1232, 1232, 1232, 1232, 1232, 1232, 1232, 1260, 1260, 1260,        &
      1260, 1260, 1260, 1260, 1280, 1280, 1280, 1280, 1280, 1280,        &
      1320, 1320, 1320, 1320, 1320, 1320, 1320, 1320, 1320, 1320,        &
      1320, 1344, 1344, 1344, 1344, 1344, 1344, 1344, 1344, 1386,        &
      1386, 1386, 1386, 1386, 1386, 1386, 1386, 1386, 1386, 1386,        &
      1386, 1386, 1386, 1408, 1408, 1408, 1408, 1408, 1408, 1408,        &
      1408, 1440, 1440, 1440, 1440, 1440, 1440, 1440, 1440, 1440,        &
      1440, 1440, 1440, 1440, 1536, 1536, 1536, 1536, 1536, 1536,        &
      1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,        &
      1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,        &
      1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,        &
      1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,        &
      1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,        &
      1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,        &
      1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,        &
      1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,        &
      1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,        &
      1536, 1536, 1536, 1536, 384*0/

!TL1534
      data lonsperlat_1534_3072_1536/                                    &
        18,   28,   36,   44,   56,   60,   66,   72,   80,   88,        &
        96,  110,  110,  120,  126,  128,  140,  144,  154,  160,        &
       168,  168,  176,  192,  192,  198,  210,  210,  220,  224,        &
       240,  240,  252,  252,  256,  264,  280,  280,  280,  288,        &
       308,  308,  308,  320,  320,  330,  336,  352,  352,  352,        &
       360,  384,  384,  384,  384,  396,  420,  420,  420,  420,        &
       440,  440,  440,  448,  462,  462,  462,  480,  480,  504,        &
       504,  504,  504,  512,  528,  528,  528,  560,  560,  560,        &
       560,  560,  576,  576,  576,  616,  616,  616,  616,  616,        &
       616,  630,  630,  640,  640,  660,  660,  660,  672,  672,        &
       704,  704,  704,  704,  704,  720,  720,  768,  768,  768,        &
       768,  768,  768,  768,  768,  770,  792,  792,  792,  840,        &
       840,  840,  840,  840,  840,  840,  840,  880,  880,  880,        &
       880,  880,  880,  896,  896,  896,  924,  924,  924,  924,        &
       924,  960,  960,  960,  960,  960,  990,  990,  990,  990,        &
       990, 1008, 1008, 1008, 1024, 1024, 1024, 1056, 1056, 1056,        &
      1056, 1056, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120,        &
      1120, 1120, 1120, 1152, 1152, 1152, 1152, 1152, 1232, 1232,        &
      1232, 1232, 1232, 1232, 1232, 1232, 1232, 1232, 1232, 1232,        &
      1232, 1232, 1260, 1260, 1260, 1260, 1260, 1280, 1280, 1280,        &
      1320, 1320, 1320, 1320, 1320, 1320, 1320, 1344, 1344, 1344,        &
      1344, 1386, 1386, 1386, 1386, 1386, 1386, 1386, 1386, 1408,        &
      1408, 1408, 1408, 1440, 1440, 1440, 1440, 1440, 1536, 1536,        &
      1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,        &
      1536, 1536, 1536, 1536, 1536, 1540, 1584, 1584, 1584, 1584,        &
      1584, 1584, 1584, 1584, 1680, 1680, 1680, 1680, 1680, 1680,        &
      1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680,        &
      1680, 1680, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760,        &
      1760, 1760, 1760, 1760, 1760, 1760, 1760, 1792, 1792, 1792,        &
      1792, 1792, 1792, 1848, 1848, 1848, 1848, 1848, 1848, 1848,        &
      1848, 1848, 1848, 1848, 1920, 1920, 1920, 1920, 1920, 1920,        &
      1920, 1920, 1920, 1920, 1920, 1920, 1920, 1920, 1980, 1980,        &
      1980, 1980, 1980, 1980, 1980, 1980, 1980, 1980, 1980, 1980,        &
      1980, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2048, 2048,        &
      2048, 2048, 2048, 2048, 2048, 2112, 2112, 2112, 2112, 2112,        &
      2112, 2112, 2112, 2112, 2112, 2112, 2112, 2112, 2240, 2240,        &
      2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240,        &
      2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240,        &
      2240, 2240, 2240, 2240, 2240, 2240, 2240, 2304, 2304, 2304,        &
      2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304, 2304,        &
      2304, 2304, 2310, 2464, 2464, 2464, 2464, 2464, 2464, 2464,        &
      2464, 2464, 2464, 2464, 2464, 2464, 2464, 2464, 2464, 2464,        &
      2464, 2464, 2464, 2464, 2464, 2464, 2464, 2464, 2464, 2464,        &
      2464, 2464, 2464, 2464, 2464, 2464, 2464, 2464, 2464, 2464,        &
      2464, 2464, 2520, 2520, 2520, 2520, 2520, 2520, 2520, 2520,        &
      2520, 2520, 2520, 2520, 2520, 2520, 2520, 2560, 2560, 2560,        &
      2560, 2560, 2560, 2560, 2560, 2560, 2560, 2560, 2640, 2640,        &
      2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640,        &
      2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640,        &
      2640, 2688, 2688, 2688, 2688, 2688, 2688, 2688, 2688, 2688,        &
      2688, 2688, 2688, 2688, 2688, 2688, 2772, 2772, 2772, 2772,        &
      2772, 2772, 2772, 2772, 2772, 2772, 2772, 2772, 2772, 2772,        &
      2772, 2772, 2772, 2772, 2772, 2772, 2772, 2772, 2772, 2772,        &
      2772, 2772, 2772, 2772, 2772, 2816, 2816, 2816, 2816, 2816,        &
      2816, 2816, 2816, 2816, 2816, 2816, 2816, 2816, 2816, 2816,        &
      2816, 2880, 2880, 2880, 2880, 2880, 2880, 2880, 2880, 2880,        &
      2880, 2880, 2880, 2880, 2880, 2880, 2880, 2880, 2880, 2880,        &
      2880, 2880, 2880, 2880, 2880, 2880, 2880, 2880, 3072, 3072,        &
      3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072,        &
      3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072,        &
      3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072,        &
      3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072,        &
      3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072,        &
      3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072,        &
      3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072,        &
      3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072,        &
      3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072,        &
      3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072,        &
      3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072,        &
      3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072,        &
      3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072,        &
      3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072,        &
      3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072,        &
      3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072,        &
      3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072,        &
      3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072,        &
      3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 768*0/

!TL1022
      data lonsperlat_1022_2048_1024/                                    &
        18,   28,   36,   44,   56,   60,   66,   72,   80,   88,        &
        96,  110,  110,  120,  126,  128,  140,  144,  154,  160,        &
       168,  168,  176,  192,  192,  198,  210,  210,  220,  224,        &
       240,  240,  252,  252,  256,  264,  280,  280,  280,  288,        &
       308,  308,  308,  320,  320,  330,  336,  352,  352,  352,        &
       360,  384,  384,  384,  384,  396,  396,  420,  420,  420,        &
       440,  440,  440,  440,  448,  462,  462,  480,  480,  480,        &
       504,  504,  504,  504,  512,  528,  528,  528,  560,  560,        &
       560,  560,  560,  576,  576,  616,  616,  616,  616,  616,        &
       616,  616,  630,  630,  640,  640,  660,  660,  660,  672,        &
       672,  704,  704,  704,  704,  704,  720,  720,  720,  768,        &
       768,  768,  768,  768,  768,  768,  768,  792,  792,  792,        &
       792,  840,  840,  840,  840,  840,  840,  840,  840,  880,        &
       880,  880,  880,  880,  880,  880,  896,  896,  896,  924,        &
       924,  924,  924,  924,  960,  960,  960,  960,  960,  960,        &
       990,  990,  990,  990,  990, 1008, 1008, 1008, 1024, 1024,        &
      1024, 1056, 1056, 1056, 1056, 1056, 1056, 1120, 1120, 1120,        &
      1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1152,        &
      1152, 1152, 1152, 1152, 1152, 1232, 1232, 1232, 1232, 1232,        &
      1232, 1232, 1232, 1232, 1232, 1232, 1232, 1232, 1232, 1232,        &
      1260, 1260, 1260, 1260, 1260, 1260, 1280, 1280, 1280, 1280,        &
      1320, 1320, 1320, 1320, 1320, 1320, 1320, 1320, 1344, 1344,        &
      1344, 1344, 1344, 1386, 1386, 1386, 1386, 1386, 1386, 1386,        &
      1386, 1386, 1408, 1408, 1408, 1408, 1440, 1440, 1440, 1440,        &
      1440, 1440, 1440, 1536, 1536, 1536, 1536, 1536, 1536, 1536,        &
      1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,        &
      1536, 1536, 1536, 1536, 1536, 1540, 1584, 1584, 1584, 1584,        &
      1584, 1584, 1584, 1584, 1584, 1584, 1584, 1680, 1680, 1680,        &
      1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680,        &
      1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680,        &
      1680, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760,        &
      1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760,        &
      1760, 1760, 1760, 1760, 1792, 1792, 1792, 1792, 1792, 1792,        &
      1792, 1792, 1792, 1792, 1848, 1848, 1848, 1848, 1848, 1848,        &
      1848, 1848, 1848, 1848, 1848, 1848, 1848, 1848, 1848, 1848,        &
      1848, 1848, 1848, 1920, 1920, 1920, 1920, 1920, 1920, 1920,        &
      1920, 1920, 1920, 1920, 1920, 1920, 1920, 1920, 1920, 1920,        &
      1920, 1920, 1920, 1920, 1920, 1920, 1920, 1920, 1920, 1920,        &
      1920, 1920, 1980, 1980, 1980, 1980, 1980, 1980, 1980, 1980,        &
      1980, 1980, 1980, 1980, 1980, 1980, 1980, 1980, 1980, 1980,        &
      1980, 1980, 1980, 1980, 1980, 1980, 1980, 1980, 1980, 1980,        &
      1980, 1980, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016,        &
      2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016,        &
      2016, 2016, 2016, 2016, 2016, 2016, 2016, 2048, 2048, 2048,        &
      2048, 2048, 2048, 2048, 2048, 2048, 2048, 2048, 2048, 2048,        &
      2048, 2048, 2048, 2048, 2048, 2048, 2048, 2048, 2048, 2048,        &
      2048, 2048, 2048, 2048, 2048, 2048, 2048, 2048, 2048, 2048,        &
      2048, 2048, 2048, 2048, 2048, 2048, 2048, 2048, 2048, 2048,        &
      2048, 2048, 2048, 2048, 2048, 2048, 2048, 2048, 2048, 2048,        &
      2048, 2048, 2048, 2048, 2048, 2048, 2048, 2048, 2048, 2048,        &
      2048, 2048, 2048, 2048, 2048, 2048, 2048, 2048, 2048, 2048,        &
      2048, 2048, 512*0/

!TL2046
      data lonsperlat_2046_4096_2048/                                    &
        18,   28,   36,   44,   56,   60,   66,   72,   80,   88,        &
        96,  110,  110,  120,  126,  128,  140,  144,  154,  160,        &
       168,  168,  176,  192,  192,  198,  210,  210,  220,  224,        &
       240,  240,  252,  252,  256,  264,  280,  280,  288,  288,        &
       308,  308,  308,  320,  320,  330,  336,  352,  352,  352,        &
       360,  384,  384,  384,  396,  396,  420,  420,  420,  420,        &
       440,  440,  440,  448,  462,  462,  462,  480,  480,  504,        &
       504,  504,  504,  512,  528,  528,  528,  560,  560,  560,        &
       560,  560,  576,  576,  616,  616,  616,  616,  616,  616,        &
       616,  630,  630,  640,  660,  660,  660,  660,  672,  704,        &
       704,  704,  704,  704,  720,  720,  720,  768,  768,  768,        &
       768,  768,  768,  768,  768,  792,  792,  792,  840,  840,        &
       840,  840,  840,  840,  840,  840,  880,  880,  880,  880,        &
       880,  880,  896,  896,  896,  924,  924,  924,  924,  960,        &
       960,  960,  960,  960,  960,  990,  990,  990,  990,  990,        &
      1008, 1008, 1008, 1024, 1024, 1024, 1056, 1056, 1056, 1056,        &
      1056, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120,        &
      1120, 1152, 1152, 1152, 1152, 1152, 1232, 1232, 1232, 1232,        &
      1232, 1232, 1232, 1232, 1232, 1232, 1232, 1232, 1232, 1260,        &
      1260, 1260, 1260, 1260, 1280, 1280, 1280, 1320, 1320, 1320,        &
      1320, 1320, 1320, 1320, 1344, 1344, 1344, 1344, 1386, 1386,        &
      1386, 1386, 1386, 1386, 1386, 1408, 1408, 1408, 1408, 1440,        &
      1440, 1440, 1440, 1440, 1536, 1536, 1536, 1536, 1536, 1536,        &
      1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,        &
      1540, 1584, 1584, 1584, 1584, 1584, 1584, 1584, 1680, 1680,        &
      1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680,        &
      1680, 1680, 1680, 1680, 1680, 1760, 1760, 1760, 1760, 1760,        &
      1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1792,        &
      1792, 1792, 1792, 1792, 1848, 1848, 1848, 1848, 1848, 1848,        &
      1848, 1848, 1848, 1848, 1920, 1920, 1920, 1920, 1920, 1920,        &
      1920, 1920, 1920, 1920, 1920, 1920, 1920, 1980, 1980, 1980,        &
      1980, 1980, 1980, 1980, 1980, 1980, 1980, 1980, 2016, 2016,        &
      2016, 2016, 2016, 2016, 2048, 2048, 2048, 2048, 2048, 2048,        &
      2112, 2112, 2112, 2112, 2112, 2112, 2112, 2112, 2112, 2112,        &
      2112, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240,        &
      2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240, 2240,        &
      2240, 2240, 2240, 2240, 2240, 2304, 2304, 2304, 2304, 2304,        &
      2304, 2304, 2304, 2304, 2304, 2304, 2304, 2310, 2310, 2464,        &
      2464, 2464, 2464, 2464, 2464, 2464, 2464, 2464, 2464, 2464,        &
      2464, 2464, 2464, 2464, 2464, 2464, 2464, 2464, 2464, 2464,        &
      2464, 2464, 2464, 2464, 2464, 2464, 2464, 2464, 2520, 2520,        &
      2520, 2520, 2520, 2520, 2520, 2520, 2520, 2520, 2520, 2560,        &
      2560, 2560, 2560, 2560, 2560, 2560, 2560, 2640, 2640, 2640,        &
      2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640, 2640,        &
      2640, 2640, 2640, 2640, 2688, 2688, 2688, 2688, 2688, 2688,        &
      2688, 2688, 2688, 2688, 2772, 2772, 2772, 2772, 2772, 2772,        &
      2772, 2772, 2772, 2772, 2772, 2772, 2772, 2772, 2772, 2772,        &
      2772, 2816, 2816, 2816, 2816, 2816, 2816, 2816, 2816, 2816,        &
      2816, 2880, 2880, 2880, 2880, 2880, 2880, 2880, 2880, 2880,        &
      2880, 2880, 2880, 2880, 2880, 3072, 3072, 3072, 3072, 3072,        &
      3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072,        &
      3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072,        &
      3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072,        &
      3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3072, 3080,        &
      3080, 3168, 3168, 3168, 3168, 3168, 3168, 3168, 3168, 3168,        &
      3168, 3168, 3168, 3168, 3168, 3168, 3168, 3168, 3168, 3168,        &
      3168, 3168, 3360, 3360, 3360, 3360, 3360, 3360, 3360, 3360,        &
      3360, 3360, 3360, 3360, 3360, 3360, 3360, 3360, 3360, 3360,        &
      3360, 3360, 3360, 3360, 3360, 3360, 3360, 3360, 3360, 3360,        &
      3360, 3360, 3360, 3360, 3360, 3360, 3360, 3360, 3360, 3360,        &
      3360, 3360, 3360, 3360, 3360, 3360, 3360, 3360, 3360, 3360,        &
      3360, 3360, 3520, 3520, 3520, 3520, 3520, 3520, 3520, 3520,        &
      3520, 3520, 3520, 3520, 3520, 3520, 3520, 3520, 3520, 3520,        &
      3520, 3520, 3520, 3520, 3520, 3520, 3520, 3520, 3520, 3520,        &
      3520, 3520, 3520, 3520, 3520, 3520, 3520, 3520, 3520, 3520,        &
      3520, 3520, 3520, 3520, 3520, 3520, 3520, 3520, 3584, 3584,        &
      3584, 3584, 3584, 3584, 3584, 3584, 3584, 3584, 3584, 3584,        &
      3584, 3584, 3584, 3584, 3584, 3584, 3584, 3584, 3696, 3696,        &
      3696, 3696, 3696, 3696, 3696, 3696, 3696, 3696, 3696, 3696,        &
      3696, 3696, 3696, 3696, 3696, 3696, 3696, 3696, 3696, 3696,        &
      3696, 3696, 3696, 3696, 3696, 3696, 3696, 3696, 3696, 3696,        &
      3696, 3696, 3696, 3696, 3696, 3696, 3696, 3840, 3840, 3840,        &
      3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840,        &
      3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840,        &
      3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840,        &
      3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840,        &
      3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840, 3840,        &
      3840, 3840, 3840, 3840, 3960, 3960, 3960, 3960, 3960, 3960,        &
      3960, 3960, 3960, 3960, 3960, 3960, 3960, 3960, 3960, 3960,        &
      3960, 3960, 3960, 3960, 3960, 3960, 3960, 3960, 3960, 3960,        &
      3960, 3960, 3960, 3960, 3960, 3960, 3960, 3960, 3960, 3960,        &
      3960, 3960, 3960, 3960, 3960, 3960, 3960, 3960, 3960, 3960,        &
      3960, 3960, 3960, 3960, 3960, 3960, 3960, 3960, 3960, 3960,        &
      3960, 3960, 3960, 3960, 3960, 3960, 4032, 4032, 4032, 4032,        &
      4032, 4032, 4032, 4032, 4032, 4032, 4032, 4032, 4032, 4032,        &
      4032, 4032, 4032, 4032, 4032, 4032, 4032, 4032, 4032, 4032,        &
      4032, 4032, 4032, 4032, 4032, 4032, 4032, 4032, 4032, 4032,        &
      4032, 4032, 4032, 4032, 4032, 4032, 4032, 4032, 4032, 4032,        &
      4032, 4032, 4032, 4032, 4032, 4032, 4032, 4096, 4096, 4096,        &
      4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096,        &
      4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096,        &
      4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096,        &
      4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096,        &
      4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096,        &
      4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096,        &
      4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096,        &
      4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096,        &
      4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096,        &
      4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096,        &
      4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096,        &
      4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096,        &
      4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096,        &
      4096, 4096, 4096, 4096, 1024*0/

      if (num_reduce == 0) then

! quadratic grid
        if (jcap==62 .and. lonf==192 .and. latg==94) then
          lonsperlat=lonsperlat_62_192_94
        else if (jcap==126 .and. lonf==384 .and. latg==190) then
          lonsperlat=lonsperlat_126_384_190
        else if (jcap==170 .and. lonf==512 .and. latg==256) then
          lonsperlat=lonsperlat_170_512_256
        else if (jcap==190 .and. lonf==576 .and. latg==288) then
          lonsperlat=lonsperlat_190_576_288
        else if (jcap==254 .and. lonf==768 .and. latg==384) then
          lonsperlat=lonsperlat_254_768_384
        else if (jcap==382 .and. lonf==1152 .and. latg==576) then
          lonsperlat=lonsperlat_382_1152_576
        else if (jcap==510 .and. lonf==1536 .and. latg==766) then
          lonsperlat=lonsperlat_510_1536_766
        else if (jcap==574 .and. lonf==1760 .and. latg==880) then
          lonsperlat=lonsperlat_574_1760_880
        else if (jcap==764 .and. lonf==2304 .and. latg==1152) then
          lonsperlat=lonsperlat_764_2304_1152
        else if (jcap==878 .and. lonf==2640 .and. latg==1320) then
          lonsperlat=lonsperlat_878_2640_1320
        else if (jcap==1278 .and. lonf==3840 .and. latg==1920) then
          lonsperlat=lonsperlat_1278_3840_1920
! linear grid
        else if (jcap==574 .and. lonf==1152 .and. latg==576) then
          lonsperlat= lonsperlat_574_1152_576
        else if (jcap==766 .and. lonf==1536 .and. latg==768) then
          lonsperlat= lonsperlat_766_1536_768
        else if (jcap==1022 .and. lonf==2048 .and. latg==1024) then
          lonsperlat= lonsperlat_1022_2048_1024
        else if (jcap==1148 .and. lonf==2304 .and. latg==1152) then
          lonsperlat= lonsperlat_1148_2304_1152
        else if (jcap==1534 .and. lonf==3072 .and. latg==1536) then
          lonsperlat= lonsperlat_1534_3072_1536
        else if (jcap==2046 .and. lonf==4096 .and. latg==2048) then
          lonsperlat= lonsperlat_2046_4096_2048
        else
          print *,' Resolution not supported '
          print *,' Use num_reduce=4 for reduced grid '
          call mpi_quit(55)
        endif

      else

! compute reduced grid using juang 2004
         if ( me == 0 ) then
           print*,' Non Standard Resolution-lonsperlat computed locally'
         endif
         call gfs_dyn_reduce_grid (abs(num_reduce),                     &
                                   jcap,lonf,latg,lonsperlat)      ! hmhj
         if ( me == 0 ) then
           print*,' Reduced grid is computed by juang (2004) '     ! hmhj
!          print *,' lonsperlat ',lonsperlat
         endif

      endif

      if ( me == 0 ) then
        print*,' jcap = ',jcap
        print*,'min,max of lonsperlat = ',minval(lonsperlat),            &
                maxval(lonsperlat)
      endif


      end subroutine set_lonsgg

      end module gfs_dynamics_initialize_mod
