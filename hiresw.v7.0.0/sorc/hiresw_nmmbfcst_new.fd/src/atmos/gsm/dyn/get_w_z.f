      module get_variables_for_WAM_IPE_coupling

      use gfs_dyn_resol_def, ONLY: lonf, levs, levh
      use gfs_dyn_layout1,   ONLY: lats_node_a
      use gfs_dyn_machine

      IMPLICIT NONE

      REAL, DIMENSION(:, :, :), ALLOCATABLE :: wwg, zzg, uug, 
     &                                         vvg, ttg, rqg,
     &                                         n2g

      END module get_variables_for_WAM_IPE_coupling

      subroutine get_w_z(grid_gr,
     &                 trie_ls,trio_ls,
     &                 LS_NODE,LS_NODES,MAX_LS_NODES,
     &                 LATS_NODES_A,GLOBAL_LATS_A,LONSPERLAT,
     &                 EPSE,EPSO,EPSEDN,EPSODN,
     &                 PLNEV_A,PLNOD_A,PLNEW_A,PLNOW_A,
     &                 PDDEV_A,PDDOD_A,SNNP1EV,SNNP1OD)
!!
! Program History Log:
! Mar 2015    Henry Juang	use existed variables to get w hydrostatically
! Feb 2016    Weiyu Yang        add u, v, t variables for the WAM-IPE coupling.
!                               The outputs for coupling are: uug --> u,
!                                                             vvg --> v,
!                                                             ttg --> t,
!                                                             wwg --> w,
!                                                             zzg --> z.
!                                                    rqg --> q, oz, clw, O1, O2
!                                                             n2g --> N2.
! Mar 2016    Sajal Kar         zzg & wwg for g(z) when var_g=true
! Apr 2016    Weiyu Yang        Bug fix (in call gfidi_hyb_gc_h_w_z)
!----------------------------------------------
!!
      use gfs_dyn_resol_def
      use gfs_dyn_layout1
      use gfs_dyn_gg_def
      use gfs_dyn_vert_def
      use gfs_dyn_coordinate_def
      use gfs_dyn_date_def
      use namelist_dynamics_def
      use gfs_dyn_mpi_def
      use gfs_dyn_dfi_mod
      use gfs_dyn_physcons, only: p0 => con_p0, fv => con_fvirt
     &,                           re => con_rerth, g0 => con_g
      use do_dynamics_mod
      use gfs_dyn_tracer_const, only: cpi
      use get_variables_for_WAM_IPE_coupling
!!     
      IMPLICIT NONE

      INTEGER,INTENT(IN):: LONSPERLAT(LATG)
!!     
      REAL(KIND=KIND_EVOD) TRIE_LS(LEN_TRIE_LS,2,LOTls)
      REAL(KIND=KIND_EVOD) TRIO_LS(LEN_TRIO_LS,2,LOTls)
      REAL(KIND=KIND_GRID) GRID_GR(lonf*lats_node_a_max,lotgr)

      integer          ls_node(ls_dim,3)
!
      INTEGER          LS_NODES(LS_DIM,NODES)
      INTEGER          MAX_LS_NODES   (NODES)
      INTEGER          LATS_NODES_A   (NODES)
      INTEGER          GLOBAL_LATS_A(LATG)
!
      REAL(KIND=KIND_EVOD)      EPSE(LEN_TRIE_LS)
      REAL(KIND=KIND_EVOD)      EPSO(LEN_TRIO_LS)
      REAL(KIND=KIND_EVOD)    EPSEDN(LEN_TRIE_LS)
      REAL(KIND=KIND_EVOD)    EPSODN(LEN_TRIO_LS)
      REAL(KIND=KIND_EVOD)   SNNP1EV(LEN_TRIE_LS)
      REAL(KIND=KIND_EVOD)   SNNP1OD(LEN_TRIO_LS)

      REAL(KIND=KIND_EVOD)   PLNEV_A(LEN_TRIE_LS,LATG2)
      REAL(KIND=KIND_EVOD)   PLNOD_A(LEN_TRIO_LS,LATG2)
      REAL(KIND=KIND_EVOD)   PLNEW_A(LEN_TRIE_LS,LATG2)
      REAL(KIND=KIND_EVOD)   PLNOW_A(LEN_TRIO_LS,LATG2)
      REAL(KIND=KIND_EVOD)   PDDEV_A(LEN_TRIE_LS,LATG2)
      REAL(KIND=KIND_EVOD)   PDDOD_A(LEN_TRIO_LS,LATG2)

      REAL(KIND=KIND_EVOD) SYN_GR_A_1(LONFX*LOTS,LATS_DIM_EXT)
      REAL(KIND=KIND_EVOD) DYN_GR_A_1(LONFX*LOTD,LATS_DIM_EXT)
      REAL(KIND=KIND_EVOD) ANL_GR_A_1(LONFX*LOTA,LATS_DIM_EXT)

      REAL(KIND=KIND_EVOD) SYN_GR_A_2(LONFX*LOTS,LATS_DIM_EXT)
      REAL(KIND=KIND_EVOD) DYN_GR_A_2(LONFX*LOTD,LATS_DIM_EXT)
      REAL(KIND=KIND_EVOD) ANL_GR_A_2(LONFX*LOTA,LATS_DIM_EXT)

      REAL(KIND=KIND_EVOD) SYN_GR_S_Z(LONFX     ,LATS_DIM_EXT)
      REAL(KIND=KIND_EVOD) ANL_GR_A_W(LONFX*levs,LATS_DIM_EXT)
      REAL(KIND=KIND_EVOD) ANL_GR_A_Z(LONFX*levs,LATS_DIM_EXT)

      REAL(KIND=KIND_GRID) tfac(lonf, levs), sumq(lonf, levs)
      REAL(KIND=KIND_GRID) tx1
      REAL(KIND=KIND_GRID), PARAMETER :: qmin=1.e-10
!!     
      REAL (KIND=KIND_grid), parameter :: cons0=0.0d0, cons1=1.0d0,
     &                                    cons2=2.0d0, cons0p5 = 0.5d0
!sk
      REAL(KIND=KIND_GRID) zs,phi,grav
      REAL(KIND=KIND_GRID) phis(lonf,lats_node_a)
      real, parameter:: g0re = g0*re, g0re2 = g0*re*re

      INTEGER               I,J,K,L,LOCL,N

      integer               lan,lat,nt
      integer               lon_dim,lons_lat,node
      integer               njeff,lon,jlonf,ilan
     &,                     ngptcd, nn, nnl

      IF(.NOT. ALLOCATED(wwg)) ALLOCATE(wwg(lonf,lats_node_a,levs))
      IF(.NOT. ALLOCATED(zzg)) ALLOCATE(zzg(lonf,lats_node_a,levs))
      IF(.NOT. ALLOCATED(uug)) ALLOCATE(uug(lonf,lats_node_a,levs))
      IF(.NOT. ALLOCATED(vvg)) ALLOCATE(vvg(lonf,lats_node_a,levs))
      IF(.NOT. ALLOCATED(ttg)) ALLOCATE(ttg(lonf,lats_node_a,levs))
      IF(.NOT. ALLOCATED(rqg)) ALLOCATE(rqg(lonf,lats_node_a,levh))
      IF(.NOT. ALLOCATED(n2g)) ALLOCATE(n2g(lonf,lats_node_a,levs))
!!     
!
!     print *,' -----------------get_initial_w_inp ------ '
!
      ngptcd = ngptc
!
!----------------------------------------------------------
      if (me < num_pes_fcst) then
!----------------------------------------------------------
! transform spectral to grid to syn
          call spect_to_grid(trie_ls,trio_ls, 
     &                       syn_gr_a_1,syn_gr_a_2,
     &                       ls_node,ls_nodes,max_ls_nodes,
     &                       lats_nodes_a,global_lats_a,lonsperlat,
     &                       epse,epso,epsedn,epsodn,
     &                       snnp1ev,snnp1od,plnev_a,plnod_a)

! ---------------------------------------------------------------------
! transform spectral deriative to dyn
        call spect_to_gridxy(trie_ls,trio_ls,
     &                       syn_gr_a_1,syn_gr_a_2,
     &                       dyn_gr_a_1,dyn_gr_a_2,
     &                       ls_node,ls_nodes,max_ls_nodes,
     &                       lats_nodes_a,global_lats_a,lonsperlat,
     &                       pddev_a,pddod_a)
! ------------------------------------------------------------------
! move zs in grid_gr to syn
        do lan=1,lats_node_a
          lat = global_lats_a(ipt_lats_node_a-1+lan)
          lons_lat = lonsperlat(lat)
          tx1      = 1.0 / coslat_a(lat)
          jlonf = (lan-1)*lonf
          do lon=1,lons_lat,ngptc
            njeff = min(ngptc,lons_lat-lon+1)
            if (height_dependent_g) then
!sk compute surface geopotential (phis) as in subroutine phi2z
              do i=lon,lon+njeff-1
                ilan=i+jlonf
                syn_gr_s_z(i,lan)=grid_gr(ilan,g_gz)
                zs=syn_gr_s_z(i,lan)
                phis(i,lan)=g0re*zs/(re+zs)
              enddo
            else !g(z)=g0
              do i=lon,lon+njeff-1
                ilan=i+jlonf
                syn_gr_s_z(i,lan)=grid_gr(ilan,g_gz)
              enddo
            endif
          enddo

          do k=1,levh
            do i=1,lons_lat
              rqg(i,lan,k) = grid_gr(i+jlonf,g_rq-1+k)
            enddo
          enddo

          do k=1,levs
            do i=1,lons_lat
              n2g(i, lan, k) = 1.0
            enddo
          enddo

! rqg containing:
! ntrac=5, nn=1, ==> q.
!          nn=2, ==> oz.
!          nn=3, ==> clw.
!          nn=4, ==> o1.
!          nn=5, ==> o2.
!------------------------
          do nn=1,ntrac
            if (cpi(nn) .ne. 0.0) then
              nnl = (nn-1)*levs
              do k=1,levs
                do i=1,lons_lat
                  n2g(i, lan, k) = n2g(i, lan, k) - rqg(i,lan,nnl+k)
                enddo
              enddo
            endif
          enddo

          if (thermodyn_id == 3) then
            do k=1,levs
              do i=1,lons_lat
                tfac(i,k) = 0.0
                sumq(i,k) = 0.0
              enddo
            enddo
            do nn=1,ntrac
              nnl = (nn-1)*levs
              if (cpi(nn) .ne. 0.0) then
                do k=1,levs
                  do i=1,lons_lat
                    sumq(i,k) = sumq(i,k) + rqg(i,lan,nnl+k)
                    tfac(i,k) = tfac(i,k) + cpi(nn)*rqg(i,lan,nnl+k)
                  enddo
                enddo
              endif
            enddo
            do k=1,levs
              do i=1,lons_lat
                tfac(i,k) = (1.0-sumq(i,k))*cpi(0) + tfac(i,k)
              enddo
            enddo
          else
            do k=1,levs
              do i=1,lons_lat
                tfac(i,k) = 1.0 + fv*max(rqg(i,lan,k),qmin)
              enddo
            enddo
          endif

          do k=1,levs
            do i=1,lons_lat
              uug(i,lan,k) = grid_gr(i+jlonf,g_uu-1+k) * tx1
              vvg(i,lan,k) = grid_gr(i+jlonf,g_vv-1+k) * tx1
              ttg(i,lan,k) = grid_gr(i+jlonf,g_tt-1+k) / tfac(i,k)
            enddo
          enddo
        enddo
!
! =============================
! -------------------------------------------------------------------
        do lan=1,lats_node_a  
!
            lat      = global_lats_a(ipt_lats_node_a-1+lan)
            lon_dim  = lon_dims_a(lan)
            lons_lat = lonsperlat(lat)

! --------------------------------------------------------------------
!$omp parallel do schedule(dynamic,1) private(lon)
!$omp+private(njeff)
            do lon=1,lons_lat,ngptcd
!!
               njeff = min(ngptcd,lons_lat-lon+1)
!
               if( gen_coord_hybrid ) then                        
                  if( thermodyn_id == 3 ) then                    
                    call gfidi_hyb_gc_h_w_z(lon_dim, njeff, lat,
     &                syn_gr_a_2(lon+(ksd   -1)*lon_dim,lan),
     &                syn_gr_a_2(lon+(kst   -1)*lon_dim,lan),
     &                syn_gr_a_2(lon+(ksu   -1)*lon_dim,lan),
     &                syn_gr_a_2(lon+(ksv   -1)*lon_dim,lan),
     &                syn_gr_a_2(lon+(ksr   -1)*lon_dim,lan),
     &                syn_gr_a_2(lon+(kspphi-1)*lon_dim,lan),
     &                syn_gr_a_2(lon+(ksplam-1)*lon_dim,lan),
     &                syn_gr_a_2(lon+(ksq   -1)*lon_dim,lan),
     &                syn_gr_a_2(lon+(kzsphi-1)*lon_dim,lan),
     &                syn_gr_a_2(lon+(kzslam-1)*lon_dim,lan),
     &                syn_gr_s_z(lon                   ,lan),
     &                rcs2_a(min(lat,latg-lat+1)),           
     &                dyn_gr_a_2(lon+(kdtphi-1)*lon_dim,lan),
     &                dyn_gr_a_2(lon+(kdtlam-1)*lon_dim,lan),
     &                dyn_gr_a_2(lon+(kdrphi-1)*lon_dim,lan),
     &                dyn_gr_a_2(lon+(kdrlam-1)*lon_dim,lan),
     &                anl_gr_a_w(lon,lan),
     &                anl_gr_a_z(lon,lan),me)

                  else
                    print *,' get_w_z error: not enthalpy '
                  endif
               else
                  print *,' get_w_z error: not gen_coord_hybrid '
               endif 
!
            enddo   !lon
! ---------------------------------------------------------------
        enddo   ! end of lan
!
! ===================================================
! move w in anl to grid_gr
        do lan=1,lats_node_a
          lat = global_lats_a(ipt_lats_node_a-1+lan)
          lon_dim = lon_dims_a(lan)
          lons_lat = lonsperlat(lat)
!$omp parallel do schedule(dynamic,1) private(lon)
!$omp+private(i,k,ilan,njeff)
          do lon=1,lons_lat,ngptc
            njeff = min(ngptc,lons_lat-lon+1)
            if (height_dependent_g) then
!sk compute zzg and grav as in subroutine phi2z, then compute wwg
              do k=1,levs
                do i=lon,lon+njeff-1
                  phi=g0*anl_gr_a_z(i+(k-1)*lon_dim,lan)
                  zzg(i,lan,k)=re*(phis(i,lan)+phi)
     &                         /(g0re-(phis(i,lan)+phi))
                  grav=g0re2/((re+zzg(i,lan,k))*(re+zzg(i,lan,k)))
                  wwg(i,lan,k)=(g0/grav)*anl_gr_a_w(i+(k-1)*lon_dim,lan)
                enddo
              enddo
            else   !g(z)=g0
              do k=1,levs
                do i=lon,lon+njeff-1
                  wwg(i,lan,k)=anl_gr_a_w(i+(k-1)*lon_dim,lan)
                  zzg(i,lan,k)=anl_gr_a_z(i+(k-1)*lon_dim,lan)
                enddo
              enddo
            endif
          enddo
        enddo

! -------------------------------------------------------------------
      endif ! only for fcst nodes

! Check outputs.
!---------------
!      print*, 'In get_w_z, uug = ', uug(5, 8, 4), uug(2, 15, 19)
!      print*, 'In get_w_z, vvg = ', vvg(5, 8, 4), vvg(2, 15, 19)
!      print*, 'In get_w_z, ttg = ', ttg(5, 8, 4), ttg(2, 15, 19)
!      print*, 'In get_w_z, wwg = ', wwg(5, 8, 4), wwg(2, 15, 19)
!      print*, 'In get_w_z, zzg = ', zzg(5, 8, 1), zzg(2, 15, 1)
!
      END subroutine get_w_z
