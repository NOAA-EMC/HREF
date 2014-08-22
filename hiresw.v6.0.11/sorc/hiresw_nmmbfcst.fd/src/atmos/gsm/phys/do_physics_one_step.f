      SUBROUTINE do_physics_one_step(deltim,kdt,PHOUR, 
!*   &                 grid_gr, sfc_fld, flx_fld,
     &                 grid_fld, sfc_fld, flx_fld, nst_fld, g3d_fld,
     &                 g2d_fld,
     &                 lats_nodes_r,global_lats_r,lonsperlar,
     &                 XLON,XLAT,COSZDG, 
     &                 HPRIME,SWH,HLW, HTRSWB,HTRLWB,      ! idea add
     &                 FLUXR,SFALB, SLAG,SDEC,CDEC,
     &                 OZPLIN,JINDX1,JINDX2, DDY,
     &                 phy_f3d,  phy_f2d, NBLCK,
     &                 ZHOUR, zhour_dfi,n3, n4, LSOUT,COLAT1,CFHOUR1,
     &                 fscav,restart_step)
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
!!
      use resol_def
      use layout1
      use vert_def
      use date_def
      use namelist_physics_def
      use mpi_def
      use ozne_def
      use gfs_physics_sfc_flx_mod
      use gfs_physics_sfc_flx_set_mod
      use gfs_physics_gridgr_mod,   ONLY: Grid_Var_Data
      use gfs_physics_nst_var_mod,  ONLY: Nst_Var_Data
      use gfs_physics_g3d_mod,      ONLY: G3D_Var_Data
      use gfs_physics_g2d_mod,      ONLY: G2D_Var_Data, g2d_zerout
!     use gfs_phy_tracer_config,    ONLY: gfs_phy_tracer_type
      use d3d_def, ONLY: d3d_zero, CLDCOV
      USE machine, ONLY: KIND_GRID, KIND_GRID, KIND_RAD,
     &                   kind_phys
c idea add by hmhj
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
!     type(gfs_phy_tracer_type) :: gfs_phy_tracer
!*    REAL(KIND=KIND_GRID)      GRID_GR(lonr*lats_node_r_max,lotgr)
      CHARACTER(16)             :: CFHOUR1
      logical                   :: restart_step
!!     
      REAL(KIND=KIND_EVOD),INTENT(IN):: deltim,PHOUR
      REAL(KIND=KIND_EVOD),INTENT(INOUT):: ZHOUR,ZHOUR_DFI
!!     
      INTEGER n3, n4
      INTEGER NBLCK
!!
      INTEGER               LATS_NODES_R(NODES)
      INTEGER              GLOBAL_LATS_R(LATR)
      INTEGER                 LONSPERLAR(LATR)
!!     
      real(kind=kind_evod) colat1, phyhour, phydt
      REAL (KIND=KIND_RAD) XLON(LONR,LATS_NODE_R)
      REAL (KIND=KIND_RAD) XLAT(LONR,LATS_NODE_R)
      REAL (KIND=KIND_RAD) COSZDG(LONR,LATS_NODE_R),
     &                     HPRIME(NMTVR,LONR,LATS_NODE_R),
     &                     FLUXR(nfxr,LONR,LATS_NODE_R),
     &                     SFALB(LONR,LATS_NODE_R),
     &                     SWH(NGPTC,LEVS,NBLCK,LATS_NODE_R),
     &                     HLW(NGPTC,LEVS,NBLCK,LATS_NODE_R),
! idea add by hmhj
     &                     HTRSWB(NGPTC,LEVS,NBDSW,NBLCK,LATS_NODE_R),
     &                     HTRLWB(NGPTC,LEVS,NBDLW,NBLCK,LATS_NODE_R)

      REAL (kind=kind_phys)
     &     phy_f3d(NGPTC,LEVS,NBLCK,lats_node_r,num_p3d),
     &     phy_f2d(lonr,lats_node_r,num_p2d),
     &     DDY(LATS_NODE_R), fscav(ntrac-ncld-1)

      real(kind=kind_evod) global_times_r(latr,nodes)

      INTEGER JINDX1(LATS_NODE_R),JINDX2(LATS_NODE_R)
!!     
      INTEGER LEV,LEVMAX
      REAL OZPLIN(LATSOZP,LEVOZP,pl_coeff,timeoz) !OZONE PL Coeff
      REAL(KIND=KIND_EVOD) SLAG,SDEC,CDEC
      INTEGER   kdt,       IERR,I,J,K,L,LOCL,N
      integer iprint
      LOGICAL LSOUT,ex_out
!
      real, PARAMETER:: RLAPSE=0.65E-2
!
      real*8 rtc, timer1, timer2
      REAL (kind=kind_phys) dt_warm
      REAL (kind=kind_phys),save :: zhour_dfin=0.
!
!      print *,' enter do_physics_one_step '
!
!     SHOUR   = SHOUR + deltim
      shour   = kdt * deltim
      fhour   = shour / 3600.
      lsfwd   = kdt == 1
!jws
      lssav=.true.
      if(ndfi>0 .and. kdt>ndfi/2 .and. kdt<=ndfi .and. ldfi ) then
        lssav = .false.
      endif
      if(.not.ldfi.and.ndfi>0.and.kdt==ndfi/2+1) then
         zhour=zhour_dfin
      endif
!jwe
      lscca   = mod(KDT ,nsswr) == 0
      lsswr   = mod(KDT ,nsswr) == 1
      lslwr   = mod(KDT ,nslwr) == 1
! test repro
      phyhour = phour + deltim/3600.
      phydt   = deltim
      if(lsfwd) phydt = 0.5*phydt

!-> Coupling insertion
!     call ATM_DBG2(kdt,phyhour,ZHOUR,SHOUR,3)
!     CALL ATM_TSTEP_INIT(kdt)
!<- Coupling insertion

!
!
!jw now all the pes are fcst pe
!jw if (.NOT.LIOPE.or.icolor.ne.2) then
          if (nscyc .gt. 0) then
            IF (mod(kdt,nscyc).eq.1) THEN
               CALL gcycle(me,LATS_NODE_R,LONSPERLAR,global_lats_r,
     &                    ipt_lats_node_r,idate,fhour,fhcyc,
     &                    XLON ,XLAT  , sfc_fld, ialb)
            ENDIF
          endif
!          print *,' num_p3d ',num_p3d
!
          if (num_p3d .eq. 3) then        ! Ferrier Microphysics initialization
            call INIT_MICRO(phydt, levs, 
     &                      NGPTC*NBLCK*lats_node_r, num_p3d,
     &                      phy_f3d(1,1,1,1,1), fhour, me)
          endif
!
!-> Coupling insertion

  ! lgetSST_cc must be defined by this moment. It used to be an argument
  ! to ATM_GETSST, accessible here via USE SURFACE_cc. Now it is defined in
  ! ATM_TSTEP_INIT called above, and the USE is removed. (Even in the earlier
  ! version lgetSST_cc did not have to be an actual argumnent, since
  ! it is in the module SURFACE_cc USEd by ATM_GETSST.)

!       call ATM_GETSST(sfc_fld%TSEA,sfc_fld%SLMSK,sfc_fld%ORO)

!<- Coupling insertion

!
         if ( nst_fcst > 1  .and. mom4ice) then ! update TSEA for OM coupling
            do j = 1, lats_node_r
              do i = 1, lonr
                if (sfc_fld%slmsk(i,j) == 0 ) then
                  dt_warm = (nst_fld%xt(i,j)+nst_fld%xt(i,j))
     &                    /  nst_fld%xz(i,j)
                  sfc_fld%TSEA(i,j) = nst_fld%tref(i,j)
     &                  + dt_warm - nst_fld%dt_cool(i,j)
     &                  - (sfc_fld%oro(i,j)-sfc_fld%oro_uf(i,j))*rlapse
                endif
              enddo
            enddo
         endif

!!

        if (lsswr .or. lslwr) then         ! Radiation Call!
c idea add by hmhj
          if(lsidea) then
            if(lsswr) then
              swh=0.
              htrswb=0.
            endif
            if(lslwr) then
              hlw=0.
              htrlwb=0.
            endif
          endif
!*        CALL GLOOPR ( grid_gr,
          CALL GLOOPR ( grid_fld, g3d_fld,
     &     LATS_NODES_R,GLOBAL_LATS_R,LONSPERLAR,phyhour,
     &     XLON,XLAT,COSZDG,flx_fld%COSZEN,
     &     sfc_fld%SLMSK,sfc_fld%SNWDPH,sfc_fld%SNCOVR,sfc_fld%SNOALB,
     &     sfc_fld%ZORL,sfc_fld%TSEA, HPRIME,SFALB,
     &     sfc_fld%ALVSF,sfc_fld%ALNSF,sfc_fld%ALVWF ,sfc_fld%ALNWF,
     &     sfc_fld%FACSF,sfc_fld%FACWF,sfc_fld%CV,sfc_fld%CVT ,
     &     sfc_fld%CVB,SWH,HLW,flx_fld%SFCNSW,flx_fld%SFCDLW,
     &     sfc_fld%FICE,sfc_fld%TISFC,flx_fld%SFCDSW,
     &     flx_fld%sfcemis,
     &     flx_fld%TSFLW,FLUXR,phy_f3d,SLAG,SDEC,CDEC,NBLCK,KDT,
     &     HTRSWB,HTRLWB)      !idea add by hmhj
!          if (iprint .eq. 1) print*,' me = fin gloopr ',me

        endif
!
!!
!*    call gloopb ( grid_gr,
      call gloopb ( grid_fld, g3d_fld,
     x     lats_nodes_r,global_lats_r,lonsperlar,
     &     phydt,phyhour,sfc_fld, flx_fld,nst_fld,SFALB,xlon,
     &     nbdsw,nbdlw,swh,hlw,HTRSWB,HTRLWB, !idea add by hmhj
     &     hprime,slag,sdec,cdec,
     &     ozplin,jindx1,jindx2,ddy,
     &     phy_f3d, phy_f2d,xlat,nblck,kdt,
     &     fscav,restart_step)
!
!!
!jw      endif !.NOT.LIOPE.or.icolor.ne.2
!--------------------------------------------
!
!      write(0,*)'in do one phys step, lsout=',lsout,'kdt=',kdt, 
!     &   'nszer=',nszer,'ldfi=',ldfi,'ndfi=',ndfi,
!     &   'fhour=',fhour,'zhour=',zhour,'zhour_dfin=',zhour_dfin,
!     &   'zhour_dfi=',zhour_dfi
      if (lsout .and. kdt .ne. 0.0 ) then
!WY bug fix.
!-----------
        IF(.NOT. ALLOCATED(SL)) ALLOCATE(SL(levs))
        IF(.NOT. ALLOCATED(SI)) ALLOCATE(SI(levs + 1))
        CALL WRTOUT_physics(phyhour,FHOUR,ZHOUR,IDATE,
     &              SL,SI,
     &              sfc_fld, flx_fld, nst_fld, g2d_fld,
     &              fluxr,
     &              lats_nodes_r,global_lats_r,lonsperlar,nblck,
     &              COLAT1,CFHOUR1,pl_coeff,'SFC.F','FLX.F','D3D.F')
      endif ! if ls_out
!
       IF (kdt>0 .and. mod(kdt,nsres).eq.0) THEN
!           write(0,*)'wrt_restart_physics,kdt=',kdt,'nsres=',nsres
           CALL wrtout_restart_physics(
     &        sfc_fld, nst_fld, fhour,idate,
     &        lats_nodes_r,global_lats_r,lonsperlar,
     &        phy_f3d, phy_f2d, ngptc, nblck, ens_nam)
       endif
!
      IF (mod(kdt,nszer).eq.0 .and. lsout.and.kdt.ne.0) THEN
        call flx_init(flx_fld,ierr)
        if(ldfi.and. kdt==ndfi/2) then
         zhour_dfi=zhour
         zhour_dfin=fhour
        endif
        zhour = fhour
        FLUXR = 0.
!
        if (ldiag3d .or. lggfs3d) then
!         if(me==0) print *, 'LU_CLDCOV: zero out d3d fields'
          call d3d_zero(ldiag3d,lggfs3d)
          if (fhour >= fhgoc3d) lggfs3d = .false.

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
         zhour=zhour_dfi
      endif
!     print *,'in phys one,kdt=',kdt,'zhour=',zhour,                   &
!     &  'zhour_dfi=',zhour_dfi,'zhour_dfin=',zhour_dfin
 
      if(ndfi>0 .and. kdt==ndfi .and. ldfi ) then
        ldfi = .false.
      endif
!
! Coupling insertion->
!     CALL ATM_SENDFLUXES(sfc_fld%SLMSK)
!<- Coupling insertion

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
