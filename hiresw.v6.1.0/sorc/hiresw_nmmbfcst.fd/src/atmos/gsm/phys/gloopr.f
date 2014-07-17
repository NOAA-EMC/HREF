       subroutine gloopr
!*   &    ( grid_gr,
     &    ( grid_fld, g3d_fld,                                          &
     &     lats_nodes_r,global_lats_r, lonsperlar, phour,               &
     &     xlon,xlat,coszdg,COSZEN,                                     &
     &     SLMSK,SNWDPH,SNCOVR,SNOALB,ZORL,TSEA,HPRIME,SFALB,           &
     &     ALVSF,ALNSF ,ALVWF ,ALNWF,FACSF ,FACWF,CV,CVT ,              &
     &     CVB  ,SWH,HLW,SFCNSW,SFCDLW,                                 &
     &     FICE ,TISFC, SFCDSW, sfcemis,                                &
     &     TSFLW,FLUXR, phy_f3d,slag,sdec,cdec,NBLCK,KDT                &
     &    )
!    &,    HTRSWB,HTRLWB)
!    &     global_times_r)

!! Code Revision:
!! Oct 11 2009       Sarah Lu, grid_gr is replaced by grid_fld
!! Oct 16 2009       Sarah Lu, grid_fld%tracers used
!! Dec 01 2009       Sarah Lu, update fcld (instant cloud cover) in addition
!!                             to cldcov (cumulative cloud cover)
!! Dec 09 2009       Sarah Lu, (1) g3d_fld added to calling argument; (2) grrad
!!                   returns instant cloud cover (cldcov_v); the accumulative 
!!                   and instant cloud cover fields are updated after grrad call
!! Dec 11 2009       Sarah Lu, ldiag3d removed from grrad calling argument
!! Jul/Aug 2009      S. Moorthi Merged with McICA version of radiation from YuTai
!! Nov 20 2012       Jun Wang  fix the vertical index from levs to levr for gt,gr..etc
!!
!!
!#include "f_hpm.h"
!
      USE MACHINE              ,     ONLY : kind_phys, kind_grid,       &
     &                                      kind_evod
      USE FUNCPHYS             ,     ONLY : fpkap
      USE PHYSCONS, fv => con_fvirt, rerth => con_rerth,
     &              rk => con_rocp

      use module_radiation_driver,   only : radinit, grrad
      use module_radiation_astronomy,only : astronomy
      USE gfs_phy_tracer_config,     only : gfs_phy_tracer

      use module_radsw_parameters,  only : topfsw_type, sfcfsw_type
      use module_radlw_parameters,  only : topflw_type, sfcflw_type
!
!! ---  for optional spectral band heating outputs
!!    use module_radsw_parameters,   only : NBDSW
!!    use module_radlw_parameters,   only : NBDLW
!
      use resol_def,            ONLY: levs, levr, latr, lonr, lotgr,    &
     &                                g_t, g_p, g_q, g_dp, g_ps,        &
     &                                ntcw, ntoz, ncld, num_p3d,        &
     &                                nmtvr, ntrac, levp1, nfxr, g_dpdt,&
     &                                lgocart
      use layout1,              ONLY: me, nodes, lats_node_r,           &
     &                                lats_node_r_max, ipt_lats_node_r
      use gg_def,               ONLY: coslat_r, sinlat_r
      use date_def,             ONLY: idate
      use namelist_physics_def, ONLY: lsswr, iaer, lslwr, sashal, ras,  &
     &                                lssav, flgmin, ldiag3d, lggfs3d,  &
     &                                iovr_lw, iovr_sw, isol, iems,     &
     &                                ialb, fhlwr, fhswr, ico2, ngptc,  &
     &                                crick_proof, norad_precip, ccnorm,&
     &                                ictm, isubc_sw, isubc_lw, fdaer
      use d3d_def ,             ONLY: cldcov
      use gfs_physics_gridgr_mod, ONLY: Grid_Var_Data
      use gfs_physics_g3d_mod,    ONLY: G3D_Var_Data
      use mersenne_twister, only : random_setseed, random_index,        &
     &                             random_stat
!
      implicit none
!
      real (kind=kind_phys), parameter :: QMIN =1.0e-10                 &
     &,                                   Typical_pgr = 95.0            &
     &,                                   cons0 = 0.0,  cons2 = 2.0     &
     &,                                   pt00001=1.0e-5
!    &,                                   pt01=0.01
!
!  --- ...  inputs:
      integer, intent(in) :: lats_nodes_r(nodes)
      integer, intent(in) :: global_lats_r(latr), lonsperlar(latr)

!*    real(kind=kind_grid) grid_gr(lonr*lats_node_r_max,lotgr)

      TYPE(Grid_Var_Data) :: grid_fld 
      TYPE(G3D_Var_Data)  :: g3d_fld 

      integer, intent(in) :: NBLCK


      real (kind=kind_phys), dimension(LONR,LATS_NODE_R), intent(in) :: &
     &                       xlon, xlat, slmsk, snwdph, zorl, tsea,     &
     &                       alvsf, alnsf, alvwf, alnwf, facsf, facwf,  &
     &                       cv, cvt, cvb, FICE, tisfc, sncovr, snoalb

      real (kind=kind_phys), intent(in) ::                              &
     &                    hprime(NMTVR,LONR,LATS_NODE_R), phour,        &
     &                    phy_f3d(NGPTC,LEVS,NBLCK,LATS_NODE_R,NUM_P3D)
!

      real (kind=kind_phys), intent(inout) ::                           &
     &                    fluxr (NFXR,LONR,LATS_NODE_R)

      integer, intent(in) :: KDT
!  --- ...  outputs:
!     real(kind=kind_evod), intent(out) ::                              &
!    &                    global_times_r(latr,NODES)

      real (kind=kind_phys), intent(out) ::                             &
     &                    swh(NGPTC,LEVS,NBLCK,LATS_NODE_R),            &
     &                    hlw(NGPTC,LEVS,NBLCK,LATS_NODE_R)

      real (kind=kind_phys),dimension(LONR,LATS_NODE_R), intent(out) :: &
     &                    coszdg, coszen, sfcnsw, sfcdlw, tsflw,        &
     &                    sfcdsw, SFALB, sfcemis 

      real (kind=kind_phys), intent(out) :: slag, sdec, cdec

!! --- ...  optional spectral band heating rates
!!    real (kind=kind_phys), optional, intent(out) ::                   &
!!   &                 htrswb(NGPTC,LEVS,NBDSW,NBLCK,LATS_NODE_R),      &
!!   &                 htrlwb(NGPTC,LEVS,NBDLW,NBLCK,LATS_NODE_R)

!  --- ...  locals:
      real(kind=kind_phys) :: prsl(NGPTC,LEVS),  prslk(NGPTC,LEVS),     &
     &                        prsi(NGPTC,LEVP1)
!    &                        prsi(NGPTC,LEVP1), prsik(NGPTC,LEVP1)

      real (kind=kind_phys) :: si_loc(LEVR+1)

      real (kind=kind_phys) ::                                          &
     &                        gt(NGPTC,LEVR),                           &
     &                        gr(NGPTC,LEVR), gr1(NGPTC,LEVR,NTRAC-1)

      real (kind=kind_phys) :: f_ice(NGPTC,LEVS), f_rain(NGPTC,LEVS),   &
     &                         r_rime(NGPTC,LEVS)

      real (kind=kind_phys) :: cldcov_v(NGPTC,LEVS), hprime_v(NGPTC),   &
     &                         fluxr_v(NGPTC,NFXR), vvel(NGPTC,LEVS)
      real (kind=kind_phys) :: flgmin_v(ngptc), work1, work2
      real (kind=kind_phys), dimension(LONR,LATS_NODE_R) ::             &
     &                         sinlat_v, coslat_v

      real (kind=kind_phys) :: rinc(5), dtsw, dtlw, solcon, raddt
      real (kind=4) :: rinc4(5)
      integer w3kindreal,w3kindint

      real (kind=kind_phys), save :: facoz

      integer :: njeff, lon, lan, lat, iblk, lons_lat, kk
      integer :: idat(8), jdat(8), DAYS(13), iday, imon, midmon, id
      integer :: nlnsp(LATS_NODE_R)
!     integer :: jlonr, ilan

!  ---  variables of instantaneous calculated toa/sfc radiation fluxes
      type (topfsw_type), dimension(NGPTC) :: topfsw
      type (sfcfsw_type), dimension(NGPTC) :: sfcfsw

      type (topflw_type), dimension(NGPTC) :: topflw
      type (sfcflw_type), dimension(NGPTC) :: sfcflw

!  ---  variables used for random number generator (thread safe mode)
      type (random_stat) :: stat
      integer :: numrdm(LONR*LATR*2), ixseed(LONR,LATS_NODE_R,2)
      integer :: ipseed, icsdlw(NGPTC), icsdsw(NGPTC)
      integer, parameter :: ipsdlim = 1.0e8      ! upper limit for random seeds


      integer, save :: icwp, k1oz, k2oz, midm, midp, ipsd0, iaerflg

!  ---  number of days in a month
      data DAYS / 31,28,31,30,31,30,31,31,30,31,30,31,30 /

!  --- ...  control parameters: 
!           (some of the them may be moved into model namelist)

!  ---  ICTM=yyyy#, controls time sensitive external data (e.g. CO2, solcon, aerosols, etc)
!     integer, parameter :: ICTM =   -2 ! same as ICTM=0, but add seasonal cycle
!                                       ! from climatology. no extrapolation.
!     integer, parameter :: ICTM =   -1 ! use user provided external data set for the
!                                       ! forecast time. no extrapolation.
!     integer, parameter :: ICTM =    0 ! use data at initial cond time, if not
!                                       ! available, use latest, no extrapolatio n.
!!    integer, parameter :: ICTM =    1 ! use data at the forecast time, if not
!                                       ! available, use latest and extrapolation.
!     integer, parameter :: ICTM =yyyy0 ! use yyyy data for the forecast time,
!                                       ! no further data extrapolation.
!     integer, parameter :: ICTM =yyyy1 ! use yyyy data for the fcst. if needed, do
!                                       ! extrapolation to match the fcst time.

!  ---  ISOL controls solar constant data source
!!    integer, parameter :: ISOL  = 0  ! use prescribed solar constant
!     integer, parameter :: ISOL  = 1  ! use varying solar const with 11-yr cycle

!  ---  ICO2 controls co2 data source for radiation
!     integer, parameter :: ICO2 = 0   ! prescribed global mean value (old opernl)
!!    integer, parameter :: ICO2 = 1   ! use obs co2 annual mean value only
!     integer, parameter :: ICO2 = 2   ! use obs co2 monthly data with 2-d variation

!  ---  IALB controls surface albedo for sw radiation
!!    integer, parameter :: IALB = 0   ! use climatology alb, based on sfc type
!     integer, parameter :: IALB = 1   ! use modis derived alb (to be developed)

!  ---  IEMS controls surface emissivity and sfc air/ground temp for lw radiation
!        ab: 2-digit control flags. a-for sfc temperature;  b-for emissivity
!!    integer, parameter :: IEMS = 00  ! same air/ground temp; fixed emis = 1.0
!!    integer, parameter :: IEMS = 01  ! same air/ground temp; varying veg typ based emis
!!    integer, parameter :: IEMS = 10  ! diff air/ground temp; fixed emis = 1.0
!!    integer, parameter :: IEMS = 11  ! diff air/ground temp; varying veg typ based emis
!  ---  IAER  controls aerosols scheme selections
!     Old definition
!     integer, parameter :: IAER  = 1  ! opac climatology, without volc forcing
!     integer, parameter :: IAER  =11  ! opac climatology, with volcanic forcing
!     integer, parameter :: IAER  = 2  ! gocart prognostic, without volc forcing
!     integer, parameter :: IAER  =12  ! gocart prognostic, with volcanic forcing
!     New definition in this code IAER = abc (a:volcanic; b:lw; c:sw)
!                             b, c values: (0:none; 1:opac; 2:gocart)
!  IAER =   0 --> no aerosol effect at all (volc, sw, lw)
!       =   1 --> only tropospheric sw aerosols, no trop-lw and volc
!       =  10 --> only tropospheric lw aerosols, no trop-sw and volc
!       =  11 --> both trop-sw and trop-lw aerosols, no volc
!       = 100 --> only strato-volc aeros, no trop-sw and trop-lw
!       = 101 --> only sw aeros (trop + volc), no lw aeros
!       = 110 --> only lw aeros (trop + volc), no sw aeros
!       = 111 --> both sw and lw aeros (trop + volc)
!

!  ---  IOVR controls cloud overlapping method in radiation:
!     integer, parameter :: IOVR_SW = 0  ! sw: random overlap clouds
!!    integer, parameter :: IOVR_SW = 1  ! sw: max-random overlap clouds

!     integer, parameter :: IOVR_LW = 0  ! lw: random overlap clouds
!!    integer, parameter :: IOVR_LW = 1  ! lw: max-random overlap clouds

!  ---  ISUBC controls sub-column cloud approximation in radiation:
!     integer, parameter :: ISUBC_SW = 0 ! sw: without sub-col clds approx
!     integer, parameter :: ISUBC_SW = 1 ! sw: sub-col clds with prescribed seeds
!     integer, parameter :: ISUBC_SW = 2 ! sw: sub-col clds with random seeds

!     integer, parameter :: ISUBC_LW = 0 ! lw: without sub-col clds approx
!     integer, parameter :: ISUBC_LW = 1 ! lw: sub-col clds with prescribed seeds
!     integer, parameter :: ISUBC_LW = 2 ! lw: sub-col clds with random seeds

!  ---  iflip indicates model vertical index direction:
!     integer, parameter :: IFLIP = 0    ! virtical profile index from top to bottom
      integer, parameter :: IFLIP = 1    ! virtical profile index from bottom to top
!
!    The following parameters are from gbphys
!
      real (kind=kind_phys), parameter :: dxmax=-16.118095651,          &
     &                                    dxmin=-9.800790154,           &
     &                                    dxinv=1.0/(dxmax-dxmin)

      integer :: ierr, dimg
      integer :: i, j, k, n, item, dbgu

      logical :: change
      logical, save :: first, sas_shal
      data  first / .true. /
!
!  ---  for debug test use
      real (kind=kind_phys) :: temlon, temlat, alon, alat
      integer :: ipt
      logical :: lprnt

!  ---  timers:
      real*8 :: rtc, timer1, timer2
!
!===> *** ...  begin here
!
!!
      integer              kap,kar,kat,kau,kav,kdrlam
      integer              ksd,ksplam,kspphi,ksq,ksr,kst
      integer              ksu,ksv,ksz,node
!!
!     print *,' enter gloopr '
!
      idat = 0
      idat(1) = idate(4)
      idat(2) = idate(2)
      idat(3) = idate(3)
      idat(5) = idate(1)
      rinc = 0.
! test repro
!     rinc(2) = fhour
      rinc(2) = phour
!     print *,' idate ',idate
!     print *,' rinc ',rinc
      call w3kind(w3kindreal,w3kindint)
      if(w3kindreal==4) then
        rinc4=rinc
        call w3movdat(rinc4, idat, jdat)
      else
        call w3movdat(rinc, idat, jdat)
      endif
!     print *,' jdat ',jdat
!
      if (ntoz .le. 0) then                ! Climatological Ozone!
!
      if(me .eq. 0) WRITE (6,989) jdat(1),jdat(2),jdat(3),jdat(5)
  989 FORMAT(' UPDATING OZONE FOR ', I4,I3,I3,I3)
!
        IDAY   = jdat(3)
        IMON   = jdat(2)
        MIDMON = DAYS(IMON)/2 + 1
        CHANGE = FIRST .OR.
     &          ( (IDAY .EQ. MIDMON) .AND. (jdat(5).EQ.0) )
!
        IF (CHANGE) THEN
            IF (IDAY .LT. MIDMON) THEN
               K1OZ = MOD(IMON+10,12) + 1
               MIDM = DAYS(K1OZ)/2 + 1
               K2OZ = IMON
               MIDP = DAYS(K1OZ) + MIDMON
            ELSE
               K1OZ = IMON
               MIDM = MIDMON
               K2OZ = MOD(IMON,12) + 1
               MIDP = DAYS(K2OZ)/2 + 1 + DAYS(K1OZ)
            ENDIF
        ENDIF
!
        IF (IDAY .LT. MIDMON) THEN
           ID = IDAY + DAYS(K1OZ)
        ELSE
           ID = IDAY
        ENDIF
        FACOZ = real (ID-MIDM) / real (MIDP-MIDM)
      endif
!
      if (first) then
        sas_shal = sashal .and. (.not. ras)
!
        si_loc(1)=1.0
        do k=1,levr-1
!*        si_loc(k+1)=si_loc(k)-grid_gr(1,g_dp+k-1)/grid_gr(1,g_ps)
          si_loc(k+1)=si_loc(k)-grid_fld%dp(1,1,k)/grid_fld%ps(1,1) 
        enddo
        si_loc(levr+1)=0.0

!  --- determin prognostic/diagnostic cloud scheme

        icwp   = 0
        if (NTCW > 0) icwp = 1

!  ---  generate initial permutation seed for random number generator

        if ( ISUBC_LW == 2 .or. ISUBC_SW == 2 ) then
          ipsd0 = 17*idate(1) + 43*idate(2) + 37*idate(3) + 23*idate(4)
          if ( me == 0 ) then
            print *,'  Radiation sub-cloud initial seed =',ipsd0,       &
     &              ' idate =',idate
          endif
        endif

        iaerflg = max( mod(IAER,10), mod(IAER/10,10) ) ! flag for trop-aer scheme selection

        first = .false.
           
      endif         ! end_if_first
!
!===> *** ...  radiation initialization
!
      dtsw  = 3600.0 * fhswr
      dtlw  = 3600.0 * fhlwr

      raddt = min(dtsw, dtlw)

      call radinit                                                      &
!  ---  input:
     &     ( si_loc, LEVR, IFLIP, idat, jdat, ICTM, ISOL, ICO2,         &
     &       IAER, IALB, IEMS, ICWP, NUM_P3D, ISUBC_SW, ISUBC_LW,       &
     &       IOVR_SW, IOVR_LW, me, raddt, fdaer )
!  ---  output: ( none )

      do j = 1, LATS_NODE_R
        k   = global_lats_r(IPT_LATS_NODE_R-1+j)
        nlnsp(j) = lonsperlar(k)

        do i = 1, nlnsp(j)
          sinlat_v(i,j) = sinlat_r(k)
          coslat_v(i,j) = coslat_r(k)
        enddo

!  ---  padding spaces left
        if (nlnsp(j) < lonr) then
          do i=nlnsp(j)+1,lonr
            sinlat_v(i,j) = 0.0
            coslat_v(i,j) = 0.0
          enddo
        endif
!       n = nlnsp(j)
!       do while (n < LONR)
!         n = n + 1
!         sinlat_v(n,j) = 0.0
!         coslat_v(n,j) = 0.0
!       enddo
      enddo
!
!===> *** ...  astronomy for sw radiation calculation.
!
!     print *,' calling astronomy'
      call astronomy                                                    &
!  ---  inputs:
     &     ( sinlat_v, coslat_v, xlon, fhswr, jdat,                     &
     &       LONR, LATS_NODE_R, nlnsp, lsswr, me,                       &
!  ---  outputs:
     &       solcon, slag, sdec, cdec, coszen, coszdg                   &
     &      )
!     print *,' returned from astro'

!
!===> *** ...  generate 2-d random seeds array for sub-grid cloud-radiation
!
      if ( ISUBC_LW==2 .or. ISUBC_SW==2 ) then
        ipseed = mod(nint(100.0*sqrt(phour*3600)), ipsdlim) + 1 + ipsd0

        call random_setseed                                             &
!  ---  inputs:
     &     ( ipseed,                                                    &
!  ---  outputs:
     &       stat                                                       &
     &      )
        call random_index                                               &
!  ---  inputs:
     &     ( ipsdlim,                                                   &
!  ---  outputs:
     &       numrdm, stat                                               &
     &     )

        do k = 1, 2
          do j = 1, lats_node_r
            lat = global_lats_r(ipt_lats_node_r-1+j)
            do i = 1, LONR
              ixseed(i,j,k) = numrdm(i+(lat-1)*LONR+(k-1)*LATR)
            enddo
          enddo
        enddo
      endif


!
!===> *** ...  spectrum to grid transformation for radiation calculation.
!     -----------------------------------
!!
!     call f_hpmstart(61,"gr delnpe")
!     call f_hpmstop(61)
!     call f_hpmstart(62,"gr delnpo")
!     call f_hpmstop(62)
!!
!     call f_hpmstart(63,"gr dezouv dozeuv")
!
!     call f_hpmstop(63)
!!
!     CALL countperf(0,5,0.)
!     CALL synctime()
!     CALL countperf(1,5,0.)
!!
!     CALL countperf(0,1,0.)
!!
!     call f_hpmstart(67,"gr sumfln")
!     call f_hpmstop(67)
!!
!     CALL countperf(1,1,0.)
!     CALL countperf(0,1,0.)                                            ! hmhj
!
!     call f_hpmstart(68,"gr sumder2")                                  ! hmhj
!     call f_hpmstop(68)                                                ! hmhj
!
      CALL countperf(1,1,0.)                                            ! hmhj
!
!
!===> *** ...  starting latitude loop
!
      do lan=1,lats_node_r
!!
         lat = global_lats_r(ipt_lats_node_r-1+lan)
!!
         lons_lat = lonsperlar(lat)

!        jlonr = (lan-1) * lonr

!     write(0,*)' in gloopr lan=',lan,' lons_lat=',lons_lat,' lat=',lat
!     write (0,*)' grid_fldps=',grid_fld%ps(1:lons_lat:ngptc,lan)
!!
!$omp parallel do schedule(dynamic,1) private(lon,j,k,item,njeff,iblk,n)
!$omp+private(vvel,gt,gr,gr1,work1,work2,flgmin_v)
!$omp+private(cldcov_v,hprime_v,fluxr_v,f_ice,f_rain,r_rime)
!$omp+private(prslk,prsl,prsi,topfsw,sfcfsw,topflw,sfcflw)
!!$omp+private(prslk,prsl,prsik,prsi,topfsw,sfcfsw,topflw,sfcflw)
!$omp+private(icsdsw,icsdlw)
!!$omp+private(lprnt,ipt,dbgu)
!$omp+private(temlon,temlat,lprnt,ipt)

!!!$omp+private(temlon,temlat,lprnt,ipt)

        DO lon=1,lons_lat,NGPTC
!!
          NJEFF = MIN(NGPTC,lons_lat-lon+1)
          iblk  = (lon-1)/ngptc + 1
!
!        write(0,*)' GLOOPR : LON=',lon,' lons_lat=',lons_lat,
!    &' njeff=',njeff,' iblk=',iblk
!        dbgu = 300 + lon
!        print *,' ngptc=',ngptc,' dbgu=',dbgu
!        write(dbgu,*)' dbgu=',dbgu,' lon=',lon,' lan=',lan
!        ipt = njeff

!
!  --- ...  for debug test
!         alon = 236.25
!         alat = 56.189
!         alon = 22.5
!         alat = -12.381
!         ipt = 0
!         do i = 1, njeff
!           item = lon + i - 1
!           temlon = xlon(item,lan) * 57.29578
!           if (temlon < 0.0) temlon = temlon + 360.0
!           temlat = xlat(item,lan) * 57.29578
!           lprnt = abs(temlon-alon) < 0.5 .and. abs(temlat-alat) < 0.5
!    &          .and. kdt > 0
!           if ( lprnt ) then
!             ipt = i
!             print *,' ipt=',ipt,' lon=',lon,' lan=',lan
!             exit
!           endif
!         enddo

          lprnt = .false.
          ipt   = 1
!!
!

          do i = 1, njeff
!           ilan = jlonr + lon+i-1
!           prsi(i,1) = grid_gr(ilan,g_ps)
            prsi(i,1) = grid_fld%ps(lon+i-1,lan)
          enddo
          do k = 1, LEVR
            do i = 1, njeff
!             ilan = jlonr + lon+i-1
!             gt(i,k)    = grid_gr(ilan,g_t+k-1)
!             gr(i,k)    = max(qmin,grid_gr(ilan,g_q+k-1))
!             prsl(i,k)  = grid_gr(ilan,g_p+k-1)
!             vvel(i,k)  = grid_gr(ilan,g_dpdt+k-1)
!             prsi(i,k+1)= prsi(i,k)-                                      &
!     &                    grid_gr(ilan,g_dp+k-1)
!*            gr(i,k)    = max(qmin,grid_fld%q(lon+i-1,lan,k))                 

              item      = lon+i-1
              gt(i,k)   = grid_fld%t(item,lan,k)                           
!     write(0,*)' gt=',gt(i,k),' grid_fld%t=',grid_fld%t(item,lan,k),
!    &' i=',i,' k=',k,' item=',item,' lan=',lan
              gr(i,k)   = max(qmin,grid_fld%tracers(1)%flds(item,lan,k))                 
              prsl(i,k)   = grid_fld%p(item,lan,k)                         
              vvel(i,k)   = grid_fld%dpdt(item,lan,k)                        
              prsi(i,k+1) = prsi(i,k) - grid_fld%dp(item,lan,k)          
            enddo
          enddo
          do i = 1, njeff
            prsi (i,levs+1) = 0.0
!           prsik(i,levs+1) = 0.0
          enddo
!         do k = 1, levs
!           do i = 1, njeff
!             prslk(i,k) = (prsl(i,k)*pt00001)**rk
!             prsik(i,k) = (prsi(i,k)*pt00001)**rk
!           enddo
!         enddo
!       write (0,*)' prslk=',prslk(1,1:6),' lan=',lan
!
!       Remaining tracers
!
         do n = 1, NTRAC-1
!           kk = g_q + n*levs
            do k = 1, LEVR
              do i = 1, njeff
!               ilan = jlonr + lon+i-1
!               gr1(i,k,n) = grid_gr(ilan,kk+k-1)
!*              gr1(i,k,ntoz-1) = grid_fld%oz(lon+i-1,lan,k)  
!*              gr1(i,k,ntcw-1) = grid_fld%cld(lon+i-1,lan,k) 
                gr1(i,k,n) = grid_fld%tracers(n+1)%flds(lon+i-1,lan,k)
              enddo
            enddo
          enddo

!!
!.....
          if (levr < levs) then
            do i=1,njeff
              prsi(i,levr+1)  = prsi(i,levp1)
              prsl(i,levr)    = (prsi(i,levp1)+prsi(i,levr)) * 0.5
!             prsik(i,levr+1) = prslk(i,levp1)
!             prslk(i,levr)   = fpkap(prsl(i,levr))
!             prslk(i,levr)   = fpkap(prsl(i,levr)*1000.0)
            enddo
          endif
!
          if (ntoz <= 0 .or. iaerflg == 2) then
            do k = 1, levs
              do i = 1, njeff
                prslk(i,k) = (prsl(i,k)*pt00001)**rk
              enddo
            enddo
          endif
!
          do i=1,njeff
            hprime_v(i) = hprime(1,lon+i-1,lan)
          enddo
!
          do k=1,nfxr
            do i=1,njeff
              fluxr_v(i,k) = fluxr(k,lon+i-1,lan)
            enddo
          enddo
          if (NUM_P3D == 3) then
            do k = 1, LEVR
              do i = 1, njeff
                f_ice (i,k) = phy_f3d(i,k,iblk,lan,1)
                f_rain(i,k) = phy_f3d(i,k,iblk,lan,2)
                r_rime(i,k) = phy_f3d(i,k,iblk,lan,3)
              enddo
            enddo

            work1 = (log(coslat_r(lat)/(lons_lat*latr)) - dxmin) * dxinv
            work1 = max(0.0, min(1.0,work1))
            work2 = flgmin(1)*work1 + flgmin(2)*(1.0-work1)
            do i=1,njeff
              flgmin_v(i) = work2
            enddo
          else
            do i=1,njeff
              flgmin_v(i) = 0.0
            enddo
          endif
 
!  *** ...  calling radiation driver
 
!
!     lprnt = me .eq. 0 .and. kdt .ge. 120
!     lprnt = me .eq. 0 .and. lan == 13
!     lprnt = me .eq. 0
!     ipt = min(91,njeff)
!     ipt = 1
!     if (lprnt) then
!     if (kdt .gt. 85) then
!     write(0,*)' calling grrad for me=',me,' lan=',lan,' lat=',lat
!    &,' num_p3d=',num_p3d,' snoalb=',snoalb(lon,lan),' lon=',lon
!    &,' tsea=',tsea(lon,lan),' sncovr=',sncovr(lon,lan),
!    &' snwdph=',snwdph(lon,lan),' ipt=',ipt,' njeff=',njeff
!     write(0,*) ' prsi in gloopr=',prsi(ipt,1:5)
!     write(0,*) ' gt in gloopr=',gt(ipt,1:5)
!     write(0,*) ' gr in gloopr=',gr(ipt,1:5)
!     endif
!


          call grrad
!  ---  inputs:
     &     ( prsi,prsl,prslk,gt,gr,gr1,vvel,slmsk(lon,lan),             &
     &       xlon(lon,lan),xlat(lon,lan),tsea(lon,lan),                 &
     &       snwdph(lon,lan),sncovr(lon,lan),snoalb(lon,lan),           &
     &       zorl(lon,lan),hprime_v,                                    &
     &       alvsf(lon,lan),alnsf(lon,lan),alvwf(lon,lan),              &
     &       alnwf(lon,lan),facsf(lon,lan),facwf(lon,lan),              &
     &       fice(lon,lan),tisfc(lon,lan),                              &
     &       solcon,coszen(lon,lan),coszdg(lon,lan),k1oz,k2oz,facoz,    &
     &       cv(lon,lan),cvt(lon,lan),cvb(lon,lan),                     &
     &       IOVR_SW,IOVR_LW,f_ice,f_rain,r_rime,flgmin_v,              &
     &       icsdsw,icsdlw,NUM_P3D,NTCW-1,NCLD,NTOZ-1,NTRAC-1,NFXR,     &
     &       dtlw,dtsw, lsswr,lslwr,lssav,sashal,norad_precip,          &
     &       crick_proof, ccnorm,                                       &
     &       NGPTC,njeff,LEVR,IFLIP, me, lprnt, ipt, kdt,               &
!  ---  outputs:
     &       swh(1:ngptc,1:levr,iblk,lan),topfsw,sfcfsw,sfalb(lon,lan), &
     &       hlw(1:ngptc,1:levr,iblk,lan),topflw,sfcflw,tsflw(lon,lan), &
     &       sfcemis(lon,lan),cldcov_v,                                 &
!  ---  input/output:
     &       fluxr_v                                                    & 
!    &       fluxr_v, dbgu                                              & 
!! ---  optional outputs:
!!   &,      HTRSWB=htrswb(1,1,1,iblk,lan),                             &
!!   &,      HTRLWB=htrlwb(1,1,1,iblk,lan)                              &
     &     )

!  --- ...  radiation fluxes for other physics process or diagnostics

          if (lsswr) then
            do i = 1, njeff
              j = lon + i - 1
              sfcdsw(j,lan) = sfcfsw(i)%dnfxc
              sfcnsw(j,lan) = sfcfsw(i)%dnfxc - sfcfsw(i)%upfxc
            enddo
          endif
          if (lslwr) then
            do i = 1, njeff
              j = lon + i - 1
              sfcdlw(j,lan) = sfcflw(i)%dnfxc
            enddo
          endif

!     if (lprnt) then
!     write(0,*)' hlw=',hlw(ipt,1:10,iblk,lan),' lan=',lan,' ipt=',ipt,
!    &' njeff=',njeff
!     write(0,*)' swh=',swh(ipt,1:10,iblk,lan),' lan=',lan
!     endif
!
! grrad routine computes cldcov_v (instant 3D cloud cover)    -- Sarah Lu
! if ldiag3d is T, update cldcov (accumulative 3D cloud cover)
! if lgocart is T, update fcld (instant 3D cloud cover)

          if (ldiag3d .or. lggfs3d) then
            do k=1,levr
              do i=1,njeff
                item = lon+i-1
                cldcov(k,item,lan) = cldcov(k,item,lan)                 &
     &                                + cldcov_v(i,k) * raddt
              enddo
            enddo
          endif
          if (lgocart) then
            do k=1,levr
              do i=1,njeff
                g3d_fld%fcld(lon+i-1,lan,k) = cldcov_v(i,k) 
              enddo
            enddo
          endif

          do k=1,nfxr
            do i=1,njeff
              fluxr(k,lon+i-1,lan) = fluxr_v(i,k)
            enddo
          enddo
          if (levr < levs) then
            do k=levr+1,levs
              do i=1,njeff
                hlw(i,k,iblk,lan) = hlw(i,levr,iblk,lan)
                swh(i,k,iblk,lan) = swh(i,levr,iblk,lan)
              enddo
            enddo
          endif
 
!$$$          write(2900+lat,*) ' ilon = ',lon
!$$$          write(2900+lat,'("swh",T16,"hlw")')
!$$$      do k=1,levs
!$$$         write(2900+lat,
!$$$     .         '(e10.3,T16,e10.3,T31,e10.3)')
!$$$     . swh(1,k,iblk,lan),hlw(1,k,iblk,lan)
!$$$       enddo
 
!!
          CALL countperf(1,12,0.)
          ENDDO
!
      enddo
!!
      call f_hpmstop(69)
!!
      CALL countperf(0,5,0.)
      CALL synctime()
      CALL countperf(1,5,0.)
!     write(0,*)'completed gloopr_v kdt=',kdt
!sela print*,'completed gloopr_v kdt=',kdt
!     print *,' end of gloopr '
!!
      return
      end subroutine gloopr
