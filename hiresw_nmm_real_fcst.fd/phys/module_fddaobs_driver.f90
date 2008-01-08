!WRF:MODEL_LAYER:PHYSICS
MODULE module_fddaobs_driver

! This obs-nudging FDDA module (RTFDDA) is developed by the 
! NCAR/RAL/NSAP (National Security Application Programs), under the 
! sponsorship of ATEC (Army Test and Evaluation Commands). ATEC is 
! acknowledged for releasing this capability for WRF community 
! research applications.
!
! The NCAR/RAL RTFDDA module was adapted, and significantly modified 
! from the obs-nudging module in the standard MM5V3.1 which was originally 
! developed by PSU (Stauffer and Seaman, 1994). 
! 
! Yubao Liu (NCAR/RAL): lead developer of the RTFDDA module 
! Al Bourgeois (NCAR/RAL): lead engineer implementing RTFDDA into WRF-ARW
! Nov. 2006
! 
! References:
!   
!   Liu, Y., A. Bourgeois, T. Warner, S. Swerdlin and J. Hacker, 2005: An
!     implementation of obs-nudging-based FDDA into WRF for supporting 
!     ATEC test operations. 2005 WRF user workshop. Paper 10.7.
!
!   Liu, Y., A. Bourgeois, T. Warner, S. Swerdlin and W. Yu, 2006: An update 
!     on "obs-nudging"-based FDDA for WRF-ARW: Verification using OSSE 
!     and performance of real-time forecasts. 2006 WRF user workshop. Paper 4.7. 

!   
!   Stauffer, D.R., and N.L. Seaman, 1994: Multi-scale four-dimensional data 
!     assimilation. J. Appl. Meteor., 33, 416-434.
!
!   http://www.rap.ucar.edu/projects/armyrange/references.html
!
 
CONTAINS

!-----------------------------------------------------------------------
SUBROUTINE fddaobs_driver( inest, domid, parid, restart,         &
               nudge_opt, iprt_errob, iprt_nudob,                &
               fdasta, fdaend,                                   &
               nudge_wind, nudge_temp, nudge_mois,               &
               nudge_pstr,                                       &
               coef_wind, coef_temp, coef_mois,                  &
               coef_pstr, rinxy, rinsig, twindo,                 &
               npfi, ionf, idynin, dtramp,                       &
               xlatc_cg, xlonc_cg, true_lat1, true_lat2,         &
               map_proj, i_parent_start, j_parent_start,         &
               parent_grid_ratio, maxdom, itimestep,             &
               dt, gmt, julday,                                  &
               max_obs, nobs_ndg_vars,    &
               nobs_err_flds, nstat, varobs, errf, dx,           &
               KPBL, HT, mut, muu, muv,                          &
               msft, msfu, msfv, p_phy, t_tendf, t0,             &
               ub, vb, tb, qvb, pbase, ptop, pp,                 &
               uratx, vratx, tratx, ru_tendf, rv_tendf,          &
               moist_tend, savwt,                                &
               ids,ide, jds,jde, kds,kde,                        & ! domain dims
               ims,ime, jms,jme, kms,kme,                        & ! memory dims
               its,ite, jts,jte, kts,kte                         ) ! tile   dims

!-----------------------------------------------------------------------
  USE module_domain
  USE module_bc
  USE module_model_constants, ONLY : rovg, rcp
  USE module_fddaobs_rtfdda

! This driver calls subroutines for fdda obs-nudging and
! returns computed tendencies

!
!-----------------------------------------------------------------------
  IMPLICIT NONE
!-----------------------------------------------------------------------
! taken from MM5 code - 03 Feb 2004.
!-----------------------------------------------------------------------

!=======================================================================
! Definitions
!-----------
!-- KPBL          vertical layer index for PBL top
!-- HT            terrain height (m)
!-- p_phy         pressure (Pa)
!-- t_tendf       temperature tendency

  INTEGER, intent(in)  :: ids,ide, jds,jde, kds,kde  ! domain dims.
  INTEGER, intent(in)  :: ims,ime, jms,jme, kms,kme  ! memory dims.
  INTEGER, intent(in)  :: its,ite, jts,jte, kts,kte  ! tile   dims.

  INTEGER, intent(in)  :: inest
  INTEGER, intent(in)  :: maxdom
  INTEGER, intent(in)  :: domid(maxdom)           ! Domain IDs
  INTEGER, intent(in)  :: parid(maxdom)           ! Parent domain IDs
  LOGICAL, intent(in)  :: restart
  INTEGER, intent(in)  :: itimestep
  INTEGER, intent(in)  :: nudge_opt
  LOGICAL, intent(in)  :: iprt_errob 
  LOGICAL, intent(in)  :: iprt_nudob 
  REAL, intent(in)     :: fdasta
  REAL, intent(in)     :: fdaend
  INTEGER, intent(in)  :: nudge_wind
  INTEGER, intent(in)  :: nudge_temp
  INTEGER, intent(in)  :: nudge_mois
  INTEGER, intent(in)  :: nudge_pstr
  REAL, intent(in) :: coef_wind
  REAL, intent(in) :: coef_temp
  REAL, intent(in) :: coef_mois
  REAL, intent(in) :: coef_pstr
  REAL, intent(inout)  :: rinxy
  REAL, intent(inout)  :: rinsig
  REAL, intent(inout)  :: twindo
  INTEGER, intent(in) :: npfi
  INTEGER, intent(in) :: ionf
  INTEGER, intent(in) :: idynin
  REAL, intent(inout) :: dtramp
  REAL, intent(in) :: xlatc_cg         ! center latitude  of coarse grid
  REAL, intent(in) :: xlonc_cg         ! center longitude of coarse grid
  REAL, intent(in) :: true_lat1
  REAL, intent(in) :: true_lat2
  INTEGER, intent(in) :: map_proj
  INTEGER, intent(in) :: i_parent_start(maxdom)
  INTEGER, intent(in) :: j_parent_start(maxdom)
  INTEGER, intent(in) :: parent_grid_ratio
  REAL, intent(in)     :: dt
  REAL, intent(in)     :: gmt
  INTEGER, intent(in)  :: julday
  INTEGER, intent(in)  :: max_obs         ! max number of observations
  INTEGER, intent(in)  :: nobs_ndg_vars
  INTEGER, intent(in)  :: nobs_err_flds
  INTEGER, intent(in)  :: nstat
  REAL, intent(inout)  :: varobs(nobs_ndg_vars, max_obs)
  REAL, intent(inout)  :: errf(nobs_err_flds, max_obs)
  REAL, intent(in)     :: dx           ! this-domain grid cell-size (m)
  INTEGER, INTENT(IN) :: kpbl( ims:ime, jms:jme )
  REAL, INTENT(IN) :: ht( ims:ime, jms:jme )
  REAL, INTENT(IN) :: mut( ims:ime , jms:jme )   ! Air mass on t-grid 
  REAL, INTENT(IN) :: muu( ims:ime , jms:jme )   ! Air mass on u-grid 
  REAL, INTENT(IN) :: muv( ims:ime , jms:jme )   ! Air mass on v-grid
  REAL, INTENT(IN) :: msft( ims:ime , jms:jme )  ! Map scale on t-grid
  REAL, INTENT(IN) :: msfu( ims:ime , jms:jme )  ! Map scale on u-grid
  REAL, INTENT(IN) :: msfv( ims:ime , jms:jme )  ! Map scale on v-grid

  REAL, INTENT(IN) :: p_phy( ims:ime, kms:kme, jms:jme )
  REAL, INTENT(INOUT) :: t_tendf( ims:ime, kms:kme, jms:jme )
  REAL, INTENT(IN) :: t0
  REAL, INTENT(INOUT) :: savwt( nobs_ndg_vars, ims:ime, kms:kme, jms:jme )


  REAL,   INTENT(IN) :: ub( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(IN) :: vb( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(IN) :: tb( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(IN) :: qvb( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(IN) :: pbase( ims:ime, kms:kme, jms:jme ) ! Base press. (Pa)
  REAL,   INTENT(IN) :: ptop
  REAL,   INTENT(IN) :: pp( ims:ime, kms:kme, jms:jme )  ! Press. pert. (Pa)
  REAL,   INTENT(IN) :: uratx( ims:ime, jms:jme )     ! On mass points
  REAL,   INTENT(IN) :: vratx( ims:ime, jms:jme )     !       "
  REAL,   INTENT(IN) :: tratx( ims:ime, jms:jme )     !       "
  REAL,   INTENT(INOUT) :: ru_tendf( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(INOUT) :: rv_tendf( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(INOUT) :: moist_tend( ims:ime, kms:kme, jms:jme )

! Local variables
  logical            :: nudge_flag   ! Flag for doing nudging 
  integer            :: KTAU         ! Forecast timestep
  real               :: xtime        ! Forecast time in minutes
  real               :: dtmin        ! dt in minutes
  integer            :: i, j, k      ! Loop counters.
  integer            :: idom         ! Loop counter.
  integer            :: nsta         ! Number of observation stations
  integer            :: infr         ! Frequency for obs input and error calc 
  integer            :: idarst       ! Flag for calling sub errob on restart
  real               :: dtr          ! Abs value of dtramp (for dynamic init)
  real               :: tconst       ! Reciprocal of dtr
  integer :: KPBLJ(its:ite)          ! 1D temp array.

  END SUBROUTINE fddaobs_driver

END MODULE module_fddaobs_driver
