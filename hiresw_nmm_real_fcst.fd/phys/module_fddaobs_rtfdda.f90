!WRF:MODEL_LAYER:PHYSICS
!
MODULE module_fddaobs_rtfdda

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

!------------------------------------------------------------------------------
  SUBROUTINE fddaobs_init(obs_nudge_opt, maxdom, inest, parid,         &
                          dx_coarse, restart, obs_twindo, itimestep,   &
                          e_sn, s_sn_cg, e_sn_cg, s_we_cg, e_we_cg,    &
                          ids,ide, jds,jde, kds,kde,                   &
                          ims,ime, jms,jme, kms,kme,                   &
                          its,ite, jts,jte, kts,kte)     
!-----------------------------------------------------------------------
!  This routine does initialization for real time fdda obs-nudging.
!
!-----------------------------------------------------------------------
  USE module_domain
!-----------------------------------------------------------------------
  IMPLICIT NONE
!-----------------------------------------------------------------------

!=======================================================================
! Definitions
!-----------
  INTEGER, intent(in)  :: maxdom
  INTEGER, intent(in)  :: obs_nudge_opt(maxdom)
  INTEGER, intent(in)  :: ids,ide, jds,jde, kds,kde,                 &
                          ims,ime, jms,jme, kms,kme,                 &
                          its,ite, jts,jte, kts,kte
  INTEGER, intent(in)  :: inest
  INTEGER, intent(in)  :: parid(maxdom)
  REAL    ,intent(in)  :: dx_coarse    ! coarse-domain grid cell-size (km)
  LOGICAL, intent(in)  :: restart
  REAL, intent(inout)  :: obs_twindo
  INTEGER, intent(in)  :: itimestep
  INTEGER, intent(in)  :: e_sn         ! ending   south-north grid index
  INTEGER, intent(in)  :: s_sn_cg      ! starting south-north coarse-grid index
  INTEGER, intent(in)  :: e_sn_cg      ! ending   south-north coarse-grid index
  INTEGER, intent(in)  :: s_we_cg      ! starting west-east   coarse-grid index
  INTEGER, intent(in)  :: e_we_cg      ! ending   west-east   coarse-grid index

! Local variables
  logical            :: nudge_flag      ! nudging flag for this nest 
  integer            :: ktau            ! current timestep
  integer            :: nest            ! loop counter
  integer            :: idom            ! domain id
  integer            :: parent          ! parent domain

  END SUBROUTINE fddaobs_init


END MODULE module_fddaobs_rtfdda

