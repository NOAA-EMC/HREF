
MODULE module_fddaobs_driver































 
CONTAINS


SUBROUTINE fddaobs_driver( inest, domid, parid, restart,         &
               nudge_opt, iprt_errob, iprt_nudob,                &
               fdasta, fdaend,                                   &
               nudge_wind, nudge_temp, nudge_mois,               &
               nudge_pstr,                                       &
               coef_wind, coef_temp, coef_mois,                  &
               coef_pstr, rinxy, rinsig,                         &
               npfi, ionf,                                       &
               obs_prt_max, obs_prt_freq, idynin, dtramp,        &
               parent_grid_ratio, maxdom, itimestep,             &
               dt, gmt, julday,                                  &
               max_obs, nobs_ndg_vars,    &
               nobs_err_flds, nstat, varobs, errf, dx,           &
               KPBL, HT, mut, muu, muv,                          &
               msftx, msfty, msfux, msfuy, msfvx, msfvy, p_phy, t_tendf, t0,             &
               ub, vb, tb, qvb, pbase, ptop, pp, phb, ph,        &
               uratx, vratx, tratx, ru_tendf, rv_tendf,          &
               moist_tend, savwt,                                &
               ids,ide, jds,jde, kds,kde,                        & 
               ims,ime, jms,jme, kms,kme,                        & 
               its,ite, jts,jte, kts,kte                         ) 


  USE module_domain
  USE module_bc
  USE module_model_constants, ONLY : g, rcp
  USE module_fddaobs_rtfdda






  IMPLICIT NONE












  INTEGER, intent(in)  :: ids,ide, jds,jde, kds,kde  
  INTEGER, intent(in)  :: ims,ime, jms,jme, kms,kme  
  INTEGER, intent(in)  :: its,ite, jts,jte, kts,kte  

  INTEGER, intent(in)  :: inest
  INTEGER, intent(in)  :: maxdom
  INTEGER, intent(in)  :: domid(maxdom)           
  INTEGER, intent(in)  :: parid(maxdom)           
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
  INTEGER, intent(in) :: npfi
  INTEGER, intent(in) :: ionf
  INTEGER, intent(in) :: obs_prt_max      
  INTEGER, intent(in) :: obs_prt_freq     
  INTEGER, intent(in) :: idynin
  REAL, intent(inout) :: dtramp
  INTEGER, intent(in) :: parent_grid_ratio
  REAL, intent(in)     :: dt
  REAL, intent(in)     :: gmt
  INTEGER, intent(in)  :: julday
  INTEGER, intent(in)  :: max_obs         
  INTEGER, intent(in)  :: nobs_ndg_vars
  INTEGER, intent(in)  :: nobs_err_flds
  INTEGER, intent(in)  :: nstat
  REAL, intent(inout)  :: varobs(nobs_ndg_vars, max_obs)
  REAL, intent(inout)  :: errf(nobs_err_flds, max_obs)
  REAL, intent(in)     :: dx           
  INTEGER, INTENT(IN) :: kpbl( ims:ime, jms:jme )
  REAL, INTENT(IN) :: ht( ims:ime, jms:jme )
  REAL, INTENT(IN) :: mut( ims:ime , jms:jme )   
  REAL, INTENT(IN) :: muu( ims:ime , jms:jme )   
  REAL, INTENT(IN) :: muv( ims:ime , jms:jme )   
  REAL, INTENT(IN) :: msftx( ims:ime , jms:jme )  
  REAL, INTENT(IN) :: msfty( ims:ime , jms:jme )  
  REAL, INTENT(IN) :: msfux( ims:ime , jms:jme )  
  REAL, INTENT(IN) :: msfuy( ims:ime , jms:jme )  
  REAL, INTENT(IN) :: msfvx( ims:ime , jms:jme )  
  REAL, INTENT(IN) :: msfvy( ims:ime , jms:jme )  

  REAL, INTENT(IN) :: p_phy( ims:ime, kms:kme, jms:jme )
  REAL, INTENT(INOUT) :: t_tendf( ims:ime, kms:kme, jms:jme )
  REAL, INTENT(IN) :: t0
  REAL, INTENT(INOUT) :: savwt( nobs_ndg_vars, ims:ime, kms:kme, jms:jme )


  REAL,   INTENT(IN) :: ub( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(IN) :: vb( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(IN) :: tb( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(IN) :: qvb( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(IN) :: pbase( ims:ime, kms:kme, jms:jme ) 
  REAL,   INTENT(IN) :: ptop
  REAL,   INTENT(IN) :: pp( ims:ime, kms:kme, jms:jme )  
  REAL,   INTENT(IN) :: phb( ims:ime, kms:kme, jms:jme ) 
  REAL,   INTENT(IN) :: ph( ims:ime, kms:kme, jms:jme )  
  REAL,   INTENT(IN) :: uratx( ims:ime, jms:jme )     
  REAL,   INTENT(IN) :: vratx( ims:ime, jms:jme )     
  REAL,   INTENT(IN) :: tratx( ims:ime, jms:jme )     
  REAL,   INTENT(INOUT) :: ru_tendf( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(INOUT) :: rv_tendf( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(INOUT) :: moist_tend( ims:ime, kms:kme, jms:jme )


  logical            :: nudge_flag   
  integer            :: KTAU         
  real               :: xtime        
  real               :: dtmin        
  integer            :: i, j, k      
  integer            :: idom         
  integer            :: nsta         
  integer            :: infr         
  integer            :: idarst       
  real               :: dtr          
  real               :: tconst       
  integer :: KPBLJ(its:ite)          

  END SUBROUTINE fddaobs_driver

END MODULE module_fddaobs_driver
