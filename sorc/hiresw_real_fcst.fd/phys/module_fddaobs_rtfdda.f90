

MODULE module_fddaobs_rtfdda


































CONTAINS


  SUBROUTINE fddaobs_init(obs_nudge_opt, maxdom, inest, parid,         &
                          idynin, dtramp, fdaend, restart,             &
                          obs_twindo_cg, obs_twindo, itimestep,        &
                          xlat, xlong,                                 &
                          iprt,                                        &
                          ids,ide, jds,jde, kds,kde,                   &
                          ims,ime, jms,jme, kms,kme,                   &
                          its,ite, jts,jte, kts,kte)     




  USE module_domain
  USE module_dm                 

  IMPLICIT NONE





  INTEGER, intent(in)  :: maxdom
  INTEGER, intent(in)  :: obs_nudge_opt(maxdom)
  INTEGER, intent(in)  :: ids,ide, jds,jde, kds,kde,                 &
                          ims,ime, jms,jme, kms,kme,                 &
                          its,ite, jts,jte, kts,kte
  INTEGER, intent(in)  :: inest
  INTEGER, intent(in)  :: parid(maxdom)
  INTEGER, intent(in)  :: idynin         
  REAL,    intent(in)  :: dtramp         
  REAL,    intent(in)  :: fdaend(maxdom) 
  LOGICAL, intent(in)  :: restart
  REAL, intent(in)     :: obs_twindo_cg  
  REAL, intent(in)     :: obs_twindo
  INTEGER, intent(in)  :: itimestep
  REAL, DIMENSION( ims:ime, jms:jme ),                            &
        INTENT(IN)     :: xlat, xlong  
  LOGICAL, intent(in)  :: iprt         


  logical            :: nudge_flag      
  integer            :: ktau            
  integer            :: nest            
  integer            :: idom            
  integer            :: parent          
  real               :: conv            
  real               :: tl1             
  real               :: tl2             
  real               :: xn1
  real               :: known_lat       
  real               :: known_lon       
  character(len=200) :: msg             

  END SUBROUTINE fddaobs_init


END MODULE module_fddaobs_rtfdda

