
SUBROUTINE med_nest_move ( parent, nest )
  
   USE module_domain
   USE module_timing
   USE module_configure
   USE module_io_domain
   USE module_dm
   TYPE(domain) , POINTER                     :: parent, nest, grid
   INTEGER dx, dy       
END SUBROUTINE med_nest_move

LOGICAL FUNCTION time_for_move2 ( parent , grid , move_cd_x, move_cd_y )
  
   USE module_domain
   USE module_configure
   USE module_compute_geop
   USE module_dm
   USE module_utility
   IMPLICIT NONE

   TYPE(domain) , POINTER    :: parent, grid
   INTEGER, INTENT(OUT)      :: move_cd_x , move_cd_y
   time_for_move2 = .FALSE.
END FUNCTION time_for_move2

LOGICAL FUNCTION time_for_move ( parent , grid , move_cd_x, move_cd_y )
   USE module_domain
   USE module_configure
   USE module_dm
USE module_timing
   USE module_utility
   IMPLICIT NONE

   TYPE(domain) , POINTER    :: parent, grid, par, nst
   INTEGER, INTENT(OUT)      :: move_cd_x , move_cd_y
   time_for_move = .FALSE.
END FUNCTION time_for_move


LOGICAL FUNCTION should_not_move ( id )
  USE module_state_description
  USE module_configure
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: id
 
  LOGICAL retval
  INTEGER cu_physics, ra_sw_physics, ra_lw_physics, sf_urban_physics, obs_nudge_opt

  retval = .FALSE.

  CALL nl_get_cu_physics( id , cu_physics )
  IF ( cu_physics .EQ. GDSCHEME ) THEN
    CALL wrf_message('Grell cumulus can not be specified with moving nests. Movement disabled.')
    retval = .TRUE.
  ENDIF

  CALL nl_get_ra_sw_physics( id , ra_sw_physics )
  IF ( ra_sw_physics .EQ. CAMSWSCHEME ) THEN
    CALL wrf_message('CAM SW radiation can not be specified with moving nests. Movement disabled.')
    retval = .TRUE.
  ENDIF
  CALL nl_get_ra_lw_physics( id , ra_lw_physics )
  IF ( ra_lw_physics .EQ. CAMLWSCHEME ) THEN
    CALL wrf_message('CAM LW radiation can not be specified with moving nests. Movement disabled.')
    retval = .TRUE.
  ENDIF

  CALL nl_get_sf_urban_physics( id , sf_urban_physics )
  IF ( sf_urban_physics .EQ. 1 .OR. sf_urban_physics .EQ. 2 ) THEN
    CALL wrf_message('UCMs Noah LSM can not be specified with moving nests. Movement disabled.')
    retval = .TRUE.
  ENDIF
  should_not_move = retval
END FUNCTION

