

































  SUBROUTINE wrf_fddaobs_in (grid ,config_flags)

    USE module_domain
    USE module_configure
    USE module_model_constants        

    IMPLICIT NONE
    TYPE(domain) :: grid
    TYPE(grid_config_rec_type),  INTENT(IN)    :: config_flags
  END SUBROUTINE wrf_fddaobs_in



