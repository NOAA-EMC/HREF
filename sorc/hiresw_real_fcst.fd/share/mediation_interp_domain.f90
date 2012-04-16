


SUBROUTINE med_interp_domain ( parent_grid , nested_grid )
   USE module_domain
   USE module_configure
   USE module_timing
   IMPLICIT NONE
   TYPE(domain), POINTER :: parent_grid , nested_grid
   TYPE(domain), POINTER :: grid
   INTEGER nlev, msize
   TYPE (grid_config_rec_type)            :: config_flags





   INTERFACE









   END INTERFACE





























   RETURN
END SUBROUTINE med_interp_domain


