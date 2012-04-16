


SUBROUTINE med_feedback_domain ( parent_grid , nested_grid )
   USE module_domain
   USE module_configure
   IMPLICIT NONE
   TYPE(domain), POINTER :: parent_grid , nested_grid
   TYPE(domain), POINTER :: grid
   INTEGER nlev, msize
   TYPE (grid_config_rec_type)            :: config_flags

   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
   INTEGER     :: sm31x, em31x, sm32x, em32x, sm33x, em33x
   INTEGER     :: sm31y, em31y, sm32y, em32y, sm33y, em33y




   INTERFACE









   END INTERFACE



























   RETURN
END SUBROUTINE med_feedback_domain


