!#define ONEWAY

SUBROUTINE med_force_domain ( parent_grid , nested_grid )
   USE module_domain
   USE module_configure
   IMPLICIT NONE
   TYPE(domain), POINTER :: parent_grid , nested_grid
   TYPE(domain), POINTER :: grid
   INTEGER nlev, msize
   TYPE (grid_config_rec_type)            :: config_flags

   INTERFACE




   END INTERFACE

write(0,*)'entered med_force_domain '
write(0,*)' nested_grid%id  ',nested_grid%id

   CALL model_to_grid_config_rec ( nested_grid%id , model_config_rec , config_flags )

write(0,*)'back from model_to_grid_config_rec in med_force_domain '

   grid => parent_grid

write(0,*)' med_force_domain 1'


   IF ( .FALSE. ) THEN




   ENDIF



   nested_grid%dtbc = 0.

   RETURN
END SUBROUTINE med_force_domain


