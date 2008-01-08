!#define ONEWAY

SUBROUTINE med_interp_domain ( parent_grid , nested_grid )
   USE module_domain
   USE module_configure
   IMPLICIT NONE
   TYPE(domain), POINTER :: parent_grid , nested_grid
   TYPE(domain), POINTER :: grid
   INTEGER nlev, msize
   TYPE (grid_config_rec_type)            :: config_flags

   INTERFACE




   END INTERFACE

   CALL model_to_grid_config_rec ( nested_grid%id , model_config_rec , config_flags )

   grid => parent_grid


! Serial code doesnt bother with intermediate grid. Go direct from Coarse to Nest.
! Only part 1 is called.

   IF      ( .FALSE. )                        THEN



   ENDIF


!#ifndef ONEWAY
!write(0,*)calling med_history_out for nested_grid
!write(0,*)Timing calling med_history_out from med_interp_domain ,nested_grid%id
!CALL med_history_out ( nested_grid , config_flags, 0, 0, 0 )
!write(0,*)back from med_history_out for nested_grid
!#endif


   RETURN
END SUBROUTINE med_interp_domain


