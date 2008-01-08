!WRF:MEDIATION_LAYER:ADT_BARRIER
!

SUBROUTINE start_domain ( grid , allowed_to_read )

   USE module_domain
   USE module_configure

   IMPLICIT NONE

   !  Input Arguments.
   TYPE (domain)          :: grid
   LOGICAL, INTENT(IN)    :: allowed_to_read
   !  Local data.
   INTEGER                :: dyn_opt
   INTEGER :: idum1, idum2




   CALL nl_get_dyn_opt( 1, dyn_opt )
  
   CALL set_scalar_indices_from_config ( head_grid%id , idum1, idum2 )

   IF (      dyn_opt .eq. DYN_NODYN ) THEN

   ELSE IF (      dyn_opt .eq. DYN_EM ) THEN

     CALL start_domain_em( grid, allowed_to_read  &
!
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/em_actual_new_args.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
,grid%em_u_b,grid%em_u_bt,grid%em_v_b,grid%em_v_bt,grid%em_w_b,grid%em_w_bt,grid%em_ph_b,grid%em_ph_bt,grid%em_t_b,grid%em_t_bt, &
grid%em_mu_b,grid%em_mu_bt,grid%moist,grid%moist_b,grid%moist_bt,grid%chem,grid%scalar,grid%scalar_b,grid%scalar_bt,grid%ozmixm, &
grid%aerosolc_1,grid%aerosolc_2,grid%fdda3d,grid%fdda2d &
!ENDOFREGISTRYGENERATEDINCLUDE
!
                         )

!### 4a. edit share/start_domain.F to call domain inits for core if any


   ELSE

     WRITE(0,*)' start_domain: unknown or unimplemented dyn_opt = ',dyn_opt
     CALL wrf_error_fatal3 ( "start_domain.b" , 68 ,  ' start_domain: unknown or unimplemented dyn_opt ' ) 
   ENDIF


END SUBROUTINE start_domain

