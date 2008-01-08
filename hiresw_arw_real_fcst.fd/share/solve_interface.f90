!WRF:MEDIATION_LAYER:ADT_BARRIER
!

SUBROUTINE solve_interface ( grid ) 

   USE module_domain
   USE module_timing
   USE module_driver_constants
   USE module_configure
   USE module_wrf_error

   IMPLICIT NONE

   INTERFACE
SUBROUTINE solve_em      ( grid , config_flags     &
!
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/em_dummy_new_args.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
,u_b,u_bt,v_b,v_bt,w_b,w_bt,ph_b,ph_bt,t_b,t_bt,mu_b,mu_bt,moist,moist_b,moist_bt,chem,scalar,scalar_b,scalar_bt,ozmixm, &
aerosolc_1,aerosolc_2,fdda3d,fdda2d &
!ENDOFREGISTRYGENERATEDINCLUDE
!
                 )

   USE module_domain
   USE module_configure
   USE module_driver_constants

   !  Input data.
   TYPE(domain) , INTENT(INOUT)                  :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)      :: config_flags

!STARTOFREGISTRYGENERATEDINCLUDE 'inc/em_dummy_new_decl.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: u_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: u_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: v_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: v_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: w_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: w_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: ph_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: ph_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: t_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: t_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),1,grid%spec_bdy_width,4)           :: mu_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),1,grid%spec_bdy_width,4)           :: mu_bt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: moist
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4,num_moist)           :: moist_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4,num_moist)           :: moist_bt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_chem)           :: chem
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_scalar)           :: scalar
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4,num_scalar)           :: scalar_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4,num_scalar)           :: scalar_bt
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%levsiz,grid%sm33:grid%em33,num_ozmixm)           :: ozmixm
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%paerlev,grid%sm33:grid%em33,num_aerosolc)           :: aerosolc_1
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%paerlev,grid%sm33:grid%em33,num_aerosolc)           :: aerosolc_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_fdda3d)           :: fdda3d
real      ,DIMENSION(grid%sm31:grid%em31,1:1,grid%sm33:grid%em33,num_fdda2d)           :: fdda2d
!ENDOFREGISTRYGENERATEDINCLUDE

END SUBROUTINE solve_em

   END INTERFACE

   TYPE(domain) , INTENT(INOUT)  :: grid
   TYPE (grid_config_rec_type)   :: config_flags

   INTEGER     :: idum1, idum2


   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
   CALL set_scalar_indices_from_config ( grid%id , idum1 , idum2 )



   IF ( config_flags%dyn_opt == DYN_NODYN  ) THEN

     CALL wrf_debug( 1 , "solve_interface: dynamics disabled\n" )

   ELSE IF ( config_flags%dyn_opt == DYN_EM  ) THEN
     CALL solve_em  ( grid , config_flags  &
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


! ###### 4. Edit share/solve_interface.F to add call to experimental core


   ELSE

     WRITE( wrf_err_message , * ) 'Invalid dynamics option: dyn_opt = ',config_flags%dyn_opt
     CALL wrf_error_fatal3 ( "solve_interface.b" , 110 ,  TRIM ( wrf_err_message ) )
     
   END IF

END SUBROUTINE solve_interface

