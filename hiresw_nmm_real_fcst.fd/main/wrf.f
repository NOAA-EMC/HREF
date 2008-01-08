!WRF:DRIVER_LAYER:MAIN
!

PROGRAM wrf

   USE module_machine
   USE module_domain
   USE module_integrate
   USE module_driver_constants
   USE module_configure

   USE module_timing
   USE module_wrf_error
   USE esmf_mod

   IMPLICIT NONE

   REAL    :: time

   INTEGER :: loop , &
              levels_to_process

   TYPE (domain) , POINTER :: keep_grid, grid_ptr, null_domain
   TYPE (domain)           :: dummy
   TYPE (grid_config_rec_type)              :: config_flags
   INTEGER                 :: number_at_same_level
   INTEGER                 :: time_step_begin_restart

   INTEGER :: max_dom , domain_id , fid , oid , idum1 , idum2 , ierr
   INTEGER :: debug_level
   INTEGER :: rc
   LOGICAL :: input_from_file
   real*8 :: timef
   real   :: btim,btimx,btimi


   CHARACTER (LEN=80)      :: rstname
   CHARACTER (LEN=80)      :: message

   INTERFACE 
     SUBROUTINE Set_Timekeeping( grid )
      USE module_domain
      TYPE(domain), POINTER :: grid
     END SUBROUTINE Set_Timekeeping
   END INTERFACE

   !  Define the name of this program (program_name defined in module_domain)

   program_name = "WRF V1.3 MODEL"

	btim=timef()

   !  Get the NAMELIST data for input.

!	call start()

   CALL init_modules

   CALL initial_config

   CALL get_debug_level ( debug_level )
   CALL set_wrf_debug_level ( debug_level )

   ! allocated and configure the mother domain

   NULLIFY( null_domain )

   CALL       wrf_message ( program_name )
   CALL       wrf_debug ( 100 , 'wrf: calling alloc_and_configure_domain ' )
   CALL alloc_and_configure_domain ( domain_id  = 1 ,                  &
                                     grid       = head_grid ,          &
                                     parent     = null_domain ,        &
                                     kid        = -1                   )

   CALL       wrf_debug ( 100 , 'wrf: calling model_to_grid_config_rec ' )
   CALL model_to_grid_config_rec ( head_grid%id , model_config_rec , config_flags )
   CALL       wrf_debug ( 100 , 'wrf: calling set_scalar_indices_from_config ' )
   CALL set_scalar_indices_from_config ( head_grid%id , idum1, idum2 )
   CALL       wrf_debug ( 100 , 'wrf: calling init_wrfio' )
   CALL init_wrfio


   CALL Set_Timekeeping (head_grid)

write(0,*)'after Set_Timekeeping ',head_grid%dt

   CALL med_initialdata_input( head_grid , config_flags )

	write(0,*) 'post med_initialdata_input'
	write(0,*) 'head_grid%nmm_t(1,1,1),(80,1,220) : ', &
                    head_grid%nmm_t(1,1,1),head_grid%nmm_t(80,1,220)

write(0,*)'after med_initialdata_input ',head_grid%dt

   !  The forecast integration for the most coarse grid is now started.  The
   !  integration is from the first step (1) to the last step of the simulation.

!!! temp bogus
!	head_grid%nmm_q=0.
!!!

   head_grid%start_subtime = head_grid%start_time
   head_grid%stop_subtime = head_grid%stop_time
   CALL       wrf_debug ( 100 , 'wrf: calling integrate' )
	write(0,*) 'time to integrate call: ', (timef()-btim)*1.e-3
	btimi=timef()
   CALL integrate ( head_grid )
	write(0,*) 'time in integrate call: ', (timef()-btimi)*1.e-3
	btimx=timef()
   CALL       wrf_debug ( 100 , 'wrf: back from integrate' )

   CALL med_shutdown_io ( head_grid , config_flags )
   CALL       wrf_debug ( 100 , 'wrf: back from med_shutdown_io' )

   CALL       wrf_debug (   0 , 'wrf: SUCCESS COMPLETE WRF' )
!	call summary()
   CALL wrf_shutdown
	write(0,*) 'time from integrate call to end: ', (timef()-btimx)*1.e-3
	write(0,*) 'total time in wrf.F: ', (timef()-btim)*1.e-3

   STOP

END PROGRAM wrf

