!WRF:DRIVER_LAYER:INTEGRATION
!

MODULE module_integrate

CONTAINS

! module_integrate:integrate
! 
! This is a driver-level routine that controls the integration of a
! domain and subdomains rooted at the domain. The integrate routine takes
! a domain pointed to by the argument grid and advances the grid and
! its associated nests from integer start_step to end_step which
! correspond to starting and ending steps on grid.
! 
! Integrate calls a number of mediation-supplied subroutines, whose
! interface blocks are provided as part of the routine.
! 
! Flow of control narrative
! 
! The routine first checks to see that it is not finished yet by
! testing to see if the total number of steps taken on the grid are
! less than the number asked for with the end_step argument. If so:
! 
! - Model_to_grid_config_rec is called to load the local config_flags
! structure with the configuration information for the grid stored
! in model_config_rec and indexed by the grids unique integer id.
! 
! - A mediation layer supplied subroutine, med_calc_model_time, is called
! to provide the driver layers notion of model time (integer steps) to
! the mediation and model layers below.
! 
! - The routine then executes a loop over steps to advance the grid
! forward the number of steps needed to bring it forward to end_step.
! 
! -- A mediation layer supplied subroutine, mediation_setup_step, to 
! allow the mediation/model layer to perform any initializations that
! are needed at the start of a grid time step
! 
! -- The code checks to see if any nests need to be opened by calling the
! framework-supplied routine nests_to_open.  If so, the nest is allocated
! an configured (framework-supplied routine alloc_and_configure_domain)
! and the initialized (mediation-supplied routine med_nest_initial).
! 
! -- If any nest was opened in the preceding step, the framework-supplied
! routine set_overlaps is called to (re)adjust overlapping regions of
! nests of the grid, the configuration of which may have changed as the
! result of introducing a new nest or nests.
! 
! -- The routine med_before_solve_io is called to give the
! mediation/model layer an opportunity to perform any I/O that needs to
! occur before the grid is integrated forward one step. In the case of
! the WRF model, for example, the model may perform lateral boundary
! input, history output, or restart output for the grid at this point.
! 
! -- The mediation-supplied solver (solve_interface) is called to advance
! the grid and all its siblings (overlapping grids at same nest level)
! forward one step.
! 
! -- For each active nest of the grid, the the integrate routine is
! called recursively to advance the nest subtree forward to the same
! time-level as grid.
! 
! -- Mediation-supplied subroutine, med_calc_model_time, is called again
! to provide update of driver layers notion of model time (integer
! steps) to the mediation and model layers below.
! 
! -- Mediation-supplied subroutine, med_after_solve_io, is called to give
! the mediation/model layer an opportunity to perform any I/O that needs
! to occur after the grid is integrated forward one step. In the case of
! WRF, for example, this is a no-op (nothing done).
! 
! - Following the completion of the loop over steps, the
! mediation-supplied routine med_last_solve_io is called to provide the
! mediation/model layer an opportunity to perform any I/O that needs to
! occur once the integration through end_step has completed.  In the case
! of WRF, for example, this is used to perform the final history and
! restart I/O when the integration of the top-level grid finishes.

RECURSIVE SUBROUTINE integrate ( grid )

   USE module_domain
   USE module_driver_constants
   USE module_nesting
   USE module_configure
   USE module_timing
   USE esmf_mod

   IMPLICIT NONE

   !  Input data.

   TYPE(domain) , POINTER :: grid

   !  Local data.

   CHARACTER*32                           :: outname, rstname
   TYPE(domain) , POINTER                 :: grid_ptr , new_nest
   INTEGER                                :: step
   INTEGER                                :: nestid , kid
   LOGICAL                                :: a_nest_was_opened
   INTEGER                                :: fid , rid
   LOGICAL                                :: lbc_opened
   REAL                                   :: time, btime, bfrq
   CHARACTER*256                          :: message, message2
   TYPE (grid_config_rec_type)            :: config_flags
   LOGICAL , EXTERNAL                     :: wrf_dm_on_monitor
   INTEGER                                :: idum1 , idum2 , ierr , open_status
   INTEGER                                :: rc

   ! interface
   INTERFACE
       ! mediation-supplied solver
     SUBROUTINE solve_interface ( grid )
       USE module_domain
       TYPE (domain) grid
     END SUBROUTINE solve_interface
       ! mediation-supplied routine to allow driver to pass time information
       ! down to mediation/model layer
     SUBROUTINE med_calc_model_time ( grid , config_flags )
       USE module_domain
       USE module_configure
       TYPE (domain) grid
       TYPE (grid_config_rec_type) config_flags
     END SUBROUTINE med_calc_model_time
       ! mediation-supplied routine that gives mediation layer opportunity to 
       ! perform I/O before the call ot the solve routine
     SUBROUTINE med_before_solve_io ( grid , config_flags )
       USE module_domain
       USE module_configure
       TYPE (domain) grid
       TYPE (grid_config_rec_type) config_flags
     END SUBROUTINE med_before_solve_io
       ! mediation-supplied routine that gives mediation layer opportunity to 
       ! perform I/O after the call ot the solve routine
     SUBROUTINE med_after_solve_io ( grid , config_flags )
       USE module_domain
       USE module_configure
       TYPE (domain) grid
       TYPE (grid_config_rec_type) config_flags
     END SUBROUTINE med_after_solve_io
       ! mediation-supplied routine that gives mediation layer opportunity to 
       ! perform I/O to initialize a new nest
     SUBROUTINE med_nest_initial ( parent , grid , config_flags )
       USE module_domain
       USE module_configure
       TYPE (domain), POINTER ::  grid , parent
       TYPE (grid_config_rec_type) config_flags
     END SUBROUTINE med_nest_initial

       ! mediation-supplied routine that gives mediation layer opportunity to 
       ! provide parent->nest forcing
     SUBROUTINE med_nest_force ( parent , grid , config_flags )
       USE module_domain
       USE module_configure
       TYPE (domain), POINTER ::  grid , parent
       TYPE (grid_config_rec_type) config_flags
     END SUBROUTINE med_nest_force

       ! mediation-supplied routine that gives mediation layer opportunity to 
       ! provide parent->nest feedback
     SUBROUTINE med_nest_feedback ( parent , grid , config_flags )
       USE module_domain
       USE module_configure
       TYPE (domain), POINTER ::  grid , parent
       TYPE (grid_config_rec_type) config_flags
     END SUBROUTINE med_nest_feedback

       ! mediation-supplied routine that gives mediation layer opportunity to 
       ! perform I/O prior to the close of this call to integrate
     SUBROUTINE med_last_solve_io ( grid , config_flags )
       USE module_domain
       USE module_configure
       TYPE (domain) grid
       TYPE (grid_config_rec_type) config_flags
     END SUBROUTINE med_last_solve_io
       ! mediation-supplied routine that gives mediation layer opportunity to 
       ! perform setup before iteration over steps in this call to integrate
     SUBROUTINE med_setup_step ( grid , config_flags )
       USE module_domain
       USE module_configure
       TYPE (domain) grid
       TYPE (grid_config_rec_type) config_flags
     END SUBROUTINE med_setup_step
       ! mediation-supplied routine that intializes the nest from the grid
       ! by interpolation

     SUBROUTINE Set_Timekeeping( grid )
       USE module_domain
       TYPE(domain), POINTER :: grid
     END SUBROUTINE

   END INTERFACE

   IF ( .NOT. ESMF_ClockIsStopTime(grid%domain_clock ,rc=rc) ) THEN
      CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
      CALL ESMF_ClockGetCurrTime( grid%domain_clock, grid%current_time, rc=rc )
      DO WHILE ( grid%current_time .LT. grid%stop_subtime )
         IF ( wrf_dm_on_monitor() ) THEN
           CALL start_timing
         END IF
         CALL med_setup_step ( grid , config_flags )
         a_nest_was_opened = .false.
         DO WHILE ( nests_to_open( grid , nestid , kid ) )
            a_nest_was_opened = .true.
            CALL alloc_and_configure_domain ( domain_id  = nestid ,                          &
                                              grid       = new_nest ,                        &
                                              parent     = grid ,                            &
                                              kid        = kid                               )
            CALL med_nest_initial ( grid , new_nest , config_flags )
            CALL Set_Timekeeping (grid)
         END DO
         IF ( a_nest_was_opened ) THEN
            CALL set_overlaps ( grid )   ! find overlapping and set pointers
         END IF
         CALL med_before_solve_io ( grid , config_flags )
         grid_ptr => grid
         DO WHILE ( ASSOCIATED( grid_ptr ) )
                           CALL wrf_debug( 100 , 'module_integrate: calling solve interface ' )
            CALL solve_interface ( grid_ptr ) 
                           CALL wrf_debug( 100 , 'module_integrate: back from solve interface ' )
            grid_ptr => grid_ptr%sibling
         END DO
         grid_ptr => grid
         DO WHILE ( ASSOCIATED( grid_ptr ) )
            DO kid = 1, max_nests
              IF ( ASSOCIATED( grid_ptr%nests(kid)%ptr ) ) THEN
                ! Recursive -- advance nests from previous time level to this time level.
                           CALL wrf_debug( 100 , 'module_integrate: calling med_nest_force ' )
                CALL med_nest_force ( grid_ptr , grid_ptr%nests(kid)%ptr , config_flags )
                           CALL wrf_debug( 100 , 'module_integrate: back from med_nest_force ' )
                grid_ptr%nests(kid)%ptr%start_subtime = grid%current_time
                grid_ptr%nests(kid)%ptr%stop_subtime = grid%current_time + grid%step_time
                CALL integrate ( grid_ptr%nests(kid)%ptr ) 
                           CALL wrf_debug( 100 , 'module_integrate: back from recursive call to integrate ' )
                           CALL wrf_debug( 100 , 'module_integrate: calling med_nest_feedback ' )
                CALL med_nest_feedback ( grid_ptr , grid_ptr%nests(kid)%ptr , config_flags )
                           CALL wrf_debug( 100 , 'module_integrate: back from med_nest_feedback ' )
              END IF
            END DO
            grid_ptr => grid_ptr%sibling
         END DO
         CALL med_calc_model_time ( grid , config_flags )
         CALL med_after_solve_io ( grid , config_flags )
         !  Report on the timing for a single time step.
         CALL ESMF_ClockAdvance( grid%domain_clock, rc=rc )
         CALL ESMF_ClockGetCurrTime( grid%domain_clock, grid%current_time, rc=rc )
         IF ( wrf_dm_on_monitor() ) THEN
           CALL ESMF_TimeGetString( grid%current_time, message2, rc=rc )
           WRITE ( message , FMT = '("main: time ",A," on domain ",I3)' ) TRIM(message2), grid%id
           CALL end_timing ( TRIM(message) )
         END IF
      END DO
      CALL med_last_solve_io ( grid , config_flags )
   END IF

END SUBROUTINE integrate

END MODULE module_integrate
