!WRF:MEDIATION_LAYER:SOLVER

SUBROUTINE solve_em ( grid , config_flags  &
! Actual arguments generated from Registry
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

! Driver layer modules
   USE module_domain
   USE module_configure
   USE module_driver_constants
   USE module_machine
   USE module_tiles
   USE module_dm
! Mediation layer modules
! Model layer modules
   USE module_model_constants
   USE module_small_step_em
   USE module_em
   USE module_big_step_utilities_em
   USE module_bc
   USE module_bc_em
   USE module_solvedebug_em
   USE module_physics_addtendc
   USE module_diffusion_em
! Registry generated module
   USE module_state_description
   USE module_radiation_driver
   USE module_surface_driver
   USE module_cumulus_driver
   USE module_microphysics_driver
   USE module_microphysics_zero_out
   USE module_pbl_driver
   USE module_fddagd_driver
   USE module_fddaobs_driver
   USE module_diagnostics

   IMPLICIT NONE

   !  Input data.

   TYPE(domain) , TARGET          :: grid

   !  Definitions of dummy arguments to this routine (generated from Registry).
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

   !  Structure that contains run-time configuration (namelist) data for domain
   TYPE (grid_config_rec_type) , INTENT(IN)          :: config_flags

   ! Local data

   INTEGER                         :: k_start , k_end, its, ite, jts, jte
   INTEGER                         :: ids , ide , jds , jde , kds , kde , &
                                      ims , ime , jms , jme , kms , kme , &
                                      ips , ipe , jps , jpe , kps , kpe
   INTEGER                         :: ij , iteration
   INTEGER                         :: im , num_3d_m , ic , num_3d_c , is , num_3d_s
   INTEGER                         :: loop
   INTEGER                         :: ijds, ijde
   INTEGER                         :: sz

   LOGICAL                         :: specified_bdy, channel_bdy


! storage for tendencies and decoupled state (generated from Registry)

!STARTOFREGISTRYGENERATEDINCLUDE 'inc/em_i1_decl.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: ru_tendf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rv_tendf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: ww1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: wwp
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rw_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rw_tendf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: w_save
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: ph_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: ph_tendf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: ph_save
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: t_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: t_tendf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: t_2save
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: muus
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: muvs
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: muave
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: mu_save
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: mu_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: mu_tendf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: tke_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: advect_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: alpha
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: gamma
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: c2a
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rho
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: phm
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: cqu
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: cqv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: cqw
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: pm1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: mu_3d
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: z_at_w
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: psim
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: psih
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: wspd
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: gz1oz0
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: br
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: tshltr
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: qshltr
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: pshltr
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: th10
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: q10
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: chklowq
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: th_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: pi_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: p_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: t_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: u_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: v_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: dz8w
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: p8w
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: t8w
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rho_phy
logical   ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: cu_act_flag
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: hol
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: regime
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: moist_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: moist_old
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_chem)           :: chem_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_chem)           :: chem_old
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_scalar)           :: scalar_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_scalar)           :: scalar_old
!ENDOFREGISTRYGENERATEDINCLUDE
! Previous time level of tracer arrays now defined as i1 variables;
! the state 4d arrays now redefined as 1-time level arrays in Registry.
! Benefit: save memory in nested runs, since only 1 domain is active at a
! time.  Potential problem on stack-limited architectures: increases
! amount of data on program stack by making these automatic arrays.

   INTEGER :: rc 
   INTEGER :: number_of_small_timesteps, rk_step
   INTEGER :: klevel,ijm,ijp,i,j,k,size1,size2    ! for prints/plots only
   INTEGER :: idum1, idum2, dynamics_option

   INTEGER :: rk_order, iwmax, jwmax, kwmax
   REAL :: dt_rk, dts_rk, dtm, wmax
   INTEGER :: l,kte,kk

! urban related variables
   INTEGER :: NUM_ROOF_LAYERS, NUM_WALL_LAYERS, NUM_ROAD_LAYERS   ! urban

! Define benchmarking timers if -DBENCH is compiled

!----------------------
! Executable statements
!----------------------

! Needed by some comm layers, grid%e.g. RSL. If needed, nmm_data_calls.inc is
! generated from the registry.  The definition of REGISTER_I1 allows
! I1 data to be communicated in this routine if necessary.
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/em_data_calls.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
!ENDOFREGISTRYGENERATEDINCLUDE

!<DESCRIPTION>
!<pre>
! solve_em is the main driver for advancing a grid a single timestep.
! It is a mediation-layer routine -> DM and SM calls are made where 
! needed for parallel processing.  
!
! solve_em can integrate the equations using 3 time-integration methods
!      
!    - 3rd order Runge-Kutta time integration (recommended)
!      
!    - 2nd order Runge-Kutta time integration
!      
! The main sections of solve_em are
!     
! (1) Runge-Kutta (RK) loop
!     
! (2) Non-timesplit physics (i.grid%e., tendencies computed for updating
!     model state variables during the first RK sub-step (loop)
!     
! (3) Small (acoustic, sound) timestep loop - within the RK sub-steps
!     
! (4) scalar advance for moist and chem scalar variables (and TKE)
!     within the RK sub-steps.
!     
! (5) time-split physics (after the RK step), currently this includes
!     only microphyics
!
! A more detailed description of these sections follows.
!</pre>
!</DESCRIPTION>

! Initialize timers if compiled with -DBENCH





















































!  set runge-kutta solver (2nd or 3rd order)

   dynamics_option = config_flags%rk_ord

!  Obtain dimension information stored in the grid data structure.
  CALL get_ijk_from_grid (  grid ,                   &
                            ids, ide, jds, jde, kds, kde,    &
                            ims, ime, jms, jme, kms, kme,    &
                            ips, ipe, jps, jpe, kps, kpe    )

  k_start         = kps
  k_end           = kpe

  ijds = min(ids, jds)
  ijde = max(ide, jde)

  num_3d_m        = num_moist
  num_3d_c        = num_chem
  num_3d_s        = num_scalar

!  Compute these starting and stopping locations for each tile and number of tiles.
!  See: http://www.mmm.ucar.edu/wrf/WG2/topics/settiles
  CALL set_tiles ( grid , ids , ide , jds , jde , ips , ipe , jps , jpe )
!print *,grid%julian,grid%julday, grid%julian,grid%julday in solve

  grid%itimestep = grid%itimestep + 1

!**********************************************************************
!
!  LET US BEGIN.......
!
!<DESCRIPTION>
!<pre>
! (1) RK integration loop is named the "Runge_Kutta_loop:"
!
!   Predictor-corrector type time integration.
!   Advection terms are evaluated at time t for the predictor step,
!   and advection is re-evaluated with the latest predicted value for
!   each succeeding time corrector step
!
!   2nd order Runge Kutta (rk_order = 2):
!   Step 1 is taken to the midpoint predictor, step 2 is the full step.
!
!   3rd order Runge Kutta (rk_order = 3):
!   Step 1 is taken to from t to dt/3, step 2 is from t to dt/2,
!   and step 3 is from t to dt.
!
!   non-timesplit physics are evaluated during first RK step and
!   these physics tendencies are stored for use in each RK pass.
!</pre>
!</DESCRIPTION>
!**********************************************************************


 rk_order = config_flags%rk_ord
 IF (grid%time_step_sound == 0) THEN
! auto-set option
! This function will give 4 for 6*dx and 6 for 10*dx and returns even numbers only
   grid%time_step_sound = max ( 2 * ( INT (300.*grid%dt/grid%dx-0.01) + 1 ), 4 )
   WRITE(wrf_err_message,*)'dx, dt, time_step_sound=',grid%dx,grid%dt,grid%time_step_sound
   CALL wrf_debug ( 50 , wrf_err_message )
 ENDIF

 grid%dts = grid%dt/float(grid%time_step_sound)

 Runge_Kutta_loop:  DO rk_step = 1, rk_order

   !  Set the step size and number of small timesteps for
   !  each part of the timestep

   dtm = grid%dt
   IF ( rk_order == 1 ) THEN   

      write(wrf_err_message,*)' leapfrog removed, error exit for dynamics_option = ',dynamics_option
      CALL wrf_error_fatal3 ( "solve_em.b" , 238 ,  wrf_err_message )

   ELSE IF ( rk_order == 2 ) THEN   ! 2nd order Runge-Kutta timestep

       IF ( rk_step == 1) THEN
             dt_rk  = 0.5*grid%dt
             dts_rk = grid%dts
             number_of_small_timesteps = grid%time_step_sound/2
       ELSE
             dt_rk = grid%dt
             dts_rk = grid%dts
             number_of_small_timesteps = grid%time_step_sound
       ENDIF

   ELSE IF ( rk_order == 3 ) THEN ! third order Runge-Kutta

       IF ( rk_step == 1) THEN
            dt_rk = grid%dt/3.
            dts_rk = dt_rk
            number_of_small_timesteps = 1
       ELSE IF (rk_step == 2) THEN
            dt_rk  = 0.5*grid%dt
            dts_rk = grid%dts
            number_of_small_timesteps = grid%time_step_sound/2
       ELSE
            dt_rk = grid%dt
            dts_rk = grid%dts
            number_of_small_timesteps = grid%time_step_sound
       ENDIF

   ELSE

      write(wrf_err_message,*)' unknown solver, error exit for dynamics_option = ',dynamics_option
      CALL wrf_error_fatal3 ( "solve_em.b" , 271 ,  wrf_err_message )

   END IF

!
!  Time level t is in the *_2 variable in the first part 
!  of the step, and in the *_1 variable after the predictor.
!  the latest predicted values are stored in the *_2 variables.
!
   CALL wrf_debug ( 200 , ' call rk_step_prep ' )


   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   DO ij = 1 , grid%num_tiles

      CALL rk_step_prep  ( config_flags, rk_step,            &
                           grid%em_u_2, grid%em_v_2, grid%em_w_2, grid%em_t_2, grid%em_ph_2, grid%em_mu_2,   &
                           moist,                            &
                           grid%em_ru, grid%em_rv, grid%em_rw, grid%em_ww, grid%em_php, grid%em_alt, grid%em_muu, grid%em_muv,   &
                           grid%em_mub, grid%em_mut, grid%em_phb, grid%em_pb, grid%em_p, grid%em_al, grid%em_alb,    &
                           cqu, cqv, cqw,                    &
                           grid%msfu, grid%msfv, grid%msft,                 &
                           grid%em_fnm, grid%em_fnp, grid%em_dnw, grid%rdx, grid%rdy,          &
                           num_3d_m,                         &
                           ids, ide, jds, jde, kds, kde,     &
                           ims, ime, jms, jme, kms, kme,     &
                           grid%i_start(ij), grid%i_end(ij), &
                           grid%j_start(ij), grid%j_end(ij), &
                           k_start, k_end                   )

   END DO
   !$OMP END PARALLEL DO


!-----------------------------------------------------------------------
!  Stencils for patch communications  (WCS, 29 June 2001)
!  Note:  the small size of this halo exchange reflects the 
!         fact that we are carrying the uncoupled variables 
!         as state variables in the mass coordinate model, as
!         opposed to the coupled variables as in the height
!         coordinate model.
!
!                           * * * * *
!         *        * * *    * * * * *
!       * + *      * + *    * * + * * 
!         *        * * *    * * * * *
!                           * * * * *
!
!  3D variables - note staggering!  grid%em_ru(X), grid%em_rv(Y), grid%em_ww(grid%em_z), grid%em_php(grid%em_z)
!
!j grid%em_ru     x
!j grid%em_rv     x
!j grid%em_ww     x
!j grid%em_php    x
!j grid%em_alt    x
!j grid%em_ph_2   x
!j grid%em_phb    x
!
!  the following are 2D (xy) variables
!
!j grid%em_muu    x
!j grid%em_muv    x
!j grid%em_mut    x
!--------------------------------------------------------------
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_A.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_A.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 1, &
     8, 3, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_ru, 1, 4, 0, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_rv, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_rw, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ww, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_php, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_alt, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_al, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_p, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_muu, 1, 4, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_muv, 1, 4, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mut, 1, 4, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ru, 1, 4, 0, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_rv, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_rw, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ww, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_php, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_alt, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_al, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_p, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_muu, 1, 4, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_muv, 1, 4, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mut, 1, 4, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_INIT_EXCH ( local_communicator, 1 , &
     8, 3, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_ru, 1, 4, 1, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_rv, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_rw, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ww, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_php, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_alt, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_al, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_p, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_muu, 1, 4, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_muv, 1, 4, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mut, 1, 4, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ru, 1, 4, 1, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_rv, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_rw, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ww, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_php, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_alt, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_al, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_p, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_muu, 1, 4, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_muv, 1, 4, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mut, 1, 4, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
!ENDOFREGISTRYGENERATEDINCLUDE

! set boundary conditions on variables 
! from big_step_prep for use in big_step_proc

!STARTOFREGISTRYGENERATEDINCLUDE 'inc/PERIOD_BDY_EM_A.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_A.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 2 , &
     9, 3, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_ru, 2, 4, 1, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_rv, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_rw, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_ww, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_php, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_alt, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_p, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_muu, 2, 4, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_muv, 2, 4, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_mut, 2, 4, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_ph_2, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_al, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_ru, 2, 4, 1, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_rv, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_rw, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_ww, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_php, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_alt, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_p, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_muu, 2, 4, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_muv, 2, 4, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_mut, 2, 4, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_ph_2, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_al, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
END IF
!ENDOFREGISTRYGENERATEDINCLUDE

!   CALL set_tiles ( grid , ids , ide , jds , jde , ips-1 , ipe+1 , jps-1 , jpe+1 )


   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   DO ij = 1 , grid%num_tiles

       CALL wrf_debug ( 200 , ' call rk_phys_bc_dry_1' )

        CALL rk_phys_bc_dry_1( config_flags, grid%em_ru, grid%em_rv, grid%em_rw, grid%em_ww,      & 
                               grid%em_muu, grid%em_muv, grid%em_mut, grid%em_php, grid%em_alt, grid%em_p,        &
                               ids, ide, jds, jde, kds, kde,      &
                               ims, ime, jms, jme, kms, kme,      &
                               ips, ipe, jps, jpe, kps, kpe,      &
                               grid%i_start(ij), grid%i_end(ij),  &
                               grid%j_start(ij), grid%j_end(ij),  &
                               k_start, k_end                )
       !TBH:  need this 2nd timestep and later
       CALL set_physical_bc3d( grid%em_al, 'grid%em_p', config_flags,            &
                               ids, ide, jds, jde, kds, kde,     &
                               ims, ime, jms, jme, kms, kme,     &
                               ips, ipe, jps, jpe, kps, kpe,     &
                               grid%i_start(ij), grid%i_end(ij), &
                               grid%j_start(ij), grid%j_end(ij), &
                               k_start    , k_end               )
       CALL set_physical_bc3d( grid%em_ph_2, 'w', config_flags,            &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 ips, ipe, jps, jpe, kps, kpe, &
                               grid%i_start(ij), grid%i_end(ij),        &
                               grid%j_start(ij), grid%j_end(ij),        &
                               k_start, k_end                )

   END DO
   !$OMP END PARALLEL DO


    rk_step_is_one : IF (rk_step == 1) THEN ! only need to initialize diffusion tendencies

 ! initialize all tendencies to zero in order to update physics
 ! tendencies first (separate from dry dynamics).
 

     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )

     DO ij = 1 , grid%num_tiles

        CALL wrf_debug ( 200 , ' call init_zero_tendency' )
        CALL init_zero_tendency ( ru_tendf, rv_tendf, rw_tendf,     &
                                  ph_tendf, t_tendf, tke_tend,      &
                                  mu_tendf,                         &
                                  moist_tend,chem_tend,scalar_tend, &
                                  num_3d_m,num_3d_c,num_3d_s,       &
                                  rk_step,                          &
                                  ids, ide, jds, jde, kds, kde,     &
                                  ims, ime, jms, jme, kms, kme,     &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij), &
                                  k_start, k_end                   )

     END DO
   !$OMP END PARALLEL DO


!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_PHYS_A.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_PHYS_A.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 1, &
     2, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_2, 1, 4, 0, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_2, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_2, 1, 4, 0, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_2, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_INIT_EXCH ( local_communicator, 1 , &
     2, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_2, 1, 4, 1, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_2, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_2, 1, 4, 1, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_2, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
!ENDOFREGISTRYGENERATEDINCLUDE

!<DESCRIPTION>
!<pre>
!(2) The non-timesplit physics begins with a call to "phy_prep"
!    (which computes some diagnostic variables such as temperature,
!    pressure, u and v at grid%em_p points, etc).  This is followed by
!    calls to the physics drivers:
!
!              radiation,
!              surface,
!              pbl,
!              cumulus,
!              3D TKE and mixing.
!<pre>
!</DESCRIPTION>



      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij )
      DO ij = 1 , grid%num_tiles

         CALL wrf_debug ( 200 , ' call phy_prep' )
         CALL phy_prep ( config_flags,                           &
                         grid%em_mut, grid%em_muu, grid%em_muv, grid%em_u_2, &
                         grid%em_v_2, grid%em_p, grid%em_pb, grid%em_alt,              &
                         grid%em_ph_2, grid%em_phb, grid%em_t_2, grid%tsk, moist, num_3d_m,   &
                         mu_3d, rho,                             &
                         th_phy, p_phy, pi_phy, u_phy, v_phy,    &
                         p8w, t_phy, t8w, grid%em_z, z_at_w,             &
                         dz8w, grid%em_fnm, grid%em_fnp,                         &    
                         grid%rthraten,                               &
                         grid%rthblten, grid%rublten, grid%rvblten,             &
                         grid%rqvblten, grid%rqcblten, grid%rqiblten,           &
                         grid%rthcuten, grid%rqvcuten, grid%rqccuten,           &
                         grid%rqrcuten, grid%rqicuten, grid%rqscuten,           &
                         grid%rthften,  grid%rqvften,                      &
                         grid%RUNDGDTEN, grid%RVNDGDTEN, grid%RTHNDGDTEN,       &
                         grid%RQVNDGDTEN, grid%RMUNDGDTEN,                 &
                         ids, ide, jds, jde, kds, kde,           &
                         ims, ime, jms, jme, kms, kme,           &
                         grid%i_start(ij), grid%i_end(ij),       &
                         grid%j_start(ij), grid%j_end(ij),       &
                         k_start, k_end                         )
      ENDDO
      !$OMP END PARALLEL DO



!  physics to implement

!      CALL set_tiles ( grid , ids , ide-1 , jds , jde-1 ips , ipe , jps , jpe )

! Open MP loops are in physics drivers
! radiation

!-----------------------------------------------------------------
! urban related variable are added to arguments of radiation_driver
!-----------------------------------------------------------------

         CALL wrf_debug ( 200 , ' call radiation_driver' )


         CALL radiation_driver(                                           &
     &         ACFRCV=grid%acfrcv      ,ACFRST=grid%acfrst      ,ALBEDO=grid%albedo      &
     &        ,CFRACH=grid%cfrach      ,CFRACL=grid%cfracl      ,CFRACM=grid%cfracm      &
     &        ,CUPPT=grid%cuppt        ,CZMEAN=grid%czmean      ,DT=grid%dt              &
     &        ,DZ8W=dz8w          ,EMISS=grid%emiss        ,GLW=grid%glw            &
     &        ,GMT=grid%gmt            ,GSW=grid%gsw            ,HBOT=grid%hbot          &
     &        ,HTOP=grid%htop ,HBOTR=grid%hbotr, HTOPR=grid%htopr ,ICLOUD=config_flags%icloud &
     &        ,ITIMESTEP=grid%itimestep,JULDAY=grid%julday, JULIAN=grid%julian      &
     &        ,JULYR=grid%julyr        ,LW_PHYSICS=config_flags%ra_lw_physics  &
     &        ,NCFRCV=grid%ncfrcv      ,NCFRST=grid%ncfrst      ,NPHS=1             &
     &        ,P8W=p8w            ,P=p_phy            ,PI=pi_phy          &
     &        ,RADT=grid%radt     ,RA_CALL_OFFSET=grid%ra_call_offset     &
     &        ,RHO=rho            ,RLWTOA=grid%rlwtoa                          &
     &        ,RSWTOA=grid%rswtoa      ,RTHRATEN=grid%rthraten                      &
     &        ,RTHRATENLW=grid%rthratenlw                                      &
     &        ,RTHRATENSW=grid%rthratensw                  ,SNOW=grid%snow          &
     &        ,STEPRA=grid%stepra      ,SWDOWN=grid%swdown      ,SWDOWNC=grid%swdownc    &
     &        ,SW_PHYSICS=config_flags%ra_sw_physics  ,T8W=t8w            &
     &        ,T=t_phy            ,TAUCLDC=grid%taucldc    ,TAUCLDI=grid%taucldi    &
     &        ,TSK=grid%tsk            ,VEGFRA=grid%vegfra     ,WARM_RAIN=grid%warm_rain &
     &        ,XICE=grid%xice                                                  &
     &        ,XLAND=grid%xland        ,XLAT=grid%xlat          ,XLONG=grid%xlong        &
!Optional urban
     &        ,DECLIN_URB=grid%declin_urb        ,COSZ_URB2D=grid%cosz_urb2d        &
     &        ,OMG_URB2D=grid%omg_urb2d                                        &
!
     &        ,Z=grid%em_z                                                        &
     &        ,LEVSIZ=grid%levsiz, N_OZMIXM=num_ozmixm                    &
     &        ,N_AEROSOLC=num_aerosolc                                    &
     &        ,PAERLEV=grid%paerlev                                       &
     &        ,CAM_ABS_DIM1=grid%cam_abs_dim1, CAM_ABS_DIM2=grid%cam_abs_dim2 &
     &        ,CAM_ABS_FREQ_S=grid%cam_abs_freq_s                         &
     &        ,XTIME=grid%xtime                                                &
            ! indexes
     &        ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde          &
     &        ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme          &
     &        ,i_start=grid%i_start,i_end=min(grid%i_end, ide-1)          &
     &        ,j_start=grid%j_start,j_end=min(grid%j_end, jde-1)          &
     &        ,kts=k_start, kte=min(k_end,kde-1)                          &
     &        ,num_tiles=grid%num_tiles                                   &
            ! Optional                          
     &        , CLDFRA=grid%cldfra                                             &
     &        , PB=grid%em_pb                                                     &
     &        , F_ICE_PHY=grid%f_ice_phy,F_RAIN_PHY=grid%f_rain_phy                 &
     &        , QV=moist(ims,kms,jms,P_QV), F_QV=F_QV                     &
     &        , QC=moist(ims,kms,jms,P_QC), F_QC=F_QC                                        &
     &        , QR=moist(ims,kms,jms,P_QR), F_QR=F_QR                     &
     &        , QI=moist(ims,kms,jms,P_QI), F_QI=F_QI                                        &
     &        , QS=moist(ims,kms,jms,P_QS), F_QS=F_QS                     &
     &        , QG=moist(ims,kms,jms,P_QG), F_QG=F_QG                     &
     &        ,LWCF=grid%lwcf                                                  &
     &        ,SWCF=grid%swcf                                                  &
     &        ,OLR=grid%olr                                                    &
     &        ,OZMIXM=grid%ozmixm, PIN=grid%pin                                     &
     &        ,M_PS_1=grid%m_ps_1, M_PS_2=grid%m_ps_2, AEROSOLC_1=grid%aerosolc_1        &
     &        ,AEROSOLC_2=grid%aerosolc_2, M_HYBI0=grid%m_hybi                      &
     &        ,ABSTOT=grid%abstot, ABSNXT=grid%absnxt, EMSTOT=grid%emstot                &
     &                                                              )



!********* Surface driver
! surface



!-----------------------------------------------------------------
! urban related variable are added to arguments of surface_driver
!-----------------------------------------------------------------
      NUM_ROOF_LAYERS = grid%num_soil_layers !urban
      NUM_WALL_LAYERS = grid%num_soil_layers !urban
      NUM_ROAD_LAYERS = grid%num_soil_layers !urban

      CALL wrf_debug ( 200 , ' call surface_driver' )
      CALL surface_driver(                                                &
     &         ACSNOM=grid%acsnom      ,ACSNOW=grid%acsnow      ,AKHS=grid%akhs          &
     &        ,AKMS=grid%akms          ,ALBBCK=grid%albbck      ,ALBEDO=grid%albedo      &
     &        ,BR=br              ,CANWAT=grid%canwat      ,CHKLOWQ=chklowq    &
     &        ,CT=grid%ct              ,DT=grid%dt         ,DX=grid%dx         &
     &        ,DZ8W=dz8w          ,DZS=grid%dzs            ,FLHC=grid%flhc          &
     &        ,FLQC=grid%flqc          ,GLW=grid%glw            ,GRDFLX=grid%grdflx      &
     &        ,GSW=grid%gsw    ,SWDOWN=grid%swdown        ,GZ1OZ0=gz1oz0      ,HFX=grid%hfx              &
     &        ,HT=grid%ht              ,IFSNOW=config_flags%ifsnow      ,ISFFLX=config_flags%isfflx      &
     &        ,ISLTYP=grid%isltyp      ,ITIMESTEP=grid%itimestep                    &
     &        ,IVGTYP=grid%ivgtyp      ,LH=grid%lh              ,LOWLYR=grid%lowlyr      &
     &        ,MAVAIL=grid%mavail      ,NUM_SOIL_LAYERS=config_flags%num_soil_layers        &
     &        ,P8W=p8w            ,PBLH=grid%pblh          ,PI_PHY=pi_phy      &
     &        ,PSFC=grid%psfc          ,PSHLTR=pshltr      ,PSIH=psih          &
     &        ,PSIM=psim          ,P_PHY=p_phy        ,Q10=q10            &
     &        ,Q2=grid%q2              ,QFX=grid%qfx            ,QSFC=grid%qsfc          &
     &        ,QSHLTR=qshltr      ,QZ0=grid%qz0            ,RAINCV=grid%raincv      &
     &        ,RA_LW_PHYSICS=config_flags%ra_lw_physics            ,RHO=rho            &
     &        ,RMOL=grid%rmol          ,SFCEVP=grid%sfcevp      ,SFCEXC=grid%sfcexc      &
     &        ,SFCRUNOFF=grid%sfcrunoff                                        &
     &        ,SF_SFCLAY_PHYSICS=config_flags%sf_sfclay_physics                        &
     &        ,SF_SURFACE_PHYSICS=config_flags%sf_surface_physics  ,SH2O=grid%sh2o          &
     &        ,SHDMAX=grid%shdmax      ,SHDMIN=grid%shdmin      ,SMOIS=grid%smois        &
     &        ,SMSTAV=grid%smstav      ,SMSTOT=grid%smstot      ,SNOALB=grid%snoalb      &
     &        ,SNOW=grid%snow          ,SNOWC=grid%snowc        ,SNOWH=grid%snowh        &
     &        ,SST=grid%sst            ,SST_UPDATE=grid%sst_update                  &
     &        ,STEPBL=grid%stepbl      ,TH10=th10          ,TH2=grid%th2            &
     &        ,THZ0=grid%thz0          ,TH_PHY=th_phy      ,TKE_MYJ=grid%tke_myj    &
     &        ,TMN=grid%tmn            ,TSHLTR=tshltr      ,TSK=grid%tsk            &
     &        ,TSLB=grid%tslb          ,T_PHY=t_phy        ,U10=grid%u10            &
     &        ,URATX=grid%uratx        ,VRATX=grid%vratx   ,TRATX=grid%tratx        &
     &        ,UDRUNOFF=grid%udrunoff  ,UST=grid%ust       ,UZ0=grid%uz0            &
     &        ,U_FRAME=grid%u_frame    ,U_PHY=u_phy        ,V10=grid%v10            &
     &        ,VEGFRA=grid%vegfra      ,VZ0=grid%vz0       ,V_FRAME=grid%v_frame    &
     &        ,V_PHY=v_phy             ,WARM_RAIN=grid%warm_rain                    &
     &        ,WSPD=wspd               ,XICE=grid%xice     ,XLAND=grid%xland        &
     &        ,Z0=grid%z0              ,Z=grid%em_z        ,ZNT=grid%znt            &
     &        ,ZS=grid%zs                                                           &
     &        ,DECLIN_URB=grid%declin_urb  ,COSZ_URB2D=grid%cosz_urb2d    & !I urban
     &        ,OMG_URB2D=grid%omg_urb2d    ,xlat_urb2d=grid%XLAT          & !I urban
     &        ,NUM_ROOF_LAYERS=num_roof_layers                            & !I urban
     &        ,NUM_WALL_LAYERS=num_wall_layers                            & !I urban
     &        ,NUM_ROAD_LAYERS=num_road_layers                            &
     &        ,DZR=grid%dzr ,DZB=grid%dzb ,DZG=grid%dzg                   & !I urban
     &        ,TR_URB2D=grid%tr_urb2d ,TB_URB2D=grid%tb_urb2d             &
     &        ,TG_URB2D=grid%tg_urb2d                                     & !H urban
     &        ,TC_URB2D=grid%tc_urb2d ,QC_URB2D=grid%qc_urb2d             & !H urban
     &        ,UC_URB2D=grid%uc_urb2d                                     & !H urban
     &        ,XXXR_URB2D=grid%xxxr_urb2d                                 &
     &        ,XXXB_URB2D=grid%xxxb_urb2d                                 & !H urban
     &        ,XXXG_URB2D=grid%xxxg_urb2d                                 &
     &        ,XXXC_URB2D=grid%xxxc_urb2d                                 & !H urban
     &        ,TRL_URB3D=grid%trl_urb3d   ,TBL_URB3D=grid%tbl_urb3d       & !H urban
     &        ,TGL_URB3D=grid%tgl_urb3d                                   & !H urban
     &        ,SH_URB2D=grid%sh_urb2d     ,LH_URB2D=grid%lh_urb2d         &
     &        ,G_URB2D=grid%g_urb2d                                       & !H urban
     &        ,RN_URB2D=grid%rn_urb2d     , TS_URB2D=grid%ts_urb2d        & !H urban 
     &        ,FRC_URB2D=grid%frc_urb2d                                   & !H urban
     &        ,UTYPE_URB2D=grid%utype_urb2d                               & !H urban
     &        ,ucmcall=grid%ucmcall                                       & !H urban
           ! Indexes
     &        ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde          &
     &        ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme          &
     &        , I_START=grid%i_start,I_END=min(grid%i_end, ide-1)         &
     &        , J_START=grid%j_start,J_END=min(grid%j_end, jde-1)         &
     &        , KTS=k_start, KTE=min(k_end,kde-1)                         &
     &        , NUM_TILES=grid%num_tiles                                  &
           ! Optional
     &        ,QV_CURR=moist(ims,kms,jms,P_QV), F_QV=F_QV                 &
     &        ,QC_CURR=moist(ims,kms,jms,P_QC), F_QC=F_QC                 &
     &        ,QR_CURR=moist(ims,kms,jms,P_QR), F_QR=F_QR                 &
     &        ,QI_CURR=moist(ims,kms,jms,P_QI), F_QI=F_QI                 &
     &        ,QS_CURR=moist(ims,kms,jms,P_QS), F_QS=F_QS                 &
     &        ,QG_CURR=moist(ims,kms,jms,P_QG), F_QG=F_QG                 &
     &        ,CAPG=grid%capg, EMISS=grid%emiss, HOL=hol,MOL=grid%mol                    &
     &        ,RAINBL=grid%rainbl,SR=grid%em_sr                                              &
     &        ,RAINNCV=grid%rainncv,REGIME=regime,T2=grid%t2,THC=grid%thc                &
     &        ,QSG=grid%qsg,QVG=grid%qvg,QCG=grid%qcg,SOILT1=grid%soilt1,TSNAV=grid%tsnav          & ! ruc lsm
     &        ,SMFR3D=grid%smfr3d,KEEPFR3DFLAG=grid%keepfr3dflag                    & ! ruc lsm
     &        ,POTEVP=grid%em_POTEVP, SNOPCX=grid%em_SNOPCX, SOILTB=grid%em_SOILTB                & ! ruc lsm
     &                                                              )


!*********
! pbl

      CALL wrf_debug ( 200 , ' call pbl_driver' )

      CALL pbl_driver(                                                    &
     &         AKHS=grid%akhs          ,AKMS=grid%akms                              &
     &        ,BL_PBL_PHYSICS=config_flags%bl_pbl_physics                 &
     &        ,BR=br              ,CHKLOWQ=chklowq    ,CT=grid%ct              &
     &        ,DT=grid%dt              ,DX=grid%dx              ,DZ8W=dz8w          &
     &        ,EL_MYJ=grid%el_myj      ,EXCH_H=grid%exch_h      ,GRDFLX=grid%grdflx      &
     &        ,GZ1OZ0=gz1oz0      ,HFX=grid%hfx            ,HT=grid%ht              &
     &        ,ITIMESTEP=grid%itimestep                    ,KPBL=grid%kpbl          &
     &        ,LH=grid%lh              ,LOWLYR=grid%lowlyr      ,P8W=p8w            &
     &        ,PBLH=grid%pblh          ,PI_PHY=pi_phy      ,PSIH=psih          &
     &        ,PSIM=psim          ,P_PHY=p_phy        ,QFX=grid%qfx            &
     &        ,QSFC=grid%qsfc          ,QZ0=grid%qz0                                &
     &        ,RA_LW_PHYSICS=config_flags%ra_lw_physics                   &
     &        ,RHO=rho            ,RQCBLTEN=grid%rqcblten  ,RQIBLTEN=grid%rqiblten  &
     &        ,RQVBLTEN=grid%rqvblten  ,RTHBLTEN=grid%rthblten  ,RUBLTEN=grid%rublten    &
     &        ,RVBLTEN=grid%rvblten    ,SNOW=grid%snow          ,STEPBL=grid%stepbl      &
     &        ,THZ0=grid%thz0          ,TH_PHY=th_phy      ,TKE_MYJ=grid%tke_myj    &
     &        ,TSK=grid%tsk            ,T_PHY=t_phy        ,UST=grid%ust            &
     &        ,UZ0=grid%uz0            ,U_FRAME=grid%u_frame    ,U_PHY=u_phy        &
     &        ,VZ0=grid%vz0            ,V_FRAME=grid%v_frame    ,V_PHY=v_phy        &
     &        ,WARM_RAIN=grid%warm_rain                    ,WSPD=wspd          &
     &        ,XICE=grid%xice          ,XLAND=grid%xland        ,Z=grid%em_z                &
     &        ,ZNT=grid%znt                                                    &
     &        ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde          &
     &        ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme          &
     &        ,I_START=grid%i_start,I_END=min(grid%i_end, ide-1)          &
     &        ,J_START=grid%j_start,J_END=min(grid%j_end, jde-1)          &
     &        ,KTS=k_start, KTE=min(k_end,kde-1)                          &
     &        ,NUM_TILES=grid%num_tiles                                   &
          ! optional
     &        ,QV_CURR=moist(ims,kms,jms,P_QV), F_QV=F_QV                 &
     &        ,QC_CURR=moist(ims,kms,jms,P_QC), F_QC=F_QC                 &
     &        ,QR_CURR=moist(ims,kms,jms,P_QR), F_QR=F_QR                 &
     &        ,QI_CURR=moist(ims,kms,jms,P_QI), F_QI=F_QI                 &
     &        ,QS_CURR=moist(ims,kms,jms,P_QS), F_QS=F_QS                 &
     &        ,QG_CURR=moist(ims,kms,jms,P_QG), F_QG=F_QG                 &
     &        ,HOL=HOL, MOL=grid%mol, REGIME=REGIME                            &
     &                                                          )



! cumulus para.

          CALL wrf_debug ( 200 , ' call cumulus_driver' )


         CALL cumulus_driver(                                             &
                 ! Prognostic variables
     &              U=u_phy   ,V=v_phy   ,TH=th_phy  ,T=t_phy             &
     &             ,W=grid%em_w_2     ,P=p_phy   ,PI=pi_phy  ,RHO=rho             &
                 ! Other arguments
     &             ,ITIMESTEP=grid%itimestep ,DT=grid%dt      ,DX=grid%dx                &
     &             ,RAINC=grid%rainc   ,RAINCV=grid%raincv   ,NCA=grid%nca               &
     &             ,HTOP=grid%cutop    ,HBOT=grid%cubot      ,KPBL=grid%kpbl             &
     &             ,DZ8W=dz8w     ,P8W=p8w                                &
     &             ,W0AVG=grid%w0avg   ,STEPCU=grid%stepcu                          &
     &             ,CLDEFI=grid%cldefi ,LOWLYR=grid%lowlyr ,XLAND=grid%xland             &
     &             ,APR_GR=grid%apr_gr ,APR_W=grid%apr_w   ,APR_MC=grid%apr_mc           &
     &             ,APR_ST=grid%apr_st ,APR_AS=grid%apr_as ,APR_CAPMA=grid%apr_capma     &
     &             ,APR_CAPME=grid%apr_capme          ,APR_CAPMI=grid%apr_capmi     &
     &             ,MASS_FLUX=grid%mass_flux          ,XF_ENS=grid%xf_ens           &
     &             ,PR_ENS=grid%pr_ens ,HT=grid%ht                                  &
     &             ,ENSDIM=config_flags%ensdim ,MAXIENS=config_flags%maxiens ,MAXENS=config_flags%maxens         &
     &             ,MAXENS2=config_flags%maxens2                ,MAXENS3=config_flags%maxens3       &
     &             ,CU_ACT_FLAG=cu_act_flag   ,WARM_RAIN=grid%warm_rain        &
     &             ,GSW=grid%gsw                                               &
                 ! Selection flag
     &             ,CU_PHYSICS=config_flags%cu_physics                    &
                 ! Dimension arguments
     &             ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde     &
     &             ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme     &
     &             ,I_START=grid%i_start,I_END=min(grid%i_end, ide-1)     &
     &             ,J_START=grid%j_start,J_END=min(grid%j_end, jde-1)     &
     &             ,KTS=k_start, KTE=min(k_end,kde-1)                     &
     &             ,NUM_TILES=grid%num_tiles                              &
                 ! Moisture tendency arguments
     &             ,RQVCUTEN=grid%rqvcuten , RQCCUTEN=grid%rqccuten                 &
     &             ,RQSCUTEN=grid%rqscuten , RQICUTEN=grid%rqicuten                 &
     &             ,RQRCUTEN=grid%rqrcuten , RQVBLTEN=grid%rqvblten                 &
     &             ,RQVFTEN=grid%rqvften                                       &
                 ! Other tendency arguments
     &             ,RTHRATEN=grid%rthraten , RTHBLTEN=grid%rthblten                 &
     &             ,RTHCUTEN=grid%rthcuten , RTHFTEN=grid%rthften                   &
                 ! Moisture tracer arguments
     &             ,QV_CURR=moist(ims,kms,jms,P_QV), F_QV=F_QV            &
     &             ,QC_CURR=moist(ims,kms,jms,P_QC), F_QC=F_QC            &
     &             ,QR_CURR=moist(ims,kms,jms,P_QR), F_QR=F_QR            &
     &             ,QI_CURR=moist(ims,kms,jms,P_QI), F_QI=F_QI            &
     &             ,QS_CURR=moist(ims,kms,jms,P_QS), F_QS=F_QS            &
     &             ,QG_CURR=moist(ims,kms,jms,P_QG), F_QG=F_QG            &
     &                                                          )


! fdda

          CALL wrf_debug ( 200 , ' call fddagd_driver' )


   CALL fddagd_driver(itimestep=grid%itimestep,dt=grid%dt,xtime=grid%XTIME,         &
                  id=grid%id,      &
                  RUNDGDTEN=grid%rundgdten,RVNDGDTEN=grid%rvndgdten,                &
                  RTHNDGDTEN=grid%rthndgdten,RQVNDGDTEN=grid%rqvndgdten,            &
                  RMUNDGDTEN=grid%rmundgdten,                                  &
                  u_ndg_old=fdda3d(ims,kms,jms,P_u_ndg_old),              &
                  v_ndg_old=fdda3d(ims,kms,jms,P_v_ndg_old),              &
                  t_ndg_old=fdda3d(ims,kms,jms,P_t_ndg_old),              &
                  q_ndg_old=fdda3d(ims,kms,jms,P_q_ndg_old),              &
                  mu_ndg_old=fdda2d(ims,1,jms,P_mu_ndg_old),              &
                  u_ndg_new=fdda3d(ims,kms,jms,P_u_ndg_new),              &
                  v_ndg_new=fdda3d(ims,kms,jms,P_v_ndg_new),              &
                  t_ndg_new=fdda3d(ims,kms,jms,P_t_ndg_new),              &
                  q_ndg_new=fdda3d(ims,kms,jms,P_q_ndg_new),              &
                  mu_ndg_new=fdda2d(ims,1,jms,P_mu_ndg_new),              &
                  u3d=grid%em_u_2,v3d=grid%em_v_2,th_phy=th_phy,rho=rho,moist=moist,      &
                  p_phy=p_phy,pi_phy=pi_phy,p8w=p8w,t_phy=t_phy,          &
                  dz8w=dz8w,z=grid%em_z,z_at_w=z_at_w,                            &
                  config_flags=config_flags,dx=grid%DX,n_moist=num_3d_m,  &
                  STEPFG=grid%STEPFG,                                          &
                  pblh=grid%pblh,ht=grid%ht,                                        &
                    IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde     &
                   ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme     &
                   ,I_START=grid%i_start,I_END=min(grid%i_end, ide-1)     &
                   ,J_START=grid%j_start,J_END=min(grid%j_end, jde-1)     &
                   ,KTS=k_start, KTE=min(k_end,kde-1)                     &
                   , num_tiles=grid%num_tiles                             )


! calculate_phy_tend


      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij )

      DO ij = 1 , grid%num_tiles

          CALL wrf_debug ( 200 , ' call calculate_phy_tend' )
          CALL calculate_phy_tend (config_flags,grid%em_mut,grid%em_muu,grid%em_muv,pi_phy,            &
                     grid%rthraten,                                         &
                     grid%rublten,grid%rvblten,grid%rthblten,                         &
                     grid%rqvblten,grid%rqcblten,grid%rqiblten,                       &
                     grid%rthcuten,grid%rqvcuten,grid%rqccuten,grid%rqrcuten,              &
                     grid%rqicuten,grid%rqscuten,                                &
                     grid%RUNDGDTEN,grid%RVNDGDTEN,grid%RTHNDGDTEN,grid%RQVNDGDTEN,        &
                     grid%RMUNDGDTEN,                                       &
                     ids,ide, jds,jde, kds,kde,                        &
                     ims,ime, jms,jme, kms,kme,                        &
                     grid%i_start(ij), min(grid%i_end(ij),ide-1),      &
                     grid%j_start(ij), min(grid%j_end(ij),jde-1),      &
                     k_start    , min(k_end,kde-1)                     )

      ENDDO
      !$OMP END PARALLEL DO


! tke diffusion

     IF(config_flags%diff_opt .eq. 2 .OR. config_flags%diff_opt .eq. 1) THEN


       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )

       DO ij = 1 , grid%num_tiles

          CALL wrf_debug ( 200 , ' call compute_diff_metrics ' )
          CALL compute_diff_metrics ( config_flags, grid%em_ph_2, grid%em_phb, grid%em_z, grid%em_rdz, grid%em_rdzw, &
                                      grid%em_zx, grid%em_zy, grid%rdx, grid%rdy,                      &
                                      ids, ide, jds, jde, kds, kde,          &
                                      ims, ime, jms, jme, kms, kme,          &
                                      grid%i_start(ij), grid%i_end(ij),      &
                                      grid%j_start(ij), grid%j_end(ij),      &
                                      k_start    , k_end                    )
       ENDDO
       !$OMP END PARALLEL DO


!STARTOFREGISTRYGENERATEDINCLUDE 'inc/PERIOD_BDY_EM_A1.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_A1.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 3 , &
     5, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_rdzw, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_rdz, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_z, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_zx, 3, 4, 1, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_zy, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_rdzw, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_rdz, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_z, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_zx, 3, 4, 1, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_zy, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
END IF
!ENDOFREGISTRYGENERATEDINCLUDE


       DO ij = 1 , grid%num_tiles

          CALL wrf_debug ( 200 , ' call bc for diffusion_metrics ' )
          CALL set_physical_bc3d( grid%em_rdzw , 'w', config_flags,           &
                                  ids, ide, jds, jde, kds, kde,       &
                                  ims, ime, jms, jme, kms, kme,       &
                                  ips, ipe, jps, jpe, kps, kpe,       &
                                  grid%i_start(ij), grid%i_end(ij),   &
                                  grid%j_start(ij), grid%j_end(ij),   &
                                  k_start    , k_end                 )
          CALL set_physical_bc3d( grid%em_rdz , 'w', config_flags,            &
                                  ids, ide, jds, jde, kds, kde,       &
                                  ims, ime, jms, jme, kms, kme,       &
                                  ips, ipe, jps, jpe, kps, kpe,       &
                                  grid%i_start(ij), grid%i_end(ij),   &
                                  grid%j_start(ij), grid%j_end(ij),   &
                                  k_start    , k_end                 )
          CALL set_physical_bc3d( grid%em_z , 'w', config_flags,              &
                                  ids, ide, jds, jde, kds, kde,       &
                                  ims, ime, jms, jme, kms, kme,       &
                                  ips, ipe, jps, jpe, kps, kpe,       &
                                  grid%i_start(ij), grid%i_end(ij),   &
                                  grid%j_start(ij), grid%j_end(ij),   &
                                  k_start    , k_end                 )
          CALL set_physical_bc3d( grid%em_zx , 'w', config_flags,             &
                                  ids, ide, jds, jde, kds, kde,       &
                                  ims, ime, jms, jme, kms, kme,       &
                                  ips, ipe, jps, jpe, kps, kpe,       &
                                  grid%i_start(ij), grid%i_end(ij),   &
                                  grid%j_start(ij), grid%j_end(ij),   &
                                  k_start    , k_end                 )
          CALL set_physical_bc3d( grid%em_zy , 'w', config_flags,             &
                                  ids, ide, jds, jde, kds, kde,       &
                                  ims, ime, jms, jme, kms, kme,       &
                                  ips, ipe, jps, jpe, kps, kpe,       &
                                  grid%i_start(ij), grid%i_end(ij),   &
                                  grid%j_start(ij), grid%j_end(ij),   &
                                  k_start    , k_end                 )

       ENDDO


!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_TKE_C.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_TKE_C.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 1, &
     7, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_2, 1, 4, 0, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_2, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_z, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_zx, 1, 4, 0, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_zy, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_rdz, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_rdzw, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_2, 1, 4, 0, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_2, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_z, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_zx, 1, 4, 0, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_zy, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_rdz, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_rdzw, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_INIT_EXCH ( local_communicator, 1 , &
     7, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_2, 1, 4, 1, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_2, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_z, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_zx, 1, 4, 1, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_zy, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_rdz, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_rdzw, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_2, 1, 4, 1, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_2, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_z, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_zx, 1, 4, 1, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_zy, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_rdz, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_rdzw, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
!ENDOFREGISTRYGENERATEDINCLUDE



       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )

       DO ij = 1 , grid%num_tiles

          CALL wrf_debug ( 200 , ' call cal_deform_and_div' )
          CALL cal_deform_and_div ( config_flags,grid%em_u_2,grid%em_v_2,grid%em_w_2,grid%div,        &
                                    grid%defor11,grid%defor22,grid%defor33,grid%defor12,     &
                                    grid%defor13,grid%defor23,                     &
                                    grid%u_base, grid%v_base,grid%msfu,grid%msfv,grid%msft,       &
                                    grid%rdx, grid%rdy, grid%em_dn, grid%em_dnw, grid%em_rdz, grid%em_rdzw,        &
                                    grid%em_fnm,grid%em_fnp,grid%cf1,grid%cf2,grid%cf3,grid%em_zx,grid%em_zy,           &
                                    ids, ide, jds, jde, kds, kde,        &
                                    ims, ime, jms, jme, kms, kme,        &
                                    grid%i_start(ij), grid%i_end(ij),    &
                                    grid%j_start(ij), grid%j_end(ij),    &
                                    k_start    , k_end                  )
       ENDDO
       !$OMP END PARALLEL DO



!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_TKE_D.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_TKE_D.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 1, &
     7, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%defor11, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor22, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor33, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor12, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor13, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor23, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%div, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%defor11, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor22, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor33, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor12, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor13, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor23, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%div, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_INIT_EXCH ( local_communicator, 1 , &
     7, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%defor11, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor22, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor33, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor12, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor13, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor23, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%div, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%defor11, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor22, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor33, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor12, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor13, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor23, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%div, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
!ENDOFREGISTRYGENERATEDINCLUDE


! calculate tke, kmh, and kmv


       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )

       DO ij = 1 , grid%num_tiles

          CALL wrf_debug ( 200 , ' call calculate_km_kh' )
          CALL calculate_km_kh( config_flags,grid%dt,grid%dampcoef,grid%zdamp,config_flags%damp_opt,     &
                                grid%xkmh,grid%xkmhd,grid%xkmv,grid%xkhh,grid%xkhv,grid%bn2,               &
                                grid%khdif,grid%kvdif,grid%div,                             &
                                grid%defor11,grid%defor22,grid%defor33,grid%defor12,             &
                                grid%defor13,grid%defor23,                             &
                                grid%em_tke_2,p8w,t8w,th_phy,           &
                                t_phy,p_phy,moist,grid%em_dn,grid%em_dnw,                    &
                                grid%dx,grid%dy,grid%em_rdz,grid%em_rdzw,config_flags%mix_cr_len,num_3d_m,          &
                                grid%cf1, grid%cf2, grid%cf3, grid%warm_rain,                    &
                                grid%kh_tke_upper_bound, grid%kv_tke_upper_bound,      &
                                ids,ide, jds,jde, kds,kde,                   &
                                ims,ime, jms,jme, kms,kme,                   &
                                grid%i_start(ij), grid%i_end(ij),            &
                                grid%j_start(ij), grid%j_end(ij),            &
                                k_start    , k_end                          )
       ENDDO
       !$OMP END PARALLEL DO


!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_TKE_E.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_TKE_E.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 1, &
     6  &
   + num_moist   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%xkmv, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkmh, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkmhd, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkhv, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkhh, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%bn2, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%xkmv, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkmh, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkmhd, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkhv, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkhh, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%bn2, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_INIT_EXCH ( local_communicator, 1 , &
     6  &
   + num_moist   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%xkmv, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkmh, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkmhd, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkhv, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkhh, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%bn2, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%xkmv, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkmh, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkmhd, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkhv, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkhh, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%bn2, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE

     ENDIF

!STARTOFREGISTRYGENERATEDINCLUDE 'inc/PERIOD_BDY_EM_PHY_BC.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_PHY_BC.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 2 , &
     15, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%rublten, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%rvblten, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%xkmh, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%xkmhd, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%xkmv, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%xkhh, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%xkhv, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%div, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%defor11, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%defor22, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%defor12, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%defor13, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%defor23, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%defor33, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_tke_2, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%rublten, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%rvblten, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%xkmh, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%xkmhd, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%xkmv, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%xkhh, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%xkhv, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%div, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%defor11, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%defor22, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%defor12, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%defor13, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%defor23, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%defor33, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_tke_2, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
END IF
!ENDOFREGISTRYGENERATEDINCLUDE
      IF ( config_flags%grid_fdda .eq. 1) THEN
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/PERIOD_BDY_EM_FDDA_BC.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_FDDA_BC.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 2 , &
     2, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%rundgdten, 2, 4, 1, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%rvndgdten, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%rundgdten, 2, 4, 1, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%rvndgdten, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
END IF
!ENDOFREGISTRYGENERATEDINCLUDE
      ENDIF
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/PERIOD_BDY_EM_CHEM.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_CHEM.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 3 , &
     0  &
   + num_chem   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
END IF
!ENDOFREGISTRYGENERATEDINCLUDE


     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )

     DO ij = 1 , grid%num_tiles

       CALL wrf_debug ( 200 , ' call phy_bc' )
       CALL phy_bc (config_flags,grid%div,grid%defor11,grid%defor22,grid%defor33,            &
                            grid%defor12,grid%defor13,grid%defor23,                     &
                            grid%xkmh,grid%xkmhd,grid%xkmv,grid%xkhh,grid%xkhv,                   &
                            grid%em_tke_2,                          &
                            grid%rublten, grid%rvblten,                            &
                            ids, ide, jds, jde, kds, kde,                &
                            ims, ime, jms, jme, kms, kme,                &
                            ips, ipe, jps, jpe, kps, kpe,                &
                            grid%i_start(ij), grid%i_end(ij),            &
                            grid%j_start(ij), grid%j_end(ij),            &
                            k_start    , k_end                           )
     ENDDO
     !$OMP END PARALLEL DO


!-----------------------------------------------------------------------
!
! MPP for some physics tendency, km, kh, deformation, and divergence
!
!               *                     *
!             * + *      * + *        +
!               *                     *
!
! (for PBL)
! grid%rublten                  x
! grid%rvblten                             x
!
! (for diff_opt >= 1)
! grid%defor11                  x
! grid%defor22                             x
! grid%defor12       x
! grid%defor13                  x
! grid%defor23                             x
! grid%div           x
! grid%xkmv          x
! grid%xkmh          x
! grid%xkmhd         x
! grid%xkhv          x
! grid%xkhh          x
! tke           x
!
!-----------------------------------------------------------------------
      IF ( config_flags%bl_pbl_physics .ge. 1 ) THEN
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_PHYS_PBL.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_PHYS_PBL.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 1, &
     2, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%rublten, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rvblten, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%rublten, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rvblten, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_INIT_EXCH ( local_communicator, 1 , &
     2, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%rublten, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rvblten, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%rublten, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rvblten, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
!ENDOFREGISTRYGENERATEDINCLUDE
      ENDIF
      IF ( config_flags%grid_fdda .eq. 1) THEN
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_FDDA.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_FDDA.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 1, &
     2, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%rundgdten, 1, 4, 0, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rvndgdten, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%rundgdten, 1, 4, 0, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rvndgdten, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_INIT_EXCH ( local_communicator, 1 , &
     2, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%rundgdten, 1, 4, 1, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rvndgdten, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%rundgdten, 1, 4, 1, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rvndgdten, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
!ENDOFREGISTRYGENERATEDINCLUDE
      ENDIF
      IF ( config_flags%diff_opt .ge. 1 ) THEN
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_PHYS_DIFFUSION.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_PHYS_DIFFUSION.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 1, &
     13, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%defor11, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor22, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor12, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor13, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor23, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%div, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkmv, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkmh, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkmhd, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkhv, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkhh, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%defor11, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor22, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor12, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor13, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor23, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%div, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkmv, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkmh, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkmhd, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkhv, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkhh, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_INIT_EXCH ( local_communicator, 1 , &
     13, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%defor11, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor22, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor12, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor13, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor23, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%div, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkmv, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkmh, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkmhd, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkhv, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkhh, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%defor11, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor22, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor12, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor13, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor23, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%div, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkmv, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkmh, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkmhd, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkhv, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkhh, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
!ENDOFREGISTRYGENERATEDINCLUDE
      ENDIF

      IF      ( config_flags%h_mom_adv_order <= 4 ) THEN
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_TKE_3.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_TKE_3.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 2, &
     2, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_INIT_EXCH ( local_communicator, 2 , &
     2, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
!ENDOFREGISTRYGENERATEDINCLUDE
      ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_TKE_5.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_TKE_5.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3, &
     2, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3 , &
     2, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
!ENDOFREGISTRYGENERATEDINCLUDE
      ELSE
        WRITE(wrf_err_message,*)'solve_em: invalid h_mom_adv_order = ',config_flags%h_mom_adv_order
        CALL wrf_error_fatal3 ( "solve_em.b" , 1036 , TRIM(wrf_err_message))
      ENDIF


      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij )

      DO ij = 1 , grid%num_tiles

          CALL wrf_debug ( 200 , ' call update_phy_ten' )
        CALL update_phy_ten(t_tendf, ru_tendf, rv_tendf,moist_tend,        &
                          mu_tendf,                                        &
                          grid%rthraten,grid%rthblten,grid%rthcuten,grid%rublten,grid%rvblten,      &
                          grid%rqvblten,grid%rqcblten,grid%rqiblten,                      &
                          grid%rqvcuten,grid%rqccuten,grid%rqrcuten,grid%rqicuten,grid%rqscuten,    &
                          grid%RUNDGDTEN,grid%RVNDGDTEN,grid%RTHNDGDTEN,grid%RQVNDGDTEN,       &
                          grid%RMUNDGDTEN,                                      &
                          num_3d_m,config_flags,rk_step,                   &
                          ids, ide, jds, jde, kds, kde,                    &
                          ims, ime, jms, jme, kms, kme,                    &
                          grid%i_start(ij), grid%i_end(ij),                &
                          grid%j_start(ij), grid%j_end(ij),                &
                          k_start, k_end                               )

      END DO
      !$OMP END PARALLEL DO


     IF( config_flags%diff_opt .eq. 2 .and. config_flags%km_opt .eq. 2 ) THEN


       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )

       DO ij = 1 , grid%num_tiles

          CALL tke_rhs  ( tke_tend,grid%bn2,                               &
                          config_flags,grid%defor11,grid%defor22,grid%defor33,       &
                          grid%defor12,grid%defor13,grid%defor23,grid%em_u_2,grid%em_v_2,grid%em_w_2,grid%div,    &
                          grid%em_tke_2,grid%em_mut,                     &
                          th_phy,p_phy,p8w,t8w,grid%em_z,grid%em_fnm,grid%em_fnp,             &
                          grid%cf1,grid%cf2,grid%cf3,grid%msft,grid%xkmh,grid%xkmv,grid%xkhv,grid%rdx,grid%rdy,    &
                          grid%dx,grid%dy,grid%dt,grid%em_zx,grid%em_zy,grid%em_rdz,grid%em_rdzw,grid%em_dn,       &
                          grid%em_dnw,config_flags%mix_cr_len,  &
                          ids, ide, jds, jde, kds, kde,               &
                          ims, ime, jms, jme, kms, kme,               &
                          grid%i_start(ij), grid%i_end(ij),           &
                          grid%j_start(ij), grid%j_end(ij),           &
                          k_start    , k_end                         )

       ENDDO
       !$OMP END PARALLEL DO


     ENDIF

! calculate vertical diffusion first and then horizontal
! (keep this order)

     IF(config_flags%diff_opt .eq. 2) THEN

       IF (config_flags%bl_pbl_physics .eq. 0) THEN


         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles

           CALL wrf_debug ( 200 , ' call vertical_diffusion_2 ' )
           CALL vertical_diffusion_2( ru_tendf, rv_tendf, rw_tendf,              &
                                      t_tendf, tke_tend,                         &
                                      moist_tend, num_3d_m,                      &
                                      chem_tend, num_3d_c,                       &
                                      scalar_tend, num_3d_s,                     &
                                      grid%em_u_2, grid%em_v_2,                                  &
                                      grid%em_t_2,grid%u_base,grid%v_base,grid%em_t_base,grid%qv_base,          &
                                      grid%em_mut,grid%em_tke_2,config_flags,                    &
                                      grid%defor13,grid%defor23,grid%defor33,                   &
                                      grid%div, moist, chem, scalar,                  &
                                      grid%xkmv, grid%xkhv, config_flags%km_opt,                        &
                                      grid%em_fnm, grid%em_fnp, grid%em_dn, grid%em_dnw, grid%em_rdz, grid%em_rdzw,              &
                                      ids, ide, jds, jde, kds, kde,              &
                                      ims, ime, jms, jme, kms, kme,              &
                                      grid%i_start(ij), grid%i_end(ij),          &
                                      grid%j_start(ij), grid%j_end(ij),          &
                                      k_start    , k_end                        )

         ENDDO
         !$OMP END PARALLEL DO


       ENDIF
!

       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles

         CALL wrf_debug ( 200 , ' call horizontal_diffusion_2' )
         CALL horizontal_diffusion_2( t_tendf, ru_tendf, rv_tendf, rw_tendf, &
                                      tke_tend,                              &
                                      moist_tend, num_3d_m,                  &
                                      chem_tend, num_3d_c,                   &
                                      scalar_tend, num_3d_s,                 &
                                      grid%em_t_2, th_phy,                           &
                                      grid%em_mut, grid%em_tke_2, config_flags,              &
                                      grid%defor11, grid%defor22, grid%defor12,             &
                                      grid%defor13, grid%defor23, grid%div,                 &
                                      moist, chem, scalar,                   &
                                      grid%msfu, grid%msfv, grid%msft, grid%xkmhd, grid%xkhh, config_flags%km_opt, &
                                      grid%rdx, grid%rdy, grid%em_rdz, grid%em_rdzw,                   &
                                      grid%em_fnm, grid%em_fnp, grid%cf1, grid%cf2, grid%cf3,               &
                                      grid%em_zx, grid%em_zy, grid%em_dn, grid%em_dnw,                       &
                                      ids, ide, jds, jde, kds, kde,          &
                                      ims, ime, jms, jme, kms, kme,          &
                                      grid%i_start(ij), grid%i_end(ij),      &
                                      grid%j_start(ij), grid%j_end(ij),      &
                                      k_start    , k_end                    )
       ENDDO
       !$OMP END PARALLEL DO


     ENDIF

!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_OBS_NUDGE.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_OBS_NUDGE.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 2, &
     2, 3, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_pb, 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_p, 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%uratx, 2, 4, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%vratx, 2, 4, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%tratx, 2, 4, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_pb, 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_p, 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%uratx, 2, 4, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%vratx, 2, 4, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%tratx, 2, 4, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_INIT_EXCH ( local_communicator, 2 , &
     2, 3, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_pb, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_p, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%uratx, 2, 4, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%vratx, 2, 4, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%tratx, 2, 4, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_pb, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_p, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%uratx, 2, 4, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%vratx, 2, 4, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%tratx, 2, 4, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
!ENDOFREGISTRYGENERATEDINCLUDE
!***********************************************************************
! This section for obs nudging
      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij )

      DO ij = 1 , grid%num_tiles

         CALL fddaobs_driver (grid%grid_id, model_config_rec%grid_id,  &
                  model_config_rec%parent_id, config_flags%restart,    &
                  grid%obs_nudge_opt,                                  &
                  grid%obs_ipf_errob,                                  &
                  grid%obs_ipf_nudob,                                  &
                  grid%fdda_start,                                     &
                  grid%fdda_end,                                       &
                  grid%obs_nudge_wind,                                 &
                  grid%obs_nudge_temp,                                 &
                  grid%obs_nudge_mois,                                 &
                  grid%obs_nudge_pstr,                                 &
                  grid%obs_coef_wind,                                  &
                  grid%obs_coef_temp,                                  &
                  grid%obs_coef_mois,                                  &
                  grid%obs_coef_pstr,                                  &             
                  grid%obs_rinxy,                                      &
                  grid%obs_rinsig,                                     &
                  grid%obs_twindo,                                     &
                  grid%obs_npfi,                                       &
                  grid%obs_ionf,                                       &
                  grid%obs_idynin,                                     &
                  grid%obs_dtramp,                                     &
                  model_config_rec%cen_lat(1),                         &
                  model_config_rec%cen_lon(1),                         &
                  config_flags%truelat1,                               &
                  config_flags%truelat2,                               &
                  config_flags%map_proj,                               &
                  model_config_rec%i_parent_start,                     &
                  model_config_rec%j_parent_start,                     &
                  grid%parent_grid_ratio,                              &
                  grid%max_dom, grid%itimestep,                        &
                  grid%dt, grid%gmt, grid%julday, grid%fdob,           &
                  grid%max_obs,                                        &
                  model_config_rec%nobs_ndg_vars,                      &
                  model_config_rec%nobs_err_flds,                      &
                  grid%fdob%nstat, grid%fdob%varobs, grid%fdob%errf,   &
                  grid%dx, grid%KPBL,grid%HT,                          &
                  grid%em_mut, grid%em_muu, grid%em_muv,               &
                  grid%msft, grid%msfu, grid%msfv,                     &
                  p_phy, t_tendf, t0,                                  &
                  grid%em_u_2, grid%em_v_2, grid%em_t_2,               &
                  moist(:,:,:,P_QV),                                   &
                  grid%em_pb, grid%p_top, grid%em_p,                   &
                  grid%uratx, grid%vratx, grid%tratx,                  &
                  ru_tendf, rv_tendf,                                  &
                  moist_tend(:,:,:,P_QV), grid%em_obs_savwt,           &
                  ids,ide, jds,jde, kds,kde,                           &
                  ims,ime, jms,jme, kms,kme,                           &
                  grid%i_start(ij), min(grid%i_end(ij),ide-1),         &
                  grid%j_start(ij), min(grid%j_end(ij),jde-1),         &
                  k_start    , min(k_end,kde-1)                     )

      ENDDO

     !$OMP END PARALLEL DO
! 
!***********************************************************************

     END IF rk_step_is_one


   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )
   DO ij = 1 , grid%num_tiles

      CALL wrf_debug ( 200 , ' call rk_tendency' )
      CALL rk_tendency ( config_flags, rk_step,                           &
                         grid%em_ru_tend, grid%em_rv_tend, rw_tend, ph_tend, t_tend,      &
                         ru_tendf, rv_tendf, rw_tendf, ph_tendf, t_tendf, &
                         mu_tend, grid%em_u_save, grid%em_v_save, w_save, ph_save,        &
                         grid%em_t_save, mu_save, grid%rthften,                        &
                         grid%em_ru, grid%em_rv, grid%em_rw, grid%em_ww,                                  &
                         grid%em_u_2, grid%em_v_2, grid%em_w_2, grid%em_t_2, grid%em_ph_2,                        &
                         grid%em_u_1, grid%em_v_1, grid%em_w_1, grid%em_t_1, grid%em_ph_1,                        &
                         grid%h_diabatic, grid%em_phb, grid%em_t_init,                         &
                         grid%em_mu_2, grid%em_mut, grid%em_muu, grid%em_muv, grid%em_mub,                        &
                         grid%em_al, grid%em_alt, grid%em_p, grid%em_pb, grid%em_php, cqu, cqv, cqw,              &
                         grid%u_base, grid%v_base, grid%em_t_base, grid%qv_base, grid%z_base,         &
                         grid%msfu, grid%msfv, grid%msft, grid%f, grid%e, grid%sina, grid%cosa,              &
                         grid%em_fnm, grid%em_fnp, grid%em_rdn, grid%em_rdnw,                             &
                         grid%dt, grid%rdx, grid%rdy, grid%khdif, grid%kvdif, grid%xkmhd,               &
                         grid%diff_6th_opt, grid%diff_6th_factor,           &
                         grid%dampcoef,grid%zdamp,config_flags%damp_opt,                         &
                         grid%cf1, grid%cf2, grid%cf3, grid%cfn, grid%cfn1, num_3d_m,              &
                         config_flags%non_hydrostatic,                    &
                         ids, ide, jds, jde, kds, kde,                    &
                         ims, ime, jms, jme, kms, kme,                    &
                         grid%i_start(ij), grid%i_end(ij),                &
                         grid%j_start(ij), grid%j_end(ij),                &
                         k_start, k_end                                  )
   END DO
   !$OMP END PARALLEL DO



   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )
   DO ij = 1 , grid%num_tiles

     IF( (config_flags%specified .or. config_flags%nested) .and. rk_step == 1 ) THEN 

       CALL relax_bdy_dry ( config_flags,                                &
                            grid%em_u_save, grid%em_v_save, ph_save, grid%em_t_save,             &
                            w_save, mu_tend,                             & 
                            grid%em_ru, grid%em_rv, grid%em_ph_2, grid%em_t_2,                           &
                            grid%em_w_2, grid%em_mu_2, grid%em_mut,                              &
                            grid%em_u_b, grid%em_v_b, grid%em_ph_b, grid%em_t_b, grid%em_w_b,                    &
                            grid%em_mu_b,                                        &
                            grid%em_u_bt, grid%em_v_bt, grid%em_ph_bt, grid%em_t_bt,                     &
                            grid%em_w_bt, grid%em_mu_bt,                                 &
                            config_flags%spec_bdy_width, grid%spec_zone, grid%relax_zone,       &
                            grid%dtbc, grid%fcx, grid%gcx,                              &
                            ijds, ijde,                                  &
                            ids,ide, jds,jde, kds,kde,                   &
                            ims,ime, jms,jme, kms,kme,                   &
                            ips,ipe, jps,jpe, kps,kpe,                   &
                            grid%i_start(ij), grid%i_end(ij),            &
                            grid%j_start(ij), grid%j_end(ij),            &
                            k_start, k_end                              )


     ENDIF

     CALL rk_addtend_dry( grid%em_ru_tend,  grid%em_rv_tend,  rw_tend,  ph_tend,  t_tend,  &
                          ru_tendf, rv_tendf, rw_tendf, ph_tendf, t_tendf, &
                          grid%em_u_save, grid%em_v_save, w_save, ph_save, grid%em_t_save, &
                          mu_tend, mu_tendf, rk_step,                      &
                          grid%h_diabatic, grid%em_mut, grid%msft, grid%msfu, grid%msfv,               &
                          ids,ide, jds,jde, kds,kde,                       &
                          ims,ime, jms,jme, kms,kme,                       &
                          ips,ipe, jps,jpe, kps,kpe,                       &
                          grid%i_start(ij), grid%i_end(ij),                &
                          grid%j_start(ij), grid%j_end(ij),                &
                          k_start, k_end                                  )

     IF( config_flags%specified .or. config_flags%nested ) THEN 
       CALL spec_bdy_dry ( config_flags,                                    &
                           grid%em_ru_tend, grid%em_rv_tend, ph_tend, t_tend,               &
                           rw_tend, mu_tend,                                &
                           grid%em_u_b, grid%em_v_b, grid%em_ph_b, grid%em_t_b,                             &
                           grid%em_w_b, grid%em_mu_b,                                       &
                           grid%em_u_bt, grid%em_v_bt, grid%em_ph_bt, grid%em_t_bt,                         &
                           grid%em_w_bt, grid%em_mu_bt,                                     &
                           config_flags%spec_bdy_width, grid%spec_zone,                       &
                           ijds, ijde,                 & ! min/max(id,jd)
                           ids,ide, jds,jde, kds,kde,  & ! domain dims
                           ims,ime, jms,jme, kms,kme,  & ! memory dims
                           ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                           grid%i_start(ij), grid%i_end(ij),                &
                           grid%j_start(ij), grid%j_end(ij),                &
                           k_start, k_end                                  )
     
     ENDIF

   END DO
   !$OMP END PARALLEL DO


!<DESCRIPTION>
!<pre>
! (3) Small (acoustic,sound) steps.
!
!    Several acoustic steps are taken each RK pass.  A small step 
!    sequence begins with calculating perturbation variables 
!    and coupling them to the column dry-air-mass mu 
!    (call to small_step_prep).  This is followed by computing
!    coefficients for the vertically implicit part of the
!    small timestep (call to calc_coef_w).  
!
!    The small steps are taken
!    in the named loop "small_steps:".  In the small_steps loop, first 
!    the horizontal momentum (u and v) are advanced (call to advance_uv),
!    next mu and theta are advanced (call to advance_mu_t) followed by
!    advancing w and the geopotential (call to advance_w).  Diagnostic
!    values for pressure and inverse density are updated at the end of
!    each small_step.
!
!    The small-step section ends with the change of the perturbation variables
!    back to full variables (call to small_step_finish).
!</pre>
!</DESCRIPTION>


   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   DO ij = 1 , grid%num_tiles

    ! Calculate coefficients for the vertically implicit acoustic/gravity wave
    ! integration.  We only need calculate these for the first pass through -
    ! the predictor step.  They are reused as is for the corrector step.
    ! For third-order RK, we need to recompute these after the first 
    ! predictor because we may have changed the small timestep -> grid%dts.

      CALL wrf_debug ( 200 , ' call calc_coef_w' )

      CALL small_step_prep( grid%em_u_1,grid%em_u_2,grid%em_v_1,grid%em_v_2,grid%em_w_1,grid%em_w_2,          &
                            grid%em_t_1,grid%em_t_2,grid%em_ph_1,grid%em_ph_2,                &
                            grid%em_mub, grid%em_mu_1, grid%em_mu_2,                  &
                            grid%em_muu, muus, grid%em_muv, muvs,             &
                            grid%em_mut, grid%em_muts, grid%em_mudf,                  & 
                            grid%em_u_save, grid%em_v_save, w_save,           & 
                            grid%em_t_save, ph_save, mu_save,         &
                            grid%em_ww, ww1,                          &
                            grid%em_dnw, c2a, grid%em_pb, grid%em_p, grid%em_alt,             &
                            grid%msfu, grid%msfv, grid%msft,                 &
                            rk_step,                          &
                            ids, ide, jds, jde, kds, kde,     &
                            ims, ime, jms, jme, kms, kme,     &
                            grid%i_start(ij), grid%i_end(ij), &
                            grid%j_start(ij), grid%j_end(ij), &
                            k_start    , k_end               )
      CALL calc_p_rho( grid%em_al, grid%em_p, grid%em_ph_2,                      &
                       grid%em_alt, grid%em_t_2, grid%em_t_save, c2a, pm1,       &
                       grid%em_mu_2, grid%em_muts, grid%em_znu, t0,              &
                       grid%em_rdnw, grid%em_dnw, grid%smdiv,                 &
                       config_flags%non_hydrostatic, 0,               &
                       ids, ide, jds, jde, kds, kde,     &
                       ims, ime, jms, jme, kms, kme,     &
                       grid%i_start(ij), grid%i_end(ij), &
                       grid%j_start(ij), grid%j_end(ij), &
                       k_start    , k_end               )

      IF (config_flags%non_hydrostatic)                                &
      CALL calc_coef_w( a,alpha,gamma,                    &
                        grid%em_mut, cqw,                         &
                        grid%em_rdn, grid%em_rdnw, c2a,                   &
                        dts_rk, g, grid%epssm,                 &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start    , k_end               )


   ENDDO
   !$OMP END PARALLEL DO



!-----------------------------------------------------------------------
!  Stencils for patch communications  (WCS, 29 June 2001)
!  Note:  the small size of this halo exchange reflects the 
!         fact that we are carrying the uncoupled variables 
!         as state variables in the mass coordinate model, as
!         opposed to the coupled variables as in the height
!         coordinate model.
!
!                              * * * * *
!            *        * * *    * * * * *
!          * + *      * + *    * * + * * 
!            *        * * *    * * * * *
!                              * * * * *
!
!  3D variables - note staggering!  grid%em_ph_2(grid%em_z), grid%em_u_save(X), grid%em_v_save(Y)
!
!j grid%em_ph_2      x
!j grid%em_al        x
!j grid%em_p         x
!j grid%em_t_1       x
!j grid%em_t_save    x
!j grid%em_u_save    x
!j grid%em_v_save    x
!
!  the following are 2D (xy) variables
!
!j grid%em_mu_1      x
!j grid%em_mu_2      x
!j grid%em_mudf      x
!--------------------------------------------------------------
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_B.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_B.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 1, &
     10, 3, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_2, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_al, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_p, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_1, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_save, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_save, 1, 4, 0, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_save, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_1, 1, 4, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_2, 1, 4, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mudf, 1, 4, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_php, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_alt, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_pb, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_2, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_al, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_p, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_1, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_save, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_save, 1, 4, 0, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_save, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_1, 1, 4, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_2, 1, 4, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mudf, 1, 4, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_php, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_alt, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_pb, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_INIT_EXCH ( local_communicator, 1 , &
     10, 3, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_2, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_al, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_p, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_1, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_save, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_save, 1, 4, 1, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_save, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_1, 1, 4, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_2, 1, 4, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mudf, 1, 4, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_php, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_alt, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_pb, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_2, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_al, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_p, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_1, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_save, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_save, 1, 4, 1, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_save, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_1, 1, 4, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_2, 1, 4, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mudf, 1, 4, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_php, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_alt, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_pb, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/PERIOD_BDY_EM_B.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_B.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 2 , &
     12, 3, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_ru_tend, 2, 4, 1, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_rv_tend, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_ph_2, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_al, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_p, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_t_1, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_t_save, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_u_save, 2, 4, 1, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_v_save, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_mu_1, 2, 4, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_mu_2, 2, 4, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_mudf, 2, 4, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_php, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_alt, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_pb, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_ru_tend, 2, 4, 1, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_rv_tend, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_ph_2, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_al, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_p, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_t_1, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_t_save, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_u_save, 2, 4, 1, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_v_save, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_mu_1, 2, 4, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_mu_2, 2, 4, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_mudf, 2, 4, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_php, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_alt, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_pb, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
END IF
!ENDOFREGISTRYGENERATEDINCLUDE


   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   DO ij = 1 , grid%num_tiles

         CALL set_physical_bc3d( grid%em_ru_tend, 'u', config_flags,          &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 ips, ipe, jps, jpe, kps, kpe, &
                           grid%i_start(ij), grid%i_end(ij),                 &
                           grid%j_start(ij), grid%j_end(ij),                 &
                           k_start    , k_end                     )

         CALL set_physical_bc3d( grid%em_rv_tend, 'v', config_flags,            &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 ips, ipe, jps, jpe, kps, kpe, &
                           grid%i_start(ij), grid%i_end(ij),                 &
                           grid%j_start(ij), grid%j_end(ij),                 &
                           k_start    , k_end                     )

         CALL set_physical_bc3d( grid%em_ph_2, 'w', config_flags,          &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 ips, ipe, jps, jpe, kps, kpe, &
                           grid%i_start(ij), grid%i_end(ij),                 &
                           grid%j_start(ij), grid%j_end(ij),                 &
                           k_start    , k_end                     )

         CALL set_physical_bc3d( grid%em_al, 'grid%em_p', config_flags,            &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 ips, ipe, jps, jpe, kps, kpe, &
                           grid%i_start(ij), grid%i_end(ij),                 &
                           grid%j_start(ij), grid%j_end(ij),                 &
                           k_start    , k_end                     )

         CALL set_physical_bc3d( grid%em_p, 'grid%em_p', config_flags,             &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 ips, ipe, jps, jpe, kps, kpe, &
                           grid%i_start(ij), grid%i_end(ij),                 &
                           grid%j_start(ij), grid%j_end(ij),                 &
                           k_start    , k_end                     )

         CALL set_physical_bc3d( grid%em_t_1, 'grid%em_p', config_flags,             &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 ips, ipe, jps, jpe, kps, kpe, &
                           grid%i_start(ij), grid%i_end(ij),                 &
                           grid%j_start(ij), grid%j_end(ij),                 &
                           k_start    , k_end                     )

         CALL set_physical_bc3d( grid%em_t_save, 't', config_flags,             &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 ips, ipe, jps, jpe, kps, kpe, &
                           grid%i_start(ij), grid%i_end(ij),                 &
                           grid%j_start(ij), grid%j_end(ij),                 &
                           k_start    , k_end                     )

         CALL set_physical_bc2d( grid%em_mu_1, 't', config_flags,          &
                                 ids, ide, jds, jde,               &
                                 ims, ime, jms, jme,               &
                                 ips, ipe, jps, jpe,               &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij) )

         CALL set_physical_bc2d( grid%em_mu_2, 't', config_flags,          &
                                 ids, ide, jds, jde,               &
                                 ims, ime, jms, jme,               &
                                 ips, ipe, jps, jpe,               &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij) )

         CALL set_physical_bc2d( grid%em_mudf, 't', config_flags,          &
                                 ids, ide, jds, jde,               &
                                 ims, ime, jms, jme,               &
                                 ips, ipe, jps, jpe,               &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij) )

    END DO
    !$OMP END PARALLEL DO


   small_steps : DO iteration = 1 , number_of_small_timesteps

   ! Boundary condition time (or communication time).  

!STARTOFREGISTRYGENERATEDINCLUDE 'inc/PERIOD_BDY_EM_B.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_B.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 2 , &
     12, 3, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_ru_tend, 2, 4, 1, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_rv_tend, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_ph_2, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_al, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_p, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_t_1, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_t_save, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_u_save, 2, 4, 1, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_v_save, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_mu_1, 2, 4, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_mu_2, 2, 4, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_mudf, 2, 4, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_php, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_alt, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_pb, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_ru_tend, 2, 4, 1, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_rv_tend, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_ph_2, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_al, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_p, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_t_1, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_t_save, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_u_save, 2, 4, 1, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_v_save, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_mu_1, 2, 4, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_mu_2, 2, 4, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_mudf, 2, 4, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_php, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_alt, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_pb, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
END IF
!ENDOFREGISTRYGENERATEDINCLUDE


      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij )

      DO ij = 1 , grid%num_tiles


         CALL advance_uv ( grid%em_u_2, grid%em_ru_tend, grid%em_v_2, grid%em_rv_tend,       &
                           grid%em_p, grid%em_pb,                            &
                           grid%em_ph_2, grid%em_php, grid%em_alt, grid%em_al, grid%em_mu_2,         &
                           grid%em_muu, cqu, grid%em_muv, cqv, grid%em_mudf,         &
                           grid%rdx, grid%rdy, dts_rk,                 &
                           grid%cf1, grid%cf2, grid%cf3, grid%em_fnm, grid%em_fnp,          &
                           grid%emdiv,                            &
                           grid%em_rdnw, config_flags,grid%spec_zone,     &
                           config_flags%non_hydrostatic,                  &
                           ids, ide, jds, jde, kds, kde,     &
                           ims, ime, jms, jme, kms, kme,     &
                           grid%i_start(ij), grid%i_end(ij), &
                           grid%j_start(ij), grid%j_end(ij), &
                           k_start    , k_end               )



         IF( config_flags%specified .or. config_flags%nested ) THEN
           CALL spec_bdyupdate(grid%em_u_2, grid%em_ru_tend, dts_rk,      &
                               'u'         , config_flags, &
                               grid%spec_zone,                  &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               grid%i_start(ij), grid%i_end(ij),         &
                               grid%j_start(ij), grid%j_end(ij),         &
                               k_start    , k_end             )

           CALL spec_bdyupdate(grid%em_v_2, grid%em_rv_tend, dts_rk,      &
                               'v'         , config_flags, &
                               grid%spec_zone,                  &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               grid%i_start(ij), grid%i_end(ij),         &
                               grid%j_start(ij), grid%j_end(ij),         &
                               k_start    , k_end             )

         ENDIF


      END DO
      !$OMP END PARALLEL DO

!
!  Stencils for patch communications  (WCS, 29 June 2001)
!
!         *                     *
!       * + *      * + *        +
!         *                     *
!
!  grid%em_u_2               x
!  grid%em_v_2                          x
!
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_C.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_C.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 1, &
     2, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_2, 1, 4, 0, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_2, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_2, 1, 4, 0, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_2, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_INIT_EXCH ( local_communicator, 1 , &
     2, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_2, 1, 4, 1, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_2, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_2, 1, 4, 1, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_2, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
!ENDOFREGISTRYGENERATEDINCLUDE

      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij )

      DO ij = 1 , grid%num_tiles

        !  advance the mass in the column, theta, and calculate grid%em_ww


        CALL advance_mu_t( grid%em_ww, ww1, grid%em_u_2, grid%em_u_save, grid%em_v_2, grid%em_v_save, &
                           grid%em_mu_2, grid%em_mut, muave, grid%em_muts, grid%em_muu, grid%em_muv,  &
                           grid%em_mudf, grid%em_ru_m, grid%em_rv_m, grid%em_ww_m,                       &
                           grid%em_t_2, grid%em_t_save, t_2save, t_tend,                              &
                           mu_tend,                                                                   &
                           grid%rdx, grid%rdy, dts_rk, grid%epssm,                                    &
                           grid%em_dnw, grid%em_fnm, grid%em_fnp, grid%em_rdnw,                       &
                           grid%msfu, grid%msfv, grid%msft,                                           &
                           iteration, config_flags,                                                   &
                           ids, ide, jds, jde, kds, kde,      &
                           ims, ime, jms, jme, kms, kme,      &
                           grid%i_start(ij), grid%i_end(ij),  &
                           grid%j_start(ij), grid%j_end(ij),  &
                           k_start    , k_end                )



         IF( config_flags%specified .or. config_flags%nested ) THEN

           CALL spec_bdyupdate(grid%em_t_2, t_tend, dts_rk,      &
                               't'         , config_flags, &
                               grid%spec_zone,                  &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               grid%i_start(ij), grid%i_end(ij),         &
                               grid%j_start(ij), grid%j_end(ij),         &
                               k_start    , k_end             )

           CALL spec_bdyupdate(grid%em_mu_2, mu_tend, dts_rk,      &
                               'm'         , config_flags, &
                               grid%spec_zone,                  &
                               ids,ide, jds,jde, 1  ,1  ,  & ! domain dims
                               ims,ime, jms,jme, 1  ,1  ,  & ! memory dims
                               ips,ipe, jps,jpe, 1  ,1  ,  & ! patch  dims
                               grid%i_start(ij), grid%i_end(ij),         &
                               grid%j_start(ij), grid%j_end(ij),         &
                               1    , 1             )

           CALL spec_bdyupdate(grid%em_muts, mu_tend, dts_rk,      &
                               'm'         , config_flags, &
                               grid%spec_zone,                  &
                               ids,ide, jds,jde, 1  ,1  ,  & ! domain dims
                               ims,ime, jms,jme, 1  ,1  ,  & ! memory dims
                               ips,ipe, jps,jpe, 1  ,1  ,  & ! patch  dims
                               grid%i_start(ij), grid%i_end(ij),         &
                               grid%j_start(ij), grid%j_end(ij),         &
                               1    , 1             )
         ENDIF


         ! sumflux accumulates the time-averged mass flux
         ! (time averaged over the acoustic steps) for use
         ! in the scalar advection (flux divergence).  Using
         ! time averaged values gives us exact scalar conservation.


         CALL sumflux ( grid%em_u_2, grid%em_v_2, grid%em_ww,                         &
                        grid%em_u_save, grid%em_v_save, ww1,                  &
                        grid%em_muu, grid%em_muv,                             &
                        grid%em_ru_m, grid%em_rv_m, grid%em_ww_m, grid%epssm,              &
                        grid%msfu, grid%msfv,                           &
                        iteration, number_of_small_timesteps, &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        grid%i_start(ij), grid%i_end(ij),     &
                        grid%j_start(ij), grid%j_end(ij),     &
                        k_start    , k_end                   )


         ! small (acoustic) step for the vertical momentum,
         ! density and coupled potential temperature.



        IF ( config_flags%non_hydrostatic ) THEN
          CALL advance_w( grid%em_w_2, rw_tend, grid%em_ww, grid%em_u_2, grid%em_v_2,       &
                          grid%em_mu_2, grid%em_mut, muave, grid%em_muts,           &
                          t_2save, grid%em_t_2, grid%em_t_save,             &
                          grid%em_ph_2, ph_save, grid%em_phb, ph_tend,      &
                          grid%ht, c2a, cqw, grid%em_alt, grid%em_alb,           &
                          a, alpha, gamma,                  &
                          grid%rdx, grid%rdy, dts_rk, t0, grid%epssm,      &
                          grid%em_dnw, grid%em_fnm, grid%em_fnp, grid%em_rdnw, grid%em_rdn,         &
                          grid%cf1, grid%cf2, grid%cf3, grid%msft,              &
                          config_flags,                     &
                          ids,ide, jds,jde, kds,kde,        & ! domain dims
                          ims,ime, jms,jme, kms,kme,        & ! memory dims
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          k_start    , k_end               )
        ENDIF


        IF( config_flags%specified .or. config_flags%nested ) THEN


           IF (config_flags%non_hydrostatic)  THEN
             CALL spec_bdyupdate_ph( ph_save, grid%em_ph_2, ph_tend, mu_tend, grid%em_muts, dts_rk, &
                                     'h'         , config_flags, &
                                     grid%spec_zone,                  &
                                     ids,ide, jds,jde, kds,kde,  & ! domain dims
                                     ims,ime, jms,jme, kms,kme,  & ! memory dims
                                     ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                     grid%i_start(ij), grid%i_end(ij),         &
                                     grid%j_start(ij), grid%j_end(ij),         &
                                     k_start    , k_end             )
             IF( config_flags%specified ) THEN
               CALL zero_grad_bdy ( grid%em_w_2,                        &
                                    'w'         , config_flags, &
                                    grid%spec_zone,                  &
                                    ids,ide, jds,jde, kds,kde,  & ! domain dims
                                    ims,ime, jms,jme, kms,kme,  & ! memory dims
                                    ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                    grid%i_start(ij), grid%i_end(ij),         &
                                    grid%j_start(ij), grid%j_end(ij),         &
                                    k_start    , k_end             )
             ELSE
               CALL spec_bdyupdate   ( grid%em_w_2, rw_tend, dts_rk,       &
                                       'h'         , config_flags, &
                                       grid%spec_zone,                  &
                                       ids,ide, jds,jde, kds,kde,  & ! domain dims
                                       ims,ime, jms,jme, kms,kme,  & ! memory dims
                                       ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                       grid%i_start(ij), grid%i_end(ij),         &
                                       grid%j_start(ij), grid%j_end(ij),         &
                                       k_start    , k_end             )
             ENDIF
          ENDIF

        ENDIF


        CALL calc_p_rho( grid%em_al, grid%em_p, grid%em_ph_2,                      &
                         grid%em_alt, grid%em_t_2, grid%em_t_save, c2a, pm1,       &
                         grid%em_mu_2, grid%em_muts, grid%em_znu, t0,              &
                         grid%em_rdnw, grid%em_dnw, grid%smdiv,                 &
                         config_flags%non_hydrostatic, iteration,       &
                         ids, ide, jds, jde, kds, kde,     &
                         ims, ime, jms, jme, kms, kme,     &
                         grid%i_start(ij), grid%i_end(ij), &
                         grid%j_start(ij), grid%j_end(ij), &
                         k_start    , k_end               )


   ENDDO
   !$OMP END PARALLEL DO

!
!  Stencils for patch communications  (WCS, 29 June 2001)
!
!         *                     *
!       * + *      * + *        +
!         *                     *
!
!  grid%em_ph_2   x
!  grid%em_al     x
!  grid%em_p      x
!
!  2D variables (x,y)
!
!  grid%em_mu_2   x
!  grid%em_muts   x
!  grid%em_mudf   x

!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_C2.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_C2.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 1, &
     3, 3, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_2, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_al, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_p, 1, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_2, 1, 4, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_muts, 1, 4, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mudf, 1, 4, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_2, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_al, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_p, 1, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_2, 1, 4, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_muts, 1, 4, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mudf, 1, 4, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_INIT_EXCH ( local_communicator, 1 , &
     3, 3, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_2, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_al, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_p, 1, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_2, 1, 4, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_muts, 1, 4, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mudf, 1, 4, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_2, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_al, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_p, 1, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_2, 1, 4, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_muts, 1, 4, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mudf, 1, 4, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/PERIOD_BDY_EM_B3.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_B3.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 2 , &
     3, 3, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_ph_2, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_al, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_p, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_mu_2, 2, 4, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_muts, 2, 4, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_mudf, 2, 4, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_ph_2, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_al, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_p, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_mu_2, 2, 4, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_muts, 2, 4, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_mudf, 2, 4, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
END IF
!ENDOFREGISTRYGENERATEDINCLUDE


      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij )

      DO ij = 1 , grid%num_tiles

        ! boundary condition set for next small timestep

         CALL set_physical_bc3d( grid%em_ph_2, 'w', config_flags,          &
                                 ids, ide, jds, jde, kds, kde,     &
                                 ims, ime, jms, jme, kms, kme,     &
                                 ips, ipe, jps, jpe, kps, kpe,     &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij), &
                                 k_start    , k_end               )

         CALL set_physical_bc3d( grid%em_al, 'grid%em_p', config_flags,            &
                                 ids, ide, jds, jde, kds, kde,     &
                                 ims, ime, jms, jme, kms, kme,     &
                                 ips, ipe, jps, jpe, kps, kpe,     &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij), &
                                 k_start    , k_end               )

         CALL set_physical_bc3d( grid%em_p, 'grid%em_p', config_flags,             &
                                 ids, ide, jds, jde, kds, kde,     &
                                 ims, ime, jms, jme, kms, kme,     &
                                 ips, ipe, jps, jpe, kps, kpe,     &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij), &
                                 k_start    , k_end               )

         CALL set_physical_bc2d( grid%em_muts, 't', config_flags,          &
                                 ids, ide, jds, jde,               &
                                 ims, ime, jms, jme,               &
                                 ips, ipe, jps, jpe,               &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij) )

         CALL set_physical_bc2d( grid%em_mu_2, 't', config_flags,          &
                                 ids, ide, jds, jde,               &
                                 ims, ime, jms, jme,               &
                                 ips, ipe, jps, jpe,               &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij) )

         CALL set_physical_bc2d( grid%em_mudf, 't', config_flags,          &
                                 ids, ide, jds, jde,               &
                                 ims, ime, jms, jme,               &
                                 ips, ipe, jps, jpe,               &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij) )

      END DO
      !$OMP END PARALLEL DO


   END DO small_steps

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   DO ij = 1 , grid%num_tiles

      CALL wrf_debug ( 200 , ' call rk_small_finish' )

      ! change time-perturbation variables back to 
      ! full perturbation variables.
      ! first get updated mu at u and v points


      CALL calc_mu_uv_1 ( config_flags,                     &
                          grid%em_muts, muus, muvs,                 &
                          ids, ide, jds, jde, kds, kde,     &
                          ims, ime, jms, jme, kms, kme,     &
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          k_start    , k_end               )


      CALL small_step_finish( grid%em_u_2, grid%em_u_1, grid%em_v_2, grid%em_v_1, grid%em_w_2, grid%em_w_1,     &
                              grid%em_t_2, grid%em_t_1, grid%em_ph_2, grid%em_ph_1, grid%em_ww, ww1,    &
                              grid%em_mu_2, grid%em_mu_1,                       &
                              grid%em_mut, grid%em_muts, grid%em_muu, muus, grid%em_muv, muvs,  & 
                              grid%em_u_save, grid%em_v_save, w_save,           &
                              grid%em_t_save, ph_save, mu_save,         &
                              grid%msfu, grid%msfv, grid%msft,                 &
                              grid%h_diabatic,                       &
                              number_of_small_timesteps,dts_rk, &
                              rk_step, rk_order,                &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start    , k_end               )
!  call  to set ru_m, rv_m and ww_m b.cs for PD advection

         IF (rk_step == 3) THEN

           CALL set_physical_bc3d( grid%em_ru_m, 'u', config_flags,   &
                                   ids, ide, jds, jde, kds, kde,      &
                                   ims, ime, jms, jme, kms, kme,      &
                                   ips, ipe, jps, jpe, kps, kpe,      &
                                   grid%i_start(ij), grid%i_end(ij),  &
                                   grid%j_start(ij), grid%j_end(ij),  &
                                   k_start    , k_end                )

           CALL set_physical_bc3d( grid%em_rv_m, 'v', config_flags,   &
                                   ids, ide, jds, jde, kds, kde,      &
                                   ims, ime, jms, jme, kms, kme,      &
                                   ips, ipe, jps, jpe, kps, kpe,      &
                                   grid%i_start(ij), grid%i_end(ij),  &
                                   grid%j_start(ij), grid%j_end(ij),  &
                                   k_start    , k_end                )

           CALL set_physical_bc3d( grid%em_ww_m, 'w', config_flags,   &
                                   ids, ide, jds, jde, kds, kde,      &
                                   ims, ime, jms, jme, kms, kme,      &
                                   ips, ipe, jps, jpe, kps, kpe,      &
                                   grid%i_start(ij), grid%i_end(ij),  &
                                   grid%j_start(ij), grid%j_end(ij),  &
                                   k_start    , k_end                )

           CALL set_physical_bc2d( grid%em_mut, 't', config_flags,   &
                                   ids, ide, jds, jde,               &
                                   ims, ime, jms, jme,                &
                                   ips, ipe, jps, jpe,                &
                                   grid%i_start(ij), grid%i_end(ij),  &
                                   grid%j_start(ij), grid%j_end(ij) )

          END IF



   END DO
   !$OMP END PARALLEL DO

!-----------------------------------------------------------------------
!  add in physics tendency first if positive definite advection is used.
!  pd advection applies advective flux limiter on last runge-kutta step
!-----------------------------------------------------------------------
! first moisture

  IF (config_flags%pd_moist .and. (rk_step == rk_order)) THEN

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )
   DO ij = 1 , grid%num_tiles
       CALL wrf_debug ( 200 , ' call rk_update_scalar_pd' )
       do im = PARAM_FIRST_SCALAR, num_3d_m
       CALL rk_update_scalar_pd( im, im,                                   &
                                 moist_old(ims,kms,jms,im),                &
                                 moist_tend(ims,kms,jms,im),               &
                                 grid%msft,                                &
                                 grid%em_mu_1, grid%em_mu_1, grid%em_mub,  &
                                 rk_step, dt_rk, grid%spec_zone,           &
                                 config_flags,                             &
                                 ids, ide, jds, jde, kds, kde,             &
                                 ims, ime, jms, jme, kms, kme,             &
                                 grid%i_start(ij), grid%i_end(ij),         &
                                 grid%j_start(ij), grid%j_end(ij),         &
                                 k_start    , k_end                       )
       ENDDO
   END DO
   !$OMP END PARALLEL DO

!---------------------- positive definite bc call

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )
   DO ij = 1 , grid%num_tiles
      IF (num_3d_m >= PARAM_FIRST_SCALAR) THEN
        DO im = PARAM_FIRST_SCALAR , num_3d_m
          CALL set_physical_bc3d( moist_old(ims,kms,jms,im), 'p', config_flags,   &
                                   ids, ide, jds, jde, kds, kde,                  &
                                   ims, ime, jms, jme, kms, kme,                  &
                                   ips, ipe, jps, jpe, kps, kpe,                  &
                                   grid%i_start(ij), grid%i_end(ij),              &
                                   grid%j_start(ij), grid%j_end(ij),              &
                                   k_start    , k_end                            )
         END DO
      ENDIF
   END DO
   !$OMP END PARALLEL DO

   if(config_flags%pd_moist) then
     IF      ( config_flags%h_sca_adv_order <= 4 ) THEN
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_MOIST_OLD_E_5.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_MOIST_OLD_E_5.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3, &
     0  &
   + num_moist   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist_old ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist_old ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3 , &
     0  &
   + num_moist   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist_old ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist_old ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
     ELSE IF ( config_flags%h_sca_adv_order <= 6 ) THEN
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_MOIST_OLD_E_7.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_MOIST_OLD_E_7.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 4, &
     0  &
   + num_moist   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist_old ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist_old ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_INIT_EXCH ( local_communicator, 4 , &
     0  &
   + num_moist   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist_old ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist_old ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
     ELSE
       WRITE(wrf_err_message,*)'solve_em: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
       CALL wrf_error_fatal3 ( "solve_em.b" , 1978 , TRIM(wrf_err_message))
     ENDIF
  endif

   END IF  ! end if for pd_moist

! scalars

  IF (config_flags%pd_scalar .and. (rk_step == rk_order)) THEN

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   DO ij = 1 , grid%num_tiles
       CALL wrf_debug ( 200 , ' call rk_update_scalar_pd' )
       do im = PARAM_FIRST_SCALAR, num_3d_s
       CALL rk_update_scalar_pd( im, im,                                  &
                                 scalar_old(ims,kms,jms,im),              &
                                 scalar_tend(ims,kms,jms,im),             &
                                 grid%msft,                               &
                                 grid%em_mu_1, grid%em_mu_1, grid%em_mub, &
                                 rk_step, dt_rk, grid%spec_zone,          &
                                 config_flags,                            &
                                 ids, ide, jds, jde, kds, kde,            &
                                 ims, ime, jms, jme, kms, kme,            &
                                 grid%i_start(ij), grid%i_end(ij),        &
                                 grid%j_start(ij), grid%j_end(ij),        &
                                 k_start    , k_end                      )
       ENDDO
   ENDDO
   !$OMP END PARALLEL DO

!---------------------- positive definite bc call

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   DO ij = 1 , grid%num_tiles
      IF (num_3d_m >= PARAM_FIRST_SCALAR) THEN
        DO im = PARAM_FIRST_SCALAR , num_3d_s
            CALL set_physical_bc3d(  scalar_old(ims,kms,jms,im), 'p', config_flags, &
                                   ids, ide, jds, jde, kds, kde,                    &
                                   ims, ime, jms, jme, kms, kme,                    &
                                   ips, ipe, jps, jpe, kps, kpe,                    &
                                   grid%i_start(ij), grid%i_end(ij),                &
                                   grid%j_start(ij), grid%j_end(ij),                &
                                   k_start    , k_end                              )
         END DO
      ENDIF
   END DO
   !$OMP END PARALLEL DO

   if(config_flags%pd_scalar) then
     IF      ( config_flags%h_sca_adv_order <= 4 ) THEN
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_SCALAR_OLD_E_5.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_SCALAR_OLD_E_5.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3, &
     0  &
   + num_scalar   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar_old ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar_old ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3 , &
     0  &
   + num_scalar   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar_old ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar_old ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
     ELSE IF ( config_flags%h_sca_adv_order <= 6 ) THEN
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_SCALAR_OLD_E_7.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_SCALAR_OLD_E_7.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 4, &
     0  &
   + num_scalar   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar_old ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar_old ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_INIT_EXCH ( local_communicator, 4 , &
     0  &
   + num_scalar   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar_old ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar_old ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
     ELSE
       WRITE(wrf_err_message,*)'solve_em: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
       CALL wrf_error_fatal3 ( "solve_em.b" , 2044 , TRIM(wrf_err_message))
     ENDIF
  endif

   END IF  ! end if for pd_scalar

! chem

  IF (config_flags%pd_chem .and. (rk_step == rk_order)) THEN

   write(6,*) ' pd advection for chem '

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )
   DO ij = 1 , grid%num_tiles
       CALL wrf_debug ( 200 , ' call rk_update_scalar_pd' )
       do im = PARAM_FIRST_SCALAR, num_3d_c
       CALL rk_update_scalar_pd( im, im,                                  &
                                 chem_old(ims,kms,jms,im),                &
                                 chem_tend(ims,kms,jms,im),               &
                                 grid%msft,                               &
                                 grid%em_mu_1, grid%em_mu_1, grid%em_mub, &
                                 rk_step, dt_rk, grid%spec_zone,          &
                                 config_flags,                            &
                                 ids, ide, jds, jde, kds, kde,            &
                                 ims, ime, jms, jme, kms, kme,            &
                                 grid%i_start(ij), grid%i_end(ij),        &
                                 grid%j_start(ij), grid%j_end(ij),        &
                                 k_start    , k_end                      )
       ENDDO
   END DO
   !$OMP END PARALLEL DO

!---------------------- positive definite bc call

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )
   DO ij = 1 , grid%num_tiles
      IF (num_3d_m >= PARAM_FIRST_SCALAR) THEN
        DO im = PARAM_FIRST_SCALAR , num_3d_c
          CALL set_physical_bc3d(  chem_old(ims,kms,jms,im), 'p', config_flags,     &
                                   ids, ide, jds, jde, kds, kde,                    &
                                   ims, ime, jms, jme, kms, kme,                    &
                                   ips, ipe, jps, jpe, kps, kpe,                    &
                                   grid%i_start(ij), grid%i_end(ij),                &
                                   grid%j_start(ij), grid%j_end(ij),                &
                                   k_start    , k_end                              )
         END DO 
      ENDIF
   END DO
   !$OMP END PARALLEL DO


   if(config_flags%pd_chem) then
     IF      ( config_flags%h_sca_adv_order <= 4 ) THEN
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_CHEM_OLD_E_5.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_CHEM_OLD_E_5.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3, &
     0  &
   + num_chem   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem_old ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem_old ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3 , &
     0  &
   + num_chem   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem_old ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem_old ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
     ELSE IF ( config_flags%h_sca_adv_order <= 6 ) THEN
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_CHEM_OLD_E_7.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_CHEM_OLD_E_7.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 4, &
     0  &
   + num_chem   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem_old ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem_old ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_INIT_EXCH ( local_communicator, 4 , &
     0  &
   + num_chem   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem_old ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem_old ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
     ELSE
       WRITE(wrf_err_message,*)'solve_em: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
       CALL wrf_error_fatal3 ( "solve_em.b" , 2111 , TRIM(wrf_err_message))
     ENDIF
  endif

  END IF  ! end if for pd_chem

! tke

  IF (config_flags%pd_tke .and. (rk_step == rk_order) &
      .and. (config_flags%km_opt .eq. 2)                ) THEN

   write(6,*) ' pd advection for tke '

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   DO ij = 1 , grid%num_tiles
       CALL wrf_debug ( 200 , ' call rk_update_scalar_pd' )
       CALL rk_update_scalar_pd( 1, 1,                                    &
                                 grid%em_tke_1,                           &
                                 tke_tend(ims,kms,jms),                   &
                                 grid%msft,                               &
                                 grid%em_mu_1, grid%em_mu_1, grid%em_mub, &
                                 rk_step, dt_rk, grid%spec_zone,          &
                                 config_flags,                            &
                                 ids, ide, jds, jde, kds, kde,            &
                                 ims, ime, jms, jme, kms, kme,            &
                                 grid%i_start(ij), grid%i_end(ij),        &
                                 grid%j_start(ij), grid%j_end(ij),        &
                                 k_start    , k_end                      )
   END DO
   !$OMP END PARALLEL DO

!---------------------- positive definite bc call

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )
   DO ij = 1 , grid%num_tiles
          CALL set_physical_bc3d(  grid%em_tke_1, 'p', config_flags,  &
                                   ids, ide, jds, jde, kds, kde,      &
                                   ims, ime, jms, jme, kms, kme,      &
                                   ips, ipe, jps, jpe, kps, kpe,      &
                                   grid%i_start(ij), grid%i_end(ij),  &
                                   grid%j_start(ij), grid%j_end(ij),  &
                                   k_start    , k_end                )
   END DO
   !$OMP END PARALLEL DO

!---  end of positive definite physics tendency update

     IF      ( config_flags%h_sca_adv_order <= 4 ) THEN
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_TKE_OLD_E_5.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_TKE_OLD_E_5.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3, &
     1, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3 , &
     1, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
!ENDOFREGISTRYGENERATEDINCLUDE
     ELSE IF ( config_flags%h_sca_adv_order <= 6 ) THEN
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_TKE_OLD_E_7.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_TKE_OLD_E_7.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 4, &
     1, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 4, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 4, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_INIT_EXCH ( local_communicator, 4 , &
     1, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 4, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 4, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
!ENDOFREGISTRYGENERATEDINCLUDE
     ELSE
       WRITE(wrf_err_message,*)'solve_em: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
       CALL wrf_error_fatal3 ( "solve_em.b" , 2173 , TRIM(wrf_err_message))
     ENDIF

   END IF  ! end if for pd_tke

!
!  Stencils for patch communications  (WCS, 29 June 2001)
!
!
! grid%em_ru_m      x
! grid%em_rv_m      x
! grid%em_ww_m      x
! grid%em_mut       x
!
!--------------------------------------------------------------

!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_D.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_D.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 2, &
     3, 1, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_ru_m, 2, 4, 0, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_rv_m, 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ww_m, 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mut, 2, 4, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ru_m, 2, 4, 0, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_rv_m, 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ww_m, 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mut, 2, 4, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_INIT_EXCH ( local_communicator, 2 , &
     3, 1, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_ru_m, 2, 4, 1, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_rv_m, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ww_m, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mut, 2, 4, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ru_m, 2, 4, 1, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_rv_m, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ww_m, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mut, 2, 4, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
!ENDOFREGISTRYGENERATEDINCLUDE

!<DESCRIPTION>
!<pre>
! (4) Still within the RK loop, the scalar variables are advanced.
!
!    For the moist and chem variables, each one is advanced
!    individually, using named loops "moist_variable_loop:"
!    and "chem_variable_loop:".  Each RK substep begins by
!    calculating the advective tendency, and, for the first RK step, 
!    3D mixing (calling rk_scalar_tend) followed by an update
!    of the scalar (calling rk_scalar_update).
!</pre>
!</DESCRIPTION>


  moist_scalar_advance: IF (num_3d_m >= PARAM_FIRST_SCALAR )  THEN

   moist_variable_loop: do im = PARAM_FIRST_SCALAR, num_3d_m

   if (grid%adv_moist_cond .or. im==p_qv ) then

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   moist_tile_loop_1: DO ij = 1 , grid%num_tiles

       CALL wrf_debug ( 200 , ' call rk_scalar_tend' )



       CALL rk_scalar_tend (  im, im, config_flags,                                      &
                              rk_step, dt_rk,                                            &
                              grid%em_ru_m, grid%em_rv_m, grid%em_ww_m,                  &
                              grid%em_mut, grid%em_mub, grid%em_mu_1,                    &
                              grid%em_alt,                                               &
                              moist_old(ims,kms,jms,im),                                 &
                              moist(ims,kms,jms,im),                                     &
                              moist_tend(ims,kms,jms,im),                                &
                              advect_tend,grid%rqvften,                                  &
                              grid%qv_base, .true., grid%em_fnm, grid%em_fnp,            &
                              grid%msfu, grid%msfv, grid%msft,                           &
                              grid%rdx, grid%rdy, grid%em_rdn, grid%em_rdnw, grid%khdif, &
                              grid%kvdif, grid%xkmhd,                                    &
                              grid%diff_6th_opt, grid%diff_6th_factor,                   &
                              config_flags%pd_moist,            &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start    , k_end               )




     IF( ( config_flags%specified .or. config_flags%nested ) .and. rk_step == 1 ) THEN 
         IF ( im .EQ. P_QV .OR. config_flags%nested ) THEN
            CALL relax_bdy_scalar ( moist_tend(ims,kms,jms,im),            & 
                                    moist(ims,kms,jms,im),  grid%em_mut,         &
                                    moist_b(1,1,1,1,im),                   &
                                    moist_bt(1,1,1,1,im),                  &
                                    config_flags%spec_bdy_width, grid%spec_zone, grid%relax_zone, &
                                    grid%dtbc, grid%fcx, grid%gcx,             &
                                    config_flags,               &
                                    ijds, ijde,                 & ! min/max(id,jd)
                                    ids,ide, jds,jde, kds,kde,  & ! domain dims
                                    ims,ime, jms,jme, kms,kme,  & ! memory dims
                                    ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                    grid%i_start(ij), grid%i_end(ij),      &
                                    grid%j_start(ij), grid%j_end(ij),      &
                                    k_start, k_end                        )

            CALL spec_bdy_scalar  ( moist_tend(ims,kms,jms,im),                &
                                    moist_b(1,1,1,1,im),                     &
                                    moist_bt(1,1,1,1,im),                    &
                                    config_flags%spec_bdy_width, grid%spec_zone,                 &
                                    config_flags,               &
                                    ijds, ijde,                 & ! min/max(id,jd)
                                    ids,ide, jds,jde, kds,kde,  & ! domain dims
                                    ims,ime, jms,jme, kms,kme,  & ! memory dims
                                    ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                    grid%i_start(ij), grid%i_end(ij),          &
                                    grid%j_start(ij), grid%j_end(ij),          &
                                    k_start, k_end                               )
          ENDIF
     ENDIF


   ENDDO moist_tile_loop_1
   !$OMP END PARALLEL DO

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   moist_tile_loop_2: DO ij = 1 , grid%num_tiles

       CALL wrf_debug ( 200 , ' call rk_update_scalar' )


       CALL rk_update_scalar( im, im,                           &
                              moist_old(ims,kms,jms,im),        &
                              moist(ims,kms,jms,im),            &
                              moist_tend(ims,kms,jms,im),       &
                              advect_tend, grid%msft,                &
                              grid%em_mu_1, grid%em_mu_2, grid%em_mub,                  &
                              rk_step, dt_rk, grid%spec_zone,        &
                              config_flags,     &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start    , k_end               )



       IF( config_flags%specified ) THEN
         IF(im .ne. P_QV)THEN
           CALL flow_dep_bdy  (  moist(ims,kms,jms,im),                     &
                               grid%em_ru_m, grid%em_rv_m, config_flags, &
                               grid%spec_zone,                  &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               grid%i_start(ij), grid%i_end(ij),                      &
                               grid%j_start(ij), grid%j_end(ij),                      &
                               k_start, k_end                               )
         ENDIF
       ENDIF


   ENDDO moist_tile_loop_2
   !$OMP END PARALLEL DO

     ENDIF  !-- if (grid%adv_moist_cond .or. im==p_qv ) then

   ENDDO moist_variable_loop

 ENDIF moist_scalar_advance


 TKE_advance: IF (config_flags%km_opt .eq. 2) then

      IF      ( config_flags%h_mom_adv_order <= 4 ) THEN
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_TKE_ADVECT_3.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_TKE_ADVECT_3.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 2, &
     1, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_INIT_EXCH ( local_communicator, 2 , &
     1, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
!ENDOFREGISTRYGENERATEDINCLUDE
      ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_TKE_ADVECT_5.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_TKE_ADVECT_5.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3, &
     1, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3 , &
     1, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
!ENDOFREGISTRYGENERATEDINCLUDE
      ELSE
        WRITE(wrf_err_message,*)'solve_em: invalid h_mom_adv_order = ',config_flags%h_mom_adv_order
        CALL wrf_error_fatal3 ( "solve_em.b" , 2341 , TRIM(wrf_err_message))
      ENDIF

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   tke_tile_loop_1: DO ij = 1 , grid%num_tiles

     CALL wrf_debug ( 200 , ' call rk_scalar_tend for tke' )
     CALL rk_scalar_tend ( 1, 1, config_flags,                                        &
                           rk_step, dt_rk,                                            &
                           grid%em_ru_m, grid%em_rv_m, grid%em_ww_m,                  &
                           grid%em_mut, grid%em_mub, grid%em_mu_1,                    &
                           grid%em_alt,                                               &
                           grid%em_tke_1,                                             &
                           grid%em_tke_2,                                             &
                           tke_tend(ims,kms,jms),                                     &
                           advect_tend,grid%rqvften,                                  &
                           grid%qv_base, .false., grid%em_fnm, grid%em_fnp,           &
                           grid%msfu, grid%msfv, grid%msft,                           &
                           grid%rdx, grid%rdy, grid%em_rdn, grid%em_rdnw, grid%khdif, &
                           grid%kvdif, grid%xkmhd,                                    &
                           grid%diff_6th_opt, grid%diff_6th_factor,                   &
                           config_flags%pd_tke,              &
                           ids, ide, jds, jde, kds, kde,     &
                           ims, ime, jms, jme, kms, kme,     &
                           grid%i_start(ij), grid%i_end(ij), &
                           grid%j_start(ij), grid%j_end(ij), &
                           k_start    , k_end               )

   ENDDO tke_tile_loop_1
   !$OMP END PARALLEL DO

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   tke_tile_loop_2: DO ij = 1 , grid%num_tiles

     CALL wrf_debug ( 200 , ' call rk_update_scalar' )
     CALL rk_update_scalar( 1, 1,                             &
                            grid%em_tke_1,               &
                            grid%em_tke_2,               &
                            tke_tend(ims,kms,jms),            &
                            advect_tend,grid%msft,                 &
                            grid%em_mu_1, grid%em_mu_2, grid%em_mub,                  &
                            rk_step, dt_rk, grid%spec_zone,        &
                            config_flags,     &
                            ids, ide, jds, jde, kds, kde,     &
                            ims, ime, jms, jme, kms, kme,     &
                            grid%i_start(ij), grid%i_end(ij), &
                            grid%j_start(ij), grid%j_end(ij), &
                            k_start    , k_end               ) 

! bound the tke (greater than 0, less than tke_upper_bound)

     CALL bound_tke( grid%em_tke_2, grid%tke_upper_bound, &
                     ids, ide, jds, jde, kds, kde,        &
                     ims, ime, jms, jme, kms, kme,        &
                     grid%i_start(ij), grid%i_end(ij),    &
                     grid%j_start(ij), grid%j_end(ij),    &
                     k_start    , k_end                  )

     IF( config_flags%specified .or. config_flags%nested ) THEN
         CALL flow_dep_bdy (  grid%em_tke_2,                     &
                              grid%em_ru_m, grid%em_rv_m, config_flags,               &
                              grid%spec_zone,                              &
                              ids,ide, jds,jde, kds,kde,  & ! domain dims
                              ims,ime, jms,jme, kms,kme,  & ! memory dims
                              ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                              grid%i_start(ij), grid%i_end(ij),       &
                              grid%j_start(ij), grid%j_end(ij),       &
                              k_start, k_end                               )
     ENDIF
   ENDDO tke_tile_loop_2
   !$OMP END PARALLEL DO

   END IF TKE_advance



!  next the other scalar species
  other_scalar_advance: IF (num_3d_s >= PARAM_FIRST_SCALAR)  THEN

   scalar_variable_loop: do is = PARAM_FIRST_SCALAR, num_3d_s
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   scalar_tile_loop_1: DO ij = 1 , grid%num_tiles

       CALL wrf_debug ( 200 , ' call rk_scalar_tend' )
       CALL rk_scalar_tend ( is, is, config_flags,                            &
                             rk_step, dt_rk,                                  &
                             grid%em_ru_m, grid%em_rv_m, grid%em_ww_m,        &
                             grid%em_mut, grid%em_mub, grid%em_mu_1,          &
                             grid%em_alt,                                     &
                             scalar_old(ims,kms,jms,is),                      &
                             scalar(ims,kms,jms,is),                          &
                             scalar_tend(ims,kms,jms,is),                     &
                             advect_tend,grid%rqvften,                        &
                             grid%qv_base, .false., grid%em_fnm, grid%em_fnp, &
                             grid%msfu, grid%msfv, grid%msft,                 &
                             grid%rdx, grid%rdy, grid%em_rdn, grid%em_rdnw,   &
                             grid%khdif, grid%kvdif, grid%xkmhd,              &
                             grid%diff_6th_opt, grid%diff_6th_factor,         &
                             config_flags%pd_scalar,           &
                             ids, ide, jds, jde, kds, kde,     &
                             ims, ime, jms, jme, kms, kme,     &
                             grid%i_start(ij), grid%i_end(ij), &
                             grid%j_start(ij), grid%j_end(ij), &
                             k_start    , k_end               )

     IF( config_flags%nested .and. (rk_step == 1) ) THEN

       IF (is .eq. P_QNI) THEN

         CALL relax_bdy_scalar ( scalar_tend(ims,kms,jms,is),            &
                                 scalar(ims,kms,jms,is),  grid%em_mut,         &
                                 scalar_b(1,1,1,1,is),                   &
                                 scalar_bt(1,1,1,1,is),                  &
                                 config_flags%spec_bdy_width, grid%spec_zone, grid%relax_zone, &
                                 grid%dtbc, grid%fcx, grid%gcx,             &
                                 config_flags,               &
                                 ijds, ijde,                 & ! min/max(id,jd)
                                 ids,ide, jds,jde, kds,kde,  & ! domain dims
                                 ims,ime, jms,jme, kms,kme,  & ! memory dims
                                 ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                 grid%i_start(ij), grid%i_end(ij),      &
                                 grid%j_start(ij), grid%j_end(ij),      &
                                 k_start, k_end                        )

         CALL spec_bdy_scalar  ( scalar_tend(ims,kms,jms,is),                &
                                 scalar_b(1,1,1,1,is),                   &
                                 scalar_bt(1,1,1,1,is),                  &
                                 config_flags%spec_bdy_width, grid%spec_zone,                 &
                                 config_flags,               &
                                 ijds, ijde,                 & ! min/max(id,jd)
                                 ids,ide, jds,jde, kds,kde,  & ! domain dims
                                 ims,ime, jms,jme, kms,kme,  & ! memory dims
                                 ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                 grid%i_start(ij), grid%i_end(ij),          &
                                 grid%j_start(ij), grid%j_end(ij),          &
                                 k_start, k_end                               )

       ENDIF

     ENDIF ! b.c test for chem nested boundary condition

   ENDDO scalar_tile_loop_1
   !$OMP END PARALLEL DO


   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   scalar_tile_loop_2: DO ij = 1 , grid%num_tiles

       CALL wrf_debug ( 200 , ' call rk_update_scalar' )
       CALL rk_update_scalar( is, is,                           &
                              scalar_old(ims,kms,jms,is),       &  ! was scalar_1
                              scalar(ims,kms,jms,is),           &
                              scalar_tend(ims,kms,jms,is),      &
                              advect_tend, grid%msft,                &
                              grid%em_mu_1, grid%em_mu_2, grid%em_mub,                  &
                              rk_step, dt_rk, grid%spec_zone,        &
                              config_flags,     &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start    , k_end               )


       IF( config_flags%specified ) THEN
           CALL flow_dep_bdy  ( scalar(ims,kms,jms,is),     &
                                grid%em_ru_m, grid%em_rv_m, config_flags,   &
                                grid%spec_zone,                  &
                                ids,ide, jds,jde, kds,kde,  & ! domain dims
                                ims,ime, jms,jme, kms,kme,  & ! memory dims
                                ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                grid%i_start(ij), grid%i_end(ij),  &
                                grid%j_start(ij), grid%j_end(ij),  &
                                k_start, k_end                    )
       ENDIF


   ENDDO scalar_tile_loop_2
   !$OMP END PARALLEL DO

   ENDDO scalar_variable_loop

 ENDIF other_scalar_advance

 !  update the pressure and density at the new time level

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )
   DO ij = 1 , grid%num_tiles



     CALL calc_p_rho_phi( moist, num_3d_m,                &
                          grid%em_al, grid%em_alb, grid%em_mu_2, grid%em_muts,              &
                          grid%em_ph_2, grid%em_p, grid%em_pb, grid%em_t_2,                 &
                          p0, t0, grid%em_znu, grid%em_dnw, grid%em_rdnw,           &
                          grid%em_rdn, config_flags%non_hydrostatic,             &
                          ids, ide, jds, jde, kds, kde,     &
                          ims, ime, jms, jme, kms, kme,     &
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          k_start    , k_end               )



   ENDDO
   !$OMP END PARALLEL DO

!  Reset the boundary conditions if there is another corrector step.
!  (rk_step < rk_order), else well handle it at the end of everything
!  (after the split physics, before exiting the timestep).

   rk_step_1_check: IF ( rk_step < rk_order ) THEN

!-----------------------------------------------------------
!  Stencils for patch communications  (WCS, 29 June 2001)
!
!  heres where we need a wide comm stencil - these are the 
!  uncoupled variables so are used for high order calc in
!  advection and mixong routines.
!
!                              * * * * *
!            *        * * *    * * * * *
!          * + *      * + *    * * + * * 
!            *        * * *    * * * * *
!                              * * * * *
!
!
! grid%em_u_2                              x
! grid%em_v_2                              x
! grid%em_w_2                              x
! grid%em_t_2                              x
! grid%em_ph_2                             x
! grid%em_al         x
!
!  2D variable
! grid%em_mu_2       x
!
!  4D variable
! moist               x
! chem                x
!scalar               x

   IF      ( config_flags%h_mom_adv_order <= 4 ) THEN
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_D2_3.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_D2_3.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 2, &
     6  &
   + num_moist   &
   + num_chem   &
   + num_scalar   &
     , 1, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_2, 2, 4, 0, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_2, 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_w_2, 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_2, 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_2, 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_2, 2, 4, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_al, 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_2, 2, 4, 0, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_2, 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_w_2, 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_2, 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_2, 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_2, 2, 4, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_al, 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_INIT_EXCH ( local_communicator, 2 , &
     6  &
   + num_moist   &
   + num_chem   &
   + num_scalar   &
     , 1, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_2, 2, 4, 1, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_2, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_w_2, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_2, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_2, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_2, 2, 4, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_al, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_2, 2, 4, 1, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_2, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_w_2, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_2, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_2, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_2, 2, 4, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_al, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
!ENDOFREGISTRYGENERATEDINCLUDE
   ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_D2_5.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_D2_5.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3, &
     6  &
   + num_moist   &
   + num_chem   &
   + num_scalar   &
     , 1, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_2, 3, 4, 0, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_2, 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_w_2, 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_2, 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_2, 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_2, 3, 4, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_al, 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_2, 3, 4, 0, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_2, 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_w_2, 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_2, 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_2, 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_2, 3, 4, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_al, 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3 , &
     6  &
   + num_moist   &
   + num_chem   &
   + num_scalar   &
     , 1, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_2, 3, 4, 1, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_2, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_w_2, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_2, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_2, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_2, 3, 4, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_al, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_2, 3, 4, 1, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_2, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_w_2, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_2, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_2, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_2, 3, 4, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_al, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
!ENDOFREGISTRYGENERATEDINCLUDE
   ELSE 
     WRITE(wrf_err_message,*)'solve_em: invalid h_mom_adv_order = ',config_flags%h_mom_adv_order
     CALL wrf_error_fatal3 ( "solve_em.b" , 2735 , TRIM(wrf_err_message))
   ENDIF
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/PERIOD_BDY_EM_D.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_D.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 3 , &
     6, 1, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_u_2, 3, 4, 1, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_v_2, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_w_2, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_t_2, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_ph_2, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_mu_2, 3, 4, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_tke_2, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_u_2, 3, 4, 1, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_v_2, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_w_2, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_t_2, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_ph_2, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_mu_2, 3, 4, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_tke_2, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
END IF
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/PERIOD_BDY_EM_MOIST2.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_MOIST2.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 3 , &
     0  &
   + num_moist   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
END IF
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/PERIOD_BDY_EM_CHEM2.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_CHEM2.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 3 , &
     0  &
   + num_chem   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
END IF
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/PERIOD_BDY_EM_SCALAR2.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_SCALAR2.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 3 , &
     0  &
   + num_scalar   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
END IF
!ENDOFREGISTRYGENERATEDINCLUDE


   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

    tile_bc_loop_1: DO ij = 1 , grid%num_tiles


      CALL wrf_debug ( 200 , ' call rk_phys_bc_dry_2' )

      CALL rk_phys_bc_dry_2( config_flags,                         &
                             grid%em_u_2, grid%em_v_2, grid%em_w_2,                    &
                             grid%em_t_2, grid%em_ph_2, grid%em_mu_2,                  &
                             ids, ide, jds, jde, kds, kde,     &
                             ims, ime, jms, jme, kms, kme,     &
                             ips, ipe, jps, jpe, kps, kpe,     &
                             grid%i_start(ij), grid%i_end(ij), &
                             grid%j_start(ij), grid%j_end(ij), &
                             k_start    , k_end               )


     IF (.not. config_flags%non_hydrostatic) THEN
     CALL diagnose_w( ph_tend, grid%em_ph_2, grid%em_ph_1, grid%em_w_2, grid%em_muts, dt_rk,  &
                      grid%em_u_2, grid%em_v_2, grid%ht,                           &
                      grid%cf1, grid%cf2, grid%cf3, grid%rdx, grid%rdy, grid%msft,          &
                      ids, ide, jds, jde, kds, kde,           &
                      ims, ime, jms, jme, kms, kme,           &
                      grid%i_start(ij), grid%i_end(ij),       &
                      grid%j_start(ij), grid%j_end(ij),       &
                      k_start    , k_end                     )
     ENDIF


      IF (num_3d_m >= PARAM_FIRST_SCALAR) THEN

        moisture_loop_bdy_1 : DO im = PARAM_FIRST_SCALAR , num_3d_m
  
          CALL set_physical_bc3d( moist(ims,kms,jms,im), 'grid%em_p', config_flags,   &
                                   ids, ide, jds, jde, kds, kde,             &
                                   ims, ime, jms, jme, kms, kme,             &
                                   ips, ipe, jps, jpe, kps, kpe,             &
                                   grid%i_start(ij), grid%i_end(ij),                   &
                                   grid%j_start(ij), grid%j_end(ij),                   &
                                   k_start    , k_end                       )
         END DO moisture_loop_bdy_1

      ENDIF

      IF (num_3d_c >= PARAM_FIRST_SCALAR) THEN

        chem_species_bdy_loop_1 : DO ic = PARAM_FIRST_SCALAR , num_3d_c

          CALL set_physical_bc3d( chem(ims,kms,jms,ic), 'grid%em_p', config_flags,   &
                                  ids, ide, jds, jde, kds, kde,            &
                                  ims, ime, jms, jme, kms, kme,            &
                                  ips, ipe, jps, jpe, kps, kpe,            &
                                  grid%i_start(ij), grid%i_end(ij),                  &
                                  grid%j_start(ij), grid%j_end(ij),                  &
                                  k_start    , k_end-1                    )

        END DO chem_species_bdy_loop_1

      END IF

      IF (num_3d_s >= PARAM_FIRST_SCALAR) THEN

        scalar_species_bdy_loop_1 : DO is = PARAM_FIRST_SCALAR , num_3d_s

          CALL set_physical_bc3d( scalar(ims,kms,jms,is), 'grid%em_p', config_flags,   &
                                  ids, ide, jds, jde, kds, kde,            &
                                  ims, ime, jms, jme, kms, kme,            &
                                  ips, ipe, jps, jpe, kps, kpe,            &
                                  grid%i_start(ij), grid%i_end(ij),                  &
                                  grid%j_start(ij), grid%j_end(ij),                  &
                                  k_start    , k_end-1                    )

        END DO scalar_species_bdy_loop_1

      END IF

      IF (config_flags%km_opt .eq. 2) THEN

        CALL set_physical_bc3d( grid%em_tke_2 , 'grid%em_p', config_flags,  &
                                ids, ide, jds, jde, kds, kde,            &
                                ims, ime, jms, jme, kms, kme,            &
                                ips, ipe, jps, jpe, kps, kpe,            &
                                grid%i_start(ij), grid%i_end(ij),        &
                                grid%j_start(ij), grid%j_end(ij),        &
                                k_start    , k_end                      )
      END IF

    END DO tile_bc_loop_1
   !$OMP END PARALLEL DO




!                           * * * * *
!         *        * * *    * * * * *
!       * + *      * + *    * * + * *
!         *        * * *    * * * * *
!                           * * * * *

! moist, chem, scalar, tke      x


      IF      ( config_flags%h_mom_adv_order <= 4 ) THEN
        IF ( (config_flags%pd_tke) .and. (rk_step == rk_order-1) ) THEN
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_TKE_5.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_TKE_5.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3, &
     2, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3 , &
     2, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
!ENDOFREGISTRYGENERATEDINCLUDE
        ELSE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_TKE_3.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_TKE_3.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 2, &
     2, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_INIT_EXCH ( local_communicator, 2 , &
     2, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
!ENDOFREGISTRYGENERATEDINCLUDE
        ENDIF
      ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN
        IF ( (config_flags%pd_tke) .and. (rk_step == rk_order-1) ) THEN
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_TKE_7.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_TKE_7.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 4, &
     2, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 4, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 4, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 4, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 4, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_INIT_EXCH ( local_communicator, 4 , &
     2, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 4, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 4, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 4, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 4, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
!ENDOFREGISTRYGENERATEDINCLUDE
        ELSE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_TKE_5.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_TKE_5.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3, &
     2, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3 , &
     2, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
!ENDOFREGISTRYGENERATEDINCLUDE
        ENDIF
      ELSE
        WRITE(wrf_err_message,*)'solve_em: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
        CALL wrf_error_fatal3 ( "solve_em.b" , 2863 , TRIM(wrf_err_message))
      ENDIF


   if ( num_moist .ge. PARAM_FIRST_SCALAR ) then
     IF      ( config_flags%h_sca_adv_order <= 4 ) THEN
       IF ( (config_flags%pd_moist) .and. (rk_step == rk_order-1) ) THEN
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_MOIST_E_5.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_MOIST_E_5.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3, &
     0  &
   + num_moist   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3 , &
     0  &
   + num_moist   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
       ELSE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_MOIST_E_3.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_MOIST_E_3.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 2, &
     0  &
   + num_moist   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_INIT_EXCH ( local_communicator, 2 , &
     0  &
   + num_moist   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
       END IF
     ELSE IF ( config_flags%h_sca_adv_order <= 6 ) THEN
       IF ( (config_flags%pd_moist) .and. (rk_step == rk_order-1) ) THEN
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_MOIST_E_7.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_MOIST_E_7.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 4, &
     0  &
   + num_moist   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_INIT_EXCH ( local_communicator, 4 , &
     0  &
   + num_moist   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
       ELSE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_MOIST_E_5.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_MOIST_E_5.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3, &
     0  &
   + num_moist   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3 , &
     0  &
   + num_moist   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
       END IF
     ELSE
       WRITE(wrf_err_message,*)'solve_em: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
       CALL wrf_error_fatal3 ( "solve_em.b" , 2887 , TRIM(wrf_err_message))
     ENDIF
   endif
   if ( num_chem >= PARAM_FIRST_SCALAR ) then
     IF      ( config_flags%h_sca_adv_order <= 4 ) THEN
       IF ( (config_flags%pd_chem) .and. (rk_step == rk_order-1) ) THEN
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_CHEM_E_5.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_CHEM_E_5.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3, &
     0  &
   + num_chem   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3 , &
     0  &
   + num_chem   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
       ELSE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_CHEM_E_3.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_CHEM_E_3.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 2, &
     0  &
   + num_chem   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_INIT_EXCH ( local_communicator, 2 , &
     0  &
   + num_chem   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
       END IF
     ELSE IF ( config_flags%h_sca_adv_order <= 6 ) THEN
       IF ( (config_flags%pd_chem) .and. (rk_step == rk_order-1) ) THEN
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_CHEM_E_7.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_CHEM_E_7.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 4, &
     0  &
   + num_chem   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_INIT_EXCH ( local_communicator, 4 , &
     0  &
   + num_chem   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
       ELSE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_CHEM_E_5.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_CHEM_E_5.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3, &
     0  &
   + num_chem   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3 , &
     0  &
   + num_chem   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
       END IF
     ELSE
       WRITE(wrf_err_message,*)'solve_em: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
       CALL wrf_error_fatal3 ( "solve_em.b" , 2905 , TRIM(wrf_err_message))
     ENDIF
   endif
   if ( num_scalar >= PARAM_FIRST_SCALAR ) then
     IF      ( config_flags%h_sca_adv_order <= 4 ) THEN
       IF ( (config_flags%pd_scalar) .and. (rk_step == rk_order-1) ) THEN
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_SCALAR_E_5.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_SCALAR_E_5.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3, &
     0  &
   + num_scalar   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3 , &
     0  &
   + num_scalar   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
       ELSE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_SCALAR_E_3.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_SCALAR_E_3.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 2, &
     0  &
   + num_scalar   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_INIT_EXCH ( local_communicator, 2 , &
     0  &
   + num_scalar   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
       END IF
     ELSE IF ( config_flags%h_sca_adv_order <= 6 ) THEN
       IF ( (config_flags%pd_scalar) .and. (rk_step == rk_order-1) ) THEN
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_SCALAR_E_7.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_SCALAR_E_7.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 4, &
     0  &
   + num_scalar   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_INIT_EXCH ( local_communicator, 4 , &
     0  &
   + num_scalar   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
       ELSE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_SCALAR_E_5.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_SCALAR_E_5.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3, &
     0  &
   + num_scalar   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3 , &
     0  &
   + num_scalar   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
       END IF
     ELSE
       WRITE(wrf_err_message,*)'solve_em: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
       CALL wrf_error_fatal3 ( "solve_em.b" , 2923 , TRIM(wrf_err_message))
     ENDIF
   endif

   ENDIF rk_step_1_check


!**********************************************************
!
!  end of RK predictor-corrector loop
!
!**********************************************************

 END DO Runge_Kutta_loop

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   DO ij = 1 , grid%num_tiles


      CALL wrf_debug ( 200 , ' call advance_ppt' )
      CALL advance_ppt(grid%rthcuten,grid%rqvcuten,grid%rqccuten,grid%rqrcuten, &
                     grid%rqicuten,grid%rqscuten,grid%rainc,grid%raincv,grid%nca,    &
                     grid%htop,grid%hbot,grid%cutop,grid%cubot,                 &
                     grid%cuppt, config_flags,                   &
                     ids,ide, jds,jde, kds,kde,             &
                     ims,ime, jms,jme, kms,kme,             &
                     grid%i_start(ij), grid%i_end(ij),      &
                     grid%j_start(ij), grid%j_end(ij),      &
                     k_start    , k_end                    )


   ENDDO
   !$OMP END PARALLEL DO

!<DESCRIPTION>
!<pre>
! (5) time-split physics.
!
!     Microphysics are the only time  split physics in the WRF model 
!     at this time.  Split-physics begins with the calculation of
!     needed diagnostic quantities (pressure, temperature, etc.)
!     followed by a call to the microphysics driver, 
!     and finishes with a clean-up, storing off of a diabatic tendency
!     from the moist physics, and a re-calulation of the  diagnostic
!     quantities pressure and density.
!</pre>
!</DESCRIPTION>

  IF (config_flags%mp_physics /= 0)  then

   IF( config_flags%specified .or. config_flags%nested ) THEN
     sz = grid%spec_zone
   ELSE
     sz = 0
   ENDIF

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij, its, ite, jts, jte )

   scalar_tile_loop_1a: DO ij = 1 , grid%num_tiles

       IF ( config_flags%periodic_x ) THEN
         its = max(grid%i_start(ij),ids)
         ite = min(grid%i_end(ij),ide-1)
       ELSE
         its = max(grid%i_start(ij),ids+sz)
         ite = min(grid%i_end(ij),ide-1-sz)
       ENDIF
       jts = max(grid%j_start(ij),jds+sz)
       jte = min(grid%j_end(ij),jde-1-sz)

       CALL wrf_debug ( 200 , ' call moist_physics_prep' )

       CALL moist_physics_prep_em( grid%em_t_2, grid%em_t_1, t0, rho,                &
                                   grid%em_al, grid%em_alb, grid%em_p, p8w, p0, grid%em_pb,          &
                                   grid%em_ph_2, grid%em_phb, th_phy, pi_phy, p_phy, &
                                   grid%em_z, z_at_w, dz8w,                  &
                                   dtm, grid%h_diabatic,                  &
                                   config_flags,grid%em_fnm, grid%em_fnp,            &
                                   ids, ide, jds, jde, kds, kde,     &
                                   ims, ime, jms, jme, kms, kme,     &
                                   its, ite, jts, jte,               &
                                   k_start    , k_end               )

   END DO scalar_tile_loop_1a
   !$OMP END PARALLEL DO

       CALL wrf_debug ( 200 , ' call microphysics_driver' )

       grid%em_sr = 0.
       specified_bdy = config_flags%specified .OR. config_flags%nested
       channel_bdy = config_flags%specified .AND. config_flags%periodic_x




       CALL microphysics_driver(                                          &
     &         DT=dtm             ,DX=grid%dx              ,DY=grid%dy              &
     &        ,DZ8W=dz8w          ,F_ICE_PHY=grid%f_ice_phy                    &
     &        ,ITIMESTEP=grid%itimestep                    ,LOWLYR=grid%lowlyr      &
     &        ,P8W=p8w            ,P=p_phy            ,PI_PHY=pi_phy      &
     &        ,RHO=rho            ,SPEC_ZONE=grid%spec_zone                    &
     &        ,SR=grid%em_sr              ,TH=th_phy                              &
     &        ,WARM_RAIN=grid%warm_rain                    ,XLAND=grid%xland        &
     &        ,SPECIFIED=specified_bdy, CHANNEL_SWITCH=channel_bdy        &
     &        ,F_RAIN_PHY=grid%f_rain_phy                                      &
     &        ,F_RIMEF_PHY=grid%f_rimef_phy                                    &
     &        ,MP_PHYSICS=config_flags%mp_physics                         &
     &        ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde          &
     &        ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme          &
     &        ,I_START=grid%i_start,I_END=min(grid%i_end, ide-1)          &
     &        ,J_START=grid%j_start,J_END=min(grid%j_end, jde-1)          &
     &        ,KTS=k_start, KTE=min(k_end,kde-1)                          &
     &        ,NUM_TILES=grid%num_tiles                                   &
                 ! Optional
     &        , RAINNC=grid%rainnc, RAINNCV=grid%rainncv                            &
     &        , SNOWNC=grid%snownc, SNOWNCV=grid%snowncv                            &
     &        , GRAUPELNC=grid%graupelnc, GRAUPELNCV=grid%graupelncv                &
     &        , W=grid%em_w_2, Z=grid%em_z, HT=grid%ht                                         &
     &        , MP_RESTART_STATE=grid%mp_restart_state                         &
     &        , TBPVS_STATE=grid%tbpvs_state                                   & ! etampnew
     &        , TBPVS0_STATE=grid%tbpvs0_state                                 & ! etampnew
     &        , QV_CURR=moist(ims,kms,jms,P_QV), F_QV=F_QV              &
     &        , QC_CURR=moist(ims,kms,jms,P_QC), F_QC=F_QC              &
     &        , QR_CURR=moist(ims,kms,jms,P_QR), F_QR=F_QR              &
     &        , QI_CURR=moist(ims,kms,jms,P_QI), F_QI=F_QI              &
     &        , QS_CURR=moist(ims,kms,jms,P_QS), F_QS=F_QS              &
     &        , QG_CURR=moist(ims,kms,jms,P_QG), F_QG=F_QG              &
     &        , QNI_CURR=scalar(ims,kms,jms,P_QNI), F_QNI=F_QNI         &
     &        , QT_CURR=scalar(ims,kms,jms,P_QT), F_QT=F_QT             &
                                                                          )



       CALL wrf_debug ( 200 , ' call moist_physics_finish' )

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij, its, ite, jts, jte )

   scalar_tile_loop_1b: DO ij = 1 , grid%num_tiles

       IF ( config_flags%periodic_x ) THEN
         its = max(grid%i_start(ij),ids)
         ite = min(grid%i_end(ij),ide-1)
       ELSE
         its = max(grid%i_start(ij),ids+sz)
         ite = min(grid%i_end(ij),ide-1-sz)
       ENDIF
       jts = max(grid%j_start(ij),jds+sz)
       jte = min(grid%j_end(ij),jde-1-sz)

       CALL microphysics_zero_out (                                    &
                     moist , num_moist , config_flags ,              &
                     ids, ide, jds, jde, kds, kde,                     &
                     ims, ime, jms, jme, kms, kme,                     &
                     its, ite, jts, jte,                               &
                     k_start    , k_end                                )

       CALL moist_physics_finish_em( grid%em_t_2, grid%em_t_1, t0, grid%em_muts, th_phy,       &
                                     grid%h_diabatic, dtm, config_flags,    &
                                     ids, ide, jds, jde, kds, kde,     &
                                     ims, ime, jms, jme, kms, kme,     &
                                     its, ite, jts, jte,               &
                                     k_start    , k_end               )

       CALL calc_p_rho_phi( moist, num_3d_m,                &
                            grid%em_al, grid%em_alb, grid%em_mu_2, grid%em_muts,              &
                            grid%em_ph_2, grid%em_p, grid%em_pb, grid%em_t_2,                 &
                            p0, t0, grid%em_znu, grid%em_dnw, grid%em_rdnw,           &
                            grid%em_rdn, config_flags%non_hydrostatic,             &
                            ids, ide, jds, jde, kds, kde,     &
                            ims, ime, jms, jme, kms, kme,     &
                            its, ite, jts, jte,               &
                            k_start    , k_end               )

   END DO scalar_tile_loop_1b
   !$OMP END PARALLEL DO


  ENDIF

  IF (.not. config_flags%non_hydrostatic) THEN
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )
   DO ij = 1 , grid%num_tiles
     CALL diagnose_w( ph_tend, grid%em_ph_2, grid%em_ph_1, grid%em_w_2, grid%em_muts, dt_rk,  &
                      grid%em_u_2, grid%em_v_2, grid%ht,                           &
                      grid%cf1, grid%cf2, grid%cf3, grid%rdx, grid%rdy, grid%msft,          &
                      ids, ide, jds, jde, kds, kde,           &
                      ims, ime, jms, jme, kms, kme,           &
                      grid%i_start(ij), grid%i_end(ij),       &
                      grid%j_start(ij), grid%j_end(ij),       &
                      k_start    , k_end                     )

   END DO
   !$OMP END PARALLEL DO
  ENDIF

   chem_tile_loop_3: DO ij = 1 , grid%num_tiles

          CALL wrf_debug ( 200 , ' call scalar_tile_loop_2' )

     IF ( num_3d_c >= PARAM_FIRST_SCALAR ) then

!
!  tiled chemistry not here, it is called from solve_interface, and found in chem_driver
!

     END IF

   END DO chem_tile_loop_3


   !  Were finished except for boundary condition (and patch) update

   ! Boundary condition time (or communication time).  At this time, we have
   ! implemented periodic and symmetric physical boundary conditions.

   ! b.c. routine for data within patch.

   ! we need to do both time levels of 
   ! data because the time filter only works in the physical solution space.

   ! First, do patch communications for boundary conditions (periodicity)

!-----------------------------------------------------------
!  Stencils for patch communications  (WCS, 29 June 2001)
!
!  heres where we need a wide comm stencil - these are the 
!  uncoupled variables so are used for high order calc in
!  advection and mixong routines.
!
!                              * * * * *
!            *        * * *    * * * * *
!          * + *      * + *    * * + * * 
!            *        * * *    * * * * *
!                              * * * * *
!
!   grid%em_u_1                            x
!   grid%em_u_2                            x
!   grid%em_v_1                            x
!   grid%em_v_2                            x
!   grid%em_w_1                            x
!   grid%em_w_2                            x
!   grid%em_t_1                            x
!   grid%em_t_2                            x
!  grid%em_ph_1                            x
!  grid%em_ph_2                            x
!  grid%em_tke_1                           x
!  grid%em_tke_2                           x
!
!    2D variables
!  grid%em_mu_1     x
!  grid%em_mu_2     x
!
!    4D variables
!  moist                         x
!   chem                         x
! scalar                         x
!----------------------------------------------------------


   IF      ( config_flags%h_mom_adv_order <= 4 ) THEN
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_D3_3.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_D3_3.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 2, &
     12  &
   + num_moist   &
   + num_chem   &
   + num_scalar   &
     , 2, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_1, 2, 4, 0, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_2, 2, 4, 0, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_1, 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_2, 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_w_1, 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_w_2, 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_1, 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_2, 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_1, 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_2, 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_1, 2, 4, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_2, 2, 4, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_1, 2, 4, 0, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_2, 2, 4, 0, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_1, 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_2, 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_w_1, 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_w_2, 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_1, 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_2, 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_1, 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_2, 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_1, 2, 4, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_2, 2, 4, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_INIT_EXCH ( local_communicator, 2 , &
     12  &
   + num_moist   &
   + num_chem   &
   + num_scalar   &
     , 2, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_1, 2, 4, 1, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_2, 2, 4, 1, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_1, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_2, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_w_1, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_w_2, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_1, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_2, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_1, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_2, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_1, 2, 4, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_2, 2, 4, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_1, 2, 4, 1, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_2, 2, 4, 1, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_1, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_2, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_w_1, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_w_2, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_1, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_2, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_1, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_2, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_1, 2, 4, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_2, 2, 4, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
!ENDOFREGISTRYGENERATEDINCLUDE
   ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_D3_5.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_D3_5.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3, &
     12  &
   + num_moist   &
   + num_chem   &
   + num_scalar   &
     , 2, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_1, 3, 4, 0, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_2, 3, 4, 0, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_1, 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_2, 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_w_1, 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_w_2, 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_1, 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_2, 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_1, 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_2, 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_1, 3, 4, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_2, 3, 4, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_1, 3, 4, 0, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_2, 3, 4, 0, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_1, 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_2, 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_w_1, 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_w_2, 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_1, 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_2, 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_1, 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_2, 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_1, 3, 4, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_2, 3, 4, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3 , &
     12  &
   + num_moist   &
   + num_chem   &
   + num_scalar   &
     , 2, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_1, 3, 4, 1, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_2, 3, 4, 1, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_1, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_2, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_w_1, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_w_2, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_1, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_2, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_1, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_2, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_1, 3, 4, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_2, 3, 4, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_1, 3, 4, 1, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_2, 3, 4, 1, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_1, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_2, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_w_1, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_w_2, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_1, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_2, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_1, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_2, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_1, 3, 4, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_2, 3, 4, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
!ENDOFREGISTRYGENERATEDINCLUDE
   ELSE 
     WRITE(wrf_err_message,*)'solve_em: invalid h_mom_adv_order = ',config_flags%h_mom_adv_order
     CALL wrf_error_fatal3 ( "solve_em.b" , 3215 , TRIM(wrf_err_message))
   ENDIF
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/PERIOD_BDY_EM_D3.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_D3.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 3 , &
     12, 2, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_u_1, 3, 4, 1, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_u_2, 3, 4, 1, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_v_1, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_v_2, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_w_1, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_w_2, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_t_1, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_t_2, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_ph_1, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_ph_2, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_tke_1, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_tke_2, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_mu_1, 3, 4, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_mu_2, 3, 4, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_u_1, 3, 4, 1, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_u_2, 3, 4, 1, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_v_1, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_v_2, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_w_1, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_w_2, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_t_1, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_t_2, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_ph_1, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_ph_2, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_tke_1, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_tke_2, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_mu_1, 3, 4, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_mu_2, 3, 4, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
END IF
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/PERIOD_BDY_EM_MOIST.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_MOIST.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 3 , &
     0  &
   + num_moist   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
END IF
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/PERIOD_BDY_EM_CHEM.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_CHEM.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 3 , &
     0  &
   + num_chem   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
END IF
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/PERIOD_BDY_EM_SCALAR.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_SCALAR.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 3 , &
     0  &
   + num_scalar   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
END IF
!ENDOFREGISTRYGENERATEDINCLUDE

!  now set physical b.c on a patch


   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   tile_bc_loop_2: DO ij = 1 , grid%num_tiles


     CALL wrf_debug ( 200 , ' call set_phys_bc_dry_2' )

     CALL set_phys_bc_dry_2( config_flags,                           &
                             grid%em_u_1, grid%em_u_2, grid%em_v_1, grid%em_v_2, grid%em_w_1, grid%em_w_2,           &
                             grid%em_t_1, grid%em_t_2, grid%em_ph_1, grid%em_ph_2, grid%em_mu_1, grid%em_mu_2,       &
                             ids, ide, jds, jde, kds, kde,           &
                             ims, ime, jms, jme, kms, kme,           &
                             ips, ipe, jps, jpe, kps, kpe,           &
                             grid%i_start(ij), grid%i_end(ij),       &
                             grid%j_start(ij), grid%j_end(ij),       &
                             k_start    , k_end                     )

     CALL set_physical_bc3d( grid%em_tke_1, 'grid%em_p', config_flags,   &
                             ids, ide, jds, jde, kds, kde,            &
                             ims, ime, jms, jme, kms, kme,            &
                             ips, ipe, jps, jpe, kps, kpe,            &
                             grid%i_start(ij), grid%i_end(ij),        &
                             grid%j_start(ij), grid%j_end(ij),        &
                             k_start    , k_end-1                    )
     CALL set_physical_bc3d( grid%em_tke_2 , 'grid%em_p', config_flags,  &
                             ids, ide, jds, jde, kds, kde,            &
                             ims, ime, jms, jme, kms, kme,            &
                             ips, ipe, jps, jpe, kps, kpe,            &
                             grid%i_start(ij), grid%i_end(ij),        &
                             grid%j_start(ij), grid%j_end(ij),        &
                             k_start    , k_end                      )

     moisture_loop_bdy_2 : DO im = PARAM_FIRST_SCALAR , num_3d_m

       CALL set_physical_bc3d( moist(ims,kms,jms,im), 'grid%em_p',           &
                               config_flags,                           &
                               ids, ide, jds, jde, kds, kde,           &
                               ims, ime, jms, jme, kms, kme,           &
                               ips, ipe, jps, jpe, kps, kpe,           &
                               grid%i_start(ij), grid%i_end(ij),       &
                               grid%j_start(ij), grid%j_end(ij),       &
                               k_start    , k_end                     )

     END DO moisture_loop_bdy_2

     chem_species_bdy_loop_2 : DO ic = PARAM_FIRST_SCALAR , num_3d_c

       CALL set_physical_bc3d( chem(ims,kms,jms,ic) , 'grid%em_p', config_flags,  &
                               ids, ide, jds, jde, kds, kde,            &
                               ims, ime, jms, jme, kms, kme,            &
                               ips, ipe, jps, jpe, kps, kpe,            &
                               grid%i_start(ij), grid%i_end(ij),                  &
                               grid%j_start(ij), grid%j_end(ij),                  &
                               k_start    , k_end                      )

     END DO chem_species_bdy_loop_2

     scalar_species_bdy_loop_2 : DO is = PARAM_FIRST_SCALAR , num_3d_s

       CALL set_physical_bc3d( scalar(ims,kms,jms,is) , 'grid%em_p', config_flags,  &
                               ids, ide, jds, jde, kds, kde,            &
                               ims, ime, jms, jme, kms, kme,            &
                               ips, ipe, jps, jpe, kps, kpe,            &
                               grid%i_start(ij), grid%i_end(ij),                  &
                               grid%j_start(ij), grid%j_end(ij),                  &
                               k_start    , k_end                      )

     END DO scalar_species_bdy_loop_2

   END DO tile_bc_loop_2
   !$OMP END PARALLEL DO


   IF( config_flags%specified .or. config_flags%nested ) THEN 
     grid%dtbc = grid%dtbc + grid%dt
   ENDIF

! calculate some model diagnostics.

         CALL wrf_debug ( 200 , ' call diagnostic_driver' )
   
         CALL diagnostic_output_calc(                                     &
     &              DPSDT=grid%dpsdt   ,DMUDT=grid%dmudt                  &
     &             ,P_PHY=p_phy   ,PK1M=grid%pk1m                         &
     &             ,MU_2=grid%em_mu_2  ,MU_2M=grid%mu_2m                  &
     &             ,U=grid%em_u_2    ,V=grid%em_v_2                       &
     &             ,RAINCV=grid%raincv    ,RAINNCV=grid%rainncv           &
     &             ,RAINC=grid%rainc    ,RAINNC=grid%rainnc               &
     &             ,HFX=grid%hfx   ,SFCEVP=grid%sfcevp    ,LH=grid%lh     &
     &             ,DT=grid%dt      ,SBW=config_flags%spec_bdy_width      &
     &             ,XTIME=grid%xtime                                      &
                 ! Selection flag
     &             ,DIAG_PRINT=config_flags%diag_print                    &
                 ! Dimension arguments
     &             ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde     &
     &             ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme     &
     &             ,IPS=ips,IPE=ipe, JPS=jps,JPE=jpe, KPS=kps,KPE=kpe     &
     &             ,I_START=grid%i_start,I_END=min(grid%i_end, ide-1)     &
     &             ,J_START=grid%j_start,J_END=min(grid%j_end, jde-1)     &
     &             ,KTS=k_start, KTE=min(k_end,kde-1)                     &
     &             ,NUM_TILES=grid%num_tiles                              &
     &                                                          )

!-----------------------------------------------------------------------
! see above
!--------------------------------------------------------------
   CALL wrf_debug ( 200 , ' call HALO_RK_E' )
   IF      ( config_flags%h_mom_adv_order <= 4 ) THEN
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_E_3.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_E_3.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 2, &
     12, 2, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_1, 2, 4, 0, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_2, 2, 4, 0, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_1, 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_2, 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_w_1, 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_w_2, 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_1, 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_2, 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_1, 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_2, 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_1, 2, 4, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_2, 2, 4, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_1, 2, 4, 0, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_2, 2, 4, 0, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_1, 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_2, 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_w_1, 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_w_2, 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_1, 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_2, 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_1, 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_2, 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_1, 2, 4, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_2, 2, 4, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_INIT_EXCH ( local_communicator, 2 , &
     12, 2, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_1, 2, 4, 1, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_2, 2, 4, 1, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_1, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_2, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_w_1, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_w_2, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_1, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_2, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_1, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_2, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_1, 2, 4, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_2, 2, 4, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_1, 2, 4, 1, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_2, 2, 4, 1, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_1, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_2, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_w_1, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_w_2, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_1, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_2, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_1, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_2, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_1, 2, 4, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_2, 2, 4, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
!ENDOFREGISTRYGENERATEDINCLUDE
   ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_E_5.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_E_5.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3, &
     12, 2, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_1, 3, 4, 0, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_2, 3, 4, 0, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_1, 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_2, 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_w_1, 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_w_2, 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_1, 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_2, 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_1, 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_2, 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_1, 3, 4, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_2, 3, 4, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_1, 3, 4, 0, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_2, 3, 4, 0, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_1, 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_2, 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_w_1, 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_w_2, 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_1, 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_2, 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_1, 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_2, 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_1, 3, 4, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_2, 3, 4, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3 , &
     12, 2, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_1, 3, 4, 1, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_2, 3, 4, 1, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_1, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_2, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_w_1, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_w_2, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_1, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_2, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_1, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_2, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_1, 3, 4, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_2, 3, 4, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_1, 3, 4, 1, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_u_2, 3, 4, 1, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_1, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_v_2, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_w_1, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_w_2, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_1, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_t_2, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_1, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_ph_2, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_1, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_tke_2, 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_1, 3, 4, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%em_mu_2, 3, 4, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
!ENDOFREGISTRYGENERATEDINCLUDE
   ELSE
     WRITE(wrf_err_message,*)'solve_em: invalid h_mom_adv_order = ',config_flags%h_mom_adv_order
     CALL wrf_error_fatal3 ( "solve_em.b" , 3341 , TRIM(wrf_err_message))
   ENDIF

   if ( num_moist >= PARAM_FIRST_SCALAR  ) then
!-----------------------------------------------------------------------
! see above
!--------------------------------------------------------------
     CALL wrf_debug ( 200 , ' call HALO_RK_MOIST' )
     IF      ( config_flags%h_mom_adv_order <= 4 ) THEN
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_MOIST_E_3.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_MOIST_E_3.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 2, &
     0  &
   + num_moist   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_INIT_EXCH ( local_communicator, 2 , &
     0  &
   + num_moist   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
     ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_MOIST_E_5.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_MOIST_E_5.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3, &
     0  &
   + num_moist   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3 , &
     0  &
   + num_moist   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
     ELSE
       WRITE(wrf_err_message,*)'solve_em: invalid h_mom_adv_order = ',config_flags%h_mom_adv_order
       CALL wrf_error_fatal3 ( "solve_em.b" , 3357 , TRIM(wrf_err_message))
     ENDIF
   endif
   if ( num_chem >= PARAM_FIRST_SCALAR ) then
!-----------------------------------------------------------------------
! see above
!--------------------------------------------------------------
     CALL wrf_debug ( 200 , ' call HALO_RK_CHEM' )
     IF      ( config_flags%h_mom_adv_order <= 4 ) THEN
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_CHEM_E_3.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_CHEM_E_3.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 2, &
     0  &
   + num_chem   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_INIT_EXCH ( local_communicator, 2 , &
     0  &
   + num_chem   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
     ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_CHEM_E_5.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_CHEM_E_5.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3, &
     0  &
   + num_chem   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3 , &
     0  &
   + num_chem   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
     ELSE
       WRITE(wrf_err_message,*)'solve_em: invalid h_mom_adv_order = ',config_flags%h_mom_adv_order
       CALL wrf_error_fatal3 ( "solve_em.b" , 3371 , TRIM(wrf_err_message))
     ENDIF
   endif
   if ( num_scalar >= PARAM_FIRST_SCALAR ) then
!-----------------------------------------------------------------------
! see above
!--------------------------------------------------------------
     CALL wrf_debug ( 200 , ' call HALO_RK_SCALAR' )
     IF      ( config_flags%h_mom_adv_order <= 4 ) THEN
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_SCALAR_E_3.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_SCALAR_E_3.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 2, &
     0  &
   + num_scalar   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_INIT_EXCH ( local_communicator, 2 , &
     0  &
   + num_scalar   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 2, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
     ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_SCALAR_E_5.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_SCALAR_E_5.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3, &
     0  &
   + num_scalar   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3 , &
     0  &
   + num_scalar   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
     ELSE
       WRITE(wrf_err_message,*)'solve_em: invalid h_mom_adv_order = ',config_flags%h_mom_adv_order
       CALL wrf_error_fatal3 ( "solve_em.b" , 3385 , TRIM(wrf_err_message))
     ENDIF
   endif

   CALL wrf_debug ( 200 , ' call end of solve_em' )

! Finish timers if compiled with -DBENCH.






















































   RETURN

END SUBROUTINE solve_em
