!WRF:MEDIATION_LAYER:
!

SUBROUTINE med_initialdata_input ( grid , config_flags )
  ! Driver layer
   USE module_domain
   USE module_io_domain
  ! Model layer
   USE module_configure
   USE module_bc_time_utilities
   USE esmf_mod

   IMPLICIT NONE


  ! Interface 
   INTERFACE
     SUBROUTINE start_domain ( grid )  ! comes from module_start in appropriate dyn_ directory
       USE module_domain
       TYPE (domain) grid
     END SUBROUTINE start_domain
   END INTERFACE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
  ! Local
   INTEGER                :: fid , ierr , myproc
   CHARACTER (LEN=80)     :: inpname , rstname, timestr
   CHARACTER (LEN=80)     :: message
   LOGICAL                :: restart
   TYPE(ESMF_Time)        :: CurrTime


   CALL get_restart( restart )
   IF ( .NOT. restart ) THEN
     !  Initialize the mother domain.
     CALL start_timing
     grid%input_from_file = .true.
     IF ( grid%input_from_file ) THEN

        CALL       wrf_debug ( 100 , 'wrf main: calling open_r_dataset for wrfinput' )
        CALL construct_filename1 ( inpname , 'wrfinput' , grid%id , 2 )
        CALL open_r_dataset ( fid, TRIM(inpname) , grid , config_flags , "DATASET=INPUT", ierr )
        IF ( ierr .NE. 0 ) THEN
          WRITE( wrf_err_message , * ) 'program wrf: error opening wrfinput for reading ierr=',ierr
          CALL WRF_ERROR_FATAL ( wrf_err_message )
        ENDIF
        CALL       wrf_debug ( 100 , 'wrf: calling input_model_input' )
write(0,*)'before input_model_input ',grid%dt,config_flags%dt
	write(0,*) 'fid passed to input_model_input: ', fid
        CALL input_model_input ( fid ,   grid , config_flags , ierr )
write(0,*)'after input_model_input ',grid%dt,config_flags%dt
        CALL       wrf_debug ( 100 , 'wrf: back from input_model_input' )
        CALL close_dataset ( fid , config_flags , "DATASET=INPUT" )

     ENDIF
     CALL start_domain ( grid )
   ELSE
     CALL construct_filename1 ( rstname , 'wrfrst' , grid%id , 2 )
     CALL ESMF_ClockGetCurrTime( grid%domain_clock, CurrTime=CurrTime, rc=ierr )
     CALL ESMF_TimeGetString( CurrTime, timestr, rc=ierr )
     rstname = TRIM(rstname) // "_" // TRIM(timestr)
     WRITE(message,*)'opening ',TRIM(rstname),' for reading'
     CALL wrf_debug ( 0 , message )
     CALL open_r_dataset ( fid , TRIM(rstname) , grid , config_flags , "DATASET=RESTART", ierr )
     IF ( ierr .NE. 0 ) THEN
       WRITE( message , '("program wrf: error opening ",A32," for reading")') TRIM(rstname)
       CALL WRF_ERROR_FATAL ( message )
     ENDIF
     CALL input_restart ( fid,   grid , config_flags , ierr )
     CALL close_dataset ( fid , config_flags , "DATASET=RESTART" )
     CALL start_domain ( grid )
   ENDIF

   RETURN
END SUBROUTINE med_initialdata_input

SUBROUTINE med_shutdown_io ( grid , config_flags )
  ! Driver layer
   USE module_domain
   USE module_io_domain
  ! Model layer
   USE module_configure

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
  ! Local
   CHARACTER (LEN=80)      :: message
   INTEGER                 :: ierr

   IF ( grid%oid > 0 ) CALL close_dataset ( grid%oid , config_flags , "DATASET=HISTORY" )

   CALL wrf_ioexit( ierr )    ! shut down the quilt I/O

   RETURN

END SUBROUTINE med_shutdown_io

SUBROUTINE med_add_config_info_to_grid ( grid )

   USE module_domain
   USE module_configure
 
   IMPLICIT NONE

   !  Input data.

   TYPE(domain) , TARGET          :: grid

!STARTOFREGISTRYGENERATEDINCLUDE 'inc/config_assigns.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
! Contains config assign statements for module_domain.F.
 grid % time_step                  = model_config_rec % time_step 
 grid % time_step_fract_num        = model_config_rec % time_step_fract_num 
 grid % time_step_fract_den        = model_config_rec % time_step_fract_den 
 grid % restart                    = model_config_rec % restart 
 grid % max_dom                    = model_config_rec % max_dom 
 grid % dyn_opt                    = model_config_rec % dyn_opt 
 grid % rk_ord                     = model_config_rec % rk_ord 
 grid % diff_opt                   = model_config_rec % diff_opt 
 grid % km_opt                     = model_config_rec % km_opt 
 grid % damp_opt                   = model_config_rec % damp_opt 
 grid % isfflx                     = model_config_rec % isfflx 
 grid % ifsnow                     = model_config_rec % ifsnow 
 grid % icloud                     = model_config_rec % icloud 
 grid % num_soil_layers            = model_config_rec % num_soil_layers 
 grid % num_land_cat               = model_config_rec % num_land_cat 
 grid % num_soil_cat               = model_config_rec % num_soil_cat 
 grid % spec_bdy_width             = model_config_rec % spec_bdy_width 
 grid % spec_zone                  = model_config_rec % spec_zone 
 grid % relax_zone                 = model_config_rec % relax_zone 
 grid % ensdim                     = model_config_rec % ensdim 
 grid % tile_sz_x                  = model_config_rec % tile_sz_x 
 grid % tile_sz_y                  = model_config_rec % tile_sz_y 
 grid % numtiles                   = model_config_rec % numtiles 
 grid % debug_level                = model_config_rec % debug_level 
 grid % irand                      = model_config_rec % irand 
 grid % run_days                   = model_config_rec % run_days 
 grid % run_hours                  = model_config_rec % run_hours 
 grid % run_minutes                = model_config_rec % run_minutes 
 grid % run_seconds                = model_config_rec % run_seconds 
 grid % start_year                 = model_config_rec % start_year (grid%id)
 grid % start_month                = model_config_rec % start_month (grid%id)
 grid % start_day                  = model_config_rec % start_day (grid%id)
 grid % start_hour                 = model_config_rec % start_hour (grid%id)
 grid % start_minute               = model_config_rec % start_minute (grid%id)
 grid % start_second               = model_config_rec % start_second (grid%id)
 grid % end_year                   = model_config_rec % end_year (grid%id)
 grid % end_month                  = model_config_rec % end_month (grid%id)
 grid % end_day                    = model_config_rec % end_day (grid%id)
 grid % end_hour                   = model_config_rec % end_hour (grid%id)
 grid % end_minute                 = model_config_rec % end_minute (grid%id)
 grid % end_second                 = model_config_rec % end_second (grid%id)
 grid % grid_id                    = model_config_rec % grid_id (grid%id)
 grid % level                      = model_config_rec % level (grid%id)
 grid % s_we                       = model_config_rec % s_we (grid%id)
 grid % e_we                       = model_config_rec % e_we (grid%id)
 grid % s_sn                       = model_config_rec % s_sn (grid%id)
 grid % e_sn                       = model_config_rec % e_sn (grid%id)
 grid % s_vert                     = model_config_rec % s_vert (grid%id)
 grid % e_vert                     = model_config_rec % e_vert (grid%id)
 grid % history_interval           = model_config_rec % history_interval (grid%id)
 grid % auxhist1_interval          = model_config_rec % auxhist1_interval (grid%id)
 grid % auxhist2_interval          = model_config_rec % auxhist2_interval (grid%id)
 grid % auxhist3_interval          = model_config_rec % auxhist3_interval (grid%id)
 grid % auxhist4_interval          = model_config_rec % auxhist4_interval (grid%id)
 grid % auxhist5_interval          = model_config_rec % auxhist5_interval (grid%id)
 grid % auxinput1_interval         = model_config_rec % auxinput1_interval (grid%id)
 grid % auxinput2_interval         = model_config_rec % auxinput2_interval (grid%id)
 grid % auxinput3_interval         = model_config_rec % auxinput3_interval (grid%id)
 grid % auxinput4_interval         = model_config_rec % auxinput4_interval (grid%id)
 grid % auxinput5_interval         = model_config_rec % auxinput5_interval (grid%id)
 grid % restart_interval           = model_config_rec % restart_interval 
 grid % frames_per_outfile         = model_config_rec % frames_per_outfile (grid%id)
 grid % time_step_sound            = model_config_rec % time_step_sound (grid%id)
 grid % parent_id                  = model_config_rec % parent_id (grid%id)
 grid % i_parent_start             = model_config_rec % i_parent_start (grid%id)
 grid % j_parent_start             = model_config_rec % j_parent_start (grid%id)
 grid % shw                        = model_config_rec % shw (grid%id)
 grid % parent_grid_ratio          = model_config_rec % parent_grid_ratio (grid%id)
 grid % parent_time_step_ratio     = model_config_rec % parent_time_step_ratio (grid%id)
 grid % moad_grid_ratio            = model_config_rec % moad_grid_ratio (grid%id)
 grid % moad_time_step_ratio       = model_config_rec % moad_time_step_ratio (grid%id)
 grid % non_hydrostatic            = model_config_rec % non_hydrostatic (grid%id)
 grid % dx                         = model_config_rec % dx (grid%id)
 grid % dy                         = model_config_rec % dy (grid%id)
 grid % dt                         = model_config_rec % dt (grid%id)
 grid % ztop                       = model_config_rec % ztop (grid%id)
 grid % zdamp                      = model_config_rec % zdamp (grid%id)
 grid % dampcoef                   = model_config_rec % dampcoef (grid%id)
 grid % smdiv                      = model_config_rec % smdiv (grid%id)
 grid % emdiv                      = model_config_rec % emdiv (grid%id)
 grid % epssm                      = model_config_rec % epssm (grid%id)
 grid % khdif                      = model_config_rec % khdif (grid%id)
 grid % kvdif                      = model_config_rec % kvdif (grid%id)
 grid % mix_cr_len                 = model_config_rec % mix_cr_len (grid%id)
 grid % tke_upper_bound            = model_config_rec % tke_upper_bound (grid%id)
 grid % kh_tke_upper_bound         = model_config_rec % kh_tke_upper_bound (grid%id)
 grid % kv_tke_upper_bound         = model_config_rec % kv_tke_upper_bound (grid%id)
 grid % radt                       = model_config_rec % radt (grid%id)
 grid % bldt                       = model_config_rec % bldt (grid%id)
 grid % cudt                       = model_config_rec % cudt (grid%id)
 grid % gsmdt                      = model_config_rec % gsmdt (grid%id)
 grid % julyr                      = model_config_rec % julyr (grid%id)
 grid % julday                     = model_config_rec % julday (grid%id)
 grid % gmt                        = model_config_rec % gmt (grid%id)
 grid % periodic_x                 = model_config_rec % periodic_x (grid%id)
 grid % symmetric_xs               = model_config_rec % symmetric_xs (grid%id)
 grid % symmetric_xe               = model_config_rec % symmetric_xe (grid%id)
 grid % open_xs                    = model_config_rec % open_xs (grid%id)
 grid % open_xe                    = model_config_rec % open_xe (grid%id)
 grid % periodic_y                 = model_config_rec % periodic_y (grid%id)
 grid % symmetric_ys               = model_config_rec % symmetric_ys (grid%id)
 grid % symmetric_ye               = model_config_rec % symmetric_ye (grid%id)
 grid % open_ys                    = model_config_rec % open_ys (grid%id)
 grid % open_ye                    = model_config_rec % open_ye (grid%id)
 grid % nested                     = model_config_rec % nested (grid%id)
 grid % specified                  = model_config_rec % specified (grid%id)
 grid % top_radiation              = model_config_rec % top_radiation (grid%id)
 grid % idtad                      = model_config_rec % idtad (grid%id)
 grid % nsoil                      = model_config_rec % nsoil (grid%id)
 grid % nphs                       = model_config_rec % nphs (grid%id)
 grid % ncnvc                      = model_config_rec % ncnvc (grid%id)
 grid % nrads                      = model_config_rec % nrads (grid%id)
 grid % nradl                      = model_config_rec % nradl (grid%id)
 grid % sigma                      = model_config_rec % sigma (grid%id)
 grid % chem_opt                   = model_config_rec % chem_opt (grid%id)
 grid % mp_physics                 = model_config_rec % mp_physics (grid%id)
 grid % ra_lw_physics              = model_config_rec % ra_lw_physics (grid%id)
 grid % ra_sw_physics              = model_config_rec % ra_sw_physics (grid%id)
 grid % bl_sfclay_physics          = model_config_rec % bl_sfclay_physics (grid%id)
 grid % bl_surface_physics         = model_config_rec % bl_surface_physics (grid%id)
 grid % bl_pbl_physics             = model_config_rec % bl_pbl_physics (grid%id)
 grid % cu_physics                 = model_config_rec % cu_physics (grid%id)
 grid % h_mom_adv_order            = model_config_rec % h_mom_adv_order (grid%id)
 grid % v_mom_adv_order            = model_config_rec % v_mom_adv_order (grid%id)
 grid % h_sca_adv_order            = model_config_rec % h_sca_adv_order (grid%id)
 grid % v_sca_adv_order            = model_config_rec % v_sca_adv_order (grid%id)
 grid % io_form_input              = model_config_rec % io_form_input 
 grid % io_form_auxinput1          = model_config_rec % io_form_auxinput1 
 grid % io_form_auxinput2          = model_config_rec % io_form_auxinput2 
 grid % io_form_auxinput3          = model_config_rec % io_form_auxinput3 
 grid % io_form_auxinput4          = model_config_rec % io_form_auxinput4 
 grid % io_form_auxinput5          = model_config_rec % io_form_auxinput5 
 grid % io_form_history            = model_config_rec % io_form_history 
 grid % io_form_auxhist1           = model_config_rec % io_form_auxhist1 
 grid % io_form_auxhist2           = model_config_rec % io_form_auxhist2 
 grid % io_form_auxhist3           = model_config_rec % io_form_auxhist3 
 grid % io_form_auxhist4           = model_config_rec % io_form_auxhist4 
 grid % io_form_auxhist5           = model_config_rec % io_form_auxhist5 
 grid % io_form_restart            = model_config_rec % io_form_restart 
 grid % io_form_boundary           = model_config_rec % io_form_boundary 
 grid % interval_seconds           = model_config_rec % interval_seconds 
 grid % real_data_init_type        = model_config_rec % real_data_init_type 
 grid % cen_lat                    = model_config_rec % cen_lat (grid%id)
 grid % cen_lon                    = model_config_rec % cen_lon (grid%id)
 grid % truelat1                   = model_config_rec % truelat1 (grid%id)
 grid % truelat2                   = model_config_rec % truelat2 (grid%id)
 grid % bdyfrq                     = model_config_rec % bdyfrq (grid%id)
 grid % iswater                    = model_config_rec % iswater (grid%id)
 grid % isice                      = model_config_rec % isice (grid%id)
 grid % map_proj                   = model_config_rec % map_proj (grid%id)
!ENDOFREGISTRYGENERATEDINCLUDE

   RETURN

END SUBROUTINE med_add_config_info_to_grid

