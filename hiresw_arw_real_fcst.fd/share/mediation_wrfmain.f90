!WRF:MEDIATION_LAYER:
!

SUBROUTINE med_initialdata_input_ptr ( grid , config_flags )
   USE module_domain
   USE module_configure
   IMPLICIT NONE
   TYPE (domain) , POINTER :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   INTERFACE 
      SUBROUTINE med_initialdata_input ( grid , config_flags )
         USE module_domain
         USE module_configure
         TYPE (domain) :: grid
         TYPE (grid_config_rec_type) , INTENT(IN) :: config_flags
      END SUBROUTINE med_initialdata_input
   END INTERFACE
   CALL  med_initialdata_input ( grid , config_flags )


END SUBROUTINE med_initialdata_input_ptr

SUBROUTINE med_initialdata_input ( grid , config_flags )
  ! Driver layer
   USE module_domain
   USE module_io_domain
   USE module_timing
  ! Model layer
   USE module_configure
   USE module_bc_time_utilities
   USE module_utility

   IMPLICIT NONE


  ! Interface 
   INTERFACE
     SUBROUTINE start_domain ( grid , allowed_to_read )  ! comes from module_start in appropriate dyn_ directory
       USE module_domain
       TYPE (domain) grid
       LOGICAL, INTENT(IN) :: allowed_to_read 
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


   CALL nl_get_restart( 1, restart )
   IF ( .NOT. restart ) THEN
     !  Initialize the mother domain.
     grid%input_from_file = .true.
     IF ( grid%input_from_file ) THEN

        CALL       wrf_debug ( 1 , 'wrf main: calling open_r_dataset for wrfinput' )

! typically <date> will not be part of input_inname but allow for it
        CALL domain_clock_get( grid, current_timestr=timestr )
        CALL construct_filename2a ( inpname , config_flags%input_inname , grid%id , 2 , timestr )

        CALL open_r_dataset ( fid, TRIM(inpname) , grid , config_flags , "DATASET=INPUT", ierr )
        IF ( ierr .NE. 0 ) THEN
          WRITE( wrf_err_message , * ) 'program wrf: error opening ',TRIM(inpname),' for reading ierr=',ierr
          CALL wrf_error_fatal3 ( "mediation_wrfmain.b" , 70 ,  wrf_err_message )
        ENDIF
        IF      ( ( grid%id .EQ. 1 ) .OR. ( config_flags%fine_input_stream .EQ. 0 ) ) THEN
           CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_model_input' )
           CALL input_model_input      ( fid ,  grid , config_flags , ierr )
           CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_model_input' )
        ELSE IF   ( config_flags%fine_input_stream .EQ. 1 ) THEN
           CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_aux_model_input1' )
           CALL input_aux_model_input1 ( fid ,   grid , config_flags , ierr )
           CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_aux_model_input1' )
        ELSE IF   ( config_flags%fine_input_stream .EQ. 2 ) THEN
           CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_aux_model_input2' )
           CALL input_aux_model_input2 ( fid ,   grid , config_flags , ierr )
           CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_aux_model_input2' )
        ELSE IF   ( config_flags%fine_input_stream .EQ. 3 ) THEN
           CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_aux_model_input3' )
           CALL input_aux_model_input3 ( fid ,   grid , config_flags , ierr )
           CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_aux_model_input3' )
        ELSE IF   ( config_flags%fine_input_stream .EQ. 4 ) THEN
           CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_aux_model_input4' )
           CALL input_aux_model_input4 ( fid ,   grid , config_flags , ierr )
           CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_aux_model_input4' )
        ELSE IF   ( config_flags%fine_input_stream .EQ. 5 ) THEN
           CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_aux_model_input5' )
           CALL input_aux_model_input5 ( fid ,   grid , config_flags , ierr )
           CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_aux_model_input5' )
        ELSE IF   ( config_flags%fine_input_stream .EQ. 6 ) THEN
           CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_aux_model_input6' )
           CALL input_aux_model_input6 ( fid ,   grid , config_flags , ierr )
           CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_aux_model_input6' )
        ELSE IF   ( config_flags%fine_input_stream .EQ. 7 ) THEN
           CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_aux_model_input7' )
           CALL input_aux_model_input7 ( fid ,   grid , config_flags , ierr )
           CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_aux_model_input7' )
        ELSE IF   ( config_flags%fine_input_stream .EQ. 8 ) THEN
           CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_aux_model_input8' )
           CALL input_aux_model_input8 ( fid ,   grid , config_flags , ierr )
           CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_aux_model_input8' )
        ELSE IF   ( config_flags%fine_input_stream .EQ. 9 ) THEN
           CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_aux_model_input9' )
           CALL input_aux_model_input9 ( fid ,   grid , config_flags , ierr )
           CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_aux_model_input9' )
        ELSE IF   ( config_flags%fine_input_stream .EQ. 10 ) THEN
           CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_aux_model_input10' )
           CALL input_aux_model_input10 ( fid ,   grid , config_flags , ierr )
           CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_aux_model_input10' )
        ELSE IF   ( config_flags%fine_input_stream .EQ. 11 ) THEN
           CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_aux_model_input11' )
           CALL input_aux_model_input11 ( fid ,   grid , config_flags , ierr )
           CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_aux_model_input11' )
        ELSE
           WRITE( message , '("med_initialdata_input: bad fine_input_stream = ",I4,")') config_flags%fine_input_stream
           CALL wrf_error_fatal3 ( "mediation_wrfmain.b" , 122 ,  message )
        END IF
        CALL close_dataset ( fid , config_flags , "DATASET=INPUT" )
     ENDIF
     grid%imask_nostag = 1
     grid%imask_xstag = 1
     grid%imask_ystag = 1
     grid%imask_xystag = 1
     CALL start_domain ( grid , .TRUE. )
   ELSE
     CALL domain_clock_get( grid, current_timestr=timestr )
     CALL construct_filename2a ( rstname , config_flags%rst_inname , grid%id , 2 , timestr )

     WRITE(message,*)'RESTART run: opening ',TRIM(rstname),' for reading'
     CALL wrf_message (  message )
     CALL open_r_dataset ( fid , TRIM(rstname) , grid , config_flags , "DATASET=RESTART", ierr )
     IF ( ierr .NE. 0 ) THEN
       WRITE( message , '("program wrf: error opening ",A32," for reading")') TRIM(rstname)
       CALL wrf_error_fatal3 ( "mediation_wrfmain.b" , 144 ,  message )
     ENDIF
     CALL input_restart ( fid,   grid , config_flags , ierr )
     CALL close_dataset ( fid , config_flags , "DATASET=RESTART" )
     grid%imask_nostag = 1
     grid%imask_xstag = 1
     grid%imask_ystag = 1
     grid%imask_xystag = 1
     CALL start_domain ( grid , .TRUE. )
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
   IF ( grid%auxhist1_oid > 0 ) CALL close_dataset ( grid%auxhist1_oid , config_flags , "DATASET=AUXHIST1" )
   IF ( grid%auxhist2_oid > 0 ) CALL close_dataset ( grid%auxhist2_oid , config_flags , "DATASET=AUXHIST2" )
   IF ( grid%auxhist3_oid > 0 ) CALL close_dataset ( grid%auxhist3_oid , config_flags , "DATASET=AUXHIST3" )
   IF ( grid%auxhist4_oid > 0 ) CALL close_dataset ( grid%auxhist4_oid , config_flags , "DATASET=AUXHIST4" )
   IF ( grid%auxhist5_oid > 0 ) CALL close_dataset ( grid%auxhist5_oid , config_flags , "DATASET=AUXHIST5" )

   IF ( grid%lbc_fid > 0 ) CALL close_dataset ( grid%lbc_fid , config_flags , "DATASET=BOUNDARY" )

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
 grid % interval_seconds           = model_config_rec % interval_seconds 
 grid % input_from_file            = model_config_rec % input_from_file (grid%id)
 grid % fine_input_stream          = model_config_rec % fine_input_stream (grid%id)
 grid % input_from_hires           = model_config_rec % input_from_hires (grid%id)
 grid % rsmas_data_path            = model_config_rec % rsmas_data_path 
 grid % history_interval           = model_config_rec % history_interval (grid%id)
 grid % frames_per_outfile         = model_config_rec % frames_per_outfile (grid%id)
 grid % frames_per_auxhist1        = model_config_rec % frames_per_auxhist1 (grid%id)
 grid % frames_per_auxhist2        = model_config_rec % frames_per_auxhist2 (grid%id)
 grid % frames_per_auxhist3        = model_config_rec % frames_per_auxhist3 (grid%id)
 grid % frames_per_auxhist4        = model_config_rec % frames_per_auxhist4 (grid%id)
 grid % frames_per_auxhist5        = model_config_rec % frames_per_auxhist5 (grid%id)
 grid % frames_per_auxhist6        = model_config_rec % frames_per_auxhist6 (grid%id)
 grid % frames_per_auxhist7        = model_config_rec % frames_per_auxhist7 (grid%id)
 grid % frames_per_auxhist8        = model_config_rec % frames_per_auxhist8 (grid%id)
 grid % frames_per_auxhist9        = model_config_rec % frames_per_auxhist9 (grid%id)
 grid % frames_per_auxhist10       = model_config_rec % frames_per_auxhist10 (grid%id)
 grid % frames_per_auxhist11       = model_config_rec % frames_per_auxhist11 (grid%id)
 grid % restart                    = model_config_rec % restart 
 grid % restart_interval           = model_config_rec % restart_interval 
 grid % io_form_input              = model_config_rec % io_form_input 
 grid % io_form_history            = model_config_rec % io_form_history 
 grid % io_form_restart            = model_config_rec % io_form_restart 
 grid % io_form_boundary           = model_config_rec % io_form_boundary 
 grid % debug_level                = model_config_rec % debug_level 
 grid % self_test_domain           = model_config_rec % self_test_domain 
 grid % history_outname            = model_config_rec % history_outname 
 grid % auxhist1_outname           = model_config_rec % auxhist1_outname 
 grid % auxhist2_outname           = model_config_rec % auxhist2_outname 
 grid % auxhist3_outname           = model_config_rec % auxhist3_outname 
 grid % auxhist4_outname           = model_config_rec % auxhist4_outname 
 grid % auxhist5_outname           = model_config_rec % auxhist5_outname 
 grid % auxhist6_outname           = model_config_rec % auxhist6_outname 
 grid % auxhist7_outname           = model_config_rec % auxhist7_outname 
 grid % auxhist8_outname           = model_config_rec % auxhist8_outname 
 grid % auxhist9_outname           = model_config_rec % auxhist9_outname 
 grid % auxhist10_outname          = model_config_rec % auxhist10_outname 
 grid % auxhist11_outname          = model_config_rec % auxhist11_outname 
 grid % history_inname             = model_config_rec % history_inname 
 grid % auxhist1_inname            = model_config_rec % auxhist1_inname 
 grid % auxhist2_inname            = model_config_rec % auxhist2_inname 
 grid % auxhist3_inname            = model_config_rec % auxhist3_inname 
 grid % auxhist4_inname            = model_config_rec % auxhist4_inname 
 grid % auxhist5_inname            = model_config_rec % auxhist5_inname 
 grid % auxhist6_inname            = model_config_rec % auxhist6_inname 
 grid % auxhist7_inname            = model_config_rec % auxhist7_inname 
 grid % auxhist8_inname            = model_config_rec % auxhist8_inname 
 grid % auxhist9_inname            = model_config_rec % auxhist9_inname 
 grid % auxhist10_inname           = model_config_rec % auxhist10_inname 
 grid % auxhist11_inname           = model_config_rec % auxhist11_inname 
 grid % auxinput1_outname          = model_config_rec % auxinput1_outname 
 grid % auxinput2_outname          = model_config_rec % auxinput2_outname 
 grid % auxinput3_outname          = model_config_rec % auxinput3_outname 
 grid % auxinput4_outname          = model_config_rec % auxinput4_outname 
 grid % auxinput5_outname          = model_config_rec % auxinput5_outname 
 grid % auxinput6_outname          = model_config_rec % auxinput6_outname 
 grid % auxinput7_outname          = model_config_rec % auxinput7_outname 
 grid % auxinput8_outname          = model_config_rec % auxinput8_outname 
 grid % auxinput9_outname          = model_config_rec % auxinput9_outname 
 grid % auxinput10_outname         = model_config_rec % auxinput10_outname 
 grid % auxinput11_outname         = model_config_rec % auxinput11_outname 
 grid % auxinput2_inname           = model_config_rec % auxinput2_inname 
 grid % auxinput3_inname           = model_config_rec % auxinput3_inname 
 grid % auxinput4_inname           = model_config_rec % auxinput4_inname 
 grid % auxinput5_inname           = model_config_rec % auxinput5_inname 
 grid % auxinput6_inname           = model_config_rec % auxinput6_inname 
 grid % auxinput7_inname           = model_config_rec % auxinput7_inname 
 grid % auxinput8_inname           = model_config_rec % auxinput8_inname 
 grid % auxinput9_inname           = model_config_rec % auxinput9_inname 
 grid % gfdda_inname               = model_config_rec % gfdda_inname 
 grid % auxinput11_inname          = model_config_rec % auxinput11_inname 
 grid % history_interval_mo        = model_config_rec % history_interval_mo (grid%id)
 grid % history_interval_d         = model_config_rec % history_interval_d (grid%id)
 grid % history_interval_h         = model_config_rec % history_interval_h (grid%id)
 grid % history_interval_m         = model_config_rec % history_interval_m (grid%id)
 grid % history_interval_s         = model_config_rec % history_interval_s (grid%id)
 grid % inputout_interval_mo       = model_config_rec % inputout_interval_mo (grid%id)
 grid % inputout_interval_d        = model_config_rec % inputout_interval_d (grid%id)
 grid % inputout_interval_h        = model_config_rec % inputout_interval_h (grid%id)
 grid % inputout_interval_m        = model_config_rec % inputout_interval_m (grid%id)
 grid % inputout_interval_s        = model_config_rec % inputout_interval_s (grid%id)
 grid % inputout_interval          = model_config_rec % inputout_interval (grid%id)
 grid % auxhist1_interval_mo       = model_config_rec % auxhist1_interval_mo (grid%id)
 grid % auxhist1_interval_d        = model_config_rec % auxhist1_interval_d (grid%id)
 grid % auxhist1_interval_h        = model_config_rec % auxhist1_interval_h (grid%id)
 grid % auxhist1_interval_m        = model_config_rec % auxhist1_interval_m (grid%id)
 grid % auxhist1_interval_s        = model_config_rec % auxhist1_interval_s (grid%id)
 grid % auxhist1_interval          = model_config_rec % auxhist1_interval (grid%id)
 grid % auxhist2_interval_mo       = model_config_rec % auxhist2_interval_mo (grid%id)
 grid % auxhist2_interval_d        = model_config_rec % auxhist2_interval_d (grid%id)
 grid % auxhist2_interval_h        = model_config_rec % auxhist2_interval_h (grid%id)
 grid % auxhist2_interval_m        = model_config_rec % auxhist2_interval_m (grid%id)
 grid % auxhist2_interval_s        = model_config_rec % auxhist2_interval_s (grid%id)
 grid % auxhist2_interval          = model_config_rec % auxhist2_interval (grid%id)
 grid % auxhist3_interval_mo       = model_config_rec % auxhist3_interval_mo (grid%id)
 grid % auxhist3_interval_d        = model_config_rec % auxhist3_interval_d (grid%id)
 grid % auxhist3_interval_h        = model_config_rec % auxhist3_interval_h (grid%id)
 grid % auxhist3_interval_m        = model_config_rec % auxhist3_interval_m (grid%id)
 grid % auxhist3_interval_s        = model_config_rec % auxhist3_interval_s (grid%id)
 grid % auxhist3_interval          = model_config_rec % auxhist3_interval (grid%id)
 grid % auxhist4_interval_mo       = model_config_rec % auxhist4_interval_mo (grid%id)
 grid % auxhist4_interval_d        = model_config_rec % auxhist4_interval_d (grid%id)
 grid % auxhist4_interval_h        = model_config_rec % auxhist4_interval_h (grid%id)
 grid % auxhist4_interval_m        = model_config_rec % auxhist4_interval_m (grid%id)
 grid % auxhist4_interval_s        = model_config_rec % auxhist4_interval_s (grid%id)
 grid % auxhist4_interval          = model_config_rec % auxhist4_interval (grid%id)
 grid % auxhist5_interval_mo       = model_config_rec % auxhist5_interval_mo (grid%id)
 grid % auxhist5_interval_d        = model_config_rec % auxhist5_interval_d (grid%id)
 grid % auxhist5_interval_h        = model_config_rec % auxhist5_interval_h (grid%id)
 grid % auxhist5_interval_m        = model_config_rec % auxhist5_interval_m (grid%id)
 grid % auxhist5_interval_s        = model_config_rec % auxhist5_interval_s (grid%id)
 grid % auxhist5_interval          = model_config_rec % auxhist5_interval (grid%id)
 grid % auxhist6_interval_mo       = model_config_rec % auxhist6_interval_mo (grid%id)
 grid % auxhist6_interval_d        = model_config_rec % auxhist6_interval_d (grid%id)
 grid % auxhist6_interval_h        = model_config_rec % auxhist6_interval_h (grid%id)
 grid % auxhist6_interval_m        = model_config_rec % auxhist6_interval_m (grid%id)
 grid % auxhist6_interval_s        = model_config_rec % auxhist6_interval_s (grid%id)
 grid % auxhist6_interval          = model_config_rec % auxhist6_interval (grid%id)
 grid % auxhist7_interval_mo       = model_config_rec % auxhist7_interval_mo (grid%id)
 grid % auxhist7_interval_d        = model_config_rec % auxhist7_interval_d (grid%id)
 grid % auxhist7_interval_h        = model_config_rec % auxhist7_interval_h (grid%id)
 grid % auxhist7_interval_m        = model_config_rec % auxhist7_interval_m (grid%id)
 grid % auxhist7_interval_s        = model_config_rec % auxhist7_interval_s (grid%id)
 grid % auxhist7_interval          = model_config_rec % auxhist7_interval (grid%id)
 grid % auxhist8_interval_mo       = model_config_rec % auxhist8_interval_mo (grid%id)
 grid % auxhist8_interval_d        = model_config_rec % auxhist8_interval_d (grid%id)
 grid % auxhist8_interval_h        = model_config_rec % auxhist8_interval_h (grid%id)
 grid % auxhist8_interval_m        = model_config_rec % auxhist8_interval_m (grid%id)
 grid % auxhist8_interval_s        = model_config_rec % auxhist8_interval_s (grid%id)
 grid % auxhist8_interval          = model_config_rec % auxhist8_interval (grid%id)
 grid % auxhist9_interval_mo       = model_config_rec % auxhist9_interval_mo (grid%id)
 grid % auxhist9_interval_d        = model_config_rec % auxhist9_interval_d (grid%id)
 grid % auxhist9_interval_h        = model_config_rec % auxhist9_interval_h (grid%id)
 grid % auxhist9_interval_m        = model_config_rec % auxhist9_interval_m (grid%id)
 grid % auxhist9_interval_s        = model_config_rec % auxhist9_interval_s (grid%id)
 grid % auxhist9_interval          = model_config_rec % auxhist9_interval (grid%id)
 grid % auxhist10_interval_mo      = model_config_rec % auxhist10_interval_mo (grid%id)
 grid % auxhist10_interval_d       = model_config_rec % auxhist10_interval_d (grid%id)
 grid % auxhist10_interval_h       = model_config_rec % auxhist10_interval_h (grid%id)
 grid % auxhist10_interval_m       = model_config_rec % auxhist10_interval_m (grid%id)
 grid % auxhist10_interval_s       = model_config_rec % auxhist10_interval_s (grid%id)
 grid % auxhist10_interval         = model_config_rec % auxhist10_interval (grid%id)
 grid % auxhist11_interval_mo      = model_config_rec % auxhist11_interval_mo (grid%id)
 grid % auxhist11_interval_d       = model_config_rec % auxhist11_interval_d (grid%id)
 grid % auxhist11_interval_h       = model_config_rec % auxhist11_interval_h (grid%id)
 grid % auxhist11_interval_m       = model_config_rec % auxhist11_interval_m (grid%id)
 grid % auxhist11_interval_s       = model_config_rec % auxhist11_interval_s (grid%id)
 grid % auxhist11_interval         = model_config_rec % auxhist11_interval (grid%id)
 grid % auxinput1_interval_mo      = model_config_rec % auxinput1_interval_mo (grid%id)
 grid % auxinput1_interval_d       = model_config_rec % auxinput1_interval_d (grid%id)
 grid % auxinput1_interval_h       = model_config_rec % auxinput1_interval_h (grid%id)
 grid % auxinput1_interval_m       = model_config_rec % auxinput1_interval_m (grid%id)
 grid % auxinput1_interval_s       = model_config_rec % auxinput1_interval_s (grid%id)
 grid % auxinput1_interval         = model_config_rec % auxinput1_interval (grid%id)
 grid % auxinput2_interval_mo      = model_config_rec % auxinput2_interval_mo (grid%id)
 grid % auxinput2_interval_d       = model_config_rec % auxinput2_interval_d (grid%id)
 grid % auxinput2_interval_h       = model_config_rec % auxinput2_interval_h (grid%id)
 grid % auxinput2_interval_m       = model_config_rec % auxinput2_interval_m (grid%id)
 grid % auxinput2_interval_s       = model_config_rec % auxinput2_interval_s (grid%id)
 grid % auxinput2_interval         = model_config_rec % auxinput2_interval (grid%id)
 grid % auxinput3_interval_mo      = model_config_rec % auxinput3_interval_mo (grid%id)
 grid % auxinput3_interval_d       = model_config_rec % auxinput3_interval_d (grid%id)
 grid % auxinput3_interval_h       = model_config_rec % auxinput3_interval_h (grid%id)
 grid % auxinput3_interval_m       = model_config_rec % auxinput3_interval_m (grid%id)
 grid % auxinput3_interval_s       = model_config_rec % auxinput3_interval_s (grid%id)
 grid % auxinput3_interval         = model_config_rec % auxinput3_interval (grid%id)
 grid % auxinput4_interval_mo      = model_config_rec % auxinput4_interval_mo (grid%id)
 grid % auxinput4_interval_d       = model_config_rec % auxinput4_interval_d (grid%id)
 grid % auxinput4_interval_h       = model_config_rec % auxinput4_interval_h (grid%id)
 grid % auxinput4_interval_m       = model_config_rec % auxinput4_interval_m (grid%id)
 grid % auxinput4_interval_s       = model_config_rec % auxinput4_interval_s (grid%id)
 grid % auxinput4_interval         = model_config_rec % auxinput4_interval (grid%id)
 grid % auxinput5_interval_mo      = model_config_rec % auxinput5_interval_mo (grid%id)
 grid % auxinput5_interval_d       = model_config_rec % auxinput5_interval_d (grid%id)
 grid % auxinput5_interval_h       = model_config_rec % auxinput5_interval_h (grid%id)
 grid % auxinput5_interval_m       = model_config_rec % auxinput5_interval_m (grid%id)
 grid % auxinput5_interval_s       = model_config_rec % auxinput5_interval_s (grid%id)
 grid % auxinput5_interval         = model_config_rec % auxinput5_interval (grid%id)
 grid % auxinput6_interval_mo      = model_config_rec % auxinput6_interval_mo (grid%id)
 grid % auxinput6_interval_d       = model_config_rec % auxinput6_interval_d (grid%id)
 grid % auxinput6_interval_h       = model_config_rec % auxinput6_interval_h (grid%id)
 grid % auxinput6_interval_m       = model_config_rec % auxinput6_interval_m (grid%id)
 grid % auxinput6_interval_s       = model_config_rec % auxinput6_interval_s (grid%id)
 grid % auxinput6_interval         = model_config_rec % auxinput6_interval (grid%id)
 grid % auxinput7_interval_mo      = model_config_rec % auxinput7_interval_mo (grid%id)
 grid % auxinput7_interval_d       = model_config_rec % auxinput7_interval_d (grid%id)
 grid % auxinput7_interval_h       = model_config_rec % auxinput7_interval_h (grid%id)
 grid % auxinput7_interval_m       = model_config_rec % auxinput7_interval_m (grid%id)
 grid % auxinput7_interval_s       = model_config_rec % auxinput7_interval_s (grid%id)
 grid % auxinput7_interval         = model_config_rec % auxinput7_interval (grid%id)
 grid % auxinput8_interval_mo      = model_config_rec % auxinput8_interval_mo (grid%id)
 grid % auxinput8_interval_d       = model_config_rec % auxinput8_interval_d (grid%id)
 grid % auxinput8_interval_h       = model_config_rec % auxinput8_interval_h (grid%id)
 grid % auxinput8_interval_m       = model_config_rec % auxinput8_interval_m (grid%id)
 grid % auxinput8_interval_s       = model_config_rec % auxinput8_interval_s (grid%id)
 grid % auxinput8_interval         = model_config_rec % auxinput8_interval (grid%id)
 grid % auxinput9_interval_mo      = model_config_rec % auxinput9_interval_mo (grid%id)
 grid % auxinput9_interval_d       = model_config_rec % auxinput9_interval_d (grid%id)
 grid % auxinput9_interval_h       = model_config_rec % auxinput9_interval_h (grid%id)
 grid % auxinput9_interval_m       = model_config_rec % auxinput9_interval_m (grid%id)
 grid % auxinput9_interval_s       = model_config_rec % auxinput9_interval_s (grid%id)
 grid % auxinput9_interval         = model_config_rec % auxinput9_interval (grid%id)
 grid % gfdda_interval_mo          = model_config_rec % gfdda_interval_mo (grid%id)
 grid % gfdda_interval_d           = model_config_rec % gfdda_interval_d (grid%id)
 grid % gfdda_interval_h           = model_config_rec % gfdda_interval_h (grid%id)
 grid % gfdda_interval_m           = model_config_rec % gfdda_interval_m (grid%id)
 grid % gfdda_interval_s           = model_config_rec % gfdda_interval_s (grid%id)
 grid % gfdda_interval             = model_config_rec % gfdda_interval (grid%id)
 grid % auxinput11_interval_mo     = model_config_rec % auxinput11_interval_mo (grid%id)
 grid % auxinput11_interval_d      = model_config_rec % auxinput11_interval_d (grid%id)
 grid % auxinput11_interval_h      = model_config_rec % auxinput11_interval_h (grid%id)
 grid % auxinput11_interval_m      = model_config_rec % auxinput11_interval_m (grid%id)
 grid % auxinput11_interval_s      = model_config_rec % auxinput11_interval_s (grid%id)
 grid % auxinput11_interval        = model_config_rec % auxinput11_interval (grid%id)
 grid % restart_interval_mo        = model_config_rec % restart_interval_mo 
 grid % restart_interval_d         = model_config_rec % restart_interval_d 
 grid % restart_interval_h         = model_config_rec % restart_interval_h 
 grid % restart_interval_m         = model_config_rec % restart_interval_m 
 grid % restart_interval_s         = model_config_rec % restart_interval_s 
 grid % history_begin_y            = model_config_rec % history_begin_y (grid%id)
 grid % history_begin_mo           = model_config_rec % history_begin_mo (grid%id)
 grid % history_begin_d            = model_config_rec % history_begin_d (grid%id)
 grid % history_begin_h            = model_config_rec % history_begin_h (grid%id)
 grid % history_begin_m            = model_config_rec % history_begin_m (grid%id)
 grid % history_begin_s            = model_config_rec % history_begin_s (grid%id)
 grid % inputout_begin_y           = model_config_rec % inputout_begin_y (grid%id)
 grid % inputout_begin_mo          = model_config_rec % inputout_begin_mo (grid%id)
 grid % inputout_begin_d           = model_config_rec % inputout_begin_d (grid%id)
 grid % inputout_begin_h           = model_config_rec % inputout_begin_h (grid%id)
 grid % inputout_begin_m           = model_config_rec % inputout_begin_m (grid%id)
 grid % inputout_begin_s           = model_config_rec % inputout_begin_s (grid%id)
 grid % auxhist1_begin_y           = model_config_rec % auxhist1_begin_y (grid%id)
 grid % auxhist1_begin_mo          = model_config_rec % auxhist1_begin_mo (grid%id)
 grid % auxhist1_begin_d           = model_config_rec % auxhist1_begin_d (grid%id)
 grid % auxhist1_begin_h           = model_config_rec % auxhist1_begin_h (grid%id)
 grid % auxhist1_begin_m           = model_config_rec % auxhist1_begin_m (grid%id)
 grid % auxhist1_begin_s           = model_config_rec % auxhist1_begin_s (grid%id)
 grid % auxhist2_begin_y           = model_config_rec % auxhist2_begin_y (grid%id)
 grid % auxhist2_begin_mo          = model_config_rec % auxhist2_begin_mo (grid%id)
 grid % auxhist2_begin_d           = model_config_rec % auxhist2_begin_d (grid%id)
 grid % auxhist2_begin_h           = model_config_rec % auxhist2_begin_h (grid%id)
 grid % auxhist2_begin_m           = model_config_rec % auxhist2_begin_m (grid%id)
 grid % auxhist2_begin_s           = model_config_rec % auxhist2_begin_s (grid%id)
 grid % auxhist3_begin_y           = model_config_rec % auxhist3_begin_y (grid%id)
 grid % auxhist3_begin_mo          = model_config_rec % auxhist3_begin_mo (grid%id)
 grid % auxhist3_begin_d           = model_config_rec % auxhist3_begin_d (grid%id)
 grid % auxhist3_begin_h           = model_config_rec % auxhist3_begin_h (grid%id)
 grid % auxhist3_begin_m           = model_config_rec % auxhist3_begin_m (grid%id)
 grid % auxhist3_begin_s           = model_config_rec % auxhist3_begin_s (grid%id)
 grid % auxhist4_begin_y           = model_config_rec % auxhist4_begin_y (grid%id)
 grid % auxhist4_begin_mo          = model_config_rec % auxhist4_begin_mo (grid%id)
 grid % auxhist4_begin_d           = model_config_rec % auxhist4_begin_d (grid%id)
 grid % auxhist4_begin_h           = model_config_rec % auxhist4_begin_h (grid%id)
 grid % auxhist4_begin_m           = model_config_rec % auxhist4_begin_m (grid%id)
 grid % auxhist4_begin_s           = model_config_rec % auxhist4_begin_s (grid%id)
 grid % auxhist5_begin_y           = model_config_rec % auxhist5_begin_y (grid%id)
 grid % auxhist5_begin_mo          = model_config_rec % auxhist5_begin_mo (grid%id)
 grid % auxhist5_begin_d           = model_config_rec % auxhist5_begin_d (grid%id)
 grid % auxhist5_begin_h           = model_config_rec % auxhist5_begin_h (grid%id)
 grid % auxhist5_begin_m           = model_config_rec % auxhist5_begin_m (grid%id)
 grid % auxhist5_begin_s           = model_config_rec % auxhist5_begin_s (grid%id)
 grid % auxhist6_begin_y           = model_config_rec % auxhist6_begin_y (grid%id)
 grid % auxhist6_begin_mo          = model_config_rec % auxhist6_begin_mo (grid%id)
 grid % auxhist6_begin_d           = model_config_rec % auxhist6_begin_d (grid%id)
 grid % auxhist6_begin_h           = model_config_rec % auxhist6_begin_h (grid%id)
 grid % auxhist6_begin_m           = model_config_rec % auxhist6_begin_m (grid%id)
 grid % auxhist6_begin_s           = model_config_rec % auxhist6_begin_s (grid%id)
 grid % auxhist7_begin_y           = model_config_rec % auxhist7_begin_y (grid%id)
 grid % auxhist7_begin_mo          = model_config_rec % auxhist7_begin_mo (grid%id)
 grid % auxhist7_begin_d           = model_config_rec % auxhist7_begin_d (grid%id)
 grid % auxhist7_begin_h           = model_config_rec % auxhist7_begin_h (grid%id)
 grid % auxhist7_begin_m           = model_config_rec % auxhist7_begin_m (grid%id)
 grid % auxhist7_begin_s           = model_config_rec % auxhist7_begin_s (grid%id)
 grid % auxhist8_begin_y           = model_config_rec % auxhist8_begin_y (grid%id)
 grid % auxhist8_begin_mo          = model_config_rec % auxhist8_begin_mo (grid%id)
 grid % auxhist8_begin_d           = model_config_rec % auxhist8_begin_d (grid%id)
 grid % auxhist8_begin_h           = model_config_rec % auxhist8_begin_h (grid%id)
 grid % auxhist8_begin_m           = model_config_rec % auxhist8_begin_m (grid%id)
 grid % auxhist8_begin_s           = model_config_rec % auxhist8_begin_s (grid%id)
 grid % auxhist9_begin_y           = model_config_rec % auxhist9_begin_y (grid%id)
 grid % auxhist9_begin_mo          = model_config_rec % auxhist9_begin_mo (grid%id)
 grid % auxhist9_begin_d           = model_config_rec % auxhist9_begin_d (grid%id)
 grid % auxhist9_begin_h           = model_config_rec % auxhist9_begin_h (grid%id)
 grid % auxhist9_begin_m           = model_config_rec % auxhist9_begin_m (grid%id)
 grid % auxhist9_begin_s           = model_config_rec % auxhist9_begin_s (grid%id)
 grid % auxhist10_begin_y          = model_config_rec % auxhist10_begin_y (grid%id)
 grid % auxhist10_begin_mo         = model_config_rec % auxhist10_begin_mo (grid%id)
 grid % auxhist10_begin_d          = model_config_rec % auxhist10_begin_d (grid%id)
 grid % auxhist10_begin_h          = model_config_rec % auxhist10_begin_h (grid%id)
 grid % auxhist10_begin_m          = model_config_rec % auxhist10_begin_m (grid%id)
 grid % auxhist10_begin_s          = model_config_rec % auxhist10_begin_s (grid%id)
 grid % auxhist11_begin_y          = model_config_rec % auxhist11_begin_y (grid%id)
 grid % auxhist11_begin_mo         = model_config_rec % auxhist11_begin_mo (grid%id)
 grid % auxhist11_begin_d          = model_config_rec % auxhist11_begin_d (grid%id)
 grid % auxhist11_begin_h          = model_config_rec % auxhist11_begin_h (grid%id)
 grid % auxhist11_begin_m          = model_config_rec % auxhist11_begin_m (grid%id)
 grid % auxhist11_begin_s          = model_config_rec % auxhist11_begin_s (grid%id)
 grid % auxinput1_begin_y          = model_config_rec % auxinput1_begin_y (grid%id)
 grid % auxinput1_begin_mo         = model_config_rec % auxinput1_begin_mo (grid%id)
 grid % auxinput1_begin_d          = model_config_rec % auxinput1_begin_d (grid%id)
 grid % auxinput1_begin_h          = model_config_rec % auxinput1_begin_h (grid%id)
 grid % auxinput1_begin_m          = model_config_rec % auxinput1_begin_m (grid%id)
 grid % auxinput1_begin_s          = model_config_rec % auxinput1_begin_s (grid%id)
 grid % auxinput2_begin_y          = model_config_rec % auxinput2_begin_y (grid%id)
 grid % auxinput2_begin_mo         = model_config_rec % auxinput2_begin_mo (grid%id)
 grid % auxinput2_begin_d          = model_config_rec % auxinput2_begin_d (grid%id)
 grid % auxinput2_begin_h          = model_config_rec % auxinput2_begin_h (grid%id)
 grid % auxinput2_begin_m          = model_config_rec % auxinput2_begin_m (grid%id)
 grid % auxinput2_begin_s          = model_config_rec % auxinput2_begin_s (grid%id)
 grid % auxinput3_begin_y          = model_config_rec % auxinput3_begin_y (grid%id)
 grid % auxinput3_begin_mo         = model_config_rec % auxinput3_begin_mo (grid%id)
 grid % auxinput3_begin_d          = model_config_rec % auxinput3_begin_d (grid%id)
 grid % auxinput3_begin_h          = model_config_rec % auxinput3_begin_h (grid%id)
 grid % auxinput3_begin_m          = model_config_rec % auxinput3_begin_m (grid%id)
 grid % auxinput3_begin_s          = model_config_rec % auxinput3_begin_s (grid%id)
 grid % auxinput4_begin_y          = model_config_rec % auxinput4_begin_y (grid%id)
 grid % auxinput4_begin_mo         = model_config_rec % auxinput4_begin_mo (grid%id)
 grid % auxinput4_begin_d          = model_config_rec % auxinput4_begin_d (grid%id)
 grid % auxinput4_begin_h          = model_config_rec % auxinput4_begin_h (grid%id)
 grid % auxinput4_begin_m          = model_config_rec % auxinput4_begin_m (grid%id)
 grid % auxinput4_begin_s          = model_config_rec % auxinput4_begin_s (grid%id)
 grid % auxinput5_begin_y          = model_config_rec % auxinput5_begin_y (grid%id)
 grid % auxinput5_begin_mo         = model_config_rec % auxinput5_begin_mo (grid%id)
 grid % auxinput5_begin_d          = model_config_rec % auxinput5_begin_d (grid%id)
 grid % auxinput5_begin_h          = model_config_rec % auxinput5_begin_h (grid%id)
 grid % auxinput5_begin_m          = model_config_rec % auxinput5_begin_m (grid%id)
 grid % auxinput5_begin_s          = model_config_rec % auxinput5_begin_s (grid%id)
 grid % auxinput6_begin_y          = model_config_rec % auxinput6_begin_y (grid%id)
 grid % auxinput6_begin_mo         = model_config_rec % auxinput6_begin_mo (grid%id)
 grid % auxinput6_begin_d          = model_config_rec % auxinput6_begin_d (grid%id)
 grid % auxinput6_begin_h          = model_config_rec % auxinput6_begin_h (grid%id)
 grid % auxinput6_begin_m          = model_config_rec % auxinput6_begin_m (grid%id)
 grid % auxinput6_begin_s          = model_config_rec % auxinput6_begin_s (grid%id)
 grid % auxinput7_begin_y          = model_config_rec % auxinput7_begin_y (grid%id)
 grid % auxinput7_begin_mo         = model_config_rec % auxinput7_begin_mo (grid%id)
 grid % auxinput7_begin_d          = model_config_rec % auxinput7_begin_d (grid%id)
 grid % auxinput7_begin_h          = model_config_rec % auxinput7_begin_h (grid%id)
 grid % auxinput7_begin_m          = model_config_rec % auxinput7_begin_m (grid%id)
 grid % auxinput7_begin_s          = model_config_rec % auxinput7_begin_s (grid%id)
 grid % auxinput8_begin_y          = model_config_rec % auxinput8_begin_y (grid%id)
 grid % auxinput8_begin_mo         = model_config_rec % auxinput8_begin_mo (grid%id)
 grid % auxinput8_begin_d          = model_config_rec % auxinput8_begin_d (grid%id)
 grid % auxinput8_begin_h          = model_config_rec % auxinput8_begin_h (grid%id)
 grid % auxinput8_begin_m          = model_config_rec % auxinput8_begin_m (grid%id)
 grid % auxinput8_begin_s          = model_config_rec % auxinput8_begin_s (grid%id)
 grid % auxinput9_begin_y          = model_config_rec % auxinput9_begin_y (grid%id)
 grid % auxinput9_begin_mo         = model_config_rec % auxinput9_begin_mo (grid%id)
 grid % auxinput9_begin_d          = model_config_rec % auxinput9_begin_d (grid%id)
 grid % auxinput9_begin_h          = model_config_rec % auxinput9_begin_h (grid%id)
 grid % auxinput9_begin_m          = model_config_rec % auxinput9_begin_m (grid%id)
 grid % auxinput9_begin_s          = model_config_rec % auxinput9_begin_s (grid%id)
 grid % gfdda_begin_y              = model_config_rec % gfdda_begin_y (grid%id)
 grid % gfdda_begin_mo             = model_config_rec % gfdda_begin_mo (grid%id)
 grid % gfdda_begin_d              = model_config_rec % gfdda_begin_d (grid%id)
 grid % gfdda_begin_h              = model_config_rec % gfdda_begin_h (grid%id)
 grid % gfdda_begin_m              = model_config_rec % gfdda_begin_m (grid%id)
 grid % gfdda_begin_s              = model_config_rec % gfdda_begin_s (grid%id)
 grid % auxinput11_begin_y         = model_config_rec % auxinput11_begin_y (grid%id)
 grid % auxinput11_begin_mo        = model_config_rec % auxinput11_begin_mo (grid%id)
 grid % auxinput11_begin_d         = model_config_rec % auxinput11_begin_d (grid%id)
 grid % auxinput11_begin_h         = model_config_rec % auxinput11_begin_h (grid%id)
 grid % auxinput11_begin_m         = model_config_rec % auxinput11_begin_m (grid%id)
 grid % auxinput11_begin_s         = model_config_rec % auxinput11_begin_s (grid%id)
 grid % restart_begin_y            = model_config_rec % restart_begin_y 
 grid % restart_begin_mo           = model_config_rec % restart_begin_mo 
 grid % restart_begin_d            = model_config_rec % restart_begin_d 
 grid % restart_begin_h            = model_config_rec % restart_begin_h 
 grid % restart_begin_m            = model_config_rec % restart_begin_m 
 grid % restart_begin_s            = model_config_rec % restart_begin_s 
 grid % history_end_y              = model_config_rec % history_end_y (grid%id)
 grid % history_end_mo             = model_config_rec % history_end_mo (grid%id)
 grid % history_end_d              = model_config_rec % history_end_d (grid%id)
 grid % history_end_h              = model_config_rec % history_end_h (grid%id)
 grid % history_end_m              = model_config_rec % history_end_m (grid%id)
 grid % history_end_s              = model_config_rec % history_end_s (grid%id)
 grid % inputout_end_y             = model_config_rec % inputout_end_y (grid%id)
 grid % inputout_end_mo            = model_config_rec % inputout_end_mo (grid%id)
 grid % inputout_end_d             = model_config_rec % inputout_end_d (grid%id)
 grid % inputout_end_h             = model_config_rec % inputout_end_h (grid%id)
 grid % inputout_end_m             = model_config_rec % inputout_end_m (grid%id)
 grid % inputout_end_s             = model_config_rec % inputout_end_s (grid%id)
 grid % auxhist1_end_y             = model_config_rec % auxhist1_end_y (grid%id)
 grid % auxhist1_end_mo            = model_config_rec % auxhist1_end_mo (grid%id)
 grid % auxhist1_end_d             = model_config_rec % auxhist1_end_d (grid%id)
 grid % auxhist1_end_h             = model_config_rec % auxhist1_end_h (grid%id)
 grid % auxhist1_end_m             = model_config_rec % auxhist1_end_m (grid%id)
 grid % auxhist1_end_s             = model_config_rec % auxhist1_end_s (grid%id)
 grid % auxhist2_end_y             = model_config_rec % auxhist2_end_y (grid%id)
 grid % auxhist2_end_mo            = model_config_rec % auxhist2_end_mo (grid%id)
 grid % auxhist2_end_d             = model_config_rec % auxhist2_end_d (grid%id)
 grid % auxhist2_end_h             = model_config_rec % auxhist2_end_h (grid%id)
 grid % auxhist2_end_m             = model_config_rec % auxhist2_end_m (grid%id)
 grid % auxhist2_end_s             = model_config_rec % auxhist2_end_s (grid%id)
 grid % auxhist3_end_y             = model_config_rec % auxhist3_end_y (grid%id)
 grid % auxhist3_end_mo            = model_config_rec % auxhist3_end_mo (grid%id)
 grid % auxhist3_end_d             = model_config_rec % auxhist3_end_d (grid%id)
 grid % auxhist3_end_h             = model_config_rec % auxhist3_end_h (grid%id)
 grid % auxhist3_end_m             = model_config_rec % auxhist3_end_m (grid%id)
 grid % auxhist3_end_s             = model_config_rec % auxhist3_end_s (grid%id)
 grid % auxhist4_end_y             = model_config_rec % auxhist4_end_y (grid%id)
 grid % auxhist4_end_mo            = model_config_rec % auxhist4_end_mo (grid%id)
 grid % auxhist4_end_d             = model_config_rec % auxhist4_end_d (grid%id)
 grid % auxhist4_end_h             = model_config_rec % auxhist4_end_h (grid%id)
 grid % auxhist4_end_m             = model_config_rec % auxhist4_end_m (grid%id)
 grid % auxhist4_end_s             = model_config_rec % auxhist4_end_s (grid%id)
 grid % auxhist5_end_y             = model_config_rec % auxhist5_end_y (grid%id)
 grid % auxhist5_end_mo            = model_config_rec % auxhist5_end_mo (grid%id)
 grid % auxhist5_end_d             = model_config_rec % auxhist5_end_d (grid%id)
 grid % auxhist5_end_h             = model_config_rec % auxhist5_end_h (grid%id)
 grid % auxhist5_end_m             = model_config_rec % auxhist5_end_m (grid%id)
 grid % auxhist5_end_s             = model_config_rec % auxhist5_end_s (grid%id)
 grid % auxhist6_end_y             = model_config_rec % auxhist6_end_y (grid%id)
 grid % auxhist6_end_mo            = model_config_rec % auxhist6_end_mo (grid%id)
 grid % auxhist6_end_d             = model_config_rec % auxhist6_end_d (grid%id)
 grid % auxhist6_end_h             = model_config_rec % auxhist6_end_h (grid%id)
 grid % auxhist6_end_m             = model_config_rec % auxhist6_end_m (grid%id)
 grid % auxhist6_end_s             = model_config_rec % auxhist6_end_s (grid%id)
 grid % auxhist7_end_y             = model_config_rec % auxhist7_end_y (grid%id)
 grid % auxhist7_end_mo            = model_config_rec % auxhist7_end_mo (grid%id)
 grid % auxhist7_end_d             = model_config_rec % auxhist7_end_d (grid%id)
 grid % auxhist7_end_h             = model_config_rec % auxhist7_end_h (grid%id)
 grid % auxhist7_end_m             = model_config_rec % auxhist7_end_m (grid%id)
 grid % auxhist7_end_s             = model_config_rec % auxhist7_end_s (grid%id)
 grid % auxhist8_end_y             = model_config_rec % auxhist8_end_y (grid%id)
 grid % auxhist8_end_mo            = model_config_rec % auxhist8_end_mo (grid%id)
 grid % auxhist8_end_d             = model_config_rec % auxhist8_end_d (grid%id)
 grid % auxhist8_end_h             = model_config_rec % auxhist8_end_h (grid%id)
 grid % auxhist8_end_m             = model_config_rec % auxhist8_end_m (grid%id)
 grid % auxhist8_end_s             = model_config_rec % auxhist8_end_s (grid%id)
 grid % auxhist9_end_y             = model_config_rec % auxhist9_end_y (grid%id)
 grid % auxhist9_end_mo            = model_config_rec % auxhist9_end_mo (grid%id)
 grid % auxhist9_end_d             = model_config_rec % auxhist9_end_d (grid%id)
 grid % auxhist9_end_h             = model_config_rec % auxhist9_end_h (grid%id)
 grid % auxhist9_end_m             = model_config_rec % auxhist9_end_m (grid%id)
 grid % auxhist9_end_s             = model_config_rec % auxhist9_end_s (grid%id)
 grid % auxhist10_end_y            = model_config_rec % auxhist10_end_y (grid%id)
 grid % auxhist10_end_mo           = model_config_rec % auxhist10_end_mo (grid%id)
 grid % auxhist10_end_d            = model_config_rec % auxhist10_end_d (grid%id)
 grid % auxhist10_end_h            = model_config_rec % auxhist10_end_h (grid%id)
 grid % auxhist10_end_m            = model_config_rec % auxhist10_end_m (grid%id)
 grid % auxhist10_end_s            = model_config_rec % auxhist10_end_s (grid%id)
 grid % auxhist11_end_y            = model_config_rec % auxhist11_end_y (grid%id)
 grid % auxhist11_end_mo           = model_config_rec % auxhist11_end_mo (grid%id)
 grid % auxhist11_end_d            = model_config_rec % auxhist11_end_d (grid%id)
 grid % auxhist11_end_h            = model_config_rec % auxhist11_end_h (grid%id)
 grid % auxhist11_end_m            = model_config_rec % auxhist11_end_m (grid%id)
 grid % auxhist11_end_s            = model_config_rec % auxhist11_end_s (grid%id)
 grid % auxinput1_end_y            = model_config_rec % auxinput1_end_y (grid%id)
 grid % auxinput1_end_mo           = model_config_rec % auxinput1_end_mo (grid%id)
 grid % auxinput1_end_d            = model_config_rec % auxinput1_end_d (grid%id)
 grid % auxinput1_end_h            = model_config_rec % auxinput1_end_h (grid%id)
 grid % auxinput1_end_m            = model_config_rec % auxinput1_end_m (grid%id)
 grid % auxinput1_end_s            = model_config_rec % auxinput1_end_s (grid%id)
 grid % auxinput2_end_y            = model_config_rec % auxinput2_end_y (grid%id)
 grid % auxinput2_end_mo           = model_config_rec % auxinput2_end_mo (grid%id)
 grid % auxinput2_end_d            = model_config_rec % auxinput2_end_d (grid%id)
 grid % auxinput2_end_h            = model_config_rec % auxinput2_end_h (grid%id)
 grid % auxinput2_end_m            = model_config_rec % auxinput2_end_m (grid%id)
 grid % auxinput2_end_s            = model_config_rec % auxinput2_end_s (grid%id)
 grid % auxinput3_end_y            = model_config_rec % auxinput3_end_y (grid%id)
 grid % auxinput3_end_mo           = model_config_rec % auxinput3_end_mo (grid%id)
 grid % auxinput3_end_d            = model_config_rec % auxinput3_end_d (grid%id)
 grid % auxinput3_end_h            = model_config_rec % auxinput3_end_h (grid%id)
 grid % auxinput3_end_m            = model_config_rec % auxinput3_end_m (grid%id)
 grid % auxinput3_end_s            = model_config_rec % auxinput3_end_s (grid%id)
 grid % auxinput4_end_y            = model_config_rec % auxinput4_end_y (grid%id)
 grid % auxinput4_end_mo           = model_config_rec % auxinput4_end_mo (grid%id)
 grid % auxinput4_end_d            = model_config_rec % auxinput4_end_d (grid%id)
 grid % auxinput4_end_h            = model_config_rec % auxinput4_end_h (grid%id)
 grid % auxinput4_end_m            = model_config_rec % auxinput4_end_m (grid%id)
 grid % auxinput4_end_s            = model_config_rec % auxinput4_end_s (grid%id)
 grid % auxinput5_end_y            = model_config_rec % auxinput5_end_y (grid%id)
 grid % auxinput5_end_mo           = model_config_rec % auxinput5_end_mo (grid%id)
 grid % auxinput5_end_d            = model_config_rec % auxinput5_end_d (grid%id)
 grid % auxinput5_end_h            = model_config_rec % auxinput5_end_h (grid%id)
 grid % auxinput5_end_m            = model_config_rec % auxinput5_end_m (grid%id)
 grid % auxinput5_end_s            = model_config_rec % auxinput5_end_s (grid%id)
 grid % auxinput6_end_y            = model_config_rec % auxinput6_end_y (grid%id)
 grid % auxinput6_end_mo           = model_config_rec % auxinput6_end_mo (grid%id)
 grid % auxinput6_end_d            = model_config_rec % auxinput6_end_d (grid%id)
 grid % auxinput6_end_h            = model_config_rec % auxinput6_end_h (grid%id)
 grid % auxinput6_end_m            = model_config_rec % auxinput6_end_m (grid%id)
 grid % auxinput6_end_s            = model_config_rec % auxinput6_end_s (grid%id)
 grid % auxinput7_end_y            = model_config_rec % auxinput7_end_y (grid%id)
 grid % auxinput7_end_mo           = model_config_rec % auxinput7_end_mo (grid%id)
 grid % auxinput7_end_d            = model_config_rec % auxinput7_end_d (grid%id)
 grid % auxinput7_end_h            = model_config_rec % auxinput7_end_h (grid%id)
 grid % auxinput7_end_m            = model_config_rec % auxinput7_end_m (grid%id)
 grid % auxinput7_end_s            = model_config_rec % auxinput7_end_s (grid%id)
 grid % auxinput8_end_y            = model_config_rec % auxinput8_end_y (grid%id)
 grid % auxinput8_end_mo           = model_config_rec % auxinput8_end_mo (grid%id)
 grid % auxinput8_end_d            = model_config_rec % auxinput8_end_d (grid%id)
 grid % auxinput8_end_h            = model_config_rec % auxinput8_end_h (grid%id)
 grid % auxinput8_end_m            = model_config_rec % auxinput8_end_m (grid%id)
 grid % auxinput8_end_s            = model_config_rec % auxinput8_end_s (grid%id)
 grid % auxinput9_end_y            = model_config_rec % auxinput9_end_y (grid%id)
 grid % auxinput9_end_mo           = model_config_rec % auxinput9_end_mo (grid%id)
 grid % auxinput9_end_d            = model_config_rec % auxinput9_end_d (grid%id)
 grid % auxinput9_end_h            = model_config_rec % auxinput9_end_h (grid%id)
 grid % auxinput9_end_m            = model_config_rec % auxinput9_end_m (grid%id)
 grid % auxinput9_end_s            = model_config_rec % auxinput9_end_s (grid%id)
 grid % gfdda_end_y                = model_config_rec % gfdda_end_y (grid%id)
 grid % gfdda_end_mo               = model_config_rec % gfdda_end_mo (grid%id)
 grid % gfdda_end_d                = model_config_rec % gfdda_end_d (grid%id)
 grid % gfdda_end_h                = model_config_rec % gfdda_end_h (grid%id)
 grid % gfdda_end_m                = model_config_rec % gfdda_end_m (grid%id)
 grid % gfdda_end_s                = model_config_rec % gfdda_end_s (grid%id)
 grid % auxinput11_end_y           = model_config_rec % auxinput11_end_y (grid%id)
 grid % auxinput11_end_mo          = model_config_rec % auxinput11_end_mo (grid%id)
 grid % auxinput11_end_d           = model_config_rec % auxinput11_end_d (grid%id)
 grid % auxinput11_end_h           = model_config_rec % auxinput11_end_h (grid%id)
 grid % auxinput11_end_m           = model_config_rec % auxinput11_end_m (grid%id)
 grid % auxinput11_end_s           = model_config_rec % auxinput11_end_s (grid%id)
 grid % io_form_auxinput1          = model_config_rec % io_form_auxinput1 
 grid % io_form_auxinput2          = model_config_rec % io_form_auxinput2 
 grid % io_form_auxinput3          = model_config_rec % io_form_auxinput3 
 grid % io_form_auxinput4          = model_config_rec % io_form_auxinput4 
 grid % io_form_auxinput5          = model_config_rec % io_form_auxinput5 
 grid % io_form_auxinput6          = model_config_rec % io_form_auxinput6 
 grid % io_form_auxinput7          = model_config_rec % io_form_auxinput7 
 grid % io_form_auxinput8          = model_config_rec % io_form_auxinput8 
 grid % io_form_auxinput9          = model_config_rec % io_form_auxinput9 
 grid % io_form_gfdda              = model_config_rec % io_form_gfdda 
 grid % io_form_auxinput11         = model_config_rec % io_form_auxinput11 
 grid % io_form_auxhist1           = model_config_rec % io_form_auxhist1 
 grid % io_form_auxhist2           = model_config_rec % io_form_auxhist2 
 grid % io_form_auxhist3           = model_config_rec % io_form_auxhist3 
 grid % io_form_auxhist4           = model_config_rec % io_form_auxhist4 
 grid % io_form_auxhist5           = model_config_rec % io_form_auxhist5 
 grid % io_form_auxhist6           = model_config_rec % io_form_auxhist6 
 grid % io_form_auxhist7           = model_config_rec % io_form_auxhist7 
 grid % io_form_auxhist8           = model_config_rec % io_form_auxhist8 
 grid % io_form_auxhist9           = model_config_rec % io_form_auxhist9 
 grid % io_form_auxhist10          = model_config_rec % io_form_auxhist10 
 grid % io_form_auxhist11          = model_config_rec % io_form_auxhist11 
 grid % auxinput1_inname           = model_config_rec % auxinput1_inname 
 grid % julyr                      = model_config_rec % julyr (grid%id)
 grid % julday                     = model_config_rec % julday (grid%id)
 grid % gmt                        = model_config_rec % gmt (grid%id)
 grid % input_inname               = model_config_rec % input_inname 
 grid % input_outname              = model_config_rec % input_outname 
 grid % bdy_inname                 = model_config_rec % bdy_inname 
 grid % bdy_outname                = model_config_rec % bdy_outname 
 grid % rst_inname                 = model_config_rec % rst_inname 
 grid % rst_outname                = model_config_rec % rst_outname 
 grid % write_input                = model_config_rec % write_input 
 grid % write_restart_at_0h        = model_config_rec % write_restart_at_0h 
 grid % adjust_output_times        = model_config_rec % adjust_output_times 
 grid % adjust_input_times         = model_config_rec % adjust_input_times 
 grid % diag_print                 = model_config_rec % diag_print 
 grid % nocolons                   = model_config_rec % nocolons 
 grid % time_step                  = model_config_rec % time_step 
 grid % time_step_fract_num        = model_config_rec % time_step_fract_num 
 grid % time_step_fract_den        = model_config_rec % time_step_fract_den 
 grid % max_dom                    = model_config_rec % max_dom 
 grid % s_we                       = model_config_rec % s_we (grid%id)
 grid % e_we                       = model_config_rec % e_we (grid%id)
 grid % s_sn                       = model_config_rec % s_sn (grid%id)
 grid % e_sn                       = model_config_rec % e_sn (grid%id)
 grid % s_vert                     = model_config_rec % s_vert (grid%id)
 grid % e_vert                     = model_config_rec % e_vert (grid%id)
 grid % num_metgrid_levels         = model_config_rec % num_metgrid_levels 
 grid % p_top_requested            = model_config_rec % p_top_requested 
 grid % interp_type                = model_config_rec % interp_type 
 grid % lowest_lev_from_sfc        = model_config_rec % lowest_lev_from_sfc 
 grid % lagrange_order             = model_config_rec % lagrange_order 
 grid % force_sfc_in_vinterp       = model_config_rec % force_sfc_in_vinterp 
 grid % zap_close_levels           = model_config_rec % zap_close_levels 
 grid % sfcp_to_sfcp               = model_config_rec % sfcp_to_sfcp 
 grid % adjust_heights             = model_config_rec % adjust_heights 
 grid % dx                         = model_config_rec % dx (grid%id)
 grid % dy                         = model_config_rec % dy (grid%id)
 grid % grid_id                    = model_config_rec % grid_id (grid%id)
 grid % parent_id                  = model_config_rec % parent_id (grid%id)
 grid % i_parent_start             = model_config_rec % i_parent_start (grid%id)
 grid % j_parent_start             = model_config_rec % j_parent_start (grid%id)
 grid % parent_grid_ratio          = model_config_rec % parent_grid_ratio (grid%id)
 grid % parent_time_step_ratio     = model_config_rec % parent_time_step_ratio (grid%id)
 grid % feedback                   = model_config_rec % feedback 
 grid % smooth_option              = model_config_rec % smooth_option 
 grid % blend_width                = model_config_rec % blend_width 
 grid % ztop                       = model_config_rec % ztop (grid%id)
 grid % moad_grid_ratio            = model_config_rec % moad_grid_ratio (grid%id)
 grid % moad_time_step_ratio       = model_config_rec % moad_time_step_ratio (grid%id)
 grid % shw                        = model_config_rec % shw (grid%id)
 grid % tile_sz_x                  = model_config_rec % tile_sz_x 
 grid % tile_sz_y                  = model_config_rec % tile_sz_y 
 grid % numtiles                   = model_config_rec % numtiles 
 grid % nproc_x                    = model_config_rec % nproc_x 
 grid % nproc_y                    = model_config_rec % nproc_y 
 grid % irand                      = model_config_rec % irand 
 grid % dt                         = model_config_rec % dt (grid%id)
 grid % num_moves                  = model_config_rec % num_moves 
 grid % vortex_interval            = model_config_rec % vortex_interval (grid%id)
 grid % max_vortex_speed           = model_config_rec % max_vortex_speed (grid%id)
 grid % corral_dist                = model_config_rec % corral_dist (grid%id)
 grid % move_id                    = model_config_rec % move_id (grid%id)
 grid % move_interval              = model_config_rec % move_interval (grid%id)
 grid % move_cd_x                  = model_config_rec % move_cd_x (grid%id)
 grid % move_cd_y                  = model_config_rec % move_cd_y (grid%id)
 grid % swap_x                     = model_config_rec % swap_x (grid%id)
 grid % swap_y                     = model_config_rec % swap_y (grid%id)
 grid % cycle_x                    = model_config_rec % cycle_x (grid%id)
 grid % cycle_y                    = model_config_rec % cycle_y (grid%id)
 grid % reorder_mesh               = model_config_rec % reorder_mesh 
 grid % perturb_input              = model_config_rec % perturb_input 
 grid % eta_levels                 = model_config_rec % eta_levels (grid%id)
 grid % max_dz                     = model_config_rec % max_dz 
 grid % mp_physics                 = model_config_rec % mp_physics (grid%id)
 grid % ra_lw_physics              = model_config_rec % ra_lw_physics (grid%id)
 grid % ra_sw_physics              = model_config_rec % ra_sw_physics (grid%id)
 grid % radt                       = model_config_rec % radt (grid%id)
 grid % sf_sfclay_physics          = model_config_rec % sf_sfclay_physics (grid%id)
 grid % sf_surface_physics         = model_config_rec % sf_surface_physics (grid%id)
 grid % bl_pbl_physics             = model_config_rec % bl_pbl_physics (grid%id)
 grid % bldt                       = model_config_rec % bldt (grid%id)
 grid % cu_physics                 = model_config_rec % cu_physics (grid%id)
 grid % cudt                       = model_config_rec % cudt (grid%id)
 grid % gsmdt                      = model_config_rec % gsmdt (grid%id)
 grid % isfflx                     = model_config_rec % isfflx 
 grid % ifsnow                     = model_config_rec % ifsnow 
 grid % icloud                     = model_config_rec % icloud 
 grid % swrad_scat                 = model_config_rec % swrad_scat 
 grid % surface_input_source       = model_config_rec % surface_input_source 
 grid % num_soil_layers            = model_config_rec % num_soil_layers 
 grid % num_months                 = model_config_rec % num_months 
 grid % maxiens                    = model_config_rec % maxiens 
 grid % maxens                     = model_config_rec % maxens 
 grid % maxens2                    = model_config_rec % maxens2 
 grid % maxens3                    = model_config_rec % maxens3 
 grid % ensdim                     = model_config_rec % ensdim 
 grid % chem_opt                   = model_config_rec % chem_opt (grid%id)
 grid % num_land_cat               = model_config_rec % num_land_cat 
 grid % num_soil_cat               = model_config_rec % num_soil_cat 
 grid % mp_zero_out                = model_config_rec % mp_zero_out 
 grid % mp_zero_out_thresh         = model_config_rec % mp_zero_out_thresh 
 grid % seaice_threshold           = model_config_rec % seaice_threshold 
 grid % sst_update                 = model_config_rec % sst_update 
 grid % ucmcall                    = model_config_rec % ucmcall 
 grid % co2tf                      = model_config_rec % co2tf 
 grid % ra_call_offset             = model_config_rec % ra_call_offset 
 grid % cam_abs_freq_s             = model_config_rec % cam_abs_freq_s 
 grid % levsiz                     = model_config_rec % levsiz 
 grid % paerlev                    = model_config_rec % paerlev 
 grid % cam_abs_dim1               = model_config_rec % cam_abs_dim1 
 grid % cam_abs_dim2               = model_config_rec % cam_abs_dim2 
 grid % cu_rad_feedback            = model_config_rec % cu_rad_feedback (grid%id)
 grid % fgdt                       = model_config_rec % fgdt (grid%id)
 grid % grid_fdda                  = model_config_rec % grid_fdda (grid%id)
 grid % if_no_pbl_nudging_uv       = model_config_rec % if_no_pbl_nudging_uv (grid%id)
 grid % if_no_pbl_nudging_t        = model_config_rec % if_no_pbl_nudging_t (grid%id)
 grid % if_no_pbl_nudging_q        = model_config_rec % if_no_pbl_nudging_q (grid%id)
 grid % if_zfac_uv                 = model_config_rec % if_zfac_uv (grid%id)
 grid % k_zfac_uv                  = model_config_rec % k_zfac_uv (grid%id)
 grid % if_zfac_t                  = model_config_rec % if_zfac_t (grid%id)
 grid % k_zfac_t                   = model_config_rec % k_zfac_t (grid%id)
 grid % if_zfac_q                  = model_config_rec % if_zfac_q (grid%id)
 grid % k_zfac_q                   = model_config_rec % k_zfac_q (grid%id)
 grid % guv                        = model_config_rec % guv (grid%id)
 grid % gt                         = model_config_rec % gt (grid%id)
 grid % gq                         = model_config_rec % gq (grid%id)
 grid % dtramp_min                 = model_config_rec % dtramp_min 
 grid % if_ramping                 = model_config_rec % if_ramping 
 grid % obs_nudge_opt              = model_config_rec % obs_nudge_opt (grid%id)
 grid % max_obs                    = model_config_rec % max_obs 
 grid % nobs_ndg_vars              = model_config_rec % nobs_ndg_vars 
 grid % nobs_err_flds              = model_config_rec % nobs_err_flds 
 grid % fdda_start                 = model_config_rec % fdda_start (grid%id)
 grid % fdda_end                   = model_config_rec % fdda_end (grid%id)
 grid % obs_nudge_wind             = model_config_rec % obs_nudge_wind (grid%id)
 grid % obs_coef_wind              = model_config_rec % obs_coef_wind (grid%id)
 grid % obs_nudge_temp             = model_config_rec % obs_nudge_temp (grid%id)
 grid % obs_coef_temp              = model_config_rec % obs_coef_temp (grid%id)
 grid % obs_nudge_mois             = model_config_rec % obs_nudge_mois (grid%id)
 grid % obs_coef_mois              = model_config_rec % obs_coef_mois (grid%id)
 grid % obs_nudge_pstr             = model_config_rec % obs_nudge_pstr (grid%id)
 grid % obs_coef_pstr              = model_config_rec % obs_coef_pstr (grid%id)
 grid % obs_rinxy                  = model_config_rec % obs_rinxy (grid%id)
 grid % obs_rinsig                 = model_config_rec % obs_rinsig 
 grid % obs_twindo                 = model_config_rec % obs_twindo 
 grid % obs_npfi                   = model_config_rec % obs_npfi 
 grid % obs_ionf                   = model_config_rec % obs_ionf 
 grid % obs_idynin                 = model_config_rec % obs_idynin 
 grid % obs_dtramp                 = model_config_rec % obs_dtramp 
 grid % obs_ipf_in4dob             = model_config_rec % obs_ipf_in4dob 
 grid % obs_ipf_errob              = model_config_rec % obs_ipf_errob 
 grid % obs_ipf_nudob              = model_config_rec % obs_ipf_nudob 
 grid % dyn_opt                    = model_config_rec % dyn_opt 
 grid % rk_ord                     = model_config_rec % rk_ord 
 grid % w_damping                  = model_config_rec % w_damping 
 grid % diff_opt                   = model_config_rec % diff_opt 
 grid % km_opt                     = model_config_rec % km_opt 
 grid % damp_opt                   = model_config_rec % damp_opt 
 grid % zdamp                      = model_config_rec % zdamp (grid%id)
 grid % dampcoef                   = model_config_rec % dampcoef (grid%id)
 grid % khdif                      = model_config_rec % khdif (grid%id)
 grid % kvdif                      = model_config_rec % kvdif (grid%id)
 grid % diff_6th_factor            = model_config_rec % diff_6th_factor (grid%id)
 grid % diff_6th_opt               = model_config_rec % diff_6th_opt (grid%id)
 grid % smdiv                      = model_config_rec % smdiv (grid%id)
 grid % emdiv                      = model_config_rec % emdiv (grid%id)
 grid % epssm                      = model_config_rec % epssm (grid%id)
 grid % non_hydrostatic            = model_config_rec % non_hydrostatic (grid%id)
 grid % time_step_sound            = model_config_rec % time_step_sound (grid%id)
 grid % h_mom_adv_order            = model_config_rec % h_mom_adv_order (grid%id)
 grid % v_mom_adv_order            = model_config_rec % v_mom_adv_order (grid%id)
 grid % h_sca_adv_order            = model_config_rec % h_sca_adv_order (grid%id)
 grid % v_sca_adv_order            = model_config_rec % v_sca_adv_order (grid%id)
 grid % pd_moist                   = model_config_rec % pd_moist (grid%id)
 grid % pd_chem                    = model_config_rec % pd_chem (grid%id)
 grid % pd_scalar                  = model_config_rec % pd_scalar (grid%id)
 grid % pd_tke                     = model_config_rec % pd_tke (grid%id)
 grid % top_radiation              = model_config_rec % top_radiation (grid%id)
 grid % mix_cr_len                 = model_config_rec % mix_cr_len (grid%id)
 grid % tke_upper_bound            = model_config_rec % tke_upper_bound (grid%id)
 grid % kh_tke_upper_bound         = model_config_rec % kh_tke_upper_bound (grid%id)
 grid % kv_tke_upper_bound         = model_config_rec % kv_tke_upper_bound (grid%id)
 grid % tke_drag_coefficient       = model_config_rec % tke_drag_coefficient (grid%id)
 grid % tke_heat_flux              = model_config_rec % tke_heat_flux (grid%id)
 grid % pert_coriolis              = model_config_rec % pert_coriolis (grid%id)
 grid % mix_full_fields            = model_config_rec % mix_full_fields (grid%id)
 grid % base_pres                  = model_config_rec % base_pres 
 grid % base_temp                  = model_config_rec % base_temp 
 grid % base_lapse                 = model_config_rec % base_lapse 
 grid % spec_bdy_width             = model_config_rec % spec_bdy_width 
 grid % spec_zone                  = model_config_rec % spec_zone 
 grid % relax_zone                 = model_config_rec % relax_zone 
 grid % specified                  = model_config_rec % specified (grid%id)
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
 grid % real_data_init_type        = model_config_rec % real_data_init_type 
 grid % background_proc_id         = model_config_rec % background_proc_id 
 grid % forecast_proc_id           = model_config_rec % forecast_proc_id 
 grid % production_status          = model_config_rec % production_status 
 grid % compression                = model_config_rec % compression 
 grid % cen_lat                    = model_config_rec % cen_lat (grid%id)
 grid % cen_lon                    = model_config_rec % cen_lon (grid%id)
 grid % truelat1                   = model_config_rec % truelat1 (grid%id)
 grid % truelat2                   = model_config_rec % truelat2 (grid%id)
 grid % moad_cen_lat               = model_config_rec % moad_cen_lat (grid%id)
 grid % stand_lon                  = model_config_rec % stand_lon (grid%id)
 grid % bdyfrq                     = model_config_rec % bdyfrq (grid%id)
 grid % iswater                    = model_config_rec % iswater (grid%id)
 grid % isice                      = model_config_rec % isice (grid%id)
 grid % isurban                    = model_config_rec % isurban (grid%id)
 grid % isoilwater                 = model_config_rec % isoilwater (grid%id)
 grid % map_proj                   = model_config_rec % map_proj (grid%id)
 grid % simulation_start_year      = model_config_rec % simulation_start_year 
 grid % simulation_start_month     = model_config_rec % simulation_start_month 
 grid % simulation_start_day       = model_config_rec % simulation_start_day 
 grid % simulation_start_hour      = model_config_rec % simulation_start_hour 
 grid % simulation_start_minute    = model_config_rec % simulation_start_minute 
 grid % simulation_start_second    = model_config_rec % simulation_start_second 
!ENDOFREGISTRYGENERATEDINCLUDE

   RETURN

END SUBROUTINE med_add_config_info_to_grid

