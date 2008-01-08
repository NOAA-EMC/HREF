!WRF:DRIVER_LAYER:MAIN
!

PROGRAM ndown_em

   USE module_machine
   USE module_domain
   USE module_initialize
   USE module_integrate
   USE module_driver_constants
   USE module_configure
   USE module_io_domain
   USE module_utility

   USE module_timing
   USE module_wrf_error
   USE module_dm

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!new for bc
   USE module_bc
   USE module_big_step_utilities_em
   USE module_get_file_names

   IMPLICIT NONE
 ! interface
   INTERFACE
     ! mediation-supplied
     SUBROUTINE med_read_wrf_chem_bioemiss ( grid , config_flags)
       USE module_domain
       TYPE (domain) grid
       TYPE (grid_config_rec_type) config_flags
     END SUBROUTINE med_read_wrf_chem_bioemiss

     SUBROUTINE init_domain_constants_em_ptr ( parent , nest )
       USE module_domain
       USE module_configure
       TYPE(domain), POINTER  :: parent , nest
     END SUBROUTINE init_domain_constants_em_ptr

   END INTERFACE



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!new for bc
   INTEGER :: ids , ide , jds , jde , kds , kde
   INTEGER :: ims , ime , jms , jme , kms , kme
   INTEGER :: ips , ipe , jps , jpe , kps , kpe
   INTEGER :: its , ite , jts , jte , kts , kte
   INTEGER :: ijds , ijde , spec_bdy_width
   INTEGER :: i , j , k , nvchem
   INTEGER :: time_loop_max , time_loop
   INTEGER :: total_time_sec , file_counter
   INTEGER :: julyr , julday , iswater , map_proj
   INTEGER :: icnt

   REAL    :: dt , new_bdy_frq
   REAL    :: gmt , cen_lat , cen_lon , dx , dy , truelat1 , truelat2 , moad_cen_lat , stand_lon

   REAL , DIMENSION(:,:,:) , ALLOCATABLE :: ubdy3dtemp1 , vbdy3dtemp1 , tbdy3dtemp1 , pbdy3dtemp1 , qbdy3dtemp1
   REAL , DIMENSION(:,:,:) , ALLOCATABLE :: mbdy2dtemp1
   REAL , DIMENSION(:,:,:) , ALLOCATABLE :: ubdy3dtemp2 , vbdy3dtemp2 , tbdy3dtemp2 , pbdy3dtemp2 , qbdy3dtemp2
   REAL , DIMENSION(:,:,:) , ALLOCATABLE :: mbdy2dtemp2
   REAL , DIMENSION(:,:,:) , ALLOCATABLE :: cbdy3dtemp1 , cbdy3dtemp2 
   REAL , DIMENSION(:,:,:,:) , ALLOCATABLE :: cbdy3dtemp0

   CHARACTER(LEN=19) :: start_date_char , current_date_char , end_date_char
   CHARACTER(LEN=19) :: stopTimeStr

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   INTEGER :: num_veg_cat , num_soil_top_cat , num_soil_bot_cat

   REAL    :: time
   INTEGER :: rc

   INTEGER :: loop , levels_to_process
   INTEGER , PARAMETER :: max_sanity_file_loop = 100

   TYPE (domain) , POINTER :: keep_grid, grid_ptr, null_domain, parent_grid , nested_grid
   TYPE (domain)           :: dummy
   TYPE (grid_config_rec_type)              :: config_flags
   INTEGER                 :: number_at_same_level
   INTEGER                 :: time_step_begin_restart

   INTEGER :: max_dom , domain_id , fid , fido, fidb , oid , idum1 , idum2 , ierr
   INTEGER :: status_next_var
   INTEGER :: debug_level
   LOGICAL :: input_from_file , need_new_file
   CHARACTER (LEN=19) :: date_string

   INTEGER                 :: nbytes
   INTEGER, PARAMETER      :: configbuflen = 4* 16384
   INTEGER                 :: configbuf( configbuflen )
   LOGICAL , EXTERNAL      :: wrf_dm_on_monitor

   INTEGER                 :: idsi
   CHARACTER (LEN=80)      :: inpname , outname , bdyname
   CHARACTER (LEN=80)      :: si_inpname
character *19 :: temp19
character *24 :: temp24 , temp24b
character(len=24) :: start_date_hold

   CHARACTER (LEN=80)      :: message
integer :: ii

   CHARACTER (LEN=10) :: release_version = 'V2.2      '

   !  Interface block for routine that passes pointers and needs to know that they
   !  are receiving pointers.

   INTERFACE

      SUBROUTINE med_interp_domain ( parent_grid , nested_grid )
         USE module_domain
         USE module_configure
         TYPE(domain), POINTER :: parent_grid , nested_grid
      END SUBROUTINE med_interp_domain

      SUBROUTINE Setup_Timekeeping( parent_grid )
         USE module_domain
         TYPE(domain), POINTER :: parent_grid
      END SUBROUTINE Setup_Timekeeping

   END INTERFACE

   !  Define the name of this program (program_name defined in module_domain)

   program_name = "NDOWN_EM " // TRIM(release_version) // " PREPROCESSOR"

   CALL disable_quilting

   !  Initialize the modules used by the WRF system.  Many of the CALLs made from the
   !  init_modules routine are NO-OPs.  Typical initializations are: the size of a 
   !  REAL, setting the file handles to a pre-use value, defining moisture and 
   !  chemistry indices, etc.

   CALL init_modules(1)   ! Phase 1 returns after MPI_INIT() (if it is called)
   CALL WRFU_Initialize( defaultCalendar=WRFU_CAL_GREGORIAN, rc=rc )
   CALL init_modules(2)   ! Phase 2 resumes after MPI_INIT() (if it is called)

   !  Get the NAMELIST data.  This is handled in the initial_config routine.  All of the
   !  NAMELIST input variables are assigned to the model_config_rec structure.  Below,
   !  note for parallel processing, only the monitor processor handles the raw Fortran
   !  I/O, and then broadcasts the info to each of the other nodes.

   IF ( wrf_dm_on_monitor() ) THEN
     CALL initial_config
   ENDIF
   CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
   CALL wrf_dm_bcast_bytes( configbuf, nbytes )
   CALL set_config_as_buffer( configbuf, configbuflen )
   CALL wrf_dm_initialize

   !  And here is an instance of using the information in the NAMELIST.  

   CALL nl_get_debug_level ( 1, debug_level )
   CALL set_wrf_debug_level ( debug_level )

   !  Allocated and configure the mother domain.  Since we are in the nesting down
   !  mode, we know a) we got a nest, and b) we only got 1 nest.

   NULLIFY( null_domain )

   CALL       wrf_message ( program_name )
   CALL       wrf_debug ( 100 , 'ndown_em: calling alloc_and_configure_domain coarse ' )
   CALL alloc_and_configure_domain ( domain_id  = 1 ,                  &
                                     grid       = head_grid ,          &
                                     parent     = null_domain ,        &
                                     kid        = -1                   )

   parent_grid => head_grid

   !  Set up time initializations.

   CALL Setup_Timekeeping ( parent_grid )

   CALL domain_clock_set( head_grid, &
                          time_step_seconds=model_config_rec%interval_seconds )
   CALL       wrf_debug ( 100 , 'ndown_em: calling model_to_grid_config_rec ' )
   CALL model_to_grid_config_rec ( parent_grid%id , model_config_rec , config_flags )
   CALL       wrf_debug ( 100 , 'ndown_em: calling set_scalar_indices_from_config ' )
   CALL set_scalar_indices_from_config ( parent_grid%id , idum1, idum2 )

   !  Initialize the I/O for WRF.

   CALL       wrf_debug ( 100 , 'ndown_em: calling init_wrfio' )
   CALL init_wrfio

   !  Some of the configuration values may have been modified from the initial READ
   !  of the NAMELIST, so we re-broadcast the configuration records.

   CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
   CALL wrf_dm_bcast_bytes( configbuf, nbytes )
   CALL set_config_as_buffer( configbuf, configbuflen )

   !  We need to current and starting dates for the output files.  The times need to be incremented
   !  so that the lateral BC files are not overwritten.

   WRITE ( start_date_char , FMT = '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)' ) &
           model_config_rec%start_year  (parent_grid%id) , &
           model_config_rec%start_month (parent_grid%id) , &
           model_config_rec%start_day   (parent_grid%id) , &
           model_config_rec%start_hour  (parent_grid%id) , &
           model_config_rec%start_minute(parent_grid%id) , &
           model_config_rec%start_second(parent_grid%id) 

   WRITE (   end_date_char , FMT = '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)' ) &
           model_config_rec%  end_year  (parent_grid%id) , &
           model_config_rec%  end_month (parent_grid%id) , &
           model_config_rec%  end_day   (parent_grid%id) , &
           model_config_rec%  end_hour  (parent_grid%id) , &
           model_config_rec%  end_minute(parent_grid%id) , &
           model_config_rec%  end_second(parent_grid%id) 

   !  Override stop time with value computed above.
   CALL domain_clock_set( parent_grid, stop_timestr=end_date_char )

   CALL geth_idts ( end_date_char , start_date_char , total_time_sec ) 

   new_bdy_frq = model_config_rec%interval_seconds
   time_loop_max = total_time_sec / model_config_rec%interval_seconds + 1

   start_date        = start_date_char // '.0000' 
   current_date      = start_date_char // '.0000' 
   start_date_hold   = start_date_char // '.0000'
   current_date_char = start_date_char

   !  Get a list of available file names to try.  This fills up the eligible_file_name
   !  array with number_of_eligible_files entries.  This routine issues a nonstandard
   !  call (system).

   file_counter = 1
   need_new_file = .FALSE.
   CALL unix_ls ( 'wrfout' , parent_grid%id )

   !  Open the input data (wrfout_d01_xxxxxx) for reading.
   
   CALL wrf_debug          ( 100 , 'ndown_em main: calling open_r_dataset for ' // TRIM(eligible_file_name(file_counter)) )
   CALL open_r_dataset     ( fid, TRIM(eligible_file_name(file_counter)) , head_grid , config_flags , "DATASET=AUXINPUT1", ierr )
   IF ( ierr .NE. 0 ) THEN
      WRITE( wrf_err_message , FMT='(A,A,A,I8)' ) 'program ndown: error opening ',TRIM(eligible_file_name(file_counter)), &
                                                  ' for reading ierr=',ierr
      CALL wrf_error_fatal3 ( "ndown_em.b" , 264 ,  wrf_err_message )
   ENDIF

   !  We know how many time periods to process, so we begin.

   big_time_loop_thingy : DO time_loop = 1 , time_loop_max

      !  Which date are we currently soliciting?

      CALL geth_newdate ( date_string , start_date_char , ( time_loop - 1 ) * NINT ( new_bdy_frq) )
print *,'-------->>>  Processing data: loop=',time_loop,'  date/time = ',date_string
      current_date_char = date_string
      current_date      = date_string // '.0000'
      start_date        = date_string // '.0000'
print *,'loopmax = ', time_loop_max, '   ending date = ',end_date_char
      CALL domain_clock_set( parent_grid, &
                             current_timestr=current_date(1:19) )

      !  Which times are in this file, and more importantly, are any of them the
      !  ones that we want?  We need to loop over times in each files, loop
      !  over files.

      get_the_right_time : DO
      
         CALL wrf_get_next_time ( fid , date_string , status_next_var )
print *,'file date/time = ',date_string,'     desired date = ',current_date_char,'     status = ', status_next_var

         IF      (  status_next_var .NE. 0 ) THEN
            CALL wrf_debug          ( 100 , 'ndown_em main: calling close_dataset  for ' // TRIM(eligible_file_name(file_counter)) )
            CALL close_dataset      ( fid , config_flags , "DATASET=INPUT" )
            file_counter = file_counter + 1
            IF ( file_counter .GT. number_of_eligible_files ) THEN
               WRITE( wrf_err_message , FMT='(A,A,A,I8)' ) 'program ndown: opening too many files'
               CALL wrf_error_fatal3 ( "ndown_em.b" , 297 ,  wrf_err_message )
            END IF
            CALL wrf_debug      ( 100 , 'ndown_em main: calling open_r_dataset for ' // TRIM(eligible_file_name(file_counter)) )
            CALL open_r_dataset ( fid, TRIM(eligible_file_name(file_counter)) , head_grid , config_flags , "DATASET=INPUT", ierr )
            IF ( ierr .NE. 0 ) THEN
               WRITE( wrf_err_message , FMT='(A,A,A,I8)' ) 'program ndown: error opening ',TRIM(eligible_file_name(file_counter)), &
                                                           ' for reading ierr=',ierr
               CALL wrf_error_fatal3 ( "ndown_em.b" , 304 ,  wrf_err_message )
            ENDIF
            CYCLE get_the_right_time
         ELSE IF ( TRIM(date_string) .LT. TRIM(current_date_char) ) THEN
            CYCLE get_the_right_time
         ELSE IF ( TRIM(date_string) .EQ. TRIM(current_date_char) ) THEN
            EXIT get_the_right_time
         ELSE IF ( TRIM(date_string) .GT. TRIM(current_date_char) ) THEN
            WRITE( wrf_err_message , FMT='(A,A,A,A,A)' ) 'Found ',TRIM(date_string),' before I found ',TRIM(current_date_char),'.'
            CALL wrf_error_fatal3 ( "ndown_em.b" , 313 ,  wrf_err_message )
         END IF
      END DO get_the_right_time 

      CALL wrf_debug          ( 100 , 'wrf: calling input_history' )
      CALL wrf_get_previous_time ( fid , date_string , status_next_var )
      CALL input_history      ( fid , head_grid , config_flags, ierr )
      CALL wrf_debug          ( 100 , 'wrf: back from input_history' )

      !  Get the coarse grid info for later transfer to the fine grid domain.

      CALL wrf_get_dom_ti_integer ( fid , 'MAP_PROJ' , map_proj , 1 , icnt , ierr ) 
      CALL wrf_get_dom_ti_real    ( fid , 'DX'  , dx  , 1 , icnt , ierr ) 
      CALL wrf_get_dom_ti_real    ( fid , 'DY'  , dy  , 1 , icnt , ierr ) 
      CALL wrf_get_dom_ti_real    ( fid , 'CEN_LAT' , cen_lat , 1 , icnt , ierr ) 
      CALL wrf_get_dom_ti_real    ( fid , 'CEN_LON' , cen_lon , 1 , icnt , ierr ) 
      CALL wrf_get_dom_ti_real    ( fid , 'TRUELAT1' , truelat1 , 1 , icnt , ierr ) 
      CALL wrf_get_dom_ti_real    ( fid , 'TRUELAT2' , truelat2 , 1 , icnt , ierr ) 
      CALL wrf_get_dom_ti_real    ( fid , 'MOAD_CEN_LAT' , moad_cen_lat , 1 , icnt , ierr ) 
      CALL wrf_get_dom_ti_real    ( fid , 'STAND_LON' , stand_lon , 1 , icnt , ierr ) 
!     CALL wrf_get_dom_ti_real    ( fid , GMT , gmt , 1 , icnt , ierr ) 
!     CALL wrf_get_dom_ti_integer ( fid , JULYR , julyr , 1 , icnt , ierr ) 
!     CALL wrf_get_dom_ti_integer ( fid , JULDAY , julday , 1 , icnt , ierr ) 
      CALL wrf_get_dom_ti_integer ( fid , 'ISWATER' , iswater , 1 , icnt , ierr ) 

      !  First time in, do this: allocate sapce for the fine grid, get the config flags, open the 
      !  wrfinput and wrfbdy files.  This COULD be done outside the time loop, I think, so check it
      !  out and move it up if you can.

      IF ( time_loop .EQ. 1 ) THEN

         CALL       wrf_message ( program_name )
         CALL       wrf_debug ( 100 , 'wrf: calling alloc_and_configure_domain fine ' )
         CALL alloc_and_configure_domain ( domain_id  = 2 ,                  &
                                           grid       = nested_grid ,        &
                                           parent     = parent_grid ,        &
                                           kid        = 1                   )
   
         CALL       wrf_debug ( 100 , 'wrf: calling model_to_grid_config_rec ' )
         CALL model_to_grid_config_rec ( nested_grid%id , model_config_rec , config_flags )
         CALL       wrf_debug ( 100 , 'wrf: calling set_scalar_indices_from_config ' )
         CALL set_scalar_indices_from_config ( nested_grid%id , idum1, idum2 )

         !  Set up time initializations for the fine grid.

         CALL Setup_Timekeeping ( nested_grid )
         ! Strictly speaking, nest stop time should come from model_config_rec...  
         CALL domain_clock_get( parent_grid, stop_timestr=stopTimeStr )
         CALL domain_clock_set( nested_grid,                        &
                                current_timestr=current_date(1:19), &
                                stop_timestr=stopTimeStr ,          &
                                time_step_seconds=                  &
                                  model_config_rec%interval_seconds )

         !  Generate an output file from this program, which will be an input file to WRF.

         CALL nl_set_bdyfrq ( nested_grid%id , new_bdy_frq )
         config_flags%bdyfrq = new_bdy_frq


         !  Initialize constants and 1d arrays in fine grid from the parent.

         CALL init_domain_constants_em_ptr ( parent_grid , nested_grid ) 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   
         CALL wrf_debug          ( 100 , 'ndown_em main: calling open_w_dataset for wrfinput' )
         CALL construct_filename1( outname , 'wrfinput' , nested_grid%id , 2 )
         CALL open_w_dataset     ( fido, TRIM(outname) , nested_grid , config_flags , output_model_input , "DATASET=INPUT", ierr )
         IF ( ierr .NE. 0 ) THEN
            WRITE( wrf_err_message , FMT='(A,A,A,I8)' ) 'program ndown: error opening ',TRIM(outname),' for reading ierr=',ierr
            CALL wrf_error_fatal3 ( "ndown_em.b" , 388 ,  wrf_err_message )
         ENDIF

         !  Various sizes that we need to be concerned about.

         ids = nested_grid%sd31
         ide = nested_grid%ed31
         kds = nested_grid%sd32
         kde = nested_grid%ed32
         jds = nested_grid%sd33
         jde = nested_grid%ed33

         ims = nested_grid%sm31
         ime = nested_grid%em31
         kms = nested_grid%sm32
         kme = nested_grid%em32
         jms = nested_grid%sm33
         jme = nested_grid%em33

         ips = nested_grid%sp31
         ipe = nested_grid%ep31
         kps = nested_grid%sp32
         kpe = nested_grid%ep32
         jps = nested_grid%sp33
         jpe = nested_grid%ep33

         ijds = MIN ( ids , jds )
         ijde = MAX ( ide , jde )

         print *, ids , ide , jds , jde , kds , kde
         print *, ims , ime , jms , jme , kms , kme
         print *, ips , ipe , jps , jpe , kps , kpe
         print *, ijds , ijde

         spec_bdy_width = model_config_rec%spec_bdy_width
         print *,'spec_bdy_width=',spec_bdy_width

         !  This is the space needed to save the current 3d data for use in computing
         !  the lateral boundary tendencies.

         ALLOCATE ( ubdy3dtemp1(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( vbdy3dtemp1(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( tbdy3dtemp1(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( pbdy3dtemp1(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( qbdy3dtemp1(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( mbdy2dtemp1(ims:ime,1:1,    jms:jme) )
         ALLOCATE ( ubdy3dtemp2(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( vbdy3dtemp2(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( tbdy3dtemp2(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( pbdy3dtemp2(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( qbdy3dtemp2(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( mbdy2dtemp2(ims:ime,1:1,    jms:jme) )
         ALLOCATE ( cbdy3dtemp0(ims:ime,kms:kme,jms:jme,1:num_chem) )
         ALLOCATE ( cbdy3dtemp1(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( cbdy3dtemp2(ims:ime,kms:kme,jms:jme) )

      END IF

      CALL domain_clock_set( nested_grid,                        &
                             current_timestr=current_date(1:19), &
                             time_step_seconds=                  &
                               model_config_rec%interval_seconds )

      !  Do the horizontal interpolation.

      nested_grid%imask_nostag = 1
      nested_grid%imask_xstag = 1
      nested_grid%imask_ystag = 1
      nested_grid%imask_xystag = 1
      CALL med_interp_domain ( head_grid , nested_grid )
      nested_grid%ht_int = nested_grid%ht

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      IF ( time_loop .EQ. 1 ) THEN

         !  Open the fine grid SI static file.
   
         CALL construct_filename1( si_inpname , 'wrfndi' , nested_grid%id , 2 )
         CALL       wrf_debug ( 100 , 'med_sidata_input: calling open_r_dataset for ' // TRIM(si_inpname) )
         CALL open_r_dataset ( idsi, TRIM(si_inpname) , nested_grid , config_flags , "DATASET=INPUT", ierr )
         IF ( ierr .NE. 0 ) THEN
            CALL wrf_error_fatal3 ( "ndown_em.b" , 470 ,  'real: error opening FG input for reading: ' // TRIM (si_inpname) )
         END IF

         !  Input data.
   
         CALL       wrf_debug ( 100 , 'ndown_em: calling input_aux_model_input2' )
         CALL input_aux_model_input2 ( idsi , nested_grid , config_flags , ierr )
         nested_grid%ht_input = nested_grid%ht
   
         !  Close this fine grid static input file.
   
         CALL       wrf_debug ( 100 , 'ndown_em: closing fine grid static input' )
         CALL close_dataset ( idsi , config_flags , "DATASET=INPUT" )

         !  We need a fine grid landuse in the interpolation.  So we need to generate
         !  that field now.

         IF      ( ( nested_grid%ivgtyp(ips,jps) .GT. 0 ) .AND. &
                   ( nested_grid%isltyp(ips,jps) .GT. 0 ) ) THEN
            DO j = jps, MIN(jde-1,jpe)
               DO i = ips, MIN(ide-1,ipe)
                  nested_grid% vegcat(i,j) = nested_grid%ivgtyp(i,j)
                  nested_grid%soilcat(i,j) = nested_grid%isltyp(i,j)
               END DO
            END DO

         ELSE IF ( ( nested_grid% vegcat(ips,jps) .GT. 0.5 ) .AND. &
                   ( nested_grid%soilcat(ips,jps) .GT. 0.5 ) ) THEN
            DO j = jps, MIN(jde-1,jpe)
               DO i = ips, MIN(ide-1,ipe)
                  nested_grid%ivgtyp(i,j) = NINT(nested_grid% vegcat(i,j))
                  nested_grid%isltyp(i,j) = NINT(nested_grid%soilcat(i,j))
               END DO
            END DO

         ELSE
            num_veg_cat      = SIZE ( nested_grid%landusef , DIM=2 )
            num_soil_top_cat = SIZE ( nested_grid%soilctop , DIM=2 )
            num_soil_bot_cat = SIZE ( nested_grid%soilcbot , DIM=2 )
   
            CALL land_percentages (  nested_grid%xland , &
                                     nested_grid%landusef , nested_grid%soilctop , nested_grid%soilcbot , &
                                     nested_grid%isltyp , nested_grid%ivgtyp , &
                                     num_veg_cat , num_soil_top_cat , num_soil_bot_cat , &
                                     ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     ips , ipe , jps , jpe , kps , kpe , &
                                     model_config_rec%iswater(nested_grid%id) )

          END IF

          DO j = jps, MIN(jde-1,jpe)
            DO i = ips, MIN(ide-1,ipe)
               nested_grid%lu_index(i,j) = nested_grid%ivgtyp(i,j)
            END DO
         END DO

         CALL check_consistency ( nested_grid%ivgtyp , nested_grid%isltyp , nested_grid%landmask , &
                                  ids , ide , jds , jde , kds , kde , &
                                  ims , ime , jms , jme , kms , kme , &
                                  ips , ipe , jps , jpe , kps , kpe , &
                                  model_config_rec%iswater(nested_grid%id) )

         CALL check_consistency2( nested_grid%ivgtyp , nested_grid%isltyp , nested_grid%landmask , &
                                  nested_grid%tmn , nested_grid%tsk , nested_grid%sst , nested_grid%xland , &
                                  nested_grid%tslb , nested_grid%smois , nested_grid%sh2o , &
                                  config_flags%num_soil_layers , nested_grid%id , &
                                  ids , ide , jds , jde , kds , kde , &
                                  ims , ime , jms , jme , kms , kme , &
                                  ips , ipe , jps , jpe , kps , kpe , &
                                  model_config_rec%iswater(nested_grid%id) )

      END IF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   
      !  We have 2 terrain elevations.  One is from input and the other is from the
      !  the horizontal interpolation.

      nested_grid%ht_fine = nested_grid%ht_input
      nested_grid%ht      = nested_grid%ht_int

      !  We have both the interpolated fields and the higher-resolution static fields.  From these
      !  the rebalancing is now done.  Note also that the field nested_grid%ht is now from the 
      !  fine grid input file (after this call is completed).

      CALL rebalance_driver ( nested_grid ) 

      !  Different things happen during the different time loops:
      !      first loop - write wrfinput file, close data set, copy files to holder arrays
      !      middle loops - diff 3d/2d arrays, compute and output bc
      !      last loop - diff 3d/2d arrays, compute and output bc, write wrfbdy file, close wrfbdy file

      IF ( time_loop .EQ. 1 ) THEN

         !  Set the time info.

         print *,'current_date = ',current_date
         CALL domain_clock_set( nested_grid, &
                                current_timestr=current_date(1:19) )

         !  Output the first time period of the data.
   
         CALL output_model_input ( fido , nested_grid , config_flags , ierr )

         CALL wrf_put_dom_ti_integer ( fido , 'MAP_PROJ' , map_proj , 1 , ierr ) 
!        CALL wrf_put_dom_ti_real    ( fido , DX  , dx  , 1 , ierr ) 
!        CALL wrf_put_dom_ti_real    ( fido , DY  , dy  , 1 , ierr ) 
         CALL wrf_put_dom_ti_real    ( fido , 'CEN_LAT' , cen_lat , 1 , ierr ) 
         CALL wrf_put_dom_ti_real    ( fido , 'CEN_LON' , cen_lon , 1 , ierr ) 
         CALL wrf_put_dom_ti_real    ( fido , 'TRUELAT1' , truelat1 , 1 , ierr ) 
         CALL wrf_put_dom_ti_real    ( fido , 'TRUELAT2' , truelat2 , 1 , ierr ) 
         CALL wrf_put_dom_ti_real    ( fido , 'MOAD_CEN_LAT' , moad_cen_lat , 1 , ierr ) 
         CALL wrf_put_dom_ti_real    ( fido , 'STAND_LON' , stand_lon , 1 , ierr ) 
         CALL wrf_put_dom_ti_integer ( fido , 'ISWATER' , iswater , 1 , ierr ) 

         !  These change if the initial time for the nest is not the same as the
         !  first time period in the WRF output file.
         !  Now that we know the starting date, we need to set the GMT, JULYR, and JULDAY
         !  values for the global attributes.  This call is based on the setting of the 
         !  current_date string.

         CALL geth_julgmt ( julyr , julday , gmt)
         CALL nl_set_julyr  ( nested_grid%id , julyr  )
         CALL nl_set_julday ( nested_grid%id , julday )
         CALL nl_set_gmt    ( nested_grid%id , gmt    )
         CALL wrf_put_dom_ti_real    ( fido , 'GMT' , gmt , 1 , ierr ) 
         CALL wrf_put_dom_ti_integer ( fido , 'JULYR' , julyr , 1 , ierr ) 
         CALL wrf_put_dom_ti_integer ( fido , 'JULDAY' , julday , 1 , ierr ) 
print *,'current_date =',current_date
print *,'julyr=',julyr
print *,'julday=',julday
print *,'gmt=',gmt
         
         !  Close the input (wrfout_d01_000000, for example) file.  Thats right, the 
         !  input is an output file.  Whodve thunk.
   
         CALL close_dataset      ( fido , config_flags , "DATASET=INPUT" )

         !  We need to save the 3d/2d data to compute a difference during the next loop.  Couple the
         !  3d fields with total mu (mub + mu_2) and the stagger-specific map scale factor.

         CALL couple ( nested_grid%em_mu_2 , nested_grid%em_mub , ubdy3dtemp1 , nested_grid%em_u_2                 , &
                       'u' , nested_grid%msfu , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
         CALL couple ( nested_grid%em_mu_2 , nested_grid%em_mub , vbdy3dtemp1 , nested_grid%em_v_2                 , &
                       'v' , nested_grid%msfv , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
         CALL couple ( nested_grid%em_mu_2 , nested_grid%em_mub , tbdy3dtemp1 , nested_grid%em_t_2                 , &
                       't' , nested_grid%msft , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
         CALL couple ( nested_grid%em_mu_2 , nested_grid%em_mub , pbdy3dtemp1 , nested_grid%em_ph_2                , &
                       'h' , nested_grid%msft , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
         CALL couple ( nested_grid%em_mu_2 , nested_grid%em_mub , qbdy3dtemp1 , nested_grid%moist(:,:,:,P_QV)    , &
                       't' , nested_grid%msft , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )

          DO j = jps , jpe
             DO i = ips , ipe
                mbdy2dtemp1(i,1,j) = nested_grid%em_mu_2(i,j)
             END DO
          END DO

         !  There are 2 components to the lateral boundaries.  First, there is the starting
         !  point of this time period - just the outer few rows and columns.

         CALL stuff_bdy     ( ubdy3dtemp1 , nested_grid%em_u_b     , 'U' , ijds , ijde , spec_bdy_width      , &
                                                                           ids , ide , jds , jde , kds , kde , &
                                                                           ims , ime , jms , jme , kms , kme , &
                                                                           ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdy     ( vbdy3dtemp1 , nested_grid%em_v_b     , 'V' , ijds , ijde , spec_bdy_width      , &
                                                                           ids , ide , jds , jde , kds , kde , &
                                                                           ims , ime , jms , jme , kms , kme , &
                                                                           ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdy     ( tbdy3dtemp1 , nested_grid%em_t_b     , 'T' , ijds , ijde , spec_bdy_width      , &
                                                                           ids , ide , jds , jde , kds , kde , &
                                                                           ims , ime , jms , jme , kms , kme , &
                                                                           ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdy     ( pbdy3dtemp1 , nested_grid%em_ph_b    , 'W' , ijds , ijde , spec_bdy_width      , &
                                                                           ids , ide , jds , jde , kds , kde , &
                                                                           ims , ime , jms , jme , kms , kme , &
                                                                           ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdy     ( qbdy3dtemp1 , nested_grid%moist_b(:,:,:,:,P_QV)                                  &
                                                                   , 'T' , ijds , ijde , spec_bdy_width      , &
                                                                           ids , ide , jds , jde , kds , kde , &
                                                                           ims , ime , jms , jme , kms , kme , &
                                                                           ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdy     ( mbdy2dtemp1 , nested_grid%em_mu_b    , 'M' , ijds , ijde , spec_bdy_width      , &
                                                                           ids , ide , jds , jde , 1 , 1 , &
                                                                           ims , ime , jms , jme , 1 , 1 , &
                                                                           ips , ipe , jps , jpe , 1 , 1 )
      ELSE IF ( ( time_loop .GT. 1 ) .AND. ( time_loop .LT. time_loop_max ) ) THEN

         CALL couple ( nested_grid%em_mu_2 , nested_grid%em_mub , ubdy3dtemp2 , nested_grid%em_u_2                 , &
                       'u' , nested_grid%msfu , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
         CALL couple ( nested_grid%em_mu_2 , nested_grid%em_mub , vbdy3dtemp2 , nested_grid%em_v_2                 , &
                       'v' , nested_grid%msfv , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
         CALL couple ( nested_grid%em_mu_2 , nested_grid%em_mub , tbdy3dtemp2 , nested_grid%em_t_2                 , &
                       't' , nested_grid%msft , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
         CALL couple ( nested_grid%em_mu_2 , nested_grid%em_mub , pbdy3dtemp2 , nested_grid%em_ph_2                , &
                       'h' , nested_grid%msft , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
         CALL couple ( nested_grid%em_mu_2 , nested_grid%em_mub , qbdy3dtemp2 , nested_grid%moist(:,:,:,P_QV)    , &
                       't' , nested_grid%msft , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )

          DO j = jps , jpe
             DO i = ips , ipe
                mbdy2dtemp2(i,1,j) = nested_grid%em_mu_2(i,j)
             END DO
          END DO

         !  During all of the loops after the first loop, we first compute the boundary
         !  tendencies with the current data values and the previously save information
         !  stored in the *bdy3dtemp1 arrays.

         CALL stuff_bdytend ( ubdy3dtemp2 , ubdy3dtemp1 , new_bdy_frq , nested_grid%em_u_bt  , 'U'  , &
                                                                  ijds , ijde , spec_bdy_width      , &
                                                                  ids , ide , jds , jde , kds , kde , &
                                                                  ims , ime , jms , jme , kms , kme , &
                                                                  ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdytend ( vbdy3dtemp2 , vbdy3dtemp1 , new_bdy_frq , nested_grid%em_v_bt  , 'V'  , &
                                                                  ijds , ijde , spec_bdy_width      , &
                                                                  ids , ide , jds , jde , kds , kde , &
                                                                  ims , ime , jms , jme , kms , kme , &
                                                                  ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdytend ( tbdy3dtemp2 , tbdy3dtemp1 , new_bdy_frq , nested_grid%em_t_bt  , 'T'  , &
                                                                  ijds , ijde , spec_bdy_width      , &
                                                                  ids , ide , jds , jde , kds , kde , &
                                                                  ims , ime , jms , jme , kms , kme , &
                                                                  ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdytend ( pbdy3dtemp2 , pbdy3dtemp1 , new_bdy_frq , nested_grid%em_ph_bt  , 'W' , &
                                                                  ijds , ijde , spec_bdy_width      , &
                                                                  ids , ide , jds , jde , kds , kde , &
                                                                  ims , ime , jms , jme , kms , kme , &
                                                                  ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdytend ( qbdy3dtemp2 , qbdy3dtemp1 , new_bdy_frq , nested_grid%moist_bt(:,:,:,:,P_QV), 'T' , &
                                                                  ijds , ijde , spec_bdy_width      , &
                                                                  ids , ide , jds , jde , kds , kde , &
                                                                  ims , ime , jms , jme , kms , kme , &
                                                                  ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdytend ( mbdy2dtemp2 , mbdy2dtemp1 , new_bdy_frq , nested_grid%em_mu_bt  , 'M' , &
                                                                  ijds , ijde , spec_bdy_width      , &
                                                                  ids , ide , jds , jde , 1 , 1 , &
                                                                  ims , ime , jms , jme , 1 , 1 , &
                                                                  ips , ipe , jps , jpe , 1 , 1 )
         IF ( time_loop .EQ. 2 ) THEN
   
            !  Generate an output file from this program, which will be an input file to WRF.

            CALL wrf_debug          ( 100 , 'ndown_em main: calling open_w_dataset for wrfbdy' )
            CALL construct_filename1( bdyname , 'wrfbdy' , nested_grid%id , 2 )
            CALL open_w_dataset     ( fidb, TRIM(bdyname) , nested_grid , config_flags , output_boundary , &
                                      "DATASET=BOUNDARY", ierr )
            IF ( ierr .NE. 0 ) THEN
               WRITE( wrf_err_message , FMT='(A,A,A,I8)' ) 'program ndown: error opening ',TRIM(bdyname),' for reading ierr=',ierr
               CALL wrf_error_fatal3 ( "ndown_em.b" , 792 ,  wrf_err_message )
            ENDIF

         END IF

         !  Both pieces of the boundary data are now available to be written.
         
      CALL domain_clock_set( nested_grid, &
                             current_timestr=current_date(1:19) )
      temp24= current_date
      temp24b=start_date_hold
      start_date = start_date_hold
      CALL geth_newdate ( temp19 , temp24b(1:19) , (time_loop-2) * model_config_rec%interval_seconds )
      current_date = temp19 //  '.0000'
      CALL geth_julgmt ( julyr , julday , gmt)
      CALL nl_set_julyr  ( nested_grid%id , julyr  )
      CALL nl_set_julday ( nested_grid%id , julday )
      CALL nl_set_gmt    ( nested_grid%id , gmt    )
      CALL wrf_put_dom_ti_real    ( fidb , 'GMT' , gmt , 1 , ierr ) 
      CALL wrf_put_dom_ti_integer ( fidb , 'JULYR' , julyr , 1 , ierr ) 
      CALL wrf_put_dom_ti_integer ( fidb , 'JULDAY' , julday , 1 , ierr ) 
      CALL domain_clock_set( nested_grid, &
                             current_timestr=current_date(1:19) )
print *,'bdy time = ',time_loop-1,'  bdy date = ',current_date,' ',start_date
      CALL output_boundary ( fidb , nested_grid , config_flags , ierr )
      current_date = temp24
      start_date = temp24b
      CALL domain_clock_set( nested_grid, &
                             current_timestr=current_date(1:19) )

         IF ( time_loop .EQ. 2 ) THEN
            CALL wrf_put_dom_ti_real    ( fidb , 'BDYFRQ' , new_bdy_frq , 1 , ierr ) 
         END IF

         !  We need to save the 3d data to compute a difference during the next loop.  Couple the
         !  3d fields with total mu (mub + mu_2) and the stagger-specific map scale factor.
         !  We load up the boundary data again for use in the next loop.

          DO j = jps , jpe
             DO k = kps , kpe
                DO i = ips , ipe
                   ubdy3dtemp1(i,k,j) = ubdy3dtemp2(i,k,j)
                   vbdy3dtemp1(i,k,j) = vbdy3dtemp2(i,k,j)
                   tbdy3dtemp1(i,k,j) = tbdy3dtemp2(i,k,j)
                   pbdy3dtemp1(i,k,j) = pbdy3dtemp2(i,k,j)
                   qbdy3dtemp1(i,k,j) = qbdy3dtemp2(i,k,j)
                END DO
             END DO
          END DO

          DO j = jps , jpe
             DO i = ips , ipe
                mbdy2dtemp1(i,1,j) = mbdy2dtemp2(i,1,j)
             END DO
          END DO

         !  There are 2 components to the lateral boundaries.  First, there is the starting
         !  point of this time period - just the outer few rows and columns.

         CALL stuff_bdy     ( ubdy3dtemp1 , nested_grid%em_u_b     , 'U' , ijds , ijde , spec_bdy_width      , &
                                                                           ids , ide , jds , jde , kds , kde , &
                                                                           ims , ime , jms , jme , kms , kme , &
                                                                           ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdy     ( vbdy3dtemp1 , nested_grid%em_v_b     , 'V' , ijds , ijde , spec_bdy_width      , &
                                                                           ids , ide , jds , jde , kds , kde , &
                                                                           ims , ime , jms , jme , kms , kme , &
                                                                           ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdy     ( tbdy3dtemp1 , nested_grid%em_t_b     , 'T' , ijds , ijde , spec_bdy_width      , &
                                                                           ids , ide , jds , jde , kds , kde , &
                                                                           ims , ime , jms , jme , kms , kme , &
                                                                           ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdy     ( pbdy3dtemp1 , nested_grid%em_ph_b    , 'W' , ijds , ijde , spec_bdy_width      , &
                                                                           ids , ide , jds , jde , kds , kde , &
                                                                           ims , ime , jms , jme , kms , kme , &
                                                                           ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdy     ( qbdy3dtemp1 , nested_grid%moist_b(:,:,:,:,P_QV)   , 'T' , ijds , ijde , spec_bdy_width      , &
                                                                           ids , ide , jds , jde , kds , kde , &
                                                                           ims , ime , jms , jme , kms , kme , &
                                                                           ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdy     ( mbdy2dtemp1 , nested_grid%em_mu_b    , 'M' , ijds , ijde , spec_bdy_width      , &
                                                                           ids , ide , jds , jde , 1 , 1 , &
                                                                           ims , ime , jms , jme , 1 , 1 , &
                                                                           ips , ipe , jps , jpe , 1 , 1 )

      ELSE IF ( time_loop .EQ. time_loop_max ) THEN

         CALL couple ( nested_grid%em_mu_2 , nested_grid%em_mub , ubdy3dtemp2 , nested_grid%em_u_2                 , &
                       'u' , nested_grid%msfu , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
         CALL couple ( nested_grid%em_mu_2 , nested_grid%em_mub , vbdy3dtemp2 , nested_grid%em_v_2                 , &
                       'v' , nested_grid%msfv , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
         CALL couple ( nested_grid%em_mu_2 , nested_grid%em_mub , tbdy3dtemp2 , nested_grid%em_t_2                 , &
                       't' , nested_grid%msft , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
         CALL couple ( nested_grid%em_mu_2 , nested_grid%em_mub , pbdy3dtemp2 , nested_grid%em_ph_2                , &
                       'h' , nested_grid%msft , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
         CALL couple ( nested_grid%em_mu_2 , nested_grid%em_mub , qbdy3dtemp2 , nested_grid%moist(:,:,:,P_QV)    , &
                       't' , nested_grid%msft , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
         mbdy2dtemp2(:,1,:) = nested_grid%em_mu_2(:,:)

         !  During all of the loops after the first loop, we first compute the boundary
         !  tendencies with the current data values and the previously save information
         !  stored in the *bdy3dtemp1 arrays.

         CALL stuff_bdytend ( ubdy3dtemp2 , ubdy3dtemp1 , new_bdy_frq , nested_grid%em_u_bt  , 'U'  , &
                                                                  ijds , ijde , spec_bdy_width      , &
                                                                  ids , ide , jds , jde , kds , kde , &
                                                                  ims , ime , jms , jme , kms , kme , &
                                                                  ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdytend ( vbdy3dtemp2 , vbdy3dtemp1 , new_bdy_frq , nested_grid%em_v_bt  , 'V'  , &
                                                                  ijds , ijde , spec_bdy_width      , &
                                                                  ids , ide , jds , jde , kds , kde , &
                                                                  ims , ime , jms , jme , kms , kme , &
                                                                  ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdytend ( tbdy3dtemp2 , tbdy3dtemp1 , new_bdy_frq , nested_grid%em_t_bt  , 'T'  , &
                                                                  ijds , ijde , spec_bdy_width      , &
                                                                  ids , ide , jds , jde , kds , kde , &
                                                                  ims , ime , jms , jme , kms , kme , &
                                                                  ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdytend ( pbdy3dtemp2 , pbdy3dtemp1 , new_bdy_frq , nested_grid%em_ph_bt  , 'W' , &
                                                                  ijds , ijde , spec_bdy_width      , &
                                                                  ids , ide , jds , jde , kds , kde , &
                                                                  ims , ime , jms , jme , kms , kme , &
                                                                  ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdytend ( qbdy3dtemp2 , qbdy3dtemp1 , new_bdy_frq , nested_grid%moist_bt(:,:,:,:,P_QV) , 'T' , &
                                                                  ijds , ijde , spec_bdy_width      , &
                                                                  ids , ide , jds , jde , kds , kde , &
                                                                  ims , ime , jms , jme , kms , kme , &
                                                                  ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdytend ( mbdy2dtemp2 , mbdy2dtemp1 , new_bdy_frq , nested_grid%em_mu_bt  , 'M' , &
                                                                  ijds , ijde , spec_bdy_width      , &
                                                                  ids , ide , jds , jde , 1 , 1 , &
                                                                  ims , ime , jms , jme , 1 , 1 , &
                                                                  ips , ipe , jps , jpe , 1 , 1 )

         IF ( time_loop .EQ. 2 ) THEN
   
            !  Generate an output file from this program, which will be an input file to WRF.

            CALL wrf_debug          ( 100 , 'ndown_em main: calling open_w_dataset for wrfbdy' )
            CALL construct_filename1( bdyname , 'wrfbdy' , nested_grid%id , 2 )
            CALL open_w_dataset     ( fidb, TRIM(bdyname) , nested_grid , config_flags , output_boundary , &
                                      "DATASET=BOUNDARY", ierr )
            IF ( ierr .NE. 0 ) THEN
               WRITE( wrf_err_message , FMT='(A,A,A,I8)' ) 'program ndown: error opening ',TRIM(bdyname),' for reading ierr=',ierr
               CALL wrf_error_fatal3 ( "ndown_em.b" , 975 ,  wrf_err_message )
            ENDIF

         END IF

         !  Both pieces of the boundary data are now available to be written.

      CALL domain_clock_set( nested_grid, &
                             current_timestr=current_date(1:19) )
      temp24= current_date
      temp24b=start_date_hold
      start_date = start_date_hold
      CALL geth_newdate ( temp19 , temp24b(1:19) , (time_loop-2) * model_config_rec%interval_seconds )
      current_date = temp19 //  '.0000'
      CALL geth_julgmt ( julyr , julday , gmt)
      CALL nl_set_julyr  ( nested_grid%id , julyr  )
      CALL nl_set_julday ( nested_grid%id , julday )
      CALL nl_set_gmt    ( nested_grid%id , gmt    )
      CALL wrf_put_dom_ti_real    ( fidb , 'GMT' , gmt , 1 , ierr ) 
      CALL wrf_put_dom_ti_integer ( fidb , 'JULYR' , julyr , 1 , ierr ) 
      CALL wrf_put_dom_ti_integer ( fidb , 'JULDAY' , julday , 1 , ierr ) 
      CALL domain_clock_set( nested_grid, &
                             current_timestr=current_date(1:19) )
      CALL output_boundary ( fidb , nested_grid , config_flags , ierr )
      current_date = temp24
      start_date = temp24b
      CALL domain_clock_set( nested_grid, &
                             current_timestr=current_date(1:19) )

         IF ( time_loop .EQ. 2 ) THEN
            CALL wrf_put_dom_ti_real    ( fidb , 'BDYFRQ' , new_bdy_frq , 1 , ierr ) 
         END IF

         !  Since this is the last time through here, we need to close the boundary file.

         CALL model_to_grid_config_rec ( nested_grid%id , model_config_rec , config_flags )
         CALL close_dataset ( fidb , config_flags , "DATASET=BOUNDARY" )


      END IF

      !  Process which time now?

   END DO big_time_loop_thingy

   CALL model_to_grid_config_rec ( parent_grid%id , model_config_rec , config_flags )
   CALL med_shutdown_io ( parent_grid , config_flags )

   CALL wrf_debug ( 0 , 'ndown_em: SUCCESS COMPLETE NDOWN_EM INIT' )

   CALL wrf_shutdown

   CALL WRFU_Finalize( rc=rc )

END PROGRAM ndown_em

SUBROUTINE land_percentages ( xland , &
                              landuse_frac , soil_top_cat , soil_bot_cat , &
                              isltyp , ivgtyp , &
                              num_veg_cat , num_soil_top_cat , num_soil_bot_cat , &
                              ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte , &
                              iswater )
   USE module_soil_pre

   IMPLICIT NONE

   INTEGER , INTENT(IN) :: ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           its , ite , jts , jte , kts , kte , &
                           iswater

   INTEGER , INTENT(IN) :: num_veg_cat , num_soil_top_cat , num_soil_bot_cat
   REAL , DIMENSION(ims:ime,1:num_veg_cat,jms:jme) , INTENT(INOUT):: landuse_frac
   REAL , DIMENSION(ims:ime,1:num_soil_top_cat,jms:jme) , INTENT(IN):: soil_top_cat
   REAL , DIMENSION(ims:ime,1:num_soil_bot_cat,jms:jme) , INTENT(IN):: soil_bot_cat
   INTEGER , DIMENSION(ims:ime,jms:jme), INTENT(OUT) :: isltyp , ivgtyp
   REAL , DIMENSION(ims:ime,jms:jme) , INTENT(OUT) :: xland

   CALL process_percent_cat_new ( xland , &
                                  landuse_frac , soil_top_cat , soil_bot_cat , &
                                  isltyp , ivgtyp , &
                                  num_veg_cat , num_soil_top_cat , num_soil_bot_cat , &
                                  ids , ide , jds , jde , kds , kde , &
                                  ims , ime , jms , jme , kms , kme , &
                                  its , ite , jts , jte , kts , kte , &
                                  iswater )

END SUBROUTINE land_percentages

SUBROUTINE check_consistency ( ivgtyp , isltyp , landmask , &
                                  ids , ide , jds , jde , kds , kde , &
                                  ims , ime , jms , jme , kms , kme , &
                                  its , ite , jts , jte , kts , kte , &
                                  iswater )

   IMPLICIT NONE

   INTEGER , INTENT(IN) :: ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           its , ite , jts , jte , kts , kte , &
                           iswater
   INTEGER , DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: isltyp , ivgtyp
   REAL    , DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: landmask

   LOGICAL :: oops
   INTEGER :: oops_count , i , j

   oops = .FALSE.
   oops_count = 0

   DO j = jts, MIN(jde-1,jte)
      DO i = its, MIN(ide-1,ite)
         IF ( ( ( landmask(i,j) .LT. 0.5 ) .AND. ( ivgtyp(i,j) .NE. iswater ) ) .OR. &
              ( ( landmask(i,j) .GT. 0.5 ) .AND. ( ivgtyp(i,j) .EQ. iswater ) ) ) THEN
            print *,'mismatch in landmask and veg type'
            print *,'i,j=',i,j, '  landmask =',NINT(landmask(i,j)),'  ivgtyp=',ivgtyp(i,j)
            oops = .TRUE.
            oops_count = oops_count + 1
landmask(i,j) = 0
ivgtyp(i,j)=16
isltyp(i,j)=14
         END IF
      END DO
   END DO

   IF ( oops ) THEN
      CALL wrf_debug( 0, 'mismatch in check_consistency, turned to water points, be careful' )
   END IF

END SUBROUTINE check_consistency

SUBROUTINE check_consistency2( ivgtyp , isltyp , landmask , &
                               tmn , tsk , sst , xland , &
                               tslb , smois , sh2o , &
                               num_soil_layers , id , &
                               ids , ide , jds , jde , kds , kde , &
                               ims , ime , jms , jme , kms , kme , &
                               its , ite , jts , jte , kts , kte , &
                               iswater )

   USE module_configure
   USE module_optional_si_input

   INTEGER , INTENT(IN) :: ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           its , ite , jts , jte , kts , kte 
   INTEGER , INTENT(IN) :: num_soil_layers , id

   INTEGER , DIMENSION(ims:ime,jms:jme) :: ivgtyp , isltyp
   REAL    , DIMENSION(ims:ime,jms:jme) :: landmask , tmn , tsk , sst , xland
   REAL    , DIMENSION(ims:ime,num_soil_layers,jms:jme) :: tslb , smois , sh2o

   INTEGER :: oops1 , oops2
   INTEGER :: i , j , k

      fix_tsk_tmn : SELECT CASE ( model_config_rec%sf_surface_physics(id) )

         CASE ( SLABSCHEME , LSMSCHEME , RUCLSMSCHEME )
            DO j = jts, MIN(jde-1,jte)
               DO i = its, MIN(ide-1,ite)
                  IF ( ( landmask(i,j) .LT. 0.5 ) .AND. ( flag_sst .EQ. 1 ) ) THEN
                     tmn(i,j) = sst(i,j)
                     tsk(i,j) = sst(i,j)
                  ELSE IF ( landmask(i,j) .LT. 0.5 ) THEN
                     tmn(i,j) = tsk(i,j)
                  END IF
               END DO
            END DO
      END SELECT fix_tsk_tmn

      !  Is the TSK reasonable?

      DO j = jts, MIN(jde-1,jte)
         DO i = its, MIN(ide-1,ite)
            IF ( tsk(i,j) .LT. 170 .or. tsk(i,j) .GT. 400. ) THEN
               print *,'error in the TSK'
               print *,'i,j=',i,j
               print *,'landmask=',landmask(i,j)
               print *,'tsk, sst, tmn=',tsk(i,j),sst(i,j),tmn(i,j)
               if(tmn(i,j).gt.170. .and. tmn(i,j).lt.400.)then
                  tsk(i,j)=tmn(i,j)
               else if(sst(i,j).gt.170. .and. sst(i,j).lt.400.)then
                  tsk(i,j)=sst(i,j)
               else
                  CALL wrf_error_fatal3 ( "ndown_em.b" , 1161 ,  'TSK unreasonable' )
               end if
            END IF
         END DO
      END DO

      !  Is the TMN reasonable?

      DO j = jts, MIN(jde-1,jte)
         DO i = its, MIN(ide-1,ite)
            IF ( ( ( tmn(i,j) .LT. 170. ) .OR. ( tmn(i,j) .GT. 400. ) ) .AND. ( landmask(i,j) .GT. 0.5 ) ) THEN
                  print *,'error in the TMN'
                  print *,'i,j=',i,j
                  print *,'landmask=',landmask(i,j)
                  print *,'tsk, sst, tmn=',tsk(i,j),sst(i,j),tmn(i,j)
               if(tsk(i,j).gt.170. .and. tsk(i,j).lt.400.)then
                  tmn(i,j)=tsk(i,j)
               else if(sst(i,j).gt.170. .and. sst(i,j).lt.400.)then
                  tmn(i,j)=sst(i,j)
               else
                  CALL wrf_error_fatal3 ( "ndown_em.b" , 1181 ,  'TMN unreasonable' )
               endif
            END IF
         END DO
      END DO

      !  Is the TSLB reasonable?

      DO j = jts, MIN(jde-1,jte)
         DO i = its, MIN(ide-1,ite)
            IF ( ( ( tslb(i,1,j) .LT. 170. ) .OR. ( tslb(i,1,j) .GT. 400. ) ) .AND. ( landmask(i,j) .GT. 0.5 ) ) THEN
                  print *,'error in the TSLB'
                  print *,'i,j=',i,j
                  print *,'landmask=',landmask(i,j)
                  print *,'tsk, sst, tmn=',tsk(i,j),sst(i,j),tmn(i,j)
                  print *,'tslb = ',tslb(i,:,j)
                  print *,'old smois = ',smois(i,:,j)
                  DO l = 1 , num_soil_layers
                     sh2o(i,l,j) = 0.0
                  END DO
                  DO l = 1 , num_soil_layers
                     smois(i,l,j) = 0.3
                  END DO
                  if(tsk(i,j).gt.170. .and. tsk(i,j).lt.400.)then
                     DO l = 1 , num_soil_layers
                        tslb(i,l,j)=tsk(i,j)
                     END DO
                  else if(sst(i,j).gt.170. .and. sst(i,j).lt.400.)then
                     DO l = 1 , num_soil_layers
                        tslb(i,l,j)=sst(i,j)
                     END DO
                  else if(tmn(i,j).gt.170. .and. tmn(i,j).lt.400.)then
                     DO l = 1 , num_soil_layers
                        tslb(i,l,j)=tmn(i,j)
                     END DO
                  else
                     CALL wrf_error_fatal3 ( "ndown_em.b" , 1217 ,  'TSLB unreasonable' )
                  endif
            END IF
         END DO
      END DO

      !  Let us make sure (again) that the landmask and the veg/soil categories match.

oops1=0
oops2=0
      DO j = jts, MIN(jde-1,jte)
         DO i = its, MIN(ide-1,ite)
            IF ( ( ( landmask(i,j) .LT. 0.5 ) .AND. ( ivgtyp(i,j) .NE. iswater .OR. isltyp(i,j) .NE. 14 ) ) .OR. &
                 ( ( landmask(i,j) .GT. 0.5 ) .AND. ( ivgtyp(i,j) .EQ. iswater .OR. isltyp(i,j) .EQ. 14 ) ) ) THEN
               IF ( tslb(i,1,j) .GT. 1. ) THEN
oops1=oops1+1
                  ivgtyp(i,j) = 5
                  isltyp(i,j) = 8
                  landmask(i,j) = 1
                  xland(i,j) = 1
               ELSE IF ( sst(i,j) .GT. 1. ) THEN
oops2=oops2+1
                  ivgtyp(i,j) = iswater
                  isltyp(i,j) = 14
                  landmask(i,j) = 0
                  xland(i,j) = 2
               ELSE
                  print *,'the landmask and soil/veg cats do not match'
                  print *,'i,j=',i,j
                  print *,'landmask=',landmask(i,j)
                  print *,'ivgtyp=',ivgtyp(i,j)
                  print *,'isltyp=',isltyp(i,j)
                  print *,'iswater=', iswater
                  print *,'tslb=',tslb(i,:,j)
                  print *,'sst=',sst(i,j)
                  CALL wrf_error_fatal3 ( "ndown_em.b" , 1252 ,  'mismatch_landmask_ivgtyp' )
               END IF
            END IF
         END DO
      END DO
if (oops1.gt.0) then
print *,'points artificially set to land : ',oops1
endif
if(oops2.gt.0) then
print *,'points artificially set to water: ',oops2
endif

END SUBROUTINE check_consistency2

SUBROUTINE init_domain_constants_em_ptr ( parent , nest ) 
   USE module_domain
   USE module_configure
   IMPLICIT NONE
   TYPE(domain), POINTER  :: parent , nest
   INTERFACE 
   SUBROUTINE init_domain_constants_em ( parent , nest )
      USE module_domain
      USE module_configure
      TYPE(domain)  :: parent , nest
   END SUBROUTINE init_domain_constants_em
   END INTERFACE 
   CALL init_domain_constants_em ( parent , nest )
END SUBROUTINE init_domain_constants_em_ptr
