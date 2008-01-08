!  Create an initial data set for the WRF model based on real data.  This
!  program is specifically set up for the NMM core.

PROGRAM real_data

   USE module_machine
   USE module_domain
   USE module_initialize
   USE module_io_domain
   USE module_driver_constants
   USE module_configure
   USE module_timing
   USE module_utility
   USE module_dm

   IMPLICIT NONE

   REAL    :: time , bdyfrq

   INTEGER :: loop , levels_to_process , debug_level


   TYPE(domain) , POINTER :: null_domain
   TYPE(domain) , POINTER :: grid
   TYPE (grid_config_rec_type)              :: config_flags
   INTEGER                :: number_at_same_level

   INTEGER :: max_dom, domain_id
   INTEGER :: idum1, idum2 
   INTEGER                 :: nbytes
!   INTEGER, PARAMETER      :: configbuflen = 2*1024
   INTEGER, PARAMETER      :: configbuflen = 4*16384
   INTEGER                 :: configbuf( configbuflen )
   LOGICAL , EXTERNAL      :: wrf_dm_on_monitor

   INTEGER :: ids , ide , jds , jde , kds , kde
   INTEGER :: ims , ime , jms , jme , kms , kme
   INTEGER :: ips , ipe , jps , jpe , kps , kpe
   INTEGER :: ijds , ijde , spec_bdy_width
   INTEGER :: i , j , k , idts


   CHARACTER (LEN=80)     :: message

   INTEGER :: start_year , start_month , start_day 
   INTEGER :: start_hour , start_minute , start_second
   INTEGER :: end_year ,   end_month ,   end_day ,   &
              end_hour ,   end_minute ,   end_second
   INTEGER :: interval_seconds , real_data_init_type
   INTEGER :: time_loop_max , time_loop, rc
   REAL    :: t1,t2

   CHARACTER (LEN=10) :: release_version = 'V2.2      '

   INTERFACE
     SUBROUTINE Setup_Timekeeping( grid )
      USE module_domain
      TYPE(domain), POINTER :: grid
     END SUBROUTINE Setup_Timekeeping
   END INTERFACE

   !  Define the name of this program (program_name defined in module_domain)

   program_name = "REAL_NMM " // TRIM(release_version) // " PREPROCESSOR"

   CALL disable_quilting

!       CALL start()

   !  Initialize the modules used by the WRF system.  
   !  Many of the CALLs made from the
   !  init_modules routine are NO-OPs.  Typical initializations 
   !  are: the size of a
   !  REAL, setting the file handles to a pre-use value, defining moisture and
   !  chemistry indices, etc.

   CALL       wrf_debug ( 100 , 'real_nmm: calling init_modules ' )

!!!!   CALL init_modules
   CALL init_modules(1)   ! Phase 1 returns after MPI_INIT() (if it is called)
   CALL WRFU_Initialize( defaultCalendar=WRFU_CAL_GREGORIAN, rc=rc )
   CALL init_modules(2)   ! Phase 2 resumes after MPI_INIT() (if it is called)

   !  The configuration switches mostly come from the NAMELIST input.

   IF ( wrf_dm_on_monitor() ) THEN
      write(message,*) 'call initial_config'
      CALL wrf_message ( message )
      CALL initial_config
   ENDIF
   CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
   CALL wrf_dm_bcast_bytes( configbuf, nbytes )
   CALL set_config_as_buffer( configbuf, configbuflen )
   CALL wrf_dm_initialize


   CALL nl_get_debug_level ( 1, debug_level )
   CALL set_wrf_debug_level ( debug_level )

   CALL  wrf_message ( program_name )

   !  Allocate the space for the mother of all domains.

   NULLIFY( null_domain )
   CALL  wrf_debug ( 100 , 'real_nmm: calling alloc_and_configure_domain ' )
   CALL alloc_and_configure_domain ( domain_id  = 1           , &
                                     grid       = head_grid   , &
                                     parent     = null_domain , &
                                     kid        = -1            )

   grid => head_grid


   CALL Setup_Timekeeping ( grid )
   CALL domain_clock_set( grid, &
                          time_step_seconds=model_config_rec%interval_seconds )
   CALL wrf_debug ( 100 , 'real_nmm: calling set_scalar_indices_from_config ' )
   CALL set_scalar_indices_from_config ( grid%id , idum1, idum2 )

   CALL     wrf_debug ( 100 , 'real_nmm: calling model_to_grid_config_rec ' )

   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

   write(message,*) 'after model_to_grid_config_rec, e_we, e_sn are: ', &
                    config_flags%e_we, config_flags%e_sn
   CALL wrf_message(message)

   !  Initialize the WRF IO: open files, init file handles, etc.

   CALL       wrf_debug ( 100 , 'real_nmm: calling init_wrfio' )
   CALL init_wrfio

!  Some of the configuration values may have been modified from the initial READ
!  of the NAMELIST, so we re-broadcast the configuration records.

   CALL wrf_debug ( 100 , 'real_nmm: re-broadcast the configuration records' )
   CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
   CALL wrf_dm_bcast_bytes( configbuf, nbytes )
   CALL set_config_as_buffer( configbuf, configbuflen )

   !   No looping in this layer.  

   CALL med_sidata_input ( grid , config_flags )

   !  We are done.

   CALL       wrf_debug (   0 , 'real_nmm: SUCCESS COMPLETE REAL_NMM INIT' )

    CALL wrf_dm_shutdown

   CALL WRFU_Finalize( rc=rc )

END PROGRAM real_data

SUBROUTINE med_sidata_input ( grid , config_flags )
  ! Driver layer
   USE module_domain
   USE module_io_domain
  ! Model layer
   USE module_configure
   USE module_bc_time_utilities
   USE module_initialize
   USE module_optional_si_input

   USE module_si_io_nmm

   USE module_date_time

   IMPLICIT NONE


  ! Interface 
   INTERFACE
     SUBROUTINE start_domain ( grid , allowed_to_read )
       USE module_domain
       TYPE (domain) grid
       LOGICAL, INTENT(IN) :: allowed_to_read
     END SUBROUTINE start_domain
   END INTERFACE

  ! Arguments
   TYPE(domain)                :: grid
   TYPE (grid_config_rec_type) :: config_flags
  ! Local
   INTEGER                :: time_step_begin_restart
   INTEGER                :: idsi , ierr , myproc
   CHARACTER (LEN=80)      :: si_inpname
   CHARACTER (LEN=132)     :: message

   CHARACTER(LEN=19) :: start_date_char , end_date_char , &
                        current_date_char , next_date_char

   INTEGER :: time_loop_max , loop
   INTEGER :: julyr , julday , LEN

   INTEGER :: io_form_auxinput1
   INTEGER, EXTERNAL :: use_package

   LOGICAL :: using_binary_wrfsi

   REAL :: gmt
   REAL :: t1,t2

   INTEGER :: numx_sm_levels_input,numx_st_levels_input
   REAL,DIMENSION(100) :: smx_levels_input,stx_levels_input






   grid%input_from_file = .true.
   grid%input_from_file = .false.

   CALL compute_si_start_and_end ( model_config_rec%start_year  (grid%id) , &
                                   model_config_rec%start_month (grid%id) , &
                                   model_config_rec%start_day   (grid%id) , &
                                   model_config_rec%start_hour  (grid%id) , &
                                   model_config_rec%start_minute(grid%id) , &
                                   model_config_rec%start_second(grid%id) , &
                                   model_config_rec%  end_year  (grid%id) , & 
                                   model_config_rec%  end_month (grid%id) , &
                                   model_config_rec%  end_day   (grid%id) , &
                                   model_config_rec%  end_hour  (grid%id) , &
                                   model_config_rec%  end_minute(grid%id) , &
                                   model_config_rec%  end_second(grid%id) , &
                                   model_config_rec%interval_seconds      , &
                                   model_config_rec%real_data_init_type   , &
                                   start_date_char , end_date_char , time_loop_max )

   !  Here we define the initial time to process, for later use by the code.

   current_date_char = start_date_char
!   start_date = start_date_char // .0000
   start_date = start_date_char 
   current_date = start_date

   CALL nl_set_bdyfrq ( grid%id , REAL(model_config_rec%interval_seconds) )

   !  Loop over each time period to process.

   write(message,*) 'time_loop_max: ', time_loop_max
   CALL wrf_message(message)
   DO loop = 1 , time_loop_max

     internal_time_loop=loop
                                                                                                                                              

!	call summary()

      write(message,*) 'loop=', loop
      CALL wrf_message(message)
                                                                                                                                              
      write(message,*) '-----------------------------------------------------------'
      CALL wrf_message(message)
                      
      write(message,*) ' '
      CALL wrf_message(message)
      write(message,'(A,A,A,I2,A,I2)') ' Current date being processed: ', &
        current_date, ', which is loop #',loop,' out of ',time_loop_max
      CALL wrf_message(message)

      !  After current_date has been set, fill in the julgmt stuff.

      CALL geth_julgmt ( config_flags%julyr , config_flags%julday , &
                                              config_flags%gmt )

      !  Now that the specific Julian info is available, 
      !  save these in the model config record.

      CALL nl_set_gmt (grid%id, config_flags%gmt)
      CALL nl_set_julyr (grid%id, config_flags%julyr)
      CALL nl_set_julday (grid%id, config_flags%julday)

      CALL nl_get_io_form_auxinput1( 1, io_form_auxinput1 )
      using_binary_wrfsi=.false.
       
       
      IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
       
        write(message,*) 'TRIM(config_flags%auxinput1_inname): ', TRIM(config_flags%auxinput1_inname)
        CALL wrf_message(message)
       
        IF (config_flags%auxinput1_inname(1:10) .eq. 'real_input') THEN
           using_binary_wrfsi=.true.
        ENDIF

      SELECT CASE ( use_package(io_form_auxinput1) )
      CASE ( IO_NETCDF   )

      !  Open the wrfinput file.

        current_date_char(11:11)='_'
 
       WRITE ( wrf_err_message , FMT='(A,A)' )'med_sidata_input: calling open_r_dataset for ',TRIM(config_flags%auxinput1_inname)
       CALL wrf_debug ( 100 , wrf_err_message )
       IF ( config_flags%auxinput1_inname(1:8) .NE. 'wrf_real' ) THEN
          CALL construct_filename4a( si_inpname , config_flags%auxinput1_inname , grid%id , 2 , current_date_char , &
                                     config_flags%io_form_auxinput1 )
       ELSE
          CALL construct_filename2a( si_inpname , config_flags%auxinput1_inname , grid%id , 2 , current_date_char )
       END IF
       CALL open_r_dataset ( idsi, TRIM(si_inpname) , grid , config_flags , "DATASET=AUXINPUT1", ierr )
 
       IF ( ierr .NE. 0 ) THEN
          CALL wrf_error_fatal3 ( "real_nmm.b" , 341 ,  'error opening ' // TRIM(si_inpname) // ' for input; bad date in namelist or file not in directory' )
       ENDIF

      !  Input data.

      CALL wrf_debug (100, 'med_sidata_input: call input_aux_model_input1_wrf')

      CALL input_aux_model_input1 ( idsi, grid, config_flags, ierr )

      !  Possible optional SI input.  This sets flags used by init_domain.

      IF ( loop .EQ. 1 ) THEN
         CALL  wrf_debug (100, 'med_sidata_input: call init_module_optional_si_input' )
         CALL init_module_optional_si_input ( grid , config_flags )
      CALL wrf_debug ( 100 , 'med_sidata_input: calling optional_si_input' )
!
      CALL optional_si_input ( grid , idsi )
	write(0,*) 'maxval st_input(1) within real_nmm: ', maxval(st_input(:,1,:))
      END IF
!
      CALL close_dataset ( idsi , config_flags , "DATASET=AUXINPUT1" )

      CASE ( IO_INTIO )

      !  Possible optional SI input.  This sets flags used by init_domain.

      IF ( loop .EQ. 1 ) THEN
         CALL  wrf_debug (100, 'med_sidata_input: call init_module_optional_si_input' )
         CALL init_module_optional_si_input ( grid , config_flags )
      END IF

      IF (using_binary_wrfsi) THEN

        current_date_char(11:11)='_'

        CALL read_si ( grid, current_date_char )
        current_date_char(11:11)='T'

      ELSE
                                                                                                                                              
        write(message,*) 'binary WPS branch'
        CALL wrf_message(message)
        current_date_char(11:11)='_'
        CALL construct_filename4a( si_inpname , config_flags%auxinput1_inname , grid%id , 2 , current_date_char , &
                                     config_flags%io_form_auxinput1 )

!	call summary()
        CALL read_wps ( grid, trim(si_inpname), current_date_char, config_flags%num_metgrid_levels )
!	call summary()

!        CALL wrf_error_fatal3 ( "real_nmm.b" , 393 , "binary WPS support deferred for initial release")
                                                                                                                                              
!!! bogus set some flags??

      flag_metgrid=1
      flag_soilhgt=1
 
          ENDIF

      CASE DEFAULT
        CALL wrf_error_fatal3 ( "real_nmm.b" , 404 , 'real: not valid io_form_auxinput1')
      END SELECT

  ELSE
        CALL wrf_error_fatal3 ( "real_nmm.b" , 408 , "WRONG DYNAMICAL CORE SELECTED FOR THIS VERSION OF REAL - CHECK dyn_opt in namelist.input file")
  ENDIF

      grid%nmm_islope=1
      grid%vegfra=grid%nmm_vegfrc
      grid%nmm_dfrlg=grid%nmm_dfl/9.81

      grid%isurban=1
      grid%isoilwater=14

      !  Initialize the mother domain for this time period with input data.

      CALL wrf_debug ( 100 , 'med_sidata_input: calling init_domain' )
      grid%input_from_file = .true.

      CALL init_domain ( grid )

      CALL model_to_grid_config_rec ( grid%id, model_config_rec, config_flags )

      !  Close this file that is output from the SI and input to this pre-proc.

      CALL wrf_debug ( 100 , 'med_sidata_input: back from init_domain' )


!!! not sure about this, but doesnt seem like needs to be called each time
      IF ( loop .EQ. 1 ) THEN
	write(0,*) 'maxval grid%nmm_cwm into start_domain: ', maxval(grid%nmm_cwm)
        CALL start_domain ( grid , .TRUE.)
	write(0,*) 'maxval grid%nmm_cwm after start_domain: ', maxval(grid%nmm_cwm)
      END IF


      config_flags%isurban=1
      config_flags%isoilwater=14

      CALL assemble_output ( grid , config_flags , loop , time_loop_max )

      !  Here we define the next time that we are going to process.

      CALL geth_newdate ( current_date_char , start_date_char , &
                          loop * model_config_rec%interval_seconds )
      current_date =  current_date_char // '.0000'

      CALL domain_clock_set( grid, current_date(1:19) )

      write(message,*) 'current_date= ', current_date
      CALL wrf_message(message)

   END DO
END SUBROUTINE med_sidata_input

SUBROUTINE compute_si_start_and_end (  &
          start_year, start_month, start_day, start_hour, &
          start_minute, start_second, &
          end_year ,   end_month ,   end_day ,   end_hour , &
          end_minute ,   end_second , &
          interval_seconds , real_data_init_type , &
          start_date_char , end_date_char , time_loop_max )

   USE module_date_time

   IMPLICIT NONE

   INTEGER :: start_year , start_month , start_day , &
              start_hour , start_minute , start_second
   INTEGER ::   end_year ,   end_month ,   end_day , &
                end_hour ,   end_minute ,   end_second
   INTEGER :: interval_seconds , real_data_init_type
   INTEGER :: time_loop_max , time_loop

   CHARACTER(LEN=132) :: message
   CHARACTER(LEN=19)  :: current_date_char , start_date_char , &
                        end_date_char , next_date_char

!   WRITE ( start_date_char , FMT = &
!         (I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2) ) &
!         start_year,start_month,start_day,start_hour,start_minute,start_second
!   WRITE (   end_date_char , FMT = &
!         (I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2) ) &
!          end_year,  end_month,  end_day,  end_hour,  end_minute,  end_second

   WRITE ( start_date_char , FMT = &
         '(I4.4,"-",I2.2,"-",I2.2,"T",I2.2,":",I2.2,":",I2.2)' ) &
         start_year,start_month,start_day,start_hour,start_minute,start_second
   WRITE (   end_date_char , FMT = &
         '(I4.4,"-",I2.2,"-",I2.2,"T",I2.2,":",I2.2,":",I2.2)' ) &
          end_year,  end_month,  end_day,  end_hour,  end_minute,  end_second

!  start_date = start_date_char // .0000

   !  Figure out our loop count for the processing times.

   time_loop = 1
   PRINT '(A,I4,A,A,A)','Time period #',time_loop, &
                        ' to process = ',start_date_char,'.'
   current_date_char = start_date_char
   loop_count : DO
      CALL geth_newdate (next_date_char, current_date_char, interval_seconds )
      IF      ( next_date_char .LT. end_date_char ) THEN
         time_loop = time_loop + 1
         PRINT '(A,I4,A,A,A)','Time period #',time_loop,&
                              ' to process = ',next_date_char,'.'
         current_date_char = next_date_char
      ELSE IF ( next_date_char .EQ. end_date_char ) THEN
         time_loop = time_loop + 1
         PRINT '(A,I4,A,A,A)','Time period #',time_loop,&
                              ' to process = ',next_date_char,'.'
         PRINT '(A,I4,A)','Total analysis times to input = ',time_loop,'.'
         time_loop_max = time_loop
         EXIT loop_count
      ELSE IF ( next_date_char .GT. end_date_char ) THEN
         PRINT '(A,I4,A)','Total analysis times to input = ',time_loop,'.'
         time_loop_max = time_loop
         EXIT loop_count
      END IF
   END DO loop_count
        write(message,*) 'done in si_start_and_end'
        CALL wrf_message(message)
END SUBROUTINE compute_si_start_and_end

SUBROUTINE assemble_output ( grid , config_flags , loop , time_loop_max )

!!! replace with something?   USE module_big_step_utilities_em

   USE module_domain
   USE module_io_domain
   USE module_configure
   USE module_date_time
   USE module_bc
   IMPLICIT NONE

   TYPE(domain)                 :: grid
   TYPE (grid_config_rec_type)  :: config_flags
   INTEGER , INTENT(IN)         :: loop , time_loop_max

   INTEGER :: ids , ide , jds , jde , kds , kde
   INTEGER :: ims , ime , jms , jme , kms , kme
   INTEGER :: ips , ipe , jps , jpe , kps , kpe
   INTEGER :: ijds , ijde , spec_bdy_width
   INTEGER :: inc_h,inc_v
   INTEGER :: i , j , k , idts

   INTEGER :: id1 , interval_seconds , ierr, rc
   INTEGER , SAVE :: id 
   CHARACTER (LEN=80) :: inpname , bdyname
   CHARACTER(LEN= 4) :: loop_char
   CHARACTER(LEN=132) :: message
character *19 :: temp19
character *24 :: temp24 , temp24b

   REAL, DIMENSION(:,:,:), ALLOCATABLE, SAVE :: ubdy3dtemp1 , vbdy3dtemp1 ,&
                                                tbdy3dtemp1 , &
				                cwmbdy3dtemp1 , qbdy3dtemp1,&
                                                q2bdy3dtemp1 , pdbdy2dtemp1
   REAL, DIMENSION(:,:,:), ALLOCATABLE, SAVE :: ubdy3dtemp2 , vbdy3dtemp2 , &
                                                tbdy3dtemp2 , & 
                                                cwmbdy3dtemp2 , qbdy3dtemp2, &
                                                q2bdy3dtemp2, pdbdy2dtemp2
   REAL :: t1,t2





   !  Various sizes that we need to be concerned about.

   ids = grid%sd31
   ide = grid%ed31-1 ! 030730tst
   jds = grid%sd32
   jde = grid%ed32-1 ! 030730tst
   kds = grid%sd33
   kde = grid%ed33-1 ! 030730tst

   ims = grid%sm31
   ime = grid%em31
   jms = grid%sm32
   jme = grid%em32
   kms = grid%sm33
   kme = grid%em33

   ips = grid%sp31
   ipe = grid%ep31-1 ! 030730tst
   jps = grid%sp32
   jpe = grid%ep32-1 ! 030730tst
   kps = grid%sp33
   kpe = grid%ep33-1 ! 030730tst

        if (IPE .ne. IDE) IPE=IPE+1
        if (JPE .ne. JDE) JPE=JPE+1

        write(message,*) 'assemble output (ids,ide): ', ids,ide
        CALL wrf_message(message)
        write(message,*) 'assemble output (ims,ime): ', ims,ime
        CALL wrf_message(message)
        write(message,*) 'assemble output (ips,ipe): ', ips,ipe
        CALL wrf_message(message)
 
        write(message,*) 'assemble output (jds,jde): ', jds,jde
        CALL wrf_message(message)
        write(message,*) 'assemble output (jms,jme): ', jms,jme
        CALL wrf_message(message)
        write(message,*) 'assemble output (jps,jpe): ', jps,jpe
        CALL wrf_message(message)
 
        write(message,*) 'assemble output (kds,kde): ', kds,kde
        CALL wrf_message(message)
        write(message,*) 'assemble output (kms,kme): ', kms,kme
        CALL wrf_message(message)
        write(message,*) 'assemble output (kps,kpe): ', kps,kpe
        CALL wrf_message(message)

   ijds = MIN ( ids , jds )
!mptest030805   ijde = MAX ( ide , jde )
   ijde = MAX ( ide , jde ) + 1   ! to make stuff_bdy dimensions consistent with alloc

   !  Boundary width, scalar value.

   spec_bdy_width = model_config_rec%spec_bdy_width
   interval_seconds = model_config_rec%interval_seconds

!-----------------------------------------------------------------------
!
   main_loop_test: IF ( loop .EQ. 1 ) THEN
!
!-----------------------------------------------------------------------

   !  This is the space needed to save the current 3d data for use in computing
   !  the lateral boundary tendencies.

      ALLOCATE ( ubdy3dtemp1(ims:ime,jms:jme,kms:kme) )
      ALLOCATE ( vbdy3dtemp1(ims:ime,jms:jme,kms:kme) )
      ALLOCATE ( tbdy3dtemp1(ims:ime,jms:jme,kms:kme) )
      ALLOCATE ( qbdy3dtemp1(ims:ime,jms:jme,kms:kme) )
      ALLOCATE ( cwmbdy3dtemp1(ims:ime,jms:jme,kms:kme) )
      ALLOCATE ( q2bdy3dtemp1(ims:ime,jms:jme,kms:kme) )
      ALLOCATE ( pdbdy2dtemp1(ims:ime,jms:jme,1:1) )

	ubdy3dtemp1=0.
	vbdy3dtemp1=0.
	tbdy3dtemp1=0.
	qbdy3dtemp1=0.
	cwmbdy3dtemp1=0.
	q2bdy3dtemp1=0.
	pdbdy2dtemp1=0.

      ALLOCATE ( ubdy3dtemp2(ims:ime,jms:jme,kms:kme) )
      ALLOCATE ( vbdy3dtemp2(ims:ime,jms:jme,kms:kme) )
      ALLOCATE ( tbdy3dtemp2(ims:ime,jms:jme,kms:kme) )
      ALLOCATE ( qbdy3dtemp2(ims:ime,jms:jme,kms:kme) )
      ALLOCATE ( cwmbdy3dtemp2(ims:ime,jms:jme,kms:kme) )
      ALLOCATE ( q2bdy3dtemp2(ims:ime,jms:jme,kms:kme) )
      ALLOCATE ( pdbdy2dtemp2(ims:ime,jms:jme,1:1) )

	ubdy3dtemp2=0.
	vbdy3dtemp2=0.
	tbdy3dtemp2=0.
	qbdy3dtemp2=0.
	cwmbdy3dtemp2=0.
	q2bdy3dtemp2=0.
	pdbdy2dtemp2=0.

      !  Open the wrfinput file.  From this program, this is an *output* file.

      CALL construct_filename1( inpname , 'wrfinput' , grid%id , 2 )

      CALL open_w_dataset ( id1, TRIM(inpname) , grid , config_flags , &
                            output_model_input , "DATASET=INPUT", ierr )

      IF ( ierr .NE. 0 ) THEN
      CALL wrf_error_fatal3 ( "real_nmm.b" , 728 ,  'real: error opening wrfinput for writing' )
      ENDIF

!     CALL calc_current_date ( grid%id , 0. )
!      grid%write_metadata = .true.

        write(message,*) 'making call to output_model_input'
        CALL wrf_message(message)

        CALL output_model_input ( id1, grid , config_flags , ierr )

!***
!***  CLOSE THE WRFINPUT DATASET
!***
      CALL close_dataset ( id1 , config_flags , "DATASET=INPUT" )

      !  We need to save the 3d data to compute a 
      !  difference during the next loop. 

!
!-----------------------------------------------------------------------
!***  SOUTHERN BOUNDARY
!-----------------------------------------------------------------------
!

        IF(JPS==JDS)THEN
          J=1
          DO k = kps , MIN(kde,kpe)
          DO i = ips , MIN(ide,ipe)
            ubdy3dtemp1(i,j,k) = grid%nmm_u(i,j,k)
            vbdy3dtemp1(i,j,k) = grid%nmm_v(i,j,k)
            tbdy3dtemp1(i,j,k) = grid%nmm_t(i,j,k)
            qbdy3dtemp1(i,j,k) = grid%nmm_q(i,j,k)
            cwmbdy3dtemp1(i,j,k) = grid%nmm_cwm(i,j,k)
            q2bdy3dtemp1(i,j,k) = grid%nmm_q2(i,j,k)
          END DO
          END DO

          DO i = ips , MIN(ide,ipe)
            pdbdy2dtemp1(i,j,1) = grid%nmm_pd(i,j)
          END DO
        ENDIF

!
!-----------------------------------------------------------------------
!***  NORTHERN BOUNDARY
!-----------------------------------------------------------------------
!
        IF(JPE==JDE)THEN
          J=MIN(JDE,JPE)
          DO k = kps , MIN(kde,kpe)
          DO i = ips , MIN(ide,ipe)
            ubdy3dtemp1(i,j,k) = grid%nmm_u(i,j,k)
            vbdy3dtemp1(i,j,k) = grid%nmm_v(i,j,k)
            tbdy3dtemp1(i,j,k) = grid%nmm_t(i,j,k)
            qbdy3dtemp1(i,j,k) = grid%nmm_q(i,j,k)
            cwmbdy3dtemp1(i,j,k) = grid%nmm_cwm(i,j,k)
            q2bdy3dtemp1(i,j,k) = grid%nmm_q2(i,j,k)
          END DO
          END DO

          DO i = ips , MIN(ide,ipe)
            pdbdy2dtemp1(i,j,1) = grid%nmm_pd(i,j)
          END DO
        ENDIF

!
!-----------------------------------------------------------------------
!***  WESTERN BOUNDARY
!-----------------------------------------------------------------------
!
        write(message,*) 'western boundary, store winds over J: ', jps, min(jpe,jde)
        CALL wrf_message(message)

        IF(IPS==IDS)THEN
          I=1
          DO k = kps , MIN(kde,kpe)
          inc_h=mod(jps+1,2)
          DO j = jps+inc_h, min(jde,jpe),2

        if (J .ge. 3 .and. J .le. JDE-2 .and. mod(J,2) .eq. 1) then
            tbdy3dtemp1(i,j,k) = grid%nmm_t(i,j,k)
            qbdy3dtemp1(i,j,k) = grid%nmm_q(i,j,k)
            cwmbdy3dtemp1(i,j,k) = grid%nmm_cwm(i,j,k)
            q2bdy3dtemp1(i,j,k) = grid%nmm_q2(i,j,k)
      if(k==1)then
        write(message,*)' loop=',loop,' i=',i,' j=',j,' tbdy3dtemp1(i,j,k)=',tbdy3dtemp1(i,j,k)
        CALL wrf_debug(10,message)
      endif
	endif
          END DO
          END DO

          DO k = kps , MIN(kde,kpe)
          inc_v=mod(jps,2)
          DO j = jps+inc_v, min(jde,jpe),2
        if (J .ge. 2 .and. J .le. JDE-1 .and. mod(J,2) .eq. 0) then
            ubdy3dtemp1(i,j,k) = grid%nmm_u(i,j,k)
            vbdy3dtemp1(i,j,k) = grid%nmm_v(i,j,k)
	endif
          END DO
          END DO
!
          inc_h=mod(jps+1,2)
        DO j = jps+inc_h, min(jde,jpe),2
        if (J .ge. 3 .and. J .le. JDE-2 .and. mod(J,2) .eq. 1) then
            pdbdy2dtemp1(i,j,1) = grid%nmm_pd(i,j)
          write(message,*)' loop=',loop,' i=',i,' j=',j,' pdbdy2dtemp1(i,j)=',pdbdy2dtemp1(i,j,1)
          CALL wrf_debug(10,message)
	endif
          END DO
        ENDIF
!
!-----------------------------------------------------------------------
!***  EASTERN BOUNDARY
!-----------------------------------------------------------------------
!
        IF(IPE==IDE)THEN
          I=MIN(IDE,IPE)
!
          DO k = kps , MIN(kde,kpe)
!
!***   Make sure the J loop is on the global boundary
!
          inc_h=mod(jps+1,2)
          DO j = jps+inc_h, min(jde,jpe),2
        if (J .ge. 3 .and. J .le. JDE-2 .and. mod(J,2) .eq. 1) then
            tbdy3dtemp1(i,j,k) = grid%nmm_t(i,j,k)
            qbdy3dtemp1(i,j,k) = grid%nmm_q(i,j,k)
            cwmbdy3dtemp1(i,j,k) = grid%nmm_cwm(i,j,k)
            q2bdy3dtemp1(i,j,k) = grid%nmm_q2(i,j,k)
	endif
          END DO
          END DO

          DO k = kps , MIN(kde,kpe)
          inc_v=mod(jps,2)
          DO j = jps+inc_v, min(jde,jpe),2
        if (J .ge. 2 .and. J .le. JDE-1 .and. mod(J,2) .eq. 0) then
            ubdy3dtemp1(i,j,k) = grid%nmm_u(i,j,k)
            vbdy3dtemp1(i,j,k) = grid%nmm_v(i,j,k)
        endif
          END DO
          END DO
!
          inc_h=mod(jps+1,2)
          DO j = jps+inc_h, min(jde,jpe),2
        if (J .ge. 3 .and. J .le. JDE-2 .and. mod(J,2) .eq. 1) then
            pdbdy2dtemp1(i,j,1) = grid%nmm_pd(i,j)
	endif
          END DO
        ENDIF


      !  There are 2 components to the lateral boundaries.  
      !  First, there is the starting
      !  point of this time period - just the outer few rows and columns.


 CALL stuff_bdy_ijk (ubdy3dtemp1, grid%nmm_u_b, 'N', ijds, ijde, spec_bdy_width  , &
                                        ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                        ims , ime , jms , jme , kms , kme , &
                                        ips , ipe , jps , jpe , kps , kpe+1 )
 CALL stuff_bdy_ijk ( vbdy3dtemp1, grid%nmm_v_b, 'N', ijds, ijde, spec_bdy_width, &
                                        ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                        ims , ime , jms , jme , kms , kme , &
                                        ips , ipe , jps , jpe , kps , kpe+1 )

 CALL stuff_bdy_ijk ( tbdy3dtemp1, grid%nmm_t_b, 'N', ijds, ijde, spec_bdy_width, &
                                        ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                        ims , ime , jms , jme , kms , kme , &
                                        ips , ipe , jps , jpe , kps , kpe+1 )

 CALL stuff_bdy_ijk ( cwmbdy3dtemp1,grid%nmm_cwm_b,'N',ijds,ijde, spec_bdy_width, &
                                        ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                        ims , ime , jms , jme , kms , kme , &
                                        ips , ipe , jps , jpe , kps , kpe+1 )

 CALL stuff_bdy_ijk ( qbdy3dtemp1, grid%nmm_q_b, 'N', ijds, ijde, spec_bdy_width, &
                                        ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                        ims , ime , jms , jme , kms , kme , &
                                        ips , ipe , jps , jpe , kps , kpe+1 )

 CALL stuff_bdy_ijk ( q2bdy3dtemp1,grid%nmm_q2_b,'N', ijds, ijde, spec_bdy_width, &
                                        ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                        ims , ime , jms , jme , kms , kme , &
                                        ips , ipe , jps , jpe , kps , kpe+1 )

 CALL stuff_bdy_ijk ( pdbdy2dtemp1,grid%nmm_pd_b,'M', ijds,ijde, spec_bdy_width, &
                                        ids , ide+1 , jds , jde+1 , 1 , 1 , &
                                        ims , ime , jms , jme , 1 , 1 , &
                                        ips , ipe , jps , jpe , 1 , 1 )

!-----------------------------------------------------------------------
!
   ELSE IF ( loop .GT. 1 ) THEN
!
!-----------------------------------------------------------------------

      write(message,*)' assemble_output loop=',loop,' in IF block'
      call wrf_message(message)

      !  Open the boundary file.

      IF ( loop .eq. 2 ) THEN
         CALL construct_filename1( bdyname , 'wrfbdy' , grid%id , 2 )
      CALL open_w_dataset ( id, TRIM(bdyname) , grid , config_flags , &
                          output_boundary , "DATASET=BOUNDARY", ierr )
         IF ( ierr .NE. 0 ) THEN
               CALL wrf_error_fatal3 ( "real_nmm.b" , 937 ,  'real: error opening wrfbdy for writing' )
         ENDIF
!         grid%write_metadata = .true.
      ELSE
! whats this do?
!         grid%write_metadata = .true.
!         grid%write_metadata = .false.
         CALL domain_clockadvance( grid )
      END IF

!
!-----------------------------------------------------------------------
!***  SOUTHERN BOUNDARY
!-----------------------------------------------------------------------
!
        IF(JPS==JDS)THEN
          J=1
          DO k = kps , MIN(kde,kpe)
          DO i = ips , MIN(ide,ipe)
            ubdy3dtemp2(i,j,k) = grid%nmm_u(i,j,k)
            vbdy3dtemp2(i,j,k) = grid%nmm_v(i,j,k)
            tbdy3dtemp2(i,j,k) = grid%nmm_t(i,j,k)
            qbdy3dtemp2(i,j,k) = grid%nmm_q(i,j,k)
            cwmbdy3dtemp2(i,j,k) = grid%nmm_cwm(i,j,k)
            q2bdy3dtemp2(i,j,k) = grid%nmm_q2(i,j,k)
          END DO
          END DO
!
          DO i = ips , MIN(ide,ipe)
            pdbdy2dtemp2(i,j,1) = grid%nmm_pd(i,j)
          END DO
        ENDIF

!
!-----------------------------------------------------------------------
!***  NORTHERN BOUNDARY
!-----------------------------------------------------------------------
!
        IF(JPE==JDE)THEN
          J=MIN(JDE,JPE)
          DO k = kps , MIN(kde,kpe)
          DO i = ips , MIN(ide,ipe)
            ubdy3dtemp2(i,j,k) = grid%nmm_u(i,j,k)
            vbdy3dtemp2(i,j,k) = grid%nmm_v(i,j,k)
            tbdy3dtemp2(i,j,k) = grid%nmm_t(i,j,k)
            qbdy3dtemp2(i,j,k) = grid%nmm_q(i,j,k)
            cwmbdy3dtemp2(i,j,k) = grid%nmm_cwm(i,j,k)
            q2bdy3dtemp2(i,j,k) = grid%nmm_q2(i,j,k)
          END DO
          END DO

          DO i = ips , MIN(ide,ipe)
            pdbdy2dtemp2(i,j,1) = grid%nmm_pd(i,j)
          END DO
        ENDIF
!
!-----------------------------------------------------------------------
!***  WESTERN BOUNDARY
!-----------------------------------------------------------------------
!
        IF(IPS==IDS)THEN
          I=1
          DO k = kps , MIN(kde,kpe)
          inc_h=mod(jps+1,2)
      if(k==1)then
        write(message,*)' assemble_ouput loop=',loop,' inc_h=',inc_h,' jps=',jps
        call wrf_debug(10,message)
      endif
          DO j = jps+inc_h, MIN(jde,jpe),2
        if (J .ge. 3 .and. J .le. jde-2 .and. mod(J,2) .eq. 1) then
            tbdy3dtemp2(i,j,k) = grid%nmm_t(i,j,k)
      if(k==1)then
        write(message,*)' loop=',loop,' i=',i,' j=',j,' tbdy3dtemp1(i,j,k)=',tbdy3dtemp1(i,j,k)
        call wrf_debug(10,message)
      endif
            qbdy3dtemp2(i,j,k) = grid%nmm_q(i,j,k)
            cwmbdy3dtemp2(i,j,k) = grid%nmm_cwm(i,j,k)
            q2bdy3dtemp2(i,j,k) = grid%nmm_q2(i,j,k)
	endif
          END DO
          END DO
!
          DO k = kps , MIN(kde,kpe)
          inc_v=mod(jps,2)
          DO j = jps+inc_v, MIN(jde,jpe),2
        if (J .ge. 2 .and. J .le. jde-1 .and. mod(J,2) .eq. 0) then
            ubdy3dtemp2(i,j,k) = grid%nmm_u(i,j,k)
            vbdy3dtemp2(i,j,k) = grid%nmm_v(i,j,k)
	endif
          END DO
          END DO

          inc_h=mod(jps+1,2)
        DO j = jps+inc_h, MIN(jde,jpe),2
        if (J .ge. 3 .and. J .le. jde-2 .and. mod(J,2) .eq. 1) then
          pdbdy2dtemp2(i,j,1) = grid%nmm_pd(i,j)
          write(message,*)' loop=',loop,' i=',i,' j=',j,' pdbdy2dtemp1(i,j)=',pdbdy2dtemp1(i,j,1)
          CALL wrf_debug(10,message)
	endif
          END DO
        ENDIF
!
!-----------------------------------------------------------------------
!***  EASTERN BOUNDARY
!-----------------------------------------------------------------------
!
        IF(IPE==IDE)THEN
          I=MIN(IDE,IPE)

          DO k = kps , MIN(kde,kpe)
          inc_h=mod(jps+1,2)
          DO j = jps+inc_h, MIN(jde,jpe),2
        if (J .ge. 3 .and. J .le. jde-2 .and. mod(J,2) .eq. 1) then
            tbdy3dtemp2(i,j,k) = grid%nmm_t(i,j,k)
            qbdy3dtemp2(i,j,k) = grid%nmm_q(i,j,k)
            cwmbdy3dtemp2(i,j,k) = grid%nmm_cwm(i,j,k)
            q2bdy3dtemp2(i,j,k) = grid%nmm_q2(i,j,k)
	endif
          END DO
          END DO

          DO k = kps , MIN(kde,kpe)
          inc_v=mod(jps,2)
          DO j = jps+inc_v, MIN(jde,jpe),2
        if (J .ge. 2 .and. J .le. jde-1 .and. mod(J,2) .eq. 0) then
            ubdy3dtemp2(i,j,k) = grid%nmm_u(i,j,k)
            vbdy3dtemp2(i,j,k) = grid%nmm_v(i,j,k)
	endif
          END DO
          END DO

          inc_h=mod(jps+1,2)
          DO j = jps+inc_h, MIN(jde,jpe),2
        if (J .ge. 3 .and. J .le. jde-2 .and. mod(J,2) .eq. 1) then
            pdbdy2dtemp2(i,j,1) = grid%nmm_pd(i,j)
	endif
          END DO
        ENDIF
!-----------------------------------------------------------------------
      !  During all of the loops after the first loop, 
      !  we first compute the boundary
      !  tendencies with the current data values 
      !  (*bdy3dtemp2 arrays) and the previously 
      !  saved information stored in the *bdy3dtemp1 arrays.


      CALL stuff_bdytend_ijk ( ubdy3dtemp2 , ubdy3dtemp1 , REAL(interval_seconds),&
                                   grid%nmm_u_bt  , 'N' , &
                                   ijds , ijde , spec_bdy_width      , &
                                   ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                   ims , ime , jms , jme , kms , kme , &
                                   ips , ipe , jps , jpe , kps , kpe+1 )
      CALL stuff_bdytend_ijk ( vbdy3dtemp2 , vbdy3dtemp1 , REAL(interval_seconds),&
                                    grid%nmm_v_bt  , 'N' , &
                                   ijds , ijde , spec_bdy_width      , &
                                   ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                   ims , ime , jms , jme , kms , kme , &
                                   ips , ipe , jps , jpe , kps , kpe+1 )
      CALL stuff_bdytend_ijk ( tbdy3dtemp2 , tbdy3dtemp1 , REAL(interval_seconds),&
                                   grid%nmm_t_bt  , 'N' , &
                                   ijds , ijde , spec_bdy_width      , &
                                   ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                   ims , ime , jms , jme , kms , kme , &
                                   ips , ipe , jps , jpe , kps , kpe+1 )

      CALL stuff_bdytend_ijk ( cwmbdy3dtemp2,cwmbdy3dtemp1,REAL(interval_seconds),&
                                   grid%nmm_cwm_bt  , 'N' , &
                                   ijds , ijde , spec_bdy_width      , &
                                   ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                   ims , ime , jms , jme , kms , kme , &
                                   ips , ipe , jps , jpe , kps , kpe+1 )

      CALL stuff_bdytend_ijk ( qbdy3dtemp2 , qbdy3dtemp1 , REAL(interval_seconds),&
                                   grid%nmm_q_bt , 'N' , &
                                   ijds , ijde , spec_bdy_width      , &
                                   ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                   ims , ime , jms , jme , kms , kme , &
                                   ips , ipe , jps , jpe , kps , kpe+1 )

    CALL stuff_bdytend_ijk ( q2bdy3dtemp2, q2bdy3dtemp1 , REAL(interval_seconds),&
                                   grid%nmm_q2_bt , 'N' , &
                                   ijds , ijde , spec_bdy_width      , &
                                   ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                   ims , ime , jms , jme , kms , kme , &
                                   ips , ipe , jps , jpe , kps , kpe+1 )

    CALL stuff_bdytend_ijk( pdbdy2dtemp2 , pdbdy2dtemp1, REAL(interval_seconds),&
                                   grid%nmm_pd_bt  , 'M' , &
                                   ijds , ijde , spec_bdy_width      , &
                                   ids , ide+1 , jds , jde+1 , 1 , 1 , &
                                   ims , ime , jms , jme , 1 , 1 , &
                                   ips , ipe , jps , jpe , 1 , 1 )

      !  Both pieces of the boundary data are now 
      !  available to be written (initial time and tendency).
      !  This looks ugly, these date shifting things.  
      !  Whats it for?  We want the "Times" variable
      !  in the lateral BDY file to have the valid times 
      !  of when the initial fields are written.
      !  Thats what the loop-2 thingy is for with the start date.  
      !  We increment the start_date so
      !  that the starting time in the attributes is the 
      !  second time period.  Why you may ask.  I
      !  agree, why indeed.

      temp24= current_date
      temp24b=start_date
      start_date = current_date
      CALL geth_newdate ( temp19 , temp24b(1:19) , &
                         (loop-2) * model_config_rec%interval_seconds )
      current_date = temp19 //  '.0000'
       CALL domain_clock_set( grid, current_date(1:19) )
      write(message,*) 'LBC valid between these times ',current_date, ' ',start_date
      CALL wrf_message(message)

      CALL output_boundary ( id, grid , config_flags , ierr )
      current_date = temp24
      start_date = temp24b

      !  OK, for all of the loops, we output the initialzation 
      !  data, which would allow us to
      !  start the model at any of the available analysis time periods.

!  WRITE ( loop_char , FMT = (I4.4) ) loop
!  CALL open_w_dataset ( id1, wrfinput//loop_char , grid , config_flags , output_model_input , "DATASET=INPUT", ierr )
!  IF ( ierr .NE. 0 ) THEN
!    CALL wrf_error_fatal3 ( "real_nmm.b" , 1163 ,  real: error opening wrfinput//loop_char// for writing )
!  ENDIF
!  grid%write_metadata = .true.

!  CALL calc_current_date ( grid%id , 0. )
!  CALL output_model_input ( id1, grid , config_flags , ierr )
!  CALL close_dataset ( id1 , config_flags , "DATASET=INPUT" )

  !  Is this or is this not the last time time?  We can remove some unnecessary
  !  stores if it is not.

      IF     ( loop .LT. time_loop_max ) THEN

         !  We need to save the 3d data to compute a 
         !  difference during the next loop.  Couple the
         !  3d fields with total mu (mub + mu_2) and the 
         !  stagger-specific map scale factor.
         !  We load up the boundary data again for use in the next loop.


!mp	change these limits?????????

         DO k = kps , kpe
            DO j = jps , jpe
               DO i = ips , ipe
                  ubdy3dtemp1(i,j,k) = ubdy3dtemp2(i,j,k)
                  vbdy3dtemp1(i,j,k) = vbdy3dtemp2(i,j,k)
                  tbdy3dtemp1(i,j,k) = tbdy3dtemp2(i,j,k)
                  cwmbdy3dtemp1(i,j,k) = cwmbdy3dtemp2(i,j,k)
                  qbdy3dtemp1(i,j,k) = qbdy3dtemp2(i,j,k)
                  q2bdy3dtemp1(i,j,k) = q2bdy3dtemp2(i,j,k)
               END DO
            END DO
         END DO

!mp	change these limits?????????

         DO j = jps , jpe
            DO i = ips , ipe
               pdbdy2dtemp1(i,j,1) = pdbdy2dtemp2(i,j,1)
            END DO
         END DO

  !  There are 2 components to the lateral boundaries.  
  !   First, there is the starting
  !  point of this time period - just the outer few rows and columns.


         CALL stuff_bdy_ijk ( ubdy3dtemp1 , grid%nmm_u_b  , 'N' ,& 
                                        ijds , ijde , spec_bdy_width      , &
                                        ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                        ims , ime , jms , jme , kms , kme , &
                                        ips , ipe , jps , jpe , kps , kpe+1 )
         CALL stuff_bdy_ijk ( vbdy3dtemp1 , grid%nmm_v_b  , 'N' , &
                                        ijds , ijde , spec_bdy_width      , &
                                        ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                        ims , ime , jms , jme , kms , kme , &
                                        ips , ipe , jps , jpe , kps , kpe+1 )
         CALL stuff_bdy_ijk ( tbdy3dtemp1 , grid%nmm_t_b  , 'N' , &
                                        ijds , ijde , spec_bdy_width      , &
                                        ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                        ims , ime , jms , jme , kms , kme , &
                                        ips , ipe , jps , jpe , kps , kpe+1 )

         CALL stuff_bdy_ijk ( cwmbdy3dtemp1 , grid%nmm_cwm_b , 'N' , &
                                          ijds , ijde , spec_bdy_width      , &
                                          ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                          ims , ime , jms , jme , kms , kme , &
                                          ips , ipe , jps , jpe , kps , kpe+1 )

         CALL stuff_bdy_ijk ( qbdy3dtemp1 , grid%nmm_q_b , 'N' ,&
                                        ijds , ijde , spec_bdy_width      , &
                                        ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                        ims , ime , jms , jme , kms , kme , &
                                        ips , ipe , jps , jpe , kps , kpe+1 )

         CALL stuff_bdy_ijk ( q2bdy3dtemp1 , grid%nmm_q2_b, 'N' ,&
                                         ijds , ijde , spec_bdy_width      , &
                                         ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                         ims , ime , jms , jme , kms , kme , &
                                         ips , ipe , jps , jpe , kps , kpe+1 )

         CALL stuff_bdy_ijk ( pdbdy2dtemp1 , grid%nmm_pd_b , 'M' ,&
                                          ijds , ijde , spec_bdy_width  , &
                                          ids , ide+1 , jds , jde+1 , 1 , 1 , &
                                          ims , ime , jms , jme , 1 , 1 , &
                                          ips , ipe , jps , jpe , 1 , 1 )

      ELSE IF ( loop .EQ. time_loop_max ) THEN

    !  If this is the last time through here, we need to close the files.

         CALL close_dataset ( id , config_flags , "DATASET=BOUNDARY" )

      END IF

   END IF main_loop_test

END SUBROUTINE assemble_output
