


PROGRAM real_data

   USE module_machine
   USE module_domain
   USE module_initialize_real
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

   INTEGER, PARAMETER      :: configbuflen = 4*32768
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

   CHARACTER (LEN=10) :: release_version = 'V3.1      '

   INTERFACE
     SUBROUTINE Setup_Timekeeping( grid )
      USE module_domain
      TYPE(domain), POINTER :: grid
     END SUBROUTINE Setup_Timekeeping
   END INTERFACE

   

   program_name = "REAL_NMM " // TRIM(release_version) // " PREPROCESSOR"

   CALL disable_quilting



   
   
   
   
   
   

   CALL       wrf_debug ( 100 , 'real_nmm: calling init_modules ' )


   CALL init_modules(1)   
   CALL WRFU_Initialize( defaultCalendar=WRFU_CAL_GREGORIAN, rc=rc )
   CALL init_modules(2)   

   

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

   

   CALL       wrf_debug ( 100 , 'real_nmm: calling init_wrfio' )
   CALL init_wrfio




   CALL wrf_debug ( 100 , 'real_nmm: re-broadcast the configuration records' )
   CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
   CALL wrf_dm_bcast_bytes( configbuf, nbytes )
   CALL set_config_as_buffer( configbuf, configbuflen )

   

   CALL med_sidata_input ( grid , config_flags )

   

   CALL       wrf_debug (   0 , 'real_nmm: SUCCESS COMPLETE REAL_NMM INIT' )

    CALL wrf_dm_shutdown

   CALL WRFU_Finalize( rc=rc )

END PROGRAM real_data

SUBROUTINE med_sidata_input ( grid , config_flags )
  
   USE module_domain
   USE module_io_domain
  
   USE module_configure
   USE module_bc_time_utilities
   USE module_initialize_real
   USE module_optional_input

   USE module_si_io_nmm

   USE module_date_time

   IMPLICIT NONE


  
   INTERFACE
     SUBROUTINE start_domain ( grid , allowed_to_read )
       USE module_domain
       TYPE (domain) grid
       LOGICAL, INTENT(IN) :: allowed_to_read
     END SUBROUTINE start_domain
   END INTERFACE

  
   TYPE(domain)                :: grid
   TYPE (grid_config_rec_type) :: config_flags
  
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

   

   current_date_char = start_date_char

   start_date = start_date_char 
   current_date = start_date

   CALL nl_set_bdyfrq ( grid%id , REAL(model_config_rec%interval_seconds) )

   

   write(message,*) 'time_loop_max: ', time_loop_max
   CALL wrf_message(message)
   DO loop = 1 , time_loop_max

     internal_time_loop=loop
                                                                                                                                              
      write(message,*) 'loop=', loop
      CALL wrf_message(message)
                                                                                                                                              
      write(message,*) '-----------------------------------------------------------'
      CALL wrf_message(message)
                      
      write(message,*) ' '
      CALL wrf_message(message)
      write(message,'(A,A,A,I2,A,I2)') ' Current date being processed: ', &
        current_date, ', which is loop #',loop,' out of ',time_loop_max
      CALL wrf_message(message)

      

      CALL geth_julgmt ( config_flags%julyr , config_flags%julday , &
                                              config_flags%gmt )

      
      

      CALL nl_set_gmt (grid%id, config_flags%gmt)
      CALL nl_set_julyr (grid%id, config_flags%julyr)
      CALL nl_set_julday (grid%id, config_flags%julday)

      CALL nl_get_io_form_auxinput1( 1, io_form_auxinput1 )
      using_binary_wrfsi=.false.
       
       
      write(message,*) 'TRIM(config_flags%auxinput1_inname): ', TRIM(config_flags%auxinput1_inname)
      CALL wrf_message(message)
       
      IF (config_flags%auxinput1_inname(1:10) .eq. 'real_input') THEN
         using_binary_wrfsi=.true.
      ENDIF

      SELECT CASE ( use_package(io_form_auxinput1) )
      CASE ( IO_NETCDF   )

      

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
          CALL wrf_error_fatal3("",301,&
'error opening ' // TRIM(si_inpname) // ' for input; bad date in namelist or file not in directory' )
       ENDIF

      

      CALL wrf_debug (100, 'med_sidata_input: call input_aux_model_input1_wrf')

      CALL input_aux_model_input1 ( idsi, grid, config_flags, ierr )

      

      IF ( loop .EQ. 1 ) THEN
         CALL  wrf_debug (100, 'med_sidata_input: call init_module_optional_input' )
         CALL init_module_optional_input ( grid , config_flags )
      CALL wrf_debug ( 100 , 'med_sidata_input: calling optional_input' )

      CALL optional_input ( grid , idsi , config_flags )
	write(0,*) 'maxval st_input(1) within real_nmm: ', maxval(st_input(:,1,:))
      END IF

      CALL close_dataset ( idsi , config_flags , "DATASET=AUXINPUT1" )

      CASE ( IO_INTIO )

      

      IF ( loop .EQ. 1 ) THEN
         CALL  wrf_debug (100, 'med_sidata_input: call init_module_optional_input' )
         CALL init_module_optional_input ( grid , config_flags )
      END IF

      IF (using_binary_wrfsi) THEN

        current_date_char(11:11)='_'
        CALL read_si ( grid, current_date_char )
        current_date_char(11:11)='T'

      ELSE
                                                                                                                                              
        write(message,*) 'binary WPS branch'
        CALL wrf_message(message)

        CALL wrf_message(message)
        CALL wrf_message(message)
        current_date_char(11:11)='_'
        CALL construct_filename4a( si_inpname , config_flags%auxinput1_inname , grid%id , 2 , current_date_char , &
                                     config_flags%io_form_auxinput1 )


        CALL read_wps ( grid, trim(si_inpname), current_date_char, config_flags%num_metgrid_levels )





      flag_metgrid=1
      flag_soilhgt=1

                                                                                                                                              
      
                                                                                                                                              




                                                                                                                                              


 
      
 








 


 
          ENDIF

      CASE DEFAULT
        CALL wrf_error_fatal3("",388,&
'real: not valid io_form_auxinput1')
      END SELECT

      grid%islope=1
      grid%vegfra=grid%vegfrc
      grid%dfrlg=grid%dfl/9.81

      grid%isurban=1
      grid%isoilwater=14

      

      CALL wrf_debug ( 100 , 'med_sidata_input: calling init_domain' )
      grid%input_from_file = .true.



      CALL init_domain ( grid )




      CALL model_to_grid_config_rec ( grid%id, model_config_rec, config_flags )

      

      CALL wrf_debug ( 100 , 'med_sidata_input: back from init_domain' )



      IF ( loop .EQ. 1 ) THEN
        CALL start_domain ( grid , .TRUE.)
      END IF


      config_flags%isurban=1
      config_flags%isoilwater=14

      CALL assemble_output ( grid , config_flags , loop , time_loop_max )

      

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








   WRITE ( start_date_char , FMT = &
         '(I4.4,"-",I2.2,"-",I2.2,"T",I2.2,":",I2.2,":",I2.2)' ) &
         start_year,start_month,start_day,start_hour,start_minute,start_second
   WRITE (   end_date_char , FMT = &
         '(I4.4,"-",I2.2,"-",I2.2,"T",I2.2,":",I2.2,":",I2.2)' ) &
          end_year,  end_month,  end_day,  end_hour,  end_minute,  end_second



   

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





   

   ids = grid%sd31
   ide = grid%ed31-1 
   jds = grid%sd32
   jde = grid%ed32-1 
   kds = grid%sd33
   kde = grid%ed33-1 

   ims = grid%sm31
   ime = grid%em31
   jms = grid%sm32
   jme = grid%em32
   kms = grid%sm33
   kme = grid%em33

   ips = grid%sp31
   ipe = grid%ep31-1 
   jps = grid%sp32
   jpe = grid%ep32-1 
   kps = grid%sp33
   kpe = grid%ep33-1 

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

   ijde = MAX ( ide , jde ) + 1   

   

   spec_bdy_width = model_config_rec%spec_bdy_width
   interval_seconds = model_config_rec%interval_seconds



   main_loop_test: IF ( loop .EQ. 1 ) THEN



   
   

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

      

      CALL construct_filename1( inpname , 'wrfinput' , grid%id , 2 )

      CALL open_w_dataset ( id1, TRIM(inpname) , grid , config_flags , &
                            output_model_input , "DATASET=INPUT", ierr )

      IF ( ierr .NE. 0 ) THEN
      CALL wrf_error_fatal3("",661,&
'real: error opening wrfinput for writing' )
      ENDIF




        write(message,*) 'making call to output_model_input'
        CALL wrf_message(message)

        CALL output_model_input ( id1, grid , config_flags , ierr )




      CALL close_dataset ( id1 , config_flags , "DATASET=INPUT" )

      
      







        IF(JPS==JDS)THEN
          J=1
          DO k = kps , MIN(kde,kpe)
          DO i = ips , MIN(ide,ipe)
            ubdy3dtemp1(i,j,k) = grid%u(i,j,k)
            vbdy3dtemp1(i,j,k) = grid%v(i,j,k)
            tbdy3dtemp1(i,j,k) = grid%t(i,j,k)
            qbdy3dtemp1(i,j,k) = grid%q(i,j,k)
            cwmbdy3dtemp1(i,j,k) = grid%cwm(i,j,k)
            q2bdy3dtemp1(i,j,k) = grid%q2(i,j,k)
          END DO
          END DO

          DO i = ips , MIN(ide,ipe)
            pdbdy2dtemp1(i,j,1) = grid%pd(i,j)
          END DO
        ENDIF






        IF(JPE==JDE)THEN
          J=MIN(JDE,JPE)
          DO k = kps , MIN(kde,kpe)
          DO i = ips , MIN(ide,ipe)
            ubdy3dtemp1(i,j,k) = grid%u(i,j,k)
            vbdy3dtemp1(i,j,k) = grid%v(i,j,k)
            tbdy3dtemp1(i,j,k) = grid%t(i,j,k)
            qbdy3dtemp1(i,j,k) = grid%q(i,j,k)
            cwmbdy3dtemp1(i,j,k) = grid%cwm(i,j,k)
            q2bdy3dtemp1(i,j,k) = grid%q2(i,j,k)
          END DO
          END DO

          DO i = ips , MIN(ide,ipe)
            pdbdy2dtemp1(i,j,1) = grid%pd(i,j)
          END DO
        ENDIF






        write(message,*) 'western boundary, store winds over J: ', jps, min(jpe,jde)
        CALL wrf_message(message)

        IF(IPS==IDS)THEN
          I=1
          DO k = kps , MIN(kde,kpe)
          inc_h=mod(jps+1,2)
          DO j = jps+inc_h, min(jde,jpe),2

        if (J .ge. 3 .and. J .le. JDE-2 .and. mod(J,2) .eq. 1) then
            tbdy3dtemp1(i,j,k) = grid%t(i,j,k)
            qbdy3dtemp1(i,j,k) = grid%q(i,j,k)
            cwmbdy3dtemp1(i,j,k) = grid%cwm(i,j,k)
            q2bdy3dtemp1(i,j,k) = grid%q2(i,j,k)
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
            ubdy3dtemp1(i,j,k) = grid%u(i,j,k)
            vbdy3dtemp1(i,j,k) = grid%v(i,j,k)
	endif
          END DO
          END DO

          inc_h=mod(jps+1,2)
        DO j = jps+inc_h, min(jde,jpe),2
        if (J .ge. 3 .and. J .le. JDE-2 .and. mod(J,2) .eq. 1) then
            pdbdy2dtemp1(i,j,1) = grid%pd(i,j)
          write(message,*)' loop=',loop,' i=',i,' j=',j,' pdbdy2dtemp1(i,j)=',pdbdy2dtemp1(i,j,1)
          CALL wrf_debug(10,message)
	endif
          END DO
        ENDIF





        IF(IPE==IDE)THEN
          I=MIN(IDE,IPE)

          DO k = kps , MIN(kde,kpe)



          inc_h=mod(jps+1,2)
          DO j = jps+inc_h, min(jde,jpe),2
        if (J .ge. 3 .and. J .le. JDE-2 .and. mod(J,2) .eq. 1) then
            tbdy3dtemp1(i,j,k) = grid%t(i,j,k)
            qbdy3dtemp1(i,j,k) = grid%q(i,j,k)
            cwmbdy3dtemp1(i,j,k) = grid%cwm(i,j,k)
            q2bdy3dtemp1(i,j,k) = grid%q2(i,j,k)
	endif
          END DO
          END DO

          DO k = kps , MIN(kde,kpe)
          inc_v=mod(jps,2)
          DO j = jps+inc_v, min(jde,jpe),2
        if (J .ge. 2 .and. J .le. JDE-1 .and. mod(J,2) .eq. 0) then
            ubdy3dtemp1(i,j,k) = grid%u(i,j,k)
            vbdy3dtemp1(i,j,k) = grid%v(i,j,k)
        endif
          END DO
          END DO

          inc_h=mod(jps+1,2)
          DO j = jps+inc_h, min(jde,jpe),2
        if (J .ge. 3 .and. J .le. JDE-2 .and. mod(J,2) .eq. 1) then
            pdbdy2dtemp1(i,j,1) = grid%pd(i,j)
	endif
          END DO
        ENDIF


      
      
      


 CALL stuff_bdy_ijk (ubdy3dtemp1, grid%u_bxs, grid%u_bxe, &
                                  grid%u_bys, grid%u_bye, &
                                  'N', spec_bdy_width  , &
                                  ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                  ims , ime , jms , jme , kms , kme , &
                                  ips , ipe , jps , jpe , kps , kpe+1 )

 CALL stuff_bdy_ijk (vbdy3dtemp1, grid%v_bxs, grid%v_bxe, &
                                  grid%v_bys, grid%v_bye, &
                                  'N', spec_bdy_width  , &
                                  ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                  ims , ime , jms , jme , kms , kme , &
                                  ips , ipe , jps , jpe , kps , kpe+1 )

 CALL stuff_bdy_ijk (tbdy3dtemp1, grid%t_bxs, grid%t_bxe, &
                                  grid%t_bys, grid%t_bye, &
                                  'N', spec_bdy_width  , &
                                  ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                  ims , ime , jms , jme , kms , kme , &
                                  ips , ipe , jps , jpe , kps , kpe+1 )

 CALL stuff_bdy_ijk (cwmbdy3dtemp1, grid%cwm_bxs, grid%cwm_bxe, &
                                  grid%cwm_bys, grid%cwm_bye, &
                                  'N', spec_bdy_width  , &
                                  ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                  ims , ime , jms , jme , kms , kme , &
                                  ips , ipe , jps , jpe , kps , kpe+1 )

 CALL stuff_bdy_ijk (qbdy3dtemp1, grid%q_bxs, grid%q_bxe, &
                                  grid%q_bys, grid%q_bye, &
                                  'N', spec_bdy_width  , &
                                  ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                  ims , ime , jms , jme , kms , kme , &
                                  ips , ipe , jps , jpe , kps , kpe+1 )

 CALL stuff_bdy_ijk (q2bdy3dtemp1, grid%q2_bxs, grid%q2_bxe, &
                                  grid%q2_bys, grid%q2_bye, &
                                  'N', spec_bdy_width  , &
                                  ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                  ims , ime , jms , jme , kms , kme , &
                                  ips , ipe , jps , jpe , kps , kpe+1 )


 CALL stuff_bdy_ijk (pdbdy2dtemp1, grid%pd_bxs, grid%pd_bxe, &
                                   grid%pd_bys, grid%pd_bye, &
                                   'M', spec_bdy_width, &
                                   ids , ide+1 , jds , jde+1 , 1 , 1 , &
                                   ims , ime , jms , jme , 1 , 1 , &
                                   ips , ipe , jps , jpe , 1 , 1 )



   ELSE IF ( loop .GT. 1 ) THEN



      write(message,*)' assemble_output loop=',loop,' in IF block'
      call wrf_message(message)

      

      IF ( loop .eq. 2 ) THEN
         CALL construct_filename1( bdyname , 'wrfbdy' , grid%id , 2 )
      CALL open_w_dataset ( id, TRIM(bdyname) , grid , config_flags , &
                          output_boundary , "DATASET=BOUNDARY", ierr )
         IF ( ierr .NE. 0 ) THEN
               CALL wrf_error_fatal3("",887,&
'real: error opening wrfbdy for writing' )
         ENDIF

      ELSE



         CALL domain_clockadvance( grid )
      END IF






        IF(JPS==JDS)THEN
          J=1
          DO k = kps , MIN(kde,kpe)
          DO i = ips , MIN(ide,ipe)
            ubdy3dtemp2(i,j,k) = grid%u(i,j,k)
            vbdy3dtemp2(i,j,k) = grid%v(i,j,k)
            tbdy3dtemp2(i,j,k) = grid%t(i,j,k)
            qbdy3dtemp2(i,j,k) = grid%q(i,j,k)
            cwmbdy3dtemp2(i,j,k) = grid%cwm(i,j,k)
            q2bdy3dtemp2(i,j,k) = grid%q2(i,j,k)
          END DO
          END DO

          DO i = ips , MIN(ide,ipe)
            pdbdy2dtemp2(i,j,1) = grid%pd(i,j)
          END DO
        ENDIF






        IF(JPE==JDE)THEN
          J=MIN(JDE,JPE)
          DO k = kps , MIN(kde,kpe)
          DO i = ips , MIN(ide,ipe)
            ubdy3dtemp2(i,j,k) = grid%u(i,j,k)
            vbdy3dtemp2(i,j,k) = grid%v(i,j,k)
            tbdy3dtemp2(i,j,k) = grid%t(i,j,k)
            qbdy3dtemp2(i,j,k) = grid%q(i,j,k)
            cwmbdy3dtemp2(i,j,k) = grid%cwm(i,j,k)
            q2bdy3dtemp2(i,j,k) = grid%q2(i,j,k)
          END DO
          END DO

          DO i = ips , MIN(ide,ipe)
            pdbdy2dtemp2(i,j,1) = grid%pd(i,j)
          END DO
        ENDIF





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
            tbdy3dtemp2(i,j,k) = grid%t(i,j,k)
      if(k==1)then
        write(message,*)' loop=',loop,' i=',i,' j=',j,' tbdy3dtemp1(i,j,k)=',tbdy3dtemp1(i,j,k)
        call wrf_debug(10,message)
      endif
            qbdy3dtemp2(i,j,k) = grid%q(i,j,k)
            cwmbdy3dtemp2(i,j,k) = grid%cwm(i,j,k)
            q2bdy3dtemp2(i,j,k) = grid%q2(i,j,k)
	endif
          END DO
          END DO

          DO k = kps , MIN(kde,kpe)
          inc_v=mod(jps,2)
          DO j = jps+inc_v, MIN(jde,jpe),2
        if (J .ge. 2 .and. J .le. jde-1 .and. mod(J,2) .eq. 0) then
            ubdy3dtemp2(i,j,k) = grid%u(i,j,k)
            vbdy3dtemp2(i,j,k) = grid%v(i,j,k)
	endif
          END DO
          END DO

          inc_h=mod(jps+1,2)
        DO j = jps+inc_h, MIN(jde,jpe),2
        if (J .ge. 3 .and. J .le. jde-2 .and. mod(J,2) .eq. 1) then
          pdbdy2dtemp2(i,j,1) = grid%pd(i,j)
          write(message,*)' loop=',loop,' i=',i,' j=',j,' pdbdy2dtemp1(i,j)=',pdbdy2dtemp1(i,j,1)
          CALL wrf_debug(10,message)
	endif
          END DO
        ENDIF





        IF(IPE==IDE)THEN
          I=MIN(IDE,IPE)

          DO k = kps , MIN(kde,kpe)
          inc_h=mod(jps+1,2)
          DO j = jps+inc_h, MIN(jde,jpe),2
        if (J .ge. 3 .and. J .le. jde-2 .and. mod(J,2) .eq. 1) then
            tbdy3dtemp2(i,j,k) = grid%t(i,j,k)
            qbdy3dtemp2(i,j,k) = grid%q(i,j,k)
            cwmbdy3dtemp2(i,j,k) = grid%cwm(i,j,k)
            q2bdy3dtemp2(i,j,k) = grid%q2(i,j,k)
	endif
          END DO
          END DO

          DO k = kps , MIN(kde,kpe)
          inc_v=mod(jps,2)
          DO j = jps+inc_v, MIN(jde,jpe),2
        if (J .ge. 2 .and. J .le. jde-1 .and. mod(J,2) .eq. 0) then
            ubdy3dtemp2(i,j,k) = grid%u(i,j,k)
            vbdy3dtemp2(i,j,k) = grid%v(i,j,k)
	endif
          END DO
          END DO

          inc_h=mod(jps+1,2)
          DO j = jps+inc_h, MIN(jde,jpe),2
        if (J .ge. 3 .and. J .le. jde-2 .and. mod(J,2) .eq. 1) then
            pdbdy2dtemp2(i,j,1) = grid%pd(i,j)
	endif
          END DO
        ENDIF

      
      
      
      
      


      CALL stuff_bdytend_ijk ( ubdy3dtemp2 , ubdy3dtemp1 , REAL(interval_seconds),&
                                   grid%u_btxs, grid%u_btxe, &
                                   grid%u_btys, grid%u_btye, &
                                   'N',  spec_bdy_width      , &
                                   ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                   ims , ime , jms , jme , kms , kme , &
                                   ips , ipe , jps , jpe , kps , kpe+1 )

      CALL stuff_bdytend_ijk ( vbdy3dtemp2 , vbdy3dtemp1 , REAL(interval_seconds),&
                                   grid%v_btxs, grid%v_btxe, &
                                   grid%v_btys, grid%v_btye, &
                                   'N',  spec_bdy_width      , &
                                   ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                   ims , ime , jms , jme , kms , kme , &
                                   ips , ipe , jps , jpe , kps , kpe+1 )

      CALL stuff_bdytend_ijk ( tbdy3dtemp2 , tbdy3dtemp1 , REAL(interval_seconds),&
                                   grid%t_btxs, grid%t_btxe, &
                                   grid%t_btys, grid%t_btye, &
                                   'N',  spec_bdy_width      , &
                                   ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                   ims , ime , jms , jme , kms , kme , &
                                   ips , ipe , jps , jpe , kps , kpe+1 )

      CALL stuff_bdytend_ijk ( cwmbdy3dtemp2 , cwmbdy3dtemp1 , REAL(interval_seconds),&
                                   grid%cwm_btxs, grid%cwm_btxe, &
                                   grid%cwm_btys, grid%cwm_btye, &
                                   'N',  spec_bdy_width      , &
                                   ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                   ims , ime , jms , jme , kms , kme , &
                                   ips , ipe , jps , jpe , kps , kpe+1 )

      CALL stuff_bdytend_ijk ( qbdy3dtemp2 , qbdy3dtemp1 , REAL(interval_seconds),&
                                   grid%q_btxs, grid%q_btxe, &
                                   grid%q_btys, grid%q_btye, &
                                   'N',  spec_bdy_width      , &
                                   ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                   ims , ime , jms , jme , kms , kme , &
                                   ips , ipe , jps , jpe , kps , kpe+1 )

      CALL stuff_bdytend_ijk ( q2bdy3dtemp2 , q2bdy3dtemp1 , REAL(interval_seconds),&
                                   grid%q2_btxs, grid%q2_btxe, &
                                   grid%q2_btys, grid%q2_btye, &
                                   'N',  spec_bdy_width      , &
                                   ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                   ims , ime , jms , jme , kms , kme , &
                                   ips , ipe , jps , jpe , kps , kpe+1 )

      CALL stuff_bdytend_ijk( pdbdy2dtemp2 , pdbdy2dtemp1, REAL(interval_seconds),&
                                   grid%pd_btxs, grid%pd_btxe, &
                                   grid%pd_btys, grid%pd_btye, &
                                   'M',  spec_bdy_width      , &
                                   ids , ide+1 , jds , jde+1 , 1 , 1 , &
                                   ims , ime   , jms , jme   , 1 , 1 , &
                                   ips , ipe   , jps , jpe   , 1 , 1 )



      
      
      
      
      
      
      
      
      
      
      

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

      
      
      












  
  

      IF     ( loop .LT. time_loop_max ) THEN

         
         
         
         
         




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



         DO j = jps , jpe
            DO i = ips , ipe
               pdbdy2dtemp1(i,j,1) = pdbdy2dtemp2(i,j,1)
	if (J .eq. jpe) write(0,*) 'I,J, PDBDy2dtemp1(i,j,1):' , I,J, PDBDy2dtemp1(i,j,1)
            END DO
         END DO

  
  
  

 CALL stuff_bdy_ijk (ubdy3dtemp1, grid%u_bxs, grid%u_bxe, &
                                  grid%u_bys, grid%u_bye, &
                                  'N', spec_bdy_width  , &
                                  ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                  ims , ime , jms , jme , kms , kme , &
                                  ips , ipe , jps , jpe , kps , kpe+1 )

 CALL stuff_bdy_ijk (vbdy3dtemp1, grid%v_bxs, grid%v_bxe, &
                                  grid%v_bys, grid%v_bye, &
                                  'N', spec_bdy_width  , &
                                  ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                  ims , ime , jms , jme , kms , kme , &
                                  ips , ipe , jps , jpe , kps , kpe+1 )

 CALL stuff_bdy_ijk (tbdy3dtemp1, grid%t_bxs, grid%t_bxe, &
                                  grid%t_bys, grid%t_bye, &
                                  'N', spec_bdy_width  , &
                                  ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                  ims , ime , jms , jme , kms , kme , &
                                  ips , ipe , jps , jpe , kps , kpe+1 )

 CALL stuff_bdy_ijk (cwmbdy3dtemp1, grid%cwm_bxs, grid%cwm_bxe, &
                                  grid%cwm_bys, grid%cwm_bye, &
                                  'N', spec_bdy_width  , &
                                  ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                  ims , ime , jms , jme , kms , kme , &
                                  ips , ipe , jps , jpe , kps , kpe+1 )

 CALL stuff_bdy_ijk (qbdy3dtemp1, grid%q_bxs, grid%q_bxe, &
                                  grid%q_bys, grid%q_bye, &
                                  'N', spec_bdy_width  , &
                                  ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                  ims , ime , jms , jme , kms , kme , &
                                  ips , ipe , jps , jpe , kps , kpe+1 )

 CALL stuff_bdy_ijk (q2bdy3dtemp1, grid%q2_bxs, grid%q2_bxe, &
                                  grid%q2_bys, grid%q2_bye, &
                                  'N', spec_bdy_width  , &
                                  ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                  ims , ime , jms , jme , kms , kme , &
                                  ips , ipe , jps , jpe , kps , kpe+1 )

 CALL stuff_bdy_ijk (pdbdy2dtemp1,grid%pd_bxs, grid%pd_bxe, &
                                  grid%pd_bys, grid%pd_bye, &
                                  'M', spec_bdy_width  , &
                                  ids , ide+1 , jds , jde+1 , 1 , 1 , &
                                  ims , ime , jms , jme , 1 , 1 , &
                                  ips , ipe , jps , jpe , 1 , 1 )

      ELSE IF ( loop .EQ. time_loop_max ) THEN

    

         CALL close_dataset ( id , config_flags , "DATASET=BOUNDARY" )

      END IF

   END IF main_loop_test

END SUBROUTINE assemble_output
