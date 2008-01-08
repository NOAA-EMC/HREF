! create an initial data set for the WRF model based on real data
PROGRAM real_data

   USE module_machine
   USE module_domain
   USE module_initialize
   USE module_io_domain
   USE module_driver_constants
   USE module_bc
   USE module_configure
   USE module_si_io_eh
   USE module_timing
#ifdef DM_PARALLEL
   USE module_dm
#endif

   IMPLICIT NONE

    REAL    :: time , bdyfrq

   INTEGER :: loop , levels_to_process


   TYPE(domain) , POINTER :: keep_grid, grid_ptr, null_domain, grid
   TYPE (grid_config_rec_type)              :: config_flags
   INTEGER                :: number_at_same_level

   INTEGER :: max_dom, domain_id
   INTEGER :: id1 , id , ierr
   INTEGER :: idum1, idum2 
#ifdef DM_PARALLEL
   INTEGER                 :: nbytes
   INTEGER, PARAMETER      :: configbuflen = 2*1024
   INTEGER                 :: configbuf( configbuflen )
   LOGICAL , EXTERNAL      :: wrf_dm_on_monitor
#endif

   INTEGER :: ids , ide , jds , jde , kds , kde
   INTEGER :: ims , ime , jms , jme , kms , kme
   INTEGER :: ips , ipe , jps , jpe , kps , kpe
   INTEGER :: ijds , ijde , spec_bdy_width
   INTEGER :: i , j , k , idts

   CHARACTER (LEN=80)     :: message

   REAL , DIMENSION(:,:,:) , ALLOCATABLE :: ubdy3dtemp
   REAL , DIMENSION(:,:,:) , ALLOCATABLE :: vbdy3dtemp
   REAL , DIMENSION(:,:,:) , ALLOCATABLE :: tbdy3dtemp
   REAL , DIMENSION(:,:,:) , ALLOCATABLE :: rbdy3dtemp
   REAL , DIMENSION(:,:,:) , ALLOCATABLE :: qbdy3dtemp
   REAL , DIMENSION(:,:,:) , ALLOCATABLE :: q3dtemp

   CHARACTER(LEN=24) :: previous_date , this_date , next_date
   CHARACTER(LEN=19) :: start_date_char , end_date_char , current_date_char , next_date_char
   CHARACTER(LEN= 4) :: loop_char

   INTEGER :: start_year , start_month , start_day , start_hour , start_minute , start_second
   INTEGER ::   end_year ,   end_month ,   end_day ,   end_hour ,   end_minute ,   end_second
   INTEGER :: interval_seconds , real_data_init_type
   INTEGER :: time_loop_max , time_loop
   INTEGER :: debug_level

   CHARACTER (LEN=80) :: inpname , bdyname

   !  Get the NAMELIST data for input.

   !  Define the name of this program (program_name defined in module_domain)

   program_name = "REAL_EH V1.3 PREPROCESSOR"

#ifdef DM_PARALLEL
   CALL disable_quilting
#endif

   CALL init_modules

#ifdef DM_PARALLEL
   IF ( wrf_dm_on_monitor() ) THEN
     CALL initial_config
   ENDIF
   CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
   CALL wrf_dm_bcast_bytes( configbuf, nbytes )
   CALL set_config_as_buffer( configbuf, configbuflen )
   CALL wrf_dm_initialize
#else
   CALL initial_config
#endif

   CALL  wrf_message ( program_name )

   CALL get_debug_level ( debug_level )
   CALL set_wrf_debug_level ( debug_level )

   !  An available simple timer from the timing module.

   NULLIFY( null_domain )
   CALL alloc_and_configure_domain ( domain_id  = 1 ,                  &
                                     grid       = head_grid ,          &
                                     parent     = null_domain ,        &
                                     kid        = -1                   )

   CALL set_scalar_indices_from_config ( head_grid%id , idum1, idum2 )
   grid => head_grid

   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

print *,'start date=',model_config_rec%start_year(grid%id),model_config_rec%start_month(grid%id),&
model_config_rec%start_day(grid%id),model_config_rec%start_hour(grid%id)
print *,'end   date=',model_config_rec%end_year(grid%id),model_config_rec%end_month(grid%id),&
model_config_rec%end_day(grid%id),model_config_rec%end_hour(grid%id)
print *,'interval  =',model_config_rec%interval_seconds
print *,'init_typ  =',model_config_rec%real_data_init_type
!  CALL get_start_year  (start_year  )
!  CALL get_start_month (start_month )
!  CALL get_start_day   (start_day   )
!  CALL get_start_hour  (start_hour  )
!  CALL get_start_minute(start_minute)
!  CALL get_start_second(start_second)
!  CALL get_end_year  (end_year  )
!  CALL get_end_month (end_month )
!  CALL get_end_day   (end_day   )
!  CALL get_end_hour  (end_hour  )
!  CALL get_end_minute(end_minute)
!  CALL get_end_second(end_second)
!  CALL get_interval_seconds (interval_seconds)
!  CALL get_real_data_init_type (real_data_init_type)

   !  Boundary width, scalar value.

   spec_bdy_width = model_config_rec%spec_bdy_width

   !  Figure out the starting and ending dates in a character format.

   start_year   = model_config_rec%start_year  (grid%id)
   start_month  = model_config_rec%start_month (grid%id)
   start_day    = model_config_rec%start_day   (grid%id)
   start_hour   = model_config_rec%start_hour  (grid%id)
   start_minute = model_config_rec%start_minute(grid%id)
   start_second = model_config_rec%start_second(grid%id)

     end_year   = model_config_rec%  end_year  (grid%id)
     end_month  = model_config_rec%  end_month (grid%id)
     end_day    = model_config_rec%  end_day   (grid%id)
     end_hour   = model_config_rec%  end_hour  (grid%id)
     end_minute = model_config_rec%  end_minute(grid%id)
     end_second = model_config_rec%  end_second(grid%id)

   interval_seconds    = model_config_rec%interval_seconds
   real_data_init_type = model_config_rec%real_data_init_type
print *,'start date=',start_year,start_month,start_day,start_hour
print *,'end   date=',end_year,end_month,end_day,end_hour
print *,'interval  =',interval_seconds
print *,'init_typ  =',real_data_init_type

   WRITE ( start_date_char , FMT = '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)' ) &
           start_year,start_month,start_day,start_hour,start_minute,start_second
   WRITE (   end_date_char , FMT = '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)' ) &
             end_year,  end_month,  end_day,  end_hour,  end_minute,  end_second
print *,'start date=',start_date_char
print *,'  end date=',  end_date_char

   !  Figure out our loop count for the processing times.

   time_loop = 1
   PRINT '(A,I4,A,A,A)','Time period #',time_loop,' to process = ',start_date_char,'.'
   current_date_char = start_date_char
   loop_count : DO
      CALL geth_newdate ( next_date_char , current_date_char , interval_seconds )
      IF      ( next_date_char .LT. end_date_char ) THEN
         time_loop = time_loop + 1 
         PRINT '(A,I4,A,A,A)','Time period #',time_loop,' to process = ',next_date_char,'.'
         current_date_char = next_date_char
      ELSE IF ( next_date_char .EQ. end_date_char ) THEN
         time_loop = time_loop + 1 
         PRINT '(A,I4,A,A,A)','Time period #',time_loop,' to process = ',next_date_char,'.'
         PRINT '(A,I4,A)','Total analysis times to input = ',time_loop,'.'
         time_loop_max = time_loop
         EXIT loop_count
      ELSE IF ( next_date_char .GT. end_date_char ) THEN
         PRINT '(A,I4,A)','Total analysis times to input = ',time_loop,'.'
         time_loop_max = time_loop
         EXIT loop_count
      END IF
   END DO loop_count

   !  Here we define the initial time to process, for later use by the code.

   current_date_char = start_date_char
   start_date = start_date_char // '.0000'
   current_date = start_date
   bdyfrq = interval_seconds
   CALL set_bdyfrq ( grid%id , bdyfrq )
print *,'interval_seconds=',interval_seconds
  
   !  Loop over each time period to process.

   DO loop = 1 , time_loop_max

      !  Initialize the mother domain for this time period.
print *,'current date = ',current_date

      grid%input_from_file = .false.
      CALL init_domain (  grid )
      CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
   
      IF ( loop .EQ. 1 ) THEN
         CALL init_wrfio
      END IF

      !  There are different things that have to happen during different time loops.
   
      IF ( loop .EQ. 1 ) THEN

         !  Open the wrfinput file.

         CALL construct_filename1 ( inpname , 'wrfinput' , grid%id , 2 )
         CALL open_w_dataset ( id1, TRIM(inpname) , head_grid , config_flags , output_model_input , "DATASET=INPUT", ierr )
         IF ( ierr .NE. 0 ) THEN
           CALL wrf_error_fatal( 'real: error opening wrfinput for writing' )
         ENDIF
   
         !  Various sizes that we need to be concerned about.
   
         ids = grid%sd31 
         ide = grid%ed31
         kds = grid%sd32 
         kde = grid%ed32
         jds = grid%sd33 
         jde = grid%ed33
      
         ims = grid%sm31 
         ime = grid%em31
         kms = grid%sm32 
         kme = grid%em32
         jms = grid%sm33 
         jme = grid%em33

         ips = grid%sp31
         ipe = grid%ep31
         kps = grid%sp32
         kpe = grid%ep32
         jps = grid%sp33
         jpe = grid%ep33

         ijds = MIN ( ids , jds )
         ijde = MAX ( ide , jde )
   
         print *, ids , ide , jds , jde , kds , kde
         print *, ims , ime , jms , jme , kms , kme
         print *, ips , ipe , jps , jpe , kps , kpe
         print *, ijds , ijde

         print *,'spec_bdy_width=',spec_bdy_width

         !  This is the space needed to save the current 3d data for use in computing
         !  the lateral boundary tendencies.

         ALLOCATE ( ubdy3dtemp(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( vbdy3dtemp(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( tbdy3dtemp(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( rbdy3dtemp(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( qbdy3dtemp(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( q3dtemp   (ims:ime,kms:kme,jms:jme) )

         !  Actually do work here - output the data!

         CALL calc_current_date ( grid%id , 0. )
         head_grid%write_metadata = .true.
         CALL output_model_input ( id1, head_grid , config_flags , ierr )
         CALL close_dataset ( id1 , config_flags , "DATASET=INPUT" )

         !  There are 2 components to the lateral boundaries.  First, there is the starting
         !  point of this time period - just the outer few rows and columns.
   
         CALL stuff_bdy     ( grid%eh_ru_1  , grid%eh_ru_b   , 'U' , ijds , ijde , spec_bdy_width      , &
                                                                     ids , ide , jds , jde , kds , kde , &
                                                                     ims , ime , jms , jme , kms , kme , &
                                                                     ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdy     ( grid%eh_rv_1  , grid%eh_rv_b   , 'V' , ijds , ijde , spec_bdy_width      , &
                                                                     ids , ide , jds , jde , kds , kde , &
                                                                     ims , ime , jms , jme , kms , kme , &
                                                                     ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdy     ( grid%eh_rtp_1 , grid%eh_rtp_b  , 'T' , ijds , ijde , spec_bdy_width      , &
                                                                     ids , ide , jds , jde , kds , kde , &
                                                                     ims , ime , jms , jme , kms , kme , &
                                                                     ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdy     ( grid%eh_rrp_1 , grid%eh_rrp_b  , 'T' , ijds , ijde , spec_bdy_width      , &
                                                                     ids , ide , jds , jde , kds , kde , &
                                                                     ims , ime , jms , jme , kms , kme , &
                                                                     ips , ipe , jps , jpe , kps , kpe )

         DO j = jps , MIN(jde-1,jpe)
            DO k = kps , kpe
               DO i = ips , MIN(ide-1,ipe)
                  q3dtemp(i,k,j) = grid%moist_1(i,k,j,P_QV)*grid%eh_rr_1(i,k,j)
               END DO
            END DO
         END DO
         CALL stuff_bdy     ( q3dtemp       , grid%eh_rqv_b  , 'T' , ijds , ijde , spec_bdy_width      , &
                                                                     ids , ide , jds , jde , kds , kde , &
                                                                     ims , ime , jms , jme , kms , kme , &
                                                                     ips , ipe , jps , jpe , kps , kpe )

         !  We need to save the 3d data to compute a difference during the next loop.
      
         DO j = jps , jpe
            DO k = kps , kpe
               DO i = ips , ipe
                  ubdy3dtemp(i,k,j) = grid%eh_ru_1(i,k,j)
                  vbdy3dtemp(i,k,j) = grid%eh_rv_1(i,k,j)
                  tbdy3dtemp(i,k,j) = grid%eh_rtp_1(i,k,j)
                  rbdy3dtemp(i,k,j) = grid%eh_rrp_1(i,k,j)
                  qbdy3dtemp(i,k,j) = grid%moist_1(i,k,j,P_QV) * grid%eh_rr_1(i,k,j)
               END DO
            END DO
         END DO
      
      ELSE IF ( loop .GT. 1 ) THEN
   
         !  During all of the loops after the first loop, we first compute the boundary
         !  tendencies with the current data values and the previously save information
         !  stored in the *bdy3dtemp arrays.

         CALL stuff_bdytend ( grid%eh_ru_1  , ubdy3dtemp , REAL(interval_seconds) , grid%eh_ru_bt  , 'U' , &
                                                               ijds , ijde , spec_bdy_width      , &
                                                               ids , ide , jds , jde , kds , kde , &
                                                               ims , ime , jms , jme , kms , kme , &
                                                               ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdytend ( grid%eh_rv_1  , vbdy3dtemp , REAL(interval_seconds) , grid%eh_rv_bt  , 'V' , &
                                                               ijds , ijde , spec_bdy_width      , &
                                                               ids , ide , jds , jde , kds , kde , &
                                                               ims , ime , jms , jme , kms , kme , &
                                                               ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdytend ( grid%eh_rtp_1 , tbdy3dtemp , REAL(interval_seconds) , grid%eh_rtp_bt , 'T' , &
                                                               ijds , ijde , spec_bdy_width      , &
                                                               ids , ide , jds , jde , kds , kde , &
                                                               ims , ime , jms , jme , kms , kme , &
                                                               ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdytend ( grid%eh_rrp_1 , rbdy3dtemp , REAL(interval_seconds) , grid%eh_rrp_bt , 'T' , &
                                                               ijds , ijde , spec_bdy_width      , &
                                                               ids , ide , jds , jde , kds , kde , &
                                                               ims , ime , jms , jme , kms , kme , &
                                                               ips , ipe , jps , jpe , kps , kpe )
         DO j = jps , MIN(jde-1,jpe)
            DO k = kps , kpe
               DO i = ips , MIN(ide-1,ipe)
                  q3dtemp(i,k,j) = grid%moist_1(i,k,j,P_QV)*grid%eh_rr_1(i,k,j)
               END DO
            END DO
         END DO
         CALL stuff_bdytend ( q3dtemp       , qbdy3dtemp , REAL(interval_seconds) , grid%eh_rqv_bt , 'T' , &
                                                               ijds , ijde , spec_bdy_width      , &
                                                               ids , ide , jds , jde , kds , kde , &
                                                               ims , ime , jms , jme , kms , kme , &
                                                               ips , ipe , jps , jpe , kps , kpe )

         !  Open the boundary file.

         IF ( loop .eq. 2 ) THEN
            CALL construct_filename1 ( bdyname , 'wrfbdy' , grid%id , 2 )
            CALL open_w_dataset ( id, TRIM(bdyname) , head_grid , config_flags , output_boundary , "DATASET=BOUNDARY", ierr )
            IF ( ierr .NE. 0 ) THEN
                  CALL wrf_error_fatal( 'real: error opening wrfbdy for writing' )
            ENDIF
            head_grid%write_metadata = .true.
         ELSE
            head_grid%write_metadata = .false.
         END IF

         !  Both pieces of the boundary data are now available to be written.

         CALL output_boundary ( id, head_grid , config_flags , ierr )

         !  OK, for all of the loops, we output the initialization data, which would allow us to 
         !  start the model at any of the available analysis time periods.

         WRITE ( loop_char , FMT = '(I4.4)' ) loop
!        CALL open_w_dataset ( id1, 'wrfinput'//loop_char , head_grid , config_flags , output_model_input , "DATASET=INPUT", ierr )
!        IF ( ierr .NE. 0 ) THEN
!          CALL wrf_error_fatal( 'real: error opening wrfinput'//loop_char//' for writing' )
!        ENDIF
         head_grid%write_metadata = .true.

         CALL calc_current_date ( grid%id , 0. )
         head_grid%write_metadata = .true.
!        CALL output_model_input ( id1, head_grid , config_flags , ierr )
!        CALL close_dataset ( id1 , config_flags , "DATASET=INPUT" )

         !  Is this or is this not the last time time?  We can remove some unnecessary
         !  stores if it is not.

         IF     ( loop .LT. time_loop_max ) THEN

            !  Here we load up the boundary data again for use in the next loop.
   
            CALL stuff_bdy     ( grid%eh_ru_1  , grid%eh_ru_b   , 'U' , ijds , ijde , spec_bdy_width      , &
                                                                        ids , ide , jds , jde , kds , kde , &
                                                                        ims , ime , jms , jme , kms , kme , &
                                                                        ips , ipe , jps , jpe , kps , kpe )
            CALL stuff_bdy     ( grid%eh_rv_1  , grid%eh_rv_b   , 'V' , ijds , ijde , spec_bdy_width      , &
                                                                        ids , ide , jds , jde , kds , kde , &
                                                                        ims , ime , jms , jme , kms , kme , &
                                                                        ips , ipe , jps , jpe , kps , kpe )
            CALL stuff_bdy     ( grid%eh_rtp_1 , grid%eh_rtp_b  , 'T' , ijds , ijde , spec_bdy_width      , &
                                                                        ids , ide , jds , jde , kds , kde , &
                                                                        ims , ime , jms , jme , kms , kme , &
                                                                        ips , ipe , jps , jpe , kps , kpe )
            CALL stuff_bdy     ( grid%eh_rrp_1 , grid%eh_rrp_b  , 'T' , ijds , ijde , spec_bdy_width      , &
                                                                        ids , ide , jds , jde , kds , kde , &
                                                                        ims , ime , jms , jme , kms , kme , &
                                                                        ips , ipe , jps , jpe , kps , kpe )
            DO j = jps , MIN(jde-1,jpe)
               DO k = kps , kpe
                  DO i = ips , MIN(ide-1,ipe)
                     q3dtemp(i,k,j) = grid%moist_1(i,k,j,P_QV)*grid%eh_rr_1(i,k,j)
                  END DO
               END DO
            END DO
            CALL stuff_bdy     ( q3dtemp       , grid%eh_rqv_b  , 'T' , ijds , ijde , spec_bdy_width      , &
                                                                        ids , ide , jds , jde , kds , kde , &
                                                                        ims , ime , jms , jme , kms , kme , &
                                                                        ips , ipe , jps , jpe , kps , kpe )
   
            !  We need to save the 3d data to compute a difference during the next loop.
         
            DO j = jps , jpe
               DO k = kps , kpe
                  DO i = ips , ipe
                     ubdy3dtemp(i,k,j) = grid%eh_ru_1(i,k,j)
                     vbdy3dtemp(i,k,j) = grid%eh_rv_1(i,k,j)
                     tbdy3dtemp(i,k,j) = grid%eh_rtp_1(i,k,j)
                     rbdy3dtemp(i,k,j) = grid%eh_rrp_1(i,k,j)
                     qbdy3dtemp(i,k,j) = grid%moist_1(i,k,j,P_QV) * grid%eh_rr_1(i,k,j)
                  END DO
               END DO
            END DO
          
         ELSE IF ( loop .EQ. time_loop_max ) THEN

            !  If this is the last time through here, we need to close the files.

            CALL close_dataset ( id , config_flags , "DATASET=BOUNDARY" )

         END IF

      END IF

      !  Process which time now?

      current_date_char = current_date(1:19)
      CALL geth_newdate ( next_date_char , current_date_char , interval_seconds )
      start_date = next_date_char // '.0000'

   END DO

   CALL       wrf_debug (   0 , 'wrf: SUCCESS COMPLETE REAL_EH INIT' )

#ifdef DM_PARALLEL
    CALL wrf_dm_shutdown
#endif

END PROGRAM real_data
