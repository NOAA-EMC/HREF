! This is a program that converts NMM data into WRF input data.
! No boundary data yet.
!
PROGRAM convert_graps

   USE module_machine
   USE module_domain
   USE module_io_domain
   USE module_driver_constants
   USE module_bc
   USE module_configure
   USE module_timing
#ifdef DM_PARALLEL
   USE module_dm
#endif

   IMPLICIT NONE

   REAL    :: time , bdyfrq

   INTEGER :: loop , levels_to_process


   TYPE(domain) , POINTER      :: keep_grid, grid_ptr, null_domain, grid
   TYPE (grid_config_rec_type) :: config_flags
   INTEGER                     :: number_at_same_level

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
   INTEGER :: i , j , k , idts
   INTEGER :: debug_level

   CHARACTER (LEN=80)     :: message

   REAL , DIMENSION(:,:,:) , ALLOCATABLE :: ubdy3dtemp1 , vbdy3dtemp1 , tbdy3dtemp1 , pbdy3dtemp1 , qbdy3dtemp1
   REAL , DIMENSION(:,:  ) , ALLOCATABLE :: mbdy2dtemp1
   REAL , DIMENSION(:,:,:) , ALLOCATABLE :: ubdy3dtemp2 , vbdy3dtemp2 , tbdy3dtemp2 , pbdy3dtemp2 , qbdy3dtemp2
   REAL , DIMENSION(:,:  ) , ALLOCATABLE :: mbdy2dtemp2

   CHARACTER(LEN=24) :: previous_date , this_date , next_date
   CHARACTER(LEN=19) :: start_date_char , end_date_char , current_date_char , next_date_char
   CHARACTER(LEN= 4) :: loop_char

   INTEGER :: start_year , start_month , start_day , start_hour , start_minute , start_second
   INTEGER ::   end_year ,   end_month ,   end_day ,   end_hour ,   end_minute ,   end_second
   INTEGER :: interval_seconds , real_data_init_type
   INTEGER :: time_loop_max , time_loop

   CHARACTER (LEN=80) :: inpname , bdyname

! these are needed on some compilers, eg compaq/alpha, to
! permit pass by reference through the registry generated
! interface to med_read_graps, below
#ifdef DEREF_KLUDGE
   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
#endif

   !  Get the NAMELIST data for input.

   !  Define the name of this program (program_name defined in module_domain)


   program_name = "REAL_EM V1.2 PREPROCESSOR"

#ifdef DM_PARALLEL
   CALL disable_quilting
#endif

   CALL init_modules

#ifdef DM_PARALLEL
   IF ( wrf_dm_on_monitor() ) THEN
     CALL initial_config
     CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
     CALL wrf_dm_bcast_bytes( configbuf, nbytes )
     CALL set_config_as_buffer( configbuf, configbuflen )
   ENDIF
   CALL wrf_dm_initialize
#else
   CALL initial_config
#endif

   CALL get_debug_level ( debug_level )
   CALL set_wrf_debug_level ( debug_level )

   CALL  wrf_message ( program_name )

   !  An available simple timer from the timing module.

   NULLIFY( null_domain )
   CALL alloc_and_configure_domain ( domain_id  = 1           , &
                                     grid       = head_grid   , &
                                     parent     = null_domain , &
                                     kid        = -1            )

   CALL set_scalar_indices_from_config ( head_grid%id , idum1, idum2 )

   grid => head_grid

   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

   IF ( config_flags%dyn_opt .NE. DYN_GRAPS ) THEN
      CALL wrf_error_fatal( "Wrong dyn_opt for GRAPS in namelist.input" )
   ENDIF

print *,'start date=',model_config_rec%start_year(grid%id),model_config_rec%start_month(grid%id),&
model_config_rec%start_day(grid%id),model_config_rec%start_hour(grid%id)
print *,'end   date=',model_config_rec%end_year(grid%id),model_config_rec%end_month(grid%id),&
model_config_rec%end_day(grid%id),model_config_rec%end_hour(grid%id)
print *,'interval  =',model_config_rec%interval_seconds
print *,'init_typ  =',model_config_rec%real_data_init_type

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

   WRITE ( start_date_char , FMT = '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)' ) &
           start_year,start_month,start_day,start_hour,start_minute,start_second
   WRITE (   end_date_char , FMT = '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)' ) &
             end_year,  end_month,  end_day,  end_hour,  end_minute,  end_second

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
   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )


! these are needed on some compilers, eg compaq/alpha, to
! permit pass by reference through the registry generated
! interface to med_read_graps, below
#ifdef DEREF_KLUDGE
   sm31             = grid%sm31
   em31             = grid%em31
   sm32             = grid%sm32
   em32             = grid%em32
   sm33             = grid%sm33
   em33             = grid%em33
write(0,*)'sm31,em31,sm32,em32,sm33,em33',sm31,em31,sm32,em32,sm33,em33
#endif


write(0,*)config_flags%grid_id

   CALL read_graps ( head_grid, config_flags,  &
!
#include "graps_actual_args.inc"
!
                     )

   CALL init_wrfio

   head_grid%input_from_file = .false.
   head_grid%write_metadata = .false.

   CALL construct_filename1( inpname , 'wrfinput' , grid%id , 2 )
   CALL open_w_dataset ( id1, TRIM(inpname) , head_grid , config_flags , output_model_input , "DATASET=INPUT", ierr )
   head_grid%write_metadata = .true.
   IF ( ierr .NE. 0 ) THEN
     CALL wrf_error_fatal( 'real: error opening wrfinput for writing' )
   ENDIF
   CALL calc_current_date ( head_grid%id , 0. )
   current_date = current_date_char
   head_grid%write_metadata = .true.
   CALL output_model_input ( id1, head_grid , config_flags , ierr )
   CALL close_dataset ( id1 , config_flags , "DATASET=INPUT" )


   current_date = current_date_char
   head_grid%write_metadata = .false.
   CALL construct_filename1( bdyname , 'wrfbdy' , grid%id , 2 )
   CALL open_w_dataset ( id, TRIM(bdyname) , head_grid , config_flags , output_boundary , "DATASET=BOUNDARY", ierr )
   head_grid%write_metadata = .true.
   bdyfrq = interval_seconds
write(0,*)'2 bdyfrq: ',bdyfrq
   CALL set_bdyfrq ( grid%id , bdyfrq )
   IF ( ierr .NE. 0 ) THEN
     CALL wrf_error_fatal( 'real: error opening wrfbdy for writing' )
   ENDIF
   CALL calc_current_date ( grid%id , 0. )
   current_date = current_date_char
   CALL output_boundary ( id, head_grid , config_flags , ierr )
   CALL close_dataset ( id , config_flags , "DATASET=BOUNDARY" )



#ifdef DM_PARALLEL
   CALL wrf_dm_shutdown
#endif
   STOP

#if 0
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
   !  Loop over each time period to process.

   DO loop = 1 , time_loop_max

      !  Initialize the mother domain for this time period.

      grid%input_from_file = .false.
      CALL init_domain (  grid )
      CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
   
      IF ( loop .EQ. 1 ) THEN
         CALL init_wrfio
      END IF

      !  There are different things that have to happen during different time loops.
      !  During EVERY time loop, EXCEPT the last, we output the starting values for the
      !  boundary data.  During EVERY time loop, EXCEPT the first, we output the 
      !  boundary tendency.
   
      IF ( loop .EQ. 1 ) THEN

         !  Open the wrfinput file.

         CALL construct_filename1( inpname , 'wrfinput' , grid%id , 2 )
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
   
         print *, ids , ide , jds , jde , kds , kde
         print *, ims , ime , jms , jme , kms , kme

         !  This is the space needed to save the current 3d data for use in computing
         !  the lateral boundary tendencies.

         ALLOCATE ( ubdy3dtemp1(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( vbdy3dtemp1(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( tbdy3dtemp1(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( pbdy3dtemp1(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( qbdy3dtemp1(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( mbdy2dtemp1(ims:ime,        jms:jme) )
         ALLOCATE ( ubdy3dtemp2(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( vbdy3dtemp2(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( tbdy3dtemp2(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( pbdy3dtemp2(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( qbdy3dtemp2(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( mbdy2dtemp2(ims:ime,        jms:jme) )

         !  Actually do work here - output the data!

         CALL calc_current_date ( head_grid%id , 0. )
         head_grid%write_metadata = .true.
         CALL output_model_input ( id1, head_grid , config_flags , ierr )
         CALL close_dataset ( id1 , config_flags , "DATASET=INPUT" )

         !  We need to save the 3d data to compute a difference during the next loop.  Couple the
         !  3d fields with total mu (mub + mu_1) and the stagger-specific map scale factor.
      
         CALL couple ( grid%em_mu_1 , grid%em_mub , ubdy3dtemp1 , grid%em_u_1                 , 'u' , grid%msfu , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ids, ide, jds, jde, kds, kde )
         CALL couple ( grid%em_mu_1 , grid%em_mub , vbdy3dtemp1 , grid%em_v_1                 , 'v' , grid%msfv , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ids, ide, jds, jde, kds, kde )
         CALL couple ( grid%em_mu_1 , grid%em_mub , tbdy3dtemp1 , grid%em_t_1                 , 't' , grid%msft , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ids, ide, jds, jde, kds, kde )
         CALL couple ( grid%em_mu_1 , grid%em_mub , pbdy3dtemp1 , grid%em_ph_1                , 'w' , grid%msft , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ids, ide, jds, jde, kds, kde )
         CALL couple ( grid%em_mu_1 , grid%em_mub , qbdy3dtemp1 , grid%moist_1(:,:,:,P_QV) , 't' , grid%msft , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ids, ide, jds, jde, kds, kde )
         mbdy2dtemp1 = grid%em_mu_1

         !  There are 2 components to the lateral boundaries.  First, there is the starting
         !  point of this time period - just the outer few rows and columns.
   
         CALL stuff_bdy     ( ubdy3dtemp1 , grid%em_u_b     , 'U' , ids , ide , jds , jde , kds , kde , &
                                                                ims , ime , jms , jme , kms , kme ) 
         CALL stuff_bdy     ( vbdy3dtemp1 , grid%em_v_b     , 'V' , ids , ide , jds , jde , kds , kde , &
                                                                ims , ime , jms , jme , kms , kme ) 
         CALL stuff_bdy     ( tbdy3dtemp1 , grid%em_t_b     , 'T' , ids , ide , jds , jde , kds , kde , &
                                                                ims , ime , jms , jme , kms , kme ) 
         CALL stuff_bdy     ( pbdy3dtemp1 , grid%em_ph_b    , 'W' , ids , ide , jds , jde , kds , kde , &
                                                                ims , ime , jms , jme , kms , kme ) 
         CALL stuff_bdy     ( qbdy3dtemp1 , grid%em_rqv_b   , 'T' , ids , ide , jds , jde , kds , kde , &
                                                                ims , ime , jms , jme , kms , kme ) 
         CALL stuff_bdy2    ( mbdy2dtemp1 , grid%em_mu_b    , 'T' , ids , ide , jds , jde , kds , kde , &
                                                                ims , ime , jms , jme , kms , kme ) 
      ELSE IF ( loop .GT. 1 ) THEN
      
         CALL couple ( grid%em_mu_1 , grid%em_mub , ubdy3dtemp2 , grid%em_u_1                 , 'u' , grid%msfu , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ids, ide, jds, jde, kds, kde )
         CALL couple ( grid%em_mu_1 , grid%em_mub , vbdy3dtemp2 , grid%em_v_1                 , 'v' , grid%msfv , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ids, ide, jds, jde, kds, kde )
         CALL couple ( grid%em_mu_1 , grid%em_mub , tbdy3dtemp2 , grid%em_t_1                 , 't' , grid%msft , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ids, ide, jds, jde, kds, kde )
         CALL couple ( grid%em_mu_1 , grid%em_mub , pbdy3dtemp2 , grid%em_ph_1                , 'w' , grid%msft , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ids, ide, jds, jde, kds, kde )
         CALL couple ( grid%em_mu_1 , grid%em_mub , qbdy3dtemp2 , grid%moist_1(:,:,:,P_QV) , 't' , grid%msft , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ids, ide, jds, jde, kds, kde )
         mbdy2dtemp2 = grid%em_mu_1
   
         !  During all of the loops after the first loop, we first compute the boundary
         !  tendencies with the current data values and the previously save information
         !  stored in the *bdy3dtemp1 arrays.

         CALL stuff_bdytend ( ubdy3dtemp2 , ubdy3dtemp1 , REAL(interval_seconds) , grid%em_u_bt  , 'U' , &
                                                               ids , ide , jds , jde , kds , kde , &
                                                               ims , ime , jms , jme , kms , kme ) 
         CALL stuff_bdytend ( vbdy3dtemp2 , vbdy3dtemp1 , REAL(interval_seconds) , grid%em_v_bt  , 'V' , &
                                                               ids , ide , jds , jde , kds , kde , &
                                                               ims , ime , jms , jme , kms , kme ) 
         CALL stuff_bdytend ( tbdy3dtemp2 , tbdy3dtemp1 , REAL(interval_seconds) , grid%em_t_bt  , 'T' , &
                                                               ids , ide , jds , jde , kds , kde , &
                                                               ims , ime , jms , jme , kms , kme ) 
         CALL stuff_bdytend ( pbdy3dtemp2 , pbdy3dtemp1 , REAL(interval_seconds) , grid%em_ph_bt  , 'W' , &
                                                               ids , ide , jds , jde , kds , kde , &
                                                               ims , ime , jms , jme , kms , kme ) 
         CALL stuff_bdytend ( qbdy3dtemp2 , qbdy3dtemp1 , REAL(interval_seconds) , grid%em_rqv_bt , 'T' , &
                                                               ids , ide , jds , jde , kds , kde , &
                                                               ims , ime , jms , jme , kms , kme ) 
         CALL stuff_bdytend2( mbdy2dtemp2 , mbdy2dtemp1 , REAL(interval_seconds) , grid%em_mu_bt  , 'T' , &
                                                               ids , ide , jds , jde , kds , kde , &
                                                               ims , ime , jms , jme , kms , kme ) 
                                                               
         !  Open the boundary file.

         IF ( loop .eq. 2 ) THEN
            CALL construct_filename1( bdyname , 'wrfbdy' , grid%id , 2 )
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

         !  OK, for all of the loops, we output the initialzation data, which would allow us to 
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

            !  We need to save the 3d data to compute a difference during the next loop.  Couple the
            !  3d fields with total mu (mub + mu_1) and the stagger-specific map scale factor.
            !  We load up the boundary data again for use in the next loop.
         
            ubdy3dtemp1 = ubdy3dtemp2
            vbdy3dtemp1 = vbdy3dtemp2
            tbdy3dtemp1 = tbdy3dtemp2
            pbdy3dtemp1 = pbdy3dtemp2
            qbdy3dtemp1 = qbdy3dtemp2
            mbdy2dtemp1 = mbdy2dtemp2

            !  There are 2 components to the lateral boundaries.  First, there is the starting
            !  point of this time period - just the outer few rows and columns.
      
            CALL stuff_bdy     ( ubdy3dtemp1 , grid%em_u_b     , 'U' , ids , ide , jds , jde , kds , kde , &
                                                                   ims , ime , jms , jme , kms , kme ) 
            CALL stuff_bdy     ( vbdy3dtemp1 , grid%em_v_b     , 'V' , ids , ide , jds , jde , kds , kde , &
                                                                   ims , ime , jms , jme , kms , kme ) 
            CALL stuff_bdy     ( tbdy3dtemp1 , grid%em_t_b     , 'T' , ids , ide , jds , jde , kds , kde , &
                                                                   ims , ime , jms , jme , kms , kme ) 
            CALL stuff_bdy     ( pbdy3dtemp1 , grid%em_ph_b    , 'W' , ids , ide , jds , jde , kds , kde , &
                                                                   ims , ime , jms , jme , kms , kme ) 
            CALL stuff_bdy     ( qbdy3dtemp1 , grid%em_rqv_b   , 'T' , ids , ide , jds , jde , kds , kde , &
                                                                   ims , ime , jms , jme , kms , kme ) 
            CALL stuff_bdy2    ( mbdy2dtemp1 , grid%em_mu_b    , 'T' , ids , ide , jds , jde , kds , kde , &
                                                                   ims , ime , jms , jme , kms , kme ) 

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

#ifdef DM_PARALLEL
    CALL wrf_dm_shutdown
#endif
#endif

END PROGRAM convert_graps
