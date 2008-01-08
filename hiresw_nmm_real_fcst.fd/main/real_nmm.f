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
   USE esmf_mod

   IMPLICIT NONE

   REAL    :: time , bdyfrq

   INTEGER :: loop , levels_to_process , debug_level


   TYPE(domain) , POINTER :: null_domain
   TYPE(domain) , POINTER :: grid
   TYPE (grid_config_rec_type)              :: config_flags
   INTEGER                :: number_at_same_level

   INTEGER :: max_dom, domain_id
   INTEGER :: idum1, idum2 

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
   TYPE(ESMF_TimeInterval) :: time_interval
   REAL    :: t1,t2

   INTERFACE
     SUBROUTINE Set_Timekeeping( grid )
      USE module_domain
      TYPE(domain), POINTER :: grid
     END SUBROUTINE Set_Timekeeping
   END INTERFACE


   !  Define the name of this program (program_name defined in module_domain)

   program_name = "REAL_NMM V1.4 PREPROCESSOR"


!	call start()

   !  Initialize the modules used by the WRF system.  
   !  Many of the CALLs made from the
   !  init_modules routine are NO-OPs.  Typical initializations 
   !  are: the size of a
   !  REAL, setting the file handles to a pre-use value, defining moisture and
   !  chemistry indices, etc.

   CALL       wrf_debug ( 100 , 'real_nmm: calling init_modules ' )

   CALL init_modules
	write(6,*) 'return init_modules'

   !  The configuration switches mostly come from the NAMELIST input.

   CALL initial_config


   CALL get_debug_level ( debug_level )
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
   CALL Set_Timekeeping ( grid )
   CALL ESMF_TimeIntervalSet ( time_interval ,  &
             S=model_config_rec%interval_seconds,rc=rc )
   CALL ESMF_ClockSetTimeStep ( grid%domain_clock , time_interval , rc=rc )


   CALL wrf_debug ( 100 , 'real_nmm: calling set_scalar_indices_from_config ' )
   CALL set_scalar_indices_from_config ( grid%id , idum1, idum2 )

   CALL     wrf_debug ( 100 , 'real_nmm: calling model_to_grid_config_rec ' )

   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

	write(6,*) 'after model_to_grid_config_rec, e_we, e_sn are: ', &
                    config_flags%e_we, config_flags%e_sn

   !  Initialize the WRF IO: open files, init file handles, etc.

   CALL       wrf_debug ( 100 , 'real_nmm: calling init_wrfio' )
   CALL init_wrfio

!  Some of the configuration values may have been modified from the initial READ
!  of the NAMELIST, so we re-broadcast the configuration records.


   !   No looping in this layer.  

   CALL med_sidata_input ( grid , config_flags )

   !  We are done.

   CALL       wrf_debug (   0 , 'real_nmm: SUCCESS COMPLETE REAL_EM INIT' )

	write(6,*) 'to wrf_dm_shutdown call'

	write(6,*) 'done done'

!	call summary()

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

   USE module_date_time

   IMPLICIT NONE


  ! Interface 
   INTERFACE
     SUBROUTINE start_domain ( grid )  ! comes from module_start 
                                       ! in appropriate dyn_ directory
       USE module_domain
       TYPE (domain) grid
     END SUBROUTINE start_domain
   END INTERFACE

  ! Arguments
   TYPE(domain)                :: grid
   TYPE (grid_config_rec_type) :: config_flags
  ! Local
   INTEGER                :: time_step_begin_restart
   INTEGER                :: idsi , ierr , myproc
   CHARACTER (LEN=80)      :: si_inpname
   CHARACTER (LEN=80)      :: message

   CHARACTER(LEN=19) :: start_date_char , end_date_char , &
                        current_date_char , next_date_char

   INTEGER :: time_loop_max , loop
   INTEGER :: julyr , julday , LEN

   REAL :: gmt
   REAL :: t1,t2

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

	write(6,*) 'return si_start_and_end'

   !  Here we define the initial time to process, for later use by the code.

   current_date_char = start_date_char
!   start_date = start_date_char // .0000
   start_date = start_date_char 
   current_date = start_date

   CALL set_bdyfrq ( grid%id , REAL(model_config_rec%interval_seconds) )

	write(6,*) 'past set_bdyfrq'

   !  Loop over each time period to process.

	write(6,*) 'time_loop_max: ', time_loop_max
   DO loop = 1 , time_loop_max

	write(6,*) 'loop ', loop

      print *,'-----------------------------------------------------------'
      print *,' '
      print '(A,A,A,I2,A,I2)' , ' Current date being processed: ', &
        current_date, ', which is loop #',loop,' out of ',time_loop_max

      !  After current_date has been set, fill in the julgmt stuff.

      CALL geth_julgmt ( config_flags%julyr , config_flags%julday , &
                                              config_flags%gmt )

	write(6,*) 'config_flags%julday: ', config_flags%julday
	write(6,*) 'config_flags%gmt: ', config_flags%gmt

      !  Now that the specific Julian info is available, 
      !  save these in the model config record.

      CALL set_gmt (grid%id, config_flags%gmt)
      CALL set_julyr (grid%id, config_flags%julyr)
      CALL set_julday (grid%id, config_flags%julday)

	write(6,*) 'past set_julday, open the file'
      !  Open the wrfinput file.

	write(6,*) 'grid%dyn_opt= ', grid%dyn_opt
	write(6,*) 'dyn_nmm= ', dyn_nmm
      IF ( grid%dyn_opt .EQ. dyn_em ) THEN
         CALL  wrf_debug ( 100 , &
   'med_sidata_input: calling open_r_dataset for wrf_real_input_em')

         CALL construct_filename2( si_inpname , 'wrf_real_input_em' , &
                                        grid%id , 2 , current_date_char )
      ELSE IF ( grid%dyn_opt .EQ. dyn_eh ) THEN
         CALL  wrf_debug ( 100 , &
   'med_sidata_input: calling open_r_dataset for wrf_real_input_eh' )

         CALL construct_filename2( si_inpname , 'wrf_real_input_eh' , &
                                         grid%id , 2 , current_date_char )
      ELSE IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
         CALL  wrf_debug ( 100 , &
   'med_sidata_input: calling open_r_dataset for real_input_nm' )
	current_date_char(11:11)='_'

	write(6,*) 'skipping construct_filename2...not needed'
!	write(6,*) construct_filename2
!         CALL construct_filename2( si_inpname , real_input_nm , &
!                                         grid%id , 2 , current_date_char )
	current_date_char(11:11)='T'
	ELSE
	write(6,*) 'dont know what to do'
	write(6,*) 'dont know about this dyn_opt'
	write(6,*) 'grid%dyn_opt= ', grid%dyn_opt
        CALL wrf_error_fatal('real: error cant handle this grid%dyn_opt' )
      END IF

!	write(6,*) trim(si_inpname):  TRIM(si_inpname)

!        CALL open_r_dataset ( idsi, TRIM(si_inpname) , &
!                              grid , config_flags , "DATASET=INPUT", ierr )
!	write(6,*) ierr from open: , ierr

!      IF ( ( ierr .NE. 0 ) .AND. ( grid%dyn_opt .EQ. dyn_em ) ) THEN
!        CALL wrf_error_fatal(real: error opening wrf_real_input_em for read )
!      ELSE IF ( ( ierr .NE. 0 ) .AND. ( grid%dyn_opt .EQ. dyn_eh ) ) THEN
!        CALL wrf_error_fatal(real: error opening wrf_real_input_eh for read )
!      ENDIF

      !  Input data.

!      CALL wrf_debug (100, med_sidata_input: call input_aux_model_input1_wrf)

!!!
!!!	input from netCDF SI output.  Adapt as needed
!!!

!	write(6,*) ide,ite: , ide,ite
!	write(6,*) jde,jte: , jde,jte

!===========================================================================
!  Dave: All variables are read in from SI
!===========================================================================

	grid%nmm_islope=-99

!!!      CALL input_aux_model_input1_wrf ( idsi ,   grid , config_flags , ierr )

!!!
!!!

      !  Possible optional SI input.  This sets flags used by init_domain.

!!!
!!!	anything important input through "optional" SI input?
!!!

      IF ( loop .EQ. 1 ) THEN
! CALL  wrf_debug (100, med_sidata_input: call init_module_optional_si_input )
!         CALL init_module_optional_si_input ( grid , config_flags )
      END IF

!      CALL  wrf_debug ( 100 , med_sidata_input: calling optional_si_input )
!	write(6,*) call optional_si_input
!      CALL  optional_si_input ( grid , idsi )
!	write(6,*) return optional_si_input

      !  Initialize the mother domain for this time period with input data.

      CALL       wrf_debug ( 100 , 'med_sidata_input: calling init_domain' )
      grid%input_from_file = .true.

!!!
!!! init_domain will call init_domain_nmm in module_intialize_real.  
!!! Fills a bunch of NHB-style (static) variables
!!! 
!!! in this version also calls read_si
!!!

	write(0,*) 'call init_domain, loop: ', loop
        CALL init_domain (  grid )
	write(0,*) 'return init_domain, loop: ', loop

      CALL model_to_grid_config_rec ( grid%id, model_config_rec, config_flags )

      !  Close this file that is output from the SI and input to this pre-proc.

      CALL       wrf_debug ( 100 , 'med_sidata_input: back from init_domain' )

!      CALL close_dataset ( idsi , config_flags , "DATASET=INPUT" )

!!! not sure about this, but doesnt seem like needs to be called each time
	if (loop .eq. 1) then
	write(6,*) 'call start_domain'
	write(0,*) 'call start_domain'
        CALL start_domain ( grid )
	write(6,*) 'return start_domain'
	write(0,*) 'call start_domain'
	endif

	write(6,*) 'call assemble_output, PD(1,1) ', grid%nmm_pd(1,1)
	write(0,*) 'call assemble_output, loop, PD(1,1) ',loop, grid%nmm_pd(1,1)
        CALL assemble_output ( grid , config_flags , loop , time_loop_max )
	write(6,*) 'return assemble_output'
	write(0,*) 'return assemble_output , loop' , loop

      !  Here we define the next time that we are going to process.

      CALL geth_newdate ( current_date_char , start_date_char , &
                          loop * model_config_rec%interval_seconds )
      current_date =  current_date_char // '.0000'

	write(6,*) 'current_date= ', current_date

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

   CHARACTER(LEN=19) :: current_date_char , start_date_char , &
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
	write(6,*) 'done in si_start_and_end'
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
   INTEGER :: i , j , k , idts

   INTEGER :: id1 , interval_seconds , ierr, rc
   INTEGER , SAVE :: id 
   CHARACTER (LEN=80) :: inpname , bdyname
   CHARACTER(LEN= 4) :: loop_char
character *19 :: temp19
character *24 :: temp24 , temp24b

   REAL, DIMENSION(:,:,:), ALLOCATABLE, SAVE :: ubdy3dtemp1 , vbdy3dtemp1 ,&
                                                tbdy3dtemp1 , &
				                cwmbdy3dtemp1 , qbdy3dtemp1,&
                                                q2bdy3dtemp1
   REAL, DIMENSION(:,:  ), ALLOCATABLE, SAVE :: pdbdy2dtemp1
   REAL, DIMENSION(:,:,:), ALLOCATABLE, SAVE :: ubdy3dtemp2 , vbdy3dtemp2 , &
                                                tbdy3dtemp2 , & 
                                                cwmbdy3dtemp2 , qbdy3dtemp2, &
                                                q2bdy3dtemp2
   REAL , DIMENSION(:,:  ) , ALLOCATABLE , SAVE :: pdbdy2dtemp2
   REAL :: t1,t2

   !  Various sizes that we need to be concerned about.

   ids = grid%sd31
   ide = grid%ed31-1 ! 030730tst
   kds = grid%sd32
   kde = grid%ed32-1 ! 030730tst
   jds = grid%sd33
   jde = grid%ed33-1 ! 030730tst

   ims = grid%sm31
   ime = grid%em31
   kms = grid%sm32
   kme = grid%em32
   jms = grid%sm33
   jme = grid%em33

   ips = grid%sp31
   ipe = grid%ep31-1 ! 030730tst
   kps = grid%sp32
   kpe = grid%ep32-1 ! 030730tst
   jps = grid%sp33
   jpe = grid%ep33-1 ! 030730tst

	write(6,*) 'assemble output (ids,ide): ', ids,ide
	write(6,*) 'assemble output (ims,ime): ', ims,ime
	write(6,*) 'assemble output (ips,ipe): ', ips,ipe

	write(6,*) 'assemble output (jds,jde): ', jds,jde
	write(6,*) 'assemble output (jms,jme): ', jms,jme
	write(6,*) 'assemble output (jps,jpe): ', jps,jpe

	write(6,*) 'assemble output (kds,kde): ', kds,kde
	write(6,*) 'assemble output (kms,kme): ', kms,kme
	write(6,*) 'assemble output (kps,kpe): ', kps,kpe



	write(6,*) 'NMM_TSK in assemble_output '
	do J=MIN(JDE,JPE),1,-JPE/20
	write(6,633) (grid%nmm_nmm_tsk(I,J),I=1,min(IDE,IPE),IPE/12)
	enddo
  633	format(15(f4.0,1x))
	
   ijds = MIN ( ids , jds )
!mptest030805   ijde = MAX ( ide , jde )
   ijde = MAX ( ide , jde ) + 1   ! to make stuff_bdy dimensions consistent with alloc

   !  Boundary width, scalar value.

   spec_bdy_width = model_config_rec%spec_bdy_width
   interval_seconds = model_config_rec%interval_seconds

	write(0,*) 'in assemble_output, entering loop if test'

   IF ( loop .EQ. 1 ) THEN

   !  This is the space needed to save the current 3d data for use in computing
   !  the lateral boundary tendencies.

	write(6,*) 'allocating 3d arrays passed into stuff_bdy with vert lims: ', &
                  kms, kme
      ALLOCATE ( ubdy3dtemp1(ims:ime,kms:kme,jms:jme) )
      ALLOCATE ( vbdy3dtemp1(ims:ime,kms:kme,jms:jme) )
      ALLOCATE ( tbdy3dtemp1(ims:ime,kms:kme,jms:jme) )
      ALLOCATE ( qbdy3dtemp1(ims:ime,kms:kme,jms:jme) )
      ALLOCATE ( cwmbdy3dtemp1(ims:ime,kms:kme,jms:jme) )
      ALLOCATE ( q2bdy3dtemp1(ims:ime,kms:kme,jms:jme) )
      ALLOCATE ( pdbdy2dtemp1(ims:ime,        jms:jme) )

	ubdy3dtemp1=0.
	vbdy3dtemp1=0.
	tbdy3dtemp1=0.
	qbdy3dtemp1=0.
	cwmbdy3dtemp1=0.
	q2bdy3dtemp1=0.
	pdbdy2dtemp1=0.

      ALLOCATE ( ubdy3dtemp2(ims:ime,kms:kme,jms:jme) )
      ALLOCATE ( vbdy3dtemp2(ims:ime,kms:kme,jms:jme) )
      ALLOCATE ( tbdy3dtemp2(ims:ime,kms:kme,jms:jme) )
      ALLOCATE ( qbdy3dtemp2(ims:ime,kms:kme,jms:jme) )
      ALLOCATE ( cwmbdy3dtemp2(ims:ime,kms:kme,jms:jme) )
      ALLOCATE ( q2bdy3dtemp2(ims:ime,kms:kme,jms:jme) )
      ALLOCATE ( pdbdy2dtemp2(ims:ime,        jms:jme) )

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
      CALL wrf_error_fatal( 'real: error opening wrfinput for writing' )
      ENDIF

!     CALL calc_current_date ( grid%id , 0. )
      grid%write_metadata = .true.

	write(0,*) 'making call to output_model_input'

      CALL output_model_input ( id1, grid , config_flags , ierr )
	write(0,*) 'ierr from output_model_input: ', ierr

      CALL close_dataset ( id1 , config_flags , "DATASET=INPUT" )

      !  We need to save the 3d data to compute a 
      !  difference during the next loop. 

   write(6,*) 'I,J,K lims: ', MIN(ide,ipe), MIN(jde,jpe), MIN(kde,kpe)


!!    Southern boundary
	J=1
        DO k = kps , MIN(kde,kpe)
          DO i = ips , MIN(ide,ipe)
            ubdy3dtemp1(i,k,j) = grid%nmm_u(i,k,j)
            vbdy3dtemp1(i,k,j) = grid%nmm_v(i,k,j)
            tbdy3dtemp1(i,k,j) = grid%nmm_t(i,k,j)
            qbdy3dtemp1(i,k,j) = grid%nmm_q(i,k,j)
            cwmbdy3dtemp1(i,k,j) = grid%nmm_cwm(i,k,j)
            q2bdy3dtemp1(i,k,j) = grid%nmm_q2(i,k,j)
          END DO
        END DO

         DO i = ips , MIN(ide,ipe)
            pdbdy2dtemp1(i,j) = grid%nmm_pd(i,j)

	if (i .eq. ips) write(0,*) 'pdbdy2dtemp1(1,1):: ', pdbdy2dtemp1(i,j)

         END DO

!!     Northern boundary
        J=MIN(JDE,JPE)

	write(6,*) 'specifying N boundary as J= ', J

        DO k = kps , MIN(kde,kpe)
          DO i = ips , MIN(ide,ipe)
            ubdy3dtemp1(i,k,j) = grid%nmm_u(i,k,j)
            vbdy3dtemp1(i,k,j) = grid%nmm_v(i,k,j)
            tbdy3dtemp1(i,k,j) = grid%nmm_t(i,k,j)
            qbdy3dtemp1(i,k,j) = grid%nmm_q(i,k,j)
            cwmbdy3dtemp1(i,k,j) = grid%nmm_cwm(i,k,j)
            q2bdy3dtemp1(i,k,j) = grid%nmm_q2(i,k,j)
	if (k .eq. kps+1 .and. i .eq. ips+1) then
	write(6,*) 'N boundary temp1 fields (u,v,t,q,cwm,q2): ', &
          ubdy3dtemp1(i,k,j),vbdy3dtemp1(i,k,j),tbdy3dtemp1(i,k,j),& 
          qbdy3dtemp1(i,k,j), cwmbdy3dtemp1(i,k,j),q2bdy3dtemp1(i,k,j)
	endif
          END DO
        END DO

         DO i = ips , MIN(ide,ipe)
            pdbdy2dtemp1(i,j) = grid%nmm_pd(i,j)
         END DO


!!     Western boundary
	I=1
        DO k = kps , MIN(kde,kpe)
          DO j = jps+2, jde-2, 2
            tbdy3dtemp1(i,k,j) = grid%nmm_t(i,k,j)
            qbdy3dtemp1(i,k,j) = grid%nmm_q(i,k,j)
            cwmbdy3dtemp1(i,k,j) = grid%nmm_cwm(i,k,j)
            q2bdy3dtemp1(i,k,j) = grid%nmm_q2(i,k,j)
          END DO
        END DO

        DO k = kps , MIN(kde,kpe)
          DO j = jps+1, jde-1, 2
            ubdy3dtemp1(i,k,j) = grid%nmm_u(i,k,j)
            vbdy3dtemp1(i,k,j) = grid%nmm_v(i,k,j)
          END DO
        END DO

        DO j = jps+2,jde-2,2
            pdbdy2dtemp1(i,j) = grid%nmm_pd(i,j)
        END DO

!!     Eastern boundary
	I=MIN(IDE,IPE)

        DO k = kps , MIN(kde,kpe)
          DO j = jps+2, jde-2, 2
            tbdy3dtemp1(i,k,j) = grid%nmm_t(i,k,j)
            qbdy3dtemp1(i,k,j) = grid%nmm_q(i,k,j)
            cwmbdy3dtemp1(i,k,j) = grid%nmm_cwm(i,k,j)
            q2bdy3dtemp1(i,k,j) = grid%nmm_q2(i,k,j)
          END DO
        END DO

        DO k = kps , MIN(kde,kpe)
          DO j = jps+1, jde-1, 2
            ubdy3dtemp1(i,k,j) = grid%nmm_u(i,k,j)
            vbdy3dtemp1(i,k,j) = grid%nmm_v(i,k,j)
          END DO
        END DO

        DO j = jps+2,jde-2,2
            pdbdy2dtemp1(i,j) = grid%nmm_pd(i,j)
        END DO


      !  There are 2 components to the lateral boundaries.  
      !  First, there is the starting
      !  point of this time period - just the outer few rows and columns.


	write(6,*) 'call stuff_bdy for U'
 CALL stuff_bdy (ubdy3dtemp1, grid%nmm_u_b, 'N', ijds, ijde, spec_bdy_width  , &
                                        ids , ide , jds , jde , kds , kde+1 , &
                                        ims , ime , jms , jme , kms , kme , &
                                        ips , ipe , jps , jpe , kps , kpe+1 )
	write(6,*) 'call stuff_bdy for V'
 CALL stuff_bdy ( vbdy3dtemp1, grid%nmm_v_b, 'N', ijds, ijde, spec_bdy_width, &
                                        ids , ide , jds , jde , kds , kde+1 , &
                                        ims , ime , jms , jme , kms , kme , &
                                        ips , ipe , jps , jpe , kps , kpe+1 )
	write(6,*) 'call stuff_bdy for T'

	
	write(6,*) 'size (dim 1) nmm_t_b: ', size(grid%nmm_t_b, dim=1)
	write(6,*) 'size (dim 2) nmm_t_b: ', size(grid%nmm_t_b, dim=2)
	write(6,*) 'size (dim 3) nmm_t_b: ', size(grid%nmm_t_b, dim=3)
	write(6,*) 'size (dim 4) nmm_t_b: ', size(grid%nmm_t_b, dim=4)

 CALL stuff_bdy ( tbdy3dtemp1, grid%nmm_t_b, 'N', ijds, ijde, spec_bdy_width, &
                                        ids , ide , jds , jde , kds , kde+1 , &
                                        ims , ime , jms , jme , kms , kme , &
                                        ips , ipe , jps , jpe , kps , kpe+1 )

 CALL stuff_bdy ( cwmbdy3dtemp1,grid%nmm_cwm_b,'N',ijds,ijde, spec_bdy_width, &
                                        ids , ide , jds , jde , kds , kde+1 , &
                                        ims , ime , jms , jme , kms , kme , &
                                        ips , ipe , jps , jpe , kps , kpe+1 )

 CALL stuff_bdy ( qbdy3dtemp1, grid%nmm_q_b, 'N', ijds, ijde, spec_bdy_width, &
                                        ids , ide , jds , jde , kds , kde+1 , &
                                        ims , ime , jms , jme , kms , kme , &
                                        ips , ipe , jps , jpe , kps , kpe+1 )

 CALL stuff_bdy ( q2bdy3dtemp1,grid%nmm_q2_b,'N', ijds, ijde, spec_bdy_width, &
                                        ids , ide , jds , jde , kds , kde+1 , &
                                        ims , ime , jms , jme , kms , kme , &
                                        ips , ipe , jps , jpe , kps , kpe+1 )

	write(6,*) 'stuff pdbdy2dtemp1... '
 CALL stuff_bdy2 ( pdbdy2dtemp1,grid%nmm_pd_b,'N', ijds,ijde, spec_bdy_width, &
                                        ids , ide , jds , jde , 1 , 1 , &
                                        ims , ime , jms , jme , 1 , 1 , &
                                        ips , ipe , jps , jpe , 1 , 1 )
	write(6,*) 'grid%nmm_pd_b(1,1,1,1): ', grid%nmm_pd_b(1,1,1,1)

   ELSE IF ( loop .GT. 1 ) THEN

	write(0,*) 'loop .gt. 1'

      !  Open the boundary file.

      IF ( loop .eq. 2 ) THEN


	write(6,*) '2nd loop...opening wrfbdy and writing'
	write(0,*) '2nd loop...opening wrfbdy and writing'
	write(6,*) 'config_flags%gmt: ', config_flags%gmt

         CALL construct_filename1( bdyname , 'wrfbdy' , grid%id , 2 )
	write(0,*) 'call open_w_dataset, id, grid%id: ', id, grid%id
      CALL open_w_dataset ( id, TRIM(bdyname) , grid , config_flags , &
                          output_boundary , "DATASET=BOUNDARY", ierr )
	write(0,*) 'return open_w_dataset for wrfbdy'


         IF ( ierr .NE. 0 ) THEN
               CALL wrf_error_fatal( 'real: error opening wrfbdy for writing' )
         ENDIF
         grid%write_metadata = .true.
      ELSE
! whats this do?
         grid%write_metadata = .true.
         grid%write_metadata = .false.
         CALL ESMF_ClockAdvance( grid%domain_clock, rc=rc )
      END IF


!!    Southern boundary
	J=1
        DO k = kps , MIN(kde,kpe)
          DO i = ips , MIN(ide,ipe)
            ubdy3dtemp2(i,k,j) = grid%nmm_u(i,k,j)
            vbdy3dtemp2(i,k,j) = grid%nmm_v(i,k,j)
            tbdy3dtemp2(i,k,j) = grid%nmm_t(i,k,j)
            qbdy3dtemp2(i,k,j) = grid%nmm_q(i,k,j)
            cwmbdy3dtemp2(i,k,j) = grid%nmm_cwm(i,k,j)
            q2bdy3dtemp2(i,k,j) = grid%nmm_q2(i,k,j)
          END DO
        END DO

         DO i = ips , MIN(ide,ipe)
            pdbdy2dtemp2(i,j) = grid%nmm_pd(i,j)

	if (I .eq. ips) write(0,*) 'pdbdy2dtemp2(1,1): ', pdbdy2dtemp2(i,j)
         END DO

!!     Northern boundary
        J=MIN(JDE,JPE)
        DO k = kps , MIN(kde,kpe)
          DO i = ips , MIN(ide,ipe)
            ubdy3dtemp2(i,k,j) = grid%nmm_u(i,k,j)
            vbdy3dtemp2(i,k,j) = grid%nmm_v(i,k,j)
            tbdy3dtemp2(i,k,j) = grid%nmm_t(i,k,j)
            qbdy3dtemp2(i,k,j) = grid%nmm_q(i,k,j)
            cwmbdy3dtemp2(i,k,j) = grid%nmm_cwm(i,k,j)
            q2bdy3dtemp2(i,k,j) = grid%nmm_q2(i,k,j)
          END DO
        END DO

         DO i = ips , MIN(ide,ipe)
            pdbdy2dtemp2(i,j) = grid%nmm_pd(i,j)
         END DO


!!     Western boundary
	I=1
        DO k = kps , MIN(kde,kpe)
          DO j = jps+2, jde-2, 2
            tbdy3dtemp2(i,k,j) = grid%nmm_t(i,k,j)
            qbdy3dtemp2(i,k,j) = grid%nmm_q(i,k,j)
            cwmbdy3dtemp2(i,k,j) = grid%nmm_cwm(i,k,j)
            q2bdy3dtemp2(i,k,j) = grid%nmm_q2(i,k,j)
          END DO
        END DO

        DO k = kps , MIN(kde,kpe)
          DO j = jps+1, jde-1, 2
            ubdy3dtemp2(i,k,j) = grid%nmm_u(i,k,j)
            vbdy3dtemp2(i,k,j) = grid%nmm_v(i,k,j)
          END DO
        END DO

        DO j = jps+2,jde-2,2
            pdbdy2dtemp2(i,j) = grid%nmm_pd(i,j)
        END DO

!!     Eastern boundary
	I=MIN(IDE,IPE)

        DO k = kps , MIN(kde,kpe)
          DO j = jps+2, jde-2, 2
            tbdy3dtemp2(i,k,j) = grid%nmm_t(i,k,j)
            qbdy3dtemp2(i,k,j) = grid%nmm_q(i,k,j)
            cwmbdy3dtemp2(i,k,j) = grid%nmm_cwm(i,k,j)
            q2bdy3dtemp2(i,k,j) = grid%nmm_q2(i,k,j)
          END DO
        END DO

        DO k = kps , MIN(kde,kpe)
          DO j = jps+1, jde-1, 2
            ubdy3dtemp2(i,k,j) = grid%nmm_u(i,k,j)
            vbdy3dtemp2(i,k,j) = grid%nmm_v(i,k,j)
          END DO
        END DO

        DO j = jps+2,jde-2,2
            pdbdy2dtemp2(i,j) = grid%nmm_pd(i,j)
        END DO

      !  During all of the loops after the first loop, 
      !  we first compute the boundary
      !  tendencies with the current data values 
      !  (*bdy3dtemp2 arrays) and the previously 
      !  saved information stored in the *bdy3dtemp1 arrays.


      CALL stuff_bdytend ( ubdy3dtemp2 , ubdy3dtemp1 , REAL(interval_seconds),&
                                   grid%nmm_u_bt  , 'N' , &
                                   ijds , ijde , spec_bdy_width      , &
                                   ids , ide , jds , jde , kds , kde+1 , & ! havent checked these +1s
                                   ims , ime , jms , jme , kms , kme , &
                                   ips , ipe , jps , jpe , kps , kpe+1 )
      CALL stuff_bdytend ( vbdy3dtemp2 , vbdy3dtemp1 , REAL(interval_seconds),&
                                    grid%nmm_v_bt  , 'N' , &
                                   ijds , ijde , spec_bdy_width      , &
                                   ids , ide , jds , jde , kds , kde+1 , &
                                   ims , ime , jms , jme , kms , kme , &
                                   ips , ipe , jps , jpe , kps , kpe+1 )
      CALL stuff_bdytend ( tbdy3dtemp2 , tbdy3dtemp1 , REAL(interval_seconds),&
                                   grid%nmm_t_bt  , 'N' , &
                                   ijds , ijde , spec_bdy_width      , &
                                   ids , ide , jds , jde , kds , kde+1 , &
                                   ims , ime , jms , jme , kms , kme , &
                                   ips , ipe , jps , jpe , kps , kpe+1 )

      CALL stuff_bdytend ( cwmbdy3dtemp2,cwmbdy3dtemp1,REAL(interval_seconds),&
                                   grid%nmm_cwm_bt  , 'N' , &
                                   ijds , ijde , spec_bdy_width      , &
                                   ids , ide , jds , jde , kds , kde+1 , &
                                   ims , ime , jms , jme , kms , kme , &
                                   ips , ipe , jps , jpe , kps , kpe+1 )

      CALL stuff_bdytend ( qbdy3dtemp2 , qbdy3dtemp1 , REAL(interval_seconds),&
                                   grid%nmm_q_bt , 'N' , &
                                   ijds , ijde , spec_bdy_width      , &
                                   ids , ide , jds , jde , kds , kde+1 , &
                                   ims , ime , jms , jme , kms , kme , &
                                   ips , ipe , jps , jpe , kps , kpe+1 )



    CALL stuff_bdytend ( q2bdy3dtemp2, q2bdy3dtemp1 , REAL(interval_seconds),&
                                   grid%nmm_q2_bt , 'N' , &
                                   ijds , ijde , spec_bdy_width      , &
                                   ids , ide , jds , jde , kds , kde+1 , &
                                   ims , ime , jms , jme , kms , kme , &
                                   ips , ipe , jps , jpe , kps , kpe+1 )

        write(0,*) 'pdbdy2dtemp2(1,1): ', pdbdy2dtemp2(1,1)

!    CALL stuff_bdytend2( pdbdy2dtemp2 , pdbdy2dtemp1, REAL(interval_seconds),&
!                                   grid%nmm_pd_bt  , N , &
!                                   ijds , ijde , spec_bdy_width      , &
!                                   ids , ide , jds , jde , kds , kde , &
!                                   ims , ime , jms , jme , kms , kme , &
!                                   ips , ipe , jps , jpe , kps , kpe )

    CALL stuff_bdytend2( pdbdy2dtemp2 , pdbdy2dtemp1, REAL(interval_seconds),&
                                   grid%nmm_pd_bt  , 'N' , &
                                   ijds , ijde , spec_bdy_width      , &
                                   ids , ide , jds , jde , 1 , 1 , &
                                   ims , ime , jms , jme , 1 , 1 , &
                                   ips , ipe , jps , jpe , 1 , 1 )

	write(0,*) 'grid%nmm_pd_bt(1,1): ', grid%nmm_pd_bt(1,1,1,1)

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
	write(0,*) 'call output_boundary, loop: ', loop
	write(6,*) 'call output_boundary, loop: ', loop
      CALL output_boundary ( id, grid , config_flags , ierr )
      current_date = temp24
      start_date = temp24b

      !  OK, for all of the loops, we output the initialzation 
      !  data, which would allow us to
      !  start the model at any of the available analysis time periods.

!  WRITE ( loop_char , FMT = (I4.4) ) loop
!  CALL open_w_dataset ( id1, wrfinput//loop_char , grid , config_flags , output_model_input , "DATASET=INPUT", ierr )
!  IF ( ierr .NE. 0 ) THEN
!    CALL wrf_error_fatal( real: error opening wrfinput//loop_char// for writing )
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

	write(6,*) 'limits at end: ipe,jpe,kpe: ', ipe,jpe,kpe

         DO j = jps , jpe
            DO k = kps , kpe
               DO i = ips , ipe
                  ubdy3dtemp1(i,k,j) = ubdy3dtemp2(i,k,j)
                  vbdy3dtemp1(i,k,j) = vbdy3dtemp2(i,k,j)
                  tbdy3dtemp1(i,k,j) = tbdy3dtemp2(i,k,j)
                  cwmbdy3dtemp1(i,k,j) = cwmbdy3dtemp2(i,k,j)
                  qbdy3dtemp1(i,k,j) = qbdy3dtemp2(i,k,j)
                  q2bdy3dtemp1(i,k,j) = q2bdy3dtemp2(i,k,j)
               END DO
            END DO
         END DO

!mp	change these limits?????????

         DO j = jps , jpe
            DO i = ips , ipe
               pdbdy2dtemp1(i,  j) = pdbdy2dtemp2(i,  j)
            END DO
         END DO

  !  There are 2 components to the lateral boundaries.  
  !   First, there is the starting
  !  point of this time period - just the outer few rows and columns.


         CALL stuff_bdy ( ubdy3dtemp1 , grid%nmm_u_b  , 'N' ,& 
                                        ijds , ijde , spec_bdy_width      , &
                                        ids , ide , jds , jde , kds , kde+1 , &
                                        ims , ime , jms , jme , kms , kme , &
                                        ips , ipe , jps , jpe , kps , kpe+1 )
         CALL stuff_bdy ( vbdy3dtemp1 , grid%nmm_v_b  , 'N' , &
                                        ijds , ijde , spec_bdy_width      , &
                                        ids , ide , jds , jde , kds , kde+1 , &
                                        ims , ime , jms , jme , kms , kme , &
                                        ips , ipe , jps , jpe , kps , kpe+1 )
         CALL stuff_bdy ( tbdy3dtemp1 , grid%nmm_t_b  , 'N' , &
                                        ijds , ijde , spec_bdy_width      , &
                                        ids , ide , jds , jde , kds , kde+1 , &
                                        ims , ime , jms , jme , kms , kme , &
                                        ips , ipe , jps , jpe , kps , kpe+1 )

         CALL stuff_bdy ( cwmbdy3dtemp1 , grid%nmm_cwm_b , 'N' , &
                                          ijds , ijde , spec_bdy_width      , &
                                          ids , ide , jds , jde , kds , kde+1 , &
                                          ims , ime , jms , jme , kms , kme , &
                                          ips , ipe , jps , jpe , kps , kpe+1 )
         CALL stuff_bdy ( qbdy3dtemp1 , grid%nmm_q_b , 'N' ,&
                                        ijds , ijde , spec_bdy_width      , &
                                        ids , ide , jds , jde , kds , kde+1 , &
                                        ims , ime , jms , jme , kms , kme , &
                                        ips , ipe , jps , jpe , kps , kpe+1 )


         CALL stuff_bdy ( q2bdy3dtemp1 , grid%nmm_q2_b, 'N' ,&
                                         ijds , ijde , spec_bdy_width      , &
                                         ids , ide , jds , jde , kds , kde+1 , &
                                         ims , ime , jms , jme , kms , kme , &
                                         ips , ipe , jps , jpe , kps , kpe+1 )

!         CALL stuff_bdy2 ( pdbdy2dtemp1 , grid%nmm_pd_b , N ,&
!                                          ijds , ijde , spec_bdy_width      , &
!                                          ids , ide , jds , jde , kds , kde , &
!                                          ims , ime , jms , jme , kms , kme , &
!                                          ips , ipe , jps , jpe , kps , kpe )

         CALL stuff_bdy2 ( pdbdy2dtemp1 , grid%nmm_pd_b , 'N' ,&
                                          ijds , ijde , spec_bdy_width  , &
                                          ids , ide , jds , jde , 1 , 1 , &
                                          ims , ime , jms , jme , 1 , 1 , &
                                          ips , ipe , jps , jpe , 1 , 1 )

	       write(0,*) 'grid%nmm_pd_b(1,1): ', grid%nmm_pd_b(1,1,1,1)

      ELSE IF ( loop .EQ. time_loop_max ) THEN

    !  If this is the last time through here, we need to close the files.

         CALL close_dataset ( id , config_flags , "DATASET=BOUNDARY" )

      END IF

   END IF

END SUBROUTINE assemble_output
