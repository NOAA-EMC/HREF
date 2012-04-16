   SUBROUTINE dfi_accumulate( grid )

      USE module_domain
      USE module_configure
      USE module_driver_constants
      USE module_machine
      USE module_dm
      USE module_model_constants
      USE module_state_description

      IMPLICIT NONE

      REAL hn

      

      TYPE(domain) , POINTER          :: grid


      IF ( grid%dfi_opt .EQ. DFI_NODFI .OR. grid%dfi_stage .EQ. DFI_FST ) RETURN

      hn = grid%hcoeff(grid%itimestep+1)





      hn = grid%hcoeff(grid%ntsd+1)
	write(0,*) 'hn applied: ', hn
       grid%dfi_pd(:,:)    = grid%dfi_pd(:,:)    + grid%pd(:,:)    * hn
       grid%dfi_pint(:,:,:)   = grid%dfi_pint(:,:,:)   + grid%pint(:,:,:)   * hn
       grid%dfi_dwdt(:,:,:)   = grid%dfi_dwdt(:,:,:)   + grid%dwdt(:,:,:)   * hn
       grid%dfi_t(:,:,:)   = grid%dfi_t(:,:,:)   + grid%t(:,:,:)   * hn
       grid%dfi_q(:,:,:)   = grid%dfi_q(:,:,:)   + grid%q(:,:,:)   * hn
       grid%dfi_q2(:,:,:)  = grid%dfi_q2(:,:,:)  + grid%q2(:,:,:)  * hn
       grid%dfi_cwm(:,:,:) = grid%dfi_cwm(:,:,:) + grid%cwm(:,:,:) * hn
       grid%dfi_u(:,:,:)   = grid%dfi_u(:,:,:)   + grid%u(:,:,:)   * hn
       grid%dfi_v(:,:,:)   = grid%dfi_v(:,:,:)   + grid%v(:,:,:)   * hn
       grid%dfi_moist(:,:,:,:)  = grid%dfi_moist(:,:,:,:)  + grid%moist(:,:,:,:)  * hn
       grid%dfi_scalar(:,:,:,:) = grid%dfi_scalar(:,:,:,:) + grid%scalar(:,:,:,:) * hn
      
      grid%hcoeff_tot = grid%hcoeff_tot + hn

   END SUBROUTINE dfi_accumulate



   SUBROUTINE wrf_dfi_bck_init ( )

      USE module_domain
      USE module_utility

      IMPLICIT NONE

      INTEGER rc

      INTERFACE
         SUBROUTINE Setup_Timekeeping(grid)
            USE module_domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE Setup_Timekeeping

         SUBROUTINE dfi_save_arrays(grid)
            USE module_domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE dfi_save_arrays

         SUBROUTINE dfi_clear_accumulation(grid)
            USE module_domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE dfi_clear_accumulation

         SUBROUTINE optfil_driver(grid)
            USE module_domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE optfil_driver

         SUBROUTINE start_domain(grid, allowed_to_read)
            USE module_domain
            TYPE (domain)          :: grid
            LOGICAL, INTENT(IN)    :: allowed_to_read
         END SUBROUTINE start_domain
      END INTERFACE

      head_grid%dfi_stage = DFI_BCK

      
      CALL nl_set_time_step ( 1, -head_grid%time_step )
      CALL nl_set_time_step_fract_num ( 1, -head_grid%time_step_fract_num )

      CALL Setup_Timekeeping (head_grid)

      
      CALL nl_set_mp_physics( 1, 0 )
      CALL nl_set_ra_lw_physics( 1, 0 )
      CALL nl_set_ra_sw_physics( 1, 0 )
      CALL nl_set_sf_surface_physics( 1, 0 )
      CALL nl_set_sf_sfclay_physics( 1, 0 )
      CALL nl_set_bl_pbl_physics( 1, 0 )
      CALL nl_set_cu_physics( 1, 0 )
      CALL nl_set_damp_opt( 1, 0 )

      






      CALL nl_get_time_step( 1, head_grid%time_step )
      if (head_grid%time_step .lt. 0) then


	write(0,*) 'changing signs for backward integration'
	write(0,*) 'head_grid%time_step is: ', head_grid%time_step
      head_grid%CPGFV = -head_grid%CPGFV
	write(0,*) 'head_grid%CPGFV now: ', head_grid%CPGFV
      head_grid%EN    = -head_grid%EN
      head_grid%ENT   = -head_grid%ENT
      head_grid%F4D   = -head_grid%F4D
      head_grid%F4Q   = -head_grid%F4Q
      head_grid%EF4T  = -head_grid%EF4T

      head_grid%EM(:) = -head_grid%EM(:)
      head_grid%EMT(:) = -head_grid%EMT(:)
      head_grid%F4Q2(:) = -head_grid%F4Q2(:)



      head_grid%WPDAR(:,:)= -head_grid%WPDAR(:,:)
      head_grid%CPGFU(:,:)= -head_grid%CPGFU(:,:)
      head_grid%CURV(:,:)= -head_grid%CURV(:,:)
      head_grid%FCP(:,:)= -head_grid%FCP(:,:)
      head_grid%FAD(:,:)= -head_grid%FAD(:,:)
      head_grid%F(:,:)= -head_grid%F(:,:)
	endif

      head_grid%start_subtime = domain_get_start_time ( head_grid )
      head_grid%stop_subtime = domain_get_stop_time ( head_grid )

      CALL WRFU_ClockSet(head_grid%domain_clock, currTime=head_grid%start_subtime, rc=rc)
 
      CALL dfi_save_arrays ( head_grid )
      CALL dfi_clear_accumulation( head_grid )
      CALL optfil_driver(head_grid)

      
      CALL start_domain ( head_grid , .TRUE. )

   END SUBROUTINE wrf_dfi_bck_init


   SUBROUTINE wrf_dfi_fwd_init ( )

      USE module_domain
      USE module_utility

      IMPLICIT NONE

      INTEGER rc

      INTERFACE
         SUBROUTINE Setup_Timekeeping(grid)
            USE module_domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE Setup_Timekeeping

         SUBROUTINE dfi_save_arrays(grid)
            USE module_domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE dfi_save_arrays

         SUBROUTINE dfi_clear_accumulation(grid)
            USE module_domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE dfi_clear_accumulation

         SUBROUTINE optfil_driver(grid)
            USE module_domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE optfil_driver

         SUBROUTINE start_domain(grid, allowed_to_read)
            USE module_domain
            TYPE (domain)          :: grid
            LOGICAL, INTENT(IN)    :: allowed_to_read
         END SUBROUTINE start_domain
      END INTERFACE

      head_grid%dfi_stage = DFI_FWD

      
      
      
      

      CALL nl_get_time_step( 1, head_grid%time_step )
      CALL nl_get_time_step_fract_num( 1, head_grid%time_step_fract_num )

      head_grid%time_step = abs(head_grid%time_step)
      CALL nl_set_time_step( 1, head_grid%time_step )

      head_grid%time_step_fract_num = abs(head_grid%time_step_fract_num)
      CALL nl_set_time_step_fract_num( 1, head_grid%time_step_fract_num )

      head_grid%itimestep=0
      head_grid%xtime=0.

      
      CALL nl_set_mp_physics( 1, head_grid%mp_physics)
      CALL nl_set_ra_lw_physics( 1, head_grid%ra_lw_physics)
      CALL nl_set_ra_sw_physics( 1, head_grid%ra_sw_physics)
      CALL nl_set_sf_surface_physics( 1, head_grid%sf_surface_physics)
      CALL nl_set_sf_sfclay_physics( 1, head_grid%sf_sfclay_physics)
      CALL nl_set_bl_pbl_physics( 1, head_grid%bl_pbl_physics)
      CALL nl_set_cu_physics( 1, head_grid%cu_physics)
      CALL nl_set_damp_opt( 1, head_grid%damp_opt)




      CALL nl_get_time_step( 1, head_grid%time_step )


      if (head_grid%cpgfv .gt. 0) then


	write(0,*) 'changing signs for forward integration'
	write(0,*) 'head_grid%time_step is: ', head_grid%time_step
      head_grid%CPGFV = -head_grid%CPGFV
	write(0,*) 'head_grid%CPGFV now: ', head_grid%CPGFV
      head_grid%EN    = -head_grid%EN
      head_grid%ENT   = -head_grid%ENT
      head_grid%F4D   = -head_grid%F4D
      head_grid%F4Q   = -head_grid%F4Q
      head_grid%EF4T  = -head_grid%EF4T

      head_grid%EM(:) = -head_grid%EM(:)
      head_grid%EMT(:) = -head_grid%EMT(:)
      head_grid%F4Q2(:) = -head_grid%F4Q2(:)



      head_grid%WPDAR(:,:)= -head_grid%WPDAR(:,:)
      head_grid%CPGFU(:,:)= -head_grid%CPGFU(:,:)
      head_grid%CURV(:,:)= -head_grid%CURV(:,:)
      head_grid%FCP(:,:)= -head_grid%FCP(:,:)
      head_grid%FAD(:,:)= -head_grid%FAD(:,:)
      head_grid%F(:,:)= -head_grid%F(:,:)
	endif


      CALL Setup_Timekeeping (head_grid)
      head_grid%start_subtime = domain_get_start_time ( head_grid )
      head_grid%stop_subtime = domain_get_stop_time ( head_grid )

      CALL WRFU_ClockSet(head_grid%domain_clock, currTime=head_grid%start_subtime, rc=rc)

      IF ( head_grid%dfi_opt .EQ. DFI_DFL ) THEN
         CALL dfi_save_arrays ( head_grid )
      END IF
      CALL dfi_clear_accumulation( head_grid )
      CALL optfil_driver(head_grid)

      

	write(0,*) 'calling start_domain from wrf_dfi_fwd_init'
      CALL start_domain ( head_grid , .TRUE. )

   END SUBROUTINE wrf_dfi_fwd_init


   SUBROUTINE wrf_dfi_fst_init ( )

      USE module_domain

      IMPLICIT NONE

      CHARACTER (LEN=80)      :: wrf_error_message

      INTERFACE
         SUBROUTINE Setup_Timekeeping(grid)
            USE module_domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE Setup_Timekeeping

         SUBROUTINE dfi_save_arrays(grid)
            USE module_domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE dfi_save_arrays

         SUBROUTINE dfi_clear_accumulation(grid)
            USE module_domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE dfi_clear_accumulation

         SUBROUTINE optfil_driver(grid)
            USE module_domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE optfil_driver

         SUBROUTINE start_domain(grid, allowed_to_read)
            USE module_domain
            TYPE (domain)          :: grid
            LOGICAL, INTENT(IN)    :: allowed_to_read
         END SUBROUTINE start_domain
      END INTERFACE

      head_grid%dfi_stage = DFI_FST 

      head_grid%itimestep=0
      head_grid%xtime=0.         

      CALL Setup_Timekeeping (head_grid)
      head_grid%start_subtime = domain_get_start_time ( head_grid )
      head_grid%stop_subtime = domain_get_stop_time ( head_grid )

      CALL start_domain ( head_grid , .TRUE. )

   END SUBROUTINE wrf_dfi_fst_init


   SUBROUTINE wrf_dfi_write_initialized_state( )

      
      USE module_domain
      USE module_io_domain
      USE module_configure

      IMPLICIT NONE

      INTEGER             :: fid, ierr
      CHARACTER (LEN=80)  :: wrf_error_message
      CHARACTER (LEN=80)  :: rstname
      CHARACTER (LEN=132) :: message

      TYPE (grid_config_rec_type) :: config_flags

      CALL model_to_grid_config_rec ( head_grid%id , model_config_rec , config_flags )

      WRITE (wrf_err_message,'(A,I4)') 'Writing out initialized model state'
      CALL wrf_message(TRIM(wrf_err_message))

      rstname = 'wrfinput_initialized_d01'
      CALL open_w_dataset ( fid, TRIM(rstname), head_grid, config_flags, output_model_input, "DATASET=INPUT", ierr )
      IF ( ierr .NE. 0 ) THEN
         WRITE( message , '("program wrf: error opening ",A," for writing")') TRIM(rstname)
         CALL wrf_error_fatal3("",348,&
message )
      END IF
      CALL output_model_input ( fid,   head_grid, config_flags, ierr )
      CALL close_dataset ( fid, config_flags, "DATASET=INPUT" )

   END SUBROUTINE wrf_dfi_write_initialized_state


   SUBROUTINE wrf_dfi_array_reset ( )

      USE module_domain

      IMPLICIT NONE

      INTERFACE
         SUBROUTINE dfi_array_reset(grid)
            USE module_domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE dfi_array_reset
      END INTERFACE

      
      
      CALL dfi_array_reset( head_grid )

   END SUBROUTINE wrf_dfi_array_reset


   SUBROUTINE optfil_driver( grid )

      USE module_domain
      USE module_wrf_error
      USE module_timing
      USE module_date_time
      USE module_configure
      USE module_state_description
      USE module_model_constants

      IMPLICIT NONE

      TYPE (domain) , POINTER                 :: grid

      
      integer :: nstep2, nstepmax, rundfi, i, rc
      real :: timestep, tauc
      TYPE(WRFU_TimeInterval) :: run_interval

      timestep=abs(grid%dt)
      run_interval = grid%stop_subtime - grid%start_subtime

      CALL WRFU_TimeIntervalGet( run_interval, S=rundfi, rc=rc )
      rundfi = abs(rundfi)

      nstep2= ceiling((1.0 + real(rundfi)/timestep) / 2.0)

      
      

	write(0,*) 'nstep2: ', nstep2

      tauc = real(grid%dfi_cutoff_seconds)

      
      grid%hcoeff(:) = 0.0
      IF ( grid%dfi_nfilter < 0 .OR. grid%dfi_nfilter > 8 ) THEN
write(0,*) 'Invalid filter specified in namelist.'
      ELSE
         call dfcoef(nstep2-1, grid%dt, tauc, grid%dfi_nfilter, grid%hcoeff)
      END IF

      IF ( MOD(int(1.0 + real(rundfi)/timestep),2) /= 0 ) THEN
         DO i=1,nstep2-1 
            grid%hcoeff(2*nstep2-i) = grid%hcoeff(i)
         END DO
      ELSE
         DO i=1,nstep2 
            grid%hcoeff(2*nstep2-i+1) = grid%hcoeff(i)
         END DO
      END IF

	write(0,*) 'leave optfil_driver with hcoeff(1:10): ', grid%hcoeff(1:10)

   END SUBROUTINE optfil_driver


   SUBROUTINE dfi_clear_accumulation( grid )

      USE module_domain
      USE module_configure
      USE module_driver_constants
      USE module_machine
      USE module_dm
      USE module_model_constants
      USE module_state_description

      IMPLICIT NONE

      
      TYPE(domain) , POINTER          :: grid


       grid%dfi_pd(:,:)    = 0.
       grid%dfi_pint(:,:,:)   = 0.
       grid%dfi_dwdt(:,:,:)   = 0.
       grid%dfi_t(:,:,:)   = 0.
       grid%dfi_q(:,:,:)   = 0.
       grid%dfi_q2(:,:,:)  = 0.
       grid%dfi_cwm(:,:,:) = 0.
       grid%dfi_u(:,:,:)   = 0.
       grid%dfi_v(:,:,:)   = 0.
      grid%dfi_moist(:,:,:,:)  = 0.
      grid%dfi_scalar(:,:,:,:) = 0.

      grid%hcoeff_tot  = 0.0

   END SUBROUTINE dfi_clear_accumulation


   SUBROUTINE dfi_save_arrays( grid )

      USE module_domain
      USE module_configure
      USE module_driver_constants
      USE module_machine
      USE module_dm
      USE module_model_constants
      USE module_state_description

      IMPLICIT NONE

      
      TYPE(domain) , POINTER          :: grid

      
      grid%dfi_SNOW(:,:)   = grid%SNOW(:,:) 
      grid%dfi_SNOWH(:,:)  = grid%SNOWH(:,:) 

      grid%dfi_CANWAT(:,:) = grid%CANWAT(:,:) 
      grid%dfi_NMM_TSK(:,:)    = grid%NMM_TSK(:,:) 
      
      grid%dfi_STC(:,:,:)         = grid%STC(:,:,:) 
      grid%dfi_SMC(:,:,:)        = grid%SMC(:,:,:) 
      grid%dfi_SH2O(:,:,:)       = grid%SH2O(:,:,:) 

   END SUBROUTINE dfi_save_arrays


   SUBROUTINE dfi_array_reset( grid )

      USE module_domain
      USE module_configure
      USE module_driver_constants
      USE module_machine
      USE module_dm
      USE module_model_constants
      USE module_state_description

      IMPLICIT NONE

      
      TYPE(domain) , POINTER          :: grid

      IF ( grid%dfi_opt .EQ. DFI_NODFI ) RETURN


      
      


	write(0,*) ' divide by grid%hcoeff_tot: ', grid%hcoeff_tot
       grid%pd(:,:)       = grid%dfi_pd(:,:)  / grid%hcoeff_tot
       grid%pint(:,:,:)      = grid%dfi_pint(:,:,:) / grid%hcoeff_tot

       grid%t(:,:,:)      = grid%dfi_t(:,:,:) / grid%hcoeff_tot
       grid%q(:,:,:)      = grid%dfi_q(:,:,:) / grid%hcoeff_tot
       grid%q2(:,:,:)     = grid%dfi_q2(:,:,:) / grid%hcoeff_tot
       grid%cwm(:,:,:)    = grid%dfi_cwm(:,:,:) / grid%hcoeff_tot
       grid%u(:,:,:)      = grid%dfi_u(:,:,:) / grid%hcoeff_tot
       grid%v(:,:,:)      = grid%dfi_v(:,:,:) / grid%hcoeff_tot
      grid%moist(:,:,:,:)  = grid%dfi_moist(:,:,:,:)  / grid%hcoeff_tot
      grid%scalar(:,:,:,:) = grid%dfi_scalar(:,:,:,:) / grid%hcoeff_tot

      
      grid%SNOW(:,:)   = grid%dfi_SNOW(:,:) 
      grid%SNOWH(:,:)  = grid%dfi_SNOWH(:,:) 

      grid%CANWAT(:,:) = grid%dfi_CANWAT(:,:) 
      grid%NMM_TSK(:,:)    = grid%dfi_NMM_TSK(:,:) 
      
      grid%STC(:,:,:)         = grid%dfi_STC(:,:,:) 
      grid%SMC(:,:,:)        = grid%dfi_SMC(:,:,:) 
      grid%SH2O(:,:,:)       = grid%dfi_SH2O(:,:,:) 


   END SUBROUTINE dfi_array_reset


   SUBROUTINE dfcoef (NSTEPS,DT,TAUC,NFILT,H)


















 
      implicit none

      integer, intent(in)   :: nsteps, nfilt
      real   , intent(in)   :: dt, tauc
      real, intent(out)  :: h(1:nsteps+1)
      
      

      integer               :: n
      real                  :: pi, omegac, x, window, deltat
      real                  :: hh(0:nsteps)

      pi=4*ATAN(1.)
      deltat=ABS(dt)

      

      if ( nfilt .eq. -1) call debug    (nsteps,h)

      IF ( NFILT .EQ. 0 ) CALL UNIFORM  (NSTEPS,HH)
      IF ( NFILT .EQ. 1 ) CALL LANCZOS  (NSTEPS,HH)
      IF ( NFILT .EQ. 2 ) CALL HAMMING  (NSTEPS,HH)
      IF ( NFILT .EQ. 3 ) CALL BLACKMAN (NSTEPS,HH)
      IF ( NFILT .EQ. 4 ) CALL KAISER   (NSTEPS,HH)
      IF ( NFILT .EQ. 5 ) CALL POTTER2  (NSTEPS,HH)
      IF ( NFILT .EQ. 6 ) CALL DOLPHWIN (NSTEPS,HH)

      IF ( NFILT .LE. 6 ) THEN     

         
         OMEGAC = 2.*PI/TAUC

         DO N=0,NSTEPS
            WINDOW = HH(N)
            IF ( N .EQ. 0 ) THEN
              X = (OMEGAC*DELTAT/PI)
            ELSE
              X = SIN(N*OMEGAC*DELTAT)/(N*PI)
            END IF
            HH(N) = X*WINDOW
         END DO

         
         CALL NORMLZ(HH,NSTEPS)

         DO N=0,NSTEPS
            H(N+1) = HH(NSTEPS-N)
         END DO

      ELSE IF ( NFILT .EQ. 7 ) THEN     

	write(0,*) 'calling DOLPH...TAUC, DT, NSTEPS: ', TAUC, DT, NSTEPS
         CALL DOLPH(DT,TAUC,NSTEPS,H)
	write(0,*) 'return DOLPH with H(1:10): ', H(1:10)

      ELSE IF ( NFILT .EQ. 8 ) THEN     

         CALL RHOFIL(DT,TAUC,2,NSTEPS*2,2,H,NSTEPS)

      END IF

      RETURN

   END SUBROUTINE dfcoef


   SUBROUTINE NORMLZ(HH,NMAX)

   

      implicit none
  
      integer, intent(in)                       :: nmax
      real   , dimension(0:nmax), intent(out)   :: hh

      
      real     :: sumhh
      integer  :: n

      SUMHH = HH(0)
      DO N=1,NMAX
        SUMHH = SUMHH + 2*HH(N)
      ENDDO
      DO N=0,NMAX
        HH(N)  = HH(N)/SUMHH
      ENDDO

      RETURN

   END subroutine normlz


   subroutine debug(nsteps, ww)

      implicit none

      integer, intent(in)                        :: nsteps
      real   , dimension(0:nsteps), intent(out)  :: ww
      integer :: n

      do n=0,nsteps
         ww(n)=0
      end do

      ww(int(nsteps/2))=1

      return

   end subroutine debug


   SUBROUTINE UNIFORM(NSTEPS,WW)

   

      implicit none

      integer, intent(in)                        :: nsteps
      real   , dimension(0:nsteps), intent(out)  :: ww
       
      integer          :: n

      DO N=0,NSTEPS
        WW(N) = 1.
      ENDDO

      RETURN

   END subroutine uniform


   SUBROUTINE LANCZOS(NSTEPS,WW)

   

      implicit none 

      integer,  parameter                      :: nmax = 1000
      integer,  intent(in)                     :: nsteps
      real   ,  dimension(0:nmax), intent(out) :: ww
      integer  ::  n
      real     :: power, pi, w

      
      POWER = 1

      PI=4*ATAN(1.)
      DO N=0,NSTEPS
         IF ( N .EQ. 0 ) THEN
            W = 1.0
         ELSE
            W = SIN(N*PI/(NSTEPS+1)) / ( N*PI/(NSTEPS+1))
         ENDIF
         WW(N) = W**POWER
      ENDDO

      RETURN

   END SUBROUTINE lanczos


   SUBROUTINE HAMMING(NSTEPS,WW)

   

      implicit none

      integer, intent(in)           :: nsteps
      real, dimension(0:nsteps)    :: ww
      integer   ::   n
      real      :: alpha, pi, w

      
      
      ALPHA=0.54

      PI=4*ATAN(1.)
      DO N=0,NSTEPS
         IF ( N .EQ. 0 ) THEN
            W = 1.0
         ELSE
            W = ALPHA + (1-ALPHA)*COS(N*PI/(NSTEPS))
         ENDIF
         WW(N) = W
      ENDDO

      RETURN

   END SUBROUTINE hamming


   SUBROUTINE BLACKMAN(NSTEPS,WW)

   

      implicit none

      integer, intent(in)           :: nsteps
      real, dimension(0:nsteps)    :: ww
      integer  :: n

      real :: pi, w

      PI=4*ATAN(1.)
      DO N=0,NSTEPS
         IF ( N .EQ. 0 ) THEN
            W = 1.0
         ELSE
            W = 0.42 + 0.50*COS(  N*PI/(NSTEPS))   &
                     + 0.08*COS(2*N*PI/(NSTEPS))
         ENDIF
         WW(N) = W
      ENDDO

      RETURN

   END SUBROUTINE blackman


   SUBROUTINE KAISER(NSTEPS,WW)

   

      implicit none

      real, external :: bessi0

      integer, intent(in)           :: nsteps
      real, dimension(0:nsteps)    :: ww
      integer   :: n
      real      :: alpha, xi0a, xn, as

      ALPHA = 1

      XI0A =  BESSI0(ALPHA)
      DO N=0,NSTEPS
         XN = N
         AS = ALPHA*SQRT(1.-(XN/NSTEPS)**2)
         WW(N) = BESSI0(AS) / XI0A
      ENDDO

      RETURN

   END SUBROUTINE kaiser


   REAL FUNCTION BESSI0(X)

   

      implicit none

      real(8)   :: Y
      real(8)   :: P1 = 1.0d0
      real(8)   :: P2 = 3.5156230D0
      real(8)   :: P3 = 3.0899424D0
      real(8)   :: P4 = 1.2067492D0
      real(8)   :: P5 = 0.2659732D0
      real(8)   :: P6 = 0.360768D-1
      real(8)   :: P7 = 0.45813D-2

      real*8    :: Q1 = 0.39894228D0
      real*8    :: Q2 = 0.1328592D-1
      real*8    :: Q3 = 0.225319D-2
      real*8    :: Q4 = -0.157565D-2
      real*8    :: Q5 = 0.916281D-2
      real*8    :: Q6 = -0.2057706D-1
      real*8    :: Q7 = 0.2635537D-1
      real*8    :: Q8 = -0.1647633D-1
      real*8    :: Q9 = 0.392377D-2

      real            :: x, ax


      IF (ABS(X).LT.3.75) THEN
        Y=(X/3.75)**2
        BESSI0=P1+Y*(P2+Y*(P3+Y*(P4+Y*(P5+Y*(P6+Y*P7)))))
      ELSE
        AX=ABS(X)
        Y=3.75/AX
        BESSI0=(EXP(AX)/SQRT(AX))*(Q1+Y*(Q2+Y*(Q3+Y*(Q4    &
           +Y*(Q5+Y*(Q6+Y*(Q7+Y*(Q8+Y*Q9))))))))
      ENDIF
      RETURN

   END FUNCTION bessi0


   SUBROUTINE POTTER2(NSTEPS,WW)

      
      

      implicit none

      integer, intent(in)                       :: nsteps
      real, dimension(0:nsteps),intent(out)     ::  ww
      integer  :: n
      real     :: ck, sum, arg

      
      real, dimension(0:3)   :: d 
      real                   :: pi
      integer                :: ip

      d(0) = 0.35577019
      d(1) = 0.2436983
      d(2) = 0.07211497
      d(3) = 0.00630165

      PI=4*ATAN(1.)

      CK = 1.0
      DO N=0,NSTEPS
         IF (N.EQ.NSTEPS) CK = 0.5
         ARG = PI*FLOAT(N)/FLOAT(NSTEPS)

         ARG = ARG/2.

         SUM = D(0)
         DO IP=1,3
            SUM = SUM + 2.*D(IP)*COS(ARG*FLOAT(IP))
         END DO
         WW(N) = CK*SUM
      END DO

      RETURN

   END SUBROUTINE potter2


   SUBROUTINE dolphwin(m, window)















      IMPLICIT NONE

      
      INTEGER, INTENT(IN)                  ::  m
      REAL, DIMENSION(0:M), INTENT(OUT)    ::  window

      
      REAL, DIMENSION(0:2*M)               :: t
      REAL, DIMENSION(0:M)                 :: w, time
      REAL    :: pi, thetas, x0, term1, term2, rr, r, db, sum, arg
      INTEGER :: n, nm1, nt, i

      PI = 4*ATAN(1.D0)
      THETAS = 2*PI/M

      N = 2*M+1
      NM1 = N-1
      X0 = 1/COS(THETAS/2)

      TERM1 = (X0 + SQRT(X0**2-1))**(FLOAT(N-1))
      TERM2 = (X0 - SQRT(X0**2-1))**(FLOAT(N-1))
      RR = 0.5*(TERM1+TERM2)
      R = 1/RR
      DB = 20*LOG10(R)
      WRITE(*,'(1X,''DOLPH: M,N='',2I8)')M,N
      WRITE(*,'(1X,''DOLPH: THETAS (STOP-BAND EDGE)='',F10.3)')THETAS
      WRITE(*,'(1X,''DOLPH: R,DB='',2F10.3)')R, DB

      DO NT=0,M
        SUM = RR
        DO I=1,M
          ARG = X0*COS(I*PI/N)
          CALL CHEBY(T,NM1,ARG)
          TERM1 = T(NM1)
          TERM2 = COS(2*NT*PI*I/N)
          SUM = SUM + 2*TERM1*TERM2
        ENDDO
        W(NT) = SUM/N
        TIME(NT) = NT
      ENDDO


      DO NT=0,M
        WINDOW(NT) = W(NT)
      ENDDO

      RETURN

   END SUBROUTINE dolphwin


   SUBROUTINE dolph(deltat, taus, m, window)












      IMPLICIT NONE

      
      INTEGER, INTENT(IN)                  ::  m
      REAL, DIMENSION(0:M), INTENT(OUT)    ::  window
      REAL, INTENT(IN)                     :: deltat, taus

      
      integer, PARAMETER        :: NMAX = 5000
      REAL, dimension(0:NMAX)   :: t, w, time
      real, dimension(0:2*nmax) :: w2
      INTEGER                   :: NPRPE=0        
      CHARACTER*80              :: MES

      real    :: pi, thetas, x0, term1, term2, rr, r,db, sum, arg, sumw
      integer :: n, nm1, i, nt
      
      PI = 4*ATAN(1.D0)

      print *, 'in dfcoef, deltat = ', deltat, 'taus=',taus

      N = 2*M+1
      NM1 = N-1

      THETAS = 2*PI*ABS(DELTAT/TAUS)
      X0 = 1/COS(THETAS/2)
      TERM1 = (X0 + SQRT(X0**2-1))**(FLOAT(N-1))
      TERM2 = (X0 - SQRT(X0**2-1))**(FLOAT(N-1))
      RR = 0.5*(TERM1+TERM2)
      R = 1/RR
      DB = 20*LOG10(R)


      WRITE(*,'(1X,''DOLPH: M,N='',2I8)')M,N
      WRITE(*,'(1X,''DOLPH: THETAS (STOP-BAND EDGE)='',F10.3)')THETAS
      WRITE(*,'(1X,''DOLPH: R,DB='',2F10.3)')R, DB

      DO NT=0,M
         SUM = 1
         DO I=1,M
            ARG = X0*COS(I*PI/N)
            CALL CHEBY(T,NM1,ARG)
            TERM1 = T(NM1)
            TERM2 = COS(2*NT*PI*I/N)
            SUM = SUM + R*2*TERM1*TERM2
         ENDDO
         W(NT) = SUM/N
         TIME(NT) = NT
         WRITE(*,'(1X,''DOLPH: TIME, W='',F10.6,2X,E17.7)')  &
           TIME(NT), W(NT)
      ENDDO

      DO NT=0,M
         W2(M+NT) = W(NT)
         W2(M-NT) = W(NT)
      ENDDO


      SUMW = 0.
      DO NT=0,2*M
         SUMW = SUMW + W2(NT)
      ENDDO
      WRITE(*,'(1X,''DOLPH: SUM OF WEIGHTS W2='',F10.4)')SUMW

      DO NT=0,2*M
         WINDOW(NT) = W2(NT)
      ENDDO

      RETURN

   END SUBROUTINE dolph


   SUBROUTINE cheby(t, n, x)







      IMPLICIT NONE

      
      INTEGER, INTENT(IN)  :: n
      REAL, INTENT(IN)     :: x
      REAL, DIMENSION(0:N) :: t
 
      integer  :: nn

      T(0) = 1
      T(1) = X
      IF(N.LT.2) RETURN
      DO NN=2,N
         T(NN) = 2*X*T(NN-1) - T(NN-2)
      ENDDO

      RETURN

   END SUBROUTINE cheby


   SUBROUTINE rhofil(dt, tauc, norder, nstep, ictype, frow, nosize)




























      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      DOUBLE PRECISION MUC


      REAL, intent(in)    ::  DT,TAUC


      integer, intent(in) ::  norder, nstep, ictype, nosize
      REAL   , dimension(0:nosize), intent(out)::  FROW


      integer, PARAMETER  :: NOMAX=100
      real   , dimension(0:NOMAX) :: acoef, bcoef
      real   , dimension(0:NOMAX,0:NOMAX) :: filter

      real   , dimension(0:NOMAX) :: alpha, beta

      real   :: DTT

      DTT = ABS(DT)
      PI = 2*DASIN(1.D0)
      IOTA = CMPLX(0.,1.)


      THETAC = 2*PI*DTT/(TAUC)
      MUC = tan(THETAC/2) 
      FC = THETAC/(2*PI)


    DO NC=0,NOMAX
       ACOEF(NC) = 0.
       BCOEF(NC) = 0.
       ALPHA(NC) = 0.
       BETA (NC) = 0.
       FROW (NC) = 0.
       DO NR=0,NOMAX
          FILTER(NR,NC) = 0.
       ENDDO
    ENDDO


    FILTER(0,0) = 1.


    IF ( ICTYPE.eq.2 ) THEN
       CALL RHOCOF(NORDER,NOMAX,MUC, ACOEF,BCOEF)
    ENDIF

    DO 100 NROW=1,NSTEP

       IF ( ICTYPE.eq.1 ) THEN
          NORD = MIN(NROW,NORDER)
          IF ( NORD.le.NORDER) THEN
             CALL RHOCOF(NORD,NOMAX,MUC, ACOEF,BCOEF)
          ENDIF
       ENDIF

       DO K=0,NROW
          ALPHA(K) = ACOEF(NROW-K)
          IF(K.lt.NROW) BETA(K) = BCOEF(NROW-K)
       ENDDO


       IF ( ICTYPE.eq.2 ) THEN
          IF ( NROW.lt.NORDER ) THEN
             CN = 0.
             DO NN=NROW+1,NORDER
                CN = CN + (ACOEF(NN)+BCOEF(NN))
             ENDDO
             ALPHA(0) = ALPHA(0) + CN
          ENDIF
       ENDIF


      SUMAB = 0.
      DO NN=0,NROW
        SUMAB = SUMAB + ALPHA(NN)
          IF(NN.lt.NROW) SUMAB = SUMAB + BETA(NN)
      ENDDO

      DO KK=0,NROW-1
         SUMBF = 0.
         DO LL=0,NROW-1
            SUMBF = SUMBF + BETA(LL)*FILTER(LL,KK)
         ENDDO
         FILTER(NROW,KK) = ALPHA(KK)+SUMBF
      ENDDO
      FILTER(NROW,NROW) = ALPHA(NROW)


      SUMROW = 0.
      DO NN=0,NROW
        SUMROW = SUMROW + FILTER(NROW,NN)
      ENDDO

100 CONTINUE

      DO NC=0,NSTEP
        FROW(NC) = FILTER(NSTEP,NC)
      ENDDO

      RETURN

   END SUBROUTINE rhofil


   SUBROUTINE rhocof(nord, nomax, muc, ca, cb)




      IMPLICIT NONE 

      
      integer, intent(in)      :: nord, nomax
      real, dimension(0:nomax) :: ca, cb

      
      double precision, external :: cnr

      
      INTEGER                  :: nn
      COMPLEX                  :: IOTA
      DOUBLE PRECISION         :: MUC, ZN
      DOUBLE PRECISION         :: pi, root2, rn, sigma, gain, sumcof

      PI = 2*ASIN(1.)
      ROOT2 = SQRT(2.)
      IOTA = (0.,1.)

      RN = 1./FLOAT(NORD)
      SIGMA = 1./( SQRT(2.**RN-1.) )

      GAIN  = (MUC*SIGMA/(1+MUC*SIGMA))**NORD
      ZN = (1-MUC*SIGMA)/(1+MUC*SIGMA)

      DO NN=0,NORD
        CA(NN) = CNR(NORD,NN)*GAIN
        IF(NN.gt.0) CB(NN) = -CNR(NORD,NN)*(-ZN)**NN
      ENDDO


      SUMCOF = 0.
      DO NN=0,NORD
        SUMCOF = SUMCOF + CA(NN)
        IF(NN.gt.0) SUMCOF = SUMCOF + CB(NN)
      ENDDO

      RETURN

   END SUBROUTINE RHOCOF


   DOUBLE PRECISION FUNCTION cnr(n,r)

   


      IMPLICIT NONE

      
      INTEGER , intent(in)    :: n, R

      
      INTEGER          :: k
      DOUBLE PRECISION :: coeff, xn, xr, xk

      IF ( R.eq.0 ) THEN
         CNR = 1.0
         RETURN
      ENDIF
      Coeff = 1.0
      XN = DFLOAT(N)
      XR = DFLOAT(R)
      DO K=1,R
        XK = DFLOAT(K)
        COEFF = COEFF * ( (XN-XR+XK)/XK )
      ENDDO
      CNR = COEFF

      RETURN

   END FUNCTION cnr


    SUBROUTINE optfil (grid,NH,DELTAT,NHMAX)






















     USE module_domain
    
     TYPE(domain) , POINTER :: grid

     REAL,DIMENSION( 20) :: EDGE
     REAL,DIMENSION( 10) :: FX, WTX, DEVIAT
     REAL,DIMENSION(2*NHMAX+1) :: H
     logical LPRINT
     REAL, INTENT (IN) :: DELTAT
     INTEGER, INTENT (IN) :: NH, NHMAX

         TAUP = 3.
         TAUS = 1.5
         LPRINT = .true.


         NL=2*NHMAX+1
        do 101 n=1,NL
          H(n)=0.
 101    continue

        NFILT = 2*NH+1
        print *,' start optfil, NFILT=', nfilt



        IF(NFILT.LE.0 .OR. NFILT.GT.128 ) THEN
	   WRITE(6,*) 'NH=',NH
       CALL wrf_error_fatal3("",1345,&
' Sorry, error 1 in call to OPTFIL ')
	ENDIF


        JTYPE = 1
        NBANDS = 2

        LGRID = 16


	  DT = ABS(DELTAT)
          FS = DT/(TAUS*3600.)
          FP = DT/(TAUP*3600.)
          IF(FS.GT.0.5) then

       CALL wrf_error_fatal3("",1361,&
' FS too large in OPTFIL ')

          end if
          IF(FP.LT.0.0) then

       CALL wrf_error_fatal3("",1367,&
' FP too small in OPTFIL ')

          end if


          WTP = 1.0
          WTS = 1.0










          IF ( LPRINT ) THEN
               TAUC = 2./((1/TAUS)+(1/TAUP))
               DTAU = 2./((1/TAUS)-(1/TAUP))
               FC = DT/(TAUC*3600.)
               DF = DT/(DTAU*3600.)
               WRITE(6,*) ' DT ' , dt
               WRITE(6,*) ' TAUS, TAUP ' , TAUS,TAUP
               WRITE(6,*) ' TAUC, DTAU ' , TAUC,DTAU
               WRITE(6,*) '   FP,   FS ' ,   FP,  FS   
               WRITE(6,*) '   FC,   DF ' ,   FC,  DF
               WRITE(6,*) '  WTS,  WTP ' ,  WTS, WTP
          ENDIF


        EDGE(1) = 0.0
        EDGE(2) = FP
        EDGE(3) = FS
        EDGE(4) = 0.5
        FX(1) = 1.0
        FX(2) = 0.0
        WTX(1) = WTP
        WTX(2) = WTS

        CALL MCCPAR(NFILT,JTYPE,NBANDS,LPRINT,LGRID,       &
                   EDGE,FX,WTX,DEVIAT, h )


        DP = DEVIAT(1)
        DS = DEVIAT(2)


      IF(MOD(NFILT,2).EQ.0) THEN
         NHALF = ( NFILT )/2
      ELSE
         NHALF = (NFILT+1)/2
      ENDIF
      DO 100 nn=1,NHALF
         H(NFILT+1-nn) = h(nn)
  100 CONTINUE


        sumh = 0 
        do 150 n=1,NFILT
          sumh = sumh + H(n)
  150   continue
  print *,'SUMH =', sumh

        do 200 n=1,NFILT 
          H(n)  = H(n)/sumh
  200   continue
        do 201 n=1,NFILT
        grid%hcoeff(n)=H(n)
  201   continue


   END SUBROUTINE optfil


      SUBROUTINE MCCPAR (NFILT,JTYPE,NBANDS,LPRINT,LGRID,    &
                   EDGE,FX,WTX,DEVIAT,h )


















      DIMENSION IEXT(66),AD(66),ALPHA(66),X(66),Y(66)
      DIMENSION H(66)
      DIMENSION DES(1045),GRID(1045),WT(1045)
      DIMENSION EDGE(20),FX(10),WTX(10),DEVIAT(10)
      DOUBLE PRECISION PI2,PI
      DOUBLE PRECISION AD,DEV,X,Y
      LOGICAL LPRINT
      
      PI  = 3.141592653589793
      PI2 = 6.283185307179586



      NFMAX = 128
100      CONTINUE





      IF(NFILT.GT.NFMAX.OR.NFILT.LT.3) THEN
         CALL wrf_error_fatal3("",1484,&
' **** ERROR IN INPUT DATA ****' )
      END IF
      IF(NBANDS.LE.0) NBANDS = 1



      IF(LGRID.LE.0) LGRID = 16
      JB = 2*NBANDS



      IF(JTYPE.EQ.0) THEN
         CALL wrf_error_fatal3("",1497,&
' **** ERROR IN INPUT DATA ****' )
      END IF
      NEG = 1
      IF(JTYPE.EQ.1) NEG = 0
      NODD = NFILT/2
      NODD = NFILT-2*NODD
      NFCNS = NFILT/2
      IF(NODD.EQ.1.AND.NEG.EQ.0) NFCNS = NFCNS+1



      GRID(1) = EDGE(1)
      DELF = LGRID*NFCNS
      DELF = 0.5/DELF
      IF(NEG.EQ.0) GOTO 135
      IF(EDGE(1).LT.DELF) GRID(1) = DELF
135      CONTINUE
      J = 1
      L = 1
      LBAND = 1
140      FUP = EDGE(L+1)
145      TEMP = GRID(J)



      DES(J) = EFF(TEMP,FX,WTX,LBAND,JTYPE)
      WT(J) = WATE(TEMP,FX,WTX,LBAND,JTYPE)
      J = J+1
      GRID(J) = TEMP+DELF
      IF(GRID(J).GT.FUP) GOTO 150
      GOTO 145
150      GRID(J-1) = FUP
      DES(J-1) = EFF(FUP,FX,WTX,LBAND,JTYPE)
      WT(J-1) = WATE(FUP,FX,WTX,LBAND,JTYPE)
      LBAND = LBAND+1
      L = L+2
      IF(LBAND.GT.NBANDS) GOTO 160
      GRID(J) = EDGE(L)
      GOTO 140
160      NGRID = J-1
      IF(NEG.NE.NODD) GOTO 165
      IF(GRID(NGRID).GT.(0.5-DELF)) NGRID = NGRID-1
165      CONTINUE



      IF(NEG) 170,170,180
170      IF(NODD.EQ.1) GOTO 200
      DO 175 J=1,NGRID
            CHANGE = DCOS(PI*GRID(J))
            DES(J) = DES(J)/CHANGE
            WT(J) = WT(J)*CHANGE
175      CONTINUE
      GOTO 200
180      IF(NODD.EQ.1) GOTO 190
      DO 185 J = 1,NGRID
            CHANGE = DSIN(PI*GRID(J))
            DES(J) = DES(J)/CHANGE
            WT(J)  = WT(J)*CHANGE
185      CONTINUE
      GOTO 200
190      DO 195 J =1,NGRID
            CHANGE = DSIN(PI2*GRID(J))
            DES(J) = DES(J)/CHANGE
            WT(J)  = WT(J)*CHANGE
195      CONTINUE



200      TEMP = FLOAT(NGRID-1)/FLOAT(NFCNS)
      DO 210 J = 1,NFCNS
            IEXT(J) = (J-1)*TEMP+1
210      CONTINUE
      IEXT(NFCNS+1) = NGRID
      NM1 = NFCNS-1
      NZ  = NFCNS+1



      CALL REMEZ(EDGE,NBANDS,PI2,AD,DEV,X,Y,GRID,DES,WT,ALPHA,IEXT,NFCNS,NGRID)



      IF(NEG) 300,300,320
300      IF(NODD.EQ.0) GOTO 310
      DO 305 J=1,NM1
            H(J) = 0.5*ALPHA(NZ-J)
305      CONTINUE
      H(NFCNS)=ALPHA(1)
      GOTO 350
310      H(1) = 0.25*ALPHA(NFCNS)
      DO 315 J = 2,NM1
            H(J) = 0.25*(ALPHA(NZ-J)+ALPHA(NFCNS+2-J))
315      CONTINUE
      H(NFCNS) = 0.5*ALPHA(1)+0.25*ALPHA(2)
      GOTO 350
320      IF(NODD.EQ.0) GOTO 330
      H(1) = 0.25*ALPHA(NFCNS)
      H(2) = 0.25*ALPHA(NM1)
      DO 325 J = 3,NM1
            H(J) = 0.25*(ALPHA(NZ-J)-ALPHA(NFCNS+3-J))
325      CONTINUE
      H(NFCNS) = 0.5*ALPHA(1)-0.25*ALPHA(3)
      H(NZ) = 0.0
      GOTO 350
330      H(1) = 0.25*ALPHA(NFCNS)
      DO 335 J =2,NM1
            H(J) = 0.25*(ALPHA(NZ-J)-ALPHA(NFCNS+2-J))
335      CONTINUE
      H(NFCNS) = 0.5*ALPHA(1)-0.25*ALPHA(2)



350   CONTINUE

      IF(LPRINT) THEN

         print *, '****************************************************'
         print *, 'FINITE IMPULSE RESPONSE (FIR)'
         print *, 'LINEAR PHASE DIGITAL FILTER DESIGN'
         print *, 'REMEZ EXCHANGE ALGORITHM'
      
      IF(JTYPE.EQ.1) WRITE(6,365)
365      FORMAT(25X,'BANDPASS FILTER'/)

      IF(JTYPE.EQ.2) WRITE(6,370)
370      FORMAT(25X,'DIFFERENTIATOR '/)

      IF(JTYPE.EQ.3) WRITE(6,375)
375      FORMAT(25X,'HILBERT TRANSFORMER '/)

      WRITE(6,378) NFILT
378      FORMAT(15X,'FILTER LENGTH =',I3/)

      WRITE(6,380)
380      FORMAT(15X,'***** IMPULSE RESPONSE *****')

      DO 381 J = 1,NFCNS
            K = NFILT+1-J
            IF(NEG.EQ.0) WRITE(6,382) J,H(J),K
            IF(NEG.EQ.1) WRITE(6,383) J,H(J),K
381      CONTINUE
382      FORMAT(20X,'H(',I3,') = ',E15.8,' = H(',I4,')')
383      FORMAT(20X,'H(',I3,') = ',E15.8,' = -H(',I4,')')

      IF(NEG.EQ.1.AND.NODD.EQ.1) WRITE(6,384) NZ
384      FORMAT(20X,'H(',I3,') = 0.0')

      DO 450 K=1,NBANDS,4
            KUP = K+3
            IF(KUP.GT.NBANDS) KUP = NBANDS
	    print *
            WRITE(6,385) (J,J=K,KUP)
385            FORMAT(24X,4('BAND',I3,8X))
            WRITE(6,390) (EDGE(2*J-1),J=K,KUP)
390            FORMAT(2X,'LOWER BAND EDGE',5F15.8)
            WRITE(6,395) (EDGE(2*J),J=K,KUP)
395            FORMAT(2X,'UPPER BAND EDGE',5F15.8)
            IF(JTYPE.NE.2) WRITE(6,400) (FX(J),J=K,KUP)
400            FORMAT(2X,'DESIRED VALUE',2X,5F15.8)
            IF(JTYPE.EQ.2) WRITE(6,405) (FX(J),J=K,KUP)
405            FORMAT(2X,'DESIRED SLOPE',2X,5F15.8)
            WRITE(6,410) (WTX(J),J=K,KUP)
410            FORMAT(2X,'WEIGHTING',6X,5F15.8)
            DO 420 J = K,KUP
                  DEVIAT(J) = DEV/WTX(J)
420            CONTINUE
            WRITE(6,425) (DEVIAT(J),J=K,KUP)
425            FORMAT(2X,'DEVIATION',6X,5F15.8)
            IF(JTYPE.NE.1) GOTO 450
            DO 430 J = K,KUP
                  DEVIAT(J) = 20.0*ALOG10(DEVIAT(J))
430            CONTINUE
            WRITE(6,435) (DEVIAT(J),J=K,KUP)
435            FORMAT(2X,'DEVIATION IN DB',5F15.8)
450      CONTINUE
      print *, 'EXTREMAL FREQUENCIES'
      WRITE(6,455) (GRID(IEXT(J)),J=1,NZ)
455      FORMAT((2X,5F15.7))
      WRITE(6,460)
460      FORMAT(1X,70(1H*))

      ENDIF



   END SUBROUTINE mccpar


      FUNCTION EFF(TEMP,FX,WTX,LBAND,JTYPE)
         DIMENSION FX(5),WTX(5)
         IF(JTYPE.EQ.2) GOTO 1
         EFF = FX(LBAND)
         RETURN
1        EFF = FX(LBAND)*TEMP
      END FUNCTION eff


      FUNCTION WATE(TEMP,FX,WTX,LBAND,JTYPE)
         DIMENSION FX(5),WTX(5)
         IF(JTYPE.EQ.2) GOTO 1
         WATE = WTX(LBAND)
         RETURN
1        IF(FX(LBAND).LT.0.0001) GOTO 2
         WATE = WTX(LBAND)/TEMP
         RETURN
2        WATE = WTX(LBAND)
      END FUNCTION wate







      
      SUBROUTINE REMEZ(EDGE,NBANDS,PI2,AD,DEV,X,Y,GRID,DES,WT,ALPHA,IEXT,NFCNS,NGRID)












      DIMENSION EDGE(20)
      DIMENSION IEXT(66),AD(66),ALPHA(66),X(66),Y(66)
      DIMENSION DES(1045),GRID(1045),WT(1045)
      DIMENSION A(66),P(65),Q(65)
      DOUBLE PRECISION PI2,DNUM,DDEN,DTEMP,A,P,Q
      DOUBLE PRECISION AD,DEV,X,Y
      DOUBLE PRECISION, EXTERNAL :: D, GEE



      ITRMAX=25
      DEVL=-1.0
      NZ=NFCNS+1
      NZZ=NFCNS+2
      NITER=0
  100 CONTINUE
      IEXT(NZZ)=NGRID+1
      NITER=NITER+1
      IF(NITER.GT.ITRMAX) GO TO 400
      DO 110 J=1,NZ
      DTEMP=GRID(IEXT(J))
      DTEMP=DCOS(DTEMP*PI2)
  110 X(J)=DTEMP
      JET=(NFCNS-1)/15+1
      DO 120 J=1,NZ
  120 AD(J)=D(J,NZ,JET,X)
      DNUM=0.0
      DDEN=0.0
      K=1
      DO 130 J=1,NZ
      L=IEXT(J)
      DTEMP=AD(J)*DES(L)
      DNUM=DNUM+DTEMP
      DTEMP=K*AD(J)/WT(L)
      DDEN=DDEN+DTEMP
  130 K=-K
      DEV=DNUM/DDEN
      NU=1
      IF(DEV.GT.0.0) NU=-1
      DEV=-NU*DEV
      K=NU
      DO 140 J=1,NZ
      L=IEXT(J)
      DTEMP=K*DEV/WT(L)
      Y(J)=DES(L)+DTEMP
  140 K=-K
      IF(DEV.GE.DEVL) GO TO 150
      WRITE(6,*) ' ******** FAILURE TO CONVERGE *********** '
      WRITE(6,*) ' PROBABLE CAUSE IS MACHINE ROUNDING ERROR '
      WRITE(6,*) ' THE IMPULSE RESPONSE MAY BE CORRECT '
      WRITE(6,*) ' CHECK WITH A FREQUENCY RESPONSE '
      WRITE(6,*) ' **************************************** '
      GO TO 400
  150 DEVL=DEV
      JCHNGE=0
      K1=IEXT(1)
      KNZ=IEXT(NZ)
      KLOW=0
      NUT=-NU
      J=1




  200 IF(J.EQ.NZZ) YNZ=COMP
      IF(J.GE.NZZ) GO TO 300
      KUP=IEXT(J+1)
      L=IEXT(J)+1
      NUT=-NUT
      IF(J.EQ.2) Y1=COMP
      COMP=DEV
      IF(L.GE.KUP) GO TO 220
      ERR=GEE(L,NZ,GRID,PI2,X,Y,AD)
      ERR=(ERR-DES(L))*WT(L)
      DTEMP=NUT*ERR-COMP
      IF(DTEMP.LE.0.0) GO TO 220
      COMP=NUT*ERR
  210 L=L+1
      IF(L.GE.KUP) GO TO 215
      ERR=GEE(L,NZ,GRID,PI2,X,Y,AD)
      ERR=(ERR-DES(L))*WT(L)
      DTEMP=NUT*ERR-COMP
      IF(DTEMP.LE.0.0) GO TO 215
      COMP=NUT*ERR
      GO TO 210
  215 IEXT(J)=L-1
      J=J+1
      KLOW=L-1
      JCHNGE=JCHNGE+1
      GO TO 200
  220 L=L-1
  225 L=L-1
      IF(L.LE.KLOW) GO TO 250
      ERR=GEE(L,NZ,GRID,PI2,X,Y,AD)
      ERR=(ERR-DES(L))*WT(L)
      DTEMP=NUT*ERR-COMP
      IF(DTEMP.GT.0.0) GO TO 230
      IF(JCHNGE.LE.0) GO TO 225
      GO TO 260
  230 COMP=NUT*ERR
  235 L=L-1
      IF(L.LE.KLOW) GO TO 240
      ERR=GEE(L,NZ,GRID,PI2,X,Y,AD)
      ERR=(ERR-DES(L))*WT(L)
      DTEMP=NUT*ERR-COMP
      IF(DTEMP.LE.0.0) GO TO 240
      COMP=NUT*ERR
      GO TO 235
  240 KLOW=IEXT(J)
      IEXT(J)=L+1
      J=J+1
      JCHNGE=JCHNGE+1
      GO TO 200
  250 L=IEXT(J)+1
      IF(JCHNGE.GT.0) GO TO 215
  255 L=L+1
      IF(L.GE.KUP) GO TO 260
      ERR=GEE(L,NZ,GRID,PI2,X,Y,AD)
      ERR=(ERR-DES(L))*WT(L)
      DTEMP=NUT*ERR-COMP
      IF(DTEMP.LE.0.0) GO TO 255
      COMP=NUT*ERR
      GO TO 210
  260 KLOW=IEXT(J)
      J=J+1
      GO TO 200
  300 IF(J.GT.NZZ) GO TO 320
      IF(K1.GT.IEXT(1)) K1=IEXT(1)
      IF(KNZ.LT.IEXT(NZ)) KNZ=IEXT(NZ)
      NUT1=NUT
      NUT=-NU
      L=0
      KUP=K1
      COMP=YNZ*(1.00001)
      LUCK=1
  310 L=L+1
      IF(L.GE.KUP) GO TO 315
      ERR=GEE(L,NZ,GRID,PI2,X,Y,AD)
      ERR=(ERR-DES(L))*WT(L)
      DTEMP=NUT*ERR-COMP
      IF(DTEMP.LE.0.0) GO TO 310
      COMP=NUT*ERR
      J=NZZ
      GO TO 210
  315 LUCK=6
      GO TO 325
  320 IF(LUCK.GT.9) GO TO 350
      IF(COMP.GT.Y1) Y1=COMP
      K1=IEXT(NZZ)
  325 L=NGRID+1
      KLOW=KNZ
      NUT=-NUT1
      COMP=Y1*(1.00001)
  330 L=L-1
      IF(L.LE.KLOW) GO TO 340
      ERR=GEE(L,NZ,GRID,PI2,X,Y,AD)
      ERR=(ERR-DES(L))*WT(L)
      DTEMP=NUT*ERR-COMP
      IF(DTEMP.LE.0.0) GO TO 330
      J=NZZ
      COMP=NUT*ERR
      LUCK=LUCK+10
      GO TO 235
  340 IF(LUCK.EQ.6) GO TO 370
      DO 345 J=1,NFCNS
  345 IEXT(NZZ-J)=IEXT(NZ-J)
      IEXT(1)=K1
      GO TO 100
  350 KN=IEXT(NZZ)
      DO 360 J=1,NFCNS
  360 IEXT(J)=IEXT(J+1)
      IEXT(NZ)=KN
      GO TO 100
  370 IF(JCHNGE.GT.0) GO TO 100




  400 CONTINUE
      NM1=NFCNS-1
      FSH=1.0E-06
      GTEMP=GRID(1)
      X(NZZ)=-2.0
      CN=2*NFCNS-1
      DELF=1.0/CN
      L=1
      KKK=0
      IF(EDGE(1).EQ.0.0.AND.EDGE(2*NBANDS).EQ.0.5) KKK=1
      IF(NFCNS.LE.3) KKK=1
      IF(KKK.EQ.1) GO TO 405
      DTEMP=DCOS(PI2*GRID(1))
      DNUM=DCOS(PI2*GRID(NGRID))
      AA=2.0/(DTEMP-DNUM)
      BB=-(DTEMP+DNUM)/(DTEMP-DNUM)
  405 CONTINUE
      DO 430 J=1,NFCNS
      FT=(J-1)*DELF
      XT=DCOS(PI2*FT)
      IF(KKK.EQ.1) GO TO 410
      XT=(XT-BB)/AA

      FT=ACOS(XT)/PI2
  410 XE=X(L)
      IF(XT.GT.XE) GO TO 420
      IF((XE-XT).LT.FSH) GO TO 415
      L=L+1
      GO TO 410
  415 A(J)=Y(L)
      GO TO 425
  420 IF((XT-XE).LT.FSH) GO TO 415
      GRID(1)=FT
      A(J)=GEE(1,NZ,GRID,PI2,X,Y,AD)
  425 CONTINUE
      IF(L.GT.1) L=L-1
  430 CONTINUE
      GRID(1)=GTEMP
      DDEN=PI2/CN
      DO 510 J=1,NFCNS
      DTEMP=0.0
      DNUM=(J-1)*DDEN
      IF(NM1.LT.1) GO TO 505
      DO 500 K=1,NM1
  500 DTEMP=DTEMP+A(K+1)*DCOS(DNUM*K)
  505 DTEMP=2.0*DTEMP+A(1)
  510 ALPHA(J)=DTEMP
      DO 550 J=2,NFCNS
  550 ALPHA(J)=2*ALPHA(J)/CN
      ALPHA(1)=ALPHA(1)/CN
      IF(KKK.EQ.1) GO TO 545
      P(1)=2.0*ALPHA(NFCNS)*BB+ALPHA(NM1)
      P(2)=2.0*AA*ALPHA(NFCNS)
      Q(1)=ALPHA(NFCNS-2)-ALPHA(NFCNS)
      DO 540 J=2,NM1
      IF(J.LT.NM1) GO TO 515
      AA=0.5*AA
      BB=0.5*BB
  515 CONTINUE
      P(J+1)=0.0
      DO 520 K=1,J
      A(K)=P(K)
  520 P(K)=2.0*BB*A(K)
      P(2)=P(2)+A(1)*2.0*AA
      JM1=J-1
      DO 525 K=1,JM1
  525 P(K)=P(K)+Q(K)+AA*A(K+1)
      JP1=J+1
      DO 530 K=3,JP1
  530 P(K)=P(K)+AA*A(K-1)
      IF(J.EQ.NM1) GO TO 540
      DO 535 K=1,J
  535 Q(K)=-A(K)
      Q(1)=Q(1)+ALPHA(NFCNS-1-J)
  540 CONTINUE
      DO 543 J=1,NFCNS
  543 ALPHA(J)=P(J)
  545 CONTINUE
      IF(NFCNS.GT.3) RETURN
      ALPHA(NFCNS+1)=0.0
      ALPHA(NFCNS+2)=0.0
   END SUBROUTINE remez

   DOUBLE PRECISION FUNCTION D(K,N,M,X)

      DIMENSION IEXT(66),AD(66),ALPHA(66),X(66),Y(66)
      DIMENSION DES(1045),GRID(1045),WT(1045)
      DOUBLE PRECISION AD,DEV,X,Y
      DOUBLE PRECISION Q
      DOUBLE PRECISION PI2
      D = 1.0
      Q = X(K)
      DO 3 L = 1,M
      DO 2 J = L,N,M
            IF(J-K) 1,2,1
1                  D = 2.0*D*(Q-X(J))
2      CONTINUE
3      CONTINUE
      D = 1.0/D
   END FUNCTION D


   DOUBLE PRECISION FUNCTION GEE(K,N,GRID,PI2,X,Y,AD)

      DIMENSION IEXT(66),AD(66),ALPHA(66),X(66),Y(66)
      DIMENSION DES(1045),GRID(1045),WT(1045)
      DOUBLE PRECISION AD,DEV,X,Y
      DOUBLE PRECISION P,C,D,XF
      DOUBLE PRECISION PI2
      P = 0.0
      XF = GRID(K)
      XF = DCOS(PI2*XF)
      D = 0.0
      DO 1 J =1,N
            C = XF-X(J)
            C = AD(J)/C
            D = D+C
            P = P+C*Y(J)
1      CONTINUE
      GEE = P/D
   END FUNCTION GEE


