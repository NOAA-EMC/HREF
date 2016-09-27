      MODULE wam_f107_kp_mod

      IMPLICIT none

      INTEGER                     :: f107_kp_size, f107_kp_interval
      INTEGER                     :: f107_kp_skip_size
      INTEGER                     :: f107_kp_read_in_size
      INTEGER                     :: kdt_3h
      REAL, POINTER, DIMENSION(:) :: f107, kp

      CONTAINs

      SUBROUTINE read_wam_f107_kp_txt

! Subprogram:  read_wam_f107_kp_txt   read-in the inputted f10.7 and kp data. 
!   Prgmmr: Weiyu Yang          Date: 2015-10-19
!
      CHARACTER*20 :: issuedate, realdate(f107_kp_size)
      CHARACTER*20 :: realdate_work
      INTEGER      :: i, j
      INTEGER      :: f107_flag_work, kp_flag_work 
      REAL         :: f107_81d_avg
      REAL         :: f107_work, kp_work

! Flags:   0=Forecast, 1=Estimated, 2=Observed
      INTEGER, DIMENSION(f107_kp_size) :: f107_flag, kp_flag 

! Skip the observation data before the forecast starting.
!--------------------------------------------------------
      OPEN(79, FILE='wam_input_f107_kp.txt', FORM='formatted')
      REWIND 79
      READ(79, 1000) issuedate
      READ(79, 1001) f107_81d_avg
      DO i = 1, 4
          READ(79, *)    
      END DO

      DO i = 1, f107_kp_skip_size
          READ(79, *) realdate_work, f107_work, kp_work,       &
               f107_flag_work, kp_flag_work
      END DO
      
      f107_kp_read_in_size = f107_kp_size - f107_kp_skip_size
      DO i = 1, f107_kp_read_in_size
          READ(79, *) realdate(i), f107(i), kp(i),             &
               f107_flag(i), kp_flag(i)
      END DO
      CLOSE(79)
1000  FORMAT(20x, a20)
1001  FORMAT(20x, f3.0)
      PRINT*, 'issuedate=', issuedate
      PRINT*, 'f107_81d_avg=', f107_81d_avg
      DO i = 1, f107_kp_read_in_size
          PRINT*, i, f107(i), kp(i), f107_flag(i), kp_flag(i)
      END DO
      END SUBROUTINE read_wam_f107_kp_txt

      END MODULE wam_f107_kp_mod
