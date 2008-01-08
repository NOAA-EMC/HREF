SUBROUTINE Set_Timekeeping ( grid )
   USE module_domain
   USE module_configure
   USE esmf_mod
   IMPLICIT NONE
   TYPE(domain), POINTER :: grid
! Local
   TYPE(ESMF_TimeInterval) :: interval, run_length
   INTEGER :: start_year,start_month,start_day,start_hour,start_minute,start_second
   INTEGER :: end_year,end_month,end_day,end_hour,end_minute,end_second
   INTEGER :: history_interval,auxhist1_interval,auxhist2_interval,auxhist3_interval, &
	      auxhist4_interval,auxhist5_interval,restart_interval
   INTEGER :: run_days, run_hours, run_minutes, run_seconds
   INTEGER :: time_step, time_step_fract_num, time_step_fract_den
   INTEGER :: rc
   REAL    :: dt
   CALL get_start_year(grid%id,start_year)
   CALL get_start_month(grid%id,start_month)
   CALL get_start_day(grid%id,start_day)
   CALL get_start_hour(grid%id,start_hour)
   CALL get_start_minute(grid%id,start_minute)
   CALL get_start_second(grid%id,start_second)
   CALL ESMF_TimeSet(grid%start_time, YR=start_year, MM=start_month, DD=start_day, &
                                      H=start_hour, M=start_minute, S=start_second )
   CALL get_run_days(run_days)
   CALL get_run_hours(run_hours)
   CALL get_run_minutes(run_minutes)
   CALL get_run_seconds(run_seconds)
   IF ( run_days .gt. 0 .or. run_hours .gt. 0 .or. run_minutes .gt. 0 .or. run_seconds .gt. 0 ) THEN
     CALL ESMF_TimeIntervalSet ( run_length , D=run_days, H=run_hours, M=run_minutes, S=run_seconds, rc=rc )
     grid%stop_time = grid%start_time + run_length
write(0,*)'set run interval ',run_days, run_hours, run_minutes, run_seconds
   ELSE
     CALL get_end_year(grid%id,end_year)
     CALL get_end_month(grid%id,end_month)
     CALL get_end_day(grid%id,end_day)
     CALL get_end_hour(grid%id,end_hour)
     CALL get_end_minute(grid%id,end_minute)
     CALL get_end_second(grid%id,end_second)
     CALL ESMF_TimeSet(grid%stop_time, YR=end_year, MM=end_month, DD=end_day, &
                                       H=end_hour, M=end_minute, S=end_second )
   ENDIF

   IF ( grid%id .EQ. head_grid%id ) THEN
      CALL get_time_step ( time_step )
      CALL get_time_step_fract_num( time_step_fract_num )
      CALL get_time_step_fract_den( time_step_fract_den )
      dt = real(time_step) + real(time_step_fract_num) / real(time_step_fract_den)
      CALL set_dt( grid%id, dt )
      grid%dt = dt
      CALL ESMF_TimeIntervalSet(head_grid%step_time, S=time_step, Sn=time_step_fract_num, Sd=time_step_fract_den, rc=rc)
   ELSE
      grid%step_time = grid%parents(1)%ptr%step_time / grid%parent_time_step_ratio
   ENDIF

   CALL ESMF_ClockSet( grid%domain_clock,TimeStep=grid%step_time,StartTime=grid%start_time,  &
					 StopTime=grid%stop_time,rc=rc)
   CALL ESMF_ClockGetAlarmList( grid%domain_clock, grid%alarms, rc )

   CALL get_history_interval( grid%id, history_interval )
   IF ( history_interval .GE. 0 ) THEN
     CALL ESMF_TimeIntervalSet( interval, M=history_interval, rc=rc )
	write(0,*) 'returned from ESMF_TimeIntervalSet with interval: ', interval
     CALL ESMF_AlarmSet( grid%alarms( HISTORY_ALARM ), RingInterval=interval, rc=rc )
     CALL ESMF_ClockAddAlarm( grid%domain_clock, grid%alarms( HISTORY_ALARM ), rc=rc )
   ENDIF
   CALL get_auxhist1_interval( grid%id, auxhist1_interval )
   IF ( auxhist1_interval .GE. 0 ) THEN
     CALL ESMF_TimeIntervalSet( interval, M=auxhist1_interval, rc=rc )
     CALL ESMF_AlarmSet( grid%alarms( AUXHIST1_ALARM ), RingInterval=interval, rc=rc )
     CALL ESMF_ClockAddAlarm( grid%domain_clock, grid%alarms( AUXHIST1_ALARM ), rc=rc )
   ENDIF
   CALL get_auxhist2_interval( grid%id, auxhist2_interval )
   IF ( auxhist2_interval .GE. 0 ) THEN
     CALL ESMF_TimeIntervalSet( interval, M=auxhist2_interval, rc=rc )
     CALL ESMF_AlarmSet( grid%alarms( AUXHIST2_ALARM ), RingInterval=interval, rc=rc )
     CALL ESMF_ClockAddAlarm( grid%domain_clock, grid%alarms( AUXHIST2_ALARM ), rc=rc )
   ENDIF
   CALL get_auxhist3_interval( grid%id, auxhist3_interval )
   IF ( auxhist3_interval .GE. 0 ) THEN
     CALL ESMF_TimeIntervalSet( interval, M=auxhist3_interval, rc=rc )
     CALL ESMF_AlarmSet( grid%alarms( AUXHIST3_ALARM ), RingInterval=interval, rc=rc )
     CALL ESMF_ClockAddAlarm( grid%domain_clock, grid%alarms( AUXHIST3_ALARM ), rc=rc )
   ENDIF
   CALL get_auxhist4_interval( grid%id, auxhist4_interval )
   IF ( auxhist4_interval .GE. 0 ) THEN
     CALL ESMF_TimeIntervalSet( interval, M=auxhist4_interval, rc=rc )
     CALL ESMF_AlarmSet( grid%alarms( AUXHIST4_ALARM ), RingInterval=interval, rc=rc )
     CALL ESMF_ClockAddAlarm( grid%domain_clock, grid%alarms( AUXHIST4_ALARM ), rc=rc )
   ENDIF
   CALL get_auxhist5_interval( grid%id, auxhist5_interval )
   IF ( auxhist5_interval .GE. 0 ) THEN
     CALL ESMF_TimeIntervalSet( interval, M=auxhist5_interval, rc=rc )
     CALL ESMF_AlarmSet( grid%alarms( AUXHIST5_ALARM ), RingInterval=interval, rc=rc )
     CALL ESMF_ClockAddAlarm( grid%domain_clock, grid%alarms( AUXHIST5_ALARM ), rc=rc )
   ENDIF
   CALL get_restart_interval( restart_interval )
	write(0,*) 'restart_interval is: ', restart_interval
   IF ( restart_interval .GE. 0 ) THEN
     CALL ESMF_TimeIntervalSet( interval, M=restart_interval, rc=rc )
	write(0,*) 'return from ESMF_TimeIntervalSet with rc: ', rc
	write(0,*) 'return interval is: ', interval
     CALL ESMF_AlarmSet( grid%alarms( RESTART_ALARM ), RingInterval=interval, rc=rc )
	write(0,*) 'return from ESMF_AlarmSet with rc: ', rc
     CALL ESMF_ClockAddAlarm( grid%domain_clock, grid%alarms( RESTART_ALARM ), rc=rc )
	write(0,*) 'return from ESMF_ClockAddAlarm with rc: ', rc

!!! turning it on here generates the 0 h restart
     CALL ESMF_AlarmTurnOn( grid%alarms( RESTART_ALARM ),  rc=rc )

   ENDIF

   CALL ESMF_AlarmEnable(grid%alarms( BOUNDARY_ALARM ), rc=rc )
   CALL ESMF_ClockAddAlarm( grid%domain_clock, grid%alarms( BOUNDARY_ALARM ), rc=rc )

   CALL ESMF_AlarmTurnOn( grid%alarms( BOUNDARY_ALARM ),  rc=rc )
   CALL ESMF_AlarmTurnOn( grid%alarms( HISTORY_ALARM ),  rc=rc )

END SUBROUTINE Set_Timekeeping
