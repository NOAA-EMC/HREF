!WRF:DRIVER_LAYER:UTIL
!

MODULE module_timing

   INTEGER, PARAMETER :: cnmax = 30
   INTEGER, DIMENSION(cnmax) :: count_int1 , count_rate_int1 , count_max_int1
   INTEGER, DIMENSION(cnmax) :: count_int2 , count_rate_int2 , count_max_int2
   INTEGER :: cn = 0 
   REAL    :: elapsed_seconds , elapsed_seconds_total = 0
   REAL    :: cpu_1 , cpu_2 , cpu_seconds , cpu_seconds_total = 0

CONTAINS

   SUBROUTINE init_module_timing
      cn = 0
   END SUBROUTINE init_module_timing

END MODULE module_timing

   SUBROUTINE start_timing
      USE module_timing

      IMPLICIT NONE

      cn = cn + 1
      IF ( cn .gt. cnmax ) THEN
        CALL wrf_message( 'module_timing: clock nesting error (too many nests)' )
        RETURN
      ENDIF
      CALL SYSTEM_CLOCK ( count_int1(cn) , count_rate_int1(cn) , count_max_int1(cn) )
!     CALL CPU_TIME ( cpu_1 )

   END SUBROUTINE start_timing


   SUBROUTINE end_timing ( string )
      USE module_timing
   
      IMPLICIT NONE

      CHARACTER *(*) :: string

      IF ( cn .lt. 1 ) THEN
        CALL wrf_message( 'module_timing: clock nesting error' ) 
        RETURN
      ENDIF

      CALL SYSTEM_CLOCK ( count_int2(cn) , count_rate_int2(cn) , count_max_int2(cn) )
!     CALL CPU_TIME ( cpu_2 )

      IF ( count_int2(cn) < count_int1(cn) ) THEN
         count_int2(cn) = count_int2(cn) + count_max_int2(cn)
      ENDIF

      count_int2(cn) = count_int2(cn) - count_int1(cn)
      elapsed_seconds = REAL(count_int2(cn)) / REAL(count_rate_int2(cn))
      elapsed_seconds_total = elapsed_seconds_total + elapsed_seconds
!      PRINT (A,A,A,F10.5,A) ,Timing for ,TRIM(string),: ,elapsed_seconds, elapsed seconds.
!     WRITE(6,(A,A,A,F10.5,A)) Timing for ,TRIM(string),: ,elapsed_seconds, elapsed seconds.
!     WRITE(0,(A,A,A,F10.5,A)) Timing for ,TRIM(string),: ,elapsed_seconds, elapsed seconds.

!     cpu_seconds = cpu_2 - cpu_1
!     cpu_seconds_total = cpu_seconds_total + cpu_seconds
!     PRINT (A,A,A,F10.5,A) ,Timing for ,TRIM(string),: ,cpu_seconds, cpu seconds.

      cn = cn - 1

   END SUBROUTINE end_timing

