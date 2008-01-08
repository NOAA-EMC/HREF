! TBH:  This version is for use with the ESMF library embedded in the WRF 
! TBH:  distribution.  
MODULE ESMF_Mod
   USE esmf_alarmmod
   USE esmf_basemod
   USE esmf_basetimemod
   USE esmf_calendarmod
   USE esmf_clockmod
   USE esmf_fractionmod
   USE esmf_timeintervalmod
   USE esmf_timemod
   USE esmf_alarmclockmod
   USE esmf_stubs   ! add new dummy interfaces and typedefs here as needed










! Note that MAX_ALARMS must match MAX_WRF_ALARMS defined in 
! ../../frame/module_domain.F !!!  Eliminate this dependence with 
! grow-as-you-go AlarmList in ESMF_Clock...  

! TBH:  TODO:  Hook this into the WRF build so WRF can use either "no-leap" or 
! TBH:         Gregorian calendar.  Now WRF is hard-wired to use Gregorian.  

! TBH:  When NO_DT_COMPONENT_INIT is set, code that uses F95 compile-time 
! TBH:  initialization of components of derived types is not included.  
! TBH:  Some older compilers like PGI 5.x do not support this F95 
! TBH:  feature.

   INTEGER, PARAMETER :: ESMF_MAX_ALARMS=28
!
END MODULE ESMF_Mod
