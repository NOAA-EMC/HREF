!WRF:DRIVER_LAYER:IO
!
!#define mpi_x_comm_size(i,j,k)  Mpi_Comm_Size ( i,j,k ) ; write(0,*) 4

! (old comment from when this file was a template)
! This is a template for adding a package-dependent implementation of
! the I/O API.  You can use the name xxx since that is already set up
! as a placeholder in module_io.F, md_calls.m4, and the Registry, or
! you can change the name here and in those other places.  For additional
! information on adding a package to WRF, see the latest version of the
! WRF Design and Implementation Document 1.1 (Draft). June 21, 2001
!

MODULE module_ext_quilt
  INTEGER, PARAMETER :: int_num_handles = 99
  LOGICAL, DIMENSION(int_num_handles) :: okay_to_write, int_handle_in_use, okay_to_commit
  INTEGER, DIMENSION(int_num_handles) :: int_num_bytes_to_write, io_form
  REAL, POINTER    :: int_local_output_buffer(:)
  INTEGER          :: int_local_output_cursor
  LOGICAL          :: quilting_enabled
  LOGICAL          :: disable_quilt = .FALSE.


  CONTAINS


    !--- ioinit
    SUBROUTINE init_module_ext_quilt
      RETURN
    END SUBROUTINE init_module_ext_quilt
END MODULE module_ext_quilt

! Call this in programs that you never want to be quilting (e.g. real)
! Must call before call to init_module_ext_quilt
!
SUBROUTINE disable_quilting
  USE module_ext_quilt
  disable_quilt = .TRUE.
  RETURN
END SUBROUTINE disable_quilting

LOGICAL FUNCTION  use_output_servers()
  USE module_ext_quilt
  use_output_servers = quilting_enabled
  RETURN
END FUNCTION use_output_servers

LOGICAL FUNCTION  use_input_servers()
  USE module_ext_quilt
  use_input_servers = .FALSE.
  RETURN
END FUNCTION use_input_servers

!--- open_for_write_begin
SUBROUTINE ext_quilt_open_for_write_begin( FileName , Comm_compute, Comm_io, SysDepInfo, &
                                     DataHandle , io_form_arg, Status )
  RETURN  
END SUBROUTINE ext_quilt_open_for_write_begin

!--- open_for_write_commit
SUBROUTINE ext_quilt_open_for_write_commit( DataHandle , Status )
  RETURN  
END SUBROUTINE ext_quilt_open_for_write_commit

!--- open_for_read 
SUBROUTINE ext_quilt_open_for_read ( FileName , Comm_compute, Comm_io, SysDepInfo, &
                               DataHandle , Status )
  RETURN  
END SUBROUTINE ext_quilt_open_for_read

!--- intio_nextrec  (INT_IO only)
SUBROUTINE ext_quilt_intio_nextrec ( DataHandle , NextRec , Status )
  RETURN  
END SUBROUTINE ext_quilt_intio_nextrec

!--- inquire_opened
SUBROUTINE ext_quilt_inquire_opened ( DataHandle, FileName , FileStatus, Status )
  RETURN
END SUBROUTINE ext_quilt_inquire_opened

!--- inquire_filename
SUBROUTINE ext_quilt_inquire_filename ( DataHandle, FileName , FileStatus, Status )
  RETURN
END SUBROUTINE ext_quilt_inquire_filename

!--- sync
SUBROUTINE ext_quilt_iosync ( DataHandle, Status )
  RETURN
END SUBROUTINE ext_quilt_iosync

!--- close
SUBROUTINE ext_quilt_ioclose ( DataHandle, Status )
  RETURN
END SUBROUTINE ext_quilt_ioclose

!--- ioexit
SUBROUTINE ext_quilt_ioexit( Status )
  RETURN  
END SUBROUTINE

SUBROUTINE server_io_exit( Status )
  RETURN  
END SUBROUTINE

!--- get_next_time (not defined for IntIO )
SUBROUTINE ext_quilt_get_next_time ( DataHandle, DateStr, Status )
  RETURN
END SUBROUTINE ext_quilt_get_next_time

!--- put_dom_ti_char
SUBROUTINE ext_quilt_set_time ( DataHandle, Data,  Status )
RETURN
END SUBROUTINE ext_quilt_set_time

!--- get_next_var  (not defined for IntIO)
SUBROUTINE ext_quilt_get_next_var ( DataHandle, VarName, Status )
  RETURN
END SUBROUTINE ext_quilt_get_next_var

!--- get_dom_ti_real
SUBROUTINE ext_quilt_get_dom_ti_real ( DataHandle,Element,   Data, Count, Outcount, Status )
RETURN
END SUBROUTINE ext_quilt_get_dom_ti_real 

!--- put_dom_ti_real
SUBROUTINE ext_quilt_put_dom_ti_real ( DataHandle,Element,   Data, Count,  Status )
RETURN
END SUBROUTINE ext_quilt_put_dom_ti_real 

!--- get_dom_ti_double
SUBROUTINE ext_quilt_get_dom_ti_double ( DataHandle,Element,   Data, Count, Outcount, Status )
RETURN
END SUBROUTINE ext_quilt_get_dom_ti_double 

!--- put_dom_ti_double
SUBROUTINE ext_quilt_put_dom_ti_double ( DataHandle,Element,   Data, Count,  Status )
RETURN
END SUBROUTINE ext_quilt_put_dom_ti_double 

!--- get_dom_ti_integer
SUBROUTINE ext_quilt_get_dom_ti_integer ( DataHandle,Element,   Data, Count, Outcount, Status )
RETURN
END SUBROUTINE ext_quilt_get_dom_ti_integer 

!--- put_dom_ti_integer
SUBROUTINE ext_quilt_put_dom_ti_integer ( DataHandle,Element,   Data, Count,  Status )
RETURN
END SUBROUTINE ext_quilt_put_dom_ti_integer 

!--- get_dom_ti_logical
SUBROUTINE ext_quilt_get_dom_ti_logical ( DataHandle,Element,   Data, Count, Outcount, Status )
RETURN
END SUBROUTINE ext_quilt_get_dom_ti_logical 

!--- put_dom_ti_logical
SUBROUTINE ext_quilt_put_dom_ti_logical ( DataHandle,Element,   Data, Count,  Status )
RETURN
END SUBROUTINE ext_quilt_put_dom_ti_logical 

!--- get_dom_ti_char
SUBROUTINE ext_quilt_get_dom_ti_char ( DataHandle,Element,   Data,  Status )
RETURN
END SUBROUTINE ext_quilt_get_dom_ti_char 

!--- put_dom_ti_char
SUBROUTINE ext_quilt_put_dom_ti_char ( DataHandle, Element,  Data,  Status )
RETURN
END SUBROUTINE ext_quilt_put_dom_ti_char 

!--- get_dom_td_real
SUBROUTINE ext_quilt_get_dom_td_real ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )
RETURN
END SUBROUTINE ext_quilt_get_dom_td_real 

!--- put_dom_td_real
SUBROUTINE ext_quilt_put_dom_td_real ( DataHandle,Element, DateStr,  Data, Count,  Status )
RETURN
END SUBROUTINE ext_quilt_put_dom_td_real 

!--- get_dom_td_double
SUBROUTINE ext_quilt_get_dom_td_double ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )
RETURN
END SUBROUTINE ext_quilt_get_dom_td_double 

!--- put_dom_td_double
SUBROUTINE ext_quilt_put_dom_td_double ( DataHandle,Element, DateStr,  Data, Count,  Status )
RETURN
END SUBROUTINE ext_quilt_put_dom_td_double 

!--- get_dom_td_integer
SUBROUTINE ext_quilt_get_dom_td_integer ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )
RETURN
END SUBROUTINE ext_quilt_get_dom_td_integer 

!--- put_dom_td_integer
SUBROUTINE ext_quilt_put_dom_td_integer ( DataHandle,Element, DateStr,  Data, Count,  Status )
RETURN
END SUBROUTINE ext_quilt_put_dom_td_integer 

!--- get_dom_td_logical
SUBROUTINE ext_quilt_get_dom_td_logical ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )
RETURN
END SUBROUTINE ext_quilt_get_dom_td_logical 

!--- put_dom_td_logical
SUBROUTINE ext_quilt_put_dom_td_logical ( DataHandle,Element, DateStr,  Data, Count,  Status )
RETURN
END SUBROUTINE ext_quilt_put_dom_td_logical 

!--- get_dom_td_char
SUBROUTINE ext_quilt_get_dom_td_char ( DataHandle,Element, DateStr,  Data,  Status )
RETURN
END SUBROUTINE ext_quilt_get_dom_td_char 

!--- put_dom_td_char
SUBROUTINE ext_quilt_put_dom_td_char ( DataHandle,Element, DateStr,  Data,  Status )
RETURN
END SUBROUTINE ext_quilt_put_dom_td_char 

!--- get_var_ti_real
SUBROUTINE ext_quilt_get_var_ti_real ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )
RETURN
END SUBROUTINE ext_quilt_get_var_ti_real 

!--- put_var_ti_real
SUBROUTINE ext_quilt_put_var_ti_real ( DataHandle,Element,  Varname, Data, Count,  Status )
RETURN
END SUBROUTINE ext_quilt_put_var_ti_real 

!--- get_var_ti_double
SUBROUTINE ext_quilt_get_var_ti_double ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )
RETURN
END SUBROUTINE ext_quilt_get_var_ti_double 

!--- put_var_ti_double
SUBROUTINE ext_quilt_put_var_ti_double ( DataHandle,Element,  Varname, Data, Count,  Status )
RETURN
END SUBROUTINE ext_quilt_put_var_ti_double 

!--- get_var_ti_integer
SUBROUTINE ext_quilt_get_var_ti_integer ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )
RETURN
END SUBROUTINE ext_quilt_get_var_ti_integer 

!--- put_var_ti_integer
SUBROUTINE ext_quilt_put_var_ti_integer ( DataHandle,Element,  Varname, Data, Count,  Status )
RETURN
END SUBROUTINE ext_quilt_put_var_ti_integer 

!--- get_var_ti_logical
SUBROUTINE ext_quilt_get_var_ti_logical ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )
RETURN
END SUBROUTINE ext_quilt_get_var_ti_logical 

!--- put_var_ti_logical
SUBROUTINE ext_quilt_put_var_ti_logical ( DataHandle,Element,  Varname, Data, Count,  Status )
RETURN
END SUBROUTINE ext_quilt_put_var_ti_logical 

!--- get_var_ti_char
SUBROUTINE ext_quilt_get_var_ti_char ( DataHandle,Element,  Varname, Data,  Status )
RETURN
END SUBROUTINE ext_quilt_get_var_ti_char 

!--- put_var_ti_char
SUBROUTINE ext_quilt_put_var_ti_char ( DataHandle,Element,  Varname, Data,  Status )

RETURN
END SUBROUTINE ext_quilt_put_var_ti_char 

!--- get_var_td_real
SUBROUTINE ext_quilt_get_var_td_real ( DataHandle,Element,  DateStr,Varname, Data, Count, Outcount, Status )
RETURN
END SUBROUTINE ext_quilt_get_var_td_real 

!--- put_var_td_real
SUBROUTINE ext_quilt_put_var_td_real ( DataHandle,Element,  DateStr,Varname, Data, Count,  Status )
RETURN
END SUBROUTINE ext_quilt_put_var_td_real 

!--- get_var_td_double
SUBROUTINE ext_quilt_get_var_td_double ( DataHandle,Element,  DateStr,Varname, Data, Count, Outcount, Status )
RETURN
END SUBROUTINE ext_quilt_get_var_td_double 

!--- put_var_td_double
SUBROUTINE ext_quilt_put_var_td_double ( DataHandle,Element,  DateStr,Varname, Data, Count,  Status )
RETURN
END SUBROUTINE ext_quilt_put_var_td_double 

!--- get_var_td_integer
SUBROUTINE ext_quilt_get_var_td_integer ( DataHandle,Element,  DateStr,Varname, Data, Count, Outcount,Status)
RETURN
END SUBROUTINE ext_quilt_get_var_td_integer 

!--- put_var_td_integer
SUBROUTINE ext_quilt_put_var_td_integer ( DataHandle,Element,  DateStr,Varname, Data, Count,  Status )
RETURN
END SUBROUTINE ext_quilt_put_var_td_integer 

!--- get_var_td_logical
SUBROUTINE ext_quilt_get_var_td_logical ( DataHandle,Element,  DateStr,Varname, Data, Count, Outcount, Status )
RETURN
END SUBROUTINE ext_quilt_get_var_td_logical 

!--- put_var_td_logical
SUBROUTINE ext_quilt_put_var_td_logical ( DataHandle,Element,  DateStr,Varname, Data, Count,  Status )
RETURN
END SUBROUTINE ext_quilt_put_var_td_logical 

!--- get_var_td_char
SUBROUTINE ext_quilt_get_var_td_char ( DataHandle,Element,  DateStr,Varname, Data,  Status )
RETURN
END SUBROUTINE ext_quilt_get_var_td_char 

!--- put_var_td_char
SUBROUTINE ext_quilt_put_var_td_char ( DataHandle,Element,  DateStr,Varname, Data,  Status )
RETURN
END SUBROUTINE ext_quilt_put_var_td_char 

!--- read_field
SUBROUTINE ext_quilt_read_field ( DataHandle , DateStr , VarName , Field , FieldType , Comm , IOComm, &
                            DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                            DomainStart , DomainEnd ,                                    &
                            MemoryStart , MemoryEnd ,                                    &
                            PatchStart , PatchEnd ,                                      &
                            Status )
RETURN
END SUBROUTINE ext_quilt_read_field

!--- write_field
SUBROUTINE ext_quilt_write_field ( DataHandle , DateStr , VarName , Field , FieldType , Comm , IOComm,  &
                             DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                             DomainStart , DomainEnd ,                                    &
                             MemoryStart , MemoryEnd ,                                    &
                             PatchStart , PatchEnd ,                                      &
                             Status )
  RETURN
END SUBROUTINE ext_quilt_write_field

!--- get_var_info  (not implemented for IntIO)
SUBROUTINE ext_quilt_get_var_info ( DataHandle , VarName , NDim , MemoryOrder , Stagger , &
                              DomainStart , DomainEnd , Status )
RETURN
END SUBROUTINE ext_quilt_get_var_info

SUBROUTINE get_mpi_comm_io_groups( retval, i )
      RETURN
END SUBROUTINE get_mpi_comm_io_groups

SUBROUTINE get_nio_tasks_in_group( retval )
      RETURN
END SUBROUTINE get_nio_tasks_in_group
!-----------------------------------------------------------------------
      SUBROUTINE write_fcstdone(DateStr)
!-----------------------------------------------------------------------
!***  Write out the fcstdone file to signal that the forecast and output
!***  for each output time are complete.
!-----------------------------------------------------------------------
!     USE module_configure
      USE module_ext_internal
!-----------------------------------------------------------------------
      implicit none
!-----------------------------------------------------------------------
      character(19),intent(in) :: DateStr
!
      character(2) :: wrf_day,wrf_hour,wrf_month
      character(4) :: wrf_year
      character(4) :: tmmark,done='DONE'
      character(50) :: fcstdone_name
!
      integer :: ier,iunit,n,n_fcsthour
      integer :: iday,ihour,iyear,month
      integer :: idif_day,idif_hour,idif_month,idif_year
      integer,save,dimension(12) ::                                     &
     &        days_per_month=(/31,28,31,30,31,30,31,31,30,31,30,31/)
!
      logical :: initial=.true.
!
      integer,save :: start_year,start_month,start_day                  &
     &,               start_hour,start_minute,start_second

      integer :: grid_id,level,s_we,e_we,s_sn,e_sn                      &
     &,          s_vert,e_vert                                          &
     &,          end_year,end_month,end_day                             &
     &,          end_hour,end_minute,end_second                         &
     &,          run_days,run_hours,run_minutes,run_seconds             &
     &,          history_interval,restart_interval                      &
     &,          frames_per_outfile,time_step_sound
!
      namelist /namelist_02/ grid_id,level,s_we,e_we,s_sn,e_sn          &
     &,                      s_vert,e_vert                              &
     &,                      start_year,start_month,start_day           &
     &,                      start_hour,start_minute,start_second       &
     &,                      end_year,end_month,end_day                 &
     &,                      end_hour,end_minute,end_second             &
     &,                      run_days,run_hours,run_minutes,run_seconds &
     &,                      history_interval,restart_interval          &
     &,                      frames_per_outfile,time_step_sound
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!***  Read the start time (year, month, day, hour) directly from 
!***  the namelist file.
!***  Save the initial time so we can compute the forecast hour.
!-----------------------------------------------------------------------
!
      if(initial)then
        call int_get_fresh_handle(iunit)
        open(unit=iunit,file="namelist.input",form="formatted"          &
     &,      status="old")
        read(iunit,namelist_02)
        close(iunit)
!
        if(start_month==2.and.mod(start_year,4)==0)days_per_month(2)=29
        initial=.false.
      endif
!
!-----------------------------------------------------------------------
!***  Extract character date and time of current forecast in DateStr.
!***  Structure of DateStr is yyyy_mm_ddThh:00:00.
!-----------------------------------------------------------------------
!
      wrf_year=DateStr(1:4)
      wrf_month=DateStr(6:7)
      wrf_day=DateStr(9:10)
      wrf_hour=DateStr(12:13)
!
!-----------------------------------------------------------------------
!***  Convert the character strings to integers.
!-----------------------------------------------------------------------
!
      read(wrf_year,*)iyear
      read(wrf_month,*)month
      read(wrf_day,*)iday
      read(wrf_hour,*)ihour
!
!-----------------------------------------------------------------------
!***  Compute the forecast hour.
!-----------------------------------------------------------------------
!
      idif_year=iyear-start_year
      idif_month=month-start_month
      idif_day=iday-start_day
      idif_hour=ihour-start_hour
!
!***  This logic applies to forecasts shorter than a month.
!
      if(idif_year>0)idif_month=idif_month+12
      if(idif_month>0)idif_day=idif_day+days_per_month(start_month)
      n_fcsthour=idif_hour+idif_day*24
      write(0,*)' finished with forecast hour=',n_fcsthour              &
     &,         ' from starttime ',start_year,' ',start_month           &
     &,         ' ',start_day,' ',start_hour
!
!-----------------------------------------------------------------------
!***  Retrieve environmental variable tmmark.
!-----------------------------------------------------------------------
!
      call getenv("tmmark",tmmark)
!
!-----------------------------------------------------------------------
!***  Write out fcstdone.
!-----------------------------------------------------------------------
!
      write(fcstdone_name,100)n_fcsthour,tmmark
  100 format('fcstdone',i2.2,'.',a4)
      call int_get_fresh_handle(iunit)
      close(iunit)
      open(unit=iunit,file=fcstdone_name,form='UNFORMATTED',iostat=ier)
      write(iunit)done
      close(iunit)
!
!-----------------------------------------------------------------------
      END SUBROUTINE write_fcstdone
!-----------------------------------------------------------------------
