!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! MODULE GRIDINFO_MODULE
!
! This module handles (i.e., acquires, stores, and makes available) all data
!   describing the model domains to be processed.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module gridinfo_module

   use misc_definitions_module
   use module_data
!   use module_debug
 

    TYPE(output_vars) :: gridout
    TYPE(boundary_vars) :: gridbdy
    TYPE(work_vars) :: w

   ! Parameters
   integer, parameter :: MAX_DOMAINS = 21, MAXLEVS=99
 
   ! Variables
   integer, dimension(MAX_DOMAINS) :: i_parent_start, j_parent_start, parent_grid_ratio, parent_id
   real,    dimension(MAX_DOMAINS) :: dom_cen_lat, dom_cen_lon
   real,    dimension(MAXLEVS)     :: coord_levs

   integer, dimension(MAX_DOMAINS) :: s_we, e_we, s_sn, e_sn
   integer :: interval_seconds, max_dom, io_form_input, io_form_output, debug_level
   integer :: dt, dt_num, dt_denom, nz, lnsh, lnsv, vcoord
    real   :: pt, ptsgm, ref_lat, ref_lon, dx, dy, pole_lat, pole_lon, truelat1, truelat2,stand_lon
    real   :: ref_x, ref_y

   character (len=MAX_FILENAME_LEN) :: opt_output_from_geogrid_path, &
                          opt_output_from_metgrid_path, opt_metgrid_tbl_path, &
                          ncep_proc_path, ncep_proc_prefix, ncep_proc_domain_type

   character (len=128), dimension(MAX_DOMAINS) :: start_date, end_date
   character (len=MAX_FILENAME_LEN), dimension(MAX_DOMAINS) :: fg_name, constants_name
   logical :: do_tiled_input, do_tiled_output, opt_ignore_dom_center, direct_temp, &
              global,ncep_processing, do_gwd, do_clouds,boundary_flux, no_flux, spectral, just_last, &
              use_igbp, no_seaice, ncep_proc_grib2, movable_nests
   character (len=3) :: grib_src
   character (len=1) :: gridtype
   character (len=128), dimension(MAX_DOMAINS) :: geog_data_res
   character (len=MAX_FILENAME_LEN) :: geog_data_path, opt_geogrid_tbl_path
   character (len=128) :: map_proj


 
   contains
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   ! Name: get_namelist_params
   !
   ! Purpose: Read namelist parameters.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   subroutine get_namelist_params(diag_prints)
 
      implicit none
  
      ! Local variables
      integer :: i, io_form_geogrid, io_form_metgrid
      integer, dimension(MAX_DOMAINS) :: start_year, start_month, start_day, start_hour, start_minute, start_second, &
                                         end_year, end_month, end_day, end_hour, end_minute, end_second
      integer :: funit
      logical :: is_used
      logical,intent(in) :: diag_prints
      character (len=3) :: wrf_core, out_format
      character(len=MAX_FILENAME_LEN) :: prefix
      logical :: ordered_by_date

      namelist /geogrid/ parent_id, parent_grid_ratio, &
                         i_parent_start, j_parent_start, s_we, e_we, s_sn, e_sn, &
                         dx, dy, map_proj, ref_x, ref_y, ref_lat, ref_lon, dom_cen_lat, dom_cen_lon, &
                         pole_lat, pole_lon, truelat1, truelat2, stand_lon, &
                         geog_data_res, geog_data_path, opt_geogrid_tbl_path, ncep_processing, &
                         ncep_proc_path, ncep_proc_prefix, ncep_proc_domain_type, do_gwd, just_last, use_igbp, &
                         ncep_proc_grib2,movable_nests

      namelist /share/ wrf_core, max_dom, start_date, end_date, &
                        start_year, end_year, start_month, end_month, &
                        start_day, end_day, start_hour, end_hour, &
                        start_minute, end_minute, start_second, end_second, &
                        interval_seconds, &
                        io_form_geogrid, opt_output_from_geogrid_path, debug_level
      namelist /ungrib/  out_format, ordered_by_date, prefix, spectral
      namelist /metgrid/ io_form_metgrid, fg_name, constants_name, opt_output_from_metgrid_path, &
                         opt_metgrid_tbl_path, opt_ignore_dom_center 
      namelist /nemsinterp/ pt,ptsgm,nz,direct_temp,global,do_clouds,grib_src,boundary_flux,no_flux,&
                           coord_levs,vcoord,lnsh,lnsv,no_seaice
        
      ! Set defaults
      io_form_geogrid = 2
      io_form_metgrid = 2
      max_dom = 1
      wrf_core = 'ARW'
      debug_level = 0
      do i=1,MAX_DOMAINS
         fg_name(i) = '*'
         constants_name(i) = '*'
         start_year(i) = 0
         start_month(i) = 0
         start_day(i) = 0
         start_hour(i) = 0
         start_minute(i) = 0
         start_second(i) = 0
         end_year(i) = 0
         end_month(i) = 0
         end_day(i) = 0
         end_hour(i) = 0
         end_minute(i) = 0
         end_second(i) = 0
         start_date(i) = '0000-00-00_00:00:00'
         end_date(i) = '0000-00-00_00:00:00'
      end do
      opt_output_from_geogrid_path = './'
      opt_output_from_metgrid_path = './'
      opt_metgrid_tbl_path = 'metgrid/'
      opt_ignore_dom_center = .false.
      interval_seconds = INVALID
      ncep_processing = .false.
      ncep_proc_path = 'NOT_SPECIFIED'
      ncep_proc_prefix = 'NOT_SPECIFIED'
      ncep_proc_domain_type = 'NOT_SPECIFIED'
      do_gwd= .false.
      do_clouds=.false.
      grib_src='GFS'
      boundary_flux=.false.
      no_flux=.false.
      use_igbp=.false.
      ncep_proc_grib2=.false.
      pt=5000.
      ptsgm=30000.
      dt=INVALID
      direct_temp=.true.
      global=.false.
      spectral=.false.
      lnsh=1
      lnsv=1
      no_seaice=.false.
      vcoord=1

      do i=1,MAX_DOMAINS
	parent_id(i)=0
	parent_grid_ratio(i)=3
	i_parent_start(i)=1
	j_parent_start(i)=1
	s_we(i)=1
	e_we(i)=9999
	s_sn(i)=1
	e_sn(i)=9999
      enddo

        ref_x=1
        ref_y=1
	ref_lat=79.0
	ref_lon=-179.0
 	dx=0.1
	dy=0.1
	map_proj='rotlat'
	pole_lat=0.
	pole_lon=0.
	truelat1=0.
	truelat2=0.
	stand_lon=0.
      do i=1,MAX_DOMAINS
        geog_data_res(i) = 'default'
      enddo
      opt_geogrid_tbl_path = 'geogrid/'
      geog_data_path = 'NOT_SPECIFIED'


  
      ! Read parameters from Fortran namelist
      do funit=10,100
         inquire(unit=funit, opened=is_used)
         if (.not. is_used) exit
      end do
      open(funit,file='namelist.nps',status='old',form='formatted',err=1000)

      read(funit,share)

      read(funit,geogrid)

	if (diag_prints) write(0,*) 'NOW HAVE DX,DY: ', dx, dy

        gridout%parent_id_out(1:21)=parent_id(1:21)
        gridout%parent_grid_ratio_out(1:21)=parent_grid_ratio(1:21)
	gridout%DLMD=dx
	gridout%DPHD=dy
        gridout%TPH0D=ref_lat
        gridout%TLM0D=ref_lon
        gridout%dom_cen_lat(1:21)=dom_cen_lat(1:21)
        gridout%dom_cen_lon(1:21)=dom_cen_lon(1:21)
      read(funit,ungrib)
        gridout%spectral=spectral
      read(funit,metgrid)
        COORD_LEVS(1)=0.9   ! bogus
      read(funit,nemsinterp)

	if (diag_prints) then
	write(0,*) 'NOW HAVE ptsgm: ', ptsgm
	write(0,*) 'NOW HAVE direct_temp: ', direct_temp
	write(0,*) 'NOW HAVE global: ', global
        write(0,*) 'NOW HAVE coord_levs(1:20): ', coord_levs(1:20)
	write(0,*) 'NOW HAVE boundary_flux: ', boundary_flux
	write(0,*) 'NOW HAVE no_flux: ', no_flux
	write(0,*) 'NOW HAVE grib_src: ', grib_src
        endif

      close(funit)

	gridout%ptsgm=ptsgm
	gridout%pt=pt
        gridout%lm=nz
        gridout%vcoord=vcoord
        gridout%direct_temp=direct_temp
        gridout%global=global
        gridout%do_clouds=do_clouds
        gridout%boundary_flux=boundary_flux
        gridout%no_flux=no_flux
        gridbdy%tboco_bdy=float(interval_seconds)
        gridbdy%lnsh=lnsh
        gridbdy%lnsv=lnsv
        gridout%use_igbp=use_igbp

        allocate(gridout%coord_levs(1:nz+1))
        gridout%coord_levs(1:nz+1)=coord_levs(1:nz+1)

        if (diag_prints)then
	write(0,*) 'gridout%lm is: ', gridout%lm
	write(0,*) 'gridbdy%tboco_bdy is: ', gridbdy%tboco_bdy
	endif

! BUG: Better handle debug_level in module_debug
!      if ( debug_level .gt. 100 ) then
!         call set_debug_level(DEBUG)
!      else
!         call set_debug_level(WARN)
!      end if


      ! Convert wrf_core to uppercase letters
      do i=1,3
         if (ichar(wrf_core(i:i)) >= 97) wrf_core(i:i) = char(ichar(wrf_core(i:i))-32)
      end do

      ! Before doing anything else, we must have a valid grid type 
      gridtype = ' '
      if (wrf_core == 'ARW') then
         gridtype = 'C'
      else if (wrf_core == 'NMM') then
         gridtype = 'E'
      else if (wrf_core == 'NMB') then
         gridtype = 'B'
      else if (wrf_core == 'SLA') then
         gridtype = 'A'
      end if

      ! Handle IO_FORM+100
      if (io_form_geogrid > 100) then
         io_form_geogrid = io_form_geogrid - 100
         do_tiled_input = .true.
      else
         do_tiled_input = .false.
      end if
      if (io_form_metgrid > 100) then
         io_form_metgrid = io_form_metgrid - 100
         do_tiled_output = .true.
      else
         do_tiled_output = .false.
      end if
  
      io_form_input = io_form_geogrid
  
      io_form_output = io_form_metgrid
  
      if (start_date(1) == '0000-00-00_00:00:00') then
         do i=1,max_dom
            ! Build starting date string
            write(start_date(i), '(i4.4,a1,i2.2,a1,i2.2,a1,i2.2,a1,i2.2,a1,i2.2)') &
               start_year(i),'-',start_month(i),'-',start_day(i),'_',start_hour(i),':',start_minute(i),':',start_second(i)
     
            ! Build ending date string
            write(end_date(i), '(i4.4,a1,i2.2,a1,i2.2,a1,i2.2,a1,i2.2,a1,i2.2)') &
               end_year(i),'-',end_month(i),'-',end_day(i),'_',end_hour(i),':',end_minute(i),':',end_second(i)
         end do
      end if
  

      ! Paths need to end with a /
      i = len_trim(opt_metgrid_tbl_path)
      if (opt_metgrid_tbl_path(i:i) /= '/') then
         opt_metgrid_tbl_path(i+1:i+1) = '/'
      end if
  
      i = len_trim(opt_output_from_geogrid_path)
      if (opt_output_from_geogrid_path(i:i) /= '/') then
         opt_output_from_geogrid_path(i+1:i+1) = '/'
      end if
  
      i = len_trim(opt_output_from_metgrid_path)
      if (opt_output_from_metgrid_path(i:i) /= '/') then
         opt_output_from_metgrid_path(i+1:i+1) = '/'
      end if


      ! Blank strings should be set to flag values
      do i=1,max_dom
         if (len_trim(constants_name(i)) == 0) then
            constants_name(i) = '*'
         end if
         if (len_trim(fg_name(i)) == 0) then
            fg_name(i) = '*'
         end if
      end do
  
      return
  
 1000 write(0,*) 'Error opening file namelist.nps'
 
   end subroutine get_namelist_params
  
end module gridinfo_module
