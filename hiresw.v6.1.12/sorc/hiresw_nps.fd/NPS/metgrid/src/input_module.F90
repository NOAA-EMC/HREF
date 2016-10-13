module input_module

   use gridinfo_module
   use misc_definitions_module
   use module_debug
   use parallel_module
   use dio
   use wrfheader

   implicit none

   type(dio_file) :: dfile
 
   integer :: nrec
 
   character (len=1) :: internal_gridtype
 
   contains
 
   subroutine input_init(nest_number, istatus)
 
      implicit none
  
      ! Arguments
      integer, intent(in) :: nest_number
      integer, intent(out) :: istatus
  
      ! Local variables
      integer :: i
      integer :: comm_1, comm_2
      character (len=MAX_FILENAME_LEN) :: input_fname
  
      istatus = 0
  
      if (my_proc_id == IO_NODE .or. do_tiled_input) then
  
         call dio_init(iret=istatus)
         call mprintf((istatus /= 0),ERROR,'Error in ext_pkg_ioinit')
     
         comm_1 = 1
         comm_2 = 1
         input_fname = ' '
         if (gridtype == 'C') then
            input_fname = trim(opt_output_from_geogrid_path)//'geo_em.d  .dio'
            i = len_trim(opt_output_from_geogrid_path)
            write(input_fname(i+9:i+10),'(i2.2)') nest_number
         else if (gridtype == 'E') then
            input_fname = trim(opt_output_from_geogrid_path)//'geo_nmm.d  .dio'
            i = len_trim(opt_output_from_geogrid_path)
            write(input_fname(i+10:i+11),'(i2.2)') nest_number
         else if (gridtype == 'B') then
            input_fname = trim(opt_output_from_geogrid_path)//'geo_nmb.d  .dio'
            i = len_trim(opt_output_from_geogrid_path)
            write(input_fname(i+10:i+11),'(i2.2)') nest_number
         else if (gridtype == 'A') then
            input_fname = trim(opt_output_from_geogrid_path)//'geo_sla.d  .dio'
            i = len_trim(opt_output_from_geogrid_path)
            write(input_fname(i+10:i+11),'(i2.2)') nest_number
         end if

         if (nprocs > 1 .and. do_tiled_input) then
            write(input_fname(len_trim(input_fname)+1:len_trim(input_fname)+5), '(a1,i4.4)') &
                            '_', my_proc_id
         end if
     
         istatus = 0
         call dio_open(dfile,trim(input_fname),"READ",iret=istatus)
         call mprintf((istatus /= 0),ERROR,'Couldn''t open file %s for input.',s1=input_fname)
     

      end if ! (my_proc_id == IO_NODE .or. do_tiled_input)
  
      nrec = 1
 
   end subroutine input_init
 
 
   subroutine read_next_field(start_patch_i, end_patch_i, &
                              start_patch_j, end_patch_j, &
                              start_patch_k, end_patch_k, &
                              cname, cunits, cdesc, memorder, stagger, &
                              dimnames, real_array, istatus)
 
      implicit none
  
      ! Arguments
      integer, intent(out) :: start_patch_i, end_patch_i, &
                              start_patch_j, end_patch_j, &
                              start_patch_k, end_patch_k
      character (len=*), intent(out) :: cname, memorder, stagger, cunits, cdesc
      character (len=128), dimension(3), intent(out) :: dimnames
      real, pointer, dimension(:,:,:) :: real_array
      integer, intent(out) :: istatus
  
      ! Local variables
      integer :: ndim
      integer :: sm1, em1, sm2, em2, sm3, em3, sp1, ep1, sp2, ep2, sp3, ep3
      integer, dimension(3) :: domain_start, domain_end
      real, pointer, dimension(:,:,:) :: real_domain
      character (len=20) :: datestr
      integer, dimension(512) :: header
      integer i,j,k

      character(len=32) :: VarName,Units,Description,MemoryOrder
      integer :: FieldType
      integer, dimension(3) :: PatchStart, PatchEnd
      integer :: dtype
      integer,dimension(10) :: bounds

      istatus = 0

      if (my_proc_id == IO_NODE .or. do_tiled_input) then
  
         if ( nrec > dio_numrec(dfile) ) then
            istatus=-1
            if (nprocs > 1) call parallel_bcast_int(istatus)
            return
         end if

         istatus = 0

         do while( nrec <= dio_numrec(dfile) )

            call dio_recinfo(dfile,nrec,cname,ndim,dtype,bounds,iret=istatus)
            nrec=nrec+1

            if (bounds(2)/=0.and.bounds(4)/=0.and.bounds(6)/=0) then

               nullify(real_domain)
               allocate(real_domain(bounds(2),bounds(4),bounds(6)))
               call dio_read(dfile,cname,real_domain,header=header,iret=istatus)
               call mprintf((istatus /= 0),ERROR,'In read_next_field(), got error code %i.', i1=istatus)
               call wrfheader_unpack(header, DateStr=DateStr, &
                                            VarName=VarName, &
                                             Units=cunits, &
                                             Description=cdesc, &
                                             FieldType=FieldType, &
                                             MemoryOrder=MemoryOrder, &
                                             Stagger=Stagger, &
                                             DimNames=DimNames, &
                                             DomainStart=domain_start, DomainEnd=domain_end, &
                                             PatchStart=PatchStart, PatchEnd=PatchEnd &
                                     )

               start_patch_i = domain_start(1) 
               start_patch_j = domain_start(2) 
               end_patch_i = domain_end(1)
               end_patch_j = domain_end(2)
               if (ndim == 3) then
        	  start_patch_k = domain_start(3) 
        	  end_patch_k = domain_end(3) 
               else
        	  domain_start(3) = 1
        	  domain_end(3) = 1
        	  start_patch_k = 1
        	  end_patch_k = 1
               end if

               exit
            end if
            if (nrec > dio_numrec(dfile)) then
               istatus=-1
            end if

         end do
     
      end if ! (my_proc_id == IO_NODE .or. do_tiled_input)

      if (nprocs > 1 .and. .not. do_tiled_input) call parallel_bcast_int(istatus)
      if (istatus /= 0) return

      if (nprocs > 1 .and. .not. do_tiled_input) then
         call parallel_bcast_char(cname, len(cname))
         call parallel_bcast_char(cunits, len(cunits))
         call parallel_bcast_char(cdesc, len(cdesc))
         call parallel_bcast_char(memorder, len(memorder))
         call parallel_bcast_char(stagger, len(stagger))
         call parallel_bcast_char(dimnames(1), 128)
         call parallel_bcast_char(dimnames(2), 128)
         call parallel_bcast_char(dimnames(3), 128)
         call parallel_bcast_int(domain_start(3))
         call parallel_bcast_int(domain_end(3))
   
         sp1 = my_minx
         ep1 = my_maxx - 1
         sp2 = my_miny
         ep2 = my_maxy - 1
         sp3 = domain_start(3)
         ep3 = domain_end(3)
   
         if (internal_gridtype == 'C') then
            if (my_x /= nproc_x - 1 .or. stagger == 'U') then
               ep1 = ep1 + 1
            end if
            if (my_y /= nproc_y - 1 .or. stagger == 'V') then
               ep2 = ep2 + 1
            end if
         else if (internal_gridtype == 'E' .or. internal_gridtype == 'B' .or. internal_gridtype == 'A') then
            ep1 = ep1 + 1
            ep2 = ep2 + 1
         end if
   
         sm1 = sp1
         em1 = ep1
         sm2 = sp2
         em2 = ep2
         sm3 = sp3
         em3 = ep3
   
         start_patch_i = sp1
         end_patch_i   = ep1
         start_patch_j = sp2
         end_patch_j   = ep2
         start_patch_k = sp3
         end_patch_k   = ep3

	call mprintf(.true.,LOGFILE,' sm1 is %i and em1 is %i', i1=sm1, i2=em1)
	call mprintf(.true.,LOGFILE,' sm2 is %i and em2 is %i', i1=sm2, i2=em2)
	call mprintf(.true.,LOGFILE,' sm3 is %i and em3 is %i', i1=sm3, i2=em3)
   
         allocate(real_array(sm1:em1,sm2:em2,sm3:em3))
         if (my_proc_id /= IO_NODE) then
            allocate(real_domain(1,1,1))
            domain_start(1) = 1
            domain_start(2) = 1
            domain_start(3) = 1
            domain_end(1) = 1
            domain_end(2) = 1
            domain_end(3) = 1
         end if
         call scatter_whole_field_r(real_array, &
                                   sm1, em1, sm2, em2, sm3, em3, &
                                   sp1, ep1, sp2, ep2, sp3, ep3, &
                                   real_domain, &
                                   domain_start(1), domain_end(1), &
                                   domain_start(2), domain_end(2), &
                                   domain_start(3), domain_end(3))
         deallocate(real_domain)

      else
  
         real_array => real_domain

      end if
 
   end subroutine read_next_field
 
   
   subroutine read_global_attrs(title, start_date, grid_type, dyn_opt, &
                                west_east_dim, south_north_dim, bottom_top_dim, &
                                we_patch_s, we_patch_e, we_patch_s_stag, we_patch_e_stag, &
                                sn_patch_s, sn_patch_e, sn_patch_s_stag, sn_patch_e_stag, &
                                map_proj, mminlu, is_water, &
                                is_ice, is_urban, isoilwater, grid_id, parent_id, i_parent_start, j_parent_start, &
                                i_parent_end, j_parent_end, dx, dy, cen_lat, moad_cen_lat, cen_lon, &
                                stand_lon, truelat1, truelat2, parent_grid_ratio, corner_lats, corner_lons)
 
      implicit none
  
      ! Arguments
      integer, intent(out) :: dyn_opt, west_east_dim, south_north_dim, bottom_top_dim, map_proj, is_water, &
                 we_patch_s, we_patch_e, we_patch_s_stag, we_patch_e_stag, &
                 sn_patch_s, sn_patch_e, sn_patch_s_stag, sn_patch_e_stag, &
                 is_ice, is_urban, isoilwater, grid_id, parent_id, i_parent_start, j_parent_start, &
                 i_parent_end, j_parent_end, parent_grid_ratio
      real, intent(out) :: dx, dy, cen_lat, moad_cen_lat, cen_lon, stand_lon, truelat1, truelat2
      real, dimension(16), intent(out) :: corner_lats, corner_lons
      character (len=128), intent(out) :: title, start_date, grid_type, mminlu
  
      ! Local variables
      integer :: istatus, i
      character (len=128) :: cunits, cdesc, cstagger
  
      if (my_proc_id == IO_NODE .or. do_tiled_input) then
  
         call ext_get_dom_ti_char          ('TITLE', title)
         call ext_get_dom_ti_char          ('SIMULATION_START_DATE', start_date)
         call ext_get_dom_ti_integer_scalar('WEST-EAST_GRID_DIMENSION', west_east_dim)
         call ext_get_dom_ti_integer_scalar('SOUTH-NORTH_GRID_DIMENSION', south_north_dim)
         call ext_get_dom_ti_integer_scalar('BOTTOM-TOP_GRID_DIMENSION', bottom_top_dim)
         call ext_get_dom_ti_integer_scalar('WEST-EAST_PATCH_START_UNSTAG', we_patch_s)
         call ext_get_dom_ti_integer_scalar('WEST-EAST_PATCH_END_UNSTAG', we_patch_e)
         call ext_get_dom_ti_integer_scalar('WEST-EAST_PATCH_START_STAG', we_patch_s_stag)
         call ext_get_dom_ti_integer_scalar('WEST-EAST_PATCH_END_STAG', we_patch_e_stag)
         call ext_get_dom_ti_integer_scalar('SOUTH-NORTH_PATCH_START_UNSTAG', sn_patch_s)
         call ext_get_dom_ti_integer_scalar('SOUTH-NORTH_PATCH_END_UNSTAG', sn_patch_e)
         call ext_get_dom_ti_integer_scalar('SOUTH-NORTH_PATCH_START_STAG', sn_patch_s_stag)
         call ext_get_dom_ti_integer_scalar('SOUTH-NORTH_PATCH_END_STAG', sn_patch_e_stag)
         call ext_get_dom_ti_char          ('GRIDTYPE', grid_type)
         call ext_get_dom_ti_real_scalar   ('DX', dx)
         call ext_get_dom_ti_real_scalar   ('DY', dy)
         call ext_get_dom_ti_integer_scalar('DYN_OPT', dyn_opt)
         call ext_get_dom_ti_real_scalar   ('CEN_LAT', cen_lat)
         call ext_get_dom_ti_real_scalar   ('CEN_LON', cen_lon)
         call ext_get_dom_ti_real_scalar   ('TRUELAT1', truelat1)
         call ext_get_dom_ti_real_scalar   ('TRUELAT2', truelat2)
         call ext_get_dom_ti_real_scalar   ('MOAD_CEN_LAT', moad_cen_lat)
         call ext_get_dom_ti_real_scalar   ('STAND_LON', stand_lon)
         call ext_get_dom_ti_real_vector   ('corner_lats', corner_lats, 16)
         call ext_get_dom_ti_real_vector   ('corner_lons', corner_lons, 16)
         call ext_get_dom_ti_integer_scalar('MAP_PROJ', map_proj)
         call ext_get_dom_ti_char          ('MMINLU', mminlu)
         call ext_get_dom_ti_integer_scalar('ISWATER', is_water)
         call ext_get_dom_ti_integer_scalar('ISICE', is_ice)
         call ext_get_dom_ti_integer_scalar('ISURBAN', is_urban)
         call ext_get_dom_ti_integer_scalar('ISOILWATER', isoilwater)
         call ext_get_dom_ti_integer_scalar('grid_id', grid_id)
         call ext_get_dom_ti_integer_scalar('parent_id', parent_id)
         call ext_get_dom_ti_integer_scalar('i_parent_start', i_parent_start)
         call ext_get_dom_ti_integer_scalar('j_parent_start', j_parent_start)
         call ext_get_dom_ti_integer_scalar('i_parent_end', i_parent_end)
         call ext_get_dom_ti_integer_scalar('j_parent_end', j_parent_end)
         call ext_get_dom_ti_integer_scalar('parent_grid_ratio', parent_grid_ratio)
   
      end if

  
      if (nprocs > 1 .and. .not. do_tiled_input) then
  
         call parallel_bcast_char(title, len(title))
         call parallel_bcast_char(start_date, len(start_date))
         call parallel_bcast_char(grid_type, len(grid_type))
         call parallel_bcast_int(west_east_dim)
         call parallel_bcast_int(south_north_dim)
         call parallel_bcast_int(bottom_top_dim)
         call parallel_bcast_int(we_patch_s)
         call parallel_bcast_int(we_patch_e)
         call parallel_bcast_int(we_patch_s_stag)
         call parallel_bcast_int(we_patch_e_stag)
         call parallel_bcast_int(sn_patch_s)
         call parallel_bcast_int(sn_patch_e)
         call parallel_bcast_int(sn_patch_s_stag)
         call parallel_bcast_int(sn_patch_e_stag)

         ! Must figure out patch dimensions from info in parallel module
!         we_patch_s      = my_minx
!         we_patch_s_stag = my_minx
!         we_patch_e      = my_maxx - 1
!         sn_patch_s      = my_miny
!         sn_patch_s_stag = my_miny
!         sn_patch_e      = my_maxy - 1
!
!         if (trim(grid_type) == 'C') then
!            if (my_x /= nproc_x - 1) then
!               we_patch_e_stag = we_patch_e + 1
!            end if
!            if (my_y /= nproc_y - 1) then
!               sn_patch_e_stag = sn_patch_e + 1
!            end if
!         else if (trim(grid_type) == 'E') then
!            we_patch_e = we_patch_e + 1
!            sn_patch_e = sn_patch_e + 1
!            we_patch_e_stag = we_patch_e
!            sn_patch_e_stag = sn_patch_e
!         end if

         call parallel_bcast_real(dx)
         call parallel_bcast_real(dy)
         call parallel_bcast_int(dyn_opt)
         call parallel_bcast_real(cen_lat)
         call parallel_bcast_real(cen_lon)
         call parallel_bcast_real(truelat1)
         call parallel_bcast_real(truelat2)
         call parallel_bcast_real(moad_cen_lat)
         call parallel_bcast_real(stand_lon)
         do i=1,16
            call parallel_bcast_real(corner_lats(i))
            call parallel_bcast_real(corner_lons(i))
         end do
         call parallel_bcast_int(map_proj)
         call parallel_bcast_char(mminlu, len(mminlu))
         call parallel_bcast_int(is_water)
         call parallel_bcast_int(is_ice)
         call parallel_bcast_int(is_urban)
         call parallel_bcast_int(isoilwater)
         call parallel_bcast_int(grid_id)
         call parallel_bcast_int(parent_id)
         call parallel_bcast_int(i_parent_start)
         call parallel_bcast_int(i_parent_end)
         call parallel_bcast_int(j_parent_start)
         call parallel_bcast_int(j_parent_end)
         call parallel_bcast_int(parent_grid_ratio)
      end if
  
      internal_gridtype = grid_type
 
   end subroutine read_global_attrs


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: ext_get_dom_ti_integer
   !
   ! Purpose: Read a domain time-independent integer attribute from input.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine ext_get_dom_ti_integer_scalar(var_name, var_value)

      implicit none

      ! Arguments
      integer, intent(out) :: var_value
      character (len=*), intent(in) :: var_name

      ! Local variables
      integer :: istatus, outcount

      call dio_read(dfile,trim(var_name), var_value, iret=istatus)
      call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')

   end subroutine ext_get_dom_ti_integer_scalar


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: ext_get_dom_ti_integer
   !
   ! Purpose: Read a domain time-independent integer attribute from input.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine ext_get_dom_ti_integer_vector(var_name, var_value, n)

      implicit none

      ! Arguments
      integer, intent(in) :: n
      integer, dimension(n), intent(out) :: var_value
      character (len=*), intent(in) :: var_name

      ! Local variables
      integer :: istatus

      call dio_read(dfile,trim(var_name), var_value(1:n), iret=istatus)
      call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')

   end subroutine ext_get_dom_ti_integer_vector


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: ext_get_dom_ti_real
   !
   ! Purpose: Read a domain time-independent real attribute from input.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine ext_get_dom_ti_real_scalar(var_name, var_value)

      implicit none

      ! Arguments
      real, intent(out) :: var_value
      character (len=*), intent(in) :: var_name

      ! Local variables
      integer :: istatus

      call dio_read(dfile,trim(var_name), var_value, iret=istatus)
      call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')

   end subroutine ext_get_dom_ti_real_scalar


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: ext_get_dom_ti_real
   !
   ! Purpose: Read a domain time-independent real attribute from input.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine ext_get_dom_ti_real_vector(var_name, var_value, n)

      implicit none

      ! Arguments
      integer, intent(in) :: n
      real, dimension(n), intent(out) :: var_value
      character (len=*), intent(in) :: var_name

      ! Local variables
      integer :: istatus

      call dio_read(dfile,trim(var_name), var_value(1:n), iret=istatus)
      call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')

   end subroutine ext_get_dom_ti_real_vector


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: ext_get_dom_ti_char
   !
   ! Purpose: Read a domain time-independent character attribute from input.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine ext_get_dom_ti_char(var_name, var_value)

      implicit none

      ! Arguments
      character (len=*), intent(in) :: var_name
      character (len=128), intent(out) :: var_value

      ! Local variables
      integer :: istatus

      call dio_read(dfile,trim(var_name), var_value, iret=istatus)
      call mprintf((istatus /= 0),ERROR,'Error in reading domain time-independent attribute')

   end subroutine ext_get_dom_ti_char

 
   subroutine input_close()
 
      implicit none
  
      ! Local variables
      integer :: istatus
  
      istatus = 0

      call dio_close(dfile,iret=istatus)
      call dio_finalize()

   end subroutine input_close
 
end module input_module
