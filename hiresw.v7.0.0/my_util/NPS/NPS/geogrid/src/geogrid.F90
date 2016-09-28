!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Program: geogrid
!
! Written by Michael G. Duda
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
program geogrid

   use gridinfo_module
   use llxy_module
   use list_module
   use module_debug
   use parallel_module
   use process_tile_module
   use process_ncep_module
   use source_data_module

   implicit none

   ! Local variables
   integer :: i, nest_level, temp, n_domains_start
   logical :: ew_extra_col, sn_extra_row
   type(list) :: level_list, ratio_list

   ! For gridgen_sfc
   integer, parameter    :: max_gen=10
   integer :: nest_top_ratio
   integer :: a_imdl, a_jmdl, a_imdl_parent(max_gen), a_jmdl_parent(max_gen)
   real(8) :: a_dx_mdl, a_dy_mdl, a_centlon_mdl, a_centlat_mdl, &
              a_dx_parent_mdl(max_gen), a_dy_parent_mdl(max_gen), a_centlon_parent_mdl(max_gen), a_centlat_parent_mdl(max_gen)

   integer :: ierr
   character(len=128) :: domain_name

   ! Prepare anything necessary to do parallel processing of domains 
   ! The parallel module should be initialized before any other calls take place
   call parallel_start()

   call mprintf(.true.,LOGFILE,' *** Starting program geogrid.exe *** ')
  
   ! Have the gridinfo module retrieve description of the grid setup
   call get_grid_params()

   NCEP: if (ncep_processing) then

      ! Get information about the source data to be processed
      call get_datalist()

      if (gridtype == 'B') then

         call compute_nest_locations()

         if (just_last) then
            n_domains_start = n_domains
         else
            n_domains_start =  1
         endif

         ! Create list to track NMMB nest_top_ratio
         call list_init(ratio_list)

         ! Process all requested domains 
         do i=n_domains_start,n_domains

            call mprintf(.true.,STDOUT,'Processing domain %i of %i', i1=i, i2=n_domains)
            call mprintf(.true.,LOGFILE,'Processing domain %i of %i', i1=i, i2=n_domains)

            ! Get information about the source data we will use for this nest
            call get_source_params(geog_data_res(i))

            ! Set transformations in llxy module to be with respect to current nest
            call select_domain(i)

            call mprintf(.true.,LOGFILE,'Dimensions for this domain are %i and %i', i1=ixdim(i),i2=jydim(i))
            ! Determine which range of indices we will work on
            call parallel_get_tile_dims(ixdim(i), jydim(i))


            movable_nests_test: if (.not.movable_nests) then

               write(domain_name,"(a,i2.2)") trim(ncep_proc_prefix)//'_d',i

               a_imdl               = proj_stack(i)%ixdim
               a_jmdl               = proj_stack(i)%jydim
               a_dx_mdl             = proj_stack(i)%loninc
               a_dy_mdl             = proj_stack(i)%latinc
               a_centlon_mdl        = proj_stack(i)%domcenlon_loc
               a_centlat_mdl        = proj_stack(i)%domcenlat_loc

               if (i > 1) then
                  a_imdl_parent(1)        = proj_stack(parent_id(i))%ixdim
                  a_jmdl_parent(1)        = proj_stack(parent_id(i))%jydim
                  a_dx_parent_mdl(1)      = proj_stack(parent_id(i))%loninc
                  a_dy_parent_mdl(1)      = proj_stack(parent_id(i))%latinc
! - this "parent" center latitude/longitude needs to be the center of the 
! - projection, so use the center lat/lon from the top level domain
                  a_centlon_parent_mdl(1) = ref_lon
                  a_centlat_parent_mdl(1) = ref_lat
               else
                  a_imdl_parent        = -999
                  a_jmdl_parent        = -999
                  a_dx_parent_mdl      = -999.0
                  a_dy_parent_mdl      = -999.0
                  a_centlon_parent_mdl = -999.0
                  a_centlat_parent_mdl = -999.0
               end if

!               ignore_gridgen_sfc=.false.
!d               ignore_gridgen_sfc=.true.

        write(0,*) 'here knowing ignore_gridgen_sfc is: ', ignore_gridgen_sfc

               if (.not. ignore_gridgen_sfc) then

        write(0,*) 'calling gridgen_sfc'
                  call gridgen_sfc(i, &
                                   trim(domain_name), &
                                   trim(ncep_proc_domain_type), &
                                   a_imdl, &
                                   a_jmdl, &
                                   a_dx_mdl, &
                                   a_dy_mdl, &
                                   a_centlon_mdl, &
                                   a_centlat_mdl, &
                                   a_imdl_parent, &
                                   a_jmdl_parent, &
                                   a_dx_parent_mdl, &
                                   a_dy_parent_mdl, &
                                   a_centlon_parent_mdl, &
                                   a_centlat_parent_mdl)
               endif

               call process_ncep(i, gridtype, domain_name, ixdim(I), jydim(I), &
                                 ixdim(I), jydim(I), 1, 1)


            else movable_nests_test

               nest_top_ratio = get_nest_top_ratio(i)
               write(domain_name,"(a,i2.2)") trim(ncep_proc_prefix)//'_r',nest_top_ratio

               a_imdl               = (proj_stack(1)%ixdim-1) * nest_top_ratio + 1
               a_jmdl               = (proj_stack(1)%jydim-1) * nest_top_ratio + 1
               a_dx_mdl             = proj_stack(1)%loninc / nest_top_ratio
               a_dy_mdl             = proj_stack(1)%latinc / nest_top_ratio
               a_centlon_mdl        = ref_lon
               a_centlat_mdl        = ref_lat
               a_imdl_parent        = -999
               a_jmdl_parent        = -999
               a_dx_parent_mdl      = -999.0
               a_dy_parent_mdl      = -999.0
               a_centlon_parent_mdl = -999.0
               a_centlat_parent_mdl = -999.0

               if (.not. list_search(ratio_list, ikey=nest_top_ratio, ivalue=temp)) then

                  call list_insert(ratio_list, ikey=nest_top_ratio, ivalue=nest_top_ratio)

                  write(0,*)' processing ',trim(domain_name)
                  call gridgen_sfc(i, &
                                   trim(domain_name), &
                                   trim(ncep_proc_domain_type), &
                                   a_imdl, &
                                   a_jmdl, &
                                   a_dx_mdl, &
                                   a_dy_mdl, &
                                   a_centlon_mdl, &
                                   a_centlat_mdl, &
                                   a_imdl_parent, &
                                   a_jmdl_parent, &
                                   a_dx_parent_mdl, &
                                   a_dy_parent_mdl, &
                                   a_centlon_parent_mdl, &
                                   a_centlat_parent_mdl)

                  if (nest_top_ratio>1 .and. my_proc_id == IO_NODE) then
                     call sfc_grib2nc(trim(domain_name),nest_top_ratio,a_imdl,a_jmdl)
                  endif

! likely not necessary, but keep for now being cautious
#ifdef _MPI
                  call MPI_Barrier(comm,ierr)
#endif

               endif

               call process_ncep(i, gridtype, domain_name, ixdim(I), jydim(I), &
                                 a_imdl, a_jmdl, proj_stack(i)%top_i_parent_start, proj_stack(i)%top_j_parent_start)

            end if movable_nests_test


         end do

         ! Free up list that was used for tracking NMMB ratio levels
         call list_destroy(ratio_list)
 
      else
         write(0,*)' only gridtype B is supported with ncep_processing=.true.'
         stop
      end if

   else

      ! Get information about the source data to be processed
      call get_datalist()

      if (gridtype == 'C') then

         ! Tell the llxy module that it can now compute parameters necessary to do 
         !   transformations for any nest 
         call compute_nest_locations()

         ! Process all requested domains 
         do i=1,n_domains
            call mprintf(.true.,STDOUT,'Processing domain %i of %i', i1=i, i2=n_domains)
            call mprintf(.true.,LOGFILE,'Processing domain %i of %i', i1=i, i2=n_domains)

            ! Get information about the source data we will use for this nest
            call get_source_params(geog_data_res(i))

            ! Set transformations in llxy module to be with respect to current nest
            call select_domain(i)

            ! Determine which range of indices we will work on
            call parallel_get_tile_dims(ixdim(i), jydim(i))

            if (my_x == nproc_x-1) then ! One more column for U points
               ew_extra_col = .true.
            else
               ew_extra_col = .false.
            end if

            if (my_y == nproc_y-1) then ! One more row for V points
               sn_extra_row = .true.
            else
               sn_extra_row = .false.
            end if

            ! Process fields for a tile of the current nest
            call process_tile(i, gridtype, dyn_opt,                 &
                              1,       ixdim(i), 1,       jydim(i), &
                              my_minx, my_maxx,  my_miny, my_maxy,  &   ! These come from parallel_module
                              ew_extra_col, sn_extra_row)
         end do

      else if (gridtype == 'E') then

         ! Get number of grid points and grid spacing for nest levels
         call compute_nest_level_info()

         ! Create list to track NMM nesting levels
         call list_init(level_list)

         ! Process all requested domains 
         do i=1,n_domains

            nest_level = get_nest_level(i)

            if (.not. list_search(level_list, ikey=nest_level, ivalue=temp)) then
               call list_insert(level_list, ikey=nest_level, ivalue=nest_level)

               if (nest_level == 1) then
                  call mprintf(.true.,STDOUT,'Processing coarse domain', i1=nest_level)
                  call mprintf(.true.,LOGFILE,'Processing coarse domain', i1=nest_level)
               else
                  call mprintf(.true.,STDOUT,'Processing nesting level %i', i1=nest_level-1)
                  call mprintf(.true.,LOGFILE,'Processing nesting level %i', i1=nest_level-1)
               end if

               ! Get information about the source data we will use for this nest
               call get_source_params(geog_data_res(i))

               ! Set transformations in llxy module to be with respect to current nest
               call select_domain(nest_level)

               ! Determine which range of indices we will work on
               call parallel_get_tile_dims(ixdim(nest_level), jydim(nest_level))

               sn_extra_row = .false.  
               ew_extra_col = .false.  

               ! Process fields for a tile of the current nest
               call process_tile(nest_level, gridtype, dyn_opt, &
                                 1, ixdim(nest_level), 1, jydim(nest_level), &
                                 my_minx, my_maxx, my_miny, my_maxy, &   ! These come from parallel_module
                                 ew_extra_col, sn_extra_row)
            end if
         end do

         ! Free up list that was used for tracking NMM nesting levels
         call list_destroy(level_list)

      else if (gridtype == 'B') then

         call compute_nest_locations()

         ! Process all requested domains 
         do i=1,n_domains

            call mprintf(.true.,STDOUT,'Processing domain %i of %i', i1=i, i2=n_domains)
            call mprintf(.true.,LOGFILE,'Processing domain %i of %i', i1=i, i2=n_domains)

            ! Get information about the source data we will use for this nest
            call get_source_params(geog_data_res(i))

            ! Set transformations in llxy module to be with respect to current nest
            call select_domain(i)

            call mprintf(.true.,LOGFILE,'Dimensions for this domain are %i and %i', i1=ixdim(i),i2=jydim(i))
            ! Determine which range of indices we will work on
            call parallel_get_tile_dims(ixdim(i), jydim(i))

            ew_extra_col = .false.
            sn_extra_row = .false.

            ! Process fields for a tile of the current nest

            call process_tile(i, gridtype, dyn_opt,                 &
                              1, ixdim(i), 1,  jydim(i), &
                              my_minx, my_maxx,  my_miny, my_maxy,  &   ! These come from parallel_module
                              ew_extra_col, sn_extra_row)
         end do
      else if (gridtype == 'A') then

         call compute_nest_locations()

         do i=1,n_domains

            ! Get information about the source data we will use for this nest
            call get_source_params(geog_data_res(i))

            ! Set transformations in llxy module to be with respect to current nest
            call select_domain(i)

            ! Determine which range of indices we will work on
            call parallel_get_tile_dims(ixdim(i), jydim(i))

            ew_extra_col = .false.
            sn_extra_row = .false.

            ! Process fields for a tile of the current nest

            print*, 'call process_tile'
            call process_tile(i, gridtype, dyn_opt,                 &
                              1, ixdim(i), 1,  jydim(i), &
                              my_minx, my_maxx,  my_miny, my_maxy,  &   ! These come from parallel_module
                              ew_extra_col, sn_extra_row)

         end do
      end if

   ENDIF NCEP

   print*, 'beyond NCEP if test'

   ! Free up memory used by list of source data to be processed
   call datalist_destroy()
 
   ! Clean up parallel stuff
   call parallel_finish()
 
   call mprintf(.true.,STDOUT,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
   call mprintf(.true.,STDOUT,'!  Successful completion of geogrid.        !')
   call mprintf(.true.,STDOUT,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')

   call mprintf(.true.,LOGFILE,' *** Successful completion of program geogrid.exe *** ')

   call close_logfile()
 
   stop

end program geogrid
