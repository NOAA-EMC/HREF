 module soil_vegtype_tile

 integer, allocatable, public  :: dominate_veg_cat(:,:)

 contains

 subroutine soil_vegtype_driver
 
 use program_setup, only         : soiltype_tile_file,    &
                                   max_soil_tiles,        &
                                   soil_tile_threshold,   &
                                   default_soil_category, &
                                   num_soil_groups,       &
                                   soil_groups,           &       
                                   vegtype_tile_file,     &
                                   max_veg_tiles,         &
                                   veg_tile_threshold,    &
                                   default_veg_category,  &
                                   num_veg_groups,        &
                                   veg_groups,            &
                                   max_num_categories

 implicit none

 integer                      :: kpds5

! call for soil type

 kpds5 = 224  
 call tile_driver(soiltype_tile_file, max_num_categories, &
                  max_soil_tiles,          &
                  soil_tile_threshold, default_soil_category, &
                  num_soil_groups, soil_groups, kpds5)

! call for veg type

 kpds5 = 225  
 call tile_driver(vegtype_tile_file, max_num_categories, &
                  max_veg_tiles,          &
                  veg_tile_threshold, default_veg_category, &
                  num_veg_groups, veg_groups, kpds5)

 return
 
 end subroutine soil_vegtype_driver

 subroutine tile_driver(source_file, max_num_categories,  &
                        max_tiles, tile_threshold, default_category, &
                        num_groups, groups_dum, kpds5)

 use init_grib1,    only        :  kpds_mdl, &
                                   kgds_mdl
 
 use grib_mod

 use init_grib2

 use program_setup, only        :  imdl, &
                                   jmdl, &
                                   remaining_tot_tiles, &
                                   roughness_file, &
                                   thinned, domain_name, grib2

 use lsmask_orog,  only         :  lsmask,       &
                                   lbms_lnd_mdl

 use calc_latlons, only         :  lat_mdl, lon_mdl

 use mpimod, only               :  myrank, istart_mdl, iend_mdl, &
                                   jstart_mdl, jend_mdl, gather, &
                                   iend_mdl_4_loops
 
 use native_endianness, only    :  to_native_endianness

 implicit none

 include 'mpif.h'

 character*256                  :: fngrib
 character*150                  :: source_file

 integer                        :: cat
 integer, intent(in)            :: default_category
 integer, allocatable           :: dominate_cat(:,:)
 integer, intent(in)            :: groups_dum(max_num_categories)
 integer, allocatable           :: groups(:)
 integer*8, parameter           :: header_bytes = 48
 integer, allocatable           :: idum_all(:,:)
 integer*4                      :: isrc, jsrc
 integer                        :: istart_src, iend_src, jstart_src, jend_src
 integer                        :: iret, iunit
 integer                        :: i, j, n
 integer                        :: kgds(200)
 integer                        :: kpds(200)
 integer, intent(in)            :: kpds5
 integer                        :: lugb
 integer, intent(in)            :: max_num_categories
 integer, intent(in)            :: max_tiles
 integer, allocatable           :: max_tiles_grid(:,:)
 integer                        :: num_bytes
 integer*4                      :: num_categories
 integer, intent(in)            :: num_groups
 integer, allocatable           :: num_tiles(:,:)        
 integer*8                      :: offset 
 integer*1, allocatable         :: srcdat(:,:)
 integer*4                      :: water_category

 real*8                         :: dlon_src, dlat_src
 real, allocatable              :: dummy(:,:)
 real, allocatable              :: dummy_all(:,:)
 real*8                         :: lon_11_src, lat_11_src
 real*4, allocatable            :: prcnt_each_cat(:,:,:)                     
 real*4, allocatable            :: prcnt_each_group(:,:,:)                      
 real, intent(in)               :: tile_threshold

 type(gribfield)                :: gfld

 type tile_data
   sequence
   integer :: category
   real*4  :: percent
 end type tile_data

 type(tile_data), allocatable   :: tiles(:,:,:)                      

 if (len_trim(source_file) == 0) return

 print*,"- INTERPOLATE SOURCE DATA TO MODEL GRID"

!-----------------------------------------------------------------------
! open high-res source data.  note: file must be opened on all tasks.
! note: source data must be a global lat/lon grid.  also, source file
! must be in the following common format:
!
! bytes 1-4   - i dimension of grid (integer*4)
! bytes 5-8   - j dimension of grid (integer*4)
! bytes 9-16  - n/s resolution in degrees (real*8)
! bytes 17-24 - e/w resolution in degrees (real*8)
! bytes 25-32 - longitude of pixel (1,1) (real*8)
! bytes 33-40 - latitude of pixel (1,1) (real*8)
! bytes 41-44 - water category (integer*4) - only one water cat allowed
! bytes 45-48 - number of categories (integer*4)
! bytes 49-...- the global data (integer*1)
!-----------------------------------------------------------------------

 print*,"- READ SOURCE DATA: ", trim(source_file)

 iunit = 15

 call mpi_file_open(mpi_comm_world, source_file, mpi_mode_rdonly, &
                    mpi_info_null, iunit, iret)

 if (iret /= 0) then
   print*,'- BAD OPEN, IERR IS ', iret
   call mpi_abort(mpi_comm_world, 1, iret)
 endif

 offset = 0_8
 call mpi_file_read_at(iunit, offset, isrc, 1, &
                       mpi_integer4, mpi_status_ignore, iret)
 if (iret /= 0) goto 9000
 call to_native_endianness(isrc)
 print*,'- ISRC ',isrc

 offset = 4_8
 call mpi_file_read_at(iunit, offset, jsrc, 1, &
                       mpi_integer4, mpi_status_ignore, iret)
 if (iret /= 0) goto 9000
 call to_native_endianness(jsrc)
 print*,'- JSRC ',jsrc

 offset = 8_8
 call mpi_file_read_at(iunit, offset, dlon_src, 1, &
                        mpi_double_precision, mpi_status_ignore, iret)
 if (iret /= 0) goto 9000
 call to_native_endianness(dlon_src)
 print*,'- DLON ', dlon_src

 offset = 16_8
 call mpi_file_read_at(iunit, offset, dlat_src, 1, &
                        mpi_double_precision, mpi_status_ignore, iret)
 if (iret /= 0) goto 9000
 call to_native_endianness(dlat_src)
 print*,'- DLAT ', dlat_src

 offset = 24_8
 call mpi_file_read_at(iunit, offset, lon_11_src, 1, &
                        mpi_double_precision, mpi_status_ignore, iret)
 if (iret /= 0) goto 9000
 call to_native_endianness(lon_11_src)
 print*,'- LON11 ',lon_11_src

 offset = 32_8
 call mpi_file_read_at(iunit, offset, lat_11_src, 1, &
                        mpi_double_precision, mpi_status_ignore, iret)
 if (iret /= 0) goto 9000
 call to_native_endianness(lat_11_src)
 print*,'- LAT11 ',lat_11_src

 offset = 40_8
 call mpi_file_read_at(iunit, offset, water_category, 1, &
                        mpi_integer4, mpi_status_ignore, iret)
 if (iret /= 0) goto 9000
 call to_native_endianness(water_category)
 print*,'- WATER CATEGORY ',water_category

 offset = 44_8
 call mpi_file_read_at(iunit, offset, num_categories, 1, &
                        mpi_integer4, mpi_status_ignore, iret)
 if (iret /= 0) goto 9000
 call to_native_endianness(num_categories)
 print*,'- NUMBER OF CATEGORIES ',num_categories

!-----------------------------------------------------------------------
! find the i/j bounds of the model grid with respect to the source grid.
!-----------------------------------------------------------------------

 call find_bounds_ll(isrc, jsrc, lat_11_src, lon_11_src, dlat_src, dlon_src, &
                     istart_src, iend_src, jstart_src, jend_src)

 allocate (srcdat(isrc,jstart_src:jend_src))

 offset    = int(isrc,8)*(int(jstart_src,8)-1_8) + header_bytes
 num_bytes = isrc * ( jend_src - jstart_src + 1 )

 call mpi_file_read_at(iunit, offset, srcdat, num_bytes, &
                       mpi_integer1, mpi_status_ignore, iret)
 if (iret /= 0) goto 9000

 print*,'- THE DATA ',maxval(srcdat),minval(srcdat)

 allocate (dominate_cat(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))
 dominate_cat=water_category
 allocate (prcnt_each_cat(istart_mdl:iend_mdl,jstart_mdl:jend_mdl,num_categories))
 prcnt_each_cat=0.0
 allocate (prcnt_each_group(istart_mdl:iend_mdl,jstart_mdl:jend_mdl,num_groups))
 prcnt_each_group=0.0
 allocate (groups(num_categories))

 groups(1:num_categories)=groups_dum(1:num_categories)

 call interp_tiles(lon_11_src, lat_11_src, &
                   srcdat, isrc, istart_src, iend_src, &
                   jstart_src, jend_src, &
                   dlon_src, dlat_src, &
                   num_categories, num_groups, &
                   water_category, default_category, &
                   tile_threshold, groups, &
                   prcnt_each_cat, prcnt_each_group,  &
                   dominate_cat)

 deallocate (srcdat)

!-----------------------------------------------------------------------
! now determine the soil tiles.  need to limit the tiles at
! each grid point to be no more than what the user specifies for 
! soil type.  also, constrain tiles to be no more than the
! running total of all tiles for all fields (variable remaining_tot_tiles).
!-----------------------------------------------------------------------

 allocate ( max_tiles_grid(istart_mdl:iend_mdl,jstart_mdl:jend_mdl) )
 
 max_tiles_grid = max_tiles

 do j = jstart_mdl, jend_mdl 
 do i = istart_mdl, iend_mdl_4_loops(j)
   if (remaining_tot_tiles(i,j) < max_tiles) then
     max_tiles_grid(i,j) = remaining_tot_tiles(i,j)
   endif
 enddo
 enddo

!-----------------------------------------------------------------------
!  choose tiles.
!-----------------------------------------------------------------------

 allocate (tiles(istart_mdl:iend_mdl,jstart_mdl:jend_mdl,max_tiles))
 tiles%percent    = 0.0
 tiles%category   = 0

 allocate (num_tiles (istart_mdl:iend_mdl,jstart_mdl:jend_mdl))
 num_tiles        = 0

 call calc_tiles(dominate_cat, &
                 prcnt_each_cat, prcnt_each_group, &
                 num_groups, num_categories, &
                 groups, max_tiles, &
                 max_tiles_grid, remaining_tot_tiles, &
                 tiles, num_tiles)

 deallocate ( groups )
 deallocate ( max_tiles_grid )
 deallocate ( prcnt_each_group )
 
!-----------------------------------------------------------------------
! output tile data in grib format
!-----------------------------------------------------------------------

 kgds = kgds_mdl
 kpds = kpds_mdl

 if (kpds5 == 224) then
   lugb   = 22
   if (grib2) then
     fngrib  = trim(domain_name)//"_soiltiles.grb2"
   else
     fngrib  = trim(domain_name)//"_soiltiles.grb"
   endif
 elseif (kpds5 == 225) then
   lugb   = 23
   if (grib2) then
     fngrib  = trim(domain_name)//"_vegtiles.grb2"
   else
     fngrib  = trim(domain_name)//"_vegtiles.grb"
   endif
 endif

 if (myrank == 0) then
   call baopenw(lugb,fngrib,iret)
   if (iret /= 0) then
     print*,"BAD OPEN OF OUTPUT GRIB FILE: ", trim(fngrib), " IRET IS ", iret
     call mpi_abort(mpi_comm_world, 1, iret)
   end if
 end if

 allocate (idum_all(imdl,jmdl))
 allocate (dummy_all(imdl,jmdl))
 
!-----------------------------------------------------------------------
! dominate category.
!-----------------------------------------------------------------------

 if(kpds5==225 .and. (trim(roughness_file)=='usgs'.or.trim(roughness_file)=='igbp'))then
   allocate(dominate_veg_cat(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))
   dominate_veg_cat=dominate_cat
 endif

 call gather(dominate_cat, imdl, jmdl, idum_all)

 dummy_all = float(idum_all)

 if (thinned) then
   call fill(dummy_all)
 end if

 if (grib2) then
   call grib2_init(gfld)
   gfld%fld=reshape(dummy_all, (/imdl*jmdl/) )
   gfld%ibmap = 0 ! bitmap applies
   gfld%bmap=reshape(lbms_lnd_mdl, (/imdl*jmdl/) )
   gfld%discipline = 2
   if(kpds5==225)then  ! veg type
     gfld%ipdtmpl(1)= 0  ! oct 10; parameter category
     gfld%ipdtmpl(2)= 198 ! oct 11; parameter
   else   ! soil type
     gfld%ipdtmpl(1)= 3  ! oct 10; parameter category
     gfld%ipdtmpl(2)= 0 ! oct 11; parameter
   endif
   gfld%idrtmpl=0
   gfld%idrtmpl(3)=0 ! decimal scaling factor
 else
   kpds(22) = 0
   kpds(5)  = kpds5
 endif

 if (myrank == 0) then
   if (grib2) then
     call putgb2(lugb,gfld,iret)
   else
     call putgb (lugb, (imdl*jmdl), kpds, kgds, lbms_lnd_mdl,    &
                 dummy_all, iret)
   endif
   if (iret /= 0) then
     print*,"BAD WRITE OF FILE: ", trim(fngrib), " IRET IS ", iret
     call mpi_abort(mpi_comm_world, 1, iret)
   end if
 end if

 if (grib2) then
   call grib2_free(gfld)
 endif

 deallocate (dominate_cat)

!-----------------------------------------------------------------------
! if the number of soil tiles chosen is greater than one, write
! out the tile information. otherwise close the file.
!-----------------------------------------------------------------------

 MULTIPLE_TILES : if (max_tiles > 1) then

   kpds(5)  = kpds(5) + 1
   kpds(22) = 0

   call gather(num_tiles, imdl, jmdl, idum_all)

   dummy_all = float(idum_all)

   if (myrank == 0) then
     if (thinned) then
       call fill(dummy_all)
     end if
     call putgb (lugb, (imdl*jmdl), kpds, kgds, lbms_lnd_mdl,    &
                 dummy_all, iret)
     if (iret /= 0) then
       print*,"BAD WRITE OF FILE: ", trim(fngrib), " IRET IS ", iret
       call mpi_abort(mpi_comm_world, 1, iret)
     end if
   end if ! rank 0

!-----------------------------------------------------------------------
! each tile's category and percent coverage.
!-----------------------------------------------------------------------

   allocate(dummy(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))

   do n = 1, max_tiles

     kpds(5)  = kpds(5) + 1
     kpds(22) = 0

     do j = jstart_mdl, jend_mdl
     do i = istart_mdl, iend_mdl
       dummy(i,j) = float(tiles(i,j,n)%category)
     enddo
     enddo     
     
     call gather(dummy, imdl, jmdl, dummy_all)

     if (myrank == 0) then
       if (thinned) then
         call fill(dummy_all)
       end if
       call putgb (lugb, (imdl*jmdl), kpds, kgds, lbms_lnd_mdl,    &
                   dummy_all, iret)
       if (iret /= 0) then
         print*,"BAD WRITE OF FILE: ", trim(fngrib), " IRET IS ", iret
         call mpi_abort(mpi_comm_world, 1, iret)
       end if
     end if  ! rank 0

     kpds(5)  = kpds(5) + 1
     kpds(22) = 1

     do j = jstart_mdl, jend_mdl
     do i = istart_mdl, iend_mdl
       dummy(i,j) = tiles(i,j,n)%percent
     enddo
     enddo     

     call gather(dummy, imdl, jmdl, dummy_all)

     if (myrank == 0) then
       if (thinned) then
        call fill(dummy_all)
       end if
       call putgb (lugb, (imdl*jmdl), kpds, kgds, lbms_lnd_mdl,    &
                   dummy_all, iret)
       if (iret /= 0) then
         print*,"BAD WRITE OF FILE: ", trim(fngrib), " IRET IS ", iret
         call mpi_abort(mpi_comm_world, 1, iret)
       end if
     end if ! rank 0

   enddo

   deallocate (tiles)

!-----------------------------------------------------------------------
! the percent of each category.
!-----------------------------------------------------------------------

 do cat = 1,num_categories

   kpds(5)  = kpds(5) + 1
   kpds(22) = 1

   dummy = prcnt_each_cat(:,:,cat)
   call gather(dummy, imdl, jmdl, dummy_all)

   if (myrank == 0) then
     if (thinned) then
       call fill(dummy_all)
     end if
     call putgb (lugb, (imdl*jmdl), kpds, kgds, lbms_lnd_mdl,    &
                 dummy_all, iret)
     if (iret /= 0) then
       print*,"BAD WRITE OF FILE: ", trim(fngrib), " IRET IS ", iret
       call mpi_abort(mpi_comm_world, 1, iret)
     end if
   end if ! rank 0

 enddo

 deallocate (dummy)

 end if MULTIPLE_TILES

 if (myrank == 0)  call baclose(lugb, iret)
 call mpi_file_close(iunit, iret)

 deallocate (dummy_all)
 deallocate (idum_all)

 deallocate (prcnt_each_cat)
 if (allocated (num_tiles))       deallocate (num_tiles)

 return 

 9000 print*,'- BAD READ OF SOURCE FILE, IERR IS ', iret
 call mpi_abort(mpi_comm_world, 1, iret)

 end subroutine tile_driver

 end module soil_vegtype_tile
