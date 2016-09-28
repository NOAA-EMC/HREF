 module program_setup
 
 implicit none

 character*20          :: domain_name
 character*20          :: domain_type
 character*150         :: gfrac_file
 character*150         :: leaf_area_idx_file
 character*150         :: lsmask_file     ! used to override lsmask calculation
 character*150         :: mxsnow_alb_file
 character*150         :: orog_file       ! used to override orog calculation
 character*150         :: roughness_file
 character*150         :: slopetype_file
 character*150         :: snowfree_albedo_file
 character*150         :: substrate_temp_file
 character*150         :: soiltype_tile_file
 character*150         :: vegtype_tile_file

 integer               :: default_soil_category
 integer               :: default_veg_category
 integer, parameter    :: max_gen=10
 integer               :: imdl, imdl_parent(max_gen)
 integer               :: jmdl, jmdl_parent(max_gen)
 integer, allocatable  :: lonsperlat_mdl (:)
 integer               :: max_orog_tiles
 integer               :: num_gen
 integer               :: num_soil_groups
 integer               :: num_veg_groups
 integer               :: max_soil_tiles
 integer               :: max_total_land_tiles
 integer               :: max_veg_tiles
 integer, allocatable  :: remaining_tot_tiles(:,:)
 integer, parameter    :: max_num_categories = 50
 integer               :: num_smooth_passes1, num_smooth_passes2, smooth
 integer               :: soil_groups(max_num_categories)
 integer               :: veg_groups(max_num_categories)

 logical               :: grib2          ! output in grib2 if true. grib 1 otherwise
 logical               :: lsmask_aavg    ! use area averaging if true, otherwise use bilinear
 logical               :: lsmask_tiles
 logical               :: orog_gwd_tiles
 logical               :: thinned        ! for global grid only, to run
                                         ! on a thinned grid

 real                  :: centlat_mdl, centlat_parent_mdl(max_gen)
 real                  :: centlon_mdl, centlon_parent_mdl(max_gen)
 real, allocatable     :: dx_gfs(:)      ! x-dir resolution, gfs thinned grids 
 real                  :: dx_mdl, dx_parent_mdl(max_gen)   ! x-dir resolution 
 real                  :: dy_mdl, dy_parent_mdl(max_gen)   ! y-dir resolution 
 real                  :: hemi_mdl       ! for polar st only. nh=1.0, sh=-1.0
 real                  :: lat_11_mdl
 real                  :: lon_11_mdl
 real                  :: lsmask_tile_threshold
 real                  :: orient_lon_mdl 
 real                  :: orog_bin_width
 real                  :: orog_tile_threshold
 real                  :: resol_mdl      ! resolution in degrees
 real                  :: soil_tile_threshold
 real                  :: tangent_lat_mdl 
 real                  :: veg_tile_threshold

 contains

!-----------------------------------------------------------------------
! read configuration namelist and get some model grid info.
!-----------------------------------------------------------------------

 subroutine setup(ndom, &
                  a_domain_name, &
                  a_domain_type, &
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
                  a_centlat_parent_mdl, &
                  a_grib2 )

 use mpimod, only           : istart_mdl, iend_mdl, &
                              jstart_mdl, jend_mdl, &
                              nam_mpi_setup, &
                              gaussian_mpi_setup

 implicit none

 include 'mpif.h'

 character*150             :: gfs_lpl_file

 integer                   :: istat, j, jj, numpts

 integer                   :: ndom
 character(len=*)          :: a_domain_name
 character(len=*)          :: a_domain_type
 integer                   :: a_imdl
 integer                   :: a_jmdl
 real                      :: a_dx_mdl
 real                      :: a_dy_mdl
 real                      :: a_centlon_mdl
 real                      :: a_centlat_mdl
 integer,dimension(max_gen):: a_imdl_parent
 integer,dimension(max_gen):: a_jmdl_parent
 real,dimension(max_gen)   :: a_dx_parent_mdl
 real,dimension(max_gen)   :: a_dy_parent_mdl
 real,dimension(max_gen)   :: a_centlon_parent_mdl
 real,dimension(max_gen)   :: a_centlat_parent_mdl
 logical                   :: a_grib2


 namelist /grid/ domain_name, domain_type, imdl, jmdl, centlat_mdl, centlon_mdl,  &
                 dx_mdl, dy_mdl, hemi_mdl, lat_11_mdl, lon_11_mdl,   &
                 orient_lon_mdl, tangent_lat_mdl, imdl_parent, jmdl_parent, &
                 dx_parent_mdl, dy_parent_mdl, centlat_parent_mdl, centlon_parent_mdl, &
                 gfs_lpl_file

 namelist /tiling/ max_total_land_tiles

 namelist /veg_tiling/ max_veg_tiles, veg_tile_threshold, &
                       default_veg_category, num_veg_groups, &
                       veg_groups

 namelist /soil_tiling/ max_soil_tiles, soil_tile_threshold, &
                        default_soil_category, num_soil_groups, &
                        soil_groups 

 namelist /lsmask_orog_tiling/ lsmask_aavg,            &
                               lsmask_tiles,           &
                               lsmask_tile_threshold,  &
                               orog_gwd_tiles,         &
                               max_orog_tiles,         &
                               orog_bin_width,         &
                               orog_tile_threshold,    &
                               smooth,                 &
                               num_smooth_passes1,     &
                               num_smooth_passes2

 namelist /input_data/ leaf_area_idx_file,               &
                       gfrac_file,                       &
                       mxsnow_alb_file,                  &
                       roughness_file,                   &
                       slopetype_file,                   &
                       snowfree_albedo_file,             &
                       soiltype_tile_file,               &
                       substrate_temp_file,              &
                       vegtype_tile_file,                &
                       lsmask_file,                      &
                       orog_file

 namelist /output_data/ grib2

!-----------------------------------------------------------------------
! initialize map projection info to missing values.  these will
! either be read from the namelist or set in the code.
!-----------------------------------------------------------------------

 domain_name="xxx"
 domain_type="xxx"
 imdl = -999                ! all projections
 jmdl = -999                ! all projections
 centlat_mdl = -999.        ! b and egrid only
 centlon_mdl = -999.        ! b and egrid only
 dx_mdl = -999.             ! all but gaussian
 dy_mdl = -999.             ! all but gaussian
 hemi_mdl = -999.           ! polar stereographic only
 lat_11_mdl = -999.         ! polar, lambert conf. and latlon
 lon_11_mdl = -999.         ! polar, lambert conf. and latlon
 orient_lon_mdl = -999.     ! polar and lambert conf only
 tangent_lat_mdl = -999.    ! lambert conformal only
 dx_parent_mdl = -999.      ! for b grid nests
 dy_parent_mdl = -999.      ! for b grid nests
 imdl_parent = -999         ! for b grid nests
 jmdl_parent = -999         ! for b grid nests
 centlat_parent_mdl = -999. ! for b grid nests
 centlon_parent_mdl = -999. ! for b grid nests
 gfs_lpl_file=''            ! for gfs grids
 grib2=.false.              ! default output is grib 1

 print*,"- READ CONFIGURATION NAMELIST"

 open(81, iostat=istat, err=900)

 read(81, nml=grid,               iostat=istat, err=910)
 read(81, nml=tiling,             iostat=istat, err=910)
 read(81, nml=veg_tiling,         iostat=istat, err=910)
 read(81, nml=soil_tiling,        iostat=istat, err=910)
 read(81, nml=lsmask_orog_tiling, iostat=istat, err=910)
 read(81, nml=input_data,         iostat=istat, err=910)
 read(81, nml=output_data,        iostat=istat, err=910)

 close(81)

 domain_name        = a_domain_name
 domain_type        = a_domain_type
 imdl               = a_imdl
 jmdl               = a_jmdl
 dx_mdl             = a_dx_mdl
 dy_mdl             = a_dy_mdl
 centlat_mdl        = a_centlat_mdl
 centlon_mdl        = a_centlon_mdl
 imdl_parent        = a_imdl_parent
 jmdl_parent        = a_jmdl_parent
 centlat_parent_mdl = a_centlat_parent_mdl
 centlon_parent_mdl = a_centlon_parent_mdl
 dx_parent_mdl      = a_dx_parent_mdl
 dy_parent_mdl      = a_dy_parent_mdl
 grib2              = a_grib2

 write(0,nml=grid)

!-----------------------------------------------------------------------
! check if domain name is recognized by the code.  if it is, set
! map projection information.  otherwise, use namelist entries.
!-----------------------------------------------------------------------

 print*,"- SET UP MAP PROJECTION INFO FOR: ", trim(domain_name)
 select case (trim(domain_name))
   case("t382", "T382")
     domain_type = 'gaussian'
     imdl = 1152
     jmdl = 576
     resol_mdl = 360.0 / float(imdl)  ! approx model res in degress
     dx_mdl    = resol_mdl     
     dy_mdl    = resol_mdl   
   case("t510", "T510")
     domain_type = 'gaussian'
     imdl = 1536
     jmdl = 766
     resol_mdl = 360.0 / float(imdl)  ! approx model res in degress
     dx_mdl    = resol_mdl     
     dy_mdl    = resol_mdl   
   case("t574", "T574")
     domain_type = 'gaussian'
     imdl = 1760
     jmdl = 880
     resol_mdl = 360.0 / float(imdl)  ! approx model res in degress
     dx_mdl    = resol_mdl     
     dy_mdl    = resol_mdl   
   case("t878", "T878")
     domain_type = 'gaussian'
     imdl = 1760
     jmdl = 880
     resol_mdl = 360.0 / float(imdl)  ! approx model res in degress
     dx_mdl    = resol_mdl     
     dy_mdl    = resol_mdl   
   case("t1148", "T1148")
     domain_type = 'gaussian'
     imdl = 2304
     jmdl = 1152
     resol_mdl = 360.0 / float(imdl)  ! approx model res in degress
     dx_mdl    = resol_mdl     
     dy_mdl    = resol_mdl   
   case("t1534", "T1534")
     domain_type = 'gaussian'
     imdl = 3072
     jmdl = 1536
     resol_mdl = 360.0 / float(imdl)  ! approx model res in degress
     dx_mdl    = resol_mdl     
     dy_mdl    = resol_mdl   
   case("t254", "T254")
     domain_type = 'gaussian'
     imdl = 768
     jmdl = 384
     resol_mdl = 360.0 / imdl  ! approx model res in degress
     dx_mdl    = resol_mdl     
     dy_mdl    = resol_mdl   
   case("t190", "T190")
     domain_type = 'gaussian'
     imdl = 576
     jmdl = 288
     resol_mdl = 360.0 / float(imdl)  ! approx model res in degress
     dx_mdl    = resol_mdl     
     dy_mdl    = resol_mdl   
   case("t170", "T170")
     domain_type = 'gaussian'
     imdl = 512
     jmdl = 256
     resol_mdl = 360.0 / imdl  ! approx model res in degress
     dx_mdl    = resol_mdl     
     dy_mdl    = resol_mdl   
   case("t126", "T126")
     domain_type = 'gaussian'
     imdl = 384
     jmdl = 190
     resol_mdl = 360.0 / float(imdl)  ! approx model res in degress
     dx_mdl    = resol_mdl     
     dy_mdl    = resol_mdl   
   case("t62", "T62")
     domain_type = 'gaussian'
     imdl = 192
     jmdl = 94
     resol_mdl = 360.0 / float(imdl)  ! approx model res in degress
     dx_mdl    = resol_mdl     
     dy_mdl    = resol_mdl   
   case("centnmm")
     imdl           = 360
     jmdl           = 809
     dx_mdl         = .033147632
     dy_mdl         = .03259901
     centlat_mdl    =  37.0
     centlon_mdl    = -98.0
     resol_mdl      = sqrt((dx_mdl**2) + (dy_mdl**2))
     domain_type    = "egrid"
   case("eta12km")
     imdl           = 606
     jmdl           = 1067
     dx_mdl         = 53.0/605.0
     dy_mdl         = 40.0/533.0
     centlat_mdl    =  50.0
     centlon_mdl    = -111.0
     resol_mdl      = sqrt((dx_mdl**2) + (dy_mdl**2))
     domain_type    = "egrid"
   case("nam12km")
     imdl           = 669
     jmdl           = 1165
     dx_mdl         = 60.0/668.0
     dy_mdl         = 45.0/582.0
     centlat_mdl    =  54.0
     centlon_mdl    = -106.0
     resol_mdl      = sqrt((dx_mdl**2) + (dy_mdl**2))
     domain_type    = "egrid"
   case("nmm16km")
     imdl           = 375
     jmdl           = 563
     dx_mdl         = 0.108
     dy_mdl         = 0.1025
     centlat_mdl    =  50.0
     centlon_mdl    = -111.0
     resol_mdl      = sqrt((dx_mdl**2) + (dy_mdl**2))
     domain_type    = "egrid"
   case("eta32km")
     imdl           = 237
     jmdl           = 387
     dx_mdl         = 53.0/236.0
     dy_mdl         = 40.0/193.0
     centlat_mdl    =  50.0
     centlon_mdl    = -111.0
     resol_mdl      = sqrt((dx_mdl**2) + (dy_mdl**2))
     domain_type    = "egrid"
   case("nmm12km")
     imdl           = 450
     jmdl           = 767
     dx_mdl         = 53.0/605.0
     dy_mdl         = 40.0/533.0
     centlat_mdl    =  50.0
     centlon_mdl    = -111.0
     resol_mdl      = sqrt((dx_mdl**2) + (dy_mdl**2))
     domain_type    = "egrid"
   case("launcher")
     imdl           = 500
     jmdl           = 799
     dx_mdl         = 53.0/605.0
     dy_mdl         = 40.0/533.0
     centlat_mdl    =  50.0
     centlon_mdl    = -111.0
     resol_mdl      = sqrt((dx_mdl**2) + (dy_mdl**2))
     domain_type    = "egrid"
   case("eastnmm")
     imdl           = 360
     jmdl           = 809
     dx_mdl         = .033147632
     dy_mdl         = .03259901
     centlat_mdl    =  37.0
     centlon_mdl    = -80.0
     resol_mdl      = sqrt((dx_mdl**2) + (dy_mdl**2))
     domain_type    = "egrid"
   case("westnmm")
     imdl           = 223
     jmdl           = 501
     dx_mdl         = 24.0/449.0
     dy_mdl         = 1.0/19.0
     centlat_mdl    =  40.0
     centlon_mdl    = -115.0
     resol_mdl      = sqrt((dx_mdl**2) + (dy_mdl**2))
     domain_type    = "egrid"
   case("aknmm")
     imdl           = 223
     jmdl           = 501
     dx_mdl         = 1.0/15.0
     dy_mdl         = 5.0/76.0
     centlat_mdl    =  63.0
     centlon_mdl    = -150.0
     resol_mdl      = sqrt((dx_mdl**2) + (dy_mdl**2))
     domain_type    = "egrid"
   case("ak_dgex")
     imdl           = 233
     jmdl           = 331
     dx_mdl         = 53.0/605.0
     dy_mdl         = 40.0/533.0
     centlat_mdl    =  61.0
     centlon_mdl    = -157.0
     resol_mdl      = sqrt((dx_mdl**2) + (dy_mdl**2))
     domain_type    = "egrid"
   case("conus_dgex")
     imdl           = 303
     jmdl           = 429
     dx_mdl         = 53.0/605.0
     dy_mdl         = 40.0/533.0
     centlat_mdl    =  40.0
     centlon_mdl    = -98.0
     resol_mdl      = sqrt((dx_mdl**2) + (dy_mdl**2))
     domain_type    = "egrid"
   case("prnmm")
     imdl           = 89
     jmdl           = 143
     dx_mdl         = 24.0/449.0
     dy_mdl         = 1.0/19.0
     centlat_mdl    =  18.0
     centlon_mdl    = -66.5
     resol_mdl      = sqrt((dx_mdl**2) + (dy_mdl**2))
     domain_type    = "egrid"
   case("hinmm")
     imdl           = 89
     jmdl           = 143
     dx_mdl         = 24.0/449.0
     dy_mdl         = 1.0/19.0
     centlat_mdl    =  20.25
     centlon_mdl    = -157.35
     resol_mdl      = sqrt((dx_mdl**2) + (dy_mdl**2))
     domain_type    = "egrid"
   case("nmmtest")
     imdl           = 89
     jmdl           = 143
     dx_mdl         = 24.0/449.0
     dy_mdl         = 1.0/19.0
     centlat_mdl    =  0.00
     centlon_mdl    = +10.00
     resol_mdl      = sqrt((dx_mdl**2) + (dy_mdl**2))
     domain_type    = "egrid"
   case default
     print*,"- USER SPECIFIED DOMAIN: ", trim(domain_name)
     select case (trim(domain_type))
       case("egrid")
         resol_mdl      = sqrt((dx_mdl**2) + (dy_mdl**2))
       case("bgrid")
         resol_mdl      = (dx_mdl + dy_mdl)*0.5
       case("bgrid_global")
         dx_mdl = 180.0/(float(imdl-1)/2.0)
         dy_mdl = 90.0/(float(jmdl-1)/2.0)
         centlat_mdl = 0.0
         centlon_mdl = 0.0
         resol_mdl   = (dx_mdl + dy_mdl)*0.5
       case("gaussian")
         resol_mdl = 360.0 / imdl  ! approx model res in degress
         dx_mdl    = resol_mdl     
         dy_mdl    = resol_mdl   
       case("polar")
         resol_mdl = dx_mdl / 111000.   ! degrees (approx.)
       case("latlon")
         resol_mdl = (abs(dx_mdl) + abs(dy_mdl)) * 0.5  ! degrees
       case("lambconf")
         resol_mdl = dx_mdl / 111.0   ! degrees (approx.)
       case default
         print*,"- UNRECOGNIZED DOMAIN TYPE: ", trim(domain_type)
         call mpi_abort(mpi_comm_world, 1, istat)
     end select 
 end select

 if (trim(domain_type) == 'gaussian') then
   thinned = .false.  ! flag to tell rest of program to handle a
                      ! global thinned grid.
   allocate(dx_gfs(jmdl))
   dx_gfs = 360.0 / float(imdl)
   allocate (lonsperlat_mdl(jmdl/2))
   lonsperlat_mdl = imdl   ! full grid
   if (len_trim(gfs_lpl_file) > 0) then
     print*,"- WILL RUN ON A THINNED GRID."
     print*,"- OPEN/READ GFS LONSPERLAT FILE: ",trim(gfs_lpl_file)
     open (27, file=trim(gfs_lpl_file), iostat=istat)
     if (istat /= 0) then
       print*,'- BAD OPEN OF LONSPERLAT FILE. ABORT. IRET: ', istat
       call mpi_abort(mpi_comm_world, 1, istat)
     endif
     read (27,*,iostat=istat) numpts, lonsperlat_mdl
     close(27)
     if (istat /= 0) then
       print*,'- BAD READ OF LONSPERLAT FILE. ABORT. IRET: ', istat
       call mpi_abort(mpi_comm_world, 1, istat)
     endif
     if (numpts /= (jmdl/2)) then
       print*,'- WRONG DIMENSIION IN LONSPERLAT FILE. ABORT.'
       call mpi_abort(mpi_comm_world, 1, istat)
     endif
     thinned = .true.
     do j = 1, jmdl
       jj = j
       if (j > jmdl/2) jj = jmdl - j + 1
       dx_gfs(j) = 360. / float(lonsperlat_mdl(jj))
     enddo
   endif
 endif

!-----------------------------------------------------------------------
! code has logic to handle nests for rotatated lat/lon b-grids.
! if not a nest, the user does not enter information
! for the "parent" variables.  however, the rest of the code uses
! these variables, so fill "parent" variables with grid info.
!-----------------------------------------------------------------------

 if (trim(domain_type)=="bgrid") then 
   if (imdl_parent(1) == -999) then
! i think only the centlon/lat is used.
     imdl_parent(1) = imdl
     jmdl_parent(1) = jmdl
     dx_parent_mdl(1) = dx_mdl
     dy_parent_mdl(1) = dy_mdl
     centlat_parent_mdl(1) = centlat_mdl
     centlon_parent_mdl(1) = centlon_mdl
     num_gen = 1  ! number of "generations" of grids
   else
     do j = 2, max_gen
       if (imdl_parent(j) == -999) then
         exit
       endif
     enddo
     num_gen=j
   endif
 endif

 print*
 print*,"- CREATING SURFACE FIELDS FOR DOMAIN ", trim(domain_name)
 print*,"- DOMAIN TYPE ", trim(domain_type)
 print*,"- I/J GRID DIMENSIONS ARE: ",imdl, jmdl
 if (trim(domain_type) == 'polar') then
   print*,"- MODEL RESOLUTION IN KM IS: ", (abs(dx_mdl) + abs(dy_mdl)) * 5.0e-4
 elseif (trim(domain_type) == 'lambconf') then
   print*,"- MODEL RESOLUTION IN KM IS: ", dx_mdl
 else
   print*,"- MODEL RESOLUTION IN DEGREES IS: ", resol_mdl
 endif
 if (trim(domain_type) == 'egrid' .or. trim(domain_type) == 'bgrid') then
   print*,"- CENTER LAT/LON ARE: ", centlat_mdl, centlon_mdl
 end if

 if (trim(domain_type) == "gaussian" .or. &
     trim(domain_type) == "latlon") then
   call gaussian_mpi_setup(imdl,jmdl,lonsperlat_mdl)
 elseif (trim(domain_type) == "bgrid_global") then
   call gaussian_mpi_setup(imdl,jmdl)
 else   
   call nam_mpi_setup(imdl,jmdl)
 endif

!-----------------------------------------------------------------------
! if orog_gwd_tiles is false, then a simple bilinear is used to
! determine terrain.  no tiles are allowed, so set max_orog_tiles
! to one regardless of what the user chooses.
!-----------------------------------------------------------------------

 if (.not. orog_gwd_tiles) max_orog_tiles=1

!-----------------------------------------------------------------------
! this software allows for the tiling of soil type, veg type and 
! orography.  the sum of the tiles for all fields can not exceed 
! what the user chooses (max_tot_tiles).   this array will keep a
! running total of the number of tiles available. 
!-----------------------------------------------------------------------

 allocate(remaining_tot_tiles(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))

 remaining_tot_tiles = max_total_land_tiles

!-----------------------------------------------------------------------
! output in grib1 or grib 2
!-----------------------------------------------------------------------

 if (grib2) then
   print*,"- OUTPUT DATA IN GRIB2 FORMAT."
 else
   print*,"- OUTPUT DATA IN GRIB1 FORMAT."
 endif

 return

900 print*,"- ERROR OPENING CONFIG NAMELIST. ISTAT IS ", istat
    call mpi_abort(mpi_comm_world, 1, istat)

910 print*,"- ERROR READING CONFIG NAMELIST. ISTAT IS ", istat
    call mpi_abort(mpi_comm_world, 1, istat)

 end subroutine setup

!-----------------------------------------------------------------------
! clear memory
!-----------------------------------------------------------------------

 subroutine setup_cleanup

 implicit none

 if (allocated(remaining_tot_tiles)) deallocate (remaining_tot_tiles)

 return

 end subroutine setup_cleanup

 end module program_setup
