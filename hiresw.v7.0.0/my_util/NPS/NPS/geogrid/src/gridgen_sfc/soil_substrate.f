 subroutine soil_substrate

 use init_grib1, only           : kpds_mdl, &
                                  kgds_mdl

 use grib_mod

 use init_grib2

 use program_setup, only        : max_orog_tiles, &
                                  resol_mdl, &
                                  imdl, &
                                  jmdl, &
                                  substrate_temp_file,  &
                                  thinned, grib2,  & 
                                  domain_type,  &
                                  dx_mdl,  &
                                  dx_gfs,  &
                                  dy_mdl,  &
                                  centlon_mdl, &
                                  centlat_mdl, domain_name

 use calc_latlons,  only        : lat_mdl, &
                                  lon_mdl, lat_first_mdl, lon_first_mdl

 use lsmask_orog,  only         : lsmask,    &
                                  lbms_lnd_mdl,  &
                                  num_orog_tiles, &
                                  orog_tiles_prcnt, &
                                  orog_tiles_elev

 use interp_utils, only         : interp_aavg_gaus,     &
                                  interp_bilinear,     &
                                  interp_nn,  &
                                  interp_aavg_nam, &
                                  interp_aavg_egrid_prep, &
                                  interp_aavg_bgrid_prep

 use mpimod, only               : gather,     &
                                  istart_mdl,  &
                                  iend_mdl, iend_mdl_4_loops, &
                                  jstart_mdl, &
                                  jend_mdl, &
                                  myrank

 use native_endianness, only    : to_native_endianness, &
                                  is_little_endian

 implicit none

 include 'mpif.h'

 real, parameter                :: hgt_1000mb   = 110.9
 real, parameter                :: lapse        = -6.5E-03

 character(len=mpi_max_error_string) :: errmsg
 character*256                       :: fngrib

 integer                        :: istart_src, iend_src, jstart_src, jend_src

 integer                        :: count
 integer*4                      :: fcst_time_unit
 integer*4                      :: grib_parm_num
 integer*8, parameter           :: header_size = 78
 integer                        :: i, ii, j, jj, jjj, n
 integer, allocatable           :: idummy(:,:)
 integer                        :: iret
 integer*4                      :: isrc, jsrc
 integer                        :: iunit_src
 integer*8                      :: offset
 integer                        :: kgds(200)
 integer                        :: kpds(200)
 integer                        :: lugb
 integer*2, allocatable         :: nearest_i(:,:), nearest_j(:,:)
 integer                        :: near_j, num_bytes
 integer*4                      :: num_records, num_time_unit
 integer*4                      :: scaling_fac
 integer*2, allocatable         :: soilt_src(:,:)
 integer                        :: tile
 integer*2                      :: undef_value
 integer*4                      :: year, mon, day, hour 

 real                           :: default_value
 real, allocatable              :: dummy(:,:)
 real*8                         :: dx_src
 real*8                         :: dy_src
 real*8                         :: lat_11_src
 real*8                         :: lon_11_src
 real, allocatable              :: soilt_avg(:)
 real, allocatable              :: soilt_mdl(:,:)
 real, allocatable              :: soilt_tiles(:,:,:)
 real                           :: sum, test
 real                           :: tavg
 real                           :: zlevel

 type(gribfield)                :: gfld

!-----------------------------------------------------------------------
! execution starts here.
!-----------------------------------------------------------------------

 if (len_trim(substrate_temp_file) == 0) return

 print*,"- INTERPOLATE SUBSTRATE TEMPERATURE DATA TO MODEL GRID"

 print*,'- OPEN SOURCE FILE ', trim(substrate_temp_file)
 iunit_src = 40
 call mpi_file_open(mpi_comm_world, substrate_temp_file, mpi_mode_rdonly, &
                    mpi_info_null, iunit_src, iret)
 if (iret /= 0) then
   print*,'- BAD OPEN: ', iret
   call mpi_abort(mpi_comm_world, 1, iret)
 endif

!-----------------------------------------------------------------------
! read every record in the source data file, interpolate the data to the
! model grid, then grib the interpolated data.
!
! source data must be on a global lat/lon grid.
!
! source data format is as follows:
!
! bytes 1-4   - number of records in file (integer*4)
! bytes 5-8   - i dimension of grid (integer*4)
! bytes 9-12  - j dimension of grid (integer*4)
! bytes 13-20 - n/s resolution in degrees (real*8)
! bytes 21-28 - e/w resolution in degrees (real*8)
! bytes 29-36 - longitude of pixel (1,1) (real*8)
! bytes 37-44 - latitude of pixel (1,1) (real*8)
! bytes 45-46 - water flag (integer*2) - only one water cat allowed
! bytes 47-50 - scaling factor (integer*4)
! bytes 51-54 - year of record (integer*4)
! bytes 55-58 - month of record (integer*4)
! bytes 59-62 - day of record (integer*4)
! bytes 63-66 - hour of record (integer*4)
! bytes 67-70 - forecast time unit (integer*4) (see grib standard)
! bytes 71-74 - num of time units (integer*4) (see grib standard)
! bytes 75-78 - grib parameter number (integer*4) (see grib standard)
! bytes 79-...- the global data (integer*2)
!
! header is repeated for each additional record.
!-----------------------------------------------------------------------

 print*,'- READ SOURCE FILE'
 offset = 0_8
 call mpi_file_read_at(iunit_src, offset, num_records, 1, &
                       mpi_integer4, mpi_status_ignore, iret)
 if (iret /= 0) goto 9000
 call to_native_endianness(num_records)
 print*,'- NUMBER OF RECORDS ',num_records

 offset = 4_8
 call mpi_file_read_at(iunit_src, offset, isrc, 1, &
                       mpi_integer4, mpi_status_ignore, iret)
 if (iret /= 0) goto 9000
 call to_native_endianness(isrc)
 print*,'- I-DIM OF SOURCE GRID ',isrc

 offset = 8_8
 call mpi_file_read_at(iunit_src, offset, jsrc, 1, &
                       mpi_integer4, mpi_status_ignore, iret)
 if (iret /= 0) goto 9000
 call to_native_endianness(jsrc)
 print*,'- J-DIM OF SOURCE GRID ',jsrc

 offset = 12_8
 call mpi_file_read_at(iunit_src, offset, dy_src, 1, &
                       mpi_double_precision, mpi_status_ignore, iret)
 if (iret /= 0) goto 9000
 call to_native_endianness(dy_src)
 print*,'- DY ',dy_src

 offset = 20_8
 call mpi_file_read_at(iunit_src, offset, dx_src, 1, &
                       mpi_double_precision, mpi_status_ignore, iret)
 if (iret /= 0) goto 9000
 call to_native_endianness(dx_src)
 print*,'- DX ',dx_src

 offset = 28_8
 call mpi_file_read_at(iunit_src, offset, lon_11_src, 1, &
                       mpi_double_precision, mpi_status_ignore, iret)
 if (iret /= 0) goto 9000
 call to_native_endianness(lon_11_src)
 print*,'- LON11 ',lon_11_src

 offset = 36_8
 call mpi_file_read_at(iunit_src, offset, lat_11_src, 1, &
                       mpi_double_precision, mpi_status_ignore, iret)
 if (iret /= 0) goto 9000
 call to_native_endianness(lat_11_src)
 print*,'- LAT11 ',lat_11_src

 offset = 44_8
 call mpi_file_read_at(iunit_src, offset, undef_value, 1, &
                       mpi_integer2, mpi_status_ignore, iret)
 if (iret /= 0) goto 9000
 call to_native_endianness(undef_value)
 print*,'- UNDEFINED VALUE ',undef_value

 offset = 46_8
 call mpi_file_read_at(iunit_src, offset, scaling_fac, 1, &
                       mpi_integer4, mpi_status_ignore, iret)
 if (iret /= 0) goto 9000
 call to_native_endianness(scaling_fac)
 print*,'- SCALING FACTOR ',scaling_fac

 offset = 50_8
 call mpi_file_read_at(iunit_src, offset, year, 1, &
                       mpi_integer4, mpi_status_ignore, iret)
 if (iret /= 0) goto 9000
 call to_native_endianness(year)
 print*,'- YEAR ',year

 offset = 54_8
 call mpi_file_read_at(iunit_src, offset, mon, 1, &
                       mpi_integer4, mpi_status_ignore, iret)
 if (iret /= 0) goto 9000
 call to_native_endianness(mon)
 print*,'- MONTH ',mon

 offset = 58_8
 call mpi_file_read_at(iunit_src, offset, day, 1, &
                       mpi_integer4, mpi_status_ignore, iret)
 if (iret /= 0) goto 9000
 call to_native_endianness(day)
 print*,'- DAY ',day

 offset = 62_8
 call mpi_file_read_at(iunit_src, offset, hour, 1, &
                       mpi_integer4, mpi_status_ignore, iret)
 if (iret /= 0) goto 9000
 call to_native_endianness(hour)
 print*,'- HOUR ',hour

 offset = 66_8
 call mpi_file_read_at(iunit_src, offset, fcst_time_unit, 1, &
                       mpi_integer4, mpi_status_ignore, iret)
 if (iret /= 0) goto 9000
 call to_native_endianness(fcst_time_unit)
 print*,'- FCST TIME UNIT ',fcst_time_unit

 offset = 70_8
 call mpi_file_read_at(iunit_src, offset, num_time_unit, 1, &
                       mpi_integer4, mpi_status_ignore, iret)
 if (iret /= 0) goto 9000
 call to_native_endianness(num_time_unit)
 print*,'- NUMBER OF TIME UNITS ',num_time_unit

 offset = 74_8
 call mpi_file_read_at(iunit_src, offset, grib_parm_num, 1, &
                       mpi_integer4, mpi_status_ignore, iret)
 if (iret /= 0) goto 9000
 call to_native_endianness(grib_parm_num)
 print*,'- GRIB PARAMETER NUMBER ',grib_parm_num

!-----------------------------------------------------------------------
! find the bounds of the model grid with respect to the source grid.
!-----------------------------------------------------------------------

 call find_bounds_ll(isrc, jsrc, lat_11_src, lon_11_src, dy_src, dx_src, &
                     istart_src, iend_src, jstart_src, jend_src)

 allocate (soilt_src(isrc,jstart_src:jend_src))

 offset = 78_8 + 2_8*(int(isrc,8)*(int(jstart_src,8)-1_8))
 num_bytes = isrc * (jend_src - jstart_src + 1)
 call mpi_file_read_at(iunit_src, offset, soilt_src, num_bytes, &
                       mpi_integer2, mpi_status_ignore, iret)
 if (iret /= 0) goto 9000

 if (is_little_endian) then
   do j = jstart_src, jend_src
   do i = 1, isrc
     call to_native_endianness(soilt_src(i,j))
   enddo
   enddo
 endif

 print*,'- THE DATA ',maxval(soilt_src),minval(soilt_src)

!-----------------------------------------------------------------------
! determine the interpolation method based on the resolutions
! of the model and source grids.
!-----------------------------------------------------------------------

 allocate (soilt_mdl(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))
 soilt_mdl = 0.0

 default_value     = -999.  ! if interpolation routines cant find 
                            ! source data near the model grid point,
                            ! the model point is set to this value.

 if ( resol_mdl <= dx_src ) then
   print*,'- WILL USE BI-LINEAR INTERPOLATION '
   call interp_bilinear (istart_mdl, iend_mdl, iend_mdl_4_loops, &
                         jstart_mdl, jend_mdl, &
                         lat_mdl, lon_mdl, lsmask, &
                         dx_src, dy_src, lat_11_src, lon_11_src, &
                         default_value, undef_value, scaling_fac, &
                         soilt_mdl, soilt_src, isrc, jstart_src, jend_src)
 else
   test = int ( (resol_mdl * 0.5) / dx_src )
   if (test == 0) then
     print*,'- WILL USE NEAREST NEIGHBOR INTERPOLATION'
     call interp_nn(istart_mdl, iend_mdl, iend_mdl_4_loops, &
                    jstart_mdl, jend_mdl,  &
                    lat_mdl, lon_mdl, lsmask, &
                    dx_src, dy_src, lat_11_src, lon_11_src, &
                    default_value, undef_value, scaling_fac,  &
                    soilt_mdl, soilt_src, isrc, jstart_src, jend_src)
   elseif (test > 0) then
     print*,'- WILL TAKE AREA AVERAGE OF SOURCE DATA. '
     if (trim(domain_type) == "egrid" .or. &
         trim(domain_type) == "bgrid") then

!-----------------------------------------------------------------------
!      for each source data point, calculate the corresponding
!      index on the model grid.  this is done once before the loop
!      in order to reduce wall clock time.
!      (lat/lon to i/j on the nam grid is very expensive
!      to calculate).
!-----------------------------------------------------------------------

       print*,'- PREP FOR AREA AVERAGING'
       allocate(nearest_i(istart_src:iend_src,jstart_src:jend_src))
       allocate(nearest_j(istart_src:iend_src,jstart_src:jend_src))

       if (trim(domain_type) == "egrid") then
         call interp_aavg_egrid_prep(istart_src, iend_src, jstart_src, jend_src, &
                                     isrc, dx_src, dy_src, lat_11_src, &
                                     lon_11_src, centlat_mdl, &
                                     centlon_mdl, dx_mdl, dy_mdl, &
                                     istart_mdl, iend_mdl, jstart_mdl, jend_mdl, &
                                     imdl, jmdl, nearest_i, nearest_j)
       else
         call interp_aavg_bgrid_prep(istart_src, iend_src, jstart_src, jend_src, &
                                     isrc, dx_src, dy_src, lat_11_src, &
                                     lon_11_src, centlat_mdl, &
                                     centlon_mdl, lat_first_mdl, lon_first_mdl, dx_mdl, dy_mdl, &
                                     istart_mdl, iend_mdl, jstart_mdl, jend_mdl, &
                                     imdl, jmdl, nearest_i, nearest_j)
       end if

       call interp_aavg_nam(istart_mdl, iend_mdl, jstart_mdl, jend_mdl, &
                            lat_mdl, lon_mdl, lsmask, &
                            dx_src, dy_src, lat_11_src, lon_11_src, &
                            default_value, undef_value, scaling_fac,  &
                            soilt_mdl, soilt_src, isrc, istart_src, iend_src, &
                            jstart_src, jend_src, nearest_i, nearest_j)

       deallocate(nearest_i, nearest_j)

     else

       call interp_aavg_gaus (istart_mdl, iend_mdl, iend_mdl_4_loops, &
                              jstart_mdl, jend_mdl, &
                              lat_mdl, lon_mdl, lsmask,  &
                              dx_gfs(jstart_mdl:jend_mdl), dy_mdl, &
                              dx_src, dy_src, lat_11_src, lon_11_src,  &
                              default_value, undef_value, scaling_fac, soilt_mdl, &
                              soilt_src, isrc, jstart_src, jend_src)


     endif
   endif
 end if

!------------------------------------------------------------------------
! check for undefined points as indicated by the flag value. 
! replace with the latitude band average.  search adjacent bands
! if nearest band is undefined.
!------------------------------------------------------------------------

 allocate (soilt_avg(jstart_src:jend_src))  ! latitude band average

 do j = jstart_src, jend_src
   sum   = 0.0
   count = 0
   do i = 1, isrc
     if (soilt_src(i,j) /= undef_value) then
       count = count + 1
       sum   = sum + float(soilt_src(i,j))/float(scaling_fac)
     end if
   enddo
   if (count > 0) then
     soilt_avg(j) = sum / float(count)
   else
     soilt_avg(j) = 0.0
   end if
 enddo

 deallocate (soilt_src)

 JLOOP : do j = jstart_mdl, jend_mdl
   ILOOP : do i = istart_mdl, iend_mdl_4_loops(j)

     if (lsmask(i,j) == 0.0) cycle ILOOP

     UNDEFINED : if (soilt_mdl(i,j) < (default_value + 0.001)) then

       near_j = nint((lat_mdl(i,j) - lat_11_src) / dy_src + 1.0)

       tavg = soilt_avg(near_j)

       if (tavg > 0.0) then

         soilt_mdl(i,j) = tavg

       else
 
         OUTER : do jj = 1, 5
         INNER : do n = -1, 1, 2

           jjj = near_j + (jj*n)

           if (jjj < jstart_src .or. jjj > jend_src) cycle INNER

           tavg = soilt_avg(jjj)

           if (tavg > 0.0) then
             soilt_mdl(i,j) = tavg
             cycle ILOOP 
           end if

         enddo INNER
         enddo OUTER 

         if (soilt_mdl(i,j) < (default_value + 0.001)) then
           print*,'- UNDEFINED SUBSTRATE TEMPERATURE AT POINT: ',i,j
           call mpi_abort(mpi_comm_world, 1, iret)
         end if

       end if
       
     end if UNDEFINED

   enddo ILOOP
 enddo JLOOP

 deallocate (soilt_avg)

!-----------------------------------------------------------------------
! adjust substrate temperature for the model terrain height.  this
! code assumes that the source data is valid at a single level,
! like sea level or 1000mb.  
!-----------------------------------------------------------------------

 zlevel = hgt_1000mb
!cggg zlevel = 0.0

 allocate (soilt_tiles(istart_mdl:iend_mdl,jstart_mdl:jend_mdl,max_orog_tiles))

 soilt_tiles = 0.0

 do j = jstart_mdl, jend_mdl
   do i = istart_mdl, iend_mdl_4_loops(j)
     if (lsmask(i,j) > 0.0) then
       do tile = 1, num_orog_tiles(i,j)
         soilt_tiles(i,j,tile) = soilt_mdl(i,j) +    &
                                (orog_tiles_elev(i,j,tile) - zlevel) * lapse
       enddo
     end if
   enddo
 enddo

 deallocate (soilt_mdl)

!-----------------------------------------------------------------------
! grib data.
!-----------------------------------------------------------------------

 kgds = kgds_mdl

 if (myrank == 0) then
   lugb = 48
   if(grib2) then
     fngrib = trim(domain_name)//"_tbot.grb2"
   else
     fngrib = trim(domain_name)//"_tbot.grb"
   endif
   call baopenw(lugb,fngrib,iret)
   if (iret /= 0) then
     print*,'- BAD OPEN, IRET IS ', iret
     call mpi_abort(mpi_comm_world, 1, iret)
   end if
 end if

 if (grib2) then
   call grib2_init(gfld)
   gfld%discipline = 2
   gfld%ipdtmpl(1)= 3  ! oct 10; parameter category
   gfld%ipdtmpl(2)= 18 ! oct 11; parameter
   gfld%ipdtmpl(10)=106  ! depth below land sfc; sec4 oct 23
   gfld%ipdtmpl(11)=0    ! sec4 oct 24; scale factor of 1st sfc
   gfld%ipdtmpl(12)=8    ! sec4 oct 25-28; value of first fixed sfc
   gfld%idsect(6) = year  ! octs 13-14;  year
   gfld%idsect(7) = mon   ! oct 15; month
   gfld%idsect(8) = day   ! oct 16; day
   gfld%idsect(9) = hour  ! oct 17; hour
   gfld%ibmap = 0 ! bitmap applies
   gfld%idrtmpl(3)=3 ! decimal scaling factor
 else
   kpds(1)  = 7               ! center id
   kpds(2)  = 255             ! process id number. ?
   kpds(3)  = kpds_mdl(3)
   kpds(4)  = kpds_mdl(4)
   kpds(5)  = grib_parm_num   ! grib parameter number 
   kpds(6)  = 111             ! level type - below ground level
   kpds(7)  = 800             ! 800 cm below ground level
   kpds(8) = mod(year,100)    ! year of century
   kpds(21) = year/100 + 1    ! century
   if (kpds(8) == 0) then
     kpds(8) = 100
     kpds(21) = kpds(21) - 1
   end if
   kpds(9)  = mon             ! month
   kpds(10) = day             ! day - greenness valid 15th of month.
   kpds(11) = hour            ! hour
   kpds(12) = 0               ! minute
   kpds(13) = fcst_time_unit  ! fcst time unit - month
   kpds(14) = 0               ! period of time, p1.  set to '0' for analysis
   kpds(15) = num_time_unit   ! number of time units, p2.
   kpds(16) = 51              ! time range indicator
   kpds(17) = 1               ! number in average
   kpds(18) = 1               ! grib edition 1
   kpds(19) = 130             ! parameter table version number
   kpds(20) = 0               ! number missing from avg/accum
   kpds(22) = 0               ! scaling factor
   kpds(23) = 0               ! subcenter
   kpds(24) = 0               ! reserved
   kpds(25) = 0               ! reserved
 endif

 allocate (dummy(imdl,jmdl))
 allocate (idummy(imdl,jmdl))

 if (max_orog_tiles == 1) then ! just write out substrate record

   kpds(22) =  3

!----------------------------------------------------------------------
!  if running a global thinned grid, need to fill in unprocessed
!  points before writing out data.
!----------------------------------------------------------------------

   call gather(soilt_tiles(:,:,1), imdl, jmdl, dummy)

   if (thinned) then
     call fill(dummy)
   end if

   if (grib2) then
     gfld%fld=reshape(dummy, (/imdl*jmdl/) )
     gfld%bmap=reshape(lbms_lnd_mdl, (/imdl*jmdl/) )
   endif

   if (myrank == 0) then
     if(grib2) then
       call putgb2(lugb,gfld,iret)
     else
       call putgb (lugb, (imdl*jmdl), kpds, kgds, lbms_lnd_mdl,    &
                   dummy, iret)
     endif
     if (iret /= 0) then
       print*,'- BAD WRITE OF FILE:', trim(fngrib), ' IRET IS ', iret
       call mpi_abort(mpi_comm_world, 1, iret)
     end if
   end if

   if (grib2) then
     call grib2_free(gfld)
   endif

 else   ! write out all tiles

   call gather(num_orog_tiles, imdl, jmdl, idummy)

   dummy = float(idummy)

!----------------------------------------------------------------------
!  if running a global thinned grid, need to fill in unprocessed
!  points before writing out data.
!----------------------------------------------------------------------

   if (thinned) then
     call fill(dummy)
   end if

   if (myrank == 0) then
     call putgb (lugb, (imdl*jmdl), kpds, kgds, lbms_lnd_mdl,    &
                 dummy, iret)
     if (iret /= 0) then
       print*,'- BAD WRITE OF FILE:', trim(fngrib), ' IRET IS ', iret
       call mpi_abort(mpi_comm_world, 1, iret)
     end if
   end if

!cggg increment parameter number for now.  need to resolve
!cggg how to do this properly.

   do tile = 1, max_orog_tiles

     kpds(5)  = kpds(5) + 1
     kpds(22) =  3

!----------------------------------------------------------------------
!  if running a global thinned grid, need to fill in unprocessed
!  points before writing out data.
!----------------------------------------------------------------------

     call gather(soilt_tiles(:,:,tile), imdl, jmdl, dummy)

     if (thinned) then
       call fill(dummy)
     end if

     if (myrank == 0) then
       call putgb (lugb, (imdl*jmdl), kpds, kgds, lbms_lnd_mdl,    &
                   dummy, iret)
       if (iret /= 0) then
         print*,'- BAD WRITE OF FILE:', trim(fngrib), ' IRET IS ', iret
         call mpi_abort(mpi_comm_world, 1, iret)
       end if
     end if

     kpds(5)  = kpds(5) + 1
     kpds(22) =  2

     call gather(orog_tiles_prcnt(:,:,tile), imdl, jmdl, dummy)

     if (thinned) then
       call fill(dummy)
     end if

     if (myrank == 0) then
       call putgb (lugb, (imdl*jmdl), kpds, kgds, lbms_lnd_mdl,    &
                   dummy, iret)
       if (iret /= 0) then
         print*,'- BAD WRITE OF FILE:', trim(fngrib), ' IRET IS ', iret
         call mpi_abort(mpi_comm_world, 1, iret)
       end if
     end if

   enddo

 end if  ! multiple tile check

 if (myrank == 0) call baclose(lugb, iret)

 call mpi_file_close(iunit_src, iret)

 deallocate (soilt_tiles)
 deallocate (idummy, dummy)

 return

 9000 print*,'- ERROR READING DATA. IRET IS: ', iret
      call mpi_error_string(iret, errmsg, ii, iret)
      print*,"- ERROR IS: ", errmsg(1:ii)
      call mpi_abort(mpi_comm_world, 1, iret)

 end subroutine soil_substrate
