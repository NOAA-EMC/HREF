 module lsmask_orog

 integer, allocatable              :: num_orog_tiles(:,:)

 integer, parameter                :: orog_tile_missing = -999.0

 logical*1, allocatable            :: lbms_lnd_mdl(:,:)
 logical*1, allocatable            :: lbms_wtr_mdl(:,:)

 real, allocatable, private        :: convexity(:,:)
 real, allocatable, private        :: gamma(:,:)
 real, allocatable, private        :: max_height(:,:)
 real, allocatable, private        :: oa(:,:,:)
 real, allocatable, private        :: ol(:,:,:)
 real, allocatable, private        :: sigma(:,:)
 real, allocatable, private        :: theta(:,:)
 real, allocatable                 :: lsmask(:,:)   ! decimal % land 
 real, allocatable                 :: orog_tiles_elev(:,:,:)
 real, allocatable                 :: orog_tiles_prcnt(:,:,:)
 real, allocatable                 :: orog_stnd_dev(:,:)
 real, allocatable                 :: wtrmask(:,:)  ! 1.0 - lsmask

 contains

!-----------------------------------------------------------------------
! calc land/sea mask and orography.
!-----------------------------------------------------------------------

 subroutine lsmask_orog_driver

 use grib_mod

 use init_grib2

 use init_grib1, only           : kpds_mdl, kgds_mdl

 use mpimod, only               : istart_mdl, iend_mdl, &
                                  jstart_mdl, jend_mdl, &
                                  iend_mdl_4_loops, myrank, &
                                  gather, &
                                  scatter

 use program_setup, only        : smooth, num_smooth_passes1, num_smooth_passes2, &
                                  thinned, orog_file, lsmask_file, &
                                  imdl, jmdl, remaining_tot_tiles, domain_type,  &
                                  max_orog_tiles, lsmask_aavg, lsmask_tiles, &
                                  orog_gwd_tiles, domain_name, grib2

 implicit none

 include 'mpif.h'

 type(gribfield)               :: gfld

 character*256                 :: fname

 integer                       :: cc
 integer                       :: count
 integer                       :: i, j, k
 integer, allocatable          :: idummy(:,:)
 integer                       :: ier
 integer                       :: ii
 integer, allocatable          :: isave(:)
 integer                       :: jj
 integer                       :: jgds(200),jpds(200)
 integer, allocatable          :: jsave(:)
 integer                       :: kgds(200)
 integer                       :: kpds(200)
 integer                       :: latbnd
 integer                       :: lskip
 integer                       :: lunout
 integer, allocatable          :: max_orog_tiles_grid(:,:)
 integer                       :: numpts
 integer                       :: tile

 logical*1                     :: lbms_src(imdl,jmdl)
 logical*1, allocatable        :: lbms(:,:)
 logical                       :: use_external_mask, use_external_orog

 real, allocatable             :: dummy(:,:)
 real, allocatable             :: dummy2(:,:)


 print*,"- CALCULATE LAND/SEA MASK AND OROGRAPHY"

 allocate (lsmask(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))
 lsmask = 0.0

 allocate (wtrmask(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))
 wtrmask = 0.0

 allocate (num_orog_tiles(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))
 allocate (orog_stnd_dev(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))
 allocate (orog_tiles_elev(istart_mdl:iend_mdl, &
                           jstart_mdl:jend_mdl, max_orog_tiles))
 allocate (orog_tiles_prcnt(istart_mdl:iend_mdl, &
                            jstart_mdl:jend_mdl,max_orog_tiles))

 num_orog_tiles   = 0
 orog_tiles_elev  = orog_tile_missing
 orog_tiles_prcnt = 0.
 orog_stnd_dev    = 0.

!-----------------------------------------------------------------------
! if user specifies a land mask and orog grib file in the namelist,
! read the data and use it to map the surface fields.  otherwise,
! let this program calculate the mask and orography.
!-----------------------------------------------------------------------

 use_external_mask = .false.
 USE_EXT_MASK: if ( index(lsmask_file, ".grb ") > 0 ) then
   use_external_mask = .true.
   allocate(dummy(imdl,jmdl))
   if (myrank == 0) then
     print*,"- GET TERRAIN AND LAND MASK FROM EXISTING FILES."
     print*,"- OPEN INPUT FILE ", trim(lsmask_file)
     call baopenr (99, lsmask_file, ier)
     if (ier /= 0) then
       print*,'- BAD OPEN, IRET IS ', ier
       call mpi_abort(mpi_comm_world, 1, ier)
     end if
     print*,"- DEGRIB DATA "
     jgds    = -1
     jpds    = -1
     jpds(5) = 81
     lskip   = -1
     call getgb(99, 0, (imdl*jmdl), -1, jpds, jgds, &
                numpts, lskip, kpds, kgds, lbms_src, dummy, ier)
     if (ier /= 0) then
       print*,"- BAD DEGRIB OF DATA. IRET IS ", ier
       call mpi_abort(mpi_comm_world, 1, ier)
     end if
     call baclose(99, ier)
     if (thinned) call thin(dummy)
   end if  ! rank is zero
   call scatter(lsmask, imdl, jmdl, dummy)
   deallocate(dummy)

 elseif (len_trim(lsmask_file) > 0) then

   if (lsmask_aavg) then
     call calc_lsmask_aavg
   else    ! bilinear technique
     call calc_lsmask_bilinear
   endif

   LAKES : if (domain_type == "egrid" .and. .not. lsmask_tiles) then

     allocate (dummy(imdl,jmdl))
     call gather(lsmask, imdl, jmdl, dummy)
     print*,"- REMOVE SMALL LAKES."
     call remove_lakes_egrid(dummy)
     call scatter(lsmask, imdl, jmdl, dummy)
     deallocate (dummy)

   elseif (domain_type == "gaussian" .and. .not. lsmask_tiles) then

     allocate (dummy(imdl,jmdl))
     call gather(lsmask, imdl, jmdl, dummy)
     print*,"- REMOVE SMALL LAKES."
     call remove_lakes_gaussian(dummy)
     call scatter(lsmask, imdl, jmdl, dummy)
     deallocate (dummy)

   elseif (domain_type == "bgrid" .and. .not. lsmask_tiles) then

     allocate (dummy(imdl,jmdl))
     call gather(lsmask, imdl, jmdl, dummy)
     print*,"- REMOVE SMALL LAKES."
     call remove_lakes_bgrid(dummy)
     call scatter(lsmask, imdl, jmdl, dummy)
     deallocate (dummy)

   end if LAKES

 else

   print*,'- MUST INPUT RAW OR PREPROCESSED LAND MASK DATA'
   call lsmask_orog_cleanup
   call mpi_abort(mpi_comm_world, 1, ier)

 end if USE_EXT_MASK ! read in mask from file?

 use_external_orog = .false.
 USE_EXT_OROG: if ( index(orog_file, ".grb ") > 0 ) then

!-----------------------------------------------------------------------
!  will not use tiles since production data is not tiled.
!-----------------------------------------------------------------------

   use_external_orog = .true.
   allocate (dummy(imdl,jmdl))
   num_orog_tiles   = 1  
   orog_tiles_prcnt(:,:,1) = 1.0
   if (myrank == 0) then
     print*,"- OPEN INPUT FILE ", trim(orog_file)
     call baopenr (99, orog_file, ier)
     if (ier /= 0) then
       print*,'- BAD OPEN, IRET IS ', ier
       call mpi_abort(mpi_comm_world, 1, ier)
     end if
     print*,"- DEGRIB TERRAIN HEIGHT DATA"
     jgds    = -1
     jpds    = -1
     jpds(5) =  8  ! param number of terrain height.
     lskip   = -1
     call getgb(99, 0, (imdl*jmdl), -1, jpds, jgds, &
                numpts, lskip, kpds, kgds, lbms_src, dummy, ier)
     if (ier /= 0) then
       print*,"- BAD DEGRIB OF DATA. IRET IS ", ier
       call mpi_abort(mpi_comm_world, 1, ier)
     end if
     if (thinned) call thin(dummy)
   end if ! rank is zero
   call scatter(orog_tiles_elev(:,:,1), imdl, jmdl, dummy)
   deallocate (dummy)

 elseif (len_trim(orog_file) > 0) then

!-----------------------------------------------------------------------
! compute the orography.
!-----------------------------------------------------------------------

   if (orog_gwd_tiles) then
     call calc_orog
   else
     call calc_orog_bilinear
   endif

!-----------------------------------------------------------------------
! don't know how to smooth terrain with multiple tiles right now.
! so, skip for now.  will need to revisit this.
!-----------------------------------------------------------------------

   SMOOTH_OROG : if (max_orog_tiles == 1 .and. domain_type == "egrid") then
  
     allocate (dummy(imdl,jmdl))
     call gather(lsmask, imdl, jmdl, dummy)
     allocate(dummy2(imdl,jmdl))
     call gather(orog_tiles_elev(:,:,1), imdl, jmdl, dummy2)

!----------------------------------------------------------------------
!  smooth of 1 - peak chopping only
!  smooth of 2 - smoother/desmoother only
!  smooth of 3 - apply both smoothers.
!  each choice smooths the lateral boundaries in the same manner.
!  note: smoothers don't work with tiling.
!----------------------------------------------------------------------

     if (smooth == 1) then   ! peak chopping
       print*,"- SMOOTH TERRAIN BY PEAK CHOPPING USING ",num_smooth_passes1, " PASSES."
       call smdhld_egrid(imdl, jmdl, dummy2, dummy, 12, num_smooth_passes1)
     end if

     if (smooth == 2) then  ! smoother/desmoother only
       print*,"- SMOOTH TERRAIN BY SMOOTHER/DESMOOTHER USING ",num_smooth_passes2, " PASSES."
       call smth_desmth_egrid(dummy2, dummy, 1, imdl, 1, jmdl, 1, 1, num_smooth_passes2, 1) 
       call smdhld_egrid(imdl, jmdl, dummy2, dummy, 12, 0) ! for lateral boundaries only 
     end if

     if (smooth == 3) then ! both smoothers
       print*,"- SMOOTH TERRAIN BY SMOOTHER/DESMOOTHER USING ",num_smooth_passes2, " PASSES."
       call smth_desmth_egrid(dummy2, dummy, 1, imdl, 1, jmdl, 1, 1, num_smooth_passes2, 1) 
       print*,"- SMOOTH TERRAIN BY PEAK CHOPPING USING ",num_smooth_passes1, " PASSES."
       call smdhld_egrid(imdl, jmdl, dummy2, dummy, 12, num_smooth_passes1) 
     endif 

     print*,'- REMOVE WATERFALLS'
     call waterfalls_egrid(dummy, dummy2)

     print*,'- CHECK COASTLINES'
     call coastlines_egrid(dummy, dummy2)

! always do this last

     print*,'- SMOOTH INNER BOUNDARY'
     call inner_bound_egrid(imdl,jmdl,dummy2)

     call scatter(orog_tiles_elev(:,:,1), imdl, jmdl, dummy2)
     deallocate (dummy, dummy2)

   elseif (max_orog_tiles == 1 .and. domain_type == "bgrid") then

     allocate (dummy(imdl,jmdl))
     call gather(lsmask, imdl, jmdl, dummy)

!----------------------------------------------------------------------
!  note: the smoother/desmoother code will eliminate more than 90% of the
!  mountain peaks that are targeted by the peak chopping code.
!----------------------------------------------------------------------

     allocate (dummy2(imdl,jmdl))
     call gather(orog_tiles_elev(:,:,1), imdl, jmdl, dummy2)

     if (smooth == 1) then  ! smoother/desmoother only
       print*,"- SMOOTH TERRAIN BY PEAK CHOPPING USING ",num_smooth_passes1, " PASSES."
       call smdhld(dummy, dummy2, imdl, jmdl, num_smooth_passes1) 
     end if

     if (smooth == 2) then  ! smoother/desmoother only
       print*,"- SMOOTH TERRAIN BY SMOOTHER/DESMOOTHER USING ",num_smooth_passes2, " PASSES."
       call smth_desmth(dummy2, dummy, 1, imdl, 1, jmdl, 1, 1, num_smooth_passes2) 
!    the peak chopping code contains the lateral boundary smoothing, so call it
!    with zero passes to do just boundaries.
       call smdhld(dummy, dummy2, imdl, jmdl, 0) 
     end if

     if (smooth == 3) then  ! both
       print*,"- SMOOTH TERRAIN BY SMOOTHER/DESMOOTHER USING ",num_smooth_passes2, " PASSES."
       call smth_desmth(dummy2, dummy, 1, imdl, 1, jmdl, 1, 1, num_smooth_passes2) 
       print*,"- SMOOTH TERRAIN BY PEAK CHOPPING USING ",num_smooth_passes1, " PASSES."
       call smdhld(dummy, dummy2, imdl, jmdl, num_smooth_passes1) 
     end if

     print*,'- REMOVE WATERFALLS'
     call waterfalls(dummy,dummy2)

     print*,'- CHECK COASTLINES'
     call coastlines(dummy, dummy2)

!     always do this last
!
!MEP - inappropriate for b-grid, and problematic for moving nest methodology

!     print*,'- SMOOTH TERRAIN ALONG INNER BOUNDARY'
!     call inner_bound(dummy, dummy2, imdl, jmdl)

     deallocate (dummy)
     call scatter(orog_tiles_elev(:,:,1), imdl, jmdl, dummy2)
     deallocate (dummy2)

   elseif (domain_type == "gaussian") then

     print*,"- NEED TO ADD SMOOTHING FOR GFS GRIDS"

   elseif (max_orog_tiles == 1) then  ! this branch for ndfd grids

     print*,'- REMOVE WATERFALLS'
     allocate (dummy2(imdl,jmdl))
     call gather(orog_tiles_elev(:,:,1), imdl, jmdl, dummy2)
     allocate (dummy(imdl,jmdl))
     call gather(lsmask, imdl, jmdl, dummy)
     call waterfalls(dummy,dummy2)
     print*,'- CHECK COASTLINES'
     call coastlines(dummy, dummy2)
     deallocate (dummy)
     call scatter(orog_tiles_elev(:,:,1), imdl, jmdl, dummy2)
     deallocate (dummy2)

   end if SMOOTH_OROG

 else 

   print*,'- MUST INPUT RAW OR PREPROCESSED OROGRAPHY DATA'
   call lsmask_orog_cleanup
   call mpi_abort(mpi_comm_world, 1, ier)

 end if USE_EXT_OROG

 777 continue

 wtrmask = 1.0 - lsmask    ! set water to "1" to fool interpolation

!-----------------------------------------------------------------------
! we restrict the total number of all land-related tile types
! (orog, veg, soil), so keep a running count of the remaining tiles.
!-----------------------------------------------------------------------

 do j = jstart_mdl, jend_mdl
   do i = istart_mdl, iend_mdl_4_loops(j)
     if (lsmask(i,j) > 0.0) then
       remaining_tot_tiles(i,j) = remaining_tot_tiles(i,j) / num_orog_tiles(i,j)
       remaining_tot_tiles(i,j) = max(remaining_tot_tiles(i,j),1)
     end if
   enddo
 enddo

!-----------------------------------------------------------------------
! create a logical version of the land/sea mask for later use for
! gribbing fields that are only valid for land or water points.
!-----------------------------------------------------------------------

 allocate (lbms_lnd_mdl(imdl,jmdl))

 lbms_lnd_mdl = .false.

 allocate(dummy(imdl,jmdl))
 call gather(lsmask, imdl, jmdl, dummy)

 if (thinned) then
   call fill(dummy)
 endif

 do j = 1, jmdl 
   do i = 1, imdl
     if (dummy(i,j) > 0.0) then
       lbms_lnd_mdl(i,j) = .true.
     end if
   enddo
 enddo

 if (maxval(dummy)==0.0)then  ! grid had no land points, can't
   kpds_mdl(4)=128            ! grib with a bitmap. 
 endif

 allocate (lbms_wtr_mdl(imdl,jmdl))

 lbms_wtr_mdl = .false.

 call gather(wtrmask, imdl, jmdl, dummy)

 if (thinned) then
   call fill(dummy)
 endif

 do j = 1, jmdl
   do i = 1, imdl
     if (dummy(i,j) > 0.0) then
       lbms_wtr_mdl(i,j) = .true.
     end if
   enddo
 enddo

 deallocate (dummy)

!-----------------------------------------------------------------------
! grib data.
!-----------------------------------------------------------------------

 if (use_external_mask) goto 888  ! mask read in from external file
                                  ! no need to write out to another file.

 allocate(lbms(imdl,jmdl))
 allocate(dummy(imdl,jmdl))

 call gather(lsmask, imdl, jmdl, dummy)

 if (thinned) then
   call fill(dummy)
 end if

 lbms = .false.   ! don't use bitmap

 lunout = 56
 if (grib2) then
   fname  = trim(domain_name)//"_slmask.grb2"
   call grib2_init(gfld)
   gfld%discipline = 2
   gfld%ipdtmpl(1) = 0  ! oct 10; parameter category
   gfld%ipdtmpl(2) = 0  ! oct 11; parameter
   gfld%idrtmpl    = 0
   gfld%idrtmpl(3) = 2  ! decimal scaling factor
   gfld%fld=reshape(dummy, (/imdl*jmdl/) )
 else
   fname    = trim(domain_name)//"_slmask.grb"
   kpds     = kpds_mdl
   kgds     = kgds_mdl
   kpds(5)  = 81    ! parameter number for land/sea mask
   kpds(22) = 2     ! scaling factor
   kpds(4)  = 128   ! don't use bitmap
 endif

 if (myrank == 0) then

   call baopenw(lunout, fname, ier)

   if (grib2) then
     call putgb2(lunout,gfld,ier)
   else
     call putgb (lunout, (imdl*jmdl), kpds, kgds, lbms,  &
                 dummy, ier)
   endif

   if (ier /= 0) then
     print*,"- ERROR GRIBBING LAND/SEA MASK, IER IS ", ier
     call mpi_abort(mpi_comm_world, 1, ier)
   end if

   call baclose(lunout, ier)

 end if 

 if (grib2) then
   call grib2_free(gfld)
 endif

 888 continue

!-----------------------------------------------------------------------
! now grib orography.
!-----------------------------------------------------------------------

 if (use_external_orog) goto 889
 
 lunout = 64
 if (grib2) then
   fname  = trim(domain_name)//"_elevtiles.grb2"
 else
   fname  = trim(domain_name)//"_elevtiles.grb"
 endif

 if (myrank == 0) call baopenw(lunout, fname, ier)

!-----------------------------------------------------------------------
! if one tile is selected, simply output the mean terrain.
!-----------------------------------------------------------------------

 MULTIPLE_TILES : if (max_orog_tiles == 1) then

   call gather(orog_tiles_elev(:,:,1), imdl, jmdl, dummy)

   if (thinned) then
     call fill(dummy)
   end if

   if(grib2) then
     call grib2_init(gfld)
     gfld%discipline = 2
     gfld%ipdtmpl(1) = 0  ! oct 10; parameter category
     gfld%ipdtmpl(2) = 7  ! oct 11; parameter
     gfld%idrtmpl    = 0
     gfld%idrtmpl(3) = 1  ! decimal scaling factor
     gfld%fld=reshape(dummy, (/imdl*jmdl/) )
   else
     kpds(22) = 1  ! scaling factor
     kpds(5)  = 8  ! parameter number of terrain height
   endif

   if (myrank == 0) then

     if (grib2) then
       call putgb2(lunout,gfld,ier)
     else
       call putgb (lunout, (imdl*jmdl), kpds, kgds, lbms, &
                   dummy, ier)
     endif

     if (ier /= 0) then
       print*,"- ERROR GRIBBING OROGRAPHY, IER IS ", ier
       call mpi_abort(mpi_comm_world, 1, ier)
     end if

   end if

   if (grib2) then
     call grib2_free(gfld)
   end if

 else ! more than one tile

   kpds(5)  = 81  ! need to ask nco how to grib tiles.
   kpds(22) = 0

   allocate (idummy(imdl,jmdl))
   call gather(num_orog_tiles, imdl, jmdl, idummy)

   dummy = float(idummy)

   if (thinned) then
     call fill(dummy)
   end if

   if (myrank == 0) then
     call putgb (lunout, (imdl*jmdl), kpds, kgds, lbms,  &
                 dummy, ier)
     if (ier /= 0) then
       print*,"- ERROR GRIBBING OROGRAPHY, IER IS ", ier
       call mpi_abort(mpi_comm_world, 1, ier)
     end if
   end if

   do tile = 1, max_orog_tiles

     kpds(22) = 1
     kpds(5)  = kpds(5) + 1

     call gather(orog_tiles_elev(:,:,tile), imdl, jmdl, dummy)

     if (thinned) then
       call fill(dummy)
     end if

     if (myrank == 0) then
       call putgb (lunout, (imdl*jmdl), kpds, kgds, lbms, &
                   dummy, ier)
       if (ier /= 0) then
         print*,"- ERROR GRIBBING OROGRAPHY, IER IS ", ier
         call mpi_abort(mpi_comm_world, 1, ier)
       end if
     end if

     kpds(22) = 3
     kpds(5)  = kpds(5) + 1

     call gather(orog_tiles_prcnt(:,:,tile), imdl, jmdl, dummy)

     if (thinned) then
       call fill(dummy)
     end if

     if (myrank == 0) then
       call putgb (lunout, (imdl*jmdl), kpds, kgds, lbms, &
                   dummy, ier)
       if (ier /= 0) then
         print*,"- ERROR GRIBBING OROGRAPHY, IER IS ", ier
         call mpi_abort(mpi_comm_world, 1, ier)
       end if
     endif ! myrank

   enddo

 endif MULTIPLE_TILES

!-----------------------------------------------------------------------
! output stnd dev of orog.  don't deallocate array.  it may be
! use later in the roughness length calculation.
!-----------------------------------------------------------------------

 if (orog_gwd_tiles) then

   call gather(orog_stnd_dev, imdl, jmdl, dummy)

   if (thinned) then
     call fill(dummy)
   end if

   if(grib2) then
     call grib2_init(gfld)
     gfld%ipdtmpl(1) = 0  ! oct 10; parameter category
     gfld%ipdtmpl(2) = 9  ! oct 11; parameter
     gfld%idrtmpl    = 0
     gfld%idrtmpl(3) = 1  ! decimal scaling factor
     gfld%fld=reshape(dummy, (/imdl*jmdl/) )
   else
     kpds(5)  = 9
   endif

   if (myrank == 0) then

     if(grib2) then
       call putgb2(lunout,gfld,ier)
     else
       call putgb (lunout, (imdl*jmdl), kpds, kgds, lbms, &
                   dummy, ier)
     endif

     if (ier /= 0) then
       print*,"- ERROR GRIBBING STND DEV OF OROG, IER IS ", ier
       call mpi_abort(mpi_comm_world, 1, ier)
     end if

   end if

   call gather(max_height, imdl, jmdl, dummy)

   if (thinned) then
     call fill(dummy)
   end if

   where (lbms_wtr_mdl) dummy = 0.0

   if(grib2) then
     gfld%ipdtmpl(1) = 0  ! oct 10; parameter category
     gfld%ipdtmpl(2) = 221  ! oct 11; parameter
     gfld%idrtmpl    = 0
     gfld%idrtmpl(3) = 1  ! decimal scaling factor
     gfld%fld=reshape(dummy, (/imdl*jmdl/) )
   else
     kpds(5) = 221
   endif

   if (myrank == 0) then
     if(grib2) then
       call putgb2(lunout,gfld,ier)
     else
       call putgb (lunout, (imdl*jmdl), kpds, kgds, lbms, &
                   dummy, ier)
     endif
     if (ier /= 0) then
       print*,"- ERROR GRIBBING MAXIMUM HEIGHT, IER IS ", ier
       call mpi_abort(mpi_comm_world, 1, ier)
     end if
   end if

   deallocate (max_height)

   call gather(sigma, imdl, jmdl, dummy)

   if (thinned) then
     call fill(dummy)
   end if

   where (lbms_wtr_mdl) dummy = 0.0

   if(grib2) then
     gfld%ipdtmpl(1) = 0  ! oct 10; parameter category
     gfld%ipdtmpl(2) = 102  ! oct 11; parameter
     gfld%idrtmpl    = 0
     gfld%idrtmpl(3) = 3  ! decimal scaling factor
     gfld%fld=reshape(dummy, (/imdl*jmdl/) )
   else
     kpds(22) = 3
     kpds(5)  = 102
   endif

   if (myrank == 0) then
     if(grib2) then
       call putgb2(lunout,gfld,ier)
     else
       call putgb (lunout, (imdl*jmdl), kpds, kgds, lbms, &
                   dummy, ier)
     endif
     if (ier /= 0) then
       print*,"- ERROR GRIBBING SIGMA, IER IS ", ier
       call mpi_abort(mpi_comm_world, 1, ier)
     end if
   end if

   deallocate(sigma)

   call gather(convexity, imdl, jmdl, dummy)

   if (thinned) then
     call fill(dummy)
   end if

   where (lbms_wtr_mdl) dummy = 0.0

   if(grib2) then
     gfld%ipdtmpl(1) = 0  ! oct 10; parameter category
     gfld%ipdtmpl(2) = 187  ! oct 11; parameter
     gfld%idrtmpl    = 0
     gfld%idrtmpl(3) = 3  ! decimal scaling factor
     gfld%fld=reshape(dummy, (/imdl*jmdl/) )
   else
     kpds(22) = 3
     kpds(5)  = 187
   endif

   if (myrank == 0) then
     if(grib2) then
       call putgb2(lunout,gfld,ier)
     else
       call putgb (lunout, (imdl*jmdl), kpds, kgds, lbms, &
                   dummy, ier)
     endif
     if (ier /= 0) then
       print*,"- ERROR GRIBBING CONVEXITY, IER IS ", ier
       call mpi_abort(mpi_comm_world, 1, ier)
     end if
   end if

   deallocate (convexity)

   call gather(gamma, imdl, jmdl, dummy)

   if (thinned) then
     call fill(dummy)
   end if

   where (lbms_wtr_mdl) dummy = 0.0

   if(grib2) then
     gfld%ipdtmpl(1) = 0  ! oct 10; parameter category
     gfld%ipdtmpl(2) = 103  ! oct 11; parameter
     gfld%idrtmpl    = 0
     gfld%idrtmpl(3) = 3  ! decimal scaling factor
     gfld%fld=reshape(dummy, (/imdl*jmdl/) )
   else
     kpds(22) = 3
     kpds(5)  = 103
   endif

   if (myrank == 0) then
     if(grib2) then
       call putgb2(lunout,gfld,ier)
     else
       call putgb (lunout, (imdl*jmdl), kpds, kgds, lbms, &
                   dummy, ier)
     endif
     if (ier /= 0) then
       print*,"- ERROR GRIBBING GAMMA, IER IS ", ier
       call mpi_abort(mpi_comm_world, 1, ier)
     end if
   end if

   deallocate (gamma)

   call gather(theta, imdl, jmdl, dummy)

   if (thinned) then
     call fill(dummy)
   end if

   where (lbms_wtr_mdl) dummy = 0.0

   if(grib2) then
     gfld%ipdtmpl(1) = 0  ! oct 10; parameter category
     gfld%ipdtmpl(2) = 101  ! oct 11; parameter
     gfld%idrtmpl    = 0
     gfld%idrtmpl(3) = 3  ! decimal scaling factor
     gfld%fld=reshape(dummy, (/imdl*jmdl/) )
   else
     kpds(22) = 3
     kpds(5)  = 101
   endif

   if (myrank == 0) then
     if(grib2) then
       call putgb2(lunout,gfld,ier)
     else
       call putgb (lunout, (imdl*jmdl), kpds, kgds, lbms, &
                   dummy, ier)
     endif
     if (ier /= 0) then
       print*,"- ERROR GRIBBING THETA, IER IS ", ier
       call mpi_abort(mpi_comm_world, 1, ier)
     end if
   end if

   deallocate (theta)

   call gather(oa(:,:,1), imdl, jmdl, dummy)

   if (thinned) then
     call fill(dummy)
   end if

   where (lbms_wtr_mdl) dummy = 0.0

   if(grib2) then
     gfld%ipdtmpl(1) = 0   ! oct 10; parameter category
     gfld%ipdtmpl(2) = 166  ! oct 11; parameter
     gfld%idrtmpl    = 0
     gfld%idrtmpl(3) = 3  ! decimal scaling factor
     gfld%fld=reshape(dummy, (/imdl*jmdl/) )
   else
     kpds(22) = 3
     kpds(5)  = 166
   endif

   if (myrank == 0) then
     if(grib2) then
       call putgb2(lunout,gfld,ier)
     else
       call putgb (lunout, (imdl*jmdl), kpds, kgds, lbms, &
                   dummy, ier)
     endif
     if (ier /= 0) then
       print*,"- ERROR GRIBBING OA1, IER IS ", ier
       call mpi_abort(mpi_comm_world, 1, ier)
     end if
   end if

   call gather(oa(:,:,2), imdl, jmdl, dummy)

   if (thinned) then
     call fill(dummy)
   end if

   where (lbms_wtr_mdl) dummy = 0.0

   if(grib2) then
     gfld%ipdtmpl(1) = 0   ! oct 10; parameter category
     gfld%ipdtmpl(2) = 167  ! oct 11; parameter
     gfld%idrtmpl    = 0
     gfld%idrtmpl(3) = 3  ! decimal scaling factor
     gfld%fld=reshape(dummy, (/imdl*jmdl/) )
   else
     kpds(22) = 3
     kpds(5)  = 167
   endif

   if (myrank == 0) then
     if(grib2) then
       call putgb2(lunout,gfld,ier)
     else
       call putgb (lunout, (imdl*jmdl), kpds, kgds, lbms, &
                   dummy, ier)
     endif
     if (ier /= 0) then
       print*,"- ERROR GRIBBING OA2, IER IS ", ier
       call mpi_abort(mpi_comm_world, 1, ier)
     end if
   end if

   call gather(oa(:,:,3), imdl, jmdl, dummy)

   if (thinned) then
     call fill(dummy)
   end if

   where (lbms_wtr_mdl) dummy = 0.0

   if(grib2) then
     gfld%ipdtmpl(1) = 0   ! oct 10; parameter category
     gfld%ipdtmpl(2) = 168  ! oct 11; parameter
     gfld%idrtmpl    = 0
     gfld%idrtmpl(3) = 3  ! decimal scaling factor
     gfld%fld=reshape(dummy, (/imdl*jmdl/) )
   else
     kpds(22) = 3
     kpds(5)  = 168
   endif

   if (myrank == 0) then
     if(grib2) then
       call putgb2(lunout,gfld,ier)
     else
       call putgb (lunout, (imdl*jmdl), kpds, kgds, lbms, &
                   dummy, ier)
     endif
     if (ier /= 0) then
       print*,"- ERROR GRIBBING OA3, IER IS ", ier
       call mpi_abort(mpi_comm_world, 1, ier)
     end if
   end if

   call gather(oa(:,:,4), imdl, jmdl, dummy)

   if (thinned) then
     call fill(dummy)
   end if

   where (lbms_wtr_mdl) dummy = 0.0

   if(grib2) then
     gfld%ipdtmpl(1) = 0   ! oct 10; parameter category
     gfld%ipdtmpl(2) = 169  ! oct 11; parameter
     gfld%idrtmpl    = 0
     gfld%idrtmpl(3) = 3  ! decimal scaling factor
     gfld%fld=reshape(dummy, (/imdl*jmdl/) )
   else
     kpds(22) = 3
     kpds(5)  = 169
   endif

   if (myrank == 0) then
     if(grib2) then
       call putgb2(lunout,gfld,ier)
     else
       call putgb (lunout, (imdl*jmdl), kpds, kgds, lbms, &
                   dummy, ier)
     endif
     if (ier /= 0) then
       print*,"- ERROR GRIBBING OA4, IER IS ", ier
       call mpi_abort(mpi_comm_world, 1, ier)
     end if
   end if

   deallocate (oa)

   call gather(ol(:,:,1), imdl, jmdl, dummy)

   if (thinned) then
     call fill(dummy)
   end if

   where (lbms_wtr_mdl) dummy = 0.0

   if(grib2) then
     gfld%ipdtmpl(1) = 0   ! oct 10; parameter category
     gfld%ipdtmpl(2) = 151  ! oct 11; parameter
     gfld%idrtmpl    = 0
     gfld%idrtmpl(3) = 3  ! decimal scaling factor
     gfld%fld=reshape(dummy, (/imdl*jmdl/) )
   else
     kpds(22) = 3
     kpds(5)  = 151
   endif

   if (myrank == 0) then
     if(grib2) then
       call putgb2(lunout,gfld,ier)
     else
       call putgb (lunout, (imdl*jmdl), kpds, kgds, lbms, &
                   dummy, ier)
     endif
     if (ier /= 0) then
       print*,"- ERROR GRIBBING OL1, IER IS ", ier
       call mpi_abort(mpi_comm_world, 1, ier)
     end if
   end if

   call gather(ol(:,:,2), imdl, jmdl, dummy)

   if (thinned) then
     call fill(dummy)
   end if

   where (lbms_wtr_mdl) dummy = 0.0

   if(grib2) then
     gfld%ipdtmpl(1) = 0   ! oct 10; parameter category
     gfld%ipdtmpl(2) = 152 ! oct 11; parameter
     gfld%idrtmpl    = 0
     gfld%idrtmpl(3) = 3  ! decimal scaling factor
     gfld%fld=reshape(dummy, (/imdl*jmdl/) )
   else
     kpds(22) = 3
     kpds(5)  = 152
   endif

   if (myrank == 0) then
     if(grib2) then
       call putgb2(lunout,gfld,ier)
     else
       call putgb (lunout, (imdl*jmdl), kpds, kgds, lbms, &
                   dummy, ier)
     endif
     if (ier /= 0) then
       print*,"- ERROR GRIBBING OL2, IER IS ", ier
       call mpi_abort(mpi_comm_world, 1, ier)
     end if
   end if

   call gather(ol(:,:,3), imdl, jmdl, dummy)

   if (thinned) then
     call fill(dummy)
   end if

   where (lbms_wtr_mdl) dummy = 0.0

   if(grib2) then
     gfld%ipdtmpl(1) = 0   ! oct 10; parameter category
     gfld%ipdtmpl(2) = 153 ! oct 11; parameter
     gfld%idrtmpl    = 0
     gfld%idrtmpl(3) = 3  ! decimal scaling factor
     gfld%fld=reshape(dummy, (/imdl*jmdl/) )
   else
     kpds(22) = 3
     kpds(5)  = 153
   endif

   if (myrank == 0) then
     if(grib2) then
       call putgb2(lunout,gfld,ier)
     else
       call putgb (lunout, (imdl*jmdl), kpds, kgds, lbms, &
                   dummy, ier)
     endif
     if (ier /= 0) then
       print*,"- ERROR GRIBBING OL3, IER IS ", ier
       call mpi_abort(mpi_comm_world, 1, ier)
     end if
   end if

   call gather(ol(:,:,4), imdl, jmdl, dummy)

   if (thinned) then
     call fill(dummy)
   end if

   where (lbms_wtr_mdl) dummy = 0.0

   if(grib2) then
     gfld%ipdtmpl(1) = 0   ! oct 10; parameter category
     gfld%ipdtmpl(2) = 154 ! oct 11; parameter
     gfld%idrtmpl    = 0
     gfld%idrtmpl(3) = 3  ! decimal scaling factor
     gfld%fld=reshape(dummy, (/imdl*jmdl/) )
   else
     kpds(22) = 3
     kpds(5)  = 154
   endif

   if (myrank == 0) then
     if(grib2) then
       call putgb2(lunout,gfld,ier)
     else
       call putgb (lunout, (imdl*jmdl), kpds, kgds, lbms, &
                   dummy, ier)
     endif
     if (ier /= 0) then
       print*,"- ERROR GRIBBING OL4, IER IS ", ier
       call mpi_abort(mpi_comm_world, 1, ier)
     end if
   end if

   deallocate (ol)

   if (grib2) then
     call grib2_free(gfld)
   endif

 endif   ! gwd terms

 deallocate (dummy)
 deallocate (lbms)

 if (myrank == 0) call baclose(lunout, ier)

 889 continue

 return

 end subroutine lsmask_orog_driver

!------------------------------------------------------------------------
! calculate terrain height using a simple bilinear technique.
! will not product gwd fields.
!------------------------------------------------------------------------

 subroutine calc_orog_bilinear

 use calc_latlons, only      : lat_mdl, lon_mdl

 use mpimod, only            : istart_mdl, iend_mdl, iend_mdl_4_loops, &
                               jstart_mdl, jend_mdl

 use program_setup, only     : orog_file

 use native_endianness, only : to_native_endianness, &
                               is_little_endian

 implicit none

 include 'mpif.h'

 integer, parameter      :: isrc=43200
 integer, parameter      :: jsrc=21600
 integer                 :: i, j, ii, jj, ip1, jp1
 integer                 :: istart_src, iend_src, jstart_src, jend_src
 integer                 :: ierr, iunit, jsrc_task
 integer*8               :: offset 
 integer*2, allocatable  :: topo_src(:,:)
 integer*2, parameter    :: topo_water_flag=-9999  
 
 real                    :: dlon_src, dlat_src, lon_11_src, lat_11_src
 real                    :: sum, w11, w12, w21, w22
 real                    :: xf, xx, yf, yy

!----------------------------------------------------------------------
! determine bounds of model grid within the 30-sec source data.
! assumes data is global lat/lon projection.
!----------------------------------------------------------------------

 print*,"- CALC OROGRAPHY USING SIMPLE BILINEAR."

 dlon_src   = 1.0 / 120.0
 dlat_src   = -(1.0 / 120.0)
 lon_11_src = -180.0 + (dlon_src*0.5)  ! lat point 1,1
 lat_11_src = 90.0 + (dlat_src*0.5)    ! lon point 1,1

 call find_bounds_ll(isrc, jsrc, lat_11_src, lon_11_src, dlat_src, dlon_src, &
                     istart_src, iend_src, jstart_src, jend_src)
 
 jsrc_task = jend_src-jstart_src + 1

!----------------------------------------------------------------------
! open and read terrain data
!----------------------------------------------------------------------

 print*,'- OPEN AND READ SOURCE FILE FOR OROGRAPHY: ',trim(orog_file)
 allocate (topo_src(isrc,jstart_src:jend_src))
 iunit=34
 call mpi_file_open(mpi_comm_world, orog_file, mpi_mode_rdonly, &
                    mpi_info_null, iunit, ierr)
 if (ierr /= 0) then
   print*,'- BAD OPEN, IERR IS: ', ierr
   call mpi_abort(mpi_comm_world, 1, ierr)
 endif
 offset=2_8*int(isrc,8)*(int(jstart_src,8)-1_8)
 call mpi_file_read_at(iunit, offset, topo_src, isrc*jsrc_task, &
                       mpi_integer2, mpi_status_ignore, ierr)
 if (ierr /= 0) then
   print*,'- BAD READ, IERR IS: ', ierr
   call mpi_abort(mpi_comm_world, 1, ierr)
 endif
 call mpi_file_close(iunit, ierr)

 if (is_little_endian) then
   do j = jstart_src, jend_src
   do i = 1, isrc
     call to_native_endianness(topo_src(i,j))
   enddo
   enddo
 endif

 do j = jstart_mdl, jend_mdl
   do i = istart_mdl, iend_mdl_4_loops(j)

     xx = (lon_mdl(i,j)-lon_11_src) / dlon_src + 1.0
     yy = (lat_mdl(i,j)-lat_11_src) / dlat_src + 1.0

     xf = xx - int(xx)
     yf = yy - int(yy)
     ii = int(xx)
     if (ii<1) ii=isrc+ii
     if (ii>isrc) ii=ii-isrc
     ip1=ii+1
     if (ip1>isrc) ip1=ip1-isrc
     jj = int(yy)
     if (jj==0) then
       jj=1
       jp1=1
     elseif (jj==jsrc) then
       jp1=jsrc
     else
       jp1=jj+1
     endif

     w11 = (1.-xf)*(1.-yf)
     w21 = xf*(1.-yf)
     w12 = (1.-xf)*yf
     w22 = xf*yf

     sum = 0.0
     if (topo_src(ii,jj) /= topo_water_flag) sum = sum + w11*float(topo_src(ii,jj))
     if (topo_src(ip1,jj) /= topo_water_flag) sum = sum + w21*float(topo_src(ip1,jj))
     if (topo_src(ii,jp1) /= topo_water_flag) sum = sum + w12*float(topo_src(ii,jp1))
     if (topo_src(ip1,jp1) /= topo_water_flag) sum = sum + w22*float(topo_src(ip1,jp1))

     orog_tiles_elev(i,j,1) =  sum

   enddo
 enddo

 deallocate (topo_src)

 num_orog_tiles = 1
 orog_tiles_prcnt = 1.0

 return

 end subroutine calc_orog_bilinear

!-----------------------------------------------------------------------
! determine orography
!-----------------------------------------------------------------------

 subroutine calc_orog

 use calc_latlons, only     : lat_mdl, lon_mdl, lat_first_mdl, lon_first_mdl

 use init_grib1, only       : kgds_mdl

 use ll2xy_utils, only      : ll2xy_egrid_pt, ll2xy_bgrid_pt, ll2xy_polar

 use mpimod, only           : istart_mdl, iend_mdl, iend_mdl_4_loops, &
                              jstart_mdl, jend_mdl, gather, myrank

 use program_setup, only    : dx_mdl, dx_gfs, dy_mdl, hemi_mdl, orient_lon_mdl, &
                              lat_11_mdl, lon_11_mdl, centlat_mdl, centlon_mdl, &
                              centlat_parent_mdl, centlon_parent_mdl, lonsperlat_mdl, orog_file, &
                              imdl, jmdl, domain_type, max_orog_tiles, &
                              orog_bin_width, orog_tile_threshold

 use native_endianness, only : to_native_endianness, &
                               is_little_endian

 implicit none

 include 'mpif.h'

 integer                 :: calc_tiles
 integer, allocatable    :: count(:,:), count_ocean(:,:)
 integer, allocatable    :: count_tile(:,:,:)
 integer, allocatable    :: count_nul(:,:),count_nll(:,:),count_nur(:,:),count_nlr(:,:)
!integer, parameter      :: isrc=108000
!integer, parameter      :: jsrc=54000
 integer, parameter      :: isrc=43200
 integer, parameter      :: jsrc=21600
 integer*2, allocatable  :: i_wrt_mdl_grid(:,:), j_wrt_mdl_grid(:,:)
 integer                 :: iend_src, istart_src
 integer                 :: jend_src, jstart_src, jsrc_task
 integer                 :: i, ii, iii, j, jj, nret
 integer                 :: ierr, iunit
 integer                 :: im1, ip1, jm1, jp1
 integer                 :: nearest_i, nearest_j
 integer, allocatable    :: nul(:,:), nll(:,:), nur(:,:), nlr(:,:)
 integer, allocatable    :: nul2(:,:), nll2(:,:), nur2(:,:), nlr2(:,:)
 integer, allocatable    :: num_tiles(:,:)
 integer*8               :: offset
 integer*2, parameter    :: off_grid_flag=-9999
 integer                 :: tile, total_tile
 integer*2, allocatable  :: topo_src(:,:)
 integer*2, parameter    :: topo_water_flag=-9999  

!cggg
 real :: topo_src_real

 real, allocatable       :: bin_ranges(:,:,:,:)
 real                    :: bin_width
 real                    :: crit_hgt, deltalon
 real                    :: dhdx, dhdy, dx, dy
 real                    :: dlat_src, dlon_src, elev
 real, allocatable       :: dx_src(:)
 real                    :: dy_src
 real                    :: hl, hlprim, hk
 real                    :: lat, lon 
 real                    :: lat_11_src, lon_11_src
 real, allocatable       :: lats_src(:),lons_src(:)
 real, allocatable       :: dum(:), ypts(:)
 real                    :: max_spread, percent
 real                    :: pi, rearth
 real, allocatable       :: sum_topo(:,:), sum_topo4(:,:)
 real, allocatable       :: sum_dhdx2(:,:), sum_dhdy2(:,:), sum_dhdxdy(:,:)
 real, allocatable       :: sum_tile(:,:,:)
 real, allocatable       :: topo(:,:), topo_max(:,:), topo_min(:,:)
 real                    :: topo_im1, topo_ip1, topo_jm1, topo_jp1
 real                    :: t, xnpu, xnpd
 real                    :: sigma2, xfpyfp, xfp2, yfp2
 real                    :: lat_src, lon_src, xgrid, ygrid

!----------------------------------------------------------------------
! determine bounds of model grid within the 30-sec source data.
! assumes data is global lat/lon projection.
!----------------------------------------------------------------------

 dlon_src   = 1.0 / 120.0
 dlat_src   = -(1.0 / 120.0)
 lon_11_src = -180.0 + (dlon_src*0.5)  ! lat point 1,1
 lat_11_src = 90.0 + (dlat_src*0.5)    ! lon point 1,1
!dlon_src   = 1.0 / 300.0
!dlat_src   = (1.0 / 300.0)
!lon_11_src = -180.0 + (dlon_src*0.5)  ! lat point 1,1
!lat_11_src = -90.0 + (dlat_src*0.5)    ! lon point 1,1

 call find_bounds_ll(isrc, jsrc, lat_11_src, lon_11_src, dlat_src, dlon_src, &
                     istart_src, iend_src, jstart_src, jend_src)
 
 jsrc_task = jend_src-jstart_src + 1

!----------------------------------------------------------------------
! open and read terrain data
!----------------------------------------------------------------------

 print*,'- OPEN AND READ SOURCE FILE FOR OROGRAPHY: ',trim(orog_file)
 allocate (topo_src(isrc,jstart_src:jend_src))
 iunit=34
 call mpi_file_open(mpi_comm_world, orog_file, mpi_mode_rdonly, &
                    mpi_info_null, iunit, ierr)
 if (ierr /= 0) then
   print*,'- BAD OPEN, IERR IS: ', ierr
   call mpi_abort(mpi_comm_world, 1, ierr)
 endif
 offset=2_8*int(isrc,8)*(int(jstart_src,8)-1_8)
 call mpi_file_read_at(iunit, offset, topo_src, isrc*jsrc_task, &
                       mpi_integer2, mpi_status_ignore, ierr)
 if (ierr /= 0) then
   print*,'- BAD READ, IERR IS: ', ierr
   call mpi_abort(mpi_comm_world, 1, ierr)
 endif
 call mpi_file_close(iunit, ierr)

 if (is_little_endian) then
   do j = jstart_src, jend_src
   do i = 1, isrc
     call to_native_endianness(topo_src(i,j))
   enddo
   enddo
 endif

!----------------------------------------------------------------------
! some gravity wave drag fields.
!----------------------------------------------------------------------

 pi     = atan(1.0) * 4.0
 rearth = 6.371e6  ! meters
 dy_src = pi * rearth / float(jsrc)   ! in meters
 dy_src = dy_src * sign(1.0,dlat_src) * 2.0   ! use centered differencing

! note: ecmwf uses a dx that is constant in km, not grid points
 allocate(dx_src(jstart_src:jend_src))
 do j = jstart_src, jend_src
   lat = lat_11_src + (j-1)*dlat_src
   lat = lat * pi / 180.0
   dx_src(j) = 2.0 * (2.0*pi*rearth/float(isrc)) * cos(lat)  ! extra two
                                                             ! for centered difference
 enddo

!----------------------------------------------------------------------
! for each source grid point, store the nearest i/j on model grid
! because this calculation is very expensive for e-grids.
!----------------------------------------------------------------------

 allocate (i_wrt_mdl_grid(istart_src:iend_src,jstart_src:jend_src))
 i_wrt_mdl_grid=off_grid_flag
 allocate (j_wrt_mdl_grid(istart_src:iend_src,jstart_src:jend_src))
 j_wrt_mdl_grid=off_grid_flag

 allocate(count(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))
 count=0
 allocate(sum_topo(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))
 sum_topo=0.
 allocate(count_ocean(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))
 count_ocean=0
 allocate(max_height(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))
 max_height = -9999.9

 if (trim(domain_type) == "gaussian") then 
   allocate (lats_src(jstart_src:jend_src))
   allocate (lons_src(jstart_src:jend_src))
   lons_src = 0.0
   do j = jstart_src, jend_src
     lats_src(j) = lat_11_src + (j-1)*dlat_src
   enddo
   allocate (ypts(jstart_src:jend_src))
   allocate (dum(jstart_src:jend_src))
   call gdswiz04(kgds_mdl,-1,jsrc_task,-999.9,dum,ypts,lons_src,lats_src, &
               nret, 0, dum, dum)
   deallocate (dum, lons_src, lats_src)
   do j = jstart_src,jend_src
     nearest_j = nint(ypts(j))
     if (nearest_j < jstart_mdl .or. nearest_j > jend_mdl) cycle
     jj = nearest_j
     if (nearest_j > jmdl/2) jj = jmdl - nearest_j + 1
     do i = istart_src, iend_src
       lon_src = lon_11_src + (i-1)*dlon_src
       nearest_i = nint(lon_src / dx_gfs(nearest_j) + 1.0)   
       if (nearest_i > lonsperlat_mdl(jj)) then
         nearest_i = nearest_i - lonsperlat_mdl(jj)
       else if (nearest_i < 1) then
         nearest_i = nearest_i + lonsperlat_mdl(jj)
       end if
       i_wrt_mdl_grid(i,j)=nearest_i
       j_wrt_mdl_grid(i,j)=nearest_j
       if (topo_src(i,j) /= topo_water_flag) then ! non-ocean point
         topo_src_real = float(topo_src(i,j))
         sum_topo(nearest_i,nearest_j) = sum_topo(nearest_i,nearest_j) + &
                                         topo_src_real
         max_height(nearest_i,nearest_j) = max(max_height(nearest_i,nearest_j), topo_src_real)
       endif
       if (topo_src(i,j) == topo_water_flag) then
         count_ocean(nearest_i,nearest_j)=count_ocean(nearest_i,nearest_j)+1
       endif
       count(nearest_i,nearest_j) = count(nearest_i,nearest_j)+1
     enddo
   enddo
   deallocate (ypts)
 elseif (trim(domain_type) == "latlon") then  ! regular lat/lon
   do j = jstart_src, jend_src
     lat_src = lat_11_src + (j-1)*dlat_src
     nearest_j = nint((lat_src - lat_11_mdl) / dy_mdl + 1.0)
     if (nearest_j < jstart_mdl .or. nearest_j > jend_mdl) cycle
     do i = istart_src, iend_src
       lon_src = lon_11_src + (i-1)*dlon_src
       nearest_i = nint(1.0 + mod((lon_src-lon_11_mdl)+3600,360.0)/dx_mdl)
       if (nearest_i < 1 .or. nearest_i > imdl) cycle  ! allow for regional grids
       i_wrt_mdl_grid(i,j)=nearest_i
       j_wrt_mdl_grid(i,j)=nearest_j
       if (topo_src(i,j) /= topo_water_flag) then ! non-ocean point
         sum_topo(nearest_i,nearest_j) = sum_topo(nearest_i,nearest_j) + &
                                               float(topo_src(i,j))
       endif
       if (topo_src(i,j) == topo_water_flag) then
         count_ocean(nearest_i,nearest_j)=count_ocean(nearest_i,nearest_j)+1
       endif
       count(nearest_i,nearest_j) = count(nearest_i,nearest_j)+1
     enddo
   enddo
 elseif (trim(domain_type) == "polar") then  ! polar stereographic
   do j = jstart_src,jend_src
     lat_src = lat_11_src + (j-1)*dlat_src
   do i = istart_src, iend_src
     iii = i
     if (i < 1) iii = i + isrc
     if (i > isrc) iii = i - isrc
     lon_src = lon_11_src + (iii-1)*dlon_src
     call ll2xy_polar(lat_11_mdl, lon_11_mdl, orient_lon_mdl, &
                      dx_mdl, dy_mdl, hemi_mdl, lat_src, lon_src, &
                      xgrid, ygrid) 
     nearest_i = nint(xgrid)
     nearest_j = nint(ygrid)
     if (nearest_i >= istart_mdl .and. nearest_i <= iend_mdl .and. &
         nearest_j >= jstart_mdl .and. nearest_j <= jend_mdl) then
       i_wrt_mdl_grid(i,j)=nearest_i
       j_wrt_mdl_grid(i,j)=nearest_j
       if (topo_src(iii,j) /= topo_water_flag) then ! non-ocean point
         sum_topo(nearest_i,nearest_j) = sum_topo(nearest_i,nearest_j) + &
                                               float(topo_src(iii,j))
       endif
       if (topo_src(iii,j) == topo_water_flag) then ! ocean point
         count_ocean(nearest_i,nearest_j)=count_ocean(nearest_i,nearest_j)+1
       endif
       count(nearest_i,nearest_j) = count(nearest_i,nearest_j)+1
     endif
   enddo
   enddo
 elseif (trim(domain_type) == "egrid") then  ! e-grid
   do j = jstart_src,jend_src
     lat_src = lat_11_src + (j-1)*dlat_src
   do i = istart_src, iend_src
     iii = i
     if (i < 1) iii = i + isrc
     if (i > isrc) iii = i - isrc
     lon_src = lon_11_src + (iii-1)*dlon_src
     call ll2xy_egrid_pt(lat_src, lon_src, imdl, jmdl, &
                         centlat_mdl, centlon_mdl,   &
                        -(dx_mdl), dy_mdl, nearest_i, nearest_j)
     if (nearest_i >= istart_mdl .and. nearest_i <= iend_mdl .and. &
         nearest_j >= jstart_mdl .and. nearest_j <= jend_mdl) then
       i_wrt_mdl_grid(i,j)=nearest_i
       j_wrt_mdl_grid(i,j)=nearest_j
       if (topo_src(iii,j) /= topo_water_flag) then ! non-ocean point
         sum_topo(nearest_i,nearest_j) = sum_topo(nearest_i,nearest_j) + &
                                               float(topo_src(iii,j))
         max_height(nearest_i,nearest_j) = max(max_height(nearest_i,nearest_j), float(topo_src(iii,j)))
       endif
       if (topo_src(iii,j) == topo_water_flag) then
         count_ocean(nearest_i,nearest_j)=count_ocean(nearest_i,nearest_j)+1
       endif
       count(nearest_i,nearest_j) = count(nearest_i,nearest_j)+1
     endif
   enddo
   enddo
 elseif (trim(domain_type) == "bgrid") then ! b-grid
   do j = jstart_src,jend_src
     lat_src = lat_11_src + (j-1)*dlat_src
   do i = istart_src, iend_src
     iii = i
     if (i < 1) iii = i + isrc
     if (i > isrc) iii = i - isrc
     lon_src = lon_11_src + (iii-1)*dlon_src
     call ll2xy_bgrid_pt_loc(centlat_parent_mdl(1), centlon_parent_mdl(1), dy_mdl, dx_mdl, &
                         lat_first_mdl, lon_first_mdl, imdl, jmdl, lat_src, lon_src, nearest_i, nearest_j) 
     if (nearest_i >= istart_mdl .and. nearest_i <= iend_mdl .and. &
         nearest_j >= jstart_mdl .and. nearest_j <= jend_mdl) then
       i_wrt_mdl_grid(i,j)=nearest_i
       j_wrt_mdl_grid(i,j)=nearest_j
       if (topo_src(iii,j) /= topo_water_flag) then ! non-ocean point
         sum_topo(nearest_i,nearest_j) = sum_topo(nearest_i,nearest_j) + &
                                               float(topo_src(iii,j))
         max_height(nearest_i,nearest_j) = max(max_height(nearest_i,nearest_j), float(topo_src(iii,j)))
       endif
       if (topo_src(iii,j) == topo_water_flag) then
         count_ocean(nearest_i,nearest_j)=count_ocean(nearest_i,nearest_j)+1
       endif
       count(nearest_i,nearest_j) = count(nearest_i,nearest_j)+1
     endif
   enddo
   enddo
 else
   print*,'- UNDEFINED MAP PROJECTION. STOP.'
   call mpi_abort(mpi_comm_world, 1, ierr)
 endif

 allocate(topo(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))

 do j = jstart_mdl, jend_mdl
  do i = istart_mdl, iend_mdl_4_loops(j)
    if (count(i,j) == 0) then
      print*,'- NO SOURCE DATA WITHIN MODEL GRID AT I/J: ', i,j
      call mpi_abort(mpi_comm_world, 1, ierr)
    endif
    topo(i,j)       = sum_topo(i,j)/float(count(i,j))
    max_height(i,j) = max( (max_height(i,j)-topo(i,j)) , 0.0)
  enddo
 enddo

 allocate(sum_dhdx2(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))
 sum_dhdx2 = 0.0
 allocate(sum_dhdy2(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))
 sum_dhdy2 = 0.0
 allocate(sum_dhdxdy(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))
 sum_dhdxdy = 0.0

 do j = jstart_src, jend_src
 do i = istart_src, iend_src
   ii = i_wrt_mdl_grid(i,j)
   jj = j_wrt_mdl_grid(i,j)
   if (ii /= off_grid_flag .and. jj /= off_grid_flag) then
     iii = i
     if (i < 1) iii = i + isrc
     if (i > isrc) iii = i - isrc
     im1 = iii - 1
     if (im1 < 1) im1 = im1 + isrc
     ip1 = iii + 1
     if (ip1 > isrc) ip1 = ip1 - isrc
     dx = dx_src(j)
     dy = dy_src
     jm1 = j-1
     if (jm1 < 1) then
       jm1 = 1  ! take one sided derivative
       dy = dy_src * 0.5
     endif
     jp1 = j+1
     if (jp1 > jsrc) then
       jp1 = jsrc  ! take one sided derivative
       dy = dy_src * 0.5
     endif
     if (topo_src(im1,j) == topo_water_flag) then
       topo_im1 = 0.0  ! ocean
     else
       topo_im1 = float(topo_src(im1,j))
     end if
     if (topo_src(ip1,j) == topo_water_flag) then
       topo_ip1 = 0.0  ! ocean
     else
       topo_ip1 = float(topo_src(ip1,j))
     end if
     if (topo_src(iii,jm1) == topo_water_flag) then
       topo_jm1 = 0.0  ! ocean
     else
       topo_jm1 = float(topo_src(iii,jm1))
     end if
     if (topo_src(iii,jp1) == topo_water_flag) then
       topo_jp1 = 0.0  ! ocean
     else
       topo_jp1 = float(topo_src(iii,jp1))
     end if
     dhdx = (topo_ip1-topo_im1) / dx
     dhdy = (topo_jp1-topo_jm1) / dy
     sum_dhdx2(ii,jj) = sum_dhdx2(ii,jj) +  dhdx**2
     sum_dhdy2(ii,jj) = sum_dhdy2(ii,jj) +  dhdy**2
     sum_dhdxdy(ii,jj) = sum_dhdxdy(ii,jj) +  dhdx*dhdy
   endif
 enddo
 enddo

 allocate(theta(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))
 theta=0.0
 allocate(gamma(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))
 gamma=0.0
 allocate(sigma(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))
 sigma=0.0

 do j = jstart_mdl, jend_mdl
  do i = istart_mdl, iend_mdl_4_loops(j)
    xfp2 = sum_dhdx2(i,j) / float(count(i,j))
    yfp2 = sum_dhdy2(i,j) / float(count(i,j))
    xfpyfp = sum_dhdxdy(i,j) / float(count(i,j))
    hk = 0.5 * (xfp2 + yfp2)
    hl = 0.5 * (xfp2 - yfp2)
    hlprim = sqrt(hl**2 + xfpyfp**2)
    theta(i,j) = 0.5 * atan2(xfpyfp,hl)
    sigma2 = hk + hlprim
    sigma(i,j) = sqrt(sigma2)
    if (sigma2 /= 0.0 .and. hk >= hlprim) then
      gamma(i,j) = sqrt( (hk - hlprim) / sigma2 )
    end if
  enddo
 enddo

 deallocate (sum_dhdx2, sum_dhdy2, sum_dhdxdy)

 sum_topo=0.
 allocate(sum_topo4(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))
 sum_topo4=0.

 do j = jstart_src, jend_src
 do i = istart_src, iend_src
   ii = i_wrt_mdl_grid(i,j)
   jj = j_wrt_mdl_grid(i,j)
   if (ii /= off_grid_flag .and. jj /= off_grid_flag) then
     iii = i
     if (i < 1) iii = i + isrc
     if (i > isrc) iii = i - isrc
     if (topo_src(iii,j) /= topo_water_flag) then
       sum_topo(ii,jj)  = sum_topo(ii,jj) + (topo(ii,jj) - float(topo_src(iii,j)))**2
       sum_topo4(ii,jj) = sum_topo4(ii,jj) + (topo(ii,jj) - float(topo_src(iii,j)))**4
     else  ! source grid is ocean
       sum_topo(ii,jj)  = sum_topo(ii,jj) + topo(ii,jj)**2
       sum_topo4(ii,jj) = sum_topo4(ii,jj) + topo(ii,jj)**4
     endif
   endif
 enddo
 enddo

 allocate(convexity(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))
 convexity = 0.
 do j = jstart_mdl, jend_mdl
  do i = istart_mdl, iend_mdl_4_loops(j)
    orog_stnd_dev(i,j) = sqrt( sum_topo(i,j)/float(count(i,j)) )
    if (orog_stnd_dev(i,j) > 1.0) then
       convexity(i,j) = ( sum_topo4(i,j) / float(count(i,j)) ) / &
                          orog_stnd_dev(i,j)**4
    end if
  enddo
 enddo

 deallocate(sum_topo, sum_topo4)

 allocate (nul(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))
 allocate (nll(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))
 allocate (nur(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))
 allocate (nlr(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))

 nul=0; nll=0; nur=0; nlr=0

 allocate (nul2(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))
 allocate (nll2(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))
 allocate (nur2(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))
 allocate (nlr2(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))

 nul2=0; nll2=0; nur2=0; nlr2=0

 allocate (count_nul(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))
 allocate (count_nll(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))
 allocate (count_nur(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))
 allocate (count_nlr(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))

 count_nul=0; count_nll=0; count_nur=0; count_nlr=0

 do j = jstart_src, jend_src
 do i = istart_src, iend_src
   ii = i_wrt_mdl_grid(i,j)
   jj = j_wrt_mdl_grid(i,j)
   if (ii /= off_grid_flag .and. jj /= off_grid_flag) then
     iii = i
     if (i < 1) iii = i + isrc
     if (i > isrc) iii = i - isrc
     crit_hgt = 1111.6 - 0.878 * orog_stnd_dev(ii,jj)
     lat = lat_11_src + (j-1)*dlat_src
     lon = lon_11_src + (iii-1)*dlon_src
     deltalon = mod((lon-lon_mdl(ii,jj))+3600,360.0)
     topo_src_real = float(topo_src(iii,j))
     if (lat >= lat_mdl(ii,jj)) then
       if (deltalon > 180.0) then
         if (topo_src_real > topo(ii,jj)) nul(ii,jj) = nul(ii,jj) + 1
         if (topo_src_real > crit_hgt) nul2(ii,jj) = nul2(ii,jj) + 1
         count_nul(ii,jj) = count_nul(ii,jj) + 1
       else
         if (topo_src_real > topo(ii,jj)) nur(ii,jj) = nur(ii,jj) + 1
         if (topo_src_real > crit_hgt) nur2(ii,jj) = nur2(ii,jj) + 1
         count_nur(ii,jj) = count_nur(ii,jj) + 1
       endif
     else
       if (deltalon > 180.0) then
         if (topo_src_real > topo(ii,jj)) nll(ii,jj) = nll(ii,jj) + 1
         if (topo_src_real > crit_hgt) nll2(ii,jj) = nll2(ii,jj) + 1
         count_nll(ii,jj) = count_nll(ii,jj) + 1
       else
         if (topo_src_real > topo(ii,jj)) nlr(ii,jj) = nlr(ii,jj) + 1
         if (topo_src_real > crit_hgt) nlr2(ii,jj) = nlr2(ii,jj) + 1
         count_nlr(ii,jj) = count_nlr(ii,jj) + 1
       endif
     endif
   endif
 enddo
 enddo

 allocate (oa(istart_mdl:iend_mdl,jstart_mdl:jend_mdl,4))
 oa = 0.0
 allocate (ol(istart_mdl:iend_mdl,jstart_mdl:jend_mdl,4))
 ol = 0.0
 
 do j = jstart_mdl, jend_mdl
 do i = istart_mdl, iend_mdl_4_loops(j)
! west wind
   xnpu = float(nul(i,j)) + float(nll(i,j))
   xnpd = float(nur(i,j)) + float(nlr(i,j))
   if (xnpd /= xnpu) oa(i,j,1) = 1.0 - xnpd / max(xnpu, 1.0)
   t = oa(i,j,1)
   oa(i,j,1) = sign ( min(abs(t), 1.0 ), t)
   ol(i,j,1) = (float(nul2(i,j)) + float(nll2(i,j))) /   &
               (float(count_nul(i,j)) + float(count_nll(i,j)))
! south wind
   xnpu = float(nll(i,j)) + float(nlr(i,j))
   xnpd = float(nul(i,j)) + float(nur(i,j))
   if (xnpd /= xnpu) oa(i,j,2) = 1.0 - xnpd / max(xnpu, 1.0)
   t = oa(i,j,2)
   oa(i,j,2) = sign ( min(abs(t), 1.0 ), t)
   ol(i,j,2) = (float(nll2(i,j)) + float(nlr2(i,j))) /   &
               (float(count_nll(i,j)) + float(count_nlr(i,j)))
! southwest wind
   xnpu = float(nll(i,j)) + &
          0.5 * ( float(nul(i,j)) + float(nlr(i,j)) )
   xnpd = float(nur(i,j)) + &
          0.5 * ( float(nul(i,j)) + float(nlr(i,j)) )
   if (xnpd /= xnpu) oa(i,j,3) = 1.0 - xnpd / max(xnpu, 1.0)
   t = oa(i,j,3)
   oa(i,j,3) = sign ( min(abs(t), 1.0 ), t)
   ol(i,j,3) = (float(nul2(i,j)) + float(nlr2(i,j))) /   &
               (float(count_nul(i,j)) + float(count_nlr(i,j)))
! northwest wind
   xnpu = float(nul(i,j)) + &
          0.5 * ( float(nur(i,j)) + float(nll(i,j)) )
   xnpd = float(nlr(i,j)) + &
          0.5 * ( float(nur(i,j)) + float(nll(i,j)) )
   if (xnpd /= xnpu) oa(i,j,4) = 1.0 - xnpd / max(xnpu, 1.0)
   t = oa(i,j,4)
   oa(i,j,4) = sign ( min(abs(t), 1.0 ), t)
   ol(i,j,4) = (float(nur2(i,j)) + float(nll2(i,j))) /   &
               (float(count_nur(i,j)) + float(count_nll(i,j)))
 enddo
 enddo

 deallocate (count_nll, count_nur, count_nul, count_nlr)
 deallocate (nul, nul2, nll, nll2, nur, nur2, nlr, nlr2)

 TILING: if (max_orog_tiles == 1) then
   print*,"- ONLY ONE OROGRAPHY TILE SELECTED"
   num_orog_tiles = 1
   orog_tiles_elev(:,:,1) = topo
   orog_tiles_prcnt(:,:,1) = 1.
   deallocate(topo, topo_src, i_wrt_mdl_grid, j_wrt_mdl_grid)
   return
 else   ! find orography tiles
   print*,"- MULTIPLE OROGRAPHY TILES SELECTED"
   allocate(topo_max(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))
   allocate(topo_min(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))
   topo_max=-99999.
   topo_min=99999.
   do j = jstart_src,jend_src
   do i = istart_src, iend_src
     ii = i_wrt_mdl_grid(i,j)
     jj = j_wrt_mdl_grid(i,j)
     if (ii /= off_grid_flag .and. jj /= off_grid_flag) then
       iii = i
       if (i < 1) iii = i + isrc
       if (i > isrc) iii = i - isrc
       if (topo_src(iii,j) /= topo_water_flag) then
         topo_max(ii,jj) = max(topo_max(ii,jj),float(topo_src(iii,j)))
         topo_min(ii,jj) = min(topo_min(ii,jj),float(topo_src(iii,j)))
       else  ! source grid is ocean
         topo_max(ii,jj) = max(topo_max(ii,jj),0.0)
         topo_min(ii,jj) = min(topo_min(ii,jj),0.0)
       endif
     endif
   enddo
   enddo

! set up bin ranges

   allocate(num_tiles(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))
   allocate(bin_ranges(istart_mdl:iend_mdl,jstart_mdl:jend_mdl,max_orog_tiles,2))
   num_tiles=1
   bin_ranges=0.
   do j=jstart_mdl,jend_mdl
     do i=istart_mdl,iend_mdl_4_loops(j)
       if(lsmask(i,j) > 0.0) then
!vic method
!         max_spread = max( (topo_max(i,j)-topo(i,j)),  &
!                           (topo(i,j)-topo_min(i,j)) )
!         calc_tiles = 2.0 * max_spread / orog_bin_width
         max_spread = topo_max(i,j)-topo_min(i,j)
         calc_tiles = max_spread / orog_bin_width
         if (calc_tiles > max_orog_tiles) then
           num_tiles(i,j) = max_orog_tiles
         else if (calc_tiles < 1) then
           num_tiles(i,j) = 1
         else
           num_tiles(i,j) = calc_tiles
         end if
!vic method
!         bin_width = 2.0 * max_spread / float(num_tiles(i,j))
!         do tile = 1, num_tiles(i,j)
!           bin_ranges(i,j,tile,1) = topo(i,j) - max_spread +       &
!                                 (bin_width * float((tile-1)))
!           bin_ranges(i,j,tile,2) = topo(i,j) - max_spread +       &
!                                 (bin_width * float(tile))
!         enddo
!end vic method
         bin_width = max_spread / float(num_tiles(i,j))
         do tile = 1, num_tiles(i,j)
           bin_ranges(i,j,tile,1) = topo_min(i,j) +       &
                                 (bin_width * float((tile-1)))
           bin_ranges(i,j,tile,2) = topo_min(i,j) +       &
                                 (bin_width * float(tile))
         enddo
         bin_ranges(i,j,1,1)              = bin_ranges(i,j,1,1) - 0.01
         bin_ranges(i,j,num_tiles(i,j),2) = bin_ranges(i,j,num_tiles(i,j),2) + 0.01
       endif
     enddo
   enddo
 end if TILING

 allocate(sum_tile(istart_mdl:iend_mdl,jstart_mdl:jend_mdl,max_orog_tiles))
 allocate(count_tile(istart_mdl:iend_mdl,jstart_mdl:jend_mdl,max_orog_tiles))

 sum_tile=0.
 count_tile=0
 do j = jstart_src,jend_src
 do i = istart_src, iend_src
   ii = i_wrt_mdl_grid(i,j)
   jj = j_wrt_mdl_grid(i,j)
   if (ii /= off_grid_flag .and. jj /= off_grid_flag) then
     if (lsmask(ii,jj) > 0.0) then
       iii = i
       if (i < 1) iii = i + isrc
       if (i > isrc) iii = i - isrc
       if (topo_src(iii,j) /= topo_water_flag) then
         elev=float(topo_src(iii,j))
       else
         elev=0.
       endif
       do tile = 1, num_tiles(ii,jj)
         if(elev >= bin_ranges(ii,jj,tile,1)   .and.   &
            elev <  bin_ranges(ii,jj,tile,2) ) then
            count_tile(ii,jj,tile) = count_tile(ii,jj,tile) + 1
            sum_tile(ii,jj,tile)  = sum_tile(ii,jj,tile) + elev
         end if
       enddo
     endif
   endif
 enddo
 enddo

 deallocate(topo_src)
 deallocate(bin_ranges)

!----------------------------------------------------------------------
! the user can select a percent threshold, below which he does
! not want a tile. merge the tiles below the threshold with an 
! adjacent bin.
!----------------------------------------------------------------------

 do j=jstart_mdl,jend_mdl
   do i=istart_mdl,iend_mdl_4_loops(j)
     if(lsmask(i,j) > 0.0) then
       total_tile=0
       do tile=1, num_tiles(i,j)
         total_tile = total_tile + count_tile(i,j,tile)
       enddo
       do tile = 1, (num_tiles(i,j)-1)
         percent= float(count_tile(i,j,tile)) / &
                  float(total_tile)
         if (percent > 0.0 .and. percent < orog_tile_threshold) then
           count_tile(i,j,tile+1) = count_tile(i,j,tile+1) + &
                                        count_tile(i,j,tile)
           count_tile(i,j,tile) = 0.0
           sum_tile(i,j,tile+1) = sum_tile(i,j,tile+1) + &
                                      sum_tile(i,j,tile)
           sum_tile(i,j,tile) = 0.0
         end if
       enddo
       do tile = num_tiles(i,j), 2, -1
         percent= float(count_tile(i,j,tile)) / &
                  float(total_tile)
         if (percent > 0.0 .and. percent < orog_tile_threshold) then
           count_tile(i,j,tile-1) = count_tile(i,j,tile-1) + &
                                        count_tile(i,j,tile)
           count_tile(i,j,tile) = 0.0
           sum_tile(i,j,tile-1) = sum_tile(i,j,tile-1) + &
                                      sum_tile(i,j,tile)
           sum_tile(i,j,tile) = 0.0
         end if
       enddo
     endif
   enddo
 enddo

 do j = jstart_mdl, jend_mdl
   do i = istart_mdl, iend_mdl_4_loops(j)
     if(lsmask(i,j) > 0.0) then
       total_tile=0
       do tile=1, num_tiles(i,j)
         total_tile = total_tile + count_tile(i,j,tile)
       enddo
       do tile=1, num_tiles(i,j)
         if (count_tile(i,j,tile) > 0) then
           num_orog_tiles(i,j) = num_orog_tiles(i,j) + 1
           orog_tiles_prcnt(i,j,num_orog_tiles(i,j))=  &
                                       float(count_tile(i,j,tile)) / &
                                       float(total_tile)
           orog_tiles_elev(i,j,num_orog_tiles(i,j))= sum_tile(i,j,tile) / &
                                       float(count_tile(i,j,tile))
         endif
       enddo
     else
       num_orog_tiles(i,j) = 1
       orog_tiles_prcnt(i,j,1) =  1.0
       orog_tiles_elev(i,j,1) = topo(i,j)
     endif
   enddo
 enddo

 deallocate(num_tiles, topo)

 return

 end subroutine calc_orog

!-------------------------------------------------------------------------
! calculate land/sea mask using a simple bilinear interpolation.
!-------------------------------------------------------------------------

 subroutine calc_lsmask_bilinear

 use calc_latlons, only     : lat_mdl, lon_mdl
 
 use mpimod, only           : istart_mdl, iend_mdl, iend_mdl_4_loops, &
                              jstart_mdl, jend_mdl

 use program_setup, only    : lsmask_file, lsmask_tiles, lsmask_tile_threshold

 implicit none

 include 'mpif.h'

 integer, parameter      :: isrc=43200
 integer, parameter      :: jsrc=21600
 integer                 :: i, j, ii, ip1, jj, jp1, ierr, iunit
 integer                 :: iend_src, istart_src
 integer                 :: jend_src, jstart_src, jsrc_task
 integer*1, allocatable  :: mask_src(:,:)
 integer*1, parameter    :: mask_water_flag=0  ! use 16 for usgs landuse data
 integer*8               :: offset
 
 real                    :: dlat_src, dlon_src
 real                    :: lat_11_src, lon_11_src
 real                    :: sum, w11, w12, w21, w22
 real                    :: xx, yy, xf, yf

 print*,"- CALCULATE LAND/SEA MASK USING BILINEAR METHOD"

!----------------------------------------------------------------------
! determine bounds of model grid within the source data.
! assumes that source data are global lat/lon projections.
!----------------------------------------------------------------------

 dlon_src   = 1.0 / 120.0
 dlat_src   = -(1.0 / 120.0)
 lon_11_src = -180.0 + (dlon_src*0.5)  ! lat point 1,1
 lat_11_src = 90.0 + (dlat_src*0.5)    ! lon point 1,1

 call find_bounds_ll(isrc, jsrc, lat_11_src, lon_11_src, dlat_src, dlon_src, &
                     istart_src, iend_src, jstart_src, jend_src)
 
 jsrc_task = jend_src-jstart_src + 1

!----------------------------------------------------------------------
! open and read landuse data
!----------------------------------------------------------------------

 allocate (mask_src(isrc,jstart_src:jend_src))
 iunit=10
 print*,'- OPEN AND READ SOURCE FILE FOR MASK: ',trim(lsmask_file)
 call mpi_file_open(mpi_comm_world, lsmask_file, mpi_mode_rdonly, &
                    mpi_info_null, iunit, ierr)
 if (ierr /= 0) then
   print*,'- BAD OPEN, IERR IS: ', ierr
   call mpi_abort(mpi_comm_world, 1, ierr)
 endif
 offset=int(isrc,8)*(int(jstart_src,8)-1_8)
 call mpi_file_read_at(iunit, offset, mask_src, isrc*jsrc_task, &
                       mpi_integer1, mpi_status_ignore, ierr)
 if (ierr /= 0) then
   print*,'- BAD READ, IERR IS: ', ierr
   call mpi_abort(mpi_comm_world, 1, ierr)
 endif
 call mpi_file_close(iunit, ierr)

 do j = jstart_mdl, jend_mdl
   do i = istart_mdl, iend_mdl_4_loops(j)

     xx = (lon_mdl(i,j)-lon_11_src) / dlon_src + 1.0
     yy = (lat_mdl(i,j)-lat_11_src) / dlat_src + 1.0

     xf = xx - int(xx)
     yf = yy - int(yy)
     ii = int(xx)
     if (ii<1) ii=isrc+ii
     if (ii>isrc) ii=ii-isrc
     ip1=ii+1
     if (ip1>isrc) ip1=ip1-isrc
     jj = int(yy)
     if (jj==0) then
       jj=1
       jp1=1
     elseif (jj==jsrc) then
       jp1=jsrc
     else
       jp1=jj+1
     endif

     w11 = (1.-xf)*(1.-yf)
     w21 = xf*(1.-yf)
     w12 = (1.-xf)*yf
     w22 = xf*yf

     sum = 0.0
     if (mask_src(ii,jj) /= mask_water_flag) sum = sum + w11
     if (mask_src(ip1,jj) /= mask_water_flag) sum = sum + w21
     if (mask_src(ii,jp1) /= mask_water_flag) sum = sum + w12
     if (mask_src(ip1,jp1) /= mask_water_flag) sum = sum + w22

     if (.not. lsmask_tiles) then
       lsmask(i,j) = nint(sum)
     else
       lsmask(i,j) = sum
       if (sum >= (1.0-lsmask_tile_threshold)) then
         lsmask(i,j) = 1.0
       elseif (sum <= lsmask_tile_threshold) then
         lsmask(i,j) = 0.0
       end if
     end if

   enddo
 enddo

 deallocate (mask_src)

 return

 end subroutine calc_lsmask_bilinear

 subroutine calc_lsmask_aavg

 use calc_latlons, only     : lat_mdl, lon_mdl, lat_first_mdl, lon_first_mdl

 use init_grib1, only       : kgds_mdl

 use ll2xy_utils, only      : ll2xy_egrid_pt, ll2xy_bgrid_pt, ll2xy_polar

 use mpimod, only           : istart_mdl, iend_mdl, iend_mdl_4_loops, &
                              jstart_mdl, jend_mdl, gather, myrank

 use program_setup, only    : dx_mdl, dx_gfs, dy_mdl, hemi_mdl, orient_lon_mdl, &
                              lat_11_mdl, lon_11_mdl, centlat_mdl, centlon_mdl, &
                              centlat_parent_mdl, centlon_parent_mdl, lonsperlat_mdl, lsmask_file, &
                              imdl, jmdl, domain_type, &
                              lsmask_tiles, lsmask_tile_threshold

 implicit none

 include 'mpif.h'

 integer, allocatable    :: count(:,:)
 integer, parameter      :: isrc=43200
 integer, parameter      :: jsrc=21600
 integer                 :: iend_src, istart_src
 integer                 :: jend_src, jstart_src, jsrc_task
 integer                 :: i, iii, j, jj, nret
 integer                 :: ierr, iunit
 integer*1, allocatable  :: mask_src(:,:)
 integer                 :: nearest_i, nearest_j
 integer*8               :: offset
 integer*1, parameter    :: mask_water_flag=0  ! use 16 for usgs landuse data

 real                    :: dlat_src, dlon_src
 real                    :: lat_11_src, lon_11_src
 real, allocatable       :: lats_src(:),lons_src(:)
 real, allocatable       :: dum(:), ypts(:)
 real, allocatable       :: sum_mask(:,:)
 real                    :: lat_src, lon_src, xgrid, ygrid

!----------------------------------------------------------------------
! determine bounds of model grid within the source data.
! assumes that source data are global lat/lon projections.
!----------------------------------------------------------------------

 dlon_src   = 1.0 / 120.0
 dlat_src   = -(1.0 / 120.0)
 lon_11_src = -180.0 + (dlon_src*0.5)  ! lat point 1,1
 lat_11_src = 90.0 + (dlat_src*0.5)    ! lon point 1,1

 call find_bounds_ll(isrc, jsrc, lat_11_src, lon_11_src, dlat_src, dlon_src, &
                     istart_src, iend_src, jstart_src, jend_src)
 
 jsrc_task = jend_src-jstart_src + 1

!----------------------------------------------------------------------
! open and read landuse and terrain data
!----------------------------------------------------------------------

 allocate (mask_src(isrc,jstart_src:jend_src))
 iunit=10
 print*,'- OPEN AND READ SOURCE FILE FOR MASK: ',trim(lsmask_file)
 call mpi_file_open(mpi_comm_world, lsmask_file, mpi_mode_rdonly, &
                    mpi_info_null, iunit, ierr)
 if (ierr /= 0) then
   print*,'- BAD OPEN, IERR IS: ', ierr
   call mpi_abort(mpi_comm_world, 1, ierr)
 endif
 offset=int(isrc,8)*(int(jstart_src,8)-1_8)
 call mpi_file_read_at(iunit, offset, mask_src, isrc*jsrc_task, &
                       mpi_integer1, mpi_status_ignore, ierr)
 if (ierr /= 0) then
   print*,'- BAD READ, IERR IS: ', ierr
   call mpi_abort(mpi_comm_world, 1, ierr)
 endif
 call mpi_file_close(iunit, ierr)

 allocate(sum_mask(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))
 sum_mask=0.
 allocate(count(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))
 count=0

 if (trim(domain_type) == "gaussian") then 
   allocate (lats_src(jstart_src:jend_src))
   allocate (lons_src(jstart_src:jend_src))
   lons_src = 0.0
   do j = jstart_src, jend_src
     lats_src(j) = lat_11_src + (j-1)*dlat_src
   enddo
   allocate (ypts(jstart_src:jend_src))
   allocate (dum(jstart_src:jend_src))
   call gdswiz04(kgds_mdl,-1,jsrc_task,-999.9,dum,ypts,lons_src,lats_src, &
               nret, 0, dum, dum)
   deallocate (dum, lons_src, lats_src)
   do j = jstart_src,jend_src
     nearest_j = nint(ypts(j))
     if (nearest_j < jstart_mdl .or. nearest_j > jend_mdl) cycle
     jj = nearest_j
     if (nearest_j > jmdl/2) jj = jmdl - nearest_j + 1
     do i = istart_src, iend_src
       lon_src = lon_11_src + (i-1)*dlon_src
       nearest_i = nint(lon_src / dx_gfs(nearest_j) + 1.0)   
       if (nearest_i > lonsperlat_mdl(jj)) then
         nearest_i = nearest_i - lonsperlat_mdl(jj)
       else if (nearest_i < 1) then
         nearest_i = nearest_i + lonsperlat_mdl(jj)
       end if
       if (mask_src(i,j) /= mask_water_flag) then
         sum_mask(nearest_i,nearest_j) = sum_mask(nearest_i,nearest_j)+ 1.0
       end if
       count(nearest_i,nearest_j) = count(nearest_i,nearest_j)+1
     enddo
   enddo
   deallocate (ypts)
 elseif (trim(domain_type) == "latlon") then  ! regular lat/lon
   do j = jstart_src, jend_src
     lat_src = lat_11_src + (j-1)*dlat_src
     nearest_j = nint((lat_src - lat_11_mdl) / dy_mdl + 1.0)
     if (nearest_j < jstart_mdl .or. nearest_j > jend_mdl) cycle
     do i = istart_src, iend_src
       lon_src = lon_11_src + (i-1)*dlon_src
       nearest_i = nint(1.0 + mod((lon_src-lon_11_mdl)+3600,360.0)/dx_mdl)
       if (nearest_i < 1 .or. nearest_i > imdl) cycle  ! allow for regional grids
       if (mask_src(i,j) /= mask_water_flag) then
         sum_mask(nearest_i,nearest_j) = sum_mask(nearest_i,nearest_j)+ 1.0
       end if
       count(nearest_i,nearest_j) = count(nearest_i,nearest_j)+1
     enddo
   enddo
 elseif (trim(domain_type) == "polar") then  ! polar stereographic
   do j = jstart_src,jend_src
     lat_src = lat_11_src + (j-1)*dlat_src
   do i = istart_src, iend_src
     iii = i
     if (i < 1) iii = i + isrc
     if (i > isrc) iii = i - isrc
     lon_src = lon_11_src + (iii-1)*dlon_src
     call ll2xy_polar(lat_11_mdl, lon_11_mdl, orient_lon_mdl, &
                      dx_mdl, dy_mdl, hemi_mdl, lat_src, lon_src, &
                      xgrid, ygrid) 
     nearest_i = nint(xgrid)
     nearest_j = nint(ygrid)
     if (nearest_i >= istart_mdl .and. nearest_i <= iend_mdl .and. &
         nearest_j >= jstart_mdl .and. nearest_j <= jend_mdl) then
       if (mask_src(iii,j) /= mask_water_flag) then
         sum_mask(nearest_i,nearest_j) = sum_mask(nearest_i,nearest_j)+ 1.0
       end if
       count(nearest_i,nearest_j) = count(nearest_i,nearest_j)+1
     endif
   enddo
   enddo
 elseif (trim(domain_type) == "egrid") then  ! e-grid
   do j = jstart_src,jend_src
     lat_src = lat_11_src + (j-1)*dlat_src
   do i = istart_src, iend_src
     iii = i
     if (i < 1) iii = i + isrc
     if (i > isrc) iii = i - isrc
     lon_src = lon_11_src + (iii-1)*dlon_src
     call ll2xy_egrid_pt(lat_src, lon_src, imdl, jmdl, &
                         centlat_mdl, centlon_mdl,   &
                        -(dx_mdl), dy_mdl, &
                         nearest_i, nearest_j)
     if (nearest_i >= istart_mdl .and. nearest_i <= iend_mdl .and. &
         nearest_j >= jstart_mdl .and. nearest_j <= jend_mdl) then
       if (mask_src(iii,j) /= mask_water_flag) then
         sum_mask(nearest_i,nearest_j) = sum_mask(nearest_i,nearest_j)+ 1.0
       end if
       count(nearest_i,nearest_j) = count(nearest_i,nearest_j)+1
     endif
   enddo
   enddo
 elseif (trim(domain_type) == "bgrid") then  ! b-grid
   do j = jstart_src,jend_src
     lat_src = lat_11_src + (j-1)*dlat_src
   do i = istart_src, iend_src
     iii = i
     if (i < 1) iii = i + isrc
     if (i > isrc) iii = i - isrc
     lon_src = lon_11_src + (iii-1)*dlon_src
     call ll2xy_bgrid_pt_loc(centlat_parent_mdl(1), centlon_parent_mdl(1), dy_mdl, dx_mdl, &
                         lat_first_mdl, lon_first_mdl, imdl, jmdl, lat_src, lon_src, nearest_i, nearest_j)
     if (nearest_i >= istart_mdl .and. nearest_i <= iend_mdl .and. &
         nearest_j >= jstart_mdl .and. nearest_j <= jend_mdl) then
       if (mask_src(iii,j) /= mask_water_flag) then
         sum_mask(nearest_i,nearest_j) = sum_mask(nearest_i,nearest_j)+ 1.0
       end if
       count(nearest_i,nearest_j) = count(nearest_i,nearest_j)+1
     endif
   enddo
   enddo
 else ! undefined
   print*,'- UNDEFINED MAP PROJECTION. STOP.'
   call mpi_abort(mpi_comm_world, 1, ierr)
 endif
 deallocate(mask_src)

 do j = jstart_mdl, jend_mdl
  do i = istart_mdl, iend_mdl_4_loops(j)
    if (count(i,j) == 0) then
      print*,'- NO SOURCE DATA WITHIN MODEL GRID AT I/J: ', i,j
      call mpi_abort(mpi_comm_world, 1, ierr)
    endif
    lsmask(i,j)=sum_mask(i,j)/float(count(i,j))
    if (.not. lsmask_tiles) then
      lsmask(i,j) = nint(lsmask(i,j))
    else
      if (lsmask(i,j) >= (1.0-lsmask_tile_threshold)) then
        lsmask(i,j) = 1.0
      elseif (lsmask(i,j) <= lsmask_tile_threshold) then
        lsmask(i,j) = 0.0
      end if
    endif
  enddo
 enddo

 return

 end subroutine calc_lsmask_aavg

!-----------------------------------------------------------------------
! free up memory
!-----------------------------------------------------------------------

 subroutine lsmask_orog_cleanup

 implicit none

 if (allocated(lsmask))           deallocate(lsmask)
 if (allocated(wtrmask))          deallocate(wtrmask)
 if (allocated(lbms_wtr_mdl))     deallocate(lbms_wtr_mdl)
 if (allocated(lbms_lnd_mdl))     deallocate(lbms_lnd_mdl)
 if (allocated(orog_tiles_elev))  deallocate(orog_tiles_elev)
 if (allocated(orog_tiles_prcnt)) deallocate(orog_tiles_prcnt)
 if (allocated(num_orog_tiles))   deallocate(num_orog_tiles)
 if (allocated(orog_stnd_dev))    deallocate(orog_stnd_dev)

 return 

 end subroutine lsmask_orog_cleanup

!-----------------------------------------------------------------------
! ensure mask and terrain are consistent at water points by
! 'flattening' the terrain.
!-----------------------------------------------------------------------

 subroutine waterfalls(lsmask, orog)

 use program_setup, only      : imdl, jmdl

 implicit none

 integer                     :: i, j, k

 real, intent(in)            :: lsmask(imdl,jmdl)
 real, intent(inout)         :: orog(imdl,jmdl)

 logical                     :: done

 do k = 1, 500
 done=.true.
 do j = 3, jmdl-2    ! avoid inner boundary.  this will disturb the
   do i = 3, imdl-2  ! special smoothing done there.
      if (lsmask(i,j) == 0.0) then   
        if(lsmask(i,j-1) == 0.0) then
           if (orog(i,j) > orog(i,j-1)) done=.false.
           orog(i,j) =  min(orog(i,j),orog(i,j-1))
        endif
        if(lsmask(i,j+1) == 0.0) then
           if (orog(i,j) > orog(i,j+1)) done=.false.
           orog(i,j) =  min(orog(i,j),orog(i,j+1))
        endif
        if(lsmask(i+1,j-1) == 0.0) then
           if (orog(i,j) > orog(i+1,j-1)) done=.false.
           orog(i,j) = min(orog(i,j),orog(i+1,j-1))
        endif
        if(lsmask(i+1,j) == 0.0) then
           if (orog(i,j) > orog(i+1,j)) done=.false.
           orog(i,j) =  min(orog(i,j),orog(i+1,j))
        endif
        if(lsmask(i+1,j+1) == 0.0) then
           if (orog(i,j) > orog(i+1,j+1)) done=.false.
           orog(i,j) = min(orog(i,j),orog(i+1,j+1))
        endif
        if(lsmask(i-1,j+1) == 0.0)then
           if (orog(i,j) > orog(i-1,j+1)) done=.false.
           orog(i,j) = min(orog(i,j),orog(i-1,j+1))
        endif
        if(lsmask(i-1,j) == 0.0) then
           if (orog(i,j) > orog(i-1,j)) done=.false.
           orog(i,j) =  min(orog(i,j),orog(i-1,j))
        endif
        if(lsmask(i-1,j-1) == 0.0) then
           if (orog(i,j) > orog(i-1,j-1)) done=.false.
           orog(i,j) = min(orog(i,j),orog(i-1,j-1))
        endif
      end if
   enddo
 enddo
 if (done) exit
 enddo

 print*,'- WATERFALLS REMOVED AFTER ', k, ' PASSES.'

 return

 end subroutine waterfalls

!-----------------------------------------------------------------------
!  remove waterfalls for staggered e-grid
!-----------------------------------------------------------------------

 subroutine waterfalls_egrid(lsmask, orog)

 use program_setup, only   : imdl, jmdl

 implicit none

 integer                  :: i, j, k

 real, intent(in)         :: lsmask(imdl,jmdl)
 real, intent(inout)      :: orog(imdl,jmdl)

 do k = 1, 5
 do j = 3, jmdl-2    ! don't apply at inner two boundary pts as this will
   do i = 3, imdl-2  ! ruin the smoothing that was already done.
     if (mod(j,2) == 0) then                ! even row
       if (lsmask(i,j) == 0.0) then   
         if(lsmask(i,j-1) == 0.0) orog(i,j) = min(orog(i,j),orog(i,j-1))
         if(lsmask(i,j+1) == 0.0) orog(i,j) = min(orog(i,j),orog(i,j+1))
         if(lsmask(i+1,j-1) == 0.0) orog(i,j) = min(orog(i,j),orog(i+1,j-1))
         if(lsmask(i+1,j+1) == 0.0) orog(i,j) = min(orog(i,j),orog(i+1,j+1))
       end if
     else                                   ! odd row
       if (lsmask(i,j) == 0.0) then
         if(lsmask(i-1,j-1) == 0.0) orog(i,j) = min(orog(i,j),orog(i-1,j-1))
         if(lsmask(i-1,j+1) == 0.0) orog(i,j) = min(orog(i,j),orog(i-1,j+1))
         if(lsmask(i,j-1) == 0.0) orog(i,j) = min(orog(i,j),orog(i,j-1))
         if(lsmask(i,j+1) == 0.0) orog(i,j) = min(orog(i,j),orog(i,j+1))
       end if
     end if
   enddo
 enddo
 enddo

 return

 end subroutine waterfalls_egrid

!-----------------------------------------------------------------------
! remove isolated lakes for e-stagger
!-----------------------------------------------------------------------
 
 subroutine remove_lakes_egrid(lsmask)

 use program_setup, only : imdl, jmdl

 implicit none

 integer                :: i, j

 real, intent(inout)    :: lsmask(imdl,jmdl)

 do j = 2, jmdl-1
   do i = 2, imdl-1
     if (mod(j,2) == 0) then                ! even row
       if (lsmask(i,j) == 0.0 .and.    &
           lsmask(i,j-1) == 1.0 .and.  &
           lsmask(i,j+1) == 1.0 .and.  &
           lsmask(i+1,j-1) == 1.0 .and. &
           lsmask(i+1,j+1) == 1.0 ) then
           print*,'- REMOVE ISOLATED LAKE AT ',i,j
           lsmask(i,j) = 1.0
       end if
     else                                   ! odd row
       if (lsmask(i,j) == 0.0 .and.    &
           lsmask(i-1,j-1) == 1.0 .and.  &
           lsmask(i-1,j+1) == 1.0 .and.  &
           lsmask(i,j-1) == 1.0 .and. &
           lsmask(i,j+1) == 1.0 ) then
           print*,'- REMOVE ISOLATED LAKE AT ',i,j
           lsmask(i,j) = 1.0
       end if
     end if
   enddo
 enddo

 end subroutine remove_lakes_egrid

!-----------------------------------------------------------------------
! remove isolated lakes and lakes that are one grid point 
! wide for b-stagger.
!-----------------------------------------------------------------------
 
 subroutine remove_lakes_bgrid(lsmask)

 use program_setup, only : imdl, jmdl

 implicit none

 integer                :: count, i, j

 real, intent(inout)    :: lsmask(imdl,jmdl)

 do j = 2, jmdl-1
   do i = 2, imdl-1
     count=0
     if (lsmask(i,j) == 0.0) then
       if(lsmask(i-1,j-1) == 1.0) count=count+1
       if(lsmask(i,j-1)   == 1.0) count=count+1
       if(lsmask(i+1,j-1) == 1.0) count=count+1
       if(lsmask(i-1,j+1) == 1.0) count=count+1
       if(lsmask(i,j+1)   == 1.0) count=count+1
       if(lsmask(i+1,j+1) == 1.0) count=count+1
       if(lsmask(i-1,j)   == 1.0) count=count+1
       if(lsmask(i+1,j)   == 1.0) count=count+1
     endif
     if (count >=7) then
       print*,'- REMOVE ISOLATED LAKE AT ',i,j
       lsmask(i,j) = 1.0
     end if
   enddo
 enddo

 end subroutine remove_lakes_bgrid

!-----------------------------------------------------------------------
! at each water point, check the elevation of the surrounding land
! points. if a land point is lower than the water point, raise 
! its elevation to that of the water point.
!-----------------------------------------------------------------------

 subroutine coastlines_egrid(lsmask, orog)

 use program_setup, only    : imdl, jmdl

 implicit none

 integer                   :: i, j

 real                      :: test
 real, intent(in)          :: lsmask(imdl,jmdl)
 real, intent(inout)       :: orog(imdl,jmdl)

 do j = 4, jmdl-3    ! don't apply this at the inner two boundary pts as that
   do i = 4, imdl-3  ! will ruin the smoothing that was already done.
     if (mod(j,2) == 0) then                ! even row
       if (lsmask(i,j) == 0.0) then
         if (lsmask(i,j-1) == 1.0) then
           test = orog(i,j) - orog(i,j-1)
           if (test >= -1.0) then
             print*,'- ELEVATED WATER AT: ',i,j
             orog(i,j-1) = orog(i,j)+1.0
           endif
         endif
         if (lsmask(i,j+1) == 1.0) then
           test = orog(i,j) - orog(i,j+1)
           if (test >= -1.0) then
             print*,'- ELEVATED WATER AT: ',i,j
             orog(i,j+1) = orog(i,j)+1.0
           endif
         end if
         if (lsmask(i+1,j-1) == 1.0) then
           test = orog(i,j) - orog(i+1,j-1)
           if (test >= -1.0) then
             print*,'- ELEVATED WATER AT: ',i,j
             orog(i+1,j-1) = orog(i,j)+1.0
           endif
         endif
         if (lsmask(i+1,j+1) == 1.0 ) then
           test = orog(i,j) - orog(i+1,j+1)
           if (test >= -1.0) then
             print*,'- ELEVATED WATER AT: ',i,j
             orog(i+1,j+1) = orog(i,j)+1.0
           endif
         endif
       end if
     else                                   ! odd row
       if (lsmask(i,j) == 0.0) then 
         if (lsmask(i-1,j-1) == 1.0) then
           test = orog(i,j) - orog(i-1,j-1)
           if (test >= -1.0) then
             print*,'- ELEVATED WATER AT: ',i,j
             orog(i-1,j-1) = orog(i,j)+1.0
           endif
         endif
         if (lsmask(i-1,j+1) == 1.0) then
           test = orog(i,j) - orog(i-1,j+1)
           if (test >= -1.0) then
             print*,'- ELEVATED WATER AT: ',i,j
             orog(i-1,j+1) = orog(i,j)+1.0
           endif
         endif
         if (lsmask(i,j-1) == 1.0) then
           test = orog(i,j) - orog(i,j-1)
           if (test >= -1.0) then
             print*,'- ELEVATED WATER AT: ',i,j
             orog(i,j-1) = orog(i,j)+1.0
           endif
         end if
         if (lsmask(i,j+1) == 1.0) then
           test = orog(i,j) - orog(i,j+1)
           if (test >= -1.0) then
             print*,'- ELEVATED WATER AT: ',i,j
             orog(i,j+1) = orog(i,j)+1.0
           endif
         endif
       end if
     end if
   enddo
 enddo

 end subroutine coastlines_egrid

!-----------------------------------------------------------------------
! at each water point, check the elevation of the surrounding land
! points. if a land point is lower than the water point, raise 
! its elevation to that of the water point.
!-----------------------------------------------------------------------

 subroutine coastlines(lsmask, orog)

 use program_setup, only    : imdl, jmdl

 implicit none

 integer                   :: i, j

 real                      :: test
 real, intent(in)          :: lsmask(imdl,jmdl)
 real, intent(inout)       :: orog(imdl,jmdl)

 do j = 4, jmdl-3    ! avoid inner boundary which has its own
   do i = 4, imdl-3  ! special smoothing.
     if (lsmask(i,j) == 0.0) then
       if (lsmask(i,j-1) == 1.0) then
         test = orog(i,j) - orog(i,j-1)
         if (test >= -1.0) then
           print*,'- ELEVATED WATER AT: ',i,j
           orog(i,j-1) = orog(i,j)+1.0
         endif
       endif
       if (lsmask(i,j+1) == 1.0) then
         test = orog(i,j) - orog(i,j+1)
         if (test >= -1.0) then
           print*,'- ELEVATED WATER AT: ',i,j
           orog(i,j+1) = orog(i,j)+1.0
         endif
       end if
       if (lsmask(i+1,j-1) == 1.0) then
         test = orog(i,j) - orog(i+1,j-1)
         if (test >= -1.0) then
           print*,'- ELEVATED WATER AT: ',i,j
           orog(i+1,j-1) = orog(i,j)+1.0
         endif
       endif
       if (lsmask(i+1,j+1) == 1.0 ) then
         test = orog(i,j) - orog(i+1,j+1)
         if (test >= -1.0) then
           print*,'- ELEVATED WATER AT: ',i,j
           orog(i+1,j+1) = orog(i,j)+1.0
         endif
       endif
       if (lsmask(i-1,j-1) == 1.0) then
         test = orog(i,j) - orog(i-1,j-1)
         if (test >= -1.0) then
           print*,'- ELEVATED WATER AT: ',i,j
           orog(i-1,j-1) = orog(i,j)+1.0
         endif
       endif
       if (lsmask(i-1,j+1) == 1.0) then
         test = orog(i,j) - orog(i-1,j+1)
         if (test >= -1.0) then
           print*,'- ELEVATED WATER AT: ',i,j
           orog(i-1,j+1) = orog(i,j)+1.0
         endif
       endif
       if (lsmask(i-1,j) == 1.0) then
         test = orog(i,j) - orog(i-1,j)
         if (test >= -1.0) then
           print*,'- ELEVATED WATER AT: ',i,j
           orog(i-1,j) = orog(i,j)+1.0
         endif
       end if
       if (lsmask(i+1,j) == 1.0) then
         test = orog(i,j) - orog(i+1,j)
         if (test >= -1.0) then
           print*,'- ELEVATED WATER AT: ',i,j
           orog(i+1,j) = orog(i,j)+1.0
         endif
       endif
     endif
   enddo
 enddo
 
 end subroutine coastlines

!-----------------------------------------------------------------------
! remove isolated lakes for gaussian grid.  can work with
! full or reduced grids.  taken from jordan's code.
!-----------------------------------------------------------------------
 
 subroutine remove_lakes_gaussian(lsmask)

 use program_setup, only      : imdl, jmdl, lonsperlat_mdl

 implicit none

 integer                :: i, in, is, iw, ie, inw, ine, isw, ise, &
                           j, jj, jn, js

 real, intent(inout)    :: lsmask(imdl,jmdl)
 real                   :: rn, rs, xn, xs

 JLOOP : do j = 2, jmdl-1
   jj = j
   if (jj > jmdl/2) jj = jmdl - j + 1
   jn = j-1
   if (jn > jmdl/2) jn = jmdl - (j-1) + 1
   js = j+1
   if (js > jmdl/2) js = jmdl - (j+1) + 1
   rn = float(lonsperlat_mdl(jn))/float(lonsperlat_mdl(jj))
   rs = float(lonsperlat_mdl(js))/float(lonsperlat_mdl(jj))
   ILOOP : do i = 1, lonsperlat_mdl(jj)
     if (lsmask(i,j) == 0.0) then
       iw=mod(i+lonsperlat_mdl(jj)-2,lonsperlat_mdl(jj))+1
       if (lsmask(iw,j) == 0.0) cycle ILOOP
       ie=mod(i,lonsperlat_mdl(jj)) + 1
       if (lsmask(ie,j) == 0.0) cycle ILOOP
       xn = rn*(i-1)+1
       if (abs(xn-nint(xn)) < 1.e-2) then
         in=mod(nint(xn)-1,lonsperlat_mdl(jn))+1
         if (lsmask(in,j-1) == 0.0) cycle ILOOP
         inw=mod(in+lonsperlat_mdl(jn)-2,lonsperlat_mdl(jn))+1
         if (lsmask(inw,j-1) == 0.0) cycle ILOOP
         ine=mod(in,lonsperlat_mdl(jn))+1
         if (lsmask(ine,j-1) == 0.0) cycle ILOOP
       else
         inw = int(xn)
         if (lsmask(inw,j-1) == 0.0) cycle ILOOP
         ine = mod(inw,lonsperlat_mdl(jn))+1
         if (lsmask(ine,j-1) == 0.0) cycle ILOOP
       endif
       xs = rs*(i-1)+1
       if (abs(xs-nint(xs)) < 1.e-2) then
         is=mod(nint(xs)-1,lonsperlat_mdl(js))+1
         if (lsmask(is,j+1) == 0.0) cycle ILOOP
         isw=mod(is+lonsperlat_mdl(js)-2,lonsperlat_mdl(js))+1
         if (lsmask(isw,j+1) == 0.0) cycle ILOOP
         ise=mod(is,lonsperlat_mdl(js))+1
         if (lsmask(ise,j+1) == 0.0) cycle ILOOP
       else
         isw = int(xs)
         if (lsmask(isw,j+1) == 0.0) cycle ILOOP
         ise = mod(isw,lonsperlat_mdl(js))+1
         if (lsmask(ise,j+1) == 0.0) cycle ILOOP
       endif
       print*,'- REMOVE ISOLATED LAKE AT ',i,j
       lsmask(i,j) = 1.0
     end if
   enddo ILOOP
 enddo JLOOP

 end subroutine remove_lakes_gaussian

 subroutine ll2xy_bgrid_pt_loc(tph0d, tlm0d, dphd, dlmd, lat11, lon11, im, jm, lat, lon, ii, jj)

 implicit none

! this routine works on a single point

 integer, intent(in) :: im, jm
 integer, intent(out) :: ii, jj

 logical, save       :: first

 real, intent(in)    :: tph0d, tlm0d, lat, lon, dphd, dlmd, lon11, lat11
 real                :: x, y

 real  , parameter   :: one = 1.0
 real  , parameter   :: two = 2.0
 real  , parameter   :: r180 = 180.0

 real  , save  :: pi, dtr, rtd, dph, dlm, tph0, tlm0, stph0, ctph0, wbd, sbd
 real    :: latr, lonr, relmi, srlmi, crlmi, lat11r, lon11r
 real    :: sph, cph, cc, anum, denom, tlon, tlat

 data first /.true./

 if (first) then
   pi = acos(-one)
   dtr = pi / r180
   rtd = r180 / pi
   dph=dphd*dtr
   dlm=dlmd*dtr
   tph0=tph0d*dtr
   tlm0=tlm0d*dtr
   stph0=sin(tph0)
   ctph0=cos(tph0)
   lat11r = lat11 * dtr
   lon11r = lon11 * dtr
   relmi=tlm0-lon11r
   srlmi=sin(relmi)
   crlmi=cos(relmi)
   sPH=SIN(lat11r)
   CPH=COS(lat11r)
   CC=CPH*CRLMI
   ANUM=CPH*SRLMI
   DENOM=CTPH0*CC+STPH0*SPH
   wbd =-ATAN2(ANUM,DENOM)
   sbd =ASIN(CTPH0*SPH-STPH0*CC)
!   first = .false.
 endif

 latr = lat * dtr
 lonr = lon * dtr
 relmi=tlm0-lonr
 srlmi=sin(relmi)
 crlmi=cos(relmi)
 sPH=SIN(latr)
 CPH=COS(latr)
 CC=CPH*CRLMI
 ANUM=CPH*SRLMI
 DENOM=CTPH0*CC+STPH0*SPH
 TLON=-ATAN2(ANUM,DENOM)
 TLAT=ASIN(CTPH0*SPH-STPH0*CC)
 y =  ((tlat - sbd) / dph) + 1.0
 x =  ((tlon - wbd) / dlm) + 1.0
 ii = nint(x)
 jj = nint(y)
 return

 end subroutine ll2xy_bgrid_pt_loc



 end module lsmask_orog
