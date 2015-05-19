 subroutine interp_tiles(lon_11_src, lat_11_src, &
                         srcdat, isrc, &
                         istart_src, iend_src, &
                         jstart_src, jend_src, &
                         dlon_src, dlat_src, & 
                         num_categories, num_groups, &
                         water_category, default_category, &
                         tile_threshold, cat_groups, &
                         out_cat, out_group, dominate_cat)

 use ll2xy_utils, only       : ll2xy_egrid_pt, ll2xy_bgrid_pt

 use program_setup,  only    : imdl,         &
                               jmdl,         &
                               dx_mdl,       &
                               dx_gfs,       &
                               dy_mdl,       &
                               centlat_mdl, centlat_parent_mdl,  &
                               centlon_mdl, centlon_parent_mdl,  &
                               tangent_lat_mdl, &
                               orient_lon_mdl, & 
                               domain_type,  &
                               lonsperlat_mdl  

 use calc_latlons, only      : lat_mdl, lon_mdl, lat_first_mdl, lon_first_mdl

 use lsmask_orog, only       : lsmask, ll2xy_bgrid_pt_loc

 use mpimod, only            : istart_mdl, iend_mdl, &
                               jstart_mdl, jend_mdl, iend_mdl_4_loops

 use init_grib1, only        : kgds_mdl
             
 implicit none

 include 'mpif.h'

 integer                    :: cat
 integer                    :: category
 integer, intent(in)        :: cat_groups(num_categories)
 integer                    :: count_cat(istart_mdl:iend_mdl,jstart_mdl:jend_mdl,num_categories) ! for each land category
 integer                    :: count_group(istart_mdl:iend_mdl,jstart_mdl:jend_mdl,num_groups)
 integer, intent(in)        :: default_category
 integer, parameter         :: default_group = 1
 integer, intent(out)       :: dominate_cat(istart_mdl:iend_mdl,jstart_mdl:jend_mdl)
 integer                    :: group, grp
 integer                    :: i, j, ierr, nret, ii, jj, iii, jjj, istart, iend
 integer, intent(in)        :: istart_src, iend_src
 integer                    :: jstart, jend, spiral_rad, krad, igrid, jgrid
 integer, intent(in)        :: isrc ! i-dimension of data source grid
 integer                    :: jsrc
 integer, intent(in)        :: jstart_src, jend_src
 integer                    :: nearest_i,nearest_j
 integer*4, intent(in)      :: num_categories
 integer, intent(in)        :: num_groups 
 integer                    :: pred_group(1)
 integer*1, intent(in)      :: srcdat(isrc,jstart_src:jend_src)
 integer                    :: total_count, total_count_sav
 integer*4, intent(in)      :: water_category

 real*8, intent(in)         :: dlon_src, dlat_src  ! lat/lon increment of the source grid
 real*8, intent(in)         :: lon_11_src, lat_11_src ! lon/lat of point (1,1) of
                                                      ! data source grid
 real*4                     :: maxcat
 real*4, intent(out)        :: out_cat(istart_mdl:iend_mdl,jstart_mdl:jend_mdl,num_categories) 
 real*4, intent(out)        :: out_group(istart_mdl:iend_mdl,jstart_mdl:jend_mdl,num_groups)   
 real                       :: percent_cat
 real, intent(in)           :: tile_threshold
 real, allocatable          :: lats_src(:), lons_src(:)
 real, allocatable          :: dum(:), ypts(:)
 real                       :: dx_meters, srclat, srclon, xgrid, ygrid

!------------------------------------------------------------------
! loop over high-res source grid.  find the nearest neighbor
! model point.  then store a count of the each cat and group
! at each model point.
!------------------------------------------------------------------

 out_cat   = 0.0
 out_group = 0.0
 count_cat   = 0
 count_group = 0

 if (trim(domain_type) == 'egrid') then
   do j = jstart_src, jend_src
   do i = istart_src, iend_src
      iii = i
      if (iii < 1) iii = isrc + iii
      if (iii > isrc) iii = iii - isrc
      category = srcdat(iii,j)
      if (category /= water_category) then
        srclat = lat_11_src + (j-1)*dlat_src
        srclon = lon_11_src + (iii-1)*dlon_src
        call ll2xy_egrid_pt(srclat, srclon, imdl, jmdl, &
                            centlat_mdl, centlon_mdl,   &
                           -(dx_mdl), dy_mdl, nearest_i, nearest_j)
        if (nearest_i >= istart_mdl .and. nearest_i <= iend_mdl .and. &
            nearest_j >= jstart_mdl .and. nearest_j <= jend_mdl) then
          count_cat(nearest_i,nearest_j,category) = count_cat(nearest_i,nearest_j,category) + 1
          group                   = cat_groups(category)
          count_group(nearest_i,nearest_j,group)  = count_group(nearest_i,nearest_j,group) + 1
        end if
      end if
   enddo
   enddo
 elseif (trim(domain_type) == 'bgrid') then
   do j = jstart_src, jend_src
   do i = istart_src, iend_src
      iii = i
      if (iii < 1) iii = isrc + iii
      if (iii > isrc) iii = iii - isrc
      category = srcdat(iii,j)
      if (category /= water_category) then
        srclat = lat_11_src + (j-1)*dlat_src
        srclon = lon_11_src + (iii-1)*dlon_src
        call ll2xy_bgrid_pt_loc(centlat_parent_mdl(1), centlon_parent_mdl(1), dy_mdl, dx_mdl, &
                            lat_first_mdl, lon_first_mdl, imdl, jmdl, srclat, srclon, nearest_i, nearest_j)
        if (nearest_i >= istart_mdl .and. nearest_i <= iend_mdl .and. &
            nearest_j >= jstart_mdl .and. nearest_j <= jend_mdl) then
          count_cat(nearest_i,nearest_j,category) = count_cat(nearest_i,nearest_j,category) + 1
          group                   = cat_groups(category)
          count_group(nearest_i,nearest_j,group)  = count_group(nearest_i,nearest_j,group) + 1
        end if
      end if
   enddo
   enddo
 elseif (trim(domain_type) == 'lambconf') then
   dx_meters = dx_mdl*1000.
   do j = jstart_src, jend_src
   do i = istart_src, iend_src
      iii = i
      if (iii < 1) iii = isrc + iii
      if (iii > isrc) iii = iii - isrc
      category = srcdat(iii,j)
      if (category /= water_category) then
        srclat = lat_11_src + (j-1)*dlat_src
        srclon = lon_11_src + (iii-1)*dlon_src
        call w3fb11(srclat,srclon,lat_first_mdl,lon_first_mdl,dx_meters,&
                    orient_lon_mdl, tangent_lat_mdl,xgrid,ygrid) 
        nearest_i = nint(xgrid)
        nearest_j = nint(ygrid)
        if (nearest_i >= istart_mdl .and. nearest_i <= iend_mdl .and. &
            nearest_j >= jstart_mdl .and. nearest_j <= jend_mdl) then
          count_cat(nearest_i,nearest_j,category) = count_cat(nearest_i,nearest_j,category) + 1
          group                   = cat_groups(category)
          count_group(nearest_i,nearest_j,group)  = count_group(nearest_i,nearest_j,group) + 1
        end if
      end if
   enddo
   enddo
 elseif (trim(domain_type) == "gaussian") then
   allocate (lats_src(jstart_src:jend_src))
   allocate (lons_src(jstart_src:jend_src))
   lons_src = 0.0
   do j = jstart_src, jend_src
     lats_src(j) = lat_11_src + (j-1)*dlat_src
   enddo
   allocate (dum(jstart_src:jend_src))
   allocate (ypts(jstart_src:jend_src))
   jsrc = jend_src - jstart_src + 1
   call gdswiz04(kgds_mdl,-1,jsrc,-999.9,dum,ypts,lons_src,lats_src, &
                 nret, 0, dum, dum)
   deallocate (dum, lons_src, lats_src)
   do j = jstart_src, jend_src
     nearest_j = nint(ypts(j))
     if (nearest_j >= jstart_mdl .and. nearest_j <= jend_mdl) then
       jj = nearest_j
       if (nearest_j > jmdl/2) jj = jmdl - nearest_j + 1
       do i = 1, isrc
         category = srcdat(i,j)
         if (category /= water_category) then
           srclon = lon_11_src + (i-1)*dlon_src
           nearest_i = nint(srclon / dx_gfs(nearest_j) + 1.0)
           if (nearest_i > imdl) then
             nearest_i = nearest_i - lonsperlat_mdl(jj)
           else if (nearest_i < 1) then
             nearest_i = nearest_i + lonsperlat_mdl(jj)
           end if
           count_cat(nearest_i,nearest_j,category) = count_cat(nearest_i,nearest_j,category) + 1
           group = cat_groups(category)
           count_group(nearest_i,nearest_j,group) =  count_group(nearest_i,nearest_j,group) + 1
         endif
       enddo
     endif
   enddo
   deallocate (ypts)
 else
   print*,'- ROUTINE INTERP_TILES: UNRECOGNIZED DOMAIN TYPE'
   call mpi_abort(mpi_comm_world, 1, ierr)
 end if

 JLOOP : do j = jstart_mdl, jend_mdl
 ILOOP : do i = istart_mdl, iend_mdl_4_loops(j)

   if (lsmask(i,j) == 0.0) cycle ILOOP

!  ------------------------------------------------------------------
!  if we found valid categories in the box, calculate the percentage
!  of each.  otherwise use a default category.
!
!  do not include any categories that represent less than the
!  tile threshold in this percentage.
!  ------------------------------------------------------------------

   total_count     = sum(count_cat(i,j,:))
   total_count_sav = total_count

   CALC_TILES : if (total_count > 0) then

     do cat = 1, num_categories

       percent_cat = float(count_cat(i,j,cat)) / float(total_count_sav)

       if (percent_cat > 0.0 .and. percent_cat < tile_threshold) then
         total_count            = total_count - count_cat(i,j,cat)
         group                  = cat_groups(cat)
         count_group(i,j,group)  = count_group(i,j,group) - count_cat(i,j,cat)
         count_cat(i,j,cat)      = 0
       end if

       if (total_count == 0) then
! might want to pick dominate category in this instance.
! however, this does not happen if you pick a low threshold, 
! say 10% or less. 
         print*,'no cats above threshold at ',i,j
         call mpi_abort(mpi_comm_world, 1, ierr)
       end if

     enddo

     pred_group = maxloc(count_group(i,j,:))
     maxcat     = -999.9

!-------------------------------------------------------------------
! calculate final % of each category in each grid box.
! calculate dominate category - the predominate category within
! the predominate group.
!-------------------------------------------------------------------

     do cat = 1, num_categories 
       if (count_cat(i,j,cat) > 0) then
         out_cat(i,j,cat) = float(count_cat(i,j,cat))  / &
                           float(total_count) * 100.0
         if (cat_groups(cat) == pred_group(1) .and. &
             out_cat(i,j,cat) > maxcat ) then
           dominate_cat(i,j) = cat
           maxcat            = out_cat(i,j,cat)
         end if
       else
         out_cat(i,j,cat) = 0.0
       end if
     enddo

     do grp = 1, num_groups
       out_group(i,j,grp) = float(count_group(i,j,grp)) /  & 
                            float(sum(count_group(i,j,:))) * 100.0
     enddo

   else  ! total count is zero

! do a spiral search to find data.

     out_cat(i,j,:) = 0.0
     out_group(i,j,:) = 0.0

     jgrid  = nint( (lat_mdl(i,j) - lat_11_src) / dlat_src + 1.0 )
     igrid  = nint( (lon_mdl(i,j) - lon_11_src) / dlon_src + 1.0 )

     if (igrid > isrc) then     ! cross dateline
       igrid = igrid - isrc
     else if (igrid < 1) then
       igrid = igrid + isrc
     end if

     spiral_rad = nint(2.0 / abs(dlat_src))  ! 2 degree maximum search rad
                                            
     SPIRAL_SEARCH : do krad = 1, spiral_rad

       istart = igrid - krad
       iend   = igrid + krad
       jstart = jgrid - krad
       jend   = jgrid + krad

       do jj = jstart, jend
       do ii = istart, iend

!-----------------------------------------------------------------------
!        search only along outer square.
!-----------------------------------------------------------------------

         if ((jj == jstart) .or. (jj == jend) .or.   &
             (ii == istart) .or. (ii == iend))  then

!-----------------------------------------------------------------------
!           ensure that point being investigated is within
!           the northern and southern bounds of the source grid.
!-----------------------------------------------------------------------

            if ((jj >= jstart_src) .and. (jj <= jend_src)) then

              jjj = jj

!-----------------------------------------------------------------------
!             adjust i-index on source grid when search
!             crosses the date line.
!-----------------------------------------------------------------------

              if (ii <= 0) then
                iii = isrc + ii
              else if (ii >= (isrc+1)) then
                iii = ii - isrc
              else
                iii = ii
              end if

!-----------------------------------------------------------------------
!             a valid value was found.
!-----------------------------------------------------------------------

              if (srcdat(iii,jjj) /= water_category) then
                category = srcdat(iii,jjj)
                out_cat(i,j,category) = 100.0
                dominate_cat(i,j) = category
                group = cat_groups(category)
                out_group(i,j,group) = 100.0
                write (6, 6000) i,j, krad
                cycle ILOOP
              end if
            end if

          end if

        enddo
        enddo

      enddo SPIRAL_SEARCH

!--------------------------------------------------------------------------
!     there was no source data within the search area, assign a default
!     value. 
!--------------------------------------------------------------------------

      out_cat(i,j,default_category) = 100.0
      dominate_cat(i,j)             = default_category
      out_group(i,j,default_group)  = 100.0
      write(6,6100) i,j

    end if CALC_TILES

  enddo ILOOP
  enddo JLOOP

 return

 6000 FORMAT (1X, '-- CIRCULAR SEARCH AT POINT ', I4, 1X, I4, ' ITERATIONS ',I4)
 6100 FORMAT (1X, '-- DEFAULT CATEGORY ASSIGNED AT PNT ', 1X,I4,1X,I4)

 end subroutine interp_tiles
