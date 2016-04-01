 subroutine find_bounds_ll(isrc, jsrc, lat_11_src, &
                           lon_11_src, dlat_src, dlon_src, &
                           istart_src, iend_src, jstart_src, jend_src )

!----------------------------------------------------------------------
! for the model points on an mpi task, find the corresponding
! i/j bounds on the source grid.  this routine works for source
! grids that are global regular lat/lon.
!----------------------------------------------------------------------

 use program_setup, only          : domain_type

 use calc_latlons, only           : lat_mdl, lon_mdl

 use mpimod, only                 : istart_mdl, iend_mdl, &
                                    jstart_mdl, jend_mdl

 implicit none

 include 'mpif.h'

 integer, intent(in)             :: isrc, jsrc   ! i/j dimesions of source grid
 integer                         :: i, j, ierr, imid, jmid
 integer, intent(out)            :: istart_src, iend_src
 integer, intent(out)            :: jstart_src, jend_src

 real, parameter                 :: cushion = 2.0  ! in degrees
 real                            :: diff, maxlat, minlat, maxlon, minlon
 real, intent(in)                :: dlat_src, dlon_src ! n/s and e/w source grid
                                                       ! resolution in degrees.
 real, intent(in)                :: lat_11_src, lon_11_src  ! lat/lon of
                                                            ! source grid point (1,1)                      

!----------------------------------------------------------------------
! the j bounds are a function of the max/min latitude on the model
! grid.  the north/south cushion is used for the 'search' portion
! of the interpolation algorithms.
!----------------------------------------------------------------------

 maxlat     = min(maxval(lat_mdl) + cushion,  90.0)
 minlat     = max(minval(lat_mdl) - cushion, -90.0)

 if (dlat_src < 0.0) then
   jstart_src      = nint((maxlat - lat_11_src) / dlat_src + 1.0)
   jend_src        = nint((minlat - lat_11_src) / dlat_src + 1.0)
 else
   jstart_src      = nint((minlat - lat_11_src) / dlat_src + 1.0)
   jend_src        = nint((maxlat - lat_11_src) / dlat_src + 1.0)
 endif
 jstart_src = max(jstart_src,1)
 jend_src   = min(jend_src, jsrc)

!----------------------------------------------------------------------
! because gaussian grids are global, need to check all i points.
!----------------------------------------------------------------------

 if (trim(domain_type) == "gaussian") then  ! global, check all i pts
   istart_src = 1
   iend_src   = isrc
 else  ! regional egrids 
!----------------------------------------------------------------------
! if near the pole, need to check all 'i' source points
!----------------------------------------------------------------------
   if (maxval(lat_mdl) > 85.0 .or. minval(lat_mdl) < -85.0) then
     istart_src = 1
     iend_src   = isrc
!----------------------------------------------------------------------
! check longitude of each model point against the longitude of
! the point at the center of the model sub grid to determine
! the eastmost and westmost longitude.  from this get the
! starting/ending 'i' source indices that envelope the
! model grid for this task.  istart_src may be negative and 
! iend_src may be greater than isrc near the 'dateline'.    
!----------------------------------------------------------------------
   else
     jmid = ((jend_mdl - jstart_mdl) / 2) + jstart_mdl
     imid = ((iend_mdl - istart_mdl) / 2) + istart_mdl
     minlon = 99999.
     maxlon = -99999.
     do j = jstart_mdl, jend_mdl
     do i = istart_mdl, iend_mdl
       diff = mod(lon_mdl(i,j)-lon_mdl(imid,jmid)+3600.,360.)
       if ( diff > 180.) then   ! west of center
         diff = diff - 360.0
         minlon = min(minlon, diff)
       else                     ! east of center
         maxlon = max(maxlon, diff)
       end if
     enddo
     enddo
     istart_src = nint((minlon+lon_mdl(imid,jmid)-cushion - lon_11_src) / dlon_src + 1.0)
     iend_src   = nint((maxlon+lon_mdl(imid,jmid)+cushion - lon_11_src) / dlon_src + 1.0)
   end if
 endif

 print*,'- SOURCE GRID BOUNDS IS/IE/JS/JE: ',istart_src,iend_src,jstart_src,jend_src

 return

 end subroutine find_bounds_ll

