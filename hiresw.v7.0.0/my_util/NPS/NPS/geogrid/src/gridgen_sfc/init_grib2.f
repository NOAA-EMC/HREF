 module init_grib2

 contains

 subroutine grib2_init(gfld)

!-----------------------------------------------------------
! initialize data structure required for grib 2 library
! with default values.
!-----------------------------------------------------------
 
 use grib_mod

 use program_setup, only   : imdl, jmdl, &
                             domain_type, dx_mdl,      &
                             dy_mdl,      &
                             centlat_parent_mdl, &
                             centlon_parent_mdl

 use calc_latlons, only    : lat_first_hpnt_unrot_mdl, &
                             lon_first_hpnt_unrot_mdl, &
                             lat_first_mdl, lon_first_mdl, &
                             lat_last_mdl, lon_last_mdl

 implicit none

 include 'mpif.h'

 integer          :: iret

 real             :: lat_last_hpnt_unrot_mdl
 real             :: lon_last_hpnt_unrot_mdl
 real             :: scale

 type(gribfield)  :: gfld

 nullify(gfld%idsect)
 nullify(gfld%local)
 nullify(gfld%list_opt)
 nullify(gfld%igdtmpl)
 nullify(gfld%ipdtmpl)
 nullify(gfld%coord_list)
 nullify(gfld%idrtmpl)
 nullify(gfld%bmap)
 nullify(gfld%fld)

! *** Section 0 ***

 gfld%version = 2
 gfld%discipline = 2

! *** Section 1 ***

 gfld%idsectlen=13
 allocate(gfld%idsect(gfld%idsectlen))
 gfld%idsect(1) = 7 ! octs 6-7; center (ncep)
 gfld%idsect(2) = 4 ! octs 8-9; subcenter (emc)
 gfld%idsect(3) = 8 ! oct 10;   grib master table version number
! note!! to use a parmeter number >= 192 you must set the local table to '1'
 gfld%idsect(4) = 1 ! oct 11;   local table
 gfld%idsect(5) = 0 ! oct 12;   significance of reference time (analysis time)
 gfld%idsect(6) = 1900 ! octs 13-14;  year
 gfld%idsect(7) = 1    ! oct 15; month
 gfld%idsect(8) = 1    ! oct 16; day
 gfld%idsect(9) = 0    ! oct 17; hour
 gfld%idsect(10) = 0   ! oct 18; minute
 gfld%idsect(11) = 0   ! oct 19; seconds
 gfld%idsect(12) = 0   ! oct 20; production status (operational product)
 gfld%idsect(13) = 0   ! oct 21; type of data (analysis products)

! *** Section 2 ***

 gfld%locallen=0

! *** Section 3 ***

 if (trim(domain_type) == 'bgrid') then
   gfld%igdtlen=22
   allocate(gfld%igdtmpl(gfld%igdtlen))
   gfld%griddef = 0         ! oct 6;  source of grid definition, specified in table 3.1
   gfld%ngrdpts = imdl*jmdl ! octs 7-10; number of grid points
   gfld%numoct_opt = 0      ! oct 11; number of octs for optional list of numbers defining # of grid points
                            ! oct 12; list of #s defining # of points
   gfld%igdtnum=1           ! octs 13-14; grid definition template number, template 3.1 - rotated lat/lon
   gfld%igdtmpl(1)=6         ! oct 15; shape of the earth, spherical with radius 6371229 meters.
   gfld%igdtmpl(2)=255       ! oct 16; scale factor of radius of spherical earth, not used.
   gfld%igdtmpl(3)=-1        ! octs 17-20; scale value of radius of spherical earth, not used.
   gfld%igdtmpl(4)=255       ! oct 21; scale factor of major axis of elliptical earth, not used.
   gfld%igdtmpl(5)=-1        ! octs 22-25; scaled value of major axis of elliptical earth, not used.
   gfld%igdtmpl(6)=255       ! oct 26; scale factor of minor axis of elliptical earth, not used.
   gfld%igdtmpl(7)=-1        ! octs 27-30; scaled value of minor axis of elliptical earth, not used.
   gfld%igdtmpl(8)=imdl       ! octs 31-34; # "i" points
   gfld%igdtmpl(9)=jmdl       ! octs 35-38; # "j" points
   gfld%igdtmpl(10)=1         ! octs 39-42; basic angle
   gfld%igdtmpl(11)=10**6     ! octs 43-46; subdivisions of basic angle
   scale=float(gfld%igdtmpl(10)*gfld%igdtmpl(11))

!  Note: for rotated lat/lon grids, the lat/lon of the corner points is the unrotated value.
!  The ecmwf and german wx service do it this way.  And wgrib2 assumes this as well (when
!  doing the -ijlat option). The grib 2 manual is unclear.

   gfld%igdtmpl(12)=nint(lat_first_hpnt_unrot_mdl*scale)  ! octs 47-50; lat of first grid point
   if (lon_first_hpnt_unrot_mdl < 0.0) then
     gfld%igdtmpl(13)=nint((lon_first_hpnt_unrot_mdl+360.0)*scale)  ! octs 51-54; lon of first grid point
   else
     gfld%igdtmpl(13)=nint(lon_first_hpnt_unrot_mdl*scale)
   endif
   gfld%igdtmpl(14)=48        ! oct 55;  resolution and component flags
   lat_last_hpnt_unrot_mdl=lat_first_hpnt_unrot_mdl + (jmdl-1)*dy_mdl
   lon_last_hpnt_unrot_mdl=lon_first_hpnt_unrot_mdl + (imdl-1)*dx_mdl
   gfld%igdtmpl(15)=nint(lat_last_hpnt_unrot_mdl*scale) ! octs 56-59; lat of last grid point
   if (lon_last_hpnt_unrot_mdl < 0.0) then
     gfld%igdtmpl(16)=nint((lon_last_hpnt_unrot_mdl+360.0)*scale) ! octs 60-63; lon of last grid point
   else
     gfld%igdtmpl(16)=nint(lon_last_hpnt_unrot_mdl*scale) ! octs 60-63; lon of last grid point
   endif
   gfld%igdtmpl(17)=nint(dx_mdl*scale)   ! oct 64-67; i direction increment
   gfld%igdtmpl(18)=nint(dy_mdl*scale)   ! oct 68-71; j direction increment
   gfld%igdtmpl(19)=64  ! oct 72; scanning mode flag
   gfld%igdtmpl(20)=nint((centlat_parent_mdl(1)-90.)*scale)  ! octs 73-76; lat of south pole of projection
   if (centlon_parent_mdl(1) < 0.) then
     gfld%igdtmpl(21)=nint((centlon_parent_mdl(1)+360.0)*scale) ! octs 77-80; long of southern pole of projection.
   else
     gfld%igdtmpl(21)=nint(centlon_parent_mdl(1)*scale) ! octs 77-80; long of southern pole of projection.
   endif
   gfld%igdtmpl(22)=nint(0.0)  ! octs 81-84; angle of rotation of projection
   gfld%num_opt = 0           ! octs 85-??; number of optional grid points.
 elseif (trim(domain_type) == 'gaussian') then
   gfld%igdtlen=19
   allocate(gfld%igdtmpl(gfld%igdtlen))
   gfld%griddef = 0         ! oct 6;  source of grid definition, specified in table 3.1
   gfld%ngrdpts = imdl*jmdl ! octs 7-10; number of grid points
   gfld%numoct_opt = 0      ! oct 11; number of octs for optional list of numbers defining # of grid points
                            ! oct 12; list of #s defining # of points
   gfld%igdtnum=40          ! octs 13-14; grid definition template number, template 3.40 - gaussain
   gfld%igdtmpl(1)=5          ! oct 15; shape of the earth, wgs84
   gfld%igdtmpl(2)=255        ! oct 16; scale factor of radius of spherical earth, not used.
   gfld%igdtmpl(3)=-1         ! octs 17-20; scale value of radius of spherical earth, not used.
   gfld%igdtmpl(4)=255        ! oct 21; scale factor of major axis of elliptical earth, not used.
   gfld%igdtmpl(5)=-1         ! octs 22-25; scaled value of major axis of elliptical earth, not used.
   gfld%igdtmpl(6)=255        ! oct 26; scale factor of minor axis of elliptical earth, not used.
   gfld%igdtmpl(7)=-1         ! octs 27-30; scaled value of minor axis of elliptical earth, not used.
   gfld%igdtmpl(8)=imdl       ! octs 31-34; # "i" points
   gfld%igdtmpl(9)=jmdl       ! octs 35-38; # "j" points
   gfld%igdtmpl(10)=1         ! octs 39-42; basic angle
   gfld%igdtmpl(11)=10**6     ! octs 43-46; subdivisions of basic angle
   scale=float(gfld%igdtmpl(10)*gfld%igdtmpl(11))
   gfld%igdtmpl(12)=nint(lat_first_mdl*scale)  ! octs 47-50; lat of first grid point
   if (lon_first_mdl < 0.0) then
     gfld%igdtmpl(13)=nint((lon_first_mdl+360.0)*scale)  ! octs 51-54; lon of first grid point
   else
     gfld%igdtmpl(13)=nint(lon_first_mdl*scale)
   endif
   gfld%igdtmpl(14)=48        ! oct 55;  resolution and component flags
   gfld%igdtmpl(15)=nint(lat_last_mdl*scale) ! octs 56-59; lat of last grid point
   if (lon_last_mdl < 0.0) then
     gfld%igdtmpl(16)=nint((lon_last_mdl+360.0)*scale) ! octs 60-63; lon of last grid point
   else
     gfld%igdtmpl(16)=nint(lon_last_mdl*scale) ! octs 60-63; lon of last grid point
   endif
   gfld%igdtmpl(17)=nint(dx_mdl*scale) ! octs 64-67; di of grid
   gfld%igdtmpl(18)= jmdl/2 ! octs 68-71; # grid pts between pole and equator
   gfld%igdtmpl(19)=0   ! oct 72; scanning mode flag
 else
   print*,'- GRIB2 OPTION DOES NOT SUPPORT THIS MAP PROJECTION ',trim(domain_type)
   call mpi_abort(mpi_comm_world, 1, iret)
 end if

! *** Section 4 ***

 gfld%num_coord=0           ! octs 6-7; number of coordinate values after template.
 gfld%ipdtnum=0             ! octs 8-9; product definition template number - table 4.0
 gfld%ipdtlen=15
 allocate(gfld%ipdtmpl(gfld%ipdtlen))
 gfld%ipdtmpl(1)= 0         ! oct 10; parameter category
! note!! to use a parmeter number >= 192 you must set the local table to '1'
 gfld%ipdtmpl(2)= 0         ! oct 11; parameter
 gfld%ipdtmpl(3)= 0         ! oct 12; type of generating process
 gfld%ipdtmpl(4)= 255       ! oct 13; background generating process identifier
 gfld%ipdtmpl(5)= 84        ! oct 14; analysis generating process identifier
 gfld%ipdtmpl(6)= 0         ! octs 15-16; hours after ob cutoff
 gfld%ipdtmpl(7)= 0         ! oct 17; minutes after ob cutoff
 gfld%ipdtmpl(8)= 1         ! oct 18; unit of time range
 gfld%ipdtmpl(9)= 0         ! octs 19-22; forecast time in units defined by oct 18
 gfld%ipdtmpl(10)=1         ! oct 23; type of first fixed surface
 gfld%ipdtmpl(11)=0         ! oct 24; scale factor of first fixed surface
 gfld%ipdtmpl(12)=0         ! octs 25-28; scale value of first fixed surface
 gfld%ipdtmpl(13)=255       ! oct 29; type of second fixed surface
 gfld%ipdtmpl(14)=255       ! oct 30; scale factor of second fixed surface
 gfld%ipdtmpl(15)=-2147483647 ! octs 31-34; scaled value of second fixed surface
                              ! note! for these particular octets, using -1 as
                              ! missing does not work because -1 may be an actual
                              ! scaled value.  after looking thru the g2 library
                              ! and some trial and error, i determined that missing
                              ! is minus 2**31-1.

! *** Section 5 ***

 gfld%idrtnum=0    ! data representation template number - table 5.0 simple packing
 gfld%idrtlen=5
 allocate(gfld%idrtmpl(gfld%idrtlen))
 gfld%idrtmpl=0
 gfld%idrtmpl(3)=2  ! decimal scaling factor

 gfld%ibmap=255   ! bitmap does not apply
 allocate(gfld%bmap(gfld%ngrdpts))
 gfld%bmap=.false.

 allocate(gfld%fld(gfld%ngrdpts)) ! will hold data values

 end subroutine grib2_init

 subroutine grib2_free(gfld)

! A modified copy of nco routine gf_free, which has serious
! logic problems.

 use grib_mod

 implicit none

 type(gribfield)  :: gfld

 deallocate(gfld%idsect)
 deallocate(gfld%igdtmpl)
 deallocate(gfld%ipdtmpl)
 deallocate(gfld%idrtmpl)
 deallocate(gfld%bmap)
 deallocate(gfld%fld)

 return

 end subroutine grib2_free

 end module init_grib2
