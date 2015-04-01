 subroutine green

 use program_setup, only         : gfrac_file, domain_name, grib2

 implicit none

 character*3                    :: interp_mask
 character*2                    :: interp_type
 character*256                  :: output_file

 integer                        :: grib_scale_fac
 integer                        :: iunit_out

 real                           :: default_value

!-----------------------------------------------------------------------
! initialize some variables, then call interp driver.
!-----------------------------------------------------------------------

 if (len_trim(gfrac_file) == 0) return

 print*,"- INTERPOLATE GREENNESS FRACTION TO MODEL GRID"
 
 if (grib2) then
   output_file    = trim(domain_name)//"_vegfrac.grb2"   ! grib file of data on model grid.
 else
   output_file    = trim(domain_name)//"_vegfrac.grb"   ! grib file of data on model grid.
 endif
 iunit_out      =  44         ! unit # of above.
 grib_scale_fac =  -1         ! # decimal places (-1 same as input data) 
 default_value  =  0.5        ! if interp routine can not find data
                              ! at a model grid point, set to this value.
 interp_type    = "xx"        ! let routine logic choose interp method
 interp_mask    = "lnd"       ! a land field

 call interp_to_mdl(gfrac_file, output_file, iunit_out, &
                    interp_type, default_value, grib_scale_fac, &
                    interp_mask)

 return

 end subroutine green
