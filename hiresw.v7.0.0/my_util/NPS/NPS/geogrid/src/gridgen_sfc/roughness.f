 subroutine roughness

 use init_grib1, only            : kpds_mdl,           &
                                   kgds_mdl

 use lsmask_orog, only           : lsmask,             &
                                   lbms_lnd_mdl

 use program_setup, only         : roughness_file,     &
                                   imdl,               &
                                   jmdl,               &
                                   lonsperlat_mdl,     &
                                   thinned, domain_name, grib2

 use soil_vegtype_tile, only     : dominate_veg_cat

 use grib_mod

 use init_grib2

 use mpimod, only                : istart_mdl, iend_mdl, jstart_mdl, &
                                   jend_mdl, myrank, gather

 implicit none

 include 'mpif.h'

 character*3                    :: interp_mask
 character*2                    :: interp_type
 character*250                  :: output_file

 integer                        :: grib_scale_fac
 integer                        :: i, j
 integer                        :: iret
 integer                        :: iunit_out
 integer                        :: kgds(200)
 integer                        :: kpds(200)

 real                           :: default_value
 real, allocatable              :: dum_all(:,:)
 real, allocatable              :: z0(:,:)
 real, pointer                  :: z0veg(:)
 real, target                   :: z0veg_igbp(20)  ! in meters
 real, target                   :: z0veg_usgs(24)  ! in meters

 type(gribfield)                :: gfld

! must use usgs vegetation types.

 data z0veg_usgs / 1.00,  0.07,  0.07,  0.07,  0.07,  0.15, &
                   0.08,  0.03,  0.05,  0.86,  0.80,  0.85, &
                   2.65,  1.09,  0.80,  0.001, 0.04,  0.05, &
                   0.01,  0.04,  0.06,  0.05,  0.03,  0.001 /

!-- IGBP z0s including Caterina's modifications

 data z0veg_igbp / 1.800, 2.653, 0.854, 2.467, 1.966, 0.050, &
                   0.030, 0.856, 0.856, 0.080, 0.040, 0.170, &
                   1.000, 0.500, 0.011, 0.011, 0.001, 0.076, &
                   0.050, 0.030 / 

!-----------------------------------------------------------------------
! initialize some variables, then call interp driver.
!-----------------------------------------------------------------------

 if (len_trim(roughness_file) == 0) return

 if (grib2) then
   output_file    = trim(domain_name)//"_z0clim.grb2"   ! grib file of data on model grid.
 else
   output_file    = trim(domain_name)//"_z0clim.grb"   ! grib file of data on model grid.
 endif
 iunit_out      =  35         ! unit # of above.

 if (trim(roughness_file) == "usgs" .or. trim(roughness_file) == "igbp") then

   allocate (z0(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))
   z0 = 0.0
  
   print*,"- CALCULATE ROUGHNESS LENGTH AS IN THE NMM MODEL"

   if (.not. allocated(dominate_veg_cat)) then
     print*,"- VEGETATION TYPE MUST BE AVAILABLE."
     call mpi_abort(mpi_comm_world, 1, iret)
   end if

   if (trim(roughness_file) == "usgs") then
     z0veg=>z0veg_usgs
   elseif (trim(roughness_file) == "igbp") then
     z0veg=>z0veg_igbp
   else
     print*,"- INVALID CHOICE OF VEGETATION TYPE: ", trim(roughness_file)
     call mpi_abort(mpi_comm_world, 1, iret)
   endif

   do j = jstart_mdl, jend_mdl
     do i = istart_mdl, iend_mdl
       if (lsmask(i,j) > 0.0) then
         z0(i,j) =  z0veg(dominate_veg_cat(i,j))
! nmmb adds 0.1 meters to roughness for all but cat 15 - perm ice/snow.
         if (trim(roughness_file) == "igbp" .and. dominate_veg_cat(i,j) /= 15) then
            z0(i,j) = z0(i,j) + 0.1 ! 
         end if
       end if
     enddo
   enddo

   allocate(dum_all(imdl,jmdl))
   call gather(z0, imdl, jmdl, dum_all)

   if (myrank == 0) then
     print*,"- OPEN: ", trim(output_file)
     call baopenw(iunit_out, output_file, iret)
     if (iret /= 0) then
       print*,'- BAD OPEN, IRET IS ', iret
       call mpi_abort(mpi_comm_world, 1, iret)
     end if
   end if

   if (grib2) then
     call grib2_init(gfld)
     gfld%discipline = 2
     gfld%ipdtmpl(1)= 0  ! oct 10; parameter category
     gfld%ipdtmpl(2)= 1  ! oct 11; parameter
     gfld%idrtmpl=0
     gfld%idrtmpl(3)=3 ! decimal scaling factor
     gfld%ibmap=0
   else
     kpds = kpds_mdl
     kgds = kgds_mdl
     kpds(5) = 83  ! roughness length in meters
     kpds(22) = 3  ! scaling factor
   endif

   print*,"- WRITE: ", trim(output_file)

   if (myrank == 0) then
     if (grib2) then
       gfld%fld=reshape(dum_all, (/imdl*jmdl/) )
       gfld%bmap=reshape(lbms_lnd_mdl, (/imdl*jmdl/) )
       call putgb2(iunit_out,gfld,iret)
     else
       call putgb (iunit_out, (imdl*jmdl), kpds, kgds, lbms_lnd_mdl,    &
                   dum_all, iret)
     endif
     if (iret /= 0) then
       print*,'- BAD WRITE OF FILE:', trim(output_file), ' IRET IS ', iret
       call mpi_abort(mpi_comm_world, 1, iret)
     end if
     call baclose(iunit_out, iret)
   end if 

   deallocate(dum_all,dominate_veg_cat)
   deallocate(z0)

   if (grib2) then
     call grib2_free(gfld)
   end if

   return

 end if

!-----------------------------------------------------------------------
! if the usgs, igbp or eta option was not chosen, interpolate an
! external roughness database to model grid.
!-----------------------------------------------------------------------
 
 print*,"- INTERPOLATE ROUGHNESS LENGTH DATA TO MODEL GRID"

 grib_scale_fac =  3          ! # decimal places (-1 same as input data) 
 default_value  =  .10        ! if interp routine can not find data
                              ! at a model grid point, set to this value.
 interp_type    = "xx"        ! let routine logic choose interp method
 interp_mask    = "lnd"       ! a land field

 call interp_to_mdl(roughness_file, output_file, iunit_out, &
                    interp_type, default_value, grib_scale_fac, &
                    interp_mask)

 return

 end subroutine roughness
