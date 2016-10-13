 module write_data

 use consts, only            : rovcp

 use program_setup, only     : output_file,   &
                               imdl_output, jmdl_output, &
                               ijmdl_output,  &
                               output_file_type

 use interp_data, only       : nsoil_output, &
                               data_output

 contains

!-----------------------------------------------------------------------
! nems is a nemio file.
!-----------------------------------------------------------------------

 subroutine write_output_data_driver

 implicit none

 include 'mpif.h'

 integer    :: iret

 select case(trim(output_file_type))
   case ("nems", "NEMS")
     call write_output_data_nems
   case ("netcdf", "NETCDF", "NetCDF")
     call write_output_data_NMMnetcdf
   case ("arwnetcdf", "arwNETCDF", "arwNetCDF")
     call write_output_data_ARWnetcdf
   case default
     print*,'** INVALID OUTPUT FILE TYPE: ',trim(output_file_type)
     call w3tage('COLDSTART_WRF')
     call mpi_abort(mpi_comm_world, 42, iret)
 end select

 return

 end subroutine write_output_data_driver

 subroutine write_output_data_ARWnetcdf

!-----------------------------------------------------------------------
! update surface records for an arw netcdf file.
!-----------------------------------------------------------------------

 use netcdf

 implicit none

 include 'mpif.h'
 include 'netcdf.inc'

 character*80           :: errmsg

 integer                :: ij, iret, mode, n, ncid, varid
 real, allocatable      :: dummy1d(:), dummy2d(:,:), dummy3d(:,:,:)

 print*,'- OPEN FILE: ', trim(output_file)
 mode=1
 iret = nf90_open(output_file, nf90_write, ncid)
 if (iret /= nf90_noerr) goto 7100

 allocate(dummy1d(ijmdl_output))
 allocate(dummy2d(imdl_output,jmdl_output))

 print*,"- UPDATE GREENNESS."
 dummy1d = data_output%greenfrc*100.
 dummy2d = reshape(dummy1d, (/imdl_output,jmdl_output/) )
 iret = nf90_inq_varid(ncid, 'VEGFRA', varid)
 if (iret /= nf90_noerr) goto 7100
 iret = nf90_put_var(ncid, varid, dummy2d)
 if (iret /= nf90_noerr) goto 7100

 print*,"- UPDATE SOIL TYPE."
 dummy1d = data_output%soil_type
 do ij = 1, ijmdl_output
   if (data_output%sea_ice_flag(ij) == 1) dummy1d(ij)= 16
   if (data_output%sea_ice_flag(ij) == 0 .and.  &
       data_output%lsmask(ij) == 0.0 ) then
     dummy1d(ij) = 14
   endif
 enddo
 dummy2d = reshape(dummy1d, (/imdl_output,jmdl_output/) )
 iret = nf90_inq_varid(ncid, 'ISLTYP', varid)
 if (iret /= nf90_noerr) goto 7100
 iret = nf90_put_var(ncid, varid, dummy2d)
 if (iret /= nf90_noerr) goto 7100

! there are two veg type records, and they are identical.
! ncar said IVGTYP is used within the land physics.
! they were not sure if LU_INDEX was used or not.
! i will update both.

 print*,"- UPDATE VEGETATION TYPE."
 dummy1d = data_output%veg_type
 do ij = 1, ijmdl_output
   if (data_output%sea_ice_flag(ij) == 1) dummy1d(ij)= 24
   if (data_output%sea_ice_flag(ij) == 0 .and.  &
       data_output%lsmask(ij) == 0.0 ) then
     dummy1d(ij) = 16
   endif
 enddo
 dummy2d = reshape(dummy1d, (/imdl_output,jmdl_output/) )
 iret = nf90_inq_varid(ncid, 'IVGTYP', varid)
 if (iret /= nf90_noerr) goto 7100
 iret = nf90_put_var(ncid, varid, dummy2d)
 if (iret /= nf90_noerr) goto 7100
 iret = nf90_inq_varid(ncid, 'LU_INDEX', varid)
 if (iret /= nf90_noerr) goto 7100
 iret = nf90_put_var(ncid, varid, dummy2d)
 if (iret /= nf90_noerr) goto 7100

 print*,"- UPDATE SNOWFREE ALBEDO."
 dummy1d = data_output%snow_free_albedo
 dummy2d = reshape(dummy1d, (/imdl_output,jmdl_output/) )
 iret = nf90_inq_varid(ncid, 'ALBBCK', varid)
 if (iret /= nf90_noerr) goto 7100
 iret = nf90_put_var(ncid, varid, dummy2d)
 if (iret /= nf90_noerr) goto 7100

!-----------------------------------------------------------------------
! skin temperature (visual inspection showed skin temp and sst records
! are the same in arw files).
!-----------------------------------------------------------------------

 print*,"- UPDATE SKIN TEMPERATURE."
 dummy1d = data_output%skin_temp
 dummy2d = reshape(dummy1d, (/imdl_output,jmdl_output/) )
 iret = nf90_inq_varid(ncid, 'TSK', varid)
 if (iret /= nf90_noerr) goto 7100
 iret = nf90_put_var(ncid, varid, dummy2d)
 if (iret /= nf90_noerr) goto 7100

 print*,"- UPDATE SST."
 dummy1d = data_output%skin_temp
 dummy2d = reshape(dummy1d, (/imdl_output,jmdl_output/) )
 iret = nf90_inq_varid(ncid, 'SST', varid)
 if (iret /= nf90_noerr) goto 7100
 iret = nf90_put_var(ncid, varid, dummy2d)
 if (iret /= nf90_noerr) goto 7100

! at open water, sst is used.  most likely
! just a flag value
 print*,"- UPDATE SUBSTRATE TEMPERATURE."
 dummy1d = data_output%substrate_temp
 do ij = 1, ijmdl_output
   if (data_output%sea_ice_flag(ij) == 0 .and.  &
       data_output%lsmask(ij) == 0.0 ) then
     dummy1d(ij) = data_output%skin_temp(ij)
   endif
 enddo
 dummy2d = reshape(dummy1d, (/imdl_output,jmdl_output/) )
 iret = nf90_inq_varid(ncid, 'TMN', varid)
 if (iret /= nf90_noerr) goto 7100
 iret = nf90_put_var(ncid, varid, dummy2d)
 if (iret /= nf90_noerr) goto 7100

 print*,"- UPDATE MAXIMUM SNOW ALBEDO."
 dummy1d = data_output%mxsnow_alb
 do ij = 1, ijmdl_output
   if (data_output%sea_ice_flag(ij) == 1) dummy1d(ij)= 0.75
   if (data_output%sea_ice_flag(ij) == 0 .and.  &
       data_output%lsmask(ij) == 0.0 ) then
     dummy1d(ij) = 0.08
   endif
 enddo
 dummy2d = reshape(dummy1d, (/imdl_output,jmdl_output/) )
 iret = nf90_inq_varid(ncid, 'SNOALB', varid)
 if (iret /= nf90_noerr) goto 7100
 iret = nf90_put_var(ncid, varid, dummy2d)
 if (iret /= nf90_noerr) goto 7100

 print*,"- UPDATE LANDMASK."
 dummy1d = data_output%lsmask
 do ij = 1, ijmdl_output
   if (data_output%sea_ice_flag(ij) == 1) then
     dummy1d(ij) = 1.0
   end if
 enddo

 dummy2d = reshape(dummy1d, (/imdl_output,jmdl_output/) )
 iret = nf90_inq_varid(ncid, 'LANDMASK', varid)
 if (iret /= nf90_noerr) goto 7100
 iret = nf90_put_var(ncid, varid, dummy2d)
 if (iret /= nf90_noerr) goto 7100

 print*,"- UPDATE XLAND."

 dummy1d = 0.0

 do ij = 1, ijmdl_output
   if (data_output%lsmask(ij) == 1.0) then
     dummy1d(ij) = 1.0
   else
     if (data_output%sea_ice_flag(ij) == 1) then
       dummy1d(ij) = 1.0
     else
       dummy1d(ij) = 2.0
     end if
   end if
 enddo

 dummy2d = reshape(dummy1d, (/imdl_output,jmdl_output/) )
 iret = nf90_inq_varid(ncid, 'XLAND', varid)
 if (iret /= nf90_noerr) goto 7100
 iret = nf90_put_var(ncid, varid, dummy2d)
 if (iret /= nf90_noerr) goto 7100

 print*,"- UPDATE SEAICE."
 dummy1d = data_output%sea_ice_flag
 dummy2d = reshape(dummy1d, (/imdl_output,jmdl_output/) )
 iret = nf90_inq_varid(ncid, 'SEAICE', varid)
 if (iret /= nf90_noerr) goto 7100
 iret = nf90_put_var(ncid, varid, dummy2d)
 if (iret /= nf90_noerr) goto 7100

!-----------------------------------------------------------------------
! canopy water
! according to mike barlage (4/12/2013 email), the arw expects units of meters.
! however, he also said that the lsminit step zeroes out this
! field.  so I will set to zero for now.  logic in the model
! indicated that the lsminit step is only invoked when not
! restarting the model.
!-----------------------------------------------------------------------

 print*,"- UPDATE CANWAT."
 dummy1d = 0.0
 dummy2d = reshape(dummy1d, (/imdl_output,jmdl_output/) )
 iret = nf90_inq_varid(ncid, 'CANWAT', varid)
 if (iret /= nf90_noerr) goto 7100
 iret = nf90_put_var(ncid, varid, dummy2d)
 if (iret /= nf90_noerr) goto 7100

 print*,"- UPDATE SNOWH."
 dummy1d = data_output%snow_depth * 0.001
 dummy2d = reshape(dummy1d, (/imdl_output,jmdl_output/) )
 iret = nf90_inq_varid(ncid, 'SNOWH', varid)
 if (iret /= nf90_noerr) goto 7100
 iret = nf90_put_var(ncid, varid, dummy2d)
 if (iret /= nf90_noerr) goto 7100

!-----------------------------------------------------------------------
! snow cover (flag indicating snow cover) don't know if model uses
! this, but will update it just in case.
!-----------------------------------------------------------------------

 dummy1d = 0.0
 where (data_output%snow_depth > 0.0) dummy1d = 1.0
 dummy2d = reshape(dummy1d, (/imdl_output,jmdl_output/) )
 iret = nf90_inq_varid(ncid, 'SNOWC', varid)
 if (iret /= nf90_noerr) goto 7100
 iret = nf90_put_var(ncid, varid, dummy2d)
 if (iret /= nf90_noerr) goto 7100

 print*,"- UPDATE SNOW."
 dummy1d = data_output%snow_liq_equiv
 dummy2d = reshape(dummy1d, (/imdl_output,jmdl_output/) )
 iret = nf90_inq_varid(ncid, 'SNOW', varid)
 if (iret /= nf90_noerr) goto 7100
 iret = nf90_put_var(ncid, varid, dummy2d)
 if (iret /= nf90_noerr) goto 7100

 print*,"- UPDATE TOTAL SOIL MOISTURE"
 allocate(dummy3d(imdl_output,jmdl_output,nsoil_output))
 do n = 1, nsoil_output
   dummy1d = data_output%soilm_tot(:,n)
   dummy2d = reshape(dummy1d, (/imdl_output,jmdl_output/) )
   dummy3d(:,:,n) = dummy2d
 enddo
 iret = nf90_inq_varid(ncid, 'SMOIS', varid)
 if (iret /= nf90_noerr) goto 7100
 iret = nf90_put_var(ncid, varid, dummy3d)
 if (iret /= nf90_noerr) goto 7100

! set to zero over land/ice and one over water.  
! arw must initialize this field.

 dummy1d = 1.0
 where (data_output%sea_ice_flag == 1) dummy1d=0.0
 where (data_output%lsmask == 1.0) dummy1d=0.0
 dummy2d = reshape(dummy1d, (/imdl_output,jmdl_output/) )
 print*,"- UPDATE LIQUID SOIL MOISTURE"
 do n = 1, nsoil_output
   dummy3d(:,:,n) = dummy2d
 enddo
 iret = nf90_inq_varid(ncid, 'SH2O', varid)
 if (iret /= nf90_noerr) goto 7100
 iret = nf90_put_var(ncid, varid, dummy3d)
 if (iret /= nf90_noerr) goto 7100

! at open water points, the sst is used as a flag
! value.

 print*,"- UPDATE SOIL TEMPERATURE."
 do n = 1, nsoil_output
   dummy1d = data_output%soil_temp(:,n)
   do ij = 1, ijmdl_output
     if (data_output%sea_ice_flag(ij) == 0 .and.  &
         data_output%lsmask(ij) == 0.0 ) then
       dummy1d(ij) = data_output%skin_temp(ij)
     endif
   enddo
   dummy2d = reshape(dummy1d, (/imdl_output,jmdl_output/) )
   dummy3d(:,:,n) = dummy2d
 enddo
 iret = nf90_inq_varid(ncid, 'TSLB', varid)
 if (iret /= nf90_noerr) goto 7100
 iret = nf90_put_var(ncid, varid, dummy3d)
 if (iret /= nf90_noerr) goto 7100

 print*,"- CLOSE FILE."
 iret = nf90_close(ncid)

 deallocate (dummy1d, dummy2d, dummy3d)

 return

 7100 print*,"- ERROR WRITING ARW NETCDF FILE, IRET IS: ", iret
 errmsg = nf90_strerror(iret)
 print*,'- ERROR IS: ',trim(errmsg)
 call w3tage('COLDSTART_WRF')
 call mpi_abort(mpi_comm_world, 36, iret)

 end subroutine write_output_data_ARWnetcdf

 subroutine write_output_data_NMMnetcdf

!-----------------------------------------------------------------------
! update surface records for an nmm netcdf file.
!-----------------------------------------------------------------------

 use netcdf

 implicit none

 character*80      :: errmsg

 integer           :: iret, mode, n, ncid, varid

 real, allocatable :: dummy2d(:,:), dummy1d(:), dummy3d(:,:,:)
 real, allocatable :: theta(:,:), sst(:,:), skint(:,:)

 include 'mpif.h'
 include 'netcdf.inc'

 print*,"- UPDATE SURFACE RECORDS IN NMM NETCDF FILE"

 print*,'- OPEN FILE: ', trim(output_file)
 mode=1
 iret = nf90_open(output_file, mode, ncid)
 if (iret /= nf90_noerr) goto 8100
 
 print*,"- GET SST"
 allocate(sst(imdl_output,jmdl_output))
 iret = nf90_inq_varid(ncid, 'SST', varid)
 iret = nf90_get_var(ncid, varid, sst)
 if (iret /= nf90_noerr) goto 8100

 print*,"- GET TSK"
 allocate(skint(imdl_output,jmdl_output))
 iret = nf90_inq_varid(ncid, 'TSK', varid)
 iret = nf90_get_var(ncid, varid, skint)
 if (iret /= nf90_noerr) goto 8100

 where (skint < 0.1) skint=sst

 print*,"- GET THS"
 allocate(theta(imdl_output,jmdl_output))
 iret = nf90_inq_varid(ncid, 'THS', varid)
 iret = nf90_get_var(ncid, varid, theta)
 if (iret /= nf90_noerr) goto 8100

 print*,"- COMPUTE RATIO OF THETA TO SKIN T"
 theta = theta /skint

 allocate (dummy2d(imdl_output,jmdl_output))
 allocate (dummy1d(ijmdl_output))

 print*,"- UPDATE LAND MASK."
 dummy1d = 1.0  ! water

 where (data_output%lsmask > 0.0)      dummy1d = 0.0  ! land
 where (data_output%sea_ice_flag == 1) dummy1d = 0.0  ! ice

 dummy2d = reshape(dummy1d, (/imdl_output,jmdl_output/) )
 iret = nf90_inq_varid(ncid, 'SM', varid)
 iret = nf90_put_var(ncid, varid, dummy2d)
 if (iret /= nf90_noerr) goto 8100

 print*,"- UPDATE ICE MASK."
 dummy1d = float(data_output%sea_ice_flag)
 dummy2d = reshape(dummy1d, (/imdl_output,jmdl_output/) )
 iret = nf90_inq_varid(ncid, 'SICE', varid)
 iret = nf90_put_var(ncid, varid, dummy2d)
 if (iret /= nf90_noerr) goto 8100

! sst field is non-zero only at open water
 print*,"- UPDATE SST."
 dummy1d=0.0
 do n = 1, ijmdl_output
   if (data_output%lsmask(n)==0.0 .and. data_output%sea_ice_flag(n)==0) then
     dummy1d(n)= data_output%skin_temp(n)
   endif
 enddo
 dummy2d = reshape(dummy1d, (/imdl_output,jmdl_output/) )
 iret = nf90_inq_varid(ncid, 'SST', varid)
 iret = nf90_put_var(ncid, varid, dummy2d)
 if (iret /= nf90_noerr) goto 8100

! skin t field is non-zero only at land and ice
 print*,"- UPDATE TSK."
 dummy1d = 0.0
 do n = 1, ijmdl_output
   if (data_output%lsmask(n)>0.0 .or. data_output%sea_ice_flag(n)==1) then
     dummy1d(n)= data_output%skin_temp(n)
   endif
 enddo
 dummy2d = reshape(dummy1d, (/imdl_output,jmdl_output/) )
 iret = nf90_inq_varid(ncid, 'TSK', varid)
 iret = nf90_put_var(ncid, varid, dummy2d)
 if (iret /= nf90_noerr) goto 8100

! skin potential temperature.  i could not find a surface pressure
! in the netcdf file, so i compute the ratio of input skin potential to
! the input skin temperature (stored in variable theta)

 print*,"- UPDATE THS."
 dummy1d = data_output%skin_temp
 dummy2d = reshape(dummy1d, (/imdl_output,jmdl_output/) )
 dummy2d = dummy2d * theta
 iret = nf90_inq_varid(ncid, 'THS', varid)
 iret = nf90_put_var(ncid, varid, dummy2d)
 if (iret /= nf90_noerr) goto 8100
 deallocate (skint, sst, theta)

 print*,"- UPDATE VEGETATION TYPE."
 dummy1d = float(data_output%veg_type)
 do n = 1, ijmdl_output
   if (data_output%lsmask(n)==0.0)then
     if (data_output%sea_ice_flag(n)==1) then
       dummy1d(n)= 24.0   ! sea ice value
     else
       dummy1d(n)= 16.0   ! open water value
     endif
   endif
 enddo
 dummy2d = reshape(dummy1d, (/imdl_output,jmdl_output/) )
 iret = nf90_inq_varid(ncid, 'IVGTYP', varid)
 iret = nf90_put_var(ncid, varid, dummy2d)
 if (iret /= nf90_noerr) goto 8100

 print*,"- UPDATE SOIL TYPE."
 dummy1d = float(data_output%soil_type)
 do n = 1, ijmdl_output
   if (data_output%lsmask(n)==0.0)then
     if (data_output%sea_ice_flag(n)==1) then
       dummy1d(n)= 16.0   ! sea ice value
     else
       dummy1d(n)= 14.0   ! open water value
     endif
   endif
 enddo
 dummy2d = reshape(dummy1d, (/imdl_output,jmdl_output/) )
 iret = nf90_inq_varid(ncid, 'ISLTYP', varid)
 iret = nf90_put_var(ncid, varid, dummy2d)
 if (iret /= nf90_noerr) goto 8100

 print*,"- UPDATE GREENNESS."
 dummy1d = data_output%greenfrc
 dummy2d = reshape(dummy1d, (/imdl_output,jmdl_output/) )
 iret = nf90_inq_varid(ncid, 'VEGFRC', varid)
 iret = nf90_put_var(ncid, varid, dummy2d)
 if (iret /= nf90_noerr) goto 8100

 print*,"- UPDATE DYNAMIC ALBEDO."
 dummy1d = data_output%albedo
 dummy2d = reshape(dummy1d, (/imdl_output,jmdl_output/) )
 iret = nf90_inq_varid(ncid, 'ALBEDO', varid)
 iret = nf90_put_var(ncid, varid, dummy2d)
 if (iret /= nf90_noerr) goto 8100

 print*,"- UPDATE MAX SNOW ALBEDO."
 dummy1d = data_output%mxsnow_alb
 dummy2d = reshape(dummy1d, (/imdl_output,jmdl_output/) )
 iret = nf90_inq_varid(ncid, 'MXSNAL', varid)
 iret = nf90_put_var(ncid, varid, dummy2d)
 if (iret /= nf90_noerr) goto 8100

 print*,"- UPDATE SNOW FREE ALBEDO."
 dummy1d = data_output%snow_free_albedo
 dummy2d = reshape(dummy1d, (/imdl_output,jmdl_output/) )
 iret = nf90_inq_varid(ncid, 'ALBASE', varid)
 iret = nf90_put_var(ncid, varid, dummy2d)
 if (iret /= nf90_noerr) goto 8100

 print*,"- UPDATE ROUGHNESS."
 dummy1d = data_output%z0 * 0.01  ! convert to meters
 dummy2d = reshape(dummy1d, (/imdl_output,jmdl_output/) )
 iret = nf90_inq_varid(ncid, 'Z0', varid)
 iret = nf90_put_var(ncid, varid, dummy2d)
 if (iret /= nf90_noerr) goto 8100

! not sure the units of cmc in sref.  since this
! field is not critical, set to zero.
 print*,"- UPDATE CANOPY MOISTURE CONTENT."
!dummy1d = data_output%canopy_mc  ! this is in meters
 dummy1d = 0.0
 dummy2d = reshape(dummy1d, (/imdl_output,jmdl_output/) )
 iret = nf90_inq_varid(ncid, 'CMC', varid)
 iret = nf90_put_var(ncid, varid, dummy2d)
 if (iret /= nf90_noerr) goto 8100

 print*,"- UPDATE SUBSTRATE TEMP."
 dummy1d = data_output%substrate_temp
 dummy2d = reshape(dummy1d, (/imdl_output,jmdl_output/) )
 iret = nf90_inq_varid(ncid, 'TGROUND', varid)
 iret = nf90_put_var(ncid, varid, dummy2d)
 if (iret /= nf90_noerr) goto 8100

 print*,"- UPDATE LIQ EQUIVALENT SNOW."
 dummy1d = data_output%snow_liq_equiv
 dummy2d = reshape(dummy1d, (/imdl_output,jmdl_output/) )
 iret = nf90_inq_varid(ncid, 'SNO', varid)
 iret = nf90_put_var(ncid, varid, dummy2d)
 if (iret /= nf90_noerr) goto 8100

 print*,"- UPDATE PHYSICAL SNOW DEPTH"
 dummy1d = data_output%snow_depth
 dummy2d = reshape(dummy1d, (/imdl_output,jmdl_output/) )
 iret = nf90_inq_varid(ncid, 'SI', varid)
 iret = nf90_put_var(ncid, varid, dummy2d)
 if (iret /= nf90_noerr) goto 8100

 print*,"- UPDATE TOTAL SOIL MOISTURE"
 allocate(dummy3d(imdl_output,jmdl_output,nsoil_output))
 do n = 1, nsoil_output
   dummy1d = data_output%soilm_tot(:,n)
   dummy2d = reshape(dummy1d, (/imdl_output,jmdl_output/) )
   dummy3d(:,:,n) = dummy2d
 enddo
 iret = nf90_inq_varid(ncid, 'SMC', varid)
 iret = nf90_put_var(ncid, varid, dummy3d)
 if (iret /= nf90_noerr) goto 8100

 print*,"- UPDATE LIQUID SOIL MOISTURE"
 do n = 1, nsoil_output
   dummy1d = data_output%soilm_liq(:,n)
   dummy2d = reshape(dummy1d, (/imdl_output,jmdl_output/) )
   dummy3d(:,:,n) = dummy2d
 enddo
 iret = nf90_inq_varid(ncid, 'SH2O', varid)
 iret = nf90_put_var(ncid, varid, dummy3d)
 if (iret /= nf90_noerr) goto 8100

 print*,"- UPDATE SOIL TEMPERATURE"
 do n = 1, nsoil_output
   dummy1d = data_output%soil_temp(:,n)
   dummy2d = reshape(dummy1d, (/imdl_output,jmdl_output/) )
   dummy3d(:,:,n) = dummy2d
 enddo
 iret = nf90_inq_varid(ncid, 'STC', varid)
 iret = nf90_put_var(ncid, varid, dummy3d)
 if (iret /= nf90_noerr) goto 8100

 print*,"- CLOSE FILE."
 iret = nf90_close(ncid)

 deallocate (dummy2d, dummy1d, dummy3d)

 return

 8100 print*,"- ERROR WRITING NMM NETCDF FILE, IRET IS: ", iret
 errmsg = nf90_strerror(iret)
 print*,'- ERROR IS: ',trim(errmsg)
 call w3tage('COLDSTART_WRF')
 call mpi_abort(mpi_comm_world, 36, iret)

 end subroutine write_output_data_NMMnetcdf

!-----------------------------------------------------------------------
! update surface records for nems style file.
!-----------------------------------------------------------------------

 subroutine write_output_data_nems

 use nemsio_module

 implicit none

 include 'mpif.h'

 character*255                      :: gfname
 character(nemsio_charkind8)        :: gaction
 character*20                       :: vlevtyp, vname

 integer(nemsio_intkind)            :: ij, iret, kmdl
 integer(nemsio_intkind)            :: vlev

 real, allocatable                  :: dummy(:), dummy2(:,:)

 type(nemsio_gfile)                 :: gfile

 print*,"- UPDATE SURFACE RECORDS IN NEMS FILE"

 print*,'- INITIALIZE NEMSIO MODULE.'
 call nemsio_init(iret=iret)
 if (iret /= 0) then
   print*,'bad init ',iret
   stop
 endif

 gfname=output_file
 print*,'- OPEN FILE FOR WRITING: ', trim(gfname)
 gaction="RDWR"
 call nemsio_open(gfile,gfname,gaction,iret=iret)
 if (iret /= 0) then
   print*,'bad iret of open ',iret
   stop
 endif

 print*,"- UPDATE VEGETATION TYPE."
 allocate(dummy(ijmdl_output))
 vname='vgtyp'
 vlevtyp='sfc'
 vlev=1
 dummy=float(data_output%veg_type)
 call nemsio_writerecv(gfile,vname,vlevtyp,vlev,dummy,iret=iret)
 if (iret /= 0) goto 9000

 print*,"- UPDATE SOIL TYPE."
 vname='sltyp'
 vlevtyp='sfc'
 vlev=1
 dummy=float(data_output%soil_type)
 call nemsio_writerecv(gfile,vname,vlevtyp,vlev,dummy,iret=iret)
 if (iret /= 0) goto 9000

 print*,"- UPDATE LAND MASK."
 dummy = 1.0  ! water

 where (data_output%lsmask > 0.0)      dummy = 0.0  ! land
 where (data_output%sea_ice_flag == 1) dummy = 0.0  ! ice

 vname='sm'
 vlevtyp='sfc'
 vlev=1
 call nemsio_writerecv(gfile,vname,vlevtyp,vlev,dummy,iret=iret)
 if (iret /= 0) goto 9000

 print*,"- UPDATE ICE MASK."
 vname='sice'
 vlevtyp='sfc'
 vlev=1
 dummy=float(data_output%sea_ice_flag)
 call nemsio_writerecv(gfile,vname,vlevtyp,vlev,dummy,iret=iret)
 if (iret /= 0) goto 9000

 print*,"- UPDATE SKIN TEMPERATURE."
 vname='tskin'
 vlevtyp='sfc'
 vlev=1
 call nemsio_writerecv(gfile,vname,vlevtyp,vlev,data_output%skin_temp,iret=iret)
 if (iret /= 0) goto 9000

! sst field is non-zero only at open water
 dummy=0.0
 do ij = 1, ijmdl_output
   if (data_output%lsmask(ij)==0.0 .and. data_output%sea_ice_flag(ij)==0) then
     dummy(ij)= data_output%skin_temp(ij)
   endif
 enddo

 print*,"- UPDATE SST"
 vname='tsea'
 vlevtyp='sfc'
 vlev=1
 call nemsio_writerecv(gfile,vname,vlevtyp,vlev,dummy,iret=iret)
 if (iret /= 0) goto 9000

! need to read surface press in order to convert skin temp to pot temp
 print*,'- READ NUMBER OF SIGMA LEVELS.'
 call nemsio_getfilehead(gfile,iret,dimz=kmdl)
 if (iret /= 0) goto 9000

 print*,'- READ SURFACE PRESSURE.'
 vname='pres'
 vlevtyp='layer'
 vlev=kmdl
 call nemsio_readrecv(gfile,vname,vlevtyp,vlev,dummy,iret=iret)
 if (iret /= 0) goto 9000

 print*,'- UPDATE SKIN POTENTIAL TEMPERATURE.'
 data_output%skin_temp=data_output%skin_temp*(100000./dummy)**rovcp
 deallocate(dummy)
 vname='ths'
 vlevtyp='sfc'
 vlev=1
 call nemsio_writerecv(gfile,vname,vlevtyp,vlev,data_output%skin_temp,iret=iret)
 if (iret /= 0) goto 9000

 print*,"- UPDATE GREENNESS FRACTION."
 vname='vegfrc'
 vlevtyp='sfc'
 vlev=1
 call nemsio_writerecv(gfile,vname,vlevtyp,vlev,data_output%greenfrc,iret=iret)
 if (iret /= 0) goto 9000

 print*,"- UPDATE ALBEDO."
 vname='albedo'
 vlevtyp='sfc'
 vlev=1
 call nemsio_writerecv(gfile,vname,vlevtyp,vlev,data_output%albedo,iret=iret)
 if (iret /= 0) goto 9000

 print*,"- UPDATE SNOW FREE ALBEDO."
 vname='albase'
 vlevtyp='sfc'
 vlev=1
 call nemsio_writerecv(gfile,vname,vlevtyp,vlev,data_output%snow_free_albedo,iret=iret)
 if (iret /= 0) goto 9000

 print*,"- UPDATE MAXIMUM SNOW ALBEDO."
 vname='mxsnal'
 vlevtyp='sfc'
 vlev=1
 call nemsio_writerecv(gfile,vname,vlevtyp,vlev,data_output%mxsnow_alb,iret=iret)
 if (iret /= 0) goto 9000

 print*,"- UPDATE SUBSTRATE TEMPERATURE."
 vname='tg'
 vlevtyp='sfc'
 vlev=1
 call nemsio_writerecv(gfile,vname,vlevtyp,vlev,data_output%substrate_temp,iret=iret)
 if (iret /= 0) goto 9000

 print*,"- UPDATE ROUGHNESS LENGTH."
 vname='zorl'
 vlevtyp='sfc'
 vlev=1
! convert to meters
 data_output%z0=data_output%z0*0.01
 call nemsio_writerecv(gfile,vname,vlevtyp,vlev,data_output%z0,iret=iret)
 if (iret /= 0) goto 9000

 print*,"- UPDATE CANOPY MOISTURE CONTENT."
 vname='cmc'
 vlevtyp='sfc'
 vlev=1
 call nemsio_writerecv(gfile,vname,vlevtyp,vlev,data_output%canopy_mc,iret=iret)
 if (iret /= 0) goto 9000

 print*,"- UPDATE SNOW DEPTH."
 vname='si'
 vlevtyp='sfc'
 vlev=1
 call nemsio_writerecv(gfile,vname,vlevtyp,vlev,data_output%snow_depth,iret=iret)
 if (iret /= 0) goto 9000

 print*,"- UPDATE SNOW LIQUID EQUIVALENT."
 vname='sno'
 vlevtyp='sfc'
 vlev=1
 call nemsio_writerecv(gfile,vname,vlevtyp,vlev,data_output%snow_liq_equiv,iret=iret)
 if (iret /= 0) goto 9000

 vname='stc'
 vlevtyp='soil layer'
 do vlev = 1, nsoil_output
   print*,"- UPDATE SOIL TEMPERATURE FOR LAYER: ",vlev
   call nemsio_writerecv(gfile,vname,vlevtyp,vlev,data_output%soil_temp(:,vlev),iret=iret)
   if (iret /= 0) goto 9000
 enddo

 vname='smc'
 vlevtyp='soil layer'
 do vlev = 1, nsoil_output
   print*,"- UPDATE TOTAL SOIL MOISTURE FOR LAYER: ",vlev
   call nemsio_writerecv(gfile,vname,vlevtyp,vlev,data_output%soilm_tot(:,vlev),iret=iret)
   if (iret /= 0) goto 9000
 enddo

 vname='sh2o'
 vlevtyp='soil layer'
 do vlev = 1, nsoil_output
   print*,"- UPDATE LIQUID SOIL MOISTURE FOR LAYER: ",vlev
   call nemsio_writerecv(gfile,vname,vlevtyp,vlev,data_output%soilm_liq(:,vlev),iret=iret)
   if (iret /= 0) goto 9000
 enddo

 print*,'- CLOSE FILE.'
 call nemsio_close(gfile,iret=iret)
 if (iret /= 0) then
   print*,'iret of close ',iret
   stop
 endif

 return

 9000 print*,"- ERROR WRITING NEMS FILE, ISTAT IS: ", iret
 call w3tage('COLDSTART_WRF')
 call mpi_abort(mpi_comm_world, 34, iret)

 end subroutine write_output_data_nems

 end module write_data
