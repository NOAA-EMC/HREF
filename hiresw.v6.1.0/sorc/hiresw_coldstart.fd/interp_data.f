 module interp_data

 use surface_chgres, only  : surface_chgres_driver, &
                             sfc2d,   &
                             sfc1d,   &
                             surface_chgres_ax2d

 implicit none

!-----------------------------------------------------------------------
! data on the output grid after interpolation.
!-----------------------------------------------------------------------

 type(sfc1d)                   :: data_output

 integer, parameter            :: nsoil_output=4

 contains

!-----------------------------------------------------------------------
! interpolate data from input to output grids.
!-----------------------------------------------------------------------

 subroutine interp

 use read_data, only         : curr_day, &
                               curr_hour, &
                               curr_mon, &
                               curr_year, &
                               data_input,  &
                               imdl_input,  &
                               jmdl_input,  &
                               nsoil_input,  &
                               kgds_input

 use program_setup, only     : lsmask_output,         &
                               orog_output,           &
                               substrate_temp_output, &
                               lats_output,           &
                               lons_output,           &
                               imdl_output,           &
                               ijmdl_output,          &
                               jmdl_output,           &
                               kgds_output, &
                               merge,       &
                               output_file_type, &
                               program_setup_cleanup
 
 implicit none

 include 'mpif.h' 

 integer                        :: ij
 integer                        :: iret
 integer                        :: lonsperlat_output((jmdl_output+1)/2)

 real                           :: fcst_hour

! output arrays are 1-d because gfs works that way.

 allocate(data_output%lats(ijmdl_output))
 data_output%lats = reshape(lats_output,(/ijmdl_output/))

 allocate(data_output%lons(ijmdl_output))
 data_output%lons = reshape(lons_output,(/ijmdl_output/))

 allocate(data_output%lsmask(ijmdl_output))
 data_output%lsmask = reshape(lsmask_output,(/ijmdl_output/))

 allocate(data_output%substrate_temp(ijmdl_output))
 data_output%substrate_temp = reshape(substrate_temp_output,(/ijmdl_output/))

 allocate(data_output%orog(ijmdl_output))
 data_output%orog = reshape(orog_output,(/ijmdl_output/))

 call program_setup_cleanup  ! free up memory

 fcst_hour = 0.0        ! only used for gfs.
 lonsperlat_output = 0  ! only used for thinning gfs grids.

 allocate(data_output%veg_type(ijmdl_output))
 allocate(data_output%sea_ice_flag(ijmdl_output))
 allocate(data_output%soil_type(ijmdl_output))
 allocate(data_output%canopy_mc(ijmdl_output))
 allocate(data_output%soilm_tot(ijmdl_output,nsoil_output))
 allocate(data_output%soilm_liq(ijmdl_output,nsoil_output))
 allocate(data_output%soil_temp(ijmdl_output,nsoil_output))
 allocate(data_output%skin_temp(ijmdl_output))
 allocate(data_output%snow_liq_equiv(ijmdl_output))
 allocate(data_output%snow_depth(ijmdl_output))
 allocate(data_output%z0(ijmdl_output))
 allocate(data_output%greenfrc(ijmdl_output))
! starting in the nems era, slope type is not used.
 if (.not. allocated(data_input%slope_type)) goto 88
   allocate(data_output%slope_type(ijmdl_output))
 88 continue
 allocate(data_output%mxsnow_alb(ijmdl_output))
 allocate(data_output%snow_free_albedo(ijmdl_output))
 allocate(data_output%albedo(ijmdl_output))

!-------------------------------------------------------------------------------
! running merged means that land states will be interpolated from two different
! sources. for ex, for a regional domain that extends beyond the ops nam domain,
! the outer row/columns will need to initialized with gfs data, while the
! interior will be initialized with ops nam data.
!
! this merging is accomplished by running this program twice.  first, run
! it to interpolate gfs land states to the output grid.  then run the
! program again to interpolate nam land states to the output grid.  when
! running it the second time, the merge flag is set to true.  this reads
! the file with the interpolated gfs land states and places the land
! records in the data_output construct.  when the surface_chgres_driver
! is called, points in the interior of the grid will be overwritten.
!-------------------------------------------------------------------------------

 if (merge) then
   if (trim(output_file_type) == "NEMS" .or. trim(output_file_type) == "nems")then
     call get_fg
   else
     print*,'- MERGE OPTION MUST BE USED WITH NEMSIO FILE.'
     call w3tage('COLDSTART_WRF')
     call mpi_abort(mpi_comm_world, 35, iret)
   endif
 end if

 call surface_chgres_driver(imdl_output, jmdl_output,     &
                            ijmdl_output, nsoil_output,          &
                            lonsperlat_output,            &
                            kgds_output, data_output,     &
                            imdl_input, jmdl_input,       &
                            nsoil_input, curr_hour, curr_mon, &
                            curr_day, curr_year, fcst_hour,         &
                            kgds_input, data_input, merge, iret)

 if (iret /=0) then
   print*,'- ERROR IN SURFACE CHGRES DRIVER, IRET IS: ', iret
   call w3tage('COLDSTART_WRF')
   call mpi_abort(mpi_comm_world, 83, iret)
 endif

! surface_chgres module does not deal with soil/veg type over water or sea ice
! because the global and regional models have different conventions.
! for wrf (both cores), set the values according to the appropriate
! usgs vegetation and statsgo soil categories.

 do ij = 1, ijmdl_output
   if (data_output%lsmask(ij) == 0.0) then        ! non-land
     if (data_output%sea_ice_flag(ij) == 1) then  ! sea ice
       data_output%veg_type(ij) = 0
       data_output%soil_type(ij) = 0
     else                                         ! open water
       data_output%veg_type(ij) = 0
       data_output%soil_type(ij) = 0
     endif
   endif
 enddo

 call surface_chgres_ax2d(data_input)  ! free up memory
 
 return

 end subroutine interp

!----------------------------------------------------------------------
! read nemsio file with interpolated gfs land states and place 
! each record in the proper data_output array.
!
! note: this routine only reads 'state' fields.  climo fields
! such as soil type, veg type, greenness, base albedo, max
! snow albedo must be read in from external files by the
! surface_chgres_driver module.  
!----------------------------------------------------------------------

 subroutine get_fg

 use nemsio_module
 use program_setup, only  : output_file, ijmdl_output

 implicit none

 include 'mpif.h'

 character*255                     :: gfname
 character(nemsio_charkind8)       :: gaction
 character*20                      :: vlevtyp, vname

 integer(nemsio_intkind)           :: iret, vlev, kmdl

 real, allocatable                 :: dummy(:), dummy2(:)

 type(nemsio_gfile)                :: gfile

 print*,'- INITIALIZE NEMSIO MODULE.'
 call nemsio_init(iret=iret)
 if (iret /= 0) goto 9000

 gfname=output_file
 print*,'- OPEN FILE FOR READING: ', trim(gfname)
 gaction="READ"
 call nemsio_open(gfile,gfname,gaction,iret=iret)
 if (iret /= 0) goto 9000

 print*,"- READ SEA ICE FLAG."
 vname='sice'
 vlevtyp='sfc'
 vlev=1
 allocate(dummy(ijmdl_output))
 call nemsio_readrecv(gfile,vname,vlevtyp,vlev,dummy,iret=iret)
 if (iret /= 0) goto 9000
 data_output%sea_ice_flag=nint(dummy)

 print*,"- READ TOTAL SOIL MOIST."
 vname='smc'
 vlevtyp='soil layer'
 do vlev=1,nsoil_output
   call nemsio_readrecv(gfile,vname,vlevtyp,vlev,dummy,iret=iret)
   if (iret /= 0) goto 9000
   data_output%soilm_tot(:,vlev)=dummy
 enddo

 print*,"- READ LIQUID SOIL MOIST."
 vname='sh2o'
 vlevtyp='soil layer'
 do vlev=1,nsoil_output
   call nemsio_readrecv(gfile,vname,vlevtyp,vlev,dummy,iret=iret)
   if (iret /= 0) goto 9000
   data_output%soilm_liq(:,vlev)=dummy
 enddo

 print*,"- READ SOIL TEMPERATURE"
 vname='stc'
 vlevtyp='soil layer'
 do vlev=1,nsoil_output
   call nemsio_readrecv(gfile,vname,vlevtyp,vlev,dummy,iret=iret)
   if (iret /= 0) goto 9000
   data_output%soil_temp(:,vlev)=dummy
 enddo

 print*,'- READ NUMBER OF SIGMA LEVELS.'
 call nemsio_getfilehead(gfile,iret,dimz=kmdl)
 if (iret /= 0) goto 9000

 print*,"- READ SURFACE PRESSURE"
 vname='pres'
 vlevtyp='layer'
 vlev=kmdl
 allocate(dummy2(ijmdl_output))
 call nemsio_readrecv(gfile,vname,vlevtyp,vlev,dummy2,iret=iret)
 if (iret /= 0) goto 9000

 print*,"- READ SKIN TEMPERATURE"
 vname='ths'
 vlevtyp='sfc'
 vlev=1
 call nemsio_readrecv(gfile,vname,vlevtyp,vlev,dummy,iret=iret)
 if (iret /= 0) goto 9000
 data_output%skin_temp=dummy/((100000./dummy2)**.286)

 print*,"- READ CANOPY MOIST CONTENT"
 vname='cmc'
 vlevtyp='sfc'
 vlev=1
 call nemsio_readrecv(gfile,vname,vlevtyp,vlev,dummy,iret=iret)
 if (iret /= 0) goto 9000
 data_output%canopy_mc=dummy

 print*,"- READ SNOW DEPTH"
 vname='si'
 vlevtyp='sfc'
 vlev=1
 call nemsio_readrecv(gfile,vname,vlevtyp,vlev,dummy,iret=iret)
 if (iret /= 0) goto 9000
 data_output%snow_depth=dummy

 print*,"- READ SNOW LIQ EQUIV"
 vname='sno'
 vlevtyp='sfc'
 vlev=1
 call nemsio_readrecv(gfile,vname,vlevtyp,vlev,dummy,iret=iret)
 if (iret /= 0) goto 9000
 data_output%snow_liq_equiv=dummy

! over land this is veg based, but over water will contain gfs
! value.
 print*,"- READ ROUGHNESS"
 vname='zorl'
 vlevtyp='sfc'
 vlev=1
 call nemsio_readrecv(gfile,vname,vlevtyp,vlev,dummy,iret=iret)
 if (iret /= 0) goto 9000
 data_output%z0=dummy*100.0  ! interp code expects cm

 deallocate(dummy2,dummy)

 print*,'- CLOSE FILE.'
 call nemsio_close(gfile,iret=iret)
 if (iret /= 0) then
   print*,'CLOSE FAILED, IRET IS:',iret
 endif

 return

 9000 print*,"- ERROR READING NEMS FILE, ISTAT IS: ", iret
 call w3tage('COLDSTART_WRF')
 call mpi_abort(mpi_comm_world, 35, iret)

 end subroutine get_fg

 end module interp_data
