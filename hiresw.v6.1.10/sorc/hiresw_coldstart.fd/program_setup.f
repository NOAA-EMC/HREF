 module program_setup

 character*150               :: input_file

!-----------------------------------------------------------------------
! input file types are:
!   edas  - old style edas file from the pre-wrf days
!   nems  - nems file for nmm e-grid or b-grid
!-----------------------------------------------------------------------

 character*6                 :: input_file_type 
 character*150, private      :: lats_output_file
 character*150, private      :: lons_output_file
 character*150, private      :: lsmask_output_file
 character*150, private      :: orog_output_file
 character*150               :: output_file

!-----------------------------------------------------------------------
! output file types are:
!   arwwrf   - wrf netcdf file, arw core
!   nmmwrf   - wrf netcdf file, nmm e-grid
!   nems     - nems binary file, nmm b-grid
!-----------------------------------------------------------------------

 character*9                 :: output_file_type  
 character*150, private      :: substrate_temp_output_file

 integer                     :: i,j
 integer                     :: imdl_output
 integer                     :: ijmdl_output
 integer                     :: jmdl_output
 integer                     :: kgds_output(200)

 logical                     :: merge
 logical, private            :: specs_from_output_file

 real, allocatable           :: lats_output(:,:)
 real, allocatable           :: lons_output(:,:)
 real, allocatable           :: lsmask_output(:,:)
 real, allocatable           :: orog_output(:,:)
 real, allocatable           :: orog_stnd_output(:,:)
 real, allocatable           :: substrate_temp_output(:,:)
 real, allocatable           :: dum1(:,:)
 real, allocatable           :: dum2(:,:)

!cgwd
 real, allocatable :: oa_output(:,:,:)
 real, allocatable :: ol_output(:,:,:)
 real, allocatable :: oc_output(:,:)
 real, allocatable :: theta_output(:,:)
 real, allocatable :: gamma_output(:,:)
 real, allocatable :: sigma_output(:,:)
 real, allocatable :: elvmax_output(:,:)

 contains

!-----------------------------------------------------------------------
! get the grid specifications for the output (or target grid):
! land mask, lats/lons, etc.
!-----------------------------------------------------------------------

 subroutine read_output_grid_specs

 implicit none

 include 'mpif.h'

 integer                        :: iret

 if (specs_from_output_file) then
   print*,"- USE OUTPUT GRID SPECS FROM BINARY FILE."
   select case(trim(output_file_type))
     case ("netcdf", "NETCDF", "NetCDF")
       call output_grid_specs_NMMnetcdf
     case ("arwnetcdf", "arwNETCDF", "arwNetCDF")
       call output_grid_specs_ARWnetcdf
     case ("nems", "NEMS")
       call output_grid_specs_nems
     case default
       print*,"INVALID CHOICE OF OUTPUT FILE TYPE: ", output_file_type
       call w3tage('COLDSTART_WRF')
       call mpi_abort(mpi_comm_world, 39, iret)
   end select
 else
   print*,"- GET OUTPUT GRID SPECS FROM GRIB FILES."
   call output_grid_specs_grib
 endif

 return
 
 end subroutine read_output_grid_specs

!-----------------------------------------------------------------------
! get output grid specs from NMMnetcdf file.
!-----------------------------------------------------------------------

 subroutine output_grid_specs_NMMnetcdf

 use netcdf

 implicit none

 include 'mpif.h'
 include 'netcdf.inc'

 character     :: gridtype

 integer       :: dimid, mode, ncid, iret
 integer       :: varid, attnum

 real          :: dlmd, dphd, truelat, stand_lon

 print*,'- OPEN FILE FOR READING: ', trim(output_file)
 mode=0
 iret = nf90_open(output_file, mode, ncid)
 if (iret /= 0) goto 8100

 iret = nf90_inq_dimid(ncid, 'west_east', dimid)
 if (iret /= 0) goto 8100
 iret = nf90_inquire_dimension(ncid, dimid, len=imdl_output)
 if (iret /= 0) goto 8100
 print*,'- I-DIM OF GRID ', imdl_output
 iret = nf90_inq_dimid(ncid, 'south_north', dimid)
 if (iret /= 0) goto 8100
 iret = nf90_inquire_dimension(ncid, dimid, len=jmdl_output)
 if (iret /= 0) goto 8100
 print*,'- J-DIM OF GRID ', jmdl_output

 ijmdl_output = imdl_output * jmdl_output

 iret = nf90_inq_varid(ncid, 'DLMD', varid)
 if (iret /= 0) goto 8100
 iret = nf90_get_var(ncid, varid, dlmd)
 if (iret /= 0) goto 8100
 print*,'- DLMD ', dlmd
 iret = nf90_inq_varid(ncid, 'DPHD', varid)
 if (iret /= 0) goto 8100
 iret = nf90_get_var(ncid, varid, dphd)
 if (iret /= 0) goto 8100
 print*,'- DPHD ', dphd

 iret = nf90_get_att(ncid, NF_GLOBAL, 'GRIDTYPE', gridtype)
 if (iret /= 0) goto 8100
 print*,'- GRID TYPE ',gridtype
 iret = nf90_get_att(ncid, NF_GLOBAL, 'TRUELAT1', truelat)
 if (iret /= 0) goto 8100
 print*,'- TRUELAT ',truelat
 iret = nf90_get_att(ncid, NF_GLOBAL, 'STAND_LON', stand_lon)
 if (iret /= 0) goto 8100
 print*,'- STANDARD LON ',stand_lon

 print*,'- READ LAT'
 allocate (lats_output(imdl_output,jmdl_output))
 iret = nf90_inq_varid(ncid, 'GLAT', varid)
 if (iret /= 0) goto 8100
 iret = nf90_get_var(ncid, varid, lats_output)
 if (iret /= 0) goto 8100
 lats_output = lats_output * 180.0 / 3.14159
 print*,'- CORNER POINT LAT ',lats_output(1,1)

 print*,'- READ LON'
 allocate (lons_output(imdl_output,jmdl_output))
 iret = nf90_inq_varid(ncid, 'GLON', varid)
 if (iret /= 0) goto 8100
 iret = nf90_get_var(ncid, varid, lons_output)
 if (iret /= 0) goto 8100
 lons_output = lons_output * 180.0 / 3.14159
 print*,'- CORNER POINT LON ',lons_output(1,1)

 print*,'- READ TERRAIN'
 allocate (orog_output(imdl_output,jmdl_output))
 iret = nf90_inq_varid(ncid, 'FIS', varid)
 if (iret /= 0) goto 8100
 iret = nf90_get_var(ncid, varid, orog_output)
 if (iret /= 0) goto 8100
 orog_output = orog_output / 9.806

 print*,'- READ SUBSTRATE TEMP'
 allocate (substrate_temp_output(imdl_output,jmdl_output))
 iret = nf90_inq_varid(ncid, 'TGROUND', varid)
 if (iret /= 0) goto 8100
 iret = nf90_get_var(ncid, varid, substrate_temp_output)
 if (iret /= 0) goto 8100

 print*,'- READ ICE MASK'
 allocate (dum1(imdl_output,jmdl_output))
 iret = nf90_inq_varid(ncid, 'SICE', varid)
 if (iret /= 0) goto 8100
 iret = nf90_get_var(ncid, varid, dum1)
 if (iret /= 0) goto 8100

 print*,'- READ LAND MASK'
 allocate (dum2(imdl_output,jmdl_output))
 iret = nf90_inq_varid(ncid, 'SM', varid)
 if (iret /= 0) goto 8100
 iret = nf90_get_var(ncid, varid, dum2)
 if (iret /= 0) goto 8100

! convert from nmm convention: 0-ice/land, 1-water
! to convention expected by this program: 0-nonland,1-land

 allocate(lsmask_output(imdl_output,jmdl_output))
 lsmask_output = 1.0                  ! land
 do j=1, jmdl_output
 do i=1, imdl_output
    if (dum1(i,j) == 1.0) lsmask_output(i,j) = 0.0  ! ice
    if (dum2(i,j) == 1.0) lsmask_output(i,j) = 0.0  ! open water
 enddo
 enddo

 print*,'- CLOSE FILE.'
 iret = nf90_close(ncid)
 if (iret /= 0) then
   print*,'** CLOSE FAILED, ISTAT IS: ',iret
 endif

! assumes nmm netcdf file is on the "e" grid.

 kgds_output=0
 kgds_output(1) = 203  ! e-grid
 kgds_output(2) = imdl_output
 kgds_output(3) = jmdl_output
 kgds_output(4) = nint(lats_output(1,1)*1000.)
 kgds_output(5) = nint(lons_output(1,1)*1000.)
 kgds_output(6) = 136        ! oct 17 - resolution and component flag
 kgds_output(7) = nint(truelat*1000.)
 kgds_output(8) = nint(stand_lon*1000.)
 kgds_output(9) = abs(nint(dlmd*1000.))
 kgds_output(10) = abs(nint(dphd*1000.))
 kgds_output(11) = 64         ! oct 28 - scanning mode flag
 kgds_output(19) = 0          ! oct 4  - # vert coordinate parameters
 kgds_output(20) = 255        ! oct 5  - not used, set to 255

 return

 8100 print*,"- ERROR READING NMM NETCDF FILE, STATUS IS: ", iret
 call w3tage('COLDSTART_WRF')
 call mpi_abort(mpi_comm_world, 43, iret)

 end subroutine output_grid_specs_NMMnetcdf

!-----------------------------------------------------------------------
! get output grid specs from ARWnetcdf file.
!-----------------------------------------------------------------------

 subroutine output_grid_specs_ARWnetcdf

 use netcdf

 implicit none

 include 'mpif.h'
 include 'netcdf.inc'

 integer    :: dimid, grid_id
 integer    :: iret, mode, ncid, varid

 real              :: dx, dy, stand_lon, truelat1, truelat2
 real, allocatable :: land_mask(:,:), ice_mask(:,:)

 print*,'- OPEN FILE FOR READING: ', trim(output_file)
 mode=0
 iret = nf90_open(output_file, mode, ncid)
 if (iret /= 0) goto 7100
 iret = nf90_inq_dimid(ncid, 'west_east', dimid)
 if (iret /= 0) goto 7100
 iret = nf90_inquire_dimension(ncid, dimid, len=imdl_output)
 if (iret /= 0) goto 7100
 print*,'- I-DIM OF GRID ', imdl_output
 iret = nf90_inq_dimid(ncid, 'south_north', dimid)
 if (iret /= 0) goto 7100
 iret = nf90_inquire_dimension(ncid, dimid, len=jmdl_output)
 if (iret /= 0) goto 7100
 print*,'- J-DIM OF GRID ', jmdl_output
 ijmdl_output = imdl_output * jmdl_output
 iret = nf90_get_att(ncid, NF_GLOBAL, 'DX', dx)
 if (iret /= 0) goto 7100
 print*,"- X RESOLUTION: ", dx
 iret = nf90_get_att(ncid, NF_GLOBAL, 'DY', dy)
 if (iret /= 0) goto 7100
 print*,"- Y RESOLUTION: ", dy
 iret = nf90_get_att(ncid, NF_GLOBAL, 'GRID_ID', grid_id)
 if (iret /= 0) goto 7100
 print*,"- GRID TYPE: ", grid_id
 iret = nf90_get_att(ncid, NF_GLOBAL, 'TRUELAT1', truelat1)
 if (iret /= 0) goto 7100
 print*,"- TRUELAT1: ", truelat1
 iret = nf90_get_att(ncid, NF_GLOBAL, 'TRUELAT2', truelat2)
 if (iret /= 0) goto 7100
 print*,"- TRUELAT2: ", truelat2
 iret = nf90_get_att(ncid, NF_GLOBAL, 'STAND_LON', stand_lon)
 if (iret /= 0) goto 7100
 print*,"- STANDARD LONGITUDE: ", stand_lon

 allocate(lats_output(imdl_output,jmdl_output))
 print*,'- READ LATITUDE'
 iret = nf90_inq_varid(ncid, 'XLAT', varid)
 if (iret /= 0) goto 7100
 iret = nf90_get_var(ncid, varid, lats_output)
 if (iret /= 0) goto 7100
 print*,'- CORNER POINT LATITUDE: ',lats_output(1,1)

 allocate(lons_output(imdl_output,jmdl_output))
 print*,'- READ LONGITUDE'
 iret = nf90_inq_varid(ncid, 'XLONG', varid)
 if (iret /= 0) goto 7100
 iret = nf90_get_var(ncid, varid, lons_output)
 if (iret /= 0) goto 7100
 print*,'- CORNER POINT LONGITUDE: ',lons_output(1,1)

 print*,'- READ TERRAIN'
 allocate (orog_output(imdl_output,jmdl_output))
 iret = nf90_inq_varid(ncid, 'HGT', varid)
 if (iret /= 0) goto 7100
 iret = nf90_get_var(ncid, varid, orog_output)
 if (iret /= 0) goto 7100
 orog_output = orog_output 

 print*,'- READ SUBSTRATE TEMP'
 allocate (substrate_temp_output(imdl_output,jmdl_output))
 iret = nf90_inq_varid(ncid, 'TMN', varid)
 if (iret /= 0) goto 7100
 iret = nf90_get_var(ncid, varid, substrate_temp_output)
 if (iret /= 0) goto 7100

 allocate(land_mask(imdl_output,jmdl_output))
 print*,'- READ LANDMASK'
 iret = nf90_inq_varid(ncid, 'LANDMASK', varid)
 if (iret /= 0) goto 7100
 iret = nf90_get_var(ncid, varid, land_mask)
 if (iret /= 0) goto 7100

 allocate(ice_mask(imdl_output,jmdl_output))
 print*,'- READ ICEMASK'
 iret = nf90_inq_varid(ncid, 'SEAICE', varid)
 if (iret /= 0) goto 7100
 iret = nf90_get_var(ncid, varid, ice_mask)
 if (iret /= 0) goto 7100

 print*,'- CLOSE FILE.'
 iret = nf90_close(ncid)
 if (iret /= 0) then
   print*,'** CLOSE FAILED, ISTAT IS: ',iret
 endif

 allocate(lsmask_output(imdl_output,jmdl_output))
! convert from arw convention: 1-ice/land, 0-water
! to convention expected by this program: 0-nonland,1-land

 lsmask_output = 0.0  ! open water and ice

 do j = 1, jmdl_output
 do i = 1, imdl_output
   if (land_mask(i,j) == 1.0 .and. ice_mask(i,j) == 0.0) then
     lsmask_output(i,j) = 1.0
   end if
 enddo
 enddo

 deallocate (ice_mask, land_mask)

 kgds_output=0
 if (grid_id == 1) then
   kgds_output(1) = 3
   kgds_output(2) = imdl_output
   kgds_output(3) = jmdl_output
   kgds_output(4) = nint(lats_output(1,1)*1000.)
   kgds_output(5) = nint(lons_output(1,1)*1000.)
   kgds_output(6) = 8
   kgds_output(7) = nint(stand_lon*1000.)
   kgds_output(8) = nint(dx)
   kgds_output(9) = nint(dy)
   kgds_output(10) = 0
   kgds_output(11) = 64
   kgds_output(12) = nint(truelat2*1000.)
   kgds_output(13) = nint(truelat1*1000.)
 else
   print*,'only works for lambert conformal'
   call mpi_abort(mpi_comm_world, 72, iret)
 endif 

 return

 7100 print*,"- ERROR READING ARW NETCDF FILE: ", trim(output_file)
 call w3tage('COLDSTART_WRF')
 call mpi_abort(mpi_comm_world, 72, iret)

 end subroutine output_grid_specs_ARWnetcdf

!-----------------------------------------------------------------------
! get output grid specs from nems file.
!-----------------------------------------------------------------------

 subroutine output_grid_specs_nems

 use nemsio_module

 implicit none

 include 'mpif.h'

 character*255                      :: gfname
 character(nemsio_charkind8)        :: gaction, modelname
 character*20                       :: vlevtyp, vname

 integer(nemsio_intkind)            :: idum3, idum4, iret
 integer(nemsio_intkind)            :: vlev

 real(nemsio_realkind)              :: dlmd, dphd, center_lat, center_lon
 real, allocatable                  :: dummy(:), dummy2(:), dummyice(:)

 type(nemsio_gfile)                 :: gfile

 print*,'- INITIALIZE NEMSIO MODULE.'
 call nemsio_init(iret=iret)
 if (iret /= 0) goto 9000

 gfname=output_file
 print*,'- OPEN FILE FOR READING: ', trim(gfname)
 gaction="READ"
 call nemsio_open(gfile,gfname,gaction,iret=iret)
 if (iret /= 0) goto 9100
 
 print*,"- READ FILE HEADER."
 call nemsio_getfilehead(gfile,iret,modelname=modelname,dimx=idum3,dimy=idum4)
 if (iret /= 0) goto 9200

 imdl_output=idum3
 jmdl_output=idum4
 ijmdl_output=imdl_output*jmdl_output

 print*,'- GET DX OF GRID.'
 call nemsio_getheadvar(gfile,'DLMD',dlmd,iret=iret)
 if (iret /= 0) goto 9200

 print*,'- GET DY OF GRID.'
 call nemsio_getheadvar(gfile,'DPHD',dphd,iret=iret)
 if (iret /= 0) goto 9200

 print*,'- GET CENTER LAT OF GRID.'
 call nemsio_getheadvar(gfile,'TPH0D',center_lat,iret=iret)
 if (iret /= 0) goto 9200

 print*,'- GET CENTER LON OF GRID.'
 call nemsio_getheadvar(gfile,'TLM0D',center_lon,iret=iret)
 if (iret /= 0) goto 9200

 print*,'- READ LATITUDES'
 allocate(dummy(ijmdl_output))
 vname='glat'
 vlevtyp='sfc'
 vlev=1
 call nemsio_readrecv(gfile,vname,vlevtyp,vlev,dummy,iret=iret)
 if (iret /= 0) goto 9200

 allocate (lats_output(imdl_output,jmdl_output))
 lats_output=reshape(dummy,(/imdl_output,jmdl_output/))

 print*,'- READ LONGITUDES'
 vname='glon'
 vlevtyp='sfc'
 vlev=1
 call nemsio_readrecv(gfile,vname,vlevtyp,vlev,dummy,iret=iret)
 if (iret /= 0) goto 9200

 allocate (lons_output(imdl_output,jmdl_output))
 lons_output=reshape(dummy,(/imdl_output,jmdl_output/))

 print*,'- READ TERRAIN'
 vname='hgt'
 vlevtyp='sfc'
 vlev=1
 call nemsio_readrecv(gfile,vname,vlevtyp,vlev,dummy,iret=iret)
 if (iret /= 0) goto 9200

 allocate (orog_output(imdl_output,jmdl_output))
 orog_output=reshape(dummy,(/imdl_output,jmdl_output/))

 print*,'- READ TBOT'
 vname='tg'
 vlevtyp='sfc'
 vlev=1
 call nemsio_readrecv(gfile,vname,vlevtyp,vlev,dummy,iret=iret)
 if (iret /= 0) goto 9200

 allocate(substrate_temp_output(imdl_output,jmdl_output))
 substrate_temp_output=reshape(dummy,(/imdl_output,jmdl_output/))

 allocate(dummyice(ijmdl_output))
 print*,'- ICE MASK'
 vname='sice'
 vlevtyp='sfc'
 vlev=1
 call nemsio_readrecv(gfile,vname,vlevtyp,vlev,dummyice,iret=iret)
 if (iret /= 0) goto 9200

 print*,'- LAND MASK'
 vname='sm'
 vlevtyp='sfc'
 vlev=1
 call nemsio_readrecv(gfile,vname,vlevtyp,vlev,dummy,iret=iret)
 if (iret /= 0) goto 9200

 allocate(dummy2(ijmdl_output))
 dummy2=1.0
 where(dummyice==1.0) dummy2=0.0
 where(dummy==1.0)    dummy2=0.0

 allocate(lsmask_output(imdl_output,jmdl_output))
 lsmask_output=reshape(dummy2,(/imdl_output,jmdl_output/))

 deallocate(dummy,dummy2,dummyice)

 print*,'- CLOSE FILE.'
 call nemsio_close(gfile,iret=iret)
 if (iret /= 0) then
   print*,'** CLOSE FAILED, ISTAT IS: ',iret
 endif

 kgds_output=0
 select case(trim(modelname))
   case ("NMME", "NMM")
     kgds_output(1) = 203  ! e-grid
   case ("NMMB")
     kgds_output(1) = 205  ! b-grid
     kgds_output(12) = nint(lats_output(imdl_output,jmdl_output)*1000.)
     kgds_output(13) = nint(lons_output(imdl_output,jmdl_output)*1000.)
   case default
     print*,'unrecognized grid '
     stop
 end select
 kgds_output(2) = imdl_output
 kgds_output(3) = jmdl_output
 kgds_output(4) = nint(lats_output(1,1)*1000.)
 kgds_output(5) = nint(lons_output(1,1)*1000.)
 kgds_output(6) = 136        ! oct 17 - resolution and component flag
 kgds_output(7) = nint(center_lat*1000.)
 kgds_output(8) = nint(center_lon*1000.)
 kgds_output(9) = abs(nint(dlmd*1000.))
 kgds_output(10) = abs(nint(dphd*1000.))
 kgds_output(11) = 64         ! oct 28 - scanning mode flag
 kgds_output(19) = 0          ! oct 4  - # vert coordinate parameters
 kgds_output(20) = 255        ! oct 5  - not used, set to 255

 return

 9000 print*,"- ERROR INITIALIZING NEMS MODULE, STATUS IS: ", iret
 call w3tage('COLDSTART_WRF')
 call mpi_abort(mpi_comm_world, 43, iret)

 9100 print*,"- ERROR OPENING NEMS FILE, STATUS IS: ", iret
 call w3tage('COLDSTART_WRF')
 call mpi_abort(mpi_comm_world, 43, iret)

 9200 print*,"- ERROR READING NEMS FILE, STATUS IS: ", iret
 call w3tage('COLDSTART_WRF')
 call mpi_abort(mpi_comm_world, 43, iret)

 end subroutine output_grid_specs_nems

!-----------------------------------------------------------------------
! read output grid specifications.
!
! if user chooses, the orog, land mask and substrate temp will
! be interpolated from the input grid.  to choose this option,
! set the file names to be zero length strings in the namelist
!-----------------------------------------------------------------------

 subroutine output_grid_specs_grib

 use read_write_utils, only   : read_grib_data

 implicit none

 include 'mpif.h'

 integer                     :: iret
 integer, parameter          :: iunit = 22
 integer                     :: jgds(200)
 integer                     :: jpds(200)
 integer                     :: kpds_output(200)
 integer                     :: lgrib
 integer                     :: lskip
 integer, parameter          :: lugi = 0
 integer                     :: numbytes
 integer                     :: numpts

 print*,"- OPEN FILE: ", trim(lats_output_file)

 call baopenr (iunit, lats_output_file, iret)

 if (iret /= 0) then
   print*,'- BAD OPEN OF FILE, IRET IS ', iret
   call w3tage("COLDSTART_WRF")
   call mpi_abort(mpi_comm_world, 56, iret)
 end if

!-----------------------------------------------------------------------
! read grib header of latitude file.  save several grid specific
! parameters.
!-----------------------------------------------------------------------

 lskip       = -1
 jpds        = -1
 jgds        = -1
 jpds(5)     = 176
 kpds_output = jpds
 kgds_output = jgds

 print*,"- READ GRIB HEADER"

 call getgbh(iunit, lugi, lskip, jpds, jgds, lgrib,  &
             numbytes, numpts, kpds_output, kgds_output, iret)

 if (iret /= 0) then
   print*,"- BAD READ OF GRIB HEADER, IRET IS ", iret
   call w3tage("COLDSTART_WRF")
   call mpi_abort(mpi_comm_world, 57, iret)
 end if

 call baclose(iunit, iret)

 imdl_output = kgds_output(2)
 jmdl_output = kgds_output(3)

 ijmdl_output = imdl_output * jmdl_output

!-----------------------------------------------------------------------
! get orography and stnd dev of orog.  if not chosen, will get from edasx.
!-----------------------------------------------------------------------

 allocate(orog_output(imdl_output,jmdl_output))
 orog_output = 0.0

 allocate(orog_stnd_output(imdl_output,jmdl_output))
 orog_stnd_output = 0.0

 allocate(theta_output(imdl_output,jmdl_output))
 theta_output=0.0
 allocate(gamma_output(imdl_output,jmdl_output))
 gamma_output=0.0
 allocate(sigma_output(imdl_output,jmdl_output))
 sigma_output=0.0
 allocate(oc_output(imdl_output,jmdl_output))
 oc_output=0.0
 allocate(elvmax_output(imdl_output,jmdl_output))
 elvmax_output=0.0
 allocate(oa_output(imdl_output,jmdl_output,4))
 oa_output=0.0
 allocate(ol_output(imdl_output,jmdl_output,4))
 ol_output=0.0


 if (len_trim(orog_output_file) > 0) then
   call read_grib_data(orog_output_file, 8, orog_output,  &
                       ijmdl_output, iret)
   if (iret /= 0) then
     call w3tage("COLDSTART_WRF")
     call mpi_abort(mpi_comm_world, 65, iret)
   end if

   call read_grib_data(orog_output_file, 9, orog_stnd_output, &
                       ijmdl_output, iret)
   print*,'std dev',maxval(orog_stnd_output)
   if (iret /= 0) then
     call w3tage("COLDSTART_WRF")
     call mpi_abort(mpi_comm_world, 65, iret)
   end if

   call read_grib_data(orog_output_file, 187, oc_output, &
                       ijmdl_output, iret)
   print*,'oc',maxval(oc_output)
   if (iret /= 0) then
     call w3tage("COLDSTART_WRF")
     call mpi_abort(mpi_comm_world, 65, iret)
   end if

   call read_grib_data(orog_output_file, 221, elvmax_output, &
                       ijmdl_output, iret)
   print*,'elvmax ',maxval(elvmax_output)
   if (iret /= 0) then
     call w3tage("COLDSTART_WRF")
     call mpi_abort(mpi_comm_world, 65, iret)
   end if

   call read_grib_data(orog_output_file, 101, theta_output, &
                       ijmdl_output, iret)
   print*,'theta ',maxval(theta_output)
   if (iret /= 0) then
     call w3tage("COLDSTART_WRF")
     call mpi_abort(mpi_comm_world, 65, iret)
   end if

   call read_grib_data(orog_output_file, 103, gamma_output, &
                       ijmdl_output, iret)
   print*,'gamma ',maxval(gamma_output)
   if (iret /= 0) then
     call w3tage("COLDSTART_WRF")
     call mpi_abort(mpi_comm_world, 65, iret)
   end if

   call read_grib_data(orog_output_file, 102, sigma_output, &
                       ijmdl_output, iret)
   print*,'sigma ',maxval(sigma_output)
   if (iret /= 0) then
     call w3tage("COLDSTART_WRF")
     call mpi_abort(mpi_comm_world, 65, iret)
   end if

   call read_grib_data(orog_output_file, 166, oa_output(:,:,1), &
                       ijmdl_output, iret)
   print*,'oa1 ',maxval(oa_output(:,:,1))
   if (iret /= 0) then
     call w3tage("COLDSTART_WRF")
     call mpi_abort(mpi_comm_world, 65, iret)
   end if

   call read_grib_data(orog_output_file, 167, oa_output(:,:,2), &
                       ijmdl_output, iret)
   print*,'oa2 ',maxval(oa_output(:,:,2))
   if (iret /= 0) then
     call w3tage("COLDSTART_WRF")
     call mpi_abort(mpi_comm_world, 65, iret)
   end if

   call read_grib_data(orog_output_file, 168, oa_output(:,:,3), &
                       ijmdl_output, iret)
   print*,'oa3 ',maxval(oa_output(:,:,3))
   if (iret /= 0) then
     call w3tage("COLDSTART_WRF")
     call mpi_abort(mpi_comm_world, 65, iret)
   end if

   call read_grib_data(orog_output_file, 169, oa_output(:,:,4), &
                       ijmdl_output, iret)
   print*,'oa4 ',maxval(oa_output(:,:,4))
   if (iret /= 0) then
     call w3tage("COLDSTART_WRF")
     call mpi_abort(mpi_comm_world, 65, iret)
   end if

   call read_grib_data(orog_output_file, 151, ol_output(:,:,1), &
                       ijmdl_output, iret)
   print*,'ol1 ',maxval(ol_output(:,:,1))
   if (iret /= 0) then
     call w3tage("COLDSTART_WRF")
     call mpi_abort(mpi_comm_world, 65, iret)
   end if

   call read_grib_data(orog_output_file, 152, ol_output(:,:,2), &
                       ijmdl_output, iret)
   print*,'ol2 ',maxval(ol_output(:,:,2))
   if (iret /= 0) then
     call w3tage("COLDSTART_WRF")
     call mpi_abort(mpi_comm_world, 65, iret)
   end if

   call read_grib_data(orog_output_file, 153, ol_output(:,:,3), &
                       ijmdl_output, iret)
   print*,'ol3 ',maxval(ol_output(:,:,3))
   if (iret /= 0) then
     call w3tage("COLDSTART_WRF")
     call mpi_abort(mpi_comm_world, 65, iret)
   end if

   call read_grib_data(orog_output_file, 154, ol_output(:,:,4), &
                       ijmdl_output, iret)
   print*,'ol4 ',maxval(ol_output(:,:,4))
   if (iret /= 0) then
     call w3tage("COLDSTART_WRF")
     call mpi_abort(mpi_comm_world, 65, iret)
   end if


end if

!-----------------------------------------------------------------------
! get latitudes.
!-----------------------------------------------------------------------

 allocate(lats_output(imdl_output,jmdl_output))

 call read_grib_data(lats_output_file, 176, lats_output, &
                     ijmdl_output, iret)

 if (iret /= 0) then
   call w3tage("COLDSTART_WRF")
   call mpi_abort(mpi_comm_world, 66, iret)
 end if

!-----------------------------------------------------------------------
! get longitudes.
!-----------------------------------------------------------------------

 allocate(lons_output(imdl_output,jmdl_output))

 call read_grib_data(lons_output_file, 177, lons_output, &
                     ijmdl_output, iret)

 if (iret /= 0) then
   call w3tage("COLDSTART_WRF")
   call mpi_abort(mpi_comm_world, 67, iret)
 end if

!-----------------------------------------------------------------------
! substrate temperature.  if not chosen, will get from edasx.
!-----------------------------------------------------------------------

 allocate(substrate_temp_output(imdl_output,jmdl_output))
 substrate_temp_output = 0.0

 if (len_trim(substrate_temp_output_file) > 0) then
   call read_grib_data(substrate_temp_output_file, 85,   &
                       substrate_temp_output, ijmdl_output, iret)
   if (iret /= 0) then
     call w3tage("COLDSTART_WRF")
     call mpi_abort(mpi_comm_world, 68, iret)
   end if
 end if

!-----------------------------------------------------------------------
! land/sea mask (actually, % land).
!-----------------------------------------------------------------------

 allocate(lsmask_output(imdl_output,jmdl_output))
 lsmask_output = -1.0

 if (len_trim(lsmask_output_file) > 0) then
   call read_grib_data(lsmask_output_file, 81,   &
                       lsmask_output(1,1), ijmdl_output, iret)
   if (iret /= 0) then
     call w3tage("COLDSTART_WRF")
     call mpi_abort(mpi_comm_world, 69, iret)
   end if
 end if

!-----------------------------------------------------------------------
! if any of the optional files are not selected, tell rest of
! program to get these data from the input file.
!-----------------------------------------------------------------------

 return

 end subroutine output_grid_specs_grib

!-----------------------------------------------------------------------
! read configuration namelist.
!-----------------------------------------------------------------------

 subroutine read_config_namelist

 implicit none

 include 'mpif.h'

 integer                     :: istat

 namelist /input_state_fields/     input_file,   &
                                   input_file_type

 namelist /output_grid_specs/      specs_from_output_file, &
                                   lats_output_file,       &
                                   lons_output_file,       &
                                   lsmask_output_file,     &
                                   orog_output_file,       &
                                   substrate_temp_output_file 

 namelist /final_output/           output_file_type,        &
                                   output_file
 
 namelist /nam_options/            merge

 print*,"- READ CONFIGURATION NAMELIST"

 open(81, iostat=istat, err=900)

 read(81, nml=input_state_fields,         iostat=istat, err=910)
 read(81, nml=output_grid_specs,          iostat=istat, err=910)
 read(81, nml=final_output,               iostat=istat, err=910)
 read(81, nml=nam_options,                iostat=istat, err=910)
 close(81)

 return

900 print*,"- ERROR OPENING CONFIG NAMELIST. ISTAT IS ", istat
    call w3tage("COLDSTART_WRF")
    call mpi_abort(mpi_comm_world, 50, istat)

910 print*,"- ERROR READING CONFIG NAMELIST. ISTAT IS ", istat
    call w3tage("COLDSTART_WRF")
    call mpi_abort(mpi_comm_world, 51, istat)
   
 end subroutine read_config_namelist

!-----------------------------------------------------------------------
! free up memory.
!-----------------------------------------------------------------------

 subroutine program_setup_cleanup

 implicit none

 if (allocated(lats_output))           deallocate (lats_output)
 if (allocated(lons_output))           deallocate (lons_output)
 if (allocated(lsmask_output))         deallocate (lsmask_output)
 if (allocated(orog_output))           deallocate (orog_output)
!cgwd this routine is called after the interpolation, but i need to
!cgwd write this to the target input file.
!cgwd if (allocated(orog_stnd_output))      deallocate (orog_stnd_output)
 if (allocated(substrate_temp_output)) deallocate (substrate_temp_output)
!cgwd  add new variables here.

 return

 end subroutine program_setup_cleanup

 end module program_setup
