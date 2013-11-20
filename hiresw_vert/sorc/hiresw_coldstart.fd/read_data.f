 module read_data

 use surface_chgres, only    : sfc2d

 type(sfc2d)                :: data_input  ! input land fields

 integer                    :: curr_day
 integer                    :: curr_hour
 integer                    :: curr_mon
 integer                    :: curr_year
 integer                    :: imdl_input, jmdl_input, ijmdl_input 
 integer                    :: kgds_input(200)
 integer                    :: nsoil_input

 contains

!-----------------------------------------------------------------------
! read input land-surface fields from edas file or another wrf nmm file.
!-----------------------------------------------------------------------

 subroutine read_input_file

 use program_setup, only        : input_file_type

 implicit none

 include 'mpif.h'

 integer   :: iret

 selectcase(trim(input_file_type))
 case("edas", "EDAS")
   call read_input_file_edas
 case("nems", "NEMS")
   call read_input_file_nems
 case("gfs", "GFS")
   call read_input_file_gfs
 case default
   print*,"- ERROR. UNRECOGNIZED INPUT FILE TYPE"
   call w3tage('COLDSTART_WRF')
   call mpi_abort(mpi_comm_world, 77, iret)
 end select

 end subroutine read_input_file

!-----------------------------------------------------------------------
! read input land-surface states from gfs restart file.
!-----------------------------------------------------------------------

 subroutine read_input_file_gfs

 use program_setup, only         : input_file

 use read_write_utils, only      : new_time

 use sfcio_module

 implicit none

 include 'mpif.h'

 character*10                 :: curr_date

 integer(sfcio_intkind)       :: iret, unit
 integer                      :: cycle_year, cycle_mon, cycle_day, cycle_hour, curr_min
 integer                      :: i, j, mask

 real                         :: fcst_hour

 type(sfcio_head)             :: header
 type(sfcio_data)             :: dummy

 unit=15

 print*,"- OPEN GFS SURFACE RESTART FILE: ", trim(input_file)
 call sfcio_sropen(unit, input_file, iret)
 if (iret /= 0) goto 9000

 print*,'- READ FILE HEADER'
 call sfcio_srhead(unit,header,iret)
 if (iret /= 0) goto 9100

 if (header%ivs < 200501) then
   print*,"- PROGRAM NOT TESTED WITH OLD STYLE GFS FILE."
   call w3tage('COLDSTART_WRF')
   call mpi_abort(mpi_comm_world, 90, iret)
 endif
 
 cycle_hour=header%idate(1)
 cycle_day=header%idate(3)
 cycle_mon=header%idate(2)
 cycle_year=header%idate(4)
 fcst_hour=header%fhour

 print*,"- CYCLE TIME OF GFS FILE IS: "
 print*,"- YEAR: ",cycle_year
 print*,"- MON:  ",cycle_mon
 print*,"- DAY:  ",cycle_day
 print*,"- HOUR: ",cycle_hour
 print*,"- FCST HOUR: ",fcst_hour

 call new_time (cycle_year, cycle_mon, cycle_day, cycle_hour,  &
                fcst_hour, curr_year, curr_mon, curr_day, curr_hour,  &
                curr_min, curr_date)

 print*,"- WILL TIME INTERPOLATE CLIMO FIELDS TO: ", curr_date
 
 nsoil_input=header%lsoil
 imdl_input=header%lonb
 jmdl_input=header%latb
 ijmdl_input=imdl_input*jmdl_input

 print*,"- INPUT GRID SPECS ARE:"
 print*,"- W-E DIMENSION: ",imdl_input
 print*,"- S-N DIMENSION: ",jmdl_input
 print*,"- NUMBER OF SOIL LAYERS: ", nsoil_input

 kgds_input = 0
 kgds_input(1) = 4                          ! OCT 6 - TYPE OF GRID (GAUSSIAN)
 kgds_input(2) = imdl_input                 ! OCT 7-8 - # PTS ON LATITUDE CIRCLE
 kgds_input(3) = jmdl_input                 ! OCT 9-10 - # PTS ON LONGITUDE CIRCLE
 kgds_input(4) = 90000                      ! OCT 11-13 - LAT OF ORIGIN
 kgds_input(5) = 0                          ! OCT 14-16 - LON OF ORIGIN
 kgds_input(6) = 128                        ! OCT 17 - RESOLUTION FLAG
 kgds_input(7) = -90000                     ! OCT 18-20 - LAT OF EXTREME POINT
 kgds_input(8) = nint(-360000./imdl_input)  ! OCT 21-23 - LON OF EXTREME POINT
 kgds_input(9) = nint((360.0 / float(imdl_input))*1000.0)
                                            ! OCT 24-25 - LONGITUDE DIRECTION INCR.
 kgds_input(10) = jmdl_input /2             ! OCT 26-27 - NUMBER OF CIRCLES POLE TO EQUATOR
 kgds_input(12) = 255                       ! OCT 29 - RESERVED
 kgds_input(20) = 255                       ! OCT 5  - NOT USED, SET TO 255

 nsoil_input = header%lsoil

 print*,'- READ SURFACE FIELDS'
 call sfcio_aldata(header, dummy, iret)
 if (iret /= 0) goto 9100
 call sfcio_srdata(unit, header, dummy, iret)
 if (iret /= 0) goto 9100

 allocate(data_input%greenfrc(imdl_input,jmdl_input))
 data_input%greenfrc=dummy%vfrac
 allocate(data_input%mxsnow_alb(imdl_input,jmdl_input))
 data_input%mxsnow_alb=dummy%snoalb
 allocate(data_input%z0(imdl_input,jmdl_input))
 data_input%z0=dummy%zorl   ! cm
 allocate(data_input%canopy_mc(imdl_input,jmdl_input))
! gfs in mm, nmm expects meters
 data_input%canopy_mc=dummy%canopy * .001
 allocate(data_input%substrate_temp(imdl_input,jmdl_input))
 data_input%substrate_temp=dummy%tg3
!-----------------------------------------------------------------------
! gfs and wrf use very different approaches for snow-free albedo
! therefore, you never want to use the albedo interpolated
! from the gfs grid.
!-----------------------------------------------------------------------
 allocate(data_input%snow_free_albedo(imdl_input,jmdl_input))
 data_input%snow_free_albedo=dummy%alvsf
 allocate(data_input%skin_temp(imdl_input,jmdl_input))
 data_input%skin_temp=dummy%tsea
 allocate(data_input%orog(imdl_input,jmdl_input))
 data_input%orog=dummy%orog
 allocate(data_input%snow_depth(imdl_input,jmdl_input))
 data_input%snow_depth=dummy%snwdph
 allocate(data_input%snow_liq_equiv(imdl_input,jmdl_input))
 data_input%snow_liq_equiv=dummy%sheleg
 allocate(data_input%veg_type(imdl_input,jmdl_input))
 data_input%veg_type=nint(dummy%vtype)
 allocate(data_input%soil_type(imdl_input,jmdl_input))
 data_input%soil_type=nint(dummy%stype)
 allocate(data_input%slope_type(imdl_input,jmdl_input))
 data_input%slope_type=nint(dummy%slope)
 allocate(data_input%soil_temp(imdl_input,jmdl_input,nsoil_input))
 data_input%soil_temp=dummy%stc
 allocate(data_input%soilm_tot(imdl_input,jmdl_input,nsoil_input))
 data_input%soilm_tot=dummy%smc
 allocate(data_input%soilm_liq(imdl_input,jmdl_input,nsoil_input))
 data_input%soilm_liq=dummy%slc
!-----------------------------------------------------------------------
! gfs mask: 0-water, 1-land, 2-seaice
! interpolation module needs a mask of 0-nonland, 1-land; and
! a sea ice flag of 0-no ice, 1-ice
!-----------------------------------------------------------------------
 allocate(data_input%lsmask(imdl_input,jmdl_input))
 allocate(data_input%sea_ice_flag(imdl_input,jmdl_input))
 do j = 1, jmdl_input
 do i = 1, imdl_input
    mask=nint(dummy%slmsk(i,j))
    if (mask == 2) then
      data_input%sea_ice_flag(i,j) = 1
      data_input%lsmask(i,j) = 0.0
    elseif (mask == 1) then
      data_input%sea_ice_flag(i,j) = 0
      data_input%lsmask(i,j) = 1.0
    elseif (mask == 0) then
      data_input%sea_ice_flag(i,j) = 0
      data_input%lsmask(i,j) = 0.0
    endif
 enddo
 enddo

 call sfcio_axdata(dummy, iret)
 call sfcio_sclose(unit, iret)

 return

 9000 print*,"- ERROR OPENING GFS RESTART FILE. STATUS IS: ", iret
 call w3tage('COLDSTART_WRF')
 call mpi_abort(mpi_comm_world, 89, iret)

 9100 print*,"- ERROR READING GFS RESTART FILE. STATUS IS: ", iret
 call w3tage('COLDSTART_WRF')
 call mpi_abort(mpi_comm_world, 90, iret)

 end subroutine read_input_file_gfs

!-----------------------------------------------------------------------
! read land-surface fields from nemsio file.
!-----------------------------------------------------------------------

 subroutine read_input_file_nems

 use nemsio_module
 use program_setup, only             : input_file

 implicit none
 
 include 'mpif.h'

 character*255                      :: gfname
 character(nemsio_charkind8)        :: gaction, modelname
 character*20                       :: vlevtyp, vname

 integer(nemsio_intkind)            :: idum2, idum3, idum4, idum5(7), iret, vlev
 integer(nemsio_intkind)            :: idum6, idum7, idum8, idum9
 integer                            :: i, j, idat(8), jdat(8), kmdl_input

 real(nemsio_realkind)              :: dx, dy, center_lat, center_lon
 real*8, allocatable                :: dummy(:), dum2d(:,:)
 real                               :: lat11, lon11, latlast, lonlast, rinc(5)

 type(nemsio_gfile)                 :: gfile

 print*,"- READ SURFACE DATA FROM INPUT GRID."

 print*,'- INITIALIZE NEMSIO MODULE.'
 call nemsio_init(iret=iret)
 if (iret /= 0) goto 9000

 gfname=input_file
 print*,'- OPEN FILE FOR READING: ', trim(gfname)
 gaction="READ"
 call nemsio_open(gfile,gfname,gaction,iret=iret)
 if (iret /= 0) goto 9000
 
 print*,"- READ FILE HEADER."
 call nemsio_getfilehead(gfile,iret,modelname=modelname,idate=idum5,&
                         nfday=idum6,nfhour=idum7,nfminute=idum8, &
                         nsoil=idum2, &
                         dimx=idum3,dimy=idum4,dimz=idum9)
 if (iret /= 0) goto 9000

 imdl_input=idum3
 jmdl_input=idum4
 kmdl_input=idum9
 ijmdl_input=imdl_input*jmdl_input
 nsoil_input=idum2

 idat=0
 jdat=0
 rinc=0.0
 idat(1)=idum5(1)  ! year
 idat(2)=idum5(2)  ! mon
 idat(3)=idum5(3)  ! day
 idat(4)=0         ! time zone, utc
 idat(5)=idum5(4)  ! hour
 idat(6)=idum5(5)  ! minute
 rinc(1)=idum6     ! days inc
 rinc(2)=idum7     ! hrs inc
 rinc(3)=idum8     ! mins inc

 call w3movdat(rinc,idat,jdat)

 curr_year=jdat(1)
 curr_mon=jdat(2)
 curr_day=jdat(3)
 curr_hour=jdat(5)

 print*,"- DATE OF NEMS INPUT FILE IS: "
 print*,"- YEAR: ",curr_year
 print*,"- MON:  ",curr_mon
 print*,"- DAY:  ",curr_day
 print*,"- HOUR: ",curr_hour

 print*,'- I-DIMENSION OF GRID: ',imdl_input
 print*,'- J-DIMENSION OF GRID: ',jmdl_input
 print*,'- K-DIMENSION OF GRID: ',kmdl_input
 print*,'- NUMBER OF SOIL LAYERS: ',nsoil_input

 print*,'- GET DX OF GRID.'
 call nemsio_getheadvar(gfile,'DLMD',dx,iret=iret)
 if (iret /= 0) goto 9000
 print*,'- DX OF GRID IS: ',dx

 print*,'- GET DY OF GRID.'
 call nemsio_getheadvar(gfile,'DPHD',dy,iret=iret)
 if (iret /= 0) goto 9000
 print*,'- DY OF GRID IS: ',dy

 print*,'- GET CENTER LAT OF GRID.'
 call nemsio_getheadvar(gfile,'TPH0D',center_lat,iret=iret)
 if (iret /= 0) goto 9000
 print*,'- CENTER LAT OF GRID: ', center_lat

 print*,'- GET CENTER LON OF GRID.'
 call nemsio_getheadvar(gfile,'TLM0D',center_lon,iret=iret)
 if (iret /= 0) goto 9000
 print*,'- CENTER LON OF GRID: ', center_lon

 print*,'- READ LATITUDES'
 allocate(dummy(ijmdl_input))
 vname='glat'
 vlevtyp='sfc'
 vlev=1
 call nemsio_readrecv(gfile,vname,vlevtyp,vlev,dummy,iret=iret)
 if (iret /= 0) goto 9000
 lat11=dummy(1)
 latlast=dummy(ijmdl_input)
 allocate(data_input%lats(imdl_input,jmdl_input))
 data_input%lats=reshape(dummy, (/imdl_input,jmdl_input/))

 print*,'- READ LONGITUDES'
 vname='glon'
 vlevtyp='sfc'
 vlev=1
 call nemsio_readrecv(gfile,vname,vlevtyp,vlev,dummy,iret=iret)
 if (iret /= 0) goto 9000
 lon11=dummy(1)
 lonlast=dummy(ijmdl_input)
 allocate(data_input%lons(imdl_input,jmdl_input))
 data_input%lons=reshape(dummy, (/imdl_input,jmdl_input/))

 print*,"- READ GREENNESS FRACTION."
 vname='vegfrc'
 vlevtyp='sfc'
 vlev=1
 call nemsio_readrecv(gfile,vname,vlevtyp,vlev,dummy,iret=iret)
 if (iret /= 0) goto 9000
 call fix_nmm8(imdl_input,jmdl_input,dummy)  ! nems file have garbage values in
                                             ! outer two rows/columns.
 allocate(data_input%greenfrc(imdl_input,jmdl_input))
 data_input%greenfrc=reshape(dummy, (/imdl_input,jmdl_input/))

 print*,"- READ SEA ICE FLAG."
 vname='sice'
 vlevtyp='sfc'
 vlev=1
 call nemsio_readrecv(gfile,vname,vlevtyp,vlev,dummy,iret=iret)
 if (iret /= 0) goto 9000
 call fix_nmm8(imdl_input,jmdl_input,dummy)
 allocate(data_input%sea_ice_flag(imdl_input,jmdl_input))
 data_input%sea_ice_flag=reshape(dummy, (/imdl_input,jmdl_input/))

 print*,"- READ LAND MASK."
 vname='sm'
 vlevtyp='sfc'
 vlev=1
 call nemsio_readrecv(gfile,vname,vlevtyp,vlev,dummy,iret=iret)
 if (iret /= 0) goto 9000
 call fix_nmm8(imdl_input,jmdl_input,dummy)

 allocate(dum2d(imdl_input,jmdl_input))
 dum2d=reshape(dummy, (/imdl_input,jmdl_input/))

 allocate(data_input%lsmask(imdl_input,jmdl_input))
 data_input%lsmask = 0.0    ! non-land
 do j = 1, jmdl_input
 do i = 1, imdl_input
   if (dum2d(i,j) == 0.0 .and. data_input%sea_ice_flag(i,j) == 0) then
     data_input%lsmask(i,j) = 1.0   ! land
   endif
 enddo
 enddo
 deallocate(dum2d)

 print*,"- READ MAX SNOW ALBEDO."
 vname='mxsnal'
 vlevtyp='sfc'
 vlev=1
 call nemsio_readrecv(gfile,vname,vlevtyp,vlev,dummy,iret=iret)
 if (iret /= 0) goto 9000
 call fix_nmm8(imdl_input,jmdl_input,dummy)
 allocate(data_input%mxsnow_alb(imdl_input,jmdl_input))
 data_input%mxsnow_alb=reshape(dummy, (/imdl_input,jmdl_input/))
 
 print*,"- READ ROUGHNESS LENGTH."
 vname='zorl'
 vlevtyp='sfc'
 vlev=1
 call nemsio_readrecv(gfile,vname,vlevtyp,vlev,dummy,iret=iret)
 if (iret /= 0) goto 9000
 call fix_nmm8(imdl_input,jmdl_input,dummy)
 allocate(data_input%z0(imdl_input,jmdl_input))
 data_input%z0=reshape(dummy, (/imdl_input,jmdl_input/))
 data_input%z0=data_input%z0*100.0  ! interp code expects cm

 print*,"- READ CANOPY MOISTURE CONTENT."
 vname='cmc'
 vlevtyp='sfc'
 vlev=1
 call nemsio_readrecv(gfile,vname,vlevtyp,vlev,dummy,iret=iret)
 if (iret /= 0) goto 9000
 call fix_nmm8(imdl_input,jmdl_input,dummy)
 allocate(data_input%canopy_mc(imdl_input,jmdl_input))
 data_input%canopy_mc=reshape(dummy, (/imdl_input,jmdl_input/))

 print*,"- READ SUBSTRATE TEMPERATURE."
 vname='tg'
 vlevtyp='sfc'
 vlev=1
 call nemsio_readrecv(gfile,vname,vlevtyp,vlev,dummy,iret=iret)
 if (iret /= 0) goto 9000
 call fix_nmm8(imdl_input,jmdl_input,dummy)
 allocate(data_input%substrate_temp(imdl_input,jmdl_input))
 data_input%substrate_temp=reshape(dummy, (/imdl_input,jmdl_input/))

 print*,"- READ BASE ALBEDO."
 vname='albase'
 vlevtyp='sfc'
 vlev=1
 call nemsio_readrecv(gfile,vname,vlevtyp,vlev,dummy,iret=iret)
 if (iret /= 0) goto 9000
 call fix_nmm8(imdl_input,jmdl_input,dummy)
 allocate(data_input%snow_free_albedo(imdl_input,jmdl_input))
 data_input%snow_free_albedo=reshape(dummy, (/imdl_input,jmdl_input/))

 print*,"- SKIN TEMPERATURE."
 vname='tskin'   ! skin potential temperature
 vlevtyp='sfc'
 vlev=1
 call nemsio_readrecv(gfile,vname,vlevtyp,vlev,dummy,iret=iret)
 if (iret /= 0) goto 9000
 call fix_nmm8(imdl_input,jmdl_input,dummy)
 allocate(data_input%skin_temp(imdl_input,jmdl_input))
 data_input%skin_temp=reshape(dummy, (/imdl_input,jmdl_input/))

 print*,"- TERRAIN."
 vname='hgt'
 vlevtyp='sfc'
 vlev=1
 call nemsio_readrecv(gfile,vname,vlevtyp,vlev,dummy,iret=iret)
 if (iret /= 0) goto 9000
 call fix_nmm8(imdl_input,jmdl_input,dummy)
 allocate(data_input%orog(imdl_input,jmdl_input))
 data_input%orog=reshape(dummy, (/imdl_input,jmdl_input/))

 print*,"- SNOW DEPTH."
 vname='si'
 vlevtyp='sfc'
 vlev=1
 call nemsio_readrecv(gfile,vname,vlevtyp,vlev,dummy,iret=iret)
 if (iret /= 0) goto 9000
 call fix_nmm8(imdl_input,jmdl_input,dummy)
 allocate(data_input%snow_depth(imdl_input,jmdl_input))
 data_input%snow_depth=reshape(dummy, (/imdl_input,jmdl_input/))

 print*,"- LIQUID EQUIVALENT SNOW."
 vname='sno'
 vlevtyp='sfc'
 vlev=1
 call nemsio_readrecv(gfile,vname,vlevtyp,vlev,dummy,iret=iret)
 if (iret /= 0) goto 9000
 call fix_nmm8(imdl_input,jmdl_input,dummy)
 allocate(data_input%snow_liq_equiv(imdl_input,jmdl_input))
 data_input%snow_liq_equiv=reshape(dummy, (/imdl_input,jmdl_input/))

 print*,"- SOIL TYPE."
 vname='sltyp'
 vlevtyp='sfc'
 vlev=1
 call nemsio_readrecv(gfile,vname,vlevtyp,vlev,dummy,iret=iret)
 if (iret /= 0) goto 9000
 call fix_nmm8(imdl_input,jmdl_input,dummy)
 allocate(data_input%soil_type(imdl_input,jmdl_input))
 data_input%soil_type=reshape(nint(dummy), (/imdl_input,jmdl_input/))

 print*,"- VEGETATION TYPE."
 vname='vgtyp'
 vlevtyp='sfc'
 vlev=1
 call nemsio_readrecv(gfile,vname,vlevtyp,vlev,dummy,iret=iret)
 if (iret /= 0) goto 9000
 call fix_nmm8(imdl_input,jmdl_input,dummy)
 allocate(data_input%veg_type(imdl_input,jmdl_input))
 data_input%veg_type=reshape(nint(dummy), (/imdl_input,jmdl_input/))

 print*,'- SOIL TEMPERATURE.'
 vname='stc'
 vlevtyp='soil layer'
 allocate(data_input%soil_temp(imdl_input,jmdl_input,nsoil_input))
 do vlev = 1, nsoil_input
   call nemsio_readrecv(gfile,vname,vlevtyp,vlev,dummy,iret=iret)
   if (iret /= 0) goto 9000
   call fix_nmm8(imdl_input,jmdl_input,dummy)
   data_input%soil_temp(:,:,vlev)=reshape( dummy , (/imdl_input,jmdl_input/))
 enddo

 print*,'- TOTAL SOIL MOISTURE.'
 vname='smc'
 vlevtyp='soil layer'
 allocate(data_input%soilm_tot(imdl_input,jmdl_input,nsoil_input))
 do vlev = 1, nsoil_input
   call nemsio_readrecv(gfile,vname,vlevtyp,vlev,dummy,iret=iret)
   if (iret /= 0) goto 9000
   call fix_nmm8(imdl_input,jmdl_input,dummy)
   data_input%soilm_tot(:,:,vlev)=reshape( dummy , (/imdl_input,jmdl_input/))
 enddo

 print*,'- LIQUID SOIL MOISTURE.'
 vname='sh2o'
 vlevtyp='soil layer'
 allocate(data_input%soilm_liq(imdl_input,jmdl_input,nsoil_input))
 do vlev = 1, nsoil_input
   call nemsio_readrecv(gfile,vname,vlevtyp,vlev,dummy,iret=iret)
   if (iret /= 0) goto 9000
   call fix_nmm8(imdl_input,jmdl_input,dummy)
   data_input%soilm_liq(:,:,vlev)=reshape( dummy , (/imdl_input,jmdl_input/))
 enddo

 print*,'- CLOSE FILE.'
 call nemsio_close(gfile,iret=iret)
 if (iret /= 0) then
   print*,'CLOSE FAILED, IRET IS:',iret
 endif

 kgds_input=0
 if (modelname == "NMME") then
   kgds_input(1) = 203
 elseif (modelname == "NMMB") then
   kgds_input(1) = 205
   kgds_input(12) = nint(latlast*1000.)
   kgds_input(13) = nint(lonlast*1000.)
 endif
 kgds_input(2) = imdl_input
 kgds_input(3) = jmdl_input
 kgds_input(4) = nint(lat11*1000.)
 kgds_input(5) = nint(lon11*1000.)
 kgds_input(6) = 136        ! oct 17 - resolution and component flag
 kgds_input(7) = nint(center_lat*1000.)
 kgds_input(8) = nint(center_lon*1000.)
 kgds_input(9) = abs(nint(dx*1000.))
 kgds_input(10) = abs(nint(dy*1000.))
 kgds_input(11) = 64         ! oct 28 - scanning mode flag
 kgds_input(19) = 0          ! oct 4  - # vert coordinate parameters
 kgds_input(20) = 255        ! oct 5  - not used, set to 255
!cggg  grib does not store with enough precision.  as far as i know,
!cggg  these array slots are not used.
 kgds_input(199) = abs(nint(dx*100000.))
 kgds_input(200) = abs(nint(dy*100000.))

 return

 9000 print*,"- ERROR READING NEMS FILE, ISTAT IS: ", iret
 call w3tage('COLDSTART_WRF')
 call mpi_abort(mpi_comm_world, 33, iret)

 end subroutine read_input_file_nems

!-----------------------------------------------------------------------
! the outer two rows/columns of the nmm restart file are hosed, so
! replace the data with nearest neighbors from the grid interior.
!-----------------------------------------------------------------------

 subroutine fix_nmm8(imdl,jmdl,var)

 implicit none

 integer :: i, j
 integer, intent(in) :: imdl,jmdl

 real*8, intent(inout) :: var(imdl,jmdl)

 do i = 3, imdl-2
   var(i,1)=var(i,3)
   var(i,2)=var(i,3)
   var(i,jmdl)=var(i,jmdl-2)
   var(i,jmdl-1)=var(i,jmdl-2)
 enddo

 do j = 3, jmdl-2
   var(1,j)=var(3,j)
   var(2,j)=var(3,j)
   var(imdl,j)=var(imdl-2,j)
   var(imdl-1,j)=var(imdl-2,j)
 enddo

 var(1,1)=var(3,3)
 var(1,2)=var(3,3)
 var(2,1)=var(3,3)
 var(2,2)=var(3,3)

 var(imdl,1)=var(imdl-2,3)
 var(imdl-1,1)=var(imdl-2,3)
 var(imdl-1,2)=var(imdl-2,3)
 var(imdl,2)=var(imdl-2,3)

 var(1,jmdl)=var(3,jmdl-2)
 var(2,jmdl)=var(3,jmdl-2)
 var(1,jmdl-1)=var(3,jmdl-2)
 var(2,jmdl-1)=var(3,jmdl-2)

 var(imdl,jmdl)=var(imdl-2,jmdl-2)
 var(imdl-1,jmdl)=var(imdl-2,jmdl-2)
 var(imdl,jmdl-1)=var(imdl-2,jmdl-2)
 var(imdl-1,jmdl-1)=var(imdl-2,jmdl-2)

 return

 end subroutine fix_nmm8

!-----------------------------------------------------------------------
! the outer two rows/columns of the nmm restart file are hosed, so
! replace the data with nearest neighbors from the grid interior.
!-----------------------------------------------------------------------

 subroutine fix_nmmi(imdl,jmdl,var)

 implicit none

 integer :: i, j
 integer, intent(in) :: imdl,jmdl

 integer, intent(inout) :: var(imdl,jmdl)

 do i = 3, imdl-2
   var(i,1)=var(i,3)
   var(i,2)=var(i,3)
   var(i,jmdl)=var(i,jmdl-2)
   var(i,jmdl-1)=var(i,jmdl-2)
 enddo

 do j = 3, jmdl-2
   var(1,j)=var(3,j)
   var(2,j)=var(3,j)
   var(imdl,j)=var(imdl-2,j)
   var(imdl-1,j)=var(imdl-2,j)
 enddo

 var(1,1)=var(3,3)
 var(1,2)=var(3,3)
 var(2,1)=var(3,3)
 var(2,2)=var(3,3)

 var(imdl,1)=var(imdl-2,3)
 var(imdl-1,1)=var(imdl-2,3)
 var(imdl-1,2)=var(imdl-2,3)
 var(imdl,2)=var(imdl-2,3)

 var(1,jmdl)=var(3,jmdl-2)
 var(2,jmdl)=var(3,jmdl-2)
 var(1,jmdl-1)=var(3,jmdl-2)
 var(2,jmdl-1)=var(3,jmdl-2)

 var(imdl,jmdl)=var(imdl-2,jmdl-2)
 var(imdl-1,jmdl)=var(imdl-2,jmdl-2)
 var(imdl,jmdl-1)=var(imdl-2,jmdl-2)
 var(imdl-1,jmdl-1)=var(imdl-2,jmdl-2)

 return

 end subroutine fix_nmmi

!-----------------------------------------------------------------------
! read the input state variables.  from 12km edas file.
!-----------------------------------------------------------------------

 subroutine read_input_file_edas

 use program_setup, only         : input_file

 use consts, only                : rovcp

 use read_write_utils, only      : new_time

 implicit none

 include 'mpif.h'

!-----------------------------------------------------------------------
! note: edas file is 4 byte ints/floats.
!-----------------------------------------------------------------------

 integer                        :: i, j
 integer                        :: idat(3)
 integer*4, allocatable         :: idum(:,:)
 integer                        :: ifhr
 integer                        :: ihrst
 integer                        :: istat

 character*10                   :: curr_date
 integer                        :: curr_min
 integer                        :: cycle_day
 integer                        :: cycle_hour
 integer                        :: cycle_mon
 integer                        :: cycle_year
 real                           :: fcst_hour

 real*4, allocatable            :: dum(:,:)
 real*4, allocatable            :: dum3d(:,:,:)
 real                           :: exner
 real*4, allocatable            :: icemask(:,:)
 real*4, allocatable            :: landmask(:,:)
 real*4, allocatable            :: psfc(:,:)
 real*4                         :: wbd, sbd, tlm0d, tph0d, dlmd, dphd

!-----------------------------------------------------------------------
! grid specs of edas data are as follows.
!-----------------------------------------------------------------------

 imdl_input = 606
 jmdl_input = 1067

 ijmdl_input = imdl_input * jmdl_input

 nsoil_input = 4

 kgds_input=0
 kgds_input(1) = 203
 kgds_input(2) = imdl_input
 kgds_input(3) = jmdl_input
 kgds_input(4) = nint(-3.441*1000.)  ! lat of corner point
 kgds_input(5) = nint(-148.799*1000.)! lon of corner point
 kgds_input(6) = 136        ! oct 17 - resolution and component flag
 kgds_input(7) = nint(50.0*1000.)    ! center lat
 kgds_input(8) = nint(-111*1000.)    ! center lon
 kgds_input(9) = nint((53./605.)*1000.)  ! dx
 kgds_input(10) = nint((40./533.)*1000.) ! dy
 kgds_input(11) = 64         ! oct 28 - scanning mode flag
 kgds_input(19) = 0          ! oct 4  - # vert coordinate parameters
 kgds_input(20) = 255        ! oct 5  - not used, set to 255

!-----------------------------------------------------------------------
! first, get cycle time of file.
!-----------------------------------------------------------------------

 print*,"- OPEN AND READ EDAS FILE: ", trim(input_file)

 open(9, file=trim(input_file), form="unformatted", status='old', &
         iostat=istat, err=9100)

 read(9, iostat=istat, err=9000, end=9000) ifhr, ihrst, idat(1), idat(2), idat(3)

 print*,"- DATE/TIME OF DATA IS..."
 print*,"- FCST HOUR:  ", ifhr
 print*,"- CYCLE HOUR: ", ihrst 
 print*,"- DAY:        ", idat(2)
 print*,"- MONTH:      ", idat(1) 
 print*,"- YEAR:       ", idat(3)

 cycle_year = idat(3)
 cycle_mon  = idat(1)
 cycle_day  = idat(2)
 cycle_hour = ihrst
 fcst_hour  = float(ifhr)

!----------------------------------------------------------------------
! based on the forecast hour of the data, calculate the current
! time for use in time interpolations.
!----------------------------------------------------------------------

 call new_time (cycle_year, cycle_mon, cycle_day, cycle_hour,  &
                fcst_hour, curr_year, curr_mon, curr_day, curr_hour,  &
                curr_min, curr_date)

 print*,"- WILL TIME INTERPOLATE CLIMO FIELDS TO: ", curr_date

 allocate (dum(imdl_input,jmdl_input))
 allocate (idum(imdl_input,jmdl_input))
 allocate (dum3d(imdl_input,jmdl_input,nsoil_input))
 allocate (landmask(imdl_input,jmdl_input))
 allocate (icemask(imdl_input,jmdl_input))
 
 read(9, iostat=istat, err=9000, end=9000) landmask ! 0-seaice/land, 1-open sea
 read(9, iostat=istat, err=9000, end=9000) icemask  ! 0-land/open sea, 1-seaice

 allocate (data_input%lsmask(imdl_input,jmdl_input))

 data_input%lsmask = 0.0 ! non-land
 where (landmask == 0.0 .and. icemask == 0.0) data_input%lsmask = 1.0  ! land

 deallocate(landmask)

 allocate (data_input%sea_ice_flag(imdl_input,jmdl_input))
 data_input%sea_ice_flag = icemask

 deallocate(icemask)

 read(9, iostat=istat, err=9000, end=9000) dum  ! lats
 read(9, iostat=istat, err=9000, end=9000) dum  ! lons

!-----------------------------------------------------------------------
! max snow albedo  (decimal)   the data contains garbage values
! at non-land points. screen these out.
!-----------------------------------------------------------------------

 read(9, iostat=istat, err=9000, end=9000) dum  
 allocate (data_input%mxsnow_alb(imdl_input,jmdl_input))
 data_input%mxsnow_alb = dum

 where (data_input%lsmask < 1.0) data_input%mxsnow_alb = 0.0

 read(9, iostat=istat, err=9000, end=9000) dum  ! espr

 allocate(data_input%substrate_temp(imdl_input,jmdl_input))
 read(9, iostat=istat, err=9000, end=9000) dum  ! substrate t
 data_input%substrate_temp = dum

 read(9, iostat=istat, err=9000, end=9000) dum  ! gffc

! note: sst data contains fill values over the interior of CONUS,
! including the great lakes.  therefore, don't use this data.
! instead, use the skin potential temperature, which appears to
! have valid data everywhere.  over water skin pot temp is the same
! as the sst if you divide by the exner function.

 read(9, iostat=istat, err=9000, end=9000) dum
! allocate(sst_input(imdl_input,jmdl_input))
! sst_input = dum

!-----------------------------------------------------------------------
! snow-free albedo
!-----------------------------------------------------------------------

 allocate(data_input%snow_free_albedo(imdl_input,jmdl_input))
 read(9, iostat=istat, err=9000, end=9000) dum
 data_input%snow_free_albedo = dum

 read(9, iostat=istat, err=9000, end=9000) wbd, sbd, tlm0d, tph0d, dlmd, dphd

!-----------------------------------------------------------------------
! veg type (usgs??)
!-----------------------------------------------------------------------

 allocate(data_input%veg_type(imdl_input,jmdl_input))
 read(9, iostat=istat, err=9000, end=9000) idum
 data_input%veg_type = idum

!-----------------------------------------------------------------------
! soil type
!-----------------------------------------------------------------------

 allocate(data_input%soil_type(imdl_input,jmdl_input))
 read(9, iostat=istat, err=9000, end=9000) idum
 data_input%soil_type = idum

!-----------------------------------------------------------------------
! slope index
!-----------------------------------------------------------------------

 allocate(data_input%slope_type(imdl_input,jmdl_input))
 read(9, iostat=istat, err=9000, end=9000) idum
 data_input%slope_type = idum
 
 deallocate (idum)

!-----------------------------------------------------------------------
! greenness
!-----------------------------------------------------------------------

 allocate(data_input%greenfrc(imdl_input,jmdl_input))
 read(9, iostat=istat, err=9000, end=9000) dum
 data_input%greenfrc = dum

 allocate (psfc(imdl_input,jmdl_input))

!-----------------------------------------------------------------------
! surface pressure (pascals)
!-----------------------------------------------------------------------

 read(9, iostat=istat, err=9000, end=9000) psfc  ! psfc

!-----------------------------------------------------------------------
! orography
!-----------------------------------------------------------------------

 allocate(data_input%orog(imdl_input,jmdl_input)) 
 read(9, iostat=istat, err=9000, end=9000) dum 
 data_input%orog = dum

!-----------------------------------------------------------------------
! roughness
!-----------------------------------------------------------------------

 allocate(data_input%z0(imdl_input, jmdl_input))
 read(9, iostat=istat, err=9000, end=9000) dum 
 data_input%z0 = dum*100.0 ! interp code wants cm

 read(9, iostat=istat, err=9000, end=9000) dum ! akhs

!-----------------------------------------------------------------------
! skin temperature.  comes in as a potential temperature.
! convert to temperature.
!-----------------------------------------------------------------------

 allocate(data_input%skin_temp(imdl_input,jmdl_input))
 read(9, iostat=istat, err=9000, end=9000) dum 
 data_input%skin_temp = dum

 do j = 1, jmdl_input
   do i = 1, imdl_input

     exner = (100000. / psfc(i,j)) ** rovcp
     
     data_input%skin_temp(i,j) = data_input%skin_temp(i,j) / exner

   enddo
 enddo 

 deallocate (psfc)

 read(9, iostat=istat, err=9000, end=9000) dum ! qs

!-----------------------------------------------------------------------
! snow liquid equivalent (meters)
!-----------------------------------------------------------------------

 allocate(data_input%snow_liq_equiv(imdl_input,jmdl_input))
 read(9, iostat=istat, err=9000, end=9000) dum 
! data_input%snow_liq_equiv = dum
 data_input%snow_liq_equiv = dum * 1000.  ! interp routine assummes mm

!-----------------------------------------------------------------------
! snow depth (meters)
!-----------------------------------------------------------------------

 allocate(data_input%snow_depth(imdl_input,jmdl_input))
 read(9, iostat=istat, err=9000, end=9000) dum 
! data_input%snow_depth = dum
 data_input%snow_depth = dum * 1000. ! interp routine assume mm

!-----------------------------------------------------------------------
! soil moisture - total
!-----------------------------------------------------------------------

 allocate(data_input%soilm_tot(imdl_input,jmdl_input,nsoil_input))
 read(9, iostat=istat, err=9000, end=9000) dum3d 
 data_input%soilm_tot = dum3d

!cggg bad values of soilm in edas file?
 where (data_input%soilm_tot < 0.02) data_input%soilm_tot=0.02

!-----------------------------------------------------------------------
! canopy mc (meters)
!-----------------------------------------------------------------------

 allocate(data_input%canopy_mc(imdl_input,jmdl_input))
 read(9, iostat=istat, err=9000, end=9000) dum 
 data_input%canopy_mc = dum

!-----------------------------------------------------------------------
! soil temperature
!-----------------------------------------------------------------------

 allocate(data_input%soil_temp(imdl_input,jmdl_input,nsoil_input))
 read(9, iostat=istat, err=9000, end=9000) dum3d 
 data_input%soil_temp = dum3d

!-----------------------------------------------------------------------
! soil moisture - liq portion.
!-----------------------------------------------------------------------

 allocate(data_input%soilm_liq(imdl_input,jmdl_input,nsoil_input))
 read(9, iostat=istat, err=9000, end=9000) dum3d ! liq soilm
 data_input%soilm_liq = dum3d

!-----------------------------------------------------------------------
! albedo, including the effects of snow.
!-----------------------------------------------------------------------

 read(9, iostat=istat, err=9000, end=9000) dum   ! albedo, inc snow effects
! allocate(albedo_input(imdl_input,jmdl_input))
! albedo_input = dum

 deallocate (dum)
 deallocate (dum3d)

 return

!-----------------------------------------------------------------------
! error handling.
!-----------------------------------------------------------------------

9000 continue
 print*,"- ** ERROR READING EDAS DATA **"
 print*,"- ** ISTAT IS ", istat
 call w3tage("COLSTART_WRF")
 call mpi_abort(mpi_comm_world, 60, istat)

9100 continue
 print*,"- ** ERROR OPENING EDAS FILE **"
 print*,"- ** ISTAT IS ", istat
 call w3tage("COLSTART_WRF")
 call mpi_abort(mpi_comm_world, 61, istat)
 
 end subroutine read_input_file_edas

 end module read_data
