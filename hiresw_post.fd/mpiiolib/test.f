 program test

 use kinds, only             : i_llong

 use soil_utils, only           : calc_soil_parms

 implicit none

 include 'mpif.h'

 character*20               :: domain
 character*132, allocatable :: datestr_all(:)
 character*132, allocatable :: varname_all(:)
 character*200              :: filename

 integer, allocatable       :: domainend_all(:,:)
 integer, allocatable       :: start_block(:)
 integer, allocatable       :: end_block(:)
 integer, allocatable       :: start_byte(:)
 integer, allocatable       :: end_byte(:)

 integer(kind=i_llong), allocatable           :: file_offset(:)

 integer                    :: ierr, iret
 integer                    :: imdl_output
 integer                    :: i, j, k, n
 integer                    :: jmdl_output
 integer                    :: index
 integer                    :: iunit
 integer                    :: lev
 integer                    :: nrecs
 integer                    :: nsoil, nvert
 integer                    :: status(mpi_status_size)
 integer, allocatable       :: soil_type(:,:)

 real, allocatable          :: data(:,:)

 integer*4, allocatable     :: idum(:,:)
 real*4, allocatable        :: dum(:,:)
 real*4, allocatable        :: lsmask(:,:)
 real*4, allocatable        :: dum3d(:,:,:)
 real,   allocatable        :: psfc(:,:)
 real*4, allocatable        :: soilt(:,:,:)  ! ikj ???
 real, allocatable          :: soilt2(:,:,:)  ! ijk
 real*4                     :: pt, pdtop
 real*4                     :: dum1d
 real, allocatable          :: soilm_save(:,:)
 real                       :: term1, term2

 integer           :: igrid, jgrid, ipt(50), jpt(50), numpts
 INTEGER, PARAMETER :: K15=SELECTED_REAL_KIND(15)
 real(kind=k15)    :: dphd, dlmd, centlat, centlon, latpt(50), lonpt(50)

 integer :: ioutcount, istatus, datahandle

 integer, parameter   :: max_soil_types=16
 real :: smclow, smchigh, smcmax(max_soil_types), &
         beta(max_soil_types), psis(max_soil_types), &
         satdk(max_soil_types), smcref(max_soil_types), &
         smcwilt(max_soil_types), smcdry(max_soil_types)

 namelist /latlons/ latpt, lonpt

 data smclow  / 0.5/
 data smchigh  / 3.0/
 data smcmax  / 0.395, 0.421, 0.434, 0.476, 0.476, 0.439, &
                0.404, 0.464, 0.465, 0.406, 0.468, 0.457,  &
                0.464, -9.99,  0.20, 0.421 /
 data beta  / 4.05,  4.26,  4.74,  5.33,  5.33,  5.25,  &
              6.77,  8.72,  8.17, 10.73, 10.39, 11.55,   &
              5.25, -9.99,  4.05,  4.26/
 data psis  /0.0350, 0.0363, 0.1413, 0.7586, 0.7586, 0.3548, &
             0.1349, 0.6166, 0.2630, 0.0977, 0.3236, 0.4677,  &
             0.3548, -9.99,  0.0350, 0.0363/
 data satdk  /1.7600e-4, 1.4078e-5, 5.2304e-6, 2.8089e-6, 2.8089e-6, &
                3.3770e-6, 4.4518e-6, 2.0348e-6, 2.4464e-6, 7.2199e-6,&
                1.3444e-6, 9.7394e-7, 3.3770e-6,     -9.99, 1.4078e-5,&
                1.4078e-5/

 call calc_soil_parms(smclow, smchigh, smcmax, &
                      beta, satdk, psis, max_soil_types, &
                      smcref, smcwilt, smcdry)

 iunit = 33

! filename="/com/eta/prod/hiresw.20050119/westnmm.t06z.wrfinput_d01"
! filename="/ptmp/wx20er/holdegd.nmm/wrfout_d01_2005-06-16T00:00:00"
! filename="/ptmp/wx20gg/nmm/na12snmm.t00z.wrfrst_d01.2005061600"
! filename="/ptmp/wx20gg/junk/wrfrst.test"
!  filename="/com/nam/prod/nam.20050530/nam.t00z.restrt03.ges"
! filename="/ptmp/wx20gg/nmm/wrfout_d01_2005-06-16T00:00:00"
! filename="/ptmp/wx20gg/sfcprelim/wrfrst_d01_2005-03-03T06:00:00.outnew"
! filename="/ptmp/wx20gg/centnmm_ctl/fcst/wrfout_d01_2005-04-26T14:00:00"
! filename="/ptmp/wx20gg/centnmm_test/fcst/wrfrst_d01_2005-04-26T12:00:00"
! filename="/ptmp/wx20gg/centnmm_ctl/fcst/wrfinput_d01"

 call getenv('DOMAIN', domain)

 select case (trim(domain))

 case("launcher")
   igrid = 360
   jgrid = 575
   dphd  = 0.075
   dlmd  = 0.088
   centlat = 40.00
   centlon = 100.0
 case("east")
   igrid = 223
   jgrid = 501
   dphd  = 1./19.
   dlmd  = 24./449.
   centlat = 37.00
   centlon = 80.0
 case("central")
   igrid = 223
   jgrid = 501
   dphd  = 1./19.
   dlmd  = 24./449.
   centlat = 37.00
   centlon = 98.0
 case("west")
   igrid = 223
   jgrid = 501
   dphd  = 1./19.
   dlmd  = 24./449.
   centlat = 40.00
   centlon = 115.0
 case("alaska")
   igrid = 223
   jgrid = 501
   dphd  = 5./76.
   dlmd  = 1./15.
   centlat = 63.00
   centlon = 150.0
 case("full")
   igrid = 606
   jgrid = 1067
   dphd  = 40./553.
   dlmd  = 53./605.
   centlat = 50.00
   centlon = 111.0
 case("prx")
   igrid = 420
   jgrid = 769
   dphd  = 40./553.
   dlmd  = 53./605.
   centlat = 50.00
   centlon = 111.0
 case("nmmsref")
   igrid = 185
   jgrid = 315
   dphd  = 40./553.
   dlmd  = 53./605.
   centlat = 50.00
   centlon = 111.0
 case("nmm16km")
   igrid = 340
   jgrid = 563
   dphd  = .108
   dlmd  = .1025
   centlat = 50.00
   centlon = 111.0
 case("nmm16km_expanded")
   igrid = 375
   jgrid = 563
   dphd  = .108
   dlmd  = .1025
   centlat = 50.00
   centlon = 111.0
 case default
   print*,'-INVALID DOMAIN SELECTED'
   stop
 end select

 read(81,nml=latlons)

 numpts = 0

 do k = 1, 50
 if (latpt(k) < -99.) exit
 numpts = numpts+1
 call g2t2h(igrid,jgrid,dphd,dlmd,centlat,centlon,latpt(k),lonpt(k),ipt(k),jpt(k))
 if (ipt(k) < 1 .or. ipt(k) > igrid) then
   print*,"- SELECTED LAT/LON OF ",latpt(k),lonpt(k)," IS OUTSIDE BOUNDS OF GRID"
   stop
 endif
 if (jpt(k) < 1 .or. jpt(k) > jgrid) then
   print*,"- SELECTED LAT/LON OF ",latpt(k),lonpt(k)," IS OUTSIDE BOUNDS OF GRID"
   stop
 endif
 write((8+k),98) latpt(k), lonpt(k), ipt(k), jpt(k)
 enddo

 98 format("LAT= ",f6.2," LON= ",f7.2," I= ",i4," J= ",i4)

 call getenv('INFILE', filename)

 call count_recs_wrf_binary_file(iunit, filename, nrecs)

 print*,'- FILE CONTAINS ',nrecs, ' RECORDS'

 allocate (datestr_all(nrecs))
 allocate (varname_all(nrecs))
 allocate (domainend_all(3,nrecs))
 allocate (start_block(nrecs))
 allocate (end_block(nrecs))
 allocate (start_byte(nrecs))
 allocate (end_byte(nrecs))
 allocate (file_offset(nrecs))

 call inventory_wrf_binary_file(iunit, filename, nrecs,  &
                                datestr_all,varname_all,domainend_all, &
                                start_block,end_block,start_byte,end_byte,file_offset)

 call retrieve_index(index, 'IVGTYP', varname_all, nrecs, iret)

 if (iret /= 0) then
   print*,"- IVGTYP not found in file"
   stop
 end if

 print*,'index is ',index
 print*,'start_byte is ',start_byte(index)
 print*,'end_byte is ',end_byte(index)
 print*,'file_offset is ',file_offset(index)
 print*,'datestr_all is ',datestr_all(index)
 print*,'varname_all is ',varname_all(index)
 print*,'domainend_all is ',domainend_all(:,index)

 imdl_output = domainend_all(1,index)
 jmdl_output = domainend_all(2,index)

 if (igrid /= imdl_output .or. jgrid /= jmdl_output)then
   print*,"- SPECIFIED GRID DIMENSIONS DO NOT MATCH THOSE IN FILE."
   print*,"- DID YOU SELECT THE CORRECT DOMAIN?"
   stop
 end if

 call mpi_init(ierr)

 if (ierr /= 0) then
   print*,"Error initializing mpi"
   stop
 end if

 call mpi_file_open(mpi_comm_world, filename, mpi_mode_rdonly, &
                    mpi_info_null, iunit, ierr)

 print*,'iunit for open is ',iunit

 if (ierr /= 0) then
   print*,"Error opening file"
   stop
 end if

!------------------------
! veg type
!------------------------

 allocate(idum(imdl_output,jmdl_output))

 idum = 0

 print*,'- GET VEG TYPE'

 call mpi_file_read_at(iunit,file_offset(index+1),idum,(imdl_output*jmdl_output), &
                        mpi_integer4, mpi_status_ignore, ierr)

 if (ierr /= 0) then
   print*,"Error reading file"
   stop
 end if

 do n = 1, numpts
 write((8+n),100) idum(ipt(n),jpt(n))
 enddo
 100 format("- VEGETATION TYPE IS ",i3)

 allocate (data(imdl_output,jmdl_output))
 data = float(idum)
 lev = 0  ! surface field

 call gribit(imdl_output,jmdl_output,225,0,lev,data)

!------------------------
! soil type
!------------------------

 idum = 0

 call retrieve_index(index, 'ISLTYP', varname_all, nrecs, iret)

 if (iret == 0) then

 print*,"- GET SOIL TYPE"
 call mpi_file_read_at(iunit,file_offset(index+1),idum,(imdl_output*jmdl_output), &
                        mpi_integer4, mpi_status_ignore, ierr)

 if (ierr /= 0) then
   print*,"Error reading file"
   stop
 end if

 do n = 1, numpts
 write((8+n),101) idum(ipt(n),jpt(n))
 enddo
 101 format("- SOIL TYPE IS ",i3)

 data = float(idum)
 lev = 0
 call gribit(imdl_output,jmdl_output,224,0,lev,data)

 allocate(soil_type(imdl_output,jmdl_output))
 soil_type = idum

 else
   print*,"- *** ISLTYP NOT FOUND"
   stop
 end if

!------------------------
! slope type
!------------------------

 idum = 0

 call retrieve_index(index, 'ISLOPE', varname_all, nrecs, iret)

 if (iret == 0) then

 print*,'- GET SLOPE TYPE'
 call mpi_file_read_at(iunit,file_offset(index+1),idum,(imdl_output*jmdl_output), &
                        mpi_integer4, mpi_status_ignore, ierr)

 if (ierr /= 0) then
   print*,"Error reading file"
   stop
 end if

 do n = 1, numpts
 write((8+n),102) idum(ipt(n),jpt(n))
 enddo
 102 format("- SLOPE TYPE IS ",i3)

 data = float(idum)
 lev = 0
 call gribit(imdl_output,jmdl_output,222,0,lev,data)

 else
   print*,"*** ISLOPE NOT FOUND"
   stop
 end if

!------------------------
! fis (geopotential)
!------------------------

 allocate(dum(imdl_output,jmdl_output))

 dum = 0

 call retrieve_index(index, 'FIS', varname_all, nrecs, iret)

 if (iret == 0) then

 print*,'- GET GEOPOTENTIAL'
 call mpi_file_read_at(iunit,file_offset(index+1),dum,(imdl_output*jmdl_output), &
                        mpi_real4, mpi_status_ignore, ierr)

 if (ierr /= 0) then
   print*,"Error reading file"
   stop
 end if

 do n = 1, numpts
 write((8+n),103) (dum(ipt(n),jpt(n))/9.81)
 enddo
 103 format("- TERRAIN HEIGHT IS ",f6.1)

 data = dum / 9.81
 lev = 0
 call gribit(imdl_output,jmdl_output,8,3,lev,data)

 else
   print*,"*** FIS NOT FOUND"
   stop
 end if

!------------------------
! greenness
!------------------------

 dum = 0

 call retrieve_index(index, 'VEGFRC', varname_all, nrecs, iret)

 if (iret == 0) then

 print*,'- GET GREENNESS'
 call mpi_file_read_at(iunit,file_offset(index+1),dum,(imdl_output*jmdl_output), &
                        mpi_real4, mpi_status_ignore, ierr)

 if (ierr /= 0) then
   print*,"Error reading file"
   stop
 end if

 do n = 1, numpts
 write((8+n),104) dum(ipt(n),jpt(n))
 enddo
 104 format("- GREENNESS IS ",f4.2)

 data = dum
 lev = 0
 call gribit(imdl_output,jmdl_output,87,2,lev,data)

 else
   print*,"*** VEGFRC NOT FOUND"
   stop
 end if

!------------------------------------------------------------------------
! surface pressure using matt's calculation.
! PINT not normally carried in wrfinput_d01 (coldstart NMM) file.
! Reconstruct using PT+PDTOP+PD = P_SURFCE
!------------------------------------------------------------------------

 print*,'- GET PSFC'
 call retrieve_index(index, 'PT', varname_all, nrecs, iret)

 if (iret /= 0) then
   print*,"*** PT NOT FOUND, CANT CALC PSFC"
   goto 45
 end if

 call mpi_file_read_at(iunit, file_offset(index+1), pt,   &
                      1, mpi_real4, mpi_status_ignore, ierr)

 print*,'pt is ',pt

 call retrieve_index(index, 'PDTOP', varname_all, nrecs, iret)

 if (iret /= 0) then
   print*,"*** PDTOP NOT FOUND, CANT CALC PSFC"
   goto 45
 end if

 call mpi_file_read_at(iunit, file_offset(index+1), pdtop,   &
                      1, mpi_real4, mpi_status_ignore, ierr)

 print*,'pdtop is ',pdtop

 call retrieve_index(index, 'PD', varname_all, nrecs, iret)

 if (iret /= 0) then
   print*,"*** PD NOT FOUND, CANT CALC PSFC"
   goto 45
 end if

 call mpi_file_read_at(iunit, file_offset(index+1), dum,   &
                       (imdl_output*jmdl_output),         &
                      mpi_real4, mpi_status_ignore, ierr)

 allocate (psfc(imdl_output,jmdl_output))

 psfc = pt + pdtop + dum
 lev = 0
 call gribit(imdl_output,jmdl_output,1,2,lev,psfc)

 do n = 1, numpts
 write((8+n),105) psfc(ipt(n),jpt(n))
 enddo
 105 format("- PSFC IS ",f8.0)

 45 continue

!------------------------
! z0
!------------------------

 dum = 0

 call retrieve_index(index, 'Z0', varname_all, nrecs, iret)

 if (iret == 0) then

 print*,'- GET Z0'
 call mpi_file_read_at(iunit,file_offset(index+1),dum,(imdl_output*jmdl_output), &
                        mpi_real4, mpi_status_ignore, ierr)

 if (ierr /= 0) then
   print*,"Error reading file"
   stop
 end if

 do n = 1, numpts
 write((8+n),106) dum(ipt(n),jpt(n))
 enddo
 106 format("- ROUGHNESS IS ",f10.6)

 data = dum
 lev = 0
 call gribit(imdl_output,jmdl_output,83,6,lev,data)

 else
   print*,"*** Z0 NOT FOUND"
   stop
 end if

!------------------------
! pblh
!------------------------

 dum = 0

 call retrieve_index(index, 'PBLH', varname_all, nrecs, iret)

 if (iret == 0) then

 print*,'- GET PBLH'
 call mpi_file_read_at(iunit,file_offset(index+1),dum,(imdl_output*jmdl_output), &
                        mpi_real4, mpi_status_ignore, ierr)

 if (ierr /= 0) then
   print*,"Error reading file"
 end if

 do n = 1, numpts
 write((8+n),200) dum(ipt(n),jpt(n))
 enddo
 200 format("- PBL HEIGHT IS ",f7.1)

 data = dum
 lev = 0
 call gribit(imdl_output,jmdl_output,221,1,lev,data)

 else
   print*,"*** PBLH NOT FOUND"
 end if

!------------------------
! ustar
!------------------------

 dum = 0

 call retrieve_index(index, 'USTAR', varname_all, nrecs, iret)

 if (iret == 0) then

 print*,'- GET USTAR'
 call mpi_file_read_at(iunit,file_offset(index+1),dum,(imdl_output*jmdl_output), &
                        mpi_real4, mpi_status_ignore, ierr)

 if (ierr /= 0) then
   print*,"Error reading file"
 end if

 do n = 1, numpts
 write((8+n),201) dum(ipt(n),jpt(n))
 enddo
 201 format("- USTAR IS ",f7.3)

 data = dum
 lev = 0
 call gribit(imdl_output,jmdl_output,253,2,lev,data)

 else
   print*,"*** USTAR NOT FOUND"
 end if

!------------------------
! sfcexc
!------------------------

 dum = 0

 call retrieve_index(index, 'SFCEXC', varname_all, nrecs, iret)

 if (iret == 0) then

 print*,'- GET SFCEXC'
 call mpi_file_read_at(iunit,file_offset(index+1),dum,(imdl_output*jmdl_output), &
                        mpi_real4, mpi_status_ignore, ierr)

 if (ierr /= 0) then
   print*,"Error reading file"
 end if

 do n = 1, numpts
 write((8+n),203) dum(ipt(n),jpt(n))
 enddo
 203 format("- SFCEXC IS ",f8.5)

 data = dum
 lev = 0
 call gribit(imdl_output,jmdl_output,208,5,lev,data)

 else
   print*,"*** SFCEXC NOT FOUND"
 end if

!------------------------
! GRNFLX
!------------------------

 dum = 0

 call retrieve_index(index, 'GRNFLX', varname_all, nrecs, iret)

 if (iret == 0) then

 print*,'- GET GRNFLX'
 call mpi_file_read_at(iunit,file_offset(index+1),dum,(imdl_output*jmdl_output), &
                        mpi_real4, mpi_status_ignore, ierr)

 if (ierr /= 0) then
   print*,"Error reading file"
 end if

 do n = 1, numpts
 write((8+n),206) dum(ipt(n),jpt(n))
 enddo
 206 format("- GRNFLX IS ",f10.3)

 data = dum
 lev = 0
 call gribit(imdl_output,jmdl_output,155,3,lev,data)

 else
   print*,"*** GRNFLX NOT FOUND"
 end if

!------------------------
! TWBS (instantaneous sensible heat flux
!------------------------

 dum = 0

 call retrieve_index(index, 'TWBS', varname_all, nrecs, iret)

 if (iret == 0) then

 print*,'- GET TWBS'
 call mpi_file_read_at(iunit,file_offset(index+1),dum,(imdl_output*jmdl_output), &
                        mpi_real4, mpi_status_ignore, ierr)

 if (ierr /= 0) then
   print*,"Error reading file"
 end if

 do n = 1, numpts
 write((8+n),208) dum(ipt(n),jpt(n))
 enddo
 208 format("- INSTANTANEOUS SENSIBLE HEAT FLUX IS ",f10.3)

 data = dum
 lev = 0
 call gribit(imdl_output,jmdl_output,122,3,lev,data)

 else
   print*,"*** TWBS NOT FOUND"
 end if

!------------------------
! QWBS (instantaneous latent heat flux
!------------------------

 dum = 0

 call retrieve_index(index, 'QWBS', varname_all, nrecs, iret)

 if (iret == 0) then

 print*,'- GET QWBS'
 call mpi_file_read_at(iunit,file_offset(index+1),dum,(imdl_output*jmdl_output), &
                        mpi_real4, mpi_status_ignore, ierr)

 if (ierr /= 0) then
   print*,"Error reading file"
 end if

 do n = 1, numpts
 write((8+n),209) dum(ipt(n),jpt(n))
 enddo
 209 format("- INSTANTANEOUS LATENT HEAT FLUX IS ",f10.3)

 data = dum
 lev = 0
 call gribit(imdl_output,jmdl_output,121,3,lev,data)

 else
   print*,"*** QWBS NOT FOUND"
 end if

!------------------------
! zeff (4 components)
!------------------------

 dum = 0

 call retrieve_index(index, 'ZEFF1', varname_all, nrecs, iret)

 if (iret == 0) then
 print*,'- GET ZEFF1'
 call mpi_file_read_at(iunit,file_offset(index+1),dum,(imdl_output*jmdl_output), &
                        mpi_real4, mpi_status_ignore, ierr)

 if (ierr /= 0) then
   print*,"Error reading file"
   stop
 end if

 do n = 1, numpts
 write((8+n),136) dum(ipt(n),jpt(n))
 enddo
 136 format("- ZEFF1 IS ",f10.6)

 data = dum
 lev = 6
 call gribit(imdl_output,jmdl_output,83,3,lev,data)
 else
   print*,"*** ZEFF1 NOT FOUND"
 end if

 dum = 0

 call retrieve_index(index, 'ZEFF2', varname_all, nrecs, iret)

 if (iret == 0) then

 print*,'- GET ZEFF2'
 call mpi_file_read_at(iunit,file_offset(index+1),dum,(imdl_output*jmdl_output), &
                        mpi_real4, mpi_status_ignore, ierr)

 if (ierr /= 0) then
   print*,"Error reading file"
   stop
 end if

 do n = 1, numpts
 write((8+n),137) dum(ipt(n),jpt(n))
 enddo
 137 format("- ZEFF2 IS ",f10.6)

 data = dum
 lev = 7
 call gribit(imdl_output,jmdl_output,83,3,lev,data)
 else
   print*,"*** ZEFF2 NOT FOUND"
 end if
 
 dum = 0

 call retrieve_index(index, 'ZEFF3', varname_all, nrecs, iret)

 if (iret == 0) then
 print*,'- GET ZEFF3'
 call mpi_file_read_at(iunit,file_offset(index+1),dum,(imdl_output*jmdl_output), &
                        mpi_real4, mpi_status_ignore, ierr)

 if (ierr /= 0) then
   print*,"Error reading file"
   stop
 end if

 do n = 1, numpts
 write((8+n),138) dum(ipt(n),jpt(n))
 enddo
 138 format("- ZEFF3 IS ",f10.6)

 data = dum
 lev = 8
 call gribit(imdl_output,jmdl_output,83,3,lev,data)
 else
   print*,"*** ZEFF3 NOT FOUND"
 end if

 dum = 0

 call retrieve_index(index, 'ZEFF4', varname_all, nrecs, iret)

 if (iret == 0) then

 print*,'- GET ZEFF4'
 call mpi_file_read_at(iunit,file_offset(index+1),dum,(imdl_output*jmdl_output), &
                        mpi_real4, mpi_status_ignore, ierr)

 if (ierr /= 0) then
   print*,"Error reading file"
   stop
 end if

 do n = 1, numpts
 write((8+n),139) dum(ipt(n),jpt(n))
 enddo
 139 format("- ZEFF4 IS ",f10.6)

 data = dum
 lev = 9
 call gribit(imdl_output,jmdl_output,83,3,lev,data)
 else
   print*,"*** ZEFF4 NOT FOUND"
 end if

!------------------------
! z0base
!------------------------

 dum = 0

 print*,'- GET Z0BASE'
 call retrieve_index(index, 'Z0BASE', varname_all, nrecs, iret)

 if (iret == 0) then
 call mpi_file_read_at(iunit,file_offset(index+1),dum,(imdl_output*jmdl_output), &
                        mpi_real4, mpi_status_ignore, ierr)

 if (ierr /= 0) then
   print*,"Error reading file"
   stop
 end if

 do n = 1, numpts
 write((8+n),159) dum(ipt(n),jpt(n))
 enddo
 159 format("- Z0BASE IS ",f10.6)

 data = dum

 lev = 0
 call gribit(imdl_output,jmdl_output,9,6,lev,data)
 else
   print*,"*** Z0BASE NOT FOUND"
 end if

!------------------------
! skint ?
!------------------------

 dum = 0

 print*,'- GET SKIN THETA'
 call retrieve_index(index, 'THS', varname_all, nrecs, iret)

 if (iret == 0) then
 call mpi_file_read_at(iunit,file_offset(index+1),dum,(imdl_output*jmdl_output), &
                        mpi_real4, mpi_status_ignore, ierr)

 if (ierr /= 0) then
   print*,"Error reading file"
   stop
 end if

! data = dum / ( (1.e5 / psfc) ** .286 )  ! convert to skin temperature
 data = dum 

 do n = 1, numpts
 write((8+n),107) data(ipt(n),jpt(n))
 enddo
 107 format("- SKIN THETA IS ",f6.2)

 deallocate (psfc)
 lev = 0
 call gribit(imdl_output,jmdl_output,13,2,lev,data)

 else
   print*,"*** THS NOT FOUND"
   stop
 end if

!------------------------
! skint
!------------------------

 dum = 0

 print*,'- GET SKIN TEMPERATURE - TSK'
 call retrieve_index(index, 'TSK', varname_all, nrecs, iret)

 if (iret == 0) then
 call mpi_file_read_at(iunit,file_offset(index+1),dum,(imdl_output*jmdl_output), &
                        mpi_real4, mpi_status_ignore, ierr)

 if (ierr /= 0) then
   print*,"Error reading file"
   stop
 end if

 do n = 1, numpts
 write((8+n),108) dum(ipt(n),jpt(n))
 enddo
 108 format("- SKIN TEMPERATURE - TSK IS ",f6.2)

 data = dum
 lev = 0
 call gribit(imdl_output,jmdl_output,11,2,lev,data)

 else
   print*,"*** TSK NOT FOUND"
 end if

!------------------------
! sst
!------------------------

 dum = 0

 print*,'- GET SST'
 call retrieve_index(index, 'SST', varname_all, nrecs, iret)

 if (iret == 0) then
 call mpi_file_read_at(iunit,file_offset(index+1),dum,(imdl_output*jmdl_output), &
                        mpi_real4, mpi_status_ignore, ierr)

 if (ierr /= 0) then
   print*,"Error reading file"
   stop
 end if

 do n = 1, numpts
 write((8+n),218) dum(ipt(n),jpt(n))
 enddo
 218 format("- SST IS ",f6.2)

 data = dum
 lev = 10
 call gribit(imdl_output,jmdl_output,11,2,lev,data)

 else
   print*,"*** SST NOT FOUND"
 end if

!------------------------
! lats ?
!------------------------

 dum = 0

 call retrieve_index(index, 'GLAT', varname_all, nrecs, iret)

 if (iret == 0) then

 call mpi_file_read_at(iunit,file_offset(index+1),dum,(imdl_output*jmdl_output), &
                        mpi_real4, mpi_status_ignore, ierr)

 if (ierr /= 0) then
   print*,"Error reading file"
   stop
 end if

 data = dum*180./3.141592654
 lev = 0
 call gribit(imdl_output,jmdl_output,176,5,lev,data)

 else
   print*,"*** GLAT NOT FOUND"
   stop
 end if

!------------------------
! lons ?
!------------------------

 dum = 0

 call retrieve_index(index, 'GLON', varname_all, nrecs, iret)

 if (iret == 0) then
 call mpi_file_read_at(iunit,file_offset(index+1),dum,(imdl_output*jmdl_output), &
                        mpi_real4, mpi_status_ignore, ierr)

 if (ierr /= 0) then
   print*,"Error reading file"
   stop
 end if

 data = dum*180./3.141592654
 lev = 0
 call gribit(imdl_output,jmdl_output,177,5,lev,data)
 else
   print*,"*** GLON NOT FOUND"
   stop
 end if

!------------------------
! sice (ice mask)
!------------------------

 dum = 0

 print*,'- GET ICE MASK'
 call retrieve_index(index, 'SICE', varname_all, nrecs, iret)

 if (iret == 0) then

 call mpi_file_read_at(iunit,file_offset(index+1),dum,(imdl_output*jmdl_output), &
                        mpi_real4, mpi_status_ignore, ierr)

 if (ierr /= 0) then
   print*,"Error reading file"
   stop
 end if

 do n = 1, numpts
 write((8+n),109) dum(ipt(n),jpt(n))
 enddo
 109 format("- ICEMASK IS ",f3.1)

 data = dum
 lev = 0
 call gribit(imdl_output,jmdl_output,91,2,lev,data)

 else
   print*,"*** SICE NOT FOUND"
   stop
 end if

 allocate (lsmask(imdl_output,jmdl_output))
 lsmask = dum   ! 0 - not ice, 1 - ice

!------------------------
! sm (land mask)
!------------------------

 dum = 0

 print*,"- GET LAND MASK"
 call retrieve_index(index, 'SM', varname_all, nrecs, iret)

 if (iret == 0) then

 call mpi_file_read_at(iunit,file_offset(index+1),dum,(imdl_output*jmdl_output), &
                        mpi_real4, mpi_status_ignore, ierr)

 if (ierr /= 0) then
   print*,"Error reading file"
   stop
 end if

 do n = 1, numpts
 write((8+n),110) dum(ipt(n),jpt(n))
 enddo
 110 format("- LANDMASK IS ",f3.1)

 data = dum

! where (dum == 0. .and. lsmask == 0.0) data = 1.0
 lev = 0
 call gribit(imdl_output,jmdl_output,81,2,lev,data)

 else
   print*,"*** SM NOT FOUND"
   stop
 end if

!------------------------
! canopy mc
!------------------------

 dum = 0

 print*,'- GET CMC'
 call retrieve_index(index, 'CMC', varname_all, nrecs, iret)

 if (iret == 0) then
 call mpi_file_read_at(iunit,file_offset(index+1),dum,(imdl_output*jmdl_output), &
                        mpi_real4, mpi_status_ignore, ierr)

 if (ierr /= 0) then
   print*,"Error reading file"
   stop
 end if

 do n = 1, numpts
 write((8+n),111) dum(ipt(n),jpt(n))
 enddo
 111 format("- CANOPY MC IS ",f8.6)

 data = dum
 lev = 0
 call gribit(imdl_output,jmdl_output,223,5,lev,data)

 else
   print*,"*** CMC NOT FOUND"
   stop
 end if

!------------------------
! albedo
!------------------------

 dum = 0

 print*,'- GET ALBEDO'
 call retrieve_index(index, 'ALBEDO', varname_all, nrecs, iret)

 if (iret == 0) then

 call mpi_file_read_at(iunit,file_offset(index+1),dum,(imdl_output*jmdl_output), &
                        mpi_real4, mpi_status_ignore, ierr)

 if (ierr /= 0) then
   print*,"Error reading file"
   stop
 end if

 do n = 1, numpts
 write((8+n),112) dum(ipt(n),jpt(n))
 enddo
 112 format("- ALBEDO IS ",f4.2)

 data = dum
 lev = 0
 call gribit(imdl_output,jmdl_output,84,2,lev,data)

 else
   print*,"*** ALBEDO NOT FOUND"
   stop
 end if

!------------------------
! snow-free albedo
!------------------------

 dum = 0

 print*,'- GET BASE ALBEDO'
 call retrieve_index(index, 'ALBASE', varname_all, nrecs, iret)

 if (iret == 0) then

 call mpi_file_read_at(iunit,file_offset(index+1),dum,(imdl_output*jmdl_output), &
                        mpi_real4, mpi_status_ignore, ierr)

 if (ierr /= 0) then
   print*,"Error reading file"
   stop
 end if

 do n = 1, numpts
 write((8+n),113) dum(ipt(n),jpt(n))
 enddo
 113 format("- BASE ALBEDO IS ",f4.2)

 data = dum
 lev = 0
 call gribit(imdl_output,jmdl_output,170,2,lev,data)

 else
   print*,"*** ALBASE NOT FOUND"
   stop
 end if

!------------------------
! snow depth
!------------------------

 dum = 0

 print*,'- GET SNOW DEPTH'
 call retrieve_index(index, 'SI', varname_all, nrecs, iret)
! call retrieve_index(index, 'SNOW', varname_all, nrecs, iret)

 if (iret == 0) then
 call mpi_file_read_at(iunit,file_offset(index+1),dum,(imdl_output*jmdl_output), &
                        mpi_real4, mpi_status_ignore, ierr)

 if (ierr /= 0) then
   print*,"Error reading file"
   stop
 end if

 do n = 1, numpts
 write((8+n),114) dum(ipt(n),jpt(n))
 enddo
 114 format("- SNOW DEPTH IS ",f6.2)

 data = dum
 lev = 0
 call gribit(imdl_output,jmdl_output,66,4,lev,data)
 else
   print*,"*** SI NOT FOUND"
 end if

!------------------------------------
! snow water equivalent depth
!------------------------------------

 dum = 0

 print*,'- GET SNOW LIQ EQUIV'
 call retrieve_index(index, 'SNO', varname_all, nrecs, iret)
! call retrieve_index(index, 'WEASD', varname_all, nrecs, iret)

 if (iret == 0) then
 call mpi_file_read_at(iunit,file_offset(index+1),dum,(imdl_output*jmdl_output), &
                        mpi_real4, mpi_status_ignore, ierr)

 if (ierr /= 0) then
   print*,"Error reading file"
   stop
 end if

 do n = 1, numpts
 write((8+n),115) dum(ipt(n),jpt(n))
 enddo
 115 format("- SNOW LIQUID EQUIV IS ",f6.2)

 data = dum
 lev = 0
 call gribit(imdl_output,jmdl_output,65,4,lev,data)
 else
   print*,"*** SNO NOT FOUND"
   stop
 end if

!------------------------
! max snow albedo
!------------------------

 dum = 0

 print*,'- GET MAX SNOW ALB'
 call retrieve_index(index, 'MXSNAL', varname_all, nrecs, iret)

 if (iret /=0 ) then
   print*,"*** MXSNAL NOT FOUND"
   goto 799
 end if

 call mpi_file_read_at(iunit,file_offset(index+1),dum,(imdl_output*jmdl_output), &
                        mpi_real4, mpi_status_ignore, ierr)

 if (ierr /= 0) then
   print*,"Error reading file"
   stop
 end if

 do n = 1, numpts
 write((8+n),116) dum(ipt(n),jpt(n))
 enddo
 116 format("- MAX SNOW ALBEDO IS ",f4.2)

 data = dum
 lev = 0
 call gribit(imdl_output,jmdl_output,159,2,lev,data)

 799 continue

!------------------------
! substrate t
!------------------------

 dum = 0

 print*,'- GET TBOT'
! call retrieve_index(index, 'TG', varname_all, nrecs, iret)
 call retrieve_index(index, 'TGROUND', varname_all, nrecs, iret)

 if (iret == 0) then
 call mpi_file_read_at(iunit,file_offset(index+1),dum,(imdl_output*jmdl_output), &
                        mpi_real4, mpi_status_ignore, ierr)

 if (ierr /= 0) then
   print*,"Error reading file"
   stop
 end if

 do n = 1, numpts
 write((8+n),117) dum(ipt(n),jpt(n))
 enddo
 117 format("- SUBSTRATE TEMP IS ",f6.2)

 data = dum

 lev = 5
 call gribit(imdl_output,jmdl_output,85,2,lev,data)
 else
   print*,"*** TGROUND NOT FOUND"
   stop
 end if

!--------------------------------------------------------------------
! soil t
!--------------------------------------------------------------------

 print*,'- GET SOIL T'
 call retrieve_index(index, 'STC', varname_all, nrecs, iret)

 if (iret == 0) then

 nsoil       = domainend_all(2,index)

 allocate (dum3d(imdl_output,nsoil,jmdl_output))
 allocate (soilt2(imdl_output,jmdl_output,nsoil))

 soilt2 = 0.
 dum3d  = 0.

 call mpi_file_read_at(iunit,file_offset(index+1),dum3d,  &
                      (nsoil*imdl_output*jmdl_output), &
                       mpi_real4, mpi_status_ignore, ierr)

 if (ierr /= 0) then
   print*,"Error reading file"
   stop
 end if

 do n = 1, numpts
 do k = 1, nsoil
 write((8+n),118) dum3d(ipt(n),k,jpt(n)), k
 enddo
 enddo
 118 format("- SOIL TEMP IS ",f6.2," AT LAYER ", i2)

 do j = 1, jmdl_output
 do k = 1, nsoil
   soilt2(:,j,k) = dum3d(:,k,j)
 enddo
 enddo

 lev =1
 call gribit(imdl_output,jmdl_output,85,2,lev,soilt2(:,:,1))
 lev =2
 call gribit(imdl_output,jmdl_output,85,2,lev,soilt2(:,:,2))
 lev =3
 call gribit(imdl_output,jmdl_output,85,2,lev,soilt2(:,:,3))
 lev =4
 call gribit(imdl_output,jmdl_output,85,2,lev,soilt2(:,:,4))
 else
   print*,"*** STC NOT FOUND"
 end if

!--------------------------------------------------------------------
! soilm - tot
!--------------------------------------------------------------------

 print*,'- GET TOTAL SOILM'
 call retrieve_index(index, 'SMC', varname_all, nrecs, iret)

 if (iret == 0) then

 soilt2 = 0.
 dum3d  = 0.

 call mpi_file_read_at(iunit,file_offset(index+1),dum3d,  &
                      (nsoil*imdl_output*jmdl_output), &
                       mpi_real4, mpi_status_ignore, ierr)

 if (ierr /= 0) then
   print*,"Error reading file"
   stop
 end if

 do n = 1, numpts
 do k = 1, nsoil
 write((8+n),119) dum3d(ipt(n),k,jpt(n)), k
 enddo
 enddo
 119 format("- TOTAL SOIL MOISTURE IS ",f6.3," AT LAYER ", i2)

 print*,'soilm tot ',maxval(dum3d),minval(dum3d)

 do j = 1, jmdl_output
 do k = 1, nsoil
   soilt2(:,j,k) = dum3d(:,k,j)
 enddo
 enddo
 
 lev =1
 call gribit(imdl_output,jmdl_output,144,3,lev,soilt2(:,:,1))
 lev = 2
 call gribit(imdl_output,jmdl_output,144,3,lev,soilt2(:,:,2))
 lev = 3
 call gribit(imdl_output,jmdl_output,144,3,lev,soilt2(:,:,3))
 lev =4
 call gribit(imdl_output,jmdl_output,144,3,lev,soilt2(:,:,4))


 allocate(soilm_save(imdl_output,jmdl_output))

 soilm_save=(0.1*soilt2(:,:,1))+(0.3*soilt2(:,:,2))+(0.6*soilt2(:,:,3))

 do j=1,jmdl_output
 do i=1,imdl_output
   if (soil_type(i,j) /=14 .and.lsmask(i,j)<0.99) then
      term1=smcmax(soil_type(i,j))-soilm_save(i,j)
      term2=smcmax(soil_type(i,j))-smcwilt(soil_type(i,j))
      soilm_save(i,j)= 1.0 - term1/term2
   else
     soilm_save(i,j)=1.0
   endif
 enddo
 enddo

 data = soilm_save
 lev =0
 call gribit(imdl_output,jmdl_output,207,3,lev,data)

 do n = 1, numpts
 write((8+n),345) data(ipt(n),jpt(n))
 enddo
 345 format("- MOISTURE AVAILABILITY IS ",f6.3)

 else
   print*,"*** SMC NOT FOUND"
   stop
 end if

!--------------------------------------------------------------------
! soilm - liq
!--------------------------------------------------------------------

 print*,'- GET LIQ SOILM'
 call retrieve_index(index, 'SH2O', varname_all, nrecs, iret)

 if (iret == 0) then

! soilt2 = 0.
 dum3d  = 0.

 call mpi_file_read_at(iunit,file_offset(index+1),dum3d,  &
                      (nsoil*imdl_output*jmdl_output), &
                       mpi_real4, mpi_status_ignore, ierr)

 if (ierr /= 0) then
   print*,"Error reading file"
   stop
 end if

 do n = 1, numpts
 do k = 1, nsoil
 write((8+n),120) dum3d(ipt(n),k,jpt(n)), k
 enddo
 enddo
 120 format("- LIQUID SOIL MOISTURE IS ",f6.3," AT LAYER ", i2)

 do j = 1, jmdl_output
 do k = 1, nsoil
   soilt2(:,j,k) = dum3d(:,k,j)
! use this to output % liquid
!   soilt2(:,j,k) = dum3d(:,k,j) / soilt2(:,j,k)
 enddo
 enddo
 
 lev =1
 call gribit(imdl_output,jmdl_output,160,3,lev,soilt2(:,:,1))
 lev =2
 call gribit(imdl_output,jmdl_output,160,3,lev,soilt2(:,:,2))
 lev =3
 call gribit(imdl_output,jmdl_output,160,3,lev,soilt2(:,:,3))
 lev =4
 call gribit(imdl_output,jmdl_output,160,3,lev,soilt2(:,:,4))

 else
   print*,"*** SH20 NOT FOUND"
   stop
 endif

 deallocate (dum3d)

!--------------------------------------------------------------------
! temp - lowest sigma
!--------------------------------------------------------------------

 print*,'- AIR T'
 call retrieve_index(index, 'T', varname_all, nrecs, iret)

 nvert       = domainend_all(2,index)

 allocate(dum3d(imdl_output,nvert,jmdl_output))

 if (iret == 0) then

 dum3d  = 0.

 call mpi_file_read_at(iunit,file_offset(index+1),dum3d,  &
                      (nvert*imdl_output*jmdl_output), &
                       mpi_real4, mpi_status_ignore, ierr)

 if (ierr /= 0) then
   print*,"Error reading file"
   stop
 end if

 do n = 1, numpts
 write((8+n),220) dum3d(ipt(n),1,jpt(n))
 enddo
 220 format("- AIR TEMP IS ",f6.1," AT LOWEST SIGMA")

 do j = 1, jmdl_output
   data(:,j) = dum3d(:,1,j)
 enddo
 
 lev =6
 call gribit(imdl_output,jmdl_output,11,1,lev,data)

 else
   print*,"*** T NOT FOUND"
   stop
 endif

!--------------------------------------------------------------------
! u wind - lowest sigma
!--------------------------------------------------------------------

 print*,'- U WIND'
 call retrieve_index(index, 'U', varname_all, nrecs, iret)

 nvert       = domainend_all(2,index)

 if (iret == 0) then

 dum3d  = 0.

 call mpi_file_read_at(iunit,file_offset(index+1),dum3d,  &
                      (nvert*imdl_output*jmdl_output), &
                       mpi_real4, mpi_status_ignore, ierr)

 if (ierr /= 0) then
   print*,"Error reading file"
   stop
 end if

 do n = 1, numpts
 write((8+n),222) dum3d(ipt(n),1,jpt(n))
 enddo
 222 format("- U WIND IS ",f6.1," AT LOWEST SIGMA")

 do j = 1, jmdl_output
   data(:,j) = dum3d(:,1,j)
 enddo
 
 lev =6
 call gribit(imdl_output,jmdl_output,33,1,lev,data)

 else
   print*,"*** U NOT FOUND"
   stop
 endif

!--------------------------------------------------------------------
! v wind - lowest sigma
!--------------------------------------------------------------------

 print*,'- V WIND'
 call retrieve_index(index, 'V', varname_all, nrecs, iret)

 nvert       = domainend_all(2,index)

 if (iret == 0) then

 dum3d  = 0.

 call mpi_file_read_at(iunit,file_offset(index+1),dum3d,  &
                      (nvert*imdl_output*jmdl_output), &
                       mpi_real4, mpi_status_ignore, ierr)

 if (ierr /= 0) then
   print*,"Error reading file"
   stop
 end if

 do n = 1, numpts
 write((8+n),224) dum3d(ipt(n),1,jpt(n))
 enddo
 224 format("- V WIND IS ",f6.1," AT LOWEST SIGMA")

 do j = 1, jmdl_output
   data(:,j) = dum3d(:,1,j)
 enddo
 
 lev =6
 call gribit(imdl_output,jmdl_output,34,1,lev,data)

 else
   print*,"*** V NOT FOUND"
   stop
 endif

!--------------------------------------------------------------------
! q - lowest sigma
!--------------------------------------------------------------------

 print*,'- Q'
 call retrieve_index(index, 'Q', varname_all, nrecs, iret)

 nvert       = domainend_all(2,index)

 if (iret == 0) then

 dum3d  = 0.

 call mpi_file_read_at(iunit,file_offset(index+1),dum3d,  &
                      (nvert*imdl_output*jmdl_output), &
                       mpi_real4, mpi_status_ignore, ierr)

 if (ierr /= 0) then
   print*,"Error reading file"
   stop
 end if

 do n = 1, numpts
 write((8+n),221) dum3d(ipt(n),1,jpt(n))
 enddo
 221 format("- SPECIFIC HUMIDITY IS ",f10.7," AT LOWEST SIGMA")

 do j = 1, jmdl_output
   data(:,j) = dum3d(:,1,j)
 enddo
 
 lev =6
 call gribit(imdl_output,jmdl_output,51,5,lev,data)

 else
   print*,"*** Q NOT FOUND"
   stop
 endif

 deallocate (dum3d)

 call mpi_file_close(iunit, ierr)

 call mpi_finalize(ierr)

 print*,'normal termination'

 stop

 end program test

!-----------------------------------------------------------------------
      subroutine G2T2H(im,jm,dphd,dlmd,tph0d,tlm0d,glatd,glond,ii,jj)
!-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER, PARAMETER :: K15=SELECTED_REAL_KIND(15)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!*** LOCAL VARIABLES
!-----------------------------------------------------------------------
!
      INTEGER :: II,IMT,JJ,JMT,K,KROWS,NCOL,NROW, im,jm
!
      REAL(KIND=K15) :: COL,D1,D2,D2R,DLM,DLM1,DLM2,DPH,GLAT,GLON       &
     &                 ,ONE,PI,R2D,ROW,TLAT,TLAT1,TLAT2                 &
     &                 ,TLON,TLON1,TLON2,TPH0,TLM0,X,Y,Z, tph0d,tlm0d   &
     &                 ,dphd, dlmd, glatd, glond
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!***
!***  CONVERT FROM GEODETIC TO TRANSFORMED COORDINATES (DEGREES)
!***
!-----------------------------------------------------------------------
      ONE=1.
!      PI=DACOS(-ONE)
      PI=ACOS(-ONE)
      D2R=PI/180.
      R2D=1./D2R
      IMT=2*IM-1
      JMT=JM/2+1
      R2D=1./D2R
!
      GLAT=GLATD*D2R
      GLON=GLOND*D2R
      DPH=DPHD*D2R
      DLM=DLMD*D2R
      TPH0=TPH0D*D2R
      TLM0=TLM0D*D2R
!
      X=COS(TPH0)*COS(GLAT)*COS(GLON-TLM0)+SIN(TPH0)*SIN(GLAT)
      Y=-COS(GLAT)*SIN(GLON-TLM0)
      Z=COS(TPH0)*SIN(GLAT)-SIN(TPH0)*COS(GLAT)*COS(GLON-TLM0)
      TLAT=R2D*ATAN(Z/SQRT(X*X+Y*Y))
      TLON=R2D*ATAN(Y/X)
!
      WRITE(6,50)TLAT,TLON
   50 FORMAT(' TRANSFORMED LATITUDE IS',F8.3                            &
     &,               4X,'LONGITUDE IS',F8.3)
!
!-----------------------------------------------------------------------
!***  FIND THE K VALUE OF THE NEAREST H POINT
!-----------------------------------------------------------------------
!
      ROW=TLAT/DPHD+JMT
      COL=TLON/DLMD+IM
      NROW=INT(ROW)
      NCOL=INT(COL)
      TLAT=TLAT*D2R
      TLON=TLON*D2R
!-----------------------------------------------------------------------
!***
!***  FIRST CONSIDER THE SITUATION WHERE THE POINT X IS AT
!***
!***              V      H
!***
!***
!***                 X
!***              H      V
!***
!-----------------------------------------------------------------------
      main_test: IF(MOD(NROW,2)==1.AND.MOD(NCOL,2)==1.OR.               &
     &              MOD(NROW,2)==0.AND.MOD(NCOL,2)==0)THEN
!-----------------------------------------------------------------------
        TLAT1=(NROW-JMT)*DPH
        TLAT2=TLAT1+DPH
        TLON1=(NCOL-IM)*DLM
        TLON2=TLON1+DLM
        DLM1=TLON-TLON1
        DLM2=TLON-TLON2
        D1=ACOS(COS(TLAT)*COS(TLAT1)*COS(DLM1)+SIN(TLAT)*SIN(TLAT1))
        D2=ACOS(COS(TLAT)*COS(TLAT2)*COS(DLM2)+SIN(TLAT)*SIN(TLAT2))
!
        IF(D1>D2)THEN
          NROW=NROW+1
          NCOL=NCOL+1
        ENDIF
!
!-----------------------------------------------------------------------
!***
!***  NOW CONSIDER THE SITUATION WHERE THE POINT X IS AT
!***
!***              H      V
!***
!***
!***                 X
!***              V      H
!***
!-----------------------------------------------------------------------
      ELSE
!-----------------------------------------------------------------------
        TLAT1=(NROW+1-JMT)*DPH
        TLAT2=TLAT1-DPH
        TLON1=(NCOL-IM)*DLM
        TLON2=TLON1+DLM
        DLM1=TLON-TLON1
        DLM2=TLON-TLON2
        D1=ACOS(COS(TLAT)*COS(TLAT1)*COS(DLM1)+SIN(TLAT)*SIN(TLAT1))
        D2=ACOS(COS(TLAT)*COS(TLAT2)*COS(DLM2)+SIN(TLAT)*SIN(TLAT2))
!
        IF(D1<D2)THEN
          NROW=NROW+1
        ELSE
          NCOL=NCOL+1
        ENDIF
!-----------------------------------------------------------------------
      ENDIF main_test
!-----------------------------------------------------------------------
!
      JJ=NROW
      II=NCOL/2
      IF(MOD(JJ,2)==1)II=II+1
!
!-----------------------------------------------------------------------
!***  NOW WE CAN FIND THE K VALUE
!-----------------------------------------------------------------------
!
      KROWS=((NROW-1)/2)*IMT
      IF(MOD(NROW,2)==1)THEN
        K=KROWS+(NCOL+1)/2
      ELSE
        K=KROWS+IM+NCOL/2
      ENDIF
!
!-----------------------------------------------------------------------
!
      WRITE(6,100)GLATD,GLOND
      WRITE(6,101)II,JJ

      return

  100 FORMAT(' INPUT LAT,LON=',2F7.2)
  101 FORMAT(' NEAREST HEIGHT POINT AT I=',I3,' J=',I3)

      end subroutine g2t2h

!----------------------------------------------------------------------
! grib driver
!----------------------------------------------------------------------

 subroutine gribit(imdl,jmdl,parm_num,scale_fac,lev,data)

 include 'mpif.h'

 implicit none

 character*150              :: output_file

 integer, intent(in)        :: imdl, jmdl, parm_num, scale_fac

 integer                    :: iret
 integer, intent(in)        :: lev
 integer, parameter         :: lugb = 24    ! unit number of output grib file
 integer                    :: kpds(200)
 integer                    :: kgds(200)

 logical*1                  :: lbms(imdl,jmdl)

 logical, save              :: openit

 real                       :: data(imdl,jmdl)

 data openit /.true./

!----------------------------------------------------------------------
! set up pds section.  retain several settings from the
! input sst grib file because we are not creating data, but
! simply interpolating someone elses.  note: need to use the grid id
! from the model data, however.
!
! don't need to set the gds section.
! since the model grid is not changing, use the kgds array
! already determined in module model_grid.
!----------------------------------------------------------------------

 kpds = 0

 kpds(1)  = 7          ! center id
 kpds(2)  = 0  ! process id number. this determined from the
                       ! input data as we are simply interpolating
                       ! that data to a different grid
! kpds(3)  = 96  ! grid specified in gds  n amer grid
! kpds(3)  = 92  ! grid specified in gds  westus grid
! kpds(3) = 91    ! cent us
! kpds(3) = 192    ! eta32km
 kpds(3) = 255   ! 12km test grid
 kpds(4)  = 192        ! include gds and a bit map section
 kpds(5)  = parm_num   ! parameter number for temperature

 if (lev == 0) then  ! ground level
  kpds(6)  = 1          ! level - ground or water surface
  kpds(7)  = 0          ! height pressure of level
 elseif (lev == 1) then  ! soil layer 1 (0-10cm)
  kpds(6)  = 112         ! layer below ground
  kpds(7)  = 10          ! layer interfaces
 elseif (lev == 2) then  ! soil layer 2 (10-40cm)
  kpds(6)  = 112         ! layer below ground
  kpds(7)  = 2600        ! layer interfaces
 elseif (lev == 3) then  ! soil layer 3 (40-100cm)
  kpds(6)  = 112         ! layer below ground
  kpds(7)  = 10340       ! layer interfaces
 elseif (lev == 4) then  ! soil layer 4 (100-200cm)
  kpds(6)  = 112         ! layer below ground
  kpds(7)  = 25800       ! layer below ground
 elseif (lev == 5) then  ! substrate level (800cm)
  kpds(6)  = 111         ! level below ground
  kpds(7)  = 800         ! depth in meters
 elseif (lev == 6) then  ! hybrid level
  kpds(6)  = 109         ! 
  kpds(7)  = 1           ! level number
 elseif (lev == 7) then  ! hybrid level
  kpds(6)  = 109         ! 
  kpds(7)  = 2           ! level number
 elseif (lev == 8) then  ! hybrid level
  kpds(6)  = 109         ! 
  kpds(7)  = 3           ! level number
 elseif (lev == 9) then  ! hybrid level
  kpds(6)  = 109         ! 
  kpds(7)  = 4           ! level number
 elseif (lev == 10) then ! mean sea level
  kpds(6)  = 102         ! 
  kpds(7)  = 0           ! level number
 end if

 kpds(8)  = 0       ! year of century     the time info is determined
 kpds(9)  = 1      ! month               by the input file.
 kpds(10) = 1        ! day
 kpds(11) = 0       ! hour
 kpds(12) = 0          ! minute
 kpds(13) = 1          ! fcst time unit - hour
 kpds(14) = 0          ! period of time, p1.  set to '0' for analysis
 kpds(15) = 0          ! number of time units, p2.
 kpds(16) = 1          ! initialized analysis product
 kpds(17) = 0          ! number in average
 kpds(18) = 1          ! grib edition 1
 kpds(19) = 130          ! parameter table version number
 kpds(20) = 0          ! number missing from avg/accum
 kpds(21) = 20    ! century - set as in the input file
 kpds(22) = scale_fac          ! decimal scale factor
 kpds(23) = 0    ! subcenter - set as in the input file
 kpds(24) = 0          ! reserved
 kpds(25) = 0          ! reserved

 kgds = 0

 kgds( 1) = 203        ! oct 6 - type of grid, arakawa staggered e-grid
 kgds( 2) = imdl       ! octs 7-8 - i dimension of grid
 kgds( 3) = jmdl       ! octs 9-10 - j dimension of grid
! kgds( 4) = nint(3.441*1000.) ! latitude of first grid point, octs 11
! kgds( 5) = nint(-148.799*1000.) ! longitude of first grid point, octs 1
 kgds( 4) = nint(25.986*1000.) ! latitude of first grid point, octs 11
 kgds( 5) = nint(-127.871*1000.) ! longitude of first grid point, octs 1
 kgds( 6) = 136        ! oct 17 - resolution and component flag
 kgds( 7) = nint(50.0*1000.)
!                           ! octs 18-20 - # mass points along southmost row
 kgds( 8) = nint(-111.0*1000.)
!                           ! octs 21-23 - # rows in each column
! kgds( 7) = nint(40.0*1000.)
                           ! octs 18-20 - # mass points along southmost row
! kgds( 8) = nint(-115.0*1000.)
                           ! octs 21-23 - # rows in each column
 kgds( 9) = nint(24./449.*1000.)  ! octs 24-25 - long direction increment
 kgds(10) = nint(1./19.*1000.)  ! octs 26-27 - lat direction increment
! kgds( 9) = nint(53./236.*1000.)  ! octs 24-25 - long direction increment
! kgds(10) = nint(40./193.*1000.)  ! octs 26-27 - lat direction increment
 kgds(11) = 64         ! oct 28 - scanning mode flag
 kgds(19) = 0          ! oct 4  - # vert coordinate parameters
 kgds(20) = 255        ! oct 5  - not used, set to 255

 call getenv('OUTFILE', output_file) 

! where (lsmask_mdl > 0.0)
  lbms = .true.
! elsewhere
!  lbms = .false.
! endwhere

 iret = 0

 if (openit) then
  print*,"- OPEN OUTPUT GRIB FILE ", trim(output_file)
  call baopenw(lugb, output_file, iret)
  openit=.false.
  if (iret /= 0) then
    print*,'- ERROR OPENING OUTPUT GRIB FILE. IRET IS ', iret
    call mpi_abort
  end if
 end if

 print*,"- WRITE OUTPUT GRIB FILE ", trim(output_file)
 call putgb (lugb, (imdl*jmdl), kpds, kgds, lbms,  &
             data, iret)

 if (iret /= 0) then
   print*,'- ERROR WRITING OUTPUT GRIB FILE. IRET IS ', iret
   call mpi_abort
 end if

! call baclose(lugb, iret)

 return

 end subroutine gribit