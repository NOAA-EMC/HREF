MODULE module_wps_io_arw

   USE module_optional_input
   IMPLICIT NONE



!! FROM MODULE_KINDS

!   The numerical data types defined in this module are:
!      i_byte    - specification kind for byte (1-byte) integer variable
!      i_short   - specification kind for short (2-byte) integer variable
!      i_long    - specification kind for long (4-byte) integer variable
!      i_llong   - specification kind for double long (8-byte) integer variable
!      r_single  - specification kind for single precision (4-byte) real variable
!      r_double  - specification kind for double precision (8-byte) real variable
!      r_quad    - specification kind for quad precision (16-byte) real variable
!
!      i_kind    - generic specification kind for default integer
!      r_kind    - generic specification kind for default floating point
!
!
! Integer type definitions below

! Integer types
  integer, parameter, public  :: i_byte  = selected_int_kind(1)      ! byte  integer
  integer, parameter, public  :: i_short = selected_int_kind(4)      ! short integer
  integer, parameter, public  :: i_long  = selected_int_kind(8)      ! long  integer
  integer, parameter, private :: llong_t = selected_int_kind(16)     ! llong integer
  integer, parameter, public  :: i_llong = max( llong_t, i_long )

! Expected 8-bit byte sizes of the integer kinds
  integer, parameter, public :: num_bytes_for_i_byte  = 1
  integer, parameter, public :: num_bytes_for_i_short = 2
  integer, parameter, public :: num_bytes_for_i_long  = 4
  integer, parameter, public :: num_bytes_for_i_llong = 8

! Define arrays for default definition
  integer, parameter, private :: num_i_kinds = 4
  integer, parameter, dimension( num_i_kinds ), private :: integer_types = (/ &
       i_byte, i_short, i_long,  i_llong  /) 
  integer, parameter, dimension( num_i_kinds ), private :: integer_byte_sizes = (/ &
       num_bytes_for_i_byte, num_bytes_for_i_short, &
       num_bytes_for_i_long, num_bytes_for_i_llong  /)

! Default values
! **** CHANGE THE FOLLOWING TO CHANGE THE DEFAULT INTEGER TYPE KIND ***
  integer, parameter, private :: default_integer = 2  ! 1=byte, 
                                                      ! 2=short, 
                                                      ! 3=long, 
                                                      ! 4=llong
  integer, parameter, public  :: i_kind = integer_types( default_integer )
  integer, parameter, public  :: num_bytes_for_i_kind = &
       integer_byte_sizes( default_integer )


! Real definitions below

! Real types
  integer, parameter, public  :: r_single = selected_real_kind(6)  ! single precision
  integer, parameter, public  :: r_double = selected_real_kind(15) ! double precision
  integer, parameter, private :: quad_t   = selected_real_kind(20) ! quad precision
  integer, parameter, public  :: r_quad   = max( quad_t, r_double )

! Expected 8-bit byte sizes of the real kinds
  integer, parameter, public :: num_bytes_for_r_single = 4
  integer, parameter, public :: num_bytes_for_r_double = 8
  integer, parameter, public :: num_bytes_for_r_quad   = 16

! Define arrays for default definition
  integer, parameter, private :: num_r_kinds = 3
  integer, parameter, dimension( num_r_kinds ), private :: real_kinds = (/ &
       r_single, r_double, r_quad    /) 
  integer, parameter, dimension( num_r_kinds ), private :: real_byte_sizes = (/ &
       num_bytes_for_r_single, num_bytes_for_r_double, &
       num_bytes_for_r_quad    /)

! Default values
! **** CHANGE THE FOLLOWING TO CHANGE THE DEFAULT REAL TYPE KIND ***
  integer, parameter, private :: default_real = 2  ! 1=single, 
                                                   ! 2=double, 
!! END FROM MODULE_KINDS

      !  Input 3D meteorological fields.


      REAL , DIMENSION(:,:,:) , ALLOCATABLE :: landuse_frac_input , &
                                               soil_top_cat_input , &
                                               soil_bot_cat_input


      !  Input 2D surface fields.

      REAL , DIMENSION(:,:)   , ALLOCATABLE :: soilt010_input , soilt040_input , &
                                               soilt100_input , soilt160_input , &
                                               soilt200_input , soilt300_input , &  
                                               soilm010_input , soilm040_input , &
                                               soilm100_input , soilm160_input , &
                                               soilm200_input , soilm300_input , &
                                               psfc_in,pmsl

      REAL , DIMENSION(:,:)   , ALLOCATABLE :: lat_wind, lon_wind 


      !  Local input arrays

      REAL,DIMENSION(:,:),ALLOCATABLE :: dum2d
      INTEGER,DIMENSION(:,:),ALLOCATABLE :: idum2d
      REAL,DIMENSION(:,:,:),ALLOCATABLE :: dum3d

      LOGICAL , SAVE :: first_time_in = .TRUE.

      INTEGER :: flag_soilt010 , flag_soilt100 ,  &
                 flag_soilt200 ,   &
        	 flag_soilm010 , flag_soilm100 ,  &
                 flag_soilm200 

      INTEGER :: flag_soilt001, flag_soilt004, flag_soilt030, flag_soilt060
      INTEGER :: flag_soilm001, flag_soilm004, flag_soilm030, flag_soilm060

!   Some constants to allow simple dimensions in the defined types
!   given below.


CONTAINS

   SUBROUTINE read_wps ( grid, filename, file_date_string, num_metgrid_levels )

      USE module_soil_pre
      USE module_domain


      IMPLICIT NONE

      INCLUDE "mpif.h"

      TYPE(domain) , INTENT(INOUT)  :: grid
      CHARACTER (LEN=19) , INTENT(IN) :: file_date_string
      CHARACTER (LEN=19)              :: VarName
      CHARACTER (LEN=150)             :: chartemp
      CHARACTER (*) , INTENT(IN) :: filename

      INTEGER :: ids,ide,jds,jde,kds,kde           &
                ,ims,ime,jms,jme,kms,kme           &
                ,its,ite,jts,jte,kts,kte

      INTEGER :: i , j , k , loop, IMAX, JMAX
      INTEGER :: DATAHANDLE, num_metgrid_levels
      INTEGER :: Sysdepinfo, Status
      INTEGER :: istatus,ioutcount,iret,index,ierr
      
      integer :: nrecs,iunit, L,hor_size,hor_size_u,hor_size_v

!!
      character*132, allocatable :: datestr_all(:)
      character*132, allocatable :: varname_all(:)
      integer, allocatable       :: domainend_all(:,:)
      integer, allocatable       :: start_block(:)
      integer, allocatable       :: end_block(:)
      integer, allocatable       :: start_byte(:)
      integer, allocatable       :: end_byte(:)
      integer(kind=i_llong), allocatable           :: file_offset(:)
!!

      REAL :: dummy,tmp,garb
      REAL, ALLOCATABLE:: dumdata(:,:,:)
      REAL, ALLOCATABLE:: dumdata_u(:,:,:)
      REAL, ALLOCATABLE:: dumdata_v(:,:,:)

      REAL :: lats16(16),lons16(16)

      CHARACTER (LEN= 8) :: dummy_char

      INTEGER :: ok , map_proj , ok_open, igarb, N
      REAL :: pt
      INTEGER :: num_veg_cat , num_soil_top_cat , num_soil_bot_cat

      SELECT CASE ( model_data_order )
         CASE ( DATA_ORDER_ZXY )
            kds = grid%sd31 ; kde = grid%ed31 ;
            ids = grid%sd32 ; ide = grid%ed32 ;
            jds = grid%sd33 ; jde = grid%ed33 ;

            kms = grid%sm31 ; kme = grid%em31 ;
            ims = grid%sm32 ; ime = grid%em32 ;
            jms = grid%sm33 ; jme = grid%em33 ;

            kts = grid%sp31 ; kte = grid%ep31 ; ! tile is entire patch
            its = grid%sp32 ; ite = grid%ep32 ; ! tile is entire patch
            jts = grid%sp33 ; jte = grid%ep33 ; ! tile is entire patch

         CASE ( DATA_ORDER_XYZ )
            ids = grid%sd31 ; ide = grid%ed31 ;
            jds = grid%sd32 ; jde = grid%ed32 ;
            kds = grid%sd33 ; kde = grid%ed33 ;

            ims = grid%sm31 ; ime = grid%em31 ;
            jms = grid%sm32 ; jme = grid%em32 ;
            kms = grid%sm33 ; kme = grid%em33 ;

            its = grid%sp31 ; ite = grid%ep31 ; ! tile is entire patch
            jts = grid%sp32 ; jte = grid%ep32 ; ! tile is entire patch
            kts = grid%sp33 ; kte = grid%ep33 ; ! tile is entire patch

         CASE ( DATA_ORDER_XZY )
            ids = grid%sd31 ; ide = grid%ed31 ;
            kds = grid%sd32 ; kde = grid%ed32 ;
            jds = grid%sd33 ; jde = grid%ed33 ;

            ims = grid%sm31 ; ime = grid%em31 ;
            kms = grid%sm32 ; kme = grid%em32 ;
            jms = grid%sm33 ; jme = grid%em33 ;

            its = grid%sp31 ; ite = grid%ep31 ; ! tile is entire patch
            kts = grid%sp32 ; kte = grid%ep32 ; ! tile is entire patch
            jts = grid%sp33 ; jte = grid%ep33 ; ! tile is entire patch

      END SELECT

      !  Initialize what soil temperature and moisture is available.


      flag_st000010 = 0
      flag_st010040 = 0
      flag_st040100 = 0
      flag_st100200 = 0
      flag_sm000010 = 0 
      flag_sm010040 = 0
      flag_sm040100 = 0
      flag_sm100200 = 0
      flag_st010200 = 0
      flag_sm010200 = 0

      flag_soilt000 = 0
      flag_soilt001 = 0
      flag_soilt004 = 0
      flag_soilt010 = 0
      flag_soilt030 = 0
      flag_soilt040 = 0
      flag_soilt060 = 0
      flag_soilt100 = 0
      flag_soilt160 = 0
      flag_soilt200 = 0 
      flag_soilt300 = 0 

      flag_soilm000 = 0
      flag_soilm001 = 0
      flag_soilm004 = 0
      flag_soilm010 = 0 
      flag_soilm030 = 0 
      flag_soilm040 = 0
      flag_soilm060 = 0
      flag_soilm100 = 0
      flag_soilm160 = 0
      flag_soilm200 = 0
      flag_soilm300 = 0

      flag_sst      = 0
      flag_toposoil = 0

      !  How many soil levels have we found?  Well, right now, none.

      num_st_levels_input = 0
      num_sm_levels_input = 0
      st_levels_input = -1
      sm_levels_input = -1

         CALL nl_set_mminlu ( grid%id, 'USGS')
         CALL nl_set_iswater (grid%id, 16 )
         CALL nl_set_isice (grid%id, 24 )


      !  Get the space for the data if this is the first time here.

	write(6,*) 'pre allocations'
!	call summary()

        IF (.NOT. ALLOCATED (pmsl)              ) ALLOCATE ( pmsl(its:ite,jts:jte) )
        IF (.NOT. ALLOCATED (psfc_in)           ) ALLOCATE ( psfc_in(its:ite,jts:jte) )

	write(6,*) 'past allocations'
!	call summary()

        !  Local arrays


!!! MPI IO

      iunit=33
      call count_recs_wrf_binary_file(iunit, trim(fileName), nrecs)
	write(0,*) 'nrecs: ', nrecs

      allocate (datestr_all(nrecs))
      allocate (varname_all(nrecs))
      allocate (domainend_all(3,nrecs))
      allocate (start_block(nrecs))
      allocate (end_block(nrecs))
      allocate (start_byte(nrecs))
      allocate (end_byte(nrecs))
      allocate (file_offset(nrecs))
 
	write(0,*) 'allocated, call inventory'
      call inventory_wrf_binary_file(iunit, trim(filename), nrecs,  &
                      datestr_all,varname_all,domainend_all,   &
                      start_block,end_block,start_byte,end_byte,file_offset)

	do N=1,NRECS
	write(0,*) 'N,varname_all(N): ',N, varname_all(N)
	enddo

      call mpi_file_open(mpi_comm_world, trim(filename),     &
                         mpi_mode_rdonly,mpi_info_null, iunit, ierr)
      if (ierr /= 0) then
       call wrf_error_fatal("Error opening file with mpi io")
      end if

      VarName='CEN_LAT'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
      if (iret /= 0) then
        print*,VarName," not found in file"
      else

        call mpi_file_read_at(iunit,file_offset(index)+5*4,      &
                              garb,1,mpi_real4,                  &
                              mpi_status_ignore, ierr)

        if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
        else
          print*,VarName, ' from MPIIO READ= ',garb
          CALL nl_set_cen_lat ( grid%id , garb )
          write(0,*) 'cenlat= ', garb
        end if
      end if

      VarName='CEN_LON'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
        call mpi_file_read_at(iunit,file_offset(index)+5*4,      &
                              garb,1,mpi_real4,                  &
                              mpi_status_ignore, ierr)
          CALL nl_set_cen_lon ( grid%id , garb )
          CALL nl_set_stand_lon ( grid%id , garb )

      VarName='TRUELAT1'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
        call mpi_file_read_at(iunit,file_offset(index)+5*4,      &
                              garb,1,mpi_real4,                  &
                              mpi_status_ignore, ierr)
          CALL nl_set_truelat1 ( grid%id , garb )

      VarName='TRUELAT2'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
        call mpi_file_read_at(iunit,file_offset(index)+5*4,      &
                              garb,1,mpi_real4,                  &
                              mpi_status_ignore, ierr)
          CALL nl_set_truelat2 ( grid%id , garb )

      VarName='MAP_PROJ'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
        call mpi_file_read_at(iunit,file_offset(index)+5*4,     &
                              igarb,1,mpi_integer4,             &
                              mpi_status_ignore, ierr)

          CALL  nl_set_map_proj( grid%id, igarb)

      VarName='ISURBAN'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
        call mpi_file_read_at(iunit,file_offset(index)+5*4,     &
                              igarb,1,mpi_integer4,             &
                              mpi_status_ignore, ierr)
	write(0,*) 'ierr, igarb for ISURBAN: ', ierr,igarb
          CALL  nl_set_isurban ( grid%id, igarb)

      VarName='ISOILWATER'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
        call mpi_file_read_at(iunit,file_offset(index)+5*4,     &
                              igarb,1,mpi_integer4,             &
                              mpi_status_ignore, ierr)
	write(0,*) 'ierr, igarb for ISOILWATER: ', ierr,igarb
          CALL  nl_set_isoilwater ( grid%id, igarb)

      VarName='MOAD_CEN_LAT'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
        call mpi_file_read_at(iunit,file_offset(index)+5*4,     &
                              garb,1,mpi_real4,             &
                              mpi_status_ignore, ierr)
	write(0,*) 'ierr, MOAD_CEN_LAT: ', ierr, garb
          CALL  nl_set_moad_cen_lat ( grid%id,garb)

      VarName='corner_lats'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
	write(0,*) 'index, file_offset(index)+5*4: ', index, file_offset(index)+5*4
        call mpi_file_read_at(iunit,file_offset(index)+5*4,     &
                              lats16,16,mpi_real4,             &
                              mpi_status_ignore, ierr)
	write(0,*) 'ierr from mpi_file_read_at for corner_lats'

      VarName='corner_lons'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
        call mpi_file_read_at(iunit,file_offset(index)+5*4,     &
                              lons16,16,mpi_real4,             &
                              mpi_status_ignore, ierr)

!    grid%em_lat_ll_t = lats16( 1)
!    grid%em_lat_ul_t = lats16( 2)
!    grid%em_lat_ur_t = lats16( 3)
!    grid%em_lat_lr_t = lats16( 4)
!    grid%em_lat_ll_u = lats16( 5)
!    grid%em_lat_ul_u = lats16( 6)
!    grid%em_lat_ur_u = lats16( 7)
!    grid%em_lat_lr_u = lats16( 8)
!    grid%em_lat_ll_v = lats16( 9)
!    grid%em_lat_ul_v = lats16(10)
!    grid%em_lat_ur_v = lats16(11)
!    grid%em_lat_lr_v = lats16(12)
!    grid%em_lat_ll_d = lats16(13)
!    grid%em_lat_ul_d = lats16(14)
!    grid%em_lat_ur_d = lats16(15)
!    grid%em_lat_lr_d = lats16(16)
!    grid%em_lon_ll_t = lons16( 1)
!    grid%em_lon_ul_t = lons16( 2)
!    grid%em_lon_ur_t = lons16( 3)
!    grid%em_lon_lr_t = lons16( 4)
!    grid%em_lon_ll_u = lons16( 5)
!    grid%em_lon_ul_u = lons16( 6)
!    grid%em_lon_ur_u = lons16( 7)
!    grid%em_lon_lr_u = lons16( 8)
!    grid%em_lon_ll_v = lons16( 9)
!    grid%em_lon_ul_v = lons16(10)
!    grid%em_lon_ur_v = lons16(11)
!    grid%em_lon_lr_v = lons16(12)
!    grid%em_lon_ll_d = lons16(13)
!    grid%em_lon_ul_d = lons16(14)
!    grid%em_lon_ur_d = lons16(15)
!    grid%em_lon_lr_d = lons16(16)




    hor_size=(IDE-IDS)*(JDE-JDS)
    hor_size_u=(IDE+1-IDS)*(JDE-JDS)
    hor_size_v=(IDE-IDS)*(JDE+1-JDS)

	write(0,*) 'hor_size: ', hor_size
	write(0,*) 'hor_size_u: ', hor_size_u
	write(0,*) 'hor_size_v: ', hor_size_v
	write(0,*) 'IDE, JDE: ', IDE, JDE

    varName='PRES'
    allocate(dumdata(IDS:IDE-1,JDS:JDE-1,num_metgrid_levels))
    allocate(dumdata_u(IDS:IDE,JDS:JDE-1,num_metgrid_levels))
    allocate(dumdata_v(IDS:IDE-1,JDS:JDE,num_metgrid_levels))

     CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
     CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                          dumdata,hor_size*num_metgrid_levels,mpi_real4,             &
                          mpi_status_ignore, ierr)

	write(0,*) 'ierr from mpi_file_read_at for PRES: ', ierr

       DO K=1,num_metgrid_levels
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%p_gc(I,K,J)=dumdata(I,J,K)
         ENDDO
        ENDDO
       ENDDO

	write(0,*) 'grid%p_gc(25,25,25): ', grid%p_gc(25,25,25)

!    varName='SMC_WPS'
!        call read_from_wps_int(filename,file_date_string,DataHandle,varname,dumdata,IDE,JDE,num_metgrid_levels)
!
!    varName='STC_WPS'
!        call read_from_wps_int(filename,file_date_string,DataHandle,varname,dumdata,IDE,JDE,num_metgrid_levels)

    varName='GHT'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata,hor_size*num_metgrid_levels,mpi_real4,             &
                             mpi_status_ignore, ierr)


       DO K=1,num_metgrid_levels
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%ght_gc(I,K,J)=dumdata(I,J,K)
         ENDDO
        ENDDO
       ENDDO

    varName='VEGCAT'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata,hor_size,mpi_real4,             &
                             mpi_status_ignore, ierr)

        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%vegcat(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

!    varName='SOIL_CAT'
!    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
!    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
!                             dumdata,hor_size,mpi_real4,             &
!                             mpi_status_ignore, ierr)
!        DO J=JTS,min(JTE,JDE-1)
!         DO I=ITS,min(ITE,IDE-1)
!           grid%input_soil_cat(I,J)=dumdata(I,J,1)
!         ENDDO
!        ENDDO

    varName='CANWAT'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata,hor_size,mpi_real4,             &
                             mpi_status_ignore, ierr)

        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%canwat(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

    varName='SNOW'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata,hor_size,mpi_real4,             &
                             mpi_status_ignore, ierr)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%snow(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

    varName='SKINTEMP'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata,hor_size,mpi_real4,             &
                             mpi_status_ignore, ierr)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%tsk_gc(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO
	write(0,*) 'skintemp(25,25): ', grid%tsk_gc(25,25)

    varName='SOILHGT'
!        call read_from_wps_int(filename,file_date_string,DataHandle,varname,dumdata,IDE,JDE,num_metgrid_levels)
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata,hor_size,mpi_real4,             &
                             mpi_status_ignore, ierr)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%toposoil(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

!    varName='LANDSEA'
!        call read_from_wps_int(filename,file_date_string,DataHandle,varname,dumdata,IDE,JDE,num_metgrid_levels)
!???

!    varName='SEAICE'
!    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
!    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
!                             dumdata,hor_size,mpi_real4,             &
!                             mpi_status_ignore, ierr)
!        DO J=JTS,min(JTE,JDE-1)
!         DO I=ITS,min(ITE,IDE-1)
!           grid%xice_gc(I,J)=dumdata(I,J,1)
!         ENDDO
!        ENDDO

    varName='ST100200'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata,hor_size,mpi_real4,             &
                             mpi_status_ignore, ierr)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%st100200(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

    varName='ST040100'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata,hor_size,mpi_real4,             &
                             mpi_status_ignore, ierr)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%st040100(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

    varName='ST010040'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata,hor_size,mpi_real4,             &
                             mpi_status_ignore, ierr)

        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%st010040(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

    varName='ST000010'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata,hor_size,mpi_real4,             &
                             mpi_status_ignore, ierr)

        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%st000010(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

    varName='SM100200'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata,hor_size,mpi_real4,             &
                             mpi_status_ignore, ierr)

        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%sm100200(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

    varName='SM040100'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata,hor_size,mpi_real4,             &
                             mpi_status_ignore, ierr)

        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%sm040100(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

    varName='SM010040'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata,hor_size,mpi_real4,             &
                             mpi_status_ignore, ierr)

        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%sm010040(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

    varName='SM000010'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata,hor_size,mpi_real4,             &
                             mpi_status_ignore, ierr)

        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%sm000010(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

    varName='PSFC'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata,hor_size,mpi_real4,             &
                             mpi_status_ignore, ierr)

	if ( ierr .eq. 0 ) flag_psfc=1

        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%psfc(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

    varName='RH'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata,hor_size*num_metgrid_levels,mpi_real4,             &
                             mpi_status_ignore, ierr)

       DO K=1,num_metgrid_levels
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%rh_gc(I,K,J)=dumdata(I,J,K)
         ENDDO
        ENDDO
       ENDDO

    varName='VV'
	write(0,*) 'grabbing VV'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata_v,hor_size_v*num_metgrid_levels,mpi_real4,             &
                             mpi_status_ignore, ierr)

	write(0,*) 'VV J lims: ', JTS,min(JTE,JDE)
	write(0,*) 'VV I lims: ', ITS,min(ITE,IDE-1)

       DO K=1,num_metgrid_levels
        DO J=JTS,min(JTE,JDE)
         DO I=ITS,min(ITE,IDE-1)
           grid%v_gc(I,K,J)=dumdata_v(I,J,K)
	if (grid%v_gc(I,K,J) .ne. grid%v_gc(I,K,J) .or. abs(grid%v_gc(I,K,J)) .gt. 100.) then
	write(0,*) 'bad v_gc defined: ', I,K,J,grid%v_gc(I,K,J)
	endif
         ENDDO
        ENDDO
       ENDDO

    varName='UU'
	write(0,*) 'grabbing UU'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata_u,hor_size_u*num_metgrid_levels,mpi_real4,             &
                             mpi_status_ignore, ierr)
	write(0,*) 'UU J lims: ', JTS,min(JTE,JDE-1)
	write(0,*) 'UU I lims: ', ITS,min(ITE,IDE)
       DO K=1,num_metgrid_levels
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE)
           grid%u_gc(I,K,J)=dumdata_u(I,J,K)
	if (grid%u_gc(I,K,J) .ne. grid%u_gc(I,K,J) .or. abs(grid%u_gc(I,K,J)) .gt. 100.) then
	write(0,*) 'bad u_gc defined: ', I,K,J,grid%u_gc(I,K,J)
	endif
         ENDDO
        ENDDO
       ENDDO

    varName='TT'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata,hor_size*num_metgrid_levels,mpi_real4,             &
                             mpi_status_ignore, ierr)
       DO K=1,num_metgrid_levels
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%t_gc(I,K,J)=dumdata(I,J,K)
	if (grid%t_gc(I,K,J) .ne. grid%t_gc(I,K,J) .or. abs(grid%t_gc(I,K,J)) .gt. 350.) then
	write(0,*) 'bad t_gc defined: ', I,K,J,grid%t_gc(I,K,J)
	endif
         ENDDO
        ENDDO
       ENDDO
	write(0,*) 't_gc(25,25,25): ', grid%t_gc(25,25,25)

!    varName='RWMR'
!    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
!    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
!                             dumdata,hor_size*num_metgrid_levels,mpi_real4,             &
!                             mpi_status_ignore, ierr)
!       DO K=1,num_metgrid_levels
!        DO J=JTS,min(JTE,JDE-1)
!         DO I=ITS,min(ITE,IDE-1)
!           grid%nmm_rwmr_gc(I,J,K)=dumdata(I,J,K)
!         ENDDO
!        ENDDO
!       ENDDO

!    varName='SNMR'
!    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
!    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
!                             dumdata,hor_size*num_metgrid_levels,mpi_real4,             &
!                             mpi_status_ignore, ierr)
!       DO K=1,num_metgrid_levels
!        DO J=JTS,min(JTE,JDE-1)
!         DO I=ITS,min(ITE,IDE-1)
!           grid%nmm_snmr_gc(I,J,K)=dumdata(I,J,K)
!         ENDDO
!        ENDDO
!       ENDDO

!    varName='CLWMR'
!    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
!    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
!                             dumdata,hor_size*num_metgrid_levels,mpi_real4,             &
!                             mpi_status_ignore, ierr)
!       DO K=1,num_metgrid_levels
!        DO J=JTS,min(JTE,JDE-1)
!         DO I=ITS,min(ITE,IDE-1)
!           grid%nmm_clwmr_gc(I,J,K)=dumdata(I,J,K)
!         ENDDO
!        ENDDO
!       ENDDO

!    varName='CICE'
!    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
!    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
!                             dumdata,hor_size*num_metgrid_levels,mpi_real4,             &
!                             mpi_status_ignore, ierr)
!       DO K=1,num_metgrid_levels
!        DO J=JTS,min(JTE,JDE-1)
!         DO I=ITS,min(ITE,IDE-1)
!           grid%nmm_cice_gc(I,J,K)=dumdata(I,J,K)
!         ENDDO
!        ENDDO
!       ENDDO

!    varName='FRIMEF'
!    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
!    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
!                             dumdata,hor_size*num_metgrid_levels,mpi_real4,             &
!                             mpi_status_ignore, ierr)
!       DO K=1,num_metgrid_levels
!        DO J=JTS,min(JTE,JDE-1)
!         DO I=ITS,min(ITE,IDE-1)
!           grid%nmm_rimef_gc(I,J,K)=dumdata(I,J,K)
!         ENDDO
!        ENDDO
!       ENDDO


     varName='PMSL'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata,hor_size,mpi_real4,             &
                             mpi_status_ignore, ierr)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%pslv_gc(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO


    varName='SLOPECAT'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata,hor_size,mpi_real4,             &
                             mpi_status_ignore, ierr)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%slopecat(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

    varName='SNOALB'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata,hor_size,mpi_real4,             &
                             mpi_status_ignore, ierr)

        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%snoalb(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

         num_veg_cat      = SIZE ( grid%landusef , DIM=2 )
         num_soil_top_cat = SIZE ( grid%soilctop , DIM=2 )
         num_soil_bot_cat = SIZE ( grid%soilcbot , DIM=2 )

    varName='GREENFRAC'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata,hor_size*12,mpi_real4,             &
                             mpi_status_ignore, ierr)

       DO K=1,12
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%greenfrac(I,K,J)=dumdata(I,J,K)
         ENDDO
        ENDDO
       ENDDO

    varName='ALBEDO12M'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata,hor_size*12,mpi_real4,             &
                             mpi_status_ignore, ierr)

       DO K=1,12
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%albedo12m(I,K,J)=dumdata(I,J,K)
         ENDDO
        ENDDO
       ENDDO

    varName='SOILCBOT'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata,hor_size*num_soil_bot_cat,mpi_real4,             &
                             mpi_status_ignore, ierr)

       DO K=1,num_soil_bot_cat
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilcbot(I,K,J)=dumdata(I,J,K)
         ENDDO
        ENDDO
       ENDDO

    varName='SOILCAT'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata,hor_size,mpi_real4,             &
                             mpi_status_ignore, ierr)

        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilcat(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

	write(0,*) 'veg_cat and soil_cat sizes:::: ', num_veg_cat , num_soil_top_cat

    varName='SOILCTOP'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata,hor_size*num_soil_top_cat,mpi_real4,             &
                             mpi_status_ignore, ierr)
       DO K=1,num_soil_top_cat
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilctop(I,K,J)=dumdata(I,J,K)
         ENDDO
        ENDDO
       ENDDO

    varName='SOILTEMP'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata,hor_size,mpi_real4,             &
                             mpi_status_ignore, ierr)

        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%tmn_gc(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

!    varName='HGT_V'
!    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
!    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
!                             dumdata,hor_size,mpi_real4,             &
!                             mpi_status_ignore, ierr)
!
!        DO J=JTS,min(JTE,JDE-1)
!         DO I=ITS,min(ITE,IDE-1)
!           grid%nmm_htv_gc(I,J)=dumdata(I,J,1)
!         ENDDO
!        ENDDO

    varName='HGT_M'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata,hor_size,mpi_real4,             &
                             mpi_status_ignore, ierr)

        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%ht_gc(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

    varName='LU_INDEX'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata,hor_size,mpi_real4,             &
                             mpi_status_ignore, ierr)

        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%lu_index(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

    varName='LANDUSEF'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata,hor_size*num_veg_cat,mpi_real4,             &
                             mpi_status_ignore, ierr)

       DO K=1,num_veg_cat
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%landusef(I,K,J)=dumdata(I,J,K)
         ENDDO
        ENDDO
       ENDDO

    varName='LANDMASK'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata,hor_size,mpi_real4,             &
                             mpi_status_ignore, ierr)

        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%landmask(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

    varName='F'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata,hor_size,mpi_real4,             &
                             mpi_status_ignore, ierr)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%f(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

    varName='E'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata,hor_size,mpi_real4,             &
                             mpi_status_ignore, ierr)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%e(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

    varName='COSALPHA'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata,hor_size,mpi_real4,             &
                             mpi_status_ignore, ierr)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%cosa(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

    varName='SINALPHA'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata,hor_size,mpi_real4,             &
                             mpi_status_ignore, ierr)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%sina(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

    varName='MAPFAC_U'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata_u,hor_size_u,mpi_real4,             &
                             mpi_status_ignore, ierr)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE)
           grid%msfu(I,J)=dumdata_u(I,J,1)

!! bandaid for newly created WPS geogrid static files
        if (grid%msfu(I,J) .lt. 0.7) then
        write(0,*) 'weird msfu at I,J: ', I,J,grid%msfu(I,J)

        if(J .eq. min(JTE,JDE-1)) then
        grid%msfu(I,J)=dumdata_u(I,J-1,1)
        write(0,*) 'changing msfu to: ',I,J,  grid%msfu(I,J)
        endif
        
        endif
!! end bandaid 

         ENDDO
        ENDDO

    varName='MAPFAC_V'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata_v,hor_size_v,mpi_real4,             &
                             mpi_status_ignore, ierr)
        DO J=JTS,min(JTE,JDE)
         DO I=ITS,min(ITE,IDE-1)
           grid%msfv(I,J)=dumdata_v(I,J,1)

        if (grid%msfv(I,J) .lt. 0.7 ) then
        write(0,*) 'weird msfv at I,J: ', I,J,grid%msfv(I,J)
        grid%msfv(I,J)=dumdata_v(I,J-1,1)
        if( J .eq. min(JTE,JDE)) then
        write(0,*) 'changing msfv to: ',I,J,  grid%msfv(I,J)
        endif

        endif

         ENDDO
        ENDDO

    varName='MAPFAC_M'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata,hor_size,mpi_real4,             &
                             mpi_status_ignore, ierr)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%msft(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

    varName='XLONG_U'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata_u,hor_size_u,mpi_real4,             &
                             mpi_status_ignore, ierr)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE)
           grid%xlong_u(I,J)=dumdata_u(I,J,1)
         ENDDO
        ENDDO

    varName='XLAT_U'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata_u,hor_size_u,mpi_real4,             &
                             mpi_status_ignore, ierr)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE)
           grid%xlat_u(I,J)=dumdata_u(I,J,1)
         ENDDO
        ENDDO

    varName='XLONG_V'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata_v,hor_size_v,mpi_real4,             &
                             mpi_status_ignore, ierr)
        DO J=JTS,min(JTE,JDE)
         DO I=ITS,min(ITE,IDE-1)
           grid%xlong_v(I,J)=dumdata_v(I,J,1)
         ENDDO
        ENDDO

    varName='XLAT_V'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata_v,hor_size_v,mpi_real4,             &
                             mpi_status_ignore, ierr)
        DO J=JTS,min(JTE,JDE)
         DO I=ITS,min(ITE,IDE-1)
           grid%xlat_v(I,J)=dumdata_v(I,J,1)
         ENDDO
        ENDDO

    varName='XLONG_M'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata,hor_size,mpi_real4,             &
                             mpi_status_ignore, ierr)

        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%xlong_gc(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

    varName='XLAT_M'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata,hor_size,mpi_real4,             &
                             mpi_status_ignore, ierr)

        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%xlat_gc(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

      call mpi_file_close(mpi_comm_world, ierr)

       varName='ST000010'
       flag_st000010 = 1
       num_st_levels_input = num_st_levels_input + 1
       st_levels_input(num_st_levels_input) = char2int2(varName(3:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
           st_input(I,num_st_levels_input + 1,J) = grid%st000010(i,j)
        ENDDO
       ENDDO

       varName='ST010040'
       flag_st010040 = 1
       num_st_levels_input = num_st_levels_input + 1
       st_levels_input(num_st_levels_input) = char2int2(varName(3:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
            st_input(I,num_st_levels_input + 1,J) = grid%st010040(i,j)
        ENDDO
       ENDDO

       varName='ST040100'
       flag_st040100 = 1
       num_st_levels_input = num_st_levels_input + 1
       st_levels_input(num_st_levels_input) = char2int2(varName(3:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
              st_input(I,num_st_levels_input + 1,J) = grid%st040100(i,j)
        ENDDO
       ENDDO
 
       varName='ST100200'
       flag_st100200 = 1
       num_st_levels_input = num_st_levels_input + 1
       st_levels_input(num_st_levels_input) = char2int2(varName(3:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
              st_input(I,num_st_levels_input + 1,J) = grid%st100200(i,j)
        ENDDO
       ENDDO

       varName='SM000010'
       flag_sm000010 = 1
       num_sm_levels_input = num_sm_levels_input + 1
       sm_levels_input(num_sm_levels_input) = char2int2(varName(3:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
           sm_input(I,num_sm_levels_input + 1,J) = grid%sm000010(i,j)
        ENDDO
       ENDDO

       varName='SM010040'
       flag_sm010040 = 1
       num_sm_levels_input = num_sm_levels_input + 1
       sm_levels_input(num_sm_levels_input) = char2int2(varName(3:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
            sm_input(I,num_sm_levels_input + 1,J) = grid%sm010040(i,j)
        ENDDO
       ENDDO

       varName='SM040100'
       flag_sm040100 = 1
       num_sm_levels_input = num_sm_levels_input + 1
       sm_levels_input(num_sm_levels_input) = char2int2(varName(3:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
              sm_input(I,num_sm_levels_input + 1,J) = grid%sm040100(i,j)
        ENDDO
       ENDDO
 
       varName='SM100200'
       flag_sm100200 = 1
       num_sm_levels_input = num_sm_levels_input + 1
       sm_levels_input(num_sm_levels_input) = char2int2(varName(3:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
              sm_input(I,num_sm_levels_input + 1,J) = grid%sm100200(i,j)
        ENDDO
       ENDDO

!           flag_sst = 1

        write(0,*) 'maxval st_input(1): ', maxval(st_input(:,1,:))
        write(0,*) 'maxval st_input(2): ', maxval(st_input(:,2,:))
        write(0,*) 'maxval st_input(3): ', maxval(st_input(:,3,:))
        write(0,*) 'maxval st_input(4): ', maxval(st_input(:,4,:))

        DEALLOCATE(pmsl)
        DEALLOCATE(psfc_in)
        DEALLOCATE(dumdata)
        DEALLOCATE(dumdata_u)
        DEALLOCATE(dumdata_v)

     end subroutine read_wps

! -------------------------------------------------------------------------
! -------------------------------------------------------------------------
! -------------------------------------------------------------------------
! -------------------------------------------------------------------------

        subroutine read_from_wps_int(filename,DateStr,dh,varname,varbuff,idim,jdim,ldim)

   implicit none

   character(*) ,intent(in) :: fileName
   character(len=19) ,intent(in) :: DateStr
   integer  :: dh
   character(len=19) :: VarName

   real :: VarBuff(idim,jdim,ldim)

   integer :: ndim,idim,jdim,ldim
   integer :: LM,IM
   integer :: WrfType,i,j,l,ll
   integer, dimension(4) :: start_index, end_index
   character (len= 4) :: staggering
   character (len= 3) :: ordering
   character (len=80), dimension(3) :: dimnames
   real, allocatable, dimension(:,:,:,:) :: data
   integer :: ierr
   character(len=132) :: Stagger
   start_index = 1
   end_index = 1

	write(6,*) 'idim*jdim*ldim: ', idim*jdim*ldim

    CALL ext_int_get_var_info(dh,TRIM(VarName),ndim,ordering,Stagger,start_index,end_index,WrfType,ierr)

!   IF ( WrfType /= WRF_REAL .AND. WrfType /= WRF_REAL8) THEN !Ignore if not a real variable
!     write(*,*) 'Error: Not a real variable',WrfType
!     return
!   ENDIF

   ALLOCATE (data (end_index(1), end_index(2), end_index(3), 1))

   IF ( ierr /= 0 ) THEN
     write(*,*)'Error: ',ierr,TRIM(VarName),' not found in ',fileName
     data=0.
     VarBuff=0.
     go to 27
   ENDIF


   CALL ext_int_read_field(dh,DateStr,TRIM(VarName),data,WrfType,0,0,0,ordering,&
                             staggering, dimnames , &
                             start_index,end_index, & !dom
                             start_index,end_index, & !mem
                             start_index,end_index, & !pat
                             ierr)

   IF (ndim .eq. 0) THEN
    VarBuff(1,1,1)=data(1,1,1,1)
   ELSEIF(ndim .eq. 1) THEN
    do l=1,lm
      VarBuff(1,1,l)=data(l,1,1,1)
    end do
   ELSEIF(ndim .eq. 2) THEN
    do i=1,idim
      do j=1,jdim
       VarBuff(i,j,1)=data(i,j,1,1)
      enddo
    enddo
   ELSEIF(ndim .eq. 3) THEN
    do l=1,end_index(3)
     do i=1,idim
      do j=1,jdim
       VarBuff(i,j,l)=data(i,j,l,1)
      enddo
     enddo
     write(*,*) Varname,' L ',l,': = ',data(1,1,l,1)
    enddo
   ENDIF

 27 continue
   DEALLOCATE(data)


     end subroutine read_from_wps_int



!!!! MPI-IO pieces

subroutine retrieve_index(index,string,varname_all,nrecs,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    retrieve_index  get record number of desired variable
!   prgmmr: parrish          org: np22                date: 2004-11-29
!
! abstract: by examining previously generated inventory of wrf binary restart file,
!             find record number that contains the header record for variable
!             identified by input character variable "string".
!
! program history log:
!   2004-11-29  parrish
!
!   input argument list:
!     string           - mnemonic for variable desired
!     varname_all      - list of all mnemonics obtained from inventory of file
!     nrecs            - total number of sequential records counted in wrf
!                        binary restart file
!
!   output argument list:
!     index            - desired record number
!     iret             - return status, set to 0 if variable was found,
!                        non-zero if not.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  implicit none

  integer,intent(out)::iret
  integer,intent(in)::nrecs
  integer,intent(out):: index
  character(*), intent(in):: string
  character(132),intent(in)::varname_all(nrecs)

  integer i

  iret=0

  do i=1,nrecs
   if(trim(string) == trim(varname_all(i))) then
      index=i
      return
   end if
  end do

  write(6,*)' problem reading wrf nmm binary file, rec id "',trim(string),'" not found'

  iret=-1

end subroutine retrieve_index
subroutine next_buf(in_unit,buf,nextbyte,locbyte,thisblock,lrecl,nreads,lastbuf)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    next_buf    bring in next direct access block
!   prgmmr: parrish          org: np22                date: 2004-11-29
!
! abstract: bring in next direct access block when needed, as the file is scanned
!             from beginning to end during counting and inventory of records.
!             (subroutines count_recs_wrf_binary_file and inventory_wrf_binary_file)
!
! program history log:
!   2004-11-29  parrish
!
!   input argument list:
!     in_unit          - fortran unit number where input file is opened through.
!     nextbyte         - byte number from beginning of file that is desired
!     locbyte          - byte number from beginning of last block read for desired byt
!     lrecl            - direct access block length
!     nreads           - number of blocks read before now (for diagnostic information
!     lastbuf          - logical, if true, then no more blocks, so return
!
!   output argument list:
!     buf              - output array containing contents of next block
!     locbyte          - byte number from beginning of new block read for desired byte
!     thisblock        - number of new block being read by this routine
!     nreads           - number of blocks read now (for diagnostic information only)
!     lastbuf          - logical, if true, then at end of file.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
!  use kinds, only: i_byte,i_llong
  implicit none

  integer(i_llong) lrecl
  integer in_unit,nreads
  integer(i_byte) buf(lrecl)
  integer(i_llong) nextbyte,locbyte,thisblock
  logical lastbuf

  integer ierr

  if(lastbuf) return

  ierr=0
  nreads=nreads+1

!  compute thisblock:

  thisblock = 1_i_llong + (nextbyte-1_i_llong)/lrecl

  locbyte = 1_i_llong+mod(locbyte-1_i_llong,lrecl)

  read(in_unit,rec=thisblock,iostat=ierr)buf
  lastbuf = ierr /= 0

end subroutine next_buf

subroutine inventory_wrf_binary_file(in_unit,wrf_ges_filename,nrecs, &
                                     datestr_all,varname_all,domainend_all, &
                                     start_block,end_block,start_byte,end_byte,file_offset)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    inventory_wrf_binary_file  get contents of wrf binary file
!   prgmmr: parrish          org: np22                date: 2004-11-29
!
! abstract: generate list of contents and map of wrf binary file which can be
!             used for reading and writing with mpi io routines.
!             same basic routine as count_recs_wrf_binary_file, except
!             now wrf unpacking routines are used to decode wrf header
!             records, and send back lists of variable mnemonics, dates,
!             grid dimensions, and byte addresses relative to start of
!             file for each field (this is used by mpi io routines).
!
! program history log:
!   2004-11-29  parrish
!
!
!   input argument list:
!     in_unit          - fortran unit number where input file is opened through.
!     wrf_ges_filename - filename of input wrf binary restart file
!     nrecs            - number of sequential records found on input wrf binary restart file.
!                          (obtained by a previous call to count_recs_wrf_binary_file)
!
!   output argument list:  (all following dimensioned nrecs)
!     datestr_all      - date character string for each field, where applicable (or else blanks)
!     varname_all      - wrf mnemonic for each variable, where applicable (or blank)
!     domainend_all    - dimensions of each field, where applicable (up to 3 dimensions)
!     start_block      - direct access block number containing 1st byte of record
!                            (after 4 byte record mark)
!     end_block        - direct access block number containing last byte of record
!                            (before 4 byte record mark)
!     start_byte       - relative byte address in direct access block of 1st byte of record
!     end_byte         - relative byte address in direct access block of last byte of record
!     file_offset      - absolute address of byte before 1st byte of record (used by mpi io)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
!   use kinds, only: r_single,i_byte,i_long,i_llong
  use module_internal_header_util
  implicit none

  integer,intent(in)::in_unit,nrecs
  character(*),intent(in)::wrf_ges_filename
  character(132),intent(out)::datestr_all(nrecs),varname_all(nrecs)
  integer,intent(out)::domainend_all(3,nrecs)
  integer,intent(out)::start_block(nrecs),end_block(nrecs)
  integer,intent(out)::start_byte(nrecs),end_byte(nrecs)
  integer(i_llong),intent(out)::file_offset(nrecs)

  integer irecs
  integer(i_llong) nextbyte,locbyte,thisblock
  integer(i_byte) lenrec4(4)
  integer(i_long) lenrec,lensave
  equivalence (lenrec4(1),lenrec)
  integer(i_byte) missing4(4)
  integer(i_long) missing
  equivalence (missing,missing4(1))
  integer(i_llong),parameter:: lrecl=2**20
  integer(i_byte) buf(lrecl)
  integer i,loc_count,nreads
  logical lastbuf
  integer(i_byte) hdrbuf4(2048)
  integer(i_long) hdrbuf(512)
  equivalence(hdrbuf(1),hdrbuf4(1))
  integer,parameter:: int_field       =       530
  integer,parameter:: int_dom_ti_char =       220
  integer,parameter:: int_dom_ti_real =       140
  integer,parameter:: int_dom_ti_integer =       180
  integer hdrbufsize
  integer inttypesize
  integer datahandle,count
  character(128) element,dumstr,strdata
  integer loccode
  character(132) blanks
  integer typesize
  integer fieldtype,comm,iocomm
  integer domaindesc
  character(132) memoryorder,stagger,dimnames(3)
  integer domainstart(3),domainend(3)
  integer memorystart(3),memoryend(3)
  integer patchstart(3),patchend(3)
  character(132) datestr,varname
  real(r_single) dummy_field(1)
!  integer dummy_field
!  real dummy_field
  integer itypesize
  integer idata(1)
  real rdata(1)

  call wrf_sizeof_integer(itypesize)
  inttypesize=itypesize

  blanks=trim(' ')

	write(0,*) 'inventory subroutine'

	write(0,*) 'opening file : ', trim(wrf_ges_filename)

  open(in_unit,file=trim(wrf_ges_filename),access='direct',recl=lrecl)
  irecs=0
  missing=-9999
  nextbyte=0_i_llong
  locbyte=lrecl
  nreads=0
  lastbuf=.false.
  do

!   get length of next record

    do i=1,4
     nextbyte=nextbyte+1_i_llong
     locbyte=locbyte+1_i_llong
     if(locbyte > lrecl .and. lastbuf) go to 900
     if(locbyte > lrecl) call next_buf(in_unit,buf,nextbyte,locbyte,thisblock,lrecl,nreads,lastbuf)
     lenrec4(i)=buf(locbyte)
    end do
    if(lenrec <= 0 .and. lastbuf) go to 900
    if(lenrec <= 0 .and. .not. lastbuf) go to 885
    nextbyte=nextbyte+1_i_llong
    locbyte=locbyte+1_i_llong
    if(locbyte > lrecl .and. lastbuf) go to 900
    if(locbyte > lrecl) call next_buf(in_unit,buf,nextbyte,locbyte,thisblock,lrecl,nreads,lastbuf)

    irecs=irecs+1
    start_block(irecs)=thisblock
    start_byte(irecs)=locbyte
    file_offset(irecs)=nextbyte-1_i_llong
    hdrbuf4(1)=buf(locbyte)
    hdrbuf4(2:4)=missing4(2:4)
    hdrbuf4(5:8)=missing4(1:4)
    datestr_all(irecs)=blanks
    varname_all(irecs)=blanks
    domainend_all(1:3,irecs)=0

    loc_count=1
    do i=2,8
       if(loc_count.ge.lenrec) exit
       loc_count=loc_count+1
       nextbyte=nextbyte+1_i_llong
       locbyte=locbyte+1_i_llong
       if(locbyte > lrecl .and. lastbuf) go to 900
       if(locbyte > lrecl) call next_buf(in_unit,buf,nextbyte,locbyte,thisblock,lrecl,nreads,lastbuf)
       hdrbuf4(i)=buf(locbyte)
    end do

         if(lenrec==2048) write(0,*)' irecs,hdrbuf(2),int_dom_ti_char,int_field=', &
                                      irecs,hdrbuf(2),int_dom_ti_char,int_field
    if(lenrec==2048.and.(hdrbuf(2) == int_dom_ti_char .or. hdrbuf(2) == int_field &
    .or. hdrbuf(2) == int_dom_ti_real .or. hdrbuf(2) == int_dom_ti_integer)) then

!    bring in next full record, so we can unpack datestr, varname, and domainend
       do i=9,lenrec
          loc_count=loc_count+1
          nextbyte=nextbyte+1_i_llong
          locbyte=locbyte+1_i_llong
          if(locbyte > lrecl .and. lastbuf) go to 900
          if(locbyte > lrecl) call next_buf(in_unit,buf,nextbyte,locbyte,thisblock,lrecl,nreads,lastbuf)
          hdrbuf4(i)=buf(locbyte)
       end do

       if(hdrbuf(2) == int_dom_ti_char) then

          call int_get_ti_header_char(hdrbuf,hdrbufsize,inttypesize, &
                   datahandle,element,dumstr,strdata,loccode)
          varname_all(irecs)=trim(element)
          datestr_all(irecs)=trim(strdata)
              write(0,*)'(1) irecs,varname,datestr = ',irecs,trim(varname_all(irecs)),trim(datestr_all(irecs))

       else if(hdrbuf(2) == int_dom_ti_real) then

          call int_get_ti_header_real(hdrbuf,hdrbufsize,inttypesize,typesize, &
                   datahandle,element,rdata,count,loccode)
          varname_all(irecs)=trim(element)
!          datestr_all(irecs)=trim(strdata)
              write(0,*)'(2) irecs,varname,datestr = ',irecs,trim(varname_all(irecs)),rdata(1:count)
         
       else if(hdrbuf(2) == int_dom_ti_integer) then

          call int_get_ti_header_integer(hdrbuf,hdrbufsize,inttypesize,typesize, &
                   datahandle,element,idata,count,loccode)
          varname_all(irecs)=trim(element)
!          datestr_all(irecs)=trim(strdata)
              write(0,*)'(3) irecs,varname,datestr = ',irecs,trim(varname_all(irecs)),idata(1:count)

       else

          call int_get_write_field_header(hdrbuf,hdrbufsize,inttypesize,typesize, &
             datahandle,datestr,varname,dummy_field,fieldtype,comm,iocomm, &
             domaindesc,memoryorder,stagger,dimnames, &
             domainstart,domainend,memorystart,memoryend,patchstart,patchend)
          varname_all(irecs)=trim(varname)
          datestr_all(irecs)=trim(datestr)
          domainend_all(1:3,irecs)=domainend(1:3)
              write(0,*)'(4) irecs,datestr,domend,varname = ', &
                  irecs,trim(datestr_all(irecs)),domainend_all(1:3,irecs),trim(varname_all(irecs))

       end if
    end if

	write(0,*) 'here in inventory'

    nextbyte=nextbyte-loc_count+lenrec
    locbyte=locbyte-loc_count+lenrec
    if(locbyte > lrecl .and. lastbuf) go to 900
    if(locbyte > lrecl) call next_buf(in_unit,buf,nextbyte,locbyte,thisblock,lrecl,nreads,lastbuf)
    end_block(irecs)=thisblock
    end_byte(irecs)=locbyte
    lensave=lenrec
    do i=1,4
     nextbyte=nextbyte+1_i_llong
     locbyte=locbyte+1_i_llong
     if(locbyte > lrecl .and. lastbuf) go to 900
     if(locbyte > lrecl) call next_buf(in_unit,buf,nextbyte,locbyte,thisblock,lrecl,nreads,lastbuf)
     lenrec4(i)=buf(locbyte)
    end do
    if(lenrec /= lensave) go to 890

  end do

880  continue
     write(6,*)' reached impossible place in inventory_wrf_binary_file'
     close(in_unit)
     return

885  continue
     write(0,*)' problem in inventory_wrf_binary_file, lenrec has bad value before end of file'
     write(0,*)'     lenrec =',lenrec
     close(in_unit)
     return

890  continue
     write(0,*)' problem in inventory_wrf_binary_file, beginning and ending rec len words unequal'
     write(0,*)'     begining reclen =',lensave
     write(0,*)'       ending reclen =',lenrec
     write(0,*)'               irecs =',irecs
     write(0,*)'               nrecs =',nrecs
     call wrf_error_fatal("curious reclen discrepancy")
     close(in_unit)
     return

900  continue
     write(0,*)' normal end of file reached in inventory_wrf_binary_file'
     write(0,*)'        nblocks=',thisblock
     write(0,*)'          irecs,nrecs=',irecs,nrecs
     write(0,*)'         nreads=',nreads
     close(in_unit)

end subroutine inventory_wrf_binary_file

SUBROUTINE wrf_sizeof_integer( retval )
  IMPLICIT NONE
  INTEGER retval
! 4 is defined by CPP
  retval = 4
  RETURN
END SUBROUTINE wrf_sizeof_integer

SUBROUTINE wrf_sizeof_real( retval )
  IMPLICIT NONE
  INTEGER retval
! 4 is defined by CPP
  retval = 4
  RETURN
END SUBROUTINE wrf_sizeof_real

!!!
!!!
!!!

subroutine count_recs_wrf_binary_file(in_unit,wrf_ges_filename,nrecs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    count_recs_binary_file  count # recs on wrf binary file
!   prgmmr: parrish          org: np22                date: 2004-11-29
!
! abstract: count number of sequential records contained in wrf binary
!             file.  this is done by opening the file in direct access
!             mode with block length of 2**20, the size of the physical
!             blocks on ibm "blue" and "white" machines.  for optimal
!             performance, change block length to correspond to the
!             physical block length of host machine disk space.
!             records are counted by looking for the 4 byte starting
!             and ending sequential record markers, which contain the
!             record size in bytes.  only blocks are read which are known
!             by simple calculation to contain these record markers.
!             even though this is done on one processor, it is still
!             very fast, and the time will always scale by the number of
!             sequential records, not their size.  this step and the
!             following inventory step consistently take less than 0.1 seconds
!             to complete.
!
! program history log:
!   2004-11-29  parrish
!
!   input argument list:
!     in_unit          - fortran unit number where input file is opened through.
!     wrf_ges_filename - filename of input wrf binary restart file
!
!   output argument list:
!     nrecs            - number of sequential records found on input wrf binary restart fil
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

!   do an initial read through of a wrf binary file, and get total number of sequential fil

!   use kinds, only: r_single,i_byte,i_long,i_llong
  implicit none

  integer,intent(in)::in_unit
  character(*),intent(in)::wrf_ges_filename
  integer,intent(out)::nrecs

  integer(i_llong) nextbyte,locbyte,thisblock
  integer(i_byte) lenrec4(4)
  integer(i_long) lenrec,lensave
  equivalence (lenrec4(1),lenrec)
  integer(i_byte) missing4(4)
  integer(i_long) missing
  equivalence (missing,missing4(1))
  integer(i_llong),parameter:: lrecl=2**20
  integer(i_byte) buf(lrecl)
  integer i,loc_count,nreads
  logical lastbuf

  open(in_unit,file=trim(wrf_ges_filename),access='direct',recl=lrecl)
  nrecs=0
  missing=-9999
  nextbyte=0_i_llong
  locbyte=lrecl
  nreads=0
  lastbuf=.false.
  do

!   get length of next record

    do i=1,4
     nextbyte=nextbyte+1_i_llong
     locbyte=locbyte+1_i_llong
     if(locbyte > lrecl .and. lastbuf) go to 900
     if(locbyte > lrecl) call next_buf(in_unit,buf,nextbyte,locbyte,thisblock,lrecl,nreads,lastbuf)
     lenrec4(i)=buf(locbyte)
    end do
    if(lenrec <= 0 .and. lastbuf) go to 900
    if(lenrec <= 0 .and. .not.lastbuf) go to 885
    nextbyte=nextbyte+1_i_llong
    locbyte=locbyte+1_i_llong
    if(locbyte > lrecl .and. lastbuf) go to 900
    if(locbyte > lrecl) call next_buf(in_unit,buf,nextbyte,locbyte,thisblock,lrecl,nreads,lastbuf)

    nrecs=nrecs+1

    loc_count=1
    do i=2,4
       if(loc_count.ge.lenrec) exit
       loc_count=loc_count+1
       nextbyte=nextbyte+1_i_llong
       locbyte=locbyte+1_i_llong
       if(locbyte > lrecl .and. lastbuf) go to 900
       if(locbyte > lrecl) call next_buf(in_unit,buf,nextbyte,locbyte,thisblock,lrecl,nreads,lastbuf)
    end do
    do i=1,4
       if(loc_count.ge.lenrec) exit
       loc_count=loc_count+1
       nextbyte=nextbyte+1_i_llong
       locbyte=locbyte+1_i_llong
       if(locbyte > lrecl .and. lastbuf) go to 900
       if(locbyte > lrecl) call next_buf(in_unit,buf,nextbyte,locbyte,thisblock,lrecl,nreads,lastbuf)
    end do
    nextbyte=nextbyte-loc_count+lenrec
    locbyte=locbyte-loc_count+lenrec
    if(locbyte > lrecl .and. lastbuf) go to 900
    if(locbyte > lrecl) call next_buf(in_unit,buf,nextbyte,locbyte,thisblock,lrecl,nreads,lastbuf)
    lensave=lenrec
    do i=1,4
     nextbyte=nextbyte+1_i_llong
     locbyte=locbyte+1_i_llong
     if(locbyte > lrecl .and. lastbuf) go to 900
     if(locbyte > lrecl) call next_buf(in_unit,buf,nextbyte,locbyte,thisblock,lrecl,nreads,lastbuf)
     lenrec4(i)=buf(locbyte)
    end do
    if(lenrec /= lensave) go to 890

  end do

880  continue
     write(6,*)' reached impossible place in count_recs_wrf_binary_file'
     close(in_unit)
     return

885  continue
     write(6,*)' problem in count_recs_wrf_binary_file, lenrec has bad value before end of file'
     write(6,*)'     lenrec =',lenrec
     close(in_unit)
     return

890  continue
     write(6,*)' problem in count_recs_wrf_binary_file, beginning and ending rec len words unequal'
     write(6,*)'     begining reclen =',lensave
     write(6,*)'       ending reclen =',lenrec
     close(in_unit)
     call wrf_error_fatal("bad reclen stuff")
     return

900  continue
     write(6,*)' normal end of file reached in count_recs_wrf_binary_file'
     write(6,*)'        nblocks=',thisblock
     write(6,*)'          nrecs=',nrecs
     write(6,*)'         nreads=',nreads
     close(in_unit)

end subroutine count_recs_wrf_binary_file

subroutine retrieve_field(in_unit,wrfges,out,start_block,end_block,start_byte,end_byte)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    retrieve_field  retrieve field from wrf binary file
!   prgmmr: parrish          org: np22                date: 2004-11-29
!
! abstract: still using direct access, retrieve a field from the wrf binary restart file.
!
! program history log:
!   2004-11-29  parrish
!
!   input argument list:
!     in_unit          - fortran unit number where input file is opened through.
!     wrfges - filename of input wrf binary restart file
!     start_block      - direct access block number containing 1st byte of record
!                            (after 4 byte record mark)
!     end_block        - direct access block number containing last byte of record
!                            (before 4 byte record mark)
!     start_byte       - relative byte address in direct access block of 1st byte of record
!     end_byte         - relative byte address in direct access block of last byte of record
!
!   output argument list:
!     out              - output buffer where desired field is deposited
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

 ! use kinds, only: i_byte,i_kind
  implicit none

  integer(i_kind),intent(in)::in_unit
  character(50),intent(in)::wrfges
  integer(i_kind),intent(in)::start_block,end_block,start_byte,end_byte
  integer(i_byte),intent(out)::out(*)

  integer(i_kind),parameter:: lrecl=2**20
  integer(i_byte) buf(lrecl)
  integer(i_kind) i,ii,k,ibegin,iend,ierr

  open(in_unit,file=trim(wrfges),access='direct',recl=lrecl)

     write(6,*)' in retrieve_field, start_block,end_block=',start_block,end_block
     write(6,*)' in retrieve_field, start_byte,end_byte=',start_byte,end_byte
  ii=0
  do k=start_block,end_block
     read(in_unit,rec=k,iostat=ierr)buf
     ibegin=1 ; iend=lrecl
     if(k == start_block) ibegin=start_byte
     if(k == end_block) iend=end_byte
     do i=ibegin,iend
        ii=ii+1
        out(ii)=buf(i)
     end do
  end do
  close(in_unit)

end subroutine retrieve_field

! ------------------------------------------------------------

      SUBROUTINE read_wps_bin_arw ( grid, filename, file_date_string, num_metgrid_levels )

      USE module_soil_pre
      USE module_domain

      IMPLICIT NONE
      INCLUDE "mpif.h"


      TYPE(domain) , INTENT(INOUT)  :: grid
      CHARACTER (LEN=19) , INTENT(IN) :: file_date_string
      CHARACTER (LEN=19)              :: VarName
      CHARACTER (LEN=150)             :: chartemp
      CHARACTER (*) , INTENT(IN) :: filename

      INTEGER :: ids,ide,jds,jde,kds,kde           &
                ,ims,ime,jms,jme,kms,kme           &
                ,its,ite,jts,jte,kts,kte

      INTEGER :: i , j , k , loop, IMAX, JMAX
      INTEGER :: DATAHANDLE, num_metgrid_levels
      INTEGER :: Sysdepinfo, Status, N
      INTEGER :: istatus,ioutcount,iret,index,ierr

      integer :: nrecs,iunit, L,hor_size

      LOGICAL :: RAP

!!
      character*132, allocatable,save :: datestr_all(:)
      character*132, allocatable,save :: varname_all(:)
      integer, allocatable,save       :: domainend_all(:,:)
      integer, allocatable,save       :: start_block(:)
      integer, allocatable,save       :: end_block(:)
      integer, allocatable,save       :: start_byte(:)
      integer, allocatable,save       :: end_byte(:)
      integer(kind=i_llong), allocatable, save           :: file_offset(:)
!!

      REAL :: dummy,tmp,garb,garb1,garb2,garb3,garb4
      REAL,ALLOCATABLE,save:: dumdata(:,:,:),dumdata_soil(:,:,:)
      REAL,ALLOCATABLE,save:: dumdata_flex(:,:,:)
      REAL, ALLOCATABLE,save:: dumdata_u(:,:,:)
      REAL, ALLOCATABLE,save:: dumdata_v(:,:,:)
      REAL , DIMENSION(:,:,:), ALLOCATABLE,save :: st_inputx , sm_inputx, sw_inputx

      CHARACTER (LEN= 8) :: dummy_char

      INTEGER :: ok , map_proj , ok_open, igarb
      REAL :: pt
      INTEGER :: num_veg_cat , num_soil_top_cat , num_soil_bot_cat

      SELECT CASE ( model_data_order )
         CASE ( DATA_ORDER_ZXY )
            kds = grid%sd31 ; kde = grid%ed31 ;
            ids = grid%sd32 ; ide = grid%ed32 ;
            jds = grid%sd33 ; jde = grid%ed33 ;

            kms = grid%sm31 ; kme = grid%em31 ;
            ims = grid%sm32 ; ime = grid%em32 ;
            jms = grid%sm33 ; jme = grid%em33 ;

            kts = grid%sp31 ; kte = grid%ep31 ; ! tile is entire patch
            its = grid%sp32 ; ite = grid%ep32 ; ! tile is entire patch
            jts = grid%sp33 ; jte = grid%ep33 ; ! tile is entire patch

         CASE ( DATA_ORDER_XYZ )
            ids = grid%sd31 ; ide = grid%ed31 ;
            jds = grid%sd32 ; jde = grid%ed32 ;
            kds = grid%sd33 ; kde = grid%ed33 ;

            ims = grid%sm31 ; ime = grid%em31 ;
            jms = grid%sm32 ; jme = grid%em32 ;
            kms = grid%sm33 ; kme = grid%em33 ;

            its = grid%sp31 ; ite = grid%ep31 ; ! tile is entire patch
            jts = grid%sp32 ; jte = grid%ep32 ; ! tile is entire patch
            kts = grid%sp33 ; kte = grid%ep33 ; ! tile is entire patch

         CASE ( DATA_ORDER_XZY )
            ids = grid%sd31 ; ide = grid%ed31 ;
            kds = grid%sd32 ; kde = grid%ed32 ;
            jds = grid%sd33 ; jde = grid%ed33 ;

            ims = grid%sm31 ; ime = grid%em31 ;
            kms = grid%sm32 ; kme = grid%em32 ;
            jms = grid%sm33 ; jme = grid%em33 ;

            its = grid%sp31 ; ite = grid%ep31 ; ! tile is entire patch
            kts = grid%sp32 ; kte = grid%ep32 ; ! tile is entire patch
            jts = grid%sp33 ; jte = grid%ep33 ; ! tile is entire patch

      END SELECT

      !  Initialize what soil temperature and moisture is available.


      flag_st000010 = 0
      flag_st010040 = 0
      flag_st040100 = 0
      flag_st100200 = 0
      flag_sm000010 = 0
      flag_sm010040 = 0
      flag_sm040100 = 0
      flag_sm100200 = 0
      flag_st010200 = 0
      flag_sm010200 = 0

      flag_soilt000 = 0
      flag_soilt001 = 0
      flag_soilt004 = 0
      flag_soilt010 = 0
      flag_soilt030 = 0
      flag_soilt040 = 0
      flag_soilt060 = 0
      flag_soilt100 = 0
      flag_soilt160 = 0
      flag_soilt200 = 0 
      flag_soilt300 = 0 

      flag_soilm000 = 0
      flag_soilm001 = 0
      flag_soilm004 = 0
      flag_soilm010 = 0 
      flag_soilm030 = 0 
      flag_soilm040 = 0
      flag_soilm060 = 0
      flag_soilm100 = 0
      flag_soilm160 = 0
      flag_soilm200 = 0
      flag_soilm300 = 0

      flag_sst      = 0
      flag_toposoil = 0

      !  How many soil levels have we found?  Well, right now, none.

      num_st_levels_input = 0
      num_sm_levels_input = 0
      st_levels_input = -1
      sm_levels_input = -1

!tryelim         CALL nl_set_mminlu ( grid%id, 'USGS')
         CALL nl_set_iswater (grid%id, 16 )
         CALL nl_set_isice (grid%id, 24 )


      !  Get the space for the data if this is the first time here.

!	write(6,*) 'pre allocations'
!        call summary()

        IF (.NOT. ALLOCATED (pmsl)              ) ALLOCATE ( pmsl(its:ite,jts:jte) )
        IF (.NOT. ALLOCATED (psfc_in)           ) ALLOCATE ( psfc_in(its:ite,jts:jte) )
        IF (.NOT. ALLOCATED (st_inputx)) ALLOCATE (st_inputx(its:ite,jts:jte,num_st_levels_alloc))
        IF (.NOT. ALLOCATED (sm_inputx)) ALLOCATE (sm_inputx(its:ite,jts:jte,num_st_levels_alloc))
        IF (.NOT. ALLOCATED (sw_inputx)) ALLOCATE (sw_inputx(its:ite,jts:jte,num_st_levels_alloc))
        IF (.NOT. ALLOCATED (soilt010_input)    ) ALLOCATE ( soilt010_input(its:ite,jts:jte) )
        IF (.NOT. ALLOCATED (soilt040_input)    ) ALLOCATE ( soilt040_input(its:ite,jts:jte) )
        IF (.NOT. ALLOCATED (soilt100_input)    ) ALLOCATE ( soilt100_input(its:ite,jts:jte) )
        IF (.NOT. ALLOCATED (soilt200_input)    ) ALLOCATE ( soilt200_input(its:ite,jts:jte) )
        IF (.NOT. ALLOCATED (soilm010_input)    ) ALLOCATE ( soilm010_input(its:ite,jts:jte) )
        IF (.NOT. ALLOCATED (soilm040_input)    ) ALLOCATE ( soilm040_input(its:ite,jts:jte) )
        IF (.NOT. ALLOCATED (soilm100_input)    ) ALLOCATE ( soilm100_input(its:ite,jts:jte) )
        IF (.NOT. ALLOCATED (soilm200_input)    ) ALLOCATE ( soilm200_input(its:ite,jts:jte) )

!       write(6,*) 'past allocations'
!       call summary()

        !  Local arrays

      VarName='CEN_LAT'
!      call retrieve_index(index,VarName,varname_all,nrecs,iret)
!      if (iret /= 0) then
!        print*,VarName," not found in file"
!      else

!        call mpi_file_read_at(iunit,file_offset(index)+5*4,      &
!                              garb,1,mpi_real4,                  &
!                              mpi_status_ignore, ierr)

!        if (ierr /= 0) then
!          print*,"Error reading ", VarName," using MPIIO"
!        else
!          print*,VarName, ' from MPIIO READ= ',garb
!          CALL nl_set_cen_lat ( grid%id , garb )
!          write(0,*) 'cenlat= ', garb
!        end if
!      end if

      VarName='CEN_LON'
!      call retrieve_index(index,VarName,varname_all,nrecs,iret)
!       call mpi_file_read_at(iunit,file_offset(index)+5*4,      &
!                              garb,1,mpi_real4,                  &
!                              mpi_status_ignore, ierr)
!          CALL nl_set_cen_lon ( grid%id , garb )
!          CALL nl_set_stand_lon ( grid%id , garb )

      VarName='TRUELAT1'
!      call retrieve_index(index,VarName,varname_all,nrecs,iret)
!        call mpi_file_read_at(iunit,file_offset(index)+5*4,      &
!                              garb,1,mpi_real4,                  &
!                              mpi_status_ignore, ierr)
!          CALL nl_set_truelat1 ( grid%id , garb )
!
      VarName='TRUELAT2'
!     call retrieve_index(index,VarName,varname_all,nrecs,iret)
!        call mpi_file_read_at(iunit,file_offset(index)+5*4,      &
!!                              garb,1,mpi_real4,                  &
!                              mpi_status_ignore, ierr)
!          CALL nl_set_truelat2 ( grid%id , garb )
!
      VarName='MAP_PROJ'
!      call retrieve_index(index,VarName,varname_all,nrecs,iret)
!        call mpi_file_read_at(iunit,file_offset(index)+5*4,     &
!                              igarb,1,mpi_integer4,             &
!                              mpi_status_ignore, ierr)
!!
!          CALL  nl_set_map_proj( grid%id, igarb)


!    CALL ext_int_ioinit(SysDepInfo,Status)
!    CALL ext_int_open_for_read( trim(fileName), 0, 0, " ", &
!                                DataHandle, Status)

    hor_size=(IDE-IDS)*(JDE-JDS)
        write(0,*) 'hor_size: ', hor_size
        write(0,*) 'IDE, JDE: ', IDE, JDE

        open(unit=47,file=trim(fileName),form='unformatted')

        read(47) garb1,garb2,garb3,garb4,igarb

! if CONUS, expect RAP data, but not if generating boundaries

        if ((garb1 .ge. 39.99 .and. garb1 .le. 40.01) .and. &
            (num_metgrid_levels .ge. 38 .and. num_metgrid_levels .le. 40)  ) then
        RAP=.true.
        else
        RAP=.false.
        endif


        write(0,*) 'garb1: ', garb1
        write(0,*) 'garb3: ', garb3
        write(0,*) 'garb4: ', garb4
        write(0,*) 'igarb: ', igarb

          CALL nl_set_cen_lat ( grid%id , garb1 )

          CALL nl_set_cen_lon ( grid%id , garb2 )
          CALL nl_set_stand_lon ( grid%id , garb2 )

          CALL nl_set_truelat1 ( grid%id , garb3 )
          CALL nl_set_truelat2 ( grid%id , garb4 )

          CALL  nl_set_map_proj( grid%id, igarb)

	if (.not. allocated(dumdata)) then
        write(0,*) 'num_metgrid_levels: ', num_metgrid_levels
    allocate(dumdata(IDS:IDE-1,JDS:JDE-1,num_metgrid_levels))
    allocate(dumdata_u(IDS:IDE,JDS:JDE-1,num_metgrid_levels))
    allocate(dumdata_v(IDS:IDE-1,JDS:JDE,num_metgrid_levels))

        
        if (RAP) then
            allocate(dumdata_soil(IDS:IDE-1,JDS:JDE-1,6))
        else
             allocate(dumdata_soil(IDS:IDE-1,JDS:JDE-1,4))
        endif

	endif

        write(0,*) 'size dumdata_v post alloc block: ', size(dumdata_v,dim=1),&
                                       size(dumdata_v,dim=2),&
                                       size(dumdata_v,dim=3)

! binary write of field: PRES        
        write(0,*) 'to read for PRES'
        write(0,*) 'shape(dumdata): ', shape(dumdata)
       read(47) dumdata
        write(0,*) 'read PRES'
        DO J=JTS,min(JTE,JDE-1)
         DO K=1,num_metgrid_levels
         DO I=ITS,min(ITE,IDE-1)
           grid%p_gc(I,K,J)=dumdata(I,J,K)
        if (I .eq. 50 .and. J .eq. 50) then
        write(0,*) 'K, grid%p_gc: ', K, grid%p_gc(I,K,J)
        endif
         ENDDO
        ENDDO
       ENDDO

        write(0,*) 'size(dumdata_soil): ', size(dumdata_soil,dim=1), &
                   size(dumdata_soil,dim=2), size(dumdata_soil,dim=3)
        write(0,*) 'to dumdata_soil read'
        read(47) ! ignore dumdata_soil
        write(0,*) 'read SOIL_LAYERS'
       read(47) dumdata_soil  ! will fill with ST000010 type fields
        write(0,*) 'read SM'

        read(47) !inogre  dumdata_soil ! will fill with ST000010 type fields
        write(0,*) 'read ST'

        write(0,*) 'to GHT read'
! binary write of field: GHT        
       read(47) dumdata
        write(0,*) 'read GHT'
        DO J=JTS,min(JTE,JDE-1)
       DO K=1,num_metgrid_levels
         DO I=ITS,min(ITE,IDE-1)
           grid%ght_gc(I,K,J)=dumdata(I,J,K)
        if (I .eq. 50 .and. J .eq. 50) then
        write(0,*) 'K, grid%ght_gc: ', K, grid%ght_gc(I,K,J)
        endif
         ENDDO
        ENDDO
       ENDDO


        IF (RAP) THEN

! binary write of field: LANDSEA 
       read(47) dumdata(:,:,1:1) ! ignored

! binary write of field: SEAICE 
        write(0,*) 'to SEAICE'
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%xice(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

! binary write of field: SOILT300
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilt300(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO
        write(0,*) 'grid%soilt300(its+5,jts+5): ', grid%soilt300(its+5,jts+5)

! binary write of field: SOILT160
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilt160(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

        if (num_metgrid_levels .eq. 40) then

! binary write of field: SOILT100
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilt100(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

! binary write of field: SOILT060
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilt060(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO
        write(0,*) 'grid%soilt060(its+5,jts+5): ', grid%soilt060(its+5,jts+5)

! binary write of field: SOILT030
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilt030(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

         endif


        if (num_metgrid_levels .eq. 38 .or. &
            num_metgrid_levels .eq. 39) then ! old RAP

! binary write of field: SOILT040
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilt040(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

! binary write of field: SOILT020
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilt020(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

! binary write of field: SOILT005
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilt005(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO
        
        endif

        if (num_metgrid_levels .eq. 40) then ! para RAP
        
! binary write of field: SOILT010
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilt010(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO
        write(0,*) 'grid%soilt010(its+5,jts+5): ', grid%soilt010(its+5,jts+5)

! binary write of field: SOILT004
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilt004(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

! binary write of field: SOILT001
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilt001(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

        endif

! binary write of field: SOILT000
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilt000(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! binary write of field: SOILM300
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilm300(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

! binary write of field: SOILM160
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilm160(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

        if (num_metgrid_levels .eq. 40) then

! binary write of field: SOILM100
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilm100(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

! binary write of field: SOILM060
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilm060(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

! binary write of field: SOILM030
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilm030(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO
        endif

        if (num_metgrid_levels .eq. 38 .or. &
            num_metgrid_levels .eq. 39) then ! old RAP

! binary write of field: SOILM040
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilm040(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

! binary write of field: SOILM020
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilm020(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

! binary write of field: SOILM005
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilm005(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

        endif

        if (num_metgrid_levels .eq. 40) then
! binary write of field: SOILM010
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilm010(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO
        write(0,*) 'grid%soilm010(its+5,jts+5): ', grid%soilm010(its+5,jts+5)

! binary write of field: SOILM004
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilm004(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

! binary write of field: SOILM001
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilm001(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO
        endif

! binary write of field: SOILM000
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilm000(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

! binary write of field: SNOWH   
        read(47) 

! binary write of field: SKINTEMP  
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%tsk_gc(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

! binary write of field: SNOW   
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%snow(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

! binary write of field: SOILHGT  
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%toposoil(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

        write(0,*) 'maxval(grid%toposoil): ', maxval(grid%toposoil)


        ELSE ! NOT RAP

! binary write of field: SNOWH   
!mp        read(47) 

! binary write of field: SNOW   
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%snow(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

! binary write of field: SKINTEMP  
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%tsk_gc(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

! binary write of field: SOILHGT  
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%toposoil(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

! binary write of field: LANDSEA 
       read(47) dumdata(:,:,1:1) ! ignored

! binary write of field: SEAICE 
        write(0,*) 'to SEAICE'
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%xice(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

! binary write of field: ST100200       
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%st100200(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

! binary write of field: ST040100      
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%st040100(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

! binary write of field: ST010040     
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%st010040(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

! binary write of field: ST000010    
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%st000010(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

! binary write of field: SM100200   
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%sm100200(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

! binary write of field: SM040100  
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%sm040100(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

! binary write of field: SM010040 
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%sm010040(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

! binary write of field: SM000010        
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%sm000010(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

        ENDIF ! RAP

!!!!!!!!!!!!!!!!!!!!!!!!!!!! similar from here down?

! binary write of field: PSFC        
        write(0,*) 'to PSFC'
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%psfc_gc(I,J)=dumdata(I,J,1)
           grid%psfc(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

        write(0,*) 'min/max of PSFC: ', minval(grid%psfc), &
                                        maxval(grid%psfc)


! binary write of field: RH         
       read(47) dumdata
       DO K=1,num_metgrid_levels
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%rh_gc(I,K,J)=dumdata(I,J,K)
        if (I .eq. 50 .and. J .eq. 50) then
        write(0,*) 'K, grid%rh_gc: ', K, grid%rh_gc(I,K,J)
        endif
         ENDDO
        ENDDO
       ENDDO
        
        write(0,*) 'min/max of grid%rh_gc: ', minval(grid%rh_gc), &
                                              maxval(grid%rh_gc)

! binary write of field: VV        
        write(0,*) 'size dumdata_v: ', size(dumdata_v,dim=1),&
                                       size(dumdata_v,dim=2),&
                                       size(dumdata_v,dim=3)
       read(47) dumdata_v
        write(0,*) 'read dumdata_v'
       DO K=1,num_metgrid_levels
        DO J=JTS,min(JTE,JDE)
         DO I=ITS,min(ITE,IDE-1)
           grid%v_gc(I,K,J)=dumdata_v(I,J,K)
         ENDDO
        ENDDO
       ENDDO

! binary write of field: UU       
        write(0,*) 'size dumdata_u: ', size(dumdata_u,dim=1),&
                                       size(dumdata_u,dim=2),&
                                       size(dumdata_u,dim=3)
       read(47) dumdata_u
        write(0,*) 'read dumdata_u'
       DO K=1,num_metgrid_levels
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE)
           grid%u_gc(I,K,J)=dumdata_u(I,J,K)
         ENDDO
        ENDDO
       ENDDO

! binary write of field: TT           
       read(47) dumdata
        write(0,*) 'read dumdata for TT'
       DO K=1,num_metgrid_levels
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%t_gc(I,K,J)=dumdata(I,J,K)
        if (I .eq. 50 .and. J .eq. 50) then
        write(0,*) 'K, grid%t_gc: ', K, grid%t_gc(I,K,J)
        endif
         ENDDO
        ENDDO
       ENDDO

! binary write of field: PMSL 
       read(47) dumdata(:,:,1:1)

        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%pslv_gc(I,J)=dumdata(I,J,1)
        if (I .eq. 50 .and. J .eq. 50) then
        write(0,*) 'grid%pslv_gc: ',  grid%pslv_gc(I,J)
        endif
         ENDDO
        ENDDO

! binary write of field:  VAR_SSO
        read(47) dumdata(:,:,1:1)
        write(0,*) 'read VAR_SSO'
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%var_sso(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO


! binary write of field: SLOPECAT        
       read(47) dumdata(:,:,1:1)
        write(0,*) 'read SLOPECAT'
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%slopecat(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

! binary write of field: SNOALB         
       read(47) dumdata(:,:,1:1)
        write(0,*) 'read SNOALB'
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%snoalb(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

! binary write of field: GREENFRAC 
        write(0,*) 'to read of GREENFRAC'
        write(0,*) 'size(dumdata): ', &
                   size(dumdata,dim=1), &
                   size(dumdata,dim=2), &
                   size(dumdata,dim=3)
       read(47) dumdata(:,:,1:12)
        write(0,*) 'read GREENFRAC'
       DO K=1,12
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%greenfrac(I,K,J)=dumdata(I,J,K)
         ENDDO
        ENDDO
       ENDDO

! binary write of field: ALBEDO12M  
        write(0,*) 'to ALBEDO12M'
       read(47) dumdata(:,:,1:12)
       DO K=1,12
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%albedo12m(I,K,J)=dumdata(I,J,K)
         ENDDO
        ENDDO
       ENDDO

       read(47) ! ignore SCB_DOM

! binary write of field: SOILCBOT  
        write(0,*) 'to SOILCBOT'
       read(47) dumdata(:,:,1:16)
       DO K=1,16
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilcbot(I,K,J)=dumdata(I,J,K)
         ENDDO
        ENDDO
       ENDDO

! SCT_DOM
       read(47) dumdata(:,:,1:1)

! binary write of field: SOILCTOP         291         379          16
       read(47) dumdata(:,:,1:16)
       DO K=1,16
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilctop(I,K,J)=dumdata(I,J,K)
         ENDDO
        ENDDO
       ENDDO

! binary write of field: SOILTEMP         291         379           1
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%tmn_gc(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

! binary write of field: HGT_V         291         379           1
!       read(47) dumdata(:,:,1:1)
!        DO J=JTS,min(JTE,JDE-1)
!         DO I=ITS,min(ITE,IDE-1)
!           grid%htv_gc(I,J)=dumdata(I,J,1)
!         ENDDO
!        ENDDO

! binary write of field: HGT_U         291         379           1
!       read(47) dumdata(:,:,1:1)
!        DO J=JTS,min(JTE,JDE-1)
!         DO I=ITS,min(ITE,IDE-1)
!           grid%htv_gc(I,J)=dumdata(I,J,1)
!         ENDDO
!        ENDDO


        read(47) ! SLPY
        read(47) ! SLPX

! binary write of field: HGT_M 
        write(0,*) 'to HGT_M'
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%ht_gc(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

! binary write of field: LU_INDEX  
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%lu_index(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

! binary write of field: LANDUSEF 
       read(47) dumdata(:,:,1:24)
       DO K=1,24
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%landusef(I,K,J)=dumdata(I,J,K)
        if (I .eq. 50 .and. J .eq. 50) then
        write(0,*) 'I,J,landusef_: ', I,J,grid%landusef(I,K,J)
        endif
         ENDDO
        ENDDO
       ENDDO

! binary write of field: LANDMASK 
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%landmask(I,J)=dumdata(I,J,1)
        if (I .eq. 50 .and. J .eq. 50) then
        write(0,*) 'I,J,landmask:: ', I,J,grid%landmask(I,J)
        endif
         ENDDO
        ENDDO

        write(0,*) 'COSALPHA'
        read(47) dumdata(:,:,1:1)  ! COSALPHA

        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%cosa(I,J)=dumdata(I,J,1)
        if (I .eq. 50 .and. J .eq. 50) then
        write(0,*) 'I,J,cosa:: ', I,J,grid%cosa(I,J)
        endif
         ENDDO
        ENDDO

        write(0,*) 'SINALPHA'
        read(47) dumdata(:,:,1:1)  ! SINALPHA

        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%sina(I,J)=dumdata(I,J,1)
        if (I .eq. 50 .and. J .eq. 50) then
        write(0,*) 'I,J,sina:: ', I,J,grid%sina(I,J)
        endif
         ENDDO
        ENDDO

        write(0,*) 'F'
       read(47) dumdata(:,:,1:1) ! F
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%f(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

        write(0,*) 'E'
       read(47) dumdata(:,:,1:1) ! E
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%e(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

        write(0,*) 'to read for MAPFAC_UY'
                       ! MAPFAC_UY
       read(47) dumdata_u(:,:,1:1)
        write(0,*) 'past read for MAPFAC_UY'

        write(0,*) 'read dumdata_u for MAPFAC_UY'
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE)

           grid%msfuy(I,J)=dumdata_u(I,J,1)

        if (I .eq. 50 .and. J .eq. 50) then
        write(0,*) 'I,J,msfuy: ', I,J,grid%msfuy(I,J)
        endif
         ENDDO
        ENDDO

        write(0,*) 'MAPFAC_VY skip'
       read(47) ! MAPFAC_VY

        write(0,*) 'MAPFAC_MY'
       read(47) ! MAPFAC_MY

        write(0,*) 'MAPFAC_UX skip'
       read(47) ! MAPFAC_UX

        write(0,*) 'MAPFAC_VX skip'
       read(47) ! MAPFAC_VX

        write(0,*) 'MAPFAC_MX skip'
       read(47) ! MAPFAC_MX

               ! MAPFAC_U
        write(0,*) 'MAPFAC_U'
       read(47) dumdata_u(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE)

           grid%msfu(I,J)=dumdata_u(I,J,1)

        if (I .eq. 50 .and. J .eq. 50) then
        write(0,*) 'I,J,msfu: ', I,J,grid%msfu(I,J)
        endif

        if (grid%msfu(I,J) .lt. 0.7 ) then
        write(0,*) 'weird msfu at I,J: ', I,J,grid%msfu(I,J)
        grid%msfu(I,J)=dumdata_u(I-1,J-1,1)
        write(0,*) 'changing msfu to: ',I,J,  grid%msfu(I,J)
        endif

         ENDDO
        ENDDO

                      ! MAPFAC_V
        write(0,*) 'MAPFAC_V'
       read(47) dumdata_v(:,:,1:1)

        DO J=JTS,min(JTE,JDE)
         DO I=ITS,min(ITE,IDE-1)
           grid%msfv(I,J)=dumdata_v(I,J,1)

        if (grid%msfv(I,J) .lt. 0.7 ) then
        write(0,*) 'weird msfv at I,J: ', I,J,grid%msfv(I,J)
        grid%msfv(I,J)=dumdata_v(I,J-1,1)
        if( J .eq. min(JTE,JDE)) then
        write(0,*) 'changing msfv to: ',I,J,  grid%msfv(I,J)
        endif

        endif

        if (I .eq. 50 .and. J .eq. 50) then
        write(0,*) 'I,J,msfv: ', I,J,grid%msfv(I,J)
        endif

         ENDDO
        ENDDO


        ! MAPFAC_M
        write(0,*) 'MAPFAC_M'
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%msft(I,J)=dumdata(I,J,1)

        if (I .eq. 50 .and. J .eq. 50) then
        write(0,*) 'I,J,msft: ', I,J,grid%msft(I,J)
        endif

         ENDDO
        ENDDO

        write(0,*) 'CLONG'
       read(47) ! CLONG
        write(0,*) 'CLAT'
       read(47) ! CLAT

        write(0,*) 'XLONG_U'
       read(47) dumdata_u(:,:,1:1) ! XLONG_U
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE)
           grid%xlong_u(I,J)=dumdata_u(I,J,1)
         ENDDO
        ENDDO

        write(0,*) 'XLAT_U'
       read(47) dumdata_u(:,:,1:1) ! XLAT_U
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE)
           grid%xlat_u(I,J)=dumdata_u(I,J,1)
         ENDDO
        ENDDO



! binary write of field: XLONG_V        
        write(0,*) 'to XLONG_V'
       read(47) dumdata_v(:,:,1:1)
        DO J=JTS,min(JTE,JDE)
         DO I=ITS,min(ITE,IDE-1)
           grid%xlong_v(I,J)=dumdata_v(I,J,1)
         ENDDO
        ENDDO

! binary write of field: XLAT_V        
       read(47) dumdata_v(:,:,1:1)
        DO J=JTS,min(JTE,JDE)
         DO I=ITS,min(ITE,IDE-1)
           grid%xlat_v(I,J)=dumdata_v(I,J,1)
         ENDDO
        ENDDO

! binary write of field: XLONG_M      
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%xlong_gc(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

! binary write of field: XLAT_M         291         379           1
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%xlat_gc(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

         num_veg_cat      = SIZE ( grid%landusef , DIM=2 )
         write(0,*) 'num_veg_cat: ', num_veg_cat
         num_soil_top_cat = SIZE ( grid%soilctop , DIM=2 )
         num_soil_bot_cat = SIZE ( grid%soilcbot , DIM=2 )


       IF (.not. RAP) THEN

       varName='ST000010'
       flag_st000010 = 1
       num_st_levels_input = num_st_levels_input + 1
       st_levels_input(num_st_levels_input) = char2int2(varName(3:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
           st_inputx(I,J,num_st_levels_input + 1) = grid%st000010(i,j)
        ENDDO
       ENDDO

       varName='ST010040'
       flag_st010040 = 1
       num_st_levels_input = num_st_levels_input + 1
       st_levels_input(num_st_levels_input) = char2int2(varName(3:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
            st_inputx(I,J,num_st_levels_input + 1) = grid%st010040(i,j)
        ENDDO
       ENDDO

       varName='ST040100'
       flag_st040100 = 1
       num_st_levels_input = num_st_levels_input + 1
       st_levels_input(num_st_levels_input) = char2int2(varName(3:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
              st_inputx(I,J,num_st_levels_input + 1) = grid%st040100(i,j)
        ENDDO
       ENDDO

       varName='ST100200'
       flag_st100200 = 1
       num_st_levels_input = num_st_levels_input + 1
       st_levels_input(num_st_levels_input) = char2int2(varName(3:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
              st_inputx(I,J,num_st_levels_input + 1) = grid%st100200(i,j)
        ENDDO
       ENDDO

       varName='SM000010'
       flag_sm000010 = 1
       num_sm_levels_input = num_sm_levels_input + 1
       sm_levels_input(num_sm_levels_input) = char2int2(varName(3:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
           sm_inputx(I,J,num_sm_levels_input + 1) = grid%sm000010(i,j)
        ENDDO
       ENDDO

       varName='SM010040'
       flag_sm010040 = 1
       num_sm_levels_input = num_sm_levels_input + 1
       sm_levels_input(num_sm_levels_input) = char2int2(varName(3:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
            sm_inputx(I,J,num_sm_levels_input + 1) = grid%sm010040(i,j)
        ENDDO
       ENDDO

       varName='SM040100'
       flag_sm040100 = 1
       num_sm_levels_input = num_sm_levels_input + 1
       sm_levels_input(num_sm_levels_input) = char2int2(varName(3:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
              sm_inputx(I,J,num_sm_levels_input + 1) = grid%sm040100(i,j)
        ENDDO
       ENDDO

       varName='SM100200'
       flag_sm100200 = 1
       num_sm_levels_input = num_sm_levels_input + 1
       sm_levels_input(num_sm_levels_input) = char2int2(varName(3:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
              sm_inputx(I,J,num_sm_levels_input + 1) = grid%sm100200(i,j)
        ENDDO
       ENDDO


       ELSE ! RAP

       varName='SOILT000'
       flag_soilt000 = 1
       num_st_levels_input = num_st_levels_input + 1
       st_levels_input(num_st_levels_input) = char2int2(varName(6:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
           st_inputx(I,J,num_st_levels_input + 1) = grid%soilt000(i,j)
        ENDDO
       ENDDO

        if (num_metgrid_levels .eq. 40) then
       varName='SOILT001'
       flag_soilt001 = 1
       num_st_levels_input = num_st_levels_input + 1
       st_levels_input(num_st_levels_input) = char2int2(varName(6:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
           st_inputx(I,J,num_st_levels_input + 1) = grid%soilt001(i,j)
        ENDDO
       ENDDO

       varName='SOILT004'
       flag_soilt004 = 1
       num_st_levels_input = num_st_levels_input + 1
       st_levels_input(num_st_levels_input) = char2int2(varName(6:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
           st_inputx(I,J,num_st_levels_input + 1) = grid%soilt004(i,j)
        ENDDO
       ENDDO


       varName='SOILT010'
       flag_soilt010 = 1
       num_st_levels_input = num_st_levels_input + 1
       st_levels_input(num_st_levels_input) = char2int2(varName(6:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
           st_inputx(I,J,num_st_levels_input + 1) = grid%soilt010(i,j)
        ENDDO
       ENDDO

       varName='SOILT030'
       flag_soilt030 = 1
       num_st_levels_input = num_st_levels_input + 1
       st_levels_input(num_st_levels_input) = char2int2(varName(6:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
           st_inputx(I,J,num_st_levels_input + 1) = grid%soilt030(i,j)
        ENDDO
       ENDDO

       varName='SOILT060'
       flag_soilt060 = 1
       num_st_levels_input = num_st_levels_input + 1
       st_levels_input(num_st_levels_input) = char2int2(varName(6:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
           st_inputx(I,J,num_st_levels_input + 1) = grid%soilt060(i,j)
        ENDDO
       ENDDO

       varName='SOILT100'
       flag_soilt100 = 1
       num_st_levels_input = num_st_levels_input + 1
       st_levels_input(num_st_levels_input) = char2int2(varName(6:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
           st_inputx(I,J,num_st_levels_input + 1) = grid%soilt100(i,j)
        ENDDO
       ENDDO 

       else ! old RAP

       varName='SOILT005'
       flag_soilt005 = 1
       num_st_levels_input = num_st_levels_input + 1
       st_levels_input(num_st_levels_input) = char2int2(varName(6:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
           st_inputx(I,J,num_st_levels_input + 1) = grid%soilt005(i,j)
        ENDDO
       ENDDO


       varName='SOILT020'
       flag_soilt020 = 1
       num_st_levels_input = num_st_levels_input + 1
       st_levels_input(num_st_levels_input) = char2int2(varName(6:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
           st_inputx(I,J,num_st_levels_input + 1) = grid%soilt020(i,j)
        ENDDO
       ENDDO

       varName='SOILT040'
       flag_soilt040 = 1
       num_st_levels_input = num_st_levels_input + 1
       st_levels_input(num_st_levels_input) = char2int2(varName(6:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
           st_inputx(I,J,num_st_levels_input + 1) = grid%soilt040(i,j)
        ENDDO
       ENDDO

       endif

       varName='SOILT160'
       flag_soilt160 = 1
       num_st_levels_input = num_st_levels_input + 1
       st_levels_input(num_st_levels_input) = char2int2(varName(6:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
           st_inputx(I,J,num_st_levels_input + 1) = grid%soilt160(i,j)
        ENDDO
       ENDDO

       varName='SOILT300'
       flag_soilt300 = 1
       num_st_levels_input = num_st_levels_input + 1
       st_levels_input(num_st_levels_input) = char2int2(varName(6:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
           st_inputx(I,J,num_st_levels_input + 1) = grid%soilt300(i,j)
        ENDDO
       ENDDO

       varName='SOILM000'
       flag_soilm000 = 1
       num_sm_levels_input = num_sm_levels_input + 1
       sm_levels_input(num_sm_levels_input) = char2int2(varName(6:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
              sm_inputx(I,J,num_sm_levels_input + 1) = grid%soilm000(i,j)
        ENDDO
       ENDDO


        if (num_metgrid_levels .eq. 40) then
       varName='SOILM001'
       flag_soilm001 = 1
       num_sm_levels_input = num_sm_levels_input + 1
       sm_levels_input(num_sm_levels_input) = char2int2(varName(6:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
              sm_inputx(I,J,num_sm_levels_input + 1) = grid%soilm001(i,j)
        ENDDO
       ENDDO

       varName='SOILM004'
       flag_soilm004 = 1
       num_sm_levels_input = num_sm_levels_input + 1
       sm_levels_input(num_sm_levels_input) = char2int2(varName(6:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
              sm_inputx(I,J,num_sm_levels_input + 1) = grid%soilm004(i,j)
        ENDDO
       ENDDO

       varName='SOILM010'
       flag_soilm010 = 1
       num_sm_levels_input = num_sm_levels_input + 1
       sm_levels_input(num_sm_levels_input) = char2int2(varName(6:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
              sm_inputx(I,J,num_sm_levels_input + 1) = grid%soilm010(i,j)
        ENDDO
       ENDDO

       varName='SOILM030'
       flag_soilm030 = 1
       num_sm_levels_input = num_sm_levels_input + 1
       sm_levels_input(num_sm_levels_input) = char2int2(varName(6:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
              sm_inputx(I,J,num_sm_levels_input + 1) = grid%soilm030(i,j)
        ENDDO
       ENDDO

       varName='SOILM060'
       flag_soilm060 = 1
       num_sm_levels_input = num_sm_levels_input + 1
       sm_levels_input(num_sm_levels_input) = char2int2(varName(6:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
              sm_inputx(I,J,num_sm_levels_input + 1) = grid%soilm060(i,j)
        ENDDO
       ENDDO

       varName='SOILM100'
       flag_soilm100 = 1
       num_sm_levels_input = num_sm_levels_input + 1
       sm_levels_input(num_sm_levels_input) = char2int2(varName(6:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
              sm_inputx(I,J,num_sm_levels_input + 1) = grid%soilm100(i,j)
        ENDDO
       ENDDO

       else ! old RAP

       varName='SOILM005'
       flag_soilm005 = 1
       num_sm_levels_input = num_sm_levels_input + 1
       sm_levels_input(num_sm_levels_input) = char2int2(varName(6:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
              sm_inputx(I,J,num_sm_levels_input + 1) = grid%soilm005(i,j)
        ENDDO
       ENDDO

       varName='SOILM020'
       flag_soilm020 = 1
       num_sm_levels_input = num_sm_levels_input + 1
       sm_levels_input(num_sm_levels_input) = char2int2(varName(6:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
              sm_inputx(I,J,num_sm_levels_input + 1) = grid%soilm020(i,j)
        ENDDO
       ENDDO

       varName='SOILM040'
       flag_soilm040 = 1
       num_sm_levels_input = num_sm_levels_input + 1
       sm_levels_input(num_sm_levels_input) = char2int2(varName(6:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
              sm_inputx(I,J,num_sm_levels_input + 1) = grid%soilm040(i,j)
        ENDDO
       ENDDO

        endif

       varName='SOILM160'
       flag_soilm160 = 1
       num_sm_levels_input = num_sm_levels_input + 1
       sm_levels_input(num_sm_levels_input) = char2int2(varName(6:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
              sm_inputx(I,J,num_sm_levels_input + 1) = grid%soilm160(i,j)
        ENDDO
       ENDDO

       varName='SOILM300'
       flag_soilm300 = 1
       num_sm_levels_input = num_sm_levels_input + 1
       sm_levels_input(num_sm_levels_input) = char2int2(varName(6:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
              sm_inputx(I,J,num_sm_levels_input + 1) = grid%soilm300(i,j)
        ENDDO
       ENDDO



       ENDIF ! on rap


            flag_sst = 1

!new
        sw_inputx=0.
!new

      sw_input=0.

        do J=JTS,min(JDE-1,JTE)
         do K=1,num_st_levels_alloc
          do I=ITS,min(IDE-1,ITE)
             st_input(I,K,J)=st_inputx(I,J,K)
             sm_input(I,K,J)=sm_inputx(I,J,K)
             sw_input(I,K,J)=sw_inputx(I,J,K)
          enddo
         enddo
        enddo

        write(0,*) 'maxval st_input(1): ', maxval(st_input(:,1,:))
        write(0,*) 'maxval st_input(2): ', maxval(st_input(:,2,:))
        write(0,*) 'maxval st_input(3): ', maxval(st_input(:,3,:))
        write(0,*) 'maxval st_input(4): ', maxval(st_input(:,4,:))

     end subroutine read_wps_bin_arw

! ------------------------------------------------------------
END MODULE module_wps_io_arw
! ------------------------------------------------------------
