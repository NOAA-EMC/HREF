

MODULE module_wps_io_arw

   USE module_optional_input

   IMPLICIT NONE


















  integer, parameter, public  :: i_byte  = selected_int_kind(1)      
  integer, parameter, public  :: i_short = selected_int_kind(4)      

  integer, parameter, public  :: i_long  = kind(1)                   
  integer, parameter, private :: llong_t = selected_int_kind(16)     
  integer, parameter, public  :: i_llong = max( llong_t, i_long )


  integer, parameter, public :: num_bytes_for_i_byte  = 1
  integer, parameter, public :: num_bytes_for_i_short = 2
  integer, parameter, public :: num_bytes_for_i_long  = 4
  integer, parameter, public :: num_bytes_for_i_llong = 8


  integer, parameter, private :: num_i_kinds = 4
  integer, parameter, dimension( num_i_kinds ), private :: integer_types = (/ &
       i_byte, i_short, i_long,  i_llong  /) 
  integer, parameter, dimension( num_i_kinds ), private :: integer_byte_sizes = (/ &
       num_bytes_for_i_byte, num_bytes_for_i_short, &
       num_bytes_for_i_long, num_bytes_for_i_llong  /)



  integer, parameter, private :: default_integer = 2  
                                                      
                                                      
                                                      
  integer, parameter, public  :: i_kind = integer_types( default_integer )
  integer, parameter, public  :: num_bytes_for_i_kind = &
       integer_byte_sizes( default_integer )





  integer, parameter, public  :: r_single = selected_real_kind(6)  
  integer, parameter, public  :: r_double = selected_real_kind(15) 
  integer, parameter, private :: quad_t   = selected_real_kind(20) 
  integer, parameter, public  :: r_quad   = max( quad_t, r_double )


  integer, parameter, public :: num_bytes_for_r_single = 4
  integer, parameter, public :: num_bytes_for_r_double = 8
  integer, parameter, public :: num_bytes_for_r_quad   = 16


  integer, parameter, private :: num_r_kinds = 3
  integer, parameter, dimension( num_r_kinds ), private :: real_kinds = (/ &
       r_single, r_double, r_quad    /) 
  integer, parameter, dimension( num_r_kinds ), private :: real_byte_sizes = (/ &
       num_bytes_for_r_single, num_bytes_for_r_double, &
       num_bytes_for_r_quad    /)



  integer, parameter, private :: default_real = 2  
                                                   


      


      REAL , DIMENSION(:,:,:) , ALLOCATABLE :: landuse_frac_input , &
                                               soil_top_cat_input , &
                                               soil_bot_cat_input


      

      REAL , DIMENSION(:,:)   , ALLOCATABLE :: soilt010_input , soilt040_input , &
                                               soilt100_input , soilt200_input , &
                                               soilm010_input , soilm040_input , &
                                               soilm100_input , soilm200_input , &
                                               psfc_in,pmsl

      REAL , DIMENSION(:,:)   , ALLOCATABLE :: lat_wind, lon_wind 


      

      REAL,DIMENSION(:,:),ALLOCATABLE :: dum2d
      INTEGER,DIMENSION(:,:),ALLOCATABLE :: idum2d
      REAL,DIMENSION(:,:,:),ALLOCATABLE :: dum3d

      LOGICAL , SAVE :: first_time_in = .TRUE.

      INTEGER :: flag_soilt010 , flag_soilt100 , flag_soilt200 , &
        	 flag_soilm010 , flag_soilm100 , flag_soilm200

      INTEGER :: flag_soilt001, flag_soilt004, flag_soilt030, flag_soilt060
      INTEGER :: flag_soilm001, flag_soilm004, flag_soilm030, flag_soilm060





      CHARACTER (LEN=256) , PRIVATE :: a_message


CONTAINS

   SUBROUTINE read_wps ( grid, filename, file_date_string, num_metgrid_levels )

      USE module_soil_pre
      USE module_domain

      IMPLICIT NONE

      TYPE(domain) , INTENT(INOUT)  :: grid
      CHARACTER (LEN=19) , INTENT(IN) :: file_date_string
      CHARACTER (LEN=4)              :: dummychar
      CHARACTER (LEN=132)              :: VarName
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


      character*132, allocatable :: datestr_all(:)
      character*132, allocatable :: varname_all(:)
      integer, allocatable       :: domainend_all(:,:)
      integer, allocatable       :: start_block(:)
      integer, allocatable       :: end_block(:)
      integer, allocatable       :: start_byte(:)
      integer, allocatable       :: end_byte(:)
      integer(kind=i_llong), allocatable           :: file_offset(:)


      REAL :: dummy,tmp,garb
      REAL, ALLOCATABLE:: dumdata(:,:,:)
      REAL, ALLOCATABLE:: dumdata_u(:,:,:)
      REAL, ALLOCATABLE:: dumdata_v(:,:,:)

      REAL :: lats16(16),lons16(16)

      CHARACTER (LEN= 8) :: dummy_char

      INTEGER :: ok , map_proj , ok_open, igarb,igarb2, N
      REAL :: pt
      INTEGER :: num_veg_cat , num_soil_top_cat , num_soil_bot_cat


      INCLUDE "mpif.h"

      SELECT CASE ( model_data_order )
         CASE ( DATA_ORDER_ZXY )
            kds = grid%sd31 ; kde = grid%ed31 ;
            ids = grid%sd32 ; ide = grid%ed32 ;
            jds = grid%sd33 ; jde = grid%ed33 ;

            kms = grid%sm31 ; kme = grid%em31 ;
            ims = grid%sm32 ; ime = grid%em32 ;
            jms = grid%sm33 ; jme = grid%em33 ;

            kts = grid%sp31 ; kte = grid%ep31 ; 
            its = grid%sp32 ; ite = grid%ep32 ; 
            jts = grid%sp33 ; jte = grid%ep33 ; 

         CASE ( DATA_ORDER_XYZ )
            ids = grid%sd31 ; ide = grid%ed31 ;
            jds = grid%sd32 ; jde = grid%ed32 ;
            kds = grid%sd33 ; kde = grid%ed33 ;

            ims = grid%sm31 ; ime = grid%em31 ;
            jms = grid%sm32 ; jme = grid%em32 ;
            kms = grid%sm33 ; kme = grid%em33 ;

            its = grid%sp31 ; ite = grid%ep31 ; 
            jts = grid%sp32 ; jte = grid%ep32 ; 
            kts = grid%sp33 ; kte = grid%ep33 ; 

         CASE ( DATA_ORDER_XZY )
            ids = grid%sd31 ; ide = grid%ed31 ;
            kds = grid%sd32 ; kde = grid%ed32 ;
            jds = grid%sd33 ; jde = grid%ed33 ;

            ims = grid%sm31 ; ime = grid%em31 ;
            kms = grid%sm32 ; kme = grid%em32 ;
            jms = grid%sm33 ; jme = grid%em33 ;

            its = grid%sp31 ; ite = grid%ep31 ; 
            kts = grid%sp32 ; kte = grid%ep32 ; 
            jts = grid%sp33 ; jte = grid%ep33 ; 

      END SELECT

      


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

      flag_soilt010 = 0
      flag_soilt040 = 0
      flag_soilt100 = 0
      flag_soilt200 = 0 
      flag_soilm010 = 0 
      flag_soilm040 = 0
      flag_soilm100 = 0
      flag_soilm200 = 0

      flag_sst      = 0
      flag_toposoil = 0

      

      num_st_levels_input = 0
      num_sm_levels_input = 0
      st_levels_input = -1
      sm_levels_input = -1

      


        IF (.NOT. ALLOCATED (pmsl)              ) ALLOCATE ( pmsl(its:ite,jts:jte) )
        IF (.NOT. ALLOCATED (psfc_in)           ) ALLOCATE ( psfc_in(its:ite,jts:jte) )



      iunit=33
      call count_recs_wrf_binary_file(iunit, trim(fileName), nrecs)

      allocate (datestr_all(nrecs))
      allocate (varname_all(nrecs))
      allocate (domainend_all(3,nrecs))
      allocate (start_block(nrecs))
      allocate (end_block(nrecs))
      allocate (start_byte(nrecs))
      allocate (end_byte(nrecs))
      allocate (file_offset(nrecs))
 
      call inventory_wrf_binary_file(iunit, trim(filename), nrecs,  &
                      datestr_all,varname_all,domainend_all,   &
                      start_block,end_block,start_byte,end_byte,file_offset)

      call mpi_file_open(mpi_comm_world, trim(filename),     &
                         mpi_mode_rdonly,mpi_info_null, iunit, ierr)
      if (ierr /= 0) then
       call wrf_error_fatal3("module_wps_io_arw.b",279,&
"Error opening file with mpi io")
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
        end if
      end if

      VarName='CEN_LON'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
        call mpi_file_read_at(iunit,file_offset(index)+5*4,      &
                              garb,1,mpi_real4,                  &
                              mpi_status_ignore, ierr)
          CALL nl_set_cen_lon ( grid%id , garb )
          CALL nl_set_stand_lon ( grid%id , garb )

      VarName='POLE_LAT'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
        call mpi_file_read_at(iunit,file_offset(index)+5*4,      &
                              garb,1,mpi_real4,                  &
                              mpi_status_ignore, ierr)
          CALL nl_set_pole_lat ( grid%id , garb )

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
          CALL  nl_set_isurban ( grid%id, igarb)

      VarName='ISWATER'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
        call mpi_file_read_at(iunit,file_offset(index)+5*4,     &
                              igarb,1,mpi_integer4,             &
                              mpi_status_ignore, ierr)
	write(a_message,*) 'setting iswater to be: ', igarb
        CALL wrf_message ( a_message )            
        CALL nl_set_iswater (grid%id, igarb )

      VarName='ISICE'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
        call mpi_file_read_at(iunit,file_offset(index)+5*4,     &
                              igarb2,1,mpi_integer4,             &
                              mpi_status_ignore, ierr)
	write(a_message,*) 'setting isice to be: ', igarb2
        CALL wrf_message ( a_message )            
        CALL nl_set_isice (grid%id, igarb2 )

      IF ( igarb .eq. 16 .and. igarb2 .eq. 24 ) THEN
      CALL nl_set_mminlu ( grid%id, 'USGS')
      ENDIF

      IF ( igarb .eq. 17 .and. igarb2 .eq. 15 ) THEN

      CALL nl_set_mminlu ( grid%id, 'MODIFIED_IGBP_MODIS_NOAH')

      ENDIF

      IF ( igarb .eq. 15 .and. igarb2 .eq. 16 ) THEN
      CALL nl_set_mminlu ( grid%id, 'SiB')
      ENDIF

      VarName='FLAG_SNOWH'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
        call mpi_file_read_at(iunit,file_offset(index)+5*4,     &
                              igarb,1,mpi_integer4,             &
                              mpi_status_ignore, ierr)
        flag_snowh=igarb

      VarName='FLAG_SNOW'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
        call mpi_file_read_at(iunit,file_offset(index)+5*4,     &
                              igarb,1,mpi_integer4,             &
                              mpi_status_ignore, ierr)
        flag_snow=igarb

      VarName='FLAG_METGRID'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
	if (iret .eq. 0) then
        call mpi_file_read_at(iunit,file_offset(index)+5*4,     &
                              igarb,1,mpi_integer4,             &
                              mpi_status_ignore, ierr)
        flag_metgrid=igarb
	endif

      VarName='FLAG_SOILHGT'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
	if (iret .eq. 0) then
        call mpi_file_read_at(iunit,file_offset(index)+5*4,     &
                              igarb,1,mpi_integer4,             &
                              mpi_status_ignore, ierr)
        flag_soilhgt=igarb
	endif

      VarName='FLAG_PSFC'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
	if (iret .eq. 0) then
        call mpi_file_read_at(iunit,file_offset(index)+5*4,     &
                              igarb,1,mpi_integer4,             &
                              mpi_status_ignore, ierr)
        flag_psfc=igarb
	endif

      VarName='FLAG_SLP'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
	if (iret .eq. 0) then
        call mpi_file_read_at(iunit,file_offset(index)+5*4,     &
                              igarb,1,mpi_integer4,             &
                              mpi_status_ignore, ierr)
        flag_slp=igarb
	endif

      VarName='NUM_METGRID_SOIL_LEVELS'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
        call mpi_file_read_at(iunit,file_offset(index)+5*4,     &
                              igarb,1,mpi_integer4,             &
                              mpi_status_ignore, ierr)

        CALL nl_set_num_metgrid_soil_levels(grid%id, igarb)
        num_sw_levels_input=igarb


      VarName='FLAG_SOIL_LEVELS'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
	if (iret .eq. 0) then
        call mpi_file_read_at(iunit,file_offset(index)+5*4,     &
                              igarb,1,mpi_integer4,             &
                              mpi_status_ignore, ierr)
        flag_soil_levels=igarb
	endif


      VarName='FLAG_SOIL_LAYERS'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
        call mpi_file_read_at(iunit,file_offset(index)+5*4,     &
                              igarb,1,mpi_integer4,             &
                              mpi_status_ignore, ierr)
        flag_soil_layers=igarb



      VarName='ISLAKE'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
        call mpi_file_read_at(iunit,file_offset(index)+5*4,     &
                              igarb,1,mpi_integer4,             &
                              mpi_status_ignore, ierr)
          CALL  nl_set_islake ( grid%id, igarb)

      VarName='ISOILWATER'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
        call mpi_file_read_at(iunit,file_offset(index)+5*4,     &
                              igarb,1,mpi_integer4,             &
                              mpi_status_ignore, ierr)
          CALL  nl_set_isoilwater ( grid%id, igarb)

      VarName='MOAD_CEN_LAT'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
        call mpi_file_read_at(iunit,file_offset(index)+5*4,     &
                              garb,1,mpi_real4,             &
                              mpi_status_ignore, ierr)
          CALL  nl_set_moad_cen_lat ( grid%id,garb)

      VarName='corner_lats'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
        call mpi_file_read_at(iunit,file_offset(index)+5*4,     &
                              lats16,16,mpi_real4,             &
                              mpi_status_ignore, ierr)

      VarName='corner_lons'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
        call mpi_file_read_at(iunit,file_offset(index)+5*4,     &
                              lons16,16,mpi_real4,             &
                              mpi_status_ignore, ierr)









    hor_size=(IDE-IDS)*(JDE-JDS)
    hor_size_u=(IDE+1-IDS)*(JDE-JDS)
    hor_size_v=(IDE-IDS)*(JDE+1-JDS)

    varName='PRES'
    allocate(dumdata(IDS:IDE-1,JDS:JDE-1,num_metgrid_levels))
    allocate(dumdata_u(IDS:IDE,JDS:JDE-1,num_metgrid_levels))
    allocate(dumdata_v(IDS:IDE-1,JDS:JDE,num_metgrid_levels))

     CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
     CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                          dumdata,hor_size*num_metgrid_levels,mpi_real4,             &
                          mpi_status_ignore, ierr)


       DO K=1,num_metgrid_levels
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%p_gc(I,K,J)=dumdata(I,J,K)
         ENDDO
        ENDDO
       ENDDO


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

    varName='SNOWH'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata,hor_size,mpi_real4,             &
                             mpi_status_ignore, ierr)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%snowh(I,J)=dumdata(I,J,1)
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

    varName='SOILHGT'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata,hor_size,mpi_real4,             &
                             mpi_status_ignore, ierr)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%toposoil(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO












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
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata_v,hor_size_v*num_metgrid_levels,mpi_real4,             &
                             mpi_status_ignore, ierr)

       DO K=1,num_metgrid_levels
        DO J=JTS,min(JTE,JDE)
         DO I=ITS,min(ITE,IDE-1)
           grid%v_gc(I,K,J)=dumdata_v(I,J,K)
	if (grid%v_gc(I,K,J) .ne. grid%v_gc(I,K,J) .or. abs(grid%v_gc(I,K,J)) .gt. 100.) then
	write(a_message,*) 'bad v_gc defined: ', I,K,J,grid%v_gc(I,K,J)
        CALL wrf_message ( a_message )            
	call wrf_error_fatal3("module_wps_io_arw.b",762,&
" bad v_gc")
	endif
         ENDDO
        ENDDO
       ENDDO

    varName='UU'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata_u,hor_size_u*num_metgrid_levels,mpi_real4,             &
                             mpi_status_ignore, ierr)
       DO K=1,num_metgrid_levels
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE)
           grid%u_gc(I,K,J)=dumdata_u(I,J,K)
	if (grid%u_gc(I,K,J) .ne. grid%u_gc(I,K,J) .or. abs(grid%u_gc(I,K,J)) .gt. 100.) then
	write(a_message,*) 'bad u_gc defined: ', I,K,J,grid%u_gc(I,K,J)
        CALL wrf_message ( a_message )            
	call wrf_error_fatal3("module_wps_io_arw.b",781,&
" bad u_gc")
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
	write(a_message,*) 'bad t_gc defined: ', I,K,J,grid%t_gc(I,K,J)
        CALL wrf_message ( a_message )            
	call wrf_error_fatal3("module_wps_io_arw.b",800,&
" bad t_gc")
	endif
         ENDDO
        ENDDO
       ENDDO



































































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

	write(a_message,*) 'veg_cat and soil_cat sizes:::: ', num_veg_cat , num_soil_top_cat
        CALL wrf_message ( a_message )            

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


        if (grid%msfu(I,J) .lt. 0.7) then
        write(a_message,*) 'weird msfu at I,J: ', I,J,grid%msfu(I,J)
        CALL wrf_message ( a_message )            

        if(J .eq. min(JTE,JDE-1)) then
        grid%msfu(I,J)=dumdata_u(I,J-1,1)
        write(a_message,*) 'changing msfu to: ',I,J,  grid%msfu(I,J)
        CALL wrf_message ( a_message )            
        endif
        
        endif


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
        write(a_message,*) 'weird msfv at I,J: ', I,J,grid%msfv(I,J)
        CALL wrf_message ( a_message )            
        grid%msfv(I,J)=dumdata_v(I,J-1,1)
        if( J .eq. min(JTE,JDE)) then
        write(a_message,*) 'changing msfv to: ',I,J,  grid%msfv(I,J)
        CALL wrf_message ( a_message )            
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

    varName='CLAT'
    CALL retrieve_index(index,VarName,varname_all,nrecs,iret)
    CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                             dumdata,hor_size,mpi_real4,             &
                             mpi_status_ignore, ierr)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%clat(I,J)=dumdata(I,J,1)
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

	write(a_message,*) 'reading XLAT_M'
        CALL wrf_message ( a_message )            
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
	write(a_message,*) 'xlat_gc defined'
        CALL wrf_message ( a_message )            

      call mpi_file_close(mpi_comm_world, ierr)

	write(a_message,*) 'to ST000010 def'
        CALL wrf_message ( a_message )            
       varName='ST000010'
       flag_st000010 = 1
       num_st_levels_input = num_st_levels_input + 1
       st_levels_input(num_st_levels_input) = char2int2(varName(3:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
           st_input(I,num_st_levels_input + 1,J) = grid%st000010(i,j)
        ENDDO
       ENDDO
	write(a_message,*) 'past ST000010 def'
        CALL wrf_message ( a_message )            

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



        write(a_message,*) 'maxval st_input(1): ', maxval(st_input(:,1,:))
        CALL wrf_message ( a_message )            
        write(a_message,*) 'maxval st_input(2): ', maxval(st_input(:,2,:))
        CALL wrf_message ( a_message )            
        write(a_message,*) 'maxval st_input(3): ', maxval(st_input(:,3,:))
        CALL wrf_message ( a_message )            
        write(a_message,*) 'maxval st_input(4): ', maxval(st_input(:,4,:))
        CALL wrf_message ( a_message )            

        DEALLOCATE(pmsl)
        DEALLOCATE(psfc_in)
        DEALLOCATE(dumdata)
        DEALLOCATE(dumdata_u)
        DEALLOCATE(dumdata_v)


     end subroutine read_wps






subroutine retrieve_index(index,string,varname_all,nrecs,iret)





























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



  thisblock = 1_i_llong + (nextbyte-1_i_llong)/lrecl

  locbyte = 1_i_llong+mod(locbyte-1_i_llong,lrecl)

  read(in_unit,rec=thisblock,iostat=ierr)buf
  lastbuf = ierr /= 0

end subroutine next_buf

subroutine inventory_wrf_binary_file(in_unit,wrf_ges_filename,nrecs, &
                                     datestr_all,varname_all,domainend_all, &
                                     start_block,end_block,start_byte,end_byte,file_offset)









































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
  real dummy_field(1)



  integer itypesize
  integer idata(1)
  real rdata(1)

  call wrf_sizeof_integer(itypesize)
  inttypesize=itypesize

  blanks=trim(' ')

	write(a_message,*) 'inventory subroutine'
        CALL wrf_message ( a_message )            

	write(a_message,*) 'opening file : ', trim(wrf_ges_filename)
        CALL wrf_message ( a_message )            

  open(in_unit,file=trim(wrf_ges_filename),access='direct',recl=lrecl)
  irecs=0
  missing=-9999
  nextbyte=0_i_llong
  locbyte=lrecl
  nreads=0
  lastbuf=.false.
  do



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



	
    if(lenrec==2048.and.(hdrbuf(2) == int_dom_ti_char .or. hdrbuf(2) == int_field &
    .or. hdrbuf(2) == int_dom_ti_real .or. hdrbuf(2) == int_dom_ti_integer)) then


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
              write(a_message,*)'(1) irecs,varname,datestr = ',irecs,trim(varname_all(irecs)),trim(datestr_all(irecs))
              CALL wrf_message ( a_message )      

       else if(hdrbuf(2) == int_dom_ti_real) then







          call int_get_ti_header_real(hdrbuf,hdrbufsize,inttypesize,typesize, &
                   datahandle,element,rdata,count,loccode)



          varname_all(irecs)=trim(element)





              write(a_message,*)'(2) irecs,varname,datestr = ',irecs,trim(varname_all(irecs)),rdata(1)
              CALL wrf_message ( a_message )      

         
       else if(hdrbuf(2) == int_dom_ti_integer) then

          call int_get_ti_header_integer(hdrbuf,hdrbufsize,inttypesize,typesize, &
                   datahandle,element,idata,count,loccode)
          varname_all(irecs)=trim(element)

              write(0,*)'(3) irecs,varname,datestr = ',irecs,trim(varname_all(irecs)),idata(1:count)

       else


          call int_get_write_field_header(hdrbuf,hdrbufsize,inttypesize,typesize, &
             datahandle,datestr,varname,dummy_field,fieldtype,comm,iocomm, &
             domaindesc,memoryorder,stagger,dimnames, &
             domainstart,domainend,memorystart,memoryend,patchstart,patchend)
          varname_all(irecs)=trim(varname)
          datestr_all(irecs)=trim(datestr)
          domainend_all(1:3,irecs)=domainend(1:3)



       end if
    end if

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
     write(a_message,*)' problem in inventory_wrf_binary_file, lenrec has bad value before end of file'
     CALL wrf_message ( a_message )               
     write(a_message,*)'     lenrec =',lenrec
     CALL wrf_message ( a_message )               
     close(in_unit)
     return

890  continue
     write(a_message,*)' problem in inventory_wrf_binary_file, beginning and ending rec len words unequal'
     CALL wrf_message ( a_message )               
     write(a_message,*)'     begining reclen =',lensave
     CALL wrf_message ( a_message )               
     write(a_message,*)'       ending reclen =',lenrec
     CALL wrf_message ( a_message )               
     write(a_message,*)'               irecs =',irecs
     CALL wrf_message ( a_message )               
     write(a_message,*)'               nrecs =',nrecs
     CALL wrf_message ( a_message )               
     call wrf_error_fatal3("module_wps_io_arw.b",1742,&
"curious reclen discrepancy")
     close(in_unit)
     return

900  continue
     write(a_message,*)' normal end of file reached in inventory_wrf_binary_file'
     CALL wrf_message ( a_message )               
     write(a_message,*)'        nblocks=',thisblock
     CALL wrf_message ( a_message )               
     write(a_message,*)'          irecs,nrecs=',irecs,nrecs
     CALL wrf_message ( a_message )               
     write(a_message,*)'         nreads=',nreads
     CALL wrf_message ( a_message )               
     close(in_unit)

end subroutine inventory_wrf_binary_file

SUBROUTINE wrf_sizeof_integer( retval )
  IMPLICIT NONE
  INTEGER retval

  retval = 4
  RETURN
END SUBROUTINE wrf_sizeof_integer

SUBROUTINE wrf_sizeof_real( retval )
  IMPLICIT NONE
  INTEGER retval

  retval = 4
  RETURN
END SUBROUTINE wrf_sizeof_real





subroutine count_recs_wrf_binary_file(in_unit,wrf_ges_filename,nrecs)








































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
     call wrf_error_fatal3("module_wps_io_arw.b",1915,&
"bad reclen stuff")
     return

900  continue
     write(6,*)' normal end of file reached in count_recs_wrf_binary_file'
     write(6,*)'        nblocks=',thisblock
     write(6,*)'          nrecs=',nrecs
     write(6,*)'         nreads=',nreads
     close(in_unit)

end subroutine count_recs_wrf_binary_file

subroutine retrieve_field(in_unit,wrfges,out,start_block,end_block,start_byte,end_byte)





























 
  implicit none

  integer(i_kind),intent(in)::in_unit
  character(50),intent(in)::wrfges
  integer(i_kind),intent(in)::start_block,end_block,start_byte,end_byte
  integer(i_byte),intent(out)::out(*)

  integer(i_llong),parameter:: lrecl=2**20
  integer(i_byte) buf(lrecl)
  integer(i_kind) i,ii,k
  integer(i_llong) ibegin,iend,ierr

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


      character*132, allocatable,save :: datestr_all(:)
      character*132, allocatable,save :: varname_all(:)
      integer, allocatable,save       :: domainend_all(:,:)
      integer, allocatable,save       :: start_block(:)
      integer, allocatable,save       :: end_block(:)
      integer, allocatable,save       :: start_byte(:)
      integer, allocatable,save       :: end_byte(:)
      integer(kind=i_llong), allocatable, save           :: file_offset(:)


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

            kts = grid%sp31 ; kte = grid%ep31 ; 
            its = grid%sp32 ; ite = grid%ep32 ; 
            jts = grid%sp33 ; jte = grid%ep33 ; 

         CASE ( DATA_ORDER_XYZ )
            ids = grid%sd31 ; ide = grid%ed31 ;
            jds = grid%sd32 ; jde = grid%ed32 ;
            kds = grid%sd33 ; kde = grid%ed33 ;

            ims = grid%sm31 ; ime = grid%em31 ;
            jms = grid%sm32 ; jme = grid%em32 ;
            kms = grid%sm33 ; kme = grid%em33 ;

            its = grid%sp31 ; ite = grid%ep31 ; 
            jts = grid%sp32 ; jte = grid%ep32 ; 
            kts = grid%sp33 ; kte = grid%ep33 ; 

         CASE ( DATA_ORDER_XZY )
            ids = grid%sd31 ; ide = grid%ed31 ;
            kds = grid%sd32 ; kde = grid%ed32 ;
            jds = grid%sd33 ; jde = grid%ed33 ;

            ims = grid%sm31 ; ime = grid%em31 ;
            kms = grid%sm32 ; kme = grid%em32 ;
            jms = grid%sm33 ; jme = grid%em33 ;

            its = grid%sp31 ; ite = grid%ep31 ; 
            kts = grid%sp32 ; kte = grid%ep32 ; 
            jts = grid%sp33 ; jte = grid%ep33 ; 

      END SELECT

      


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

      

      num_st_levels_input = 0
      num_sm_levels_input = 0
      st_levels_input = -1
      sm_levels_input = -1


         CALL nl_set_iswater (grid%id, 16 )
         CALL nl_set_isice (grid%id, 24 )


      




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




        

      VarName='CEN_LAT'


















      VarName='CEN_LON'







      VarName='TRUELAT1'






      VarName='TRUELAT2'






      VarName='MAP_PROJ'












    hor_size=(IDE-IDS)*(JDE-JDS)
        write(0,*) 'hor_size: ', hor_size
        write(0,*) 'IDE, JDE: ', IDE, JDE

        open(unit=47,file=trim(fileName),form='unformatted')

        read(47) garb1,garb2,garb3,garb4,igarb



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
        read(47) 
        write(0,*) 'read SOIL_LAYERS'
       read(47) dumdata_soil  
        write(0,*) 'read SM'

        read(47) 
        write(0,*) 'read ST'

        write(0,*) 'to GHT read'

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


       read(47) dumdata(:,:,1:1) 


        write(0,*) 'to SEAICE'
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%xice(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO


       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilt300(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO
        write(0,*) 'grid%soilt300(its+5,jts+5): ', grid%soilt300(its+5,jts+5)


       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilt160(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

        if (num_metgrid_levels .eq. 40) then


       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilt100(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO


       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilt060(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO
        write(0,*) 'grid%soilt060(its+5,jts+5): ', grid%soilt060(its+5,jts+5)


       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilt030(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

         endif


        if (num_metgrid_levels .eq. 38 .or. &
            num_metgrid_levels .eq. 39) then 


       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilt040(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO


       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilt020(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO


       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilt005(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO
        
        endif

        if (num_metgrid_levels .eq. 40) then 
        

       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilt010(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO
        write(0,*) 'grid%soilt010(its+5,jts+5): ', grid%soilt010(its+5,jts+5)


       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilt004(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO


       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilt001(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

        endif


       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilt000(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO




       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilm300(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO


       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilm160(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

        if (num_metgrid_levels .eq. 40) then


       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilm100(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO


       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilm060(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO


       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilm030(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO
        endif

        if (num_metgrid_levels .eq. 38 .or. &
            num_metgrid_levels .eq. 39) then 


       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilm040(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO


       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilm020(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO


       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilm005(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

        endif

        if (num_metgrid_levels .eq. 40) then

       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilm010(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO
        write(0,*) 'grid%soilm010(its+5,jts+5): ', grid%soilm010(its+5,jts+5)


       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilm004(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO


       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilm001(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO
        endif


       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilm000(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO


        read(47) 


       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%tsk_gc(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO


       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%snow(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO


       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%toposoil(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

        write(0,*) 'maxval(grid%toposoil): ', maxval(grid%toposoil)


        ELSE 





       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%snow(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO


       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%tsk_gc(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO


       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%toposoil(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO


       read(47) dumdata(:,:,1:1) 


        write(0,*) 'to SEAICE'
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%xice(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO


       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%st100200(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO


       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%st040100(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO


       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%st010040(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO


       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%st000010(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO


       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%sm100200(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO


       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%sm040100(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO


       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%sm010040(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO


       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%sm000010(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

        ENDIF 




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


       read(47) dumdata(:,:,1:1)

        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%pslv_gc(I,J)=dumdata(I,J,1)
        if (I .eq. 50 .and. J .eq. 50) then
        write(0,*) 'grid%pslv_gc: ',  grid%pslv_gc(I,J)
        endif
         ENDDO
        ENDDO


        read(47) dumdata(:,:,1:1)
        write(0,*) 'read VAR_SSO'
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%var_sso(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO



       read(47) dumdata(:,:,1:1)
        write(0,*) 'read SLOPECAT'
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%slopecat(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO


       read(47) dumdata(:,:,1:1)
        write(0,*) 'read SNOALB'
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%snoalb(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO


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


        write(0,*) 'to ALBEDO12M'
       read(47) dumdata(:,:,1:12)
       DO K=1,12
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%albedo12m(I,K,J)=dumdata(I,J,K)
         ENDDO
        ENDDO
       ENDDO

       read(47) 


        write(0,*) 'to SOILCBOT'
       read(47) dumdata(:,:,1:16)
       DO K=1,16
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilcbot(I,K,J)=dumdata(I,J,K)
         ENDDO
        ENDDO
       ENDDO


       read(47) dumdata(:,:,1:1)


       read(47) dumdata(:,:,1:16)
       DO K=1,16
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%soilctop(I,K,J)=dumdata(I,J,K)
         ENDDO
        ENDDO
       ENDDO


       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%tmn_gc(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO


















        read(47) 
        read(47) 


        write(0,*) 'to HGT_M'
       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%ht_gc(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO


       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%lu_index(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO


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
        read(47) dumdata(:,:,1:1)  

        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%cosa(I,J)=dumdata(I,J,1)
        if (I .eq. 50 .and. J .eq. 50) then
        write(0,*) 'I,J,cosa:: ', I,J,grid%cosa(I,J)
        endif
         ENDDO
        ENDDO

        write(0,*) 'SINALPHA'
        read(47) dumdata(:,:,1:1)  

        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%sina(I,J)=dumdata(I,J,1)
        if (I .eq. 50 .and. J .eq. 50) then
        write(0,*) 'I,J,sina:: ', I,J,grid%sina(I,J)
        endif
         ENDDO
        ENDDO

        write(0,*) 'F'
       read(47) dumdata(:,:,1:1) 
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%f(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

        write(0,*) 'E'
       read(47) dumdata(:,:,1:1) 
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%e(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

        write(0,*) 'to read for MAPFAC_UY'
                       
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
       read(47) 

        write(0,*) 'MAPFAC_MY'
       read(47) 

        write(0,*) 'MAPFAC_UX skip'
       read(47) 

        write(0,*) 'MAPFAC_VX skip'
       read(47) 

        write(0,*) 'MAPFAC_MX skip'
       read(47) 

               
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
       read(47) 
        write(0,*) 'CLAT'
       read(47) 

        write(0,*) 'XLONG_U'
       read(47) dumdata_u(:,:,1:1) 
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE)
           grid%xlong_u(I,J)=dumdata_u(I,J,1)
         ENDDO
        ENDDO

        write(0,*) 'XLAT_U'
       read(47) dumdata_u(:,:,1:1) 
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE)
           grid%xlat_u(I,J)=dumdata_u(I,J,1)
         ENDDO
        ENDDO




        write(0,*) 'to XLONG_V'
       read(47) dumdata_v(:,:,1:1)
        DO J=JTS,min(JTE,JDE)
         DO I=ITS,min(ITE,IDE-1)
           grid%xlong_v(I,J)=dumdata_v(I,J,1)
         ENDDO
        ENDDO


       read(47) dumdata_v(:,:,1:1)
        DO J=JTS,min(JTE,JDE)
         DO I=ITS,min(ITE,IDE-1)
           grid%xlat_v(I,J)=dumdata_v(I,J,1)
         ENDDO
        ENDDO


       read(47) dumdata(:,:,1:1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%xlong_gc(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO


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


       ELSE 

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

       else 

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

       else 

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



       ENDIF 


            flag_sst = 1


        sw_inputx=0.


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


END MODULE module_wps_io_arw
