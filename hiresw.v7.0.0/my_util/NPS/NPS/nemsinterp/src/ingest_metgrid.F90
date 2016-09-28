MODULE ingest_metgrid

   use gridinfo_module
   use misc_definitions_module
   use dio

   IMPLICIT NONE

      !  Input 3D LSM fields.

      REAL , DIMENSION(:,:,:) , ALLOCATABLE :: landuse_frac_input , &
                                               soil_top_cat_input , &
                                               soil_bot_cat_input

      !  Input 2D surface fields.

      REAL , DIMENSION(:,:)   , ALLOCATABLE ::  psfc_in, pmsl

      !  Local input arrays

      REAL,DIMENSION(:,:),ALLOCATABLE :: dum2d
      INTEGER,DIMENSION(:,:),ALLOCATABLE :: idum2d
      REAL,DIMENSION(:,:,:),ALLOCATABLE :: dum3d

      LOGICAL , SAVE :: first_time_in = .TRUE.

!   Some constants to allow simple dimensions in the defined types
!   given below.

      INTEGER, PARAMETER          :: var_maxdims = 5
      INTEGER, PARAMETER          :: max_staggers_xy_new = 4
      INTEGER, PARAMETER          :: max_staggers_xy_old = 3
      INTEGER, PARAMETER          :: max_staggers_z = 2
      INTEGER, PARAMETER          :: max_standard_lats = 4
      INTEGER, PARAMETER          :: max_standard_lons = 4
      INTEGER, PARAMETER          :: max_fg_variables = 200
      INTEGER, PARAMETER          :: max_vertical_levels = 2000

     ! WRF I/O API related variables
     integer :: handle
     type(dio_file) :: dfile

     CHARACTER (LEN=19) :: DateStr

CONTAINS


   SUBROUTINE READ_NPS ( grid, filename, file_date_string, num_metgrid_levels, &
                         ncep_processing, do_gwd, do_clouds, no_seaice, spectral,use_igbp,grib_src, &
                         ITS,ITE,JTS,JTE , &
                         IMS,IME,JMS,JME )

      use module_debug
      USE module_data

      IMPLICIT NONE

      ! Arguments
      TYPE(input_vars) , INTENT(INOUT)  :: grid
      CHARACTER (*) , INTENT(IN) :: filename
      CHARACTER (LEN=19) , INTENT(IN) :: file_date_string
      CHARACTER (LEN=3) , INTENT(IN) :: grib_src
      INTEGER, INTENT(OUT) :: num_metgrid_levels
      INTEGER, INTENT(IN) :: ITS,ITE,JTS,JTE , &
                             IMS,IME,JMS,JME
      LOGICAL :: ncep_processing, do_gwd, do_clouds, no_seaice, spectral,use_igbp
      LOGICAL :: print_diag

#include "wrf_io_flags.h"
#include "wrf_status_codes.h"

      ! Local variables
      CHARACTER (LEN=79)              :: VarName
      CHARACTER (LEN=150)             :: chartemp
      character (len=30)              :: recname
      character (len=2)               :: lchar


      INTEGER :: ids,ide,jds,jde,kds,kde           &
                ,kms,kme           &
                ,kts,kte, KCAT

      INTEGER :: i , j , k , loop, IMAX, JMAX, N
      INTEGER :: DATAHANDLE
      INTEGER :: Sysdepinfo, Status
      INTEGER :: istatus,ioutcount,iret,index,ierr
      integer :: comm_1, comm_2

      integer :: nrecs,iunit, L,hor_size, Ilook,Jlook

!      integer, parameter:: numsoil=4

      integer :: numsoil


      REAL :: dummy,tmp,rmax
      REAL, ALLOCATABLE:: dumdata(:,:,:)

      CHARACTER (LEN= 8) :: dummy_char

      INTEGER :: ok , map_proj , ok_open, igarb
      REAL :: pt
      INTEGER :: num_veg_cat , num_soil_top_cat , num_soil_bot_cat

      LOGICAL:: memory_allocate


      DateStr = file_date_string
      !  Get the space for the data if this is the first time here.

      memory_allocate=.true.

      istatus = 0
#ifdef IO_NETCDF
      if (io_form_input == NETCDF_FORMAT) call ext_ncd_ioinit('sysdep info', istatus)
#endif
#ifdef IO_DIO
      if (io_form_input == DIO_FORMAT) call dio_init(iret=istatus)
#endif
      call mprintf((istatus /= 0),ERROR,'Error in ext_pkg_ioinit')

      comm_1 = 1
      comm_2 = 1

#ifdef IO_NETCDF
      if (io_form_input == NETCDF_FORMAT) &
            call ext_ncd_open_for_read(trim(filename), comm_1, comm_2, 'sysdep info', handle, istatus)
#endif
#ifdef IO_DIO
      if (io_form_input == DIO_FORMAT) &
            call dio_open(dfile,trim(filename),"READ",iret=istatus)
#endif
      call mprintf((istatus /= 0),ERROR,'Couldn''t open file %s for input.',s1=filename)

      call ext_get_dom_ti_integer_scalar('WEST-EAST_GRID_DIMENSION', IDE)
      call ext_get_dom_ti_integer_scalar('SOUTH-NORTH_GRID_DIMENSION', JDE)
      call ext_get_dom_ti_integer_scalar('BOTTOM-TOP_GRID_DIMENSION', num_metgrid_levels)

      IDS=1
      IDE=IDE+1

      grid%IDS=IDS
      grid%ITS=ITS
      grid%IDE=IDE
      grid%ITE=ITE
      grid%IMS=IMS
      grid%IME=IME

      JDS=1
      JDE=JDE+1

      grid%JDS=JDS
      grid%JTS=JTS
      grid%JDE=JDE
      grid%JTE=JTE
      grid%JMS=JMS
      grid%JME=JME

      if (ITS .eq. 1 .and. JTS .eq. 1) then
        print_diag=.true. 
      else
        print_diag=.false.
      endif


        if (grib_src .eq. 'RAP' ) then
        numsoil=4
        else
        numsoil=4
        endif

      grid%numsoil=numsoil

   

!       grid%ncep_processing=ncep_processing

	if  (ncep_processing .and. print_diag) then
	write(0,*) 'NCEP_PROCESSING'
	write(0,*) 'need to treat things like albedo and vegfrac specially now'
	endif

      

      hor_size=(IDE-IDS)*(JDE-JDS)

	if (print_diag) then
      write(0,*) 'hor_size: ', hor_size
      write(0,*) 'IDE, JDE: ', IDE, JDE
	endif

       IF (allocated(grid%PRES)) THEN
         if(print_diag) write(0,*) 'everything already allocated'
       ELSE
         if(print_diag) write(0,*) 'allocating everything....'
         IF (memory_allocate) THEN

            allocate(grid%PRES(IMS:IME,JMS:JME,num_metgrid_levels))     !;  grid%PRES = X'FF911299'
            if (spectral) then
            allocate(grid%PINT(IMS:IME,JMS:JME,num_metgrid_levels))     !;  grid%PINT = X'FF911299'
            allocate(grid%SPECHUMD(IMS:IME,JMS:JME,num_metgrid_levels))	!;  grid%SPECHUMD = X'FF911299'
            endif
            allocate(grid%GHT(IMS:IME,JMS:JME,num_metgrid_levels))      !;  grid%GHT = X'FF911299'
            allocate(grid%RH(IMS:IME,JMS:JME,num_metgrid_levels))	!;  grid%RH = X'FF911299'
            allocate(grid%VV(IMS:IME,JMS:JME,num_metgrid_levels))	!;  grid%VV = X'FF911299'
            allocate(grid%UU(IMS:IME,JMS:JME,num_metgrid_levels))	!;  grid%UU = X'FF911299'
            allocate(grid%TT(IMS:IME,JMS:JME,num_metgrid_levels))	!;  grid%TT = X'FF911299'
            allocate(grid%FRIMEF(IMS:IME,JMS:JME,num_metgrid_levels))	!;  grid%FRIMEF = X'FF911299'
            allocate(grid%RWMR(IMS:IME,JMS:JME,num_metgrid_levels))	!;  grid%RWMR = X'FF911299'
            allocate(grid%SNMR(IMS:IME,JMS:JME,num_metgrid_levels))	!;  grid%SNMR = X'FF911299'
            allocate(grid%CICE(IMS:IME,JMS:JME,num_metgrid_levels))	!;  grid%CICE = X'FF911299'
            allocate(grid%CLWMR(IMS:IME,JMS:JME,num_metgrid_levels))	!;  grid%CLWMR = X'FF911299'
            allocate(grid%VEGCAT(IMS:IME,JMS:JME))			!;  grid%VEGCAT = X'FF911299'
            allocate(grid%SOILCAT(IMS:IME,JMS:JME))			!;  grid%SOILCAT = X'FF911299'
            allocate(grid%CANWAT(IMS:IME,JMS:JME))			!;  grid%CANWAT = X'FF911299'
            allocate(grid%SNOW(IMS:IME,JMS:JME))			!;  grid%SNOW = X'FF911299'
            allocate(grid%SKINTEMP(IMS:IME,JMS:JME))			!;  grid%SKINTEMP = X'FF911299'
            allocate(grid%SOILHGT(IMS:IME,JMS:JME))			!;  grid%SOILHGT = X'FF911299'
            allocate(grid%STDVTOPO(IMS:IME,JMS:JME))			!;  grid%STDVTOPO = X'FF911299'
            allocate(grid%SEAICE(IMS:IME,JMS:JME))			!;  grid%SEAICE = X'FF911299'
            allocate(grid%STC_WPS(IMS:IME,JMS:JME,numsoil))		!;  grid%STC_WPS = X'FF911299'
            allocate(grid%SMC_WPS(IMS:IME,JMS:JME,numsoil))		!;  grid%SMC_WPS = X'FF911299'
            allocate(grid%PSFC(IMS:IME,JMS:JME))			!;  grid%PSFC = X'FF911299'
            allocate(grid%SLOPECAT(IMS:IME,JMS:JME))			!;  grid%SLOPECAT = X'FF911299'
            allocate(grid%SNOALB(IMS:IME,JMS:JME))			!;  grid%SNOALB = X'FF911299'
            allocate(grid%GREENFRAC(IMS:IME,JMS:JME,12))		!;  grid%GREENFRAC = X'FF911299'
       if (ncep_processing) then
            allocate(grid%ALBEDO12M(IMS:IME,JMS:JME,4))		!;  grid%ALBEDO12M = X'FF911299'
       else
            allocate(grid%ALBEDO12M(IMS:IME,JMS:JME,12))		!;  grid%ALBEDO12M = X'FF911299'
       endif
            allocate(grid%SOILCTOP(IMS:IME,JMS:JME,16)) 		!;  grid%SOILCTOP = X'FF911299'
            allocate(grid%SOILTEMP(IMS:IME,JMS:JME))			!;  grid%SOILTEMP = X'FF911299'
            allocate(grid%HGT_M(IMS:IME,JMS:JME))			!;  grid%HGT_M = X'FF911299'
            allocate(grid%LU_INDEX(IMS:IME,JMS:JME))			!;  grid%LU_INDEX = X'FF911299'
	if (use_igbp) then
            allocate(grid%LANDUSEF(IMS:IME,JMS:JME,20)) 		!;  grid%LANDUSEF = X'FF911299'
        else
            allocate(grid%LANDUSEF(IMS:IME,JMS:JME,24)) 		!;  grid%LANDUSEF = X'FF911299'
        endif
            allocate(grid%LANDMASK(IMS:IME,JMS:JME))			!;  grid%LANDMASK = X'FF911299'
       if (grid%gtype .ne. 'A') then
            allocate(grid%XLONG_V(IMS:IME,JMS:JME))			!;  grid%XLONG_V = X'FF911299'
            allocate(grid%XLAT_V(IMS:IME,JMS:JME))			!;  grid%XLAT_V = X'FF911299'
       endif
            allocate(grid%XLONG_M(IMS:IME,JMS:JME))			!;  grid%XLONG_M = X'FF911299'
            allocate(grid%XLAT_M(IMS:IME,JMS:JME))			!;  grid%XLAT_M = X'FF911299'
       if (ncep_processing .and. do_gwd) then
            allocate(grid%GWD_OROG(IMS:IME,JMS:JME,14))			!;  grid%GWD_OROG = X'FF911299'
       endif

         ELSE

            allocate(grid%PRES(ITS:min(ITE,IDE-1),JTS:min(JTE,JDE-1),num_metgrid_levels))  !;  grid%PRES = X'FF911299'
            if (spectral) then
            allocate(grid%PINT(ITS:min(ITE,IDE-1),JTS:min(JTE,JDE-1),num_metgrid_levels))     !;  grid%PINT = X'FF911299'
            allocate(grid%SPECHUMD(ITS:min(ITE,IDE-1),JTS:min(JTE,JDE-1),num_metgrid_levels))     !;  grid%SPECHUMD = X'FF911299'
            endif
            allocate(grid%GHT(ITS:min(ITE,IDE-1),JTS:min(JTE,JDE-1),num_metgrid_levels))   !;  grid%GHT = X'FF911299'
            allocate(grid%RH(ITS:min(ITE,IDE-1),JTS:min(JTE,JDE-1),num_metgrid_levels))    !;  grid%RH = X'FF911299'
            allocate(grid%VV(ITS:min(ITE,IDE-1),JTS:min(JTE,JDE-1),num_metgrid_levels))    !;  grid%VV = X'FF911299'
            allocate(grid%UU(ITS:min(ITE,IDE-1),JTS:min(JTE,JDE-1),num_metgrid_levels))    !;  grid%UU = X'FF911299'
            allocate(grid%TT(ITS:min(ITE,IDE-1),JTS:min(JTE,JDE-1),num_metgrid_levels))    !;  grid%TT = X'FF911299'
            allocate(grid%FRIMEF(ITS:min(ITE,IDE-1),JTS:min(JTE,JDE-1),num_metgrid_levels))	!;  grid%FRIMEF = X'FF911299'
            allocate(grid%RWMR(ITS:min(ITE,IDE-1),JTS:min(JTE,JDE-1),num_metgrid_levels))	!;  grid%RWMR = X'FF911299'
            allocate(grid%SNMR(ITS:min(ITE,IDE-1),JTS:min(JTE,JDE-1),num_metgrid_levels))	!;  grid%SNMR = X'FF911299'
            allocate(grid%CICE(ITS:min(ITE,IDE-1),JTS:min(JTE,JDE-1),num_metgrid_levels))	!;  grid%CICE = X'FF911299'
            allocate(grid%CLWMR(ITS:min(ITE,IDE-1),JTS:min(JTE,JDE-1),num_metgrid_levels))	!;  grid%CICE = X'FF911299'
            allocate(grid%VEGCAT(ITS:min(ITE,IDE-1),JTS:min(JTE,JDE-1)))		   !;  grid%VEGCAT = X'FF911299'
            allocate(grid%SOILCAT(ITS:min(ITE,IDE-1),JTS:min(JTE,JDE-1)))		   !;  grid%SOILCAT = X'FF911299'
            allocate(grid%CANWAT(ITS:min(ITE,IDE-1),JTS:min(JTE,JDE-1)))		   !;  grid%CANWAT = X'FF911299'
            allocate(grid%SNOW(ITS:min(ITE,IDE-1),JTS:min(JTE,JDE-1)))			   !;  grid%SNOW = X'FF911299'
            allocate(grid%SKINTEMP(ITS:min(ITE,IDE-1),JTS:min(JTE,JDE-1)))		   !;  grid%SKINTEMP = X'FF911299'
            allocate(grid%SOILHGT(ITS:min(ITE,IDE-1),JTS:min(JTE,JDE-1)))		   !;  grid%SOILHGT = X'FF911299'
            allocate(grid%STDVTOPO(ITS:min(ITE,IDE-1),JTS:min(JTE,JDE-1)))		   !;  grid%STDVTOPO = X'FF911299'
            allocate(grid%SEAICE(ITS:min(ITE,IDE-1),JTS:min(JTE,JDE-1)))		   !;  grid%SEAICE = X'FF911299'
            allocate(grid%STC_WPS(ITS:min(ITE,IDE-1),JTS:min(JTE,JDE-1),numsoil))	   !;  grid%STC_WPS = X'FF911299'
            allocate(grid%SMC_WPS(ITS:min(ITE,IDE-1),JTS:min(JTE,JDE-1),numsoil))	   !;  grid%SMC_WPS = X'FF911299'
            allocate(grid%PSFC(ITS:min(ITE,IDE-1),JTS:min(JTE,JDE-1)))			   !;  grid%PSFC = X'FF911299'
            allocate(grid%SLOPECAT(ITS:min(ITE,IDE-1),JTS:min(JTE,JDE-1)))		   !;  grid%SLOPECAT = X'FF911299'
            allocate(grid%SNOALB(ITS:min(ITE,IDE-1),JTS:min(JTE,JDE-1)))		   !;  grid%SNOALB = X'FF911299'
            allocate(grid%GREENFRAC(ITS:min(ITE,IDE-1),JTS:min(JTE,JDE-1),12))		   !;  grid%GREENFRAC = X'FF911299'
       if (ncep_processing) then
            allocate(grid%ALBEDO12M(ITS:min(ITE,IDE-1),JTS:min(JTE,JDE-1),4))		   !;  grid%ALBEDO12M = X'FF911299'
       else
            allocate(grid%ALBEDO12M(ITS:min(ITE,IDE-1),JTS:min(JTE,JDE-1),12))		   !;  grid%ALBEDO12M = X'FF911299'
       endif
            allocate(grid%SOILCTOP(ITS:min(ITE,IDE-1),JTS:min(JTE,JDE-1),16))		   !;  grid%SOILCTOP = X'FF911299'
            allocate(grid%SOILTEMP(ITS:min(ITE,IDE-1),JTS:min(JTE,JDE-1)))		   !;  grid%SOILTEMP = X'FF911299'
            allocate(grid%HGT_M(ITS:min(ITE,IDE-1),JTS:min(JTE,JDE-1))) 		   !;  grid%HGT_M = X'FF911299'
            allocate(grid%LU_INDEX(ITS:min(ITE,IDE-1),JTS:min(JTE,JDE-1)))		   !;  grid%LU_INDEX = X'FF911299'
	if (use_igbp) then
            allocate(grid%LANDUSEF(ITS:min(ITE,IDE-1),JTS:min(JTE,JDE-1),20)) 		!;  grid%LANDUSEF = X'FF911299'
        else
            allocate(grid%LANDUSEF(ITS:min(ITE,IDE-1),JTS:min(JTE,JDE-1),24))		   !;  grid%LANDUSEF = X'FF911299'
        endif
            allocate(grid%LANDMASK(ITS:min(ITE,IDE-1),JTS:min(JTE,JDE-1)))		   !;  grid%LANDMASK = X'FF911299'
       if (grid%gtype .ne. 'A') then
            allocate(grid%XLONG_V(ITS:min(ITE,IDE-1),JTS:min(JTE,JDE-1)))		   !;  grid%XLONG_V = X'FF911299'
            allocate(grid%XLAT_V(ITS:min(ITE,IDE-1),JTS:min(JTE,JDE-1)))		   !;  grid%XLAT_V = X'FF911299'
       endif
            allocate(grid%XLONG_M(ITS:min(ITE,IDE-1),JTS:min(JTE,JDE-1)))		   !;  grid%XLONG_M = X'FF911299'
            allocate(grid%XLAT_M(ITS:min(ITE,IDE-1),JTS:min(JTE,JDE-1)))		   !;  grid%XLAT_M = X'FF911299'
       if (ncep_processing .and. do_gwd) then
            allocate(grid%GWD_OROG(ITS:min(ITE,IDE-1),JTS:min(JTE,JDE-1),14))	           !;  grid%GWD_OROG = X'FF911299'
       endif

        ENDIF
       ENDIF

	Ilook=336
	Jlook=29

       grid%STDVTOPO=0.

       allocate(dumdata(IDS:IDE-1,JDS:JDE-1,num_metgrid_levels))

    specblock:   IF (spectral) THEN

! =============================================================

       varName='PRES'

       if (io_form_input == NETCDF_FORMAT) then
        call var_read(varName,dumdata(:,:,1:num_metgrid_levels-2),iret=ierr)
       else
        do K=1,num_metgrid_levels-2
          write(lchar,47) K
  47      format(I2.2)
          recname=trim(varName)//'_'//lchar
          call var_read(trim(recname),dumdata(:,:,K:K),iret=ierr)
        end do
       endif
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif

       rmax=-9999.
       DO K=1,num_metgrid_levels-2
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%PRES(I,J,K)=dumdata(I,J,K)
	if (grid%PRES(I,J,K) .gt. rmax) rmax=grid%PRES(I,J,K)
	if (I .eq. Ilook .and. J .eq. Jlook) then
	write(0,*) 'K, grid%PRES(Ilook,Jlook) ingested: ', I,J,K,grid%PRES(I,J,K)
	endif
         ENDDO
        ENDDO
       ENDDO

	if (print_diag) write(0,*) 'rmax of greatest PRES ingest: ', rmax

! =============================================================

        varName='PINT'
       if (io_form_input == NETCDF_FORMAT) then
       call var_read(varName,dumdata(:,:,1:num_metgrid_levels-1),iret=ierr)
       else
        do K=1,num_metgrid_levels-1
          write(lchar,47) K
          recname=trim(varName)//'_'//lchar
          call var_read(trim(recname),dumdata(:,:,K:K),iret=ierr)
        end do
       endif

       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif

       rmax=-9999.
       DO K=1,num_metgrid_levels-1
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%PINT(I,J,K)=dumdata(I,J,K)
	if (grid%PINT(I,J,K) .gt. rmax) rmax=grid%PINT(I,J,K)
	if (I .eq. Ilook .and. J .eq. Jlook) then
	write(0,*) 'K, grid%PINT(Ilook,Jlook) ingested: ', I,J,K,grid%PINT(I,J,K)
	endif
         ENDDO
        ENDDO
       ENDDO

	if (print_diag) write(0,*) 'rmax of greatest PINT ingest: ', rmax

! =============================================================
        varName='SPECHUMD'
       if (io_form_input == NETCDF_FORMAT) then
        call var_read(varName,dumdata(:,:,1:num_metgrid_levels-2),iret=ierr)
       else
        do K=1,num_metgrid_levels-2
          write(lchar,47) K
          recname=trim(varName)//'_'//lchar
          call var_read(trim(recname),dumdata(:,:,K:K),iret=ierr)
        end do
       endif
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif

       DO K=1,num_metgrid_levels-2
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%SPECHUMD(I,J,K)=dumdata(I,J,K)
         ENDDO
        ENDDO
       ENDDO

! =============================================================

       varName='GHT'
       if (io_form_input == NETCDF_FORMAT) then
        call var_read(varName,dumdata(:,:,1:num_metgrid_levels-1),iret=ierr)
       else
        do K=1,num_metgrid_levels-1
          write(lchar,47) K
          recname=trim(varName)//'_'//lchar
          call var_read(trim(recname),dumdata(:,:,K:K),iret=ierr)
        end do
       endif
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif

       DO K=1,num_metgrid_levels-1
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%GHT(I,J,K)=dumdata(I,J,K)
         ENDDO
        ENDDO
       ENDDO

! =============================================================

       varName='RH'
       if (print_diag) write(0,*)' reading ', varName
       if (io_form_input == NETCDF_FORMAT) then
        call var_read(varName,dumdata(:,:,1:num_metgrid_levels-2),iret=ierr)
       else
        do K=1,num_metgrid_levels-2
          write(lchar,47) K
          recname=trim(varName)//'_'//lchar
          call var_read(trim(recname),dumdata(:,:,K:K),iret=ierr)
        end do
       endif

       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif
       DO K=1,num_metgrid_levels-2
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%RH(I,J,K)=dumdata(I,J,K)
         ENDDO
        ENDDO
       ENDDO

! =============================================================

       varName='VV'
       if (print_diag) write(0,*)' reading ', varName
       if (io_form_input == NETCDF_FORMAT) then
        call var_read(varName,dumdata(:,:,1:num_metgrid_levels-2),iret=ierr)
       else
        do K=1,num_metgrid_levels-2
          write(lchar,47) K
          recname=trim(varName)//'_'//lchar
          call var_read(trim(recname),dumdata(:,:,K:K),iret=ierr)
        end do
       endif
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif
       DO K=1,num_metgrid_levels-2
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%VV(I,J,K)=dumdata(I,J,K)
         ENDDO
        ENDDO
       ENDDO

! =============================================================

       varName='UU'
       if (print_diag) write(0,*)' reading ', varName
       if (io_form_input == NETCDF_FORMAT) then
        call var_read(varName,dumdata(:,:,1:num_metgrid_levels-2),iret=ierr)
       else
        do K=1,num_metgrid_levels-2
          write(lchar,47) K
          recname=trim(varName)//'_'//lchar
          call var_read(trim(recname),dumdata(:,:,K:K),iret=ierr)
        end do
       endif
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif
       DO K=1,num_metgrid_levels-2
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%UU(I,J,K)=dumdata(I,J,K)
         ENDDO
        ENDDO
       ENDDO

! =============================================================

       varName='TT'
       if (print_diag) write(0,*)' reading ', varName
       if (io_form_input == NETCDF_FORMAT) then
        call var_read(varName,dumdata(:,:,1:num_metgrid_levels-2),iret=ierr)
       else
        do K=1,num_metgrid_levels-2
          write(lchar,47) K
          recname=trim(varName)//'_'//lchar
          call var_read(trim(recname),dumdata(:,:,K:K),iret=ierr)
        end do
       endif
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif
       DO K=1,num_metgrid_levels-2
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%TT(I,J,K)=dumdata(I,J,K)
         ENDDO
        ENDDO
       ENDDO

! =============================================================
        else ! non spectral 3D reads
! =============================================================

       varName='PRES'
       if (io_form_input == NETCDF_FORMAT) then
        call var_read(varName,dumdata,iret=ierr)
       else
        do K=1,num_metgrid_levels
          write(lchar,47) K
          recname=trim(varName)//'_'//lchar
          call var_read(trim(recname),dumdata(:,:,K:K),iret=ierr)
        end do
       endif
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif

       rmax=-9999.
       DO K=1,num_metgrid_levels
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%PRES(I,J,K)=dumdata(I,J,K)
	if (grid%PRES(I,J,K) .gt. rmax) rmax=grid%PRES(I,J,K)
	if (I .eq. Ilook .and. J .eq. Jlook) then
	write(0,*) 'K, grid%PRES(Ilook,Jlook) ingested: ', I,J,K,grid%PRES(I,J,K)
	endif
         ENDDO
        ENDDO
       ENDDO

       if(print_diag) write(0,*) 'rmax of greatest PRES ingest: ', rmax

! =============================================================

       varName='GHT'
       if (io_form_input == NETCDF_FORMAT) then
        call var_read(varName,dumdata,iret=ierr)
       else
        do K=1,num_metgrid_levels
          write(lchar,47) K
          recname=trim(varName)//'_'//lchar
          call var_read(trim(recname),dumdata(:,:,K:K),iret=ierr)
        end do
       endif
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif

       DO K=1,num_metgrid_levels
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%GHT(I,J,K)=dumdata(I,J,K)
         ENDDO
        ENDDO
       ENDDO

! =============================================================

       varName='RH'
       if (print_diag) write(0,*)' reading ', varName
       if (io_form_input == NETCDF_FORMAT) then
        call var_read(varName,dumdata,iret=ierr)
       else
        do K=1,num_metgrid_levels
          write(lchar,47) K
          recname=trim(varName)//'_'//lchar
          call var_read(trim(recname),dumdata(:,:,K:K),iret=ierr)
        end do
       endif
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif
       DO K=1,num_metgrid_levels
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%RH(I,J,K)=dumdata(I,J,K)
         ENDDO
        ENDDO
       ENDDO

! =============================================================

       varName='VV'
       if (print_diag) write(0,*)' reading ', varName
       if (io_form_input == NETCDF_FORMAT) then
        call var_read(varName,dumdata,iret=ierr)
       else
        do K=1,num_metgrid_levels
          write(lchar,47) K
          recname=trim(varName)//'_'//lchar
          call var_read(trim(recname),dumdata(:,:,K:K),iret=ierr)
        end do
       endif
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif
       DO K=1,num_metgrid_levels
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%VV(I,J,K)=dumdata(I,J,K)
         ENDDO
        ENDDO
       ENDDO

! =============================================================

       varName='UU'
       if (print_diag) write(0,*)' reading ', varName
       if (io_form_input == NETCDF_FORMAT) then
        call var_read(varName,dumdata,iret=ierr)
       else
        do K=1,num_metgrid_levels
          write(lchar,47) K
          recname=trim(varName)//'_'//lchar
          call var_read(trim(recname),dumdata(:,:,K:K),iret=ierr)
        end do
       endif
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif
       DO K=1,num_metgrid_levels
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%UU(I,J,K)=dumdata(I,J,K)
         ENDDO
        ENDDO
       ENDDO

! =============================================================

       varName='TT'
       if (print_diag) write(0,*)' reading ', varName
       if (io_form_input == NETCDF_FORMAT) then
        call var_read(varName,dumdata,iret=ierr)
       else
        do K=1,num_metgrid_levels
          write(lchar,47) K
          recname=trim(varName)//'_'//lchar
          call var_read(trim(recname),dumdata(:,:,K:K),iret=ierr)
        end do
       endif
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif
       DO K=1,num_metgrid_levels
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%TT(I,J,K)=dumdata(I,J,K)
         ENDDO
        ENDDO
       ENDDO

! =============================================================
        endif specblock
! =============================================================


        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
	if (I .eq. Ilook .and. J .eq. Jlook) then
       DO K=1,num_metgrid_levels-1
	write(0,*) 'K, grid%GHT(Ilook,Jlook) ingested: ', I,J,K,grid%GHT(I,J,K)
       ENDDO
	endif
        ENDDO
       ENDDO

 
 633	format(18(f6.0,1x))
 634	format(18(f7.0,1x))



       varName='CANWAT'
       if (print_diag) write(0,*) 'setting CANWAT to ZERO'
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%CANWAT(I,J)=0.
         ENDDO
        ENDDO
!       ENDIF

       varName='SNOW'
       if (print_diag) write(0,*)' reading ', varName
       call var_read(varName,dumdata(:,:,1:1),iret=ierr)
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
          grid%snow(I,J)=dumdata(I,J,1)
        ENDDO
       ENDDO



       varName='SKINTEMP'

       if (no_seaice) then

       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
          grid%SKINTEMP(I,J)=275.
        ENDDO
       ENDDO

       else

       if (print_diag) write(0,*)' reading ', varName
       call var_read(varName,dumdata(:,:,1:1),iret=ierr)
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
          grid%SKINTEMP(I,J)=dumdata(I,J,1)
        ENDDO
       ENDDO

       endif

       varName='SOILHGT'
       if (print_diag) write(0,*)' reading ', varName
       call var_read(varName,dumdata(:,:,1:1),iret=ierr)
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
          grid%SOILHGT(I,J)=dumdata(I,J,1)
        ENDDO
       ENDDO

       varName='HGTSTDV'
       if (.NOT. do_gwd) then
        write(0,*) 'not do_gwd, setting STDH to ZERO'
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%STDVTOPO(I,J)=0.
         ENDDO
        ENDDO
       else

       if (print_diag) write(0,*)' reading ', varName
       call var_read(varName,dumdata(:,:,1:1),iret=ierr)
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
          grid%STDVTOPO(I,J)=dumdata(I,J,1)
        ENDDO
       ENDDO

       endif 

       varName='SEAICE'

       if (no_seaice) then

       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
          grid%SEAICE(I,J)=0.
        ENDDO
       ENDDO


       else

       if (print_diag) write(0,*)' reading ', varName
       call var_read(varName,dumdata(:,:,1:1),iret=ierr)
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
          grid%SEAICE(I,J)=dumdata(I,J,1)
        ENDDO
       ENDDO

       endif

       varName='STC_WPS'
       if (no_seaice) then
         dumdata(:,:,1:numsoil)=280.
       else
         if (print_diag) write(0,*)' reading ', varName
         if (io_form_input == NETCDF_FORMAT) then
          call var_read(varName,dumdata(:,:,1:numsoil),iret=ierr)
         else
          do K=1,numsoil
           write(lchar,47) K
           recname=trim(varName)//'_'//lchar
          call var_read(trim(recname),dumdata(:,:,K:K),iret=ierr)
          end do
         endif
         if(ierr/=0) then
           write(0,*)'error while reading ',varName
           stop
         endif
       endif


!!! flip in vertical???
       DO K=1,numsoil
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%STC_WPS(I,J,K)=dumdata(I,J,numsoil-K+1)
         ENDDO
        ENDDO
!	if (print_diag) write(0,*) 'K, minval, maxval STC_WPS: ', K, &
!                      minval(grid%STC_WPS(:,:,K)), maxval(grid%STC_WPS(:,:,K))
       ENDDO

       varName='SMC_WPS'

       if (no_seaice) then
         dumdata(:,:,1:numsoil)=0.2
       else
       if (print_diag) write(0,*)' reading ', varName
         if (io_form_input == NETCDF_FORMAT) then
          call var_read(varName,dumdata(:,:,1:numsoil),iret=ierr)
         else
          do K=1,numsoil
           write(lchar,47) K
           recname=trim(varName)//'_'//lchar
           call var_read(trim(recname),dumdata(:,:,K:K),iret=ierr)
          end do
         endif
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif

       endif
!!! flip in vertical???
       DO K=1,numsoil
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%SMC_WPS(I,J,K)=dumdata(I,J,numsoil-K+1)
         ENDDO
        ENDDO
       ENDDO

       varName='PSFC'
       if (print_diag) write(0,*)' reading ', varName
       call var_read(varName,dumdata(:,:,1:1),iret=ierr)
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
          grid%psfc(I,J)=dumdata(I,J,1)
        ENDDO
       ENDDO

!
!  reinstate cloud fields with smarts to handle things if the fields arent available?

	if (do_clouds .and. .not. spectral) then

	if (grib_src .eq. 'NAM') then

	if (print_diag) write(0,*) 'NAM read of RWMR'
       varName='RWMR'
       if (print_diag) write(0,*)' reading ', varName
       if (io_form_input == NETCDF_FORMAT) then
        call var_read(varName,dumdata,iret=ierr)
       else
        do K=1,num_metgrid_levels
          write(lchar,47) K
          recname=trim(varName)//'_'//lchar
          call var_read(trim(recname),dumdata(:,:,K:K),iret=ierr)
        end do
       endif
       if(ierr/=0) then
         write(0,*)'error while reading... ',varName, '...set to zero'
	 grid%RWMR=0.
       else
         DO K=1,num_metgrid_levels
          DO J=JTS,min(JTE,JDE-1)
            DO I=ITS,min(ITE,IDE-1)
              grid%RWMR(I,J,K)=dumdata(I,J,K)
            ENDDO
          ENDDO
         ENDDO
       endif

       varName='SNMR'
       if (print_diag) write(0,*)' reading ', varName
       if (io_form_input == NETCDF_FORMAT) then
        call var_read(varName,dumdata,iret=ierr)
       else
        do K=1,num_metgrid_levels
          write(lchar,47) K
          recname=trim(varName)//'_'//lchar
          call var_read(trim(recname),dumdata(:,:,K:K),iret=ierr)
        end do
       endif
       if(ierr/=0) then
         write(0,*)'error while reading... ',varName, '...set to zero'
	 grid%SNMR=0.
       else
         DO K=1,num_metgrid_levels
          DO J=JTS,min(JTE,JDE-1)
            DO I=ITS,min(ITE,IDE-1)
              grid%SNMR(I,J,K)=dumdata(I,J,K)
            ENDDO
          ENDDO
         ENDDO
       endif

       varName='CICE'
       if (print_diag) write(0,*)' reading ', varName
       if (io_form_input == NETCDF_FORMAT) then
        call var_read(varName,dumdata,iret=ierr)
       else
        do K=1,num_metgrid_levels
          write(lchar,47) K
          recname=trim(varName)//'_'//lchar
          call var_read(trim(recname),dumdata(:,:,K:K),iret=ierr)
        end do
       endif
       if(ierr/=0) then
         write(0,*)'error while reading... ',varName, '...set to zero'
	 grid%CICE=0.
       else
         DO K=1,num_metgrid_levels
          DO J=JTS,min(JTE,JDE-1)
            DO I=ITS,min(ITE,IDE-1)
              grid%CICE(I,J,K)=dumdata(I,J,K)
            ENDDO
          ENDDO
         ENDDO
       endif

	else

	write(0,*) 'NOT NAM, so zeroing RWMR, SNMR, CICE'

        grid%RWMR=0.
        grid%SNMR=0.
        grid%CICE=0.
	
	endif ! check on NAM

       varName='CLWMR'
       if (print_diag) write(0,*)' reading ', varName
       if (io_form_input == NETCDF_FORMAT) then
        call var_read(varName,dumdata,iret=ierr)
       else
        do K=1,num_metgrid_levels
          write(lchar,47) K
          recname=trim(varName)//'_'//lchar
          call var_read(trim(recname),dumdata(:,:,K:K),iret=ierr)
        end do
       endif
       if(ierr/=0) then
         write(6,*)'error while reading... ',varName, '...set to zero'
	 grid%CLWMR=0.
       else
         DO K=1,num_metgrid_levels
          DO J=JTS,min(JTE,JDE-1)
            DO I=ITS,min(ITE,IDE-1)
              grid%CLWMR(I,J,K)=dumdata(I,J,K)
            ENDDO
          ENDDO
         ENDDO
	write(0,*) 'maxval of CLWMR ingested: ', maxval(grid%CLWMR)
       endif

	if (grib_src .eq. 'NAM') then

       varName='FRIMEF'
       if (print_diag) write(0,*)' reading ', varName
       if (io_form_input == NETCDF_FORMAT) then
        call var_read(varName,dumdata,iret=ierr)
       else
        do K=1,num_metgrid_levels
          write(lchar,47) K
          recname=trim(varName)//'_'//lchar
          call var_read(trim(recname),dumdata(:,:,K:K),iret=ierr)
        end do
       endif
       if(ierr/=0) then
         write(0,*)'error while reading... ',varName, '...set to zero'
	 grid%FRIMEF=0.
       else
         DO K=1,num_metgrid_levels
          DO J=JTS,min(JTE,JDE-1)
            DO I=ITS,min(ITE,IDE-1)
              grid%FRIMEF(I,J,K)=dumdata(I,J,K)
            ENDDO
          ENDDO
         ENDDO
       endif

       else
	write(0,*) 'not NAM, zero out FRIMEF'
	grid%FRIMEF=0.
       endif ! check on NAM

	else

	write(0,*) 'skipped cloud field reads'

	endif
! done clouds

       varName='SLOPECAT'
       if (print_diag) write(0,*)' reading ', varName
       call var_read(varName,dumdata(:,:,1:1),iret=ierr)
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
          grid%SLOPECAT(I,J)=dumdata(I,J,1)
        ENDDO
       ENDDO

       varName='SNOALB'
       if (print_diag) write(0,*)' reading ', varName
       call var_read(varName,dumdata(:,:,1:1),iret=ierr)
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
          grid%SNOALB(I,J)=dumdata(I,J,1)
        ENDDO
       ENDDO

       num_veg_cat      = SIZE ( grid%LANDUSEF , DIM=3 )

	if (print_diag) write(0,*) 'num_veg_cat from LANDUSEF: ', num_veg_cat

       num_soil_top_cat = SIZE ( grid%SOILCTOP , DIM=3 )

       varName='GREENFRAC'
       if (print_diag) write(0,*)' reading ', varName
       if (io_form_input == NETCDF_FORMAT) then
        call var_read(varName,dumdata(:,:,1:12),iret=ierr)
       else
        do K=1,12
          write(lchar,47) K
          recname=trim(varName)//'_'//lchar
          call var_read(trim(recname),dumdata(:,:,K:K),iret=ierr)
        end do
       endif
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif
       DO K=1,12
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%GREENFRAC(I,J,K)=dumdata(I,J,K)
         ENDDO
        ENDDO
       ENDDO

!!! does geogrid somehow write out 12 records???

       if (ncep_processing) then

	if (print_diag) write(0,*) 'only 4 quarter albedo records'
       varName='ALBEDO12M'
       if (print_diag) write(0,*)' reading ', varName
       if (print_diag) write(0,*) 'size dumdata arrray: ', size(dumdata,dim=1),size(dumdata,dim=2),size(dumdata,dim=3)
       if (io_form_input == NETCDF_FORMAT) then
        call var_read(varName,dumdata(:,:,1:12),iret=ierr)
       else
        do K=1,12
          write(lchar,47) K
          recname=trim(varName)//'_'//lchar
          call var_read(trim(recname),dumdata(:,:,K:K),iret=ierr)
        end do
       endif
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif
       DO K=1,4
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%ALBEDO12M(I,J,K)=dumdata(I,J,K)
         ENDDO
        ENDDO
       ENDDO

       else

       varName='ALBEDO12M'
	write(0,*) '12 monthly albedo records'
       if (print_diag) write(0,*)' reading ', varName
       if (io_form_input == NETCDF_FORMAT) then
        call var_read(varName,dumdata(:,:,1:12),iret=ierr)
       else
        do K=1,12
          write(lchar,47) K
          recname=trim(varName)//'_'//lchar
          call var_read(trim(recname),dumdata(:,:,K:K),iret=ierr)
        end do
       endif
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif
       DO K=1,12
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%ALBEDO12M(I,J,K)=dumdata(I,J,K)
         ENDDO
        ENDDO
       ENDDO

	endif

	if (.NOT. ncep_processing) then

       varName='SOILCTOP'
       if (print_diag) write(0,*)' reading ', varName
       if (io_form_input == NETCDF_FORMAT) then
        call var_read(varName,dumdata(:,:,1:num_soil_top_cat),iret=ierr)
       else
        do K=1,num_soil_top_cat
          write(lchar,47) K
          recname=trim(varName)//'_'//lchar
          call var_read(trim(recname),dumdata(:,:,K:K),iret=ierr)
        end do
       endif
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif
       DO K=1,num_soil_top_cat
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%SOILCTOP(I,J,K)=dumdata(I,J,K)
         ENDDO
        ENDDO
       ENDDO

        else

       varName='SOILCTOP'
       if (print_diag) write(0,*)' reading ', varName
       if (io_form_input == NETCDF_FORMAT) then
        call var_read(varName,dumdata(:,:,1:16),iret=ierr)
       else
        do K=1,16
          write(lchar,47) K
          recname=trim(varName)//'_'//lchar
          call var_read(trim(recname),dumdata(:,:,K:K),iret=ierr)
        end do
       endif
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif
	if (print_diag) write(0,*) 'placing dumdata(:,:,1) into soilcat'
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%SOILCAT(I,J)=dumdata(I,J,1)
         ENDDO
        ENDDO

	endif

       varName='SOILTEMP'
       if (print_diag) write(0,*)' reading ', varName
       call var_read(varName,dumdata(:,:,1:1),iret=ierr)
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
          grid%SOILTEMP(I,J)=dumdata(I,J,1)
        ENDDO
       ENDDO

	if (print_diag) write(0,*) 'ncep_processing is: ', ncep_processing
	
       varName='HGT_M'
       if (print_diag) write(0,*)' reading ', varName
       call var_read(varName,dumdata(:,:,1:1),iret=ierr)
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
          grid%HGT_M(I,J)=dumdata(I,J,1)
        ENDDO
       ENDDO

       varName='LU_INDEX'
       if (print_diag) write(0,*)' reading ', varName
       call var_read(varName,dumdata(:,:,1:1),iret=ierr)
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
          grid%LU_INDEX(I,J)=dumdata(I,J,1)
        ENDDO
       ENDDO
	if (ncep_processing) then
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
          grid%VEGCAT(I,J)=dumdata(I,J,1)
        ENDDO
       ENDDO
	endif

       varName='LANDUSEF'
       if (print_diag) write(0,*)' reading ', varName
       if (io_form_input == NETCDF_FORMAT) then
        call var_read(varName,dumdata(:,:,1:num_veg_cat),iret=ierr)
       else
        do K=1,num_veg_cat
          write(lchar,47) K
          recname=trim(varName)//'_'//lchar
          call var_read(trim(recname),dumdata(:,:,K:K),iret=ierr)
        end do
       endif
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif
       DO K=1,num_veg_cat
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
           grid%LANDUSEF(I,J,K)=dumdata(I,J,K)
         ENDDO
        ENDDO
       ENDDO

!!! is there a dominant land-use category?

	if (.NOT. ncep_processing) then
        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
          rmax=0.
          KCAT=0
          DO K=1,num_veg_cat
             IF (grid%LANDUSEF(I,J,K) .gt. rmax) KCAT=K
          ENDDO
          grid%VEGCAT(I,J)=KCAT
         ENDDO
        ENDDO

        DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)
          rmax=0.
          KCAT=0
          DO K=1,num_soil_top_cat
             IF (grid%SOILCTOP(I,J,K) .gt. rmax) KCAT=K
          ENDDO
          grid%SOILCAT(I,J)=KCAT
         ENDDO
        ENDDO

	endif

       varName='LANDMASK'
       if (print_diag) write(0,*)' reading ', varName
       call var_read(varName,dumdata(:,:,1:1),iret=ierr)
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
          grid%LANDMASK(I,J)=dumdata(I,J,1)
        ENDDO
       ENDDO

       if (grid%gtype .ne. 'A') then

       varName='XLONG_V'
       if (print_diag) write(0,*)' reading ', varName
       call var_read(varName,dumdata(:,:,1:1),iret=ierr)
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
          grid%XLONG_V(I,J)=dumdata(I,J,1)
        ENDDO
       ENDDO

       varName='XLAT_V'
       if (print_diag) write(0,*)' reading ', varName
       call var_read(varName,dumdata(:,:,1:1),iret=ierr)
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
          grid%XLAT_V(I,J)=dumdata(I,J,1)
        ENDDO
       ENDDO
      
       else

       write(0,*) 'skipping XLONG_V and XLAT_V because on A grid'
	
       endif

       varName='XLONG_M'
       if (print_diag) write(0,*)' reading ', varName
       call var_read(varName,dumdata(:,:,1:1),iret=ierr)
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
          grid%XLONG_M(I,J)=dumdata(I,J,1)
        ENDDO
       ENDDO

       varName='XLAT_M'
       if (print_diag) write(0,*)' reading ', varName
       call var_read(varName,dumdata(:,:,1:1),iret=ierr)
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
          grid%XLAT_M(I,J)=dumdata(I,J,1)
        ENDDO
       ENDDO

! ---------------------------------
      IF (ncep_processing .and. do_gwd) then
! ---------------------------------

       varName='HGTSTDV'
       if (print_diag) write(0,*)' reading ', varName
       call var_read(varName,dumdata(:,:,1:1),iret=ierr)
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
          grid%GWD_OROG(I,J,1)=dumdata(I,J,1)
        ENDDO
       ENDDO

       varName='HGTCNVX'
       if (print_diag) write(0,*)' reading ', varName
       call var_read(varName,dumdata(:,:,1:1),iret=ierr)
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
          grid%GWD_OROG(I,J,2)=dumdata(I,J,1)
        ENDDO
       ENDDO

       varName='HGTOA1'
       if (print_diag) write(0,*)' reading ', varName
       call var_read(varName,dumdata(:,:,1:1),iret=ierr)
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
          grid%GWD_OROG(I,J,3)=dumdata(I,J,1)
        ENDDO
       ENDDO

       varName='HGTOA2'
       if (print_diag) write(0,*)' reading ', varName
       call var_read(varName,dumdata(:,:,1:1),iret=ierr)
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
          grid%GWD_OROG(I,J,4)=dumdata(I,J,1)
        ENDDO
       ENDDO

       varName='HGTOA3'
       if (print_diag) write(0,*)' reading ', varName
       call var_read(varName,dumdata(:,:,1:1),iret=ierr)
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
          grid%GWD_OROG(I,J,5)=dumdata(I,J,1)
        ENDDO
       ENDDO

       varName='HGTOA4'
       if (print_diag) write(0,*)' reading ', varName
       call var_read(varName,dumdata(:,:,1:1),iret=ierr)
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
          grid%GWD_OROG(I,J,6)=dumdata(I,J,1)
        ENDDO
       ENDDO

       varName='HGTOL1'
       if (print_diag) write(0,*)' reading ', varName
       call var_read(varName,dumdata(:,:,1:1),iret=ierr)
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
          grid%GWD_OROG(I,J,7)=dumdata(I,J,1)
        ENDDO
       ENDDO

       varName='HGTOL2'
       if (print_diag) write(0,*)' reading ', varName
       call var_read(varName,dumdata(:,:,1:1),iret=ierr)
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
          grid%GWD_OROG(I,J,8)=dumdata(I,J,1)
        ENDDO
       ENDDO

       varName='HGTOL3'
       if (print_diag) write(0,*)' reading ', varName
       call var_read(varName,dumdata(:,:,1:1),iret=ierr)
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
          grid%GWD_OROG(I,J,9)=dumdata(I,J,1)
        ENDDO
       ENDDO

       varName='HGTOL4'
       if (print_diag) write(0,*)' reading ', varName
       call var_read(varName,dumdata(:,:,1:1),iret=ierr)
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
          grid%GWD_OROG(I,J,10)=dumdata(I,J,1)
        ENDDO
       ENDDO

       varName='HGTTHTA'
       if (print_diag) write(0,*)' reading ', varName
       call var_read(varName,dumdata(:,:,1:1),iret=ierr)
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
          grid%GWD_OROG(I,J,11)=dumdata(I,J,1)
        ENDDO
       ENDDO

       varName='HGTGMMA'
       if (print_diag) write(0,*)' reading ', varName
       call var_read(varName,dumdata(:,:,1:1),iret=ierr)
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
          grid%GWD_OROG(I,J,12)=dumdata(I,J,1)
        ENDDO
       ENDDO

       varName='HGTSGMA'
       if (print_diag) write(0,*)' reading ', varName
       call var_read(varName,dumdata(:,:,1:1),iret=ierr)
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
          grid%GWD_OROG(I,J,13)=dumdata(I,J,1)
        ENDDO
       ENDDO

       varName='HGTMAX'
       if (print_diag) write(0,*)' reading ', varName
       call var_read(varName,dumdata(:,:,1:1),iret=ierr)
       if(ierr/=0) then
         write(0,*)'error while reading ',varName
         stop
       endif
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
          grid%GWD_OROG(I,J,14)=dumdata(I,J,1)
        ENDDO
       ENDDO

! ---------------------------------
      ENDIF
! ---------------------------------


#ifdef IO_NETCDF
      if (io_form_input == NETCDF_FORMAT) then
         call ext_ncd_ioclose(handle, istatus)
         call ext_ncd_ioexit(istatus)
      end if
#endif
#ifdef IO_DIO
      if (io_form_input == DIO_FORMAT) then
         call dio_close(dfile,iret=istatus)
         call dio_finalize()
      end if
#endif

      DEALLOCATE(dumdata)

      return

     END SUBROUTINE READ_NPS

   subroutine var_read(varName,var,iret)

      use module_debug
      implicit none

      ! Arguments
      character(len=*), intent(in) :: varName
      real,dimension(:,:,:), intent(out) :: var
      integer, intent(out) :: iret

      ! Local variables
      integer :: istatus
      integer :: ndim, wrftype
      integer, dimension(3) :: domain_start, domain_end

      character(len=32), dimension(3) :: dimnames
      character(len=3) :: memorder, stagger

      iret=0
#ifdef IO_NETCDF
      if (io_form_input == NETCDF_FORMAT) then
         istatus = 0
         domain_start = 1
         domain_end = 1
         call ext_ncd_get_var_info(handle, varName, ndim, memorder, stagger, domain_start, domain_end, wrftype, istatus)
         call mprintf((istatus /= 0),ERROR,'In read_next_field(), problems with ext_pkg_get_var_info()')
 
         if (lbound(var,dim=1) /= domain_start(1) .or. &
             lbound(var,dim=2) /= domain_start(2) .or. &
             lbound(var,dim=3) /= domain_start(3) ) then
            write(0,*)lbound(var)
            write(0,*)domain_start
            call mprintf(.true.,ERROR,'In var_read, lbound(var) /= domain_start for varName='//trim(varName))
         end if
         if (ubound(var,dim=1) /= domain_end(1) .or. &
             ubound(var,dim=2) /= domain_end(2) .or. &
             ubound(var,dim=3) /= domain_end(3) ) then
            write(0,*)ubound(var)
            write(0,*)domain_end
            !write(0,*) varName, ndim, memorder, stagger, domain_start, domain_end, wrftype, istatus
            call mprintf(.true.,ERROR,'In var_read, ubound(var) /= domain_end   for varName='//trim(varName))
         end if

         istatus = 0
         call ext_ncd_read_field(handle, DateStr, varName, var, wrftype, &
                          1, 1, 0, memorder, stagger, &
                          dimnames, domain_start, domain_end, domain_start, domain_end, &
                          domain_start, domain_end, istatus)
         call mprintf((istatus /= 0),ERROR,'In read_next_field(), got error code %i.', i1=istatus)


      end if
#endif
#ifdef IO_DIO
      if (io_form_input == DIO_FORMAT) then
         call dio_read(dfile,varName,var,iret=iret)
      end if
#endif

   end subroutine var_read

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: ext_get_dom_ti_integer
   !
   ! Purpose: Read a domain time-independent integer attribute from input.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine ext_get_dom_ti_integer_scalar(var_name, var_value)

      use module_debug
      implicit none

      ! Arguments
      integer, intent(out) :: var_value
      character (len=*), intent(in) :: var_name

      ! Local variables
      integer :: istatus, outcount

#ifdef IO_NETCDF
      if (io_form_input == NETCDF_FORMAT) then
         call ext_ncd_get_dom_ti_integer(handle, trim(var_name), &
                                         var_value, &
                                         1, outcount, istatus)
      end if
#endif
#ifdef IO_DIO
      if (io_form_input == DIO_FORMAT) call dio_read(dfile,trim(var_name), var_value, iret=istatus)
#endif
      call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')

   end subroutine ext_get_dom_ti_integer_scalar


END MODULE ingest_metgrid
