 program coldstart_wrf
!$$$  main program documentation block
!                .      .    .                                       .
! main program: coldstart_wrf
!   prgmmr: gayno            ORG: NP2                DATE: 2005-08-03
!
! abstract: interpolates land states from an edas grid, an nmm
!           grid, or the gfs grid ---> to an nmm grid.
!
! program history log:
!   2005-08-03  gayno     initial version
!   2006-03-13  gayno     added option to get input land states
!                         from a wrf nmm binary file
!   2009-07-13  gayno     added option to process b-grids, 
!                         nemsio formatted files, and to
!                         merge gfs and nam land states
!                         for 'large' regional grids.
!   2012-11-07  gayno     port to WCOSS machine.  remove
!                         read/write of wrf binary files.
!
! usage:
!        
!   input files:
!     configuration namelist - fort.81
!     edas land surface binary restart file 
!     nmm nemsio file
!     gfs surface restart file
!     output grid land/sea mask (optional)
!     output grid latitudes (optional)
!     output grid longitudes (optional)    
!     output grid orography (optional)
!     output grid substrate temp (optional)
!     output grid snow-free albedo file (optional)
!     output grid greenness (optional)
!     output grid max snow albedo (optional)
!     output grid slope type (optional)
!     output grid soil type (optional)
!     output grid veg type (optional)
!     output grid roughness (optional)
!
!   output files:  (including scratch files)
!     nmm nemsio file
!
!   exit states:
!     cond =     0 - successful run
!          =    33 - error reading input grid nemsio file
!          =    34 - error writing output grid nemsio file
!          =    35 - error reading output grid nemsio file
!          =    36 - error updating output grid netcdf file
!          =    39 - invalid choice of output file type
!          =    40 - error initializing mpi environment
!          =    42 - invalid output file type
!          =    43 - error reading grid specs from output file
!          =    50 - error opening fort.81 config namelist
!          =    51 - error reading fort.81 config namelist
!          =    56 - bad open output grid latitude file
!          =    57 - bad read of output grid lat file grid header
!          =    60 - error reading edas restart file
!          =    61 - error opening edas restart file
!          =    65 - error reading output grid orog file
!          =    66 - error reading output grid lat file
!          =    67 - error reading output grid lon file
!          =    68 - error reading output grid tbot file
!          =    69 - error reading output grid land mask file
!          =    77 - invalid choice of input file type 
!          =    83 - error in surface_chgres module
!
! remarks: none
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

 use program_setup, only        : read_config_namelist,   &
                                  read_output_grid_specs

 use read_data, only            : read_input_file

 use interp_data, only          : interp

 use write_data, only           : write_output_data_driver

 implicit none

 integer                       :: ierr
 
 CALL W3TAGB('COLDSTART_WRF',2005,0210,0000,'NP2')

 call mpi_init(ierr)

 if (ierr /= 0) then
   print*,"- ERROR INITIALIZING MPI.  IERR IS: ", ierr
   call w3tage('COLDSTART_WRF')
   call errexit(40)
 end if

 call read_config_namelist

 call read_output_grid_specs

 call read_input_file

! interpolate land fields from input grid to output grid.
 call interp

! update nmm nemsio file with interpolated land fields.
 call write_output_data_driver

 call mpi_finalize(ierr)

 print*,"-------------------------"
 print*,"-- NORMAL TERMINATION ---"
 print*,"-------------------------"

 CALL W3TAGE('COLDSTART_WRF')

 stop 0
 
 end program coldstart_wrf
