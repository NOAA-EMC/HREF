! Colarco, May 18, 2007
! sed command to replace F77 comments with F90: sed 's/^c/\!/g'
! F90-version of original CARMA init.f routine (see comments below from
! original routine header).

      subroutine init ( carma, rc )

!     types
      use carma_types_mod

      implicit none

      integer, intent(out) :: rc

!     Local -- to be incorporated into type def ?
      logical :: all_ok
      integer :: nrad, NXORNYP1, NXORNY, NXY, NXP1, NYP1, ns
      character(len=255) :: PROGNAM, PROGTAG

#include "carma_globaer.h"

      rc = 0

      NXY = NX*NY
      NXP1 = NX+1
      NYP1 = NY+1
      NXORNY = NX
      NXORNYP1 = NXP1
      PROGNAM = 'CARMA'
      PROGTAG = '2.2'
      ns = 23

#ifdef DEBUG
       write(*,*) '+ init'
#endif
!
!
!  @(#) init.f  McKie  Oct-1995
!  This routine performs all initializations at the beginning
!  of each run of the model.
!
!  Argument list input:
!    None.
!
!  Argument list output:
!    None.
!
!
!  Include global constants and variables
!
!      include 'globaer.h'
!
!
!  Declare local variables
!
!      logical all_ok
!
!
!  Define formats
!
    1 format('Initialization for ',a,' (Version ',a,')')
    2 format(a,':  ',i6)
    3 format(a,':  ',f12.2)
    4 format(a,':  ',a)
    5 format(/,'Model will run with the following values:')
    6 format(a,':  ',L7)
    7 format('Error--(init) ',a,'=',i5,' not max of ', &
        a,'=',i5,3x,a,'=',i5)
    8 format('Warning--(init): ',a,' because do_parcel = .true.')
    9 format(/,'End of model initialization')
!
!---------------------------------------------------------------------------
!
!
!  Define run control values that can change from run to run.
!   (Could be input from a data file at this spot in the
!    code, but why not just explicitly define them here)
!
!
!  Define begin & end timestep indices for this run.
!   <ibtime> is 0 for a cold start new simulation.
!   <ibtime> is the ending timestep of previous run for a restart.
!   <ietime> is the maximum ending timestep for current run
!  Note:  <time> .gt. <endtime> also ends current run, so
!         <ietime> and <endtime> control when current runs end.
!
      ibtime = 0
      ietime = 1
!
!
!  Total simulation time for this run
!
      endtime = 8.d3
!
!
!  Define timestep size [s].
!
if ( .not. do_hostmodel ) dtime = 1800.d0
!
!
!  Define flag to control if history output is done to netcdf format file
!    .true.  for netcdf history output
!    .false. for traditional Fortran binary output
!
      do_netcdf = .false.
!
!
!  Define names of input & output files for this run.
!
      resifil = 'carma_res.in'		! Restart input file

!     prtofil = '/dev/tty'              ! Output print file
      prtofil = 'carma.p'               ! Output print file

      if( do_netcdf )then
       hisofil = 'carma_his.cdf'        ! Output history file (netcdf)
      else
       hisofil = 'carma_his.bin'        ! Output history file (binary)
      endif

      resofil = 'carma_res.out'         ! Restart output file

      stepofil = 'substep.out'          ! Timestepping output file

      radofil = 'carma_rad.out'         ! Radiation submodel print output
!
!
!  Define frequencies of print and history output:
!   use timestep period (nprint, nhist, nrest) when > 0, otherwise
!   use time period (pprint, phist, prest).
!
!
      nprint = 5        ! timestep period between outputs to print file
      nhist  = 1        ! timestep period between outputs to history file
      nrest  = -20        ! timestep period between outputs to restart file

      pprint = 100.        ! time period between outputs to print file [s]
      phist  = 100.        ! time period between outputs to history file [s]
      prest  = 100000.        ! time period between outputs to restart file [s]
!
!
#undef DO_RAD
#ifdef DO_RAD
!  Define frequency for radiation calcs (same convention as above)
!
      nrad   = -1         ! timestep period between radiation calcs
      prad   =  60.       ! time period between radiation calcs [s]
!
#endif
!
!  Define flags for various processes:
!
!
!  Define flags for overall timestepping output to
!   print, history, & restart files
!
      do_print = .true.
      do_hist = .false.
      do_rest = .false.
!
!
#ifdef DO_RAD
!  Define flag to control whether radiative transfer is to be computed
!
      do_rad = .true.
!
!
!  Define flag to control whether layers below the model domain are included in
!   the radiative transfer model
!
      do_below = .false.
!
#endif
!
!  Define flag to control whether the model is to be run as a parcel simulation
!   (multiple parcels may be simulated by allowing NZ > 1).
!
      do_parcel = .false.
!
!
!  Define flag to control whether any coagulation is to be simulated.
!
      if(.not. do_hostmodel) then
       do_coag = .false.
#ifdef COAGTEST
       do_coag = .true.
#endif
      endif
!
!
!  Define flag to control whether condensational growth is to be simulated
!   (evaporation and nucleation also).
!
      do_grow = .false.
!
!
!  Define flag to control whether temperature is changed by latent heating.
!   Note: Setting this flag to .false. will not prevent 
!   potential temperature <pt> or potential temperature concentration <ptc>
!   from changing due to transport.
!
      do_thermo = .false.
!
!
!  Define flag to control whether vertical transport occurs.
!
      if(.not. do_hostmodel) then
       do_vtran = .true.
#ifdef COAGTEST
       do_vtran = .false.
#endif
      endif

!
!
!  Define flags to control vertical boundary conditions:
!    <itbnd_pc> = I_FIXED_CONC: use specified concentration at the boundary
!               = I_FLUX_SPEC:  use specified flux
!    <ibbnd_pc>: same as <itbnd_pc>, but for bottom boundary;
!   equivalent parameters are defined for gases and potential temperature
!   boundary conditions.
!
      itbnd_pc  = I_FIXED_CONC
      ibbnd_pc  = I_FIXED_CONC
      itbnd_gc  = I_FLUX_SPEC
      ibbnd_gc  = I_FLUX_SPEC
      itbnd_ptc = I_FIXED_CONC
      ibbnd_ptc = I_FIXED_CONC
!
!
!  Define flag to control whether horizontal transport occurs in the
!   east-west or north-south directions (always .false. for a parcel simulation).
!
      do_ew = .false.
      do_ns = .false.
!
!
!  Define flag to control which horizontal advection algorithm is
!   used:
!    <ihoradv> = I_PPM: Use piecewise polynomial method
!    <ihoradv> = I_GALERKIN: Use Galerkin method with Chapeau functions
!
      ihoradv = I_PPM
!
!
!  Define the minimum and maximum number of time substeps for fast
!   microphysics (nucleation and condensation), as well as the threshold
!   particle concentration [cm^-3] in a grid cell, below which the
!   minimum number of substeps is always used.
!
      minsubsteps = 2
      maxsubsteps = 1000
      conmax = 1.e-1_f
!
!
!  Define flag to control whether a variable time-step should be used.
!   Also define min and max time-steps, maximum tolerance in particle
!   concentration changes <dpctol>, maximum tolerance in gas concentration 
!   changes <dgstol>, and minimum relative concentration to consider <conmax>
!   (i.e., for bins with concentrations less than <conmax>*max(pc), we don't
!   worry about how large the changes were).
!
      do_varstep = .false.
!
!
!  Set up things that depend on variable timestepping
!
      if( do_varstep )then
        dtmin  = 2.e-3_f
        dtmax  = 5.e0_f
        dpctol = 0.8_f
        dgstol = 0.2_f
      else
        do_step = .true.
      endif
!
!
!  Define flag to control if error trapping for debugging is to be done
!   (May have no effect on some systems.  Mainly useful for sunos.)
!
      do_error = .true.
!
!
!  End of per run control values definition (usually no changes below here)
!
!---------------------------------------------------------------------------
!
!
!  Open output print file
!
      open(unit=LUNOPRT,file=prtofil,status='unknown')
!
!
!  Open output history file if traditional binary output is requested (non-netcdf)
!
      if( do_hist )then
       if( .not. do_netcdf )then
        open(unit=LUNOHIS,file=hisofil,status='unknown',form='unformatted')
       endif
      endif
!
!
!  Open output restart file
!
      if( do_hist )then
       open(unit=LUNORES,file=resofil,status='unknown',form='unformatted')
      endif
!
!
!  Open output file for timestep diagnostics
!
      open(unit=LUNOSTEP,file=stepofil,status='unknown')
!
!
!  Open file for radiation submodel print output
!
      open(unit=LUNORAD,file=radofil,status='unknown')
!
!
!  Announce entry to this routine 
!
!      if( DEBUG ) write(LUNOPRT,'(/,a)') 'Enter init'
!
!
!  Report model name & version tag
!
      call prtsep ( carma, rc )
      write(LUNOPRT,1) PROGNAM, PROGTAG
!
!
!  Check critical symbolic constants for consistency
! 
      all_ok = .true.
      if( NXORNY .ne. max(NX,NY) )then
       write(LUNOPRT,7) 'NXORNY',NXORNY, 'NX',NX, 'NY',NY
       all_ok = .false.
      endif
      if( NXORNYP1 .ne. max(NXP1,NYP1) )then
       write(LUNOPRT,7) 'NXORNYP1',NXORNYP1, 'NXP1',NXP1, 'NYP1',NYP1
       all_ok = .false.
      endif
      if( .not. all_ok ) stop 1
!
!
!  Set up error trapping (for debugging) if it was requested
!
      if( do_error )then
       call setuperr ( carma, rc )
      endif
!
!
!  Initialize # history timepoints output in this run
!
      khist = 0
!
!
!  Do either:
!    A cold start to begin a new simulation, or
!    A restart from a previous simulation
!
      if( ibtime .eq. 0 )then
       call initnew ( carma, rc )
       if(rc /= 0) return
      else
!       call initres
      endif
!
!
!  Ensure consistency of control flags
! 
      if( do_parcel )then

        if( NXY .ne. 1 )then
          write(LUNOPRT,'(/,a)') 'do_parcel = .true. requires NXY = 1'
          stop 1
        endif
          
        if( do_vtran )then
          do_vtran = .false.
          write(LUNOPRT,8) 'do_vtran set to .false.'
        endif

        if( do_ns )then
          do_ns = .false.
          write(LUNOPRT,8) 'do_ns set to .false.'
        endif

        if( do_ew )then
          do_ew = .false.
          write(LUNOPRT,8) 'do_ew set to .false.'
        endif

      endif
!
!
!  Check to make sure model includes at least 5 layers if <do_vtran>
!   is .true., <NX> is at least 5 if <do_ew> is .true., and <NY>
!   is at least 5 if <do_ns> is .true.
!
      if( do_vtran .and. NZ.lt.5 )then
        write(LUNOPRT,'(/,a)') 'Cannot do vertical transport with NZ < 5'
        stop 1
      endif

      if( do_ew .and. NX.lt.5 )then
        write(LUNOPRT,'(/,a)') 'Cannot do east-west transport with NX < 5'
        stop 1
      endif

      if( do_ns .and. NY.lt.5 )then
        write(LUNOPRT,'(/,a)') 'Cannot do east-west transport with NY < 5'
        stop 1
      endif
!
!
!  Report some initialization values
!
      write(LUNOPRT,5)

      write(LUNOPRT,2) 'ibtime', ibtime
      write(LUNOPRT,2) 'ietime', ietime
      write(LUNOPRT,3) 'endtime', endtime

      write(LUNOPRT,2) 'NX', NX
      write(LUNOPRT,2) 'NY', NY
      write(LUNOPRT,2) 'NZ', NZ

      write(LUNOPRT,3) 'time', time
      write(LUNOPRT,2) 'itime', itime
      write(LUNOPRT,3) 'dtime', dtime

      write(LUNOPRT,6) 'do_error', do_error
      write(LUNOPRT,6) 'do_netcdf', do_netcdf
      write(LUNOPRT,6) 'do_parcel', do_parcel
      write(LUNOPRT,6) 'do_coag', do_coag
      write(LUNOPRT,6) 'do_grow', do_grow
      write(LUNOPRT,6) 'do_thermo', do_thermo
      write(LUNOPRT,6) 'do_vtran', do_vtran
      write(LUNOPRT,2) ' rhFlag', rhFlag
      write(LUNOPRT,6) 'do_ew', do_ew
      write(LUNOPRT,6) 'do_ns', do_ns

#ifdef DO_RAD
      write(LUNOPRT,6) 'do_rad', do_rad
      if( do_rad )then
        if( nrad .gt. 0 )then
          write(LUNOPRT,2) 'nrad', nrad
        else
          write(LUNOPRT,3) 'prad', prad
        endif
      endif
#endif

      write(LUNOPRT,6) 'do_print', do_print
      if( do_print )then
        if( nprint .gt. 0 )then
          write(LUNOPRT,2) 'nprint', nprint
        else
          write(LUNOPRT,3) 'pprint', pprint
        endif
      endif

      write(LUNOPRT,6) 'do_hist', do_hist
      if( do_hist )then
        if( nhist .gt. 0 )then
          write(LUNOPRT,2) 'nhist', nhist
        else
          write(LUNOPRT,3) 'phist', phist
        endif
      endif

      write(LUNOPRT,6) 'do_rest', do_rest
      if( do_rest) then
        if( nrest .gt. 0 )then
          write(LUNOPRT,2) 'nrest', nrest
        else
          write(LUNOPRT,3) 'prest', prest
        endif
      endif

!      call dblank(simtitle, ns)
      write(LUNOPRT,4) 'simtitle', simtitle(1:ns)
!
!
!  Possibly write initial state to print file and history file
!
      if( do_print )then
!        call outprt
      endif

      if( do_hist )then
!        call outhis
      endif
!
!
!  Report end of initialization
!
      write(LUNOPRT,9)
      call prtsep ( carma, rc )
!
!
!  Return to caller with model initializations complete
!

!     Temporary
      if(do_print) close(unit=LUNOPRT)
      if(do_hist)  close(unit=LUNORES)
      if(do_hist .and. .not. do_netcdf) close(unit=LUNOHIS)
      close(unit=LUNORAD)
      close(unit=LUNOSTEP)


      return
      end
