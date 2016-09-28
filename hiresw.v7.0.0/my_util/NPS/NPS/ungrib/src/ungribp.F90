! ****************************************************************************!
! program ungribp                                                             !
!                                                                             !
! Questions, comments, suggestions, even complaints should be directed to:    !
!                        wrfhelp@ucar.edu                                     !
! Externals:                                                                  !
!    Module TABLE                                                             !
!    Module GRIDINFO                                                          !
!    Module FILELIST                                                          !
!    Subroutine READ_NAMELIST                                                 !
!    Subroutine PARSE_TABLE                                                   !
!    Subroutine CLEAR_STORAGE                                                 !
!    Subroutine RD_GRIB                                                       !
!    Subroutine RD_GRIB2                                                      !
!    Subroutine PUT_STORAGE                                                   !
!    Subroutine OUTPUT                                                        !
!    Subroutine C_CLOSE                                                       !
!    Subroutine RRPR                                                          !
!    Subroutine DATINT                                                        !
!    Subroutine FILE_DELETE                                                   !
!                                                                             !
! Kevin W. Manning, NCAR/MMM  - original 'pregrid' code, 1998-2001            !
! Jim Bresch, Michael Duda, Dave Gill, NCAR/MMM - adapted for WPS, 2006       !
!                                                                             !
!*****************************************************************************!
!                                                                             !
!*****************************************************************************!
!                                                                             !
! This program reads GRIB-formatted data and puts it into intermediate format !
! for passing data to a horizontal interpolation program. The intermediate    !
! format can be for WPS, SI, or MM5.                                          !
!                                                                             !
! The program tries to read from files called "GRIBFILE.AAA", "GRIBFILE.AAB", !
! "GRIBFILE.AAC", ... "GRIBFILE.ABA", "GRIBFILE.ABB", ... "GRIBFILE.ZZZ" until!
! it cannot find a file.  This naming format allows for up to 17576 files,    !
! which should be enough for most applications.                               !
!                                                                             !
! The program searches through those "GRIBFILE.???" files, and pulls out all  !
! the requested fields which fall between a starting date and an ending date. !
! It writes the fields from a given time period to a file named according to  !
! the date and hour, i.e., "FILE:YYYY-MM-DD_HH"                               !
!                                                                             !
!*****************************************************************************!
program ungribp

  use table
  use gridinfo
  use storage_module
  use filelist
  use datarray
  use module_debug
  use misc_definitions_module
  use stringutil

  implicit none

  include "mpif.h"

  integer :: nunit1 = 12
  character(LEN=132) :: gribflnm = 'GRIBFILE.001        '

  integer :: debug_level
  integer :: LUN1, KMAX

  integer , parameter :: maxlvl = 100

  real, allocatable, save, dimension(:,:,:) :: TGRID, UGRID, &
                                               VGRID,QGRID, &
                                               CWMGRID, PRESGRID, &
                                               PINTGRID, ZGRID 
  real , dimension(maxlvl) :: plvl
  integer :: iplvl, mype, npes, I,J

  integer :: nlvl, icount, filenum

  real :: startlat, startlon, deltalat, deltalon
  real :: level
  character (LEN=9) ::  field
  character (LEN=3) ::  out_format
  character (LEN=MAX_FILENAME_LEN) ::  prefix

  logical :: readit, spectral

  integer, dimension(255) :: iuarr = 0

  character (LEN=19) :: HSTART, HEND, HDATE
  character(LEN=19) :: hsave  = '0000-00-00_00:00:00'
  integer :: itime
  integer :: ntimes
  integer :: interval
  integer :: ierr, ierrmpi
  logical :: ordered_by_date
  integer :: grib_version
  integer :: vtable_columns


  call mprintf(.true.,STDOUT,' *** Starting program ungrib.exe *** ')
  call mprintf(.true.,LOGFILE,' *** Starting program ungrib.exe *** ')

  call mpi_init(ierrmpi)
  call mpi_comm_rank(MPI_COMM_WORLD,mype,ierrmpi)
  call mpi_comm_size(MPI_COMM_WORLD,npes,ierrmpi)

! -----------------
! Read the namelist, and return the information we want:

  call read_namelist(hstart, hend, interval, ntimes, &
       ordered_by_date, debug_level, out_format, prefix, spectral)

  call mprintf(.true.,INFORM,"Interval value: %i seconds or  %f hours", &
               i1=interval, f1=float(interval)/3600.)

  call mprintf(.true.,STDOUT,'Path to intermediate files is %s',s1=get_path(prefix))
  call mprintf(.true.,LOGFILE,'Path to intermediate files is %s',s1=get_path(prefix))

! -----------------
! Determine GRIB Edition number
  grib_version=0
  call edition_num(nunit1, gribflnm, grib_version, ierr)
  call mprintf((ierr.eq.3),ERROR,"GRIB file problem")
  if (grib_version.eq.2) then
     vtable_columns=11 
#if defined (USE_PNG) && (USE_JPEG2000)
     call mprintf(.true.,INFORM, &
        "Linked in png and jpeg libraries for Grib Edition 2")
#else
     call mprintf(.true.,STDOUT,"WARNING - Grib Edition 2 data detected, and")
     call mprintf(.true.,STDOUT,"        - png and jpeg libs were NOT selected")
     call mprintf(.true.,STDOUT,"        - during the build.")
     call mprintf(.true.,STDOUT,"Stopping")
     call mprintf(.true.,LOGFILE,"WARNING - Grib Edition 2 data detected, and")
     call mprintf(.true.,LOGFILE,"        - png and jpeg libs were NOT selected")
     call mprintf(.true.,LOGFILE,"        - during the build.")
     call mprintf(.true.,LOGFILE,"Stopping")
     call mprintf(.true.,ERROR,"NEED_GRIB2_LIBS")
#endif
  else
     vtable_columns=7 
  endif
  call mprintf(.true.,INFORM,"Reading Grib Edition %i", i1=grib_version)

! -----------------
! Read the "Vtable" file, and put the information contained therein into 
! the module "table".

  call parse_table(debug_level,vtable_columns)

  call mprintf(.true.,DEBUG,"Parsed the vtable.")

! -----------------
! LOOP2 cycles through the list of files named GRIBFILE.???, until it finds
! a non-existent file.  Then we exit

  icount = 1
  LOOP2 : DO

     filenum = mype+1+(icount-1)*npes
     icount = icount + 1

     write(gribflnm,"(A,I3.3)") 'GRIBFILE.',filenum

     ! Set READIT to .TRUE., meaning that we have not read any records yet 
     ! from the file GRIBFLNM.

     call mprintf(.true.,DEBUG,"Reading from gribflnm %s ",s1=gribflnm)

     readit = .TRUE.  ! i.e., "Yes, we want to read a record."

    
! LOOP1 reads through the file GRIBFLNM, exiting under two conditions:
!        1) We have hit the end-of-file
!        2) We have read past the ending date HEND.
!
! Condition 2 assumes that the data in file GRIBFLNM are ordered in time.

     LOOP1 : DO

        ! At the beginning of LOOP1, we are at a new time period.
        ! Clear the storage arrays and associated level information.
        nlvl = 0
        plvl = -999.
        call clear_storage

! LOOP0 reads through the file GRIBFLNM, looking for data of the current 
! date.  It exits under the following conditions.
!          1) We have hit the end-of-file
!          2) The GRIBFLNM variable has been updated to a nonexistent file.
!          3) We start reading a new date and the data are assumed to be 
!             ordered by date.
!          4) We have a valid record and the data are not assumed to be 
!             ordered by date.

        LOOP0 : DO

           ! If we need to read a new grib record, then read one.
           if (READIT) then

              if (grib_version.ne.2) then 
                call mprintf(.true.,DEBUG, &
                     "Calling rd_grib1 with iunit %i", i1=nunit1)
                call mprintf(.true.,DEBUG, &
                     "flnm = %s",s1=gribflnm)
                 ! Read one record at a time from GRIB1 (and older Editions) 
                 call rd_grib1(nunit1, gribflnm, level, field, &
                      hdate, ierr, iuarr, debug_level)
!          write(0,*) 'back from rd_grib1 with field,hdate: ', field,hdate
              else 

                 ! Read one file of records from GRIB2.
                 call mprintf(.true.,DEBUG,"Calling rd_grib2")
                 call rd_grib2(nunit1, gribflnm, hdate, &
                      grib_version, ierr, debug_level)
                 FIELD='NULL'

              endif

              call mprintf(.true.,DEBUG,"ierr = %i ",i1=ierr)
              if (ierr.eq.1) then 
                 ! We have hit the end of a file.  Exit LOOP0 so we can 
                 ! write output for date HDATE, and then exit LOOP1
                 ! to update the GRIBFLNM
                 hsave = hdate
                 print*, 'exiting LOOP0...ierr=1 ', hsave
                 exit LOOP0
              endif

              if (ierr.eq.2) then
                 ! We have hit the end of all the files.  We can exit LOOP2
                 ! because there are no more files to read.
                 print*, 'exiting LOOP2...ierr=2'
                 exit LOOP2
              endif

              call mprintf(.true.,DEBUG, &
               "Read a record %s with date %s", s1=field,s2=hdate(1:13))

           endif

           call mprintf(.true.,DEBUG, &
            "hdate = %s , hsave = %s ",s1=hdate(1:13), s2=hsave(1:13) )

!           if (hdate < hstart) then
!              ! The data read has a date HDATE earlier than the starting
!              ! date HSTART.  So cycle LOOP0 to read the the next GRIB record.
!              READIT = .TRUE.
!              cycle LOOP0
!           endif

           if (FIELD.EQ.'NULL') then
              ! The data read does not match any fields requested
              ! in the Vtable.  So cycle LOOP0 to read the next GRIB record.
              READIT = .TRUE.
              cycle LOOP0
           endif

           if (ordered_by_date .and. (hdate > hsave)) then

              ! Exit LOOP0, because we started to read data from another 
              ! date.

              call mprintf(.true.,DEBUG, &
              "hdate %s > hsave %s so exit LOOP0",s1=hdate,s2=hsave)

              ! We set READIT to FALSE because we have not yet processed
              ! the data from this record, and we will want to process this
              ! data on the next pass of LOOP1 (referring to the next time
              ! period of interest.

              READIT = .FALSE.

              exit LOOP0

           endif

! When we have reached this point, we have a data array ARRAY which has 
! some data we want to save, with field name FIELD at pressure level 
! LEVEL (Pa).  Dimensions of this data are map%nx and map%ny.  Put
! this data into storage.

           if (((field == "SST").or.(field == "SKINTEMP")) .and. &
                (level /= 200100.)) level = 200100.
           iplvl = int(level)

           call mprintf((.not.allocated(rdatarray)),ERROR, &
           "GRIB data slab not allocated in ungrib.F before call to put_storage.")

           call put_storage(iplvl,field, &
                reshape(rdatarray(1:map%nx*map%ny),(/map%nx, map%ny/)),&
                map%nx,map%ny)
           deallocate(rdatarray)

           ! Since we processed the record we just read, we set
           ! READIT to .TRUE. so that LOOP0 will read the next record.
           READIT = .TRUE.

           if (.not. ordered_by_date) then
              if (hdate >= hstart) then
                 hsave = hdate
              endif
              exit LOOP0
           endif

        enddo LOOP0

! When we have reached this point, we have either hit the end of file, or 
! hit the end of the current date.  Either way, we want to output
! the data found for this date.  This current date is in HSAVE.
! However, if (HSAVE == 0000-00-00_00:00:00), no output is necessary,
! because that 0000 date is the flag for the very opening of a file.

        if ((hsave(1:4).ne.'0000').and.(hsave.le.hend)) then

           LUN1=27+filenum-1
    SPECT: if (spectral) then

              KMAX=64
              if (.not. allocated(TGRID)) then
              ALLOCATE(TGRID(map%nx,map%ny,KMAX))
              ALLOCATE(ZGRID(map%nx,map%ny,KMAX+1))
              ALLOCATE(UGRID(map%nx,map%ny,KMAX))
              ALLOCATE(VGRID(map%nx,map%ny,KMAX))
              ALLOCATE(QGRID(map%nx,map%ny,KMAX))
              ALLOCATE(CWMGRID(map%nx,map%ny,KMAX))
              ALLOCATE(PRESGRID(map%nx,map%ny,KMAX))
              ALLOCATE(PINTGRID(map%nx,map%ny,KMAX+1))
              endif

              call COF2GRD (LUN1,map%nx,map%ny,KMAX, &
                            TGRID,ZGRID,UGRID,VGRID, &
                            QGRID,CWMGRID,PRESGRID,PINTGRID )

              call N_S_FLIP(TGRID,map%nx,map%ny,KMAX)
              call N_S_FLIP(UGRID,map%nx,map%ny,KMAX)
              call N_S_FLIP(VGRID,map%nx,map%ny,KMAX)
              call N_S_FLIP(QGRID,map%nx,map%ny,KMAX)
              call N_S_FLIP(PRESGRID,map%nx,map%ny,KMAX)
              do iplvl=1,KMAX
              call put_storage(iplvl,'TT',       TGRID(:,:,iplvl), map%nx,map%ny)
              call put_storage(iplvl,'UU',       UGRID(:,:,iplvl), map%nx,map%ny)
              call put_storage(iplvl,'VV',       VGRID(:,:,iplvl), map%nx,map%ny)
              call put_storage(iplvl,'SPECHUMD', QGRID(:,:,iplvl), map%nx,map%ny)
              call put_storage(iplvl,'PRESSURE', PRESGRID(:,:,iplvl), map%nx,map%ny)
              enddo

              call N_S_FLIP(PINTGRID,map%nx,map%ny,KMAX+1)
              call N_S_FLIP(ZGRID,map%nx,map%ny,KMAX+1)
              do iplvl=1,KMAX+1
              call put_storage(iplvl,'PINT',     PINTGRID(:,:,iplvl), map%nx,map%ny)
              call put_storage(iplvl,'HGT',      ZGRID(:,:,iplvl), map%nx,map%ny)
              enddo

           endif SPECT
           
           if (debug_level .gt. -1) print*, '--- Calling output: '//hsave(1:13)
           call output(hsave, nlvl, maxlvl, plvl, interval, 1, out_format, prefix, debug_level)
           hsave=hdate

           ! If the next record we process has a date later than HEND,
           ! then time to exit LOOP1.
           if ((ordered_by_date) .and. (hdate.gt.hend)) exit LOOP1

        else
           hsave = hdate
        endif

        ! If we hit the end-of-file, its time to exit LOOP1 so we can
        ! increment the GRIBFLNM to read the next file.
        if (ierr.eq.1) exit LOOP1

     enddo LOOP1

! When we have reached this point, we read all the data we want to from 
! file GRIBFLNM (either because we reached the end-of-file, or we 
! read past the date HEND).  So we close the file and cycle LOOP2 to read 
! the next file.

     if (grib_version.ne.2) then
        call c_close(iuarr(nunit1), debug_level, ierr)
        iuarr(nunit1) = 0
     endif 
     hsave = '0000-00-00_00:00:00'

  enddo LOOP2
      
! Now Reread, process, and reoutput.

  call mprintf(.true.,INFORM,"First pass done, doing a reprocess")
!
  call rrpr(hstart, ntimes, interval, nlvl, maxlvl, plvl, debug_level, out_format, prefix)

! Make sure the filedates are in order, with an inefficient sort:

  call sort_filedates

! Interpolate temporally to fill in missing times:

!  call datint(filedates, nfiles, hstart, ntimes, interval, out_format, prefix)

! Now delete the temporary files:

   call file_delete(filedates, nfiles, trim(get_path(prefix))//'PFILE:', interval)

! And Now we are done:
   call MPI_BARRIER(MPI_COMM_WORLD,ierrmpi)

   if ( mype == 0 ) then
   call mprintf(.true.,STDOUT,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
   call mprintf(.true.,STDOUT,'!  Successful completion of ungrib.   !')
!  call mprintf(.true.,STDOUT,"!  We're hauling gear at Bandimere.   !")
   call mprintf(.true.,STDOUT,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')

   call mprintf(.true.,LOGFILE,' *** Successful completion of program ungrib.exe *** ')
   end if

   call close_logfile()

   call mpi_finalize(ierrmpi)

contains

  subroutine sort_filedates
    implicit none

    integer :: n
    logical :: done
    if (nfiles > 1) then
       done = .FALSE.
       do while ( .not. done)
          done = .TRUE.
          do n = 1, nfiles-1
             if (filedates(n) > filedates(n+1)) then
                filedates(size(filedates)) = filedates(n)
                filedates(n) = filedates(n+1)
                filedates(n+1) = filedates(size(filedates))
                filedates(size(filedates)) = '0000-00-00 00:00:00.0000'
                done = .FALSE.
             endif
          enddo
       enddo
    endif
  end subroutine sort_filedates

  subroutine N_S_FLIP(A,nx,ny,nz)

    real A(nx,ny,nz),tmp(nx,ny,nz)
    integer:: I,J,JJ,K,nx,ny,nz

    do K=1,nz
    do J=1,ny
        JJ=ny-J+1
        do I=1,nx
        tmp(I,JJ,K)=A(I,J,K)
        enddo
    enddo
    enddo

    A=tmp

  end subroutine N_S_FLIP

end program ungribp
