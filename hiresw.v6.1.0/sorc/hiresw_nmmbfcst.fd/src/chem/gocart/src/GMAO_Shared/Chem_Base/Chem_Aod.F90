! Compute the Aerosol Optical Thickness for an input Chem Bundle
! - get the chemistry registry
! - pass the chem bundle to the AOD routine

  program Chem_Aod

  use m_die, only: die
  use Chem_MieMod
  use Chem_RegistryMod
  use Chem_BundleMod

  implicit none

  character(len=*), parameter :: myname = 'chem_aod'
  type(Chem_Mie)      :: mie_tables
  type(Chem_Registry) :: regInp   ! chemistry registry
  type(Chem_Bundle)   :: w_c      ! chemistry bundle
  type(Chem_Bundle)   :: w_tau    ! tau chemistry bundle
  type(Chem_Bundle)   :: w_tauabs ! tau absorption chemistry bundle
  type(Chem_Bundle)   :: w_ssa    ! ssa chemistry bundle
  real, pointer       :: tau(:,:,:), ssa(:,:,:)
  integer :: i, j, k, im, jm, km, idx
  integer :: i1, i2, ig, j1, j2, jg, ik, iq, iz
  integer :: nymd, nhms, timidx, freq, rc, ier
  integer :: idxTable
  integer iarg, iargc, argc, lenfile
  logical :: doing_tauabs2d
  logical :: doing_ssa2d
  logical :: doing_geos4
  logical :: doing_dry   ! if true, calculate like rh = 0%
  logical :: new, verbose
  real :: channel, tau_, ssa_, scalerh, maxRH
  real, pointer :: rh(:,:,:)
  character(len=255) :: infile, outfile, filename, rcfile, argv
  character(len=14)  :: datestr
  character(len=8)   :: yyyymmddstr
  character(len=4)   :: hhnnstr

! Parse the command line (see usage() below)
  argc = iargc()
  if(argc .lt. 1) call usage()
  iarg = 0
  outfile = 'chem_aod'
  rcfile  = 'Aod_Registry.rc'
  doing_tauabs2d = .false.
  doing_ssa2d = .false.
  doing_geos4 = .false.
  verbose = .false.
  doing_dry = .false.
  do i = 0, 32767
   iarg = iarg+1
   if(iarg .gt. argc) exit
   call GetArg(iarg, argv)
   select case(argv)
    case ("-geos4")
     doing_geos4 = .true.
    case ("-dryaer")
     doing_dry = .true.
    case ("-v")
     verbose = .true.
    case ("-tauabs2d")
     doing_tauabs2d = .true.
    case ("-ssa2d")
     doing_ssa2d = .true.
    case ("-o")
     if(iarg+1 .gt. argc) call usage()
     iarg = iarg+1
     call GetArg(iarg, outfile)
    case ("-t")
     if(iarg+1 .gt. argc) call usage()
     iarg = iarg+1
     call GetArg(iarg, rcfile)
    case default
     infile = argv
   end select
  end do
  rcfile = trim(rcfile)
  infile = trim(infile)
  outfile = trim(outfile)
  lenfile = len(trim(outfile))

! Scaling of Relative Humidity
! Input optics files (e.g., optics_XX.nc4) have a fractional RH
! coordinate (that is, RH varies 0 - 1 in the file, as in GEOS-5).
! In GEOS-4, however, RH in the chem.eta file is represented as
! a percentage, so it varies 0 - 100%.  For compatibility we
! introduce a flag "-geos4" on execution.  If present, the RH
! from the chem.eta file is divided by 100 on input to the Mie
! calculator.
! If you requested to do the calculation like the aerosols were
! dry, then we set the RH like it is 0%
  scaleRH = 1.
  if(doing_geos4) scaleRH = 1. / 100.
  if(doing_dry)   scaleRH = 0.

! Hardwired: Read the input chemistry registry from Chem_Registry.rc
! This registry file describes the input chemistry bundle being 
! operated on.
! ------------------------------------------------------------------
  regInp = Chem_RegistryCreate(ier,'Chem_MieRegistry.rc')
  if(ier /= 0) call die(myname, 'cannot create registry')
  if(verbose) call Chem_RegistryPrint(regInp)

! Hardwired: use the Chem_MieMod function to create the Mie tables
! -------------------------------------------------------------------------
  mie_tables = Chem_MieCreate(rcfile,ier)

! Hardwired: we know the chem bundle files contain four time steps per file
! We will loop over all the times in the file and write them out
! -------------------------------------------------------------------------
  new = .true.
  do idx = 1, 1

!  Read the chemistry bundle from the infile
!  -------------------------------------------------
   call Chem_BundleRead(infile, nymd, nhms, w_c, rc, freq=freq, &
                        ChemReg=regInp, timidx=idx)

   print *, 'Computing AOD for ', nymd, nhms

!  Check the RH seems sane
   if(.not. doing_geos4 .and. .not. doing_dry) then
    maxrh = maxval(w_c%rh)
    if(maxrh .gt. 2.) then
     print *, 'Maximum RH value = ', maxRH
     print *, 'Should you have chosen "-geos4" as a command line option?'
    endif
   endif

!  ==================================================================================
!  The enclosed bundle of code selects on what calculation we run and what is written

!  Simple for now: only do tau2d
!  Create the output Chem_Bundle
   i1 = w_c%grid%i1
   i2 = w_c%grid%i2
   ig = w_c%grid%ig
   j1 = w_c%grid%j1
   j2 = w_c%grid%j2
   jg = w_c%grid%jg
   im = w_c%grid%im
   jm = w_c%grid%jm
   km = mie_tables%nch

   call Chem_BundleCreate(regInp, &
                          i1, i2, ig, im, &
                          j1, j2, jg, jm, km, &
                          w_tau, ier, &
                          lev=mie_tables%channels, levUnits="m")
   if(doing_ssa2d .or. doing_tauabs2d) then
    call Chem_BundleCreate(regInp, &
                           i1, i2, ig, im, &
                           j1, j2, jg, jm, km, &
                           w_ssa, ier, &
                           lev=mie_tables%channels, levUnits="m")
   endif
   if(doing_tauabs2d) then
    call Chem_BundleCreate(regInp, &
                           i1, i2, ig, im, &
                           j1, j2, jg, jm, km, &
                           w_tauabs, ier, &
                           lev=mie_tables%channels, levUnits="m")
   endif

   if(ier /= 0) call die(myname, 'cannot create tau2d bundle')

   do ik = 1, km
    channel = mie_tables%channels(ik)

    do iq = 1, mie_tables%nq
     idxTable = Chem_MieQueryIdx(mie_tables,mie_tables%vname(iq),rc)

     if(idxTable .ne. -1) then
      do k = 1, w_c%grid%km
      do j = 1, jm
      do i = 1, im
      call Chem_MieQuery(mie_tables, idxTable, 1.*ik, &
                         w_c%qa(iq)%data3d(i,j,k)*w_c%delp(i,j,k)/9.81, &
                         w_c%rh(i,j,k) * scaleRH, tau=tau_, ssa=ssa_)
      w_tau%qa(iq)%data3d(i,j,ik) = w_tau%qa(iq)%data3d(i,j,ik) + tau_
      if(doing_ssa2d) w_ssa%qa(iq)%data3d(i,j,ik) = w_ssa%qa(iq)%data3d(i,j,ik) + tau_*ssa_
      if(doing_tauabs2d) w_tauabs%qa(iq)%data3d(i,j,ik) = &
                       w_tauabs%qa(iq)%data3d(i,j,ik) + (1.-ssa_)*tau_
      enddo
      enddo
      enddo
     endif
    enddo

   enddo

 if(doing_ssa2d) then
      do iq = 1, mie_tables%nq
       idxTable = Chem_MieQueryIdx(mie_tables,mie_tables%vname(iq),rc)
       if(idxTable .ne. -1) then
         w_ssa%qa(iq)%data3d = w_ssa%qa(iq)%data3d / w_tau%qa(iq)%data3d
       endif
      end do
 end if

!  Write the Chem_Bundle out
   write(yyyymmddstr,'(i8.8)') nymd
   write(hhnnstr,'(i4.4)') nhms/100
   datestr = yyyymmddstr//'_'//hhnnstr//'z'

   filename = trim(outfile(1:lenfile)//'.inst2d_ext_x.'//datestr//'.nc4')
   call Chem_BundleWrite( filename, nymd, nhms, 0, w_tau, rc, &
                          verbose=verbose, new=new)

   if(doing_ssa2d) then 
    filename = trim(outfile(1:lenfile)//'.inst2d_ssa_x.'//datestr//'.nc4')
    call Chem_BundleWrite( filename, nymd, nhms, 0, w_ssa, rc, &
                           verbose=verbose, new=new)
   endif

   if(doing_tauabs2d) then 
    filename = trim(outfile(1:lenfile)//'.inst2d_abs_x.'//datestr//'.nc4')
    call Chem_BundleWrite( filename, nymd, nhms, 0, w_tauabs, rc, &
                           verbose=verbose, new=new)
   endif

!  ==================================================================================

!  Don't overwrite the file
!  ------------------------
   new = .false.

  enddo   ! idx (time increment in input file)

! Destroy Mie tables
  call Chem_BundleDestroy(w_tau, rc)
  if(doing_ssa2d) call Chem_BundleDestroy(w_ssa, rc)
  if(doing_tauabs2d) call Chem_BundleDestroy(w_tauabs, rc)
  call Chem_MieDestroy(mie_tables,ier)
  call Chem_RegistryDestroy(regInp, rc)

! ----------------------------------------------------------------------------
  contains

  subroutine usage()
  print *
  print *,'Usage: '
  print *,'  Chem_Aod.x [-tauabs2d -ssa2d '
  print *,'              -o outfile -t rcfile ] infile'
  print *
  print *, 'where'
  print *
  print *, '-geos4       to specify that the relative humidity of input file'
  print *, '             varies 0 - 100 instead of 0 - 1 as in GEOS-5'
  print *, '-dryaer      to specify to ignore the relative humidity in the'
  print *, '             input file; compute all properties like RH = 0%'
  print *, '-tauabs2d    request column integrated absorption aerosol optical thickness'
  print *, '-ssa2d       request column integrated single scattering albedo'
  print *, '-o expid     filename will look like expid.inst2d_ext_x.YYYYMMDD_HHNNz.nc4'
  print *, '-t rcfile    resource file specifying channels for AOD calc'
  print *, '-v           request verbose output'
  print *, 'infile       mandatory input aer_v file'
  print *
  call exit(1)
  end subroutine usage

end
