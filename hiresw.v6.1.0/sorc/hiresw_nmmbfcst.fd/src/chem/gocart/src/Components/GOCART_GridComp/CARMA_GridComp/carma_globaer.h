!JAS
! The CARMA Globaer object
! ------------------------
  type(carmatype), target :: carma

  logical :: do_hostmodel

   integer :: NX, NY, NZ, NZP1
   integer :: NGROUP, NELEM, NBIN, NGAS, NSOLUTE
   integer :: rhFlag


! Output logical unit numbers
  integer, pointer :: LUNOPRT , &  ! output print file
                      LUNOSTEP, &  ! time step info output
                      LUNIRES , &  ! input restart file
                      LUNORES , &  ! output restart file
                      LUNOHIS , &  ! output history file
                      LUNMIE  , &  ! input and output of Mie coefficients
                      LUNORAD      ! print output from radiation submodel

!  Model startup control variables
   character(len=255), pointer :: prtofil, resifil, resofil, hisofil, &
                                  stepofil, radofil
   logical, pointer            :: do_print, do_hist, do_rest
   integer , pointer           :: ibtime, ietime, &
                                  nprint, nhist, nrest, &
                                  pprint, phist, prest, khist
   real(kind=f), pointer       :: endtime

!  Gridding information

   integer, pointer                        :: igridv, igridh
   real(kind=f), pointer, dimension(:,:,:) :: xmet, ymet, zmet
   real(kind=f), pointer, dimension(:,:,:) :: xc, yc, zc
   real(kind=f), pointer, dimension(:,:,:) :: dx, dy, dz
   real(kind=f), pointer, dimension(:,:,:) :: xl, yl, xu, yu
   real(kind=f), pointer, dimension(:,:,:) :: zl
   real(kind=f), pointer, dimension(:,:)   :: rlon, rlat
   real(kind=f), pointer                   :: rlon0, rlat0, rlat1, rlat2, &
                                              hemisph
   real(kind=f), pointer                   :: dlon, dlat
   real(kind=f), pointer                   :: dom_llx, dom_urx, dom_lly, &
                                              dom_ury
   character(len=255), pointer             :: gridname


!  Model option & control variables

   logical, pointer    :: do_coag, do_grow, do_thermo, do_ew, do_ns, &
                          do_vtran, &
                          do_varstep, do_step, do_ccoef, &
                          do_error, do_netcdf, do_parcel
   logical, pointer, dimension(:,:) :: if_nuc  !(NELEM,NELEM)
   logical, pointer, dimension(:)   :: if_sec_mom, is_grp_ice, is_grp_mixed
                                       !(NGROUP)
   real(kind=f), pointer :: time, dtime, dtmin, dtmax, dpctol, dgstol, &
                            conmax, &
                            period_nuc, maxsubsteps, minsubsteps, dtime_save
   real(kind=f), pointer, dimension(:)   :: time_nuc  ! NGROUP
   integer, pointer    :: itime, ntsubsteps, ifall, icoagop, icollec, &
                          itbnd_pc, ibbnd_pc, itbnd_gc, ibbnd_gc, itbnd_ptc, &
                          ibbnd_ptc, ihoradv, ncdf_file
   integer, pointer, dimension(:)    :: nelemg, ncore, ishape, ienconc, &
                                        ! NGROUP
                                        imomelem, inucgas
   integer, pointer, dimension(:)    :: igelem, itype, icomp, igrowgas, &
                                        nnuc2elem, & !NELEM
                                        ievp2elem, isolelem, nnucelem
   integer, pointer, dimension(:,:)   :: icoag !(NGROUP,NGROUP)
   integer, pointer, dimension(:,:)   :: inucproc, inuc2elem, icorelem 
                                         !(NELEM,NELEM)
   integer, pointer, dimension(:,:)   :: icoagelem, & ! (NELEM,NGROUP)
                                         inucelem     ! (NELEM,NELEM*NGROUP)
   integer, pointer, dimension(:,:,:) :: inuc2bin, &  ! (NBIN,NGROUP,NGROUP)
                                         ievp2bin, &  ! (NBIN,NGROUP,NGROUP)
                                         nnucbin      ! (NGROUP,NBIN,NGROUP)
   integer, pointer                   :: inucbin(:,:,:,:) 
                                         ! (NBIN*NGROUP,NGROUP,NBIN,NGROUP)
   character(len=255), pointer               :: simtitle
   character(len=255), pointer, dimension(:) :: elemname,  & ! (NELEM)
                                                groupname, & ! (NGROUP)
                                                gasname,   & ! (NGAS)
                                                solname      ! (NSOLUTE)

!  Particle grid structure

   real(kind=f), pointer, dimension(:)         :: rmin => null(), &
                                                  rmassmin => null(), &
                                                  rmrat => null()
   real(kind=f), pointer, dimension(:,:)       :: r => null(), &
                                                  rmass => null(), &
                                                  vol => null(), &
                                                  dr => null(), &
                                                  dm => null(), &
                                                  dv => null(), &
                                                  rmassup => null(), &
                                                  rup => null(), &
                                                  rlow => null()
   real(kind=f), pointer, dimension(:,:,:,:)   :: diffmass => null()
   real(kind=f), pointer, dimension(:,:,:,:,:) :: rhop => null()
   real(kind=f), pointer, dimension(:)         :: rhoelem => null(), &
                                                  eshape => null()

!  Model primary variables

   real(kind=f), pointer, dimension(:,:,:,:,:) :: pc
   real(kind=f), pointer, dimension(:,:,:,:)   :: gc
   real(kind=f), pointer, dimension(:,:,:)     :: ptc

!  Model secondary variables

   real(kind=f), pointer, dimension(:,:,:,:,:) ::  pcl => null(), &
                                                   d_pc => null()
   real(kind=f), pointer, dimension(:,:,:,:)   ::  gcl => null(), &
                                                   d_gc => null()
   real(kind=f), pointer, dimension(:,:,:)     ::  ptcl => null(), &
                                                   d_ptc => null()
   real(kind=f), pointer, dimension(:)         ::  pcmax => null()
   real(kind=f), pointer, dimension(:,:,:,:)   ::  pconmax => null()
   real(kind=f), pointer, dimension(:,:,:,:,:) ::  coaglg => null()
   real(kind=f), pointer, dimension(:,:,:,:,:) ::  coagpe => null()
   real(kind=f), pointer, dimension(:,:,:)     ::  rnuclg => null()
   real(kind=f), pointer, dimension(:,:)       ::  rnucpe => null(), &
                                                   growpe => null(), &
                                                   evappe => null()
   real(kind=f), pointer, dimension(:,:)       ::  growlg => null(), &
                                                   evaplg => null()
   real(kind=f), pointer, dimension(:)         ::  gasprod => null()
!   real(kind=f), pointer, dimension(:)         ::  vertdifd => null(), &
!                                                   vertdifu => null()
   real(kind=f), pointer, dimension(:,:)     :: ptc_topbnd, ptc_botbnd
   real(kind=f), pointer, dimension(:,:,:)   :: ftopgas, fbotgas, &
                                                gc_topbnd, gc_botbnd
   real(kind=f), pointer, dimension(:,:,:,:) :: ftoppart, fbotpart, &
                                                pc_topbnd, pc_botbnd
   real(kind=f), pointer, dimension(:,:)     :: cmf => null()
   logical, pointer, dimension(:,:)          :: totevap => null()
   integer, pointer, dimension(:)            :: inucmin => null(), &
                                                inucstep => null()

!  Coagulation kernels and bin pair mapping

   real(kind=f), pointer :: ck0 => null(), grav_e_coll0 => null()
   real(kind=f), pointer, dimension(:,:,:,:,:)     :: ckernel => null()
   real(kind=f), pointer, dimension(:,:,:,:,:,:,:) :: pkernel => null()
   real(kind=f), pointer, dimension(:,:,:,:,:)     :: volx => null()
   integer, pointer, dimension(:,:,:)              :: ilow => null(), &
                                                      jlow => null(), &
                                                      iup => null(), &
                                                      jup => null()
   integer, pointer, dimension(:,:)                :: npairl => null(), &
                                                      npairu => null()

!  Coagulation group pair mapping

   integer, pointer, dimension(:,:,:)  :: iglow => null(), &
                                          jglow => null(), &
                                          igup => null(), &
                                          jgup => null()

!  Particle fall velocities, transport rates, and coagulation kernels

   real(kind=f), pointer, dimension(:,:,:)         :: bpm => null(), &
                                                      vf => null(), &
                                                      re => null()
   real(kind=f), pointer                       :: vf_const => null()

! Atmospheric Structure 

   real(kind=f), pointer                   :: zbot
   real(kind=f), pointer, dimension(:,:,:) :: rhoa, t, p, u, v, &
                                              rhoaold, relhum, told, pold
   real(kind=f), pointer, dimension(:,:)   :: p_surf, p_top, t_surf
   real(kind=f), pointer, dimension(:)     :: rmu, thcond
   real(kind=f), pointer, dimension(:,:,:) :: dkz, dkx, dky, w

!  Condensational growth parameters

   real(kind=f), pointer, dimension(:)       :: gwtmol => null()
   real(kind=f), pointer, dimension(:,:)     :: diffus => null(), &
                                                rlhe => null(), &
                                                rlhm => null()
   real(kind=f), pointer, dimension(:,:,:,:) :: pvapl => null(), &
                                                pvapi => null()
   real(kind=f), pointer, dimension(:)       :: surfctwa => null(), &
                                                surfctiw => null(), &
                                                surfctia => null()
   real(kind=f), pointer, dimension(:,:)     :: akelvin => null(), &
                                                akelvini => null()
   real(kind=f), pointer, dimension(:,:,:)   :: ft => null(), &
                                                gro => null(), &
                                                gro1 => null()
   real(kind=f), pointer, dimension(:,:)     :: gro2 => null()
   real(kind=f), pointer, dimension(:,:,:)   :: gvrat => null()
   real(kind=f), pointer, dimension(:,:,:,:) :: supsatl => null(), &
                                                supsati => null(), &
                                                supsatlold => null(), &
                                                supsatiold => null()
   real(kind=f), pointer, dimension(:,:,:)   :: scrit => null()
   real(kind=f), pointer, dimension(:)       :: sol_ions => null(), &
                                                solwtmol => null(), &
                                                rhosol => null()
   real(kind=f), pointer, dimension(:,:)     :: rlh_nuc => null()

!  Aerosol information
   real(kind=f), pointer, dimension(:,:,:,:)   :: fluxpcout


   do_hostmodel = carma%do_hostmodel

!  Associate the local variables with the object
!  ---------------------------------------------
   NX = carma%NX
   NY = carma%NY
   NZ = carma%NZ
   NZP1 = carma%NZP1
   NGAS = carma%NGAS
   NBIN = carma%NBIN
   NGROUP = carma%NGROUP
   NELEM  = carma%NELEM

   rhFlag = carma%rhFlag

!  Model logical units for I/O
   LUNOPRT  => carma%LUNOPRT
   LUNOSTEP => carma%LUNOSTEP
   LUNIRES  => carma%LUNIRES
   LUNORES  => carma%LUNORES
   LUNOHIS  => carma%LUNOHIS
   LUNMIE   => carma%LUNMIE
   LUNORAD  => carma%LUNORAD


!  Model startup control variables

   prtofil  => carma%prtofil
   resifil  => carma%resifil
   resofil  => carma%resofil
   hisofil  => carma%hisofil
   stepofil => carma%stepofil
   radofil  => carma%radofil
   do_print => carma%do_print
   do_hist  => carma%do_hist
   do_rest  => carma%do_rest
   ibtime   => carma%ibtime
   ietime   => carma%ietime
   endtime  => carma%endtime
   nprint   => carma%nprint
   nhist    => carma%nhist
   nrest    => carma%nrest
   pprint   => carma%pprint
   phist    => carma%phist
   prest    => carma%prest
   khist    => carma%khist

!  Gridding Information

   igridv => carma%igridv
   igridh => carma%igridh
   xmet => carma%xmet
   ymet => carma%ymet
   zmet => carma%zmet
   xc => carma%xc
   yc => carma%yc
   zc => carma%zc
   dx => carma%dx
   dy => carma%dy
   dz => carma%dz
   xl => carma%xl
   yl => carma%yl
   xu => carma%xu
   yu => carma%yu
   zl => carma%zl
   rlon => carma%rlon
   rlat => carma%rlat
   rlon0 => carma%rlon0
   rlat0 => carma%rlat0
   rlat1 => carma%rlat1
   rlat2 => carma%rlat2
   hemisph => carma%hemisph
   dlon  => carma%dlon
   dlat  => carma%dlat
   dom_llx => carma%dom_llx
   dom_urx => carma%dom_urx
   dom_lly => carma%dom_lly
   dom_ury => carma%dom_ury
   gridname => carma%gridname

!  Model option & control variables

   do_coag => carma%do_coag
   do_grow => carma%do_grow
   do_thermo => carma%do_thermo
   do_ew => carma%do_ew
   do_ns => carma%do_ns
   do_vtran => carma%do_vtran
   do_varstep => carma%do_varstep
   do_step => carma%do_step
   do_ccoef => carma%do_ccoef
   do_error => carma%do_error
   do_netcdf => carma%do_netcdf
   do_parcel => carma%do_parcel
   if_nuc => carma%if_nuc
   if_sec_mom => carma%if_sec_mom
   is_grp_ice => carma%is_grp_ice
   is_grp_mixed => carma%is_grp_mixed
   time => carma%time
   dtime => carma%dtime
   dtmin => carma%dtmin
   dtmax => carma%dtmax
   dpctol => carma%dpctol
   dgstol => carma%dgstol
   conmax => carma%conmax
   period_nuc => carma%period_nuc
   maxsubsteps => carma%maxsubsteps
   minsubsteps => carma%minsubsteps
   dtime_save => carma%dtime_save
   time_nuc => carma%time_nuc
   itime => carma%itime
   ntsubsteps => carma%ntsubsteps
   ifall => carma%ifall
   icoagop => carma%icoagop
   icollec => carma%icollec
   itbnd_pc => carma%itbnd_pc
   ibbnd_pc => carma%ibbnd_pc
   itbnd_gc => carma%itbnd_gc
   ibbnd_gc => carma%ibbnd_gc
   itbnd_ptc => carma%itbnd_ptc
   ibbnd_ptc => carma%ibbnd_ptc
   ihoradv => carma%ihoradv
   ncdf_file => carma%ncdf_file
   nelemg => carma%nelemg
   ncore => carma%ncore
   ishape => carma%ishape
   ienconc => carma%ienconc
   imomelem => carma%imomelem
   inucgas => carma%inucgas
   igelem => carma%igelem
   itype => carma%itype
   icomp => carma%icomp
   igrowgas => carma%igrowgas
   nnuc2elem => carma%nnuc2elem
   ievp2elem => carma%ievp2elem
   isolelem => carma%isolelem
   nnucelem => carma%nnucelem
   inucproc => carma%inucproc
   inuc2elem => carma%inuc2elem
   icorelem => carma%icorelem
   inucelem => carma%inucelem
   inuc2bin => carma%inuc2bin
   ievp2bin => carma%ievp2bin
   nnucbin => carma%nnucbin
   inucbin => carma%inucbin
   simtitle => carma%simtitle
   elemname => carma%elemname
   groupname => carma%groupname
   gasname => carma%gasname
   solname => carma%solname

!  Particle grid structure

   rmin => carma%rmin
   rmassmin => carma%rmassmin
   rmrat => carma%rmrat
   r => carma%r
   rmass => carma%rmass
   vol => carma%vol
   dr => carma%dr
   dm => carma%dm
   dv => carma%dv
   rmassup => carma%rmassup
   rup => carma%rup
   rlow => carma%rlow
   diffmass => carma%diffmass
   rhop => carma%rhop
   rhoelem => carma%rhoelem
   eshape => carma%eshape

!  Atmospheric structure

   zbot => carma%zbot
   rhoa => carma%rhoa
   t => carma%t
   p => carma%p
   u => carma%u
   v => carma%v
   rhoaold => carma%rhoaold
   relhum => carma%relhum
   told => carma%told
   pold => carma%pold
   p_surf => carma%p_surf
   p_top => carma%p_top
   t_surf => carma%t_surf
   dkz => carma%dkz
   dkx => carma%dkx
   dky => carma%dky
   w => carma%w

! Model primary vars

   pc => carma%pc
   gc => carma%gc
   ptc => carma%ptc

!  Model secondary variables

   pcl => carma%pcl
   d_pc => carma%d_pc
   gcl => carma%gcl
   d_gc => carma%d_gc
   ptcl => carma%ptcl
   d_ptc => carma%d_ptc
   pcmax => carma%pcmax
   pconmax => carma%pconmax
   coaglg => carma%coaglg
   coagpe => carma%coagpe
   rnuclg => carma%rnuclg
   rnucpe => carma%rnucpe
   growpe => carma%growpe
   evappe => carma%evappe
   growlg => carma%growlg
   evaplg => carma%evaplg
   gasprod => carma%gasprod
!   vertdifd => carma%vertdifd
!   vertdifu => carma%vertdifu
   cmf => carma%cmf
   totevap => carma%totevap
   inucmin => carma%inucmin
   inucstep => carma%inucstep
   ptc_topbnd => carma%ptc_topbnd
   ptc_botbnd => carma%ptc_botbnd
   gc_topbnd  => carma%gc_topbnd
   gc_botbnd  => carma%gc_botbnd
   pc_topbnd  => carma%pc_topbnd
   pc_botbnd  => carma%pc_botbnd
   ftoppart   => carma%ftoppart
   fbotpart   => carma%fbotpart
   ftopgas    => carma%ftopgas
   fbotgas    => carma%fbotgas

!  Coagulation kernels and bin pair mapping
   ck0 => carma%ck0
   grav_e_coll0 => carma%grav_e_coll0
   if(do_coag) then
    icoag => carma%icoag
    icoagelem => carma%icoagelem
    ckernel  => carma%ckernel(NX,NY)%data5d
    pkernel => carma%pkernel(NX,NY)%data7d
    volx => carma%volx
    ilow => carma%ilow
    jlow => carma%jlow
    iup => carma%iup
    jup => carma%jup
    npairl => carma%npairl
    npairu => carma%npairu
!   Coagulation group pair mapping
    iglow => carma%iglow
    jglow => carma%jglow
    igup => carma%igup
    jgup => carma%jgup
   endif

!  Particle fall velocities, transport rates, and coagulation kernels
   vf_const => carma%vf_const

!  Condensational growth parameters
   gwtmol => carma%gwtmol
   diffus => carma%diffus
   rlhe => carma%rlhe
   rlhm => carma%rlhm
   pvapl => carma%pvapl
   pvapi => carma%pvapi
   surfctwa => carma%surfctwa
   surfctiw => carma%surfctiw
   surfctia => carma%surfctia
   akelvin => carma%akelvin
   akelvini => carma%akelvini
   ft => carma%ft
   gro => carma%gro
   gro1 => carma%gro1
   gro2 => carma%gro2
   gvrat => carma%gvrat
   supsatl => carma%supsatl
   supsati => carma%supsati
   supsatlold => carma%supsatlold
   supsatiold => carma%supsatiold
   scrit => carma%scrit
   sol_ions => carma%sol_ions
   solwtmol => carma%solwtmol
   rhosol => carma%rhosol
   rlh_nuc => carma%rlh_nuc

!  Aerosol info

   fluxpcout => carma%fluxpcout

!  Pointers to vars of type carmakerneltype

   bpm => carma%bpm(NX,NY)%data3d
   vf  => carma%vf(NX,NY)%data3d
   re  => carma%re(NX,NY)%data3d
   rmu => carma%rmu(NX,NY)%data1d
   thcond => carma%thcond(NX,NY)%data1d
