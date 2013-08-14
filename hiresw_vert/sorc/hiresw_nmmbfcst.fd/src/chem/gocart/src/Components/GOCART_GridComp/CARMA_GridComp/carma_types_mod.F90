! PRC/JAS 5/11/2007
! This module defines the type "carmatype" (and potentially other types) and
! provides subroutines to create and destroy this type.
! The create routine additionally finalizes the coupling of the host model
! to CARMA.  The assumption re: particle concentrations is that the tracer
! array from the host model is units of Mass Mixing Ratio and so we have
! to transfer the input q-array to appropriate CARMA units.
!
! The procedure for adding a variable to the type:
!  1) Add the variable as a scalar or pointer in the type definition (top
!     of this module)
!  2) If the new var is dynamic, allocate the variable in the create routine
!  3) Deallocate the variable in the destroy routine
!  4) Add an alias for the variable (a pointer) to carma_globaer.h and
!     associate it with the variable in this typedef.  
!     See separate carma_globaer.h file.

  module carma_types_mod

      use carma_constants_mod
!      use Chem_UtilMod

      implicit none
real(kind=f) :: qmin, qmax
! New type carmakerneltype is for properties that used to depend on vertical
!  level (k) and now depend on vertical level and horizontal position (ix,iy).
!  If the old array had 1 dim, it's now a 2-D array of the old 1-D array.
!
! Examples: rmu(k) is now carma%rmu(ix,iy)%data1d   
!           ckernel(k,ibin,jbin,igrp,jgrp) is now carma%ckernel(ix,iy)%data5d 
!
! This construct allows properties to have horizontal variability that wasn't
!  needed in the old column model.  The fall velocity in the lowest atmospheric
!  layer can vary according to the air density in each grid box.
!
! Even though it appears as if each var of type carmakernel can have 7 fields,
!  only 1 field will be allocated and deallocated.  This field is the one that
!  has the same dimension as the old array without horizontal variability.
!
! -JAS
! June 25, 2007

      type carmakerneltype      ! container for multi-spatial dimension kernels
        real(kind=f), pointer, dimension(:)             :: data1d
        real(kind=f), pointer, dimension(:,:)           :: data2d
        real(kind=f), pointer, dimension(:,:,:)         :: data3d
        real(kind=f), pointer, dimension(:,:,:,:)       :: data4d
        real(kind=f), pointer, dimension(:,:,:,:,:)     :: data5d
        real(kind=f), pointer, dimension(:,:,:,:,:,:)   :: data6d
        real(kind=f), pointer, dimension(:,:,:,:,:,:,:) :: data7d
      end type carmakerneltype
         
!--

      type carmatype

!       Am I asking the host model to supply parameters?
        logical :: do_hostmodel

        integer :: NX, NY, NZ, NZP1
        integer :: NGROUP, NELEM, NBIN, NGAS
        integer :: NSOLUTE
        integer :: rhflag

        real(kind=f) :: dxfix, dyfix, dzfix

! Output logical unit numbers
! This is a hack for now...not clear yet how to handle print output
        integer :: LUNOPRT , &  ! output print file
                   LUNOSTEP, &  ! time step info output
                   LUNIRES , &  ! input restart file
                   LUNORES , &  ! output restart file
                   LUNOHIS , &  ! output history file
                   LUNMIE  , &  ! input and output of Mie coefficients
                   LUNORAD      ! print output from radiation submodel

! Model startup control variables
!
!  (These variables are defined fresh for each new run, both
!   for cold starts and restarts, and do not get dumped into
!   the output restart file at the end of a run.  They are
!   all defined at the beginning of the init routine.  They
!   control the type of initialization that will be performed,
!   as well as control things that can change from run to run
!   within a single simulation.)
!
!   ibtime    Beginning timestep index for this run 
!   ietime    Ending timestep index for this run 
!   endtime   Total simulation time for this run
!   nprint    # of timesteps between print reports (used when > 0)
!   nhist     # of timesteps between history output (used when > 0)
!   nrest     # of timesteps between restart output (used when > 0)
!   pprint    time period between print reports  (used when nprint < 0)
!   phist     time period between history outputs (used when nhist < 0)
!   prest     time period between restart outputs (used when nrest < 0)
!   khist     Counter for # of history timepoints output this run
!   prtofil   Name of output print file
!   resifil   Name of input restart file
!   resofil   Name of output restart file
!   hisofil   Name of output history file
!   stepofil  Name of time-step diagnostics file
!   radofil   Name of radiation submodel print output file
!   do_print  .t. if print output during timestepping is desired
!   do_hist   .t. if history output during timestepping is desired
!   do_rest   .t. if restart output during timestepping is desired
!
       character(len=255) :: prtofil, resifil, resofil, hisofil, &
                             stepofil, radofil
       logical            :: do_print, do_hist, do_rest
       integer            :: ibtime, ietime, &
                             nprint, nhist, nrest, &
                             pprint, phist, prest, khist
       real(kind=f)       :: endtime

! Model Grid
!
!  igridv     flag to specify desired vertical grid coord system    {initatm}
!  igridh     flag to specify desired horizontal grid coord system  {initatm}
!  xmet       Horizontal ds/dx (ds is metric distance)              {initatm}
!  ymet       Horizontal ds/dy (ds is metric distance)              {initatm}
!  zmet       Vertical ds/dz (ds is metric distance)                {initatm}
!  xc         Horizontal position at center of box                  {initatm}
!  yc         Horizontal position at center of box                  {initatm}
!  zc         Altitude at layer mid-point                           {initatm}
!  dx         Horizontal grid spacing                               {initatm}
!  dy         Horizontal grid spacing                               {initatm}
!  dz         Thickness of vertical layers                          {initatm}
!  xl         Horizontal position at lower edge of box              {initatm}
!  yl         Horizontal position at lower edge of box              {initatm}
!  xu         Horizontal position at upper edge of box              {initatm}
!  yu         Horizontal position at upper edge of box              {initatm}
!  zl         Altitude at top of layer                              {initatm}
!  rlon,rlat  Longitude, latitude [deg] at each horiz grid point    {initatm}
!  rlon0      center longitude for LC, PS, LL projections           {initatm}
!  rlat0      true latitude for ME projection                       {initatm}
!  rlat1      #1 true latitude for LC projection                    {initatm}
!  rlat2      #2 true latitude for LC projection                    {initatm}
!  hemisph    +1.=southern, -1.=northern hemisphere for PS, LC proj {initatm}
!  dom_llx    Domain limit, lower left x coordinate                 {initatm}
!  dom_lly    Domain limit, lower left y coordiante                 {initatm}
!  dom_urx    Domain limit, upper right x coordinate                {initatm}
!  dom_ury    Domain limit, upper right y coordinate                {initatm}
!  gridname   Text description of horiz & vert grid coord system    {initatm}

        integer :: igridv, igridh
        real(kind=f), pointer, dimension(:,:,:)     :: xmet => null(), &
                                                       ymet => null(), &
                                                       zmet => null(), &
                                                       xc => null(), &
                                                       yc => null(), &
                                                       zc => null(), &
                                                       dx => null(), &
                                                       dy => null(), &
                                                       dz => null(), &
                                                       xl => null(), &
                                                       yl => null(), &
                                                       xu => null(), &
                                                       yu => null()
        real(kind=f), pointer, dimension(:,:,:)     :: zl => null()
        real(kind=f), pointer, dimension(:,:)       :: rlon => null(), &
                                                       rlat => null()
        real(kind=f)                                :: rlon0,  &
                                                       rlat0, &
                                                       rlat1, &
                                                       rlat2, &
                                                       hemisph, &
                                                       dlon, &
                                                       dlat, &
                                                       dom_llx, &
                                                       dom_urx, &
                                                       dom_lly, &
                                                       dom_ury
         character(len=255)                         :: gridname

! Model option & control variables
!
!   time        Simulation time at end of current timestep [s]
!   dtime       Timestep size [s]
!   dtmin       Minimum time-step
!   dtmax       Maximum time-step
!   dpctol      Maximum change in particle concentrations that will be tolerated
!   dgstol      Maximum change in gas concentrations that will be tolerated
!   conmax      Minumum relative concentration to consider in varstep   {prestep}
!   itime       Timestep index at end of current timestep 
!   igelem      Groups to which elements belong                     {setupbins}
!   itype       Particle type specification array                   {setupbins}
!   icomp       Particle compound specification array               {setupbins}
!   nelemg      Number of elements in group           
!   ncore       Number of core elements (itype = 2) in group           
!   icorelem    Core elements (itype = 2) in group           
!   ienconc     Particle number conc. element for group             {setupbins}
!   ishape      Describes particle shape for group
!   icoag       Coagulation mapping array                           {setupcoag}
!   icoagelem   Coagulation element mapping array                   {setupcoag}
!   ifall       Fall velocity options                               {setupvfall}
!   icoagop     Coagulation kernel options                          {setupckern}
!   icollec     Gravitational collection options                      {setupckern}
!   itbnd_pc    Top boundary condition flag for particles             {init}
!   ibbnd_pc    Bottom boundary condition flag for particles          {init}
!   itbnd_gc    Top boundary condition flag for gas                   {init}
!   ibbnd_gc    Bottom boundary condition flag for gas                {init}
!   itbnd_ptc   Top boundary condition flag for potential temp.       {init}
!   ibbnd_ptc   Bottom boundary condition flag for potential temp.    {init}
!   ihoradv     Specification of horizontal advection algorithm       {init}
!   do_coag     If .true. then do coagulation                         {init}
!   do_grow     If .true. then do condensational growth and evap.     {init}
!   do_thermo   If .true. then do solve thermodynamics equation       {init}
!   do_vtran    If .true. then do vertical transport                  {init}
!   do_ew       If .true. then do east-west transport                 {init}
!   do_ns       If .true. then do north-south transport               {init}
!   do_ccoef    If .true. then calculate coefficients for htran       {htranglk}
!   do_varstep  If .true then use variable time-step                  {init}
!   do_step     If .false. then step was too big, so don't advance    {varstep}
!   do_error    If .true. then do error trapping for debugging        {init}
!   do_netcdf   If .true. then output history in netcdf file format   {init}
!   do_parcel   If .true. then do parcel simulation                   {init}
!   ncdf_file   Netcdf file handle integer for internal netcdf use    {outhis_ncdf}
!   sec_mom     If .true. then core second moment (itype = 3) used    {setupgrow}
!   igrowgas    Gas that condenses into a particle element            {setupgrow}
!   inucgas     Gas that nucleates a particle group                   {setupnuc}
!   if_nuc      Nucleation conditional array                          {setupaer}
!   inucproc    Nucleation conditional array                          {setupaer}
!   nnuc2elem   Number of elements that nucleate to element           {setupnuc}
!   inuc2elem   Nucleation transfers particles into element inuc2elem {setupnuc}
!   ievp2elem   Total evap. transfers particles into group ievp2elem  {setupnuc}
!   ievp2bin    Total evap. transfers particles into bin ievp2bin     {setupnuc}
!   inuc2bin    Nucleation transfers particles into bin inuc2bin      {setupnuc}
!   isolelem    Index of solute for each particle element             {setupnuc}
!   ntsubsteps  Number of time substeps for fast microphysics processes
!   maxsubsteps Maximum number of time substeps allowed
!   minsubsteps Maximum number of time substeps allowed
!   simtitle   Model simulation title string
!   elemname   Names of particle elements
!   groupname  Names of particle elements
!   gasname    Names of gas species
!   solname    Names of solutes

      logical             :: do_coag, do_grow, do_thermo, do_ew, do_ns, do_vtran, &
                             do_varstep, do_step, do_ccoef, &
                             do_error, do_netcdf, do_parcel
      logical, pointer, dimension(:,:) :: if_nuc  !(NELEM,NELEM)
      logical, pointer, dimension(:)   :: if_sec_mom, is_grp_ice, is_grp_mixed !(NGROUP)

      real(kind=f)        :: time, dtime, dtmin, dtmax, dpctol, dgstol, conmax, &
                             period_nuc, maxsubsteps, minsubsteps, dtime_save
      real(kind=f), pointer, dimension(:)   :: time_nuc  ! NGROUP
      integer             :: itime, ntsubsteps, ifall, icoagop, icollec, &
                             itbnd_pc, ibbnd_pc, itbnd_gc, ibbnd_gc, itbnd_ptc, &
                             ibbnd_ptc, ihoradv, ncdf_file
      integer, pointer, dimension(:)    :: nelemg, ncore, ishape, ienconc, &  ! NGROUP
                                           imomelem, inucgas
      integer, pointer, dimension(:)    :: igelem, itype, icomp, igrowgas, nnuc2elem, & !NELEM
                                           ievp2elem, isolelem, nnucelem
      integer, pointer, dimension(:,:)   :: icoag !(NGROUP,NGROUP)
      integer, pointer, dimension(:,:)   :: inucproc, inuc2elem, icorelem !(NELEM,NELEM)
      integer, pointer, dimension(:,:)   :: icoagelem, & ! (NELEM,NGROUP)
                                            inucelem     ! (NELEM,NELEM*NGROUP)
      integer, pointer, dimension(:,:,:) :: inuc2bin, &  ! (NBIN,NGROUP,NGROUP)
                                            ievp2bin, &  ! (NBIN,NGROUP,NGROUP)
                                            nnucbin      ! (NGROUP,NBIN,NGROUP)
      integer, pointer                   :: inucbin(:,:,:,:) ! (NBIN*NGROUP,NGROUP,NBIN,NGROUP)
      character(len=255)                        :: simtitle
      character(len=255), pointer, dimension(:) :: elemname,  & ! (NELEM)
                                                   groupname, & ! (NGROUP)
                                                   gasname,   & ! (NGAS)
                                                   solname      ! (NSOLUTE)

! Particle grid structure
!
!   rmin      Radius of particle in first bin [m] 
!   rmassmin  Mass of particle in first bin [kg]
!   rmrat     Ratio of masses of particles in consecutive bins  {setupaer}
!   r         Radius bins [m]
!   rmass     Mass bins [kg]
!   vol       Particle volume [m^3]
!   dr        Width of bins in radius space [m]
!   dm        Width of bins in mass space [kg]
!   dv        Width of bins in volume space [m^3]
!   rmassup   Upper bin boundary mass [kg]
!   rup       Upper bin boundary radius [m]
!   rlow      Lower bin boundary radius [m]
!   diffmass  Difference between <rmass> values
!   rhop      Mass density of particle groups [kg/m^3]
!   rhoelem   Mass density of particle elements [kg/m^3]
!   eshape    Ratio of particle length / diameter 

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

! Primary model state variables
!
!  pc          Particle concentration                             {initaer}
!  gc          Gas concentration [g/x_units/y_units/z_units]      {initgas}
!  ptc         Potential temperature concentration [g K/x_units/y_units/z_units]

         real(kind=f), pointer, dimension(:,:,:,:,:) :: pc => null()
         real(kind=f), pointer, dimension(:,:,:,:)   :: gc => null()
         real(kind=f), pointer, dimension(:,:,:)     :: ptc => null()

! Secondary model variables
!
!   pcl         Particle concentration at beginning of time-step
!   pcmax       Maximum concentration for each element             {prestep}
!   pconmax     Maximum particle concentration for each grid point
!   gcl         Gas concentration at beginning of time-step
!   ptcl        Potential temperature concentration at beginning of time-step
!   d_pc        Change in particle concentration due to transport
!   d_gc        Change in gas concentration due to transport
!   d_ptc       Change in potential temperature concentration due to transport
!   coaglg      Total particle loss rate due to coagulation for group
!   coagpe      Particle production due to coagulation 
!   rnuclg      Total particle loss rate due to nucleation for group
!   rnucpe      Particle production due to nucleation 
!   growlg      Total particle loss rate due to growth for group
!   growle      Partial particle loss rate due to growth for element 
!   growpe      Particle production due to growth 
!   evaplg      Total particle loss rate due to evaporation for group
!   evapls      Partial particle loss rate due to evaporation for element
!   evappe      Particle production due to evaporation
!   gasprod     Gas production term
!   vertdifu    Upward vertical flux at grid boundary
!   vertdifd    Downward vertical flux at grid boundary
!   ftopgas     Downward gas flux across top boundary of model
!   fbotgas     Upward gas flux across bottom boundary of model
!   ftoppart    Downward particle flux across top boundary of model
!   fbotpart    Upward flux particle across bottom boundary of model
!   pc_topbnd   Particle concentration assumed just above the top boundary
!   pc_botbnd   Particle concentration assumed just below the bottom boundary
!   gc_topbnd   Gas concentration assumed just above the top boundary
!   gc_botbnd   Gas concentration assumed just below the bottom boundary
!   ptc_topbnd  Thermodynamic variable value assumed just above the top boundary
!   ptc_botbnd  Thermodynamic variable value assumed just below the bottom boundary
!   cmf         Core mass fraction in a droplet 
!   totevap     .true. if droplets are totally evaporating to CN
!   inucmin     Index of smallest particle nucleated; used for tot. evap.
!   inucstep    Index of smallest particle nucleated during time step

      real(kind=f), pointer, dimension(:,:,:,:,:) :: pcl => null(), &
                                                     d_pc => null()
      real(kind=f), pointer, dimension(:,:,:,:)   :: gcl => null(), &
                                                     d_gc => null()
      real(kind=f), pointer, dimension(:,:,:)     :: ptcl => null(), &
                                                     d_ptc => null()
      real(kind=f), pointer, dimension(:)         :: pcmax => null()
      real(kind=f), pointer, dimension(:,:,:,:)   :: pconmax => null()
      real(kind=f), pointer, dimension(:,:,:,:,:) :: coaglg => null()
      real(kind=f), pointer, dimension(:,:,:,:,:) :: coagpe => null()
      real(kind=f), pointer, dimension(:,:,:)     :: rnuclg => null()
      real(kind=f), pointer, dimension(:,:)       :: rnucpe => null(), &
                                                     growpe => null(), &
                                                     evappe => null()
      real(kind=f), pointer, dimension(:,:)       :: growlg => null(), &
                                                     evaplg => null()
      real(kind=f), pointer, dimension(:)         :: gasprod => null()
!      real(kind=f), pointer, dimension(:)         :: vertdifd => null(), &
!                                                     vertdifu => null()
      real(kind=f), pointer, dimension(:,:)       :: ptc_topbnd => null(), &
                                                     ptc_botbnd => null()
      real(kind=f), pointer, dimension(:,:,:)     :: ftopgas => null(), &
                                                     fbotgas => null(), &
                                                     gc_topbnd => null(), &
                                                     gc_botbnd => null()
      real(kind=f), pointer, dimension(:,:,:,:)   :: ftoppart => null(), &
                                                     fbotpart => null(), &
                                                     pc_topbnd => null(), &
                                                     pc_botbnd => null()
      real(kind=f), pointer, dimension(:,:)       :: cmf => null()
      logical, pointer, dimension(:,:)            :: totevap => null()
      integer, pointer, dimension(:)              :: inucmin => null(), &
                                                     inucstep => null()

!  Coagulation kernels and bin pair mapping
!
!   ck0           Constant coagulation kernel           {setupaer}
!   grav_e_coll0  Constant value for collection effic.  {setupaer}
!   ckernel       Coagulation kernels [m^3/s]         {setupckern}
!    (ckernel is now type carmakerneltype.  See below.)
!   pkernel       Coagulation production variables      {setupcoag}
!    (pkernel is now type carmakerneltype.  See below.)
!   volx          Coagulation subdivision variable      {setupcoag}
!   ilow          Bin pairs for coagulation production  {setupcoag}
!   jlow          Bin pairs for coagulation production  {setupcoag}
!   iup           Bin pairs for coagulation production  {setupcoag}
!   jup           Bin pairs for coagulation production  {setupcoag}
!   npairl        Bin pair indices                      {setupcoag}
!   npairu        Bin pair indices                      {setupcoag}

      real(kind=f) :: ck0, grav_e_coll0
      real(kind=f), pointer, dimension(:,:,:,:,:)     :: volx => null()
      integer, pointer, dimension(:,:,:)              :: ilow => null(), &
                                                         jlow => null(), &
                                                         iup => null(), &
                                                         jup => null()
      integer, pointer, dimension(:,:)                :: npairl => null(), &
                                                         npairu => null()

!  Coagulation group pair mapping
!
!   iglow      Group pairs for coagulation production  {setupcoag}
!   jglow      Group pairs for coagulation production  {setupcoag}
!   igup       Group pairs for coagulation production  {setupcoag}
!   jgup       Group pairs for coagulation production  {setupcoag}

integer, pointer, dimension(:,:,:) :: iglow, jglow, igup, jgup

!  Particle fall velocities
!
!   bpm       Corrects for non-sphericity and non-continuum effects {setupvfall}
!   vf        Fall velocities at layer mid-pt                       {setupvfall}
!   re        Reynolds' number based on <vfall>                     {setupvfall}
!    (bpm, vf, and re are now type carmakerneltype.  See below.)
!   vf_const  Constant vertical fall velocity when ifall=0          {setupaer}

         real(kind=f) :: vf_const

! Atmospheric Structure
!
!  zbot      height of the bottom of the model [m]                  {initatm}
!  rhoa      Air density at layer mid-pt [kg/x_units/y_units/z_units] {initatm}
!  t         Air temperature at layer mid-pt [deg_K]                  {initatm}
!  t_surf    Air temperature at surface [deg_K]                      {initatm}
!  p         Atmospheric pressure at layer mid-pt [Pa]                {initatm}
!  rmu       Air viscosity at layer mid-pt [kg/m/s]                   {initatm}
!  thcond    Thermal conductivity of dry air [J/m/sec/deg_K]      {initatm}
!    (rmu and thcond are now type carmakerneltype.  See below.)
!  w         Vertical wind speed at layer boundary [z_units/s]       {initatm}
!  u         East-west wind speed at layer center [x_units/s]        {initatm}
!  v         North-south wind speed at layer center [y_units/s]      {initatm}
!  p_surf    surface pressure [Pa]                                    {initatm}
!  p_top     Atmospheric pressure at top of model domain [Pa]         {initatm}
!  dkz       Vert diffusion coef at layer boundary [z_units^2/s]      {initatm}
!  dkx       Horiz x diffusion coeff at layer boundary [x_units^2/s] {initatm}
!  dky       Horiz y diffusion coeff at layer boundary [y_units^2/s] {initatm}
!  told      Temperature at beginning of time-step
!  pold      Pressure at beginning of time-step
!  rhoaold   Air density at beginning of time-step
!  relhum    Hacked in relative humidity from hostmodel

        real(kind=f)                                :: zbot
        real(kind=f), pointer, dimension(:,:,:)     :: rhoa => null(), &
                                                       t => null(), &
                                                       p => null(), &
                                                       u => null(), &
                                                       v => null(), &
                                                       pold => null(), &
                                                       told => null(), &
                                                       rhoaold => null(), &
                                                       relhum => null()
        real(kind=f), pointer, dimension(:,:)       :: p_surf => null(), &
                                                       p_top => null(), &
                                                       t_surf => null()
        real(kind=f), pointer, dimension(:,:,:)     :: dkz => null(), &
                                                       dkx => null(), &
                                                       dky => null(), &
                                                       w => null()

! Condensational growth parameters
!
!   gwtmol    Molecular weight for gases [kg/mol]                 {setupgrow}
!   diffus    Diffusivity of gas in air [m^2/s]                   {setupgrow}
!   rlhe      Latent heat of evaporation for gas [m^2/s^2]        {setupgrow}
!   rlhm      Latent heat of ice melting for gas [m^2/s^2]        {setupgrow}
!   pvapl     Saturation vapor pressure over water [newton/m^2]   {vaporp}   
!   pvapi     Saturation vapor pressure over ice [newton/m^2]     {vaporp}   
!   surfctwa  Surface tension of water-air interface              {setupgkern}
!   surfctiw  Surface tension of water-ice interface              {setupgkern}
!   surfctia  Surface tension of ice-air interface                {setupgkern}
!   akelvin   Exponential arg. in curvature term for growth       {setupgkern}
!   akelvini  Curvature term for ice                              {setupgkern}
!   ft        Ventilation factor                                  {setupgkern}
!   gro       Growth kernel [UNITS?]                              {setupgkern}
!   gro1      Growth kernel conduction term [UNITS?]              {setupgkern}
!   gro2      Growth kernel radiation term [UNITS?]               {setupgkern}
!   gvrat     For converting particle growth to gas loss [UNITS?] {setupgkern}
!   supsatl   Supersaturation of vapor w.r.t. liquid water [dimless]
!   supsati   Supersaturation of vapor w.r.t. ice [dimless]                  
!   supsatlold Supersaturation (liquid) before time-step    {prestep}
!   supsatiold Supersaturation (ice) before time-step    {prestep}
!   scrit     Critical supersaturation for nucleation [dimless]   {setupnuc}
!   sol_ions  Number of ions solute dissociates into              {setupnuc}
!   solwtmol  Molecular weight of solute                          {setupnuc}
!   rhosol    Mass density of solute                              {setupnuc}
!   rlh_nuc   Nucleation latent heat                              {setupaer}

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



! Microphysical parameters
!
!  fluxpcout   Flux of particles output across model surface boundary 

         real(kind=f), pointer, dimension(:,:,:,:)   :: fluxpcout => null()

! New type carmakerneltype is for properties that used to depend on vertical
!  level (k) and now depend on vertical level and horizontal position (ix,iy)

         type(carmakerneltype), pointer, dimension(:,:) :: ckernel => null() 
         type(carmakerneltype), pointer, dimension(:,:) :: pkernel => null() 

         type(carmakerneltype), pointer, dimension(:,:) :: bpm => null(), &
                                                       vf => null(), &
                                                       re => null()

         type(carmakerneltype), pointer, dimension(:,:) :: rmu => null() 
         type(carmakerneltype), pointer, dimension(:,:) :: thcond => null() 

      end type carmatype

  contains

!     Allocate space for the carma object
!     Couple information from the GCM to the carma object

      subroutine carma_create( NX, NY, NZ                           &
                             , NGROUP, NELEM, NBIN, NGAS, NSOLUTE   &
                             , carma                                &
!     OPTIONAL ARGUMENTS
                             , do_hostmodel, igridv, igridh         &
                             , dtime, t, t_surf, rhoa               &
                             , p_surf, p_top, delp, relhum          &
                             , dxfix, dyfix, dzfix                  &
                             , dlon, dlat                           &
                             , dom_llx, dom_urx                     &
                             , dom_lly, dom_ury                     &
                             , q                                    & 
                             , do_coag, do_vtran                    &
                             , ifall, rhflag                        &
                             , r, rlow, rup, rhop                   &
                             , rmin, rmrat                          &
                             , rc )

      implicit none

!     Inputs
      integer :: NX, NY, NZ
      integer :: NGROUP, NELEM, NBIN, NGAS, NSOLUTE

!     Output
      type( carmatype ) :: carma


!     Optional
      logical, optional :: do_hostmodel
      integer, optional :: igridv, igridh
      real(kind=f), optional :: dtime
      real(kind=f), optional, dimension(NX,NY)    :: t_surf, p_surf, p_top
      real(kind=f), optional, dimension(NX,NY,NZ) :: t, rhoa, delp, relhum
      real(kind=f), optional, dimension(NX,NY,NZ,NBIN,NELEM) :: q
      real(kind=f), optional, dimension( NBIN, NGROUP )     :: r, rlow, rup, rhop
      real(kind=f), optional, dimension( NGROUP )     :: rmin, rmrat
      real(kind=f), optional :: dxfix, dyfix, dzfix, dlon, dlat, &
                                dom_llx, dom_urx, dom_lly, dom_ury
      logical, optional :: do_coag, do_vtran
      integer, optional :: ifall, rhFlag
      integer, optional :: rc

! Local
      integer :: igrp, ielem
      integer :: ix, iy, k, ibin, ier
      real(kind=f) :: pe(NX,NY)
      integer :: NZP1
      logical :: doing_hostmodel

!     Executable code
#ifdef DEBUG
      write(*,*) '+ carma_create'
#endif
      rc = 0
      ier = 0


!     Fill scalar components of carma structure with default values
!     -------------------------------------------------------------
      carma%NX = NX
      carma%NY = NY
      carma%NZ = NZ
      NZP1 = NZ + 1
      carma%NZP1 = NZP1

      carma%NGROUP = NGROUP 
      carma%NELEM = NELEM
      carma%NBIN = NBIN
      carma%NGAS = NGAS
      carma%NSOLUTE = NSOLUTE

!     Output logical units

      carma%LUNOPRT  = 10  ! output print file
      carma%LUNOSTEP = 11  ! time step info output
      carma%LUNIRES  = 12  ! input restart file
      carma%LUNORES  = 13  ! output restart file
      carma%LUNOHIS  = 14  ! output history file
      carma%LUNMIE   = 15  ! input and output of Mie coefficients
      carma%LUNORAD  = 16  ! print output from radiation submodel

!     Model control variables

      carma%prtofil  = 'carma.p'
      carma%resifil  = 'carma_res.in'
      carma%resofil  = 'carma_res.out'
      carma%hisofil  = 'carma_his.bin'
      carma%stepofil = 'substep.out'
      carma%radofil  = 'carma_rad.out'
      carma%do_print = .false.
      carma%do_hist  = .false.
      carma%do_rest  = .false.
      carma%ibtime   = 0
      carma%ietime   = 1
      carma%endtime  = 8000._f
      carma%nprint   = 1
      carma%nhist    = 1
      carma%nrest    = 1
      carma%pprint   = 100.
      carma%phist    = 100.
      carma%prest    = 100000.

!--

      carma%do_coag = .false.
      carma%do_grow =  .false.
      carma%do_thermo =  .false.
      carma%do_ew =  .false.
      carma%do_ns =  .false.
      carma%do_vtran =  .false.
      carma%do_varstep =  .false.
      carma%do_step =  .false.
      carma%do_ccoef =  .false.
      carma%do_error =  .false.
      carma%do_netcdf =  .false.
      carma%do_parcel =  .false.
      carma%itbnd_pc = I_FIXED_CONC
      carma%ibbnd_pc = I_FIXED_CONC


!     If we are supplying inputs from a host model, check variables
!     for consistency and fill in necessary logical and scalar fields.
!     Spatial and particle grid fields will be handled below (after
!     data allocations).
!     ----------------------------------------------------------------
      carma%do_hostmodel = .false.
      if( present(do_hostmodel) ) then
       carma%do_hostmodel = do_hostmodel
       doing_hostmodel = do_hostmodel

       if(doing_hostmodel) then

!         Check computational grid
          if(.not.(present(igridv)) .or. .not.(present(igridh))) rc = 1
          if(rc /= 0) return
          carma%igridv = igridv
          carma%igridh = igridh

!         Check for needed thermodynamic variables
          if(.not.present(t))      rc = 2
          if(.not.present(rhoa))   rc = 2
          if(.not.present(t_surf)) rc = 2
          if(.not.present(p_surf)) rc = 2
          if(.not.present(dtime))  rc = 2
          if(rc /= 0) return

!         Check vertical grid against other needed variables
          if(igridv .eq. I_SIG) then
           if(.not.present(p_top))    rc = 3
           if(.not.present(delp))     rc = 3
          elseif(igridv .eq. I_CART) then
           if(.not.present(dzfix))    rc = 3
          else
           rc = 4
          endif
          if(rc /= 0) return

!         Check horizontal grid against other needed variables
          if(igridh .eq. I_LL) then
           if(.not.present(dlon))    rc = 5
           if(.not.present(dlat))    rc = 5
           if(.not.present(dom_llx)) rc = 5
           if(.not.present(dom_lly)) rc = 5
           if(.not.present(dom_urx)) rc = 5
           if(.not.present(dom_ury)) rc = 5
          elseif(igridh .eq. I_CART) then
           if(.not.present(dxfix))   rc = 5
           if(.not.present(dyfix))   rc = 5
          else
           rc = 6
          endif

!         Check the bins spacing
          if(present(r)) then
           if(r(NBIN,NGROUP) .lt. 0._f)                        rc = 7
           if(present(rmin) .or. present(rmrat))               rc = 7
           if(.not.present(rlow) .or. .not.present(rup))       rc = 7
          elseif(present(rmin)) then
           if(rmin(NGROUP) .lt. 0._f)                          rc = 8
           if(present(r) .or. present(rlow) .or. present(rup)) rc = 8
           if(.not.present(rmrat))                             rc = 8
          else
           rc = 9
          endif

          if(rc /= 0) return

       endif  ! do_hostmodel
      else  ! present( do_hostmodel )
       doing_hostmodel = .false.
      endif

      if(doing_hostmodel) then
       carma%dtime     = dtime
       if(present(do_coag))  carma%do_coag  = do_coag
       if(present(do_vtran)) carma%do_vtran = do_vtran
       if(present(ifall))    carma%ifall    = ifall
       if(present(rhFlag))   carma%rhFlag   = rhFlag
      endif



!     Allocate space for model grid variables
!     ---------------------------------------
      allocate( carma%xmet(NX,NY,NZ), carma%ymet(NX,NY,NZ), &
                carma%zmet(NX,NY,NZ), &
                carma%xc(NX,NY,NZ), carma%yc(NX,NY,NZ), &
                carma%zc(NX,NY,NZ), &
                carma%dx(NX,NY,NZ), carma%dy(NX,NY,NZ), &
                carma%dz(NX,NY,NZ), &
                carma%xl(NX,NY,NZ), carma%yl(NX,NY,NZ), &
                carma%xu(NX,NY,NZ), carma%yu(NX,NY,NZ), &
                carma%zl(NX,NY,NZP1), &
                carma%rlon(NX,NY), carma%rlat(NX,NY), &
                stat=ier)

      if(ier /= 0) then
       rc = 10
       return
      endif

!     Allocate space for particle grid structure
!     ------------------------------------------
      allocate( carma%rmin(NGROUP), carma%rmassmin(NGROUP), &
                carma%rmrat(NGROUP), &
                carma%r(NBIN,NGROUP), carma%rmass(NBIN,NGROUP), &
                carma%vol(NBIN,NGROUP), carma%dr(NBIN,NGROUP), &
                carma%dm(NBIN,NGROUP), carma%dv(NBIN,NGROUP), &
                carma%rmassup(NBIN,NGROUP), carma%rup(NBIN,NGROUP), &
                carma%rlow(NBIN,NGROUP), &
                carma%diffmass(NBIN,NGROUP,NBIN,NGROUP), &
                carma%rhop(NX,NY,NZ,NBIN,NGROUP), &
                carma%rhoelem(NELEM), carma%eshape(NGROUP), stat=ier )

      if( doing_hostmodel )then
        do igrp = 1, NGROUP
          do ibin = 1, NBIN
            carma%rhop(:,:,:,ibin,igrp) = rhop(ibin,igrp)
          enddo
        enddo

        if(present(r)) then
         carma%r = r
         carma%rlow = rlow
         carma%rup = rup
         carma%rmin = -1._f
         call carma_bins( NBIN, NGROUP, rhop, &
                          carma%rmin, carma%rmrat, carma%rmassmin, &
                          r, carma%dr, carma%rmass, carma%rmassup, rlow, rup, &
                          carma%dm, carma%vol, rc )
         carma%rmin = -1._f
        else
         carma%rmin  = rmin
         carma%rmrat = rmrat
         carma%r = -1._f
         call carma_bins( NBIN, NGROUP, rhop, &
                          rmin, rmrat, carma%rmassmin, &
                          carma%r, carma%dr, carma%rmass, carma%rmassup, &
                          carma%rlow, carma%rup, &
                          carma%dm, carma%vol, rc )
         carma%r = -1._f
        endif

      endif

      if(ier /= 0) then
       rc = 11
       return
      endif

!     Allocate and initialize model primary variables
!     -----------------------------------------------
      allocate( carma%pc(NX,NY,NZ,NBIN,NELEM), &
                carma%gc(NX,NY,NZ,NGAS), &
                carma%ptc(NX,NY,NZ), stat=ier )
      if(ier /= 0) then
       rc = 12
       return
      endif

!     Allocate and fill in atmospheric structure variables
!     ----------------------------------------------------
      allocate( carma%rhoa( NX, NY, NZ ) &
              , carma%t( NX, NY, NZ) &
              , carma%p(NX,NY,NZ) &
              , carma%u(NX,NY,NZ) &
              , carma%v(NX,NY,NZ) &
              , carma%pold(NX,NY,NZ) &
              , carma%told(NX,NY,NZ) &
              , carma%rhoaold(NX,NY,NZ) &
              , carma%relhum(NX,NY,NZ) &
              , carma%p_surf(NX,NY) &
              , carma%p_top(NX,NY) &
              , carma%t_surf(NX,NY) &
              , carma%dkz(NX,NY,NZP1) &
              , carma%dkx(NX,NY,NZ) &
              , carma%dky(NX,NY,NZ) &
              , carma%w(NX,NY,NZP1) &
              , stat = ier)

      if(ier /= 0) then
       rc = 13
       return
      endif

!     Allocate model secondary variables
!     ----------------------------------
      allocate( carma%pcl(NX,NY,NZ,NBIN,NELEM), &
                carma%gcl(NX,NY,NZ,NGAS), &
                carma%ptcl(NX,NY,NZ), &
                carma%d_pc(NX,NY,NZ,NBIN,NELEM), &
                carma%d_gc(NX,NY,NZ,NGAS), &
                carma%d_ptc(NX,NY,NZ), &
                carma%pcmax(NELEM), &
                carma%pconmax(NX,NY,NZ,NGROUP), &
                carma%coaglg(NX,NY,NZ,NBIN,NGROUP), &
                carma%coagpe(NX,NY,NZ,NBIN,NELEM), &
                carma%rnuclg(NBIN,NGROUP,NGROUP), &
                carma%rnucpe(NBIN,NELEM), &
                carma%growlg(NBIN,NGROUP), &
                carma%growpe(NBIN,NELEM), &
                carma%evaplg(NBIN,NGROUP), &
                carma%evappe(NBIN,NELEM), &
                carma%gasprod(NGAS), &
!                carma%vertdifd(NZP1), &
!                carma%vertdifu(NZP1), &
                carma%cmf(NBIN,NGROUP), &
                carma%totevap(NBIN,NGROUP), &
                carma%inucmin(NGROUP), &
                carma%inucstep(NGROUP), &
                carma%ptc_topbnd(NX,NY), carma%ptc_botbnd(NX,NY), &
                carma%ftopgas(NX,NY,NGAS), carma%fbotgas(NX,NY,NGAS), &
                carma%gc_topbnd(NX,NY,NGAS), carma%gc_botbnd(NX,NY,NGAS), &
                carma%ftoppart(NX,NY,NBIN,NELEM), &
                carma%fbotpart(NX,NY,NBIN,NELEM), &
                carma%pc_topbnd(NX,NY,NBIN,NELEM), &
                carma%pc_botbnd(NX,NY,NBIN,NELEM), &
                carma%bpm(NX,NY), &
                carma%vf(NX,NY), &
                carma%re(NX,NY), &
                carma%rmu(NX,NY), &
                carma%thcond(NX,NY), stat = ier )

      if(ier /= 0) then
       rc = 14
       return
      endif

!     Allocate spatially varying kernels.
      do iy = 1, NY
       do ix = 1, NX
         allocate ( carma%bpm(ix,iy)%data3d(NZ,NBIN,NGROUP), &
                    carma%vf(ix,iy)%data3d(NZP1,NBIN,NGROUP), &
                    carma%re(ix,iy)%data3d(NZ,NBIN,NGROUP), &
                    carma%rmu(ix,iy)%data1d(NZ), &
                    carma%thcond(ix,iy)%data1d(NZ), &
                    stat = ier )
        if(ier /= 0) then
         rc = 15
         return
        endif
       enddo  ! iy
      enddo  ! ix

!     Allocate space for model option & control variables
!     ---------------------------------------------------
      allocate( carma%if_nuc(NELEM,NELEM), carma%if_sec_mom(NGROUP), &
                carma%is_grp_ice(NGROUP), carma%is_grp_mixed(NGROUP), &
                carma%time_nuc(NGROUP), carma%nelemg(NGROUP), carma%ncore(NGROUP), &
                carma%ishape(NGROUP), carma%ienconc(NGROUP), carma%imomelem(NGROUP), &
                carma%inucgas(NGROUP), carma%igelem(NELEM), carma%itype(NELEM), &
                carma%icomp(NELEM), carma%igrowgas(NELEM), carma%nnuc2elem(NELEM), &
                carma%ievp2elem(NELEM), carma%isolelem(NELEM), carma%nnucelem(NELEM), &
                carma%inucproc(NELEM,NELEM), &
                carma%inuc2elem(NELEM,NELEM), carma%icorelem(NELEM,NELEM), &
                carma%inucelem(NELEM,NELEM*NGROUP), &
                carma%inuc2bin(NBIN,NGROUP,NGROUP), carma%ievp2bin(NBIN,NGROUP,NGROUP), &
                carma%nnucbin(NGROUP,NBIN,NGROUP), carma%inucbin(NBIN*NGROUP,NGROUP,NBIN,NGROUP), &
                carma%elemname(NELEM), carma%groupname(NGROUP), carma%gasname(NGAS), &
                carma%solname(NSOLUTE), stat=ier) 

      if(ier /= 0) then
       rc = 16
       return
      endif

!     Allocate space for coagulation related variables
!     ------------------------------------------------
      if(carma%do_coag) then

       allocate( carma%icoag(NGROUP,NGROUP), &
                 carma%icoagelem(NELEM,NGROUP), &
                 carma%ckernel(NX,NY), &
                 carma%pkernel(NX,NY), &
!                bin pair mapping arrays
                 carma%volx(NGROUP,NGROUP,NGROUP,NBIN,NBIN), &
                 carma%ilow(NGROUP,NBIN,NBIN*NBIN), &
                 carma%jlow(NGROUP,NBIN,NBIN*NBIN), &
                 carma%iup(NGROUP,NBIN,NBIN*NBIN), &
                 carma%jup(NGROUP,NBIN,NBIN*NBIN), &
                 carma%npairl(NGROUP,NBIN), &
                 carma%npairu(NGROUP,NBIN), &
!                group pair mapping
                 carma%iglow(NGROUP,NBIN,NBIN*NBIN), &
                 carma%jglow(NGROUP,NBIN,NBIN*NBIN), &
                 carma%igup(NGROUP,NBIN,NBIN*NBIN), &
                 carma%jgup(NGROUP,NBIN,NBIN*NBIN), stat = ier )
       if(ier /= 0) then
        rc = 17
        return
       endif

!      Allocate spatial varying coagulation kernels
       do iy = 1, NY
        do ix = 1, NX
          allocate ( carma%ckernel(ix,iy)% &
                       data5d(NZ,NBIN,NBIN,NGROUP,NGROUP), &
                     carma%pkernel(ix,iy)% &
                       data7d(NZ,NBIN,NBIN,NGROUP,NGROUP,NGROUP,6), &
                     stat = ier )
         if(ier /= 0) then
          rc = 18
          return
         endif
        enddo  ! iy
       enddo  ! ix

      endif   ! carma%do_coag


!     Condensational growth parameters
!     --------------------------------
      allocate( carma%gwtmol(NGAS), carma%diffus(NZ,NGAS), &
                carma%rlhe(NZ,NGAS), carma%rlhm(NZ,NGAS), &
                carma%pvapl(NX,NY,NZ,NGAS), &
                carma%pvapi(NX,NY,NZ,NGAS), &
                carma%surfctwa(NZ), carma%surfctiw(NZ), carma%surfctia(NZ), &
                carma%akelvin(NZ,NGAS), carma%akelvini(NZ,NGAS), &
                carma%ft(NZ,NBIN,NGROUP), &
                carma%gro(NZ,NBIN,NGROUP),  &
                carma%gro1(NZ,NBIN,NGROUP),  &
                carma%gro2(NZ,NGROUP),  &
                carma%gvrat(NBIN,NELEM,NGAS), &
                carma%supsatl(NX,NY,NZ,NGAS), &
                carma%supsati(NX,NY,NZ,NGAS), &
                carma%supsatlold(NX,NY,NZ,NGAS), &
                carma%supsatiold(NX,NY,NZ,NGAS), &
                carma%scrit(NZ,NBIN,NGROUP), &
                carma%sol_ions(NSOLUTE), carma%solwtmol(NSOLUTE), &
                carma%rhosol(NSOLUTE), &
                carma%rlh_nuc(NELEM,NELEM), stat=ier)

      if(ier /= 0) then
       rc = 19
       return
      endif

!     Allocate and fill in microphysical parameters
!     ---------------------------------------------
      allocate( carma%fluxpcout(NX,NY,NBIN,NELEM), &
                stat=ier)

      if(ier /= 0) then
       rc = 20
       return
      endif

!     Initialize certain model variables
!     ----------------------------------
      carma%ptc_topbnd = 0._f
      carma%ptc_botbnd = 0._f
      carma%gc_topbnd = 0._f
      carma%gc_botbnd = 0._f
      carma%pc_topbnd = 0._f
      carma%pc_botbnd = 0._f
      carma%ftoppart = 0._f
      carma%fbotpart = 0._f
      carma%ftopgas = 0._f
      carma%fbotgas = 0._f

!     Fill in the relative humidity passed from model
      carma%relhum = 0._f
      if(doing_hostmodel) then
       if(present(relhum)) carma%relhum = relhum
      endif


!     If getting from the hostmodel, then fill in the passed variables
      if( doing_hostmodel ) then
       carma%rhoa      = rhoa
       carma%t         = t
       carma%t_surf    = t_surf
       carma%p_surf    = p_surf

       if(present(r)) carma%r = r

       do igrp = 1, NGROUP
       do ibin = 1, NBIN
         if(present(rhop)) carma%rhop(:,:,:,ibin,igrp) = rhop(ibin,igrp)
       end do
       end do

!      Map the q-array to pc/gc

!      q-mapping is difficult if you want to deal with both number
!       concentrations and coremass concentrations because the model does
!       not yet know the values of nelemg( igrp ).  - JAS
!
!      carma%pc is in #/m3.  It has not been converted into the model metrics
!      the conversion happens in initaer.F90.  - JAS

       if(present(q)) then
        carma%gc = tiny(1._f)
        igrp = 1        
        do ielem = 1, NELEM
        do ibin = 1, NBIN
         carma%pc(:,:,:,ibin,ielem) =  q(:,:,:,ibin,ielem) * rhoa / carma%rmass(ibin,igrp)
#ifdef DEBUG
!   call pmaxmin('CARMAcreate:  q       ', q(:,:,:,ibin,ielem)    , qmin, qmax, NX*NY*NZ,1, 1. )
#endif
        end do
        end do
       endif

!      Fill in params on horizontal grid
       carma%igridh = igridh
       if(igridh .eq. I_LL) then
        carma%dlon = dlon
        carma%dlat = dlat
        carma%dom_llx = dom_llx
        carma%dom_lly = dom_lly
        carma%dom_urx = dom_urx
        carma%dom_ury = dom_ury
       else
        carma%dxfix = dxfix
        carma%dyfix = dyfix
       endif

!      Fill in the vertical grid
       carma%igridv = igridv
       if(igridv .eq. I_CART) then
        carma%dzfix = dzfix
       else
        carma%p_top  = p_top
        carma%p_surf = p_surf
!       define vertical sigma values at grid box boundaries.
!       Since we are specifying the vertical sigma coordinate we choose the
!       following prescription: sigma = (p-p_top)/(p_surf-p_top)
!PRC - for now we reduce to assumption of actual lid going to zero, so
! we formulate sigma = p / p_surf
        carma%zl(:,:,NZP1) = 1._f
!        pe = carma%p_surf - carma%p_top
        pe = carma%p_surf
        do k = NZ, 1, -1
         pe = pe - delp(:,:,k)
!         carma%zl(:,:,k) = pe / ( carma%p_surf - carma%p_top )
         carma%zl(:,:,k) = pe / ( carma%p_surf )
        enddo
       endif

      endif  ! do_hostmodel

!     All OK
!     ------
      rc = 0

      end subroutine carma_create

!--

!     Clean Up/Deallocate CARMA object
!     Return q-array to host model coordinates

      subroutine carma_destroy( NX, NY, NZ &
                              , NGROUP, NELEM, NBIN, NGAS &
                              , NSOLUTE &
                              , carma &
!     OPTIONAL ARGUMENTS
                              , t, t_surf, rhoa                &
                              , p_surf, p_top, delp                  &
                              , q                                    & 
                              , rc )

      implicit none

!     Inputs
      integer :: NX, NY, NZ
      integer :: NGROUP, NELEM, NBIN, NGAS
      integer :: NSOLUTE

!     Output
      type( carmatype ) :: carma


!     Optional
      real(kind=f), optional, dimension(NX,NY)    :: t_surf, p_surf, p_top
      real(kind=f), optional, dimension(NX,NY,NZ) :: t, rhoa, delp
      real(kind=f), optional, dimension(NX,NY,NZ,NBIN,NELEM) :: q
      integer, optional :: rc

!     Local vars

       integer :: ix, iy, ibin, igrp, ielem
       integer, parameter :: LUNOJAS = 42
       integer :: ier


!     Executable code
#ifdef DEBUG
      write(*,*) '+ carma_destroy'
#endif
      rc = 0
      ier = 0

!     Map back from pc and gc to q-array

!      q-mapping is difficult if you want to deal with both number
!       concentrations and coremass concentrations because the model does
!       not yet know the values of nelemg( igrp ).  - JAS
!
!      carma%pc and carma%rhoa are concentrations in model coords, so the
!      metrics cancel.  - JAS

      if(present(q)) then
        igrp = 1        
        do ielem = 1, NELEM
        do ibin = 1, NBIN
         q(:,:,:,ibin,ielem) = carma%pc(:,:,:,ibin,ielem) / carma%rhoa * carma%rmass(ibin,igrp)
#ifdef DEBUG
!   call pmaxmin('CARMAdestroy:  q     ', q(:,:,:,ibin,ielem)    , qmin, qmax, NX*NY*NZ,1, 1. )
#endif
        end do
        end do
      endif

!     Deallocate space for model grid variables
!     -----------------------------------------
      deallocate( carma%xmet, carma%ymet, carma%zmet, &
                  carma%xc, carma%yc, carma%zc, &
                  carma%dx, carma%dy, carma%dz, &
                  carma%xl, carma%yl, carma%xu, carma%yu, &
                  carma%zl, &
                  carma%rlon, carma%rlat, &
                  stat=ier)
      if(ier /= 0) then
       rc = 10
       return
      endif

!     Deallocate space for particle grid structure
!     --------------------------------------------
      deallocate( carma%rmin, carma%rmassmin, carma%rmrat, &
                  carma%r, carma%rmass, &
                  carma%vol, carma%dr, &
                  carma%dm, carma%dv, &
                  carma%rmassup, carma%rup, &
                  carma%rlow, &
                  carma%diffmass, &
                  carma%rhop, &
                  carma%rhoelem, carma%eshape, stat=ier )
      if(ier /= 0) then
       rc = 11
       return
      endif

!     Deallocate primary model variables
!     ----------------------------------
      deallocate( carma%pc, carma%gc, carma%ptc, stat=ier)
      if(ier /= 0) then
       rc = 12
       return
      endif

!     Deallocate space for atmospheric structure variables
!     ----------------------------------------------------
      deallocate( carma%rhoa, carma%t, carma%p, &
                  carma%u, carma%v, &
                  carma%rhoaold, carma%relhum, carma%told, carma%pold, &
                  carma%p_surf, carma%p_top, carma%t_surf, &
                  carma%dkz, carma%dkx, carma%dky, carma%w, &
                  stat = ier)
      if(ier /= 0) then
       rc = 13
       return
      endif

!     Deallocate space for secondary model variables
!     ----------------------------------------------
!     Deallocate spatial varying kernels
      do iy = 1, NY
       do ix = 1, NX
        deallocate( carma%bpm(ix,iy)%data3d, &
                    carma%vf(ix,iy)%data3d, &
                    carma%re(ix,iy)%data3d,  &
                    carma%rmu(ix,iy)%data1d, &
                    carma%thcond(ix,iy)%data1d, &
                    stat=ier)
        if(ier /= 0) then
         rc = 14
         return
        endif
       enddo  ! ix
      enddo  ! iy

      deallocate( carma%pcl, &
                  carma%gcl, &
                  carma%ptcl, &
                  carma%d_pc, &
                  carma%d_gc, &
                  carma%d_ptc, &
                  carma%pcmax, &
                  carma%pconmax, &
                  carma%coaglg, &
                  carma%coagpe, &
                  carma%rnuclg, &
                  carma%rnucpe, &
                  carma%growlg, &
                  carma%growpe, &
                  carma%evaplg, &
                  carma%evappe, &
                  carma%gasprod, &
!                  carma%vertdifd, &
!                  carma%vertdifu, &
                  carma%cmf, &
                  carma%totevap, &
                  carma%inucmin, &
                  carma%inucstep, &
                  carma%ptc_topbnd, carma%ptc_botbnd, &
                  carma%ftopgas, carma%fbotgas, &
                  carma%gc_topbnd, carma%gc_botbnd, &
                  carma%ftoppart, carma%fbotpart, &
                  carma%pc_topbnd, carma%pc_botbnd, &
                  carma%bpm, &
                  carma%vf, &
                  carma%re, &
                  carma%rmu, &
                  carma%thcond, &
                  stat=ier )

      if(ier /= 0) then
       rc = 15
       return
      endif

!     Deallocate space for model option & control variables
!     -----------------------------------------------------
      deallocate( carma%if_nuc, carma%if_sec_mom, &
                  carma%is_grp_ice, carma%is_grp_mixed, &
                  carma%time_nuc, carma%nelemg, carma%ncore, &
                  carma%ishape, carma%ienconc, carma%imomelem, &
                  carma%inucgas, carma%igelem, carma%itype, &
                  carma%icomp, carma%igrowgas, carma%nnuc2elem, &
                  carma%ievp2elem, carma%isolelem, carma%nnucelem, &
                  carma%inucproc, &
                  carma%inuc2elem, carma%icorelem, &
                  carma%inucelem, &
                  carma%inuc2bin, carma%ievp2bin, &
                  carma%nnucbin, carma%inucbin, &
                  carma%elemname, carma%groupname, carma%gasname, &
                  carma%solname, stat=ier) 
      if(ier /= 0) then
       rc = 16
       return
      endif

!     Deallocate space for coagulation related variables
!     --------------------------------------------------
      if(carma%do_coag) then
!      Deallocate spatially varying kernels
       do iy = 1, NY
        do ix = 1, NX
         deallocate( carma%ckernel(ix,iy)%data5d, &
                     carma%pkernel(ix,iy)%data7d, &
                     stat=ier)
         if(ier /= 0) then
          rc = 17
          return
         endif
        enddo  ! ix
       enddo  ! iy

       deallocate( carma%icoag, &
                   carma%icoagelem, &
                   carma%ckernel, &
                   carma%pkernel, &
!                  bin pair mapping arrays
                   carma%volx, & 
                   carma%ilow, &
                   carma%jlow, &
                   carma%iup, &
                   carma%jup, &
                   carma%npairl, &
                   carma%npairu, &
!                  group pair mapping
                   carma%iglow, &
                   carma%jglow, &
                   carma%igup, &
                   carma%jgup, &
                   stat=ier )
       if(ier /= 0) then
        rc = 18
        return
       endif
      endif    ! carma%do_coag


!     Deallocate space for condensational growth parameters
!     -----------------------------------------------------
      deallocate( carma%gwtmol, carma%diffus, &
                  carma%rlhe, carma%rlhm, &
                  carma%pvapl, &
                  carma%pvapi, &
                  carma%surfctwa, carma%surfctiw, carma%surfctia, &
                  carma%akelvin, carma%akelvini, &
                  carma%ft, &
                  carma%gro, &
                  carma%gro1, &
                  carma%gro2, &
                  carma%gvrat, &
                  carma%supsatl, &
                  carma%supsati, &
                  carma%supsatlold, carma%supsatiold, &
                  carma%scrit, &
                  carma%sol_ions, carma%solwtmol, carma%rhosol, &
                  carma%rlh_nuc, stat=ier)

      if(ier /= 0) then
       rc = 19
       return
      endif

!     Deallocate space for microphysical parameters
!     ---------------------------------------------
      deallocate( carma%fluxpcout, &
                  stat=ier)
      if(ier /= 0) then
       rc = 20
       return
      endif


!     All OK
!     ------
      rc = 0

  end subroutine carma_destroy

end module
