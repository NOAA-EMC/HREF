Module Chem_ArrayMod

   private
   public Chem_Array

!  The Chem_Array, a light weight ESMF-like array
!  ----------------------------------------------
   type Chem_Array
        integer :: rank
        logical :: did_alloc = .false. ! useful to keep track of allocations
        real, pointer :: data2d(:,:)   => null()
        real, pointer :: data3d(:,:,:) => null()

!       PRC
!       Additions to Chem_Array to support CARMA

!       Services requested on per tracer basis
        character(len=255) :: wantServices = ' '
!       Tracer particle dry radius [m] at bin center, and lower and upper
!       bin edges.  Bin center is the volume center of the bin,
!       assuming spherical particles, as in CARMA.  Set = -1. for non-aerosol.
        real :: r = -1.                ! particle radius at bin center [m]
        real :: rlow = -1.             ! particle radius at bin lower bound [m]
        real :: rup = -1.              ! particle radius at bin upper bound [m]
!       Alternatively, could specify rmin and rmrat
        real :: rmin = -1.
        real :: rmrat = -1.
!       Tracer particle dry density [kg m-3], set = -1. for non-aerosol
        real :: rhop = -1.

!       A per-tracer scavenging efficiency in convective updrafts [1 / km]
        real :: fscav = 0.0

!       A kluge for doing RH affected fall velocities in CARMA
        integer :: irhFlag = 0

   end type Chem_Array

 end Module Chem_ArrayMod
