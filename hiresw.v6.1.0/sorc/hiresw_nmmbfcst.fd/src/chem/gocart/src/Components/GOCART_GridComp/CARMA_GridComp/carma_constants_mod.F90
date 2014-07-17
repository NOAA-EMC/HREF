module carma_constants_mod

use carma_precision_mod

implicit none

!--
! Index values of CARMA's flags.  In a given list, begin with 1
! (instead of 0) so that undefined flags will produce an error. 
!
! For example:
! if( itype(ielem) .eq. I_INVOLATILE )then
!
! If itype(ielem) hasn't been defined (and is still 0), we do not want
! to execute the statements that follow.

!  Define values of flag used for specification of
!  horizontal transport algorithm

integer, parameter :: I_PPM = 1
integer, parameter :: I_GALERKIN = 2

!  Define values of flag used for vertical transport
!  boundary conditions

integer, parameter :: I_FIXED_CONC = 1
integer, parameter :: I_FLUX_SPEC = 2

!  Define values of flag used for particle element
!  composition specification

integer, parameter :: I_DUST = 1 
integer, parameter :: I_H2SO4 = 2
integer, parameter :: I_WATER = 3
integer, parameter :: I_ICE = 4
integer, parameter :: I_MIXEDWAT = 5
integer, parameter :: I_BLACKCARBON = 6
integer, parameter :: I_ORGANICCARBON = 7

!  Define values of flag used for particle element
!  type specification

integer, parameter :: I_INVOLATILE = 1
integer, parameter :: I_VOLATILE = 2
integer, parameter :: I_COREMASS = 3
integer, parameter :: I_VOLCORE = 4
integer, parameter :: I_CORE2MOM = 5

!  Define values of flag used for nucleation process
!  specification

integer, parameter :: I_DROPACT = 1
integer, parameter :: I_AERFREEZE = 2
integer, parameter :: I_DROPFREEZE = 3
integer, parameter :: I_MIXEDFREEZE = 4
integer, parameter :: I_MIXEDMELT = 5
integer, parameter :: I_ICEMELT = 6 

!  Define values of flag used specify direction in
!  horizontal transport calculations

integer, parameter :: IDIRX = 1 
integer, parameter :: IDIRY = 2

!  Define values of symbols used to specify horizontal & vertical grid type.
!   Grid selection is made by defining each of the variables
!   <igridv> and <igridh> to one of the grid types known to the model.
!
!   Possible values for igridv:
!       I_CART    cartesian
!       I_SIG     sigma
!
!    Possible values for igridh:
!       I_CART   cartesian
!       I_LL     longitude_latitude
!       I_LC     lambert_conformal
!       I_PS     polar_stereographic
!       I_ME     mercator

integer, parameter :: I_CART = 1
integer, parameter :: I_SIG = 2
integer, parameter :: I_LL = 3
integer, parameter :: I_LC = 4
integer, parameter :: I_PS = 5
integer, parameter :: I_ME = 6 

!  Define values of flag used to specify calculation of solar zenith angle

integer, parameter :: I_FIXED = 1
integer, parameter :: I_DIURNAL = 2

!--
! Physical constants

! Meter-Kilogram-Second (MKS) convention for units
! This convention is different from CARMA's original 
!  Centimeter-Gram-Second (CGS) convention.  Be wary of
!  this conversion to the new convention.

! Use the _f for all literal constants, e.g. 1.2e_f.
! If you omit the _f in the initialization, a compiler may cast this
!  number into single precision and then store it as _f precision.

! Define triple-point temperature (K)
real(kind=f), parameter :: T0 = 273.16_f

! Define constants for circles and trig  
real(kind=f), parameter :: PI = 3.14159265358979_f 
real(kind=f), parameter :: DEG2RAD = pi / 180._f
real(kind=f), parameter :: RAD2DEG = 180._f / pi

! Acceleration of gravity near Earth surface [ m/s^2 ]
real(kind=f), parameter :: GRAV = 9.806_f 

! Define planet equatorial radius [ m ]
real(kind=f), parameter :: REARTH  = 6.37e+6_f 

! Define avogadro's number [ # particles / mole ]
real(kind=f), parameter :: AVG = 6.02252e+23_f
 
! Define Boltzmann's constant [ J / deg_K ]
real(kind=f), parameter :: BK = 1.38054e-23_f 
 
! Define Loschmidt's number [ mole / m^3, @ STP ]
real(kind=f), parameter :: ALOS = 2.68719e+25_f

! Define molecular weight of dry air [ kg / mole ]
real(kind=f), parameter :: WTMOL_AIR = 28.966e-3_f

! Define reference pressure, e.g. for potential temp calcs [ Pa ]
real(kind=f), parameter :: PREF = 1.e5_f

! Define conversion factor for mb to mks [ Pa ] units
real(kind=f), parameter :: RMB2MKS = 100.d+0

! Define universal gas constant [ J / deg_K / mole ]
real(kind=f), parameter :: RGAS = 8.31430_f 

! Define gas constant for dry air [ J / deg_K / mole ]
real(kind=f), parameter :: R_AIR = RGAS / WTMOL_AIR

! Define number of seconds per the planet's day [ s / d ]
real(kind=f), parameter :: SCDAY = 86400._f
 
! Define specific heat at constant pres of dry air [ m^2 / s^2 / deg_K ]
real(kind=f), parameter :: CP = 1.004e3_f 
 
! Define ratio of gas constant for dry air and specific heat
real(kind=f), parameter :: RKAPPA = R_AIR / CP

! Define mass density of liquid water [ kg / m^3 ]
real(kind=f), parameter :: RHO_W = 1.e3_f

! Define mass density of water ice [ kg / m^3 ]
real(kind=f), parameter :: RHO_I = 0.93e3_f

!--
! For air viscosity calculations
! Air viscosity <rmu> is from Sutherland's equation (using Smithsonian
!   Meteorological Tables, in which there is a misprint -- T is deg_K, not
!   deg_C.

real(kind=f), parameter :: rmu_0 = 1.8325e-5_f  
real(kind=f), parameter :: rmu_t0 = 296.16_f     
real(kind=f), parameter :: rmu_c = 120._f        
real(kind=f), parameter :: rmu_const = rmu_0 * (rmu_t0 + rmu_c)
                                  ! [=] kg/(m*s*K**0.5) 

!--
! Numerical constants
!
! JAS -- these parameters used to reside in precision.h or globaer.h.
! Their declarations look identical in any precision, so move them here to
! carma_constants.

real(kind=f), parameter :: ONE = 1._f

!  Define smallest possible number such that ONE + ALMOST_ZERO > ONE

real(kind=f), parameter :: ALMOST_ZERO = epsilon( ONE )
real(kind=f), parameter :: ALMOST_ONE  = ONE - ALMOST_ZERO

! Define small particle number concentration
! [ # / x_units / y_units / z_units ]

real(kind=f), parameter :: SMALL_PC = tiny( ONE )

!  Define particle number concentration [ # / ? ]
!  used to decide whether to bypass microphysical processes.
!  Set it to SMALL_PC to never bypass the calculations.

real(kind=f), parameter :: FEW_PC = SMALL_PC * 1.e0_f

!  Define core fraction (for core mass and second moment) used
!  when particle number concentrations are limited to SMALL_PC

real(kind=f), parameter :: FIX_COREF = epsilon( ONE )

end module 
