      MODULE MACHINE

      IMPLICIT NONE
      
!  Machine dependant constants
      integer, parameter :: kind_io4  = 4, kind_io8  = 8 , kind_ior = 8 &
     &,                     kind_evod = 8, kind_dbl_prec = 8            &
     &,                     kind_qdt_prec = 16                          &
     &,                     kind_rad  = 8                               &
     &,                     kind_phys = 8                               &
     &,                     kind_grid = 8                               &
     &,                     kind_REAL = 8                               &! used in cmp_comm
     &,                     kind_INTEGER = 4                             ! -,,-
!
      real(kind=kind_evod), parameter :: mprec = 1.e-12           ! machine precision to restrict dep
      real(kind=kind_evod), parameter :: grib_undef = 9.99e20     ! grib undefine value
!
      END MODULE MACHINE
