!
      MODULE MODULE_INDX
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!
!***  INDEX INCREMENTS FOR MOVING TO NEIGHBORS ON THE E-GRID
!
      INTEGER,ALLOCATABLE,DIMENSION(:) :: IHE,IHW,IVE,IVW,IRAD         &
                                         ,IHEG,IHWG,IVEG,IVWG,IRADG
!----------------------------------------------------------------------
!
!***  INDEX INCREMENTS FOR 3RD INDEX IN WORKING ARRAYS IN PFDHT, DDAMP,
!***  ADVE, AND HDIFF.
!
      INTEGER,ALLOCATABLE,DIMENSION(:,:,:) :: INDX3_WRK
!----------------------------------------------------------------------
!
!***  INCREMENTS TO J1_00 IN UPSTREAM HORIZONTAL ADVECTION.
!
      INTEGER,DIMENSION(-2:2,-2:2) :: INC_UPS
!----------------------------------------------------------------------
!
!***  NUMBER OF POINTS NEEDED IN EACH ROW FOR UPSTREAM COMPUTATIONS
!
      INTEGER,ALLOCATABLE,DIMENSION(:) :: N_IUP_H,N_IUP_V              &
                                         ,N_IUP_ADH,N_IUP_ADV
!
!***  I VALUES IN EACH ROW NEEDED FOR UPSTREAM ADVECTION
!
      INTEGER,ALLOCATABLE,DIMENSION(:,:) :: IUP_H,IUP_V,IUP_ADH,IUP_ADV
!----------------------------------------------------------------------

      CONTAINS
         SUBROUTINE init_module_indx
         END SUBROUTINE init_module_indx
      END MODULE MODULE_INDX
