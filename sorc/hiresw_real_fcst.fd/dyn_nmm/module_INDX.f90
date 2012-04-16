
      MODULE MODULE_INDX





      INTEGER,ALLOCATABLE,DIMENSION(:) :: IHE,IHW,IVE,IVW,IRAD         &
                                         ,IHEG,IHWG,IVEG,IVWG,IRADG





      INTEGER,ALLOCATABLE,DIMENSION(:,:,:) :: INDX3_WRK




      INTEGER,DIMENSION(-2:2,-2:2) :: INC_UPS




      INTEGER,ALLOCATABLE,DIMENSION(:) :: N_IUP_H,N_IUP_V              &
                                         ,N_IUP_ADH,N_IUP_ADV



      INTEGER,ALLOCATABLE,DIMENSION(:,:) :: IUP_H,IUP_V,IUP_ADH,IUP_ADV


      CONTAINS
         SUBROUTINE init_module_indx
         END SUBROUTINE init_module_indx
      END MODULE MODULE_INDX
