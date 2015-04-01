      module gfs_dyn_gg_def
      use gfs_dyn_machine

      implicit none
      
      REAL(KIND=kind_dbl_prec) ,ALLOCATABLE ::  colrad_a(:),wgt_a(:),
     . wgtcs_a(:),rcs2_a(:),sinlat_a(:),coslat_a(:)
      end module gfs_dyn_gg_def
