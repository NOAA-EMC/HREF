      module gg_def
      use machine, ONLY: KIND_EVOD

      implicit none

      REAL(KIND=KIND_EVOD) ,ALLOCATABLE ::  colrad_r(:),wgt_r(:),
     & wgtcs_r(:),rcs2_r(:),sinlat_r(:),coslat_r(:)
      end module gg_def
