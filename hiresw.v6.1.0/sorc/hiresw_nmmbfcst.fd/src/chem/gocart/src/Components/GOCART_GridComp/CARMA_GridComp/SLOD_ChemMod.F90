      module SLOD_ChemMod

!      use carma_precision

#ifdef SINGLE
      integer, parameter :: f = selected_real_kind(6,37)
#else
      integer, parameter :: f = selected_real_kind(15,307)
#endif

      type Chem_Grid
       real(kind=f)          :: lon_min, lon_max, lon_del
       real(kind=f), pointer :: lon(:) => null()
       real(kind=f)          :: lat_min, lat_max, lat_del
       real(kind=f), pointer :: lat(:) => null()
       real(kind=f)          :: ptop
      end type Chem_Grid

      type Chem_Array
       real(kind=f), pointer :: data2d(:,:) => null()
       real(kind=f), pointer :: data3d(:,:,:) => null()
      end type Chem_Array

      type Chem_Bundle
       type(Chem_Grid) :: grid
       real(kind=f), pointer :: delp(:,:,:) => null()
       real(kind=f), pointer :: rh(:,:,:) => null()
       type(chem_array), pointer :: qa(:) => null()
      end type Chem_Bundle


      end module
