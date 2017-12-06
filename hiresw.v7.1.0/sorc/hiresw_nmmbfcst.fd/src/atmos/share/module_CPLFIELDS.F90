#include "../../ESMFVersionDefine.h"

module module_CPLFIELDS

  !-----------------------------------------------------------------------------
  ! ATM Coupling Fields: export and import
  !
  !-----------------------------------------------------------------------------

#ifdef WITH_NUOPC
  use ESMF
  use NUOPC
#endif
  
  implicit none
  
  private
  
#ifdef WITH_NUOPC

  ! Regular (non-reduced) Gaussian Grid ---------------
  public            :: gauss2d
  type(ESMF_Grid)   :: gauss2d

  ! Export Fields ----------------------------------------
  public            :: mean_zonal_moment_flx,       & !  1
                       mean_merid_moment_flx,       & !  2
                       mean_sensi_heat_flx,         & !  3
                       mean_laten_heat_flx,         & !  4
                       mean_down_lw_flx,            & !  5
                       mean_down_sw_flx,            & !  6
                       mean_prec_rate,              & !  7
                       inst_zonal_moment_flx,       & !  8
                       inst_merid_moment_flx,       & !  9
                       inst_sensi_heat_flx,         & ! 10
                       inst_laten_heat_flx,         & ! 11
                       inst_down_lw_flx,            & ! 12
                       inst_down_sw_flx,            & ! 13
                       inst_temp_height2m,          & ! 14
                       inst_spec_humid_height2m,    & ! 15
                       inst_u_wind_height10m,       & ! 16
                       inst_v_wind_height10m,       & ! 17
                       inst_temp_height_surface,    & ! 18
                       inst_pres_height_surface,    & ! 19
                       inst_surface_height,         & ! 20
                       mean_net_lw_flx,             & ! 21
                       mean_net_sw_flx,             & ! 22
                       inst_net_lw_flx,             & ! 23
                       inst_net_sw_flx,             & ! 24
                       mean_down_sw_ir_dir_flx,     & ! 25
                       mean_down_sw_ir_dif_flx,     & ! 26
                       mean_down_sw_vis_dir_flx,    & ! 27
                       mean_down_sw_vis_dif_flx,    & ! 28
                       inst_down_sw_ir_dir_flx,     & ! 29
                       inst_down_sw_ir_dif_flx,     & ! 30
                       inst_down_sw_vis_dir_flx,    & ! 31
                       inst_down_sw_vis_dif_flx,    & ! 32
                       mean_net_sw_ir_dir_flx,      & ! 33
                       mean_net_sw_ir_dif_flx,      & ! 34
                       mean_net_sw_vis_dir_flx,     & ! 35
                       mean_net_sw_vis_dif_flx,     & ! 36
                       inst_net_sw_ir_dir_flx,      & ! 37
                       inst_net_sw_ir_dif_flx,      & ! 38
                       inst_net_sw_vis_dir_flx,     & ! 39
                       inst_net_sw_vis_dif_flx,     & ! 40
                       inst_ir_dir_albedo,          & ! 41
                       inst_ir_dif_albedo,          & ! 42
                       inst_vis_dir_albedo,         & ! 43
                       inst_vis_dif_albedo            ! 44
                       
  type(ESMF_Field)  :: mean_zonal_moment_flx,       & !  1
                       mean_merid_moment_flx,       & !  2
                       mean_sensi_heat_flx,         & !  3
                       mean_laten_heat_flx,         & !  4
                       mean_down_lw_flx,            & !  5
                       mean_down_sw_flx,            & !  6
                       mean_prec_rate,              & !  7
                       inst_zonal_moment_flx,       & !  8
                       inst_merid_moment_flx,       & !  9
                       inst_sensi_heat_flx,         & ! 10
                       inst_laten_heat_flx,         & ! 11
                       inst_down_lw_flx,            & ! 12
                       inst_down_sw_flx,            & ! 13
                       inst_temp_height2m,          & ! 14
                       inst_spec_humid_height2m,    & ! 15
                       inst_u_wind_height10m,       & ! 16
                       inst_v_wind_height10m,       & ! 17
                       inst_temp_height_surface,    & ! 18
                       inst_pres_height_surface,    & ! 19
                       inst_surface_height,         & ! 20
                       mean_net_lw_flx,             & ! 21
                       mean_net_sw_flx,             & ! 22
                       inst_net_lw_flx,             & ! 23
                       inst_net_sw_flx,             & ! 24
                       mean_down_sw_ir_dir_flx,     & ! 25
                       mean_down_sw_ir_dif_flx,     & ! 26
                       mean_down_sw_vis_dir_flx,    & ! 27
                       mean_down_sw_vis_dif_flx,    & ! 28
                       inst_down_sw_ir_dir_flx,     & ! 29
                       inst_down_sw_ir_dif_flx,     & ! 30
                       inst_down_sw_vis_dir_flx,    & ! 31
                       inst_down_sw_vis_dif_flx,    & ! 32
                       mean_net_sw_ir_dir_flx,      & ! 33
                       mean_net_sw_ir_dif_flx,      & ! 34
                       mean_net_sw_vis_dir_flx,     & ! 35
                       mean_net_sw_vis_dif_flx,     & ! 36
                       inst_net_sw_ir_dir_flx,      & ! 37
                       inst_net_sw_ir_dif_flx,      & ! 38
                       inst_net_sw_vis_dir_flx,     & ! 39
                       inst_net_sw_vis_dif_flx,     & ! 40
                       inst_ir_dir_albedo,          & ! 41
                       inst_ir_dif_albedo,          & ! 42
                       inst_vis_dir_albedo,         & ! 43
                       inst_vis_dif_albedo            ! 44
  
  ! Import Fields ----------------------------------------
  public            :: inst_sea_surf_temp
  type(ESMF_Field)  :: inst_sea_surf_temp
  
  ! Utility GSM members ----------------------------------
  public            :: global_lats_ptr
  integer, pointer  :: global_lats_ptr(:)
  public            :: lonsperlat_ptr
  integer, pointer  :: lonsperlat_ptr(:)

#endif

  ! Methods
  public fillExportFields
  public setupGauss2d
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
#ifdef WITH_NUOPC
  subroutine fillExportFields(data_a2oi, lonr, latr, rootPet, rc)
    real(kind=ESMF_KIND_R8), target, intent(in) :: data_a2oi(:,:,:)
    integer, intent(in)                         :: lonr, latr, rootPet
    integer, intent(out), optional              :: rc
    
    !-----
    ! Fill updated data into the export Fields.
    !-----
    
    if (present(rc)) rc=ESMF_SUCCESS
    
    call fillFields( &
      fieldList=(/ &
        mean_zonal_moment_flx,       & !  1
        mean_merid_moment_flx,       & !  2
        mean_sensi_heat_flx,         & !  3
        mean_laten_heat_flx,         & !  4
        mean_down_lw_flx,            & !  5
        mean_down_sw_flx,            & !  6
        mean_prec_rate,              & !  7
        inst_zonal_moment_flx,       & !  8
        inst_merid_moment_flx,       & !  9
        inst_sensi_heat_flx,         & ! 10
        inst_laten_heat_flx,         & ! 11
        inst_down_lw_flx,            & ! 12
        inst_down_sw_flx,            & ! 13
        inst_temp_height2m,          & ! 14
        inst_spec_humid_height2m,    & ! 15
        inst_u_wind_height10m,       & ! 16
        inst_v_wind_height10m,       & ! 17
        inst_temp_height_surface,    & ! 18
        inst_pres_height_surface,    & ! 19
        inst_surface_height,         & ! 20
        mean_net_lw_flx,             & ! 21
        mean_net_sw_flx,             & ! 22
        inst_net_lw_flx,             & ! 23
        inst_net_sw_flx,             & ! 24
        mean_down_sw_ir_dir_flx,     & ! 25
        mean_down_sw_ir_dif_flx,     & ! 26
        mean_down_sw_vis_dir_flx,    & ! 27
        mean_down_sw_vis_dif_flx,    & ! 28
        inst_down_sw_ir_dir_flx,     & ! 29
        inst_down_sw_ir_dif_flx,     & ! 30
        inst_down_sw_vis_dir_flx,    & ! 31
        inst_down_sw_vis_dif_flx,    & ! 32
        mean_net_sw_ir_dir_flx,      & ! 33
        mean_net_sw_ir_dif_flx,      & ! 34
        mean_net_sw_vis_dir_flx,     & ! 35
        mean_net_sw_vis_dif_flx,     & ! 36
        inst_net_sw_ir_dir_flx,      & ! 37
        inst_net_sw_ir_dif_flx,      & ! 38
        inst_net_sw_vis_dir_flx,     & ! 39
        inst_net_sw_vis_dif_flx,     & ! 40
        inst_ir_dir_albedo,          & ! 41
        inst_ir_dif_albedo,          & ! 42
        inst_vis_dir_albedo,         & ! 43
        inst_vis_dif_albedo          & ! 44
      /), &
      idList=(/ &
          1, &
          2, &
          3, &
          4, &
          5, &
          6, &
          7, &
          8, &
          9, &
         10, &
         11, &
         12, &
         13, &
         14, &
         15, &
         16, &
         17, &
         18, &
         19, &
         20, &
         21, &
         22, &
         23, &
         24, &
         25, &
         26, &
         27, &
         28, &
         29, &
         30, &
         31, &
         32, &
         33, &
         34, &
         35, &
         36, &
         37, &
         38, &
         39, &
         40, &
         41, &
         42, &
         43, &
         44  &
      /), rc=rc)
    ESMF_ERR_RETURN(rc,rc)

  contains
  
    subroutine fillFields(fieldList, idList, rc)
      type(ESMF_Field)  :: fieldList(:)
      integer           :: idList(:)
      integer, optional :: rc
      
      integer           :: i
      
      if (present(rc)) rc=ESMF_SUCCESS
      
      do i=1, size(fieldList)
        if (NUOPC_IsCreated(fieldList(i))) then
          call ESMF_FieldScatter(fieldList(i), data_a2oi(:,:,idList(i)), &
            rootPet=rootPet, rc=rc)
          ESMF_ERR_RETURN(rc,rc)
        endif
      enddo
      
    end subroutine

  end subroutine
#else
  subroutine fillExportFields(data_a2oi, lonr, latr, rootPet, rc)
    real(kind=8)                                :: data_a2oi(:,:,:)
    integer, intent(in)                         :: lonr, latr, rootPet
    integer, optional                           :: rc
  end subroutine
#endif
  
  !-----------------------------------------------------------------------------

#ifdef WITH_NUOPC
  subroutine setupGauss2d(lonr, latr, pi, colrad_a, lats_node_a, &
    global_lats_a, lonsperlat, rc)
    integer, intent(in)                         :: lonr, latr 
    real(kind=ESMF_KIND_R8), intent(in)         :: pi, colrad_a(:)
    integer, intent(in)                         :: lats_node_a
    integer, intent(in), target                 :: global_lats_a(:)
    integer, intent(in), target                 :: lonsperlat(:)
    integer, intent(out), optional              :: rc
    
    !-----
    ! Create a regular (non-reduced) Gaussian Grid according to NEMS parameters.
    !-----

    integer                                     :: i, j
    real(kind=ESMF_KIND_R8), pointer            :: lonPtr(:,:), latPtr(:,:)
    type(ESMF_VM)                               :: vm
    integer                                     :: petCount
    integer, allocatable                        :: latCounts(:)

    if (present(rc)) rc=ESMF_SUCCESS
    
    call ESMF_VMGetCurrent(vm, rc=rc)
    ESMF_ERR_RETURN(rc,rc)
    
    call ESMF_VMGet(vm, petCount=petCount, rc=rc)
    ESMF_ERR_RETURN(rc,rc)
    
    allocate(latCounts(petCount))

    ! gather the latitude counts on all PETs as an array
    call ESMF_VMAllGather(vm, (/lats_node_a/), latCounts, count=1, rc=rc)
    ESMF_ERR_RETURN(rc,rc)

    ! Create a global spherical grid that is decomposed along latitude dim
    ! the same way that GSM decomposes the Grid.
    gauss2d = ESMF_GridCreate1PeriDim(minIndex=(/1,1/), &
      countsPerDEDim1=(/lonr/),&! 1 DE along "i", i.e. longitude, w/ all longit.
      countsPerDEDim2=latCounts,&! petCount DEs along "j", i.e. latitude w/ cnts
      indexflag=ESMF_INDEX_GLOBAL, coordSys=ESMF_COORDSYS_SPH_DEG, rc=rc)
    ESMF_ERR_RETURN(rc,rc)
    
    deallocate(latCounts)

    ! add coordinates    
    call ESMF_GridAddCoord(gauss2d, rc=rc)
    ESMF_ERR_RETURN(rc,rc)
    
    call ESMF_GridGetCoord(gauss2d, coordDim=1, farrayPtr=lonPtr, rc=rc)
    ESMF_ERR_RETURN(rc,rc)

    call ESMF_GridGetCoord(gauss2d, coordDim=2, farrayPtr=latPtr, rc=rc)
    ESMF_ERR_RETURN(rc,rc)
    
    ! fill coordinate arrays the same way GSM sets up a non-reduced Gaussian
    do j=lbound(lonPtr,2),ubound(lonPtr,2)
    do i=lbound(lonPtr,1),ubound(lonPtr,1)
      lonPtr(i,j) = 360./real(lonr) * (i-1)
      if (j <= latr/2) then
        latPtr(i,j) = 90. - 180./pi * colrad_a(j)
      else
        latPtr(i,j) = 180./pi * colrad_a(latr+1-j) - 90.
      endif
    enddo
    enddo
    
    ! store GSM members for easier access
    global_lats_ptr => global_lats_a
    lonsperlat_ptr => lonsperlat
    
  end subroutine
#else
  subroutine setupGauss2d(lonr, latr, pi, colrad_a, lats_node_a, &
    global_lats_a, lonsperlat, rc)
    integer, intent(in)                         :: lonr, latr 
    real(kind=8), intent(in)                    :: pi, colrad_a(:)
    integer, intent(in)                         :: lats_node_a
    integer, intent(in), target                 :: global_lats_a(:)
    integer, intent(in), target                 :: lonsperlat(:)
    integer, optional                           :: rc
  end subroutine
#endif

  !-----------------------------------------------------------------------------

end module
