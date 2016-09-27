#include "../../ESMFVersionDefine.h"

! !REVISION HISTORY:
!
!  Jan 2016      Patrick Tripp - NUOPC/GSM merge: export/importFieldsList used always

module module_CPLFIELDS

  !-----------------------------------------------------------------------------
  ! ATM Coupling Fields: export and import
  !
  !-----------------------------------------------------------------------------

#ifdef WITH_NUOPC
  use NUOPC
#endif

  use ESMF
  
  implicit none
  
  private
 
  integer, public, parameter :: NimportFields = 14
  integer, public, parameter :: NexportFields = 48
 
#ifdef WITH_NUOPC
  real(kind=ESMF_KIND_R8),parameter :: Rearth=6376000.  ! copied from atmos/share/module_CONSTANTS.F90

  ! Regular (non-reduced) Gaussian Grid ---------------
  public            :: gauss2d
  type(ESMF_Grid)   :: gauss2d
#endif 

! PT: these are needed in non NUOPC 
! #ifdef WITH_NUOPC

  ! Export Fields ----------------------------------------
  type(ESMF_Field), public   :: exportFields(NexportFields)
  character(len=40), public, parameter :: exportFieldsList(NexportFields) = (/ &
      "mean_zonal_moment_flx                  ", &
      "mean_merid_moment_flx                  ", &
      "mean_sensi_heat_flx                    ", &
      "mean_laten_heat_flx                    ", &
      "mean_down_lw_flx                       ", &
      "mean_down_sw_flx                       ", &
      "mean_prec_rate                         ", &
      "inst_zonal_moment_flx                  ", &
      "inst_merid_moment_flx                  ", &
      "inst_sensi_heat_flx                    ", &
      "inst_laten_heat_flx                    ", &
      "inst_down_lw_flx                       ", &
      "inst_down_sw_flx                       ", &
      "inst_temp_height2m                     ", &
      "inst_spec_humid_height2m               ", &
      "inst_zonal_wind_height10m              ", &
      "inst_merid_wind_height10m              ", &
      "inst_temp_height_surface               ", &
      "inst_pres_height_surface               ", &
      "inst_surface_height                    ", &
      "mean_net_lw_flx                        ", &
      "mean_net_sw_flx                        ", &
      "inst_net_lw_flx                        ", &
      "inst_net_sw_flx                        ", &
      "mean_down_sw_ir_dir_flx                ", &
      "mean_down_sw_ir_dif_flx                ", &
      "mean_down_sw_vis_dir_flx               ", &
      "mean_down_sw_vis_dif_flx               ", &
      "inst_down_sw_ir_dir_flx                ", &
      "inst_down_sw_ir_dif_flx                ", &
      "inst_down_sw_vis_dir_flx               ", &
      "inst_down_sw_vis_dif_flx               ", &
      "mean_net_sw_ir_dir_flx                 ", &
      "mean_net_sw_ir_dif_flx                 ", &
      "mean_net_sw_vis_dir_flx                ", &
      "mean_net_sw_vis_dif_flx                ", &
      "inst_net_sw_ir_dir_flx                 ", &
      "inst_net_sw_ir_dif_flx                 ", &
      "inst_net_sw_vis_dir_flx                ", &
      "inst_net_sw_vis_dif_flx                ", &
!     "inst_ir_dir_albedo                     ", &
!     "inst_ir_dif_albedo                     ", &
!     "inst_vis_dir_albedo                    ", &
!     "inst_vis_dif_albedo                    ", &
      "inst_land_sea_mask                     ", &
      "inst_temp_height_lowest                ", &
      "inst_spec_humid_height_lowest          ", &
      "inst_zonal_wind_height_lowest          ", &
      "inst_merid_wind_height_lowest          ", &
      "inst_pres_height_lowest                ", &
      "inst_height_lowest                     ", &
      "mean_fprec_rate                        "  /)
  
  ! Import Fields ----------------------------------------
  type(ESMF_Field), public   :: importFields(NimportFields)
  character(len=40), public, parameter :: importFieldsList(NimportFields) = (/ &
      "land_mask                              ", &
      "surface_temperature                    ", &
      "sea_surface_temperature                ", &
      "ice_fraction                           ", &
      "inst_ice_ir_dif_albedo                 ", &
      "inst_ice_ir_dir_albedo                 ", &
      "inst_ice_vis_dif_albedo                ", &
      "inst_ice_vis_dir_albedo                ", &
      "mean_up_lw_flx                         ", &
      "mean_laten_heat_flx                    ", &
      "mean_sensi_heat_flx                    ", &
      "mean_evap_rate                         ", &
      "mean_zonal_moment_flx                  ", &
      "mean_merid_moment_flx                  "  /)
  
  ! Utility GSM members ----------------------------------
  public            :: global_lats_ptr
  integer, pointer  :: global_lats_ptr(:)
  public            :: lonsperlat_ptr
  integer, pointer  :: lonsperlat_ptr(:)

  ! Methods
  public fillExportFields
  public queryFieldList
  public setupGauss2d
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
#ifdef WITH_NUOPC
  subroutine fillExportFields(data_a2oi, lonr, latr, rootPet, rc)
    real(kind=ESMF_KIND_R8), target, intent(in) :: data_a2oi(:,:,:)
    integer, intent(in)                         :: lonr, latr, rootPet
    integer, intent(out), optional              :: rc
    
    integer           :: n
    !-----
    ! Fill updated data into the export Fields.
    !-----
    
    if (present(rc)) rc=ESMF_SUCCESS
    
    do n=1, size(exportFields)
      if (ESMF_FieldIsCreated(exportFields(n))) then
        call ESMF_FieldScatter(exportFields(n), data_a2oi(:,:,n), &
          rootPet=rootPet, rc=rc)
        ESMF_ERR_RETURN(rc,rc)
      endif
    enddo

    ESMF_ERR_RETURN(rc,rc)

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
    real(kind=ESMF_KIND_R8), pointer            :: lonCorPtr(:,:), latCorPtr(:,:)
    real(kind=ESMF_KIND_R8), pointer            :: areaPtr(:,:)
    integer(kind=ESMF_KIND_I4), pointer         :: maskPtr(:,:)
    real(kind=ESMF_KIND_R8)                     :: latCorjp1
    character(len=256)                          :: tmpstr
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
    
    ! add coordinates    
    call ESMF_GridAddCoord(gauss2d, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    ESMF_ERR_RETURN(rc,rc)

    call ESMF_GridAddCoord(gauss2d, staggerLoc=ESMF_STAGGERLOC_CORNER, rc=rc)
    ESMF_ERR_RETURN(rc,rc)
    
    call ESMF_GridAddItem(gauss2d, itemFlag=ESMF_GRIDITEM_MASK, itemTypeKind=ESMF_TYPEKIND_I4, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    ESMF_ERR_RETURN(rc,rc)

    call ESMF_GridAddItem(gauss2d, itemFlag=ESMF_GRIDITEM_AREA, itemTypeKind=ESMF_TYPEKIND_R8, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    ESMF_ERR_RETURN(rc,rc)

    !--- CORNERS ---

    call ESMF_GridGetCoord(gauss2d, coordDim=1, staggerloc=ESMF_STAGGERLOC_CORNER, farrayPtr=lonCorPtr, rc=rc)
    ESMF_ERR_RETURN(rc,rc)

    call ESMF_GridGetCoord(gauss2d, coordDim=2, staggerloc=ESMF_STAGGERLOC_CORNER, farrayPtr=latCorPtr, rc=rc)
    ESMF_ERR_RETURN(rc,rc)
    
    write(tmpstr,'(a,4i8)') 'gsm gauss2d corner ',lbound(lonCorPtr,1),ubound(lonCorPtr,1),lbound(lonCorPtr,2),ubound(lonCorPtr,2)
    call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, line=__LINE__, file=__FILE__, rc=rc)

    ! fill coordinate arrays the same way GSM sets up a non-reduced Gaussian
    do j=lbound(lonCorPtr,2),ubound(lonCorPtr,2)
    do i=lbound(lonCorPtr,1),ubound(lonCorPtr,1)
      lonCorPtr(i,j) = 360./real(lonr) * (real(i)-1.5)
      if (j == 1) then
        latCorPtr(i,j) = 90.
      elseif (j == latr+1) then
        latCorPtr(i,j) = -90.
      elseif (j == latr/2+1) then
        latCorPtr(i,j) = 0.
      elseif (j < latr/2+1) then
        latCorPtr(i,j) = 90. - 180./pi * 0.5*(colrad_a(j)+colrad_a(j-1))
      else
        latCorPtr(i,j) = 180./pi * 0.5*(colrad_a(latr+1-j)+colrad_a(latr+1-j+1)) - 90.
      endif
    enddo
    enddo

    !--- CENTERS ---

    call ESMF_GridGetCoord(gauss2d, coordDim=1, farrayPtr=lonPtr, rc=rc)
    ESMF_ERR_RETURN(rc,rc)

    call ESMF_GridGetCoord(gauss2d, coordDim=2, farrayPtr=latPtr, rc=rc)
    ESMF_ERR_RETURN(rc,rc)
    
    call ESMF_GridGetItem(gauss2d, itemflag=ESMF_GRIDITEM_MASK, farrayPtr=maskPtr, rc=rc)
    ESMF_ERR_RETURN(rc,rc)

    call ESMF_GridGetItem(gauss2d, itemflag=ESMF_GRIDITEM_AREA, farrayPtr=areaPtr, rc=rc)
    ESMF_ERR_RETURN(rc,rc)

    write(tmpstr,'(a,4i8)') 'gsm gauss2d center ',lbound(lonPtr,1),ubound(lonPtr,1),lbound(lonPtr,2),ubound(lonPtr,2)
    call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, line=__LINE__, file=__FILE__, rc=rc)

    ! fill coordinate arrays the same way GSM sets up a non-reduced Gaussian
    ! tcraig, this is not correct, but is a starting point.
    do j=lbound(lonPtr,2),ubound(lonPtr,2)
      if (j+1 == 1) then
        latCorjp1 = 90.
      elseif (j+1 == latr+1) then
        latCorjp1 = -90.
      elseif (j+1 == latr/2+1) then
        latCorjp1 = 0.
      elseif (j+1 < latr/2+1) then
        latCorjp1 = 90. - 180./pi * 0.5*(colrad_a(j)+colrad_a(j+1))
      else
        latCorjp1 = 180./pi * 0.5*(colrad_a(latr+1-j)+colrad_a(latr+1-j-1)) - 90.
      endif
    do i=lbound(lonPtr,1),ubound(lonPtr,1)
      lonPtr(i,j) = 360./real(lonr) * (i-1)
      if (j <= latr/2) then
        latPtr(i,j) = 90. - 180./pi * colrad_a(j)
      else
        latPtr(i,j) = 180./pi * colrad_a(latr+1-j) - 90.
      endif
      maskPtr(i,j) = 1
!      areaPtr(i,j) = abs(2.*pi/real(lonr) * cos(latPtr(i,j)*pi/180.) * pi/real(latr) * Rearth * Rearth)
      areaPtr(i,j) = abs(2.*pi/real(lonr) * cos(latPtr(i,j)*pi/180.) * pi/180.*(latCorjp1-latCorPtr(i,j)) * Rearth * Rearth)
    enddo
    enddo
    
    ! store GSM members for easier access
    global_lats_ptr => global_lats_a
    lonsperlat_ptr => lonsperlat
    
    deallocate(latCounts)

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

  integer function queryFieldList(fieldlist, fieldname, abortflag, rc)
    ! returns integer index of first found fieldname in fieldlist
    ! by default, will abort if field not found, set abortflag to false 
    !   to turn off the abort.
    ! return value of < 1 means the field was not found
    character(len=*),intent(in) :: fieldlist(:)
    character(len=*),intent(in) :: fieldname
    logical, optional :: abortflag
    integer, optional :: rc

    integer :: n
    logical :: labort

    labort = .true.
    if (present(abortflag)) then
      labort = abortflag
    endif

    queryFieldList = 0
    n = 1
    do while (queryFieldList < 1 .and. n <= size(fieldlist))  
      if (trim(fieldlist(n)) == trim(fieldname)) then
        queryFieldList = n
      else
        n = n + 1
      endif
    enddo

    if (labort .and. queryFieldList < 1) then
! #ifdef WITH_NUOPC
     call ESMF_LogWrite('queryFieldList ABORT on fieldname '//trim(fieldname), ESMF_LOGMSG_INFO, line=__LINE__, file=__FILE__, rc=rc)
      CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
! #endif
    endif
  end function queryFieldList
  !-----------------------------------------------------------------------------

end module
