 subroutine gridgen_sfc(ndom, &
                        a_domain_name, &
                        a_domain_type, &
                        a_imdl, &
                        a_jmdl, &
                        a_dx_mdl, &
                        a_dy_mdl, &
                        a_centlon_mdl, &
                        a_centlat_mdl, &
                        a_imdl_parent, &
                        a_jmdl_parent, &
                        a_dx_parent_mdl, &
                        a_dy_parent_mdl, &
                        a_centlon_parent_mdl, &
                        a_centlat_parent_mdl)

 use program_setup, only   : setup,   &
                             setup_cleanup, grib2, &
                             max_gen

 use calc_latlons,  only   : calc_latlons_mdl, &
                             calc_latlons_cleanup 
 
 use lsmask_orog,  only    : lsmask_orog_driver, &
                             lsmask_orog_cleanup

 use soil_vegtype_tile, only : soil_vegtype_driver

 use init_grib1, only        : init_pds_gds

 use mpimod, only          : myrank, mpi_cleanup

 implicit none

 include 'mpif.h'

 integer                  :: ierr

 integer                  :: ndom
 character(len=*)         :: a_domain_name
 character(len=*)         :: a_domain_type
 integer                  :: a_imdl
 integer                  :: a_jmdl
 real                     :: a_dx_mdl
 real                     :: a_dy_mdl
 real                     :: a_centlon_mdl
 real                     :: a_centlat_mdl
 integer,dimension(max_gen) :: a_imdl_parent
 integer,dimension(max_gen)                  :: a_jmdl_parent
 real,dimension(max_gen)                     :: a_dx_parent_mdl
 real,dimension(max_gen)                     :: a_dy_parent_mdl
 real,dimension(max_gen)                     :: a_centlon_parent_mdl
 real,dimension(max_gen)                     :: a_centlat_parent_mdl

 call w3tagb('GRIDGEN_SFC',2005,0136,0000,'NP2')

!-----------------------------------------------------------------------
! get user-specified options and grid information.
!-----------------------------------------------------------------------

 call setup(ndom, &
            a_domain_name, &
            a_domain_type, &
            a_imdl, &
            a_jmdl, &
            a_dx_mdl, &
            a_dy_mdl, &
            a_centlon_mdl, &
            a_centlat_mdl, &
            a_imdl_parent, &
            a_jmdl_parent, &
            a_dx_parent_mdl, &
            a_dy_parent_mdl, &
            a_centlon_parent_mdl, &
            a_centlat_parent_mdl, & 
            grib2 )

!-----------------------------------------------------------------------
! calc lat/lons on the model grid
!-----------------------------------------------------------------------

 call calc_latlons_mdl

!-----------------------------------------------------------------------
! if outputing data in grib 1, set up grid header information.
! grib2 initialization occurs in the individual routines.
!-----------------------------------------------------------------------

 if (.not. grib2) call init_pds_gds

!-----------------------------------------------------------------------
! get land sea mask and orography.
!-----------------------------------------------------------------------

 call lsmask_orog_driver

!-----------------------------------------------------------------------
! interpolate leaf area index.
!-----------------------------------------------------------------------

 call leaf_area_index

!-----------------------------------------------------------------------
! interpolate hi-res soil and vegetation types using tiling option.
!-----------------------------------------------------------------------

 call soil_vegtype_driver

!-----------------------------------------------------------------------
! interpolate greenness fraction.
!-----------------------------------------------------------------------

 call green 

!-----------------------------------------------------------------------
! interpolate maximum snow albedo.
!-----------------------------------------------------------------------

 call max_snow_albedo 

!-----------------------------------------------------------------------
! interpolate slope type.
!-----------------------------------------------------------------------
 
 call slope_type

!-----------------------------------------------------------------------
! interpolate roughness data.
!-----------------------------------------------------------------------

 call roughness

!-----------------------------------------------------------------------
! interpolate snow free albedo.
!-----------------------------------------------------------------------

 call snow_free_albedo

!-----------------------------------------------------------------------
! interpolate soil substrate temperature.
! does not take advantage of mpi yet.
!-----------------------------------------------------------------------

 call soil_substrate 

!-----------------------------------------------------------------------
! grib lat/lons
!-----------------------------------------------------------------------

 call grib_latlons

!-----------------------------------------------------------------------
! free up memory.
!-----------------------------------------------------------------------

 call lsmask_orog_cleanup
 call calc_latlons_cleanup
 call setup_cleanup
 call mpi_cleanup

 print*,"************************************"
 print*,"** NORMAL TERMINATION FOR TASK:", myrank,"**"
 print*,"************************************"

 call w3tage('GRIDGEN_SFC')

 return

 end subroutine gridgen_sfc
