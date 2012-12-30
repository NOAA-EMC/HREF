MODULE map_utils

! Module that defines constants, data structures, and
! subroutines used to convert grid indices to lat/lon
! and vice versa.   
!
! SUPPORTED PROJECTIONS
! ---------------------
! Cylindrical Lat/Lon (code = PROJ_LATLON)
! Mercator (code = PROJ_MERC)
! Lambert Conformal (code = PROJ_LC)
! Gaussian (code = PROJ_GAUSS)
! Polar Stereographic (code = PROJ_PS)
! Rotated Lat/Lon (code = PROJ_ROTLL)
!
! REMARKS
! -------
! The routines contained within were adapted from routines
! obtained from NCEP's w3 library.  The original NCEP routines were less
! flexible (e.g., polar-stereo routines only supported truelat of 60N/60S)
! than what we needed, so modifications based on equations in Hoke, Hayes, and
! Renninger (AFGWC/TN/79-003) were added to improve the flexibility.  
! Additionally, coding was improved to F90 standards and the routines were
! combined into this module.  
!
! ASSUMPTIONS
! -----------
!  Grid Definition:
!    For mercator, lambert conformal, and polar-stereographic projections,
!    the routines within assume the following:
!
!       1.  Grid is dimensioned (i,j) where i is the East-West direction,
!           positive toward the east, and j is the north-south direction,
!           positive toward the north.
!       2.  Origin is at (1,1) and is located at the southwest corner,
!           regardless of hemispere.
!       3.  Grid spacing (dx) is always positive.
!       4.  Values of true latitudes must be positive for NH domains
!           and negative for SH domains.
!
!     For the latlon and Gaussian projection, the grid origin may be at any
!     of the corners, and the deltalat and deltalon values can be signed to
!     account for this using the following convention:
!       Origin Location        Deltalat Sign      Deltalon Sign
!       ---------------        -------------      -------------
!        SW Corner                  +                   +
!        NE Corner                  -                   -
!        NW Corner                  -                   +
!        SE Corner                  +                   -
!
!  Data Definitions:
!       1. Any arguments that are a latitude value are expressed in
!          degrees north with a valid range of -90 -> 90
!       2. Any arguments that are a longitude value are expressed in
!          degrees east with a valid range of -180 -> 180.
!       3. Distances are in meters and are always positive.
!       4. The standard longitude (stdlon) is defined as the longitude
!          line which is parallel to the grid's y-axis (j-direction), along
!          which latitude increases (NOT the absolute value of latitude, but
!          the actual latitude, such that latitude increases continuously
!          from the south pole to the north pole) as j increases.
!       5. One true latitude value is required for polar-stereographic and
!          mercator projections, and defines at which latitude the
!          grid spacing is true.  For lambert conformal, two true latitude
!          values must be specified, but may be set equal to each other to
!          specify a tangent projection instead of a secant projection.
!
! USAGE
! -----
! To use the routines in this module, the calling routines must have the
! following statement at the beginning of its declaration block:
!   USE map_utils
!
! The use of the module not only provides access to the necessary routines,
! but also defines a structure of TYPE (proj_info) that can be used
! to declare a variable of the same type to hold your map projection
! information.  It also defines some integer parameters that contain
! the projection codes so one only has to use those variable names rather
! than remembering the acutal code when using them.  The basic steps are
! as follows:
!
!   1.  Ensure the "USE map_utils" is in your declarations.
!   2.  Declare the projection information structure as type(proj_info):
!         TYPE(proj_info) :: proj
!   3.  Populate your structure by calling the map_set routine:
!         CALL map_set(code,lat1,lon1,knowni,knownj,dx,stdlon,truelat1,truelat2,proj)
!       where:
!         code (input) = one of PROJ_LATLON, PROJ_MERC, PROJ_LC, PROJ_PS,
!                        PROJ_GAUSS, or PROJ_ROTLL
!         lat1 (input) = Latitude of grid origin point (i,j)=(1,1)
!                         (see assumptions!)
!         lon1 (input) = Longitude of grid origin
!         knowni (input) = origin point, x-location
!         knownj (input) = origin point, y-location
!         dx (input) = grid spacing in meters (ignored for LATLON projections)
!         stdlon (input) = Standard longitude for PROJ_PS and PROJ_LC,
!               deltalon (see assumptions) for PROJ_LATLON,
!               ignored for PROJ_MERC
!         truelat1 (input) = 1st true latitude for PROJ_PS, PROJ_LC, and
!                PROJ_MERC, deltalat (see assumptions) for PROJ_LATLON
!         truelat2 (input) = 2nd true latitude for PROJ_LC,
!                ignored for all others.
!         proj (output) = The structure of type (proj_info) that will be fully
!                populated after this call
!
!   4.  Now that the proj structure is populated, you may call either
!       of the following routines:
!
!       latlon_to_ij(proj, lat, lon, i, j)
!       ij_to_latlon(proj, i, j, lat, lon)
!
!       It is incumbent upon the calling routine to determine whether or
!       not the values returned are within your domain's bounds.  All values
!       of i, j, lat, and lon are REAL values.
!
!
! REFERENCES
! ----------
!  Hoke, Hayes, and Renninger, "Map Preojections and Grid Systems for
!       Meteorological Applications." AFGWC/TN-79/003(Rev), Air Weather
!       Service, 1985.
!
!  NCAR MM5v3 Modeling System, REGRIDDER program, module_first_guess_map.F
!  NCEP routines w3fb06, w3fb07, w3fb08, w3fb09, w3fb11, w3fb12
!
! HISTORY
! -------
! 27 Mar 2001 - Original Version
!               Brent L. Shaw, NOAA/FSL (CSU/CIRA)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   use constants_module
   use misc_definitions_module
   use module_debug
 
   ! Define some private constants
   INTEGER, PRIVATE, PARAMETER :: HIGH = 8
 
   TYPE proj_info
 
      INTEGER          :: code     ! Integer code for projection TYPE
      INTEGER          :: nlat     ! For Gaussian -- number of latitude points 
                                   !  north of the equator 
      INTEGER          :: ixdim    ! For Rotated Lat/Lon -- number of mass points
                                   !  in an odd row
      INTEGER          :: jydim    ! For Rotated Lat/Lon -- number of rows
      INTEGER          :: stagger  ! For Rotated Lat/Lon -- mass or velocity grid 
      REAL             :: phi      ! For Rotated Lat/Lon -- domain half-extent in 
                                   !  degrees latitude
      REAL             :: lambda   ! For Rotated Lat/Lon -- domain half-extend in
                                   !  degrees longitude
      REAL             :: lat1     ! SW latitude (1,1) in degrees (-90->90N)
      REAL             :: lon1     ! SW longitude (1,1) in degrees (-180->180E)
      REAL             :: dx       ! Grid spacing in meters at truelats, used
                                   !  only for ps, lc, and merc projections
      REAL             :: latinc   ! Latitude increment for cylindrical lat/lon
      REAL             :: loninc   ! Longitude increment for cylindrical lat/lon
                                   !  also the lon increment for Gaussian grid
      REAL             :: dlat     ! Lat increment for lat/lon grids
      REAL             :: dlon     ! Lon increment for lat/lon grids
      REAL             :: stdlon   ! Longitude parallel to y-axis (-180->180E)
      REAL             :: truelat1 ! First true latitude (all projections)
      REAL             :: truelat2 ! Second true lat (LC only)
      REAL             :: hemi     ! 1 for NH, -1 for SH
      REAL             :: cone     ! Cone factor for LC projections
      REAL             :: polei    ! Computed i-location of pole point
      REAL             :: polej    ! Computed j-location of pole point
      REAL             :: rsw      ! Computed radius to SW corner
      REAL             :: rebydx   ! Earth radius divided by dx
      REAL             :: knowni   ! X-location of known lat/lon
      REAL             :: knownj   ! Y-location of known lat/lon
      REAL             :: re_m     ! Radius of spherical earth, meters
      LOGICAL          :: init     ! Flag to indicate if this struct is 
                                   !  ready for use
      LOGICAL          :: wrap     ! For Gaussian -- flag to indicate wrapping 
                                   !  around globe?
      LOGICAL          :: comp_ll  ! Work in computational lat/lon space for
      REAL, POINTER, DIMENSION(:) :: gauss_lat  ! Latitude array for Gaussian grid
 
   END TYPE proj_info
 
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 CONTAINS
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
   SUBROUTINE map_init(proj)
      ! Initializes the map projection structure to missing values
  
      IMPLICIT NONE
      TYPE(proj_info), INTENT(INOUT)  :: proj
  
      proj%lat1     = -999.9
      proj%lon1     = -999.9
      proj%dx       = -999.9
      proj%latinc   = -999.9
      proj%loninc   = -999.9
      proj%stdlon   = -999.9
      proj%truelat1 = -999.9
      proj%truelat2 = -999.9
      proj%phi      = -999.9
      proj%lambda   = -999.9
      proj%ixdim    = -999.9
      proj%jydim    = -999.9
      proj%stagger  = HH
      proj%nlat     = 0
      proj%hemi     = 0.0
      proj%cone     = -999.9
      proj%polei    = -999.9
      proj%polej    = -999.9
      proj%rsw      = -999.9
      proj%knowni   = -999.9
      proj%knownj   = -999.9
      proj%re_m     = EARTH_RADIUS_M
      proj%init     = .FALSE.
      proj%wrap     = .FALSE.
      proj%comp_ll  = .false.
      nullify(proj%gauss_lat)
   
   END SUBROUTINE map_init


   SUBROUTINE map_set(proj_code, proj, lat1, lon1, knowni, knownj, dx, latinc, &
                      loninc, stdlon, truelat1, truelat2, nlat, ixdim, jydim, &
                      stagger, phi, lambda, r_earth)
      ! Given a partially filled proj_info structure, this routine computes
      ! polei, polej, rsw, and cone (if LC projection) to complete the 
      ! structure.  This allows us to eliminate redundant calculations when
      ! calling the coordinate conversion routines multiple times for the
      ! same map.
      ! This will generally be the first routine called when a user wants
      ! to be able to use the coordinate conversion routines, and it
      ! will call the appropriate subroutines based on the 
      ! proj%code which indicates which projection type this is.
  
      IMPLICIT NONE
      
      ! Declare arguments
      INTEGER, INTENT(IN)               :: proj_code
      INTEGER, INTENT(IN), OPTIONAL     :: nlat
      INTEGER, INTENT(IN), OPTIONAL     :: ixdim
      INTEGER, INTENT(IN), OPTIONAL     :: jydim
      INTEGER, INTENT(IN), OPTIONAL     :: stagger
      REAL, INTENT(IN), OPTIONAL        :: latinc
      REAL, INTENT(IN), OPTIONAL        :: loninc
      REAL, INTENT(IN), OPTIONAL        :: lat1
      REAL, INTENT(IN), OPTIONAL        :: lon1
      REAL, INTENT(IN), OPTIONAL        :: dx
      REAL, INTENT(IN), OPTIONAL        :: stdlon
      REAL, INTENT(IN), OPTIONAL        :: truelat1
      REAL, INTENT(IN), OPTIONAL        :: truelat2
      REAL, INTENT(IN), OPTIONAL        :: knowni
      REAL, INTENT(IN), OPTIONAL        :: knownj
      REAL, INTENT(IN), OPTIONAL        :: phi
      REAL, INTENT(IN), OPTIONAL        :: lambda
      REAL, INTENT(IN), OPTIONAL        :: r_earth
      TYPE(proj_info), INTENT(OUT)      :: proj

      INTEGER :: iter
      REAL :: dummy_lon1
      REAL :: dummy_stdlon
  
      ! First, verify that mandatory parameters are present for the specified proj_code
      IF ( proj_code == PROJ_LC ) THEN
         IF ( .NOT.PRESENT(truelat1) .OR. &
              .NOT.PRESENT(truelat2) .OR. &
              .NOT.PRESENT(lat1) .OR. &
              .NOT.PRESENT(lon1) .OR. &
              .NOT.PRESENT(knowni) .OR. &
              .NOT.PRESENT(knownj) .OR. &
              .NOT.PRESENT(stdlon) .OR. &
              .NOT.PRESENT(dx) ) THEN
            PRINT '(A,I2)', 'The following are mandatory parameters for projection code : ', proj_code
            PRINT '(A)', ' truelat1, truelat2, lat1, lon1, knowni, knownj, stdlon, dx'
            call mprintf(.true.,ERROR,'MAP_INIT')
         END IF
      ELSE IF ( proj_code == PROJ_PS ) THEN
         IF ( .NOT.PRESENT(truelat1) .OR. &
              .NOT.PRESENT(lat1) .OR. &
              .NOT.PRESENT(lon1) .OR. &
              .NOT.PRESENT(knowni) .OR. &
              .NOT.PRESENT(knownj) .OR. &
              .NOT.PRESENT(stdlon) .OR. &
              .NOT.PRESENT(dx) ) THEN
            PRINT '(A,I2)', 'The following are mandatory parameters for projection code : ', proj_code
            PRINT '(A)', ' truelat1, lat1, lon1, knonwi, knownj, stdlon, dx'
            call mprintf(.true.,ERROR,'MAP_INIT')
         END IF
      ELSE IF ( proj_code == PROJ_PS_WGS84 ) THEN
         IF ( .NOT.PRESENT(truelat1) .OR. &
              .NOT.PRESENT(lat1) .OR. &
              .NOT.PRESENT(lon1) .OR. &
              .NOT.PRESENT(knowni) .OR. &
              .NOT.PRESENT(knownj) .OR. &
              .NOT.PRESENT(stdlon) .OR. &
              .NOT.PRESENT(dx) ) THEN
            PRINT '(A,I2)', 'The following are mandatory parameters for projection code : ', proj_code
            PRINT '(A)', ' truelat1, lat1, lon1, knonwi, knownj, stdlon, dx'
            call mprintf(.true.,ERROR,'MAP_INIT')
         END IF
      ELSE IF ( proj_code == PROJ_MERC ) THEN
         IF ( .NOT.PRESENT(truelat1) .OR. &
              .NOT.PRESENT(lat1) .OR. &
              .NOT.PRESENT(lon1) .OR. &
              .NOT.PRESENT(knowni) .OR. &
              .NOT.PRESENT(knownj) .OR. &
              .NOT.PRESENT(dx) ) THEN
            PRINT '(A,I2)', 'The following are mandatory parameters for projection code : ', proj_code
            PRINT '(A)', ' truelat1, lat1, lon1, knowni, knownj, dx'
            call mprintf(.true.,ERROR,'MAP_INIT')
         END IF
      ELSE IF ( proj_code == PROJ_LATLON ) THEN
         IF ( .NOT.PRESENT(latinc) .OR. &
              .NOT.PRESENT(loninc) .OR. &
              .NOT.PRESENT(knowni) .OR. &
              .NOT.PRESENT(knownj) .OR. &
              .NOT.PRESENT(lat1) .OR. &
              .NOT.PRESENT(lon1) ) THEN
            PRINT '(A,I2)', 'The following are mandatory parameters for projection code : ', proj_code
            PRINT '(A)', ' latinc, loninc, knowni, knownj, lat1, lon1'
            call mprintf(.true.,ERROR,'MAP_INIT')
         END IF
      ELSE IF ( proj_code == PROJ_GAUSS ) THEN
         IF ( .NOT.PRESENT(nlat) .OR. &
              .NOT.PRESENT(lat1) .OR. &
              .NOT.PRESENT(lon1) .OR. &
              .NOT.PRESENT(loninc) ) THEN
            PRINT '(A,I2)', 'The following are mandatory parameters for projection code : ', proj_code
            PRINT '(A)', ' nlat, lat1, lon1, loninc'
            call mprintf(.true.,ERROR,'MAP_INIT')
         END IF
      ELSE IF ( proj_code == PROJ_ROTLL ) THEN
         IF ( .NOT.PRESENT(ixdim) .OR. &
              .NOT.PRESENT(jydim) .OR. &
              .NOT.PRESENT(phi) .OR. &
              .NOT.PRESENT(lambda) .OR. &
              .NOT.PRESENT(lat1) .OR. &
              .NOT.PRESENT(lon1) .OR. &
              .NOT.PRESENT(stagger) ) THEN
            PRINT '(A,I2)', 'The following are mandatory parameters for projection code : ', proj_code
            PRINT '(A)', ' ixdim, jydim, phi, lambda, lat1, lon1, stagger'
            call mprintf(.true.,ERROR,'MAP_INIT')
         END IF
      ELSE
         PRINT '(A,I2)', 'Unknown projection code: ', proj%code
         call mprintf(.true.,ERROR,'MAP_INIT')
      END IF
  
      ! Check for validity of mandatory variables in proj
      IF ( PRESENT(lat1) ) THEN
         IF ( ABS(lat1) .GT. 90. ) THEN
            PRINT '(A)', 'Latitude of origin corner required as follows:'
            PRINT '(A)', '    -90N <= lat1 < = 90.N'
            call mprintf(.true.,ERROR,'MAP_INIT')
         ENDIF
      ENDIF
  
      IF ( PRESENT(lon1) ) THEN
         dummy_lon1 = lon1
         IF ( ABS(dummy_lon1) .GT. 180.) THEN
            iter = 0 
            DO WHILE (ABS(dummy_lon1) > 180. .AND. iter < 10)
               IF (dummy_lon1 < -180.) dummy_lon1 = dummy_lon1 + 360.
               IF (dummy_lon1 > 180.) dummy_lon1 = dummy_lon1 - 360.
               iter = iter + 1
            END DO
            IF (abs(dummy_lon1) > 180.) THEN
               PRINT '(A)', 'Longitude of origin required as follows:'
               PRINT '(A)', '   -180E <= lon1 <= 180W'
               call mprintf(.true.,ERROR,'MAP_INIT')
            ENDIF
         ENDIF
      ENDIF
  
      IF ( PRESENT(dx) ) THEN
         IF ((dx .LE. 0.).AND.(proj_code .NE. PROJ_LATLON)) THEN
            PRINT '(A)', 'Require grid spacing (dx) in meters be positive!'
            call mprintf(.true.,ERROR,'MAP_INIT')
         ENDIF
      ENDIF
  
      IF ( PRESENT(stdlon) ) THEN
         dummy_stdlon = stdlon
         IF ((ABS(dummy_stdlon) > 180.).AND.(proj_code /= PROJ_MERC)) THEN
            iter = 0 
            DO WHILE (ABS(dummy_stdlon) > 180. .AND. iter < 10)
               IF (dummy_stdlon < -180.) dummy_stdlon = dummy_stdlon + 360.
               IF (dummy_stdlon > 180.) dummy_stdlon = dummy_stdlon - 360.
               iter = iter + 1
            END DO
            IF (abs(dummy_stdlon) > 180.) THEN
               PRINT '(A)', 'Need orientation longitude (stdlon) as: '
               PRINT '(A)', '   -180E <= stdlon <= 180W' 
               call mprintf(.true.,ERROR,'MAP_INIT')
            ENDIF
         ENDIF
      ENDIF
  
      IF ( PRESENT(truelat1) ) THEN
         IF (ABS(truelat1).GT.90.) THEN
            PRINT '(A)', 'Set true latitude 1 for all projections!'
            call mprintf(.true.,ERROR,'MAP_INIT')
         ENDIF
      ENDIF
     
      CALL map_init(proj) 
      proj%code  = proj_code
      IF ( PRESENT(lat1) )     proj%lat1     = lat1
      IF ( PRESENT(lon1) )     proj%lon1     = dummy_lon1
      IF ( PRESENT(latinc) )   proj%latinc   = latinc
      IF ( PRESENT(loninc) )   proj%loninc   = loninc
      IF ( PRESENT(knowni) )   proj%knowni   = knowni
      IF ( PRESENT(knownj) )   proj%knownj   = knownj
      IF ( PRESENT(dx) )       proj%dx       = dx
      IF ( PRESENT(stdlon) )   proj%stdlon   = dummy_stdlon
      IF ( PRESENT(truelat1) ) proj%truelat1 = truelat1
      IF ( PRESENT(truelat2) ) proj%truelat2 = truelat2
      IF ( PRESENT(nlat) )     proj%nlat     = nlat
      IF ( PRESENT(ixdim) )    proj%ixdim    = ixdim
      IF ( PRESENT(jydim) )    proj%jydim    = jydim
      IF ( PRESENT(stagger) )  proj%stagger  = stagger
      IF ( PRESENT(phi) )      proj%phi      = phi
      IF ( PRESENT(lambda) )   proj%lambda   = lambda
      IF ( PRESENT(r_earth) )  proj%re_m     = r_earth
  
      IF ( PRESENT(dx) ) THEN 
         IF ( (proj_code == PROJ_LC) .OR. (proj_code == PROJ_PS) .OR. &
              (proj_code == PROJ_PS_WGS84) .OR. (proj_code == PROJ_MERC) ) THEN
            proj%dx = dx
            IF (truelat1 .LT. 0.) THEN
               proj%hemi = -1.0 
            ELSE
               proj%hemi = 1.0
            ENDIF
            proj%rebydx = proj%re_m / dx
         ENDIF
      ENDIF

      pick_proj: SELECT CASE(proj%code)
  
         CASE(PROJ_PS)
            CALL set_ps(proj)

         CASE(PROJ_PS_WGS84)
            CALL set_ps_wgs84(proj)
   
         CASE(PROJ_LC)
            IF (ABS(proj%truelat2) .GT. 90.) THEN
!               PRINT '(A)', 'Second true latitude not set, assuming a tangent'
!               PRINT '(A,F10.3)', 'projection at truelat1: ', proj%truelat1
               proj%truelat2=proj%truelat1
            ENDIF
            CALL set_lc(proj)
      
         CASE (PROJ_MERC)
            CALL set_merc(proj)
      
         CASE (PROJ_LATLON)
   
         CASE (PROJ_GAUSS)
            CALL set_gauss(proj)
      
         CASE (PROJ_ROTLL)
     
      END SELECT pick_proj
      proj%init = .TRUE.

      RETURN

   END SUBROUTINE map_set


   SUBROUTINE latlon_to_ij(proj, lat, lon, i, j)
      ! Converts input lat/lon values to the cartesian (i,j) value
      ! for the given projection. 
  
      IMPLICIT NONE
      TYPE(proj_info), INTENT(IN)          :: proj
      REAL, INTENT(IN)                     :: lat
      REAL, INTENT(IN)                     :: lon
      REAL, INTENT(OUT)                    :: i
      REAL, INTENT(OUT)                    :: j
  
      IF (.NOT.proj%init) THEN
         PRINT '(A)', 'You have not called map_set for this projection!'
         call mprintf(.true.,ERROR,'LATLON_TO_IJ')
      ENDIF
  
      SELECT CASE(proj%code)
   
         CASE(PROJ_LATLON)
            CALL llij_latlon(lat,lon,proj,i,j)
   
         CASE(PROJ_MERC)
            CALL llij_merc(lat,lon,proj,i,j)
   
         CASE(PROJ_PS)
            CALL llij_ps(lat,lon,proj,i,j)

         CASE(PROJ_PS_WGS84)
            CALL llij_ps_wgs84(lat,lon,proj,i,j)
         
         CASE(PROJ_LC)
            CALL llij_lc(lat,lon,proj,i,j)
   
         CASE(PROJ_GAUSS)
            CALL llij_gauss(lat,lon,proj,i,j)
   
         CASE(PROJ_ROTLL)
            CALL llij_rotlatlon(lat,lon,proj,i,j)
	if (abs(lon) .gt. 49.0 .and. abs(lon) .lt. 50.0) then
	write(0,*) 'lat,lon,i,j: ', lat,lon,i,j
	endif
   
         CASE DEFAULT
            PRINT '(A,I2)', 'Unrecognized map projection code: ', proj%code
            call mprintf(.true.,ERROR,'LATLON_TO_IJ')
    
      END SELECT

      RETURN

   END SUBROUTINE latlon_to_ij


   SUBROUTINE ij_to_latlon(proj, i, j, lat, lon)
      ! Computes geographical latitude and longitude for a given (i,j) point
      ! in a grid with a projection of proj
  
      IMPLICIT NONE
      TYPE(proj_info),INTENT(IN)          :: proj
      REAL, INTENT(IN)                    :: i
      REAL, INTENT(IN)                    :: j
      REAL, INTENT(OUT)                   :: lat
      REAL, INTENT(OUT)                   :: lon
  
      IF (.NOT.proj%init) THEN
         PRINT '(A)', 'You have not called map_set for this projection!'
         call mprintf(.true.,ERROR,'IJ_TO_LATLON')
      ENDIF
      SELECT CASE (proj%code)
  
         CASE (PROJ_LATLON)
            CALL ijll_latlon(i, j, proj, lat, lon)
   
         CASE (PROJ_MERC)
            CALL ijll_merc(i, j, proj, lat, lon)
   
         CASE (PROJ_PS)
            CALL ijll_ps(i, j, proj, lat, lon)

         CASE (PROJ_PS_WGS84)
            CALL ijll_ps_wgs84(i, j, proj, lat, lon)
   
         CASE (PROJ_LC)
            CALL ijll_lc(i, j, proj, lat, lon)
   
         CASE (PROJ_ROTLL)
            CALL ijll_rotlatlon(i, j, proj, lat, lon)
   
         CASE DEFAULT
            PRINT '(A,I2)', 'Unrecognized map projection code: ', proj%code
            call mprintf(.true.,ERROR,'IJ_TO_LATLON')
  
      END SELECT
      RETURN
   END SUBROUTINE ij_to_latlon


   SUBROUTINE set_ps(proj)
      ! Initializes a polar-stereographic map projection from the partially
      ! filled proj structure. This routine computes the radius to the
      ! southwest corner and computes the i/j location of the pole for use
      ! in llij_ps and ijll_ps.
      IMPLICIT NONE
   
      ! Declare args
      TYPE(proj_info), INTENT(INOUT)    :: proj
  
      ! Local vars
      REAL                              :: ala1
      REAL                              :: alo1
      REAL                              :: reflon
      REAL                              :: scale_top
  
      ! Executable code
      reflon = proj%stdlon + 90.
  
      ! Compute numerator term of map scale factor
      scale_top = 1. + proj%hemi * SIN(proj%truelat1 * rad_per_deg)
  
      ! Compute radius to lower-left (SW) corner
      ala1 = proj%lat1 * rad_per_deg
      proj%rsw = proj%rebydx*COS(ala1)*scale_top/(1.+proj%hemi*SIN(ala1))
  
      ! Find the pole point
      alo1 = (proj%lon1 - reflon) * rad_per_deg
      proj%polei = proj%knowni - proj%rsw * COS(alo1)
      proj%polej = proj%knownj - proj%hemi * proj%rsw * SIN(alo1)

      RETURN

   END SUBROUTINE set_ps


   SUBROUTINE llij_ps(lat,lon,proj,i,j)
      ! Given latitude (-90 to 90), longitude (-180 to 180), and the
      ! standard polar-stereographic projection information via the 
      ! public proj structure, this routine returns the i/j indices which
      ! if within the domain range from 1->nx and 1->ny, respectively.
  
      IMPLICIT NONE
  
      ! Delcare input arguments
      REAL, INTENT(IN)               :: lat
      REAL, INTENT(IN)               :: lon
      TYPE(proj_info),INTENT(IN)     :: proj
  
      ! Declare output arguments     
      REAL, INTENT(OUT)              :: i !(x-index)
      REAL, INTENT(OUT)              :: j !(y-index)
  
      ! Declare local variables
      
      REAL                           :: reflon
      REAL                           :: scale_top
      REAL                           :: ala
      REAL                           :: alo
      REAL                           :: rm
  
      ! BEGIN CODE
    
      reflon = proj%stdlon + 90.
     
      ! Compute numerator term of map scale factor
  
      scale_top = 1. + proj%hemi * SIN(proj%truelat1 * rad_per_deg)
  
      ! Find radius to desired point
      ala = lat * rad_per_deg
      rm = proj%rebydx * COS(ala) * scale_top/(1. + proj%hemi *SIN(ala))
      alo = (lon - reflon) * rad_per_deg
      i = proj%polei + rm * COS(alo)
      j = proj%polej + proj%hemi * rm * SIN(alo)
   
      RETURN

   END SUBROUTINE llij_ps


   SUBROUTINE ijll_ps(i, j, proj, lat, lon)
 
      ! This is the inverse subroutine of llij_ps.  It returns the 
      ! latitude and longitude of an i/j point given the projection info 
      ! structure.  
  
      IMPLICIT NONE
  
      ! Declare input arguments
      REAL, INTENT(IN)                    :: i    ! Column
      REAL, INTENT(IN)                    :: j    ! Row
      TYPE (proj_info), INTENT(IN)        :: proj
      
      ! Declare output arguments
      REAL, INTENT(OUT)                   :: lat     ! -90 -> 90 north
      REAL, INTENT(OUT)                   :: lon     ! -180 -> 180 East
  
      ! Local variables
      REAL                                :: reflon
      REAL                                :: scale_top
      REAL                                :: xx,yy
      REAL                                :: gi2, r2
      REAL                                :: arccos
  
      ! Begin Code
  
      ! Compute the reference longitude by rotating 90 degrees to the east
      ! to find the longitude line parallel to the positive x-axis.
      reflon = proj%stdlon + 90.
     
      ! Compute numerator term of map scale factor
      scale_top = 1. + proj%hemi * SIN(proj%truelat1 * rad_per_deg)
  
      ! Compute radius to point of interest
      xx = i - proj%polei
      yy = (j - proj%polej) * proj%hemi
      r2 = xx**2 + yy**2
  
      ! Now the magic code
      IF (r2 .EQ. 0.) THEN 
         lat = proj%hemi * 90.
         lon = reflon
      ELSE
         gi2 = (proj%rebydx * scale_top)**2.
         lat = deg_per_rad * proj%hemi * ASIN((gi2-r2)/(gi2+r2))
         arccos = ACOS(xx/SQRT(r2))
         IF (yy .GT. 0) THEN
            lon = reflon + deg_per_rad * arccos
         ELSE
            lon = reflon - deg_per_rad * arccos
         ENDIF
      ENDIF
    
      ! Convert to a -180 -> 180 East convention
      IF (lon .GT. 180.) lon = lon - 360.
      IF (lon .LT. -180.) lon = lon + 360.

      RETURN
   
   END SUBROUTINE ijll_ps


   SUBROUTINE set_ps_wgs84(proj)
      ! Initializes a polar-stereographic map projection (WGS84 ellipsoid) 
      ! from the partially filled proj structure. This routine computes the 
      ! radius to the southwest corner and computes the i/j location of the 
      ! pole for use in llij_ps and ijll_ps.

      IMPLICIT NONE
   
      ! Arguments
      TYPE(proj_info), INTENT(INOUT)    :: proj
  
      ! Local variables
      real :: h, mc, tc, t, rho

      h = proj%hemi

      mc = cos(h*proj%truelat1*rad_per_deg)/sqrt(1.0-(E_WGS84*sin(h*proj%truelat1*rad_per_deg))**2.0)
      tc = sqrt(((1.0-sin(h*proj%truelat1*rad_per_deg))/(1.0+sin(h*proj%truelat1*rad_per_deg)))* &
                (((1.0+E_WGS84*sin(h*proj%truelat1*rad_per_deg))/(1.0-E_WGS84*sin(h*proj%truelat1*rad_per_deg)))**E_WGS84 ))

      ! Find the i/j location of reference lat/lon with respect to the pole of the projection
      t = sqrt(((1.0-sin(h*proj%lat1*rad_per_deg))/(1.0+sin(h*proj%lat1*rad_per_deg)))* &
               (((1.0+E_WGS84*sin(h*proj%lat1*rad_per_deg))/(1.0-E_WGS84*sin(h*proj%lat1*rad_per_deg)) )**E_WGS84 ) )
      rho = h * (A_WGS84 / proj%dx) * mc * t / tc
      proj%polei = rho * sin((h*proj%lon1 - h*proj%stdlon)*rad_per_deg)
      proj%polej = -rho * cos((h*proj%lon1 - h*proj%stdlon)*rad_per_deg)

      RETURN

   END SUBROUTINE set_ps_wgs84


   SUBROUTINE llij_ps_wgs84(lat,lon,proj,i,j)
      ! Given latitude (-90 to 90), longitude (-180 to 180), and the
      ! standard polar-stereographic projection information via the 
      ! public proj structure, this routine returns the i/j indices which
      ! if within the domain range from 1->nx and 1->ny, respectively.
  
      IMPLICIT NONE
  
      ! Arguments
      REAL, INTENT(IN)               :: lat
      REAL, INTENT(IN)               :: lon
      REAL, INTENT(OUT)              :: i !(x-index)
      REAL, INTENT(OUT)              :: j !(y-index)
      TYPE(proj_info),INTENT(IN)     :: proj
  
      ! Local variables
      real :: h, mc, tc, t, rho

      h = proj%hemi

      mc = cos(h*proj%truelat1*rad_per_deg)/sqrt(1.0-(E_WGS84*sin(h*proj%truelat1*rad_per_deg))**2.0)
      tc = sqrt(((1.0-sin(h*proj%truelat1*rad_per_deg))/(1.0+sin(h*proj%truelat1*rad_per_deg)))* &
                (((1.0+E_WGS84*sin(h*proj%truelat1*rad_per_deg))/(1.0-E_WGS84*sin(h*proj%truelat1*rad_per_deg)))**E_WGS84 ))

      t = sqrt(((1.0-sin(h*lat*rad_per_deg))/(1.0+sin(h*lat*rad_per_deg))) * &
               (((1.0+E_WGS84*sin(h*lat*rad_per_deg))/(1.0-E_WGS84*sin(h*lat*rad_per_deg)))**E_WGS84))

      ! Find the x/y location of the requested lat/lon with respect to the pole of the projection
      rho = (A_WGS84 / proj%dx) * mc * t / tc
      i = h *  rho * sin((h*lon - h*proj%stdlon)*rad_per_deg)
      j = h *(-rho)* cos((h*lon - h*proj%stdlon)*rad_per_deg)

      ! Get i/j relative to reference i/j
      i = proj%knowni + (i - proj%polei)
      j = proj%knownj + (j - proj%polej)
  
      RETURN

   END SUBROUTINE llij_ps_wgs84


   SUBROUTINE ijll_ps_wgs84(i, j, proj, lat, lon)
 
      ! This is the inverse subroutine of llij_ps.  It returns the 
      ! latitude and longitude of an i/j point given the projection info 
      ! structure.  
  
      implicit none
  
      ! Arguments
      REAL, INTENT(IN)                    :: i    ! Column
      REAL, INTENT(IN)                    :: j    ! Row
      REAL, INTENT(OUT)                   :: lat     ! -90 -> 90 north
      REAL, INTENT(OUT)                   :: lon     ! -180 -> 180 East
      TYPE (proj_info), INTENT(IN)        :: proj

      ! Local variables
      real :: h, mc, tc, t, rho, x, y
      real :: chi, a, b, c, d

      h = proj%hemi
      x = (i - proj%knowni + proj%polei)
      y = (j - proj%knownj + proj%polej)

      mc = cos(h*proj%truelat1*rad_per_deg)/sqrt(1.0-(E_WGS84*sin(h*proj%truelat1*rad_per_deg))**2.0)
      tc = sqrt(((1.0-sin(h*proj%truelat1*rad_per_deg))/(1.0+sin(h*proj%truelat1*rad_per_deg))) * &
                (((1.0+E_WGS84*sin(h*proj%truelat1*rad_per_deg))/(1.0-E_WGS84*sin(h*proj%truelat1*rad_per_deg)))**E_WGS84 ))

      rho = sqrt((x*proj%dx)**2.0 + (y*proj%dx)**2.0)
      t = rho * tc / (A_WGS84 * mc) 

      lon = h*proj%stdlon + h*atan2(h*x,h*(-y))

      chi = PI/2.0-2.0*atan(t)
      a = 1./2.*E_WGS84**2. + 5./24.*E_WGS84**4. +  1./40.*E_WGS84**6.  +    73./2016.*E_WGS84**8.
      b =                     7./24.*E_WGS84**4. + 29./120.*E_WGS84**6. + 54113./40320.*E_WGS84**8.
      c =                                           7./30.*E_WGS84**6.  +    81./280.*E_WGS84**8.
      d =                                                                  4279./20160.*E_WGS84**8.

      lat = chi + sin(2.*chi)*(a + cos(2.*chi)*(b + cos(2.*chi)*(c + d*cos(2.*chi))))
      lat = h * lat

      lat = lat*deg_per_rad
      lon = lon*deg_per_rad

      RETURN
   
   END SUBROUTINE ijll_ps_wgs84


   SUBROUTINE set_lc(proj)
      ! Initialize the remaining items in the proj structure for a
      ! lambert conformal grid.
  
      IMPLICIT NONE
      
      TYPE(proj_info), INTENT(INOUT)     :: proj
  
      REAL                               :: arg
      REAL                               :: deltalon1
      REAL                               :: tl1r
      REAL                               :: ctl1r
  
      ! Compute cone factor
      CALL lc_cone(proj%truelat1, proj%truelat2, proj%cone)
  
      ! Compute longitude differences and ensure we stay out of the
      ! forbidden "cut zone"
      deltalon1 = proj%lon1 - proj%stdlon
      IF (deltalon1 .GT. +180.) deltalon1 = deltalon1 - 360.
      IF (deltalon1 .LT. -180.) deltalon1 = deltalon1 + 360.
  
      ! Convert truelat1 to radian and compute COS for later use
      tl1r = proj%truelat1 * rad_per_deg
      ctl1r = COS(tl1r)
  
      ! Compute the radius to our known lower-left (SW) corner
      proj%rsw = proj%rebydx * ctl1r/proj%cone * &
             (TAN((90.*proj%hemi-proj%lat1)*rad_per_deg/2.) / &
              TAN((90.*proj%hemi-proj%truelat1)*rad_per_deg/2.))**proj%cone
  
      ! Find pole point
      arg = proj%cone*(deltalon1*rad_per_deg)
      proj%polei = proj%hemi*proj%knowni - proj%hemi * proj%rsw * SIN(arg)
      proj%polej = proj%hemi*proj%knownj + proj%rsw * COS(arg)  
  
      RETURN

   END SUBROUTINE set_lc                             


   SUBROUTINE lc_cone(truelat1, truelat2, cone)
 
   ! Subroutine to compute the cone factor of a Lambert Conformal projection
 
      IMPLICIT NONE
      
      ! Input Args
      REAL, INTENT(IN)             :: truelat1  ! (-90 -> 90 degrees N)
      REAL, INTENT(IN)             :: truelat2  !   "   "  "   "     "
  
      ! Output Args
      REAL, INTENT(OUT)            :: cone
  
      ! Locals
  
      ! BEGIN CODE
  
      ! First, see if this is a secant or tangent projection.  For tangent
      ! projections, truelat1 = truelat2 and the cone is tangent to the 
      ! Earth's surface at this latitude.  For secant projections, the cone
      ! intersects the Earth's surface at each of the distinctly different
      ! latitudes
      IF (ABS(truelat1-truelat2) .GT. 0.1) THEN
         cone = ALOG10(COS(truelat1*rad_per_deg)) - &
                ALOG10(COS(truelat2*rad_per_deg))
         cone = cone /(ALOG10(TAN((45.0 - ABS(truelat1)/2.0) * rad_per_deg)) - &
                ALOG10(TAN((45.0 - ABS(truelat2)/2.0) * rad_per_deg)))        
      ELSE
         cone = SIN(ABS(truelat1)*rad_per_deg )  
      ENDIF

      RETURN

   END SUBROUTINE lc_cone


   SUBROUTINE ijll_lc( i, j, proj, lat, lon)
 
   ! Subroutine to convert from the (i,j) cartesian coordinate to the 
   ! geographical latitude and longitude for a Lambert Conformal projection.
 
   ! History:
   ! 25 Jul 01: Corrected by B. Shaw, NOAA/FSL
   ! 
      IMPLICIT NONE
  
      ! Input Args
      REAL, INTENT(IN)              :: i        ! Cartesian X coordinate
      REAL, INTENT(IN)              :: j        ! Cartesian Y coordinate
      TYPE(proj_info),INTENT(IN)    :: proj     ! Projection info structure
  
      ! Output Args                 
      REAL, INTENT(OUT)             :: lat      ! Latitude (-90->90 deg N)
      REAL, INTENT(OUT)             :: lon      ! Longitude (-180->180 E)
  
      ! Locals 
      REAL                          :: inew
      REAL                          :: jnew
      REAL                          :: r
      REAL                          :: chi,chi1,chi2
      REAL                          :: r2
      REAL                          :: xx
      REAL                          :: yy
  
      ! BEGIN CODE
  
      chi1 = (90. - proj%hemi*proj%truelat1)*rad_per_deg
      chi2 = (90. - proj%hemi*proj%truelat2)*rad_per_deg
  
      ! See if we are in the southern hemispere and flip the indices
      ! if we are. 
      inew = proj%hemi * i
      jnew = proj%hemi * j
  
      ! Compute radius**2 to i/j location
      xx = inew - proj%polei
      yy = proj%polej - jnew
      r2 = (xx*xx + yy*yy)
      r = SQRT(r2)/proj%rebydx
     
      ! Convert to lat/lon
      IF (r2 .EQ. 0.) THEN
         lat = proj%hemi * 90.
         lon = proj%stdlon
      ELSE
         
         ! Longitude
         lon = proj%stdlon + deg_per_rad * ATAN2(proj%hemi*xx,yy)/proj%cone
         lon = AMOD(lon+360., 360.)
   
         ! Latitude.  Latitude determined by solving an equation adapted 
         ! from:
         !  Maling, D.H., 1973: Coordinate Systems and Map Projections
         ! Equations #20 in Appendix I.  
           
         IF (chi1 .EQ. chi2) THEN
            chi = 2.0*ATAN( ( r/TAN(chi1) )**(1./proj%cone) * TAN(chi1*0.5) )
         ELSE
            chi = 2.0*ATAN( (r*proj%cone/SIN(chi1))**(1./proj%cone) * TAN(chi1*0.5)) 
         ENDIF
         lat = (90.0-chi*deg_per_rad)*proj%hemi
  
      ENDIF
  
      IF (lon .GT. +180.) lon = lon - 360.
      IF (lon .LT. -180.) lon = lon + 360.
 
      RETURN

   END SUBROUTINE ijll_lc


   SUBROUTINE llij_lc( lat, lon, proj, i, j)
 
   ! Subroutine to compute the geographical latitude and longitude values
   ! to the cartesian x/y on a Lambert Conformal projection.
     
      IMPLICIT NONE
  
      ! Input Args
      REAL, INTENT(IN)              :: lat      ! Latitude (-90->90 deg N)
      REAL, INTENT(IN)              :: lon      ! Longitude (-180->180 E)
      TYPE(proj_info),INTENT(IN)      :: proj     ! Projection info structure
  
      ! Output Args                 
      REAL, INTENT(OUT)             :: i        ! Cartesian X coordinate
      REAL, INTENT(OUT)             :: j        ! Cartesian Y coordinate
  
      ! Locals 
      REAL                          :: arg
      REAL                          :: deltalon
      REAL                          :: tl1r
      REAL                          :: rm
      REAL                          :: ctl1r
      
  
      ! BEGIN CODE
      
      ! Compute deltalon between known longitude and standard lon and ensure
      ! it is not in the cut zone
      deltalon = lon - proj%stdlon
      IF (deltalon .GT. +180.) deltalon = deltalon - 360.
      IF (deltalon .LT. -180.) deltalon = deltalon + 360.
      
      ! Convert truelat1 to radian and compute COS for later use
      tl1r = proj%truelat1 * rad_per_deg
      ctl1r = COS(tl1r)     
     
      ! Radius to desired point
      rm = proj%rebydx * ctl1r/proj%cone * &
           (TAN((90.*proj%hemi-lat)*rad_per_deg/2.) / &
            TAN((90.*proj%hemi-proj%truelat1)*rad_per_deg/2.))**proj%cone
  
      arg = proj%cone*(deltalon*rad_per_deg)
      i = proj%polei + proj%hemi * rm * SIN(arg)
      j = proj%polej - rm * COS(arg)
  
      ! Finally, if we are in the southern hemisphere, flip the i/j
      ! values to a coordinate system where (1,1) is the SW corner
      ! (what we assume) which is different than the original NCEP
      ! algorithms which used the NE corner as the origin in the 
      ! southern hemisphere (left-hand vs. right-hand coordinate?)
      i = proj%hemi * i  
      j = proj%hemi * j

      RETURN
   END SUBROUTINE llij_lc


   SUBROUTINE set_merc(proj)
   
      ! Sets up the remaining basic elements for the mercator projection
  
      IMPLICIT NONE
      TYPE(proj_info), INTENT(INOUT)       :: proj
      REAL                                 :: clain
  
  
      !  Preliminary variables
  
      clain = COS(rad_per_deg*proj%truelat1)
      proj%dlon = proj%dx / (proj%re_m * clain)
  
      ! Compute distance from equator to origin, and store in the 
      ! proj%rsw tag.
  
      proj%rsw = 0.
      IF (proj%lat1 .NE. 0.) THEN
         proj%rsw = (ALOG(TAN(0.5*((proj%lat1+90.)*rad_per_deg))))/proj%dlon
      ENDIF

      RETURN

   END SUBROUTINE set_merc


   SUBROUTINE llij_merc(lat, lon, proj, i, j)
 
      ! Compute i/j coordinate from lat lon for mercator projection
    
      IMPLICIT NONE
      REAL, INTENT(IN)              :: lat
      REAL, INTENT(IN)              :: lon
      TYPE(proj_info),INTENT(IN)    :: proj
      REAL,INTENT(OUT)              :: i
      REAL,INTENT(OUT)              :: j
      REAL                          :: deltalon
  
      deltalon = lon - proj%lon1
      IF (deltalon .LT. -180.) deltalon = deltalon + 360.
      IF (deltalon .GT. 180.) deltalon = deltalon - 360.
      i = proj%knowni + (deltalon/(proj%dlon*deg_per_rad))
      j = proj%knownj + (ALOG(TAN(0.5*((lat + 90.) * rad_per_deg)))) / &
             proj%dlon - proj%rsw
  
      RETURN

   END SUBROUTINE llij_merc


   SUBROUTINE ijll_merc(i, j, proj, lat, lon)
 
      ! Compute the lat/lon from i/j for mercator projection
  
      IMPLICIT NONE
      REAL,INTENT(IN)               :: i
      REAL,INTENT(IN)               :: j    
      TYPE(proj_info),INTENT(IN)    :: proj
      REAL, INTENT(OUT)             :: lat
      REAL, INTENT(OUT)             :: lon 
  
  
      lat = 2.0*ATAN(EXP(proj%dlon*(proj%rsw + j-proj%knownj)))*deg_per_rad - 90.
      lon = (i-proj%knowni)*proj%dlon*deg_per_rad + proj%lon1
      IF (lon.GT.180.) lon = lon - 360.
      IF (lon.LT.-180.) lon = lon + 360.
      RETURN

   END SUBROUTINE ijll_merc


   SUBROUTINE llij_latlon(lat, lon, proj, i, j)
  
      ! Compute the i/j location of a lat/lon on a LATLON grid.
      IMPLICIT NONE
      REAL, INTENT(IN)             :: lat
      REAL, INTENT(IN)             :: lon
      TYPE(proj_info), INTENT(IN)  :: proj
      REAL, INTENT(OUT)            :: i
      REAL, INTENT(OUT)            :: j
  
      REAL                         :: deltalat
      REAL                         :: deltalon
      REAL                         :: lon360
  
      ! Compute deltalat and deltalon as the difference between the input 
      ! lat/lon and the origin lat/lon
      deltalat = lat - proj%lat1
      deltalon = lon - proj%lon1      
      
      ! Compute i/j
      i = deltalon/proj%loninc
      j = deltalat/proj%latinc

      i = i + proj%knowni
      j = j + proj%knownj
  
      RETURN

   END SUBROUTINE llij_latlon


   SUBROUTINE ijll_latlon(i, j, proj, lat, lon)
  
      ! Compute the lat/lon location of an i/j on a LATLON grid.
      IMPLICIT NONE
      REAL, INTENT(IN)             :: i
      REAL, INTENT(IN)             :: j
      TYPE(proj_info), INTENT(IN)  :: proj
      REAL, INTENT(OUT)            :: lat
      REAL, INTENT(OUT)            :: lon
  
      REAL                         :: i_work, j_work
      REAL                         :: deltalat
      REAL                         :: deltalon
      REAL                         :: lon360
  
      i_work = i - proj%knowni
      j_work = j - proj%knownj

      ! Compute deltalat and deltalon 
      deltalat = j_work*proj%latinc
      deltalon = i_work*proj%loninc
  
      lat = proj%lat1 + deltalat
      lon = proj%lon1 + deltalon
  
      RETURN

   END SUBROUTINE ijll_latlon


   SUBROUTINE llij_rotlatlon(lat, lon, proj, i, j)
   
      IMPLICIT NONE
    
      ! Arguments
      REAL, INTENT(IN) :: lat, lon
      REAL, INTENT(OUT) :: i, j
      TYPE (proj_info), INTENT(IN) :: proj
      
      ! Local variables
      REAL(KIND=HIGH) :: dphd,dlmd !Grid increments, degrees
      INTEGER :: ii,imt,jj,jmt,k,krows,ncol,nrow,iri
      REAL(KIND=HIGH) :: glatd  !Geographic latitude, positive north
      REAL(KIND=HIGH) :: glond  !Geographic longitude, positive west
      REAL(KIND=HIGH) :: col,d1,d2,d2r,dlm,dlm1,dlm2,dph,glat,glon,    &
              pi,r2d,row,tlat,tlat1,tlat2,              &
              tlon,tlon1,tlon2,tph0,tlm0,x,y,z

      glatd = lat
      glond = -lon
  
      dphd = proj%phi/REAL((proj%jydim-1)/2)
      dlmd = proj%lambda/REAL(proj%ixdim-1)
	
      pi = ACOS(-1.0)
      d2r = pi/180.
      r2d = 1./d2r
  
      imt = 2*proj%ixdim-1
      jmt = proj%jydim/2+1

      glat = glatd*d2r
      glon = glond*d2r
      dph = dphd*d2r
      dlm = dlmd*d2r
      tph0 = proj%lat1*d2r
      tlm0 = -proj%lon1*d2r
  
      x = COS(tph0)*COS(glat)*COS(glon-tlm0)+SIN(tph0)*SIN(glat)
      y = -COS(glat)*SIN(glon-tlm0)
      z = COS(tph0)*SIN(glat)-SIN(tph0)*COS(glat)*COS(glon-tlm0)

!	if (abs(lon) .gt. 49.0 .and. abs(lon) .lt. 50.0) then
!	write(0,*) 'glat, glon: ', glat, glon
!	write(0,*) 'x,y,z: ', x,y,z
!	write(0,*) 'term for ATAN in tlat: ', (z/SQRT(x*x+y*y))
!	endif

      tlat = r2d*ATAN(z/SQRT(x*x+y*y))
      tlon = r2d*ATAN(y/x)

!!
	if (abs(glat-tph0) .gt. pi/2.) then
! set artificially large to avoid data outside of domain
	tlat=120.
	tlon=500.
	endif
	
!!


!	if (abs(lon) .gt. 49.0 .and. abs(lon) .lt. 50.0) then
!	write(0,*) '--> lat, lon, tlat, tlon :: ', lat,lon,tlat,tlon
!	endif
	
      row = tlat/dphd+jmt
      col = tlon/dlmd+proj%ixdim

      nrow = NINT(row)
      ncol = NINT(col)

      tlat = tlat*d2r
      tlon = tlon*d2r
  
      IF (proj%stagger == HH) THEN
  
         IF ((abs(MOD(nrow,2)) == 1 .AND. abs(MOD(ncol,2)) == 1) .OR. &
             (MOD(nrow,2) == 0 .AND. MOD(ncol,2) == 0)) THEN

            tlat1 = (nrow-jmt)*dph
            tlat2 = tlat1+dph
            tlon1 = (ncol-proj%ixdim)*dlm
            tlon2 = tlon1+dlm

            dlm1 = tlon-tlon1
            dlm2 = tlon-tlon2
            d1 = ACOS(COS(tlat)*COS(tlat1)*COS(dlm1)+SIN(tlat)*SIN(tlat1))
            d2 = ACOS(COS(tlat)*COS(tlat2)*COS(dlm2)+SIN(tlat)*SIN(tlat2))

            IF (d1 > d2) THEN
               nrow = nrow+1
               ncol = ncol+1
            END IF
   
         ELSE
            tlat1 = (nrow+1-jmt)*dph
            tlat2 = tlat1-dph
            tlon1 = (ncol-proj%ixdim)*dlm
            tlon2 = tlon1+dlm
            dlm1 = tlon-tlon1
            dlm2 = tlon-tlon2
            d1 = ACOS(COS(tlat)*COS(tlat1)*COS(dlm1)+SIN(tlat)*SIN(tlat1))
            d2 = ACOS(COS(tlat)*COS(tlat2)*COS(dlm2)+SIN(tlat)*SIN(tlat2))

            IF (d1 < d2) THEN
               nrow = nrow+1
            ELSE
               ncol = ncol+1
            END IF
         END IF
  
      ELSE IF (proj%stagger == VV) THEN
  
         IF ((MOD(nrow,2) == 0 .AND. abs(MOD(ncol,2)) == 1) .OR. &
             (abs(MOD(nrow,2)) == 1 .AND. MOD(ncol,2) == 0)) THEN
            tlat1 = (nrow-jmt)*dph
            tlat2 = tlat1+dph
            tlon1 = (ncol-proj%ixdim)*dlm
            tlon2 = tlon1+dlm
            dlm1 = tlon-tlon1
            dlm2 = tlon-tlon2
            d1 = ACOS(COS(tlat)*COS(tlat1)*COS(dlm1)+SIN(tlat)*SIN(tlat1))
            d2 = ACOS(COS(tlat)*COS(tlat2)*COS(dlm2)+SIN(tlat)*SIN(tlat2))
    
            IF (d1 > d2) THEN
               nrow = nrow+1
               ncol = ncol+1
            END IF
   
         ELSE
            tlat1 = (nrow+1-jmt)*dph
            tlat2 = tlat1-dph
            tlon1 = (ncol-proj%ixdim)*dlm
            tlon2 = tlon1+dlm
            dlm1 = tlon-tlon1
            dlm2 = tlon-tlon2
            d1 = ACOS(COS(tlat)*COS(tlat1)*COS(dlm1)+SIN(tlat)*SIN(tlat1))
            d2 = ACOS(COS(tlat)*COS(tlat2)*COS(dlm2)+SIN(tlat)*SIN(tlat2))
    
            IF (d1 < d2) THEN
               nrow = nrow+1
            ELSE
               ncol = ncol+1
            END IF
         END IF
      END IF
  

!!! Added next line as a Kludge - not yet understood why needed
	if (ncol .le. 0) ncol=ncol-1

      jj = nrow
      ii = ncol/2

      IF (proj%stagger == HH) THEN
         IF (abs(MOD(jj,2)) == 1) ii = ii+1
      ELSE IF (proj%stagger == VV) THEN
         IF (MOD(jj,2) == 0) ii=ii+1
      END IF

      i = REAL(ii)
      j = REAL(jj)

   END SUBROUTINE llij_rotlatlon


   SUBROUTINE ijll_rotlatlon(i, j, proj, lat,lon)
   
      IMPLICIT NONE
    
      ! Arguments
      REAL, INTENT(IN) :: i, j
      REAL, INTENT(OUT) :: lat, lon
      TYPE (proj_info), INTENT(IN) :: proj
      
      ! Local variables
      INTEGER :: ih,jh
      INTEGER :: midcol,midrow,ncol,iadd1,iadd2,imt,jh2,knrow,krem,kv,nrow
      REAL :: dphd,dlmd !Grid increments, degrees
      REAL(KIND=HIGH) :: arg1,arg2,d2r,fctr,glatr,glatd,glond,pi, &
              r2d,tlatd,tlond,tlatr,tlonr,tlm0,tph0
  
      ih = NINT(i)
      jh = NINT(j)
  
      dphd = proj%phi/REAL((proj%jydim-1)/2)
      dlmd = proj%lambda/REAL(proj%ixdim-1)
    
      pi = ACOS(-1.0)
      d2r = pi/180.
      r2d = 1./d2r
      tph0 = proj%lat1*d2r
      tlm0 = -proj%lon1*d2r

      midrow = (proj%jydim+1)/2
      midcol = proj%ixdim

!      IF (proj%stagger == HH) THEN

!!!         ncol = 2*ih-1+MOD(jh+1,2)
         ncol = 2*ih-1+abs(MOD(jh+1,2))
         tlatd = (jh-midrow)*dphd
         tlond = (ncol-midcol)*dlmd
       IF (proj%stagger == VV) THEN
          if (mod(jh,2) .eq. 0) then
             tlond = tlond - DLMD
          else
             tlond = tlond + DLMD
          end if
       END IF
    
      tlatr = tlatd*d2r
      tlonr = tlond*d2r
      arg1 = SIN(tlatr)*COS(tph0)+COS(tlatr)*SIN(tph0)*COS(tlonr)
      glatr = ASIN(arg1)
     
      glatd = glatr*r2d
     
      arg2 = COS(tlatr)*COS(tlonr)/(COS(glatr)*COS(tph0))-TAN(glatr)*TAN(tph0)
      IF (ABS(arg2) > 1.) arg2 = ABS(arg2)/arg2
      fctr = 1.
      IF (tlond > 0.) fctr = -1.
     
      glond = tlm0*r2d+fctr*ACOS(arg2)*r2d

      lat = glatd
      lon = -glond

      IF (lon .GT. +180.) lon = lon - 360.
      IF (lon .LT. -180.) lon = lon + 360.
   
   END SUBROUTINE ijll_rotlatlon


   SUBROUTINE set_gauss(proj)
    
      IMPLICIT NONE
 
      ! Argument
      type (proj_info), intent(inout) :: proj
 
      !  Initialize the array that will hold the Gaussian latitudes.
 
      IF ( ASSOCIATED( proj%gauss_lat ) ) THEN
         DEALLOCATE ( proj%gauss_lat )
      END IF
 
      !  Get the needed space for our array.
 
      ALLOCATE ( proj%gauss_lat(proj%nlat*2) )
 
      !  Compute the Gaussian latitudes.
 
      CALL gausll( proj%nlat*2 , proj%gauss_lat )
 
      !  Now, these could be upside down from what we want, so let's check.
      !  We take advantage of the equatorial symmetry to remove any sort of
      !  array re-ordering.
 
      IF ( ABS(proj%gauss_lat(1) - proj%lat1) .GT. 0.01 ) THEN
         proj%gauss_lat = -1. * proj%gauss_lat
      END IF
 
      !  Just a sanity check.
 
      IF ( ABS(proj%gauss_lat(1) - proj%lat1) .GT. 0.01 ) THEN
         PRINT '(A)','Oops, something is not right with the Gaussian latitude computation.'
         PRINT '(A,F8.3,A)','The input data gave the starting latitude as ',proj%lat1,'.'
         PRINT '(A,F8.3,A)','This routine computed the starting latitude as +-',ABS(proj%gauss_lat(1)),'.'
         PRINT '(A,F8.3,A)','The difference is larger than 0.01 degrees, which is not expected.'
         call mprintf(.true.,ERROR,'Gaussian_latitude_computation')
      END IF
 
   END SUBROUTINE set_gauss


   SUBROUTINE gausll ( nlat , lat_sp )
 
      IMPLICIT NONE
   
      INTEGER                            :: nlat , i
      REAL (KIND=HIGH) , PARAMETER       :: pi = 3.141592653589793
      REAL (KIND=HIGH)                   :: sum1, sum2, sum3, sum4, xn, a, b
      REAL (KIND=HIGH) , DIMENSION(nlat) :: cosc , gwt , sinc , colat , wos2 , lat , mlat
      REAL             , DIMENSION(nlat) :: lat_sp
   
      CALL lggaus(nlat, cosc, gwt, sinc, colat, wos2)
   
      DO i = 1, nlat
         lat(i) = ACOS(sinc(i)) * 180._HIGH / pi
         IF (i.gt.nlat/2) lat(i) = -lat(i)
      END DO
   
      lat_sp = REAL(lat)
 
   END SUBROUTINE gausll


   SUBROUTINE lggaus( nlat, cosc, gwt, sinc, colat, wos2 )
 
      IMPLICIT NONE
 
      !  LGGAUS finds the Gaussian latitudes by finding the roots of the
      !  ordinary Legendre polynomial of degree NLAT using Newton's
      !  iteration method.
      
      !  On entry:
            integer NLAT ! the number of latitudes (degree of the polynomial)
      
      !  On exit: for each Gaussian latitude
      !     COSC   - cos(colatitude) or sin(latitude)
      !     GWT    - the Gaussian weights
      !     SINC   - sin(colatitude) or cos(latitude)
      !     COLAT  - the colatitudes in radians
      !     WOS2   - Gaussian weight over sin**2(colatitude)
 
      REAL (KIND=HIGH) , DIMENSION(nlat) :: cosc , gwt , sinc , colat  , wos2 
      REAL (KIND=HIGH) , PARAMETER       :: pi = 3.141592653589793
 
      !  Convergence criterion for iteration of cos latitude
 
      REAL , PARAMETER :: xlim  = 1.0E-14
 
      INTEGER :: nzero, i, j
      REAL (KIND=HIGH) :: fi, fi1, a, b, g, gm, gp, gt, delta, c, d
 
      !  The number of zeros between pole and equator
 
      nzero = nlat/2
 
      !  Set first guess for cos(colat)
 
      DO i=1,nzero
         cosc(i) = SIN( (i-0.5)*pi/nlat + pi*0.5 )
      END DO
 
      !  Constants for determining the derivative of the polynomial
      fi  = nlat
      fi1 = fi+1.0
      a   = fi*fi1 / SQRT(4.0*fi1*fi1-1.0)
      b   = fi1*fi / SQRT(4.0*fi*fi-1.0)
 
      !  Loop over latitudes, iterating the search for each root
 
      DO i=1,nzero
         j=0
 
         !  Determine the value of the ordinary Legendre polynomial for
         !  the current guess root
 
         DO
            CALL lgord( g, cosc(i), nlat )
   
            !  Determine the derivative of the polynomial at this point
   
            CALL lgord( gm, cosc(i), nlat-1 )
            CALL lgord( gp, cosc(i), nlat+1 )
            gt = (cosc(i)*cosc(i)-1.0) / (a*gp-b*gm)
   
            !  Update the estimate of the root
   
            delta   = g*gt
            cosc(i) = cosc(i) - delta
   
            !  If convergence criterion has not been met, keep trying
   
            j = j+1
            IF( ABS(delta).GT.xlim ) CYCLE
   
            !  Determine the Gaussian weights
   
            c      = 2.0 *( 1.0-cosc(i)*cosc(i) )
            CALL lgord( d, cosc(i), nlat-1 )
            d      = d*d*fi*fi
            gwt(i) = c *( fi-0.5 ) / d
            EXIT
 
         END DO
 
      END DO
 
      !  Determine the colatitudes and sin(colat) and weights over sin**2
 
      DO i=1,nzero
         colat(i)= ACOS(cosc(i))
         sinc(i) = SIN(colat(i))
         wos2(i) = gwt(i) /( sinc(i)*sinc(i) )
      END DO
 
      !  If NLAT is odd, set values at the equator
 
      IF( MOD(nlat,2) .NE. 0 ) THEN
         i       = nzero+1
         cosc(i) = 0.0
         c       = 2.0
         CALL lgord( d, cosc(i), nlat-1 )
         d       = d*d*fi*fi
         gwt(i)  = c *( fi-0.5 ) / d
         colat(i)= pi*0.5
         sinc(i) = 1.0
         wos2(i) = gwt(i)
      END IF
 
      !  Determine the southern hemisphere values by symmetry
 
      DO i=nlat-nzero+1,nlat
         cosc(i) =-cosc(nlat+1-i)
         gwt(i)  = gwt(nlat+1-i)
         colat(i)= pi-colat(nlat+1-i)
         sinc(i) = sinc(nlat+1-i)
         wos2(i) = wos2(nlat+1-i)
      END DO
 
   END SUBROUTINE lggaus


   SUBROUTINE lgord( f, cosc, n )
 
      IMPLICIT NONE
 
      !  LGORD calculates the value of an ordinary Legendre polynomial at a
      !  specific latitude.
      
      !  On entry:
      !     cosc - COS(colatitude)
      !     n      - the degree of the polynomial
      
      !  On exit:
      !     f      - the value of the Legendre polynomial of degree N at
      !              latitude ASIN(cosc)
 
      REAL (KIND=HIGH) :: s1, c4, a, b, fk, f, cosc, colat, c1, fn, ang
      INTEGER :: n, k
 
      !  Determine the colatitude
 
      colat = ACOS(cosc)
 
      c1 = SQRT(2.0_HIGH)
      DO k=1,n
         c1 = c1 * SQRT( 1.0 - 1.0/(4*k*k) )
      END DO
 
      fn = n
      ang= fn * colat
      s1 = 0.0
      c4 = 1.0
      a  =-1.0
      b  = 0.0
      DO k=0,n,2
         IF (k.eq.n) c4 = 0.5 * c4
         s1 = s1 + c4 * COS(ang)
         a  = a + 2.0
         b  = b + 1.0
         fk = k
         ang= colat * (fn-fk-2.0)
         c4 = ( a * (fn-b+1.0) / ( b * (fn+fn-a) ) ) * c4
      END DO 
 
      f = s1 * c1
 
   END SUBROUTINE lgord


   SUBROUTINE llij_gauss (lat, lon, proj, i, j) 
 
      IMPLICIT NONE
 
      REAL    , INTENT(IN)  :: lat, lon
      REAL    , INTENT(OUT) :: i, j
      TYPE (proj_info), INTENT(IN) :: proj
 
      INTEGER :: n , n_low
      LOGICAL :: found = .FALSE.
      REAL    :: diff_1 , diff_nlat
 
      !  The easy one first, get the x location.  The calling routine has already made
      !  sure that the necessary assumptions concerning the sign of the deltalon and the
      !  relative east/west'ness of the longitude and the starting longitude are consistent
      !  to allow this easy computation.
 
      i = ( lon - proj%lon1 ) / proj%loninc + 1.
 
      !  Since this is a global data set, we need to be concerned about wrapping the
      !  fields around the globe.
 
!      IF      ( ( proj%loninc .GT. 0 ) .AND. &
!                ( FLOOR((lon-proj%lon1)/proj%loninc) + 1 .GE. proj%ixdim ) .AND. &
!                ( lon + proj%loninc .GE. proj%lon1 + 360 ) ) THEN
!! BUG: We may need to set proj%wrap, but proj is intent(in)
!! WHAT IS THIS USED FOR?
!!        proj%wrap = .TRUE.
!         i = proj%ixdim
!      ELSE IF ( ( proj%loninc .LT. 0 ) .AND. &
!                ( FLOOR((lon-proj%lon1)/proj%loninc) + 1 .GE. proj%ixdim ) .AND. &
!                ( lon + proj%loninc .LE. proj%lon1 - 360 ) ) THEN
! ! BUG: We may need to set proj%wrap, but proj is intent(in)
! ! WHAT IS THIS USED FOR?
! !        proj%wrap = .TRUE.
!         i = proj%ixdim
!      END IF
 
      !  Yet another quicky test, can we find bounding values?  If not, then we may be
      !  dealing with putting data to a polar projection, so just give them them maximal
      !  value for the location.  This is an OK assumption for the interpolation across the
      !  top of the pole, given how close the longitude lines are.
 
      IF ( ABS(lat) .GT. ABS(proj%gauss_lat(1)) ) THEN
 
         diff_1    = lat - proj%gauss_lat(1)
         diff_nlat = lat - proj%gauss_lat(proj%nlat*2)
 
         IF ( ABS(diff_1) .LT. ABS(diff_nlat) ) THEN
            j = 1
         ELSE
            j = proj%nlat*2
         END IF
 
      !  If the latitude is between the two bounding values, we have to search and interpolate.
 
      ELSE
 
         DO n = 1 , proj%nlat*2 -1
            IF ( ( proj%gauss_lat(n) - lat ) * ( proj%gauss_lat(n+1) - lat ) .LE. 0 ) THEN
               found = .TRUE.
               n_low = n
               EXIT
            END IF
         END DO
 
         !  Everything still OK?
  
         IF ( .NOT. found ) THEN
            PRINT '(A)','Troubles in river city.  No bounding values of latitude found in the Gaussian routines.'
            call mprintf(.true.,ERROR,'Gee_no_bounding_lats_Gaussian')
         END IF
 
         j = ( ( proj%gauss_lat(n_low) - lat                     ) * ( n_low + 1 ) + &
               ( lat                   - proj%gauss_lat(n_low+1) ) * ( n_low     ) ) / &
               ( proj%gauss_lat(n_low) - proj%gauss_lat(n_low+1) )
 
      END IF

   END SUBROUTINE llij_gauss 
  
END MODULE map_utils
