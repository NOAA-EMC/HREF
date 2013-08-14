!!!!!  ==========================================================  !!!!!
!!!!!                    module physpara description               !!!!!
!!!!!  ==========================================================  !!!!!
!                                                                      !
!     This module defines commonly used control variables/parameters   !
!     in physics related programs.                                     !
!                                                                      !
!     Section 1 contains control variables defined in the form of      !
!     parameter. They are pre-determined choices and not adjustable    !
!     during model's run-time.                                         !
!                                                                      !
!     Section 2 contains control variables defined as module variables.!
!     They are more flexible to be changed during run-time by either   !
!     through input namelist, or through model environment condition.  !
!     They are preassigned here as the default values.                 !
!                                                                      !
!!!!!  ==========================================================  !!!!!

!========================================!
      module physpara                    !
!........................................!
!
!     implicit   none

!  --- ...  define kind parameters here

!   ** if already exist, use the module containing kind definitions
      use machine

!   ** otherwise, define kind parameter here
!     implicit   none
!     integer, public, parameter :: kind_io4 = 4
!     integer, public, parameter :: kind_io8 = 8
!     integer, public, parameter :: kind_phys= selected_real_kind(13,60) ! the '60' maps to 64-bit real
!      .....

!     implicit   none
!
      public

!==================================================================================
!  Section - 1 -
!     control flags are pre-set as run-time non-adjuztable parameters.
!==================================================================================

! ............................................. !
!  -1.1- control flags for sw radiation         !
! ............................................. !
 !     integer,parameter :: iswrate = 2  ! sw heating rate unit control flag
                                        ! =1:k/day; =2:k/second.
 !     integer,parameter :: iswrgas = 1  ! sw rare gases effect control flag (ch4,n2o,o2,...)
                                        ! =0:no; =1:yes.
 !     integer,parameter :: iswcliq = 1  ! sw optical property for liquid clouds
                                        ! =0:input cld opt depth, ignoring iswcice setting
                                        ! =1:input cwp,rew, use hu and stamnes(1993) method
                                        ! =2:not defined yet
 !     integer,parameter :: iswcice = 3  ! sw optical property for ice clouds (only iswcliq>0)
                                        ! =0:not defined yet
                                        ! =1:input cip,rei, use ebert and curry (1992) method
                                        ! =2:input cip,rei, use streamer v3.0 (2001) method
                                        ! =3:input cip,rei, use fu's method (1996) method
 !     integer,parameter :: iswmode = 2  ! sw control flag for 2-stream transfer scheme
                                        ! =1:delta-eddington    (joseph et al., 1976)
                                        ! =2:pifm               (zdunkowski et al., 1980)
                                        ! =3:discrete ordinates (liou, 1973)


! ............................................. !
!  -1.2- control flags for lw radiation         !
! ............................................. !
 !     integer,parameter :: ilwrate = 2  ! lw heating rate unit (1:k/day; 2:k/second)
                                        ! =1:k/day; =2:k/second.
 !     integer,parameter :: ilwrgas = 1  ! lw rare gases effect control flag (ch4,n2o,o2,cfcs...)
                                        ! =0:no; =1:yes.
 !     integer,parameter :: ilwcliq = 1  ! lw optical property for liquid clouds
                                        ! =0:input cld opt depth, ignoring ilwcice setting
                                        ! =1:input cwp,rew, use hu and stamnes(1993) method
                                        ! =2:not defined yet
 !     integer,parameter :: ilwcice = 3  ! lw optical property for ice clouds (only ilwcliq>0)
                                        ! =0:not defined yet
                                        ! =1:input cip,rei, use ebert and curry (1992) method
                                        ! =2:input cip,rei, use streamer (1996) method
                                        ! =3:input cip,rei, use fu's method (1998) method

! ............................................. !
!  -1.3- control flag for lw aerosol property   !
      logical,parameter :: lalw1bd =.false. ! =t: use 1 broad-band lw aeros properties
                                            ! =f: use multi bands aeros properites




!==================================================================================
!  Section - 2 -
!     values of control flags might be re-set in initialization subroutines
!       (may be adjusted at run time based on namelist input or run condition)
!==================================================================================

! ............................................. !
!  -2.1- for module radiation_astronomy         !
! ............................................. !
      integer, save :: isolar  = 0      ! solar constant scheme control flag

      character, save :: solar_file*26  ! external solar constant data table
!     data solar_file   / 'solarconstantdata.txt     ' /
      data solar_file   / 'solarconstant_noaa_a0.txt ' /

! ............................................. !
!  -2.2- for module radiation_aerosols          !
! ............................................. !

      integer, save :: iaermdl = 0      ! aerosol model scheme control flag
      integer, save :: iaerflg = 0      ! aerosol effect control flag

      logical, save :: lalwflg = .true. ! lw aerosols effect control flag
      logical, save :: laswflg = .true. ! sw aerosols effect control flag
      logical, save :: lavoflg = .true. ! stratospheric volcanic effect flag

      character, save :: aeros_file*26  ! external aerosols data file
!     data aeros_file   / 'climaeropac_global.txt    ' /
      data aeros_file   / 'aerosol.dat               ' /

! ............................................. !
!  -2.3- for module radiation_gases             !
! ............................................. !

      integer, save :: ico2flg = 0      ! co2 data source control flag
      integer, save :: ictmflg = 0      ! external data time/date control flag
      integer, save :: ioznflg = 1      ! ozone data source control flag

      character, save :: co2dat_file*26 ! external co2 2d monthly obsv data table
      character, save :: co2gbl_file*26 ! external co2 global annual mean data tb
      character, save :: co2usr_file*26 ! external co2 user defined data table
      character, save :: co2cyc_file*26 ! external co2 clim monthly cycle data tb
      data co2dat_file   / 'co2historicaldata_2004.txt' /   !year is run-time selected
      data co2gbl_file   / 'co2historicaldata_glob.txt' /
      data co2usr_file   / 'co2userdata.txt           ' /
      data co2cyc_file   / 'co2monthlycyc.txt         ' /

! ............................................. !
!  -2.4- for module radiation_clouds            !
! ............................................. !

      integer, save :: icldflg = 1      ! cloud optical property scheme control flag
      integer, save :: icmphys = 1      ! cloud microphysics scheme control flag
      integer, save :: iovrsw  = 1      ! cloud overlapping control flag for sw
      integer, save :: iovrlw  = 1      ! cloud overlapping control flag for lw

      logical, save :: lcrick  =.false. ! eliminating CRICK control flag
      logical, save :: lcnorm  =.false. ! in-cld condensate control flag
      logical, save :: lnoprec =.false. ! precip effect on radiation flag (ferrier microphysics)
      logical, save :: lsashal =.false. ! shallow convection flag

! ............................................. !
!  -2.5- for module radiation_surface           !
! ............................................. !
      integer, save :: ialbflg = 0      ! surface albedo scheme control flag
      integer, save :: iemsflg = 0      ! surface emissivity scheme control flag

      character, save :: semis_file*26  ! external sfc emissivity data table
      data semis_file   / 'sfc_emissivity_idx.txt    ' /

! ............................................. !
!  -2.6- general purpose                        !
! ............................................. !

      integer, save :: ivflip  = 1      ! vertical profile indexing flag
      integer, save :: isubcsw = 0      ! sub-column cloud approx flag in sw radiation
      integer, save :: isubclw = 0      ! sub-column cloud approx flag in lw radiation
      integer, save :: ipsd0   = 0      ! initial permutation seed for mcica radiation


! ......................................................................!
!  variables used in old module_RRTM and now updated in rad_initialize  !
! ......................................................................!

      integer,parameter :: NP3Dx=3 
                               ! 3: ferrier's microphysics cloud scheme (only stratiform cloud)
                               !    (set iflagliq>0 in radsw_param.f and radlw_param.f)
                               ! 4: zhao/carr/sundqvist microphysics cloud (now available in the NMMB)
                               ! 5: NAM stratiform + convective cloud optical depth and fraction
                               !    (set iflagliq=0 in radsw_param.f and radlw_param.f)
      integer,parameter :: ISOLx=0    
                               ! 0: use a fixed solar constant value (default)
                               ! 1: use 11-year cycle solar constant table
      integer,parameter :: ICO2x=1 
                               ! 0: use prescribed global mean co2   (default)
                               ! 1: use observed co2 annual mean value only
                               ! 2: use obs co2 monthly data with 2-d variation
      integer,parameter :: ICWP=1 
                               ! control flag for cloud generation schemes
                               !  0: use diagnostic cloud scheme
                               ! -1: use diagnostic cloud scheme (use with NMMB for NP3D=5)(GFDL type)
                               !  1: use prognostic cloud scheme (use with NMMB for NP3D=3)
      integer,parameter :: IALBx=2
                               ! control flag for surface albedo schemes
                               ! 0: climatology, based on surface veg types  ! ONLY THIS ONE WORKS (GFS)
                               ! 1: modis retrieval based surface albedo scheme
                               ! 2: use externally provided albedoes directly. ! ONLY THIS ONE WORKS for regional
                               !    (CALCULATES ALBEDO FROM NMMB MONTHLY CLIMATOLOGY AS IN GFDL RADIATION)
      integer,parameter :: IEMSx=0
                               ! control flag for surface emissivity schemes
                               ! 0: fixed value of 1.0   (default)
                               ! 1: varying value based on surface veg types
      integer,parameter :: IAERx=11
                               ! flag for aerosols scheme selection (all options work for NMMB)
                               ! - 3-digit aerosol flag (volc,lw,sw)
                               !   0: turn all aeros effects off (sw,lw,volc)
                               !   1: use clim tropspheric aerosol for sw only
                               !  10: use clim tropspheric aerosol for lw only
                               !  11: use clim tropspheric aerosol for both sw and lw
                               ! 100: volc aerosol only for both sw and lw
                               ! 101: volc and clim trops aerosol for sw only
                               ! 110: volc and clim trops aerosol for lw only
                               ! 111: volc and clim trops aerosol for both sw and lw
                               !   2: gocart/BSC-Dust tropspheric aerosol for sw only
                               !  20: gocart/BSC-Dust tropspheric aerosol for lw only
                               !  22: gocart/BSC-Dust tropspheric aerosol for both sw and lw
                               ! 102: volc and gocart trops aerosol for sw only
                               ! 120: volc and gocart trops aerosol for lw only
                               ! 122: volc and gocart trops aerosol for both sw and lw
       integer,parameter :: NTRAC=3
                               ! dimension veriable for array oz
       integer,parameter :: NTOZx=0
                               !  0: climatological ozone profile
                               ! >0: interactive ozone profile
       integer,parameter :: NCLDX=1
                               !  only used when ntcw .gt. 0
       integer,parameter :: NTCWx=3
                               !  0: no cloud condensate calculated
                               ! >0: array index location for cloud condensate
       integer,parameter :: IOVR_SWx=1
                               !  0 sw: random overlap clouds
                               !  1 sw: max-random overlap clouds
       integer,parameter :: IOVR_LWx=1
                               !  0 lw: random overlap clouds
                               !  1 lw: max-random overlap clouds
       integer,parameter :: ICTMx=1
                               !  0: use data at initial cond time, if not
                               !     available, use latest, no extrapolation.
                               !  1: use data at the forecast time, if not
                               !     available, use latest and extrapolation.
                               ! -1: use user provided external data for
                               !     the fcst time, no extrapolation.
                               ! -2: same as ictm=0, but add seasonal cycle
                               !     from climatology. no extrapolation.
                               ! yyyy0: use yyyy data for the forecast time,
                               !     no further data extrapolation.
                               ! yyyy1: use yyyy data for the fcst.
                               !     if needed, do extrapolation to match the fcst time.
       integer,parameter :: IFLIPx=0
                               !   0: input data from toa to sfc
                               !   1: input data from sfc to toa
       integer,parameter :: IAER_MDL=0
                               !  default aerosol model is opac-climatology
                               !  > 0,  future gocart-clim/prog scheme (not ready)

       integer,parameter :: ISUBCSWx=0
                               !  isubcsw/isubclw
                               !  sub-column cloud approx control flag (sw/lw rad)
                               !  0: with out sub-column cloud approximation
                               !  1: mcica sub-col approx. prescribed random seed
                               !  2: mcica sub-col approx. provided random seed
       integer,parameter :: ISUBCLWx=0

       logical :: LSSAV=.TRUE.
                               ! logical flag for store 3-d cloud field
                               !  ** need to be .TRUE. for non-zero FLUXR_V & CLDCOV_V off GRRAD
       logical :: NORAD_PRECIPx=.false.
                               ! flag for precip in radiation
                               ! .true. snow/rain has no impact on radiation
       logical :: CRICK_PROOFx=.false.
                               ! flag for eliminating CRICK (smooths profiles)
       logical :: CCNORMx=.true.
                               ! flag for incloud condensate mixing ratio
       logical :: SASHALx=.false.
                               ! New Massflux based shallow convection  (Not in use for NMMB)
       logical :: LPRNT=.FALSE.
!
!...................................!
      end module physpara           !
!===================================!
