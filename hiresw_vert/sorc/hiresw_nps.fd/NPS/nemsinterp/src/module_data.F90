     module module_data

      TYPE input_vars

      REAL, ALLOCATABLE,  DIMENSION(:,:,:) :: PRES
      REAL, POINTER, DIMENSION(:,:,:) :: PINT
      REAL, POINTER, DIMENSION(:,:,:) :: SMC_WPS
      REAL, POINTER, DIMENSION(:,:,:) :: STC_WPS
      REAL, POINTER, DIMENSION(:,:,:) :: GHT
      REAL, POINTER, DIMENSION(:,:,:) :: RH
      REAL, POINTER, DIMENSION(:,:,:) :: SPECHUMD
      REAL, POINTER, DIMENSION(:,:,:) :: UU
      REAL, POINTER, DIMENSION(:,:,:) :: VV
      REAL, POINTER, DIMENSION(:,:,:) :: TT
!new
      REAL, POINTER, DIMENSION(:,:,:) :: FRIMEF
      REAL, POINTER, DIMENSION(:,:,:) :: RWMR
      REAL, POINTER, DIMENSION(:,:,:) :: SNMR
      REAL, POINTER, DIMENSION(:,:,:) :: CICE
      REAL, POINTER, DIMENSION(:,:,:) :: CLWMR
!new
      REAL, POINTER, DIMENSION(:,:)   :: CANWAT
      REAL, POINTER, DIMENSION(:,:)   :: SNOW
      REAL, POINTER, DIMENSION(:,:)   :: SKINTEMP
      REAL, POINTER, DIMENSION(:,:)   :: SOILHGT
      REAL, POINTER, DIMENSION(:,:)   :: LANDSEA
      REAL, POINTER, DIMENSION(:,:)   :: SEAICE
!      REAL, POINTER, DIMENSION(:,:)   :: ST100200
!      REAL, POINTER, DIMENSION(:,:)   :: ST040100
!      REAL, POINTER, DIMENSION(:,:)   :: ST010040
!      REAL, POINTER, DIMENSION(:,:)   :: ST000010
!      REAL, POINTER, DIMENSION(:,:)   :: SM100200
!      REAL, POINTER, DIMENSION(:,:)   :: SM040100
!      REAL, POINTER, DIMENSION(:,:)   :: SM010040
!      REAL, POINTER, DIMENSION(:,:)   :: SM000010
      REAL, POINTER, DIMENSION(:,:)   :: PSFC
      REAL, POINTER, DIMENSION(:,:)   :: SLOPECAT
      REAL, POINTER, DIMENSION(:,:)   :: VEGCAT
      REAL, POINTER, DIMENSION(:,:)   :: SOILCAT
      REAL, POINTER, DIMENSION(:,:)   :: SNOALB
      REAL, POINTER, DIMENSION(:,:,:) :: GREENFRAC
      REAL, POINTER, DIMENSION(:,:,:) :: ALBEDO12M
      REAL, POINTER, DIMENSION(:,:,:) :: SOILCTOP
      REAL, POINTER, DIMENSION(:,:)   :: SOILTEMP
      REAL, POINTER, DIMENSION(:,:)   :: HGT_M
      REAL, POINTER, DIMENSION(:,:)   :: HGT_V
      REAL, POINTER, DIMENSION(:,:)   :: STDVTOPO
      REAL, POINTER, DIMENSION(:,:)   :: XLAT_M
      REAL, POINTER, DIMENSION(:,:)   :: XLAT_V
      REAL, POINTER, DIMENSION(:,:)   :: XLONG_M
      REAL, POINTER, DIMENSION(:,:)   :: XLONG_V
      REAL, POINTER, DIMENSION(:,:)   :: LU_INDEX
      REAL, POINTER, DIMENSION(:,:,:) :: LANDUSEF
      REAL, POINTER, DIMENSION(:,:)   :: LANDMASK
      REAL, POINTER, DIMENSION(:,:,:) :: GWD_OROG
      INTEGER                         :: xdim
      INTEGER                         :: ydim
      INTEGER                         :: zdim
      INTEGER                         :: num_metgrid_levels
      INTEGER                         :: ITS,ITE,IDS,IDE,IMS,IME
      INTEGER                         :: JTS,JTE,JDS,JDE,JMS,JME
      INTEGER                         :: KTS,KTE,KDS,KDE,KMS,KME
      CHARACTER(LEN=19)               :: current_date
      CHARACTER(LEN=19)               :: sdate
      CHARACTER(LEN=1)                :: gtype
      INTEGER                         :: numsoil
      INTEGER                         :: fcstlength
      LOGICAL                         :: first_time
      END TYPE input_vars
      TYPE(input_vars):: gridin

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      TYPE output_vars

! 0D vars
      REAL                       :: PT
      REAL                       :: PTSGM
      REAL                       :: PDTOP
      REAL                       :: DLMD
      REAL                       :: DPHD
      REAL                       :: TPH0D
      REAL                       :: TLM0D
      REAL                       :: dom_cen_lat(21)
      REAL                       :: dom_cen_lon(21)
      INTEGER                    :: VCOORD
      INTEGER                    :: LM
      INTEGER                    :: LPT2
      INTEGER                    :: i_parent_start_loc
      INTEGER                    :: j_parent_start_loc
      INTEGER                    :: parent_grid_ratio_out(21)
      INTEGER                    :: parent_id_out(21)
      LOGICAL                    :: direct_temp
      LOGICAL                    :: GLOBAL
      LOGICAL                    :: do_clouds
      LOGICAL                    :: spectral 
      LOGICAL                    :: ncep_processing
      LOGICAL                    :: boundary_flux
      LOGICAL                    :: no_flux 
      LOGICAL                    :: use_igbp

!      REAL                       :: DT
!      REAL                       :: DT_NUM
!      REAL                       :: DT_DENOM

! 1D vars
      REAL, POINTER, DIMENSION(:)     :: ETA1, ETA2
      REAL, POINTER, DIMENSION(:)     :: AETA1, AETA2
      REAL, POINTER, DIMENSION(:)     :: DETA1, DETA2
      REAL, POINTER, DIMENSION(:)     :: NL_ETALEVS, coord_levs
      REAL, POINTER, DIMENSION(:)     :: DZSOIL
      REAL, POINTER, DIMENSION(:)     :: SLDPTH
      REAL, POINTER, DIMENSION(:)     :: RTDPTH

! 2D vars
      REAL, allocatable,  DIMENSION(:,:)   :: PD
      REAL, POINTER, DIMENSION(:,:)   :: SM
      REAL, POINTER, DIMENSION(:,:)   :: FIS
      REAL, POINTER, DIMENSION(:,:)   :: STDVTOPO
      REAL, POINTER, DIMENSION(:,:)   :: ALBEDO
      REAL, POINTER, DIMENSION(:,:)   :: ALBASE
      REAL, POINTER, DIMENSION(:,:)   :: EPSR
      REAL, POINTER, DIMENSION(:,:)   :: MXSNAL
      REAL, POINTER, DIMENSION(:,:)   :: TSK
      REAL, POINTER, DIMENSION(:,:)   :: SST
      REAL, POINTER, DIMENSION(:,:)   :: SNO
      REAL, POINTER, DIMENSION(:,:)   :: WEASD
      REAL, POINTER, DIMENSION(:,:)   :: SI
      REAL, POINTER, DIMENSION(:,:)   :: SICE
      REAL, POINTER, DIMENSION(:,:)   :: TG
      REAL, POINTER, DIMENSION(:,:)   :: CMC
      REAL, POINTER, DIMENSION(:,:)   :: SR
      REAL, POINTER, DIMENSION(:,:)   :: USTAR
      REAL, POINTER, DIMENSION(:,:)   :: Z0
      REAL, POINTER, DIMENSION(:,:)   :: VEGFRA
!      REAL, POINTER, DIMENSION(:,:)   :: GLAT
!      REAL, POINTER, DIMENSION(:,:)   :: GLON
      INTEGER, POINTER, DIMENSION(:,:):: ISLTYP, IVGTYP

! 3D vars
      REAL, POINTER, DIMENSION(:,:,:) :: U
      REAL, POINTER, DIMENSION(:,:,:) :: V
      REAL, POINTER, DIMENSION(:,:,:) :: Q
      REAL, POINTER, DIMENSION(:,:,:) :: CWM
      REAL, POINTER, DIMENSION(:,:,:) :: T
      REAL, POINTER, DIMENSION(:,:,:) :: STC
      REAL, POINTER, DIMENSION(:,:,:) :: SMC
      REAL, POINTER, DIMENSION(:,:,:) :: SH2O
      REAL, POINTER, DIMENSION(:,:,:) :: pint_out

      END TYPE output_vars

      TYPE work_vars

      REAL, POINTER, DIMENSION(:,:,:) :: P3D_OUT
      REAL, POINTER, DIMENSION(:,:,:) :: P3DV_OUT
      REAL, POINTER, DIMENSION(:,:,:) :: P3DV_IN
      REAL, POINTER, DIMENSION(:,:,:) :: QTMP
      REAL, POINTER, DIMENSION(:,:,:) :: QTMP2
      REAL, POINTER, DIMENSION(:,:,:) :: model_Z
!      REAL, POINTER, DIMENSION(:,:,:) :: pint_out
!cloud fields
      REAL, POINTER, DIMENSION(:,:,:) :: RWMR_input
      REAL, POINTER, DIMENSION(:,:,:) :: CLWMR_input
      REAL, POINTER, DIMENSION(:,:,:) :: SNMR_input
      REAL, POINTER, DIMENSION(:,:,:) :: CICE_input
      REAL, POINTER, DIMENSION(:,:,:) :: RIMEF_input
!cloud fields

      END TYPE work_vars

      TYPE boundary_vars
! 0D vars
      REAL                    :: TBOCO_BDY
      INTEGER                 :: LNSH, LNSV


      REAL, POINTER, DIMENSION(:,:) :: pd_new_n, pd_new_s
      REAL, POINTER, DIMENSION(:,:) :: pd_old_n, pd_old_s
      REAL, POINTER, DIMENSION(:,:) :: pd_new_w, pd_new_e
      REAL, POINTER, DIMENSION(:,:) :: pd_old_w, pd_old_e

      REAL, POINTER, DIMENSION(:,:,:) :: t_new_n, t_new_s
      REAL, POINTER, DIMENSION(:,:,:) :: t_old_n, t_old_s
      REAL, POINTER, DIMENSION(:,:,:) :: t_new_w, t_new_e
      REAL, POINTER, DIMENSION(:,:,:) :: t_old_w, t_old_e

      REAL, POINTER, DIMENSION(:,:,:) :: q_new_n, q_new_s
      REAL, POINTER, DIMENSION(:,:,:) :: q_old_n, q_old_s
      REAL, POINTER, DIMENSION(:,:,:) :: q_new_w, q_new_e
      REAL, POINTER, DIMENSION(:,:,:) :: q_old_w, q_old_e

      REAL, POINTER, DIMENSION(:,:,:) :: cwm_new_n, cwm_new_s
      REAL, POINTER, DIMENSION(:,:,:) :: cwm_old_n, cwm_old_s
      REAL, POINTER, DIMENSION(:,:,:) :: cwm_new_w, cwm_new_e
      REAL, POINTER, DIMENSION(:,:,:) :: cwm_old_w, cwm_old_e

      REAL, POINTER, DIMENSION(:,:,:) :: u_new_n, u_new_s
      REAL, POINTER, DIMENSION(:,:,:) :: u_old_n, u_old_s
      REAL, POINTER, DIMENSION(:,:,:) :: u_new_w, u_new_e
      REAL, POINTER, DIMENSION(:,:,:) :: u_old_w, u_old_e

      REAL, POINTER, DIMENSION(:,:,:) :: v_new_n, v_new_s
      REAL, POINTER, DIMENSION(:,:,:) :: v_old_n, v_old_s
      REAL, POINTER, DIMENSION(:,:,:) :: v_new_w, v_new_e
      REAL, POINTER, DIMENSION(:,:,:) :: v_old_w, V_old_e

      REAL, POINTER, DIMENSION(:,:,:)   :: pdb_n, pdb_s, pdb_w, pdb_e
      REAL, POINTER, DIMENSION(:,:,:,:) :: tb_n, tb_s, tb_w, tb_e
      REAL, POINTER, DIMENSION(:,:,:,:) :: qb_n, qb_s, qb_w, qb_e
      REAL, POINTER, DIMENSION(:,:,:,:) :: cwmb_n, cwmb_s, cwmb_w, cwmb_e
      REAL, POINTER, DIMENSION(:,:,:,:) :: ub_n, ub_s, ub_w, ub_e
      REAL, POINTER, DIMENSION(:,:,:,:) :: vb_n, vb_s, vb_w, vb_e

      REAL, POINTER, DIMENSION(:,:,:)   :: pdb_n_g, pdb_s_g, pdb_w_g, pdb_e_g
      REAL, POINTER, DIMENSION(:,:,:,:) :: tb_n_g, tb_s_g, tb_w_g, tb_e_g
      REAL, POINTER, DIMENSION(:,:,:,:) :: qb_n_g, qb_s_g, qb_w_g, qb_e_g
      REAL, POINTER, DIMENSION(:,:,:,:) :: cwmb_n_g, cwmb_s_g, cwmb_w_g, cwmb_e_g
      REAL, POINTER, DIMENSION(:,:,:,:) :: ub_n_g, ub_s_g, ub_w_g, ub_e_g
      REAL, POINTER, DIMENSION(:,:,:,:) :: vb_n_g, vb_s_g, vb_w_g, vb_e_g

      END TYPE boundary_vars 

	END MODULE module_data
