      module namelist_dynamics_def

!
! program lot
! 06 Apr 2012:    Henry Juang add some options for NDSL
! 05 Oct 2012:    Jun Wang    add sigio_out
! 02 Apr 2014:    Jun Wang    add dfilevs
! Mar 07 2014     Weiyu Yang - add wam_ipe_coupling, height_dependent_g
!
      use gfs_dyn_machine
      implicit none
      
      integer nsres,nsout,igen,ngptc,levwgt(2),k2o,nsout_hf
      integer dfilevs
      real(kind=kind_evod) fhrot,fhmax,fhout,fhres,fhini,fhdfi
      real(kind=kind_evod) filta,ref_temp,sl_epsln,cdamp(2)
     &,                    ref_pres,fhout_hf,fhmax_hf
      real(kind=kind_evod) hdif_fac,hdif_fac2,slrd0,wgtm(2)
      REAL(KIND = kind_evod) :: phigs,phigs_d
      logical lsfwd,ldfi_spect, shuff_lats_a,reshuff_lats_a
      logical,target :: hybrid,gen_coord_hybrid
      logical zflxtvd,explicit

      logical nemsio_in, nemsio_out, sigio_out
      logical reduced_grid, semi_implicit_temp_profile
      logical mass_dp, process_split
!
      logical herm_x,  herm_y,  herm_z,  lin_xyz, wgt_cub_lin_xyz,lin_xy
     &,       semilag, redgg_a, gg_tracers
     &,       wgt_cub_lin_xyz_trc
     &,       time_extrap_etadot,settls_dep3ds,settls_dep3dg
     &,       iter_one_no_interp,cont_eq_opt1,opt1_3d_qcubic

      logical ndslfv
! hmhj idea add
      logical lsidea
! WAM IPE coupling flags.
!------------------------
      logical :: wam_ipe_coupling, height_dependent_g

      character*20 ens_nam
!
      end module namelist_dynamics_def
