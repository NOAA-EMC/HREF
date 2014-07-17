      module namelist_dynamics_def

!
! program lot
! 06 Apr 2012:    Henry Juang add some options for NDSL
! 05 Oct 2012:    Jun Wang    add sigio_out
!
      use gfs_dyn_machine
      implicit none
      
      integer nsres,nsout,igen,ngptc,num_reduce
      real(kind=kind_evod) fhrot,fhmax,fhout,fhres,fhini,fhdfi
      real(kind=kind_evod) filta,ref_temp
      real(kind=kind_evod) hdif_fac,hdif_fac2,slrd0
      logical lsfwd,ldfi_spect
      logical shuff_lats_a,reshuff_lats_a
      logical,target :: hybrid,gen_coord_hybrid
      logical zflxtvd,explicit


      logical nemsio_in, nemsio_out, sigio_out
      logical reduced_grid, semi_implicit_temp_profile
      logical mass_dp, process_split
      logical ndslfv
! hmhj idea add
      logical lsidea

      character*20 ens_nam
!
      end module namelist_dynamics_def
