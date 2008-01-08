!STARTOFREGISTRYGENERATEDINCLUDE 'inc/nodyn_i1_decl.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: psim
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: psih
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: wspd
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: gz1oz0
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: br
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: tshltr
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: qshltr
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: pshltr
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: th10
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: q10
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: chklowq
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: th_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: pi_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: p_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: t_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: u_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: v_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: dz8w
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: p8w
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: t8w
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rho_phy
logical   ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: cu_act_flag
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: hol
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: regime
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: moist_tend
#ifndef NO_I1_OLD
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: moist_old
#endif
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_chem)           :: chem_tend
#ifndef NO_I1_OLD
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_chem)           :: chem_old
#endif
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_scalar)           :: scalar_tend
#ifndef NO_I1_OLD
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_scalar)           :: scalar_old
#endif
!ENDOFREGISTRYGENERATEDINCLUDE
