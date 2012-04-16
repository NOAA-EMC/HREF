!STARTOFREGISTRYGENERATEDINCLUDE 'inc/i1_decl.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: chklowq
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: moist_tend
#ifndef NO_I1_OLD
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: moist_old
#endif
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_moist)           :: dfi_moist_tend
#ifndef NO_I1_OLD
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_moist)           :: dfi_moist_old
#endif
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_scalar)           :: scalar_tend
#ifndef NO_I1_OLD
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_scalar)           :: scalar_old
#endif
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32,num_dfi_scalar)           :: dfi_scalar_tend
#ifndef NO_I1_OLD
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32,num_dfi_scalar)           :: dfi_scalar_old
#endif
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32,num_chem)           :: chem_tend
#ifndef NO_I1_OLD
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32,num_chem)           :: chem_old
#endif
!ENDOFREGISTRYGENERATEDINCLUDE
