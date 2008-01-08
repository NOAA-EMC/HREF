!STARTOFREGISTRYGENERATEDINCLUDE 'inc/state_subtypes.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
TYPE fdob_type
integer                                  :: domain_tot
integer                                  :: domain_init
integer                                  :: ieodi
integer                                  :: iwtsig
integer                                  :: nstat
integer                                  :: ktaur
integer                                  :: sn_maxcg
integer                                  :: we_maxcg
integer                                  :: sn_end
integer                                  :: levidn(max_domains)
real                                     :: ds_cg
real                                     :: window
real                                     :: rtlast
real                                     :: datend
real                                     :: rinfmn
real                                     :: rinfmx
real                                     :: pfree
real                                     :: dcon
real                                     :: dpsmx
real                                     :: tfaci
real                                     :: xn
real      ,DIMENSION(:,:)     ,POINTER   :: varobs
real      ,DIMENSION(:,:)     ,POINTER   :: errf
real      ,DIMENSION(:)       ,POINTER   :: timeob
real      ,DIMENSION(:)       ,POINTER   :: nlevs_ob
real      ,DIMENSION(:)       ,POINTER   :: lev_in_ob
real      ,DIMENSION(:)       ,POINTER   :: plfo
real      ,DIMENSION(:)       ,POINTER   :: elevob
real      ,DIMENSION(:)       ,POINTER   :: rio
real      ,DIMENSION(:)       ,POINTER   :: rjo
real      ,DIMENSION(:)       ,POINTER   :: rko
END TYPE fdob_type
!ENDOFREGISTRYGENERATEDINCLUDE
