      module gfs_dyn_layout1
      implicit none
      
!
! program log:
! 20110220     henry jaung  add more indexes for mass_dp and ndslfv options
!

      integer           nodes, nodes_comp,nodes_io,
     x                  me,lon_dim_a,
     x                  ls_dim,
     x                  ls_max_node,
     x                  lats_dim_a,
     x                  lats_dim_ext,
     x                  lats_node_a,
     x                  lats_node_a_max,
     x                  lats_node_ext,
     x                  ipt_lats_node_a,
     x                  ipt_lats_node_ext,
     x                  len_trie_ls,
     x                  len_trio_ls,
     x                  len_trie_ls_max,
     x                  len_trio_ls_max,
     x                  me_l_0
cc
      INTEGER ,ALLOCATABLE :: lat1s_a(:),
     .  lon_dims_a(:),lon_dims_ext(:)

!hmhj ndslfv
      integer   lonfull,lonhalf,lonpart,lonlenmax,mylonlen
      integer   latfull,lathalf,latpart,latlenmax,mylatlen
      integer   ndslhvar,ndslvvar

      integer, allocatable :: lonstr(:),lonlen(:)
      integer, allocatable :: latstr(:),latlen(:)
      real, allocatable :: cosglat(:)
      real, allocatable :: gglat(:),gglati(:),gslati(:),ggfact(:,:)
      real, allocatable :: gglon(:),ggloni(:)

      end module gfs_dyn_layout1
