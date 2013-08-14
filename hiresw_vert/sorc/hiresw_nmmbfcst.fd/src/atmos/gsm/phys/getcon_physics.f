      subroutine getcon_physics(
     x                  n3,n4,
     x                  lats_nodes_r,global_lats_r,
     x                  lonsperlar,
     x                  lats_nodes_ext,global_lats_ext,
     x                  colat1,idrt)
cc
      use resol_def,            ONLY: latr, jintmx, nypt, lonrx, lonr,
     &                                latr2                             
      use layout1,              ONLY: me, nodes, lon_dims_r, 
     &                                lon_dims_ext, ipt_lats_node_r, 
     &                                ipt_lats_node_ext,
     &                                lats_node_r_max, lats_node_ext,
     &                                lats_node_r, lats_dim_r, 
     &                                lats_dim_ext
      use gg_def,               ONLY: colrad_r, wgt_r, wgtcs_r, rcs2_r, 
     &                                sinlat_r, coslat_r
      use namelist_physics_def, ONLY: shuff_lats_r
!jw      use mpi_def,              ONLY: icolor, liope
      USE machine,              ONLY: kind_dbl_prec, kind_evod
      implicit none
cc
!!
      integer              i,j,k,l,lat,lev
      integer              n,n3,n4,idrt
cc
cc
cc
cc
      integer               lats_nodes_r(nodes)
      integer              global_lats_r(latr)
      integer                 lonsperlar(latr)
cc
      integer                lats_nodes_ext(nodes)
      integer        global_lats_ext(latr+2*jintmx+2*nypt*(nodes-1))
cc
cc
      real(kind=kind_dbl_prec) ,allocatable:: colrad_dp(:)
      real(kind=kind_dbl_prec) ,allocatable::    wgt_dp(:)
      real(kind=kind_dbl_prec) ,allocatable::  wgtcs_dp(:)
      real(kind=kind_dbl_prec) ,allocatable::   rcs2_dp(:)
cc
      integer              iprint,locl,node,nodesio
      integer              len_trie_ls_nod
      integer              len_trio_ls_nod
cc
      integer              indev
      integer              indod
cc
      integer              indlsev,jbasev
      integer              indlsod,jbasod
cc
      integer gl_lats_index
      integer global_time_sort_index_r(latr)
      integer nodes_tmp
cc
      include 'function2'
cc
      real(kind=kind_evod) global_time_r(latr)
cc
!     logical shuffled
cc
      real(kind=kind_evod) colat1
cc
      real(kind=kind_evod) cons0,cons0p5,cons0p92      !constant
      real(kind=kind_evod) cons1                       !constant
cc
cc
      cons0    =   0.d0       !constant
      cons0p5  =   0.5d0      !constant
      cons0p92 =   0.92d0     !constant
      cons1    =   1.d0       !constant
cc
      iprint = 0
cc
!     print 100, jcap, levs
100   format (1h0,'getcon physics ',i3,i3,' created january 2008')
cc
cc
!     write(0,*)' in getcon physicsb: lonsperlar ',lonsperlar
      do lat = 1, latr2
         lonsperlar(latr+1-lat) = lonsperlar(lat)
      end do
!     write(0,*)' in getcon physics: lonsperlar ',lonsperlar
cc
!jw
      idrt=4                                            !INTEGER DATA REPRESENTATION TYPE :4 Gaussian ,0:LATLON
!jw      if (liope) then
!jw         if (icolor.eq.2) then
!jw           nodesio=1
!jw         else
!jw           nodesio=nodes
!jw         endif
!jw      else
         nodesio=nodes
!jw      endif
c
!jw      if (nodesio .eq. 1 .and. nodes .eq. 1
!jw     .    .or. (nodes .eq. 2 .and. nodesio .eq. 1) ) then
      if (nodesio .eq. 1 .and. nodes .eq. 1) then
         shuff_lats_r = .false.
!       print*,' NO SHUFFLING WITH 1 COMPUTE TASK - nodes = ',nodes
      endif
!!
cc
!      print *,' getcon shuff_lats_r ',shuff_lats_r
      if (shuff_lats_r) then
 
        gl_lats_index = 0
        global_lats_r = -1

        do lat = 1,latr
         global_time_r(lat) = lonsperlar(lat)
        enddo
c
cmy sort the lat times in descending order
c
        call sortrx(latr,-global_time_r,global_time_sort_index_r)
        if (iprint .eq. 1)
     &   print *,' getcon_physics after sortrx for r index = ',
     &    global_time_sort_index_r
 
cmy input lat time index in descending order
cmy output global_lats_r and lats_nodes_r (gl_lats_index temp)
cmy
        gl_lats_index = 0
cmy
        nodes_tmp = nodes
!jw        if (liope .and. icolor .eq. 2) nodes_tmp = nodes-1
!jw        if (liope .and. icolor .eq. 2) lats_nodes_r(nodes)=0
        do node=1,nodes_tmp
!          print *,' node gl_lats_index ',gl_lats_index
           call get_lats_node_r( node-1, global_lats_r,
     &                 lats_nodes_r(node),
     &                 gl_lats_index,global_time_sort_index_r,iprint)
!           if (me+1 .eq. node .and. iprint .eq. 1)
           if (me+1 .eq. node)
     &     print *,' node lats_nodes_r(node) ',lats_nodes_r(node)
        enddo
        call setlats_r_ext_shuff(lats_nodes_r,lats_nodes_ext,
     &           global_lats_r, global_lats_ext,iprint,lonsperlar)
       else

        call setlats_r(lats_nodes_r,lats_nodes_ext,global_lats_r,
     &                 global_lats_ext,iprint,lonsperlar)

      endif ! shuff_lats_r

!     write(0,*)' in getcon physics: lonsperlar ',lonsperlar
!
!      print *,' getcon physics: lats_nodes_r',lats_nodes_r
      iprint = 0
cc
      lats_dim_r=0
      do node=1,nodes
         lats_dim_r = max(lats_dim_r,lats_nodes_r(node))
      enddo
!      print *,' getcon physics: lats_dim_r',lats_dim_r
cc
      lats_dim_ext=0
      do node=1,nodes
             lats_dim_ext =
     &   max(lats_dim_ext, lats_nodes_ext(node), lats_nodes_r(node))
      enddo
!      print *,' getcon physics: lats_dim_ext',lats_dim_ext
cc
      lats_node_r = lats_nodes_r(me+1)
!      print *,' getcon physics: lats_node_r',lats_node_r
!
      lats_node_ext = lats_nodes_ext(me+1)
!      print *,' getcon physics: lats_node_ext',lats_node_ext
c
      lats_node_r_max=0
      do i=1,nodes
        lats_node_r_max=max(lats_node_r_max,lats_nodes_r(i))
      enddo
!      print *,' getcon physics: lats_node_r_max',lats_node_r_max
c
cc
      ipt_lats_node_r=1
      ipt_lats_node_ext=1
 
!     if ( .not. shuffled .and. me .gt. 0 ) then
      if ( .not. shuff_lats_r .and. me .gt. 0 ) then
         do node=1,me
          ipt_lats_node_ext = ipt_lats_node_ext + lats_nodes_ext(node)
         enddo
      endif
c
      if ( me .gt. 0 ) then
         do node=1,me
            ipt_lats_node_r = ipt_lats_node_r + lats_nodes_r(node)
         enddo
      endif
!jw      if (liope .and. icolor .eq. 2) then
!jw            ipt_lats_node_r = 1
!jw            ipt_lats_node_ext = 1
!jw      endif
!     print *,' getcon physics: ipt_lats_node_ext',ipt_lats_node_ext
!     print *,' getcon physics: ipt_lats_node_r',ipt_lats_node_r
c
cc
      n3    = 51
      n4    = 52
cc
cc
      iprint = 0
!     if ( me .eq. 0 ) iprint = 1
!
      if ( kind_evod .eq. 8 ) then !------------------------------------

           call glats_physics(latr2,colrad_r(1:latr2),wgt_r,wgtcs_r,
     &                        rcs2_r,iprint)
!!
           colat1=colrad_r(1)
!!
           do i=latr2+1,latr
              colrad_r(i)=colrad_r(latr+1-i)
           enddo
cc
      else !------------------------------------------------------------
           allocate  ( colrad_dp(latr2) )
           allocate  (    wgt_dp(latr2) )
           allocate  (  wgtcs_dp(latr2) )
           allocate  (   rcs2_dp(latr2) )
cc
           call glats_physics(latr2,colrad_dp,wgt_dp,wgtcs_dp,rcs2_dp,
     &                        iprint)
!!
           colat1=colrad_dp(1)
!!
           do i=1,latr2
              colrad_r(i) = colrad_dp(i)
                 wgt_r(i) =    wgt_dp(i)
               wgtcs_r(i) =  wgtcs_dp(i)
                rcs2_r(i) =   rcs2_dp(i)
           enddo
cc
           do i=latr2+1,latr
              colrad_r(i) = colrad_dp(latr+1-i)
           enddo
cc
           deallocate  ( colrad_dp )
           deallocate  (    wgt_dp )
           deallocate  (  wgtcs_dp )
           deallocate  (   rcs2_dp )
cc
      endif !-----------------------------------------------------------
cc
!     print *,' getcon physics: colrad_r',colrad_r
cc
cc
      do j=1,latr
        if (j.le.latr2) then
          sinlat_r(j) = cos(colrad_r(j))
        else
          sinlat_r(j) = -cos(colrad_r(j))
        endif
        coslat_r(j) = sqrt(1. E 0 -sinlat_r(j)*sinlat_r(j))
      enddo
cc
!     print *,' getcon physics: sinlat_r',sinlat_r
!     print *,' getcon physics: coslat_r',coslat_r
cc
cc
      do j=1,lats_node_r
         lat = global_lats_r(ipt_lats_node_r-1+j)
         if ( lonsperlar(lat) .eq. lonr ) then
            lon_dims_r(j) = lonrx
         else
            lon_dims_r(j) = lonsperlar(lat) + 2
         endif
      enddo
cc
!     print *,' getcon physics: lon_dims_r',lon_dims_r
cc
!     if (.not. shuffled) then
!     do j=1,lats_node_ext
!        lat = global_lats_ext(ipt_lats_node_ext-1+j)
!        if ( lonsperlar(lat) .eq. lonr ) then
!           lon_dims_ext(j) = lonrx
!        else
!           lon_dims_ext(j) = lonsperlar(lat) + 1+2*nxpt+1
!        endif
!     enddo
!     endif
cc
!     print *,' end of getcon physics '
      return
      end
