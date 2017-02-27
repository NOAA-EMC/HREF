      subroutine setlats_a_slg(lats_nodes_a,global_lats_a,
     &                         iprint,lonsperlat)
!
      use gfs_dyn_resol_def , only : latg,lonf
      use gfs_dyn_layout1   , only : nodes
!     use gfs_dyn_mpi_def   , only : icolor,liope
!
      implicit none
!
      integer              lats_nodes_a(nodes)
      integer              global_lats_a(latg)
      integer              iprint,opt,ifin,nodesio
      integer              lonsperlat(latg)
      integer              jcount,jpt,lat,lats_sum,node,i
      integer              ilatpe,ngrptg,ngrptl,ipe,irest,idp
!
      integer,allocatable :: lats_hold(:,:)
!
      allocate ( lats_hold(latg,nodes) )
!
!     iprint = 1
      iprint = 0
      opt    = 1                       ! reduced grid
      if (opt == 2) lonsperlat = lonf  ! full grid
      lats_nodes_a = 0
!     if (liope .and. icolor == 2) then
!       nodesio = 1
!     else
        nodesio = nodes
!     endif
!
      ngrptg = 0
      do lat=1,latg
         do i=1,lonsperlat(lat)
           ngrptg = ngrptg + 1
         enddo
      enddo
!
!   ngrptg contains total number of grid points.
!
!     distribution of the grid

      ilatpe = ngrptg / nodesio
      ngrptl = 0
      ipe    = 0
      irest  = 0
      idp    = 1
      do lat=1,latg
        ifin   = lonsperlat(lat)
        ngrptl = ngrptl + ifin
        if (ngrptl*nodesio <= ngrptg+irest) then
          lats_nodes_a(ipe+1)  = lats_nodes_a(ipe+1) + 1
          lats_hold(idp,ipe+1) = lat
          idp = idp + 1
        else
          ipe = ipe + 1
          if (ipe <= nodesio) lats_hold(1,ipe+1) = lat
          idp    = 2
          irest  = irest + ngrptg - (ngrptl-ifin)*nodesio
          ngrptl = ifin
          lats_nodes_a(ipe+1) = lats_nodes_a(ipe+1) + 1
        endif
      enddo
!!
!!........................................................
!!
      jpt = 0
      do node=1,nodesio
         if ( lats_nodes_a(node) > 0 ) then
            do jcount=1,lats_nodes_a(node)
               global_lats_a(jpt+jcount) = lats_hold(jcount,node)
            enddo
         endif
         jpt = jpt + lats_nodes_a(node)
      enddo
!!
      deallocate (lats_hold)
      if ( iprint .ne. 1 ) return
!!
      jpt=0
      do node=1,nodesio
         if ( lats_nodes_a(node) .gt. 0 ) then
            print 600
            lats_sum=0
            do jcount=1,lats_nodes_a(node)
               lats_sum=lats_sum + lonsperlat(global_lats_a(jpt+jcount))
               print 700, node-1,
     x                    node,    lats_nodes_a(node),
     x                    jpt+jcount, global_lats_a(jpt+jcount),
     x                     lonsperlat(global_lats_a(jpt+jcount)),
     x                    lats_sum
            enddo
         endif
         jpt=jpt+lats_nodes_a(node)
      enddo
!
      print 600
!
  600 format ( ' ' )
!
  700 format (  'setlats  me=', i4,
     x          '  lats_nodes_a(',  i4, ' )=', i4,
     x          '  global_lats_a(', i4, ' )=', i4,
     x          '  lonsperlat=', i5,
     x          '  lats_sum=',   i6 )
!
      return
      end
