      subroutine sumfln(flnev,flnod,lat1s,plnev,plnod,
     x                  nvars,ls_node,latl2,
     x                  workdim,nvarsdim,four_gr,
     x                  ls_nodes,max_ls_nodes,
     x                  lats_nodes,global_lats,
     x                  lats_node,ipt_lats_node,lon_dims,dimg,
     x                  lons_lat,londi,latl)
cc
! program log
! 2011 02 20 : henry jaung, updated to work with NDSL.
cc
      use gfs_dyn_resol_def
      use gfs_dyn_layout1
      use gfs_dyn_mpi_def
!#ifndef IBM
!     USE omp_lib
!#endif
      implicit none
cc
      integer lat1s(0:jcap),latl2
cc
      integer              nvars
      real(kind=kind_evod) flnev(len_trie_ls,2,nvars)
      real(kind=kind_evod) flnod(len_trio_ls,2,nvars)
cc
      real(kind=kind_evod) plnev(len_trie_ls,latl2)
      real(kind=kind_evod) plnod(len_trio_ls,latl2)
cc
      integer              ls_node(ls_dim,3)
cc
!cmr  ls_node(1,1) ... ls_node(ls_max_node,1) : values of L
!cmr  ls_node(1,2) ... ls_node(ls_max_node,2) : values of jbasev
!cmr  ls_node(1,3) ... ls_node(ls_max_node,3) : values of jbasod
cc
cc    local scalars
cc    -------------
cc
      integer              j, k, l, lat, lat1, n
cc
cc    local arrays
cc    ------------
cc
      real(kind=kind_evod) apev(latl2,2,nvars)
      real(kind=kind_evod) apod(latl2,2,nvars)
      integer              num_threads, num_parthds
      integer              nvar_thread_max
      integer              nvar_1,nvar_2
      integer              thread
ccxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
cc
      integer              nvarsdim,latl
      integer              workdim
      integer              id,dimg,londi
      integer lats_node,ipt_lats_node
      integer lon_dims(latgd)
cc
cc
      real(kind=kind_evod) four_gr( londi*nvarsdim, workdim )
cc
      integer              ls_nodes(ls_dim,nodes)
      integer              max_ls_nodes(nodes)
      integer                lats_nodes(nodes)
cjfe  integer        global_lats(latg+2*jintmx+2*nypt*(nodes-1))
      integer        global_lats(latl+dimg)
      integer               lons_lat(latl)
cc
      real(kind=kind_mpi) works(2,nvars,ls_dim*workdim,nodes)
      real(kind=kind_mpi) workr(2,nvars,ls_dim*workdim,nodes)
cc
      integer                    kpts(1+jcap)
      integer                    kptr(1+jcap)
      integer              sendcounts(1+jcap)
      integer              recvcounts(1+jcap)
      integer                 sdispls(1+jcap)
cc
      integer              ierr,ilat,ipt_ls,i3
      integer              lat_r,lmax,ls_r,lval,i,ii,jj
      integer              ndisp,node,nvar
 
cc    for OMP buffer copy
      integer ilat_list(nodes)
cc
cc    statement functions
cc    -------------------
cc
      integer              indlsev,jbasev
      integer              indlsod,jbasod
cc
      include 'function2'
cc
      real(kind=kind_evod) cons0     !constant
      real(kind=kind_evod) cons1     !constant
cc
      cons0 = 0.d0     !constant
      cons1 = 1.d0     !constant
cc
ccxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
cc
      call countperf(0,8,0.)
cc
      call f_hpmstart(18,"000_sumfln_sum2eo")
!#ifdef IBM
      num_threads=NUM_PARTHDS()
!#else
!     num_threads=omp_get_num_threads()
!#endif
      nvar_thread_max=(nvars+num_threads-1)/num_threads
      kpts   = 0
cc
      do j = 1, ls_max_node   ! start of do j loop #####################
cc
cc
cc
              l=ls_node(j,1)
         jbasev=ls_node(j,2)
         jbasod=ls_node(j,3)
cc
         lat1 = lat1s(l)
      if ( kind_evod .eq. 8 ) then !------------------------------------
cjfe!$omp+shared(jbasev,jbasod,l,lat1,nvar_thread_max,nvars)
!$omp parallel do shared(apev,apod,flnev,flnod,plnev,plnod)
!$omp+private(thread,nvar_1,nvar_2)
         do thread=1,num_threads   ! start of thread loop ..............
            nvar_1=(thread-1)*nvar_thread_max+1
            nvar_2=min(nvar_1+nvar_thread_max-1,nvars)

            if( nvar_2 >= nvar_1 ) then       ! hmhj

cc          compute the even and odd components
cc          of the fourier coefficients
cc
cc          compute the sum of the even real      terms for each level
cc          compute the sum of the even imaginary terms for each level
cc
            call dgemm('t','n',latl2-lat1+1, 2*(nvar_2-nvar_1+1),
     &                 (jcap+3-l)/2,cons1,     !constant
     &                 plnev(indlsev(l,l),lat1),len_trie_ls,
     &                 flnev(indlsev(l,l),1,nvar_1),len_trie_ls,cons0,     !constant
     &                 apev(lat1,1,nvar_1), latl2)
cc
cc          compute the sum of the odd real      terms for each level
cc          compute the sum of the odd imaginary terms for each level
cc
            call dgemm('t','n',latl2-lat1+1, 2*(nvar_2-nvar_1+1),
     &                 (jcap+2-l)/2,cons1,     !constant
     &                 plnod(indlsod(l+1,l),lat1), len_trio_ls,
     &                 flnod(indlsod(l+1,l),1,nvar_1),len_trio_ls,cons0,     !constant
     &                 apod(lat1,1,nvar_1), latl2)

            endif             ! hmhj
cc
         enddo   ! end of thread loop ..................................
      else !------------------------------------------------------------
cjfe!$omp+shared(jbasev,jbasod,l,lat1,nvar_thread_max,nvars)
!$omp parallel do shared(apev,apod,flnev,flnod,plnev,plnod)
!$omp+private(thread,nvar_1,nvar_2)
         do thread=1,num_threads   ! start of thread loop ..............
            nvar_1=(thread-1)*nvar_thread_max+1
            nvar_2=min(nvar_1+nvar_thread_max-1,nvars)

            if( nvar_2 >= nvar_1 ) then               ! hmhj

cc          compute the even and odd components
cc          of the fourier coefficients
cc
cc          compute the sum of the even real      terms for each level
cc          compute the sum of the even imaginary terms for each level
cc
            call sgemm('t','n',latl2-lat1+1, 2*(nvar_2-nvar_1+1),
     &                 (jcap+3-l)/2,cons1,     !constant
     &                 plnev(indlsev(l,l),lat1),len_trie_ls,
     &                 flnev(indlsev(l,l),1,nvar_1),len_trie_ls,cons0,     !constant
     &                 apev(lat1,1,nvar_1), latl2)
cc
cc          compute the sum of the odd real      terms for each level
cc          compute the sum of the odd imaginary terms for each level
cc
            call sgemm('t','n',latl2-lat1+1, 2*(nvar_2-nvar_1+1),
     &                 (jcap+2-l)/2,cons1,     !constant
     &                 plnod(indlsod(l+1,l),lat1), len_trio_ls,
     &                 flnod(indlsod(l+1,l),1,nvar_1),len_trio_ls,cons0,     !constant
     &                 apod(lat1,1,nvar_1), latl2)

            endif             ! hmhj
cc
         enddo   ! end of thread loop ..................................
      endif !-----------------------------------------------------------
cc
ccxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
cc
cc       compute the fourier coefficients for each level
cc       -----------------------------------------------
cc
         ilat_list(1) = 0
         do node = 1, nodes - 1
           ilat_list(node+1) = ilat_list(node) + lats_nodes(node)
         end do
 
!$omp parallel do private(node,jj,ilat,lat,ipt_ls,nvar)
         do node=1,nodes
            do jj=1,lats_nodes(node)
               ilat = ilat_list(node) + jj
               lat=global_lats(ilat)
               ipt_ls=min(lat,latl-lat+1)
               if ( ipt_ls .ge. lat1s(ls_nodes(j,me+1)) ) then
                  kpts(node)=kpts(node)+1
cc
                  if ( lat .le. latl2 ) then
cc
cc                   northern hemisphere
                     do nvar=1,nvars
cc
                       works(1,nvar,kpts(node),node)=apev(ipt_ls,1,nvar)
     x                                              +apod(ipt_ls,1,nvar)
                       works(2,nvar,kpts(node),node)=apev(ipt_ls,2,nvar)
     x                                              +apod(ipt_ls,2,nvar)
cc
                     enddo
cc
                  else
cc
cc                   southern hemisphere
                     do nvar=1,nvars
cc
                       works(1,nvar,kpts(node),node)=apev(ipt_ls,1,nvar)
     x                                              -apod(ipt_ls,1,nvar)
                       works(2,nvar,kpts(node),node)=apev(ipt_ls,2,nvar)
     x                                              -apod(ipt_ls,2,nvar)
cc
                     enddo
                  endif
               endif
            enddo
         enddo
cc
      enddo   ! end of do j loop #######################################
cc
      call f_hpmstop(18)
cc
      call countperf(1,8,0.)
cc
      call f_hpmstart(19,"1st_sumfln")
cc
cc
      kptr = 0
      do node=1,nodes
         do l=1,max_ls_nodes(node)
            lval=ls_nodes(l,node)+1
            do j=1,lats_node
               lat=global_lats(ipt_lats_node-1+j)
               if ( min(lat,latl-lat+1) .ge. lat1s(lval-1) ) then
                  kptr(node)=kptr(node)+1
               endif
            enddo
         enddo
      enddo
cc
cc
      do node=1,nodes
         sendcounts(node) = kpts(node) * 2 * nvars
         recvcounts(node) = kptr(node) * 2 * nvars
            sdispls(node) = (node-1) * 2*ls_dim*workdim*nvars
      end do
cc
      call f_hpmstop(19)
cc
cc
      call f_hpmstart(84,"bar_sumfln_alltoallv")
cc
      call mpi_barrier (mc_comp,ierr)
cc
      call f_hpmstop(84)
cc
cc
      call f_hpmstart(20,"2nd_sumfln_alltoallv")
cc
      call mpi_alltoallv(works,sendcounts,sdispls,mpi_r_mpi,
     x                   workr,recvcounts,sdispls,mpi_r_mpi,
     x                   mc_comp,ierr)
cc
      call f_hpmstop(20)
cc
cjfe  call mpi_barrier (mc_comp,ierr)
cc
cc
      call f_hpmstart(21,"3rd_sumfln")
cc
!$omp parallel do private(j,lat,lmax,nvar,ndisp,lval)
      do j=1,lats_node
         lat = global_lats(ipt_lats_node-1+j)
         lmax = min(jcap,lons_lat(lat)/2)
         if ( (lmax+1)*2+1 .le. lons_lat(lat)+2 ) then
            do nvar=1,nvars
               ndisp = (nvar-1)*lon_dims(j)
               do lval = (lmax+1)*2+1, lons_lat(lat)+2
cc
                  four_gr(            ndisp+lval,j) = cons0    !constant
cc
               enddo
            enddo
         endif
      enddo
cc
      kptr = 0
         id=0
!!
!$omp parallel do private(node,l,lval,j,lat,ndisp,nvar)
      do node=1,nodes
         do l=1,max_ls_nodes(node)
            lval=ls_nodes(l,node)+1
            do j=1,lats_node
               lat=global_lats(ipt_lats_node-1+j)
               if ( min(lat,latl-lat+1) .ge. lat1s(lval-1) ) then
                  kptr(node)=kptr(node)+1
                  do nvar=1,nvars
cc
                     ndisp = (nvar-1)*lon_dims(j)
cc
cjfe                 four_gr(ndisp+2*lval-1,j) =
                     four_gr(ndisp+2*lval-1+id,j) =
     x                 workr(1,nvar,kptr(node),node)
cc
cjfe                 four_gr(ndisp+2*lval  ,j) =
                     four_gr(ndisp+2*lval+id,j) =
     x                 workr(2,nvar,kptr(node),node)
cc
                  enddo
               endif
            enddo
         enddo
      enddo
cc
      call f_hpmstop(21)
cc
      return
      end
      subroutine sumflna(flnev,flnod,lat1s,plnev,plnod,
     x                  nvars,ls_node,latl2,
     x                  workdim,nvarsdim,four_gr,
     x                  ls_nodes,max_ls_nodes,
     x                  lats_nodes,global_lats,
     x                  lats_node,ipt_lats_node,lon_dims,dimg,
     x                  lons_lat,londi,latl)
      use gfs_dyn_resol_def
      use gfs_dyn_layout1
      use gfs_dyn_mpi_def
!#ifndef IBM
!     USE omp_lib
!#endif
      implicit none
      integer lat1s(0:jcap),latl2
      real(kind=kind_evod) flnev(len_trie_ls,2*nvars)
      real(kind=kind_evod) flnod(len_trio_ls,2*nvars)
      real(kind=kind_evod) plnev(len_trie_ls,latl2)
      real(kind=kind_evod) plnod(len_trio_ls,latl2)
      integer              nvars
      integer              ls_node(ls_dim,3)
      integer              j, k, l, lat, lat1, n
      real(kind=kind_evod) apev(2*nvars,latl2)! real(kind=ki
      real(kind=kind_evod) apod(2*nvars,latl2)! real(kind=ki
      integer              num_threads, num_parthds
      integer              nvar_thread_max
      integer              nvar_1,nvar_2
      integer              thread
      integer              nvarsdim,latl
      integer              workdim
      integer              id,dimg,londi
      integer lats_node,ipt_lats_node
      integer lon_dims(latgd)
      real(kind=kind_evod) four_gr( londi*nvarsdim, workdim )
      integer              ls_nodes(ls_dim,nodes)
      integer              max_ls_nodes(nodes)
      integer                lats_nodes(nodes)
      integer        global_lats(latl+dimg)
      integer               lons_lat(latl)
      real(kind=kind_mpi) works(2,nvars,ls_dim*workdim,nodes)
      real(kind=kind_mpi) workr(2,nvars,ls_dim*workdim,nodes)
      integer                    kpts(1+jcap)
      integer                    kptr(1+jcap)
      integer              sendcounts(1+jcap)
      integer              recvcounts(1+jcap)
      integer                 sdispls(1+jcap)
      integer              ierr,ilat,ipt_ls,i3
      integer              lat_r,lmax,ls_r,lval,i,ii,jj
      integer              ndisp,node,nvar
      integer ilat_list(nodes)
      integer              indlsev,jbasev
      integer              indlsod,jbasod
      include 'function2'
      real(kind=kind_evod) cons0     !constant
      real(kind=kind_evod) cons1     !constant
      cons0 = 0.d0     !constant
      cons1 = 1.d0     !constant
!#ifdef IBM
      num_threads=num_parthds()
!#else
!     num_threads=omp_get_num_threads()
!#endif
      nvar_thread_max=(nvars+num_threads-1)/num_threads
      kpts   = 0
      do j = 1, ls_max_node   ! start of do j loop #####################
              l=ls_node(j,1)
         jbasev=ls_node(j,2)
         jbasod=ls_node(j,3)
         lat1 = lat1s(l)
      if ( kind_evod .eq. 8 ) then !------------------------------------
!$omp parallel do shared(apev,apod,flnev,flnod,plnev,plnod)
!$omp+private(thread,nvar_1,nvar_2)
         do thread=1,num_threads   ! start of thread loop ..............
            nvar_1=(thread-1)*nvar_thread_max+1
            nvar_2=min(nvar_1+nvar_thread_max-1,nvars)

            if( nvar_2 >= nvar_1 ) then               ! hmhj

            call dgemm(
     &                 't',
     &                 'n',
     &                 2*(nvar_2-nvar_1+1),
     &                 latl2-lat1+1,
     &                 (jcap+3-l)/2,
     &                 cons1,     !constant
     &                 flnev(indlsev(l,l),2*nvar_1-1),
     &                 len_trie_ls,
     &                 plnev(indlsev(l,l),lat1),
     &                 len_trie_ls,
     &                 cons0,     !constant
     &                 apev(2*nvar_1-1,lat1),
     &                 2*nvars
     &                 )
            call dgemm(
     &                 't',
     &                 'n',
     &                 2*(nvar_2-nvar_1+1),
     &                 latl2-lat1+1,
     &                 (jcap+2-l)/2,
     &                 cons1,     !constant
     &                 flnod(indlsod(l+1,l),2*nvar_1-1),
     &                 len_trio_ls,
     &                 plnod(indlsod(l+1,l),lat1),
     &                 len_trio_ls,
     &                 cons0,     !constant
     &                 apod(2*nvar_1-1,lat1),
     &                 2*nvars
     &                 )
            endif             ! hmhj

         enddo   ! end of thread loop ..................................
      else !------------------------------------------------------------
!$omp parallel do shared(apev,apod,flnev,flnod,plnev,plnod)
!$omp+private(thread,nvar_1,nvar_2)
         do thread=1,num_threads   ! start of thread loop ..............
            nvar_1=(thread-1)*nvar_thread_max+1
            nvar_2=min(nvar_1+nvar_thread_max-1,nvars)
         enddo   ! end of thread loop ..................................
      endif !-----------------------------------------------------------
         ilat_list(1) = 0
         do node = 1, nodes - 1
           ilat_list(node+1) = ilat_list(node) + lats_nodes(node)
         end do
!$omp parallel do private(node,jj,ilat,lat,ipt_ls,nvar)
         do node=1,nodes
            do jj=1,lats_nodes(node)
               ilat = ilat_list(node) + jj
               lat=global_lats(ilat)
               ipt_ls=min(lat,latl-lat+1)
               if ( ipt_ls .ge. lat1s(ls_nodes(j,me+1)) ) then
                  kpts(node)=kpts(node)+1
                  if ( lat .le. latl2 ) then
                     do nvar=1,nvars
                       works(1,nvar,kpts(node),node) =
     x                 apev(2*nvar-1,ipt_ls)+apod(2*nvar-1,ipt_ls)
                       works(2,nvar,kpts(node),node) =
     x                 apev(2*nvar,ipt_ls)+apod(2*nvar,ipt_ls)
                     enddo
                  else
                     do nvar=1,nvars
                       works(1,nvar,kpts(node),node) =
     x                 apev(2*nvar-1,ipt_ls)-apod(2*nvar-1,ipt_ls)
                       works(2,nvar,kpts(node),node) =
     x                 apev(2*nvar,ipt_ls)-apod(2*nvar,ipt_ls)
                     enddo
                  endif
               endif
            enddo
         enddo
      enddo   ! end of do j loop #######################################
      kptr = 0
      do node=1,nodes
         do l=1,max_ls_nodes(node)
            lval=ls_nodes(l,node)+1
            do j=1,lats_node
               lat=global_lats(ipt_lats_node-1+j)
               if ( min(lat,latl-lat+1) .ge. lat1s(lval-1) ) then
                  kptr(node)=kptr(node)+1
               endif
            enddo
         enddo
      enddo
      do node=1,nodes
         sendcounts(node) = kpts(node) * 2 * nvars
         recvcounts(node) = kptr(node) * 2 * nvars
            sdispls(node) = (node-1) * 2*ls_dim*workdim*nvars
      end do
      call mpi_barrier (mc_comp,ierr)
      call mpi_alltoallv(works,sendcounts,sdispls,mpi_r_mpi,
     x                   workr,recvcounts,sdispls,mpi_r_mpi,
     x                   mc_comp,ierr)
!$omp parallel do private(j,lat,lmax,nvar,ndisp,lval)
      do j=1,lats_node
         lat = global_lats(ipt_lats_node-1+j)
         lmax = min(jcap,lons_lat(lat)/2)
         if ( (lmax+1)*2+1 .le. lons_lat(lat)+2 ) then
            do nvar=1,nvars
               ndisp = (nvar-1)*lon_dims(j)
               do lval = (lmax+1)*2+1, lons_lat(lat)+2
                  four_gr(            ndisp+lval,j) = cons0    !constant
               enddo
            enddo
         endif
      enddo
      kptr = 0
         id=0
!$omp parallel do private(node,l,lval,j,lat,ndisp,nvar)
      do node=1,nodes
         do l=1,max_ls_nodes(node)
            lval=ls_nodes(l,node)+1
            do j=1,lats_node
               lat=global_lats(ipt_lats_node-1+j)
               if ( min(lat,latl-lat+1) .ge. lat1s(lval-1) ) then
                  kptr(node)=kptr(node)+1
                  do nvar=1,nvars
                     ndisp = (nvar-1)*lon_dims(j)
                     four_gr(ndisp+2*lval-1+id,j) =
     x                 workr(1,nvar,kptr(node),node)
                     four_gr(ndisp+2*lval+id,j) =
     x                 workr(2,nvar,kptr(node),node)
                  enddo
               endif
            enddo
         enddo
      enddo
      return
      end
