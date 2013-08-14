      subroutine getpwatptot (psg,ttg,rqg,
     &    global_lats_a,lonsperlat, pwat,ptot,ptrc)
!!
!! program log
!!   2007      Henry H. Juang
!!   20100205  J. WANG - this routins is moved from input_fields, it
!!                       computes pwat and ptot
!!   20100825  Sarah Lu - modified to compute tracer global sum
!!
      use gfs_dyn_resol_def
      use gfs_dyn_layout1
      use gfs_dyn_gg_def
      use gfs_dyn_vert_def
      use gfs_dyn_date_def
      use namelist_dynamics_def
      use gfs_dyn_coordinate_def 
      use gfs_dyn_tracer_const
      use gfs_dyn_physcons, fv => con_fvirt, rerth => con_rerth,
     &              grav => con_g,  cp => con_cp , rd => con_rd
      use gfs_dyn_tracer_config, only: glbsum                     !glbsum
      implicit none
!
      integer,intent(in) ::   global_lats_a(latg)
      integer,intent(in) ::   lonsperlat(latg)
!
      REAL(KIND=KIND_GRID),intent(in) :: psg (lonf,lats_node_a_max)
      REAL(KIND=KIND_GRID),intent(in) :: ttg (lonf,lats_node_a_max,levs)
      REAL(KIND=KIND_GRID),intent(in) :: rqg (lonf,lats_node_a_max,levh)
      REAL(KIND=KIND_GRID),intent(out) :: pwat (lonf,lats_node_a)
      REAL(KIND=KIND_GRID),intent(out) :: ptot (lonf,lats_node_a)
      REAL(KIND=KIND_GRID),intent(out) :: ptrc (lonf,lats_node_a,ntrac)        !glbsum

!
!local vars
      REAL(KIND=KIND_GRID) work   (lonf)
      REAL(KIND=KIND_GRID) tki    (lonf,levp1)
      REAL(KIND=KIND_GRID) prsi   (lonf,levp1)
!
      real(kind=kind_evod)   tfac(lonf,levs), sumq(lonf,levs), tkrt0
!
      integer              i,j,k,kk, nn, nnl
      integer              l,lan,lat
      integer              lons_lat
!
      real(kind=kind_evod), parameter :: qmin=1.0e-10, rkappa = cp / rd
      real(kind=kind_evod), parameter :: one=1.0, pa2cb=0.001
!
!--------------------------------------------------------------------
!
      do lan=1,lats_node_a
        lat = global_lats_a(ipt_lats_node_a-1+lan)
        lons_lat = lonsperlat(lat)
!
! save surface pressure as mass for dry mass adjuctment
! from get model ps (log surface pressure or surface pressure)
        if (gen_coord_hybrid) then   ! Ps is the prognostic variable
          do i=1,lons_lat
            ptot (i,lan) =  psg(i,lan)
          enddo
        else                         ! ln(Ps) is the prognostic variable
          do i=1,lons_lat
            ptot(i,lan) = exp(psg(i,lan)) 
          enddo
        endif
!       call mymaxmin(psg(1,lan),lons_lat,lonf,1,' psg in com to mdl')
!
! get pressure at interfaces for pwat 
        if (gen_coord_hybrid) then  
          tki = 0.0
          do k=2,levs
            do i=1,lons_lat
              tkrt0 = (ttg(i,lan,k-1)+ttg(i,lan,k))
     &                           /(thref(k-1)+thref(k))
              tkrt0 = tkrt0**rkappa
              tki (i,k)=ck5(k)*tkrt0
            enddo
          enddo
          do k=1,levp1
            do i=1,lons_lat
              prsi(i,k)  = ak5(k)+bk5(k)*ptot(i,lan)+tki(i,k) 
            enddo
          enddo
        else if (hybrid) then
          do k=1,levp1
            kk=levp1+1-k
            do i=1,lons_lat
              prsi(i,k)  = ak5(kk)+bk5(kk)*ptot(i,lan)
            enddo
          enddo
        else
          do k=1,levp1
            do i=1,lons_lat
              prsi(i,k)  = si(k)*ptot(i,lan)
            enddo
          enddo
        endif                      
!
! get pwat (total vertical integrated water)
        do i=1,lons_lat
          pwat(i,lan) = 0.0
        enddo
        do k=1,levs
          do i=1,lons_lat
            work(i) = 0.0
          enddo
          if( ncld.gt.0 ) then
            do nn=ntcw,ntcw+ncld-1
              nnl = (nn-1)*levs
              do i=1,lons_lat
                work(i) = work(i) + rqg(i,lan,nnl+k)
              enddo
            enddo
          endif
          do i=1,lons_lat
            pwat(i,lan) = pwat(i,lan) + (prsi(i,k)-prsi(i,k+1))
     &                                * (rqg(i,lan,k) + work(i))
          enddo
        enddo

!
! get ptrc (tracer global sum)                                   !glbsum
!
        if( glbsum ) then                                        !glbsum
          do nn = 1, ntrac                                       !glbsum
            nnl = (nn-1)*levs                                    !glbsum
            do i=1,lons_lat                                      !glbsum
             ptrc(i,lan,nn) = 0.0                                !glbsum
             do k=1,levs                                         !glbsum
               ptrc(i,lan,nn) = ptrc(i,lan,nn) +                 !glbsum
     &         (prsi(i,k)-prsi(i,k+1))*rqg(i,lan,nnl+k)          !glbsum
             enddo                                               !glbsum
            enddo                                                !glbsum
          enddo                                                  !glbsum
        endif                                                    !glbsum

!
!       call mymaxmin(rqg(1,lan,1),lons_lat,lonf,1,' rqg in com to mdl')
!       call mymaxmin(pwat(1,lan),lons_lat,lonf,1,' pwat in com to mdl')
      enddo
!
!!
      return
      end
