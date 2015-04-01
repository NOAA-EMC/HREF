      subroutine NDFDgrid(veg_nam_ndfd,tnew,dewnew,unew,vnew, &
      qnew,pnew,topo_ndfd,veg_ndfd,gdin,VALIDPT,core,dx)
    use constants
    use grddef
    use aset2d
    use aset3d 
    use rdgrib
    
    REAL, INTENT(INOUT) :: TNEW(:,:),DEWNEW(:,:),UNEW(:,:),VNEW(:,:),PNEW(:,:)
    REAL, INTENT(INOUT) :: QNEW(:,:)
    REAL, INTENT(INOUT) :: VEG_NAM_NDFD(:,:),TOPO_NDFD(:,:),VEG_NDFD(:,:)
    LOGICAL, INTENT(IN) :: VALIDPT(:,:)
    TYPE (GINFO)        :: GDIN
 
    character(len=4)  :: core

    REAL, ALLOCATABLE   :: EXN(:,:) 
    REAL, ALLOCATABLE   :: ROUGH_MOD(:,:)
    REAL, ALLOCATABLE   :: TTMP(:,:),DTMP(:,:),UTMP(:,:),VTMP(:,:)
!    REAL, ALLOCATABLE   :: SFCHTNEW(:,:)

!    LOGICAL*1,   ALLOCATABLE   :: MASK(:)
!    REAL,        ALLOCATABLE   :: GRID(:)
    INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200)
      real exn0,exn1, wsp
      integer nmod(2)
      integer i,j, ierr,k,ib,jb, ivar,ix,iy
      integer ibuf, ia,ja,iw,jw,id,n_rough_yes,n_rough_no
      integer m_rough_yes,m_rough_no
      real zs,qv,qq,e,enl,dwpt,z6,t6,gam,tsfc,td
      real tddep,td_orig,zdif_max,tup, qvdif2m5m,qv2m
      real qc,qvc,thetavc,uc,vc,ratio,speed,speedc,frac
      real tmean,dz,theta1,theta6
      logical ladjland,lconus,lnest

      REAL(KIND=8) :: btim
      REAL :: time_begin, time_end

 INTERFACE
!    SUBROUTINE vadjust(VALIDPT,U,V,HTOPO,DX,DY,IM,JM)
    SUBROUTINE vadjust(VALIDPT,VEG_NDFD,U,V,HTOPO,DX,DY,IM,JM,gdin)

    use constants
    use grddef
    use aset2d
    use aset3d

    LOGICAL, INTENT(IN) :: VALIDPT(:,:)
    REAL, INTENT(IN) :: VEG_NDFD(:,:)
    REAL, INTENT(INOUT) :: U(:,:),V(:,:)
    REAL, INTENT(IN) :: HTOPO(:,:),DX,DY
    TYPE (GINFO)        :: GDIN
    REAL, ALLOCATABLE   :: UB(:,:),VB(:,:)
    REAL, ALLOCATABLE   :: PHI(:,:,:)
    real HBAR,DXI,DYI,FX,FY,HTOIM1,HTOJM1,HTOIP1,HTOJP1,DHDX,DHDY, &
         DXSQ,DYSQ,DSQ,FACT,ERROR,ERR,EPSI,OVREL,XX,YY
    integer itmax,ii,jj,kk,idir,it
    END SUBROUTINE vadjust
 END INTERFACE



      print *, '***********************************'
      print *, 'Into NDFDgrid'
      print *, '***********************************'

      IM=gdin%IMAX;JM=gdin%JMAX;LM=gdin%KMAX
      ITOT=IM*JM
      ladjland=.false.
      lconus=.false.
      lnest=gdin%lnest

      ALLOCATE (EXN(IM,JM),ROUGH_MOD(IM,JM),STAT=kret)
      ALLOCATE (TTMP(IM,JM),DTMP(IM,JM),STAT=kret)
      ALLOCATE (UTMP(IM,JM),VTMP(IM,JM),STAT=kret)

!  read in 5 km topography
!  changed name for consistency with non-conus region names
!  changed to unit 48 for consistency with other domains
!  CHANGE to read GRIB FILES for non-conus regions 


        write(0,*) 'have core here to use: ', core(1:4)

      if (gdin%region .eq. 'CS') then
        lconus=.TRUE.
        print *, 'read in Binary topo and veg files '
        open (46, file='TOPONDFD', form='unformatted')
        read (46) topo_ndfd
        close (46)
     
!  read in 5 km vegetation for CONUS domain
        open (48, file='LANDNDFD', form='unformatted')
        read (48) veg_ndfd
        close (48)
      else 
        rghlim=0.5
        veglim=0.5
        scale=100.
        ivgid=81
        print* , ' set veglim,rghlim to:  ', veglim,rghlim
        print*, ' gdin%region: ', gdin%region
!       Conus 2.5 km land/veg is grib formatted

        if ( gdin%region .eq. 'CS2P' ) ivgid=225
        if ( gdin%region .eq. 'CONU' ) ivgid=225

      print *, 'READ IN  GRIB  TOPO file'
        JGDS=-1
        CALL RDHDRS(46,47,IGDNUM,GDIN,NUMVAL)
        if (allocated(grid)) deallocate(grid)
        if (allocated(mask)) deallocate(mask)
!        DEALLOCATE(GRID,MASK)
        ALLOCATE (GRID(NUMVAL),MASK(NUMVAL),STAT=kret)
        J=0;JPDS=-1;JPDS(3)=IGDNUM;JPDS(5)=8;JPDS(6)=1
        CALL SETVAR(46,47,NUMVAL,J,JPDS,JGDS,KF,K,KPDS,KGDS,MASK,GRID,topo_ndfd,IRET,ISTAT)

     print *, 'READ IN GRIB LAND COVER file'
        CALL RDHDRS(48,49,IGDNUM,GDIN,NUMVAL)
        J=0;JPDS=-1;JPDS(3)=IGDNUM;JPDS(5)=ivgid;JPDS(6)=1
        CALL SETVAR(48,49,NUMVAL,J,JPDS,JGDS,KF,K,KPDS,KGDS,MASK,GRID,veg_ndfd,IRET,ISTAT)
        print*, ' min, max of veg_ndfd: ', minval(veg_ndfd),maxval(veg_ndfd)
        print*, 'read ivgid: ', ivgid

!        print*, 'veg_ndfd(251,100); ', veg_ndfd(251,100)
!        print*, 'veg_ndfd(253,131); ', veg_ndfd(253,131)

        if (gdin%region .eq. 'CS2P') then 
          lconus=.TRUE.
          where (veg_ndfd.le.0.)veg_ndfd=16.
        endif


        DEALLOCATE(GRID,MASK)
      endif
     
      if(lconus) then 
        rghlim=0.05  !Why different for non-conus ????
        veglim=16.
        scale=1.
        where(veg_nam_ndfd.eq.16.) veg_nam_ndfd = -1.
        where(veg_nam_ndfd.ne.16. .and. veg_nam_ndfd.gt.0.) veg_nam_ndfd = 0.10
        where(veg_nam_ndfd.eq.-1.) veg_nam_ndfd =  0.
      endif

      print *,'NDFDgrid NDFD TOPO: ',MINVAL(topo_ndfd),MAXVAL(topo_ndfd)
      print *,'NDFDgrid NAM TOPO:  ',MINVAL(zsfc),MAXVAL(zsfc)
      print *,'NDFDgrid HGHT:      ',MINVAL(hght),MAXVAL(hght)
      print *,'NDFDgrid NAM Q :    ',MINVAL(q),MAXVAL(q)
      print *,'NDFDgrid NAM Q2:    ',MINVAL(q2),MAXVAL(q2)
      print *,'NDFDgrid NAM T:     ',MINVAL(T),MAXVAL(T)
      print *,'lconus,lnest        ',lconus,lnest

         zdif_max = -1000.
         n_rough_yes=0
         n_rough_no =0
!C ****************************************************************
! -- Now let's start reducing to NDFD topo elevation.
!C ****************************************************************
        where (zsfc .lt. 0.) zsfc=0.0
!        allocate(sfchtnew(gdin%imax,gdin%jmax))
!        sfchtnew = topo_ndfd
        tnew=spval;qnew=spval
        dewnew=spval;unew=spval;vnew=spval
        pnew=spval

        do 120 j=1,jm
        do 120 i=1,im
        if (.not. validpt(i,j)) goto 120
        exn(i,j) = cpd_p*(psfc(i,j)/P1000)**rovcp_p
! ---   z = surface elevation
          zs = zsfc(i,j)

! --- q = specific humidity at 2m from NAM model sfc
! --- dew-point temperature at original sfc
         td_orig=d2(i,j)

! --- dewpoint depression
          tddep = max(0.,t2(i,j) - td_orig )
          qv= q(i,j,1)
          QQ = QV/(1.+QV)
          tp1=T(I,J,1)
          
! --- Base Td on 2m q
          qv = qq/(1.-qq)

! ---   get values at level 6 for lapse rate calculations

          QQ = Q(I,J,6)/(1.+Q(i,j,6))

          exn(i,j) = cpd_p*(pmid(i,j,6)/P1000)**rovcp_p
          T6=T(I,J,6)
          Z1=HGHT(I,J,1)
          Z6=HGHT(I,J,6)
          GAM = (TP1-T6)/(Z6-Z1)

!============================================
          if (topo_ndfd(i,j).le.zs ) then
!============================================
          GAM = MIN(GAMD,MAX(GAM,GAMi))

! --- temperature at NDFD topo
! -- again, use 2m T at NAM regular terrain from similarity
!      theory for derivation of 2m T at topomini elevation
          tsfc = t2(i,j) + (zs-topo_ndfd(i,j))*gam

!  Don't let reduced valley temps be
!     any lower than NAM 2m temp minus 10K.
          tsfc = max(t2(i,j)-10.,tsfc)
!  Can't let valley temps go below NAM dewpoint temps.
          tsfc = max (tsfc,td_orig)

! --- pressure at NDFD topo
          tmean = (tsfc+t2(i,j)) * 0.5
          dz = zs-topo_ndfd(i,j)
          pnew(i,j) = psfc(i,j) * exp(g0_p*dz/(rd_p*tmean))

! --- temperature
          tnew(i,j) = tsfc
    if (i.eq.251.and. j.eq.100)print *,'**tnew(a) tnew, t2, ',validpt(i,j),tnew(i,j), &
     t2(i,j), topo_ndfd(i,j),zs

! Set dewpoint depression to that at original sfc

! --- dew-pt at topomini
          dewnew(i,j) = tsfc - tddep

! --- surface winds
! -- use 10 m wind values derived from similarity theory
!   gsm  use u and v of level 1 or 10m???
          unew(i,j) = u10(i,j)
          vnew(i,j) = v10(i,j)

!============================================
          ELSE if (topo_ndfd(i,j).gt.zs) then
!============================================
! ----  Now only if topo_NDFD is above the NAM model elevation

!        Here, when topo-NDFD > topo-NAM, we allow a small
!        subisothermal lapse rate with slight warming with height.

          GAM = MIN(GAMD,MAX(GAM,GAMsubj))

          DO K=1,LM
           if (hght(i,j,k) .gt. topo_ndfd(i,j)) exit
          ENDDO 

          if (k .eq. 1) then
            zbot=zs
            pbot=psfc(i,j)
            tbot=t2(i,j)
            qbot=q2(i,j)
          else 
            zbot=hght(i,j,k-1)
            pbot=pmid(i,j,k-1)
            tbot=t(i,j,k-1)
            qbot=q(i,j,k-1)
          endif
          frac = (topo_ndfd(i,j)-zbot) / (hght(i,j,k)-zbot)
          exn1 = (pbot/P1000)**rovcp_p
          exn0 = (pmid(i,j,k)/P1000)**rovcp_p
! --- pressure at NDFD topo
          pnew(i,j) = P1000* ((exn1 +frac * (exn0 - exn1)) **cpovr_p)
          thetak=((P1000/PMID(i,j,k))**CAPA)*T(i,j,k)
          thetak1=((P1000/pbot)**CAPA)*tbot  
          thetavc = thetak1+frac * (thetak-thetak1)
          qvc = qbot+frac * (Q(i,j,k)-qbot)
          qc = qvc/(1.+qvc)

! --- temperature
! GSM changed the tup computation, as it appears to give
!   a better-looking product.  need to revisit at some point 
          tup=t2(i,j)+frac*(t(i,j,k)-t2(i,j))

! Is Tup already Temperature for nests ???????  
          if (.not.lconus) &
          tup = thetavc*(pnew(i,j)/P1000)**rovcp_p/(1.+0.6078*qc)
            
!  provisional 2m temp at NDFD topo
          tnew(i,j) = t2(i,j) + (tup-tp1)

! --- Dont let extrapolated temp to be any larger than
!     the value at the NAM terrain level.
!     This will avoid the problem with NDFD temp values
!     being set to be much warmer than NAM 2m temp.

      tsfc=t2(i,j) + (zs-topo_ndfd(i,j))*gam

      if (tnew(i,j) .gt. t2(i,j))  tnew(i,j) = min(tnew(i,j),tsfc)

    if (i.eq.251.and. j.eq.100)print *,'**tnew(b)',validpt(i,j),tnew(i,j), &
     topo_ndfd(i,j),zs

! --- Just use q at NAM 1st level in this case.
!     should use q2, but the values dont look good
!     Obtain Td corresponding to NDFD pres otherwise.
!TEST      qv=q2(i,j)

!---> Alaska, Choose q at 1st level for more realistic output 
!     Also for CONUS....others ???
!TEST      if (gdin%region .eq. 'AK' .or. lnest) qv=q(i,j,1)
       qv=q(i,j,1)

      e=pnew(i,j)/100.*qv/(0.62197+qv)
! --- dew-point temperature at original sfc
      ENL = ALOG(E)
      DWPT = (243.5*ENL-440.8)/(19.48-ENL)
      td = dwpt + 273.15
! --- dewpoint temperature
      dewnew(i,j) = min(td,tnew(i,j))
      if (k .eq. 1) then
        uc = u10(i,j)+frac * (uwnd(i,j,k)-u10(i,j))
        vc = v10(i,j)+frac * (vwnd(i,j,k)-v10(i,j))
      else
        uc = uwnd(i,j,k-1)+frac * (uwnd(i,j,k)-uwnd(i,j,k-1))
        vc = vwnd(i,j,k-1)+frac * (vwnd(i,j,k)-vwnd(i,j,k-1))
      endif

! -- 0.7 factor is a wag at surface effects on wind speed
!     when interpolating from the free atmosphere to
!     the NDFD topo.
!          speedc = 0.7*sqrt(uc*uc+vc*vc)
!          speed = sqrt(uwnd(i,j,1)**2 + vwnd(i,j,1)**2)
!          ratio = max(1.,speedc/(max(0.001,speed)) )
!          unew(i,j) = ratio*(uwnd(i,j,1))
!          vnew(i,j) = ratio*(vwnd(i,j,1))

          unew(i,j) = uc
          vnew(i,j) = vc

!============================================
        END IF
!============================================

120     continue

!       Adjust winds to topography
        dy=dx   ! dx should be passed in from MAIN


        write(0,*) 'call vadjust'
        btim=timef()
        call cpu_time(time_begin)
        write(0,*) 'btim: ', btim

!        call vadjust(validpt,unew,vnew,topo_ndfd,dx,dy,im,jm)
        call vadjust(validpt,veg_ndfd,unew,vnew,topo_ndfd,dx,dy,im,jm,gdin)

        call cpu_time(time_end)
        write(0,*) 'return vadjust'
        write(0,*) 'time for vadjust: ', time_end-time_begin


!============================================
! -- use land mask to get better temps/dewpoint/winds
!      near coastlines.
!    Use nearest neighbor adjustment where NAM
!      land-water mask does not mask NDFD land-water mask 
!============================================

!  create temporary holder for u,v,t,td so that the "real"
!   values don't get shifted around in the adjustment
         ttmp=tnew
         dtmp=dewnew
         utmp=unew
         vtmp=vnew
         rough_mod = veg_nam_ndfd

        print*, ' min/max of rough_mod:  ', minval(rough_mod),maxval(rough_mod)

        do J=JM,1,-JM/45
        write(6,237) (min(rough_mod(I,J),99.),I=1,IM,IM/30)
        enddo

  237   format(35(f3.0,1x))

! ----------------------------------------------------
! -- Adjust to rough_mod iteratively for land to water
! ----------------------------------------------------
       do k=1,15
!       do k=1,0
       nmod = 0
       do j=1,jm
        jm1 = max(1,j-1)
        jp1 = min(jm,j+1)
       do i=1,im
        im1 = max(1,i-1)
        ip1 = min(im,i+1)
        ladjland=.false.

!        if (core(1:4) .eq. 'nmmb' ) then
!         if (rough_mod(I,J) .ne. 0) then
!         endif
!        endif

        if (I .eq. 251 .and. J .eq. 100) then
        print*, 'i,j,lconus, veg_ndfd, veglim: ', i,j,lconus, &
                   veg_ndfd(i,j),veglim
        endif

        if (I .eq. 253 .and. J .eq. 131) then
        print*, 'i,j,lconus, veg_ndfd, veglim: ', i,j,lconus, &
                   veg_ndfd(i,j),veglim
        endif

        if (lconus .and. veg_ndfd(i,j).eq.veglim) ladjland=.true.

        if (.not.lconus .and. veg_ndfd(i,j).lt.veglim) then
        if ( (I .eq. 251 .and. J .eq. 100) .or. (I .eq. 253 .and. J .eq. 131)) then     
        print*, 'adjusting to land: ', I,J
        endif
                ladjland=.true.
        endif

        if (ladjland) then
         if(rough_mod(i,j).gt. rghlim) then
          if(any(rough_mod(im1:ip1,jm1:jp1).lt.rghlim)) then 
             if(validpt(i,j)) then
!        write(0,*) 'I,J,rough_mod(i,j) was(1): ', I,J,rough_mod(i,j)
              rough_mod(i,j) = 0.0
              nmod(1) = nmod(1) + 1
             endif
           end if
          end if
        else
          if(rough_mod(i,j).lt.rghlim) then
           if(any(rough_mod(im1:ip1,jm1:jp1).gt.rghlim)) then
            if (validpt(i,j)) then      ! Added 03-13-13
!        write(0,*) 'I,J,rough_mod(i,j) was(2): ', I,J,rough_mod(i,j)
             rough_mod(i,j) = 0.1*scale
             nmod(2) = nmod(2) + 1
            endif
           end if
          end if
        end if
        end do
        end do
        write (6,*)k,' No. pts changed land-to-water',nmod(1)
        write (6,*)k,' No. pts changed water-to-land',nmod(2)
       end do

        print*, 'modified rough_mod after switching'

        do J=JM,1,-JM/45
        write(6,237) (rough_mod(I,J),I=1,IM,IM/30)
        enddo

       do j=1,jm
       do i=1,im
        if (I .eq. 251 .and. J .eq. 100) then
        write(6,*) 'I,J,veg_NAM, rghlim: ', I,J,veg_nam_ndfd(i,J),rghlim
        write(6,*) 'I,J,rough_mod: ', I,J,rough_mod(i,J)
        endif
         if (veg_nam_ndfd(i,j).gt.rghlim) then
          if (rough_mod(i,j).lt.rghlim) then
! -----------------------------------------------------------------
! -- i.e.  NDFD grid-point is over WATER (per rough_mod)
!          NAM-interp grid-point is over LAND
! -----------------------------------------------------------------
               
          do ibuf=1,10
           ia = max(1,i-ibuf)
           ib = min(im,i+ibuf)
           ja = max(1,j-ibuf)
           jb = min(jm,j+ibuf)
               
            do jw = ja,jb
            do iw = ia,ib
              if (veg_nam_ndfd(iw,jw).lt.rghlim) then  ! should be roughlim ?
                if(validpt(iw,jw)) then
                 unew(i,j) = utmp(iw,jw)
                 vnew(i,j) = vtmp(iw,jw)
                 tnew(i,j) = ttmp(iw,jw)
        if (I .eq. 253 .and. J .eq. 131) then
        print*, 'roughness stuff, I,J,tnew(i,J): ', I,J,tnew(i,j)
        endif
        if (I .eq. 251 .and. J .eq. 100) then
        print*, 'roughness stuff, I,J,tnew(i,J): ', I,J,tnew(i,j)
        endif
                 dewnew(i,j) = dtmp(iw,jw)
                 n_rough_yes = n_rough_yes+1
                 goto 883  ! check if leaves all three loops
                endif
              end if
            end do
            end do
               
          end do
              
 883      continue 
    if (i.eq.251.and. j.eq.100)print *,'z0-n >.05',validpt(i,j),tnew(i,j), &
     topo_ndfd(i,j),zs,veg_nam_ndfd(i,j),rghlim
          end if 
         end if 
! -----------------------------------------------------------------
! -- i.e.  NDFD grid-point is over LAND (per rough_mod)
!          NAM-interp grid-point is over WATER
! -----------------------------------------------------------------
          if (veg_nam_ndfd(i,j).lt.rghlim .and. rough_mod(i,j).gt.rghlim) then
               
           do ibuf=1,10
            ia = max(1,i-ibuf)
            ib = min(im,i+ibuf)
            ja = max(1,j-ibuf)
            jb = min(jm,j+ibuf)
               
            do jw = ja,jb
            do iw = ia,ib
              if (veg_nam_ndfd(iw,jw).gt.rghlim) then
                if(validpt(iw,jw)) then
                 unew(i,j) = utmp(iw,jw)
                 vnew(i,j) = vtmp(iw,jw)
                 tnew(i,j) = ttmp(iw,jw)
                 dewnew(i,j) = dtmp(iw,jw)
                 m_rough_yes = m_rough_yes+1
                 goto 783 !check if leasve all 3 loops (not,i,)
                endif
              end if
            end do
            end do
           end do
 783 continue
    if (i.eq.251.and. j.eq.100)print *,'z0n<.05',validpt(i,j),tnew(i,j), &
     topo_ndfd(i,j),zs,veg_nam_ndfd(i,j),rghlim,rough_mod(i,j)
         end if

       end do
       end do

       where(validpt)  
         where (dewnew.lt.spval) &
         qnew=PQ0/PSFC*EXP(A2*(dewnew-A3)/(dewnew-A4))
       endwhere
       return
       end
