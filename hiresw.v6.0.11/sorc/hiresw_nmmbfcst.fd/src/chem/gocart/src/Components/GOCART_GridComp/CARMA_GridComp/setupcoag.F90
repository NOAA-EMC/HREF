       subroutine setupcoag ( carma, rc )

!     types
      use carma_types_mod

      implicit none

      integer, intent(out) :: rc
 
!  @(#) setupcoag.f  Jensen  Oct-1995
!  This routine sets up mapping arrays and precomputed
!  factors for coagulation.
!
!  This routine requires that <ckernel> has been defined.
!  (setupckern.f must be called before this)
!
!
!  Argument list input:
!    None.
!
!  Argument list output:
!    None.
!
!
!  Include global constants and variables
!
!      include 'globaer.h'
!
!
!      dimension kbin(NGROUP,NGROUP,NGROUP,NBIN,NBIN)
integer, allocatable :: kbin(:,:,:,:,:)
integer :: ios

!--
 
integer :: ielem, isolto, icompto, igto, ig, iepart 
integer :: icompfrom, ic, iecore  
integer :: isolfrom
integer :: igrp, jg, i, j , ipair
real(kind=f) :: rmsum
integer :: ibin
real(kind=f) :: rmkbin
integer :: kb, ncg 
real(kind=f) :: rmk 
integer :: k
real(kind=f) :: pkernl, pkernu
integer :: ix, iy

!--

#include "carma_globaer.h"

!--
!  Announce entry to this routine
!
!      if( DEBUG ) write(LUNOPRT,'(/,a)') 'Enter setupcoag'
#ifdef DEBUG
  write(*,*) '+ setupcoag'
#endif

rc = 0

allocate( kbin( carma%ngroup, carma%ngroup, carma%ngroup, &
                carma%nbin, carma%nbin ), stat=ios)
if(ios /= 0) then
 rc = 1
 return
endif

!-----------------------------------------------------------------
!
!
!  For each element <ielem> and each group <ig>, determine which element in <ig>
!  contributes to production  in <ielem>: <icoagelem(ielem,ig)>.
!  If no elements in <ig> are transfered into element <ielem> during coagulation,
!  then set <icoagelem(ielem,ig)> to 0.
!
      do ielem = 1,NELEM

        isolto = isolelem(ielem)           ! target solute type
        icompto = icomp(ielem)             ! target element compound
        igto = igelem(ielem)               ! target group

        do ig = 1, NGROUP                 ! source group

          iepart = ienconc(ig)            ! source particle number concentration element
!
!
!  If <ielem> is particle number concentration type or <ig> is pure group, then
!  the source element is the particle number concentration element of group <ig>.
!
          if( ( itype(ielem) .eq. I_INVOLATILE .or. &
                itype(ielem) .eq. I_VOLATILE ) .or. &
                ncore(ig) .eq. 0 ) then
            icoagelem(ielem,ig) = iepart
!
!
!  Otherwise, use element compound names to determine which source element matches
!  target core mass element.
!
          else
            icompfrom = icomp(iepart)       ! source element compound
            if( icompfrom .eq. icompto )then
              icoagelem(ielem,ig) = iepart
            else
              do ic = 1,ncore(ig)
                iecore = icorelem(ic,ig)       ! absolute element number of core
                icompfrom = icomp(iecore)    ! source element compound
                if( icompfrom .eq. icompto ) &
                  icoagelem(ielem,ig) = iecore
              enddo
            endif
          endif
!
!
!  Otherwise, use solute types to determine which source element matches
!  target core mass element.
!
!          else
!            isolfrom = isolelem(ienconc(ig))   ! source solute type
!            if( isolfrom .eq. isolto ) then
!              icoagelem(ielem,ig) = ienconc(ig)
!            else
!              do ic = 1,ncore(ig)
!                iecore = icorelem(ic,ig)       ! absolute element number of core
!                isolfrom = isolelem(iecore)    ! source solute type
!                if( isolfrom .eq. isolto )
!     $            icoagelem(ielem,ig) = iecore
!              enddo
!            endif
!          endif
!
!
!  If <ielem> is a core mass type and <ig> is a pure CN group and the
!  solutes don't match, then set <icoagelem> to zero to make sure no
!  coag production occurs.
!
          if( itype(ielem) .eq. I_COREMASS .and. &
              itype(ienconc(ig)).eq. I_INVOLATILE &
              .and. ncore(ig) .eq. 0 ) then
            isolfrom = isolelem(ienconc(ig))
            if( isolfrom .ne. isolto ) then
              icoagelem(ielem,ig) = 0
            endif
          endif

        enddo          ! end of (ig = 1, NGROUP)

      enddo            ! end of (ielem = 1,NELEM)

!
!
!  Calculate lower bin <kbin> which coagulated particle goes into
!  and make sure it is less than <NBIN>+1
!
!  Colliding particles come from group <ig>, bin <i> and group <jg>, bin <j>
!  Resulting particle lands in group <igrp>, between <ibin> and <ibin> + 1
!
      do igrp = 1, NGROUP
        do ig = 1, NGROUP
          do jg = 1, NGROUP
            do i = 1, NBIN
              do j = 1, NBIN

                rmsum = rmass(i,ig) + rmass(j,jg)

                do ibin = 1, NBIN-1
                  if( rmsum .ge. rmass(ibin,igrp) .and. &
                      rmsum .lt. rmass(ibin+1,igrp) ) then
                    kbin(igrp,ig,jg,i,j) = ibin
                  endif
                enddo

                if( rmsum .ge. rmass(NBIN,igrp) ) &
                         kbin(igrp,ig,jg,i,j) = NBIN

              enddo
            enddo
          enddo
        enddo
      enddo
!
!
!  Calculate partial loss fraction
!
!  This fraction is needed because when a particle in bin <i> collides
!  with a particle in bin <j> resulting in a particle whose mass falls
!  between <i> and <i>+1, only partial loss occurs from bin <i>.
!
!  Since different particle groups have different radius grids, this
!  fraction is a function of the colliding groups and the resulting group.
!
      do igrp = 1, NGROUP
        do ig = 1, NGROUP
          do jg = 1, NGROUP

            if( igrp .eq. icoag(ig,jg) ) then

              do i = 1, NBIN
                do j = 1,NBIN

                  volx(igrp,ig,jg,i,j) = 1.
 
                  if(kbin(igrp,ig,jg,i,j).eq.i) then
 
                    rmkbin = rmass(kbin(igrp,ig,jg,i,j),igrp)
                    volx(igrp,ig,jg,i,j) = 1. -                       &
                        (rmrat(igrp)*rmkbin-rmass(i,ig)-rmass(j,jg))  &
                        /(rmrat(igrp)*rmkbin-rmkbin)*                 &
                        rmass(i,ig)/(rmass(i,ig) + rmass(j,jg))

                  endif

                enddo
              enddo
            endif
          enddo
        enddo
      enddo
!
!
!  Calculate mapping functions that specify sets of quadruples
!  (group pairs and bin pairs) that contribute to production
!  in each bin. Mass transfer from <ig,i> to <igrp,ibin> occurs due to
!  collisions between particles in <ig,i> and particles in <jg,j>.
!  2 sets of quadruples must be generated:
!     low: k = ibin and (k != i or ig != igrp)  and  icoag(ig,jg) = igrp
!      up: k+1 = ibin        and  icoag(ig,jg) = igrp
!
!  npair#(igrp,ibin) is the number of pairs in each set (# = l,u)
!  i#, j#, ig#, and jg# are the bin pairs and group pairs in each
!  set (# = low, up)
!
      do igrp = 1, NGROUP
        do ibin = 1, NBIN

          npairl(igrp,ibin) = 0
          npairu(igrp,ibin) = 0

          do ig = 1, NGROUP
          do jg = 1, NGROUP
            do i = 1, NBIN
            do j = 1, NBIN

              kb = kbin(igrp,ig,jg,i,j)
              ncg = icoag(ig,jg)

              if( kb+1.eq.ibin .and. ncg.eq.igrp ) then
                npairu(igrp,ibin) = npairu(igrp,ibin) + 1
                iup(igrp,ibin,npairu(igrp,ibin)) = i
                jup(igrp,ibin,npairu(igrp,ibin)) = j
                igup(igrp,ibin,npairu(igrp,ibin)) = ig
                jgup(igrp,ibin,npairu(igrp,ibin)) = jg
              endif

              if( kb.eq.ibin .and. ncg.eq.igrp .and. &
                (i.ne.ibin .or. ig.ne.igrp) ) then
                npairl(igrp,ibin) = npairl(igrp,ibin) + 1
                ilow(igrp,ibin,npairl(igrp,ibin)) = i
                jlow(igrp,ibin,npairl(igrp,ibin)) = j
                iglow(igrp,ibin,npairl(igrp,ibin)) = ig
                jglow(igrp,ibin,npairl(igrp,ibin)) = jg
              endif

            enddo
            enddo
          enddo
          enddo
        enddo
      enddo
!
!
!  Do some extra debugging reports  (normally commented)
!
#ifdef DEBUG
      write(LUNOPRT,*) ' '
      write(LUNOPRT,*) 'Coagulation group mapping:'
      do ig = 1, NGROUP
        do jg = 1, NGROUP
          write(LUNOPRT,*) 'ig jg icoag = ', ig, jg, icoag(ig,jg)
        enddo
      enddo
      write(LUNOPRT,*) ' '
      write(LUNOPRT,*) 'Coagulation element mapping:'
      do ielem = 1, NELEM
        do ig = 1, NGROUP
          write(LUNOPRT,*) 'ielem ig icoagelem icomp(ielem) = ', &
           ielem, ig, icoagelem(ielem,ig), icomp(ielem)
        enddo
      enddo
      write(LUNOPRT,*) ' '
      write(LUNOPRT,*) 'Coagulation bin mapping arrays'
      do igrp = 1, NGROUP
        do ibin = 1,3
          write(LUNOPRT,*) 'igrp, ibin = ',igrp, ibin
          do ipair = 1,npairl(igrp,ibin)
            write(LUNOPRT,*) 'low:np,ig,jg,i,j ', &
                ipair,iglow(igrp,ibin,ipair), &
            jglow(igrp,ibin,ipair), ilow(igrp,ibin,ipair), &
                  jlow(igrp,ibin,ipair)
          enddo
          do ipair = 1,npairu(igrp,ibin)
            write(LUNOPRT,*) 'up:np,ig,jg,i,j ', &
                ipair,igup(igrp,ibin,ipair), &
            jgup(igrp,ibin,ipair), iup(igrp,ibin,ipair), &
                 jup(igrp,ibin,ipair)
          enddo
        enddo
      enddo
stop
#endif
!
!
!  Calculate variables needed in routine coagp.f
!
      do ix = 1, NX
      do iy = 1, NY

      ckernel => carma%ckernel(ix,iy)%data5d
      pkernel => carma%pkernel(ix,iy)%data7d

      do igrp = 1, NGROUP
        do ig = 1, NGROUP
        do jg = 1, NGROUP

          if( igrp .eq. icoag(ig,jg) ) then

            do i = 1, NBIN
            do j = 1, NBIN

              rmk = rmass(kbin(igrp,ig,jg,i,j),igrp)
              rmsum = rmass(i,ig) + rmass(j,jg)

              do k = 1, NZ

                pkernl = ckernel(k,i,j,ig,jg)* &
                         (rmrat(igrp)*rmk - rmsum) / &
                         (rmrat(igrp)*rmk - rmk)

                pkernu = ckernel(k,i,j,ig,jg)* &
                         (rmsum - rmk) / &
                         (rmrat(igrp)*rmk - rmk)

                if( kbin(igrp,ig,jg,i,j) .eq. NBIN )then
                  pkernl = ckernel(k,i,j,ig,jg)* &
                           rmsum / rmass(NBIN,igrp)
                  pkernu = 0.
                endif
  
                pkernel(k,i,j,ig,jg,igrp,1) = pkernu * &
                                              rmass(i,ig)/rmsum
                pkernel(k,i,j,ig,jg,igrp,2) = pkernl * &
                                              rmass(i,ig)/rmsum
                pkernel(k,i,j,ig,jg,igrp,3) = pkernu * &
                                              rmk*rmrat(igrp)/rmsum
                pkernel(k,i,j,ig,jg,igrp,4) = pkernl * &
                                              rmk/rmsum
                pkernel(k,i,j,ig,jg,igrp,5) = pkernu * &
                                       ( rmk*rmrat(igrp)/rmsum )**2
                pkernel(k,i,j,ig,jg,igrp,6) = pkernl * &
                                              ( rmk/rmsum )**2

              enddo  ! k

            enddo
            enddo

          endif

        enddo
        enddo
      enddo

      enddo  ! iy
      enddo  ! ix

!
!
!  Return to caller with coagulation mapping arrays defined
!
      deallocate(kbin, stat = ios)
      if(ios /= 0) rc = 1

      return
      end
