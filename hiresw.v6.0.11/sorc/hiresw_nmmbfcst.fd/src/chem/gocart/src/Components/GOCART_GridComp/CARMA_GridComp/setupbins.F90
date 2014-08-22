! Colarco, May 21, 2007
! sed command to replace F77 comments with F90: sed 's/^c/\!/g'
! F90-version of original CARMA setupbins.f routine (see comments below from
! original routine header).

      subroutine setupbins ( carma, rc )

!     types
      use carma_types_mod

      implicit none

      integer, intent(out) :: rc

!     Local declarations
      integer :: ielem, ibin, i, j, ix, iy, iz, ie, ig, igrp, jgrp

#include "carma_globaer.h"

      rc = 0

#ifdef DEBUG
       write(*,*) '+ setupbins'
#endif
!
!
!  @(#) setupbins.f  Jensen  Oct-1995
!  This routine evaluates the derived mapping arrays and sets up
!  the particle size bins.
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
!  Define formats
!
    1 format(a,':  ',12i6)
    2 format(a,':  ',i6)
    3 format(a,':  ',f12.2)
    4 format(a,':  ',12f12.2)
    6 format(a,':  ',1p12e12.3)
    5 format(/,'Particle grid structure (setupbins):')
!
!-------------------------------------------------------------------------------
!
!  Announce entry to this routine
!
!      if( DEBUG ) write(LUNOPRT,'(/,a)') 'Enter setupbins'
!
!  Determine the total number of elements 
!
!
!
!
!  Determine which elements are particle number concentrations
!  <ienconc(igroup)> is the element corresponding to particle number 
!  concentration in group <igroup>
!
      igrp = 0
      do ielem = 1, NELEM
        if( itype(ielem) .eq. I_INVOLATILE .or.  &
            itype(ielem) .eq. I_VOLATILE )then

          igrp = igrp + 1
          ienconc(igrp) = ielem
        endif
      enddo
      
      if( igrp .gt. NGROUP )then
        write(LUNOPRT,'(/,a)') 'bad itype array'
        stop 1
      endif

!
!  Determine which group each element belongs to
!  i.e., <igelem(ielem)> is the group to which element <ielem> belongs
!
      igrp = 0
      do ielem = 1, NELEM
        if( itype(ielem) .eq. I_INVOLATILE .or. &
            itype(ielem) .eq. I_VOLATILE )then
          igrp = igrp + 1
        endif
        igelem(ielem) = igrp
      enddo
!
!
!  Determine how many cores are in each group <ncore>.
!  The core elements in a group are given by <icorelem(1:ncore,igroup)>.
!
!  Also evaluate whether or not second moment is used <if_sec_mom> for each group.
!
      ielem = 0

      do igrp = 1, NGROUP

        ncore(igrp) = 0
        if_sec_mom(igrp) = .false.
        imomelem(igrp) = 0

        do j = 1, nelemg(igrp)

          ielem = ielem + 1

          if( itype(ielem) .eq. I_COREMASS .or. &
              itype(ielem) .eq. I_VOLCORE )then

            ncore(igrp) = ncore(igrp) + 1
            icorelem(ncore(igrp),igrp) = ielem

          elseif( itype(ielem) .eq. I_CORE2MOM )then

            if_sec_mom(igrp) = .true.
            imomelem(igrp) = ielem

          endif

        enddo
      enddo
!
!
!  Particle mass densities (NXYZ*NBIN for each group) -- the user might want
!  to modify this (this code segment does not appear in setupaer subroutine
!  because <igelem> is not defined until this subroutine).
!

    if( .not. do_hostmodel )then
      do ig = 1,NGROUP
        ie = ienconc(ig)
        do ibin = 1,NBIN
          do iz = 1,NZ
           do iy = 1, NY
            do ix = 1, NX
             rhop(ix,iy,iz,ibin,ig) = rhoelem(ie)
!  Set initial density of all hydrometeor groups to 1 such that nucleation
!  mapping arrays are calculated correctly.
             if( itype(ie) .ne. I_INVOLATILE ) then
               rhop(ix,iy,iz,ibin,ig) = 1000._f
             endif
            enddo
           enddo
          enddo
        enddo
      enddo
    endif
!
!
!  Handle the creation of the particle radius/mass bin spacing
!  If not doing the host model we have specified rmin and rmrat, and
!  the rest follows.  Alternatively, if we are doing the host model
!  we have possibly filled in rmrat and rmin and can also use the 
!  following code.

      if(.not.do_hostmodel) r(:,:) = -1._f
      call carma_bins(    NBIN, NGROUP, rhop(NX,NY,NZ,:,:), &
                          rmin, rmrat, rmassmin, &
                          r, dr, rmass, rmassup, rlow, rup, &
                          dm, vol, rc )

!
!
!  Evaluate differences between valuse of <rmass> in different bins.
!
      do igrp = 1, NGROUP
       do jgrp = 1, NGROUP
        do i = 1, NBIN
         do j = 1, NBIN
           diffmass(i,igrp,j,jgrp) = rmass(i,igrp) - rmass(j,jgrp)
         enddo
        enddo
       enddo
      enddo
!
!
!  Report some initialization values
!
      write(LUNOPRT,5)
      write(LUNOPRT,2) 'NGROUP ',NGROUP
      write(LUNOPRT,2) 'NELEM  ',NELEM
      write(LUNOPRT,2) 'NBIN   ',NBIN
      write(LUNOPRT,6) 'Massmin',(rmassmin(i),i=1,NGROUP)
      write(LUNOPRT,4) 'Mrat   ',(rmrat(i),i=1,NGROUP)
      write(LUNOPRT,1) 'nelemg ',(nelemg(i),i=1,NGROUP)
      write(LUNOPRT,1) 'itype  ',(itype(i),i=1,NELEM)
      write(LUNOPRT,1) 'ienconc',(ienconc(i),i=1,NGROUP)
      write(LUNOPRT,1) 'igelem ',(igelem(i),i=1,NELEM)
      write(LUNOPRT,1) 'ncore  ',(ncore(i),i=1,NGROUP)
!
!
!  Return to caller with particle grid initialized
!
      return
      end
