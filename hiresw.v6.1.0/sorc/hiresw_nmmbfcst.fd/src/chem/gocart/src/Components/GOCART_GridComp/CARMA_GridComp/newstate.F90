! Colarco, May 14, 2007
! sed command to replace F77 comments with F90: sed 's/^c/\!/g'
! F90-version of original CARMA newstate.f routine (see comments below from
! original routine header).

      subroutine newstate ( carma, rc )

!     types
      use carma_types_mod

      implicit none

      integer, intent(out) :: rc

!  @(#) newstate.f  McKie  Oct-1995
!  This routine manages the calculations that update state variables
!  of the model with new values at the current simulation time.

!  Include global constants and variables

#include "carma_globaer.h"

#ifdef DEBUG
      write(*,*) '+ newstate'
#endif

!  Calculate changes due to horizontal transport
!
!      if( do_ew .or. do_ns )then
!        call horizont
!      endif


!  Calculate changes due to vertical transport

      if( do_vtran )then
        call vertical ( carma, rc )
      endif

#undef PRC
#ifdef PRC
!
!  Calculate transport forcings for parcel model
!
      if( do_parcel )then
        call parcel
      endif
!
!
!  Calculate new saturation ratios for use in nsubsteps.f
!
      do ixyz = 1,NXYZ
        pt = ptc3(ixyz) / rhoa3(ixyz)
        if( do_thermo )then
          t3(ixyz) = pt * ( p3(ixyz) / PREF )**rkappa
        endif
        call supersat
      enddo


!  Calculate the changes in concentrations and supersaturation
!  due to transport
!
      if( do_ew .or. do_ns .or. do_vtran .or. do_parcel )then

        do ixyz = 1,NXYZ

          d_ptc(ixyz) = ptc3(ixyz) - ptcl(ixyz)

          do igas = 1,NGAS
            d_gc(ixyz,igas) = gc3(ixyz,igas) - gcl(ixyz,igas)
          enddo

          do ielem = 1,NELEM
            do ibin = 1,NBIN
              d_pc(ixyz,ibin,ielem) = pc3(ixyz,ibin,ielem) -
     $                                pcl(ixyz,ibin,ielem)
            enddo
          enddo

        enddo
!
!
!  Reset concentrations to their (pre-advection) values from the
!  beginning of the time-step
!
        do ixyz = 1,NXYZ

          ptc3(ixyz) = ptcl(ixyz)
          pt = ptc3(ixyz) / rhoa3(ixyz)
          if( do_thermo )then
            t3(ixyz) = pt * ( p3(ixyz) / PREF )**rkappa
          endif

          do igas = 1,NGAS
            gc3(ixyz,igas) = gcl(ixyz,igas)
          enddo

          do ielem = 1,NELEM
            do ibin = 1,NBIN
              pc3(ixyz,ibin,ielem) = pcl(ixyz,ibin,ielem)
            enddo
          enddo

        enddo

      else

        do ixyz = 1,NXYZ

          d_ptc(ixyz) = 0.

            do igas = 1,NGAS
            d_gc(ixyz,igas) = 0.
          enddo

          do ielem = 1,NELEM
            do ibin = 1,NBIN
              d_pc(ixyz,ibin,ielem) = 0.
            enddo
          enddo

        enddo

      endif

#endif

!  Calculate changes in particle concentrations due to microphysical
!  processes, part 1.  (potentially slower microphysical calcs)
!  All spatial points are handled by one call to this routine.

      call microslow( carma, rc )

#undef JAS
#ifdef JAS

!  Save current timestep used for processes so far
!
      dtime_save = dtime
!
!
!  Initialize diagnostics for substeping
!
      max_ntsub = 0
      avg_ntsub = 0.
!
!
!  Visit each of the spatial grid points
!
      do iz = 1,NZ
        do iy = 1,NY
          do ix = 1,NX
!
!
!  Compute linearized spatial grid pt indices for horiz 2-D & overall 3-D
!   (Global vars <ix>, <iy>, <iz>, <ixy>, <ixyz> used by various microphysical routines)
!
            ixy = NX * ( iy - 1 ) + ix 
            ixyz = NXY * ( iz - 1 ) + ixy
!
!
!  Compute or specify number of sub-timestep intervals for current spatial point
!  (Could be same for all spatial pts, or could vary as a function of location)
!
            ntsubsteps = minsubsteps
            call nsubsteps
            max_ntsub = max( max_ntsub, ntsubsteps )
            avg_ntsub = avg_ntsub + float(ntsubsteps)/float(NXYZ)
!
!
!  Compute sub-timestep time interval for current spatial grid point
!
            dtime = dtime_save / float( ntsubsteps )
!
!
!  Do sub-timestepping for current spatial grid point
!
            do isubtim = 1,ntsubsteps
!
!
!  Apply the fractional advective forcing to specify the values of
!  concentrations at this sub-timestep
!
              ptc3(ixyz) = ptc3(ixyz) + d_ptc(ixyz)/ntsubsteps

              do igas = 1,NGAS
                gc3(ixyz,igas) = gc3(ixyz,igas) +
     $                           d_gc(ixyz,igas)/ntsubsteps
              enddo

              do ielem = 1,NELEM
                do ibin = 1,NBIN
                  pc3(ixyz,ibin,ielem) = pc3(ixyz,ibin,ielem) + 
     $                           d_pc(ixyz,ibin,ielem)/ntsubsteps
!                 
!    
!  Prevent particle concentrations from dropping below SMALL_PC
!  (In some cases, this prevention of zero concentrations
!  may be needed to prevent numerical problems)
!
!                  call smallconc(ibin,ielem)

                enddo
              enddo
!
!
!  Update temperature
!
              pt = ptc3(ixyz) / rhoa3(ixyz)
              if( do_thermo )then
                t3(ixyz) = pt * ( p3(ixyz) / PREF )**rkappa
              endif
!
!
!  Calculate changes in particle concentrations for current spatial point
!  due to microphysical processes, part 2.  (faster microphysical calcs)
!
              call microfast
!
!
!  Go do next sub-timestep
!
            enddo
!
!
!  Go do next spatial grid point
!
          enddo
        enddo
      enddo
!
!
!  Report substep diagnostics
!
      if( itime .eq. 0 ) write(LUNOSTEP,1)
      write(LUNOSTEP,2) itime, time, max_ntsub, avg_ntsub
!
!
!  Restore normal timestep
!
      dtime = dtime_save
!
!
!  Hydrostatically update atmospheric profiles
!
      if( .not. do_parcel )then
        call hydrostat
      endif

! for #ifdef JAS

#endif

!  Return to caller with new state computed 
!

      rc = 0

      return
      end
