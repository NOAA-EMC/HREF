! Colarco, May 21, 2007
! sed command to replace F77 comments with F90: sed 's/^c/\!/g'
! F90-version of original CARMA setupaer.f routine (see comments below from
! original routine header).

      subroutine setupaer ( carma, rc )

!     types
      use carma_types_mod

      implicit none

      integer, intent(out) :: rc

!     Local declarations
      integer :: ie1, ie2, iefrom, ieto, igroup, igrp

#include "carma_globaer.h"

      rc = 0

#ifdef DEBUG
       write(*,*) '+ setupaer'
#endif

!
!
!  @(#) setupaer.f  Ackerman  Jan-1996
!  This master routine sets up user-defined mapping arrays and parameters
!  and calls all the other setup routines to calculate other time-independent
!  parameters for aerosol and cloud microphysics.
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
!-------------------------------------------------------------------------------
!
!  Announce entry to this routine
!
!      if( DEBUG ) write(LUNOPRT,'(/,a)') 'Enter setupaer'
!
!-------------------------------------------------------------------------------
!
!
!==Set up particle types and mapping arrays and structure of size grid.
!
!  Sample setup of particle types and the corresponding mapping arrays:
!
!    CN: sulfate particles
!    Cloud hydrometeors: liquid water drops and frogs
!    Core masses: mass of sulfate in water drops and mass of worms in frogs
!
!    group 1   sulfate particles                  element 1   (#/cm^3)
!    group 2   liquid drops                       element 2   (#/cm^3)
!              mass of sulfate in cloud drops     element 3   (g/cm^3)
!    group 3   frogs                              element 4   (#/cm^3)
!              mass of worms in frogs             element 5   (g/cm^3)
!
!    NGROUP  = 3
!    NELEM   = 5
!    nelemg  = (1,2,2)
!    itype   = (0,1,2,1,2)
!    ienconc = (1,2,4)
!    igelem  = (1,2,2,3,3)
!
!
!  Name for each group
!
      groupname(:) = 'sulfate CN'
!      groupname(2) = 'water droplets'
!
!
!  Number of elements in each group (elements in a group can include
!  particle number concentration, mass concentrations of cores, and second
!  moments of core mass distributions).
!
      nelemg(:) = 1
!      nelemg(2) = 2
!
!
!  Name for each element
!
      elemname(:) = 'sulfate CN'
!      elemname(2) = 'water droplets'
!      elemname(3) = 'mass of sulfate in droplets'
!
!
!  This array specifies the composition of each element:
!     I_H2SO4    is sulfuric acid
!     I_WATER    is liquid water
!     I_ICE      is ice water
!     I_MIXEDWAT is mixed phase (ice/liquid) water
!
      icomp(:) = I_H2SO4
!
!
!  This array specifies the type of each element:
!     I_INVOLATILE is CN (involatile) number concentration [#/cm^3]
!     I_VOLATILE   is water droplet (volatile) number concentration [#/cm^3]
!     I_COREMASS   is core mass concentration [g/cm^3]
!     I_VOLCORE    is core mass concentration [g/cm^3] of a volatile core
!     I_CORE2MOM   is core second moment (mass^2) [g^2/cm^3]
!
      itype(:) = I_INVOLATILE
!
!
!  Mass density for each particle element.  For elements of
!  <itype> = I_VOLATILE, <rhoelem> is the density of the shell.
!
!  PRC: the mapping of input tracer array to groups and elements is incomplete
!       at this point.  Keep in mind that rhoelem should in future be specified
!       according to whether a do_hostmodel = .true. run is called for.
      rhoelem(:) = 1380._f
!
!
!  Minimum radius for each group (used below to calculate <rmassmin>) [m]
!
!      rmin(1) = 1.e-8_f
!      rmin(2) = 5.e-7_f
!
!
!  Ratio of particle mass between successive bins (one for each group)
!
!      rmrat(1) = 2.4_f
!      rmrat(2) = 3.1_f

!  Handle the possible initial values of the particle radius/mass bins
!  If doing the host model, one set of either (r, rlow, rup) or
!  (rmin, rmrat) has been specified.  We'll check which in setupbins.
!  Else, pick some initial values.

      if(.not. do_hostmodel) then
       do igrp = 1, NGROUP
        rmin(igrp) = 1.e-8_f
        rmrat(igrp) = 2._f
       enddo
      endif

!
!
!  The values of <ishape> and <eshape> determine particle geometry
!  (one for each group):
!
!    <ishape> = 1: spherical
!    <ishape> = 2: hexagonal prisms or plates
!    <ishape> = 3: circular disks, cylinders, or spheroids
!
      ishape(:) = 1
!
!    <eshape> = particle length/diameter
!
      eshape(:) = 1.
!
!
#ifdef BCOC
      groupname(1) = 'black carbon'
      groupname(2) = 'organic carbon'
      groupname(3) = 'mixed bc/oc'
      nelemg(1) = 1
      nelemg(2) = 1
      nelemg(3) = 2
      elemname(1) = 'black carbon'
      elemname(2) = 'organic carbon'
      elemname(3) = 'mixed bc/oc'
      elemname(4) = 'mass of BC in mixed'
      icomp(1)   = I_BLACKCARBON
      icomp(2)   = I_ORGANICCARBON
      icomp(3)   = I_ORGANICCARBON
      icomp(4)   = I_BLACKCARBON
      itype(1:3) = I_INVOLATILE
      itype(4)   = I_COREMASS
      rhoelem(1) = 1000.
!!      rhoelem(2) = 1800.
!!      rhoelem(3) = 1800.
      rhoelem(2) = 1000.
      rhoelem(3) = 1000.
      rhoelem(4) = 1000.
      ishape(:) = 1
      eshape(:) = 1
#endif
!
#ifdef CLDICE
      groupname(1) = 'bc1'
      groupname(2) = 'bc2'
      nelemg(1) = 2
      nelemg(2) = 2
      elemname(1) = 'bc1'
      elemname(2) = 'mass of OC in bc1'
      elemname(3) = 'bc2'
      elemname(4) = 'mass of OC in bc2'
      icomp(1)   = I_BLACKCARBON
      icomp(2)   = I_ORGANICCARBON
      icomp(3)   = I_BLACKCARBON
      icomp(4)   = I_ORGANICCARBON
      itype(1) = I_INVOLATILE
      itype(2)   = I_COREMASS
      itype(3) = I_INVOLATILE
      itype(4)   = I_COREMASS
      rhoelem(1) = 1000.
      rhoelem(2) = 1000.
      rhoelem(3) = 1000.
      rhoelem(4) = 1000.
      ishape(:) = 1
      eshape(:) = 1
#endif
!
!  Evaluate derived bin mapping arrays and set up the particle size bins.
!
      call setupbins ( carma, rc )
!
!-------------------------------------------------------------------------------
!
!
!==Set options for particle fall velocities.
!
!
!    <ifall> = 0: use constant fall velocity <vf_const>, but still calculate
!                 <vf> for use in Reynolds' number <re>
!            = 1: use calculated value of <vf>
!
      ifall = 1
!
!
!  <vf_const> is only used when <ifall> = 0
!
      vf_const = 0.0_f
!
!
!#ifdef FALLTEST
!      ifall = 0
!      vf_const = 0.01_f
!#endif
#if defined(BCOC) || defined(CLDICE)
      ifall = 0
      vf_const = 0.0_f
#endif

!  Evaluate fall velocities.
!
      call setupvf ( carma, rc )
!
!
!-------------------------------------------------------------------------------
!
!
!  Define mapping arrays and parameters for condensational growth, evaporation,
!  and nucleation.
!
!
!==Set up gas descriptions and mapping arrays used for condensational growth.
!
!
!  Names of gas species
!
!      gasname(1) = 'water vapor'
!
!
!  Molecular weights of gas species
!
!      gwtmol(1) = 0.018_f
!
!
!  Array <igrowgas> maps a particle element to its associated gas for
!  condensational growth/evaporation.
!
!  Set to zero if there is no growth specific to particle element
!  (i.e., use zero if cores do not grow, even if they are a component
!  of a particle group that does grow; use zero for core second moment).
!
!  *** Condensational growth of cores is not presently treated. ***
!
      igrowgas(:) = 0
!      igrowgas(2) = 1
!      igrowgas(3) = 0
!
!
!  If <is_grp_ice> = .true. then the particle group is an ice crystal,
!  else the particle group is liquid (or does not grow).  This array
!  is used to select the appropriate ventilation factors in setupgkern.f.
!
      is_grp_ice(:) = .false.
!      is_grp_ice(2) = .false.
!
!
!  If <is_grp_mixed> = .true. then the particle group is a mixed ice/liquid
!  hydrometeor.  This array is used to select processes (such as core
!  melting) that only occur in mixed particles.
!
      is_grp_mixed(:) = .false.
!      is_grp_mixed(2) = .false.
!
!
!==Set up mapping arrays for nucleation and total evaporation.
!
!
!  Array <inucgas> maps a particle group to its associated gas for nucleation:
!  Nucleation from group <igroup> is associated with gas <inucgas(igroup)>
!  Set to zero if particles are not subject to nucleation.
!
      inucgas(:) = 0
!      inucgas(2) = 0
!
!
!  Nucleation mapping:
!
!  Nucleation transfers particle mass from element <ielem> to element
!  <inuc2elem(i,ielem)>, where <i> ranges from 0 to the number of elements
!  nucleating from <ielem>.
!
      do ie1 = 1,NELEM
        do ie2 = 1,NELEM
          inuc2elem(ie1,ie2) = 0
        enddo
      enddo
!      inuc2elem(1,1) = 3
!
!
!  <inucproc(iefrom,ieto)> specifies what nucleation process nucleates
!  particles from element <ielem> to element <ieto>:
!   I_DROPACT:  Aerosol activation to droplets
!   I_AERFREEZE: Aerosol homogeneous freezing
!   I_DROPFREEZE: Droplet homogeneous freezing
!   I_MIXEDFREEZE: Mixed total freezing
!   I_MIXEDMELT: Mixed total melting
!  Set to zero if particles are not subject to nucleation.
!
      do iefrom = 1,NELEM
        do ieto = 1,NELEM
          inucproc(iefrom,ieto) = 0
        enddo
      enddo
!      inucproc(1,3) = I_DROPACT
!
!
!  Initialize nucleation update time interval [s].
!
!      period_nuc = 900._f
      period_nuc = endtime + 1._f
!
!
!  Initialize nucleation update times [s] and index of smallest bin 
!  in each group from which a particle has nucleated.
!
      do igroup = 1,NGROUP
        time_nuc(igroup) = 0.
        inucmin(igroup) = NBIN
      enddo
!
!
!  Total evaporation mapping: total evaporation transfers particle mass from
!  element <ielem> to element <ievp2elem(ielem)>.
!  Set to zero if element is not subject to total evaporation.
!
!  This array is not automatically derived from <inuc2elem> because multiple
!  elements can nucleate to a particular element (reverse mapping is not
!  unique).
!
      ievp2elem(:) = 0
!      ievp2elem(2) = 0
!      ievp2elem(3) = 1
!
!
!==Set up solute properties and mapping arrays.
!
!
!  Solute name (one for each solute)
!
      solname(:) = 'sulfuric acid'
!
!
!  Solute molecular weights (one for each solute)
!
      solwtmol(:) = 0.098_f
!
!
!  Solute mass densities (one for each solute) [kg/m^3]
!
      rhosol(:) = 1380._f
!
!
!  Number of ions that solute dissociates into (one for each solute)
!
      sol_ions(:) = 2._f
!
!
!  Solute mapping: particle element <ielem> is composed of solute
!  <isolelem(ielem)>.  Should only be non-zero for elements of
!  itype = I_INVOLATILE [involatile number concentration] or 
!  itype = I_COREMASS [core mass concentration]).
!
      isolelem(:) = 0
!      isolelem(2) = 0
!      isolelem(3) = 1
!
!
!  Evaluate time-independent parameters and derived mapping arrays
!  used for condensational growth and evaporation.
!
!      call setupgrow
!      call setupgkern
!
!
!  <rlh_nuc(iefrom,ieto)> is the latent heat released by nucleation
!  from element <iefrom> to element <ieto> [m^2/s^2].
!
      do iefrom = 1,NELEM
        do ieto = 1,NELEM
          rlh_nuc(iefrom,ieto) = 0._f
        enddo
      enddo
!
!
!  Evaluate time-independent parameters and derived mapping arrays
!  used for nucleation.
!
!      call setupnuc
!
!-------------------------------------------------------------------------------
!
!
!==Set options for coagulation kernel:
!
      if( do_coag )then
!
!
!   <icoagop> = 0: use fixed coagulation kernel <ck0>
!             = 1: calculate the coagulation kernel
!
#ifdef COAGTEST
        icoagop = 0
#else
        icoagop = 1
#endif

!
!
!   <icollec> determines gravitational collection efficiencies
!     = 0: use constant value <grav_e_coll0>
!     = 1: use binwise maxima of Fuchs' and Langmuir's efficiencies
!     = 2: use input data
!
        icollec = 2
!
!
!  Fixed kernel value <ck0> only used when <icoagop> = 0 [m3 s-1]
!
        ck0 = 1.e-11_f

!  Coagulation via Brownian diffusion in the continuum limit for
!    two particles of the same size at 25oC = 
!
!       8 * kB * T / 3 / dynamic viscosity of air 
!
!  Used to compare to Jacobsen et al., "Modeling coagulation among
!   particles of different composition and size," Atmospheric Environment
!   28, 1327-13338, 1994.
 
        ck0 = 8._f * bk * 298._f / 3._f / 1.85e-5_f

!  <grav_e_coll0> only used when <icollec> = 0
!
        grav_e_coll0 = 1._f
!
!
!  This <NGROUP> by <NGROUP> array maps aerosol groups for coagulation
!  <icoag(i,j)> is the particle group resulting from a collision
!  between particles in groups <i> and <j>.
!  [This array must be diagonal, so the user need only specify
!  elements (i=1:NGROUP,j=1:i). ]

! JAS: this comment is a bit confusing.  setupckern is going to overwrite
! some of the icoag array to make it diagonal.  To get the desired array,
! specifiy icoag(i,j) for cases where i = j and cases where i > j. 

        icoag(1,1) = 1
!        icoag(2,1) = 2
!        icoag(2,2) = 2
!
#ifdef BCOC
        icoag(:,:) = 0
        icoag(1,1) = 1
        icoag(2,2) = 2
        icoag(2,1) = 3
        icoag(3,1) = 3
        icoag(3,2) = 3
        icoag(3,3) = 3
#endif
#ifdef CLDICE
        icoag(:,:) = 0
        icoag(1,1) = 1
        icoag(2,2) = 2
        icoag(2,1) = 2
#endif
!
!  Evaluate derived coagulation mapping arrays and kernels.
!
        call setupckern( carma, rc )
        call setupcoag( carma, rc )

      endif
!
!
!  Return to caller with aerosol and cloud microphysics mapping arrays
!  and time-independent parameters defined.
!

      return
      end
