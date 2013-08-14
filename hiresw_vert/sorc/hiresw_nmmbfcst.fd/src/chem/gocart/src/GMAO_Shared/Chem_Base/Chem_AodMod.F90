!-------------------------------------------------------------------------
!      NASA/GSFC, Global Modeling & Assimilation Office, Code 900.3      !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  Chem_AodMod --- Aerosol Optical Depth Calculator
!
! !INTERFACE:
!

   module  Chem_AodMod

! !USES:

   use m_die, only: die
   Use Chem_RegistryMod
   Use Chem_BundleMod
   Use Chem_MieTableMod
   Use m_inpak90

   implicit none

! !PUBLIC TYPES:
!
   PRIVATE
   PUBLIC  Chem_Aod        ! Holds Lookup Tables (LUT), etc
                           
!
! !PUBLIC MEMBER FUNCTIONS:
!
   PUBLIC  Chem_AodCreate  ! Constructor 
   PUBLIC  Chem_AodDestroy ! Destructor
   PUBLIC  Chem_AodRun     ! Calculates AOD given mixing ratios
   PUBLIC  Chem_AodAdj     ! Calculates Adjoint of AOD obs operator

!
! !DESCRIPTION:
!
!  This module implements an Aerosol Optical Depth calculator.
!
! !REVISION HISTORY:
!
!  09Mar2005 da Silva  API, prologues.
!  23Mar2005 Colarco   Implemented
!  29Mar2005 da Silva  Simplified Run method, added optional 2D output.
!
!EOP
!-------------------------------------------------------------------------

! Contains the per species look-up tables of bext, bsca, etc.
! These are contained here only at the channels specified in the
! AOD resource file.
! Also contained: map_XX -> for each species, one for each q-bin in that
!                           species.  Set = 0 to not calculate AOD for
!                           that bin; set to bin in mie_XX table used
!                           to compute AOD
!                 rh_XX  -> for each species, one for each q-bin in that
!                           species.  Set = 0 to not consider RH adjustment
!                           of bext, etc., set = 1 to consider RH
! --------
  type Chem_Aod

     integer :: nch                  ! number of channels
     real, pointer    :: channels(:) ! wavelengths
     character(len=255) :: du_optics_file
     character(len=255) :: ss_optics_file
     character(len=255) :: bc_optics_file
     character(len=255) :: oc_optics_file
     character(len=255) :: su_optics_file

     type(Chem_Registry) :: aodReg  ! AOD registry

     type(Chem_MieTable) :: mie_DU  ! mie tables -- dim(nch,nrh,nbin)
     type(Chem_MieTable) :: mie_SS 
     type(Chem_MieTable) :: mie_BC 
     type(Chem_MieTable) :: mie_OC 
     type(Chem_MieTable) :: mie_SU 

     integer, pointer :: rh_DU(:) => null()   ! per size bin: 0 if tau RH insensitive
     integer, pointer :: rh_SS(:) => null()   !               1 if it is
     integer, pointer :: rh_OC(:) => null()
     integer, pointer :: rh_BC(:) => null()
     integer, pointer :: rh_SU(:) => null()

     integer, pointer :: map_DU(:) => null()  ! per size bin
     integer, pointer :: map_SS(:) => null()  ! which table bin to map to (0 = no tau)
     integer, pointer :: map_OC(:) => null()
     integer, pointer :: map_BC(:) => null()
     integer, pointer :: map_SU(:) => null()

  end type Chem_Aod

CONTAINS

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Chem_AodCreate --- Construct AOD Registry Information and
!                                get tables
!
! !INTERFACE:
!

  Function Chem_AodCreate ( w_c, rc, rcfile )

  implicit none
  type(Chem_Aod) Chem_AodCreate 

! !INPUT PARAMETERS:

   type(Chem_Bundle), intent(in) :: w_c     ! Chem bundle (mixing ratio)
   character(len=*), OPTIONAL    :: rcfile  ! Resource file name which contains
                                            ! requested channels and output
                                            ! fields.  If not present, assume
                                            ! a default resoure file

! !OUTPUT PARAMETERS:

   integer, intent(out)          ::  rc     ! Error return code:
                                            !  0 - all is well
                                            !  1 - 

! !DESCRIPTION:
!
!
! !REVISION HISTORY:
!
!  09Mar2005 da Silva  API, prologues.
!  23Mar2005 Colarco   Implemented
!
!EOP
!-------------------------------------------------------------------------

   character(len=*), parameter ::  myname = 'Chem_AodCreate'

   type(Chem_Aod)     :: this
   character(len=255) :: rcfilen = 'Aod_Registry.rc'
   integer            :: nq, ios, ier, fid, ncid, i, j, n
   real, pointer      :: rh_table(:), lambda_table(:), &
                         bext(:,:,:), bsca(:,:,:), reff(:,:)

   rc = 0

!  If resource file passed, then parse it and get the channels to calculate
!  on, otherwise use a default resource file.
!  ------------------------------------------------------

   if ( present(rcfile) ) then
     rcfilen = trim(rcfile)
   else
     rcfilen = 'Aod_Registry.rc'
   endif

!  Create AOD registry
!  -------------------
   this%aodReg = Chem_RegistryCreate ( rc, rcfile=rcfilen ) 
   call Chem_RegistryPrint ( this%aodReg )
             

!  Load the resource file
!  ----------------------
   call i90_loadf ( rcfilen, ier )
   if ( ier /= 0 ) call die(myname, 'could not read rc file '// &
        trim(rcfilen) )


!  Set the number of channels to calculate over
   call i90_label ( 'n_channels:', ier )
   if ( ier /= 0 ) then
    call die(myname, 'could not find channel number request')
   else
    this%nch = i90_gint ( ier )
    if ( ier /= 0 ) call die(myname,'could not parse number of channels')
   end if


   if(this%nch .gt. 0) then

!   Set the channels to calculate over
    allocate( this%channels(this%nch), stat = rc )
    if ( rc /= 0 ) return
    call i90_label ( 'r_channels:', ier )
    if ( ier /= 0 ) then
     call die(myname, 'could not find channel number request')
    else
     do n = 1, this%nch
      this%channels(n) = i90_gfloat ( ier )
      if ( ier /= 0 ) call die(myname,'could not parse channels')
     enddo
    end if
   else
!   Default will just choose 550 nm
    this%nch = 1
    allocate( this%channels(this%nch), stat = rc )
    if ( rc /= 0 ) return
    this%channels(1) = 5.5e-7
   endif

!  Get the needed mie tables, based on the input chem registry
!  Crucial: this is where the mapping is also being done to RH and bin,
!  so there is some possibility of error.  Also, should be some consistency
!  check with the Chem registry here.
!  ------------------------------------------------------

!  Get the DU optical properties
   if( w_c%reg%doing_DU) then
     call i90_label ( 'filename_optical_properties_DU:', ier )
     if ( ier /= 0 ) then
      call die(myname, 'could not parse DU filename label')
     else
      call i90_gtoken ( this%du_optics_file, ier )
      if ( ier /= 0 ) call die(myname,'could not parse DU filename')
     end if
!    Load the mie table
     this%mie_DU = Chem_MieTableCreate(this%du_optics_file, ier)
     call Chem_MieTableRead( this%mie_DU, this%nch, this%channels, rc )
     if ( rc /= 0 ) return
!    Map the DU classes to the table
     allocate ( this%rh_DU(w_c%reg%n_DU), this%map_DU(w_c%reg%n_DU), stat=rc)
     if ( rc /= 0 ) return
     this%rh_DU(:) = 0
     this%map_DU(1) = 1
     this%map_DU(2) = 2
     this%map_DU(3) = 3
     this%map_DU(4) = 4
     this%map_DU(5) = 5
   endif

!  Get the SS optical properties
   if( w_c%reg%doing_SS) then
     call i90_label ( 'filename_optical_properties_SS:', ier )
     if ( ier /= 0 ) then
      call die(myname, 'could not parse SS filename label')
     else
      call i90_gtoken ( this%ss_optics_file, ier )
      if ( ier /= 0 ) call die(myname,'could not parse SS filename')
     end if
!    Load the mie table
     this%mie_SS = Chem_MieTableCreate(this%ss_optics_file, ier)
     call Chem_MieTableRead( this%mie_SS, this%nch, this%channels, rc )
     if ( rc /= 0 ) return
!    Map the SS classes to the table
     allocate ( this%rh_SS(w_c%reg%n_SS), this%map_SS(w_c%reg%n_SS), stat=rc)
     if ( rc /= 0 ) return
     this%rh_SS(:) = 1
     this%map_SS(1) = 1
     this%map_SS(2) = 2
     this%map_SS(3) = 3
     this%map_SS(4) = 4
     this%map_SS(5) = 5
   endif

!  Get the BC optical properties
   if( w_c%reg%doing_BC) then
     call i90_label ( 'filename_optical_properties_BC:', ier )
     if ( ier /= 0 ) then
      call die(myname, 'could not parse BC filename label')
     else
      call i90_gtoken ( this%bc_optics_file, ier )
      if ( ier /= 0 ) call die(myname,'could not parse BC filename')
     end if
!    Load the mie table
     this%mie_BC = Chem_MieTableCreate(this%bc_optics_file, ier)
     call Chem_MieTableRead( this%mie_BC, this%nch, this%channels, rc )
     if ( rc /= 0 ) return
!    Map the BC classes to the table
     allocate ( this%rh_BC(w_c%reg%n_BC), this%map_BC(w_c%reg%n_BC), stat=rc)
     if ( rc /= 0 ) return
     this%rh_BC(1) = 0
     this%rh_BC(2) = 1
     this%map_BC(1) = 1
     this%map_BC(2) = 1
   endif

!  Get the OC optical properties
   if( w_c%reg%doing_OC) then
     call i90_label ( 'filename_optical_properties_OC:', ier )
     if ( ier /= 0 ) then
      call die(myname, 'could not parse OC filename label')
     else
      call i90_gtoken ( this%oc_optics_file, ier )
      if ( ier /= 0 ) call die(myname,'could not parse OC filename')
     end if
!    Load the mie table
     this%mie_OC = Chem_MieTableCreate(this%oc_optics_file, ier)
     call Chem_MieTableRead( this%mie_OC, this%nch, this%channels, rc )
     if ( rc /= 0 ) return
!    Map the OC classes to the table
     allocate ( this%rh_OC(w_c%reg%n_OC), this%map_OC(w_c%reg%n_OC), stat=rc)
     if ( rc /= 0 ) return
     this%rh_OC(1) = 0
     this%rh_OC(2) = 1
     this%map_OC(1) = 1
     this%map_OC(2) = 1
   endif

!  Get the SU optical properties
   if( w_c%reg%doing_SU) then
     call i90_label ( 'filename_optical_properties_SU:', ier )
     if ( ier /= 0 ) then
      call die(myname, 'could not parse SU filename label')
     else
      call i90_gtoken ( this%su_optics_file, ier )
      if ( ier /= 0 ) call die(myname,'could not parse SU filename')
     end if
!    Load the mie table
     this%mie_SU = Chem_MieTableCreate(this%su_optics_file, ier)
     call Chem_MieTableRead( this%mie_SU, this%nch, this%channels, rc )
     if ( rc /= 0 ) return
!    Map the SU classes to the table
     allocate ( this%rh_SU(w_c%reg%n_SU), this%map_SU(w_c%reg%n_SU), stat=rc)
     if ( rc /= 0 ) return
     this%rh_SU(1) = 0
     this%rh_SU(2) = 0
     this%rh_SU(3) = 1
     this%rh_SU(4) = 1
     this%map_SU(1) = 0
     this%map_SU(2) = 0
     this%map_SU(3) = 1
     this%map_SU(4) = 1
   endif


   call I90_Release()

!  All done
!  --------
   Chem_AodCreate = this
   
   return 

 end Function Chem_AodCreate

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Chem_AodDestroy --- Destruct Chemisty Registry
!
! !INTERFACE:
!
  subroutine Chem_AodDestroy ( this, rc )

! !USES:

  implicit none

! !INPUT/OUTPUT PARAMETERS:

   type(Chem_Aod), intent(inout) :: this

! !OUTPUT PARAMETERS:

  integer, intent(out)           ::  rc     ! Error return code:
                                            !  0 - all is well
                                            !  1 - 

! !DESCRIPTION: Destructor for AOD object.
!
! !REVISION HISTORY:
!
!  09Mar2005 da Silva  API, prologues.
!  23Mar2005 Colarco   Implemented
!
!EOP
!-------------------------------------------------------------------------
   integer ier(6)

   rc = 0

   call Chem_RegistryDestroy ( this%aodReg, ier(1) )
   if ( ier(1) /= 0 ) then
      rc = 1
      return
   end if

   deallocate ( this%channels, stat = ier(1) )
   deallocate ( this%rh_DU, this%map_DU, stat=ier(2) )
   deallocate ( this%rh_SS, this%map_SS, stat=ier(3) )
   deallocate ( this%rh_BC, this%map_BC, stat=ier(4) )
   deallocate ( this%rh_OC, this%map_OC, stat=ier(5) )
   deallocate ( this%rh_SU, this%map_SU, stat=ier(6) )

   if ( any(ier /= 0 ) ) then
      rc = 2
      return
   end if


end subroutine Chem_AodDestroy 


!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Chem_AodRun --- Calculates AOD given aerosol mixing ratios
!
! !INTERFACE:
!
   SUBROUTINE Chem_AodRun ( this, w_c, rc,          & 
                            which,                  &
                            w_tau,  w_ssa,          &  
                            w_bck, &
                            tau_2d, ssa_2d          ) 


! !INPUT PARAMETERS:

   IMPLICIT none
   TYPE(Chem_Aod), intent(in)       :: this           ! AOD registry
   type(Chem_Bundle), intent(in)    :: w_c            ! mixing ratios 
   character(len=255), optional     :: which(:)       ! which tracer 
                                                      ! species to sum over
! !OUTPUT PARAMETERS:

   integer, intent(out)                       :: rc       ! error code
   type(Chem_Bundle), OPTIONAL, intent(inout) :: w_tau    ! AOD
   type(Chem_Bundle), OPTIONAL, intent(inout) :: w_ssa    ! SSA
   type(Chem_Bundle), OPTIONAL, intent(inout) :: w_bck    ! backscatter

                                                                ! (im,jm,nch)
   real,              OPTIONAL, intent(inout) :: tau_2d(:,:,:)  ! 2D AOD
   real,              OPTIONAL, intent(inout) :: ssa_2d(:,:,:)  ! 2D SSA

! !DESCRIPTION:
!
!   Calculates the AOD observation operator.
!   Inputs are:
!     this      - structure containing registry and lookup tables
!                 as function of species (and size class), RH, and lambda
!     w_c       - structure containing the constituent registry,
!                 mixing ratios, pressure level thickness, and RH
!   By default then the extinction optical thickness is computed per grid box
!   as a sum over all constituents in the registry.  In the future may change
!   the sense of this.
!   Depending on flag this%rh_XX(:) sense, the computations use a linear
!   interpolation of, e.g., this%mie_XX%bext to RH
!   Outputs are:
!     w_tau     - type chem_bundle structure which contains extinction
!                 AOD as function of x, y, z, and channels requested.
!     w_ssa     - type chem_bundle structure which contains single scatter
!                 albedo as function of x, y, z, and channels requested. (OPTIONAL)

!
! !REVISION HISTORY:
!
!  09Mar2005 da Silva  API, prologues.
!  23Mar2005 Colarco   Implemented
!
!EOP
!-------------------------------------------------------------------------

   character(len=*), parameter ::  myname = 'Chem_AodRun'

   integer :: i1, i2, j1, j2, im, jm, nbeg, nend, km, nch
   integer :: i, j, k, l, n, ios
   integer :: nqtype, mMap, mRh
   real, parameter :: grav = 9.81    ! acceleration of gravity in m2 s-1
   real :: bext, bsca, bbck, berr, rh
   real, pointer :: tauSca(:,:,:), tauExt(:,:,:), bckSca(:,:,:)
   real, pointer :: tauExt_2D(:,:), tauSca_2D(:,:)
   logical :: doing_DU, doing_SS, doing_SU, doing_OC, doing_BC
   logical :: doing_SSA  ! true if computing single scatter albedo
   logical :: doing_BCK  ! true if computing the aerosol backscatter


!  Initialize local variables
!  --------------------------
   rc = 0
   i1 = w_c%grid%i1; i2 = w_c%grid%i2; im = w_c%grid%im
   j1 = w_c%grid%j1; j2 = w_c%grid%j2; jm = w_c%grid%jm
   km = w_c%grid%km
   nch  = this%nch


!  Check for the optional parameter to set the species to integrate
!  Default: set species to false and then reset according either to
!  which values or values (if present) or from chem registry.
!  -----------------------------------------------------------------------
   doing_DU = .false.
   doing_SS = .false.
   doing_SU = .false.
   doing_OC = .false.
   doing_BC = .false.
   if( present(which)) then
      nqtype = size(which)
      do i = 1, nqtype
         if(trim(which(i)) .eq. 'DU') doing_DU = .true.
         if(trim(which(i)) .eq. 'SS') doing_SS = .true.
         if(trim(which(i)) .eq. 'SU') doing_SU = .true.
         if(trim(which(i)) .eq. 'OC') doing_OC = .true.
         if(trim(which(i)) .eq. 'BC') doing_BC = .true.
      end do
      doing_DU = doing_DU .AND. w_c%reg%doing_DU
      doing_SS = doing_SS .AND. w_c%reg%doing_SS
      doing_SU = doing_SU .AND. w_c%reg%doing_SU
      doing_BC = doing_BC .AND. w_c%reg%doing_BC
      doing_OC = doing_OC .AND. w_c%reg%doing_OC
   else
      doing_DU = w_c%reg%doing_DU
      doing_SS = w_c%reg%doing_SS
      doing_SU = w_c%reg%doing_SU
      doing_OC = w_c%reg%doing_OC
      doing_BC = w_c%reg%doing_BC
   end if

   allocate ( tauExt(i1:i2,j1:j2,1:km), stat=ios )
   if ( ios /= 0 ) then
      rc = 100
      return
   end if
   tauExt = 0.0

!  Initialize output variables
!  ---------------------------
   doing_SSA = .false.
   doing_BCK = .false.
   if ( present(w_tau) ) then
        do l = 1, nch
           w_tau%qa(l)%data3d = 0.0
        end do
   end if
   if ( present(w_ssa) ) then
        do l = 1, nch
           w_ssa%qa(l)%data3d = 0.0
        end do
        doing_SSA = .true.
        do n = 1, nch
           w_ssa%reg%vtitle(n)(1:3) = 'SSA' ! dirty trick
        end do
   end if
   if ( present(w_bck) ) then
        do l = 1, nch
           w_bck%qa(l)%data3d = 0.0
        end do
        doing_BCK = .true.
        do n = 1, nch
           w_bck%reg%vtitle(n)(1:3) = 'BCK' ! dirty trick
        end do
   end if
   if ( present(tau_2d) ) then
        tau_2d = 0.0
   end if
   if ( present(ssa_2d) ) then
        ssa_2d = 0.0
        doing_SSA = .true.
   end if

   allocate ( tauExt_2D(i1:i2,j1:j2),   stat=ios )
   if ( ios /= 0 ) then
      rc = 100
      return
   end if
   allocate ( tauSca_2D(i1:i2,j1:j2),   stat=ios )
   if ( ios /= 0 ) then
      rc = 100
      return
   end if
   if(doing_SSA) then
      allocate ( tauSca(i1:i2,j1:j2,1:km), stat=ios )
      if ( ios /= 0 ) then
         rc = 200
         return
      end if
   endif
   if(doing_BCK) then
      allocate ( bckSca(i1:i2,j1:j2,1:km), stat=ios )
      if ( ios /= 0 ) then
         rc = 300
         return
      end if
   endif

!  For each channel ...
!  --------------------
   do l = 1, nch

!    Extinction and (optionally) scattering AOD are stored in temporary
!     3D arrays for later retur
!    -----------------------------------------------------------------------
     tauExt    = 0.0
     tauExt_2D = 0.0
     if ( doing_SSA ) then
        tauSca    = 0.0
        tauSca_2D = 0.0
     end if
     if( doing_BCK ) then
        bckSca    = 0.0
     endif

!    Dust
!    ----
     call WorkHorse_ ( doing_DU, w_c%reg%i_DU, w_c%reg%j_DU, &
                       this%rh_DU, this%map_DU, this%mie_DU ) 

!    Sea salt
!    --------
     call WorkHorse_ ( doing_SS, w_c%reg%i_SS, w_c%reg%j_SS, &
                       this%rh_SS, this%map_SS, this%mie_SS ) 

!    Black carbon
!    ------------
     call WorkHorse_ ( doing_BC, w_c%reg%i_BC, w_c%reg%j_BC, &
                       this%rh_BC, this%map_BC, this%mie_BC ) 


!    Organic Carbon
!    --------------
     call WorkHorse_ ( doing_OC, w_c%reg%i_OC, w_c%reg%j_OC, &
                       this%rh_OC, this%map_OC, this%mie_OC ) 


!    Sulfates
!    --------
     call WorkHorse_ ( doing_SU, w_c%reg%i_SU, w_c%reg%j_SU, &
                       this%rh_SU, this%map_SU, this%mie_SU ) 


!    Return requested output
!    -----------------------
     if ( present(w_tau) ) then
        w_tau%qa(l)%data3d(:,:,:) = tauExt(:,:,:)        
     end if
     
     if ( present(w_bck) ) then
        w_bck%qa(l)%data3d(:,:,:) = bckSca(:,:,:)        
     end if
     
     if ( present(tau_2D) .or. present(ssa_2D) ) then
        do k = 1, km
           tauExt_2d(:,:) = tauExt_2d(:,:) + tauExt(:,:,k)
        end do
     end if
     
     if ( present(tau_2D) )then
        tau_2D(:,:,l) = tauExt_2D(:,:)
     end if
     
     if ( present(w_ssa) ) then
        w_ssa%qa(l)%data3d(:,:,:) = tauSca(:,:,:) / tauExt(:,:,:)        
     end if
     
     if ( present(ssa_2D) ) then
        do k = 1, km
           tauSca_2d(:,:) = tauSca_2d(:,:) + tauSca(:,:,k)
        end do
        ssa_2D(:,:,l) = tauSca_2d(:,:) / tauExt_2D(:,:)        
     end if
     
     
  end do ! for each channel

!   Clean up
!   --------
    deallocate ( tauExt, tauExt_2D, tauSca_2D, stat=ios)
    if ( ios /= 0 ) then
       rc = 300
       return
    end if
    if ( doing_SSA ) then
       deallocate ( tauSca, stat=ios)
       if ( ios /= 0 ) then
          rc = 400
          return
       end if
    end if
    if ( doing_BCK ) then
       deallocate ( bckSca, stat=ios)
       if ( ios /= 0 ) then
          rc = 500
          return
       end if
    end if

!   All done
!   --------
    rc = 0


contains

    subroutine WorkHorse_ ( doing_XX, i_XX, j_XX, rh_XX, map_XX, mie_XX ) 

    logical, intent(in) :: doing_XX
    integer, intent(in) :: i_XX, j_XX
    integer, intent(in) :: rh_XX(:), map_XX(:)
    type(Chem_MieTable), intent(in) :: mie_XX

    if ( .not.doing_XX ) return

     do n = i_XX, j_XX

      bext = 0.
      bsca = 0.
      bbck = 0.
      mRh = rh_XX(n-i_XX+1)
      mMap = map_XX(n-i_XX+1)

      if(mMap .gt. 0) then

      do k = 1, km
       do j = j1, j2
        do i = i1, i2

          rh = w_c%rh(i,j,k)/100.
          if(mRh .eq. 0) then
           bext = mie_XX%bext(l,1,mMap)
           bsca = mie_XX%bsca(l,1,mMap)
           bbck = mie_XX%bbck(l,1,mMap)
          else
           call polint_(mie_XX%rh, mie_XX%bext(l,:,mMap), &
                       mie_XX%nrh, rh, bext, berr)
           if(doing_SSA) &
           call polint_(mie_XX%rh, mie_XX%bsca(l,:,mMap), &
                       mie_XX%nrh, rh, bsca, berr)
           if(doing_BCK) &
           call polint_(mie_XX%rh, mie_XX%bbck(l,:,mMap), &
                       mie_XX%nrh, rh, bbck, berr)
          endif
          tauExt(i,j,k) = tauExt(i,j,k) &
                        +  bext*w_c%qa(n)%data3d(i,j,k)*w_c%delp(i,j,k)/grav

          if(doing_SSA) then
           tauSca(i,j,k) = tauSca(i,j,k) &
                         +  bsca*w_c%qa(n)%data3d(i,j,k)*w_c%delp(i,j,k)/grav
          endif

          if(doing_BCK) then
           bckSca(i,j,k) = bckSca(i,j,k) &
                         +  bbck*w_c%qa(n)%data3d(i,j,k)*w_c%delp(i,j,k)/grav
          endif

        enddo ! i

       enddo  ! j

      enddo   ! k

      endif   ! whether this bin enters tau calculation

     end do   ! over bins

   end subroutine WorkHorse_

   subroutine polint_ ( x, y, n, xWant, yWant, yErr )
   integer :: n
   real ::yErr,x(n),y(n),xWant,yWant

!  given array x(n) of independent variables and array y(n) of dependent
!  variables, compute the linear interpolated result yWant at xWant and 
!  return with a dummy error estimate yErr.
!  Hacked up from Numerical Recipes Chapter 3

   integer :: i, j
   real    :: dx, slope

!  on out of bounds, set i to lower or upper limit
   i = 0
   if(xWant .lt. x(1)) then 
!    write(*,*) "Wanted: ", xWant, ", lower bound: ", x(1)
    i = 1
   endif
   if(xWant .gt. x(n)) then 
!    write(*,*) "Wanted: ", xWant, ", upper bound: ", x(n)
    i = n
   endif

!  if i is still zero find i less than xWant
   do j = 1, n
    if(xWant .ge. x(j)) i = j
   enddo

!  slope
   if(i .eq. n) then 
    slope = 0.
   else
    slope = (y(i+1)-y(i)) / (x(i+1)-x(i))
   endif
   dx = xWant - x(i)
   yWant = y(i) + slope*dx

   yErr = 0.

   return
   end subroutine polint_
  
 end SUBROUTINE Chem_AodRun
  
!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Chem_AodAdj_ --- Calculates AOD Adjoint operator
!
! !INTERFACE:
!
   SUBROUTINE Chem_AodAdj ( this, w_tau, w_c )

! !INPUT PARAMETERS:

   IMPLICIT none
   TYPE(Chem_Aod), intent(in)       :: This

   type(Chem_Bundle), intent(inout) :: w_tau  ! AOD

! !OUTPUT PARAMETERS:

   type(Chem_Bundle), intent(in)    :: w_c    ! mixing ratios 


! !DESCRIPTION:
!
!   Calculates the adjoint of the AOD observation operator.
!
! !REVISION HISTORY:
!
!  09Mar2005 da Silva  API, prologues.
!
!EOP
!-------------------------------------------------------------------------

  
END SUBROUTINE Chem_AodAdj



 end module Chem_AodMod

