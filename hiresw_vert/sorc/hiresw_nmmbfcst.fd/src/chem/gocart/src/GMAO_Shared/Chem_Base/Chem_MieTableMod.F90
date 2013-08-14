!-------------------------------------------------------------------------
!      NASA/GSFC, Global Modeling & Assimilation Office, Code 900.3      !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  Chem_MieTableMod --- Reader for aerosol mie tables
!
! !INTERFACE:
!

   module  Chem_MieTableMod

! !USES:

   use m_die, only: die, warn

   implicit none
   include "netcdf.inc"    ! Required for Mie tables stored as NCDF files

! !PUBLIC TYPES:
!
   PRIVATE
   PUBLIC  Chem_MieTable        ! Holds Mie Lookup Tables
                           
!
! !PUBLIC MEMBER FUNCTIONS:
!
   PUBLIC  Chem_MieTableCreate  ! Constructor 
   PUBLIC  Chem_MieTableDestroy ! Destructor
   PUBLIC  Chem_MieTableRead    ! Read the mie table from the file

!
! !DESCRIPTION:
!
!  This module read the mie aerosol tables.
!
! !REVISION HISTORY:
!
!  23Mar2005 Colarco - Initial code.
!  31Mar2005 Todling - Declared netcdf nf_ routines as external (OSF1) 
!                      Removed # from include netcdf.inc
!
!EOP
!-------------------------------------------------------------------------

! Mie LUT table
! Will be reduced from input files to the desired channels
! --------
  type Chem_MieTable

     character(len=255) :: mietablename
     integer :: nlambda         ! number of wavelengths in table
     integer :: nrh             ! number of RH values in table
     integer :: nbin            ! number of size bins in table
     real, pointer    :: lambda(:) => null()      ! wavelengths [m]
     real, pointer    :: rh(:) => null()           ! RH values   [fraction]
     real, pointer    :: bext(:,:,:) => null()     ! bext values [m2 kg-1]
     real, pointer    :: bsca(:,:,:) => null()     ! bsca values [m2 kg-1]
     real, pointer    :: bbck(:,:,:) => null()     ! bbck values [m2 kg-1]
     real, pointer    :: g(:,:,:) => null()        ! asymmetry parameter

     integer          :: rhi(991)        ! pointer to rh map
     real             :: rha(991)        ! slope on rh map

  end type Chem_MieTable

# ifndef HAS_NETCDF3
!  external nf_open, nf_inq_dimid, nf_inq_dimlen, nf_inq_varid, &
!           nf_get_var_double, nf_close      
#endif


CONTAINS

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Chem_MieTableCreate --- Construct Chemistry Registry
!
! !INTERFACE:
!

  Function Chem_MieTableCreate ( rcfile, rc )

  implicit none
  type(Chem_MieTable) Chem_MieTableCreate 

! !INPUT PARAMETERS:

   character(len=*) :: rcfile  ! Mie table file name

! !OUTPUT PARAMETERS:

   integer, intent(out) ::  rc            ! Error return code:
                                          !  0 - all is well
                                          !  1 - 

! !DESCRIPTION:
!
!
! !REVISION HISTORY:
!
!  09Mar2005 da Silva  API, prologues.
!
!EOP
!-------------------------------------------------------------------------

   character(len=*), parameter ::  myname = 'Chem_MieTableCreate'

   type(Chem_MieTable) :: this

   rc = 0

   this%mietablename = rcfile

!  Note: The actual allocation is done doing read because dimensions are
!        read from file

!  All done
!  --------
   Chem_MieTableCreate = this
   
   return 

 end Function Chem_MieTableCreate

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Chem_MieTableDestroy --- Destruct Mie Table
!
! !INTERFACE:
!
  subroutine Chem_MieTableDestroy ( this, rc )

! !USES:

  implicit none

! !INPUT/OUTPUT PARAMETERS:

   type(Chem_MieTable), intent(inout) :: this

! !OUTPUT PARAMETERS:

  integer, intent(out)          ::  rc     ! Error return code:
                                           !  0 - all is well
                                           !  1 - 

! !DESCRIPTION: Destructor for AOD object.
!
! !REVISION HISTORY:
!
!  23Mar2005 Colarco
!
!EOP
!-------------------------------------------------------------------------
   integer ios

   rc = 0

! Set these to invalid values
! ---------------------------
  this%nlambda = -1
  this%nrh = -1
  this%nbin = -1

! Deallocate whatever has been allocated
! --------------------------------------
  if ( associated(this%lambda) ) deallocate(this%lambda, stat=rc)
  if ( rc /= 0 ) return
  if ( associated(this%rh) )     deallocate(this%rh, stat=rc)
  if ( rc /= 0 ) return
  if ( associated(this%bext) )   deallocate(this%bext, stat=rc)
  if ( rc /= 0 ) return
  if ( associated(this%bsca) )   deallocate(this%bsca, stat=rc)
  if ( rc /= 0 ) return
  if ( associated(this%bbck) )   deallocate(this%bbck, stat=rc)
  if ( rc /= 0 ) return
  if ( associated(this%g) )      deallocate(this%g, stat=rc)
  if ( rc /= 0 ) return

end subroutine Chem_MieTableDestroy 

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Chem_MieTableRead --- Read and fill in the Mie table, interpolated
!                              to the requested channels
!
! !INTERFACE:
!
   SUBROUTINE Chem_MieTableRead ( this, nch, channels, rc )

! !INPUT PARAMETERS:

   IMPLICIT none
   TYPE(Chem_MieTable), intent(inout)       :: this
   integer :: nch               ! number of channels to interpolate table to
   real    :: channels(:)       ! channels to interpolate table to
   integer, intent(out) :: rc   ! return code


! !DESCRIPTION:
!
!   Fills in the Mie table
!
! !REVISION HISTORY:
!
!  23Mar2005 Colarco
!
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter :: myname = 'Chem_MieTableRead'

      integer :: ncid, idimid, ivarid, ios, n, i, j, ip1
      integer :: nch_table, nrh_table, nbin_table
!     Tables are hard-wired as single precision
      real*8, pointer :: channels_table(:), rh_table(:), &
                         bext_table(:,:,:), bsca_table(:,:,:), &
                         bbck_table(:,:,:), g_table(:,:,:)
      real :: yerr

      rc = 0

!     Open the table and get the dimensions
!     -------------------------------------
      rc = nf_open(this%mietablename, NF_NOWRITE, ncid)
      rc = nf_inq_dimid(ncid,'rh',idimid)
      rc = nf_inq_dimlen(ncid,idimid,nrh_table)
      rc = nf_inq_dimid(ncid,'lambda',idimid)
      rc = nf_inq_dimlen(ncid,idimid,nch_table)
      rc = nf_inq_dimid(ncid,'radius',idimid)
      rc = nf_inq_dimlen(ncid,idimid,nbin_table)

!     Get the table contents
!     -------------------------------------
!      allocate ( channels_table(nch_table), rh_table(nrh_table), &
!                bext_table(nch_table,nrh_table,nbin_table), &
!                bsca_table(nch_table,nrh_table,nbin_table), &
!                bbck_table(nch_table,nrh_table,nbin_table), &
!                g_table(nch_table,nrh_table,nbin_table), stat = rc )

      allocate(channels_table(nch_table),stat = rc )
      if ( rc /= 0 ) return
      allocate(rh_table(nrh_table),stat = rc )
      if ( rc /= 0 ) return
      allocate(bext_table(nch_table,nrh_table,nbin_table),stat = rc )
      if ( rc /= 0 ) return
      allocate(bsca_table(nch_table,nrh_table,nbin_table),stat = rc )
      if ( rc /= 0 ) return
      allocate(bbck_table(nch_table,nrh_table,nbin_table), stat = rc )
      if ( rc /= 0 ) return
      allocate(g_table(nch_table,nrh_table,nbin_table), stat = rc )
      if ( rc /= 0 ) return

      rc = nf_inq_varid(ncid,'lambda',ivarid)
      if ( rc /= 0 ) return
      rc = nf_get_var_double(ncid,ivarid,channels_table)
      if ( rc /= 0 ) return
      rc = nf_inq_varid(ncid,'bext',ivarid)
      if ( rc /= 0 ) return
      rc = nf_get_var_double(ncid,ivarid,bext_table)
      if ( rc /= 0 ) return
      rc = nf_inq_varid(ncid,'bsca',ivarid)
      if ( rc /= 0 ) return
      rc = nf_get_var_double(ncid,ivarid,bsca_table)
      if ( rc /= 0 ) return
      rc = nf_inq_varid(ncid,'bbck',ivarid)
      if ( rc /= 0 ) return
      rc = nf_get_var_double(ncid,ivarid,bbck_table)
      if ( rc /= 0 ) return
      rc = nf_inq_varid(ncid,'g',ivarid)
      if ( rc /= 0 ) return
      rc = nf_get_var_double(ncid,ivarid,g_table)
      if ( rc /= 0 ) return
      rc = nf_inq_varid(ncid,'rh',ivarid)
      if ( rc /= 0 ) return
      rc = nf_get_var_double(ncid,ivarid,rh_table)

!     Close the table file
!     -------------------------------------
      rc = nf_close(ncid)
      if ( rc /= 0 ) return

!     Setup the table to be returned
!     -------------------------------------
      this%nlambda = nch
      this%nrh = nrh_table
      this%nbin = nbin_table

!      allocate ( this%lambda(this%nLambda), this%rh(this%nrh), &
!                 this%bext(this%nLambda,this%nrh,this%nbin),   &
!                 this%bsca(this%nLambda,this%nrh,this%nbin),   &
!                 this%bbck(this%nLambda,this%nrh,this%nbin),   &
!                 this%g(this%nLambda,this%nrh,this%nbin),      &
!                 stat = rc )

      allocate (this%lambda(this%nLambda),stat = rc )
      if ( rc /= 0 ) return
      allocate (this%rh(this%nrh),stat = rc )
      if ( rc /= 0 ) return
      allocate (this%bext(this%nLambda,this%nrh,this%nbin),stat = rc )
      if ( rc /= 0 ) return
      allocate (this%bsca(this%nLambda,this%nrh,this%nbin),stat = rc )
      if ( rc /= 0 ) return
      allocate (this%bbck(this%nLambda,this%nrh,this%nbin),stat = rc )
      if ( rc /= 0 ) return
      allocate (this%g(this%nLambda,this%nrh,this%nbin),   stat = rc )
      if ( rc /= 0 ) return

!     Preserve the full RH structure of the input table
      this%rh(:) = rh_table(:)

!     Insert the requested channels in the output table
      this%lambda(:) = channels(:)

!     Now we linearly interpolate the input table to the output table grid
!     of requested channels
      do j = 1, this%nbin
       do i = 1, this%nrh
        do n = 1, this%nlambda
         call polint(channels_table,bext_table(:,i,j),nch_table, &
                     this%lambda(n),this%bext(n,i,j),yerr)
         call polint(channels_table,bsca_table(:,i,j),nch_table, &
                     this%lambda(n),this%bsca(n,i,j),yerr)
         call polint(channels_table,bbck_table(:,i,j),nch_table, &
                     this%lambda(n),this%bbck(n,i,j),yerr)
         call polint(channels_table,g_table(:,i,j),nch_table, &
                     this%lambda(n),this%g(n,i,j),yerr)
        enddo
       enddo
      enddo

!     Now we do a mapping of the RH from the input table to some high
!     resolution representation.  This is to spare us the need to
!     do a full-up interpolation later on.
!     RH input from the table is scaled 0 - 0.99
!     We resolve the map to 0 - 0.990 in steps of 0.001 (991 total steps)
      do j = 1, 991
       do i = this%nrh, 1, -1
        if( (j-1) .ge. int(this%rh(i)*1000)) then
         ip1 = i + 1
         this%rhi(j) = i
         if(ip1 .gt. this%nrh) then
          this%rha(j) = 0.
         else
          this%rha(j) =   ( (j-1)/1000. - this%rh(i)) &
                       /  ( this%rh(ip1)- this%rh(i))
         endif
         exit
        endif
       enddo
!       print *, j, this%rhi(j), this%rha(j), this%rh(this%rhi(j))
      enddo

!      deallocate (channels_table, rh_table, bext_table, bsca_table, &
!                  bbck_table, g_table, stat = rc )

      deallocate (channels_table, stat = rc )
      if ( rc /= 0 ) return
      deallocate (rh_table, stat = rc )
      if ( rc /= 0 ) return
      deallocate (bext_table, stat = rc )
      if ( rc /= 0 ) return
      deallocate (bsca_table, stat = rc )
      if ( rc /= 0 ) return
      deallocate (bbck_table, stat = rc )
      if ( rc /= 0 ) return
      deallocate (g_table, stat = rc )
      if ( rc /= 0 ) return
return

contains

   subroutine polint(x,y,n,xWant,yWant,yErr)
   integer :: n
!  recall, table hard-wired single precision
   real*8 :: x(n),y(n)
   real   :: xWant, yWant, yErr

!  given array x(n) of independent variables and array y(n) of dependent
!  variables, compute the linear interpolated result yWant at xWant and return
!  with a dummy error estimate yErr.  Hacked up from Numerical Recipes Chapter 3

   integer :: i, j
   real    :: dx, slope
   character(len=255) :: msg

!  on out of bounds, set i to lower or upper limit
   i = 0
   if(xWant .lt. x(1)) then 
    write(msg,*) "in polint, wanted: ", xWant, ", got lower bound: ", x(1)
    call warn(myname,msg)
    i = 1
   endif
   if(xWant .gt. x(n)) then 
    write(msg,*) "in polint, wanted: ", xWant, ", got upper bound: ", x(n)
    call warn(myname,msg)
    i = n
   endif

!  if i is still zero find i less than xWant
   if(i .eq. 0) then
    do j = 1, n
     if(xWant .ge. x(j)) i = j
    enddo
   endif

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
   end subroutine polint

END SUBROUTINE Chem_MieTableRead


 end module Chem_MieTableMod

