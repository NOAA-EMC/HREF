

#define VERIFY_(A)   if(  A/=0) then; if(present(rc)) rc=A; PRINT *, __LINE__; return; endif
#define ASSERT_(A)   if(.not.A) then; if(present(rc)) rc=1; PRINT *, __LINE__; return; endif
#define RETURN_(A)   if(present(rc)) rc=A; return
#define SUCCESS      0
#define DEALOC_(A)   if(associated(A)) deallocate(A)


!BOP

! !MODULE: MAPL_HorzTransMod
!    A Module to do linear transformations on 2-dimensional arrays


! !INTERFACE:

module MAPL_HorzTransformMod

!  $Id: MAPL_HorzTransform.F90,v 1.7.20.3.2.3 2009/03/16 17:38:49 dkokron Exp $

  implicit none
  private

! PUBLIC TYPES:

  public MAPL_HorzTransform

! PUBLIC MEMBER FUNCTIONS:

  public MAPL_HorzTransformIsCreated
  public MAPL_HorzTransformCreate
  public MAPL_HorzTransformDestroy
  public MAPL_HorzTransformRun

  public MAPL_DimTopoCyclic
  public MAPL_DimTopoEdge
  public MAPL_DimTopoCenter


! !DESCRIPTION:

! This package performs a serial linear transformation with a
!   previously computed sparse matrix stored in a MAPL_HorzBinTransform
!   object.  Currently, the Transform objects that can be created
!   are limited to a conservative ``binning'',
!   bilinear intepolation, or biquadratic interpolation
!   of uniform-resolution, 2-dimensional
!   (LogicalRectangular) grids that span the same domain. 
!
! Either dimension of the array can be cyclic or bounded. If bounded
!   the grids can have either an edge or a center at the domain boundary,
!   the only difference being that an "edge" boundary does not allow
!   any extrapolation on input values below the first or above the last
!   point. Edge is useful only for meridional interpolation between the
!   two poles.
!   
! To use the pachage, a transform is first created using {\bf MAPL_HorzTransformCreate} and
!   then used repeatedly by passing it to the interpolation routine {\bf MAPL_HorzTransformRun}
!   together with input and output arrays. When a transfor is no longer needed, it can be
!   destroyed with {\bf MAPL_HorzTransformDestroy}. 
!
! In addition to these three methods and the type of the class object {\bf MAPL_HorzTransform},
!   the only puclic quantities are the allowed values of an enumeration that   
!   specifies the 'topology' of a dimension (cyclic, edge, or centered).
!   
! The package is not parallelized, so all the data must be on a single processor. It  
!   does not rely on an other modules or libraries.
!   
!EOP

! Topology enumeration:
!   Edge and Center refer to the location of the 
!   first and last values in a dimension. The distance between points
!   is always the same, but with edge the first and last points represent 
!   only half a grid box, as with the meridional dimension in the grid
!   used by FV.

  integer, parameter :: MAPL_DimTopoCyclic = 0
  integer, parameter :: MAPL_DimTopoEdge   = -1
  integer, parameter :: MAPL_DimTopoCenter = 1

! The code is mostly general for N dimensions, but in fact works only
! for transformations of 2-dimensional arrays.

  integer, parameter :: NUMDIMS = 2

  type Weights
     real, pointer :: f(:)
  end type Weights

  type Mapping
     type(Weights), pointer :: WeightList(:)
  end type Mapping

  type MAPL_HorzTransform
     private
     logical                :: created=.false.
     integer                :: Order
     integer                :: N_in  (NUMDIMS), N_out  (NUMDIMS)
     integer                :: topoIN(NUMDIMS), topoOUT(NUMDIMS)
     real                   :: XmaxIn(NUMDIMS), XmaxOut(NUMDIMS)
     real                   :: XminIn(NUMDIMS), XminOut(NUMDIMS)
     type(Mapping)          :: DimMapping(NUMDIMS)
     character(len=64)      :: gridtypeIN, gridtypeOUT
  end type MAPL_HorzTransform

  interface MAPL_HorzTransformCreate
     module procedure MAPL_HorzTransformCreateBySize
     module procedure MAPL_HorzTransformCreateGEOS
  end interface

  interface MAPL_HorzTransformRun
     module procedure MAPL_HorzTransformRun2
     module procedure MAPL_HorzTransformRun3
  end interface

  real, parameter :: wc = 0.7

contains

  logical function MAPL_HorzTransformIsCreated(Trans)
    type (MAPL_HorzTransform), intent(IN ) :: Trans

    MAPL_HorzTransformIsCreated = Trans%created

  end function MAPL_HorzTransformIsCreated

  subroutine MAPL_HorzTransformCreateGEOS  (Trans,           &
                                       im_in,  jm_in,   &
                                       im_out, jm_out,  &
                                       XYOFFSET, Order, &
                                       gridtypeIn,      &
                                       gridtypeOUT,     &
                                                      rc)

    type (MAPL_HorzTransform), intent(OUT) :: Trans
    integer,              intent(IN ) :: im_in, jm_in, im_out, jm_out
    integer, optional,    intent(IN ) :: XYOFFSET
    integer, optional,    intent(IN ) :: Order
    character(len=*),optional, intent(IN ) :: gridtypeIN
    character(len=*),optional, intent(IN ) :: gridtypeOut
    integer, optional,    intent(OUT) :: rc

    integer :: xyoffset_
    integer :: order_
    character(64):: gridtypeIN_
    character(64):: gridtypeOut_

! Specialized interface for use in GEOS5. Input is always on an FV grid;
! Output grid is determined by xyoffset=[0-3], which denote DCPC, DEPC, 
! DCPE, or DEPE, respectively. DCPC (0) and DEPE (3) are the most used. 
! The order of the interpolation can be 0=binning or 1=bilinear.

    if(present(GridTypeIn)) then
       GridTypeIn_ = trim(GridTypeIn)
    else
       GridTypeIn_ = 'UNKNOWN'
    endif

    if(present(GridTypeOut)) then
       GridTypeOut_ = trim(GridTypeOut)
    else
       GridTypeOut_ = 'UNKNOWN'
    endif

    order_ = 0
    if(present(Order)) order_ =Order

    Xyoffset_ = 0
    if(present(Xyoffset)) Xyoffset_ = Xyoffset

    select case(Xyoffset_)
    case(0)  ! FV to FV (DCPC)
       call MAPL_HorzTransformCreateBySize(Trans,                      &
                     (/ im_in              , jm_in            /), &
                     (/ im_out             , jm_out           /), &
                     (/ MAPL_DimTopoCyclic , MAPL_DimTopoEdge /), &
                     (/ MAPL_DimTopoCyclic , MAPL_DimTopoEdge /), &
                     (/ -180.              , -90.             /), &
                     (/ -180.              , -90.             /), &
                     (/  180.-(360./IM_in ),  90.             /), &
                     (/  180.-(360./IM_out),  90.             /), &
                     gridtypeIN_,                                 &
                     gridtypeOUT_,                                &
                     order_,                                      &
                                                               rc )
    case(1)  ! FV to FV with dateline edge (DEPC)
       call MAPL_HorzTransformCreateBySize(Trans,                      &
                     (/im_in              , jm_in             /), &
                     (/im_out             , jm_out            /), &
                     (/MAPL_DimTopoCyclic , MAPL_DimTopoEdge  /), &
                     (/MAPL_DimTopoCyclic , MAPL_DimTopoEdge  /), &
                     (/-180.              , -90.              /), &
                     (/-180.+(180./im_out), -90.              /), &
                     (/ 180.-(360./IM_in ),  90.              /), &
                     (/ 180.-(180./IM_out),  90.              /), &
                     gridtypeIN_,                                 &
                     gridtypeOUT_,                                &
                     order_,                                      &
                                                               rc )
    case(2)  !  FV to DCPE
       call MAPL_HorzTransformCreateBySize(Trans,                      &
                     (/im_in              , jm_in             /), &
                     (/im_out             , jm_out            /), &
                     (/MAPL_DimTopoCyclic , MAPL_DimTopoEdge  /), &
                     (/MAPL_DimTopoCyclic , MAPL_DimTopoCenter/), &
                     (/-180.              , -90.              /), &
                     (/-180.              , -90.+(90./jm_out) /), &
                     (/ 180.-(360./IM_in ),  90.              /), &
                     (/ 180.-(360./IM_out),  90.-(90./jm_out) /), &
                     gridtypeIN_,                                 &
                     gridtypeOUT_,                                &
                     order_,                                      &
                                                               rc )
    case(3)  !  FV to DEPE
       call MAPL_HorzTransformCreateBySize(Trans,                      &
                     (/im_in              , jm_in             /), &
                     (/im_out             , jm_out            /), &
                     (/MAPL_DimTopoCyclic , MAPL_DimTopoEdge  /), &
                     (/MAPL_DimTopoCyclic , MAPL_DimTopoCenter/), &
                     (/-180.              , -90.              /), &
                     (/-180.+(180./im_out), -90.+(90./jm_out) /), &
                     (/ 180.-(360./IM_in ),  90.              /), &
                     (/ 180.-(180./IM_out),  90.-(90./jm_out) /), &
                     gridtypeIN_,                                 &
                     gridtypeOUT_,                                &
                     order_,                                      &
                                                               rc )
    case default
       ASSERT_(.false.)
    end select

  end subroutine MAPL_HorzTransformCreateGEOS



  subroutine MAPL_HorzTransformCreateBySize(Trans,           &
                                       N_in ,  N_out,   &
                                       topoIN, topoOUT, &
                                       FirstIN,FirstOUT,&
                                       LastIN, LastOUT, &
                                       gridtypeIN,      &
                                       gridtypeOUT,     &
                                       order,           &
                                                    rc)
    type (MAPL_HorzTransform), intent(OUT) :: Trans
    integer,              intent(IN ) :: N_in    (NUMDIMS)
    integer,              intent(IN ) :: N_out   (NUMDIMS)
    integer,              intent(IN ) :: topoIN  (NUMDIMS) 
    integer,              intent(IN ) :: topoOUT (NUMDIMS) 
    real,                 intent(IN ) :: FirstIN (NUMDIMS)
    real,                 intent(IN ) :: LastIN  (NUMDIMS)
    real,                 intent(IN ) :: FirstOUT(NUMDIMS)
    real,                 intent(IN ) :: LastOUT (NUMDIMS)
    integer,              intent(IN ) :: order
    character(len=*),     intent(IN ) :: gridtypeIN
    character(len=*),     intent(IN ) :: gridtypeOUT
    integer, optional,    intent(OUT) :: rc

    integer :: status
    integer :: i, j
    real    :: rngIn, rngOut

    real, allocatable :: xin(:), xout(:)

    if(present(rc)) rc = 0

    Trans%N_in    = N_in
    Trans%N_out   = N_out
    Trans%topoIN  = topoIN
    Trans%topoOUT = topoOUT
    Trans%XminIn  = FirstIN
    Trans%XmaxIn  = LastIN  
    Trans%XminOut = FirstOUT
    Trans%XmaxOut = LastOUT
    Trans%Order   = order
    Trans%GridTypeIn = GridTypeIn
    Trans%GridTypeOut= GridTypeOut

    ASSERT_(all(Trans%N_in >1))
    ASSERT_(all(Trans%N_out>1))

    do i=1,NUMDIMS

       if(topoIN(i)==MAPL_DimTopoEdge) then
          ASSERT_(Trans%XminIn(i)<=Trans%XminOut(i))
          ASSERT_(Trans%XmaxIn(i)>=Trans%XmaxOut(i))
       end if

       if(topoIN(i)==MAPL_DimTopoCyclic) then
          ASSERT_(topoOUT(i)==MAPL_DimTopoCyclic)
       end if

       if(topoOUT(i)==MAPL_DimTopoCyclic) then
          ASSERT_(topoIN(i)==MAPL_DimTopoCyclic)
       end if

       allocate(Trans%DimMapping(i)%WeightList(n_out(i)), stat=STATUS)
       VERIFY_(STATUS)

       if(topoIN(i)==MAPL_DimTopoCyclic) then
          allocate(xin (2*Trans%N_in (i)+1),stat=STATUS)
          VERIFY_(STATUS)
          allocate(xout(  Trans%N_out(i)+1),stat=STATUS)
          VERIFY_(STATUS)
       elseif(order==0) then
          allocate(xin (  Trans%N_in (i)+1),stat=STATUS)
          VERIFY_(STATUS)
          allocate(xout(  Trans%N_out(i)+1),stat=STATUS)
          VERIFY_(STATUS)
       else
          allocate(xin (  Trans%N_in (i)  ),stat=STATUS)
          VERIFY_(STATUS)
          allocate(xout(  Trans%N_out(i)  ),stat=STATUS)
          VERIFY_(STATUS)
       endif

       call GetX(xin , Trans%N_in(i) , Trans%XminIn (i), Trans%XmaxIn (i),  &
            Trans%TopoIn (i), order, rc=status)
       VERIFY_(STATUS)

       call GetX(xout, Trans%N_out(i), Trans%XminOut(i), Trans%XmaxOut(i),  &
            Trans%TopoOut(i), order, rc=status)
       VERIFY_(STATUS)

       if(topoIN(i)==MAPL_DimTopoCyclic) then
          rngIn  = ((Trans%XmaxIn (i)-Trans%XminIn (i))*Trans%N_in (i))/(Trans%N_in (i)-1)
          rngOut = ((Trans%XmaxOut(i)-Trans%XminOut(i))*Trans%N_out(i))/(Trans%N_out(i)-1)

          ASSERT_(abs( (rngIn-rngOut)/rngIn ) < 1.e-5)

          if(xout(1)<xin(1)) then
             xout  = xout + int((xin(1)-xout(1))/rngIn+1)*rngIn
          else
             xout  = xout + int((xin(1)-xout(1))/rngIn)*rngIn
          end if
       end if

       ASSERT_(xin(size(xin)) >= xout(size(xout)))
       ASSERT_(xin(        1) <= xout(         1))

       select case (order)
       case(0)
          call ComputeDimBinWeights(Trans%DimMapping(i)%WeightList,Xin,Xout, &
           HasPoles=(topoIN(i)==MAPL_DimTopoEdge.and.topoOUT(i)==MAPL_DimTopoEdge),&
           rc=status)
          VERIFY_(STATUS)
       case(1)
          call ComputeDimLinWeights(Trans%DimMapping(i)%WeightList,Xin,Xout,rc=status)
          VERIFY_(STATUS)
       case default
          ASSERT_(.false.)
       end select

!       ASSERT_(all(Trans%DimMapping(i)%WeightList(:)%n0<=2*Trans%N_in(i)))

       deallocate(Xin )
       deallocate(Xout)

    end do

    Trans%created = .true.

    RETURN_(SUCCESS)

  end subroutine MAPL_HorzTransformCreateBySize

  subroutine GetX(X,N,xmin,xmax,topo,order,rc)

    real,                 intent(OUT) :: x(:)
    integer,              intent(IN ) :: N
    integer,              intent(IN ) :: topo
    integer,              intent(IN ) :: order
    real,                 intent(IN ) :: xmin
    real,                 intent(IN ) :: xmax
    integer, optional,    intent(OUT) :: rc

    integer :: status, j, jm
    real    :: dx

    jm = size(X)
    dx = (Xmax-xmin) / (N-1)

    if(order==1) then
       x(1)       = xmin
    else
       x(1)       = xmin-0.5*dx
    end if

    do j=2,JM
       x(j) = x(1) + (j - 1)*dx
    end do

    if(topo==MAPL_DimTopoEdge  ) then
       x( 1) = xmin
       x(JM) = xmax
    end if

    RETURN_(SUCCESS)
  end subroutine GetX


  subroutine ComputeDimLinWeights(Weight,Xin,Xout,rc)
    
    type(Weights),     intent(INOUT) :: Weight(:)
    real,              intent(IN   ) :: Xin(:), Xout(:)
    integer, optional, intent(OUT  ) :: rc


    ! Compute weights for binned interpolation along a dimension.
    ! Xout are the N_in + 1 input bin edges.
    ! Xin  are the N_out + 1 output bin edges
    ! Weigths are the mapping


    integer :: j_out, j0, j1
    integer :: N_in
    integer :: status
    real, pointer :: b(:)

    N_in  = size(Xin )

    do j_out=1,size(Weight)
       j0 = 1
       do           
          if(Xout(j_out  )<=Xin(j0+1)) exit
          j0=j0+1
          ASSERT_(j0 < N_in )
       end do
       j1 = j0 + 1

       allocate(b(j0:j1), stat=STATUS)
       VERIFY_(STATUS)

       b(j0  ) = (Xin(j1)-Xout(j_out))/(Xin(j1)-Xin(j0))
       b(j0+1) = 1.0 - b(j0)

       Weight(j_out)%f  => b

    end do

  end subroutine ComputeDimLinWeights

  subroutine ComputeDimBinWeights(Weight,Xin,Xout,HasPoles,rc)
    
    type(Weights),     intent(INOUT) :: Weight(:)
    real,              intent(IN   ) :: Xin(:), Xout(:)
    logical,           intent(IN   ) :: HasPoles
    integer, optional, intent(OUT  ) :: rc


    ! Compute weights for binned interpolation along a dimension.
    ! Xout are the N_in + 1 input bin edges.
    ! Xin  are the N_out + 1 output bin edges
    ! Weigths are the mapping


    integer :: j_out, j0, j1, j
    integer :: N_in, N_out
    integer :: status
    real    :: dx, ff
    real, pointer :: b(:)

    N_in  = size(Xin )-1
    N_out = size(Weight)

    do j_out=1,N_out
       j0 = 1
       do           
          if(Xout(j_out  )>=Xin(j0) .and. Xout(j_out  )<=Xin(j0+1)) exit
          j0=j0+1
          ASSERT_(j0 <= N_in )
       end do

       j1 = j0
       do
          if(Xout(j_out+1)>=Xin(j1) .and. Xout(j_out+1)<=Xin(j1+1)) exit
          j1=j1+1
          ASSERT_(j1 <= N_in )
       end do

       allocate(b(j0:j1), stat=STATUS)
       VERIFY_(STATUS)

       if(j0==j1) then
          b(j0) = 1.
       else
          dx    = Xin(j0+1)-Xout(j_out)
          ff    = dx
          b(j0) = dx
          do j=j0+1,j1-1
             dx   = Xin(j+1) - Xin(j)
             ff   = ff + dx
             b(j) = dx
          end do
          dx    = Xout(j_out+1)-Xin(j1)
          ff    = ff + dx
          b(j1) = dx
          b     = b/ff
       end if

       Weight(j_out)%f  => b

    end do

    if(HasPoles) then
       deallocate(Weight(    1)%f)
       deallocate(Weight(N_out)%f)
       allocate  (Weight(    1)%f(1   :1   ))
       allocate  (Weight(N_out)%f(N_in:N_in))
       Weight(    1)%f  =  1.
       Weight(N_out)%f  =  1.
    endif

  end subroutine ComputeDimBinWeights

  subroutine DestroyMapping(MAP, rc)
    type (Mapping),    intent(INOUT) :: Map
    integer, optional, intent(  OUT) :: rc

    integer :: i
    
    do i=1,size(MAP%WeightList)
       DEALOC_(MAP%WeightList(i)%f)
    end do

    DEALOC_(Map%Weightlist)

    RETURN_(SUCCESS)
  end subroutine DestroyMapping

  subroutine MAPL_HorzTransformDestroy(Trans, rc)
    type (MAPL_HorzTransform), intent(INOUT) :: Trans
    integer, optional,    intent(  OUT) :: rc

    integer :: status
    integer :: i, j

    if(Trans%created) then

       do I=1,NUMDIMS
          call DestroyMapping(Trans%DimMapping(i), RC=STATUS)
          VERIFY_(STATUS)
       enddo

       trans%created = .false.

    endif

    RETURN_(SUCCESS)
  end subroutine MAPL_HorzTransformDestroy

  subroutine MAPL_HorzTransformRun2(Trans, qin, qout, undef, rc)

!    Trans ....  Precomputed transform
!      qin ....  Input Variable
!      qout....  Output Variable
!    undef ....  UNDEF Value

    type (MAPL_HorzTransform), intent(IN   ) :: Trans
    real,                    intent(IN   ) :: qin (:,:)
    real,                    intent(OUT  ) :: qout(:,:)
    real,    optional,       intent(IN   ) :: undef
    integer, optional,       intent(  OUT) :: rc

    real            :: undef_
    real            :: q, w, f
    integer         :: STATUS
    integer         :: i,j
    integer         :: i0,i1,j0,j1
    integer         :: ii,jj,jx,ix
    real, pointer   :: fx(:), fy(:)
    
    logical :: doCube2Latlon, doLatlon2Cube
    integer :: npx, npy
    integer :: nlon, nlat

    if(present(rc)) rc = 0
    
    if(present(undef)) then
       undef_ = undef
    else
       undef_ = huge(undef_)
    end if

    ASSERT_(all(shape(qin )==Trans%N_in ))
    ASSERT_(all(shape(qout)==Trans%N_out))

    doCube2Latlon = Trans%gridtypeIN =='Cubed-Sphere'
    doLatlon2Cube = Trans%gridtypeOUT=='Cubed-Sphere'

    if (doCube2Latlon) then 

       npx  = Trans%N_in (1)
       npy  = Trans%N_in (2)
       nlon = Trans%N_out(1)
       nlat = Trans%N_out(2)
       call cube2latlon(npx, npy, nlon, nlat, qin, qout)

    elseif(doLatlon2Cube) then  ! stubbed

       ASSERT_(.false.)
       npx  = Trans%N_in (1)
       npy  = Trans%N_in (2)
       nlon = Trans%N_out(1)
       nlat = Trans%N_out(2)
!       call Latlon2Cube(npx, npy, nlon, nlat, qin, qout)

    else

    do j=1,Trans%N_out(2)
       j0 = lbound(Trans%DimMapping(2)%WeightList(j)%f,1)
       j1 = ubound(Trans%DimMapping(2)%WeightList(j)%f,1)
       fy =>Trans%DimMapping(2)%WeightList(j)%f

       do i=1,Trans%N_out(1)
          i0 = lbound(Trans%DimMapping(1)%WeightList(i)%f,1)
          i1 = ubound(Trans%DimMapping(1)%WeightList(i)%f,1)
          fx =>Trans%DimMapping(1)%WeightList(i)%f

          q = 0.0
          w = 0.0

          do jj=j0,j1
             if(jj>Trans%N_in(2)) then
                jx = jj - Trans%N_in(2)
             else
                jx = jj
             end if

             do ii=i0,i1
                if(ii>Trans%N_in(1)) then
                   ix = ii - Trans%N_in(1)
                else
                   ix = ii
                end if

                if(qin(ix,jx) /= undef_) then
                   f = fx(ii)*fy(jj)
                   q = q + f*qin(ix,jx)
                   w = w + f           
                end if
             end do
          end do

          if ( w >= wc ) then
             qout(i,j) = q / w
          else
             qout(i,j) = undef_
          end if

       end do
    end do

    endif

    RETURN_(SUCCESS)
  end subroutine MAPL_HorzTransformRun2

  subroutine MAPL_HorzTransformRun3(Trans, qin, qout, undef, rc)

!    Trans ....  Precomputed transform
!      qin ....  Input Variable
!      qout....  Output Variable
!    undef ....  UNDEF Value

    type (MAPL_HorzTransform), intent(IN   ) :: Trans
    real,                    intent(IN   ) :: qin (:,:,:)
    real,                    intent(OUT  ) :: qout(:,:,:)
    real,    optional,       intent(IN   ) :: undef
    integer, optional,       intent(  OUT) :: rc

    real          :: undef_
    real          :: q(size(qin,3))
    real          :: w(size(qin,3))
    real          :: f
    integer       :: STATUS
    integer       :: i,j,k
    integer       :: i0,i1,j0,j1
    integer       :: ii,jj,jx,ix
    integer       :: sh(3)

    logical :: doCube2Latlon, doLatlon2Cube
    integer :: npx, npy, npz
    integer :: nlon, nlat

    real, pointer :: fx(:), fy(:)

    if(present(rc)) rc = 0
    
    if(present(undef)) then
       undef_ = undef
    else
       undef_ = huge(undef_)
    end if

    sh = shape(qin )
    ASSERT_(all(sh(1:2)==Trans%N_in ))
    sh = shape(qout)
    ASSERT_(all(sh(1:2)==Trans%N_out))

    ASSERT_(size(qin,3)==size(qout,3))

    doCube2Latlon = Trans%gridtypeIN =='Cubed-Sphere'
    doLatlon2Cube = Trans%gridtypeOUT=='Cubed-Sphere'

    if (doCube2Latlon) then 

       npx  = Trans%N_in(1)
       npy  = Trans%N_in(2)
       npz  = size(qin,3)
       nlon = Trans%N_out(1)
       nlat = Trans%N_out(2)
       do k=1,npz
          call cube2latlon(npx, npy, nlon, nlat, qin(:,:,k), qout(:,:,k))
       enddo

    elseif(doLatlon2Cube) then  ! stubbed

       ASSERT_(.false.)
       npx  = Trans%N_in (1)
       npy  = Trans%N_in (2)
       npz  = size(qin,3)
       nlon = Trans%N_out(1)
       nlat = Trans%N_out(2)
       do k=1,npz
!       call Latlon2Cube(npx, npy, nlon, nlat, qin(:,:,k), qout(:,:,k))
       end do
    else

    do j=1,Trans%N_out(2)
       j0 = lbound(Trans%DimMapping(2)%WeightList(j)%f,1)
       j1 = ubound(Trans%DimMapping(2)%WeightList(j)%f,1)
       fy =>Trans%DimMapping(2)%WeightList(j)%f

       do i=1,Trans%N_out(1)
          i0 = lbound(Trans%DimMapping(1)%WeightList(i)%f,1)
          i1 = ubound(Trans%DimMapping(1)%WeightList(i)%f,1)
          fx =>Trans%DimMapping(1)%WeightList(i)%f

          q = 0.0
          w = 0.0

          do jj=j0,j1
             if(jj>Trans%N_in(2)) then
                jx = jj - Trans%N_in(2)
             else
                jx = jj
             end if

             do ii=i0,i1
                if(ii>Trans%N_in(1)) then
                   ix = ii - Trans%N_in(1)
                else
                   ix = ii
                end if

                f = fx(ii)*fy(jj)

                where(qin(ix,jx,:) /= undef_)
                   q = q + f*qin(ix,jx,:)
                   w = w + f           
                end where
             end do
          end do

          where( w >= wc )
             qout(i,j,:) = q / w
          elsewhere
             qout(i,j,:) = undef_
          end where

       end do
    end do

    endif

    RETURN_(SUCCESS)
  end subroutine MAPL_HorzTransformRun3

end module MAPL_HorzTransformMod
