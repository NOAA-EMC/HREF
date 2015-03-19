#include "../../../ESMFVersionDefine.h"

#if (ESMF_MAJOR_VERSION < 5 || ESMF_MINOR_VERSION < 2)
#undef ESMF_520r
#else
#define ESMF_520r
#endif

      module module_digital_filter_gfs
!
! a generic digital filter for any model under ESMF 
!
! March 2007	Hann-Ming Henry Juang
! February 2008 Weiyu Yang, updated to use the ESMF 3.1.0 library.
! November 2009 Jun Wang, digital filter is done on n+1 time step.
! February 2011 Weiyu Yang, Updated to use both the ESMF 4.0.0rp2 library,
!                           ESMF 5 library and the the ESMF 3.1.0rp2 library.
! May      2011 Weiyu Yang, Modified for using the ESMF 5.2.0r_beta_snapshot_07.
! Sep      2011 Weiyu Yang, Modified for using the ESMF 5.2.0r library.
!----------------------------------------------------------------------------
!
      USE esmf_mod

      implicit none

! ---------
! dynamics
! ---------
      real(8), allocatable, save :: dyn_array_save(:,:,:,:)
      real(8)             , save :: totalsum
      character(20), allocatable, save :: dyn_name(:)
      integer,       allocatable, save :: dyn_dim(:,:)
      integer,       allocatable, save :: dyn_items_ord(:)
      integer,                    save :: dyn_items, kstep, nstep
      integer,                    save :: lvl_change
      character(20)	state_name
! ---------
! physics
! ---------
      type(esmf_state) , save :: phy_state_save
      character(20), allocatable, save :: phy_name(:)
      integer		phy_items


      contains

! ---------------------------------------------------------------
! subroutine for dynamics
! ---------------------------------------------------------------
      subroutine digital_filter_dyn_init_gfs(dyn_state,ndfistep,dfilevs)
!
      implicit none
      type(esmf_state), intent(in) ::   dyn_state
      integer,          intent(in) ::   ndfistep
      integer,          intent(in) ::   dfilevs
!
      TYPE(ESMF_VM)                :: vm
      TYPE(ESMF_Field)             :: FIELD
      type(esmf_Grid)              :: GRID
      type(esmf_DistGrid)          :: distGRID
      type(ESMF_DELayout)          :: LAYOUT
      integer                      :: gridRank, dim_max(3)
      integer                      :: m,n,rc,ierr,dyn_items_tmp
      integer                      :: me, nodes,nDes,deId
      integer                      :: deList(1)

      INTEGER, DIMENSION(:, :), POINTER :: AL, AU
!
!      CALL ESMF_VMGetCurrent(vm, rc = rc)
!      CALL ESMF_VMGet(vm, localpet = me, petcount = nodes, rc = rc)

      nstep = ndfistep 
      kstep = - nstep -1
      lvl_change = dfilevs
      call esmf_stateget(state     = dyn_state 				&
                        ,name      = state_name 			&
                        ,itemcount = dyn_items_tmp 			&
                        ,rc=rc)
!      print *,' in digital_filter_dyn_init itemcount = ',dyn_items_tmp
      allocate(dyn_name(dyn_items_tmp))
      allocate(dyn_items_ord(dyn_items_tmp))
      allocate(dyn_dim (3,dyn_items_tmp))

      call esmf_stateget(state=dyn_state 				&
                        ,itemnamelist = dyn_name 			&
                        ,rc=rc)
!      print *,' in digital_filter_dyn_init itemname = ',dyn_name
      dim_max   = 0
      dyn_items = 0
      do n=1,dyn_items_tmp
        if(index(trim(dyn_name(n)),"_dfi")>0) then
          dyn_items = dyn_items + 1
          dyn_items_ord(dyn_items) = n

          CALL ESMF_StateGet(dyn_state, ItemName=dyn_name(n),           &
                             field=Field, rc = rc)
!           print *,'dfi dyn name=',trim(dyn_name(n))
!
          call esmf_Fieldget(FIELD        				&
                            ,grid              = grid                   &
                            ,rc                = rc)
!
          call ESMF_GridGet    (GRID, dimCount=gridRank, distGrid=distGrid, rc=rc)
          call ESMF_DistGridGet(distGRID, delayout=layout, rc=rc)
          call ESMF_DELayoutGet(layout, deCount =nDEs, localDeList=deList, rc=rc )
          deId = deList(1)
!          print *,'get grid info,gridRank=',gridRank,'ndes=',ndes,'deId=',deId
          if(gridRank /= 3) then
            print *,'WARNING: the rank of digital filter variables is not 3!'
            stop
          endif
!
          allocate (AL(gridRank,0:nDEs-1), stat = ierr )
          allocate (AU(gridRank,0:nDEs-1), stat = ierr )
!
          call ESMF_DistGridGet(distgrid, &
           minIndexPDimPDe=AL, maxIndexPDimPDe=AU, rc=rc )

          do m=1,gridRank
            dyn_dim(m,dyn_items) = AU(m, deId)-AL(m, deId)+1
            dim_max(m) = max(dyn_dim(m,dyn_items),dim_max(m))
          enddo
          if(trim(dyn_name(n))=='hs_dfi' .or. trim(dyn_name(n))=='ps_dfi') &
             dyn_dim(3,dyn_items) = 1

          print *,' in digital_filter_init itemname = ',trim(dyn_name(n)),'dim=', &
             dyn_dim(:,dyn_items),'dim_max=',dim_max
!           
          deallocate(AU, AL)
        endif
      enddo

      if(minval(dim_max) > 1) then
        allocate(dyn_array_save(dim_max(1),dim_max(2),dim_max(3),dyn_items))
 
        totalsum = 0.0
        dyn_array_save = 0.0
      endif
!      
      end subroutine digital_filter_dyn_init_gfs

! ---------------------------------------------------------------
      subroutine digital_filter_dyn_sum_gfs(dyn_state)
!
      implicit none
      type(esmf_state), intent(in)  :: dyn_state
!
      TYPE(ESMF_Field)		    :: Field
      real(ESMF_KIND_R8), dimension(:,:), pointer :: tmp_ptr2d
      real(ESMF_KIND_R8), dimension(:,:,:), pointer :: tmp_ptr
      real(8)                          :: sx, wx, digfil
      integer                       :: n, i, rc,item,dim1,dim2,dim3

        kstep = kstep + 1
        sx     = acos(-1.)*kstep/nstep
        wx     = acos(-1.)*kstep/(nstep+1)
        if( kstep.ne.0 ) then
            digfil = sin(wx)/wx*sin(sx)/sx
        else
            digfil=1
        endif 

!         print *,' in digital_filter_sum digfil =',digfil,'kstep=',kstep,'nstep=',nstep,  &
!           '=',lvl_change
        totalsum = totalsum + digfil
        
        do n=1,dyn_items
          item=dyn_items_ord(n)
          CALL ESMF_StateGet(dyn_state, ItemName=dyn_name(item),      &
                 Field=FIELD, rc = rc)
          dim1=dyn_dim(1,n)
          dim2=dyn_dim(2,n)
          dim3=dyn_dim(3,n)
          if(dim3==1) then
            nullify(tmp_ptr2D)

            CALL ESMF_FieldGet(FIELD, localDe = 0, farrayPtr = tmp_ptr2D, rc = rc) 
            dyn_array_save(1:dim1,1:dim2,1,n)=                     &
               dyn_array_save(1:dim1,1:dim2,1,n)+                    &
               digfil*tmp_ptr2D(1:dim1,1:dim2)
!            print *,'dfi sum,2D dyn_name=',dyn_name(item),'rc=',rc,'value=', &
!              maxval(tmp_ptr2D),minval(tmp_ptr2D),'dyn_dfy_save=',&
!              maxval(dyn_array_save(1:dim1,1:dim2,1,n)),minval(dyn_array_save(1:dim1,1:dim2,1,n))
          else
            nullify(tmp_ptr)

            CALL ESMF_FieldGet(FIELD, localDe = 0, farrayPtr = tmp_ptr, rc = rc) 
            if(lvl_change/=dim3) then
              if(kstep==0) then
                dyn_array_save(1:dim1,1:dim2,lvl_change+1:dim3,n)=                     &
                  tmp_ptr(1:dim1,1:dim2,lvl_change+1:dim3)
              endif
              dyn_array_save(1:dim1,1:dim2,1:lvl_change,n)=                     &
                dyn_array_save(1:dim1,1:dim2,1:lvl_change,n)+                   &
                digfil*tmp_ptr(1:dim1,1:dim2,1:lvl_change)
            else
              
              dyn_array_save(1:dim1,1:dim2,1:dim3,n)=                     &
                dyn_array_save(1:dim1,1:dim2,1:dim3,n)+                    &
                digfil*tmp_ptr(1:dim1,1:dim2,1:dim3)
!            print *,'dfi sum,dyn_name=',dyn_name(item),'rc=',rc,'value=', &
!              maxval(tmp_ptr(1:dim1,1:dim2,1:dim3)),minval(tmp_ptr(1:dim1,1:dim2,1:dim3)), &
!              'ydn_save=',maxval(dyn_array_save(1:dim1,1:dim2,1:dim3,n)), &
!              minval(dyn_array_save(1:dim1,1:dim2,1:dim3,n))
           endif
         endif
        enddo

      end subroutine digital_filter_dyn_sum_gfs

! ---------------------------------------------------------------
      subroutine digital_filter_dyn_average_gfs(dyn_state)
!
      implicit none
      type(esmf_state), intent(inout) :: dyn_state
!
      TYPE(ESMF_Field)                :: FIELD
      real(ESMF_KIND_R8), dimension(:,:,:), pointer :: tmp_ptr
      real(ESMF_KIND_R8), dimension(:,:), pointer :: tmp_ptr2D
      real(ESMF_KIND_R8), dimension(:,:,:), pointer :: tmp_ptr1
      real(8)                         :: totalsumi
      integer                         :: n, i, rc,item
      integer                         :: dim1,dim2,dim3
!
      totalsumi = 1.0 / totalsum

      do n=1,dyn_items
!
!
        item=dyn_items_ord(n)
        CALL ESMF_StateGet(dyn_state, ItemName=dyn_name(item),            &
              Field=FIELD, rc = rc)

        dim1=dyn_dim(1,n)
        dim2=dyn_dim(2,n)
        dim3=dyn_dim(3,n)
        if(dim3==1) then
          nullify(tmp_ptr2D)

            dyn_array_save(:,:,1,n)=dyn_array_save(:,:,1,n)*totalsumi
 
            CALL ESMF_FieldGet(FIELD, localDe = 0, farrayPtr = tmp_ptr2D, rc = rc) 
!          print *,'dfi dyn save field ',trim(dyn_name(item)),'rc=',rc,'value=',  &
!           maxval(tmp_ptr2D),minval(tmp_ptr2D),'dyn_save=',  &
!           maxval(dyn_array_save(1:dim1,1:dim2,1,n)), &
!           minval(dyn_array_save(1:dim1,1:dim2,1,n))
          tmp_ptr2D(1:dim1,1:dim2) = dyn_array_save(1:dim1,1:dim2,1,n)
        else
          nullify(tmp_ptr)

             dyn_array_save(:,:,1:lvl_change,n)=dyn_array_save(:,:,1:lvl_change,n)*totalsumi
            

            CALL ESMF_FieldGet(FIELD, localDe = 0, farrayPtr = tmp_ptr, rc = rc) 
!          print *,'dfi dyn save field ',trim(dyn_name(item)),'=',  &
!           maxval(tmp_ptr),minval(tmp_ptr)
          tmp_ptr(1:dim1,1:dim2,1:dim3) = dyn_array_save(1:dim1,1:dim2,1:dim3,n)
!
!jun testing
           CALL ESMF_FieldGet(FIELD, localDe=0, farrayPtr=tmp_ptr1, rc = rc) 
           do i=1,dim3
             print *,'field is updated lvl=',i,' data=',maxval(tmp_ptr1(1:dim1,1:dim2,i)),minval(tmp_ptr1(1:dim1,1:dim2,i))
           enddo

        endif
!
!
      enddo

      if(allocated(dyn_name)) deallocate(dyn_name)
      if(allocated(dyn_name)) deallocate(dyn_dim)
      if(allocated(dyn_array_save)) deallocate(dyn_array_save)

      end subroutine digital_filter_dyn_average_gfs

! ---------------------------------------------------------------
! subroutines for physics
! ---------------------------------------------------------------
      subroutine digital_filter_phy_init_gfs(phy_state)
!
      implicit none
      type(esmf_state), intent(in) :: phy_state
!
      integer 			rc

      call esmf_stateget(state     = phy_state,				&
                         itemcount = phy_items,				&
                         rc=rc)
      allocate(phy_name(phy_items))
      call esmf_stateget(state=phy_state,				&
                         itemnamelist = phy_name,			&
                         rc=rc)
!       print *,'dfi phy init, phy_items=',phy_items,'names=',phy_name(1:phy_items)
      phy_state_save=esmf_statecreate(STATENAME  ="digital filter phy"    	&
                                     ,stateintent=ESMF_STATEINTENT_UNSPECIFIED &
                                     ,rc         =rc)
!
      end subroutine digital_filter_phy_init_gfs

! ---------------------------------------------------------------
      subroutine digital_filter_phy_save_gfs(phy_state)
!
      implicit none
      type(esmf_state), intent(in) :: phy_state
!
!jw      TYPE(ESMF_Array)             :: tmp_array
      TYPE(ESMF_Field)             :: tmp_field
      TYPE(ESMF_FieldBUNDLE)       :: tmp_bundle
      TYPE(ESMF_STATE)             :: tmp_state
      type(ESMF_StateItemType)     :: itemtype

      integer                      :: n, rc
!
      do n=1,phy_items

        CALL ESMF_StateGet(phy_state, phy_name(n),itemtype,rc=rc)
!        print *,'restor data,phy_name=',phy_name(n),'itemtype=',itemtype,'rc=',rc

        if(itemtype==ESMF_STATEITEM_FIELDBUNDLE) then
          CALL ESMF_StateGet(phy_state, phy_name(n), tmp_bundle, rc = rc)
          CALL ESMF_StateAdd(phy_state_save, LISTWRAPPER(tmp_bundle), rc = rc)
!        print *,'save bundle data,phy_name=',phy_name(n),'rc=',rc
        else if(itemtype==ESMF_STATEITEM_FIELD) then
          CALL ESMF_StateGet(phy_state, phy_name(n), tmp_field, rc = rc)
          CALL ESMF_StateAdd(phy_state_save, LISTWRAPPER(tmp_field), rc = rc)
!        print *,'save field data,phy_name=',phy_name(n),'rc=',rc
        else if(itemtype==ESMF_STATEITEM_STATE) then
          CALL ESMF_StateGet(phy_state, phy_name(n), tmp_state, rc = rc)
          CALL ESMF_StateAdd(phy_state_save, LISTWRAPPER(tmp_state), rc = rc)
!        print *,'save state data,phy_name=',phy_name(n),'rc=',rc
        endif

      enddo
      end subroutine digital_filter_phy_save_gfs

! ---------------------------------------------------------------
      subroutine digital_filter_phy_restore_gfs(phy_state)
!
      implicit none
      type(esmf_state), intent(inout) :: phy_state
!
      TYPE(ESMF_field)                :: tmp_field
      TYPE(ESMF_FieldBundle)          :: tmp_bundle
      TYPE(ESMF_STATE)                :: tmp_state
      type(ESMF_StateItemType)        :: itemtype
      integer                         :: n, rc
!
      do n=1,phy_items

        CALL ESMF_StateGet(phy_state_save, phy_name(n),itemtype,rc=rc)
!        print *,'restor data,phy_name=',phy_name(n),'itemtype=',itemtype,'rc=',rc

        if(itemtype==ESMF_STATEITEM_FIELDBUNDLE) then
          CALL ESMF_StateGet(phy_state_save, phy_name(n), tmp_bundle, rc = rc)
          CALL ESMF_StateAdd(phy_state, LISTWRAPPER(tmp_bundle), rc = rc)
!          print *,'restor bundle, ',trim(phy_name(n)),'rc=',rc
        else if(itemtype==ESMF_STATEITEM_FIELD) then
          CALL ESMF_StateGet(phy_state_save, phy_name(n), tmp_field, rc = rc)
          CALL ESMF_StateAdd(phy_state, LISTWRAPPER(tmp_field), rc = rc)
!          print *,'restor field, ',trim(phy_name(n)),'rc=',rc
        else if(itemtype==ESMF_STATEITEM_STATE) then
          CALL ESMF_StateGet(phy_state_save, phy_name(n), tmp_state, rc = rc)
          CALL ESMF_StateAdd(phy_state, LISTWRAPPER(tmp_state), rc = rc)
!          print *,'restor state, ',trim(phy_name(n)),'rc=',rc
        endif
      enddo
      call esmf_statedestroy(phy_state_save,rc=rc)

      end subroutine digital_filter_phy_restore_gfs


      end module module_digital_filter_gfs
