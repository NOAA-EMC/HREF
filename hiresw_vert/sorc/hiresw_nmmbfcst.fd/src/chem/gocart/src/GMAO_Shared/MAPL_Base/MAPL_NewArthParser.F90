#include "MAPL_Generic.h"
#define  locate_(num) num=__LINE__
#define  tiny  1.000e-19
#define  large 9.999e+29

module MAPL_NewArthParserMod

! !MODULE: MAPL_NewArthParserMod

#ifdef __Protex__
! !DESCRIPTION:
! {\tt MAPL\_NewArthParserMod} has been designed to provide arithmetic parsing operations
! for arbitary numbers of given fields within a collection. MAPL_NewArthParserMod contains two 
! public member subroutines which are {\tt MAPL\_SetExpression} and {\tt MAPL\_RunExpression}.
! {\tt MAPL\_SetExpression} sets rewrite flag and tmpfields. The rewrite flag provides the flag (.true. or .false.)
! whether the field variable needs rewriting or not. If the field contains the arithmetic parsing expression,
! the rewrite flag is `.true.' and then the field needs to be calculated according to the arithmetic parsing
! expression. The tmpfields should contain temporarily information for the fields if the rewrite flag is 
! `.true.' ($tmpfields(1,:)$ is copy of the $fields(1,:)$ and $tmpfields(2,:)$ is the registory expression 
! of $tmpfields(1,:)$). 
! {\tt MAPL\_RunExpression} runs arithmetic parsing operations of the field by using given field variables. 
! It retrieve given fields and then it performs arithmetic parsing operations of the field according to the
! arithmetic parsing expression.
! Both {\tt MAPL\_SetExpression} and {\tt MAPL\_RunExpression} are called by the module 
! {\tt MAPL\_HistoryGridCompMod}.
! The module {\tt MAPL\_NewArthParserMod} is controlled by the HISTORY.rc file. 
! If the HISTORY.rc file does not contain the arithmetic parsing expression of the field,  
! the functionality of the module {\tt MAPL\_NewArthParserMod} would never be activated.    
!
! The following is a sample HISORY.rc, which contains the arithmetic parsing expression of the field. 
!
! \begin{verbatim}
!
!EXPID:  fvhs_example
!EXPDSC: fvhs_(ESMF07_EXAMPLE)_5x4_Deg
!
!COLLECTIONS:
!      'dynamics_vars_p'
!      ::
!
!dynamics_vars_p.template:   '%y4%m2%d2_%h2%n2z.hdf',
!dynamics_vars_p.format:     'CFIO',
!dynamics_vars_p.frequency:  030000,
!dynamics_vars_p.duration:   030000,
!dynamics_vars_p.resolution: 72 45,
!dynamics_vars_p.vscale:     100.0,
!dynamics_vars_p.xyoffset:   3,
!dynamics_vars_p.vunit:      'hPa',
!dynamics_vars_p.vvars:      'log(PLE)' , 'FVDYNAMICS'          ,
!dynamics_vars_p.levels:      1000 900 850 750 500 300 250 150 100 70 50 30 20 10 7 5 2 1 0.7,
!dynamics_vars_p.fields:     'T_EQ'     , 'HSPHYSICS'           , 
!                            'U'        , 'FVDYNAMICS'          , 
!                            'V'        , 'FVDYNAMICS'          , 
!                            'T'        , 'FVDYNAMICS'          , 
!                            'U*V'      , 'FVDYNAMICS'          , 'uv',
!                            'U+V'      , 'FVDYNAMICS'          , 'UVsum',
!                            'U^2.0'    , 'FVDYNAMICS'          , 'Upow',
!                            'V^2.0'    , 'FVDYNAMICS'          , 'Vpow',
!                            'Upow+Vpow', 'FVDYNAMICS'          , 'UVpow_sum',
!                            '(U^2.0+V^2.0)^0.5', 'FVDYNAMICS'  , 'Vel',
!                            'UVpow_sum^0.5-Vel', 'FVDYNAMICS'  , 'Vel0',
!                            '(Upow+Vpow+2.0*uv)-(U+V)^2.0+Vel', 'FVDYNAMICS', 'Vel1',
!                            '(U^2.0+V^2.0-V^2.0)^0.5'         , 'FVDYNAMICS', 'Vel2'
!                            '(U+V)^2.0-V^2.0-2.0*U*V'         , 'FVDYNAMICS', 'Upow2',
!                            '((-U-V)^2.0)^0.5'                , 'FVDYNAMICS', 'UVsum2',
!                            'T-T_EQ'   , 'FVDYNAMICS'          , 'Tc',
!                            'T+T+T'    , 'FVDYNAMICS'          , 'Tsum',
!                            'T/T_EQ'   , 'FVDYNAMICS'          , 'Tnormal',
!                            'T/273.0'  , 'FVDYNAMICS'          , 'Tn',
!                            'Tn*273.0' , 'FVDYNAMICS'          , 'To',
!                            '(Tsum^2.0)^0.5',  'FVDYNAMICS'    , 'Tsum_pow',
!                            '(Tc+T_EQ)/273.0', 'FVDYNAMICS'    , 'Tmix',
!                            '1000.0*U*V*T', 'FVDYNAMICS'       , 'UVT_scale',
!                            'UVT_scale/1000.0', 'FVDYNAMICS'   , 'UVT',
!                            'UVT/uv'   , 'FVDYNAMICS'          , 'Tk2',
!                            '(uv-T+T_EQ)/T_EQ', 'FVDYNAMICS'   , 'UVT_mix',
!                            'U+U*V-V^2.0-3.0*T*T_EQ+T_EQ^2.0', 'FVDYNAMICS', 'UVT_mix2',
!                      ::
!
!
!\end{verbatim}
#endif


! !USES:

  use ESMF_Mod
  use ESMFL_Mod
  use MAPL_BaseMod
  use MAPL_VarSpecMod
  use MAPL_ConstantsMod
  use MAPL_IOMod
  use MAPL_CommsMod
  use MAPL_GenericMod
  use MAPL_LocStreamMod
  use MAPL_CFIOMod
  use MAPL_GenericCplCompMod

  implicit none

  private

! !PUBLIC MEMEBER FUNCTIONS:

  public MAPL_SetExpression
  public MAPL_RunExpression


!EOP
!=====================================================================

! Other private types to be used on collection type
!--------------------------------------------------

  type Ptrs_Type
     integer:: rank
     integer, dimension(ESMF_MAXDIM):: lb,ub
     integer:: length
     character(len=ESMF_MAXSTR):: expr='',realexpr=''
     real         :: Q=0.0
     real, pointer:: Q1D(:    ) => null()
     real, pointer:: Q2D(:,:  ) => null()
     real, pointer:: Q3D(:,:,:) => null()
  end type Ptrs_Type

!===========================================================
contains
!===========================================================

! !IROUTINE: MAPL_SetExpression 

! !DESCRIPTION: MAPL_SetExpression sets tmpfields and rewrite flag.

! !INTERFACE:

subroutine MAPL_SetExpression(gc,nfield,fields,tmpfields,rewrite,tmprank,tmplb,tmpub,rc)

! !ARGUMENTS:

  type(ESMF_GridComp), intent(inout):: gc
  integer,intent(in)::nfield
  character*(*), intent(inout):: fields(:,:)
  character*(*), intent(inout):: tmpfields(:,:)
  type(ESMF_Logical),intent(inout):: rewrite(:)
  integer, intent(inout)::tmprank(:),tmplb(:,:),tmpub(:,:)
  integer, optional, intent(out) :: rc

! Local variables: 

  integer:: i,m,k,nb,nl,nv,status,len_list,flag,tmpflag
  character(len=3) :: indx=''
  character(len=ESMF_MAXSTR) :: expr,tmpexpr
  character(len=ESMF_MAXSTR) :: Iam,Comp_Name
  integer,allocatable:: mask(:)
  character(len=ESMF_MAXSTR),allocatable::tmpfields3(:)

! Get my name and set-up traceback handle
! ---------------------------------------
    Iam = "MAPL_SetExpression"
    call ESMF_GridCompGet(GC, NAME=COMP_NAME, RC=STATUS )
    VERIFY_(status)
    Iam = trim(COMP_NAME) // Iam

! Allocate local memory
!-------------------------------------------------------------------
  allocate(mask(1:nfield),stat=status); VERIFY_(status);  mask= 0
  allocate(tmpfields3(1:nfield),stat=status); VERIFY_(status); tmpfields3=''

! Set rewrite flag and tmpfields.
! To keep consistency, all the arithmetic parsing output fields must
! only be combinations of the alias output field variables (i.e., fields(3,:))
! rather than the actual output field variables (i.e., fields(1,:)).  
!-------------------------------------------------------------------

  do m=1,nfield
    tmpfields(1,m)= trim(fields(1,m))
    tmpfields3(m) = trim(fields(3,m))
    tmpexpr= trim(fields(1,m))
    if (scan(trim(tmpexpr),'()^*/+-.')/=0) then
       mask(m)= 0
       rewrite(m)= ESMF_TRUE 
    else
       mask(m)= 1
       rewrite(m)= ESMF_FALSE     
    endif
  enddo

! Set tmpfields:
! tmpfields(1,:) is the copy of fields(1,:) (the actual output field variable) 
! tmpfields3(:)  is the copy of fields(3,:) (the alias output field variable)
! tmpfields(2,:) is the registry expression of the entire output field. 
!-------------------------------------------------------------------

! Set fields in consistent form.
! Set temporarily fields (copy of acutal fields): tmpfields(1,:) 
!-------------------------------------------------------------------
  do m=1,nfield
    tmpexpr= trim(fields(1,m))
    call MAPL_fields(gc,m,tmpexpr,tmpfields(1,:),tmpfields3,mask,rc=status); VERIFY_(status)
    tmpfields(1,m)= trim(tmpexpr)
  enddo

! Set temporarily fields (alias of acutal fields): tmpfields3(:) 
!-------------------------------------------------------------------
  do m=1,nfield
    tmpexpr= trim(fields(1,m))
    if (scan(trim(tmpexpr),'()^*/+-.')/=0) then
       tmpfields3(m)= trim(fields(3,m))
    else
       tmpfields3(m)= trim(fields(1,m))
    endif
  enddo

! Set temporarily fields (registery fields): tmpfields(2,:) 
!-------------------------------------------------------------------
  do m=1,nfield
    expr= trim(tmpfields(1,m))
    call MAPL_ExpressionGetVars(gc,expr,tmpfields3,len_list,rc=status); VERIFY_(status)
    tmpfields(2,m)= trim(expr)
  enddo

! Reset temporarily fields (alias of acutal fields): tmpfields(2,:) 
!-------------------------------------------------------------------
  do m=1,nfield
    tmpexpr= trim(tmpfields(2,m))
    call MAPL_registery(gc,m,tmpexpr,tmpfields(2,:),mask,rc=status); VERIFY_(status)
    tmpfields(2,m)= trim(tmpexpr)
  enddo

! Change the arithmetic parsing field containing mutiple variables
! to the dummy default field containing a single field variable.
! Since MAPL_HistoryGridCompMod does not understand arithmetic parsing field variable,
! we need to change the arithmetic parsing field variable to the dummy field to allocate memory.
! But the actual arithmetic parsing field already has been copied to the temporialy field.
!----------------------------------------------------------------------
 do m=1,nfield
     expr= trim(tmpfields(2,m))
     if (Rewrite(m)==ESMF_TRUE) then
         call MAPL_default(gc,m,expr,tmpexpr,tmpfields(1,:),fields(2,:),tmprank,tmplb,tmpub,rc=status); VERIFY_(status)
         fields(1,m)= trim(expr)
         fields(2,m)= trim(tmpexpr)
     endif
 enddo

! Deallocate local memory 
!-------------------------------------------------------------------
  deallocate(mask)
  deallocate(tmpfields3)

!  if( MAPL_AM_I_ROOT() ) then
!    print *,'!----------------- Finishing MAPL_SetExpression -----------------!'
!  endif
  RETURN_(ESMF_SUCCESS)

end subroutine MAPL_SetExpression
!=================================================================

! !IROUTINE: MAPL_RunExpression

! !DESCRIPTION: MAPL_RunExpression runs arithmetic parsing operations among given field variables 

! !INTERFACE:

subroutine MAPL_RunExpression(gc,GIM,bundle,fields,tmpfields,rewrite,nfield,rc)

! !ARGUMENTS:
 
  type(ESMF_GridComp), intent(inout):: gc
  type (ESMF_State),  intent(in)    :: GIM
  type(ESMF_Bundle), intent(inout)  :: bundle
  character*(*), intent(in):: fields(:,:),tmpfields(:,:)
  type(ESMF_Logical), intent(inout) :: rewrite(:)
  integer, intent(in):: nfield
  integer, optional, intent(out) :: rc

! Local variables:
  character(len=3)               :: indx=''
  character(len=ESMF_MAXSTR)     :: Iam,Comp_Name,expr
  integer:: i,m,nmax,STATUS
  type(ESMF_Field)               :: field,newfield
  type(ESMF_Array)               :: array,newarray
  integer, dimension(ESMF_MAXDIM):: lbounds,ubounds
  integer                        :: rank
  type(Ptrs_Type):: rewrite_field
  type(Ptrs_Type), pointer:: given_field(:)
  real, pointer:: Q1D(:),Q2D(:,:),Q3D(:,:,:)

! Get my name and set-up traceback handle
! ---------------------------------------
    Iam = "MAPL_RunExpression"
    call ESMF_GridCompGet(GC, NAME=COMP_NAME, RC=STATUS )
    VERIFY_(status)
    Iam = trim(COMP_NAME) // Iam
  
! Allocate memory 
!----------------------------------------------------------------------
  nmax= nfield+30
  allocate(given_field(1:nmax),stat=status); VERIFY_(status)
 
! Retrieve given fields 
!----------------------------------------------------------------------
  do m=1,nfield
  if (ReWrite(m)==ESMF_FALSE) then
     call ESMF_StateGetField(GIM,trim(fields(3,m)),field,rc=status); VERIFY_(STATUS)
!     call ESMF_BundleGetFbundle,trim(fields(3,m)),field,rc=STATUS); VERIFY_(STATUS)
     call ESMF_FieldGetArray(field,array,rc=STATUS); VERIFY_(STATUS)
     call ESMF_ArrayGet(array,RANK= rank,lbounds= lbounds,ubounds= ubounds,rc=STATUS); VERIFY_(STATUS)
     given_field(m)%rank = rank
     given_field(m)%lb(:)= lbounds(:)
     given_field(m)%ub(:)= ubounds(:)
     call ptrallocate(gc,given_field(m),given_field(m),rc=status); VERIFY_(status)
     if(rank==1)then
         call ESMF_ArrayGetData(array, Q1D, rc=status);  VERIFY_(STATUS)
         given_field(m)%Q1D= Q1D
     else if(rank==2)then
         call ESMF_ArrayGetData(array, Q2D, rc=status);  VERIFY_(STATUS)
         given_field(m)%Q2D= Q2D
     else if(rank==3)then
         call ESMF_ArrayGetData(array, Q3D, rc=status);  VERIFY_(STATUS)
         given_field(m)%Q3D= Q3D
     endif
  endif
  enddo
  
  if (associated(Q1D))nullify(Q1D)
  if (associated(Q2D))nullify(Q2D)
  if (associated(Q3D))nullify(Q3D)


! Perform arithmetic parsing operations of the output field
! using given output fields as well as derived output fields.
!----------------------------------------------------------------------
  do m=1,nfield
  if (ReWrite(m)==ESMF_TRUE) then
     call ESMF_StateGetField(GIM,trim(fields(3,m)),newfield,rc=status); VERIFY_(STATUS)
!     call ESMF_BundleGetField(bundle,trim(fields(3,m)),newfield,rc=STATUS); VERIFY_(STATUS)
     call ESMF_FieldGetArray(newfield,array=newarray,rc=STATUS);  VERIFY_(STATUS)
     call ESMF_ArrayGet(newarray,RANK= rank,lbounds= lbounds,ubounds= ubounds,rc=STATUS);  VERIFY_(STATUS)
     rewrite_field%rank  = rank
     rewrite_field%lb(:) = lbounds(:)
     rewrite_field%ub(:) = ubounds(:)
     rewrite_field%length= nfield 
     rewrite_field%expr  = trim(tmpfields(2,m))
     rewrite_field%realexpr= trim(tmpfields(1,m))
     indx= ''
     write(indx(1:2),'(i2.2)',iostat=status)m     
!     if( MAPL_AM_I_ROOT() ) then
!        write(*,'(a,a,a,a,a,a,a)')trim(indx),'-','Arithmetic parsing:',trim(tmpfields(1,m)),'==>',trim(tmpfields(2,m))
!     endif
     call ptrallocate(gc,rewrite_field,rewrite_field,rc=status); VERIFY_(status)
     call MAPL_parser(gc,m,given_field,rewrite_field,rc=status); VERIFY_(status)
     if(rewrite_field%rank==1)then
        newarray= ESMF_ArrayCreate(rewrite_field%Q1D,ESMF_DATA_COPY,rc=status);  VERIFY_(STATUS)
     else if(rewrite_field%rank==2)then
        newarray= ESMF_ArrayCreate(rewrite_field%Q2D,ESMF_DATA_COPY,rc=status);  VERIFY_(STATUS)
     else if(rewrite_field%rank==3)then
        newarray= ESMF_ArrayCreate(rewrite_field%Q3D,ESMF_DATA_COPY,rc=status);  VERIFY_(STATUS)
     endif
     call ESMF_FieldSetArray(newfield, array=newarray, rc=status);  VERIFY_(STATUS)
     call ptrdeallocate(gc,rewrite_field,rewrite_field,rc=status); VERIFY_(status)
  endif
  enddo


! Deallocate memory                           
!----------------------------------------------------------------------
  do m=1,nfield
     call ptrdeallocate(gc,given_field(m),given_field(m),rc=status); VERIFY_(status)
  enddo
  if(associated(given_field))deallocate(given_field)

  RETURN_(ESMF_SUCCESS)

end subroutine MAPL_RunExpression
!===========================================================
! !IROUTINE: MAPL_parser

! !DESCRIPTION: MAPL_parser provides arithmetic parsing operations out of given field variables.

! !INTERFACE:

subroutine MAPL_parser(gc,mth,given_field,rewrite_field,rc)

! !ARGUMENTS:

  type(ESMF_GridComp), intent(inout):: gc
  integer, intent(in)           :: mth
  type(Ptrs_Type), intent(inout):: rewrite_field
  type(Ptrs_Type), intent(inout):: given_field(:)
  integer, optional, intent(out):: RC

! Local variables:
  integer :: status,i,j,nbeg,nend,len_expr,last,start,end,nmax,len_list
  character(len=3)           :: indx=''
  character(len=ESMF_MAXSTR) :: expr,newexpr,tmpexpr
  character(len=ESMF_MAXSTR) :: Iam,COMP_NAME

! Get my name and set-up traceback handle
! ---------------------------------------
  Iam = "MAPL_parser"
  call ESMF_GridCompGet(GC, NAME=COMP_NAME, RC=STATUS )
  VERIFY_(status)
  Iam = trim(COMP_NAME) // Iam

! Set local variables 
!----------------------------------
  len_list= rewrite_field%length
  expr    = rewrite_field%expr
  newexpr = rewrite_field%expr

! Set local parameters 
!---------------------------------- 
  len_expr= len_trim(expr)
  last    = len_list

! Process parenthetical expressions
!----------------------------------

PARENS: do

! Find inner parens
!------------------
     NBEG = index(NEWEXPR,'(',BACK=.true.)

!  If none, evaluate final simple expression and exit
!----------------------------------------------------
if(NBEG == 0) then
     expr= newexpr
     len_expr= len_trim(expr)
     exit
end if
!------------------------------------------------
     NEND = index(NEWEXPR(NBEG+1:),')') + NBEG
     EXPR = NEWEXPR(NBEG+1:NEND-1)
     len_expr= len_trim(expr)

! We do not allow empty parens
!-----------------------------
     ASSERT_(len(trim(EXPR)) > 0)

! Calculate Arithmetic Parser for the inside parenthetical expression
!----------------------------------------------------------------
     if (scan(trim(expr),'^*/')/=0) then
        call new_operation(gc,rewrite_field,expr,tmpexpr,last,given_field,rc=status); VERIFY_(status)
     else
       tmpexpr= trim(expr)
     endif
     call calculate(gc,rewrite_field,tmpexpr,last,given_field,rc=status); VERIFY_(status)

     indx=''
     write(indx(1:2),'(i2.2)',iostat=status)last
     expr= trim(adjustl(indx))

! Replace the parenthetical expression and its surrounding parens
! with a register id in the full expression (NewExpr).
!----------------------------------------------------------------
     NewExpr = adjustl(trim(NewExpr(:NBEG-1))//trim(adjustl(EXPR))//adjustl(NewExpr(NEND+1:)))

end do PARENS

! Calculate Arithmetic Parser for the global expression
!----------------------------------------------------------------
 if (scan(trim(newexpr),'^*/')/=0) then
   call new_operation(gc,rewrite_field,newexpr,tmpexpr,last,given_field,rc=status); VERIFY_(status)
 else
   tmpexpr= trim(newexpr)
 endif
 call calculate(gc,rewrite_field,tmpexpr,last,given_field,rc=status); VERIFY_(status)

! Update rewrite_field pointer 
!----------------------------------------------------------------
 call ptrtoptr(gc,rewrite_field,given_field(last),rewrite_field,rc=status); VERIFY_(status)

! Deallocate local memory 
!----------------------------------------------------------------
 start= len_list+1
 end  = last
 do i=start,end
    call ptrdeallocate(gc,rewrite_field,given_field(i),rc=status); VERIFY_(status)
 enddo


 RETURN_(ESMF_SUCCESS)
end subroutine MAPL_parser
!===========================================================
! !IROUTINE: ptrallocate 

! !DESCRIPTION: ptrallocate provides memory allocation for pointer 

! !INTERFACE:

subroutine ptrallocate(gc,array,ptr,rc)

! !ARGUMENTS:

  type(ESMF_GridComp), intent(inout):: gc
  type(Ptrs_Type),intent(in):: array
  type(Ptrs_Type),intent(inout) :: ptr
  integer, optional, intent(out):: RC

! Local variables:
  integer:: status
  character(len=ESMF_MAXSTR):: Iam,COMP_NAME

! Get my name and set-up traceback handle
! ---------------------------------------
    Iam = "ptrallocate"
    call ESMF_GridCompGet(GC, NAME=COMP_NAME, RC=STATUS )
    VERIFY_(status)
    Iam = trim(COMP_NAME) // Iam

! Allocate memory depending on size of rank (1,2, or 3) 
! --------------------------------------------------------
  ptr%rank = array%rank
  ptr%lb(:)= array%lb(:) 
  ptr%ub(:)= array%ub(:)

  select case (array%rank)
    case(0)
      ptr%Q= 0.0
    case(1)
         if (associated(ptr%Q1D))deallocate(ptr%Q1D)
         allocate(ptr%Q1D(array%lb(1):array%ub(1)),stat=status); VERIFY_(status)
         ptr%Q1D(:)= 0.0
    case(2)
         if (associated(ptr%Q2D))deallocate(ptr%Q2D)
         allocate(ptr%Q2D(array%lb(1):array%ub(1), &
                      array%lb(2):array%ub(2)),stat=status); VERIFY_(status)
         ptr%Q2D(:,:)= 0.0
    case(3)
         if (associated(ptr%Q3D))deallocate(ptr%Q3D)
         allocate(ptr%Q3D(array%lb(1):array%ub(1), &
                      array%lb(2):array%ub(2), &
                      array%lb(3):array%ub(3)),stat=status); VERIFY_(status)
         ptr%Q3D(:,:,:)= 0.0
    case default
     ASSERT_(.false.)
  end select

  RETURN_(ESMF_SUCCESS)
end subroutine ptrallocate
!===========================================================
! !IROUTINE: ptrdeallocate

! !DESCRIPTION: ptrdeallocate provides memory deallocation for pointer

! !INTERFACE:

subroutine ptrdeallocate(gc,array,ptr,rc)

! !ARGUMENTS:

  type(ESMF_GridComp), intent(inout):: gc
  type(Ptrs_Type),intent(in):: array
  type(Ptrs_Type),intent(inout) :: ptr
  integer, optional, intent(out):: RC
! Local variables:
  integer:: status
  character(len=ESMF_MAXSTR):: Iam,COMP_NAME

! Get my name and set-up traceback handle
! ---------------------------------------
  Iam = "ptrdeallocate"
  call ESMF_GridCompGet(GC, NAME=COMP_NAME, RC=STATUS )
  VERIFY_(status)
  Iam = trim(COMP_NAME) // Iam

! Deallocate memory depending on size of rank (1,2, or 3)
! --------------------------------------------------------
  select case(array%rank)
    case (0)
    case (1) 
     if (associated(ptr%Q1D))deallocate(ptr%Q1D)
    case (2)
     if (associated(ptr%Q2D))deallocate(ptr%Q2D)
    case (3)
     if (associated(ptr%Q3D))deallocate(ptr%Q3D)
    case default  
     ASSERT_(.false.)
  end select

  RETURN_(ESMF_SUCCESS)
end subroutine ptrdeallocate
!===========================================================
! !IROUTINE: scalartoptr 

! !DESCRIPTION: scalartoptr assigns scalar to pointer.  

! !INTERFACE:

subroutine scalartoptr(gc,array,scalar,ptr_out,rc)

! !ARGUMENTS:

  type(ESMF_GridComp), intent(inout):: gc
  type(Ptrs_Type),intent(in):: array
  Real,intent(in) :: scalar
  type(Ptrs_Type),intent(inout) :: ptr_out
  integer, optional, intent(out):: RC
! Local variables:
  integer:: status
  character(len=ESMF_MAXSTR):: Iam,COMP_NAME

! Get my name and set-up traceback handle
! ---------------------------------------
  Iam = "scalartoptr"
  call ESMF_GridCompGet(GC, NAME=COMP_NAME, RC=STATUS )
  VERIFY_(status)
  Iam = trim(COMP_NAME) // Iam

! Assign scalar to pointer depending on size of rank (1,2, or 3)
! --------------------------------------------------------
  ptr_out%rank = array%rank
  ptr_out%lb(:)= array%lb(:)
  ptr_out%ub(:)= array%ub(:)

  select case (ptr_out%rank)
    case(0)
     ptr_out%Q  = scalar
    case(1)
     ptr_out%Q1D(:)= scalar
    case(2)
     ptr_out%Q2D(:,:)= scalar
    case(3)
     ptr_out%Q3D(:,:,:)= scalar
    case default
     ASSERT_(.false.)
  end select

  RETURN_(ESMF_SUCCESS)
end subroutine scalartoptr
!===========================================================
! !IROUTINE: ptrtoptr

! !DESCRIPTION: ptrtoptr assigns pointer to pointer. 

! !INTERFACE:

subroutine ptrtoptr(gc,array,ptr_in,ptr_out,rc)

! !ARGUMENTS:

  type(ESMF_GridComp), intent(inout):: gc
  type(Ptrs_Type),intent(in):: array
  type(Ptrs_Type),intent(in):: ptr_in
  type(Ptrs_Type),intent(inout) :: ptr_out
  integer, optional, intent(out):: RC
! Local variables:
  integer:: status,dim,i,j,k
  integer:: istart,iend,jstart,jend,kstart,kend
  character(len=ESMF_MAXSTR):: Iam,COMP_NAME 
 
! Get my name and set-up traceback handle
! ---------------------------------------
  Iam = "ptrtoptr"
  call ESMF_GridCompGet(GC, NAME=COMP_NAME, RC=STATUS )
  VERIFY_(status)
  Iam = trim(COMP_NAME) // Iam
 
! Assign local dimension 
! --------------------------------------------------------
  ptr_out%rank = array%rank
  ptr_out%lb(:)= array%lb(:)
  ptr_out%ub(:)= array%ub(:)
  ASSERT_(ptr_in%rank>=0 .and. ptr_in%rank<=3)
  ASSERT_(ptr_out%rank>=0 .and. ptr_out%rank<=3)

  istart= MAX(ptr_in%lb(1),ptr_out%lb(1))
  iend  = MIN(ptr_in%ub(1),ptr_out%ub(1))
  jstart= MAX(ptr_in%lb(2),ptr_out%lb(2))
  jend  = MIN(ptr_in%ub(2),ptr_out%ub(2))
if (ptr_out%rank==3 .and. ptr_in%rank==3) then
  kstart= MAX(ptr_in%lb(3),ptr_out%lb(3))
  kend  = MIN(ptr_in%ub(3),ptr_out%ub(3))
endif

!  if( MAPL_AM_I_ROOT() .and. ptr_in%rank /=array%rank ) then
!     write(*,'(a,i2,a,i2)')' mismatch rank: (out)',ptr_out%rank,'<=(used)',ptr_in%rank
!     if (ptr_in%lb(1)/=array%lb(1) .or. ptr_in%ub(1)/=array%ub(1)) then
!         write(*,'(a,i1,a,i2,a,i1,a,i2,a)')' mismatch 1st dim: (out)(',ptr_out%lb(1),',',ptr_out%ub(1),')<=(used)(', istart,',',iend,')'
!     elseif (ptr_in%lb(2)/=array%lb(2) .or. ptr_in%ub(2)/=array%ub(2)) then
!         write(*,'(a,i1,a,i2,a,i1,a,i2,a)')' mismatch 2nd dim: (out)(',ptr_out%lb(2),',',ptr_out%ub(2),')<=(used)(', jstart,',',jend,')'
!     elseif (ptr_in%lb(3)/=array%lb(3) .or. ptr_in%ub(3)/=array%ub(3)) then
!         write(*,'(a,i1,a,i2,a,i1,a,i2,a)')' mismatch 3rd dim: (out)(',ptr_out%lb(3),',',ptr_out%ub(3),')<=(used)(', kstart,',',kend,')'
!     endif
!  endif

! Assign pointer to pointer (input pointer project to output pointer) for same rank 
! -----------------------------------------------------------------------------------
  if(ptr_in%rank==ptr_out%rank) then
    select case(ptr_out%rank)
      case(0)
        ptr_out%Q= ptr_in%Q
      case(1)
        do i=istart,iend
           ptr_out%Q1D(i)= ptr_in%Q1D(i)
        enddo
      case(2)
        do j=jstart,jend
        do i=istart,iend
           ptr_out%Q2D(i,j)= ptr_in%Q2D(i,j)
        enddo
        enddo       
      case(3)
        do k=kstart,kend
        do j=jstart,jend
        do i=istart,iend
           ptr_out%Q3D(i,j,k)= ptr_in%Q3D(i,j,k)
        enddo
        enddo
        enddo
      case default
        ASSERT_(.false.)
    end select
! Assign pointer to pointer (input pointer project to output pointer) for mismatch rank 
! ---------------------------------------------------------------------------------------
  else if (ptr_in%rank < ptr_out%rank) then
    if (ptr_in%rank==2 .and. ptr_out%rank==3) then
        do j=jstart,jend
        do i=istart,iend
           ptr_out%Q3D(i,j,:)= ptr_in%Q2D(i,j)
        enddo
        enddo
    elseif (ptr_in%rank==1 .and. ptr_out%rank==3) then
        do i=istart,iend
           ptr_out%Q3D(i,:,:)= ptr_in%Q1D(i)
        enddo 
    elseif (ptr_in%rank==1 .and. ptr_out%rank==2) then
        do i=istart,iend
           ptr_out%Q2D(i,:)= ptr_in%Q1D(i)
        enddo
    endif
  else
    ASSERT_(.false.)
  endif


  RETURN_(ESMF_SUCCESS)
end subroutine ptrtoptr
!===========================================================
! !IROUTINE: ptroperation 
 
! !DESCRIPTION: ptroperation calculates arithmetic operation between left and right variables.          

! !INTERFACE:

subroutine ptroperation(gc,array,op,left,right,tmp,rc)

! !ARGUMENTS:

  type(ESMF_GridComp), intent(inout):: gc
  type(Ptrs_Type),intent(in):: array
  character*1,intent(in):: op
  type(Ptrs_Type),intent(in) :: left,right
  type(Ptrs_Type),intent(inout) :: tmp
  integer, optional, intent(out):: RC
! Local variables:
  integer:: status,dim,i,j,k
  integer:: istart,iend,jstart,jend,kstart,kend
  integer:: im,jm,km,in,jn,kn
  real   :: abs_left, abs_right 
  Logical:: nan_left,nan_right,exp_flag=.true.,inf_flag=.false.
  character(len=ESMF_MAXSTR):: Iam,COMP_NAME,realexpr

! Get my name and set-up traceback handle
! ---------------------------------------
  Iam = "ptroperation"
  call ESMF_GridCompGet(GC, NAME=COMP_NAME, RC=STATUS )
  VERIFY_(status)
  Iam = trim(COMP_NAME) // Iam
 
! Assign local dimension 
! ------------------------------------------------------
  realexpr = array%realexpr
  tmp%rank = array%rank
  tmp%lb(:)= array%lb(:)
  tmp%ub(:)= array%ub(:)

  ASSERT_(left%rank==right%rank)
  ASSERT_(tmp%rank ==right%rank .and. tmp%rank ==left%rank)

  istart= MAX(left%lb(1),right%lb(1))
  iend  = MIN(left%ub(1),right%ub(1))
  jstart= MAX(left%lb(2),right%lb(2))
  jend  = MIN(left%ub(2),right%ub(2))
if (left%rank==3 .and. right%rank==3) then
  kstart= MAX(left%lb(3),right%lb(3))
  kend  = MIN(left%ub(3),right%ub(3))
endif

! Calculate arithmetic operation between left and right variables depending on size of rank (1,2, or 3)
! ------------------------------------------------------------------------------------------------------
  select case(tmp%rank)
   case(0)
     abs_left= ABS(left%Q);    abs_right= ABS(right%Q)
     nan_left= ISNAN(left%Q);  nan_right= ISNAN(right%Q)
     if (op=='^' .and. left%Q<0.0 .and. right%Q<1.0)then
         exp_flag=.false.; go to 99
     endif
     if (op=='^' .and. abs_left>tiny .and. abs_left<large .and. &
          nan_left==.false. .and. nan_right==.false.) then
         tmp%Q=left%Q ** right%Q
     elseif (op=='*' .and. abs_left<large .and. abs_right<large .and. &
             nan_left==.false. .and. nan_right==.false.) then
         tmp%Q=left%Q * right%Q
     elseif (op=='/' .and. abs_right>tiny .and. abs_left<large .and. &
             nan_left==.false. .and. nan_right==.false.) then
         tmp%Q=left%Q / right%Q
     endif
   case(1)
    if (associated(tmp%Q1D) .and. associated(left%Q1D) .and. associated(right%Q1D)) then
    do i=istart,iend
     abs_left= ABS(left%Q1D(i));   abs_right= ABS(right%Q1D(i))
     nan_left= ISNAN(left%Q1D(i)); nan_right= ISNAN(right%Q1D(i))
     if (op=='^' .and. left%Q1D(i)<0.0 .and. right%Q1D(i)<1.0)then
         exp_flag=.false.; go to 99
     endif
     if (op=='^' .and. abs_left>tiny .and. abs_left<large .and. &
          nan_left==.false. .and. nan_right==.false.) then
         tmp%Q1D(i)=left%Q1D(i) ** right%Q1D(i)
     elseif (op=='*' .and. abs_left<large .and. abs_right<large .and. &
             nan_left==.false. .and. nan_right==.false.) then
         tmp%Q1D(i)=left%Q1D(i) * right%Q1D(i)
     elseif (op=='/' .and. abs_right>tiny .and. abs_left<large .and. &
             nan_left==.false. .and. nan_right==.false.) then
         tmp%Q1D(i)=left%Q1D(i) / right%Q1D(i)
     endif
    enddo
    else
     ASSERT_(.false.)
    endif
   case(2)
    if (associated(tmp%Q2D) .and. associated(left%Q2D) .and. associated(right%Q2D)) then
    do j=jstart,jend
    do i=istart,iend
     abs_left= ABS(left%Q2D(i,j));   abs_right= ABS(right%Q2D(i,j))
     nan_left= ISNAN(left%Q2D(i,j)); nan_right= ISNAN(right%Q2D(i,j))
     if (op=='^' .and. left%Q2D(i,j)<0.0 .and. right%Q2D(i,j)<1.0)then
         exp_flag=.false.; go to 99
     endif
     if (op=='^' .and. abs_left>tiny .and. abs_left<large .and. &
          nan_left==.false. .and. nan_right==.false.) then
         tmp%Q2D(i,j)=left%Q2D(i,j) ** right%Q2D(i,j)
     elseif (op=='*' .and. abs_left<large .and. abs_right<large .and. &
             nan_left==.false. .and. nan_right==.false.) then
         tmp%Q2D(i,j)=left%Q2D(i,j) * right%Q2D(i,j)
     elseif (op=='/' .and. abs_right>tiny .and. abs_left<large .and. &
             nan_left==.false. .and. nan_right==.false.) then
         tmp%Q2D(i,j)=left%Q2D(i,j) / right%Q2D(i,j)
     endif
    enddo
    enddo
    else
     ASSERT_(.false.)
    endif
   case(3)
    if (associated(tmp%Q3D) .and. associated(left%Q3D) .and. associated(right%Q3D)) then
    do k=kstart,kend
    do j=jstart,jend
    do i=istart,iend
     abs_left= ABS(left%Q3D(i,j,k));   abs_right= ABS(right%Q3D(i,j,k))
     nan_left= ISNAN(left%Q3D(i,j,k)); nan_right= ISNAN(right%Q3D(i,j,k))
     if (op=='^' .and. left%Q3D(i,j,k)<0.0 .and. right%Q3D(i,j,k)<1.0)then
         exp_flag=.false.; go to 99
     endif
     if (op=='^' .and. abs_left>tiny .and. abs_left<large .and. &
          nan_left==.false. .and. nan_right==.false.) then
         tmp%Q3D(i,j,k)=left%Q3D(i,j,k) ** right%Q3D(i,j,k)
     elseif (op=='*' .and. abs_left<large .and. abs_right<large .and. &
             nan_left==.false. .and. nan_right==.false.) then
         tmp%Q3D(i,j,k)=left%Q3D(i,j,k) * right%Q3D(i,j,k)
     elseif (op=='/' .and. abs_right>tiny .and. abs_left<large .and. &
             nan_left==.false. .and. nan_right==.false.) then
         tmp%Q3D(i,j,k)=left%Q3D(i,j,k) / right%Q3D(i,j,k)
     endif
    enddo
    enddo
    enddo
    else
     ASSERT_(.false.)
    endif
   case default
     ASSERT_(.false.)
  end select
                                                                  
99 &
if(exp_flag==.false.) then
   if (MAPL_AM_I_ROOT()) then
      print *,'===> Exponentiation Error occurred! Stop here at ',trim(realexpr); ASSERT_(.false.)
   else
      rc= ESMF_FAILURE
   endif
endif

  RETURN_(ESMF_SUCCESS)
end subroutine ptroperation
!===========================================================
! !IROUTINE: ptrsum

! !DESCRIPTION: ptrsum provides summation of variables.

! !INTERFACE:

subroutine ptrsum(gc,array,sign,tmp,sum,rc)

! !ARGUMENTS:

  type(ESMF_GridComp), intent(inout):: gc
  type(Ptrs_Type),intent(in):: array
  real,intent(in):: sign
  type(Ptrs_Type),intent(in) :: tmp
  type(Ptrs_Type),intent(inout) :: sum
  integer, optional, intent(out):: RC
! Local variables:
  integer:: status,dim,i
  character(len=ESMF_MAXSTR):: Iam,COMP_NAME

! Get my name and set-up traceback handle
! ---------------------------------------
  Iam = "ptrsum"
  call ESMF_GridCompGet(GC, NAME=COMP_NAME, RC=STATUS )
  VERIFY_(status)
  Iam = trim(COMP_NAME) // Iam

! Perform summation of variables depending on size of rank (1,2, or 3)
! ----------------------------------------------------------------------
  ASSERT_(array%rank==tmp%rank)
  ASSERT_(array%rank==sum%rank)

  dim= array%rank
  do i=1,dim
     ASSERT_(array%lb(i)==tmp%lb(i) .and. array%ub(i)==tmp%ub(i))
     ASSERT_(array%lb(i)==sum%lb(i) .and. array%ub(i)==sum%ub(i))
  enddo

  select case(array%rank)
   case (0)
     sum%Q=sum%Q + sign*tmp%Q
   case (1)
    if (associated(sum%Q1D) .and. associated(tmp%Q1D)) then
     sum%Q1D=sum%Q1D + sign*tmp%Q1D
    else
     ASSERT_(.false.)
    endif
   case (2)
    if (associated(sum%Q2D) .and. associated(tmp%Q2D)) then
     sum%Q2D=sum%Q2D + sign*tmp%Q2D
    else
     ASSERT_(.false.)
    endif
   case (3)
    if (associated(sum%Q3D) .and. associated(tmp%Q3D)) then
     sum%Q3D=sum%Q3D + sign*tmp%Q3D
    else
     ASSERT_(.false.)
    endif
   case default
     ASSERT_(.false.)
  end select

  RETURN_(ESMF_SUCCESS)
end subroutine ptrsum
!===========================================================
! !IROUTINE: new_operation 

! !DESCRIPTION: new_operation updates new expression from given expression of arithmetic parsing field. 

! !INTERFACE:

subroutine new_operation(gc,array,expr,newexpr,last,ptr,rc)

! !ARGUMENTS:

  type(ESMF_GridComp), intent(inout):: gc
  type(Ptrs_Type),intent(in):: array
  character*(*),intent(in)::expr
  character*(*),intent(inout)::newexpr
  integer,intent(inout)::last
  type(Ptrs_Type),intent(inout):: ptr(:)
  integer, optional, intent(OUT) :: RC
! Local variables:
  integer:: status,i,j,varnum,start,num,len_expr,nleft,nright
  real:: scalar
  character*1:: op(3)=(/'^','*','/'/),optmp=''
  character(len=ESMF_MAXSTR):: argl,argr,tmpexpr
  character(len=ESMF_MAXSTR):: Iam,COMP_NAME
  type(Ptrs_Type):: left,right,tmp


! Get my name and set-up traceback handle
! ---------------------------------------
  Iam = "new_operation"
  call ESMF_GridCompGet(GC, NAME=COMP_NAME, RC=STATUS )
  VERIFY_(status)
  Iam = trim(COMP_NAME) // Iam

! Get value for given expression and update new expression
! ---------------------------------------------------------
  start   = last+1
  len_expr= len_trim(expr)
  newexpr = trim(expr)
  num=0

! Operation Loop 
! ------------------------------------------------
do j=1,3

  do i=1,20
     tmpexpr= trim(newexpr)
     optmp= trim(op(j))
     num= index(tmpexpr,optmp(1:1))
     if (num==0) exit
     call GetDyadArgs(gc,tmpexpr,num,argl,argr,last,rc=status); VERIFY_(status)
     argl= trim(argl)
     argr= trim(argr)

     if (scan(argl,'.')/=0) then
         read(argl,*)scalar
         call ptrallocate(gc,array,left,rc=status); VERIFY_(status)
         call scalartoptr(gc,array,scalar,left,rc=status); VERIFY_(status)
     elseif (scan(argl,'0123456789')/=0) then
         read(argl,*)varnum
         call ptrallocate(gc,array,left,rc=status); VERIFY_(status)
         call ptrtoptr(gc,array,ptr(varnum),left,rc=status); VERIFY_(status)
     else
         ASSERT_(.false.)
     endif
     if (scan(argr,'.')/=0) then
         read(argr,*)scalar
         call ptrallocate(gc,array,right,rc=status); VERIFY_(status)
         call scalartoptr(gc,array,scalar,right,rc=status); VERIFY_(status)
     elseif (scan(argr,'0123456789')/=0) then
         read(argr,*)varnum
         call ptrallocate(gc,array,right,rc=status); VERIFY_(status)
         call ptrtoptr(gc,array,ptr(varnum),right,rc=status); VERIFY_(status)
     else
         ASSERT_(.false.)
     endif
     call ptrallocate(gc,array,tmp,rc=status); VERIFY_(status)
     call ptroperation(gc,array,optmp(1:1),left,right,tmp,rc=status); VERIFY_(status)
     if (status==ESMF_FAILURE)exit
     call ptrallocate(gc,array,ptr(last),rc=status); VERIFY_(status)
     call ptrtoptr(gc,array,tmp,ptr(last),rc=status); VERIFY_(status)

    ! update new expression
    !--------------------------------------------------
     call setnewexpr(gc,tmpexpr,optmp(1:1),last,rc=status); VERIFY_(status)
     newexpr= trim(tmpexpr)

     ASSERT_(i<=10) 
  enddo

enddo


! Deallocate local memory
! -----------------------------------------------------
  call ptrdeallocate(gc,array,left,rc=status); VERIFY_(status)
  call ptrdeallocate(gc,array,right,rc=status); VERIFY_(status)
  call ptrdeallocate(gc,array,tmp,rc=status); VERIFY_(status)


  RETURN_(ESMF_SUCCESS)
end subroutine new_operation
!===========================================================
! !IROUTINE: calculate 

! !DESCRIPTION: calculate computes arithmetic parsing operations of the expresion.     

! !INTERFACE:

subroutine calculate(gc,array,expr,last,ptr,rc)

! !ARGUMENTS:

  type(ESMF_GridComp), intent(inout):: gc
  type(Ptrs_Type),intent(in):: array
  character*(*),intent(in):: expr
  integer,   intent(inout):: last
  type(Ptrs_Type),intent(inout):: ptr(:)
  integer, optional, intent(OUT) :: RC
! Local variables:
  integer :: i,status,pos1,pos2,pos,nbeg,nend,len,varnum
  real:: scalar,sign
  character*1:: op
  character(len=ESMF_MAXSTR):: tmpexpr,argl,argr,indx,accumexpr
  character(len=ESMF_MAXSTR):: Iam,COMP_NAME
  type(Ptrs_Type):: tmp,sum

! Get my name and set-up traceback handle
! ---------------------------------------
  Iam = "calculate"
  call ESMF_GridCompGet(GC, NAME=COMP_NAME, RC=STATUS )
  VERIFY_(status)
  Iam = trim(COMP_NAME) // Iam

! Allocate local memory
!--------------------------------------------------
  call ptrallocate(gc,array,sum,rc=status); VERIFY_(status)
  call ptrallocate(gc,array,tmp,rc=status); VERIFY_(status)

!--------------------------------------------------
  last= last+1
  tmpexpr= expr
  accumexpr=''
  op=''
  sign= 1.0

! Calculate arithmetic parsing operations of the expression
!------------------------------------------------------------
do i=1,20

  pos1= index(tmpexpr,'+')
  pos2= index(tmpexpr,'-')

  if (pos1==0 .and. pos2==0) then
     argr= trim(tmpexpr)
     exit
  elseif (pos1>1 .and. pos2==0) then
     pos= pos1
  elseif (pos2>1 .and. pos1==0) then
     pos= pos2
  elseif (pos1>1 .and. pos2>1) then
     pos= min(pos1,pos2)
  elseif (pos1==1 .or. pos2==1) then
     pos= 1
  endif

  argl = adjustl(tmpexpr(:pos-1))
  argr = adjustl(tmpexpr(pos+1:))

  nbeg = scan(argl,'+-',BACK=.true.) + 1
  nend = scan(argr,'+-')
  argl = argl(nbeg:)

  if (trim(argl)=='') then
      scalar= 0.0
      call scalartoptr(gc,array,scalar,tmp,rc=status); VERIFY_(status)
  elseif (scan(argl,'.')/=0) then
      read(argl,*)scalar
      call scalartoptr(gc,array,scalar,tmp,rc=status); VERIFY_(status)
  elseif (scan(argl,'0123456789')/=0) then
      read(argl,*)varnum
      call ptrtoptr(gc,array,ptr(varnum),tmp,rc=status); VERIFY_(status)
  else
      ASSERT_(.false.)
  endif
  call ptrsum(gc,array,sign,tmp,sum,rc=status); VERIFY_(status)

  accumexpr= trim(accumexpr)//trim(op)//trim(argl)

  if(tmpexpr(pos:pos)=='+')then
     sign= 1.0; op='+'
  else if(tmpexpr(pos:pos)=='-')then
     sign=-1.0; op='-'
  else
     ASSERT_(.false.)
  endif
  tmpexpr= trim(argr)
enddo

  if (scan(argr,'.')/=0) then
      read(argr,*)scalar
      call scalartoptr(gc,array,scalar,tmp,rc=status); VERIFY_(status)
  elseif (scan(argr,'0123456789')/=0) then
      read(argr,*)varnum
      call ptrtoptr(gc,array,ptr(varnum),tmp,rc=status); VERIFY_(status)
  else
      ASSERT_(.false.)
  endif
  call ptrsum(gc,array,sign,tmp,sum,rc=status); VERIFY_(status)

  accumexpr= trim(accumexpr)//trim(op)//trim(argr)

  call ptrallocate(gc,array,ptr(last),rc=status); VERIFY_(status)
  call ptrtoptr(gc,array,sum,ptr(last),rc=status); VERIFY_(status)

! Deallocate local memory
!----------------------------------------------
  call ptrdeallocate(gc,array,sum,rc=status); VERIFY_(status)
  call ptrdeallocate(gc,array,tmp,rc=status); VERIFY_(status)

  RETURN_(ESMF_SUCCESS)
end subroutine calculate
!===========================================================
! !IROUTINE: setexpression 

! !DESCRIPTION: setexpression identifies any registered variable from the arithmetic parsing expression. 

! !INTERFACE:

subroutine setexpression(gc,expr,vars,newexpr,mask,flag,rc)

! !ARGUMENTS:

  type(ESMF_GridComp), intent(inout):: gc
  character*(*),intent(inout)  :: expr
  character*(*),     intent(IN):: Vars(:)
  character*(*),intent(out)    :: newexpr
  integer,intent(in)           :: mask(:)
  integer,intent(inout)        :: flag
  integer, optional, intent(OUT):: RC
! Local variables:
  integer :: status,pos,nbeg,nend,len,varnum
  character(len=ESMF_MAXSTR):: argl,argr,tmp
  character(len=ESMF_MAXSTR):: Iam,COMP_NAME

! Get my name and set-up traceback handle
! ---------------------------------------

  Iam = "setexpression"
  call ESMF_GridCompGet(GC, NAME=COMP_NAME, RC=STATUS )
  VERIFY_(status)
  Iam = trim(COMP_NAME) // Iam

! Identify any registered variable from the arithmetic parsing expression
!---------------------------------------------------------------------------
  tmp    = expr
  newexpr= tmp

do
  pos = scan(tmp,'*^/+-()')
  argl= adjustl(tmp(:pos-1))
  argr= adjustl(tmp(pos+1:))

  nbeg = scan(argl,'*^/+-()',BACK=.true.)+1
  nend = scan(argr,'*^/+-()')

  argl= argl(nbeg:)

  if (pos==0) then
    newexpr= trim(tmp)
    exit
  elseif (pos>2) then
    if (scan(trim(argl),'.')==0) then
       newexpr= trim(argl)
       exit
    endif
  endif
  tmp= trim(argr)
enddo

  read(newexpr,*)varnum
  tmp= vars(varnum)
  newexpr= tmp
  flag= mask(varnum)

do
  pos = scan(tmp,'*^/+-()')
  argl= adjustl(tmp(:pos-1))
  argr= adjustl(tmp(pos+1:))

  nbeg = scan(argl,'*^/+-()',BACK=.true.)+1
  nend = scan(argr,'*^/+-()')

  argl= argl(nbeg:)

  if (pos==0) then
    newexpr= trim(tmp)
    exit
  elseif (pos>1) then
    if (scan(trim(argl),'0123456789')==0) then
       newexpr= trim(argl)
       exit
    endif
  endif
  tmp= trim(argr)
enddo

  RETURN_(ESMF_SUCCESS)
end subroutine setexpression
!===========================================================
! !IROUTINE: setnewexpr

! !DESCRIPTION: setnewexpr sets new registory expression from the given registory expression.

! !INTERFACE:

subroutine setnewexpr(gc,expr,op,loc,rc)

! !ARGUMENTS:

  type(ESMF_GridComp), intent(inout):: gc
  character*(*),intent(inout) :: expr
  character*1,intent(in):: op
  integer, intent(in):: loc
  integer, optional, intent(OUT) :: RC
! Local variables:
  integer :: status,pos,nbeg,nend,len
  character(len=3)::indx=''
  character(len=ESMF_MAXSTR):: argl,argr,tmp
  character(len=ESMF_MAXSTR):: Iam,COMP_NAME
 
! Get my name and set-up traceback handle
! ---------------------------------------
  Iam = "setnewexpr"
  call ESMF_GridCompGet(GC, NAME=COMP_NAME, RC=STATUS )
  VERIFY_(status)
  Iam = trim(COMP_NAME) // Iam

!-------------------------------------------------------------

  pos= index(expr, op)
  ARGL = adjustl(EXPR(:POS-1))
  ARGR = adjustl(EXPR(POS+1:))

  NBEG = scan(ARGL,'*^/+-',BACK=.true.) + 1
  NEND = scan(ARGR,'*^/+-')

  ARGL = ARGL(nbeg:)

  if(NEND /= 0) then
     NEND = NEND - 1
     ARGR = adjustl(ARGR(:NEND))
  else
     NEND = len(trim(ARGR))
  end if

! Set new registory expression from the given registory expression
!---------------------------------------------------------------------
  indx=''
  write(indx(1:2),'(i2.2)',iostat=status)loc
  tmp= trim(adjustl(indx))
  EXPR = EXPR(:NBEG-1)//trim(adjustl(tmp))//EXPR(NEND+POS+1:)

  RETURN_(ESMF_SUCCESS)
end subroutine setnewexpr
!===========================================================
! !IROUTINE: GetDyadArgs 

! !DESCRIPTION: GetDyadArgs sets new registory expression from the given registory expression.

! !INTERFACE:

subroutine GetDyadArgs(gc,EXPR,POS,ARGL,ARGR,cnt,rc)

! !ARGUMENTS:

  type(ESMF_GridComp), intent(inout):: gc
  character*(*),    intent(IN) :: EXPR
  integer,          intent(INOUT) :: POS
  character*(*),    intent(OUT) :: ARGL
  character*(*),    intent(OUT) :: ARGR
  integer, intent(inout):: cnt
  integer, optional, intent(OUT) :: RC
! Local variables:
  integer :: status,nbeg, nend, len
  character(len=ESMF_MAXSTR):: Iam,COMP_NAME

! Get my name and set-up traceback handle
! ---------------------------------------
  Iam = "GetDyadArgs"
  call ESMF_GridCompGet(GC, NAME=COMP_NAME, RC=STATUS )
  VERIFY_(status)
  Iam = trim(COMP_NAME) // Iam

! Get dynamic arguments between operations
!-----------------------------------------
 
  ARGL = adjustl(EXPR(:POS-1))
  ARGR = adjustl(EXPR(POS+1:))

  NBEG = scan(ARGL,'+-*^/',BACK=.true.) + 1
  NEND = scan(ARGR,'+-*^/')

  ARGL = ARGL(nbeg:)

  if(NEND /= 0) then
     NEND = NEND - 1
     ARGR = adjustl(ARGR(:NEND))
  else
     NEND = len(trim(ARGR))
  end if

!-------------------------------------------------------------
  cnt= cnt+1

  RETURN_(ESMF_SUCCESS)
end subroutine GetDyadArgs
!===========================================================
! !IROUTINE: MAPL_fields 

! !DESCRIPTION: MAPL_fields sets the consistent expression for the output field.

! !INTERFACE:

subroutine MAPL_fields(gc,mth,Expr,vv,vv3,mask,RC)

! !ARGUMENTS:

  type(ESMF_GridComp), intent(inout):: gc
  character*(*),     intent(INOUT) :: Expr
  character*(*),     intent(IN)  :: vv(:),vv3(:)
  integer,           intent(IN)  :: mth,mask(:)
  integer, optional, intent(OUT) :: RC
  integer       :: status,i,iter,nlength,pos,nbeg,nend,ntmp

! Local variables: 
  character*1:: op=''
  character*3:: indx=''
  character(len=ESMF_MAXSTR):: tmp,tmpexpr,accumexpr,argl,argr,replace
  character(len=ESMF_MAXSTR):: Iam,COMP_NAME

! Get my name and set-up traceback handle
! ---------------------------------------
  Iam = "MAPL_fields"
  call ESMF_GridCompGet(GC, NAME=COMP_NAME, RC=STATUS )
  VERIFY_(status)
  Iam = trim(COMP_NAME) // Iam

! Replace inconsistent output field expression to consistent expression
! -------------------------------------------------------------------------
  nlength= size(vv)
  tmp= trim(expr)

  accumexpr=''
  do iter=1,100
    replace=''
    pos = scan(tmp,'^*/+-()')
    if (pos==0)exit

    argl= adjustl(tmp(:pos-1))
    argr= adjustl(tmp(pos+1:))
    op  = adjustl(tmp(pos:pos))

    nbeg= scan(argl,'*^/+-()',BACK=.true.)+1
    nend= scan(argr,'*^/+-()')

    argl= argl(nbeg:)

    do i=1,nlength
    if (mask(i)==1) then
       tmpexpr= trim(vv3(i))
       ntmp= len_trim(tmpexpr)
       if (argl(:pos-1)==tmpexpr(:ntmp))then
            replace= trim(vv(i))
       endif
    endif
    enddo

    if (trim(replace)=='')replace=trim(argl)

    accumexpr= trim(accumexpr)//trim(replace)//trim(op)
    tmp= trim(argr)
  enddo

  if (pos==0)then
    ntmp= len_trim(tmp)
    argl= tmp(:ntmp)
    do i=1,nlength
    if (mask(i)==1) then
        tmpexpr= trim(vv3(i))
        if (trim(argl)==trim(tmpexpr))then
            replace= trim(vv(i))
        endif
    endif
    enddo
    if (trim(replace)=='')replace=trim(argl)
    accumexpr= trim(accumexpr)//trim(replace)
  endif

  expr= trim(accumexpr)

!if (mth==nlength .and. MAPL_AM_I_ROOT()) then
!  print *,'!----------------- Finishing MAPL_fields -----------------!'
!endif

  RETURN_(ESMF_SUCCESS)
end subroutine MAPL_fields
!===========================================================
! !IROUTINE: MAPL_registery 

! !DESCRIPTION: MAPL_registery sets the output field into simplied registery.

! !INTERFACE:

subroutine MAPL_registery(gc,mth,Expr,vv2,mask,RC)

! !ARGUMENTS:

  type(ESMF_GridComp), intent(inout):: gc
  character*(*),     intent(INOUT) :: Expr
  character*(*),     intent(IN)  :: vv2(:)
  integer,           intent(IN)  :: mth,mask(:)
  integer, optional, intent(OUT) :: RC
  integer:: status,i,iter,nlength,pos,nbeg,nend,ntmp,varnum

! Local variables: 
  character*1:: op=''
  character*3:: indx=''
  character(len=ESMF_MAXSTR):: tmp,accumexpr,argl,argr,replace
  character(len=ESMF_MAXSTR):: Iam,COMP_NAME

! Get my name and set-up traceback handle
! ---------------------------------------
  Iam = "MAPL_registery"
  call ESMF_GridCompGet(GC, NAME=COMP_NAME, RC=STATUS )
  VERIFY_(status)
  Iam = trim(COMP_NAME) // Iam

! Replace initial registery expression to simplied expression
! ------------------------------------------------------------
  nlength= size(vv2)
  tmp= trim(expr)

  accumexpr=''
  do iter=1,100
    replace=''
    pos = scan(tmp,'^*/+-()')
    if (pos==0)exit

    argl= adjustl(tmp(:pos-1))
    argr= adjustl(tmp(pos+1:))
    op  = adjustl(tmp(pos:pos))

    nbeg= scan(argl,'*^/+-()',BACK=.true.)+1
    nend= scan(argr,'*^/+-()')

    argl= argl(nbeg:)

    if (scan(trim(argl),'.')/=0) then
       replace= trim(argl)
    elseif (scan(trim(argl),'0123456789')/=0) then
       read(argl,*)varnum
       tmp= vv2(varnum)
       ntmp= scan(tmp,'^*/+-')
       if (ntmp==0) then
          replace= trim(tmp)
       else
          replace= '('//trim(tmp)//')'
       endif
    endif

    accumexpr= trim(accumexpr)//trim(replace)//trim(op)
    tmp= trim(argr)
  enddo

  if (pos==0)then
    ntmp= len_trim(tmp)
    argl= tmp(:ntmp)
    if (scan(trim(argl),'.')/=0) then
       replace= trim(argl)
    elseif (scan(trim(argl),'0123456789')/=0) then
       read(argl,*)varnum
       tmp= vv2(varnum)
       ntmp= scan(tmp,'^*/+-')
       if (ntmp==0) then
          replace= trim(tmp)
       else
          replace= '('//trim(tmp)//')'
       endif
    endif
    accumexpr= trim(accumexpr)//trim(replace)
  endif

  expr= trim(accumexpr)

!if (mth==nlength .and. MAPL_AM_I_ROOT()) then
!  print *,'!----------------- Finishing MAPL_registery -----------------!'
!endif

  RETURN_(ESMF_SUCCESS)
end subroutine MAPL_registery
!===========================================================
! !IROUTINE: MAPL_default

! !DESCRIPTION: MAPL_default creates default field among arithmetic parsing fields. 

! !INTERFACE:

subroutine MAPL_default(gc,mth,expr,tmpexpr,vv1,vv2,tmprank,tmplb,tmpub,RC)

! !ARGUMENTS:

  type(ESMF_GridComp), intent(inout):: gc
  integer, intent(in)               :: mth
  character*(*),     intent(INOUT)  :: expr,tmpexpr
  character*(*),     intent(IN)     :: vv1(:),vv2(:)
  integer, intent(inout)            :: tmprank(:),tmplb(:,:),tmpub(:,:)
  integer, optional, intent(OUT)    :: RC

! Local variables:
  integer:: status,i,j,k,m,n,iter,nlength,pos,nbeg,nend,count,varnum
  character(len=ESMF_MAXSTR):: tmp,argl,argr
  character(len=ESMF_MAXSTR):: Iam,COMP_NAME
  integer:: vars(20),rank,indx,itmp,jtmp(3),ktmp(3),indx_final

! Get my name and set-up traceback handle
! ---------------------------------------
  Iam = "MAPL_default"
  call ESMF_GridCompGet(GC, NAME=COMP_NAME, RC=STATUS )
  VERIFY_(status)
  Iam = trim(COMP_NAME) // Iam

! Replace initial registery expression to simplied expression
! ------------------------------------------------------------
  tmp= trim(expr)
  count=0
  do iter=1,10
    pos= scan(tmp,'^*/+-()')
    argl= adjustl(tmp(:pos-1))
    argr= adjustl(tmp(pos+1:))
 
    nbeg= scan(argl,'*^/+-()',BACK=.true.)+1
    nend= scan(argr,'*^/+-()')

    argl= argl(nbeg:)
    
    if (scan(trim(argl),'0123456789')/=0 .and. scan(trim(argl),'.')==0) then
       read(argl,*)varnum
       count= count+1
       vars(count)= varnum 
    endif
    tmp= trim(argr)
    if (pos==0)then
      if (scan(trim(argr),'0123456789')/=0 .and. scan(trim(argr),'.')==0) then
         read(argr,*)varnum
         count= count+1
         vars(count)= varnum
      endif
      exit
    endif
  enddo

  itmp= tmprank(1) 
  do m=1,count
     indx= vars(m)
     itmp= MAX(itmp,tmprank(indx))
  enddo

  do rank=1,itmp
     jtmp(rank)= tmplb(1,rank)
     do n=1,count
        indx= vars(n)
        jtmp(rank)= MIN(jtmp(rank),tmplb(indx,rank))
     enddo
  enddo

  do rank=1,itmp
     ktmp(rank)= tmpub(1,rank)
     do n=1,count
        indx= vars(n)
        ktmp(rank)= MAX(ktmp(rank),tmpub(indx,rank))
     enddo
  enddo

  indx_final= vars(1)
  do m=1,count
     indx= vars(m)
     select case (tmprank(indx))
       case(1)
         if (itmp==tmprank(indx) .and. jtmp(1)==tmplb(indx,1) .and. ktmp(1)==tmpub(indx,1)) then
            indx_final= indx
         endif 
       case(2)
         if (itmp==tmprank(indx) .and. jtmp(1)==tmplb(indx,1) .and. ktmp(1)==tmpub(indx,1) &
             .and. jtmp(2)==tmplb(indx,2) .and. ktmp(2)==tmpub(indx,2) ) then
            indx_final= indx
         endif
       case(3)
         if (itmp==tmprank(indx) .and. jtmp(1)==tmplb(indx,1) .and. ktmp(1)==tmpub(indx,1) &
             .and. jtmp(2)==tmplb(indx,2) .and. ktmp(2)==tmpub(indx,2) &
             .and. jtmp(3)==tmplb(indx,3) .and. ktmp(3)==tmpub(indx,3) ) then
            indx_final= indx 
         endif 
       case default
         ASSERT_(.false.)
     end select
  enddo

! Assign new default fields(1,:) and fields(2,:) for arithmetic parsing expression
!-----------------------------------------------------------------------------------
  expr   = trim(vv1(indx_final))
  tmpexpr= trim(vv2(indx_final))

  tmprank(mth)= tmprank(indx)
  tmplb(mth,:)= tmplb(indx,:)
  tmpub(mth,:)= tmpub(indx,:)

!  if( MAPL_AM_I_ROOT() ) then
!     print *,'rank:',itmp
!     print *,'lb:',(jtmp(i),i=1,itmp)
!     print *,'ub:',(ktmp(j),j=1,itmp)
!     print *,'comp:',(vars(m),m=1,count)
!     print *,'indx:',indx_final
!     print *,trim(expr),',',trim(tmpexpr)
!  endif
  
  RETURN_(ESMF_SUCCESS)
end subroutine MAPL_default
!===========================================================
! !IROUTINE: MAPL_ExpressionGetVars 

! !DESCRIPTION: MAPL_ExpressionGetVars sets the registory expression out of the actual expression.

! !INTERFACE:

subroutine MAPL_ExpressionGetVars(gc,Expr,Vars,last,RC)

! !ARGUMENTS:

  type(ESMF_GridComp), intent(inout):: gc
  character*(*),     intent(INOUT) :: Expr
  character*(*),     intent(IN)  :: Vars(:)
  integer,           intent(OUT) :: Last
  integer, optional, intent(OUT) :: RC
  integer       :: nb, nl, nv, status, PosOfSymbol
  integer       :: Left, Right, iter
  character(len=ESMF_MAXSTR):: vv(size(vars))
  character(len=ESMF_MAXSTR):: Iam,COMP_NAME

! Get my name and set-up traceback handle
! ---------------------------------------
  Iam = "MAPL_ExpressionGetVars"
  call ESMF_GridCompGet(GC, NAME=COMP_NAME, RC=STATUS )
  VERIFY_(status)
  Iam = trim(COMP_NAME) // Iam

! Remove blanks and check parens
! ---------------------------------------
  vv(:) = vars(:)
  nb    = 1
  Left  = 0
  Right = 0

  do while(len_trim(expr(nb:))>0)
     if(expr(nb:nb)=='') then
        expr(nb:) = adjustl(expr(nb:))
     end if
     if    (expr(nb:nb)=='(') then
        Left  = Left  + 1
     elseif(expr(nb:nb)==')') then
        Right = Right + 1
     end if
     nb = nb + 1
  enddo

  ASSERT_(Left==Right)

! Number of variables initially in list
! ---------------------------------------
  nv = count(vv /= '')

! Loop over expr string looking for variables and replacing them
! with their 2-digit index into the vars array. If the are not
! present in the vars array, they are added to it and nv is bumped.
!------------------------------------------------------------------
  nb= 1

  do iter=1,100
     PosOfSymbol =  scan(expr(nb:),'+-*/^()')

     if(PosOfSymbol==1) then
        nb = nb + 1
     else
        if(PosOfSymbol==0) then
           nl = len_trim(expr)
      else
           nl = nb + PosOfSymbol - 2
        end if

        call ReplaceToken(gc,expr,nb,nl,nv,vv,rc=status); VERIFY_(status)
        if (status==ESMF_FAILURE)exit
     endif

     if(nb>len_trim(expr)) exit
  end do

  Last = NV

  RETURN_(ESMF_SUCCESS)


end subroutine MAPL_ExpressionGetVars
!===========================================================
! !IROUTINE: ReplaceToken 

! !DESCRIPTION: ReplaceToken replaces the actual variable by the 2-digit registory variable.

! !INTERFACE:

subroutine ReplaceToken(gc,expr,nb,nl,nv,vv,rc)

! !ARGUMENTS:

  type(ESMF_GridComp), intent(inout):: gc
  character*(*),     intent(INOUT) :: expr
  character*(*),     intent(INOUT) :: vv(:)
  integer,           intent(INOUT) :: nb,nv
  integer,           intent(IN   ) :: nl
  integer, optional, intent(  OUT) :: RC
! Local variables:
  character(len=ESMF_MAXSTR)  :: Tmp
  integer                     :: l,nlength, status,num
  Logical :: flag=.true.
  character(len=ESMF_MAXSTR):: Iam,COMP_NAME

! Get my name and set-up traceback handle
! ---------------------------------------
  Iam = "ReplaceToken"
  call ESMF_GridCompGet(GC, NAME=COMP_NAME, RC=STATUS )
  VERIFY_(status)
  Iam = trim(COMP_NAME) // Iam

! ---------------------------------------
!if( MAPL_AM_I_ROOT() ) then
!  print *,trim(expr)
!endif
 
  nlength= count(vv/='')
  
  if(index(expr(nb:nl),'.') == 0) then
!     ASSERT_(scan(expr(nb:nb),'0123456789')==0)
     if(scan(expr(nb:nb),'0123456789')/=0)then
         flag=.false.;locate_(num);go to 999
     endif
     tmp= expr(nl+1:)

     do l=1,nv
        if(vv(l)==adjustl(expr(nb:nl))) exit
     enddo
     if(L==nv+1) then
!        ASSERT_(L<=size(vv))
        if(L>size(vv))then
           flag=.false.;locate_(num);go to 999
        endif
        nv = L
        vv(nv) = adjustl(expr(nb:nl))
     end if

     ASSERT_(L<=nlength)
     write(expr(nb:nb+1),'(i2.2)',iostat=status)L
     VERIFY_(STATUS)

     nb = nb+3
     expr(nb-1:) = adjustl(tmp)
  else
!     ASSERT_(scan(expr(nb:nb),'0123456789')/=0)
     if(scan(expr(nb:nb),'0123456789')==0)then
         flag=.false.;locate_(num);go to 999
     endif
     nb = nl+2
  endif

!if( MAPL_AM_I_ROOT() ) then
!  print *,'=',trim(expr)
!endif

999 &
if(flag==.false.) then
   if(MAPL_AM_I_ROOT()) then
      write(*,'(1x,a,i6)')&
      '===> Check Typos in the Arithmetic Parsing expression! Stop here at line:',num
      ASSERT_(flag)
   else
      rc= ESMF_FAILURE
   endif
endif


  RETURN_(ESMF_SUCCESS)
end subroutine ReplaceToken
!===========================================================

end module MAPL_NewArthParserMod

