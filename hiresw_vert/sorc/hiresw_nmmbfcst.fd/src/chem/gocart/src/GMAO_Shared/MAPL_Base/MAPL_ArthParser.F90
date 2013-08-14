! $Id: MAPL_ArthParser.F90,v 1.23 2008/01/09 16:19:09 f4mjs Exp $

#include "MAPL_Generic.h"

module MAPL_ArthParserMod

  use ESMF_Mod
  use MAPL_BaseMod

  implicit none

  private

  public Mapl_ExpressionEvaluate
  public Mapl_ExpressionGetVars

  integer, parameter :: MXREG = 99

  type RealPtrs
     integer       :: rank        = 0
     real, pointer :: Ptr2(:,:  ) => null()
     real, pointer :: Ptr3(:,:,:) => null()
  end type RealPtrs

  type ParserType
     type(RealPtrs)          :: REGVAR(MXREG)
     type(RealPtrs)          :: Left
     type(RealPtrs)          :: Right
     type(RealPtrs), pointer :: B(:) => null()
     real                    :: UNDEF
     integer                 :: IM, JM, LM
  end type ParserType

contains

subroutine MAPL_ExpressionEvaluate(Expression,Bundle,Undef,Ptr2,Ptr3,rc)

  character*(*),     intent(IN   ) :: Expression
  type(ESMF_Bundle), intent(IN   ) :: Bundle
  real,              intent(IN   ) :: Undef
  real,    optional, pointer       :: Ptr2(:,:)
  real,    optional, pointer       :: Ptr3(:,:,:)
  integer, optional, intent(  OUT) :: RC

  character(len=ESMF_MAXSTR)  :: Iam='MAPL_ExpressionEvaluate'
  integer                     :: status

  type(ParserType), target    :: Parser 
  integer                     :: n, nbeg, nend

  character*(len(Expression)) :: EXPR, NEWEXPR
  integer                     :: ResultReg
  type(RealPtrs),  pointer    :: Result

  character(len=ESMF_MAXSTR), &
                  allocatable :: NAMES(:)

! Begin
!------

! One of the result pointers must be present
!-------------------------------------------

  ASSERT_(present(Ptr2) .or. present(Ptr3))

! Make a copy of the original expression so that we can modify it
!----------------------------------------------------------------

  NEWEXPR = Expression

! Initialize Parser structure with variable list and 
!  array dimensions from the Bundle.
!---------------------------------------------------

  call MAPL_BundleGetDataPtrs(Bundle,Parser%B,Parser%IM,Parser%JM,Parser%LM,RC=STATUS)
  VERIFY_(STATUS)

  Parser%Undef = Undef

! Left and Right are used to hold scalars; these are always 2-d
!--------------------------------------------------------------

  call AllocatePtrs(Parser%Left ,Parser%IM,Parser%JM,rc=STATUS); VERIFY_(STATUS)
  call AllocatePtrs(Parser%Right,Parser%IM,Parser%JM,rc=STATUS); VERIFY_(STATUS)

! Process parenthetical expressions
!----------------------------------

  PARENS: do

! Find inner parens
!------------------
     
     NBEG = index(NEWEXPR,'(',BACK=.true.)

!  If none, evaluate final simple expression and exit
!----------------------------------------------------

     if(NBEG == 0) then
        call EvalSimpleExpr(NEWEXPR,Parser,ResultReg,RC=STATUS)
        VERIFY_(STATUS)
        exit
     end if

! Put exprssion contained in the parens into EXPR
!------------------------------------------------

     NEND = index(NEWEXPR(NBEG+1:),')') + NBEG
     EXPR = NEWEXPR(NBEG+1:NEND-1)

! We do not allow empty parens
!-----------------------------

     ASSERT_(len(trim(EXPR)) > 0)

! Evaluate deepest parenthetical expression and put the 
!  result in a register. EXPR now contains the 3 character
!  id of the result register.
!---------------------------------------------------------

     call EvalSimpleExpr(EXPR,Parser,ResultReg,RC=STATUS)
     VERIFY_(STATUS)

! Replace the parenthetical expression and its surrounding parens
!   with a register id in the full expression (NewExpr).
!----------------------------------------------------------------

     NewExpr = adjustl(trim(NewExpr(:NBEG-1))//trim(adjustl(EXPR))//adjustl(NewExpr(NEND+1:)))

  end do PARENS

! At this point only one register should be allocated and it
!  should contain the result.
  
  Result => Parser%RegVar(ResultReg)

!  N = count(associated(Parser%RegVar(:)%Ptr2)) + &
!      count(associated(Parser%RegVar(:)%Ptr3))
!
!  ASSERT_(N==1)

! Copy result pointers, checking for consistency between
!   rank of result and available operands.
!-------------------------------------------------------

  if(present(Ptr2)) then
     if(Result%rank==2) then
        Ptr2 => Result%Ptr2
     else
        Ptr2 => null()
        ASSERT_(present(Ptr3))
     end if
  endif

  if(present(Ptr3)) then
     if(Result%rank==3) then
        Ptr3 => Result%Ptr3
     else
        Ptr3 => null()
        ASSERT_(present(Ptr2))
     end if
  endif

! Clean-up
!---------

  call DeallocPtrs(Parser%Left )
  call DeallocPtrs(Parser%Right)

  deallocate(Parser%B)

  RETURN_(ESMF_SUCCESS)
end subroutine  MAPL_ExpressionEvaluate

!===========================================================

subroutine EvalSimpleExpr(EXPR,Parser,ResultReg,rc)
  character*(*),    intent(INOUT) :: EXPR
  type(ParserType), intent(INOUT) :: Parser
  integer,          intent(  OUT) :: ResultReg
  integer,optional, intent(  OUT) :: RC

! Evaluates an expression without parentheses, establishing
!  the precedence of the arithmetic operators as:
!
!    + - lower than  * / lower than  ^ 
!
! Each call to ProcOps treats one of these three levels
! clearing all ops at that level by combining their
! immediate operands from left to right.
!-----------------------------------------------------------

  character(len=ESMF_MAXSTR)      :: Iam='EvalSimpleExpr'
  character(len=ESMF_MAXSTR)      :: Exp
  integer                         :: status
  
!   EXPONENTATION
!----------------------------

  call procOps(EXPR,'^','^',PARSER,ResultReg,RC=STATUS)
  VERIFY_(STATUS)

!   MULTIPLICATION AND DIVISION
!----------------------------

  call procOps(EXPR,'*','/',PARSER,ResultReg,RC=STATUS)
  VERIFY_(STATUS)

!   ADDITION AND SUBTRACTION
!----------------------------

  call procOps(EXPR,'+','-',PARSER,ResultReg,RC=STATUS)
  VERIFY_(STATUS)

! Finally account for trivial expressions consisting of a 
!  single variable or scalar.
!--------------------------------------------------------

  if(Parser%RegVar(ResultReg)%rank==0 ) then
     EXP = '+'//adjustl(EXPR)
     call procOps(EXP,'+','+',PARSER,ResultReg,RC=STATUS)
     VERIFY_(STATUS)
  end if

  RETURN_(ESMF_SUCCESS)
end subroutine EvalSimpleExpr

!===============================================================

subroutine ProcOPS(EXPR,OP1,OP2,PARSER,ResultReg,rc)
  character*(*),    intent(INOUT) :: EXPR
  character*1,      intent(IN   ) :: OP1, OP2
  type(ParserType), intent(INOUT) :: PARSER
  integer,          intent(  OUT) :: ResultReg
  integer,optional, intent(  OUT) :: RC

  character(len=ESMF_MAXSTR)  :: Iam='ProcOPS'
  integer     :: pos1, pos2, status

  do
     POS1 = index(EXPR,OP1)
     POS2 = index(EXPR,OP2)

     if(POS1==0 .and. POS2 == 0) exit

     if(POS2 > 0 .and. (POS1==0 .or. POS2<POS1)) then
        call DODYAD(EXPR,OP2,POS2,PARSER,ResultReg,RC=STATUS)
        VERIFY_(STATUS)
     else
        call DODYAD(EXPR,OP1,POS1,PARSER,ResultReg,RC=STATUS)
        VERIFY_(STATUS)
     end if
  end do

  RETURN_(ESMF_SUCCESS)
end subroutine ProcOPS

!===============================================================

subroutine  DODYAD(EXPR,OP,POS,PARSER,ResultReg,rc)
  character*(*),    intent(INOUT) :: EXPR
  character*1,      intent(IN   ) :: OP
  integer,          intent(INOUT) :: POS
  type(ParserType), intent(INOUT) :: PARSER
  integer,          intent(  OUT) :: ResultReg
  integer,optional, intent(  OUT) :: RC


  character(len=ESMF_MAXSTR) :: Iam='DODYAD'
  integer                    :: STATUS,n

  character*(len(EXPR))      :: ARGL, ARGR
  character*3                :: ResultRegId
  integer                    :: NBEG, NEND, Itmp, IPL, NREG
  integer                    :: RegLeft, RegRight, Ndealloc, ResultRank
  real                       :: Tmp

  type(RealPtrs) :: Left, Right

! Get the immediate left and right operands of the dyad with
!  the op at POS. On exit, EXPR contains the expression with 
!  the dyad replaced by 'R##' and POS the index of the first '#'

  call GetDyadArgs(EXPR,POS,ARGL,ARGR)

! Obtain left  and right operands.
!---------------------------------

  call GetOperand(ARGL, Parser%RegVar, Parser%B, Parser%Left, Op, Left, RegLeft, rc=status)
  VERIFY_(STATUS)

  call GetOperand(ARGR, Parser%RegVar, Parser%B, Parser%Right,Op, Right,RegRight,rc=status)
  VERIFY_(STATUS)

! The rank of the result is the larger of the ranks
!  of the two operands
!--------------------------------------------------

  ResultRank = max(Left%rank,Right%rank)

! Set the result register and find out if we need to deallocate
! an operand register. If an operand register of the right
! rank exists we use it for the result, if not we find an empty
! slot in the register list Parser%RegVar. If we have two operand
! registers or one that has lesser rank than the result, we need
! to deallocate a register. This logic finds both the result
! register and the one to be deallocated, if any.
!--------------------------------------------------------------------------

  if    (RegLeft> 0 .and. RegRight> 0) then
     if(RegLeft>=RegRight) then
        Ndealloc  = RegRight
        ResultReg = RegLeft
     else
        Ndealloc  = RegLeft
        ResultReg = RegRight
     end if
  else if(RegLeft==0 .and. RegRight==0) then
     Ndealloc  = 0
     ResultReg = GetRegister(Parser%RegVar,rc=status)
     VERIFY_(STATUS)
  else if(Regleft==0                  ) then
     if(Parser%RegVar(RegRight)%rank == ResultRank) then
        ResultReg = RegRight
        Ndealloc  = 0
     else
        Ndealloc  = RegRight
        ResultReg = GetRegister(Parser%RegVar,rc=status)
        VERIFY_(STATUS)
     end if
  else
     if(Parser%RegVar(RegLeft)%rank == ResultRank) then
        Ndealloc  = 0
        ResultReg = RegLeft
     else
        Ndealloc  = RegLeft
        ResultReg = GetRegister(Parser%RegVar,rc=status)
        VERIFY_(STATUS)
     end if
  end if

! If necassary allocate the result register
!------------------------------------------

  if(Parser%RegVar(ResultReg)%rank == 0) then
     if(ResultRank==3) then
        call AllocatePtrs(Parser%RegVar(ResultReg),Parser%IM,Parser%JM,Parser%LM,RC=status)
        VERIFY_(STATUS)
     else
        call AllocatePtrs(Parser%RegVar(ResultReg),Parser%IM,Parser%JM,          RC=status)
        VERIFY_(STATUS)
     end if
  endif

! Update the expression with the result register index
!-----------------------------------------------------

  write(EXPR(POS:POS+1),'(I2.2)') ResultReg

! Evaluate the dyadic expression R = L op R
!------------------------------------------

  call EvalDyad(Parser%RegVar(ResultReg), Left, Right, OP, Parser%undef)

! Deallocate the unneeded register. This happens
!  when both operands where in registers
!  or when a single operand register had the wrong rank.
!--------------------------------------------------------

  if(Ndealloc /= 0) then
     call DeallocPtrs(Parser%RegVar(Ndealloc))
  endif

! All Done
!---------

  RETURN_(ESMF_SUCCESS)
end subroutine DODYAD


!===========================================================

subroutine EvalDyad(Result, Left,Right,OP,UNDEF,RC)
  type(RealPtrs),   intent(IN   ) :: Left, Right
  character*(1),    intent(IN   ) :: OP
  real,             intent(IN   ) :: UNDEF
  type(RealPtrs),   intent(  OUT) :: Result
  integer,optional, intent(  OUT) :: RC

  integer                     :: status
  character(len=ESMF_MAXSTR)  :: Iam='EvalDyad'

  ASSERT_(Left %rank>=2 .and. Left %rank<=3)
  ASSERT_(Right%rank>=2 .and. Right%rank<=3)

  ASSERT_(Result%rank==max(Right%rank,Left%rank))

  if    (Left%rank==2 .and. Right%rank==2 ) then
     call  L2R2(Result%Ptr2, Left%Ptr2, OP, Right%Ptr2, UNDEF)
  elseif(Left%rank==3 .and. Right%rank==3 ) then
     call  L3R3(Result%Ptr3, Left%Ptr3, OP, Right%Ptr3, UNDEF)
  elseif(Left%rank==2 .and. Right%rank==3 ) then
     call  L2R3(Result%Ptr3, Left%Ptr2, OP, Right%Ptr3, UNDEF)
  elseif(Left%rank==3 .and. Right%rank==2 ) then
     call  L3R2(Result%Ptr3, Left%Ptr3, OP, Right%Ptr2, UNDEF)
  end if

  return
end subroutine EvalDyad

subroutine MAPL_ExpressionGetVars(Expr,Vars,last,RC)
  character*(*),     intent(INOUT) :: Expr
  character*(*),     intent(INOUT) :: Vars(:)
  integer,           intent(  OUT) :: Last
  integer, optional, intent(  OUT) :: RC

  character(len=ESMF_MAXSTR)  :: Iam='MAPL_ExpressionGetVars'
  integer       :: nb, nl, nv, status, PosOfSymbol
  integer       :: Left, Right

! Remove blanks and check parens

  nb    = 1
  Left  = 0
  Right = 0

  do while(len_trim(expr(nb:))>0)
     if(expr(nb:nb)==' ') then 
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

  nv = count(Vars /= '')

! Loop over expr string looking for variables and replacing them
! with their 2-digit index into the vars array. If the are not
! present in the vars array, they are added to it and nv is bumped.
  
  nb    = 1
  do
     PosOfSymbol =  scan(expr(nb:),'+-*/^()')

     if(PosOfSymbol==1) then
        nb = nb + 1
     else
        if(PosOfSymbol==0) then
           nl = len_trim(expr)
        else
           nl = nb + PosOfSymbol - 2
        end if

        call ReplaceToken(expr,nb,nl,nv,Vars,rc=status)
        VERIFY_(STATUS)
     endif

     if(nb>len_trim(expr)) exit
  end do

  Last = NV

  RETURN_(ESMF_SUCCESS)


end subroutine MAPL_ExpressionGetVars


subroutine ReplaceToken(expr,nb,nl,nv,vv,rc)

  character*(*),     intent(INOUT) :: expr
  character*(*),     intent(INOUT) :: vv(:)
  integer,           intent(INOUT) :: nb,nv
  integer,           intent(IN   ) :: nl
  integer, optional, intent(  OUT) :: RC

  character(len=ESMF_MAXSTR)  :: Iam='ReplaceToken'
  character(len=ESMF_MAXSTR)  :: Tmp
  integer                     :: l, status


  if(index(expr(nb:nl),'.') == 0) then
     ASSERT_(scan(expr(nb:nb),'0123456789')==0)
     tmp  = expr(nl+1:)
     do l=1,nv
        if(vv(l)==adjustl(expr(nb:nl))) exit
     enddo

     if(L==nv+1) then
        ASSERT_(L<=size(vv))
        nv = L
        vv(nv) = adjustl(expr(nb:nl))
     end if

     write(expr(nb:nb+1),'(i2.2)',iostat=status) l
     VERIFY_(STATUS)

     nb = nb+3
     expr(nb-1:) = adjustl(tmp)
  else
     ASSERT_(scan(expr(nb:nb),'0123456789')/=0)
     nb = nl+2
  endif

  RETURN_(ESMF_SUCCESS)
end subroutine ReplaceToken

subroutine MAPL_BundleGetDataPtrs(Bundle,Result,im,jm,lm,rc)
  type(ESMF_Bundle),      intent(IN   ) :: Bundle
  type(RealPtrs), pointer               :: Result(:)
  integer,                intent(  OUT) :: im, jm, lm
  integer, optional,      intent(  OUT) :: RC

  character(len=ESMF_MAXSTR)  :: Iam='MAPL_BundleGetDataPtrs'
  character(len=ESMF_MAXSTR), pointer  :: names(:)

  integer          :: status
  integer          :: nrec,n
  type(ESMF_Field) :: Field
  type(ESMF_Array) :: Array


  call ESMF_BundleGet(Bundle,fieldCount=NREC,RC=STATUS)
  VERIFY_(STATUS)

  allocate(Result(NREC), stat=status)     
  VERIFY_(STATUS)
  allocate(NAMES (NREC), stat=status)     
  VERIFY_(STATUS)

  call ESMF_BundleGetFieldNames(Bundle,nameList=NAMES,RC=STATUS)
  VERIFY_(STATUS)

  im = -1
  jm = -1
  lm = -1

  do N=1,NREC
     call ESMF_BundleGetDataPointer(Bundle,names(n),Result(N)%Ptr2,rc=status)
     if(STATUS/=ESMF_SUCCESS) then

        call ESMF_BundleGetDataPointer(Bundle,names(n),Result(N)%Ptr3,rc=status)
        VERIFY_(STATUS)
        if(im==-1) then
           im = size(Result(N)%Ptr3,1)
           jm = size(Result(N)%Ptr3,2)
        else
           ASSERT_(IM==size(Result(N)%Ptr3,1))
           ASSERT_(JM==size(Result(N)%Ptr3,2))
        end if
        if(lm==-1) then
           lm = size(Result(N)%Ptr3,3)
        else
           ASSERT_(LM==size(Result(N)%Ptr3,3))
        end if
        Result(N)%rank = 3
        nullify(Result(N)%Ptr2)

     else

        if(im==-1) then
           im = size(Result(N)%Ptr2,1)
           jm = size(Result(N)%Ptr2,2)
        else
           ASSERT_(IM==size(Result(N)%Ptr2,1))
           ASSERT_(JM==size(Result(N)%Ptr2,2))
        end if
        Result(N)%rank = 2
        nullify(Result(N)%Ptr3)

     endif
  end do
  lm = max(1,lm)
  deallocate(NAMES)

  RETURN_(ESMF_SUCCESS)
end subroutine MAPL_BundleGetDataPtrs

subroutine NullifyPtrs(A)
  type(RealPtrs), intent(INOUT) :: A
  A%rank = 0
  nullify(A%Ptr2)
  nullify(A%Ptr3)
end subroutine NullifyPtrs

subroutine DeallocPtrs(A)
  type(RealPtrs), intent(INOUT) :: A
  if(associated(A%Ptr2)) deallocate(A%Ptr2)
  if(associated(A%Ptr3)) deallocate(A%Ptr3)
  call NullifyPtrs(A)
end subroutine DeallocPtrs


subroutine AllocatePtrs(A,IM,JM,LM,RC)
  type(RealPtrs),   intent(INOUT) :: A
  integer,          intent(IN   ) :: IM, JM
  integer,optional, intent(IN   ) :: LM
  integer,optional, intent(  OUT) :: RC

  integer STATUS
  character(len=ESMF_MAXSTR)  :: Iam='AllocatePtrs'

  if(present(LM)) then
     A%rank = 3
     nullify(A%Ptr2)
     allocate(A%Ptr3(IM,JM,LM),stat=status)
     VERIFY_(STATUS)
  else
     A%rank = 2
     nullify(A%Ptr3)
     allocate(A%Ptr2(IM,JM   ),stat=status)
     VERIFY_(STATUS)
  end if

  RETURN_(ESMF_SUCCESS)
end subroutine AllocatePtrs

!=================================================================

subroutine GetDyadArgs(EXPR,POS,ARGL,ARGR)
  character*(*),    intent(INOUT) :: EXPR
  integer,          intent(INOUT) :: POS
  character*(*),    intent(  OUT) :: ARGL
  character*(*),    intent(  OUT) :: ARGR

  integer :: nbeg, nend
  
  ARGL = adjustl(EXPR(:POS-1))
  ARGR = adjustl(EXPR(POS+1:))

  NBEG = scan(ARGL,'+-*/^',BACK=.true.) + 1
  NEND = scan(ARGR,'+-*/^')

  ARGL = ARGL(nbeg:)  

  if(NEND /= 0) then
     NEND = NEND - 1
     ARGR = adjustl(ARGR(:NEND))
  else
     NEND = len(trim(ARGR))
  end if

! Replace the dyad 'L op R' with the id of the result register
!-------------------------------------------------------------

  EXPR = EXPR(:NBEG-1)//'R##'//EXPR(NEND+POS+1:)
  POS  = NBEG+1

  return
end subroutine GetDyadArgs


function GetRegister(List,rc) result(Register)
  type(RealPtrs),   intent(IN   ) :: List(:)
  integer                         :: Register
  integer,optional, intent(  OUT) :: RC

  character(len=ESMF_MAXSTR)  :: Iam='GetRegister'

! Look for an empty register
!---------------------------

  Register=1
  do while(List(Register)%rank>0) 
     Register=Register+1
     ASSERT_(Register<=MXREG)
  end do

  RETURN_(ESMF_SUCCESS)
end function GetRegister


subroutine GetOperand(OprndString, RegList, VarList, Tmp, Op, Operand, Reg, rc)
  character*(*),    intent(IN   ) :: OprndString
  type(RealPtrs),   intent(IN   ) :: RegList(:), VarList(:)
  type(RealPtrs),   intent(INOUT) :: Tmp
  character*(1),    intent(IN   ) :: Op
  type(RealPtrs),   intent(  OUT) :: Operand
  integer,          intent(  OUT) :: Reg
  integer,optional, intent(  OUT) :: RC

! Get the actual operand from the operand char string.
!  Operand is contained in a RealPtr structure.
!----------------------------------------------------

  character(len=ESMF_MAXSTR)  :: Iam='GetOperand'
  real    :: scalar
  integer :: VarNum


! Reg is 0 if it anything other than a register; 
!  otherwise, it is the register index.
!-----------------------------------------------

  Reg  = index(OprndString,'R')

! Operand can be register, missing, a fixed-point float,
! or a variable.
!-------------------------------------------------------

  if(Reg /= 0) then
     read(OprndString(Reg:Reg+2),'(I2)') Reg
     Operand  = RegList(Reg)

  elseif(len(trim(OprndString))==0 .and. (OP=='+' .or. OP=='-')) then
     Tmp%Ptr2 = 0.0
     Operand  = Tmp

  elseif(index(OprndString,'.') /= 0) then
     read(OprndString,*) Scalar
     Tmp%Ptr2 = Scalar
     Operand  = Tmp

  else
     read(OprndString,*) VarNum
     ASSERT_(VarNum <= size(VarList))
     Operand  = VarList(VarNum)
  end if

  RETURN_(ESMF_SUCCESS)
end subroutine GetOperand


subroutine L2R2(Y,L,OP,R,Undef)
  real,          intent(IN) :: L(:,:), R(:,:)
  character*1,   intent(IN) :: OP
  real,          intent(IN) :: Undef
  real,          intent(OUT):: Y(:,:)
  
  Y = UNDEF

  select case (OP)
  case ('+')
     where(L/=UNDEF .and. R/=UNDEF                ) Y = L +  R
  case ('-')
     where(L/=UNDEF .and. R/=UNDEF                ) Y = L -  R
  case ('*')
     where(L/=UNDEF .and. R/=UNDEF                ) Y = L *  R
  case ('/')
     where(L/=UNDEF .and. R/=UNDEF .and. R  /= 0.0) Y = L /  R
  case ('^')
     where(L/=UNDEF .and. R/=UNDEF .and. L  >= 0.0) Y = L ** R
  end select

  return
end subroutine L2R2


subroutine L2R3(Y,L,OP,R,Undef)
  real,          intent(IN) :: L(:,:), R(:,:,:)
  character*(*), intent(IN) :: OP
  real,          intent(IN) :: Undef
  real,          intent(OUT):: Y(:,:,:)

  integer :: I 

  do I=1,size(R,3)
     call L2R2(Y(:,:,I),L,op,R(:,:,I),UNDEF)
  end do

end subroutine L2R3

subroutine L3R2(Y,L,OP,R,Undef)
  real,          intent(IN) :: L(:,:,:), R(:,:)
  character*(*), intent(IN) :: OP
  real,          intent(IN) :: Undef
  real,          intent(OUT):: Y(:,:,:)

  integer :: I

  do I=1,size(L,3)
     call L2R2(Y(:,:,I),L(:,:,I),op,R,UNDEF)
  end do

end subroutine L3R2

subroutine L3R3(Y,L,OP,R,Undef)
  real,          intent(IN) :: L(:,:,:), R(:,:,:)
  character*(*), intent(IN) :: OP
  real,          intent(IN) :: Undef
  real,          intent(OUT):: Y(:,:,:)

  integer :: I

  do I=1,size(L,3)
     call L2R2(Y(:,:,I),L(:,:,I),op,R(:,:,I),UNDEF)
  end do

end subroutine L3R3

end module MAPL_ArthParserMod
