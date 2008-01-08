subroutine ext_ncd_ioinit(SysDepInfo, Status)
  CHARACTER*(*)             :: SysDepInfo
  INTEGER                   :: Status
return
end subroutine ext_ncd_ioinit

subroutine ext_ncd_open_for_read( FileName, Comm, IOComm, &
                 SysDepInfo, DataHandle, Status)
  character*(*)              :: FileName
  integer                    :: Comm
  integer                    :: IOComm
  character*(*)              :: SysDepInfo
  integer                    :: DataHandle
  integer                    :: Status
return
end subroutine ext_ncd_open_for_read

subroutine ext_ncd_get_dom_ti_char(DataHandle,Element,Data,Status)
  integer                   :: DataHandle
  character*(*)             :: Element
  integer                   :: Status
  character*(*)             :: Data(*)
return
end subroutine ext_ncd_get_dom_ti_char


subroutine ext_ncd_get_var_info(DataHandle,Name,NDim,MemoryOrder,&
      Stagger,DomainStart,DomainEnd,WrfType,Status)
  integer                   :: DataHandle
  character*(*)             :: Name
  integer                   :: NDim
  character*(*)             :: MemoryOrder
  character*(*)             :: Stagger ! Dummy for now
  integer ,dimension(*)     :: DomainStart, DomainEnd
  integer                   :: WrfType
  integer                   :: Status
return
end subroutine ext_ncd_get_var_info

subroutine ext_ncd_read_field(DataHandle,DateStr,Var,Field,FieldType,Comm,  &
  IOComm, DomainDesc, MemoryOrdIn, Stagger, DimNames,                       &
  DomainStart,DomainEnd,MemoryStart,MemoryEnd,PatchStart,PatchEnd,Status)
  integer                          :: DataHandle
  character*(*)                    :: DateStr
  character*(*)                    :: Var
  integer                          :: Field(*)
  integer                          :: FieldType
  integer                          :: Comm
  integer                          :: IOComm
  integer                          :: DomainDesc
  character*(*)                    :: MemoryOrdIn
  character*(*)                    :: Stagger ! Dummy for now
  character*(*) , dimension (*)    :: DimNames
  integer ,dimension(*)            :: DomainStart, DomainEnd
  integer ,dimension(*)            :: MemoryStart, MemoryEnd
  integer ,dimension(*)            :: PatchStart,  PatchEnd
  integer                          :: Status
return
end subroutine ext_ncd_read_field

subroutine ext_ncd_get_dom_ti_real(DataHandle,Element,Data,Count,&
       OutCount,Status)
  integer                   :: DataHandle
  character*(*)             :: Element
  real                      :: Data(*)
  integer                   :: Count
  integer                   :: OutCount
  integer                   :: Status
return
end subroutine ext_ncd_get_dom_ti_real

subroutine ext_ncd_get_dom_ti_integer(DataHandle,Element,Data,Count,&
       OutCount,Status)
  integer                   :: DataHandle
  character*(*)             :: Element
  integer                      :: Data(*)
  integer                   :: Count
  integer                   :: OutCount
  integer                   :: Status
return
end subroutine ext_ncd_get_dom_ti_integer







