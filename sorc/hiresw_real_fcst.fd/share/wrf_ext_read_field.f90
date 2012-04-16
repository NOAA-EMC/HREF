
  SUBROUTINE wrf_ext_read_field( DataHandle,DateStr,Var,Field,FieldType,Comm,IOComm, &
                                 DomainDesc, bdy_mask, MemoryOrder,Stagger,             &
                                 debug_message ,                              &
                                 ds1, de1, ds2, de2, ds3, de3,                &
                                 ms1, me1, ms2, me2, ms3, me3,                &
                                 ps1, pe1, ps2, pe2, ps3, pe3, Status          )
    USE module_io
    USE module_wrf_error
    IMPLICIT NONE

    integer                                      :: DataHandle
    character*(*)                                :: DateStr
    character*(*)                                :: Var
    integer                                      :: Field(*)
    integer                                      :: FieldType
    integer                                      :: Comm
    integer                                      :: IOComm
    integer                                      :: DomainDesc
    logical, dimension(4)                        :: bdy_mask
    character*(*)                                :: MemoryOrder
    character*(*)                                :: Stagger
    character*(*)                                :: debug_message

    INTEGER ,       INTENT(IN   ) :: ds1, de1, ds2, de2, ds3, de3, &
                                     ms1, me1, ms2, me2, ms3, me3, &
                                     ps1, pe1, ps2, pe2, ps3, pe3

    INTEGER       itrace
    INTEGER , DIMENSION(3) :: domain_start , domain_end
    INTEGER , DIMENSION(3) :: memory_start , memory_end
    INTEGER , DIMENSION(3) :: patch_start , patch_end
    CHARACTER*80 , DIMENSION(3) :: dimnames

    integer                       ,intent(inout)   :: Status

    domain_start(1) = ds1 ; domain_end(1) = de1 ;
    patch_start(1)  = ps1 ; patch_end(1)  = pe1 ;
    memory_start(1) = ms1 ; memory_end(1) = me1 ;
    domain_start(2) = ds2 ; domain_end(2) = de2 ;
    patch_start(2)  = ps2 ; patch_end(2)  = pe2 ;
    memory_start(2) = ms2 ; memory_end(2) = me2 ;
    domain_start(3) = ds3 ; domain_end(3) = de3 ;
    patch_start(3)  = ps3 ; patch_end(3)  = pe3 ;
    memory_start(3) = ms3 ; memory_end(3) = me3 ;

    CALL debug_io_wrf ( debug_message,DateStr,                          &
                        domain_start,domain_end,patch_start,patch_end,  &
                        memory_start,memory_end                          )

    Status = 0

    CALL wrf_read_field (   &
                       DataHandle                 &  
                      ,DateStr                    &  
                      ,Var                        &  
                      ,Field                      &  
                      ,FieldType                  &  
                      ,Comm                       &  
                      ,IOComm                     &  
                      ,DomainDesc                 &  
                      ,bdy_mask                   &  
                      ,MemoryOrder                &  
                      ,Stagger                    &  
                      ,dimnames                   &  
                      ,domain_start               &  
                      ,domain_end                 &  
                      ,memory_start               &  
                      ,memory_end                 &  
                      ,patch_start                &  
                      ,patch_end                  &  
                      ,Status )
    IF ( wrf_at_debug_level(300) ) THEN
      WRITE(wrf_err_message,*) debug_message,' Status = ',Status
      CALL wrf_message ( TRIM(wrf_err_message) )
    ENDIF

  END SUBROUTINE wrf_ext_read_field

