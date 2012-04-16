
  SUBROUTINE wrf_ext_write_field(DataHandle,DateStr,Var,Field,FieldType,Comm,IOComm, &
                                 DomainDesc,                      &
                                 bdy_mask   ,                     &
                                 dryrun        ,                  &
                                 MemoryOrder,                     &
                                 Stagger,                         &
                                 Dimname1, Dimname2, Dimname3 ,   &
                                 Desc, Units,                     &
                                 debug_message ,                              &
                                 ds1, de1, ds2, de2, ds3, de3,                &
                                 ms1, me1, ms2, me2, ms3, me3,                &
                                 ps1, pe1, ps2, pe2, ps3, pe3, Status          )
    USE module_io
    USE module_wrf_error
    USE module_state_description
    USE module_timing
    IMPLICIT NONE

    INTEGER       itrace
    integer                                      :: DataHandle
    character*(*)                                :: DateStr
    character*(*)                                :: Var
    integer                                      :: Field(*)
    integer                                      :: FieldType
    integer                                      :: Comm
    integer                                      :: IOComm
    integer                                      :: DomainDesc
    logical                                      :: dryrun
    character*(*)                                :: MemoryOrder
    logical, dimension(4)                        :: bdy_mask
    character*(*)                                :: Stagger
    character*(*)                                :: Dimname1, Dimname2, Dimname3
    character*(*)                                :: Desc, Units
    character*(*)                                :: debug_message

    INTEGER ,       INTENT(IN   ) :: ds1, de1, ds2, de2, ds3, de3, &
                                     ms1, me1, ms2, me2, ms3, me3, &
                                     ps1, pe1, ps2, pe2, ps3, pe3

    
    INTEGER , DIMENSION(3) :: domain_start , domain_end
    INTEGER , DIMENSION(3) :: memory_start , memory_end
    INTEGER , DIMENSION(3) :: patch_start , patch_end
    CHARACTER*80 , DIMENSION(3) :: dimnames

    integer                       ,intent(inout)   :: Status
    LOGICAL for_out, horiz_stagger
    INTEGER Hndl, io_form
    LOGICAL, EXTERNAL :: has_char
    INTEGER, EXTERNAL :: use_package

    IF ( wrf_at_debug_level( 500 ) ) THEN
      call start_timing
    ENDIF
    domain_start(1) = ds1 ; domain_end(1) = de1 ;
    patch_start(1)  = ps1 ; patch_end(1)  = pe1 ;
    memory_start(1) = ms1 ; memory_end(1) = me1 ;
    domain_start(2) = ds2 ; domain_end(2) = de2 ;
    patch_start(2)  = ps2 ; patch_end(2)  = pe2 ;
    memory_start(2) = ms2 ; memory_end(2) = me2 ;
    domain_start(3) = ds3 ; domain_end(3) = de3 ;
    patch_start(3)  = ps3 ; patch_end(3)  = pe3 ;
    memory_start(3) = ms3 ; memory_end(3) = me3 ;

    dimnames(1) = Dimname1
    dimnames(2) = Dimname2
    dimnames(3) = Dimname3

    CALL debug_io_wrf ( debug_message,DateStr,                          &
                        domain_start,domain_end,patch_start,patch_end,  &
                        memory_start,memory_end                          )
    Status = 0


    CALL wrf_write_field (   &
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

    CALL get_handle ( Hndl, io_form , for_out, DataHandle )

    IF ( ( dryrun .AND. ( use_package(io_form) .EQ. IO_NETCDF .OR. &
                          use_package(io_form) .EQ. IO_PNETCDF ) ) .OR. &
                        ( use_package(io_form) .EQ. IO_PHDF5  )   ) THEN

      CALL wrf_put_var_ti_char( &
                       DataHandle                 &  
                      ,"description"              &  
                      ,Var                        &  
                      ,Desc                       &  
                      ,Status )
      CALL wrf_put_var_ti_char( &
                       DataHandle                 &  
                      ,"units"                    &  
                      ,Var                        &  
                      ,Units                      &  
                      ,Status )
      CALL wrf_put_var_ti_char( &
                       DataHandle                 &  
                      ,"stagger"                  &  
                      ,Var                        &  
                      ,Stagger                    &  
                      ,Status )
    ENDIF

    IF ( wrf_at_debug_level(300) ) THEN
      WRITE(wrf_err_message,*) debug_message,' Status = ',Status
      CALL wrf_message ( TRIM(wrf_err_message) )
    ENDIF

    IF ( wrf_at_debug_level( 500 ) ) THEN
      CALL end_timing('wrf_ext_write_field')
    ENDIF

  END SUBROUTINE wrf_ext_write_field
