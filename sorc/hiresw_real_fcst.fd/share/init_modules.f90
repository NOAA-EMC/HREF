

SUBROUTINE init_modules( phase )
 USE module_bc
 USE module_configure
 USE module_driver_constants
 USE module_model_constants
 USE module_domain
 USE module_machine
 USE module_nesting
 USE module_timing
 USE module_tiles
 USE module_io_wrf
 USE module_io
 USE module_wrf_quilt
 USE module_dm
 USE module_ext_internal




























 INTEGER, INTENT(IN) :: phase    
                                 

IF ( phase == 1 ) THEN
 CALL init_module_bc
 CALL init_module_configure
 CALL init_module_driver_constants
 CALL init_module_model_constants
 CALL init_module_domain
 CALL init_module_machine

 CALL init_module_ext_internal  
 CALL split_communicator
 CALL init_module_wrf_quilt    
 CALL init_module_dm
ELSE
 CALL init_module_nesting
 CALL init_module_timing
 CALL init_module_tiles
 CALL init_module_io_wrf
 CALL init_module_io


 CALL init_modules_nmm
ENDIF
 
END SUBROUTINE init_modules

