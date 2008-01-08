!WRF:MEDIATION_LAYER:IO
!

MODULE module_io_domain
USE module_io
USE module_io_wrf
USE module_date_time

   PRIVATE open_dataset

CONTAINS

SUBROUTINE output_history ( fid , grid , config_flags , ierr )
   USE module_domain
   USE module_state_description
   USE module_configure
   IMPLICIT NONE
   TYPE(domain) :: grid
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
   INTEGER fid, im , ierr
   call output_history_wrf( fid , grid , config_flags , ierr )
   RETURN
END SUBROUTINE output_history

SUBROUTINE output_aux_hist1 ( fid , grid , config_flags , ierr )
   USE module_domain
   USE module_state_description
   USE module_configure
   IMPLICIT NONE
   TYPE(domain) :: grid
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
   INTEGER fid, im , ierr
   call output_aux_hist1_wrf( fid , grid , config_flags , ierr )
   RETURN
END SUBROUTINE output_aux_hist1

SUBROUTINE output_aux_hist2 ( fid , grid , config_flags , ierr )
   USE module_domain
   USE module_state_description
   USE module_configure
   IMPLICIT NONE
   TYPE(domain) :: grid
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
   INTEGER fid, im , ierr
   call output_aux_hist2_wrf( fid , grid , config_flags , ierr )
   RETURN
END SUBROUTINE output_aux_hist2

SUBROUTINE output_aux_hist3 ( fid , grid , config_flags , ierr )
   USE module_domain
   USE module_state_description
   USE module_configure
   IMPLICIT NONE
   TYPE(domain) :: grid
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
   INTEGER fid, im , ierr
   call output_aux_hist3_wrf( fid , grid , config_flags , ierr )
   RETURN
END SUBROUTINE output_aux_hist3

SUBROUTINE output_aux_hist4 ( fid , grid , config_flags , ierr )
   USE module_domain
   USE module_state_description
   USE module_configure
   IMPLICIT NONE
   TYPE(domain) :: grid
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
   INTEGER fid, im , ierr
   call output_aux_hist4_wrf( fid , grid , config_flags , ierr )
   RETURN
END SUBROUTINE output_aux_hist4

SUBROUTINE output_aux_hist5 ( fid , grid , config_flags , ierr )
   USE module_domain
   USE module_state_description
   USE module_configure
   IMPLICIT NONE
   TYPE(domain) :: grid
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
   INTEGER fid, im , ierr
   call output_aux_hist5_wrf( fid , grid , config_flags , ierr )
   RETURN
END SUBROUTINE output_aux_hist5

SUBROUTINE output_restart ( fid , grid , config_flags , ierr )
   USE module_domain
   USE module_state_description
   USE module_configure
   IMPLICIT NONE
   TYPE(domain) :: grid
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
   INTEGER fid, im , ierr
   call output_restart_wrf( fid , grid , config_flags , ierr )
   RETURN
END SUBROUTINE output_restart

SUBROUTINE output_model_input ( fid , grid , config_flags , ierr )
   USE module_domain
   USE module_state_description
   USE module_configure
   IMPLICIT NONE
   TYPE(domain) :: grid
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
   INTEGER fid, im , ierr
   call output_model_input_wrf( fid , grid , config_flags , ierr )
   RETURN
END SUBROUTINE output_model_input

SUBROUTINE output_aux_model_input1 ( fid , grid , config_flags , ierr )
   USE module_domain
   USE module_state_description
   USE module_configure
   IMPLICIT NONE
   TYPE(domain) :: grid
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
   INTEGER fid, im , ierr
   call output_aux_model_input1_wrf( fid , grid , config_flags , ierr )
   RETURN
END SUBROUTINE output_aux_model_input1

SUBROUTINE output_aux_model_input2 ( fid , grid , config_flags , ierr )
   USE module_domain
   USE module_state_description
   USE module_configure
   IMPLICIT NONE
   TYPE(domain) :: grid
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
   INTEGER fid, im , ierr
   call output_aux_model_input2_wrf( fid , grid , config_flags , ierr )
   RETURN
END SUBROUTINE output_aux_model_input2

SUBROUTINE output_aux_model_input3 ( fid , grid , config_flags , ierr )
   USE module_domain
   USE module_state_description
   USE module_configure
   IMPLICIT NONE
   TYPE(domain) :: grid
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
   INTEGER fid, im , ierr
   call output_aux_model_input3_wrf( fid , grid , config_flags , ierr )
   RETURN
END SUBROUTINE output_aux_model_input3

SUBROUTINE output_aux_model_input4 ( fid , grid , config_flags , ierr )
   USE module_domain
   USE module_state_description
   USE module_configure
   IMPLICIT NONE
   TYPE(domain) :: grid
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
   INTEGER fid, im , ierr
   call output_aux_model_input4_wrf( fid , grid , config_flags , ierr )
   RETURN
END SUBROUTINE output_aux_model_input4

SUBROUTINE output_aux_model_input5 ( fid , grid , config_flags , ierr )
   USE module_domain
   USE module_state_description
   USE module_configure
   IMPLICIT NONE
   TYPE(domain) :: grid
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
   INTEGER fid, im , ierr
   call output_aux_model_input5_wrf( fid , grid , config_flags , ierr )
   RETURN
END SUBROUTINE output_aux_model_input5

SUBROUTINE output_boundary ( fid , grid , config_flags , ierr )
   USE module_domain
   USE module_state_description
   USE module_configure
   IMPLICIT NONE
   TYPE(domain) :: grid
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
   INTEGER fid, im , ierr

	IF ( grid%io_form_boundary .eq. 1 ) THEN
	write(0,*) 'call output_boundary_wrfspec'
	ierr=0
   call output_boundary_wrfspec( fid , grid , config_flags , ierr )
	write(0,*) 'return output_boundary_wrfspec ', ierr
	ELSE
	write(0,*) 'call output_boundary_wrf (normal)'
   call output_boundary_wrf( fid , grid , config_flags , ierr )
	ENDIF

   RETURN
END SUBROUTINE output_boundary

SUBROUTINE input_history ( fid,  grid , config_flags )
   USE module_domain
   USE module_state_description
   USE module_configure
   IMPLICIT NONE
   TYPE(domain) :: grid
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
   INTEGER fid, im , ierr
   call input_history_wrf( fid , grid , config_flags , ierr )
   RETURN
END SUBROUTINE input_history

SUBROUTINE input_aux_hist1 ( fid,  grid , config_flags )
   USE module_domain
   USE module_state_description
   USE module_configure
   IMPLICIT NONE
   TYPE(domain) :: grid
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
   INTEGER fid, im , ierr
   call input_aux_hist1_wrf( fid , grid , config_flags , ierr )
   RETURN
END SUBROUTINE input_aux_hist1

SUBROUTINE input_aux_hist2 ( fid,  grid , config_flags )
   USE module_domain
   USE module_state_description
   USE module_configure
   IMPLICIT NONE
   TYPE(domain) :: grid
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
   INTEGER fid, im , ierr
   call input_aux_hist2_wrf( fid , grid , config_flags , ierr )
   RETURN
END SUBROUTINE input_aux_hist2

SUBROUTINE input_aux_hist3 ( fid,  grid , config_flags )
   USE module_domain
   USE module_state_description
   USE module_configure
   IMPLICIT NONE
   TYPE(domain) :: grid
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
   INTEGER fid, im , ierr
   call input_aux_hist3_wrf( fid , grid , config_flags , ierr )
   RETURN
END SUBROUTINE input_aux_hist3

SUBROUTINE input_aux_hist4 ( fid,  grid , config_flags )
   USE module_domain
   USE module_state_description
   USE module_configure
   IMPLICIT NONE
   TYPE(domain) :: grid
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
   INTEGER fid, im , ierr
   call input_aux_hist4_wrf( fid , grid , config_flags , ierr )
   RETURN
END SUBROUTINE input_aux_hist4

SUBROUTINE input_aux_hist5 ( fid,  grid , config_flags )
   USE module_domain
   USE module_state_description
   USE module_configure
   IMPLICIT NONE
   TYPE(domain) :: grid
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
   INTEGER fid, im , ierr
   call input_aux_hist5_wrf( fid , grid , config_flags , ierr )
   RETURN
END SUBROUTINE input_aux_hist5

SUBROUTINE input_restart ( fid,  grid , config_flags , ierr )
   USE module_domain
   USE module_state_description
   USE module_configure
   IMPLICIT NONE
   TYPE(domain) :: grid
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
   INTEGER fid, im , ierr
   call input_restart_wrf( fid , grid , config_flags , ierr )
   RETURN
END SUBROUTINE input_restart

SUBROUTINE input_model_input ( fid,  grid , config_flags , ierr )
   USE module_domain
   USE module_state_description
   USE module_configure
   IMPLICIT NONE
   TYPE(domain) :: grid
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
   INTEGER fid, im , ierr
   call input_model_input_wrf( fid , grid , config_flags , ierr )
   RETURN
END SUBROUTINE input_model_input

SUBROUTINE input_aux_model_input1 ( fid,  grid , config_flags , ierr )
   USE module_domain
   USE module_state_description
   USE module_configure
   IMPLICIT NONE
   TYPE(domain) :: grid
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
   INTEGER fid, im , ierr
   call input_aux_model_input1_wrf( fid , grid , config_flags , ierr )
   RETURN
END SUBROUTINE input_aux_model_input1

SUBROUTINE input_aux_model_input2 ( fid,  grid , config_flags , ierr )
   USE module_domain
   USE module_state_description
   USE module_configure
   IMPLICIT NONE
   TYPE(domain) :: grid
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
   INTEGER fid, im , ierr
   call input_aux_model_input2_wrf( fid , grid , config_flags , ierr )
   RETURN
END SUBROUTINE input_aux_model_input2

SUBROUTINE input_aux_model_input3 ( fid,  grid , config_flags , ierr )
   USE module_domain
   USE module_state_description
   USE module_configure
   IMPLICIT NONE
   TYPE(domain) :: grid
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
   INTEGER fid, im , ierr
   call input_aux_model_input3_wrf( fid , grid , config_flags , ierr )
   RETURN
END SUBROUTINE input_aux_model_input3

SUBROUTINE input_aux_model_input4 ( fid,  grid , config_flags , ierr )
   USE module_domain
   USE module_state_description
   USE module_configure
   IMPLICIT NONE
   TYPE(domain) :: grid
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
   INTEGER fid, im , ierr
   call input_aux_model_input4_wrf( fid , grid , config_flags , ierr )
   RETURN
END SUBROUTINE input_aux_model_input4

SUBROUTINE input_aux_model_input5 ( fid,  grid , config_flags , ierr )
   USE module_domain
   USE module_state_description
   USE module_configure
   IMPLICIT NONE
   TYPE(domain) :: grid
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
   INTEGER fid, im , ierr
   call input_aux_model_input5_wrf( fid , grid , config_flags , ierr )
   RETURN
END SUBROUTINE input_aux_model_input5

SUBROUTINE input_boundary ( fid,  grid , config_flags , ierr )
   USE module_domain
   USE module_state_description
   USE module_configure
   IMPLICIT NONE
   TYPE(domain) :: grid
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
   INTEGER fid, im , ierr
	write(0,*) 'io_form_boundary: ', grid%io_form_boundary
	IF ( grid%io_form_boundary .eq. 1 ) THEN
	write(0,*) 'call input_boundary_wrfspec'
   call input_boundary_wrfspec( fid , grid , config_flags , ierr )
	write(0,*) 'return input_boundary_wrfspec'
        ELSE
   call input_boundary_wrf( fid , grid , config_flags , ierr )
	ENDIF
   RETURN
END SUBROUTINE input_boundary

SUBROUTINE open_r_dataset ( id , fname , grid , config_flags , sysdepinfo, ierr )
   USE module_domain
   USE module_io_wrf
   USE module_configure
   TYPE (domain)             :: grid
   CHARACTER*(*) :: fname
   CHARACTER*(*) :: sysdepinfo
   INTEGER      , INTENT(INOUT) :: id , ierr
   LOGICAL , EXTERNAL :: wrf_dm_on_monitor
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
   CHARACTER*128             :: DataSet
   LOGICAL                   :: anyway
   CALL wrf_open_for_read ( fname ,                     &
                            grid%communicator ,         &
                            grid%iocommunicator ,       &
                            sysdepinfo ,                &
                            id ,                        &
                            ierr )
   RETURN
END SUBROUTINE

SUBROUTINE open_w_dataset ( id , fname , grid , config_flags , outsub , sysdepinfo, ierr )
   USE module_domain
   USE module_io_wrf
   USE module_configure
   USE module_date_time
   TYPE (domain)             :: grid
   CHARACTER*(*) :: fname
   CHARACTER*(*) :: sysdepinfo
   INTEGER      , INTENT(INOUT) :: id , ierr
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
   LOGICAL , EXTERNAL :: wrf_dm_on_monitor
   EXTERNAL outsub
   CHARACTER*128             :: DataSet
   LOGICAL                   :: anyway
   CALL wrf_debug ( 100 , 'calling wrf_open_for_write_begin in open_w_dataset' )
	write(0,*) 'call wrf_open_for_write_begin'
   CALL wrf_open_for_write_begin ( fname ,     &
                                   grid%communicator ,         &
                                   grid%iocommunicator ,       &
                                   sysdepinfo ,                &
                                   id ,                        &
                                   ierr )
	write(0,*) 'return wrf_open_for_write_begin'
   IF ( ierr .LE. 0 ) THEN
     CALL wrf_debug ( 100 , 'calling outsub in open_w_dataset' )
	write(0,*) 'call outsub'
     CALL outsub( id , grid , config_flags , ierr )
	write(0,*) 'return outsub'
   ENDIF
   IF ( ierr .LE. 0 ) THEN
     CALL wrf_debug ( 100 , 'calling wrf_open_for_write_commit in open_w_dataset' )
	write(0,*) 'call wrf_open_for_write_commit'
     CALL wrf_open_for_write_commit ( id ,                        &
                                      ierr )
	write(0,*) 'return wrf_open_for_write_commit'
   ENDIF
END SUBROUTINE open_w_dataset

SUBROUTINE close_dataset( id , config_flags, sysdepinfo ) 
   USE module_configure
   IMPLICIT NONE
   INTEGER id , ierr
   LOGICAL , EXTERNAL :: wrf_dm_on_monitor
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
   CHARACTER*(*) :: sysdepinfo
   CHARACTER*128             :: DataSet
   LOGICAL                   :: anyway
   CALL wrf_ioclose( id , ierr )
END SUBROUTINE close_dataset

END MODULE module_io_domain

! move outside module so callable without USE of module
SUBROUTINE construct_filename1( result , basename , fld1 , len1 )
  IMPLICIT NONE
  CHARACTER*(*) :: result
  CHARACTER*(*) :: basename
  INTEGER , INTENT(IN) :: fld1 , len1
  CHARACTER*64         :: t1, zeros
  
  CALL zero_pad ( t1 , fld1 , len1 )
  result = TRIM(basename) // "_d" // TRIM(t1)
  RETURN
END SUBROUTINE construct_filename1

SUBROUTINE construct_filename2( result , basename , fld1 , len1 , date_char )
  IMPLICIT NONE
  CHARACTER*(*) :: result
  CHARACTER*(*) :: basename
  CHARACTER*(*) :: date_char
  INTEGER , INTENT(IN) :: fld1 , len1
  CHARACTER*64         :: t1, zeros
  
  CALL zero_pad ( t1 , fld1 , len1 )
  result = TRIM(basename) // ".d" // TRIM(t1) // "." // TRIM(date_char)
  RETURN
END SUBROUTINE construct_filename2

SUBROUTINE construct_filename ( result , basename , fld1 , len1 , fld2 , len2 )
  IMPLICIT NONE
  CHARACTER*(*) :: result
  CHARACTER*(*) :: basename
  INTEGER , INTENT(IN) :: fld1 , len1 , fld2 , len2
  CHARACTER*64         :: t1, t2, zeros
  
  CALL zero_pad ( t1 , fld1 , len1 )
  CALL zero_pad ( t2 , fld2 , len2 )
  result = TRIM(basename) // "_d" // TRIM(t1) // "_" // TRIM(t2)
  RETURN
END SUBROUTINE construct_filename

SUBROUTINE construct_filename3 ( result , basename , fld1 , len1 , fld2 , len2, fld3, len3 )
  IMPLICIT NONE
  CHARACTER*(*) :: result
  CHARACTER*(*) :: basename
  INTEGER , INTENT(IN) :: fld1 , len1 , fld2 , len2, fld3, len3
  CHARACTER*64         :: t1, t2, t3, zeros

  CALL zero_pad ( t1 , fld1 , len1 )
  CALL zero_pad ( t2 , fld2 , len2 )
  CALL zero_pad ( t3 , fld3 , len3 )
  result = TRIM(basename) // "_d" // TRIM(t1) // "_" // TRIM(t2) // "_" // TRIM(t3)
  RETURN
END SUBROUTINE construct_filename3

SUBROUTINE append_to_filename ( result , basename , fld1 , len1 )
  IMPLICIT NONE
  CHARACTER*(*) :: result
  CHARACTER*(*) :: basename
  INTEGER , INTENT(IN) :: fld1 , len1
  CHARACTER*64         :: t1, zeros
  
  CALL zero_pad ( t1 , fld1 , len1 )
  result = TRIM(basename) // "_" // TRIM(t1)
  RETURN
END SUBROUTINE append_to_filename


SUBROUTINE zero_pad ( result , fld1 , len1 )
  IMPLICIT NONE
  CHARACTER*(*) :: result
  INTEGER , INTENT (IN)      :: fld1 , len1
  INTEGER                    :: d , x
  CHARACTER*64         :: t2, zeros
  x = fld1 ; d = 0
  DO WHILE ( x > 0 )
    x = x / 10
    d = d + 1
  END DO
  write(t2,'(I9)')fld1
  zeros = '0000000000000000000000000000000'
  result = zeros(1:len1-d) // t2(9-d+1:9)
  RETURN
END SUBROUTINE zero_pad


SUBROUTINE init_wrfio
   USE module_io
   CALL wrf_ioinit(ierr)
END SUBROUTINE init_wrfio

