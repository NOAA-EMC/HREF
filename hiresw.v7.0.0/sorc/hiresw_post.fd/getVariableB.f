!!!@PROCESS NOEXTCHK
subroutine getVariableB(fileName,DateStr,dh,VarName,VarBuff,IM,JSTA_2L,JEND_2U,LM,IM1,JS,JE,LM1)


!     SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    getVariable    Read data from WRF output
!   PRGRMMR: MIKE BALDWIN    ORG: NSSL/SPC   DATE: 2002-04-08
!
! ABSTRACT:  THIS ROUTINE READS DATA FROM A WRF OUTPUT FILE
!   USING WRF I/O API.
!   .
!
! PROGRAM HISTORY LOG:
!   02-10-31  H CHUANG - MODIFY TO READ WRF BINARY OUTPUT
!
! USAGE:    CALL getVariable(fileName,DateStr,dh,VarName,VarBuff,IM,JSTA_2L,JEND_2U,LM,IM1,JS,JE,LM1)
!
!   INPUT ARGUMENT LIST:
!     fileName : Character(len=256) : name of WRF output file
!     DateStr  : Character(len=19)  : date/time of requested variable
!     dh :  integer                 : data handle
!     VarName :  Character(len=31)  : variable name
!     IM :  integer  : X dimension of data array
!     JSTA_2L :  integer  : start Y dimension of data array
!     JEND_2U :  integer  : end Y dimension of data array
!     LM :  integer  : Z dimension of data array
!     IM1 :  integer  : amount of data pulled in X dimension 
!     JS :  integer  : start Y dimension of amount of data array pulled
!     JE :  integer  : end Y dimension of amount of data array pulled
!     LM1 :  integer  : amount of data pulled in Z dimension
!
!   data is flipped in the Z dimension from what is originally given
!   the code requires the Z dimension to increase with pressure
!
!   OUTPUT ARGUMENT LIST:
!     VarBuff : real(IM,JSTA_2L:JEND_2U,LM) : requested data array
!
!   OUTPUT FILES:
!     NONE
!
!   SUBPROGRAMS CALLED:
!     UTILITIES:
!       NONE
!     LIBRARY:
!       WRF I/O API
!       NETCDF

 ! This subroutine reads the values of the variable named VarName into the buffer
 ! VarBuff. VarBuff is filled with data only for I=1,IM1 and for J=JS,JE
 ! and for L=1,Lm1, presumably this will be
 ! the portion of VarBuff that is needed for this task.

   use ctlblk_mod, only: me, MPI_COMM_COMP, icnt, idsp
   use wrf_io_flags_mod
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   implicit none
   include "mpif.h"
!
   character(len=256) ,intent(in) :: fileName
   character(len=19) ,intent(in) :: DateStr
   integer ,intent(in) :: dh
   character(*) ,intent(in) :: VarName
   real,intent(out) :: VarBuff(IM,JSTA_2L:JEND_2U,LM)
   integer,intent(in) :: IM,LM,JSTA_2L,JEND_2U
   integer,intent(in) :: IM1,LM1,JS,JE
   integer :: ndim
   integer :: WrfType,i,j,l,ll,K,INDEX,sizesend
   integer, dimension(4) :: start_index, end_index
   character (len= 4) :: staggering
   character (len= 3) :: ordering
   character (len=80), dimension(3) :: dimnames
   real, allocatable, dimension(:,:,:,:) :: data
   real, allocatable, dimension(:) :: data_1d, data_1d_out
   integer :: ierr
   character(len=132) :: Stagger

   start_index = 1
   end_index = 1
!     write(*,*)'fileName,DateStr,dh,VarName in getVariable= ',fileName,DateStr,dh,VarName
   call ext_int_get_var_info(dh,TRIM(VarName),ndim,ordering,Stagger,start_index,end_index,WrfType,ierr)
      call mpi_bcast(ndim,1,MPI_integer,0,MPI_COMM_COMP,ierr)
      call mpi_bcast(end_index,4,MPI_integer,0,MPI_COMM_COMP,ierr)


!!     write(0,*)' VarName,end_index(1,2,3)= ',VarName,end_index(1),end_index(2),end_index(3)
   IF ( ierr /= 0 ) THEN
     write(*,*)'Error: ',ierr,TRIM(VarName),' not found in ',fileName
!CHUANG make sure data=0 when not found in wrf output
     data=0.
   VarBuff=0.
     go to 27  
   ENDIF
   if( WrfType /= WRF_REAL .AND. WrfType /= WRF_REAL8) then !Ignore if not a real variable
     write(*,*) 'Error: Not a real variable',WrfType
     return
   endif
!  write(*,'(A9,1x,I1,3(1x,I3),1x,A,1x,A)')&
!           trim(VarName), ndim, end_index(1), end_index(2), end_index(3), &
!           trim(ordering), trim(DateStr)

   allocate(data (end_index(1), end_index(2), end_index(3), 1))
!   call ext_int_read_field(dh,DateStr,TRIM(VarName),data,WrfType,0,0,0,ordering,&
! Chuang: change WrfType to WRF_REAL because this specifys the data type 
! WRF IO API will output to "data"

	if (allocated(data_1d)) then 
		deallocate(data_1d)
	endif

	if (allocated(data_1d_out)) then 
		deallocate(data_1d_out)
	endif

	if (ME .eq. 0) then
   call ext_int_read_field(dh,DateStr,TRIM(VarName),data,WrfType,0,0,0,ordering,&
                             staggering, dimnames , &
                             start_index,end_index, & !dom 
                             start_index,end_index, & !mem
                             start_index,end_index, & !pat
                             ierr)
   IF ( ierr /= 0 ) THEN
     write(*,*)'Error reading ',Varname,' from ',fileName
     write(*,*)' ndim = ', ndim
     write(*,*)' end_index(1) ',end_index(1)
     write(*,*)' end_index(2) ',end_index(2)
     write(*,*)' end_index(3) ',end_index(3)
   ENDIF


!CHUANG: accomendate different array arrangement with different ndim
   if(ndim.eq.1)then
    if(lm1.gt.end_index(1))write(*,*) 'Err:',Varname,' LM1=',lm1,&
                ' but data dim=',end_index(1)
   else if(ndim.eq.2)then
    if (im1.gt.end_index(1)) write(*,*) 'Err:',Varname,' IM1=',im1,&
                ' but data dim=',end_index(1)
    if (je.gt.end_index(2)) write(*,*) 'Err:',Varname,' JE=',je,&
                ' but data dim=',end_index(2)
   else if(ndim.eq.3)then
    if (im1.gt.end_index(1)) write(*,*) 'Err:',Varname,' IM1=',im1,&
                ' but data dim=',end_index(1)
    if (je.gt.end_index(2)) write(*,*) 'Err:',Varname,' JE=',je,&
                ' but data dim=',end_index(2)
    if (lm1.gt.end_index(3)) write(*,*) 'Err:',Varname,' LM1=',lm1,&
                ' but data dim=',end_index(3)
   end if

   if (ndim.gt.3) then
     write(*,*) 'Error: ndim = ',ndim
   endif 

!       else
!	write(0,*) 'skipped ext_int_read_field'

	endif

        sizesend=(end_index(1)-start_index(1)+1)* &
                 (end_index(2)-start_index(2)+1)* &
                 (end_index(3)-start_index(3)+1)

! note, the complication added by converting to data_1d did not
! allow the code to get any further along.

	allocate(data_1d(sizesend))
	data_1d=0.

	if (ME .eq. 0) then
        do J=1,end_index(3)
        do K=1,end_index(2)
        do I=1,end_index(1)
	INDEX=(J-1)*(end_index(2)*end_index(1))+((K-1)*end_index(1))+I
	data_1d(INDEX)=data(I,K,J,1)
	enddo
	enddo
	enddo
	endif

!	write(0,*) 'size(data_1d) sizesend: ', size(data_1d), sizesend
	
      call mpi_bcast(data_1d,sizesend,MPI_real4,0,MPI_COMM_COMP,ierr)

!	write(0,*) 'ierr from mpi_bcast: ', ierr

! confirm that everyone has data??

        do J=1,end_index(3)
        do K=1,end_index(2)
        do I=1,end_index(1)
	INDEX=(J-1)*(end_index(2)*end_index(1))+((K-1)*end_index(1))+I
	data(I,K,J,1)=data_1d(INDEX)
	enddo
	enddo
	enddo

!	write(0,*) 'have data(1,1,1,1) now: ', data(1,1,1,1)

   if (ndim .eq. 0)then
    VarBuff(1,1,1)=data(1,1,1,1)
   else if(ndim .eq. 1)then
    do l=1,lm
      VarBuff(1,1,l)=data(l,1,1,1)
    end do
   else if(ndim .eq. 2)then
    do i=1,im1
      do j=js,je
       VarBuff(i,j,1)=data(i,j,1,1)
      enddo
    enddo 
   else if(ndim .eq. 3)then
    do l=1,lm1
     ll=lm1-l+1  ! flip the z axis not sure about soil
     do i=1,im1
      do j=js,je
       VarBuff(i,j,l)=data(i,j,ll,1)
!mp       VarBuff(i,j,l)=data(i,ll,j,1)
      enddo
     enddo
!     write(*,*) Varname,' L ',l,': = ',data(1,1,ll,1)
    enddo
   end if
 27 continue
   deallocate(data)
   return

end subroutine getVariableB


!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine getVariableBikj(fileName,DateStr,dh,VarName,VarBuff,IM,JSTA_2L,JEND_2U,LM,IM1,&
                          JS,JE,LM1)


!     SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    getVariable    Read data from WRF output
!   PRGRMMR: MIKE BALDWIN    ORG: NSSL/SPC   DATE: 2002-04-08
!
! ABSTRACT:  THIS ROUTINE READS DATA FROM A WRF OUTPUT FILE
!   USING WRF I/O API.
!   .
!
! PROGRAM HISTORY LOG:
!   02-10-31  H CHUANG - MODIFY TO READ WRF BINARY OUTPUT
!
! USAGE:    CALL getVariable(fileName,DateStr,dh,VarName,VarBuff,IM,JSTA_2L,JEND_2U,LM,IM1,JS,JE,LM1)
!
!   INPUT ARGUMENT LIST:
!     fileName : Character(len=256) : name of WRF output file
!     DateStr  : Character(len=19)  : date/time of requested variable
!     dh :  integer                 : data handle
!     VarName :  Character(len=31)  : variable name
!     IM :  integer  : X dimension of data array
!     JSTA_2L :  integer  : start Y dimension of data array
!     JEND_2U :  integer  : end Y dimension of data array
!     LM :  integer  : Z dimension of data array
!     IM1 :  integer  : amount of data pulled in X dimension 
!     JS :  integer  : start Y dimension of amount of data array pulled
!     JE :  integer  : end Y dimension of amount of data array pulled
!     LM1 :  integer  : amount of data pulled in Z dimension
!
!   data is flipped in the Z dimension from what is originally given
!   the code requires the Z dimension to increase with pressure
!
!   OUTPUT ARGUMENT LIST:
!     VarBuff : real(IM,JSTA_2L:JEND_2U,LM) : requested data array
!
!   OUTPUT FILES:
!     NONE
!
!   SUBPROGRAMS CALLED:
!     UTILITIES:
!       NONE
!     LIBRARY:
!       WRF I/O API
!       NETCDF

 ! This subroutine reads the values of the variable named VarName into the buffer
 ! VarBuff. VarBuff is filled with data only for I=1,IM1 and for J=JS,JE
 ! and for L=1,Lm1, presumably this will be
 ! the portion of VarBuff that is needed for this task.

   use wrf_io_flags_mod
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   implicit none
!
   character(len=256) ,intent(in) :: fileName
   character(len=19) ,intent(in) :: DateStr
   integer ,intent(in) :: dh
   character(*) ,intent(in) :: VarName
   real,intent(out) :: VarBuff(IM,JSTA_2L:JEND_2U,LM)
   integer,intent(in) :: IM,LM,JSTA_2L,JEND_2U
   integer,intent(in) :: IM1,LM1,JS,JE
   integer :: ndim
   integer :: WrfType,i,j,l,ll
   integer, dimension(4) :: start_index, end_index
   character (len= 4) :: staggering
   character (len= 3) :: ordering
   character (len=80), dimension(3) :: dimnames
   real, allocatable, dimension(:,:,:,:) :: data
   integer :: ierr
   character(len=132) :: Stagger


!        write(6,*) 'using ikj reader'

   start_index = 1
   end_index = 1
!     write(*,*)'fileName,DateStr,dh,VarName in getVariable= ',fileName,DateStr,dh,VarName
   call ext_int_get_var_info(dh,TRIM(VarName),ndim,ordering,Stagger,start_index,end_index,WrfType,ierr)
!     write(*,*)'VarName,end_index(1,2,3),ndim= ',VarName,end_index(1),end_index(2),end_index(3),ndim   
   IF ( ierr /= 0 ) THEN
     write(*,*)'Error: ',ierr,TRIM(VarName),' not found in ',fileName
!CHUANG make sure data=0 when not found in wrf output
     data=0.
   VarBuff=0.
     go to 27  
   ENDIF
   if( WrfType /= WRF_REAL .AND. WrfType /= WRF_REAL8) then !Ignore if not a real variable
     write(*,*) 'Error: Not a real variable',WrfType
     return
   endif
!  write(*,'(A9,1x,I1,3(1x,I3),1x,A,1x,A)')&
!           trim(VarName), ndim, end_index(1), end_index(2), end_index(3), &
!           trim(ordering), trim(DateStr)

   allocate(data (end_index(1), end_index(2), end_index(3), 1))
!   call ext_int_read_field(dh,DateStr,TRIM(VarName),data,WrfType,0,0,0,ordering,&
! Chuang: change WrfType to WRF_REAL because this specifys the data type 
! WRF IO API will output to "data"
   call ext_int_read_field(dh,DateStr,TRIM(VarName),data,WrfType,0,0,0,ordering,&
                             staggering, dimnames , &
                             start_index,end_index, & !dom 
                             start_index,end_index, & !mem
                             start_index,end_index, & !pat
                             ierr)
   IF ( ierr /= 0 ) THEN
     write(*,*)'Error reading ',Varname,' from ',fileName
     write(*,*)' ndim = ', ndim
     write(*,*)' end_index(1) ',end_index(1)
     write(*,*)' end_index(2) ',end_index(2)
     write(*,*)' end_index(3) ',end_index(3)
   ENDIF
!CHUANG: accomendate different array arrangement with different ndim
   if(ndim.eq.1)then
    if(lm1.gt.end_index(1))write(*,*) 'Err:',Varname,' LM1=',lm1,&
                ' but data dim=',end_index(1)
   else if(ndim.eq.2)then
    if (im1.gt.end_index(1)) write(*,*) 'Err:',Varname,' IM1=',im1,&
                ' but data dim=',end_index(1)
    if (je.gt.end_index(2)) write(*,*) 'Err:',Varname,' JE=',je,&
                ' but data dim=',end_index(2)
   else if(ndim.eq.3)then
    if (im1.gt.end_index(1)) write(*,*) 'Err:',Varname,' IM1=',im1,&
                ' but data dim=',end_index(1)
    if (je.gt.end_index(3)) write(*,*) 'Err:',Varname,' JE=',je,&
                ' but data dim=',end_index(3)
    if (lm1.gt.end_index(2)) write(*,*) 'Err:',Varname,' LM1=',lm1,&
                ' but data dim=',end_index(2)
   end if

   if (ndim.gt.3) then
     write(*,*) 'Error: ndim = ',ndim
   endif 

   if (ndim .eq. 0)then
    VarBuff(1,1,1)=data(1,1,1,1)
   else if(ndim .eq. 1)then
    do l=1,lm
      VarBuff(1,1,l)=data(l,1,1,1)
    end do
   else if(ndim .eq. 2)then
    do i=1,im1
      do j=js,je
       VarBuff(i,j,1)=data(i,j,1,1)
      enddo
    enddo 
   else if(ndim .eq. 3)then
    do l=1,lm1
     ll=lm1-l+1  ! flip the z axis not sure about soil
     do i=1,im1
      do j=js,je
       VarBuff(i,j,l)=data(i,ll,j,1)
      enddo
     enddo
!     write(0,*) Varname,' L ',l,': = ',data(1,ll,1,1)
!     write(*,*) Varname,' L ',l,': = ',data(1,ll,1,1)
    enddo
   end if
 27 continue
   deallocate(data)
   return

end subroutine getVariableBikj

subroutine getVariableBikj_p(fileName,DateStr,dh,VarName,VarBuff,IM,JSTA_2L,JEND_2U,LM,IM1,&
                          JS,JE,LM1)


!     SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    getVariable    Read data from WRF output
!   PRGRMMR: MIKE BALDWIN    ORG: NSSL/SPC   DATE: 2002-04-08
!
! ABSTRACT:  THIS ROUTINE READS DATA FROM A WRF OUTPUT FILE
!   USING WRF I/O API.
!   .
!
! PROGRAM HISTORY LOG:
!   02-10-31  H CHUANG - MODIFY TO READ WRF BINARY OUTPUT
!
! USAGE:    CALL getVariable(fileName,DateStr,dh,VarName,VarBuff,IM,JSTA_2L,JEND_2U,LM,IM1,JS,JE,LM1)
!
!   INPUT ARGUMENT LIST:
!     fileName : Character(len=256) : name of WRF output file
!     DateStr  : Character(len=19)  : date/time of requested variable
!     dh :  integer                 : data handle
!     VarName :  Character(len=31)  : variable name
!     IM :  integer  : X dimension of data array
!     JSTA_2L :  integer  : start Y dimension of data array
!     JEND_2U :  integer  : end Y dimension of data array
!     LM :  integer  : Z dimension of data array
!     IM1 :  integer  : amount of data pulled in X dimension 
!     JS :  integer  : start Y dimension of amount of data array pulled
!     JE :  integer  : end Y dimension of amount of data array pulled
!     LM1 :  integer  : amount of data pulled in Z dimension
!
!   data is flipped in the Z dimension from what is originally given
!   the code requires the Z dimension to increase with pressure
!
!   OUTPUT ARGUMENT LIST:
!     VarBuff : real(IM,JSTA_2L:JEND_2U,LM) : requested data array
!
!   OUTPUT FILES:
!     NONE
!
!   SUBPROGRAMS CALLED:
!     UTILITIES:
!       NONE
!     LIBRARY:
!       WRF I/O API
!       NETCDF

 ! This subroutine reads the values of the variable named VarName into the buffer
 ! VarBuff. VarBuff is filled with data only for I=1,IM1 and for J=JS,JE
 ! and for L=1,Lm1, presumably this will be
 ! the portion of VarBuff that is needed for this task.


   use ctlblk_mod, only: me, MPI_COMM_COMP, arw_icnt, arw_idsp, &
                         arw_icnt_u, arw_idsp_u, &
                         arw_icnt_v, arw_idsp_v,jsvmine,jevmine
   use wrf_io_flags_mod
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   implicit none

   include "mpif.h"
!
   character(len=256) ,intent(in) :: fileName
   character(len=19) ,intent(in) :: DateStr
   integer ,intent(in) :: dh
   character(*) ,intent(in) :: VarName
   real,intent(out) :: VarBuff(IM,JSTA_2L:JEND_2U,LM)
   integer,intent(in) :: IM,LM,JSTA_2L,JEND_2U
   integer,intent(in) :: IM1,LM1,JS,JE
   integer :: ndim,sizesend
   integer :: WrfType,i,j,l,ll,K,INDEX,LMLOC, LOCDIM
   integer, dimension(4) :: start_index, end_index
   character (len= 4) :: staggering
   character (len= 3) :: ordering
   character (len=80), dimension(3) :: dimnames
   real, allocatable, dimension(:,:,:,:) :: data
   real, allocatable, dimension(:) :: data_1d, data_1d_out
   integer :: ierr
   character(len=132) :: Stagger

!        write(6,*) 'using ikj reader'

   start_index = 1
   end_index = 1
!     write(*,*)'fileName,DateStr,dh,VarName in getVariable= ',fileName,DateStr,dh,VarName


   call ext_int_get_var_info(dh,TRIM(VarName),ndim,ordering,Stagger,start_index,end_index,WrfType,ierr)

      call mpi_bcast(ndim,1,MPI_integer,0,MPI_COMM_COMP,ierr)
      call mpi_bcast(end_index,4,MPI_integer,0,MPI_COMM_COMP,ierr)

! get on root task and broadcast?

! do all tasks return the same end_index values?  Seemed like no earlier
!    write(0,*)'ME,VarName,end_index(1,2,3),ndim= ',ME,trim(VarName),end_index(1),end_index(2),end_index(3),ndim

   IF ( ierr /= 0 ) THEN
     write(0,*)'Error: ',ierr,TRIM(VarName),' not found in ',fileName
!CHUANG make sure data=0 when not found in wrf output
     data=0.
   VarBuff=0.
     go to 27  
   ENDIF
   if( WrfType /= WRF_REAL .AND. WrfType /= WRF_REAL8) then !Ignore if not a real variable
     write(*,*) 'Error: Not a real variable',WrfType
     return
   endif
!  write(*,'(A9,1x,I1,3(1x,I3),1x,A,1x,A)')&
!           trim(VarName), ndim, end_index(1), end_index(2), end_index(3), &
!           trim(ordering), trim(DateStr)

   allocate(data (end_index(1), end_index(2), end_index(3), 1))

!   call ext_int_read_field(dh,DateStr,TRIM(VarName),data,WrfType,0,0,0,ordering,&
! Chuang: change WrfType to WRF_REAL because this specifys the data type 
! WRF IO API will output to "data"

! restrict this to a single root task, then distribute?

	if (allocated(data_1d)) then 
		deallocate(data_1d)
	endif

	if (allocated(data_1d_out)) then 
		deallocate(data_1d_out)
	endif

	if (ME .eq. 0) then

   call ext_int_read_field(dh,DateStr,TRIM(VarName),data,WrfType,0,0,0,ordering,&
                             staggering, dimnames , &
                             start_index,end_index, & !dom 
                             start_index,end_index, & !mem
                             start_index,end_index, & !pat
                             ierr)

	write(0,*) 'root sees data(1,1,1,1) as: ', data(1,1,1,1)
   IF ( ierr /= 0 ) THEN
     write(*,*)'Error reading ',Varname,' from ',fileName
     write(*,*)' ndim = ', ndim
     write(*,*)' end_index(1) ',end_index(1)
     write(*,*)' end_index(2) ',end_index(2)
     write(*,*)' end_index(3) ',end_index(3)
   ENDIF

!CHUANG: accomendate different array arrangement with different ndim
   if(ndim.eq.1)then
    if(lm1.gt.end_index(1))write(*,*) 'Err:',Varname,' LM1=',lm1,&
                ' but data dim=',end_index(1)
   else if(ndim.eq.2)then
    if (im1.gt.end_index(1)) write(*,*) 'Err:',Varname,' IM1=',im1,&
                ' but data dim=',end_index(1)
    if (je.gt.end_index(2)) write(*,*) 'Err:',Varname,' JE=',je,&
                ' but data dim=',end_index(2)
   else if(ndim.eq.3)then
    if (im1.gt.end_index(1)) write(*,*) 'Err:',Varname,' IM1=',im1,&
                ' but data dim=',end_index(1)
    if (je.gt.end_index(3)) write(*,*) 'Err:',Varname,' JE=',je,&
                ' but data dim=',end_index(3)
    if (lm1.gt.end_index(2)) write(*,*) 'Err:',Varname,' LM1=',lm1,&
                ' but data dim=',end_index(2)
   end if

   if (ndim.gt.3) then
     write(*,*) 'Error: ndim = ',ndim
   endif 

	endif

        sizesend=(end_index(1)-start_index(1)+1)* &
                 (end_index(2)-start_index(2)+1)* &
                 (end_index(3)-start_index(3)+1)

! note, the complication added by converting to data_1d did not
! allow the code to get any further along.

	allocate(data_1d(sizesend))
	data_1d=0.

	if (ME .eq. 0) then
        do J=1,end_index(3)
        do K=1,end_index(2)
        do I=1,end_index(1)
	INDEX=(J-1)*(end_index(2)*end_index(1))+((K-1)*end_index(1))+I
	data_1d(INDEX)=data(I,K,J,1)
	enddo
	enddo
	enddo
	endif

	LMLOC=end_index(2)

	if (trim(VarName) == 'U') then
	 allocate(data_1d_out(LMLOC*arw_icnt_u(me)))
         call mpi_scatterv(data_1d,     LMLOC*arw_icnt_u, LMLOC*arw_idsp_u, MPI_real4, &
                           data_1d_out, LMLOC*arw_icnt_u(me),         MPI_real4, 0, MPI_COMM_COMP, ierr )
	write(0,*) 'data_1d_out allocated for U: ', LMLOC*arw_icnt_u(me)
	else if (trim(VarName) == 'V') then

        LOCDIM=(jevmine(me)-jsvmine(me)+1)*(im1)*LMLOC
	write(0,*) 'me, arw_icnt_v(me), LMLOC, jsvmine, jevmine: ', me, arw_icnt_v(me), LMLOC, jsvmine(me), jevmine(me)
	 allocate(data_1d_out(LMLOC*arw_icnt_v(me)))
	write(0,*) 'data_1d_out allocated for V: jsvmine,jevmine ', jsvmine(me),jevmine(me), LMLOC*arw_icnt_v(me), &
                   LOCDIM

	if (LOCDIM .gt. LMLOC*arw_icnt_v(me) ) then
	write(0,*) 'local fill space is larger than what will be available' , me
	write(0,*) 'local fill space is larger than what will be available', me
	write(0,*) 'STOP'
	STOP
	endif


         call mpi_scatterv(data_1d,     LMLOC*arw_icnt_v, LMLOC*arw_idsp_v, MPI_real4, &
                           data_1d_out, LMLOC*arw_icnt_v(me),         MPI_real4, 0, MPI_COMM_COMP, ierr )
        else
	 allocate(data_1d_out(LMLOC*arw_icnt(me)))
!	write(0,*) 'data_1d_out allocated for scalar: ', LMLOC*arw_icnt(me)
         call mpi_scatterv(data_1d,     LMLOC*arw_icnt, LMLOC*arw_idsp, MPI_real4, &
                           data_1d_out, LMLOC*arw_icnt(me),         MPI_real4, 0, MPI_COMM_COMP, ierr )
	endif
	
!      call mpi_bcast(data_1d,sizesend,MPI_real4,0,MPI_COMM_COMP,ierr)
!!	write(0,*) 'ierr from mpi_bcast: ', ierr
!
!        do J=1,end_index(3)
!        do K=1,end_index(2)
!        do I=1,end_index(1)
!	INDEX=(J-1)*(end_index(2)*end_index(1))+((K-1)*end_index(1))+I
!	data(I,K,J,1)=data_1d(INDEX)
!	enddo
!	enddo
!	enddo

! mpi_scatterv stuff	

!
	if (trim(VarName) == 'V') then

	write(0,*) 'jsvmine, jevmine: ', jsvmine(me), jevmine(me)
        do J=jsvmine(me),jevmine(me)
        do K=1,end_index(2)
        do I=1,im1

	INDEX=((j-jsvmine(me)+1)-1)*(end_index(2)*im1)+((K-1)*im1)+I

	if (INDEX .gt. size(data_1d_out)) then
	write(0,*) 'Js, I,J,K,INDEX,size: ',js, I,J,K,INDEX,size(data_1d_out)
	endif

	data(I,K,J,1)=data_1d_out(INDEX)
        enddo
        enddo
	enddo

	else

        do J=js,je
        do K=1,end_index(2)
        do I=1,im1

	INDEX=((j-js+1)-1)*(end_index(2)*im1)+((K-1)*im1)+I

	if (INDEX .gt. size(data_1d_out)) then
	write(0,*) 'Js, I,J,K,INDEX,size: ',js, I,J,K,INDEX,size(data_1d_out)
!	write(0,*) 'shape(data): ', shape(data)
	endif

	data(I,K,J,1)=data_1d_out(INDEX)
        enddo
        enddo
	enddo

	endif

!	write(0,*) 'have data(1,1,1,1) now: ', data(1,1,1,1)

   if (ndim .eq. 0)then
    VarBuff(1,1,1)=data(1,1,1,1)
   else if(ndim .eq. 1)then
    do l=1,lm
      VarBuff(1,1,l)=data(l,1,1,1)
    end do
   else if(ndim .eq. 2)then
    do i=1,im1
      do j=js,je
       VarBuff(i,j,1)=data(i,j,1,1)
      enddo
    enddo 
   else if(ndim .eq. 3)then
    do l=1,lm1
     ll=lm1-l+1  ! flip the z axis not sure about soil
     do i=1,im1
      do j=js,je
       VarBuff(i,j,l)=data(i,ll,j,1)
      enddo
     enddo
!     write(0,*) Varname,' L ',l,': = ',data(1,ll,1,1)
!     write(*,*) Varname,' L ',l,': = ',data(1,ll,1,1)
    enddo
   end if
 27 continue
   deallocate(data)
   return

end subroutine getVariableBikj_p
