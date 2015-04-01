	program find_factors
      
!   check divisibility by 2,3,5,7,11

!	write(6,*) 'enter value to check'
	read(5,*) ival
	
	ival_orig=ival

!	write(6,*) 'ival_orig...into program: ', ival_orig
	
!	if (mod(ival,2) .ne. 0) then
	ival=ival-1
!	endif

!	write(6,*) 'ival passed into check_factor: ', ival

	CALL check_factor (ival, iret)

!	write(6,*) 'ival, iret: ', ival, iret

	if (iret .eq. ival-1) ival=iret+1

!	write(6,*) 'ival post iftest: ', ival

	if (iret .ne. 1) then

	do
	ival=ival+2
	CALL check_factor (ival, iret)
         if (iret .eq. 1) exit
	end do

	endif

!	write(6,*) 'ended with valid length of ', ival
	write(6,*) ival

	end program find_factors

!!!!!!!!!!!!!!!!

	SUBROUTINE check_factor (ival_input, iret)

        INTEGER:: ival, ival_input, iret

	ival=ival_input

	icheck_11=ival/11
	imod_11 = mod(ival,11)
	if (imod_11 .eq. 0) then
	 ival=icheck_11
	endif

	icheck_7=ival/7
	imod_7=mod(ival,7)
	if (imod_7 .eq. 0) then
	 ival=icheck_7
        endif

	icheck_5=ival/5
	imod_5=mod(ival,5)
	if (imod_5 .eq. 0) then
	 ival=icheck_5
        endif

	do N=1,2
	icheck_3=ival/3
	imod_3=mod(ival,3)
	if (imod_3 .eq. 0) then
	 ival=icheck_3
        endif
	enddo

	do N=1,25
	icheck_2=ival/2
	imod_2=mod(ival,2)
	if (imod_2 .eq. 0) then
	 ival=icheck_2
        endif
	enddo

!	if (ival .eq. 1) then
!	write(6,*) ival_orig, ' is a good value'
!	else
!	write(6,*) ival_orig, ' is an INVALID CHOICE'
!	endif

	iret=ival

	end subroutine check_factor
