        integer A(10,10,10)

        do i=1,10
         do j=1,10
           do k=1,10
             A(i,j,k)=1
           end do
         end do
        end do

       write(*,'(10i5)') (A(i,5,5),i=1,10) 
       call modify_array (A(:,5,5),10)
       write(*,'(10i5)') (A(i,5,5),i=1,10)

        stop
        end
 

       subroutine modify_array (a,n)
        integer a(n)
         do i=1,n
           a(i)=2
         end do
        return
        end 
         
