



      subroutine vspow(z,y,x,n)
      real*4 x(*),y(*),z(*)
      do 10 j=1,n
      z(j)=y(j)**x(j)
   10 continue
      return
      end

      subroutine vpow(z,y,x,n)
      real*8 x(*),y(*),z(*)
      do 10 j=1,n
      z(j)=y(j)**x(j)
   10 continue
      return
      end

