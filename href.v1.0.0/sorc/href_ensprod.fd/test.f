        integer x(13)
        real miss(13),wgt(13)
        character*1 opr      
        data (x(i),i=1,13)
     +  /5,5,6,6,6,5,5,4,4,4,4,5,5/
        miss=0.0
        wgt=1.0
  
       opr='-'

       call getprob(x,13,5.0,6.0,opr,prob,miss,wgt)

        write(*,*) prob
        stop
        end

         
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c getporb: compute probability accoding to the given thresholds 
c Author: Binbin Zhou
c  Aug. 4, 2005
c
c  input: x     - data
c         thrs  - threshold
c         opr    - operation (>, < , =, - )
c
c  output: prob
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

	subroutine getprob(x,n,thrs1,thrs2,opr,prob,miss,wgt)
	real x(*), wgt(*), thrs1, thrs2, prob
        integer n,miss(*)
        character*1 opr
        real count


         wsum=0.0
         do i=1,n
           if(miss(i).eq.0) then
            wsum=wsum+wgt(i)
           end if 
         end do

        count = 0.0

        do i = 1, n

         if (miss(i).eq.0) then
          if (trim(opr).eq.'>') then
            if (x(i).ge.thrs1)count=count+wgt(i)/wsum
          else if (trim(opr).eq.'<') then
            if (x(i).le.thrs1) count=count+wgt(i)/wsum
          else if (trim(opr).eq.'=') then
            if (x(i).eq.thrs1) count=count+wgt(i)/wsum
          else if (trim(opr).eq.'-') then
            if (x(i).gt.thrs1.and.x(i).le.thrs2) 
     +       count = count + wgt(i)/wsum
          end if
         end if
        end do

         prob = 100.0 * count

        return
        end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   subroutine getmean: compute mean and spread of one dimension array  
c   Author: Binbin Zhou
c   Aug. 3, 2005
c   Apr. 7 2009: Zhou B. Add weight (wgt) for VSREF
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

	subroutine getmean (x,n,mean,spread,miss,wgt)
	 real x(*), wgt(*), mean, spread
         integer n,miss(*)
      

         wsum=0.0
         do i=1,n
           if(miss(i).eq.0) then
             wsum=wsum+wgt(i)
           end if
         end do
 
         if(wsum.eq.0.0) then         !if All members are missing
           mean = -9999.0
           spread=0.
           return
         end if

         mean = 0.
         do i=1,n
           if(miss(i).eq.0) then
            mean = mean + x(i)*(wgt(i)/wsum)
           end if
         end do

         spread = 0.
         do i = 1, n
           if(miss(i).eq.0) then
            spread = spread + (wgt(i)/wsum)*(x(i)-mean)**2
           end if
         end do

         spread = sqrt (spread )

         return
         end
                          
      
        subroutine getmean_fog (x, n,  mean, spread, miss,wgt)
         real x(*), wgt(*), mean, spread
         integer n,miss(*)


         wsum=0.0
         do i=1,n
          if(miss(i).eq.0) then          
           if(x(i).gt.0.0) then
            wsum=wsum+wgt(i)
           end if
          end if
         end do

         mean = 0.
         do i=1,n
          if(miss(i).eq.0) then
           if(wsum.gt.0.0) then
             mean = mean + x(i)*(wgt(i)/wsum)
           end if
          end if
         end do

         if(mean.eq.0.0) then
          spread = 0.0
          return
         end if


cvsref         mean = mean / n

         spread = 0.
         do i = 1, n
          if(miss(i).eq.0) then
           spread = spread + (wgt(i)/wsum)*(x(i)-mean)**2
          end if
         end do

         spread = sqrt (spread )

         return
         end

 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   subroutine get_cond_mean: compute conditional mean and spread of one dimension array
c      Conditional mean is the mean under some condition, if equal to certain
c      very large value, then it is not taken into mean, but spread still
c      count it (only "less than" case)
c
c   Author: Binbin Zhou
c   Aug. 3, 2005
c   Dec. 15, 2008: Use dominant numbers as mean
c   Apr. 7, 2009: Zhou B. Add weight (wgt) for VSREF
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
                                                                                                     
        subroutine get_cond_mean (x,n,alarge,mean,spread,miss,wgt)
         real x(*),wgt(30), mean, spread, alarge,count, half
         integer n,miss(*)
                                                                                                     
         mean = 0.
         count = 0.

         wsum=0.0
         count = 0.
         do 102 i=1,n
           if(miss(i).eq.0) then
             if(x(i).ge.alarge) goto 102
             wsum=wsum+wgt(i)
             count = count + 1.0
           end if
102      continue
         half=count/2.0
    
         do 100 i=1,n
          if (miss(i).eq.0) then
           if(x(i).ge.alarge) goto 100 
            mean = mean + x(i)*(wgt(i)/wsum)
          end if
100      continue 
                     
         if( count .le. half ) then           !only most of member happen                                                             
           mean = alarge
         end if 
                                                                                            
         spread = 0.
         do 200 i = 1, n                         
          if(x(i).ge.alarge.or.miss(i).eq.1) goto 200                       !!! modified in Apr. 9 2008 
           spread = spread + (wgt(i)/wsum)*(x(i)-mean)**2
200      continue
            
         if (mean.eq.alarge) then
             spread=0.0              !!! for VSREF, assume this. Apr. 22, 2009
         else 
             spread = sqrt (spread )
         end if
       
         return
         end


        subroutine get_cond_mean_test(igrid,x,n,alarge,mean,spread,
     +         miss,wgt)
         real x(*),wgt(30), mean, spread, alarge,count, half
         integer n,miss(*)

         mean = 0.
         count = 0.

         wsum=0.0
         count = 0.
         do 102 i=1,n
          if(miss(i).eq.0) then
           if(x(i).ge.alarge) goto 102
           wsum=wsum+wgt(i)
           count = count + 1.0
          end if
102      continue
         half=count/2.0
 
         if(igrid.eq.737601) write(*,*)'half=',half,wsum
  
         do 100 i=1,n
           if(miss(i).eq.0) then
            if(x(i).ge.alarge) goto 100
            mean = mean + x(i)*(wgt(i)/wsum)
            if(igrid.eq.737601) write(*,*)i,'mean=',mean
           end if
100      continue
 
         if( count .le. half ) then           !only most of member happen

           mean = alarge
         end if

         if(igrid.eq.737601) write(*,*)'mean=',mean


         spread = 0.
         do 200 i = 1, n 
          if(x(i).ge.alarge.or.miss(i).eq.1) goto 200                       !!! modified in Apr. 9 2008
           spread = spread + (wgt(i)/wsum)*(x(i)-mean)**2
           if(igrid.eq.737601) write(*,*)i,'spread=',spread
200      continue

         if (mean.eq.alarge) then
             spread=0.0              !!! for VSREF, assume this. Apr. 22, 2009
         else
             spread = sqrt (spread )
             if(igrid.eq.737601) write(*,*)'spread=',spread
         end if

         return
         end

        subroutine get_cond_mean_lwc(x,n,alarge,mean,spread,miss)
         real x(*), mean, spread, alarge,count
         integer n,miss(*)
                                                                                                                                                                                       
         mean = 0.
         count = 0.
                                                                                                                                                                                       
         do 100 i=1,n
           if(x(i).eq.alarge.and.miss(i).eq.0) goto 100
            mean = mean + x(i)
            count = count + 1.0
100      continue
                                                                                                                                                                                       
         if(count .gt. 0.0) then
          mean = mean / count
         else
          mean = 0.0
         end if
                    
         spread = 0.
         count = 0.
         do i = 1, n
           if(x(i).gt.alarge.and.miss(i).eq.0) then
            count = count + 1.0
            spread = spread + (x(i)-mean)**2
           end if
         end do
          
         if(count .gt. 0.0) then          
            spread = sqrt (spread / count )
         else 
            spread = 0.0
         end if
           
         return
         end

 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   subroutine getwindmean: compute mean and spread of wind vector
c   Author: Binbin Zhou
c   March 1, 2006
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
                                                                                                                                          
        subroutine getwindmean (u,v,n, mean, spread,miss,wgt)
         real u(*),v(*), Umean, Vmean, Uspread,Vspread,
     +             mean, spread,count,wgt(*)         
         integer n,miss(*) 
                                                                                                                                          
         Umean = 0.
         Vmean = 0.
         mean = 0.
         count=0.0

         wsum=0.0
         do i=1,n
          if(miss(i).eq.0) then
c           Umean = Umean + u(i)
c           Vmean = Vmean + v(i)
c           mean = mean + sqrt(v(i)*v(i)+u(i)*u(i))
c           count=count+1.0
           wsum=wsum+wgt(i)          
          end if
         end do
        

         do i=1,n
           if(miss(i).eq.0) then
            Umean = Umean + u(i)*(wgt(i)/wsum)
            Vmean = Vmean + v(i)*(wgt(i)/wsum)
            mean = mean + sqrt(v(i)*v(i)+u(i)*u(i))
     +        *(wgt(i)/wsum)
           end if
         end do
     
         Uspread = 0.
         Vspread = 0.
         spread = 0.
         do i = 1, n
          if(miss(i).eq.0) then
           Uspread=Uspread+(wgt(i)/wsum)*(u(i)-Umean)**2
           Vspread=Vspread+(wgt(i)/wsum)*(v(i)-Vmean)**2
          end if
         end do
 
         Uspread=sqrt(Uspread)     
         Vspread=sqrt(Vspread)     
         spread = sqrt(Uspread*Uspread + Vspread*Vspread)

         return 
         end

 
