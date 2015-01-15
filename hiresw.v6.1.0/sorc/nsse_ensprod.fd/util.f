cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc  getlvl function is to get the number 'nn' from 'Mnn/Pnn/Dnn/Hnn'
c   Author: Binbin Zhou
C*  B. Zhou      03/2013 Adapted onto Zeus/wcoss Linux compiler version
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
   
	function getlvl(S)
 	integer getlvl, Length, temp
        character*3  S, S1
  
        Length=LEN_TRIM(S)
        S1=S(2:Length)
        call ST_NUMB(S1, temp, ier)
         getlvl = temp
        return
        end
        
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C This function is to get grib id  from region string x 
C such as  x = 'G211/region' or just x = 'G212', 
C Where 212 is grib id
C
C Author Binbin Zhou, 
c        Mar 20, 2005
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        function ID (x)
        character(20) x, gribid
        integer p, length, y
        p = index(x,'/')
        length = len_trim(x)
        if (p.gt.0) then
         gribid = x (2:p-1)
        else
         gribid = x (2:length)
        end if

        call st_numb (gribid, y, ier)
        ID = y

        return
        end 

       
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C This function is to get numerical number from a string x, 
C such as x = '100.05', '.5', '100', '100.', etc
C Author: Binbin ZHou
C         Mar 20, 2005
Ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

	function CharToReal(x)     
        real CharToReal
        character(*) x 
        character*20 p1, p2
        integer p, length, d1, d2, pp2
        real r1,r2       

        length = len_trim(x)
        p = index(x,'.')       !e.g.  x=.15: p=1
        if (p.eq.1) then
          p1 = '0'
          p2 = x(p+1:length)
          pp2= len_trim(p2)
        else if (p.gt.1) then   !e.g. x=12.5: p=3
          p1 = x (1:p-1)
          p2 = x(p+1:length)
          pp2= len_trim(p2)
        else
          p1 = x
          p2 = '0'
          pp2 = 0
        end if

        call st_numb (p1,d1,ier)
        call st_numb (p2,d2,ier)

        r1 = float(d2)

        if(pp2.eq.1) then
          r2 = r1 /10.0
        else if (pp2.eq.2) then
          r2 = r1 /100.0
        else if (pp2.eq.3) then
          r2 = r1 /1000.0
        else if (pp2.eq.4) then
          r2 = r1 /10000.0
        else if (pp2.eq.5) then
          r2 = r1 /100000.0
        else if (pp2.eq.6) then
          r2 = r1 /1000000.0
        end if
    
        if(d1 .ge. 0) then 
          CharToReal = d1 + r2
        else
          CharToReal = -1.0 * (abs(d1) + r2)
        end if

         return  
         end


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   function index_table() returns the numvar index of pair (kpds5, kpds6) 
c   last appearing in the direct variable sequence from the table file
c
c   Author: Binbin Zhou
c           Aug. 4, 2005
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	function index_table(k5,k6,n5,n6, n)
          integer k5(*), k6(*), n5, n6, n
          index_table = 0
          do i = 1, n
           if(k5(i).eq.n5.and. k6(i).eq.n6 ) then
             index_table = i
             return
           end if
          end do
         return
         end
       

          function index_table_var(var,k5,k6,n5,n6,name,n)
          integer k5(*), k6(*), n5, n6, n
          character*4 var(*), name

          index_table_var = 0
          do i = 1, n
           if(k5(i).eq.n5.and. k6(i).eq.n6
     +     .and. trim(var(i)).eq.trim(name) ) then
             index_table_var = i
             return
           end if
          end do
         return
         end

 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c    function index_int_array() returns the last first location
c    of element in the array x 
c
c    Author: Binbin Zhou
c           Aug. 4, 2005
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
   
        function index_int_array(x, element, n)
          integer x(*), element, n
          index_int_array = 0
          do i = 1, n
            if(x(i).eq.element) then
              index_int_array = i
              return
            end if
          end do
         return
         end
          

          

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c    subroutine readtbl() is to read table file in which variables and derived 
c       variables are defined
c    Original Author: Binbin Zhou
c
c    05-08-02  B. Zhou original code was built    
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

	subroutine readtbl(nunit) 

        INCLUDE 'parm.inc'
 
        Integer numvar, nsubstr, nm, ier, lng, getlvl
        Character*200 oneline
        Character*20  substr(50)

c    for variables
        Character*4 vname(maxvar)
        Integer k5(maxvar), k6(maxvar), k4(maxvar)
        Character*1 Msignal(maxvar), Psignal(maxvar)
        Character*3 Mn(maxvar), Pn(maxvar), Tn(maxvar)
        Integer Mlvl(maxvar), MeanLevel(maxvar,maxmlvl)
        Integer Plvl(maxvar), ProbLevel(maxvar,maxplvl)
        Character*1 op(maxvar)
        Integer Tlvl(maxvar)
        Character*20 Cthrs
        Real      Thrs(maxvar,maxtlvl)

c    for derived variables
        Character*4 dvname(maxvar)
        Integer dk5(maxvar), dk6(maxvar),dk4(maxvar)
        Character*1 dMsignal(maxvar), dPsignal(maxvar)
        Character*3 dMn(maxvar), dPn(maxvar), dTn(maxvar)
        Integer dMlvl(maxvar), dMeanLevel(maxvar,maxmlvl)
        Integer dPlvl(maxvar), dProbLevel(maxvar,maxplvl)
        Character*1 dop(maxvar)
        Integer dTlvl(maxvar)
        Real    dThrs(maxvar,maxtlvl)
        Integer MPairLevel(maxvar,maxmlvl,2)
        Integer PPairLevel(maxvar,maxplvl,2)

c   for max,min,10,25,50,90% mean products
        Character*4 qvname(maxvar)
        Integer qk5(maxvar), qk6(maxvar),qk4(maxvar)
        Character*1 qMsignal(maxvar)
        Character*3 qMn(maxvar)
        Integer qMlvl(maxvar), qMeanLevel(maxvar,maxmlvl)

        Integer missvar(maxvar,30)         !dynamically build a missing array for direct variable missing in each member

        common /tbl/numvar,
     +              vname,k4,k5,k6,Mlvl,Plvl,Tlvl,
     +              MeanLevel,ProbLevel,Thrs,
     +              Msignal,Psignal,op 

        common /dtbl/nderiv,
     +              dvname,dk4,dk5,dk6,dMlvl,dPlvl,dTlvl,
     +              dMeanLevel,dProbLevel,dThrs,
     +              dMsignal,dPsignal,MPairLevel,PPairLevel,dop


        common /qtbl/nmxp,
     +              qvname,qk4,qk5,qk6,qMlvl,
     +              qMeanLevel,qMsignal


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       First, read direct variable information
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        read(nunit,*) numvar  
        n1 = 1 
        n2 = numvar

c        write(*,*) 'numvar=',numvar

        do 1000 n = n1, n2       

         read (nunit, '(A)') oneline
c         write(*,*) oneline
         call ST_CLST( oneline, ' ', ' ', 50, substr, nsubstr, ier ) !note: if too many levels, need to increase 50
czeus         call ST_RMBL( substr(1), vname(n), lng, ier)
         lng=len_trim(substr(1))
         vname(n)=substr(1)(1:lng)

         call ST_NUMB( substr(2), k4(n), ier)
         call ST_NUMB( substr(3), k5(n), ier)
         call ST_NUMB( substr(4), k6(n), ier)

czeus         call ST_RMBL( substr(4), Mn(n), lng, ier)
         lng=len_trim(substr(5))
         Mn(n)=substr(5)(1:lng)

         Msignal(n)=Mn(n)(1:1)
         Mlvl(n) = getlvl(Mn(n))

         if ( Mlvl(n) .gt. 0 ) then         !Mean level > 0 
           do i = 1, Mlvl(n)
             call ST_NUMB(substr(5+i), MeanLevel(n,i), ier)
           end do
         end if

         nm = 5 + Mlvl(n)

c         write(*,*) vname(n), k4(n),k5(n),k6(n),Mn(n),Msignal(n),
c     +       Mlvl(n),(MeanLevel(n,i),i=1,Mlvl(n))

         if ( nsubstr .gt. nm) then      !there are Prob requests    
  
czeus           call ST_RMBL(substr(nm+1), Pn(n), lng, ier)
           lng=len_trim(substr(nm+1))
           Pn(n)=substr(nm+1)(1:lng)

           Psignal(n)=Pn(n)(1:1)
           Plvl(n) = getlvl(Pn(n))
          
           do i = 1, Plvl(n)
             call ST_NUMB(substr(nm+1+i), ProbLevel(n,i), ier)
           end do

           np=nm+1+Plvl(n)         

           OP(n)=substr(np+1)

czeus           call ST_RMBL(substr(np+2), Tn(n), lng, ier)
           lng=len_trim(substr(np+2))
           Tn(n)=substr(np+2)(1:lng)

           Tlvl(n) = getlvl(Tn(n))

           do i = 1, Tlvl(n)
czeus             call ST_RMBL(substr(np+2+i), Cthrs, lng, ier)
             lng=len_trim(substr(np+2+i))
             Cthrs=substr(np+2+i)(1:lng)

             Thrs(n,i)=CharToReal(Cthrs) 
           end do

c         write(*,*) Pn(n),Psignal(n),Plvl(n),
c     +    (ProbLevel(n,i),i=1,Plvl(n)),
c     +    OP(n),Tn(n),Tlvl(n),(Thrs(n,i),i=1,Tlvl(n))

         end if

1000     continue

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c          Then, read derived variable information
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

         read(nunit,*,END=3000) nderiv

c         n1 = numvar + 1
c         n2 = numvar + nderiv
c         goto 2000
 
          n1 = 1
	  n2 = nderiv
          
       do 1100 n = n1, n2

         read (nunit, '(A)') oneline

         call ST_CLST( oneline, ' ', ' ', 50, substr, nsubstr, ier )
czeus         call ST_RMBL( substr(1), dvname(n), lng, ier)
         lng=len_trim(substr(1))
         dvname(n)=substr(1)(1:lng)

         call ST_NUMB( substr(2), dk4(n), ier)
         call ST_NUMB( substr(3), dk5(n), ier)
         call ST_NUMB( substr(4), dk6(n), ier)
                                                                                                                                                                                                             
czeus         call ST_RMBL( substr(4), dMn(n), lng, ier)
         lng=len_trim(substr(5))
         dMn(n)=substr(5)(1:lng)

         dMsignal(n)=dMn(n)(1:1)
         dMlvl(n) = getlvl(dMn(n))
                                                                                                                                                                                                             
         if ( dMlvl(n) .gt. 0 ) then         !Mean level > 0
           do i = 1, dMlvl(n)
             call ST_NUMB(substr(5+i), dMeanLevel(n,i), ier)
           end do
         end if
              
          
         if( dk6(n).eq.101) then      !Thickness, then need to read additional
c          write(*,*) 'substr=',(substr(i),i=1,nsubstr)
c          write(*,*) 'nsubstr=',nsubstr, dMlvl(n)

          do i = 1, dMlvl(n)
            call ST_NUMB(substr(5+dMlvl(n)+i*2-1),MPairLevel(n,i,1),ier)
            call ST_NUMB(substr(5+dMlvl(n)+i*2),  MPairLevel(n,i,2),ier)
c            write(*,*)'MPairLevel=',MPairLevel(n,i,1),MPairLevel(n,i,2)
          end do


          nm = 5 + 3*dMlvl(n)  
 
         else
                                                                                                                             
          nm = 5 + dMlvl(n)
 
         end if     
                                                                                                                                                                                                   
         if ( nsubstr .gt. nm) then      !there are Prob requests
                                                                                                                                                                                                             
czeus           call ST_RMBL(substr(nm+1), dPn(n), lng, ier)
           lng=len_trim(substr(nm+1))
           dPn(n)=substr(nm+1)(1:lng)

           dPsignal(n)=dPn(n)(1:1)
           dPlvl(n) = getlvl(dPn(n))
                                                                                                                                                                                                             
           do i = 1, dPlvl(n)
             call ST_NUMB(substr(nm+1+i), dProbLevel(n,i), ier)
           end do
 
          if(dk6(n).eq.101 .or.dk6(n).eq.104 .or.dk6(n).eq.106   !then need to read additional
     +     .or.dk6(n).eq.112 .or.dk6(n).eq.114 .or.            !level pairs
     +         dk6(n).eq.121. or.dk6(n).eq.128 .or.
     +         dk6(n).eq.141. or.dk6(n).eq.116) then

            do i = 1, dPlvl(n)
             call ST_NUMB(substr(nm+1+dPlvl(n)+i*2-1),
     +                              PPairLevel(n,i,1),ier)
             call ST_NUMB(substr(nm+1+dPlvl(n)+i*2),  
     +                              PPairLevel(n,i,2),ier)
            end do
            np = nm + 1 + 3*dPlvl(n)
          else
            np=nm + 1 + dPlvl(n)
          end if 

           dOP(n)=substr(np+1)

czeus           call ST_RMBL(substr(np+2), dTn(n), lng, ier)
           lng=len_trim(substr(np+2))
           dTn(n)=substr(np+2)(1:lng)

           dTlvl(n) = getlvl(dTn(n))
                                                                                                                                                                                                             
           do i = 1, dTlvl(n)
czeus             call ST_RMBL(substr(np+2+i), Cthrs, lng, ier)
             lng=len_trim(substr(np+2+i))
             Cthrs=substr(np+2+i)(1:lng)

             dThrs(n,i)=CharToReal(Cthrs) 
c             write(*,*) "Cthrs, dThrs(n,i)=",Cthrs, dThrs(n,i)
           end do
               
         end if
 
1100    continue  

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   Then read max,max,10,25,50 and 50% mean  products request (MXP)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        read(nunit,*,END=3000) nmxp

        n1 = 1
        n2 = nmxp

        do 1200 n = n1, n2

         read (nunit, '(A)') oneline

         call ST_CLST( oneline, ' ', ' ', 50, substr, nsubstr, ier ) !note: if too many levels, need to increase 50
CWCOSS         call ST_RMBL( substr(1), qvname(n), lng, ier)
         lng=len_trim(substr(1))
         qvname(n)=substr(1)(1:lng)

         call ST_NUMB( substr(2), qk4(n), ier)
         call ST_NUMB( substr(3), qk5(n), ier)
         call ST_NUMB( substr(4), qk6(n), ier)

CWCOSS         call ST_RMBL( substr(5), qMn(n), lng, ier)
         lng=len_trim(substr(5))
         qMn(n)=substr(5)(1:lng)

         qMsignal(n)=qMn(n)(1:1)
         qMlvl(n) = getlvl(qMn(n))

         if ( qMlvl(n) .gt. 0 ) then         ! qMean level > 0
           do i = 1, qMlvl(n)
             call ST_NUMB(substr(5+i), qMeanLevel(n,i), ier)
           end do
         end if

1200    continue


  
3000    return
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
            if (x(i).ge.thrs1.and.x(i).lt.thrs2) 
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
 
c         if(igrid.eq.737601) write(*,*)'half=',half,wsum
  
         do 100 i=1,n
           if(miss(i).eq.0) then
            if(x(i).ge.alarge) goto 100
            mean = mean + x(i)*(wgt(i)/wsum)
c            if(igrid.eq.737601) write(*,*)i,'mean=',mean
           end if
100      continue
 
         if( count .le. half ) then           !only most of member happen

           mean = alarge
         end if

c         if(igrid.eq.737601) write(*,*)'mean=',mean


         spread = 0.
         do 200 i = 1, n 
          if(x(i).ge.alarge.or.miss(i).eq.1) goto 200                       !!! modified in Apr. 9 2008
           spread = spread + (wgt(i)/wsum)*(x(i)-mean)**2
c           if(igrid.eq.737601) write(*,*)i,'spread=',spread
200      continue

         if (mean.eq.alarge) then
             spread=0.0              !!! for VSREF, assume this. Apr. 22, 2009
         else
             spread = sqrt (spread )
c             if(igrid.eq.737601) write(*,*)'spread=',spread
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

 
