cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc  getlvl function is to get the number 'nn' from 'Mnn/Pnn/Dnn/Hnn'
c   Author: Binbin Zhou
c   02/05/2009: enhanced the way of calculating ens mean of cloud ceiling
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
c   function index_table() returns the numvar index of pair (kpds5, kpds6)'s 
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
        Integer k5(maxvar), k6(maxvar)
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
        Integer dk5(maxvar), dk6(maxvar)
        Character*1 dMsignal(maxvar), dPsignal(maxvar)
        Character*3 dMn(maxvar), dPn(maxvar), dTn(maxvar)
        Integer dMlvl(maxvar), dMeanLevel(maxvar,maxmlvl)
        Integer dPlvl(maxvar), dProbLevel(maxvar,maxplvl)
        Character*1 dop(maxvar)
        Integer dTlvl(maxvar)
        Real    dThrs(maxvar,maxtlvl)
        Integer MPairLevel(maxvar,maxmlvl,2)
        Integer PPairLevel(maxvar,maxplvl,2)


        common /tbl/numvar,
     +              vname,k5,k6,Mlvl,Plvl,Tlvl,
     +              MeanLevel,ProbLevel,Thrs,
     +              Msignal,Psignal,op

        common /dtbl/nderiv,
     +              dvname,dk5,dk6,dMlvl,dPlvl,dTlvl,
     +              dMeanLevel,dProbLevel,dThrs,
     +              dMsignal,dPsignal,MPairLevel,PPairLevel,dop



cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       First, read direct variable information
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        read(nunit,*) numvar  
        n1 = 1 
        n2 = numvar

        do 1000 n = n1, n2       

         read (nunit, '(A)') oneline
     
         call ST_CLST( oneline, ' ', ' ', 50, substr, nsubstr, ier ) !note: if too many levels, need to increase 50
         call ST_RMBL( substr(1), vname(n), lng, ier)
         call ST_NUMB( substr(2), k5(n), ier)
         call ST_NUMB( substr(3), k6(n), ier)

         call ST_RMBL( substr(4), Mn(n), lng, ier)
         Msignal(n)=Mn(n)(1:1)
         Mlvl(n) = getlvl(Mn(n))

         if ( Mlvl(n) .gt. 0 ) then         !Mean level > 0 
           do i = 1, Mlvl(n)
             call ST_NUMB(substr(4+i), MeanLevel(n,i), ier)
           end do
         end if

         nm = 4 + Mlvl(n)

         if ( nsubstr .gt. nm) then      !there are Prob requests    
  
           call ST_RMBL(substr(nm+1), Pn(n), lng, ier)
           Psignal(n)=Pn(n)(1:1)
           Plvl(n) = getlvl(Pn(n))
          
           do i = 1, Plvl(n)
             call ST_NUMB(substr(nm+1+i), ProbLevel(n,i), ier)
           end do

           np=nm+1+Plvl(n)         

           OP(n)=substr(np+1)

           call ST_RMBL(substr(np+2), Tn(n), lng, ier)
           Tlvl(n) = getlvl(Tn(n))

           do i = 1, Tlvl(n)
             call ST_RMBL(substr(np+2+i), Cthrs, lng, ier)
             Thrs(n,i)=CharToReal(Cthrs) 
           end do

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
         call ST_RMBL( substr(1), dvname(n), lng, ier)
         call ST_NUMB( substr(2), dk5(n), ier)
         call ST_NUMB( substr(3), dk6(n), ier)
                                                                                                                                                                                                             
         call ST_RMBL( substr(4), dMn(n), lng, ier)
         dMsignal(n)=dMn(n)(1:1)
         dMlvl(n) = getlvl(dMn(n))
                                                                                                                                                                                                             
         if ( dMlvl(n) .gt. 0 ) then         !Mean level > 0
           do i = 1, dMlvl(n)
             call ST_NUMB(substr(4+i), dMeanLevel(n,i), ier)
           end do
         end if
              
          
         if( dk6(n).eq.101.or.dk6(n).eq.104.or.dk6(n).eq.106   !then need to read additional
     +     .or.dk6(n).eq.112 .or.dk6(n).eq.114 .or.           !level pairs
     +         dk6(n).eq.121. or.dk6(n).eq.128 .or.   
     +         dk6(n).eq.141. or.dk6(n).eq.116) then
     
          do i = 1, dMlvl(n)
            call ST_NUMB(substr(4+dMlvl(n)+i*2-1),MPairLevel(n,i,1),ier)
            call ST_NUMB(substr(4+dMlvl(n)+i*2),  MPairLevel(n,i,2),ier)
          end do
          
          nm = 4 + 3*dMlvl(n)  
 
         else
                                                                                                                             
          nm = 4 + dMlvl(n)
 
         end if     
                                                                                                                                                                                                   
         if ( nsubstr .gt. nm) then      !there are Prob requests
                                                                                                                                                                                                             
           call ST_RMBL(substr(nm+1), dPn(n), lng, ier)
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

           call ST_RMBL(substr(np+2), dTn(n), lng, ier)
           dTlvl(n) = getlvl(dTn(n))
                                                                                                                                                                                                             
           do i = 1, dTlvl(n)
             call ST_RMBL(substr(np+2+i), Cthrs, lng, ier)
             dThrs(n,i)=CharToReal(Cthrs) 
             write(*,*) "Cthrs, dThrs(n,i)=",Cthrs, dThrs(n,i)
           end do
               
         end if
 
1100    continue  
  
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

	subroutine getprob(x,n,thrs1, thrs2, opr, prob)
	real x(*), thrs1, thrs2, prob
        integer n, count, njff
        character*1 opr

c        do i = 1, n
c         x(i) = abs(x(i)
c        end do
c        thrs1 = abs(thrs1)
c        thrs2 = abs(thrs2)

        count = 0
        njff = 0
        do 100 i = 1, n
         if (x(i) .le. -990.0) goto 100
          njff = njff + 1
         if (trim(opr).eq.'>') then
           if (x(i).ge.thrs1) count = count + 1
         else if (trim(opr).eq.'<') then
           if (x(i).le.thrs1) count = count + 1
         else if (trim(opr).eq.'=') then
           if (x(i).eq.thrs1) count = count + 1
         else if (trim(opr).eq.'-') then
           if (x(i).ge.thrs1.and.x(i).lt.thrs2) 
     +       count = count + 1
         end if
100     continue

        if (njff .gt. 0 ) then
          prob = 100.0 * count / njff
        else
          prob = 0.
        end if

        return
        end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   subroutine getmean: compute mean and spread of one dimension array  
c   Author: Binbin Zhou
c   Aug. 3, 2005
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

	subroutine getmean (x, n,  mean, spread)
	 real x(*), mean, spread
         integer n, njff
       
         mean = 0.
         njff = 0
         do 100 i=1,n
          if (x(i) .le. -990.0) goto 100
           njff = njff +1
           mean = mean + x(i)
100      continue 

         if (njff .gt.0 ) then
           mean = mean / njff
         end if

         spread = 0.
         njff = 0
         do 200 i = 1, n
          if (x(i) .le. -990.0) goto 200
          njff = njff +1
          spread = spread + (x(i)-mean)**2
200      continue

         if (njff .gt. 1 ) then
          spread = sqrt (spread / ( njff - 1) )
         else
          spread = 0.
         end if

         return
         end
                          
       
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   subroutine get_cond_mean: compute conditional mean and spread of one dimension array
c      Conditional mean is the mean under some condition, if equal to certain
c      very large value, then it is not taken into mean, but spread still
c      count it
c
c   Author: Binbin Zhou
c   Aug. 3, 2005
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C New: calculate ens mean if more than half members having cloud. Otherwise, no ceiling in ens mean 
        subroutine get_cond_mean (x, n, alarge, mean, spread)
         real x(*), mean, spread, alarge,count, half
         integer n, njff

         mean = 0.
         count = 0.
         half=n/2.0

         njff=0
         do 100 i=1,n
           if (x(i) .le. -990.0) goto 100
           if(x(i).ge.alarge) goto 100
            mean = mean + x(i)
            count = count + 1.0
100      continue

         if( count .gt. half ) then           !only most of member happen
           mean = mean / count
         else
           mean = alarge
         end if

         spread = 0.
         count = 0.
         do 200 i = 1, n
          if (x(i) .le. -990.0) goto 200
          if(x(i).ge.alarge) goto 200           !!! modified in Apr. 9 2008
           spread = spread + (x(i)-mean)**2
           count = count + 1.0
200      continue

         if (count .gt. 1 ) then
          spread = sqrt (spread / ( count-1) )
         else
          spread = 0.
         end if

         return
         end
                                                                 
C Old: calculate ens mean based on any members having cloud even "only one single member has cloud and all
C the rest have no cloud" situation
        subroutine get_cond_mean_old (x, n, alarge, mean, spread)
         real x(*), mean, spread, alarge,count
         integer n
                                                                                                     
         mean = 0.
         count = 0.

         do 100 i=1,n
           if(x(i).ge.alarge) goto 100 
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
         do 200 i = 1, n                         
          if(x(i).ge.alarge) goto 200           !!! modified in Apr. 9 2008 
           spread = spread + (x(i)-mean)**2
           count = count + 1.0
200      continue
            
         if (count .gt. 1 ) then                                                                                         
          spread = sqrt (spread / ( count-1) )
         else
          spread = 0.
         end if
       
         return
         end


        subroutine get_cond_mean_lwc(x,n,alarge,mean,spread)
         real x(*), mean, spread, alarge,count
         integer n,njff
                                                                                                                                                                                       
         mean = 0.
         count = 0.
         njff = 0                                                                                                                                                                              
         do 100 i=1,n
           if (x(i) .le. -990.0) goto 100
           if(x(i).eq.alarge) goto 100
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
         do 200 i = 1, n
           if (x(i) .le. -990.0) goto 200
           if(x(i).gt.alarge) then
            count = count + 1.0
            spread = spread + (x(i)-mean)**2
           end if
200      continue 
          
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
                                                                                                                                          
        subroutine getwindmean (u,v,n, mean, spread)
         real u(*),v(*), Umean, Vmean, Uspread,Vspread,
     +             mean, spread         
         integer n,njff
                                                                                                                                          
         Umean = 0.
         Vmean = 0.
         mean = 0.
         njff = 0
         do 100 i=1,n
          if (u(i).lt. -990.0 .or. 
     +        v(i).lt. -990.0) goto 100
           njff = njff + 1
           Umean = Umean + u(i)
           Vmean = Vmean + v(i)
           mean = mean + sqrt(v(i)*v(i)+u(i)*u(i))
100     continue 
                
        if (njff. gt. 0) then                                                                                                                         
         Umean = Umean / njff
         Vmean = Vmean / njff
         mean = mean / njff
        end if  
     
         Uspread = 0.
         Vspread = 0.
         spread = 0.

         do 200 i = 1, n
          if (u(i).lt. -990.0 .or.
     +        v(i).lt. -990.0) goto 200
          njff = njff + 1
           Uspread = Uspread + (u(i)-Umean)**2
           Vspread = Vspread + (v(i)-Vmean)**2
200      continue 
                  
        if (njff .gt. 1) then                                                                                                                        
         Uspread = sqrt (Uspread / (n-1))
         Vspread = sqrt (Vspread / (n-1))
         spread = sqrt(Uspread*Uspread + Vspread*Vspread)         
        else
         Uspread = 0.
         Vspread = 0.
         spread = 0.
        end if

         return
         end

C**********************************************************************
      SUBROUTINE RDGB(LUGB,LGRIB,LSKIP,KPDS,KGDS,NDATA,LBMS,DATA)
C
C  READ GRIB FILE
C  INPUT
C    LUGB - LOGICAL UNIT TO READ
C    LGRIB - LENGTH OF GRIB RECORD
C    LSKIP - BYTES TO SKIP FOR GRIB RECORD
C  OUTPUT
C    KPDS(22) - UNPACKED PRODUCT DEFINITION SECTION
C    KGDS(20) - UNPACKED GRID DEFINITION SECTION
C    NDATA    - NUMBER OF DATA POINTS
C    LBMS(NDATA) - LOGICAL BIT MAP
C    DATA(NDATA) - DATA UNPACKED
C
      CHARACTER GRIB(LGRIB)*1
      INTEGER KPDS(25),KGDS(22),KPTR(20)
      LOGICAL LBMS(*)
      REAL DATA(*)
      NDATA=0
      CALL BAREAD(LUGB,LSKIP,LGRIB,LREAD,GRIB)
      IF(LREAD.LT.LGRIB) RETURN
      CALL W3FI63(GRIB,KPDS,KGDS,LBMS,DATA,KPTR,IRET)
      IF(IRET.NE.0) RETURN
      NDATA=KPTR(10)
c     print*,'ndata= ',ndata
      RETURN
      END

C********************************************
C********************************************
      SUBROUTINE SKGB(LUGB,ISEEK,LGRIB,LSKIP)
C
C  SEEK FOR NEXT GRIB1 RECORD WITHIN THE NEXT LSEEK=4096 BYTES
C  INPUT
C    LUGB  - LOGICAL UNIT TO READ
C    ISEEK - BYTES TO SKIP BEFORE SEARCH (SET TO 0 AT START)
C  OUTPUT
C    ISEEK - NUMBER OF BYTES READ SO FAR
C    LGRIB - LENGTH OF GRIB RECORD (0 IF NOT FOUND)
C    LSKIP - BYTES TO SKIP FOR GRIB RECORD
C
      PARAMETER(LSEEK=4096)
      CHARACTER C*(LSEEK)
      CALL BAREAD(LUGB,ISEEK,LSEEK,LREAD,C)
      DO I=0,LREAD-8
        IF(C(I+1:I+4).EQ.'GRIB'.AND.MOVA2I(C(I+8:I+8)).EQ.1) THEN
          LGRIB=MOVA2I(C(I+5:I+5))*65536
     &         +MOVA2I(C(I+6:I+6))*256
     &         +MOVA2I(C(I+7:I+7))
          LSKIP=ISEEK+I
          ISEEK=LSKIP+LGRIB
          RETURN
        ENDIF
      ENDDO
      LGRIB=0
      LSKIP=0
      ISEEK=ISEEK+LSEEK
c      CALL W3TAGE('SREF_COM_GRIB')
      RETURN
         END
 
