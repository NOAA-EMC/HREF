cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  Subroutine apcp: compute 3/6/12/24 hour accumulated precipitation
c  mean/sprea/probability based on Jun Du's old version
c  Note: The mean is "unditional mean", ie. 0 precipitation is also
c        taken into account in mean
c
c  Author: Binbin Zhou, Agu. 8, 2005
c
c  Input: nv, itime, i00, precip, jf, iens, interval
c  output: derv_mn,  derv_pr
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
   	subroutine apcp(nv,itime,i00,precip,jf,iens,interval,
     +   loutput, derv_mn,derv_sp,derv_pr,prcpmax,prcpmin) 

         include 'parm.inc'

C for variable table:
        Integer numvar, nderiv
        Character*4 vname(maxvar)
        Integer k5(maxvar), k6(maxvar)
        Character*1 Msignal(maxvar), Psignal(maxvar)
        Integer Mlvl(maxvar), MeanLevel(maxvar,maxmlvl)
        Integer Plvl(maxvar), ProbLevel(maxvar,maxplvl)
        Integer Tlvl(maxvar)
        Character*1 op(maxvar)
        Real    Thrs(maxvar,maxtlvl)
                                                                                                                                                                
c    for derived variables
        Character*4 dvname(maxvar)
        Integer dk5(maxvar), dk6(maxvar)
        Character*1 dMsignal(maxvar), dPsignal(maxvar)
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


        INTEGER, intent(IN) :: nv,itime,i00,jf,iens,interval,loutput
        REAL,dimension(jf,iens,loutput),intent(IN) :: precip
        REAL,dimension(jf,nderiv,maxmlvl),intent(INOUT) :: derv_mn
        REAL,dimension(jf,nderiv,maxmlvl),intent(INOUT) :: derv_sp
        REAL,dimension(jf,nderiv,maxplvl,maxtlvl),intent(INOUT) :: 
     +               derv_pr
        REAL,dimension(jf,maxmlvl),intent(INOUT)        :: prcpmax
        REAL,dimension(jf,maxmlvl),intent(INOUT)        :: prcpmin
       

        INTEGER               :: tick ! counter for counting previous times           
        REAL,dimension(iens)  :: apoint
        REAL                  :: amean,aspread,aprob
          
	write(0,*) 'precip dimensions inside apcp: ', jf, iens, loutput

          ID_PRCP = index_table(k5,k6,dk5(nv),1,maxvar)       !search index of direct variable in the table

          if (ID_PRCP .gt.0 ) then 

             do lv=1,dMlvl(nv)                                  !for accumulated hours
               write(0,*) 'itime, dmeanlevel: ', 
     &              itime, dMeanLevel(nv,lv)      

               if(itime.ge.dMeanLevel(nv,lv)) then

                 do igrid = 1,jf
                   apoint = 0.
                   tick = dMeanLevel(nv,lv)/interval - 1
                   do while ( tick .ge. 0 )
	if (igrid .eq. jf/2) then
	write(0,*) 'apoint adding precip from i00, tick, i00-tick: ',
     &               i00,tick,i00-tick
        write(0,*) 'precip val: ', precip(igrid,5:10,i00-tick) 
	endif
                     apoint= apoint + precip(igrid,:,i00-tick)

c	if (maxval(precip(igrid,:,i00-tick)) .gt. 15.0) then
c 	write(0,*) 'big precip: ', igrid, precip(igrid,:,i00-tick)
c	endif

c        if (maxval(apoint) .gt. 15.0) then
c	write(0,*) 'big precip: ', igrid, precip(igrid,:,i00-tick)
c	endif

                     tick = tick - 1
                   end do
C	write(0,*) 'maxval(apoint): ', maxval(apoint)
                   call getmean(apoint,iens,amean,aspread)
                   derv_mn(igrid,nv,lv) = amean
                   derv_sp(igrid,nv,lv) = aspread
                   prcpmax(igrid,lv) = maxval(apoint)
                   prcpmin(igrid,lv) = minval(apoint)


                 end do
               end if
             end do

	write(0,*) 'max prcpmax,derv_mn: ',maxval(prcpmax),maxval(derv_mn)

	do JJ=1,iens
	write(0,*) 'JJ, maxval(precip(:,JJ,:)): ', maxval(precip(:,JJ,:))
	enddo

	

             do lv=1,dPlvl(nv)
              if(itime.ge.dProbLevel(nv,lv)) then

                do lt =1,dTlvl(nv)
                  do igrid = 1, jf
                    apoint = 0.
                    tick = dProbLevel(nv,lv)/interval - 1
                    do while ( tick .ge. 0 )
                     apoint= apoint + precip(igrid,:,i00-tick)
                     tick = tick - 1
                    end do

                    if(trim(dop(nv)).ne.'-') then
                     thr1 = dThrs(nv,lt)
                     thr2 = 0.
               call getprob(apoint,iens,thr1,thr2,dop(nv),aprob)
                     derv_pr(igrid,nv,lv,lt)=aprob
                    else
                     if(lt.lt.dTlvl(nv)) then
                       thr1 = dThrs(nv,lt)
                       thr2 = dThrs(nv,lt+1)
               call getprob(apoint,iens,thr1,thr2,dop(nv),aprob)
                       derv_pr(igrid,nv,lv,lt)=aprob
                     end if
                    end if

                  end do
                end do

              end if
             end do
           
           end if

           return
           end
