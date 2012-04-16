cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     subroutine thickness: compute distance between 2 pressure levels
c     first search for index of HGT from the table -> IDV, if found:
c     then search level pair (MPairlevel) index from MeanLevel-> L1, L2, if found:
c     then compute the difference between the two levels
c     
c     Author: Binbin Zhou, Aug, 5, 2005
c      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
   	subroutine thickness (nv,rawdata, jf, iens,  
     +             derv_mn, derv_sp, derv_pr)

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


        INTEGER, intent(IN) :: nv, jf, iens
        REAL,dimension(jf,iens,numvar,maxmlvl),intent(IN) :: rawdata
        REAL,dimension(jf,nderiv,maxmlvl),intent(INOUT) :: derv_mn
        REAL,dimension(jf,nderiv,maxmlvl),intent(INOUT) :: derv_sp
        REAL,dimension(jf,nderiv,maxplvl,maxtlvl),intent(INOUT) :: 
     +               derv_pr

        real apoint(iens)

          IDV = index_table(k5,k6,7,100,maxvar)      !search index of direct variable in the table

          if (IDV .gt.0 ) then

             do lv=1,dMlvl(nv)                       !for all pairs of levels

                L1=index_int_array(MeanLevel(IDV,:),            !search index of 1st level from  
     +                            MPairlevel(nv,lv,1),maxmlvl)  !   IDV-th variable's MeanLevel 
                if (L1.eq.0) then
                  write(*,*) MPairlevel(nv,lv,1), 'not found'
                  STOP
                end if
                L2=index_int_array(MeanLevel(IDV,:),            !search index of 2nd level from
     +                            MPairlevel(nv,lv,2),maxmlvl)  !   IDV-th variable's MeanLevel
                if (L2.eq.0) then
                  write(*,*) MPairlevel(nv,lv,2), 'not found'
                  STOP
                end if

                do igrid = 1,jf
                  apoint = abs( rawdata(igrid,:,IDV,L1) -
     +                          rawdata(igrid,:,IDV,L2) )
                  call getmean(apoint,iens,amean,aspread)
                  derv_mn(igrid,nv,lv)=amean
                  derv_sp(igrid,nv,lv)=aspread
                end do

              end do

              do lv=1,dPlvl(nv)
                do lt = 1, dTlvl(nv)

                 L1=index_int_array(MeanLevel(IDV,:),          !search index of 1st level from
     +                            PPairlevel(nv,lv,1),maxmlvl) ! IDV-th variable's MeanLevel
                 if (L1.eq.0) STOP 111                         ! note: also from mean's rawdata
                 L2=index_int_array(MeanLevel(IDV,:),          ! samething for 2nd level
     +                            PPairlevel(nv,lv,2),maxmlvl)
                 if (L2.eq.0) STOP 112
                              
                 do igrid = 1,jf
                  apoint = abs( rawdata(igrid,:,IDV,L1) -
     +                          rawdata(igrid,:,IDV,L2) )

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
              end do

           end if

           return
           end
