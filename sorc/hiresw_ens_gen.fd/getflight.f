cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     subroutine flight_res: compute flight_restriction condition prob
c     
c     Author: Binbin Zhou, Oct 1, 2007
c     Modification: 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
   	subroutine flight_res (nv,rawdata,jf,iens,derv_pr)

            include 'parm.inc'

C for variable table:
        Integer numvar
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
        REAL,dimension(jf,nderiv,maxplvl,maxtlvl),intent(INOUT) :: 
     +               derv_pr

        
        real FLTapnt(iens), count, aprob
        integer ID_FLT

        ID_FLT = index_table(k5,k6,20,2,maxvar) !search flight condition in control table

        if (ID_FLT.gt.0) then      

           write(*,*) 'ID_FLT=',ID_FLT

           do igrid = 1,jf

             FLTapnt=rawdata(igrid,:,ID_FLT,1)

             if(igrid.eq.2051) then
               write(*,*)'FLTapnt=', FLTapnt
             end if


            do lv=1,dPlvl(nv)

              do lt = 1, dTlvl(nv)
 
               thr1 = dThrs(nv,lt)
                             
               if(thr1.eq.2.0) then
                 count = 0.
                 do m=1,iens
                  if(FLTapnt(m).le.2.0) then
                    count = count + 1.
                  end if
                 end do
                 aprob = 100.0*count/iens 
               else
                 count = 0.
                 do m=1,iens
                  if(FLTapnt(m).eq.thr1) then
                   count = count + 1.
                  end if
                 end do
                 aprob = 100.0*count/iens
               end if

               derv_pr(igrid,nv,lv,lt)=aprob
        
c               if(igrid.gt.2000.and.igrid.lt.2011) then
c                write(*,*) 'Thre', dop(nv),dThrs(nv,lt),':',
c     +        derv_pr(igrid,nv,lv,lt),derv_pr(igrid,1,1,1)
c               end if

              end do
             end do

           end do

        end if

        return
        end
