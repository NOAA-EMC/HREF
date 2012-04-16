cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     subroutine wind: compute wind speed at 10m or high pressure levels
c     first search for index of U, V from the table -> ID_U, ID_V, if found:
c     then search level index from MeanLevel-> L, if found:
c     then compute the wind  W = SQRT(U*U+V*V)
c     
c     Author: Binbin Zhou, Aug, 5, 2005
c     Modification: 
c      03/01/2006: Binbin ZHou: modify wind speed spread computation method
c                   by considering the directions of wind vectors
c
c      
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
   	subroutine wind (nv,rawdata, jf, iens,  
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

        real apoint(iens),Uapoint(iens),Vapoint(iens)

          if(dk6(nv).eq.105) then
           ID_U = index_table(k5,k6,33,105,maxvar)      !search index of direct variable in the table
           ID_V = index_table(k5,k6,34,105,maxvar)      !search index of direct variable in the table
          end if

          if(dk6(nv).eq.100) then
           ID_U = index_table(k5,k6,33,100,maxvar)      !search index of direct variable in the table
           ID_V = index_table(k5,k6,34,100,maxvar)      !search index of direct variable in the table
          end if


          if (ID_U .gt.0 .and. ID_V .gt.0 ) then

             do lv=1,dMlvl(nv)                       !for all pairs of levels

                L1=index_int_array(MeanLevel(ID_U,:),           !search index of 1st level from  
     +                    dMeanlevel(nv,lv),maxmlvl)            !   ID_U-th variable's MeanLevel 
                if (L1.eq.0) then
                 write(*,*) dMeanlevel(nv,lv), ' not found'
                 stop
                end if

                L2=index_int_array(MeanLevel(ID_V,:),           !search index of 1st level from
     +                    dMeanlevel(nv,lv),maxmlvl)            !   ID_U-th variable's MeanLevel
                if (L2.eq.0) then
                 write(*,*) dMeanlevel(nv,lv), ' not found'
                 stop
                end if
  
                do igrid = 1,jf
                  Uapoint=rawdata(igrid,:,ID_U,L1)
                  Vapoint=rawdata(igrid,:,ID_V,L2)
                  call getwindmean(Uapoint, Vapoint, iens,
     +                                 amean,aspread)
                  derv_mn(igrid,nv,lv)=amean
                  derv_sp(igrid,nv,lv)=aspread

                  if(igrid.eq.23283) then
        write(*,*) L1,L2,dMeanlevel(nv,lv),dMeanlevel(nv,lv)
        write(*,*)nv,lv, derv_mn(igrid,nv,lv), 
     +            derv_sp(igrid,nv,lv)
                  end if
                end do

              end do

              do lv=1,dPlvl(nv)

                L1=index_int_array(MeanLevel(ID_U,:),           !search index of 1st level from
     +                     dProblevel(nv,lv),maxmlvl)           !   ID_U-th variable's MeanLevel
                if (L1.eq.0) STOP 113
                                                                                                                                               
                L2=index_int_array(MeanLevel(ID_V,:),           !search index of 1st level from
     +                     dProblevel(nv,lv),maxmlvl)           !   ID_U-th variable's MeanLevel
                if (L2.eq.0) STOP 114

                do lt = 1, dTlvl(nv)
                              
                 do igrid = 1,jf
                  apoint=sqrt(rawdata(igrid,:,ID_U,L1)*
     +                        rawdata(igrid,:,ID_U,L1)
     +                      + rawdata(igrid,:,ID_V,L2)*
     +                        rawdata(igrid,:,ID_V,L2))

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
