ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  Subroutine preciptype: Compute precipitation type rain/freezing rain/snow
c  based on Jun Du's old version
c
c  Author: Binbin Zhou, Aug. 4, 2005 
c  Modification history:
c   01/12/2006: Geoff Manikin: Add Geoff Manikin's algorithm to determine dominant precip type 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
   	subroutine  preciptype(nv,rawdata, jf, iens,  
     +             derv_mn, derv_pr, ptype_mn, ptype_pr)

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
        REAL,dimension(jf,nderiv,maxplvl,maxtlvl),intent(INOUT) :: 
     +               derv_pr

        REAL,dimension(jf,maxmlvl,4),intent(INOUT) :: ptype_mn
        REAL,dimension(jf,maxmlvl,4),intent(INOUT) :: ptype_pr

          ID_RAIN = index_table(k5,k6,140,1,maxvar)      !search index of direct variable in the table
          ID_FRZR = index_table(k5,k6,141,1,maxvar)      !search index of direct variable in the table
          ID_ICEP = index_table(k5,k6,142,1,maxvar)      !search index of direct variable in the table
          ID_SNOW = index_table(k5,k6,143,1,maxvar)      !search index of direct variable in the table

           ptype_mn = 0.
           ptype_pr = 0.

          if (ID_RAIN .gt.0  .and. ID_FRZR.gt.0 .and. 
     +        ID_ICEP .gt.0  .and. ID_SNOW.gt.0) then
              
             do lv=1,dMlvl(nv)                                  !for all levels
               do igrid = 1,jf

                 crain = 0.
                 cfrzr = 0.
                 csnow = 0.
                 cslet = 0.

                 do irun = 1,iens 
                   crain = crain + rawdata(igrid, irun, ID_RAIN, lv)
                   cfrzr = cfrzr + rawdata(igrid, irun, ID_FRZR, lv)
                   csnow = csnow + rawdata(igrid, irun, ID_SNOW, lv)
                   cslet = cslet + rawdata(igrid, irun, ID_ICEP, lv)
                 end do  

cc  following is part is the code copy from Geoff Manikin's doninant precip type decision
cc  importance priority order:  freezing_rain(1) > snow(2) > sleet(3) > rain(4)            
            if(crain.ge.1.0.or.cfrzr.ge.1.0.or.csnow.ge.1.0.or.
     +         cslet.ge.1.0) then
              if(csnow.ge.cslet) then
                if(csnow.ge.cfrzr) then
                  if(csnow.ge.crain) then
                   ptype_mn(igrid,lv,2)=1.      !snow
                   goto 800
                  else
                   ptype_mn(igrid,lv,4)=1.      !rain
                   goto 800
                  end if
                else if(cfrzr.ge.crain) then
                   ptype_mn(igrid,lv,1)=1.      !freezing rain
                   goto 800
                else
                   ptype_mn(igrid,lv,4)=1.      !rain
                   goto 800
                end if
               else if(cslet.gt.cfrzr) then
                if(cslet.ge.crain) then
                   ptype_mn(igrid,lv,3)=1.      !sleet
                   goto 800
                else
                   ptype_mn(igrid,lv,4)=1.      !rain
                   goto 800
                end if
               else if(cfrzr.ge.crain) then
                   ptype_mn(igrid,lv,1)=1.      !freezing rain
                   goto 800
               else
                   ptype_mn(igrid,lv,4)=1.      !rain
                   goto 800
               end if

800           continue    
             end if

             if(igrid.eq.15000) then
              write(*,*)'IN preciptype ####################'
              write(*,*)cfrzr,csnow,cslet,crain,
     +        (ptype_mn(igrid,lv,n),n=1,4)
             end if

            end do
           end do

             do lv=1,dPlvl(nv)
              do lt =1,dTlvl(nv)
                do igrid = 1, jf
   
                 crain = 0.
                 cfrzr = 0.
                 csnow = 0.
                 cslet = 0.

                 if(dThrs(nv,lt).eq.1.0) then

                  do irun=1,iens
                   crain = crain + rawdata(igrid, irun, ID_RAIN, lv)
                   cfrzr = cfrzr + rawdata(igrid, irun, ID_FRZR, lv)
                   csnow = csnow + rawdata(igrid, irun, ID_SNOW, lv)
                   cslet = cslet + rawdata(igrid, irun, ID_ICEP, lv)
                  end do
                  ptype_pr(igrid,lv,1)=cfrzr*100./iens        
                  ptype_pr(igrid,lv,2)=csnow*100./iens
                  ptype_pr(igrid,lv,3)=cslet*100./iens      
                  ptype_pr(igrid,lv,4)=crain*100./iens
                 end if

                end do
              end do
             end do

           end if

           return
           end
