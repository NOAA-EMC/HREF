cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     subroutine getfog: compute ceiling height
c     
c     Author: Binbin Zhou, Jun 1, 2006
c     Modification: 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
   	subroutine getceiling (nv, rawdata, jf, iens,  
     +             vrbl_mn, vrbl_sp, vrbl_pr)

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
                                                                                                                                                                
        common /tbl/numvar,
     +              vname,k5,k6,Mlvl,Plvl,Tlvl,
     +              MeanLevel,ProbLevel,Thrs,
     +              Msignal,Psignal,op
                                                                                                                                                                

        INTEGER, intent(IN) :: nv, jf, iens
        REAL,dimension(jf,iens,numvar,maxmlvl),intent(IN) :: rawdata
        REAL,dimension(jf,numvar,maxmlvl),intent(INOUT) :: vrbl_mn
        REAL,dimension(jf,numvar,maxmlvl),intent(INOUT) :: vrbl_sp
        REAL,dimension(jf,numvar,maxplvl,maxtlvl),intent(INOUT) :: 
     +               vrbl_pr

        real CEILapoint(iens),TCLDapoint(iens),CLDBapoint(iens),
     +       SFCHapoint(iens)
        real ceil(jf,iens)

           ID_TCLD = index_table(k5,k6,71,200,maxvar)   !search index of total cloud in the table
           ID_CLDB = index_table(k5,k6,7,2,maxvar)      !search index of cloud base heightin the table
           ID_SFCH = index_table(k5,k6,7,1,maxvar)      !search index of surface height in the table


          if (ID_TCLD .gt.0 .and. ID_CLDB .gt.0 .and.
     +       ID_SFCH .gt.0 ) then

              do igrid = 1,jf

               TCLDapoint=rawdata(igrid,:,ID_TCLD,1)
               CLDBapoint=rawdata(igrid,:,ID_CLDB,1)
               SFCHapoint=rawdata(igrid,:,ID_SFCH,1)

               do i = 1, iens
                if(CLDBapoint(i).lt.0.0) CEILapoint(i)=20000.0
                if(TCLDapoint(i).ge.50.0 .and.
     +             CLDBapoint(i).gt.0.0  ) then
                   CEILapoint(i) = CLDBapoint(i) - SFCHapoint(i)
                   if(CEILapoint(i).lt.0.0) CEILapoint(i)=0.0
                else
                   CEILapoint(i)=20000.0
                end if
               end do               
 
               ceil(igrid,:)=CEILapoint(:)
                     
               call get_cond_mean (CEILapoint, iens, 20000.0, 
     +              amean, aspread)

               vrbl_mn(igrid,nv,1)=amean
               vrbl_sp(igrid,nv,1)=aspread
              end do

           do 30 lv=1,Plvl(nv)

             do lt = 1, Tlvl(nv)
                              
              do igrid = 1,jf
               CEILapoint=ceil(igrid,:)

               if(trim(op(nv)).ne.'-') then
                 thr1 = Thrs(nv,lt)
                 thr2 = 0.
                 call getprob(CEILapoint,iens,
     +                thr1,thr2,op(nv),aprob)
                 vrbl_pr(igrid,nv,lv,lt)=aprob
                else
                  if(lt.lt.Tlvl(nv)) then
                    thr1 = Thrs(nv,lt)
                    thr2 = Thrs(nv,lt+1)
                    call getprob(CEILapoint,iens,
     +                   thr1,thr2,op(nv),aprob)
                    vrbl_pr(igrid,nv,lv,lt)=aprob
                   end if
                end if
              end do
             end do

30          continue  

        end if

        return
        end
