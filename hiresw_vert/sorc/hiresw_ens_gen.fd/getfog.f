cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
c     subroutine getfog: compute fog occurrence
c     
c     Author: Binbin Zhou, Jun 1, 2006
c     Modification: 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
   	subroutine getfog (nv,rawdata,jf,iens,derv_pr)

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

        real FOGapnt(iens),TCLDapnt(iens),CLDBapnt(iens),
     +       CLDTapnt(iens),SFCHapnt(iens),CWTRapnt(iens)
        real fog(jf,iens),aprob

        ID_TCLD = index_table(k5,k6,71,200,maxvar)   !search index of total cloud in the table
        ID_CLDB = index_table(k5,k6,7,2,maxvar)      !search index of cloud base heightin the table
        ID_CLDT = index_table(k5,k6,7,3,maxvar)      !search index of cloud cloud heightin the table
        ID_SFCH = index_table(k5,k6,7,1,maxvar)      !search index of surface height in the table
        ID_CWTR = index_table(k5,k6,153,100,maxvar)  !search index of cloud LWC in the table

        write(*,*) 'In getfog -->'
        write(*,*)'ID_TCLD,ID_CLDB,ID_CLDT,ID_SFCH ID_CWTR=',
     +             ID_TCLD,ID_CLDB,ID_CLDT,ID_SFCH,ID_CWTR


      if (ID_TCLD .gt.0 .and. ID_CLDB .gt.0 .and.
     +    ID_CLDT .gt.0 .and. ID_SFCH .gt.0 .and.
     +    ID_CWTR  .gt.0 ) then

        do igrid = 1,jf

          TCLDapnt=rawdata(igrid,:,ID_TCLD,1)
          CLDBapnt=rawdata(igrid,:,ID_CLDB,1)
          CLDTapnt=rawdata(igrid,:,ID_CLDT,1)
          SFCHapnt=rawdata(igrid,:,ID_SFCH,1)
          CWTRapnt=rawdata(igrid,:,ID_CWTR,1)

          if(igrid.eq.2051) then
            write(*,*) TCLDapnt
            write(*,*) CLDBapnt
            write(*,*) CLDTapnt
            write(*,*) SFCHapnt
          end if

          FOGapnt = 0.
          hasfog= 0.

          do i = 1, iens

            if(CLDBapnt(i).ge.0.0) Then
              CLDBapnt(i) = CLDBapnt(i) - SFCHapnt(i)
              if(CLDBapnt(i).lt.0.0)  CLDBapnt(i) = 0.0
            end if

            if(CLDTapnt(i).ge.0.0) Then
             CLDTapnt(i) = CLDTapnt(i) - SFCHapnt(i)
             if(CLDTapnt(i).le.0.0) CLDTapnt(i) = 0.0
            end if

c            if(CLDBapnt(i).ge.0.0.and.CLDBapnt(i).le.10.0) then           ! fog
c              if(CLDTapnt(i).gt.0.0.and.CLDTapnt(i).le.200.0) then
c                FOGapnt(i) = 1.                                           !radiation fog
c                hasfog=1.
c              else if(CLDTapnt(i).ge.200.0.and.
c     +                                  CLDTapnt(i).le.400.0) then   !advection fog/sea fog
c                FOGapnt(i) = 1.
c                hasfog=1.                                            
c              else
c                FOGapnt(i) = 0.                                            !nothing
c              end if
c            else if (CLDBapnt(i).ge.0.0.and.CLDBapnt(i).gt.10.0) then       ! cloud         
c              if(CLDTapnt(i).gt.0.0.and.CLDTapnt(i).le.1000.0) then
c                FOGapnt(i) = 3.                                            !low cloud
c              else
c                FOGapnt(i) = 0                                             !higher cloud
c              end if
c            end if

             if((CLDBapnt(i).ge.0.0 .and. CLDBapnt(i).le.200.0) .and.
     +          (CLDTapnt(i).ge.0.0 .and. CLDTapnt(i).le.400.0)) then
                FOGapnt(i) = 1.                                          
                hasfog=1.
             else
                FOGapnt(i) = 0.
             endif   

           end do

c            if(hasfog.eq.1.0) then
c            write(*,'(A17,i8,A14,10f4.0)') 'Has fog at point ', igrid,
c     +               ' from  member:', (FOGapnt(i),i=1,iens)
c            write(*,'(A20,10f8.1)')'             Fog top',
c     +                                  (CLDTapnt(i),i=1,iens)
c            write(*,'(A20,10f8.3)')'             Fog LWC', 
c     +          rawdata(igrid,:,ID_CWTR,1)*1000.
c            write(*,'(A20,10f8.3)')'                    ',
c     +          rawdata(igrid,:,ID_CWTR,2)*1000.
c            write(*,'(A20,10f8.3)')'                    ',
c     +          rawdata(igrid,:,ID_CWTR,3)*1000.
c            end if


            fog(igrid,:)=FOGapnt(:)

            if(igrid.eq.2051) then
              write(*,*) 'fog=',(fog(igrid,k),k=1,iens)
            end if
                     
        end do


           do 30 lv=1,dPlvl(nv)

             derv_pr(:,nv,lv,:)=0.0

             do lt = 1, dTlvl(nv)
 
              thr1 = dThrs(nv,lt)
                             
              do igrid = 1,jf

               FOGapnt=fog(igrid,:)

               call getprob(FOGapnt,iens,
     +                thr1,thr2,dop(nv),aprob)

               if (aprob .le. 10.0 ) aprob = 0.0 

          
               derv_pr(igrid,nv,lv,lt)=aprob 
        
               if(igrid.eq.2051) then
                write(*,*) 'Thre', dop(nv),dThrs(nv,lt),':',
     +                      derv_pr(igrid,nv,lv,lt)
               end if

              end do
             end do

30          continue  

        end if

        return
        end
