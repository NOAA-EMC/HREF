ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  print_rawdata: print the read in rawdata mean/prob  data 
c  Author: B. Zhou
c          Aug. 9, 2005
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

	subroutine print_rawdata(rawdata_mn,rawdata_pr,
     +     jf,iens,start_grd,end_grd)

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
                         

        common /tbl/numvar,
     +              vname,k5,k6,Mlvl,Plvl,Tlvl,
     +              MeanLevel,ProbLevel,Thrs,
     +              Msignal,Psignal,op

        INTEGER, intent(IN) :: jf, iens, start_grd,end_grd
        REAL,dimension(jf,iens,numvar,maxmlvl),intent(IN) :: rawdata_mn
        REAL,dimension(jf,iens,numvar,maxmlvl),intent(IN) :: rawdata_pr
                                                                                                                      
        do nv = 1, numvar

         do lv = 1, Mlvl(nv)
          do igrid=start_grd, end_grd
            if(iens.eq.21) then
             write(*,101) vname(nv), "_mn", ' level_',lv,':',
     +       (rawdata_mn(igrid,irun,nv,lv),irun=1,iens)
101          format(a4,a3,a7,i3,a1,21f8.1)
            else 
             write(*,102) vname(nv), "_mn", ' level_',lv,':',
     +       (rawdata_mn(igrid,irun,nv,lv),irun=1,iens)
102          format(a4,a3,a7,i3,a1,15f8.1)
            end if 
           end do
          end do
          
         do lv = 1, Plvl(nv)
          do igrid=start_grd, end_grd
             write(*,101) vname(nv),"_pr",' level_',lv,':',
     +       (rawdata_pr(igrid,irun,nv,lv),irun=1,iens)
          end do
         end do

        end do

        return
        end



ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  print_result: print the mean/spread/prob results
c  Author: B. Zhou
c          Aug. 9, 2005
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

	subroutine print_result(vrbl_mn,vrbl_sp,vrbl_pr,
     +     derv_mn,derv_sp,derv_pr,ptype_mn,ptype_pr,
     +     prcpmax,prcpmin,snowmax,snowmin,
     +     jf,iens,start_grd,end_grd)

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


        INTEGER, intent(IN) :: jf, iens, start_grd,end_grd
                                                                                                                     
        REAL,dimension(jf,nderiv,maxmlvl),intent(INOUT) :: derv_mn
        REAL,dimension(jf,nderiv,maxmlvl),intent(INOUT) :: derv_sp
        REAL,dimension(jf,nderiv,maxplvl,maxtlvl),intent(INOUT) ::
     +                                                     derv_pr
        REAL,dimension(jf,numvar,maxmlvl),intent(INOUT) :: vrbl_mn
        REAL,dimension(jf,numvar,maxmlvl),intent(INOUT) :: vrbl_sp
        REAL,dimension(jf,numvar,maxplvl,maxtlvl),intent(INOUT) ::
     +                                                     vrbl_pr
        REAL,dimension(jf,maxmlvl),intent(INOUT)        :: prcpmax
        REAL,dimension(jf,maxmlvl),intent(INOUT)        :: prcpmin
        REAL,dimension(jf,maxmlvl),intent(INOUT)        :: snowmax
        REAL,dimension(jf,maxmlvl),intent(INOUT)        :: snowmin
        REAL,dimension(jf,maxmlvl,4),intent(INOUT)      :: ptype_mn
        REAL,dimension(jf,maxmlvl,4),intent(INOUT)      :: ptype_pr

        write(*,*) 'print out mean/spread of direct fileds at grid:',
     +   start_grd,'-', end_grd 
        do nv = 1, numvar
          if(trim(Msignal(nv)).eq.'M') then
           do lv = 1, Mlvl(nv)
            do igrid=start_grd, end_grd
            write(*,*) vname(nv),' at ', MeanLevel(nv,lv),
     +          ': mean=', vrbl_mn(igrid,nv,lv), 
     +          '  spread=', vrbl_sp(igrid,nv,lv)
            end do
           end do
          end if
        end do

        write(*,*) 'print out prob for direct fields at grid:',
     +   start_grd,'-', end_grd
        do nv = 1, numvar
          if(trim(Psignal(nv)).eq.'P') then
           do lv = 1, Plvl(nv)
            do 101 lt = 1, Tlvl(nv)
             if(trim(op(nv)).eq.'-'.and.lt.eq.Tlvl(nv))
     +          goto 101   
             do igrid=start_grd, end_grd
              write(*,*) vname(nv), op(nv),Thrs(nv,lt),' at ',
     +          ProbLevel(nv,lv),
     +          ': prob=', vrbl_pr(igrid,nv,lv,lt)
             end do
101         continue   
           end do
          end if
        end do


        write(*,*) 'print out mean/spread/prob for derived fields ',
     +  'at grid:',  start_grd,'-', end_grd

        do 150 nv = 1, nderiv
 
          if(dk5(nv).ne.61.and.dk5(nv).ne.79.
     +      and.dk5(nv).ne.140)               then    !non-accumulated fields mean/spread
            do lv = 1, dMlvl(nv)
             do igrid = start_grd, end_grd
               write(*,*) dvname(nv),' at ', dMeanLevel(nv,lv),
     +          ': mean=',  derv_mn(igrid,nv,lv), 
     +          ' spread=', derv_sp(igrid,nv,lv)
             end do
            end do
                                                                                                                                               
            do lv = 1, dPlvl(nv)                      !non-accumulated fields prob
             do 102 lt = 1, dTlvl(nv)
               if(trim(dop(nv)).eq.'-'.and.lt.eq.dTlvl(nv))
     +          goto 102
              do igrid=start_grd, end_grd
                write(*,*) dvname(nv),dop(nv),dThrs(nv,lt),' at ', 
     +          dProbLevel(nv,lv),':prob=', derv_pr(igrid,nv,lv,lt)
             end do
102         continue
           end do

          end if
        
         if(dk5(nv).eq.61) then                    
            do lv = 1, dMlvl(nv)                   ! rain mean/spread
             do igrid = start_grd, end_grd
               write(*,*) dMeanLevel(nv,lv),'hr ', dvname(nv),
     +          ': mean=',  derv_mn(igrid,nv,lv),
     +          ' spread=', derv_sp(igrid,nv,lv),
     +          ' maximum=',prcpmax(igrid,lv),
     +          ' minimum=',prcpmin(igrid,lv)
             end do
            end do
                                                                                                                                               
            do lv = 1, dPlvl(nv)                  ! rain prob
             do 103 lt = 1, dTlvl(nv)
              if(trim(dop(nv)).eq.'-'.and.lt.eq.dTlvl(nv))
     +         goto 103
              do igrid=start_grd, end_grd
               write(*,*) dProbLevel(nv,lv),'hr ',dvname(nv),
     +         dop(nv),dThrs(nv,lt),
     +         ':prob=', derv_pr(igrid,nv,lv,lt)
              end do
103          continue
            end do
         end if

         if(dk5(nv).eq.79) then              
            do lv = 1, dMlvl(nv)                 !snow mean/spread
             do igrid = start_grd, end_grd
               write(*,*) dMeanLevel(nv,lv),'hr ', dvname(nv),
     +          ': mean=',  derv_mn(igrid,nv,lv),
     +          ' spread=', derv_sp(igrid,nv,lv),
     +          ' maximum=',snowmax(igrid,lv),
     +          ' minimum=',snowmin(igrid,lv)
             end do
            end do
                                                                                                                                               
            do lv = 1, dPlvl(nv)                !snow prob
             do 104 lt = 1, dTlvl(nv)
               if(trim(dop(nv)).eq.'-'.and.lt.eq.dTlvl(nv))
     +          goto 104
              do igrid=start_grd, end_grd
                write(*,*) dProbLevel(nv,lv),'hr ',dvname(nv),
     +          dop(nv),dThrs(nv,lt),
     +          ':prob=', derv_pr(igrid,nv,lv,lt)
              end do
104          continue
            end do
         end if

         if(dk5(nv).eq.140) then        !precip type

           do lv = 1, dMlvl(nv)                 !mean
            do igrid = start_grd, end_grd
               write(*,*) dMeanLevel(nv,lv),' at ', dMeanLevel(nv,lv),
     +          ': FrzR ',  ptype_mn(igrid,lv,1),
     +          ', Snow ',  ptype_mn(igrid,lv,2),
     +          ', Slt  ',  ptype_mn(igrid,lv,3),
     +          ', Rain ',  ptype_mn(igrid,lv,4)
             end do
           end do
 
          do lv = 1, dPlvl(nv)                   !prob       
            do 105 lt = 1, dTlvl(nv)
              if(trim(dop(nv)).eq.'-'.and.lt.eq.dTlvl(nv))
     +          goto 105
              do igrid=start_grd, end_grd
                write(*,*) dvname(nv),dop(nv),dThrs(nv,lt),' at ',
     +          dProbLevel(nv,lv),'FrzR prob=', ptype_pr(igrid,lv,1),
     +          ', Snow prob=', ptype_pr(igrid,lv,2),
     +          ', Slt  prob=', ptype_pr(igrid,lv,3),
     +          ', Rain prob=', ptype_pr(igrid,lv,4)
              end do
105         continue
          end do
         end if


150     continue

        return
        end



	subroutine print_kpds(nv,np,sts,kpds,kens,kprob,xprob)

         dimension kpds(25),kens(5),kprob(2),xprob(2)
 
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

        character*3 sts   
        integer fl
        if(trim(sts).eq.'_mn') fl=30
        if(trim(sts).eq.'_sp') fl=40
        if(trim(sts).eq.'_pr') fl=50

        if (np.eq.1) then
         write(*,*) 'write direct ',nv,vname(nv),sts,' into grib ',fl
         write(*,*) '   kpds=', (kpds(i),i=1,25)
         write(*,*) '   kens=', (kens(i),i=1,5)
         write(*,*) '   kprob=',(kprob(i),i=1,2)
         write(*,*) '   xprob=',(xprob(i),i=1,2)
         write(*,*) '----------------------------------------------'  
        else if(np.eq.2) then
         write(*,*) 'write derived ',nv,dvname(nv),sts,' into grib ',fl
         write(*,*) '   kpds=', (kpds(i),i=1,25)
         write(*,*) '   kens=', (kens(i),i=1,5)
         write(*,*) '   kprob=',(kprob(i),i=1,2)
         write(*,*) '   xprob=',(xprob(i),i=1,2)
         write(*,*) '----------------------------------------------'
        end if

         return
         end
 
