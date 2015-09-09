      subroutine packGB2_mean_derv(imean,isprd,derv_mn,
     +     derv_sp,nv,jpd1,jpd2,jpd10,jpd27,jf,Lm,
     +     iens,iyr,imon,idy,ihr,ifhr,gribid,gfld)

        use grib_mod
        include 'parm.inc'

        type(gribfield) :: gfld


c    for derived variables
        Character*4 dvname(maxvar)
        Integer dk5(maxvar), dk6(maxvar), dk4(maxvar)
        Character*1 dMsignal(maxvar), dPsignal(maxvar)
        Integer dMlvl(maxvar), dMeanLevel(maxvar,maxmlvl)
        Integer dPlvl(maxvar), dProbLevel(maxvar,maxplvl)
        Character*1 dop(maxvar)
        Integer dTlvl(maxvar)
        Real    dThrs(maxvar,maxtlvl)
        Integer MPairLevel(maxvar,maxmlvl,2)
        Integer PPairLevel(maxvar,maxplvl,2)


        common /dtbl/nderiv,
     +              dvname,dk4,dk5,dk6,dMlvl,dPlvl,dTlvl,
     +              dMeanLevel,dProbLevel,dThrs,
     +              dMsignal,dPsignal,MPairLevel,PPairLevel,dop

         INTEGER,intent(IN) :: imean,isprd,
     +     nv,jpd1,jpd2,jpd10,jpd27,jf,Lm,
     +     iens,iyr,imon,idy,ihr,ifhr,gribid

        REAL,dimension(jf,Lm),intent(IN) :: derv_mn
        REAL,dimension(jf,Lm),intent(IN) :: derv_sp

        INTEGER,allocatable,dimension(:) ::   ipdtmpl

        integer ml

        write(*,*) 'packing derv mean/spred for nv ',nv

        write(*,*) imean,isprd
     +     nv,jpd1,jpd2,jpd10,jpd27,jf,Lm,
     +     iens,iyr,imon,idy,ihr,ifhr,gribid


        DO 1000 ml=1,dMlvl(nv)


            !redefine some of gfld%idsect() array elements 
            gfld%idsect(1)=7
            gfld%idsect(2)=2
            gfld%idsect(3)=0   !experimental, see Table 1.0
            gfld%idsect(4)=1   !experimental, see Table 1.1
            gfld%idsect(5)=1   !experimental, see Table 1.2
            gfld%idsect(6)=iyr  !year
            gfld%idsect(7)=imon !mon
            gfld%idsect(8)=idy  !day
            gfld%idsect(9)=ihr  !cycle time
            gfld%idsect(10)=0
            gfld%idsect(11)=0
            gfld%idsect(12)=0
            gfld%idsect(13)=1


         if (ml.eq.1) then              !this is just do once

            if (jpd1.eq.1.and.jpd2.eq.8 ) then  
             ipdtnum=12                  !ensemble APCP mean use Template 4.12
             ipdtlen=31
            else 
             ipdtnum=2                   !ensemble NON-accum mean use Template 4.2
             ipdtlen=17
            end if

            allocate (ipdtmpl(ipdtlen))


             ipdtmpl(1)=jpd1
             ipdtmpl(2)=jpd2
             ipdtmpl(3)=4
             ipdtmpl(4)=0
             ipdtmpl(5)=117              !shared with NARRE-TL suggested by Tom Hultquist
             ipdtmpl(6)=0
             ipdtmpl(7)=0
             ipdtmpl(8)=1
             ipdtmpl(9)=ifhr             !fcst hours   
             ipdtmpl(10)=jpd10
             ipdtmpl(11)=gfld%ipdtmpl(11)
             !ipdtmpl(12)= see below 
             ipdtmpl(13)=gfld%ipdtmpl(13)
             ipdtmpl(14)=gfld%ipdtmpl(14)
             ipdtmpl(15)=gfld%ipdtmpl(15)
             ipdtmpl(16)=1               !weighted mean of all members
             ipdtmpl(17)=iens            !number of members
         
            if (jpd1.eq.1.and.jpd2.eq.8 ) then  !Template 4.12 has extra elements than Template 4.2
              call get_time_GB2(iyr,imon,idy,ihr,ifhr,
     +            iyr1,imon1,idy1,ihr1)

              ipdtmpl(18)=iyr1   !year
              ipdtmpl(19)=imon1  !mon 
              ipdtmpl(20)=idy1   !day
              ipdtmpl(9) =ifhr-jpd27        !overwrite for APCP: Beginning time of accumulation
              ipdtmpl(21)=ihr1              !fcst time (end of forecsat time), ie. fcst hour in clock (day/hour)   
              ipdtmpl(22)=0   
              ipdtmpl(23)=0  
              ipdtmpl(24)=1   
              ipdtmpl(25)=0   
              ipdtmpl(26)=1   
              ipdtmpl(27)=2                 !See Table 4.11, same start time (or same cycle time) 
              ipdtmpl(28)=1  
              ipdtmpl(29)=jpd27 
              ipdtmpl(30)=1
              ipdtmpl(31)=0   
             end if
 
          end if  

          if(jpd10.eq.100) then
             ipdtmpl(12)=dMeanLevel(nv,ml)*100
          else if (jpd1.eq.3.and.jpd2.eq.5.and.jpd10.eq.101) then            !thickness case
             ipdtmpl(10)=100
             ipdtmpl(11)=0
             ipdtmpl(12)=MPairLevel(nv,ml,1)*100
             ipdtmpl(13)=100
             ipdtmpl(14)=0
             ipdtmpl(15)=MPairLevel(nv,ml,2)*100
             !write (*,*) 'ipdtmpl=',ipdtmpl 
             !write (*,'(10f9.2)')(derv_mn(i,ml),i=10001,10010)  
             !write (*,'(10f9.2)')(derv_sp(i,ml),i=10001,10010)  
          else
             ipdtmpl(12)=dMeanLevel(nv,ml)
          end if

          gfld%fld=derv_mn(:,ml) 
          
          gfld%ibmap=0     !important reseting

          write(*,*) 'Call Zputgb2 for mean...'
          !write (*,'(10f9.2)')(gfld%fld(i),i=10001,10010)

          iret=0
          call Zputgb2(imean,gfld,ipdtmpl,ipdtnum,ipdtlen,iret)
  
          if(iret.ne.0) then
           write(*,*) 'Zputgb2 derv mean error:',iret
          end if

          gfld%fld=derv_sp(:,ml)

          iret=0

          write(*,*) 'Call Zputgb2 for spread...'
          call Zputgb2(isprd,gfld,ipdtmpl,ipdtnum,ipdtlen,iret)
          
          if(iret.ne.0) then
           write(*,*) 'Zputgb2 derv spread error:',iret
          end if

          write(*,*) 'Packing derv ',nv,' mean/spread  done!'
          
1000    CONTINUE   


        return
        end
