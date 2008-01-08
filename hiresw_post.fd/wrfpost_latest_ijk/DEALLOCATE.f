!@PROCESS NOEXTCHK
      SUBROUTINE DE_ALLOCATE	
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C SUBPROGRAM:    MPI_FIRST   SET UP MESSGAE PASSING INFO
C   PRGRMMR: TUCCILLO        ORG: IBM
C
C ABSTRACT:
C     SETS UP MESSAGE PASSING INFO
C   .
C
C PROGRAM HISTORY LOG:
C   00-01-06  TUCCILLO - ORIGINAL
C   01-10-25  H CHUANG - MODIFIED TO PROCESS HYBRID MODEL OUTPUT
C   02-06-19  MIKE BALDWIN - WRF VERSION
C
C USAGE:    CALL MPI_FIRST
C   INPUT ARGUMENT LIST:
C
C   OUTPUT ARGUMENT LIST:
C
C   OUTPUT FILES:
C     STDOUT  - RUN TIME STANDARD OUT.
C
C   SUBPROGRAMS CALLED:
C       PARA_RANGE
C     UTILITIES:
C       NONE
C     LIBRARY:
C       COMMON - CTLBLK.comm
C
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : IBM RS/6000 SP
C$$$
c
      use vrbls3d
      use vrbls2d
      use soil
      use masks
c
!      include "parmeta"
      include "CTLBLK.comm"
      include "params"
!      include "parmsoil"
      include 'mpif.h'
c
c
c     deallocate arrays
c
C
C     FROM VRBLS3D
C
      deallocate(u)
      deallocate(v)
      deallocate(t)
! CHUANG ADD POTENTIAL TEMP BECAUSE WRF OUTPUT THETA
      deallocate(th)   
      deallocate(q)
!      deallocate(w(im,jsta_2l:jend_2u,lp1))
      deallocate(uh)
      deallocate(vh)
      deallocate(wh)
      deallocate(pmid)
      deallocate(pmidv)
      deallocate(pint)
      deallocate(alpint)
      deallocate(zmid)
      deallocate(zint)
!      deallocate(rainw(im,jsta_2l:jend_2u,lm))
      deallocate(q2)
      deallocate(omga)
      deallocate(T_ADJ)
      deallocate(ttnd)
      deallocate(rswtt)
      deallocate(rlwtt)
      deallocate(exch_h) 
      deallocate(train)
      deallocate(tcucn)
      deallocate(el_myj)
C     MP FIELD   
      deallocate(cwm)
      deallocate(F_ice)
      deallocate(F_rain)
      deallocate(F_RimeF)
      deallocate(QQW)
      deallocate(QQI)
      deallocate(QQR)
      deallocate(QQS)
      deallocate(QQG)
      deallocate(CFR)
      deallocate(DBZ)
      deallocate(DBZR)
      deallocate(DBZI)
      deallocate(DBZC)
      deallocate(mcvg)
C
C     FROM SOIL
C
      deallocate(smc)
      deallocate(stc)
      deallocate(sh2o)
      deallocate(SLDPTH)
      deallocate(RTDPTH)
C
C     FROM VRBLS2D
C
      deallocate(u10)
      deallocate(v10)
      deallocate(tshltr)
      deallocate(qshltr)
      deallocate(smstav)
      deallocate(ssroff)
      deallocate(bgroff)
      deallocate(vegfrc)
      deallocate(acsnow)
      deallocate(acsnom)
      deallocate(cmc)
      deallocate(sst)
      deallocate(qz0)
      deallocate(thz0)
      deallocate(uz0)
      deallocate(vz0)
      deallocate(qs)
      deallocate(ths)
      deallocate(sno)
      deallocate(akms)
      deallocate(akhs)
      deallocate(cuprec)
      deallocate(acprec)
      deallocate(ancprc)
      deallocate(cuppt)
      deallocate(rswin)
      deallocate(rlwin)
      deallocate(rlwtoa)
      deallocate(tg)
      deallocate(sfcshx)
      deallocate(sfclhx)
      deallocate(fis)
      deallocate(t500)
      deallocate(cfracl)
      deallocate(cfracm)
      deallocate(cfrach)
      deallocate(acfrst)
      deallocate(acfrcv)
      deallocate(hbot)
      deallocate(htop)
      deallocate(aswin)
      deallocate(alwin)
      deallocate(aswout)
      deallocate(alwout)
      deallocate(aswtoa)
      deallocate(alwtoa)
      deallocate(czen)
      deallocate(czmean)
      deallocate(sigt4)
      deallocate(rswout)
      deallocate(radot)
      deallocate(ncfrst)  ! real
      deallocate(ncfrcv)  ! real
      deallocate(smstot)
      deallocate(pctsno)
      deallocate(pshltr)
      deallocate(th10)
      deallocate(q10)
      deallocate(sr)
      deallocate(prec)
      deallocate(subshx)
      deallocate(snopcx)
      deallocate(sfcuvx)
      deallocate(sfcevp)
      deallocate(potevp)
      deallocate(z0)
      deallocate(ustar)
      deallocate(pblh)
      deallocate(twbs)
      deallocate(qwbs)
      deallocate(sfcexc)
      deallocate(grnflx)
      deallocate(soiltb)
      deallocate(z1000)
      deallocate(slp)
      deallocate(pslp)
      deallocate(f)
      deallocate(albedo)
      deallocate(albase)
      deallocate(cldfra)
      deallocate(cprate)
      deallocate(cnvcfr)
      deallocate(ivgtyp)
      deallocate(isltyp)
      deallocate(hbotd)
      deallocate(htopd)
      deallocate(hbots)
      deallocate(htops)
      deallocate(cldefi)
      deallocate(islope)
      deallocate(si)
      deallocate(lspa)
      deallocate(rswinc)
      deallocate(vis)
      deallocate(pd)
      deallocate(mxsnal)
C
C     FROM MASKS
C
      deallocate(hbm2)
      deallocate(sm)
      deallocate(sice)
      deallocate(lmh)  ! real
      deallocate(lmv)  ! real
      deallocate(gdlat)
      deallocate(gdlon)
      deallocate(dx)
      deallocate(dy)
      deallocate(htm)
      deallocate(vtm)
C
      end
