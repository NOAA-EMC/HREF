Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C
C DoPhase is a subroutine written and provided by Jim Ramer at NOAA/FSL
C
C    Ramer, J, 1993: An empirical technique for diagnosing precipitation
C           type from model output.  Preprints, 5th Conf. on Aviation
C           Weather Systems, Vienna, VA, Amer. Meteor. Soc., 227-230.
C
C   CODE ADAPTED FOR WRF POST  24 AUGUST 2005    G MANIKIN
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C
      SUBROUTINE CALWXT_RAMER(T,Q,PMID,PINT,LMH,PREC,PTYP)

c      SUBROUTINE dophase(pq,   !  input pressure sounding mb
c     +    t,   !  input temperature sounding K
c     +    pmid,   !  input pressure
C     +    pint,   !  input interface pressure
c     +    q,   !  input spec humidityfraction
c     +    lmh,   !  input number of levels in sounding
c     +    prec,      ! input amount of precipitation
c     +    ptyp) !  output(2) phase 2=Rain, 3=Frzg, 4=Solid,
C                                               6=IP     JC  9/16/99
!      include "parmeta" 
      include "params"
      include "CTLBLK.comm" 
      LOGICAL trace
      PARAMETER (trace = .false.)
c      PARAMETER (A2=17.2693882,A3=273.16,A4=35.86,PQ0=379.90516)
c      PARAMETER (G=9.80665,CP=1004.686,RCP=0.2857141,LECP=1572.5)
      PARAMETER (RCP=0.2857141,LECP=1572.5)
      PARAMETER (twice=266.55,rhprcp=0.80,deltag=1.02,prcpmin=0.3,
     *             emelt=0.045,rlim=0.04,slim=0.85)
      PARAMETER (twmelt=273.15,tz=273.15,efac=1.0,PTHRESH=0.000004)
C
      INTEGER*4 i, k1, lll, k2, toodry, iflag, nq
C
C
      REAL rcp, flg, flag, xxx ,mye, icefrac
      DIMENSION T(IM,jsta_2l:jend_2u,LM),Q(IM,jsta_2l:jend_2u,LM),
     &    P(IM,jsta_2l:jend_2u,LM),
     &    TQ(IM,jsta_2l:jend_2u,LM),QQ(IM,jsta_2l:jend_2u,LM),
     &    PQ(IM,jsta_2l:jend_2u,LM),RHQ(IM,jsta_2l:jend_2u,LM),
     &    PMID(IM,jsta_2l:jend_2u,LM),PINT(IM,jsta_2l:jend_2u,LP1)
      REAL LMH(IM,jsta_2l:jend_2u),PREC(IM,jsta_2l:jend_2u)
      DIMENSION tqtmp(im,jsta_2l:jend_2u,lm)
      DIMENSION pqtmp(im,jsta_2l:jend_2u,lm)
      DIMENSION rhqtmp(im,jsta_2l:jend_2u,lm)
      DIMENSION PTYP(IM,JM), TWQ(IM,JM,LM)
      REAL, ALLOCATABLE :: TWET(:,:,:)

C
      COMMON /flagflg/ flag, flg
      DATA iflag / -9/
C
C  Initialize.
      IF (trace) WRITE (20,*) '******* NEW STATION ******'
      IF (trace) WRITE (20,*) 'Twmelt,Twice,rhprcp,Emelt'
      IF (trace) WRITE (20,*) twmelt, twice, rhprcp, emelt
      icefrac = flag
C
      DO J=JSTA,JEND
      DO I=1,IM
       PTYP(I,J) = 0
       NQ=LMH(I,J)
       DO 88 L = 1,NQ
        LEV=NQ-L+1
        P(I,J,L)=PMID(I,J,L)
        QC=PQ0/P(I,J,L) * EXP(A2*(T(I,J,L)-A3)/(T(I,J,L)-A4))
        RHQTMP(I,J,LEV)=Q(I,J,L)/QC
        PQTMP(I,J,LEV)=P(I,J,L)/100.
        TQTMP(I,J,LEV)=T(I,J,L)
   88 CONTINUE

      do 92 L=1,NQ 
         TQ(I,J,L)=TQTMP(I,J,L)
         PQ(I,J,L)=PQTMP(I,J,L)
         RHQ(I,J,L)=RHQTMP(I,J,L)
   92 continue
      enddo
      enddo

C  BIG LOOP
      DO 800 J=JSTA,JEND
      DO 800 I=1,IM
C
C   SKIP THIS POINT IF NO PRECIP THIS TIME STEP
C
      IF (PREC(I,J).LE.PTHRESH) GOTO 800
      LMHK=NINT(LMH(I,J))

C
C
CCC   RATE RESTRICTION REMOVED BY JOHN CORTINAS 3/16/99
C
C     Construct wet-bulb sounding, locate generating level.
      twmax = -999.0
      rhmax = 0.0
      k1 = 0    !  top of precip generating layer
      k2 = 0    !  layer of maximum rh
C
      IF (trace) WRITE (20,*) 'rhq(1)', rhq(i,j,1)
      IF (rhq(I,J,1).lt.rhprcp) THEN
          toodry = 1
      ELSE
          toodry = 0
      END IF
C
C     toodry=((Rhq(I,J,1).lt.rhprcp).and.1)
      pbot = pq(I,J,1)
      NQ=LMH(I,J)
      DO 10 L = 1, nq
          xxx = tdofesat(esat(tq(I,J,L))*rhq(I,J,L))
          IF (trace) WRITE (20,*) 'T,Rh,Td,P,nq ', tq(I,J,L),
     +       rhq(I,J,L), xxx, PQ(I,J,L), NQ
          twq(I,J,L) = xmytw(tq(I,J,L),xxx,pq(I,J,L))
          IF(I .EQ. 324 .and. J .EQ. 390) THEN
            print *, 'tw ramer ', L, Twq(I,J,L)
          ENDIF
          IF (trace) WRITE (20,*) 'Twq(I,J,L),L ', twq(I,J,L), L
          twmax = amax1(twq(I,J,L),twmax)
          IF (trace) WRITE (20,*) 'Tw,Rh,P ', twq(I,J,L) - 273.15,
     +        rhq(I,J,L), pq(I,J,L)
          IF (pq(I,J,L).ge.400.0) THEN
              IF (rhq(I,J,L).gt.rhmax) THEN
                  rhmax = rhq(I,J,L)
                  k2 = i
                  IF (trace) WRITE (20,*) 'rhmax,k2,L', rhmax, k2, L
              END IF
C
              IF (L.ne.1) THEN
                IF (trace) WRITE (20,*) 'ME: toodry,L', toodry, L
                 IF (rhq(I,J,L).ge.rhprcp.or.toodry.eq.0) THEN
                  IF (toodry.ne.0) THEN
                    dpdrh = alog(pq(I,J,L)/pq(I,J,L-1)) / 
     +                       (rhq(I,J,L)-RHQ(I,J,L-1))
                    pbot = exp(alog(pq(I,J,L))+(rhprcp-
     +                       rhq(I,J,L))*dpdrh)
C
Clin                dpdrh=(Pq(I,J,L)-Pq(I,J,L-1))/
Clin                       (Rhq(I,J,L)-Rhq(I,J,L-1))
Clin                pbot=Pq(I,J,L)+(rhprcp-Rhq(I,J,L))*dpdrh
                    ptw = pq(I,J,L)
                    toodry = 0
                    IF (trace) WRITE (20,*) 'dpdrh,pbot,rhprcp-rhq(I,J,L),
     +                L,ptw,toodry', dpdrh, pbot, rhprcp - rhq(I,J,L),
     +                      L,ptw,toodry
                    ELSE IF (rhq(I,J,L).ge.rhprcp) THEN
                      ptw = pq(I,J,L)
                      IF (trace) WRITE (20,*) 'HERE1: ptw,toodry', 
     +                    ptw, toodry
                    ELSE
                      toodry = 1
                      dpdrh = alog(pq(I,J,L)/pq(I,J,L-1)) / 
     +                    (rhq(I,J,L)-rhq(I,J,L-1))
                      ptw = exp(alog(pq(I,J,L))+(rhprcp-rhq(I,J,L))
     +                       *dpdrh)
                      IF (trace) WRITE (20,*) 
     +                      'HERE2:dpdrh,pbot,L,ptw,toodry', dpdrh, 
     +                      pbot, L, ptw, toodry
Clin                dpdrh=(Pq(i)-Pq(i-1))/(Rhq(i)-Rhq(i-1))
Clin                ptw=Pq(i)+(rhprcp-Rhq(i))*dpdrh
C
                      END IF
C
                      IF (trace) WRITE (20,*) 'HERE3:pbot,ptw,deltag', 
     +                    pbot, ptw, deltag
                      IF (pbot/ptw.ge.deltag) THEN
Clin                      If (pbot-ptw.lt.deltag) Goto 2003
                          k1 = L
                          ptop = ptw
                      END IF
                  END IF
              END IF
          END IF
C
   10 CONTINUE

C
C     Gross checks for liquid and solid precip which dont require generating level.
C
      IF (twq(I,J,1).ge.273.15+2.0) THEN
          ptyp(i,j) = 8   ! liquid
          IF (trace) PRINT *, 'liquid'
          icefrac = 0.0
          goto 800 
      END IF
C
      IF (twmax.le.twice) THEN
          icefrac = 1.0
          ptyp(i,j) = 1   !  solid
          goto 800 
      END IF
C
C     Check to see if we had no success with locating a generating level.
C
      IF (trace) WRITE (20,*) 'HERE6: k1,ptyp', k1, ptyp(i,j)
      IF (k1.eq.0) THEN
          rate = flag
          goto 800 
      END IF
C
      IF (ptop.eq.pq(I,J,k1)) THEN
          twtop = twq(I,J,k1)
          rhtop = rhq(I,J,k1)
          k2 = k1
          k1 = k1 - 1
      ELSE
          k2 = k1
          k1 = k1 - 1
          wgt1 = alog(ptop/pq(I,J,k2)) / alog(pq(I,J,k1)/pq(I,J,k2))
Clin      wgt1=(ptop-Pq(I,J,k2))/(Pq(I,J,k1)-Pq(I,J,k2))
          wgt2 = 1.0 - wgt1
          twtop = twq(I,J,k1) * wgt1 + twq(I,J,k2) * wgt2
          rhtop = rhq(I,J,k1) * wgt1 + rhq(I,J,k2) * wgt2
      END IF
C
      IF (trace) WRITE (20,*) 
     +    'HERE7: ptop,k1,pq(I,J,k1),twtop,rhtop,k2,wgt1,     wgt2', 
     +     ptop,k1, pq(I,J,k1), twtop, rhtop, k2, wgt1, wgt2
C

C     Calculate temp and wet-bulb ranges below precip generating level.
      DO 20 L = 1, k1
          twmax = amax1(twq(i,j,l),twmax)
   20 CONTINUE
C
C     Gross check for solid precip, initialize ice fraction.
      IF (trace) WRITE (20,*) twmax
      IF (twtop.le.twice) THEN
          icefrac = 1.0
          IF (twmax.le.twmelt) THEN     ! gross check for solid precip.
              IF (trace) PRINT *, 'solid'
              ptyp(i,j) = 1       !   solid precip
              goto 800 
          END IF
          lll = 0
      ELSE
          icefrac = 0.0
          lll = 1
      END IF
C
C     Loop downward through sounding from highest precip generating level.
   30 CONTINUE
C
      IF (trace) PRINT *, ptop, twtop - 273.15, icefrac
      IF (trace) WRITE (20,*) 'P,Tw,frac,twq(I,J,k1)', ptop, 
     +    twtop - 273.15, icefrac, twq(I,J,k1)
      IF (icefrac.ge.1.0) THEN  !  starting as all ice
          IF (trace) WRITE (20,*) 'ICEFRAC=1', icefrac
c          print *, 'twq twmwelt twtop ', twq(I,J,k1), twmelt, twtop
          IF (twq(I,J,k1).lt.twmelt) GO TO 40       ! cannot commence melting
          IF (twq(I,J,k1).eq.twtop) GO TO 40        ! both equal twmelt, nothing h
          wgt1 = (twmelt-twq(I,J,k1)) / (twtop-twq(I,J,k1))
          rhavg = rhq(I,J,k1) + wgt1 * (rhtop-rhq(I,J,k1)) / 2
          dtavg = (twmelt-twq(I,J,k1)) / 2
          dpk = wgt1 * alog(pq(I,J,k1)/ptop)        !lin   dpk=wgt1*(Pq(k1)-Ptop)
C         mye=emelt*(1.0-(1.0-Rhavg)*efac)
          mye = emelt * rhavg ** efac
          icefrac = icefrac + dpk * dtavg / mye
          IF (trace) WRITE (20,*) 
     +        'HERE8: wgt1,rhavg,dtavg,dpk,mye,icefrac', wgt1, rhavg, 
     +        dtavg, dpk, mye, icefrac
      ELSE IF (icefrac.le.0.0) THEN     !  starting as all liquid
          IF (trace) WRITE (20,*) 'HERE9: twtop,twq(I,J,k1),k1,lll'
     +    , twtop, twq(I,J,k1), k1, lll
          lll = 1
C         If (Twq(I,J,k1).le.Twice) icefrac=1.0 ! autoconvert
C         Goto 1020
          IF (twq(I,J,k1).gt.twice) GO TO 40        ! cannot commence freezing
          IF (twq(I,J,k1).eq.twtop) THEN
              wgt1 = 0.5
          ELSE
              wgt1 = (twice-twq(I,J,k1)) / (twtop-twq(I,J,k1))
          END IF
          rhavg = rhq(I,J,k1) + wgt1 * (rhtop-rhq(I,J,k1)) / 2
          dtavg = twmelt - (twq(I,J,k1)+twice) / 2
          dpk = wgt1 * alog(pq(I,J,k1)/ptop)      !lin  dpk=wgt1*(Pq(k1)-Ptop)
C         mye=emelt*(1.0-(1.0-Rhavg)*efac)
          mye = emelt * rhavg ** efac
          icefrac = icefrac + dpk * dtavg / mye
          IF (trace) WRITE (20,*) 'HERE10: wgt1,rhtop,rhq(I,J,k1),
     +        dtavg', wgt1, rhtop, rhq(I,J,k1), dtavg
      ELSE IF ((twq(I,J,k1).le.twmelt).and.(twq(I,J,k1).lt.twmelt)) THEN ! mix
          rhavg = (rhq(I,J,k1)+rhtop) / 2
          dtavg = twmelt - (twq(I,J,k1)+twtop) / 2
          dpk = alog(pq(I,J,k1)/ptop)       !lin   dpk=Pq(I,J,k1)-Ptop
C         mye=emelt*(1.0-(1.0-Rhavg)*efac)
          mye = emelt * rhavg ** efac
          icefrac = icefrac + dpk * dtavg / mye
           
          IF (trace) WRITE (20,*) 'HERE11: twq(i,j,K1),twtop', 
     +        twq(i,j,k1),twtop
      ELSE      ! mix where Tw curve crosses twmelt in layer
          IF (twq(I,J,k1).eq.twtop) GO TO 40   ! both equal twmelt, nothing h
          wgt1 = (twmelt-twq(I,J,k1)) / (twtop-twq(I,J,k1))
          wgt2 = 1.0 - wgt1
          rhavg = rhtop + wgt2 * (rhq(I,J,k1)-rhtop) / 2
          dtavg = (twmelt-twtop) / 2
          dpk = wgt2 * alog(pq(I,J,k1)/ptop)     !lin   dpk=wgt2*(Pq(k1)-Ptop)
C         mye=emelt*(1.0-(1.0-Rhavg)*efac)
          mye = emelt * rhavg ** efac
          icefrac = icefrac + dpk * dtavg / mye
          icefrac = amin1(1.0,amax1(icefrac,0.0))
          IF (trace) WRITE (20,*) 'HERE12: twq(I,J,k1),twtop,icefrac,wgt1,wg
     +        t2,rhavg,rhtop,rhq(I,J,k1),dtavg,k1', twq(I,J,k1), twtop,
     +        icefrac,wgt1,wgt2, rhavg, rhtop, rhq(I,J,k1), dtavg, k1
          IF (icefrac.le.0.0) THEN
C             If (Twq(I,J,k1).le.Twice) icefrac=1.0 ! autoconvert
C             Goto 1020
              IF (twq(I,J,k1).gt.twice) GO TO 40    ! cannot commence freezin
              wgt1 = (twice-twq(I,J,k1)) / (twtop-twq(I,J,k1))
              dtavg = twmelt - (twq(I,J,k1)+twice) / 2
              IF (trace) WRITE (20,*) 'IN IF'
          ELSE
              dtavg = (twmelt-twq(I,J,k1)) / 2
              IF (trace) WRITE (20,*) 'IN ELSE'
          END IF
          IF (trace) WRITE (20,*) 'NEW ICE FRAC CALC'
          rhavg = rhq(I,J,k1) + wgt1 * (rhtop-rhq(I,J,k1)) / 2
          dpk = wgt1 * alog(pq(I,J,k1)/ptop)     !lin  dpk=wgt1*(Pq(k1)-Ptop)
C         mye=emelt*(1.0-(1.0-Rhavg)*efac)
          mye = emelt * rhavg ** efac
          icefrac = icefrac + dpk * dtavg / mye
          IF (trace) WRITE (20,*) 'HERE13: icefrac,k1,dtavg,rhavg', 
     +        icefrac, k1, dtavg, rhavg
      END IF
C
      icefrac = amin1(1.0,amax1(icefrac,0.0))
      IF (trace) WRITE (20,*) 'NEW ICEFRAC:', icefrac, icefrac
C
C     Get next level down if there is one, loop back.
   40 IF (k1.gt.1) THEN
          IF (trace) WRITE (20,*) 'LOOPING BACK'
          twtop = twq(I,J,k1)
          ptop = pq(I,J,k1)
          rhtop = rhq(I,J,k1)
          k1 = k1 - 1
          GO TO 30
      END IF
C
C
C     Determine precip type based on snow fraction and surface wet-bulb.
C
      IF (trace) WRITE (20,*) 'P,Tw,frac,lll', pq(I,J,k1),
     +    twq(I,J,k2) - 273.15, icefrac, lll
C
      IF (icefrac.ge.slim) THEN
          IF (lll.ne.0) THEN
              ptyp(i,j) = 2       ! Ice Pellets   JC 9/16/99
              IF (trace) WRITE (20,*) 'frozen'
          ELSE
              ptyp(i,j) = 1       !  Snow
              IF (trace) WRITE (20,*) 'snow'
          END IF
      ELSE IF (icefrac.le.rlim) THEN
          IF (twq(i,j,1).lt.tz) THEN
              ptyp(i,j) = 4       !  Freezing Precip
              IF (trace) WRITE (20,*) 'freezing'
          ELSE
              ptyp(i,j) = 8       !  Rain
              IF (trace) WRITE (20,*) 'liquid'
          END IF
      ELSE
          IF (trace) WRITE (20,*) 'Mix'
          IF (twq(i,j,1).lt.tz) THEN
              IF (trace) WRITE (20,*) 'freezing'
cGSM not sure what to do when 'mix' is predicted;   In previous
cGSM   versions of this code for which I had to have an answer,
cGSM   I chose sleet.  Here, though, since we have 4 other
cGSM   algorithms to provide an answer, I will not declare a
cGSM   type from the Ramer in this situation and allow the
cGSM   other algorithms to make the call.
      
              ptyp(i,j) = 0       !  don't know 
c              ptyp = 5       !  Mix
          ELSE
c              ptyp = 5       !  Mix
              ptyp(i,j) = 0       !  don't know 
          END IF
      END IF
      IF (trace) WRITE (20,*) "Returned ptyp is:ptyp,lll ", ptyp, lll
      IF (trace) WRITE (20,*) "Returned icefrac is: ", icefrac
 800  CONTINUE 
      DO 900 J=JSTA,JEND
      DO 900 I=1,IM
 900  CONTINUE
      RETURN
C
      END
C
      REAL*4 FUNCTION esat(t)
C
C*  Calculates saturation vapor pressure in millibars as a function of
C*  either Kelvin of Celcius temperature.
C
      IMPLICIT NONE
C
      REAL*4 t, k
C
      REAL*4 flag, flg
      COMMON /flagflg/ flag, flg
C
C  Account for both Celsius and Kelvin.
      k = t
      IF (k.lt.100.) k = k + 273.15
C     
C     Flag ridiculous values.
      IF (k.lt.0.0.or.k.gt.373.15) THEN
          esat = flag
          RETURN
      END IF
C     
C     Avoid floating underflow.
      IF (k.lt.173.15) THEN
          esat = 3.777647E-05
          RETURN
      END IF
C     
C     Calculation for normal range of values.
      esat = exp(26.660820-0.0091379024*k-6106.3960/k)
C     
      RETURN
      END
C
      REAL*4 FUNCTION tdofesat(es)
C
C*  As a function of saturation vapor pressure in millibars, returns
C*  dewpoint in degrees K.
C
      IMPLICIT NONE
C
      REAL*4 es, lim1, lim2, b
C
      DATA lim1, lim2 /3.777647E-05, 980.5386/
C
      REAL*4 flag, flg
      COMMON /flagflg/ flag, flg
C
C  Flag ridiculous values.
      IF (es.lt.0.0.or.es.gt.lim2) THEN
          tdofesat = flag
          RETURN
      END IF
C     
C     Avoid floating underflow.
      IF (es.lt.lim1) THEN
          tdofesat = 173.15
          RETURN
      END IF
C     
C     Calculations for normal range of values.
      b = 26.66082 - alog(es)
      tdofesat = (b-sqrt(b*b-223.1986)) / 0.0182758048
C     
      RETURN
      END
C
c      REAL*4 FUNCTION mytw(t,td,p)
      FUNCTION xmytw(t,td,p)
C
      IMPLICIT NONE
C
      INTEGER*4 cflag, l
      REAL*4 f, c0, c1, c2, k, kd, kw, ew, t, td, p, ed, fp, s,
     *          de, xmytw
      DATA f, c0, c1, c2 /0.0006355, 26.66082, 0.0091379024, 6106.3960/
C
C
      xmytw = (t+td) / 2
      IF (td.ge.t) RETURN
C
      IF (t.lt.100.0) THEN
          k = t + 273.15
          kd = td + 273.15
          IF (kd.ge.k) RETURN
          cflag = 1
      ELSE
          k = t
          kd = td
          cflag = 0
      END IF
C
      ed = c0 - c1 * kd - c2 / kd
      IF (ed.lt.-14.0.or.ed.gt.7.0) RETURN
      ed = exp(ed)
      ew = c0 - c1 * k - c2 / k
      IF (ew.lt.-14.0.or.ew.gt.7.0) RETURN
      ew = exp(ew)
      fp = p * f
      s = (ew-ed) / (k-kd)
      kw = (k*fp+kd*s) / (fp+s)
C
      DO 10 l = 1, 5
          ew = c0 - c1 * kw - c2 / kw
          IF (ew.lt.-14.0.or.ew.gt.7.0) RETURN
          ew = exp(ew)
          de = fp * (k-kw) + ed - ew
          IF (abs(de/ew).lt.1E-5) GO TO 20
          s = ew * (c1-c2/(kw*kw)) - fp
          kw = kw - de / s
   10 CONTINUE
   20 CONTINUE
C
c      print *, 'kw ', kw
      IF (cflag.ne.0) THEN
          xmytw = kw - 273.15
      ELSE
          xmytw = kw
      END IF
C
      RETURN
      END
