



MODULE module_diag_afwa_hail

CONTAINS












































  SUBROUTINE hailstone_driver ( TCA, h1d, ht, PA, rho1d,&
                                RA, qi1d,qc1d,qr1d,qs1d,qg1d,ng1d,  &
                                VUU, wdur,                          &
                                nz,dhail1,dhail2,dhail3,dhail4,     &
                                dhail5                             )
      
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: nz
  
    REAL, DIMENSION( nz ),             &
         INTENT(IN   ) ::                                  TCA  & 
                                              ,          rho1d  &
                                              ,            h1d  &
                                              ,             PA  & 
                                              ,             RA  & 
                                              ,            VUU  & 
                                              , qi1d,qc1d,qr1d  &
                                              , qs1d,qg1d,ng1d  

    REAL, INTENT(IN   ) ::                                  ht  &
                                              ,           wdur 

    
    REAL, INTENT(INOUT) ::                              dhail1 & 
                                              ,         dhail2 &
                                              ,         dhail3 &
                                              ,         dhail4 &
                                              ,         dhail5
    
    REAL ZBAS, TBAS, WBASP     
    REAL RBAS                  
    REAL cwitot                
    INTEGER KBAS               
    REAL ZFZL, TFZL, WFZLP     
    REAL RFZL                  
    INTEGER KFZL               
    INTEGER nofroze            
    INTEGER ITIME              
    REAL TAU                   
    REAL g                     
    REAL r_d                   
    
    REAL*8 DD, D               
    REAL VT                    
    REAL V                     
    REAL TS                    
    REAL FW                    
    REAL DENSE                 
    INTEGER ITYPE              
    
    REAL, DIMENSION( nz ) ::  &
      RIA, &                   
      RWA                      
    
    REAL P                     
    REAL RS                    
    REAL RI, RW                
    REAL XI, XW                
    REAL PC                    
    REAL TC                    
    REAL VU                    
    REAL DENSA                 
    REAL Z                     
    REAL DELRW                 
    REAL d02,d05,d10,d15,d20   
    REAL, DIMENSION(5) :: dhails     
                                     
    
    REAL TLAYER,RLAYER,PLAYER  
    REAL TSUM,RSUM,PSUM        
    REAL LDEPTH                
    
    REAL GM,GM1,GMW,GMI,DGM,DGMW,DGMI,DI
    REAL dum
      
    REAL sec, secdel           
    INTEGER i, j, k, IFOUT, ind(1)
    CHARACTER*256 :: message

    secdel = 1.0 
    g=9.81
    r_d = 287.
            

    TAU = 3600.
      

    DO i=1,5
       dhails(i) = 0.
    ENDDO
    ITIME = INT(wdur)
 
    
    
    
    KBAS=nz
    KFZL=nz
    DO k=1,nz
         cwitot = qi1d(k) + qc1d(k)
         RIA(k) = qi1d(k) + qs1d(k) + qg1d(k)
         RWA(k) = qc1d(k) + qr1d(k)
         IF ((RIA(k) .ge. 0.0001) .and. (TCA(k).lt.273.15) .and. &
             (k .lt. KFZL)) THEN
            KFZL = k
         ENDIF
         IF ((cwitot .ge. 1.E-12) .and. (k .lt. KBAS)) THEN
            KBAS = k
         ENDIF
    ENDDO
    
    IF (KFZL .lt. KBAS) THEN
       KFZL = KBAS
    ENDIF

    
    ZFZL = h1d(KFZL)
    TFZL = TCA(KFZL)
    WFZLP = PA(KFZL)
    RFZL = RA(KFZL)
    ZBAS = h1d(KBAS)
    TBAS = TCA(KBAS)
    WBASP = PA(KBAS)
    RBAS = RA(KBAS)


    
    
    
    
    d02 = 1.E-5  
    d05 = 2.E-5  
    d10 = 3.E-5  
    d15 = 4.E-5  
    d20 = 5.E-5  
    

    
    DO i=1,5
      SELECT CASE (i)   
        CASE (1)
        
        DD = d02
        CASE (2)
        DD = d05
        CASE (3)  
        DD = d10
        CASE (4)
        DD = d15
        CASE (5)
        DD = d20
      END SELECT

      
      sec = 60
 
      
      P = WFZLP
      RS = RFZL
      TC = TFZL
      VU = VUU(KFZL)  
      Z = ZFZL - ht
      LDEPTH = Z
      DENSA = rho1d(KFZL)

      
      nofroze=1 
      TS = TC
      D = DD   
      FW = 0.0
      DENSE = 500.  

      
      DO WHILE (sec .lt. TAU)
         sec = sec + secdel
         
         
         
         
         
         CALL INTERP(VUU,VU,P,IFOUT,PA,nz)
         
         
         IF (IFOUT.EQ.1) GOTO 100
         
         
         
         IF (sec .gt. ITIME) VU = 0
         
         
         
         CALL TERMINL(DENSA,DENSE,D,VT,TC)
         
         
         V = VU - VT
         
         
         P = P - DENSA*g*V*secdel
         Z = Z + V*secdel

         
         CALL INTERP(TCA,TC,P,IFOUT,PA,nz)
         CALL INTERP(RA,RS,P,IFOUT,PA,nz)
         
         
         DENSA=P/(r_d*(1.+0.609*RS/(1.+RS))*TC)
         
         
         CALL INTERP(RIA,RI,P,IFOUT,PA,nz)
         CALL INTERP(RWA,RW,P,IFOUT,PA,nz)
         XI = RI * DENSA
         XW = RW * DENSA
         IF( (XW+XI).GT.0) THEN
           PC = XI / (XW+XI)
         ELSE
           PC = 1.
         ENDIF
         
         
         
         
         
         
         IF (TS.GE.264.15 .AND. TC.GE.264.15 .AND. NOFROZE.EQ.0) THEN
           IF (TC.LE.265.15) THEN 
             FW=0.  
             TS=TC 
             ITYPE=1 
             NOFROZE=1 
           ELSE  
             FW=1.               
             TS=TC 
             ITYPE=2 
             NOFROZE=0 
           ENDIF 
         ELSE
           IF (TS.LT.273.155) THEN 
             FW=0.
             ITYPE=1
           ELSE 
             TS=273.155
             ITYPE=2
           ENDIF
         ENDIF

        
        
        
 
        
        CALL VAPORCLOSE(DELRW,PC,TS,TC,ITYPE)
      
        
        
        CALL MASSAGR(D,GM,GM1,GMW,GMI,DGM,DGMW,DGMI,DI, &
                 TC,TS,P,DENSE,FW,VT,XW,XI,secdel,ITYPE) 
         

        
        CALL HEATBUD(TS,FW,TC,VT,DELRW,D,DENSA,GM1,DGM,DGMW,  &
                     DGMI,GMW,GMI,DI,secdel,ITYPE,P)

 
        
        
        
        IF(D.GT.0.009) THEN   
           CALL BREAKUP(DENSE,D,GM,FW)
        ENDIF
        
        
        
        IF (Z .LE. ZBAS) GOTO 200
        
      ENDDO  

100   CONTINUE 
200   CONTINUE 

      
      
      
      IF (P.lt.PA(nz)) THEN
         
         D=0.0
      
      ELSE IF(ABS(FW - 1.0).LT.0.001) THEN
         
         D=0.0
      ELSE IF (Z.GT.0) THEN
         
         
        
         
         TSUM = 0.
         RSUM = 0.
         PSUM = 0.
         DO k=1,KBAS
            TSUM = TSUM + TCA(k)
            PSUM = PSUM + PA(k)
            RSUM = RSUM + RA(k)
         ENDDO
         TLAYER = TSUM / KBAS
         PLAYER = PSUM / KBAS
         RLAYER = RSUM / KBAS
           
         CALL MELT(D,TLAYER,PLAYER,RLAYER,LDEPTH,VT)
      ENDIF 
      
      
      dhails(i) = D * 1000

    ENDDO  
  
    
    DO j=1,4
      DO k=j+1,5
         IF (dhails(j).lt.dhails(k)) THEN
            dum = dhails(j)
            dhails(j) = dhails(k)
            dhails(k) = dum
         ENDIF
      ENDDO
    ENDDO
    
    dhail1 = dhails(1)
    dhail2 = dhails(2)
    dhail3 = dhails(3)
    dhail4 = dhails(4)
    dhail5 = dhails(5)
  
  END SUBROUTINE hailstone_driver



  SUBROUTINE INTERP(AA,A,P,IFOUT,PA,ITEL)
  
  
  
  
  
  
  
  
  
  
  
  
  
      IMPLICIT NONE
      
      REAL A, P
      REAL, DIMENSION( ITEL) :: AA, PA
      INTEGER ITEL, IFOUT
      
      INTEGER I
      REAL PDIFF, VDIFF, RDIFF, VERH, ADIFF
      
      IFOUT=1
      
      DO I=1,ITEL-1
        IF (P.LE.PA(I) .AND. P.GT.PA(I+1)) THEN
          
          PDIFF = PA(I)-PA(I+1)
          VDIFF = PA(I)-P
          VERH = VDIFF/PDIFF     
          
          
          RDIFF = AA(I+1) - AA(I)
          
          
          A = AA(I) + RDIFF*VERH
          
          
          IFOUT=0
          EXIT
        ENDIF
      ENDDO
      
  END SUBROUTINE INTERP
      

  SUBROUTINE TERMINL(DENSA,DENSE,D,VT,TC)
  
  
  
  
  
  
  
  
  
  
  
      IMPLICIT NONE
      
      REAL*8 D
      REAL DENSA, DENSE, TC, VT
      REAL GMASS, GX, RE, W, Y
      REAL, PARAMETER :: PI = 3.141592654, G = 9.78956
      REAL ANU
      
      
      GMASS = (DENSE * PI * (D**3.)) / 6.
      
      
      ANU = (0.00001718)*(273.16+120.)/(TC+120.)*(TC/273.16)**(1.5)
      
      
      GX=(8.0*GMASS*G*DENSA)/(PI*(ANU*ANU))
      RE=(GX/0.6)**0.5

      
      
      IF (GX.LT.550) THEN
        W=LOG10(GX)
        Y= -1.7095 + 1.33438*W - 0.11591*(W**2.0)      
        RE=10**Y
        VT=ANU*RE/(D*DENSA)
      ELSE IF (GX.GE.550.AND.GX.LT.1800) THEN
        W=LOG10(GX)
        Y= -1.81391 + 1.34671*W - 0.12427*(W**2.0) + 0.0063*(W**3.0)
        RE=10**Y
        VT=ANU*RE/(D*DENSA)
      ELSE IF (GX.GE.1800.AND.GX.LT.3.45E08) THEN
        RE=0.4487*(GX**0.5536)
        VT=ANU*RE/(D*DENSA)
      ELSE 
        RE=(GX/0.6)**0.5
        VT=ANU*RE/(D*DENSA)
      ENDIF
      
  END SUBROUTINE TERMINL   
   
   
  SUBROUTINE VAPORCLOSE(DELRW,PC,TS,TC,ITYPE)
  
  
  
  
  
  
  
  
  
  
  
  
  
  

      IMPLICIT NONE
      REAL DELRW, PC, TS, TC
      INTEGER ITYPE
      
      REAL RV, ALV, ALS, RATIO
      DATA RV/461.48/,ALV/2500000./,ALS/2836050./ 
      REAL ESAT, RHOKOR, ESATW, RHOOMGW, ESATI, RHOOMGI, RHOOMG

      
      RATIO = 1./273.16
      IF(ITYPE.EQ.2) THEN 
        ESAT=611.*EXP(ALV/RV*(RATIO-1./TS))
      ELSE  
        ESAT=611.*EXP(ALS/RV*(RATIO-1./TS))
      ENDIF
      RHOKOR=ESAT/(RV*TS)
      
      
      ESATW=611.*EXP(ALV/RV*(RATIO-1./TC))
      RHOOMGW=ESATW/(RV*TC)
      ESATI=611.*EXP(ALS/RV*(RATIO-1./TC))
      RHOOMGI=ESATI/(RV*TC)
      RHOOMG=PC*(RHOOMGI-RHOOMGW)+RHOOMGW

      
      
      DELRW=(RHOKOR-RHOOMG) 
  END SUBROUTINE VAPORCLOSE
     
      

  SUBROUTINE MASSAGR(D,GM,GM1,GMW,GMI,DGM,DGMW,DGMI,DI,      &
                 TC,TS,P,DENSE,FW,VT,XW,XI,SEKDEL,ITYPE)  
  
  
  
            
      IMPLICIT NONE
      REAL*8 D
      REAL GM,GM1,GMW,GMI,DGM,DGMW,DGMI,DI,  &
                 TC,TS,P,DENSE,FW,VT,XW,XI,SEKDEL
      INTEGER ITYPE 
      
      REAL PI, D0, GMW2, GMI2, EW, EI
      
      REAL DENSEL 
      REAL DC 
      REAL VOLL, VOLT 
      
      PI=3.141592654

      
      D0=0.226*1.E-4  
      DI=D0*(TC/273.16)**1.81*(100000./P)
  
      
      EW=1.0
      
      
      
      IF(TS.GE.268.15)THEN
        EI=1.00
      ELSE
        EI=0.21
      ENDIF

      
      
      GM=PI/6.*(D**3.)*DENSE
      GMW=FW*GM
      GMI=GM-GMW

      
      GM1=GM

      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      GMW2=GMW+SEKDEL*(PI/4.*D**2.*VT*XW*EW)
      DGMW=GMW2-GMW 
      GMW=GMW2
      
      GMI2=GMI+SEKDEL*(PI/4.*D**2.*VT*XI*EI)
      DGMI=GMI2-GMI 
      GMI=GMI2
      
      DGM=DGMW+DGMI 
      
      IF (ITYPE.EQ.1) THEN 
          
          DC = (0.74*XW / (PI*1000.*3.E8))**0.33333333 * 1.E6 
          
          DENSEL = 0.11*(DC*VT / (273.15-TS))**0.76 
          DENSEL = DENSEL * 1000. 
          
          IF (DENSEL.LT.100) DENSEL=100
          IF (DENSEL.GT.900) DENSEL=900
      ELSE 
          DENSEL = 900.  
      ENDIF
      
      VOLL = DGM / DENSEL
      
      VOLT = VOLL + GM/DENSE
      
      DENSE = (GM+DGM) / VOLT
      D=D+SEKDEL*0.5*VT/DENSE*(XW*EW+XI*EI)      
      

  END SUBROUTINE MASSAGR



  SUBROUTINE HEATBUD(TS,FW,TC,VT,DELRW,D,DENSA,GM1,DGM,DGMW,       &
                     DGMI,GMW,GMI,DI,SEKDEL,ITYPE,P)
  
  
  
  
  
      
      IMPLICIT NONE
      REAL*8 D
      REAL TS,FW,TC,VT,DELRW,DENSA,GM1,DGM,DGMW,  &
                    DGMI,GMW,GMI,DI,SEKDEL,P
      INTEGER ITYPE
      
      REAL RV, RD, G, PI, ALF, ALV, ALS, CI, CW, AK, ANU
      REAL H, E, RE, AH, AE, TCC, TSC
      DATA RV/461.48/,RD/287.04/,G/9.78956/
      DATA PI/3.141592654/
      DATA ALF/3.50E5/ 
      DATA ALV/2.5E6/  
      DATA ALS/2.85E6/ 
      DATA CI/2093/    
      DATA CW/4187/    
      
      
      
      AK=(5.8+0.0184*(TC-273.155))*1.E-3*4.187  
      
      ANU=1.717E-5*(393.0/(TC+120.0))*(TC/273.155)**1.5

      
      RE=D*VT*DENSA/ANU
      
      
      H=(1.46E-5/DI)**(0.333333333)*(RE**0.50) 
      E=(1.46E-5/AK)**(0.333333333)*(RE**0.50) 
      
      

      
      IF(RE.LT.6000.0)THEN
         AH=0.78+0.308*H
         AE=0.78+0.308*E
      ELSEIF(RE.GE.6000.0.AND.RE.LT.20000.0)THEN
         AH=0.76*H
         AE=0.76*E
      ELSEIF(RE.GE.20000.0) THEN
         AH=(0.57+9.0E-6*RE)*H
         AE=(0.57+9.0E-6*RE)*E
      ENDIF

      
      

      TCC = TC - 273.15
      TSC = TS - 273.15
      IF(ITYPE.EQ.1) THEN
      
         
         
         
         TS=TS-(TS-273.15)*DGM/GM1+SEKDEL/(GM1*CI)*                &
            (2.*PI*D*(AH*AK*(TC-TS)-AE*ALS*DI*DELRW)+     &
            DGMW/SEKDEL*(ALF+CW*TCC)+DGMI/SEKDEL*CI*TCC)
      ELSE IF (ITYPE.EQ.2) THEN
      
         
         
         
         FW=FW-FW*DGM/GM1+SEKDEL/(GM1*ALF)*               &
            (2.*PI*D*(AH*AK*TCC-AE*ALV*DI*DELRW)+          &
            DGMW/SEKDEL*(ALF+CW*TCC)+DGMI/SEKDEL*CI*TCC)
      ENDIF

      IF(FW.GT.1.)FW=1.
      IF(FW.LT.0.)FW=0.
  END SUBROUTINE HEATBUD


  
  SUBROUTINE BREAKUP(DENSE,D,GM,FW)
  
  
  
  

      IMPLICIT NONE
      REAL*8 D
      REAL DENSE, GM, FW
      
      REAL WATER, GMI, CRIT, WAT, PI
      DATA PI/3.141592654/

      WATER=FW*GM
      GMI=GM-WATER

      
      
      CRIT=0.268+0.1389*GMI 
      IF (WATER.GT.CRIT)THEN
         WAT=WATER-CRIT
         GM=GM-WAT
         FW=(CRIT)/GM
       
         IF(FW.GT.1.0) FW=1.0
         IF(FW.LT.0.0) FW=0.0

         
         DENSE=(FW*(0.1)+0.9) * 1000.
         D=(6.*GM/(PI*DENSE))**(0.333333333)
      ENDIF
  END SUBROUTINE BREAKUP
  
  
  SUBROUTINE MELT(D,TLAYER,PLAYER,RLAYER,LDEPTH,VT)
  
  
  
  
  
  
  
  
  
  
  
  
  
      IMPLICIT NONE

      REAL*8 D
      REAL TLAYER, PLAYER, RLAYER, LDEPTH, VT
      REAL eenv, delta, ewet, de, der, wetold, wetbulb, wetbulbk
      REAL tdclayer, tclayer, eps, b, hplayer
      REAL*8 a
      REAL sd, lt, ka, lf, lv, t0, dv, pi, rv, rhoice, &
           tres, re, delt, esenv, rhosenv, essfc, rhosfc, dsig, &
           dmdt, mass, massorg, newmass, gamma, r, rho
      INTEGER wcnt
      
      
      tclayer = TLAYER - 273.155
      a = 2.53E11
      b = 5.42E3
      tdclayer = b / LOG(a*eps / (rlayer*player))
      hplayer = player / 100.
      
      
      eps = 0.622
      eenv = (player*rlayer) / (rlayer+eps)
      eenv = eenv / 100.  
      
      
      gamma = 6.6E-4*player
      delta = (4098.0*eenv)/((tdclayer+237.7)*(tdclayer+237.7))
      wetbulb = ((gamma*tclayer)+(delta*tdclayer))/(gamma+delta)
      
      
      wcnt = 0
      DO WHILE (wcnt .lt. 11)
        ewet = 6.108*(exp((17.27*wetbulb)/(237.3 + wetbulb))) 
        de = (0.0006355*hplayer*(tclayer-wetbulb))-(ewet-eenv)
        der= (ewet*(.0091379024 - (6106.396/(273.155+wetbulb)**2))) &
             - (0.0006355*hplayer)
        wetold = wetbulb
        wetbulb = wetbulb - de/der
        wcnt = wcnt + 1
        IF ((abs(wetbulb-wetold)/wetbulb.gt.0.0001)) THEN
           EXIT
        ENDIF
      ENDDO
      
      wetbulbk = wetbulb + 273.155  
      ka = .02 
      lf = 3.34e5 
      lv = 2.5e6  
      t0 = 273.155 
      dv = 0.25e-4 
      pi = 3.1415927
      rv = 1004. - 287. 
      rhoice = 917.0 
      r = D/2. 
      
      
      tres = LDEPTH / VT
        
      
      
      
      rho = 85000./(287.*TLAYER)
      re = rho*r*VT*.01/1.7e-5
      
      
      delt = wetbulb 
                            

      
      
      esenv = 610.8*(exp((17.27*wetbulb)/  &
               (237.3 + wetbulb))) 
      rhosenv = esenv/(rv*wetbulbk)
      essfc = 610.8*(exp((17.27*(t0-273.155))/  &
               (237.3 + (t0-273.155)))) 
      rhosfc = essfc/(rv*t0)
      dsig = rhosenv - rhosfc

      
      dmdt = (-1.7*pi*r*(re**0.5)/lf)*((ka*delt)+((lv-lf)*dv*dsig))
      IF (dmdt.gt.0.) dmdt = 0
      mass = dmdt*tres
      
      
      massorg = 1.33333333*pi*r*r*r*rhoice
      newmass = massorg + mass
      if (newmass.lt.0.0) newmass = 0.0
      D = 2.*(0.75*newmass/(pi*rhoice))**0.333333333
  END SUBROUTINE MELT

END MODULE module_diag_afwa_hail
