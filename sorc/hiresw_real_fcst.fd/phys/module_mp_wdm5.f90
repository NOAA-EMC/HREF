

MODULE module_mp_wdm5

   REAL, PARAMETER, PRIVATE :: dtcldcr     = 120. 
   REAL, PARAMETER, PRIVATE :: n0r = 8.e6         
   REAL, PARAMETER, PRIVATE :: avtr = 841.9       
   REAL, PARAMETER, PRIVATE :: bvtr = 0.8         
   REAL, PARAMETER, PRIVATE :: r0 = .8e-5         
   REAL, PARAMETER, PRIVATE :: peaut = .55        
   REAL, PARAMETER, PRIVATE :: xncr = 3.e8        
   REAL, PARAMETER, PRIVATE :: xmyu = 1.718e-5    
   REAL, PARAMETER, PRIVATE :: avts = 11.72       
   REAL, PARAMETER, PRIVATE :: bvts = .41         
   REAL, PARAMETER, PRIVATE :: n0smax =  1.e11    
   REAL, PARAMETER, PRIVATE :: lamdacmax = 1.e10  
   REAL, PARAMETER, PRIVATE :: lamdarmax = 1.e8   
   REAL, PARAMETER, PRIVATE :: lamdasmax = 1.e5   
   REAL, PARAMETER, PRIVATE :: lamdagmax = 6.e4   
   REAL, PARAMETER, PRIVATE :: dicon = 11.9       
   REAL, PARAMETER, PRIVATE :: dimax = 500.e-6    
   REAL, PARAMETER, PRIVATE :: n0s = 2.e6         
   REAL, PARAMETER, PRIVATE :: alpha = .12        
   REAL, PARAMETER, PRIVATE :: pfrz1 = 100.       
   REAL, PARAMETER, PRIVATE :: pfrz2 = 0.66       
   REAL, PARAMETER, PRIVATE :: qcrmin = 1.e-9     
   REAL, PARAMETER, PRIVATE :: ncmin = 1.e1       
   REAL, PARAMETER, PRIVATE :: nrmin = 1.e-2      
   REAL, PARAMETER, PRIVATE :: eacrc = 1.0        

   REAL, PARAMETER, PRIVATE :: satmax = 1.0048    
                                                  
   REAL, PARAMETER, PRIVATE :: actk = 0.6         
   REAL, PARAMETER, PRIVATE :: actr = 1.5         
   REAL, PARAMETER, PRIVATE :: ncrk1 = 3.03e3     
   REAL, PARAMETER, PRIVATE :: ncrk2 = 2.59e15    
   REAL, PARAMETER, PRIVATE :: di100 = 1.e-4      
   REAL, PARAMETER, PRIVATE :: di600 = 6.e-4      
   REAL, PARAMETER, PRIVATE :: di2000 = 20.e-4    
   REAL, PARAMETER, PRIVATE :: di82 = 82.e-6      
   REAL, PARAMETER, PRIVATE :: di15 = 15.e-6      

   REAL, SAVE ::                                      &
             qc0, qck1,pidnc,bvtr1,bvtr2,bvtr3,bvtr4, &
             bvtr5,bvtr7,bvtr2o5,bvtr3o5,g1pbr,g2pbr, &
             g3pbr,g4pbr,g5pbr,g7pbr,g5pbro2,g7pbro2, &
             pvtr,pvtrn,eacrr,pacrr,                  &
             precr1,precr2,xmmax,roqimax,bvts1,       &
             bvts2,bvts3,bvts4,g1pbs,g3pbs,g4pbs,     &
             g5pbso2,pvts,pacrs,precs1,precs2,pidn0r, &
             pidn0s,pidnr,xlv1,pacrc,                 &
             rslopecmax,rslopec2max,rslopec3max,      &
             rslopermax,rslopesmax,rslopegmax,        &
             rsloperbmax,rslopesbmax,rslopegbmax,     &
             rsloper2max,rslopes2max,rslopeg2max,     &
             rsloper3max,rslopes3max,rslopeg3max



CONTAINS


  SUBROUTINE wdm5(th, q, qc, qr, qi, qs                            &
                 ,nn, nc, nr                                       &
                 ,den, pii, p, delz                                &
                 ,delt,g, cpd, cpv, ccn0, rd, rv, t0c              &
                 ,ep1, ep2, qmin                                   &
                 ,XLS, XLV0, XLF0, den0, denr                      &
                 ,cliq,cice,psat                                   &
                 ,rain, rainncv                                    &
                 ,snow, snowncv                                    &
                 ,sr                                               &
                 ,ids,ide, jds,jde, kds,kde                        &
                 ,ims,ime, jms,jme, kms,kme                        &
                 ,its,ite, jts,jte, kts,kte                        &
                                                                   )

  IMPLICIT NONE






























  INTEGER,      INTENT(IN   )    ::   ids,ide, jds,jde, kds,kde , &
                                      ims,ime, jms,jme, kms,kme , &
                                      its,ite, jts,jte, kts,kte
  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                 &
        INTENT(INOUT) ::                                          &
                                                             th,  &
                                                              q,  &
                                                              qc, &
                                                              qi, &
                                                              qr, &
                                                              qs, &
                                                              nn, & 
                                                              nc, &
                                                              nr   
  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                 &
        INTENT(IN   ) ::                                          &
                                                             den, &
                                                             pii, &
                                                               p, &
                                                            delz
  REAL, INTENT(IN   ) ::                                    delt, &
                                                               g, &
                                                              rd, &
                                                              rv, &
                                                             t0c, &
                                                            den0, &
                                                             cpd, &
                                                             cpv, &
                                                            ccn0, &
                                                             ep1, &
                                                             ep2, &
                                                            qmin, &
                                                             XLS, &
                                                            XLV0, &
                                                            XLF0, &
                                                            cliq, &
                                                            cice, &
                                                            psat, &
                                                            denr
  REAL, DIMENSION( ims:ime , jms:jme ),                           &
        INTENT(INOUT) ::                                    rain, &
                                                         rainncv, &
                                                              sr

  REAL, DIMENSION( ims:ime , jms:jme ), OPTIONAL,                 &
        INTENT(INOUT) ::                                    snow, &
                                                         snowncv


  REAL, DIMENSION( its:ite , kts:kte ) ::   t
  REAL, DIMENSION( its:ite , kts:kte, 2 ) ::   qci, qrs
  REAL, DIMENSION( its:ite , kts:kte, 3 ) ::   ncr
  CHARACTER*256 :: emess
  INTEGER :: mkx_test
  INTEGER ::               i,j,k



      DO j=jts,jte
         DO k=kts,kte
         DO i=its,ite
            t(i,k)=th(i,k,j)*pii(i,k,j)
            qci(i,k,1) = qc(i,k,j)
            qci(i,k,2) = qi(i,k,j)
            qrs(i,k,1) = qr(i,k,j)
            qrs(i,k,2) = qs(i,k,j)
            ncr(i,k,1) = nn(i,k,j)                          
            ncr(i,k,2) = nc(i,k,j)                         
            ncr(i,k,3) = nr(i,k,j)                          
         ENDDO
         ENDDO

         
         

         CALL wdm52D(t, q(ims,kms,j), qci, qrs, ncr               &
                    ,den(ims,kms,j)                               &
                    ,p(ims,kms,j), delz(ims,kms,j)                &
                    ,delt,g, cpd, cpv, ccn0, rd, rv, t0c          &
                    ,ep1, ep2, qmin                               &
                    ,XLS, XLV0, XLF0, den0, denr                  &
                    ,cliq,cice,psat                               &
                    ,j                                            &
                    ,rain(ims,j),rainncv(ims,j)                   &
                    ,sr(ims,j)                                    &
                    ,ids,ide, jds,jde, kds,kde                    &
                    ,ims,ime, jms,jme, kms,kme                    &
                    ,its,ite, jts,jte, kts,kte                    &
                    ,snow(ims,j),snowncv(ims,j)                   &
                                                                  )

         DO K=kts,kte
         DO I=its,ite
            th(i,k,j)=t(i,k)/pii(i,k,j)
            qc(i,k,j) = qci(i,k,1)
            qi(i,k,j) = qci(i,k,2)
            qr(i,k,j) = qrs(i,k,1)
            qs(i,k,j) = qrs(i,k,2)
            nn(i,k,j) = ncr(i,k,1)
            nc(i,k,j) = ncr(i,k,2)
            nr(i,k,j) = ncr(i,k,3)
         ENDDO
         ENDDO
      ENDDO

  END SUBROUTINE wdm5


  SUBROUTINE wdm52D(t, q, qci, qrs, ncr, den, p, delz             &
                   ,delt,g, cpd, cpv, ccn0, rd, rv, t0c           &
                   ,ep1, ep2, qmin                                &
                   ,XLS, XLV0, XLF0, den0, denr                   &
                   ,cliq,cice,psat                                &
                   ,lat                                           &
                   ,rain,rainncv                                  &
                   ,sr                                            &
                   ,ids,ide, jds,jde, kds,kde                     &
                   ,ims,ime, jms,jme, kms,kme                     &
                   ,its,ite, jts,jte, kts,kte                     &
                   ,snow,snowncv                                  &
                                                                  )

  IMPLICIT NONE

  INTEGER,      INTENT(IN   )    ::   ids,ide, jds,jde, kds,kde , &
                                      ims,ime, jms,jme, kms,kme , &
                                      its,ite, jts,jte, kts,kte,  &
                                      lat
  REAL, DIMENSION( its:ite , kts:kte ),                           &
        INTENT(INOUT) ::                                          &
                                                               t
  REAL, DIMENSION( its:ite , kts:kte, 2 ),                        &
        INTENT(INOUT) ::                                          &
                                                             qci, &
                                                             qrs 
  REAL, DIMENSION( its:ite , kts:kte, 3 ),                        &
        INTENT(INOUT) ::                                          &
                                                             ncr
  REAL, DIMENSION( ims:ime , kms:kme ),                           &
        INTENT(INOUT) ::                                          &
                                                               q
  REAL, DIMENSION( ims:ime , kms:kme ),                           &
        INTENT(IN   ) ::                                          &
                                                             den, &
                                                               p, &
                                                            delz
  REAL, INTENT(IN   ) ::                                    delt, &
                                                               g, &
                                                             cpd, &
                                                             cpv, &
                                                            ccn0, &
                                                             t0c, &
                                                            den0, &
                                                              rd, &
                                                              rv, &
                                                             ep1, &
                                                             ep2, &
                                                            qmin, &
                                                             XLS, &
                                                            XLV0, &
                                                            XLF0, &
                                                            cliq, &
                                                            cice, &
                                                            psat, &
                                                            denr
  REAL, DIMENSION( ims:ime ),                                     &
        INTENT(INOUT) ::                                    rain, &
                                                         rainncv, &
                                                              sr

  REAL, DIMENSION( ims:ime ),     OPTIONAL,                       &
        INTENT(INOUT) ::                                    snow, &
                                                         snowncv


  REAL, DIMENSION( its:ite , kts:kte , 2) ::                      &
        rh, qs, rslope, rslope2, rslope3, rslopeb,                &
        falk, fall, work1
  REAL, DIMENSION( its:ite , kts:kte ) ::                         &
        rslopec, rslopec2,rslopec3                           
  REAL, DIMENSION( its:ite , kts:kte,  2) ::                      &
        avedia 
  REAL, DIMENSION( its:ite , kts:kte ) ::                         &
        workn,falln,falkn
  REAL, DIMENSION( its:ite , kts:kte ) ::                         &
              falkc, work1c, work2c, fallc
  REAL, DIMENSION( its:ite , kts:kte ) ::                         &
        pcact, praut, psaut, prevp, psdep, pracw, psaci, psacw,   &  
        pigen, pidep, pcond, prevp_s,                             &
        xl, cpm, work2, psmlt, psevp, denfac, xni,                &
        n0sfac
  REAL, DIMENSION( its:ite , kts:kte ) ::                         &
        nraut, nracw, nrevp, ncevp, nccol, nrcol,                 &
        nsacw, nseml, ncact 
  REAL :: ifac, sfac

  REAL, DIMENSION(its:ite) :: xal, xbl


  REAL, DIMENSION( its:ite )           :: tvec1
  INTEGER, DIMENSION( its:ite ) :: mnstep, numndt
  INTEGER, DIMENSION( its:ite ) :: mstep, numdt
  REAL, DIMENSION(its:ite) :: rmstep
  REAL dtcldden, rdelz, rdtcld
  LOGICAL, DIMENSION( its:ite ) :: flgcld
  REAL  ::  pi,                                                   &
            cpmcal, xlcal, lamdac, lamdar, lamdas, diffus,        &
            viscos, xka, venfac, conden, diffac,                  &
            x, y, z, a, b, c, d, e,                               &
            ndt, qdt, holdrr, holdrs, supcol, supcolt, pvt,       &
            coeres, supsat, dtcld, xmi, eacrs, satdt,             &
            vt2i,vt2s,acrfac, coecol,                             &
            nfrzdtr, nfrzdtc,                                     &
            taucon, lencon, lenconcr,                             &
            qimax, diameter, xni0, roqi0,                         &
            fallsum, fallsum_qsi, xlwork2, factor, source,        &
            value, xlf, pfrzdtc, pfrzdtr, supice
  REAL :: temp 
  REAL  :: holdc, holdci
  INTEGER :: i, j, k, mstepmax,                                                &
            iprt, latd, lond, loop, loops, ifsat, n

  REAL  :: dldti, xb, xai, tr, xbi, xa, hvap, cvap, hsub, dldt, ttp
  REAL  :: logtr




      cpmcal(x) = cpd*(1.-max(x,qmin))+max(x,qmin)*cpv
      xlcal(x) = xlv0-xlv1*(x-t0c)





      lamdac(x,y,z)= exp(log(((pidnc*z)/(x*y)))*((.33333333)))
      lamdar(x,y,z)= exp(log(((pidnr*z)/(x*y)))*((.33333333)))
      lamdas(x,y,z)= sqrt(sqrt(pidn0s*z/(x*y)))    













      pi = 4. * atan(1.)




      do k = kts, kte
        do i = its, ite
          qci(i,k,1) = max(qci(i,k,1),0.0)
          qrs(i,k,1) = max(qrs(i,k,1),0.0)
          qci(i,k,2) = max(qci(i,k,2),0.0)
          qrs(i,k,2) = max(qrs(i,k,2),0.0)
          ncr(i,k,1) = max(ncr(i,k,1),0.)
          ncr(i,k,2) = max(ncr(i,k,2),0.)
          ncr(i,k,3) = max(ncr(i,k,3),0.) 
        enddo
      enddo





      do k = kts, kte
        do i = its, ite
          cpm(i,k) = cpmcal(q(i,k))
          xl(i,k) = xlcal(t(i,k))
        enddo
      enddo




      loops = max(nint(delt/dtcldcr),1)
      dtcld = delt/loops
      if(delt.le.dtcldcr) dtcld = delt

      do loop = 1,loops




      do i = its, ite
        mstep(i) = 1
        mnstep(i) = 1
        flgcld(i) = .true.
      enddo






      do k = kts, kte
        CALL vsrec( tvec1(its), den(its,k), ite-its+1)
        do i = its, ite
          tvec1(i) = tvec1(i)*den0
        enddo
        CALL vssqrt( denfac(its,k), tvec1(its), ite-its+1)
      enddo




      hsub = xls
      hvap = xlv0
      cvap = cpv
      ttp=t0c+0.01
      dldt=cvap-cliq
      xa=-dldt/rv
      xb=xa+hvap/(rv*ttp)
      dldti=cvap-cice
      xai=-dldti/rv
      xbi=xai+hsub/(rv*ttp)


      do k = kts, kte
        do i = its, ite
          if(t(i,k).lt.ttp) then
            xal(i) = xai
            xbl(i) = xbi
          else
            xal(i) = xa
            xbl(i) = xb
          endif
        enddo
        do i = its, ite
          tr=ttp/t(i,k)
          logtr=log(tr)
          qs(i,k,1)=psat*exp(logtr*(xa)+xb*(1.-tr))
          qs(i,k,1) = ep2 * qs(i,k,1) / (p(i,k) - qs(i,k,1))
          qs(i,k,1) = max(qs(i,k,1),qmin)
          rh(i,k,1) = max(q(i,k) / qs(i,k,1),qmin)
          qs(i,k,2)=psat*exp(logtr*(xal(i))+xbl(i)*(1.-tr))
          qs(i,k,2) = ep2 * qs(i,k,2) / (p(i,k) - qs(i,k,2))
          qs(i,k,2) = max(qs(i,k,2),qmin)
          rh(i,k,2) = max(q(i,k) / qs(i,k,2),qmin)
        enddo
      enddo




      do k = kts, kte
        do i = its, ite
          prevp(i,k) = 0.
          psdep(i,k) = 0.
          praut(i,k) = 0.
          psaut(i,k) = 0.
          pracw(i,k) = 0.
          psaci(i,k) = 0.
          psacw(i,k) = 0.
          pigen(i,k) = 0.
          pidep(i,k) = 0.
          pcond(i,k) = 0.
          psmlt(i,k) = 0.
          psevp(i,k) = 0.
          pcact(i,k) = 0.
          prevp_s(i,k) = 0.
          falk(i,k,1) = 0.
          falk(i,k,2) = 0.
          fall(i,k,1) = 0.
          fall(i,k,2) = 0.
          fallc(i,k) = 0.
          falkc(i,k) = 0.
          falln(i,k) = 0.
          falkn(i,k) = 0.
          xni(i,k) = 1.e3
          nsacw(i,k) = 0.
          nseml(i,k) = 0.
          nracw(i,k) = 0.
          nccol(i,k) = 0.
          nrcol(i,k) = 0.
          ncact(i,k) = 0.
          nraut(i,k) = 0.
          nrevp(i,k) = 0.
          ncevp(i,k) = 0.
        enddo
      enddo





      do k = kts, kte
        do i = its, ite
          supcol = t0c-t(i,k)



          n0sfac(i,k) = max(min(exp(alpha*supcol),n0smax/n0s),1.)
          if(qrs(i,k,1).le.qcrmin .or. ncr(i,k,3).le.nrmin)then
            rslope(i,k,1) = rslopermax
            rslopeb(i,k,1) = rsloperbmax
            rslope2(i,k,1) = rsloper2max
            rslope3(i,k,1) = rsloper3max
          else
            rslope(i,k,1) = 1./lamdar(qrs(i,k,1),den(i,k),ncr(i,k,3))
            rslopeb(i,k,1) = exp(log(rslope(i,k,1))*(bvtr))
            rslope2(i,k,1) = rslope(i,k,1)*rslope(i,k,1)
            rslope3(i,k,1) = rslope2(i,k,1)*rslope(i,k,1)
          endif
          if(qci(i,k,1).le.qmin .or. ncr(i,k,2).le.ncmin)then
            rslopec(i,k) = rslopecmax
            rslopec2(i,k) = rslopec2max
            rslopec3(i,k) = rslopec3max
          else
            rslopec(i,k) = 1./lamdac(qci(i,k,1),den(i,k),ncr(i,k,2))
            rslopec2(i,k) = rslopec(i,k)*rslopec(i,k)
            rslopec3(i,k) = rslopec2(i,k)*rslopec(i,k)
          endif
          if(qrs(i,k,2).le.qcrmin)then
            rslope(i,k,2) = rslopesmax
            rslopeb(i,k,2) = rslopesbmax
            rslope2(i,k,2) = rslopes2max
            rslope3(i,k,2) = rslopes3max
          else
            rslope(i,k,2) = 1./lamdas(qrs(i,k,2),den(i,k),n0sfac(i,k))
            rslopeb(i,k,2) = exp(log(rslope(i,k,2))*(bvts))
            rslope2(i,k,2) = rslope(i,k,2)*rslope(i,k,2)
            rslope3(i,k,2) = rslope2(i,k,2)*rslope(i,k,2)
          endif





          temp = (den(i,k)*max(qci(i,k,2),qmin))
          temp = sqrt(sqrt(temp*temp*temp))
          xni(i,k) = min(max(5.38e7*temp,1.e3),1.e6)
        enddo
      enddo

      mstepmax = 1
      numndt = 1
      do k = kte, kts, -1
        do i = its, ite
          workn(i,k) = pvtrn*rslopeb(i,k,1)*denfac(i,k)/delz(i,k)
          numndt(i) = max(nint(workn(i,k)*dtcld+.5),1)
          if(numndt(i).ge.mnstep(i)) mnstep(i) = numndt(i)
        enddo
      enddo 
      do i = its, ite
        if(mstepmax.le.mnstep(i)) mstepmax = mnstep(i)
      enddo 

      do n = 1, mstepmax
        k = kte
        do i = its, ite
          if(n.le.mnstep(i)) then
            falkn(i,k) = den(i,k)*ncr(i,k,3)*workn(i,k)/mnstep(i)
            falln(i,k) = falln(i,k)+falkn(i,k)
            ncr(i,k,3) = max(ncr(i,k,3)-falkn(i,k)                             &
                          *dtcld/den(i,k),0.)
          endif
        enddo
        do k = kte-1, kts, -1
          do i = its, ite
            if(n.le.mnstep(i)) then
              falkn(i,k) = den(i,k)*ncr(i,k,3)*workn(i,k)/mnstep(i)
              falln(i,k) = falln(i,k)+falkn(i,k)
              ncr(i,k,3) = max(ncr(i,k,3)-(falkn(i,k)-falkn(i,k+1)             &
                          *delz(i,k+1)/delz(i,k))*dtcld/den(i,k),0.)
            endif
          enddo
        enddo
      enddo

      mstepmax = 1
      numdt = 1
      do k = kte, kts, -1
        do i = its, ite
          work1(i,k,1) = pvtr*rslopeb(i,k,1)*denfac(i,k)/delz(i,k)
          work1(i,k,2) = pvts*rslopeb(i,k,2)*denfac(i,k)/delz(i,k)
          numdt(i) = max(nint(max(work1(i,k,1),work1(i,k,2))*dtcld+.5),1)
          if(numdt(i).ge.mstep(i)) mstep(i) = numdt(i)
        enddo
      enddo
      do i = its, ite
        if(mstepmax.le.mstep(i)) mstepmax = mstep(i)
        rmstep(i) = 1./mstep(i)
      enddo

      do n = 1, mstepmax
        k = kte
        do i = its, ite
          if(n.le.mstep(i)) then


              falk(i,k,1) = den(i,k)*qrs(i,k,1)*work1(i,k,1)*rmstep(i)
              falk(i,k,2) = den(i,k)*qrs(i,k,2)*work1(i,k,2)*rmstep(i)
              fall(i,k,1) = fall(i,k,1)+falk(i,k,1)
              fall(i,k,2) = fall(i,k,2)+falk(i,k,2)


              dtcldden = dtcld/den(i,k)
              qrs(i,k,1) = max(qrs(i,k,1)-falk(i,k,1)*dtcldden,0.)
              qrs(i,k,2) = max(qrs(i,k,2)-falk(i,k,2)*dtcldden,0.)
            endif
          enddo
        do k = kte-1, kts, -1
          do i = its, ite
            if(n.le.mstep(i)) then


              falk(i,k,1) = den(i,k)*qrs(i,k,1)*work1(i,k,1)*rmstep(i)
              falk(i,k,2) = den(i,k)*qrs(i,k,2)*work1(i,k,2)*rmstep(i)
              fall(i,k,1) = fall(i,k,1)+falk(i,k,1)
              fall(i,k,2) = fall(i,k,2)+falk(i,k,2)




              dtcldden = dtcld/den(i,k)
              rdelz = 1./delz(i,k)
              qrs(i,k,1) = max(qrs(i,k,1)-(falk(i,k,1)-falk(i,k+1,1)           &
                          *delz(i,k+1)*rdelz)*dtcldden,0.)
              qrs(i,k,2) = max(qrs(i,k,2)-(falk(i,k,2)-falk(i,k+1,2)           &
                          *delz(i,k+1)*rdelz)*dtcldden,0.)
            endif
          enddo
        enddo
        do k = kte, kts, -1
          do i = its, ite
            if(n.le.mstep(i)) then
              if(t(i,k).gt.t0c .and. qrs(i,k,2).gt.0.) then




                xlf = xlf0

                work2(i,k)= (exp(log(((1.496e-6*((t(i,k))*sqrt(t(i,k)))        &
                            /((t(i,k))+120.)/(den(i,k)))/(8.794e-5             &
                            *exp(log(t(i,k))*(1.81))/p(i,k))))                 &
                            *((.3333333)))/sqrt((1.496e-6*((t(i,k))            &
                            *sqrt(t(i,k)))/((t(i,k))+120.)/(den(i,k))))        &
                            *sqrt(sqrt(den0/(den(i,k)))))
                coeres = rslope2(i,k,2)*sqrt(rslope(i,k,2)*rslopeb(i,k,2))



                psmlt(i,k) = (1.414e3*(1.496e-6 * ((t(i,k))*sqrt(t(i,k)))      &  
                             /((t(i,k))+120.)/(den(i,k)))*(den(i,k)))/xlf      &
                            *(t0c-t(i,k))*pi/2.*n0sfac(i,k)                    &
                            *(precs1*rslope2(i,k,2)+precs2*work2(i,k)*coeres)
                psmlt(i,k) = min(max(psmlt(i,k)*dtcld/mstep(i),-qrs(i,k,2)     &
                            /mstep(i)),0.)
                qrs(i,k,2) = qrs(i,k,2) + psmlt(i,k)
                qrs(i,k,1) = qrs(i,k,1) - psmlt(i,k)




                if(qrs(i,k,2).gt.qcrmin) then
                  sfac = rslope(i,k,2)*n0s*n0sfac(i,k)/qrs(i,k,2)
                  ncr(i,k,3) = ncr(i,k,3) - sfac*psmlt(i,k)
                endif
                t(i,k) = t(i,k) + xlf/cpm(i,k)*psmlt(i,k)
              endif
            endif
          enddo
        enddo
      enddo



      mstepmax = 1
      mstep = 1
      numdt = 1
      do k = kte, kts, -1
        do i = its, ite
          if(qci(i,k,2).le.0.) then
            work2c(i,k) = 0.
          else
            xmi = den(i,k)*qci(i,k,2)/xni(i,k)

            diameter  = max(min(dicon * sqrt(xmi),dimax), 1.e-25)
            work1c(i,k) = 1.49e4*exp(log(diameter)*(1.31))
            work2c(i,k) = work1c(i,k)/delz(i,k)
          endif
          numdt(i) = max(nint(work2c(i,k)*dtcld+.5),1)
          if(numdt(i).ge.mstep(i)) mstep(i) = numdt(i)
        enddo
      enddo
      do i = its, ite
        if(mstepmax.le.mstep(i)) mstepmax = mstep(i)
      enddo

      do n = 1, mstepmax
        k = kte
        do i = its, ite
          if(n.le.mstep(i)) then
            falkc(i,k) = den(i,k)*qci(i,k,2)*work2c(i,k)/mstep(i)
            holdc = falkc(i,k)
            fallc(i,k) = fallc(i,k)+falkc(i,k)
            holdci = qci(i,k,2)
            qci(i,k,2) = max(qci(i,k,2)-falkc(i,k)*dtcld/den(i,k),0.)
          endif
        enddo
        do k = kte-1, kts, -1
          do i = its, ite
            if(n.le.mstep(i)) then
              falkc(i,k) = den(i,k)*qci(i,k,2)*work2c(i,k)/mstep(i)
              holdc = falkc(i,k)
              fallc(i,k) = fallc(i,k)+falkc(i,k)
              holdci = qci(i,k,2)
              qci(i,k,2) = max(qci(i,k,2)-(falkc(i,k)-falkc(i,k+1)*delz(i,k+1) &      
                          /delz(i,k))*dtcld/den(i,k),0.)
            endif
          enddo
        enddo
      enddo





      do i = its, ite
        fallsum = fall(i,1,1)+fall(i,1,2)+fallc(i,1)
        fallsum_qsi = fall(i,1,2)+fallc(i,1)
        rainncv(i) = 0.
        if(fallsum.gt.0.) then
          rainncv(i) = fallsum*delz(i,1)/denr*dtcld*1000.
          rain(i) = fallsum*delz(i,1)/denr*dtcld*1000.+rain(i)
        endif
        if (PRESENT (snowncv) .and. PRESENT (snow)) then 
          snowncv(i) = 0.
          if(fallsum_qsi.gt.0.) then
            snowncv(i) = fallsum_qsi*delz(i,kts)/denr*dtcld*1000.
            snow(i) = fallsum_qsi*delz(i,kts)/denr*dtcld*1000.+snow(i)
          endif
        endif 
        sr(i) = 0.
        if(fallsum.gt.0.)sr(i)=fallsum_qsi*delz(i,kts)/denr*dtcld*1000.        &
        /(rainncv(i)+1.e-12)
      enddo





      do k = kts, kte
        do i = its, ite
          supcol = t0c-t(i,k)
          xlf = xls-xl(i,k)
          if(supcol.lt.0.) xlf = xlf0
          if(supcol.lt.0 .and. qci(i,k,2).gt.0.) then
            qci(i,k,1) = qci(i,k,1)+qci(i,k,2)




            if(qci(i,k,2).gt.qmin) then 
              ifac = xni(i,k)/qci(i,k,2)
              ncr(i,k,2) = ncr(i,k,2)+ifac*qci(i,k,2)
            endif           
            t(i,k) = t(i,k) - xlf/cpm(i,k)*qci(i,k,2)
            qci(i,k,2) = 0.
          endif




          if(supcol.gt.40. .and. qci(i,k,1).gt.0.) then
            qci(i,k,2) = qci(i,k,2) + qci(i,k,1)




            if(ncr(i,k,2).gt.0.) ncr(i,k,2) = 0.
            t(i,k) = t(i,k) + xlf/cpm(i,k)*qci(i,k,1)
            qci(i,k,1) = 0.
          endif




          if(supcol.gt.0. .and. qci(i,k,1).gt.0.) then
            supcolt=min(supcol,70.)
            pfrzdtc = min(pi*pi*pfrz1*(exp(pfrz2*supcolt)-1.)*denr/den(i,k)    &
                   *ncr(i,k,2)*rslopec3(i,k)*rslopec3(i,k)/18.*dtcld,qci(i,k,1))       




            if(ncr(i,k,2).gt.ncmin) then
              nfrzdtc = min(pi*pfrz1*(exp(pfrz2*supcolt)-1.)*ncr(i,k,2)        &
                      *rslopec3(i,k)/6.*dtcld,ncr(i,k,2))
              ncr(i,k,2) = ncr(i,k,2) - nfrzdtc
            endif
            qci(i,k,2) = qci(i,k,2) + pfrzdtc
            t(i,k) = t(i,k) + xlf/cpm(i,k)*pfrzdtc
            qci(i,k,1) = qci(i,k,1)-pfrzdtc
           endif




          if(supcol.gt.0. .and. qrs(i,k,1).gt.0.) then
            supcolt=min(supcol,70.)
            pfrzdtr = min(140.*(pi*pi)*pfrz1*ncr(i,k,3)*denr/den(i,k)          &
                  *(exp(pfrz2*supcolt)-1.)*rslope3(i,k,1)*rslope3(i,k,1)       &
                  *dtcld,qrs(i,k,1)) 




            if(ncr(i,k,3).gt.nrmin) then
              nfrzdtr = min(4.*pi*pfrz1*ncr(i,k,3)*(exp(pfrz2*supcolt)-1.)     &
                       *rslope3(i,k,1)*dtcld,ncr(i,k,3))
              ncr(i,k,3) = ncr(i,k,3)-nfrzdtr
            endif
            qrs(i,k,2) = qrs(i,k,2) + pfrzdtr
            t(i,k) = t(i,k) + xlf/cpm(i,k)*pfrzdtr
            qrs(i,k,1) = qrs(i,k,1)-pfrzdtr
          endif
        enddo
      enddo

      do k = kts, kte
        do i = its, ite
          ncr(i,k,2) = max(ncr(i,k,2),0.0)
          ncr(i,k,3) = max(ncr(i,k,3),0.0)
        enddo
      enddo









      do k = kts, kte
        do i = its, ite
          if(qrs(i,k,1).le.qcrmin .or. ncr(i,k,3).le.nrmin)then
            rslope(i,k,1) = rslopermax
            rslopeb(i,k,1) = rsloperbmax
            rslope2(i,k,1) = rsloper2max
            rslope3(i,k,1) = rsloper3max
          else
            rslope(i,k,1) = 1./lamdar(qrs(i,k,1),den(i,k),ncr(i,k,3))
            rslopeb(i,k,1) = exp(log(rslope(i,k,1))*(bvtr))
            rslope2(i,k,1) = rslope(i,k,1)*rslope(i,k,1)
            rslope3(i,k,1) = rslope2(i,k,1)*rslope(i,k,1)
          endif


          avedia(i,k,2) = rslope(i,k,1)*((24.)**(.3333333))

          if(qci(i,k,1).le.qmin .or. ncr(i,k,2).le.ncmin)then
            rslopec(i,k) = rslopecmax
            rslopec2(i,k) = rslopec2max
            rslopec3(i,k) = rslopec3max
          else
            rslopec(i,k) = 1./lamdac(qci(i,k,1),den(i,k),ncr(i,k,2))
            rslopec2(i,k) = rslopec(i,k)*rslopec(i,k)
            rslopec3(i,k) = rslopec2(i,k)*rslopec(i,k)
          endif


          avedia(i,k,1) = rslopec(i,k)

          if(qrs(i,k,2).le.qcrmin)then
            rslope(i,k,2) = rslopesmax
            rslopeb(i,k,2) = rslopesbmax
            rslope2(i,k,2) = rslopes2max
            rslope3(i,k,2) = rslopes3max
          else

            rslope(i,k,2) = 1./(sqrt(sqrt(pidn0s*(n0sfac(i,k))/((qrs(i,k,2))   &   
                           *(den(i,k))))))
            rslopeb(i,k,2) = exp(log(rslope(i,k,2))*(bvts))
            rslope2(i,k,2) = rslope(i,k,2)*rslope(i,k,2)
            rslope3(i,k,2) = rslope2(i,k,2)*rslope(i,k,2)
          endif
        enddo
      enddo

      do k = kts, kte
        do i = its, ite

          work1(i,k,1) = ((((den(i,k))*(xl(i,k))*(xl(i,k)))*((t(i,k))+120.)    &                          
                       *(den(i,k)))/(1.414e3*(1.496e-6*((t(i,k))*sqrt(t(i,k))))&
                       *(den(i,k))*(rv*(t(i,k))*(t(i,k)))))                    &
                      +  p(i,k)/((qs(i,k,1))*(8.794e-5*exp(log(t(i,k))*(1.81))))

          work1(i,k,2) = ((((den(i,k))*(xls)*(xls))*((t(i,k))+120.)*(den(i,k)))&
                       /(1.414e3*(1.496e-6*((t(i,k))*sqrt(t(i,k))))*(den(i,k)) & 
                       *(rv*(t(i,k))*(t(i,k))))                                & 
                      + p(i,k)/(qs(i,k,2)*(8.794e-5*exp(log(t(i,k))*(1.81)))))

          work2(i,k) = (exp(.3333333*log(((1.496e-6 * ((t(i,k))*sqrt(t(i,k)))) &
                     *p(i,k))/(((t(i,k))+120.)*den(i,k)*(8.794e-5              &
                     *exp(log(t(i,k))*(1.81))))))*sqrt(sqrt(den0/(den(i,k))))) & 
                      /sqrt((1.496e-6*((t(i,k))*sqrt(t(i,k))))                 &
                      /(((t(i,k))+120.)*den(i,k)))
        enddo 
      enddo 









      do k = kts, kte
        do i = its, ite
          supsat = max(q(i,k),qmin)-qs(i,k,1)
          satdt = supsat/dtcld




          lencon  = 2.7e-2*den(i,k)*qci(i,k,1)*(1.e20/16.*rslopec2(i,k)        &
                  *rslopec2(i,k)-0.4)
          lenconcr = max(1.2*lencon,qcrmin)
          if(avedia(i,k,1).gt.di15) then
            taucon = 3.7/den(i,k)/qci(i,k,1)/(0.5e6*rslopec(i,k)-7.5)
            praut(i,k) = lencon/taucon
            praut(i,k) = min(max(praut(i,k),0.),qci(i,k,1)/dtcld)




            nraut(i,k) = 3.5e9*den(i,k)*praut(i,k)
            if(qrs(i,k,1).gt.lenconcr)                                         &
            nraut(i,k) = ncr(i,k,3)/qrs(i,k,1)*praut(i,k)
            nraut(i,k) = min(nraut(i,k),ncr(i,k,2)/dtcld)
          endif






          if(qrs(i,k,1).ge.lenconcr) then
            if(avedia(i,k,2).ge.di100) then
              nracw(i,k) = min(ncrk1*ncr(i,k,2)*ncr(i,k,3)*(rslopec3(i,k)      &
                         + 24.*rslope3(i,k,1)),ncr(i,k,2)/dtcld)
              pracw(i,k) = min(pi/6.*(denr/den(i,k))*ncrk1*ncr(i,k,2)          &
                         *ncr(i,k,3)*rslopec3(i,k)*(2.*rslopec3(i,k)           &
                         + 24.*rslope3(i,k,1)),qci(i,k,1)/dtcld)
            else
              nracw(i,k) = min(ncrk2*ncr(i,k,2)*ncr(i,k,3)*(2.*rslopec3(i,k)   &
                         *rslopec3(i,k)+5040.*rslope3(i,k,1)                   &
                         *rslope3(i,k,1)),ncr(i,k,2)/dtcld)
              pracw(i,k) = min(pi/6.*(denr/den(i,k))*ncrk2*ncr(i,k,2)          &
                         *ncr(i,k,3)*rslopec3(i,k)*(6.*rslopec3(i,k)           &
                         *rslopec3(i,k)+5040.*rslope3(i,k,1)                   &
                         *rslope3(i,k,1)),qci(i,k,1)/dtcld)
            endif
          endif




          if(avedia(i,k,1).ge.di100) then
            nccol(i,k) = ncrk1*ncr(i,k,2)*ncr(i,k,2)*rslopec3(i,k)
          else
            nccol(i,k) = 2.*ncrk2*ncr(i,k,2)*ncr(i,k,2)*rslopec3(i,k)        & 
                        *rslopec3(i,k)
          endif




          if(qrs(i,k,1).ge.lenconcr) then
            if(avedia(i,k,2).lt.di100) then
              nrcol(i,k) = 5040.*ncrk2*ncr(i,k,3)*ncr(i,k,3)*rslope3(i,k,1)    &
                          *rslope3(i,k,1)
            elseif(avedia(i,k,2).ge.di100 .and. avedia(i,k,2).lt.di600) then
              nrcol(i,k) = 24.*ncrk1*ncr(i,k,3)*ncr(i,k,3)*rslope3(i,k,1)
            elseif(avedia(i,k,2).ge.di600 .and. avedia(i,k,2).lt.di2000) then
              coecol = -2.5e3*(avedia(i,k,2)-di600)
              nrcol(i,k) = 24.*exp(coecol)*ncrk1*ncr(i,k,3)*ncr(i,k,3)         &
                         *rslope3(i,k,1)
            else
              nrcol(i,k) = 0.
            endif
          endif




          if(qrs(i,k,1).gt.0.) then
            coeres = rslope(i,k,1)*sqrt(rslope(i,k,1)*rslopeb(i,k,1))
            prevp(i,k) = (rh(i,k,1)-1.)*ncr(i,k,3)*(precr1*rslope(i,k,1)       &
                         +precr2*work2(i,k)*coeres)/work1(i,k,1)
            if(prevp(i,k).lt.0.) then
              prevp(i,k) = max(prevp(i,k),-qrs(i,k,1)/dtcld)
              prevp(i,k) = max(prevp(i,k),satdt/2)




              if(avedia(i,k,2).le.di82) then
                nrevp(i,k) = ncr(i,k,3)/dtcld




                prevp_s(i,k) = qrs(i,k,1)/dtcld
              endif
            else

              prevp(i,k) = min(prevp(i,k),satdt/2)
            endif
          endif
        enddo
      enddo














      rdtcld = 1./dtcld
      do k = kts, kte
        do i = its, ite
          supcol = t0c-t(i,k)
          supsat = max(q(i,k),qmin)-qs(i,k,2)
          satdt = supsat/dtcld
          ifsat = 0





          temp = (den(i,k)*max(qci(i,k,2),qmin))
          temp = sqrt(sqrt(temp*temp*temp))
          xni(i,k) = min(max(5.38e7*temp,1.e3),1.e6)
          eacrs = exp(0.07*(-supcol))

          if(supcol.gt.0) then
            if(qrs(i,k,2).gt.qcrmin .and. qci(i,k,2).gt.qmin) then
              xmi = den(i,k)*qci(i,k,2)/xni(i,k)
              diameter  = min(dicon * sqrt(xmi),dimax)
              vt2i = 1.49e4*diameter**1.31
              vt2s = pvts*rslopeb(i,k,2)*denfac(i,k)




              acrfac = 2.*rslope3(i,k,2)+2.*diameter*rslope2(i,k,2)            &
                      + diameter**2*rslope(i,k,2)
              psaci(i,k) = pi*qci(i,k,2)*eacrs*n0s*n0sfac(i,k)*abs(vt2s-vt2i)  &
                         *acrfac/4.
            endif
          endif




          if(qrs(i,k,2).gt.qcrmin .and. qci(i,k,1).gt.qmin) then
            psacw(i,k) = min(pacrc*n0sfac(i,k)*rslope3(i,k,2)*rslopeb(i,k,2)   &
                        *qci(i,k,1)*denfac(i,k),qci(i,k,1)*rdtcld)
          endif




         if(qrs(i,k,2).gt.qcrmin .and. ncr(i,k,2).gt.ncmin) then
           nsacw(i,k) = min(pacrc*n0sfac(i,k)*rslope3(i,k,2)*rslopeb(i,k,2)    &
                       *ncr(i,k,2)*denfac(i,k),ncr(i,k,2)/dtcld)
         endif
         if(supcol.le.0) then
           xlf = xlf0




              if  (qrs(i,k,2).gt.qcrmin) then
                sfac = rslope(i,k,2)*n0s*n0sfac(i,k)/qrs(i,k,2)
                nseml(i,k) = -sfac*min(max(cliq*supcol*(psacw(i,k))/xlf        &
                            ,-qrs(i,k,2)/dtcld),0.)
              endif   
         endif
         if(supcol.gt.0) then




            if(qci(i,k,2).gt.0 .and. ifsat.ne.1) then
              xmi = den(i,k)*qci(i,k,2)/xni(i,k)
              diameter = dicon * sqrt(xmi)
              pidep(i,k) = 4.*diameter*xni(i,k)*(rh(i,k,2)-1.)/work1(i,k,2)
              supice = satdt-prevp(i,k)
              if(pidep(i,k).lt.0.) then


                pidep(i,k) = max(max(pidep(i,k),satdt*.5),supice)
                pidep(i,k) = max(pidep(i,k),-qci(i,k,2)*rdtcld)
              else

                pidep(i,k) = min(min(pidep(i,k),satdt*.5),supice)
              endif
              if(abs(prevp(i,k)+pidep(i,k)).ge.abs(satdt)) ifsat = 1
            endif




            if(qrs(i,k,2).gt.0. .and. ifsat.ne.1) then
              coeres = rslope2(i,k,2)*sqrt(rslope(i,k,2)*rslopeb(i,k,2))
              psdep(i,k) = (rh(i,k,2)-1.)*n0sfac(i,k)*(precs1*rslope2(i,k,2)   & 
                           + precs2*work2(i,k)*coeres)/work1(i,k,2)
              supice = satdt-prevp(i,k)-pidep(i,k)
              if(psdep(i,k).lt.0.) then


                psdep(i,k) = max(psdep(i,k),-qrs(i,k,2)*rdtcld)
                psdep(i,k) = max(max(psdep(i,k),satdt*.5),supice)
              else

                psdep(i,k) = min(min(psdep(i,k),satdt*.5),supice)
              endif
              if(abs(prevp(i,k)+pidep(i,k)+psdep(i,k)).ge.abs(satdt)) ifsat = 1
            endif




            if(supsat.gt.0 .and. ifsat.ne.1) then
              supice = satdt-prevp(i,k)-pidep(i,k)-psdep(i,k)
              xni0 = 1.e3*exp(0.1*supcol)
              roqi0 = 4.92e-11*exp(log(xni0)*(1.33))
              pigen(i,k) = max(0.,(roqi0/den(i,k)-max(qci(i,k,2),0.))*rdtcld)
              pigen(i,k) = min(min(pigen(i,k),satdt),supice)
            endif





            if(qci(i,k,2).gt.0.) then
              qimax = roqimax/den(i,k)

              psaut(i,k) = max(0.,(qci(i,k,2)-qimax)*rdtcld)
            endif
          endif




          if(supcol.lt.0.) then
            if(qrs(i,k,2).gt.0. .and. rh(i,k,1).lt.1.)                         &
              psevp(i,k) = psdep(i,k)*work1(i,k,2)/work1(i,k,1)

              psevp(i,k) = min(max(psevp(i,k),-qrs(i,k,2)*rdtcld),0.)
          endif
        enddo
      enddo






      do k = kts, kte
        do i = its, ite
          if(t(i,k).le.t0c) then



            value = max(qmin,qci(i,k,1))
            source = (praut(i,k)+pracw(i,k)+psacw(i,k)-prevp_s(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              praut(i,k) = praut(i,k)*factor
              pracw(i,k) = pracw(i,k)*factor
              psacw(i,k) = psacw(i,k)*factor
              prevp_s(i,k) = prevp_s(i,k)*factor
            endif



            value = max(qmin,qci(i,k,2))
            source = (psaut(i,k)+psaci(i,k)-pigen(i,k)-pidep(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              psaut(i,k) = psaut(i,k)*factor
              psaci(i,k) = psaci(i,k)*factor
              pigen(i,k) = pigen(i,k)*factor
              pidep(i,k) = pidep(i,k)*factor
            endif




            value = max(qmin,qrs(i,k,1))
            source = (-praut(i,k)-pracw(i,k)-prevp(i,k)+prevp_s(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              praut(i,k) = praut(i,k)*factor
              pracw(i,k) = pracw(i,k)*factor
              prevp(i,k) = prevp(i,k)*factor
              prevp_s(i,k) = prevp_s(i,k)*factor
            endif



            value = max(qmin,qrs(i,k,2))
            source = (-psdep(i,k)-psaut(i,k)-psaci(i,k)-psacw(i,k))*dtcld  
            if (source.gt.value) then
              factor = value/source
              psdep(i,k) = psdep(i,k)*factor
              psaut(i,k) = psaut(i,k)*factor
              psaci(i,k) = psaci(i,k)*factor
              psacw(i,k) = psacw(i,k)*factor
            endif



            value = max(ncmin,ncr(i,k,2))
            source = (+nraut(i,k)+nccol(i,k)+nracw(i,k)+nsacw(i,k)             &   
                    -nrevp(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              nraut(i,k) = nraut(i,k)*factor
              nccol(i,k) = nccol(i,k)*factor
              nracw(i,k) = nracw(i,k)*factor
              nsacw(i,k) = nsacw(i,k)*factor
              nrevp(i,k) = nrevp(i,k)*factor
            endif



            value = max(nrmin,ncr(i,k,3))
            source = (-nraut(i,k)+nrcol(i,k)+nrevp(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              nraut(i,k) = nraut(i,k)*factor
              nrcol(i,k) = nrcol(i,k)*factor
              nrevp(i,k) = nrevp(i,k)*factor
            endif

            work2(i,k)=-(prevp(i,k)+psdep(i,k)+pigen(i,k)+pidep(i,k))

            q(i,k) = q(i,k)+work2(i,k)*dtcld
            qci(i,k,1) = max(qci(i,k,1)-(praut(i,k)+pracw(i,k)+psacw(i,k)      &  
                        +prevp_s(i,k))*dtcld,0.)
            qrs(i,k,1) = max(qrs(i,k,1)+(praut(i,k)+pracw(i,k)+prevp(i,k)      &    
                        -prevp_s(i,k))*dtcld,0.)
            qci(i,k,2) = max(qci(i,k,2)-(psaut(i,k)+psaci(i,k)-pigen(i,k)      & 
                        -pidep(i,k))*dtcld,0.)
            qrs(i,k,2) = max(qrs(i,k,2)+(psdep(i,k)+psaut(i,k)+psaci(i,k)      &
                        +psacw(i,k))*dtcld,0.)
            ncr(i,k,2) = max(ncr(i,k,2)+(-nraut(i,k)-nccol(i,k)-nracw(i,k)     &
                         -nsacw(i,k)+nrevp(i,k))*dtcld,0.)
            ncr(i,k,3) = max(ncr(i,k,3)+(nraut(i,k)-nrcol(i,k)-nrevp(i,k))     &
                        *dtcld,0.)
            xlf = xls-xl(i,k)
            xlwork2 = -xls*(psdep(i,k)+pidep(i,k)+pigen(i,k))                  &
                      -xl(i,k)*prevp(i,k)-xlf*psacw(i,k)
            t(i,k) = t(i,k)-xlwork2/cpm(i,k)*dtcld
          else



            value = max(qmin,qci(i,k,1))
            source=(praut(i,k)+pracw(i,k)+psacw(i,k)-prevp_s(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              praut(i,k) = praut(i,k)*factor
              pracw(i,k) = pracw(i,k)*factor
              psacw(i,k) = psacw(i,k)*factor
              prevp_s(i,k) = prevp_s(i,k)*factor
            endif



            value = max(qmin,qrs(i,k,1))
            source = (-praut(i,k)-pracw(i,k)-prevp(i,k)+prevp_s(i,k)           &         
                    -psacw(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              praut(i,k) = praut(i,k)*factor
              pracw(i,k) = pracw(i,k)*factor
              prevp(i,k) = prevp(i,k)*factor
              psacw(i,k) = psacw(i,k)*factor
              prevp_s(i,k) = prevp_s(i,k)*factor
            endif  



            value = max(qcrmin,qrs(i,k,2))
            source=(-psevp(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              psevp(i,k) = psevp(i,k)*factor
            endif



            value = max(ncmin,ncr(i,k,2))
            source = (+nraut(i,k)+nccol(i,k)+nracw(i,k)+nsacw(i,k)             &
                     -nrevp(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              nraut(i,k) = nraut(i,k)*factor
              nccol(i,k) = nccol(i,k)*factor
              nracw(i,k) = nracw(i,k)*factor
              nsacw(i,k) = nsacw(i,k)*factor
              nrevp(i,k) = nrevp(i,k)*factor
            endif



            value = max(nrmin,ncr(i,k,3))
            source = (-nraut(i,k)-nseml(i,k)+nrcol(i,k)+nrevp(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              nraut(i,k) = nraut(i,k)*factor
              nseml(i,k) = nseml(i,k)*factor
              nrcol(i,k) = nrcol(i,k)*factor
              nrevp(i,k) = nrevp(i,k)*factor
            endif
            work2(i,k)=-(prevp(i,k)+psevp(i,k))

            q(i,k) = q(i,k)+work2(i,k)*dtcld
            qci(i,k,1) = max(qci(i,k,1)-(praut(i,k)+pracw(i,k)+psacw(i,k)      &         
                        +prevp_s(i,k))*dtcld,0.)
            qrs(i,k,1) = max(qrs(i,k,1)+(praut(i,k)+pracw(i,k)+prevp(i,k)      & 
                        +psacw(i,k)-prevp_s(i,k))*dtcld,0.)
            qrs(i,k,2) = max(qrs(i,k,2)+psevp(i,k)*dtcld,0.)
            ncr(i,k,2) = max(ncr(i,k,2)+(-nraut(i,k)-nccol(i,k)-nracw(i,k)     &
                        -nsacw(i,k)+nrevp(i,k))*dtcld,0.)
            ncr(i,k,3) = max(ncr(i,k,3)+(nraut(i,k)+nseml(i,k)-nrcol(i,k)      &
                          -nrevp(i,k))*dtcld,0.)
            xlf = xls-xl(i,k)
            xlwork2 = -xl(i,k)*(prevp(i,k)+psevp(i,k))
            t(i,k) = t(i,k)-xlwork2/cpm(i,k)*dtcld
          endif
        enddo
      enddo




      hsub = xls
      hvap = xlv0
      cvap = cpv
      ttp=t0c+0.01
      dldt=cvap-cliq
      xa=-dldt/rv
      xb=xa+hvap/(rv*ttp)
      dldti=cvap-cice
      xai=-dldti/rv
      xbi=xai+hsub/(rv*ttp)
      do k = kts, kte
      do i = its, ite
          tr=ttp/t(i,k)
          logtr = log(tr)
          qs(i,k,1)=psat*exp(logtr*(xa)+xb*(1.-tr))
          qs(i,k,1) = ep2 * qs(i,k,1) / (p(i,k) - qs(i,k,1))
          qs(i,k,1) = max(qs(i,k,1),qmin)
          rh(i,k,1) = max(q(i,k) / qs(i,k,1),qmin)
        enddo
      enddo

      do k = kts, kte
        do i = its, ite



          if(ncr(i,k,1).eq.0.) ncr(i,k,1) = ccn0




          if(rh(i,k,1).gt.1.) then
            ncact(i,k) = max(0.,((ncr(i,k,1)+ncr(i,k,2))                       &
                       *min(1.,(rh(i,k,1)/satmax)**actk) - ncr(i,k,2)))/dtcld
            ncact(i,k) =min(ncact(i,k),max(ncr(i,k,1),0.)/dtcld)
            pcact(i,k) = min(4.*pi*denr*(actr*1.E-6)**3*ncact(i,k)/            &
                         (3.*den(i,k)),max(q(i,k),0.)/dtcld)
            q(i,k) = max(q(i,k)-pcact(i,k)*dtcld,0.)
            qci(i,k,1) = max(qci(i,k,1)+pcact(i,k)*dtcld,0.)
            ncr(i,k,1) = max(ncr(i,k,1)-ncact(i,k)*dtcld,0.)
            ncr(i,k,2) = max(ncr(i,k,2)+ncact(i,k)*dtcld,0.)
            t(i,k) = t(i,k)+pcact(i,k)*xl(i,k)/cpm(i,k)*dtcld
          endif




          tr=ttp/t(i,k)
          qs(i,k,1)=psat*exp(log(tr)*(xa))*exp(xb*(1.-tr))
          qs(i,k,1) = ep2 * qs(i,k,1) / (p(i,k) - qs(i,k,1))
          qs(i,k,1) = max(qs(i,k,1),qmin)
          work1(i,k,1) = ((max(q(i,k),qmin)-(qs(i,k,1)))/(1.+(xl(i,k))         &
                       *(xl(i,k))/(rv*(cpm(i,k)))*(qs(i,k,1))/((t(i,k))        &
                       *(t(i,k)))))
          work2(i,k) = qci(i,k,1)+work1(i,k,1)
          pcond(i,k) = min(max(work1(i,k,1)/dtcld,0.),max(q(i,k),0.)/dtcld)
          if(qci(i,k,1).gt.0. .and. work1(i,k,1).lt.0.)                        &
            pcond(i,k) = max(work1(i,k,1),-qci(i,k,1))/dtcld



          if(pcond(i,k).eq.-qci(i,k,1)/dtcld) then
            ncr(i,k,2) = 0.
            ncr(i,k,1) = ncr(i,k,1)+ncr(i,k,2)
          endif

          q(i,k) = q(i,k)-pcond(i,k)*dtcld
          qci(i,k,1) = max(qci(i,k,1)+pcond(i,k)*dtcld,0.)
          t(i,k) = t(i,k)+pcond(i,k)*xl(i,k)/cpm(i,k)*dtcld
        enddo
      enddo




      do k = kts, kte
        do i = its, ite
          if(qci(i,k,1).le.qmin) qci(i,k,1) = 0.0
          if(qci(i,k,2).le.qmin) qci(i,k,2) = 0.0
        enddo
      enddo
      enddo                  
  END SUBROUTINE wdm52d

      REAL FUNCTION rgmma(x)

  IMPLICIT NONE


      REAL :: euler
      PARAMETER (euler=0.577215664901532)
      REAL :: x, y
      INTEGER :: i
      if(x.eq.1.)then
        rgmma=0.
          else
        rgmma=x*exp(euler*x)
        do i=1,10000
          y=float(i)
          rgmma=rgmma*(1.000+x/y)*exp(-x/y)
        enddo
        rgmma=1./rgmma
      endif
      END FUNCTION rgmma


      REAL FUNCTION fpvs(t,ice,rd,rv,cvap,cliq,cice,hvap,hsub,psat,t0c)

      IMPLICIT NONE

      REAL t,rd,rv,cvap,cliq,cice,hvap,hsub,psat,t0c,dldt,xa,xb,dldti,         &
           xai,xbi,ttp,tr
      INTEGER ice

      ttp=t0c+0.01
      dldt=cvap-cliq
      xa=-dldt/rv
      xb=xa+hvap/(rv*ttp)
      dldti=cvap-cice
      xai=-dldti/rv
      xbi=xai+hsub/(rv*ttp)
      tr=ttp/t
      if(t.lt.ttp .and. ice.eq.1) then
        fpvs=psat*exp(log(tr)*(xai))*exp(xbi*(1.-tr))
      else
        fpvs=psat*exp(log(tr)*(xa))*exp(xb*(1.-tr))
      endif

      END FUNCTION fpvs

  SUBROUTINE wdm5init(den0,denr,dens,cl,cpv,ccn0,allowed_to_read)

  IMPLICIT NONE


   REAL, INTENT(IN) :: den0,denr,dens,cl,cpv,ccn0
   LOGICAL, INTENT(IN) :: allowed_to_read
   REAL :: pi

   pi = 4.*atan(1.)
   xlv1 = cl-cpv

   qc0  = 4./3.*pi*denr*r0**3*xncr/den0  
   qck1 = .104*9.8*peaut/(xncr*denr)**(1./3.)/xmyu*den0**(4./3.) 
   pidnc = pi*denr/6.

   bvtr1 = 1.+bvtr
   bvtr2 = 2.+bvtr
   bvtr3 = 3.+bvtr
   bvtr4 = 4.+bvtr
   bvtr5 = 5.+bvtr
   bvtr7 = 7.+bvtr
   bvtr2o5 = 2.5+.5*bvtr
   bvtr3o5 = 3.5+.5*bvtr
   g1pbr = rgmma(bvtr1)
   g2pbr = rgmma(bvtr2)
   g3pbr = rgmma(bvtr3)
   g4pbr = rgmma(bvtr4)            
   g5pbr = rgmma(bvtr5)
   g7pbr = rgmma(bvtr7)
   g5pbro2 = rgmma(bvtr2o5)
   g7pbro2 = rgmma(bvtr3o5)
   pvtr = avtr*g5pbr/24.
   pvtrn = avtr*g2pbr
   eacrr = 1.0
   pacrr = pi*n0r*avtr*g3pbr*.25*eacrr
   precr1 = 2.*pi*1.56
   precr2 = 2.*pi*.31*avtr**.5*g7pbro2
   pidn0r =  pi*denr*n0r
   pidnr = 4.*pi*denr
   xmmax = (dimax/dicon)**2
   roqimax = 2.08e22*dimax**8

   bvts1 = 1.+bvts
   bvts2 = 2.5+.5*bvts
   bvts3 = 3.+bvts
   bvts4 = 4.+bvts
   g1pbs = rgmma(bvts1)    
   g3pbs = rgmma(bvts3)
   g4pbs = rgmma(bvts4)    
   g5pbso2 = rgmma(bvts2)
   pvts = avts*g4pbs/6.
   pacrs = pi*n0s*avts*g3pbs*.25
   precs1 = 4.*n0s*.65
   precs2 = 4.*n0s*.44*avts**.5*g5pbso2
   pidn0s =  pi*dens*n0s
   pacrc = pi*n0s*avts*g3pbs*.25*eacrc

   rslopecmax = 1./lamdacmax
   rslopermax = 1./lamdarmax
   rslopesmax = 1./lamdasmax
   rsloperbmax = rslopermax ** bvtr
   rslopesbmax = rslopesmax ** bvts
   rslopec2max = rslopecmax * rslopecmax
   rsloper2max = rslopermax * rslopermax
   rslopes2max = rslopesmax * rslopesmax
   rslopec3max = rslopec2max * rslopecmax
   rsloper3max = rsloper2max * rslopermax
   rslopes3max = rslopes2max * rslopesmax

  END SUBROUTINE wdm5init
END MODULE module_mp_wdm5
