










MODULE module_bl_mynn

  USE module_model_constants, only: &
       &karman, g, p1000mb, &
       &cp, r_d, rcp, xlv, &
       &svp1, svp2, svp3, svpt0, ep_1, ep_2

  IMPLICIT NONE


  REAL, PARAMETER :: cphm_st=5.0, cphm_unst=16.0, &
                     cphh_st=5.0, cphh_unst=16.0

  REAL, PARAMETER :: xlvcp=xlv/cp, ev=xlv, rd=r_d, rk=cp/rd, &
       &svp11=svp1*1.e3, p608=ep_1, ep_3=1.-ep_2

  REAL, PARAMETER :: tref=300.0    
  REAL, PARAMETER :: tv0=p608*tref, tv1=(1.+p608)*tref, gtr=g/tref


  REAL, PARAMETER :: &
       &vk  = karman, &
       &pr  =  0.74, &
       &g1  =  0.235, &
       &b1  = 24.0, &
       &b2  = 15.0, &
       &c2  =  0.75, &
       &c3  =  0.352, &
       &c4  =  0.0, &
       &c5  =  0.2, &
       &a1  = b1*( 1.0-3.0*g1 )/6.0, &

       &c1  = g1 -1.0/( 3.0*a1*2.88449914061481660), &
       &a2  = a1*( g1-c1 )/( g1*pr ), &
       &g2  = b2/b1*( 1.0-c3 ) +2.0*a1/b1*( 3.0-2.0*c2 )

  REAL, PARAMETER :: &
       &cc2 =  1.0-c2, &
       &cc3 =  1.0-c3, &
       &e1c =  3.0*a2*b2*cc3, &
       &e2c =  9.0*a1*a2*cc2, &
       &e3c =  9.0*a2*a2*cc2*( 1.0-c5 ), &
       &e4c = 12.0*a1*a2*cc2, &
       &e5c =  6.0*a1*a1


  REAL, PARAMETER :: qmin=0.0, zmax=1.0, cns=2.7, &
       &alp1=0.23, alp2=1.0, alp3=5.0, alp4=100.0



  REAL, PARAMETER :: gno=4.64158883361278196
  REAL, PARAMETER :: gpw=5./3., qcgmin=1.e-8,qkemin=1.e-12


  REAL, PARAMETER :: rr2=0.7071068, rrp=0.3989423

  INTEGER :: mynn_level

CONTAINS

































































































































  SUBROUTINE  mym_initialize ( kts,kte,&
       &            dz, zw,  &
       &            u, v, thl, qw, &

       &            ust, rmo, &
       &            Qke, Tsq, Qsq, Cov)


    
    INTEGER, INTENT(IN)   :: kts,kte

    REAL, INTENT(IN)   :: ust, rmo
    REAL, DIMENSION(kts:kte), INTENT(in) :: dz
    REAL, DIMENSION(kts:kte+1), INTENT(in) :: zw
    REAL, DIMENSION(kts:kte), INTENT(in) :: u,v,thl,qw

    REAL, DIMENSION(kts:kte), INTENT(out) :: qke,tsq,qsq,cov


    REAL, DIMENSION(kts:kte) :: &
         &ql,el,pdk,pdt,pdq,pdc,dtl,dqw,dtv,&
         &gm,gh,sm,sh,qkw,vt,vq
    INTEGER :: k,l,lmax
    REAL :: phm,vkz,elq,elv,b1l,b2l,pmz=1.,phh=1.,flt=0.,flq=0.,tmpq



    DO k = kts,kte
       ql(k) = 0.0
       vt(k) = 0.0
       vq(k) = 0.0
    END DO

    CALL mym_level2 ( kts,kte,&
         &            dz,  &
         &            u, v, thl, qw, &
         &            ql, vt, vq, &
         &            dtl, dqw, dtv, gm, gh, sm, sh )



    el (kts) = 0.0
    qke(kts) = ust**2 * ( b1*pmz )**(2.0/3.0)

    phm      = phh*b2 / ( b1*pmz )**(1.0/3.0)
    tsq(kts) = phm*( flt/ust )**2
    qsq(kts) = phm*( flq/ust )**2
    cov(kts) = phm*( flt/ust )*( flq/ust )

    DO k = kts+1,kte
       vkz = vk*zw(k)
       el (k) = vkz/( 1.0 + vkz/100.0 )
       qke(k) = 0.0

       tsq(k) = 0.0
       qsq(k) = 0.0
       cov(k) = 0.0
    END DO



    lmax = 5  

    DO l = 1,lmax

       CALL mym_length ( kts,kte,&
            &            dz, zw, &
            &            rmo, flt, flq, &
            &            vt, vq, &
            &            qke, &
            &            dtv, &
            &            el, &
            &            qkw)

       DO k = kts+1,kte
          elq = el(k)*qkw(k)
          pdk(k) = elq*( sm(k)*gm (k)+&
               &sh(k)*gh (k) )
          pdt(k) = elq*  sh(k)*dtl(k)**2
          pdq(k) = elq*  sh(k)*dqw(k)**2
          pdc(k) = elq*  sh(k)*dtl(k)*dqw(k)
       END DO


       vkz = vk*0.5*dz(kts)

       elv = 0.5*( el(kts+1)+el(kts) ) /  vkz 
       qke(kts) = ust**2 * ( b1*pmz*elv    )**(2.0/3.0)

       phm      = phh*b2 / ( b1*pmz/elv**2 )**(1.0/3.0)
       tsq(kts) = phm*( flt/ust )**2
       qsq(kts) = phm*( flq/ust )**2
       cov(kts) = phm*( flt/ust )*( flq/ust )

       DO k = kts+1,kte-1
          b1l = b1*0.25*( el(k+1)+el(k) )
          tmpq=MAX(b1l*( pdk(k+1)+pdk(k) ),qkemin)

          qke(k) = tmpq**(2.0/3.0)


          IF ( qke(k) .LE. 0.0 ) THEN
             b2l = 0.0
          ELSE
             b2l = b2*( b1l/b1 ) / SQRT( qke(k) )
          END IF

          tsq(k) = b2l*( pdt(k+1)+pdt(k) )
          qsq(k) = b2l*( pdq(k+1)+pdq(k) )
          cov(k) = b2l*( pdc(k+1)+pdc(k) )
       END DO


    END DO






    qke(kte)=qke(kte-1)
    tsq(kte)=tsq(kte-1)
    qsq(kte)=qsq(kte-1)
    cov(kte)=cov(kte-1)




  END SUBROUTINE mym_initialize
  
















  SUBROUTINE  mym_level2 (kts,kte,&
       &            dz, &
       &            u, v, thl, qw, &
       &            ql, vt, vq, &
       &            dtl, dqw, dtv, gm, gh, sm, sh )


    INTEGER, INTENT(IN)   :: kts,kte
    REAL, DIMENSION(kts:kte), INTENT(in) :: dz
    REAL, DIMENSION(kts:kte), INTENT(in) :: u,v,thl,qw,ql,vt,vq


    REAL, DIMENSION(kts:kte), INTENT(out) :: &
         &dtl,dqw,dtv,gm,gh,sm,sh

    INTEGER :: k

    REAL :: rfc,f1,f2,rf1,rf2,smc,shc,&
         &ri1,ri2,ri3,ri4,duz,dtz,dqz,vtt,vqq,dtq,dzk,afk,abk,ri,rf






    rfc = g1/( g1+g2 )
    f1  = b1*( g1-c1 ) +3.0*a2*( 1.0    -c2 )*( 1.0-c5 ) &
    &                   +2.0*a1*( 3.0-2.0*c2 )
    f2  = b1*( g1+g2 ) -3.0*a1*( 1.0    -c2 )
    rf1 = b1*( g1-c1 )/f1
    rf2 = b1*  g1     /f2
    smc = a1 /a2*  f1/f2
    shc = 3.0*a2*( g1+g2 )

    ri1 = 0.5/smc
    ri2 = rf1*smc
    ri3 = 4.0*rf2*smc -2.0*ri2
    ri4 = ri2**2

    DO k = kts+1,kte
       dzk = 0.5  *( dz(k)+dz(k-1) )
       afk = dz(k)/( dz(k)+dz(k-1) )
       abk = 1.0 -afk
       duz = ( u(k)-u(k-1) )**2 +( v(k)-v(k-1) )**2
       duz =   duz                    /dzk**2
       dtz = ( thl(k)-thl(k-1) )/( dzk )
       dqz = ( qw(k)-qw(k-1) )/( dzk )

       vtt =  1.0 +vt(k)*abk +vt(k-1)*afk
       vqq =  tv0 +vq(k)*abk +vq(k-1)*afk
       dtq =  vtt*dtz +vqq*dqz

       dtl(k) =  dtz
       dqw(k) =  dqz
       dtv(k) =  dtq




       gm (k) =  duz
       gh (k) = -dtq*gtr


       ri = -gh(k)/MAX( duz, 1.0e-10 )

       rf = MIN( ri1*( ri+ri2-SQRT(ri**2-ri3*ri+ri4) ), rfc )

       sh (k) = shc*( rfc-rf )/( 1.0-rf )
       sm (k) = smc*( rf1-rf )/( rf2-rf ) * sh(k)
    END DO

    RETURN

  END SUBROUTINE mym_level2













  SUBROUTINE  mym_length ( kts,kte,&
    &            dz, zw, &
    &            rmo, flt, flq, &
    &            vt, vq, &
    &            qke, &
    &            dtv, &
    &            el, &
    &            qkw)
    
    INTEGER, INTENT(IN)   :: kts,kte
    REAL, DIMENSION(kts:kte), INTENT(in) :: dz
    REAL, DIMENSION(kts:kte+1), INTENT(in) :: zw
    REAL, INTENT(in) :: rmo,flt,flq
    REAL, DIMENSION(kts:kte), INTENT(IN) :: qke,vt,vq


    REAL, DIMENSION(kts:kte), INTENT(out) :: &
         &qkw, el
    REAL, DIMENSION(kts:kte), INTENT(in) :: dtv

    REAL :: elt,vsc

    INTEGER :: i,j,k
    REAL :: afk,abk,zwk,dzk,qdz,vflx,bv,elb,els,elf




    DO k = kts+1,kte
       afk = dz(k)/( dz(k)+dz(k-1) )
       abk = 1.0 -afk
       qkw(k) = SQRT(MAX(qke(k)*abk+qke(k-1)*&
            &afk,1.0e-10))
    END DO

    elt = 1.0e-5
    vsc = 1.0e-5


    DO k = kts+1,kte-1
       zwk = zw(k)
       dzk = 0.5*( dz(k)+dz(k-1) )
       qdz = MAX( qkw(k)-qmin, 0.0 )*dzk
             elt = elt +qdz*zwk
             vsc = vsc +qdz
    END DO

    vflx = ( vt(kts)+1.0 )*flt +( vq(kts)+tv0 )*flq
    elt =  alp1*elt/vsc
    vsc = ( gtr*elt*MAX( vflx, 0.0 ) )**(1.0/3.0)


    el(kts) = 0.0

    DO k = kts+1,kte
       zwk = zw(k)

       IF ( dtv(k) .GT. 0.0 ) THEN
          bv  = SQRT( gtr*dtv(k) )
          elb = alp2*qkw(k) / bv &
               &       *( 1.0 + alp3/alp2*&
               &SQRT( vsc/( bv*elt ) ) )
          elf=elb
          elf = alp2 * qkw(k)/bv
       ELSE
          elb = 1.0e10
          elf =elb
       END IF


       IF ( rmo .GT. 0.0 ) THEN
          els =  vk*zwk &
               &        /( 1.0 + cns*MIN( zwk*rmo, zmax ) )
       ELSE
          els =  vk*zwk &
               &  *( 1.0 - alp4*    zwk*rmo         )**0.2
       END IF

       el(k) =      MIN(elb/( elb/elt+elb/els+1.0 ),elf)


    END DO

    RETURN

  END SUBROUTINE mym_length

































  SUBROUTINE  mym_turbulence ( kts,kte,&
    &            levflag, &
    &            dz, zw, &
    &            u, v, thl, ql, qw, &
    &            qke, tsq, qsq, cov, &
    &            vt, vq,&
    &            rmo, flt, flq, &
    &            El, Dfm, Dfh, Dfq, Tcd, Qcd, Pdk, Pdt, Pdq, Pdc)


    INTEGER, INTENT(IN)   :: kts,kte
    INTEGER, INTENT(IN)   :: levflag
    REAL, DIMENSION(kts:kte), INTENT(in) :: dz
    REAL, DIMENSION(kts:kte+1), INTENT(in) :: zw
    REAL, INTENT(in) :: rmo,flt,flq   
    REAL, DIMENSION(kts:kte), INTENT(in) :: u,v,thl,qw,& 
         &ql,vt,vq,qke,tsq,qsq,cov

    REAL, DIMENSION(kts:kte), INTENT(out) :: dfm,dfh,dfq,&
         &pdk,pdt,pdq,pdc,tcd,qcd,el

    REAL, DIMENSION(kts:kte) :: qkw,dtl,dqw,dtv,gm,gh,sm,sh

    INTEGER :: k

    REAL :: e6c,dzk,afk,abk,vtt,vqq,&
         &cw25,clow,cupp,gamt,gamq,smd,gamv,elq,elh

    DOUBLE PRECISION  q2sq, t2sq, r2sq, c2sq, elsq, gmel, ghel
    DOUBLE PRECISION  q3sq, t3sq, r3sq, c3sq, dlsq, qdiv
    DOUBLE PRECISION  e1, e2, e3, e4, enum, eden, wden













    CALL mym_level2 (kts,kte,&
    &            dz, &
    &            u, v, thl, qw, &
    &            ql, vt, vq, &
    &            dtl, dqw, dtv, gm, gh, sm, sh )

    CALL mym_length (kts,kte, &
    &            dz, zw, &
    &            rmo, flt, flq, &
    &            vt, vq, &
    &            qke, &
    &            dtv, &
    &            el, &
    &            qkw)

    DO k = kts+1,kte
       dzk = 0.5  *( dz(k)+dz(k-1) )
       afk = dz(k)/( dz(k)+dz(k-1) )
       abk = 1.0 -afk
       elsq = el (k)**2
       q2sq = b1*elsq*( sm(k)*gm(k)+sh(k)*gh(k) )
       q3sq = qkw(k)**2


       gmel = gm (k)*elsq
       ghel = gh (k)*elsq



       IF ( q3sq .LT. q2sq ) THEN
          qdiv = SQRT( q3sq/q2sq )
          sm(k) = sm(k) * qdiv
          sh(k) = sh(k) * qdiv

          e1   = q3sq - e1c*ghel * qdiv**2
          e2   = q3sq - e2c*ghel * qdiv**2
          e3   = e1   + e3c*ghel * qdiv**2
          e4   = e1   - e4c*ghel * qdiv**2
          eden = e2*e4 + e3*e5c*gmel * qdiv**2
          eden = MAX( eden, 1.0d-20 )
       ELSE
          e1   = q3sq - e1c*ghel
          e2   = q3sq - e2c*ghel
          e3   = e1   + e3c*ghel
          e4   = e1   - e4c*ghel
          eden = e2*e4 + e3*e5c*gmel
          eden = MAX( eden, 1.0d-20 )

          qdiv = 1.0
          sm(k) = q3sq*a1*( e3-3.0*c1*e4       )/eden
          sh(k) = q3sq*a2*( e2+3.0*c1*e5c*gmel )/eden
       END IF


       IF ( levflag .EQ. 3 ) THEN
          t2sq = qdiv*b2*elsq*sh(k)*dtl(k)**2
          r2sq = qdiv*b2*elsq*sh(k)*dqw(k)**2
          c2sq = qdiv*b2*elsq*sh(k)*dtl(k)*dqw(k)
          t3sq = MAX( tsq(k)*abk+tsq(k-1)*afk, 0.0 )
          r3sq = MAX( qsq(k)*abk+qsq(k-1)*afk, 0.0 )
          c3sq =      cov(k)*abk+cov(k-1)*afk


          c3sq = SIGN( MIN( ABS(c3sq), SQRT(t3sq*r3sq) ), c3sq )

          vtt  = 1.0 +vt(k)*abk +vt(k-1)*afk
          vqq  = tv0 +vq(k)*abk +vq(k-1)*afk
          t2sq = vtt*t2sq +vqq*c2sq
          r2sq = vtt*c2sq +vqq*r2sq
          c2sq = MAX( vtt*t2sq+vqq*r2sq, 0.0d0 )
          t3sq = vtt*t3sq +vqq*c3sq
          r3sq = vtt*c3sq +vqq*r3sq
          c3sq = MAX( vtt*t3sq+vqq*r3sq, 0.0d0 )

          cw25 = e1*( e2 + 3.0*c1*e5c*gmel*qdiv**2 )/( 3.0*eden )


          dlsq =  elsq
          IF ( q3sq/dlsq .LT. -gh(k) ) q3sq = -dlsq*gh(k)


          e2   = q3sq - e2c*ghel * qdiv**2
          e3   = q3sq + e3c*ghel * qdiv**2
          e4   = q3sq - e4c*ghel * qdiv**2
          eden = e2*e4  + e3 *e5c*gmel * qdiv**2

          wden = cc3*gtr**2 * dlsq**2/elsq * qdiv**2 &
               &        *( e2*e4c - e3c*e5c*gmel * qdiv**2 )

          IF ( wden .NE. 0.0 ) THEN
             clow = q3sq*( 0.12-cw25 )*eden/wden
             cupp = q3sq*( 0.76-cw25 )*eden/wden

             IF ( wden .GT. 0.0 ) THEN
                c3sq  = MIN( MAX( c3sq, c2sq+clow ), c2sq+cupp )
             ELSE
                c3sq  = MAX( MIN( c3sq, c2sq+clow ), c2sq+cupp )
             END IF
          END IF

          e1   = e2 + e5c*gmel * qdiv**2
          eden = MAX( eden, 1.0d-20 )


          e6c  = 3.0*a2*cc3*gtr * dlsq/elsq



          IF ( t2sq .GE. 0.0 ) THEN
             enum = MAX( qdiv*e6c*( t3sq-t2sq ), 0.0d0 )
          ELSE
             enum = MIN( qdiv*e6c*( t3sq-t2sq ), 0.0d0 )
          ENDIF

          gamt =-e1  *enum    /eden



          IF ( r2sq .GE. 0.0 ) THEN
             enum = MAX( qdiv*e6c*( r3sq-r2sq ), 0.0d0 )
          ELSE
             enum = MIN( qdiv*e6c*( r3sq-r2sq ), 0.0d0 )
          ENDIF

          gamq =-e1  *enum    /eden



          enum = MAX( qdiv*e6c*( c3sq-c2sq ), 0.0d0)

          smd  = dlsq*enum*gtr/eden * qdiv**2 * (e3c+e4c)*a1/a2
          gamv = e1  *enum*gtr/eden


          sm(k) = sm(k) +smd


          qdiv = 1.0


       ELSE

          gamt = 0.0
          gamq = 0.0
          gamv = 0.0
       END IF

       elq = el(k)*qkw(k)
       elh = elq*qdiv

       pdk(k) = elq*( sm(k)*gm (k) &
            &                    +sh(k)*gh (k)+gamv )
       pdt(k) = elh*( sh(k)*dtl(k)+gamt )*dtl(k)
       pdq(k) = elh*( sh(k)*dqw(k)+gamq )*dqw(k)
       pdc(k) = elh*( sh(k)*dtl(k)+gamt )&
            &*dqw(k)*0.5 &
                  &+elh*( sh(k)*dqw(k)+gamq )*dtl(k)*0.5

       tcd(k) = elq*gamt
       qcd(k) = elq*gamq

       dfm(k) = elq*sm (k) / dzk
       dfh(k) = elq*sh (k) / dzk



       dfq(k) =     dfm(k)

    END DO

    dfm(kts) = 0.0
    dfh(kts) = 0.0
    dfq(kts) = 0.0
    tcd(kts) = 0.0
    qcd(kts) = 0.0

    tcd(kte) = 0.0
    qcd(kte) = 0.0


    DO k = kts,kte-1
       dzk = dz(k)
       tcd(k) = ( tcd(k+1)-tcd(k) )/( dzk )
       qcd(k) = ( qcd(k+1)-qcd(k) )/( dzk )
    END DO

    RETURN

  END SUBROUTINE mym_turbulence














































  SUBROUTINE  mym_predict (kts,kte,&
       &            levflag,  &
       &            delt,&
       &            dz, &
       &            ust, flt, flq, pmz, phh, &
       &            el, dfq, &
       &            pdk, pdt, pdq, pdc,&
       &            qke, tsq, qsq, cov)

    INTEGER, INTENT(IN)   :: kts,kte    
    INTEGER, INTENT(IN) :: levflag
    REAL, INTENT(IN) :: delt
    REAL, DIMENSION(kts:kte), INTENT(IN) :: dz, dfq,el
    REAL, DIMENSION(kts:kte), INTENT(INOUT) :: pdk, pdt, pdq, pdc
    REAL, INTENT(IN) ::  flt, flq, ust, pmz, phh
    REAL, DIMENSION(kts:kte), INTENT(INOUT) :: qke,tsq, qsq, cov
    
    INTEGER :: k,nz
    REAL, DIMENSION(kts:kte) :: qkw, bp, rp, df3q
    REAL :: vkz,pdk1,phm,pdt1,pdq1,pdc1,b1l,b2l
    REAL, DIMENSION(kts:kte) :: dtz
    REAL, DIMENSION(1:kte-kts+1) :: a,b,c,d

    nz=kte-kts+1


    vkz = vk*0.5*dz(kts)






    DO k = kts,kte

       qkw(k) = SQRT( MAX( qke(k), 0.0 ) )
       df3q(k)=3.*dfq(k)
       dtz(k)=delt/dz(k)
    END DO

    pdk1 = 2.0*ust**3*pmz/( vkz )
    phm  = 2.0/ust   *phh/( vkz )
    pdt1 = phm*flt**2
    pdq1 = phm*flq**2
    pdc1 = phm*flt*flq


    pdk(kts) = pdk1 -pdk(kts+1)




    pdt(kts) = pdt(kts+1)
    pdq(kts) = pdq(kts+1)
    pdc(kts) = pdc(kts+1)



    DO k = kts,kte-1
       b1l = b1*0.5*( el(k+1)+el(k) )
       bp(k) = 2.*qkw(k) / b1l
       rp(k) = pdk(k+1) + pdk(k) 
    END DO
    






    DO k=kts,kte-1
       a(k-kts+1)=-dtz(k)*df3q(k)
       b(k-kts+1)=1.+dtz(k)*(df3q(k)+df3q(k+1))+bp(k)*delt
       c(k-kts+1)=-dtz(k)*df3q(k+1)
       d(k-kts+1)=rp(k)*delt + qke(k)
    ENDDO








    a(nz)=-1. 
    b(nz)=1.
    c(nz)=0.
    d(nz)=0.

    CALL tridiag(nz,a,b,c,d)

    DO k=kts,kte
       qke(k)=d(k-kts+1)
    ENDDO
      

    IF ( levflag .EQ. 3 ) THEN








       DO k = kts,kte-1
          b2l = b2*0.5*( el(k+1)+el(k) )
          bp(k) = 2.*qkw(k) / b2l
          rp(k) = pdt(k+1) + pdt(k) 
       END DO
       

       






       DO k=kts,kte-1
          a(k-kts+1)=-dtz(k)*dfq(k)
          b(k-kts+1)=1.+dtz(k)*(dfq(k)+dfq(k+1))+bp(k)*delt
          c(k-kts+1)=-dtz(k)*dfq(k+1)
          d(k-kts+1)=rp(k)*delt + tsq(k)
       ENDDO








       a(nz)=-1. 
       b(nz)=1.
       c(nz)=0.
       d(nz)=0.
       
       CALL tridiag(nz,a,b,c,d)
       
       DO k=kts,kte
          tsq(k)=d(k-kts+1)
       ENDDO
       


       DO k = kts,kte-1
          b2l = b2*0.5*( el(k+1)+el(k) )
          bp(k) = 2.*qkw(k) / b2l
          rp(k) = pdq(k+1) +pdq(k) 
       END DO
       

       






       DO k=kts,kte-1
          a(k-kts+1)=-dtz(k)*dfq(k)
          b(k-kts+1)=1.+dtz(k)*(dfq(k)+dfq(k+1))+bp(k)*delt
          c(k-kts+1)=-dtz(k)*dfq(k+1)
          d(k-kts+1)=rp(k)*delt + qsq(k)
       ENDDO








       a(nz)=-1. 
       b(nz)=1.
       c(nz)=0.
       d(nz)=0.
       
       CALL tridiag(nz,a,b,c,d)
       
       DO k=kts,kte
          qsq(k)=d(k-kts+1)
       ENDDO
       


       DO k = kts,kte-1
          b2l = b2*0.5*( el(k+1)+el(k) )
          bp(k) = 2.*qkw(k) / b2l
          rp(k) = pdc(k+1) + pdc(k) 
       END DO
       

       






       DO k=kts,kte-1
          a(k-kts+1)=-dtz(k)*dfq(k)
          b(k-kts+1)=1.+dtz(k)*(dfq(k)+dfq(k+1))+bp(k)*delt
          c(k-kts+1)=-dtz(k)*dfq(k+1)
          d(k-kts+1)=rp(k)*delt + cov(k)
       ENDDO








       a(nz)=-1. 
       b(nz)=1.
       c(nz)=0.
       d(nz)=0.
       
       CALL tridiag(nz,a,b,c,d)
       
       DO k=kts,kte
          cov(k)=d(k-kts+1)
       ENDDO
       
    ELSE

       DO k = kts,kte-1
          IF ( qkw(k) .LE. 0.0 ) THEN
             b2l = 0.0
          ELSE
             b2l = b2*0.25*( el(k+1)+el(k) )/qkw(k)
          END IF

          tsq(k) = b2l*( pdt(k+1)+pdt(k) )
          qsq(k) = b2l*( pdq(k+1)+pdq(k) )
          cov(k) = b2l*( pdc(k+1)+pdc(k) )
       END DO
       




       tsq(kte)=tsq(kte-1)
       qsq(kte)=qsq(kte-1)
       cov(kte)=cov(kte-1)
      
    END IF

  END SUBROUTINE mym_predict
  






























  SUBROUTINE  mym_condensation (kts,kte, &
    &            levflag,  &
    &            dz, &
    &            thl, qw, &
    &            p,exner, &
    &            tsq, qsq, cov, &
    &            Vt, Vq)

    INTEGER, INTENT(IN)   :: kts,kte
    INTEGER, INTENT(in) :: levflag

    REAL, DIMENSION(kts:kte), INTENT(IN) :: dz
    REAL, DIMENSION(kts:kte), INTENT(IN) :: p,exner, thl, qw, &
         &tsq, qsq, cov

    REAL, DIMENSION(kts:kte), INTENT(OUT) :: vt,vq
    

    REAL, DIMENSION(kts:kte) :: qmq,alp,bet,sgm,ql,cld

    DOUBLE PRECISION :: t3sq, r3sq, c3sq


    REAL :: p2a,t,esl,qsl,dqsl,q1,cld0,eq1,qll,&
         &q2p,pt,rac,qt
    INTEGER :: i,j,k

    REAL :: erf



    DO k = kts,kte-1
       p2a = exner(k)
       t  = thl(k)*p2a 










       esl=svp11*EXP(svp2*(t-svpt0)/(t-svp3))
       qsl=ep_2*esl/(p(k)-ep_3*esl)

       dqsl = qsl*ep_2*ev/( rd*t**2 )

       qmq(k) = qw(k) -qsl

       alp(k) = 1.0/( 1.0+dqsl*xlvcp )
       bet(k) = dqsl*p2a

       t3sq = MAX( tsq(k), 0.0 )
       r3sq = MAX( qsq(k), 0.0 )
       c3sq =      cov(k)
       c3sq = SIGN( MIN( ABS(c3sq), SQRT(t3sq*r3sq) ), c3sq )

       r3sq = r3sq +bet(k)**2*t3sq -2.0*bet(k)*c3sq
       sgm(k) = SQRT( MAX( r3sq, 1.0d-10 ) )
    END DO

    DO k = kts,kte-1
       q1   = qmq(k) / sgm(k)
       cld0 = 0.5*( 1.0+erf( q1*rr2 ) )



       eq1  = rrp*EXP( -0.5*q1*q1 )
       qll  = MAX( cld0*q1 + eq1, 0.0 )

       cld(k) = cld0
       ql (k) = alp(k)*sgm(k)*qll

       q2p  = xlvcp/exner( k )
       pt   = thl(k) +q2p*ql(k)
       qt   = 1.0 +p608*qw(k) -(1.+p608)*ql(k)
       rac  = alp(k)*( cld0-qll*eq1 )*( q2p*qt-(1.+p608)*pt )

       vt (k) =      qt-1.0 -rac*bet(k)
       vq (k) = p608*pt-tv0 +rac
    END DO


    cld(kte) = cld(kte-1)
    ql(kte) = ql(kte-1)
    vt(kte) = vt(kte-1)
    vq(kte) = vq(kte-1)

    RETURN

  END SUBROUTINE mym_condensation

  SUBROUTINE mynn_tendencies(kts,kte,&
       &levflag,grav_settling,&
       &delt,&
       &dz,&
       &u,v,th,qv,qc,p,exner,&
       &thl,sqv,sqc,sqw,&
       &ust,flt,flq,wspd,qcg,&
       &tsq,qsq,cov,&
       &tcd,qcd,&
       &dfm,dfh,dfq,&
       &Du,Dv,Dth,Dqv,Dqc)

    INTEGER, INTENT(in) :: kts,kte
    INTEGER, INTENT(in) :: grav_settling,levflag










    REAL, DIMENSION(kts:kte), INTENT(in) :: u,v,th,qv,qc,p,exner,&
         &dfm,dfh,dfq,dz,tsq,qsq,cov,tcd,qcd
    REAL, DIMENSION(kts:kte), INTENT(inout) :: thl,sqw,sqv,sqc
    REAL, DIMENSION(kts:kte), INTENT(out) :: du,dv,dth,dqv,dqc
    REAL, INTENT(IN) :: delt,ust,flt,flq,wspd,qcg







    REAL, DIMENSION(kts:kte) :: dtz,vt,vq

    REAL, DIMENSION(1:kte-kts+1) :: a,b,c,d

    REAL :: rhs,gfluxm,gfluxp,dztop
    INTEGER :: k,kk,nz

    nz=kte-kts+1

    dztop=.5*(dz(kte)+dz(kte-1))

    DO k=kts,kte
       dtz(k)=delt/dz(k)
    ENDDO


   
    k=kts

    a(1)=0.
    b(1)=1.+dtz(k)*(dfm(k+1)+ust**2/wspd)
    c(1)=-dtz(k)*dfm(k+1)
    d(1)=u(k)





    
    DO k=kts+1,kte-1
       kk=k-kts+1
       a(kk)=-dtz(k)*dfm(k)
       b(kk)=1.+dtz(k)*(dfm(k)+dfm(k+1))
       c(kk)=-dtz(k)*dfm(k+1)
       d(kk)=u(k)
    ENDDO

















    a(nz)=0
    b(nz)=1.
    c(nz)=0.
    d(nz)=u(kte)

    CALL tridiag(nz,a,b,c,d)
    
    DO k=kts,kte
       du(k)=(d(k-kts+1)-u(k))/delt
    ENDDO



    k=kts

    a(1)=0.
    b(1)=1.+dtz(k)*(dfm(k+1)+ust**2/wspd)
    c(1)=-dtz(k)*dfm(k+1)
    d(1)=v(k)






    DO k=kts+1,kte-1
       kk=k-kts+1
       a(kk)=-dtz(k)*dfm(k)
       b(kk)=1.+dtz(k)*(dfm(k)+dfm(k+1))
       c(kk)=-dtz(k)*dfm(k+1)
       d(kk)=v(k)
    ENDDO


















    a(nz)=0
    b(nz)=1.
    c(nz)=0.
    d(nz)=v(kte)

    CALL tridiag(nz,a,b,c,d)
    
    DO k=kts,kte
       dv(k)=(d(k-kts+1)-v(k))/delt
    ENDDO



    k=kts

    a(1)=0.
    b(1)=1.+dtz(k)*dfh(k+1)
    c(1)=-dtz(k)*dfh(k+1)
    


    IF (qcg < qcgmin) THEN
       IF (sqc(k) > qcgmin) THEN
          gfluxm=grav_settling*gno*sqc(k)**gpw
       ELSE
          gfluxm=0.
       ENDIF
    ELSE
       gfluxm=grav_settling*gno*(qcg/(1.+qcg))**gpw
    ENDIF

    IF (.5*(sqc(k+1)+sqc(k)) > qcgmin) THEN
       gfluxp=grav_settling*gno*(.5*(sqc(k+1)+sqc(k)))**gpw
    ELSE
       gfluxp=0.
    ENDIF

    rhs=-xlvcp/exner(k)&
         &*( &
         &(gfluxp - gfluxm)/dz(k)&
         & ) + tcd(k)

    d(1)=thl(k)+dtz(k)*flt+rhs*delt
    
    DO k=kts+1,kte-1
       kk=k-kts+1
       a(kk)=-dtz(k)*dfh(k)
       b(kk)=1.+dtz(k)*(dfh(k)+dfh(k+1)) 
       c(kk)=-dtz(k)*dfh(k+1)

       IF (.5*(sqc(k+1)+sqc(k)) > qcgmin) THEN
          gfluxp=grav_settling*gno*(.5*(sqc(k+1)+sqc(k)))**gpw
       ELSE
          gfluxp=0.
       ENDIF
       
       IF (.5*(sqc(k-1)+sqc(k)) > qcgmin) THEN
          gfluxm=grav_settling*gno*(.5*(sqc(k-1)+sqc(k)))**gpw
       ELSE
          gfluxm=0.
       ENDIF

       rhs=-xlvcp/exner(k)&
            &*( &
            &(gfluxp - gfluxm)/dz(k)&
            & ) + tcd(k)
       d(kk)=thl(k)+rhs*delt
    ENDDO







 











    a(nz)=0.
    b(nz)=1.
    c(nz)=0.
    d(nz)=thl(kte)

    CALL tridiag(nz,a,b,c,d)
    
    DO k=kts,kte
       thl(k)=d(k-kts+1)
    ENDDO



    k=kts
  
    a(1)=0.
    b(1)=1.+dtz(k)*dfh(k+1)
    c(1)=-dtz(k)*dfh(k+1)
    
    IF (qcg < qcgmin) THEN
       IF (sqc(k) > qcgmin) THEN
          gfluxm=grav_settling*gno*sqc(k)**gpw
       ELSE
          gfluxm=0.
       ENDIF
    ELSE
       gfluxm=grav_settling*gno*(qcg/(1.+qcg))**gpw
    ENDIF
    
    IF (.5*(sqc(k+1)+sqc(k)) > qcgmin) THEN
       gfluxp=grav_settling*gno*(.5*(sqc(k+1)+sqc(k)))**gpw
    ELSE
       gfluxp=0.
    ENDIF

    rhs=&
         &( &
         &(gfluxp - gfluxm)/dz(k)& 
        & ) + qcd(k)
    
    d(1)=sqw(k)+dtz(k)*flq+rhs*delt
    
    DO k=kts+1,kte-1
       kk=k-kts+1
       a(kk)=-dtz(k)*dfh(k)
       b(kk)=1.+dtz(k)*(dfh(k)+dfh(k+1)) 
       c(kk)=-dtz(k)*dfh(k+1)

       IF (.5*(sqc(k+1)+sqc(k)) > qcgmin) THEN
          gfluxp=grav_settling*gno*(.5*(sqc(k+1)+sqc(k)))**gpw
       ELSE
          gfluxp=0.
       ENDIF

       IF (.5*(sqc(k-1)+sqc(k)) > qcgmin) THEN
          gfluxm=grav_settling*gno*(.5*(sqc(k-1)+sqc(k)))**gpw
       ELSE
          gfluxm=0.
       ENDIF

       rhs=&
            &( &
            &(gfluxp - gfluxm)/dz(k)&
            & ) + qcd(k)
       d(kk)=sqw(k) + rhs*delt
    ENDDO








 










    a(nz)=0.
    b(nz)=1.
    c(nz)=0.
    d(nz)=sqw(kte)

    CALL tridiag(nz,a,b,c,d)


    
    DO k=kts,kte
       sqw(k)=d(k-kts+1)
       sqv(k)=sqw(k)-sqc(k)
       Dqv(k)=(sqv(k)/(1.-sqv(k))-qv(k))/delt

       Dqc(k)=0.
       Dth(k)=(thl(k)+xlvcp/exner(k)*sqc(k)-th(k))/delt
    ENDDO

  END SUBROUTINE mynn_tendencies

  SUBROUTINE retrieve_exchange_coeffs(kts,kte,&
       &dfm,dfh,dfq,dz,&
       &K_m,K_h,K_q)

    INTEGER , INTENT(in) :: kts,kte

    REAL, DIMENSION(KtS:KtE), INTENT(in) :: dz,dfm,dfh,dfq

    REAL, DIMENSION(KtS:KtE), INTENT(out) :: &
         &K_m, K_h, K_q


    INTEGER :: k
    REAL :: dzk

    K_m(kts)=0.
    K_h(kts)=0.
    K_q(kts)=0.

    DO k=kts+1,kte
       dzk = 0.5  *( dz(k)+dz(k-1) )
       K_m(k)=dfm(k)*dzk
       K_h(k)=dfh(k)*dzk
       K_q(k)=dfq(k)*dzk
    ENDDO

  END SUBROUTINE retrieve_exchange_coeffs


  SUBROUTINE tridiag(n,a,b,c,d)





 
    
    INTEGER, INTENT(in):: n
    REAL, DIMENSION(n), INTENT(in) :: a,b
    REAL, DIMENSION(n), INTENT(inout) :: c,d
    
    INTEGER :: i
    REAL :: p
    REAL, DIMENSION(n) :: q
    
    c(n)=0.
    q(1)=-c(1)/b(1)
    d(1)=d(1)/b(1)
    
    DO i=2,n
       p=1./(b(i)+a(i)*q(i-1))
       q(i)=-c(i)*p
       d(i)=(d(i)-a(i)*d(i-1))*p
    ENDDO
    
    DO i=n-1,1,-1
       d(i)=d(i)+q(i)*d(i+1)
    ENDDO
    
  END SUBROUTINE tridiag

  SUBROUTINE mynn_bl_driver(&
       &initflag,&
       &grav_settling,&
       &delt,&
       &dz,&
       &u,v,th,qv,qc,&
       &p,exner,rho,&
       &xland,ts,qsfc,qcg,ps,&
       &ust,ch,hfx,qfx,rmol,wspd,&
       &Qke,Tsq,Qsq,Cov,&
       &Du,Dv,Dth,&
       &Dqv,Dqc,&

       &K_h,k_m&
       &,IDS,IDE,JDS,JDE,KDS,KDE                    &
       &,IMS,IME,JMS,JME,KMS,KME                    &
       &,ITS,ITE,JTS,JTE,KTS,KTE)
    
    INTEGER, INTENT(in) :: initflag
    INTEGER, INTENT(in) :: grav_settling
    
    INTEGER,INTENT(IN) :: &
         & IDS,IDE,JDS,JDE,KDS,KDE &
         &,IMS,IME,JMS,JME,KMS,KME &
         &,ITS,ITE,JTS,JTE,KTS,KTE
    







    
    REAL, INTENT(in) :: delt
    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(in) :: dz,&
         &u,v,th,qv,qc,p,exner,rho 
    REAL, DIMENSION(IMS:IME,JMS:JME), INTENT(in) :: xland,ust,&
         &ch,rmol,ts,qsfc,qcg,ps,hfx,qfx, wspd

    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(inout) :: &
         &Qke,Tsq,Qsq,Cov
    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(out) :: &
         &Du,Dv,Dth,Dqv,Dqc

    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(out) :: &
         &K_h,K_m




    INTEGER :: ITF,JTF,KTF
    INTEGER :: i,j,k
    REAL, DIMENSION(KMS:KME) :: thl,sqv,sqc,sqw,&
         &El, Dfm, Dfh, Dfq, Tcd, Qcd, Pdk, Pdt, Pdq, Pdc, Vt, Vq

    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME) :: K_q

    REAL, DIMENSION(KMS:KME+1) :: zw
    
    REAL :: cpm,sqcg,flt,flq,pmz,phh,exnerg,zet
    
    INTEGER, SAVE :: levflag
    
    JTF=MIN0(JTE,JDE-1)
    ITF=MIN0(ITE,IDE-1)
    KTF=MIN0(KTE,KDE-1)
    
    IF (initflag > 0) THEN

       levflag=mynn_level

       DO j=JTS,JTF
          DO i=ITS,ITF
             DO k=KTS,KTF
                sqv(k)=qv(i,k,j)/(1.+qv(i,k,j))
                thl(k)=th(i,k,j)

                IF (k==kts) THEN
                   zw(k)=0.
                ELSE
                   zw(k)=zw(k-1)+dz(i,k-1,j)
                ENDIF

                k_m(i,k,j)=0.
                k_h(i,k,j)=0.
                k_q(i,k,j)=0.

             ENDDO
             
             zw(ktf+1)=zw(ktf)+dz(i,ktf,j)

             CALL mym_initialize ( kts,kte,&
                  &dz(i,kts:kte,j), zw,  &
                  &u(i,kts:kte,j), v(i,kts:kte,j), &
                  &thl, sqv,&
                  &ust(i,j), rmol(i,j),&
                  &Qke(i,kts:kte,j), Tsq(i,kts:kte,j), &
                  &Qsq(i,kts:kte,j), Cov(i,kts:kte,j))

          ENDDO
       ENDDO
       
    ENDIF

    DO j=JTS,JTF
       DO i=ITS,ITF
          DO k=KTS,KTF
             sqv(k)=qv(i,k,j)/(1.+qv(i,k,j))
             sqc(k)=qc(i,k,j)/(1.+qc(i,k,j))
             sqw(k)=sqv(k)+sqc(k)
             thl(k)=th(i,k,j)-xlvcp/exner(i,k,j)*sqc(k)
             IF (k==kts) THEN
                zw(k)=0.
             ELSE
                zw(k)=zw(k-1)+dz(i,k-1,j)
             ENDIF
          ENDDO

          zw(ktf+1)=zw(ktf)+dz(i,ktf,j)          
          
          sqcg=qcg(i,j)/(1.+qcg(i,j))
          cpm=cp*(1.+0.8*qv(i,kts,j))



          exnerg=(ps(i,j)/p1000mb)**rcp
          flt = hfx(i,j)/( rho(i,kts,j)*cpm ) &
         +xlvcp*ch(i,j)*(sqc(kts)/exner(i,kts,j)-sqcg/exnerg)
          flq = qfx(i,j)/  rho(i,kts,j)       &
               -ch(i,j)*(sqc(kts)               -sqcg       )


          zet = 0.5*dz(i,kts,j)*rmol(i,j)
          if ( zet >= 0.0 ) then
            pmz = 1.0 + (cphm_st-1.0) * zet
            phh = 1.0 +  cphh_st      * zet
          else
            pmz = 1.0/    (1.0-cphm_unst*zet)**0.25 - zet
            phh = 1.0/SQRT(1.0-cphh_unst*zet)
          end if


          CALL  mym_condensation ( kts,kte,&
               &levflag,  &
               &dz(i,kts:kte,j), &
               &thl, sqw, &
               &p(i,kts:kte,j),exner(i,kts:kte,j), &
               &tsq(i,kts:kte,j), qsq(i,kts:kte,j), cov(i,kts:kte,j), &
               &Vt, Vq)
          
          CALL mym_turbulence ( kts,kte,&
               &levflag, &
               &dz(i,kts:kte,j), zw, &
               &u(i,kts:kte,j), v(i,kts:kte,j), thl, sqc, sqw, &
               &qke(i,kts:kte,j), tsq(i,kts:kte,j), &
               &qsq(i,kts:kte,j), cov(i,kts:kte,j), &
               &vt, vq,&
               &rmol(i,j), flt, flq, &
               &El, Dfm, Dfh, Dfq, Tcd, Qcd, Pdk, Pdt, Pdq, Pdc)

          
          CALL mym_predict (kts,kte,&
               &levflag,  &
               &delt,&
               &dz(i,kts:kte,j), &
               &ust(i,j), flt, flq, pmz, phh, &
               &el, dfq, &
               &pdk, pdt, pdq, pdc,&
               &Qke(i,kts:kte,j), Tsq(i,kts:kte,j), &
               &Qsq(i,kts:kte,j), Cov(i,kts:kte,j))

          CALL mynn_tendencies(kts,kte,&
               &levflag,grav_settling,&
               &delt,&
               &dz(i,kts:kte,j),&
               &u(i,kts:kte,j),v(i,kts:kte,j),&
               &th(i,kts:kte,j),qv(i,kts:kte,j),qc(i,kts:kte,j),&
               &p(i,kts:kte,j),exner(i,kts:kte,j),&
               &thl,sqv,sqc,sqw,&
               &ust(i,j),flt,flq,wspd(i,j),qcg(i,j),&
               &tsq(i,kts:kte,j),qsq(i,kts:kte,j),cov(i,kts:kte,j),&
               &tcd,qcd,&
               &dfm,dfh,dfq,&
               &Du(i,kts:kte,j),Dv(i,kts:kte,j),Dth(i,kts:kte,j),&
               &Dqv(i,kts:kte,j),Dqc(i,kts:kte,j))

          CALL retrieve_exchange_coeffs(kts,kte,&
               &dfm,dfh,dfq,dz(i,kts:kte,j),&
               &K_m(i,kts:kte,j),K_h(i,kts:kte,j),K_q(i,kts:kte,j))


       ENDDO
    ENDDO
    
  END SUBROUTINE mynn_bl_driver

  SUBROUTINE mynn_bl_init_driver(&
       &Du,Dv,Dth,&
       &Dqv,Dqc&
       &,RESTART,ALLOWED_TO_READ,LEVEL&
       &,IDS,IDE,JDS,JDE,KDS,KDE                    &
       &,IMS,IME,JMS,JME,KMS,KME                    &
       &,ITS,ITE,JTS,JTE,KTS,KTE)

    LOGICAL,INTENT(IN) :: ALLOWED_TO_READ,RESTART
    INTEGER,INTENT(IN) :: LEVEL

    INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE,                    &
         &                IMS,IME,JMS,JME,KMS,KME,                    &
         &                ITS,ITE,JTS,JTE,KTS,KTE
    
    
    REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT) :: &
         &Du,Dv,Dth,Dqv,Dqc

    INTEGER :: I,J,K,ITF,JTF,KTF
    
    JTF=MIN0(JTE,JDE-1)
    KTF=MIN0(KTE,KDE-1)
    ITF=MIN0(ITE,IDE-1)
    
    IF(.NOT.RESTART)THEN
       DO J=JTS,JTF
          DO K=KTS,KTF
             DO I=ITS,ITF
                Du(i,k,j)=0.
                Dv(i,k,j)=0.
                Dth(i,k,j)=0.
                Dqv(i,k,j)=0.
                Dqc(i,k,j)=0.
             ENDDO
          ENDDO
       ENDDO
    ENDIF

    mynn_level=level

  END SUBROUTINE mynn_bl_init_driver
  
END MODULE module_bl_mynn
