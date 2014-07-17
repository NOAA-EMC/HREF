module my_fncs_mod
   implicit none
   private
   public :: NccnFNC,SxFNC,gamma,gammaDP,gser,gammln,gammp,cfg,gamminc
   contains
 REAL FUNCTION NccnFNC(Win,Tin,Pin,CCNtype)
  IMPLICIT NONE
  real, intent(in) :: Win, Tin, Pin
  integer, intent(in) :: CCNtype
  real :: T,p,x,y,a,b,c,d,e,f,g,h,T2,T3,T4,x2,x3,x4,p2
  x= log10(Win*100.); x2= x*x; x3= x2*x; x4= x2*x2
  T= Tin - 273.15; T2= T*T; T3= T2*T; T4= T2*T2
  p= Pin*0.01; p2= p*p
  if (CCNtype==1) then
     a= 1.47e-9*T4 -6.944e-8*T3 -9.933e-7*T2 +2.7278e-4*T -6.6853e-4
     b=-1.41e-8*T4 +6.662e-7*T3 +4.483e-6*T2 -2.0479e-3*T +4.0823e-2
     c= 5.12e-8*T4 -2.375e-6*T3 +4.268e-6*T2 +3.9681e-3*T -3.2356e-1
     d=-8.25e-8*T4 +3.629e-6*T3 -4.044e-5*T2 +2.1846e-3*T +9.1227e-1
     e= 5.02e-8*T4 -1.973e-6*T3 +3.944e-5*T2 -9.0734e-3*T +1.1256e0
     f= -1.424e-6*p2 +3.631e-3*p -1.986
     g= -0.0212*x4 +0.1765*x3 -0.3770*x2 -0.2200*x +1.0081
     h= 2.47e-6*T3 -3.654e-5*T2 +2.3327e-3*T +0.1938
     y= a*x4 + b*x3 + c*x2 + d*x + e + f*g*h
     NccnFNC= 10.**min(2.,max(0.,y)) *1.e6
  else if (CCNtype==2) then
     a= 0.
     b= 0.
     c=-2.112e-9*T4 +3.9836e-8*T3 +2.3703e-6*T2 -1.4542e-4*T -0.0698
     d=-4.210e-8*T4 +5.5745e-7*T3 +1.8460e-5*T2 +9.6078e-4*T +0.7120
     e= 1.434e-7*T4 -1.6455e-6*T3 -4.3334e-5*T2 -7.6720e-3*T +1.0056
     f= 1.340e-6*p2 -3.5114e-3*p +1.9453
     g= 4.226e-3*x4 -5.6012e-3*x3 -8.7846e-2*x2 +2.7435e-2*x +0.9932
     h= 5.811e-9*T4 +1.5589e-7*T3 -3.8623e-5*T2 +1.4471e-3*T +0.1496
     y= a*x4 +b*x3 +c*x2 + d*x + e + (f*g*h)
     NccnFNC= 10.**max(0.,y) *1.e6
  else
    print*, '*** STOPPED in MODULE ### NccnFNC  *** '
    print*, '    Parameter CCNtype incorrectly specified'
    stop
  endif
 END FUNCTION NccnFNC
   real FUNCTION SxFNC(Win,Tin,Pin,Qsw,Qsi,CCNtype,WRT)
 IMPLICIT NONE
  integer, intent(IN) :: WRT
  integer, intent(IN) :: CCNtype
  real, intent(IN) :: Win, Tin, Pin, Qsw, Qsi
  real :: Si,Sw,Qv,T,p,x,a,b,c,d,f,g,h,Pcorr,T2corr,T2,T3,T4,x2,x3,x4,p2
  real, parameter :: TRPL= 273.15
  x= log10(max(Win,1.e-20)*100.); x2= x*x; x3= x2*x; x4= x2*x2
  T= Tin; T2= T*T; T3= T2*T; T4= T2*T2
  p= Pin*0.01; p2= p*p
  if (CCNtype==1) then
     a= -5.109e-7*T4 -3.996e-5*T3 -1.066e-3*T2 -1.273e-2*T +0.0659
     b= 2.014e-6*T4 +1.583e-4*T3 +4.356e-3*T2 +4.943e-2*T -0.1538
     c= -2.037e-6*T4 -1.625e-4*T3 -4.541e-3*T2 -5.118e-2*T +0.1428
     d= 3.812e-7*T4 +3.065e-5*T3 +8.795e-4*T2 +9.440e-3*T +6.14e-3
     f= -2.012e-6*p2 + 4.1913e-3*p - 1.785e0
     g= 2.832e-1*x3 -5.6990e-1*x2 +5.1105e-1*x -4.1747e-4
     h= 1.173e-6*T3 +3.2174e-5*T2 -6.8832e-4*T +6.7888e-2
     Pcorr= f*g*h
     T2corr= 0.9581-4.449e-3*T-2.016e-4*T2-3.307e-6*T3-1.725e-8*T4
  else if (CCNtype==2) then
     a= 3.80e-5*T2 +1.65e-4*T +9.88e-2
     b= -7.38e-5*T2 -2.53e-3*T -3.23e-1
     c= 8.39e-5*T2 +3.96e-3*T +3.50e-1
     d= -1.88e-6*T2 -1.33e-3*T -3.73e-2
     f= -1.9761e-6*p2 + 4.1473e-3*p - 1.771e0
     g= 0.1539*x4 -0.5575*x3 +0.9262*x2 -0.3498*x -0.1293
     h=-8.035e-9*T4+3.162e-7*T3+1.029e-5*T2-5.931e-4*T+5.62e-2
     Pcorr= f*g*h
     T2corr= 0.98888-5.0525e-4*T-1.7598e-5*T2-8.3308e-8*T3
  else
    print*, '*** STOPPED in MODULE ### SxFNC  *** '
    print*, '    Parameter CCNtype incorrectly specified'
    stop
  endif
  Sw= (a*x3 + b*x2 +c*x + d) + Pcorr
  Sw= 1. + 0.01*Sw
  Qv= Qsw*Sw
  Si= Qv/Qsi
  Si= Si*T2corr
  if (WRT.eq.1) then
     SxFNC= Sw
  else
     SxFNC= Si
  endif
  if (Win.le.0.) SxFNC= 1.
 END function SxFNC
 real FUNCTION gamma(xx)
  IMPLICIT NONE
  real, intent(IN) :: xx
  integer :: j
  real*8 :: ser,stp,tmp,x,y,cof(6),gammadp
  SAVE cof,stp
  DATA cof,stp/76.18009172947146d0,-86.50532032941677d0, &
       24.01409824083091d0,-1.231739572450155d0,.1208650973866179d-2, &
       -.5395239384953d-5,2.5066282746310005d0/
  x=dble(xx)
  y=x
  tmp=x+5.5d0
  tmp=(x+0.5d0)*log(tmp)-tmp
  ser=1.000000000190015d0
  do j=1,4
     y=y+1.d0
     ser=ser+cof(j)/y
  enddo
  gammadp=tmp+log(stp*ser/x)
  gammadp= exp(gammadp)
  gamma = sngl(gammadp)
 END FUNCTION gamma
 FUNCTION gammaDP(xx)
  IMPLICIT NONE
  DOUBLE PRECISION, INTENT(IN) :: xx
  DOUBLE PRECISION :: gammaDP
  INTEGER :: j
  DOUBLE PRECISION :: ser,stp,tmp,x,y,cof(6)
  SAVE cof,stp
  DATA cof,stp/76.18009172947146d0,-86.50532032941677d0, &
       24.01409824083091d0,-1.231739572450155d0,.1208650973866179d-2, &
       -.5395239384953d-5,2.5066282746310005d0/
  x=xx
  y=x
  tmp=x+5.5d0
  tmp=(x+0.5d0)*log(tmp)-tmp
  ser=1.000000000190015d0
  do j=1,4
     y=y+1.d0
     ser=ser+cof(j)/y
  enddo
  gammaDP=tmp+log(stp*ser/x)
  gammaDP= exp(gammaDP)
 END FUNCTION gammaDP
 SUBROUTINE gser(gamser,a,x,gln)
 implicit none
 integer :: itmax
 real :: a,gamser,gln,x,eps
 parameter (itmax=100, eps=3.e-7)
 integer :: n
 real :: ap,de1,summ
 gln=gammln(a)
 if(x.le.0.)then
    if(x.lt.0.) call wrf_error_fatal3("<stdin>",367,&
'WARNING: x <0 in gser' )
    gamser=0.
    return
 endif
 ap=a
 summ=1./a
 de1=summ
 do n=1,itmax
    ap=ap+1.
    de1=de1*x/ap
    summ=summ+de1
    if(abs(de1).lt.abs(summ)*eps) goto 1
 enddo
 call wrf_error_fatal3("<stdin>",381,&
'Warning: a too large, itmax too small in gser')
1 gamser=summ*exp(-x+a*log(x)-gln)
 return
END SUBROUTINE gser
 real FUNCTION gammln(xx)
  IMPLICIT NONE
  real, intent(IN) :: xx
  integer :: j
  real*8 :: ser,stp,tmp,x,y,cof(6)
  SAVE cof,stp
  DATA cof,stp/76.18009172947146d0,-86.50532032941677d0, &
       24.01409824083091d0,-1.231739572450155d0,.1208650973866179d-2, &
       -.5395239384953d-5,2.5066282746310005d0/
  x=dble(xx)
  y=x
  tmp=x+5.5d0
  tmp=(x+0.5d0)*log(tmp)-tmp
  ser=1.000000000190015d0
  do j=1,6
     y=y+1.d0
     ser=ser+cof(j)/y
  enddo
  gammln= sngl( tmp+log(stp*ser/x) )
 END FUNCTION gammln
 real FUNCTION gammp(a,x)
 implicit none
 real :: a,x,gammcf,gamser,gln
 if(x.lt.0..or.a.le.0.) call wrf_error_fatal3("<stdin>",438,&
'warning : bad arguments in gammq' )
 if(x.lt.a+1.)then
    call gser(gamser,a,x,gln)
    gammp=gamser
 else
    call cfg(gammcf,a,x,gln)
    gammp=1.-gammcf
 endif
 return
 END FUNCTION gammp
 SUBROUTINE cfg(gammcf,a,x,gln)
 implicit none
 integer :: i,itmax
 real :: a,gammcf,gln,x,eps,fpmin
 real :: an,b,c,d,de1,h
 parameter (itmax=100,eps=3.e-7)
 gln=gammln(a)
 b=x+1.-a
 c=1./fpmin
 d=1./b
 h=d
 do i= 1,itmax
   an=-i*(i-a)
   b=b+2.
   d=an*d+b
   if(abs(d).lt.fpmin)d=fpmin
   c=b+an/c
 if(abs(c).lt.fpmin) c=fpmin
   d=1./d
   de1=d*c
   h=h*de1
   if(abs(de1-1.).lt.eps) goto 1
 enddo
 call wrf_error_fatal3("<stdin>",485,&
'Warning: a too large, itmax too small in gcf')
1 gammcf=exp(-x+a*log(x)-gln)*h
 return
END SUBROUTINE cfg
 real FUNCTION gamminc(p,xmax)
 real :: p,xmax
 gamminc= gammp(p,xmax)*exp(gammln(p))
 end FUNCTION gamminc
end module my_fncs_mod
module my_sedi_mod
   implicit none
  private
  public :: SEDI_main_1b,SEDI_main_2,countColumns
   contains
 SUBROUTINE SEDI_main_2(QX,NX,cat,Q,T,DE,iDE,gamfact,epsQ,epsN,afx,bfx,cmx,dmx, &
                        ckQx1,ckQx2,ckQx4,LXP,ni,nk,VxMax,DxMax,dt,DZ,massFlux, &
                        ktop_sedi,GRAV,massFlux3D)
  use my_fncs_mod
  implicit none
  real, dimension(:,:), intent(inout) :: QX,NX,Q,T
  real, dimension(:), intent(out) :: massFlux
  real, optional, dimension(:,:), intent(out) :: massFlux3D
  real, dimension(:,:), intent(in) :: DE,iDE,DZ
  real, intent(in) :: epsQ,epsN,VxMax,LXP,afx,bfx,cmx,dmx,ckQx1,ckQx2,ckQx4,DxMax,dt,GRAV
  integer, dimension(:), intent(in) :: ktop_sedi
  integer, intent(in) :: ni,nk,cat
  logical :: slabHASmass,locallim,QxPresent
  integer :: nnn,a,i,k,counter,l,km1,kp1,ks,kw,idzmin
  integer, dimension(nk) :: flim_Q,flim_N
  integer, dimension(ni) :: activeColumn,npassx,ke
  real :: VqMax,VnMax,iLAMx,iLAMxB0,tmp1,tmp2,tmp3,Dx,iDxMax,icmx, &
                            VincFact,ratio_Vn2Vq,zmax_Q,zmax_N,tempo,idmx,Nos_Thompson, &
                            No_s,iLAMs
  real, dimension(ni,nk) :: VVQ,VVN,RHOQX,gamfact
  real, dimension(ni) :: dzMIN,dtx,VxMaxx
  real, dimension(nk) :: vp_Q,vp_N,zt_Q,zt_N,zb_Q,zb_N,dzi,Q_star,N_star
  real, dimension(0:nk) :: zz
  real, parameter :: epsilon = 1.e-2
  real, parameter :: thrd = 1./3.
  real, parameter :: sxth = 1./6.
  real, parameter :: CoMAX = 2.0
   massFlux = 0.
   VincFact = 1.
   if (present(massFlux3D)) massFlux3D= 0.
   call countColumns(QX,ni,nk,epsQ,counter,activeColumn,ktop_sedi)
   ratio_Vn2Vq= ckQx2/ckQx1
   iDxMax= 1./DxMax
   icmx = 1./cmx
   idmx = 1./dmx
   ks = nk
   ke = ktop_sedi
   kw = -1
   VVQ = 0.
   VVN = 0.
   VqMax= 0.
   VnMax= 0.
   DO a= 1,counter
      i= activeColumn(a)
      VVQ(i,:) = 0.
      do k= ktop_sedi(i),nk
         QxPresent = (QX(i,k)>epsQ .and. NX(i,k)>epsN)
         if (QxPresent) VVQ(i,k)= calcVV()*ckQx1
         if (present(massFlux3D)) massFlux3D(i,k)= VVQ(i,k)*DE(i,k)*QX(i,k)
      enddo
      Vxmaxx(i)= min( VxMax, maxval(VVQ(i,:))*VincFact )
      dzMIN(i) = minval(DZ(i,:))
      npassx(i)= max(1, nint( dt*Vxmaxx(i)/(CoMAX*dzMIN(i)) ))
      dtx(i) = dt/float(npassx(i))
      DO nnn= 1,npassx(i)
         locallim = (nnn==1)
         do k= ktop_sedi(i),nk
           RHOQX(i,k) = DE(i,k)*QX(i,k)
           QxPresent = (QX(i,k)>epsQ .and. NX(i,k)>epsN)
           if (QxPresent) then
              if (locallim) then
                 VVQ(i,k)= -VVQ(i,k)
              else
                 VVQ(i,k)= -calcVV()*ckQx1
              endif
              VVN(i,k)= VVQ(i,k)*ratio_Vn2Vq
              VqMax = max(VxMAX,-VVQ(i,k))
              VnMax = max(VxMAX,-VVN(i,k))
           else
              VVQ(i,k)= 0.
              VVN(i,k)= 0.
              VqMax = 0.
              VnMax = 0.
           endif
         enddo
         massFlux(i)= massFlux(i) - VVQ(i,nk)*DE(i,nk)*QX(i,nk)
         zz(ks)= 0.
         do k= ks,ke(i),kw
            zz(k+kw)= zz(k)+dz(i,k)
            dzi(k) = 1./dz(i,k)
            vp_Q(k) = 0.
            vp_N(k) = 0.
         enddo
         do k=ks,ke(i),kw
            zb_Q(k)= zz(k) + VVQ(i,k)*dtx(i)
            zb_N(k)= zz(k) + VVN(i,k)*dtx(i)
         enddo
         zt_Q(ke(i))= zb_Q(ke(i)) + dz(i,ke(i))
         zt_N(ke(i))= zb_N(ke(i)) + dz(i,ke(i))
         do k= ks,ke(i)-kw,kw
            zb_Q(k)= min(zb_Q(k+kw)-epsilon*dz(i,k), zz(k)+VVQ(i,k)*dtx(i))
            zb_N(k)= min(zb_N(k+kw)-epsilon*dz(i,k), zz(k)+VVN(i,k)*dtx(i))
            zt_Q(k)= zb_Q(k+kw)
            zt_N(k)= zb_N(k+kw)
         enddo
         do k=ks,ke(i),kw
            Q_star(k)= RHOQX(i,k)*dz(i,k)/(zt_Q(k)-zb_Q(k))
            N_star(k)= NX(i,k)*dz(i,k)/(zt_N(k)-zb_N(k))
         enddo
         if (locallim) then
            zmax_Q= abs(VqMax*dtx(i))
            zmax_N= abs(VnMax*dtx(i))
            do l=ks,ke(i),kw
               flim_Q(l)= l
               flim_N(l)= l
               do k= l,ke(i),kw
                  if (zmax_Q.ge.zz(k)-zz(l+kw)) flim_Q(l)= k
                  if (zmax_N.ge.zz(k)-zz(l+kw)) flim_N(l)= k
               enddo
            enddo
         endif
         do l=ks,ke(i),kw
            do k=l,flim_Q(l),kw
               vp_Q(l)= vp_Q(l) + Q_star(k)*max(0.,min(zz(l+kw),zt_Q(k))-max(zz(l),zb_Q(k)))
            enddo
            do k=l,flim_N(l),kw
               vp_N(l)= vp_N(l) + N_star(k)*max(0.,min(zz(l+kw),zt_N(k))-max(zz(l),zb_N(k)))
            enddo
         enddo
         do k=ks,ke(i),kw
            RHOQX(i,k)= vp_Q(k)*dzi(k)
               NX(i,k)= vp_N(k)*dzi(k)
         enddo
         do k= ktop_sedi(i),nk
           QX(i,k)= RHOQX(i,k)*iDE(i,k)
           QxPresent= (QX(i,k)>epsQ .and. NX(i,k)>epsN)
           if (QxPresent) then
              Dx= (DE(i,k)*QX(i,k)/(NX(i,k)*cmx))**idmx
              if (cat==1 .and. Dx>3.e-3) then
                 tmp1 = Dx-3.e-3; tmp1= tmp1*tmp1
                 tmp2 = (Dx/DxMAX); tmp2= tmp2*tmp2*tmp2
                 NX(i,k)= NX(i,k)*max((1.+2.e4*tmp1),tmp2)
              else
                 NX(i,k)= NX(i,k)*(max(Dx,DxMAX)*iDxMAX)**dmx
              endif
           else
              Q(i,k) = Q(i,k) + QX(i,k)
              T(i,k) = T(i,k) - LXP*QX(i,k)
              QX(i,k)= 0.
              NX(i,k)= 0.
           endif
         enddo
       ENDDO
       massFlux(i)= massFlux(i)/float(npassx(i))
    ENDDO
CONTAINS
   real function calcVV()
      iLAMx = ((QX(i,k)*DE(i,k)/NX(i,k))*ckQx4)**idmx
      iLAMxB0 = iLAMx**bfx
      calcVV = gamfact(i,k)*iLAMxB0
   end function calcVV
 END SUBROUTINE SEDI_main_2
 SUBROUTINE SEDI_main_1b(QX,cat,T,DE,iDE,gamfact,epsQ,afx,bfx,icmx,dmx,ckQx1,ckQx4, &
                         ni,nk,VxMax,DxMax,dt,DZ,massFlux,No_x,ktop_sedi,GRAV, &
                         massFlux3D)
  use my_fncs_mod
  implicit none
  real, dimension(:,:), intent(inout) :: QX,T
  real, dimension(:), intent(out) :: massFlux
  real, optional, dimension(:,:), intent(out) :: massFlux3D
  real, dimension(:,:), intent(in) :: DE,iDE,DZ
  real, intent(in) :: epsQ,VxMax,afx,bfx,icmx,dmx,ckQx1,ckQx4,DxMax,dt,GRAV,No_x
  integer, dimension(:), intent(in) :: ktop_sedi
  integer, intent(in) :: ni,nk,cat
  logical :: slabHASmass,locallim,QxPresent
  integer :: nnn,a,i,k,counter,l,km1,kp1,ks,kw,idzmin
  integer, dimension(nk) :: flim_Q
  integer, dimension(ni) :: activeColumn,npassx,ke
  real :: VqMax,iLAMx,iLAMxB0,tmp1,tmp2,Dx,iDxMax,VincFact,NX,iNo_x, &
                            zmax_Q,zmax_N,tempo
  real, dimension(ni,nk) :: VVQ,RHOQX,gamfact
  real, dimension(ni) :: dzMIN,dtx,VxMaxx
  real, dimension(nk) :: vp_Q,zt_Q,zb_Q,dzi,Q_star
  real, dimension(0:nk) :: zz
  real, parameter :: epsilon = 1.e-2
  real, parameter :: thrd = 1./3.
  real, parameter :: sxth = 1./6.
  real, parameter :: CoMAX = 2.0
   massFlux= 0.
   VincFact= 1.
   if (present(massFlux3D)) massFlux3D= 0.
   call countColumns(QX,ni,nk,epsQ,counter,activeColumn,ktop_sedi)
   iNo_x = 1./No_x
   iDxMax= 1./DxMax
   ks = nk
   ke = ktop_sedi
   kw = -1
   VVQ = 0.
   VqMax= 0.
   DO a= 1,counter
      i= activeColumn(a)
      VVQ(i,:) = 0.
      do k= ktop_sedi(i),nk
         QxPresent = (QX(i,k)>epsQ)
         if (QxPresent) then
              if (cat==2) then
                 NX = 5.*exp(0.304*(273.15-max(233.,T(i,k))))
                 iLAMx = (ckQx4*QX(i,k)*DE(i,k)/NX)**thrd
              else if (cat==3) then
                 iNo_x = 1./min(2.e+8, 2.e+6*exp(-0.12*min(-0.001,T(i,k)-273.15)))
                 iLAMx = sqrt(sqrt(QX(i,k)*DE(i,k)*icmx*sxth*iNo_x))
              else
                 iLAMx = sqrt(sqrt(QX(i,k)*DE(i,k)*icmx*sxth*iNo_x))
              endif
              VVQ(i,k) = -gamfact(i,k)*ckQx1*iLAMx**bfx
         endif
         if (present(massFlux3D)) massFlux3D(i,k)= -VVQ(i,k)*DE(i,k)*QX(i,k)
      enddo
      Vxmaxx(i)= min( VxMax, maxval(VVQ(i,:))*VincFact )
      dzMIN(i) = minval(DZ(i,:))
      npassx(i)= max(1, nint( dt*Vxmaxx(i)/(CoMAX*dzMIN(i)) ))
      dtx(i) = dt/float(npassx(i))
      DO nnn= 1,npassx(i)
         locallim = (nnn==1)
         do k= ktop_sedi(i),nk
           RHOQX(i,k) = DE(i,k)*QX(i,k)
           QxPresent = (QX(i,k)>epsQ)
            if (QxPresent) then
               if (cat==2) then
                  NX = 5.*exp(0.304*(273.15-max(233.,T(i,k))))
                  iLAMx = (ckQx4*QX(i,k)*DE(i,k)/NX)**thrd
               else if (cat==3) then
                  iNo_x = 1./min(2.e+8, 2.e+6*exp(-0.12*min(-0.001,T(i,k)-273.15)))
                  iLAMx = sqrt(sqrt(QX(i,k)*DE(i,k)*icmx*sxth*iNo_x))
               else
                  iLAMx = sqrt(sqrt(QX(i,k)*DE(i,k)*icmx*sxth*iNo_x))
               endif
               VVQ(i,k) = -gamfact(i,k)*ckQx1*iLAMx**bfx
               VqMax = max(VxMAX,-VVQ(i,k))
            endif
         enddo
         zz(ks)= 0.
         do k= ks,ke(i),kw
            zz(k+kw)= zz(k)+dz(i,k)
            dzi(k) = 1./dz(i,k)
            vp_Q(k) = 0.
         enddo
         do k=ks,ke(i),kw
            zb_Q(k)= zz(k) + VVQ(i,k)*dtx(i)
         enddo
         zt_Q(ke(i))= zb_Q(ke(i)) + dz(i,ke(i))
         do k= ks,ke(i)-kw,kw
            zb_Q(k)= min(zb_Q(k+kw)-epsilon*dz(i,k), zz(k)+VVQ(i,k)*dtx(i))
            zt_Q(k)= zb_Q(k+kw)
         enddo
         do k=ks,ke(i),kw
            Q_star(k)= RHOQX(i,k)*dz(i,k)/(zt_Q(k)-zb_Q(k))
         enddo
         if (locallim) then
            zmax_Q= abs(VqMax*dtx(i))
            do l=ks,ke(i),kw
               flim_Q(l)= l
               do k= l,ke(i),kw
                  if (zmax_Q.ge.zz(k)-zz(l+kw)) flim_Q(l)= k
               enddo
            enddo
         endif
         do l=ks,ke(i),kw
            do k=l,flim_Q(l),kw
               vp_Q(l)= vp_Q(l) + Q_star(k)*max(0.,min(zz(l+kw),zt_Q(k))-max(zz(l),zb_Q(k)))
            enddo
         enddo
         do k=ks,ke(i),kw
            RHOQX(i,k)= vp_Q(k)*dzi(k)
         enddo
         do k= ktop_sedi(i),nk
           QX(i,k)= RHOQX(i,k)*iDE(i,k)
         enddo
         massFlux(i)= massFlux(i) - VVQ(i,nk)*DE(i,nk)*QX(i,nk)
       ENDDO
       massFlux(i)= massFlux(i)/float(npassx(i))
    ENDDO
 END SUBROUTINE SEDI_main_1b
 SUBROUTINE countColumns(QX,ni,nk,minQX,counter,activeColumn,ktop_sedi)
  implicit none
  integer, intent(in) :: ni,nk
  integer, dimension(:), intent(in) :: ktop_sedi
  integer, intent(out) :: counter
  integer, dimension(:), intent(out) :: activeColumn
  real, dimension(:,:), intent(in) :: QX
  real, intent(in) :: minQX
  integer :: i
  integer, dimension(ni) :: k
   counter = 0
   activeColumn= 0
   do i=1,ni
      k(i)= ktop_sedi(i)-1
      do
         k(i)=k(i)+1
         if (QX(i,k(i))>minQX) then
            counter=counter+1
            activeColumn(counter)=i
            k(i)=0
            exit
         else
            if (k(i)==nk) then
               k(i)=0
               exit
            endif
         endif
      enddo
   enddo
 END SUBROUTINE countColumns
end module my_sedi_mod
module my_dmom_mod
  implicit none
  private
  public :: mp_milbrandt2mom_main
  contains
 SUBROUTINE mp_milbrandt2mom_main(W_omega,T,Q,QC,QR,QI,QN,QG,QH,NC,NR,NY,NN,NG,NH,PS,TM, &
     QM,QCM,QRM,QIM,QNM,QGM,QHM,NCM,NRM,NYM,NNM,NGM,NHM,PSM,S,RT_rn1,RT_rn2,RT_fr1,RT_fr2,&
     RT_sn1,RT_sn2,RT_sn3,RT_pe1,RT_pe2,RT_peL,RT_snd,GZ,T_TEND,Q_TEND,QCTEND,QRTEND, &
     QITEND,QNTEND,QGTEND,QHTEND,NCTEND,NRTEND,NYTEND,NNTEND,NGTEND,NHTEND,dt,NI,N,NK, &
     J,KOUNT,CCNtype,precipDiag_ON,sedi_ON,warmphase_ON,autoconv_ON,icephase_ON,snow_ON, &
     initN,dblMom_c,dblMom_r,dblMom_i,dblMom_s,dblMom_g,dblMom_h,Dm_c,Dm_r,Dm_i,Dm_s, &
     Dm_g,Dm_h,ZET,ZEC,SLW,VIS,VIS1,VIS2,VIS3,h_CB,h_ML1,h_ML2,h_SN,SS01,SS02,SS03,SS04, &
     SS05,SS06,SS07,SS08,SS09,SS10,SS11,SS12,SS13,SS14,SS15,SS16,SS17,SS18,SS19,SS20)
  use my_fncs_mod
  use my_sedi_mod
    use module_model_constants, ONLY: CPD => cp, CPV => cpv, RGASD => r_d, RGASV => r_v, &
        EPS1 => EP_2, DELTA => EP_1, CAPPA => rcp, GRAV => g, CHLC => XLV, CHLF => XLF
  implicit none
  integer, intent(in) :: NI,NK,N,J,KOUNT,CCNtype
  real, intent(in) :: dt
  real, dimension(:), intent(in) :: PS,PSM
  real, dimension(:), intent(out) :: h_CB,h_ML1,h_ML2,h_SN
  real, dimension(:), intent(out) :: RT_rn1,RT_rn2,RT_fr1,RT_fr2,RT_sn1,RT_sn2, &
                                          RT_sn3,RT_pe1,RT_pe2,RT_peL,ZEC,RT_snd
  real, dimension(:,:), intent(in) :: W_omega,S,GZ
  real, dimension(:,:), intent(inout) :: T,Q,QC,QR,QI,QN,QG,QH,NC,NR,NY,NN,NG,NH, &
        TM,QM,QCM,QRM,QIM,QNM,QGM,QHM,NCM,NRM,NYM,NNM,NGM,NHM
  real, dimension(:,:), intent(out) :: T_TEND,QCTEND,QRTEND,QITEND,QNTEND, &
        QGTEND,QHTEND,Q_TEND,NCTEND,NRTEND,NYTEND,NNTEND,NGTEND,NHTEND,ZET,Dm_c, &
        Dm_r,Dm_i,Dm_s,Dm_g,Dm_h,SLW,VIS,VIS1,VIS2,VIS3,SS01,SS02,SS03,SS04,SS05,SS06, &
        SS07,SS08,SS09,SS10,SS11,SS12,SS13,SS14,SS15,SS16,SS17,SS18,SS19,SS20
  logical, intent(in) :: dblMom_c,dblMom_r,dblMom_i,dblMom_s, &
        dblMom_g,dblMom_h,precipDiag_ON,sedi_ON,icephase_ON,snow_ON,warmphase_ON, &
        autoconv_ON,initN
  logical :: log1,log2,log3,log4,doneK,rainPresent,calcDiag,CB_found,ML_found, &
             SN_found
  logical, dimension(size(QC,dim=1),size(QC,dim=2)) :: activePoint
  integer, dimension(size(QC,dim=1)) :: ktop_sedi
  integer :: i,k,niter,ll,start
  real :: tmp1,tmp2,tmp3,tmp4,tmp5,tmp6,tmp7,tmp8,tmp9,tmp10, &
       VDmax,NNUmax,X,D,DEL,QREVP,NuDEPSOR,NuCONTA,NuCONTB,NuCONTC,iMUkin,Ecg,Erg, &
       NuCONT,GG,Na,Tcc,F1,F2,Kdiff,PSIa,Kn,source,sink,sour,ratio,qvs0,Kstoke, &
       DELqvs,ft,esi,Si,Simax,Vq,Vn,Vz,LAMr,No_r_DM,No_i,No_s,No_g,No_h,D_sll, &
       iABi,ABw,VENTr,VENTs,VENTg,VENTi,VENTh,Cdiff,Ka,MUdyn,MUkin,DEo,Ng_tail, &
       gam,ScTHRD,Tc,mi,ff,Ec,Ntr,Dho,DMrain,Ech,DMice,DMsnow,DMgrpl,DMhail, &
       ssat,Swmax,dey,Esh,Eii,Eis,Ess,Eig,Eih,FRAC,JJ,Dirg,Dirh,Dsrs,Dsrg,Dsrh, &
       Dgrg,Dgrh,SIGc,L,TAU,DrAUT,DrINIT,Di,Ds,Dg,Dh,qFact,nFact,Ki,Rz,NgCNgh, &
       vr0,vi0,vs0,vg0,vh0,Dc,Dr,QCLcs,QCLrs,QCLis,QCLcg,QCLrg,QCLig,NhCNgh, &
       QCLch,QCLrh,QCLsh,QMLir,QMLsr,QMLgr,QMLhr,QCLih,QVDvg,QVDvh,QSHhr, &
       QFZci,QNUvi,QVDvi,QCNis,QCNis1,QCNis2,QCLir,QCLri,QCNsg,QCLsr,QCNgh, &
       QCLgr,QHwet,QVDvs,QFZrh,QIMsi,QIMgi,NMLhr,NVDvh,NCLir,NCLri,NCLrh, &
       NCLch,NCLsr,NCLirg,NCLirh,NrFZrh,NhFZrh,NCLsrs,NCLsrg,NCLsrh,NCLgrg, &
       NCLgrh,NVDvg,NMLgr,NiCNis,NsCNis,NVDvs,NMLsr,NCLsh,NCLss,NNUvi,NFZci,NVDvi, &
       NCLis,NCLig,NCLih,NMLir,NCLrs,NCNsg,NCLcs,NCLcg,NIMsi,NIMgi,NCLgr,NCLrg, &
       NSHhr,RCAUTR,RCACCR,CCACCR,CCSCOC,CCAUTR,CRSCOR,ALFx,des_pmlt,Ecs,des,ides, &
       LAMx,iLAMx,iLAMxB0,Dx,ffx,iLAMc,iNCM,iNRM,iNYM,iNNM,iNGM,iLAMs_D3, &
       iLAMg,iLAMg2,iLAMgB0,iLAMgB1,iLAMgB2,iLAMh,iLAMhB0,iLAMhB1,iLAMhB2,iNHM, &
       iLAMi,iLAMi2,iLAMi3,iLAMi4,iLAMi5,iLAMiB0,iLAMiB1,iLAMiB2,iLAMr6,iLAMh2, &
       iLAMs,iLAMs2,iLAMsB0,iLAMsB1,iLAMsB2,iLAMr,iLAMr2,iLAMr3,iLAMr4,iLAMr5, &
       iLAMc2,iLAMc3,iLAMc4,iLAMc5,iLAMc6,iQCM,iQRM,iQIM,iQNM,iQGM,iQHM,iEih,iEsh, &
       N_c,N_r,N_i,N_s,N_g,N_h,fluxV_i,fluxV_g,fluxV_s,rhos_mlt,fracLiq
  real, save :: idt,iMUc,cmr,cmi,cms,cmg,cmh,icmr,icmi,icmg,icms,icmh,idew,idei, &
       ideh,ideg,GC1,imso,icexc9,cexr1,cexr2,cexr3,No_s_SM,No_r,idms,imgo,icexs2, &
       cexr4,cexr5,cexr6,cexr9,icexr9,ckQr1,ckQr2,ckQr3,ckQi1,ckQi2,ckQi3,ckQi4, &
       icexi9,ckQs1,ckQs2,cexs1,cexs2,ckQg1,ckQg2,ckQg4,ckQh1,ckQh2,ckQh4,GR37,dms, &
       LCP,LFP,LSP,ck5,ck6,PI2,PIov4,PIov6,CHLS,iCHLF,cxr,cxi,Gzr,Gzi,Gzs,Gzg,Gzh, &
       N_c_SM,iGC1,GC2,GC3,GC4,GC5,iGC5,GC6,GC7,GC8,GC11,GC12,GC13,GC14,iGR34,mso, &
       GC15,GR1,GR3,GR13,GR14,GR15,GR17,GR31,iGR31,GR32,GR33,GR34,GR35,GR36,GI4, &
       GI6,GI20,GI21,GI22,GI31,GI32,GI33,GI34,GI35,iGI31,GI11,GI36,GI37,GI40,iGG34, &
       GS09,GS11,GS12,GS13,iGS20,GS31,iGS31,GS32,GS33,GS34,GS35,GS36,GS40,iGS40, &
       GS50,GG09,GG11,GG12,GG13,GG31,iGG31,GG32,GG33,GG34,GG35,GG36,GG40,iGG99,GH09,&
       GH11,GH12,GH13,GH31,GH32,GH33,GH40,GR50,GG50,iGH34,GH50,iGH99,iGH31,iGS34, &
       iGS20_D3,GS40_D3,cms_D3,eds,fds,rfact_FvFm
  real, parameter :: MUc = 3.
  real, parameter :: alpha_c = 1.
  real, parameter :: alpha_r = 0.
  real, parameter :: alpha_i = 0.
  real, parameter :: alpha_s = 0.
  real, parameter :: alpha_g = 0.
  real, parameter :: alpha_h = 0.
  real, parameter :: No_s_max = 1.e+8
  real, parameter :: lamdas_min= 500.
  real, parameter :: No_r_SM = 1.e+7
  real, parameter :: No_g_SM = 4.e+6
  real, parameter :: No_h_SM = 1.e+5
  real, parameter :: afr= 149.100, bfr= 0.5000
  real, parameter :: afi= 71.340, bfi= 0.6635
  real, parameter :: afs= 11.720, bfs= 0.4100
  real, parameter :: afg= 19.300, bfg= 0.3700
  real, parameter :: afh= 206.890, bfh= 0.6384
  real, parameter :: epsQ = 1.e-14
  real, parameter :: epsN = 1.e-3
  real, parameter :: epsQ2 = 1.e-6
  real, parameter :: epsVIS= 1.
  real, parameter :: iLAMmin1= 1.e-6
  real, parameter :: iLAMmin2= 1.e-10
  real, parameter :: eps = 1.e-32
  real, parameter :: k1 = 0.001
  real, parameter :: k2 = 0.0005
  real, parameter :: k3 = 2.54
  real, parameter :: CPW = 4218., CPI=2093.
  real, parameter :: deg = 400., mgo= 1.6e-10
  real, parameter :: deh = 900.
  real, parameter :: dei = 500., mio=1.e-12, Nti0=1.e3
  real, parameter :: dew = 1000.
  real, parameter :: desFix= 100.
  real, parameter :: desMax= 500.
  real, parameter :: Dso = 125.e-6
  real, parameter :: dmr = 3., dmi= 3., dmg= 3., dmh= 3.
  real, parameter :: DrMax= 5.e-3, VrMax= 16., epsQr_sedi= 1.e-8
  real, parameter :: DiMax= 5.e-3, ViMax= 2., epsQi_sedi= 1.e-10
  real, parameter :: DsMax= 5.e-3, VsMax= 2., epsQs_sedi= 1.e-8
  real, parameter :: DgMax= 50.e-3, VgMax= 8., epsQg_sedi= 1.e-8
  real, parameter :: DhMax= 80.e-3, VhMax= 25., epsQh_sedi= 1.e-10
  real, parameter :: thrd = 1./3.
  real, parameter :: sixth = 0.5*thrd
  real, parameter :: Ers = 1., Eci= 1.
  real, parameter :: Eri = 1., Erh= 1.
  real, parameter :: Xdisp = 0.25
  real, parameter :: aa11 = 9.44e15, aa22= 5.78e3, Rh= 41.e-6
  real, parameter :: Avx = 0.78, Bvx= 0.30
  real, parameter :: Abigg = 0.66, Bbigg= 100.
  real, parameter :: fdielec = 4.464
  real, parameter :: zfact = 1.e+18
  real, parameter :: minZET = -99.
  real, parameter :: maxVIS = 99.e+3
  real, parameter :: Drshed = 0.001
  real, parameter :: SIGcTHRS = 15.e-6
  real, parameter :: KK1 = 3.03e3
  real, parameter :: KK2 = 2.59e15
  real, parameter :: Dhh = 82.e-6
  real, parameter :: gzMax_sedi = 200000.
  real, parameter :: Dr_large = 200.e-6
  real, parameter :: Ds_large = 200.e-6
  real, parameter :: Dh_large = 1.0e-2
  real, parameter :: Dh_min = 5.0e-3
  real, parameter :: Dr_3cmpThrs = 2.5e-3
  real, parameter :: w_CNgh = 3.
  real, parameter :: Ngh_crit = 0.01
  real, parameter :: Tc_FZrh = -10.
  real, parameter :: CNsgThres = 1.0
  real, parameter :: capFact_i = 0.5
  real, parameter :: capFact_s = 0.5
  real, parameter :: noVal_h_XX = -1.
  real, parameter :: minSnowSize = 1.e-4
  real, parameter :: Fv_Dsmin = 125.e-6
  real, parameter :: Fv_Dsmax = 0.008
  real, parameter :: Ni_max = 1.e+7
  real, parameter :: TRPL =.27316e+3
  real, parameter :: TCDK =.27315e+3
  real, parameter :: RAUW =.1e+4
  real, parameter :: EPS2 =.3780199778986
  real, parameter :: TGL =.27316e+3
  real, parameter :: CONSOL =.1367e+4
  real, parameter :: RAYT =.637122e+7
  real, parameter :: STEFAN =.566948e-7
  real, parameter :: PI =.314159265359e+1
  real, parameter :: OMEGA =.7292e-4
  real, parameter :: KNAMS =.514791
  real, parameter :: STLO =.6628486583943e-3
  real, parameter :: KARMAN =.35
  real, parameter :: RIC =.2
      REAL TTT, PRS, QQQ, EEE, TVI, QST, QQH
      REAL T00, PR0, TF, PF,FFF , DDFF
      REAL QSM , DLEMX
      REAL*8 FOEW,FODLE,FOQST,FODQS,FOEFQ,FOQFE,FOTVT,FOTTV,FOHR
      REAL*8 FOLV,FOLS,FOPOIT,FOPOIP,FOTTVH,FOTVHT
      REAL*8 FOEWA,FODLA,FOQSA,FODQA,FOHRA
      REAL*8 FESI,FDLESI,FESMX,FDLESMX,FQSMX,FDQSMX
      FOEW(TTT) = 610.78D0*DEXP( DMIN1(DSIGN(17.269D0, &
       DBLE(TTT)-DBLE(TRPL)),DSIGN &
       (21.875D0,DBLE(TTT)-DBLE(TRPL)))*DABS(DBLE(TTT)-DBLE(TRPL))/ &
       (DBLE(TTT)-35.86D0+DMAX1(0.D0,DSIGN &
       (28.2D0,DBLE(TRPL)-DBLE(TTT)))))
      FODLE(TTT)=(4097.93D0+DMAX1(0.D0,DSIGN(1709.88D0, &
       DBLE(TRPL)-DBLE(TTT)))) &
       /((DBLE(TTT)-35.86D0+DMAX1(0.D0,DSIGN(28.2D0, &
       DBLE(TRPL)-DBLE(TTT))))*(DBLE(TTT)-35.86D0+DMAX1(0.D0 &
       ,DSIGN(28.2D0,DBLE(TRPL)-DBLE(TTT)))))
      FOQST(TTT,PRS) = DBLE(EPS1)/(DMAX1(1.D0,DBLE(PRS)/FOEW(TTT))- &
       DBLE(EPS2))
      FODQS(QST,TTT)=DBLE(QST)*(1.D0+DBLE(DELTA)*DBLE(QST))*FODLE(TTT)
      FOEFQ(QQQ,PRS) = DMIN1(DBLE(PRS),(DBLE(QQQ)*DBLE(PRS)) / &
       (DBLE(EPS1) + DBLE(EPS2)*DBLE(QQQ)))
      FOQFE(EEE,PRS) = DMIN1(1.D0,DBLE(EPS1)*DBLE(EEE)/(DBLE(PRS)- &
       DBLE(EPS2)*DBLE(EEE)))
      FOTVT(TTT,QQQ) = DBLE(TTT) * (1.0D0 + DBLE(DELTA)*DBLE(QQQ))
      FOTVHT(TTT,QQQ,QQH) = DBLE(TTT) * &
           (1.0D0 + DBLE(DELTA)*DBLE(QQQ) - DBLE(QQH))
      FOTTV(TVI,QQQ) = DBLE(TVI) / (1.0D0 + DBLE(DELTA)*DBLE(QQQ))
      FOTTVH(TVI,QQQ,QQH) = DBLE(TVI) / &
           (1.0D0 + DBLE(DELTA)*DBLE(QQQ) - DBLE(QQH))
       FOHR(QQQ,TTT,PRS) = MIN(DBLE(PRS),FOEFQ(QQQ,PRS)) / FOEW(TTT)
      FOLV(TTT) =DBLE(CHLC) - 2317.D0*(DBLE(TTT)-DBLE(TRPL))
      FOLS(TTT) = DBLE(CHLC)+DBLE(CHLF)+(DBLE(CPV)- &
                  (7.24D0*DBLE(TTT)+128.4D0))*(DBLE(TTT)-DBLE(TRPL))
      FOPOIT(T00,PR0,PF)=DBLE(T00)*(DBLE(PR0)/DBLE(PF))** &
                       (-DBLE(CAPPA))
      FOPOIP(T00,TF,PR0)=DBLE(PR0)*DEXP(-(DLOG(DBLE(T00)/DBLE(TF))/ &
                       DBLE(CAPPA)))
      FOEWA(TTT)=610.78D0*DEXP(17.269D0*(DBLE(TTT)-DBLE(TRPL))/ &
       (DBLE(TTT)-35.86D0))
      FODLA(TTT)=17.269D0*(DBLE(TRPL)-35.86D0)/(DBLE(TTT)-35.86D0)**2
      FOQSA(TTT,PRS)=DBLE(EPS1)/(DMAX1(1.D0,DBLE(PRS)/FOEWA(TTT))- &
       DBLE(EPS2))
      FODQA(QST,TTT)=DBLE(QST)*(1.D0+DBLE(DELTA)*DBLE(QST))*FODLA(TTT)
      FOHRA(QQQ,TTT,PRS)=MIN(DBLE(PRS),FOEFQ(QQQ,PRS))/FOEWA(TTT)
      FESI(TTT)=610.78D0*DEXP(21.875D0*(DBLE(TTT)-DBLE(TRPL))/ &
             (DBLE(TTT)-7.66D0) )
      FDLESI(TTT)=21.875D0*(DBLE(TRPL)-7.66D0)/(DBLE(TTT)-7.66D0)**2
      FESMX(TTT,FFF) = (1.D0-DBLE(FFF))*FOEWA(TTT)+DBLE(FFF)*FESI(TTT)
      FDLESMX(TTT,FFF,DDFF) = ( (1.D0-DBLE(FFF))*FOEWA(TTT)*FODLA(TTT) &
                            + DBLE(FFF)*FESI(TTT)*FDLESI(TTT) &
                  + DBLE(DDFF)*(FESI(TTT)-FOEWA(TTT)) )/FESMX(TTT,FFF)
      FQSMX(TTT,PRS,FFF) = DBLE(EPS1)/ &
              (DMAX1(1.D0,DBLE(PRS)/FESMX(TTT,FFF) ) - DBLE(EPS2) )
      FDQSMX(QSM,DLEMX) = DBLE(QSM ) *(1.D0 + DBLE(DELTA)* DBLE(QSM ) ) &
                           * DBLE(DLEMX )
  real, parameter :: LAMa0 = 6.6e-8
  real, parameter :: T0 = 293.15
  real, parameter :: p0 = 101325.
  real, parameter :: Ra = 1.e-6
  real, parameter :: kBoltz = 1.381e-23
  real, parameter :: KAPa = 5.39e5
  logical, parameter :: iceDep_ON = .true.
  logical, parameter :: grpl_ON = .true.
  logical, parameter :: hail_ON = .true.
  logical, parameter :: rainAccr_ON = .true.
  logical, parameter :: snowSpherical = .false.
  integer, parameter :: primIceNucl = 1
  real, parameter :: outfreq = 60.
  real, dimension(size(QC,dim=1),size(QC,dim=2)) :: DE,iDE,DP,QSS,QSW,QSI,WZ,DZ,RHOQX,FLIM, &
        VQQ,gamfact,gamfact_r,massFlux3D_r,massFlux3D_s
  real, dimension(size(QC,dim=1)) :: fluxM_r,fluxM_i,fluxM_s,fluxM_g,fluxM_h, &
        HPS,dum
  integer, dimension(size(QC,dim=1)) :: activeColumn
  do k= 1,nk
    do i= 1,ni
      tmp1= S(i,k)*PSM(i)/(RGASD*TM(i,k))
      tmp2= S(i,k)*PS(i)/(RGASD*T(i,k))
      NCM(i,k)= NCM(i,k)*tmp1; NC(i,k)= NC(i,k)*tmp2
      NRM(i,k)= NRM(i,k)*tmp1; NR(i,k)= NR(i,k)*tmp2
      NYM(i,k)= NYM(i,k)*tmp1; NY(i,k)= NY(i,k)*tmp2
      NNM(i,k)= NNM(i,k)*tmp1; NN(i,k)= NN(i,k)*tmp2
      NGM(i,k)= NGM(i,k)*tmp1; NG(i,k)= NG(i,k)*tmp2
      NHM(i,k)= NHM(i,k)*tmp1; NH(i,k)= NH(i,k)*tmp2
    enddo
  enddo
  SS01= 0.; SS02= 0.; SS03= 0.; SS04= 0.; SS05= 0.; SS06= 0.; SS07= 0.; SS08= 0.
  SS09= 0.; SS10= 0.; SS11= 0.; SS12= 0.; SS13= 0.; SS14= 0.; SS15= 0.; SS16= 0.
  SS17= 0.; SS18= 0.; SS19= 0.; SS20= 0.
  ktop_sedi= 0
  do i=1,ni
     do k=1,nk
       ktop_sedi(i)= k
       if (GZ(i,k)<gzMax_sedi) exit
     enddo
  enddo
  calcDiag = .true.
  if (.TRUE.) then
   PI2 = PI*2.
   PIov4 = 0.25*PI
   PIov6 = PI*sixth
   CHLS = CHLC+CHLF
   LCP = CHLC/CPD
   LFP = CHLF/CPD
   iCHLF = 1./CHLF
   LSP = LCP+LFP
   ck5 = 4098.170*LCP
   ck6 = 5806.485*LSP
   idt = 1./dt
   imgo = 1./mgo
   idew = 1./dew
   idei = 1./dei
   ideg = 1./deg
   ideh = 1./deh
   cmr = PIov6*dew; icmr= 1./cmr
   cmi = 440.; icmi= 1./cmi
   cmg = PIov6*deg; icmg= 1./cmg
   cmh = PIov6*deh; icmh= 1./cmh
   cms_D3 = PIov6*desFix
   if (snowSpherical) then
      cms = cms_D3
      dms = 3.
   else
      cms = 0.1597; dms = 2.078
   endif
   icms = 1./cms
   idms = 1./dms
   mso = cms*Dso**dms
   imso = 1./mso
   eds = cms/PIov6
   fds = dms-3.
   if (fds/=-1. .and..not.snowSpherical) GS50= gamma(1.+fds+alpha_s)
   iMUc = 1./MUc
   GC1 = gamma(alpha_c+1.0)
   iGC1 = 1./GC1
   GC2 = gamma(alpha_c+1.+3.0*iMUc)
   GC3 = gamma(alpha_c+1.+6.0*iMUc)
   GC4 = gamma(alpha_c+1.+9.0*iMUc)
   GC11 = gamma(1.0*iMUc+1.0+alpha_c)
   GC12 = gamma(2.0*iMUc+1.0+alpha_c)
   GC5 = gamma(1.0+alpha_c)
   iGC5 = 1./GC5
   GC6 = gamma(1.0+alpha_c+1.0*iMUc)
   GC7 = gamma(1.0+alpha_c+2.0*iMUc)
   GC8 = gamma(1.0+alpha_c+3.0*iMUc)
   GC13 = gamma(3.0*iMUc+1.0+alpha_c)
   GC14 = gamma(4.0*iMUc+1.0+alpha_c)
   GC15 = gamma(5.0*iMUc+1.0+alpha_c)
   icexc9 = 1./(GC2*iGC1*PIov6*dew)
   if (CCNtype==1) then
      N_c_SM = 0.8e+8
   elseif (CCNtype==2) then
      N_c_SM = 2.0e+8
   elseif (CCNtype==3) then
      N_c_SM = 5.0e+8
   else
      N_c_SM = 2.0e+8
   endif
   cexr1 = 1.+alpha_r+dmr+bfr
   cexr2 = 1.+alpha_r+dmr
   GR17 = gamma(2.5+alpha_r+0.5*bfr)
   GR31 = gamma(1.+alpha_r)
   iGR31 = 1./GR31
   GR32 = gamma(2.+alpha_r)
   GR33 = gamma(3.+alpha_r)
   GR34 = gamma(4.+alpha_r)
   iGR34 = 1./GR34
   GR35 = gamma(5.+alpha_r)
   GR36 = gamma(6.+alpha_r)
   GR37 = gamma(7.+alpha_r)
   GR50 = (No_r_SM*GR31)**0.75
   cexr5 = 2.+alpha_r
   cexr6 = 2.5+alpha_r+0.5*bfr
   cexr9 = cmr*GR34*iGR31; icexr9= 1./cexr9
   cexr3 = 1.+bfr+alpha_r
   cexr4 = 1.+alpha_r
   ckQr1 = afr*gamma(1.+alpha_r+dmr+bfr)/gamma(1.+alpha_r+dmr)
   ckQr2 = afr*gamma(1.+alpha_r+bfr)*GR31
   ckQr3 = afr*gamma(7.+alpha_r+bfr)/GR37
   if (.not.dblMom_r) then
      No_r = No_r_SM
   endif
   GI4 = gamma(alpha_i+dmi+bfi)
   GI6 = gamma(2.5+bfi*0.5+alpha_i)
   GI11 = gamma(1.+bfi+alpha_i)
   GI20 = gamma(0.+bfi+1.+alpha_i)
   GI21 = gamma(1.+bfi+1.+alpha_i)
   GI22 = gamma(2.+bfi+1.+alpha_i)
   GI31 = gamma(1.+alpha_i)
   iGI31 = 1./GI31
   GI32 = gamma(2.+alpha_i)
   GI33 = gamma(3.+alpha_i)
   GI34 = gamma(4.+alpha_i)
   GI35 = gamma(5.+alpha_i)
   GI36 = gamma(6.+alpha_i)
   GI40 = gamma(1.+alpha_i+dmi)
   icexi9 = 1./(cmi*gamma(1.+alpha_i+dmi)*iGI31)
   ckQi1 = afi*gamma(1.+alpha_i+dmi+bfi)/GI40
   ckQi2 = afi*GI11*iGI31
   ckQi4 = 1./(cmi*GI40*iGI31)
   cexs1 = 2.5+0.5*bfs+alpha_s
   cexs2 = 1.+alpha_s+dms
   icexs2 = 1./cexs2
   GS09 = gamma(2.5+bfs*0.5+alpha_s)
   GS11 = gamma(1.+bfs+alpha_s)
   GS12 = gamma(2.+bfs+alpha_s)
   GS13 = gamma(3.+bfs+alpha_s)
   GS31 = gamma(1.+alpha_s)
   iGS31 = 1./GS31
   GS32 = gamma(2.+alpha_s)
   GS33 = gamma(3.+alpha_s)
   GS34 = gamma(4.+alpha_s)
   iGS34 = 1./GS34
   GS35 = gamma(5.+alpha_s)
   GS36 = gamma(6.+alpha_s)
   GS40 = gamma(1.+alpha_s+dms)
   iGS40 = 1./GS40
   iGS20 = 1./(GS40*iGS31*cms)
   ckQs1 = afs*gamma(1.+alpha_s+dms+bfs)*iGS40
   ckQs2 = afs*GS11*iGS31
   GS40_D3 = gamma(1.+alpha_s+3.)
   iGS20_D3= 1./(GS40_D3*iGS31*cms_D3)
   rfact_FvFm= PIov6*icms*gamma(4.+bfs+alpha_s)/gamma(1.+dms+bfs+alpha_s)
   GG09 = gamma(2.5+0.5*bfg+alpha_g)
   GG11 = gamma(1.+bfg+alpha_g)
   GG12 = gamma(2.+bfg+alpha_g)
   GG13 = gamma(3.+bfg+alpha_g)
   GG31 = gamma(1.+alpha_g)
   iGG31 = 1./GG31
   GG32 = gamma(2.+alpha_g)
   GG33 = gamma(3.+alpha_g)
   GG34 = gamma(4.+alpha_g)
   iGG34 = 1./GG34
   GG35 = gamma(5.+alpha_g)
   GG36 = gamma(6.+alpha_g)
   GG40 = gamma(1.+alpha_g+dmg)
   iGG99 = 1./(GG40*iGG31*cmg)
   GG50 = (No_g_SM*GG31)**0.75
   ckQg1 = afg*gamma(1.+alpha_g+dmg+bfg)/GG40
   ckQg2 = afg*GG11*iGG31
   ckQg4 = 1./(cmg*GG40*iGG31)
   GH09 = gamma(2.5+bfh*0.5+alpha_h)
   GH11 = gamma(1.+bfh+alpha_h)
   GH12 = gamma(2.+bfh+alpha_h)
   GH13 = gamma(3.+bfh+alpha_h)
   GH31 = gamma(1.+alpha_h)
   iGH31 = 1./GH31
   GH32 = gamma(2.+alpha_h)
   GH33 = gamma(3.+alpha_h)
   iGH34 = 1./gamma(4.+alpha_h)
   GH40 = gamma(1.+alpha_h+dmh)
   iGH99 = 1./(GH40*iGH31*cmh)
   GH50 = (No_h_SM*GH31)**0.75
   ckQh1 = afh*gamma(1.+alpha_h+dmh+bfh)/GH40
   ckQh2 = afh*GH11*iGH31
   ckQh4 = 1./(cmh*GH40*iGH31)
  endif
  tmp1= 1./GRAV
  do k=2,nk
     DZ(:,k)= (GZ(:,k-1)-GZ(:,k))*tmp1
  enddo
  DZ(:,1)= DZ(:,2)
  T_TEND = T ; Q_TEND = Q
  QCTEND = QC; QRTEND = QR; QITEND = QI; QNTEND = QN; QGTEND = QG; QHTEND = QH
  NCTEND = NC; NRTEND = NR; NYTEND = NY; NNTEND = NN; NGTEND = NG; NHTEND = NH
  IF (initN) THEN
     do k= 1,nk
        do i= 1,ni
           tmp1= S(i,k)*PSM(i)/(RGASD*TM(i,k))
           tmp2= S(i,k)*PS(i)/(RGASD*T(i,k))
           if (QCM(i,k)>epsQ .and. NCM(i,k)<epsN) &
              NCM(i,k)= N_c_SM
           if (QC(i,k)>epsQ .and. NC(i,k)<epsN) &
              NC(i,k) = N_c_SM
           if (QRM(i,k)>epsQ .and. NRM(i,k)<epsN) &
              NRM(i,k)= (No_r_SM*GR31)**(3./(4.+alpha_r))*(GR31*iGR34*tmp1*QRM(i,k)* &
                        icmr)**((1.+alpha_r)/(4.+alpha_r))
           if (QR(i,k)>epsQ .and. NR(i,k)<epsN) &
              NR(i,k)= (No_r_SM*GR31)**(3./(4.+alpha_r))*(GR31*iGR34*tmp2*QR(i,k)* &
                       icmr)**((1.+alpha_r)/(4.+alpha_r))
           if (QIM(i,k)>epsQ .and. NYM(i,k)<epsN) &
              NYM(i,k)= N_Cooper(TRPL,TM(i,k))
           if (QI(i,k)>epsQ .and. NY(i,k)<epsN) &
              NY(i,k)= N_Cooper(TRPL,T(i,k))
           if (QNM(i,k)>epsQ .and. NNM(i,k)<epsN) then
              No_s= Nos_Thompson(TRPL,TM(i,k))
              NNM(i,k)= (No_s*GS31)**(dms*icexs2)*(GS31*iGS40*icms*tmp1*QNM(i,k))** &
                        ((1.+alpha_s)*icexs2)
           endif
           if (QN(i,k)>epsQ .and. NN(i,k)<epsN) then
              No_s= Nos_Thompson(TRPL,T(i,k))
              NN(i,k)= (No_s*GS31)**(dms*icexs2)*(GS31*iGS40*icms*tmp2*QN(i,k))** &
                       ((1.+alpha_s)*icexs2)
           endif
           if (QGM(i,k)>epsQ .and. NGM(i,k)<epsN) &
              NGM(i,k)= (No_g_SM*GG31)**(3./(4.+alpha_g))*(GG31*iGG34*tmp1*QGM(i,k)* &
                        icmg)**((1.+alpha_g)/(4.+alpha_g))
           if (QG(i,k)>epsQ .and. NG(i,k)<epsN) &
              NG(i,k)= (No_g_SM*GG31)**(3./(4.+alpha_g))*(GG31*iGG34*tmp2*QG(i,k)* &
                   icmg)**((1.+alpha_g)/(4.+alpha_g))
           if (QHM(i,k)>epsQ .and. NHM(i,k)<epsN) &
              NHM(i,k)= (No_h_SM*GH31)**(3./(4.+alpha_h))*(GH31*iGH34*tmp1*QHM(i,k)* &
                        icmh)**((1.+alpha_h)/(4.+alpha_h))
           if (QH(i,k)>epsQ .and. NH(i,k)<epsN) &
              NH(i,k)= (No_h_SM*GH31)**(3./(4.+alpha_h))*(GH31*iGH34*tmp2*QH(i,k)* &
                        icmh)**((1.+alpha_h)/(4.+alpha_h))
        enddo
     enddo
  ENDIF
  do k= 1,nk
     do i= 1,ni
       IF (dblMom_c) THEN
         if(QC(i,k)<epsQ .or. NC(i,k)<epsN) then
            Q(i,k) = Q(i,k) + QC(i,k)
            QC(i,k)= 0.; NC(i,k)= 0.
         endif
         if(QCM(i,k)<epsQ .or. NCM(i,k)<epsN) then
            QM(i,k) = QM(i,k) + QCM(i,k)
            QCM(i,k)= 0.; NCM(i,k)= 0.
         endif
       ELSE
         if(QC(i,k)<epsQ) then
            Q(i,k) = Q(i,k) + QC(i,k)
            QC(i,k)= 0.
         endif
         if(QCM(i,k)<epsQ) then
            QM(i,k) = QM(i,k) + QCM(i,k)
            QCM(i,k)= 0.
         endif
       ENDIF
       IF (dblMom_r) THEN
         if (QR(i,k)<epsQ .or. NR(i,k)<epsN) then
            Q(i,k) = Q(i,k) + QR(i,k)
            QR(i,k)= 0.; NR(i,k)= 0.
         endif
         if (QRM(i,k)<epsQ .or. NRM(i,k)<epsN) then
            QM(i,k) = QM(i,k) + QRM(i,k)
            QRM(i,k)= 0.; NRM(i,k)= 0.
         endif
       ELSE
         if (QR(i,k)<epsQ) then
            Q(i,k) = Q(i,k) + QR(i,k)
            QR(i,k)= 0.
         endif
         if (QRM(i,k)<epsQ) then
            QM(i,k) = QM(i,k) + QRM(i,k)
            QRM(i,k)= 0.
         endif
       ENDIF
       IF (dblMom_i) THEN
         if (QI(i,k)<epsQ .or. NY(i,k)<epsN) then
            Q(i,k) = Q(i,k) + QI(i,k)
            QI(i,k)= 0.; NY(i,k)= 0.
         endif
         if (QIM(i,k)<epsQ .or. NYM(i,k)<epsN) then
            QM(i,k) = QM(i,k) + QIM(i,k)
            QIM(i,k)= 0.; NYM(i,k)= 0.
         endif
       ELSE
         if (QI(i,k)<epsQ) then
            Q(i,k) = Q(i,k) + QI(i,k)
            QI(i,k)= 0.
         endif
         if (QIM(i,k)<epsQ) then
            QM(i,k) = QM(i,k) + QIM(i,k)
            QIM(i,k)= 0.
         endif
       ENDIF
       IF (dblMom_s) THEN
         if (QN(i,k)<epsQ .or. NN(i,k)<epsN) then
            Q(i,k) = Q(i,k) + QN(i,k)
            QN(i,k)= 0.; NN(i,k)= 0.
         endif
         if (QNM(i,k)<epsQ .or. NNM(i,k)<epsN) then
            QM(i,k) = QM(i,k) + QNM(i,k)
            QNM(i,k)= 0.; NNM(i,k)= 0.
         endif
       ELSE
         if (QN(i,k)<epsQ) then
            Q(i,k) = Q(i,k) + QN(i,k)
            QN(i,k)= 0.
         endif
         if (QNM(i,k)<epsQ) then
            QM(i,k) = QM(i,k) + QNM(i,k)
            QNM(i,k)= 0.
         endif
       ENDIF
       IF (dblMom_g) THEN
         if (QG(i,k)<epsQ .or. NG(i,k)<epsN) then
            Q(i,k) = Q(i,k) + QG(i,k)
            QG(i,k)= 0.; NG(i,k)= 0.
         endif
         if (QGM(i,k)<epsQ .or. NGM(i,k)<epsN) then
            QM(i,k) = QM(i,k) + QGM(i,k)
            QGM(i,k)= 0.; NGM(i,k)= 0.
         endif
       ELSE
         if (QG(i,k)<epsQ) then
            Q(i,k) = Q(i,k) + QG(i,k)
            QG(i,k)= 0.
         endif
         if (QGM(i,k)<epsQ) then
            QM(i,k) = QM(i,k) + QGM(i,k)
            QGM(i,k)= 0.
         endif
       ENDIF
       IF (dblMom_h) THEN
         if (QH(i,k)<epsQ .or. NH(i,k)<epsN) then
            Q(i,k) = Q(i,k) + QH(i,k)
            QH(i,k)= 0.; NH(i,k)= 0.
         endif
         if (QHM(i,k)<epsQ .or. NHM(i,k)<epsN) then
            QM(i,k) = QM(i,k) + QHM(i,k)
            QHM(i,k)= 0.; NHM(i,k)= 0.
         endif
       ELSE
         if (QH(i,k)<epsQ) then
            Q(i,k) = Q(i,k) + QH(i,k)
            QH(i,k)= 0.
         endif
         if (QHM(i,k)<epsQ) then
            QM(i,k) = QM(i,k) + QHM(i,k)
            QHM(i,k)= 0.
         endif
       ENDIF
    enddo
  enddo
  QM = max(QM,0.)
  Q = max(Q ,0.)
  HPS= 0.5*(PSM+PS); TM = 0.5*(TM + T); QM = 0.5*(QM + Q)
  QCM= 0.5*(QCM+QC); QRM= 0.5*(QRM+QR); QIM= 0.5*(QIM+QI)
  QNM= 0.5*(QNM+QN); QGM= 0.5*(QGM+QG); QHM= 0.5*(QHM+QH)
  if (dblMom_c) NCM= 0.5*(NCM+NC)
  if (dblMom_r) NRM= 0.5*(NRM+NR)
  if (dblMom_i) NYM= 0.5*(NYM+NY)
  if (dblMom_s) NNM= 0.5*(NNM+NN)
  if (dblMom_g) NGM= 0.5*(NGM+NG)
  if (dblMom_h) NHM= 0.5*(NHM+NH)
  do k=1,nk
     do i=1,ni
        QSW(i,k)= sngl(FOQSA(TM(i,k),HPS(i)*S(i,k)))
        QSS(i,k)= sngl(FOQST( T(i,k), PS(i)*S(i,k)))
        QSI(i,k)= sngl(FOQST(TM(i,k),HPS(i)*S(i,k)))
        DE(i,k) = S(i,k)*HPS(i)/(RGASD*TM(i,k))
        iDE(i,k)= 1./DE(i,k)
     enddo
  enddo
  do i= 1,ni
     DEo = DE(i,nk)
     gamfact(i,:) = sqrt(DEo/(DE(i,:)))
     gamfact_r(i,:)= sqrt( 1./(DE(i,:)))
     do k= 2,nk-1
        WZ(i,k)= -0.5/(DE(i,k)*GRAV)*(W_omega(i,k-1)+W_omega(i,k+1))
     enddo
     WZ(i,1) = -0.5/(DE(i,1) *GRAV)*W_omega(i,1)
     WZ(i,nk)= -0.5/(DE(i,nk)*GRAV)*W_omega(i,nk)
  enddo
  activePoint = .false.
  DO k=2,nk
     DO i=1,ni
        log1= ((QIM(i,k)+QGM(i,k)+QNM(i,k)+QHM(i,k))<epsQ)
        log2= ((QCM(i,k)+QRM(i,k)) <epsQ)
        log3= ((TM(i,k)>TRPL) .and. log1)
        log4= log1.and.log2.and.(QM(i,k)<QSI(i,k))
        if (.not.( log3 .or. log4 ) .and. icephase_ON) then
          activePoint(i,k)= .true.
        endif
     ENDDO
  ENDDO
  DO k= 2,nk
    DO i= 1,ni
      IF (activePoint(i,k)) THEN
       Tc= TM(i,k)-TRPL
       if (Tc<-120. .or. Tc>50.) &
        print*, '***WARNING*** -- In MICROPHYSICS --  Ambient Temp.(C):',Tc
       Cdiff = (2.2157e-5+0.0155e-5*Tc)*1.e5/(S(i,k)*HPS(i))
       MUdyn = 1.72e-5*(393./(TM(i,k)+120.))*(TM(i,k)/TRPL)**1.5
       MUkin = MUdyn*iDE(i,k)
       iMUkin= 1./MUkin
       ScTHRD= (MUkin/Cdiff)**thrd
       Ka = 2.3971e-2 + 0.0078e-2*Tc
       Kdiff = (9.1018e-11*TM(i,k)*TM(i,k)+8.8197e-8*TM(i,k)-(1.0654e-5))
       gam = gamfact(i,k)
       Eis = min(0.05*exp(0.1*Tc),1.)
       Eig = min(0.01*exp(0.1*Tc),1.)
       Eii = 0.1*Eis
       Ess = Eis; Eih = Eig; Esh = Eig
       iEih = 1./Eih
       iEsh = 1./Esh
       qvs0 = sngl(FOQSA(TRPL,HPS(i)*S(i,k)))
       DELqvs= qvs0-(QM(i,k))
       if (QCM(i,k)>epsQ) then
          if (.not. dblMom_c) NCM(i,k)= N_c_SM
          iQCM = 1./QCM(i,k)
          iNCM = 1./NCM(i,k)
          Dc = Dm_x(DE(i,k),QCM(i,k),iNCM,icmr,thrd)
          iLAMc = iLAMDA_x(DE(i,k),QCM(i,k),iNCM,icexc9,thrd)
          iLAMc2 = iLAMc *iLAMc
          iLAMc3 = iLAMc2*iLAMc
          iLAMc4 = iLAMc2*iLAMc2
          iLAMc5 = iLAMc3*iLAMc2
       else
          Dc = 0.; iLAMc3= 0.
          iLAMc = 0.; iLAMc4= 0.
          iLAMc2 = 0.; iLAMc5= 0.
       endif
       if (QRM(i,k)>epsQ) then
          if (.not. dblMom_r) NRM(i,k)= GR50*sqrt(sqrt(GR31*iGR34*DE(i,k)*QRM(i,k)*icmr))
          iQRM = 1./QRM(i,k)
          iNRM = 1./NRM(i,k)
          Dr = Dm_x(DE(i,k),QRM(i,k),iNRM,icmr,thrd)
          iLAMr = max( iLAMmin1, iLAMDA_x(DE(i,k),QRM(i,k),iNRM,icexr9,thrd) )
          tmp1 = 1./iLAMr
          iLAMr2 = iLAMr *iLAMr
          iLAMr3 = iLAMr2*iLAMr
          iLAMr4 = iLAMr2*iLAMr2
          iLAMr5 = iLAMr3*iLAMr2
          if (Dr>40.e-6) then
             vr0 = gamfact_r(i,k)*ckQr1*iLAMr**bfr
          else
             vr0 = 0.
          endif
       else
          iLAMr = 0.; Dr = 0.; vr0 = 0.
          iLAMr2 = 0.; iLAMr3= 0.; iLAMr4= 0.; iLAMr5 = 0.
       endif
       if (QIM(i,k)>epsQ) then
          if (.not. dblMom_i) NYM(i,k)= N_Cooper(TRPL,TM(i,k))
          iQIM = 1./QIM(i,k)
          iNYM = 1./NYM(i,k)
          iLAMi = max( iLAMmin2, iLAMDA_x(DE(i,k),QIM(i,k),iNYM,icexi9,thrd) )
          iLAMi2 = iLAMi *iLAMi
          iLAMi3 = iLAMi2*iLAMi
          iLAMi4 = iLAMi2*iLAMi2
          iLAMi5 = iLAMi3*iLAMi2
          iLAMiB0= iLAMi**(bfi)
          iLAMiB1= iLAMi**(bfi+1.)
          iLAMiB2= iLAMi**(bfi+2.)
          vi0 = gamfact(i,k)*ckQi1*iLAMiB0
          Di = Dm_x(DE(i,k),QIM(i,k),iNYM,icmi,thrd)
       else
          iLAMi = 0.; vi0 = 0.; Di = 0.
          iLAMi2 = 0.; iLAMi3 = 0.; iLAMi4 = 0.; iLAMi5= 0.
          iLAMiB0= 0.; iLAMiB1= 0.; iLAMiB2= 0.
       endif
       if (QNM(i,k)>epsQ) then
          if (.not.dblMom_s) then
             No_s_SM = Nos_Thompson(TRPL,TM(i,k))
             NNM(i,k)= (No_s*GS31)**(dms*icexs2)*(GS31*iGS40*icms*DE(i,k)*QNM(i,k))** &
                       ((1.+alpha_s)*icexs2)
          endif
          iQNM = 1./QNM(i,k)
          iNNM = 1./NNM(i,k)
          iLAMs = max( iLAMmin2, iLAMDA_x(DE(i,k),QNM(i,k),iNNM,iGS20,idms) )
          iLAMs_D3= max(iLAMmin2, iLAMDA_x(DE(i,k),QNM(i,k),iNNM,iGS20_D3,thrd) )
          iLAMs2 = iLAMs*iLAMs
          iLAMsB0= iLAMs**(bfs)
          iLAMsB1= iLAMs**(bfs+1.)
          iLAMsB2= iLAMs**(bfs+2.)
          vs0 = gamfact(i,k)*ckQs1*iLAMsB0
          Ds = min(DsMax, Dm_x(DE(i,k),QNM(i,k),iNNM,icms,idms))
          if (snowSpherical) then
             des = desFix
          else
             des = des_OF_Ds(Ds,desMax,eds,fds)
          endif
         if (dblMom_s) then
            No_s= NNM(i,k)*iGS31/iLAMs_D3
         else
            No_s= No_s_SM
         endif
         VENTs= Avx*GS32*iLAMs_D3**2. + Bvx*ScTHRD*sqrt(gamfact(i,k)*afs*iMUkin)*GS09* &
                iLAMs_D3**cexs1
       else
          iLAMs = 0.; vs0 = 0.; Ds = 0.; iLAMs2= 0.
          iLAMsB0= 0.; iLAMsB1= 0.; iLAMsB1= 0.
          des = desFix
       endif
       ides = 1./des
       if (QGM(i,k)>epsQ) then
          if (.not.dblMom_g) NGM(i,k)= GG50*sqrt(sqrt(GG31*GG34*DE(i,k)*QGM(i,k)*icmg))
          iQGM = 1./QGM(i,k)
          iNGM = 1./NGM(i,k)
          iLAMg = max( iLAMmin1, iLAMDA_x(DE(i,k),QGM(i,k),iNGM,iGG99,thrd) )
          iLAMg2 = iLAMg *iLAMg
          iLAMgB0= iLAMg**(bfg)
          iLAMgB1= iLAMg**(bfg+1.)
          iLAMgB2= iLAMg**(bfg+2.)
          if (dblMom_g) then
             No_g= NGM(i,k)*iGG31/iLAMg
          else
             No_g= No_g_SM
          endif
          vg0 = gamfact(i,k)*ckQg1*iLAMgB0
          Dg = Dm_x(DE(i,k),QGM(i,k),iNGM,icmg,thrd)
       else
          iLAMg = 0.; vg0 = 0.; Dg = 0.; No_g = 0.
          iLAMg2 = 0.; iLAMgB0= 0.; iLAMgB1= 0.; iLAMgB1= 0.
       endif
       if (QHM(i,k)>epsQ) then
          if (.not.dblMom_h) NHM(i,k)= GH50*sqrt(sqrt(GH31*iGH34*DE(i,k)*QHM(i,k)*icmh))
          iQHM = 1./QHM(i,k)
          iNHM = 1./NHM(i,k)
          iLAMh = max( iLAMmin1, iLAMDA_x(DE(i,k),QHM(i,k),iNHM,iGH99,thrd) )
          iLAMh2 = iLAMh*iLAMh
          iLAMhB0= iLAMh**(bfh)
          iLAMhB1= iLAMh**(bfh+1.)
          iLAMhB2= iLAMh**(bfh+2.)
          if (dblMom_h) then
               No_h= NHM(i,k)*iGH31/iLAMh**(1.+alpha_h)
          else
               No_h= No_h_SM
          endif
          vh0 = gamfact(i,k)*ckQh1*iLAMhB0
          Dh = Dm_x(DE(i,k),QHM(i,k),iNHM,icmh,thrd)
       else
          iLAMh = 0.; vh0 = 0.; Dh = 0.; No_h= 0.
          iLAMhB0= 0.; iLAMhB1= 0.; iLAMhB1= 0.
       endif
       QNUvi=0.; QVDvi=0.; QVDvs=0.; QVDvg=0.; QVDvh=0.
       QCLcs=0.; QCLcg=0.; QCLch=0.; QFZci=0.; QCLri=0.; QMLsr=0.
       QCLrs=0.; QCLrg=0.; QMLgr=0.; QCLrh=0.; QMLhr=0.; QFZrh=0.
       QMLir=0.; QCLsr=0.; QCLsh=0.; QCLgr=0.; QCNgh=0.
       QCNis=0.; QCLir=0.; QCLis=0.; QCLih=0.
       QIMsi=0.; QIMgi=0.; QCNsg=0.; QHwet=0.
       NCLcs= 0.; NCLcg=0.; NCLch=0.; NFZci=0.; NMLhr=0.; NhCNgh=0.
       NCLri= 0.; NCLrs=0.; NCLrg=0.; NCLrh=0.; NMLsr=0.; NMLgr=0.
       NMLir= 0.; NSHhr=0.; NNUvi=0.; NVDvi=0.; NVDvh=0.; QCLig=0.
       NCLir= 0.; NCLis=0.; NCLig=0.; NCLih=0.; NIMsi=0.; NIMgi=0.
       NiCNis=0.; NsCNis=0.; NVDvs=0.; NCNsg=0.; NCLgr=0.; NCLsrh=0.
       NCLss= 0.; NCLsr=0.; NCLsh=0.; NCLsrs=0.; NCLgrg=0.; NgCNgh=0.
       NVDvg= 0.; NCLirg=0.; NCLsrg=0.; NCLgrh=0.; NrFZrh=0.; NhFZrh=0.
       NCLirh=0.
       Dirg=0.; Dirh=0.; Dsrs= 0.; Dsrg= 0.; Dsrh= 0.; Dgrg=0.; Dgrh=0.
       if (QNM(i,k)>epsQ) then
          if (QCM(i,k)>epsQ) then
             Ecs= min(Dc,30.e-6)*3.333e+4*sqrt(min(Ds,1.e-3)*1.e+3)
             QCLcs= dt*gam*afs*cmr*Ecs*PIov4*iDE(i,k)*(NCM(i,k)*NNM(i,k))*iGC5*iGS31* &
                    (GC13*GS13*iLAMc3*iLAMsB2+2.*GC14*GS12*iLAMc4*iLAMsB1+GC15*GS11* &
                    iLAMc5*iLAMsB0)
             NCLcs= dt*gam*afs*PIov4*Ecs*(NCM(i,k)*NNM(i,k))*iGC5*iGS31*(GC5*GS13* &
                    iLAMsB2+2.*GC11*GS12*iLAMc*iLAMsB1+GC12*GS11*iLAMc2*iLAMsB0)
             if (.not. snowSpherical) then
                tmp1 = 0.6366
                QCLcs= tmp1*QCLcs
                NCLcs= tmp1*NCLcs
             endif
             QCLcs= min(QCLcs, QCM(i,k))
             NCLcs= min(NCLcs, NCM(i,k))
          else
             QCLcs= 0.; NCLcs= 0.
          endif
          if (QIM(i,k)>epsQ) then
             tmp1= vs0-vi0
             tmp3= sqrt(tmp1*tmp1+0.04*vs0*vi0)
             QCLis= dt*cmi*iDE(i,k)*PI*6.*Eis*(NYM(i,k)*NNM(i,k))*tmp3*iGI31*iGS31*(0.5* &
                    iLAMs2*iLAMi3+2.*iLAMs*iLAMi4+5.*iLAMi5)
             NCLis= dt*PIov4*Eis*(NYM(i,k)*NNM(i,k))*GI31*GS31*tmp3*(GI33*GS31*iLAMi2+ &
                    2.*GI32*GS32*iLAMi*iLAMs+GI31*GS33*iLAMs2)
             QCLis= min(QCLis, (QIM(i,k)))
             NCLis= min(QCLis*(NYM(i,k)*iQIM), NCLis)
          else
             QCLis= 0.; NCLis= 0.
          endif
          if (dblMom_s) then
             NCLss= dt*0.93952*Ess*(DE(i,k)*(QNM(i,k)))**((2.+bfs)*thrd)*(NNM(i,k))** &
                    ((4.-bfs)*thrd)
             NCLss= min(NCLss, 0.5*(NNM(i,k)))
          endif
       else
          QCLcs= 0.; NCLcs= 0.; QCLis= 0.; NCLis= 0.; NCLss= 0.
       endif
       if (QGM(i,k)>epsQ) then
          if (QCM(i,k)>epsQ) then
             Kstoke = dew*vg0*Dc*Dc/(9.*MUdyn*Dg)
             Kstoke = max(1.5,min(10.,Kstoke))
             Ecg = 0.55*log10(2.51*Kstoke)
             QCLcg= dt*gam*afg*cmr*Ecg*PIov4*iDE(i,k)*(NCM(i,k)*NGM(i,k))*iGC5*iGG31* &
                    (GC13*GG13*iLAMc3*iLAMgB2+ 2.*GC14*GG12*iLAMc4*iLAMgB1+GC15*GG11* &
                    iLAMc5*iLAMgB0)
             NCLcg= dt*gam*afg*PIov4*Ecg*(NCM(i,k)*NGM(i,k))*iGC5*iGG31*(GC5*GG13* &
                    iLAMgB2+2.*GC11*GG12*iLAMc*iLAMgB1+GC12*GG11*iLAMc2*iLAMgB0)
             QCLcg= min(QCLcg, (QCM(i,k)))
             NCLcg= min(NCLcg, (NCM(i,k)))
          else
             QCLcg= 0.; NCLcg= 0.
          endif
          if (QIM(i,k)>epsQ) then
             tmp1= vg0-vi0
             tmp3= sqrt(tmp1*tmp1+0.04*vg0*vi0)
             QCLig= dt*cmi*iDE(i,k)*PI*6.*Eig*(NYM(i,k)*NGM(i,k))*tmp3*iGI31*iGG31*(0.5* &
                    iLAMg2*iLAMi3+2.*iLAMg*iLAMi4+5.*iLAMi5)
             NCLig= dt*PIov4*Eig*(NYM(i,k)*NGM(i,k))*GI31*GG31*tmp3*(GI33*GG31*iLAMi2+ &
                    2.*GI32*GG32*iLAMi*iLAMg+GI31*GG33*iLAMg2)
             QCLig= min(QCLig, (QIM(i,k)))
             NCLig= min(QCLig*(NYM(i,k)*iQIM), NCLig)
          else
             QCLig= 0.; NCLig= 0.
          endif
       else
          QCLcg= 0.; QCLrg= 0.; QCLig= 0.
          NCLcg= 0.; NCLrg= 0.; NCLig= 0.
       endif
       if (QHM(i,k)>epsQ) then
          if (QCM(i,k)>epsQ) then
             Ech = exp(-8.68e-7*Dc**(-1.6)*Dh)
             QCLch= dt*gam*afh*cmr*Ech*PIov4*iDE(i,k)*(NCM(i,k)*NHM(i,k))*iGC5*iGH31* &
                    (GC13*GH13*iLAMc3*iLAMhB2+2.*GC14*GH12*iLAMc4*iLAMhB1+GC15*GH11* &
                    iLAMc5*iLAMhB0)
             NCLch= dt*gam*afh*PIov4*Ech*(NCM(i,k)*NHM(i,k))*iGC5*iGH31*(GC5*GH13* &
                    iLAMhB2+2.*GC11*GH12*iLAMc*iLAMhB1+GC12*GH11*iLAMc2*iLAMhB0)
             QCLch= min(QCLch, QCM(i,k))
             NCLch= min(NCLch, NCM(i,k))
          else
             QCLch= 0.; NCLch= 0.
          endif
          if (QRM(i,k)>epsQ) then
             tmp1= vh0-vr0
             tmp3= sqrt(tmp1*tmp1+0.04*vh0*vr0)
             QCLrh= dt*cmr*Erh*PIov4*iDE(i,k)*(NHM(i,k)*NRM(i,k))*iGR31*iGH31*tmp3* &
                    (GR36*GH31*iLAMr5+2.*GR35*GH32*iLAMr4*iLAMh+GR34*GH33*iLAMr3*iLAMh2)
             NCLrh= dt*PIov4*Erh*(NHM(i,k)*NRM(i,k))*iGR31*iGH31*tmp3*(GR33*GH31* &
                    iLAMr2+2.*GR32*GH32*iLAMr*iLAMh+GR31*GH33*iLAMh2)
             QCLrh= min(QCLrh, QRM(i,k))
             NCLrh= min(NCLrh, QCLrh*(NRM(i,k)*iQRM))
          else
             QCLrh= 0.; NCLrh= 0.
          endif
          if (QIM(i,k)>epsQ) then
             tmp1 = vh0-vi0
             tmp3 = sqrt(tmp1*tmp1+0.04*vh0*vi0)
             QCLih= dt*cmi*iDE(i,k)*PI*6.*Eih*(NYM(i,k)*NHM(i,k))*tmp3*iGI31*iGH31*(0.5* &
                    iLAMh2*iLAMi3+2.*iLAMh*iLAMi4+5.*iLAMi5)
             NCLih= dt*PIov4*Eih*(NYM(i,k)*NHM(i,k))*GI31*GH31*tmp3*(GI33*GH31*iLAMi2+ &
                    2.*GI32*GH32*iLAMi*iLAMh+GI31*GH33*iLAMh2)
             QCLih= min(QCLih, QIM(i,k))
             NCLih= min(QCLih*(NYM(i,k)*iQIM), NCLih)
          else
             QCLih= 0.; NCLih= 0.
          endif
          if (QNM(i,k)>epsQ) then
             tmp1 = vh0-vs0
             tmp3 = sqrt(tmp1*tmp1+0.04*vh0*vs0)
             tmp4 = iLAMs2*iLAMs2
             if (snowSpherical) then
                QCLsh= dt*cms*iDE(i,k)*PI*6.*Esh*(NNM(i,k)*NHM(i,k))*tmp3*iGS31*iGH31* &
                       (0.5*iLAMh2*iLAMs2*iLAMs+2.*iLAMh*tmp4+5.*tmp4*iLAMs)
             else
                QCLsh= dt*cms*iDE(i,k)*PI*0.25*Esh*tmp3*NNM(i,k)*NHM(i,k)*iGS31*iGH31* &
                       (GH33*GS33*iLAMh**2.*iLAMs**2. + 2.*GH32*GS34*iLAMh*iLAMs**3. + &
                        GH31*GS35*iLAMs**4.)
             endif
             NCLsh= dt*PIov4*Esh*(NNM(i,k)*NHM(i,k))*GS31*GH31*tmp3*(GS33*GH31*iLAMs2+ &
                    2.*GS32*GH32*iLAMs*iLAMh+GS31*GH33*iLAMh2)
             QCLsh= min(QCLsh, (QNM(i,k)))
             NCLsh= min((NNM(i,k)*iQNM)*QCLsh, NCLsh, (NNM(i,k)))
          else
             QCLsh= 0.; NCLsh= 0.
          endif
          VENTh= Avx*GH32*iLAMh**(2.+alpha_h) + Bvx*ScTHRD*sqrt(gam*afh*iMUkin)*GH09* &
                 iLAMh**(2.5+0.5*bfh+alpha_h)
          QHwet= max(0., dt*PI2*(DE(i,k)*CHLC*Cdiff*DELqvs-Ka*Tc)*No_h*iDE(i,k)/(CHLF+ &
                 CPW*Tc)*VENTh+(QCLih*iEih+QCLsh*iEsh)*(1.-CPI*Tc/(CHLF+CPW*Tc)) )
       else
          QCLch= 0.; QCLrh= 0.; QCLih= 0.; QCLsh= 0.; QHwet= 0.
          NCLch= 0.; NCLrh= 0.; NCLsh= 0.; NCLih= 0.
       endif
       IF (TM(i,k)>TRPL .and. warmphase_ON) THEN
          QMLir = QIM(i,k)
          QIM(i,k)= 0.
          NMLir = NYM(i,k)
          if (QNM(i,k)>epsQ) then
             QMLsr= dt*(PI2*iDE(i,k)*iCHLF*No_s*VENTs*(Ka*Tc-CHLC*Cdiff*DELqvs) + CPW* &
                    iCHLF*Tc*(QCLcs+QCLrs)*idt)
             QMLsr= min(max(QMLsr,0.), QNM(i,k))
             NMLsr= NNM(i,k)*iQNM*QMLsr
          else
             QMLsr= 0.; NMLsr= 0.
          endif
          if (QGM(i,k)>epsQ) then
             VENTg= Avx*GG32*iLAMg*iLAMg+Bvx*ScTHRD*sqrt(gam*afg*iMUkin)*GG09*iLAMg** &
                    (2.5+0.5*bfg+alpha_g)
             QMLgr= dt*(PI2*iDE(i,k)*iCHLF*No_g*VENTg*(Ka*Tc-CHLC*Cdiff*DELqvs) + CPW* &
                    iCHLF*Tc*(QCLcg+QCLrg)*idt)
             QMLgr= min(max(QMLgr,0.), QGM(i,k))
             NMLgr= NGM(i,k)*iQGM*QMLgr
          else
             QMLgr= 0.; NMLgr= 0.
          endif
          if (QHM(i,k)>epsQ.and.Tc>5.) then
             VENTh= Avx*GH32*iLAMh**(2.+alpha_h) + Bvx*ScTHRD*sqrt(gam*afh*iMUkin)*GH09* &
                    iLAMh**(2.5+0.5*bfh+alpha_h)
             QMLhr= dt*(PI2*iDE(i,k)*iCHLF*No_h*VENTh*(Ka*Tc-CHLC*Cdiff*DELqvs) + CPW/ &
                    CHLF*Tc*(QCLch+QCLrh)*idt)
             QMLhr= min(max(QMLhr,0.), QHM(i,k))
             NMLhr= NHM(i,k)*iQHM*QMLhr
             if(QCLrh>0.) NMLhr= NMLhr*0.1
          else
             QMLhr= 0.; NMLhr= 0.
          endif
          QNUvi= 0.; QFZci= 0.; QVDvi= 0.; QVDvs= 0.; QVDvg= 0.
          QCLis= 0.; QCNis1=0.; QCNis2=0.
          QCNgh= 0.; QIMsi= 0.; QIMgi= 0.; QCLir= 0.; QCLri= 0.
          QCLrs= 0.; QCLgr= 0.; QCLrg= 0.; QCNis= 0.; QVDvh= 0.
          QCNsg= 0.; QCLsr= 0.
          NNUvi= 0.; NFZci= 0.; NCLgr= 0.; NCLrg= 0.; NgCNgh= 0.
          NCLis= 0.; NVDvi= 0.; NVDvs= 0.; NVDvg= 0.; NVDvh= 0.
          NCNsg= 0.; NhCNgh= 0.; NiCNis=0.; NsCNis=0.; NCLrs= 0.
          NIMsi= 0.; NIMgi= 0.; NCLir= 0.; NCLri= 0.; NCLsr= 0.
       ELSE
          tmp1 = 1./QSI(i,k)
          Si = QM(i,k) *tmp1
          tmp2 = TM(i,k)*TM(i,k)
          iABi = 1./( CHLS*CHLS/(Ka*RGASV*tmp2) + 1./(DE(i,k)*(QSI(i,k))*Cdiff) )
          QMLir= 0.; QMLsr= 0.; QMLgr= 0.; QMLhr= 0.
          NMLir= 0.; NMLsr= 0.; NMLgr= 0.; NMLhr= 0.
          if (Tc<Tc_FZrh .and. QRM(i,k)>epsQ .and. hail_ON) then
             NrFZrh= -dt*Bbigg*(exp(Abigg*Tc)-1.)*DE(i,k)*QRM(i,k)*idew
             Rz= 1.
             NhFZrh= Rz*NrFZrh
             QFZrh = NrFZrh*(QRM(i,k)*iNRM)
          else
             QFZrh= 0.; NrFZrh= 0.; NhFZrh= 0.
          endif
          if (dblMom_c) then
             if (QCM(i,k)>epsQ) then
                tmp2 = Tc*Tc; tmp3= tmp2*Tc; tmp4= tmp2*tmp2
                JJ = (10.**max(-20.,(-606.3952-52.6611*Tc-1.7439*tmp2-0.0265*tmp3- &
                         1.536e-4*tmp4)))
                tmp1 = 1.e6*(DE(i,k)*(QCM(i,k)*iNCM)*icmr)
                FRAC = 1.-exp(-JJ*PIov6*tmp1*dt)
                if (Tc>-30.) FRAC= 0.
                if (Tc<-50.) FRAC= 1.
                QFZci= FRAC*QCM(i,k)
                NFZci= FRAC*NCM(i,k)
             else
                QFZci= 0.; NFZci= 0.
             endif
          else
             if (QCM(i,k)>epsQ .and. Tc<-35.) then
                FRAC= 1.
                QFZci= FRAC*QCM(i,k)
                NFZci= FRAC*N_c_SM
             else
                QFZci= 0.; NFZci= 0.
             endif
          endif
          if (dblMom_i) then
            NNUvi= 0.; QNUvi= 0.
            if (primIceNucl==1) then
               NuDEPSOR= 0.; NuCONT= 0.
               Simax = min(Si, SxFNC(WZ(i,k),Tc,HPS(i)*S(i,k),QSW(i,k),QSI(i,k),CCNtype, &
                              2))
               tmp1 = T(i,k)-7.66
               NNUmax = max(0., DE(i,k)/mio*(Q(i,k)-QSS(i,k))/(1.+ck6*(QSS(i,k)/(tmp1* &
                        tmp1))))
               if (Tc<-5. .and. Si>1.) then
                  NuDEPSOR= max(0., 1.e3*exp(12.96*(Simax-1.)-0.639)-(NYM(i,k)))
               endif
               if (QCM(i,k)>epsQ .and. Tc<-2.) then
                  GG = 1.*idew/(RGASV*(TM(i,k))/((QSW(i,k)*HPS(i)*S(i,k))/EPS1)/ &
                              Cdiff+CHLC/Ka/(TM(i,k))*(CHLC/RGASV/(TM(i,k))-1.))
                  Swmax = SxFNC(WZ(i,k),Tc,HPS(i)*S(i,k),QSW(i,k),QSI(i,k),CCNtype,1)
                  ssat = min((QM(i,k)/QSW(i,k)), Swmax) -1.
                  Tcc = Tc + GG*ssat*CHLC/Kdiff
                  Na = exp(4.11-0.262*Tcc)
                  Kn = LAMa0*(TM(i,k))*p0/(T0*(HPS(i)*S(i,k))*Ra)
                  PSIa = -kBoltz*Tcc/(6.*pi*Ra*MUdyn)*(1.+Kn)
                  ft = 0.4*(1.+1.45*Kn+0.4*Kn*exp(-1./Kn))*(Ka+2.5*Kn*KAPa)/ &
                           (1.+3.*Kn)/(2.*Ka+5.*KAPa*Kn+KAPa)
                  Dc = (DE(i,k)*(QCM(i,k)*iNCM)*icmr)**thrd
                  F1 = PI2*Dc*Na*(NCM(i,k))
                  F2 = Ka/(HPS(i)*S(i,k))*(Tc-Tcc)
                  NuCONTA= -F1*F2*RGASV*(TM(i,k))/CHLC*iDE(i,k)
                  NuCONTB= F1*F2*ft*iDE(i,k)
                  NuCONTC= F1*PSIa
                  NuCONT = max(0.,(NuCONTA+NuCONTB+NuCONTC)*dt)
               endif
               if (icephase_ON) then
                  NNUvi= min(NNUmax, NuDEPSOR + NuCONT )
                  QNUvi= mio*iDE(i,k)*NNUvi
                  QNUvi= min(QNUvi,(Q(i,k)))
               endif
            elseif (primIceNucl==2) then
               if (Tc<-5. .and. Si>1.08) then
                  NNUvi= max(N_Cooper(TRPL,T(i,k))-NYM(i,k),0.)
                  QNUvi= min(mio*iDE(i,k)*NNUvi, Q(i,k))
               endif
            endif
          else
             if (QIM(i,k)<=epsQ .and. Tc<-5. .and. Si>1.08) then
                NNUvi = N_Cooper(TRPL,T(i,k))
                QNUvi= mio*iDE(i,k)*NNUvi
                QNUvi= min(QNUvi,Q(i,k))
             endif
          endif
          IF (QIM(i,k)>epsQ) THEN
             No_i = NYM(i,k)*iGI31/iLAMi
             VENTi= Avx*GI32*iLAMi*iLAMi+Bvx*ScTHRD*sqrt(gam*afi*iMUkin)*GI6*iLAMi** &
                    (2.5+0.5*bfi+alpha_i)
             QVDvi= dt*capFact_i*iABi*(PI2*(Si-1.)*No_i*VENTi)
             tmp1 = T(i,k)-7.66
             VDmax = (Q(i,k)-QSS(i,k))/(1.+ck6*(QSS(i,k))/(tmp1*tmp1))
             if(Si>=1.) then
                QVDvi= min(max(QVDvi,0.),VDmax)
             else
                if (VDmax<0.) QVDvi= max(QVDvi,VDmax)
             endif
             if (.not. iceDep_ON) QVDvi= 0.
             NVDvi= min(0., (NYM(i,k)*iQIM)*QVDvi)
             mi= DE(i,k)*(QIM(i,k)*iNYM)
             if (mi<=0.5*mso.and.abs(0.5*mso-mi)>1.e-20) then
                QCNis1= (mi/(mso-mi))*QVDvi
             else
                QCNis1= QVDvi + (1.-0.5*mso/mi)*QIM(i,k)
             endif
             QCNis1= max(0., QCNis1)
             if(Di<0.5*Dso) then
                Ki = PIov6*Di*Di*vi0*Eii*Xdisp
                tmp1 = log(Di/Dso)
                tmp2 = tmp1*tmp1*tmp1
                QCNis2= -dt*0.5*(QIM(i,k)*NYM(i,k))*Ki/tmp2
             else
                Ki= 0.; QCNis2= 0.
             endif
             QCNis = QCNis1 + QCNis2
             NsCNis= DE(i,k)*imso*QCNis
             NiCNis= (DE(i,k)*imso*QCNis1 + 0.5*Ki*NYM(i,k)*NYM(i,k))
             NiCNis= min(NiCNis, NYM(i,k)*0.1)
             if (.not.(snow_ON)) then
                QCNis= 0.; NiCNis= 0.; NsCNis= 0.
             endif
             if (QRM(i,k)>epsQ .and. QIM(i,k)>epsQ) then
                tmp1 = vr0-vi0
                tmp3 = sqrt(tmp1*tmp1+0.04*vr0*vi0)
                QCLir= dt*cmi*Eri*PIov4*iDE(i,k)*(NRM(i,k)*NYM(i,k))*iGI31*iGR31*tmp3* &
                       (GI36*GR31*iLAMi5+2.*GI35*GR32*iLAMi4*iLAMr+GI34*GR33*iLAMi3* &
                       iLAMr2)
                NCLri= dt*PIov4*Eri*(NRM(i,k)*NYM(i,k))*iGI31*iGR31*tmp3*(GI33*GR31* &
                       iLAMi2+2.*GI32*GR32*iLAMi*iLAMr+GI31*GR33*iLAMr2)
                QCLri= dt*cmr*Eri*PIov4*iDE(i,k)*(NYM(i,k)*NRM(i,k))*iGR31*iGI31*tmp3* &
                       (GR36*GI31 *iLAMr5+2.*GR35*GI32*iLAMr4*iLAMi+GR34*GI33*iLAMr3* &
                       iLAMi2)
                NCLir= min(QCLir*(NYM(i,k)*iQIM), NCLri)
                QCLri= min(QCLri, (QRM(i,k))); QCLir= min(QCLir, (QIM(i,k)))
                NCLri= min(NCLri, (NRM(i,k))); NCLir= min(NCLir, (NYM(i,k)))
                tmp1= max(Di,Dr)
                dey= (dei*Di*Di*Di+dew*Dr*Dr*Dr)/(tmp1*tmp1*tmp1)
                if (dey>0.5*(deg+deh) .and. Dr>Dr_3cmpThrs .and. hail_ON) then
                   Dirg= 0.; Dirh= 1.
                else
                   Dirg= 1.; Dirh= 0.
                endif
                if (.not. grpl_ON) Dirg= 0.
             else
                QCLir= 0.; NCLir= 0.; QCLri= 0.
                NCLri= 0.; Dirh = 0.; Dirg= 0.
             endif
             ff= 0.
             if(Tc>=-8..and.Tc<=-5.) ff= 3.5e8*(Tc +8.)*thrd
             if(Tc> -5..and.Tc< -3.) ff= 3.5e8*(-3.-Tc)*0.5
             NIMsi= DE(i,k)*ff*QCLcs
             NIMgi= DE(i,k)*ff*QCLcg
             QIMsi= mio*iDE(i,k)*NIMsi
             QIMgi= mio*iDE(i,k)*NIMgi
          ELSE
             QVDvi= 0.; QCNis= 0.
             QIMsi= 0.; QIMgi= 0.; QCLri= 0.; QCLir= 0.
             NVDvi= 0.; NCLir= 0.; NIMsi= 0.
             NiCNis=0.; NsCNis=0.; NIMgi= 0.; NCLri= 0.
          ENDIF
          IF (QNM(i,k)>epsQ) THEN
             QVDvs = dt*capFact_s*iABi*(PI2*(Si-1.)*No_s*VENTs - CHLS*CHLF/(Ka*RGASV* &
                     TM(i,k)*TM(i,k))*QCLcs*idt)
             tmp1 = T(i,k)-7.66
             VDmax = (Q(i,k)-QSS(i,k))/(1.+ck6*(QSS(i,k))/(tmp1*tmp1))
             if(Si>=1.) then
                QVDvs= min(max(QVDvs,0.),VDmax)
             else
                if (VDmax<0.) QVDvs= max(QVDvs,VDmax)
             endif
             NVDvs= -min(0.,(NNM(i,k)*iQNM)*QVDvs)
             if (QCLcs>CNsgThres*QVDvs .and. 0.99*deg>des) then
                QCNsg= (deg/(deg-des))*QCLcs
             else
                QCNsg= 0.
             endif
             if (.not. grpl_ON) QCNsg= 0.
             NCNsg= DE(i,k)*imgo*QCNsg
             NCNsg= min(NCNsg, (0.5*NNM(i,k)*iQNM)*QCNsg)
              if (QRM(i,k)>epsQ .and. QNM(i,k)>epsQ .and. Tc<-5.) then
                tmp1 = vs0-vr0
                tmp2 = sqrt(tmp1*tmp1+0.04*vs0*vr0)
                tmp6 = iLAMs2*iLAMs2*iLAMs
                QCLrs= dt*cmr*Ers*PIov4*iDE(i,k)*NNM(i,k)*NRM(i,k)*iGR31*iGS31*tmp2* &
                       (GR36*GS31*iLAMr5+2.*GR35*GS32*iLAMr4*iLAMs+GR34*GS33*iLAMr3* &
                       iLAMs2)
                NCLrs= dt*0.25e0*PI*Ers*(NNM(i,k)*NRM(i,k))*iGR31*iGS31*tmp2*(GR33* &
                       GS31*iLAMr2+2.*GR32*GS32*iLAMr*iLAMs+GR31*GS33*iLAMs2)
                if (snowSpherical) then
                   QCLsr= dt*cms*Ers*PIov4*iDE(i,k)*(NRM(i,k)*NNM(i,k))*iGS31*iGR31* &
                          tmp2*(GS36*GR31*tmp6+2.*GS35*GR32*iLAMs2*iLAMs2*iLAMr+GS34* &
                          GR33*iLAMs2*iLAMs*iLAMr2)
                else
                   QCLsr= dt*cms*iDE(i,k)*PI*0.25*ERS*tmp2*NNM(i,k)*NRM(i,k)*iGS31* &
                          iGR31*(GR33*GS33*iLAMr**2.*iLAMs**2. + 2.*GR32*GS34*iLAMr* &
                          iLAMs**3. +GR31*GS35*iLAMs**4.)
                endif
                NCLsr= min(QCLsr*(NNM(i,k)*iQNM), NCLrs)
                QCLrs= min(QCLrs, QRM(i,k)); QCLsr= min(QCLsr, QNM(i,k))
                NCLrs= min(NCLrs, NRM(i,k)); NCLsr= min(NCLsr, NNM(i,k))
                Dsrs= 0.; Dsrg= 0.; Dsrh= 0.
                tmp1= max(Ds,Dr)
                tmp2= tmp1*tmp1*tmp1
                dey = (des*Ds*Ds*Ds + dew*Dr*Dr*Dr)/tmp2
                if (dey<=0.5*(des+deg) ) Dsrs= 1.
                if (dey >0.5*(des+deg) .and. dey<0.5*(deg+deh)) Dsrg= 1.
                if (dey>=0.5*(deg+deh)) then
                   Dsrh= 1.
                   if (.not.hail_ON .or. Dr<Dr_3cmpThrs) then
                      Dsrg= 1.; Dsrh= 0.
                   endif
                endif
                if (.not. grpl_ON) Dsrg=0.
             else
                QCLrs= 0.; QCLsr= 0.; NCLrs= 0.; NCLsr= 0.
             endif
          ELSE
             QVDvs= 0.; QCLcs= 0.; QCNsg= 0.; QCLsr= 0.; QCLrs= 0.
             NVDvs= 0.; NCLcs= 0.; NCLsr= 0.; NCLrs= 0.; NCNsg= 0.
          ENDIF
          IF (QGM(i,k)>epsQ) THEN
             if (WZ(i,k)>w_CNgh .and. hail_ON) then
                D_sll = 0.01*(exp(min(20.,-Tc/(1.1e4*DE(i,k)*(QCM(i,k)+QRM(i,k))-1.3e3* &
                        DE(i,k)*(QIM(i,k))+1.)))-1.)
                D_sll = 2.0*D_sll
                D_sll = min(1., max(0.0001,D_sll))
                tmp1 = exp(-D_sll/iLAMg)
                Ng_tail = No_g*iLAMg*tmp1
                if (Ng_tail > Ngh_crit) then
                   QCNgh = idt*cmg*No_g*tmp1*(D_sll**3.*iLAMg + 3.*D_sll**2.*iLAMg**2. &
                           + 6.*D_sll*iLAMg**3. + 6.*iLAMg**4.)
                   NgCNgh= idt*No_g*iLAMg*tmp1
                   Rz= 1.
                   NhCNgh= Rz*NgCNgh
                else
                   QCNgh = 0.; NgCNgh = 0.; NhCNgh = 0.
                endif
             endif
             if (QRM(i,k)>epsQ) then
                tmp1 = vg0-vr0
                tmp2 = sqrt(tmp1*tmp1 + 0.04*vg0*vr0)
                tmp8 = iLAMg2*iLAMg
                tmp9 = tmp8*iLAMg
                tmp10= tmp9*iLAMg
                Kstoke = dew*abs(vg0-vr0)*Dr*Dr/(9.*MUdyn*Dg)
                Kstoke = max(1.5,min(10.,Kstoke))
                Erg = 0.55*log10(2.51*Kstoke)
                QCLrg= dt*cmr*Erg*PIov4*iDE(i,k)*(NGM(i,k)*NRM(i,k))*iGR31*iGG31*tmp2* &
                       (GR36*GG31*iLAMr5+2.*GR35*GG32*iLAMr4*iLAMg+GR34*GG33*iLAMr3* &
                       iLAMg2)
                NCLrg= dt*PIov4*Erg*(NGM(i,k)*NRM(i,k))*iGR31*iGG31*tmp2*(GR33*GG31* &
                       iLAMr2+2.*GR32*GG32*iLAMr*iLAMg+GR31*GG33*iLAMg2)
                QCLgr= dt*cmg*Erg*PIov4*iDE(i,k)*(NRM(i,k)*NGM(i,k))*iGG31*iGR31*tmp2* &
                       (GG36*GR31*tmp10+2.*GG35*GR32*tmp9*iLAMr+GG34*GR33*tmp8*iLAMr2)
                NCLgr= min(NCLrg, QCLgr*(NGM(i,k)*iQGM))
                QCLrg= min(QCLrg, QRM(i,k)); QCLgr= min(QCLgr, QGM(i,k))
                NCLrg= min(NCLrg, NRM(i,k)); NCLgr= min(NCLgr, NGM(i,k))
                tmp1= max(Dg,Dr)
                tmp2= tmp1*tmp1*tmp1
                dey = (deg*Dg*Dg*Dg + dew*Dr*Dr*Dr)/tmp2
                if (dey>0.5*(deg+deh) .and. Dr>Dr_3cmpThrs .and. hail_ON) then
                   Dgrg= 0.; Dgrh= 1.
                else
                   Dgrg= 1.; Dgrh= 0.
                endif
             else
                QCLgr= 0.; QCLrg= 0.; NCLgr= 0.; NCLrg= 0.
             endif
          ELSE
             QVDvg= 0.; QCNgh= 0.; QCLgr= 0.; QCLrg= 0.; NgCNgh= 0.
             NVDvg= 0.; NhCNgh= 0.; NCLgr= 0.; NCLrg= 0.
          ENDIF
          IF (QHM(i,k)>epsQ) THEN
             if (QHwet<(QCLch+QCLrh+QCLih+QCLsh) .and. Tc>-40.) then
                QCLih= min(QCLih*iEih, QIM(i,k))
                NCLih= min(NCLih*iEih, NYM(i,k))
                QCLsh= min(QCLsh*iEsh, QNM(i,k))
                NCLsh= min(NCLsh*iEsh, NNM(i,k))
                tmp3 = QCLrh
                QCLrh= QHwet-(QCLch+QCLih+QCLsh)
                QSHhr= tmp3-QCLrh
                NSHhr= DE(i,k)*QSHhr/(cmr*Drshed*Drshed*Drshed)
             else
                NSHhr= 0.
             endif
          ELSE
             QVDvh= 0.; NVDvh= 0.; NSHhr= 0.
          ENDIF
       ENDIF
       do niter= 1,2
          source= Q(i,k) +dim(-QVDvi,0.)+dim(-QVDvs,0.)+dim(-QVDvg,0.)+dim(-QVDvh,0.)
          sink = QNUvi+dim(QVDvi,0.)+dim(QVDvs,0.)
          sour = max(source,0.)
          if(sink>sour) then
             ratio= sour/sink
             QNUvi= ratio*QNUvi; NNUvi= ratio*NNUvi
             if(QVDvi>0.) then
               QVDvi= ratio*QVDvi; NVDvi= ratio*NVDvi
             endif
             if(QVDvs>0.) then
               QVDvs=ratio*QVDvs; NVDvs=ratio*NVDvs
             endif
             QVDvg= ratio*QVDvg; NVDvg= ratio*NVDvg
             QVDvh= ratio*QVDvh; NVDvh= ratio*NVDvh
          endif
          source= QC(i,k)
          sink = QCLcs+QCLcg+QCLch+QFZci
          sour = max(source,0.)
          if(sink>sour) then
             ratio= sour/sink
             QFZci= ratio*QFZci; NFZci= ratio*NFZci
             QCLcs= ratio*QCLcs; NCLcs= ratio*NCLcs
             QCLcg= ratio*QCLcg; NCLcg= ratio*NCLcg
             QCLch= ratio*QCLch; NCLch= ratio*NCLch
          endif
          source= QR(i,k)+QMLsr+QMLgr+QMLhr+QMLir
          sink = QCLri+QCLrs+QCLrg+QCLrh+QFZrh
          sour = max(source,0.)
          if(sink>sour) then
             ratio= sour/sink
             QCLrg= ratio*QCLrg; QCLri= ratio*QCLri; NCLri= ratio*NCLri
             QCLrs= ratio*QCLrs; NCLrs= ratio*NCLrs; QCLrg= ratio*QCLrg
             NCLrg= ratio*NCLrg; QCLrh= ratio*QCLrh; NCLrh= ratio*NCLrh
             QFZrh= ratio*QFZrh; NrFZrh=ratio*NrFZrh; NhFZrh=ratio*NhFZrh
             if (ratio==0.) then
                Dirg= 0.; Dirh= 0.; Dgrg= 0.; Dgrh= 0.
                Dsrs= 0.; Dsrg= 0.; Dsrh= 0.
              endif
          endif
          source= QI(i,k)+QNUvi+dim(QVDvi,0.)+QFZci
          sink = QCNis+QCLir+dim(-QVDvi,0.)+QCLis+QCLig+QCLih+QMLir
          sour = max(source,0.)
          if(sink>sour) then
             ratio= sour/sink
             QMLir= ratio*QMLir; NMLir= ratio*NMLir
             if (QVDvi<0.) then
                QVDvi= ratio*QVDvi; NVDvi= ratio*NVDvi
             endif
             QCNis= ratio*QCNis; NiCNis= ratio*NiCNis; NsCNis= ratio*NsCNis
             QCLir= ratio*QCLir; NCLir= ratio*NCLir; QCLig= ratio*QCLig
             QCLis= ratio*QCLis; NCLis= ratio*NCLis
             QCLih= ratio*QCLih; NCLih= ratio*NCLih
             if (ratio==0.) then
                Dirg= 0.; Dirh= 0.
             endif
          endif
          source= QN(i,k)+QCNis+dim(QVDvs,0.)+QCLis+Dsrs*(QCLrs+QCLsr)+QCLcs
          sink = dim(-QVDvs,0.)+QCNsg+QMLsr+QCLsr+QCLsh
          sour = max(source,0.)
          if(sink>sour) then
             ratio= sour/sink
             if(QVDvs<=0.) then
                QVDvs= ratio*QVDvs; NVDvs= ratio*NVDvs
             endif
             QCNsg= ratio*QCNsg; NCNsg= ratio*NCNsg; QMLsr= ratio*QMLsr
             NMLsr= ratio*NMLsr; QCLsr= ratio*QCLsr; NCLsr= ratio*NCLsr
             QCLsh= ratio*QCLsh; NCLsh= ratio*NCLsh
             if (ratio==0.) then
                Dsrs= 0.; Dsrg= 0.; Dsrh= 0.
             endif
          endif
          source= QG(i,k)+QCNsg+dim(QVDvg,0.)+Dirg*(QCLri+QCLir)+Dgrg*(QCLrg+QCLgr)+ &
                  QCLcg+Dsrg*(QCLrs+QCLsr)+QCLig
          sink = dim(-QVDvg,0.)+QMLgr+QCNgh+QCLgr
          sour = max(source,0.)
          if(sink>sour) then
             ratio= sour/sink
             QVDvg= ratio*QVDvg; NVDvg= ratio*NVDvg; QMLgr = ratio*QMLgr
             NMLgr= ratio*NMLgr; QCNgh= ratio*QCNgh; NgCNgh= ratio*NgCNgh
             QCLgr= ratio*QCLgr; NCLgr= ratio*NCLgr; NhCNgh= ratio*NhCNgh
             if (ratio==0.) then
                Dgrg= 0.; Dgrh= 0.
             endif
          endif
          source= QH(i,k)+dim(QVDvh,0.)+QCLch+QCLrh+Dirh*(QCLri+QCLir)+QCLih+QCLsh+ &
                  Dsrh*(QCLrs+QCLsr)+QCNgh+Dgrh*(QCLrg+QCLgr)+QFZrh
          sink = dim(-QVDvh,0.)+QMLhr
          sour = max(source,0.)
          if(sink>sour) then
             ratio= sour/sink
             QVDvh= ratio*QVDvh; NVDvh= ratio*NVDvh
             QMLhr= ratio*QMLhr; NMLhr= ratio*NMLhr
          endif
       enddo
       NCLirg= 0.; NCLirh= 0.; NCLsrs= 0.; NCLsrg= 0.
       NCLsrh= 0.; NCLgrg= 0.; NCLgrh= 0.
       if (QCLir+QCLri>0.) then
          tmp1 = max(Dr,Di)
          tmp2 = tmp1*tmp1*tmp1*PIov6
          NCLirg= Dirg*DE(i,k)*(QCLir+QCLri)/(deg*tmp2)
          NCLirh= Dirh*DE(i,k)*(QCLir+QCLri)/(deh*tmp2)
       endif
       if (QCLsr+QCLrs>0.) then
          tmp1 = max(Dr,Ds)
          tmp2 = tmp1*tmp1*tmp1*PIov6
          NCLsrs= Dsrs*DE(i,k)*(QCLsr+QCLrs)/(des*tmp2)
          NCLsrg= Dsrg*DE(i,k)*(QCLsr+QCLrs)/(deg*tmp2)
          NCLsrh= Dsrh*DE(i,k)*(QCLsr+QCLrs)/(deh*tmp2)
       endif
       if (QCLgr+QCLrg>0.) then
          tmp1 = max(Dr,Dg)
          tmp2 = tmp1*tmp1*tmp1*PIov6
          NCLgrg= Dgrg*DE(i,k)*(QCLgr+QCLrg)/(deg*tmp2)
          NCLgrh= Dgrh*DE(i,k)*(QCLgr+QCLrg)/(deh*tmp2)
       endif
       Q(i,k) = Q(i,k) -QNUvi -QVDvi -QVDvs -QVDvg -QVDvh
       QC(i,k)= QC(i,k) -QCLcs -QCLcg -QCLch -QFZci
       QR(i,k)= QR(i,k) -QCLri +QMLsr -QCLrs -QCLrg +QMLgr -QCLrh +QMLhr -QFZrh +QMLir
       QI(i,k)= QI(i,k) +QNUvi +QVDvi +QFZci -QCNis -QCLir -QCLis -QCLig &
                        -QMLir -QCLih +QIMsi +QIMgi
       QG(i,k)= QG(i,k) +QCNsg +QVDvg +QCLcg -QCLgr-QMLgr -QCNgh -QIMgi +QCLig &
                        +Dirg*(QCLri+QCLir) +Dgrg*(QCLrg+QCLgr) +Dsrg*(QCLrs+QCLsr)
       QN(i,k)= QN(i,k) +QCNis +QVDvs +QCLcs -QCNsg -QMLsr -QIMsi -QCLsr +QCLis -QCLsh &
                        +Dsrs*(QCLrs+QCLsr)
       QH(i,k)= QH(i,k) +Dirh*(QCLri+QCLir) -QMLhr +QVDvh +QCLch +Dsrh*(QCLrs+QCLsr) &
                        +QCLih +QCLsh +QFZrh +QCLrh +QCNgh +Dgrh*(QCLrg+QCLgr)
       if (dblMom_c) NC(i,k)= NC(i,k) -NCLcs -NCLcg -NCLch -NFZci
       if (dblMom_r) NR(i,k)= NR(i,k) -NCLri -NCLrs -NCLrg -NCLrh +NMLsr +NMLgr +NMLhr &
                                      -NrFZrh +NMLir +NSHhr
       if (dblMom_i) NY(i,k)= NY(i,k) +NNUvi +NVDvi +NFZci -NCLir -NCLis -NCLig -NCLih &
                                      -NMLir +NIMsi +NIMgi -NiCNis
       if (dblMom_s) NN(i,k)= NN(i,k) +NsCNis -NVDvs -NCNsg -NMLsr -NCLss -NCLsr -NCLsh &
                                      +NCLsrs
       if (dblMom_g) NG(i,k)= NG(i,k) +NCNsg -NCLgr -NVDvg -NMLgr +NCLirg +NCLsrg &
                                      +NCLgrg -NgCNgh
       if (dblMom_h) NH(i,k)= NH(i,k) +NhFZrh +NhCNgh -NMLhr -NVDvh +NCLirh +NCLsrh &
                                      +NCLgrh
       T(i,k)= T(i,k) +LFP*(QCLri+QCLcs+QCLrs+QFZci-QMLsr+QCLcg+QCLrg-QMLir-QMLgr &
                        -QMLhr+QCLch+QCLrh+QFZrh) +LSP*(QNUvi+QVDvi+QVDvs+QVDvg+QVDvh)
       IF (dblMom_c) THEN
         if(QC(i,k)<epsQ .or. NC(i,k)<epsN) then
            Q(i,k) = Q(i,k) + QC(i,k)
            T(i,k) = T(i,k) - LCP*QC(i,k)
            QC(i,k)= 0.; NC(i,k)= 0.
         endif
       ELSE
         if(QC(i,k)<epsQ) then
            Q(i,k) = Q(i,k) + QC(i,k)
            T(i,k) = T(i,k) - LCP*QC(i,k)
            QC(i,k)= 0.
         endif
       ENDIF
       IF (dblMom_r) THEN
         if (QR(i,k)<epsQ .or. NR(i,k)<epsN) then
            Q(i,k) = Q(i,k) + QR(i,k)
            T(i,k) = T(i,k) - LCP*QR(i,k)
            QR(i,k)= 0.; NR(i,k)= 0.
         endif
       ELSE
         if (QR(i,k)<epsQ) then
            Q(i,k) = Q(i,k) + QR(i,k)
            T(i,k) = T(i,k) - LCP*QR(i,k)
           QR(i,k)= 0.
         endif
       ENDIF
       IF (dblMom_i) THEN
         if (QI(i,k)<epsQ .or. NY(i,k)<epsN) then
            Q(i,k) = Q(i,k) + QI(i,k)
            T(i,k) = T(i,k) - LSP*QI(i,k)
            QI(i,k)= 0.; NY(i,k)= 0.
         endif
       ELSE
         if (QI(i,k)<epsQ) then
            Q(i,k) = Q(i,k) + QI(i,k)
            T(i,k) = T(i,k) - LSP*QI(i,k)
            QI(i,k)= 0.
         endif
       ENDIF
       IF (dblMom_s) THEN
         if (QN(i,k)<epsQ .or. NN(i,k)<epsN) then
            Q(i,k) = Q(i,k) + QN(i,k)
            T(i,k) = T(i,k) - LSP*QN(i,k)
            QN(i,k)= 0.; NN(i,k)= 0.
         endif
       ELSE
         if (QN(i,k)<epsQ) then
            Q(i,k) = Q(i,k) + QN(i,k)
            T(i,k) = T(i,k) - LSP*QN(i,k)
            QN(i,k)= 0.
         endif
       ENDIF
       IF (dblMom_g) THEN
         if (QG(i,k)<epsQ .or. NG(i,k)<epsN) then
            Q(i,k) = Q(i,k) + QG(i,k)
            T(i,k) = T(i,k) - LSP*QG(i,k)
            QG(i,k)= 0.; NG(i,k)= 0.
         endif
       ELSE
         if (QG(i,k)<epsQ) then
            Q(i,k) = Q(i,k) + QG(i,k)
            T(i,k) = T(i,k) - LSP*QG(i,k)
            QG(i,k)= 0.
         endif
       ENDIF
       IF (dblMom_h) THEN
         if (QH(i,k)<epsQ .or. NH(i,k)<epsN) then
            Q(i,k) = Q(i,k) + QH(i,k)
            T(i,k) = T(i,k) - LSP*QH(i,k)
            QH(i,k)= 0.; NH(i,k)= 0.
         else if (QH(i,k)>epsQ .and. NH(i,k)>epsN) then
            Dh= (DE(i,k)*QH(i,k)/NH(i,k)*icmh)**thrd
            if (Dh<Dh_min) then
               QG(i,k)= QG(i,k) + QH(i,k)
               NG(i,k)= NG(i,k) + NH(i,k)
               QH(i,k)= 0.; NH(i,k)= 0.
            endif
         endif
       ELSE
         if (QH(i,k)<epsQ) then
            Q(i,k) = Q(i,k) + QH(i,k)
            T(i,k) = T(i,k) - LSP*QH(i,k)
            QH(i,k)= 0.
         endif
       ENDIF
       Q(i,k)= max(Q(i,k),0.)
       NY(i,k)= min(NY(i,k), Ni_max)
      ENDIF
    ENDDO
  ENDDO
 IF (warmphase_ON) THEN
  DO k= 2,nk
     DO i= 1,ni
        RCAUTR= 0.; CCACCR= 0.; Dc= 0.; iLAMc= 0.; L = 0.
        RCACCR= 0.; CCSCOC= 0.; Dr= 0.; iLAMr= 0.; TAU= 0.
        CCAUTR= 0.; CRSCOR= 0.; SIGc= 0.; DrINIT= 0.
        iLAMc3= 0.; iLAMc6= 0.; iLAMr3= 0.; iLAMr6= 0.
        if (dblMom_r) then
           rainPresent= (QRM(i,k)>epsQ .and. NRM(i,k)>epsN)
        else
           rainPresent= (QRM(i,k)>epsQ)
        endif
        if (.not. dblMom_c) NCM(i,k)= N_c_SM
        if (QCM(i,k)>epsQ .and. NCM(i,k)>epsN) then
           iLAMc = iLAMDA_x(DE(i,k),QCM(i,k),1./NCM(i,k),icexc9,thrd)
           iLAMc3= iLAMc*iLAMc*iLAMc
           iLAMc6= iLAMc3*iLAMc3
           Dc = iLAMc*(GC2*iGC1)**thrd
           SIGc = iLAMc*( GC3*iGC1- (GC2*iGC1)*(GC2*iGC1) )**sixth
           L = 0.027*DE(i,k)*QCM(i,k)*(6.25e18*SIGc*SIGc*SIGc*Dc-0.4)
           if (SIGc>SIGcTHRS) TAU= 3.7/(DE(i,k)*(QCM(i,k))*(0.5e6*SIGc-7.5))
        endif
        if (rainPresent) then
           if (dblMom_r) then
              Dr = Dm_x(DE(i,k),QRM(i,k),1./NRM(i,k),icmr,thrd)
              if (Dr>3.e-3) then
                 tmp1 = (Dr-3.e-3); tmp2= (Dr/DrMAX); tmp3= tmp2*tmp2*tmp2
                 NRM(i,k)= NRM(i,k)*max((1.+2.e4*tmp1*tmp1),tmp3)
                 tmp1 = DE(i,k)*QRM(i,k)*icmr
                 Dr = (tmp1/NRM(i,k))**thrd
              endif
           else
              NRM(i,k)= GR50*sqrt(sqrt(GR31*iGR34*DE(i,k)*QRM(i,k)*icmr))
              Dr = Dm_x(DE(i,k),QRM(i,k),1./NRM(i,k),icmr,thrd)
           endif
           iLAMr = iLAMDA_x(DE(i,k),QRM(i,k),1./NRM(i,k),icexr9,thrd)
           iLAMr3= iLAMr*iLAMr*iLAMr
           iLAMr6= iLAMr3*iLAMr3
        endif
        if (QCM(i,k)>epsQ .and. SIGc>SIGcTHRS .and. autoconv_ON) then
           RCAUTR= min( max(L/TAU,0.), QCM(i,k)*idt )
           DrINIT= max(83.e-6, 12.6e-4/(0.5e6*SIGc-3.5))
           DrAUT = max(DrINIT, Dr)
           CCAUTR= RCAUTR*DE(i,k)/(cmr*DrAUT*DrAUT*DrAUT)
           if (dblMom_c) CCSCOC= min(KK2*NCM(i,k)*NCM(i,k)*GC3*iGC1*iLAMc6, NCM(i,k)* &
                                 idt)
        endif
        if (((QRM(i,k))>1.2*max(L,0.)*iDE(i,k).or.Dr>max(5.e-6,DrINIT)).and.rainAccr_ON &
             .and. rainPresent) then
           if (QCM(i,k)>epsQ.and.L>0.) then
              if (Dr.ge.100.e-6) then
                 CCACCR = KK1*(NCM(i,k)*NRM(i,k))*(GC2*iGC1*iLAMc3+GR34*iGR31*iLAMr3)
                 RCACCR = cmr*iDE(i,k)*KK1*(NCM(i,k)*NRM(i,k))*iLAMc3*(GC3*iGC1*iLAMc3+ &
                          GC2*iGC1*GR34*iGR31*iLAMr3)
              else
                 CCACCR = KK2*(NCM(i,k)*NRM(i,k))*(GC3*iGC1*iLAMc6+GR37*iGR31*iLAMr6)
                 tmp1 = cmr*iDE(i,k)
                 tmp2 = KK2*(NCM(i,k)*NRM(i,k))*iLAMc3
                 RCACCR = tmp1 * tmp2
                 tmp1 = GC4*iGR31
                 tmp1 = (tmp1)*iLAMc6
                 tmp2 = GC2*iGC1
                 tmp2 = tmp2*GR37*iGR31
                 tmp2 = (tmp2)*iLAMr6
                 RCACCR = RCACCR * (tmp1 + tmp2)
              endif
              CCACCR = min(CCACCR,(NC(i,k))*idt)
              RCACCR = min(RCACCR,(QC(i,k))*idt)
            endif
           if (dblMom_r) then
              tmp1= NRM(i,k)*NRM(i,k)
              if (Dr.ge.100.e-6) then
                 CRSCOR= KK1*tmp1*GR34*iGR31*iLAMr3
              else
                 CRSCOR= KK2*tmp1*GR37*iGR31*iLAMr6
              endif
              Ec= 1.
              if (Dr >= 600.e-6) Ec= exp(-2.5e3*(Dr-6.e-4))
              if (Dr >= 2000.e-6) Ec= 0.
              CRSCOR= min(Ec*CRSCOR,(0.5*NR(i,k))*idt)
           endif
        endif
        source= QC(i,k)
        sink = (RCAUTR+RCACCR)*dt
        if (sink>source) then
           ratio = source/sink
           RCAUTR= ratio*RCAUTR
           RCACCR= ratio*RCACCR
           CCACCR= ratio*CCACCR
        endif
        QC(i,k)= max(0., QC(i,k)+(-RCAUTR-RCACCR)*dt )
        QR(i,k)= max(0., QR(i,k)+( RCAUTR+RCACCR)*dt )
        if (dblMom_c) NC(i,k)= max(0., NC(i,k)+(-CCACCR-CCSCOC)*dt )
        if (dblMom_r) NR(i,k)= max(0., NR(i,k)+( CCAUTR-CRSCOR)*dt )
        if (dblMom_r) then
           if (QR(i,k)>epsQ .and. NR(i,k)>epsN) then
              Dr = Dm_x(DE(i,k),QR(i,k),1./NR(i,k),icmr,thrd)
              if (Dr>3.e-3) then
                 tmp1= (Dr-3.e-3); tmp2= tmp1*tmp1
                 tmp3= (Dr/DrMAX); tmp4= tmp3*tmp3*tmp3
                 NR(i,k)= NR(i,k)*(max((1.+2.e4*tmp2),tmp4))
              elseif (Dr<Dhh) then
                 QC(i,k)= QC(i,k) + QR(i,k)
                 NC(i,k)= NC(i,k) + NR(i,k)
                 QR(i,k)= 0.; NR(i,k)= 0.
              endif
           else
              QR(i,k)= 0.; NR(i,k)= 0.
           endif
        endif
     ENDDO
  ENDDO
  DO k=1,nk
     DO i=1,ni
        DEo = DE(i,nk)
        gam = sqrt(DEo*iDE(i,k))
        QSS(i,k)= sngl(FOQSA(T(i,k), PS(i)*S(i,k)))
        ssat = Q(i,k)/QSS(i,k)-1.
        Tc = T(i,k)-TRPL
        Cdiff = max(1.62e-5, (2.2157e-5 + 0.0155e-5*Tc)) *1.e5/(S(i,k)*PS(i))
        MUdyn = max(1.51e-5, (1.7153e-5 + 0.0050e-5*Tc))
        MUkin = MUdyn*iDE(i,k)
        iMUkin = 1./MUkin
        Ka = max(2.07e-2, (2.3971e-2 + 0.0078e-2*Tc))
        ScTHRD = (MUkin/Cdiff)**thrd
        X= Q(i,k)-QSS(i,k)
        if (dblMom_r) then
           rainPresent= (QR(i,k)>epsQ .and. NR(i,k)>epsN)
        else
           rainPresent= (QR(i,k)>epsQ)
        endif
        IF(X>0. .or. QC(i,k)>epsQ .or. rainPresent) THEN
           tmp1 = T(i,k)-35.86
           X = X/(1.+ck5*QSS(i,k)/(tmp1*tmp1))
           if (X<(-QC(i,k))) then
              D= 0.
              if(rainPresent) then
                 if(QM(i,k)<QSW(i,k)) then
                    MUkin = (1.715e-5+5.e-8*Tc)*iDE(i,k)
                    iMUkin= 1./MUkin
                    if (dblMom_r) then
                        Dr = Dm_x(DE(i,k),QR(i,k),1./NR(i,k),icmr,thrd)
                        iLAMr= iLAMDA_x(DE(i,k),QR(i,k),1./NR(i,k),icexr9,thrd)
                        LAMr = 1./iLAMr
                        No_r_DM= sngl(dble(NR(i,k))*dble(LAMr)**dble(1.+alpha_r))*iGR31
                        No_r = No_r_DM
                    else
                       iLAMr = sqrt(sqrt( (QR(i,k)*DE(i,k))/(GR34*cmr*No_r) ))
                    endif
                    VENTr= Avx*GR32*iLAMr**cexr5 + Bvx*ScTHRD*sqrt(gam*afr*iMUkin)*GR17* &
                           iLAMr**cexr6
                    ABw = CHLC*CHLC/(Ka*RGASV*T(i,k)*T(i,k))+1./(DE(i,k)*(QSS(i,k))* &
                           Cdiff)
                    QREVP= -dt*(PI2*ssat*No_r*VENTr/ABw)
                    if ((QR(i,k))>QREVP) then
                       DEL= -QREVP
                    else
                       DEL= -QR(i,k)
                    endif
                    D= max(X+QC(i,k), DEL)
                 endif
              endif
              X= D - QC(i,k)
              QR(i,k)= QR(i,k) + D
              if (QR(i,k)>0. .and. dblMom_r) &
                   NR(i,k)= max(0.,NR(i,k)+D*NR(i,k)/QR(i,k))
              QC(i,k)= 0.; NC(i,k)= 0.
              T(i,k) = T(i,k) + LCP*X
              Q(i,k) = Q(i,k) - X
           else
              if (ssat>0. .and. WZ(i,k)>0. .and. dblMom_c) &
                   NC(i,k)= max(NC(i,k),NccnFNC(WZ(i,k),TM(i,k),HPS(i)*S(i,k),CCNtype))
              T(i,k) = T(i,k) + LCP*X
              Q(i,k) = Q(i,k) - X
              QC(i,k) = QC(i,k) + X
              if (dblMom_c) then
                  if (X<0.) then
                     if (QC(i,k)>0.) then
                        NC(i,k)= max(0., NC(i,k) + X*NC(i,k)/QC(i,k) )
                     else
                        NC(i,k)= 0.
                     endif
                  endif
                  if (QC(i,k)>0..and.NC(i,k)==0.) NC(i,k)= 1.e7
              endif
           endif
        ENDIF
        if (dblMom_r) then
            if (QR(i,k)<epsQ.or.NR(i,k)<epsN) then
               Q(i,k) = Q(i,k) + QR(i,k)
               T(i,k) = T(i,k) - QR(i,k)*LCP
               QR(i,k)= 0.; NR(i,k)= 0.
            endif
        else
            if (QR(i,k)<epsQ) then
               Q(i,k) = Q(i,k) + QR(i,k)
               T(i,k) = T(i,k) - QR(i,k)*LCP
               QR(i,k)= 0.
            endif
        endif
     ENDDO
  ENDDO
 ENDIF
 IF (sedi_ON) THEN
   fluxM_r= 0.; fluxM_i= 0.; fluxM_s= 0.; fluxM_g= 0.; fluxM_h= 0.
   RT_rn1 = 0.; RT_rn2 = 0.; RT_fr1 = 0.; RT_fr2 = 0.; RT_sn1 = 0.
   RT_sn2 = 0.; RT_sn3 = 0.; RT_pe1 = 0.; RT_pe2 = 0.; RT_peL = 0.
   if (DblMom_r) then
     call SEDI_main_2(QR,NR,1,Q,T,DE,iDE,gamfact_r,epsQr_sedi,epsN,afr,bfr,cmr,dmr, &
                      ckQr1,ckQr2,icexr9,LCP,ni,nk,VrMax,DrMax,dt,DZ,fluxM_r,ktop_sedi, &
                      GRAV,massFlux3D=massFlux3D_r)
   else
      call SEDI_main_1b(QR,1,T,DE,iDE,gamfact_r,epsQr_sedi,afr,bfr,icmr,dmr,ckQr1, &
                        icexr9,ni,nk,VrMax,DrMax,dt,DZ,fluxM_r,No_r_SM,ktop_sedi,GRAV, &
                        massFlux3D=massFlux3D_r)
   endif
   if (DblMom_i) then
     call SEDI_main_2(QI,NY,2,Q,T,DE,iDE,gamfact,epsQi_sedi,epsN,afi,bfi,cmi,dmi,ckQi1, &
                      ckQi2,ckQi4,LSP,ni,nk,ViMax,DiMax,dt,DZ,fluxM_i,ktop_sedi,GRAV)
   else
     call SEDI_main_1b(QI,2,T,DE,iDE,gamfact,epsQi_sedi,afi,bfi,icmi,dmi,ckQi1,ckQi4, &
                     ni,nk,ViMax,DiMax,dt,DZ,fluxM_i,-99.,ktop_sedi,GRAV)
   endif
   if (DblMom_s) then
     call SEDI_main_2(QN,NN,3,Q,T,DE,iDE,gamfact,epsQs_sedi,epsN,afs,bfs,cms,dms,ckQs1, &
                      ckQs2,iGS20,LSP,ni,nk,VsMax,DsMax,dt,DZ,fluxM_s,ktop_sedi,GRAV, &
                      massFlux3D=massFlux3D_s)
   else
     call SEDI_main_1b(QN,3,T,DE,iDE,gamfact,epsQs_sedi,afs,bfs,icms,dms,ckQs1,iGS20, &
                      ni,nk,VsMax,DsMax,dt,DZ,fluxM_s,-99.,ktop_sedi,GRAV,massFlux3D= &
                      massFlux3D_s)
   endif
   if (DblMom_g) then
     call SEDI_main_2(QG,NG,4,Q,T,DE,iDE,gamfact,epsQg_sedi,epsN,afg,bfg,cmg,dmg,ckQg1, &
                      ckQg2,ckQg4,LSP,ni,nk,VgMax,DgMax,dt,DZ,fluxM_g,ktop_sedi,GRAV)
   else
     call SEDI_main_1b(QG,4,T,DE,iDE,gamfact,epsQg_sedi,afg,bfg,icmg,dmg,ckQg1,ckQg4, &
                      ni,nk,VgMax,DgMax,dt,DZ,fluxM_g,No_g_SM,ktop_sedi,GRAV)
   endif
   if (DblMom_h) then
     call SEDI_main_2(QH,NH,5,Q,T,DE,iDE,gamfact,epsQh_sedi,epsN,afh,bfh,cmh,dmh,ckQh1, &
                      ckQh2,ckQh4,LSP,ni,nk,VhMax,DhMax,dt,DZ,fluxM_h,ktop_sedi,GRAV)
   else
     call SEDI_main_1b(QH,5,T,DE,iDE,gamfact,epsQh_sedi,afh,bfh,icmh,dmh,ckQh1,ckQh4, &
                      ni,nk,VhMax,DhMax,dt,DZ,fluxM_h,No_h_SM,ktop_sedi,GRAV)
   endif
   do k= 1,nk
      do i= 1,ni
         if (QN(i,k)>epsQ .and. NN(i,k)>epsN) then
            iLAMs = max( iLAMmin2, iLAMDA_x(DE(i,k),QN(i,k), 1./NN(i,k),iGS20,idms) )
            tmp1 = min(NN(i,k)/iLAMs,No_s_max)
            NN(i,k)= tmp1**(dms/(1.+dms))*(iGS20*DE(i,k)*QN(i,k))**(1./(1.+dms))
            iLAMs = max( iLAMmin2, iLAMDA_x(DE(i,k),QN(i,k),1./NN(i,k),iGS20,idms) )
            tmp2 = 1./iLAMs
            tmp4 = 0.6*lamdas_min
            tmp5 = 2.*tmp4
            tmp3 = tmp2 + tmp4*(max(0.,tmp5-tmp2)/tmp5)**2.
            tmp3 = max(tmp3, lamdas_min)
            NN(i,k)= NN(i,k)*(tmp3*iLAMs)**dms
         endif
      enddo
   enddo
   RT_rn1 = fluxM_r *idew
   RT_sn1 = fluxM_i *idew
   RT_sn2 = fluxM_s *idew
   RT_sn3 = fluxM_g *idew
   RT_pe1 = fluxM_h *idew
   do i= 1,ni
      fluxV_i= fluxM_i(i)*idei
      fluxV_g= fluxM_g(i)*ideg
      if (QN(i,nk)>epsQ .and. NN(i,nk)>epsN .and. fluxM_s(i)>0.) then
         tmp1= 1./iLAMDA_x(DE(i,nk),QN(i,nk),1./NN(i,nk),iGS20,idms)
         fluxV_s= fluxM_s(i)*rfact_FvFm*tmp1**(dms-3.)
      else
         fluxV_s=0.
      endif
      tmp1= fluxV_i + fluxV_g + fluxV_s
      tmp2= QR(i,nk) + QI(i,nk) + QN(i,nk) + QG(i,nk)
      if (T(i,nk)>TRPL .and. tmp2>epsQ) then
         fracLiq= QR(i,nk)/tmp2
      else
         fracLiq= 0.
      endif
      tmp3= RT_sn1(i) + RT_sn2(i) + RT_sn3(i)
      RT_snd(i)= (1.-fracLiq)*tmp1 + fracLiq*tmp3
   enddo
   IF (precipDiag_ON) THEN
      DO i= 1,ni
         DE(i,nk)= S(i,nk)*PS(i)/(RGASD*T(i,nk))
         if (DblMom_r) then
            N_r= NR(i,nk)
         else
            N_r= (No_r*GR31)**(3./(4.+alpha_r))*(GR31*iGR34*DE(i,nk)*QR(i,nk)*icmr)** &
                  ((1.+alpha_r)/(4.+alpha_r))
         endif
         if (QR(i,nk)>epsQ .and. N_r>epsN) then
            Dm_r(i,nk)= (DE(i,nk)*icmr*QR(i,nk)/N_r)**thrd
            if (Dm_r(i,nk)>Dr_large) then
               RT_rn2(i)= RT_rn1(i); RT_rn1(i)= 0.
            endif
         endif
         if (T(i,nk)<TRPL) then
            RT_fr1(i)= RT_rn1(i); RT_rn1(i)= 0.
            RT_fr2(i)= RT_rn2(i); RT_rn2(i)= 0.
         endif
         if (T(i,nk)>(TRPL+5.0)) then
            RT_pe2(i)= RT_pe1(i); RT_pe1(i)= 0.
         endif
         if (QH(i,nk)>epsQ) then
            if (DblMom_h) then
               N_h= NH(i,nk)
            else
               N_h= (No_h_SM*GH31)**(3./(4.+alpha_h))*(GH31*iGH34*DE(i,nk)*QH(i,nk)* &
                  icmh)**((1.+alpha_h)/(4.+alpha_h))
            endif
            Dm_h(i,nk)= Dm_x(DE(i,nk),QH(i,nk),1./N_h,icmh,thrd)
            if (DM_h(i,nk)>Dh_large) RT_peL(i)= RT_pe2(i)
         endif
      ENDDO
   ENDIF
 ELSE
    massFlux3D_r= 0.
    massFlux3D_s= 0.
 ENDIF
 where (Q<0.) Q= 0.
  IF (calcDiag) THEN
     ZEC= minZET
     cxr= icmr*icmr
     cxi= 1./fdielec*icmr*icmr
     Gzr= (6.+alpha_r)*(5.+alpha_r)*(4.+alpha_r)/((3.+alpha_r)*(2.+alpha_r)*(1.+alpha_r))
     Gzi= (6.+alpha_i)*(5.+alpha_i)*(4.+alpha_i)/((3.+alpha_i)*(2.+alpha_i)*(1.+alpha_i))
     if (snowSpherical) then
        Gzs= (6.+alpha_s)*(5.+alpha_s)*(4.+alpha_s)/((3.+alpha_s)*(2.+alpha_s)* &
             (1.+alpha_s))
     else
        Gzs= (4.+alpha_s)*(3.+alpha_s)/((2.+alpha_s)*(1.+alpha_s))
     endif
     Gzg= (6.+alpha_g)*(5.+alpha_g)*(4.+alpha_g)/((3.+alpha_g)*(2.+alpha_g)*(1.+alpha_g))
     Gzh= (6.+alpha_h)*(5.+alpha_h)*(4.+alpha_h)/((3.+alpha_h)*(2.+alpha_h)*(1.+alpha_h))
     do k= 1,nk
       do i= 1,ni
           DE(i,k)= S(i,k)*PS(i)/(RGASD*T(i,k))
           tmp9= DE(i,k)*DE(i,k)
           if (DblMom_c) then
              N_c= NC(i,k)
           else
              N_c= N_c_SM
           endif
           if (DblMom_r) then
              N_r= NR(i,k)
           else
              N_r= (No_r_SM*GR31)**(3./(4.+alpha_r))*(GR31*iGR34*DE(i,k)*QR(i,k)*icmr)** &
                   ((1.+alpha_r)/(4.+alpha_r))
           endif
           if (DblMom_i) then
              N_i= NY(i,k)
           else
              N_i= N_Cooper(TRPL,T(i,k))
           endif
           if (DblMom_s) then
              N_s= NN(i,k)
           else
              No_s= Nos_Thompson(TRPL,T(i,k))
              N_s = (No_s*GS31)**(dms/(1.+dms+alpha_s))*(GS31*iGS34*DE(i,k)*QN(i,k)* &
                    icms)**((1.+alpha_s)/(1.+dms+alpha_s))
           endif
           if (DblMom_g) then
              N_g= NG(i,k)
           else
              N_g= (No_g_SM*GG31)**(3./(4.+alpha_g))*(GG31*GG34*DE(i,k)*QG(i,k)*icmg)** &
                   ((1.+alpha_g)/(4.+alpha_g))
           endif
           if (DblMom_h) then
              N_h= NH(i,k)
           else
              N_h= (No_h_SM*GH31)**(3./(4.+alpha_h))*(GH31*iGH34*DE(i,k)*QH(i,k)*icmh)** &
                   ((1.+alpha_h)/(4.+alpha_h))
           endif
           tmp1= 0.; tmp2= 0.; tmp3= 0.; tmp4= 0.; tmp5= 0.
           if (QR(i,k)>epsQ .and. N_r>epsN) tmp1 = cxr*Gzr*tmp9*QR(i,k)*QR(i,k)/N_r
           if (QI(i,k)>epsQ .and. N_i>epsN) tmp2 = cxi*Gzi*tmp9*QI(i,k)*QI(i,k)/N_i
           if (QN(i,k)>epsQ .and. N_s>epsN) tmp3 = cxi*Gzs*tmp9*QN(i,k)*QN(i,k)/N_s
           if (QG(i,k)>epsQ .and. N_g>epsN) tmp4 = cxi*Gzg*tmp9*QG(i,k)*QG(i,k)/N_g
           if (QH(i,k)>epsQ .and. N_h>epsN) tmp5 = cxi*Gzh*tmp9*QH(i,k)*QH(i,k)/N_h
           if ( T(i,k)>TRPL) then
             tmp2= tmp2*fdielec
             tmp3= tmp3*fdielec
             tmp4= tmp4*fdielec
             tmp5= tmp5*fdielec
           endif
           ZET(i,k) = tmp1 + tmp2 + tmp3 + tmp4 + tmp5
           if (ZET(i,k)>0.) then
              ZET(i,k)= 10.*log10((ZET(i,k)*Zfact))
           else
              ZET(i,k)= minZET
           endif
           ZET(i,k)= max(ZET(i,k),minZET)
           ZEC(i)= max(ZEC(i),ZET(i,k))
           Dm_c(i,k)= 0.; Dm_r(i,k)= 0.; Dm_i(i,k)= 0.
           Dm_s(i,k)= 0.; Dm_g(i,k)= 0.; Dm_h(i,k)= 0.
           if(QC(i,k)>epsQ.and.N_c>epsN) Dm_c(i,k)=Dm_x(DE(i,k),QC(i,k),1./N_c,icmr,thrd)
           if(QR(i,k)>epsQ.and.N_r>epsN) Dm_r(i,k)=Dm_x(DE(i,k),QR(i,k),1./N_r,icmr,thrd)
           if(QI(i,k)>epsQ.and.N_i>epsN) Dm_i(i,k)=Dm_x(DE(i,k),QI(i,k),1./N_i,icmi,thrd)
           if(QN(i,k)>epsQ.and.N_s>epsN) Dm_s(i,k)=Dm_x(DE(i,k),QN(i,k),1./N_s,icms,idms)
           if(QG(i,k)>epsQ.and.N_g>epsN) Dm_g(i,k)=Dm_x(DE(i,k),QG(i,k),1./N_g,icmg,thrd)
           if(QH(i,k)>epsQ.and.N_h>epsN) Dm_h(i,k)=Dm_x(DE(i,k),QH(i,k),1./N_h,icmh,thrd)
           SLW(i,k)= 0.
           if (T(i,k)<TRPL) SLW(i,k)= DE(i,k)*(QC(i,k)+QR(i,k))
           tmp1= QC(i,k)*DE(i,k)*1.e+3
           tmp2= N_c*1.e-6
           if (tmp1>0.005 .and. tmp2>1.) then
              VIS1(i,k)= max(epsVIS,1000.*(1.13*(tmp1*tmp2)**(-0.51)))
           else
              VIS1(i,k)= 3.*maxVIS
           endif
           tmp1= massFlux3D_r(i,k)*idew*3.6e+6
           if (tmp1>0.01) then
              VIS2(i,k)= max(epsVIS,1000.*(-4.12*tmp1**0.176+9.01))
           else
              VIS2(i,k)= 3.*maxVIS
           endif
           tmp1= massFlux3D_s(i,k)*idew*3.6e+6
           if (tmp1>0.01) then
              VIS3(i,k)= max(epsVIS,1000.*(1.10*tmp1**(-0.701)))
           else
              VIS3(i,k)= 3.*maxVIS
           endif
           VIS(i,k) = min(maxVIS, 1./(1./VIS1(i,k) + 1./VIS2(i,k) + 1./VIS3(i,k)))
           VIS1(i,k)= min(maxVIS, VIS1(i,k))
           VIS2(i,k)= min(maxVIS, VIS2(i,k))
           VIS3(i,k)= min(maxVIS, VIS3(i,k))
        enddo
     enddo
     h_CB = noVal_h_XX
     h_SN = noVal_h_XX
     h_ML1= noVal_h_XX
     h_ML2= noVal_h_XX
     tmp1= 1./GRAV
     do i= 1,ni
        CB_found= .false.; SN_found= .false.; ML_found= .false.
        do k= nk,2,-1
           if ((QC(i,k)>epsQ2.or.QI(i,k)>epsQ2) .and. .not.CB_found) then
              h_CB(i) = GZ(i,k)*tmp1
              CB_found= .true.
           endif
           if ( ((QN(i,k)>epsQ2 .and. Dm_s(i,k)>minSnowSize) .or. &
                 (QG(i,k)>epsQ2 .and. Dm_g(i,k)>minSnowSize)) .and. .not.SN_found) then
              h_SN(i) = GZ(i,k)*tmp1
              SN_found= .true.
           endif
           if (T(i,k)>TRPL .and. T(i,k-1)<TRPL .and. .not.ML_found) then
              h_ML1(i) = GZ(i,k)*tmp1
              ML_found= .true.
           endif
        enddo
     enddo
     do i= 1,ni
        ML_found= .false.
        do k= 2,nk
           if (T(i,k)>TRPL .and. T(i,k-1)<TRPL .and. .not.ML_found) then
              h_ML2(i) = GZ(i,k)*tmp1
              ML_found= .true.
           endif
        enddo
     enddo
  ENDIF
  do k= 1,nk
     DE(:,k) = S(:,k)*PS(:)/(RGASD*T(:,k))
     iDE(:,k)= 1./DE(:,k)
  enddo
  NC= NC*iDE; NCTEND= NCTEND*iDE
  NR= NR*iDE; NRTEND= NRTEND*iDE
  NY= NY*iDE; NYTEND= NYTEND*iDE
  NN= NN*iDE; NNTEND= NNTEND*iDE
  NG= NG*iDE; NGTEND= NGTEND*iDE
  NH= NH*iDE; NHTEND= NHTEND*iDE
      do k= 1,nk
         do i= 1,ni
            tmp1=T_TEND(i,k); T_TEND(i,k)=(T(i,k) -T_TEND(i,k))*iDT; T(i,k) = tmp1
            tmp1=Q_TEND(i,k); Q_TEND(i,k)=(Q(i,k) -Q_TEND(i,k))*iDT; Q(i,k) = tmp1
            tmp1=QCTEND(i,k); QCTEND(i,k)=(QC(i,k)-QCTEND(i,k))*iDT; QC(i,k)= tmp1
            tmp1=QRTEND(i,k); QRTEND(i,k)=(QR(i,k)-QRTEND(i,k))*iDT; QR(i,k)= tmp1
            tmp1=QITEND(i,k); QITEND(i,k)=(QI(i,k)-QITEND(i,k))*iDT; QI(i,k)= tmp1
            tmp1=QNTEND(i,k); QNTEND(i,k)=(QN(i,k)-QNTEND(i,k))*iDT; QN(i,k)= tmp1
            tmp1=QGTEND(i,k); QGTEND(i,k)=(QG(i,k)-QGTEND(i,k))*iDT; QG(i,k)= tmp1
            tmp1=QHTEND(i,k); QHTEND(i,k)=(QH(i,k)-QHTEND(i,k))*iDT; QH(i,k)= tmp1
            if (DblMom_c) then
             tmp1=NCTEND(i,k); NCTEND(i,k)=(NC(i,k)-NCTEND(i,k))*iDT; NC(i,k)= tmp1
            endif
            if (DblMom_r) then
             tmp1=NRTEND(i,k); NRTEND(i,k)=(NR(i,k)-NRTEND(i,k))*iDT; NR(i,k)= tmp1
            endif
            if (DblMom_i) then
             tmp1=NYTEND(i,k); NYTEND(i,k)=(NY(i,k)-NYTEND(i,k))*iDT; NY(i,k)= tmp1
            endif
            if (DblMom_s) then
             tmp1=NNTEND(i,k); NNTEND(i,k)=(NN(i,k)-NNTEND(i,k))*iDT; NN(i,k)= tmp1
            endif
            if (DblMom_g) then
             tmp1=NGTEND(i,k); NGTEND(i,k)=(NG(i,k)-NGTEND(i,k))*iDT; NG(i,k)= tmp1
            endif
            if (DblMom_h) then
             tmp1=NHTEND(i,k); NHTEND(i,k)=(NH(i,k)-NHTEND(i,k))*iDT; NH(i,k)= tmp1
            endif
         enddo
      enddo
END SUBROUTINE mp_milbrandt2mom_main
   real function des_OF_Ds(Ds_local,desMax_local,eds_local,fds_local)
      real :: Ds_local,desMax_local,eds_local,fds_local
      des_OF_Ds= min(desMax_local, eds_local*exp(fds_local*log(Ds_local)))
   end function des_OF_Ds
   real function Dm_x(DE_local,QX_local,iNX_local,icmx_local,idmx_local)
      real :: DE_local,QX_local,iNX_local,icmx_local,idmx_local
      Dm_x = exp(idmx_local*log(DE_local*QX_local*iNX_local*icmx_local))
   end function Dm_x
   real function iLAMDA_x(DE_local,QX_local,iNX_local,icex_local,idmx_local)
      real :: DE_local,QX_local,iNX_local,icex_local,idmx_local
      iLAMDA_x = exp(idmx_local*log(DE_local*QX_local*iNX_local*icex_local))
   end function
   real function N_Cooper(TRPL_local,T_local)
      real :: TRPL_local,T_local
      N_Cooper= 5.*exp(0.304*(TRPL_local-max(233.,T_local)))
   end function N_Cooper
   real function Nos_Thompson(TRPL_local,T_local)
      real :: TRPL_local,T_local
      Nos_Thompson= min(2.e+8, 2.e+6*exp(-0.12*min(-0.001,T_local-TRPL_local)))
   end function Nos_Thompson
END MODULE my_dmom_mod
 MODULE module_mp_milbrandt2mom
      use module_wrf_error
      use my_dmom_mod
      implicit none
      CONTAINS
      SUBROUTINE milbrandt2mom_init
      END SUBROUTINE milbrandt2mom_init
      SUBROUTINE mp_milbrandt2mom_driver(qv, qc, qr, qi, qs, qg, qh, nc, nr, ni, ns, ng, &
                              nh, th, pii, p, w, dz, dt_in, itimestep, &
                              RAINNC, RAINNCV, SNOWNC, SNOWNCV, GRPLNC, GRPLNCV, &
                              HAILNC, HAILNCV, SR, Zet, &
                              ids,ide, jds,jde, kds,kde, &
                              ims,ime, jms,jme, kms,kme, &
                              its,ite, jts,jte, kts,kte)
      implicit none
      integer, intent(in):: ids,ide, jds,jde, kds,kde, &
                            ims,ime, jms,jme, kms,kme, &
                            its,ite, jts,jte, kts,kte
      real, dimension(ims:ime, kms:kme, jms:jme), intent(inout):: &
                            qv,qc,qr,qi,qs,qg,qh,nc,nr,ni,ns,ng,nh,th,Zet
      real, dimension(ims:ime, kms:kme, jms:jme), intent(in):: &
                            pii,p,w,dz
      real, dimension(ims:ime, jms:jme), intent(inout):: &
                            RAINNC,RAINNCV,SNOWNC,SNOWNCV,GRPLNC,GRPLNCV,HAILNC,HAILNCV, &
                            SR
      real, intent(in):: dt_in
      integer, intent(in):: itimestep
      real, dimension(1:ite-its+1,1:kte-kts+1) :: t2d,qv2d,qc2d,qr2d,qi2d,qs2d,qg2d,qh2d,&
            nc2d,nr2d,ni2d,ns2d,ng2d,nh2d,p2d,dz2d,rho,irho,omega2d,t2d_m,qv2d_m,qc2d_m, &
            qr2d_m,qi2d_m,qs2d_m,qg2d_m,qh2d_m,nc2d_m,nr2d_m,ni2d_m,ns2d_m,ng2d_m,nh2d_m,&
            sigma2d,tmp01,tmp02,tmp03,tmp04,tmp05,tmp06,tmp07,tmp08,tmp09,tmp10,tmp11, &
            tmp12,tmp13,tmp14,tmp15,tmp16,tmp17,tmp18,gz2d,zet2d
      real, dimension(1:ite-its+1,1:kte-kts+1) :: Dm_c,Dm_r,Dm_i,Dm_s,Dm_g,Dm_h, &
            SLW,VIS,VIS1,VIS2,VIS3,SS01,SS02,SS03,SS04,SS05,SS06,SS07,SS08,SS09,SS10, &
            SS11,SS12,SS13,SS14,SS15,SS16,SS17,SS18,SS19,SS20,T_tend,Q_tend,QCtend, &
            QRtend,QItend,QStend,QGtend,QHtend,NCtend,NRtend,NItend,NStend,NGtend,NHtend
      real, dimension(1:ite-its+1) :: rt_rn1,rt_rn2,rt_fr1,rt_fr2,rt_sn1,rt_sn2,rt_sn3, &
            rt_pe1,rt_pe2,rt_peL,rt_snd,ZEC,h_CB,h_ML1,h_ML2,h_SN,p_src
      real :: dt,ms2mmstp
      real :: qc_max,qr_max,qs_max,qi_max,qg_max,qh_max,nc_max,nr_max,ns_max,ni_max, &
                 ng_max,nh_max
      integer :: i,j,k,i2d,j2d,k2d,i2d_max,k2d_max
      integer :: imax_qc, imax_qr, imax_qi, imax_qs, imax_qg, imax_qh
      integer :: imax_nc, imax_nr, imax_ni, imax_ns, imax_ng, imax_nh
      integer :: jmax_qc, jmax_qr, jmax_qi, jmax_qs, jmax_qg, jmax_qh
      integer :: jmax_nc, jmax_nr, jmax_ni, jmax_ns, jmax_ng, jmax_nh
      integer :: kmax_qc, kmax_qr, kmax_qi, kmax_qs, kmax_qg, kmax_qh
      integer :: kmax_nc, kmax_nr, kmax_ni, kmax_ns, kmax_ng, kmax_nh
      integer :: i_start, j_start, i_end, j_end, CCNtype
      logical :: precipDiag_ON,sedi_ON,warmphase_ON,autoconv_ON,icephase_ON,snow_ON, &
                 initN,dblMom_c,dblMom_r,dblMom_i,dblMom_s,dblMom_g,dblMom_h
      real, parameter :: ms2mmh = 3.6e+6
      real, parameter :: R_d = 287.04
      character*512 :: mp_debug
      i2d_max = ite-its+1
      k2d_max = kte-kts+1
      dt = dt_in
      ms2mmstp = 1.e+3*dt
      CCNtype = 2.
      precipDiag_ON = .true.; dblMom_c = .true.
      sedi_ON = .true.; dblMom_r = .true.
      warmphase_ON = .true.; dblMom_i = .true.
      autoconv_ON = .true.; dblMom_s = .true.
      icephase_ON = .true.; dblMom_g = .true.
      snow_ON = .true.; dblMom_h = .true.
      initN = .true.
      qc_max = 0.; nc_max = 0.
      qr_max = 0.; nr_max = 0.
      qi_max = 0.; ni_max = 0.
      qs_max = 0.; ns_max = 0.
      qg_max = 0.; ng_max = 0.
      qh_max = 0.; nh_max = 0.
      imax_qc = 0; imax_nc = 0; jmax_qc = 0; jmax_nc = 0; kmax_qc = 0; kmax_nc = 0
      imax_qr = 0; imax_nr = 0; jmax_qr = 0; jmax_nr = 0; kmax_qr = 0; kmax_nr = 0
      imax_qi = 0; imax_ni = 0; jmax_qi = 0; jmax_ni = 0; kmax_qi = 0; kmax_ni = 0
      imax_qs = 0; imax_ns = 0; jmax_qs = 0; jmax_ns = 0; kmax_qs = 0; kmax_ns = 0
      imax_qg = 0; imax_ng = 0; jmax_qg = 0; jmax_ng = 0; kmax_qg = 0; kmax_ng = 0
      imax_qh = 0; imax_nh = 0; jmax_qh = 0; jmax_nh = 0; kmax_qh = 0; kmax_nh = 0
      RAINNCV(its:ite,jts:jte) = 0.
      SNOWNCV(its:ite,jts:jte) = 0.
      GRPLNCV(its:ite,jts:jte) = 0.
      HAILNCV(its:ite,jts:jte) = 0.
      SR(its:ite,jts:jte) = 0.
      do i = 1, 512
         mp_debug(i:i) = char(0)
      enddo
      j_loop1: do j = jts, jte
         j2d = j-jts+1
       i_loop1: do i = its, ite
         i2d = i-its+1
         gz2d(i2d,kts)= 0.
         do k = kts+1, kte
             gz2d(i2d,k)= gz2d(i2d,k-1) + dz(i,k,j)*9.81
         enddo
         k_loop1: do k = kts, kte
            k2d = k-kts+1
            t2d(i2d,k2d) = th(i,k,j)*pii(i,k,j)
            p2d(i2d,k2d) = p(i,k,j)
            dz2d(i2d,k2d) = dz(i,k,j)
            qv2d(i2d,k2d) = qv(i,k,j)
            rho(i2d,k2d) = p2d(i2d,k)/(R_d*t2d(i2d,k))
            omega2d(i2d,k2d)= -w(i,k,j)*rho(i2d,k2d)*9.81
            qc2d(i2d,k2d) = qc(i,k,j); nc2d(i2d,k2d) = nc(i,k,j)
            qi2d(i2d,k2d) = qi(i,k,j); ni2d(i2d,k2d) = ni(i,k,j)
            qr2d(i2d,k2d) = qr(i,k,j); nr2d(i2d,k2d) = nr(i,k,j)
            qs2d(i2d,k2d) = qs(i,k,j); ns2d(i2d,k2d) = ns(i,k,j)
            qg2d(i2d,k2d) = qg(i,k,j); ng2d(i2d,k2d) = ng(i,k,j)
            qh2d(i2d,k2d) = qh(i,k,j); nh2d(i2d,k2d) = nh(i,k,j)
         enddo k_loop1
           K_loop9: do k= kts, kte
            k2d = k-kts+1
            sigma2d(i2d,k2d)= p2d(i2d,k2d)/p2d(i2d,kte-kts+1)
           enddo K_loop9
       enddo i_loop1
       p_src(:)= p2d(:,k2d_max)
       tmp01= omega2d; tmp02= t2d; tmp03= qv2d; tmp04= qc2d; tmp05=qr2d; tmp06=qi2d
       tmp07= qs2d; tmp08= qg2d; tmp09= qh2d; tmp10= nc2d; tmp11=nr2d; tmp12=ni2d
       tmp13= ns2d; tmp14= ng2d; tmp15= nh2d; tmp16= sigma2d; tmp17=dz2d; tmp18=gz2d
       do k = kts-1,kte-1
          k2d = k-kts+1
          omega2d(:,k2d+1)= tmp01(:,k2d_max-k2d)
          t2d(:,k2d+1) = tmp02(:,k2d_max-k2d)
          qv2d(:,k2d+1) = tmp03(:,k2d_max-k2d)
          qc2d(:,k2d+1) = tmp04(:,k2d_max-k2d)
          qr2d(:,k2d+1) = tmp05(:,k2d_max-k2d)
          qi2d(:,k2d+1) = tmp06(:,k2d_max-k2d)
          qs2d(:,k2d+1) = tmp07(:,k2d_max-k2d)
          qg2d(:,k2d+1) = tmp08(:,k2d_max-k2d)
          qh2d(:,k2d+1) = tmp09(:,k2d_max-k2d)
          nc2d(:,k2d+1) = tmp10(:,k2d_max-k2d)
          nr2d(:,k2d+1) = tmp11(:,k2d_max-k2d)
          ni2d(:,k2d+1) = tmp12(:,k2d_max-k2d)
          ns2d(:,k2d+1) = tmp13(:,k2d_max-k2d)
          ng2d(:,k2d+1) = tmp14(:,k2d_max-k2d)
          nh2d(:,k2d+1) = tmp15(:,k2d_max-k2d)
          sigma2d(:,k2d+1)= tmp16(:,k2d_max-k2d)
          dz2d(:,k2d+1) = tmp17(:,k2d_max-k2d)
          gz2d(:,k2d+1) = tmp18(:,k2d_max-k2d)
       enddo
       t2d_m = t2d; qv2d_m = qv2d
       qc2d_m = qc2d; nc2d_m = nc2d
       qr2d_m = qr2d; nr2d_m = nr2d
       qi2d_m = qi2d; ni2d_m = ni2d
       qs2d_m = qs2d; ns2d_m = ns2d
       qg2d_m = qg2d; ng2d_m = ng2d
       qh2d_m = qh2d; nh2d_m = nh2d
       call mp_milbrandt2mom_main(omega2d,t2d,qv2d,qc2d,qr2d,qi2d,qs2d,qg2d,qh2d,nc2d, &
            nr2d,ni2d,ns2d,ng2d,nh2d,p_src,t2d_m,qv2d_m,qc2d_m,qr2d_m,qi2d_m,qs2d_m, &
            qg2d_m,qh2d_m,nc2d_m,nr2d_m,ni2d_m,ns2d_m,ng2d_m,nh2d_m,p_src,sigma2d, &
            rt_rn1,rt_rn2,rt_fr1,rt_fr2,rt_sn1,rt_sn2,rt_sn3,rt_pe1,rt_pe2,rt_peL,rt_snd,&
            gz2d,T_tend,Q_tend,QCtend,QRtend,QItend,QStend,QGtend,QHtend,NCtend,NRtend, &
            NItend,NStend,NGtend,NHtend,dt,i2d_max,1,k2d_max,j,itimestep,CCNtype,precipDiag_ON,&
            sedi_ON,warmphase_ON,autoconv_ON,icephase_ON,snow_ON,initN,dblMom_c,dblMom_r,&
            dblMom_i,dblMom_s,dblMom_g,dblMom_h,Dm_c,Dm_r,Dm_i,Dm_s,Dm_g,Dm_h,Zet2d,ZEC, &
            SLW,VIS,VIS1,VIS2,VIS3,h_CB,h_ML1,h_ML2,h_SN,SS01,SS02,SS03,SS04,SS05,SS06, &
            SS07,SS08,SS09,SS10,SS11,SS12,SS13,SS14,SS15,SS16,SS17,SS18,SS19,SS20)
       t2d(:,:) = t2d(:,:) + T_tend(:,:)*dt
       qv2d(:,:)= qv2d(:,:) + Q_tend(:,:)*dt
       qc2d(:,:)= qc2d(:,:) + QCtend(:,:)*dt; nc2d(:,:)= nc2d(:,:) + NCtend(:,:)*dt
       qr2d(:,:)= qr2d(:,:) + QRtend(:,:)*dt; nr2d(:,:)= nr2d(:,:) + NRtend(:,:)*dt
       qi2d(:,:)= qi2d(:,:) + QItend(:,:)*dt; ni2d(:,:)= ni2d(:,:) + NItend(:,:)*dt
       qs2d(:,:)= qs2d(:,:) + QStend(:,:)*dt; ns2d(:,:)= ns2d(:,:) + NStend(:,:)*dt
       qg2d(:,:)= qg2d(:,:) + QGtend(:,:)*dt; ng2d(:,:)= ng2d(:,:) + NGtend(:,:)*dt
       qh2d(:,:)= qh2d(:,:) + QHtend(:,:)*dt; nh2d(:,:)= nh2d(:,:) + NHtend(:,:)*dt
       tmp02= t2d; tmp03= qv2d; tmp04= qc2d; tmp05=qr2d; tmp06=qi2d
       tmp07= qs2d; tmp08= qg2d; tmp09= qh2d; tmp10= nc2d; tmp11=nr2d; tmp12=ni2d
       tmp13= ns2d; tmp14= ng2d; tmp15= nh2d; tmp16= Zet2d; tmp17=ss01; tmp18=ss02
       do k = kts-1,kte-1
          k2d = k-kts+1
          t2d(:,k2d+1) = tmp02(:,k2d_max-k2d)
          qv2d(:,k2d+1) = tmp03(:,k2d_max-k2d)
          qc2d(:,k2d+1) = tmp04(:,k2d_max-k2d)
          qr2d(:,k2d+1) = tmp05(:,k2d_max-k2d)
          qi2d(:,k2d+1) = tmp06(:,k2d_max-k2d)
          qs2d(:,k2d+1) = tmp07(:,k2d_max-k2d)
          qg2d(:,k2d+1) = tmp08(:,k2d_max-k2d)
          qh2d(:,k2d+1) = tmp09(:,k2d_max-k2d)
          nc2d(:,k2d+1) = tmp10(:,k2d_max-k2d)
          nr2d(:,k2d+1) = tmp11(:,k2d_max-k2d)
          ni2d(:,k2d+1) = tmp12(:,k2d_max-k2d)
          ns2d(:,k2d+1) = tmp13(:,k2d_max-k2d)
          ng2d(:,k2d+1) = tmp14(:,k2d_max-k2d)
          nh2d(:,k2d+1) = tmp15(:,k2d_max-k2d)
          Zet2d(:,k2d+1) = tmp16(:,k2d_max-k2d)
       enddo
       i_loop2: do i = its, ite
         i2d = i-its+1
         RAINNCV(i,j) = (rt_rn1(i2d)+rt_rn2(i2d)+rt_fr1(i2d)+rt_fr2(i2d)+rt_sn1(i2d)+ &
                         rt_sn2(i2d)+rt_sn3(i2d)+rt_pe1(i2d)+rt_pe2(i2d))*ms2mmstp
         SNOWNCV(i,j) = (rt_sn1(i2d) + rt_sn2(i2d))*ms2mmstp
         HAILNCV(i,j) = (rt_pe1(i2d) + rt_pe2(i2d))*ms2mmstp
         GRPLNCV(i,j) = rt_sn3(i2d) *ms2mmstp
         RAINNC(i,j) = RAINNC(i,j) + RAINNCV(i,j)
         SNOWNC(i,j) = SNOWNC(i,j) + SNOWNCV(i,j)
         HAILNC(i,j) = HAILNC(i,j) + HAILNCV(i,j)
         GRPLNC(i,j) = GRPLNC(i,j) + GRPLNCV(i,j)
         SR(i,j) = (SNOWNCV(i,j)+HAILNCV(i,j)+GRPLNCV(i,j))/(RAINNCV(i,j)+1.e-12)
         k_loop2: do k = kts, kte
            k2d = k-kts+1
            if(.not.(t2d(i2d,k2d)>=173.) .or. (t2d(i2d,k2d)>1000.)) then
               write(6,*)
               write(6,*) '*** Stopping in mp_milbrandt2mom_driver due to unrealistic temperature ***'
               write(6,*) ' step: ',itimestep
               write(6,'(a5,5i5,8e15.5)') 'i,k: ',i,j,k,i2d,k2d,t2d(i2d,k2d),qv2d(i2d,k2d),qc2d(i2d,k2d),qr2d(i2d,k2d), &
                                                     qi2d(i2d,k2d),qs2d(i2d,k2d),qg2d(i2d,k2d),qh2d(i2d,k2d)
               write(6,*)
               stop
            endif
            th(i,k,j) = t2d(i2d,k2d)/pii(i,k,j)
            qv(i,k,j) = qv2d(i2d,k2d)
            qc(i,k,j) = qc2d(i2d,k2d); nc(i,k,j) = nc2d(i2d,k2d)
            qi(i,k,j) = qi2d(i2d,k2d); ni(i,k,j) = ni2d(i2d,k2d)
            qr(i,k,j) = qr2d(i2d,k2d); nr(i,k,j) = nr2d(i2d,k2d)
            qs(i,k,j) = qs2d(i2d,k2d); ns(i,k,j) = ns2d(i2d,k2d)
            qg(i,k,j) = qg2d(i2d,k2d); ng(i,k,j) = ng2d(i2d,k2d)
            qh(i,k,j) = qh2d(i2d,k2d); nh(i,k,j) = nh2d(i2d,k2d)
            Zet(i,k,j)= Zet2d(i2d,k2d)
         enddo k_loop2
       enddo i_loop2
      enddo j_loop1
      do i = 1, 256
         mp_debug(i:i) = char(0)
      enddo
      END SUBROUTINE mp_milbrandt2mom_driver
END MODULE module_mp_milbrandt2mom
