      subroutine evpduct ( z0,t,q,tsea,slpr,ustar,tstar,qstar,
     1                     xland,evpht,dmdz,nn)    

#include <evpduct.prol>
c rcs keywords: evpduct.F,v
c               2.2 1999/03/01 21:36:04
c SCCS IDENTIFICATION:  %W% %G%
c***********************************************************************
c
      use domdec
      implicit none
c
c***********************************************************************
c
      integer idbms
      integer imaxev
      integer imaxm1
      parameter (imaxev=41 , imaxm1=imaxev-1)
c
      character*1  cfluid
      character*8  isub
      character*10 cdtg
      character*10 cdtgmi
      character*24 dsetnm
      character*24 lvltyp
      character*32 geomnm
      character*32 mdltyp
      character*32 units
      character*7  outtyp
      character*32 parmnm
      character*80 dsetux
c
      integer nn
      integer i
      integer im
      integer j
      integer jn
      integer kerr
c
      logical lmpi2
      logical lwritu
c
      real kappa
      real delz
      real eps
cth      real es
      real g
      real grd
      real obkh
      real pz
      real psi
      real qsst
      real qzp
      real rm1
      real sst
      real sstv
      real thsst
      real thzp
      real tvz
      real tzp
      real vk
      real pp
      real qq
      real qstarm
      real tstarm
      real tt
      real top
      real tvstar
      real refr
c
      real z0(iminf(nn):imaxf(nn),jminf(nn):jmaxf(nn))
      real z0tmp(iminf(nn):imaxf(nn),jminf(nn):jmaxf(nn))
      real t(iminf(nn):imaxf(nn),jminf(nn):jmaxf(nn))
      real q(iminf(nn):imaxf(nn),jminf(nn):jmaxf(nn))
      real tsea(iminf(nn):imaxf(nn),jminf(nn):jmaxf(nn))

      real es(iminf(nn):imaxf(nn),jminf(nn):jmaxf(nn))

      real slpr(iminf(nn):imaxf(nn),jminf(nn):jmaxf(nn))
      real ustar(iminf(nn):imaxf(nn),jminf(nn):jmaxf(nn))
      real ustartmp(iminf(nn):imaxf(nn),jminf(nn):jmaxf(nn))
      real qstar(iminf(nn):imaxf(nn),jminf(nn):jmaxf(nn))
      real qstartmp(iminf(nn):imaxf(nn),jminf(nn):jmaxf(nn))
      real tstar(iminf(nn):imaxf(nn),jminf(nn):jmaxf(nn))
      real tstartmp(iminf(nn):imaxf(nn),jminf(nn):jmaxf(nn))
      real xland(iminf(nn):imaxf(nn),jminf(nn):jmaxf(nn))
      real evpht(iminf(nn):imaxf(nn),jminf(nn):jmaxf(nn))
      real dmdz(iminf(nn):imaxf(nn),jminf(nn):jmaxf(nn))
      real rlev(2)
      real xm(imaxev)
      real tl1(imaxev)
      real zz(imaxev)
      real zzc(imaxev)
      real rad(imaxev)
c
c***********************************************************************
c          data statements
c***********************************************************************
c
      data eps    /.62197/
      data g      /9.80616/
      data kappa  /.286/
      data vk     /.4/
c
c***********************************************************************
c        index of refraction function
c***********************************************************************
c
      refr(pp,tt,qq) = (77.6*pp/tt)*
     1        (1.0+(4810.0*qq/((.62197+.37803*qq)*tt)))
c
      rm1 = 1.0/(1.14*vk)
      grd = g/287.1
c
c***********************************************************************
c        determine evap duct ht to 1 m accuracy
c        from surface to 40 m (imaxm1)
c***********************************************************************
c
      delz = 1.0
      zz(1) = 0.
      zzc(1) = 0.
      do i = 1,imaxm1
         zz(i+1) = float(i)
         zzc(i+1) = .157*zz(i+1)
      end do
c
c***********************************************************************
c        compute evap duct height for each grid point
c***********************************************************************
c
      call vesat (tsea,es,nn)

      do jn = jmini(nn),jmaxi(nn)
         do im = imini(nn),imaxi(nn)
c
            if (xland(im,jn).eq.0.) then
c
c***********************************************************************
c                 sea surface values
c***********************************************************************
c
               pz = slpr(im,jn)
               sst = tsea(im,jn)
cth               call vesat (sst,es,nn)
c
c***********************************************************************
c                 assume 35% salinity
c***********************************************************************
c
               es(im,jn) = .981*es(im,jn)
               qsst = eps*es(im,jn)/(pz-.37803*es(im,jn))
               thsst = sst*((1000.0/pz)**kappa)
               sstv = sst*(1.0+.609*qsst)
               tvz = sstv
c
c***********************************************************************
c                 compute m at sea surface
c***********************************************************************
c
               xm(1) = refr(pz,sst,qsst)
c
c***********************************************************************
c                 Sign of COAMPS fluxes is opposite
c                 the convention in the equations 
c***********************************************************************
c
               qstarm = -qstar(im,jn)
               tstarm = -tstar(im,jn)
c
c***********************************************************************
c                 Monin Obukhov length
c***********************************************************************
c
               tvstar = tstarm*(1.+.609*q(im,jn)) + 
     1                  .609*t(im,jn)*qstarm
               if (tvstar.eq.0.) tvstar = .00000001
               obkh = t(im,jn)*(1.+.609*q(im,jn))*ustar(im,jn)
     1                        *ustar(im,jn)/(g*vk*tvstar)
c
c***********************************************************************
c                 neutral case
c***********************************************************************
c
               do i = 2,imaxev
                  tl1(i) = alog(zz(i)/z0(im,jn))
               end do
c
c***********************************************************************
c                 stability adjustment
c***********************************************************************
c
               if (obkh.gt.0.0) then
c
c***********************************************************************
c                    stable case
c***********************************************************************
c
                   do i = 2,imaxev
                      tl1(i) = tl1(i) + 7.*zz(i)/obkh
                   end do
c
               else if (obkh.lt.0.0) then
c
c***********************************************************************
c                    unstable case
c***********************************************************************
c
                  do i = 2,imaxev
                     rad(i) = 1.0-16.*zz(i)/obkh
                  end do
                  do i = 2,imaxev
                     psi  = 2.0*alog(.5*(1.0+sqrt(rad(i))))
                     tl1(i)  = tl1(i)-psi
                  end do
c
               endif
c
               tstarm = rm1*tstarm
               qstarm = rm1*qstarm
c
               do i = 2,imaxev
c
c***********************************************************************
c                    compute t and q at z
c***********************************************************************
c
                  thzp = thsst+tl1(i)*tstarm
                  qzp  = qsst+tl1(i)*qstarm
c
c***********************************************************************
c                    estimate p at z assuming constant virtual
c                    temp from below
c***********************************************************************
c
                  pz = pz*exp(-grd*delz/tvz)
c
c***********************************************************************
c                    compute real temp at z
c***********************************************************************
c    
                  tzp = thzp/((1000.0/pz)**kappa)
c
c***********************************************************************
c                    compute new virtual temp
c***********************************************************************
c
                  tvz = tzp*(1.0+.609*qzp)
c
c***********************************************************************
c                    compute modified index of refraction at z
c***********************************************************************
c
                  xm(i) = refr(pz,tzp,qzp) + zzc(i)
c
               end do
c
c***********************************************************************
c                 find minimum in M
c                 convert dz to km for dM/dz
c***********************************************************************
c
               evpht(im,jn) = top
               do i = 1,imaxm1
                  if (xm(i+1).gt.xm(i)) then
                     evpht(im,jn) = float(i-1)*delz
                     dmdz(im,jn) = (xm(i)-xm(1))/
     *                             (evpht(im,jn)+.0000001)*1000.0
                     go to 100
                  endif
               end do
               dmdz(im,jn) = (xm(imaxev)-xm(1))/evpht(im,jn)*1000.0
 100           continue
c
            else
c
c***********************************************************************
c                 grid point over land (undefined)
c***********************************************************************
c
               evpht(im,jn) = 0.0
               dmdz(im,jn)  = 117.8
c
            endif
c
         end do
      end do
c
c***********************************************************************
c        Define boundaries (Surface fluxes are zero on boundaries)
c***********************************************************************
c

      if (nsouth_nest(nn) .eq. 1) then
        do im = imini(nn),imaxi(nn)
          evpht(im,jminp_nest(nn)) = evpht(im,jminp_nest(nn)+1)
          dmdz (im,jminp_nest(nn)) = dmdz (im,jminp_nest(nn)+1)
        enddo
      endif
      if (nnorth_nest(nn) .eq. 1) then
        do im = imini(nn),imaxi(nn)
          evpht(im,jmaxp_nest(nn)) = evpht(im,jmaxp_nest(nn)-1)
          dmdz (im,jmaxp_nest(nn)) = dmdz (im,jmaxp_nest(nn)-1)
        enddo
      endif

      if (nwest_nest(nn) .eq. 1) then
        do jn = jmini(nn),jmaxi(nn)
          evpht(iminp_nest(nn),jn) = evpht(iminp_nest(nn)+1,jn)
          dmdz (iminp_nest(nn),jn) = dmdz (iminp_nest(nn)+1,jn)
        enddo
      endif
      if (neast_nest(nn) .eq. 1) then
        do jn = jmini(nn),jmaxi(nn)
          evpht(imaxp_nest(nn),jn) = evpht(imaxp_nest(nn)-1,jn)
          dmdz (imaxp_nest(nn),jn) = dmdz (imaxp_nest(nn)-1,jn)
        enddo
      endif
c
c   sw corner
c
      if (nsouth_nest(nn) .eq. 1.and.nwest_nest(nn) .eq. 1)then

        evpht(iminp_nest(nn),jminp_nest(nn))=0.5*(
     1  evpht(iminp_nest(nn),jminp_nest(nn)+1)+
     2  evpht(iminp_nest(nn)+1,jminp_nest(nn)))

        dmdz (iminp_nest(nn),jminp_nest(nn))=0.5*(
     1  dmdz (iminp_nest(nn),jminp_nest(nn)+1)+
     2  dmdz (iminp_nest(nn)+1,jminp_nest(nn)))
      endif
c
c   se corner
c
      if (nsouth_nest(nn) .eq. 1.and.neast_nest(nn) .eq. 1)then

        evpht(imaxp_nest(nn),jminp_nest(nn))=0.5*(
     1  evpht(imaxp_nest(nn)-1,jminp_nest(nn))+
     2  evpht(imaxp_nest(nn),jminp_nest(nn)+1))

        dmdz (imaxp_nest(nn),jminp_nest(nn))=0.5*(
     1  dmdz (imaxp_nest(nn)-1,jminp_nest(nn))+
     2  dmdz (imaxp_nest(nn),jminp_nest(nn)+1))
      endif
c
c    nw corner
c
      if (nnorth_nest(nn) .eq. 1.and.nwest_nest(nn) .eq. 1)then

        evpht(iminp_nest(nn),jmaxp_nest(nn))=0.5*(
     1  evpht(iminp_nest(nn)+1,jmaxp_nest(nn))+
     2  evpht(iminp_nest(nn),jmaxp_nest(nn)-1))

        dmdz (iminp_nest(nn),jmaxp_nest(nn))=0.5*(
     1  dmdz (iminp_nest(nn)+1,jmaxp_nest(nn))+
     2  dmdz (iminp_nest(nn),jmaxp_nest(nn)-1))
      endif
c
c    ne corner
c
      if (nnorth_nest(nn) .eq. 1.and.neast_nest(nn) .eq. 1)then

        evpht(imaxp_nest(nn),jmaxp_nest(nn))=0.5*(
     1  evpht(imaxp_nest(nn)-1,jmaxp_nest(nn))+
     2  evpht(imaxp_nest(nn),jmaxp_nest(nn)-1))

        dmdz (imaxp_nest(nn),jmaxp_nest(nn))=0.5*(
     1  dmdz (imaxp_nest(nn)-1,jmaxp_nest(nn))+
     2  dmdz (imaxp_nest(nn),jmaxp_nest(nn)-1))
      endif
c
      return
c
c***********************************************************************
c        unable to read old z0, ustar, tstar and qstar fields
c***********************************************************************
c
 200  continue
      if (npr == npr0) then
        print*,'evpduct: bad read of ustar,qstar and tstar'
        print*,'evpduct: set evap ducts to zero'
      endif
      do jn = jlo1_nest(nn), jhi1_nest(nn)
         do im = ilo1_nest(nn), ihi1_nest(nn)
            evpht(im,jn) = 0.0
            dmdz(im,jn)  = 117.8
         end do
      end do
c
c***********************************************************************
c
      return
      end
