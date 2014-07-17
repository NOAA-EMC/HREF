craa********************************************************************
c Starting May 27, 2008: modified by RAA to impose horizontal physical
C     dissipation on top of numerical diffusion in all layers.
c Jan 4, 2007: modified by Rashid Akmaev based on DELDIFS_hyb to do
c horizontal viscosity, thermal conduction, and diffusion of major 
c species (O and O2) with global mean coefficients.
craa********************************************************************
      SUBROUTINE idea_deldifs_init(SL,
     X                   LS_NODE,hybrid,gen_coord_hybrid)
!
! Jan 10 2013   J. Wang, this file is separated from idea_deldif.f
!                        for initialization only
!
      use gfs_dyn_resol_def
      use gfs_dyn_layout1
      use gfs_dyn_coordinate_def				! hmhj
      use gfs_dyn_deldifs_def
      use gfs_dyn_physcons, rerth => con_rerth
     &                    ,  rd => con_rd, cp => con_cp
      
      IMPLICIT NONE
!
      logical,intent(in) :: hybrid, gen_coord_hybrid
!
      REAL(KIND=KIND_EVOD),intent(in) :: SL(LEVS)
!
      INTEGER,intent(in)  :: LS_NODE(LS_DIM,3)
!
!CMR  LS_NODE(1,1) ... LS_NODE(LS_MAX_NODE,1) : VALUES OF L
!CMR  LS_NODE(1,2) ... LS_NODE(LS_MAX_NODE,2) : VALUES OF JBASEV
!CMR  LS_NODE(1,3) ... LS_NODE(LS_MAX_NODE,3) : VALUES OF JBASOD
!
      INTEGER              I,IS,IT,JDEL,JDELH,K,KD,KU
craa********************************************************************
c idea change1
c IDEA-related changes
      INTEGER              L,LOCL,N,N0,ND,NP,NPD
      INTEGER              N00
!
      INTEGER              INDEV
      INTEGER              INDOD
      integer              indev1,indev2
      integer              indod1,indod2
      real(kind=kind_evod), parameter :: rkappa = cp / rd
      REAL(KIND=KIND_EVOD) DN1,REALVAL,RTNP,DF_DK,SLRD0,FTRD1,RTRD1,FSHK
c INPUT
c
craa********************************************************************
!
      REAL(KIND=KIND_EVOD), parameter :: CONS0=0.0, CONS1=1.0, CONS2=2.0
!
      INTEGER              INDLSEV,JBASEV
      INTEGER              INDLSOD,JBASOD
!
      INCLUDE 'function2'
!
!     print *,' enter idea_deldifs ' 				! hmhj
!......................................................................
!
c Begin initialization
craa***********************************LEVS*********************************
craa********************************************************************
!
       CALL COUNTPERF(0,15,0.)
!!
       allocate(RTRD(LEVS),RTHK(LEVS),sf(levs))
       ALLOCATE ( DNE(LEN_TRIE_LS) )
       ALLOCATE ( DNO(LEN_TRIO_LS) )
       ALLOCATE ( BKLY(levs) )        					! hmhj
       ALLOCATE ( CKLY(levs) )        					! hmhj
craa********************************************************************
c idea change2
c IDEA-related changes
       ALLOCATE (dneidea(LEN_TRIE_LS) )
       ALLOCATE (dnoidea(LEN_TRIO_LS) )
c idea change2 end
craa********************************************************************
       BKLY(:) = 1.0
       CKLY(:) = 0.0
       if (gen_coord_hybrid) then					! hmhj
          DO  k=1,LEVS							! hmhj
! hmhj ak5, bk5, ck5 in gen_coord_hybrid is the same order as model index
            BKLY(k)=0.5*(bk5(k)+bk5(k+1))				! hmhj
            CKLY(k)=0.5*(ck5(k)+ck5(k+1))*rkappa/thref(k)	        ! hmhj
            if( me.eq.0 )						! hmhj
     &         print*,'sl bkly ckly  in deldif=',k,sl(k),bkly(k),ckly(k)! hmhj
          enddo								! hmhj
       else if (hybrid) then						! hmhj
          DO  k=1,LEVS
! hmhj   sl(k) go bottom to top but bk(k) go top to bottom
            BKLY(k)=0.5*(bk5(levs-k+1)+bk5(levs-k+2))/SL(k)
            if( me.eq.0 ) print*,'sl bkly in deldif=',k,sl(k),bkly(k)
          enddo
       endif
!
       IF(JCAP.GT.170) THEN
!         RECIPROCAL OF TIME SCALE OF DIFFUSION AT REFERENCE WAVENUMBER NP
          RTNP=(JCAP/170.)**4*1.1/3600
          NP=JCAP
          N0=0             ! MAXIMUM WAVENUMBER FOR ZERO DIFFUSION
          JDEL=8           ! ORDER OF DIFFUSION (EVEN POWER TO RAISE DEL)
          FSHK=2.2         ! EXTRA HEIGHT-DEPENDENT DIFFUSION FACTOR PER SCALE HEIGHT
       ELSEIF(JCAP.EQ.170) THEN
!         RECIPROCAL OF TIME SCALE OF DIFFUSION AT REFERENCE WAVENUMBER NP
          RTNP=4*3.E15/(RERTH**4)*FLOAT(80*81)**2
          NP=JCAP
          N0=0.55*JCAP     ! MAXIMUM WAVENUMBER FOR ZERO DIFFUSION
          JDEL=2           ! ORDER OF DIFFUSION (EVEN POWER TO RAISE DEL)
          FSHK=1.0         ! EXTRA HEIGHT-DEPENDENT DIFFUSION FACTOR PER SCALE HEIGHT
       ELSEIF(JCAP.EQ.126) THEN					! hmhj
!         BELOW HAS BEEN TESTED IN SIHMA-THETA FOR 2 YEAR CFS RUN	! hmhj
          RTNP=4*3.E15/(RERTH**4)*FLOAT(80*81)**2			! hmhj
          NP=JCAP							! hmhj
          N0=0.0           						! hmhj
          JDEL=4           						! hmhj
          FSHK=1.0         						! hmhj
       ELSE
!         RECIPROCAL OF TIME SCALE OF DIFFUSION AT REFERENCE WAVENUMBER NP
          RTNP=1*3.E15/(RERTH**4)*FLOAT(80*81)**2
          NP=JCAP
c idea change3
          N0=0.55*JCAP     ! MAXIMUM WAVENUMBER FOR ZERO DIFFUSION
          JDEL=2           ! ORDER OF DIFFUSION (EVEN POWER TO RAISE DEL)
          FSHK=1.0         ! EXTRA HEIGHT-DEPENDENT DIFFUSION FACTOR PER SCALE HEIGHT
c         N0=0.     ! MAXIMUM WAVENUMBER FOR ZERO DIFFUSION
c         JDEL=4           ! ORDER OF DIFFUSION (EVEN POWER TO RAISE DEL)
c         FSHK=1.5         ! EXTRA HEIGHT-DEPENDENT DIFFUSION FACTOR PER SCALE HEIGHT
       ENDIF
!
       N00=N0
!
       SLRD0=0.002     ! SIGMA LEVEL AT WHICH TO BEGIN RAYLEIGH DAMPING
!       RTRD1=1./(10.*86400.) ! RECIPROCAL OF TIME SCALE PER SCALE HEIGHT
       RTRD1=0.
!       RTRD1=1./(5.*86400.) ! RECIPROCAL OF TIME SCALE PER SCALE HEIGHT
!jw        RTRD1=1./(2.*86400) ! RECIPROCAL OF TIME SCALE PER SCALE HEIGHT
                    !  ABOVE BEGINNING SIGMA LEVEL FOR RAYLEIGH DAMPING

        IF (ME.EQ.0) THEN
          PRINT 6,RTNP,NP,N0,JDEL
    6     FORMAT(' HORIZONTAL DIFFUSION PARAMETERS'/
     &  '   EFFECTIVE ',6PF10.3,' MICROHERTZ AT WAVENUMBER ',I4/
     &  '   MAXIMUM WAVENUMBER FOR ZERO DIFFUSION ',I4/
     &  '   ORDER OF DIFFUSION ',I2)

         print *, '***IDEA*** Using physical diffusion in all layers'
         print *,JCAP,N0,FSHK,rtrd1

        ENDIF
c idea change3 end
craa********************************************************************
!
        DO K=1,LEVS
          IF(SL(K).LT.SLRD0) THEN
            if (k .gt. levr) then
! idea
              RTRD(K)=RTRD1*LOG(SLRD0/SL(K)) ** 2
!             RTRD(K)=RTRD1*LOG(SLRD0/SL(K)) ** 3
            else
              RTRD(K)=RTRD1*LOG(SLRD0/SL(K))
            endif
c idea
c             RTRD(K)=min(1.e-5,RTRD(K))
              RTRD(K)=min(RTRD1,RTRD(K))
          ELSE
            RTRD(K)=0
          ENDIF
          RTHK(K)=(SL(K))**LOG(1/FSHK)
        ENDDO
!
        JDELH=JDEL/2
        NPD=MAX(NP-N0,0)
        REALVAL=NPD*(NPD+1)
        DN1=CONS2*RTNP/REALVAL**JDELH
!
!......................................................................
!
        DO LOCL=1,LS_MAX_NODE
               L=LS_NODE(LOCL,1)
          JBASEV=LS_NODE(LOCL,2)
          INDEV=INDLSEV(L,L)
          DO N=L,JCAP,2
            ND=MAX(N-N0,0)
            REALVAL=ND*(ND+1)
            DNE(INDEV)=DN1*REALVAL**JDELH
!
            REALVAL=real(N*(N+1))
            DNEidea(INDEV)=real(REALVAL)
            INDEV=INDEV+1
          ENDDO
        ENDDO
!
!......................................................................
!
        DO LOCL=1,LS_MAX_NODE
               L=LS_NODE(LOCL,1)
          JBASEV=LS_NODE(LOCL,2)
          if (mod(L,2).eq.mod(jcap+1,2)) then
            DNE(INDLSEV(JCAP+1,L))=CONS0 ! SET THE EVEN (N-L) TERMS OF THE TOP ROW TO ZERO
            DNEidea(INDLSEV(JCAP+1,L))=CONS0 ! SET THE EVEN (N-L) TERMS OF THE TOP ROW TO ZERO
          ENDIF
        ENDDO
!
!......................................................................
!
        DO LOCL=1,LS_MAX_NODE
               L=LS_NODE(LOCL,1)
          JBASOD=LS_NODE(LOCL,3)
          INDOD=INDLSOD(L+1,L)
          DO N=L+1,JCAP,2
            ND=MAX(N-N0,0)
            REALVAL=ND*(ND+1)
            DNO(INDOD)=DN1*REALVAL**JDELH
!
            REALVAL=real(N*(N+1))
            DNOidea(INDOD)=real(REALVAL)
            INDOD=INDOD+1
          ENDDO
        ENDDO
!
!......................................................................
!
        DO LOCL=1,LS_MAX_NODE
               L=LS_NODE(LOCL,1)
          JBASOD=LS_NODE(LOCL,3)
          if (mod(L,2).ne.mod(jcap+1,2)) then
            DNO(INDLSOD(JCAP+1,L))=CONS0 ! SET THE ODD (N-L) TERMS OF THE TOP ROW TO ZERO
            DNOidea(INDLSOD(JCAP+1,L))=CONS0 ! SET THE ODD (N-L) TERMS OF THE TOP ROW TO ZERO
          ENDIF
        ENDDO
!
!......................................................................
!
        DO K=1,LEVS
          KD=MAX(K-1,1)
          KU=MIN(K+1,LEVS)
          SF(K)=SL(K)/(SL(KU)-SL(KD))/SQRT(CONS2)     !CONSTANT
        ENDDO
!
        CALL COUNTPERF(1,15,0.)
!!
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
c     if(me.eq.0) then 
c        realval=real((JCAP-N00)*(JCAP-N00+1))
c        fact=real(JCAP*(JCAP+1))
c        print '(a,i5,5es11.3)','www5',k,rtrd(k),dn1*rthk(k)*realval,
c    $        dn2*fact,dn3*fact,dn4*fact
c     endif
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        RETURN
craa********************************************************************
craa********************************************************************
c End initialization
!!
      RETURN
      END
