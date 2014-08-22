module module_stoch
      implicit none
      public :: SETUP_STOCH, UPDATE_STOCH,do_fftback_along_x,do_fftback_along_y,&
                SP2GP_prep
      INTEGER :: LMINFORC, LMAXFORC, KMINFORC, KMAXFORC, &
      & LMINFORCT, LMAXFORCT, KMINFORCT, KMAXFORCT
      REAL :: ALPH, TOT_BACKSCAT_PSI, TOT_BACKSCAT_T, REXPONENT
      INTEGER :: LENSAV
      INTEGER,ALLOCATABLE:: wavenumber_k(:), wavenumber_l(:),ISEED(:)
      REAL, ALLOCATABLE :: WSAVE1(:),WSAVE2(:)
      REAL, PARAMETER:: RPI= 3.141592653589793
      REAL, PARAMETER:: CP= 1006
      save
contains
      subroutine SETUP_STOCH( &
                       VERTSTRUCC,VERTSTRUCS, &
                       SPT_AMP,SPSTREAM_AMP, &
                       stoch_vertstruc_opt, &
                       itime_step,DX,DY,NENS, &
                       TOT_BACKSCAT_PSI,TOT_BACKSCAT_T, &
                       ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte )
      USE module_configure
      IMPLICIT NONE
      TYPE (grid_config_rec_type) :: config_flags
      INTEGER :: IER,IK,IL,I,J
      INTEGER :: itime_step,stoch_vertstruc_opt
      INTEGER :: KMAX,LMAX,LENSAV,ILEV
      INTEGER , INTENT(IN) :: ids, ide, jds, jde, kds, kde, &
                                   ims, ime, jms, jme, kms, kme, &
                                   its, ite, jts, jte, kts, kte
      REAL :: DX,DY,RY,RX,RATIO_BACKSCAT,TOT_BACKSCAT_PSI,TOT_BACKSCAT_T
      REAL :: ZGAMMAN,ZTAU,ZCONSTF0,ZCONSTF0T,ZSIGMA2_EPS, RHOKLMAX,ZREF,RHOKL,EPS
      REAL, DIMENSION (ims:ime,kms:kme,jms:jme) :: VERTSTRUCC,VERTSTRUCS
      REAL, DIMENSION (ims:ime,jms:jme) :: SPSTREAM_AMP,SPT_AMP
      REAL, DIMENSION (ids:ide,jds:jde) :: ZCHI,ZCHIT
      INTEGER :: how_many, nens
      LOGICAL :: is_print = .true.
      LOGICAL , EXTERNAL :: wrf_dm_on_monitor
      KMAX=(jde-jds)+1
      LMAX=(ide-ids)+1
      RY= KMAX*DY
      RX= LMAX*DY
      LENSAV= 4*(KMAX+LMAX)+INT(LOG(REAL(KMAX))) + INT(LOG(REAL(LMAX))) + 8
      IF ( ALLOCATED(WSAVE1) ) DEALLOCATE(WSAVE1)
      IF ( ALLOCATED(WSAVE2) ) DEALLOCATE(WSAVE2)
      ALLOCATE(WSAVE1(LENSAV),WSAVE2(LENSAV))
      IF ( ALLOCATED(WAVENUMBER_K)) DEALLOCATE(WAVENUMBER_K)
      IF ( ALLOCATED(WAVENUMBER_L)) DEALLOCATE(WAVENUMBER_L)
      ALLOCATE (wavenumber_k(jds:jde),wavenumber_l(ids:ide))
      call CFFT1I (LMAX, WSAVE1, LENSAV, IER)
      if(ier.ne. 0) write(*,95) ier
      call CFFT1I (KMAX, WSAVE2, LENSAV, IER)
      if(ier.ne. 0) write(*,95) ier
      95 format('error in cFFT2I=  'i5)
      call findindex( wavenumber_k, wavenumber_l, &
                      ids, ide, jds, jde, kds, kde, &
                      ims, ime, jms, jme, kms, kme, &
                      its, ite, jts, jte, kts, kte )
      REXPONENT=-1.83
      KMINFORC=0
      KMAXFORC=min0(40,KMAX/2)
      LMINFORC=KMINFORC
      LMAXFORC=KMAXFORC
      KMINFORCT=0
      KMAXFORCT=KMAXFORC
      LMINFORCT=KMINFORCT
      LMAXFORCT=KMAXFORCT
      ZTAU = 2.E04/12.
      ALPH = float(itime_step)/ZTAU
      ZSIGMA2_EPS=1./(12.0*ALPH)
      IF (LMAXFORC>LMAX/2) then
        LMAXFORC=min0(40,LMAX/2)-1
        KMAXFORC=LMAXFORC
      ENDIF
      IF (LMAXFORCT>LMAX/2) then
        LMAXFORCT=min0(40,LMAX/2)-1
        KMAXFORCT=LMAXFORCT
      ENDIF
      IF ((LMINFORC>LMAXFORC).or.(KMINFORC>KMAXFORC)) then
        WRITE(*,'('' LMINFORC>LMAXFORC IN SETUP_STOCH.F90'')')
        STOP
      ENDIF
      IF ((KMAXFORC>KMAX/2).or.(LMAXFORC>LMAX/2)) then
        WRITE(*,'('' KMAXFORC>KMAX/2 IN SETUP_STOCH.F90'')')
        print*,KMAXFORC,KMAX/2
        STOP
      ENDIF
      IF ((KMINFORC.ne.LMINFORC).or.(KMAXFORC.ne.LMAXFORC)) then
        WRITE(*,'('' Forcing is non-homogenious in latitude and longitude'')')
        WRITE(*,'('' If this is what you want, comment *stop* IN SETUP_STOCH.F90'')')
        STOP
      ENDIF
      if (is_print) then
      WRITE(*,'(''                                               '')')
      WRITE(*,'('' =============================================='')')
      WRITE(*,'('' >> Initializing stochastic kinetic-energy backscatter scheme << '')')
      WRITE(*,'('' Total backscattered energy, TOT_BACKSCAT_PSI '',E12.5)') TOT_BACKSCAT_PSI
      WRITE(*,'('' Total backscattered temperature, TOT_BACKSCAT_T '',E12.5)') TOT_BACKSCAT_T
      WRITE(*,'('' Exponent for energy spectra, REXPONENT ='',E12.5)') REXPONENT
      WRITE(*,'('' Minimal wavenumber of streamfunction forcing, LMINFORC ='',I10)') LMINFORC
      WRITE(*,'('' Maximal wavenumber of streamfunction forcing, LMAXFORC ='',I10)') LMAXFORC
      WRITE(*,'('' Minimal wavenumber of streamfunction forcing, KMINFORC ='',I10)') KMINFORC
      WRITE(*,'('' Maximal wavenumber of streamfunction forcing, KMAXFORC ='',I10)') KMAXFORC
      WRITE(*,'('' Minimal wavenumber of temperature forcing, LMINFORCT ='',I10)') LMINFORCT
      WRITE(*,'('' Maximal wavenumber of temperature forcing, LMAXFORCT ='',I10)') LMAXFORCT
      WRITE(*,'('' stoch_vertstruc_opt                             '',I10)') stoch_vertstruc_opt
      WRITE(*,'('' Time step: itime_step='',I10)') itime_step
      WRITE(*,'('' Decorrelation time of noise, ZTAU ='',E12.5)') ZTAU
      WRITE(*,'('' Variance of noise, ZSIGMA2_EPS  ='',E12.5)') ZSIGMA2_EPS
      WRITE(*,'('' Autoregressive parameter 1-ALPH ='',E12.5)') 1.-ALPH
      WRITE(*,'('' =============================================='')')
      endif
      ZCHI = 0.0
      do IK=KMINFORC,KMAXFORC
      do IL=LMINFORC,LMAXFORC
      if ((sqrt(float(IK*IK+IL*IL))).le.(KMAXFORC)) then
        if ((IK>0).or.(IL>0)) then
          ZCHI(IL+1,IK+1)=((IK/RY*IK/RY)+(IL/RX*IL/RX))**(REXPONENT/2.)
        endif
      endif
      enddo
      enddo
      ZGAMMAN = 0.0
      DO IK=KMINFORC,KMAXFORC
      DO IL=LMINFORC,LMAXFORC
      if (sqrt(float(IK*IK+IL*IL)).le.KMAXFORC) then
        if ((IK>0).or.(IL>0)) then
          ZGAMMAN= ZGAMMAN + ((IK/RY*IK/RY)+(IL/RX*IL/RX))**(REXPONENT+1)
        endif
      endif
      ENDDO
      ENDDO
      ZGAMMAN=4.0*ZGAMMAN
      ZCONSTF0=SQRT(ALPH*TOT_BACKSCAT_PSI/(float(itime_step)*ZSIGMA2_EPS*ZGAMMAN))/(2*RPI)
      ZCHIT = 0.0
      do IK=KMINFORCT,KMAXFORCT
      do IL=LMINFORCT,LMAXFORCT
      if ((sqrt(float(IK*IK+IL*IL))).le.(KMAXFORCT)) then
        if ((IK>0).or.(IL>0)) then
          ZCHIT(IL+1,IK+1)=((IK/RY*IK/RY)+(IL/RX*IL/RX))**(REXPONENT/2.)
        endif
      endif
      enddo
      enddo
      ZGAMMAN = 0.0
      DO IK=KMINFORCT,KMAXFORCT
      DO IL=LMINFORCT,LMAXFORCT
      if (sqrt(float(IK*IK+IL*IL)).le.KMAXFORC) then
        if ((IK>0).or.(IL>0)) then
          ZGAMMAN= ZGAMMAN + ((IK/RY*IK/RY)+(IL/RX*IL/RX))**(REXPONENT+1)
        endif
      endif
      ENDDO
      ENDDO
      ZGAMMAN=4.0*ZGAMMAN
      ZCONSTF0T=TOT_BACKSCAT_T /cp* SQRT(ALPH/(ZSIGMA2_EPS*ZGAMMAN))/(2*RPI)
      SPSTREAM_AMP=0.0
      SPT_AMP=0.0
      SPT_AMP=0.0
      DO IK=jts,jte
      DO IL=its,ite
      if ((IL .le. (LMAX/2+1)) .and. (IK .le. (KMAX/2+1)) ) then
        SPSTREAM_AMP(IL,IK) = ZCONSTF0 *ZCHI(IL,IK)
        SPT_AMP(IL,IK) = ZCONSTF0T*ZCHIT(IL,IK)
      endif
      ENDDO
      ENDDO
      DO IK=jts,jte
      DO IL=its,ite
      if ( (IL .gt. (LMAX/2+1)) .and. (IK .le. (KMAX/2+1)) ) then
        SPSTREAM_AMP(IL,IK) = ZCONSTF0 *ZCHI(LMAX-IL+2,IK)
        SPT_AMP(IL,IK) = ZCONSTF0T*ZCHIT(LMAX-IL+2,IK)
      endif
      ENDDO
      ENDDO
      DO IK=jts,jte
      DO IL=its,ite
      if ((IK .gt. (KMAX/2+1)) .and. (IL.le.LMAX/2) ) then
        SPSTREAM_AMP(IL,IK) = ZCONSTF0 *ZCHI(IL,KMAX-IK+2)
        SPT_AMP(IL,IK) = ZCONSTF0T*ZCHIT(IL,KMAX-IK+2)
      endif
      ENDDO
      ENDDO
      DO IK=jts,jte
      DO IL=its,ite
      if ((IK .gt. (KMAX/2+1)) .and. (IL.gt.LMAX/2) ) then
        SPSTREAM_AMP(IL,IK) = ZCONSTF0 *ZCHI(LMAX-IL+2,KMAX-IK+2)
        SPT_AMP(IL,IK) = ZCONSTF0T*ZCHIT(LMAX-IL+2,KMAX-IK+2)
      endif
      ENDDO
      ENDDO
      IF (stoch_vertstruc_opt>0) then
        VERTSTRUCC=0.0
        VERTSTRUCS=0.0
        RHOKLMAX= sqrt(KMAX**2/DY**2 + LMAX**2/DX**2)
        ZREF=32.0
        DO ILEV=kts,kte
          DO IK=jts,jte
            DO IL=its,ite
            if (IL.le.(LMAX/2)) then
              RHOKL = sqrt((IK+1)**2/DY**2 + (IL+1)**2/DX**2)
              EPS = ((RHOKLMAX - RHOKL)/ RHOKLMAX) * (ILEV/ZREF) * RPI
              VERTSTRUCC(IL,ILEV,IK) = cos ( eps* (IL+1) )
              VERTSTRUCS(IL,ILEV,IK) = sin ( eps* (IL+1) )
             else
              RHOKL = sqrt((IK+1)**2/DY**2 + (LMAX-IL+2)**2/DX**2)
              EPS = ((RHOKLMAX - RHOKL)/ RHOKLMAX) * (ILEV/ZREF) * RPI
              VERTSTRUCC (IL,ILEV,IK) = cos ( eps* (LMAX-IL+2) )
              VERTSTRUCS (IL,ILEV,IK) = - sin ( eps* (LMAX-IL+2) )
            endif
            ENDDO
          ENDDO
        ENDDO
      END IF
       CALL random_seed(size=how_many)
       IF ( ALLOCATED(ISEED) ) DEALLOCATE(ISEED)
       ALLOCATE (ISEED(how_many))
       iseed=0
       IF ( wrf_dm_on_monitor() ) THEN
          iseed(1) = 7654321
          iseed(2) = 2*(nens*811)+1
          call random_seed(put=iseed)
       END IF
       CALL wrf_dm_bcast_integer ( iseed , how_many )
       END subroutine SETUP_STOCH
      subroutine UPDATE_STOCH( &
                       SPSTREAMFORCS,SPSTREAMFORCC,SPTFORCS,SPTFORCC, &
                       SPT_AMP,SPSTREAM_AMP, &
                       itime,ij, &
                       ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte )
      IMPLICIT NONE
      REAL, DIMENSION( ids:ide,jds:jde) :: ZRANDNOSS1,ZRANDNOSC1,ZRANDNOSS2,ZRANDNOSC2
      REAL, DIMENSION (ims:ime,jms:jme) :: SPSTREAMFORCS,SPSTREAMFORCC,SPTFORCS,SPTFORCc,SPSTREAM_AMP,SPT_AMP
      INTEGER , INTENT(IN) :: ids, ide, jds, jde, kds, kde, &
                                                ims, ime, jms, jme, kms, kme, &
                                                its, ite, jts, jte, kts, kte
      REAL :: Z, thresh
      INTEGER ::IL, IK,LMAX,KMAX,i,itime,ij,llmax,kkmax,how_many
      LOGICAL :: LGAUSS
      KMAX=(jde-jds)+1
      LMAX=(ide-ids)+1
      CALL random_seed(size=how_many)
      call random_seed(put=iseed)
      LGAUSS=.false.
      thresh=3.0
      IF (LGAUSS) then
        DO IK=jds,int(jde/2.0)+2
          DO IL=ids,ide
            do
              call gauss_noise(z)
              if (abs(z).le.thresh) exit
            enddo
            ZRANDNOSS1(IL,IK)=z
            do
              call gauss_noise(z)
              if (abs(z).le.thresh) exit
            enddo
            ZRANDNOSC1(IL,IK)=z
            do
              call gauss_noise(z)
              if (abs(z).le.thresh) exit
            enddo
            ZRANDNOSS2(IL,IK)=z
            do
              call gauss_noise(z)
              if (abs(z).le.thresh) exit
            enddo
            ZRANDNOSC2(IL,IK)=z
          ENDDO
        ENDDO
      ELSE
        DO IK=jds,int(jde/2.0)+2
          DO IL=ids,ide
            CALL RANDOM_NUMBER(z)
            ZRANDNOSS1(IL,IK)=z-0.5
            CALL RANDOM_NUMBER(z)
            ZRANDNOSC1(IL,IK)=z-0.5
            CALL RANDOM_NUMBER(z)
            ZRANDNOSS2(IL,IK)=z-0.5
            CALL RANDOM_NUMBER(z)
            ZRANDNOSC2(IL,IK)=z-0.5
          ENDDO
        ENDDO
      ENDIF
      call random_seed(get=iseed)
      DO IK=jts,jte
      if (IK.le.(KMAX/2)) then
        DO IL=its,ite
          SPSTREAMFORCC(IL,IK) = (1.-ALPH)*SPSTREAMFORCC(IL,IK) + SPSTREAM_AMP(IL,IK)*(ZRANDNOSC1(IL,IK))
          SPSTREAMFORCS(IL,IK) = (1.-ALPH)*SPSTREAMFORCS(IL,IK) + SPSTREAM_AMP(IL,IK)*(ZRANDNOSS1(IL,IK))
          SPTFORCC(IL,IK) = (1.-ALPH)*SPTFORCC(IL,IK) + SPT_AMP(IL,IK) *(ZRANDNOSC2(IL,IK))
          SPTFORCS(IL,IK) = (1.-ALPH)*SPTFORCS(IL,IK) + SPT_AMP(IL,IK) *(ZRANDNOSS2(IL,IK))
        ENDDO
      endif
      ENDDO
      DO IK=jts,jte
      if (IK.ge.(KMAX/2+1))then
        DO IL=its,ite
        if (IL>1) then
          SPSTREAMFORCC(IL,IK)= (1.-ALPH)* SPSTREAMFORCC(IL,IK) + &
                                                  SPSTREAM_AMP(IL,IK) * ZRANDNOSC1(LMAX-IL+2,KMAX-IK+2)
          SPSTREAMFORCS(IL,IK)= -((1.-ALPH)*(-1.0*SPSTREAMFORCS(IL,IK))+ &
                                                  SPSTREAM_AMP(IL,IK) * ZRANDNOSS1(LMAX-IL+2,KMAX-IK+2))
          SPTFORCC(IL,IK)= (1.-ALPH)* SPTFORCC(IL,IK) + &
                                                  SPT_AMP(IL,IK) * ZRANDNOSC2(LMAX-IL+2,KMAX-IK+2)
          SPTFORCS(IL,IK)= -((1.-ALPH)*(-1.0*SPTFORCS(IL,IK))+ &
                                                  SPT_AMP(IL,IK) * ZRANDNOSS2(LMAX-IL+2,KMAX-IK+2))
        else
          SPSTREAMFORCC(1,IK) = (1.-ALPH) * SPSTREAMFORCC(1,IK) + &
                                                  SPSTREAM_AMP(1,IK) * ZRANDNOSC1(1,KMAX-IK+2)
          SPSTREAMFORCS(1,IK) = -((1.-ALPH)*(-1.0*SPSTREAMFORCS(1,IK))+ &
                                                  SPSTREAM_AMP(1,IK) * ZRANDNOSS1(1,KMAX-IK+2))
          SPTFORCC(1,IK) = (1.-ALPH) * SPTFORCC(1,IK) + &
                                                  SPT_AMP(1,IK) * ZRANDNOSC2(1,KMAX-IK+2)
          SPTFORCS(1,IK) = -((1.-ALPH)*(-1.0*SPTFORCS(1,IK))+ &
                                                  SPT_AMP(1,IK) * ZRANDNOSS2(1,KMAX-IK+2))
         endif
       ENDDO
     endif
     ENDDO
     END subroutine UPDATE_STOCH
      SUBROUTINE CALCULATE_STOCH_TEN( &
                       ru_tendf,rv_tendf,t_tendf, &
                       GPUFORC,GPVFORC,GPTFORC, &
                       ru_real,rv_real,rt_real, &
                       mu,mub, &
                       ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte, &
                       dt)
       IMPLICIT NONE
       INTEGER , INTENT(IN) :: ids, ide, jds, jde, kds, kde, &
                                       ims, ime, jms, jme, kms, kme, &
                                       its, ite, jts, jte, kts, kte
       REAL , DIMENSION(ims:ime , kms:kme, jms:jme),INTENT(INOUT) :: &
                                        ru_tendf, rv_tendf, t_tendf, &
                                        GPUFORC,GPVFORC,GPTFORC
       REAL , DIMENSION(ims:ime , kms:kme, jms:jme),INTENT(IN) :: &
                                        ru_real,rv_real,rt_real
       REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: mu,mub
       INTEGER :: I,J,K
       REAL :: dt
       DO j = jts,MIN(jde-1,jte)
         DO k = kts,kte-1
           DO i = its,ite
             GPUFORC(i,k,j)= ru_real(i,k,j)
           ENDDO
         ENDDO
       ENDDO
       DO j = jts,jte
         DO k = kts,kte-1
           DO i = its,MIN(ide-1,ite)
             GPVFORC(i,k,j)= rv_real(i,k,j)
           ENDDO
         ENDDO
       ENDDO
       DO j = jts,MIN(jde-1,jte)
         DO k = kts,kte-1
           DO i = its,MIN(ide-1,ite)
             GPTFORC(i,k,j)= rt_real(i,k,j)
           ENDDO
         ENDDO
       ENDDO
       END SUBROUTINE CALCULATE_STOCH_TEN
      SUBROUTINE UPDATE_STOCH_TEN(ru_tendf,rv_tendf,t_tendf, &
                       GPUFORC,GPVFORC,GPTFORC, &
                       mu,mub, &
                       ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte, &
                       dt )
       IMPLICIT NONE
       INTEGER , INTENT(IN) :: ids, ide, jds, jde, kds, kde, &
                                       ims, ime, jms, jme, kms, kme, &
                                       its, ite, jts, jte, kts, kte
       REAL , DIMENSION(ims:ime , kms:kme, jms:jme),INTENT(INOUT) :: &
                                       ru_tendf, rv_tendf, t_tendf
       REAL , DIMENSION(ims:ime , kms:kme, jms:jme) :: &
                                       GPUFORC,GPVFORC,GPTFORC
       REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: mu,mub
       INTEGER :: I,J,K
       REAL :: dt,xm
       DO j = jts,MIN(jde-1,jte)
         DO k = kts,kte-1
           DO i = its,ite
             ru_tendf(i,k,j) = ru_tendf(i,k,j) + GPUFORC(i,k,j) * (mu(i,j)+mub(i,j))
           ENDDO
         ENDDO
       ENDDO
       DO j = jts,jte
         DO i = its,MIN(ide-1,ite)
           DO k = kts,kte-1
             rv_tendf(i,k,j) = rv_tendf(i,k,j) + GPVFORC(i,k,j) * (mu(i,j)+mub(i,j))
           ENDDO
         ENDDO
       ENDDO
       DO j = jts,MIN(jde-1,jte)
         DO k = kts,kte-1
           DO i = its,MIN(ide-1,ite)
             t_tendf(i,k,j) = t_tendf(i,k,j) + GPTFORC(i,k,j) * (mu(i,j)+mub(i,j))
           ENDDO
         ENDDO
       ENDDO
       END SUBROUTINE UPDATE_STOCH_TEN
       subroutine SP2GP_prep( &
                       SPSTREAMFORCS,SPSTREAMFORCC,SPTFORCS,SPTFORCC, &
                       VERTSTRUCC,VERTSTRUCS, &
                       RU_REAL,RV_REAL,RT_REAL, &
                       RU_IMAG,RV_IMAG,RT_IMAG, &
                       dx,dy,stoch_vertstruc_opt, &
                       ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte )
      IMPLICIT NONE
      INTEGER , INTENT(IN) :: ids, ide, jds, jde, kds, kde, &
                                   ims, ime, jms, jme, kms, kme, &
                                   its, ite, jts, jte, kts, kte
      REAL, DIMENSION (ims:ime , jms:jme) :: SPSTREAMFORCS,SPSTREAMFORCC,SPTFORCS,SPTFORCC
      REAL, DIMENSION (ims:ime , kms:kme, jms:jme) :: RU_REAL,RV_REAL,RT_REAL,RU_IMAG,RV_IMAG,RT_IMAG, &
                                                         VERTSTRUCC,VERTSTRUCS
      INTEGER :: IK,IL,ILEV,NLAT,NLON,stoch_vertstruc_opt
      REAL :: dx,dy,RY,RX
      NLAT=(jde-jds)+1
      NLON=(ide-ids)+1
      RY= NLAT*DY
      RX= NLON*DX
      DO ILEV=kts,kte
      if (stoch_vertstruc_opt==0) then
        DO IL=its,ite
        DO IK=jts,jte
          rt_real(IL,ILEV,IK) = SPTFORCC(IL,IK)
          rt_imag(IL,ILEV,IK) = SPTFORCS(IL,IK)
          ru_real(IL,ILEV,IK) = 2*RPI/RY* wavenumber_k(IK) * SPSTREAMFORCS(IL,IK)
          ru_imag(IL,ILEV,IK) =-2*RPI/RY* wavenumber_k(IK) * SPSTREAMFORCC(IL,IK)
          rv_real(IL,ILEV,IK) =-2*RPI/RX* wavenumber_l(IL) * SPSTREAMFORCS(IL,IK)
          rv_imag(IL,ILEV,IK) = 2*RPI/RX* wavenumber_l(IL) * SPSTREAMFORCC(IL,IK)
        ENDDO
        ENDDO
       elseif (stoch_vertstruc_opt==1) then
        DO IL=its,ite
        DO IK=jts,jte
          rt_real(IL,ILEV,IK) = SPTFORCC(IL,IK)*VERTSTRUCC(IL,ILEV,IK) - SPTFORCS(IL,IK)*VERTSTRUCS(IL,ILEV,IK)
          rt_imag(IL,ILEV,IK) = SPTFORCC(IL,IK)*VERTSTRUCS(IL,ILEV,IK) + SPTFORCS(IL,IK)*VERTSTRUCC(IL,ILEV,IK)
          ru_real(IL,ILEV,IK) = 2*RPI/RY* wavenumber_k(IK) *&
                            (+SPSTREAMFORCC(IL,IK)*VERTSTRUCS(IL,ILEV,IK) + SPSTREAMFORCS(IL,IK)*VERTSTRUCC(IL,ILEV,IK))
          ru_imag(IL,ILEV,IK) = 2*RPI/RY* wavenumber_k(IK) *&
                            (-SPSTREAMFORCC(IL,IK)*VERTSTRUCC(IL,ILEV,IK) + SPSTREAMFORCS(IL,IK)*VERTSTRUCS(IL,ILEV,IK))
          rv_real(IL,ILEV,IK) = 2*RPI/RX* wavenumber_l(IL) *&
                             (-SPSTREAMFORCC(IL,IK)*VERTSTRUCS(IL,ILEV,IK) - SPSTREAMFORCS(IL,IK)*VERTSTRUCC(IL,ILEV,IK))
          rv_imag(IL,ILEV,IK) = 2*RPI/RX* wavenumber_l(IL) *&
                             (+SPSTREAMFORCC(IL,IK)*VERTSTRUCC(IL,ILEV,IK) - SPSTREAMFORCS(IL,IK)*VERTSTRUCS(IL,ILEV,IK))
        ENDDO
        ENDDO
      endif
      ENDDO
      END subroutine SP2GP_prep
       subroutine do_fftback_along_x(fieldc_U_xxx,fields_U_xxx, &
                                     fieldc_V_xxx,fields_V_xxx, &
                                     fieldc_T_xxx,fields_T_xxx, &
                               ids, ide, jds, jde, kds, kde, &
                               ims, ime, jms, jme, kms, kme, &
                               ips, ipe, jps, jpe, kps, kpe, &
                               imsx,imex,jmsx,jmex,kmsx,kmex, &
                               ipsx,ipex,jpsx,jpex,kpsx,kpex, &
                               imsy,imey,jmsy,jmey,kmsy,kmey, &
                               ipsy,ipey,jpsy,jpey,kpsy,kpey, &
                               k_start , k_end &
                              )
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde, &
                             ims, ime, jms, jme, kms, kme, &
                             ips, ipe, jps, jpe, kps, kpe, &
                             imsx,imex,jmsx,jmex,kmsx,kmex, &
                             ipsx,ipex,jpsx,jpex,kpsx,kpex, &
                             imsy,imey,jmsy,jmey,kmsy,kmey, &
                             ipsy,ipey,jpsy,jpey,kpsy,kpey, &
                             k_start , k_end
       REAL, DIMENSION (imsx:imex, kmsx:kmex, jmsx:jmex) :: fieldc_U_xxx,fields_U_xxx, &
                                                               fieldc_V_xxx,fields_V_xxx, &
                                                               fieldc_T_xxx,fields_T_xxx
       COMPLEX, DIMENSION (ipsx:ipex) :: dummy_complex
       INTEGER :: IER,LENWRK,KMAX,LMAX,I,J,K
       REAL, ALLOCATABLE :: WORK(:)
       CHARACTER (LEN=160) :: mess
       KMAX=(jde-jds)+1
       LMAX=(ide-ids)+1
       LENWRK=2*KMAX*LMAX
       ALLOCATE(WORK(LENWRK))
       LENSAV= 4*(KMAX+LMAX)+INT(LOG(REAL(KMAX))) + INT(LOG(REAL(LMAX))) + 8
       DO k=kpsx,kpex
         DO j = jpsx, jpex
           DO i = ipsx, ipex
             dummy_complex(i)=cmplx(fieldc_U_xxx(i,k,j),fields_U_xxx(i,k,j))
           ENDDO
           CALL cFFT1B (LMAX, 1 ,dummy_complex,LMAX, WSAVE1, LENSAV, WORK, LENWRK, IER)
           if (ier.ne.0) then
              WRITE(mess,FMT='(A)') 'error in cFFT1B in do_fftback_along_x, field U'
              CALL wrf_debug(0,mess)
           end if
           DO i = ipsx, ipex
             fieldc_U_xxx(i,k,j)=real(dummy_complex(i))
             fields_U_xxx(i,k,j)=imag(dummy_complex(i))
           END DO
         END DO
       END DO
       DO k=kpsx,kpex
         DO j = jpsx, jpex
           DO i = ipsx, ipex
             dummy_complex(i)=cmplx(fieldc_V_xxx(i,k,j),fields_V_xxx(i,k,j))
           ENDDO
           CALL cFFT1B (LMAX, 1 ,dummy_complex,LMAX, WSAVE1, LENSAV, WORK, LENWRK, IER)
           if (ier.ne.0) then
              WRITE(mess,FMT='(A)') 'error in cFFT1B in do_fftback_along_x, field V'
              CALL wrf_debug(0,mess)
           end if
           DO i = ipsx,ipex
             fieldc_V_xxx(i,k,j)=real(dummy_complex(i))
             fields_V_xxx(i,k,j)=imag(dummy_complex(i))
           END DO
         END DO
       END DO
       DO k=kpsx,kpex
         DO j = jpsx, jpex
           DO i = ipsx, ipex
             dummy_complex(i)=cmplx(fieldc_T_xxx(i,k,j),fields_T_xxx(i,k,j))
           ENDDO
           CALL cFFT1B (LMAX, 1 ,dummy_complex,LMAX, WSAVE1, LENSAV, WORK, LENWRK, IER)
           if (ier.ne.0) then
              WRITE(mess,FMT='(A)') 'error in cFFT1B in do_fftback_along_x, field T'
              CALL wrf_debug(0,mess)
           end if
           DO i = ipsx, ipex
            fieldc_T_xxx(i,k,j)=real(dummy_complex(i))
            fields_T_xxx(i,k,j)=imag(dummy_complex(i))
           END DO
         END DO
       END DO
       DEALLOCATE(WORK)
       end subroutine do_fftback_along_x
       subroutine do_fftback_along_y(fieldc_U_yyy,fields_U_yyy, &
                                     fieldc_V_yyy,fields_V_yyy, &
                                     fieldc_T_yyy,fields_T_yyy, &
                               ids, ide, jds, jde, kds, kde, &
                               ims, ime, jms, jme, kms, kme, &
                               ips, ipe, jps, jpe, kps, kpe, &
                               imsx,imex,jmsx,jmex,kmsx,kmex, &
                               ipsx,ipex,jpsx,jpex,kpsx,kpex, &
                               imsy,imey,jmsy,jmey,kmsy,kmey, &
                               ipsy,ipey,jpsy,jpey,kpsy,kpey, &
                               k_start , k_end &
                              )
       IMPLICIT NONE
       INTEGER :: IER,LENWRK,KMAX,LMAX,I,J,K
       INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde, &
                             ims, ime, jms, jme, kms, kme, &
                             ips, ipe, jps, jpe, kps, kpe, &
                             imsx,imex,jmsx,jmex,kmsx,kmex, &
                             ipsx,ipex,jpsx,jpex,kpsx,kpex, &
                             imsy,imey,jmsy,jmey,kmsy,kmey, &
                             ipsy,ipey,jpsy,jpey,kpsy,kpey, &
                             k_start , k_end
       REAL, DIMENSION (imsy:imey, kmsy:kmey, jmsy:jmey) :: fieldc_U_yyy,fields_U_yyy, &
                                                               fieldc_V_yyy,fields_V_yyy, &
                                                               fieldc_T_yyy,fields_T_yyy
       COMPLEX, DIMENSION (jpsy:jpey) :: dummy_complex
       REAL, ALLOCATABLE :: WORK(:)
       CHARACTER (LEN=160) :: mess
       KMAX=(jde-jds)+1
       LMAX=(ide-ids)+1
       LENWRK=2*KMAX*LMAX
       ALLOCATE(WORK(LENWRK))
       LENSAV= 4*(KMAX+LMAX)+INT(LOG(REAL(KMAX))) + INT(LOG(REAL(LMAX))) + 8
        DO k=kpsy,kpey
          DO i = ipsy, ipey
            DO j = jpsy,jpey
            dummy_complex(j)=cmplx(fieldc_U_yyy(i,k,j),fields_U_yyy(i,k,j))
            ENDDO
            CALL cFFT1B (KMAX, 1 ,dummy_complex,KMAX, WSAVE2, LENSAV, WORK, LENWRK, IER)
            if (ier.ne.0) then
               WRITE(mess,FMT='(A)') 'error in cFFT1B in do_fftback_along_y, field U'
               CALL wrf_debug(0,mess)
            end if
            DO j = jpsy, jpey
            fieldc_U_yyy(i,k,j)=real(dummy_complex(j))
            fields_U_yyy(i,k,j)=imag(dummy_complex(j))
            END DO
          END DO
        END DO
        DO k=kpsy,kpey
          DO i = ipsy, ipey
            DO j = jpsy, jpey
            dummy_complex(j)=cmplx(fieldc_V_yyy(i,k,j),fields_V_yyy(i,k,j))
            ENDDO
            CALL cFFT1B (KMAX, 1 ,dummy_complex,KMAX, WSAVE2, LENSAV, WORK, LENWRK, IER)
            if (ier.ne.0) then
               WRITE(mess,FMT='(A)') 'error in cFFT1B in do_fftback_along_y, field V'
               CALL wrf_debug(0,mess)
            end if
            DO j = jpsy, jpey
            fieldc_V_yyy(i,k,j)=real(dummy_complex(j))
            fields_V_yyy(i,k,j)=imag(dummy_complex(j))
            END DO
          END DO
        END DO
        DO k=kpsy,kpey
          DO i = ipsy, ipey
            DO j = jpsy,jpey
            dummy_complex(j)=cmplx(fieldc_T_yyy(i,k,j),fields_T_yyy(i,k,j))
            ENDDO
            CALL cFFT1B (KMAX, 1 ,dummy_complex,KMAX, WSAVE2, LENSAV, WORK, LENWRK, IER)
            if (ier.ne.0) then
               WRITE(mess,FMT='(A)') 'error in cFFT1B in do_fftback_along_y, field T'
               CALL wrf_debug(0,mess)
            end if
            DO j = jpsy,jpey
            fieldc_T_yyy(i,k,j)=real(dummy_complex(j))
            fields_T_yyy(i,k,j)=imag(dummy_complex(j))
            END DO
          END DO
        END DO
       DEALLOCATE(WORK)
       end subroutine do_fftback_along_y
      subroutine findindex( wavenumber_k, wavenumber_L, &
                       ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte )
      IMPLICIT NONE
      INTEGER :: IK,IL,KMAX,LMAX
      INTEGER, DIMENSION (jds:jde):: wavenumber_k
      INTEGER, DIMENSION (ids:ide):: wavenumber_l
      INTEGER , INTENT(IN) :: ids, ide, jds, jde, kds, kde, &
                                   ims, ime, jms, jme, kms, kme, &
                                   its, ite, jts, jte, kts, kte
      KMAX=(jde-jds)+1
      LMAX=(ide-ids)+1
      DO IK=1,KMAX/2+1
        wavenumber_k(IK)=IK-1
      ENDDO
      DO IK=KMAX,KMAX/2+2,-1
        wavenumber_k(IK)=IK-KMAX-1
      ENDDO
      DO IL=1,LMAX/2+1
        wavenumber_l(IL)=IL-1
      ENDDO
      DO IL=LMAX,LMAX/2+2,-1
        wavenumber_l(IL)=IL-LMAX-1
      ENDDO
      END subroutine findindex
     subroutine gauss_noise(z)
      real :: z
      real :: x,y,r, coeff
      do
      call random_number( x )
      call random_number( y )
      x = 2.0 * x - 1.0
      y = 2.0 * y - 1.0
      r = x * x + y * y
      if ( r > 0.0 .and. r < 1.0 ) exit
      end do
      coeff = sqrt( -2.0 * log(r) / r )
      z = coeff * x
     end subroutine gauss_noise
      end module module_stoch
