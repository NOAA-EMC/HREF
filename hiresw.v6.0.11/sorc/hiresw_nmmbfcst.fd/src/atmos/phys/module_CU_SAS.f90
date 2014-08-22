!-----------------------------------------------------------------------
!
      MODULE MODULE_CU_SAS
!
!     12-10-2010  Created by Weiguo Wang
!-----------------------------------------------------------------------
!
!***  THE CONVECTION DRIVERS AND PACKAGES
!
!-----------------------------------------------------------------------
!
      USE MODULE_INCLUDE
!
      USE MODULE_CONSTANTS,ONLY : g99 => g, CP, ELWV,EPSQ
      use machine , only : kind_phys
      use funcphys , only : fpvs, gpvs

!
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      PRIVATE
      REAL,    PARAMETER ::    XLV=ELWV
!
      PUBLIC :: SASDRV 
      PUBLIC :: SAS_INIT
!
!-----------------------------------------------------------------------
       CONTAINS
!
!-----------------------------------------------------------------------
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!-----------------------------------------------------------------------
!
      SUBROUTINE SASDRV( &
                        IMS,IME,JMS,JME &
                       ,ITS,ITE,JTS,JTE,lm &
                       ,DT,NTSD,NCNVC &
                       ,TH,T,SICE,OMGALF,SHEAT,LHEAT,PBLH,U,V &
                       ,WATER,P_QV,P_QC,P_QR,P_QS,P_QI,P_QG,NUM_WATER &
                       ,PINT,PMID,exner,RR,DZ &
                       ,XLAND,CU_ACT_FLAG &
                       ,RAINCV,CUTOP,CUBOT &   !! out below
                       ,DUDT,DVDT &
                      ! optional
                       ,RTHCUTEN,RQVCUTEN &
                       ,RQCCUTEN,RQRCUTEN &
                       ,RQICUTEN,RQSCUTEN &
                       ,RQGCUTEN &
                       )
!-----------------------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------------------------------
      INTEGER,INTENT(IN):: &
       IMS,IME,JMS,JME & 
      ,ITS,ITE,JTS,JTE,lm
!
      INTEGER,INTENT(IN) :: P_QV,P_QG,P_QR,P_QI,P_QC,P_QS,NUM_WATER
!
      INTEGER,INTENT(IN) :: ntsd,NCNVC
      REAL,   INTENT(IN) :: DT
!
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN):: &
       XLAND,SICE,PBLH,SHEAT,LHEAT
!
      REAL,DIMENSION(IMS:IME,JMS:JME,1:lm),INTENT(IN):: &
       dz,exner,OMGALF,pmid,rr,t,th,U,V 
!
      REAL,DIMENSION(IMS:IME,JMS:JME,1:lm+1),INTENT(IN):: &
       pint
!
      REAL,DIMENSION(IMS:IME,JMS:JME,1:lm,NUM_WATER),INTENT(IN):: &
       WATER 
!
      REAL,DIMENSION(IMS:IME,JMS:JME,1:lm),optional,intent(inout):: &
       RQVCUTEN,RTHCUTEN &
      ,RQCCUTEN,RQRCUTEN &
      ,RQSCUTEN,RQICUTEN &
      ,RQGCUTEN 
! 
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT):: &
       RAINCV
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT):: &
       CUBOT,CUTOP
!
      REAL,DIMENSION(IMS:IME,JMS:JME,1:lm),INTENT(OUT):: &
       DUDT,DVDT 
!
      LOGICAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT):: &
       CU_ACT_FLAG
!
      LOGICAL DEEP, SHALLOW
!
!-----------------------------------------------------------------------
!***
!***  LOCAL VARIABLES
!***
!-----------------------------------------------------------------------
!      INTEGER :: LBOT,LPBL,LTOP
! 
!      REAL,DIMENSION(1:lm) :: DPCOL,DQDT,DTDT,PCOL,QCOL,TCOL
!
      INTEGER :: I,J,K,ICLDCK,KFLIP

! For SAS
      INTEGER :: KM
      INTEGER, PARAMETER :: IX=1, IM=1, ncloud=1
      INTEGER :: jcap, kcnv(IX), KBOT(IX), KTOP(IX)
      REAL(kind=kind_phys), DIMENSION(IX,lm) :: delp, prsl,phil,q1,t1,u1,v1,VVEL,     &
                                ud_mf,dd_mf,dt_mf, q0,t0,u0,v0
      REAL(kind=kind_phys), DIMENSION(IX) :: psp,cldwrk,rn,slimsk,hpbl,hflx,evap
      REAL(kind=kind_phys), DIMENSION(IX,lm,2) :: CLW, CLW0  !! 1-ice  2-liquid 
      REAL(kind=kind_phys) :: fract, tmp, delt, landmask, DTCNVC, mommix
      REAL, DIMENSION(lm+1)    :: ZF
      LOGICAL :: lpr
       lpr=.true.
       lpr=.false.

      DEEP = .TRUE.
      SHALLOW = .TRUE.
      KM = lm
       mommix = 1.0    !!! HWRF uses this to adjust/tune moment mixing

!.......................................................................
!$omp parallel do                &
!$omp     private(k,j,i)
!.......................................................................
       DO K=1,lm
        DO J=JMS,JME
         DO I=IMS,IME
          DUDT(I,J,K) = 0.0
          DVDT(I,J,K) = 0.0
         ENDDO
        ENDDO
       ENDDO
!.......................................................................
!$omp end parallel do              
!.......................................................................
!.......................................................................
!$omp parallel do                &
!$omp     private(k,j,i)
!.......................................................................
       DO K=1,lm
        DO J=JMS,JME
         DO I=IMS,IME
            RTHCUTEN(I,J,K) = 0.0
            RQVCUTEN(I,J,K) = 0.0
            RQCCUTEN(I,J,K) = 0.0
            RQRCUTEN(I,J,K) = 0.0
            RQICUTEN(I,J,K) = 0.0
            RQSCUTEN(I,J,K) = 0.0
            RQGCUTEN(I,J,K) = 0.0
         ENDDO
        ENDDO
       ENDDO
!.......................................................................
!$omp end parallel do                
!.......................................................................
      IF ( (.NOT. DEEP) .AND. (.NOT. SHALLOW) ) RETURN

!-----------------------------------------------------------------------
!
!***  PREPARE TO CALL SAS CONVECTION SCHEME
!
!-----------------------------------------------------------------------
!
!***  CHECK TO SEE IF THIS IS A CONVECTION TIMESTEP
!                                                                        
      ICLDCK=MOD(ntsd,NCNVC)                                              
!-----------------------------------------------------------------------
!                                                                      
!***  COMPUTE CONVECTION EVERY NCNVC*DT/60.0 MINUTES
!                                                                     

      IF(ICLDCK==0.OR.ntsd==0)THEN                       !!! call convection
!
        DO J=JTS,JTE
        DO I=ITS,ITE
          CU_ACT_FLAG(I,J)=.TRUE.
        ENDDO
        ENDDO
!
        DTCNVC=DT*NCNVC
!
!.......................................................................
!$omp parallel do                &
!$omp     private(j,i,k,landmask,slimsk,zf,kflip,delt,psp,prsl,delp,phil,u1,&
!$omp             v1,t1,q1,clw,ud_mf,dd_mf,dt_mf,cldwrk,vvel,hflx,evap,hpbl,&
!$omp             kcnv,kbot,ktop,u0,v0,t0,q0,clw0,tmp,fract,rn,jcap)
!.......................................................................
        DO J=JTS,JTE  
        DO I=ITS,ITE
!
      !    DO K=1,lm
      !      DQDT(K)=0.
      !      DTDT(K)=0.
      !    ENDDO
!
          RAINCV(I,J)=0.
!
!***  CONVERT TO BMJ LAND MASK (1.0 FOR SEA; 0.0 FOR LAND)
!
          LANDMASK=XLAND(I,J)-1.
          SLIMSK(1) = 1. - LANDMASK
          IF(SICE(I,J) > 0.5) SLIMSK(1) = 2     !! 0-sea; 1-land; 2-ice 
!
!***  FILL 1-D VERTICAL ARRAYS 
!
          ZF(1) = 0.0
          DO K=2,LM+1 
           KFLIP = LM + 1 + 1 -K
           ZF(K) = ZF(K-1) + DZ(I,J,KFLIP)
          ENDDO
           delt = 2.0 * DTCNVC
           PSP(1) = PINT(I,J,lm+1)        ! Surface pressure, Pa
          DO K=1,lm
           kflip = LM + 1 -K
           prsl(1,K)  = pmid(I,J,KFLIP)
           delp(1,K)  = RR(I,J,KFLIP)*g99*DZ(I,J,KFLIP) 
           phil(1,K)  = 0.5*(ZF(K) + ZF(K+1) )*g99              
           u1(1,K)    = (U(I,J  ,KFLIP)+U(I-1,J  ,KFLIP)                       & 
                        +U(I,J-1,KFLIP)+U(I-1,J-1,KFLIP))*0.25
           v1(1,K)    = (V(I,J  ,KFLIP)+V(I-1,J  ,KFLIP)                       &
                        +V(I,J-1,KFLIP)+V(I-1,J-1,KFLIP))*0.25
           t1(1,K)    = T(I,J,KFLIP)
!***  CONVERT FROM MIXING RATIO TO SPECIFIC HUMIDITY
        !   q1(1,K)   = MAX(EPSQ,QV(I,J,K)/(1.+QV(I,J,K))) 
           q1(1,K)    = MAX(EPSQ,WATER(I,J,KFLIP,P_QV)/(1.+WATER(I,J,KFLIP,P_QV))) 
           clw(1,K,1) = WATER(I,J,KFLIP,P_QC)+WATER(I,J,KFLIP,P_QR)                         ! Liquid
           clw(1,K,2) = WATER(I,J,KFLIP,P_QI)+WATER(I,J,KFLIP,P_QS)+WATER(I,J,KFLIP,P_QG)   ! ICE
           ud_mf(1,K) = 0.0
           dd_mf(1,K) = 0.0
           dt_mf(1,K) = 0.0 
           cldwrk(1) = 0.0
           VVEL(1,K)    = omgalf(I,J,KFLIP)*CP*RR(I,J,KFLIP)    !! dp/dt pa/s
          ENDDO
            hflx(1) = SHEAT(I,J)/RR(I,J,LM)/CP            ! W/m2 to K m/s
            evap(1) = LHEAT(I,J)/RR(I,J,LM)/XLV
            hpbl(1) = PBLH(I,J)

           KCNV(1)  = 0     
           KBOT(1)  = KM 
           KTOP(1)  = 1       
           u0 = u1
           v0 = v1
           t0 = t1
           q0 = q1
           clw0 = clw   


!
!-----------------------------------------------------------------------
!***
!***  CALL CONVECTION
!***
      IF(DEEP) THEN                      !! DEEP

       CALL sascnvn(im,ix,km,jcap,delt,delp,prsl,psp,phil,clw,           &
          q1,t1,u1,v1,cldwrk,rn,kbot,ktop,kcnv,slimsk,                     &
          VVEL,ncloud,ud_mf,dd_mf,dt_mf)
!***  CONVECTIVE CLOUD TOP AND BOTTOM FROM THIS CALL
!
       !   CUTOP(I,J) = REAL( lm+1-KTOP(1) )   !BMJ
       !   CUBOT(I,J) = REAL( lm+1-KBOT(1) )   !BMJ
          CUTOP(I,J) = KTOP(1)
          CUBOT(I,J) = KBOT(1)

!***  ALL UNITS IN BMJ SCHEME ARE MKS, THUS CONVERT PRECIP FROM METERS
!***  TO MILLIMETERS PER STEP FOR OUTPUT.
!
          if(lpr .and. i == 20 .and. j == 10)write(0,*)'deep rain=',0.5*rn(1)*1e3/ncnvc 
          
          RAINCV(I,J)=RAINCV(I,J) + 0.5 * rn(1)*1.E3/NCNVC    !! Rain from Deep conv 
!
      ENDIF                             !! DEEP

      IF (SHALLOW) THEN                 !! Shallow
       CALL shalcnv(im,ix,km,jcap,delt,delp,prsl,psp,phil,clw,          &
            q1,t1,u1,v1,rn,kbot,ktop,kcnv,slimsk,                          &
            VVEL,ncloud,hpbl,hflx,evap,ud_mf,dt_mf)

          if(lpr .and.i == 20 .and. j == 10)write(0,*)'shallow rain=',0.5*rn(1)*1.E3/NCNVC 

          RAINCV(I,J)=RAINCV(I,J) + 0.5 * rn(1)*1.E3/NCNVC   !! Rain from shallow conv

      ENDIF                             !! Shallow

!   compute tendency , either shallow or deep happens. only one of them happens
!***  COMPUTE HEATING AND MOISTENING TENDENCIES
!
              DO K=1,LM
                KFLIP = LM+1-K
                DUDT(I,J,KFLIP) = mommix*(u1(1,K)-u0(1,K))/delt
                DVDT(I,J,KFLIP) = mommix*(v1(1,K)-v0(1,K))/delt
              ENDDO

            IF(PRESENT(RTHCUTEN).AND.PRESENT(RQVCUTEN))THEN
              DO K=1,lm
                KFLIP = LM+1-K
                RTHCUTEN(I,J,KFLIP)=(t1(1,K)-t0(1,K))/delt/exner(I,J,KFLIP)
!
!***  CONVERT FROM SPECIFIC HUMIDTY BACK TO MIXING RATIO
!
                RQVCUTEN(I,J,KFLIP)=(q1(1,K)-q0(1,K))/DELT/(1.-q0(1,K))**2
              ENDDO
            ENDIF
            IF(    PRESENT(RQCCUTEN).OR.PRESENT(RQRCUTEN)     &
               .OR.PRESENT(RQICUTEN).OR.PRESENT(RQSCUTEN)     &
               .OR.PRESENT(RQGCUTEN))THEN
                 DO K=1,LM                           !! K
                   KFLIP=LM+1-K
                   tmp   = (CLW(1,K,1)-CLW0(1,K,1))/DELT
              ! IF liquid water=0 at t0, then change is assigned to QC tendency
                   RQCCUTEN(I,J,KFLIP) = tmp             
                    IF(CLW0(1,K,1) .GT. EPSQ ) THEN
                       fract = WATER(I,J,KFLIP,P_QC)/CLW0(1,K,1)
                       RQCCUTEN(I,J,KFLIP) = tmp*fract
                       RQRCUTEN(I,J,KFLIP) = tmp*(1.0-fract)
                           
                          if(abs(rqccuten(i,j,kflip)) .gt. 0.1) then
                            write(0,*)'i=,j=',i,j,kflip
                            write(0,*)'qc=',water(i,j,kflip,p_qc)
                            write(0,*)'qr=',water(i,j,kflip,p_qr)
                            write(0,*)'clw,clw0=',clw(1,k,1),clw0(1,k,1)
                            write(0,*)'rqccuten=',rqccuten(i,j,kflip)
                            write(0,*)'delt=',delt
                            write(0,*)'q1,q0=',q1(1,k),q0(1,k)
                            write(0,*)'t1,t0=',t1(1,k),t0(1,k)
                            write(0,*)'water(i,j,k,:)',water(i,j,kflip,:)
                            stop
                          endif
                    ENDIF

                   tmp   = (CLW(1,K,2)-CLW0(1,K,2))/DELT 
                   RQICUTEN(I,J,KFLIP) = tmp             
                    IF(CLW0(1,K,2) .GT. EPSQ ) THEN
                       fract = WATER(I,J,KFLIP,P_QI)/CLW0(1,K,2)
                       RQICUTEN(I,J,KFLIP) = tmp*fract
                       fract = WATER(I,J,KFLIP,P_QS)/CLW0(1,K,2)
                       RQSCUTEN(I,J,KFLIP) = tmp*fract
                       fract = WATER(I,J,KFLIP,P_QG)/CLW0(1,K,2)
                       RQGCUTEN(I,J,KFLIP) = tmp*fract
                    ENDIF
                   
                 ENDDO                              !! K 
            ENDIF 


!
!-----------------------------------------------------------------------
!
        IF(LPR) THEN
          if(i == 20 .and. j == 10) then
           write(0,*)'u1=,',u1
           write(0,*)'v1=,',v1
           write(0,*)'q1=,',q1
           write(0,*)'W=,',vvel
           write(0,*)'psp=,',psp
           write(0,*)'prsl=,',prsl
           write(0,*)'delp=,',delp
           write(0,*)'phil=,',phil
           write(0,*)'dudt=',dudt(i,j,:)
           write(0,*)'dvdt=',dvdt(i,j,:)
           write(0,*)'dthdt=',rthcuten(i,j,:)
           write(0,*)'dqvdt=',rqvcuten(i,j,:)
           write(0,*)'dqcdt=',rqccuten(i,j,:)
           write(0,*)'dqidt=',rqicuten(i,j,:)
           write(0,*)'dqsdt=',rqscuten(i,j,:)
           write(0,*)'dqgdt=',rqgcuten(i,j,:)
           write(0,*)'max dqvdt,location=', maxval(abs(rqvcuten)),maxloc(abs(rqvcuten))
           write(0,*)'max dqcdt,location=', maxval(abs(rqccuten)),maxloc(abs(rqccuten))
           write(0,*)'max dqrdt,location=', maxval(abs(rqrcuten)),maxloc(abs(rqrcuten))
           write(0,*)'max dqidt,location=', maxval(abs(rqicuten)),maxloc(abs(rqicuten))
           write(0,*)'max dqsdt,location=', maxval(abs(rqscuten)),maxloc(abs(rqscuten))
           write(0,*)'max dqgdt,location=', maxval(abs(rqgcuten)),maxloc(abs(rqgcuten))
            write(0,*)'min water',minval(water)
           write(0,*)'clw0(1,2)=',clw0(1,10,1),clw0(1,10,2)
           write(0,*)'water(p_qc,p_qr)=',water(i,j,lm+1-10,p_qc),water(i,j,lm+1-10,p_qr)
           write(0,*)'water(p_qi,p_qs,p_qg)=',water(i,j,lm+1-10,p_qi),water(i,j,lm+1-10,p_qs),water(i,j,lm+1-10,p_qg)
           write(0,*)'EXNER=',exner(i,j,:)
           write(0,*)'hPBL=',hpbl(1)
           write(0,*)'SICE=',sice(i,j)
           write(0,*)'RAIN=,',raincv(I,J)
           write(0,*)'kbot=,',kbot
           write(0,*)'ktop=,',ktop
          endif           

        ENDIF
        ENDDO
        ENDDO
!.......................................................................
!$omp end parallel do              
!.......................................................................
!
      ENDIF                                               !! end of convection
!
      END SUBROUTINE SASDRV
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
        SUBROUTINE SAS_INIT
          CALL GPVS
        END SUBROUTINE SAS_INIT

      END MODULE MODULE_CU_SAS
!
!-----------------------------------------------------------------------
