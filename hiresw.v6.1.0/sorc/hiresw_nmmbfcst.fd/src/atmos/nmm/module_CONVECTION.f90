!-----------------------------------------------------------------------
!
      MODULE MODULE_CONVECTION
!
!-----------------------------------------------------------------------
!
!***  THE CONVECTION DRIVERS AND PACKAGES
!
!-----------------------------------------------------------------------
!
      USE MODULE_INCLUDE
!
      USE MODULE_CONTROL,ONLY : NMMB_FINALIZE

      USE MODULE_CU_BMJ
      USE MODULE_CU_SAS
!
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      PRIVATE
!
      PUBLIC :: CUCNVC
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!***  THE CONVECTION OPTIONS
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      INTEGER(KIND=KINT),PARAMETER :: KFETASCHEME=1                     &
                                     ,BMJSCHEME=2                       &
                                     ,GDSCHEME=3                        & 
                                     ,SASSCHEME=4 
!
!-----------------------------------------------------------------------
!
      CONTAINS
!
!-----------------------------------------------------------------------
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!-----------------------------------------------------------------------
      SUBROUTINE CUCNVC(NTSD,DT,NCNVC,NRADS,NRADL,MINUTES_HISTORY       &
                       ,ENTRAIN,NEWALL,NEWSWAP,NEWUPUP,NODEEP           &
                       ,FRES,FR,FSL,FSS                                 &
                       ,DYH,RESTRT,HYDRO                                &
                       ,CLDEFI,NUM_WATER                                &
                       ,F_ICE,F_RAIN                                    &
                       ,P_QV,P_QC,P_QR,P_QI,P_QS,P_QG                   &
                       ,F_QV,F_QC,F_QR,F_QI,F_QS,F_QG                   &
                       ,DSG2,SGML2,SG2,PDSG1,PSGML1,PSG1                &
                       ,dxh                                             &
                       ,PT,PD,T,Q,CWM,TCUCN,WATER                       &
                       ,OMGALF,U,V                                      &
                       ,FIS,W0AVG                                       &
                       ,PREC,ACPREC,CUPREC,CUPPT,CPRATE                 &
                       ,CNVBOT,CNVTOP,SM,LPBL                           &
                       ,HTOP,HTOPD,HTOPS                                &
                       ,HBOT,HBOTD,HBOTS                                &
                       ,AVCNVC,ACUTIM                                   &
                       ,RSWIN,RSWOUT                                    &
                       ,CONVECTION,CU_PHYSICS                           &
!!!! added for SAS
                       ,SICE,QWBS,TWBS,PBLH,DUDT_PHY,DVDT_PHY           &
!!!
                       ,A2,A3,A4,CAPPA,CP,ELIV,ELWV,EPSQ,G              &
                       ,P608,PQ0,R_D,TIW                                &
                       ,IDS,IDE,JDS,JDE,LM                              &
                       ,IMS,IME,JMS,JME                                 &
                       ,ITS,ITE,JTS,JTE                                 &
                       ,ITS_B1,ITE_B1,JTS_B1,JTE_B1                     &
                                                    )
!***********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    CUCNVC      CONVECTIVE PRECIPITATION OUTER DRIVER
!   PRGRMMR: BLACK           ORG: W/NP22     DATE: 02-03-21       
!     
! ABSTRACT:
!     CUCVNC DRIVES THE WRF CONVECTION SCHEMES
!     
! PROGRAM HISTORY LOG:
!   02-03-21  BLACK      - ORIGINATOR
!   04-11-18  BLACK      - THREADED
!   06-10-11  BLACK      - BUILT INTO UMO PHYSICS COMPONENT
!   08-08     JANJIC     - Synchronize WATER array and Q.
!   10-10-26  WEIGUO WANG - add GFS SAS convection
!     
! USAGE: CALL CUCNVC FROM PHY_RUN
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM
!$$$  
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
      character(99),intent(in):: &
       convection
!
      logical(kind=klog),intent(in):: &
       hydro,restrt &
      ,entrain,newall,newswap,newupup,nodeep &
      ,f_qv,f_qc,f_qr,f_qi,f_qs,f_qg
!
      integer(kind=kint),intent(in):: &
       cu_physics &
      ,ids,ide,jds,jde,lm &
      ,ims,ime,jms,jme &
      ,its,ite,jts,jte &
      ,its_b1,ite_b1,jts_b1,jte_b1 &
      ,ncnvc,minutes_history &
      ,nrads,nradl,ntsd,num_water &
      ,p_qv,p_qc,p_qr,p_qi,p_qs,p_qg
!
      integer(kind=kint),dimension(ims:ime,jms:jme),intent(in):: &
       lpbl
!
      real(kind=kfpt),intent(in):: &
       a2,a3,a4,cappa,cp,dt,dyh,eliv,elwv,epsq &
      ,fres,fr,fsl,fss,g,p608,pq0,pt,r_d,tiw
!
      real(kind=kfpt),dimension(1:lm),intent(in):: &
       dsg2,pdsg1,psgml1,sgml2
!
      real(kind=kfpt),dimension(1:lm+1),intent(in):: &
       psg1,sg2 
!
      real(kind=kfpt),dimension(jds:jde),intent(in):: &
       dxh
!
      real(kind=kfpt),dimension(ims:ime,jms:jme),intent(in):: &
       fis,pd &
      ,rswin,rswout,sm
!
      real(kind=kfpt),dimension(ims:ime,jms:jme),intent(inout):: &
       acprec,cldefi &
      ,cnvbot,cnvtop &
      ,cuppt,cuprec &
      ,hbot,htop &
      ,hbotd,htopd &
      ,hbots,htops &
      ,prec,cprate &
      ,acutim,avcnvc  !<-- were scalars
!
      real(kind=kfpt),dimension(ims:ime,jms:jme),intent(in):: &
       sice,qwbs,twbs,pblh  !fOR SAS
!
      real(kind=kfpt),dimension(ims:ime,jms:jme,1:lm),intent(in):: &
       f_ice &
      ,f_rain &
      ,omgalf,u,v
!
      real(kind=kfpt),dimension(ims:ime,jms:jme,1:lm),intent(out):: &
       dudt_phy,dvdt_phy
!
      real(kind=kfpt),dimension(ims:ime,jms:jme,1:lm),intent(inout):: &
       q,t &
      ,cwm &
      ,tcucn
!
      real(kind=kfpt),dimension(ims:ime,1:lm+1,jms:jme),intent(inout):: &
       w0avg
!
      real(kind=kfpt),dimension(ims:ime,jms:jme,1:lm,1:num_water) &
                     ,intent(inout):: &
       water
!-----------------------------------------------------------------------
!***  LOCAL VARIABLES
!-----------------------------------------------------------------------
!
      logical(kind=klog):: &
       restart,warm_rain
!
      logical(kind=klog),dimension(ims:ime,jms:jme):: &
       cu_act_flag
!
      integer(kind=kint):: &
       i,j &
      ,k &
      ,mnto &
      ,n,ncubot,ncutop,n_timstps_output
!
      integer(kind=kint),dimension(ims:ime,jms:jme):: &
       KPBL,LBOT,LTOP
!
      real(kind=kfpt):: &
       cf_hi,dqdt,dtcnvc,dtdt,fice,frain,g_inv &
      ,pcpcol,pdsl,plyr,qi,ql,ql_k,qr,qw,rdtcnvc &
      ,tl
!
      REAL(kind=kfpt),DIMENSION(IMS:IME,JMS:JME):: &
       CUBOT,CUTOP,NCA &
      ,RAINC,RAINCV,SFCZ,XLAND
!
      REAL(kind=kfpt),DIMENSION(IMS:IME,JMS:JME,1:LM):: &
       DZ,pmid,exner &
      ,qv,th,rr &
      ,RQCCUTEN,RQRCUTEN &
      ,RQICUTEN,RQSCUTEN &
      ,RQVCUTEN,RTHCUTEN &
      ,RQGCUTEN &
      ,u_phy,v_phy

      REAL(kind=kfpt),DIMENSION(IMS:IME,JMS:JME,1:LM+1):: &
       PINT
!-----------------------------------------------------------------------
!***  For temperature change check only.
!-----------------------------------------------------------------------
!zj      REAL(kind=kfpt) :: DTEMP_CHECK=1.0
      REAL(kind=kfpt) :: TCHANGE
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!***  TRANSLATE THE CONVECTION OPTIONS IN THE CONFIG FILE TO THEIR
!***  ANALOGS IN THE WRF
!-----------------------------------------------------------------------
!
!rv - CU_PHYSICS already determined in init
!rv   SELECT CASE (TRIM(CONVECTION))
!rv     CASE ('bmj')
!rv       CU_PHYSICS=2
!rv     CASE ('kf')
!rv       CU_PHYSICS=1
!rv     CASE ('sas')
!rv       CU_PHYSICS=4
!rv     CASE ('gd')
!rv       CU_PHYSICS=3
!rv     CASE ('none')
!         WRITE(0,*)' User selected to run without parameterized convection.'
!rv     CASE DEFAULT
!rv       WRITE(0,*)' User selected CONVECTION=',TRIM(CONVECTION)
!rv       WRITE(0,*)' Improper selection of Convection scheme in CUCNVC'
!rv       CALL NMMB_FINALIZE
!rv   END SELECT
!
!-----------------------------------------------------------------------
!***  RESET THE HBOT/HTOP CONVECTIVE CLOUD BOTTOM (BASE) AND TOP ARRAYS
!***  USED IN RADIATION.  THEY STORE THE MAXIMUM VERTICAL LIMITS OF 
!***  CONVECTIVE CLOUD BETWEEN RADIATION CALLS.  THESE ARRAYS ARE OUT
!***  OF THE WRF PHYSICS AND THUS THEIR VALUES INCREASE UPWARD.
!***  CUPPT IS THE ACCUMULATED CONVECTIVE PRECIPITATION BETWEEN
!***  RADIATION CALLS.
!-----------------------------------------------------------------------
!
      IF(MOD(NTSD,NRADS)==0.OR.MOD(NTSD,NRADL)==0)THEN
         DO J=JMS,JME
         DO I=IMS,IME
           HTOP(I,J)=0.
           HBOT(I,J)=REAL(LM+1)
           CUPPT(I,J)=0.
         ENDDO
         ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
      IF(MOD(NTSD,NCNVC)/=0.AND.CONVECTION=='bmj')RETURN
      IF(MOD(NTSD,NCNVC)/=0.AND.CONVECTION=='sas')RETURN
!-----------------------------------------------------------------------
!
      RESTART=RESTRT
!
!-----------------------------------------------------------------------
!
      IF(CONVECTION=='kf')THEN
!
        IF(.NOT.RESTART.AND.NTSD==0)THEN
!jaa!zj$omp parallel do                                                       &
!jaa!zj$omp& private(i,j,k)
          DO J=JTS,JTE
          DO K=1,LM+1
          DO I=ITS,ITE
            W0AVG(I,K,J)=0.
          ENDDO
          ENDDO
          ENDDO
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!***  GENERAL PREPARATION 
!-----------------------------------------------------------------------
!
!-- AVCNVC,ACUTIM were scalars but changed to 2D arrays to allow for updates in ESMF
!
      DO J=JTS,JTE
      DO I=ITS,ITE
         AVCNVC(I,J)=AVCNVC(I,J)+1.
         ACUTIM(I,J)=ACUTIM(I,J)+1.
      ENDDO
      ENDDO
!
      DTCNVC=NCNVC*DT
      RDTCNVC=1./DTCNVC
      G_INV=1./G
!
!.......................................................................
!zj$omp parallel do &
!zj$omp& private(j,i,k,pdsl,plyr,ql,tl)
!.......................................................................
      DO J=JTS,JTE
      DO I=ITS,ITE
!
        PDSL=PD(I,J)
        RAINCV(I,J)=0.
        RAINC(I,J)=0.
        PINT(I,J,LM+1)=SG2(LM+1)*PDSL+PSG1(LM+1)
        XLAND(I,J)=SM(I,J)+1.
        NCA(I,J)=0.
        SFCZ(I,J)=FIS(I,J)*G_INV
!
        CUTOP(I,J)=999.
        CUBOT(I,J)=999.
!
!***  LPBL IS THE MODEL LAYER CONTAINING THE PBL TOP
!***  COUNTING DOWNWARD FROM THE TOP OF THE DOMAIN
!***  SO KPBL IS THE SAME LAYER COUNTING UPWARD FROM 
!***  THE GROUND.
!
        KPBL(I,J)=LPBL(I,J)
!
!-----------------------------------------------------------------------
!***  FILL VERTICAL WORKING ARRAYS.
!-----------------------------------------------------------------------
!
        DO K=1,lm
!
          PLYR=SGML2(K)*PDSL+PSGML1(K)

          QL=MAX(Q(I,J,K),EPSQ)
          TL=T(I,J,K)
          RR(I,J,K)=PLYR/(R_D*TL*(.608*ql+1.))
          t(i,j,k)=TL
!
          exner(I,J,K)=(PLYR*1.E-5)**cappa
          th(I,J,K)=TL/exner(i,j,k)
          pint(i,j,k)=sg2(k)*pdsl+psg1(k) !zj
          pmid(I,J,K)=PLYR
!
        ENDDO
      ENDDO
      ENDDO
!.......................................................................
!zj$omp end parallel do
!.......................................................................
!
!-----------------------------------------------------------------------
!***  Compute velocity components at mass points.
!-----------------------------------------------------------------------
!
!.......................................................................
!zj$omp parallel do &
!zj$omp& private(j,i,k)
!.......................................................................
      do k=1,lm
        do j=jms,jme
          do i=ims,ime
            u_phy(i,j,k)=0.
            v_phy(i,j,k)=0.
!
            RTHCUTEN(I,J,K)=0.
            RQVCUTEN(I,J,K)=0.
            RQCCUTEN(I,J,K)=0.
            RQRCUTEN(I,J,K)=0.
            RQICUTEN(I,J,K)=0.
            RQSCUTEN(I,J,K)=0.
            RQGCUTEN(I,J,K)=0.
            dudt_phy(i,j,k)=0.
            dvdt_phy(i,j,k)=0.
          enddo
        enddo
!
        do j=jts_b1,jte_b1
          do i=its_b1,ite_b1
            u_phy(i,j,k)=(u(i,j  ,k)+u(i-1,j  ,k) &
                         +u(i,j-1,k)+u(i-1,j-1,k))*0.25
            v_phy(i,j,k)=(v(i,j  ,k)+v(i-1,j  ,k) &
                         +v(i,j-1,k)+v(i-1,j-1,k))*0.25
          ENDDO
        ENDDO
      ENDDO
!.......................................................................
!zj$omp end parallel do
!.......................................................................
!-----------------------------------------------------------------------
!.......................................................................
!zj$omp parallel do                                                       &
!zj$omp private(i,j,k,pdsl,plyr,ql_k)
!.......................................................................
      DO J=JTS,JTE
        DO I=ITS,ITE
          PDSL=PD(I,J)
          PLYR=PSGML1(LM)+SGML2(LM)*PDSL+PT
          DZ(I,J,LM)=T(I,J,LM)*(.608*Q(I,J,LM)+1.)*R_D &
                    *(PINT(I,J,LM+1)-PINT(I,J,LM)) &
                    /(PLYR*G)
        ENDDO
!
        DO K=LM-1,1,-1
        DO I=ITS,ITE
          QL_K=MAX(Q(I,J,K),EPSQ)
          DZ(I,J,K)=T(I,J,K)*(.608*QL_K+1.)*R_D &
                    *(PINT(I,J,K+1)-PINT(I,J,K)) &
                    /(PMID(I,J,K)*G)
        ENDDO
        ENDDO
!
      ENDDO
!.......................................................................
!zj$omp end parallel do
!.......................................................................
!
!-----------------------------------------------------------------------
!***  SYNCHRONIZE MIXING RATIO IN WATER ARRAY WITH SPECIFIC HUMIDITY.
!-----------------------------------------------------------------------
!
!.......................................................................
!zj$omp parallel do private(i,j,k)
!.......................................................................
      DO K=1,LM
        DO J=JMS,JME
          DO I=IMS,IME
            WATER(I,J,K,P_QV)=Q(I,J,K)/(1.-Q(I,J,K))
            qv(i,j,k)=water(i,j,k,p_qv)
          ENDDO
        ENDDO
      ENDDO
!.......................................................................
!zj$omp end parallel do
!.......................................................................

!write(0,*)'A2,A3,A4,cappa,CP,ELIV,ELWV,EPSQ,p608,PQ0,R_D,TIW' &
!,A2,A3,A4,cappa,CP,ELIV,ELWV,EPSQ,p608,PQ0,R_D,TIW

!
!-----------------------------------------------------------------------
!
!***  SINGLE-COLUMN CONVECTION
!
!-----------------------------------------------------------------------
      IF (CU_PHYSICS /= 0) THEN

          cps_select: SELECT CASE(cu_physics)

            CASE (BMJSCHEME)
 
            call  bmjdrv( &
                         ids,ide,jds,jde &
                        ,ims,ime,jms,jme &
                        ,its,ite,jts,jte,lm &
                        ,its_b1,ite_b1,jts_b1,jte_b1 &
                        ,entrain,newall,newswap,newupup,nodeep &
                        ,a2,a3,a4,cappa,cp,eliv,elwv,epsq,g &
                        ,p608,pq0,r_d,tiw &
                        ,fres,fr,fsl,fss &
                        ,dt,dyh,ntsd,ncnvc &
                        ,raincv,cutop,cubot,dxh,kpbl &
                        ,th,t,qv,u_phy,v_phy,dudt_phy,dvdt_phy &
                        ,pint,pmid,exner &
                        ,cldefi,xland,cu_act_flag &
                      ! optional
                        ,rthcuten,rqvcuten &
                        )
!-----------------------------------------------------------------------
           CASE (SASSCHEME)
           call sasdrv( &
                       ims,ime,jms,jme &
                      ,its,ite,jts,jte,lm &
                      ,dt,ntsd,ncnvc &
                      ,th,t,sice,omgalf,twbs,qwbs,pblh,u_phy,v_phy & !zj orig u&v 
                      ,water,p_qv,p_qc,p_qr,p_qs,p_qi,p_qg,num_water &
                      ,pint,pmid,exner,rr,dz &
                      ,xland,cu_act_flag &
                      ,raincv,cutop,cubot &
                      ,dudt_phy,dvdt_phy &
                      ! optional
                      ,rthcuten, rqvcuten &
                      ,rqccuten, rqrcuten &
                      ,rqicuten, rqscuten &
                      ,rqgcuten  &
                      )
            CASE DEFAULT

              WRITE( 0 , * ) 'The cumulus option does not exist: cu_physics = ', cu_physics

          END SELECT cps_select

      END IF
!
!-----------------------------------------------------------------------
!
!***  CNVTOP/CNVBOT HOLD THE MAXIMUM VERTICAL LIMITS OF CONVECTIVE CLOUD 
!***  BETWEEN HISTORY OUTPUT TIMES.  HBOTS/HTOPS STORE SIMILIAR INFORMATION
!***  FOR SHALLOW (NONPRECIPITATING) CONVECTION, AND HBOTD/HTOPD ARE FOR
!***  DEEP (PRECIPITATING) CONVECTION.  
!
      CF_HI=REAL(MINUTES_HISTORY)/60.
      N_TIMSTPS_OUTPUT=NINT(3600.*CF_HI/DT)
      MNTO=MOD(NTSD,N_TIMSTPS_OUTPUT)
!
      IF(MNTO>0.AND.MNTO<=NCNVC)THEN
        DO J=JTS,JTE
        DO I=ITS,ITE
          CNVBOT(I,J)=REAL(LM+1.)
          CNVTOP(I,J)=0.
          HBOTD(I,J)=REAL(LM+1.)
          HTOPD(I,J)=0.
          HBOTS(I,J)=REAL(LM+1.)
          HTOPS(I,J)=0.
        ENDDO
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!.......................................................................
!zj$omp parallel do                                                       &
!zj$omp& private(j,k,i,dqdt,dtdt,tchange,pcpcol,ncubot,ncutop)
!.......................................................................
!-----------------------------------------------------------------------
      do j=jts_b1,jte_b1
      do i=its_b1,ite_b1
!-----------------------------------------------------------------------
!
!***  UPDATE TEMPERATURE, SPECIFIC HUMIDITY, AND HEATING.
!
        DO K=1,LM
!
!***  RQVCUTEN IN BMJDRV IS THE MIXING RATIO TENDENCY,
!***  SO RETRIEVE DQDT BY CONVERTING TO SPECIFIC HUMIDITY.
!
          DQDT=RQVCUTEN(I,J,K)/(1.+WATER(I,J,K,P_QV))**2
!
!***  RTHCUTEN IN BMJDRV IS DTDT OVER exner.
!
          DTDT=RTHCUTEN(I,J,K)*exner(I,J,K)
          T(I,J,K)=T(I,J,K)+DTDT*DTCNVC
          Q(I,J,K)=Q(I,J,K)+DQDT*DTCNVC
          TCUCN(I,J,K)=TCUCN(I,J,K)+DTDT
          WATER(I,J,K,P_QV)=Q(I,J,K)/(1.-Q(I,J,K))       !Convert to mixing ratio

!!! WANG, 11-2-2010 SAS convection
                IF(CONVECTION=='sas') THEN
                 WATER(I,J,K,P_QC)=WATER(I,J,K,P_QC)+DTCNVC*RQCCUTEN(I,J,K)
                 WATER(I,J,K,P_QR)=WATER(I,J,K,P_QR)+DTCNVC*RQRCUTEN(I,J,K)
                 WATER(I,J,K,P_QI)=WATER(I,J,K,P_QI)+DTCNVC*RQICUTEN(I,J,K)
                 WATER(I,J,K,P_QS)=WATER(I,J,K,P_QS)+DTCNVC*RQSCUTEN(I,J,K)
                 WATER(I,J,K,P_QG)=WATER(I,J,K,P_QG)+DTCNVC*RQGCUTEN(I,J,K)
                ENDIF
!!! wang, 11-2-2010
!
!zj          TCHANGE=DTDT*DTCNVC
!zj          IF(ABS(TCHANGE)>DTEMP_CHECK)THEN
!zj            WRITE(0,*)'BIG T CHANGE BY CONVECTION:',TCHANGE,' at (',I,',',J,',',K,')' 
!zj	  ENDIF
!
        ENDDO

!write(0,*),'t',(rthcuten(i,j,k),k=1,lm)
!write(0,*),'q',(rqvcuten(i,j,k),k=1,lm)
!write(0,*),'u',(dudt_phy(i,j,k),k=1,lm)
!write(0,*),'v',(dvdt_phy(i,j,k),k=1,lm)
!write(0,*),'exner',(exner(i,j,k),k=1,lm)


!
!***  UPDATE PRECIPITATION
!
        PCPCOL=RAINCV(I,J)*1.E-3*NCNVC
        PREC(I,J)=PREC(I,J)+PCPCOL
        ACPREC(I,J)=ACPREC(I,J)+PCPCOL
        CUPREC(I,J)=CUPREC(I,J)+PCPCOL
        CUPPT(I,J)=CUPPT(I,J)+PCPCOL
        CPRATE(I,J)=PCPCOL
!
!***  SAVE CLOUD TOP AND BOTTOM FOR RADIATION (HTOP/HBOT) AND
!***  FOR OUTPUT (CNVTOP/CNVBOT, HTOPS/HBOTS, HTOPD/HBOTD) ARRAYS.
!***  MUST BE TREATED SEPARATELY FROM EACH OTHER.
!
        NCUTOP=NINT(CUTOP(I,J))
        NCUBOT=NINT(CUBOT(I,J))
!
        IF(NCUTOP>1.AND.NCUTOP<LM+1)THEN
          HTOP(I,J)=MAX(CUTOP(I,J),HTOP(I,J))
          CNVTOP(I,J)=MAX(CUTOP(I,J),CNVTOP(I,J))
          IF(PCPCOL>0.)THEN
            HTOPD(I,J)=MAX(CUTOP(I,J),HTOPD(I,J))
          ELSE
            HTOPS(I,J)=MAX(CUTOP(I,J),HTOPS(I,J))
          ENDIF
        ENDIF
        IF(NCUBOT>0.AND.NCUBOT<LM+1)THEN
          HBOT(I,J)=MIN(CUBOT(I,J),HBOT(I,J))
          CNVBOT(I,J)=MIN(CUBOT(I,J),CNVBOT(I,J))
          IF(PCPCOL>0.)THEN
            HBOTD(I,J)=MIN(CUBOT(I,J),HBOTD(I,J))
          ELSE
            HBOTS(I,J)=MIN(CUBOT(I,J),HBOTS(I,J))
          ENDIF
        ENDIF
!
      ENDDO
      ENDDO
!.......................................................................
!zj$omp end parallel do
!.......................................................................
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE CUCNVC
!
!-----------------------------------------------------------------------
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!-----------------------------------------------------------------------
!
      END MODULE MODULE_CONVECTION
!
!-----------------------------------------------------------------------
