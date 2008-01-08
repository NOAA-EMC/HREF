!WRF:MODEL_LAYER:PHYSICS
!
MODULE module_bl_lsm

  REAL, SAVE    :: SCFX(30)

  INTEGER, SAVE :: iseason
 
CONTAINS

!----------------------------------------------------------------
   SUBROUTINE lsm(DZ8W,QV3D,P8W3D,RHO3D,                        &
                  T3D,TH3D,TSK,CHS,                             &
                  HFX,QFX,QGH,GSW,GLW,SMSTAV,SMSTOT,SFCRUNOFF,  &
                  UDRUNOFF,IVGTYP,ISLTYP,VEGFRA,SFCEVP,GRDFLX,  &
                  SFCEXC,ACSNOW,ACSNOM,ALB,TMN,XLAND,XICE,      &
                  th2,t2,q2,SNOWC,chs2,QSFC,CHKLOWQ,RAINBL,     &
                  num_soil_layers,DT,DZS,ITIMESTEP,             &
                  SMOIS,TSLB,SNOW,CANWAT,CPM,ROVCP,             &  !STEMP
                  ids,ide, jds,jde, kds,kde,                    &
                  ims,ime, jms,jme, kms,kme,                    &
                  its,ite, jts,jte, kts,kte                     )
!----------------------------------------------------------------
    IMPLICIT NONE
!----------------------------------------------------------------              
!----------------------------------------------------------------
!-- DZ8W        thickness of layers (m)
!-- T3D         temperature (K)
!-- QV3D        3D water vapor mixing ratio (Kg/Kg)
!-- P3D         3D pressure (Pa)
!-- FLHC        exchange coefficient for heat (m/s)
!-- FLQC        exchange coefficient for moisture (m/s)
!-- PSFC        surface pressure (Pa)
!-- XLAND       land mask (1 for land, 2 for water)
!-- TMN         soil temperature at lower boundary (K)
!-- HFX         upward heat flux at the surface (W/m^2)
!-- QFX         upward moisture flux at the surface (kg/m^2/s)
!-- TSK         surface temperature (K)
!-- GSW         downward short wave flux at ground surface (W/m^2)
!-- GLW         downward long wave flux at ground surface (W/m^2)
!-- CAPG        heat capacity for soil (J/K/m^3)
!-- THC         thermal inertia (Cal/cm/K/s^0.5)
!-- SNOWC       flag indicating snow coverage (1 for snow cover)
!-- EMISS       surface emissivity (between 0 and 1)
!-- DELTSM      time step (second)
!-- ROVCP       R/CP
!-- XLV         latent heat of melting (J/kg)
!-- DTMIN       time step (minute)
!-- IFSNOW      ifsnow=1 for snow-cover effects
!-- SVP1        constant for saturation vapor pressure (kPa)
!-- SVP2        constant for saturation vapor pressure (dimensionless)
!-- SVP3        constant for saturation vapor pressure (K)
!-- SVPT0       constant for saturation vapor pressure (K)
!-- EP1         constant for virtual temperature (R_v/R_d - 1) (dimensionless)
!-- EP2         constant for specific humidity calculation
!               (R_d/R_v) (dimensionless)
!-- KARMAN      Von Karman constant
!-- EOMEG       angular velocity of earth's rotation (rad/s)
!-- STBOLT      Stefan-Boltzmann constant (W/m^2/K^4)
!-- STEM        soil temperature in 5-layer model
!-- ZS          depths of centers of soil layers
!-- DZS         thicknesses of soil layers
!-- num_soil_layers   the number of soil layers
!-- ids         start index for i in domain
!-- ide         end index for i in domain
!-- jds         start index for j in domain
!-- jde         end index for j in domain
!-- kds         start index for k in domain
!-- kde         end index for k in domain
!-- ims         start index for i in memory
!-- ime         end index for i in memory
!-- jms         start index for j in memory
!-- jme         end index for j in memory
!-- kms         start index for k in memory
!-- kme         end index for k in memory
!-- its         start index for i in tile
!-- ite         end index for i in tile
!-- jts         start index for j in tile
!-- jte         end index for j in tile
!-- kts         start index for k in tile
!-- kte         end index for k in tile
!----------------------------------------------------------------
   INTEGER,  INTENT(IN   )   ::     ids,ide, jds,jde, kds,kde,  &
                                    ims,ime, jms,jme, kms,kme,  &
                                    its,ite, jts,jte, kts,kte

   INTEGER, INTENT(IN)       ::     num_soil_layers,ITIMESTEP

!
   REAL,     INTENT(IN   )   ::     DT,ROVCP

   REAL,     DIMENSION( ims:ime , 1:num_soil_layers, jms:jme ), &
             INTENT(INOUT)   ::                          SMOIS, & ! new
						         TSLB     ! TSLB     STEMP

   REAL,     DIMENSION(1:num_soil_layers), INTENT(IN)::DZS

   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(INOUT)    ::                            TSK, & !was TGB (temperature)
					                   HFX, &     
						           QFX, &     
						           QSFC,&     
						          SNOW, & !new
						           ALB, &
                                                         SNOWC, & 
                                                        CANWAT, & ! new
                                                        SMSTAV, &
                                                        SMSTOT, &
                                                     SFCRUNOFF, &
                                                      UDRUNOFF, &
                                                        SFCEVP, &
                                                        GRDFLX, &
                                                        ACSNOW, &
                                                        ACSNOM, &
                                                            Q2, &
                                                           TH2, &
                                                            T2, &
                                                        SFCEXC

   INTEGER, DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(IN   )    ::                         IVGTYP, &
                                                        ISLTYP

   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(IN   )    ::                            TMN, &
                                                         XLAND, &
                                                          XICE, &
                                                        VEGFRA, &
                                                           GSW, &
                                                           GLW     

   REAL,    DIMENSION( ims:ime, kms:kme, jms:jme )            , &
            INTENT(IN   )    ::                           QV3D, &
                                                         p8w3D, &
                                                         rho3D, &
                                                          TH3D, &
                                                           T3D, &
                                                           DZ8W

!
   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(IN   )    ::                         RAINBL

!
   REAL,     DIMENSION( its:ite, jts:jte )                    , &
             INTENT(IN   )               ::               CHS2, &
                                                           CHS, &
                                                           QGH, &
                                                           CPM
   REAL,     DIMENSION( its:ite, jts:jte )                    , &
             INTENT(OUT  )               ::            CHKLOWQ
! LOCAL VARS

   REAL,     DIMENSION( its:ite ) ::                      QV1D, &
                                                           T1D, &
                                                          TH1D, &
                                                          ZA1D, &
                                                         p8w1D, &
                                                         rho1D, &
                                                        PREC1D
                                                                           
   INTEGER ::  I,J

   DO J=jts,jte

      DO i=its,ite
         T1D(i)    = T3D(i,1,j)
         TH1D(i)   = TH3D(i,1,j)
         QV1D(i)   = QV3D(i,1,j)
         P8w1D(i)  = P8w3D(i,1,j)
         ZA1D(i)   = 0.5*DZ8W(i,1,j)
         rho1D(i)  = rho3D(i,1,j)
         PREC1D(i) = RAINBL(i,j)/DT
      ENDDO

!FLHC = SFCEXC
    
      CALL SURFCE(J,ZA1D,QV1D,P8W1D,RHO1D,T1D,TH1D,TSK,CHS(its,j),      &
                  PREC1D,HFX,QFX,QGH(its,j),GSW,GLW,SMSTAV,SMSTOT,SFCRUNOFF,   &
                  UDRUNOFF,IVGTYP,ISLTYP,VEGFRA,SFCEVP,GRDFLX,          &
                  SFCEXC,ACSNOW,ACSNOM,ALB,TMN,XLAND,XICE,              &
                  th2,t2,q2,SNOWC,chs2(its,j),QSFC,CHKLOWQ(its,j),      &
                  num_soil_layers,DT,DZS,ITIMESTEP,                     &
                  SMOIS,TSLB,SNOW,CANWAT,CPM(its,j),ROVCP,              &  !STEMP
                  ims,ime,jms,jme,kms,kme,                              &
                  its,ite,jts,jte,kts,kte                               ) 

   ENDDO

   END SUBROUTINE lsm

!------------------------------------------------------------------------     
   SUBROUTINE SURFCE(J,ZA,QV,P8W,RHO,T,TH,TSK,CHS,PREC,HFX,QFX,         &
                     QGH,GSW,GLW,SMSTAV,SMSTOT,SFCRUNOFF,UDRUNOFF,      &
                     IVGTYP,ISLTYP,VEGFRA,SFCEVP,GRDFLX,                &
                     SFCEXC,ACSNOW,ACSNOM,ALB,TMN,XLAND,XICE,           &
                     th2,t2,q2,SNOWC,chs2,QSFC,CHKLOWQ,                 &
                     num_soil_layers,DT,DZS,ITIMESTEP,                  &
                     SMOIS,TSLB,SNOW,CANWAT,CPM,ROVCP,                  &  !STEMP
                     ims,ime,jms,jme,kms,kme,                           &
                     its,ite,jts,jte,kts,kte                            ) 
!------------------------------------------------------------------------     
      IMPLICIT NONE                                                     
!------------------------------------------------------------------------     
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                .      .    .                                          
! SUBPROGRAM:    SURFCE      CALCULATE SURFACE CONDITIONS               
!   PRGRMMR: F. CHEN         DATE: 97-12-06                             
!                                                                       
! ABSTRACT:                                                             
!   THIS ROUTINE IS THE DRIVER FOR COMPUTATION OF GROUND CONDITIONS     
!   BY USING A LAND SURFACE MODEL (LSM).                                
!                                                                       
! PROGRAM HISTORY LOG:                                                  
!   97-12-06  CHEN - ORIGINATOR                                         
!                                                                       
! REFERENCES:                                                           
!   PAN AND MAHRT (1987) BOUN. LAYER METEOR.                            
!   CHEN ET AL. (1996)  J. GEOPHYS. RES.                                
!   CHEN ET AL. (1997)  BOUN. LAYER METEOR.                             
!   CHEN and Dudhia (2000)  Mon. Wea. Rev. 
!                                                                       
!   SUBPROGRAMS CALLED:                                                 
!     SFLX                                                              
!                                                                       
!     SET LOCAL PARAMETERS.                                             
!----------------------------------------------------------------------
   INTEGER,  INTENT(IN   )   ::           ims,ime, jms,jme, kms,kme,  &
                                          its,ite, jts,jte, kts,kte,  &
                                          J,ITIMESTEP      

   INTEGER , INTENT(IN)      ::           num_soil_layers

   REAL,     INTENT(IN   )   ::           DT,ROVCP

   REAL,     DIMENSION(1:num_soil_layers), INTENT(IN)::DZS

                                                 
   REAL, PARAMETER  :: PQ0=1000.0*0.6112*0.622
   REAL, PARAMETER  :: TRESH=.95E0, A2=17.67,A3=273.15,A4=29.65,      &
                       T0=273.16E0,T1=274.16E0,ROW=1.E3,              &
                       ELWV=2.50E6,ELIV=2.8337E6,ELIW=.3337E6,        &
                       A23M4=A2*(A3-A4), RLIVWV=ELIV/ELWV,            &
                       ROWLIW=ROW*ELIW,ROWLIV=ROW*ELIV

   INTEGER,  PARAMETER  :: NROOT=3
!                                                                       
   REAL,     DIMENSION( ims:ime , 1:num_soil_layers, jms:jme ),       &
             INTENT(INOUT)   ::                          SMOIS,       & ! new
						         TSLB           ! new  !STEMP


   REAL,    DIMENSION( ims:ime, jms:jme )                           , &
            INTENT(INOUT)    ::                                  TSK, & !was TGB (temperature)
						                 HFX, & !new
						                 QFX, & !new
						                 QSFC,& !new
						                SNOW, & !new
						 	         ALB, &
                                                               SNOWC, & 
                                                              CANWAT, & ! new
                                                              SMSTAV, &
                                                              SMSTOT, &
                                                           SFCRUNOFF, &
                                                            UDRUNOFF, &
                                                              SFCEVP, &
                                                              GRDFLX, &
                                                              ACSNOW, &
                                                              ACSNOM

   INTEGER, DIMENSION( ims:ime, jms:jme )                           , &
            INTENT(IN   )    ::                               IVGTYP, &
                                                              ISLTYP

   REAL,    DIMENSION( ims:ime, jms:jme )                           , &
            INTENT(IN   )    ::                                  TMN, &
                                                               XLAND, &
                                                                XICE, &
                                                              VEGFRA, &
                                                                 GSW, &
                                                                 GLW 

   REAL,    DIMENSION( ims:ime, jms:jme )                           , &
            INTENT(INOUT)    ::                                   Q2, &
							         TH2, &
							          T2, &
                                                              SFCEXC

   REAL,    DIMENSION( its:ite )                                    , &
            INTENT(IN   )    ::                                   ZA, &
							          TH, & 
							          QV, & 
							           T, & 
						                 p8w, &
							         rho, &
							         CHS, &
                                                                 QGH, &
							         CPM, &
							        CHS2, &
                                                                PREC    ! one time step in mm
   REAL,    DIMENSION( its:ite )                                    , &
            INTENT(OUT  )    ::                                 CHKLOWQ


   REAL,    DIMENSION( its:ite )   ::                          TGDSA 

! LOCAL VARS

    REAL, DIMENSION(1:num_soil_layers) :: SMOIS1D,STEMP1D

!---------------------------------------------------------------------- 
!***  DECLARATIONS FOR IMPLICIT NONE                                    
    REAL :: FDTLIW,Q2SAT,Z,FK,SOLDN,SFCTMP,SFCTH2,SFCPRS,             &
            PRCP,Q2K,DQSDTK,SATFLG,TBOTK,CHK,VGFRCK,T1K,              &
            CMCK,Q2M,SNODPK,PLFLX,ELFLX,HFLX,GFLX,RNOF1K,             &
            RNOF2K,Q1K,SMELTK,SOILQW,SOILQM,T2K,PRESK,CHFF,STIMESTEP  

    INTEGER :: I,K,NS,ICE,IVGTPK,ISLTPK,NOOUT,NSOIL
!---------------------------------------------------------------------- 
                                                                        
!***********************************************************************
!                         START SURFCE HERE                             
!***                                                                    
!***  SET CONSTANTS CALCULATED HERE FOR CLARITY.                        
!***                                                                    
      FDTLIW=DT/ROWLIW                                              
!      FDTLIV=DT/ROWLIV                                             
!      FDTW=DT/2.5E9                                                
!***                                                                    
!***  SET LSM CONSTANTS AND TIME INDEPENDENT VARIABLES                  
!***  INITIALIZE LSM HISTORICAL VARIABLES                               
!***                                                                    
!-----------------------------------------------------------------------

      NSOIL=num_soil_layers

      IF(ITIMESTEP.EQ.1)THEN                                                 
        DO 50 I=its,ite
!*** SET ZERO-VALUE FOR SOME OUTPUT DIAGNOSTIC ARRAYS                   
          IF((XLAND(I,J)-1.5).GE.0.)THEN                                
! check sea-ice point                                                   
            IF(XICE(I,J).EQ.1.)PRINT*,' sea-ice at water point, I=',I,  &
              'J=',J
!***   Open Water Case                                                  
            SMSTAV(I,J)=1.0                                             
            SMSTOT(I,J)=1.0                                             
            DO NS=1,NSOIL                                               
              SMOIS(I,NS,J)=1.0                                          
              TSLB(I,NS,J)=273.16                                          !STEMP
            ENDDO                                                       
          ELSE                                                          
            IF(XICE(I,J).EQ.1.)THEN                                     
!***        SEA-ICE CASE                                                
              SMSTAV(I,J)=1.0                                           
              SMSTOT(I,J)=1.0                                           
              DO NS=1,NSOIL                                             
                SMOIS(I,NS,J)=1.0                                        
              ENDDO                                                     
            ENDIF                                                       
          ENDIF                                                         
!                                                                       
   50   CONTINUE                                                        
      ENDIF                                                             
!-----------------------------------------------------------------------
      DO 100 I=its,ite                                                    
!       SFCPRS=(A(KL)*PSB(I,J)+PTOP+PP3D(I,J,KL)*0.001)*1.E3          
        SFCPRS=p8w(I)
        Q2SAT=QGH(I)                                                  
        CHKLOWQ(I)=1.
        CHFF=CHS(I)*RHO(I)*CPM(I)
!CHK*RHO*CP                                                             
! TGDSA: potential T
        TGDSA(I)=TSK(I,J)*(1.E5/SFCPRS)**ROVCP 
!***                                                                    
!***  LOADING AND UNLOADING MM5/LSM LAND SOIL VARIABLES                 
!***                                                                    
        IF((XLAND(I,J)-1.5).GE.0.)THEN                                  
!*** Water                                                              
!CC     Q2SAT=PQ0/SFCPRS*EXP(A2*(TGDSA(I)-A3)/(TGDSA(I)-A4))            
          HFX(I,J)=CHFF*(TGDSA(I)-TH(I))                         
          QFX(I,J)=RHO(I)*CHS(I)*(Q2SAT-QV(I))                      
          SFCEVP(I,J)=SFCEVP(I,J)+QFX(I,J)*DT                       
        ELSE                                                            
!*** LAND OR SEA-ICE                                                    
!ATEC          ICE=INT(XICE(I,J)+0.3)                                   
          IF (XICE(I,J) .GT. 0.5) THEN                                  
             ICE=1                                                      
          ELSE                                                          
             ICE=0                                                      
          ENDIF                                                         
          Z=ZA(I)                                                    
          FK=GSW(I,J)+GLW(I,J)                                          
          SOLDN=GSW(I,J)/(1.-ALB(I,J))                                  
          SFCTMP=T(I)                                               
          SFCTH2=SFCTMP+(0.0097545*Z)                                   
          PRCP=PREC(I)                                                  
          Q2K=QV(I)                                                  
!          Q2SAT=PQ0/SFCPRS*EXP(A2*(SFCTMP-A3)/(SFCTMP-A4))              
          DQSDTK=Q2SAT*A23M4/(SFCTMP-A4)**2                             
!***  CHECK FOR SATURATION AT THE LOWEST MODEL LEVEL                    
          IF((Q2K.GE.Q2SAT*TRESH))THEN                                  
            SATFLG=0.                                                   
            CHKLOWQ(I)=0.
          ELSE                                                          
            SATFLG=1.0                                                  
          ENDIF                                                         
          IF(ICE.EQ.0)THEN                                              
            TBOTK=TMN(I,J)                                              
          ELSE                                                          
            TBOTK=271.16                                                
          ENDIF                                                         
          CHK=CHS(I)                                                    
          IVGTPK=IVGTYP(I,J)                                            
          ISLTPK=ISLTYP(I,J)                                            
          VGFRCK=VEGFRA(I,J)/100.                                       
          IF(IVGTPK.EQ.25) VGFRCK=0.0001
          IF(ISLTPK.EQ.14.AND.XICE(I,J).EQ.0.)THEN                      
         PRINT*,' SOIL TYPE FOUND TO BE WATER AT A LAND-POINT'          
         PRINT*,i,j,'RESET SOIL in surfce.F'                      
!           ISLTYP(I,J)=7                                               
            ISLTPK=7                                                    
          ENDIF                                                         
          T1K=TSK(I,J)
          CMCK=CANWAT(I,J)                                                
!*** convert snow depth from mm to meter                                
          SNODPK=SNOW(I,J)*0.001                                        
!                                                                       
          DO 70 NS=1,NSOIL                                              
            SMOIS1D(NS)=SMOIS(I,NS,J)                                       
            STEMP1D(NS)=TSLB(I,NS,J)                                          !STEMP
   70     CONTINUE                                                      
!                                                                       
!        print*,'BF SFLX','ISLTPK',ISLTPK,'IVGTPK=',IVGTPK,'SMOIS1D',&
!              SMOIS1D,'STEMP1',STEMP1D,'VGFRCK',VGFRCK
!-----------------------------------------------------------------------
         CALL SFLX(ICE,SATFLG,DT,Z,NSOIL,NROOT,DZS,FK,SOLDN,SFCPRS,    &
              PRCP,SFCTMP,SFCTH2,Q2K,Q2SAT,DQSDTK,TBOTK,CHK,CHFF,      &
              IVGTPK,ISLTPK,VGFRCK,PLFLX,ELFLX,HFLX,GFLX,RNOF1K,RNOF2K,&
              Q1K,SMELTK,T1K,CMCK,SMOIS1D,STEMP1D,SNODPK,SOILQW,SOILQM)      
!-----------------------------------------------------------------------
!***  DIAGNOSTICS                                                       
!        Convert the water unit into mm                                 
          SFCRUNOFF(I,J)=SFCRUNOFF(I,J)+RNOF1K*DT*1000.0                  
          UDRUNOFF(I,J)=UDRUNOFF(I,J)+RNOF2K*DT*1000.0                  
          SMSTAV(I,J)=SOILQW                                            
          SMSTOT(I,J)=SOILQM*1000.                                      
          SFCEXC(I,J)=CHK                                               
!       IF(SNOB(I,J).GT.0..OR.SICE(I,J).GT.0.)THEN                      
!         QFC1(I,J)=QFC1(I,J)*RLIVWV                                    
!       ENDIF                                                           
          IF(T(I).LE.T0)THEN                                        
            ACSNOW(I,J)=ACSNOW(I,J)+PREC(I)*DT                     
          ENDIF                                                         
          IF(SNOW(I,J).GT.0.)THEN                                       
            ACSNOM(I,J)=ACSNOM(I,J)+SMELTK*1000.                    
!            SNOPCX(I,J)=SNOPCX(I,J)-SMELTK/FDTLIW                       
          ENDIF                                                         
!       POTEVP(I,J)=POTEVP(I,J)+PLFLX*FDTW                              
!       POTFLX(I,J)=POTFLX(I,J)-PLFLX                                   
!***  WRF LOWER BOUNDARY CONDITIONS                                     
          GRDFLX(I,J)=GFLX                                              
          HFX(I,J)=HFLX                                                 
          QFX(I,J)=ELFLX/ELWV                                           
          SFCEVP(I,J)=SFCEVP(I,J)+QFX(I,J)*DT                       
          TSK(I,J)=T1K
          T2K=T1K-HFX(I,J)/(RHO(I)*CPM(I)*CHS2(I))
          TH2(I,J)=T2K*(1.E5/SFCPRS)**ROVCP                                  
          T2(I,J)=T2K
          Q2M=Q1K-QFX(I,J)/(RHO(I)*CHS2(I))                            
          Q2(I,J)=Q2M
!        t2k=th2k/(1.E5/SFCPRS)**ROVCP                                  
!        QS(I,J)=Q1K                                                    
         QSFC(I,J)=Q1K                                                    
!***  UPDATE STATE VARIABLES 
          SNOW(I,J)=SNODPK*1000.0                                       
          CANWAT(I,J)=CMCK                                                
          IF(SNOW(I,J).GT.1.0)THEN                                      
!           ALB(I,J)=0.01*ALBD(IVGTPK,ISEASON)*(1.+SCFX(IVGTPK))            
            SNOWC(I,J)=1.0                                              
          ELSE                                                          
!           ALB(I,J)=0.01*ALBD(IVGTPK,ISEASON)                              
            SNOWC(I,J)=0.0                                              
          ENDIF                                                         
!                                                                       
          DO 80 NS=1,NSOIL                                              
           SMOIS(I,NS,J)=SMOIS1D(NS)                                       
           TSLB(I,NS,J)=STEMP1D(NS)                                        !  STEMP
   80     CONTINUE                                                      
        ENDIF                                                           
#if 0
        NOOUT=0                                                         

        IF((ITIMESTEP.EQ.1.OR.MOD(ITIMESTEP,1).EQ.0)  &
            .AND. I .EQ.29.AND.J.EQ.23) THEN                                             
!         print*, 'GLW',GLW(I,J),'GSW',GSW(I,J)
          print*, 'T2K',T2K,'T1K',T1K,'HFX',HFX(I,J),'RHO',RHO(I),'CPM',CPM(I), &
                'CHS2',CHS2(I),'soil T',STEMP1D,'soil m', SMOIS1D
!          print*,'Q2M',Q2M,'Q1K',Q1K,'QFX',QFX(I,J),'RHO',RHO(I),'CHS2',CHS2(I),'latent',ELFLX
        ENDIF

        IF(NOOUT.EQ.1)GOTO 100                                          
!     write output to 29                                                
        IF(ITIMESTEP.EQ.1.AND.I.EQ.1.AND.J.EQ.1) &
                                                            WRITE (29,*)&
          'itimestep ','   FK     ','  SOLDN   ','  SFCPR   ',          &
          '  SFCTMP  ','    Q2K   ','   TBOTK  ',          &
          '   CHK    ','  ELFLX   ','   HFLX   ','   GFLX   ',          &
          ' RNOF1K   ',' RNOF2K   ','    T1K   ','   CMCK   ',          &
          '  SMCK1   ','  SMCK2   ','  SMCK3   ','  SMCK4   ',          &
          '  STCK1   ','  STCK2   ','  STCK3   ','  STCK4   ',          &
          ' SNODPK   ','      T2   ',                                   &
          '     Q2   ',' SMSTOT   ',' SFCEVP   ', ' RAIN'                        
        IF((ITIMESTEP.EQ.1.OR.MOD(ITIMESTEP,1).EQ.0)  &
            .AND. I .EQ.29.AND.J.EQ.23) THEN                                             
        print *,'outputting at itimestep =', itimestep
          STIMESTEP=FLOAT(itimestep)
          WRITE (29,1029)STIMESTEP,FK,SOLDN,SFCPRS/100.,SFCTMP,1000.*       &
                         Q2K,TBOTK,1000.*CHK,ELFLX,HFLX,GFLX,SFCRUNOFF(I,J)&
                         ,UDRUNOFF(I,J),T1K,CMCK,SMOIS1D,STEMP1D,SNODPK,        &
!                       ,UDRUNOFF(I,J),T1K,CMCK,SMOIS1D(3),SMOIS1D(7),SMOIS1D(11),&
!                        SMOIS1D(14),STEMP1D(3), STEMP1D(7),STEMP1D(11), &
!                        STEMP1D(14), SNODPK,        &
                         T2K,1000.*Q2M,SMSTOT(I,J),SFCEVP(I,J),PRCP
 1029     FORMAT (29F10.4)                                              
!         IF(ITIMESTEP.EQ.0)WRITE (39,*)'   P      ','   T      ',      &
!           '   TH     ','   Q      ','   U      ','   V      ',        &
!           '   QC     '                                                
!         WRITE (39,1039)itimestep
!         DO K=kts,kte
!           WRITE (39,1039)PRESK,TX(I,K),THX(I,K),1000.*QX(I,K),UX(I,K),&
!                          VX(I,K),1000.*QCX(I,K)                       
 1039       FORMAT (7F10.5)                                             
!         ENDDO                                                         
        ENDIF                                                           
!                                                                       
#endif
  100 CONTINUE                                                          
!                                                                       
!-----------------------------------------------------------------------
  END SUBROUTINE SURFCE
!-----------------------------------------------------------------------

      SUBROUTINE CANRES(SOLAR,CH,SFCTMP,Q2,SFCPRS,SMC,ZSOIL,NSOIL,      &
                       SMCWLT,SMCREF,RCMIN,RC,PC,NROOT,Q2SAT,DQSDT2,    &
                       TOPT,RSMAX,RGL,HS)                              
      IMPLICIT NONE                                                     
                                                                        
!       THIS ROUTINE CALCULATES THE CANOPY RESISTANCE WHICH DEPENDS     
!       ON INCOMING SOLAR RADIATION, AIR TEMPERATURE, ATMOSPHERIC       
!       WATER VAPOR PRESSURE DEFICIT AT THE LOWEST MODEL LEVEL, ANS SOIL
!       MOISTURE.                                                       
! ----------------------------------------------------------------------
!	SOURCE:  JARVIS (1976), Jacquemin and Noilhan (1990 BLM)              
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!	INPUT:   SOLAR: incoming solar radiation                              
!                CH: surface exchange coefficient for heat and moisture 
!                SFCTMP: air temperature at 1st level                   
!                Q2:  air humidity at 1st level                         
!                SFCPRS: surface pressure                               
!                SMC: volumetric soil moisture                          
!                ZSOIL: soil depth                                      
!                NSOIL: number of soil layers                           
!                SMCWLT: wilting point                                  
!                SMCREF: reference soil moisture (field capacity)       
!                                                                       
!	OUTPUT:  PC: plant coefficient                                        
!                RC: canopy resistance                                  
! ----------------------------------------------------------------------
                                                                        
      INTEGER NSOIL,NROOT                                         
      REAL      ZSOIL(NSOIL), SMC(NSOIL), PART(NSOIL)                   
      INTEGER DATE, DDTIME                                              
      REAL SIGMA, RD, CP, SLV                                           
      PARAMETER (SIGMA=5.67E-8, RD=287.0, CP=1004.5, SLV=2.5E6)         
! **********************************************************************
!----------------------------------------------------------------------
! DECLARATIONS FOR IMPLICIT NONE                                        
    INTEGER :: K                                                         
    REAL :: RCS,RCT,RCQ,RCSOIL,RC,FF,SOLAR,RGL,RCMIN,RSMAX,TOPT,      &
            SFCTMP,P,SFCPRS,QS,Q2SAT,HS,Q2,GX,SMCWLT,SMCREF,TAIR4,    &
            ST1,SLVCP,RR,CH,DELTA,DQSDT2,PC
!----------------------------------------------------------------------
! ----------------------------------------------------------------------
! FOR LOW CROPS USE THESE VALUES FROM JACQUEMIN AND NOILHAN (90 BLM)    
! ----------------------------------------------------------------------
      RCS=0.0                                                           
      RCT=0.0                                                           
      RCQ=0.0                                                           
      RCSOIL=0.0                                                        
      RC=0.0                                                            
! ----------------------------------------------------------------------
! CONTRIBUTION DUE TO INCOMING SOLAR RADIATION.                         
! ----------------------------------------------------------------------
      FF=0.55*2.0*SOLAR/RGL                                             
      RCS=(FF+RCMIN/RSMAX)/(1.0+FF)                                     
      RCS=AMAX1(RCS,0.0001)                                             
! ----------------------------------------------------------------------
! CONTRIBUTION DUE TO THE TEMPERATURE AT THE FIRST MODEL LEVEL.         
! ----------------------------------------------------------------------
      RCT=1.0-0.0016*((TOPT-SFCTMP)**2.0)                               
      RCT=AMAX1(RCT,0.0001)                                             
! ----------------------------------------------------------------------
! CONTRIBUTION DUE TO VAPOR PRESSURE DEFICIT AT THE FIRST MODEL LEVEL.  
! ----------------------------------------------------------------------
      P=SFCPRS                                                          
      QS=Q2SAT                                                          
! RCQ expression from Ssib                                              
      RCQ=1.0/(1.0+HS*(QS-Q2))                                          
      RCQ=AMAX1(RCQ,0.01)                                               
! ----------------------------------------------------------------------
! CONTRIBUTION DUE TO SOIL MOISTURE AVAILABILITY.                       
! DETERMINE CONTRIBUTION FROM EACH SOIL LAYER, THEN ADD THEM UP.        
! ----------------------------------------------------------------------
      GX=(SMC(1)-SMCWLT)/(SMCREF-SMCWLT)                                
      IF(GX.GT.1.)GX=1.                                                 
      IF(GX.LT.0.)GX=0.                                                 
      PART(1)=(ZSOIL(1)/ZSOIL(NROOT))*GX                                
      DO 10 K=2,NROOT                                                   
        GX=(SMC(K)-SMCWLT)/(SMCREF-SMCWLT)                              
        IF(GX.GT.1.)GX=1.                                               
        IF(GX.LT.0.)GX=0.                                               
        PART(K)=((ZSOIL(K)-ZSOIL(K-1))/ZSOIL(NROOT))*GX                 
   10 CONTINUE                                                          
      DO 20 K=1,NROOT                                                   
        RCSOIL=RCSOIL+PART(K)                                           
   20 CONTINUE                                                          
      RCSOIL=AMAX1(RCSOIL,0.0001)                                       
! ----------------------------------------------------------------------
!         DETERMINE CANOPY RESISTANCE DUE TO ALL FACTORS.               
!         CONVERT CANOPY RESISTANCE (RC) TO PLANT COEFFICIENT (PC).     
! ----------------------------------------------------------------------
      RC=RCMIN/(RCS*RCT*RCQ*RCSOIL)                                     
      TAIR4=SFCTMP**4                                                   
      ST1=(4.*SIGMA*RD)/CP                                              
      SLVCP=SLV/CP                                                      
      RR=ST1*TAIR4/(SFCPRS*CH)+1.0                                      
      DELTA=SLVCP*DQSDT2                                                
      PC=(RR+DELTA)/(RR*(1.+RC*CH)+DELTA)                               

      END SUBROUTINE CANRES
                                                                        

      SUBROUTINE DCOEF ( Z, Z0, T1V, TH2V, SFCSPD, CM, CH, RIB )             
      IMPLICIT NONE                                                     
                                                                        
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C    NAME:  DETERMINE COEFFICIENTS (DCOEF)       VERSION: N/A          
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      REAL A                                                            
      REAL AH                                                           
      REAL AM                                                           
      REAL B1                                                           
      REAL B2                                                           
      REAL CH                                                           
      REAL CM                                                           
      REAL CTP                                                          
      REAL CUP                                                          
      REAL CUS                                                          
      REAL EXMCH                                                        
      REAL G                                                            
      REAL PR                                                           
      REAL RIB                                                          
      REAL SFCSPD                                                       
      REAL TH2V                                                         
      REAL T1V                                                          
      REAL VK                                                           
      REAL Z                                                            
      REAL Z0                                                           
      REAL Z0H                                                          

      DATA B1     / 9.4 /                                               
      DATA B2     / 15.0 /                                              
      DATA CUS    / 7.4 /                                               
      DATA EXMCH  / -1. /                                               
      DATA G      / 9.806 /                                             
!                                                                       
!      DATA PR     / .74 /                                              
!   Set PR=1 the same as in Ek's version for PILPS 1994                 
!                                                                       
      DATA PR     / 1.0 /                                               
      DATA VK     / .4 /                                                
                                                                        
!                                                                       
      SFCSPD=MAX(SFCSPD,0.01)                                           
!   Set Z0H as function of Z0 as in Beljaars and Betts (1992?)          
!                                                                       
      Z0H=Z0/10.                                                        
!                                                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC       
!     CALC A FRICTION VELOCITY FOR USE IN CALCULATING THE DRAG          
!     COEF FOR MOMENTUM AND ONE FOR USE IN CALCULATING THE DRAG         
!     COEF FOR HEAT.                                                    
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC       
      A=VK/ALOG(Z/Z0)                                                   
      AM=A*A                                                            
      AH=A*VK/ALOG(Z/Z0H)                                               
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC       
!     CALC A BULK RICHARDSON NUMBER.  CONSTRAIN ITS VALUE IN THE        
!     STABLE CASE TO A MAXIMUM OF 1.0 TO AVOID CREATING EXCHANGE        
!     COEFFICIENTS THAT APPROACH ZERO.                                  
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC       
      RIB=G*Z*(TH2V-T1V)/(TH2V*SFCSPD*SFCSPD)                           
      RIB=MIN(RIB,1.0)                                                  
      IF(RIB.GE.0.)THEN                                                 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC       
!       IF THE RICHARDSON NUMBER IS .GE. ZERO, THE AIR IS STABLY        
!       STRATIFIED.  CALC THE DRAG COEFFICIENTS USING A METHOD          
!       DEVELOPED BY MAHRT (MONTHLY WEATHER REVIEW, 1987).  THE         
!       SMALLEST ALLOWABLE CM VALUE IS 1.0E-6.                          
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC       
        CM=AM*SFCSPD*EXP(EXMCH*RIB)                                     
        IF(CM.LT.1.0E-6)THEN                                            
          CM=1.0E-6                                                     
          CH=1.0E-6                                                     
        ELSE                                                            
          CH=(CM*AH/AM)/PR                                              
        ENDIF                                                           
      ELSE                                                              
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC       
!       OTHERWISE, THE AIR IS UNSTABLY STRATIFIED AND THE DRAG          
!       COEFFICIENTS WILL BE CALCULATED AS FOLLOWS.                     
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC       
        CUP=CUS*AM*B1*SQRT(-RIB*Z/Z0)                                   
        CTP=CUS*AH*B1*SQRT(-RIB*Z/Z0H)                                  
        CM=(1.0-(B1*RIB)/(1.0+CUP))*AM*SFCSPD                           
        CH=(1.0-(B2*RIB)/(1.0+CTP))*AH*SFCSPD/PR                        
      ENDIF                                                             
                                                                        
      END SUBROUTINE DCOEF

      FUNCTION DEVAP ( ETP1, SMC, ZSOIL, SHDFAC, SMCMAX, B,             &
                      DKSAT, DWSAT, SMCDRY, SMCREF, SMCWLT)            
      IMPLICIT NONE                                                     
                                                                        
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C    NAME:  DIRECT EVAPORATION (DEVAP) FUNCTION  VERSION: N/A          
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      REAL B                                                            
      REAL DEVAP                                                        
      REAL DKSAT                                                        
      REAL DWSAT                                                        
      REAL ETP1                                                         
      REAL FX                                                           
      REAL SHDFAC                                                       
      REAL SMC                                                          
      REAL SMCDRY                                                       
      REAL SMCMAX                                                       
      REAL WCND                                                         
      REAL WDF                                                          
      REAL ZSOIL                                                        
      REAL SMCWLT                                                       
      REAL SMCREF                                                       
                                                                        
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
!     GET SOIL H2O DIFFUSIVITY AND HYDRAULIC CONDUCTIVITY GIVEN SMC     
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
                                                                        
      CALL WDFCND(WDF,WCND,SMC,SMCMAX,B,DKSAT,DWSAT)                    
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
!     FX REPRESENTS SOIL MOISTURE FLUX / ATMOSPHERIC DEMAND             
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
!      FX = ( -2. * WDF * ( SMC - SMCDRY ) / ZSOIL - WCND ) / ETP1      
!*** test of BETA Methode, F. Chen 7/11/96 ***                          
      FX=(SMC-SMCWLT)/(SMCREF-SMCWLT)                                   
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
!     FX > 1 REPRESENTS DEMAND CONTROL                                  
!     FX < 1 REPRESENTS FLUX CONTROL                                    
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
      FX=MAX(MIN(FX,1.),0.)                                             
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
!     ALLOW FOR THE DIRECT-EVAP-REDUCING EFFECT OF SHADE                
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
      DEVAP=FX*(1.0-SHDFAC)*ETP1                                        
                                                                        
      END FUNCTION DEVAP

      SUBROUTINE HRT ( RHSTS, STC, SMC, SMCMAX, NSOIL, ZSOIL, YY, ZZ1,  &
                      TBOT, B, F1, DF1, AI, BI, CI)                               
      IMPLICIT NONE
                                                                  
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C    PURPOSE:  TO CALCULATE THE RIGHT HAND SIDE OF THE TIME TENDENCY   
!C    =======   TERM OF THE SOIL THERMAL DIFFUSION EQUATION.  ALSO TO   
!C              COMPUTE ( PREPARE ) THE MATRIX COEFFICIENTS FOR THE     
!C              TRI-DIAGONAL MATRIX OF THE IMPLICIT TIME SCHEME.        
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INTEGER NSOIL                                                     
      REAL AI    (NSOIL)                                              
      REAL BI    (NSOIL)                                              
      REAL CAIR                                                         
      REAL CH2O                                                         
      REAL CI    (NSOIL)                                              
      REAL CSOIL                                                        
      REAL DDZ                                                          
      REAL DDZ2                                                         
      REAL DENOM                                                        
      REAL DFMAX1                                                       
      REAL DF1                                                          
      REAL DFMAXN                                                       
      REAL DTSDZ                                                        
      REAL DTSDZ2                                                       
      REAL F1                                                           
      REAL HCPCT                                                        
      INTEGER K                                                         
      REAL MXSMC                                                        
      REAL RHSTS ( NSOIL )                                              
      REAL S                                                            
      REAL SMC   ( NSOIL )                                              
      REAL SMCMAX                                                       
      REAL STC   ( NSOIL )                                              
      REAL TBOT                                                         
      REAL YY                                                           
      REAL ZBOT                                                         
      REAL ZSOIL ( NSOIL )                                              
      REAL ZZ1                                                          

!----------------------------------------------------------------------
!***  DECLARATIONS FOR IMPLICIT NONE
    REAL :: B
!----------------------------------------------------------------------

!      PARAMETER(CAIR=1004.0,CH2O=4.2E6,CSOIL=1.26E6,ZBOT=-3.0)         
      PARAMETER(CAIR=1004.0,CH2O=4.2E6, CSOIL=2.0E6,ZBOT=-3.0)          
                                                                        
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
!     RETRIEVE THE THERMAL DIFFUSIVITY FOR THE WETTER OF THE TOP SOIL   
!     LAYER OR THE NEXT LOWER SOIL LAYER                                
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
                                                                        
      MXSMC=MAX(SMC(1),SMC(2))                                          
      CALL TDFCND(DFMAX1,MXSMC,B,F1)                                    
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
!     CALC THE HEAT CAPACITY OF THE TOP SOIL LAYER                      
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
      HCPCT=SMC(1)*CH2O+(1.0-SMCMAX)*CSOIL+(SMCMAX-SMC(1))*CAIR         
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
!     CALC THE MATRIX COEFFICIENTS AI, BI, AND CI FOR THE TOP LAYER     
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
      DDZ=1.0/(-0.5*ZSOIL(2))                                           
      AI(1)=0.0                                                         
      CI(1)=(DFMAX1*DDZ)/(ZSOIL(1)*HCPCT)                               
      BI(1)=-CI(1)+DF1/(0.5*ZSOIL(1)*ZSOIL(1)*HCPCT*ZZ1)                
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
!     CALC THE VERTICAL SOIL TEMP GRADIENT BTWN THE TOP AND 2ND SOIL    
!     LAYERS.  RECALC/ADJUST THE SOIL HEAT FLUX.  USE THE GRADIENT      
!     AND FLUX TO CALC RHSTS FOR THE TOP SOIL LAYER.                    
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
      DTSDZ=(STC(1)-STC(2))/(-0.5*ZSOIL(2))                             
      S=DF1*(STC(1)-YY)/(0.5*ZSOIL(1)*ZZ1)                              
      RHSTS(1)=(DFMAX1*DTSDZ-S)/(ZSOIL(1)*HCPCT)                        
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
!     INITIALIZE DDZ2                                                   
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
      DDZ2=0.0                                                          
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
!     LOOP THRU THE REMAINING SOIL LAYERS, REPEATING THE ABOVE PROCESS  
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
      DO 40 K=2,NSOIL                                                   
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
!       CALC THIS SOIL LAYER'S HEAT CAPACITY                            
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
        HCPCT=SMC(K)*CH2O+(1.0-SMCMAX)*CSOIL+(SMCMAX-SMC(K))*CAIR       
        IF(K.NE.NSOIL)THEN                                              
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
!         RETRIEVE THE THERMAL DIFFUSIVITY FOR THE WETTER OF THE        
!         CURRENT OR THE NEXT LOWER SOIL LAYER                          
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
          MXSMC=MAX(SMC(K),SMC(K+1))                                    
          CALL TDFCND(DFMAXN,MXSMC,B,F1)                                
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
!         CALC THE VERTICAL SOIL TEMP GRADIENT THRU THIS LAYER.         
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
          DENOM=0.5*(ZSOIL(K-1)-ZSOIL(K+1))                             
          DTSDZ2=(STC(K)-STC(K+1))/DENOM                                
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
!         CALC THE MATRIX COEF, CI, AFTER CALC'NG ITS PARTIAL PRODUCT   
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
          DDZ2=2./(ZSOIL(K-1)-ZSOIL(K+1))                               
          CI(K)=-DFMAXN*DDZ2/((ZSOIL(K-1)-ZSOIL(K))*HCPCT)              
        ELSE                                                            
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
!         RETRIEVE THE THERMAL DIFFUSIVITY FOR THE LOWEST SOIL LAYER    
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
          CALL TDFCND(DFMAXN,SMC(K),B,F1)                               
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
!         CALC THE VERTICAL SOIL TEMP GRADIENT THRU THE LOWEST LAYER    
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
          DTSDZ2=(STC(K)-TBOT)/(.5*(ZSOIL(K-1)+ZSOIL(K))-ZBOT)          
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
!         SET MATRIX COEF, CI TO ZERO                                   
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
          CI(K)=0.                                                      
        ENDIF                                                           
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
!       CALC RHSTS FOR THIS LAYER AFTER CALC'NG A PARTIAL PRODUCT       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
        DENOM=(ZSOIL(K)-ZSOIL(K-1))*HCPCT                               
        RHSTS(K)=(DFMAXN*DTSDZ2-DFMAX1*DTSDZ)/DENOM                     
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
!       CALC MATRIX COEFS, AI, AND BI FOR THIS LAYER.                   
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
        AI(K)=-DFMAX1*DDZ/((ZSOIL(K-1)-ZSOIL(K))*HCPCT)                 
        BI(K)=-(AI(K)+CI(K))                                            
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
!       RESET VALUES OF DFMAX1, DTSDZ, AND DDZ FOR LOOP TO NEXT SOIL LYR
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
        DFMAX1=DFMAXN                                                   
        DTSDZ=DTSDZ2                                                    
        DDZ=DDZ2                                                        
   40 CONTINUE                                                          
      END SUBROUTINE HRT
                                                                        

      SUBROUTINE HRTICE (RHSTS,STC,SMC,SMCMAX,NSOIL,ZSOIL,YY,ZZ1,       &
                      TBOT, B, F1, DF1, AI,BI,CI)                               
      IMPLICIT NONE                                                     
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C    PURPOSE:  TO CALCULATE THE RIGHT HAND SIDE OF THE TIME TENDENCY   
!C    =======   TERM OF THE SOIL THERMAL DIFFUSION EQUATION.  ALSO TO   
!C              COMPUTE ( PREPARE ) THE MATRIX COEFFICIENTS FOR THE     
!C              TRI-DIAGONAL MATRIX OF THE IMPLICIT TIME SCHEME.        
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INTEGER NSOIL                                                     
      REAL AI    ( NSOIL )                                              
      REAL B                                                            
      REAL BI    ( NSOIL )                                              
      REAL CAIR                                                         
      REAL CH2O                                                         
      REAL CI    ( NSOIL )                                              
      REAL CSOIL                                                        
      REAL DDZ                                                          
      REAL DDZ2                                                         
      REAL DENOM                                                        
      REAL DFMAX1                                                       
      REAL DF1                                                          
      REAL DFMAXN                                                       
      REAL DTSDZ                                                        
      REAL DTSDZ2                                                       
      REAL F1                                                           
      REAL HCPCT                                                        
      INTEGER K                                                         
      REAL MXSMC                                                        
      REAL RHSTS ( NSOIL )                                              
      REAL S                                                            
      REAL SMC   ( NSOIL )                                              
      REAL SMCMAX                                                       
      REAL STC   ( NSOIL )                                              
      REAL TBOT                                                         
      REAL YY                                                           
      REAL ZBOT                                                         
      REAL ZSOIL ( NSOIL )                                              
      REAL ZZ1                                                          

      PARAMETER(CAIR=1004.0,CH2O=4.2E6,CSOIL=1.26E6)                   
                                                                        
      ZBOT=ZSOIL(NSOIL)                                                 
      DFMAX1=2.2                                                        
      HCPCT=1880.0*917.0                                                
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
!     CALC THE MATRIX COEFFICIENTS AI, BI, AND CI FOR THE TOP LAYER     
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
      DDZ=1.0/(-0.5*ZSOIL(2))                                           
      AI(1)=0.0                                                         
      CI(1)=(DFMAX1*DDZ)/(ZSOIL(1)*HCPCT)                               
      BI(1)=-CI(1)+DF1/(0.5*ZSOIL(1)*ZSOIL(1)*HCPCT*ZZ1)                
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
!     CALC THE VERTICAL SOIL TEMP GRADIENT BTWN THE TOP AND 2ND SOIL    
!     LAYERS.  RECALC/ADJUST THE SOIL HEAT FLUX.  USE THE GRADIENT      
!     AND FLUX TO CALC RHSTS FOR THE TOP SOIL LAYER.                    
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
      DTSDZ=(STC(1)-STC(2))/(-0.5*ZSOIL(2))                             
      S=DF1*(STC(1)-YY)/(0.5*ZSOIL(1)*ZZ1)                              
      RHSTS(1)=(DFMAX1*DTSDZ-S)/(ZSOIL(1)*HCPCT)                        
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
!     INITIALIZE DDZ2                                                   
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
      DDZ2=0.0                                                          
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
!     LOOP THRU THE REMAINING SOIL LAYERS, REPEATING THE ABOVE PROCESS  
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
      DO 40 K=2,NSOIL                                                   
        IF(K.NE.NSOIL)THEN                                              
          DFMAXN=2.2                                                    
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
!         CALC THE VERTICAL SOIL TEMP GRADIENT THRU THIS LAYER.         
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
          DENOM=0.5*(ZSOIL(K-1)-ZSOIL(K+1))                             
          DTSDZ2=(STC(K)-STC(K+1))/DENOM                                
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
!         CALC THE MATRIX COEF, CI, AFTER CALC'NG ITS PARTIAL PRODUCT   
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
          DDZ2=2./(ZSOIL(K-1)-ZSOIL(K+1))                               
          CI(K)=-DFMAXN*DDZ2/((ZSOIL(K-1)-ZSOIL(K))*HCPCT)              
        ELSE                                                            
          DFMAXN=2.2                                                    
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
!         CALC THE VERTICAL SOIL TEMP GRADIENT THRU THE LOWEST LAYER    
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
          DTSDZ2=(STC(K)-TBOT)/(.5*(ZSOIL(K-1)+ZSOIL(K))-ZBOT)          
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
!         SET MATRIX COEF, CI TO ZERO                                   
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
          CI(K)=0.                                                      
        ENDIF                                                           
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
!       CALC RHSTS FOR THIS LAYER AFTER CALC'NG A PARTIAL PRODUCT       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
        DENOM=(ZSOIL(K)-ZSOIL(K-1))*HCPCT                               
        RHSTS(K)=(DFMAXN*DTSDZ2-DFMAX1*DTSDZ)/DENOM                     
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
!       CALC MATRIX COEFS, AI, AND BI FOR THIS LAYER.                   
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
        AI(K)=-DFMAX1*DDZ/((ZSOIL(K-1)-ZSOIL(K))*HCPCT)                 
        BI(K)=-(AI(K)+CI(K))                                            
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
!       RESET VALUES OF DFMAX1, DTSDZ, AND DDZ FOR LOOP TO NEXT SOIL LYR
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
        DFMAX1=DFMAXN                                                   
        DTSDZ=DTSDZ2                                                    
        DDZ=DDZ2                                                        
   40 CONTINUE                                                          
      END SUBROUTINE HRTICE
                                                                        
      SUBROUTINE HSTEP ( STC, RHSTS, DT, NSOIL, AI, BI, CI)                        
      IMPLICIT NONE                                                     
                                                                        
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C    PURPOSE:  TO CALCULATE/UPDATE THE SOIL TEMPERATURE FIELD.         
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INTEGER K                                                         
      INTEGER NSOIL                                                     
                                                                        
      REAL AI    ( NSOIL )                                              
      REAL BI    ( NSOIL )                                              
      REAL CI    ( NSOIL )                                              
      REAL DT                                                           
      REAL RHSTS ( NSOIL )                                              
      REAL STC   ( NSOIL )                                              

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
!     CREATE FINITE DIFFERENCE VALUES FOR USE IN ROSR12 ROUTINE         
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
                                                                        
      DO 10 K=1,NSOIL                                                   
        RHSTS(K)=RHSTS(K)*DT                                            
        AI(K)=AI(K)*DT                                                  
        BI(K)=1.+BI(K)*DT                                               
        CI(K)=CI(K)*DT                                                  
   10 CONTINUE                                                          
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
!     SOLVE THE TRI-DIAGONAL MATRIX EQUATION                            
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
      CALL ROSR12(CI,AI,BI,CI,RHSTS,RHSTS,NSOIL)                        
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
!     CALC/UPDATE THE SOIL TEMPS USING MATRIX SOLUTION                  
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
      DO 20 K=1,NSOIL                                                   
        STC(K)=STC(K)+CI(K)                                             
   20 CONTINUE                                                          
                                                                        
      END SUBROUTINE HSTEP

      SUBROUTINE NOPAC ( ETP, ETA, PRCP, SMC, SMCMAX, SMCWLT,           &
                        SMCREF,SMCDRY,CMC,CMCMAX, NSOIL, DT, SHDFAC,    &
                        Q1, Q2, T1, SFCTMP, T24, TH2, F, F1, S, STC,    &
                        EPSCA, B, PC, RCH, RR,  CFACTR, ZSOIL, DKSAT,   &
                        DWSAT, TBOT, RUNOFF1, RUNOFF2,RUNOFF3, EDIR1,   &
                        EC1, ETT1, NROOT, ICE,FLX1,FLX3)                         
      IMPLICIT NONE                                                     
                                                                        
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C    PURPOSE:  TO CALCULATE SOIL MOISTURE AND HEAT FLUX VALUES AND UPDA
!C    =======   SOIL MOISTURE CONTENT AND SOIL HEAT CONTENT VALUES FOR  
!C              THE CASE WHEN NO SNOW PACK IS PRESENT.                  
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      INTEGER NSOIL                                                     
                                                                        
      REAL B                                                            
      REAL BETA                                                         
      REAL CFACTR                                                       
      REAL CMC                                                          
      REAL CMCMAX                                                       
      REAL CP                                                           
      REAL DEW                                                          
      REAL DF1                                                          
      REAL DKSAT                                                        
      REAL DT                                                           
      REAL DWSAT                                                        
      REAL EPSCA                                                        
      REAL ETA                                                          
      REAL ETA1                                                         
      REAL ETP                                                          
      REAL ETP1                                                         
      REAL F                                                            
      REAL F1                                                           
      REAL FLX1                                                         
      REAL FLX3                                                         
      REAL PC                                                           
      REAL PRCP                                                         
      REAL PRCP1                                                        
      REAL Q2                                                           
      REAL RCH                                                          
      REAL RR                                                           
      REAL S                                                            
      REAL SFCTMP                                                       
      REAL SHDFAC                                                       
      REAL SIGMA                                                        
      REAL SMC   ( NSOIL )                                              
      REAL SMCDRY                                                       
      REAL SMCMAX                                                       
      REAL SMCREF                                                       
      REAL SMCWLT                                                       
      REAL STC   ( NSOIL )                                              
      REAL T1                                                           
      REAL T24                                                          
      REAL TBOT                                                         
      REAL TH2                                                          
      REAL YY                                                           
      REAL YYNUM                                                        
      REAL ZSOIL ( NSOIL )                                              
      REAL ZZ1                                                          
      REAL RUNOFF1,RUNOFF2,RUNOFF3,EDIR1,EC1,ETT1,Q1                    
      INTEGER NROOT,ICE                                                 
!                                                                       
      PARAMETER(CP=1004.5, SIGMA=5.67E-8)                               
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!     EXECUTABLE CODE BEGINS HERE.....                                  
!     CONVERT ETP FROM KG M-2 S-1 TO MS-1 AND INITIALIZE DEW.           
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
                                                                        
      PRCP1=PRCP*0.001                                                  
      ETP1=ETP*0.001                                                    
      DEW=0.0                                                           
      IF(ETP.GT.0.0)THEN                                                
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!       CONVERT PRCP FROM  KG M-2 S-1  TO  M S-1                        
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
        CALL SMFLX(ETA1,SMC,NSOIL,CMC,ETP1,DT,PRCP1,ZSOIL,SMCMAX,B,PC,  &
            SMCWLT,DKSAT,DWSAT,SMCREF,SHDFAC,CMCMAX,SMCDRY,CFACTR,      &
            RUNOFF1,RUNOFF2,RUNOFF3,EDIR1,EC1,ETT1,SFCTMP,Q2,NROOT)     
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!       CONVERT MODELED EVAPOTRANSPIRATION FM  M S-1  TO  KG M-2 S-1    
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
        ETA=ETA1*1000.0                                                 
      ELSE                                                              
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!       IF ETP < 0, ASSUME DEW FORMS (TRANSFORM ETP1 INTO DEW           
!       AND REINITIALIZE ETP1 TO ZERO)                                  
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
        DEW=-ETP1                                                       
        ETP1=0.0                                                        
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!       CONVERT PRCP FROM  KG M-2 S-1  TO  M S-1  AND ADD DEW AMT       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
        PRCP1=PRCP1+DEW                                                 
!                                                                       
        CALL SMFLX(ETA1,SMC,NSOIL,CMC,ETP1,DT,PRCP1,ZSOIL,SMCMAX,B,PC,  &
             SMCWLT,DKSAT,DWSAT,SMCREF,SHDFAC,CMCMAX,SMCDRY,CFACTR,     &
             RUNOFF1,RUNOFF2,RUNOFF3,EDIR1,EC1,ETT1,SFCTMP,Q2,NROOT)    
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!       CONVERT MODELED EVAPOTRANSPIRATION FM  M S-1  TO  KG M-2 S-1    
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
        ETA=ETA1*1000.0                                                 
      ENDIF                                                             
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!     BASED ON ETP AND E VALUES, DETERMINE BETA                         
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
      IF(ETP.LE.0.0)THEN                                                
        BETA=0.0                                                        
        IF(ETP.LT.0.0)THEN                                              
          BETA=1.0                                                      
          ETA=ETP                                                       
        ENDIF                                                           
      ELSE                                                              
        BETA=ETA/ETP                                                    
      ENDIF                                                             
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!     CALC MIXING RATIO AT GRND LEVEL (SKIN)                            
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
      Q1=Q2+ETA*CP/RCH                                                  
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!     GET SOIL THERMAL DIFFUXIVITY/CONDUCTIVITY FOR TOP SOIL LYR,       
!     CALC ADJUSTED TOP LYR SOIL TEMP AND ADJUSTED SOIL FLUX, THEN      
!     CALL SHFLX TO COMPUTE/UPDATE SOIL HEAT FLUX AND SOIL TEMPS.       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
      CALL TDFCND(DF1,SMC(1),B,F1)                                      
      YYNUM=F-SIGMA*T24                                                 
      YY=SFCTMP+(YYNUM/RCH+TH2-SFCTMP-BETA*EPSCA)/RR                    
      ZZ1=DF1/(-0.5*ZSOIL(1)*RCH*RR)+1                                  
      CALL SHFLX(S,STC,SMC,SMCMAX,NSOIL,T1,DT,YY,ZZ1,ZSOIL,TBOT,B,F1,   &
                 DF1,ICE)                                                        
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!     SET FLX1, AND FLX3 TO ZERO SINCE THEY ARE NOT USED.  FLX2         
!     WAS SIMILARLY INITIALIZED IN THE PENMAN ROUTINE.                  
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
      FLX1=0.0                                                          
      FLX3=0.0                                                          
      END  SUBROUTINE NOPAC
                                                                        
      SUBROUTINE PENMAN(SFCTMP,SFCPRS,CH,T2V,TH2,PRCP,F,T24,S,Q2,       &
         Q2SAT,ETP,RCH,EPSCA,RR,SNOWNG,FRZGRA,DQSDT2,FLX2)    
      IMPLICIT NONE                                                     
                                                                        
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C    PURPOSE:  TO CALCULATE POTENTIAL EVAPORATION FOR THE CURRENT POINT
!C    =======   VARIOUS PARTIAL SUMS/PRODUCTS ARE ALSO CALCULATED AND   
!C              PASSED BACK TO THE CALLING ROUTINE FOR LATER USE.       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      LOGICAL SNOWNG                                                    
      LOGICAL FRZGRA                                                    
                                                                        
      REAL A                                                            
      REAL CH                                                           
      REAL CP                                                           
      REAL CPH2O                                                        
      REAL CPICE                                                        
      REAL DELTA                                                        
      REAL DQSDT2                                                       
      REAL ELCP                                                         
      REAL EPSCA                                                        
      REAL ETP                                                          
      REAL F                                                            
      REAL FLX2                                                         
      REAL FNET                                                         
      REAL LSUBC                                                        
      REAL LSUBF                                                        
      REAL PRCP                                                         
      REAL Q2                                                           
      REAL Q2SAT                                                        
      REAL R                                                            
      REAL RAD                                                          
      REAL RCH                                                          
      REAL RHO                                                          
      REAL RR                                                           
      REAL S                                                            
      REAL SFCPRS                                                       
      REAL SFCTMP                                                       
      REAL SIGMA                                                        
      REAL T24                                                          
      REAL T2V                                                          
      REAL TH2                                                          
      REAL TT,T                                                         

      PARAMETER(CP=1004.6,CPH2O=4.218E+3,CPICE=2.106E+3,R=287.04,       &
         ELCP=2.4888E+3,LSUBF=3.335E+5,LSUBC=2.5E+6,SIGMA=5.67E-8)      
                                                                        
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
!     EXECUTABLE CODE BEGINS HERE...DEFINE FUNCTION.                    
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
                                                                        
      TT(T) = T * T                                                     
      FLX2=0.0                                                          
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
!     PREPARE PARTIAL QUANTITIES FOR PENMAN EQUATION.                   
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
      DELTA=ELCP*DQSDT2                                                 
      T24=TT(SFCTMP)*TT(SFCTMP)                                         
      RR=T24*6.48E-8/(SFCPRS*CH)+1.0                                    
      RHO=SFCPRS/(R*T2V)                                                
      RCH=RHO*CP*CH                                                     
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
!     ADJUST THE PARTIAL SUMS / PRODUCTS WITH THE LATENT HEAT           
!     EFFECTS CAUSED BY FALLING PRECIPITATION.                          
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
      IF(.NOT.SNOWNG)THEN                                               
        IF(PRCP.GT.0.0)RR=RR+CPH2O*PRCP/RCH                             
      ELSE                                                              
        RR=RR+CPICE*PRCP/RCH                                            
      ENDIF                                                             
      FNET=F-SIGMA*T24-S                                                
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
!     INCLUDE THE LATENT HEAT EFFECTS OF FRZNG RAIN CONVERTING TO       
!     ICE ON IMPACT IN THE CALCULATION OF FLX2 AND FNET.                
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
      IF(FRZGRA)THEN                                                    
        FLX2=-LSUBF*PRCP                                                
        FNET=FNET-FLX2                                                  
      ENDIF                                                             
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
!     FINISH PENMAN EQUATION CALCULATIONS.                              
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
      RAD=FNET/RCH+TH2-SFCTMP                                           
      A=ELCP*(Q2SAT-Q2)                                                 
      EPSCA=(A*RR+RAD*DELTA)/(DELTA+RR)                                 
      ETP=EPSCA*RCH/LSUBC                                               
      END SUBROUTINE PENMAN
                                                                        
! ----------------------------------------------------------------------

       SUBROUTINE  PRMSOI(SOLTYP,SBB,SDRYSMC,SF11,SMAXSMC,SREFSMC,      &
                          SSATPSI,SSATDK,SSATDW,SWLTSMC)                
       IMPLICIT NONE

!                                                                       
!    Set-up soil Parameters for given soil type	                        
!                                                                       
!      Input:                                                           
!           SOLTYP: Soil type                                           
!     Output:                                                           
!                                                                       
!      Soil parameters:                                                 
!       SMAXSMC: MAX soil moisture content (porosity)                   
!       SREFSMC: Reference soil moisture  (field capacity)              
!       SWLTSMC: Wilting point soil moisture                            
!       SDRYSMC: Air dry soil moist content limits                      
!       SSATPSI: SAT (saturation) soil potential                        
!        SSATDK: SAT soil conductivity                                  
!           SBB: B parameter                                            
!        SSATDW: SAT soil diffusivity                                   
!          SF11: Soil thermal diffusivity/conductivity coef.            
!                                                                       
!  Modified by F. Chen (12/22/97)  to use the STATSGO soil map   
!  Modified By F. Chen (01/22/00)  to include PLaya, Lava, and White San
! **********************************************************************
!                                                                       
!     SOIL TYPES    STATSGO (Miller ??, 199?)  Cosby et al (1984)       
!             1          SAND                  SAND                     
!             2          LOAMY SAND            LOAMY SAND               
!             3          SANDY LOAM            SANDY LOAM               
!             4          SILT LOAM             SILTY LOAM               
!             5          SILT                  SILTY LOAM               
!             6          LOAM                  LOAM                     
!             7          SANDY CLAY LOAM       SANDY CLAY LOAM          
!             8          SILTY CLAY LOAM       SILTY CLAY LOAM          
!             9          CLAY LOAM             CLAY LOAM                
!            10          SANDY CLAY            SANDY CLAY               
!            11          SILTY CLAY            SILTY CLAY               
!            12          CLAY                  LIGHT CLAY               
!            13          ORGANIC MATERIALS     LOAM                     
!            14          WATER                                          
!            15          BEDROCK                                        
!            16          OTHER (land-ice)                               
!                        the value of this class is the same as in class
!            17          Playa 
!            18          Lava
!            19          White Sand
!                                                                       
!***********************************************************************
!                                                                       
        INTEGER SOLTYP                                                  
                                                                        
        REAL BB(19),DRYSMC(19),F11(19),MAXSMC(19),REFSMC(19),SATPSI(19),&
             SATDK(19),SATDW(19),WLTSMC(19)                             
!                                                                       
        DATA MAXSMC/0.339, 0.421, 0.434, 0.476, 0.476,                  &
                    0.439, 0.404, 0.464, 0.465, 0.406,                  &
                    0.468, 0.468, 0.439, 1.0,   0.20,                   &
                    0.421, 0.468, 0.200, 0.339/    
        DATA SATPSI/0.069, 0.036, 0.141, 0.759, 0.759,                  &
                    0.355, 0.135, 0.617, 0.263, 0.098,                  &
                    0.324, 0.468, 0.355, 0.0,   0.069,                  &
                    0.036, 0.468, 0.069, 0.069/    
        DATA SATDK /1.07E-6, 1.41E-5, 5.23E-6, 2.81E-6, 2.81E-6,        &         
                    3.38E-6, 4.45E-6, 2.04E-6, 2.45E-6, 7.22E-6,        &
                    1.34E-6, 9.74E-7, 3.38E-6, 0.0, 1.41E-4,            &
                    1.41E-5, 9.74E-7, 1.41E-4, 1.07E-6/                 
        DATA BB    /2.79,  4.26,  4.74,  5.33,  5.33,                   &
                    5.25,  6.66,  8.72,  8.17,  10.73,                  &
                   10.39, 11.55,  5.25,  0.0,   2.79,                   &
                    4.26,  11.55,  2.79,  2.79/   
!        DATA REFSMC/0.236, 0.283, 0.312, 0.360, 0.360
        DATA REFSMC/0.236, 0.383, 0.383, 0.360, 0.383,                  &
                    0.329, 0.314, 0.387, 0.382, 0.338,                  &
                    0.404, 0.412, 0.329, 0.0,   0.17,                   &
                    0.283, 0.454, 0.17, 0.236/  
        DATA WLTSMC/0.010, 0.028, 0.047, 0.084, 0.084,                  &
                    0.066, 0.067, 0.120, 0.103, 0.100,                  &
                    0.126, 0.138, 0.066, 0.0,   0.006,                  &
                    0.028, 0.030, 0.006, 0.01/            
        DATA SATDW/0.608E-6, 0.514E-5, 0.805E-5,  0.239E-4, 0.239E-4,   &  
                   0.143E-4, 0.990E-5, 0.237E-4,  0.113E-4, 0.187E-4,   &
                   0.964E-5, 0.112E-4, 0.143E-4,  0.0,      0.136E-3,   &
                   0.514E-5, 0.112E-4, 0.136E-3,  0.608E-6/ 
        DATA F11 /-0.472, -1.044, -0.569,  0.162,  0.162,               &
                  -0.327, -1.491, -1.118, -1.297, -3.209,               &
                  -1.916, -2.138, -0.327,  0.0,   -1.111,               &
                  -1.044, -10.472, -0.472, -0.472/                      
                                                                        
!        DATA DRYSMC/0.07,  0.14,  0.22,  0.08,  0.18,  0.16,  0.120,   &
!                     0.10,  0.07/                                      
! NOTE: SATDW = BB*SATDK*(SATPSI/MAXSMC)                                
!         F11 = ALOG10(SATPSI) + BB*ALOG10(MAXSMC) + 2.0                
!       REFSMC1=MAXSMC*(5.79E-9/SATDK)**(1/(2*BB+3)) 5.79E-9 m/s= 0.5 mm
!       REFSMC=REFSMC1+1./3.(MAXSMC-REFSMC1)                            
!       WLTSMC1=MAXSMC*(200./SATPSI)**(-1./BB)    (Wetzel and Chang, 198
!       WLTSMC=WLTSMC1-0.5*WLTSMC1                                      
! Note: the values for playa is set for it to have a thermal conductivit
! as sand and to have a hydrulic conductivity as clay
                                                                        
        SAVE                                                            

!----------------------------------------------------------------------
!***  DECLARATIONS FOR IMPLICIT NONE
    REAL :: SBB, SDRYSMC, SF11, SMAXSMC, SREFSMC, SSATPSI, SSATDK,    &
            SSATDW,SWLTSMC
!----------------------------------------------------------------------
                                                                        
!    SOIL PARAMETERS                                                    
                                                                        
      SBB=BB(SOLTYP)                                                    
!      SDRYSMC = DRYSMC(SOLTYP)                                         
      SF11=F11(SOLTYP)                                                  
      SMAXSMC=MAXSMC(SOLTYP)                                            
      SREFSMC=REFSMC(SOLTYP)                                            
      SSATPSI=SATPSI(SOLTYP)                                            
      SSATDK=SATDK(SOLTYP)                                              
      SSATDW=SATDW(SOLTYP)                                              
      SWLTSMC=WLTSMC(SOLTYP)                                            
      SDRYSMC=SWLTSMC                                                   
!                                                                       
      END SUBROUTINE  PRMSOI

!----------------------------------------------------------------------------
                                                                        
       SUBROUTINE  PRMVEG_SIB(VEGTYP,CMCMAX,TOPT,RSMAX,ALBEDO,Z0,       &         
                          SHDFAC,NROOT,RCMIN,RGL,HS)                             
       IMPLICIT NONE
!                                                                                
!    Set-up vegetation parameters for a given vegetaion type	                 
!                                                                                
!       Input:                                                                   
!           VEGTYP: Vegetation type                                              
!      Output:                                                                   
!     Vegetation parameters:                                                     
!               ALBEDO: SFC albedo                                               
!               CMXTBL: MAX CNPY Capacity                                        
!                   Z0: Roughness length                                         
!               SHDFAC: Plant shade factor                                       
!                NROOT: Rooting depth                                            
!                RCMIN: Mimimum stomatal resistance                              
!        RSMAX and RGL: Parameters used in radiation stress function             
!                   HS: Parameter used in vapor pressure deficit functio         
!                 TOPT: Parameter used in temperature stress function            
!               CMCMAX: Maximum canopy water capacity                            
!                                                                                
! **********************************************************************         
!                                                                                
!     SSiB Vegetation Types (Dorman and Sellers, 1989; JAM)                      
!                                                                                
!    1:   Broadleaf-evergreen trees  (tropical forest)                          
!    2:   Broadleaf-deciduous tress                                             
!    3:   Broadleaf and needleleaf tress (mixed forest)                          
!    4:   Needleleaf-evergreen trees                                             
!    5:   Needleleaf-deciduous tress (larch)                                     
!    6:   Broadleaf tress with groundcover (savanna)                             
!    7:   Groundcover only (perennial)                                           
!    8:   Broadleaf shrubs with perennial groundcover                            
!    9:   Broadleaf shrubs with bare soil                                        
!   10:   Dwarf trees and shrubs with groundcover (tundra)                       
!   11:   Bare soil                                                              
!   12:   Cultivations (The same parameters for the Type 7)                      
!   13:   Wetland                                                                
!   14:   Dry coastal complex                                                    
!   15:   Water                                                                  
!   16:   Glacial                                                                
!                                                                                
!***********************************************************************         
!                                                                                
        INTEGER VEGTYP,NROTBL(16)                                                
        REAL ALBTBL(16),Z0TBL(16),SHDTBL(16),RSMTBL(16),                &         
             RGLTBL(16),HSTBL(16)                                                
!                                                                                
        DATA ALBTBL/0.11, 0.12,  0.12,  0.10,  0.11,  0.19,  0.19,      &         
        0.25, 0.25,  0.16,  0.12,  0.19, 0.12, 0.19, 0.19,  0.80/                
        DATA  Z0TBL/2.653,0.826, 0.800, 1.089, 0.854, 0.856, 0.075,     &         
        0.238,0.065, 0.050, 0.011, 0.075, 0.04, 0.075, 0.01, 0.011/              
        DATA SHDTBL/0.90, 0.80,  0.80,  0.85,  0.40,  0.50,  0.80,      &         
        0.20, 0.15, 0.30, 0.0,   0.80,  0.50,   0.20, 0.0, 0.0/                  
        DATA NROTBL/16*3/                                                        
        DATA RSMTBL/150.0,100.0, 125.0, 150.0, 100.0, 70.0,  40.0,      &         
        300.0,400.0, 150.0, 999.0, 40.0, 150.0, 400.0, 100.0, 999.0/             
        DATA RGLTBL/30.0, 30.0,  30.0,  30.0,  30.0,  65.0,  100.0,     &         
        100.0,100.0, 100.0, 999.0, 100.0, 100.0, 100.0, 400.0, 999.0/            
        DATA HSTBL/41.69, 54.53, 51.93, 47.35, 47.35, 54.53, 36.35,     &         
        42.00, 42.00, 42.00, 999.0, 36.35, 60.0, 200.0, 500.0, 999.0/            
                                                                                 
        SAVE                                                                     

!----------------------------------------------------------------------
!***  DECLARATIONS FOR IMPLICIT NONE
    REAL :: CMCMAX, TOPT, RSMAX, ALBEDO, Z0, SHDFAC, NROOT, RCMIN,    &
            RGL, HS                                                                                 
!----------------------------------------------------------------------

!       ALBEDO = ALBTBL(VEGTYP)                                                  
!        Z0 = Z0TBL(VEGTYP)                                                      
!        NROOT = NROTBL(VEGTYP)                                                  
!        SHDFAC = SHDTBL(VEGTYP)                                                 
      RCMIN=RSMTBL(VEGTYP)                                                       
      RGL=RGLTBL(VEGTYP)                                                         
      HS=HSTBL(VEGTYP)                                                           
      IF(VEGTYP.EQ.11)SHDFAC=0.0                                                 
!                                                                               
      END SUBROUTINE  PRMVEG_SIB

!------------------------------------------------------------------

       SUBROUTINE  PRMVEG_USGS(VEGTYP,CMCMAX,TOPT,RSMAX,ALBEDO,Z0,      &
                          SHDFAC,NROOT,RCMIN,RGL,HS)                    
       IMPLICIT NONE
!                                                                       
!    Set-up vegetation parameters for a given vegetaion type	           
!                                                                       
!      Input:                                                           
!           VEGTYP: Vegetation type                                     
!     Output:                                                           
!     Vegetation parameters:                                            
!             ALBEDO: SFC albedo                                        
!             CMXTBL: MAX CNPY Capacity                                 
!                 Z0: Roughness length                                  
!             SHDFAC: Plant shade factor                                
!              NROOT: Rooting depth                                     
!              RCMIN: Mimimum stomatal resistance                       
!        RSMAX and RGL: Parameters used in radiation stress function    
!                   HS: Parameter used in vapor pressure deficit functio
!                 TOPT: Parameter used in temperature stress function   
!               CMCMAX: Maximum canopy water capacity                   
!                                                                       
! **********************************************************************
!                                                                       
!     USGS Vegetation Types                                             
!                                                                       
!    1:   Urban and Built-Up Land                                       
!    2:   Dryland Cropland and Pasture                                  
!    3:   Irrigated Cropland and Pasture                                
!    4:   Mixed Dryland/Irrigated Cropland and Pasture                  
!    5:   Cropland/Grassland Mosaic                                     
!    6:   Cropland/Woodland Mosaic                                      
!    7:   Grassland                                                     
!    8:   Shrubland                                                     
!    9:   Mixed Shrubland/Grassland                                     
!   10:   Savanna                                                       
!   11:   Deciduous Broadleaf Forest                                    
!   12:   Deciduous Needleleaf Forest                                   
!   13:   Evergreen Broadleaf Forest                                    
!   14:   Evergreen Needleleaf Fores                                    
!   15:   Mixed Forest                                                  
!   16:   Water Bodies                                                  
!   17:   Herbaceous Wetland                                            
!   18:   Wooded Wetland                                                
!   19:   Barren or Sparsely Vegetated                                  
!   20:   Herbaceous Tundra                                             
!   21:   Wooded Tundra                                                 
!   22:   Mixed Tundra                                                  
!   23:   Bare Ground Tundra                                            
!   24:   Snow or Ice      
!   25:   PLaya
!   26:   Lava
!   27:   White Sand 
!                                                                       
!***********************************************************************
!                                                                       
        INTEGER VEGTYP,NROTBL(27)                                       
        REAL ALBTBL(27),Z0TBL(27),SHDTBL(27),RSMTBL(27),                &
             RGLTBL(27),HSTBL(27)                                       
                                                                        
!                                                                       
        DATA ALBTBL/.25, .19, .15, .17, .19,                            & 
                    .19, .19, .25, .23, .20,                            & 
                    .12, .11, .11, .10, .12,                            &
                    .19, .12, .12, .12, .16,                            &  
                    .16, .16, .17, .70, .30,                            &
                    .16, .60/     
        DATA Z0TBL/1.00, .07,  .07,  .07, .07,                          &
                    .15, .08,  .03,  .05, .86,                          &
                    .80, .85, 2.65, 1.09, .80,                          &
                    .01, .04,  .50,  .01, .04,                          &
                    .06, .05,  .03,  .01, .01,                          &
                    .15, .01/                                           
        DATA SHDTBL/.10, .80, .80, .80, .80,                            & 
                    .80, .80, .70, .70, .50,                            &           
                    .80, .70, .95, .70, .80,                            &
                    .00, .60, .60, .01, .60,                            &          
                    .60, .60, .30, .00, .50,                            &
                    .00, .00/                
        DATA NROTBL/27*3/                                               
        DATA RSMTBL/200.,  40.,  40.,  40.,  40.,                       &
                     70.,  40., 300., 170.,  70.,                       &
                    100., 150., 150., 125., 125.,                       &
                    100.,  40., 100., 999., 150.,                       &
                    150., 150., 200., 999.,  40.,                       &
                    999., 999./             
        DATA RGLTBL/999., 100., 100., 100., 100.,                       &
                     65., 100., 100., 100.,  65.,                       &
                     30.,  30.,  30.,  30.,  30.,                       &
                     30., 100.,  30., 999., 100.,                       &
                    100., 100., 100., 999., 100.,                       &
                    999., 999./      
        DATA HSTBL/999.0,  36.25, 36.25, 36.25, 36.25,                  &
                    44.14, 36.35, 42.00, 39.18, 54.53,                  &
                    54.53, 47.35, 41.69, 47.35, 51.93,                  &
                    51.75, 60.00, 51.93, 999.0, 42.00,                  &
                    42.00, 42.00, 42.00, 999.0, 36.25,                  &
                    999.0, 999.0/                               
                                                                        
        SAVE                                                            

!----------------------------------------------------------------------
!***  DECLARATIONS FOR IMPLICIT NONE
    REAL :: CMCMAX, TOPT, RSMAX, ALBEDO, Z0, SHDFAC, RCMIN, RGL, HS 
    INTEGER :: NROOT
!----------------------------------------------------------------------
                                                                        
!       ALBEDO = ALBTBL(VEGTYP)                                         
!        Z0 = Z0TBL(VEGTYP)                                             
!        NROOT = NROTBL(VEGTYP)                                         
!        SHDFAC = SHDTBL(VEGTYP)                                        
      RCMIN=RSMTBL(VEGTYP)                                              
      RGL=RGLTBL(VEGTYP)                                                
      HS=HSTBL(VEGTYP)                                                  
      IF(VEGTYP.EQ.19.OR.VEGTYP.EQ.24.OR.VEGTYP.EQ.26.OR.VEGTYP.EQ.27)  &
        SHDFAC=0.0                                                
!                                                                       
                                                                        
      END SUBROUTINE PRMVEG_USGS
!                                                                       

!---------------------------------------------------------------------

      SUBROUTINE REDPRM(VEGTYP,SOITYP,                                  &
       CMCMAX,TOPT,RSMAX,ALBEDO,Z0,SHDFAC,NROOT,RCMIN,RGL,HS,           &
       B,DKSAT,DWSAT,SMCMAX,SMCWLT,SMCREF,SMCDRY,F1)                    
      IMPLICIT NONE 
!                                                                       
!    This subroutine reads the soil and vegetation parameters           
!    required for the execution of the OSU land-surface scheme.         
!    by F. Chen 3/15/95                                                 
!                                                                       
      INTEGER NROOT,VEGTYP,SOITYP                                       

!----------------------------------------------------------------------
!***  DECLARATIONS FOR IMPLICIT NONE
    REAL :: CMCMAX, TOPT, RSMAX, ALBEDO, Z0, SHDFAC, RCMIN, RGL, HS,  &
            B, DKSAT, DWSAT, SMCMAX, SMCWLT, SMCREF, SMCDRY, F1,      &
            PSISAT
!----------------------------------------------------------------------

                                                                        
!      Set-up soil parameters                                           
                                                                        
      CALL PRMSOI(SOITYP,B,SMCDRY,F1,SMCMAX,SMCREF,PSISAT,DKSAT,DWSAT,  &
           SMCWLT)                                                      
!                                                                       
!      Set-up vegetation parameters                                     
!     IF(MMINLU.EQ.'SiB ')THEN                                          
!       CALL PRMVEG_SIB(VEGTYP,CMCMAX,TOPT,RSMAX,ALBEDO,Z0,SHDFAC,NROOT,&
!            RCMIN,RGL,HS)                                              
!                                                                       
!     ELSEIF(MMINLU.EQ.'USGS')THEN                                      
        CALL PRMVEG_USGS(VEGTYP,CMCMAX,TOPT,RSMAX,ALBEDO,Z0,SHDFAC,     &
                         NROOT,RCMIN,RGL,HS)                                             
!     ELSE                                                              
!                                                                       
!        PRINT *,'mminlu = ',mminlu                                     
!        STOP 'UNRECOGNIZED LANDUSE'                                     
!     ENDIF                                                             
                                                                        
      END SUBROUTINE REDPRM

      SUBROUTINE ROSR12 ( P, A, B, C, D, DELTA, NSOIL )                 
      IMPLICIT NONE
                                                                        
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C    PURPOSE:  TO INVERT (SOLVE) THE TRI-DIAGONAL MATRIX PROBLEM SHOWN 
!C    =======   BELOW:                                                  
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      INTEGER K                                                         
      INTEGER KK                                                        
      INTEGER NSOIL                                                     
                                                                        
      REAL P(*)                                                         
      REAL A(*)                                                         
      REAL B(*)                                                         
      REAL C(*)                                                         
      REAL D(*)                                                         
      REAL DELTA(*)                                                     

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!     INITIALIZE EQN COEF C FOR THE LOWEST SOIL LAYER.                  
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
                                                                        
      C(NSOIL)=0.0                                                      
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!     SOLVE THE COEFS FOR THE 1ST SOIL LAYER                            
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
      P(1)=-C(1)/B(1)                                                   
      DELTA(1)=D(1)/B(1)                                                
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!     SOLVE THE COEFS FOR SOIL LAYERS 2 THRU NSOIL                      
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
      DO 10 K=2,NSOIL                                                   
        P(K)=-C(K)*(1.0/(B(K)+A(K)*P(K-1)))                             
        DELTA(K)=(D(K)-A(K)*DELTA(K-1))*(1.0/(B(K)+A(K)*P(K-1)))        
   10 CONTINUE                                                          
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!     SET P TO DELTA FOR LOWEST SOIL LAYER.                             
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
      P(NSOIL)=DELTA(NSOIL)                                             
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!     ADJUST P FOR SOIL LAYERS 2 THRU NSOIL                             
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
      DO 20 K=2,NSOIL                                                   
        KK=NSOIL-K+1                                                    
        P(KK)=P(KK)*P(KK+1)+DELTA(KK)                                   
   20 CONTINUE                                                          
      
      END SUBROUTINE ROSR12

      SUBROUTINE SFLX(ICE,SATFLG,DT,Z,NSOIL, NROOT, SLDPTH,             &
       F,SOLDN,SFCPRS,PRCP,SFCTMP,TH2,Q2,Q2SAT,DQSDT2,TBOT,CH,CHKFF,    & 
       VEGTYP,SOITYP,SHDFAC,                                            & 
       ETP,ETA,H,S,RUNOFF1,RUNOFF2,Q1,SNMAX,                            &
       T1,CMC,SMC,STC,SNODEP,SOILW,SOILM)                               
      IMPLICIT NONE                                                     
                                                                        
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C    PURPOSE:  TO CALCULATE AND ACCUMULATE EVAPOTRANSPIRATION VALUES   
!C    =======   BASED UPON A SOIL HYDROLOGY MODEL FROM OREGON STATE     
!C              UNIVERSITY. THE ROUTINE ALSO CALCULATES SOIL TEMPS      
!C              AND MOISTURE VALUES AT TWO SOIL DEPTHS.                 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                  Argument list in the CALL SFLX                       
! Prepared by Fei Chen in 12/97                                         
! 1. Calling Statement                                                  
!        CALL  SFLX(ICE,SATFLG,DT,Z,NSOIL,NROOT, SLDPTH,                
!     &   F,SOLDN,SFCPRS,PRCP,SFCTMP,TH2,Q2,Q2SAT,DQSDT2,TBOT,CH,CHKFF, 
!     &   VEGTYP,SOITYP,SHDFRC,                                         
!     &   ETP,ETA,H,S,RUNOFF1,RUNOFF2,Q1,SNMAX,                         
!     &   T1,CMC,SMC,STC,SNODEP,SOILW,SOILM)                            
! 2. Input                                                              
!                  *** General Parameters ***                           
!          ICE: SEA-ICE FLAG  (=1: sea-ice, =0: land)                   
!       SATFLG: ATMOSPHERIC SATURATION FLAG  (=0.0: Saturation (ETP=0.0 
!                                             =1.0: otherwise)          
!           DT: TIMESTEP (SEC)                                          
!            Z: HT OF FIRST MODEL LEVEL ABOVE GROUND (M)                
!        NSOIL: NUMBER OF SOIL LAYERS                                   
!        NROOT: NUMBER OF ROOT-ZONE LAYERS                              
!       SLDPTH: THE DEPTH OF EACH SOIL LAYER (M)                        
!                  *** Atmospheric variables ***                        
!            F: NET DOWNWARD RADIATION (W M-2; positive, if downward)   
!                F=SOLDN-SOLUP+LONGDN=(1-ALBEDO)*SOLDN+LONGDN           
!        SOLDN: SOLAR DOWNWARD RADIATION (W M-2; positive, if downward) 
!       SFCPRS: PRESSURE AT 1ST MDL LVL ABV SKIN (PASCALS)              
!         PRCP: PRECIP RATE (KG M-2 S-1)                                
!       SFCTMP: AIR TEMPERATURE  AT 1ST MDL LVL ABV SKIN (K)            
!          TH2: AIR POTENTIAL TEMPERATURE  EXTRAPOLATED TO THE SURFACE (
!           Q2: MIXING RATIO AT 1ST MDL LVL ABV SKIN (KG KG-1)          
!        Q2SAT: SAT MIXING RATIO AT 1ST MDL LVL ABV SKIN (KG KG-1)      
!       DQSDT2: SLOPE OF SAT SPECIFIC HUMIDITY CURVE AT T=T2 (KG KG-1 K-
!         TBOT: BOTTOM SOIL TEMPERATURE (local yearly-mean air temperatu
!           CH: DRAG COEF FOR HEAT AND MOISTURE (M S-1)                 
!        CHKFF: DRAG COEF FOR HEAT AND MOISTURE MULTIPLIED BY RHO*Cp    
!                  *** Canopy/Soil Characteristics ***                  
!       VEGTYP: Vegetation type                                         
!       SOITYP: Soil type                                               
!       SHDFRC: PLANT SHADING FACTOR (or GREENESS FRACTION) (UNITLESS)  
!                 *** State Variables ***                               
!           T1: SKIN (GRND SFC) TEMPERATURE (K)                         
!          CMC: CANOPY MOISTURE CONTENT (M)                             
!   SMC(NSOIL): SOIL MOISTURE CONTENT (VOLUMETRIC FRACTION)             
!   STC(NSOIL): SOIL TEMP (K)                                           
!       SNODEP: WATER EQUIVALENT SNOW DEPTH (M)                         
!    *** Note: The above state variables are also output ***            
! 3. OUTPUT                                                             
!          ETP: POTENTIAL EVAPORATION (W M-2)                           
!          ETA: ACTUAL HEAT FLUX (W M-2: negative, if upward from surfac
!            H: SENSIBLE HEAT FLUX (W M-2: negative, if upward from surf
!            S: SOIL HEAT FLUX (W M-2: negative, if downward from surfac
!      RUNOFF1: GRND SFC RUNOFF (M S-1)                                 
!      RUNOFF2: UNDERGROUND RUNOFF (M S-1)                              
!           Q1: EFFECTIVE MIXING RATIO AT GRND SFC ( KG KG-1)           
!        SNMAX: SNOW MELT (M) (WATER EQUIVALENT)                        
!        SOILW: AVAILABLE WTAER CONTENT (M)                             
!        SOILM: MAX WTAER CONTENT (M)                                   
                                                                        
                                                                        
      INTEGER ICE                                                       
      INTEGER NSOIL,VEGTYP,SOITYP,NROOT                                 
                                                                        
      LOGICAL SNOWNG                                                    
      LOGICAL FRZGRA                                                    
                                                                        
      REAL B                                                            
      REAL CFACTR                                                       
      REAL CH                                                           
      REAL CMC                                                          
      REAL CMCMAX                                                       
      REAL DF1                                                          
      REAL DKSAT                                                        
                                                                        
      REAL DT                                                           
      REAL DWSAT                                                        
      REAL EPSCA                                                        
      REAL ESD                                                          
      REAL ETA                                                          
      REAL ETP                                                          
      REAL F                                                            
      REAL F1                                                           
      REAL PC                                                           
      REAL PRCP                                                         
      REAL Q1                                                           
      REAL Q2                                                           
      REAL Q2SAT                                                        
      REAL RCH                                                          
      REAL RR                                                           
      REAL RSMAX                                                        
      REAL S                                                            
      REAL SFCPRS                                                       
      REAL SFCTMP                                                       
      REAL SHDFAC                                                       
      REAL SMC(NSOIL),STC(NSOIL),SLDPTH(NSOIL)                          
      REAL SMCDRY                                                       
      REAL SMCMAX                                                       
      REAL SMCREF                                                       
      REAL SMCWLT                                                       
      REAL SNODEP                                                       
      REAL T1                                                           
      REAL T1MIN                                                        
      REAL T1V                                                          
      REAL T24                                                          
      REAL T2V                                                          
      REAL TBOT                                                         
      REAL TH2                                                          
      REAL TH2V                                                         
      REAL TOPT                                                         
      REAL Z                                                            
      REAL Z0                                                           
      REAL ZSOIL  ( NSOIL )                                             
!        Some universal veg/soil parameters                             
      PARAMETER(CFACTR=0.5,CMCMAX=0.5E-3,TOPT=298.0,RSMAX=5000.0)       
!----------------------------------------------------------------------
!***  DECLARATIONS FOR IMPLICIT NONE
    INTEGER :: KZ,K                                                      
    REAL :: RUNOFF1,RUNOFF2,RUNOFF3,SNMAX,ALBEDO,RCMIN,RGL,HS,PRCP1,  &
            DQSDT2,SATFLG,SOLDN,RC,EDIR1,EC1,ETT1,HEAT,CHKFF,T14,H,   &
            FUP,RES,SNOFLX,SOILM,SOILW,SOILWM,SOILWW,FLX1,FLX2,FLX3   
!----------------------------------------------------------------------


!                                                                       
!   Initialization                                                      
                                                                        
      RUNOFF1=0.0                                                       
      RUNOFF2=0.0                                                       
      RUNOFF3=0.0                                                       
      SNMAX=0.0                                                         
!                                                                       
!   Specify the VEG/SOIL parameters                                     
!                                                                       
      CALL REDPRM(VEGTYP,SOITYP,CMCMAX,TOPT,RSMAX,ALBEDO,Z0,SHDFAC,     &
       NROOT,RCMIN,RGL,HS,B,DKSAT,DWSAT,SMCMAX,SMCWLT,SMCREF,SMCDRY,F1)  
      IF(ICE.EQ.1)THEN                                                  
        DO KZ=1,NSOIL                                                   
          ZSOIL(KZ)=-3.*FLOAT(KZ)/FLOAT(NSOIL)                          
        ENDDO                                                           
      ELSE                                                              
        ZSOIL(1)=-SLDPTH(1)                                             
        DO KZ=2,NSOIL                                                   
          ZSOIL(KZ)=-SLDPTH(KZ)+ZSOIL(KZ-1)                             
        ENDDO                                                           
      ENDIF                                                             
!                                                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC     
!     EXECUTABLE CODE BEGINS HERE...INITIALIZE MISC VARIABLES.          
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC     
      S=0.0                                                             
      SNOWNG=.FALSE.                                                    
      FRZGRA=.FALSE.                                                    
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC     
!     CONVERT THE INPUT SNOW DEPTH (METERS) FROM A SNOWBOARD TYPE       
!     MEASUREMENT TO A LIQUID EQUIVALENT SNOW DEPTH (ESD, IN METERS)    
!     USING A 10 TO 1 RATIO.                                            
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC     
!      ESD = SNODEP * 0.1                                               
      ESD=SNODEP                                                        
      IF(ICE.EQ.1)THEN                                                  
        ESD=0.01                                                        
      ENDIF                                                             
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC     
!     CALC VIRTUAL TEMPS AND POTENTIAL TEMPS AT GRND (SUB 1) AND AT     
!     THE 1ST MDL LVL ABV THE GRND (SUB 2). EXPON IS CP DIVD BY R.      
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC     
      T1V=T1*(1.0+0.61*Q2)                                              
      T2V=SFCTMP*(1.0+0.61*Q2)                                          
!      TH2 = SFCTMP + ( 0.0097545 * Z )                                 
      TH2V=TH2*(1.0+0.61*Q2)                                            
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC     
!     DETERMINE IF IT'S PRECIPING AND WHAT KIND OF PRECIP IT IS.        
!     IF IT'S PRCPING AND THE AIR TEMP IS COLDER THAN 0 C, IT'S SNOWING!
!     IF IT'S PRCPING AND THE AIR TEMP IS WARMER THAN 0 C, BUT THE GRND 
!     TEMP IS COLDER THAN 0 C, FREEZING RAIN IS PRESUMED TO BE FALLING. 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC     
      IF(PRCP.GT.0.0)THEN                                              
        IF(SFCTMP.LE.271.16)THEN                                      
          FRZGRA=.TRUE.
          SNOWNG=.TRUE.
        ELSE IF(SFCTMP.LE.273.16)THEN
          SNOWNG=.TRUE.                                                
        ENDIF                                                         
      ENDIF                                                             
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC     
!     IF EITHER PRCP FLAG IS SET, CONVERT THE PRCP RATE FROM            
!     KG M-2 S-1 TO A LIQUID EQUIV SNOW DEPTH (METERS) AND ACCUM IT.    
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC     
      IF((SNOWNG).OR.(FRZGRA))THEN                                      
        ESD=ESD+PRCP*DT*0.001                                           
        PRCP1=0.0                                                       
      ELSE                                                              
        PRCP1=PRCP                                                      
      ENDIF                                                             
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC     
!     Check: if ESD < 1.E-6 m. The snow cover will be ignored.          
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC     
      IF(ESD.LT.1.E-6)THEN                                              
        SNMAX=ESD                                                       
        ESD=0.0                                                         
        SNOWNG=.FALSE.                                                  
      ENDIF                                                             
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC     
!     CALC GRND HEAT FLUX FOR SNOW PACK CASE (WILL EFFECTIVELY          
!     CALC THE HEAT FLUX FROM THE SNOW PACK).  REMEMBER, IF             
!     T1 > 273.16 K, THERE WILL BE SNOW MELT AND THE SKIN TEMP WILL     
!     BE 273.16 K. CONST (0.35) IS THERMAL DIFFUSIVITY FOR SNOW.        
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC     
      IF(ESD.GT.0.0)THEN                                                
        T1MIN=AMIN1(T1,273.16)                                          
        S=0.35*(T1MIN-STC(1))/(10.0*ESD)                                
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC     
!       SET THE MINIMUM FLUX WITHIN SNOW TO -200 W M-2                  
!       FOR NUMERICAL REASONS                                           
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC     
        S=MIN(MAX(S,-100.0),100.0)                                      
      ELSE                                                              
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC     
!       CALCULATE GRND HEAT FLUX FOR CASE WITH NO SNOW PACK             
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC     
        CALL TDFCND(DF1,SMC(1),B,F1)                                    
        S=DF1*(STC(1)-T1)/(0.5*ZSOIL(1))                                
      ENDIF                                                             
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC     
!     CALL PENMAN SUBROUTINE TO CALCULATE POTENTIAL EVAPORATION         
!     AND OTHER PARTIAL PRODUCTS AND SUMS FOR LATER CALCULATIONS.       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC     
      CALL PENMAN(SFCTMP,SFCPRS,CH,T2V,TH2,PRCP,F,T24,S,Q2,Q2SAT,ETP,   &
             RCH,EPSCA,RR,SNOWNG,FRZGRA,DQSDT2,FLX2)                              
      IF(SATFLG.LE.0.5)ETP=0.0                                          
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC     
!     CALL CANRES to calculate the canopy resistance and convert it     
!     into PC                                                           
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC     
      IF(SHDFAC.GT.1.E-6)THEN                                           
        CALL CANRES(SOLDN,CH,SFCTMP,Q2,SFCPRS,SMC,ZSOIL,NSOIL,SMCWLT,   & 
             SMCREF,RCMIN,RC,PC,NROOT,Q2SAT,DQSDT2,TOPT,RSMAX,RGL,HS)   
      ENDIF                                                             
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC     
!       IF POINT IS TO BE FULLY PROCESSED....                           
!       BASED ON WHETHER OR NOT THERE IS A SNOW PACK, CALL SNOPAC OR    
!       NOPAC TO CALCULATE AN ESTIMATE OF ACTUAL EVAPOTRANSPIRATION,    
!       UPDATE SOIL MOISTURE, AND UPDATE SOIL TEMPERATURE.              
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC     
      IF(ESD.EQ.0.0)THEN                                                
        CALL NOPAC(ETP,ETA,PRCP,SMC,SMCMAX,SMCWLT,SMCREF,SMCDRY,CMC,    &
             CMCMAX,NSOIL,DT,SHDFAC,Q1,Q2,T1,SFCTMP,T24,TH2,F,F1,S,STC, &
             EPSCA,B,PC,RCH,RR,CFACTR,ZSOIL,DKSAT,DWSAT,TBOT,RUNOFF1,   &
             RUNOFF2,RUNOFF3,EDIR1,EC1,ETT1,NROOT,ICE,FLX1,FLX3)                  
      ELSE                                                              
        CALL SNOPAC(ETP,ETA,PRCP,PRCP1,SNOWNG,SMC,SMCMAX,SMCWLT,SMCREF, &
             SMCDRY,CMC,CMCMAX,NSOIL,DT,Q1,Q2,T1,SFCTMP,T24,TH2,F,F1,S, &
             STC,EPSCA,SFCPRS,B,PC,RCH,RR,CFACTR,ESD,ZSOIL,DWSAT,DKSAT, &
             TBOT,SHDFAC,RUNOFF1,RUNOFF2,RUNOFF3,EDIR1,EC1,ETT1,NROOT,  &
             SNMAX,ICE,FLX1,FLX3)                                                 
      ENDIF                                                             
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC     
!     RECONVERT THE LIQUID EQUIVALENT SNOW DEPTH (METERS) TO AN         
!     OUTPUT SNOW DEPTH (METERS) USING A 10 TO 1 RATIO.                 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC     
!      SNODEP = ESD * 10.0                                              
      SNODEP=ESD                                                        
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC      
!     CALC SUB-PRODUCTS THEN USE TO CALC SENSIBLE HEAT FLUX.            
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC      
      HEAT=-CHKFF*(TH2-T1)                                              
      T14=T1*T1*T1*T1                                                   
      RR=T14*6.48E-8/(SFCPRS*CH)+1.0                                    
      H=HEAT                                                            
!                                                                       
!  Convert ETA from KG M-2 S-1 to W M-2                                 
!                                                                       
      ETA=ETA*2.5E+6                                                    
      ETP=ETP*2.5E+6                                                    
      FUP=5.67E-8*T14                                                   
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC      
!     CALCULATE RESIDUAL OF ALL SURFACE ENERGY BALANCE EQN TERMS.       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC      
      RES=F-H-S-ETA-FUP-FLX1-FLX2-FLX3                                  
      SNOFLX=FLX3                                                       
      S=S+RES                                                           
! Convert the sign of soil heat flux so that:                           
!         S>0: warm the surface  (night time)                           
!         S<0: cool the surface  (day time)                             
!      S=-1.0*S                                                         
!                                                                       
!  Convert RUNOFF3 from M to M S-1                                      
!                                                                       
      RUNOFF3=RUNOFF3/DT                                                
      RUNOFF2=RUNOFF2+RUNOFF3                                           
      SOILM=-1.0*SMC(1)*ZSOIL(1)                                        
      DO K=2,NSOIL                                                      
        SOILM=SOILM+SMC(K)*(ZSOIL(K-1)-ZSOIL(K))                        
      ENDDO                                                             
      SOILWM=-1.0*(SMCMAX-SMCWLT)*ZSOIL(1)                              
      SOILWW=-1.0*(SMC(1)-SMCWLT)*ZSOIL(1)                              
      DO K=2,NROOT                                                      
        SOILWM=SOILWM+(SMCMAX-SMCWLT)*(ZSOIL(K-1)-ZSOIL(K))             
        SOILWW=SOILWW+(SMC(K)-SMCWLT)*(ZSOIL(K-1)-ZSOIL(K))             
      ENDDO                                                             
      SOILW=SOILWW/SOILWM                                               
!                                                                       
      END SUBROUTINE SFLX

      SUBROUTINE SHFLX (S,STC,SMC,SMCMAX,NSOIL,T1,DT,YY,ZZ1,ZSOIL,TBOT, &
                         B,F1,DF1,ICE )                                 
      IMPLICIT NONE
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C    PURPOSE:  TO CALCULATE SOIL HEAT FLUX.  SOIL TEMPERATURE CONTENT  
!C    =======   (AN ARRAY CONTAINING SOIL TEMPERATURES) IS ALSO UPDATED.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      
!----------------------------------------------------------------------
!***  DECLARATIONS FOR IMPLICIT NONE
    INTEGER ::  ICE,NSOIL
!----------------------------------------------------------------------
                                                                  
      REAL AI (NSOIL)
      REAL B                                                            
      REAL BI (NSOIL)
      REAL CI (NSOIL)
      REAL DF1                                                          
      REAL DT                                                           
      REAL F1                                                           
      REAL RHSTS (NSOIL  )                                              
      REAL S                                                            
      REAL SMC   ( NSOIL )                                              
      REAL SMCMAX                                                       
      REAL STC   ( NSOIL )                                              
      REAL T1                                                           
      REAL TBOT                                                         
      REAL YY                                                           
      REAL ZSOIL ( NSOIL )                                              
      REAL ZZ1                                                          

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!     HRT ROUTINE CALCS THE RIGHT HAND SIDE OF THE SOIL TEMP DIF EQN    
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
                                                                        
      IF(ICE.EQ.1)THEN                                                  
        CALL HRTICE(RHSTS,STC,SMC,SMCMAX,NSOIL,ZSOIL,YY,ZZ1,TBOT,B,F1,  & 
             DF1,AI, BI, CI)                                                       
      ELSE                                                              
        CALL HRT(RHSTS,STC,SMC,SMCMAX,NSOIL,ZSOIL,YY,ZZ1,TBOT,B,F1,DF1, &
             AI, BI, CI) 
      ENDIF                                                             
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!     HSTEP ROUTINE CALCS/UPDATES SOIL TEMPS BASED ON RHSTS             
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
      CALL HSTEP(STC,RHSTS,DT,NSOIL,AI,BI,CI)                                    
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!     CALC THE GRND (SKIN) TEMPERATURE                                  
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
      T1=(YY+(ZZ1-1.0)*STC(1))/ZZ1                                      
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!     CALC THE SFC SOIL HEAT FLUX                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
      S=DF1*(STC(1)-T1)/(0.5*ZSOIL(1))                                  
      END SUBROUTINE SHFLX

      SUBROUTINE SMFLX ( ETA1,SMC,NSOIL,CMC,ETP1,DT,PRCP1,ZSOIL,        &
           SMCMAX,B,PC,SMCWLT,DKSAT,DWSAT,SMCREF,SHDFAC,CMCMAX,         &
           SMCDRY,CFACTR, RUNOFF1,RUNOFF2, RUNOFF3, EDIR1, EC1,         &
           ETT1, SFCTMP,Q2,NROOT)                                       
      IMPLICIT NONE
                                                                        
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C    PURPOSE:  TO CALCULATE SOIL MOISTURE FLUX.  THE SOIL MOISTURE     
!C    =======   CONTENT (SMC - A PER UNIT VOLUME MEASUREMENT) IS A      
!C              DEPENDENT VARIABLE THAT IS UPDATED WITH PROGNOSTIC EQNS.
!C              THE CANOPY MOISTURE CONTENT (CMC) IS ALSO UPDATED.      
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
!----------------------------------------------------------------------
!***  DECLARATIONS FOR IMPLICIT NONE
    REAL :: RUNOFF1,RUNOFF2, RUNOFF3, EDIR1, EC1, ETT1, SFCTMP, Q2,   &
            DUMMY, CMC2MS
    INTEGER ::  NROOT,K,NSOIL
!----------------------------------------------------------------------

      REAL AI (NSOIL)
      REAL B                                                            
      REAL BI (NSOIL)
      REAL CFACTR                                                       
      REAL CI (NSOIL)
      REAL CMC                                                          
      REAL CMCMAX                                                       
      REAL DKSAT                                                        
      REAL DRIP                                                         
      REAL DT                                                           
      REAL DWSAT                                                        
      REAL EC                                                           
      REAL EDIR                                                         
      REAL ET     ( NSOIL )                                             
      REAL ETA1                                                         
      REAL ETP1                                                         
      REAL ETT                                                          
      REAL EXCESS                                                       
      REAL PC                                                           
      REAL PCPDRP                                                       
      REAL PRCP1                                                        
      REAL RHSCT                                                        
      REAL RHSTT  ( NSOIL )                                             
      REAL RUNOFF                                                       
      REAL SHDFAC                                                       
      REAL SMC    ( NSOIL )                                             
      REAL SMCA   ( NSOIL )                                             
      REAL SMCDRY                                                       
      REAL SMCFG  ( NSOIL )                                             
      REAL SMCMAX                                                       
      REAL SMCREF                                                       
      REAL SMCWLT                                                       
      REAL TRHSCT                                                       
      REAL ZSOIL  ( NSOIL )                                             

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!     EXECUTABLE CODE BEGINS HERE....IF THE POTENTIAL EVAPOTRANS-       
!     PIRATION IS GREATER THAN ZERO...                                  
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
      DUMMY=0.                                                          
      EDIR=0.                                                           
      EC=0.                                                             
      ETT=0.                                                            
      DO 10 K=1,NSOIL                                                   
        ET(K)=0.                                                        
   10 CONTINUE                                                          
!                                                                       
      IF(ETP1.GT.0.0)THEN                                               
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!       RETRIEVE DIRECT EVAPORATION FROM SOIL SURFACE                   
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
        EDIR=                                                           &
             DEVAP(ETP1,SMC(1),ZSOIL(1),SHDFAC,SMCMAX,B,DKSAT,DWSAT,    &
             SMCDRY,SMCREF,SMCWLT)                                      
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!       INITIALIZE PLANT TOTAL TRANSPIRATION, RETRIEVE PLANT            
!       TRANSPIRATION, AND ACCUMULATE IT FOR ALL SOIL LAYERS.           
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
        ETT=0.                                                          
        IF(SHDFAC.GT.0.0)THEN                                           
          CALL TRANSP(ET,NSOIL,ETP1,SMC,CMC,ZSOIL,SHDFAC,SMCWLT,CMCMAX, &
               PC,CFACTR,SMCREF,SFCTMP,Q2,NROOT)                        
          DO 20 K=1,NSOIL                                               
            ETT=ETT+ET(K)                                               
   20     CONTINUE                                                      
        ENDIF                                                           
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!       CALCULATE CANOPY EVAPORATION                                    
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
        EC=SHDFAC*((CMC/CMCMAX)**CFACTR)*ETP1                           
!********  EC should be limited by the total amount of available        
!          water on the canopy. Modified by F.CHEN on 10/18/94          
!********                                                               
        CMC2MS=CMC/DT                                                   
        EC=MIN(CMC2MS,EC)                                               
      ENDIF                                                             
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!     TOTAL UP EVAP AND TRANSP TYPES TO OBTAIN ACTUAL EVAPOTRANSP       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
      EDIR1=EDIR                                                        
      EC1=EC                                                            
      ETT1=ETT                                                          
      ETA1=EDIR+ETT+EC                                                  
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!     COMPUTE THE RIGHT HAND SIDE OF THE CANOPY EQN TERM ( RHSCT )      
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
      RHSCT=SHDFAC*PRCP1-EC                                             
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!     CONVERT RHSCT (A RATE) TO TRHSCT (AN AMT) AND ADD IT TO EXISTING  
!     CMC. IF RESULTING AMT EXCEEDS MAX CAPACITY, IT BECOMES DRIP       
!     AND WILL FALL TO THE GRND.                                        
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
      DRIP=0.                                                           
      TRHSCT=DT*RHSCT                                                   
      EXCESS=CMC+TRHSCT                                                 
      IF(EXCESS.GT.CMCMAX)DRIP=EXCESS-CMCMAX                            
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!     PCPDRP IS THE COMBINED PRCP1 AND DRIP (FROM CMC) THAT             
!     GOES INTO THE SOIL                                                
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
      PCPDRP=(1.-SHDFAC)*PRCP1+DRIP/DT                                  
!C       print*,' **************** SMLX ******************'             
!C       print*,' PCPDRP=', PCPDRP, ' EDIR=', EDIR,' ET=', ET,          
!C     *      'SMC(1)=', SMC(1), 'SMC(2)=', SMC(2), ' PRCP1=', PRCP1    
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!     CALL SUBROUTINES SRT AND SSTEP TO SOLVE THE SOIL MOISTURE         
!     TENDENCY EQUATIONS. ITERATE TWICE THRU THE CALLS. THIS ELIMINATES 
!     2-DELTA-T OSCILLATIONS IN THE SMC VALUES EVEN WITH TIMESTEPS OF   
!     UP TO 1 HOUR. THE 1ST SET OF CALLS YIELDS 1ST GUESS SMC VALUES,   
!     THE 2ND SET OF CALLS YIELDS FINAL SMC VALUES.                     
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
      IF(PCPDRP.GT.0.0)THEN                                             
        CALL SRT(RHSTT,RUNOFF,EDIR,ET,SMC,SMC,NSOIL,PCPDRP,ZSOIL,DWSAT, &
             DKSAT,SMCMAX,B,RUNOFF1,RUNOFF2,DT,SMCWLT,AI,BI,CI)                  
        CALL SSTEP(SMCFG,SMC,DUMMY,RHSTT,RHSCT,DT,NSOIL,SMCMAX,CMCMAX,  &
             RUNOFF3,ZSOIL,AI,BI,CI)                                             
        DO 30 K=1,NSOIL                                                 
          SMCA(K)=(SMC(K)+SMCFG(K))*0.5                                 
   30   CONTINUE                                                        
        CALL SRT(RHSTT,RUNOFF,EDIR,ET,SMC,SMCA,NSOIL,PCPDRP,ZSOIL,DWSAT,&
             DKSAT,SMCMAX,B,RUNOFF1,RUNOFF2,DT,SMCWLT,AI,BI,CI)                  
        CALL SSTEP(SMC,SMC,CMC,RHSTT,RHSCT,DT,NSOIL,SMCMAX,CMCMAX,      &
             RUNOFF3,ZSOIL,AI,BI,CI)                                             
      ELSE                                                              
        CALL SRT(RHSTT,RUNOFF,EDIR,ET,SMC,SMC,NSOIL,PCPDRP,ZSOIL,DWSAT, &
             DKSAT,SMCMAX,B,RUNOFF1,RUNOFF2,DT,SMCWLT,AI,BI,CI)                  
        CALL SSTEP(SMC,SMC,CMC,RHSTT,RHSCT,DT,NSOIL,SMCMAX,CMCMAX,      &
             RUNOFF3,ZSOIL,AI,BI,CI)                                             
      ENDIF                                                             
      END SUBROUTINE SMFLX
                                                                        
      SUBROUTINE SNOPAC ( ETP,ETA,PRCP,PRCP1,SNOWNG,SMC,SMCMAX,SMCWLT,  &
               SMCREF, SMCDRY, CMC, CMCMAX, NSOIL, DT, Q1,              & 
               Q2,T1,SFCTMP,T24,TH2,F,F1,S,STC,EPSCA,SFCPRS,            &
               B, PC, RCH, RR, CFACTR, ESD,                             &
               ZSOIL, DWSAT, DKSAT, TBOT, SHDFAC, RUNOFF1,              &
               RUNOFF2,RUNOFF3,EDIR1,EC1,ETT1,NROOT,SNMAX,ICE,FLX1,FLX3)  
      IMPLICIT NONE
                                                                        
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C    PURPOSE:  TO CALCULATE SOIL MOISTURE AND HEAT FLUX VALUES AND UPDA
!C    =======   SOIL MOISTURE CONTENT AND SOIL HEAT CONTENT VALUES FOR  
!C              THE CASE WHEN A SNOW PACK IS PRESENT.                   
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      INTEGER NSOIL                                                     
                                                                        
      LOGICAL SNOWNG                                                    
                                                                        
      REAL B                                                            
      REAL BETA                                                         
      REAL CFACTR                                                       
      REAL CMC                                                          
      REAL CMCMAX                                                       
      REAL CP                                                           
      REAL CPH2O                                                        
      REAL CPICE                                                        
      REAL DENOM                                                        
      REAL DEW                                                          
      REAL DF1                                                          
      REAL DKSAT                                                        
      REAL DT                                                           
      REAL DWSAT                                                        
      REAL EPSCA                                                        
      REAL ESD                                                          
      REAL ETA                                                          
      REAL ETA1                                                         
      REAL ETP                                                          
      REAL ETP1                                                         
      REAL ETP2                                                         
      REAL EX                                                           
      REAL F                                                            
      REAL FLX1                                                         
      REAL FLX3                                                         
      REAL F1                                                           
      REAL LSUBF                                                        
      REAL LSUBC                                                        
      REAL LSUBS                                                        
      REAL PC                                                           
      REAL PRCP                                                         
      REAL PRCP1                                                        
      REAL Q1                                                           
      REAL Q2                                                           
      REAL RCH                                                          
      REAL RR                                                           
      REAL S                                                            
      REAL S1                                                           
      REAL SFCTMP                                                       
      REAL SHDFAC                                                       
      REAL SIGMA                                                        
      REAL SMC     ( NSOIL )                                            
      REAL SMCDRY                                                       
      REAL SMCMAX                                                       
      REAL SMCREF                                                       
      REAL SMCWLT                                                       
      REAL SNMAX                                                        
      REAL STC     ( NSOIL )                                            
      REAL T1                                                           
      REAL T11                                                          
      REAL T12                                                          
      REAL T12A                                                         
      REAL T12B                                                         
      REAL T24                                                          
      REAL TBOT                                                         
      REAL TH2                                                          
      REAL YY                                                           
      REAL ZSOIL( NSOIL )                                               
      REAL ZZ1

!----------------------------------------------------------------------
!***  DECLARATIONS FOR IMPLICIT NONE
    REAL :: SFCPRS, RUNOFF1, RUNOFF2, RUNOFF3, EDIR1, EC1, ETT1,      &
            QSAT, ETP3, SEH, T14
    INTEGER :: NROOT, ICE 
!----------------------------------------------------------------------


      PARAMETER(CP=1004.5,CPH2O=4.218E+3,CPICE=2.106E+3,                &
           LSUBF=3.335E+5,LSUBC=2.5E+6,LSUBS=2.83E+6,SIGMA=5.67E-8)     
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!     EXECUTABLE CODE BEGINS HERE...                                    
!     CONVERT ETP FM  KG M-2 S-1  TO  M S-1  AND THEN TO AN AMOUNT      
!     (M) AND CALL IT AN EFFECTIVE SNOWPACK REDUCTION AMT, ETP2 (M).    
!     THIS IS THE AMT THE SNOWPACK WOULD BE REDUCED DUE TO EVAPORATION  
!     FROM THE SNOW SFC DURING THE TIMESTEP.  EVAPORATION WILL          
!     PROCEED AT THE POTNTL RATE UNLESS THE SNOW DEPTH IS LESS THAN     
!     THE EXPECTED SNOWPACK REDUCTION.                                  
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
                                                                        
      PRCP1=PRCP1*0.001                                                 
      ETP2=ETP*0.001*DT                                                 
      BETA=1.0                                                          
      IF(ICE.NE.1)THEN                                                  
        IF(ESD.LT.ETP2)THEN                                             
          BETA=ESD/ETP2                                                 
        ENDIF                                                           
      ENDIF                                                             
      DEW=0.0                                                           
      IF(ETP.LT.0.0)THEN                                                
        DEW=-ETP*0.001                                                  
      ENDIF                                                             
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!     CALC AN 'EFFECTIVE SNOW-GRND SFC TEMP' BASED ON HEAT FLUXES       
!     BTWN THE SNOW PACK AND THE SOIL AND ON NET RADIATION. THE         
!     CONSTANT 0.35 (W M-1 K-1) IS THE THERMAL DIFFUSIVITY OF SNOW,     
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
      T12A=((F-SIGMA*T24)/RCH+TH2-SFCTMP-BETA*EPSCA)/RR                 
      T12B=0.35*STC(1)/(10.0*ESD*RR*RCH)                                
      DENOM=1.0+0.35/(10.0*ESD*RR*RCH)                                  
      T12=(SFCTMP+T12A+T12B)/DENOM                                      
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!     RECALCULATE THE HEAT FLUX INTO OR OUT OF THE SOIL,                
!     AND SET THE EFFECTIVE POTENTIAL EVAPOTRANSP TO ZERO.              
!     THE CONSTANT 0.35 (W M-1 K-1) IS THE THERMAL DIFFUSIVITY OF SNOW. 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
      S=0.35*(T12-STC(1))/(10.0*ESD)                                    
      S=MIN(MAX(S,-100.0),100.0)                                        
      IF(T12.LE.273.16)THEN                                             
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!       IF THE 'EFFECTIVE SNOW-GRND SFC TEMP' IS AT OR BLO FREEZING,    
!       NO SNOW MELT WILL OCCUR.  SET THE SKIN TEMP TO THIS             
!       EFFECTIVE TEMP AND SET THE EFFECTIVE PRECIP TO ZERO.            
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
        ESD=AMAX1(0.0,ESD-ETP2)                                         
        T1=T12                                                          
        FLX3=0.0                                                        
        EX=0.0                                                          
        SNMAX=0.0                                                       
      ELSE                                                              
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!       IF THE 'EFFECTIVE SNOW-GRND SFC TEMP' IS ABOVE FREEZING, SNOW   
!       MELT WILL OCCUR. CALL THE SNOW MELT RATE,EX AND AMT, SNMAX.     
!       REVISE THE EFFECTIVE SNOW DEPTH.  REVISE THE SKIN TEMP BECAUSE  
!       IT WOULD HAVE CHGD DUE TO THE LATENT HEAT RELEASED BY THE       
!       MELTING. CALC THE LATENT HEAT RELEASED, FLX3. SET THE EFFECTIVE 
!       PRECIP, PRCP1 TO THE SNOW MELT RATE, EX FOR USE IN SMFLX.       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
! This is the old version of snow melt                                  
!        EX = 0.001 * RCH * RR * ( T12 - 273.16 ) / LSUBF               
!        SNMAX = EX * DT                                                
!        EX = AMIN1 ( ESD / DT, EX )                                    
!        ESD = AMAX1 ( ESD - SNMAX, 0.0 )                               
!        T1 = AMAX1 ( 273.16, (T12-(LSUBF * EX * 1000.0 / (RCH*RR))))   
!        FLX3 = LSUBF * EX * 1000.0                                     
!        PRCP1 = PRCP1 + EX                                             
! Below is the correction of evaporation considering snow melting       
!                    F. Chen (96/3/18)                                  
        T1=273.16                                                       
        QSAT=(0.622*6.11E2)/(SFCPRS-0.378*6.11E2)                       
        ETP=RCH*(QSAT-Q2)/CP                                            
        ETP2=ETP*0.001*DT                                               
        BETA=1.0                                                        
        IF(ESD.LE.ETP2)THEN                                             
          BETA=ESD/ETP2                                                 
          ESD=0.0                                                       
          SNMAX=0.0                                                     
          EX=0.0                                                        
        ELSE                                                            
          ESD=MAX(0.0,ESD-ETP2)                                         
          ETP3=ETP*LSUBC                                                
          S=0.35*(T1-STC(1))/(10.0*ESD)                                 
          S=MIN(MAX(S,-100.0),100.0)                                    
          SEH=RCH*(T1-TH2)                                              
          T14=T1*T1                                                     
          T14=T14*T14                                                   
          FLX3=F-SIGMA*T14-S-SEH-ETP3                                   
          IF(FLX3.LE.0.0)FLX3=0.0                                       
          EX=FLX3*0.001/LSUBF                                           
          SNMAX=EX*DT                                                   
        ENDIF                                                           
        IF(SNMAX.LT.ESD)THEN                                            
          ESD=ESD-SNMAX                                                 
        ELSE                                                            
          EX=ESD/DT                                                     
          SNMAX=ESD                                                     
          ESD=0.0                                                       
          FLX3=EX*1000.0*LSUBF                                          
        ENDIF                                                           
!         CALL TDFCND ( DF1, SMC(1), B, F1 )                            
!         YYNUM = F - SIGMA * T24                                       
!         YY = SFCTMP + (YYNUM/RCH+TH2-SFCTMP-(FLX3+ETP3)) / RR         
!          ZZ1 = DF1 / ( -0.5 * ZSOIL(1) * RCH * RR ) + 1.0             
!          T1 = (YY + (ZZ1 - 1.0) * STC(1)) / ZZ1                       
!         S = DF1 * ( STC(1) - T1 ) / ( 0.5 * ZSOIL(1) )                
        PRCP1=PRCP1+EX                                                  
      ENDIF                                                             
! SET THE EFFECTIVE POTENTIAL EVAPOTRANSP TO ZERO                       
      ETP1=0.0                                                          
!                                                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!     SMFLX RETURNS SOIL MOISTURE VALUES AND PRELIMINARY VALUES OF      
!     EVAPOTRANSPIRATION.  IN THIS, THE SNOW PACK CASE, THE PRELIM-     
!     VALUES (ETA1) ARE NOT USED IN SUBSEQUENT CALCULATION OF E.        
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
      IF(ICE.NE.1)THEN                                                  
        CALL SMFLX(ETA1,SMC,NSOIL,CMC,ETP1,DT,PRCP1,ZSOIL,SMCMAX,B,PC,  &
             SMCWLT,DKSAT,DWSAT,SMCREF,SHDFAC,CMCMAX,SMCDRY,CFACTR,     &
             RUNOFF1,RUNOFF2,RUNOFF3,EDIR1,EC1,ETT1,SFCTMP,Q2,NROOT)    
      ENDIF                                                             
      ETA=BETA*ETP                                                      
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!     CALC/UPDATE THE GRND SFC (SKIN) MIXING RATIO.                     
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
      Q1=Q2+ETA*CP/RCH                                                  
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!     RETRIEVE THE SOIL TEMP DIFFUSIVITY/CONDUCTIVITY.                  
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
      CALL TDFCND(DF1,SMC(1),B,F1)                                      
      IF(ICE.EQ.1)DF1=2.2                                               
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!     THE 'ADJUSTED TOP SOIL LYR TEMP' ( YY ) AND THE 'ADJUSTED SOIL    
!     HEAT FLUX' ( ZZ1 ) ARE SET TO THE TOP SOIL LYR TEMP, AND 1,       
!     RESPECTIVELY.  THESE ARE CLOSE-ENOUGH APPROXIMATIONS BECAUSE      
!     THE SFC HEAT FLUX TO BE COMPUTED IN SHFLX WILL EFFECTIVELY BE     
!     THE FLUX AT THE SNOW TOP SURFACE.  T11 IS A DUMMY ARGUEMENT       
!     SINCE WE WILL NOT USE ITS VALUE AS REVISED BY SHFLX.              
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
      ZZ1=1.0                                                           
      YY=STC(1)-0.5*S*ZSOIL(1)*ZZ1/DF1                                  
      T11=T1                                                            
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!     SHFLX WILL CALC/UPDATE THE SOIL TEMPS. NOTE THE SOIL HEAT FLUX    
!     ( S1 ) AND THE SKIN TEMP ( T11 ) OUTPUT FROM THIS CALL ARE NOT    
!     USED IN ANY SUBSEQUENT CALCULATIONS.                              
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
      CALL SHFLX(S1,STC,SMC,SMCMAX,NSOIL,T11,DT,YY,ZZ1,ZSOIL,TBOT,B,F1, &
           DF1,ICE)                                                     
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!     IF PRECIP IS FALLING, CALC HEAT FLUX FROM SNOW SFC TO NEWLY       
!     ACCUMULATING PRECIP.                                              
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
      FLX1=0.0                                                          
      IF(SNOWNG)THEN                                                    
        FLX1=CPICE*PRCP*(T1-SFCTMP)                                     
      ELSE                                                              
        IF(PRCP.GT.0.0)FLX1=CPH2O*PRCP*(T1-SFCTMP)                      
      ENDIF                                                             
      END SUBROUTINE SNOPAC
                                                                        
      SUBROUTINE SRT (RHSTT,RUNOFF,EDIR,ET,SMC,SMCA,NSOIL,PCPDRP,       &
       ZSOIL,DWSAT,DKSAT,SMCMAX,B,RUNOFF1,RUNOFF2,DT,SMCWLT,AI,BI,CI)         
      IMPLICIT NONE
                                                                        
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C    PURPOSE:  TO CALCULATE THE RIGHT HAND SIDE OF THE TIME TENDENCY   
!C    =======   TERM OF THE SOIL WATER DIFFUSION EQUATION.  ALSO TO     
!C              COMPUTE ( PREPARE ) THE MATRIX COEFFICIENTS FOR THE     
!C              TRI-DIAGONAL MATRIX OF THE IMPLICIT TIME SCHEME.        
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!----------------------------------------------------------------------
!***  DECLARATIONS FOR IMPLICIT NONE
    INTEGER ::  K,NSOIL, IOHINF, KS
    REAL :: RUNOFF1, RUNOFF2, DT, SMCWLT, REFKDT, DT1, DD, VAL,       &
            DDT, PX, SSTT, REFDK
!----------------------------------------------------------------------

      REAL AI     ( NSOIL )                                             
      REAL B                                                            
      REAL BI     ( NSOIL )                                             
      REAL CI     ( NSOIL )                                             
      REAL DMAX   ( NSOIL )                                             
      REAL DDZ                                                          
      REAL DDZ2                                                         
      REAL DENOM                                                        
      REAL DENOM2                                                       
      REAL DKSAT                                                        
      REAL DSMDZ                                                        
      REAL DSMDZ2                                                       
      REAL DWSAT                                                        
      REAL EDIR                                                         
      REAL ET     ( NSOIL )                                             
      REAL INFMAX                                                       
      REAL KDT                                                          
      REAL MXSMC                                                        
      REAL MXSMC2                                                       
      REAL NUMER                                                        
      REAL PCPDRP                                                       
      REAL PDDUM                                                        
      REAL RHSTT  ( NSOIL )                                             
      REAL RUNOFF                                                       
      REAL SMC   ( NSOIL )                                              
      REAL SMCA  ( NSOIL )                                              
      REAL SMCMAX                                                       
      REAL WCND                                                         
      REAL WCND2                                                        
      REAL WDF                                                          
      REAL WDF2                                                         
      REAL ZSOIL  ( NSOIL )                                             
                                                                        
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!     DETERMINE RAINFALL INFILTRATION RATE AND RUNOFF                   
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
                                                                        
!                                                                       
! *************  Include the infiltration formule from Schaake and Koren
!           Model                                                       
!                                                                       
!C    MODIFIED BY Q DUAN                                                
!C    REPLACE THE MAXIMUM INFILTRATION EQUATION IN OSU MODEL            
!C    BY SCHAAKE/KOREN EXPRESSION                                       
!C                                                                      
!C    INITIALIZE PARAMETERS AND VARIABLES                               
!C                                                                      
                                                                        
      IOHINF=1                                                          
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!     DETERMINE RAINFALL INFILTRATION RATE AND RUNOFF                   
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
      PDDUM=PCPDRP                                                      
      RUNOFF1=0.0                                                       
      IF(PCPDRP.NE.0.0)THEN                                             
!C++  Modified by Q. Duan, 5/16/94                                      
!        IF (IOHINF .EQ. 1) THEN                                        
!          REFKDT = 3.0                                                 
!          REFDK = 3.4341E-6                                            
!FEI Modify the value of REFKDT and REFDK according to recent           
!    PILPS2d and GSWP  results REFDK=2.E-6 is the sat. DK. value        
! for the soil type 2 (last PILPS2 Fei GSWP runs used REFKDT=0.75, REFDK
        REFKDT=3.0                                                      
        REFDK=2.0E-6                                                    
        DT1=DT/86400.                                                   
!         SMCAV = SMCMAX - SMCWLT                                       
!         DMAX(1)=-ZSOIL(1)*SMCAV                                       
!         DMAX(1)=DMAX(1)*(1.0 - (SMCA(1)-SMCWLT)/SMCAV)                
        DMAX(1)=-ZSOIL(1)*(SMCMAX-SMCA(1))                              
        DD=DMAX(1)                                                      
        DO KS=2,NSOIL                                                   
!          DMAX(KS)=(ZSOIL(KS-1)-ZSOIL(KS))*SMCAV                       
!          DMAX(KS)=DMAX(KS)*(1.0 - (SMCA(KS)-SMCWLT)/SMCAV)            
          DMAX(KS)=(ZSOIL(KS-1)-ZSOIL(KS))*(SMCMAX-SMCA(KS))            
          DD=DD+DMAX(KS)                                                
        ENDDO                                                           
        KDT=REFKDT*DKSAT/REFDK                                          
!C          VAL = (1.-EXP(-KDT*SQRT(DT1)))                              
! remove the SQRT by F. Chen, 07/08/96                                  
        VAL=(1.-EXP(-KDT*DT1))                                          
        DDT=DD*VAL                                                      
        PX=PCPDRP*DT                                                    
        IF(PX.LT.0.0)PX=0.0                                             
        INFMAX=(PX*(DDT/(PX+DDT)))/DT                                   
        INFMAX=MIN(INFMAX,DKSAT)                                        
!        ELSE                                                           
!          INFMAX = -DWSAT*(SMCMAX-SMCA(1))/(0.5*ZSOIL(1))+DKSAT        
!          IF (SMCA(1).GE.SMCMAX) INFMAX = DKSAT                        
!                                                                       
!        END IF                                                         
!                                                                       
!*************** RUNOFF1: SURFACE RUNOFF *******                        
!                                                                       
        IF(PCPDRP.GT.INFMAX)THEN                                        
          RUNOFF1=PCPDRP-INFMAX                                         
          PDDUM=INFMAX                                                  
        ENDIF                                                           
      ENDIF                                                             
!                                                                       
!*********   Below is the original OSU model                            
!                                                                       
!      PDDUM = PCPDRP                                                   
!      print*,' ************** in SRT *******************'              
!      print*,' PCPDRP=', PCPDRP                                        
!      RUNOFF1 = 0.0                                                    
!      IF ( PCPDRP .NE. 0.0 ) THEN                                      
!        INFMAX = - DWSAT * (SMCMAX - SMCA(1))/(0.5 * ZSOIL(1)) + DKSAT 
!        IF ( SMCA(1) .GE. SMCMAX ) INFMAX = DKSAT                      
!        IF ( PCPDRP .GT. INFMAX ) THEN                                 
!          RUNOFF1 = PCPDRP - INFMAX                                    
!                                                                       
!*************** RUNOFF1: SURFACE RUNOFF *******                        
!                                                                       
!          PDDUM = INFMAX                                               
!        END IF                                                         
!      END IF                                                           
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!     DETERMINE SOIL MOISTURE DIFFUSIVITY (WDF) & CONDUCTIVITY (WCND)   
!     COEFS FOR TOP SOIL LYR USING THE HIGHER OF SMC FOR THE TOP        
!     SOIL LAYER OR SMC FOR THE NEXT LOWER SOIL LAYER.                  
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!      MXSMC = MAX( SMCA(1), SMCA(2) )                                  
      MXSMC=SMCA(1)                                                     
      CALL WDFCND(WDF,WCND,MXSMC,SMCMAX,B,DKSAT,DWSAT)                  
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!     CALC THE MATRIX COEFFICIENTS AI, BI, AND CI FOR THE TOP LAYER     
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
      DDZ=1./(-.5*ZSOIL(2))                                             
      AI(1)=0.0                                                         
      BI(1)=WDF*DDZ/(-ZSOIL(1))                                         
      CI(1)=-BI(1)                                                      
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!     CALC RHSTT FOR THE TOP LAYER AFTER CALC'NG THE VERTICAL SOIL      
!     MOISTURE GRADIENT BTWN THE TOP AND NEXT TO TOP LAYERS.            
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
      DSMDZ=(SMC(1)-SMC(2))/(-.5*ZSOIL(2))                              
      RHSTT(1)=(WDF*DSMDZ+WCND-PDDUM+EDIR+ET(1))/ZSOIL(1)               
      SSTT=WDF*DSMDZ+WCND+EDIR+ET(1)                                    
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!     INITIALIZE DDZ2                                                   
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
      DDZ2=0.0                                                          
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!     LOOP THRU THE REMAINING SOIL LAYERS, REPEATING THE ABV PROCESS    
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
      DO 10 K=2,NSOIL                                                   
        DENOM2=(ZSOIL(K-1)-ZSOIL(K))                                    
        IF(K.NE.NSOIL)THEN                                              
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!       RETRIEVE THE SOIL WATER DIFFUSIVITY FOR THE WETTER OF THE       
!       CURRENT OR THE NEXT LOWER SOIL LAYER                            
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!          MXSMC2 = MAX ( SMCA(K), SMCA(K+1) )                          
! ************ PILPS TEST *************                                 
          MXSMC2=SMCA(K)                                                
          CALL WDFCND(WDF2,WCND2,MXSMC2,SMCMAX,B,DKSAT,DWSAT)           
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!       CALC SOME PARTIAL PRODUCTS FOR LATER USE IN CALC'NG RHSTT       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
          DENOM=(ZSOIL(K-1)-ZSOIL(K+1))                                 
          DSMDZ2=(SMC(K)-SMC(K+1))/(DENOM*0.5)                          
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!         CALC THE MATRIX COEF, CI, AFTER CALC'NG ITS PARTIAL PRODUCT   
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
          DDZ2=2.0/DENOM                                                
          CI(K)=-WDF2*DDZ2/DENOM2                                       
        ELSE                                                            
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!         RETRIEVE THE SOIL WATER DIFFUSIVITY AND HYDRAULIC             
!         CONDUCTIVITY FOR THIS LAYER                                   
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
          CALL WDFCND(WDF2,WCND2,SMCA(NSOIL),SMCMAX,B,DKSAT,DWSAT)      
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!         CALC A PARTIAL PRODUCT FOR LATER USE IN CALC'NG RHSTT         
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
          DSMDZ2=0.0                                                    
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!         SET MATRIX COEF CI TO ZERO                                    
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
          CI(K)=0.0                                                     
        ENDIF                                                           
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!       CALC RHSTT FOR THIS LAYER AFTER CALC'NG ITS NUMERATOR           
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
        NUMER=(WDF2*DSMDZ2)+WCND2-(WDF*DSMDZ)-WCND+ET(K)                
!       print*,'DENOM2=',DENOM2,NUMER,k,nsoil
        RHSTT(K)=NUMER/(-DENOM2)                                        
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!       CALC MATRIX COEFS, AI, AND BI FOR THIS LAYER                    
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
        AI(K)=-WDF*DDZ/DENOM2                                           
        BI(K)=-(AI(K)+CI(K))                                            
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!       RESET VALUES OF WDF, WCND, DSMDZ, AND DDZ FOR LOOP TO NEXT LYR  
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
        IF(K.EQ.NSOIL)THEN                                              
!*************** RUNOFF2: ground water runoff ***********               
          RUNOFF2=WCND2                                                 
        ENDIF                                                           
        IF(K.NE.NSOIL)THEN                                              
          WDF=WDF2                                                      
          WCND=WCND2                                                    
          DSMDZ=DSMDZ2                                                  
          DDZ=DDZ2                                                      
        ENDIF                                                           
   10 CONTINUE                                                          
      END SUBROUTINE SRT

      SUBROUTINE SSTEP ( SMCOUT, SMCIN, CMC, RHSTT, RHSCT, DT,          &
        NSOIL, SMCMAX, CMCMAX, RUNOFF3, ZSOIL, AI,BI,CI )        
      IMPLICIT NONE
                                                                        
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C    PURPOSE:  TO CALCULATE/UPDATE THE SOIL MOISTURE CONTENT VALUES    
!C    =======   AND THE CANOPY MOISTURE CONTENT VALUES.                 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INTEGER K                                                         
      INTEGER NSOIL                                                     
      REAL AI     ( NSOIL )                                             
      REAL BI     ( NSOIL )                                             
      REAL CI     ( NSOIL )                                             
      REAL CMC                                                          
      REAL CMCMAX                                                       
      REAL DT                                                           
      REAL RHSCT                                                        
      REAL RHSTT  ( NSOIL )                                             
      REAL SMCIN  ( NSOIL )                                             
      REAL SMCMAX                                                       
      REAL SMCOUT ( NSOIL )                                             
      REAL ZSOIL(NSOIL)                                                 

!----------------------------------------------------------------------
!***  DECLARATIONS FOR IMPLICIT NONE
    REAL :: RUNOFF3, DDZ
    INTEGER :: KK11
!----------------------------------------------------------------------

                                                                        
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
!     CREATE 'AMOUNT' VALUES OF VARIABLES TO BE INPUT TO THE            
!     TRI-DIAGONAL MATRIX ROUTINE.                                      
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
                                                                        
      DO 10 K=1,NSOIL                                                   
        RHSTT(K)=RHSTT(K)*DT                                            
        AI(K)=AI(K)*DT                                                  
        BI(K)=1.+BI(K)*DT                                               
        CI(K)=CI(K)*DT                                                  
   10 CONTINUE                                                          
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
!     CALL ROSR12 TO SOLVE THE TRI-DIAGONAL MATRIX                      
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
      CALL ROSR12(CI,AI,BI,CI,RHSTT,RHSTT,NSOIL)                        
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
!     SUM THE PREVIOUS SMC VALUE AND THE MATRIX SOLUTION TO GET A       
!     NEW VALUE.  MIN ALLOWABLE VALUE OF SMC WILL BE 0.02.              
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
!   ****************** RUNOFF3: Runoff within soil layers *******       
      RUNOFF3=0.                                                        
      DO 20 K=1,NSOIL                                                   
        SMCOUT(K)=SMCIN(K)+CI(K)                                        
        IF(SMCOUT(K).GT.SMCMAX)THEN                                     
          IF(K.EQ.1)THEN                                                
            DDZ=-ZSOIL(1)                                               
          ELSE                                                          
            KK11=K-1                                                    
            DDZ=-ZSOIL(K)+ZSOIL(KK11)                                   
          ENDIF                                                         
          RUNOFF3=RUNOFF3-((SMCMAX-SMCOUT(K))*DDZ)                      
        ENDIF                                                           
        SMCOUT(K)=MAX(MIN(SMCOUT(K),SMCMAX),0.02)                       
   20 CONTINUE                                                          
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
!     CONVERT RHSCT TO AN 'AMOUNT' VALUE AND ADD TO PREVIOUS            
!     CMC VALUE TO GET NEW CMC VALUE.                                   
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
      CMC=CMC+DT*RHSCT                                                  
      CMC=MIN(MAX(CMC,0.),CMCMAX)                                       
      END SUBROUTINE SSTEP

      SUBROUTINE TDFCND ( DF, SMC, B, F1 )                              
      IMPLICIT NONE
                                                                        
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C    PURPOSE:  TO CALCULATE THERMAL DIFFUSIVITY AND CONDUCTIVITY OF    
!C    =======   THE SOIL FOR A GIVEN POINT AND TIME.                    
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      REAL B                                                            
      REAL DF                                                           
      REAL F1                                                           
      REAL PF                                                           
      REAL SMC                                                          
                                                                        
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     IF THE SOIL HAS ANY MOISTURE CONTENT COMPUTE A PARTIAL SUM/PRODUCT
!     OTHERWISE USE A CONSTANT VALUE WHICH WORKS WELL WITH MOST SOILS   
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      IF(SMC.GT.0.)THEN                                                 
        PF=F1-B*ALOG10(SMC)                                             
      ELSE                                                              
        PF=5.2                                                          
      ENDIF                                                             
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     IF THE PARTIAL SUM/PRODUCT IS LOWER THAN OR EQUAL TO 5.1, COMPUTE 
!     A THERMAL CONDUCTIVITY/DIFFUSIVITY.  CAP ITS VALUE AT 1.9.        
!     OTHERWISE SET THE THERMAL CONDUCTIVITY/DIFFUSIVITY TO A CONSTANT. 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IF(PF.LE.5.1)THEN                                                 
        DF=EXP(-(2.7+PF))*420.0                                         
        DF=AMIN1(DF,1.9)                                                
      ELSE                                                              
        DF=0.1744                                                       
      ENDIF                                                             
! ***  Snesitivity test ***                                             
!      DF = 1.2*DF                                                      
      END SUBROUTINE TDFCND

      SUBROUTINE TRANSP ( ET, NSOIL, ETP1, SMC, CMC, ZSOIL,             &
            SHDFAC,SMCWLT,CMCMAX,PC,CFACTR,SMCREF,SFCTMP,Q2,NROOT)      
      IMPLICIT NONE
                                                                        
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C    PURPOSE:  TO CALCULATE TRANSPIRATION FROM THE VEGTYP FOR THIS PT. 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      INTEGER K                                                         
      INTEGER NSOIL                                                     
                                                                        
      REAL CFACTR                                                       
      REAL CMC                                                          
      REAL CMCMAX                                                       
      REAL ET ( NSOIL )                                                 
      REAL ETP1                                                         
      REAL ETP1A                                                        
!      REAL GX                                                           
      REAL PC                                                           
      REAL SHDFAC                                                       
      REAL SMC    ( NSOIL )                                             
      REAL SMCREF                                                       
      REAL SMCWLT                                                       
      REAL ZSOIL  ( NSOIL )                                             
                                                                        
!----------------------------------------------------------------------
!***  DECLARATIONS FOR IMPLICIT NONE
    REAL :: SFCTMP, Q2
    INTEGER :: NROOT
!----------------------------------------------------------------------
!                                                                       
                                                                        
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!       INITIALIZE  PLANT TRANSP TO ZERO FOR ALL SOIL LAYERS.           
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
                                                                        
      DO 20 K=1,NSOIL                                                   
        ET(K)=0.                                                        
   20 CONTINUE                                                          
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!       CALC AN 'ADJUSTED' POTNTL TRANSPIRATION                         
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
      ETP1A=SHDFAC*PC*ETP1*(1.0-(CMC/CMCMAX)**CFACTR)                   
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!       CALC THE PLANT TRANSP.  FIRST, CALC GX AND ET FOR THE 1ST       
!       (TOP) SOIL LAYER.  GX IS THE SIMPLE TRANSPIRATION FUNCTION, A   
!       RATIO OF THE ACTUAL SMC SURPLUS TO THE REFERENCE SMC SURPLUS.   
!       USE THE ABSOLUTE DEPTH OF THE 1ST LAYER (ZSOIL(1)) TO GET       
!       THE PROPORTION OF THE POTENTIAL IN THE FINAL CALCULATION.       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!        GX = ( SMC(1) - SMCWLT ) / ( SMCREF - SMCWLT )                 
!        GX = MAX ( MIN ( GX, 1. ), 0. )                                
! Test canopy resistance                                                
!        GX = 1.0                                                       
!        ET(1) = ( ZSOIL(1) / ZSOIL(NROOT) ) * GX * ETP1A               
      ET(1)=(ZSOIL(1)/ZSOIL(NROOT))*ETP1A                               
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!       LOOP DOWN THRU THE SOIL LAYERS REPEATING THE OPERATION ABOVE,   
!       BUT USING THE THICKNESS OF THE SOIL LAYER (RATHER THAN THE      
!       ABSOLUTE DEPTH OF EACH LAYER) IN THE FINAL CALCULATION.         
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
      DO 10 K=2,NROOT                                                   
!          GX = ( SMC(K) - SMCWLT ) / ( SMCREF - SMCWLT )               
!          GX = MAX ( MIN ( GX, 1. ), 0. )                              
! test canopy resistance                                                
!          GX = 1.0                                                     
!         ET(K) = ((ZSOIL(K)-ZSOIL(K-1))/ZSOIL(NROOT))*GX*ETP1A         
        ET(K)=((ZSOIL(K)-ZSOIL(K-1))/ZSOIL(NROOT))*ETP1A                
   10 CONTINUE                                                          
      END SUBROUTINE TRANSP

      SUBROUTINE WDFCND ( WDF,WCND,SMC,SMCMAX,B,DKSAT,DWSAT )           
      IMPLICIT NONE
                                                                        
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C    PURPOSE:  TO CALCULATE SOIL WATER DIFFUSIVITY AND SOIL            
!C    =======   HYDRAULIC CONDUCTIVITY.                                 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      REAL B                                                            
      REAL DKSAT                                                        
      REAL DWSAT                                                        
      REAL EXPON                                                        
      REAL FACTR2                                                       
      REAL SMC                                                          
      REAL SMCMAX                                                       
      REAL WCND                                                         
      REAL WDF                                                          
                                                                        
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!     CALC THE RATIO OF THE ACTUAL TO THE MAX PSBL SOIL H2O CONTENT     
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
                                                                        
      SMC=SMC                                                           
      SMCMAX=SMCMAX                                                     
      FACTR2=SMC/SMCMAX                                                 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!     PREP AN EXPNTL COEF AND CALC THE SOIL WATER DIFFUSIVITY           
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
      EXPON=B+2.0                                                       
      WDF=DWSAT*FACTR2**EXPON                                           
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!     RESET THE EXPNTL COEF AND CALC THE HYDRAULIC CONDUCTIVITY         
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
      EXPON=(2.0*B)+3.0                                                 
      WCND=DKSAT*FACTR2**EXPON                                          
      END SUBROUTINE WDFCND 
!

  SUBROUTINE lsminit(isn,XICE,VEGFRA,SNOW,SNOWC, CANWAT,SMSTAV,    &
                     SMSTOT, SFCRUNOFF,UDRUNOFF,GRDFLX,ACSNOW,     &
                     ACSNOM,IVGTYP,ISLTYP,TSLB,SMOIS,DZS,SFCEVP,   & !  STEMP
                     TMN,                                          &
                     num_soil_layers,                              &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte                     )

   IMPLICIT NONE 

   INTEGER,  INTENT(IN   )   ::     ids,ide, jds,jde, kds,kde,  &
                                    ims,ime, jms,jme, kms,kme,  &
                                    its,ite, jts,jte, kts,kte

   INTEGER, INTENT(IN)       ::     num_soil_layers

   REAL,    DIMENSION( num_soil_layers), INTENT(IN) :: DZS

   REAL,    DIMENSION( ims:ime, num_soil_layers, jms:jme )    , &
            INTENT(INOUT)    ::                          SMOIS, & 
                                                         TSLB      !STEMP

   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(INOUT)    ::                           SNOW, & 
                                                         SNOWC, & 
                                                        CANWAT, &
                                                        SMSTAV, &
                                                        SMSTOT, &
                                                     SFCRUNOFF, &
                                                      UDRUNOFF, &
                                                        SFCEVP, &
                                                        GRDFLX, &
                                                        ACSNOW, &
                                                          XICE, &
                                                        VEGFRA, &
                                                        TMN, &
                                                        ACSNOM

   INTEGER, DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(INOUT)    ::                         IVGTYP, &
                                                        ISLTYP

!

  INTEGER, INTENT(IN) :: isn
  INTEGER             :: iseason
  INTEGER :: icm,jcm,itf,jtf
  INTEGER ::  I,J,L


   itf=min0(ite,ide-1)
   jtf=min0(jte,jde-1)

   icm = ide/2
   jcm = jde/2

   iseason=isn

   DO J=jts,jtf
       DO I=its,itf
!      SNOW(i,j)=0.
       SNOWC(i,j)=0.
!      SMSTAV(i,j)=
!      SMSTOT(i,j)=
!      SFCRUNOFF(i,j)=
!      UDRUNOFF(i,j)=
!      GRDFLX(i,j)=
!      ACSNOW(i,j)=
!      ACSNOM(i,j)=
    ENDDO
   ENDDO


  END SUBROUTINE lsminit

END MODULE module_bl_lsm



