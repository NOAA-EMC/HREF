









      MODULE module_gwd


      USE MODULE_EXT_INTERNAL     





      INTEGER, PARAMETER :: KIND_PHYS=SELECTED_REAL_KIND(13,60) 
      INTEGER,PRIVATE,SAVE :: IMX, NMTVR, IDBG, JDBG
      REAL,PRIVATE,SAVE :: RAD_TO_DEG   
      REAL,PRIVATE,SAVE :: DEG_TO_RAD   
      REAL (KIND=KIND_PHYS),PRIVATE,SAVE :: DELTIM,RDELTIM
      REAL(kind=kind_phys),PRIVATE,PARAMETER :: SIGFAC=0.0   


      CONTAINS



      SUBROUTINE GWD_init (DTPHS,DELX,DELY,CEN_LAT,CEN_LON,RESTRT       &
     &                     ,GLAT,GLON,CROT,SROT,HANGL                   &
     &                     ,IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE )


      IMPLICIT NONE






































      REAL, INTENT(IN) :: DTPHS,DELX,DELY,CEN_LAT,CEN_LON
      LOGICAL, INTENT(IN) :: RESTRT
      REAL, INTENT(IN), DIMENSION (ims:ime,jms:jme) :: GLON,GLAT
      REAL, INTENT(OUT), DIMENSION (ims:ime,jms:jme) :: CROT,SROT
      REAL, INTENT(INOUT), DIMENSION (ims:ime,jms:jme) :: HANGL
      INTEGER, INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                      ,IMS,IME,JMS,JME,KMS,KME                    &
     &                      ,ITS,ITE,JTS,JTE,KTS,KTE



      REAL, PARAMETER :: POS1=1.,NEG1=-1.
      REAL :: DX,DTR,LAT0,LoN0,CLAT0,SLAT0,CLAT,DLON,X,Y,TLON,ROT
      INTEGER :: I,J








      DX=SQRT((DELX)**2+(DELY)**2)   

      IMX=INT(360./DX+.5)



      NMTVR=14            
      DELTIM=DTPHS
      RDELTIM=1./DTPHS




      DTR=ACOS(-1.)/180. 
      DEG_TO_RAD=DTR     

      LAT0=DTR*CEN_LON   
      LoN0=DTR*CEN_LAT   

      DTR=1./DTR         
      RAD_TO_DEG=DTR     

      CLAT0=COS(LAT0)
      SLAT0=SIN(LAT0)
      DO J=JTS,JTE
        DO I=ITS,ITE
          CLAT=COS(GLAT(I,J))
          DLON=GLON(I,J)-LoN0
          X=CLAT0*CLAT*COS(DLON)+SLAT0*SIN(GLAT(I,J))
          Y=-CLAT*SIN(DLON)
          TLON=ATAN(Y/X)              
          X=SLAT0*SIN(TLON)/CLAT
          Y=MIN(POS1, MAX(NEG1, X) )
          ROT=ASIN(Y)                 
          CROT(I,J)=COS(ROT)
          SROT(I,J)=SIN(ROT)
        ENDDO    
      ENDDO      
      IF (.NOT.RESTRT) THEN

        DO J=JTS,JTE
          DO I=ITS,ITE
            HANGL(I,J)=DTR*HANGL(I,J)  
          ENDDO    
        ENDDO      
      ENDIF































      END SUBROUTINE GWD_init



      SUBROUTINE GWD_driver(U,V,T,Q,Z,DP,PINT,PMID,EXNR, KPBL, ITIME    &
     &                     ,HSTDV,HCNVX,HASYW,HASYS,HASYSW,HASYNW       &
     &                     ,HLENW,HLENS,HLENSW,HLENNW                   &
     &                     ,HANGL,HANIS,HSLOP,HZMAX,CROT,SROT           &
     &                     ,DUDT,DVDT,UGWDsfc,VGWDsfc                   &
     &                     ,IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE )

























































      REAL, INTENT(IN), DIMENSION (ims:ime, kms:kme, jms:jme) ::        &
     &                                   U,V,T,Q,Z,DP,PINT,PMID,EXNR
      REAL, INTENT(IN), DIMENSION (ims:ime, jms:jme) :: HSTDV,HCNVX     &
     &      ,HASYW,HASYS,HASYSW,HASYNW,HLENW,HLENS,HLENSW,HLENNW,HANGL  &
     &      ,HANIS,HSLOP,HZMAX,CROT,SROT
      INTEGER, INTENT(IN), DIMENSION (ims:ime, jms:jme) :: KPBL
      INTEGER, INTENT(IN) :: ids,ide,jds,jde,kds,kde                    &
     &,                      ims,ime,jms,jme,kms,kme                    &
     &,                      its,ite,jts,jte,kts,kte,ITIME




      REAL, INTENT(OUT), DIMENSION (ims:ime, kms:kme, jms:jme) ::       &
     &                                                        DUDT,DVDT
      REAL, INTENT(OUT), DIMENSION (ims:ime, jms:jme) :: UGWDsfc,VGWDsfc




      INTEGER, PARAMETER :: IM=1    
      REAL(KIND=KIND_PHYS), PARAMETER :: G=9.806, GHALF=.5*G            &
     &,                                  THRESH=1.E-6, dtlarge=1.   
      INTEGER, DIMENSION (IM) :: LPBL
      REAL(KIND=KIND_PHYS), DIMENSION (IM,4) :: OA4,CLX4
      REAL(KIND=KIND_PHYS), DIMENSION (IM) :: DUsfc,DVsfc               &
     &,                              HPRIME,OC,THETA,SIGMA,GAMMA,ELVMAX
      REAL(KIND=KIND_PHYS), DIMENSION (IM,KTS:KTE) :: DUDTcol,DVDTcol   &
     &,                    Ucol,Vcol,Tcol,Qcol,DPcol,Pcol,EXNcol,PHIcol
      REAL(KIND=KIND_PHYS), DIMENSION (IM,KTS:KTE+1) :: PINTcol,PHILIcol
      INTEGER :: I,J,IJ,K,Imid,Jmid
      REAL :: Ugeo,Vgeo,Umod,Vmod, TERRtest,TERRmin
      REAL(KIND=KIND_PHYS) :: TEST
      CHARACTER(LEN=255) :: message


logical :: lprnt  
character (len=26) :: label
integer :: kpblmin,kpblmax, mype, iunit
real :: hzmaxmin,hzmaxmax,hanglmin,hanglmax,hslopmin,hslopmax,hanismin,hanismax  &
,hstdvmin,hstdvmax,hcnvxmin,hcnvxmax,hasywmin,hasywmax,hasysmin,hasysmax  &
,hasyswmin,hasyswmax,hasynwmin,hasynwmax,hlenwmin,hlenwmax,hlensmin,hlensmax  &
,hlenswmin,hlenswmax,hlennwmin,hlennwmax,zsmin,zsmax,delu,delv

real :: helvmin,helvmax






lprnt=.false.

if (itime <= 1) then
  CALL WRF_GET_MYPROC(MYPE)   
  kpblmin=100
  kpblmax=-100
  hzmaxmin=1.e6
  hzmaxmax=-1.e6
  hanglmin=1.e6
  hanglmax=-1.e6
  hslopmin=1.e6
  hslopmax=-1.e6
  hanismin=1.e6
  hanismax=-1.e6
  hstdvmin=1.e6
  hstdvmax=-1.e6
  hcnvxmin=1.e6
  hcnvxmax=-1.e6
  hasywmin=1.e6
  hasywmax=-1.e6
  hasysmin=1.e6
  hasysmax=-1.e6
  hasyswmin=1.e6
  hasyswmax=-1.e6
  hasynwmin=1.e6
  hasynwmax=-1.e6
  hlenwmin=1.e6
  hlenwmax=-1.e6
  hlensmin=1.e6
  hlensmax=-1.e6
  hlenswmin=1.e6
  hlenswmax=-1.e6
  hlennwmin=1.e6
  hlennwmax=-1.e6
  zsmin=1.e6
  zsmax=-1.e6

  helvmin=1.e6
  helvmax=-1.e6
  do j=jts,jte
    do i=its,ite
      kpblmin=min(kpblmin,kpbl(i,j))
      kpblmax=max(kpblmax,kpbl(i,j))
      helvmin=min(helvmin,hzmax(i,j))
      helvmax=max(helvmax,hzmax(i,j))
      hanglmin=min(hanglmin,hangl(i,j))
      hanglmax=max(hanglmax,hangl(i,j))
      hslopmin=min(hslopmin,hslop(i,j))
      hslopmax=max(hslopmax,hslop(i,j))
      hanismin=min(hanismin,hanis(i,j))
      hanismax=max(hanismax,hanis(i,j))
      hstdvmin=min(hstdvmin,hstdv(i,j))
      hstdvmax=max(hstdvmax,hstdv(i,j))
      hcnvxmin=min(hcnvxmin,hcnvx(i,j))
      hcnvxmax=max(hcnvxmax,hcnvx(i,j))
      hasywmin=min(hasywmin,hasyw(i,j))
      hasywmax=max(hasywmax,hasyw(i,j))
      hasysmin=min(hasysmin,hasys(i,j))
      hasysmax=max(hasysmax,hasys(i,j))
      hasyswmin=min(hasyswmin,hasysw(i,j))
      hasyswmax=max(hasyswmax,hasysw(i,j))
      hasynwmin=min(hasynwmin,hasynw(i,j))
      hasynwmax=max(hasynwmax,hasynw(i,j))
      hlenwmin=min(hlenwmin,hlenw(i,j))
      hlenwmax=max(hlenwmax,hlenw(i,j))
      hlensmin=min(hlensmin,hlens(i,j))
      hlensmax=max(hlensmax,hlens(i,j))
      hlenswmin=min(hlenswmin,hlensw(i,j))
      hlenswmax=max(hlenswmax,hlensw(i,j))
      hlennwmin=min(hlennwmin,hlennw(i,j))
      hlennwmax=max(hlennwmax,hlennw(i,j))
      zsmin=min(zsmin,z(i,1,j))
      zsmax=max(zsmax,z(i,1,j))
    enddo
  enddo
  write(message,*) 'Maximum and minimum values within GWD-driver for MYPE=',MYPE
  write(message,"(i3,2(a,e12.5))") mype,':  deltim=',deltim,'  rdeltim=',rdeltim
  CALL wrf_message(trim(message))
  write(message,"(i3,2(a,i3))")    mype,':  kpblmin=',kpblmin,'  kpblmax=',kpblmax
  CALL wrf_message(trim(message))
  write(message,"(i3,2(a,e12.5))") mype,':  helvmin=',helvmin,'  helvmax=',helvmax
  CALL wrf_message(trim(message))
  write(message,"(i3,2(a,e12.5))") mype,':  hanglmin=',hanglmin,'  hanglmax=',hanglmax
  CALL wrf_message(trim(message))
  write(message,"(i3,2(a,e12.5))") mype,':  hslopmin=',hslopmin,'  hslopmax=',hslopmax
  CALL wrf_message(trim(message))
  write(message,"(i3,2(a,e12.5))") mype,':  hanismin=',hanismin,'  hanismax=',hanismax
  CALL wrf_message(trim(message))
  write(message,"(i3,2(a,e12.5))") mype,':  hstdvmin=',hstdvmin,'  hstdvmax=',hstdvmax
  CALL wrf_message(trim(message))
  write(message,"(i3,2(a,e12.5))") mype,':  hcnvxmin=',hcnvxmin,'  hcnvxmax=',hcnvxmax
  CALL wrf_message(trim(message))
  write(message,"(i3,2(a,e12.5))") mype,':  hasywmin=',hasywmin,'  hasywmax=',hasywmax
  CALL wrf_message(trim(message))
  write(message,"(i3,2(a,e12.5))") mype,':  hasysmin=',hasysmin,'  hasysmax=',hasysmax
  CALL wrf_message(trim(message))
  write(message,"(i3,2(a,e12.5))") mype,':  hasyswmin=',hasyswmin,'  hasyswmax=',hasyswmax
  CALL wrf_message(trim(message))
  write(message,"(i3,2(a,e12.5))") mype,':  hasynwmin=',hasynwmin,'  hasynwmax=',hasynwmax
  CALL wrf_message(trim(message))
  write(message,"(i3,2(a,e12.5))") mype,':  hlenwmin=',hlenwmin,'  hlenwmax=',hlenwmax
  CALL wrf_message(trim(message))
  write(message,"(i3,2(a,e12.5))") mype,':  hlensmin=',hlensmin,'  hlensmax=',hlensmax
  CALL wrf_message(trim(message))
  write(message,"(i3,2(a,e12.5))") mype,':  hlenswmin=',hlenswmin,'  hlenswmax=',hlenswmax
  CALL wrf_message(trim(message))
  write(message,"(i3,2(a,e12.5))") mype,':  hlennwmin=',hlennwmin,'  hlennwmax=',hlennwmax
  CALL wrf_message(trim(message))
  write(message,"(i3,2(a,e12.5))") mype,':  zsmin=',zsmin,'  zsmax=',zsmax
  CALL wrf_message(trim(message))

endif   





      DO J=JMS,JME
      DO K=KMS,KME
      DO I=IMS,IME
        DUDT(I,K,J)=0.
        DVDT(I,K,J)=0.
      ENDDO
      ENDDO
      ENDDO

      DO J=JMS,JME
      DO I=IMS,IME
        UGWDsfc(I,J)=0.
        VGWDsfc(I,J)=0.
      ENDDO
      ENDDO






      DO J=JTS,JTE
        DO I=ITS,ITE
          if (kpbl(i,j)<kts .or. kpbl(i,j)>kte) go to 100



          TERRtest=HZMAX(I,J)+SIGFAC*HSTDV(I,J)
          TERRmin=Z(I,2,J)-Z(I,1,J)
          IF (TERRtest < TERRmin) GO TO 100







          DO K=KTS,KTE
            DUDTcol(IM,K)=0.
            DVDTcol(IM,K)=0.



            Ucol(IM,K)=U(I,K,J)*CROT(I,J)+V(I,K,J)*SROT(I,J)
            Vcol(IM,K)=V(I,K,J)*CROT(I,J)-U(I,K,J)*SROT(I,J)

            Tcol(IM,K)=T(I,K,J)
            Qcol(IM,K)=Q(I,K,J)



            DPcol(IM,K)=.001*DP(I,K,J)
            PINTcol(IM,K)=.001*PINT(I,K,J)
            Pcol(IM,K)=.001*PMID(I,K,J)
            EXNcol(IM,K)=EXNR(I,K,J)




            PHILIcol(IM,K)=G*(Z(I,K,J)-Z(I,1,J))
            PHIcol(IM,K)=GHALF*(Z(I,K,J)+Z(I,K+1,J))-G*Z(I,1,J)
          ENDDO   

          PINTcol(IM,KTE+1)=.001*PINT(I,KTE+1,J)
          PHILIcol(IM,KTE+1)=G*(Z(I,KTE+1,J)-Z(I,1,J))



          HPRIME(IM)=HSTDV(I,J)   
          OC(IM)=HCNVX(I,J)       
          OA4(IM,1)=HASYW(I,J)    
          OA4(IM,2)=HASYS(I,J)    
          OA4(IM,3)=HASYSW(I,J)   
          OA4(IM,4)=HASYNW(I,J)   
          CLX4(IM,1)=HLENW(I,J)   
          CLX4(IM,2)=HLENS(I,J)   
          CLX4(IM,3)=HLENSW(I,J)  
          CLX4(IM,4)=HLENNW(I,J)  
          THETA(IM)=HANGL(I,J)       
          SIGMA(IM)=HSLOP(I,J)       
          GAMMA(IM)=HANIS(I,J)       
          ELVMAX(IM)=HZMAX(I,J)      
          LPBL(IM)=KPBL(I,J)      





          DUsfc(IM)=0.             
          DVsfc(IM)=0.             
















































write(label,"('GWD:i,j,n=',2i5,i6)") I,J,ITIME

















          CALL GWD_col(DVDTcol,DUDTcol, DUsfc,DVsfc                     & 
     &,              Ucol,Vcol,Tcol,Qcol,PINTcol,DPcol,Pcol,EXNcol      & 
     &,              PHILIcol,PHIcol                                    & 
     &,              HPRIME,OC,OA4,CLX4,THETA,SIGMA,GAMMA,ELVMAX        & 
     &,              LPBL,IM,KTS,KTE,LABEL,LPRNT )                        


















































          DO K=KTS,KTE
            TEST=ABS(DUDTcol(IM,K))+ABS(DVDTcol(IM,K))
            IF (TEST > THRESH) THEN














              Ugeo=Ucol(IM,K)+DUDTcol(IM,K)*DELTIM
              Vgeo=Vcol(IM,K)+DVDTcol(IM,K)*DELTIM



              Umod=Ugeo*CROT(I,J)-Vgeo*SROT(I,J)
              Vmod=Ugeo*SROT(I,J)+Vgeo*CROT(I,J)



              DUDT(I,K,J)=RDELTIM*(Umod-U(I,K,J))
              DVDT(I,K,J)=RDELTIM*(Vmod-V(I,K,J))



test=abs(dudt(i,k,j))+abs(dvdt(i,k,j))
if (test > dtlarge) write(6,"(2a,i2,2(a,e12.4))")  &
label,' => k=',k,'  dudt=',dudt(i,k,j),'  dvdt=',dvdt(i,k,j)





            ENDIF     

          ENDDO   



          UGWDsfc(I,J)=DUsfc(IM)*CROT(I,J)-DVsfc(IM)*SROT(I,J)
          VGWDsfc(I,J)=DUsfc(IM)*SROT(I,J)+DVsfc(IM)*CROT(I,J)

100       CONTINUE
        ENDDO     
      ENDDO       

      END SUBROUTINE GWD_driver





      SUBROUTINE GWD_col (A,B, DUsfc,DVsfc                              &  
     &, U1,V1,T1,Q1, PRSI,DEL,PRSL,PRSLK, PHII,PHIL                     &  
     &, HPRIME,OC,OA4,CLX4,THETA,SIGMA,GAMMA,ELVMAX                     &  
     &, KPBL,IM,KTS,KTE, LABEL,LPRNT )                                     






































































































































      IMPLICIT NONE



      INTEGER, INTENT(IN) :: IM,KTS,KTE
      REAL(kind=kind_phys), INTENT(IN), DIMENSION(IM,KTS:KTE) ::        &
     &                                 U1,V1,T1,Q1,DEL,PRSL,PRSLK,PHIL
      REAL(kind=kind_phys), INTENT(IN), DIMENSION(IM,KTS:KTE+1) ::      &
     &                                                       PRSI,PHII
      REAL(kind=kind_phys), INTENT(IN), DIMENSION(IM,4) :: OA4,CLX4
      REAL(kind=kind_phys), INTENT(IN), DIMENSION(IM) ::                &
     &                              HPRIME,OC,THETA,SIGMA,GAMMA,ELVMAX
      INTEGER, INTENT(IN), DIMENSION(IM) :: KPBL
      CHARACTER (LEN=26), INTENT(IN) :: LABEL
      LOGICAL, INTENT(IN) :: LPRNT



      REAL(kind=kind_phys), INTENT(INOUT), DIMENSION(IM,KTS:KTE) :: A,B
      REAL(kind=kind_phys), INTENT(INOUT), DIMENSION(IM) :: DUsfc,DVsfc








      REAL(kind=kind_phys), PARAMETER :: PI=3.1415926535897931        &
     &,        G=9.806, CP=1004.6, RD=287.04, RV=461.6                &
     &,        FV=RV/RD-1., RDI=1./RD, GOR=G/RD, GR2=G*GOR, GOCP=G/CP &
     &,        ROG=1./G, ROG2=ROG*ROG                                 &
     &,        DW2MIN=1., RIMIN=-100., RIC=0.25, BNV2MIN=1.0E-5       &
     &,        EFMIN=0.0, EFMAX=10.0, hpmax=200.0                     & 
     &,        FRC=1.0, CE=0.8, CEOFRC=CE/FRC, frmax=100.             &
     &,        CG=0.5, GMAX=1.0, CRITAC=5.0E-4, VELEPS=1.0            &
     &,        FACTOP=0.5, RLOLEV=500.0, HZERO=0., HONE=1.            & 
     &,        HE_4=.0001, HE_2=.01                                   & 



     &,  cdmb = 1.0        &    

     &,  hncrit=8000.      &    


     &,  hminmt=50.        &    
     &,  hstdmin=25.       &    
     &,  minwnd=0.1        &    
     &,  dpmin=5.0              
                                

      integer, parameter :: mdir=8
      real(kind=kind_phys), parameter :: FDIR=mdir/(PI+PI)





      integer,save :: nwdir(mdir)
      data nwdir /6,7,5,8,2,3,1,4/

      LOGICAL ICRILV(IM)





      real(kind=kind_phys), DIMENSION(IM) :: WK,PE,EK,ZBK,UP,TAUB,XN    &
     & ,YN,UBAR,VBAR,ULOW,OA,CLX,ROLL,ULOI,DTFAC,XLINV,DELKS,DELKS1     &
     & ,SCOR,BNV2bar, ELEVMX   

      real(kind=kind_phys), DIMENSION(IM,KTS:KTE) ::                    &
     &                      BNV2LM,DB,ANG,UDS,BNV2,RI_N,TAUD,RO,VTK,VTJ
      real(kind=kind_phys), DIMENSION(IM,KTS:KTE-1) :: VELCO
      real(kind=kind_phys), DIMENSION(IM,KTS:KTE+1) :: TAUP
      real(kind=kind_phys), DIMENSION(KTE-1) :: VELKO

      integer, DIMENSION(IM) ::                                         &
     &                 kref,kint,iwk,iwk2,ipt,kreflm,iwklm,iptlm,idxzb



      real(kind=kind_phys) :: ZLEN, DBTMP, R, PHIANG, DBIM              &
     &,                   xl,     rcsks, bnv,   fr                      &
     &,                   brvf,   cleff, tem,   tem1,  tem2, temc, temv &
     &,                   wdir,   ti,    rdz,   dw2,   shr2, bvf2       &
     &,                   rdelks, wtkbj, efact, coefm, gfobnv           &
     &,                   scork,  rscor, hd,    fro,   rim,  sira       &
     &,                   dtaux,  dtauy, pkp1log, pklog

      integer :: ncnt, kmm1, kmm2, lcap, lcapp1, kbps, kbpsp1,kbpsm1    &
     &, kmps, kmpsp1, idir, nwd, i, j, k, klcap, kp1, kmpbl, npt, npr   &
     &, idxm1, ktrial, klevm1, kmll,kmds, KM                            &

     &, ME              

real :: rcl,rcs  




      KM = KTE
      npr = 0
      DO I = 1, IM
         DUsfc(I) = 0.
         DVsfc(I) = 0.



         ELEVMX(I) = ELVMAX(I)
      ENDDO



      ipt = 0
      npt = 0
      IF (NMTVR >= 14) then 
        DO I = 1,IM
          IF (elvmax(i) > HMINMT .AND. hprime(i) > HE_4) then
             npt = npt + 1
             ipt(npt) = i
          ENDIF
        ENDDO
      ELSE
        DO I = 1,IM
          IF (hprime(i) > HE_4) then
            npt = npt + 1
            ipt(npt) = i
          ENDIF
        ENDDO
      ENDIF    



rcl=1.
rcs=1.























      IF (npt <= 0) RETURN     

      do i=1,npt
        IDXZB(i) = 0
      enddo

      DO K = 1, KM
      DO I = 1, IM
      DB(I,K) = 0.
      ANG(I,K) = 0.
      UDS(I,K) = 0.
      ENDDO
      ENDDO



      KMM1   = KM - 1
      KMM2   = KM - 2
      LCAP   = KM
      LCAPP1 = LCAP + 1


      IF (NMTVR .eq. 14) then 






        do i=1,npt
          iwklm(i) = 2
          kreflm(i) = 0
        enddo










        KMLL = kmm1







        DO I = 1, npt
          j = ipt(i)
          ELEVMX(J) = min (ELEVMX(J) + sigfac * hprime(j), hncrit)





        ENDDO

        DO K = 1,KMLL
          DO I = 1, npt
            j = ipt(i)


            pkp1log =  phil(j,k+1) * ROG
            pklog =  phil(j,k) * ROG
            if ( ( ELEVMX(j) .le.  pkp1log ) .and.                      &
     &           ( ELEVMX(j) .ge.   pklog  ) ) THEN

               wk(i)  = G * ELEVMX(j) / ( phil(j,k+1) - phil(j,k) )
               iwklm(I)  =  MAX(iwklm(I), k+1 ) 



            endif



            VTJ(I,K)  = T1(J,K)  * (1.+FV*Q1(J,K))  
            VTK(I,K)  = VTJ(I,K) / PRSLK(J,K)       
            RO(I,K)   = RDI * PRSL(J,K) / VTJ(I,K)  



          ENDDO    

        ENDDO      
















        klevm1 = KMLL - 1
        DO K = 1, klevm1  
          DO I = 1, npt
           j   = ipt(i)
            RDZ  = g   / ( phil(j,k+1) - phil(j,k) )

            BNV2LM(I,K) = (G+G) * RDZ * ( VTK(I,K+1)-VTK(I,K) )         &
     &                     / ( VTK(I,K+1)+VTK(I,K) )
            bnv2lm(i,k) = max( bnv2lm(i,k), bnv2min )



          ENDDO
        ENDDO

        DO I = 1, npt
          J   = ipt(i)
          DELKS(I)  = 1.0 / (PRSI(J,1) - PRSI(J,iwklm(i)))
          DELKS1(I) = 1.0 / (PRSL(J,1) - PRSL(J,iwklm(i)))
          UBAR (I)  = 0.0
          VBAR (I)  = 0.0
          ROLL (I)  = 0.0
          PE   (I)  = 0.0
          EK   (I)  = 0.0
          BNV2bar(I) = (PRSL(J,1)-PRSL(J,2)) * DELKS1(I) * BNV2LM(I,1)
        ENDDO





        DO Ktrial = KMLL, 1, -1
          DO I = 1, npt
             IF ( Ktrial .LT. iwklm(I) .and. kreflm(I) .eq. 0 ) then
                kreflm(I) = Ktrial

if (lprnt) print *,'Ktrial,iwklm(i),kreflm(i)=',Ktrial,iwklm(i),kreflm(I)

             ENDIF
          ENDDO
        ENDDO











        DO I = 1, npt
          DO K = 1, Kreflm(I)
            J        = ipt(i)
            RDELKS     = DEL(J,K) * DELKS(I)


            RCSKS      = RCS      * RDELKS
            UBAR(I)    = UBAR(I)  + RCSKS  * U1(J,K) 
            VBAR(I)    = VBAR(I)  + RCSKS  * V1(J,K) 

            ROLL(I)    = ROLL(I)  + RDELKS * RO(I,K) 
            RDELKS     = (PRSL(J,K)-PRSL(J,K+1)) * DELKS1(I)
            BNV2bar(I) = BNV2bar(I) + BNV2lm(I,K) * RDELKS






          ENDDO
        ENDDO















        DO I = 1, npt
          J = ipt(i)





          DO K = iwklm(I), 1, -1
            PHIANG   =  RAD_TO_DEG*atan2(V1(J,K),U1(J,K))
            ANG(I,K) = ( THETA(J) - PHIANG )
            if ( ANG(I,K) .gt.  90. ) ANG(I,K) = ANG(I,K) - 180.
            if ( ANG(I,K) .lt. -90. ) ANG(I,K) = ANG(I,K) + 180.


            UDS(I,K) = rcs*                                             &
      &          MAX(SQRT(U1(J,K)*U1(J,K) + V1(J,K)*V1(J,K)), minwnd)

            IF (IDXZB(I) .eq. 0 ) then
              PE(I) = PE(I) + BNV2lm(I,K) *                             &
     &           ( G * ELEVMX(J) - phil(J,K) ) *                        &
     &           ( PHII(J,K+1) - PHII(J,K) ) * ROG2













              UP(I)  =  UDS(I,K) * cos(DEG_TO_RAD*ANG(I,K))
              EK(I)  = 0.5 *  UP(I) * UP(I) 


              IF ( PE(I) .ge.  EK(I) ) IDXZB(I) = K


            ENDIF     





          ENDDO       
        ENDDO         

        DO I = 1, npt
          J    = ipt(i)

          ZBK(I) =  ELEVMX(J) - SQRT(UBAR(I)**2 + VBAR(I)**2)/BNV2bar(I)
        ENDDO














        DO I = 1, npt
          J = ipt(i)
          ZLEN = 0.
          IF ( IDXZB(I) .gt. 0 ) then 
            DO K = IDXZB(I), 1, -1
              IF (PHIL(J,IDXZB(I)) > PHIL(J,K)) THEN
                ZLEN = SQRT( ( PHIL(J,IDXZB(I))-PHIL(J,K) ) /           &
     &                       ( PHIL(J,K ) + G * hprime(J) ) )

                R = (cos(DEG_TO_RAD*ANG(I,K))**2 + GAMMA(J) * sin(DEG_TO_RAD*ANG(I,K))**2) / &
     &              (gamma(J) * cos(DEG_TO_RAD*ANG(I,K))**2 + sin(DEG_TO_RAD*ANG(I,K))**2)

                DBTMP = 0.25 *  CDmb *                                  &
     &                  MAX( 2. - 1. / R, HZERO ) * sigma(J) *          &
     &                  MAX(cos(DEG_TO_RAD*ANG(I,K)), gamma(J)*sin(DEG_TO_RAD*ANG(I,K))) *  &
     &                  ZLEN / hprime(J) 
                DB(I,K) =  DBTMP * UDS(I,K)    







              ENDIF        
            ENDDO          
          endif
        ENDDO              





      ENDIF      




      KMPBL  = km / 2 



      if (imx .gt. 0) then


        cleff = 0.5E-5 * SQRT(FLOAT(IMX)/192.0) 


      endif





      DO K = 1,KM
        DO I =1,npt
          J         = ipt(i)
          VTJ(I,K)  = T1(J,K)  * (1.+FV*Q1(J,K))
          VTK(I,K)  = VTJ(I,K) / PRSLK(J,K)
          RO(I,K)   = RDI * PRSL(J,K) / VTJ(I,K)  
          TAUP(I,K) = 0.0




        ENDDO
      ENDDO





      DO K = 1,KMM1
        DO I =1,npt
          J         = ipt(i)
          TI        = 2.0 / (T1(J,K)+T1(J,K+1))
          TEM       = TI  / (PRSL(J,K)-PRSL(J,K+1))

          RDZ       = g   / (phil(j,k+1) - phil(j,k))
          TEM1      = U1(J,K) - U1(J,K+1)
          TEM2      = V1(J,K) - V1(J,K+1)



          DW2       = rcl*(TEM1*TEM1 + TEM2*TEM2)

          SHR2      = MAX(DW2,DW2MIN) * RDZ * RDZ
          BVF2      = G*(GOCP+RDZ*(VTJ(I,K+1)-VTJ(I,K))) * TI
          ri_n(I,K) = MAX(BVF2/SHR2,RIMIN)   



          BNV2(I,K) = (G+G) * RDZ * (VTK(I,K+1)-VTK(I,K))               &
     &                            / (VTK(I,K+1)+VTK(I,K))
          bnv2(i,k) = max( bnv2(i,k), bnv2min )






        ENDDO     
      ENDDO       





















      do i=1,npt
        iwk(i) = 2
      enddo



      DO K=3,KMPBL
        DO I=1,npt
          j   = ipt(i)
          tem = (prsi(j,1) - prsi(j,k))
          if (tem .lt. dpmin) iwk(i) = k
        enddo
      enddo

      KBPS = 1
      KMPS = KM
      DO I=1,npt
        J         = ipt(i)
        kref(I)   = MAX(IWK(I), KPBL(J)+1 ) 
        DELKS(I)  = 1.0 / (PRSI(J,1) - PRSI(J,kref(I)))
        DELKS1(I) = 1.0 / (PRSL(J,1) - PRSL(J,kref(I)))
        UBAR (I)  = 0.0
        VBAR (I)  = 0.0
        ROLL (I)  = 0.0
        KBPS      = MAX(KBPS,  kref(I))
        KMPS      = MIN(KMPS,  kref(I))

        BNV2bar(I) = (PRSL(J,1)-PRSL(J,2)) * DELKS1(I) * BNV2(I,1)
      ENDDO


      KBPSP1 = KBPS + 1
      KBPSM1 = KBPS - 1
      DO K = 1,KBPS
        DO I = 1,npt
          IF (K .LT. kref(I)) THEN
            J          = ipt(i)
            RDELKS     = DEL(J,K) * DELKS(I)




            RCSKS      = RCS      * RDELKS
            UBAR(I)    = UBAR(I)  + RCSKS  * U1(J,K)   
            VBAR(I)    = VBAR(I)  + RCSKS  * V1(J,K)   


            ROLL(I)    = ROLL(I)  + RDELKS * RO(I,K)   
            RDELKS     = (PRSL(J,K)-PRSL(J,K+1)) * DELKS1(I)
            BNV2bar(I) = BNV2bar(I) + BNV2(I,K) * RDELKS
          ENDIF
        ENDDO
      ENDDO













      DO I = 1,npt
        J      = ipt(i)
        wdir   = atan2(UBAR(I),VBAR(I)) + pi
        idir   = mod(nint(fdir*wdir),mdir) + 1
        nwd    = nwdir(idir)
        OA(I)  = (1-2*INT( (NWD-1)/4 )) * OA4(J,MOD(NWD-1,4)+1)
        CLX(I) = CLX4(J,MOD(NWD-1,4)+1)
      ENDDO













      DO I = 1,npt
        XN(I)     = 0.0
        YN(I)     = 0.0
        TAUB (I)  = 0.0
        ULOW (I)  = 0.0
        DTFAC(I)  = 1.0
        ICRILV(I) = .FALSE. 



        ULOW(I) = MAX(SQRT(UBAR(I)*UBAR(I) + VBAR(I)*VBAR(I)), HONE)
        ULOI(I) = 1.0 / ULOW(I)
      ENDDO






      DO  K = 1,KMM1
        DO  I = 1,npt
          J            = ipt(i)



          VELCO(I,K)   = 0.5*rcs*((U1(J,K)+U1(J,K+1))*UBAR(I)            &
     &                       +  (V1(J,K)+V1(J,K+1))*VBAR(I))

          VELCO(I,K)   = VELCO(I,K) * ULOI(I)






        ENDDO
      ENDDO




      do i=1,npt
        kint(i) = km
      enddo
      do k = 1,kmm1
        do i = 1,npt
          IF (K .GT. kref(I)) THEN
            if(velco(i,k) .lt. veleps .and. kint(i) .eq. km) then
              kint(i) = k+1
            endif
          endif
        enddo
      enddo

      do i=1,npt
        kint(i) = kref(i)
      enddo


      DO I = 1,npt
        J      = ipt(i)
        BNV    = SQRT( BNV2bar(I) )
        FR     = BNV     * ULOI(I) * min(HPRIME(J),hpmax)
        FR     = MIN(FR, FRMAX)
        XN(I)  = UBAR(I) * ULOI(I)
        YN(I)  = VBAR(I) * ULOI(I)






        EFACT    = (OA(I) + 2.) ** (CEOFRC*FR)
        EFACT    = MIN( MAX(EFACT,EFMIN), EFMAX )

        COEFM    = (1. + CLX(I)) ** (OA(I)+1.)

        XLINV(I) = COEFM * CLEFF

        TEM      = FR    * FR * OC(J)
        GFOBNV   = GMAX  * TEM / ((TEM + CG)*BNV)  

        TAUB(I)  = XLINV(I) * ROLL(I) * ULOW(I) * ULOW(I)               &
     &           * ULOW(I)  * GFOBNV  * EFACT         




        K        = MAX(1, kref(I)-1)
        TEM      = MAX(VELCO(I,K)*VELCO(I,K), HE_4)
        SCOR(I)  = BNV2(I,K) / TEM  
      ENDDO    











      DO K = 1, KBPS
        DO I = 1,npt
          IF (K .LE. kref(I)) TAUP(I,K) = TAUB(I)
        ENDDO
      ENDDO




      DO K = KMPS, KMM1                   
        KP1 = K + 1
        DO I = 1, npt





          IF (K .GE. kref(I)) THEN
            ICRILV(I) = ICRILV(I) .OR. ( ri_n(I,K) .LT. RIC)            &
     &                            .OR. (VELCO(I,K) .LE. 0.0)
          ENDIF
        ENDDO

        DO I = 1,npt
          IF (K .GE. kref(I))   THEN







            IF (.NOT.ICRILV(I) .AND. TAUP(I,K) .GT. 0.0 ) THEN
              TEMV = 1.0 / max(VELCO(I,K), HE_2)

              IF (OA(I).GT.0. .AND. kp1 .lt. kint(i)) THEN
                SCORK   = BNV2(I,K) * TEMV * TEMV
                RSCOR   = MIN(HONE, SCORK / SCOR(I))
                SCOR(I) = SCORK
              ELSE 
                RSCOR   = 1.
              ENDIF

              BRVF = SQRT(BNV2(I,K))        

              TEM1 = XLINV(I)*(RO(I,KP1)+RO(I,K))*BRVF*0.5              &
     &                       * max(VELCO(I,K),HE_2)
              HD   = SQRT(TAUP(I,K) / TEM1)
              FRO  = BRVF * HD * TEMV



              TEM2   = SQRT(ri_n(I,K))
              TEM    = 1. + TEM2 * FRO
              RIM    = ri_n(I,K) * (1.-FRO) / (TEM * TEM)













              IF (RIM .LE. RIC .AND.                                    &

     &           (OA(I) .LE. 0. .OR.  kp1 .ge. kint(i) )) THEN
                 TEMC = 2.0 + 1.0 / TEM2
                 HD   = VELCO(I,K) * (2.*SQRT(TEMC)-TEMC) / BRVF
                 TAUP(I,KP1) = TEM1 * HD * HD
              ELSE 
                 TAUP(I,KP1) = TAUP(I,K) * RSCOR
              ENDIF
              taup(i,kp1) = min(taup(i,kp1), taup(i,k))





            ENDIF    
          ENDIF      
        ENDDO        
      ENDDO          





      IF(LCAP .LE. KM) THEN



         DO KLCAP = LCAPP1, KM+1
            DO I = 1,npt
              SIRA          = PRSI(ipt(I),KLCAP) / PRSI(ipt(I),LCAP)
              TAUP(I,KLCAP) = SIRA * TAUP(I,LCAP)








            ENDDO
         ENDDO
      ENDIF



      DO I=1,npt




        SCOR(I) = 1.0/RCS

      ENDDO
      DO K = 1,KM
        DO I = 1,npt
          TAUD(I,K) = G * (TAUP(I,K+1) - TAUP(I,K)) * SCOR(I)           &
     &                                              / DEL(ipt(I),K)






        ENDDO
      ENDDO





      DO KLCAP = LCAP, KM
         DO I = 1,npt
            TAUD(I,KLCAP) = TAUD(I,KLCAP) * FACTOP




         ENDDO
      ENDDO





      DO K = 1,KMM1
        DO I = 1,npt
           IF (K .GT. kref(I) .and. PRSI(ipt(i),K) .GE. RLOLEV) THEN
             IF(TAUD(I,K).NE.0.) THEN



               TEM = rcs*DELTIM * TAUD(I,K)

               DTFAC(I) = MIN(DTFAC(I),ABS(VELCO(I,K)/TEM))





             ENDIF
           ENDIF
        ENDDO
      ENDDO











      DO K = 1,KM
        DO I = 1,npt
          J          = ipt(i)
          TAUD(I,K)  = TAUD(I,K) * DTFAC(I)
          DTAUX      = TAUD(I,K) * XN(I)
          DTAUY      = TAUD(I,K) * YN(I)

          if ( K .lt. IDXZB(I) .AND. IDXZB(I) .ne. 0 ) then
            DBIM = DB(I,K) / (1.+DB(I,K)*DELTIM)
            A(J,K)  = - DBIM * V1(J,K) + A(J,K)
            B(J,K)  = - DBIM * U1(J,K) + B(J,K)
            DUsfc(J)   = DUsfc(J) - DBIM * V1(J,K) * DEL(J,K)
            DVsfc(J)   = DVsfc(J) - DBIM * U1(J,K) * DEL(J,K)







          else

            A(J,K)     = DTAUY     + A(J,K)
            B(J,K)     = DTAUX     + B(J,K)
            DUsfc(J)   = DUsfc(J)  + DTAUX * DEL(J,K)
            DVsfc(J)   = DVsfc(J)  + DTAUY * DEL(J,K)





          endif





        ENDDO      
      ENDDO        





      DO I = 1,npt
        J          = ipt(i)




        TEM    =  -1.E3*ROG*rcs
        DUsfc(J) = TEM * DUsfc(J)
        DVsfc(J) = TEM * DVsfc(J)





      ENDDO

      END SUBROUTINE GWD_col



      END MODULE module_gwd
