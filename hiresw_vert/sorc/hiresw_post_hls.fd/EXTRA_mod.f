      module extra
      real, allocatable :: PINT(:,:,:),ALPINT(:,:,:),ZINT(:,:,:)
     &,KMNTM(:),KMNT(:,:),SLP(:,:)
     &,T500(:,:),PDSL(:,:),PDVP1(:,:),PSLP(:,:)
     &,PTSL(:,:),PFSL(:,:)
     &,TSL(:,:),TTV(:,:),FSL(:,:)
     &,QSL(:,:),Z1000(:,:)
      
      logical OLDRD,STDRD
      end module extra
C
c     PARAMETER (IMJM=IM*JM-JM/2)
c     COMMON /EXTRA/
c    & PINT(IM,JM,LP1),ALPINT(IM,JM,LP1),ZINT(IM,JM,LP1)
c    &,KMNTM(LM),KMNT(IMJM,LM),SLP(IM,JM)
c    &,T500(IM,JM),PDSL(IM,JM),PDVP1(IM,JM),PSLP(IM,JM)
c    &,PTSL(IM,JM),PFSL(IM,JM)
c    &,TSL(IM,JM),TTV(IM,JM),FSL(IM,JM)
c    &,QSL(IM,JM),Z1000(IM,JM)
c    &,OLDRD,STDRD
