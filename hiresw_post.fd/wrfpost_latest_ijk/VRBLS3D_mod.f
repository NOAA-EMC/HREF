C   01-10-22  H CHUANG - MODIFIED TO PROCESS HYBRID MODEL OUTPUT
C   02-04-17  BALDWIN  - MODIFIED TO INCLUDE ALL 3D ARRAYS
      module vrbls3d
      real, allocatable :: UH(:,:,:),VH(:,:,:),WH(:,:,:)
     &,U(:,:,:),V(:,:,:),T(:,:,:),Q(:,:,:)
     &,CWM(:,:,:),Q2(:,:,:),PMID(:,:,:),PMIDV(:,:,:)
     &,PINT(:,:,:),ALPINT(:,:,:),ZMID(:,:,:)
     &,ZINT(:,:,:),OMGA(:,:,:)
     &,TH(:,:,:),T_ADJ(:,:,:)
     &,F_ice(:,:,:),F_rain(:,:,:),F_RimeF(:,:,:)
     &,QQW(:,:,:), QQI(:,:,:), QQR(:,:,:), QQS(:,:,:), QQG(:,:,:)
     &,CFR(:,:,:), DBZ(:,:,:), DBZR(:,:,:), DBZI(:,:,:), DBZC(:,:,:)
     &,TTND(:,:,:),RSWTT(:,:,:),RLWTT(:,:,:)
     &,EXCH_H(:,:,:),TRAIN(:,:,:),TCUCN(:,:,:),EL_MYJ(:,:,:)
     &,MCVG(:,:,:)
      end module vrbls3d
