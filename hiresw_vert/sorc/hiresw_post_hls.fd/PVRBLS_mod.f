      module pvrbls
      real, allocatable ::
     & Z0    (:,:),AKMS  (:,:),AKHS  (:,:),THS   (:,:),QS(:,:)
     &,UZ0   (:,:),VZ0   (:,:),THZ0  (:,:),QZ0   (:,:)
     &,RF    (:,:),TWBS  (:,:),QWBS  (:,:)
     &,SNO   (:,:),SI   (:,:),CLDEFI(:,:)
     &,PREC  (:,:),ACPREC(:,:),ACCLIQ(:,:),CUPREC(:,:)
     &,Q2(:,:,:)
      end module pvrbls
C-----------------------------------------------------------------------
C                            C O M M O N /PVRBLS/
C    & Z0    (IM,JM),AKMS  (IM,JM),AKHS  (IM,JM),THS   (IM,JM),QS(IM,JM)
C    &,UZ0   (IM,JM),VZ0   (IM,JM),THZ0  (IM,JM),QZ0   (IM,JM)
C    &,RF    (IM,JM),TWBS  (IM,JM),QWBS  (IM,JM)
C    &,SNO   (IM,JM),WET   (IM,JM),CLDEFI(IM,JM)
C    &,PREC  (IM,JM),ACPREC(IM,JM),ACCLIQ(IM,JM),CUPREC(IM,JM)
C    &,Q2(IM,JM,LM)
C-----------------------------------------------------------------------
