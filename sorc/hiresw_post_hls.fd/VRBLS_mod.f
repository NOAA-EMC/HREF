C   01-10-22  H CHUANG - MODIFIED TO PROCESS HYBRID MODEL OUTPUT
      module vrbls
      real, allocatable :: PD(:,:),RES(:,:),FIS(:,:)
     &,U(:,:,:),V(:,:,:),T(:,:,:),Q(:,:,:),W(:,:,:)
     &,DWDT(:,:,:)
      end module vrbls
C
C                            C O M M O N /VRBLS/
C    & PD(IM,JM),RES(IM,JM),FIS(IM,JM)
C
C    &,U(IM,JM,LM),V(IM,JM,LM),T(IM,JM,LM),Q(IM,JM,LM)
