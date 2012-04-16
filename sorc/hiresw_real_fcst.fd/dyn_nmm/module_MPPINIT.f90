





      MODULE MODULE_MPPINIT


      USE MODULE_MPP


      CONTAINS


      SUBROUTINE MPPINIT(IDS,IDE,JDS,JDE,KDS,KDE                       &
                        ,IMS,IME,JMS,JME,KMS,KME                       &
                        ,IPS,IPE,JPS,JPE,KPS,KPE)






































      IMPLICIT NONE


      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
                           ,IMS,IME,JMS,JME,KMS,KME                    &
                           ,IPS,IPE,JPS,JPE,KPS,KPE




      CALL wrf_get_myproc ( MYPE )
      CALL wrf_get_nproc  ( NPES )
      CALL wrf_get_dm_communicator ( mpi_comm_comp )


      MYIS    = MAX( IPS-0, IDS+0 )
      MYIS_P1 = MAX( IPS-1, IDS+0 )
      MYIS_P2 = MAX( IPS-2, IDS+0 )
      MYIS_P3 = MAX( IPS-3, IDS+0 )
      MYIS_P4 = MAX( IPS-4, IDS+0 )
      MYIS_P5 = MAX( IPS-5, IDS+0 )

      MYIS1   = MAX( IPS-0, IDS+1 )
      MYIS1_P1= MAX( IPS-1, IDS+1 )
      MYIS1_P2= MAX( IPS-2, IDS+1 )
      MYIS1_P3= MAX( IPS-3, IDS+1 )
      MYIS1_P4= MAX( IPS-4, IDS+1 )
      MYIS1_P5= MAX( IPS-5, IDS+1 )

      MYIS2   = MAX( IPS-0, IDS+2 )
      MYIS2_P1= MAX( IPS-1, IDS+2 )
      MYIS2_P2= MAX( IPS-2, IDS+2 )
      MYIS2_P3= MAX( IPS-3, IDS+2 )
      MYIS2_P4= MAX( IPS-4, IDS+2 )
      MYIS2_P5= MAX( IPS-5, IDS+2 )

      MYIS3   = MAX( IPS-0, IDS+3 )
      MYIS3_P1= MAX( IPS-1, IDS+3 )
      MYIS3_P2= MAX( IPS-2, IDS+3 )
      MYIS3_P3= MAX( IPS-3, IDS+3 )
      MYIS3_P4= MAX( IPS-4, IDS+3 )
      MYIS3_P5= MAX( IPS-5, IDS+3 )

      MYIS4   = MAX( IPS-0, IDS+4 )
      MYIS4_P1= MAX( IPS-1, IDS+4 )
      MYIS4_P2= MAX( IPS-2, IDS+4 )
      MYIS4_P3= MAX( IPS-3, IDS+4 )
      MYIS4_P4= MAX( IPS-4, IDS+4 )
      MYIS4_P5= MAX( IPS-5, IDS+4 )

      MYIS5   = MAX( IPS-0, IDS+5 )
      MYIS5_P1= MAX( IPS-1, IDS+5 )
      MYIS5_P2= MAX( IPS-2, IDS+5 )
      MYIS5_P3= MAX( IPS-3, IDS+5 )
      MYIS5_P4= MAX( IPS-4, IDS+5 )
      MYIS5_P5= MAX( IPS-5, IDS+5 )


      MYIE    = MIN( IPE+0, IDE-0 )
      MYIE_P1 = MIN( IPE+1, IDE-0 )
      MYIE_P2 = MIN( IPE+2, IDE-0 )
      MYIE_P3 = MIN( IPE+3, IDE-0 )
      MYIE_P4 = MIN( IPE+4, IDE-0 )
      MYIE_P5 = MIN( IPE+5, IDE-0 )

      MYIE1   = MIN( IPE+0, IDE-1 )
      MYIE1_P1= MIN( IPE+1, IDE-1 )
      MYIE1_P2= MIN( IPE+2, IDE-1 )
      MYIE1_P3= MIN( IPE+3, IDE-1 )
      MYIE1_P4= MIN( IPE+4, IDE-1 )
      MYIE1_P5= MIN( IPE+5, IDE-1 )

      MYIE2   = MIN( IPE+0, IDE-2 )
      MYIE2_P1= MIN( IPE+1, IDE-2 )
      MYIE2_P2= MIN( IPE+2, IDE-2 )
      MYIE2_P3= MIN( IPE+3, IDE-2 )
      MYIE2_P4= MIN( IPE+4, IDE-2 )
      MYIE2_P5= MIN( IPE+5, IDE-2 )

      MYIE3   = MIN( IPE+0, IDE-3 )
      MYIE3_P1= MIN( IPE+1, IDE-3 )
      MYIE3_P2= MIN( IPE+2, IDE-3 )
      MYIE3_P3= MIN( IPE+3, IDE-3 )
      MYIE3_P4= MIN( IPE+4, IDE-3 )
      MYIE3_P5= MIN( IPE+5, IDE-3 )

      MYIE4   = MIN( IPE+0, IDE-4 )
      MYIE4_P1= MIN( IPE+1, IDE-4 )
      MYIE4_P2= MIN( IPE+2, IDE-4 )
      MYIE4_P3= MIN( IPE+3, IDE-4 )
      MYIE4_P4= MIN( IPE+4, IDE-4 )
      MYIE4_P5= MIN( IPE+5, IDE-4 )

      MYIE5   = MIN( IPE+0, IDE-5 )
      MYIE5_P1= MIN( IPE+1, IDE-5 )
      MYIE5_P2= MIN( IPE+2, IDE-5 )
      MYIE5_P3= MIN( IPE+3, IDE-5 )
      MYIE5_P4= MIN( IPE+4, IDE-5 )
      MYIE5_P5= MIN( IPE+5, IDE-5 )


      MYJS    = MAX( JPS-0, JDS+0 )
      MYJS_P1 = MAX( JPS-1, JDS+0 )
      MYJS_P2 = MAX( JPS-2, JDS+0 )
      MYJS_P3 = MAX( JPS-3, JDS+0 )
      MYJS_P4 = MAX( JPS-4, JDS+0 )
      MYJS_P5 = MAX( JPS-5, JDS+0 )

      MYJS1   = MAX( JPS-0, JDS+1 )
      MYJS1_P1= MAX( JPS-1, JDS+1 )
      MYJS1_P2= MAX( JPS-2, JDS+1 )
      MYJS1_P3= MAX( JPS-3, JDS+1 )
      MYJS1_P4= MAX( JPS-4, JDS+1 )
      MYJS1_P5= MAX( JPS-5, JDS+1 )

      MYJS2   = MAX( JPS-0, JDS+2 )
      MYJS2_P1= MAX( JPS-1, JDS+2 )
      MYJS2_P2= MAX( JPS-2, JDS+2 )
      MYJS2_P3= MAX( JPS-3, JDS+2 )
      MYJS2_P4= MAX( JPS-4, JDS+2 )
      MYJS2_P5= MAX( JPS-5, JDS+2 )

      MYJS3   = MAX( JPS-0, JDS+3 )
      MYJS3_P1= MAX( JPS-1, JDS+3 )
      MYJS3_P2= MAX( JPS-2, JDS+3 )
      MYJS3_P3= MAX( JPS-3, JDS+3 )
      MYJS3_P4= MAX( JPS-4, JDS+3 )
      MYJS3_P5= MAX( JPS-5, JDS+3 )

      MYJS4   = MAX( JPS-0, JDS+4 )
      MYJS4_P1= MAX( JPS-1, JDS+4 )
      MYJS4_P2= MAX( JPS-2, JDS+4 )
      MYJS4_P3= MAX( JPS-3, JDS+4 )
      MYJS4_P4= MAX( JPS-4, JDS+4 )
      MYJS4_P5= MAX( JPS-5, JDS+4 )

      MYJS5   = MAX( JPS-0, JDS+5 )
      MYJS5_P1= MAX( JPS-1, JDS+5 )
      MYJS5_P2= MAX( JPS-2, JDS+5 )
      MYJS5_P3= MAX( JPS-3, JDS+5 )
      MYJS5_P4= MAX( JPS-4, JDS+5 )
      MYJS5_P5= MAX( JPS-5, JDS+5 )


      MYJE    = MIN( JPE+0, JDE-0 )
      MYJE_P1 = MIN( JPE+1, JDE-0 )
      MYJE_P2 = MIN( JPE+2, JDE-0 )
      MYJE_P3 = MIN( JPE+3, JDE-0 )
      MYJE_P4 = MIN( JPE+4, JDE-0 )
      MYJE_P5 = MIN( JPE+5, JDE-0 )

      MYJE1   = MIN( JPE+0, JDE-1 )
      MYJE1_P1= MIN( JPE+1, JDE-1 )
      MYJE1_P2= MIN( JPE+2, JDE-1 )
      MYJE1_P3= MIN( JPE+3, JDE-1 )
      MYJE1_P4= MIN( JPE+4, JDE-1 )
      MYJE1_P5= MIN( JPE+5, JDE-1 )

      MYJE2   = MIN( JPE+0, JDE-2 )
      MYJE2_P1= MIN( JPE+1, JDE-2 )
      MYJE2_P2= MIN( JPE+2, JDE-2 )
      MYJE2_P3= MIN( JPE+3, JDE-2 )
      MYJE2_P4= MIN( JPE+4, JDE-2 )
      MYJE2_P5= MIN( JPE+5, JDE-2 )

      MYJE3   = MIN( JPE+0, JDE-3 )
      MYJE3_P1= MIN( JPE+1, JDE-3 )
      MYJE3_P2= MIN( JPE+2, JDE-3 )
      MYJE3_P3= MIN( JPE+3, JDE-3 )
      MYJE3_P4= MIN( JPE+4, JDE-3 )
      MYJE3_P5= MIN( JPE+5, JDE-3 )

      MYJE4   = MIN( JPE+0, JDE-4 )
      MYJE4_P1= MIN( JPE+1, JDE-4 )
      MYJE4_P2= MIN( JPE+2, JDE-4 )
      MYJE4_P3= MIN( JPE+3, JDE-4 )
      MYJE4_P4= MIN( JPE+4, JDE-4 )
      MYJE4_P5= MIN( JPE+5, JDE-4 )

      MYJE5   = MIN( JPE+0, JDE-5 )
      MYJE5_P1= MIN( JPE+1, JDE-5 )
      MYJE5_P2= MIN( JPE+2, JDE-5 )
      MYJE5_P3= MIN( JPE+3, JDE-5 )
      MYJE5_P4= MIN( JPE+4, JDE-5 )
      MYJE5_P5= MIN( JPE+5, JDE-5 )


      END SUBROUTINE MPPINIT









































































































































































































      END MODULE MODULE_MPPINIT

