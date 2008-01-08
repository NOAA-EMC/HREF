!STARTOFREGISTRYGENERATEDINCLUDE 'inc/scalar_indices.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
  P_qv = 1 ; F_qv = .FALSE. 
  P_qc = 1 ; F_qc = .FALSE. 
  P_qr = 1 ; F_qr = .FALSE. 
  P_qi = 1 ; F_qi = .FALSE. 
  P_qs = 1 ; F_qs = .FALSE. 
  P_qg = 1 ; F_qg = .FALSE. 
  P_qni = 1 ; F_qni = .FALSE. 
  IF (model_config_rec%dyn_opt==0)THEN
  END IF
  IF (model_config_rec%dyn_opt==4)THEN
  END IF
  IF (model_config_rec%dyn_opt==5)THEN
  END IF
  IF (model_config_rec%mp_physics(idomain)==0)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_stream_table( idomain, P_qv ) = 33554432
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   F_qv = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==1)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_stream_table( idomain, P_qv ) = 33554432
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_stream_table( idomain, P_qc ) = 33554432
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_stream_table( idomain, P_qr ) = 33554432
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   F_qr = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==2)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_stream_table( idomain, P_qv ) = 33554432
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_stream_table( idomain, P_qc ) = 33554432
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_stream_table( idomain, P_qr ) = 33554432
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_stream_table( idomain, P_qi ) = 33554432
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_stream_table( idomain, P_qs ) = 33554432
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   F_qs = .TRUE.
   IF ( moist_index_table( PARAM_qg , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qg = moist_num_table(idomain)
     moist_index_table( PARAM_qg , idomain ) = P_qg
   ELSE
     P_qg = moist_index_table( PARAM_qg , idomain )
   END IF
   moist_stream_table( idomain, P_qg ) = 33554432
   moist_dname_table( idomain, P_qg ) = 'QGRAUP'
   moist_desc_table( idomain, P_qg ) = 'Graupel mixing ratio'
   moist_units_table( idomain, P_qg ) = 'kg kg-1'
   F_qg = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==3)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_stream_table( idomain, P_qv ) = 33554432
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_stream_table( idomain, P_qc ) = 33554432
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_stream_table( idomain, P_qr ) = 33554432
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   F_qr = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==4)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_stream_table( idomain, P_qv ) = 33554432
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_stream_table( idomain, P_qc ) = 33554432
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_stream_table( idomain, P_qr ) = 33554432
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_stream_table( idomain, P_qi ) = 33554432
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_stream_table( idomain, P_qs ) = 33554432
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   F_qs = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==5)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_stream_table( idomain, P_qv ) = 33554432
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_stream_table( idomain, P_qc ) = 33554432
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_stream_table( idomain, P_qr ) = 33554432
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_stream_table( idomain, P_qs ) = 33554432
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   F_qs = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==6)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_stream_table( idomain, P_qv ) = 33554432
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_stream_table( idomain, P_qc ) = 33554432
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_stream_table( idomain, P_qr ) = 33554432
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_stream_table( idomain, P_qi ) = 33554432
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_stream_table( idomain, P_qs ) = 33554432
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   F_qs = .TRUE.
   IF ( moist_index_table( PARAM_qg , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qg = moist_num_table(idomain)
     moist_index_table( PARAM_qg , idomain ) = P_qg
   ELSE
     P_qg = moist_index_table( PARAM_qg , idomain )
   END IF
   moist_stream_table( idomain, P_qg ) = 33554432
   moist_dname_table( idomain, P_qg ) = 'QGRAUP'
   moist_desc_table( idomain, P_qg ) = 'Graupel mixing ratio'
   moist_units_table( idomain, P_qg ) = 'kg kg-1'
   F_qg = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==98)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_stream_table( idomain, P_qv ) = 33554432
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_stream_table( idomain, P_qc ) = 33554432
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_stream_table( idomain, P_qr ) = 33554432
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   F_qr = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==99)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_stream_table( idomain, P_qv ) = 33554432
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_stream_table( idomain, P_qc ) = 33554432
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_stream_table( idomain, P_qr ) = 33554432
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_stream_table( idomain, P_qi ) = 33554432
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_stream_table( idomain, P_qs ) = 33554432
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   F_qs = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==8)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_stream_table( idomain, P_qv ) = 33554432
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_stream_table( idomain, P_qc ) = 33554432
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_stream_table( idomain, P_qr ) = 33554432
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_stream_table( idomain, P_qi ) = 33554432
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_stream_table( idomain, P_qs ) = 33554432
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   F_qs = .TRUE.
   IF ( moist_index_table( PARAM_qg , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qg = moist_num_table(idomain)
     moist_index_table( PARAM_qg , idomain ) = P_qg
   ELSE
     P_qg = moist_index_table( PARAM_qg , idomain )
   END IF
   moist_stream_table( idomain, P_qg ) = 33554432
   moist_dname_table( idomain, P_qg ) = 'QGRAUP'
   moist_desc_table( idomain, P_qg ) = 'Graupel mixing ratio'
   moist_units_table( idomain, P_qg ) = 'kg kg-1'
   F_qg = .TRUE.
   IF ( scalar_index_table( PARAM_qni , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qni = scalar_num_table(idomain)
     scalar_index_table( PARAM_qni , idomain ) = P_qni
   ELSE
     P_qni = scalar_index_table( PARAM_qni , idomain )
   END IF
   scalar_stream_table( idomain, P_qni ) = 2113953792
   scalar_dname_table( idomain, P_qni ) = 'QNI'
   scalar_desc_table( idomain, P_qni ) = 'Ice Number concentration'
   scalar_units_table( idomain, P_qni ) = '  kg(-1)'
   F_qni = .TRUE.
  END IF
  IF (model_config_rec%ra_lw_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%ra_lw_physics(idomain)==3)THEN
  END IF
  IF (model_config_rec%ra_lw_physics(idomain)==99)THEN
  END IF
  IF (model_config_rec%ra_sw_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%ra_sw_physics(idomain)==2)THEN
  END IF
  IF (model_config_rec%ra_sw_physics(idomain)==3)THEN
  END IF
  IF (model_config_rec%ra_sw_physics(idomain)==99)THEN
  END IF
  IF (model_config_rec%sf_sfclay_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%sf_sfclay_physics(idomain)==2)THEN
  END IF
  IF (model_config_rec%sf_sfclay_physics(idomain)==3)THEN
  END IF
  IF (model_config_rec%sf_surface_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%sf_surface_physics(idomain)==2)THEN
  END IF
  IF (model_config_rec%sf_surface_physics(idomain)==3)THEN
  END IF
  IF (model_config_rec%sf_surface_physics(idomain)==99)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==2)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==3)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==99)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==2)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==3)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==4)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==99)THEN
  END IF
  IF (model_config_rec%io_form_restart==1)THEN
  END IF
  IF (model_config_rec%io_form_restart==2)THEN
  END IF
  IF (model_config_rec%io_form_restart==3)THEN
  END IF
  IF (model_config_rec%io_form_restart==4)THEN
  END IF
  IF (model_config_rec%io_form_restart==5)THEN
  END IF
  IF (model_config_rec%io_form_restart==6)THEN
  END IF
  IF (model_config_rec%io_form_restart==7)THEN
  END IF
  IF (model_config_rec%io_form_restart==8)THEN
  END IF
  IF (model_config_rec%io_form_restart==9)THEN
  END IF
  IF (model_config_rec%io_form_restart==10)THEN
  END IF
  IF (model_config_rec%io_form_restart==11)THEN
  END IF
!ENDOFREGISTRYGENERATEDINCLUDE
