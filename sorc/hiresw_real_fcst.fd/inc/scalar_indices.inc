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
  P_dfi_qv = 1 ; F_dfi_qv = .FALSE. 
  P_dfi_qc = 1 ; F_dfi_qc = .FALSE. 
  P_dfi_qr = 1 ; F_dfi_qr = .FALSE. 
  P_dfi_qi = 1 ; F_dfi_qi = .FALSE. 
  P_dfi_qs = 1 ; F_dfi_qs = .FALSE. 
  P_dfi_qg = 1 ; F_dfi_qg = .FALSE. 
  P_qni = 1 ; F_qni = .FALSE. 
  P_qns = 1 ; F_qns = .FALSE. 
  P_qnr = 1 ; F_qnr = .FALSE. 
  P_qng = 1 ; F_qng = .FALSE. 
  P_qnn = 1 ; F_qnn = .FALSE. 
  P_qnc = 1 ; F_qnc = .FALSE. 
  P_dfi_qndrop = 1 ; F_dfi_qndrop = .FALSE. 
  P_dfi_qni = 1 ; F_dfi_qni = .FALSE. 
  P_dfi_qt = 1 ; F_dfi_qt = .FALSE. 
  P_dfi_qns = 1 ; F_dfi_qns = .FALSE. 
  P_dfi_qnr = 1 ; F_dfi_qnr = .FALSE. 
  P_dfi_qng = 1 ; F_dfi_qng = .FALSE. 
  P_dfi_qnn = 1 ; F_dfi_qnn = .FALSE. 
  P_dfi_qnc = 1 ; F_dfi_qnc = .FALSE. 
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
  IF (model_config_rec%mp_physics(idomain)==7)THEN
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
   IF ( scalar_index_table( PARAM_qnr , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnr = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnr , idomain ) = P_qnr
   ELSE
     P_qnr = scalar_index_table( PARAM_qnr , idomain )
   END IF
   scalar_stream_table( idomain, P_qnr ) = 2113953792
   scalar_dname_table( idomain, P_qnr ) = 'QNR'
   scalar_desc_table( idomain, P_qnr ) = 'Rain Number concentration'
   scalar_units_table( idomain, P_qnr ) = '  kg(-1)'
   F_qnr = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==10)THEN
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
   IF ( scalar_index_table( PARAM_qns , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qns = scalar_num_table(idomain)
     scalar_index_table( PARAM_qns , idomain ) = P_qns
   ELSE
     P_qns = scalar_index_table( PARAM_qns , idomain )
   END IF
   scalar_stream_table( idomain, P_qns ) = 2113953792
   scalar_dname_table( idomain, P_qns ) = 'QNS'
   scalar_desc_table( idomain, P_qns ) = 'Snow Number concentration'
   scalar_units_table( idomain, P_qns ) = '  kg(-1)'
   F_qns = .TRUE.
   IF ( scalar_index_table( PARAM_qnr , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnr = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnr , idomain ) = P_qnr
   ELSE
     P_qnr = scalar_index_table( PARAM_qnr , idomain )
   END IF
   scalar_stream_table( idomain, P_qnr ) = 2113953792
   scalar_dname_table( idomain, P_qnr ) = 'QNR'
   scalar_desc_table( idomain, P_qnr ) = 'Rain Number concentration'
   scalar_units_table( idomain, P_qnr ) = '  kg(-1)'
   F_qnr = .TRUE.
   IF ( scalar_index_table( PARAM_qng , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qng = scalar_num_table(idomain)
     scalar_index_table( PARAM_qng , idomain ) = P_qng
   ELSE
     P_qng = scalar_index_table( PARAM_qng , idomain )
   END IF
   scalar_stream_table( idomain, P_qng ) = 2113953792
   scalar_dname_table( idomain, P_qng ) = 'QNG'
   scalar_desc_table( idomain, P_qng ) = 'Graupel Number concentration'
   scalar_units_table( idomain, P_qng ) = '  kg(-1)'
   F_qng = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==14)THEN
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
   IF ( scalar_index_table( PARAM_qnn , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnn = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnn , idomain ) = P_qnn
   ELSE
     P_qnn = scalar_index_table( PARAM_qnn , idomain )
   END IF
   scalar_stream_table( idomain, P_qnn ) = 2113953792
   scalar_dname_table( idomain, P_qnn ) = 'QNCCN'
   scalar_desc_table( idomain, P_qnn ) = 'CCN Number concentration'
   scalar_units_table( idomain, P_qnn ) = '  kg(-1)'
   F_qnn = .TRUE.
   IF ( scalar_index_table( PARAM_qnc , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnc = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnc , idomain ) = P_qnc
   ELSE
     P_qnc = scalar_index_table( PARAM_qnc , idomain )
   END IF
   scalar_stream_table( idomain, P_qnc ) = 2113953792
   scalar_dname_table( idomain, P_qnc ) = 'QNCLOUD'
   scalar_desc_table( idomain, P_qnc ) = 'cloud water Number concentration'
   scalar_units_table( idomain, P_qnc ) = '  kg(-1)'
   F_qnc = .TRUE.
   IF ( scalar_index_table( PARAM_qnr , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnr = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnr , idomain ) = P_qnr
   ELSE
     P_qnr = scalar_index_table( PARAM_qnr , idomain )
   END IF
   scalar_stream_table( idomain, P_qnr ) = 2113953792
   scalar_dname_table( idomain, P_qnr ) = 'QNR'
   scalar_desc_table( idomain, P_qnr ) = 'Rain Number concentration'
   scalar_units_table( idomain, P_qnr ) = '  kg(-1)'
   F_qnr = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==16)THEN
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
   IF ( scalar_index_table( PARAM_qnn , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnn = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnn , idomain ) = P_qnn
   ELSE
     P_qnn = scalar_index_table( PARAM_qnn , idomain )
   END IF
   scalar_stream_table( idomain, P_qnn ) = 2113953792
   scalar_dname_table( idomain, P_qnn ) = 'QNCCN'
   scalar_desc_table( idomain, P_qnn ) = 'CCN Number concentration'
   scalar_units_table( idomain, P_qnn ) = '  kg(-1)'
   F_qnn = .TRUE.
   IF ( scalar_index_table( PARAM_qnc , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnc = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnc , idomain ) = P_qnc
   ELSE
     P_qnc = scalar_index_table( PARAM_qnc , idomain )
   END IF
   scalar_stream_table( idomain, P_qnc ) = 2113953792
   scalar_dname_table( idomain, P_qnc ) = 'QNCLOUD'
   scalar_desc_table( idomain, P_qnc ) = 'cloud water Number concentration'
   scalar_units_table( idomain, P_qnc ) = '  kg(-1)'
   F_qnc = .TRUE.
   IF ( scalar_index_table( PARAM_qnr , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnr = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnr , idomain ) = P_qnr
   ELSE
     P_qnr = scalar_index_table( PARAM_qnr , idomain )
   END IF
   scalar_stream_table( idomain, P_qnr ) = 2113953792
   scalar_dname_table( idomain, P_qnr ) = 'QNR'
   scalar_desc_table( idomain, P_qnr ) = 'Rain Number concentration'
   scalar_units_table( idomain, P_qnr ) = '  kg(-1)'
   F_qnr = .TRUE.
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
  IF (model_config_rec%mp_physics_dfi(idomain)==-1)THEN
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==0)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qv ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   F_dfi_qv = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==1)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qv ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qc ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qr ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   F_dfi_qr = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==2)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qv ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qc ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qr ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qi , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qi = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qi , idomain ) = P_dfi_qi
   ELSE
     P_dfi_qi = dfi_moist_index_table( PARAM_dfi_qi , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qi ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qi ) = 'QICE'
   dfi_moist_desc_table( idomain, P_dfi_qi ) = 'Ice mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qi ) = 'kg kg-1'
   F_dfi_qi = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qs ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   F_dfi_qs = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qg , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qg = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qg , idomain ) = P_dfi_qg
   ELSE
     P_dfi_qg = dfi_moist_index_table( PARAM_dfi_qg , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qg ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qg ) = 'QGRAUP'
   dfi_moist_desc_table( idomain, P_dfi_qg ) = 'Graupel mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qg ) = 'kg kg-1'
   F_dfi_qg = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==3)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qv ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qc ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qr ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   F_dfi_qr = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==4)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qv ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qc ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qr ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qi , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qi = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qi , idomain ) = P_dfi_qi
   ELSE
     P_dfi_qi = dfi_moist_index_table( PARAM_dfi_qi , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qi ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qi ) = 'QICE'
   dfi_moist_desc_table( idomain, P_dfi_qi ) = 'Ice mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qi ) = 'kg kg-1'
   F_dfi_qi = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qs ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   F_dfi_qs = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==5)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qv ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qc ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qr ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qi , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qi = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qi , idomain ) = P_dfi_qi
   ELSE
     P_dfi_qi = dfi_moist_index_table( PARAM_dfi_qi , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qi ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qi ) = 'QICE'
   dfi_moist_desc_table( idomain, P_dfi_qi ) = 'Ice mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qi ) = 'kg kg-1'
   F_dfi_qi = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qs ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   F_dfi_qs = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qg , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qg = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qg , idomain ) = P_dfi_qg
   ELSE
     P_dfi_qg = dfi_moist_index_table( PARAM_dfi_qg , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qg ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qg ) = 'QGRAUP'
   dfi_moist_desc_table( idomain, P_dfi_qg ) = 'Graupel mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qg ) = 'kg kg-1'
   F_dfi_qg = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==6)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qv ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qc ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qr ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qi , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qi = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qi , idomain ) = P_dfi_qi
   ELSE
     P_dfi_qi = dfi_moist_index_table( PARAM_dfi_qi , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qi ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qi ) = 'QICE'
   dfi_moist_desc_table( idomain, P_dfi_qi ) = 'Ice mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qi ) = 'kg kg-1'
   F_dfi_qi = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qs ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   F_dfi_qs = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qg , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qg = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qg , idomain ) = P_dfi_qg
   ELSE
     P_dfi_qg = dfi_moist_index_table( PARAM_dfi_qg , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qg ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qg ) = 'QGRAUP'
   dfi_moist_desc_table( idomain, P_dfi_qg ) = 'Graupel mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qg ) = 'kg kg-1'
   F_dfi_qg = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==7)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qv ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qc ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qr ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qi , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qi = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qi , idomain ) = P_dfi_qi
   ELSE
     P_dfi_qi = dfi_moist_index_table( PARAM_dfi_qi , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qi ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qi ) = 'QICE'
   dfi_moist_desc_table( idomain, P_dfi_qi ) = 'Ice mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qi ) = 'kg kg-1'
   F_dfi_qi = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qs ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   F_dfi_qs = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qg , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qg = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qg , idomain ) = P_dfi_qg
   ELSE
     P_dfi_qg = dfi_moist_index_table( PARAM_dfi_qg , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qg ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qg ) = 'QGRAUP'
   dfi_moist_desc_table( idomain, P_dfi_qg ) = 'Graupel mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qg ) = 'kg kg-1'
   F_dfi_qg = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==8)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qv ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qc ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qr ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qi , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qi = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qi , idomain ) = P_dfi_qi
   ELSE
     P_dfi_qi = dfi_moist_index_table( PARAM_dfi_qi , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qi ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qi ) = 'QICE'
   dfi_moist_desc_table( idomain, P_dfi_qi ) = 'Ice mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qi ) = 'kg kg-1'
   F_dfi_qi = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qs ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   F_dfi_qs = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qg , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qg = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qg , idomain ) = P_dfi_qg
   ELSE
     P_dfi_qg = dfi_moist_index_table( PARAM_dfi_qg , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qg ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qg ) = 'QGRAUP'
   dfi_moist_desc_table( idomain, P_dfi_qg ) = 'Graupel mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qg ) = 'kg kg-1'
   F_dfi_qg = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qni , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qni = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qni , idomain ) = P_dfi_qni
   ELSE
     P_dfi_qni = dfi_scalar_index_table( PARAM_dfi_qni , idomain )
   END IF
   dfi_scalar_stream_table( idomain, P_dfi_qni ) = 2113929216
   dfi_scalar_dname_table( idomain, P_dfi_qni ) = 'DFI_QNICE'
   dfi_scalar_desc_table( idomain, P_dfi_qni ) = 'Ice Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qni ) = '  kg-1'
   F_dfi_qni = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnr , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnr = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnr , idomain ) = P_dfi_qnr
   ELSE
     P_dfi_qnr = dfi_scalar_index_table( PARAM_dfi_qnr , idomain )
   END IF
   dfi_scalar_stream_table( idomain, P_dfi_qnr ) = 2113929216
   dfi_scalar_dname_table( idomain, P_dfi_qnr ) = 'DFI_QNRAIN'
   dfi_scalar_desc_table( idomain, P_dfi_qnr ) = 'Rain Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qnr ) = '  kg(-1)'
   F_dfi_qnr = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==10)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qv ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qc ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qr ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qi , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qi = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qi , idomain ) = P_dfi_qi
   ELSE
     P_dfi_qi = dfi_moist_index_table( PARAM_dfi_qi , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qi ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qi ) = 'QICE'
   dfi_moist_desc_table( idomain, P_dfi_qi ) = 'Ice mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qi ) = 'kg kg-1'
   F_dfi_qi = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qs ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   F_dfi_qs = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qg , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qg = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qg , idomain ) = P_dfi_qg
   ELSE
     P_dfi_qg = dfi_moist_index_table( PARAM_dfi_qg , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qg ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qg ) = 'QGRAUP'
   dfi_moist_desc_table( idomain, P_dfi_qg ) = 'Graupel mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qg ) = 'kg kg-1'
   F_dfi_qg = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qni , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qni = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qni , idomain ) = P_dfi_qni
   ELSE
     P_dfi_qni = dfi_scalar_index_table( PARAM_dfi_qni , idomain )
   END IF
   dfi_scalar_stream_table( idomain, P_dfi_qni ) = 2113929216
   dfi_scalar_dname_table( idomain, P_dfi_qni ) = 'DFI_QNICE'
   dfi_scalar_desc_table( idomain, P_dfi_qni ) = 'Ice Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qni ) = '  kg-1'
   F_dfi_qni = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qns , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qns = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qns , idomain ) = P_dfi_qns
   ELSE
     P_dfi_qns = dfi_scalar_index_table( PARAM_dfi_qns , idomain )
   END IF
   dfi_scalar_stream_table( idomain, P_dfi_qns ) = 2113929216
   dfi_scalar_dname_table( idomain, P_dfi_qns ) = 'DFI_QNSNOW'
   dfi_scalar_desc_table( idomain, P_dfi_qns ) = 'Snow Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qns ) = '  kg(-1)'
   F_dfi_qns = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnr , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnr = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnr , idomain ) = P_dfi_qnr
   ELSE
     P_dfi_qnr = dfi_scalar_index_table( PARAM_dfi_qnr , idomain )
   END IF
   dfi_scalar_stream_table( idomain, P_dfi_qnr ) = 2113929216
   dfi_scalar_dname_table( idomain, P_dfi_qnr ) = 'DFI_QNRAIN'
   dfi_scalar_desc_table( idomain, P_dfi_qnr ) = 'Rain Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qnr ) = '  kg(-1)'
   F_dfi_qnr = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qng , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qng = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qng , idomain ) = P_dfi_qng
   ELSE
     P_dfi_qng = dfi_scalar_index_table( PARAM_dfi_qng , idomain )
   END IF
   dfi_scalar_stream_table( idomain, P_dfi_qng ) = 2113929216
   dfi_scalar_dname_table( idomain, P_dfi_qng ) = 'DFI_QNGRAUPEL'
   dfi_scalar_desc_table( idomain, P_dfi_qng ) = 'Graupel Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qng ) = '  kg(-1)'
   F_dfi_qng = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==14)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qv ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qc ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qr ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qi , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qi = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qi , idomain ) = P_dfi_qi
   ELSE
     P_dfi_qi = dfi_moist_index_table( PARAM_dfi_qi , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qi ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qi ) = 'QICE'
   dfi_moist_desc_table( idomain, P_dfi_qi ) = 'Ice mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qi ) = 'kg kg-1'
   F_dfi_qi = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qs ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   F_dfi_qs = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnn , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnn = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnn , idomain ) = P_dfi_qnn
   ELSE
     P_dfi_qnn = dfi_scalar_index_table( PARAM_dfi_qnn , idomain )
   END IF
   dfi_scalar_stream_table( idomain, P_dfi_qnn ) = 2113929216
   dfi_scalar_dname_table( idomain, P_dfi_qnn ) = 'DFI_QNCC'
   dfi_scalar_desc_table( idomain, P_dfi_qnn ) = 'CNN Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qnn ) = '  kg(-1)'
   F_dfi_qnn = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnc , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnc = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnc , idomain ) = P_dfi_qnc
   ELSE
     P_dfi_qnc = dfi_scalar_index_table( PARAM_dfi_qnc , idomain )
   END IF
   dfi_scalar_stream_table( idomain, P_dfi_qnc ) = 2113929216
   dfi_scalar_dname_table( idomain, P_dfi_qnc ) = 'DFI_QNCLOUD'
   dfi_scalar_desc_table( idomain, P_dfi_qnc ) = 'Cloud Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qnc ) = '  kg(-1)'
   F_dfi_qnc = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnr , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnr = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnr , idomain ) = P_dfi_qnr
   ELSE
     P_dfi_qnr = dfi_scalar_index_table( PARAM_dfi_qnr , idomain )
   END IF
   dfi_scalar_stream_table( idomain, P_dfi_qnr ) = 2113929216
   dfi_scalar_dname_table( idomain, P_dfi_qnr ) = 'DFI_QNRAIN'
   dfi_scalar_desc_table( idomain, P_dfi_qnr ) = 'Rain Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qnr ) = '  kg(-1)'
   F_dfi_qnr = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==16)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qv ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qc ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qr ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qi , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qi = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qi , idomain ) = P_dfi_qi
   ELSE
     P_dfi_qi = dfi_moist_index_table( PARAM_dfi_qi , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qi ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qi ) = 'QICE'
   dfi_moist_desc_table( idomain, P_dfi_qi ) = 'Ice mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qi ) = 'kg kg-1'
   F_dfi_qi = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qs ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   F_dfi_qs = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qg , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qg = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qg , idomain ) = P_dfi_qg
   ELSE
     P_dfi_qg = dfi_moist_index_table( PARAM_dfi_qg , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qg ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qg ) = 'QGRAUP'
   dfi_moist_desc_table( idomain, P_dfi_qg ) = 'Graupel mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qg ) = 'kg kg-1'
   F_dfi_qg = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnn , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnn = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnn , idomain ) = P_dfi_qnn
   ELSE
     P_dfi_qnn = dfi_scalar_index_table( PARAM_dfi_qnn , idomain )
   END IF
   dfi_scalar_stream_table( idomain, P_dfi_qnn ) = 2113929216
   dfi_scalar_dname_table( idomain, P_dfi_qnn ) = 'DFI_QNCC'
   dfi_scalar_desc_table( idomain, P_dfi_qnn ) = 'CNN Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qnn ) = '  kg(-1)'
   F_dfi_qnn = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnc , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnc = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnc , idomain ) = P_dfi_qnc
   ELSE
     P_dfi_qnc = dfi_scalar_index_table( PARAM_dfi_qnc , idomain )
   END IF
   dfi_scalar_stream_table( idomain, P_dfi_qnc ) = 2113929216
   dfi_scalar_dname_table( idomain, P_dfi_qnc ) = 'DFI_QNCLOUD'
   dfi_scalar_desc_table( idomain, P_dfi_qnc ) = 'Cloud Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qnc ) = '  kg(-1)'
   F_dfi_qnc = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnr , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnr = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnr , idomain ) = P_dfi_qnr
   ELSE
     P_dfi_qnr = dfi_scalar_index_table( PARAM_dfi_qnr , idomain )
   END IF
   dfi_scalar_stream_table( idomain, P_dfi_qnr ) = 2113929216
   dfi_scalar_dname_table( idomain, P_dfi_qnr ) = 'DFI_QNRAIN'
   dfi_scalar_desc_table( idomain, P_dfi_qnr ) = 'Rain Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qnr ) = '  kg(-1)'
   F_dfi_qnr = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==98)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qv ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qc ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qr ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qi , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qi = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qi , idomain ) = P_dfi_qi
   ELSE
     P_dfi_qi = dfi_moist_index_table( PARAM_dfi_qi , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qi ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qi ) = 'QICE'
   dfi_moist_desc_table( idomain, P_dfi_qi ) = 'Ice mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qi ) = 'kg kg-1'
   F_dfi_qi = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qs ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   F_dfi_qs = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qg , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qg = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qg , idomain ) = P_dfi_qg
   ELSE
     P_dfi_qg = dfi_moist_index_table( PARAM_dfi_qg , idomain )
   END IF
   dfi_moist_stream_table( idomain, P_dfi_qg ) = 33554432
   dfi_moist_dname_table( idomain, P_dfi_qg ) = 'QGRAUP'
   dfi_moist_desc_table( idomain, P_dfi_qg ) = 'Graupel mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qg ) = 'kg kg-1'
   F_dfi_qg = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qni , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qni = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qni , idomain ) = P_dfi_qni
   ELSE
     P_dfi_qni = dfi_scalar_index_table( PARAM_dfi_qni , idomain )
   END IF
   dfi_scalar_stream_table( idomain, P_dfi_qni ) = 2113929216
   dfi_scalar_dname_table( idomain, P_dfi_qni ) = 'DFI_QNICE'
   dfi_scalar_desc_table( idomain, P_dfi_qni ) = 'Ice Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qni ) = '  kg-1'
   F_dfi_qni = .TRUE.
  END IF
  IF (model_config_rec%ra_lw_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%ra_lw_physics(idomain)==3)THEN
  END IF
  IF (model_config_rec%ra_lw_physics(idomain)==4)THEN
  END IF
  IF (model_config_rec%ra_lw_physics(idomain)==99)THEN
  END IF
  IF (model_config_rec%ra_lw_physics(idomain)==31)THEN
  END IF
  IF (model_config_rec%ra_sw_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%ra_sw_physics(idomain)==2)THEN
  END IF
  IF (model_config_rec%ra_sw_physics(idomain)==3)THEN
  END IF
  IF (model_config_rec%ra_sw_physics(idomain)==4)THEN
  END IF
  IF (model_config_rec%ra_sw_physics(idomain)==99)THEN
  END IF
  IF (model_config_rec%sf_sfclay_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%sf_sfclay_physics(idomain)==2)THEN
  END IF
  IF (model_config_rec%sf_sfclay_physics(idomain)==3)THEN
  END IF
  IF (model_config_rec%sf_sfclay_physics(idomain)==88)THEN
  END IF
  IF (model_config_rec%sf_sfclay_physics(idomain)==4)THEN
  END IF
  IF (model_config_rec%sf_sfclay_physics(idomain)==7)THEN
  END IF
  IF (model_config_rec%sf_surface_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%sf_surface_physics(idomain)==2)THEN
  END IF
  IF (model_config_rec%sf_surface_physics(idomain)==3)THEN
  END IF
  IF (model_config_rec%sf_surface_physics(idomain)==88)THEN
  END IF
  IF (model_config_rec%sf_surface_physics(idomain)==7)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==2)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==3)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==4)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==7)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==8)THEN
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
  IF (model_config_rec%cu_physics(idomain)==5)THEN
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
  IF (model_config_rec%dfi_opt==0)THEN
  END IF
  IF (model_config_rec%dfi_opt==1)THEN
  END IF
  IF (model_config_rec%dfi_opt==2)THEN
  END IF
  IF (model_config_rec%dfi_opt==3)THEN
  END IF
  IF (model_config_rec%dfi_stage==0)THEN
  END IF
  IF (model_config_rec%dfi_stage==1)THEN
  END IF
  IF (model_config_rec%dfi_stage==2)THEN
  END IF
  IF (model_config_rec%dfi_stage==3)THEN
  END IF
!ENDOFREGISTRYGENERATEDINCLUDE
