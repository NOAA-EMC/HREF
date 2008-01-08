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
  P_qt = 1 ; F_qt = .FALSE. 
  P_mth01 = 1 ; F_mth01 = .FALSE. 
  P_mth02 = 1 ; F_mth02 = .FALSE. 
  P_mth03 = 1 ; F_mth03 = .FALSE. 
  P_mth04 = 1 ; F_mth04 = .FALSE. 
  P_mth05 = 1 ; F_mth05 = .FALSE. 
  P_mth06 = 1 ; F_mth06 = .FALSE. 
  P_mth07 = 1 ; F_mth07 = .FALSE. 
  P_mth08 = 1 ; F_mth08 = .FALSE. 
  P_mth09 = 1 ; F_mth09 = .FALSE. 
  P_mth10 = 1 ; F_mth10 = .FALSE. 
  P_mth11 = 1 ; F_mth11 = .FALSE. 
  P_mth12 = 1 ; F_mth12 = .FALSE. 
  P_sul = 1 ; F_sul = .FALSE. 
  P_sslt = 1 ; F_sslt = .FALSE. 
  P_dust1 = 1 ; F_dust1 = .FALSE. 
  P_dust2 = 1 ; F_dust2 = .FALSE. 
  P_dust3 = 1 ; F_dust3 = .FALSE. 
  P_dust4 = 1 ; F_dust4 = .FALSE. 
  P_ocpho = 1 ; F_ocpho = .FALSE. 
  P_bcpho = 1 ; F_bcpho = .FALSE. 
  P_ocphi = 1 ; F_ocphi = .FALSE. 
  P_bcphi = 1 ; F_bcphi = .FALSE. 
  P_bg = 1 ; F_bg = .FALSE. 
  P_volc = 1 ; F_volc = .FALSE. 
  P_u_ndg_new = 1 ; F_u_ndg_new = .FALSE. 
  P_v_ndg_new = 1 ; F_v_ndg_new = .FALSE. 
  P_t_ndg_new = 1 ; F_t_ndg_new = .FALSE. 
  P_q_ndg_new = 1 ; F_q_ndg_new = .FALSE. 
  P_ph_ndg_new = 1 ; F_ph_ndg_new = .FALSE. 
  P_u_ndg_old = 1 ; F_u_ndg_old = .FALSE. 
  P_v_ndg_old = 1 ; F_v_ndg_old = .FALSE. 
  P_t_ndg_old = 1 ; F_t_ndg_old = .FALSE. 
  P_q_ndg_old = 1 ; F_q_ndg_old = .FALSE. 
  P_ph_ndg_old = 1 ; F_ph_ndg_old = .FALSE. 
  P_mu_ndg_new = 1 ; F_mu_ndg_new = .FALSE. 
  P_mu_ndg_old = 1 ; F_mu_ndg_old = .FALSE. 
  IF (model_config_rec%dyn_opt==0)THEN
  END IF
  IF (model_config_rec%dyn_opt==2)THEN
  END IF
  IF (model_config_rec%mp_physics(idomain)==0)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_stream_table( idomain, P_qv ) = 2113953794
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
   moist_stream_table( idomain, P_qv ) = 2113953794
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
   moist_stream_table( idomain, P_qc ) = 2113953794
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
   moist_stream_table( idomain, P_qr ) = 2113953794
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
   moist_stream_table( idomain, P_qv ) = 2113953794
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
   moist_stream_table( idomain, P_qc ) = 2113953794
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
   moist_stream_table( idomain, P_qr ) = 2113953794
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
   moist_stream_table( idomain, P_qi ) = 2113953794
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
   moist_stream_table( idomain, P_qs ) = 2113953794
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
   moist_stream_table( idomain, P_qg ) = 2113953794
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
   moist_stream_table( idomain, P_qv ) = 2113953794
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
   moist_stream_table( idomain, P_qc ) = 2113953794
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
   moist_stream_table( idomain, P_qr ) = 2113953794
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
   moist_stream_table( idomain, P_qv ) = 2113953794
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
   moist_stream_table( idomain, P_qc ) = 2113953794
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
   moist_stream_table( idomain, P_qr ) = 2113953794
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
   moist_stream_table( idomain, P_qi ) = 2113953794
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
   moist_stream_table( idomain, P_qs ) = 2113953794
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
   moist_stream_table( idomain, P_qv ) = 2113953794
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
   moist_stream_table( idomain, P_qc ) = 2113953794
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
   moist_stream_table( idomain, P_qr ) = 2113953794
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
   moist_stream_table( idomain, P_qi ) = 2113953794
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
   moist_stream_table( idomain, P_qs ) = 2113953794
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
   moist_stream_table( idomain, P_qg ) = 2113953794
   moist_dname_table( idomain, P_qg ) = 'QGRAUP'
   moist_desc_table( idomain, P_qg ) = 'Graupel mixing ratio'
   moist_units_table( idomain, P_qg ) = 'kg kg-1'
   F_qg = .TRUE.
   IF ( scalar_index_table( PARAM_qt , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qt = scalar_num_table(idomain)
     scalar_index_table( PARAM_qt , idomain ) = P_qt
   ELSE
     P_qt = scalar_index_table( PARAM_qt , idomain )
   END IF
   scalar_stream_table( idomain, P_qt ) = 2113953794
   scalar_dname_table( idomain, P_qt ) = 'CWM'
   scalar_desc_table( idomain, P_qt ) = 'Total condensate mixing ratio'
   scalar_units_table( idomain, P_qt ) = 'kg kg-1'
   F_qt = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==6)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_stream_table( idomain, P_qv ) = 2113953794
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
   moist_stream_table( idomain, P_qc ) = 2113953794
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
   moist_stream_table( idomain, P_qr ) = 2113953794
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
   moist_stream_table( idomain, P_qi ) = 2113953794
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
   moist_stream_table( idomain, P_qs ) = 2113953794
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
   moist_stream_table( idomain, P_qg ) = 2113953794
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
   moist_stream_table( idomain, P_qv ) = 2113953794
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
   moist_stream_table( idomain, P_qc ) = 2113953794
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
   moist_stream_table( idomain, P_qr ) = 2113953794
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
   moist_stream_table( idomain, P_qv ) = 2113953794
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
   moist_stream_table( idomain, P_qc ) = 2113953794
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
   moist_stream_table( idomain, P_qr ) = 2113953794
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
   moist_stream_table( idomain, P_qi ) = 2113953794
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
   moist_stream_table( idomain, P_qs ) = 2113953794
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
   moist_stream_table( idomain, P_qv ) = 2113953794
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
   moist_stream_table( idomain, P_qc ) = 2113953794
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
   moist_stream_table( idomain, P_qr ) = 2113953794
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
   moist_stream_table( idomain, P_qi ) = 2113953794
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
   moist_stream_table( idomain, P_qs ) = 2113953794
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
   moist_stream_table( idomain, P_qg ) = 2113953794
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
   scalar_stream_table( idomain, P_qni ) = 2113953794
   scalar_dname_table( idomain, P_qni ) = 'QNICE'
   scalar_desc_table( idomain, P_qni ) = 'Ice Number concentration'
   scalar_units_table( idomain, P_qni ) = '  kg(-1)'
   F_qni = .TRUE.
  END IF
  IF (model_config_rec%ra_lw_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%ra_lw_physics(idomain)==3)THEN
   IF ( ozmixm_index_table( PARAM_mth01 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth01 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth01 , idomain ) = P_mth01
   ELSE
     P_mth01 = ozmixm_index_table( PARAM_mth01 , idomain )
   END IF
   ozmixm_stream_table( idomain, P_mth01 ) = 0
   ozmixm_dname_table( idomain, P_mth01 ) = ''
   ozmixm_desc_table( idomain, P_mth01 ) = '-'
   ozmixm_units_table( idomain, P_mth01 ) = '-'
   F_mth01 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth02 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth02 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth02 , idomain ) = P_mth02
   ELSE
     P_mth02 = ozmixm_index_table( PARAM_mth02 , idomain )
   END IF
   ozmixm_stream_table( idomain, P_mth02 ) = 0
   ozmixm_dname_table( idomain, P_mth02 ) = ''
   ozmixm_desc_table( idomain, P_mth02 ) = '-'
   ozmixm_units_table( idomain, P_mth02 ) = '-'
   F_mth02 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth03 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth03 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth03 , idomain ) = P_mth03
   ELSE
     P_mth03 = ozmixm_index_table( PARAM_mth03 , idomain )
   END IF
   ozmixm_stream_table( idomain, P_mth03 ) = 0
   ozmixm_dname_table( idomain, P_mth03 ) = ''
   ozmixm_desc_table( idomain, P_mth03 ) = '-'
   ozmixm_units_table( idomain, P_mth03 ) = '-'
   F_mth03 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth04 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth04 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth04 , idomain ) = P_mth04
   ELSE
     P_mth04 = ozmixm_index_table( PARAM_mth04 , idomain )
   END IF
   ozmixm_stream_table( idomain, P_mth04 ) = 0
   ozmixm_dname_table( idomain, P_mth04 ) = ''
   ozmixm_desc_table( idomain, P_mth04 ) = '-'
   ozmixm_units_table( idomain, P_mth04 ) = '-'
   F_mth04 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth05 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth05 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth05 , idomain ) = P_mth05
   ELSE
     P_mth05 = ozmixm_index_table( PARAM_mth05 , idomain )
   END IF
   ozmixm_stream_table( idomain, P_mth05 ) = 0
   ozmixm_dname_table( idomain, P_mth05 ) = ''
   ozmixm_desc_table( idomain, P_mth05 ) = '-'
   ozmixm_units_table( idomain, P_mth05 ) = '-'
   F_mth05 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth06 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth06 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth06 , idomain ) = P_mth06
   ELSE
     P_mth06 = ozmixm_index_table( PARAM_mth06 , idomain )
   END IF
   ozmixm_stream_table( idomain, P_mth06 ) = 0
   ozmixm_dname_table( idomain, P_mth06 ) = ''
   ozmixm_desc_table( idomain, P_mth06 ) = '-'
   ozmixm_units_table( idomain, P_mth06 ) = '-'
   F_mth06 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth07 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth07 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth07 , idomain ) = P_mth07
   ELSE
     P_mth07 = ozmixm_index_table( PARAM_mth07 , idomain )
   END IF
   ozmixm_stream_table( idomain, P_mth07 ) = 0
   ozmixm_dname_table( idomain, P_mth07 ) = ''
   ozmixm_desc_table( idomain, P_mth07 ) = '-'
   ozmixm_units_table( idomain, P_mth07 ) = '-'
   F_mth07 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth08 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth08 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth08 , idomain ) = P_mth08
   ELSE
     P_mth08 = ozmixm_index_table( PARAM_mth08 , idomain )
   END IF
   ozmixm_stream_table( idomain, P_mth08 ) = 0
   ozmixm_dname_table( idomain, P_mth08 ) = ''
   ozmixm_desc_table( idomain, P_mth08 ) = '-'
   ozmixm_units_table( idomain, P_mth08 ) = '-'
   F_mth08 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth09 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth09 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth09 , idomain ) = P_mth09
   ELSE
     P_mth09 = ozmixm_index_table( PARAM_mth09 , idomain )
   END IF
   ozmixm_stream_table( idomain, P_mth09 ) = 0
   ozmixm_dname_table( idomain, P_mth09 ) = ''
   ozmixm_desc_table( idomain, P_mth09 ) = '-'
   ozmixm_units_table( idomain, P_mth09 ) = '-'
   F_mth09 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth10 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth10 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth10 , idomain ) = P_mth10
   ELSE
     P_mth10 = ozmixm_index_table( PARAM_mth10 , idomain )
   END IF
   ozmixm_stream_table( idomain, P_mth10 ) = 0
   ozmixm_dname_table( idomain, P_mth10 ) = ''
   ozmixm_desc_table( idomain, P_mth10 ) = '-'
   ozmixm_units_table( idomain, P_mth10 ) = '-'
   F_mth10 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth11 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth11 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth11 , idomain ) = P_mth11
   ELSE
     P_mth11 = ozmixm_index_table( PARAM_mth11 , idomain )
   END IF
   ozmixm_stream_table( idomain, P_mth11 ) = 0
   ozmixm_dname_table( idomain, P_mth11 ) = ''
   ozmixm_desc_table( idomain, P_mth11 ) = '-'
   ozmixm_units_table( idomain, P_mth11 ) = '-'
   F_mth11 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth12 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth12 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth12 , idomain ) = P_mth12
   ELSE
     P_mth12 = ozmixm_index_table( PARAM_mth12 , idomain )
   END IF
   ozmixm_stream_table( idomain, P_mth12 ) = 0
   ozmixm_dname_table( idomain, P_mth12 ) = ''
   ozmixm_desc_table( idomain, P_mth12 ) = '-'
   ozmixm_units_table( idomain, P_mth12 ) = '-'
   F_mth12 = .TRUE.
   IF ( aerosolc_index_table( PARAM_sul , idomain ) .lt. 1 ) THEN
     aerosolc_num_table(idomain) = aerosolc_num_table(idomain) + 1
     P_sul = aerosolc_num_table(idomain)
     aerosolc_index_table( PARAM_sul , idomain ) = P_sul
   ELSE
     P_sul = aerosolc_index_table( PARAM_sul , idomain )
   END IF
   aerosolc_stream_table( idomain, P_sul ) = 0
   aerosolc_dname_table( idomain, P_sul ) = 'SUL'
   aerosolc_desc_table( idomain, P_sul ) = 'SUL aerosol concentration'
   aerosolc_units_table( idomain, P_sul ) = '-'
   F_sul = .TRUE.
   IF ( aerosolc_index_table( PARAM_sslt , idomain ) .lt. 1 ) THEN
     aerosolc_num_table(idomain) = aerosolc_num_table(idomain) + 1
     P_sslt = aerosolc_num_table(idomain)
     aerosolc_index_table( PARAM_sslt , idomain ) = P_sslt
   ELSE
     P_sslt = aerosolc_index_table( PARAM_sslt , idomain )
   END IF
   aerosolc_stream_table( idomain, P_sslt ) = 0
   aerosolc_dname_table( idomain, P_sslt ) = 'SSLT'
   aerosolc_desc_table( idomain, P_sslt ) = 'SSLT aerosol concentration'
   aerosolc_units_table( idomain, P_sslt ) = '-'
   F_sslt = .TRUE.
   IF ( aerosolc_index_table( PARAM_dust1 , idomain ) .lt. 1 ) THEN
     aerosolc_num_table(idomain) = aerosolc_num_table(idomain) + 1
     P_dust1 = aerosolc_num_table(idomain)
     aerosolc_index_table( PARAM_dust1 , idomain ) = P_dust1
   ELSE
     P_dust1 = aerosolc_index_table( PARAM_dust1 , idomain )
   END IF
   aerosolc_stream_table( idomain, P_dust1 ) = 0
   aerosolc_dname_table( idomain, P_dust1 ) = 'DUST1'
   aerosolc_desc_table( idomain, P_dust1 ) = 'DUST1 aerosol concentration'
   aerosolc_units_table( idomain, P_dust1 ) = '-'
   F_dust1 = .TRUE.
   IF ( aerosolc_index_table( PARAM_dust2 , idomain ) .lt. 1 ) THEN
     aerosolc_num_table(idomain) = aerosolc_num_table(idomain) + 1
     P_dust2 = aerosolc_num_table(idomain)
     aerosolc_index_table( PARAM_dust2 , idomain ) = P_dust2
   ELSE
     P_dust2 = aerosolc_index_table( PARAM_dust2 , idomain )
   END IF
   aerosolc_stream_table( idomain, P_dust2 ) = 0
   aerosolc_dname_table( idomain, P_dust2 ) = 'DUST2'
   aerosolc_desc_table( idomain, P_dust2 ) = 'DUST2 aerosol concentration'
   aerosolc_units_table( idomain, P_dust2 ) = '-'
   F_dust2 = .TRUE.
   IF ( aerosolc_index_table( PARAM_dust3 , idomain ) .lt. 1 ) THEN
     aerosolc_num_table(idomain) = aerosolc_num_table(idomain) + 1
     P_dust3 = aerosolc_num_table(idomain)
     aerosolc_index_table( PARAM_dust3 , idomain ) = P_dust3
   ELSE
     P_dust3 = aerosolc_index_table( PARAM_dust3 , idomain )
   END IF
   aerosolc_stream_table( idomain, P_dust3 ) = 0
   aerosolc_dname_table( idomain, P_dust3 ) = 'DUST3'
   aerosolc_desc_table( idomain, P_dust3 ) = 'DUST3 aerosol concentration'
   aerosolc_units_table( idomain, P_dust3 ) = '-'
   F_dust3 = .TRUE.
   IF ( aerosolc_index_table( PARAM_dust4 , idomain ) .lt. 1 ) THEN
     aerosolc_num_table(idomain) = aerosolc_num_table(idomain) + 1
     P_dust4 = aerosolc_num_table(idomain)
     aerosolc_index_table( PARAM_dust4 , idomain ) = P_dust4
   ELSE
     P_dust4 = aerosolc_index_table( PARAM_dust4 , idomain )
   END IF
   aerosolc_stream_table( idomain, P_dust4 ) = 0
   aerosolc_dname_table( idomain, P_dust4 ) = 'DUST4'
   aerosolc_desc_table( idomain, P_dust4 ) = 'DUST4 aerosol concentration'
   aerosolc_units_table( idomain, P_dust4 ) = '-'
   F_dust4 = .TRUE.
   IF ( aerosolc_index_table( PARAM_ocpho , idomain ) .lt. 1 ) THEN
     aerosolc_num_table(idomain) = aerosolc_num_table(idomain) + 1
     P_ocpho = aerosolc_num_table(idomain)
     aerosolc_index_table( PARAM_ocpho , idomain ) = P_ocpho
   ELSE
     P_ocpho = aerosolc_index_table( PARAM_ocpho , idomain )
   END IF
   aerosolc_stream_table( idomain, P_ocpho ) = 0
   aerosolc_dname_table( idomain, P_ocpho ) = 'OCPHO'
   aerosolc_desc_table( idomain, P_ocpho ) = 'OCPHO aerosol concentration'
   aerosolc_units_table( idomain, P_ocpho ) = '-'
   F_ocpho = .TRUE.
   IF ( aerosolc_index_table( PARAM_bcpho , idomain ) .lt. 1 ) THEN
     aerosolc_num_table(idomain) = aerosolc_num_table(idomain) + 1
     P_bcpho = aerosolc_num_table(idomain)
     aerosolc_index_table( PARAM_bcpho , idomain ) = P_bcpho
   ELSE
     P_bcpho = aerosolc_index_table( PARAM_bcpho , idomain )
   END IF
   aerosolc_stream_table( idomain, P_bcpho ) = 0
   aerosolc_dname_table( idomain, P_bcpho ) = 'BCPHO'
   aerosolc_desc_table( idomain, P_bcpho ) = 'BCPHO aerosol concentration'
   aerosolc_units_table( idomain, P_bcpho ) = '-'
   F_bcpho = .TRUE.
   IF ( aerosolc_index_table( PARAM_ocphi , idomain ) .lt. 1 ) THEN
     aerosolc_num_table(idomain) = aerosolc_num_table(idomain) + 1
     P_ocphi = aerosolc_num_table(idomain)
     aerosolc_index_table( PARAM_ocphi , idomain ) = P_ocphi
   ELSE
     P_ocphi = aerosolc_index_table( PARAM_ocphi , idomain )
   END IF
   aerosolc_stream_table( idomain, P_ocphi ) = 0
   aerosolc_dname_table( idomain, P_ocphi ) = 'OCPHI'
   aerosolc_desc_table( idomain, P_ocphi ) = 'OCPHI aerosol concentration'
   aerosolc_units_table( idomain, P_ocphi ) = '-'
   F_ocphi = .TRUE.
   IF ( aerosolc_index_table( PARAM_bcphi , idomain ) .lt. 1 ) THEN
     aerosolc_num_table(idomain) = aerosolc_num_table(idomain) + 1
     P_bcphi = aerosolc_num_table(idomain)
     aerosolc_index_table( PARAM_bcphi , idomain ) = P_bcphi
   ELSE
     P_bcphi = aerosolc_index_table( PARAM_bcphi , idomain )
   END IF
   aerosolc_stream_table( idomain, P_bcphi ) = 0
   aerosolc_dname_table( idomain, P_bcphi ) = 'BCPHI'
   aerosolc_desc_table( idomain, P_bcphi ) = 'BCPHI aerosol concentration'
   aerosolc_units_table( idomain, P_bcphi ) = '-'
   F_bcphi = .TRUE.
   IF ( aerosolc_index_table( PARAM_bg , idomain ) .lt. 1 ) THEN
     aerosolc_num_table(idomain) = aerosolc_num_table(idomain) + 1
     P_bg = aerosolc_num_table(idomain)
     aerosolc_index_table( PARAM_bg , idomain ) = P_bg
   ELSE
     P_bg = aerosolc_index_table( PARAM_bg , idomain )
   END IF
   aerosolc_stream_table( idomain, P_bg ) = 0
   aerosolc_dname_table( idomain, P_bg ) = 'BG'
   aerosolc_desc_table( idomain, P_bg ) = 'BG aerosol concentration'
   aerosolc_units_table( idomain, P_bg ) = '-'
   F_bg = .TRUE.
   IF ( aerosolc_index_table( PARAM_volc , idomain ) .lt. 1 ) THEN
     aerosolc_num_table(idomain) = aerosolc_num_table(idomain) + 1
     P_volc = aerosolc_num_table(idomain)
     aerosolc_index_table( PARAM_volc , idomain ) = P_volc
   ELSE
     P_volc = aerosolc_index_table( PARAM_volc , idomain )
   END IF
   aerosolc_stream_table( idomain, P_volc ) = 0
   aerosolc_dname_table( idomain, P_volc ) = 'VOLC'
   aerosolc_desc_table( idomain, P_volc ) = 'VOLC aerosol concentration'
   aerosolc_units_table( idomain, P_volc ) = '-'
   F_volc = .TRUE.
  END IF
  IF (model_config_rec%ra_lw_physics(idomain)==99)THEN
  END IF
  IF (model_config_rec%ra_sw_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%ra_sw_physics(idomain)==2)THEN
  END IF
  IF (model_config_rec%ra_sw_physics(idomain)==3)THEN
   IF ( ozmixm_index_table( PARAM_mth01 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth01 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth01 , idomain ) = P_mth01
   ELSE
     P_mth01 = ozmixm_index_table( PARAM_mth01 , idomain )
   END IF
   ozmixm_stream_table( idomain, P_mth01 ) = 0
   ozmixm_dname_table( idomain, P_mth01 ) = ''
   ozmixm_desc_table( idomain, P_mth01 ) = '-'
   ozmixm_units_table( idomain, P_mth01 ) = '-'
   F_mth01 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth02 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth02 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth02 , idomain ) = P_mth02
   ELSE
     P_mth02 = ozmixm_index_table( PARAM_mth02 , idomain )
   END IF
   ozmixm_stream_table( idomain, P_mth02 ) = 0
   ozmixm_dname_table( idomain, P_mth02 ) = ''
   ozmixm_desc_table( idomain, P_mth02 ) = '-'
   ozmixm_units_table( idomain, P_mth02 ) = '-'
   F_mth02 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth03 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth03 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth03 , idomain ) = P_mth03
   ELSE
     P_mth03 = ozmixm_index_table( PARAM_mth03 , idomain )
   END IF
   ozmixm_stream_table( idomain, P_mth03 ) = 0
   ozmixm_dname_table( idomain, P_mth03 ) = ''
   ozmixm_desc_table( idomain, P_mth03 ) = '-'
   ozmixm_units_table( idomain, P_mth03 ) = '-'
   F_mth03 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth04 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth04 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth04 , idomain ) = P_mth04
   ELSE
     P_mth04 = ozmixm_index_table( PARAM_mth04 , idomain )
   END IF
   ozmixm_stream_table( idomain, P_mth04 ) = 0
   ozmixm_dname_table( idomain, P_mth04 ) = ''
   ozmixm_desc_table( idomain, P_mth04 ) = '-'
   ozmixm_units_table( idomain, P_mth04 ) = '-'
   F_mth04 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth05 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth05 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth05 , idomain ) = P_mth05
   ELSE
     P_mth05 = ozmixm_index_table( PARAM_mth05 , idomain )
   END IF
   ozmixm_stream_table( idomain, P_mth05 ) = 0
   ozmixm_dname_table( idomain, P_mth05 ) = ''
   ozmixm_desc_table( idomain, P_mth05 ) = '-'
   ozmixm_units_table( idomain, P_mth05 ) = '-'
   F_mth05 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth06 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth06 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth06 , idomain ) = P_mth06
   ELSE
     P_mth06 = ozmixm_index_table( PARAM_mth06 , idomain )
   END IF
   ozmixm_stream_table( idomain, P_mth06 ) = 0
   ozmixm_dname_table( idomain, P_mth06 ) = ''
   ozmixm_desc_table( idomain, P_mth06 ) = '-'
   ozmixm_units_table( idomain, P_mth06 ) = '-'
   F_mth06 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth07 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth07 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth07 , idomain ) = P_mth07
   ELSE
     P_mth07 = ozmixm_index_table( PARAM_mth07 , idomain )
   END IF
   ozmixm_stream_table( idomain, P_mth07 ) = 0
   ozmixm_dname_table( idomain, P_mth07 ) = ''
   ozmixm_desc_table( idomain, P_mth07 ) = '-'
   ozmixm_units_table( idomain, P_mth07 ) = '-'
   F_mth07 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth08 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth08 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth08 , idomain ) = P_mth08
   ELSE
     P_mth08 = ozmixm_index_table( PARAM_mth08 , idomain )
   END IF
   ozmixm_stream_table( idomain, P_mth08 ) = 0
   ozmixm_dname_table( idomain, P_mth08 ) = ''
   ozmixm_desc_table( idomain, P_mth08 ) = '-'
   ozmixm_units_table( idomain, P_mth08 ) = '-'
   F_mth08 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth09 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth09 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth09 , idomain ) = P_mth09
   ELSE
     P_mth09 = ozmixm_index_table( PARAM_mth09 , idomain )
   END IF
   ozmixm_stream_table( idomain, P_mth09 ) = 0
   ozmixm_dname_table( idomain, P_mth09 ) = ''
   ozmixm_desc_table( idomain, P_mth09 ) = '-'
   ozmixm_units_table( idomain, P_mth09 ) = '-'
   F_mth09 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth10 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth10 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth10 , idomain ) = P_mth10
   ELSE
     P_mth10 = ozmixm_index_table( PARAM_mth10 , idomain )
   END IF
   ozmixm_stream_table( idomain, P_mth10 ) = 0
   ozmixm_dname_table( idomain, P_mth10 ) = ''
   ozmixm_desc_table( idomain, P_mth10 ) = '-'
   ozmixm_units_table( idomain, P_mth10 ) = '-'
   F_mth10 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth11 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth11 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth11 , idomain ) = P_mth11
   ELSE
     P_mth11 = ozmixm_index_table( PARAM_mth11 , idomain )
   END IF
   ozmixm_stream_table( idomain, P_mth11 ) = 0
   ozmixm_dname_table( idomain, P_mth11 ) = ''
   ozmixm_desc_table( idomain, P_mth11 ) = '-'
   ozmixm_units_table( idomain, P_mth11 ) = '-'
   F_mth11 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth12 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth12 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth12 , idomain ) = P_mth12
   ELSE
     P_mth12 = ozmixm_index_table( PARAM_mth12 , idomain )
   END IF
   ozmixm_stream_table( idomain, P_mth12 ) = 0
   ozmixm_dname_table( idomain, P_mth12 ) = ''
   ozmixm_desc_table( idomain, P_mth12 ) = '-'
   ozmixm_units_table( idomain, P_mth12 ) = '-'
   F_mth12 = .TRUE.
   IF ( aerosolc_index_table( PARAM_sul , idomain ) .lt. 1 ) THEN
     aerosolc_num_table(idomain) = aerosolc_num_table(idomain) + 1
     P_sul = aerosolc_num_table(idomain)
     aerosolc_index_table( PARAM_sul , idomain ) = P_sul
   ELSE
     P_sul = aerosolc_index_table( PARAM_sul , idomain )
   END IF
   aerosolc_stream_table( idomain, P_sul ) = 0
   aerosolc_dname_table( idomain, P_sul ) = 'SUL'
   aerosolc_desc_table( idomain, P_sul ) = 'SUL aerosol concentration'
   aerosolc_units_table( idomain, P_sul ) = '-'
   F_sul = .TRUE.
   IF ( aerosolc_index_table( PARAM_sslt , idomain ) .lt. 1 ) THEN
     aerosolc_num_table(idomain) = aerosolc_num_table(idomain) + 1
     P_sslt = aerosolc_num_table(idomain)
     aerosolc_index_table( PARAM_sslt , idomain ) = P_sslt
   ELSE
     P_sslt = aerosolc_index_table( PARAM_sslt , idomain )
   END IF
   aerosolc_stream_table( idomain, P_sslt ) = 0
   aerosolc_dname_table( idomain, P_sslt ) = 'SSLT'
   aerosolc_desc_table( idomain, P_sslt ) = 'SSLT aerosol concentration'
   aerosolc_units_table( idomain, P_sslt ) = '-'
   F_sslt = .TRUE.
   IF ( aerosolc_index_table( PARAM_dust1 , idomain ) .lt. 1 ) THEN
     aerosolc_num_table(idomain) = aerosolc_num_table(idomain) + 1
     P_dust1 = aerosolc_num_table(idomain)
     aerosolc_index_table( PARAM_dust1 , idomain ) = P_dust1
   ELSE
     P_dust1 = aerosolc_index_table( PARAM_dust1 , idomain )
   END IF
   aerosolc_stream_table( idomain, P_dust1 ) = 0
   aerosolc_dname_table( idomain, P_dust1 ) = 'DUST1'
   aerosolc_desc_table( idomain, P_dust1 ) = 'DUST1 aerosol concentration'
   aerosolc_units_table( idomain, P_dust1 ) = '-'
   F_dust1 = .TRUE.
   IF ( aerosolc_index_table( PARAM_dust2 , idomain ) .lt. 1 ) THEN
     aerosolc_num_table(idomain) = aerosolc_num_table(idomain) + 1
     P_dust2 = aerosolc_num_table(idomain)
     aerosolc_index_table( PARAM_dust2 , idomain ) = P_dust2
   ELSE
     P_dust2 = aerosolc_index_table( PARAM_dust2 , idomain )
   END IF
   aerosolc_stream_table( idomain, P_dust2 ) = 0
   aerosolc_dname_table( idomain, P_dust2 ) = 'DUST2'
   aerosolc_desc_table( idomain, P_dust2 ) = 'DUST2 aerosol concentration'
   aerosolc_units_table( idomain, P_dust2 ) = '-'
   F_dust2 = .TRUE.
   IF ( aerosolc_index_table( PARAM_dust3 , idomain ) .lt. 1 ) THEN
     aerosolc_num_table(idomain) = aerosolc_num_table(idomain) + 1
     P_dust3 = aerosolc_num_table(idomain)
     aerosolc_index_table( PARAM_dust3 , idomain ) = P_dust3
   ELSE
     P_dust3 = aerosolc_index_table( PARAM_dust3 , idomain )
   END IF
   aerosolc_stream_table( idomain, P_dust3 ) = 0
   aerosolc_dname_table( idomain, P_dust3 ) = 'DUST3'
   aerosolc_desc_table( idomain, P_dust3 ) = 'DUST3 aerosol concentration'
   aerosolc_units_table( idomain, P_dust3 ) = '-'
   F_dust3 = .TRUE.
   IF ( aerosolc_index_table( PARAM_dust4 , idomain ) .lt. 1 ) THEN
     aerosolc_num_table(idomain) = aerosolc_num_table(idomain) + 1
     P_dust4 = aerosolc_num_table(idomain)
     aerosolc_index_table( PARAM_dust4 , idomain ) = P_dust4
   ELSE
     P_dust4 = aerosolc_index_table( PARAM_dust4 , idomain )
   END IF
   aerosolc_stream_table( idomain, P_dust4 ) = 0
   aerosolc_dname_table( idomain, P_dust4 ) = 'DUST4'
   aerosolc_desc_table( idomain, P_dust4 ) = 'DUST4 aerosol concentration'
   aerosolc_units_table( idomain, P_dust4 ) = '-'
   F_dust4 = .TRUE.
   IF ( aerosolc_index_table( PARAM_ocpho , idomain ) .lt. 1 ) THEN
     aerosolc_num_table(idomain) = aerosolc_num_table(idomain) + 1
     P_ocpho = aerosolc_num_table(idomain)
     aerosolc_index_table( PARAM_ocpho , idomain ) = P_ocpho
   ELSE
     P_ocpho = aerosolc_index_table( PARAM_ocpho , idomain )
   END IF
   aerosolc_stream_table( idomain, P_ocpho ) = 0
   aerosolc_dname_table( idomain, P_ocpho ) = 'OCPHO'
   aerosolc_desc_table( idomain, P_ocpho ) = 'OCPHO aerosol concentration'
   aerosolc_units_table( idomain, P_ocpho ) = '-'
   F_ocpho = .TRUE.
   IF ( aerosolc_index_table( PARAM_bcpho , idomain ) .lt. 1 ) THEN
     aerosolc_num_table(idomain) = aerosolc_num_table(idomain) + 1
     P_bcpho = aerosolc_num_table(idomain)
     aerosolc_index_table( PARAM_bcpho , idomain ) = P_bcpho
   ELSE
     P_bcpho = aerosolc_index_table( PARAM_bcpho , idomain )
   END IF
   aerosolc_stream_table( idomain, P_bcpho ) = 0
   aerosolc_dname_table( idomain, P_bcpho ) = 'BCPHO'
   aerosolc_desc_table( idomain, P_bcpho ) = 'BCPHO aerosol concentration'
   aerosolc_units_table( idomain, P_bcpho ) = '-'
   F_bcpho = .TRUE.
   IF ( aerosolc_index_table( PARAM_ocphi , idomain ) .lt. 1 ) THEN
     aerosolc_num_table(idomain) = aerosolc_num_table(idomain) + 1
     P_ocphi = aerosolc_num_table(idomain)
     aerosolc_index_table( PARAM_ocphi , idomain ) = P_ocphi
   ELSE
     P_ocphi = aerosolc_index_table( PARAM_ocphi , idomain )
   END IF
   aerosolc_stream_table( idomain, P_ocphi ) = 0
   aerosolc_dname_table( idomain, P_ocphi ) = 'OCPHI'
   aerosolc_desc_table( idomain, P_ocphi ) = 'OCPHI aerosol concentration'
   aerosolc_units_table( idomain, P_ocphi ) = '-'
   F_ocphi = .TRUE.
   IF ( aerosolc_index_table( PARAM_bcphi , idomain ) .lt. 1 ) THEN
     aerosolc_num_table(idomain) = aerosolc_num_table(idomain) + 1
     P_bcphi = aerosolc_num_table(idomain)
     aerosolc_index_table( PARAM_bcphi , idomain ) = P_bcphi
   ELSE
     P_bcphi = aerosolc_index_table( PARAM_bcphi , idomain )
   END IF
   aerosolc_stream_table( idomain, P_bcphi ) = 0
   aerosolc_dname_table( idomain, P_bcphi ) = 'BCPHI'
   aerosolc_desc_table( idomain, P_bcphi ) = 'BCPHI aerosol concentration'
   aerosolc_units_table( idomain, P_bcphi ) = '-'
   F_bcphi = .TRUE.
   IF ( aerosolc_index_table( PARAM_bg , idomain ) .lt. 1 ) THEN
     aerosolc_num_table(idomain) = aerosolc_num_table(idomain) + 1
     P_bg = aerosolc_num_table(idomain)
     aerosolc_index_table( PARAM_bg , idomain ) = P_bg
   ELSE
     P_bg = aerosolc_index_table( PARAM_bg , idomain )
   END IF
   aerosolc_stream_table( idomain, P_bg ) = 0
   aerosolc_dname_table( idomain, P_bg ) = 'BG'
   aerosolc_desc_table( idomain, P_bg ) = 'BG aerosol concentration'
   aerosolc_units_table( idomain, P_bg ) = '-'
   F_bg = .TRUE.
   IF ( aerosolc_index_table( PARAM_volc , idomain ) .lt. 1 ) THEN
     aerosolc_num_table(idomain) = aerosolc_num_table(idomain) + 1
     P_volc = aerosolc_num_table(idomain)
     aerosolc_index_table( PARAM_volc , idomain ) = P_volc
   ELSE
     P_volc = aerosolc_index_table( PARAM_volc , idomain )
   END IF
   aerosolc_stream_table( idomain, P_volc ) = 0
   aerosolc_dname_table( idomain, P_volc ) = 'VOLC'
   aerosolc_desc_table( idomain, P_volc ) = 'VOLC aerosol concentration'
   aerosolc_units_table( idomain, P_volc ) = '-'
   F_volc = .TRUE.
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
  IF (model_config_rec%grid_fdda(idomain)==1)THEN
   IF ( fdda3d_index_table( PARAM_u_ndg_old , idomain ) .lt. 1 ) THEN
     fdda3d_num_table(idomain) = fdda3d_num_table(idomain) + 1
     P_u_ndg_old = fdda3d_num_table(idomain)
     fdda3d_index_table( PARAM_u_ndg_old , idomain ) = P_u_ndg_old
   ELSE
     P_u_ndg_old = fdda3d_index_table( PARAM_u_ndg_old , idomain )
   END IF
   fdda3d_stream_table( idomain, P_u_ndg_old ) = 41943040
   fdda3d_dname_table( idomain, P_u_ndg_old ) = 'U_NDG_OLD'
   fdda3d_desc_table( idomain, P_u_ndg_old ) = 'OLD X WIND FOR FDDA GRID NUDGING'
   fdda3d_units_table( idomain, P_u_ndg_old ) = 'm s-1'
   F_u_ndg_old = .TRUE.
   IF ( fdda3d_index_table( PARAM_v_ndg_old , idomain ) .lt. 1 ) THEN
     fdda3d_num_table(idomain) = fdda3d_num_table(idomain) + 1
     P_v_ndg_old = fdda3d_num_table(idomain)
     fdda3d_index_table( PARAM_v_ndg_old , idomain ) = P_v_ndg_old
   ELSE
     P_v_ndg_old = fdda3d_index_table( PARAM_v_ndg_old , idomain )
   END IF
   fdda3d_stream_table( idomain, P_v_ndg_old ) = 41943040
   fdda3d_dname_table( idomain, P_v_ndg_old ) = 'V_NDG_OLD'
   fdda3d_desc_table( idomain, P_v_ndg_old ) = 'OLD Y WIND FOR FDDA GRID NUDGING'
   fdda3d_units_table( idomain, P_v_ndg_old ) = 'm s-1'
   F_v_ndg_old = .TRUE.
   IF ( fdda3d_index_table( PARAM_t_ndg_old , idomain ) .lt. 1 ) THEN
     fdda3d_num_table(idomain) = fdda3d_num_table(idomain) + 1
     P_t_ndg_old = fdda3d_num_table(idomain)
     fdda3d_index_table( PARAM_t_ndg_old , idomain ) = P_t_ndg_old
   ELSE
     P_t_ndg_old = fdda3d_index_table( PARAM_t_ndg_old , idomain )
   END IF
   fdda3d_stream_table( idomain, P_t_ndg_old ) = 41943040
   fdda3d_dname_table( idomain, P_t_ndg_old ) = 'T_NDG_OLD'
   fdda3d_desc_table( idomain, P_t_ndg_old ) = 'OLD PERT POT TEMP FOR FDDA GRID NUDGING'
   fdda3d_units_table( idomain, P_t_ndg_old ) = 'K'
   F_t_ndg_old = .TRUE.
   IF ( fdda3d_index_table( PARAM_q_ndg_old , idomain ) .lt. 1 ) THEN
     fdda3d_num_table(idomain) = fdda3d_num_table(idomain) + 1
     P_q_ndg_old = fdda3d_num_table(idomain)
     fdda3d_index_table( PARAM_q_ndg_old , idomain ) = P_q_ndg_old
   ELSE
     P_q_ndg_old = fdda3d_index_table( PARAM_q_ndg_old , idomain )
   END IF
   fdda3d_stream_table( idomain, P_q_ndg_old ) = 41943040
   fdda3d_dname_table( idomain, P_q_ndg_old ) = 'Q_NDG_OLD'
   fdda3d_desc_table( idomain, P_q_ndg_old ) = 'OLD WATER VAPOR MIX RATIO FOR FDDA GRID NUDGING'
   fdda3d_units_table( idomain, P_q_ndg_old ) = 'kg/kg'
   F_q_ndg_old = .TRUE.
   IF ( fdda3d_index_table( PARAM_ph_ndg_old , idomain ) .lt. 1 ) THEN
     fdda3d_num_table(idomain) = fdda3d_num_table(idomain) + 1
     P_ph_ndg_old = fdda3d_num_table(idomain)
     fdda3d_index_table( PARAM_ph_ndg_old , idomain ) = P_ph_ndg_old
   ELSE
     P_ph_ndg_old = fdda3d_index_table( PARAM_ph_ndg_old , idomain )
   END IF
   fdda3d_stream_table( idomain, P_ph_ndg_old ) = 41943040
   fdda3d_dname_table( idomain, P_ph_ndg_old ) = 'PH_NDG_OLD'
   fdda3d_desc_table( idomain, P_ph_ndg_old ) = 'OLD PERT GEOPOTENTIAL FOR FDDA GRID NUDGING'
   fdda3d_units_table( idomain, P_ph_ndg_old ) = 'kg/kg'
   F_ph_ndg_old = .TRUE.
   IF ( fdda3d_index_table( PARAM_u_ndg_new , idomain ) .lt. 1 ) THEN
     fdda3d_num_table(idomain) = fdda3d_num_table(idomain) + 1
     P_u_ndg_new = fdda3d_num_table(idomain)
     fdda3d_index_table( PARAM_u_ndg_new , idomain ) = P_u_ndg_new
   ELSE
     P_u_ndg_new = fdda3d_index_table( PARAM_u_ndg_new , idomain )
   END IF
   fdda3d_stream_table( idomain, P_u_ndg_new ) = 41943040
   fdda3d_dname_table( idomain, P_u_ndg_new ) = 'U_NDG_NEW'
   fdda3d_desc_table( idomain, P_u_ndg_new ) = 'NEW X WIND FOR FDDA GRID NUDGING'
   fdda3d_units_table( idomain, P_u_ndg_new ) = 'm s-1'
   F_u_ndg_new = .TRUE.
   IF ( fdda3d_index_table( PARAM_v_ndg_new , idomain ) .lt. 1 ) THEN
     fdda3d_num_table(idomain) = fdda3d_num_table(idomain) + 1
     P_v_ndg_new = fdda3d_num_table(idomain)
     fdda3d_index_table( PARAM_v_ndg_new , idomain ) = P_v_ndg_new
   ELSE
     P_v_ndg_new = fdda3d_index_table( PARAM_v_ndg_new , idomain )
   END IF
   fdda3d_stream_table( idomain, P_v_ndg_new ) = 41943040
   fdda3d_dname_table( idomain, P_v_ndg_new ) = 'V_NDG_NEW'
   fdda3d_desc_table( idomain, P_v_ndg_new ) = 'NEW Y WIND FOR FDDA GRID NUDGING'
   fdda3d_units_table( idomain, P_v_ndg_new ) = 'm s-1'
   F_v_ndg_new = .TRUE.
   IF ( fdda3d_index_table( PARAM_t_ndg_new , idomain ) .lt. 1 ) THEN
     fdda3d_num_table(idomain) = fdda3d_num_table(idomain) + 1
     P_t_ndg_new = fdda3d_num_table(idomain)
     fdda3d_index_table( PARAM_t_ndg_new , idomain ) = P_t_ndg_new
   ELSE
     P_t_ndg_new = fdda3d_index_table( PARAM_t_ndg_new , idomain )
   END IF
   fdda3d_stream_table( idomain, P_t_ndg_new ) = 41943040
   fdda3d_dname_table( idomain, P_t_ndg_new ) = 'T_NDG_NEW'
   fdda3d_desc_table( idomain, P_t_ndg_new ) = 'NEW PERT POT TEMP FOR FDDA GRID NUDGING'
   fdda3d_units_table( idomain, P_t_ndg_new ) = 'K'
   F_t_ndg_new = .TRUE.
   IF ( fdda3d_index_table( PARAM_q_ndg_new , idomain ) .lt. 1 ) THEN
     fdda3d_num_table(idomain) = fdda3d_num_table(idomain) + 1
     P_q_ndg_new = fdda3d_num_table(idomain)
     fdda3d_index_table( PARAM_q_ndg_new , idomain ) = P_q_ndg_new
   ELSE
     P_q_ndg_new = fdda3d_index_table( PARAM_q_ndg_new , idomain )
   END IF
   fdda3d_stream_table( idomain, P_q_ndg_new ) = 41943040
   fdda3d_dname_table( idomain, P_q_ndg_new ) = 'Q_NDG_NEW'
   fdda3d_desc_table( idomain, P_q_ndg_new ) = 'NEW WATER VAPOR MIX RATIO FOR FDDA GRID NUDGING'
   fdda3d_units_table( idomain, P_q_ndg_new ) = 'kg/kg'
   F_q_ndg_new = .TRUE.
   IF ( fdda3d_index_table( PARAM_ph_ndg_new , idomain ) .lt. 1 ) THEN
     fdda3d_num_table(idomain) = fdda3d_num_table(idomain) + 1
     P_ph_ndg_new = fdda3d_num_table(idomain)
     fdda3d_index_table( PARAM_ph_ndg_new , idomain ) = P_ph_ndg_new
   ELSE
     P_ph_ndg_new = fdda3d_index_table( PARAM_ph_ndg_new , idomain )
   END IF
   fdda3d_stream_table( idomain, P_ph_ndg_new ) = 41943040
   fdda3d_dname_table( idomain, P_ph_ndg_new ) = 'PH_NDG_NEW'
   fdda3d_desc_table( idomain, P_ph_ndg_new ) = 'NEW PERT GEOPOTENTIAL FOR FDDA GRID NUDGING'
   fdda3d_units_table( idomain, P_ph_ndg_new ) = 'kg/kg'
   F_ph_ndg_new = .TRUE.
   IF ( fdda2d_index_table( PARAM_mu_ndg_old , idomain ) .lt. 1 ) THEN
     fdda2d_num_table(idomain) = fdda2d_num_table(idomain) + 1
     P_mu_ndg_old = fdda2d_num_table(idomain)
     fdda2d_index_table( PARAM_mu_ndg_old , idomain ) = P_mu_ndg_old
   ELSE
     P_mu_ndg_old = fdda2d_index_table( PARAM_mu_ndg_old , idomain )
   END IF
   fdda2d_stream_table( idomain, P_mu_ndg_old ) = 41943040
   fdda2d_dname_table( idomain, P_mu_ndg_old ) = 'MU_NDG_OLD'
   fdda2d_desc_table( idomain, P_mu_ndg_old ) = 'OLD PERT COLUMN DRY MASS FOR FDDA GRID NUDGING'
   fdda2d_units_table( idomain, P_mu_ndg_old ) = 'Pa'
   F_mu_ndg_old = .TRUE.
   IF ( fdda2d_index_table( PARAM_mu_ndg_new , idomain ) .lt. 1 ) THEN
     fdda2d_num_table(idomain) = fdda2d_num_table(idomain) + 1
     P_mu_ndg_new = fdda2d_num_table(idomain)
     fdda2d_index_table( PARAM_mu_ndg_new , idomain ) = P_mu_ndg_new
   ELSE
     P_mu_ndg_new = fdda2d_index_table( PARAM_mu_ndg_new , idomain )
   END IF
   fdda2d_stream_table( idomain, P_mu_ndg_new ) = 41943040
   fdda2d_dname_table( idomain, P_mu_ndg_new ) = 'MU_NDG_NEW'
   fdda2d_desc_table( idomain, P_mu_ndg_new ) = 'NEW PERT COLUMN DRY MASS FOR FDDA GRID NUDGING'
   fdda2d_units_table( idomain, P_mu_ndg_new ) = 'Pa'
   F_mu_ndg_new = .TRUE.
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
