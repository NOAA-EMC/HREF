


MODULE module_fddagd_driver
CONTAINS


   SUBROUTINE fddagd_driver(itimestep,dt,xtime,                   &
                  id,  &
                  RUNDGDTEN,RVNDGDTEN,RTHNDGDTEN,                 &
                  RPHNDGDTEN,RQVNDGDTEN,RMUNDGDTEN,               &
                  u_ndg_old,v_ndg_old,t_ndg_old,ph_ndg_old,       &
                  q_ndg_old,mu_ndg_old,       &
                  u_ndg_new,v_ndg_new,t_ndg_new,ph_ndg_new,       &
                  q_ndg_new,mu_ndg_new,       &
                  u3d,v3d,th_phy,ph,rho,moist,                    &
                  p_phy,pi_phy,p8w,t_phy,dz8w,z,z_at_w,           &
                  grid,config_flags,DX,n_moist,                   &
                  STEPFG,                                         &
                  pblh,ht,regime,znt,                             &
                  ids,ide, jds,jde, kds,kde,                      &
                  ims,ime, jms,jme, kms,kme,                      &
                  i_start,i_end, j_start,j_end, kts,kte, num_tiles, &
                  u10, v10, th2, q2, &
                  u10_ndg_old, v10_ndg_old, t2_ndg_old, th2_ndg_old, q2_ndg_old,  &
                  rh_ndg_old, psl_ndg_old, ps_ndg_old, tob_ndg_old, odis_ndg_old, &
                  u10_ndg_new, v10_ndg_new, t2_ndg_new, th2_ndg_new, q2_ndg_new,  &
                  rh_ndg_new, psl_ndg_new, ps_ndg_new, tob_ndg_new, odis_ndg_new, &
                  ips,ipe,jps,jpe,kps,kpe,                          &
                  imsx,imex,jmsx,jmex,kmsx,kmex,                    &
                  ipsx,ipex,jpsx,jpex,kpsx,kpex,                    &
                  imsy,imey,jmsy,jmey,kmsy,kmey,                    &
                  ipsy,ipey,jpsy,jpey,kpsy,kpey                     )

   USE module_configure
   USE module_state_description
   USE module_model_constants
   USE module_domain, ONLY : domain



   USE module_fdda_psufddagd
   USE module_fdda_spnudging


   IMPLICIT NONE












































































   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
   TYPE(domain) , TARGET          :: grid


   INTEGER , INTENT(IN)         ::     id

   INTEGER,    INTENT(IN   )    ::     ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       kts,kte, num_tiles,        &
                                       ips,ipe,jps,jpe,kps,kpe,   &
                                       imsx,imex,jmsx,jmex,kmsx,kmex,   &
                                       ipsx,ipex,jpsx,jpex,kpsx,kpex,   &
                                       imsy,imey,jmsy,jmey,kmsy,kmey,   &
                                       ipsy,ipey,jpsy,jpey,kpsy,kpey,   &
                                       n_moist           

   INTEGER, DIMENSION(num_tiles), INTENT(IN) ::                   &
  &                                    i_start,i_end,j_start,j_end

   INTEGER,    INTENT(IN   )    ::     itimestep,STEPFG

   REAL,       INTENT(IN   )    ::     DT,DX,XTIME



   REAL,       DIMENSION( ims:ime, kms:kme, jms:jme ),            &
               INTENT(IN   )    ::                         p_phy, &
                                                          pi_phy, &
                                                             p8w, &
                                                             rho, &
                                                           t_phy, &
                                                             u3d, &
                                                             v3d, &
                                                              ph, &
                                                            dz8w, &
                                                               z, &
                                                          z_at_w, &
                                                          th_phy

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, n_moist ),         &
         INTENT(IN ) ::                                    moist



   REAL,       DIMENSION( ims:ime, kms:kme, jms:jme ),            &
               INTENT(INOUT)    ::                       RUNDGDTEN, &
                                                         RVNDGDTEN, &
                                                        RTHNDGDTEN, &
                                                        RPHNDGDTEN, &
                                                        RQVNDGDTEN

   REAL,       DIMENSION( ims:ime,  jms:jme ),            &
               INTENT(INOUT)    ::                      RMUNDGDTEN

   REAL,       DIMENSION( ims:ime, kms:kme, jms:jme ),            &
               INTENT(INOUT)    ::                       u_ndg_old, &
                                                         v_ndg_old, &
                                                         t_ndg_old, &
                                                         ph_ndg_old,&
                                                         q_ndg_old, &
                                                         u_ndg_new, &
                                                         v_ndg_new, &
                                                         t_ndg_new, &
                                                         ph_ndg_new,&
                                                         q_ndg_new
   REAL,       DIMENSION( ims:ime,  jms:jme ),            &
               INTENT(INOUT)    ::                       mu_ndg_old, &
                                                         mu_ndg_new


   REAL,    DIMENSION( ims:ime , jms:jme ),     &
               INTENT(IN   ) ::           pblh, &
                                            ht, &
                                           znt

   REAL,    DIMENSION( ims:ime , jms:jme ), INTENT(INOUT   ) :: regime

   REAL,       DIMENSION( ims:ime, jms:jme ),            &
               INTENT(IN   )    ::                       u10, &
                                                         v10, &
                                                         th2, &
                                                         q2

   REAL,       DIMENSION( ims:ime, jms:jme ),            &
               INTENT(IN)       ::                       u10_ndg_old,  &
                                                         v10_ndg_old,  &
                                                         t2_ndg_old,   &
                                                         th2_ndg_old,  &
                                                         q2_ndg_old,   &
                                                         rh_ndg_old,   &
                                                         psl_ndg_old,  &
                                                         ps_ndg_old,   &
                                                         odis_ndg_old,  &
                                                         u10_ndg_new,  &
                                                         v10_ndg_new,  &
                                                         t2_ndg_new,   &
                                                         th2_ndg_new,  &
                                                         q2_ndg_new,   &
                                                         rh_ndg_new,   &
                                                         psl_ndg_new,  &
                                                         ps_ndg_new,   &
                                                         odis_ndg_new

   REAL,       DIMENSION( ims:ime, jms:jme ),            &
               INTENT(IN)       ::                       tob_ndg_old,  &
                                                         tob_ndg_new




   INTEGER :: i,J,K,NK,jj,ij
   CHARACTER (LEN=256) :: message




   END SUBROUTINE fddagd_driver
END MODULE module_fddagd_driver
