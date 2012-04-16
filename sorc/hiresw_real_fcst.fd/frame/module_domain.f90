
























MODULE module_domain

   USE module_driver_constants
   USE module_machine
   USE module_configure
   USE module_wrf_error
   USE module_utility
   USE module_domain_type

   
   
   
   

   
   
   
   
   

   TYPE(domain) , POINTER :: head_grid , new_grid , next_grid , old_grid

   
   
   
   

   TYPE domain_levels
      TYPE(domain) , POINTER                              :: first_domain
   END TYPE domain_levels

   TYPE(domain_levels) , DIMENSION(max_levels)            :: head_for_each_level

   
   TYPE(domain), POINTER :: current_grid
   LOGICAL, SAVE :: current_grid_set = .FALSE.

   
   PRIVATE domain_time_test_print
   PRIVATE test_adjust_io_timestr

   INTERFACE get_ijk_from_grid
     MODULE PROCEDURE get_ijk_from_grid1, get_ijk_from_grid2
   END INTERFACE


CONTAINS

   SUBROUTINE adjust_domain_dims_for_move( grid , dx, dy )
    IMPLICIT NONE

    TYPE( domain ), POINTER   :: grid
    INTEGER, INTENT(IN) ::  dx, dy

    data_ordering : SELECT CASE ( model_data_order )
       CASE  ( DATA_ORDER_XYZ )
            grid%sm31  = grid%sm31 + dx
            grid%em31  = grid%em31 + dx
            grid%sm32  = grid%sm32 + dy
            grid%em32  = grid%em32 + dy
            grid%sp31  = grid%sp31 + dx
            grid%ep31  = grid%ep31 + dx
            grid%sp32  = grid%sp32 + dy
            grid%ep32  = grid%ep32 + dy
            grid%sd31  = grid%sd31 + dx
            grid%ed31  = grid%ed31 + dx
            grid%sd32  = grid%sd32 + dy
            grid%ed32  = grid%ed32 + dy

       CASE  ( DATA_ORDER_YXZ )
            grid%sm31  = grid%sm31 + dy
            grid%em31  = grid%em31 + dy
            grid%sm32  = grid%sm32 + dx
            grid%em32  = grid%em32 + dx
            grid%sp31  = grid%sp31 + dy
            grid%ep31  = grid%ep31 + dy
            grid%sp32  = grid%sp32 + dx
            grid%ep32  = grid%ep32 + dx
            grid%sd31  = grid%sd31 + dy
            grid%ed31  = grid%ed31 + dy
            grid%sd32  = grid%sd32 + dx
            grid%ed32  = grid%ed32 + dx

       CASE  ( DATA_ORDER_ZXY )
            grid%sm32  = grid%sm32 + dx
            grid%em32  = grid%em32 + dx
            grid%sm33  = grid%sm33 + dy
            grid%em33  = grid%em33 + dy
            grid%sp32  = grid%sp32 + dx
            grid%ep32  = grid%ep32 + dx
            grid%sp33  = grid%sp33 + dy
            grid%ep33  = grid%ep33 + dy
            grid%sd32  = grid%sd32 + dx
            grid%ed32  = grid%ed32 + dx
            grid%sd33  = grid%sd33 + dy
            grid%ed33  = grid%ed33 + dy

       CASE  ( DATA_ORDER_ZYX )
            grid%sm32  = grid%sm32 + dy
            grid%em32  = grid%em32 + dy
            grid%sm33  = grid%sm33 + dx
            grid%em33  = grid%em33 + dx
            grid%sp32  = grid%sp32 + dy
            grid%ep32  = grid%ep32 + dy
            grid%sp33  = grid%sp33 + dx
            grid%ep33  = grid%ep33 + dx
            grid%sd32  = grid%sd32 + dy
            grid%ed32  = grid%ed32 + dy
            grid%sd33  = grid%sd33 + dx
            grid%ed33  = grid%ed33 + dx

       CASE  ( DATA_ORDER_XZY )
            grid%sm31  = grid%sm31 + dx
            grid%em31  = grid%em31 + dx
            grid%sm33  = grid%sm33 + dy
            grid%em33  = grid%em33 + dy
            grid%sp31  = grid%sp31 + dx
            grid%ep31  = grid%ep31 + dx
            grid%sp33  = grid%sp33 + dy
            grid%ep33  = grid%ep33 + dy
            grid%sd31  = grid%sd31 + dx
            grid%ed31  = grid%ed31 + dx
            grid%sd33  = grid%sd33 + dy
            grid%ed33  = grid%ed33 + dy

       CASE  ( DATA_ORDER_YZX )
            grid%sm31  = grid%sm31 + dy
            grid%em31  = grid%em31 + dy
            grid%sm33  = grid%sm33 + dx
            grid%em33  = grid%em33 + dx
            grid%sp31  = grid%sp31 + dy
            grid%ep31  = grid%ep31 + dy
            grid%sp33  = grid%sp33 + dx
            grid%ep33  = grid%ep33 + dx
            grid%sd31  = grid%sd31 + dy
            grid%ed31  = grid%ed31 + dy
            grid%sd33  = grid%sd33 + dx
            grid%ed33  = grid%ed33 + dx

    END SELECT data_ordering


    RETURN
   END SUBROUTINE adjust_domain_dims_for_move

   SUBROUTINE get_ijk_from_grid1 (  grid ,                   &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe,    &
                           imsx, imex, jmsx, jmex, kmsx, kmex,    &
                           ipsx, ipex, jpsx, jpex, kpsx, kpex,    &
                           imsy, imey, jmsy, jmey, kmsy, kmey,    &
                           ipsy, ipey, jpsy, jpey, kpsy, kpey )
    IMPLICIT NONE
    TYPE( domain ), INTENT (IN)  :: grid
    INTEGER, INTENT(OUT) ::                                 &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe,    &
                           imsx, imex, jmsx, jmex, kmsx, kmex,    &
                           ipsx, ipex, jpsx, jpex, kpsx, kpex,    &
                           imsy, imey, jmsy, jmey, kmsy, kmey,    &
                           ipsy, ipey, jpsy, jpey, kpsy, kpey

     CALL get_ijk_from_grid2 (  grid ,                   &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe )
     data_ordering : SELECT CASE ( model_data_order )
       CASE  ( DATA_ORDER_XYZ )
           imsx = grid%sm31x ; imex = grid%em31x ; jmsx = grid%sm32x ; jmex = grid%em32x ; kmsx = grid%sm33x ; kmex = grid%em33x ;
           ipsx = grid%sp31x ; ipex = grid%ep31x ; jpsx = grid%sp32x ; jpex = grid%ep32x ; kpsx = grid%sp33x ; kpex = grid%ep33x ;
           imsy = grid%sm31y ; imey = grid%em31y ; jmsy = grid%sm32y ; jmey = grid%em32y ; kmsy = grid%sm33y ; kmey = grid%em33y ;
           ipsy = grid%sp31y ; ipey = grid%ep31y ; jpsy = grid%sp32y ; jpey = grid%ep32y ; kpsy = grid%sp33y ; kpey = grid%ep33y ;
       CASE  ( DATA_ORDER_YXZ )
           imsx = grid%sm32x ; imex = grid%em32x ; jmsx = grid%sm31x ; jmex = grid%em31x ; kmsx = grid%sm33x ; kmex = grid%em33x ;
           ipsx = grid%sp32x ; ipex = grid%ep32x ; jpsx = grid%sp31x ; jpex = grid%ep31x ; kpsx = grid%sp33x ; kpex = grid%ep33x ;
           imsy = grid%sm32y ; imey = grid%em32y ; jmsy = grid%sm31y ; jmey = grid%em31y ; kmsy = grid%sm33y ; kmey = grid%em33y ;
           ipsy = grid%sp32y ; ipey = grid%ep32y ; jpsy = grid%sp31y ; jpey = grid%ep31y ; kpsy = grid%sp33y ; kpey = grid%ep33y ;
       CASE  ( DATA_ORDER_ZXY )
           imsx = grid%sm32x ; imex = grid%em32x ; jmsx = grid%sm33x ; jmex = grid%em33x ; kmsx = grid%sm31x ; kmex = grid%em31x ;
           ipsx = grid%sp32x ; ipex = grid%ep32x ; jpsx = grid%sp33x ; jpex = grid%ep33x ; kpsx = grid%sp31x ; kpex = grid%ep31x ;
           imsy = grid%sm32y ; imey = grid%em32y ; jmsy = grid%sm33y ; jmey = grid%em33y ; kmsy = grid%sm31y ; kmey = grid%em31y ;
           ipsy = grid%sp32y ; ipey = grid%ep32y ; jpsy = grid%sp33y ; jpey = grid%ep33y ; kpsy = grid%sp31y ; kpey = grid%ep31y ;
       CASE  ( DATA_ORDER_ZYX )
           imsx = grid%sm33x ; imex = grid%em33x ; jmsx = grid%sm32x ; jmex = grid%em32x ; kmsx = grid%sm31x ; kmex = grid%em31x ;
           ipsx = grid%sp33x ; ipex = grid%ep33x ; jpsx = grid%sp32x ; jpex = grid%ep32x ; kpsx = grid%sp31x ; kpex = grid%ep31x ;
           imsy = grid%sm33y ; imey = grid%em33y ; jmsy = grid%sm32y ; jmey = grid%em32y ; kmsy = grid%sm31y ; kmey = grid%em31y ;
           ipsy = grid%sp33y ; ipey = grid%ep33y ; jpsy = grid%sp32y ; jpey = grid%ep32y ; kpsy = grid%sp31y ; kpey = grid%ep31y ;
       CASE  ( DATA_ORDER_XZY )
           imsx = grid%sm31x ; imex = grid%em31x ; jmsx = grid%sm33x ; jmex = grid%em33x ; kmsx = grid%sm32x ; kmex = grid%em32x ;
           ipsx = grid%sp31x ; ipex = grid%ep31x ; jpsx = grid%sp33x ; jpex = grid%ep33x ; kpsx = grid%sp32x ; kpex = grid%ep32x ;
           imsy = grid%sm31y ; imey = grid%em31y ; jmsy = grid%sm33y ; jmey = grid%em33y ; kmsy = grid%sm32y ; kmey = grid%em32y ;
           ipsy = grid%sp31y ; ipey = grid%ep31y ; jpsy = grid%sp33y ; jpey = grid%ep33y ; kpsy = grid%sp32y ; kpey = grid%ep32y ;
       CASE  ( DATA_ORDER_YZX )
           imsx = grid%sm33x ; imex = grid%em33x ; jmsx = grid%sm31x ; jmex = grid%em31x ; kmsx = grid%sm32x ; kmex = grid%em32x ;
           ipsx = grid%sp33x ; ipex = grid%ep33x ; jpsx = grid%sp31x ; jpex = grid%ep31x ; kpsx = grid%sp32x ; kpex = grid%ep32x ;
           imsy = grid%sm33y ; imey = grid%em33y ; jmsy = grid%sm31y ; jmey = grid%em31y ; kmsy = grid%sm32y ; kmey = grid%em32y ;
           ipsy = grid%sp33y ; ipey = grid%ep33y ; jpsy = grid%sp31y ; jpey = grid%ep31y ; kpsy = grid%sp32y ; kpey = grid%ep32y ;
     END SELECT data_ordering
   END SUBROUTINE get_ijk_from_grid1

   SUBROUTINE get_ijk_from_grid2 (  grid ,                   &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe )

    IMPLICIT NONE

    TYPE( domain ), INTENT (IN)  :: grid
    INTEGER, INTENT(OUT) ::                                 &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe

    data_ordering : SELECT CASE ( model_data_order )
       CASE  ( DATA_ORDER_XYZ )
           ids = grid%sd31 ; ide = grid%ed31 ; jds = grid%sd32 ; jde = grid%ed32 ; kds = grid%sd33 ; kde = grid%ed33 ;
           ims = grid%sm31 ; ime = grid%em31 ; jms = grid%sm32 ; jme = grid%em32 ; kms = grid%sm33 ; kme = grid%em33 ;
           ips = grid%sp31 ; ipe = grid%ep31 ; jps = grid%sp32 ; jpe = grid%ep32 ; kps = grid%sp33 ; kpe = grid%ep33 ; 
       CASE  ( DATA_ORDER_YXZ )
           ids = grid%sd32  ; ide = grid%ed32  ; jds = grid%sd31  ; jde = grid%ed31  ; kds = grid%sd33  ; kde = grid%ed33  ; 
           ims = grid%sm32  ; ime = grid%em32  ; jms = grid%sm31  ; jme = grid%em31  ; kms = grid%sm33  ; kme = grid%em33  ; 
           ips = grid%sp32  ; ipe = grid%ep32  ; jps = grid%sp31  ; jpe = grid%ep31  ; kps = grid%sp33  ; kpe = grid%ep33  ; 
       CASE  ( DATA_ORDER_ZXY )
           ids = grid%sd32  ; ide = grid%ed32  ; jds = grid%sd33  ; jde = grid%ed33  ; kds = grid%sd31  ; kde = grid%ed31  ; 
           ims = grid%sm32  ; ime = grid%em32  ; jms = grid%sm33  ; jme = grid%em33  ; kms = grid%sm31  ; kme = grid%em31  ; 
           ips = grid%sp32  ; ipe = grid%ep32  ; jps = grid%sp33  ; jpe = grid%ep33  ; kps = grid%sp31  ; kpe = grid%ep31  ; 
       CASE  ( DATA_ORDER_ZYX )
           ids = grid%sd33  ; ide = grid%ed33  ; jds = grid%sd32  ; jde = grid%ed32  ; kds = grid%sd31  ; kde = grid%ed31  ; 
           ims = grid%sm33  ; ime = grid%em33  ; jms = grid%sm32  ; jme = grid%em32  ; kms = grid%sm31  ; kme = grid%em31  ; 
           ips = grid%sp33  ; ipe = grid%ep33  ; jps = grid%sp32  ; jpe = grid%ep32  ; kps = grid%sp31  ; kpe = grid%ep31  ; 
       CASE  ( DATA_ORDER_XZY )
           ids = grid%sd31  ; ide = grid%ed31  ; jds = grid%sd33  ; jde = grid%ed33  ; kds = grid%sd32  ; kde = grid%ed32  ; 
           ims = grid%sm31  ; ime = grid%em31  ; jms = grid%sm33  ; jme = grid%em33  ; kms = grid%sm32  ; kme = grid%em32  ; 
           ips = grid%sp31  ; ipe = grid%ep31  ; jps = grid%sp33  ; jpe = grid%ep33  ; kps = grid%sp32  ; kpe = grid%ep32  ; 
       CASE  ( DATA_ORDER_YZX )
           ids = grid%sd33  ; ide = grid%ed33  ; jds = grid%sd31  ; jde = grid%ed31  ; kds = grid%sd32  ; kde = grid%ed32  ; 
           ims = grid%sm33  ; ime = grid%em33  ; jms = grid%sm31  ; jme = grid%em31  ; kms = grid%sm32  ; kme = grid%em32  ; 
           ips = grid%sp33  ; ipe = grid%ep33  ; jps = grid%sp31  ; jpe = grid%ep31  ; kps = grid%sp32  ; kpe = grid%ep32  ; 
    END SELECT data_ordering
   END SUBROUTINE get_ijk_from_grid2




   SUBROUTINE get_ijk_from_subgrid (  grid ,                &
                           ids0, ide0, jds0, jde0, kds0, kde0,    &
                           ims0, ime0, jms0, jme0, kms0, kme0,    &
                           ips0, ipe0, jps0, jpe0, kps0, kpe0    )
    TYPE( domain ), INTENT (IN)  :: grid
    INTEGER, INTENT(OUT) ::                                 &
                           ids0, ide0, jds0, jde0, kds0, kde0,    &
                           ims0, ime0, jms0, jme0, kms0, kme0,    &
                           ips0, ipe0, jps0, jpe0, kps0, kpe0
   
    INTEGER              ::                                 &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe
     CALL get_ijk_from_grid (  grid ,                         &
                             ids, ide, jds, jde, kds, kde,    &
                             ims, ime, jms, jme, kms, kme,    &
                             ips, ipe, jps, jpe, kps, kpe    )
     ids0 = ids
     ide0 = ide * grid%sr_x
     ims0 = (ims-1)*grid%sr_x+1
     ime0 = ime * grid%sr_x
     ips0 = (ips-1)*grid%sr_x+1
     ipe0 = ipe * grid%sr_x

     jds0 = jds
     jde0 = jde * grid%sr_y
     jms0 = (jms-1)*grid%sr_y+1
     jme0 = jme * grid%sr_y
     jps0 = (jps-1)*grid%sr_y+1
     jpe0 = jpe * grid%sr_y

     kds0 = kds
     kde0 = kde
     kms0 = kms
     kme0 = kme
     kps0 = kps
     kpe0 = kpe
   RETURN
   END SUBROUTINE get_ijk_from_subgrid




   SUBROUTINE wrf_patch_domain( id , domdesc , parent, parent_id , parent_domdesc , &
                            sd1 , ed1 , sp1 , ep1 , sm1 , em1 , &
                            sd2 , ed2 , sp2 , ep2 , sm2 , em2 , &
                            sd3 , ed3 , sp3 , ep3 , sm3 , em3 , &
                                        sp1x , ep1x , sm1x , em1x , &
                                        sp2x , ep2x , sm2x , em2x , &
                                        sp3x , ep3x , sm3x , em3x , &
                                        sp1y , ep1y , sm1y , em1y , &
                                        sp2y , ep2y , sm2y , em2y , &
                                        sp3y , ep3y , sm3y , em3y , &
                            bdx , bdy , bdy_mask )
















































   USE module_machine
   IMPLICIT NONE
   LOGICAL, DIMENSION(4), INTENT(OUT)  :: bdy_mask
   INTEGER, INTENT(IN)   :: sd1 , ed1 , sd2 , ed2 , sd3 , ed3 , bdx , bdy
   INTEGER, INTENT(OUT)  :: sp1  , ep1  , sp2  , ep2  , sp3  , ep3  , &  
                            sm1  , em1  , sm2  , em2  , sm3  , em3
   INTEGER, INTENT(OUT)  :: sp1x , ep1x , sp2x , ep2x , sp3x , ep3x , &  
                            sm1x , em1x , sm2x , em2x , sm3x , em3x
   INTEGER, INTENT(OUT)  :: sp1y , ep1y , sp2y , ep2y , sp3y , ep3y , &  
                            sm1y , em1y , sm2y , em2y , sm3y , em3y
   INTEGER, INTENT(IN)   :: id , parent_id , parent_domdesc
   INTEGER, INTENT(INOUT)  :: domdesc
   TYPE(domain), POINTER :: parent



   INTEGER spec_bdy_width

   CALL nl_get_spec_bdy_width( 1, spec_bdy_width )















   CALL wrf_dm_patch_domain( id , domdesc , parent_id , parent_domdesc , &
                             sd1 , ed1 , sp1 , ep1 , sm1 , em1 , &
                             sd2 , ed2 , sp2 , ep2 , sm2 , em2 , &
                             sd3 , ed3 , sp3 , ep3 , sm3 , em3 , &
                                         sp1x , ep1x , sm1x , em1x , &
                                         sp2x , ep2x , sm2x , em2x , &
                                         sp3x , ep3x , sm3x , em3x , &
                                         sp1y , ep1y , sm1y , em1y , &
                                         sp2y , ep2y , sm2y , em2y , &
                                         sp3y , ep3y , sm3y , em3y , &
                             bdx , bdy )

   SELECT CASE ( model_data_order )
      CASE ( DATA_ORDER_XYZ )
   bdy_mask( P_XSB ) = ( sd1                  <= sp1 .AND. sp1 <= sd1+spec_bdy_width-1 )
   bdy_mask( P_YSB ) = ( sd2                  <= sp2 .AND. sp2 <= sd2+spec_bdy_width-1 )
   bdy_mask( P_XEB ) = ( ed1-spec_bdy_width-1 <= ep1 .AND. ep1 <= ed1                  )
   bdy_mask( P_YEB ) = ( ed2-spec_bdy_width-1 <= ep2 .AND. ep2 <= ed2                  )
      CASE ( DATA_ORDER_YXZ )
   bdy_mask( P_XSB ) = ( sd2                  <= sp2 .AND. sp2 <= sd2+spec_bdy_width-1 )
   bdy_mask( P_YSB ) = ( sd1                  <= sp1 .AND. sp1 <= sd1+spec_bdy_width-1 )
   bdy_mask( P_XEB ) = ( ed2-spec_bdy_width-1 <= ep2 .AND. ep2 <= ed2                  )
   bdy_mask( P_YEB ) = ( ed1-spec_bdy_width-1 <= ep1 .AND. ep1 <= ed1                  )
      CASE ( DATA_ORDER_ZXY )
   bdy_mask( P_XSB ) = ( sd2                  <= sp2 .AND. sp2 <= sd2+spec_bdy_width-1 )
   bdy_mask( P_YSB ) = ( sd3                  <= sp3 .AND. sp3 <= sd3+spec_bdy_width-1 )
   bdy_mask( P_XEB ) = ( ed2-spec_bdy_width-1 <= ep2 .AND. ep2 <= ed2                  )
   bdy_mask( P_YEB ) = ( ed3-spec_bdy_width-1 <= ep3 .AND. ep3 <= ed3                  )
      CASE ( DATA_ORDER_ZYX )
   bdy_mask( P_XSB ) = ( sd3                  <= sp3 .AND. sp3 <= sd3+spec_bdy_width-1 )
   bdy_mask( P_YSB ) = ( sd2                  <= sp2 .AND. sp2 <= sd2+spec_bdy_width-1 )
   bdy_mask( P_XEB ) = ( ed3-spec_bdy_width-1 <= ep3 .AND. ep3 <= ed3                  )
   bdy_mask( P_YEB ) = ( ed2-spec_bdy_width-1 <= ep2 .AND. ep2 <= ed2                  )
      CASE ( DATA_ORDER_XZY )
   bdy_mask( P_XSB ) = ( sd1                  <= sp1 .AND. sp1 <= sd1+spec_bdy_width-1 )
   bdy_mask( P_YSB ) = ( sd3                  <= sp3 .AND. sp3 <= sd3+spec_bdy_width-1 )
   bdy_mask( P_XEB ) = ( ed1-spec_bdy_width-1 <= ep1 .AND. ep1 <= ed1                  )
   bdy_mask( P_YEB ) = ( ed3-spec_bdy_width-1 <= ep3 .AND. ep3 <= ed3                  )
      CASE ( DATA_ORDER_YZX )
   bdy_mask( P_XSB ) = ( sd3                  <= sp3 .AND. sp3 <= sd3+spec_bdy_width-1 )
   bdy_mask( P_YSB ) = ( sd1                  <= sp1 .AND. sp1 <= sd1+spec_bdy_width-1 )
   bdy_mask( P_XEB ) = ( ed3-spec_bdy_width-1 <= ep3 .AND. ep3 <= ed3                  )
   bdy_mask( P_YEB ) = ( ed1-spec_bdy_width-1 <= ep1 .AND. ep1 <= ed1                  )
   END SELECT


   RETURN
   END SUBROUTINE wrf_patch_domain

   SUBROUTINE alloc_and_configure_domain ( domain_id , grid , parent, kid )









































      USE module_alloc_space      
      IMPLICIT NONE

      

      INTEGER , INTENT(IN)                           :: domain_id
      TYPE( domain ) , POINTER                       :: grid
      TYPE( domain ) , POINTER                       :: parent
      INTEGER , INTENT(IN)                           :: kid    

      
      INTEGER                     :: sd1 , ed1 , sp1 , ep1 , sm1 , em1
      INTEGER                     :: sd2 , ed2 , sp2 , ep2 , sm2 , em2
      INTEGER                     :: sd3 , ed3 , sp3 , ep3 , sm3 , em3

      INTEGER                     :: sd1x , ed1x , sp1x , ep1x , sm1x , em1x
      INTEGER                     :: sd2x , ed2x , sp2x , ep2x , sm2x , em2x
      INTEGER                     :: sd3x , ed3x , sp3x , ep3x , sm3x , em3x

      INTEGER                     :: sd1y , ed1y , sp1y , ep1y , sm1y , em1y
      INTEGER                     :: sd2y , ed2y , sp2y , ep2y , sm2y , em2y
      INTEGER                     :: sd3y , ed3y , sp3y , ep3y , sm3y , em3y

      TYPE(domain) , POINTER      :: new_grid
      INTEGER                     :: i
      INTEGER                     :: parent_id , parent_domdesc , new_domdesc
      INTEGER                     :: bdyzone_x , bdyzone_y
      INTEGER                     :: nx, ny







      data_ordering : SELECT CASE ( model_data_order )
        CASE  ( DATA_ORDER_XYZ )

          CALL nl_get_s_we( domain_id , sd1 )
          CALL nl_get_e_we( domain_id , ed1 )
          CALL nl_get_s_sn( domain_id , sd2 )
          CALL nl_get_e_sn( domain_id , ed2 )
          CALL nl_get_s_vert( domain_id , sd3 )
          CALL nl_get_e_vert( domain_id , ed3 )
          nx = ed1-sd1+1
          ny = ed2-sd2+1

        CASE  ( DATA_ORDER_YXZ )

          CALL nl_get_s_sn( domain_id , sd1 )
          CALL nl_get_e_sn( domain_id , ed1 )
          CALL nl_get_s_we( domain_id , sd2 )
          CALL nl_get_e_we( domain_id , ed2 )
          CALL nl_get_s_vert( domain_id , sd3 )
          CALL nl_get_e_vert( domain_id , ed3 )
          nx = ed2-sd2+1
          ny = ed1-sd1+1

        CASE  ( DATA_ORDER_ZXY )

          CALL nl_get_s_vert( domain_id , sd1 )
          CALL nl_get_e_vert( domain_id , ed1 )
          CALL nl_get_s_we( domain_id , sd2 )
          CALL nl_get_e_we( domain_id , ed2 )
          CALL nl_get_s_sn( domain_id , sd3 )
          CALL nl_get_e_sn( domain_id , ed3 )
          nx = ed2-sd2+1
          ny = ed3-sd3+1

        CASE  ( DATA_ORDER_ZYX )

          CALL nl_get_s_vert( domain_id , sd1 )
          CALL nl_get_e_vert( domain_id , ed1 )
          CALL nl_get_s_sn( domain_id , sd2 )
          CALL nl_get_e_sn( domain_id , ed2 )
          CALL nl_get_s_we( domain_id , sd3 )
          CALL nl_get_e_we( domain_id , ed3 )
          nx = ed3-sd3+1
          ny = ed2-sd2+1

        CASE  ( DATA_ORDER_XZY )

          CALL nl_get_s_we( domain_id , sd1 )
          CALL nl_get_e_we( domain_id , ed1 )
          CALL nl_get_s_vert( domain_id , sd2 )
          CALL nl_get_e_vert( domain_id , ed2 )
          CALL nl_get_s_sn( domain_id , sd3 )
          CALL nl_get_e_sn( domain_id , ed3 )
          nx = ed1-sd1+1
          ny = ed3-sd3+1

        CASE  ( DATA_ORDER_YZX )

          CALL nl_get_s_sn( domain_id , sd1 )
          CALL nl_get_e_sn( domain_id , ed1 )
          CALL nl_get_s_vert( domain_id , sd2 )
          CALL nl_get_e_vert( domain_id , ed2 )
          CALL nl_get_s_we( domain_id , sd3 )
          CALL nl_get_e_we( domain_id , ed3 )
          nx = ed3-sd3+1
          ny = ed1-sd1+1

      END SELECT data_ordering

      IF ( num_time_levels > 3 ) THEN
        WRITE ( wrf_err_message , * ) 'alloc_and_configure_domain: ', &
          'Incorrect value for num_time_levels ', num_time_levels
        CALL wrf_error_fatal3("",607,&
TRIM ( wrf_err_message ) )
      ENDIF

      IF (ASSOCIATED(parent)) THEN
        parent_id = parent%id
        parent_domdesc = parent%domdesc
      ELSE
        parent_id = -1
        parent_domdesc = -1
      ENDIF


      CALL get_bdyzone_x( bdyzone_x )
      CALL get_bdyzone_y( bdyzone_y )

      ALLOCATE ( new_grid )
      ALLOCATE ( new_grid%parents( max_parents ) )
      ALLOCATE ( new_grid%nests( max_nests ) )
      NULLIFY( new_grid%sibling )
      DO i = 1, max_nests
         NULLIFY( new_grid%nests(i)%ptr )
      ENDDO
      NULLIFY  (new_grid%next)
      NULLIFY  (new_grid%same_level)
      NULLIFY  (new_grid%i_start)
      NULLIFY  (new_grid%j_start)
      NULLIFY  (new_grid%i_end)
      NULLIFY  (new_grid%j_end)
      ALLOCATE( new_grid%domain_clock )
      new_grid%domain_clock_created = .FALSE.
      ALLOCATE( new_grid%alarms( MAX_WRF_ALARMS ) )    
      ALLOCATE( new_grid%alarms_created( MAX_WRF_ALARMS ) )
      DO i = 1, MAX_WRF_ALARMS
        new_grid%alarms_created( i ) = .FALSE.
      ENDDO
      new_grid%time_set = .FALSE.
      new_grid%is_intermediate = .FALSE.

      
      
      
      
      

 
      IF ( domain_id .NE. 1 ) THEN
         new_grid%parents(1)%ptr => parent
         new_grid%num_parents = 1
         parent%nests(kid)%ptr => new_grid
         new_grid%child_of_parent(1) = kid    
         parent%num_nests = parent%num_nests + 1
      END IF
      new_grid%id = domain_id                 

      CALL wrf_patch_domain( domain_id  , new_domdesc , parent, parent_id, parent_domdesc , &

                             sd1 , ed1 , sp1 , ep1 , sm1 , em1 , &     
                             sd2 , ed2 , sp2 , ep2 , sm2 , em2 , &     
                             sd3 , ed3 , sp3 , ep3 , sm3 , em3 , &

                                     sp1x , ep1x , sm1x , em1x , &     
                                     sp2x , ep2x , sm2x , em2x , &
                                     sp3x , ep3x , sm3x , em3x , &

                                     sp1y , ep1y , sm1y , em1y , &     
                                     sp2y , ep2y , sm2y , em2y , &
                                     sp3y , ep3y , sm3y , em3y , &

                         bdyzone_x  , bdyzone_y , new_grid%bdy_mask &
      ) 


      new_grid%domdesc = new_domdesc
      new_grid%num_nests = 0
      new_grid%num_siblings = 0
      new_grid%num_parents = 0
      new_grid%max_tiles   = 0
      new_grid%num_tiles_spec   = 0
      new_grid%nframes   = 0         

      CALL alloc_space_field ( new_grid, domain_id , 3 , 3 , .FALSE. ,      &
                               sd1, ed1, sd2, ed2, sd3, ed3,       &
                               sm1,  em1,  sm2,  em2,  sm3,  em3,  &
                               sm1x, em1x, sm2x, em2x, sm3x, em3x, &   
                               sm1y, em1y, sm2y, em2y, sm3y, em3y  &   
      )

      new_grid%sd31                            = sd1 
      new_grid%ed31                            = ed1
      new_grid%sp31                            = sp1 
      new_grid%ep31                            = ep1 
      new_grid%sm31                            = sm1 
      new_grid%em31                            = em1
      new_grid%sd32                            = sd2 
      new_grid%ed32                            = ed2
      new_grid%sp32                            = sp2 
      new_grid%ep32                            = ep2 
      new_grid%sm32                            = sm2 
      new_grid%em32                            = em2
      new_grid%sd33                            = sd3 
      new_grid%ed33                            = ed3
      new_grid%sp33                            = sp3 
      new_grid%ep33                            = ep3 
      new_grid%sm33                            = sm3 
      new_grid%em33                            = em3

      new_grid%sp31x                           = sp1x
      new_grid%ep31x                           = ep1x
      new_grid%sm31x                           = sm1x
      new_grid%em31x                           = em1x
      new_grid%sp32x                           = sp2x
      new_grid%ep32x                           = ep2x
      new_grid%sm32x                           = sm2x
      new_grid%em32x                           = em2x
      new_grid%sp33x                           = sp3x
      new_grid%ep33x                           = ep3x
      new_grid%sm33x                           = sm3x
      new_grid%em33x                           = em3x

      new_grid%sp31y                           = sp1y
      new_grid%ep31y                           = ep1y
      new_grid%sm31y                           = sm1y
      new_grid%em31y                           = em1y
      new_grid%sp32y                           = sp2y
      new_grid%ep32y                           = ep2y
      new_grid%sm32y                           = sm2y
      new_grid%em32y                           = em2y
      new_grid%sp33y                           = sp3y
      new_grid%ep33y                           = ep3y
      new_grid%sm33y                           = sm3y
      new_grid%em33y                           = em3y

      SELECT CASE ( model_data_order )
         CASE  ( DATA_ORDER_XYZ )
            new_grid%sd21 = sd1 ; new_grid%sd22 = sd2 ;
            new_grid%ed21 = ed1 ; new_grid%ed22 = ed2 ;
            new_grid%sp21 = sp1 ; new_grid%sp22 = sp2 ;
            new_grid%ep21 = ep1 ; new_grid%ep22 = ep2 ;
            new_grid%sm21 = sm1 ; new_grid%sm22 = sm2 ;
            new_grid%em21 = em1 ; new_grid%em22 = em2 ;
            new_grid%sd11 = sd1
            new_grid%ed11 = ed1
            new_grid%sp11 = sp1
            new_grid%ep11 = ep1
            new_grid%sm11 = sm1
            new_grid%em11 = em1
         CASE  ( DATA_ORDER_YXZ )
            new_grid%sd21 = sd1 ; new_grid%sd22 = sd2 ;
            new_grid%ed21 = ed1 ; new_grid%ed22 = ed2 ;
            new_grid%sp21 = sp1 ; new_grid%sp22 = sp2 ;
            new_grid%ep21 = ep1 ; new_grid%ep22 = ep2 ;
            new_grid%sm21 = sm1 ; new_grid%sm22 = sm2 ;
            new_grid%em21 = em1 ; new_grid%em22 = em2 ;
            new_grid%sd11 = sd1
            new_grid%ed11 = ed1
            new_grid%sp11 = sp1
            new_grid%ep11 = ep1
            new_grid%sm11 = sm1
            new_grid%em11 = em1
         CASE  ( DATA_ORDER_ZXY )
            new_grid%sd21 = sd2 ; new_grid%sd22 = sd3 ;
            new_grid%ed21 = ed2 ; new_grid%ed22 = ed3 ;
            new_grid%sp21 = sp2 ; new_grid%sp22 = sp3 ;
            new_grid%ep21 = ep2 ; new_grid%ep22 = ep3 ;
            new_grid%sm21 = sm2 ; new_grid%sm22 = sm3 ;
            new_grid%em21 = em2 ; new_grid%em22 = em3 ;
            new_grid%sd11 = sd2
            new_grid%ed11 = ed2
            new_grid%sp11 = sp2
            new_grid%ep11 = ep2
            new_grid%sm11 = sm2
            new_grid%em11 = em2
         CASE  ( DATA_ORDER_ZYX )
            new_grid%sd21 = sd2 ; new_grid%sd22 = sd3 ;
            new_grid%ed21 = ed2 ; new_grid%ed22 = ed3 ;
            new_grid%sp21 = sp2 ; new_grid%sp22 = sp3 ;
            new_grid%ep21 = ep2 ; new_grid%ep22 = ep3 ;
            new_grid%sm21 = sm2 ; new_grid%sm22 = sm3 ;
            new_grid%em21 = em2 ; new_grid%em22 = em3 ;
            new_grid%sd11 = sd2
            new_grid%ed11 = ed2
            new_grid%sp11 = sp2
            new_grid%ep11 = ep2
            new_grid%sm11 = sm2
            new_grid%em11 = em2
         CASE  ( DATA_ORDER_XZY )
            new_grid%sd21 = sd1 ; new_grid%sd22 = sd3 ;
            new_grid%ed21 = ed1 ; new_grid%ed22 = ed3 ;
            new_grid%sp21 = sp1 ; new_grid%sp22 = sp3 ;
            new_grid%ep21 = ep1 ; new_grid%ep22 = ep3 ;
            new_grid%sm21 = sm1 ; new_grid%sm22 = sm3 ;
            new_grid%em21 = em1 ; new_grid%em22 = em3 ;
            new_grid%sd11 = sd1
            new_grid%ed11 = ed1
            new_grid%sp11 = sp1
            new_grid%ep11 = ep1
            new_grid%sm11 = sm1
            new_grid%em11 = em1
         CASE  ( DATA_ORDER_YZX )
            new_grid%sd21 = sd1 ; new_grid%sd22 = sd3 ;
            new_grid%ed21 = ed1 ; new_grid%ed22 = ed3 ;
            new_grid%sp21 = sp1 ; new_grid%sp22 = sp3 ;
            new_grid%ep21 = ep1 ; new_grid%ep22 = ep3 ;
            new_grid%sm21 = sm1 ; new_grid%sm22 = sm3 ;
            new_grid%em21 = em1 ; new_grid%em22 = em3 ;
            new_grid%sd11 = sd1
            new_grid%ed11 = ed1
            new_grid%sp11 = sp1
            new_grid%ep11 = ep1
            new_grid%sm11 = sm1
            new_grid%em11 = em1
      END SELECT

      CALL med_add_config_info_to_grid ( new_grid )           



      new_grid%tiled                           = .false.
      new_grid%patched                         = .false.
      NULLIFY(new_grid%mapping)




      grid => new_grid


      ALLOCATE( grid%lattsloc( grid%max_ts_locs ) )
      ALLOCATE( grid%lontsloc( grid%max_ts_locs ) )
      ALLOCATE( grid%nametsloc( grid%max_ts_locs ) )
      ALLOCATE( grid%desctsloc( grid%max_ts_locs ) )
      ALLOCATE( grid%itsloc( grid%max_ts_locs ) )
      ALLOCATE( grid%jtsloc( grid%max_ts_locs ) )
      ALLOCATE( grid%id_tsloc( grid%max_ts_locs ) )
      ALLOCATE( grid%ts_filename( grid%max_ts_locs ) )
      grid%ntsloc        = 0
      grid%ntsloc_domain = 0

      CALL wrf_get_dm_communicator ( grid%communicator )
      CALL wrf_dm_define_comms( grid )

   END SUBROUTINE alloc_and_configure_domain









   SUBROUTINE alloc_space_field ( grid,   id, setinitval_in ,  tl_in , inter_domain_in ,   &
                                  sd31, ed31, sd32, ed32, sd33, ed33, &
                                  sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                  sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                  sm31y, em31y, sm32y, em32y, sm33y, em33y )

      USE module_alloc_space, ONLY : alloc_space_field_core
      IMPLICIT NONE

      

      TYPE(domain)               , POINTER          :: grid
      INTEGER , INTENT(IN)            :: id
      INTEGER , INTENT(IN)            :: setinitval_in   
      INTEGER , INTENT(IN)            :: sd31, ed31, sd32, ed32, sd33, ed33
      INTEGER , INTENT(IN)            :: sm31, em31, sm32, em32, sm33, em33
      INTEGER , INTENT(IN)            :: sm31x, em31x, sm32x, em32x, sm33x, em33x
      INTEGER , INTENT(IN)            :: sm31y, em31y, sm32y, em32y, sm33y, em33y

      
      
      
      
      INTEGER , INTENT(IN)            :: tl_in
  
      
      
      LOGICAL , INTENT(IN)            :: inter_domain_in

      
      CALL alloc_space_field_core ( grid,   id, setinitval_in ,  tl_in , inter_domain_in ,   &
                                    sd31, ed31, sd32, ed32, sd33, ed33, &
                                    sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                    sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                    sm31y, em31y, sm32y, em32y, sm33y, em33y )

   END SUBROUTINE alloc_space_field






   SUBROUTINE dealloc_space_domain ( id )
      
      IMPLICIT NONE

      

      INTEGER , INTENT(IN)            :: id

      

      TYPE(domain) , POINTER          :: grid
      LOGICAL                         :: found

      

      grid => head_grid
      old_grid => head_grid
      found = .FALSE.

      
      
      

      find_grid : DO WHILE ( ASSOCIATED(grid) ) 
         IF ( grid%id == id ) THEN
            found = .TRUE.
            old_grid%next => grid%next
            CALL domain_destroy( grid )
            EXIT find_grid
         END IF
         old_grid => grid
         grid     => grid%next
      END DO find_grid

      IF ( .NOT. found ) THEN
         WRITE ( wrf_err_message , * ) 'module_domain: ', &
           'dealloc_space_domain: Could not de-allocate grid id ',id
         CALL wrf_error_fatal3("",939,&
TRIM( wrf_err_message ) ) 
      END IF

   END SUBROUTINE dealloc_space_domain








   SUBROUTINE domain_destroy ( grid )
      
      IMPLICIT NONE

      

      TYPE(domain) , POINTER          :: grid

      CALL dealloc_space_field ( grid )
      DEALLOCATE( grid%parents )
      DEALLOCATE( grid%nests )
      
      CALL domain_clock_destroy( grid )
      CALL domain_alarms_destroy( grid )
      IF ( ASSOCIATED( grid%i_start ) ) THEN
        DEALLOCATE( grid%i_start ) 
      ENDIF
      IF ( ASSOCIATED( grid%i_end ) ) THEN
        DEALLOCATE( grid%i_end )
      ENDIF
      IF ( ASSOCIATED( grid%j_start ) ) THEN
        DEALLOCATE( grid%j_start )
      ENDIF
      IF ( ASSOCIATED( grid%j_end ) ) THEN
        DEALLOCATE( grid%j_end )
      ENDIF
      IF ( ASSOCIATED( grid%itsloc ) ) THEN
        DEALLOCATE( grid%itsloc )
      ENDIF 
      IF ( ASSOCIATED( grid%jtsloc ) ) THEN
        DEALLOCATE( grid%jtsloc )
      ENDIF 
      IF ( ASSOCIATED( grid%id_tsloc ) ) THEN
        DEALLOCATE( grid%id_tsloc )
      ENDIF 
      IF ( ASSOCIATED( grid%lattsloc ) ) THEN
        DEALLOCATE( grid%lattsloc )
      ENDIF 
      IF ( ASSOCIATED( grid%lontsloc ) ) THEN
        DEALLOCATE( grid%lontsloc )
      ENDIF 
      IF ( ASSOCIATED( grid%nametsloc ) ) THEN
        DEALLOCATE( grid%nametsloc )
      ENDIF 
      IF ( ASSOCIATED( grid%desctsloc ) ) THEN
        DEALLOCATE( grid%desctsloc )
      ENDIF 
      IF ( ASSOCIATED( grid%ts_filename ) ) THEN
        DEALLOCATE( grid%ts_filename )
      ENDIF 
      DEALLOCATE( grid )
      NULLIFY( grid )

   END SUBROUTINE domain_destroy

   RECURSIVE SUBROUTINE show_nest_subtree ( grid )
      TYPE(domain), POINTER :: grid
      INTEGER myid
      INTEGER kid
      IF ( .NOT. ASSOCIATED( grid ) ) RETURN
      myid = grid%id
      write(0,*)'show_nest_subtree ',myid
      DO kid = 1, max_nests
        IF ( ASSOCIATED( grid%nests(kid)%ptr ) ) THEN
          IF ( grid%nests(kid)%ptr%id .EQ. myid ) THEN
            CALL wrf_error_fatal3("",1017,&
'show_nest_subtree: nest hierarchy corrupted' )
          ENDIF
          CALL show_nest_subtree( grid%nests(kid)%ptr )
        ENDIF
      ENDDO
   END SUBROUTINE show_nest_subtree
   







   SUBROUTINE dealloc_space_field ( grid )
      
      IMPLICIT NONE

      

      TYPE(domain)              , POINTER :: grid

      

      INTEGER                             ::  ierr







IF ( ASSOCIATED( grid%x_1 ) ) THEN 
  DEALLOCATE(grid%x_1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1053,&
'frame/module_domain.f: Failed to dallocate grid%x_1. ')
 endif
  NULLIFY(grid%x_1)
ENDIF
IF ( ASSOCIATED( grid%x_2 ) ) THEN 
  DEALLOCATE(grid%x_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1061,&
'frame/module_domain.f: Failed to dallocate grid%x_2. ')
 endif
  NULLIFY(grid%x_2)
ENDIF
IF ( ASSOCIATED( grid%lu_index ) ) THEN 
  DEALLOCATE(grid%lu_index,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1069,&
'frame/module_domain.f: Failed to dallocate grid%lu_index. ')
 endif
  NULLIFY(grid%lu_index)
ENDIF
IF ( ASSOCIATED( grid%lu_mask ) ) THEN 
  DEALLOCATE(grid%lu_mask,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1077,&
'frame/module_domain.f: Failed to dallocate grid%lu_mask. ')
 endif
  NULLIFY(grid%lu_mask)
ENDIF
IF ( ASSOCIATED( grid%p_gc ) ) THEN 
  DEALLOCATE(grid%p_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1085,&
'frame/module_domain.f: Failed to dallocate grid%p_gc. ')
 endif
  NULLIFY(grid%p_gc)
ENDIF
IF ( ASSOCIATED( grid%vegcat ) ) THEN 
  DEALLOCATE(grid%vegcat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1093,&
'frame/module_domain.f: Failed to dallocate grid%vegcat. ')
 endif
  NULLIFY(grid%vegcat)
ENDIF
IF ( ASSOCIATED( grid%soilcat ) ) THEN 
  DEALLOCATE(grid%soilcat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1101,&
'frame/module_domain.f: Failed to dallocate grid%soilcat. ')
 endif
  NULLIFY(grid%soilcat)
ENDIF
IF ( ASSOCIATED( grid%input_soil_cat ) ) THEN 
  DEALLOCATE(grid%input_soil_cat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1109,&
'frame/module_domain.f: Failed to dallocate grid%input_soil_cat. ')
 endif
  NULLIFY(grid%input_soil_cat)
ENDIF
IF ( ASSOCIATED( grid%tsk_gc ) ) THEN 
  DEALLOCATE(grid%tsk_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1117,&
'frame/module_domain.f: Failed to dallocate grid%tsk_gc. ')
 endif
  NULLIFY(grid%tsk_gc)
ENDIF
IF ( ASSOCIATED( grid%xice_gc ) ) THEN 
  DEALLOCATE(grid%xice_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1125,&
'frame/module_domain.f: Failed to dallocate grid%xice_gc. ')
 endif
  NULLIFY(grid%xice_gc)
ENDIF
IF ( ASSOCIATED( grid%ght_gc ) ) THEN 
  DEALLOCATE(grid%ght_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1133,&
'frame/module_domain.f: Failed to dallocate grid%ght_gc. ')
 endif
  NULLIFY(grid%ght_gc)
ENDIF
IF ( ASSOCIATED( grid%rh_gc ) ) THEN 
  DEALLOCATE(grid%rh_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1141,&
'frame/module_domain.f: Failed to dallocate grid%rh_gc. ')
 endif
  NULLIFY(grid%rh_gc)
ENDIF
IF ( ASSOCIATED( grid%v_gc ) ) THEN 
  DEALLOCATE(grid%v_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1149,&
'frame/module_domain.f: Failed to dallocate grid%v_gc. ')
 endif
  NULLIFY(grid%v_gc)
ENDIF
IF ( ASSOCIATED( grid%u_gc ) ) THEN 
  DEALLOCATE(grid%u_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1157,&
'frame/module_domain.f: Failed to dallocate grid%u_gc. ')
 endif
  NULLIFY(grid%u_gc)
ENDIF
IF ( ASSOCIATED( grid%t_gc ) ) THEN 
  DEALLOCATE(grid%t_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1165,&
'frame/module_domain.f: Failed to dallocate grid%t_gc. ')
 endif
  NULLIFY(grid%t_gc)
ENDIF
IF ( ASSOCIATED( grid%snoalb ) ) THEN 
  DEALLOCATE(grid%snoalb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1173,&
'frame/module_domain.f: Failed to dallocate grid%snoalb. ')
 endif
  NULLIFY(grid%snoalb)
ENDIF
IF ( ASSOCIATED( grid%greenfrac_gc ) ) THEN 
  DEALLOCATE(grid%greenfrac_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1181,&
'frame/module_domain.f: Failed to dallocate grid%greenfrac_gc. ')
 endif
  NULLIFY(grid%greenfrac_gc)
ENDIF
IF ( ASSOCIATED( grid%albedo12m_gc ) ) THEN 
  DEALLOCATE(grid%albedo12m_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1189,&
'frame/module_domain.f: Failed to dallocate grid%albedo12m_gc. ')
 endif
  NULLIFY(grid%albedo12m_gc)
ENDIF
IF ( ASSOCIATED( grid%soilcbot_gc ) ) THEN 
  DEALLOCATE(grid%soilcbot_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1197,&
'frame/module_domain.f: Failed to dallocate grid%soilcbot_gc. ')
 endif
  NULLIFY(grid%soilcbot_gc)
ENDIF
IF ( ASSOCIATED( grid%soilctop_gc ) ) THEN 
  DEALLOCATE(grid%soilctop_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1205,&
'frame/module_domain.f: Failed to dallocate grid%soilctop_gc. ')
 endif
  NULLIFY(grid%soilctop_gc)
ENDIF
IF ( ASSOCIATED( grid%tmn_gc ) ) THEN 
  DEALLOCATE(grid%tmn_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1213,&
'frame/module_domain.f: Failed to dallocate grid%tmn_gc. ')
 endif
  NULLIFY(grid%tmn_gc)
ENDIF
IF ( ASSOCIATED( grid%htv_gc ) ) THEN 
  DEALLOCATE(grid%htv_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1221,&
'frame/module_domain.f: Failed to dallocate grid%htv_gc. ')
 endif
  NULLIFY(grid%htv_gc)
ENDIF
IF ( ASSOCIATED( grid%ht_gc ) ) THEN 
  DEALLOCATE(grid%ht_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1229,&
'frame/module_domain.f: Failed to dallocate grid%ht_gc. ')
 endif
  NULLIFY(grid%ht_gc)
ENDIF
IF ( ASSOCIATED( grid%landusef_gc ) ) THEN 
  DEALLOCATE(grid%landusef_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1237,&
'frame/module_domain.f: Failed to dallocate grid%landusef_gc. ')
 endif
  NULLIFY(grid%landusef_gc)
ENDIF
IF ( ASSOCIATED( grid%vlon_gc ) ) THEN 
  DEALLOCATE(grid%vlon_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1245,&
'frame/module_domain.f: Failed to dallocate grid%vlon_gc. ')
 endif
  NULLIFY(grid%vlon_gc)
ENDIF
IF ( ASSOCIATED( grid%vlat_gc ) ) THEN 
  DEALLOCATE(grid%vlat_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1253,&
'frame/module_domain.f: Failed to dallocate grid%vlat_gc. ')
 endif
  NULLIFY(grid%vlat_gc)
ENDIF
IF ( ASSOCIATED( grid%hlon_gc ) ) THEN 
  DEALLOCATE(grid%hlon_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1261,&
'frame/module_domain.f: Failed to dallocate grid%hlon_gc. ')
 endif
  NULLIFY(grid%hlon_gc)
ENDIF
IF ( ASSOCIATED( grid%hlat_gc ) ) THEN 
  DEALLOCATE(grid%hlat_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1269,&
'frame/module_domain.f: Failed to dallocate grid%hlat_gc. ')
 endif
  NULLIFY(grid%hlat_gc)
ENDIF
IF ( ASSOCIATED( grid%hbm2 ) ) THEN 
  DEALLOCATE(grid%hbm2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1277,&
'frame/module_domain.f: Failed to dallocate grid%hbm2. ')
 endif
  NULLIFY(grid%hbm2)
ENDIF
IF ( ASSOCIATED( grid%hbm3 ) ) THEN 
  DEALLOCATE(grid%hbm3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1285,&
'frame/module_domain.f: Failed to dallocate grid%hbm3. ')
 endif
  NULLIFY(grid%hbm3)
ENDIF
IF ( ASSOCIATED( grid%vbm2 ) ) THEN 
  DEALLOCATE(grid%vbm2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1293,&
'frame/module_domain.f: Failed to dallocate grid%vbm2. ')
 endif
  NULLIFY(grid%vbm2)
ENDIF
IF ( ASSOCIATED( grid%vbm3 ) ) THEN 
  DEALLOCATE(grid%vbm3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1301,&
'frame/module_domain.f: Failed to dallocate grid%vbm3. ')
 endif
  NULLIFY(grid%vbm3)
ENDIF
IF ( ASSOCIATED( grid%sm ) ) THEN 
  DEALLOCATE(grid%sm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1309,&
'frame/module_domain.f: Failed to dallocate grid%sm. ')
 endif
  NULLIFY(grid%sm)
ENDIF
IF ( ASSOCIATED( grid%sice ) ) THEN 
  DEALLOCATE(grid%sice,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1317,&
'frame/module_domain.f: Failed to dallocate grid%sice. ')
 endif
  NULLIFY(grid%sice)
ENDIF
IF ( ASSOCIATED( grid%pd ) ) THEN 
  DEALLOCATE(grid%pd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1325,&
'frame/module_domain.f: Failed to dallocate grid%pd. ')
 endif
  NULLIFY(grid%pd)
ENDIF
IF ( ASSOCIATED( grid%pd_bxs ) ) THEN 
  DEALLOCATE(grid%pd_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1333,&
'frame/module_domain.f: Failed to dallocate grid%pd_bxs. ')
 endif
  NULLIFY(grid%pd_bxs)
ENDIF
IF ( ASSOCIATED( grid%pd_bxe ) ) THEN 
  DEALLOCATE(grid%pd_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1341,&
'frame/module_domain.f: Failed to dallocate grid%pd_bxe. ')
 endif
  NULLIFY(grid%pd_bxe)
ENDIF
IF ( ASSOCIATED( grid%pd_bys ) ) THEN 
  DEALLOCATE(grid%pd_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1349,&
'frame/module_domain.f: Failed to dallocate grid%pd_bys. ')
 endif
  NULLIFY(grid%pd_bys)
ENDIF
IF ( ASSOCIATED( grid%pd_bye ) ) THEN 
  DEALLOCATE(grid%pd_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1357,&
'frame/module_domain.f: Failed to dallocate grid%pd_bye. ')
 endif
  NULLIFY(grid%pd_bye)
ENDIF
IF ( ASSOCIATED( grid%pd_btxs ) ) THEN 
  DEALLOCATE(grid%pd_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1365,&
'frame/module_domain.f: Failed to dallocate grid%pd_btxs. ')
 endif
  NULLIFY(grid%pd_btxs)
ENDIF
IF ( ASSOCIATED( grid%pd_btxe ) ) THEN 
  DEALLOCATE(grid%pd_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1373,&
'frame/module_domain.f: Failed to dallocate grid%pd_btxe. ')
 endif
  NULLIFY(grid%pd_btxe)
ENDIF
IF ( ASSOCIATED( grid%pd_btys ) ) THEN 
  DEALLOCATE(grid%pd_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1381,&
'frame/module_domain.f: Failed to dallocate grid%pd_btys. ')
 endif
  NULLIFY(grid%pd_btys)
ENDIF
IF ( ASSOCIATED( grid%pd_btye ) ) THEN 
  DEALLOCATE(grid%pd_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1389,&
'frame/module_domain.f: Failed to dallocate grid%pd_btye. ')
 endif
  NULLIFY(grid%pd_btye)
ENDIF
IF ( ASSOCIATED( grid%fis ) ) THEN 
  DEALLOCATE(grid%fis,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1397,&
'frame/module_domain.f: Failed to dallocate grid%fis. ')
 endif
  NULLIFY(grid%fis)
ENDIF
IF ( ASSOCIATED( grid%res ) ) THEN 
  DEALLOCATE(grid%res,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1405,&
'frame/module_domain.f: Failed to dallocate grid%res. ')
 endif
  NULLIFY(grid%res)
ENDIF
IF ( ASSOCIATED( grid%t ) ) THEN 
  DEALLOCATE(grid%t,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1413,&
'frame/module_domain.f: Failed to dallocate grid%t. ')
 endif
  NULLIFY(grid%t)
ENDIF
IF ( ASSOCIATED( grid%t_bxs ) ) THEN 
  DEALLOCATE(grid%t_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1421,&
'frame/module_domain.f: Failed to dallocate grid%t_bxs. ')
 endif
  NULLIFY(grid%t_bxs)
ENDIF
IF ( ASSOCIATED( grid%t_bxe ) ) THEN 
  DEALLOCATE(grid%t_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1429,&
'frame/module_domain.f: Failed to dallocate grid%t_bxe. ')
 endif
  NULLIFY(grid%t_bxe)
ENDIF
IF ( ASSOCIATED( grid%t_bys ) ) THEN 
  DEALLOCATE(grid%t_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1437,&
'frame/module_domain.f: Failed to dallocate grid%t_bys. ')
 endif
  NULLIFY(grid%t_bys)
ENDIF
IF ( ASSOCIATED( grid%t_bye ) ) THEN 
  DEALLOCATE(grid%t_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1445,&
'frame/module_domain.f: Failed to dallocate grid%t_bye. ')
 endif
  NULLIFY(grid%t_bye)
ENDIF
IF ( ASSOCIATED( grid%t_btxs ) ) THEN 
  DEALLOCATE(grid%t_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1453,&
'frame/module_domain.f: Failed to dallocate grid%t_btxs. ')
 endif
  NULLIFY(grid%t_btxs)
ENDIF
IF ( ASSOCIATED( grid%t_btxe ) ) THEN 
  DEALLOCATE(grid%t_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1461,&
'frame/module_domain.f: Failed to dallocate grid%t_btxe. ')
 endif
  NULLIFY(grid%t_btxe)
ENDIF
IF ( ASSOCIATED( grid%t_btys ) ) THEN 
  DEALLOCATE(grid%t_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1469,&
'frame/module_domain.f: Failed to dallocate grid%t_btys. ')
 endif
  NULLIFY(grid%t_btys)
ENDIF
IF ( ASSOCIATED( grid%t_btye ) ) THEN 
  DEALLOCATE(grid%t_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1477,&
'frame/module_domain.f: Failed to dallocate grid%t_btye. ')
 endif
  NULLIFY(grid%t_btye)
ENDIF
IF ( ASSOCIATED( grid%q ) ) THEN 
  DEALLOCATE(grid%q,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1485,&
'frame/module_domain.f: Failed to dallocate grid%q. ')
 endif
  NULLIFY(grid%q)
ENDIF
IF ( ASSOCIATED( grid%q_bxs ) ) THEN 
  DEALLOCATE(grid%q_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1493,&
'frame/module_domain.f: Failed to dallocate grid%q_bxs. ')
 endif
  NULLIFY(grid%q_bxs)
ENDIF
IF ( ASSOCIATED( grid%q_bxe ) ) THEN 
  DEALLOCATE(grid%q_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1501,&
'frame/module_domain.f: Failed to dallocate grid%q_bxe. ')
 endif
  NULLIFY(grid%q_bxe)
ENDIF
IF ( ASSOCIATED( grid%q_bys ) ) THEN 
  DEALLOCATE(grid%q_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1509,&
'frame/module_domain.f: Failed to dallocate grid%q_bys. ')
 endif
  NULLIFY(grid%q_bys)
ENDIF
IF ( ASSOCIATED( grid%q_bye ) ) THEN 
  DEALLOCATE(grid%q_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1517,&
'frame/module_domain.f: Failed to dallocate grid%q_bye. ')
 endif
  NULLIFY(grid%q_bye)
ENDIF
IF ( ASSOCIATED( grid%q_btxs ) ) THEN 
  DEALLOCATE(grid%q_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1525,&
'frame/module_domain.f: Failed to dallocate grid%q_btxs. ')
 endif
  NULLIFY(grid%q_btxs)
ENDIF
IF ( ASSOCIATED( grid%q_btxe ) ) THEN 
  DEALLOCATE(grid%q_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1533,&
'frame/module_domain.f: Failed to dallocate grid%q_btxe. ')
 endif
  NULLIFY(grid%q_btxe)
ENDIF
IF ( ASSOCIATED( grid%q_btys ) ) THEN 
  DEALLOCATE(grid%q_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1541,&
'frame/module_domain.f: Failed to dallocate grid%q_btys. ')
 endif
  NULLIFY(grid%q_btys)
ENDIF
IF ( ASSOCIATED( grid%q_btye ) ) THEN 
  DEALLOCATE(grid%q_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1549,&
'frame/module_domain.f: Failed to dallocate grid%q_btye. ')
 endif
  NULLIFY(grid%q_btye)
ENDIF
IF ( ASSOCIATED( grid%u ) ) THEN 
  DEALLOCATE(grid%u,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1557,&
'frame/module_domain.f: Failed to dallocate grid%u. ')
 endif
  NULLIFY(grid%u)
ENDIF
IF ( ASSOCIATED( grid%u_bxs ) ) THEN 
  DEALLOCATE(grid%u_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1565,&
'frame/module_domain.f: Failed to dallocate grid%u_bxs. ')
 endif
  NULLIFY(grid%u_bxs)
ENDIF
IF ( ASSOCIATED( grid%u_bxe ) ) THEN 
  DEALLOCATE(grid%u_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1573,&
'frame/module_domain.f: Failed to dallocate grid%u_bxe. ')
 endif
  NULLIFY(grid%u_bxe)
ENDIF
IF ( ASSOCIATED( grid%u_bys ) ) THEN 
  DEALLOCATE(grid%u_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1581,&
'frame/module_domain.f: Failed to dallocate grid%u_bys. ')
 endif
  NULLIFY(grid%u_bys)
ENDIF
IF ( ASSOCIATED( grid%u_bye ) ) THEN 
  DEALLOCATE(grid%u_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1589,&
'frame/module_domain.f: Failed to dallocate grid%u_bye. ')
 endif
  NULLIFY(grid%u_bye)
ENDIF
IF ( ASSOCIATED( grid%u_btxs ) ) THEN 
  DEALLOCATE(grid%u_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1597,&
'frame/module_domain.f: Failed to dallocate grid%u_btxs. ')
 endif
  NULLIFY(grid%u_btxs)
ENDIF
IF ( ASSOCIATED( grid%u_btxe ) ) THEN 
  DEALLOCATE(grid%u_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1605,&
'frame/module_domain.f: Failed to dallocate grid%u_btxe. ')
 endif
  NULLIFY(grid%u_btxe)
ENDIF
IF ( ASSOCIATED( grid%u_btys ) ) THEN 
  DEALLOCATE(grid%u_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1613,&
'frame/module_domain.f: Failed to dallocate grid%u_btys. ')
 endif
  NULLIFY(grid%u_btys)
ENDIF
IF ( ASSOCIATED( grid%u_btye ) ) THEN 
  DEALLOCATE(grid%u_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1621,&
'frame/module_domain.f: Failed to dallocate grid%u_btye. ')
 endif
  NULLIFY(grid%u_btye)
ENDIF
IF ( ASSOCIATED( grid%v ) ) THEN 
  DEALLOCATE(grid%v,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1629,&
'frame/module_domain.f: Failed to dallocate grid%v. ')
 endif
  NULLIFY(grid%v)
ENDIF
IF ( ASSOCIATED( grid%v_bxs ) ) THEN 
  DEALLOCATE(grid%v_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1637,&
'frame/module_domain.f: Failed to dallocate grid%v_bxs. ')
 endif
  NULLIFY(grid%v_bxs)
ENDIF
IF ( ASSOCIATED( grid%v_bxe ) ) THEN 
  DEALLOCATE(grid%v_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1645,&
'frame/module_domain.f: Failed to dallocate grid%v_bxe. ')
 endif
  NULLIFY(grid%v_bxe)
ENDIF
IF ( ASSOCIATED( grid%v_bys ) ) THEN 
  DEALLOCATE(grid%v_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1653,&
'frame/module_domain.f: Failed to dallocate grid%v_bys. ')
 endif
  NULLIFY(grid%v_bys)
ENDIF
IF ( ASSOCIATED( grid%v_bye ) ) THEN 
  DEALLOCATE(grid%v_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1661,&
'frame/module_domain.f: Failed to dallocate grid%v_bye. ')
 endif
  NULLIFY(grid%v_bye)
ENDIF
IF ( ASSOCIATED( grid%v_btxs ) ) THEN 
  DEALLOCATE(grid%v_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1669,&
'frame/module_domain.f: Failed to dallocate grid%v_btxs. ')
 endif
  NULLIFY(grid%v_btxs)
ENDIF
IF ( ASSOCIATED( grid%v_btxe ) ) THEN 
  DEALLOCATE(grid%v_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1677,&
'frame/module_domain.f: Failed to dallocate grid%v_btxe. ')
 endif
  NULLIFY(grid%v_btxe)
ENDIF
IF ( ASSOCIATED( grid%v_btys ) ) THEN 
  DEALLOCATE(grid%v_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1685,&
'frame/module_domain.f: Failed to dallocate grid%v_btys. ')
 endif
  NULLIFY(grid%v_btys)
ENDIF
IF ( ASSOCIATED( grid%v_btye ) ) THEN 
  DEALLOCATE(grid%v_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1693,&
'frame/module_domain.f: Failed to dallocate grid%v_btye. ')
 endif
  NULLIFY(grid%v_btye)
ENDIF
IF ( ASSOCIATED( grid%told ) ) THEN 
  DEALLOCATE(grid%told,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1701,&
'frame/module_domain.f: Failed to dallocate grid%told. ')
 endif
  NULLIFY(grid%told)
ENDIF
IF ( ASSOCIATED( grid%uold ) ) THEN 
  DEALLOCATE(grid%uold,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1709,&
'frame/module_domain.f: Failed to dallocate grid%uold. ')
 endif
  NULLIFY(grid%uold)
ENDIF
IF ( ASSOCIATED( grid%vold ) ) THEN 
  DEALLOCATE(grid%vold,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1717,&
'frame/module_domain.f: Failed to dallocate grid%vold. ')
 endif
  NULLIFY(grid%vold)
ENDIF
IF ( ASSOCIATED( grid%hcoeff ) ) THEN 
  DEALLOCATE(grid%hcoeff,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1725,&
'frame/module_domain.f: Failed to dallocate grid%hcoeff. ')
 endif
  NULLIFY(grid%hcoeff)
ENDIF
IF ( ASSOCIATED( grid%dfi_pd ) ) THEN 
  DEALLOCATE(grid%dfi_pd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1733,&
'frame/module_domain.f: Failed to dallocate grid%dfi_pd. ')
 endif
  NULLIFY(grid%dfi_pd)
ENDIF
IF ( ASSOCIATED( grid%dfi_pint ) ) THEN 
  DEALLOCATE(grid%dfi_pint,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1741,&
'frame/module_domain.f: Failed to dallocate grid%dfi_pint. ')
 endif
  NULLIFY(grid%dfi_pint)
ENDIF
IF ( ASSOCIATED( grid%dfi_dwdt ) ) THEN 
  DEALLOCATE(grid%dfi_dwdt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1749,&
'frame/module_domain.f: Failed to dallocate grid%dfi_dwdt. ')
 endif
  NULLIFY(grid%dfi_dwdt)
ENDIF
IF ( ASSOCIATED( grid%dfi_t ) ) THEN 
  DEALLOCATE(grid%dfi_t,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1757,&
'frame/module_domain.f: Failed to dallocate grid%dfi_t. ')
 endif
  NULLIFY(grid%dfi_t)
ENDIF
IF ( ASSOCIATED( grid%dfi_q ) ) THEN 
  DEALLOCATE(grid%dfi_q,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1765,&
'frame/module_domain.f: Failed to dallocate grid%dfi_q. ')
 endif
  NULLIFY(grid%dfi_q)
ENDIF
IF ( ASSOCIATED( grid%dfi_u ) ) THEN 
  DEALLOCATE(grid%dfi_u,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1773,&
'frame/module_domain.f: Failed to dallocate grid%dfi_u. ')
 endif
  NULLIFY(grid%dfi_u)
ENDIF
IF ( ASSOCIATED( grid%dfi_v ) ) THEN 
  DEALLOCATE(grid%dfi_v,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1781,&
'frame/module_domain.f: Failed to dallocate grid%dfi_v. ')
 endif
  NULLIFY(grid%dfi_v)
ENDIF
IF ( ASSOCIATED( grid%dfi_q2 ) ) THEN 
  DEALLOCATE(grid%dfi_q2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1789,&
'frame/module_domain.f: Failed to dallocate grid%dfi_q2. ')
 endif
  NULLIFY(grid%dfi_q2)
ENDIF
IF ( ASSOCIATED( grid%dfi_cwm ) ) THEN 
  DEALLOCATE(grid%dfi_cwm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1797,&
'frame/module_domain.f: Failed to dallocate grid%dfi_cwm. ')
 endif
  NULLIFY(grid%dfi_cwm)
ENDIF
IF ( ASSOCIATED( grid%dfi_rrw ) ) THEN 
  DEALLOCATE(grid%dfi_rrw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1805,&
'frame/module_domain.f: Failed to dallocate grid%dfi_rrw. ')
 endif
  NULLIFY(grid%dfi_rrw)
ENDIF
IF ( ASSOCIATED( grid%dfi_stc ) ) THEN 
  DEALLOCATE(grid%dfi_stc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1813,&
'frame/module_domain.f: Failed to dallocate grid%dfi_stc. ')
 endif
  NULLIFY(grid%dfi_stc)
ENDIF
IF ( ASSOCIATED( grid%dfi_smc ) ) THEN 
  DEALLOCATE(grid%dfi_smc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1821,&
'frame/module_domain.f: Failed to dallocate grid%dfi_smc. ')
 endif
  NULLIFY(grid%dfi_smc)
ENDIF
IF ( ASSOCIATED( grid%dfi_sh2o ) ) THEN 
  DEALLOCATE(grid%dfi_sh2o,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1829,&
'frame/module_domain.f: Failed to dallocate grid%dfi_sh2o. ')
 endif
  NULLIFY(grid%dfi_sh2o)
ENDIF
IF ( ASSOCIATED( grid%dfi_snow ) ) THEN 
  DEALLOCATE(grid%dfi_snow,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1837,&
'frame/module_domain.f: Failed to dallocate grid%dfi_snow. ')
 endif
  NULLIFY(grid%dfi_snow)
ENDIF
IF ( ASSOCIATED( grid%dfi_snowh ) ) THEN 
  DEALLOCATE(grid%dfi_snowh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1845,&
'frame/module_domain.f: Failed to dallocate grid%dfi_snowh. ')
 endif
  NULLIFY(grid%dfi_snowh)
ENDIF
IF ( ASSOCIATED( grid%dfi_canwat ) ) THEN 
  DEALLOCATE(grid%dfi_canwat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1853,&
'frame/module_domain.f: Failed to dallocate grid%dfi_canwat. ')
 endif
  NULLIFY(grid%dfi_canwat)
ENDIF
IF ( ASSOCIATED( grid%dfi_nmm_tsk ) ) THEN 
  DEALLOCATE(grid%dfi_nmm_tsk,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1861,&
'frame/module_domain.f: Failed to dallocate grid%dfi_nmm_tsk. ')
 endif
  NULLIFY(grid%dfi_nmm_tsk)
ENDIF
IF ( ASSOCIATED( grid%dfi_snowc ) ) THEN 
  DEALLOCATE(grid%dfi_snowc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1869,&
'frame/module_domain.f: Failed to dallocate grid%dfi_snowc. ')
 endif
  NULLIFY(grid%dfi_snowc)
ENDIF
IF ( ASSOCIATED( grid%dx_nmm ) ) THEN 
  DEALLOCATE(grid%dx_nmm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1877,&
'frame/module_domain.f: Failed to dallocate grid%dx_nmm. ')
 endif
  NULLIFY(grid%dx_nmm)
ENDIF
IF ( ASSOCIATED( grid%wpdar ) ) THEN 
  DEALLOCATE(grid%wpdar,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1885,&
'frame/module_domain.f: Failed to dallocate grid%wpdar. ')
 endif
  NULLIFY(grid%wpdar)
ENDIF
IF ( ASSOCIATED( grid%cpgfu ) ) THEN 
  DEALLOCATE(grid%cpgfu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1893,&
'frame/module_domain.f: Failed to dallocate grid%cpgfu. ')
 endif
  NULLIFY(grid%cpgfu)
ENDIF
IF ( ASSOCIATED( grid%curv ) ) THEN 
  DEALLOCATE(grid%curv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1901,&
'frame/module_domain.f: Failed to dallocate grid%curv. ')
 endif
  NULLIFY(grid%curv)
ENDIF
IF ( ASSOCIATED( grid%fcp ) ) THEN 
  DEALLOCATE(grid%fcp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1909,&
'frame/module_domain.f: Failed to dallocate grid%fcp. ')
 endif
  NULLIFY(grid%fcp)
ENDIF
IF ( ASSOCIATED( grid%fdiv ) ) THEN 
  DEALLOCATE(grid%fdiv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1917,&
'frame/module_domain.f: Failed to dallocate grid%fdiv. ')
 endif
  NULLIFY(grid%fdiv)
ENDIF
IF ( ASSOCIATED( grid%f ) ) THEN 
  DEALLOCATE(grid%f,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1925,&
'frame/module_domain.f: Failed to dallocate grid%f. ')
 endif
  NULLIFY(grid%f)
ENDIF
IF ( ASSOCIATED( grid%fad ) ) THEN 
  DEALLOCATE(grid%fad,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1933,&
'frame/module_domain.f: Failed to dallocate grid%fad. ')
 endif
  NULLIFY(grid%fad)
ENDIF
IF ( ASSOCIATED( grid%ddmpu ) ) THEN 
  DEALLOCATE(grid%ddmpu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1941,&
'frame/module_domain.f: Failed to dallocate grid%ddmpu. ')
 endif
  NULLIFY(grid%ddmpu)
ENDIF
IF ( ASSOCIATED( grid%ddmpv ) ) THEN 
  DEALLOCATE(grid%ddmpv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1949,&
'frame/module_domain.f: Failed to dallocate grid%ddmpv. ')
 endif
  NULLIFY(grid%ddmpv)
ENDIF
IF ( ASSOCIATED( grid%deta ) ) THEN 
  DEALLOCATE(grid%deta,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1957,&
'frame/module_domain.f: Failed to dallocate grid%deta. ')
 endif
  NULLIFY(grid%deta)
ENDIF
IF ( ASSOCIATED( grid%rdeta ) ) THEN 
  DEALLOCATE(grid%rdeta,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1965,&
'frame/module_domain.f: Failed to dallocate grid%rdeta. ')
 endif
  NULLIFY(grid%rdeta)
ENDIF
IF ( ASSOCIATED( grid%aeta ) ) THEN 
  DEALLOCATE(grid%aeta,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1973,&
'frame/module_domain.f: Failed to dallocate grid%aeta. ')
 endif
  NULLIFY(grid%aeta)
ENDIF
IF ( ASSOCIATED( grid%f4q2 ) ) THEN 
  DEALLOCATE(grid%f4q2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1981,&
'frame/module_domain.f: Failed to dallocate grid%f4q2. ')
 endif
  NULLIFY(grid%f4q2)
ENDIF
IF ( ASSOCIATED( grid%etax ) ) THEN 
  DEALLOCATE(grid%etax,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1989,&
'frame/module_domain.f: Failed to dallocate grid%etax. ')
 endif
  NULLIFY(grid%etax)
ENDIF
IF ( ASSOCIATED( grid%dfl ) ) THEN 
  DEALLOCATE(grid%dfl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",1997,&
'frame/module_domain.f: Failed to dallocate grid%dfl. ')
 endif
  NULLIFY(grid%dfl)
ENDIF
IF ( ASSOCIATED( grid%deta1 ) ) THEN 
  DEALLOCATE(grid%deta1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2005,&
'frame/module_domain.f: Failed to dallocate grid%deta1. ')
 endif
  NULLIFY(grid%deta1)
ENDIF
IF ( ASSOCIATED( grid%aeta1 ) ) THEN 
  DEALLOCATE(grid%aeta1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2013,&
'frame/module_domain.f: Failed to dallocate grid%aeta1. ')
 endif
  NULLIFY(grid%aeta1)
ENDIF
IF ( ASSOCIATED( grid%eta1 ) ) THEN 
  DEALLOCATE(grid%eta1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2021,&
'frame/module_domain.f: Failed to dallocate grid%eta1. ')
 endif
  NULLIFY(grid%eta1)
ENDIF
IF ( ASSOCIATED( grid%deta2 ) ) THEN 
  DEALLOCATE(grid%deta2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2029,&
'frame/module_domain.f: Failed to dallocate grid%deta2. ')
 endif
  NULLIFY(grid%deta2)
ENDIF
IF ( ASSOCIATED( grid%aeta2 ) ) THEN 
  DEALLOCATE(grid%aeta2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2037,&
'frame/module_domain.f: Failed to dallocate grid%aeta2. ')
 endif
  NULLIFY(grid%aeta2)
ENDIF
IF ( ASSOCIATED( grid%eta2 ) ) THEN 
  DEALLOCATE(grid%eta2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2045,&
'frame/module_domain.f: Failed to dallocate grid%eta2. ')
 endif
  NULLIFY(grid%eta2)
ENDIF
IF ( ASSOCIATED( grid%em ) ) THEN 
  DEALLOCATE(grid%em,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2053,&
'frame/module_domain.f: Failed to dallocate grid%em. ')
 endif
  NULLIFY(grid%em)
ENDIF
IF ( ASSOCIATED( grid%emt ) ) THEN 
  DEALLOCATE(grid%emt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2061,&
'frame/module_domain.f: Failed to dallocate grid%emt. ')
 endif
  NULLIFY(grid%emt)
ENDIF
IF ( ASSOCIATED( grid%adt ) ) THEN 
  DEALLOCATE(grid%adt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2069,&
'frame/module_domain.f: Failed to dallocate grid%adt. ')
 endif
  NULLIFY(grid%adt)
ENDIF
IF ( ASSOCIATED( grid%adu ) ) THEN 
  DEALLOCATE(grid%adu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2077,&
'frame/module_domain.f: Failed to dallocate grid%adu. ')
 endif
  NULLIFY(grid%adu)
ENDIF
IF ( ASSOCIATED( grid%adv ) ) THEN 
  DEALLOCATE(grid%adv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2085,&
'frame/module_domain.f: Failed to dallocate grid%adv. ')
 endif
  NULLIFY(grid%adv)
ENDIF
IF ( ASSOCIATED( grid%em_loc ) ) THEN 
  DEALLOCATE(grid%em_loc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2093,&
'frame/module_domain.f: Failed to dallocate grid%em_loc. ')
 endif
  NULLIFY(grid%em_loc)
ENDIF
IF ( ASSOCIATED( grid%emt_loc ) ) THEN 
  DEALLOCATE(grid%emt_loc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2101,&
'frame/module_domain.f: Failed to dallocate grid%emt_loc. ')
 endif
  NULLIFY(grid%emt_loc)
ENDIF
IF ( ASSOCIATED( grid%pdsl ) ) THEN 
  DEALLOCATE(grid%pdsl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2109,&
'frame/module_domain.f: Failed to dallocate grid%pdsl. ')
 endif
  NULLIFY(grid%pdsl)
ENDIF
IF ( ASSOCIATED( grid%pdslo ) ) THEN 
  DEALLOCATE(grid%pdslo,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2117,&
'frame/module_domain.f: Failed to dallocate grid%pdslo. ')
 endif
  NULLIFY(grid%pdslo)
ENDIF
IF ( ASSOCIATED( grid%psdt ) ) THEN 
  DEALLOCATE(grid%psdt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2125,&
'frame/module_domain.f: Failed to dallocate grid%psdt. ')
 endif
  NULLIFY(grid%psdt)
ENDIF
IF ( ASSOCIATED( grid%div ) ) THEN 
  DEALLOCATE(grid%div,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2133,&
'frame/module_domain.f: Failed to dallocate grid%div. ')
 endif
  NULLIFY(grid%div)
ENDIF
IF ( ASSOCIATED( grid%few ) ) THEN 
  DEALLOCATE(grid%few,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2141,&
'frame/module_domain.f: Failed to dallocate grid%few. ')
 endif
  NULLIFY(grid%few)
ENDIF
IF ( ASSOCIATED( grid%fne ) ) THEN 
  DEALLOCATE(grid%fne,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2149,&
'frame/module_domain.f: Failed to dallocate grid%fne. ')
 endif
  NULLIFY(grid%fne)
ENDIF
IF ( ASSOCIATED( grid%fns ) ) THEN 
  DEALLOCATE(grid%fns,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2157,&
'frame/module_domain.f: Failed to dallocate grid%fns. ')
 endif
  NULLIFY(grid%fns)
ENDIF
IF ( ASSOCIATED( grid%fse ) ) THEN 
  DEALLOCATE(grid%fse,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2165,&
'frame/module_domain.f: Failed to dallocate grid%fse. ')
 endif
  NULLIFY(grid%fse)
ENDIF
IF ( ASSOCIATED( grid%omgalf ) ) THEN 
  DEALLOCATE(grid%omgalf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2173,&
'frame/module_domain.f: Failed to dallocate grid%omgalf. ')
 endif
  NULLIFY(grid%omgalf)
ENDIF
IF ( ASSOCIATED( grid%petdt ) ) THEN 
  DEALLOCATE(grid%petdt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2181,&
'frame/module_domain.f: Failed to dallocate grid%petdt. ')
 endif
  NULLIFY(grid%petdt)
ENDIF
IF ( ASSOCIATED( grid%rtop ) ) THEN 
  DEALLOCATE(grid%rtop,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2189,&
'frame/module_domain.f: Failed to dallocate grid%rtop. ')
 endif
  NULLIFY(grid%rtop)
ENDIF
IF ( ASSOCIATED( grid%pblh ) ) THEN 
  DEALLOCATE(grid%pblh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2197,&
'frame/module_domain.f: Failed to dallocate grid%pblh. ')
 endif
  NULLIFY(grid%pblh)
ENDIF
IF ( ASSOCIATED( grid%lpbl ) ) THEN 
  DEALLOCATE(grid%lpbl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2205,&
'frame/module_domain.f: Failed to dallocate grid%lpbl. ')
 endif
  NULLIFY(grid%lpbl)
ENDIF
IF ( ASSOCIATED( grid%mixht ) ) THEN 
  DEALLOCATE(grid%mixht,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2213,&
'frame/module_domain.f: Failed to dallocate grid%mixht. ')
 endif
  NULLIFY(grid%mixht)
ENDIF
IF ( ASSOCIATED( grid%ustar ) ) THEN 
  DEALLOCATE(grid%ustar,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2221,&
'frame/module_domain.f: Failed to dallocate grid%ustar. ')
 endif
  NULLIFY(grid%ustar)
ENDIF
IF ( ASSOCIATED( grid%z0 ) ) THEN 
  DEALLOCATE(grid%z0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2229,&
'frame/module_domain.f: Failed to dallocate grid%z0. ')
 endif
  NULLIFY(grid%z0)
ENDIF
IF ( ASSOCIATED( grid%z0base ) ) THEN 
  DEALLOCATE(grid%z0base,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2237,&
'frame/module_domain.f: Failed to dallocate grid%z0base. ')
 endif
  NULLIFY(grid%z0base)
ENDIF
IF ( ASSOCIATED( grid%ths ) ) THEN 
  DEALLOCATE(grid%ths,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2245,&
'frame/module_domain.f: Failed to dallocate grid%ths. ')
 endif
  NULLIFY(grid%ths)
ENDIF
IF ( ASSOCIATED( grid%mavail ) ) THEN 
  DEALLOCATE(grid%mavail,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2253,&
'frame/module_domain.f: Failed to dallocate grid%mavail. ')
 endif
  NULLIFY(grid%mavail)
ENDIF
IF ( ASSOCIATED( grid%qsh ) ) THEN 
  DEALLOCATE(grid%qsh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2261,&
'frame/module_domain.f: Failed to dallocate grid%qsh. ')
 endif
  NULLIFY(grid%qsh)
ENDIF
IF ( ASSOCIATED( grid%twbs ) ) THEN 
  DEALLOCATE(grid%twbs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2269,&
'frame/module_domain.f: Failed to dallocate grid%twbs. ')
 endif
  NULLIFY(grid%twbs)
ENDIF
IF ( ASSOCIATED( grid%qwbs ) ) THEN 
  DEALLOCATE(grid%qwbs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2277,&
'frame/module_domain.f: Failed to dallocate grid%qwbs. ')
 endif
  NULLIFY(grid%qwbs)
ENDIF
IF ( ASSOCIATED( grid%taux ) ) THEN 
  DEALLOCATE(grid%taux,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2285,&
'frame/module_domain.f: Failed to dallocate grid%taux. ')
 endif
  NULLIFY(grid%taux)
ENDIF
IF ( ASSOCIATED( grid%tauy ) ) THEN 
  DEALLOCATE(grid%tauy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2293,&
'frame/module_domain.f: Failed to dallocate grid%tauy. ')
 endif
  NULLIFY(grid%tauy)
ENDIF
IF ( ASSOCIATED( grid%prec ) ) THEN 
  DEALLOCATE(grid%prec,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2301,&
'frame/module_domain.f: Failed to dallocate grid%prec. ')
 endif
  NULLIFY(grid%prec)
ENDIF
IF ( ASSOCIATED( grid%aprec ) ) THEN 
  DEALLOCATE(grid%aprec,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2309,&
'frame/module_domain.f: Failed to dallocate grid%aprec. ')
 endif
  NULLIFY(grid%aprec)
ENDIF
IF ( ASSOCIATED( grid%acprec ) ) THEN 
  DEALLOCATE(grid%acprec,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2317,&
'frame/module_domain.f: Failed to dallocate grid%acprec. ')
 endif
  NULLIFY(grid%acprec)
ENDIF
IF ( ASSOCIATED( grid%cuprec ) ) THEN 
  DEALLOCATE(grid%cuprec,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2325,&
'frame/module_domain.f: Failed to dallocate grid%cuprec. ')
 endif
  NULLIFY(grid%cuprec)
ENDIF
IF ( ASSOCIATED( grid%lspa ) ) THEN 
  DEALLOCATE(grid%lspa,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2333,&
'frame/module_domain.f: Failed to dallocate grid%lspa. ')
 endif
  NULLIFY(grid%lspa)
ENDIF
IF ( ASSOCIATED( grid%ddata ) ) THEN 
  DEALLOCATE(grid%ddata,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2341,&
'frame/module_domain.f: Failed to dallocate grid%ddata. ')
 endif
  NULLIFY(grid%ddata)
ENDIF
IF ( ASSOCIATED( grid%accliq ) ) THEN 
  DEALLOCATE(grid%accliq,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2349,&
'frame/module_domain.f: Failed to dallocate grid%accliq. ')
 endif
  NULLIFY(grid%accliq)
ENDIF
IF ( ASSOCIATED( grid%sno ) ) THEN 
  DEALLOCATE(grid%sno,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2357,&
'frame/module_domain.f: Failed to dallocate grid%sno. ')
 endif
  NULLIFY(grid%sno)
ENDIF
IF ( ASSOCIATED( grid%si ) ) THEN 
  DEALLOCATE(grid%si,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2365,&
'frame/module_domain.f: Failed to dallocate grid%si. ')
 endif
  NULLIFY(grid%si)
ENDIF
IF ( ASSOCIATED( grid%cldefi ) ) THEN 
  DEALLOCATE(grid%cldefi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2373,&
'frame/module_domain.f: Failed to dallocate grid%cldefi. ')
 endif
  NULLIFY(grid%cldefi)
ENDIF
IF ( ASSOCIATED( grid%deep ) ) THEN 
  DEALLOCATE(grid%deep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2381,&
'frame/module_domain.f: Failed to dallocate grid%deep. ')
 endif
  NULLIFY(grid%deep)
ENDIF
IF ( ASSOCIATED( grid%rf ) ) THEN 
  DEALLOCATE(grid%rf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2389,&
'frame/module_domain.f: Failed to dallocate grid%rf. ')
 endif
  NULLIFY(grid%rf)
ENDIF
IF ( ASSOCIATED( grid%th10 ) ) THEN 
  DEALLOCATE(grid%th10,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2397,&
'frame/module_domain.f: Failed to dallocate grid%th10. ')
 endif
  NULLIFY(grid%th10)
ENDIF
IF ( ASSOCIATED( grid%q10 ) ) THEN 
  DEALLOCATE(grid%q10,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2405,&
'frame/module_domain.f: Failed to dallocate grid%q10. ')
 endif
  NULLIFY(grid%q10)
ENDIF
IF ( ASSOCIATED( grid%pshltr ) ) THEN 
  DEALLOCATE(grid%pshltr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2413,&
'frame/module_domain.f: Failed to dallocate grid%pshltr. ')
 endif
  NULLIFY(grid%pshltr)
ENDIF
IF ( ASSOCIATED( grid%tshltr ) ) THEN 
  DEALLOCATE(grid%tshltr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2421,&
'frame/module_domain.f: Failed to dallocate grid%tshltr. ')
 endif
  NULLIFY(grid%tshltr)
ENDIF
IF ( ASSOCIATED( grid%qshltr ) ) THEN 
  DEALLOCATE(grid%qshltr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2429,&
'frame/module_domain.f: Failed to dallocate grid%qshltr. ')
 endif
  NULLIFY(grid%qshltr)
ENDIF
IF ( ASSOCIATED( grid%q2 ) ) THEN 
  DEALLOCATE(grid%q2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2437,&
'frame/module_domain.f: Failed to dallocate grid%q2. ')
 endif
  NULLIFY(grid%q2)
ENDIF
IF ( ASSOCIATED( grid%q2_bxs ) ) THEN 
  DEALLOCATE(grid%q2_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2445,&
'frame/module_domain.f: Failed to dallocate grid%q2_bxs. ')
 endif
  NULLIFY(grid%q2_bxs)
ENDIF
IF ( ASSOCIATED( grid%q2_bxe ) ) THEN 
  DEALLOCATE(grid%q2_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2453,&
'frame/module_domain.f: Failed to dallocate grid%q2_bxe. ')
 endif
  NULLIFY(grid%q2_bxe)
ENDIF
IF ( ASSOCIATED( grid%q2_bys ) ) THEN 
  DEALLOCATE(grid%q2_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2461,&
'frame/module_domain.f: Failed to dallocate grid%q2_bys. ')
 endif
  NULLIFY(grid%q2_bys)
ENDIF
IF ( ASSOCIATED( grid%q2_bye ) ) THEN 
  DEALLOCATE(grid%q2_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2469,&
'frame/module_domain.f: Failed to dallocate grid%q2_bye. ')
 endif
  NULLIFY(grid%q2_bye)
ENDIF
IF ( ASSOCIATED( grid%q2_btxs ) ) THEN 
  DEALLOCATE(grid%q2_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2477,&
'frame/module_domain.f: Failed to dallocate grid%q2_btxs. ')
 endif
  NULLIFY(grid%q2_btxs)
ENDIF
IF ( ASSOCIATED( grid%q2_btxe ) ) THEN 
  DEALLOCATE(grid%q2_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2485,&
'frame/module_domain.f: Failed to dallocate grid%q2_btxe. ')
 endif
  NULLIFY(grid%q2_btxe)
ENDIF
IF ( ASSOCIATED( grid%q2_btys ) ) THEN 
  DEALLOCATE(grid%q2_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2493,&
'frame/module_domain.f: Failed to dallocate grid%q2_btys. ')
 endif
  NULLIFY(grid%q2_btys)
ENDIF
IF ( ASSOCIATED( grid%q2_btye ) ) THEN 
  DEALLOCATE(grid%q2_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2501,&
'frame/module_domain.f: Failed to dallocate grid%q2_btye. ')
 endif
  NULLIFY(grid%q2_btye)
ENDIF
IF ( ASSOCIATED( grid%t_adj ) ) THEN 
  DEALLOCATE(grid%t_adj,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2509,&
'frame/module_domain.f: Failed to dallocate grid%t_adj. ')
 endif
  NULLIFY(grid%t_adj)
ENDIF
IF ( ASSOCIATED( grid%t_old ) ) THEN 
  DEALLOCATE(grid%t_old,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2517,&
'frame/module_domain.f: Failed to dallocate grid%t_old. ')
 endif
  NULLIFY(grid%t_old)
ENDIF
IF ( ASSOCIATED( grid%zero_3d ) ) THEN 
  DEALLOCATE(grid%zero_3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2525,&
'frame/module_domain.f: Failed to dallocate grid%zero_3d. ')
 endif
  NULLIFY(grid%zero_3d)
ENDIF
IF ( ASSOCIATED( grid%w0avg ) ) THEN 
  DEALLOCATE(grid%w0avg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2533,&
'frame/module_domain.f: Failed to dallocate grid%w0avg. ')
 endif
  NULLIFY(grid%w0avg)
ENDIF
IF ( ASSOCIATED( grid%akhs_out ) ) THEN 
  DEALLOCATE(grid%akhs_out,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2541,&
'frame/module_domain.f: Failed to dallocate grid%akhs_out. ')
 endif
  NULLIFY(grid%akhs_out)
ENDIF
IF ( ASSOCIATED( grid%akms_out ) ) THEN 
  DEALLOCATE(grid%akms_out,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2549,&
'frame/module_domain.f: Failed to dallocate grid%akms_out. ')
 endif
  NULLIFY(grid%akms_out)
ENDIF
IF ( ASSOCIATED( grid%albase ) ) THEN 
  DEALLOCATE(grid%albase,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2557,&
'frame/module_domain.f: Failed to dallocate grid%albase. ')
 endif
  NULLIFY(grid%albase)
ENDIF
IF ( ASSOCIATED( grid%albedo ) ) THEN 
  DEALLOCATE(grid%albedo,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2565,&
'frame/module_domain.f: Failed to dallocate grid%albedo. ')
 endif
  NULLIFY(grid%albedo)
ENDIF
IF ( ASSOCIATED( grid%cnvbot ) ) THEN 
  DEALLOCATE(grid%cnvbot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2573,&
'frame/module_domain.f: Failed to dallocate grid%cnvbot. ')
 endif
  NULLIFY(grid%cnvbot)
ENDIF
IF ( ASSOCIATED( grid%cnvtop ) ) THEN 
  DEALLOCATE(grid%cnvtop,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2581,&
'frame/module_domain.f: Failed to dallocate grid%cnvtop. ')
 endif
  NULLIFY(grid%cnvtop)
ENDIF
IF ( ASSOCIATED( grid%czen ) ) THEN 
  DEALLOCATE(grid%czen,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2589,&
'frame/module_domain.f: Failed to dallocate grid%czen. ')
 endif
  NULLIFY(grid%czen)
ENDIF
IF ( ASSOCIATED( grid%czmean ) ) THEN 
  DEALLOCATE(grid%czmean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2597,&
'frame/module_domain.f: Failed to dallocate grid%czmean. ')
 endif
  NULLIFY(grid%czmean)
ENDIF
IF ( ASSOCIATED( grid%embck ) ) THEN 
  DEALLOCATE(grid%embck,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2605,&
'frame/module_domain.f: Failed to dallocate grid%embck. ')
 endif
  NULLIFY(grid%embck)
ENDIF
IF ( ASSOCIATED( grid%epsr ) ) THEN 
  DEALLOCATE(grid%epsr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2613,&
'frame/module_domain.f: Failed to dallocate grid%epsr. ')
 endif
  NULLIFY(grid%epsr)
ENDIF
IF ( ASSOCIATED( grid%gffc ) ) THEN 
  DEALLOCATE(grid%gffc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2621,&
'frame/module_domain.f: Failed to dallocate grid%gffc. ')
 endif
  NULLIFY(grid%gffc)
ENDIF
IF ( ASSOCIATED( grid%glat ) ) THEN 
  DEALLOCATE(grid%glat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2629,&
'frame/module_domain.f: Failed to dallocate grid%glat. ')
 endif
  NULLIFY(grid%glat)
ENDIF
IF ( ASSOCIATED( grid%glon ) ) THEN 
  DEALLOCATE(grid%glon,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2637,&
'frame/module_domain.f: Failed to dallocate grid%glon. ')
 endif
  NULLIFY(grid%glon)
ENDIF
IF ( ASSOCIATED( grid%nmm_tsk ) ) THEN 
  DEALLOCATE(grid%nmm_tsk,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2645,&
'frame/module_domain.f: Failed to dallocate grid%nmm_tsk. ')
 endif
  NULLIFY(grid%nmm_tsk)
ENDIF
IF ( ASSOCIATED( grid%hdac ) ) THEN 
  DEALLOCATE(grid%hdac,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2653,&
'frame/module_domain.f: Failed to dallocate grid%hdac. ')
 endif
  NULLIFY(grid%hdac)
ENDIF
IF ( ASSOCIATED( grid%hdacv ) ) THEN 
  DEALLOCATE(grid%hdacv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2661,&
'frame/module_domain.f: Failed to dallocate grid%hdacv. ')
 endif
  NULLIFY(grid%hdacv)
ENDIF
IF ( ASSOCIATED( grid%mxsnal ) ) THEN 
  DEALLOCATE(grid%mxsnal,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2669,&
'frame/module_domain.f: Failed to dallocate grid%mxsnal. ')
 endif
  NULLIFY(grid%mxsnal)
ENDIF
IF ( ASSOCIATED( grid%radin ) ) THEN 
  DEALLOCATE(grid%radin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2677,&
'frame/module_domain.f: Failed to dallocate grid%radin. ')
 endif
  NULLIFY(grid%radin)
ENDIF
IF ( ASSOCIATED( grid%radot ) ) THEN 
  DEALLOCATE(grid%radot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2685,&
'frame/module_domain.f: Failed to dallocate grid%radot. ')
 endif
  NULLIFY(grid%radot)
ENDIF
IF ( ASSOCIATED( grid%sigt4 ) ) THEN 
  DEALLOCATE(grid%sigt4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2693,&
'frame/module_domain.f: Failed to dallocate grid%sigt4. ')
 endif
  NULLIFY(grid%sigt4)
ENDIF
IF ( ASSOCIATED( grid%tg ) ) THEN 
  DEALLOCATE(grid%tg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2701,&
'frame/module_domain.f: Failed to dallocate grid%tg. ')
 endif
  NULLIFY(grid%tg)
ENDIF
IF ( ASSOCIATED( grid%dfrlg ) ) THEN 
  DEALLOCATE(grid%dfrlg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2709,&
'frame/module_domain.f: Failed to dallocate grid%dfrlg. ')
 endif
  NULLIFY(grid%dfrlg)
ENDIF
IF ( ASSOCIATED( grid%lvl ) ) THEN 
  DEALLOCATE(grid%lvl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2717,&
'frame/module_domain.f: Failed to dallocate grid%lvl. ')
 endif
  NULLIFY(grid%lvl)
ENDIF
IF ( ASSOCIATED( grid%cwm ) ) THEN 
  DEALLOCATE(grid%cwm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2725,&
'frame/module_domain.f: Failed to dallocate grid%cwm. ')
 endif
  NULLIFY(grid%cwm)
ENDIF
IF ( ASSOCIATED( grid%cwm_bxs ) ) THEN 
  DEALLOCATE(grid%cwm_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2733,&
'frame/module_domain.f: Failed to dallocate grid%cwm_bxs. ')
 endif
  NULLIFY(grid%cwm_bxs)
ENDIF
IF ( ASSOCIATED( grid%cwm_bxe ) ) THEN 
  DEALLOCATE(grid%cwm_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2741,&
'frame/module_domain.f: Failed to dallocate grid%cwm_bxe. ')
 endif
  NULLIFY(grid%cwm_bxe)
ENDIF
IF ( ASSOCIATED( grid%cwm_bys ) ) THEN 
  DEALLOCATE(grid%cwm_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2749,&
'frame/module_domain.f: Failed to dallocate grid%cwm_bys. ')
 endif
  NULLIFY(grid%cwm_bys)
ENDIF
IF ( ASSOCIATED( grid%cwm_bye ) ) THEN 
  DEALLOCATE(grid%cwm_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2757,&
'frame/module_domain.f: Failed to dallocate grid%cwm_bye. ')
 endif
  NULLIFY(grid%cwm_bye)
ENDIF
IF ( ASSOCIATED( grid%cwm_btxs ) ) THEN 
  DEALLOCATE(grid%cwm_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2765,&
'frame/module_domain.f: Failed to dallocate grid%cwm_btxs. ')
 endif
  NULLIFY(grid%cwm_btxs)
ENDIF
IF ( ASSOCIATED( grid%cwm_btxe ) ) THEN 
  DEALLOCATE(grid%cwm_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2773,&
'frame/module_domain.f: Failed to dallocate grid%cwm_btxe. ')
 endif
  NULLIFY(grid%cwm_btxe)
ENDIF
IF ( ASSOCIATED( grid%cwm_btys ) ) THEN 
  DEALLOCATE(grid%cwm_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2781,&
'frame/module_domain.f: Failed to dallocate grid%cwm_btys. ')
 endif
  NULLIFY(grid%cwm_btys)
ENDIF
IF ( ASSOCIATED( grid%cwm_btye ) ) THEN 
  DEALLOCATE(grid%cwm_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2789,&
'frame/module_domain.f: Failed to dallocate grid%cwm_btye. ')
 endif
  NULLIFY(grid%cwm_btye)
ENDIF
IF ( ASSOCIATED( grid%rrw ) ) THEN 
  DEALLOCATE(grid%rrw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2797,&
'frame/module_domain.f: Failed to dallocate grid%rrw. ')
 endif
  NULLIFY(grid%rrw)
ENDIF
IF ( ASSOCIATED( grid%rrw_bxs ) ) THEN 
  DEALLOCATE(grid%rrw_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2805,&
'frame/module_domain.f: Failed to dallocate grid%rrw_bxs. ')
 endif
  NULLIFY(grid%rrw_bxs)
ENDIF
IF ( ASSOCIATED( grid%rrw_bxe ) ) THEN 
  DEALLOCATE(grid%rrw_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2813,&
'frame/module_domain.f: Failed to dallocate grid%rrw_bxe. ')
 endif
  NULLIFY(grid%rrw_bxe)
ENDIF
IF ( ASSOCIATED( grid%rrw_bys ) ) THEN 
  DEALLOCATE(grid%rrw_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2821,&
'frame/module_domain.f: Failed to dallocate grid%rrw_bys. ')
 endif
  NULLIFY(grid%rrw_bys)
ENDIF
IF ( ASSOCIATED( grid%rrw_bye ) ) THEN 
  DEALLOCATE(grid%rrw_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2829,&
'frame/module_domain.f: Failed to dallocate grid%rrw_bye. ')
 endif
  NULLIFY(grid%rrw_bye)
ENDIF
IF ( ASSOCIATED( grid%rrw_btxs ) ) THEN 
  DEALLOCATE(grid%rrw_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2837,&
'frame/module_domain.f: Failed to dallocate grid%rrw_btxs. ')
 endif
  NULLIFY(grid%rrw_btxs)
ENDIF
IF ( ASSOCIATED( grid%rrw_btxe ) ) THEN 
  DEALLOCATE(grid%rrw_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2845,&
'frame/module_domain.f: Failed to dallocate grid%rrw_btxe. ')
 endif
  NULLIFY(grid%rrw_btxe)
ENDIF
IF ( ASSOCIATED( grid%rrw_btys ) ) THEN 
  DEALLOCATE(grid%rrw_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2853,&
'frame/module_domain.f: Failed to dallocate grid%rrw_btys. ')
 endif
  NULLIFY(grid%rrw_btys)
ENDIF
IF ( ASSOCIATED( grid%rrw_btye ) ) THEN 
  DEALLOCATE(grid%rrw_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2861,&
'frame/module_domain.f: Failed to dallocate grid%rrw_btye. ')
 endif
  NULLIFY(grid%rrw_btye)
ENDIF
IF ( ASSOCIATED( grid%f_ice ) ) THEN 
  DEALLOCATE(grid%f_ice,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2869,&
'frame/module_domain.f: Failed to dallocate grid%f_ice. ')
 endif
  NULLIFY(grid%f_ice)
ENDIF
IF ( ASSOCIATED( grid%f_rain ) ) THEN 
  DEALLOCATE(grid%f_rain,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2877,&
'frame/module_domain.f: Failed to dallocate grid%f_rain. ')
 endif
  NULLIFY(grid%f_rain)
ENDIF
IF ( ASSOCIATED( grid%f_rimef ) ) THEN 
  DEALLOCATE(grid%f_rimef,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2885,&
'frame/module_domain.f: Failed to dallocate grid%f_rimef. ')
 endif
  NULLIFY(grid%f_rimef)
ENDIF
IF ( ASSOCIATED( grid%cldfra ) ) THEN 
  DEALLOCATE(grid%cldfra,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2893,&
'frame/module_domain.f: Failed to dallocate grid%cldfra. ')
 endif
  NULLIFY(grid%cldfra)
ENDIF
IF ( ASSOCIATED( grid%sr ) ) THEN 
  DEALLOCATE(grid%sr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2901,&
'frame/module_domain.f: Failed to dallocate grid%sr. ')
 endif
  NULLIFY(grid%sr)
ENDIF
IF ( ASSOCIATED( grid%cfrach ) ) THEN 
  DEALLOCATE(grid%cfrach,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2909,&
'frame/module_domain.f: Failed to dallocate grid%cfrach. ')
 endif
  NULLIFY(grid%cfrach)
ENDIF
IF ( ASSOCIATED( grid%cfracl ) ) THEN 
  DEALLOCATE(grid%cfracl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2917,&
'frame/module_domain.f: Failed to dallocate grid%cfracl. ')
 endif
  NULLIFY(grid%cfracl)
ENDIF
IF ( ASSOCIATED( grid%cfracm ) ) THEN 
  DEALLOCATE(grid%cfracm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2925,&
'frame/module_domain.f: Failed to dallocate grid%cfracm. ')
 endif
  NULLIFY(grid%cfracm)
ENDIF
IF ( ASSOCIATED( grid%islope ) ) THEN 
  DEALLOCATE(grid%islope,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2933,&
'frame/module_domain.f: Failed to dallocate grid%islope. ')
 endif
  NULLIFY(grid%islope)
ENDIF
IF ( ASSOCIATED( grid%dzsoil ) ) THEN 
  DEALLOCATE(grid%dzsoil,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2941,&
'frame/module_domain.f: Failed to dallocate grid%dzsoil. ')
 endif
  NULLIFY(grid%dzsoil)
ENDIF
IF ( ASSOCIATED( grid%rtdpth ) ) THEN 
  DEALLOCATE(grid%rtdpth,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2949,&
'frame/module_domain.f: Failed to dallocate grid%rtdpth. ')
 endif
  NULLIFY(grid%rtdpth)
ENDIF
IF ( ASSOCIATED( grid%sldpth ) ) THEN 
  DEALLOCATE(grid%sldpth,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2957,&
'frame/module_domain.f: Failed to dallocate grid%sldpth. ')
 endif
  NULLIFY(grid%sldpth)
ENDIF
IF ( ASSOCIATED( grid%cmc ) ) THEN 
  DEALLOCATE(grid%cmc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2965,&
'frame/module_domain.f: Failed to dallocate grid%cmc. ')
 endif
  NULLIFY(grid%cmc)
ENDIF
IF ( ASSOCIATED( grid%grnflx ) ) THEN 
  DEALLOCATE(grid%grnflx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2973,&
'frame/module_domain.f: Failed to dallocate grid%grnflx. ')
 endif
  NULLIFY(grid%grnflx)
ENDIF
IF ( ASSOCIATED( grid%pctsno ) ) THEN 
  DEALLOCATE(grid%pctsno,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2981,&
'frame/module_domain.f: Failed to dallocate grid%pctsno. ')
 endif
  NULLIFY(grid%pctsno)
ENDIF
IF ( ASSOCIATED( grid%soiltb ) ) THEN 
  DEALLOCATE(grid%soiltb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2989,&
'frame/module_domain.f: Failed to dallocate grid%soiltb. ')
 endif
  NULLIFY(grid%soiltb)
ENDIF
IF ( ASSOCIATED( grid%vegfrc ) ) THEN 
  DEALLOCATE(grid%vegfrc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",2997,&
'frame/module_domain.f: Failed to dallocate grid%vegfrc. ')
 endif
  NULLIFY(grid%vegfrc)
ENDIF
IF ( ASSOCIATED( grid%shdmin ) ) THEN 
  DEALLOCATE(grid%shdmin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3005,&
'frame/module_domain.f: Failed to dallocate grid%shdmin. ')
 endif
  NULLIFY(grid%shdmin)
ENDIF
IF ( ASSOCIATED( grid%shdmax ) ) THEN 
  DEALLOCATE(grid%shdmax,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3013,&
'frame/module_domain.f: Failed to dallocate grid%shdmax. ')
 endif
  NULLIFY(grid%shdmax)
ENDIF
IF ( ASSOCIATED( grid%sh2o ) ) THEN 
  DEALLOCATE(grid%sh2o,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3021,&
'frame/module_domain.f: Failed to dallocate grid%sh2o. ')
 endif
  NULLIFY(grid%sh2o)
ENDIF
IF ( ASSOCIATED( grid%smc ) ) THEN 
  DEALLOCATE(grid%smc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3029,&
'frame/module_domain.f: Failed to dallocate grid%smc. ')
 endif
  NULLIFY(grid%smc)
ENDIF
IF ( ASSOCIATED( grid%stc ) ) THEN 
  DEALLOCATE(grid%stc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3037,&
'frame/module_domain.f: Failed to dallocate grid%stc. ')
 endif
  NULLIFY(grid%stc)
ENDIF
IF ( ASSOCIATED( grid%hstdv ) ) THEN 
  DEALLOCATE(grid%hstdv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3045,&
'frame/module_domain.f: Failed to dallocate grid%hstdv. ')
 endif
  NULLIFY(grid%hstdv)
ENDIF
IF ( ASSOCIATED( grid%hcnvx ) ) THEN 
  DEALLOCATE(grid%hcnvx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3053,&
'frame/module_domain.f: Failed to dallocate grid%hcnvx. ')
 endif
  NULLIFY(grid%hcnvx)
ENDIF
IF ( ASSOCIATED( grid%hasyw ) ) THEN 
  DEALLOCATE(grid%hasyw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3061,&
'frame/module_domain.f: Failed to dallocate grid%hasyw. ')
 endif
  NULLIFY(grid%hasyw)
ENDIF
IF ( ASSOCIATED( grid%hasys ) ) THEN 
  DEALLOCATE(grid%hasys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3069,&
'frame/module_domain.f: Failed to dallocate grid%hasys. ')
 endif
  NULLIFY(grid%hasys)
ENDIF
IF ( ASSOCIATED( grid%hasysw ) ) THEN 
  DEALLOCATE(grid%hasysw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3077,&
'frame/module_domain.f: Failed to dallocate grid%hasysw. ')
 endif
  NULLIFY(grid%hasysw)
ENDIF
IF ( ASSOCIATED( grid%hasynw ) ) THEN 
  DEALLOCATE(grid%hasynw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3085,&
'frame/module_domain.f: Failed to dallocate grid%hasynw. ')
 endif
  NULLIFY(grid%hasynw)
ENDIF
IF ( ASSOCIATED( grid%hlenw ) ) THEN 
  DEALLOCATE(grid%hlenw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3093,&
'frame/module_domain.f: Failed to dallocate grid%hlenw. ')
 endif
  NULLIFY(grid%hlenw)
ENDIF
IF ( ASSOCIATED( grid%hlens ) ) THEN 
  DEALLOCATE(grid%hlens,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3101,&
'frame/module_domain.f: Failed to dallocate grid%hlens. ')
 endif
  NULLIFY(grid%hlens)
ENDIF
IF ( ASSOCIATED( grid%hlensw ) ) THEN 
  DEALLOCATE(grid%hlensw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3109,&
'frame/module_domain.f: Failed to dallocate grid%hlensw. ')
 endif
  NULLIFY(grid%hlensw)
ENDIF
IF ( ASSOCIATED( grid%hlennw ) ) THEN 
  DEALLOCATE(grid%hlennw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3117,&
'frame/module_domain.f: Failed to dallocate grid%hlennw. ')
 endif
  NULLIFY(grid%hlennw)
ENDIF
IF ( ASSOCIATED( grid%hangl ) ) THEN 
  DEALLOCATE(grid%hangl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3125,&
'frame/module_domain.f: Failed to dallocate grid%hangl. ')
 endif
  NULLIFY(grid%hangl)
ENDIF
IF ( ASSOCIATED( grid%hanis ) ) THEN 
  DEALLOCATE(grid%hanis,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3133,&
'frame/module_domain.f: Failed to dallocate grid%hanis. ')
 endif
  NULLIFY(grid%hanis)
ENDIF
IF ( ASSOCIATED( grid%hslop ) ) THEN 
  DEALLOCATE(grid%hslop,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3141,&
'frame/module_domain.f: Failed to dallocate grid%hslop. ')
 endif
  NULLIFY(grid%hslop)
ENDIF
IF ( ASSOCIATED( grid%hzmax ) ) THEN 
  DEALLOCATE(grid%hzmax,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3149,&
'frame/module_domain.f: Failed to dallocate grid%hzmax. ')
 endif
  NULLIFY(grid%hzmax)
ENDIF
IF ( ASSOCIATED( grid%crot ) ) THEN 
  DEALLOCATE(grid%crot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3157,&
'frame/module_domain.f: Failed to dallocate grid%crot. ')
 endif
  NULLIFY(grid%crot)
ENDIF
IF ( ASSOCIATED( grid%srot ) ) THEN 
  DEALLOCATE(grid%srot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3165,&
'frame/module_domain.f: Failed to dallocate grid%srot. ')
 endif
  NULLIFY(grid%srot)
ENDIF
IF ( ASSOCIATED( grid%ugwdsfc ) ) THEN 
  DEALLOCATE(grid%ugwdsfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3173,&
'frame/module_domain.f: Failed to dallocate grid%ugwdsfc. ')
 endif
  NULLIFY(grid%ugwdsfc)
ENDIF
IF ( ASSOCIATED( grid%vgwdsfc ) ) THEN 
  DEALLOCATE(grid%vgwdsfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3181,&
'frame/module_domain.f: Failed to dallocate grid%vgwdsfc. ')
 endif
  NULLIFY(grid%vgwdsfc)
ENDIF
IF ( ASSOCIATED( grid%dwdtmn ) ) THEN 
  DEALLOCATE(grid%dwdtmn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3189,&
'frame/module_domain.f: Failed to dallocate grid%dwdtmn. ')
 endif
  NULLIFY(grid%dwdtmn)
ENDIF
IF ( ASSOCIATED( grid%dwdtmx ) ) THEN 
  DEALLOCATE(grid%dwdtmx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3197,&
'frame/module_domain.f: Failed to dallocate grid%dwdtmx. ')
 endif
  NULLIFY(grid%dwdtmx)
ENDIF
IF ( ASSOCIATED( grid%dwdt ) ) THEN 
  DEALLOCATE(grid%dwdt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3205,&
'frame/module_domain.f: Failed to dallocate grid%dwdt. ')
 endif
  NULLIFY(grid%dwdt)
ENDIF
IF ( ASSOCIATED( grid%pdwdt ) ) THEN 
  DEALLOCATE(grid%pdwdt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3213,&
'frame/module_domain.f: Failed to dallocate grid%pdwdt. ')
 endif
  NULLIFY(grid%pdwdt)
ENDIF
IF ( ASSOCIATED( grid%pint ) ) THEN 
  DEALLOCATE(grid%pint,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3221,&
'frame/module_domain.f: Failed to dallocate grid%pint. ')
 endif
  NULLIFY(grid%pint)
ENDIF
IF ( ASSOCIATED( grid%w ) ) THEN 
  DEALLOCATE(grid%w,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3229,&
'frame/module_domain.f: Failed to dallocate grid%w. ')
 endif
  NULLIFY(grid%w)
ENDIF
IF ( ASSOCIATED( grid%z ) ) THEN 
  DEALLOCATE(grid%z,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3237,&
'frame/module_domain.f: Failed to dallocate grid%z. ')
 endif
  NULLIFY(grid%z)
ENDIF
IF ( ASSOCIATED( grid%acfrcv ) ) THEN 
  DEALLOCATE(grid%acfrcv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3245,&
'frame/module_domain.f: Failed to dallocate grid%acfrcv. ')
 endif
  NULLIFY(grid%acfrcv)
ENDIF
IF ( ASSOCIATED( grid%acfrst ) ) THEN 
  DEALLOCATE(grid%acfrst,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3253,&
'frame/module_domain.f: Failed to dallocate grid%acfrst. ')
 endif
  NULLIFY(grid%acfrst)
ENDIF
IF ( ASSOCIATED( grid%ssroff ) ) THEN 
  DEALLOCATE(grid%ssroff,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3261,&
'frame/module_domain.f: Failed to dallocate grid%ssroff. ')
 endif
  NULLIFY(grid%ssroff)
ENDIF
IF ( ASSOCIATED( grid%bgroff ) ) THEN 
  DEALLOCATE(grid%bgroff,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3269,&
'frame/module_domain.f: Failed to dallocate grid%bgroff. ')
 endif
  NULLIFY(grid%bgroff)
ENDIF
IF ( ASSOCIATED( grid%rlwin ) ) THEN 
  DEALLOCATE(grid%rlwin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3277,&
'frame/module_domain.f: Failed to dallocate grid%rlwin. ')
 endif
  NULLIFY(grid%rlwin)
ENDIF
IF ( ASSOCIATED( grid%rlwout ) ) THEN 
  DEALLOCATE(grid%rlwout,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3285,&
'frame/module_domain.f: Failed to dallocate grid%rlwout. ')
 endif
  NULLIFY(grid%rlwout)
ENDIF
IF ( ASSOCIATED( grid%rlwtoa ) ) THEN 
  DEALLOCATE(grid%rlwtoa,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3293,&
'frame/module_domain.f: Failed to dallocate grid%rlwtoa. ')
 endif
  NULLIFY(grid%rlwtoa)
ENDIF
IF ( ASSOCIATED( grid%alwin ) ) THEN 
  DEALLOCATE(grid%alwin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3301,&
'frame/module_domain.f: Failed to dallocate grid%alwin. ')
 endif
  NULLIFY(grid%alwin)
ENDIF
IF ( ASSOCIATED( grid%alwout ) ) THEN 
  DEALLOCATE(grid%alwout,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3309,&
'frame/module_domain.f: Failed to dallocate grid%alwout. ')
 endif
  NULLIFY(grid%alwout)
ENDIF
IF ( ASSOCIATED( grid%alwtoa ) ) THEN 
  DEALLOCATE(grid%alwtoa,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3317,&
'frame/module_domain.f: Failed to dallocate grid%alwtoa. ')
 endif
  NULLIFY(grid%alwtoa)
ENDIF
IF ( ASSOCIATED( grid%rswin ) ) THEN 
  DEALLOCATE(grid%rswin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3325,&
'frame/module_domain.f: Failed to dallocate grid%rswin. ')
 endif
  NULLIFY(grid%rswin)
ENDIF
IF ( ASSOCIATED( grid%rswinc ) ) THEN 
  DEALLOCATE(grid%rswinc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3333,&
'frame/module_domain.f: Failed to dallocate grid%rswinc. ')
 endif
  NULLIFY(grid%rswinc)
ENDIF
IF ( ASSOCIATED( grid%rswout ) ) THEN 
  DEALLOCATE(grid%rswout,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3341,&
'frame/module_domain.f: Failed to dallocate grid%rswout. ')
 endif
  NULLIFY(grid%rswout)
ENDIF
IF ( ASSOCIATED( grid%rswtoa ) ) THEN 
  DEALLOCATE(grid%rswtoa,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3349,&
'frame/module_domain.f: Failed to dallocate grid%rswtoa. ')
 endif
  NULLIFY(grid%rswtoa)
ENDIF
IF ( ASSOCIATED( grid%aswin ) ) THEN 
  DEALLOCATE(grid%aswin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3357,&
'frame/module_domain.f: Failed to dallocate grid%aswin. ')
 endif
  NULLIFY(grid%aswin)
ENDIF
IF ( ASSOCIATED( grid%aswout ) ) THEN 
  DEALLOCATE(grid%aswout,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3365,&
'frame/module_domain.f: Failed to dallocate grid%aswout. ')
 endif
  NULLIFY(grid%aswout)
ENDIF
IF ( ASSOCIATED( grid%aswtoa ) ) THEN 
  DEALLOCATE(grid%aswtoa,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3373,&
'frame/module_domain.f: Failed to dallocate grid%aswtoa. ')
 endif
  NULLIFY(grid%aswtoa)
ENDIF
IF ( ASSOCIATED( grid%sfcshx ) ) THEN 
  DEALLOCATE(grid%sfcshx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3381,&
'frame/module_domain.f: Failed to dallocate grid%sfcshx. ')
 endif
  NULLIFY(grid%sfcshx)
ENDIF
IF ( ASSOCIATED( grid%sfclhx ) ) THEN 
  DEALLOCATE(grid%sfclhx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3389,&
'frame/module_domain.f: Failed to dallocate grid%sfclhx. ')
 endif
  NULLIFY(grid%sfclhx)
ENDIF
IF ( ASSOCIATED( grid%subshx ) ) THEN 
  DEALLOCATE(grid%subshx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3397,&
'frame/module_domain.f: Failed to dallocate grid%subshx. ')
 endif
  NULLIFY(grid%subshx)
ENDIF
IF ( ASSOCIATED( grid%snopcx ) ) THEN 
  DEALLOCATE(grid%snopcx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3405,&
'frame/module_domain.f: Failed to dallocate grid%snopcx. ')
 endif
  NULLIFY(grid%snopcx)
ENDIF
IF ( ASSOCIATED( grid%sfcuvx ) ) THEN 
  DEALLOCATE(grid%sfcuvx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3413,&
'frame/module_domain.f: Failed to dallocate grid%sfcuvx. ')
 endif
  NULLIFY(grid%sfcuvx)
ENDIF
IF ( ASSOCIATED( grid%potevp ) ) THEN 
  DEALLOCATE(grid%potevp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3421,&
'frame/module_domain.f: Failed to dallocate grid%potevp. ')
 endif
  NULLIFY(grid%potevp)
ENDIF
IF ( ASSOCIATED( grid%potflx ) ) THEN 
  DEALLOCATE(grid%potflx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3429,&
'frame/module_domain.f: Failed to dallocate grid%potflx. ')
 endif
  NULLIFY(grid%potflx)
ENDIF
IF ( ASSOCIATED( grid%tlmin ) ) THEN 
  DEALLOCATE(grid%tlmin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3437,&
'frame/module_domain.f: Failed to dallocate grid%tlmin. ')
 endif
  NULLIFY(grid%tlmin)
ENDIF
IF ( ASSOCIATED( grid%tlmax ) ) THEN 
  DEALLOCATE(grid%tlmax,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3445,&
'frame/module_domain.f: Failed to dallocate grid%tlmax. ')
 endif
  NULLIFY(grid%tlmax)
ENDIF
IF ( ASSOCIATED( grid%t02_min ) ) THEN 
  DEALLOCATE(grid%t02_min,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3453,&
'frame/module_domain.f: Failed to dallocate grid%t02_min. ')
 endif
  NULLIFY(grid%t02_min)
ENDIF
IF ( ASSOCIATED( grid%t02_max ) ) THEN 
  DEALLOCATE(grid%t02_max,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3461,&
'frame/module_domain.f: Failed to dallocate grid%t02_max. ')
 endif
  NULLIFY(grid%t02_max)
ENDIF
IF ( ASSOCIATED( grid%rh02_min ) ) THEN 
  DEALLOCATE(grid%rh02_min,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3469,&
'frame/module_domain.f: Failed to dallocate grid%rh02_min. ')
 endif
  NULLIFY(grid%rh02_min)
ENDIF
IF ( ASSOCIATED( grid%rh02_max ) ) THEN 
  DEALLOCATE(grid%rh02_max,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3477,&
'frame/module_domain.f: Failed to dallocate grid%rh02_max. ')
 endif
  NULLIFY(grid%rh02_max)
ENDIF
IF ( ASSOCIATED( grid%rlwtt ) ) THEN 
  DEALLOCATE(grid%rlwtt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3485,&
'frame/module_domain.f: Failed to dallocate grid%rlwtt. ')
 endif
  NULLIFY(grid%rlwtt)
ENDIF
IF ( ASSOCIATED( grid%rswtt ) ) THEN 
  DEALLOCATE(grid%rswtt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3493,&
'frame/module_domain.f: Failed to dallocate grid%rswtt. ')
 endif
  NULLIFY(grid%rswtt)
ENDIF
IF ( ASSOCIATED( grid%tcucn ) ) THEN 
  DEALLOCATE(grid%tcucn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3501,&
'frame/module_domain.f: Failed to dallocate grid%tcucn. ')
 endif
  NULLIFY(grid%tcucn)
ENDIF
IF ( ASSOCIATED( grid%train ) ) THEN 
  DEALLOCATE(grid%train,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3509,&
'frame/module_domain.f: Failed to dallocate grid%train. ')
 endif
  NULLIFY(grid%train)
ENDIF
IF ( ASSOCIATED( grid%ncfrcv ) ) THEN 
  DEALLOCATE(grid%ncfrcv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3517,&
'frame/module_domain.f: Failed to dallocate grid%ncfrcv. ')
 endif
  NULLIFY(grid%ncfrcv)
ENDIF
IF ( ASSOCIATED( grid%ncfrst ) ) THEN 
  DEALLOCATE(grid%ncfrst,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3525,&
'frame/module_domain.f: Failed to dallocate grid%ncfrst. ')
 endif
  NULLIFY(grid%ncfrst)
ENDIF
IF ( ASSOCIATED( grid%max10mw ) ) THEN 
  DEALLOCATE(grid%max10mw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3533,&
'frame/module_domain.f: Failed to dallocate grid%max10mw. ')
 endif
  NULLIFY(grid%max10mw)
ENDIF
IF ( ASSOCIATED( grid%max10u ) ) THEN 
  DEALLOCATE(grid%max10u,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3541,&
'frame/module_domain.f: Failed to dallocate grid%max10u. ')
 endif
  NULLIFY(grid%max10u)
ENDIF
IF ( ASSOCIATED( grid%max10v ) ) THEN 
  DEALLOCATE(grid%max10v,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3549,&
'frame/module_domain.f: Failed to dallocate grid%max10v. ')
 endif
  NULLIFY(grid%max10v)
ENDIF
IF ( ASSOCIATED( grid%maxupdr ) ) THEN 
  DEALLOCATE(grid%maxupdr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3557,&
'frame/module_domain.f: Failed to dallocate grid%maxupdr. ')
 endif
  NULLIFY(grid%maxupdr)
ENDIF
IF ( ASSOCIATED( grid%maxdndr ) ) THEN 
  DEALLOCATE(grid%maxdndr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3565,&
'frame/module_domain.f: Failed to dallocate grid%maxdndr. ')
 endif
  NULLIFY(grid%maxdndr)
ENDIF
IF ( ASSOCIATED( grid%maxhlcy ) ) THEN 
  DEALLOCATE(grid%maxhlcy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3573,&
'frame/module_domain.f: Failed to dallocate grid%maxhlcy. ')
 endif
  NULLIFY(grid%maxhlcy)
ENDIF
IF ( ASSOCIATED( grid%maxdbz ) ) THEN 
  DEALLOCATE(grid%maxdbz,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3581,&
'frame/module_domain.f: Failed to dallocate grid%maxdbz. ')
 endif
  NULLIFY(grid%maxdbz)
ENDIF
IF ( ASSOCIATED( grid%ihe ) ) THEN 
  DEALLOCATE(grid%ihe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3589,&
'frame/module_domain.f: Failed to dallocate grid%ihe. ')
 endif
  NULLIFY(grid%ihe)
ENDIF
IF ( ASSOCIATED( grid%ihw ) ) THEN 
  DEALLOCATE(grid%ihw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3597,&
'frame/module_domain.f: Failed to dallocate grid%ihw. ')
 endif
  NULLIFY(grid%ihw)
ENDIF
IF ( ASSOCIATED( grid%ive ) ) THEN 
  DEALLOCATE(grid%ive,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3605,&
'frame/module_domain.f: Failed to dallocate grid%ive. ')
 endif
  NULLIFY(grid%ive)
ENDIF
IF ( ASSOCIATED( grid%ivw ) ) THEN 
  DEALLOCATE(grid%ivw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3613,&
'frame/module_domain.f: Failed to dallocate grid%ivw. ')
 endif
  NULLIFY(grid%ivw)
ENDIF
IF ( ASSOCIATED( grid%irad ) ) THEN 
  DEALLOCATE(grid%irad,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3621,&
'frame/module_domain.f: Failed to dallocate grid%irad. ')
 endif
  NULLIFY(grid%irad)
ENDIF
IF ( ASSOCIATED( grid%iheg ) ) THEN 
  DEALLOCATE(grid%iheg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3629,&
'frame/module_domain.f: Failed to dallocate grid%iheg. ')
 endif
  NULLIFY(grid%iheg)
ENDIF
IF ( ASSOCIATED( grid%ihwg ) ) THEN 
  DEALLOCATE(grid%ihwg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3637,&
'frame/module_domain.f: Failed to dallocate grid%ihwg. ')
 endif
  NULLIFY(grid%ihwg)
ENDIF
IF ( ASSOCIATED( grid%iveg ) ) THEN 
  DEALLOCATE(grid%iveg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3645,&
'frame/module_domain.f: Failed to dallocate grid%iveg. ')
 endif
  NULLIFY(grid%iveg)
ENDIF
IF ( ASSOCIATED( grid%ivwg ) ) THEN 
  DEALLOCATE(grid%ivwg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3653,&
'frame/module_domain.f: Failed to dallocate grid%ivwg. ')
 endif
  NULLIFY(grid%ivwg)
ENDIF
IF ( ASSOCIATED( grid%iradg ) ) THEN 
  DEALLOCATE(grid%iradg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3661,&
'frame/module_domain.f: Failed to dallocate grid%iradg. ')
 endif
  NULLIFY(grid%iradg)
ENDIF
IF ( ASSOCIATED( grid%n_iup_h ) ) THEN 
  DEALLOCATE(grid%n_iup_h,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3669,&
'frame/module_domain.f: Failed to dallocate grid%n_iup_h. ')
 endif
  NULLIFY(grid%n_iup_h)
ENDIF
IF ( ASSOCIATED( grid%n_iup_v ) ) THEN 
  DEALLOCATE(grid%n_iup_v,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3677,&
'frame/module_domain.f: Failed to dallocate grid%n_iup_v. ')
 endif
  NULLIFY(grid%n_iup_v)
ENDIF
IF ( ASSOCIATED( grid%n_iup_adh ) ) THEN 
  DEALLOCATE(grid%n_iup_adh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3685,&
'frame/module_domain.f: Failed to dallocate grid%n_iup_adh. ')
 endif
  NULLIFY(grid%n_iup_adh)
ENDIF
IF ( ASSOCIATED( grid%n_iup_adv ) ) THEN 
  DEALLOCATE(grid%n_iup_adv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3693,&
'frame/module_domain.f: Failed to dallocate grid%n_iup_adv. ')
 endif
  NULLIFY(grid%n_iup_adv)
ENDIF
IF ( ASSOCIATED( grid%iup_h ) ) THEN 
  DEALLOCATE(grid%iup_h,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3701,&
'frame/module_domain.f: Failed to dallocate grid%iup_h. ')
 endif
  NULLIFY(grid%iup_h)
ENDIF
IF ( ASSOCIATED( grid%iup_v ) ) THEN 
  DEALLOCATE(grid%iup_v,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3709,&
'frame/module_domain.f: Failed to dallocate grid%iup_v. ')
 endif
  NULLIFY(grid%iup_v)
ENDIF
IF ( ASSOCIATED( grid%iup_adh ) ) THEN 
  DEALLOCATE(grid%iup_adh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3717,&
'frame/module_domain.f: Failed to dallocate grid%iup_adh. ')
 endif
  NULLIFY(grid%iup_adh)
ENDIF
IF ( ASSOCIATED( grid%iup_adv ) ) THEN 
  DEALLOCATE(grid%iup_adv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3725,&
'frame/module_domain.f: Failed to dallocate grid%iup_adv. ')
 endif
  NULLIFY(grid%iup_adv)
ENDIF
IF ( ASSOCIATED( grid%imask_nostag ) ) THEN 
  DEALLOCATE(grid%imask_nostag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3733,&
'frame/module_domain.f: Failed to dallocate grid%imask_nostag. ')
 endif
  NULLIFY(grid%imask_nostag)
ENDIF
IF ( ASSOCIATED( grid%imask_xstag ) ) THEN 
  DEALLOCATE(grid%imask_xstag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3741,&
'frame/module_domain.f: Failed to dallocate grid%imask_xstag. ')
 endif
  NULLIFY(grid%imask_xstag)
ENDIF
IF ( ASSOCIATED( grid%imask_ystag ) ) THEN 
  DEALLOCATE(grid%imask_ystag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3749,&
'frame/module_domain.f: Failed to dallocate grid%imask_ystag. ')
 endif
  NULLIFY(grid%imask_ystag)
ENDIF
IF ( ASSOCIATED( grid%imask_xystag ) ) THEN 
  DEALLOCATE(grid%imask_xystag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3757,&
'frame/module_domain.f: Failed to dallocate grid%imask_xystag. ')
 endif
  NULLIFY(grid%imask_xystag)
ENDIF
IF ( ASSOCIATED( grid%sm000007 ) ) THEN 
  DEALLOCATE(grid%sm000007,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3765,&
'frame/module_domain.f: Failed to dallocate grid%sm000007. ')
 endif
  NULLIFY(grid%sm000007)
ENDIF
IF ( ASSOCIATED( grid%sm007028 ) ) THEN 
  DEALLOCATE(grid%sm007028,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3773,&
'frame/module_domain.f: Failed to dallocate grid%sm007028. ')
 endif
  NULLIFY(grid%sm007028)
ENDIF
IF ( ASSOCIATED( grid%sm028100 ) ) THEN 
  DEALLOCATE(grid%sm028100,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3781,&
'frame/module_domain.f: Failed to dallocate grid%sm028100. ')
 endif
  NULLIFY(grid%sm028100)
ENDIF
IF ( ASSOCIATED( grid%sm100255 ) ) THEN 
  DEALLOCATE(grid%sm100255,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3789,&
'frame/module_domain.f: Failed to dallocate grid%sm100255. ')
 endif
  NULLIFY(grid%sm100255)
ENDIF
IF ( ASSOCIATED( grid%st000007 ) ) THEN 
  DEALLOCATE(grid%st000007,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3797,&
'frame/module_domain.f: Failed to dallocate grid%st000007. ')
 endif
  NULLIFY(grid%st000007)
ENDIF
IF ( ASSOCIATED( grid%st007028 ) ) THEN 
  DEALLOCATE(grid%st007028,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3805,&
'frame/module_domain.f: Failed to dallocate grid%st007028. ')
 endif
  NULLIFY(grid%st007028)
ENDIF
IF ( ASSOCIATED( grid%st028100 ) ) THEN 
  DEALLOCATE(grid%st028100,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3813,&
'frame/module_domain.f: Failed to dallocate grid%st028100. ')
 endif
  NULLIFY(grid%st028100)
ENDIF
IF ( ASSOCIATED( grid%st100255 ) ) THEN 
  DEALLOCATE(grid%st100255,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3821,&
'frame/module_domain.f: Failed to dallocate grid%st100255. ')
 endif
  NULLIFY(grid%st100255)
ENDIF
IF ( ASSOCIATED( grid%sm000010 ) ) THEN 
  DEALLOCATE(grid%sm000010,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3829,&
'frame/module_domain.f: Failed to dallocate grid%sm000010. ')
 endif
  NULLIFY(grid%sm000010)
ENDIF
IF ( ASSOCIATED( grid%sm010040 ) ) THEN 
  DEALLOCATE(grid%sm010040,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3837,&
'frame/module_domain.f: Failed to dallocate grid%sm010040. ')
 endif
  NULLIFY(grid%sm010040)
ENDIF
IF ( ASSOCIATED( grid%sm040100 ) ) THEN 
  DEALLOCATE(grid%sm040100,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3845,&
'frame/module_domain.f: Failed to dallocate grid%sm040100. ')
 endif
  NULLIFY(grid%sm040100)
ENDIF
IF ( ASSOCIATED( grid%sm100200 ) ) THEN 
  DEALLOCATE(grid%sm100200,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3853,&
'frame/module_domain.f: Failed to dallocate grid%sm100200. ')
 endif
  NULLIFY(grid%sm100200)
ENDIF
IF ( ASSOCIATED( grid%sm010200 ) ) THEN 
  DEALLOCATE(grid%sm010200,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3861,&
'frame/module_domain.f: Failed to dallocate grid%sm010200. ')
 endif
  NULLIFY(grid%sm010200)
ENDIF
IF ( ASSOCIATED( grid%soilm000 ) ) THEN 
  DEALLOCATE(grid%soilm000,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3869,&
'frame/module_domain.f: Failed to dallocate grid%soilm000. ')
 endif
  NULLIFY(grid%soilm000)
ENDIF
IF ( ASSOCIATED( grid%soilm005 ) ) THEN 
  DEALLOCATE(grid%soilm005,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3877,&
'frame/module_domain.f: Failed to dallocate grid%soilm005. ')
 endif
  NULLIFY(grid%soilm005)
ENDIF
IF ( ASSOCIATED( grid%soilm020 ) ) THEN 
  DEALLOCATE(grid%soilm020,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3885,&
'frame/module_domain.f: Failed to dallocate grid%soilm020. ')
 endif
  NULLIFY(grid%soilm020)
ENDIF
IF ( ASSOCIATED( grid%soilm040 ) ) THEN 
  DEALLOCATE(grid%soilm040,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3893,&
'frame/module_domain.f: Failed to dallocate grid%soilm040. ')
 endif
  NULLIFY(grid%soilm040)
ENDIF
IF ( ASSOCIATED( grid%soilm160 ) ) THEN 
  DEALLOCATE(grid%soilm160,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3901,&
'frame/module_domain.f: Failed to dallocate grid%soilm160. ')
 endif
  NULLIFY(grid%soilm160)
ENDIF
IF ( ASSOCIATED( grid%soilm300 ) ) THEN 
  DEALLOCATE(grid%soilm300,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3909,&
'frame/module_domain.f: Failed to dallocate grid%soilm300. ')
 endif
  NULLIFY(grid%soilm300)
ENDIF
IF ( ASSOCIATED( grid%sw000010 ) ) THEN 
  DEALLOCATE(grid%sw000010,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3917,&
'frame/module_domain.f: Failed to dallocate grid%sw000010. ')
 endif
  NULLIFY(grid%sw000010)
ENDIF
IF ( ASSOCIATED( grid%sw010040 ) ) THEN 
  DEALLOCATE(grid%sw010040,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3925,&
'frame/module_domain.f: Failed to dallocate grid%sw010040. ')
 endif
  NULLIFY(grid%sw010040)
ENDIF
IF ( ASSOCIATED( grid%sw040100 ) ) THEN 
  DEALLOCATE(grid%sw040100,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3933,&
'frame/module_domain.f: Failed to dallocate grid%sw040100. ')
 endif
  NULLIFY(grid%sw040100)
ENDIF
IF ( ASSOCIATED( grid%sw100200 ) ) THEN 
  DEALLOCATE(grid%sw100200,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3941,&
'frame/module_domain.f: Failed to dallocate grid%sw100200. ')
 endif
  NULLIFY(grid%sw100200)
ENDIF
IF ( ASSOCIATED( grid%sw010200 ) ) THEN 
  DEALLOCATE(grid%sw010200,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3949,&
'frame/module_domain.f: Failed to dallocate grid%sw010200. ')
 endif
  NULLIFY(grid%sw010200)
ENDIF
IF ( ASSOCIATED( grid%soilw000 ) ) THEN 
  DEALLOCATE(grid%soilw000,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3957,&
'frame/module_domain.f: Failed to dallocate grid%soilw000. ')
 endif
  NULLIFY(grid%soilw000)
ENDIF
IF ( ASSOCIATED( grid%soilw005 ) ) THEN 
  DEALLOCATE(grid%soilw005,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3965,&
'frame/module_domain.f: Failed to dallocate grid%soilw005. ')
 endif
  NULLIFY(grid%soilw005)
ENDIF
IF ( ASSOCIATED( grid%soilw020 ) ) THEN 
  DEALLOCATE(grid%soilw020,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3973,&
'frame/module_domain.f: Failed to dallocate grid%soilw020. ')
 endif
  NULLIFY(grid%soilw020)
ENDIF
IF ( ASSOCIATED( grid%soilw040 ) ) THEN 
  DEALLOCATE(grid%soilw040,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3981,&
'frame/module_domain.f: Failed to dallocate grid%soilw040. ')
 endif
  NULLIFY(grid%soilw040)
ENDIF
IF ( ASSOCIATED( grid%soilw160 ) ) THEN 
  DEALLOCATE(grid%soilw160,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3989,&
'frame/module_domain.f: Failed to dallocate grid%soilw160. ')
 endif
  NULLIFY(grid%soilw160)
ENDIF
IF ( ASSOCIATED( grid%soilw300 ) ) THEN 
  DEALLOCATE(grid%soilw300,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",3997,&
'frame/module_domain.f: Failed to dallocate grid%soilw300. ')
 endif
  NULLIFY(grid%soilw300)
ENDIF
IF ( ASSOCIATED( grid%st000010 ) ) THEN 
  DEALLOCATE(grid%st000010,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4005,&
'frame/module_domain.f: Failed to dallocate grid%st000010. ')
 endif
  NULLIFY(grid%st000010)
ENDIF
IF ( ASSOCIATED( grid%st010040 ) ) THEN 
  DEALLOCATE(grid%st010040,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4013,&
'frame/module_domain.f: Failed to dallocate grid%st010040. ')
 endif
  NULLIFY(grid%st010040)
ENDIF
IF ( ASSOCIATED( grid%st040100 ) ) THEN 
  DEALLOCATE(grid%st040100,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4021,&
'frame/module_domain.f: Failed to dallocate grid%st040100. ')
 endif
  NULLIFY(grid%st040100)
ENDIF
IF ( ASSOCIATED( grid%st100200 ) ) THEN 
  DEALLOCATE(grid%st100200,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4029,&
'frame/module_domain.f: Failed to dallocate grid%st100200. ')
 endif
  NULLIFY(grid%st100200)
ENDIF
IF ( ASSOCIATED( grid%st010200 ) ) THEN 
  DEALLOCATE(grid%st010200,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4037,&
'frame/module_domain.f: Failed to dallocate grid%st010200. ')
 endif
  NULLIFY(grid%st010200)
ENDIF
IF ( ASSOCIATED( grid%soilt000 ) ) THEN 
  DEALLOCATE(grid%soilt000,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4045,&
'frame/module_domain.f: Failed to dallocate grid%soilt000. ')
 endif
  NULLIFY(grid%soilt000)
ENDIF
IF ( ASSOCIATED( grid%soilt005 ) ) THEN 
  DEALLOCATE(grid%soilt005,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4053,&
'frame/module_domain.f: Failed to dallocate grid%soilt005. ')
 endif
  NULLIFY(grid%soilt005)
ENDIF
IF ( ASSOCIATED( grid%soilt020 ) ) THEN 
  DEALLOCATE(grid%soilt020,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4061,&
'frame/module_domain.f: Failed to dallocate grid%soilt020. ')
 endif
  NULLIFY(grid%soilt020)
ENDIF
IF ( ASSOCIATED( grid%soilt040 ) ) THEN 
  DEALLOCATE(grid%soilt040,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4069,&
'frame/module_domain.f: Failed to dallocate grid%soilt040. ')
 endif
  NULLIFY(grid%soilt040)
ENDIF
IF ( ASSOCIATED( grid%soilt160 ) ) THEN 
  DEALLOCATE(grid%soilt160,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4077,&
'frame/module_domain.f: Failed to dallocate grid%soilt160. ')
 endif
  NULLIFY(grid%soilt160)
ENDIF
IF ( ASSOCIATED( grid%soilt300 ) ) THEN 
  DEALLOCATE(grid%soilt300,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4085,&
'frame/module_domain.f: Failed to dallocate grid%soilt300. ')
 endif
  NULLIFY(grid%soilt300)
ENDIF
IF ( ASSOCIATED( grid%landmask ) ) THEN 
  DEALLOCATE(grid%landmask,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4093,&
'frame/module_domain.f: Failed to dallocate grid%landmask. ')
 endif
  NULLIFY(grid%landmask)
ENDIF
IF ( ASSOCIATED( grid%topostdv ) ) THEN 
  DEALLOCATE(grid%topostdv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4101,&
'frame/module_domain.f: Failed to dallocate grid%topostdv. ')
 endif
  NULLIFY(grid%topostdv)
ENDIF
IF ( ASSOCIATED( grid%toposlpx ) ) THEN 
  DEALLOCATE(grid%toposlpx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4109,&
'frame/module_domain.f: Failed to dallocate grid%toposlpx. ')
 endif
  NULLIFY(grid%toposlpx)
ENDIF
IF ( ASSOCIATED( grid%toposlpy ) ) THEN 
  DEALLOCATE(grid%toposlpy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4117,&
'frame/module_domain.f: Failed to dallocate grid%toposlpy. ')
 endif
  NULLIFY(grid%toposlpy)
ENDIF
IF ( ASSOCIATED( grid%greenmax ) ) THEN 
  DEALLOCATE(grid%greenmax,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4125,&
'frame/module_domain.f: Failed to dallocate grid%greenmax. ')
 endif
  NULLIFY(grid%greenmax)
ENDIF
IF ( ASSOCIATED( grid%greenmin ) ) THEN 
  DEALLOCATE(grid%greenmin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4133,&
'frame/module_domain.f: Failed to dallocate grid%greenmin. ')
 endif
  NULLIFY(grid%greenmin)
ENDIF
IF ( ASSOCIATED( grid%albedomx ) ) THEN 
  DEALLOCATE(grid%albedomx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4141,&
'frame/module_domain.f: Failed to dallocate grid%albedomx. ')
 endif
  NULLIFY(grid%albedomx)
ENDIF
IF ( ASSOCIATED( grid%slopecat ) ) THEN 
  DEALLOCATE(grid%slopecat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4149,&
'frame/module_domain.f: Failed to dallocate grid%slopecat. ')
 endif
  NULLIFY(grid%slopecat)
ENDIF
IF ( ASSOCIATED( grid%toposoil ) ) THEN 
  DEALLOCATE(grid%toposoil,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4157,&
'frame/module_domain.f: Failed to dallocate grid%toposoil. ')
 endif
  NULLIFY(grid%toposoil)
ENDIF
IF ( ASSOCIATED( grid%landusef ) ) THEN 
  DEALLOCATE(grid%landusef,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4165,&
'frame/module_domain.f: Failed to dallocate grid%landusef. ')
 endif
  NULLIFY(grid%landusef)
ENDIF
IF ( ASSOCIATED( grid%soilctop ) ) THEN 
  DEALLOCATE(grid%soilctop,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4173,&
'frame/module_domain.f: Failed to dallocate grid%soilctop. ')
 endif
  NULLIFY(grid%soilctop)
ENDIF
IF ( ASSOCIATED( grid%soilcbot ) ) THEN 
  DEALLOCATE(grid%soilcbot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4181,&
'frame/module_domain.f: Failed to dallocate grid%soilcbot. ')
 endif
  NULLIFY(grid%soilcbot)
ENDIF
IF ( ASSOCIATED( grid%ts_hour ) ) THEN 
  DEALLOCATE(grid%ts_hour,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4189,&
'frame/module_domain.f: Failed to dallocate grid%ts_hour. ')
 endif
  NULLIFY(grid%ts_hour)
ENDIF
IF ( ASSOCIATED( grid%ts_u ) ) THEN 
  DEALLOCATE(grid%ts_u,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4197,&
'frame/module_domain.f: Failed to dallocate grid%ts_u. ')
 endif
  NULLIFY(grid%ts_u)
ENDIF
IF ( ASSOCIATED( grid%ts_v ) ) THEN 
  DEALLOCATE(grid%ts_v,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4205,&
'frame/module_domain.f: Failed to dallocate grid%ts_v. ')
 endif
  NULLIFY(grid%ts_v)
ENDIF
IF ( ASSOCIATED( grid%ts_q ) ) THEN 
  DEALLOCATE(grid%ts_q,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4213,&
'frame/module_domain.f: Failed to dallocate grid%ts_q. ')
 endif
  NULLIFY(grid%ts_q)
ENDIF
IF ( ASSOCIATED( grid%ts_t ) ) THEN 
  DEALLOCATE(grid%ts_t,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4221,&
'frame/module_domain.f: Failed to dallocate grid%ts_t. ')
 endif
  NULLIFY(grid%ts_t)
ENDIF
IF ( ASSOCIATED( grid%ts_psfc ) ) THEN 
  DEALLOCATE(grid%ts_psfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4229,&
'frame/module_domain.f: Failed to dallocate grid%ts_psfc. ')
 endif
  NULLIFY(grid%ts_psfc)
ENDIF
IF ( ASSOCIATED( grid%ts_tsk ) ) THEN 
  DEALLOCATE(grid%ts_tsk,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4237,&
'frame/module_domain.f: Failed to dallocate grid%ts_tsk. ')
 endif
  NULLIFY(grid%ts_tsk)
ENDIF
IF ( ASSOCIATED( grid%ts_tslb ) ) THEN 
  DEALLOCATE(grid%ts_tslb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4245,&
'frame/module_domain.f: Failed to dallocate grid%ts_tslb. ')
 endif
  NULLIFY(grid%ts_tslb)
ENDIF
IF ( ASSOCIATED( grid%ts_clw ) ) THEN 
  DEALLOCATE(grid%ts_clw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4253,&
'frame/module_domain.f: Failed to dallocate grid%ts_clw. ')
 endif
  NULLIFY(grid%ts_clw)
ENDIF
IF ( ASSOCIATED( grid%moist ) ) THEN 
  DEALLOCATE(grid%moist,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4261,&
'frame/module_domain.f: Failed to dallocate grid%moist. ')
 endif
  NULLIFY(grid%moist)
ENDIF
IF ( ASSOCIATED( grid%dfi_moist ) ) THEN 
  DEALLOCATE(grid%dfi_moist,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4269,&
'frame/module_domain.f: Failed to dallocate grid%dfi_moist. ')
 endif
  NULLIFY(grid%dfi_moist)
ENDIF
IF ( ASSOCIATED( grid%scalar ) ) THEN 
  DEALLOCATE(grid%scalar,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4277,&
'frame/module_domain.f: Failed to dallocate grid%scalar. ')
 endif
  NULLIFY(grid%scalar)
ENDIF
IF ( ASSOCIATED( grid%scalar_bxs ) ) THEN 
  DEALLOCATE(grid%scalar_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4285,&
'frame/module_domain.f: Failed to dallocate grid%scalar_bxs. ')
 endif
  NULLIFY(grid%scalar_bxs)
ENDIF
IF ( ASSOCIATED( grid%scalar_bxe ) ) THEN 
  DEALLOCATE(grid%scalar_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4293,&
'frame/module_domain.f: Failed to dallocate grid%scalar_bxe. ')
 endif
  NULLIFY(grid%scalar_bxe)
ENDIF
IF ( ASSOCIATED( grid%scalar_bys ) ) THEN 
  DEALLOCATE(grid%scalar_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4301,&
'frame/module_domain.f: Failed to dallocate grid%scalar_bys. ')
 endif
  NULLIFY(grid%scalar_bys)
ENDIF
IF ( ASSOCIATED( grid%scalar_bye ) ) THEN 
  DEALLOCATE(grid%scalar_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4309,&
'frame/module_domain.f: Failed to dallocate grid%scalar_bye. ')
 endif
  NULLIFY(grid%scalar_bye)
ENDIF
IF ( ASSOCIATED( grid%scalar_btxs ) ) THEN 
  DEALLOCATE(grid%scalar_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4317,&
'frame/module_domain.f: Failed to dallocate grid%scalar_btxs. ')
 endif
  NULLIFY(grid%scalar_btxs)
ENDIF
IF ( ASSOCIATED( grid%scalar_btxe ) ) THEN 
  DEALLOCATE(grid%scalar_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4325,&
'frame/module_domain.f: Failed to dallocate grid%scalar_btxe. ')
 endif
  NULLIFY(grid%scalar_btxe)
ENDIF
IF ( ASSOCIATED( grid%scalar_btys ) ) THEN 
  DEALLOCATE(grid%scalar_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4333,&
'frame/module_domain.f: Failed to dallocate grid%scalar_btys. ')
 endif
  NULLIFY(grid%scalar_btys)
ENDIF
IF ( ASSOCIATED( grid%scalar_btye ) ) THEN 
  DEALLOCATE(grid%scalar_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4341,&
'frame/module_domain.f: Failed to dallocate grid%scalar_btye. ')
 endif
  NULLIFY(grid%scalar_btye)
ENDIF
IF ( ASSOCIATED( grid%dfi_scalar ) ) THEN 
  DEALLOCATE(grid%dfi_scalar,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4349,&
'frame/module_domain.f: Failed to dallocate grid%dfi_scalar. ')
 endif
  NULLIFY(grid%dfi_scalar)
ENDIF
IF ( ASSOCIATED( grid%dfi_scalar_bxs ) ) THEN 
  DEALLOCATE(grid%dfi_scalar_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4357,&
'frame/module_domain.f: Failed to dallocate grid%dfi_scalar_bxs. ')
 endif
  NULLIFY(grid%dfi_scalar_bxs)
ENDIF
IF ( ASSOCIATED( grid%dfi_scalar_bxe ) ) THEN 
  DEALLOCATE(grid%dfi_scalar_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4365,&
'frame/module_domain.f: Failed to dallocate grid%dfi_scalar_bxe. ')
 endif
  NULLIFY(grid%dfi_scalar_bxe)
ENDIF
IF ( ASSOCIATED( grid%dfi_scalar_bys ) ) THEN 
  DEALLOCATE(grid%dfi_scalar_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4373,&
'frame/module_domain.f: Failed to dallocate grid%dfi_scalar_bys. ')
 endif
  NULLIFY(grid%dfi_scalar_bys)
ENDIF
IF ( ASSOCIATED( grid%dfi_scalar_bye ) ) THEN 
  DEALLOCATE(grid%dfi_scalar_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4381,&
'frame/module_domain.f: Failed to dallocate grid%dfi_scalar_bye. ')
 endif
  NULLIFY(grid%dfi_scalar_bye)
ENDIF
IF ( ASSOCIATED( grid%dfi_scalar_btxs ) ) THEN 
  DEALLOCATE(grid%dfi_scalar_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4389,&
'frame/module_domain.f: Failed to dallocate grid%dfi_scalar_btxs. ')
 endif
  NULLIFY(grid%dfi_scalar_btxs)
ENDIF
IF ( ASSOCIATED( grid%dfi_scalar_btxe ) ) THEN 
  DEALLOCATE(grid%dfi_scalar_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4397,&
'frame/module_domain.f: Failed to dallocate grid%dfi_scalar_btxe. ')
 endif
  NULLIFY(grid%dfi_scalar_btxe)
ENDIF
IF ( ASSOCIATED( grid%dfi_scalar_btys ) ) THEN 
  DEALLOCATE(grid%dfi_scalar_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4405,&
'frame/module_domain.f: Failed to dallocate grid%dfi_scalar_btys. ')
 endif
  NULLIFY(grid%dfi_scalar_btys)
ENDIF
IF ( ASSOCIATED( grid%dfi_scalar_btye ) ) THEN 
  DEALLOCATE(grid%dfi_scalar_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4413,&
'frame/module_domain.f: Failed to dallocate grid%dfi_scalar_btye. ')
 endif
  NULLIFY(grid%dfi_scalar_btye)
ENDIF
IF ( ASSOCIATED( grid%chem ) ) THEN 
  DEALLOCATE(grid%chem,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4421,&
'frame/module_domain.f: Failed to dallocate grid%chem. ')
 endif
  NULLIFY(grid%chem)
ENDIF
IF ( ASSOCIATED( grid%smois ) ) THEN 
  DEALLOCATE(grid%smois,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4429,&
'frame/module_domain.f: Failed to dallocate grid%smois. ')
 endif
  NULLIFY(grid%smois)
ENDIF
IF ( ASSOCIATED( grid%tslb ) ) THEN 
  DEALLOCATE(grid%tslb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4437,&
'frame/module_domain.f: Failed to dallocate grid%tslb. ')
 endif
  NULLIFY(grid%tslb)
ENDIF
IF ( ASSOCIATED( grid%gsw ) ) THEN 
  DEALLOCATE(grid%gsw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4445,&
'frame/module_domain.f: Failed to dallocate grid%gsw. ')
 endif
  NULLIFY(grid%gsw)
ENDIF
IF ( ASSOCIATED( grid%xlat ) ) THEN 
  DEALLOCATE(grid%xlat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4453,&
'frame/module_domain.f: Failed to dallocate grid%xlat. ')
 endif
  NULLIFY(grid%xlat)
ENDIF
IF ( ASSOCIATED( grid%xlong ) ) THEN 
  DEALLOCATE(grid%xlong,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4461,&
'frame/module_domain.f: Failed to dallocate grid%xlong. ')
 endif
  NULLIFY(grid%xlong)
ENDIF
IF ( ASSOCIATED( grid%xland ) ) THEN 
  DEALLOCATE(grid%xland,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4469,&
'frame/module_domain.f: Failed to dallocate grid%xland. ')
 endif
  NULLIFY(grid%xland)
ENDIF
IF ( ASSOCIATED( grid%raincv ) ) THEN 
  DEALLOCATE(grid%raincv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4477,&
'frame/module_domain.f: Failed to dallocate grid%raincv. ')
 endif
  NULLIFY(grid%raincv)
ENDIF
IF ( ASSOCIATED( grid%psfc ) ) THEN 
  DEALLOCATE(grid%psfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4485,&
'frame/module_domain.f: Failed to dallocate grid%psfc. ')
 endif
  NULLIFY(grid%psfc)
ENDIF
IF ( ASSOCIATED( grid%th2 ) ) THEN 
  DEALLOCATE(grid%th2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4493,&
'frame/module_domain.f: Failed to dallocate grid%th2. ')
 endif
  NULLIFY(grid%th2)
ENDIF
IF ( ASSOCIATED( grid%t2 ) ) THEN 
  DEALLOCATE(grid%t2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4501,&
'frame/module_domain.f: Failed to dallocate grid%t2. ')
 endif
  NULLIFY(grid%t2)
ENDIF
IF ( ASSOCIATED( grid%u10 ) ) THEN 
  DEALLOCATE(grid%u10,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4509,&
'frame/module_domain.f: Failed to dallocate grid%u10. ')
 endif
  NULLIFY(grid%u10)
ENDIF
IF ( ASSOCIATED( grid%v10 ) ) THEN 
  DEALLOCATE(grid%v10,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4517,&
'frame/module_domain.f: Failed to dallocate grid%v10. ')
 endif
  NULLIFY(grid%v10)
ENDIF
IF ( ASSOCIATED( grid%xice ) ) THEN 
  DEALLOCATE(grid%xice,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4525,&
'frame/module_domain.f: Failed to dallocate grid%xice. ')
 endif
  NULLIFY(grid%xice)
ENDIF
IF ( ASSOCIATED( grid%lai ) ) THEN 
  DEALLOCATE(grid%lai,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4533,&
'frame/module_domain.f: Failed to dallocate grid%lai. ')
 endif
  NULLIFY(grid%lai)
ENDIF
IF ( ASSOCIATED( grid%smstav ) ) THEN 
  DEALLOCATE(grid%smstav,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4541,&
'frame/module_domain.f: Failed to dallocate grid%smstav. ')
 endif
  NULLIFY(grid%smstav)
ENDIF
IF ( ASSOCIATED( grid%smstot ) ) THEN 
  DEALLOCATE(grid%smstot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4549,&
'frame/module_domain.f: Failed to dallocate grid%smstot. ')
 endif
  NULLIFY(grid%smstot)
ENDIF
IF ( ASSOCIATED( grid%sfcrunoff ) ) THEN 
  DEALLOCATE(grid%sfcrunoff,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4557,&
'frame/module_domain.f: Failed to dallocate grid%sfcrunoff. ')
 endif
  NULLIFY(grid%sfcrunoff)
ENDIF
IF ( ASSOCIATED( grid%udrunoff ) ) THEN 
  DEALLOCATE(grid%udrunoff,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4565,&
'frame/module_domain.f: Failed to dallocate grid%udrunoff. ')
 endif
  NULLIFY(grid%udrunoff)
ENDIF
IF ( ASSOCIATED( grid%ivgtyp ) ) THEN 
  DEALLOCATE(grid%ivgtyp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4573,&
'frame/module_domain.f: Failed to dallocate grid%ivgtyp. ')
 endif
  NULLIFY(grid%ivgtyp)
ENDIF
IF ( ASSOCIATED( grid%isltyp ) ) THEN 
  DEALLOCATE(grid%isltyp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4581,&
'frame/module_domain.f: Failed to dallocate grid%isltyp. ')
 endif
  NULLIFY(grid%isltyp)
ENDIF
IF ( ASSOCIATED( grid%vegfra ) ) THEN 
  DEALLOCATE(grid%vegfra,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4589,&
'frame/module_domain.f: Failed to dallocate grid%vegfra. ')
 endif
  NULLIFY(grid%vegfra)
ENDIF
IF ( ASSOCIATED( grid%sfcevp ) ) THEN 
  DEALLOCATE(grid%sfcevp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4597,&
'frame/module_domain.f: Failed to dallocate grid%sfcevp. ')
 endif
  NULLIFY(grid%sfcevp)
ENDIF
IF ( ASSOCIATED( grid%grdflx ) ) THEN 
  DEALLOCATE(grid%grdflx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4605,&
'frame/module_domain.f: Failed to dallocate grid%grdflx. ')
 endif
  NULLIFY(grid%grdflx)
ENDIF
IF ( ASSOCIATED( grid%albbck ) ) THEN 
  DEALLOCATE(grid%albbck,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4613,&
'frame/module_domain.f: Failed to dallocate grid%albbck. ')
 endif
  NULLIFY(grid%albbck)
ENDIF
IF ( ASSOCIATED( grid%sfcexc ) ) THEN 
  DEALLOCATE(grid%sfcexc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4621,&
'frame/module_domain.f: Failed to dallocate grid%sfcexc. ')
 endif
  NULLIFY(grid%sfcexc)
ENDIF
IF ( ASSOCIATED( grid%snotime ) ) THEN 
  DEALLOCATE(grid%snotime,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4629,&
'frame/module_domain.f: Failed to dallocate grid%snotime. ')
 endif
  NULLIFY(grid%snotime)
ENDIF
IF ( ASSOCIATED( grid%acsnow ) ) THEN 
  DEALLOCATE(grid%acsnow,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4637,&
'frame/module_domain.f: Failed to dallocate grid%acsnow. ')
 endif
  NULLIFY(grid%acsnow)
ENDIF
IF ( ASSOCIATED( grid%acsnom ) ) THEN 
  DEALLOCATE(grid%acsnom,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4645,&
'frame/module_domain.f: Failed to dallocate grid%acsnom. ')
 endif
  NULLIFY(grid%acsnom)
ENDIF
IF ( ASSOCIATED( grid%rmol ) ) THEN 
  DEALLOCATE(grid%rmol,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4653,&
'frame/module_domain.f: Failed to dallocate grid%rmol. ')
 endif
  NULLIFY(grid%rmol)
ENDIF
IF ( ASSOCIATED( grid%snow ) ) THEN 
  DEALLOCATE(grid%snow,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4661,&
'frame/module_domain.f: Failed to dallocate grid%snow. ')
 endif
  NULLIFY(grid%snow)
ENDIF
IF ( ASSOCIATED( grid%canwat ) ) THEN 
  DEALLOCATE(grid%canwat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4669,&
'frame/module_domain.f: Failed to dallocate grid%canwat. ')
 endif
  NULLIFY(grid%canwat)
ENDIF
IF ( ASSOCIATED( grid%sst ) ) THEN 
  DEALLOCATE(grid%sst,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4677,&
'frame/module_domain.f: Failed to dallocate grid%sst. ')
 endif
  NULLIFY(grid%sst)
ENDIF
IF ( ASSOCIATED( grid%weasd ) ) THEN 
  DEALLOCATE(grid%weasd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4685,&
'frame/module_domain.f: Failed to dallocate grid%weasd. ')
 endif
  NULLIFY(grid%weasd)
ENDIF
IF ( ASSOCIATED( grid%znt ) ) THEN 
  DEALLOCATE(grid%znt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4693,&
'frame/module_domain.f: Failed to dallocate grid%znt. ')
 endif
  NULLIFY(grid%znt)
ENDIF
IF ( ASSOCIATED( grid%mol ) ) THEN 
  DEALLOCATE(grid%mol,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4701,&
'frame/module_domain.f: Failed to dallocate grid%mol. ')
 endif
  NULLIFY(grid%mol)
ENDIF
IF ( ASSOCIATED( grid%noahres ) ) THEN 
  DEALLOCATE(grid%noahres,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4709,&
'frame/module_domain.f: Failed to dallocate grid%noahres. ')
 endif
  NULLIFY(grid%noahres)
ENDIF
IF ( ASSOCIATED( grid%tke_myj ) ) THEN 
  DEALLOCATE(grid%tke_myj,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4717,&
'frame/module_domain.f: Failed to dallocate grid%tke_myj. ')
 endif
  NULLIFY(grid%tke_myj)
ENDIF
IF ( ASSOCIATED( grid%el_myj ) ) THEN 
  DEALLOCATE(grid%el_myj,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4725,&
'frame/module_domain.f: Failed to dallocate grid%el_myj. ')
 endif
  NULLIFY(grid%el_myj)
ENDIF
IF ( ASSOCIATED( grid%exch_h ) ) THEN 
  DEALLOCATE(grid%exch_h,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4733,&
'frame/module_domain.f: Failed to dallocate grid%exch_h. ')
 endif
  NULLIFY(grid%exch_h)
ENDIF
IF ( ASSOCIATED( grid%exch_m ) ) THEN 
  DEALLOCATE(grid%exch_m,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4741,&
'frame/module_domain.f: Failed to dallocate grid%exch_m. ')
 endif
  NULLIFY(grid%exch_m)
ENDIF
IF ( ASSOCIATED( grid%thz0 ) ) THEN 
  DEALLOCATE(grid%thz0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4749,&
'frame/module_domain.f: Failed to dallocate grid%thz0. ')
 endif
  NULLIFY(grid%thz0)
ENDIF
IF ( ASSOCIATED( grid%qz0 ) ) THEN 
  DEALLOCATE(grid%qz0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4757,&
'frame/module_domain.f: Failed to dallocate grid%qz0. ')
 endif
  NULLIFY(grid%qz0)
ENDIF
IF ( ASSOCIATED( grid%uz0 ) ) THEN 
  DEALLOCATE(grid%uz0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4765,&
'frame/module_domain.f: Failed to dallocate grid%uz0. ')
 endif
  NULLIFY(grid%uz0)
ENDIF
IF ( ASSOCIATED( grid%vz0 ) ) THEN 
  DEALLOCATE(grid%vz0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4773,&
'frame/module_domain.f: Failed to dallocate grid%vz0. ')
 endif
  NULLIFY(grid%vz0)
ENDIF
IF ( ASSOCIATED( grid%flhc ) ) THEN 
  DEALLOCATE(grid%flhc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4781,&
'frame/module_domain.f: Failed to dallocate grid%flhc. ')
 endif
  NULLIFY(grid%flhc)
ENDIF
IF ( ASSOCIATED( grid%flqc ) ) THEN 
  DEALLOCATE(grid%flqc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4789,&
'frame/module_domain.f: Failed to dallocate grid%flqc. ')
 endif
  NULLIFY(grid%flqc)
ENDIF
IF ( ASSOCIATED( grid%qsg ) ) THEN 
  DEALLOCATE(grid%qsg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4797,&
'frame/module_domain.f: Failed to dallocate grid%qsg. ')
 endif
  NULLIFY(grid%qsg)
ENDIF
IF ( ASSOCIATED( grid%qvg ) ) THEN 
  DEALLOCATE(grid%qvg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4805,&
'frame/module_domain.f: Failed to dallocate grid%qvg. ')
 endif
  NULLIFY(grid%qvg)
ENDIF
IF ( ASSOCIATED( grid%qcg ) ) THEN 
  DEALLOCATE(grid%qcg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4813,&
'frame/module_domain.f: Failed to dallocate grid%qcg. ')
 endif
  NULLIFY(grid%qcg)
ENDIF
IF ( ASSOCIATED( grid%soilt1 ) ) THEN 
  DEALLOCATE(grid%soilt1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4821,&
'frame/module_domain.f: Failed to dallocate grid%soilt1. ')
 endif
  NULLIFY(grid%soilt1)
ENDIF
IF ( ASSOCIATED( grid%tsnav ) ) THEN 
  DEALLOCATE(grid%tsnav,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4829,&
'frame/module_domain.f: Failed to dallocate grid%tsnav. ')
 endif
  NULLIFY(grid%tsnav)
ENDIF
IF ( ASSOCIATED( grid%psfc_out ) ) THEN 
  DEALLOCATE(grid%psfc_out,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4837,&
'frame/module_domain.f: Failed to dallocate grid%psfc_out. ')
 endif
  NULLIFY(grid%psfc_out)
ENDIF
IF ( ASSOCIATED( grid%uz0h ) ) THEN 
  DEALLOCATE(grid%uz0h,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4845,&
'frame/module_domain.f: Failed to dallocate grid%uz0h. ')
 endif
  NULLIFY(grid%uz0h)
ENDIF
IF ( ASSOCIATED( grid%vz0h ) ) THEN 
  DEALLOCATE(grid%vz0h,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4853,&
'frame/module_domain.f: Failed to dallocate grid%vz0h. ')
 endif
  NULLIFY(grid%vz0h)
ENDIF
IF ( ASSOCIATED( grid%dudt ) ) THEN 
  DEALLOCATE(grid%dudt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4861,&
'frame/module_domain.f: Failed to dallocate grid%dudt. ')
 endif
  NULLIFY(grid%dudt)
ENDIF
IF ( ASSOCIATED( grid%dvdt ) ) THEN 
  DEALLOCATE(grid%dvdt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4869,&
'frame/module_domain.f: Failed to dallocate grid%dvdt. ')
 endif
  NULLIFY(grid%dvdt)
ENDIF
IF ( ASSOCIATED( grid%qsfc ) ) THEN 
  DEALLOCATE(grid%qsfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4877,&
'frame/module_domain.f: Failed to dallocate grid%qsfc. ')
 endif
  NULLIFY(grid%qsfc)
ENDIF
IF ( ASSOCIATED( grid%akhs ) ) THEN 
  DEALLOCATE(grid%akhs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4885,&
'frame/module_domain.f: Failed to dallocate grid%akhs. ')
 endif
  NULLIFY(grid%akhs)
ENDIF
IF ( ASSOCIATED( grid%akms ) ) THEN 
  DEALLOCATE(grid%akms,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4893,&
'frame/module_domain.f: Failed to dallocate grid%akms. ')
 endif
  NULLIFY(grid%akms)
ENDIF
IF ( ASSOCIATED( grid%htop ) ) THEN 
  DEALLOCATE(grid%htop,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4901,&
'frame/module_domain.f: Failed to dallocate grid%htop. ')
 endif
  NULLIFY(grid%htop)
ENDIF
IF ( ASSOCIATED( grid%hbot ) ) THEN 
  DEALLOCATE(grid%hbot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4909,&
'frame/module_domain.f: Failed to dallocate grid%hbot. ')
 endif
  NULLIFY(grid%hbot)
ENDIF
IF ( ASSOCIATED( grid%htopr ) ) THEN 
  DEALLOCATE(grid%htopr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4917,&
'frame/module_domain.f: Failed to dallocate grid%htopr. ')
 endif
  NULLIFY(grid%htopr)
ENDIF
IF ( ASSOCIATED( grid%hbotr ) ) THEN 
  DEALLOCATE(grid%hbotr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4925,&
'frame/module_domain.f: Failed to dallocate grid%hbotr. ')
 endif
  NULLIFY(grid%hbotr)
ENDIF
IF ( ASSOCIATED( grid%htopd ) ) THEN 
  DEALLOCATE(grid%htopd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4933,&
'frame/module_domain.f: Failed to dallocate grid%htopd. ')
 endif
  NULLIFY(grid%htopd)
ENDIF
IF ( ASSOCIATED( grid%hbotd ) ) THEN 
  DEALLOCATE(grid%hbotd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4941,&
'frame/module_domain.f: Failed to dallocate grid%hbotd. ')
 endif
  NULLIFY(grid%hbotd)
ENDIF
IF ( ASSOCIATED( grid%htops ) ) THEN 
  DEALLOCATE(grid%htops,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4949,&
'frame/module_domain.f: Failed to dallocate grid%htops. ')
 endif
  NULLIFY(grid%htops)
ENDIF
IF ( ASSOCIATED( grid%hbots ) ) THEN 
  DEALLOCATE(grid%hbots,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4957,&
'frame/module_domain.f: Failed to dallocate grid%hbots. ')
 endif
  NULLIFY(grid%hbots)
ENDIF
IF ( ASSOCIATED( grid%cuppt ) ) THEN 
  DEALLOCATE(grid%cuppt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4965,&
'frame/module_domain.f: Failed to dallocate grid%cuppt. ')
 endif
  NULLIFY(grid%cuppt)
ENDIF
IF ( ASSOCIATED( grid%cprate ) ) THEN 
  DEALLOCATE(grid%cprate,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4973,&
'frame/module_domain.f: Failed to dallocate grid%cprate. ')
 endif
  NULLIFY(grid%cprate)
ENDIF
IF ( ASSOCIATED( grid%f_ice_phy ) ) THEN 
  DEALLOCATE(grid%f_ice_phy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4981,&
'frame/module_domain.f: Failed to dallocate grid%f_ice_phy. ')
 endif
  NULLIFY(grid%f_ice_phy)
ENDIF
IF ( ASSOCIATED( grid%f_rain_phy ) ) THEN 
  DEALLOCATE(grid%f_rain_phy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4989,&
'frame/module_domain.f: Failed to dallocate grid%f_rain_phy. ')
 endif
  NULLIFY(grid%f_rain_phy)
ENDIF
IF ( ASSOCIATED( grid%f_rimef_phy ) ) THEN 
  DEALLOCATE(grid%f_rimef_phy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",4997,&
'frame/module_domain.f: Failed to dallocate grid%f_rimef_phy. ')
 endif
  NULLIFY(grid%f_rimef_phy)
ENDIF
IF ( ASSOCIATED( grid%mass_flux ) ) THEN 
  DEALLOCATE(grid%mass_flux,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",5005,&
'frame/module_domain.f: Failed to dallocate grid%mass_flux. ')
 endif
  NULLIFY(grid%mass_flux)
ENDIF
IF ( ASSOCIATED( grid%apr_gr ) ) THEN 
  DEALLOCATE(grid%apr_gr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",5013,&
'frame/module_domain.f: Failed to dallocate grid%apr_gr. ')
 endif
  NULLIFY(grid%apr_gr)
ENDIF
IF ( ASSOCIATED( grid%apr_w ) ) THEN 
  DEALLOCATE(grid%apr_w,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",5021,&
'frame/module_domain.f: Failed to dallocate grid%apr_w. ')
 endif
  NULLIFY(grid%apr_w)
ENDIF
IF ( ASSOCIATED( grid%apr_mc ) ) THEN 
  DEALLOCATE(grid%apr_mc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",5029,&
'frame/module_domain.f: Failed to dallocate grid%apr_mc. ')
 endif
  NULLIFY(grid%apr_mc)
ENDIF
IF ( ASSOCIATED( grid%apr_st ) ) THEN 
  DEALLOCATE(grid%apr_st,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",5037,&
'frame/module_domain.f: Failed to dallocate grid%apr_st. ')
 endif
  NULLIFY(grid%apr_st)
ENDIF
IF ( ASSOCIATED( grid%apr_as ) ) THEN 
  DEALLOCATE(grid%apr_as,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",5045,&
'frame/module_domain.f: Failed to dallocate grid%apr_as. ')
 endif
  NULLIFY(grid%apr_as)
ENDIF
IF ( ASSOCIATED( grid%apr_capma ) ) THEN 
  DEALLOCATE(grid%apr_capma,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",5053,&
'frame/module_domain.f: Failed to dallocate grid%apr_capma. ')
 endif
  NULLIFY(grid%apr_capma)
ENDIF
IF ( ASSOCIATED( grid%apr_capme ) ) THEN 
  DEALLOCATE(grid%apr_capme,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",5061,&
'frame/module_domain.f: Failed to dallocate grid%apr_capme. ')
 endif
  NULLIFY(grid%apr_capme)
ENDIF
IF ( ASSOCIATED( grid%apr_capmi ) ) THEN 
  DEALLOCATE(grid%apr_capmi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",5069,&
'frame/module_domain.f: Failed to dallocate grid%apr_capmi. ')
 endif
  NULLIFY(grid%apr_capmi)
ENDIF
IF ( ASSOCIATED( grid%xf_ens ) ) THEN 
  DEALLOCATE(grid%xf_ens,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",5077,&
'frame/module_domain.f: Failed to dallocate grid%xf_ens. ')
 endif
  NULLIFY(grid%xf_ens)
ENDIF
IF ( ASSOCIATED( grid%pr_ens ) ) THEN 
  DEALLOCATE(grid%pr_ens,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",5085,&
'frame/module_domain.f: Failed to dallocate grid%pr_ens. ')
 endif
  NULLIFY(grid%pr_ens)
ENDIF
IF ( ASSOCIATED( grid%rthften ) ) THEN 
  DEALLOCATE(grid%rthften,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",5093,&
'frame/module_domain.f: Failed to dallocate grid%rthften. ')
 endif
  NULLIFY(grid%rthften)
ENDIF
IF ( ASSOCIATED( grid%rqvften ) ) THEN 
  DEALLOCATE(grid%rqvften,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",5101,&
'frame/module_domain.f: Failed to dallocate grid%rqvften. ')
 endif
  NULLIFY(grid%rqvften)
ENDIF
IF ( ASSOCIATED( grid%snowh ) ) THEN 
  DEALLOCATE(grid%snowh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",5109,&
'frame/module_domain.f: Failed to dallocate grid%snowh. ')
 endif
  NULLIFY(grid%snowh)
ENDIF
IF ( ASSOCIATED( grid%rhosn ) ) THEN 
  DEALLOCATE(grid%rhosn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",5117,&
'frame/module_domain.f: Failed to dallocate grid%rhosn. ')
 endif
  NULLIFY(grid%rhosn)
ENDIF
IF ( ASSOCIATED( grid%smfr3d ) ) THEN 
  DEALLOCATE(grid%smfr3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",5125,&
'frame/module_domain.f: Failed to dallocate grid%smfr3d. ')
 endif
  NULLIFY(grid%smfr3d)
ENDIF
IF ( ASSOCIATED( grid%keepfr3dflag ) ) THEN 
  DEALLOCATE(grid%keepfr3dflag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",5133,&
'frame/module_domain.f: Failed to dallocate grid%keepfr3dflag. ')
 endif
  NULLIFY(grid%keepfr3dflag)
ENDIF
IF ( ASSOCIATED( grid%mp_restart_state ) ) THEN 
  DEALLOCATE(grid%mp_restart_state,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",5141,&
'frame/module_domain.f: Failed to dallocate grid%mp_restart_state. ')
 endif
  NULLIFY(grid%mp_restart_state)
ENDIF
IF ( ASSOCIATED( grid%tbpvs_state ) ) THEN 
  DEALLOCATE(grid%tbpvs_state,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",5149,&
'frame/module_domain.f: Failed to dallocate grid%tbpvs_state. ')
 endif
  NULLIFY(grid%tbpvs_state)
ENDIF
IF ( ASSOCIATED( grid%tbpvs0_state ) ) THEN 
  DEALLOCATE(grid%tbpvs0_state,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",5157,&
'frame/module_domain.f: Failed to dallocate grid%tbpvs0_state. ')
 endif
  NULLIFY(grid%tbpvs0_state)
ENDIF
IF ( ASSOCIATED( grid%lu_state ) ) THEN 
  DEALLOCATE(grid%lu_state,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("",5165,&
'frame/module_domain.f: Failed to dallocate grid%lu_state. ')
 endif
  NULLIFY(grid%lu_state)
ENDIF


   END SUBROUTINE dealloc_space_field



   RECURSIVE SUBROUTINE find_grid_by_id ( id, in_grid, result_grid )
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: id
      TYPE(domain), POINTER     :: in_grid 
      TYPE(domain), POINTER     :: result_grid






      TYPE(domain), POINTER     :: grid_ptr
      INTEGER                   :: kid
      LOGICAL                   :: found
      found = .FALSE.
      IF ( ASSOCIATED( in_grid ) ) THEN
      IF ( in_grid%id .EQ. id ) THEN
         result_grid => in_grid
      ELSE
         grid_ptr => in_grid
         DO WHILE ( ASSOCIATED( grid_ptr ) .AND. .NOT. found )
            DO kid = 1, max_nests
               IF ( ASSOCIATED( grid_ptr%nests(kid)%ptr ) .AND. .NOT. found ) THEN
                  CALL find_grid_by_id ( id, grid_ptr%nests(kid)%ptr, result_grid )
                  IF ( ASSOCIATED( result_grid ) ) THEN
                    IF ( result_grid%id .EQ. id ) found = .TRUE.
                  ENDIF
               ENDIF
            ENDDO
            IF ( .NOT. found ) grid_ptr => grid_ptr%sibling
         ENDDO
      ENDIF
      ENDIF
      RETURN
   END SUBROUTINE find_grid_by_id


   FUNCTION first_loc_integer ( array , search ) RESULT ( loc ) 
 
      IMPLICIT NONE

      

      INTEGER , INTENT(IN) , DIMENSION(:) :: array
      INTEGER , INTENT(IN)                :: search

      

      INTEGER                             :: loc






      
      

      INTEGER :: loop

      loc = -1
      find : DO loop = 1 , SIZE(array)
         IF ( search == array(loop) ) THEN         
            loc = loop
            EXIT find
         END IF
      END DO find

   END FUNCTION first_loc_integer

   SUBROUTINE init_module_domain
   END SUBROUTINE init_module_domain










      FUNCTION domain_get_current_time ( grid ) RESULT ( current_time ) 
        IMPLICIT NONE




        TYPE(domain), INTENT(IN) :: grid
        
        TYPE(WRFU_Time) :: current_time
        
        INTEGER :: rc
        CALL WRFU_ClockGet( grid%domain_clock, CurrTime=current_time, &
                            rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL wrf_error_fatal3("",5272,&
            'domain_get_current_time:  WRFU_ClockGet failed' )
        ENDIF
      END FUNCTION domain_get_current_time


      FUNCTION domain_get_start_time ( grid ) RESULT ( start_time ) 
        IMPLICIT NONE




        TYPE(domain), INTENT(IN) :: grid
        
        TYPE(WRFU_Time) :: start_time
        
        INTEGER :: rc
        CALL WRFU_ClockGet( grid%domain_clock, StartTime=start_time, &
                            rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL wrf_error_fatal3("",5292,&
            'domain_get_start_time:  WRFU_ClockGet failed' )
        ENDIF
      END FUNCTION domain_get_start_time


      FUNCTION domain_get_stop_time ( grid ) RESULT ( stop_time ) 
        IMPLICIT NONE




        TYPE(domain), INTENT(IN) :: grid
        
        TYPE(WRFU_Time) :: stop_time
        
        INTEGER :: rc
        CALL WRFU_ClockGet( grid%domain_clock, StopTime=stop_time, &
                            rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL wrf_error_fatal3("",5312,&
            'domain_get_stop_time:  WRFU_ClockGet failed' )
        ENDIF
      END FUNCTION domain_get_stop_time


      FUNCTION domain_get_time_step ( grid ) RESULT ( time_step ) 
        IMPLICIT NONE




        TYPE(domain), INTENT(IN) :: grid
        
        TYPE(WRFU_TimeInterval) :: time_step
        
        INTEGER :: rc
        CALL WRFU_ClockGet( grid%domain_clock, timeStep=time_step, &
                            rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL wrf_error_fatal3("",5332,&
            'domain_get_time_step:  WRFU_ClockGet failed' )
        ENDIF
      END FUNCTION domain_get_time_step


      FUNCTION domain_get_advanceCount ( grid ) RESULT ( advanceCount ) 
        IMPLICIT NONE





        TYPE(domain), INTENT(IN) :: grid
        
        INTEGER :: advanceCount
        
        INTEGER(WRFU_KIND_I8) :: advanceCountLcl
        INTEGER :: rc
        CALL WRFU_ClockGet( grid%domain_clock, &
                            advanceCount=advanceCountLcl, &
                            rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL wrf_error_fatal3("",5355,&
            'domain_get_advanceCount:  WRFU_ClockGet failed' )
        ENDIF
        advanceCount = advanceCountLcl
      END FUNCTION domain_get_advanceCount


      SUBROUTINE domain_alarms_destroy ( grid )
        IMPLICIT NONE





        TYPE(domain), INTENT(INOUT) :: grid
        
        INTEGER                     :: alarmid

        IF ( ASSOCIATED( grid%alarms ) .AND. &
             ASSOCIATED( grid%alarms_created ) ) THEN
          DO alarmid = 1, MAX_WRF_ALARMS
            IF ( grid%alarms_created( alarmid ) ) THEN
              CALL WRFU_AlarmDestroy( grid%alarms( alarmid ) )
              grid%alarms_created( alarmid ) = .FALSE.
            ENDIF
          ENDDO
          DEALLOCATE( grid%alarms )
          NULLIFY( grid%alarms )
          DEALLOCATE( grid%alarms_created )
          NULLIFY( grid%alarms_created )
        ENDIF
      END SUBROUTINE domain_alarms_destroy


      SUBROUTINE domain_clock_destroy ( grid )
        IMPLICIT NONE




        TYPE(domain), INTENT(INOUT) :: grid
        IF ( ASSOCIATED( grid%domain_clock ) ) THEN
          IF ( grid%domain_clock_created ) THEN
            CALL WRFU_ClockDestroy( grid%domain_clock )
            grid%domain_clock_created = .FALSE.
          ENDIF
          DEALLOCATE( grid%domain_clock )
          NULLIFY( grid%domain_clock )
        ENDIF
      END SUBROUTINE domain_clock_destroy


      FUNCTION domain_last_time_step ( grid ) RESULT ( LAST_TIME ) 
        IMPLICIT NONE





        TYPE(domain), INTENT(IN) :: grid
        
        LOGICAL :: LAST_TIME
        LAST_TIME =   domain_get_stop_time( grid ) .EQ. &
                    ( domain_get_current_time( grid ) + &
                      domain_get_time_step( grid ) )
      END FUNCTION domain_last_time_step



      FUNCTION domain_clockisstoptime ( grid ) RESULT ( is_stop_time ) 
        IMPLICIT NONE





        TYPE(domain), INTENT(IN) :: grid
        
        LOGICAL :: is_stop_time
        INTEGER :: rc
        is_stop_time = WRFU_ClockIsStopTime( grid%domain_clock , rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL wrf_error_fatal3("",5437,&
            'domain_clockisstoptime:  WRFU_ClockIsStopTime() failed' )
        ENDIF
      END FUNCTION domain_clockisstoptime



      FUNCTION domain_clockisstopsubtime ( grid ) RESULT ( is_stop_subtime ) 
        IMPLICIT NONE





        TYPE(domain), INTENT(IN) :: grid
        
        LOGICAL :: is_stop_subtime
        INTEGER :: rc
        TYPE(WRFU_TimeInterval) :: timeStep
        TYPE(WRFU_Time) :: currentTime
        LOGICAL :: positive_timestep
        is_stop_subtime = .FALSE.
        CALL domain_clock_get( grid, time_step=timeStep, &
                                     current_time=currentTime )
        positive_timestep = ESMF_TimeIntervalIsPositive( timeStep )
        IF ( positive_timestep ) THEN


          IF ( ESMF_TimeGE( currentTime, grid%stop_subtime ) ) THEN
            is_stop_subtime = .TRUE.
          ENDIF
        ELSE


          IF ( ESMF_TimeLE( currentTime, grid%stop_subtime ) ) THEN
            is_stop_subtime = .TRUE.
          ENDIF
        ENDIF
      END FUNCTION domain_clockisstopsubtime




      FUNCTION domain_get_sim_start_time ( grid ) RESULT ( simulationStartTime ) 
        IMPLICIT NONE












        TYPE(domain), INTENT(IN) :: grid
        
        TYPE(WRFU_Time) :: simulationStartTime
        
        INTEGER :: rc
        INTEGER :: simulation_start_year,   simulation_start_month, &
                   simulation_start_day,    simulation_start_hour , &
                   simulation_start_minute, simulation_start_second
        CALL nl_get_simulation_start_year   ( 1, simulation_start_year   )
        CALL nl_get_simulation_start_month  ( 1, simulation_start_month  )
        CALL nl_get_simulation_start_day    ( 1, simulation_start_day    )
        CALL nl_get_simulation_start_hour   ( 1, simulation_start_hour   )
        CALL nl_get_simulation_start_minute ( 1, simulation_start_minute )
        CALL nl_get_simulation_start_second ( 1, simulation_start_second )
        CALL WRFU_TimeSet( simulationStartTime,       &
                           YY=simulation_start_year,  &
                           MM=simulation_start_month, &
                           DD=simulation_start_day,   &
                           H=simulation_start_hour,   &
                           M=simulation_start_minute, &
                           S=simulation_start_second, &
                           rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL nl_get_start_year   ( 1, simulation_start_year   )
          CALL nl_get_start_month  ( 1, simulation_start_month  )
          CALL nl_get_start_day    ( 1, simulation_start_day    )
          CALL nl_get_start_hour   ( 1, simulation_start_hour   )
          CALL nl_get_start_minute ( 1, simulation_start_minute )
          CALL nl_get_start_second ( 1, simulation_start_second )
          CALL wrf_debug( 150, "WARNING:  domain_get_sim_start_time using head_grid start time from namelist" )
          CALL WRFU_TimeSet( simulationStartTime,       &
                             YY=simulation_start_year,  &
                             MM=simulation_start_month, &
                             DD=simulation_start_day,   &
                             H=simulation_start_hour,   &
                             M=simulation_start_minute, &
                             S=simulation_start_second, &
                             rc=rc )
        ENDIF
        RETURN
      END FUNCTION domain_get_sim_start_time

      FUNCTION domain_get_time_since_sim_start ( grid ) RESULT ( time_since_sim_start ) 
        IMPLICIT NONE









        TYPE(domain), INTENT(IN) :: grid
        
        TYPE(WRFU_TimeInterval) :: time_since_sim_start
        
        TYPE(WRFU_Time) :: lcl_currtime, lcl_simstarttime
        lcl_simstarttime = domain_get_sim_start_time( grid )
        lcl_currtime = domain_get_current_time ( grid )
        time_since_sim_start = lcl_currtime - lcl_simstarttime
      END FUNCTION domain_get_time_since_sim_start




      SUBROUTINE domain_clock_get( grid, current_time,                &
                                         current_timestr,             &
                                         current_timestr_frac,        &
                                         start_time, start_timestr,   &
                                         stop_time, stop_timestr,     &
                                         time_step, time_stepstr,     &
                                         time_stepstr_frac,           &
                                         advanceCount,                &
                                         currentDayOfYearReal,        &
                                         minutesSinceSimulationStart, &
                                         timeSinceSimulationStart,    &
                                         simulationStartTime,         &
                                         simulationStartTimeStr )
        IMPLICIT NONE
        TYPE(domain),            INTENT(IN)              :: grid
        TYPE(WRFU_Time),         INTENT(  OUT), OPTIONAL :: current_time
        CHARACTER (LEN=*),       INTENT(  OUT), OPTIONAL :: current_timestr
        CHARACTER (LEN=*),       INTENT(  OUT), OPTIONAL :: current_timestr_frac
        TYPE(WRFU_Time),         INTENT(  OUT), OPTIONAL :: start_time
        CHARACTER (LEN=*),       INTENT(  OUT), OPTIONAL :: start_timestr
        TYPE(WRFU_Time),         INTENT(  OUT), OPTIONAL :: stop_time
        CHARACTER (LEN=*),       INTENT(  OUT), OPTIONAL :: stop_timestr
        TYPE(WRFU_TimeInterval), INTENT(  OUT), OPTIONAL :: time_step
        CHARACTER (LEN=*),       INTENT(  OUT), OPTIONAL :: time_stepstr
        CHARACTER (LEN=*),       INTENT(  OUT), OPTIONAL :: time_stepstr_frac
        INTEGER,                 INTENT(  OUT), OPTIONAL :: advanceCount
        
        
        REAL,                    INTENT(  OUT), OPTIONAL :: currentDayOfYearReal
        
        
        TYPE(WRFU_Time),         INTENT(  OUT), OPTIONAL :: simulationStartTime
        CHARACTER (LEN=*),       INTENT(  OUT), OPTIONAL :: simulationStartTimeStr
        
        
        TYPE(WRFU_TimeInterval), INTENT(  OUT), OPTIONAL :: timeSinceSimulationStart
        
        REAL,                    INTENT(  OUT), OPTIONAL :: minutesSinceSimulationStart






        
        TYPE(WRFU_Time) :: lcl_currtime, lcl_stoptime, lcl_starttime
        TYPE(WRFU_Time) :: lcl_simulationStartTime
        TYPE(WRFU_TimeInterval) :: lcl_time_step, lcl_timeSinceSimulationStart
        INTEGER :: days, seconds, Sn, Sd, rc
        CHARACTER (LEN=256) :: tmp_str
        CHARACTER (LEN=256) :: frac_str
        REAL(WRFU_KIND_R8) :: currentDayOfYearR8
        IF ( PRESENT( start_time ) ) THEN
          start_time = domain_get_start_time ( grid )
        ENDIF
        IF ( PRESENT( start_timestr ) ) THEN
          lcl_starttime = domain_get_start_time ( grid )
          CALL wrf_timetoa ( lcl_starttime, start_timestr )
        ENDIF
        IF ( PRESENT( time_step ) ) THEN
          time_step = domain_get_time_step ( grid )
        ENDIF
        IF ( PRESENT( time_stepstr ) ) THEN
          lcl_time_step = domain_get_time_step ( grid )
          CALL WRFU_TimeIntervalGet( lcl_time_step, &
                                     timeString=time_stepstr, rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
            CALL wrf_error_fatal3("",5627,&
              'domain_clock_get:  WRFU_TimeIntervalGet() failed' )
          ENDIF
        ENDIF
        IF ( PRESENT( time_stepstr_frac ) ) THEN
          lcl_time_step = domain_get_time_step ( grid )
          CALL WRFU_TimeIntervalGet( lcl_time_step, timeString=tmp_str, &
                                     Sn=Sn, Sd=Sd, rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
            CALL wrf_error_fatal3("",5636,&
              'domain_clock_get:  WRFU_TimeIntervalGet() failed' )
          ENDIF
          CALL fraction_to_string( Sn, Sd, frac_str )
          time_stepstr_frac = TRIM(tmp_str)//TRIM(frac_str)
        ENDIF
        IF ( PRESENT( advanceCount ) ) THEN
          advanceCount = domain_get_advanceCount ( grid )
        ENDIF
        
        
        
        
        
        
        IF ( PRESENT( current_time ) ) THEN
          current_time = domain_get_current_time ( grid )
        ENDIF
        IF ( PRESENT( current_timestr ) ) THEN
          lcl_currtime = domain_get_current_time ( grid )
          CALL wrf_timetoa ( lcl_currtime, current_timestr )
        ENDIF
        
        IF ( PRESENT( current_timestr_frac ) ) THEN
          lcl_currtime = domain_get_current_time ( grid )
          CALL wrf_timetoa ( lcl_currtime, tmp_str )
          CALL WRFU_TimeGet( lcl_currtime, Sn=Sn, Sd=Sd, rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
            CALL wrf_error_fatal3("",5664,&
              'domain_clock_get:  WRFU_TimeGet() failed' )
          ENDIF
          CALL fraction_to_string( Sn, Sd, frac_str )
          current_timestr_frac = TRIM(tmp_str)//TRIM(frac_str)
        ENDIF
        IF ( PRESENT( stop_time ) ) THEN
          stop_time = domain_get_stop_time ( grid )
        ENDIF
        IF ( PRESENT( stop_timestr ) ) THEN
          lcl_stoptime = domain_get_stop_time ( grid )
          CALL wrf_timetoa ( lcl_stoptime, stop_timestr )
        ENDIF
        IF ( PRESENT( currentDayOfYearReal ) ) THEN
          lcl_currtime = domain_get_current_time ( grid )
          CALL WRFU_TimeGet( lcl_currtime, dayOfYear_r8=currentDayOfYearR8, &
                             rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
            CALL wrf_error_fatal3("",5682,&
                   'domain_clock_get:  WRFU_TimeGet(dayOfYear_r8) failed' )
          ENDIF
          currentDayOfYearReal = REAL( currentDayOfYearR8 ) - 1.0
        ENDIF
        IF ( PRESENT( simulationStartTime ) ) THEN
          simulationStartTime = domain_get_sim_start_time( grid )
        ENDIF
        IF ( PRESENT( simulationStartTimeStr ) ) THEN
          lcl_simulationStartTime = domain_get_sim_start_time( grid )
          CALL wrf_timetoa ( lcl_simulationStartTime, simulationStartTimeStr )
        ENDIF
        IF ( PRESENT( timeSinceSimulationStart ) ) THEN
          timeSinceSimulationStart = domain_get_time_since_sim_start( grid )
        ENDIF
        IF ( PRESENT( minutesSinceSimulationStart ) ) THEN
          lcl_timeSinceSimulationStart = domain_get_time_since_sim_start( grid )
          CALL WRFU_TimeIntervalGet( lcl_timeSinceSimulationStart, &
                                     D=days, S=seconds, Sn=Sn, Sd=Sd, rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
            CALL wrf_error_fatal3("",5702,&
                   'domain_clock_get:  WRFU_TimeIntervalGet() failed' )
          ENDIF
          
          minutesSinceSimulationStart = ( REAL( days ) * 24. * 60. ) + &
                                        ( REAL( seconds ) / 60. )
          IF ( Sd /= 0 ) THEN
            minutesSinceSimulationStart = minutesSinceSimulationStart + &
                                          ( ( REAL( Sn ) / REAL( Sd ) ) / 60. )
          ENDIF
        ENDIF
        RETURN
      END SUBROUTINE domain_clock_get

      FUNCTION domain_clockisstarttime ( grid ) RESULT ( is_start_time ) 
        IMPLICIT NONE





        TYPE(domain), INTENT(IN) :: grid
        
        LOGICAL :: is_start_time
        TYPE(WRFU_Time) :: start_time, current_time
        CALL domain_clock_get( grid, current_time=current_time, &
                                     start_time=start_time )
        is_start_time = ( current_time == start_time )
      END FUNCTION domain_clockisstarttime

      FUNCTION domain_clockissimstarttime ( grid ) RESULT ( is_sim_start_time ) 
        IMPLICIT NONE





        TYPE(domain), INTENT(IN) :: grid
        
        LOGICAL :: is_sim_start_time
        TYPE(WRFU_Time) :: simulationStartTime, current_time
        CALL domain_clock_get( grid, current_time=current_time, &
                                     simulationStartTime=simulationStartTime )
        is_sim_start_time = ( current_time == simulationStartTime )
      END FUNCTION domain_clockissimstarttime




      SUBROUTINE domain_clock_create( grid, StartTime, &
                                            StopTime,  &
                                            TimeStep )
        IMPLICIT NONE
        TYPE(domain),            INTENT(INOUT) :: grid
        TYPE(WRFU_Time),         INTENT(IN   ) :: StartTime
        TYPE(WRFU_Time),         INTENT(IN   ) :: StopTime
        TYPE(WRFU_TimeInterval), INTENT(IN   ) :: TimeStep





        
        INTEGER :: rc
        grid%domain_clock = WRFU_ClockCreate( TimeStep= TimeStep,  &
                                              StartTime=StartTime, &
                                              StopTime= StopTime,  &
                                              rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL wrf_error_fatal3("",5771,&
            'domain_clock_create:  WRFU_ClockCreate() failed' )
        ENDIF
        grid%domain_clock_created = .TRUE.
        RETURN
      END SUBROUTINE domain_clock_create



      SUBROUTINE domain_alarm_create( grid, alarm_id, interval, &
                                            begin_time, end_time )
        USE module_utility
        IMPLICIT NONE
        TYPE(domain), POINTER :: grid
        INTEGER, INTENT(IN) :: alarm_id
        TYPE(WRFU_TimeInterval), INTENT(IN), OPTIONAL :: interval
        TYPE(WRFU_TimeInterval), INTENT(IN), OPTIONAL :: begin_time
        TYPE(WRFU_TimeInterval), INTENT(IN), OPTIONAL :: end_time





        
        INTEGER :: rc




        LOGICAL :: interval_only, all_args, no_args
        TYPE(WRFU_Time) :: startTime
        interval_only = .FALSE.
        all_args = .FALSE.
        no_args = .FALSE.
        IF ( ( .NOT. PRESENT( begin_time ) ) .AND. &
             ( .NOT. PRESENT( end_time   ) ) .AND. &
             (       PRESENT( interval   ) ) ) THEN
           interval_only = .TRUE.
        ELSE IF ( ( .NOT. PRESENT( begin_time ) ) .AND. &
                  ( .NOT. PRESENT( end_time   ) ) .AND. &
                  ( .NOT. PRESENT( interval   ) ) ) THEN
           no_args = .TRUE.
        ELSE IF ( (       PRESENT( begin_time ) ) .AND. &
                  (       PRESENT( end_time   ) ) .AND. &
                  (       PRESENT( interval   ) ) ) THEN
           all_args = .TRUE.
        ELSE
           CALL wrf_error_fatal3("",5818,&
             'ERROR in domain_alarm_create:  bad argument list' )
        ENDIF
        CALL domain_clock_get( grid, start_time=startTime )
        IF ( interval_only ) THEN
           grid%io_intervals( alarm_id ) = interval
           grid%alarms( alarm_id ) = &
             WRFU_AlarmCreate( clock=grid%domain_clock, &
                               RingInterval=interval,   &
                               rc=rc )
        ELSE IF ( no_args ) THEN
           grid%alarms( alarm_id ) = &
             WRFU_AlarmCreate( clock=grid%domain_clock, &
                               RingTime=startTime,      &
                               rc=rc )
        ELSE IF ( all_args ) THEN
           grid%io_intervals( alarm_id ) = interval
           grid%alarms( alarm_id ) = &
             WRFU_AlarmCreate( clock=grid%domain_clock,         &
                               RingTime=startTime + begin_time, &
                               RingInterval=interval,           &
                               StopTime=startTime + end_time,   &
                               rc=rc )
        ENDIF
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL wrf_error_fatal3("",5843,&
            'domain_alarm_create:  WRFU_AlarmCreate() failed' )
        ENDIF
        CALL WRFU_AlarmRingerOff( grid%alarms( alarm_id ) , rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL wrf_error_fatal3("",5848,&
            'domain_alarm_create:  WRFU_AlarmRingerOff() failed' )
        ENDIF
        grid%alarms_created( alarm_id ) = .TRUE.
      END SUBROUTINE domain_alarm_create



      SUBROUTINE domain_clock_set( grid, current_timestr, &
                                         stop_timestr,    &
                                         time_step_seconds )
        IMPLICIT NONE
        TYPE(domain),      INTENT(INOUT)           :: grid
        CHARACTER (LEN=*), INTENT(IN   ), OPTIONAL :: current_timestr
        CHARACTER (LEN=*), INTENT(IN   ), OPTIONAL :: stop_timestr
        INTEGER,           INTENT(IN   ), OPTIONAL :: time_step_seconds






        
        TYPE(WRFU_Time) :: lcl_currtime, lcl_stoptime
        TYPE(WRFU_TimeInterval) :: tmpTimeInterval
        INTEGER :: rc
        IF ( PRESENT( current_timestr ) ) THEN
          CALL wrf_atotime( current_timestr(1:19), lcl_currtime )
          CALL WRFU_ClockSet( grid%domain_clock, currTime=lcl_currtime, &
                              rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
            CALL wrf_error_fatal3("",5879,&
              'domain_clock_set:  WRFU_ClockSet(CurrTime) failed' )
          ENDIF
        ENDIF
        IF ( PRESENT( stop_timestr ) ) THEN
          CALL wrf_atotime( stop_timestr(1:19), lcl_stoptime )
          CALL WRFU_ClockSet( grid%domain_clock, stopTime=lcl_stoptime, &
                              rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
            CALL wrf_error_fatal3("",5888,&
              'domain_clock_set:  WRFU_ClockSet(StopTime) failed' )
          ENDIF
        ENDIF
        IF ( PRESENT( time_step_seconds ) ) THEN
          CALL WRFU_TimeIntervalSet( tmpTimeInterval, &
                                     S=time_step_seconds, rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
            CALL wrf_error_fatal3("",5896,&
              'domain_clock_set:  WRFU_TimeIntervalSet failed' )
          ENDIF
          CALL WRFU_ClockSet ( grid%domain_clock,        &
                               timeStep=tmpTimeInterval, &
                               rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
            CALL wrf_error_fatal3("",5903,&
              'domain_clock_set:  WRFU_ClockSet(TimeStep) failed' )
          ENDIF
        ENDIF
        RETURN
      END SUBROUTINE domain_clock_set


      
      
      SUBROUTINE domain_clockprint ( level, grid, pre_str )
        IMPLICIT NONE
        INTEGER,           INTENT( IN) :: level
        TYPE(domain),      INTENT( IN) :: grid
        CHARACTER (LEN=*), INTENT( IN) :: pre_str
        CALL wrf_clockprint ( level, grid%domain_clock, pre_str )
        RETURN
      END SUBROUTINE domain_clockprint


      
      
      SUBROUTINE domain_clockadvance ( grid )
        IMPLICIT NONE
        TYPE(domain), INTENT(INOUT) :: grid
        INTEGER :: rc
        CALL domain_clockprint ( 250, grid, &
          'DEBUG domain_clockadvance():  before WRFU_ClockAdvance,' )
        CALL WRFU_ClockAdvance( grid%domain_clock, rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL wrf_error_fatal3("",5933,&
            'domain_clockadvance:  WRFU_ClockAdvance() failed' )
        ENDIF
        CALL domain_clockprint ( 250, grid, &
          'DEBUG domain_clockadvance():  after WRFU_ClockAdvance,' )
        
        
        CALL domain_clock_get( grid, minutesSinceSimulationStart=grid%xtime )
        CALL domain_clock_get( grid, currentDayOfYearReal=grid%julian )
        RETURN
      END SUBROUTINE domain_clockadvance



      
      
      SUBROUTINE domain_setgmtetc ( grid, start_of_simulation )
        IMPLICIT NONE
        TYPE (domain), INTENT(INOUT) :: grid
        LOGICAL,       INTENT(  OUT) :: start_of_simulation
        
        CHARACTER (LEN=132)          :: message
        TYPE(WRFU_Time)              :: simStartTime
        INTEGER                      :: hr, mn, sec, ms, rc
        CALL domain_clockprint(150, grid, &
          'DEBUG domain_setgmtetc():  get simStartTime from clock,')
        CALL domain_clock_get( grid, simulationStartTime=simStartTime, &
                                     simulationStartTimeStr=message )
        CALL WRFU_TimeGet( simStartTime, YY=grid%julyr, dayOfYear=grid%julday, &
                           H=hr, M=mn, S=sec, MS=ms, rc=rc)
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL wrf_error_fatal3("",5964,&
            'domain_setgmtetc:  WRFU_TimeGet() failed' )
        ENDIF
        WRITE( wrf_err_message , * ) 'DEBUG domain_setgmtetc():  simulation start time = [',TRIM( message ),']'
        CALL wrf_debug( 150, TRIM(wrf_err_message) )
        grid%gmt=hr+real(mn)/60.+real(sec)/3600.+real(ms)/(1000*3600)
        WRITE( wrf_err_message , * ) 'DEBUG domain_setgmtetc():  julyr,hr,mn,sec,ms,julday = ', &
                                     grid%julyr,hr,mn,sec,ms,grid%julday
        CALL wrf_debug( 150, TRIM(wrf_err_message) )
        WRITE( wrf_err_message , * ) 'DEBUG domain_setgmtetc():  gmt = ',grid%gmt
        CALL wrf_debug( 150, TRIM(wrf_err_message) )
        start_of_simulation = domain_ClockIsSimStartTime(grid)
        RETURN
      END SUBROUTINE domain_setgmtetc
     


      
      
      SUBROUTINE set_current_grid_ptr( grid_ptr )
        IMPLICIT NONE
        TYPE(domain), POINTER :: grid_ptr






        current_grid_set = .TRUE.
        current_grid => grid_ptr

      END SUBROUTINE set_current_grid_ptr







      
      SUBROUTINE domain_time_test_print ( pre_str, name_str, res_str )
        IMPLICIT NONE
        CHARACTER (LEN=*), INTENT(IN) :: pre_str
        CHARACTER (LEN=*), INTENT(IN) :: name_str
        CHARACTER (LEN=*), INTENT(IN) :: res_str
        CHARACTER (LEN=512) :: out_str
        WRITE (out_str,                                            &
          FMT="('DOMAIN_TIME_TEST ',A,':  ',A,' = ',A)") &
          TRIM(pre_str), TRIM(name_str), TRIM(res_str)
        CALL wrf_debug( 0, TRIM(out_str) )
      END SUBROUTINE domain_time_test_print

      
      SUBROUTINE test_adjust_io_timestr( TI_h, TI_m, TI_s, &
        CT_yy,  CT_mm,  CT_dd,  CT_h,  CT_m,  CT_s,        &
        ST_yy,  ST_mm,  ST_dd,  ST_h,  ST_m,  ST_s,        &
        res_str, testname )
        INTEGER, INTENT(IN) :: TI_H
        INTEGER, INTENT(IN) :: TI_M
        INTEGER, INTENT(IN) :: TI_S
        INTEGER, INTENT(IN) :: CT_YY
        INTEGER, INTENT(IN) :: CT_MM  
        INTEGER, INTENT(IN) :: CT_DD  
        INTEGER, INTENT(IN) :: CT_H
        INTEGER, INTENT(IN) :: CT_M
        INTEGER, INTENT(IN) :: CT_S
        INTEGER, INTENT(IN) :: ST_YY
        INTEGER, INTENT(IN) :: ST_MM  
        INTEGER, INTENT(IN) :: ST_DD  
        INTEGER, INTENT(IN) :: ST_H
        INTEGER, INTENT(IN) :: ST_M
        INTEGER, INTENT(IN) :: ST_S
        CHARACTER (LEN=*), INTENT(IN) :: res_str
        CHARACTER (LEN=*), INTENT(IN) :: testname
        
        TYPE(WRFU_TimeInterval) :: TI
        TYPE(WRFU_Time) :: CT, ST
        LOGICAL :: test_passed
        INTEGER :: rc
        CHARACTER(LEN=WRFU_MAXSTR) :: TI_str, CT_str, ST_str, computed_str
        
        CALL WRFU_TimeIntervalSet( TI, H=TI_H, M=TI_M, S=TI_S, rc=rc )
        CALL wrf_check_error( WRFU_SUCCESS, rc, &
                              'FAIL:  '//TRIM(testname)//'WRFU_TimeIntervalSet() ', &
                              "module_domain.F" , &
                              1985  )
        CALL WRFU_TimeIntervalGet( TI, timeString=TI_str, rc=rc )
        CALL wrf_check_error( WRFU_SUCCESS, rc, &
                              'FAIL:  '//TRIM(testname)//'WRFU_TimeGet() ', &
                              "module_domain.F" , &
                              1990  )
        
        CALL WRFU_TimeSet( CT, YY=CT_YY, MM=CT_MM, DD=CT_DD , &
                                H=CT_H,   M=CT_M,   S=CT_S, rc=rc )
        CALL wrf_check_error( WRFU_SUCCESS, rc, &
                              'FAIL:  '//TRIM(testname)//'WRFU_TimeSet() ', &
                              "module_domain.F" , &
                              1997  )
        CALL WRFU_TimeGet( CT, timeString=CT_str, rc=rc )
        CALL wrf_check_error( WRFU_SUCCESS, rc, &
                              'FAIL:  '//TRIM(testname)//'WRFU_TimeGet() ', &
                              "module_domain.F" , &
                              2002  )
        
        CALL WRFU_TimeSet( ST, YY=ST_YY, MM=ST_MM, DD=ST_DD , &
                                H=ST_H,   M=ST_M,   S=ST_S, rc=rc )
        CALL wrf_check_error( WRFU_SUCCESS, rc, &
                              'FAIL:  '//TRIM(testname)//'WRFU_TimeSet() ', &
                              "module_domain.F" , &
                              2009  )
        CALL WRFU_TimeGet( ST, timeString=ST_str, rc=rc )
        CALL wrf_check_error( WRFU_SUCCESS, rc, &
                              'FAIL:  '//TRIM(testname)//'WRFU_TimeGet() ', &
                              "module_domain.F" , &
                              2014  )
        
        CALL adjust_io_timestr ( TI, CT, ST, computed_str )
        
        test_passed = .FALSE.
        IF ( LEN_TRIM(res_str) == LEN_TRIM(computed_str) ) THEN
          IF ( res_str(1:LEN_TRIM(res_str)) == computed_str(1:LEN_TRIM(computed_str)) ) THEN
            test_passed = .TRUE.
          ENDIF
        ENDIF
        
        IF ( test_passed ) THEN
          WRITE(*,FMT='(A)') 'PASS:  '//TRIM(testname)
        ELSE
          WRITE(*,*) 'FAIL:  ',TRIM(testname),':  adjust_io_timestr(',    &
            TRIM(TI_str),',',TRIM(CT_str),',',TRIM(ST_str),')  expected <', &
            TRIM(res_str),'>  but computed <',TRIM(computed_str),'>'
        ENDIF
      END SUBROUTINE test_adjust_io_timestr

      
      
      
      
      
      SUBROUTINE domain_time_test ( grid, pre_str )
        IMPLICIT NONE
        TYPE(domain),      INTENT(IN) :: grid
        CHARACTER (LEN=*), INTENT(IN) :: pre_str
        
        LOGICAL, SAVE :: one_time_tests_done = .FALSE.
        REAL :: minutesSinceSimulationStart
        INTEGER :: advance_count, rc
        REAL :: currentDayOfYearReal
        TYPE(WRFU_TimeInterval) :: timeSinceSimulationStart
        TYPE(WRFU_Time) :: simulationStartTime
        CHARACTER (LEN=512) :: res_str
        LOGICAL :: self_test_domain
        
        
        
        
        
        
        CALL nl_get_self_test_domain( 1, self_test_domain )
        IF ( self_test_domain ) THEN
          CALL domain_clock_get( grid, advanceCount=advance_count )
          WRITE ( res_str, FMT="(I8.8)" ) advance_count
          CALL domain_time_test_print( pre_str, 'advanceCount', res_str )
          CALL domain_clock_get( grid, currentDayOfYearReal=currentDayOfYearReal )
          WRITE ( res_str, FMT='(F10.6)' ) currentDayOfYearReal
          CALL domain_time_test_print( pre_str, 'currentDayOfYearReal', res_str )
          CALL domain_clock_get( grid, minutesSinceSimulationStart=minutesSinceSimulationStart )
          WRITE ( res_str, FMT='(F10.6)' ) minutesSinceSimulationStart
          CALL domain_time_test_print( pre_str, 'minutesSinceSimulationStart', res_str )
          CALL domain_clock_get( grid, current_timestr=res_str )
          CALL domain_time_test_print( pre_str, 'current_timestr', res_str )
          CALL domain_clock_get( grid, current_timestr_frac=res_str )
          CALL domain_time_test_print( pre_str, 'current_timestr_frac', res_str )
          CALL domain_clock_get( grid, timeSinceSimulationStart=timeSinceSimulationStart )
          CALL WRFU_TimeIntervalGet( timeSinceSimulationStart, timeString=res_str, rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
            CALL wrf_error_fatal3("",6140,&
              'domain_time_test:  WRFU_TimeIntervalGet() failed' )
          ENDIF
          CALL domain_time_test_print( pre_str, 'timeSinceSimulationStart', res_str )
          
          
          IF ( .NOT. one_time_tests_done ) THEN
            one_time_tests_done = .TRUE.
            CALL domain_clock_get( grid, simulationStartTimeStr=res_str )
            CALL domain_time_test_print( pre_str, 'simulationStartTime', res_str )
            CALL domain_clock_get( grid, start_timestr=res_str )
            CALL domain_time_test_print( pre_str, 'start_timestr', res_str )
            CALL domain_clock_get( grid, stop_timestr=res_str )
            CALL domain_time_test_print( pre_str, 'stop_timestr', res_str )
            CALL domain_clock_get( grid, time_stepstr=res_str )
            CALL domain_time_test_print( pre_str, 'time_stepstr', res_str )
            CALL domain_clock_get( grid, time_stepstr_frac=res_str )
            CALL domain_time_test_print( pre_str, 'time_stepstr_frac', res_str )
            
            
            
            
            
            
            CALL test_adjust_io_timestr( TI_h=3, TI_m=0, TI_s=0,          &
              CT_yy=2000,  CT_mm=1,  CT_dd=26,  CT_h=0,  CT_m=0,  CT_s=0, &
              ST_yy=2000,  ST_mm=1,  ST_dd=24,  ST_h=12, ST_m=0,  ST_s=0, &
              res_str='2000-01-26_00:00:00', testname='adjust_io_timestr_1' )
            
            
            
            
            
          ENDIF
        ENDIF
        RETURN
      END SUBROUTINE domain_time_test






END MODULE module_domain









SUBROUTINE get_current_time_string( time_str )
  USE module_domain
  IMPLICIT NONE
  CHARACTER (LEN=*), INTENT(OUT) :: time_str
  
  INTEGER :: debug_level_lcl

  time_str = ''
  IF ( current_grid_set ) THEN








    IF ( current_grid%time_set ) THEN

      
      CALL get_wrf_debug_level( debug_level_lcl )
      CALL set_wrf_debug_level ( 0 )
      current_grid_set = .FALSE.
      CALL domain_clock_get( current_grid, current_timestr_frac=time_str )
      
      CALL set_wrf_debug_level ( debug_level_lcl )
      current_grid_set = .TRUE.

    ENDIF
  ENDIF

END SUBROUTINE get_current_time_string






SUBROUTINE get_current_grid_name( grid_str )
  USE module_domain
  IMPLICIT NONE
  CHARACTER (LEN=*), INTENT(OUT) :: grid_str
  grid_str = ''
  IF ( current_grid_set ) THEN
    WRITE(grid_str,FMT="('d',I2.2)") current_grid%id
  ENDIF
END SUBROUTINE get_current_grid_name



