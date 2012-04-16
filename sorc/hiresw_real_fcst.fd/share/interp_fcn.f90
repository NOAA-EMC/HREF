






   SUBROUTINE interp_fcn ( cfld,                                 &  
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  
                           imask,                                &  
                           xstag, ystag,                         &  
                           ipos, jpos,                           &  
                           nri, nrj                             )   
     USE module_timing
     USE module_configure
     IMPLICIT NONE


     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask

     



     INTEGER ci, cj, ck, ni, nj, nk, ip, jp, ioff, joff, nioff, njoff
     INTEGER nfx, ior
     PARAMETER (ior=2)
     INTEGER nf
     REAL psca(cims:cime,cjms:cjme,nri*nrj)
     LOGICAL icmask( cims:cime, cjms:cjme )
     INTEGER i,j,k
     INTEGER nrio2, nrjo2

     
     


     ioff  = 0 ; joff  = 0
     nioff = 0 ; njoff = 0
     IF ( xstag ) THEN 
       ioff = (nri-1)/2
       nioff = nri 
     ENDIF
     IF ( ystag ) THEN
       joff = (nrj-1)/2
       njoff = nrj
     ENDIF

     nrio2 = nri/2
     nrjo2 = nrj/2

     nfx = nri * nrj
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( i,j,k,ni,nj,ci,cj,ip,jp,nk,ck,nf,icmask,psca )
     DO k = ckts, ckte
        icmask = .FALSE.
        DO nf = 1,nfx
           DO j = cjms,cjme
              nj = (j-jpos) * nrj + ( nrjo2 + 1 )  
              DO i = cims,cime
                ni = (i-ipos) * nri + ( nrio2 + 1 )    
                if ( ni .ge. nits-nioff-nrio2 .and. ni .le. nite+nioff+nrio2 .and. nj .ge. njts-njoff-nrjo2 .and. nj .le. njte+njoff+nrjo2 ) then



                  if ( imask(ni,nj) .eq. 1 ) then
                    icmask( i, j ) = .TRUE.
                  endif
                  if (ni .ge. nits-nioff .and. nj .ge. njts-njoff ) then
                    if ( imask(ni-nioff,nj-njoff) .eq. 1) then
                        icmask( i, j ) = .TRUE.
                    endif
                  endif
                endif
                psca(i,j,nf) = cfld(i,k,j)
              ENDDO
           ENDDO
        ENDDO








        CALL sint( psca,                     &
                   cims, cime, cjms, cjme, icmask,   &
                   cits-1, cite+1, cjts-1, cjte+1, nrj*nri, xstag, ystag )



        DO nj = njts, njte+joff
           cj = jpos + (nj-1) / nrj 
           jp = mod ( nj-1 , nrj )  
           nk = k
           ck = nk
           DO ni = nits, nite+ioff
               ci = ipos + (ni-1) / nri      
               ip = mod ( ni-1 , nri )  
               if ( imask ( ni, nj ) .eq. 1 .or. imask ( ni-ioff, nj-joff ) .eq. 1  ) then
                 nfld( ni-ioff, nk, nj-joff ) = psca( ci , cj, ip+1 + (jp)*nri )
               endif
           ENDDO
        ENDDO
     ENDDO
   !$OMP END PARALLEL DO


     RETURN

   END SUBROUTINE interp_fcn




   SUBROUTINE copy_fcn ( cfld,                                 &  
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  
                           imask,                                &  
                           xstag, ystag,                         &  
                           ipos, jpos,                           &  
                           nri, nrj                             )   
     USE module_configure
     IMPLICIT NONE


     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ), INTENT(OUT) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ),INTENT(IN)  :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ),INTENT(IN)  :: imask

     

     INTEGER ci, cj, ck, ni, nj, nk, ip, jp, ioff, joff, ioffa, joffa
     INTEGER :: icmin,icmax,jcmin,jcmax
     INTEGER :: istag,jstag, ipoints,jpoints,ijpoints
     INTEGER , PARAMETER :: passes = 2
     INTEGER spec_zone

     
     
     
     
     

     
     
     

     
     
 
     
     
     

     
     
     

     CALL nl_get_spec_zone( 1 , spec_zone )
     istag = 1 ; jstag = 1
     IF ( xstag ) istag = 0
     IF ( ystag ) jstag = 0

     IF( MOD(nrj,2) .NE. 0) THEN  

        IF      ( ( .NOT. xstag ) .AND. ( .NOT. ystag ) ) THEN
           DO cj = MAX(jpos+spec_zone,cjts),MIN(jpos+(njde-njds)/nrj-jstag-spec_zone,cjte)
              nj = (cj-jpos)*nrj + jstag + 1
              DO ck = ckts, ckte
                 nk = ck
                 DO ci = MAX(ipos+spec_zone,cits),MIN(ipos+(nide-nids)/nri-istag-spec_zone,cite)
                    ni = (ci-ipos)*nri + istag + 1
                    cfld( ci, ck, cj ) = 0.
                    DO ijpoints = 1 , nri * nrj
                       ipoints = MOD((ijpoints-1),nri) + 1 - nri/2 - 1
                       jpoints = (ijpoints-1)/nri + 1 - nrj/2 - 1
                       cfld( ci, ck, cj ) =  cfld( ci, ck, cj ) + &
                                             1./REAL(nri*nrj) * nfld( ni+ipoints , nk , nj+jpoints )
                    END DO










                 ENDDO
              ENDDO
           ENDDO

        ELSE IF ( (       xstag ) .AND. ( .NOT. ystag ) ) THEN
           DO cj = MAX(jpos+spec_zone,cjts),MIN(jpos+(njde-njds)/nrj-jstag-spec_zone,cjte)
              nj = (cj-jpos)*nrj + jstag + 1
              DO ck = ckts, ckte
                 nk = ck
                 DO ci = MAX(ipos+spec_zone,cits),MIN(ipos+(nide-nids)/nri-istag-spec_zone,cite)
                    ni = (ci-ipos)*nri + istag + 1
                    cfld( ci, ck, cj ) = 0.
                    DO ijpoints = (nri+1)/2 , (nri+1)/2 + nri*(nri-1) , nri
                       ipoints = MOD((ijpoints-1),nri) + 1 - nri/2 - 1
                       jpoints = (ijpoints-1)/nri + 1 - nrj/2 - 1
                       cfld( ci, ck, cj ) =  cfld( ci, ck, cj ) + &
                                             1./REAL(nri    ) * nfld( ni+ipoints , nk , nj+jpoints )
                    END DO




                 ENDDO
              ENDDO
           ENDDO

        ELSE IF ( ( .NOT. xstag ) .AND. (       ystag ) ) THEN
           DO cj = MAX(jpos+spec_zone,cjts),MIN(jpos+(njde-njds)/nrj-jstag-spec_zone,cjte)
              nj = (cj-jpos)*nrj + jstag + 1
              DO ck = ckts, ckte
                 nk = ck
                 DO ci = MAX(ipos+spec_zone,cits),MIN(ipos+(nide-nids)/nri-istag-spec_zone,cite)
                    ni = (ci-ipos)*nri + istag + 1
                    cfld( ci, ck, cj ) = 0.
                    DO ijpoints = ( nrj*nrj +1 )/2 - nrj/2 , ( nrj*nrj +1 )/2 - nrj/2 + nrj-1
                       ipoints = MOD((ijpoints-1),nri) + 1 - nri/2 - 1
                       jpoints = (ijpoints-1)/nri + 1 - nrj/2 - 1
                       cfld( ci, ck, cj ) =  cfld( ci, ck, cj ) + &
                                             1./REAL(    nrj) * nfld( ni+ipoints , nk , nj+jpoints )
                    END DO




                 ENDDO
              ENDDO
           ENDDO

        END IF

     

     ELSE IF ( MOD(nrj,2) .EQ. 0) THEN
        IF ( ( .NOT. xstag ) .AND. ( .NOT. ystag ) ) THEN

        
        
        
        
        
        
   
        
        
        
        
   
        
        
        
        












































           
           
           
           
   
           
           
           
           
           
           
           
           
           DO cj = MAX(jpos+spec_zone,cjts),MIN(jpos+(njde-njds)/nrj-jstag-spec_zone,cjte)
              nj = (cj-jpos)*nrj + jstag
              DO ck = ckts, ckte
                 nk = ck
                 DO ci = MAX(ipos+spec_zone,cits),MIN(ipos+(nide-nids)/nri-istag-spec_zone,cite)
                    ni = (ci-ipos)*nri + istag
                    cfld( ci, ck, cj ) = 0.
                    DO ijpoints = 1 , nri * nrj
                       ipoints = MOD((ijpoints-1),nri)
                       jpoints = (ijpoints-1)/nri
                       cfld( ci, ck, cj ) =  cfld( ci, ck, cj ) + &
                                             1./REAL(nri*nrj) * nfld( ni+ipoints , nk , nj+jpoints )
                    END DO





                 END DO
              END DO
           END DO

        

        ELSE IF ( (       xstag ) .AND. ( .NOT. ystag ) ) THEN



























           DO cj = MAX(jpos+spec_zone,cjts),MIN(jpos+(njde-njds)/nrj-jstag-spec_zone,cjte)
              nj = (cj-jpos)*nrj + 1
              DO ck = ckts, ckte
                 nk = ck
                 DO ci = MAX(ipos+spec_zone,cits),MIN(ipos+(nide-nids)/nri-istag-spec_zone,cite)
                    ni = (ci-ipos)*nri + 1
                    cfld( ci, ck, cj ) = 0.
                    DO ijpoints = 1 , nri*nrj , nri
                       ipoints = MOD((ijpoints-1),nri)
                       jpoints = (ijpoints-1)/nri
                       cfld( ci, ck, cj ) =  cfld( ci, ck, cj ) + &
                                             1./REAL(nri    ) * nfld( ni+ipoints , nk , nj+jpoints )
                    END DO



                 ENDDO
              ENDDO
           ENDDO

        

        ELSE IF ( ( .NOT. xstag ) .AND. (       ystag ) ) THEN
           DO cj = MAX(jpos+spec_zone,cjts),MIN(jpos+(njde-njds)/nrj-jstag-spec_zone,cjte)
              nj = (cj-jpos)*nrj + 1
              DO ck = ckts, ckte
                 nk = ck
                 DO ci = MAX(ipos+spec_zone,cits),MIN(ipos+(nide-nids)/nri-istag-spec_zone,cite)
                    ni = (ci-ipos)*nri + 1
                    cfld( ci, ck, cj ) = 0.
                    DO ijpoints = 1 , nri
                       ipoints = MOD((ijpoints-1),nri)
                       jpoints = (ijpoints-1)/nri
                       cfld( ci, ck, cj ) =  cfld( ci, ck, cj ) + &
                                             1./REAL(nri    ) * nfld( ni+ipoints , nk , nj+jpoints )
                    END DO



                 ENDDO
              ENDDO
           ENDDO
        END IF
     END IF

     RETURN

   END SUBROUTINE copy_fcn




   SUBROUTINE copy_fcnm (  cfld,                                 &  
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  
                           imask,                                &  
                           xstag, ystag,                         &  
                           ipos, jpos,                           &  
                           nri, nrj                             )   
     USE module_configure
     USE module_wrf_error
     IMPLICIT NONE


     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ), INTENT(OUT) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ), INTENT(IN) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: imask

     

     INTEGER ci, cj, ck, ni, nj, nk, ip, jp, ioff, joff, ioffa, joffa
     INTEGER :: icmin,icmax,jcmin,jcmax
     INTEGER :: istag,jstag, ipoints,jpoints,ijpoints
     INTEGER , PARAMETER :: passes = 2
     INTEGER spec_zone

     CALL nl_get_spec_zone( 1, spec_zone ) 
     istag = 1 ; jstag = 1
     IF ( xstag ) istag = 0
     IF ( ystag ) jstag = 0

     IF( MOD(nrj,2) .NE. 0) THEN  

        DO cj = MAX(jpos+spec_zone,cjts),MIN(jpos+(njde-njds)/nrj-jstag-spec_zone,cjte)
           nj = (cj-jpos)*nrj + jstag + 1
           DO ck = ckts, ckte
              nk = ck
              DO ci = MAX(ipos+spec_zone,cits),MIN(ipos+(nide-nids)/nri-istag-spec_zone,cite)
                 ni = (ci-ipos)*nri + istag + 1
                 cfld( ci, ck, cj ) =  nfld( ni  , nk , nj  )
              ENDDO
           ENDDO
        ENDDO

     ELSE  
        DO cj = MAX(jpos+spec_zone,cjts),MIN(jpos+(njde-njds)/nrj-jstag-spec_zone,cjte)
           nj = (cj-jpos)*nrj + 1
           DO ck = ckts, ckte
              nk = ck
              DO ci = MAX(ipos+spec_zone,cits),MIN(ipos+(nide-nids)/nri-istag-spec_zone,cite)
                 ni = (ci-ipos)*nri + 1
                 ipoints = nri/2 -1
                 jpoints = nrj/2 -1
                 cfld( ci, ck, cj ) =  nfld( ni+ipoints , nk , nj+jpoints )
              END DO
           END DO
        END DO

     END IF

     RETURN

   END SUBROUTINE copy_fcnm




   SUBROUTINE copy_fcni ( cfld,                                 &  
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  
                           imask,                                &  
                           xstag, ystag,                         &  
                           ipos, jpos,                           &  
                           nri, nrj                             )   
     USE module_configure
     USE module_wrf_error
     IMPLICIT NONE


     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     INTEGER, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ), INTENT(OUT) :: cfld
     INTEGER, DIMENSION ( nims:nime, nkms:nkme, njms:njme ), INTENT(IN) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ), INTENT(IN)  :: imask

     

     INTEGER ci, cj, ck, ni, nj, nk, ip, jp, ioff, joff, ioffa, joffa
     INTEGER :: icmin,icmax,jcmin,jcmax
     INTEGER :: istag,jstag, ipoints,jpoints,ijpoints
     INTEGER , PARAMETER :: passes = 2
     INTEGER spec_zone

     CALL nl_get_spec_zone( 1, spec_zone ) 
     istag = 1 ; jstag = 1
     IF ( xstag ) istag = 0
     IF ( ystag ) jstag = 0

     IF( MOD(nrj,2) .NE. 0) THEN  

        DO cj = MAX(jpos+spec_zone,cjts),MIN(jpos+(njde-njds)/nrj-jstag-spec_zone,cjte)
           nj = (cj-jpos)*nrj + jstag + 1
           DO ck = ckts, ckte
              nk = ck
              DO ci = MAX(ipos+spec_zone,cits),MIN(ipos+(nide-nids)/nri-istag-spec_zone,cite)
                 ni = (ci-ipos)*nri + istag + 1
                 cfld( ci, ck, cj ) =  nfld( ni  , nk , nj  )
              ENDDO
           ENDDO
        ENDDO

     ELSE  
        DO cj = MAX(jpos+spec_zone,cjts),MIN(jpos+(njde-njds)/nrj-jstag-spec_zone,cjte)
           nj = (cj-jpos)*nrj + 1
           DO ck = ckts, ckte
              nk = ck
              DO ci = MAX(ipos+spec_zone,cits),MIN(ipos+(nide-nids)/nri-istag-spec_zone,cite)
                 ni = (ci-ipos)*nri + 1
                 ipoints = nri/2 -1
                 jpoints = nrj/2 -1
                 cfld( ci, ck, cj ) =  nfld( ni+ipoints , nk , nj+jpoints )
              END DO
           END DO
        END DO

     END IF

     RETURN

   END SUBROUTINE copy_fcni



   SUBROUTINE bdy_interp ( cfld,                                 &  
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  
                           imask,                                &  
                           xstag, ystag,                         &  
                           ipos, jpos,                           &  
                           nri, nrj,                             &  
                           cbdy_xs, nbdy_xs,                           &
                           cbdy_xe, nbdy_xe,                           &
                           cbdy_ys, nbdy_ys,                           &
                           cbdy_ye, nbdy_ye,                           &
                           cbdy_txs, nbdy_txs,                       &
                           cbdy_txe, nbdy_txe,                       &
                           cbdy_tys, nbdy_tys,                       &
                           cbdy_tye, nbdy_tye,                       &
                           cdt, ndt                              &
                           )   
     USE module_configure
     IMPLICIT NONE

     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj

     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask
     REAL,  DIMENSION( * ), INTENT(INOUT) :: cbdy_xs, cbdy_txs, nbdy_xs, nbdy_txs
     REAL,  DIMENSION( * ), INTENT(INOUT) :: cbdy_xe, cbdy_txe, nbdy_xe, nbdy_txe
     REAL,  DIMENSION( * ), INTENT(INOUT) :: cbdy_ys, cbdy_tys, nbdy_ys, nbdy_tys
     REAL,  DIMENSION( * ), INTENT(INOUT) :: cbdy_ye, cbdy_tye, nbdy_ye, nbdy_tye
     REAL cdt, ndt

     

     INTEGER nijds, nijde, spec_bdy_width

     nijds = min(nids, njds)
     nijde = max(nide, njde)
     CALL nl_get_spec_bdy_width( 1, spec_bdy_width )

     CALL bdy_interp1( cfld,                                 &  
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  
                           nijds, nijde , spec_bdy_width ,       &  
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw, imask,                           &
                           xstag, ystag,                         &  
                           ipos, jpos,                           &  
                           nri, nrj,                             &
                           cbdy_xs, nbdy_xs,                           &
                           cbdy_xe, nbdy_xe,                           &
                           cbdy_ys, nbdy_ys,                           &
                           cbdy_ye, nbdy_ye,                           &
                           cbdy_txs, nbdy_txs,                       &
                           cbdy_txe, nbdy_txe,                       &
                           cbdy_tys, nbdy_tys,                       &
                           cbdy_tye, nbdy_tye,                       &
                           cdt, ndt                              &
                                        )

     RETURN

   END SUBROUTINE bdy_interp

   SUBROUTINE bdy_interp1( cfld,                                 &  
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  
                           nijds, nijde, spec_bdy_width ,          &
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw1,                                 &
                           imask,                                &  
                           xstag, ystag,                         &  
                           ipos, jpos,                           &  
                           nri, nrj,                             &
                           cbdy_xs, bdy_xs,                           &
                           cbdy_xe, bdy_xe,                           &
                           cbdy_ys, bdy_ys,                           &
                           cbdy_ye, bdy_ye,                           &
                           cbdy_txs, bdy_txs,                       &
                           cbdy_txe, bdy_txe,                       &
                           cbdy_tys, bdy_tys,                       &
                           cbdy_tye, bdy_tye,                       &
                           cdt, ndt                              &
                                        )

     USE module_configure
     use module_state_description
     IMPLICIT NONE

     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw1,                                 &  
                            ipos, jpos,                           &
                            nri, nrj
     INTEGER, INTENT(IN) :: nijds, nijde, spec_bdy_width
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ), INTENT(INOUT) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ), INTENT(INOUT) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask
     REAL, DIMENSION ( * ), INTENT(INOUT) :: cbdy_xs, cbdy_txs   
     REAL, DIMENSION ( * ), INTENT(INOUT) :: cbdy_xe, cbdy_txe   
     REAL, DIMENSION ( * ), INTENT(INOUT) :: cbdy_ys, cbdy_tys   
     REAL, DIMENSION ( * ), INTENT(INOUT) :: cbdy_ye, cbdy_tye   
     REAL                                 :: cdt, ndt
     REAL, DIMENSION ( njms:njme, nkms:nkme, spec_bdy_width ), INTENT(INOUT) :: bdy_xs, bdy_txs
     REAL, DIMENSION ( njms:njme, nkms:nkme, spec_bdy_width ), INTENT(INOUT) :: bdy_xe, bdy_txe
     REAL, DIMENSION ( nims:nime, nkms:nkme, spec_bdy_width ), INTENT(INOUT) :: bdy_ys, bdy_tys
     REAL, DIMENSION ( nims:nime, nkms:nkme, spec_bdy_width ), INTENT(INOUT) :: bdy_ye, bdy_tye

     

     REAL*8 rdt
     INTEGER ci, cj, ck, ni, nj, nk, ni1, nj1, nk1, ip, jp, ioff, joff
     INTEGER nfx, ior
     PARAMETER (ior=2)
     INTEGER nf
     REAL psca1(cims:cime,cjms:cjme,nri*nrj)
     REAL psca(cims:cime,cjms:cjme,nri*nrj)
     LOGICAL icmask( cims:cime, cjms:cjme )
     INTEGER i,j,k
     INTEGER shw
     INTEGER spec_zone 
     INTEGER relax_zone
     INTEGER sz
     INTEGER n2ci,n
     INTEGER n2cj


     n2ci(n) = (n+ipos*nri-1)/nri
     n2cj(n) = (n+jpos*nrj-1)/nrj

     rdt = 1.D0/cdt

     shw = 0

     ioff = 0 ; joff = 0
     IF ( xstag ) ioff = (nri-1)/2
     IF ( ystag ) joff = (nrj-1)/2

     
     

     CALL nl_get_spec_zone( 1, spec_zone )
     CALL nl_get_relax_zone( 1, relax_zone )
     sz = MIN(MAX( spec_zone, relax_zone + 1 ),spec_bdy_width)

     nfx = nri * nrj

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( i,j,k,ni,nj,ni1,nj1,ci,cj,ip,jp,nk,ck,nf,icmask,psca,psca1 )
     DO k = ckts, ckte

        DO nf = 1,nfx
           DO j = cjms,cjme
              nj = (j-jpos) * nrj + ( nrj / 2 + 1 )  
              DO i = cims,cime
                ni = (i-ipos) * nri + ( nri / 2 + 1 )   
                psca1(i,j,nf) = cfld(i,k,j)
              ENDDO
           ENDDO
        ENDDO




               IF   ( njts .ge. njds .and. njts .le. njds + sz + joff  ) THEN
        CALL sintb( psca1, psca,                     &
          cims, cime, cjms, cjme, icmask,  &
          n2ci(nits)-1, n2ci(nite)+1, n2cj(MAX(njts,njds)), n2cj(MIN(njte,njds+sz+joff)), nrj*nri, xstag, ystag )
               ENDIF

               IF   ( njte .le. njde .and. njte .ge. njde - sz - joff ) THEN
        CALL sintb( psca1, psca,                     &
          cims, cime, cjms, cjme, icmask,  &
          n2ci(nits)-1, n2ci(nite)+1, n2cj(MAX(njts,njde-sz-joff)), n2cj(MIN(njte,njde-1+joff)), nrj*nri, xstag, ystag )
               ENDIF

               IF   ( nits .ge. nids .and. nits .le. nids + sz + ioff  ) THEN
        CALL sintb( psca1, psca,                     &
          cims, cime, cjms, cjme, icmask,  &
          n2ci(MAX(nits,nids)), n2ci(MIN(nite,nids+sz+ioff)), n2cj(njts)-1, n2cj(njte)+1, nrj*nri, xstag, ystag )
               ENDIF

               IF   ( nite .le. nide .and. nite .ge. nide - sz - ioff ) THEN
        CALL sintb( psca1, psca,                     &
          cims, cime, cjms, cjme, icmask,  &
          n2ci(MAX(nits,nide-sz-ioff)), n2ci(MIN(nite,nide-1+ioff)), n2cj(njts)-1, n2cj(njte)+1, nrj*nri, xstag, ystag )
               ENDIF

        DO nj1 = MAX(njds,njts-1), MIN(njde+joff,njte+joff+1) 
           cj = jpos + (nj1-1) / nrj     
           jp = mod ( nj1-1 , nrj )  
           nk = k
           ck = nk
           DO ni1 = MAX(nids,nits-1), MIN(nide+ioff,nite+ioff+1)
               ci = ipos + (ni1-1) / nri      
               ip = mod ( ni1-1 , nri )  

               ni = ni1-ioff
               nj = nj1-joff

               IF ( ( ni.LT.nids) .OR. (nj.LT.njds) ) THEN
                  CYCLE
               END IF




        
               IF   ( ni .ge. nids .and. ni .lt. nids + sz ) THEN
                 bdy_txs( nj,k,ni ) = rdt*(psca(ci+shw,cj+shw,ip+1+(jp)*nri)-nfld(ni,k,nj))
                 bdy_xs( nj,k,ni ) = psca(ci+shw,cj+shw,ip+1+(jp)*nri )
               ENDIF

        
               IF   ( nj .ge. njds .and. nj .lt. njds + sz ) THEN
                 bdy_tys( ni,k,nj ) = rdt*(psca(ci+shw,cj+shw,ip+1+(jp)*nri)-nfld(ni,k,nj))
                 bdy_ys( ni,k,nj ) = psca(ci+shw,cj+shw,ip+1+(jp)*nri )
               ENDIF

        
               IF ( xstag ) THEN
                 IF   ( ni .ge. nide - sz + 1 .AND. ni .le. nide ) THEN
                   bdy_txe( nj,k,nide-ni+1 ) = rdt*(psca(ci+shw,cj+shw,ip+1+(jp)*nri)-nfld(ni,k,nj))
                   bdy_xe( nj,k,nide-ni+1 ) = psca(ci+shw,cj+shw,ip+1+(jp)*nri )
                 ENDIF
               ELSE
                 IF   ( ni .ge. nide - sz .AND. ni .le. nide-1 ) THEN
                   bdy_txe( nj,k,nide-ni ) = rdt*(psca(ci+shw,cj+shw,ip+1+(jp)*nri)-nfld(ni,k,nj))
                   bdy_xe( nj,k,nide-ni ) = psca(ci+shw,cj+shw,ip+1+(jp)*nri )
                 ENDIF
               ENDIF

        
               IF ( ystag ) THEN
                 IF   ( nj .ge. njde - sz + 1 .AND. nj .le. njde  ) THEN
                   bdy_tye( ni,k,njde-nj+1 ) = rdt*(psca(ci+shw,cj+shw,ip+1+(jp)*nri)-nfld(ni,k,nj))
                   bdy_ye( ni,k,njde-nj+1 ) = psca(ci+shw,cj+shw,ip+1+(jp)*nri )
                 ENDIF
               ELSE
                 IF   (  nj .ge. njde - sz .AND. nj .le. njde-1 ) THEN
                   bdy_tye(ni,k,njde-nj ) = rdt*(psca(ci+shw,cj+shw,ip+1+(jp)*nri)-nfld(ni,k,nj))
                   bdy_ye( ni,k,njde-nj ) = psca(ci+shw,cj+shw,ip+1+(jp)*nri )
                 ENDIF
               ENDIF

           ENDDO
        ENDDO
     ENDDO
   !$OMP END PARALLEL DO

     RETURN

   END SUBROUTINE bdy_interp1



   SUBROUTINE interp_fcni( cfld,                                 &  
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  
                           imask,                                &  
                           xstag, ystag,                         &  
                           ipos, jpos,                           &  
                           nri, nrj                             )   
     USE module_configure
     IMPLICIT NONE


     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     INTEGER, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
     INTEGER, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask

     

     INTEGER ci, cj, ck, ni, nj, nk, ip, jp

     
     




     DO nj = njts, njte
        cj = jpos + (nj-1) / nrj     
        jp = mod ( nj , nrj )  
        DO nk = nkts, nkte
           ck = nk
           DO ni = nits, nite
              ci = ipos + (ni-1) / nri      
              ip = mod ( ni , nri )  
              
              
              nfld( ni, nk, nj ) = cfld( ci , ck , cj )
           ENDDO
        ENDDO
     ENDDO

     RETURN

   END SUBROUTINE interp_fcni

   SUBROUTINE interp_fcnm( cfld,                                 &  
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  
                           imask,                                &  
                           xstag, ystag,                         &  
                           ipos, jpos,                           &  
                           nri, nrj                             )   
     USE module_configure
     IMPLICIT NONE


     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask

     

     INTEGER ci, cj, ck, ni, nj, nk, ip, jp

     
     




     DO nj = njts, njte
        cj = jpos + (nj-1) / nrj     
        jp = mod ( nj , nrj )  
        DO nk = nkts, nkte
           ck = nk
           DO ni = nits, nite
              ci = ipos + (ni-1) / nri      
              ip = mod ( ni , nri )  
              
              
              nfld( ni, nk, nj ) = cfld( ci , ck , cj )
           ENDDO
        ENDDO
     ENDDO

     RETURN

   END SUBROUTINE interp_fcnm

   SUBROUTINE interp_mask_land_field ( enable,                   &  
                                       cfld,                     &  
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  
                           imask,                                &  
                           xstag, ystag,                         &  
                           ipos, jpos,                           &  
                           nri, nrj,                             &  
                           clu, nlu                              )

      USE module_configure
      USE module_wrf_error

      IMPLICIT NONE
   
   
      LOGICAL, INTENT(IN) :: enable
      INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                             cims, cime, ckms, ckme, cjms, cjme,   &
                             cits, cite, ckts, ckte, cjts, cjte,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nims, nime, nkms, nkme, njms, njme,   &
                             nits, nite, nkts, nkte, njts, njte,   &
                             shw,                                  &
                             ipos, jpos,                           &
                             nri, nrj
      LOGICAL, INTENT(IN) :: xstag, ystag
   
      REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
      REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask
   
      REAL, DIMENSION ( cims:cime, cjms:cjme ) :: clu
      REAL, DIMENSION ( nims:nime, njms:njme ) :: nlu
   
      
   
      INTEGER ci, cj, ck, ni, nj, nk, ip, jp
      INTEGER :: icount , ii , jj , ist , ien , jst , jen , iswater
      REAL :: avg , sum , dx , dy
      INTEGER , PARAMETER :: max_search = 5
      CHARACTER*120 message
   
      
   
      CALL nl_get_iswater(1,iswater)

      
   
      IF ( ( .NOT. xstag) .AND. ( .NOT. ystag ) ) THEN

         

       IF ( enable ) THEN

         DO nj = njts, njte
            IF ( MOD ( nrj , 2 ) .EQ. 0 ) THEN
               cj = ( nj + (nrj/2)-1 ) / nrj + jpos -1 
            ELSE
               cj = ( nj + (nrj-1)/2 ) / nrj + jpos -1 
            END IF
            DO nk = nkts, nkte
               ck = nk
               DO ni = nits, nite
                  IF ( MOD ( nri , 2 ) .EQ. 0 ) THEN
                     ci = ( ni + (nri/2)-1 ) / nri + ipos -1 
                  ELSE
                     ci = ( ni + (nri-1)/2 ) / nri + ipos -1 
                  END IF
   



                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  


                  

                  IF ( MOD ( nri , 2 ) .EQ. 0 ) THEN
                     dx = ( REAL ( MOD ( ni+(nri-1)/2 , nri ) ) + 0.5 ) / REAL ( nri ) 
                  ELSE 
                     dx =   REAL ( MOD ( ni+(nri-1)/2 , nri ) )         / REAL ( nri ) 
                  END IF
                  IF ( MOD ( nrj , 2 ) .EQ. 0 ) THEN
                     dy = ( REAL ( MOD ( nj+(nrj-1)/2 , nrj ) ) + 0.5 ) / REAL ( nrj ) 
                  ELSE 
                     dy =   REAL ( MOD ( nj+(nrj-1)/2 , nrj ) )         / REAL ( nrj ) 
                  END IF
   
                  

                  IF      ( ( NINT(nlu(ni  ,nj  )) .EQ. iswater ) ) THEN
                     

                     nfld(ni,nk,nj) =  -1

                  
                  

                  ELSE IF ( ( NINT(nlu(ni  ,nj  )) .NE. iswater ) .AND. &
                            ( NINT(clu(ci  ,cj  )) .NE. iswater ) .AND. &
                            ( NINT(clu(ci+1,cj  )) .NE. iswater ) .AND. &
                            ( NINT(clu(ci  ,cj+1)) .NE. iswater ) .AND. &
                            ( NINT(clu(ci+1,cj+1)) .NE. iswater ) ) THEN
                     nfld(ni,nk,nj) = ( 1. - dx ) * ( ( 1. - dy ) * cfld(ci  ,ck,cj  )   + &
                                                             dy   * cfld(ci  ,ck,cj+1) ) + &
                                             dx   * ( ( 1. - dy ) * cfld(ci+1,ck,cj  )   + &
                                                             dy   * cfld(ci+1,ck,cj+1) )

                  
                  

                  ELSE IF ( ( NINT(nlu(ni  ,nj  )) .NE. iswater ) .AND. &
                            ( NINT(clu(ci  ,cj  )) .EQ. iswater ) .AND. &
                            ( NINT(clu(ci+1,cj  )) .EQ. iswater ) .AND. &
                            ( NINT(clu(ci  ,cj+1)) .EQ. iswater ) .AND. &
                            ( NINT(clu(ci+1,cj+1)) .EQ. iswater ) ) THEN

                     nfld(ni,nk,nj) = -1

                  
                  
                  ELSE IF ( NINT(nlu(ni  ,nj  )) .NE. iswater ) THEN
                     icount = 0
                     sum = 0
                     IF ( NINT(clu(ci  ,cj  )) .NE. iswater ) THEN
                        icount = icount + 1
                        sum = sum + cfld(ci  ,ck,cj  )
                     END IF
                     IF ( NINT(clu(ci+1,cj  )) .NE. iswater ) THEN
                        icount = icount + 1
                        sum = sum + cfld(ci+1,ck,cj  )
                     END IF
                     IF ( NINT(clu(ci  ,cj+1)) .NE. iswater ) THEN
                        icount = icount + 1
                        sum = sum + cfld(ci  ,ck,cj+1)
                     END IF
                     IF ( NINT(clu(ci+1,cj+1)) .NE. iswater ) THEN
                        icount = icount + 1
                        sum = sum + cfld(ci+1,ck,cj+1)
                     END IF
                     nfld(ni,nk,nj) = sum / REAL ( icount ) 
                  END IF
               END DO
            END DO
         END DO


         

         sum = 0
         icount = 0 
         DO nj = njts, njte
            DO nk = nkts, nkte
               DO ni = nits, nite
                  IF ( ( nfld(ni,nk,nj) .GT. -1.e19 ) .AND. (  nfld(ni,nk,nj) .LT. 1.e19 ) ) THEN
                     icount = icount + 1
                     sum = sum + nfld(ni,nk,nj)
                  END IF
               END DO
            END DO
         END DO
       ELSE
         sum = 0.
         icount = 0
       ENDIF
       CALL wrf_dm_bcast_real( sum, 1 )
       CALL wrf_dm_bcast_integer( icount, 1 )
       IF ( enable ) THEN
         IF ( icount .GT. 0 ) THEN
           avg = sum / REAL ( icount ) 

         
         

           DO nj = njts, njte
              DO nk = nkts, nkte
                 DO ni = nits, nite
                    IF ( nfld(ni,nk,nj) .LT. -1.e19 ) THEN
                       IF ( MOD ( nrj , 2 ) .EQ. 0 ) THEN
                          cj = ( nj + (nrj/2)-1 ) / nrj + jpos -1 
                       ELSE
                          cj = ( nj + (nrj-1)/2 ) / nrj + jpos -1 
                       END IF
                       IF ( MOD ( nri , 2 ) .EQ. 0 ) THEN
                          ci = ( ni + (nri/2)-1 ) / nri + ipos -1 
                       ELSE
                          ci = ( ni + (nri-1)/2 ) / nri + ipos -1 
                       END IF
                       ist = MAX (ci-max_search,cits)
                       ien = MIN (ci+max_search,cite,cide-1)
                       jst = MAX (cj-max_search,cjts)
                       jen = MIN (cj+max_search,cjte,cjde-1)
                       icount = 0 
                       sum = 0
                       DO jj = jst,jen
                          DO ii = ist,ien
                             IF ( NINT(clu(ii,jj)) .NE. iswater ) THEN
                                icount = icount + 1
                                sum = sum + cfld(ii,nk,jj)
                             END IF
                          END DO
                       END DO
                       IF ( icount .GT. 0 ) THEN
                          nfld(ni,nk,nj) = sum / REAL ( icount ) 
                       ELSE

                          write(message,*) 'horizontal interp error - island, using average ', avg
                          CALL wrf_message ( message )
                          nfld(ni,nk,nj) = avg
                       END IF        
                    END IF
                 END DO
              END DO
           END DO
         ENDIF
       ENDIF
      ELSE
         CALL wrf_error_fatal3("",1267,&
"only unstaggered fields right now" )
      END IF

   END SUBROUTINE interp_mask_land_field

   SUBROUTINE interp_mask_water_field ( enable,                  &  
                                        cfld,                    &  
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  
                           imask,                                &  
                           xstag, ystag,                         &  
                           ipos, jpos,                           &  
                           nri, nrj,                             &  
                           clu, nlu                              )

      USE module_configure
      USE module_wrf_error

      IMPLICIT NONE
   
   
      LOGICAL, INTENT(IN) :: enable
      INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                             cims, cime, ckms, ckme, cjms, cjme,   &
                             cits, cite, ckts, ckte, cjts, cjte,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nims, nime, nkms, nkme, njms, njme,   &
                             nits, nite, nkts, nkte, njts, njte,   &
                             shw,                                  &
                             ipos, jpos,                           &
                             nri, nrj
      LOGICAL, INTENT(IN) :: xstag, ystag
   
      REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
      REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask
   
      REAL, DIMENSION ( cims:cime, cjms:cjme ) :: clu
      REAL, DIMENSION ( nims:nime, njms:njme ) :: nlu
   
      
   
      INTEGER ci, cj, ck, ni, nj, nk, ip, jp
      INTEGER :: icount , ii , jj , ist , ien , jst , jen , iswater
      REAL :: avg , sum , dx , dy
      INTEGER , PARAMETER :: max_search = 5
   
      
   
      CALL nl_get_iswater(1,iswater)

      
   
      IF ( ( .NOT. xstag) .AND. ( .NOT. ystag ) ) THEN

       IF ( enable ) THEN
         

         DO nj = njts, njte
            IF ( MOD ( nrj , 2 ) .EQ. 0 ) THEN
               cj = ( nj + (nrj/2)-1 ) / nrj + jpos -1 
            ELSE
               cj = ( nj + (nrj-1)/2 ) / nrj + jpos -1 
            END IF
            DO nk = nkts, nkte
               ck = nk
               DO ni = nits, nite
                  IF ( MOD ( nri , 2 ) .EQ. 0 ) THEN
                     ci = ( ni + (nri/2)-1 ) / nri + ipos -1 
                  ELSE
                     ci = ( ni + (nri-1)/2 ) / nri + ipos -1 
                  END IF
   



                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  


                  

                  IF ( MOD ( nri , 2 ) .EQ. 0 ) THEN
                     dx = ( REAL ( MOD ( ni+(nri-1)/2 , nri ) ) + 0.5 ) / REAL ( nri ) 
                  ELSE 
                     dx =   REAL ( MOD ( ni+(nri-1)/2 , nri ) )         / REAL ( nri ) 
                  END IF
                  IF ( MOD ( nrj , 2 ) .EQ. 0 ) THEN
                     dy = ( REAL ( MOD ( nj+(nrj-1)/2 , nrj ) ) + 0.5 ) / REAL ( nrj ) 
                  ELSE 
                     dy =   REAL ( MOD ( nj+(nrj-1)/2 , nrj ) )         / REAL ( nrj ) 
                  END IF
   
                  

                  IF      ( ( NINT(nlu(ni  ,nj  )) .NE. iswater ) ) THEN
                     

                     nfld(ni,nk,nj) = -1

                  
                  

                  ELSE IF ( ( NINT(nlu(ni  ,nj  )) .EQ. iswater ) .AND. &
                            ( NINT(clu(ci  ,cj  )) .EQ. iswater ) .AND. &
                            ( NINT(clu(ci+1,cj  )) .EQ. iswater ) .AND. &
                            ( NINT(clu(ci  ,cj+1)) .EQ. iswater ) .AND. &
                            ( NINT(clu(ci+1,cj+1)) .EQ. iswater ) ) THEN
                     nfld(ni,nk,nj) = ( 1. - dx ) * ( ( 1. - dy ) * cfld(ci  ,ck,cj  )   + &
                                                             dy   * cfld(ci  ,ck,cj+1) ) + &
                                             dx   * ( ( 1. - dy ) * cfld(ci+1,ck,cj  )   + &
                                                             dy   * cfld(ci+1,ck,cj+1) )

                  
                  

                  ELSE IF ( ( NINT(nlu(ni  ,nj  )) .EQ. iswater ) .AND. &
                            ( NINT(clu(ci  ,cj  )) .NE. iswater ) .AND. &
                            ( NINT(clu(ci+1,cj  )) .NE. iswater ) .AND. &
                            ( NINT(clu(ci  ,cj+1)) .NE. iswater ) .AND. &
                            ( NINT(clu(ci+1,cj+1)) .NE. iswater ) ) THEN

                     nfld(ni,nk,nj) = -1

                  
                  
                  ELSE IF ( NINT(nlu(ni  ,nj  )) .EQ. iswater ) THEN
                     icount = 0
                     sum = 0
                     IF ( NINT(clu(ci  ,cj  )) .EQ. iswater ) THEN
                        icount = icount + 1
                        sum = sum + cfld(ci  ,ck,cj  )
                     END IF
                     IF ( NINT(clu(ci+1,cj  )) .EQ. iswater ) THEN
                        icount = icount + 1
                        sum = sum + cfld(ci+1,ck,cj  )
                     END IF
                     IF ( NINT(clu(ci  ,cj+1)) .EQ. iswater ) THEN
                        icount = icount + 1
                        sum = sum + cfld(ci  ,ck,cj+1)
                     END IF
                     IF ( NINT(clu(ci+1,cj+1)) .EQ. iswater ) THEN
                        icount = icount + 1
                        sum = sum + cfld(ci+1,ck,cj+1)
                     END IF
                     nfld(ni,nk,nj) = sum / REAL ( icount ) 
                  END IF
               END DO
            END DO
         END DO

         

         sum = 0
         icount = 0 
         DO nj = njts, njte
            DO nk = nkts, nkte
               DO ni = nits, nite
                  IF ( ( nfld(ni,nk,nj) .GT. -1.e19 ) .AND. (  nfld(ni,nk,nj) .LT. 1.e19 ) ) THEN
                     icount = icount + 1
                     sum = sum + nfld(ni,nk,nj)
                  END IF
               END DO
            END DO
         END DO
       ELSE
         sum = 0.
         icount = 0
       ENDIF
       CALL wrf_dm_bcast_real( sum, 1 )
       CALL wrf_dm_bcast_integer( icount, 1 )
       IF ( enable ) THEN
         IF ( icount .NE. 0 ) THEN
           avg = sum / REAL ( icount ) 


           
           

           DO nj = njts, njte
              DO nk = nkts, nkte
                 DO ni = nits, nite
                    IF ( nfld(ni,nk,nj) .LT. -1.e19 ) THEN
                       IF ( MOD ( nrj , 2 ) .EQ. 0 ) THEN
                          cj = ( nj + (nrj/2)-1 ) / nrj + jpos -1 
                       ELSE
                          cj = ( nj + (nrj-1)/2 ) / nrj + jpos -1 
                       END IF
                       IF ( MOD ( nri , 2 ) .EQ. 0 ) THEN
                          ci = ( ni + (nri/2)-1 ) / nri + ipos -1 
                       ELSE
                          ci = ( ni + (nri-1)/2 ) / nri + ipos -1 
                       END IF
                       ist = MAX (ci-max_search,cits)
                       ien = MIN (ci+max_search,cite,cide-1)
                       jst = MAX (cj-max_search,cjts)
                       jen = MIN (cj+max_search,cjte,cjde-1)
                       icount = 0 
                       sum = 0
                       DO jj = jst,jen
                          DO ii = ist,ien
                             IF ( NINT(clu(ii,jj)) .EQ. iswater ) THEN
                                icount = icount + 1
                                sum = sum + cfld(ii,nk,jj)
                             END IF
                          END DO
                       END DO
                       IF ( icount .GT. 0 ) THEN
                          nfld(ni,nk,nj) = sum / REAL ( icount ) 
                       ELSE
  
                          print *,'horizontal interp error - lake, using average ',avg
                          nfld(ni,nk,nj) = avg
                       END IF        
                    END IF
                 END DO
              END DO
           END DO
         ENDIF
       ENDIF
      ELSE
         CALL wrf_error_fatal3("",1506,&
"only unstaggered fields right now" )
      END IF

   END SUBROUTINE interp_mask_water_field

   SUBROUTINE none
   END SUBROUTINE none

   SUBROUTINE smoother ( cfld , &
                      cids, cide, ckds, ckde, cjds, cjde,   &
                      cims, cime, ckms, ckme, cjms, cjme,   &
                      cits, cite, ckts, ckte, cjts, cjte,   &
                      nids, nide, nkds, nkde, njds, njde,   &
                      nims, nime, nkms, nkme, njms, njme,   &
                      nits, nite, nkts, nkte, njts, njte,   &
                      xstag, ystag,                         &  
                      ipos, jpos,                           &  
                      nri, nrj                              &
                      )
 
      USE module_configure
      IMPLICIT NONE
   
      INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                             cims, cime, ckms, ckme, cjms, cjme,   &
                             cits, cite, ckts, ckte, cjts, cjte,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nims, nime, nkms, nkme, njms, njme,   &
                             nits, nite, nkts, nkte, njts, njte,   &
                             nri, nrj,                             &  
                             ipos, jpos
      LOGICAL, INTENT(IN) :: xstag, ystag
      INTEGER             :: smooth_option, feedback , spec_zone
   
      REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld

      

      CALL nl_get_feedback       ( 1, feedback  )
      IF ( feedback == 0 ) RETURN
      CALL nl_get_spec_zone ( 1, spec_zone )

      
      
      

      CALL nl_get_smooth_option  ( 1, smooth_option  )

      IF      ( smooth_option == 0 ) THEN

      ELSE IF ( smooth_option == 1 ) THEN
         CALL sm121 ( cfld , &
                      cids, cide, ckds, ckde, cjds, cjde,   &
                      cims, cime, ckms, ckme, cjms, cjme,   &
                      cits, cite, ckts, ckte, cjts, cjte,   &
                      xstag, ystag,                         &  
                      nids, nide, nkds, nkde, njds, njde,   &
                      nims, nime, nkms, nkme, njms, njme,   &
                      nits, nite, nkts, nkte, njts, njte,   &
                      nri, nrj,                             &  
                      ipos, jpos                            &  
                      )
      ELSE IF ( smooth_option == 2 ) THEN
         CALL smdsm ( cfld , &
                      cids, cide, ckds, ckde, cjds, cjde,   &
                      cims, cime, ckms, ckme, cjms, cjme,   &
                      cits, cite, ckts, ckte, cjts, cjte,   &
                      xstag, ystag,                         &  
                      nids, nide, nkds, nkde, njds, njde,   &
                      nims, nime, nkms, nkme, njms, njme,   &
                      nits, nite, nkts, nkte, njts, njte,   &
                      nri, nrj,                             &  
                      ipos, jpos                            &  
                      )
      END IF

   END SUBROUTINE smoother 

   SUBROUTINE sm121 ( cfld , &
                      cids, cide, ckds, ckde, cjds, cjde,   &
                      cims, cime, ckms, ckme, cjms, cjme,   &
                      cits, cite, ckts, ckte, cjts, cjte,   &
                      xstag, ystag,                         &  
                      nids, nide, nkds, nkde, njds, njde,   &
                      nims, nime, nkms, nkme, njms, njme,   &
                      nits, nite, nkts, nkte, njts, njte,   &
                      nri, nrj,                             &  
                      ipos, jpos                            &  
                      )
   
      USE module_configure
      IMPLICIT NONE
   
      INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                             cims, cime, ckms, ckme, cjms, cjme,   &
                             cits, cite, ckts, ckte, cjts, cjte,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nims, nime, nkms, nkme, njms, njme,   &
                             nits, nite, nkts, nkte, njts, njte,   &
                             nri, nrj,                             &  
                             ipos, jpos
      LOGICAL, INTENT(IN) :: xstag, ystag
   
      REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
      REAL, DIMENSION ( cims:cime,            cjms:cjme ) :: cfldnew
   
      INTEGER                        :: i , j , k , loop
      INTEGER :: istag,jstag

      INTEGER, PARAMETER  :: smooth_passes = 1 

      istag = 1 ; jstag = 1
      IF ( xstag ) istag = 0
      IF ( ystag ) jstag = 0
   
      
   
      smoothing_passes : DO loop = 1 , smooth_passes
   
         DO k = ckts , ckte
   
            

            DO i = MAX(ipos,cits-3) , MIN(ipos+(nide-nids)/nri,cite+3)
               DO j = MAX(jpos,cjts-3) , MIN(jpos+(njde-njds)/nrj,cjte+3)
                  cfldnew(i,j) = cfld(i,k,j) 
               END DO
            END DO

            
   
            DO i = MAX(ipos+2,cits-2) , MIN(ipos+(nide-nids)/nri-2-istag,cite+2)
            DO j = MAX(jpos+2,cjts-2) , MIN(jpos+(njde-njds)/nrj-2-jstag,cjte+2)
                  cfldnew(i,j) = 0.25 * ( cfld(i,k,j+1) + 2.*cfld(i,k,j) + cfld(i,k,j-1) )
               END DO
            END DO

            
       
            DO j = MAX(jpos+2,cjts-2) , MIN(jpos+(njde-njds)/nrj-2-jstag,cjte+2)
            DO i = MAX(ipos+2,cits-2) , MIN(ipos+(nide-nids)/nri-2-istag,cite+2)
                  cfld(i,k,j) =  0.25 * ( cfldnew(i+1,j) + 2.*cfldnew(i,j) + cfldnew(i-1,j) )
               END DO
            END DO
       
         END DO
    
      END DO smoothing_passes
   
   END SUBROUTINE sm121

   SUBROUTINE smdsm ( cfld , &
                      cids, cide, ckds, ckde, cjds, cjde,   &
                      cims, cime, ckms, ckme, cjms, cjme,   &
                      cits, cite, ckts, ckte, cjts, cjte,   &
                      xstag, ystag,                         &  
                      nids, nide, nkds, nkde, njds, njde,   &
                      nims, nime, nkms, nkme, njms, njme,   &
                      nits, nite, nkts, nkte, njts, njte,   &
                      nri, nrj,                             &  
                      ipos, jpos                            &  
                      )
   
      USE module_configure
      IMPLICIT NONE
   
      INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                             cims, cime, ckms, ckme, cjms, cjme,   &
                             cits, cite, ckts, ckte, cjts, cjte,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nims, nime, nkms, nkme, njms, njme,   &
                             nits, nite, nkts, nkte, njts, njte,   &
                             nri, nrj,                             &  
                             ipos, jpos
      LOGICAL, INTENT(IN) :: xstag, ystag
   
      REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
      REAL, DIMENSION ( cims:cime,            cjms:cjme ) :: cfldnew
   
      REAL , DIMENSION ( 2 )         :: xnu
      INTEGER                        :: i , j , k , loop , n 
      INTEGER :: istag,jstag

      INTEGER, PARAMETER  :: smooth_passes = 1 

      xnu  =  (/ 0.50 , -0.52 /)
    
      istag = 1 ; jstag = 1
      IF ( xstag ) istag = 0
      IF ( ystag ) jstag = 0
   
      
      
   
      smoothing_passes : DO loop = 1 , smooth_passes * 2
   
         n  =  2 - MOD ( loop , 2 )
    
         DO k = ckts , ckte
   
            DO i = MAX(ipos+2,cits-2) , MIN(ipos+(nide-nids)/nri-2-istag,cite+2)
               DO j = MAX(jpos+2,cjts-2) , MIN(jpos+(njde-njds)/nrj-2-jstag,cjte+2)
                  cfldnew(i,j) = cfld(i,k,j) + xnu(n) * ((cfld(i,k,j+1) + cfld(i,k,j-1)) * 0.5-cfld(i,k,j))
               END DO
            END DO
       
            DO i = MAX(ipos+2,cits-2) , MIN(ipos+(nide-nids)/nri-2-istag,cite+2)
               DO j = MAX(jpos+2,cjts-2) , MIN(jpos+(njde-njds)/nrj-2-jstag,cjte+2)
                  cfld(i,k,j) = cfldnew(i,j)
               END DO
            END DO
       
            DO j = MAX(jpos+2,cjts-2) , MIN(jpos+(njde-njds)/nrj-2-jstag,cjte+2)
               DO i = MAX(ipos+2,cits-2) , MIN(ipos+(nide-nids)/nri-2-istag,cite+2)
                  cfldnew(i,j) = cfld(i,k,j) + xnu(n) * ((cfld(i+1,k,j) + cfld(i-1,k,j)) * 0.5-cfld(i,k,j))
               END DO
            END DO
       
            DO j = MAX(jpos+2,cjts-2) , MIN(jpos+(njde-njds)/nrj-2-jstag,cjte+2)
               DO i = MAX(ipos+2,cits-2) , MIN(ipos+(nide-nids)/nri-2-istag,cite+2)
                  cfld(i,k,j) = cfldnew(i,j)
               END DO
            END DO
   
         END DO
    
      END DO smoothing_passes
   
   END SUBROUTINE smdsm




   SUBROUTINE mark_domain (  cfld,                                 &  
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  
                           imask,                                &  
                           xstag, ystag,                         &  
                           ipos, jpos,                           &  
                           nri, nrj                             )   
     USE module_configure
     USE module_wrf_error
     IMPLICIT NONE


     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ), INTENT(OUT) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ), INTENT(IN) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: imask

     

     INTEGER ci, cj, ck, ni, nj, nk, ip, jp, ioff, joff, ioffa, joffa
     INTEGER :: icmin,icmax,jcmin,jcmax
     INTEGER :: istag,jstag, ipoints,jpoints,ijpoints

     istag = 1 ; jstag = 1
     IF ( xstag ) istag = 0
     IF ( ystag ) jstag = 0

     DO cj = MAX(jpos+1,cjts),MIN(jpos+(njde-njds)/nrj-jstag-1,cjte)
        nj = (cj-jpos)*nrj + jstag + 1
        DO ck = ckts, ckte
           nk = ck
           DO ci = MAX(ipos+1,cits),MIN(ipos+(nide-nids)/nri-istag-1,cite)
              ni = (ci-ipos)*nri + istag + 1
              cfld( ci, ck, cj ) =  9021000.  
           ENDDO
        ENDDO
     ENDDO

   END SUBROUTINE mark_domain







 SUBROUTINE interp_mass_nmm (cfld,                                 &  
                             cids, cide, ckds, ckde, cjds, cjde,   &
                             cims, cime, ckms, ckme, cjms, cjme,   &
                             cits, cite, ckts, ckte, cjts, cjte,   &
                             nfld,                                 &  
                             nids, nide, nkds, nkde, njds, njde,   &
                             nims, nime, nkms, nkme, njms, njme,   &
                             nits, nite, nkts, nkte, njts, njte,   &
                             shw,                                  &  
                             imask,                                &  
                             xstag, ystag,                         &  
                             ipos, jpos,                           &  
                             nri, nrj,                             &  
                             CII, IIH, CJJ, JJH, CBWGT1, HBWGT1,   &  
                             CBWGT2, HBWGT2, CBWGT3, HBWGT3,       &  
                             CBWGT4, HBWGT4,                       &  
                             CZ3d, Z3d,                            &  
                             CFIS,FIS,                             &  
                             CSM,SM,                               &  
                             CPDTOP,PDTOP,                         &
                             CPTOP,PTOP,                           &
                             CPSTD,PSTD,                           &
                             CKZMAX,KZMAX                          )

   USE MODULE_MODEL_CONSTANTS
   USE module_timing
   IMPLICIT NONE

   LOGICAL,INTENT(IN) :: xstag, ystag
   INTEGER,INTENT(IN) :: ckzmax,kzmax
   INTEGER,INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                         cims, cime, ckms, ckme, cjms, cjme,   &
                         cits, cite, ckts, ckte, cjts, cjte,   &
                         nids, nide, nkds, nkde, njds, njde,   &
                         nims, nime, nkms, nkme, njms, njme,   &
                         nits, nite, nkts, nkte, njts, njte,   &
                         shw,ipos,jpos,nri,nrj

   INTEGER,DIMENSION(nims:nime,njms:njme),          INTENT(IN)           :: IMASK



   INTEGER,DIMENSION(cims:cime,cjms:cjme),          INTENT(IN)           :: CII,CJJ     
   REAL,DIMENSION(cims:cime,cjms:cjme),             INTENT(IN)           :: CBWGT1,CBWGT2,CBWGT3
   REAL,DIMENSION(cims:cime,cjms:cjme),             INTENT(IN)           :: CBWGT4,CFIS,CSM
   REAL,DIMENSION(cims:cime,cjms:cjme,ckms:ckme),   INTENT(IN)           :: CFLD
   REAL,DIMENSION(cims:cime,cjms:cjme,1:KZMAX),     INTENT(IN)           :: CZ3d
   REAL,DIMENSION(1:KZMAX),                         INTENT(IN)           :: CPSTD
   REAL,INTENT(IN)                                                       :: CPDTOP,CPTOP



   INTEGER,DIMENSION(nims:nime,njms:njme),          INTENT(IN)           :: IIH,JJH
   REAL,DIMENSION(nims:nime,njms:njme),             INTENT(IN)           :: HBWGT1,HBWGT2,HBWGT3
   REAL,DIMENSION(nims:nime,njms:njme),             INTENT(IN)           :: HBWGT4
   REAL,DIMENSION(nims:nime,njms:njme),             INTENT(IN)           :: FIS,SM
   REAL,DIMENSION(nims:nime,njms:njme,nkms:nkme),   INTENT(INOUT)        :: NFLD
   REAL,DIMENSION(1:KZMAX),                         INTENT(IN)           :: PSTD
   REAL,DIMENSION(nims:nime,njms:njme,1:KZMAX),     INTENT(OUT)          :: Z3d
   REAL,INTENT(IN)                                                       :: PDTOP,PTOP



   INTEGER,PARAMETER                                          :: JTB=134
   REAL, PARAMETER                                            :: LAPSR=6.5E-3,GI=1./G, D608=0.608
   REAL, PARAMETER                                            :: COEF3=R_D*GI*LAPSR
   INTEGER                                                    :: I,J,K,IDUM
   REAL                                                       :: dlnpdz,tvout,pmo
   REAL,DIMENSION(nims:nime,njms:njme)                        :: ZS,DUM2d
   REAL,DIMENSION(JTB)                                        :: PIN,ZIN,Y2,PIO,ZOUT,DUM1,DUM2




     DO J=NJTS,MIN(NJTE,NJDE-1)
     DO I=NITS,MIN(NITE,NIDE-1)
       IF(IIH(i,j).LT.(CIDS-shw) .OR. IIH(i,j).GT.(CIDE+shw)) &
           CALL wrf_error_fatal3("",1879,&
'mass points:check domain bounds along x' )
       IF(JJH(i,j).LT.(CJDS-shw) .OR. JJH(i,j).GT.(CJDE+shw)) &
           CALL wrf_error_fatal3("",1882,&
'mass points:check domain bounds along y' )
     ENDDO
    ENDDO

    IF(KZMAX .GT. (JTB-10)) &
        CALL wrf_error_fatal3("",1888,&
'mass points: increase JTB in interp_mass_nmm')














    DO J=NJTS,MIN(NJTE,NJDE-1)
      DO I=NITS,MIN(NITE,NIDE-1)
         ZS(I,J)=FIS(I,J)/G
      ENDDO
    ENDDO



















    Z3d=0.0
    DO K=NKTS,KZMAX                
      DO J=NJTS,MIN(NJTE,NJDE-1)
        DO I=NITS,MIN(NITE,NIDE-1)

           IF(MOD(JJH(I,J),2) .NE. 0)THEN    
               Z3d(I,J,K) = HBWGT1(I,J)*CZ3d(IIH(I,J),  JJH(I,J)  ,K) &
                          + HBWGT2(I,J)*CZ3d(IIH(I,J)+1,JJH(I,J)  ,K) &
                          + HBWGT3(I,J)*CZ3d(IIH(I,J),  JJH(I,J)-1,K) &
                          + HBWGT4(I,J)*CZ3d(IIH(I,J),  JJH(I,J)+1,K)
           ELSE
               Z3d(I,J,K) = HBWGT1(I,J)*CZ3d(IIH(I,J),  JJH(I,J)  ,K) &
                          + HBWGT2(I,J)*CZ3d(IIH(I,J)+1,JJH(I,J)  ,K) &
                          + HBWGT3(I,J)*CZ3d(IIH(I,J)+1,JJH(I,J)-1,K) &
                          + HBWGT4(I,J)*CZ3d(IIH(I,J)+1,JJH(I,J)+1,K)

           ENDIF

        ENDDO
      ENDDO
    ENDDO



    DO J=NJTS,MIN(NJTE,NJDE-1)
      DO I=NITS,MIN(NITE,NIDE-1)

          IF (ZS(I,J) .LT. Z3d(I,J,1)) THEN
            dlnpdz     = (log(PSTD(1))-log(PSTD(2)) )/(Z3d(i,j,1)-Z3d(i,j,2))
            dum2d(i,j) = exp(log(PSTD(1)) + dlnpdz*(ZS(I,J) - Z3d(i,j,1)))
            dum2d(i,j) = dum2d(i,j) - PDTOP -PTOP
          ELSE                                           
            DO K =NKTS,KZMAX-1                           
             IF(ZS(I,J) .GE. Z3d(I,J,K) .AND. ZS(I,J) .LT. Z3d(I,J,K+1))THEN
               dlnpdz     = (log(PSTD(K))-log(PSTD(K+1)) ) /(Z3d(I,J,K)-Z3d(I,J,K+1))
               dum2d(i,j) = exp(log(PSTD(K)) + dlnpdz*(ZS(I,J)- Z3d(I,J,K)))
               dum2d(i,j) = dum2d(i,j) - PDTOP -PTOP
             ENDIF
            ENDDO
          ENDIF
          IF(ZS(I,J) .GE. Z3d(I,J,KZMAX))THEN
             WRITE(0,*)'I=',I,'J=',J,'K=',KZMAX,'TERRAIN HEIGHT',ZS(I,J),'Z3d',Z3d(I,J,KZMAX)
             CALL wrf_error_fatal3 ( "interp_fcn.b" , 176 , "MOUNTAIN TOO HIGH TO FIT THE MODEL DEPTH")
          ENDIF

      ENDDO
    ENDDO

    DO K=NKDS,NKDE                       
      DO J=NJTS,MIN(NJTE,NJDE-1)
       DO I=NITS,MIN(NITE,NIDE-1)
         IF(IMASK(I,J) .NE. 1)THEN
           NFLD(I,J,K)= dum2d(i,j)         
         ENDIF
       ENDDO
      ENDDO
    ENDDO


  END SUBROUTINE interp_mass_nmm



 SUBROUTINE nmm_bdymass_hinterp ( cfld,                              &  
                               cids, cide, ckds, ckde, cjds, cjde,   &
                               cims, cime, ckms, ckme, cjms, cjme,   &
                               cits, cite, ckts, ckte, cjts, cjte,   &
                               nfld,                                 &  
                               nids, nide, nkds, nkde, njds, njde,   &
                               nims, nime, nkms, nkme, njms, njme,   &
                               nits, nite, nkts, nkte, njts, njte,   &
                               shw,                                  &  
                               imask,                                &  
                               xstag, ystag,                         &  
                               ipos, jpos,                           &  
                               nri, nrj,                             &  
                               c_bxs,n_bxs,                          &
                               c_bxe,n_bxe,                          &
                               c_bys,n_bys,                          &
                               c_bye,n_bye,                          &
                               c_btxs,n_btxs,                        &
                               c_btxe,n_btxe,                        &
                               c_btys,n_btys,                        &
                               c_btye,n_btye,                        &
                               CTEMP_B,NTEMP_B,                      &  
                               CTEMP_BT,NTEMP_BT,                    &  
                               CII, IIH, CJJ, JJH, CBWGT1, HBWGT1,   &  
                               CBWGT2, HBWGT2, CBWGT3, HBWGT3,       &  
                               CBWGT4, HBWGT4,                       &  
                               CZ3d, Z3d,                            &  
                               CFIS,FIS,                             &  
                               CSM,SM,                               &  
                               CPDTOP,PDTOP,                         &
                               CPTOP,PTOP,                           &
                               CPSTD,PSTD,                           &
                               CKZMAX,KZMAX                          )


     USE MODULE_MODEL_CONSTANTS
     USE module_configure
     USE module_wrf_error

     IMPLICIT NONE


     INTEGER, INTENT(IN) :: ckzmax,kzmax
     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj


   REAL, DIMENSION ( cims:cime, cjms:cjme, ckms:ckme ), INTENT(OUT) :: ctemp_b,ctemp_bt
   REAL, DIMENSION ( nims:nime, njms:njme, nkms:nkme ), INTENT(OUT) :: ntemp_b,ntemp_bt
   LOGICAL, INTENT(IN) :: xstag, ystag
   REAL,  DIMENSION( * ), INTENT(INOUT) :: c_bxs,n_bxs,c_bxe,n_bxe,c_bys,n_bys,c_bye,n_bye
   REAL,  DIMENSION( * ), INTENT(INOUT) :: c_btxs,n_btxs,c_btxe,n_btxe,c_btys,n_btys,c_btye,n_btye



   INTEGER,DIMENSION(nims:nime,njms:njme),          INTENT(IN)           :: IMASK
   INTEGER,DIMENSION(cims:cime,cjms:cjme),          INTENT(IN)           :: CII,CJJ     
   REAL,DIMENSION(cims:cime,cjms:cjme),             INTENT(IN)           :: CBWGT1,CBWGT2,CBWGT3
   REAL,DIMENSION(cims:cime,cjms:cjme),             INTENT(IN)           :: CBWGT4,CFIS,CSM
   REAL,DIMENSION(cims:cime,cjms:cjme,ckms:ckme),   INTENT(IN)           :: CFLD
   REAL,DIMENSION(cims:cime,cjms:cjme,1:KZMAX),     INTENT(IN)           :: CZ3d
   REAL,DIMENSION(1:KZMAX),                         INTENT(IN)           :: CPSTD
   REAL,INTENT(IN)                                                       :: CPDTOP,CPTOP



   INTEGER,DIMENSION(nims:nime,njms:njme),          INTENT(IN)           :: IIH,JJH
   REAL,DIMENSION(nims:nime,njms:njme),             INTENT(IN)           :: HBWGT1,HBWGT2,HBWGT3
   REAL,DIMENSION(nims:nime,njms:njme),             INTENT(IN)           :: HBWGT4
   REAL,DIMENSION(nims:nime,njms:njme),             INTENT(IN)           :: FIS,SM
   REAL,DIMENSION(nims:nime,njms:njme,nkms:nkme),   INTENT(INOUT)        :: NFLD
   REAL,DIMENSION(1:KZMAX),                         INTENT(IN)           :: PSTD
   REAL,DIMENSION(nims:nime,njms:njme,1:KZMAX),     INTENT(OUT)          :: Z3d
   REAL,INTENT(IN)                                                       :: PDTOP,PTOP



     INTEGER                                     :: nijds, nijde, spec_bdy_width,i,j,k
     REAL                                        :: dlnpdz,dum2d
     REAL,DIMENSION(nims:nime,njms:njme)         :: zs

  INTEGER,PARAMETER                                                :: JTB=134
  INTEGER                                                          :: ii,jj
  REAL, DIMENSION (nims:nime,njms:njme)                            :: CWK1,CWK2,CWK3,CWK4

     nijds = min(nids, njds)
     nijde = max(nide, njde)
     CALL nl_get_spec_bdy_width( 1, spec_bdy_width )





    DO J=NJTS,MIN(NJTE,NJDE-1)
      DO I=NITS,MIN(NITE,NIDE-1)
         ZS(I,J)=FIS(I,J)/G
      ENDDO
    ENDDO



       NMM_XS: IF(NITS .EQ. NIDS)THEN

        I = NIDS

        DO K=NKTS,KZMAX
          DO J = NJTS,MIN(NJTE,NJDE-1)
            IF(MOD(JJH(I,J),2) .NE. 0)THEN      
              Z3d(I,J,K) = HBWGT1(I,J)*CZ3d(IIH(I,J),  JJH(I,J)  ,K) &
                         + HBWGT2(I,J)*CZ3d(IIH(I,J)+1,JJH(I,J)  ,K) &
                         + HBWGT3(I,J)*CZ3d(IIH(I,J),  JJH(I,J)-1,K) &
                         + HBWGT4(I,J)*CZ3d(IIH(I,J),  JJH(I,J)+1,K)
            ELSE
              Z3d(I,J,K) = HBWGT1(I,J)*CZ3d(IIH(I,J),  JJH(I,J)  ,K) &
                         + HBWGT2(I,J)*CZ3d(IIH(I,J)+1,JJH(I,J)  ,K) &
                         + HBWGT3(I,J)*CZ3d(IIH(I,J)+1,JJH(I,J)-1,K) &
                         + HBWGT4(I,J)*CZ3d(IIH(I,J)+1,JJH(I,J)+1,K)
            ENDIF
          END DO
        END DO

        DO J = NJTS,MIN(NJTE,NJDE-1)
          IF(MOD(J,2) .NE. 0)THEN
            IF (ZS(I,J) .LT. Z3d(I,J,2)) THEN              
               dlnpdz     = (log(PSTD(1))-log(PSTD(2)) )/(Z3d(I,J,1)-Z3d(I,J,2))
               dum2d      = exp(log(PSTD(1)) + dlnpdz*(ZS(I,J) - Z3d(I,J,1)))
               CWK1(I,J)  = dum2d -PDTOP -PTOP
            ELSE 
              DO K =NKTS,KZMAX-1
               IF(ZS(I,J) .GE. Z3d(I,J,K) .AND. ZS(I,J) .LT. Z3d(I,J,K+1))THEN
                 dlnpdz     = (log(PSTD(K))-log(PSTD(K+1)) ) /(Z3d(I,J,K)-Z3d(I,J,K+1))
                 dum2d      = exp(log(PSTD(K)) + dlnpdz*(ZS(I,J)- Z3d(I,J,K)))
                 CWK1(I,J)  = dum2d -PDTOP -PTOP
               ENDIF
              ENDDO
            ENDIF
            IF(ZS(I,J) .GE. Z3d(I,J,KZMAX))THEN
               WRITE(0,*)'I=',I,'J=',J,'K=',K,'TERRAIN HEIGHT',ZS(I,J),'Z3d',Z3d(I,J,KZMAX)
               CALL wrf_error_fatal3("",2136,&
"BC:MOUNTAIN TOO HIGH TO FIT THE MODEL DEPTH")
            ENDIF
          ELSE
           CWK1(I,J)=0.
          ENDIF
        ENDDO

        DO J = NJTS,MIN(NJTE,NJDE-1)
         DO K = NKDS,NKDE
           ntemp_b(i,j,k)     = CWK1(I,J)
           ntemp_bt(i,j,k)    = 0.0
         END DO
        END DO
       ENDIF NMM_XS



       NMM_XE: IF(NITE-1 .EQ. NIDE-1)THEN

       I = NIDE-1
       II = NIDE - I

       DO K=NKTS,KZMAX
         DO J=NJTS,MIN(NJTE,NJDE-1)
             IF(MOD(JJH(I,J),2) .NE. 0)THEN    
                 Z3d(I,J,K) = HBWGT1(I,J)*CZ3d(IIH(I,J),  JJH(I,J)  ,K) &
                            + HBWGT2(I,J)*CZ3d(IIH(I,J)+1,JJH(I,J)  ,K) &
                            + HBWGT3(I,J)*CZ3d(IIH(I,J),  JJH(I,J)-1,K) &
                            + HBWGT4(I,J)*CZ3d(IIH(I,J),  JJH(I,J)+1,K)
             ELSE
                 Z3d(I,J,K) = HBWGT1(I,J)*CZ3d(IIH(I,J),  JJH(I,J)  ,K) &
                            + HBWGT2(I,J)*CZ3d(IIH(I,J)+1,JJH(I,J)  ,K) &
                            + HBWGT3(I,J)*CZ3d(IIH(I,J)+1,JJH(I,J)-1,K) &
                            + HBWGT4(I,J)*CZ3d(IIH(I,J)+1,JJH(I,J)+1,K)
             ENDIF
         ENDDO
       ENDDO

        DO J = NJTS,MIN(NJTE,NJDE-1)
          IF(MOD(J,2) .NE.0)THEN                
            IF (ZS(I,J) .LT. Z3d(I,J,2)) THEN              
               dlnpdz     = (log(PSTD(1))-log(PSTD(2)) )/(Z3d(I,J,1)-Z3d(I,J,2))
               dum2d      = exp(log(PSTD(1)) + dlnpdz*(ZS(I,J) - Z3d(I,J,1)))
               CWK2(I,J)  = dum2d -PDTOP -PTOP
            ELSE 
              DO K =NKTS,KZMAX-1
               IF(ZS(I,J) .GE. Z3d(I,J,K) .AND. ZS(I,J) .LT. Z3d(I,J,K+1))THEN
                 dlnpdz     = (log(PSTD(K))-log(PSTD(K+1)) ) /(Z3d(I,J,K)-Z3d(I,J,K+1))
                 dum2d      = exp(log(PSTD(K)) + dlnpdz*(ZS(I,J)- Z3d(I,J,K)))
                 CWK2(I,J)  = dum2d -PDTOP -PTOP
               ENDIF
              ENDDO
            ENDIF
            IF(ZS(I,J) .GE. Z3d(I,J,KZMAX))THEN
               WRITE(0,*)'I=',I,'J=',J,'K=',K,'TERRAIN HEIGHT',ZS(I,J),'Z3d',Z3d(I,J,KZMAX)
               CALL wrf_error_fatal3("",2192,&
"BC:MOUNTAIN TOO HIGH TO FIT THE MODEL DEPTH")
            ENDIF
          ELSE
              CWK2(I,J) = 0.0
          ENDIF
        ENDDO

        DO J = NJTS,MIN(NJTE,NJDE-1)
         DO K = NKDS,NKDE
           ntemp_b(i,j,k)     = CWK2(I,J)
           ntemp_bt(i,j,k)    = 0.0
         END DO
        END DO
       ENDIF NMM_XE



       NMM_YS: IF(NJTS .EQ. NJDS)THEN

        J = NJDS
        DO K=NKTS,KZMAX
         DO I = NITS,MIN(NITE,NIDE-1)
            IF(MOD(JJH(I,J),2) .NE. 0)THEN    
                Z3d(I,J,K) = HBWGT1(I,J)*CZ3d(IIH(I,J),  JJH(I,J)  ,K) &
                           + HBWGT2(I,J)*CZ3d(IIH(I,J)+1,JJH(I,J)  ,K) &
                           + HBWGT3(I,J)*CZ3d(IIH(I,J),  JJH(I,J)-1,K) &
                           + HBWGT4(I,J)*CZ3d(IIH(I,J),  JJH(I,J)+1,K)
            ELSE
                Z3d(I,J,K) = HBWGT1(I,J)*CZ3d(IIH(I,J),  JJH(I,J)  ,K) &
                           + HBWGT2(I,J)*CZ3d(IIH(I,J)+1,JJH(I,J)  ,K) &
                           + HBWGT3(I,J)*CZ3d(IIH(I,J)+1,JJH(I,J)-1,K) &
                           + HBWGT4(I,J)*CZ3d(IIH(I,J)+1,JJH(I,J)+1,K)
            ENDIF
         END DO
        END DO

        DO I = NITS,MIN(NITE,NIDE-1)
          IF (ZS(I,J) .LT. Z3d(I,J,2)) THEN              
               dlnpdz     = (log(PSTD(1))-log(PSTD(2)) )/(Z3d(I,J,1)-Z3d(I,J,2))
               dum2d      = exp(log(PSTD(1)) + dlnpdz*(ZS(I,J) - Z3d(I,J,1)))
               CWK3(I,J)  = dum2d -PDTOP -PTOP
          ELSE 
              DO K =NKTS,KZMAX-1
               IF(ZS(I,J) .GE. Z3d(I,J,K) .AND. ZS(I,J) .LT. Z3d(I,J,K+1))THEN
                 dlnpdz     = (log(PSTD(K))-log(PSTD(K+1)) ) /(Z3d(I,J,K)-Z3d(I,J,K+1))
                 dum2d      = exp(log(PSTD(K)) + dlnpdz*(ZS(I,J)- Z3d(I,J,K)))
                 CWK3(I,J)  = dum2d -PDTOP -PTOP
               ENDIF
              ENDDO
          ENDIF
          IF(ZS(I,J) .GE. Z3d(I,J,KZMAX))THEN
             WRITE(0,*)'I=',I,'J=',J,'K=',K,'TERRAIN HEIGHT',ZS(I,J),'Z3d',Z3d(I,J,KZMAX)
             CALL wrf_error_fatal3("",2245,&
"BC:MOUNTAIN TOO HIGH TO FIT THE MODEL DEPTH")
          ENDIF
        ENDDO

        DO K = NKDS, NKDE
         DO I = NITS,MIN(NITE,NIDE-1)
           ntemp_b(i,j,k)     = CWK3(I,J)
           ntemp_bt(i,j,k)    = 0.0
         END DO
        END DO
       END IF NMM_YS



       NMM_YE: IF(NJTE-1 .EQ. NJDE-1)THEN

        J = NJDE-1
        JJ = NJDE - J
        DO K=NKTS,KZMAX
         DO I = NITS,MIN(NITE,NIDE-1)
             IF(MOD(JJH(I,J),2) .NE. 0)THEN    
                 Z3d(I,J,K) = HBWGT1(I,J)*CZ3d(IIH(I,J),  JJH(I,J)  ,K) &
                            + HBWGT2(I,J)*CZ3d(IIH(I,J)+1,JJH(I,J)  ,K) &
                            + HBWGT3(I,J)*CZ3d(IIH(I,J),  JJH(I,J)-1,K) &
                            + HBWGT4(I,J)*CZ3d(IIH(I,J),  JJH(I,J)+1,K)
             ELSE
                 Z3d(I,J,K) = HBWGT1(I,J)*CZ3d(IIH(I,J),  JJH(I,J)  ,K) &
                            + HBWGT2(I,J)*CZ3d(IIH(I,J)+1,JJH(I,J)  ,K) &
                            + HBWGT3(I,J)*CZ3d(IIH(I,J)+1,JJH(I,J)-1,K) &
                            + HBWGT4(I,J)*CZ3d(IIH(I,J)+1,JJH(I,J)+1,K)
             ENDIF
         END DO
        END DO

        DO I = NITS,MIN(NITE,NIDE-1)
          IF (ZS(I,J) .LT. Z3d(I,J,2)) THEN              
               dlnpdz     = (log(PSTD(1))-log(PSTD(2)) )/(Z3d(I,J,1)-Z3d(I,J,2))
               dum2d      = exp(log(PSTD(1)) + dlnpdz*(ZS(I,J) - Z3d(I,J,1)))
               CWK4(I,J)  = dum2d -PDTOP -PTOP
          ELSE 
              DO K =NKTS,KZMAX-1
               IF(ZS(I,J) .GE. Z3d(I,J,K) .AND. ZS(I,J) .LT. Z3d(I,J,K+1))THEN
                 dlnpdz     = (log(PSTD(K))-log(PSTD(K+1)) ) /(Z3d(I,J,K)-Z3d(I,J,K+1))
                 dum2d      = exp(log(PSTD(K)) + dlnpdz*(ZS(I,J)- Z3d(I,J,K)))
                 CWK4(I,J)  = dum2d -PDTOP -PTOP
               ENDIF
              ENDDO
          ENDIF
          IF(ZS(I,J) .GE. Z3d(I,J,KZMAX))THEN
             WRITE(0,*)'I=',I,'J=',J,'K=',K,'TERRAIN HEIGHT',ZS(I,J),'Z3d',Z3d(I,J,KZMAX)
             CALL wrf_error_fatal3("",2296,&
"BC:MOUNTAIN TOO HIGH TO FIT THE MODEL DEPTH")
          ENDIF
        ENDDO

        DO K = NKDS,NKDE
         DO I = NITS,MIN(NITE,NIDE-1)
              ntemp_b(i,j,k)     = CWK4(I,J)
              ntemp_bt(i,j,k)    = 0.0
         END DO
        END DO
       END IF NMM_YE

     RETURN

   END SUBROUTINE nmm_bdymass_hinterp







 SUBROUTINE interp_scalar_nmm (cfld,                               &  
                               cids,cide,ckds,ckde,cjds,cjde,      &
                               cims,cime,ckms,ckme,cjms,cjme,      &
                               cits,cite,ckts,ckte,cjts,cjte,      &
                               nfld,                               &  
                               nids,nide,nkds,nkde,njds,njde,      &
                               nims,nime,nkms,nkme,njms,njme,      &
                               nits,nite,nkts,nkte,njts,njte,      &
                               shw,                                &  
                               imask,                              &  
                               xstag,ystag,                        &  
                               ipos,jpos,                          &  
                               nri,nrj,                            &  
                               CII, IIH, CJJ, JJH, CBWGT1, HBWGT1, &  
                               CBWGT2, HBWGT2, CBWGT3, HBWGT3,     &  
                               CBWGT4, HBWGT4,                     &  
                               CC3d,C3d,                           &
                               CPD,PD,                             &
                               CPSTD,PSTD,                         &
                               CPDTOP,PDTOP,                       &
                               CPTOP,PTOP,                         &
                               CETA1,ETA1,CETA2,ETA2               )

   USE MODULE_MODEL_CONSTANTS
   USE module_timing
   IMPLICIT NONE

   LOGICAL,INTENT(IN) :: xstag, ystag
   INTEGER,INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                         cims, cime, ckms, ckme, cjms, cjme,   &
                         cits, cite, ckts, ckte, cjts, cjte,   &
                         nids, nide, nkds, nkde, njds, njde,   &
                         nims, nime, nkms, nkme, njms, njme,   &
                         nits, nite, nkts, nkte, njts, njte,   &
                         shw,ipos,jpos,nri,nrj

   INTEGER,DIMENSION(nims:nime,njms:njme),   INTENT(IN)      :: IMASK



   INTEGER,DIMENSION(cims:cime,cjms:cjme),        INTENT(IN) :: CII,CJJ   
   REAL,DIMENSION(cims:cime,cjms:cjme),           INTENT(IN) :: CBWGT1,CBWGT2
   REAL,DIMENSION(cims:cime,cjms:cjme),           INTENT(IN) :: CBWGT3,CBWGT4

   REAL,DIMENSION(cims:cime,cjms:cjme,ckms:ckme), INTENT(IN) :: CFLD
   REAL,DIMENSION(cims:cime,cjms:cjme,ckms:ckme), INTENT(IN) :: CC3d  
   REAL,DIMENSION(ckms:ckme),                     INTENT(IN) :: CPSTD
   REAL,DIMENSION(cims:cime,cjms:cjme),           INTENT(IN) :: CPD
   REAL,DIMENSION(ckms:ckme),                     INTENT(IN) :: CETA1,CETA2
   REAL,                                          INTENT(IN) :: CPDTOP,CPTOP



   INTEGER,DIMENSION(nims:nime,njms:njme),        INTENT(IN) :: IIH,JJH
   REAL,DIMENSION(nims:nime,njms:njme),           INTENT(IN) :: HBWGT1,HBWGT2
   REAL,DIMENSION(nims:nime,njms:njme),           INTENT(IN) :: HBWGT3,HBWGT4

   REAL,DIMENSION(nims:nime,njms:njme,nkms:nkme), INTENT(OUT):: NFLD  
   REAL,DIMENSION(nims:nime,njms:njme,nkms:nkme), INTENT(OUT):: C3d   
   REAL,DIMENSION(nkms:nkme),                     INTENT(IN) :: PSTD
   REAL,DIMENSION(nims:nime,njms:njme),           INTENT(IN) :: PD
   REAL,DIMENSION(nkms:nkme),                     INTENT(IN) :: ETA1,ETA2
   REAL,INTENT(IN)                                           :: PDTOP,PTOP



   INTEGER,PARAMETER                                         :: JTB=134
   INTEGER                                                   :: I,J,K
   REAL,DIMENSION(JTB)                                       :: PIN,CIN,Y2,PIO,PTMP,COUT,DUM1,DUM2






    IF(nkme .GT. (JTB-10) .OR. NKDE .GT. (JTB-10)) &
      CALL wrf_error_fatal3("",2395,&
'mass points: increase JTB in interp_mass_nmm')



















    C3d=0.0
    DO K=NKDS,NKDE-1                
      DO J=NJTS,MIN(NJTE,NJDE-1)
        DO I=NITS,MIN(NITE,NIDE-1)
         IF(IMASK(I,J) .NE. 1)THEN
           IF(MOD(JJH(I,J),2) .NE. 0)THEN    
               C3d(I,J,K) = HBWGT1(I,J)*CC3d(IIH(I,J),  JJH(I,J)  ,K) &
                          + HBWGT2(I,J)*CC3d(IIH(I,J)+1,JJH(I,J)  ,K) &
                          + HBWGT3(I,J)*CC3d(IIH(I,J),  JJH(I,J)-1,K) &
                          + HBWGT4(I,J)*CC3d(IIH(I,J),  JJH(I,J)+1,K)

           ELSE
               C3d(I,J,K) = HBWGT1(I,J)*CC3d(IIH(I,J),  JJH(I,J)  ,K) &
                          + HBWGT2(I,J)*CC3d(IIH(I,J)+1,JJH(I,J)  ,K) &
                          + HBWGT3(I,J)*CC3d(IIH(I,J)+1,JJH(I,J)-1,K) &
                          + HBWGT4(I,J)*CC3d(IIH(I,J)+1,JJH(I,J)+1,K)

           ENDIF
         ENDIF
        ENDDO
      ENDDO
    ENDDO




    DO J=NJTS,MIN(NJTE,NJDE-1)
     DO I=NITS,MIN(NITE,NIDE-1)
      IF(IMASK(I,J) .NE. 1)THEN



         CIN=0.;PIN=0.;Y2=0;PIO=0.;PTMP=0.;COUT=0.;DUM1=0.;DUM2=0.

         DO K=NKDS+1,NKDE                    
           PIN(K-1) = EXP((ALOG(PSTD(NKDE-K+1))+ALOG(PSTD(NKDE-K+2)))*0.5)
           CIN(K-1) = C3d(I,J,NKDE-K+1)
         ENDDO

         Y2(1   )=0.
         Y2(NKDE-1)=0.

         DO K=NKDS,NKDE                         
           PTMP(K) = ETA1(K)*PDTOP + ETA2(K)*PD(I,J) + PTOP
         ENDDO

         DO K=NKDS,NKDE-1                        
           PIO(K) = EXP((ALOG(PTMP(K))+ALOG(PTMP(K+1)))*0.5)
         ENDDO

         IF(PTMP(1) .GE. PSTD(1))THEN           
           PIN(NKDE-1) = PIO(1)                 
           WRITE(0,*)'WARNING: NESTED DOMAIN PRESSURE AT LOWEST LEVEL HIGHER THAN PSTD'
           WRITE(0,*)'I,J,PIO(1),PSTD(1)',I,J,PIO(1),PSTD(1)
         ENDIF

         CALL SPLINE2(I,J,JTB,NKDE-1,PIN,CIN,Y2,NKDE-1,PIO,COUT,DUM1,DUM2)  

         DO K=1,NKDE-1
           NFLD(I,J,K)= COUT(K)  
         ENDDO

      ENDIF
     ENDDO
    ENDDO

 END SUBROUTINE interp_scalar_nmm



 SUBROUTINE  nmm_bdy_scalar (cfld,                               &  
                             cids,cide,ckds,ckde,cjds,cjde,      &
                             cims,cime,ckms,ckme,cjms,cjme,      &
                             cits,cite,ckts,ckte,cjts,cjte,      &
                             nfld,                               &  
                             nids,nide,nkds,nkde,njds,njde,      &
                             nims,nime,nkms,nkme,njms,njme,      &
                             nits,nite,nkts,nkte,njts,njte,      &
                             shw,                                &  
                             imask,                              &  
                             xstag,ystag,                        &  
                             ipos,jpos,                          &  
                             nri,nrj,                            &  
                               c_bxs,n_bxs,                          &
                               c_bxe,n_bxe,                          &
                               c_bys,n_bys,                          &
                               c_bye,n_bye,                          &
                               c_btxs,n_btxs,                        &
                               c_btxe,n_btxe,                        &
                               c_btys,n_btys,                        &
                               c_btye,n_btye,                        &
                             cdt, ndt,                           &
                             CTEMP_B,NTEMP_B,                    &  
                             CTEMP_BT,NTEMP_BT,                  &
                             CII, IIH, CJJ, JJH, CBWGT1, HBWGT1, &  
                             CBWGT2, HBWGT2, CBWGT3, HBWGT3,     &  
                             CBWGT4, HBWGT4,                     &  
                             CC3d,C3d,                           &
                             CPD,PD,                             &
                             CPSTD,PSTD,                         &
                             CPDTOP,PDTOP,                       &
                             CPTOP,PTOP,                         &
                             CETA1,ETA1,CETA2,ETA2               )
   USE MODULE_MODEL_CONSTANTS
   USE module_timing
   IMPLICIT NONE

   LOGICAL,INTENT(IN)                                               :: xstag, ystag
   REAL, INTENT(INOUT)                                              :: cdt, ndt
   INTEGER,INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                         cims, cime, ckms, ckme, cjms, cjme,   &
                         cits, cite, ckts, ckte, cjts, cjte,   &
                         nids, nide, nkds, nkde, njds, njde,   &
                         nims, nime, nkms, nkme, njms, njme,   &
                         nits, nite, nkts, nkte, njts, njte,   &
                         shw,ipos,jpos,nri,nrj
   REAL, DIMENSION ( cims:cime, cjms:cjme, ckms:ckme ), INTENT(OUT) :: ctemp_b,ctemp_bt
   REAL, DIMENSION ( nims:nime, njms:njme, nkms:nkme ), INTENT(OUT) :: ntemp_b,ntemp_bt
   REAL,  DIMENSION( * ), INTENT(INOUT) :: c_bxs,n_bxs,c_bxe,n_bxe,c_bys,n_bys,c_bye,n_bye
   REAL,  DIMENSION( * ), INTENT(INOUT) :: c_btxs,n_btxs,c_btxe,n_btxe,c_btys,n_btys,c_btye,n_btye


   INTEGER,DIMENSION(nims:nime,njms:njme),        INTENT(IN) :: IMASK



   INTEGER,DIMENSION(cims:cime,cjms:cjme),        INTENT(IN) :: CII,CJJ   
   REAL,DIMENSION(cims:cime,cjms:cjme),           INTENT(IN) :: CBWGT1,CBWGT2
   REAL,DIMENSION(cims:cime,cjms:cjme),           INTENT(IN) :: CBWGT3,CBWGT4
   REAL,DIMENSION(cims:cime,cjms:cjme,ckms:ckme), INTENT(IN) :: CFLD
   REAL,DIMENSION(cims:cime,cjms:cjme,ckms:ckme), INTENT(IN) :: CC3d 
   REAL,DIMENSION(ckms:ckme),                     INTENT(IN) :: CPSTD
   REAL,DIMENSION(cims:cime,cjms:cjme),           INTENT(IN) :: CPD
   REAL,DIMENSION(ckms:ckme),                     INTENT(IN) :: CETA1,CETA2
   REAL,                                          INTENT(IN) :: CPDTOP,CPTOP



   INTEGER,DIMENSION(nims:nime,njms:njme),        INTENT(IN) :: IIH,JJH
   REAL,DIMENSION(nims:nime,njms:njme),           INTENT(IN) :: HBWGT1,HBWGT2
   REAL,DIMENSION(nims:nime,njms:njme),           INTENT(IN) :: HBWGT3,HBWGT4
   REAL,DIMENSION(nims:nime,njms:njme,nkms:nkme), INTENT(OUT):: NFLD 
   REAL,DIMENSION(nims:nime,njms:njme,nkms:nkme), INTENT(OUT):: C3d   
   REAL,DIMENSION(nkms:nkme),                     INTENT(IN) :: PSTD
   REAL,DIMENSION(nims:nime,njms:njme),           INTENT(IN) :: PD
   REAL,DIMENSION(nkms:nkme),                     INTENT(IN) :: ETA1,ETA2
   REAL,INTENT(IN)                                           :: PDTOP,PTOP



   INTEGER,PARAMETER                                       :: JTB=134
   INTEGER                                                 :: I,J,K,II,JJ
   REAL,DIMENSION(JTB)                                     :: PIN,CIN,Y2,PIO,PTMP,COUT,DUM1,DUM2
   REAL, DIMENSION (nims:nime,njms:njme,nkms:nkme)         :: CWK1,CWK2,CWK3,CWK4





    IF(nkme .GT. (JTB-10) .OR. NKDE .GT. (JTB-10)) &
      CALL wrf_error_fatal3("",2576,&
'mass points: increase JTB in interp_mass_nmm')



    NMM_XS: IF(NITS .EQ. NIDS)THEN

      I = NIDS
      DO K=NKDS,NKDE-1                
        DO J = NJTS,MIN(NJTE,NJDE-1)
          IF(MOD(JJH(I,J),2) .NE. 0)THEN      
            C3d(I,J,K) = HBWGT1(I,J)*CC3d(IIH(I,J),  JJH(I,J)  ,K) &
                       + HBWGT2(I,J)*CC3d(IIH(I,J)+1,JJH(I,J)  ,K) &
                       + HBWGT3(I,J)*CC3d(IIH(I,J),  JJH(I,J)-1,K) &
                       + HBWGT4(I,J)*CC3d(IIH(I,J),  JJH(I,J)+1,K)
          ELSE
            C3d(I,J,K) = HBWGT1(I,J)*CC3d(IIH(I,J),  JJH(I,J)  ,K) &
                       + HBWGT2(I,J)*CC3d(IIH(I,J)+1,JJH(I,J)  ,K) &
                       + HBWGT3(I,J)*CC3d(IIH(I,J)+1,JJH(I,J)-1,K) &
                       + HBWGT4(I,J)*CC3d(IIH(I,J)+1,JJH(I,J)+1,K)
          ENDIF
        ENDDO
      ENDDO

      DO J=NJTS,MIN(NJTE,NJDE-1)
       IF(MOD(J,2) .NE. 0)THEN
         CIN=0.;PIN=0.;Y2=0;PIO=0.;PTMP=0.;COUT=0.;DUM1=0.;DUM2=0. 
         DO K=NKDS+1,NKDE                    
           PIN(K-1) = EXP((ALOG(PSTD(NKDE-K+1))+ALOG(PSTD(NKDE-K+2)))*0.5)
           CIN(K-1) = C3d(I,J,NKDE-K+1)
         ENDDO
         Y2(1   )=0.
         Y2(NKDE-1)=0.
         DO K=NKDS,NKDE                         
           PTMP(K) = ETA1(K)*PDTOP + ETA2(K)*PD(I,J) + PTOP
         ENDDO
         DO K=NKDS,NKDE-1                        
           PIO(K) = EXP((ALOG(PTMP(K))+ALOG(PTMP(K+1)))*0.5)
         ENDDO
         IF(PTMP(1) .GE. PSTD(1))THEN           
           PIN(NKDE-1) = PIO(1)                 
           WRITE(0,*)'WARNING: NESTED DOMAIN PRESSURE AT LOWEST LEVEL HIGHER THAN PSTD'
           WRITE(0,*)'I,J,PIO(1),PSTD(1)',I,J,PIO(1),PSTD(1)
         ENDIF

         CALL SPLINE2(I,J,JTB,NKDE-1,PIN,CIN,Y2,NKDE-1,PIO,COUT,DUM1,DUM2)  

         DO K=1,NKDE-1
           CWK1(I,J,K)= COUT(K)  
         ENDDO
       ELSE
         DO K=NKDS,NKDE-1
          CWK1(I,J,K)=0.0
         ENDDO
       ENDIF
      ENDDO

      DO J = NJTS,MIN(NJTE,NJDE-1)
       DO K = NKDS,NKDE-1
         ntemp_b(i,j,k)     = CWK1(I,J,K)
         ntemp_bt(i,j,k)    = 0.0
       END DO
      END DO

    ENDIF NMM_XS




    NMM_XE: IF(NITE-1 .EQ. NIDE-1)THEN

     I = NIDE-1
      DO K=NKDS,NKDE-1                
        DO J = NJTS,MIN(NJTE,NJDE-1)
          IF(MOD(JJH(I,J),2) .NE. 0)THEN      
            C3d(I,J,K) = HBWGT1(I,J)*CC3d(IIH(I,J),  JJH(I,J)  ,K) &
                       + HBWGT2(I,J)*CC3d(IIH(I,J)+1,JJH(I,J)  ,K) &
                       + HBWGT3(I,J)*CC3d(IIH(I,J),  JJH(I,J)-1,K) &
                       + HBWGT4(I,J)*CC3d(IIH(I,J),  JJH(I,J)+1,K)
          ELSE
            C3d(I,J,K) = HBWGT1(I,J)*CC3d(IIH(I,J),  JJH(I,J)  ,K) &
                       + HBWGT2(I,J)*CC3d(IIH(I,J)+1,JJH(I,J)  ,K) &
                       + HBWGT3(I,J)*CC3d(IIH(I,J)+1,JJH(I,J)-1,K) &
                       + HBWGT4(I,J)*CC3d(IIH(I,J)+1,JJH(I,J)+1,K)
          ENDIF
        ENDDO
      ENDDO

     DO J=NJTS,MIN(NJTE,NJDE-1)
      IF(MOD(J,2) .NE. 0)THEN
         CIN=0.;PIN=0.;Y2=0;PIO=0.;PTMP=0.;COUT=0.;DUM1=0.;DUM2=0. 
         DO K=NKDS+1,NKDE                    
           PIN(K-1) = EXP((ALOG(PSTD(NKDE-K+1))+ALOG(PSTD(NKDE-K+2)))*0.5)
           CIN(K-1) = C3d(I,J,NKDE-K+1)
         ENDDO
         Y2(1   )=0.
         Y2(NKDE-1)=0.
         DO K=NKDS,NKDE                         
           PTMP(K) = ETA1(K)*PDTOP + ETA2(K)*PD(I,J) + PTOP
         ENDDO
         DO K=NKDS,NKDE-1                        
           PIO(K) = EXP((ALOG(PTMP(K))+ALOG(PTMP(K+1)))*0.5)
         ENDDO
         IF(PTMP(1) .GE. PSTD(1))THEN           
           PIN(NKDE-1) = PIO(1)                 
           WRITE(0,*)'WARNING: NESTED DOMAIN PRESSURE AT LOWEST LEVEL HIGHER THAN PSTD'
           WRITE(0,*)'I,J,PIO(1),PSTD(1)',I,J,PIO(1),PSTD(1)
         ENDIF

         CALL SPLINE2(I,J,JTB,NKDE-1,PIN,CIN,Y2,NKDE-1,PIO,COUT,DUM1,DUM2)  

         DO K=1,NKDE-1
           CWK2(I,J,K)= COUT(K)  
         ENDDO
      ELSE
         DO K=NKDS,NKDE-1
           CWK2(I,J,K)=0.0
         ENDDO
      ENDIF
     ENDDO

       DO J = NJTS,MIN(NJTE,NJDE-1)
        DO K = NKDS,MIN(NKTE,NKDE-1)
          ntemp_b(i,j,k)     = CWK2(I,J,K)
          ntemp_bt(i,j,k)    = 0.0
        END DO
       END DO

    ENDIF NMM_XE



    NMM_YS: IF(NJTS .EQ. NJDS)THEN

     J = NJDS
      DO K=NKDS,NKDE-1
       DO I = NITS,MIN(NITE,NIDE-1)
          IF(MOD(JJH(I,J),2) .NE. 0)THEN      
            C3d(I,J,K) = HBWGT1(I,J)*CC3d(IIH(I,J),  JJH(I,J)  ,K) &
                       + HBWGT2(I,J)*CC3d(IIH(I,J)+1,JJH(I,J)  ,K) &
                       + HBWGT3(I,J)*CC3d(IIH(I,J),  JJH(I,J)-1,K) &
                       + HBWGT4(I,J)*CC3d(IIH(I,J),  JJH(I,J)+1,K)
          ELSE
            C3d(I,J,K) = HBWGT1(I,J)*CC3d(IIH(I,J),  JJH(I,J)  ,K) &
                       + HBWGT2(I,J)*CC3d(IIH(I,J)+1,JJH(I,J)  ,K) &
                       + HBWGT3(I,J)*CC3d(IIH(I,J)+1,JJH(I,J)-1,K) &
                       + HBWGT4(I,J)*CC3d(IIH(I,J)+1,JJH(I,J)+1,K)

          ENDIF
        ENDDO
      ENDDO

     DO I=NITS,MIN(NITE,NIDE-1)
         CIN=0.;PIN=0.;Y2=0;PIO=0.;PTMP=0.;COUT=0.;DUM1=0.;DUM2=0. 
         DO K=NKDS+1,NKDE                    
           PIN(K-1) = EXP((ALOG(PSTD(NKDE-K+1))+ALOG(PSTD(NKDE-K+2)))*0.5)
           CIN(K-1) = C3d(I,J,NKDE-K+1)
         ENDDO
         Y2(1   )=0.
         Y2(NKDE-1)=0.
         DO K=NKDS,NKDE                         
           PTMP(K) = ETA1(K)*PDTOP + ETA2(K)*PD(I,J) + PTOP
         ENDDO
         DO K=NKDS,NKDE-1                        
           PIO(K) = EXP((ALOG(PTMP(K))+ALOG(PTMP(K+1)))*0.5)
         ENDDO
         IF(PTMP(1) .GE. PSTD(1))THEN           
           PIN(NKDE-1) = PIO(1)                 
           WRITE(0,*)'WARNING: NESTED DOMAIN PRESSURE AT LOWEST LEVEL HIGHER THAN PSTD'
           WRITE(0,*)'I,J,PIO(1),PSTD(1)',I,J,PIO(1),PSTD(1)
         ENDIF

         CALL SPLINE2(I,J,JTB,NKDE-1,PIN,CIN,Y2,NKDE-1,PIO,COUT,DUM1,DUM2)  

         DO K=1,NKDE-1
           CWK3(I,J,K)= COUT(K)  
         ENDDO
     ENDDO

     DO K = NKDS,NKDE-1
      DO I = NITS,MIN(NITE,NIDE-1)
        ntemp_b(i,J,K)     = CWK3(I,J,K)
        ntemp_bt(i,J,K)    = 0.0
      ENDDO
      ENDDO

    ENDIF NMM_YS



    NMM_YE: IF(NJTE-1 .EQ. NJDE-1)THEN

     J = NJDE-1
      DO K=NKDS,NKDE-1
        DO I = NITS,MIN(NITE,NIDE-1)
          IF(MOD(JJH(I,J),2) .NE. 0)THEN      
            C3d(I,J,K) = HBWGT1(I,J)*CC3d(IIH(I,J),  JJH(I,J)  ,K) &
                       + HBWGT2(I,J)*CC3d(IIH(I,J)+1,JJH(I,J)  ,K) &
                       + HBWGT3(I,J)*CC3d(IIH(I,J),  JJH(I,J)-1,K) &
                       + HBWGT4(I,J)*CC3d(IIH(I,J),  JJH(I,J)+1,K)
          ELSE
            C3d(I,J,K) = HBWGT1(I,J)*CC3d(IIH(I,J),  JJH(I,J)  ,K) &
                       + HBWGT2(I,J)*CC3d(IIH(I,J)+1,JJH(I,J)  ,K) &
                       + HBWGT3(I,J)*CC3d(IIH(I,J)+1,JJH(I,J)-1,K) &
                       + HBWGT4(I,J)*CC3d(IIH(I,J)+1,JJH(I,J)+1,K)

          ENDIF
        ENDDO
      ENDDO

     DO I=NITS,MIN(NITE,NIDE-1)
         CIN=0.;PIN=0.;Y2=0;PIO=0.;PTMP=0.;COUT=0.;DUM1=0.;DUM2=0. 
         DO K=NKDS+1,NKDE                    
           PIN(K-1) = EXP((ALOG(PSTD(NKDE-K+1))+ALOG(PSTD(NKDE-K+2)))*0.5)
           CIN(K-1) = C3d(I,J,NKDE-K+1)
         ENDDO
         Y2(1   )=0.
         Y2(NKDE-1)=0.
         DO K=NKDS,NKDE                         
           PTMP(K) = ETA1(K)*PDTOP + ETA2(K)*PD(I,J) + PTOP
         ENDDO
         DO K=NKDS,NKDE-1                        
           PIO(K) = EXP((ALOG(PTMP(K))+ALOG(PTMP(K+1)))*0.5)
         ENDDO
         IF(PTMP(1) .GE. PSTD(1))THEN           
           PIN(NKDE-1) = PIO(1)                 
           WRITE(0,*)'WARNING: NESTED DOMAIN PRESSURE AT LOWEST LEVEL HIGHER THAN PSTD'
           WRITE(0,*)'I,J,PIO(1),PSTD(1)',I,J,PIO(1),PSTD(1)
         ENDIF

         CALL SPLINE2(I,J,JTB,NKDE-1,PIN,CIN,Y2,NKDE-1,PIO,COUT,DUM1,DUM2)  

         DO K=1,NKDE-1
           CWK4(I,J,K)= COUT(K)  
         ENDDO
     ENDDO

     DO K = NKDS,NKDE-1
      DO I = NITS,MIN(NITE,NIDE-1)
        ntemp_b(i,J,K)     = CWK4(I,J,K)
        ntemp_bt(i,J,K)    = 0.0
      END DO
      END DO

    ENDIF NMM_YE

  END SUBROUTINE nmm_bdy_scalar



 SUBROUTINE SPLINE2(I,J,JTBX,NOLD,XOLD,YOLD,Y2,NNEW,XNEW,YNEW,P,Q)
























      IMPLICIT NONE

      INTEGER,INTENT(IN) :: I,J,JTBX,NNEW,NOLD
      REAL,DIMENSION(JTBX),INTENT(IN) :: XNEW,XOLD,YOLD
      REAL,DIMENSION(JTBX),INTENT(INOUT) :: P,Q,Y2
      REAL,DIMENSION(JTBX),INTENT(OUT) :: YNEW

      INTEGER :: II,JJ,K,K1,K2,KOLD,NOLDM1
      REAL :: AK,BK,CK,DEN,DX,DXC,DXL,DXR,DYDXL,DYDXR                 &
             ,RDX,RTDXC,X,XK,XSQ,Y2K,Y2KP1




      II=9999
      JJ=9999
      IF(I.eq.II.and.J.eq.JJ)THEN
        WRITE(0,*)'DEBUG in SPLINE2: I,J',I,J
        WRITE(0,*)'DEBUG in SPLINE2:HSO= ',xnew(1:nold)
        DO K=1,NOLD
         WRITE(0,*)'DEBUG in SPLINE2:L,ZETAI,PINTI= ' &
                        ,K,YOLD(K),XOLD(K)
        ENDDO
      ENDIF

      NOLDM1=NOLD-1

      DXL=XOLD(2)-XOLD(1)
      DXR=XOLD(3)-XOLD(2)
      DYDXL=(YOLD(2)-YOLD(1))/DXL
      DYDXR=(YOLD(3)-YOLD(2))/DXR
      RTDXC=0.5/(DXL+DXR)

      P(1)= RTDXC*(6.*(DYDXR-DYDXL)-DXL*Y2(1))
      Q(1)=-RTDXC*DXR

      IF(NOLD.EQ.3)GO TO 150

      K=3

  100 DXL=DXR
      DYDXL=DYDXR
      DXR=XOLD(K+1)-XOLD(K)
      DYDXR=(YOLD(K+1)-YOLD(K))/DXR
      DXC=DXL+DXR
      DEN=1./(DXL*Q(K-2)+DXC+DXC)

      P(K-1)= DEN*(6.*(DYDXR-DYDXL)-DXL*P(K-2))
      Q(K-1)=-DEN*DXR

      K=K+1
      IF(K.LT.NOLD)GO TO 100

  150 K=NOLDM1

  200 Y2(K)=P(K-1)+Q(K-1)*Y2(K+1)

      K=K-1
      IF(K.GT.1)GO TO 200

      K1=1

  300 XK=XNEW(K1)

      DO 400 K2=2,NOLD

      IF(XOLD(K2).GT.XK)THEN
        KOLD=K2-1
        GO TO 450
      ENDIF

  400 CONTINUE

      YNEW(K1)=YOLD(NOLD)
      GO TO 600

  450 IF(K1.EQ.1)GO TO 500
      IF(K.EQ.KOLD)GO TO 550

  500 K=KOLD

      Y2K=Y2(K)
      Y2KP1=Y2(K+1)
      DX=XOLD(K+1)-XOLD(K)
      RDX=1./DX

      AK=.1666667*RDX*(Y2KP1-Y2K)
      BK=0.5*Y2K
      CK=RDX*(YOLD(K+1)-YOLD(K))-.1666667*DX*(Y2KP1+Y2K+Y2K)

  550 X=XK-XOLD(K)
      XSQ=X*X

      YNEW(K1)=AK*XSQ*X+BK*XSQ+CK*X+YOLD(K)



      IF(I.eq.II.and.J.eq.JJ)THEN
        WRITE(0,*) 'DEBUG:: k1,xnew(k1),ynew(k1): ', K1,XNEW(k1),YNEW(k1)
      ENDIF 


  600 K1=K1+1
      IF(K1.LE.NNEW)GO TO 300

      RETURN

      END SUBROUTINE SPLINE2





  SUBROUTINE interp_h_nmm (cfld,                                 &  
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  
                           imask,                                &  
                           xstag, ystag,                         &  
                           ipos, jpos,                           &  
                           nri, nrj,                             &  
                           CII, IIH, CJJ, JJH, CBWGT1, HBWGT1,   &  
                           CBWGT2, HBWGT2, CBWGT3, HBWGT3,       &  
                           CBWGT4, HBWGT4                        )  
     USE module_timing
     IMPLICIT NONE

     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, cjms:cjme, ckms:ckme ) :: cfld
     REAL, DIMENSION ( nims:nime, njms:njme, nkms:nkme ) :: nfld
     REAL,    DIMENSION ( cims:cime, cjms:cjme ), INTENT(IN) :: CBWGT1,CBWGT2,CBWGT3,CBWGT4    
     REAL,    DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: HBWGT1,HBWGT2,HBWGT3,HBWGT4
     INTEGER, DIMENSION ( cims:cime, cjms:cjme ), INTENT(IN) :: CII,CJJ                        
     INTEGER, DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: IIH,JJH
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask


     INTEGER i,j,k



    DO J=NJTS,MIN(NJTE,NJDE-1)
     DO I=NITS,MIN(NITE,NIDE-1)
       IF(IIH(i,j).LT.(CIDS-shw) .OR. IIH(i,j).GT.(CIDE+shw)) &
           CALL wrf_error_fatal3("",3010,&
'hpoints:check domain bounds along x' )
       IF(JJH(i,j).LT.(CJDS-shw) .OR. JJH(i,j).GT.(CJDE+shw)) &
           CALL wrf_error_fatal3("",3013,&
'hpoints:check domain bounds along y' )
     ENDDO
    ENDDO















     DO K=NKDS,NKDE
       DO J=NJTS,MIN(NJTE,NJDE-1)
        DO I=NITS,MIN(NITE,NIDE-1)
         IF(IMASK(I,J) .NE. 1)THEN

           IF(MOD(JJH(I,J),2) .NE. 0)THEN    
               NFLD(I,J,K) = HBWGT1(I,J)*CFLD(IIH(I,J),  JJH(I,J)  ,K) &
                           + HBWGT2(I,J)*CFLD(IIH(I,J)+1,JJH(I,J)  ,K) &
                           + HBWGT3(I,J)*CFLD(IIH(I,J),  JJH(I,J)-1,K) &
                           + HBWGT4(I,J)*CFLD(IIH(I,J),  JJH(I,J)+1,K)
           ELSE
               NFLD(I,J,K) = HBWGT1(I,J)*CFLD(IIH(I,J),  JJH(I,J)  ,K) &
                           + HBWGT2(I,J)*CFLD(IIH(I,J)+1,JJH(I,J)  ,K) &
                           + HBWGT3(I,J)*CFLD(IIH(I,J)+1,JJH(I,J)-1,K) &
                           + HBWGT4(I,J)*CFLD(IIH(I,J)+1,JJH(I,J)+1,K)
           ENDIF

         ENDIF
        ENDDO
       ENDDO
     ENDDO

  END SUBROUTINE interp_h_nmm

  SUBROUTINE interp_v_nmm (cfld,                                 &  
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  
                           imask,                                &  
                           xstag, ystag,                         &  
                           ipos, jpos,                           &  
                           nri, nrj,                             &  
                           CII, IIV, CJJ, JJV, CBWGT1, VBWGT1,   &  
                           CBWGT2, VBWGT2, CBWGT3, VBWGT3,       &  
                           CBWGT4, VBWGT4                        )  
     USE module_timing
     IMPLICIT NONE

     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, cjms:cjme, ckms:ckme ) :: cfld
     REAL, DIMENSION ( nims:nime, njms:njme, nkms:nkme ) :: nfld
     REAL,    DIMENSION ( cims:cime, cjms:cjme ), INTENT(IN) :: CBWGT1,CBWGT2,CBWGT3,CBWGT4    
     REAL,    DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: VBWGT1,VBWGT2,VBWGT3,VBWGT4
     INTEGER, DIMENSION ( cims:cime, cjms:cjme ), INTENT(IN) :: CII,CJJ                        
     INTEGER, DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: IIV,JJV
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask


     INTEGER i,j,k





    DO J=NJTS,MIN(NJTE,NJDE-1)
     DO I=NITS,MIN(NITE,NIDE-1)
       IF(IIV(i,j).LT.(CIDS-shw) .OR. IIV(i,j).GT.(CIDE+shw)) &
           CALL wrf_error_fatal3("",3104,&
'vpoints:check domain bounds along x' )
       IF(JJV(i,j).LT.(CJDS-shw) .OR. JJV(i,j).GT.(CJDE+shw)) &
           CALL wrf_error_fatal3("",3107,&
'vpoints:check domain bounds along y' )
     ENDDO
    ENDDO















     DO K=NKDS,NKDE
       DO J=NJTS,MIN(NJTE,NJDE-1)
        DO I=NITS,MIN(NITE,NIDE-1)
         IF(IMASK(I,J) .NE. 1)THEN

            IF(MOD(JJV(I,J),2) .NE. 0)THEN    
                NFLD(I,J,K) = VBWGT1(I,J)*CFLD(IIV(I,J),  JJV(I,J)  ,K) &
                            + VBWGT2(I,J)*CFLD(IIV(I,J)+1,JJV(I,J)  ,K) &
                            + VBWGT3(I,J)*CFLD(IIV(I,J)+1,JJV(I,J)-1,K) &
                            + VBWGT4(I,J)*CFLD(IIV(I,J)+1,JJV(I,J)+1,K)
            ELSE
                NFLD(I,J,K) = VBWGT1(I,J)*CFLD(IIV(I,J),  JJV(I,J)  ,K) &
                            + VBWGT2(I,J)*CFLD(IIV(I,J)+1,JJV(I,J)  ,K) &
                            + VBWGT3(I,J)*CFLD(IIV(I,J),  JJV(I,J)-1,K) &
                            + VBWGT4(I,J)*CFLD(IIV(I,J),  JJV(I,J)+1,K)
            ENDIF

         ENDIF
        ENDDO
       ENDDO
     ENDDO

  END SUBROUTINE interp_v_nmm






  SUBROUTINE interp_hnear_nmm (cfld,                                 &  
                               cids, cide, ckds, ckde, cjds, cjde,   &
                               cims, cime, ckms, ckme, cjms, cjme,   &
                               cits, cite, ckts, ckte, cjts, cjte,   &
                               nfld,                                 &  
                               nids, nide, nkds, nkde, njds, njde,   &
                               nims, nime, nkms, nkme, njms, njme,   &
                               nits, nite, nkts, nkte, njts, njte,   &
                               shw,                                  &  
                               imask,                                &  
                               xstag, ystag,                         &  
                               ipos, jpos,                           &  
                               nri, nrj,                             &  
                               CII, IIH, CJJ, JJH, CBWGT1, HBWGT1,   &  
                               CBWGT2, HBWGT2, CBWGT3, HBWGT3,       &  
                               CBWGT4, HBWGT4                        )  
     USE module_timing
     IMPLICIT NONE

     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, cjms:cjme, ckms:ckme ) :: cfld
     REAL, DIMENSION ( nims:nime, njms:njme, nkms:nkme ) :: nfld
     REAL,    DIMENSION ( cims:cime, cjms:cjme ), INTENT(IN) :: CBWGT1,CBWGT2,CBWGT3,CBWGT4    
     REAL,    DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: HBWGT1,HBWGT2,HBWGT3,HBWGT4
     INTEGER, DIMENSION ( cims:cime, cjms:cjme ), INTENT(IN) :: CII,CJJ                        
     INTEGER, DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: IIH,JJH
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask



     LOGICAL  FLIP
     INTEGER  i,j,k,n
     REAL     SUM,AMAXVAL
     REAL,    DIMENSION (4, nims:nime, njms:njme ) :: NBWGT
















     DO J=NJTS,MIN(NJTE,NJDE-1)
      DO I=NITS,MIN(NITE,NIDE-1)
       IF(IMASK(I,J) .NE. 1)THEN
         NBWGT(1,I,J)=HBWGT1(I,J)
         NBWGT(2,I,J)=HBWGT2(I,J)
         NBWGT(3,I,J)=HBWGT3(I,J)
         NBWGT(4,I,J)=HBWGT4(I,J)
       ENDIF
      ENDDO
     ENDDO

     DO J=NJTS,MIN(NJTE,NJDE-1)
      DO I=NITS,MIN(NITE,NIDE-1)
       IF(IMASK(I,J) .NE. 1)THEN

          AMAXVAL=0.
          DO N=1,4
            AMAXVAL=amax1(NBWGT(N,I,J),AMAXVAL)
          ENDDO

          FLIP=.TRUE.
          SUM=0.0
          DO N=1,4
             IF(AMAXVAL .EQ. NBWGT(N,I,J) .AND. FLIP)THEN
               NBWGT(N,I,J)=1.0
               FLIP=.FALSE.
             ELSE
               NBWGT(N,I,J)=0.0
             ENDIF
             SUM=SUM+NBWGT(N,I,J)
             IF(SUM .GT. 1.0)CALL wrf_error_fatal3("",3245,&
"horizontal interp error - interp_hnear_nmm" )
          ENDDO

       ENDIF
      ENDDO
     ENDDO

     DO K=NKDS,NKDE
       DO J=NJTS,MIN(NJTE,NJDE-1)
        DO I=NITS,MIN(NITE,NIDE-1)
         IF(IMASK(I,J) .NE. 1)THEN
            IF(MOD(JJH(I,J),2) .NE. 0)THEN    
                NFLD(I,J,K) = NBWGT(1,I,J)*CFLD(IIH(I,J),  JJH(I,J)  ,K) &
                            + NBWGT(2,I,J)*CFLD(IIH(I,J)+1,JJH(I,J)  ,K) &
                            + NBWGT(3,I,J)*CFLD(IIH(I,J),  JJH(I,J)-1,K) &
                            + NBWGT(4,I,J)*CFLD(IIH(I,J),  JJH(I,J)+1,K)
            ELSE
                NFLD(I,J,K) = NBWGT(1,I,J)*CFLD(IIH(I,J),  JJH(I,J)  ,K) &
                            + NBWGT(2,I,J)*CFLD(IIH(I,J)+1,JJH(I,J)  ,K) &
                            + NBWGT(3,I,J)*CFLD(IIH(I,J)+1,JJH(I,J)-1,K) &
                            + NBWGT(4,I,J)*CFLD(IIH(I,J)+1,JJH(I,J)+1,K)
            ENDIF
         ENDIF
        ENDDO
       ENDDO
     ENDDO

  END SUBROUTINE interp_hnear_nmm






  SUBROUTINE interp_hnear_ikj_nmm (cfld,                                 &  
                               cids, cide, ckds, ckde, cjds, cjde,   &
                               cims, cime, ckms, ckme, cjms, cjme,   &
                               cits, cite, ckts, ckte, cjts, cjte,   &
                               nfld,                                 &  
                               nids, nide, nkds, nkde, njds, njde,   &
                               nims, nime, nkms, nkme, njms, njme,   &
                               nits, nite, nkts, nkte, njts, njte,   &
                               shw,                                  &  
                               imask,                                &  
                               xstag, ystag,                         &  
                               ipos, jpos,                           &  
                               nri, nrj,                             &  
                               CII, IIH, CJJ, JJH, CBWGT1, HBWGT1,   &  
                               CBWGT2, HBWGT2, CBWGT3, HBWGT3,       &  
                               CBWGT4, HBWGT4                        )  
     USE module_timing
     IMPLICIT NONE

     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
     REAL,    DIMENSION ( cims:cime, cjms:cjme ), INTENT(IN) :: CBWGT1,CBWGT2,CBWGT3,CBWGT4    
     REAL,    DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: HBWGT1,HBWGT2,HBWGT3,HBWGT4
     INTEGER, DIMENSION ( cims:cime, cjms:cjme ), INTENT(IN) :: CII,CJJ                        
     INTEGER, DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: IIH,JJH
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask



     LOGICAL  FLIP
     INTEGER  i,j,k,n
     REAL     SUM,AMAXVAL
     REAL,    DIMENSION (4, nims:nime, njms:njme ) :: NBWGT
















     DO J=NJTS,MIN(NJTE,NJDE-1)
      DO I=NITS,MIN(NITE,NIDE-1)
       IF(IMASK(I,J) .NE. 1)THEN
         NBWGT(1,I,J)=HBWGT1(I,J)
         NBWGT(2,I,J)=HBWGT2(I,J)
         NBWGT(3,I,J)=HBWGT3(I,J)
         NBWGT(4,I,J)=HBWGT4(I,J)
       ENDIF
      ENDDO
     ENDDO

     DO J=NJTS,MIN(NJTE,NJDE-1)
      DO I=NITS,MIN(NITE,NIDE-1)
       IF(IMASK(I,J) .NE. 1)THEN

          AMAXVAL=0.
          DO N=1,4
            AMAXVAL=amax1(NBWGT(N,I,J),AMAXVAL)
          ENDDO

          FLIP=.TRUE.
          SUM=0.0
          DO N=1,4
             IF(AMAXVAL .EQ. NBWGT(N,I,J) .AND. FLIP)THEN
               NBWGT(N,I,J)=1.0
               FLIP=.FALSE.
             ELSE
               NBWGT(N,I,J)=0.0
             ENDIF
             SUM=SUM+NBWGT(N,I,J)
             IF(SUM .GT. 1.0)CALL wrf_error_fatal3("",3370,&
"horizontal interp error - interp_hnear_nmm" )
          ENDDO

       ENDIF
      ENDDO
     ENDDO

     DO J=NJTS,MIN(NJTE,NJDE-1)
       DO K=NKDS,NKDE
        DO I=NITS,MIN(NITE,NIDE-1)
         IF(IMASK(I,J) .NE. 1)THEN
            IF(MOD(JJH(I,J),2) .NE. 0)THEN    
                NFLD(I,K,J) = NBWGT(1,I,J)*CFLD(IIH(I,J),  K,JJH(I,J)  ) &
                            + NBWGT(2,I,J)*CFLD(IIH(I,J)+1,K,JJH(I,J)  ) &
                            + NBWGT(3,I,J)*CFLD(IIH(I,J),  K,JJH(I,J)-1) &
                            + NBWGT(4,I,J)*CFLD(IIH(I,J),  K,JJH(I,J)+1)
            ELSE
                NFLD(I,K,J) = NBWGT(1,I,J)*CFLD(IIH(I,J),  K,JJH(I,J)  ) &
                            + NBWGT(2,I,J)*CFLD(IIH(I,J)+1,K,JJH(I,J)  ) &
                            + NBWGT(3,I,J)*CFLD(IIH(I,J)+1,K,JJH(I,J)-1) &
                            + NBWGT(4,I,J)*CFLD(IIH(I,J)+1,K,JJH(I,J)+1)
            ENDIF
         ENDIF
        ENDDO
       ENDDO
     ENDDO

  END SUBROUTINE interp_hnear_ikj_nmm





  SUBROUTINE interp_int_hnear_nmm (cfld,                                 &  
                                   cids, cide, ckds, ckde, cjds, cjde,   &
                                   cims, cime, ckms, ckme, cjms, cjme,   &
                                   cits, cite, ckts, ckte, cjts, cjte,   &
                                   nfld,                                 &  
                                   nids, nide, nkds, nkde, njds, njde,   &
                                   nims, nime, nkms, nkme, njms, njme,   &
                                   nits, nite, nkts, nkte, njts, njte,   &
                                   shw,                                  &  
                                   imask,                                &  
                                   xstag, ystag,                         &  
                                   ipos, jpos,                           &  
                                   nri, nrj,                             &  
                                   CII, IIH, CJJ, JJH, CBWGT1, HBWGT1,   &  
                                   CBWGT2, HBWGT2, CBWGT3, HBWGT3,       &  
                                   CBWGT4, HBWGT4                        )  
     USE module_timing
     IMPLICIT NONE

     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     INTEGER, DIMENSION ( cims:cime, cjms:cjme, ckms:ckme ) :: cfld
     INTEGER, DIMENSION ( nims:nime, njms:njme, nkms:nkme ) :: nfld
     REAL,    DIMENSION ( cims:cime, cjms:cjme ), INTENT(IN) :: CBWGT1,CBWGT2,CBWGT3,CBWGT4    
     REAL,    DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: HBWGT1,HBWGT2,HBWGT3,HBWGT4
     INTEGER, DIMENSION ( cims:cime, cjms:cjme ), INTENT(IN) :: CII,CJJ                        
     INTEGER, DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: IIH,JJH
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask



     LOGICAL  FLIP 
     INTEGER  i,j,k,n
     REAL     SUM,AMAXVAL
     REAL,    DIMENSION (4, nims:nime, njms:njme ) :: NBWGT
















     DO J=NJTS,MIN(NJTE,NJDE-1)
       DO I=NITS,MIN(NITE,NIDE-1)
        IF(IMASK(I,J) .NE. 1)THEN
          NBWGT(1,I,J)=HBWGT1(I,J)
          NBWGT(2,I,J)=HBWGT2(I,J)
          NBWGT(3,I,J)=HBWGT3(I,J)
          NBWGT(4,I,J)=HBWGT4(I,J)
        ENDIF
       ENDDO
     ENDDO

     DO J=NJTS,MIN(NJTE,NJDE-1)
      DO I=NITS,MIN(NITE,NIDE-1)
       IF(IMASK(I,J) .NE. 1)THEN

          AMAXVAL=0.
          DO N=1,4
            AMAXVAL=amax1(NBWGT(N,I,J),AMAXVAL)
          ENDDO

          FLIP=.TRUE.
          SUM=0.0
          DO N=1,4
             IF(AMAXVAL .EQ. NBWGT(N,I,J) .AND. FLIP)THEN
               NBWGT(N,I,J)=1.0
               FLIP=.FALSE.
             ELSE
               NBWGT(N,I,J)=0.0
             ENDIF
             SUM=SUM+NBWGT(N,I,J)
             IF(SUM .GT. 1.0)CALL wrf_error_fatal3("",3494,&
"horizontal interp error - interp_hnear_nmm" )
          ENDDO

       ENDIF
      ENDDO
     ENDDO

     DO J=NJTS,MIN(NJTE,NJDE-1)
       DO K=NKTS,NKTS
        DO I=NITS,MIN(NITE,NIDE-1)
         IF(IMASK(I,J) .NE. 1)THEN
           IF(MOD(JJH(I,J),2) .NE. 0)THEN    
               NFLD(I,J,K) = NBWGT(1,I,J)*CFLD(IIH(I,J),  JJH(I,J)  ,K) &
                           + NBWGT(2,I,J)*CFLD(IIH(I,J)+1,JJH(I,J)  ,K) &
                           + NBWGT(3,I,J)*CFLD(IIH(I,J),  JJH(I,J)-1,K) &
                           + NBWGT(4,I,J)*CFLD(IIH(I,J),  JJH(I,J)+1,K)
           ELSE
               NFLD(I,J,K) = NBWGT(1,I,J)*CFLD(IIH(I,J),  JJH(I,J)  ,K) &
                           + NBWGT(2,I,J)*CFLD(IIH(I,J)+1,JJH(I,J)  ,K) &
                           + NBWGT(3,I,J)*CFLD(IIH(I,J)+1,JJH(I,J)-1,K) &
                           + NBWGT(4,I,J)*CFLD(IIH(I,J)+1,JJH(I,J)+1,K)
           ENDIF
         ENDIF
        ENDDO
       ENDDO
     ENDDO

  END SUBROUTINE interp_int_hnear_nmm



   SUBROUTINE nmm_bdy_hinterp (cfld,                                 &  
                               cids, cide, ckds, ckde, cjds, cjde,   &
                               cims, cime, ckms, ckme, cjms, cjme,   &
                               cits, cite, ckts, ckte, cjts, cjte,   &
                               nfld,                                 &  
                               nids, nide, nkds, nkde, njds, njde,   &
                               nims, nime, nkms, nkme, njms, njme,   &
                               nits, nite, nkts, nkte, njts, njte,   &
                               shw,                                  &  
                               imask,                                &  
                               xstag, ystag,                         &  
                               ipos, jpos,                           &  
                               nri, nrj,                             &  
                               c_bxs,n_bxs,                          &
                               c_bxe,n_bxe,                          &
                               c_bys,n_bys,                          &
                               c_bye,n_bye,                          &
                               c_btxs,n_btxs,                        &
                               c_btxe,n_btxe,                        &
                               c_btys,n_btys,                        &
                               c_btye,n_btye,                        &
                               CTEMP_B,NTEMP_B,                      &  
                               CTEMP_BT,NTEMP_BT,                    &  
                               CII, IIH, CJJ, JJH, CBWGT1, HBWGT1,   &  
                               CBWGT2, HBWGT2, CBWGT3, HBWGT3,       &  
                               CBWGT4, HBWGT4                        )  


     USE module_configure
     USE module_wrf_error

     IMPLICIT NONE


     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj

     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, cjms:cjme, ckms:ckme ) :: cfld
     REAL, DIMENSION ( nims:nime, njms:njme, nkms:nkme ) :: nfld

     REAL, DIMENSION ( cims:cime, cjms:cjme, ckms:ckme ) :: ctemp_b,ctemp_bt
     REAL, DIMENSION ( nims:nime, njms:njme, nkms:nkme ) :: ntemp_b,ntemp_bt

     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask
     REAL,  DIMENSION( * ), INTENT(INOUT) :: c_bxs,n_bxs,c_bxe,n_bxe,c_bys,n_bys,c_bye,n_bye
     REAL,  DIMENSION( * ), INTENT(INOUT) :: c_btxs,n_btxs,c_btxe,n_btxe,c_btys,n_btys,c_btye,n_btye
     REAL,    DIMENSION ( cims:cime, cjms:cjme ), INTENT(IN) :: CBWGT1,CBWGT2,CBWGT3,CBWGT4    
     REAL,    DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: HBWGT1,HBWGT2,HBWGT3,HBWGT4
     INTEGER, DIMENSION ( cims:cime, cjms:cjme ), INTENT(IN) :: CII,CJJ                        
     INTEGER, DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: IIH,JJH



     INTEGER :: i,j,k
     REAL, DIMENSION ( nims:nime, njms:njme, nkms:nkme )    :: cwk1,cwk2,cwk3,cwk4



       NMM_XS: IF(NITS .EQ. NIDS)THEN

        I = NIDS
        DO K = NKDS,NKDE
         DO J = NJTS,MIN(NJTE,NJDE-1)
              IF(MOD(J,2) .NE.0)THEN                
                IF(MOD(JJH(I,J),2) .NE. 0)THEN      
                   CWK1(I,J,K) = HBWGT1(I,J)*CFLD(IIH(I,J),  JJH(I,J)  ,K) &
                               + HBWGT2(I,J)*CFLD(IIH(I,J)+1,JJH(I,J)  ,K) &
                               + HBWGT3(I,J)*CFLD(IIH(I,J),  JJH(I,J)-1,K) &
                               + HBWGT4(I,J)*CFLD(IIH(I,J),  JJH(I,J)+1,K)


                ELSE
                   CWK1(I,J,K) = HBWGT1(I,J)*CFLD(IIH(I,J),  JJH(I,J)  ,K) &
                               + HBWGT2(I,J)*CFLD(IIH(I,J)+1,JJH(I,J)  ,K) &
                               + HBWGT3(I,J)*CFLD(IIH(I,J)+1,JJH(I,J)-1,K) &
                               + HBWGT4(I,J)*CFLD(IIH(I,J)+1,JJH(I,J)+1,K)
                ENDIF
              ELSE
                CWK1(I,J,K) = 0.0      
              ENDIF
              ntemp_b(i,J,K)     = CWK1(I,J,K)
              ntemp_bt(i,J,K)    = 0.0
         END DO
        END DO
       ENDIF NMM_XS



       NMM_XE: IF(NITE-1 .EQ. NIDE-1)THEN

        I = NIDE-1
        DO K = NKDS,NKDE
         DO J = NJTS,MIN(NJTE,NJDE-1)
              IF(MOD(J,2) .NE.0)THEN                
                IF(MOD(JJH(I,J),2) .NE. 0)THEN    
                   CWK2(I,J,K) = HBWGT1(I,J)*CFLD(IIH(I,J),  JJH(I,J)  ,K) &
                               + HBWGT2(I,J)*CFLD(IIH(I,J)+1,JJH(I,J)  ,K) &
                               + HBWGT3(I,J)*CFLD(IIH(I,J),  JJH(I,J)-1,K) &
                               + HBWGT4(I,J)*CFLD(IIH(I,J),  JJH(I,J)+1,K)
                ELSE
                   CWK2(I,J,K) = HBWGT1(I,J)*CFLD(IIH(I,J),  JJH(I,J)  ,K) &
                               + HBWGT2(I,J)*CFLD(IIH(I,J)+1,JJH(I,J)  ,K) &
                               + HBWGT3(I,J)*CFLD(IIH(I,J)+1,JJH(I,J)-1,K) &
                               + HBWGT4(I,J)*CFLD(IIH(I,J)+1,JJH(I,J)+1,K)
                ENDIF
              ELSE
                CWK2(I,J,K) = 0.0      
              ENDIF
              ntemp_b(i,J,K)     = CWK2(I,J,K)
              ntemp_bt(i,J,K)    = 0.0
         END DO
        END DO
       ENDIF NMM_XE



       NMM_YS: IF(NJTS .EQ. NJDS)THEN

        J = NJDS
        DO K = NKDS, NKDE
         DO I = NITS,MIN(NITE,NIDE-1)
              IF(MOD(JJH(I,J),2) .NE. 0)THEN    
                 CWK3(I,J,K) = HBWGT1(I,J)*CFLD(IIH(I,J),  JJH(I,J)  ,K) &
                             + HBWGT2(I,J)*CFLD(IIH(I,J)+1,JJH(I,J)  ,K) &
                             + HBWGT3(I,J)*CFLD(IIH(I,J),  JJH(I,J)-1,K) &
                             + HBWGT4(I,J)*CFLD(IIH(I,J),  JJH(I,J)+1,K)
              ELSE
                 CWK3(I,J,K) = HBWGT1(I,J)*CFLD(IIH(I,J),  JJH(I,J)  ,K) &
                             + HBWGT2(I,J)*CFLD(IIH(I,J)+1,JJH(I,J)  ,K) &
                             + HBWGT3(I,J)*CFLD(IIH(I,J)+1,JJH(I,J)-1,K) &
                             + HBWGT4(I,J)*CFLD(IIH(I,J)+1,JJH(I,J)+1,K)
              ENDIF
              ntemp_b(i,J,K)     = CWK3(I,J,K)
              ntemp_bt(i,J,K)    = 0.0
         END DO
        END DO
       END IF NMM_YS



       NMM_YE: IF(NJTE-1 .EQ. NJDE-1)THEN

        J = NJDE-1
        DO K = NKDS,NKDE
         DO I = NITS,MIN(NITE,NIDE-1)
              IF(MOD(JJH(I,J),2) .NE. 0)THEN    
                 CWK4(I,J,K) = HBWGT1(I,J)*CFLD(IIH(I,J),  JJH(I,J)  ,K) &
                             + HBWGT2(I,J)*CFLD(IIH(I,J)+1,JJH(I,J)  ,K) &
                             + HBWGT3(I,J)*CFLD(IIH(I,J),  JJH(I,J)-1,K) &
                             + HBWGT4(I,J)*CFLD(IIH(I,J),  JJH(I,J)+1,K)
              ELSE
                 CWK4(I,J,K) = HBWGT1(I,J)*CFLD(IIH(I,J),  JJH(I,J)  ,K) &
                             + HBWGT2(I,J)*CFLD(IIH(I,J)+1,JJH(I,J)  ,K) &
                             + HBWGT3(I,J)*CFLD(IIH(I,J)+1,JJH(I,J)-1,K) &
                             + HBWGT4(I,J)*CFLD(IIH(I,J)+1,JJH(I,J)+1,K)

              ENDIF
              ntemp_b(i,J,K)     = CWK4(I,J,K)
              ntemp_bt(i,J,K)    = 0.0
         END DO
        END DO
       END IF NMM_YE

     RETURN

   END SUBROUTINE nmm_bdy_hinterp



   SUBROUTINE nmm_bdy_vinterp ( cfld,                                 &  
                               cids, cide, ckds, ckde, cjds, cjde,   &
                               cims, cime, ckms, ckme, cjms, cjme,   &
                               cits, cite, ckts, ckte, cjts, cjte,   &
                               nfld,                                 &  
                               nids, nide, nkds, nkde, njds, njde,   &
                               nims, nime, nkms, nkme, njms, njme,   &
                               nits, nite, nkts, nkte, njts, njte,   &
                               shw,                                  &  
                               imask,                                &  
                               xstag, ystag,                         &  
                               ipos, jpos,                           &  
                               nri, nrj,                             &  
                               c_bxs,n_bxs,                          &
                               c_bxe,n_bxe,                          &
                               c_bys,n_bys,                          &
                               c_bye,n_bye,                          &
                               c_btxs,n_btxs,                        &
                               c_btxe,n_btxe,                        &
                               c_btys,n_btys,                        &
                               c_btye,n_btye,                        &
                               CTEMP_B,NTEMP_B,                      &  
                               CTEMP_BT,NTEMP_BT,                    &  
                               CII, IIV, CJJ, JJV, CBWGT1, VBWGT1,   &  
                               CBWGT2, VBWGT2, CBWGT3, VBWGT3,       &  
                               CBWGT4, VBWGT4                        )  


     USE module_configure
     USE module_wrf_error

     IMPLICIT NONE


     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj

     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, cjms:cjme, ckms:ckme ) :: cfld
     REAL, DIMENSION ( nims:nime, njms:njme, nkms:nkme ) :: nfld

     REAL, DIMENSION ( cims:cime, cjms:cjme, ckms:ckme ) :: ctemp_b,ctemp_bt
     REAL, DIMENSION ( nims:nime, njms:njme, nkms:nkme ) :: ntemp_b,ntemp_bt

     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask
     REAL,  DIMENSION( * ), INTENT(INOUT) :: c_bxs,n_bxs,c_bxe,n_bxe,c_bys,n_bys,c_bye,n_bye
     REAL,  DIMENSION( * ), INTENT(INOUT) :: c_btxs,n_btxs,c_btxe,n_btxe,c_btys,n_btys,c_btye,n_btye
     REAL,    DIMENSION ( cims:cime, cjms:cjme ), INTENT(IN) :: CBWGT1,CBWGT2,CBWGT3,CBWGT4    
     REAL,    DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: VBWGT1,VBWGT2,VBWGT3,VBWGT4
     INTEGER, DIMENSION ( cims:cime, cjms:cjme ), INTENT(IN) :: CII,CJJ                        
     INTEGER, DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: IIV,JJV



     INTEGER :: i,j,k
     REAL, DIMENSION ( nims:nime, njms:njme, nkms:nkme )    :: cwk1,cwk2,cwk3,cwk4



       NMM_XS: IF(NITS .EQ. NIDS)THEN

        I = NIDS
        DO K = NKDS,NKDE
         DO J = NJTS,MIN(NJTE,NJDE-1)
              IF(MOD(J,2) .EQ.0)THEN                
                IF(MOD(JJV(I,J),2) .NE. 0)THEN      
                   CWK1(I,J,K) = VBWGT1(I,J)*CFLD(IIV(I,J),  JJV(I,J)  ,K) &
                               + VBWGT2(I,J)*CFLD(IIV(I,J)+1,JJV(I,J)  ,K) &
                               + VBWGT3(I,J)*CFLD(IIV(I,J)+1,JJV(I,J)-1,K) &
                               + VBWGT4(I,J)*CFLD(IIV(I,J)+1,JJV(I,J)+1,K)
                ELSE
                   CWK1(I,J,K) = VBWGT1(I,J)*CFLD(IIV(I,J),  JJV(I,J)  ,K) &
                               + VBWGT2(I,J)*CFLD(IIV(I,J)+1,JJV(I,J)  ,K) &
                               + VBWGT3(I,J)*CFLD(IIV(I,J),  JJV(I,J)-1,K) &
                               + VBWGT4(I,J)*CFLD(IIV(I,J),  JJV(I,J)+1,K)
                ENDIF
              ELSE
                CWK1(I,J,K) = 0.0 
              ENDIF
              ntemp_b(i,J,K)     = CWK1(I,J,K)
              ntemp_bt(i,J,K)    = 0.0
         END DO
        END DO
       ENDIF NMM_XS



       NMM_XE: IF(NITE-1 .EQ. NIDE-1)THEN

        I = NIDE-1
        DO K = NKDS,NKDE
         DO J = NJTS,MIN(NJTE,NJDE-1)
              IF(MOD(J,2) .EQ.0)THEN                
                IF(MOD(JJV(I,J),2) .NE. 0)THEN      
                   CWK2(I,J,K) = VBWGT1(I,J)*CFLD(IIV(I,J),  JJV(I,J)  ,K) &
                               + VBWGT2(I,J)*CFLD(IIV(I,J)+1,JJV(I,J)  ,K) &
                               + VBWGT3(I,J)*CFLD(IIV(I,J)+1,JJV(I,J)-1,K) &
                               + VBWGT4(I,J)*CFLD(IIV(I,J)+1,JJV(I,J)+1,K)
                ELSE
                   CWK2(I,J,K) = VBWGT1(I,J)*CFLD(IIV(I,J),  JJV(I,J)  ,K) &
                               + VBWGT2(I,J)*CFLD(IIV(I,J)+1,JJV(I,J)  ,K) &
                               + VBWGT3(I,J)*CFLD(IIV(I,J),  JJV(I,J)-1,K) &
                               + VBWGT4(I,J)*CFLD(IIV(I,J),  JJV(I,J)+1,K)
                ENDIF
              ELSE
                CWK2(I,J,K) = 0.0      
              ENDIF
              ntemp_b(i,J,K)     = CWK2(I,J,K)
              ntemp_bt(i,J,K)    = 0.0
         END DO
        END DO
       ENDIF NMM_XE



       NMM_YS: IF(NJTS .EQ. NJDS)THEN

        J = NJDS
        DO K = NKDS, NKDE
         DO I = NITS,MIN(NITE,NIDE-2)     
              IF(MOD(JJV(I,J),2) .NE. 0)THEN    
                 CWK3(I,J,K) = VBWGT1(I,J)*CFLD(IIV(I,J),  JJV(I,J)  ,K) &
                             + VBWGT2(I,J)*CFLD(IIV(I,J)+1,JJV(I,J)  ,K) &
                             + VBWGT3(I,J)*CFLD(IIV(I,J)+1,JJV(I,J)-1,K) &
                             + VBWGT4(I,J)*CFLD(IIV(I,J)+1,JJV(I,J)+1,K)
              ELSE
                 CWK3(I,J,K) = VBWGT1(I,J)*CFLD(IIV(I,J),  JJV(I,J)  ,K) &
                             + VBWGT2(I,J)*CFLD(IIV(I,J)+1,JJV(I,J)  ,K) &
                             + VBWGT3(I,J)*CFLD(IIV(I,J),  JJV(I,J)-1,K) &
                             + VBWGT4(I,J)*CFLD(IIV(I,J),  JJV(I,J)+1,K)
              ENDIF
              ntemp_b(i,J,K)     = CWK3(I,J,K)
              ntemp_bt(i,J,K)    = 0.0
         END DO
        END DO
       END IF NMM_YS



       NMM_YE: IF(NJTE-1 .EQ. NJDE-1)THEN

        J = NJDE-1
        DO K = NKDS,NKDE
         DO I = NITS,MIN(NITE,NIDE-2)   
              IF(MOD(JJV(I,J),2) .NE. 0)THEN    
                 CWK4(I,J,K) = VBWGT1(I,J)*CFLD(IIV(I,J),  JJV(I,J)  ,K) &
                             + VBWGT2(I,J)*CFLD(IIV(I,J)+1,JJV(I,J)  ,K) &
                             + VBWGT3(I,J)*CFLD(IIV(I,J)+1,JJV(I,J)-1,K) &
                             + VBWGT4(I,J)*CFLD(IIV(I,J)+1,JJV(I,J)+1,K)
              ELSE
                 CWK4(I,J,K) = VBWGT1(I,J)*CFLD(IIV(I,J),  JJV(I,J)  ,K) &
                             + VBWGT2(I,J)*CFLD(IIV(I,J)+1,JJV(I,J)  ,K) &
                             + VBWGT3(I,J)*CFLD(IIV(I,J),  JJV(I,J)-1,K) &
                             + VBWGT4(I,J)*CFLD(IIV(I,J),  JJV(I,J)+1,K)
              ENDIF
              ntemp_b(i,J,K)     = CWK4(I,J,K)
              ntemp_bt(i,J,K)    = 0.0
         END DO
        END DO
       END IF NMM_YE

     RETURN

   END SUBROUTINE nmm_bdy_vinterp







   SUBROUTINE nmm_copy      ( cfld,                                 &  
                              cids, cide, ckds, ckde, cjds, cjde,   &
                              cims, cime, ckms, ckme, cjms, cjme,   &
                              cits, cite, ckts, ckte, cjts, cjte,   &
                              nfld,                                 &  
                              nids, nide, nkds, nkde, njds, njde,   &
                              nims, nime, nkms, nkme, njms, njme,   &
                              nits, nite, nkts, nkte, njts, njte,   &
                              shw,                                  &  
                              imask,                                &  
                              xstag, ystag,                         &  
                              ipos, jpos,                           &  
                              nri, nrj,                             &  
                              CII, IIH, CJJ, JJH                    )

     USE module_timing
     IMPLICIT NONE

     LOGICAL, INTENT(IN) :: xstag, ystag
     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     REAL, DIMENSION ( cims:cime, cjms:cjme, ckms:ckme ), INTENT(IN)    :: cfld
     REAL, DIMENSION ( nims:nime, njms:njme, nkms:nkme ), INTENT(INOUT) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: imask
     INTEGER, DIMENSION ( cims:cime, cjms:cjme ), INTENT(IN) :: CII,CJJ                        
     INTEGER, DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: IIH,JJH


     INTEGER i,j,k

     DO J=NJTS,MIN(NJTE,NJDE-1)
       DO K=NKTS,NKTE
        DO I=NITS,MIN(NITE,NIDE-1)
           NFLD(I,J,K) = CFLD(IIH(I,J),JJH(I,J),K)
        ENDDO
       ENDDO
     ENDDO

  RETURN

  END SUBROUTINE nmm_copy





  SUBROUTINE test_nmm (cfld,                                 &  
                       cids, cide, ckds, ckde, cjds, cjde,   &
                       cims, cime, ckms, ckme, cjms, cjme,   &
                       cits, cite, ckts, ckte, cjts, cjte,   &
                       nfld,                                 &  
                       nids, nide, nkds, nkde, njds, njde,   &
                       nims, nime, nkms, nkme, njms, njme,   &
                       nits, nite, nkts, nkte, njts, njte,   &
                       shw,                                  & 
                       imask,                                & 
                       xstag, ystag,                         & 
                       ipos, jpos,                           & 
                       nri, nrj,                             & 
                       CII, IIH, CJJ, JJH, CBWGT1, HBWGT1,   & 
                       CBWGT2, HBWGT2, CBWGT3, HBWGT3,       & 
                       CBWGT4, HBWGT4                        ) 
     USE module_timing
     IMPLICIT NONE

     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, cjms:cjme, ckms:ckme ) :: cfld
     REAL, DIMENSION ( nims:nime, njms:njme, nkms:nkme ) :: nfld
     REAL,    DIMENSION ( cims:cime, cjms:cjme ), INTENT(IN) :: CBWGT1,CBWGT2,CBWGT3,CBWGT4    
     REAL,    DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: HBWGT1,HBWGT2,HBWGT3,HBWGT4
     INTEGER, DIMENSION ( cims:cime, cjms:cjme ), INTENT(IN) :: CII,CJJ                        
     INTEGER, DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: IIH,JJH
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask


     INTEGER i,j,k
     REAL,PARAMETER                                :: error=0.0001,error1=1.0
     REAL                                          :: diff



    DO J=NJTS,MIN(NJTE,NJDE-1)
     DO I=NITS,MIN(NITE,NIDE-1)
       IF(IIH(i,j).LT.(CIDS-shw) .OR. IIH(i,j).GT.(CIDE+shw)) &
           CALL wrf_error_fatal3("",3984,&
'hpoints:check domain bounds along x' )
       IF(JJH(i,j).LT.(CJDS-shw) .OR. JJH(i,j).GT.(CJDE+shw)) &
           CALL wrf_error_fatal3("",3987,&
'hpoints:check domain bounds along y' )
     ENDDO
    ENDDO


















     DO J=NJTS,MIN(NJTE,NJDE-1)
       DO K=NKDS,NKDE
        DO I=NITS,MIN(NITE,NIDE-1)
          IF(ABS(1.0-HBWGT1(I,J)) .LE. ERROR)THEN
             DIFF=ABS(NFLD(I,J,K)-CFLD(IIH(I,J),JJH(I,J),K))
             IF(DIFF .GT. ERROR)THEN
              CALL wrf_debug(1,"dyn_nmm: NON-COINCIDENT, NESTED MASS POINT") 
              WRITE(0,*)I,IIH(I,J),J,JJH(I,J),HBWGT1(I,J),NFLD(I,J,K),CFLD(IIH(I,J),JJH(I,J),K),DIFF 
             ENDIF
             IF(DIFF .GT. ERROR1)THEN
              WRITE(0,*)I,IIH(I,J),J,JJH(I,J),HBWGT1(I,J),NFLD(I,J,K),CFLD(IIH(I,J),JJH(I,J),K),DIFF
              CALL wrf_error_fatal3("",4020,&
'dyn_nmm: NON-COINCIDENT, NESTED MASS POINT')
             ENDIF
          ENDIF
        ENDDO
       ENDDO
     ENDDO

  END SUBROUTINE test_nmm




   SUBROUTINE nmm_feedback ( cfld,                                 &  
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  
                           imask,                                &  
                           xstag, ystag,                         &  
                           ipos, jpos,                           &  
                           nri, nrj,                             &  
                           CII, IIH, CJJ, JJH,                   &
                           CBWGT1, HBWGT1, CBWGT2, HBWGT2,       &
                           CBWGT3, HBWGT3, CBWGT4, HBWGT4        )
     USE module_configure
     IMPLICIT NONE


     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     INTEGER,DIMENSION(cims:cime,cjms:cjme),  INTENT(IN)    :: CII,CJJ     
     INTEGER,DIMENSION(nims:nime,njms:njme),  INTENT(IN)    :: IIH,JJH
     REAL,DIMENSION(cims:cime,cjms:cjme),     INTENT(IN)    :: CBWGT1,CBWGT2,CBWGT3,CBWGT4
     REAL,DIMENSION(nims:nime,njms:njme),     INTENT(IN)    :: HBWGT1,HBWGT2,HBWGT3,HBWGT4
     LOGICAL, INTENT(IN)                                    :: xstag, ystag

     REAL, DIMENSION ( cims:cime, cjms:cjme, ckms:ckme ), INTENT(OUT) :: cfld
     REAL, DIMENSION ( nims:nime, njms:njme, nkms:nkme ), INTENT(IN)  :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ),INTENT(IN)           :: imask

     

     INTEGER ci, cj, ck, ni, nj, nk, ip, jp, ioff, joff, ioffa, joffa
     INTEGER :: icmin,icmax,jcmin,jcmax
     INTEGER :: is, ipoints,jpoints,ijpoints
     INTEGER , PARAMETER :: passes = 2
     REAL    :: AVGH




   IF(nri .ne. 3 .OR. nrj .ne. 3)               &
    CALL wrf_error_fatal3("",4084,&
'Feedback works for only 1:3 ratios, currently. Modify the namelist' )



   CFLD = 9999.0

   DO ck = ckts, ckte
     nk = ck
     DO cj = MAX(jpos+1,cjts),MIN(jpos+(njde-njds)/nrj-1,cjte)  
       nj = (cj-jpos)*nrj + 1
       if(mod(cj,2) .eq. 0)THEN
         is=0 
       else
         is=1 
       endif
       DO ci = MAX(ipos+is,cits),MIN(ipos+(nide-nids)/nri-1,cite) 
         ni = (ci-ipos)*nri + 2 -is
         IF(IS==0)THEN    




          AVGH =                          NFLD(NI,NJ+2,NK)                                       &
               +          NFLD(NI  ,NJ+1,NK)              + NFLD(NI+1,NJ+1,NK)                   &
               + NFLD(NI-1,NJ  ,NK)     + NFLD(NI,NJ  ,NK)                 + NFLD(NI+1,NJ  ,NK)  &
               +          NFLD(NI  ,NJ-1,NK)              + NFLD(NI+1,NJ-1,NK)                   &
               +                          NFLD(NI,NJ-2,NK)

         ELSE




          AVGH =                          NFLD(NI,NJ+2,NK)                                       &
               +          NFLD(NI-1,NJ+1,NK)              + NFLD(NI,NJ+1,NK)                     &
               + NFLD(NI-1,NJ  ,NK)     + NFLD(NI,NJ  ,NK)                 + NFLD(NI+1,NJ  ,NK)  &
               +          NFLD(NI-1,NJ-1,NK)              + NFLD(NI,NJ-1,NK)                     &
               +                          NFLD(NI,NJ-2,NK)

         ENDIF


       CFLD(CI,CJ,CK) = AVGH/9.0
       ENDDO
     ENDDO
   ENDDO

   END SUBROUTINE nmm_feedback



   SUBROUTINE nmm_vfeedback ( cfld,                              &  
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  
                           imask,                                &  
                           xstag, ystag,                         &  
                           ipos, jpos,                           &  
                           nri, nrj,                             &  
                           CII, IIV, CJJ, JJV,                   &
                           CBWGT1, VBWGT1, CBWGT2, VBWGT2,       &
                           CBWGT3, VBWGT3, CBWGT4, VBWGT4        )
     USE module_configure
     IMPLICIT NONE


     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     INTEGER,DIMENSION(cims:cime,cjms:cjme),  INTENT(IN)    :: CII,CJJ     
     INTEGER,DIMENSION(nims:nime,njms:njme),  INTENT(IN)    :: IIV,JJV
     REAL,DIMENSION(cims:cime,cjms:cjme),     INTENT(IN)    :: CBWGT1,CBWGT2,CBWGT3,CBWGT4
     REAL,DIMENSION(nims:nime,njms:njme),     INTENT(IN)    :: VBWGT1,VBWGT2,VBWGT3,VBWGT4
     LOGICAL, INTENT(IN)                                    :: xstag, ystag

     REAL, DIMENSION ( cims:cime, cjms:cjme, ckms:ckme ), INTENT(OUT) :: cfld
     REAL, DIMENSION ( nims:nime, njms:njme, nkms:nkme ), INTENT(IN)  :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ),INTENT(IN)           :: imask

     

     INTEGER ci, cj, ck, ni, nj, nk, ip, jp, ioff, joff, ioffa, joffa
     INTEGER :: icmin,icmax,jcmin,jcmax
     INTEGER :: is, ipoints,jpoints,ijpoints
     INTEGER , PARAMETER :: passes = 2
     REAL :: AVGV




    IF(nri .ne. 3 .OR. nrj .ne. 3)               &
      CALL wrf_error_fatal3("",4187,&
'Feedback works for only 1:3 ratios, currently. Modify the namelist')



   CFLD = 9999.0

   DO ck = ckts, ckte
    nk = ck
    DO cj = MAX(jpos+1,cjts),MIN(jpos+(njde-njds)/nrj-1,cjte)  
     nj = (cj-jpos)*nrj + 1
     if(mod(cj,2) .eq. 0)THEN
      is=1 
     else
      is=0 
     endif
     DO ci = MAX(ipos+is,cits),MIN(ipos+(nide-nids)/nri-1,cite) 
       ni = (ci-ipos)*nri + 2 -is
         IF(IS==0)THEN    




          AVGV =                          NFLD(NI,NJ+2,NK)                                       &
               +          NFLD(NI  ,NJ+1,NK)              + NFLD(NI+1,NJ+1,NK)                   &
               + NFLD(NI-1,NJ  ,NK)     + NFLD(NI,NJ  ,NK)                 + NFLD(NI+1,NJ  ,NK)  &
               +          NFLD(NI  ,NJ-1,NK)              + NFLD(NI+1,NJ-1,NK)                   &
               +                          NFLD(NI,NJ-2,NK)

         ELSE




          AVGV =                          NFLD(NI,NJ+2,NK)                                       &
               +          NFLD(NI-1,NJ+1,NK)              + NFLD(NI,NJ+1,NK)                     &
               + NFLD(NI-1,NJ  ,NK)     + NFLD(NI,NJ  ,NK)                 + NFLD(NI+1,NJ  ,NK)  &
               +          NFLD(NI-1,NJ-1,NK)              + NFLD(NI,NJ-1,NK)                     &
               +                          NFLD(NI,NJ-2,NK)

         ENDIF


       CFLD(CI,CJ,CK) = AVGV/9.0
     ENDDO
    ENDDO
   ENDDO

   END SUBROUTINE nmm_vfeedback 


   SUBROUTINE nmm_smoother ( cfld , &
                             cids, cide, ckds, ckde, cjds, cjde,   &
                             cims, cime, ckms, ckme, cjms, cjme,   &
                             cits, cite, ckts, ckte, cjts, cjte,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nims, nime, nkms, nkme, njms, njme,   &
                             nits, nite, nkts, nkte, njts, njte,   &
                             xstag, ystag,                         &
                             ipos, jpos,                           &
                             nri, nrj                              &
                             )

      USE module_configure
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                             cims, cime, ckms, ckme, cjms, cjme,   &
                             cits, cite, ckts, ckte, cjts, cjte,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nims, nime, nkms, nkme, njms, njme,   &
                             nits, nite, nkts, nkte, njts, njte,   &
                             nri, nrj,                             &
                             ipos, jpos
      REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ), INTENT(INOUT) :: cfld
      LOGICAL, INTENT(IN) :: xstag, ystag


      

      INTEGER             :: feedback
      INTEGER, PARAMETER  :: smooth_passes = 5

      REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfldnew
      INTEGER :: ci, cj, ck
      INTEGER :: is, npass
      REAL    :: AVGH

      RETURN
      

      CALL nl_get_feedback       ( 1, feedback  )
      IF ( feedback == 0 ) RETURN

      WRITE(0,*)'SIMPLE SMOOTHER IS SWITCHED ON FOR HEIGHT'

      DO npass = 1, smooth_passes

      DO cj = MAX(jpos+1,cjts),MIN(jpos+(njde-njds)/nrj-1,cjte)  
       if(mod(cj,2) .eq. 0)THEN
        is=0 
       else
        is=1 
       endif
       DO ck = ckts, ckte
        DO ci = MAX(ipos+is,cits),MIN(ipos+(nide-nids)/nri-1,cite) 
            IF(IS==0)THEN    
             AVGH = CFLD(CI,CK,CJ+1) + CFLD(CI,CK,CJ-1) + CFLD(CI+1,CK,CJ+1) + CFLD(CI+1,CK,CJ-1)
            ELSE
             AVGH = CFLD(CI,CK,CJ+1) + CFLD(CI,CK,CJ-1) + CFLD(CI-1,CK,CJ+1) + CFLD(CI-1,CK,CJ-1)
            ENDIF
            CFLDNEW(CI,CK,CJ) = (AVGH + 4*CFLD(CI,CK,CJ)) / 8.0
        ENDDO
       ENDDO
      ENDDO

      DO cj = MAX(jpos+1,cjts),MIN(jpos+(njde-njds)/nrj-1,cjte)  
       if(mod(cj,2) .eq. 0)THEN
        is=0 
       else
        is=1 
       endif
       DO ck = ckts, ckte
        DO ci = MAX(ipos+is,cits),MIN(ipos+(nide-nids)/nri-1,cite) 
           CFLD(CI,CK,CJ) = CFLDNEW(CI,CK,CJ)
        ENDDO
       ENDDO
      ENDDO

      ENDDO   

   END SUBROUTINE nmm_smoother


   SUBROUTINE nmm_vsmoother ( cfld , &
                             cids, cide, ckds, ckde, cjds, cjde,   &
                             cims, cime, ckms, ckme, cjms, cjme,   &
                             cits, cite, ckts, ckte, cjts, cjte,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nims, nime, nkms, nkme, njms, njme,   &
                             nits, nite, nkts, nkte, njts, njte,   &
                             xstag, ystag,                         &
                             ipos, jpos,                           &
                             nri, nrj                              &
                             )

      USE module_configure
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                             cims, cime, ckms, ckme, cjms, cjme,   &
                             cits, cite, ckts, ckte, cjts, cjte,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nims, nime, nkms, nkme, njms, njme,   &
                             nits, nite, nkts, nkte, njts, njte,   &
                             nri, nrj,                             &
                             ipos, jpos
      REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ), INTENT(INOUT) :: cfld
      LOGICAL, INTENT(IN) :: xstag, ystag


      

      INTEGER             :: feedback
      INTEGER, PARAMETER  :: smooth_passes = 5

      REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfldnew
      INTEGER :: ci, cj, ck
      INTEGER :: is, npass
      REAL    :: AVGV

      RETURN
      

      CALL nl_get_feedback       ( 1, feedback  )
      IF ( feedback == 0 ) RETURN

      WRITE(0,*)'SIMPLE SMOOTHER IS SWITCHED ON FOR VELOCITY'

      DO npass = 1, smooth_passes

      DO cj = MAX(jpos+1,cjts),MIN(jpos+(njde-njds)/nrj-1,cjte)  
       if(mod(cj,2) .eq. 0)THEN
        is=1 
       else
        is=0 
       endif
       DO ck = ckts, ckte
        DO ci = MAX(ipos+is,cits),MIN(ipos+(nide-nids)/nri-1,cite) 
            IF(IS==0)THEN    
             AVGV = CFLD(CI,CK,CJ+1) + CFLD(CI,CK,CJ-1) + CFLD(CI+1,CK,CJ+1) + CFLD(CI+1,CK,CJ-1)
            ELSE
             AVGV = CFLD(CI,CK,CJ+1) + CFLD(CI,CK,CJ-1) + CFLD(CI-1,CK,CJ+1) + CFLD(CI-1,CK,CJ-1)
            ENDIF
            CFLDNEW(CI,CK,CJ) = (AVGV + 4*CFLD(CI,CK,CJ)) / 8.0
        ENDDO
       ENDDO
      ENDDO

      DO cj = MAX(jpos+1,cjts),MIN(jpos+(njde-njds)/nrj-1,cjte)  
       if(mod(cj,2) .eq. 0)THEN
        is=1 
       else
        is=0 
       endif
       DO ck = ckts, ckte
        DO ci = MAX(ipos+is,cits),MIN(ipos+(nide-nids)/nri-1,cite) 
           CFLD(CI,CK,CJ) = CFLDNEW(CI,CK,CJ)
        ENDDO
       ENDDO
      ENDDO

      ENDDO

   END SUBROUTINE nmm_vsmoother



