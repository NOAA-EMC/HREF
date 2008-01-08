!WRF:MEDIATION_LAYER:INTERPOLATIONFUNCTION
!

!#define DUMBCOPY

   SUBROUTINE interp_fcn ( cfld,                                 &  ! CD field
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  ! ND field
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  ! stencil half width for interp
                           xstag, ystag,                         &  ! staggering of field
                           ipos, jpos,                           &  ! Position of lower left of nest in CD
                           nri, nrj                             )   ! nest ratios
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

     ! Local

     INTEGER ci, cj, ck, ni, nj, nk, ip, jp, ioff, joff
     INTEGER nfx, ior
     PARAMETER (ior=2)
     INTEGER nf
     REAL psca(cims:cime,cjms:cjme,nri*nrj)
     INTEGER i,j,k

     ! Iterate over the ND tile and compute the values
     ! from the CD tile. 

!write(0,("cids:cide, ckds:ckde, cjds:cjde ",6i4))cids,cide, ckds,ckde, cjds,cjde
!write(0,("cims:cime, ckms:ckme, cjms:cjme ",6i4))cims,cime, ckms,ckme, cjms,cjme
!write(0,("cits:cite, ckts:ckte, cjts:cjte ",6i4))cits,cite, ckts,ckte, cjts,cjte
!write(0,("nims:nime, nkms:nkme, njms:njme ",6i4))nims,nime, nkms,nkme, njms,njme
!write(0,("nits:nite, nkts:nkte, njts:njte ",6i4))nits,nite, nkts,nkte, njts,njte


     ioff = 0 ; joff = 0
     IF ( xstag ) ioff = (nri-1)/2
     IF ( ystag ) joff = (nrj-1)/2

     nfx = nri * nrj
     DO k = ckts, ckte
        DO nf = 1,nfx
           DO j = cjms,cjme
              DO i = cims,cime
                psca(i,j,nf) = cfld(i,k,j)
!if (k==1.and.nf==1) write(0,*)& ,i,j,nf,psca(i,j,nf)
              ENDDO
           ENDDO
        ENDDO

! tile dims in this call to sint are 1-over to account for the fact
! that the number of cells on the nest local subdomain is not 
! necessarily a multiple of the nest ratio in a given dim.
! this could be a little less ham-handed.

        CALL sint( psca,                     &
                   cims, cime, cjms, cjme,   &
                   cits-1, cite+1, cjts-1, cjte+1, nrj*nri, xstag, ystag )

        DO nj = njts, njte+joff
           cj = jpos + (nj-1) / nrj     ! j coord of CD point 
           jp = mod ( nj-1 , nrj )  ! coord of ND w/i CD point
           nk = k
           ck = nk
           DO ni = nits, nite+ioff
               ci = ipos + (ni-1) / nri      ! j coord of CD point 
               ip = mod ( ni-1 , nri )  ! coord of ND w/i CD point
!               nfld( ni-ioff, nk, nj-joff ) = psca( ci+shw , cj+shw, ip+1 + (jp)*nri )
               nfld( ni-ioff, nk, nj-joff ) = psca( ci , cj, ip+1 + (jp)*nri )
!if (nk==1) write(0,*)+ ,ni-ioff, nk, nj-joff, nfld( ni-ioff, nk, nj-joff )
           ENDDO
        ENDDO
     ENDDO


     RETURN

   END SUBROUTINE interp_fcn

!==================================
! this is the default function used in feedback.

   SUBROUTINE copy_fcn ( cfld,                                 &  ! CD field
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  ! ND field
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  ! stencil half width for interp
                           xstag, ystag,                         &  ! staggering of field
                           ipos, jpos,                           &  ! Position of lower left of nest in CD
                           nri, nrj                             )   ! nest ratios
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

     ! Local

     INTEGER ci, cj, ck, ni, nj, nk, ip, jp, ioff, joff, ioffa, joffa

     ! Iterate over the ND tile and compute the values
     ! from the CD tile. 

!write(0,(") cims:cime, ckms:ckme, cjms:cjme ",6i4))cims,cime, ckms,ckme, cjms,cjme
!write(0,(") nims:nime, nkms:nkme, njms:njme ",6i4))nims,nime, nkms,nkme, njms,njme
!write(0,(") cits:cite, ckts:ckte, cjts:cjte ",6i4))cits,cite, ckts,ckte, cjts,cjte
!write(0,(") nits:nite, nkts:nkte, njts:njte ",6i4))nits,nite, nkts,nkte, njts,njte

     ioff  = 0 ; joff  = 0
     ioffa = 0 ; joffa = 0
     IF( mod(nri,2) .ne. 0) then ! odd refinement ratio
       IF ( xstag ) ioff = (nri-1)/2
       IF ( ystag ) joff = (nrj-1)/2
     ELSE  ! even refinement ratio
       IF ( xstag ) then
           ioff = (nri)/2
           ioffa = 1
       END IF
       IF ( ystag ) then
           joff = (nrj)/2
           joffa = 1
       END IF
     END IF

!write(0,*)ioff, joff ,ioff,joff

     IF( mod(nrj,2) .ne. 0) THEN  ! odd refinement ratio

     DO nj = njts, njte
	cj = jpos + (nj-1) / nrj     ! j coord of CD point 
        jp = mod ( nj , nrj )  ! coord of ND w/i CD point
	DO nk = nkts, nkte
	   ck = nk
           DO ni = nits, nite
	      ci = ipos + (ni-1) / nri      ! j coord of CD point 
              ip = mod ( ni , nri )  ! coord of ND w/i CD point
              IF ((ip == nri/2 + 1 - ioff) .AND. (jp == nrj/2 + 1 - joff) ) THEN
!	        cfld( ci+shw, ck, cj+shw ) = nfld( ni , nk , nj )
!	        cfld( ci, ck, cj ) = nfld( ni , nk , nj )
	        cfld( ci, ck, cj ) = (1./9.)*(               &
                                       nfld( ni  , nk , nj  )  &
                                      +nfld( ni-1, nk , nj  )  &
                                      +nfld( ni+1, nk , nj  )  &
                                      +nfld( ni  , nk , nj-1)  &
                                      +nfld( ni-1, nk , nj-1)  &
                                      +nfld( ni+1, nk , nj-1)  &
                                      +nfld( ni  , nk , nj+1)  &
                                      +nfld( ni-1, nk , nj+1)  &
                                      +nfld( ni+1, nk , nj+1) )
!if ( ck == 1 ) write(0,*)>> ci, cj, ni, ip ,ci,cj, ni,ip,cfld( ci, ck, cj )
              ENDIF
           ENDDO
        ENDDO
     ENDDO

     ELSE  ! even refinement ratio

     DO nj = njts, njte
	cj = jpos + (nj-1) / nrj     ! j coord of CD point 
        jp = mod ( nj , nrj )  ! coord of ND w/i CD point
	DO nk = nkts, nkte
	   ck = nk
           DO ni = nits, nite
	      ci = ipos + (ni-1) / nri      ! j coord of CD point 
              ip = mod ( ni , nri )  ! coord of ND w/i CD point
              IF ((ip == nri/2 - ioff ) .AND. (jp == nrj/2 - joff ) ) THEN
!	        cfld( ci+shw, ck, cj+shw ) = nfld( ni , nk , nj )
	        cfld( ci, ck, cj ) = 0.25*( nfld( ni   + ioffa , nk , nj   + joffa ) +   &
                                            nfld( ni+1         , nk , nj   + joffa ) +   &
                                            nfld( ni   + ioffa , nk , nj+1         ) +   &
                                            nfld( ni+1         , nk , nj+1         )    )
!if ( ck == 1 ) write(0,*)>> ci ni ip ,ci,ni,ip,cfld( ci, ck, cj )
              ENDIF
           ENDDO
        ENDDO
     ENDDO

     END IF

     RETURN

   END SUBROUTINE copy_fcn

!==================================

   SUBROUTINE bdy_interp ( cfld,                                 &  ! CD field
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  ! ND field
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  ! stencil half width
                           xstag, ystag,                         &  ! staggering of field
                           ipos, jpos,                           &  ! Position of lower left of nest in CD
                           nri, nrj,                             &  ! nest ratios
                           cdt, ndt,                             &
                           cbdy, nbdy,                           &
                           cbdy_t, nbdy_t                        &
                           )   ! boundary arrays
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
     REAL,  DIMENSION( * ), INTENT(INOUT) :: cbdy, cbdy_t, nbdy, nbdy_t
     REAL cdt, ndt

     ! Local

     INTEGER nijds, nijde, spec_bdy_width

     nijds = min(nids, njds)
     nijde = max(nide, njde)
     CALL get_spec_bdy_width( spec_bdy_width )

     CALL bdy_interp1( cfld,                                 &  ! CD field
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  ! ND field
                           nijds, nijde , spec_bdy_width ,       &  
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &
                           xstag, ystag,                         &  ! staggering of field
                           ipos, jpos,                           &  ! Position of lower left of nest in CD
                           nri, nrj,                             &
                           cdt, ndt,                             &
                           cbdy, nbdy,                           &
                           cbdy_t, nbdy_t                        &
                                        )

     RETURN

   END SUBROUTINE bdy_interp

   SUBROUTINE bdy_interp1( cfld,                                 &  ! CD field
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  ! ND field
                           nijds, nijde, spec_bdy_width ,          &
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw1,                                 &
                           xstag, ystag,                         &  ! staggering of field
                           ipos, jpos,                           &  ! Position of lower left of nest in CD
                           nri, nrj,                             &
                           cdt, ndt,                             &
                           cbdy, bdy,                            &
                           cbdy_t, bdy_t                         &
                                        )

     use module_state_description
     IMPLICIT NONE

     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw1,                                 &  ! ignore
                            ipos, jpos,                           &
                            nri, nrj
     INTEGER, INTENT(IN) :: nijds, nijde, spec_bdy_width
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ), INTENT(INOUT) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ), INTENT(INOUT) :: nfld
     REAL, DIMENSION ( * ), INTENT(INOUT) :: cbdy, cbdy_t   ! not used
     REAL                                 :: cdt, ndt
     REAL, DIMENSION ( nijds:nijde, nkms:nkme, spec_bdy_width, 4 ), INTENT(INOUT) :: bdy, bdy_t

     ! Local

     REAL*8 rdt
     INTEGER ci, cj, ck, ni, nj, nk, ni1, nj1, nk1, ip, jp, ioff, joff
     INTEGER nfx, ior
     PARAMETER (ior=2)
     INTEGER nf
     REAL psca(cims:cime,cjms:cjme,nri*nrj)
     INTEGER i,j,k
     INTEGER shw

     rdt = 1.D0/cdt

     shw = 0


     ioff = 0 ; joff = 0
     IF ( xstag ) ioff = (nri-1)/2
     IF ( ystag ) joff = (nrj-1)/2

     ! Iterate over the ND tile and compute the values
     ! from the CD tile. 


     nfx = nri * nrj
     DO k = ckts, ckte
        DO nf = 1,nfx
           DO j = cjms,cjme
              DO i = cims,cime
                psca(i,j,nf) = cfld(i,k,j)
              ENDDO
           ENDDO
        ENDDO

! tile dims in this call to sint are 1-over to account for the fact
! that the number of cells on the nest local subdomain is not 
! necessarily a multiple of the nest ratio in a given dim.
! this could be a little less ham-handed.

!write(0,(")) nijds:nijde nkms:nkme spec_bdy_width",5i4))nijds,nijde,nkms,nkme,spec_bdy_width
!write(0,(")) cims:cime, ckms:ckme, cjms:cjme ",6i4))cims,cime, ckms,ckme, cjms,cjme
!write(0,(")) nims:nime, nkms:nkme, njms:njme ",6i4))nims,nime, nkms,nkme, njms,njme
!write(0,(")) cits:cite, ckts:ckte, cjts:cjte ",6i4))cits,cite, ckts,ckte, cjts,cjte
!write(0,(")) nits:nite, nkts:nkte, njts:njte ",6i4))nits,nite, nkts,nkte, njts,njte

        CALL sint( psca,                     &
                   cims, cime, cjms, cjme,   &
                   cits-1, cite+1, cjts-1, cjte+1, nrj*nri, xstag, ystag )

        DO nj1 = njts, njte+joff
           cj = jpos + (nj1-1) / nrj     ! j coord of CD point 
           jp = mod ( nj1-1 , nrj )  ! coord of ND w/i CD point
           nk = k
           ck = nk
           DO ni1 = nits, nite+ioff
               ci = ipos + (ni1-1) / nri      ! j coord of CD point 
               ip = mod ( ni1-1 , nri )  ! coord of ND w/i CD point

               ni = ni1-ioff
               nj = nj1-joff

!bdy contains the value at t-dt. psca contains the value at t
!compute dv/dt and store in bdy_t
!afterwards store the new value of v at t into bdy
!question: how bdy gets set first time through?? isnt that stored in nfld right now?
!compute the derivative using that instead?
! how to get rdt down here??

               IF   ( ni .ge. nids .and. ni .lt. nids + spec_bdy_width ) THEN
                 bdy_t( nj,k,ni, P_XSB ) = rdt*(psca(ci+shw,cj+shw,ip+1+(jp)*nri)-nfld(ni,k,nj))
                 bdy( nj,k,ni, P_XSB ) = psca(ci+shw,cj+shw,ip+1+(jp)*nri )
!if (k==1) write(0,*) P_XSB: ,nj,ni,bdy( nj,k,ni,P_XSB),psca(ci+shw,cj+shw,ip+1+(jp)*nri)
!if (k==1) write(0,*) P_XSB: ,ioff,joff,bdy( nj,k,ni,P_XSB),nfld(ni,k,nj)
               ENDIF

               IF   ( nj .ge. njds .and. nj .lt. njds + spec_bdy_width ) THEN
                 bdy_t( ni,k,nj, P_YSB ) = rdt*(psca(ci+shw,cj+shw,ip+1+(jp)*nri)-nfld(ni,k,nj))
                 bdy( ni,k,nj, P_YSB ) = psca(ci+shw,cj+shw,ip+1+(jp)*nri )
!if (k==1) write(0,*) P_YSB: ,nj,ni,ci,cj, bdy( nj,k,ni, P_YSB ) 
               ENDIF

               IF ( xstag ) THEN
                 IF   ( ni .le. nide .and. ni .ge. nide - spec_bdy_width + 1 ) THEN
                   bdy_t( nj,k,nide-ni+1, P_XEB ) = rdt*(psca(ci+shw,cj+shw,ip+1+(jp)*nri)-nfld(ni,k,nj))
                   bdy( nj,k,nide-ni+1, P_XEB ) = psca(ci+shw,cj+shw,ip+1+(jp)*nri )
                 ENDIF
               ELSE
                 IF   ( ni .le. nide-1 .and. ni .ge. nide - spec_bdy_width ) THEN
                   bdy_t( nj,k,nide-ni, P_XEB ) = rdt*(psca(ci+shw,cj+shw,ip+1+(jp)*nri)-nfld(ni,k,nj))
                   bdy( nj,k,nide-ni, P_XEB ) = psca(ci+shw,cj+shw,ip+1+(jp)*nri )
!if (k==1) write(0,*) P_XEB: ,ci,cj,bdy( nj,k,nide-ni,P_XEB),nfld(ni,k,nj)
                 ENDIF
               ENDIF

               IF ( ystag ) THEN
                 IF   ( nj .le. njde .and. nj .ge. njde - spec_bdy_width + 1 ) THEN
                   bdy_t(ni,k,njde-nj+1,P_YEB ) = rdt*(psca(ci+shw,cj+shw,ip+1+(jp)*nri)-nfld(ni,k,nj))
                   bdy( ni,k,njde-nj+1,P_YEB ) = psca(ci+shw,cj+shw,ip+1+(jp)*nri )
!if (k==1) write(0,(7I4,e13.5))ni,nj,njde,njde-nj+1,ci,cj,ip+1+(jp)*nri,bdy( ni,k,njde-nj+1,P_YEB )
!if (k==1) write(0,(7I4,2e13.5))ni,nj,njde,njde-nj+1,ci,cj,ip+1+(jp)*nri,bdy( ni,k,njde-nj+1,P_YEB ),psca(ci+shw,cj+shw,ip+1+(jp)*nri )
                 ENDIF
               ELSE
                 IF   ( nj .le. njde-1 .and. nj .ge. njde - spec_bdy_width ) THEN
                   bdy_t(ni,k,njde-nj,P_YEB ) = rdt*(psca(ci+shw,cj+shw,ip+1+(jp)*nri)-nfld(ni,k,nj))
                   bdy( ni,k,njde-nj,P_YEB ) = psca(ci+shw,cj+shw,ip+1+(jp)*nri )
!if (k==1) write(0,(7I4,e13.5))ni,nj,njde,njde-nj,ci,cj,ip+1+(jp)*nri,bdy( ni,k,njde-nj,P_YEB )
!if (k==1) write(0,(7I4,2e13.5))ni,nj,njde,njde-nj,ci,cj,ip+1+(jp)*nri,bdy( ni,k,njde-nj,P_YEB ),psca(ci+shw,cj+shw,ip+1+(jp)*nri )
                 ENDIF
               ENDIF

           ENDDO
        ENDDO
     ENDDO


     RETURN

   END SUBROUTINE bdy_interp1



   SUBROUTINE interp_fcni( cfld,                                 &  ! CD field
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  ! ND field
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           xstag, ystag,                         &  ! staggering of field
                           ipos, jpos,                           &  ! Position of lower left of nest in CD
                           nri, nrj                             )   ! nest ratios
     IMPLICIT NONE


     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     INTEGER, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
     INTEGER, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld

     ! Local

     INTEGER ci, cj, ck, ni, nj, nk, ip, jp

     ! Iterate over the ND tile and compute the values
     ! from the CD tile. 

!write(0,("cits:cite, ckts:ckte, cjts:cjte ",6i4))cits,cite, ckts,ckte, cjts,cjte
!write(0,("nits:nite, nkts:nkte, njts:njte ",6i4))nits,nite, nkts,nkte, njts,njte

     DO nj = njts, njte
	cj = jpos + (nj-1) / nrj     ! j coord of CD point 
        jp = mod ( nj , nrj )  ! coord of ND w/i CD point
	DO nk = nkts, nkte
	   ck = nk
           DO ni = nits, nite
	      ci = ipos + (ni-1) / nri      ! j coord of CD point 
              ip = mod ( ni , nri )  ! coord of ND w/i CD point
              ! This is a trivial implementation of the interp_fcn; just copies
              ! the values from the CD into the ND
	      nfld( ni, nk, nj ) = cfld( ci , ck , cj )
           ENDDO
        ENDDO
     ENDDO

     RETURN

   END SUBROUTINE interp_fcni

   SUBROUTINE interp_fcnm( cfld,                                 &  ! CD field
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  ! ND field
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           xstag, ystag,                         &  ! staggering of field
                           ipos, jpos,                           &  ! Position of lower left of nest in CD
                           nri, nrj                             )   ! nest ratios
     IMPLICIT NONE


     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld

     ! Local

     INTEGER ci, cj, ck, ni, nj, nk, ip, jp

     ! Iterate over the ND tile and compute the values
     ! from the CD tile. 

!write(0,("mask cits:cite, ckts:ckte, cjts:cjte ",6i4))cits,cite, ckts,ckte, cjts,cjte
!write(0,("mask nits:nite, nkts:nkte, njts:njte ",6i4))nits,nite, nkts,nkte, njts,njte

     DO nj = njts, njte
	cj = jpos + (nj-1) / nrj     ! j coord of CD point 
        jp = mod ( nj , nrj )  ! coord of ND w/i CD point
	DO nk = nkts, nkte
	   ck = nk
           DO ni = nits, nite
	      ci = ipos + (ni-1) / nri      ! j coord of CD point 
              ip = mod ( ni , nri )  ! coord of ND w/i CD point
              ! This is a trivial implementation of the interp_fcn; just copies
              ! the values from the CD into the ND
	      nfld( ni, nk, nj ) = cfld( ci , ck , cj )
           ENDDO
        ENDDO
     ENDDO

     RETURN

   END SUBROUTINE interp_fcnm

   SUBROUTINE interp_mask_land_field ( cfld,                     &  ! CD field
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  ! ND field
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  ! stencil half width
                           xstag, ystag,                         &  ! staggering of field
                           ipos, jpos,                           &  ! Position of lower left of nest in CD
                           nri, nrj,                             &  ! nest ratios
                           clu, nlu                              )

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
   
      REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
      REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
   
      REAL, DIMENSION ( cims:cime, cjms:cjme ) :: clu
      REAL, DIMENSION ( nims:nime, njms:njme ) :: nlu
   
      ! Local
   
      INTEGER ci, cj, ck, ni, nj, nk, ip, jp
      INTEGER :: icount , ii , jj , ist , ien , jst , jen , iswater
      REAL :: sum , dx , dy
   
      !  Find out what the water value is.
   
      CALL get_iswater(1,iswater)

      !  Right now, only mass point locations permitted.
   
      IF ( ( .NOT. xstag) .AND. ( .NOT. ystag ) ) THEN

         !  Loop over each i,k,j in the nested domain.

         DO nj = njts, njte
            cj = ( nj + 1) / nrj + jpos -1 ! coarse position equal to or below nest point
            DO nk = nkts, nkte
               ck = nk
               DO ni = nits, nite
                  ci = ( ni + 1) / nri + ipos -1 ! coarse position equal to or to the left of nest point
   



                  !
                  !                    (ci,cj+1)     (ci+1,cj+1)
                  !               -        -------------
                  !         1-dy  |        |           |
                  !               |        |           |
                  !               -        |  *        |
                  !          dy   |        | (ni,nj)   |
                  !               |        |           |
                  !               -        -------------
                  !                    (ci,cj)       (ci+1,cj)  
                  !
                  !                        |--|--------|
                  !                         dx  1-dx         


                  !  At ni=2, we are on the coarse grid point, so dx = 0

                  dx = REAL ( MOD ( ni+1 , nri ) ) / REAL ( nri ) 
                  dy = REAL ( MOD ( nj+1 , nrj ) ) / REAL ( nrj ) 
   
                  !  This is a "land only" field.  If this is a water point, no operations required.

                  IF      ( ( NINT(nlu(ni  ,nj  )) .EQ. iswater ) ) THEN
                     ! noop
                     nfld(ni,nk,nj) =  1.e20

                  !  If this is a nested land point, and the surrounding coarse values are all land points,
                  !  then this is a simple 4-pt interpolation.

                  ELSE IF ( ( NINT(nlu(ni  ,nj  )) .NE. iswater ) .AND. &
                            ( NINT(clu(ci  ,cj  )) .NE. iswater ) .AND. &
                            ( NINT(clu(ci+1,cj  )) .NE. iswater ) .AND. &
                            ( NINT(clu(ci  ,cj+1)) .NE. iswater ) .AND. &
                            ( NINT(clu(ci+1,cj+1)) .NE. iswater ) ) THEN
                     nfld(ni,nk,nj) = ( 1. - dx ) * ( ( 1. - dy ) * cfld(ci  ,ck,cj  )   + &
                                                             dy   * cfld(ci  ,ck,cj+1) ) + &
                                             dx   * ( ( 1. - dy ) * cfld(ci+1,ck,cj  )   + &
                                                             dy   * cfld(ci+1,ck,cj+1) )

                  !  If this is a nested land point and there are NO coarse land values surrounding,
                  !  we temporarily punt.

                  ELSE IF ( ( NINT(nlu(ni  ,nj  )) .NE. iswater ) .AND. &
                            ( NINT(clu(ci  ,cj  )) .EQ. iswater ) .AND. &
                            ( NINT(clu(ci+1,cj  )) .EQ. iswater ) .AND. &
                            ( NINT(clu(ci  ,cj+1)) .EQ. iswater ) .AND. &
                            ( NINT(clu(ci+1,cj+1)) .EQ. iswater ) ) THEN
                     nfld(ni,nk,nj) = -1.e20

                  !  If there are some water points and some land points, take an average. 
                  
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

         !  OK, if there were any of those island situations, we try to search a bit broader
         !  of an area in the coarse grid.

         DO nj = njts, njte
            DO nk = nkts, nkte
               DO ni = nits, nite
                  IF ( nfld(ni,nk,nj) .LT. -1.e19 ) THEN
                     cj = ( nj + 1) / nrj + jpos -1
                     ci = ( ni + 1) / nri + ipos -1
                     ist = MAX (ci-2,cits)
                     ien = MIN (ci+2,cite,cide-1)
                     jst = MAX (cj-2,cjts)
                     jen = MIN (cj+2,cjte,cjde-1)
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
                        CALL wrf_error_fatal ( "horizontal interp error - island" )
                     END IF        
                  END IF
               END DO
            END DO
         END DO
      ELSE
         CALL wrf_error_fatal ( "only unstaggered fields right now" )
      END IF

   END SUBROUTINE interp_mask_land_field

   SUBROUTINE interp_mask_water_field ( cfld,                    &  ! CD field
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  ! ND field
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  ! stencil half width
                           xstag, ystag,                         &  ! staggering of field
                           ipos, jpos,                           &  ! Position of lower left of nest in CD
                           nri, nrj,                             &  ! nest ratios
                           clu, nlu                              )

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
   
      REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
      REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
   
      REAL, DIMENSION ( cims:cime, cjms:cjme ) :: clu
      REAL, DIMENSION ( nims:nime, njms:njme ) :: nlu
   
      ! Local
   
      INTEGER ci, cj, ck, ni, nj, nk, ip, jp
      INTEGER :: icount , ii , jj , ist , ien , jst , jen , iswater
      REAL :: sum , dx , dy
   
      !  Find out what the water value is.
   
      CALL get_iswater(1,iswater)

      !  Right now, only mass point locations permitted.
   
      IF ( ( .NOT. xstag) .AND. ( .NOT. ystag ) ) THEN

         !  Loop over each i,k,j in the nested domain.

         DO nj = njts, njte
            cj = ( nj + 1) / nrj + jpos -1 ! coarse position equal to or below nest point
            DO nk = nkts, nkte
               ck = nk
               DO ni = nits, nite
                  ci = ( ni + 1) / nri + ipos -1 ! coarse position equal to or to the left of nest point
   



                  !
                  !                    (ci,cj+1)     (ci+1,cj+1)
                  !               -        -------------
                  !         1-dy  |        |           |
                  !               |        |           |
                  !               -        |  *        |
                  !          dy   |        | (ni,nj)   |
                  !               |        |           |
                  !               -        -------------
                  !                    (ci,cj)       (ci+1,cj)  
                  !
                  !                        |--|--------|
                  !                         dx  1-dx         


                  !  At ni=2, we are on the coarse grid point, so dx = 0

                  dx = REAL ( MOD ( ni+1 , nri ) ) / REAL ( nri ) 
                  dy = REAL ( MOD ( nj+1 , nrj ) ) / REAL ( nrj ) 
   
                  !  This is a "water only" field.  If this is a land point, no operations required.

                  IF      ( ( NINT(nlu(ni  ,nj  )) .NE. iswater ) ) THEN
                     ! noop
                     nfld(ni,nk,nj) =  1.e20

                  !  If this is a nested water point, and the surrounding coarse values are all water points,
                  !  then this is a simple 4-pt interpolation.

                  ELSE IF ( ( NINT(nlu(ni  ,nj  )) .EQ. iswater ) .AND. &
                            ( NINT(clu(ci  ,cj  )) .EQ. iswater ) .AND. &
                            ( NINT(clu(ci+1,cj  )) .EQ. iswater ) .AND. &
                            ( NINT(clu(ci  ,cj+1)) .EQ. iswater ) .AND. &
                            ( NINT(clu(ci+1,cj+1)) .EQ. iswater ) ) THEN
                     nfld(ni,nk,nj) = ( 1. - dx ) * ( ( 1. - dy ) * cfld(ci  ,ck,cj  )   + &
                                                             dy   * cfld(ci  ,ck,cj+1) ) + &
                                             dx   * ( ( 1. - dy ) * cfld(ci+1,ck,cj  )   + &
                                                             dy   * cfld(ci+1,ck,cj+1) )

                  !  If this is a nested water point and there are NO coarse water values surrounding,
                  !  we temporarily punt.

                  ELSE IF ( ( NINT(nlu(ni  ,nj  )) .EQ. iswater ) .AND. &
                            ( NINT(clu(ci  ,cj  )) .NE. iswater ) .AND. &
                            ( NINT(clu(ci+1,cj  )) .NE. iswater ) .AND. &
                            ( NINT(clu(ci  ,cj+1)) .NE. iswater ) .AND. &
                            ( NINT(clu(ci+1,cj+1)) .NE. iswater ) ) THEN
                     nfld(ni,nk,nj) = -1.e20

                  !  If there are some land points and some water points, take an average. 
                  
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

         !  OK, if there were any of those lake situations, we try to search a bit broader
         !  of an area in the coarse grid.

         DO nj = njts, njte
            DO nk = nkts, nkte
               DO ni = nits, nite
                  IF ( nfld(ni,nk,nj) .LT. -1.e19 ) THEN
                     cj = ( nj + 1) / nrj + jpos -1
                     ci = ( ni + 1) / nri + ipos -1
                     ist = MAX (ci-2,cits)
                     ien = MIN (ci+2,cite,cide-1)
                     jst = MAX (cj-2,cjts)
                     jen = MIN (cj+2,cjte,cjde-1)
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
                        CALL wrf_error_fatal ( "horizontal interp error - lake" )
                     END IF        
                  END IF
               END DO
            END DO
         END DO
      ELSE
         CALL wrf_error_fatal ( "only unstaggered fields right now" )
      END IF

   END SUBROUTINE interp_mask_water_field

   SUBROUTINE none
   END SUBROUTINE none


