        module boundary

        USE parallel_module
        USE module_data
        USE date_pack

        CONTAINS

        SUBROUTINE proc_bdy (grid3d, gridin, bdy, time, n)

         implicit none

#ifdef _MPI
         include 'mpif.h'
#endif


         TYPE(output_vars):: grid3d
         TYPE(input_vars):: gridin
         TYPE(boundary_vars):: bdy

         INTEGER, intent(in):: time, n

         INTEGER :: IEND, JEND, LNSH, LNSV, LM
         INTEGER :: IMS, IME, JMS, JME
         INTEGER :: IDS, IDE, JDS, JDE
         INTEGER :: ITS, ITE, JTS, JTE
         INTEGER :: I, J, L, M, FHR, iunit,iunit2, ierr, istat, irecv
         INTEGER :: KFLIP, IHRSTBC, FMIN

         INTEGER :: mpi_comm_comp, npes, mype, IDATBC(3), NCOUNT

         REAL :: tboco

         LOGICAL:: n_bdy, s_bdy
         LOGICAL:: w_bdy, e_bdy, opened, runbc, do_work_loc
         LOGICAL, ALLOCATABLE:: do_work(:)

         CHARACTER(LEN=100) :: esmf_boundary
         CHARACTER(LEN=19) :: bdy_date

!        -------------

! values of comm, nprocs, and my_proc_id are available from parallel_module

        MPI_COMM_COMP=comm
        NPES=nprocs
        MYPE=my_proc_id

        if (.not. allocated(do_work)) then
          allocate(do_work(0:NPES-1))
        endif


        do_work=.false.

        write(0,*) 'MYPE, NPES: ', MYPE, NPES
         IEND=gridin%IDE-1 !?
         JEND=gridin%JDE-1 !?

         IMS =gridin%IMS
         IME =gridin%IME
         JMS =gridin%JMS
         JME =gridin%JME

         IDS =gridin%IDS
         IDE =gridin%IDE
         JDS =gridin%JDS
         JDE =gridin%JDE

         ITS =gridin%ITS
         ITE =gridin%ITE
         JTS =gridin%JTS
         JTE =gridin%JTE

         LM = gridin%KDE-1

         LNSH=bdy%LNSH
         LNSV=bdy%LNSV

!        LNSH=1
!        LNSV=1


	write(0,*) 'have LNSH, LNSV as: ', LNSH, LNSV


         tboco=bdy%tboco_bdy

        IF (time .eq. 0) then

        ALLOCATE(bdy%pd_new_n(ITS:min(ITE,IEND),1:LNSH))
        ALLOCATE(bdy%pd_new_s(ITS:min(ITE,IEND),1:LNSH))
        ALLOCATE(bdy%pd_old_n(ITS:min(ITE,IEND),1:LNSH))
        ALLOCATE(bdy%pd_old_s(ITS:min(ITE,IEND),1:LNSH))

        ALLOCATE(bdy%pd_new_w(1:LNSH,JTS:min(JTE,JEND)))
        ALLOCATE(bdy%pd_new_e(1:LNSH,JTS:min(JTE,JEND)))
        ALLOCATE(bdy%pd_old_w(1:LNSH,JTS:min(JTE,JEND)))
        ALLOCATE(bdy%pd_old_e(1:LNSH,JTS:min(JTE,JEND)))
!
        ALLOCATE(bdy%t_new_n(ITS:min(ITE,IEND),1:LNSH,LM))
        ALLOCATE(bdy%t_new_s(ITS:min(ITE,IEND),1:LNSH,LM))
        ALLOCATE(bdy%t_old_n(ITS:min(ITE,IEND),1:LNSH,LM))
        ALLOCATE(bdy%t_old_s(ITS:min(ITE,IEND),1:LNSH,LM))

        ALLOCATE(bdy%t_new_w(1:LNSH,JTS:min(JTE,JEND),LM))
        ALLOCATE(bdy%t_new_e(1:LNSH,JTS:min(JTE,JEND),LM))
        ALLOCATE(bdy%t_old_w(1:LNSH,JTS:min(JTE,JEND),LM))
        ALLOCATE(bdy%t_old_e(1:LNSH,JTS:min(JTE,JEND),LM))
!
        ALLOCATE(bdy%u_new_n(ITS:min(ITE,IEND),1:LNSV,LM))
        ALLOCATE(bdy%u_new_s(ITS:min(ITE,IEND),1:LNSV,LM))
        ALLOCATE(bdy%u_old_n(ITS:min(ITE,IEND),1:LNSV,LM))
        ALLOCATE(bdy%u_old_s(ITS:min(ITE,IEND),1:LNSV,LM))

        ALLOCATE(bdy%u_new_w(1:LNSV,JTS:min(JTE,JEND),LM))
        ALLOCATE(bdy%u_new_e(1:LNSV,JTS:min(JTE,JEND),LM))
        ALLOCATE(bdy%u_old_w(1:LNSV,JTS:min(JTE,JEND),LM))
        ALLOCATE(bdy%u_old_e(1:LNSV,JTS:min(JTE,JEND),LM))
!
        ALLOCATE(bdy%v_new_n(ITS:min(ITE,IEND),1:LNSV,LM))
        ALLOCATE(bdy%v_new_s(ITS:min(ITE,IEND),1:LNSV,LM))
        ALLOCATE(bdy%v_old_n(ITS:min(ITE,IEND),1:LNSV,LM))
        ALLOCATE(bdy%v_old_s(ITS:min(ITE,IEND),1:LNSV,LM))

        ALLOCATE(bdy%v_new_w(1:LNSV,JTS:min(JTE,JEND),LM))
        ALLOCATE(bdy%v_new_e(1:LNSV,JTS:min(JTE,JEND),LM))
        ALLOCATE(bdy%v_old_w(1:LNSV,JTS:min(JTE,JEND),LM))
        ALLOCATE(bdy%v_old_e(1:LNSV,JTS:min(JTE,JEND),LM))
!
        ALLOCATE(bdy%q_new_n(ITS:min(ITE,IEND),1:LNSH,LM))
        ALLOCATE(bdy%q_new_s(ITS:min(ITE,IEND),1:LNSH,LM))
        ALLOCATE(bdy%q_old_n(ITS:min(ITE,IEND),1:LNSH,LM))
        ALLOCATE(bdy%q_old_s(ITS:min(ITE,IEND),1:LNSH,LM))

        ALLOCATE(bdy%q_new_w(1:LNSH,JTS:min(JTE,JEND),LM))
        ALLOCATE(bdy%q_new_e(1:LNSH,JTS:min(JTE,JEND),LM))
        ALLOCATE(bdy%q_old_w(1:LNSH,JTS:min(JTE,JEND),LM))
        ALLOCATE(bdy%q_old_e(1:LNSH,JTS:min(JTE,JEND),LM))
!
        ALLOCATE(bdy%cwm_new_n(ITS:min(ITE,IEND),1:LNSH,LM))
        ALLOCATE(bdy%cwm_new_s(ITS:min(ITE,IEND),1:LNSH,LM))
        ALLOCATE(bdy%cwm_old_n(ITS:min(ITE,IEND),1:LNSH,LM))
        ALLOCATE(bdy%cwm_old_s(ITS:min(ITE,IEND),1:LNSH,LM))

        ALLOCATE(bdy%cwm_new_w(1:LNSH,JTS:min(JTE,JEND),LM))
        ALLOCATE(bdy%cwm_new_e(1:LNSH,JTS:min(JTE,JEND),LM))
        ALLOCATE(bdy%cwm_old_w(1:LNSH,JTS:min(JTE,JEND),LM))
        ALLOCATE(bdy%cwm_old_e(1:LNSH,JTS:min(JTE,JEND),LM))


! ZERO N/S arrays

        DO L=1,LM
         DO J=1,LNSH
          DO I=ITS,min(ITE,IEND)
           IF (L .eq. 1) THEN
             bdy%pd_new_n(I,J)=0.
             bdy%pd_new_s(I,J)=0.
             bdy%pd_old_n(I,J)=0.
             bdy%pd_old_s(I,J)=0.
           ENDIF
             bdy%t_new_n(I,J,L)=0.
             bdy%t_new_s(I,J,L)=0.
             bdy%t_old_n(I,J,L)=0.
             bdy%t_old_s(I,J,L)=0.

             bdy%q_new_n(I,J,L)=0.
             bdy%q_new_s(I,J,L)=0.
             bdy%q_old_n(I,J,L)=0.
             bdy%q_old_s(I,J,L)=0.

             bdy%cwm_new_n(I,J,L)=0.
             bdy%cwm_new_s(I,J,L)=0.
             bdy%cwm_old_n(I,J,L)=0.
             bdy%cwm_old_s(I,J,L)=0.
          ENDDO
         ENDDO
        ENDDO

        DO L=1,LM
         DO J=1,LNSV
          DO I=ITS,min(ITE,IEND)
             bdy%u_new_n(I,J,L)=0.
             bdy%u_new_s(I,J,L)=0.
             bdy%u_old_n(I,J,L)=0.
             bdy%u_old_s(I,J,L)=0.

             bdy%v_new_n(I,J,L)=0.
             bdy%v_new_s(I,J,L)=0.
             bdy%v_old_n(I,J,L)=0.
             bdy%v_old_s(I,J,L)=0.
          ENDDO
         ENDDO
        ENDDO

! ZERO W/E arrays

        DO L=1,LM
         DO J=JTS,min(JTE,JEND)
          DO I=1,LNSH
           IF (L .eq. 1) THEN
             bdy%pd_new_w(I,J)=0.
             bdy%pd_new_e(I,J)=0.
             bdy%pd_old_w(I,J)=0.
             bdy%pd_old_e(I,J)=0.
           ENDIF

             bdy%t_new_w(I,J,L)=0.
             bdy%t_new_e(I,J,L)=0.
             bdy%t_old_w(I,J,L)=0.
             bdy%t_old_e(I,J,L)=0.

             bdy%q_new_w(I,J,L)=0.
             bdy%q_new_e(I,J,L)=0.
             bdy%q_old_w(I,J,L)=0.
             bdy%q_old_e(I,J,L)=0.

             bdy%cwm_new_w(I,J,L)=0.
             bdy%cwm_new_e(I,J,L)=0.
             bdy%cwm_old_w(I,J,L)=0.
             bdy%cwm_old_e(I,J,L)=0.
          ENDDO
         ENDDO
        ENDDO

        DO L=1,LM
         DO J=JTS,min(JTE,JEND)
          DO I=1,LNSV
             bdy%u_new_w(I,J,L)=0.
             bdy%u_new_e(I,J,L)=0.
             bdy%u_old_w(I,J,L)=0.
             bdy%u_old_e(I,J,L)=0.

             bdy%v_new_w(I,J,L)=0.
             bdy%v_new_e(I,J,L)=0.
             bdy%v_old_w(I,J,L)=0.
             bdy%v_old_e(I,J,L)=0.
          ENDDO
         ENDDO
        ENDDO

        endif

        n_bdy=.false.
        s_bdy=.false.
        w_bdy=.false.
        e_bdy=.false.

        IF (ITS .eq. IDS) w_bdy=.true.
        IF (JTS .eq. JDS) s_bdy=.true.
        IF (ITE .eq. IDE-1) e_bdy=.true.
        IF (JTE .eq. JDE-1) n_bdy=.true.

        write(0,*) 'boundary logicals w, s, e, n: ', MYPE, w_bdy, s_bdy, e_bdy, n_bdy


!       EXTRACT current values into NEW

!! Need to know if touching a particular boundary

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        IF (n_bdy) THEN

!        write(0,*) 'IMS, IME along N boundary: ', IMS, IME
        DO J=1,LNSH
         DO I=ITS,min(ITE,IDE-1)
            bdy%pd_new_n(I,J)=grid3d%PD(I,J+JEND-lnsh)
!        if (mod(I,15) .eq. 0) then
!        endif
         ENDDO
        ENDDO

        DO L=1,LM
         DO J=1,LNSH
!          DO I=IMS,IME
          DO I=ITS,min(ITE,IDE-1)


            bdy%t_new_n(I,J,L)=    grid3d%T(I,J+JEND-lnsh,L)
            bdy%q_new_n(I,J,L)=    grid3d%Q(I,J+JEND-lnsh,L)
            bdy%cwm_new_n(I,J,L)=grid3d%CWM(I,J+JEND-lnsh,L)

!        if (I .eq. 25) then
!        write(0,*) 'I, L, t, q, cwm:: ', I, L, bdy%t_new_n(I,J,L), bdy%q_new_n(I,J,L), bdy%cwm_new_n(I,J,L)
!        endif

          ENDDO
         ENDDO
        ENDDO

        DO L=1,LM
         DO J=1,LNSV
!         DO I=IMS,IME
          DO I=ITS,min(ITE,IDE-1)
!           bdy%u_new_n(I,J,L)=grid3d%U(I,JEND-J+1,L) 
!           bdy%v_new_n(I,J,L)=grid3d%V(I,JEND-J+1,L) 
           bdy%u_new_n(I,J,L)=grid3d%U(I,J+JEND-lnsv-1,L) ! one row less than scalar N boundary
           bdy%v_new_n(I,J,L)=grid3d%V(I,J+JEND-lnsv-1,L) ! one row less than scalar N boundary
          ENDDO
         ENDDO
        ENDDO

        ENDIF ! n_bdy

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        IF (s_bdy) THEN

        DO J=1,LNSH
!         DO I=IMS,IME
          DO I=ITS,min(ITE,IDE-1)
            bdy%pd_new_s(I,J)=grid3d%PD(I,JDS+J-1)
         ENDDO
        ENDDO

        DO L=1,LM
         DO J=1,LNSH
!          DO I=IMS,IME
          DO I=ITS,min(ITE,IDE-1)
            bdy%t_new_s(I,J,L)=grid3d%T(I,JDS+J-1,L)
            bdy%q_new_s(I,J,L)=grid3d%Q(I,JDS+J-1,L)
            bdy%cwm_new_s(I,J,L)=grid3d%CWM(I,JDS+J-1,L)
          ENDDO
         ENDDO
        ENDDO

        DO L=1,LM
         DO J=1,LNSV
!          DO I=IMS,IME
          DO I=ITS,min(ITE,IDE-1)
            bdy%u_new_s(I,J,L)=grid3d%U(I,JDS+J-1,L)
            bdy%v_new_s(I,J,L)=grid3d%V(I,JDS+J-1,L)
          ENDDO
         ENDDO
        ENDDO

        ENDIF ! s_bdy

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        IF (w_bdy) THEN

!        DO J=JMS,JME
        DO J=JTS,min(JTE,JDE-1)
         DO I=1,LNSH
            bdy%pd_new_w(I,J)=grid3d%PD(IDS+I-1,J)

!        if (J .eq. JDE/2) then
!        write(0,*) ' === pd_new_w(I,J) === ', I,J, bdy%pd_new_w(I,J)
!        endif

         ENDDO
        ENDDO

        DO L=1,LM
!         DO J=JMS,JME
        DO J=JTS,min(JTE,JDE-1)
          DO I=1,LNSH
            bdy%t_new_w(I,J,L)=grid3d%T(IDS+I-1,J,L)
            bdy%q_new_w(I,J,L)=grid3d%Q(IDS+I-1,J,L)
            bdy%cwm_new_w(I,J,L)=grid3d%CWM(IDS+I-1,J,L)
          ENDDO
         ENDDO
        ENDDO

        DO L=1,LM
!         DO J=JMS,JME
        DO J=JTS,min(JTE,JDE-1)
          DO I=1,LNSV
            bdy%u_new_w(I,J,L)=grid3d%U(IDS+I-1,J,L)
            bdy%v_new_w(I,J,L)=grid3d%V(IDS+I-1,J,L)
          ENDDO
         ENDDO
        ENDDO

        ENDIF ! w_bdy

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        IF (e_bdy) THEN

!        DO J=JMS,JME
        DO J=JTS,min(JTE,JDE-1)
         DO I=1,LNSH
            bdy%pd_new_e(I,J)=grid3d%PD(I+IEND-lnsh,J)
         ENDDO
        ENDDO

        DO L=1,LM
!         DO J=JMS,JME
        DO J=JTS,min(JTE,JDE-1)
          DO I=1,LNSH
            bdy%t_new_e(I,J,L)=grid3d%T(I+IEND-lnsh,J,L)
            bdy%q_new_e(I,J,L)=grid3d%Q(I+IEND-lnsh,J,L)
            bdy%cwm_new_e(I,J,L)=grid3d%CWM(I+IEND-lnsh,J,L)
          ENDDO
         ENDDO
        ENDDO

        DO L=1,LM
!         DO J=JMS,JME
        DO J=JTS,min(JTE,JDE-1)
          DO I=1,LNSV
            bdy%u_new_e(I,J,L)=grid3d%U(I+IEND-lnsv-1,J,L)
            bdy%v_new_e(I,J,L)=grid3d%V(I+IEND-lnsv-1,J,L)
          ENDDO
         ENDDO
        ENDDO

        ENDIF ! e_bdy

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! ***
! ***       COMPUTE LOCAL DIFFERENCES, 
! ***       GENERATE GLOBAL BOUNDARY ARRAYS
! ***       WRITE boco FILE
! ***
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        IF (time .eq. 0) then

! local arrays

        ALLOCATE(bdy%pdb_n(ITS:min(ITE,IEND),1:LNSH,2))
        ALLOCATE(bdy%pdb_s(ITS:min(ITE,IEND),1:LNSH,2))
        ALLOCATE(bdy%pdb_w(1:LNSH,JTS:min(JTE,JEND),2))
        ALLOCATE(bdy%pdb_e(1:LNSH,JTS:min(JTE,JEND),2))

        ALLOCATE(bdy%tb_n(ITS:min(ITE,IEND),1:LNSH,LM,2))
        ALLOCATE(bdy%tb_s(ITS:min(ITE,IEND),1:LNSH,LM,2))
        ALLOCATE(bdy%tb_w(1:LNSH,JTS:min(JTE,JEND),LM,2))
        ALLOCATE(bdy%tb_e(1:LNSH,JTS:min(JTE,JEND),LM,2))

        ALLOCATE(bdy%qb_n(ITS:min(ITE,IEND),1:LNSH,LM,2))
        ALLOCATE(bdy%qb_s(ITS:min(ITE,IEND),1:LNSH,LM,2))
        ALLOCATE(bdy%qb_w(1:LNSH,JTS:min(JTE,JEND),LM,2))
        ALLOCATE(bdy%qb_e(1:LNSH,JTS:min(JTE,JEND),LM,2))

        ALLOCATE(bdy%cwmb_n(ITS:min(ITE,IEND),1:LNSH,LM,2))
        ALLOCATE(bdy%cwmb_s(ITS:min(ITE,IEND),1:LNSH,LM,2))
        ALLOCATE(bdy%cwmb_w(1:LNSH,JTS:min(JTE,JEND),LM,2))
        ALLOCATE(bdy%cwmb_e(1:LNSH,JTS:min(JTE,JEND),LM,2))

        ALLOCATE(bdy%ub_n(ITS:min(ITE,IEND),1:LNSV,LM,2))
        ALLOCATE(bdy%ub_s(ITS:min(ITE,IEND),1:LNSV,LM,2))
        ALLOCATE(bdy%ub_w(1:LNSV,JTS:min(JTE,JEND),LM,2))
        ALLOCATE(bdy%ub_e(1:LNSV,JTS:min(JTE,JEND),LM,2))

        ALLOCATE(bdy%vb_n(ITS:min(ITE,IEND),1:LNSV,LM,2))
        ALLOCATE(bdy%vb_s(ITS:min(ITE,IEND),1:LNSV,LM,2))
        ALLOCATE(bdy%vb_w(1:LNSV,JTS:min(JTE,JEND),LM,2))
        ALLOCATE(bdy%vb_e(1:LNSV,JTS:min(JTE,JEND),LM,2))

! ZERO N/S arrays

       DO M=1,2
        DO L=1,LM
         DO J=1,LNSH
          DO I=ITS,min(ITE,IEND)
           IF (L .eq. 1) THEN
             bdy%pdb_n(I,J,M)=0.
             bdy%pdb_s(I,J,M)=0.
           ENDIF
             bdy%tb_n(I,J,L,M)=0.
             bdy%tb_s(I,J,L,M)=0.
             bdy%qb_n(I,J,L,M)=0.
             bdy%qb_s(I,J,L,M)=0.
             bdy%cwmb_n(I,J,L,M)=0.
             bdy%cwmb_s(I,J,L,M)=0.
          ENDDO
         ENDDO
        ENDDO
       ENDDO

       DO M=1,2
        DO L=1,LM
         DO J=1,LNSV
          DO I=ITS,min(ITE,IEND)
             bdy%ub_n(I,J,L,M)=0.
             bdy%ub_s(I,J,L,M)=0.
             bdy%vb_n(I,J,L,M)=0.
             bdy%vb_s(I,J,L,M)=0.
          ENDDO
         ENDDO
        ENDDO
       ENDDO

! ZERO W/E arrays

       DO M=1,2
        DO L=1,LM
         DO J=JTS,min(JTE,JEND)
          DO I=1,LNSH
           IF (L .eq. 1) THEN
             bdy%pdb_w(I,J,M)=0.
             bdy%pdb_e(I,J,M)=0.
           ENDIF
             bdy%tb_w(I,J,L,M)=0.
             bdy%tb_e(I,J,L,M)=0.
             bdy%qb_w(I,J,L,M)=0.
             bdy%qb_e(I,J,L,M)=0.
             bdy%cwmb_w(I,J,L,M)=0.
             bdy%cwmb_e(I,J,L,M)=0.
          ENDDO
         ENDDO
        ENDDO
       ENDDO
        
       DO M=1,2
        DO L=1,LM
         DO J=JTS,min(JTE,JEND)
          DO I=1,LNSV
             bdy%ub_w(I,J,L,M)=0.
             bdy%ub_e(I,J,L,M)=0.
             bdy%vb_w(I,J,L,M)=0.
             bdy%vb_e(I,J,L,M)=0.
          ENDDO
         ENDDO
        ENDDO
       ENDDO

! global arrays

        ALLOCATE(bdy%pdb_n_g(IDS:IEND,1:LNSH,2)) 
        ALLOCATE(bdy%pdb_s_g(IDS:IEND,1:LNSH,2)) 
        ALLOCATE(bdy%pdb_w_g(1:LNSH,JDS:JEND,2)) 
        ALLOCATE(bdy%pdb_e_g(1:LNSH,JDS:JEND,2)) 

        ALLOCATE(bdy%tb_n_g(IDS:IEND,1:LNSH,LM,2))
        ALLOCATE(bdy%tb_s_g(IDS:IEND,1:LNSH,LM,2))
        ALLOCATE(bdy%tb_w_g(1:LNSH,JDS:JEND,LM,2))
        ALLOCATE(bdy%tb_e_g(1:LNSH,JDS:JEND,LM,2))

        ALLOCATE(bdy%qb_n_g(IDS:IEND,1:LNSH,LM,2))
        ALLOCATE(bdy%qb_s_g(IDS:IEND,1:LNSH,LM,2))
        ALLOCATE(bdy%qb_w_g(1:LNSH,JDS:JEND,LM,2))
        ALLOCATE(bdy%qb_e_g(1:LNSH,JDS:JEND,LM,2))

        ALLOCATE(bdy%cwmb_n_g(IDS:IEND,1:LNSH,LM,2))
        ALLOCATE(bdy%cwmb_s_g(IDS:IEND,1:LNSH,LM,2))
        ALLOCATE(bdy%cwmb_w_g(1:LNSH,JDS:JEND,LM,2))
        ALLOCATE(bdy%cwmb_e_g(1:LNSH,JDS:JEND,LM,2))

        ALLOCATE(bdy%ub_n_g(IDS:IEND,1:LNSV,LM,2))
        ALLOCATE(bdy%ub_s_g(IDS:IEND,1:LNSV,LM,2))
        ALLOCATE(bdy%ub_w_g(1:LNSV,JDS:JEND,LM,2))
        ALLOCATE(bdy%ub_e_g(1:LNSV,JDS:JEND,LM,2))

        ALLOCATE(bdy%vb_n_g(IDS:IEND,1:LNSV,LM,2))
        ALLOCATE(bdy%vb_s_g(IDS:IEND,1:LNSV,LM,2))
        ALLOCATE(bdy%vb_w_g(1:LNSV,JDS:JEND,LM,2))
        ALLOCATE(bdy%vb_e_g(1:LNSV,JDS:JEND,LM,2))

! ZERO N/S arrays

        if (MYPE .eq. 0) then

       DO M=1,2
        DO L=1,LM
         DO J=1,LNSH
          DO I=IDS,IEND
           IF (L .eq. 1) THEN
             bdy%pdb_n_g(I,J,M)=0.
             bdy%pdb_s_g(I,J,M)=0.
           ENDIF
             bdy%tb_n_g(I,J,L,M)=0.
             bdy%tb_s_g(I,J,L,M)=0.
             bdy%qb_n_g(I,J,L,M)=0.
             bdy%qb_s_g(I,J,L,M)=0.
             bdy%cwmb_n_g(I,J,L,M)=0.
             bdy%cwmb_s_g(I,J,L,M)=0.
          ENDDO
         ENDDO
        ENDDO
       ENDDO

       DO M=1,2
        DO L=1,LM
         DO J=1,LNSV
          DO I=IDS,IEND
             bdy%ub_n_g(I,J,L,M)=0.
             bdy%ub_s_g(I,J,L,M)=0.
             bdy%vb_n_g(I,J,L,M)=0.
             bdy%vb_s_g(I,J,L,M)=0.
          ENDDO
         ENDDO
        ENDDO
       ENDDO

! ZERO W/E arrays

       DO M=1,2
        DO L=1,LM
         DO J=JDS,JEND
          DO I=1,LNSH
           IF (L .eq. 1) THEN
             bdy%pdb_w_g(I,J,M)=0.
             bdy%pdb_e_g(I,J,M)=0.
           ENDIF
             bdy%tb_w_g(I,J,L,M)=0.
             bdy%tb_e_g(I,J,L,M)=0.
             bdy%qb_w_g(I,J,L,M)=0.
             bdy%qb_e_g(I,J,L,M)=0.
             bdy%cwmb_w_g(I,J,L,M)=0.
             bdy%cwmb_e_g(I,J,L,M)=0.
          ENDDO
         ENDDO
        ENDDO
       ENDDO
        
       DO M=1,2
        DO L=1,LM
         DO J=JDS,JEND
          DO I=1,LNSV
             bdy%ub_w_g(I,J,L,M)=0.
             bdy%ub_e_g(I,J,L,M)=0.
             bdy%vb_w_g(I,J,L,M)=0.
             bdy%vb_e_g(I,J,L,M)=0.
          ENDDO
         ENDDO
        ENDDO
       ENDDO

        endif ! only zero on root task
        
        ENDIF ! time=0

        IF (time .ge. 1) THEN 

        IF (n_bdy) THEN

        DO J=1,LNSH
!         DO I=IMS,IME
         DO I=ITS,min(ITE,IDE-1)
            bdy%pdb_n(I,J,1) =   bdy%pd_old_n(I,J)  
            bdy%pdb_n(I,J,2) = ( bdy%pd_new_n(I,J) - bdy%pd_old_n(I,J) ) / tboco
         ENDDO
        ENDDO

        DO L=1,LM
!        KFLIP=LM+1-L
         KFLIP=L

         DO J=1,LNSH
!          DO I=IMS,IME
         DO I=ITS,min(ITE,IDE-1)
            bdy%tb_n(I,J,L,1) =   bdy%t_old_n(I,J,KFLIP)  
            bdy%tb_n(I,J,L,2) = ( bdy%t_new_n(I,J,KFLIP)- bdy%t_old_n(I,J,KFLIP) ) / tboco
            bdy%qb_n(I,J,L,1) =   bdy%q_old_n(I,J,KFLIP) 
            bdy%qb_n(I,J,L,2) = ( bdy%q_new_n(I,J,KFLIP) - bdy%q_old_n(I,J,KFLIP) ) / tboco
            bdy%cwmb_n(I,J,L,1) =  bdy%cwm_old_n(I,J,KFLIP) 
            bdy%cwmb_n(I,J,L,2) =( bdy%cwm_new_n(I,J,KFLIP) - bdy%cwm_old_n(I,J,KFLIP) ) / tboco
          ENDDO
         ENDDO
        ENDDO

        DO L=1,LM
       KFLIP=LM+1-L
         KFLIP=L
         DO J=1,LNSV
!          DO I=IMS,IME
         DO I=ITS,min(ITE,IDE-1)
            bdy%ub_n(I,J,L,1)=   bdy%u_old_n(I,J,KFLIP) 
            bdy%ub_n(I,J,L,2)= ( bdy%u_new_n(I,J,KFLIP) - bdy%u_old_n(I,J,KFLIP) ) / tboco
            bdy%vb_n(I,J,L,1)=   bdy%v_old_n(I,J,KFLIP) 
            bdy%vb_n(I,J,L,2)= ( bdy%v_new_n(I,J,KFLIP) - bdy%v_old_n(I,J,KFLIP) ) / tboco
          ENDDO
         ENDDO
        ENDDO
 
        ENDIF ! n_bdy

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        IF (s_bdy) THEN

        DO J=1,LNSH
!         DO I=IMS,IME
         DO I=ITS,min(ITE,IDE-1)
            bdy%pdb_s(I,J,1)=   bdy%pd_old_s(I,J) 
            bdy%pdb_s(I,J,2)= ( bdy%pd_new_s(I,J) -  bdy%pd_old_s(I,J) ) / tboco
         ENDDO
        ENDDO

        DO L=1,LM
       KFLIP=LM+1-L
         KFLIP=L
         DO J=1,LNSH
!          DO I=IMS,IME
         DO I=ITS,min(ITE,IDE-1)
            bdy%tb_s(I,J,L,1)=   bdy%t_old_s(I,J,KFLIP) 
            bdy%tb_s(I,J,L,2)= ( bdy%t_new_s(I,J,KFLIP) - bdy%t_old_s(I,J,KFLIP) ) / tboco
            bdy%qb_s(I,J,L,1)=   bdy%q_old_s(I,J,KFLIP)
            bdy%qb_s(I,J,L,2)= ( bdy%q_new_s(I,J,KFLIP) -  bdy%q_old_s(I,J,KFLIP) ) / tboco
            bdy%cwmb_s(I,J,L,1)=   bdy%cwm_old_s(I,J,KFLIP) 
            bdy%cwmb_s(I,J,L,2)= ( bdy%cwm_new_s(I,J,KFLIP) - bdy%cwm_old_s(I,J,KFLIP) ) / tboco
          ENDDO
         ENDDO
        ENDDO

        DO L=1,LM
       KFLIP=LM+1-L
         KFLIP=L
         DO J=1,LNSV
!          DO I=IMS,IME
         DO I=ITS,min(ITE,IDE-1)
            bdy%ub_s(I,J,L,1)=   bdy%u_old_s(I,J,KFLIP) 
            bdy%ub_s(I,J,L,2)= ( bdy%u_new_s(I,J,KFLIP) - bdy%u_old_s(I,J,KFLIP) ) / tboco
            bdy%vb_s(I,J,L,1)=   bdy%v_old_s(I,J,KFLIP) 
            bdy%vb_s(I,J,L,2)= ( bdy%v_new_s(I,J,KFLIP) - bdy%v_old_s(I,J,KFLIP) ) / tboco
          ENDDO
         ENDDO
        ENDDO
 
        ENDIF ! s_bdy

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        IF (w_bdy) THEN

!        DO J=JMS,JME
!        write(0,*) 'JTS, JTS,min(JTE,JDE-1) defining pdb_w: ', JTS,min(JTE,JDE-1)
        DO J=JTS,min(JTE,JDE-1)
         DO I=1,LNSH
            bdy%pdb_w(I,J,1)=bdy%pd_old_w(I,J)
            bdy%pdb_w(I,J,2)= ( bdy%pd_new_w(I,J) - bdy%pd_old_w(I,J) ) / tboco
!        if (J .eq. JDE/2) then
!        write(0,*) ' === pd_w(I,J,:) === ', I,J, bdy%pdb_w(I,J,1), bdy%pdb_w(I,J,2)
!        endif
         ENDDO
        ENDDO

        DO L=1,LM
         KFLIP=L
!       KFLIP=LM+1-L
!         DO J=JMS,JME
        DO J=JTS,min(JTE,JDE-1)
          DO I=1,LNSH
            bdy%tb_w(I,J,L,1)=bdy%t_old_w(I,J,KFLIP)
            bdy%tb_w(I,J,L,2)= ( bdy%t_new_w(I,J,KFLIP) - bdy%t_old_w(I,J,KFLIP) ) / tboco

	if ( bdy%tb_w(I,J,L,1) .ge. 100 .and. bdy%tb_w(I,J,L,1) .le. 400) then
	else
	write(0,*) 'weird tb_w:: ', I,J,L, bdy%tb_w(I,J,L,1)
	endif

            bdy%qb_w(I,J,L,1)=bdy%q_old_w(I,J,KFLIP)
            bdy%qb_w(I,J,L,2)= ( bdy%q_new_w(I,J,KFLIP) - bdy%q_old_w(I,J,KFLIP) ) / tboco
            bdy%cwmb_w(I,J,L,1)=bdy%cwm_old_w(I,J,KFLIP)
            bdy%cwmb_w(I,J,L,2)= ( bdy%cwm_new_w(I,J,KFLIP) - bdy%cwm_old_w(I,J,KFLIP) ) / tboco
          ENDDO
         ENDDO
        ENDDO

        DO L=1,LM
!       KFLIP=LM+1-L
         KFLIP=L
!         DO J=JMS,JME
        DO J=JTS,min(JTE,JDE-1)
          DO I=1,LNSV
            bdy%ub_w(I,J,L,1)=bdy%u_old_w(I,J,KFLIP)
            bdy%ub_w(I,J,L,2)= ( bdy%u_new_w(I,J,KFLIP) - bdy%u_old_w(I,J,KFLIP) ) / tboco
            bdy%vb_w(I,J,L,1)=bdy%v_old_w(I,J,KFLIP)
            bdy%vb_w(I,J,L,2)= ( bdy%v_new_w(I,J,KFLIP) - bdy%v_old_w(I,J,KFLIP) ) / tboco
          ENDDO
         ENDDO
        ENDDO

        ENDIF ! w_bdy

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        IF (e_bdy) THEN

!        DO J=JMS,JME
        DO J=JTS,min(JTE,JDE-1)
         DO I=1,LNSH
            bdy%pdb_e(I,J,1)=bdy%pd_old_e(I,J)
            bdy%pdb_e(I,J,2)=( bdy%pd_new_e(I,J) - bdy%pd_old_e(I,J) ) / tboco
         ENDDO
        ENDDO

        DO L=1,LM
       KFLIP=LM+1-L
         KFLIP=L
!         DO J=JMS,JME
        DO J=JTS,min(JTE,JDE-1)
          DO I=1,LNSH
            bdy%tb_e(I,J,L,1)=bdy%t_old_e(I,J,KFLIP)
        if (J .eq. 10) then
!        write(0,*) 'BDY I,J,L, bdy%tb_e(I,J,L,1): ', I,J,L, bdy%tb_e(I,J,L,1)
        endif
            bdy%tb_e(I,J,L,2)= ( bdy%t_new_e(I,J,KFLIP) - bdy%t_old_e(I,J,KFLIP) ) / tboco
            bdy%qb_e(I,J,L,1)=bdy%q_old_e(I,J,KFLIP)
            bdy%qb_e(I,J,L,2)= ( bdy%q_new_e(I,J,KFLIP) - bdy%q_old_e(I,J,KFLIP) ) / tboco
            bdy%cwmb_e(I,J,L,1)=bdy%cwm_old_e(I,J,KFLIP)
            bdy%cwmb_e(I,J,L,2)= ( bdy%cwm_new_e(I,J,KFLIP) - bdy%cwm_old_e(I,J,KFLIP) ) / tboco
          ENDDO
         ENDDO
        ENDDO

        DO L=1,LM
       KFLIP=LM+1-L
         KFLIP=L
!         DO J=JMS,JME
        DO J=JTS,min(JTE,JDE-1)
          DO I=1,LNSV
            bdy%ub_e(I,J,L,1)=bdy%u_old_e(I,J,KFLIP)
            bdy%ub_e(I,J,L,2)= ( bdy%u_new_e(I,J,KFLIP) -  bdy%u_old_e(I,J,KFLIP) ) / tboco

!        if (J .eq. 5 .and. L .eq. 1) then
!        write(0,*) 'I, bdy%u_new_e(I,J,KFLIP), bdy%u_old_e(I,J,KFLIP): ', I, bdy%u_new_e(I,J,KFLIP), bdy%u_old_e(I,J,KFLIP)
!        write(0,*) 'I, bdy%ub_e(I,J,L,2): ', I, bdy%ub_e(I,J,L,2)
!        endif

!        if (J .eq. 12 .and. L .eq. 9) then
!        write(0,*) 'defining ub_e(1,12,9,1): ',  bdy%ub_e(I,J,L,1),   bdy%ub_e(I,J,L,2)
!        endif
            bdy%vb_e(I,J,L,1)=bdy%v_old_e(I,J,KFLIP)
            bdy%vb_e(I,J,L,2)=( bdy%v_new_e(I,J,KFLIP) -  bdy%v_old_e(I,J,KFLIP) ) / tboco
          ENDDO
         ENDDO
        ENDDO

        ENDIF ! e_bdy

        ENDIF ! time .ge. 1

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!**       MOVE "NEW" VALUES into "OLD" ARRAYS 
!**       FOR NEXT TIME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        IF (n_bdy) THEN

        DO J=1,LNSH
!         DO I=IMS,IME
         DO I=ITS,min(ITE,IDE-1)
            bdy%pd_old_n(I,J)=bdy%pd_new_n(I,J)
         ENDDO
        ENDDO

        DO L=1,LM
         DO J=1,LNSH
!          DO I=IMS,IME
         DO I=ITS,min(ITE,IDE-1)
            bdy%t_old_n(I,J,L)=bdy%t_new_n(I,J,L)
            bdy%q_old_n(I,J,L)=bdy%q_new_n(I,J,L)
            bdy%cwm_old_n(I,J,L)=bdy%cwm_new_n(I,J,L)
          ENDDO
         ENDDO
        ENDDO

        DO L=1,LM
         DO J=1,LNSV
!          DO I=IMS,IME
         DO I=ITS,min(ITE,IDE-1)
            bdy%u_old_n(I,J,L)=bdy%u_new_n(I,J,L)
            bdy%v_old_n(I,J,L)=bdy%v_new_n(I,J,L)
          ENDDO
         ENDDO
        ENDDO
 
        ENDIF ! n_bdy

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        IF (s_bdy) THEN

        DO J=1,LNSH
!         DO I=IMS,IME
         DO I=ITS,min(ITE,IDE-1)
            bdy%pd_old_s(I,J)=bdy%pd_new_s(I,J)
         ENDDO
        ENDDO

        DO L=1,LM
         DO J=1,LNSH
!          DO I=IMS,IME
         DO I=ITS,min(ITE,IDE-1)
            bdy%t_old_s(I,J,L)=bdy%t_new_s(I,J,L)
            bdy%q_old_s(I,J,L)=bdy%q_new_s(I,J,L)
            bdy%cwm_old_s(I,J,L)=bdy%cwm_new_s(I,J,L)
          ENDDO
         ENDDO
        ENDDO

        DO L=1,LM
         DO J=1,LNSV
!          DO I=IMS,IME
         DO I=ITS,min(ITE,IDE-1)
            bdy%u_old_s(I,J,L)=bdy%u_new_s(I,J,L)
            bdy%v_old_s(I,J,L)=bdy%v_new_s(I,J,L)
          ENDDO
         ENDDO
        ENDDO
 
        ENDIF ! s_bdy

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        IF (w_bdy) THEN

!        DO J=JMS,JME
        DO J=JTS,min(JTE,JDE-1)
         DO I=1,LNSH
            bdy%pd_old_w(I,J)=bdy%pd_new_w(I,J)
!        if (J .eq. JDE/2) then
!        write(0,*) ' === pd_old_w(I,J) === ', I,J, bdy%pd_old_w(I,J)
!        endif
         ENDDO
        ENDDO

        DO L=1,LM
!         DO J=JMS,JME
        DO J=JTS,min(JTE,JDE-1)
          DO I=1,LNSH
            bdy%t_old_w(I,J,L)=bdy%t_new_w(I,J,L)
            bdy%q_old_w(I,J,L)=bdy%q_new_w(I,J,L)
            bdy%cwm_old_w(I,J,L)=bdy%cwm_new_w(I,J,L)
          ENDDO
         ENDDO
        ENDDO

        DO L=1,LM
!         DO J=JMS,JME
        DO J=JTS,min(JTE,JDE-1)
          DO I=1,LNSV
            bdy%u_old_w(I,J,L)=bdy%u_new_w(I,J,L)
            bdy%v_old_w(I,J,L)=bdy%v_new_w(I,J,L)
          ENDDO
         ENDDO
        ENDDO

        ENDIF ! w_bdy

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        IF (e_bdy) THEN

!        DO J=JMS,JME
        DO J=JTS,min(JTE,JDE-1)
         DO I=1,LNSH
            bdy%pd_old_e(I,J)=bdy%pd_new_e(I,J)
         ENDDO
        ENDDO

        DO L=1,LM
!         DO J=JMS,JME
        DO J=JTS,min(JTE,JDE-1)
          DO I=1,LNSH
            bdy%t_old_e(I,J,L)=bdy%t_new_e(I,J,L)
            bdy%q_old_e(I,J,L)=bdy%q_new_e(I,J,L)
            bdy%cwm_old_e(I,J,L)=bdy%cwm_new_e(I,J,L)
          ENDDO
         ENDDO
        ENDDO

        DO L=1,LM
!         DO J=JMS,JME
        DO J=JTS,min(JTE,JDE-1)
          DO I=1,LNSV
            bdy%u_old_e(I,J,L)=bdy%u_new_e(I,J,L)
        if (J .eq. 12 .and. L .eq. 9) then
        write(0,*) 'defined bdy%u_old_e(I,12,9): ', bdy%u_old_e(I,J,L)
        endif
            bdy%v_old_e(I,J,L)=bdy%v_new_e(I,J,L)
          ENDDO
         ENDDO
        ENDDO

        ENDIF ! e_bdy

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!***
!***    NEED TO MAKE THESE ARRAYS GLOBAL IN SCOPE
!***    BEFORE WRITING THEM TO THE OUTPUT boco. FILES
!***
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        if (time .ge. 1) then

        FHR=(time-1)*tboco/3600.
!        write(esmf_boundary,'(A,I3.3)')'boco.',FHR

        if (mod(tboco,3600.) .ne. 0) then
        FMIN=(time-1)*tboco/60.

! alt form commented out below needs to subtract FHR to leave just minutes
!        write(esmf_boundary,'(A,I3.3,A,I2.2)')'boco.',FHR,'_',FMIN
        write(esmf_boundary,'(A,I3.3)')'boco.',FMIN
	else
	FMIN=0
        write(esmf_boundary,'(A,I4.4)')'boco.',FHR
	endif



        write(0,*) 'current_date: ', gridin%current_date
        call geth_newdate(bdy_date, gridin%current_date, -INT(tboco))
        write(0,*) 'bdy_date: ', bdy_date

        READ(bdy_date,FMT='(    I4)') idatbc(3)
        READ(bdy_date,FMT='( 5X,I2)') idatbc(2)
        READ(bdy_date,FMT='( 8X,I2)') idatbc(1)
        READ(bdy_date,FMT='(11X,I2)') IHRSTBC
        write(0,*) 'idatbc, IHRSTBC, FHR  ', idatbc, IHRSTBC, FHR

        runbc=.true.

        if (MYPE .eq. 0) then

        open_bdy: do l=51,99
          inquire(l,opened=opened)
          if(.not.opened)then
            iunit=l
            open(unit=iunit,file=esmf_boundary,status='new',form='unformatted')
            write(0,*) 'opening with iunit: ', iunit
            exit open_bdy
          endif
        end do open_bdy

        endif


!        write(0,*) 'call make_global_bdy_2d for pdb_w, MYPE, NPES, MPI_COMM_COMP ', MYPE, NPES, MPI_COMM_COMP

        do_work_loc=.false.
        if (w_bdy) then
        do_work_loc=.true.
        endif
#ifdef _MPI
!	write(0,*) 'call MPI_ALLGATHER'
        CALL MPI_ALLGATHER(do_work_loc,1,MPI_LOGICAL,do_work,1,MPI_LOGICAL, MPI_COMM_COMP, IERR)
!	write(0,*) 'return MPI_ALLGATHER'
#endif

        CALL make_global_bdy_2d(bdy%pdb_w, bdy%pdb_w_g, MYPE, NPES, MPI_COMM_COMP, &
                            1, LNSH, JDS, JEND, 1, 1                          , &
                             1, LNSH, JTS, min(JTE,JEND), 1, 1, do_work )

!        if (MYPE .eq. 0) then
!        write(0,*) 'MYPE, bdy%pdb_w_g(1) out of make_global_bdy_2d: ', MYPE, bdy%PDB_w_g(:,:,1)
!        endif


        do_work_loc=.false.
        if (e_bdy) then
        do_work_loc=.true.
        endif
#ifdef _MPI
        CALL MPI_ALLGATHER(do_work_loc,1,MPI_LOGICAL,do_work,1,MPI_LOGICAL, MPI_COMM_COMP, IERR)
#endif

!        write(0,*) 'pdb_e MYPE, CALL make_global_bdy_2d: ', MYPE, do_work
        CALL make_global_bdy_2d(bdy%pdb_e, bdy%pdb_e_g, MYPE, NPES, MPI_COMM_COMP, &
                             1, LNSH, JDS, JEND, 1, 1                          , &
                             1, LNSH, JTS, min(JTE,JEND), 1, 1, do_work )

        do_work_loc=.false.
        if (n_bdy) then
        do_work_loc=.true.
        endif
#ifdef _MPI
        CALL MPI_ALLGATHER(do_work_loc,1,MPI_LOGICAL,do_work,1,MPI_LOGICAL, MPI_COMM_COMP, IERR)
#endif

!        write(0,*) 'pdb_n MYPE, CALL make_global_bdy_2d: ', MYPE, do_work
        CALL make_global_bdy_2d(bdy%pdb_n, bdy%pdb_n_g, MYPE, NPES, MPI_COMM_COMP, &
                             IDS, IEND, 1, LNSH, 1, 1                          , &
                             ITS, min(ITE,IEND), 1, LNSH, 1, 1, do_work )

        do_work_loc=.false.
        if (s_bdy) then
        do_work_loc=.true.
        endif
#ifdef _MPI
        CALL MPI_ALLGATHER(do_work_loc,1,MPI_LOGICAL,do_work,1,MPI_LOGICAL, MPI_COMM_COMP, IERR)
#endif

!        write(0,*) 'pdb_s MYPE, CALL make_global_bdy_2d: ', MYPE, do_work
        CALL make_global_bdy_2d(bdy%pdb_s, bdy%pdb_s_g, MYPE, NPES, MPI_COMM_COMP, &
                             IDS, IEND, 1, LNSH, 1, 1                          , &
                             ITS, min(ITE,IEND), 1, LNSH, 1, 1, do_work )

!
!!!!!! MANY CHANGES HERE DOWN

        do_work_loc=.false.
        if (w_bdy) then
        do_work_loc=.true.
        endif
#ifdef _MPI
        CALL MPI_ALLGATHER(do_work_loc,1,MPI_LOGICAL,do_work,1,MPI_LOGICAL, MPI_COMM_COMP, IERR)
#endif

        CALL make_global_bdy(bdy%tb_w, bdy%tb_w_g, MYPE, NPES, MPI_COMM_COMP, &
                             1, LNSH, JDS, JEND, 1, LM                          , &
                             1, LNSH, JTS, min(JTE,JEND), 1, LM  , do_work )

	write(0,*) 'min, max of bdy%tb_w_g: ', minval(bdy%tb_w_g), maxval(bdy%tb_w_g)
	write(0,*) 'tb_w_g(:,84,1,1): ', bdy%tb_w_g(:,84,1,1)

        do_work_loc=.false.
        if (e_bdy) then
        do_work_loc=.true.
        endif
#ifdef _MPI
        CALL MPI_ALLGATHER(do_work_loc,1,MPI_LOGICAL,do_work,1,MPI_LOGICAL, MPI_COMM_COMP, IERR)
#endif

        CALL make_global_bdy(bdy%tb_e, bdy%tb_e_g, MYPE, NPES, MPI_COMM_COMP, &
                             1, LNSH, JDS, JEND, 1, LM                          , &
                             1, LNSH, JTS, min(JTE,JEND), 1, LM  , do_work )
	write(0,*) 'min, max of bdy%tb_e_g: ', minval(bdy%tb_e_g), maxval(bdy%tb_e_g)

        do_work_loc=.false.
        if (n_bdy) then
        do_work_loc=.true.
        endif
#ifdef _MPI
        CALL MPI_ALLGATHER(do_work_loc,1,MPI_LOGICAL,do_work,1,MPI_LOGICAL, MPI_COMM_COMP, IERR)
#endif

        CALL make_global_bdy(bdy%tb_n, bdy%tb_n_g, MYPE, NPES, MPI_COMM_COMP, &
                             IDS, IEND, 1, LNSH, 1, LM                          , &
                             ITS, min(ITE,IEND), 1, LNSH, 1, LM , do_work )
	write(0,*) 'min, max of bdy%tb_n_g: ', minval(bdy%tb_n_g), maxval(bdy%tb_n_g)

        do_work_loc=.false.
        if (s_bdy) then
        do_work_loc=.true.
        endif
#ifdef _MPI
        CALL MPI_ALLGATHER(do_work_loc,1,MPI_LOGICAL,do_work,1,MPI_LOGICAL, MPI_COMM_COMP, IERR)
#endif

        CALL make_global_bdy(bdy%tb_s, bdy%tb_s_g, MYPE, NPES, MPI_COMM_COMP, &
                             IDS, IEND, 1, LNSH, 1, LM                          , &
                             ITS, min(ITE,IEND), 1, LNSH, 1, LM , do_work )



!!!!!!!!!!!!!!!!!
!

        do_work_loc=.false.
        if (w_bdy) then
        do_work_loc=.true.
        endif
#ifdef _MPI
        CALL MPI_ALLGATHER(do_work_loc,1,MPI_LOGICAL,do_work,1,MPI_LOGICAL, MPI_COMM_COMP, IERR)
#endif

        CALL make_global_bdy(bdy%qb_w, bdy%qb_w_g, MYPE, NPES, MPI_COMM_COMP, &
                             1, LNSH, JDS, JEND, 1, LM                          , &
                             1, LNSH, JTS, min(JTE,JEND), 1, LM  , do_work )

        do_work_loc=.false.
        if (e_bdy) then
        do_work_loc=.true.
        endif
#ifdef _MPI
        CALL MPI_ALLGATHER(do_work_loc,1,MPI_LOGICAL,do_work,1,MPI_LOGICAL, MPI_COMM_COMP, IERR)
#endif

        CALL make_global_bdy(bdy%qb_e, bdy%qb_e_g, MYPE, NPES, MPI_COMM_COMP, &
                             1, LNSH, JDS, JEND, 1, LM                          , &
                             1, LNSH, JTS, min(JTE,JEND), 1, LM  , do_work )

        do_work_loc=.false.
        if (n_bdy) then
        do_work_loc=.true.
        endif
#ifdef _MPI
        CALL MPI_ALLGATHER(do_work_loc,1,MPI_LOGICAL,do_work,1,MPI_LOGICAL, MPI_COMM_COMP, IERR)
#endif

        CALL make_global_bdy(bdy%qb_n, bdy%qb_n_g, MYPE, NPES, MPI_COMM_COMP, &
                             IDS, IEND, 1, LNSH, 1, LM                          , &
                             ITS, min(ITE,IEND), 1, LNSH, 1, LM , do_work )

        do_work_loc=.false.
        if (s_bdy) then
        do_work_loc=.true.
        endif
#ifdef _MPI
        CALL MPI_ALLGATHER(do_work_loc,1,MPI_LOGICAL,do_work,1,MPI_LOGICAL, MPI_COMM_COMP, IERR)
#endif

        CALL make_global_bdy(bdy%qb_s, bdy%qb_s_g, MYPE, NPES, MPI_COMM_COMP, &
                             IDS, IEND, 1, LNSH, 1, LM                          , &
                             ITS, min(ITE,IEND), 1, LNSH, 1, LM , do_work )


!!!!!!!!!!!!!!!!!!!
!

        do_work_loc=.false.
        if (w_bdy) then
        do_work_loc=.true.
        endif
#ifdef _MPI
        CALL MPI_ALLGATHER(do_work_loc,1,MPI_LOGICAL,do_work,1,MPI_LOGICAL, MPI_COMM_COMP, IERR)
#endif

        CALL make_global_bdy(bdy%cwmb_w, bdy%cwmb_w_g, MYPE, NPES, MPI_COMM_COMP, &
                             1, LNSH, JDS, JEND, 1, LM                          , &
                             1, LNSH, JTS, min(JTE,JEND), 1, LM  , do_work )

        do_work_loc=.false.
        if (e_bdy) then
        do_work_loc=.true.
        endif
#ifdef _MPI
        CALL MPI_ALLGATHER(do_work_loc,1,MPI_LOGICAL,do_work,1,MPI_LOGICAL, MPI_COMM_COMP, IERR)
#endif

        CALL make_global_bdy(bdy%cwmb_e, bdy%cwmb_e_g, MYPE, NPES, MPI_COMM_COMP, &
                             1, LNSH, JDS, JEND, 1, LM                          , &
                             1, LNSH, JTS, min(JTE,JEND), 1, LM  , do_work )

        do_work_loc=.false.
        if (n_bdy) then
        do_work_loc=.true.
        endif
#ifdef _MPI
        CALL MPI_ALLGATHER(do_work_loc,1,MPI_LOGICAL,do_work,1,MPI_LOGICAL, MPI_COMM_COMP, IERR)
#endif

        CALL make_global_bdy(bdy%cwmb_n, bdy%cwmb_n_g, MYPE, NPES, MPI_COMM_COMP, &
                             IDS, IEND, 1, LNSH, 1, LM                          , &
                             ITS, min(ITE,IEND), 1, LNSH, 1, LM , do_work )

        do_work_loc=.false.
        if (n_bdy) then
        do_work_loc=.true.
        endif
#ifdef _MPI
        CALL MPI_ALLGATHER(do_work_loc,1,MPI_LOGICAL,do_work,1,MPI_LOGICAL, MPI_COMM_COMP, IERR)
#endif

        CALL make_global_bdy(bdy%cwmb_s, bdy%cwmb_s_g, MYPE, NPES, MPI_COMM_COMP, &
                             IDS, IEND, 1, LNSH, 1, LM                          , &
                             ITS, min(ITE,IEND), 1, LNSH, 1, LM , do_work )


!!!!!!!!!!!!!!!!
!
        do_work_loc=.false.
        if (w_bdy) then
        do_work_loc=.true.
        endif
#ifdef _MPI
        CALL MPI_ALLGATHER(do_work_loc,1,MPI_LOGICAL,do_work,1,MPI_LOGICAL, MPI_COMM_COMP, IERR)
#endif

        CALL make_global_bdy(bdy%ub_w, bdy%ub_w_g, MYPE, NPES, MPI_COMM_COMP, &
                             1, LNSV, JDS, JEND, 1, LM                          , &
                             1, LNSV, JTS, min(JTE,JEND), 1, LM , do_work )

        do_work_loc=.false.
        if (e_bdy) then
        do_work_loc=.true.
        endif
#ifdef _MPI
        CALL MPI_ALLGATHER(do_work_loc,1,MPI_LOGICAL,do_work,1,MPI_LOGICAL, MPI_COMM_COMP, IERR)
#endif

        CALL make_global_bdy(bdy%ub_e, bdy%ub_e_g, MYPE, NPES, MPI_COMM_COMP, &
                             1, LNSV, JDS, JEND, 1, LM                          , &
                             1, LNSV, JTS, min(JTE,JEND), 1, LM , do_work )

        do_work_loc=.false.
        if (n_bdy) then
        do_work_loc=.true.
        endif
#ifdef _MPI
        CALL MPI_ALLGATHER(do_work_loc,1,MPI_LOGICAL,do_work,1,MPI_LOGICAL, MPI_COMM_COMP, IERR)
#endif

        CALL make_global_bdy(bdy%ub_n, bdy%ub_n_g, MYPE, NPES, MPI_COMM_COMP, &
                             IDS, IEND, 1, LNSV, 1, LM                          , &
                             ITS, min(ITE,IEND), 1, LNSV, 1, LM , do_work )
        do_work_loc=.false.
        if (s_bdy) then
        do_work_loc=.true.
        endif
#ifdef _MPI
        CALL MPI_ALLGATHER(do_work_loc,1,MPI_LOGICAL,do_work,1,MPI_LOGICAL, MPI_COMM_COMP, IERR)
#endif

        CALL make_global_bdy(bdy%ub_s, bdy%ub_s_g, MYPE, NPES, MPI_COMM_COMP, &
                             IDS, IEND, 1, LNSV, 1, LM                          , &
                             ITS, min(ITE,IEND), 1, LNSV, 1, LM , do_work )

!!!!!!!!!!!!!!!!!!!!!!
!
        do_work_loc=.false.
        if (w_bdy) then
        do_work_loc=.true.
        endif
#ifdef _MPI
        CALL MPI_ALLGATHER(do_work_loc,1,MPI_LOGICAL,do_work,1,MPI_LOGICAL, MPI_COMM_COMP, IERR)
#endif

        CALL make_global_bdy(bdy%vb_w, bdy%vb_w_g, MYPE, NPES, MPI_COMM_COMP, &
                             1, LNSV, JDS, JEND, 1, LM                          , &
                             1, LNSV, JTS, min(JTE,JEND), 1, LM , do_work )

        do_work_loc=.false.
        if (e_bdy) then
        do_work_loc=.true.
        endif
#ifdef _MPI
        CALL MPI_ALLGATHER(do_work_loc,1,MPI_LOGICAL,do_work,1,MPI_LOGICAL, MPI_COMM_COMP, IERR)
#endif

        CALL make_global_bdy(bdy%vb_e, bdy%vb_e_g, MYPE, NPES, MPI_COMM_COMP, &
                             1, LNSV, JDS, JEND, 1, LM                          , &
                             1, LNSV, JTS, min(JTE,JEND), 1, LM , do_work )

        do_work_loc=.false.
        if (n_bdy) then
        do_work_loc=.true.
        endif
#ifdef _MPI
        CALL MPI_ALLGATHER(do_work_loc,1,MPI_LOGICAL,do_work,1,MPI_LOGICAL, MPI_COMM_COMP, IERR)
#endif

        CALL make_global_bdy(bdy%vb_n, bdy%vb_n_g, MYPE, NPES, MPI_COMM_COMP, &
                             IDS, IEND, 1, LNSV, 1, LM                          , &
                             ITS, min(ITE,IEND), 1, LNSV, 1, LM , do_work )
        do_work_loc=.false.
        if (s_bdy) then
        do_work_loc=.true.
        endif
#ifdef _MPI
        CALL MPI_ALLGATHER(do_work_loc,1,MPI_LOGICAL,do_work,1,MPI_LOGICAL, MPI_COMM_COMP, IERR)
#endif

        CALL make_global_bdy(bdy%vb_s, bdy%vb_s_g, MYPE, NPES, MPI_COMM_COMP, &
                             IDS, IEND, 1, LNSV, 1, LM                          , &
                             ITS, min(ITE,IEND), 1, LNSV, 1, LM , do_work )


!        write(0,*) 'to mpi_barrier call : ', my_proc_id

!        call MPI_BARRIER(IERR)

        write(0,*) 'past all make_global_bdy calls... ', my_proc_id

        if (MYPE .eq. 0) then

        write(iunit) runbc, idatbc, IHRSTBC, tboco
        write(iunit) bdy%pdb_s_g, bdy%pdb_n_g, bdy%pdb_w_g, bdy%pdb_e_g
        write(iunit) bdy%tb_s_g, bdy%tb_n_g, bdy%tb_w_g, bdy%tb_e_g
        write(iunit) bdy%qb_s_g, bdy%qb_n_g, bdy%qb_w_g, bdy%qb_e_g
        write(iunit) bdy%cwmb_s_g, bdy%cwmb_n_g, bdy%cwmb_w_g, bdy%cwmb_e_g
        write(iunit) bdy%ub_s_g, bdy%ub_n_g, bdy%ub_w_g, bdy%ub_e_g
        write(iunit) bdy%vb_s_g, bdy%vb_n_g, bdy%vb_w_g, bdy%vb_e_g
        close(iunit)

        endif

        endif



        write(0,*) 'leave proc_bdy'

        END SUBROUTINE proc_bdy


        end module boundary
