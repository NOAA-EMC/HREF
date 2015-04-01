        program generate_levs


        real :: input_levs(41)

        real :: outlevs50(51)
        real :: outlevs45(46)


!        data input_levs/1.0000, .9930, .9849, &
!                       .9756, .9649, .9525, .9382, .9215, .9019, &
!                       .8790, .8521, .8209, .7853, &
!                       .7454, .7016, .6546, .6052, .5545, &
!                       .5039, .4548, .4088, .3669, &
!                       .3295, .2965, .2675, .2413, .2169, &
!                       .1937, .1712, .1496, .1290, &
!                       .1094, .0912, .0743, .0588, .0448, &
!                       .0325, .0218, .0128, .0055, 0./

        data input_levs/1.0000, 0.994,   0.9875,  0.9805, &
                        0.973 , 0.9648, 0.9558, 0.9460, 0.9350, 0.9215, &
                        0.9045, 0.8825, 0.8545, 0.8185, 0.7745, 0.7235, &
                        0.6695, 0.6140, 0.5580, 0.5035, &
                        0.4515, 0.4025, 0.3575, 0.3195, 0.2865, &
                        0.2575, 0.2310, 0.2070, 0.1840, 0.1620,&
                        0.1414, 0.1218, 0.1036, 0.0867, 0.0712, 0.0572,  &
                        0.0444, 0.0324, 0.0209, 0.0099, 0.0000/


        call bilin_interp(input_levs, 41, outlevs50, 51)

!        call bilin_interp(input_levs, 41, outlevs45, 46)


        end program generate_levs


        subroutine bilin_interp(inlevs,ninput,outlevs,noutput)

        real, intent(in):: inlevs(ninput)
        real, intent(out):: outlevs(noutput)
        integer:: I, J

        real :: indelt(ninput-1), inratio(ninput-1)
        real :: outdelt(noutput-1), outratio(noutput-1)

        real :: d1, d2, dtot, f1, f2


        do I=1,ninput
!        indelt(I)=inlevs(I)-inlevs(I+1)
        inratio(I)=float(I)/float(ninput)
!        write(0,*) 'I, inratio, indelt: ', I, inratio(I), indelt(I)
        enddo


        outlevs(1)=1.0

        do I=2,noutput

        outratio(I)=float(I)/(float(noutput))
!        write(0,*) 'I, outdelt: ', I, outratio(I)

    J_search: do J=1,ninput-1

        if(inratio(J).le.outratio(I) .and. inratio(J+1).ge. &
                                    outratio(I)) then
!        write(0,*) 'compute for: ', outratio(I), 'using ' ,&
!           J, J+1, inratio(J),inratio(J+1)

        d1=outratio(I)-inratio(J)
        d2=inratio(J+1)-outratio(I)
        dtot=inratio(J+1)-inratio(J)

        f1=1.-d1/dtot
        f2=1.-d2/dtot

!        write(0,*) 'f1, f2, inlevs vals: ', f1, f2, inlevs(J),inlevs(J+1)
        outlevs(I)=f1*inlevs(J)+f2*inlevs(J+1)

        exit J_search
        endif

        if (I .eq. noutput .and. J .eq. ninput-1) then
         outlevs(noutput)=0.
        endif

        end do J_search

        enddo

  632   format(9(f6.4,',',1x))
  633   format(51(f6.4,','))

        write(0,632) (outlevs(I),i=1,9)
        write(0,632) (outlevs(I),i=10,18)
        write(0,632) (outlevs(I),i=19,27)
        write(0,632) (outlevs(I),i=28,36)
        write(0,632) (outlevs(I),i=37,45)
        write(0,632) (outlevs(I),i=46,51)
        write(0,*) ' '
        write(0,633) (outlevs(I),i=noutput,1,-1)

        end subroutine bilin_interp
