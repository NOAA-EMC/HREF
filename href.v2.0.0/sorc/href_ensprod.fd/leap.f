        integer y,m,d,h,yy,mm,dd,hh
        write(*,*) 'input: y, m, d, m'
        read (*,*) y, m , d, h
        call get_ymd(y,m,d,h,yy,mm,dd,hh)
        write(*,*) yy,mm,dd,hh
        stop
        end


c      PROGRAM LEAPYR
!     Variables
c      IMPLICIT NONE
c      INTEGER  :: YEAR
c      LOGICAL  :: LEAP
c      WRITE (UNIT=*, FMT='(A)', ADVANCE='NO') ' Enter year:  '
c      READ (UNIT=*, FMT=*) YEAR
c      WRITE (UNIT=*, FMT='(1X,L1)') LEAP(YEAR)
c      if( LEAP(YEAR).eq..TRUE.) write(*,*) 'Feb=', 29
c      if( LEAP(YEAR).eq..FALSE.) write(*,*) 'Feb=', 28
c      STOP
c      END

      subroutine get_ymd (y,m,d,h, yy,mm,dd,hh)
      integer y,m,d,h, yy,mm,dd,hh
      integer md
      logical leap

      yy=y
      mm=m
      dd=d
      hh=h

       if (m.eq.1.or.m.eq.3.or.m.eq.5.or.m.eq.7.or.
     +     m.eq.8.or.m.eq.10.or.m.eq.12) then
         md=31
       else if (m.eq.4.or.m.eq.6.or.m.eq.9.or.m.eq.11) then
         md=30
       else
         if(leap(y).eq..true.) then
           md=29
         else
           md=28
         end if
       end if

       if(h.ge.24) then
         hh=mod(h,24)
         kd=int(h/24) 
         dd=d+kd
         if (dd.gt.md) then
           dd=dd-md
           mm=m+1
           if (mm.gt.12) then
            mm=1
            yy=y+1
           end if
         end if
       end if

       return
       end


!***********************************************************************************************************************************
!  LEAP
!
!  Input:
!     year  -  Gregorian year (integer)
!  Output:
!     Function return value = .true. if year is a leap year, and .false.
!     otherwise.
!***********************************************************************************************************************************

      FUNCTION LEAP (YEAR) RESULT (LEAPFLAG)

      IMPLICIT NONE

      INTEGER :: YEAR
      LOGICAL :: LEAPFLAG

      LEAPFLAG = .FALSE.
      IF (MOD(YEAR,4) .EQ. 0)   LEAPFLAG = .TRUE.
      IF (MOD(YEAR,100) .EQ. 0) LEAPFLAG = .FALSE.
      IF (MOD(YEAR,400) .EQ. 0) LEAPFLAG = .TRUE.
      RETURN
      END

