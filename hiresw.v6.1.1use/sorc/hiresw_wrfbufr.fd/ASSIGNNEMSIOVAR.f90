      subroutine assignnemsiovar(im,jsta,jend,jsta_2l,jend_2u &
      ,l,nrec,fldsize &
      ,spval,tmp,recname,reclevtyp,reclev,VarName,VcoordName &
      ,buf)
!      
      implicit none
      INCLUDE "mpif.h"
!
      integer,intent(in) :: im,jsta,jend,jsta_2l,jend_2u,l,nrec,fldsize
      integer,intent(in) :: reclev(nrec)
      real,intent(in) :: spval,tmp(fldsize*nrec)
      character*8,intent(in) :: recname(nrec)
      character*16,intent(in) :: reclevtyp(nrec)
      character(len=20),intent(in) :: VarName,VcoordName
      real,intent(out) :: buf(im,jsta_2l:jend_2u)
      integer :: fldst,recn,js,j,i 
      
!        write(0,*) 'inside assignnemsiovar'
!        write(0,*) 'varname, vcoordname: ', varname, vcoordname
!        write(0,*) 'im,jsta,jend,jsta_2l,jend_2u: ', &
!                    im,jsta,jend,jsta_2l,jend_2u
!        write(0,*) 'L,nrec,fldsize: ', L,nrec,fldsize
!        write(0,*) 'size(tmp,recname,reclevtyp,reclev: ', & 
!                size(tmp),size(recname),size(reclevtyp),size(reclev)
!        write(0,*) 'size(buf): ', size(buf)

!        write(0,*) 'varname(1:8) into getrecn: ', varname(1:8)
!        write(0,*) 'recname: ', recname(1:5)
!        write(0,*) 'reclevtyp: ', reclevtyp(1:5)
!        write(0,*) 'vcoordname: ', vcoordname(1:5)

      call getrecn(recname,reclevtyp,reclev,nrec,varname(1:8),VcoordName,l,recn)
!        write(0,*) 'recn: ', recn
      if(recn/=0) then
        fldst=(recn-1)*fldsize
!        write(0,*) 'fldsize: ', fldsize
!        write(0,*) 'fldst: ', fldst


        do j=jsta,jend
          js=(j-jsta)*im
          do i=1,im
            buf(i,j)=tmp(i+js+fldst)
          enddo
        enddo
      else
        print*,'fail to read ', varname, ' assign missing value'
        buf=spval
      endif


      RETURN
      END   

!-----------------------------------------------------------------------
!#######################################################################
!-----------------------------------------------------------------------
!
      SUBROUTINE getrecn(recname,reclevtyp,reclev,nrec,fldname,          &
                         fldlevtyp,fldlev,recn)
!-----------------------------------------------------------------------
!-- this subroutine searches the field list to find out a specific field,
!-- and return the field number for that field
!-----------------------------------------------------------------------
!
        implicit none
!
        integer,intent(in)      :: nrec
        character(*),intent(in) :: recname(nrec)
        character(*),intent(in) :: reclevtyp(nrec)
        integer,intent(in)      :: reclev(nrec)
        character(*),intent(in) :: fldname
        character(*),intent(in) :: fldlevtyp
        integer,intent(in)      :: fldlev
        integer,intent(out)     :: recn
!
        integer i
!
        recn=0
        do i=1,nrec
          if(trim(recname(i))==trim(fldname).and.                        &
            trim(reclevtyp(i))==trim(fldlevtyp) .and.                    &
            reclev(i)==fldlev) then
            recn=i
            return
          endif
        enddo
!
        if(recn==0) print *,'WARNING: field ',trim(fldname),' ',         &
          trim(fldlevtyp),' ',fldlev,' is not in the nemsio file!'
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE getrecn 
