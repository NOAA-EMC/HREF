call emduct(grid1,grid2,grid3,grid4,grid5,ref,zmid) 

      subroutine emduct 
     1  (dctbh,dctth,dctst,dcttp,dctme,ref,z)
#include <emduct.prol>
c rcs keywords: $RCSfile: emduct.F,v $
c SCCS IDENTIFICATION:  %W% %G%
c
c   subroutine computes the duct base height, strength and thickness
c
!      use domdec
!      implicit none
      
      INCLUDE "parmout"
      INCLUDE "params"
C     INCLUDE REQUIRED COMMONS.
!      INCLUDE "RQSTFLD.comm"
      INCLUDE "CTLBLK.comm"
    
!      integer nn
c
c     dctbh  = duct base height
c     dctth  = duct thickness   
c     dctst  = duct strength ht
c     dcttp  = duct top  height 
c     dctme  = duct m-deficit


      real ref    (im,JSTA_2L:JEND_2U,lm)
      real z      (im,JSTA_2L:JEND_2U,lm)
      real dctbh  (im,jm)
      real dctth  (im,jm)
      real dctst  (im,jm)
      real dcttp  (im,jm)
      real dctme  (im,jm)
c
      integer km
      integer l
      integer lmax 
c
c***********************************************************************
c       local variables
c***********************************************************************
c
      integer i
      integer j
      integer k
      integer krm  (2)  
c
      real    dbh  (2)  
      real    dth  (2)  
      real    dst  (2)  
      real    rmax (2)
      real    rmin (2)
c
c***********************************************************************
c  Interior plus one boundary row
c***********************************************************************

!      ilo1 = ilo1_nest(nn)
!      ihi1 = ihi1_nest(nn)
!      jlo1 = jlo1_nest(nn)
!      jhi1 = jhi1_nest(nn)

c***********************************************************************
c          loop over each grid point
c***********************************************************************
c
      do j=jsta,jend
       do i=1,im
c
         lmax=0
         l=1
c***********************************************************************
c          loop from bottom level up 
c          to find minimum in ref - checks for up to two mins and uses
c          the one that gives surface based ducting regardless of its
c          strength
c***********************************************************************
c
         do k=lm-1,2,-1         
           if(ref(i,j,k).lt.ref(i,j,k-1).and.
     1        ref(i,j,k).lt.ref(i,j,k+1)) then
             rmin(l)=ref(i,j,k)
             krm(l)=k
             lmax=l
             l=l+1
             if (l.eq.3) then
               goto 19
             endif
           endif
         enddo
 19      continue
c
c***********************************************************************
c if trapping layer not found set values for no ducting in 
c duct base height, strength, thickness and go to next grid point
c***********************************************************************
c
         if (lmax.eq.0) then
           dctbh(i,j)=-99.9  
           dctst(i,j)=-99.9   
           dctth(i,j)=-99.9  
           dcttp(i,j)=-99.9  
           dctme(i,j)=-99.9   
           goto 18
         endif

c***********************************************************************
c if found a trapping layer
c find rmin below to get duct base height, strength, thickness
c***********************************************************************
c
         do l=1,lmax

           km=krm(l)

           do k=km+1,lm
             if(ref(i,j,k).lt.rmin(l)) then
               dbh(l)=z(i,j,k)  
               dth(l)=z(i,j,km)-z(i,j,k)   
               goto 21 
             endif
           enddo
           dbh(l)=-51.0
           dth(l)=z(i,j,km)
 21        continue
           do k=km+1,lm
             if(ref(i,j,k).gt.ref(i,j,k-1).and.
     1         ref(i,j,k).gt.ref(i,j,k+1)) then
               rmax(l)=ref(i,j,k)
               dst(l)=rmax(l)-rmin(l)
               goto 22
             endif
           enddo
           dst(l)=ref(i,j,lm)-rmin(l)
 22        continue

         enddo
c
c***********************************************************************
c choose duct based upon which has lowest duct base height
c***********************************************************************
c
         if (lmax.eq.1) then
            dctst(i,j)=dst(1)
            dctbh(i,j)=dbh(1)
            dctth(i,j)=dth(1)
            dcttp(i,j)=z(i,j,krm(1))  
            dctme(i,j)=ref(i,j,lm)-rmin(1)
            if (dctme(i,j).gt.0.0) then
              dctme(i,j)= 6.0
            endif
         elseif(dbh(1).lt.dbh(2)) then
            dctst(i,j)=dst(1)
            dctbh(i,j)=dbh(1)
            dctth(i,j)=dth(1)
            dcttp(i,j)=z(i,j,krm(1))  
            dctme(i,j)=ref(i,j,lm)-rmin(1)
            if (dctme(i,j).gt.0.0) then
              dctme(i,j)=  6.0
            endif
         else
            dctst(i,j)=dst(2)
            dctbh(i,j)=dbh(2)
            dctth(i,j)=dth(2)
            dcttp(i,j)=z(i,j,krm(2))  
            dctme(i,j)=ref(i,j,lm)-rmin(2)
            if (dctme(i,j).gt.0.0) then
              dctme(i,j)=  6.0
            endif
         endif
c
c***********************************************************************
c if duct to weak, set values to no duct
c***********************************************************************
c
         if (dctst(i,j).lt.3.0) then
           dctbh(i,j)=-99.9  
           dctst(i,j)=-99.9  
           dctth(i,j)=-99.9  
           dcttp(i,j)=-99.9  
           dctme(i,j)=-99.9   
         endif

 18      continue
c
       enddo  
       enddo  
c
c***********************************************************************
c
      return
      end
