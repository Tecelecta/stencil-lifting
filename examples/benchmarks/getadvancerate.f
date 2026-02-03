c***********************************************************************
c***********************************************************************
c***********************************************************************
  
      subroutine getadvancerate(ifirst0,ilast0,
     &                      ifirst1,ilast1,
     &                      ifirst2,ilast2,
     &                      dx,
     &                      source,flux0,flux1,flux2,
     &                      advancerate)
c***********************************************************************
      implicit none

c***********************************************************************
      integer ifirst0, ilast0,ifirst1, ilast1,ifirst2,ilast2
      double precision dx(0:3-1)
      double precision
     &     flux0(ifirst0:ilast0+1,
     &          ifirst1:ilast1,
     &          ifirst2:ilast2,5),
     &     flux1(ifirst1:ilast1+1,
     &          ifirst2:ilast2,
     &          ifirst0:ilast0,5),
     &     flux2(ifirst2:ilast2+1,
     &          ifirst0:ilast0,
     &          ifirst1:ilast1,5)
       double precision 
     &     source(ifirst0:ilast0,
     &          ifirst1:ilast1,
     &          ifirst2:ilast2,1:5),
     &     advancerate(ifirst0:ilast0,
     &          ifirst1:ilast1,
     &          ifirst2:ilast2,1:5)


      integer ic0,ic1,ic2,k
      
c***********************************************************************
c***********************************************************************
      do k = 1 , 5
      do ic2=ifirst2,ilast2
       do ic1=ifirst1,ilast1
        do ic0=ifirst0,ilast0
           advancerate(ic0,ic1,ic2,k)=
     &          -(flux0(ic0+1,ic1,ic2,k)-flux0(ic0,ic1,ic2,k))/dx(0)
     &          -(flux1(ic1+1,ic2,ic0,k)-flux1(ic1,ic2,ic0,k))/dx(1)
     &          -(flux2(ic2+1,ic0,ic1,k)-flux2(ic2,ic0,ic1,k))/dx(2)
     &          + source(ic0,ic1,ic2,k)
        enddo
       enddo
      enddo
      enddo
      return
      end
     
