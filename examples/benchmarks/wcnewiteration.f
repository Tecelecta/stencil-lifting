










c***********************************************************************
c
c     迭代求解控制函数.
c
c***********************************************************************
      subroutine wcnewiteration(
     &  ifirst0,ilast0,ifirst1,ilast1,
     &  gcw0,gcw1,wo,wn)
c***********************************************************************
      implicit none
c***********************************************************************
c***********************************************************************     
c input arrays:
      integer ifirst0,ilast0,ifirst1,ilast1
      integer gcw0,gcw1
c
c variables in 2d indexed
c
      double precision
     &     wo(ifirst0-gcw0:ilast0+gcw0,
     &          ifirst1-gcw1:ilast1+gcw1),
     &     wn(ifirst0:ilast0,
     &          ifirst1:ilast1)
c
      integer J,K
c
c***********************************************************************     
C
      DO K=ifirst1,ilast1
      DO J=ifirst0,ilast0
         WN(J,K)=WO(J,K)/4.0
     &        +(WO(J+1,K)+WO(J-1,K)+WO(J,K+1)+WO(J,K-1))/8.
     &        +(WO(J+1,K+1)+WO(J+1,K-1)+WO(J-1,K+1)+WO(J-1,K-1))/16.
      ENDDO
      ENDDO
c
      return
      end
