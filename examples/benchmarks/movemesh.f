










c***********************************************************************
c
c
c***********************************************************************
      subroutine movemesh(
     &  ifirst0,ilast0,ifirst1,ilast1,
     &  gcw0,gcw1,
     &  xo, yo, wo, xn, yn)
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
     &     xo(ifirst0-gcw0:ilast0+1+gcw0,
     &          ifirst1-gcw1:ilast1+1+gcw1),
     &     yo(ifirst0-gcw0:ilast0+1+gcw0,
     &          ifirst1-gcw1:ilast1+1+gcw1),
     &     wo(ifirst0-gcw0:ilast0+gcw0,
     &          ifirst1-gcw1:ilast1+gcw1),
     &     xn(ifirst0:ilast0+1,
     &          ifirst1:ilast1+1),
     &     yn(ifirst0:ilast0+1,
     &          ifirst1:ilast1+1)
c
      integer J,K
      double precision
     &  CXP,CXM,CYP,CYM
c
c***********************************************************************     
C
      DO K=ifirst1,ilast1+1
      DO J=ifirst0,ilast0+1
         CXP=0.5*(WO(J,K)+WO(J,K-1))
         CXM=0.5*(WO(J-1,K)+WO(J-1,K-1))                       
         CYP=0.5*(WO(J,K)+WO(J-1,K))          
         CYM=0.5*(WO(J,K-1)+WO(J-1,K-1))
         XN(J,K)=( CXP*XO(J+1,K)+CXM*XO(J-1,K)
     &            +CYP*XO(J,K+1)+CYM*XO(J,K-1) )/(CXP+CXM+CYP+CYM)        
         YN(J,K)=( CXP*YO(J+1,K)+CXM*YO(J-1,K)
     &            +CYP*YO(J,K+1)+CYM*YO(J,K-1) )/(CXP+CXM+CYP+CYM)
      ENDDO
      ENDDO
c
      return
      end
