










c***********************************************************************
c
c
c***********************************************************************
      subroutine computegradient(
     &  ifirst0,ilast0,ifirst1,ilast1,
     &  gcw0,gcw1,
     &  xo, yo, uo, dux, duy)
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
     &     uo(ifirst0-gcw0:ilast0+gcw0,
     &          ifirst1-gcw1:ilast1+gcw1,2+2),
     &     dux(ifirst0-gcw0:ilast0+gcw0,
     &          ifirst1-gcw1:ilast1+gcw1,2+2),
     &     duy(ifirst0-gcw0:ilast0+gcw0,
     &          ifirst1-gcw1:ilast1+gcw1,2+2)
c
      integer J,K,L
      double precision
     &  DUL,DUR
c
c***********************************************************************     
C
      DO K=ifirst1,ilast1
      DO J=ifirst0,ilast0
         DO L=1,4
            DUL=(UO(J,K,L)-UO(J-1,K,L))/(XO(J,K)-XO(J-1,K))
            DUR=(UO(J+1,K,L)-UO(J,K,L))/(XO(J+1,K)-XO(J,K))
            DUX(J,K,L)= 0.5*(SIGN(1.0D0,DUL)+SIGN(1.0D0,DUR))
     &                 *2.0*ABS(DUL*DUR)/(ABS(DUL)+ABS(DUR)+1.0E-10)
         ENDDO
      ENDDO
      ENDDO
C
      DO K=ifirst1,ilast1 
      DO J=ifirst0,ilast0  
          DO L=1,4
             DUL=(UO(J,K,L)-UO(J,K-1,L))/(YO(J,K)-YO(J,K-1))
             DUR=(UO(J,K+1,L)-UO(J,K,L))/(YO(J,K+1)-YO(J,K))
             DUY(J,K,L)= 0.5*(SIGN(1.0D0,DUL)+SIGN(1.0D0,DUR))
     &                *2.0*ABS(DUL*DUR)/(ABS(DUL)+ABS(DUR)+1.0E-10)
          ENDDO
      ENDDO      
      ENDDO      
c
      return
      end
