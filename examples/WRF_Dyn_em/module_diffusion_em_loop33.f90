SUBROUTINE horizontal_diffusion_u_2(titau1avg,titau2avg, zx_at_u, zy_at_u, titau1, titau2, zx, zy,&
    fnm, fnp,n1,n2,n3)

   INTEGER :: n1,n2,n3,i,j,k
   REAL(kind=8), DIMENSION(100,100,100) :: titau1avg,titau2avg, zx_at_u, zy_at_u, titau1, titau2, zx, zy
   REAL(kind=8), DIMENSION(100) :: fnm, fnp


DO j = 1, 100
    DO k = 1+1,100
    DO i = 1, 100
       titau1avg(i,k,j)=0.5*(fnm(k)*(titau1(i-1,k  ,j)+titau1(i,k  ,j))+ &
                             fnp(k)*(titau1(i-1,k-1,j)+titau1(i,k-1,j)))
       titau2avg(i,k,j)=0.5*(fnm(k)*(titau2(i,k  ,j+1)+titau2(i,k  ,j))+ &
                             fnp(k)*(titau2(i,k-1,j+1)+titau2(i,k-1,j)))
 !     tmpzy = 0.25*( zy(i-1,k,j  )+zy(i,k,j  )+ &
 !                    zy(i-1,k,j+1)+zy(i,k,j+1)  )
 !      tmpzeta_z = 0.5*(zeta_z(i,j)+zeta_z(i-1,j))
 !      titau1avg(i,k,j)=titau1avg(i,k,j)*zx(i,k,j)*tmpzeta_z
 !      titau2avg(i,k,j)=titau2avg(i,k,j)*tmpzy    *tmpzeta_z
       zx_at_u(i, k, j) = 0.5 * (zx(i, k, j) + zx(i, k + 1 , j))
       zy_at_u(i, k, j) = 0.125 * (zy(i - 1, k, j) + zy(i, k, j) + &
           zy(i - 1, k, j + 1) + zy(i, k, j + 1) + zy(i - 1, k + 1, j) + &
           zy(i, k + 1, j) + zy(i - 1, k + 1, j + 1) + zy(i, k + 1, j + 1))
 !     titau1avg(i,k,j)=titau1avg(i,k,j)*zx(i,k,j)
 !     titau2avg(i,k,j)=titau2avg(i,k,j)*tmpzy
    ENDDO
    ENDDO
    ENDDO

end  SUBROUTINE
