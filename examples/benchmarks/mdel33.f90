SUBROUTINE horizontal_diffusion_u_2( &
    titau1avg, titau2avg, zx_at_u, zy_at_u, &
    titau1, titau2, fnm, fnp, zx, zy,              &
    jts, jte, kts, kte, its, ite        )

    INTEGER :: jts, jte, kts, kte, its, ite 
    REAL(kind=8), DIMENSION( kts:kte ) ::    fnm, fnp !1p*2
    REAL(kind=8), DIMENSION( its:ite, kts:kte+1, jts:jte) :: zx !2p
    REAL(kind=8), DIMENSION( its-1:ite, kts:kte+1, jts:jte+1) :: zy !8p
    REAL(kind=8), DIMENSION( its-1:ite+1, kts:kte, jts-1:jte+1) :: titau1, & !4p
                                                                   titau2 !4p
    REAL(kind=8), DIMENSION( its:ite, kts:kte, jts:jte) :: zx_at_u,&
                                                           zy_at_u,&
                                                           titau1avg, &
                                                           titau2avg !4o

DO j = jts, jte
    DO k = kts+1, kte
        DO i = its, ite
            titau1avg(i,k,j)=0.5 * (fnm(k)*(titau1(i-1,k  ,j)+titau1(i,k  ,j))+ &
                                    fnp(k)*(titau1(i-1,k-1,j)+titau1(i,k-1,j)))
            titau2avg(i,k,j)=0.5 * (fnm(k)*(titau2(i,k  ,j+1)+titau2(i,k  ,j))+ &
                                    fnp(k)*(titau2(i,k-1,j+1)+titau2(i,k-1,j)))
        !   tmpzy = 0.25*( zy(i-1,k,j  )+zy(i,k,j  )+ &
        !                   zy(i-1,k,j+1)+zy(i,k,j+1)  )
        !   tmpzeta_z = 0.5*(zeta_z(i,j)+zeta_z(i-1,j))
        !   titau1avg(i,k,j)=titau1avg(i,k,j)*zx(i,k,j)*tmpzeta_z
        !   titau2avg(i,k,j)=titau2avg(i,k,j)*tmpzy    *tmpzeta_z
            zx_at_u(i, k, j) = 0.5 * (zx(i, k, j) + zx(i, k+1 , j))
            zy_at_u(i, k, j) = 0.125 * (zy(i-1, k,   j  ) + zy(i, k,   j  ) + &
                                        zy(i-1, k,   j+1) + zy(i, k,   j+1) + &
                                        zy(i-1, k+1, j  ) + zy(i, k+1, j  ) + &
                                        zy(i-1, k+1, j+1) + zy(i, k+1, j+1))
        !   titau1avg(i,k,j)=titau1avg(i,k,j)*zx(i,k,j)
        !   titau2avg(i,k,j)=titau2avg(i,k,j)*tmpzy
        ENDDO
    ENDDO
ENDDO

END SUBROUTINE
