 SUBROUTINE PROFQ(DT2)
! **********************************************************************
! * Updated: Sep. 24, 2003 *
! * FUNCTION : Solves for q2 (twice the turbulent kinetic energy), *
! * q2l (q2 x turbulent length scale), km (vertical *
! * kinematic viscosity) and kh (vertical kinematic *
! * diffusivity), using a simplified version of the *
! * level 2 1/2 model of Mellor and Yamada (1982). *
! * In this version, the Craig-Banner sub-model whereby breaking wave *
! * tke is injected into the surface is included. However, we use an *
! * analytical solution to the near surface tke equation to solve for *
! * q2 at the surface giving the same result as C-B diffusion. The new *
! * scheme is simpler and more robust than the latter scheme. *
! * *
! * References *
! * Craig, P. D. and M. L. Banner, Modeling wave-enhanced turbulence *
! * in the ocean surface layer. J. Phys. Oceanogr., 24, 2546-2559, *
! * 1994. *
! * Ezer, T., On the seasonal mixed-layer simulated by a basin-scale *
! * ocean model and the Mellor-Yamada turbulence scheme, *
! * J. Geophys. Res., 105(C7), 16,843-16,855, 2000. *
! * Mellor, G.L. and T. Yamada, Development of a turbulence *
! * closure model for geophysical fluid fluid problems, *
! * Rev. Geophys. Space Phys., 20, 851-875, 1982. *
! * Mellor, G. L., One-dimensional, ocean surface layer modeling, *
! * a problem and a solution. J. Phys. Oceanogr., 31(3), 790-809, *
! * 2001. *
! * Mellor, G.L. and A. Blumberg, Wave breaking and ocean surface *
! * thermal response, J. Phys. Oceanogr., 2003. *
! * Stacey, M. W., Simulations of the wind-forced near-surface *
! * circulation in Knight Inlet: a parameterization of the *
! * roughness length. J. Phys. Oceanogr., 29, 1363-1367, 1999. *
! * *
! **********************************************************************
!
 USE LIMS
 USE CONTROL
 USE DATA_KIND_MOD
 USE CONST, ONLY : LE,ZERO,TBIAS,SBIAS,GRAV,RHOREF,UMOL
 USE ALL_VAR, ONLY : RHO,L,KM,U,V,KH,A,C,KQ,EE,GG,UF,T,S,Q2B,Q2LB,DTEF, &
                     Q2,VF,TPS,ETF,WUSURF,WVSURF,WUBOT,WVBOT,Z,H,DZZ, &
                     DZ,ZZ
 IMPLICIT NONE
 REAL(kind_r4),INTENT(IN) :: DT2

 REAL(kind_r4),PARAMETER :: A1 = 0.92,B1 = 16.6, A2 = 0.74, B2 = 10.1, &
                            C1 = 0.08,E1 = 1.8, E2 = 1.33, E3 = 1.0, &
                            KAPPA = 0.40, SQ = 0.20,SEF = 1.0, &
                         !   SMALL = 1.E-8,CONST1 = 16.6**.6666667*SEF, &
                            SMALL = 1.E-8, &
                            GHC=-6.0
 REAL(kind_r4) CONST1
 REAL(kind_r4),DIMENSION(ISLON-LE:IELON+LE,ISLAT-LE:IELAT+LE,KB) :: &
               GH,PROD,BOYGR,STF
 REAL(kind_r4) :: P,COEF4,COEF5,COEF1,COEF2,COEF3,TP,SP
 INTEGER KI
 CONST1=16.6**.6666667*SEF 

! SM >>> A
! SH >>> C
! KN >>> PROD
! DH >>> TPS
! CC >> DTEF
! EQUIVALENCE (A,SM),(C,SH),(PROD,KN),(TPS,DH)
! EQUIVALENCE (DTEF,CC)

! LOOP 50
! DO 50 J=1,JM
! DO 50 I=1,IM
 DO J=ISLAT,IELAT
 DO I=ISLON,IELON
 TPS(I,J)=H(I,J)+ETF(I,J)
 END DO
 END DO
! LOOP 100
! DO 100 J=1,JM
! DO 100 I=1,IM
 DO K=2,KBM1
 DO J=ISLAT,IELAT
 DO I=ISLON,IELON
 A(I,J,K)=-DT2*(KQ(I,J,K+1)+KQ(I,J,K)+2.*UMOL)*.5 &
     /(DZZ(K-1)*DZ(K)*TPS(I,J)*TPS(I,J))
 C(I,J,K)=-DT2*(KQ(I,J,K-1)+KQ(I,J,K)+2.*UMOL)*.5 &
     /(DZZ(K-1)*DZ(K-1)*TPS(I,J)*TPS(I,J))
 END DO
 END DO
 END DO
!***********************************************************************
! *
! THE FOLLOWING SECTION SOLVES THE EQUATION *
! DT2*(KQ*Q2')' - Q2*(2.*DT2*DTEF+1.) = -Q2B *
! *
!***********************************************************************
!------ SURFACE AND BOTTOM B.C.S ------------
!CONST1=16.6**.6666667*SEF
! LOOP 90
! DO 90 J=1,JMM1
! DO 90 I=1,IMM1
 DO J=ISLAT,JELAT
 DO I=ISLON,JELON
 EE(I,J,1)=ZERO
 GG(I,J,1)=SQRT( (.5*(WUSURF(I,J)+WUSURF(I+1,J)))**2 &
                 +(.5*(WVSURF(I,J)+WVSURF(I,J+1)))**2 )*CONST1
 UF(I,J,KB)=SQRT( (.5*(WUBOT(I,J)+WUBOT(I+1,J)))**2 &
                 +(.5*(WVBOT(I,J)+WVBOT(I,J+1)))**2 )*CONST1
 END DO
 END DO

!----- Calculate speed of sound squared ----------------------------
! LOOP 101
! DO 101 J=1,JM
! DO 101 I=1,IM
 DO K=1,KBM1
 DO J=ISLAT,IELAT
 DO I=ISLON,IELON
 TP=T(I,J,K)+TBIAS
 SP=S(I,J,K)+SBIAS
! Calculate pressure in units of decibars
 P=-GRAV*RHOREF*ZZ(K)*H(I,J)*1.E-4
 DTEF(I,J,K)=1449.1+.00821*P+4.55*TP -.045*TP**2 &
                               +1.34*(SP- 35.0)
 DTEF(I,J,K)=DTEF(I,J,K)/SQRT((1.-.01642*P/DTEF(I,J,K)) &
        *(1.-0.40*P/DTEF(I,J,K)**2))
 END DO
 END DO
 END DO
!----- Calculate buoyancy gradient ---------------------------------
! LOOP 102
! DO 102 J=1,JM
! DO 102 I=1,IM
 DO K=2,KBM1
 DO J=ISLAT,IELAT
 DO I=ISLON,IELON
 Q2B(I,J,K)=ABS(Q2B(I,J,K))
 Q2LB(I,J,K)=ABS(Q2LB(I,J,K))
 BOYGR(I,J,K)=GRAV*((RHO(I,J,K-1)-RHO(I,J,K))/(DZZ(K-1)*H(I,J))) &
  +GRAV**2*2./(DTEF(I,J,K-1)**2+DTEF(I,J,K)**2)
 END DO
 END DO
 END DO
! LOOP 220
! DO 220 J=1,JM
! DO 220 I=1,IM
 DO K=2,KBM1
 DO J=ISLAT,IELAT
 DO I=ISLON,IELON
 L(I,J,K)=Q2LB(I,J,K)/Q2B(I,J,K)
 GH(I,J,K)=L(I,J,K)**2/Q2B(I,J,K)*BOYGR(I,J,K)
 GH(I,J,K)=MIN(GH(I,J,K),.028)
 END DO
 END DO
 END DO

 L(:,:,1)=ZERO
 L(:,:,KB)=ZERO
 GH(:,:,1)=ZERO
 GH(:,:,KB)=ZERO
!------ CALC. T.K.E. PRODUCTION -----------------------------------
! LOOP 120
! DO 120 J=2,JMM1
! DO 120 I=1,IMM1
 DO K=2,KBM1
 DO J=JSLAT,JELAT
 DO I=ISLON,JELON
 PROD(I,J,K)=KM(I,J,K)*.25*SEF &
        *( (U(I,J,K)-U(I,J,K-1)+U(I+1,J,K)-U(I+1,J,K-1))**2 &
          +(V(I,J,K)-V(I,J,K-1)+V(I,J+1,K)-V(I,J+1,K-1))**2 ) &
               /(DZZ(K-1)*TPS(I,J))**2
 PROD(I,J,K)=PROD(I,J,K)+KH(I,J,K)*BOYGR(I,J,K)
 END DO
 END DO
 END DO

! GHC=-6.0
! LOOP 110
! DO 110 J=1,JM
! DO 110 I=1,IM
 DO K=1,KB
 DO J=ISLAT,IELAT
 DO I=ISLON,IELON
 STF(I,J,K)=1.
 IF(GH(I,J,K)<0.) STF(I,J,K)=1.0-0.9*(GH(I,J,K)/GHC)**1.5
 IF(GH(I,J,K)<GHC) STF(I,J,K)=0.1
 DTEF(I,J,K)=Q2B(I,J,K)*SQRT(Q2B(I,J,K))/(B1*Q2LB(I,J,K)+SMALL) &
            *STF(I,J,K)
 END DO
 END DO
 END DO
! LOOP 140
! DO 140 J=1,JM
! DO 140 I=1,IM
 DO K=2,KBM1
 DO J=ISLAT,IELAT
 DO I=ISLON,IELON
 GG(I,J,K)=1./(A(I,J,K)+C(I,J,K)*(1.-EE(I,J,K-1)) &
     -(2.*DT2*DTEF(I,J,K)+1.) )
 EE(I,J,K)=A(I,J,K)*GG(I,J,K)
 GG(I,J,K)=(-2.*DT2*PROD(I,J,K) &
   +C(I,J,K)*GG(I,J,K-1)-UF(I,J,K))*GG(I,J,K)
 END DO
 END DO
 END DO

! LOOP 150
! DO 150 J=1,JM
! DO 150 I=1,IM
 DO K=1,KBM1
   KI=KB-K
   DO J=ISLAT,IELAT
   DO I=ISLON,IELON
   UF(I,J,KI)=EE(I,J,KI)*UF(I,J,KI+1)+GG(I,J,KI)
   END DO
   END DO
 END DO
!***********************************************************************
! *
! THE FOLLOWING SECTION SOLVES THE EQUATION *
! DT2(KQ*Q2L')' - Q2L*(DT2*DTEF+1.) = -Q2LB *
! *
!***********************************************************************
! LOOP 155
! DO 155 J=1,JM
! DO 155 I=1,IM
 DO J=ISLAT,IELAT
 DO I=ISLON,IELON
 IF(UF(I,J,2)<SMALL)UF(I,J,2)=SMALL
 EE(I,J,2)=ZERO
 GG(I,J,2)=-KAPPA*Z(2)*TPS(I,J)*UF(I,J,2)
 VF(I,J,KB)=ZERO
 END DO
 END DO
! LOOP 160
! DO 160 J=1,JM
! DO 160 I=1,IM
 DO K=3,KBM1
 DO J=ISLAT,IELAT
 DO I=ISLON,IELON
 DTEF(I,J,K) =DTEF(I,J,K)*(1.+E2*((1./ABS(Z(K)-Z(1))+ &
     1./ABS(Z(K)-Z(KB))) *L(I,J,K)/(TPS(I,J)*KAPPA))**2)
 GG(I,J,K)=1./(A(I,J,K)+C(I,J,K)*(1.-EE(I,J,K-1)) &
     -(DT2*DTEF(I,J,K)+1.))
 EE(I,J,K)=A(I,J,K)*GG(I,J,K)
 GG(I,J,K)=(DT2*(-PROD(I,J,K) &
    *L(I,J,K)*E1)+C(I,J,K)*GG(I,J,K-1)-VF(I,J,K))*GG(I,J,K)
 END DO
 END DO
 END DO
! LOOP 170
! DO 170 J=1,JM
! DO 170 I=1,IM
 DO K=1,KB-2
   KI=KB-K
   DO J=ISLAT,IELAT
   DO I=ISLON,IELON
   VF(I,J,KI)=EE(I,J,KI)*VF(I,J,KI+1)+GG(I,J,KI)
   END DO
   END DO
 END DO
! LOOP 180
! DO 180 J=1,JM
! DO 180 I=1,IM
 DO K=2,KBM1
 DO J=ISLAT,IELAT
 DO I=ISLON,IELON
 IF(UF(I,J,K)>SMALL.AND.VF(I,J,K)>SMALL)CYCLE
    UF(I,J,K)=SMALL
    VF(I,J,K)=SMALL
 END DO
 END DO
 END DO
!***********************************************************************
! *
! THE FOLLOWING SECTION SOLVES FOR KM AND KH *
! *
!***********************************************************************
 COEF4=18.*A1*A1+9.*A1*A2
 COEF5=9.*A1*A2
! NOTE THAT SM,SH LIMIT TO INFINITY WHEN GH APPROACHES 0.0288
! LOOP 230
! DO 230 J=1,JM
! DO 230 I=1,IM
 DO K=1,KB
 DO J=ISLAT,IELAT
 DO I=ISLON,IELON
 COEF1=A2*(1.-6.*A1/B1*STF(I,J,K))
 COEF2=3.*A2*B2/STF(I,J,K)+18.*A1*A2
 COEF3=A1*(1.-3.*C1-6.*A1/B1*STF(I,J,K))
 C(I,J,K)=COEF1/(1.-COEF2*GH(I,J,K))
 A(I,J,K)=COEF3+C(I,J,K)*COEF4*GH(I,J,K)
 A(I,J,K)=A(I,J,K)/(1.-COEF5*GH(I,J,K))
 END DO
 END DO
 END DO

! LOOP 280
! DO 280 J=1,JM
! DO 280 I=1,IM
 DO K=1,KB
 DO J=ISLAT,IELAT
 DO I=ISLON,IELON
 PROD(I,J,K)=L(I,J,K)*SQRT(ABS(Q2(I,J,K)))
 KQ(I,J,K)=(PROD(I,J,K)*.41*C(I,J,K)+KQ(I,J,K))*.5
 KM(I,J,K)=(PROD(I,J,K)*A(I,J,K)+KM(I,J,K))*.5
 KH(I,J,K)=(PROD(I,J,K)*C(I,J,K)+KH(I,J,K))*.5
 END DO
 END DO
 END DO
 RETURN

 END
